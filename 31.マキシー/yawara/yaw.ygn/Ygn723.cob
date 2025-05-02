000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YGN723.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
      *         �J���e�i���j����Ώێ҃��X�g �y����z
000100*       MED = YGN720 YGN723P 
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2016-04-07
000130 DATE-COMPILED.          2016-04-07
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
000610     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000620                             ORGANIZATION             IS  INDEXED
000630                             ACCESS MODE              IS  DYNAMIC
000640                             RECORD KEY               IS  ���|�����敪
000650                             FILE STATUS              IS  ��ԃL�[
000660                             LOCK        MODE         IS  AUTOMATIC.
000670     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000680                             ORGANIZATION             IS  INDEXED
000690                             ACCESS MODE              IS  DYNAMIC
000700                             RECORD KEY               IS  ���|����敪
000710                             FILE STATUS              IS  ��ԃL�[
000720                             LOCK        MODE         IS  AUTOMATIC.
000730     SELECT  �{�p�����}�X�^ ASSIGN     TO        SEJOHOL
000740                             ORGANIZATION             IS  INDEXED
000750                             ACCESS MODE              IS  DYNAMIC
000760                             RECORD KEY               IS  �{��|�{�p���ԍ�
                                   FILE STATUS              IS  ��ԃL�[
000770                             LOCK        MODE         IS  AUTOMATIC.
000410     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000420                             ORGANIZATION             IS  INDEXED
000430                             ACCESS MODE              IS  DYNAMIC
000440                             RECORD KEY               IS  ���|�敪�R�[�h
000450                                                          ���|���̃R�[�h
000460                             FILE STATUS              IS  ��ԃL�[
000470                             LOCK        MODE         IS  AUTOMATIC.
           SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  ��|�{�p�a��N��
                                                                ��|���҃R�[�h
                                   ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
                                                                ��|���҃J�i
                                                                ��|���҃R�[�h
                                   ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
                                                                ��|�{�p�a��N��
                                   ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
                                                                ��|�ی����
                                                                ��|�ی��Ҕԍ�
                                                                ��|���҃R�[�h
                                   ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
                                                                ��|������
                                                                ��|��p���S�Ҕԍ�
                                                                ��|���҃R�[�h
                                   ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
                                                                ��|�������
                                                                ��|��p���S�Ҕԍ�����
                                                                ��|���҃R�[�h
                                   ALTERNATE RECORD KEY     IS  ��|�����a��N��
                                                                ��|�{�p�a��N��
                                                                ��|���҃R�[�h
                                   FILE STATUS              IS  ��ԃL�[
                                   LOCK        MODE         IS  AUTOMATIC.
000860     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W7211L.DAT"
000960                             ORGANIZATION             IS  INDEXED
000970                             ACCESS                   IS  DYNAMIC
000980                             RECORD      KEY          IS  ��P�|�{�p�a��N����
001030                                                          ��P�|���҃R�[�h
000910                             ALTERNATE RECORD KEY     IS  ��P�|�{�p�a��N����
                                                                ��P�|���҃J�i
                                                                ��P�|���҃R�[�h
000910                             ALTERNATE RECORD KEY     IS  ��P�|���҃R�[�h
                                                                ��P�|�{�p�a��N����
000910                             ALTERNATE RECORD KEY     IS  ��P�|���҃J�i
                                                                ��P�|���҃R�[�h
                                                                ��P�|�{�p�a��N����
001040                             FILE        STATUS       IS  ��ԃL�[
001050                             LOCK        MODE         IS  AUTOMATIC.
000780     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF001
000790                             SYMBOLIC    DESTINATION  IS "PRT"
000800                             FORMAT                   IS  ��`�̖��o
000810                             GROUP                    IS  ���ڌQ���o
000820                             PROCESSING  MODE         IS  ������ʂo
000830                             UNIT        CONTROL      IS  �g������o
000840                             FILE        STATUS       IS  �ʒm���o.
000850******************************************************************
000860*                      DATA DIVISION                             *
000870******************************************************************
000880 DATA                    DIVISION.
000890 FILE                    SECTION.
000950*                           �m�q�k��  �P�Q�W�n
000960 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000970     COPY GENGOU         OF  XFDLIB  JOINING    ��   AS  PREFIX.
000980*                           �m�q�k��  �Q�T�U�n
000990 FD  ������}�X�^      BLOCK   CONTAINS   1   RECORDS.
001000     COPY SEIGYO         OF  XFDLIB  JOINING    ��   AS  PREFIX.
001010*                           �m�q�k��  �U�S�O�n
001020 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
001030     COPY SEJOHO         OF  XFDLIB  JOINING    �{�� AS  PREFIX.
000840*                           �m�q�k��  �P�Q�W�n
000850 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
000860     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *                           �m�q�k��  �R�Q�O�n
       FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
           COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001310**************************
001320* ��ƃt�@�C���P�^�J���e *
001330**************************
001900*                         �m�q�k��  �Q�V�Q�n
001910 FD  ��ƃt�@�C���P RECORD  CONTAINS 272 CHARACTERS.
001920 01 ��P�|���R�[�h.
001930    03 ��P�|���R�[�h�L�[.
001940       05 ��P�|�{�p�a��N����.
001950          07 ��P�|�{�p�a��               PIC 9.
001960          07 ��P�|�{�p�N��.
001970             09 ��P�|�{�p�N              PIC 9(2).
001980             09 ��P�|�{�p��              PIC 9(2).
001990          07 ��P�|�{�p��                 PIC 9(2).
002000       05 ��P�|���҃R�[�h.
002010          07 ��P�|���Ҕԍ�                PIC 9(6).
002020          07 ��P�|�}��                    PIC X(1).
002030    03 ��P�|���R�[�h�f�[�^.
002100       05 ��P�|���҃J�i                   PIC X(50).
002110       05 ��P�|���Ҏ���                   PIC X(50).
002130       05 ��P�|����.
001550          07 ��P�|�������z                PIC 9(5).
                07 ��P�|����                    OCCURS 4.
001550             09 ��P�|�������z             PIC 9(5).
001550          07 ��P�|���̑�                  PIC 9(5).
001550          07 ��P�|㪖@��                  PIC 9(5).
001550          07 ��P�|�������q                PIC 9(5).
001550          07 ��P�|���×�                  PIC 9(5).
002300          07 ��P�|��p�z                  PIC 9(6).
002270          07 ��P�|�ꕔ���S��              PIC 9(5).
002280          07 ��P�|�R�����g                PIC X(100).
002310       05 FILLER                           PIC X(2).
      *
001040 FD  ����t�@�C��.
001050     COPY YGN723P        OF  XMDLIB.
001060******************************************************************
001070*                WORKING-STORAGE SECTION                         *
001080******************************************************************
001090 WORKING-STORAGE         SECTION.
       01 �s�J�E���^                         PIC 9(2) VALUE ZERO.
       01 �ŃJ�E���^                         PIC 9(4) VALUE ZERO.
       01 �ő�s��                           PIC 9(2) VALUE ZERO.
001100 01 �L�[����                           PIC X    VALUE SPACE.
001110 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
001120 01 �I���t���O                         PIC X(3) VALUE SPACE.
002120 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
001140 01 �I���t���O�R                       PIC X(3) VALUE SPACE.
001150 01 �m�F���͂v                         PIC X(1) VALUE SPACE.
001160 01 �t�@�C�����v                       PIC N(2) VALUE SPACE.
001170 01 �J�����g�����v                     PIC 9(1) VALUE ZERO.
001180 01 ���s�L�[�v                         PIC X(4) VALUE SPACE.
001190 01 �O�a��v                           PIC 9 VALUE ZERO.
001200 01 �s���t���O                         PIC X(3) VALUE SPACE.
001220 01 �����ړ��L�[                       PIC X(4) VALUE SPACE.
001580 01 �I�[�v���t���O                     PIC X(3) VALUE SPACE.
002940 01 ���Ҕԍ��v                         PIC 9(6) VALUE ZERO.
001460 01 ���҃R�[�h�v.
001470    03 ���Ҕԍ��v�o                    PIC 9(6) VALUE ZERO.
001480    03 �}�Ԃv�o                        PIC X(1) VALUE SPACE.
002940 01 ���ʃR�[�h�v.
          03 ������ʂv                      PIC 9(2) VALUE ZERO.
          03 ���ʂv                          PIC 9(2) VALUE ZERO.
       01 �]�A�敪�v                         PIC 9(1) VALUE ZERO.
       01 ���̓f�[�^�v.
002940    03 �J�n���ʃR�[�h�v.
             05 �J�n������ʂv               PIC 9(2) VALUE ZERO.
             05 �J�n���ʂv                   PIC 9(2) VALUE ZERO.
          03 �J�n�]�A�敪�v                  PIC 9    VALUE ZERO.
002940    03 �I�����ʃR�[�h�v.
             05 �I��������ʂv               PIC 9(2) VALUE ZERO.
             05 �I�����ʂv                   PIC 9(2) VALUE ZERO.
          03 �I���]�A�敪�v                  PIC 9    VALUE ZERO.
001560*
001570 01 ���̂v.
001580   03 ���̂v�P                         PIC N(12) VALUE SPACE.
001590   03 FILLER                           PIC N(2) VALUE SPACE.
001240 01 �{�p�a��N���v�q.
001250    03 �{�p�a��v�q                    PIC 9    VALUE ZERO.
001260    03 �{�p�N���v�q.
001270       05 �{�p�N�v�q                   PIC 9(2) VALUE ZERO.
001280       05 �{�p���v�q                   PIC 9(2) VALUE ZERO.
001240 01 �����a��N�����v.
001250    03 �����a��v                      PIC 9    VALUE ZERO.
001260    03 �����N���v.
002960       05 �����N�v                     PIC 9(2) VALUE ZERO.
002970       05 �������v                     PIC 9(2) VALUE ZERO.
002970    03 �������v                        PIC 9(2) VALUE ZERO.
001320 01 �������̂v                         PIC N(2) VALUE SPACE.
002433* �G���[���b�Z�[�W�p
002434 01 �G���[���b�Z�[�W�v.
002435    03 �G���[���҃R�[�h�v              PIC X(7)  VALUE SPACE.
002436    03 �G���[��؂�v                  PIC X(1)  VALUE SPACE.
002437    03 �G���[�ی���ʂv                PIC X(2)  VALUE SPACE.
002438    03 FILLER                          PIC X(10) VALUE SPACE.
       01 �J�Ѓt���O                         PIC X(3)  VALUE SPACE.
001530*
001540 01 �������.
001550     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
001560     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
001570     03 ������ʂo                     PIC X(2) VALUE SPACE.
001580     03 �g������o.
001590         05 �[������o.
001600             07 �ړ������o             PIC X(1).
001610             07 �ړ��s���o             PIC 9(3).
001620         05 �ڍא���o                 PIC X(2).
001630     03 �ʒm���o                     PIC X(2) VALUE SPACE.
001640     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
001650 01 �v�Z�@����N�v                     PIC 9(2).
001660* ���t�v�n�q�j
001670 01 �a��I���N�v                       PIC 9(4).
001680 01 �v�Z�@�a��N�v                     PIC 9(2).
001690 01 �v�Z�@����.
001700    03 �v�Z�@����N                    PIC 9(4).
001710    03 �v�Z�@�����                  PIC 9(4).
001720 01 �v�Z�@����q REDEFINES �v�Z�@����.
001730    03 �v�Z�@���I                      PIC 9(2).
001740    03 �v�Z�@���t                      PIC 9(6).
001750    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
001760       05 �v�Z�@�N��                   PIC 9(4).
001770       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
001780         07 �v�Z�@�N                   PIC 9(2).
001790         07 �v�Z�@��                   PIC 9(2).
001800       05 �v�Z�@��                     PIC 9(2).
001810*
003340******************************************************************
003350*                          �A������                              *
003360******************************************************************
003370*
      **********************
      * ���b�Z�[�W�\���L�[ *
      **********************
      *
       01 �A���|�L�[ IS EXTERNAL.
          03  �A���|���b�Z�[�W                 PIC N(20).
      *
002634 01 �A���R�|�L�[ IS EXTERNAL.
002635    03  �A���R�|���b�Z�[�W             PIC N(20).
002636    03  �A���R�|���b�Z�[�W�P           PIC X(20).
      *
001793 01 �A���V�|�L�[ IS EXTERNAL.
001794    03  �A���V�|���b�Z�[�W�P               PIC X(40).
001795    03  �A���V�|���b�Z�[�W�Q               PIC X(40).
001796*
003080****************
003090* ��ʓ��͏�� *
003100****************
004960 01 �A���|���̓f�[�^�x�f�m�V�Q�O IS EXTERNAL.
004970    03 �A���|�J�n�a��N����.
004980       05 �A���|�J�n�a��                  PIC 9(1).
004990       05 �A���|�J�n�N                    PIC 9(2).
005000       05 �A���|�J�n��                    PIC 9(2).
005010       05 �A���|�J�n��                    PIC 9(2).
005020    03 �A���|�I���a��N����.
005030       05 �A���|�I���a��                  PIC 9(1).
005040       05 �A���|�I���N                    PIC 9(2).
005050       05 �A���|�I����                    PIC 9(2).
005060       05 �A���|�I����                    PIC 9(2).
005080    03 �A���|�ی����                     PIC 9(2).
005090    03 �A���|�{�l�Ƒ��敪                 PIC 9(1).
005100    03 �A���|���҃R�[�h.
005110       05 �A���|���Ҕԍ�                  PIC 9(6).
005120       05 �A���|�}��                      PIC X(1).
005140    03 �A���|�������                     PIC 9(2).
      *
004140 01 �A���|�\���t���O�x�f�m�V�Q�O IS EXTERNAL GLOBAL.
          03 �A���|�v���r���[�敪               PIC 9(1).
002870************
002880* ����L�[ *
002890************
       01 �A��|�Ώۃf�[�^�x�f�m�V�Q�O IS EXTERNAL.
          03 �A��|�{�p�a��N����.
             05 �A��|�{�p�a��                  PIC 9(1).
             05 �A��|�{�p�N                    PIC 9(2).
             05 �A��|�{�p��                    PIC 9(2).
             05 �A��|�{�p��                    PIC 9(2).
          03 �A��|���҃R�[�h.
             05 �A��|���Ҕԍ�                  PIC 9(6).
             05 �A��|�}��                      PIC X(1).
          03 �A��|���N���[�h                   PIC 9(1).
          03 �A��|�������[�h                   PIC 9(1).
          03 �A��|���׃��[�h                   PIC 9(1).
          03 �A��|���v���[�h                   PIC 9(1).
          03 �A��|�R�����g���[�h               PIC 9(1).
          03 �A��|�ǂݏ�                       PIC X(4).
          03 �A��|�i��                         PIC 9(2).
          03 �A��|�����L�[                     PIC X(4).
004580*
000540************************************
000550* �v�����^�t�@�C���쐬�p           *
000560************************************
000570 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000580     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
000590     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
000600     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
000610     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
007308*
003890******************************************************************
003900*                      PROCEDURE  DIVISION                       *
003910******************************************************************
003920 PROCEDURE               DIVISION.
003930************
003940*           *
003950* ��������   *
003960*           *
003970************
002570     PERFORM �v�����^�t�@�C���쐬.
003980     PERFORM ������.
001790************
001800*           *
001810* �又��     *
001820*           *
001830************
004550     PERFORM �������.
004620************
004630*           *
004640* �I������   *
004650*           *
004660************
004670     PERFORM �I������.
004680     EXIT PROGRAM.
004690*
004700*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002860*================================================================*
002870 �v�����^�t�@�C���쐬 SECTION.
002880*================================================================*
002890*   / ������ /
002900     MOVE SPACE TO �g�A�o�q�s�e�|�쐬�f�[�^.
002910     INITIALIZE �g�A�o�q�s�e�|�쐬�f�[�^.
002920*
002930*
002940*--���� �ύX�ӏ� ����--------------------------------------*
002970*   �g�p����v�����^�t�@�C�����Z�b�g
002231     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
002972*
002973*   �g�p���钠�[�v���O�������Z�b�g
002974     MOVE "YGN723"              TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
003000*     MOVE 1 TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
004710*================================================================*
004720 ������ SECTION.
004730*
004770     OPEN INPUT �����}�X�^.
004780             MOVE NC"����" TO �t�@�C�����v.
004790             PERFORM �I�[�v���`�F�b�N.
004800     OPEN INPUT ������}�X�^.
004810             MOVE NC"����" TO �t�@�C�����v.
004820             PERFORM �I�[�v���`�F�b�N.
004830     OPEN INPUT �{�p�����}�X�^.
004840             MOVE NC"�{��" TO �t�@�C�����v.
004850             PERFORM �I�[�v���`�F�b�N.
004800     OPEN INPUT ���̃}�X�^.
004810             MOVE NC"����" TO �t�@�C�����v.
004820             PERFORM �I�[�v���`�F�b�N.
004800     OPEN INPUT ��f�ҏ��e.
004810             MOVE NC"��f" TO �t�@�C�����v.
004820             PERFORM �I�[�v���`�F�b�N.
007640     OPEN INPUT ��ƃt�@�C���P.
007650             MOVE NC"��P" TO �t�@�C�����v.
007660             PERFORM �I�[�v���`�F�b�N.
005960*     OPEN I-O   ����t�@�C��.
      *         MOVE NC"����t�@�C��" TO �t�@�C�����v.
      *         PERFORM �G���[�����o.
004900*
004910*    /* ���ݓ��t�擾 */
004920     ACCEPT �v�Z�@���t FROM DATE.
004930*    /* 1980�`2079�N�̊ԂŐݒ� */
004940     IF �v�Z�@�N > 80
004950         MOVE 19 TO �v�Z�@���I
004960     ELSE
004970         MOVE 20 TO �v�Z�@���I
004980     END-IF.
004990*
005000     PERFORM �J�����g�����擾.
005010     PERFORM �a��I���N�擾.
005020     COMPUTE �v�Z�@�a��N�v = �v�Z�@����N - �a��I���N�v.
005060*
005080*================================================================*
005090 �I�[�v���`�F�b�N SECTION.
005100*
005110     IF ��ԃL�[  NOT =  "00"
005120         DISPLAY �t�@�C�����v NC"�e�I�[�v���G���[" UPON CONS
005130         DISPLAY NC"��ԃL�[�F" ��ԃL�[           UPON CONS
005140         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005150                                                   UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
005160         ACCEPT  �L�[���� FROM CONS
005170         PERFORM �t�@�C����
005180         EXIT PROGRAM.
005190*================================================================*
005200 �J�����g�����擾 SECTION.
005210*
005220     MOVE ZEROS TO ���|����敪.
005230     READ ������}�X�^
005240     NOT INVALID KEY
005250         MOVE ���|�J�����g���� TO �J�����g�����v
005260     END-READ.
005270*
005280*================================================================*
005290 �a��I���N�擾 SECTION.
005300*
005320     MOVE �J�����g�����v TO ���|�����敪.
005330     READ �����}�X�^
005340     INVALID KEY
005350         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
005360         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005370                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
005380         ACCEPT  �L�[���� FROM CONS
005390         PERFORM �I������
005400         EXIT PROGRAM
005410     NOT INVALID KEY
005420         COMPUTE �O�a��v = �J�����g�����v - 1
005430         MOVE �O�a��v TO ���|�����敪
005440         READ �����}�X�^
005450         INVALID KEY
005460             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
005470             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005480                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
005490             ACCEPT  �L�[���� FROM CONS
005500             PERFORM �I������
005510             EXIT PROGRAM
005520         NOT INVALID KEY
005530             MOVE ���|�I������N TO �a��I���N�v
005540         END-READ
005550     END-READ.
005560*
006150*================================================================*
006770 �t�@�C���� SECTION.
006780*
006790     CLOSE  �����}�X�^     ������}�X�^  �{�p�����}�X�^
                  ��f�ҏ��e   ���̃}�X�^      ��ƃt�@�C���P.
002989*
002990     IF ( �I�[�v���t���O = "YES" )
002991         CLOSE ����t�@�C��
003041     END-IF.
003042*
006810*================================================================*
006820 �I������ SECTION.
006830*
006840     PERFORM �t�@�C����.
      *================================================================*
010770 ������� SECTION.
010780*
           MOVE 50    TO �ő�s��.
           MOVE 99    TO �s�J�E���^.
           MOVE ZERO  TO �ŃJ�E���^.
007740     MOVE SPACE TO �I���t���O.
      *
           MOVE ZERO  TO ��P�|�{�p�a��N����.
           MOVE SPACE TO ��P�|���҃J�i.
           MOVE SPACE TO ��P�|���҃R�[�h.
      *
           IF �A��|�ǂݏ� = "BANG"
               START ��ƃt�@�C���P KEY IS > ��P�|���҃R�[�h
                                             ��P�|�{�p�a��N����
               END-START
           ELSE
               START ��ƃt�@�C���P KEY IS > ��P�|���҃J�i
                                             ��P�|���҃R�[�h
                                             ��P�|�{�p�a��N����
           END-IF.
013500     IF ��ԃL�[  =  "00"
               PERFORM ��ƃt�@�C���P�Ǎ�
               PERFORM UNTIL �I���t���O = "YES"
                   IF ( �s�J�E���^        >= �ő�s��   )
                       COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
                       PERFORM �w�b�_���R�[�h�Z�b�g
                       PERFORM �w�b�_�������
                       MOVE ZERO TO �s�J�E���^
                   ELSE
004540                 PERFORM ���׃��R�[�h�Z�b�g
004550                 PERFORM ���׈������
                       MOVE ��P�|���҃R�[�h TO ���҃R�[�h�v
                       PERFORM UNTIL ( ��P�|���҃R�[�h NOT = ���҃R�[�h�v ) OR
                                     ( �I���t���O = "YES" )
                           PERFORM ��ƃt�@�C���P�Ǎ�
                       END-PERFORM
                   END-IF
               END-PERFORM
               IF �J�Ѓt���O = "YES"
                   PERFORM �J�Ј������
               END-IF
           END-IF.
      *================================================================*
007700 ��ƃt�@�C���P�Ǎ� SECTION.
007710*
007720     READ ��ƃt�@�C���P NEXT
007730     AT END
007740         MOVE "YES" TO �I���t���O
007750     END-READ.
007760*================================================================*
       �w�b�_���R�[�h�Z�b�g SECTION.
      *
           MOVE SPACE TO YGN723P.
           INITIALIZE YGN723P.
           MOVE �ŃJ�E���^         TO �y�[�W.
      */�J�n�a��N��
006960     MOVE �A���|�J�n�a��    TO ���|�����敪.
006970     READ �����}�X�^
006980     INVALID KEY
006990         MOVE SPACE         TO �J�n�a��
007000     NOT INVALID KEY
007020         MOVE ���|��������  TO �J�n�a��
007030     END-READ.
003140     MOVE �A���|�J�n�N      TO �J�n�N.
003150     MOVE �A���|�J�n��      TO �J�n��.
      */�I���a��N��
006960     MOVE �A���|�I���a��    TO ���|�����敪.
006970     READ �����}�X�^
006980     INVALID KEY
006990         MOVE SPACE         TO �I���a��
007000     NOT INVALID KEY
007020         MOVE ���|��������  TO �I���a��
007030     END-READ.
003140     MOVE �A���|�I���N      TO �I���N.
003150     MOVE �A���|�I����      TO �I����.
007700*================================================================*
010770 ���׃��R�[�h�Z�b�g SECTION.
010780*
           MOVE SPACE TO YGN723P.
           INITIALIZE YGN723P.
      *
           MOVE ��P�|���҃R�[�h TO ���҃R�[�h.
           MOVE ��P�|���҃R�[�h TO ��|���҃R�[�h.
001390     MOVE ��P�|�{�p�a��   TO ��|�{�p�a��.
001400     MOVE ��P�|�{�p�N     TO ��|�{�p�N.
001410     MOVE ��P�|�{�p��     TO ��|�{�p��.
009200     READ ��f�ҏ��e
009210     INVALID KEY
009220         MOVE  NC"��f�ҏ�񂪓o�^����Ă��܂���" TO �A���|���b�Z�[�W
009230         CALL   "MSG001"
009240         CANCEL "MSG001"
009250     NOT INVALID KEY
               MOVE ��|���Ҏ���     TO ���Ҏ���
               MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ���
      */�ی����
               MOVE 02                   TO ���|�敪�R�[�h
               MOVE ��|�ی����         TO ���|���̃R�[�h
               READ ���̃}�X�^
               NOT INVALID KEY
                   MOVE ���|����         TO �ی���ʗ���
               END-READ
      *
               IF ��|�{�p�a��N�� < 42004
                   MOVE 02                   TO ���|�敪�R�[�h
                   MOVE ��|������         TO ���|���̃R�[�h
                   READ ���̃}�X�^
                   NOT INVALID KEY
                       MOVE ���|����         TO �����ʗ���
                   END-READ
               END-IF
      *
               MOVE 12                   TO ���|�敪�R�[�h
               MOVE ��|�������         TO ���|���̃R�[�h
               READ ���̃}�X�^
               NOT INVALID KEY
                   MOVE ���|����         TO ������ʗ���
               END-READ
           END-READ.
      *================================================================*
       �w�b�_������� SECTION.
011790*
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
004360*
011800     MOVE "YGN723P"  TO  ��`�̖��o.
011810     MOVE "HEAD01"   TO  ���ڌQ���o.
011820     WRITE YGN723P.
           PERFORM �G���[�����o.
      *================================================================*
       ���׈������ SECTION.
011790*
011800     MOVE "YGN723P"  TO  ��`�̖��o.
011810     MOVE "GRP001"   TO  ���ڌQ���o.
011820     WRITE YGN723P.
           PERFORM �G���[�����o.
           COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
      *================================================================*
       �J�Ј������ SECTION.
011790*
011800     MOVE "YGN723P"  TO  ��`�̖��o.
011810     MOVE "GRP002"   TO  ���ڌQ���o.
011820     WRITE YGN723P.
           PERFORM �G���[�����o.
      *================================================================*
       ���ŏ���  SECTION.
      *
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
004360*
011820     MOVE SPACE TO YGN723P.
011820     INITIALIZE YGN723P.
           MOVE "YGN723P" TO  ��`�̖��o.
           MOVE "CT"      TO  ������ʂo.
           MOVE "PAGE"    TO  �g������o.
           MOVE SPACE     TO  ���ڌQ���o.
011820     WRITE YGN723P.
           PERFORM �G���[�����o.
           MOVE SPACE     TO  �g������o.
      *================================================================*
014060 �G���[�\�� SECTION.
014070*
014080     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C�����v UPON CONS.
014090     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
014100     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
014110     ACCEPT  �L�[���� FROM CONS.
      *================================================================*
       �G���[�����o SECTION.
      *
           IF �ʒm���o NOT = "00"
               DISPLAY NC"���[�G���["              UPON CONS
               DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
               DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
               DISPLAY NC"�g������o�F" �g������o UPON CONS
               DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
                                                   UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
               ACCEPT  �L�[���� FROM CONS
               PERFORM �t�@�C����
               MOVE 99 TO PROGRAM-STATUS
               EXIT PROGRAM
           END-IF.
      *================================================================*
014130 �G���[�\���q SECTION.
014140*
014150     DISPLAY NC"�t�@�C���Ǎ��G���[" �t�@�C�����v UPON CONS.
014160     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
014170     ACCEPT  �L�[���� FROM CONS.
014180     PERFORM �t�@�C����.
014190     EXIT PROGRAM.
014200*================================================================*
014210 �G���[�\�����̑� SECTION.
014220*
014230     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
014240     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
014250                                                   UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
014260     ACCEPT  �L�[���� FROM CONS.
014270     PERFORM �t�@�C����.
014280     EXIT PROGRAM.
001370*================================================================*
014300******************************************************************
014310 END PROGRAM YGN723.
014320******************************************************************
