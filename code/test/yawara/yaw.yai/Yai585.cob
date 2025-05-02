000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAI585.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*      �V���p��   �������y����z�_+����޳�ޔ�
000100*  �����N���o�[�W����
000101*         MED = YAI585P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2013-06-27
000130 DATE-COMPILED.          2013-06-27
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
000260     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  ���|�����敪
000300                             FILE STATUS              IS  ��ԃL�[
000310                             LOCK        MODE         IS  AUTOMATIC.
000380     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS  �ہ|�ی����
000420                                                          �ہ|�ی��Ҕԍ�
000430                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000440                                                          �ہ|�ی��Җ���
000450                                                          �ہ|�ی��Ҕԍ�
000460                             FILE STATUS              IS  ��ԃL�[
000470                             LOCK        MODE         IS  AUTOMATIC.
000480     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000490                             ORGANIZATION             IS  INDEXED
000500                             ACCESS MODE              IS  DYNAMIC
000510                             RECORD KEY               IS  �s�|������
000520                                                          �s�|�s�����ԍ�
000530                             ALTERNATE RECORD KEY     IS  �s�|������
000540                                                          �s�|�s��������
000550                                                          �s�|�s�����ԍ�
000560                             FILE STATUS              IS  ��ԃL�[
000570                             LOCK        MODE         IS  AUTOMATIC.
000650     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS �{��|�{�p���ԍ�
000690                             FILE STATUS              IS  ��ԃL�[
000700                             LOCK        MODE         IS  AUTOMATIC.
000710     SELECT  ������}�X�^    ASSIGN      TO        SEIKYUSL
000720                             ORGANIZATION             IS  INDEXED
000730                             ACCESS MODE              IS  DYNAMIC
000740                             RECORD KEY               IS  ����|�ی����
000750                                                          ����|�ی��Ҕԍ�
000760                             FILE STATUS              IS  ��ԃL�[
000770                             LOCK    MODE             IS  AUTOMATIC.
000783*
000108     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5831L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  ��Q�|�ی����
000113                                                          ��Q�|�ی��Ҕԍ�
000114                             ALTERNATE RECORD KEY     IS  ��Q�|��������P
000115                                                          ��Q�|��
000116                                                          ��Q�|�������
000117                                                          ��Q�|�ی����
000118                                                          ��Q�|�ی��Ҕԍ�
000119                             FILE        STATUS       IS  ��ԃL�[
000120                             LOCK        MODE         IS  AUTOMATIC.
000806*
000980     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF001
000990                             SYMBOLIC    DESTINATION  IS "PRT"
001000                             FORMAT                   IS  ��`�̖��o
001010                             GROUP                    IS  ���ڌQ���o
001020                             PROCESSING  MODE         IS  ������ʂo
001030                             UNIT        CONTROL      IS  �g������o
001040                             FILE        STATUS       IS  �ʒm���o.
001050******************************************************************
001060*                      DATA DIVISION                             *
001070******************************************************************
001080 DATA                    DIVISION.
001090 FILE                    SECTION.
001100*                           �m�q�k��  �P�Q�W�n
001110 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001120     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001160*                           �m�q�k��  �R�Q�O�n
001170 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001180     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001190*                           �m�q�k��  �Q�T�U�n
001200 FD  �s�����}�X�^        BLOCK   CONTAINS   1   RECORDS.
001210     COPY SITYOSN         OF  XFDLIB  JOINING   �s   AS  PREFIX.
001250*                           �m�q�k��  �P�Q�W�n
001260 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001270     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001280*                           �m�q�k��  �P�Q�W�n
001290 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001300     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
001320*                           �m�q�k��  �P�Q�W�n
001330 FD  ��ƃt�@�C���Q RECORD  CONTAINS 128 CHARACTERS.
001340 01  ��Q�|���R�[�h.
001350     03  ��Q�|���R�[�h�L�[.
001400         05  ��Q�|�ی����                  PIC 9(2).
001410         05  ��Q�|�ی��Ҕԍ�                PIC X(10).
001440     03  ��Q�|���R�[�h�f�[�^.
001420         05  ��Q�|��                        PIC X(2).
001431         05  ��Q�|�������.
001432             07  ��Q�|��������P            PIC 9(2).
001433             07  ��Q�|��������Q            PIC 9.
001450         05  ��Q�|����                      PIC 9(4).
001460         05  ��Q�|��p�z                    PIC 9(9).
001470         05  ��Q�|���S�z                    PIC 9(9).
001480         05  ��Q�|�����z                    PIC 9(9).
001490         05  ��Q�|�{�l����                  PIC 9(3).
001500         05  ��Q�|�{�l��p�z                PIC 9(7).
001510         05  ��Q�|�{�l���S�z                PIC 9(7).
001520         05  ��Q�|�{�l�����z                PIC 9(7).
001530         05  ��Q�|�Ƒ�����                  PIC 9(3).
001540         05  ��Q�|�Ƒ���p�z                PIC 9(7).
001550         05  ��Q�|�Ƒ����S�z                PIC 9(7).
001560         05  ��Q�|�Ƒ������z                PIC 9(7).
001570         05  FILLER                          PIC X(32).
001700*
001710 FD  ����t�@�C��.
001720     COPY YAI585P        OF  XMDLIB.
001721*
001730*----------------------------------------------------------------*
001740******************************************************************
001750*                WORKING-STORAGE SECTION                         *
001760******************************************************************
001770 WORKING-STORAGE         SECTION.
001780 01 �L�[����                           PIC X     VALUE SPACE.
001790 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
001800 01 �I���t���O                         PIC X(3)  VALUE SPACE.
001810 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
001860 01 �t�@�C����                         PIC N(2)  VALUE SPACE.
001890 01 �ی���ʂv                         PIC 9(2)  VALUE ZERO.
001900 01 �ی��Ҕԍ��v                       PIC X(10) VALUE SPACE.
001910 01 �ی��Ҕԍ��P�v                     PIC X(10) VALUE SPACE.
001920 01 �ی��Ҕԍ��Q�v                     PIC X(10) VALUE SPACE.
001921 01 ����t���O                         PIC X(3)  VALUE SPACE.
001210 01 �I�[�v���t���O                     PIC X(3)   VALUE SPACE.
001930*
001940 01 �s�J�E���^                         PIC 9(2)  VALUE ZERO.
001950 01 �ŃJ�E���^                         PIC 9(4)  VALUE ZERO.
001960 01 �ő�s��                           PIC 9(2)  VALUE ZERO.
001970 01 �w�b�_�s��                         PIC 9(2)  VALUE ZERO.
001980 01 �ړ��s���v                         PIC 9(2)  VALUE ZERO.
002000 01 �ی����̂v                         PIC N(2) VALUE SPACE.
002010*
002030 01 �{�p�a��N���v.
002040     03 �{�p�a��v                     PIC 9(1)  VALUE ZERO.
002050     03 �{�p�N���v.
002060        05 �{�p�N�v                    PIC 9(2)  VALUE ZERO.
002070        05 �{�p���v                    PIC 9(2)  VALUE ZERO.
002220**
002290**************
002300* �{�p����� *
002310**************
002320 01 �{�p�����v.
002330    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
002340    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
001940    03 �_���t�ԍ��v.
             05 �_���t�L���v                 PIC X(2)   VALUE SPACE.
             05 �_���t�ԍ��P�v               PIC X(7)   VALUE SPACE.
             05 �_���t��؂P�v               PIC X(1)   VALUE SPACE.
             05 �_���t�ԍ��Q�v               PIC X(1)   VALUE SPACE.
             05 �_���t��؂Q�v               PIC X(1)   VALUE SPACE.
             05 �_���t�ԍ��R�v               PIC X(1)   VALUE SPACE.
002350    03 �{�p���Z���v.
002360       05 �{�p���Z���P�v               PIC X(50)  VALUE SPACE.
002370       05 �{�p���Z���Q�v               PIC X(50)  VALUE SPACE.
002380    03 �{�p���X�֔ԍ��v.
002390       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
002400       05 �{�p���X�֔ԍ���؂v         PIC X(1)   VALUE SPACE.
002410       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
002420    03 �{�p���d�b�ԍ��v                PIC X(15)  VALUE SPACE.
002430    03 �������v.
002440        05 ������s���v              PIC X(40)  VALUE SPACE.
002450        05 ������s�x�X���v          PIC X(40)  VALUE SPACE.
002460        05 �a����ʂv                  PIC 9(1)   VALUE ZERO.
002470        05 ��s�ԍ��v                  PIC X(4)   VALUE SPACE.
002480        05 �X�ԍ��v                    PIC X(3)   VALUE SPACE.
002490        05 �����ԍ��v                  PIC X(10)  VALUE SPACE.
002500        05 �������`�l�J�i�v.
002500           07 �������`�l�J�i�P�v       PIC X(60)  VALUE SPACE.
002500           07 �������`�l�J�i�Q�v       PIC X(60)  VALUE SPACE.
002501        05 �������`�l�v.
002501           07 �������`�l�P�v           PIC X(60)  VALUE SPACE.
002501           07 �������`�l�Q�v           PIC X(60)  VALUE SPACE.
002510*
002520 01 �A�Ԃv                             PIC 9(3)   VALUE ZERO.
002530 01 ��s���x�X���v                     PIC X(82)  VALUE SPACE.
002540 01 �a����ʃR�����g�v                 PIC N(2)   VALUE SPACE.
002550 01 �T���v                             PIC N(4)   VALUE SPACE.
002551**
002552* �Еۗp
002553 01 �ڔ���敪�v                       PIC 9     VALUE ZERO.
002554*
002560********************
002570* �ی��ҕʍ��v���z *
002580********************
002590 01 �ی��ҕʍ��v���z.
002600    03 ���z�v                          PIC N(2)  VALUE SPACE.
002610    03 �������b�Z�[�W�v                PIC N(15) VALUE SPACE.
002620    03 �~�v                            PIC N(1)  VALUE SPACE.
002630    03 �����v                          PIC 9(3)  VALUE ZERO.
002640    03 ��p�z�v                        PIC 9(8)  VALUE ZERO.
002650    03 �����z�v                        PIC 9(7)  VALUE ZERO.
002660    03 �����於�̂v                    PIC X(40) VALUE SPACE.
002670    03 �x���������v                    PIC X(40) VALUE SPACE.
002680    03 �����v                          PIC X(24) VALUE SPACE.
002690    03 �ی��҈����v.
002700       05 �ی��҈����P�v               PIC X(40) VALUE SPACE.
002710       05 �ی��҈����Q�v               PIC X(40) VALUE SPACE.
002711**
002720 01 ��ʏ��v.
002730    03 �����N���v.
002740       05 �����a��v                   PIC 9     VALUE ZERO.
002750       05 �����N�v                     PIC 9(2)  VALUE ZERO.
002760       05 �������v                     PIC 9(2)  VALUE ZERO.
002770    03 ��o�N�����v.
002780       05 ��o�a��v                   PIC 9     VALUE ZERO.
002790       05 ��o�N�v                     PIC 9(2)  VALUE ZERO.
002800       05 ��o���v                     PIC 9(2)  VALUE ZERO.
002810       05 ��o���v                     PIC 9(2)  VALUE ZERO.
002821***
002822* �G���[���b�Z�[�W�p
002823 01 �G���[���b�Z�[�W�v.
002824    03 �G���[�ی���ʂv                PIC X(2) VALUE SPACE.
002825    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
002826    03 �G���[�ی��Ҕԍ��v              PIC X(10) VALUE SPACE.
002827    03 FILLER                          PIC X(7) VALUE SPACE.
002828*
002829***
002842* ���̑���p
002844 01 ���̑��ҏW�v.
002845    03 ���̑��ҏW���e�v                PIC X(42) VALUE SPACE.
002846*
003280 01 �J�E���^                           PIC 9(3)  VALUE ZERO.
003290 01 �J�E���^�Q                         PIC 9(3)  VALUE ZERO.
004200 01 ���v                               PIC 9(3)  VALUE ZERO.
004210 01 �]�v                               PIC 9(3)  VALUE ZERO.
002878***********************************************************************
002879 01 �������.
002880     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
002881     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
002882     03 ������ʂo                     PIC X(2) VALUE SPACE.
002883     03 �g������o.
002890         05 �[������o.
002900             07 �ړ������o             PIC X(1).
002910             07 �ړ��s���o             PIC 9(3).
002920         05 �ڍא���o                 PIC X(2).
002930     03 �ʒm���o                     PIC X(2) VALUE SPACE.
002940     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
002950*
002960 01 �v�Z�@����N�v                     PIC 9(2).
002970* ���t�v�n�q�j
002980 01 �a��I���N�v                       PIC 9(4).
002990 01 �v�Z�@����.
003000    03 �v�Z�@����N                    PIC 9(4).
003010    03 �v�Z�@�����                  PIC 9(4).
003020 01 �v�Z�@����q REDEFINES �v�Z�@����.
003030    03 �v�Z�@���I                      PIC 9(2).
003040    03 �v�Z�@���t                      PIC 9(6).
003050    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
003060       05 �v�Z�@�N��                   PIC 9(4).
003070       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
003080         07 �v�Z�@�N                   PIC 9(2).
003090         07 �v�Z�@��                   PIC 9(2).
003100       05 �v�Z�@��                     PIC 9(2).
003110*
003120******************************************************************
003130*                          �A������                              *
003140******************************************************************
003150*
003160********************
003170* ���b�Z�[�W�\���L�[ *
003180********************
003184 01 �A���|�L�[ IS EXTERNAL.
003185    03  �A���|���b�Z�[�W               PIC N(20).
003186*
003190 01 �A���R�|�L�[ IS EXTERNAL.
003200    03  �A���R�|���b�Z�[�W             PIC N(20).
003210    03  �A���R�|���b�Z�[�W�P           PIC X(20).
003220*
       01 �A���|��ʏ��x�`�h�T�W�O IS EXTERNAL.
          03 �A���|�����a��N��.
             05 �A���|�����a��               PIC 9(1).
             05 �A���|�����N��.
                07 �A���|�����N              PIC 9(2).
                07 �A���|������              PIC 9(2).
          03 �A���|�쐬�a��N����.
             05 �A���|�쐬�a��N��.
                07 �A���|�쐬�a��            PIC 9(1).
                07 �A���|�쐬�N              PIC 9(2).
                07 �A���|�쐬��              PIC 9(2).
             05 �A���|�쐬��                 PIC 9(2).
       01 �A���|���̓f�[�^�x�`�h�T�W�O   IS EXTERNAL.
          03 �A���|�ی����                  PIC 9(2).
          03 �A���|�ی��Ҕԍ�                PIC X(10).
002351*
       01 �A��|�v���r���[ IS EXTERNAL.
          03 �A��|�v���r���[�敪            PIC 9.
003020*
000540************************************
000550* �v�����^�t�@�C���쐬�p           *
000560************************************
000570 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000580     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
000590     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
000600     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
000610     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
003470*
003480******************************************************************
003490*                      PROCEDURE  DIVISION                       *
003500******************************************************************
003510 PROCEDURE               DIVISION.
003520************
003530*           *
003540* ��������   *
003550*           *
003560************
002570     PERFORM �v�����^�t�@�C���쐬.
003581     PERFORM ������.
003980     PERFORM �{�p�����擾.
003583************
003590*           *
003600* �又��     *
003610*           *
003620************
003660     PERFORM �������.
003760************
003770*           *
003780* �I������   *
003790*           *
003800************
003810     PERFORM �I������.
003820     EXIT PROGRAM.
003830*
003840*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YAI585"              TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A��|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
003850*================================================================*
003860 ������ SECTION.
003870*
003880     PERFORM �t�@�C���I�[�v��.
003961*
003970     PERFORM �A�����ڑޔ�.
004400*================================================================*
004410 �A�����ڑޔ� SECTION.
004420*
004430     MOVE �A���|�����a��  TO �����a��v.
004440     MOVE �A���|�����N    TO �����N�v.
004450     MOVE �A���|������    TO �������v.
004460     MOVE �A���|�쐬�a��  TO ��o�a��v.
004470     MOVE �A���|�쐬�N    TO ��o�N�v.
004480     MOVE �A���|�쐬��    TO ��o���v.
004490     MOVE �A���|�쐬��    TO ��o���v.
004510*
004520*================================================================*
004530 �{�p�����擾 SECTION.
004540*
004550     MOVE ZERO  TO �{��|�{�p���ԍ�.
004560     READ �{�p�����}�X�^
004570     INVALID KEY
004580         CONTINUE
004590     NOT INVALID KEY
004600*
004610         MOVE �{��|�X�֔ԍ��P       TO �{�p���X�֔ԍ��P�v
004620         MOVE "-"                    TO �{�p���X�֔ԍ���؂v
004630         MOVE �{��|�X�֔ԍ��Q       TO �{�p���X�֔ԍ��Q�v
004640         MOVE �{��|��\�Җ�         TO ��\�Җ��v
004650         MOVE �{��|�ڍ��@��         TO �ڍ��@���v
004660         STRING �{��|�Z���P  DELIMITED BY SPACE
004670                �{��|�Z���Q  DELIMITED BY SPACE
004680           INTO �{�p���Z���v
004690         END-STRING
004700         MOVE �{��|�d�b�ԍ�         TO �{�p���d�b�ԍ��v
004701         MOVE �{��|�V�_���t�ԍ�     TO �_���t�ԍ��v
004710*
004720         MOVE �{��|������s��     TO ������s���v
004730         MOVE �{��|������s�x�X�� TO ������s�x�X���v
004740         MOVE �{��|�a�����         TO �a����ʂv
004750         MOVE �{��|��s�ԍ�         TO ��s�ԍ��v
004760         MOVE �{��|�X�ԍ�           TO �X�ԍ��v
004770         MOVE �{��|�����ԍ�         TO �����ԍ��v
004780         MOVE �{��|�������`�l�J�i   TO �������`�l�J�i�v
004781         MOVE �{��|�������`�l       TO �������`�l�v
004790         STRING ������s���v     DELIMITED BY SPACE
004800                " "                DELIMITED BY SIZE
004810                ������s�x�X���v DELIMITED BY SPACE
004820                INTO ��s���x�X���v
004830         END-STRING
004840         EVALUATE �a����ʂv
004850         WHEN 1
004860             MOVE NC"����" TO �a����ʃR�����g�v
004870         WHEN 2
004880             MOVE NC"����" TO �a����ʃR�����g�v
004890         WHEN OTHER
004900             MOVE SPACE    TO �a����ʃR�����g�v
004910         END-EVALUATE
004920*
004930     END-READ.
004940*================================================================*
004950 �t�@�C���I�[�v�� SECTION.
004960*
004970     OPEN INPUT   �����}�X�^
004980         MOVE NC"����" TO �t�@�C����.
004990         PERFORM �I�[�v���`�F�b�N.
005030     OPEN INPUT   �ی��҃}�X�^
005040         MOVE NC"�ی�" TO �t�@�C����.
005050         PERFORM �I�[�v���`�F�b�N.
005060     OPEN INPUT   �s�����}�X�^
005070         MOVE NC"�s��" TO �t�@�C����.
005080         PERFORM �I�[�v���`�F�b�N.
005120     OPEN INPUT   �{�p�����}�X�^
005130         MOVE NC"�{��" TO �t�@�C����.
005140         PERFORM �I�[�v���`�F�b�N.
005150     OPEN INPUT   ������}�X�^
005160         MOVE NC"����" TO �t�@�C����.
005170         PERFORM �I�[�v���`�F�b�N.
005200*================================================================*
005210 �I�[�v���`�F�b�N SECTION.
005220*
005230     IF ��ԃL�[  NOT =  "00"
005240         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
005250         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
005260         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005270                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
005280         ACCEPT  �L�[���� FROM CONS
005290         PERFORM �t�@�C����
005300         EXIT PROGRAM.
005310*================================================================*
005320 �t�@�C���� SECTION.
005330*
002990     IF ( �I�[�v���t���O = "YES" )
002991         CLOSE ����t�@�C��
003041     END-IF.
005340     CLOSE �����}�X�^     �ی��҃}�X�^
005350           �s�����}�X�^   �{�p�����}�X�^ ������}�X�^.
005370*================================================================*
005380 �I������ SECTION.
005390*
005400     PERFORM �t�@�C����.
005410*================================================================*
005420 �G���[�\�� SECTION.
005430*
005440     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
005450     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
005460     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
005470     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
005480     ACCEPT  �L�[���� FROM CONS.
005490     PERFORM �t�@�C����.
005500     EXIT PROGRAM.
005510*================================================================*
005520 ������� SECTION.
005530*
005570     OPEN INPUT  ��ƃt�@�C���Q.
005580         MOVE NC"��Q" TO �t�@�C����.
005590         PERFORM �I�[�v���`�F�b�N.
005611     MOVE �A���|�ی����    TO  ��Q�|�ی����.
005612     MOVE �A���|�ی��Ҕԍ�  TO  ��Q�|�ی��Ҕԍ�.
005617     READ ��ƃt�@�C���Q
           NOT INVALID KEY
005703         MOVE SPACE TO YAI585P
005704****                INITIALIZE YAI585P
005705         PERFORM �w�b�_�Z�b�g
005706         PERFORM ���׃Z�b�g
005707         PERFORM �󎚏���
005720     END-READ.
006570*
006580     CLOSE ��ƃt�@�C���Q.
006581*
006660*================================================================*
006670 ��ƃt�@�C���Q�Ǎ� SECTION.
006680*
006690     READ ��ƃt�@�C���Q NEXT
006700     AT END
006710         MOVE "YES" TO �I���t���O
006720     END-READ.
006721*================================================================*
006722 �󎚏���  SECTION.
006723*
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
013440*
006724     MOVE "YAI585P" TO  ��`�̖��o.
006725     MOVE SPACE     TO  ������ʂo.
006726     MOVE "SCREEN"  TO  ���ڌQ���o.
006727     WRITE YAI585P.
006728     PERFORM �G���[�����o.
006930*================================================================*
006940 �w�b�_�Z�b�g SECTION.
006950*
006982* �����̘a����擾
006983*     MOVE �����a��v         TO ���|�����敪.
006984*     READ �����}�X�^
006985*     INVALID KEY
006986*         MOVE SPACE          TO �����a���
006987*     NOT INVALID KEY
006988*         MOVE ���|��������   TO �����a���
006989*     END-READ.
006990*
      */�����C��/������20190514
037370     IF �����a��v > 4
              MOVE �����a��v         TO ���|�����敪
037380        READ �����}�X�^
037390        NOT INVALID KEY
037400            MOVE ���|��������   TO �{�p�a��
037410        END-READ
              MOVE "===="             TO �{�p�a�����
           END-IF.
      */�����C��/������20190514
006991     MOVE �����N�v           TO �����N.
006992     MOVE �������v           TO ������.
007001*
007211*     MOVE �{�p���Z���v       TO �Z��.
007220*     MOVE �{�p���X�֔ԍ��v   TO �X�֔ԍ�.
007230     MOVE ��\�Җ��v         TO ��\�Җ�.
007240     MOVE �ڍ��@���v         TO �ڍ��@��.
004972     MOVE �_���t�ԍ��P�v     TO �_���t�ԍ��P.
004972     MOVE �_���t�ԍ��Q�v     TO �_���t�ԍ��Q.
004972     MOVE �_���t�ԍ��R�v     TO �_���t�ԍ��R.
007250*     MOVE �{�p���d�b�ԍ��v   TO �d�b�ԍ�.
007254*
007260*     MOVE ��s���x�X���v     TO ��s���x�X��.
007270*     MOVE �a����ʃR�����g�v TO �a�����.
007280*     MOVE �����ԍ��v         TO �����ԍ�.
      */�ϔC�ҏ��Ή�
007281*     MOVE �������`�l�J�i�v   TO �������`�l�J�i.
007282*     MOVE �������`�l�v       TO �������`�l.
007281*     MOVE �������`�l�J�i�P�v   TO �������`�l�J�i.
007281*     MOVE �������`�l�J�i�Q�v   TO �������`�l�J�i�Q.
007282*     MOVE �������`�l�P�v       TO �������`�l.
007282*     MOVE �������`�l�Q�v       TO �������`�l�Q.
007283*
007284*     MOVE NC"�U����F"       TO �U����\��.
007285*
007292* / �ی���/
007300     MOVE ��Q�|�ی����     TO �ی���ʂv.
007310     MOVE ��Q�|�ی��Ҕԍ�   TO �ی��Ҕԍ��v.
007320     EVALUATE �ی���ʂv
007330     WHEN 1 THRU 4
007340     WHEN 6 THRU 9
007350*     WHEN 70 
007360*     WHEN 80
007370         PERFORM �ی��ҏ��擾
007380     WHEN 5
007390     WHEN 50 THRU 60
007400         PERFORM �s�������擾
007410     END-EVALUATE.
007420     PERFORM ���������擾.
007421*
007430     MOVE �ی��҈����v  TO �ی��Җ���.
007490*
      */�ی��Ҕԍ����󎚂���/081016
      *     IF ��Q�|�ی���� <= 09
      *         MOVE NC"�ی��Ҕԍ�"   TO �ی��Ҕԍ��\��
      *         MOVE ��Q�|�ی��Ҕԍ� TO �ی��Ҕԍ�
      *     END-IF.
007741*================================================================*
007742 ���׃Z�b�g SECTION.
007743*
007760     IF ��Q�|�{�l���� NOT = ZERO
007761        MOVE ��Q�|�{�l����     TO �{�l����
007762        MOVE ��Q�|�{�l��p�z   TO �{�l��p�z
007763        MOVE ��Q�|�{�l���S�z   TO �{�l���S�z
007770        MOVE ��Q�|�{�l�����z   TO �{�l�����z
007771     END-IF.
007772     IF ��Q�|�Ƒ����� NOT = ZERO
007774        MOVE ��Q�|�Ƒ�����     TO �Ƒ�����
007775        MOVE ��Q�|�Ƒ���p�z   TO �Ƒ���p�z
007776        MOVE ��Q�|�Ƒ����S�z   TO �Ƒ����S�z
007777        MOVE ��Q�|�Ƒ������z   TO �Ƒ������z
007778     END-IF.
007780*
008290*================================================================*
008300 �G���[�����o SECTION.
008310*
008320     IF �ʒm���o NOT = "00"
008330         DISPLAY NC"���[�G���["              UPON CONS
008340         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
008350         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
008360         DISPLAY NC"�g������o�F" �g������o UPON CONS
008370         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
008380                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008390         ACCEPT  �L�[���� FROM CONS
008400         PERFORM �t�@�C����
008410         EXIT PROGRAM
008420     END-IF.
008430*================================================================*
008440 �ی��ҏ��擾 SECTION.
008450*
008451     MOVE  SPACE         TO �����於�̂v.
008452     MOVE  SPACE         TO �x���������v.
008453     MOVE  ZERO          TO �ڔ���敪�v.
008454*
008460     MOVE �ی���ʂv     TO �ہ|�ی����.
008470     MOVE �ی��Ҕԍ��v   TO �ہ|�ی��Ҕԍ�.
008480     READ �ی��҃}�X�^
008490     INVALID KEY
008500         MOVE SPACE      TO �����於�̂v
008510         MOVE SPACE      TO �x���������v
008520     NOT INVALID KEY
008530         IF �ہ|��������敪 = 1
008540             MOVE �ہ|�ی����   TO ����|�ی����
008550             MOVE �ہ|�ی��Ҕԍ� TO ����|�ی��Ҕԍ�
008560             READ ������}�X�^
008570             INVALID KEY
008580                 MOVE SPACE             TO �����於�̂v
008590                 MOVE SPACE             TO �x���������v
008600             NOT INVALID KEY
008610                 MOVE ����|�ی��Җ���  TO �����於�̂v
008620                 MOVE ����|�x��������  TO �x���������v
008630             END-READ
008640         ELSE
008650             MOVE �ہ|�ی��Җ���        TO �����於�̂v
008660             MOVE �ہ|�x��������        TO �x���������v
008661             MOVE �ہ|�ڔ���敪        TO �ڔ���敪�v
008670         END-IF
008680     END-READ.
008690*================================================================*
008700 �s�������擾 SECTION.
008710*
008711     MOVE  SPACE         TO �����於�̂v.
008712     MOVE  SPACE         TO �x���������v.
008713*
008720     MOVE �ی���ʂv               TO �s�|������.
008730     MOVE �ی��Ҕԍ��v             TO �s�|�s�����ԍ�.
008740     READ �s�����}�X�^
008750     INVALID KEY
008760         MOVE SPACE                TO �����於�̂v
008770         MOVE SPACE                TO �x���������v
008780     NOT INVALID KEY
008790         IF �s�|������敪 = 1
008800             MOVE �ی���ʂv       TO ����|�ی����
008810             MOVE �ی��Ҕԍ��v     TO ����|�ی��Ҕԍ�
008820             READ ������}�X�^
008830             INVALID KEY
008840                 MOVE SPACE        TO �����於�̂v
008850                 MOVE SPACE        TO �x���������v
008860             NOT INVALID KEY
008868                 MOVE ����|�ی��Җ���   TO �����於�̂v
008869                 MOVE ����|�x��������   TO �x���������v
008890             END-READ
008900          ELSE
008908             MOVE �s�|�s��������   TO �����於�̂v
008909             MOVE �s�|�x��������   TO �x���������v
008930          END-IF
008940      END-READ.
008950*================================================================*
008960 ���������擾 SECTION.
008970*
008971     MOVE SPACE TO �ی��҈����v.
008972     IF �����於�̂v NOT = SPACE
008980         EVALUATE �ی���ʂv
008981         WHEN 2
008982             IF �ڔ���敪�v = 1
008983                MOVE SPACE            TO �����v
008984             ELSE
008985                MOVE "�Љ�ی�������" TO �����v
008986             END-IF
008987         WHEN 6
008988             IF �ڔ���敪�v = 1
008989*                MOVE "�i���فj"               TO �����v
                      CONTINUE
008991             ELSE
008992*                MOVE "�Љ�ی��������i���فj" TO �����v
008992                MOVE "�Љ�ی�������" TO �����v
008993             END-IF
008994         WHEN 7
008995*             MOVE "�i�D���j"       TO �����v
                   CONTINUE
009020         WHEN 3
009030             MOVE "���N�ی��g��"   TO �����v
009031         WHEN 4
009032             MOVE "���ϑg��"       TO �����v
009033         WHEN 8
009034             MOVE "�i�ސE�j"       TO �����v
009040         WHEN OTHER
009050             MOVE SPACE            TO �����v
009060         END-EVALUATE
009070*
009080         IF �x���������v = SPACE
009090             STRING  �����於�̂v  DELIMITED BY SPACE
009100                     �����v        DELIMITED BY SPACE
009120                    INTO �ی��҈����v
009130             END-STRING
009140         ELSE
009150             STRING  �����於�̂v  DELIMITED BY SPACE
009160                     �����v        DELIMITED BY SPACE
009170                     " "           DELIMITED BY SIZE
009180                     �x���������v  DELIMITED BY SPACE
009200                    INTO �ی��҈����v
009210             END-STRING
009220         END-IF
009221     END-IF.
009222*
009605*================================================================*
009606******************************************************************
009607 END PROGRAM YAI585.
009608******************************************************************
