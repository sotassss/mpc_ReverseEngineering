000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAZ582.
000060 AUTHOR.                 �r�c �K�q
000070*
000080*----------------------------------------------------------------*
000090*         ��o�p���׏����X�g�i�_+����޳�ޔŁj
000100*  �����N���o�[�W����
000110*         MED = YAZ582P
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2012-05-10
000140 DATE-COMPILED.          2012-05-10
000150*----------------------------------------------------------------*
000160******************************************************************
000170*            ENVIRONMENT         DIVISION                        *
000180******************************************************************
000190 ENVIRONMENT             DIVISION.
000200 CONFIGURATION           SECTION.
000210 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000220 OBJECT-COMPUTER.        FMV-DESKPOWER.
000230 SPECIAL-NAMES.          CONSOLE  IS  CONS
000240                         SYSERR   IS  MSGBOX.
000250 INPUT-OUTPUT            SECTION.
000260 FILE-CONTROL.
000270     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000280                             ORGANIZATION             IS  INDEXED
000290                             ACCESS MODE              IS  DYNAMIC
000300                             RECORD KEY               IS  ���|�����敪
000310                             FILE STATUS              IS  ��ԃL�[
000320                             LOCK        MODE         IS  AUTOMATIC.
000330     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000340                             ORGANIZATION             IS  INDEXED
000350                             ACCESS MODE              IS  DYNAMIC
000360                             RECORD KEY               IS  ���|�敪�R�[�h
000370                                                          ���|���̃R�[�h
000380                             FILE STATUS              IS  ��ԃL�[
000390                             LOCK        MODE         IS  AUTOMATIC.
000450     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS MODE              IS  DYNAMIC
000480                             RECORD KEY               IS  �ہ|�ی����
000490                                                          �ہ|�ی��Ҕԍ�
000500                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000510                                                          �ہ|�ی��Җ���
000520                                                          �ہ|�ی��Ҕԍ�
000530                             FILE STATUS              IS  ��ԃL�[
000540                             LOCK        MODE         IS  AUTOMATIC.
000450     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS MODE              IS  DYNAMIC
000480                             RECORD KEY               IS  �s�|������
000490                                                          �s�|�s�����ԍ�
000500                             ALTERNATE RECORD KEY     IS  �s�|������
000510                                                          �s�|�s��������
000520                                                          �s�|�s�����ԍ�
000530                             FILE STATUS              IS  ��ԃL�[
000540                             LOCK        MODE         IS  AUTOMATIC.
000400     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000410                             ORGANIZATION             IS  INDEXED
000420                             ACCESS MODE              IS  DYNAMIC
000430                             RECORD KEY               IS  ���|����敪
000440                             FILE STATUS              IS  ��ԃL�[
000450                             LOCK        MODE         IS  AUTOMATIC.
000330     SELECT  �{�p�����}�X�^ ASSIGN     TO        SEJOHOL
000340                             ORGANIZATION             IS  INDEXED
000350                             ACCESS MODE              IS  DYNAMIC
000360                             RECORD KEY               IS  �{��|�{�p���ԍ�
000370                             FILE STATUS              IS  ��ԃL�[
000380                             LOCK        MODE         IS  AUTOMATIC.
000130     SELECT  ���Z�v�g�e      ASSIGN      TO        RECEPTL
000140                             ORGANIZATION             IS  INDEXED
000150                             ACCESS MODE              IS  DYNAMIC
000160                             RECORD KEY               IS  ���Z�|�{�p�a��N��
000170                                                          ���Z�|���҃R�[�h
000180                                                          ���Z�|���Z���
000190                             ALTERNATE RECORD KEY     IS  ���Z�|���҃R�[�h
000200                                                          ���Z�|�{�p�a��N��
000210                                                          ���Z�|���Z���
000220                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000230                                                          ���Z�|�{�p�a��N��
000240                                                          ���Z�|���҃R�[�h
000250                                                          ���Z�|���Z���
000260                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000270                                                          ���Z�|���Z���
000280                                                          ���Z�|�����ی��Ҕԍ�
000290                                                          ���Z�|���҃R�[�h
000300                                                          ���Z�|�{�p�a��N��
000310                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000320                                                          ���Z�|�����ی��Ҕԍ�
000330                                                          ���Z�|���҃R�[�h
000340                                                          ���Z�|���Z���
000350                                                          ���Z�|�{�p�a��N��
000360                             FILE STATUS              IS  ��ԃL�[
000370                             LOCK        MODE         IS  AUTOMATIC.
000241     SELECT  ���ۏ��e      ASSIGN      TO        SEIHOJL
000242                             ORGANIZATION           IS INDEXED
000243                             ACCESS MODE            IS DYNAMIC
000244                             RECORD KEY             IS ���ہ|�{�p�a��N��
000245                                                       ���ہ|���҃R�[�h
000255                             ALTERNATE RECORD KEY   IS ���ہ|���҃R�[�h
000265                                                       ���ہ|�{�p�a��N��
000277                             FILE STATUS            IS ��ԃL�[
000278                             LOCK        MODE       IS AUTOMATIC.
000241     SELECT  �J�Џ��e      ASSIGN      TO        ROUSAIJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS �J�Ё|�{�p�a��N��
000245                                                         �J�Ё|���҃R�[�h
000255                             ALTERNATE RECORD KEY     IS �J�Ё|���҃R�[�h
000265                                                         �J�Ё|�{�p�a��N��
000277                             FILE STATUS              IS ��ԃL�[
000278                             LOCK        MODE         IS AUTOMATIC.
001130     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5811L.DAT"
001140                             ORGANIZATION             IS  INDEXED
001150                             ACCESS                   IS  DYNAMIC
001160                             RECORD      KEY          IS  ��P�|�������
001180                                                          ��P�|�۔�
001170                                                          ��P�|���
001190                                                          ��P�|�ی��Ҕԍ�
001170                                                          ��P�|�{�l�Ƒ��敪
001200                                                          ��P�|���҃R�[�h
001210                                                          ��P�|�{�p�a��N��
000360                             FILE STATUS              IS  ��ԃL�[
000370                             LOCK        MODE         IS  AUTOMATIC.
000630     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF001
000640                             SYMBOLIC    DESTINATION  IS "PRT"
000650                             FORMAT                   IS  ��`�̖��o
000660                             GROUP                    IS  ���ڌQ���o
000670                             PROCESSING  MODE         IS  ������ʂo
000680                             UNIT        CONTROL      IS  �g������o
000690                             FILE        STATUS       IS  �ʒm���o.
000700******************************************************************
000710*                      DATA DIVISION                             *
000720******************************************************************
000730 DATA                    DIVISION.
000740 FILE                    SECTION.
000750*                           �m�q�k��  �P�Q�W�n
000760 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000770     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000780*                           �m�q�k��  �P�Q�W�n
000790 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
000800     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001000*                           �m�q�k��  �R�Q�O�n
001010 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001020     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000970*                           �m�q�k��  �Q�T�U�n
000980 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000990     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
000810*                           �m�q�k��  �Q�T�U�n
000820 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
000830     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000970*                           �m�q�k��  �U�S�O�n
000980 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
000990     COPY SEJOHO         OF  XFDLIB  JOINING    �{�� AS  PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001080* 
000280 FD  ���ۏ��e          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   ����   AS  PREFIX.
001510*
000280 FD  �J�Џ��e          BLOCK   CONTAINS   1   RECORDS.
000281     COPY ROUSAIJ         OF  XFDLIB  JOINING   �J��   AS  PREFIX.
000870*
000880 FD  ����t�@�C��.
000890     COPY YAZ582P       OF  XMDLIB.
000900*
001520 FD  ��ƃt�@�C���P RECORD  CONTAINS 176 CHARACTERS.
001530 01  ��P�|���R�[�h.
001540     03  ��P�|���R�[�h�L�[.
001590         05  ��P�|�������.
                   07  ��P�|����                PIC 9(2).
001570             07  ��P�|��                  PIC 9(2).
001580             07  ��P�|�ێ�                PIC 9(1).
001720         05  ��P�|�۔�                    PIC X(6).
001600         05  ��P�|���                    PIC X(2).
001710         05  ��P�|�ی��Ҕԍ�              PIC X(10).
001700         05  ��P�|�{�l�Ƒ��敪            PIC 9(1).
001620         05  ��P�|���҃R�[�h.
001630             07  ��P�|���Ҕԍ�            PIC 9(6).
001640             07  ��P�|�}��                PIC X(1).
001650         05  ��P�|�{�p�a��N��.
001660             07  ��P�|�{�p�a��            PIC 9(1).
001670             07  ��P�|�{�p�N              PIC 9(2).
001680             07  ��P�|�{�p��              PIC 9(2).
001690     03  ��Q�|���R�[�h�f�[�^.
001600         05  ��P�|�ی����                PIC 9(2).
001600         05  ��P�|�������                PIC 9(2).
001740         05  ��P�|���Ҏ���                PIC X(50).
001750         05  ��P�|��ی��Ҏ���            PIC X(50).
001880         05  ��P�|������                  PIC 9(2).
001790         05  ��P�|��p�z                  PIC 9(7).
001800         05  ��P�|���S�z                  PIC 9(7).
001810         05  ��P�|�����z                  PIC 9(7).
002360         05  ��P�|�O���敪                PIC 9(1).
002370         05  FILLER                        PIC X(12).
001770*
001780*----------------------------------------------------------------*
001790******************************************************************
001800*                WORKING-STORAGE SECTION                         *
001810******************************************************************
001820 WORKING-STORAGE         SECTION.
001830 01 �L�[����                           PIC X    VALUE SPACE.
001840 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
001850 01 �I���t���O                         PIC X(3) VALUE SPACE.
001860 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
001870 01 �I���t���O�R                       PIC X(3) VALUE SPACE.
001880 01 �����t���O                         PIC X(4) VALUE SPACE.
001890 01 ��ƃt���O                         PIC X(3) VALUE SPACE.
001900 01 ��ƈړ��L�[                       PIC X(4) VALUE SPACE.
001910 01 �I���s�t���O                       PIC X(3) VALUE SPACE.
001920 01 �t�@�C����                         PIC N(2).
002000 01 �ی���ʂv                         PIC 9(2) VALUE ZERO.
002000 01 ���v                               PIC 9(3) VALUE ZERO.
002560 01 �ی��Ҕԍ��v                       PIC X(10) VALUE SPACE.
002010 01 ��������v                         PIC 9(2) VALUE ZERO.
001210 01 �I�[�v���t���O                     PIC X(3)   VALUE SPACE.
002040*
002050 01 �s�J�E���^                         PIC 9(2) VALUE 0.
002060 01 �ŃJ�E���^                         PIC 9(4) VALUE 0.
002070 01 �ő�s��                           PIC 9(2) VALUE 0.
002080 01 �w�b�_�s��                         PIC 9(2) VALUE 0.
002090 01 �����ړ��L�[                       PIC X(4) VALUE SPACE.
002100 01 �J�����g�����v                     PIC 9(1) VALUE ZERO.
002110*
002330 01 ����ԍ��v                         PIC X(10) VALUE SPACE.
001630 01 �ڍ��@���v                         PIC X(50) VALUE SPACE.
001640 01 ��\�Җ��v                         PIC X(50) VALUE SPACE.
002120 01 ���v�W�v�v.
002130    03 �������v�v                      PIC 9(4) VALUE ZERO.
002170    03 �����z���v�v                    PIC 9(8) VALUE ZERO.
002130    03 ���������v�v                    PIC 9(4) VALUE ZERO.
002170    03 �������z���v�v                  PIC 9(8) VALUE ZERO.
      *
       01 �����v                             PIC N(5) VALUE SPACE.
       01 �����v�q                           PIC N(3) VALUE SPACE.
002610 01 �����於�̂v.
002620    03 ��������於�̂v                PIC X(40) VALUE SPACE.
002300*
002320 01  �������̂v                        PIC N(1) VALUE SPACE.
002480*
002490 01 �����a��N���v.
002500     03 �����a��v                     PIC 9.
002510     03 �����N���v.
002520        05 �����N�v                    PIC 9(2).
002530        05 �������v                    PIC 9(2).
002540*
002550 01 ����{�p�N���v.
002560    03 ����������v                    PIC X(1) VALUE SPACE.
002560    03 ����{�p�N�v                    PIC X(2) VALUE SPACE.
002570    03 �����؂v                      PIC X    VALUE "/".
002580    03 ����{�p���v                    PIC X(2) VALUE SPACE.
002610*
002620* �G���[���b�Z�[�W�p
002630 01 �G���[���b�Z�[�W�v.
002640    03 �G���[���҃R�[�h�v              PIC X(7) VALUE SPACE.
002650    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
002660    03 �G���[���ʃR�[�h�v              PIC X(7) VALUE SPACE.
002670    03 FILLER                          PIC X(5) VALUE SPACE.
002680*
002690******
002700 01 �������.
002710     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
002720     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
002730     03 ������ʂo                     PIC X(2) VALUE SPACE.
002740     03 �g������o.
002750         05 �[������o.
002760             07 �ړ������o             PIC X(1).
002770             07 �ړ��s���o             PIC 9(3).
002780         05 �ڍא���o                 PIC X(2).
002790     03 �ʒm���o                     PIC X(2) VALUE SPACE.
002800     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
002810*
002980******************************************************************
002990*                          �A������                              *
003000******************************************************************
003010*
003020********************
003030* ���b�Z�[�W�\���L�[ *
003040********************
003050 01 �A���|�L�[ IS EXTERNAL.
003060    03  �A���|���b�Z�[�W               PIC N(20).
003070 01 �A���R�|�L�[ IS EXTERNAL.
003080    03  �A���R�|���b�Z�[�W             PIC N(20).
003090    03  �A���R�|���b�Z�[�W�P           PIC X(20).
003100*
003910 01 �A���|��ʏ��x�`�y�T�W�O   IS EXTERNAL.
003920    03 �A���|�����a��N��.
003930       05 �A���|�����a��               PIC 9.
003940       05 �A���|�����N                 PIC 9(2).
003950       05 �A���|������                 PIC 9(2).
          03 �A���|�v���r���[�敪            PIC 9(1).
      *
       01 �A��|������x�`�y�T�W�O IS EXTERNAL GLOBAL.
          03 �A��|��o�a��N��.
             05 �A��|��o�a��               PIC 9.
             05 �A��|��o�N                 PIC 9(2).
             05 �A��|��o��                 PIC 9(2).
004320*
003910 01 �A��|������x�`�y�T�W�P   IS EXTERNAL.
003930    03 �A��|���ی���               PIC 9(4).
003940    03 �A��|���ې����z             PIC 9(8).
003930    03 �A��|�ސE����               PIC 9(4).
003940    03 �A��|�ސE�����z             PIC 9(8).
003930    03 �A��|�V�l����               PIC 9(4).
003940    03 �A��|�V�l�����z             PIC 9(8).
003930    03 �A��|�㍂����               PIC 9(4).
003940    03 �A��|�㍂�����z             PIC 9(8).
003930    03 �A��|���g����               PIC 9(4).
003940    03 �A��|���g�����z             PIC 9(8).
003930    03 �A��|���ۍ��v����           PIC 9(4).
003940    03 �A��|���ۍ��v�����z         PIC 9(8).
003930    03 �A��|�Еی���               PIC 9(4).
003940    03 �A��|�Еې����z             PIC 9(8).
003930    03 �A��|�����               PIC 9(4).
003940    03 �A��|������z             PIC 9(8).
003930    03 �A��|�D������               PIC 9(4).
003940    03 �A��|�D�������z             PIC 9(8).
003930    03 �A��|�g������               PIC 9(4).
003940    03 �A��|�g�������z             PIC 9(8).
003930    03 �A��|���q����               PIC 9(4).
003940    03 �A��|���q�����z             PIC 9(8).
003930    03 �A��|���ό���               PIC 9(4).
003940    03 �A��|���ϐ����z             PIC 9(8).
003930    03 �A��|�픚����               PIC 9(4).
003940    03 �A��|�픚�����z             PIC 9(8).
003930    03 �A��|��Q����               PIC 9(4).
003940    03 �A��|��Q�����z             PIC 9(8).
003930    03 �A��|��q����               PIC 9(4).
003940    03 �A��|��q�����z             PIC 9(8).
003930    03 �A��|��������               PIC 9(4).
003940    03 �A��|���������z             PIC 9(8).
003930    03 �A��|��������               PIC 9(4).
003940    03 �A��|���������z             PIC 9(8).
003930    03 �A��|���ی���               PIC 9(4).
003940    03 �A��|���ې����z             PIC 9(8).
003930    03 �A��|�J�Ќ���               PIC 9(4).
003940    03 �A��|�J�А����z             PIC 9(8).
003930    03 �A��|��������               PIC 9(4).
003940    03 �A��|���������z             PIC 9(8).
003930    03 �A��|�����               PIC 9(4).
003940    03 �A��|������z             PIC 9(8).
003930    03 �A��|���v����               PIC 9(5).
003940    03 �A��|���v�����z             PIC 9(9).
003260*
000540************************************
000550* �v�����^�t�@�C���쐬�p           *
000560************************************
000570 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000580     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
000590     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
000600     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
000610     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
002980*
003270******************************************************************
003280*                      PROCEDURE  DIVISION                       *
003290******************************************************************
003300 PROCEDURE               DIVISION.
003310************
003320*           *
003330* ��������   *
003340*           *
003350************
002560     MOVE SPACE TO �I�[�v���t���O.
002570     PERFORM �v�����^�t�@�C���쐬.
003360     PERFORM ������.
003370************
003380*           *
003390* �又��     *
003400*           *
003410************
002790     PERFORM �{�p�����}�X�^�Ǎ�.
003420     PERFORM �������.
003430************
003440*           *
003450* �I������   *
003460*           *
003470************
003480     PERFORM �I������.
003490     EXIT PROGRAM.
003500*
003510*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002971     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
002972*
002973*   �g�p���钠�[�v���O�������Z�b�g
002974     MOVE "YAZ582"             TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
003520*================================================================*
003530 ������ SECTION.
003540*
004080     OPEN INPUT  ��ƃt�@�C���P.
004090         MOVE NC"���" TO �t�@�C����.
004100         PERFORM �I�[�v���`�F�b�N.
004110     OPEN INPUT  �����}�X�^
004120         MOVE NC"����" TO �t�@�C����.
004130         PERFORM �I�[�v���`�F�b�N.
004140     OPEN INPUT  ���̃}�X�^
004150         MOVE NC"����" TO �t�@�C����.
004160         PERFORM �I�[�v���`�F�b�N.
004180     OPEN INPUT �ی��҃}�X�^.
004190         MOVE NC"�ی��҃}�X�^" TO �t�@�C����.
004200         PERFORM �I�[�v���`�F�b�N.
003410     OPEN INPUT �s�����}�X�^.
003420         MOVE NC"�s�����}�X�^" TO �t�@�C����.
003430         PERFORM �I�[�v���`�F�b�N.
004170     OPEN INPUT  ������}�X�^
004180         MOVE NC"������" TO �t�@�C����.
004190         PERFORM �I�[�v���`�F�b�N.
002960     OPEN INPUT �{�p�����}�X�^.
002970         MOVE NC"�{��" TO �t�@�C����.
002980         PERFORM �I�[�v���`�F�b�N.
006630     OPEN INPUT ���Z�v�g�e.
006640         MOVE NC"���Z" TO �t�@�C����.
006650         PERFORM �I�[�v���`�F�b�N.
006630     OPEN INPUT ���ۏ��e.
006640         MOVE NC"����" TO �t�@�C����.
006650         PERFORM �I�[�v���`�F�b�N.
006630     OPEN INPUT �J�Џ��e.
006640         MOVE NC"�J��" TO �t�@�C����.
006650         PERFORM �I�[�v���`�F�b�N.
004250*================================================================*
004260 �I�[�v���`�F�b�N SECTION.
004270*
004280     IF ��ԃL�[  NOT =  "00"
004290         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
004300         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
004310         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004320                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
004330         ACCEPT  �L�[���� FROM CONS
004340         PERFORM �t�@�C����
004350         EXIT PROGRAM.
004360*================================================================*
004370 �t�@�C���� SECTION.
004380*
003570     IF ( �I�[�v���t���O = "YES" )
003580         CLOSE ����t�@�C��
003590     ELSE
003600         MOVE  NC"�@�@�f�[�^���O���ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
003610         CALL   "MSG001"
003620         CANCEL "MSG001"
003630     END-IF.
003640*
004390     CLOSE ��ƃt�@�C���P  ������}�X�^  ���̃}�X�^  �{�p�����}�X�^
                 �ی��҃}�X�^    �s�����}�X�^    �����}�X�^
                 ���Z�v�g�e      ���ۏ��e      �J�Џ��e.
004410*================================================================*
004420 �I������ SECTION.
004430*
004440     PERFORM �t�@�C����.
004450*================================================================*
004460 �G���[�\�� SECTION.
004470*
004480     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
004490     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
004500     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
004510     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
004520     ACCEPT  �L�[���� FROM CONS.
004530     PERFORM �t�@�C����.
004540     EXIT PROGRAM.
005790*================================================================*
005800 �G���[�\���q SECTION.
005810*
005820     DISPLAY NC"�t�@�C���Ǎ��G���[" �t�@�C���� UPON CONS.
005830     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
005840     ACCEPT  �L�[���� FROM CONS.
005850     PERFORM �t�@�C����.
005860     EXIT PROGRAM.
004550*================================================================*
004560 ������� SECTION.
004570*
004580     MOVE 45    TO �ő�s��.
004590     MOVE 2     TO �w�b�_�s��.
004600*
004610     MOVE SPACE TO �I���t���O.
004620*
004630     MOVE ZERO  TO �s�J�E���^.
004640     MOVE ZERO  TO �ŃJ�E���^.
004660     PERFORM ���v�l������.
004660     PERFORM �����v�l������.
004670     PERFORM ��ƃt�@�C���P�Ǎ�.
004680     IF  �I���t���O = "YES"
004690         MOVE  NC"�@�Y������f�[�^������܂���B" TO �A���|���b�Z�[�W
004700         CALL   "MSG001"
004710         CANCEL "MSG001"
004720         PERFORM �t�@�C����
004730         MOVE 99 TO PROGRAM-STATUS
004740         EXIT PROGRAM
004750     END-IF.
004840*
004850     MOVE 1     TO �ŃJ�E���^
004860     PERFORM �w�b�_�������
004870*
004880     PERFORM UNTIL �I���t���O = "YES"
004890        IF ( �s�J�E���^ >= �ő�s�� )
004970           PERFORM ���ŏ���
004980           COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
004990           PERFORM �w�b�_�������
005110        END-IF
004830        MOVE ��P�|����       TO ��������v
005120        PERFORM ���׃w�b�_�������
004890        IF ( �s�J�E���^ >= �ő�s�� )
004970           PERFORM ���ŏ���
004980           COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
004990           PERFORM �w�b�_�������
005110        END-IF
004880        PERFORM UNTIL ( �I���t���O = "YES" ) OR
                            ( ��P�|���� NOT = ��������v)
004830           MOVE ��P�|�ی��Ҕԍ� TO �ی��Ҕԍ��v
005120           PERFORM ���ׂP�������
004890           IF ( �s�J�E���^ >= �ő�s�� )
004970              PERFORM ���ŏ���
004980              COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
004990              PERFORM �w�b�_�������
005110           END-IF
004880           PERFORM UNTIL ( �I���t���O = "YES" ) OR
                               ( ��P�|�ی��Ҕԍ� NOT = �ی��Ҕԍ��v)
004890              IF ( �s�J�E���^ >= �ő�s�� )
004970                 PERFORM ���ŏ���
004980                 COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
004990                 PERFORM �w�b�_�������
005110              END-IF
005120              PERFORM ���ׂQ�������
005130              PERFORM ���v�z�݌v
005340*
005380              MOVE ��P�|����       TO ��������v
005380              MOVE ��P�|��         TO ���v
004830              MOVE ��P�|�ی��Ҕԍ� TO �ی��Ҕԍ��v
005390*
005400              PERFORM ��ƃt�@�C���P�Ǎ�
                 END-PERFORM
005630           PERFORM ���v�������
004660           PERFORM ���v�l������
                 IF ( ��������v = 1 ) AND
                    (( ���v NOT = ��P�|�� ) OR (��������v NOT = ��P�|����))
005630              PERFORM �����v�������
004660              PERFORM �����v�l������
004890              IF ( �s�J�E���^ >= �ő�s�� )
004970                 PERFORM ���ŏ���
004980                 COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
004990                 PERFORM �w�b�_�������
005110              END-IF
005380              MOVE ��P�|��         TO ���v
                    IF ( ��P�|���� = 1 )
005120                 PERFORM ���׃w�b�_�������
004890                 IF ( �s�J�E���^ >= �ő�s�� )
004970                    PERFORM ���ŏ���
004980                    COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
004990                    PERFORM �w�b�_�������
                       END-IF
005110              END-IF
                 END-IF
005600*
005610        END-PERFORM
005630        PERFORM ���ۍ��v�������
005800     END-PERFORM.
005850*
005870*================================================================*
005880 ���ŏ���  SECTION.
005890*
005900     MOVE SPACE TO YAZ582P.
005910     INITIALIZE YAZ582P.
005920     MOVE "YAZ582P" TO  ��`�̖��o.
005930     MOVE "CT"       TO  ������ʂo.
005940     MOVE "PAGE"     TO  �g������o.
005950     MOVE SPACE      TO  ���ڌQ���o.
005960     WRITE YAZ582P.
005970     PERFORM �G���[�����o.
005980     MOVE SPACE      TO  �g������o.
006030*
006040*================================================================*
006050 �w�b�_������� SECTION.
006060*
006120     MOVE SPACE TO YAZ582P.
006130     INITIALIZE YAZ582P.
006260     MOVE �ŃJ�E���^     TO ��.
006270*
006280* �Ώې����a����擾
006290     MOVE �A��|��o�a��  TO ���|�����敪.
006300     READ �����}�X�^
006310     INVALID KEY
006320         MOVE SPACE         TO �����a���
006330     NOT INVALID KEY
006340         MOVE ���|��������  TO �����a���
006350     END-READ.
006360     MOVE �A��|��o�N      TO �����N.
006370     MOVE �A��|��o��      TO ������.
006380*
006390     MOVE NC"������"        TO �����\��.
006400*
           MOVE �ڍ��@���v        TO �ڍ��@��.
           MOVE ����ԍ��v        TO ����ԍ�.
006080     PERFORM �w�b�_�󎚂P.
006420*
006430     IF �ŃJ�E���^ = 1
007330         MOVE SPACE TO YAZ582P
007340         INITIALIZE YAZ582P
 06440         MOVE �A��|���v����    TO �S����
006450         MOVE �A��|���v�����z  TO �S�����z
               MOVE ��\�Җ��v        TO �{�p�Җ�
006080         PERFORM �w�b�_�󎚂Q
006460     END-IF.
007320*
007330     MOVE SPACE TO YAZ582P.
007340     INITIALIZE YAZ582P.
006080     PERFORM �w�b�_�󎚂R.
006510*
003230*================================================================*
003240 �{�p�����}�X�^�Ǎ� SECTION.
003250*
003260     MOVE ZERO TO �{��|�{�p���ԍ�.
003270     READ �{�p�����}�X�^
003280     INVALID KEY
003290         MOVE NC"�{��" TO �t�@�C����
003300         PERFORM �G���[�\���q
003310         PERFORM �t�@�C����
003320         MOVE 99 TO PROGRAM-STATUS
003330         EXIT PROGRAM
003340     NOT INVALID KEY
003400         MOVE �{��|�ڍ��@��         TO �ڍ��@���v
003430         MOVE �{��|�ڍ��t�����ԍ� TO ����ԍ��v
003410         MOVE �{��|��\�Җ�         TO ��\�Җ��v
003660*
003670     END-READ.
007160*================================================================*
007170 �w�b�_�󎚂P  SECTION.
007180*
006050     IF ( �I�[�v���t���O NOT = "YES" )
006060        MOVE "YES" TO �I�[�v���t���O
006070        OPEN I-O  ����t�@�C��
006080        PERFORM �G���[�����o
006090     END-IF.
005410*
007190     MOVE "YAZ582P" TO  ��`�̖��o.
007200     MOVE SPACE      TO  ������ʂo.
007210     MOVE "HEAD01"   TO  ���ڌQ���o.
007220     WRITE YAZ582P.
007230     PERFORM �G���[�����o.
007240     MOVE �w�b�_�s�� TO �s�J�E���^.
007160*================================================================*
007170 �w�b�_�󎚂Q  SECTION.
007180*
007190     MOVE "YAZ582P" TO  ��`�̖��o.
007200     MOVE SPACE      TO  ������ʂo.
007210     MOVE "HEAD02"   TO  ���ڌQ���o.
007220     WRITE YAZ582P.
007230     PERFORM �G���[�����o.
008380     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
007160*================================================================*
007170 �w�b�_�󎚂R  SECTION.
007180*
007190     MOVE "YAZ582P" TO  ��`�̖��o.
007200     MOVE SPACE      TO  ������ʂo.
007210     MOVE "HEAD03"   TO  ���ڌQ���o.
007220     WRITE YAZ582P.
007230     PERFORM �G���[�����o.
008380     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
006520*================================================================*
006530 ���׃w�b�_������� SECTION.
006540*
007280     PERFORM ���׃w�b�_�Z�b�g.
007290     PERFORM ���׃w�b�_��.
007300*================================================================*
007310 ���׃w�b�_�Z�b�g SECTION.
007320*
007330     MOVE SPACE TO YAZ582P.
007340     INITIALIZE YAZ582P.
           MOVE SPACE TO �����v.
007320*
013500**********************************************
013510*  �ėp�������:�ی�����(�ی���ʃR�[�h)     *
013520*    1:����(1)�A�ސE(8)�A�㍂(5)             *
013530*    2:�Е�(2)                               *
013540*    3:����(2)                               *
013550*    4:�D��(7)                               *
013560*    5:�g��(3)                               *
013570*    6:���q��(9)                             *
013580*    7:����(4)                               *
013590*    8:�픚(54)                              *
013590*    9:��Q(53)                              *
013590*   10:��q(52)                              *
013590*   11:�q��(55,60)                           *
013590*   12:�S�P�V(51)                            *
013590*   13:����(85)                              *
013590***   14:�J��(70)                              *
013590***   15:����(80)                              *
013630*                                            *
013640*  ���ی���� 06:���ق� 02:����֊܂߂�B    *
      *                                            *
      *  �{�@���ԍ�                                *
      *  ���یn�̂� 1:���� 2:���g 3:�㍂           *
013650**********************************************
007030*
           EVALUATE ��������v
           WHEN 1
              MOVE 13                    TO ���|�敪�R�[�h
              MOVE ��P�|��              TO ���|���̃R�[�h
              READ ���̃}�X�^
              INVALID KEY
                  MOVE SPACE             TO �����v�q
              NOT INVALID KEY
                  MOVE ���|��������      TO �����v�q
              END-READ
              STRING NC"�m"         DELIMITED BY SIZE
                     �����v�q       DELIMITED BY SPACE
                     NC"�n"         DELIMITED BY SIZE
                INTO �����v
              END-STRING
              MOVE �����v                TO ����
           WHEN 2
              MOVE NC"�Љ�ی�"          TO �ی����
              MOVE �A��|�Еی���        TO ��ʌ���
              MOVE �A��|�Еې����z      TO ��ʐ����z
           WHEN 3
              MOVE NC"������"        TO �ی����
              MOVE �A��|�����        TO ��ʌ���
              MOVE �A��|������z      TO ��ʐ����z
           WHEN 4
              MOVE NC"�D��"              TO �ی����
              MOVE �A��|�D������        TO ��ʌ���
              MOVE �A��|�D�������z      TO ��ʐ����z
           WHEN 5
              MOVE NC"���N�ی��g��"      TO �ی����
              MOVE �A��|�g������        TO ��ʌ���
              MOVE �A��|�g�������z      TO ��ʐ����z
           WHEN 6
              MOVE NC"���q��"            TO �ی����
              MOVE �A��|���q����        TO ��ʌ���
              MOVE �A��|���q�����z      TO ��ʐ����z
           WHEN 7
              MOVE NC"���ϕی�"          TO �ی����
              MOVE �A��|���ό���        TO ��ʌ���
              MOVE �A��|���ϐ����z      TO ��ʐ����z
           WHEN 8
              MOVE NC"�����i�����j"      TO �ی����
              MOVE �A��|�픚����        TO ��ʌ���
              MOVE �A��|�픚�����z      TO ��ʐ����z
           WHEN 9
              MOVE NC"�����i��j"        TO �ی����
              MOVE �A��|��Q����        TO ��ʌ���
              MOVE �A��|��Q�����z      TO ��ʐ����z
           WHEN 10
              MOVE NC"�����i�e�j"        TO �ی����
              MOVE �A��|��q����        TO ��ʌ���
              MOVE �A��|��q�����z      TO ��ʐ����z
           WHEN 11
              MOVE NC"�����i�q�j"        TO �ی����
              MOVE �A��|��������        TO ��ʌ���
              MOVE �A��|���������z      TO ��ʐ����z
           WHEN 12
              MOVE NC"�����i���j"        TO �ی����
              MOVE �A��|��������        TO ��ʌ���
              MOVE �A��|���������z      TO ��ʐ����z
           WHEN 13
              MOVE NC"�����ی�"          TO �ی����
              MOVE �A��|���ی���        TO ��ʌ���
              MOVE �A��|���ې����z      TO ��ʐ����z
      *     WHEN 14
      *        MOVE NC"�J��"              TO �ی����
      *        MOVE �A��|�J�Ќ���        TO ��ʌ���
      *        MOVE �A��|�J�А����z      TO ��ʐ����z
      *     WHEN 15
      *        MOVE NC"������"            TO �ی����
      *        MOVE �A��|��������        TO ��ʌ���
      *        MOVE �A��|���������z      TO ��ʐ����z
      *     WHEN 16
      *        MOVE NC"����"              TO �ی����
      *        MOVE �A��|�����        TO ��ʌ���
      *        MOVE �A��|������z      TO ��ʐ����z
           END-EVALUATE.
007150*
008310*================================================================*
008320 ���׃w�b�_��  SECTION.
008330*
           IF ��������v = 1
008350        MOVE "GRP006"    TO  ���ڌQ���o
           ELSE
008350        MOVE "GRP001"    TO  ���ڌQ���o
           END-IF.
008340     MOVE "YAZ582P"  TO  ��`�̖��o.
008360     WRITE YAZ582P.
008370     PERFORM �G���[�����o.
008380     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
007250*================================================================*
007260 ���ׂP������� SECTION.
007270*
007280     PERFORM ���ׂP�Z�b�g.
007290     PERFORM ���ׂP��.
007300*================================================================*
007310 ���ׂP�Z�b�g SECTION.
007320*
007330     MOVE SPACE TO YAZ582P.
007340     INITIALIZE YAZ582P.
007350*
007360     MOVE ��P�|�ی��Ҕԍ�    TO �ی��Ҕԍ�.
           EVALUATE TRUE
           WHEN (��P�|�ی���� = 05)
           WHEN (��P�|�ی���� <= 60) AND (��P�|�ی���� >= 50)
              PERFORM �s�������擾
      *     WHEN ��P�|�ی���� = 70 
      *        MOVE ��P�|�{�p�a��N�� TO �J�Ё|�{�p�a��N��
      *        MOVE ��P�|���҃R�[�h   TO �J�Ё|���҃R�[�h
      *        READ �J�Џ��e
      *        NOT INVALID KEY
      *           MOVE �J�Ё|�J�Ў��Ə�����  TO �����於�̂v
      *        END-READ
           WHEN ��P�|�ی���� = 85 
              MOVE ��P�|�{�p�a��N�� TO ���ہ|�{�p�a��N��
              MOVE ��P�|���҃R�[�h   TO ���ہ|���҃R�[�h
              READ ���ۏ��e
              NOT INVALID KEY
                 MOVE ���ہ|���ێs������  TO �����於�̂v
              END-READ
           WHEN OTHER
              PERFORM ��������擾
           END-EVALUATE.
           MOVE �����於�̂v        TO �ی��Җ���.
008080*
008310*================================================================*
008320 ���ׂP��  SECTION.
008330*
008340     MOVE "YAZ582P"  TO  ��`�̖��o.
008350     MOVE "GRP002"    TO  ���ڌQ���o.
008360     WRITE YAZ582P.
008370     PERFORM �G���[�����o.
008380     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
008390*================================================================*
008400 ���ׂQ������� SECTION.
008410*
008420     PERFORM ���ׂQ�Z�b�g.
008430     PERFORM ���ׂQ��.
008440*================================================================*
008450 ���ׂQ�Z�b�g SECTION.
008460*
007330     MOVE SPACE TO YAZ582P.
007340     INITIALIZE YAZ582P.
007350*
007370     MOVE ��P�|���҃R�[�h    TO ���҃R�[�h.
007380     MOVE ��P�|���Ҏ���      TO ���Ҏ���.
           IF ( ��P�|�ی���� NOT = 05 ) AND ( ��P�|�ی���� NOT = 09 ) AND
              ( ��P�|�ی���� < 50 )
007390        IF ��P�|�{�l�Ƒ��敪 = 1
007400            MOVE NC"�i�{�l�j"    TO �{�l�Ƒ��敪
007410        ELSE
007400            MOVE NC"�i�Ƒ��j"    TO �{�l�Ƒ��敪
007430        END-IF
007430     END-IF.
007390     IF ��P�|�{�l�Ƒ��敪 NOT = 1
007420         MOVE ��P�|��ی��Ҏ���  TO ��ی��Ҏ���
007430     END-IF.
007530**
      *     IF ��P�|�{�p�a��N�� NOT = ZERO
006290        MOVE ��P�|�{�p�a��   TO ���|�����敪
006300        READ �����}�X�^
006310        INVALID KEY
006320            MOVE SPACE        TO ����������v
006330        NOT INVALID KEY
006340            MOVE ���|������   TO ����������v
006350        END-READ
              IF ��P�|�{�p�a�� = 4
                 MOVE "H"           TO ����������v
              END-IF
              MOVE ��P�|�{�p�N     TO ����{�p�N�v
              MOVE ��P�|�{�p��     TO ����{�p���v
              MOVE ����{�p�N���v   TO �{�p�N��
      *     END-IF.
007560     MOVE ��P�|������        TO ������.
007560     MOVE ��P�|��p�z        TO ��p�z.
007570     MOVE ��P�|���S�z        TO ���S�z.
007580     MOVE ��P�|�����z        TO �����z.
007580     MOVE ��P�|�O���敪      TO �O���敪.
           IF ( ��P�|�ی���� < 50 )
              EVALUATE ��P�|�������
              WHEN 52
                 MOVE NC"�e"        TO ����
              WHEN 53
                 MOVE NC"��"        TO ����
              WHEN 54
                 MOVE NC"��"        TO ����
              WHEN 55
              WHEN 60
                 MOVE NC"�q"        TO ����
              END-EVALUATE
           END-IF.
008680*
008920*================================================================*
008930 ���ׂQ��  SECTION.
008940*
008950     MOVE "YAZ582P"  TO  ��`�̖��o.
008960     MOVE "GRP003"    TO  ���ڌQ���o.
008970     WRITE YAZ582P.
008980     PERFORM �G���[�����o.
008990     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
010110*================================================================*
010120 ��ƃt�@�C���P�Ǎ� SECTION.
010130*
010140     READ ��ƃt�@�C���P NEXT
010150     AT END
010160         MOVE "YES" TO �I���t���O
010170     END-READ.
010180*
010190*================================================================*
010200 ���v�z�݌v SECTION.
010210*
010220     COMPUTE �������v�v     = �������v�v   + 1.
010260     COMPUTE �����z���v�v   = �����z���v�v + ��P�|�����z.
010210*
010220     COMPUTE ���������v�v   = ���������v�v   + 1.
010260     COMPUTE �������z���v�v = �������z���v�v + ��P�|�����z.
010430*
010440*================================================================*
010450 ���v������� SECTION.
010460*
010520     MOVE SPACE TO YAZ582P.
010530     INITIALIZE YAZ582P.
010540*
           IF �������v�v > 1
              MOVE NC"���v"             TO ���v�b�l
010550        MOVE �������v�v           TO �������v
010590        MOVE �����z���v�v         TO �����z���v
010480        PERFORM ���v��
004890        IF ( �s�J�E���^ >= �ő�s�� )
004970           PERFORM ���ŏ���
004980           COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
004990           PERFORM �w�b�_�������
005110        END-IF
           END-IF.
010600*================================================================*
010610 ���v�� SECTION.
010620*
010630     MOVE "YAZ582P"  TO  ��`�̖��o.
010640     MOVE "GRP004"    TO  ���ڌQ���o.
010650     WRITE YAZ582P.
010660     PERFORM �G���[�����o.
010670     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
010440*================================================================*
010450 �����v������� SECTION.
010460*
010520     MOVE SPACE TO YAZ582P.
010530     INITIALIZE YAZ582P.
010540*
           MOVE NC"�s���{���@�����v" TO ���v�b�l.
010550     MOVE ���������v�v         TO �������v.
010590     MOVE �������z���v�v       TO �����z���v.
010480     PERFORM �����v��.
010600*================================================================*
010610 �����v�� SECTION.
010620*
010630     MOVE "YAZ582P"  TO  ��`�̖��o.
010640     MOVE "GRP004"    TO  ���ڌQ���o.
010650     WRITE YAZ582P.
010660     PERFORM �G���[�����o.
010670     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
010440*================================================================*
010450 ���ۍ��v������� SECTION.
010460*
010520     MOVE SPACE TO YAZ582P.
010530     INITIALIZE YAZ582P.
010540*
           IF ��������v = 1
004890        IF ( �s�J�E���^ > �ő�s�� - 4 )
004970           PERFORM ���ŏ���
004980           COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
004990           PERFORM �w�b�_�������
005110        END-IF
              MOVE �A��|���ی���       TO ���ی���
              MOVE �A��|���ې����z     TO ���ې����z
              MOVE �A��|�ސE����       TO �ސE����
              MOVE �A��|�ސE�����z     TO �ސE�����z
              MOVE �A��|�V�l����       TO �V�l����
              MOVE �A��|�V�l�����z     TO �V�l�����z
              MOVE �A��|�㍂����       TO �㍂����
              MOVE �A��|�㍂�����z     TO �㍂�����z
              MOVE �A��|���g����       TO ���g����
              MOVE �A��|���g�����z     TO ���g�����z
              MOVE �A��|���ۍ��v����   TO ���ۍ��v����
              MOVE �A��|���ۍ��v�����z TO ���ۍ��v�����z
010480        PERFORM ���ۍ��v��
004890        IF ( �s�J�E���^ >= �ő�s�� )
004970           PERFORM ���ŏ���
004980           COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
004990           PERFORM �w�b�_�������
005110        END-IF
           END-IF.
010600*================================================================*
010610 ���ۍ��v�� SECTION.
010620*
010630     MOVE "YAZ582P"  TO  ��`�̖��o.
010640     MOVE "GRP005"    TO  ���ڌQ���o.
010650     WRITE YAZ582P.
010660     PERFORM �G���[�����o.
010670     COMPUTE �s�J�E���^ = �s�J�E���^ + 4.
011190*================================================================*
011200 ���v�l������ SECTION.
011210*
011220     MOVE ZERO  TO �������v�v.
011260     MOVE ZERO  TO �����z���v�v.
011190*================================================================*
011200 �����v�l������ SECTION.
011210*
011220     MOVE ZERO  TO ���������v�v.
011260     MOVE ZERO  TO �������z���v�v.
006180*================================================================*
006190 ��������擾 SECTION.
006200*
006210*********************************************************
006220* �A���f�[�^����ی��҃}�X�^��萿������擾����B      *
006230* �� ������...... �����於�̂v�Ɋi�[                    *
006240*********************************************************
006250     MOVE ��P�|�ی����      TO �ہ|�ی����.
006260     MOVE ��P�|�ی��Ҕԍ�    TO �ہ|�ی��Ҕԍ�.
006270     MOVE SPACE               TO �����於�̂v.
006280     READ �ی��҃}�X�^
006290     INVALID KEY
006300         MOVE SPACE           TO �����於�̂v
006310     NOT INVALID KEY
006320*         MOVE �ہ|�ی��Җ���  TO �����於�̂v
029020          EVALUATE �ہ|�ی���� 
029130* �g���͎x�����܂ň�
029140          WHEN  03
029150              STRING �ہ|�ی��Җ���  DELIMITED BY SPACE
029160                     "���N�ی��g��"  DELIMITED BY SIZE
029170                     "  "            DELIMITED BY SIZE
029180                     �ہ|�x��������  DELIMITED BY SPACE
029190                     INTO �����於�̂v
029200              END-STRING
029210* ���ς͎x�����܂ň�
029220          WHEN  04
                    IF �ہ|�ی��Ҕԍ� = "34130021"
                        MOVE �ہ|�ی��Җ��� TO �����於�̂v
                    ELSE
029230                  STRING �ہ|�ی��Җ���  DELIMITED BY SPACE
029240                         "���ϑg��"      DELIMITED BY SIZE
029250                         "  "            DELIMITED BY SIZE
029260                         �ہ|�x��������  DELIMITED BY SPACE
029270                         INTO �����於�̂v
029280                  END-STRING
                    END-IF
029290          WHEN OTHER
029300              MOVE �ہ|�ی��Җ���    TO �����於�̂v
029310          END-EVALUATE
006330     END-READ.
005040*================================================================*
005050 �s�������擾 SECTION.
005060*
005070****************************************************
005080* �A���f�[�^����ی��҃}�X�^��萿������擾����B *
005090* �� ������...... �����於�̂v�Ɋi�[               *
005100****************************************************
005110     MOVE ��P�|�ی����     TO �s�|������.
005120     MOVE ��P�|�ی��Ҕԍ�   TO �s�|�s�����ԍ�.
005130     READ �s�����}�X�^
005140     INVALID KEY
005150         MOVE SPACE          TO �����於�̂v
005160     NOT INVALID KEY
005170         MOVE �s�|�s�������� TO �����於�̂v
005180     END-READ.
013010*================================================================*
013020 �G���[�����o SECTION.
013030*
013040     IF �ʒm���o NOT = "00"
013050         DISPLAY NC"���[�G���["              UPON CONS
013060         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
013070         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
013080         DISPLAY NC"�g������o�F" �g������o UPON CONS
013090         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
013100                                             UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
013110         ACCEPT  �L�[���� FROM CONS
013120         PERFORM �t�@�C����
013130         EXIT PROGRAM
013140     END-IF.
013150*================================================================*
013160******************************************************************
013170 END PROGRAM YAZ582.
013180******************************************************************
