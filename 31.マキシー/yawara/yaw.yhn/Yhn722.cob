000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN722.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*    ���{�ق˂��E�I���E����܎t����@�{�p�̎����@���
000100*         MED = YHN720 YHN662P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2014-09-01
000130 DATE-COMPILED.          2014-09-01
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
000320     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  ���|����敪
000360                             FILE STATUS              IS  ��ԃL�[
000370                             LOCK        MODE         IS  AUTOMATIC.
000410     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000420                             ORGANIZATION             IS  INDEXED
000430                             ACCESS MODE              IS  DYNAMIC
000440                             RECORD KEY               IS  ���|�敪�R�[�h
000450                                                          ���|���̃R�[�h
000460                             FILE STATUS              IS  ��ԃL�[
000470                             LOCK        MODE         IS  AUTOMATIC.
000380     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS ��|�{�p�a��N��
000420                                                          ��|���҃R�[�h
000430                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000440                                                          ��|���҃J�i
000450                                                          ��|���҃R�[�h
000460                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000470                                                         ��|�{�p�a��N��
000480                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000490                                                          ��|�ی����
000500                                                          ��|�ی��Ҕԍ�
000510                                                          ��|���҃R�[�h
000520                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000530                                                          ��|������
000540                                                     ��|��p���S�Ҕԍ�
000550                                                          ��|���҃R�[�h
000560                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000570                                                          ��|�������
000580                                                  ��|��p���S�Ҕԍ�����
000590                                                          ��|���҃R�[�h
000600                             ALTERNATE RECORD KEY  IS ��|�����a��N��
000610                                                      ��|�{�p�a��N��
000620                                                      ��|���҃R�[�h
000630                             FILE STATUS              IS  ��ԃL�[
000640                             LOCK        MODE         IS  AUTOMATIC.
000680     SELECT  �{�p�L�^�e      ASSIGN      TO      SEKIROKL
000690                             ORGANIZATION        IS  INDEXED
000700                             ACCESS MODE         IS  DYNAMIC
000710                             RECORD KEY          IS  �{�L�|�{�p�a��N����
000720                                                     �{�L�|���҃R�[�h
000730                             ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
000740                                                     �{�L�|�{�p�a��N����
000750                             FILE STATUS              IS  ��ԃL�[
000760                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  ��ƃt�@�C���P  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7211L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  ��P�|�{�p�a��N����
000610                                                          ��P�|���҃R�[�h
000910                             ALTERNATE RECORD KEY     IS  ��P�|�{�p�a��N����
                                                                ��P�|���҃J�i
                                                                ��P�|���҃R�[�h
000910                             ALTERNATE RECORD KEY     IS  ��P�|���҃R�[�h
                                                                ��P�|�{�p�a��N����
000910                             ALTERNATE RECORD KEY     IS  ��P�|���҃J�i
                                                                ��P�|���҃R�[�h
                                                                ��P�|�{�p�a��N����
000620                             FILE        STATUS       IS  ��ԃL�[
000630                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  ��ƃt�@�C���Q  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7212L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  ��Q�|�{�p�a��N��
000610                                                          ��Q�|���҃R�[�h
000620                             FILE        STATUS       IS  ��ԃL�[
000630                             LOCK        MODE         IS  AUTOMATIC.
000720     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
000730                             SYMBOLIC    DESTINATION  IS "PRT"
000740                             FORMAT                   IS  ��`�̖��o
000750                             GROUP                    IS  ���ڌQ���o
000760                             PROCESSING  MODE         IS  ������ʂo
000770                             UNIT        CONTROL      IS  �g������o
000780                             FILE        STATUS       IS  �ʒm���o.
000790******************************************************************
000800*                      DATA DIVISION                             *
000810******************************************************************
000820 DATA                    DIVISION.
000830 FILE                    SECTION.
000840*                           �m�q�k��  �P�Q�W�n
000850 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000860     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000870*                           �m�q�k��  �Q�T�U�n
000880 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
000890     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000840*                           �m�q�k��  �P�Q�W�n
000850 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
000860     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000900*                           �m�q�k��  �R�Q�O�n
000910 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
000920     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001160*                           �m�q�k��  �Q�T�U�n
001170 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
001180    COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
001310*****************
001320* ��ƃt�@�C���P *
001330*****************
001340*                         �m�q�k��  �P�U�O�n
001350 FD  ��ƃt�@�C���P RECORD  CONTAINS 160 CHARACTERS.
001360 01 ��P�|���R�[�h.
001370    03 ��P�|���R�[�h�L�[.
001535       05 ��P�|�{�p�a��N����.
001536          07 ��P�|�{�p�a��               PIC 9.
001537          07 ��P�|�{�p�N��.
001538             09 ��P�|�{�p�N              PIC 9(2).
001539             09 ��P�|�{�p��              PIC 9(2).
001540          07 ��P�|�{�p��                 PIC 9(2).
001460       05 ��P�|���҃R�[�h.
001470          07 ��P�|���Ҕԍ�                PIC 9(6).
001480          07 ��P�|�}��                    PIC X(1).
001490    03 ��P�|���R�[�h�f�[�^.
             05 ��P�|���҃J�i                   PIC X(50).
             05 ��P�|���Ҏ���                   PIC X(50).
             05 ��P�|����.
001550          07 ��P�|�������z                PIC 9(5).
001550          07 ��P�|�������z                PIC 9(5).
001550          07 ��P�|��Ë��z                PIC 9(5).
001550          07 ��P�|㪖@���z                PIC 9(5).
001550          07 ��P�|�d�Ë��z                PIC 9(5).
001551          07 ��P�|��p�z                  PIC 9(5).
001551          07 ��P�|�ꕔ���S��              PIC 9(5).
001500       05 FILLER                           PIC X(11).
001340*                         �m�q�k��  �P�Q�W�n
001350 FD  ��ƃt�@�C���Q RECORD  CONTAINS 128 CHARACTERS.
001360 01 ��Q�|���R�[�h.
001370    03 ��Q�|���R�[�h�L�[.
001535       05 ��Q�|�{�p�a��N��.
001536          07 ��Q�|�{�p�a��                PIC 9.
001537          07 ��Q�|�{�p�N��.
001538             09 ��Q�|�{�p�N               PIC 9(2).
001539             09 ��Q�|�{�p��               PIC 9(2).
001460       05 ��Q�|���҃R�[�h.
001470          07 ��Q�|���Ҕԍ�                PIC 9(6).
001480          07 ��Q�|�}��                    PIC X(1).
001490    03 ��Q�|���R�[�h�f�[�^.
001535       05 ��Q�|�ŏI�ʉ@��.
001539          07 ��Q�|�ʉ@��                  PIC 9(2).
001539          07 ��Q�|�ʉ@��                  PIC 9(2).
001420       05 ��Q�|�{�p��                   PIC 9(2).
001420       05 ��Q�|��p�z                     PIC 9(6).
001420       05 ��Q�|�����z                     PIC 9(6).
001420       05 ��Q�|���S��                     PIC 9(5).
             05 ��Q�|�]�A                       OCCURS 5.
                07 ��Q�|�]�A�敪                PIC N(1).
             05 ��Q�|�����v                     PIC 9(5).
001550       05 ��Q�|�����v                     PIC 9(5).
001550       05 ��Q�|��Ìv                     PIC 9(5).
001550       05 ��Q�|㪖@�v                     PIC 9(5).
001550       05 ��Q�|�d�Ìv                     PIC 9(5).
001551       05 ��Q�|��p�v                     PIC 9(5).
001551       05 ��Q�|���S�v                     PIC 9(5).
001500       05 FILLER                           PIC X(48).
001150*
001160 FD  ����t�@�C��.
001170     COPY YHN662P        OF  XMDLIB.
001180*----------------------------------------------------------------*
001190******************************************************************
001200*                WORKING-STORAGE SECTION                         *
001210******************************************************************
001220 WORKING-STORAGE         SECTION.
001230 01 �L�[����                           PIC X     VALUE SPACE.
001240 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
001250 01 �I���t���O                         PIC X(3)  VALUE SPACE.
001260 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
001270 01 �����t���O                         PIC X(3)  VALUE SPACE.
001280 01 �t�@�C����                         PIC N(6)  VALUE SPACE.
001290 01 ���Z�v�g�o�f�v                     PIC X(8)  VALUE SPACE.
001300 01 �O�a��v                           PIC 9     VALUE ZERO.
001310 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
001210 01 �I�[�v���t���O                     PIC X(3)   VALUE SPACE.
      */�V�p���Ή�/0408
001840 01 �J���e�p����ʂv                   PIC 9(1) VALUE ZERO.
001330 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
001330 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
001330 01 �J�E���^�P                         PIC 9(2)  VALUE ZERO.
001340 01 ���Ҕԍ��v                         PIC 9(6)  VALUE ZERO.
001720 01 �m�F�v                             PIC X(4) VALUE SPACE.
001240 01 �s�J�E���^                         PIC 9(2) VALUE 0.
001260 01 �ő�s��                           PIC 9(2) VALUE 0.
001270 01 �w�b�_�s��                         PIC 9(2) VALUE 0.
001280 01 �ړ��s���v                         PIC 9(2) VALUE 0.
001350**
001360 01 �x���t���O                         PIC X(3) VALUE SPACE.
001370 01 �x���񐔂v                         PIC 9(4) VALUE ZERO.
001380 01 �x���b�m�s                         PIC 9(5) VALUE ZERO.
001390**
001360 01 ���v�v.
001380    03 �������v�v                      PIC 9(5) VALUE ZERO.
001390    03 �������v�v                      PIC 9(5) VALUE ZERO.
001400    03 ��×��v�v                      PIC 9(5) VALUE ZERO.
001400    03 㪖@���v�v                      PIC 9(5) VALUE ZERO.
001400    03 �d�×��v�v                      PIC 9(5) VALUE ZERO.
001400    03 ���S���v�v                      PIC 9(5) VALUE ZERO.
001400*
001360 01 �����v.
001380    03 �������v                        PIC 9(5) VALUE ZERO.
001390    03 �������v                        PIC 9(5) VALUE ZERO.
001400    03 ��×��v                        PIC 9(5) VALUE ZERO.
001400    03 㪖@���v                        PIC 9(5) VALUE ZERO.
001400    03 �d�×��v                        PIC 9(5) VALUE ZERO.
001400    03 ���S���v                        PIC 9(5) VALUE ZERO.
001410****************
001420* �A�����ڑҔ� *
001430****************
001440*    ************
001450*    * ����L�[ *
001460*    ************
001470 01 �Ώۃf�[�^�v�q.
001480    03 �{�p�a��N���v�q.
001490       05 �{�p�a��v�q                  PIC 9(1)  VALUE ZERO.
001500       05 �{�p�N�v�q                    PIC 9(2)  VALUE ZERO.
001510       05 �{�p���v�q                    PIC 9(2)  VALUE ZERO.
001520    03 �J�n���v�q                       PIC 9(2)  VALUE ZERO.
001530    03 �I�����v�q                       PIC 9(2)  VALUE ZERO.
001540    03 ���҃J�i�v�q                     PIC X(50) VALUE SPACE.
001550    03 ���҃R�[�h�v.
001560       05 ���Ҕԍ��v                    PIC 9(6)  VALUE ZERO.
001570       05 �}�Ԃv                        PIC X(1)  VALUE SPACE.
001580    03 ������[�h�e�v�q                 PIC 9(1)  VALUE ZERO.
001630    03 �󎚈ʒu�b�m�s                   PIC 9(2)  VALUE ZERO.
001640**************
001650* ��f�ҏ�� *
001660**************
001670 01 ��f�ҏ��v.
001680    03 �{�p�N���v.
001690       05 �{�p�a��v                   PIC 9(1)   VALUE ZERO.
001690       05 �{�p�N�v                     PIC 9(2)   VALUE ZERO.
001700       05 �{�p���v                     PIC 9(2)   VALUE ZERO.
001710    03 �ی��Ҕԍ��v.
001720       05 ����ی��Ҕԍ��v             PIC X(6)   VALUE SPACE.
001730       05 FILLER                       PIC X(4)   VALUE SPACE.
001740    03 �ސE�ی��Ҕԍ��v.
001750       05 FILLER                       PIC X(2)   VALUE SPACE.
001760       05 �ސE����ی��Ҕԍ��v         PIC X(6)   VALUE SPACE.
001770       05 FILLER                       PIC X(2)   VALUE SPACE.
001780    03 �s�����ԍ��v.
001790       05 ����s�����ԍ��v             PIC X(8)   VALUE SPACE.
001800       05 FILLER                       PIC X(2)   VALUE SPACE.
001810    03 �ی���ʂv                      PIC 9(2)   VALUE ZERO.
001810    03 �����ʂv                      PIC 9(2)   VALUE ZERO.
001810    03 ������ʂv                      PIC 9(2)   VALUE ZERO.
001820    03 ���ҏ��v.
001830       05 ���҃J�i�v                   PIC X(50)  VALUE SPACE.
001840       05 ���Ҏ����v                   PIC X(50)  VALUE SPACE.
       01 ���Ҏ����v�o.
          03 ���Җ��v�o                      OCCURS 10. 
             05 ���Җ��v                        PIC N(1)   VALUE SPACE.
       01 �R�����g�v.
          03 �R�����g�P�v                       PIC X(50) VALUE SPACE.
          03 �R�����g�Q�v                       PIC X(50) VALUE SPACE.
001831* POWER COBOL�p
001832 01 dll-name  PIC X(260)  VALUE SPACE.
001833 01 form-name PIC X(14)   VALUE SPACE.
002210*******************************************************************
002220 01 �������.
002230     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
002240     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
002250     03 ������ʂo                     PIC X(2) VALUE SPACE.
002260     03 �g������o.
002270         05 �[������o.
002280             07 �ړ������o             PIC X(1) VALUE SPACE.
002290             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
002300         05 �ڍא���o                 PIC X(2) VALUE SPACE.
002310     03 �ʒm���o                     PIC X(2) VALUE SPACE.
002320     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
002330*
002340 01 �v�Z�@����N�v                     PIC 9(2) VALUE ZERO.
002350* ���t�v�n�q�j
002360 01 �a��I���N�v                       PIC 9(4) VALUE ZERO.
002370 01 �v�Z�@����.
002380    03 �v�Z�@����N                    PIC 9(4) VALUE ZERO.
002390    03 �v�Z�@�����                  PIC 9(4) VALUE ZERO.
002400 01 �v�Z�@����q REDEFINES �v�Z�@����.
002410    03 �v�Z�@���I                      PIC 9(2).
002420    03 �v�Z�@���t                      PIC 9(6).
002430    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
002440       05 �v�Z�@�N��                   PIC 9(4).
002450       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
002460         07 �v�Z�@�N                   PIC 9(2).
002470         07 �v�Z�@��                   PIC 9(2).
002480       05 �v�Z�@��                     PIC 9(2).
002490*
002500******************************************************************
002510*                          �A������                              *
002520******************************************************************
002510**********************
002520* ���b�Z�[�W�\���L�[ *
002530**********************
002540 01 �A���|�L�[ IS EXTERNAL.
002550    03  �A���|���b�Z�[�W                 PIC N(20).
003540*
       01 �A���b�Z�[�W�o���O�O�T�P IS EXTERNAL.
          03 �A���o�|���b�Z�[�W�ԍ�                PIC 9(2).
          03 �A���o�|���b�Z�[�W.
             05 �A���o�|���b�Z�[�W���e             PIC X(40) OCCURS 6.
          03 �A���o�|���b�Z�[�W�P                  PIC X(20).
          03 �A���o�|���b�Z�[�W�Q                  PIC X(12).
          03 �A���o�|�Ԃ�l                        PIC X.
002560*
002870************
002880* ����L�[ *
002890************
       01 �A��|�Ώۃf�[�^�x�g�m�V�Q�O IS EXTERNAL.
          03 �A��|���v���[�h                   PIC 9(1).
          03 �A��|���׃��[�h                   PIC 9(1).
          03 �A��|���i��                       PIC 9(2).
      */�ڍ�
          03 �A��|��������敪                 PIC 9(1).
          03 �A��|�{�p����敪                 PIC 9(1).
      *
006560 01 �A��|�Ώۃf�[�^ IS EXTERNAL.
006570    03 �A��|�{�p�a��N��.
006580       05 �A��|�{�p�a��                  PIC 9(1).
006590       05 �A��|�{�p�N                    PIC 9(2).
006600       05 �A��|�{�p��                    PIC 9(2).
006610    03 �A��|�ی����                     PIC 9(2).
006620    03 �A��|�ی��Ҕԍ�                   PIC X(10).
006630    03 �A��|�{�l�Ƒ��敪                 PIC 9(1).
006640    03 �A��|��ی��҃J�i                 PIC X(20).
006650    03 �A��|���҃R�[�h.
006660       05 �A��|���Ҕԍ�                  PIC 9(6).
006670       05 �A��|�}��                      PIC X(1).
006680    03 �A��|������[�h�e                 PIC 9(1).
006690*/�ڍ�
006700    03 �A��|�ی��؈���敪               PIC 9(1).
006710    03 �A��|�����ڍ� OCCURS 7.
006720       05 �A��|��������s                PIC 9(1).
006730       05 �A��|���ʈ���敪              PIC 9(1).
006740       05 �A��|�]�A����敪              PIC 9(1).
006750       05 �A��|��������敪              PIC 9(1).
       01 �A���|�\���t���O�U�U�O IS EXTERNAL.
          03 �A���|�v���r���[�敪               PIC 9(1).
003020*
000540************************************
000550* �v�����^�t�@�C���쐬�p           *
000560************************************
000570 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000580     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
000590     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
000600     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
000610     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
000993************************************
000994* �v�����^�t�@�C���쐬����p       *
000995************************************
000996 01 �g�A����o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000997     03 �g�A����o�q�s�e�|�p�����         PIC X(8).
003080*
003090******************************************************************
003100*                      PROCEDURE  DIVISION                       *
003110******************************************************************
003120 PROCEDURE               DIVISION.
003130************
003140*           *
003150* ��������   *
003160*           *
003170************
002570     PERFORM �v�����^�t�@�C���쐬.
003180     PERFORM ������.
003190     PERFORM ������擾.
003200************
003210*           *
003220* �又��     *
003230*           *
003240************
003250* ���
           INITIALIZE    YHN662P.
           MOVE SPACE TO YHN662P.
003290     PERFORM ����Z�b�g�P.
003350************
003360*           *
003370* �I������   *
003380*           *
003390************
003400     PERFORM �I������.
003410     PERFORM �x������.
003420     EXIT PROGRAM.
003430*
003440*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002860*================================================================*
002870 �v�����^�t�@�C���쐬 SECTION.
002880*================================================================*
002890*   / ������ /
002900     MOVE SPACE TO �g�A�o�q�s�e�|�쐬�f�[�^.
002910     INITIALIZE �g�A�o�q�s�e�|�쐬�f�[�^.
002225     MOVE SPACE TO �g�A����o�q�s�e�|�쐬�f�[�^.
002226     INITIALIZE �g�A����o�q�s�e�|�쐬�f�[�^.
002920*
002930*
002940*--���� �ύX�ӏ� ����--------------------------------------*
002230*   �g�p����p����ʃZ�b�g
           MOVE "KARUTE"              TO �g�A����o�q�s�e�|�p�����.
002970*   �g�p����v�����^�t�@�C�����Z�b�g
002231     MOVE "PRTF002"             TO �g�A�o�q�s�e�|�t�@�C����.
002972*
002973*   �g�p���钠�[�v���O�������Z�b�g
002974     MOVE "YHN662"              TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
003450*================================================================*
003460 ������ SECTION.
003470*
004010     OPEN INPUT   �����}�X�^
004020         MOVE NC"����" TO �t�@�C����.
004030         PERFORM �I�[�v���`�F�b�N.
004040     OPEN INPUT   ������}�X�^
004050         MOVE NC"������" TO �t�@�C����.
004060         PERFORM �I�[�v���`�F�b�N.
004800     OPEN INPUT ���̃}�X�^.
004810         MOVE NC"����" TO �t�@�C����.
004820         PERFORM �I�[�v���`�F�b�N.
004070     OPEN INPUT   ��f�ҏ��e.
004080         MOVE NC"���" TO �t�@�C����.
004090         PERFORM �I�[�v���`�F�b�N.
007480     OPEN INPUT �{�p�L�^�e.
007490         MOVE NC"�{�p�L�^�e"   TO �t�@�C����.
007500         PERFORM �I�[�v���`�F�b�N.
004100     OPEN INPUT ��ƃt�@�C���P
004110         MOVE NC"��ƂP" TO �t�@�C����.
004120         PERFORM �I�[�v���`�F�b�N.
004100     OPEN INPUT ��ƃt�@�C���Q
004110         MOVE NC"��ƂQ" TO �t�@�C����.
004120         PERFORM �I�[�v���`�F�b�N.
004150*================================================================*
004160 �I�[�v���`�F�b�N SECTION.
004170*
004180     IF ��ԃL�[  NOT =  "00"
004190         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
004200         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
004210         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004220                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
004230         ACCEPT  �L�[���� FROM CONS
004240         PERFORM �t�@�C����
004250         EXIT PROGRAM.
004260*================================================================*
004270 ������擾 SECTION.
004280*
004290     MOVE ZERO TO ���|����敪
004300     READ ������}�X�^
004310     NOT INVALID KEY
004320         MOVE ���|�x����       TO �x���񐔂v
      */�V�p���Ή�/0408
               MOVE ���|�J���e�p����� TO �J���e�p����ʂv
004330     END-READ.
004340*
004350*================================================================*
004360 �x������ SECTION.
004370*
004380     PERFORM VARYING �x���b�m�s FROM 1 BY 1
004390                                UNTIL �x���b�m�s > �x���񐔂v
004400         MOVE SPACE TO �x���t���O
004410     END-PERFORM.
004420*
004590*================================================================*
004600 ����Z�b�g�P SECTION.
004610*
002940     MOVE SPACE TO �I���t���O.
002950     MOVE ZERO  TO �s�J�E���^.
002980     PERFORM ���v�l������.
           MOVE 1                 TO �󎚈ʒu�b�m�s.
           MOVE �A��|���҃R�[�h  TO ��P�|���҃R�[�h.
           MOVE ZERO              TO ��P�|�{�p�a��.
           MOVE ZERO              TO ��P�|�{�p�N.
           MOVE ZERO              TO ��P�|�{�p��.
           START ��ƃt�@�C���P KEY IS >= ��P�|���҃R�[�h
                                          ��P�|�{�p�a��N����
           END-START.
013500     IF ��ԃL�[  =  "00"
013510         MOVE SPACE  TO �I���t���O
013520         PERFORM ��ƃt�@�C���P�Ǎ�
003000         MOVE ��P�|���҃R�[�h   TO ���҃R�[�h�v
               MOVE ��P�|�{�p�a��     TO �{�p�a��v
               MOVE ��P�|�{�p�N       TO �{�p�N�v
               MOVE ��P�|�{�p��       TO �{�p���v
               IF �A��|���i�� = ZERO
                   MOVE 1              TO �J�E���^
               ELSE
                   MOVE �A��|���i��   TO �J�E���^
               END-IF
      */      */���҃R�[�h���ς��܂ō�ƃt�@�C����ǂ�
003070         PERFORM UNTIL ( �I���t���O = "YES" ) OR
003090                       ( ��P�|���҃R�[�h NOT = ���҃R�[�h�v )
      *           */�Q�X�s�𒴂�������y�[�W
004380             PERFORM UNTIL ( �J�E���^ > 29 ) OR
003090                           ( ��P�|���҃R�[�h NOT = ���҃R�[�h�v ) OR
                                 ( �I���t���O = "YES" )
      */              */�����܂����ꍇ�̏���������
004380                 IF ( ��P�|�{�p�N NOT = �{�p�N�v ) OR
                          ( ��P�|�{�p�� NOT = �{�p���v )
      */                  */���v�s�̈������
                           IF �A��|���v���[�h = 1
      */                      */���v�s�͂Q�s�g���ׂQ�W�ŉ���
                               IF �J�E���^ > 28
      */                          */���������K�v20212
                                    MOVE 1 TO �J�E���^
      */                          */���ׂ̈������
                                   PERFORM ���׈��
      */                          */���ŏ���
003650                             PERFORM ���ŏ���
      *
                                   MOVE 1                    TO �A���o�|���b�Z�[�W�ԍ�
                                   MOVE "���̗p�����Z�b�g���Ă�������" TO �A���o�|���b�Z�[�W
                                   MOVE "PMSG0051.DLL" TO dll-name
                                   MOVE "PMSG0051"     TO form-name
                                   CALL "POWEROPENSHEET" USING dll-name form-name
                                   EVALUATE  �A���o�|�Ԃ�l
005871                             WHEN "Y"
                                       CONTINUE
                                   WHEN OTHER
      */                              */�m�@���́@�L�����Z��
                                       PERFORM ����s���ޔ�
                                       PERFORM �I������
                                       EXIT PROGRAM
                                   END-EVALUATE
                               END-IF
      */                      */���v�s�̓]�L����
003130                         PERFORM ���v�������
      */                      */���v�s��]�L���ĉ��ł���ꍇ�̏���
                               IF �J�E���^ > 28
      */                          */���������K�v20215
                                   MOVE 1 TO �J�E���^
                                   PERFORM ���׈��
003650                             PERFORM ���ŏ���
      *
                                   MOVE 1                    TO �A���o�|���b�Z�[�W�ԍ�
                                   MOVE "���̗p�����Z�b�g���Ă�������" TO �A���o�|���b�Z�[�W
                                   MOVE "PMSG0051.DLL" TO dll-name
                                   MOVE "PMSG0051"     TO form-name
                                   CALL "POWEROPENSHEET" USING dll-name form-name
                                   EVALUATE  �A���o�|�Ԃ�l
005871                             WHEN "Y"
                                       CONTINUE
                                   WHEN OTHER
      */                              */�m�@���́@�L�����Z��
                                       PERFORM ����s���ޔ�
                                       PERFORM �I������
                                       EXIT PROGRAM
                                   END-EVALUATE
                               END-IF
      */                  */���v�s�̏�����
                               PERFORM ���v�l������
                           END-IF
                       END-IF
      */              */�����܂����ꍇ�̏���������
004640                 IF �A��|���׃��[�h = 1
      */                  */���׍s�̓]�L
                           PERFORM ���׈������
                       END-IF
003590                 MOVE ��P�|���҃R�[�h   TO ���҃R�[�h�v
003590                 MOVE ��P�|�{�p�a��     TO �{�p�a��v
003590                 MOVE ��P�|�{�p�N       TO �{�p�N�v
003590                 MOVE ��P�|�{�p��       TO �{�p���v
003600                 PERFORM ��ƃt�@�C���P�Ǎ�
003610             END-PERFORM
      */          */�P�ŕ��]�L�܂��͂P�l���]�L�����甲���遪����
      */          */�P�ŕ��]�L���Ă����ꍇ��
                   IF �J�E���^ > 28
      */              */���������K�v
                       MOVE 1 TO �J�E���^
      */              */���׍s��������ĉ��ł���
                       PERFORM ���׈��
003650                 PERFORM ���ŏ���
      */�P�l���̈�����I����Ă��Ȃ��A�܂��́A
      */�P�l��������I����č��v�s�̈���w��̎��Ɏ��ł̎w��20130
                       IF ((�I���t���O           = "YES") OR 
                           (��P�|���҃R�[�h NOT = ���҃R�[�h�v)) AND 
                           (�A��|���v���[�h     = ZERO)
      */                  */���łȂ�
                           CONTINUE
                       ELSE
                           MOVE 1                    TO �A���o�|���b�Z�[�W�ԍ�
                           MOVE "���̗p�����Z�b�g���Ă�������" TO �A���o�|���b�Z�[�W
                           MOVE "PMSG0051.DLL" TO dll-name
                           MOVE "PMSG0051"     TO form-name
                           CALL "POWEROPENSHEET" USING dll-name form-name
                           EVALUATE  �A���o�|�Ԃ�l
005871                     WHEN "Y"
                               CONTINUE
                           WHEN OTHER
      */                      */�m�@���́@�L�����Z��
                               PERFORM ����s���ޔ�
                               PERFORM �I������
                               EXIT PROGRAM
                           END-EVALUATE
                           MOVE 1  TO �󎚈ʒu�b�m�s
      */���v�s�̈�����K�v�ȏꍇ������20130
                           IF �A��|���v���[�h = 1
003090                         IF ( ��P�|���҃R�[�h NOT = ���҃R�[�h�v ) OR
                                  ( �I���t���O = "YES" )
                                   MOVE SPACE TO YHN662P
                                   PERFORM ���v�������
                                   PERFORM ���׈��
                                   MOVE �J�E���^ TO �󎚈ʒu�b�m�s
                               END-IF
                           END-IF
                       END-IF
      */���v�s�̈�����K�v�ȏꍇ������20130
                   ELSE
      */              */���v�s�̈��
                       IF �A��|���v���[�h = 1
                           IF �J�E���^ > 28
      */                      */����������20130
                               MOVE 1    TO �J�E���^
                               PERFORM ���׈��
003650                         PERFORM ���ŏ���
      *
      */���b�Z�[�W�{�b�N�X�̕ύX/
                               MOVE 1                    TO �A���o�|���b�Z�[�W�ԍ�
                               MOVE "���̗p�����Z�b�g���Ă�������" TO �A���o�|���b�Z�[�W
                               MOVE "PMSG0051.DLL" TO dll-name
                               MOVE "PMSG0051"     TO form-name
                               CALL "POWEROPENSHEET" USING dll-name form-name
                               EVALUATE  �A���o�|�Ԃ�l
005871                         WHEN "Y"
                                   CONTINUE
                               WHEN OTHER
      */                          */�m�@���́@�L�����Z��
                                   PERFORM ����s���ޔ�
                                   PERFORM �I������
                                   EXIT PROGRAM
                               END-EVALUATE
                           END-IF
      */                  */���v�s�̓]�L����
                           PERFORM ���v�������
      */                  */���v��������čŏI�s�ɒB�����ꍇ20208
                           IF �J�E���^ > 29
                               MOVE 1 TO �J�E���^
                           END-IF
                       END-IF
                       PERFORM ���v�l������
                       PERFORM ���׈��
                       PERFORM ���ŏ���
                       MOVE �J�E���^ TO �󎚈ʒu�b�m�s
                   END-IF
003620         END-PERFORM
               MOVE �J�E���^ TO �󎚈ʒu�b�m�s
               PERFORM ����s���ޔ�
           END-IF.
004900*================================================================*
004910 ���׈������ SECTION.
004920*
      */���͂P�s�ڂ܂��͓����̏���݈̂������B
           PERFORM �{�p�L�^�e�Ǎ�
      *     IF (�J�E���^ = 1) OR (��P�|�{�p�� = �{�L�|�{�p��)
      *         MOVE ��P�|�{�p��   TO ��  (�J�E���^)
      *         MOVE "/"            TO ���(�J�E���^)
      *     END-IF
005510     MOVE ��P�|�{�p��       TO ��(�J�E���^).
           MOVE "/"                TO ���(�J�E���^).
005520     MOVE ��P�|�{�p��       TO ��(�J�E���^).
004940********************
004950* �����f�[�^�Z�b�g *
004960********************
005010     MOVE ��P�|�������z     TO  ��������(�J�E���^).
005010     MOVE ��P�|�������z     TO  ������(�J�E���^).
005010     MOVE ��P�|��Ë��z     TO  ��×�(�J�E���^).
005010     MOVE ��P�|㪖@���z     TO  㪖@��(�J�E���^).
005010     MOVE ��P�|�d�Ë��z     TO  �d�×�(�J�E���^).
           MOVE ��P�|��p�z       TO  ��p�z(�J�E���^).
           MOVE ��P�|�ꕔ���S��   TO  ���S��(�J�E���^).
           COMPUTE �J�E���^ = �J�E���^ + 1.
005730*================================================================*
005740 ���v�l������ SECTION.
005750*
005760     MOVE ZERO  TO ���v�v.
004900*================================================================*
004910 ���v������� SECTION.
004920*
           MOVE �{�p�a��v   TO ��Q�|�{�p�a��.
           MOVE �{�p�N�v     TO ��Q�|�{�p�N.
           MOVE �{�p���v     TO ��Q�|�{�p��.
           MOVE ���҃R�[�h�v TO ��Q�|���҃R�[�h.
           READ ��ƃt�@�C���Q
           NOT INVALID KEY
               MOVE NC"���v"     TO ���v(�J�E���^)
               MOVE ��Q�|�����v TO ��������(�J�E���^)
               MOVE ��Q�|�����v TO ������(�J�E���^)
               MOVE ��Q�|��Ìv TO ��×�(�J�E���^)
               MOVE ��Q�|㪖@�v TO 㪖@��(�J�E���^)
               MOVE ��Q�|�d�Ìv TO �d�×�(�J�E���^)
               MOVE ��Q�|��p�v TO ��p�z(�J�E���^)
               MOVE ��Q�|���S�v TO ���S��        (�J�E���^)
      *
               COMPUTE �J�E���^ = �J�E���^ + 1
               MOVE NC"��p�z"            TO ��p(�J�E���^)
               MOVE ��Q�|��p�z          TO ���v��p�z(�J�E���^)
               MOVE NC"�~"                TO �~�P(�J�E���^)
               MOVE NC"�����z"            TO ����(�J�E���^)
               MOVE ��Q�|�����z          TO ���v�����z(�J�E���^)
               MOVE NC"�~"                TO �~�Q(�J�E���^)
               MOVE ��Q�|�ʉ@��          TO �ʉ@��(�J�E���^)
               MOVE NC"��"                TO ���P(�J�E���^)
               MOVE ��Q�|�ʉ@��          TO �ʉ@��(�J�E���^)
               MOVE NC"��"                TO ���P(�J�E���^)
               MOVE NC"��"              TO �{�p��(�J�E���^)
               MOVE ��Q�|�{�p��        TO ��(�J�E���^)
               MOVE NC"��"                TO ��(�J�E���^)
               MOVE ��Q�|�]�A�敪(1)     TO �]�A�P(�J�E���^)
               MOVE ��Q�|�]�A�敪(2)     TO �]�A�Q(�J�E���^)
               MOVE ��Q�|�]�A�敪(3)     TO �]�A�R(�J�E���^)
               MOVE ��Q�|�]�A�敪(4)     TO �]�A�S(�J�E���^)
               MOVE ��Q�|�]�A�敪(5)     TO �]�A�T(�J�E���^)
               COMPUTE �J�E���^ = �J�E���^ + 1
           END-READ.
007070*================================================================*
007080 ��ƃt�@�C���P�Ǎ� SECTION.
007090*
007100     READ ��ƃt�@�C���P NEXT
007110     AT END
007120         MOVE "YES" TO �I���t���O
007130     END-READ.
007140*
007150*================================================================*
007160 ���׈�� SECTION.
007170*
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
013440*
007180     MOVE "YHN662P"  TO  ��`�̖��o.
007190     MOVE "GRP002"   TO  ���ڌQ���o.
003770     MOVE SPACE      TO  ������ʂo.
007200     WRITE YHN662P.
007210     PERFORM �G���[�����o.
003730*================================================================*
003740 ���ŏ���  SECTION.
003750*
003760     MOVE "YHN662P" TO  ��`�̖��o.
003770     MOVE "CT"      TO  ������ʂo.
003780     MOVE "PAGE"    TO  �g������o.
003790     MOVE SPACE     TO  ���ڌQ���o.
003800     WRITE YHN662P.
003810     PERFORM �G���[�����o.
003820     MOVE SPACE     TO  �g������o.
003821*
003822     CLOSE  ����t�@�C��.
004320     MOVE SPACE TO �I�[�v���t���O.
           MOVE SPACE TO YHN662P.
003825*
007610*================================================================*
007620 �G���[�\�� SECTION.
007630*
007640     DISPLAY �t�@�C����   NC"�t�@�C�������G���["   UPON CONS.
007650     DISPLAY NC"��ԃL�[�F" ��ԃL�[               UPON CONS.
007660     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
007670     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"  UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
007680     ACCEPT  �L�[���� FROM CONS.
007690     PERFORM �t�@�C����.
007700     EXIT PROGRAM.
007290*================================================================*
007300 �G���[�����o SECTION.
007310*
007320     IF �ʒm���o NOT = "00"
007330         DISPLAY NC"���[�G���["              UPON CONS
007340         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
007350         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
007360         DISPLAY NC"�g������o�F" �g������o UPON CONS
007370         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
007380                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007390         ACCEPT  �L�[���� FROM CONS
007400         PERFORM �t�@�C����
007410         MOVE 99 TO PROGRAM-STATUS
007420         EXIT PROGRAM
007430     END-IF.
007440*================================================================*
007450 �t�@�C���� SECTION.
007460*
002990     IF ( �I�[�v���t���O = "YES" )
007470        CLOSE ����t�@�C��
           END-IF.
007480     CLOSE �����}�X�^   ������}�X�^ ���̃}�X�^
                 ��f�ҏ��e   ��ƃt�@�C���P ��ƃt�@�C���Q �{�p�L�^�e.
007490*================================================================*
007500 �I������ SECTION.
007510*
007520     PERFORM �t�@�C����.
006850*================================================================*
006860 ����s���ޔ� SECTION.
006870     CLOSE ��f�ҏ��e.
006880     PERFORM �x������.
006890     OPEN I-O ��f�ҏ��e.
006900         MOVE NC"��f" TO �t�@�C����.
006910         PERFORM �I�[�v���`�F�b�N.
006920     PERFORM �x������.
006930*
006940     MOVE ZERO            TO ��|�{�p�a��.
006950     MOVE ZERO            TO ��|�{�p�N.
006960     MOVE ZERO            TO ��|�{�p��.
006970     MOVE ���҃R�[�h�v    TO ��|���҃R�[�h.
006990*
007000     READ ��f�ҏ��e
007010     NOT INVALID KEY
007020         MOVE �󎚈ʒu�b�m�s TO ��|�J���e������s��
007030         REWRITE ��|���R�[�h
007040         INVALID KEY
007050             MOVE NC"��e" TO �t�@�C����
007060             PERFORM �G���[�\��
007070         END-REWRITE
007080     END-READ.
007090     PERFORM �x������.
007100     CLOSE ��f�ҏ��e.
007110     PERFORM �x������.
007120     OPEN INPUT ��f�ҏ��e.
007130         MOVE NC"��f" TO �t�@�C����.
007140         PERFORM �I�[�v���`�F�b�N.
007070*================================================================*
       �{�p�L�^�e�Ǎ� SECTION.
      *
013530     MOVE ��P�|���Ҕԍ�  TO �{�L�|���Ҕԍ�
013540     MOVE ��P�|�}��      TO �{�L�|�}��
013550     MOVE ��P�|�{�p�a��  TO �{�L�|�{�p�a��
013560     MOVE ��P�|�{�p�N    TO �{�L�|�{�p�N
013570     MOVE ��P�|�{�p��    TO �{�L�|�{�p��
013580     MOVE ZERO            TO �{�L�|�{�p��
013590     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
013600                                  �{�L�|�{�p�a��N����
013610     END-START
013620     IF ��ԃL�[ = "00"
               READ �{�p�L�^�e NEXT
               AT END
                   MOVE SPACE TO �{�L�|���R�[�h
               END-READ
               IF (��P�|���҃R�[�h = �{�L�|���҃R�[�h) AND
                  (��P�|�{�p�a��   = �{�L�|�{�p�a��  ) AND
                  (��P�|�{�p�N     = �{�L�|�{�p�N    ) AND
                  (��P�|�{�p��     = �{�L�|�{�p��    )
                   CONTINUE
               ELSE
                   MOVE SPACE TO �{�L�|���R�[�h
               END-IF
           END-IF
007070*================================================================*
007540******************************************************************
007550 END PROGRAM YHN722.
007560******************************************************************
