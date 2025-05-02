000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCB7221.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*         �����_���t���� �J���e���y����z
000100*         MED = YCB720 YCB7221P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2012-11-13
000130 DATE-COMPILED.          2012-11-13
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
000320     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  ���|����敪
000360                             FILE STATUS              IS  ��ԃL�[
000370                             LOCK        MODE         IS  AUTOMATIC.
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
000620     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000630                             ORGANIZATION             IS  INDEXED
000640                             ACCESS MODE              IS  DYNAMIC
000650                             RECORD KEY               IS ���|�{�p�a��N��
000660                                                         ���|���҃R�[�h
000670                             ALTERNATE RECORD KEY     IS ���|���҃R�[�h
000680                                                         ���|�{�p�a��N��
000690                             FILE STATUS              IS  ��ԃL�[
000700                             LOCK        MODE         IS  AUTOMATIC.
000260     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY           IS �{�L�|�{�p�a��N����
000300                                                     �{�L�|���҃R�[�h
000310                             ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
000320                                                     �{�L�|�{�p�a��N����
000330                             FILE STATUS              IS  ��ԃL�[
000340                             LOCK        MODE         IS  AUTOMATIC.
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
000650     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W72111L.DAT"
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS                   IS  DYNAMIC
000680                             RECORD      KEY    IS  ��P�|�{�p�a��N����
000690                             FILE        STATUS       IS  ��ԃL�[
000700                             LOCK        MODE         IS  AUTOMATIC.
000710*
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
000870*                           �m�q�k��  �Q�T�U�n
000880 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
000890     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000900*                           �m�q�k��  �R�Q�O�n
000910 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
000920     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000940*                           �m�q�k��  �U�S�O�n
000950 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
000960     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000880*                           �m�q�k��  �Q�T�U�n
000890 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
000900     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
000930*                           �m�q�k��  �Q�T�U�n
000940 FD  ��ƃt�@�C���P RECORD  CONTAINS 256 CHARACTERS.
000950 01 ��P�|���R�[�h.
000960    03 ��P�|���R�[�h�L�[.
000970       05  ��P�|�{�p�a��N����.
000980          07 ��P�|�{�p�a��               PIC 9.
000990          07 ��P�|�{�p�N��.
001000             09 ��P�|�{�p�N              PIC 9(2).
001010             09 ��P�|�{�p��              PIC 9(2).
001020          07 ��P�|�{�p��                 PIC 9(2).
001030    03 ��P�|���R�[�h�f�[�^.
001040       05 ��P�|���҃R�[�h.
001050          07 ��P�|���Ҕԍ�               PIC 9(6).
001060          07 ��P�|�}��                   PIC X.
001080       05 ��P�|���`�F�b�N                PIC 9.
             05 ��P�|��p�z                    PIC 9(6).
001130       05 ��P�|�ꕔ���S��                PIC 9(5).
001551       05 ��P�|�R�����g                  PIC X(100).
      *
             05 ��P�|������                    PIC 9(5).
             05 ��P�|�������Z.
                07 ��P�|�������ԊO             PIC 9.
                07 ��P�|�����x��               PIC 9.
                07 ��P�|�����[��               PIC 9.
             05 ��P�|�������Z��                PIC 9(5).
             05 ��P�|�Č���                    PIC 9(5).
             05 ��P�|����.
                07 ��P�|���Ë���               PIC 9(2)V9.
                07 ��P�|���É�               PIC 9(2).
                07 ��P�|���×�                 PIC 9(5).
                07 ��P�|���É��Z��             PIC 9(5).
                07 ��P�|���É��Z.
                   09 ��P�|���Ö��            PIC 9.
                   09 ��P�|���Ó�H            PIC 9.
                   09 ��P�|���Ö\���J          PIC 9.
                   09 ��P�|���Î��ԊO          PIC 9.
             05 ��P�|���񏈒u���v              PIC 9(6).
             05 ��P�|��×�                    PIC 9(6).
             05 ��P�|��㪖@��                  PIC 9(5).
             05 ��P�|��㪖@��                  PIC 9(5).
             05 ��P�|�d�×�                    PIC 9(5).
             05 ��P�|��                        PIC 9(1).
             05 ��P�|��                        PIC 9(1).
             05 ��P�|��                        PIC 9(1).
             05 ��P�|�������q���Z��            PIC 9(5).
             05 ��P�|�^����×�                PIC 9(4).
001210       05 FILLER                          PIC X(54).
001150*
001160 FD  ����t�@�C��.
001170     COPY YCB7221P        OF  XMDLIB.
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
001320 01 ���ʒu�b�m�s                       PIC 9(2)  VALUE ZERO.
001330 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
001340 01 ���Ҕԍ��v                         PIC 9(6)  VALUE ZERO.
       01 �s�J�E���^                         PIC 9(2)  VALUE ZERO.
       01 �����b�m�s                         PIC 9(2)  VALUE ZERO.
002520 01 ���ʐ��v                           PIC 9     VALUE ZERO.
001210 01 �I�[�v���t���O                     PIC X(3)   VALUE SPACE.
001350**
001360 01 �x���t���O                         PIC X(3) VALUE SPACE.
001370 01 �x���񐔂v                         PIC 9(4) VALUE ZERO.
001380 01 �x���b�m�s                         PIC 9(5) VALUE ZERO.
001390**
       01 ��p�z�v                           PIC 9(6) VALUE ZERO.
       01 �����z�v                           PIC 9(6) VALUE ZERO.
       01 ���S�z�v                           PIC 9(6) VALUE ZERO.
      *
       01 �R�����g�v.
         03 �R�����g�v�P                     PIC X(30) VALUE SPACE.
         03 �R�����g�v�Q                     PIC X(30) VALUE SPACE.
         03 �R�����g�v�R                     PIC X(30) VALUE SPACE.
       01 ���Ńt���O                         PIC 9(1) VALUE ZERO.
001400*
001540 01 �ޔ����ڂf�v.
001550   03 ���Z�v�g��ނv                 PIC X(4) VALUE SPACE.
001560   03 ���Z�v�g��ނf�v               PIC X(4) VALUE SPACE.
001570   03 ���Z�v�g��ʂf�v               PIC 9(2) VALUE ZERO.
      *
       01 ���É��Z�񐔂v                   PIC 9(2) VALUE ZERO.
      * 01 �I���a��N�����v                 PIC 9(7) VALUE ZERO.
      *
       01 ���ʃJ�E���^                     PIC 9(2) VALUE ZERO.
       01 ���ʃJ�E���^�Q                   PIC 9(2) VALUE ZERO.
      *
       01 �p�����ʐ��v                       PIC 9(2) VALUE ZERO.
      *
       01 �J�n�a��N�����v.
         03 �J�n�a��N���v.
           05 �J�n�a��v                     PIC 9.
           05 �J�n�N���v.
              07 �J�n�N�v                    PIC 9(2).
              07 �J�n���v                    PIC 9(2).
         03 �J�n���v                         PIC 9(2).
      *
       01 �I���a��N�����v.
         03 �I���a��N���v.
           05 �I���a��v                     PIC 9.
           05 �I���N���v.
              07 �I���N�v                    PIC 9(2).
              07 �I�����v                    PIC 9(2).
         03 �I�����v                         PIC 9(2).
      *
       01 ��É\�Z�萔�j�v                 PIC 9(2) VALUE ZERO.
       01 �]�A�敪�v                         PIC 9 VALUE ZERO.
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
001550    03 ���҃R�[�h�v�q.
001560       05 ���Ҕԍ��v�q                  PIC 9(6)  VALUE ZERO.
001570       05 �}�Ԃv�q                      PIC X(1)  VALUE SPACE.
001580    03 ������[�h�e�v�q                 PIC 9(1)  VALUE ZERO.
001590    03 �����ƃ��[�h�v�q                 PIC 9(1)  VALUE ZERO.
001600    03 �������[�h�v�q                   PIC 9(1)  VALUE ZERO.
001610    03 �N�����[�h�v�q                   PIC 9(1)  VALUE ZERO.
001630    03 �������[�h�v�q                   PIC 9(1)  VALUE ZERO.
001630    03 ���v���[�h�v�q                   PIC 9(1)  VALUE ZERO.
001630    03 �R�����g���[�h�v�q               PIC 9(1)  VALUE ZERO.
001640    03 �󎚈ʒu�b�m�s                   PIC 9     VALUE ZERO.
          03 �J�n�i���v�q                     PIC 9(2)  VALUE ZERO.
001650**************
001660* ��f�ҏ�� *
001670**************
001680 01 ��f�ҏ��v.
001690    03 �{�p�N���v.
001700       05 �{�p�N�v                     PIC 9(2)   VALUE ZERO.
001710       05 �{�p���v                     PIC 9(2)   VALUE ZERO.
001720    03 �ی��Ҕԍ��v.
001730       05 ����ی��Ҕԍ��v             PIC X(6)   VALUE SPACE.
001740       05 FILLER                       PIC X(4)   VALUE SPACE.
001750    03 �ސE�ی��Ҕԍ��v.
001760       05 FILLER                       PIC X(2)   VALUE SPACE.
001770       05 �ސE����ی��Ҕԍ��v         PIC X(6)   VALUE SPACE.
001780       05 FILLER                       PIC X(2)   VALUE SPACE.
001790    03 �s�����ԍ��v.
001800       05 ����s�����ԍ��v             PIC X(8)   VALUE SPACE.
001810       05 FILLER                       PIC X(2)   VALUE SPACE.
001820    03 �ی���ʂv                      PIC 9(2)   VALUE ZERO.
001830    03 ���ҏ��v.
001840       05 ���҃J�i�v                   PIC X(50)  VALUE SPACE.
001850       05 ���Ҏ����v                   PIC X(50)  VALUE SPACE.
002160 01 ����.
002170    03 �d㪗��v                           PIC 9(4) VALUE ZERO.
002180    03 �ꕔ���S���v                       PIC 9(5) VALUE ZERO.
002190    03 ���ʂv                             OCCURS 4.
002200       05 ���ʌv�v                        PIC 9(4) VALUE ZERO.
002210       05 �����v                          PIC 9(2) VALUE ZERO.
002220*******************************************************************
002230 01 �������.
002240     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
002250     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
002260     03 ������ʂo                     PIC X(2) VALUE SPACE.
002270     03 �g������o.
002280         05 �[������o.
002290             07 �ړ������o             PIC X(1) VALUE SPACE.
002300             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
002310         05 �ڍא���o                 PIC X(2) VALUE SPACE.
002320     03 �ʒm���o                     PIC X(2) VALUE SPACE.
002330     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
002340*
002350 01 �v�Z�@����N�v                     PIC 9(2) VALUE ZERO.
002360* ���t�v�n�q�j
002370 01 �a��I���N�v                       PIC 9(4) VALUE ZERO.
002380 01 �v�Z�@����.
002390    03 �v�Z�@����N                    PIC 9(4) VALUE ZERO.
002400    03 �v�Z�@�����                  PIC 9(4) VALUE ZERO.
002410 01 �v�Z�@����q REDEFINES �v�Z�@����.
002420    03 �v�Z�@���I                      PIC 9(2).
002430    03 �v�Z�@���t                      PIC 9(6).
002440    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
002450       05 �v�Z�@�N��                   PIC 9(4).
002460       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
002470         07 �v�Z�@�N                   PIC 9(2).
002480         07 �v�Z�@��                   PIC 9(2).
002490       05 �v�Z�@��                     PIC 9(2).
002500*
002510******************************************************************
002520*                          �A������                              *
002530******************************************************************
002820****************
002830* ��ʓ��͏�� *
002840****************
002850 01 �A���|���̓f�[�^�x�b�a�V�Q�O IS EXTERNAL.
002860    03 �A���|�{�p�a��N��.
002870       05 �A���|�{�p�a��                  PIC 9(1).
002880       05 �A���|�{�p�N                    PIC 9(2).
002890       05 �A���|�{�p��                    PIC 9(2).
002900    03 �A���|�J�n���t.
002910       05 �A���|�J�n��                    PIC 9(2).
002920    03 �A���|�I�����t.
002930       05 �A���|�I����                    PIC 9(2).
002940    03 �A���|���҃R�[�h.
002950       05 �A���|���Ҕԍ�                  PIC 9(6).
002960       05 �A���|�}��                      PIC X(1).
002970    03 �A���|������[�h�e                 PIC 9(1).
002980    03 �A���|�����ƃ��[�h                 PIC 9(1).
002990    03 �A���|�������[�h                   PIC 9(1).
          03 �A���|�R�����g���[�h               PIC 9(1).
          03 �A���|����i��                     PIC 9(2).
      */�A�����̎�������̗L��0302
          03 �A���|�������[�h                   PIC X(4).
003070*
006580 01 �A���|�L�[ IS EXTERNAL.
006590    03 �A���|�ی����                       PIC 9(2).
006600*
006040**********************
006050* ���b�Z�[�W�\���L�[ *
006060**********************
006070 01 �A���|�L�[ IS EXTERNAL.
006080    03  �A���|���b�Z�[�W                 PIC N(20).
      *
004100 01 �A���T�P�|�L�[ IS EXTERNAL.
004110    03  �A���T�P�|���b�Z�[�W               PIC N(20).
004120    03  �A���T�P�|�����l                   PIC X.
004121    03  �A���T�P�|�Ԃ�l                   PIC X.
003020*
       01 �A���|�\���t���O�x�b�a�V�Q�O IS EXTERNAL GLOBAL.
          03 �A���|�v���r���[�敪               PIC 9(1).
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
007308*
003110******************************************************************
003120*                      PROCEDURE  DIVISION                       *
003130******************************************************************
003140 PROCEDURE               DIVISION.
003150************
003160*           *
003170* ��������   *
003180*           *
003190************
002570     PERFORM �v�����^�t�@�C���쐬.
003200     PERFORM ������.
003210     PERFORM ������擾.
003220************
003230*           *
003240* �又��     *
003250*           *
003260************
003270* ���
           MOVE ZERO TO ���Ńt���O.
003280     PERFORM �A�����ڑҔ�.
003290     PERFORM ����Z�b�g.
      *
      */���ł��Ĉ�����Ă����ꍇ�A���y�[�W�̈����I�ԁB�F�P
      */���ł��Ĉ�����Ȃ������ꍇ�A�������ň�����Ȃ��B�F�Q
      */���ł����Ă��Ȃ������ꍇ�A�������ň������B    �F�O
           EVALUATE ���Ńt���O
           WHEN 1
               MOVE  NC"�@�@�@���̗p�����Z�b�g���Ă�������" TO �A���T�P�|���b�Z�[�W
               MOVE "Y" TO �A���T�P�|�����l
               CALL   "MSG0051"
               CANCEL "MSG0051"
           WHEN 2
               MOVE "N" TO �A���T�P�|�Ԃ�l
           WHEN ZERO
               MOVE "Y" TO �A���T�P�|�Ԃ�l
           WHEN OTHER
               MOVE "Y" TO �A���T�P�|�Ԃ�l
           END-EVALUATE
      *
           IF �A���T�P�|�Ԃ�l = "Y"
003300         PERFORM �������
               PERFORM ����s���ޔ�
           END-IF.
003310************
003320*           *
003330* �I������   *
003340*           *
003350************
003360     PERFORM �I������.
003370     PERFORM �x������.
003380     EXIT PROGRAM.
003390*
003400*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCB7221"             TO �g�A�o�q�s�e�|���[�v���O������.
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
003410*================================================================*
003420 ������ SECTION.
003430*
004000     OPEN INPUT   ������}�X�^
004010         MOVE NC"������" TO �t�@�C����.
004020         PERFORM �I�[�v���`�F�b�N.
004030     OPEN INPUT   ��f�ҏ��e.
004040         MOVE NC"���" TO �t�@�C����.
004050         PERFORM �I�[�v���`�F�b�N.
007250     OPEN INPUT �����f�[�^�e.
007260         MOVE NC"����" TO �t�@�C����.
007270         PERFORM �I�[�v���`�F�b�N.
007190     OPEN INPUT �{�p�L�^�e.
007200         MOVE NC"�{�L" TO �t�@�C����.
007210         PERFORM �I�[�v���`�F�b�N.
007870     OPEN INPUT ���Z�v�g�e.
007880         MOVE NC"���Z"         TO �t�@�C����.
007890         PERFORM �I�[�v���`�F�b�N.
004060     OPEN INPUT ��ƃt�@�C���P
004070         MOVE NC"��ƂP" TO �t�@�C����.
004080         PERFORM �I�[�v���`�F�b�N.
004110*================================================================*
004120 �I�[�v���`�F�b�N SECTION.
004130*
004140     IF ��ԃL�[  NOT =  "00"
004150         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
004160         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
004170         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004180                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
004190         ACCEPT  �L�[���� FROM CONS
004200         PERFORM �t�@�C����
004210         EXIT PROGRAM.
004220*================================================================*
004230 ������擾 SECTION.
004240*
004250     MOVE ZERO TO ���|����敪
004260     READ ������}�X�^
004270     NOT INVALID KEY
004280         MOVE ���|�x����       TO �x���񐔂v
004290     END-READ.
004300*
004310*================================================================*
004320 �x������ SECTION.
004330*
004340     PERFORM VARYING �x���b�m�s FROM 1 BY 1
004350               UNTIL �x���b�m�s > �x���񐔂v
004360         MOVE SPACE TO �x���t���O
004370     END-PERFORM.
004380*
004390*================================================================*
004400 �A�����ڑҔ� SECTION.
004410*
004420     MOVE �A���|�{�p�a��           TO �{�p�a��v�q.
004430     MOVE �A���|�{�p�N             TO �{�p�N�v�q.
004440     MOVE �A���|�{�p��             TO �{�p���v�q.
004450     MOVE �A���|�J�n��             TO �J�n���v�q.
004460     MOVE �A���|�I����             TO �I�����v�q.
004470     MOVE �A���|���Ҕԍ�           TO ���Ҕԍ��v�q.
004480     MOVE �A���|�}��               TO �}�Ԃv�q.
004490     MOVE �A���|������[�h�e       TO ������[�h�e�v�q.
004500     MOVE �A���|�����ƃ��[�h       TO �����ƃ��[�h�v�q.
004510     MOVE �A���|�������[�h         TO �������[�h�v�q.
004540     MOVE �A���|�R�����g���[�h     TO �R�����g���[�h�v�q.
           MOVE �A���|����i��           TO �J�n�i���v�q.
004560*================================================================*
004570 ����Z�b�g SECTION.
004580*
004590     PERFORM ���ڏ�����.
004600     PERFORM ��f�ҏ��擾.
004640     IF �������[�h�v�q = 1
004650         MOVE ���Ҏ����v     TO ���Ҏ���
      *         MOVE ���Ҕԍ��v�q   TO ���Ҕԍ�
      *         MOVE �}�Ԃv�q       TO �}��
004660     END-IF.
004800     IF (�����ƃ��[�h�v�q   = 1) OR
              (�R�����g���[�h�v�q = 1)
               PERFORM ���׍s�Z�b�g
               PERFORM �������z�Z�b�g
004890     END-IF.
004900*================================================================*
005310 ���ڏ����� SECTION.
005320*
005330     INITIALIZE ��f�ҏ��v.
005350     INITIALIZE ����.
005360     MOVE SPACE TO YCB7221P.
005370     INITIALIZE YCB7221P.
005380*================================================================*
005390 ���׍s�Z�b�g SECTION.
005480     MOVE �{�p�a��v�q       TO ��P�|�{�p�a��
005490     MOVE �{�p�N�v�q         TO ��P�|�{�p�N
005500     MOVE �{�p���v�q         TO ��P�|�{�p��
005510     MOVE �J�n���v�q         TO ��P�|�{�p��
005520     START ��ƃt�@�C���P KEY IS >= ��P�|�{�p�a��N����
005530     END-START
005540     IF ��ԃL�[ = "00"
005550         PERFORM ��ƃt�@�C���P�Ǎ�
               IF �J�n�i���v�q = ZERO
                   MOVE 1 TO �s�J�E���^
               ELSE
                   MOVE �J�n�i���v�q TO �s�J�E���^
               END-IF
               PERFORM UNTIL �I���t���O = "YES"
005470             IF �����ƃ��[�h�v�q = 1
                       MOVE ��P�|��p�z       TO ��p�z      (�s�J�E���^)
                       MOVE ��P�|�{�p��       TO �{�p��      (�s�J�E���^)
                       MOVE ��P�|�{�p��       TO �{�p��      (�s�J�E���^)
                       MOVE ��P�|�ꕔ���S��   TO ���S�z      (�s�J�E���^)
                       MOVE ��P�|�������Z��   TO �������Z��  (�s�J�E���^)
                       IF �{�p�a��N���v�q < 43006
                           MOVE ��P�|���񏈒u���v TO ���񏈒u���v(�s�J�E���^)
                       ELSE
                           COMPUTE ���񏈒u���v(�s�J�E���^) = ��P�|���񏈒u���v + ��P�|�������q���Z��
                                                            + ��P�|�^����×�
                       END-IF
                       MOVE ��P�|��×�       TO ��×��v    (�s�J�E���^)
                       MOVE ��P�|�d�×�       TO �d�×��v    (�s�J�E���^)
                       COMPUTE �����Č����×�(�s�J�E���^) = ��P�|������ + 
                              ��P�|�Č��� + ��P�|���×� + ��P�|���É��Z��
                       COMPUTE 㪖@���v(�s�J�E���^) = ��P�|��㪖@�� +
                                                      ��P�|��㪖@��
      *
005690             END-IF
                   IF �R�����g���[�h�v�q = 1
                       MOVE ��P�|�R�����g       TO �R�����g�v
                       MOVE �R�����g�v�P         TO �R�����g�P(�s�J�E���^)
                       MOVE �R�����g�v�Q         TO �R�����g�Q(�s�J�E���^)
                       MOVE �R�����g�v�R         TO �R�����g�R(�s�J�E���^)
                   END-IF
      *
                   COMPUTE �s�J�E���^ = �s�J�E���^ + 1
005700             PERFORM ��ƃt�@�C���P�Ǎ�
      *
      */���׍s���R�P�s�ŉ���
                   IF (�s�J�E���^ > 31) AND (�I���t���O = SPACE)
                       IF ���Ńt���O NOT = ZERO
                           MOVE  NC"�@�@�@���̗p�����Z�b�g���Ă�������" TO �A���T�P�|���b�Z�[�W
                           MOVE "Y" TO �A���T�P�|�����l
                           CALL   "MSG0051"
                           CANCEL "MSG0051"
                       ELSE
                           MOVE "Y" TO �A���T�P�|�Ԃ�l
                       END-IF
                       IF �A���T�P�|�Ԃ�l = "Y"
                           MOVE 1 TO ���Ńt���O
                           PERFORM �������
                           PERFORM ���ŏ���
                           PERFORM ����s���ޔ�
                           MOVE SPACE TO YCB7221P
�@                         INITIALIZE YCB7221P
                           MOVE 1 TO �s�J�E���^
      *                   */���Ō�͎������������
                           MOVE ���Ҏ����v TO ���Ҏ���
                       ELSE
                           MOVE 2 TO ���Ńt���O
                           MOVE "YES" TO �I���t���O
                       END-IF
                   END-IF
005710         END-PERFORM
005720     END-IF.
005740*================================================================*
006890 ��f�ҏ��擾 SECTION.
006900*
006910**************************************************
006920* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
006930* �� �{�p�N ..... �{�p�N�v�Ɋi�[                 *
006940* �� �{�p�� ..... �{�p���v�Ɋi�[                 *
006950* �� ���Ҕԍ�.... ���Ҕԍ��v�Ɋi�[���e�c�A�ԗp   *
006960* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
006970**************************************************
006980     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
006990     MOVE �{�p�N�v�q         TO ��|�{�p�N.
007000     MOVE �{�p���v�q         TO ��|�{�p��.
007010     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
007020     READ ��f�ҏ��e
007030     INVALID KEY
007040         CONTINUE
007050*            /* ���肦�Ȃ� */
007060     NOT INVALID KEY
007070         MOVE ��|�{�p�N       TO �{�p�N�v
007080         MOVE ��|�{�p��       TO �{�p���v
007090         MOVE ��|���Ҕԍ�     TO ���Ҕԍ��v
007100         MOVE ��|���Ҏ���     TO ���Ҏ����v
007110     END-READ.
007120*================================================================*
007130 ��ƃt�@�C���P�Ǎ� SECTION.
007140*
007150     READ ��ƃt�@�C���P NEXT
007160     AT END
007170         MOVE "YES" TO �I���t���O
007180     END-READ.
007190*
007200*================================================================*
007210 ������� SECTION.
007220*
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
013440*
007230     MOVE "YCB7221P"  TO  ��`�̖��o.
007240     MOVE "GRP001"   TO  ���ڌQ���o.
007250     WRITE YCB7221P.
007260     PERFORM �G���[�����o.
007270*================================================================*
007280 �G���[�����o SECTION.
007290*
007300     IF �ʒm���o NOT = "00"
007310         DISPLAY NC"���[�G���["              UPON CONS
007320         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
007330         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
007340         DISPLAY NC"�g������o�F" �g������o UPON CONS
007350         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
007360                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007370         ACCEPT  �L�[���� FROM CONS
007380         PERFORM �t�@�C����
007390         MOVE 99 TO PROGRAM-STATUS
007400         EXIT PROGRAM
007410     END-IF.
007420*================================================================*
007430 �t�@�C���� SECTION.
007440*
002990     IF ( �I�[�v���t���O = "YES" )
002991         CLOSE ����t�@�C��
003041     END-IF.
007460     CLOSE ������}�X�^ ��f�ҏ��e 
                 �����f�[�^�e  �{�p�L�^�e     ���Z�v�g�e.
007470*================================================================*
007480 �I������ SECTION.
007490*
007500     PERFORM �t�@�C����.
007510*================================================================*
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
006970     MOVE ���҃R�[�h�v�q  TO ��|���҃R�[�h.
006990*
007000     READ ��f�ҏ��e
007010     NOT INVALID KEY
               IF �s�J�E���^ > 31
                   MOVE 1 TO �s�J�E���^
               END-IF
007020         MOVE �s�J�E���^ TO ��|�J���e������s��
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
007530*================================================================*
007620 �G���[�\�� SECTION.
007630*
007640     DISPLAY �t�@�C����   NC"�t�@�C�������G���["   UPON CONS.
007650     DISPLAY NC"��ԃL�[�F" ��ԃL�[               UPON CONS.
007660     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
007670     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
                                                         UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
007680     ACCEPT  �L�[���� FROM CONS.
007690     PERFORM �t�@�C����.
007700     EXIT PROGRAM.
007290*================================================================*
012090 �{�p�L�^�e�Ǎ� SECTION.
012100*
012110     READ �{�p�L�^�e NEXT
012120     AT END
012130         MOVE "YES" TO �I���t���O�Q
012140     END-READ.
009040*================================================================*
009050 ���Z�v�g�ďo���P SECTION.
009060*
           IF ��|������� NOT = ZERO
              MOVE  3   TO ���Z�|���Z���
           ELSE
              IF ��|������ NOT = ZERO
                 MOVE  2   TO ���Z�|���Z���
              ELSE
                 MOVE  1   TO ���Z�|���Z���
              END-IF
           END-IF.
005200     MOVE ��|�{�p�a��  TO ���Z�|�{�p�a��.
005210     MOVE ��|�{�p�N    TO ���Z�|�{�p�N.  
005220     MOVE ��|�{�p��    TO ���Z�|�{�p��.  
005230     MOVE ��|���Ҕԍ�  TO ���Z�|���Ҕԍ�.
005240     MOVE ��|�}��      TO ���Z�|�}��.    
           READ ���Z�v�g�e.
009370*
023480*================================================================*
003740 ���ŏ���  SECTION.
003750*
003822     CLOSE  ����t�@�C��.
004320     MOVE SPACE TO �I�[�v���t���O.
003823*     OPEN I-O   ����t�@�C��.
003824*     PERFORM �G���[�����o.
003825*
007610*================================================================*
       �������z�Z�b�g SECTION.
      *
006980     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
006990     MOVE �{�p�N�v�q         TO ��|�{�p�N.
007000     MOVE �{�p���v�q         TO ��|�{�p��.
007010     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
007020     READ ��f�ҏ��e
007030     INVALID KEY
007040         CONTINUE
007050*            /* ���肦�Ȃ� */
007060     NOT INVALID KEY
007700         PERFORM ���Z�v�g�ďo���P
           END-READ.
      *
           IF �{�p�a��N���v�q < 43006
               COMPUTE �������񋟗� = ���Z�|�������q���Z�� + ���Z�|�{�p���񋟗�
           ELSE
               MOVE ���Z�|�{�p���񋟗� TO �������񋟗�
           END-IF.
      *
009040*================================================================*
007520******************************************************************
007530 END PROGRAM YCB7221.
007540******************************************************************
