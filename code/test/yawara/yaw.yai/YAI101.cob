000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAI101.
000060 AUTHOR.                 �R�c �_�V�@�r�c �K�q
000070*
000080*----------------------------------------------------------------*
000090*      ��oFPD�쐬�y�ް��쐬�z�_����޳��95��
000140*----------------------------------------------------------------*
000150 DATE-WRITTEN.           2010-04-08
000160 DATE-COMPILED.          2010-04-08
000170*----------------------------------------------------------------*
      */�������q�^����Òǉ�/20180612
      */���׏����s�ȍ~�ǉ�/20221117/�r�c
000180******************************************************************
000190*            ENVIRONMENT         DIVISION                        *
000200******************************************************************
000210 ENVIRONMENT             DIVISION.
000220 CONFIGURATION           SECTION.
000230 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000240 OBJECT-COMPUTER.        FMV-DESKPOWER.
000250 SPECIAL-NAMES.          CONSOLE  IS  CONS
000260                         SYSERR   IS  MSGBOX.
000270 INPUT-OUTPUT            SECTION.
000280 FILE-CONTROL.
000281     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000282                             ORGANIZATION             IS  INDEXED
000283                             ACCESS MODE              IS  DYNAMIC
000284                             RECORD KEY               IS  ���|����敪
000285                             FILE STATUS              IS  ��ԃL�[
000286                             LOCK        MODE         IS  AUTOMATIC.
000290     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000300                             ORGANIZATION             IS  INDEXED
000310                             ACCESS MODE              IS  DYNAMIC
000320                             RECORD KEY               IS  ���|�����敪
000330                             FILE STATUS              IS  ��ԃL�[
000340                             LOCK        MODE         IS  AUTOMATIC.
000350     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000360                             ORGANIZATION             IS  INDEXED
000370                             ACCESS MODE              IS  DYNAMIC
000380                             RECORD KEY               IS  ���|�敪�R�[�h
000390                                                          ���|���̃R�[�h
000400                             FILE STATUS              IS  ��ԃL�[
000410                             LOCK        MODE         IS  AUTOMATIC.
000420     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS �{��|�{�p���ԍ�
000460                             FILE STATUS              IS  ��ԃL�[
000470                             LOCK        MODE         IS  AUTOMATIC.
000480     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
000490                             ORGANIZATION             IS  INDEXED
000500                             ACCESS MODE              IS  DYNAMIC
000510                             RECORD KEY           IS �{�L�|�{�p�a��N����
000520                                                     �{�L�|���҃R�[�h
000530                             ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
000540                                                     �{�L�|�{�p�a��N����
000550                             FILE STATUS              IS  ��ԃL�[
000560                             LOCK        MODE         IS  AUTOMATIC.
000260     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  ��|�{�p�a��N��
000300                                                          ��|���҃R�[�h
000310                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000320                                                          ��|���҃J�i
000330                                                          ��|���҃R�[�h
000340                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000350                                                          ��|�{�p�a��N��
000360                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000370                                                          ��|�ی����
000380                                                          ��|�ی��Ҕԍ�
000390                                                          ��|���҃R�[�h
000400                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000410                                                          ��|������
000420                                                          ��|��p���S�Ҕԍ�
000430                                                          ��|���҃R�[�h
000440                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000450                                                          ��|�������
000460                                                          ��|��p���S�Ҕԍ�����
000470                                                          ��|���҃R�[�h
000480                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
000490                                                          ��|�{�p�a��N��
000500                                                          ��|���҃R�[�h
000510                             FILE STATUS              IS  ��ԃL�[
000520                             LOCK        MODE         IS  AUTOMATIC.
000840     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS ���|�{�p�a��N��
000880                                                         ���|���҃R�[�h
000890                             ALTERNATE RECORD KEY     IS ���|���҃R�[�h
000900                                                         ���|�{�p�a��N��
000910                             FILE STATUS              IS  ��ԃL�[
000920                             LOCK        MODE         IS  AUTOMATIC.
000921     SELECT  ���������e      ASSIGN      TO        HUGEINL
000922                             ORGANIZATION             IS  INDEXED
000923                             ACCESS MODE              IS  DYNAMIC
000924                             RECORD KEY               IS  �����|�敪�R�[�h
000925                                                          �����|���������R�[�h
000926                             FILE STATUS              IS  ��ԃL�[
000927                             LOCK        MODE         IS  AUTOMATIC.
000930     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000940                             ORGANIZATION             IS  INDEXED
000950                             ACCESS MODE              IS  DYNAMIC
000960                             RECORD KEY               IS  �s�|������
000970                                                          �s�|�s�����ԍ�
000980                             ALTERNATE RECORD KEY     IS  �s�|������
000990                                                          �s�|�s��������
001000                                                          �s�|�s�����ԍ�
001010                             FILE STATUS              IS  ��ԃL�[
001020                             LOCK        MODE         IS  AUTOMATIC.
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
000611     SELECT  ��v�f�[�^�e    ASSIGN      TO        KAIKEIL
000612                             ORGANIZATION             IS  INDEXED
000613                             ACCESS MODE              IS  DYNAMIC
000089                             RECORD KEY               IS  ��|�{�p�a��N����
000090                                                          ��|���҃R�[�h
000092                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000093                                                          ��|�{�p�a��N����
000621                             FILE STATUS              IS  ��ԃL�[
000622                             LOCK        MODE         IS  AUTOMATIC.
000500     SELECT  ���ʃ}�X�^      ASSIGN      TO        BUICODEL
000510                             ORGANIZATION             IS  INDEXED
000520                             ACCESS MODE              IS  DYNAMIC
000530                             RECORD KEY               IS  ���|���ʃR�[�h
000540                             FILE STATUS              IS  ��ԃL�[
000550                             LOCK        MODE         IS  AUTOMATIC.
001113     SELECT  �����p���҂e    ASSIGN      TO        CHOKEIL
001120                             ORGANIZATION             IS INDEXED
001130                             ACCESS MODE              IS DYNAMIC
001140                             RECORD KEY               IS ���p�|�{�p�a��N��
001150                                                         ���p�|���҃R�[�h
001160                             ALTERNATE RECORD KEY     IS ���p�|���҃R�[�h
001170                                                         ���p�|�{�p�a��N��
001180                             FILE STATUS              IS ��ԃL�[
001190                             LOCK      MODE           IS AUTOMATIC.
001090     SELECT  ��ƃt�@�C���P  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001100                             ORGANIZATION             IS  SEQUENTIAL
001110                             ACCESS                   IS  SEQUENTIAL
001120                             FILE        STATUS       IS  ��ԃL�[
001130                             LOCK        MODE         IS  AUTOMATIC.
001520     SELECT ��ƃt�@�C���Q   ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1012L.DAT"
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
000861                             RECORD      KEY          IS  ��Q�|�ی��ҋ敪
000952                             FILE STATUS              IS  ��ԃL�[
001600                             LOCK        MODE         IS  AUTOMATIC.
001520     SELECT ��ƃt�@�C���R   ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1013L.DAT"
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
000861                             RECORD      KEY          IS  ��R�|�ԍ�
000952                             FILE STATUS              IS  ��ԃL�[
001600                             LOCK        MODE         IS  AUTOMATIC.
001520     SELECT ��ƃt�@�C���S   ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1014L.DAT"
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
000861                             RECORD      KEY          IS  ��S�|�{�p�a��N��
                                                                ��S�|���҃R�[�h
                                                                ��S�|���Z���
000310                             ALTERNATE RECORD KEY     IS  ��S�|���R�[�h�敪
                                                                ��S�|��o�敪
                                                                ��S�|�ی��Ҕԍ�
                                                                ��S�|�{�p�a��N��
                                                                ��S�|���҃R�[�h
                                                                ��S�|���Z���
000952                             FILE STATUS              IS  ��ԃL�[
001600                             LOCK        MODE         IS  AUTOMATIC.
001224******************************************************************
001230*                      DATA DIVISION                             *
001240******************************************************************
001250 DATA                    DIVISION.
001260 FILE                    SECTION.
001261*                           �m�q�k��  �Q�T�U�n
001262 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001263     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001270*                           �m�q�k��  �P�Q�W�n
001280 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001290     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001300*                           �m�q�k��  �P�Q�W�n
001310 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001320     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001330*
001340 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
001350     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001360*                           �m�q�k��  �Q�T�U�n
001370 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
001380     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
001390*                           �m�q�k��  �R�Q�O�n
001400 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001410     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001420*                           �m�q�k��  �P�Q�W�n
001430 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001440     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001441*                           �m�q�k��  �P�Q�W�n
001442 FD  ���������e         BLOCK   CONTAINS   1   RECORDS.
001443     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
001450*                           �m�q�k��  �Q�T�U�n
001460 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001470     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
000981*                           �m�q�k��  �T�P�Q�n
000982 FD  ��v�f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
000983     COPY KAIKEI          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001580*                           �m�q�k��  �P�Q�W�n
001590 FD  ���ʃ}�X�^          BLOCK   CONTAINS   1   RECORDS.
001600     COPY BUICODE         OF  XFDLIB  JOINING   ��   AS  PREFIX.
003410*                           �m�q�k��  �P�Q�W�n
003420 FD  �����p���҂e        BLOCK   CONTAINS   1   RECORDS.
003430     COPY CHOKEI     OF  XFDLIB  JOINING   ���p   AS  PREFIX.
001510**
001520 FD ��ƃt�@�C���P RECORD  CONTAINS 20 CHARACTERS.
001530 01 ��P�|���R�[�h.
           03 ��P�|���R�[�h�f�[�^.
               05 ��P�|���R�[�h�敪             PIC 9(2).
               05 ��P�|����ԍ�                 PIC 9(5).
               05 ��P�|�����N��                 PIC 9(6).
               05 ��P�|�f�ÔN��                 PIC 9(6).
               05 ��P�|���R�[�h�h�c             PIC X(1).
002352*
001520 FD ��ƃt�@�C���Q RECORD  CONTAINS 24 CHARACTERS.
001530 01 ��Q�|���R�[�h.
           03 ��Q�|���R�[�h�f�[�^.
               05 ��Q�|���R�[�h�敪             PIC 9(2).
               05 ��Q�|�ی��ҋ敪               PIC 9(2).
               05 ��Q�|�{�p����                 PIC 9(3).
               05 ��Q�|�{�p����                 PIC 9(7).
               05 ��Q�|�Ԗߌ���                 PIC 9(3).
               05 ��Q�|�Ԗߋ��z                 PIC 9(7).
002352*
001520 FD ��ƃt�@�C���R RECORD  CONTAINS 24 CHARACTERS.
001530 01 ��R�|���R�[�h.
           03 ��R�|���R�[�h�f�[�^.
               05 ��R�|���R�[�h�敪             PIC 9(2).
               05 ��R�|�ԍ�                     PIC 9(2).
               05 ��R�|�{�l��������             PIC 9(3).
               05 ��R�|�{�l�������z             PIC 9(7).
               05 ��R�|�Ƒ���������             PIC 9(3).
               05 ��R�|�Ƒ��������z             PIC 9(7).
002352*
001520* FD ��ƃt�@�C���S RECORD  CONTAINS 1777 CHARACTERS.
001520* FD ��ƃt�@�C���S RECORD  CONTAINS 1784 CHARACTERS.
001520 FD ��ƃt�@�C���S RECORD  CONTAINS 2023 CHARACTERS.
001530 01 ��S�|���R�[�h.
000510     03 ��S�|���R�[�h�L�[.
000520         05 ��S�|�{�p�a��N��.
000530             07 ��S�|�{�p�a��             PIC 9.
000540             07 ��S�|�{�p�N��.
000550                 09 ��S�|�{�p�N           PIC 9(2).
000560                 09 ��S�|�{�p��           PIC 9(2).
000570         05 ��S�|���҃R�[�h.
000580             07 ��S�|���Ҕԍ�             PIC 9(6).
000590             07 ��S�|�}��                 PIC X.
               05 ��S�|���Z���                 PIC 9(2).
           03 ��S�|���R�[�h�f�[�^.
               05 ��S�|���R�[�h�敪             PIC 9(2).
               05 ��S�|��o�敪                 PIC 9(1).
               05 ��S�|�ی��Ҕԍ�               PIC X(8).
               05 ��S�|�ی��؋L��.
                   07 ��S�|�ی��؋L���m         PIC N(10).
               05 ��S�|�ی��ؔԍ�               PIC X(16).
               05 ��S�|�V�l��Ï����敪         PIC 9(1).
               05 ��S�|�s�����ԍ�               PIC X(8).
               05 ��S�|�󋋎Ҕԍ�               PIC X(16).
               05 ��S�|�{�l�Ƒ��敪             PIC 9(1).
               05 ��S�|��Ï����敪             PIC 9(1).
               05 ��S�|��ی��҃J�i             PIC X(20).
               05 ��S�|��ی��Ҏ���.
                   07 ��S�|��ی��Ҏ����m       PIC N(16).
               05 ��S�|���҃J�i                 PIC X(20).
               05 ��S�|���Ҏ���.
                   07 ��S�|���Ҏ����m           PIC N(16).
               05 ��S�|���Ґ���                 PIC 9(1).
               05 ��S�|���Ґ��N����             PIC 9(8).
               05 ��S�|���v���z                 PIC 9(7).
               05 ��S�|���t����                 PIC 9(2).
               05 ��S�|�ꕔ���S��               PIC 9(6).
               05 ��S�|�������z                 PIC 9(6).
               05 ��S�|�f�ÔN��                 PIC 9(6).
               05 ��S�|���ʐ�                   PIC 9(1).
               05 ��S�|������                   PIC 9(3).
               05 ��S�|�[���敪                 PIC 9(2).
               05 ��S�|���t����                 PIC X(1).
               05 ��S�|�V�K�敪                 PIC 9(1).
               05 ��S�|�p���敪                 PIC 9(1).
               05 ��S�|������                 PIC 9(1).
               05 ��S�|�������ԊO���Z��       PIC 9(1).
               05 ��S�|�����x�����Z��         PIC 9(1).
               05 ��S�|�����[����Z��         PIC 9(1).
               05 ��S�|�Č���                 PIC 9(1).
               05 ��S�|���Ë���                 PIC 9(3).
               05 ��S�|���É�                 PIC 9(2).
               05 ��S�|��ԉ��Z���É�         PIC 9(1).
               05 ��S�|��H���Z���É�         PIC 9(2).
               05 ��S�|�\���J����Z���É�     PIC 9(2).
000561         05 ��S�|�������q���           PIC 9.
000562         05 ��S�|�������q����           PIC 9.
000563         05 ��S�|�������q����           PIC 9.
000564         05 ��S�|���񋟗���           PIC 9.
000566         05 ��S�|�������ʃf�[�^  OCCURS 8.
000567             07 ��S�|�����敪             PIC 9.
                   07 ��S�|��ʃR�[�h.
                       09 ��S�|�������R�[�h     PIC 9(3).
                       09 ��S�|�ו��ʃR�[�h     PIC 9(2).
                       09 ��S�|���E�敪         PIC 9(1).
000568             07 ��S�|������               PIC N(16).
000569             07 ��S�|���񏈒u��         PIC 9.
                   07 ��S�|��������.
                       09 ��S�|���������m       PIC N(20).
                   07 ��S�|�������R.
                       09 ��S�|�������R�m       PIC N(30).
000570             07 ��S�|�����N����           PIC 9(8).
000571             07 ��S�|�����N����           PIC 9(8).
000572             07 ��S�|�{�p�J�n�N����       PIC 9(8).
000573             07 ��S�|�{�p�I���N����       PIC 9(8).
000574             07 ��S�|���ʎ�����           PIC 9(2).
000575             07 ��S�|�]�A�敪             PIC 9.
000576             07 ��S�|��É�             PIC 9(2).
000577             07 ��S�|��㪖@��           PIC 9.
000578             07 ��S�|��㪖@��           PIC 9(2).
000579             07 ��S�|�d�É�             PIC 9(2).
000580             07 ��S�|�����ʒ����敪       PIC 9.
000581             07 ��S�|���������敪         PIC 9.
                   07 ��S�|�����ʔ�p�z         PIC 9(6).
               05 ��S�|���k�x����             PIC 9.
      */�������q�^����Òǉ�/20180612
               05 ��S�|�������q��             PIC 9.
               05 ��S�|�^����×���           PIC 9.
               05 ��S�|�^����×�               PIC 9(5).
      */���׏����s�ȍ~�ǉ�/20221117
               05 ��S�|���׏����s��           PIC 9.
               05 ��S�|���׏����s��             PIC 9(3).
               05 ��S�|���׏����s����           PIC 9(4).
002210         05 ��S�|��.
002210             07 ��S�|�{�p��               PIC X(1) OCCURS 31.
002210         05 ��S�|������                   PIC 9(5).
002210         05 ��S�|�������Z                 PIC 9(5).
002210         05 ��S�|���k�x����               PIC 9(5).
002210         05 ��S�|�Č���                   PIC 9(5).
002210         05 ��S�|���×�                   PIC 9(5).
002210         05 ��S�|���É��Z                 PIC 9(5).
000561         05 ��S�|�������q                 PIC 9(5).
002210         05 ��S�|���񋟗�               PIC 9(5).
002760         05 ��S�|���ʃf�[�^  OCCURS 8.
                   07 ��S�|��×�               PIC 9(5).
                   07 ��S�|��㪖@��             PIC 9(5).
                   07 ��S�|��㪖@��             PIC 9(5).
                   07 ��S�|�d�×�               PIC 9(5).
002353*----------------------------------------------------------------*
002354******************************************************************
002360*                WORKING-STORAGE SECTION                         *
002370******************************************************************
002380 WORKING-STORAGE         SECTION.
002390 01 �L�[����                           PIC X    VALUE SPACE.
002400 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
002410 01 �����t���O                         PIC X(3) VALUE SPACE.
002420 01 �I���t���O                         PIC X(3) VALUE SPACE.
002430 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
001270 01 �I���t���O�S                       PIC X(3) VALUE SPACE.
002440 01 ���s�L�[�v                         PIC X(3) VALUE SPACE.
002450 01 �{�p�L�^�L�v                       PIC X(3) VALUE SPACE.
002460 01 �t�@�C����                         PIC N(8) VALUE SPACE.
002470*
002480 01 �ی���ʂv�q                       PIC 9(2) VALUE ZERO.
002490 01 ���҃R�[�h�v�q.
002500    03 ���Ҕԍ��v�q                    PIC 9(6) VALUE ZERO.
002510    03 �}�Ԃv�q                        PIC X    VALUE SPACE.
002520*
002530 01 ����`���v�q                       PIC 9    VALUE ZERO.
002540 01 �ی��Ҕԍ��v�q                     PIC X(10) VALUE SPACE.
002550 01 ���Z�v�g��ނv�q                   PIC X(4) VALUE SPACE.
002560 01 �{�l�Ƒ��敪�v�q                   PIC 9    VALUE ZERO.
002570 01 �����v                             PIC N(2) VALUE SPACE.
002580 01 �{�p�a��N���v�q.
002590    03 �{�p�a��v�q                    PIC 9    VALUE ZERO.
002600    03 �{�p�N�v�q                      PIC 9(2) VALUE ZERO.
002610    03 �{�p���v�q                      PIC 9(2) VALUE ZERO.
002620 01 �����a��N���v�q.
002630    03 �����a��v�q                    PIC 9    VALUE ZERO.
002640    03 �����N�v�q                      PIC 9(2) VALUE ZERO.
002650    03 �������v�q                      PIC 9(2) VALUE ZERO.
002660**
002670 01 �A�Ԃv                             PIC 9(4) VALUE ZERO.
002680 01 �����t���O                         PIC X(3) VALUE SPACE.
002690 01 �������̂v                         PIC N(6)  VALUE SPACE.
002700 01 ������ʕϊ��O�v                   PIC 9(2)  VALUE ZERO.
002710 01 ������ʕϊ���v                   PIC 9     VALUE ZERO.
002720 01 �]�A�ϊ��O�v                       PIC 9     VALUE ZERO.
002730 01 �]�A�ϊ���v                       PIC 9     VALUE ZERO.
002740**
002750 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002760 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
002770 01 �J�E���^�Q                         PIC 9(3)  VALUE ZERO.
002780 01 �J�E���^�R                         PIC 9(2)  VALUE ZERO.
002790 01 �S�p��                           PIC X(2)  VALUE X"8140".
002800 01 ���p��                           PIC X(2)  VALUE X"2020".
002810**
       01 �����v.
          03 �S�p�����v                      PIC N(10) VALUE SPACE.
002820** �G���[���b�Z�[�W�p
002830 01 �G���[���b�Z�[�W�v.
002840    03 �G���[���҃R�[�h�v              PIC X(7) VALUE SPACE.
002850    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
002860    03 �G���[�ی���ʂv                PIC X(2) VALUE SPACE.
002870    03 FILLER                          PIC X(10) VALUE SPACE.
002880** �ی��Ҕԍ��E�l�ߗp
002890 01 �ی��Ҕԍ��v�s.
002900    03 �ی��Ҕԍ����l�߂v.
002910      05 �ی��Ҕԍ����l�߂v�P          PIC X OCCURS 8 VALUE SPACE.
002920    03 �ی��Ҕԍ��E�l�߂v.
002930      05 �ی��Ҕԍ��E�l�߂v�P          PIC X OCCURS 8 VALUE ZERO.
002940    03 �ی��Ҕԍ������v                PIC 9(8)  VALUE ZERO.
002950    03 �ی��Ҕԍ��v                    PIC X(8)  VALUE SPACE.
002960** ����ԍ��E�l�ߗp
002970 01 ����ԍ��v�s.
002980    03 ����ԍ����l�߂v.
002990      05 ����ԍ����l�߂v�P            PIC X OCCURS 7 VALUE SPACE.
003000    03 ����ԍ��E�l�߂v.
003010      05 ����ԍ��E�l�߂v�P            PIC X OCCURS 7 VALUE ZERO.
003020    03 ����ԍ������v                  PIC 9(7)  VALUE ZERO.
003030    03 ����ԍ��v                      PIC X(7)  VALUE SPACE.
003040** ������t���[�N�p
003050 01 ����N���v.
003060    03 ����N�v                        PIC 9(4) VALUE ZERO.
003070    03 ����v                        PIC 9(2) VALUE ZERO.
003080** ������N���p
003090 01 ������N���v.
003100    03 ������N�v                    PIC 9(4) VALUE ZERO.
003110    03 ��������v                    PIC 9(2) VALUE ZERO.
003120** ����{�p�N���p
003130 01 ����{�p�N���v.
003140    03 ����{�p�N�v                    PIC 9(4) VALUE ZERO.
003150    03 ����{�p���v                    PIC 9(2) VALUE ZERO.
003080** �����o�N���p
003090 01 �����o�N���v.
003100    03 �����o�N�v                    PIC 9(4) VALUE ZERO.
003110    03 �����o���v                    PIC 9(2) VALUE ZERO.

003160** �L�����l�ߗp
003170 01 �L���v�s.
003180    03 �L�����v.
003190      05 �L�����v�P                    PIC N OCCURS 12 VALUE SPACE.
003200    03 �L�����l�߂v.
003210      05 �L�����l�߂v�P                PIC N OCCURS 12 VALUE SPACE.
003180    03 �L�����w�v.
003190      05 �L�����w�v�P                  PIC X OCCURS 24 VALUE SPACE.
003200    03 �L�����l�߂w�v.
003210      05 �L�����l�߂w�v�P              PIC X OCCURS 24 VALUE SPACE.
003220    03 �L���v.
003230      05 �L���m�v                      PIC N(12) VALUE SPACE.
003240    03 �L���o�v.
003250      05 �L���o�m�v                    PIC X(24) VALUE SPACE.
003260** �������S�Ҕԍ����l�ߗp
003270 01 �����ԍ��v�s.
003280    03 �����ԍ����v.
003290      05 �����ԍ����v�P                PIC X OCCURS 10 VALUE SPACE.
003300    03 �����ԍ����l�߂v.
003310      05 �����ԍ����l�߂v�P            PIC X OCCURS 10 VALUE SPACE.
003320    03 �����ԍ��v                      PIC X(10) VALUE SPACE.
003330*
003340** ����N�������[�N�p
003350 01 �v�Z����N�����v.
003360    03 �v�Z����N�v                    PIC 9(4) VALUE ZERO.
003370    03 �v�Z����v                    PIC 9(2) VALUE ZERO.
003380    03 �v�Z������v                    PIC 9(2) VALUE ZERO.
003390 01 �v�Z�a��N�����v.
003400    03 �v�Z�a��v                      PIC 9 VALUE ZERO.
003410    03 �v�Z�N�v                        PIC 9(2) VALUE ZERO.
003420    03 �v�Z���v                        PIC 9(2) VALUE ZERO.
003430    03 �v�Z���v                        PIC 9(2) VALUE ZERO.
003440** �}�Ԕ���p
003450 01 �J�n�f�Ó��蓮�敪�v               PIC 9    VALUE ZERO.
003460*
003470* �I�����ޔ�p
003480 01 �I���N�����v�s.
003490    03 �I���a��v�s                    PIC 9     VALUE ZERO.
003500    03 �I���N�v�s                      PIC 9(2)  VALUE ZERO.
003510    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
003520    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
003530* �������ޔ�p
003540 01 �����N�����v�s.
003550    03 �����a��v�s                    PIC 9     VALUE ZERO.
003560    03 �����N�v�s                      PIC 9(2)  VALUE ZERO.
003570    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003580    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003590*
003591* �A�v�̋��z�ޔ�p
003592 01 �A�v���z�v.
003593    03  ��p�z�v                   PIC 9(6) VALUE ZERO.
003594    03  ���S�z�v                   PIC 9(6) VALUE ZERO.
003595    03  �����z�v                   PIC 9(6) VALUE ZERO.
003596    03  ��p�z�V�l�v               PIC 9(6) VALUE ZERO.
003597    03  ���S�z�V�l�v               PIC 9(6) VALUE ZERO.
003598    03  �����z�V�l�v               PIC 9(6) VALUE ZERO.
003599    03  ��p�z�����v               PIC 9(6) VALUE ZERO.
003600    03  ���S�z�����v               PIC 9(5) VALUE ZERO.
003601    03  �����z�����v               PIC 9(5) VALUE ZERO.
003602    03  ���S���v                   PIC 9(3) VALUE ZERO.
003603*
003604* ���������p
003605 01 ���������v�s.
003606    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
003607    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
003608    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
003609    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
003610    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
003611    03 ���������i���o�[�v�s.
003612       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
003613    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
003614 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
003615 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
003616 01 ���������s�a�k.
003617    03 ���������R�[�h�s�a�k            OCCURS 9.
003618       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
003619       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
003620       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
003621 01 �����������e�v.
003622    03 �����������e�����v              PIC X(318) OCCURS 9 VALUE SPACE.
003623    03 �����������e�����w�v.
003624       05 �����������e�P�w�v           PIC X(74)  VALUE SPACE.
003625       05 �����������e�Q�w�v           PIC X(74)  VALUE SPACE.
003626       05 �����������e�R�w�v           PIC X(74)  VALUE SPACE.
003650       05 �����������e�S�w�v           PIC X(96)  VALUE SPACE.
003627*
003628** ���������E�������R����敪�p
003629 01 ������������敪�v                 PIC 9 VALUE ZERO.
003630 01 �������R����敪�v                 PIC 9 VALUE ZERO.
003631*
003632** �������Z�܂Ƃߗp
003633 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
003634*
003635**********************************************************************************
003636*
003637 01 �ޔ����ڂf�v.
003638   03 ���Z�v�g��ނv                   PIC X(4).
003640   03 ���Z�v�g��ނf�v                 PIC X(4).
003650   03 ���Z�v�g��ʂf�v                 PIC 9(2).
003660*
003670****************
003680* �����f�[�^�e *
003690****************
003700 01 �������v.
003710    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
003720    03 ���ʏ��v  OCCURS   9.
003730       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
003740       05 ���ʃR�[�h�v.
003750          07 ������ʂv                PIC 9(2)  VALUE ZERO.
003760          07 ���ʂv                    PIC 9(2)  VALUE ZERO.
003770          07 ���E�敪�v                PIC 9(1)  VALUE ZERO.
003780          07 �����ʒu�ԍ��v            PIC 9(2)  VALUE ZERO.
003790       05 �������v                     PIC N(18) VALUE SPACE.
003800       05 �����N�����v.
003810          07 �����a��v                PIC 9     VALUE ZERO.
003820          07 �����N�v                  PIC 9(2)  VALUE ZERO.
003830          07 �������v                  PIC 9(2)  VALUE ZERO.
003840          07 �������v                  PIC 9(2)  VALUE ZERO.
003850       05 �����N�����v.
003860          07 �����a��v                PIC 9     VALUE ZERO.
003870          07 �����N�v                  PIC 9(2)  VALUE ZERO.
003880          07 �������v                  PIC 9(2)  VALUE ZERO.
003890          07 �������v                  PIC 9(2)  VALUE ZERO.
003900       05 �J�n�N�����v.
003910          07 �J�n�a��v                PIC 9     VALUE ZERO.
003920          07 �J�n�N�v                  PIC 9(2)  VALUE ZERO.
003930          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
003940          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
003950       05 �I���N�����v.
003960          07 �I���a��v                PIC 9     VALUE ZERO.
003970          07 �I���N�v                  PIC 9(2)  VALUE ZERO.
003980          07 �I�����v                  PIC 9(2)  VALUE ZERO.
003990          07 �I�����v                  PIC 9(2)  VALUE ZERO.
004000       05 �������v                     PIC 9(2)  VALUE ZERO.
004010       05 ���񏈒u�񐔂v               PIC 9     VALUE ZERO.
004020       05 �]�A�敪�v                   PIC 9(1)  VALUE ZERO.
004030    03 �V�K�敪�v                      PIC 9(1)  VALUE ZERO.
004040    03 �p���敪�v                      PIC 9(1)  VALUE ZERO.
          03 ���������v OCCURS 27.
004041       05 ���������v�o                 PIC X(74) VALUE SPACE.
004050*
004060*********************************************************************
004070*    ************
004080*    * ������� *
004090*    ************
004100*    �����̗���
004110***********************
004120 01 �����P�v�q.
004130   03 �����v�q.
004140      05 �����񐔂v                 PIC 9(1)    VALUE ZERO.
004150      05 �������ԊO�񐔂v           PIC 9(1)    VALUE ZERO.
004160      05 �����x���񐔂v             PIC 9(1)    VALUE ZERO.
004170      05 �����[��񐔂v             PIC 9(1)    VALUE ZERO.
004180   03 �Č��񐔂v                    PIC 9(1)    VALUE ZERO.
004190   03 ���Âv�q.
004200      05 ���É񐔂v                 PIC 9(2)    VALUE ZERO.
004210      05 ���Ë����v                 PIC 9(3)V9  VALUE ZERO.
004211      05 ���Ë����Q�v               PIC 9(3)    VALUE ZERO.
004220      05 ���Ö�Ԃv                 PIC 9(1)    VALUE ZERO.
004230      05 ���Ó�H�v                 PIC 9(2)    VALUE ZERO.
004240      05 ���Ö\���v                 PIC 9(2)    VALUE ZERO.
004250   03 ��񐔂v                      PIC 9(1)    VALUE ZERO.
004260   03 ���񐔂v                      PIC 9(1)    VALUE ZERO.
004270   03 ���񐔂v                      PIC 9(1)    VALUE ZERO.
004280   03 ���񋟗��񐔂v              PIC 9(1)    VALUE ZERO.
004290   03 �ꕔ���S���v�q                PIC 9(6)    VALUE ZERO.
004300   03 �������z�v�q                  PIC 9(6)    VALUE ZERO.
004310   03 ���t�����v�q                  PIC 9(1)    VALUE ZERO.
004320   03 �󋋎ҕ��S�z�v�q              PIC 9(6)    VALUE ZERO.
004330   03 �����������z�v�q              PIC 9(6)    VALUE ZERO.
      */
         03 ���k�x���񐔂v                PIC 9(1)    VALUE ZERO.
         03 �{�p���s�v.
            05 �{�p���v                   PIC 9(1) OCCURS 31 VALUE ZERO.
         03 �������q�񐔂v                PIC 9(2)    VALUE ZERO.
      */
         03 �������v                      PIC 9(5) VALUE ZERO.
         03 �������Z�v                    PIC 9(5) VALUE ZERO.
         03 ���k�x�����v                  PIC 9(5) VALUE ZERO.
         03 �Č����v                      PIC 9(5) VALUE ZERO.
         03 ���×��v                      PIC 9(5) VALUE ZERO.
         03 ���É��Z�v                    PIC 9(5) VALUE ZERO.
         03 �������q�v                    PIC 9(5) VALUE ZERO.
         03 ���񋟗��v                  PIC 9(5) VALUE ZERO.
      */���׏����s�̐����Z�ǉ�/20221212
         03 ���׏����s�񐔂v              PIC 9(1)    VALUE ZERO.
         03 ���׏����s�v                  PIC 9(3)    VALUE ZERO.
         03 ���׏����s�����v.
            05 ���׏����s���v             PIC 9(2)    VALUE ZERO.
            05 ���׏����s���v             PIC 9(2)    VALUE ZERO.
004340*
004350* �������ʖ��̗���
004360***********************
004370 01 �����Q�v�q.
004380   03 ���񏈒u�v�q    OCCURS   9.
004390      05 ���񏈒u���v�q             PIC 9(5)    VALUE ZERO.
004400   03 �����ʋ敪�v�q  OCCURS   9.
004410      05 �����ʋ敪�v               PIC 9(1)    VALUE ZERO.
004420   03 �����敪�v�q  OCCURS   9.
004430      05 �����敪�v                 PIC 9(1)    VALUE ZERO.
004440*
004450* �������̗���
004460***********************
004470 01 �����R�v�q.
004480**********
004490* �P���� *
004500**********
004510   03 ���ʂP�v�q.
004520      05 ��ÂP�v�q.
004530         07 ��É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
004540      05 ��㪖@�P�v�q.
004550         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
004560      05 ��㪖@�P�v�q.
004570         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
004580      05 �d�ÂP�v�q.
004590         07 �d�É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
004600**********
004610* �Q���� *
004620**********
004630   03 ���ʂQ�v�q.
004640      05 ��ÂQ�v�q.
004650         07 ��É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
004660      05 ��㪖@�Q�v�q.
004670         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
004680      05 ��㪖@�Q�v�q.
004690         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
004700      05 �d�ÂQ�v�q.
004710         07 �d�É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
004720******************
004730* �R���ʁ^�W�� *
004740******************
004750   03 ���ʂR�W�v�q.
004760      05 ��ÂR�W�v�q.
004770         07 ��É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
004780      05 ��㪖@�R�W�v�q.
004790         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
004800      05 ��㪖@�R�W�v�q.
004810         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
004820      05 �d�ÂR�W�v�q.
004830         07 �d�É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
004840******************
004850* �R���ʁ^�P�O�� *
004860******************
004870   03 ���ʂR�O�v�q.
004880      05 ��ÂR�O�v�q.
004890         07 ��É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
004900      05 ��㪖@�R�O�v�q.
004910         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
004920      05 ��㪖@�R�O�v�q.
004930         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
004940      05 �d�ÂR�O�v�q.
004950         07 �d�É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
004960******************
004970* �R���ʁ^���v�@ *
004980******************
004990   03 ���ʂR�v�q.
005000      05 ��ÂR�v�q.
005010         07 ��É񐔂R�v�q                PIC 9(2)  VALUE ZERO.
005020      05 ��㪖@�R�v�q.
005030         07 ��㪖@�񐔂R�v�q              PIC 9(2)  VALUE ZERO.
005040      05 ��㪖@�R�v�q.
005050         07 ��㪖@�񐔂R�v�q              PIC 9(2)  VALUE ZERO.
005060      05 �d�ÂR�v�q.
005070         07 �d�É񐔂R�v�q                PIC 9(2)  VALUE ZERO.
005080****************
005090* �S���ʁ^�T�� *
005100****************
005110   03 ���ʂS�T�v�q.
005120      05 ��ÂS�T�v�q.
005130         07 ��É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
005140      05 ��㪖@�S�T�v�q.
005150         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
005160      05 ��㪖@�S�T�v�q.
005170         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
005180      05 �d�ÂS�T�v�q.
005190         07 �d�É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
005200****************
005210* �S���ʁ^�W�� *
005220****************
005230   03 ���ʂS�W�v�q.
005240      05 ��ÂS�W�v�q.
005250         07 ��É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
005260      05 ��㪖@�S�W�v�q.
005270         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
005280      05 ��㪖@�S�W�v�q.
005290         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
005300      05 �d�ÂS�W�v�q.
005310         07 �d�É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
005320******************
005330* �S���ʁ^�P�O�� *
005340******************
005350   03 ���ʂS�O�v�q.
005360      05 ��ÂS�O�v�q.
005370         07 ��É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
005380      05 ��㪖@�S�O�v�q.
005390         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
005400      05 ��㪖@�S�O�v�q.
005410         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
005420      05 �d�ÂS�O�v�q.
005430         07 �d�É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
005440******************
005450* �S���ʁ^���v�@ *
005460******************
005470   03 ���ʂS�v�q.
005480      05 ��ÂS�v�q.
005490         07 ��É񐔂S�v�q                PIC 9(2)  VALUE ZERO.
005500      05 ��㪖@�S�v�q.
005510         07 ��㪖@�񐔂S�v�q              PIC 9(2)  VALUE ZERO.
005520      05 ��㪖@�S�v�q.
005530         07 ��㪖@�񐔂S�v�q              PIC 9(2)  VALUE ZERO.
005540      05 �d�ÂS�v�q.
005550         07 �d�É񐔂S�v�q                PIC 9(2)  VALUE ZERO.
005560********************
005570* �T���ʁ^�Q�D�T�� *
005580********************
005590   03 ���ʂT�Q�v�q.
005600      05 ��ÂT�Q�v�q.
005610         07 ��É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
005620      05 ��㪖@�T�Q�v�q.
005630         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
005640      05 ��㪖@�T�Q�v�q.
005650         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
005660      05 �d�ÂT�Q�v�q.
005670         07 �d�É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
005680****************
005690* �T���ʁ^�T�� *
005700****************
005710   03 ���ʂT�T�v�q.
005720      05 ��ÂT�T�v�q.
005730         07 ��É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
005740      05 ��㪖@�T�T�v�q.
005750         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
005760      05 ��㪖@�T�T�v�q.
005770         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
005780      05 �d�ÂT�T�v�q.
005790         07 �d�É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
005800****************
005810* �T���ʁ^�W�� *
005820****************
005830   03 ���ʂT�W�v�q.
005840      05 ��ÂT�W�v�q.
005850         07 ��É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
005860      05 ��㪖@�T�W�v�q.
005870         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
005880      05 ��㪖@�T�W�v�q.
005890         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
005900      05 �d�ÂT�W�v�q.
005910         07 �d�É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
005920******************
005930* �T���ʁ^�P�O�� *
005940******************
005950   03 ���ʂT�O�v�q.
005960      05 ��ÂT�O�v�q.
005970         07 ��É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
005980      05 ��㪖@�T�O�v�q.
005990         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
006000      05 ��㪖@�T�O�v�q.
006010         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
006020      05 �d�ÂT�O�v�q.
006030         07 �d�É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
006040******************
006050* �T���ʁ^���v�@ *
006060******************
006070   03 ���ʂT�v�q.
006080      05 ��ÂT�v�q.
006090         07 ��É񐔂T�v�q                PIC 9(2)  VALUE ZERO.
006100      05 ��㪖@�T�v�q.
006110         07 ��㪖@�񐔂T�v�q              PIC 9(2)  VALUE ZERO.
006120      05 ��㪖@�T�v�q.
006130         07 ��㪖@�񐔂T�v�q              PIC 9(2)  VALUE ZERO.
006140      05 �d�ÂT�v�q.
006150         07 �d�É񐔂T�v�q                PIC 9(2)  VALUE ZERO.
006160*
006170*****************************************************************
006180 01 �v�Z�@����N�v                     PIC 9(2).
006190* ���t�v�n�q�j
006200 01 �v�Z�@����.
006210    03 �v�Z�@����N                    PIC 9(4).
006220    03 �v�Z�@�����                  PIC 9(4).
006230 01 �v�Z�@����q REDEFINES �v�Z�@����.
006240    03 �v�Z�@���I                      PIC 9(2).
006250    03 �v�Z�@���t                      PIC 9(6).
006260    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
006270       05 �v�Z�@�N��                   PIC 9(4).
006280       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
006290         07 �v�Z�@�N                   PIC 9(2).
006300         07 �v�Z�@��                   PIC 9(2).
006310       05 �v�Z�@��                     PIC 9(2).
006320*
      * C �A�g�p
       01  �����P�v        PIC X(4096).
       01  �����Q�v        PIC X(512).
       01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
      *
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
006330******************************************************************
006340*                          �A������                              *
006350******************************************************************
006360*
006370********************
006380* ���b�Z�[�W�\���L�[ *
006390********************
006400 01 �A���|�L�[ IS EXTERNAL.
006410    03  �A���|���b�Z�[�W               PIC N(20).
006420*
006430 01 �A���R�|�L�[ IS EXTERNAL.
006440    03  �A���R�|���b�Z�[�W             PIC N(20).
006450    03  �A���R�|���b�Z�[�W�P           PIC X(20).
006460*
006470****************
006480* ��ʓ��͏�� *
006490****************
002278* 01 �A���|��ʏ��x�i�a�T�W�O IS EXTERNAL.
002279*    03 �A���|�����a��N��.
002280*       05 �A���|�����a��               PIC 9.
002281*       05 �A���|�����N��.
002282*         07 �A���|�����N               PIC 9(2).
002283*         07 �A���|������               PIC 9(2).
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
006560*
009851************************
009852* �������R���Z�b�g     *
009853************************
009854 01 �A�����|�L�[ IS EXTERNAL.
009855    03 �A�����|�{�p�N��.
009856       05 �A�����|�{�p�a��               PIC 9.
009857       05 �A�����|�{�p�N                 PIC 9(2).
009858       05 �A�����|�{�p��                 PIC 9(2).
009859    03  �A�����|���҃R�[�h.
009860       05 �A�����|���Ҕԍ�               PIC 9(6).
009861       05 �A�����|�}��                   PIC X.
009862    03 �A�����|������                    PIC 9(2).
009863    03 �A�����|���R��                    PIC N(63) OCCURS 15.
009864*
009865************************
009866* �������Z�܂Ƃ�
009867************************
009868 01 �A���Z�܂Ƃ߁|�L�[ IS EXTERNAL.
009869    03 �A���Z�܂Ƃ߁|�{�p�a��N��.
009870       05 �A���Z�܂Ƃ߁|�{�p�a��               PIC 9.
009871       05 �A���Z�܂Ƃ߁|�{�p�N��.
009872          07 �A���Z�܂Ƃ߁|�{�p�N              PIC 9(2).
009873          07 �A���Z�܂Ƃ߁|�{�p��              PIC 9(2).
009874    03 �A���Z�܂Ƃ߁|���҃R�[�h.
009875       05 �A���Z�܂Ƃ߁|���Ҕԍ�               PIC 9(6).
009876       05 �A���Z�܂Ƃ߁|�}��                   PIC X(1).
009877**-------------------------------------------------------**
009878*   1:�������Z�v�g�Ȃ��̖{�̂܂Ƃ߂̔���
009879*   2:���l�E���p�̎Еۏ������Z���̔���
009880    03 �A���Z�܂Ƃ߁|����敪                  PIC 9.
009881**-------------------------------------------------------**
009882*  / OUT /�@ 0:�ΏۊO�A1:�Ώ�
009883    03 �A���Z�܂Ƃ߁|���茋��                  PIC 9.
009884**
009885*
      * �Í������p
       01 �A�Í������|�Í���� IS EXTERNAL.
          03 �A�Í������|���͏��.
             05 �A�Í������|�L��               PIC X(24).
             05 �A�Í������|�ԍ�               PIC X(30).
             05 �A�Í������|�Í�������.
               07 �A�Í������|�Í����Ҕԍ�     PIC X(6).
               07 �A�Í������|�Í�����L��     PIC X.
               07 �A�Í������|�Í�����ԍ�     PIC X.
               07 �A�Í������|�Í��L��         PIC X(24).
               07 �A�Í������|�Í��ԍ�         PIC X(30).
          03 �A�Í������|�o�͏��.
             05 �A�Í������|���������L��       PIC X(24).
             05 �A�Í������|���������ԍ�       PIC X(30).
      * 
009886******************************************************************
009887*                      PROCEDURE  DIVISION                       *
009888******************************************************************
009890 PROCEDURE               DIVISION.
009900************
009910*           *
009920* ��������   *
009930*           *
009940************
009950     PERFORM ������.
009960     PERFORM ������擾.
009961     PERFORM �{�p�����擾.
009970************
009980*           *
009990* �又��     *
010000*           *
010010************
010020     PERFORM ��ƃt�@�C���P�쐬.
010020     PERFORM ��ƃt�@�C���Q�쐬.
010020     PERFORM ��ƃt�@�C���R�쐬.
010020     PERFORM ��ƃt�@�C���S�쐬.
010030************
010040*           *
010050* �I������   *
010060*           *
010070************
010080     PERFORM �I������.
010090     MOVE ZERO TO PROGRAM-STATUS.
010100     EXIT PROGRAM.
010110*
010120*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
010130*================================================================*
010140 ������ SECTION.
010150*
010160     PERFORM �t�@�C���I�[�v��.
010170* �A�����ڂ̑Ҕ�
010180     MOVE �A���|�����a��  TO �����a��v�q.
010190     MOVE �A���|�����N    TO �����N�v�q.
010200     MOVE �A���|������    TO �������v�q.
010210*
010220     MOVE ZERO            TO �A�Ԃv.
010230*
010240* ������N���̎擾
010250     MOVE ZERO          TO ����N���v  ������N���v.
010260     MOVE �����a��v�q  TO ���|�����敪.
010270     READ �����}�X�^
010280     NOT INVALID KEY
010290         MOVE ���|�J�n����N TO ����N�v
010300     END-READ.
010310*
010320     IF ����N�v = ZERO
010330          MOVE  NC"�����}�X�^�ɊJ�n����N��o�^���ĉ�����" TO �A���|���b�Z�[�W
010340          CALL   "MSG001"
010350          CANCEL "MSG001"
010360          PERFORM �t�@�C����
010370          MOVE 99 TO PROGRAM-STATUS
010380          EXIT PROGRAM
010390     ELSE
010400          COMPUTE ����N�v = ����N�v + �����N�v�q - 1
010410          MOVE �������v�q TO ����v
010420     END-IF.
010430*
010440     MOVE ����N���v   TO  ������N���v.
010450*
010460*================================================================*
010470 �t�@�C���I�[�v�� SECTION.
010480*
010481     OPEN INPUT ������}�X�^.
010482         MOVE NC"������" TO �t�@�C����.
010483         PERFORM �I�[�v���`�F�b�N.
010490     OPEN INPUT �����}�X�^.
010500         MOVE NC"�����}�X�^" TO �t�@�C����.
010510         PERFORM �I�[�v���`�F�b�N.
010520     OPEN INPUT ���̃}�X�^.
010530         MOVE NC"���̃}�X�^" TO �t�@�C����.
010540         PERFORM �I�[�v���`�F�b�N.
010550     OPEN INPUT �{�p�����}�X�^
010560         MOVE NC"�{��" TO �t�@�C����.
010570         PERFORM �I�[�v���`�F�b�N.
010580     OPEN INPUT �{�p�L�^�e.
010590         MOVE NC"�{�p�L�^�e" TO �t�@�C����.
010600         PERFORM �I�[�v���`�F�b�N.
010610     OPEN INPUT ��f�ҏ��e.
010620         MOVE NC"��f�ҏ��e" TO �t�@�C����.
010630         PERFORM �I�[�v���`�F�b�N.
010640     OPEN INPUT �����f�[�^�e.
010650         MOVE NC"�����f�[�^�e" TO �t�@�C����.
010660         PERFORM �I�[�v���`�F�b�N.
010661     OPEN INPUT ���������e.
010662         MOVE NC"��������" TO �t�@�C����.
010663         PERFORM �I�[�v���`�F�b�N.
010670     OPEN INPUT �s�����}�X�^
010680         MOVE NC"�s����" TO �t�@�C����.
010690         PERFORM �I�[�v���`�F�b�N.
006630     OPEN INPUT ���Z�v�g�e.
006640         MOVE NC"���Z" TO �t�@�C����.
006650         PERFORM �I�[�v���`�F�b�N.
003001     OPEN INPUT ��v�f�[�^�e.
003002         MOVE NC"��v" TO �t�@�C����.
003003         PERFORM �I�[�v���`�F�b�N.
013960     OPEN INPUT ���ʃ}�X�^
013970         MOVE NC"����" TO �t�@�C����.
013980         PERFORM �I�[�v���`�F�b�N.
005350     OPEN INPUT �����p���҂e.
005360         MOVE NC"���p" TO �t�@�C����.
005370         PERFORM �I�[�v���`�F�b�N.
011344     OPEN OUTPUT ��ƃt�@�C���P.
011345         MOVE NC"��P" TO �t�@�C����.
011346         PERFORM �I�[�v���`�F�b�N.
010730*
010740*================================================================*
010750 �I�[�v���`�F�b�N SECTION.
010760*
010770     IF ��ԃL�[  NOT =  "00"
010780         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
010790         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
010800         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
010810                                                 UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
010820         ACCEPT  �L�[���� FROM CONS
010830         PERFORM �t�@�C����
010840         MOVE 99 TO PROGRAM-STATUS
010850         EXIT PROGRAM.
010860*================================================================*
010870 �t�@�C���� SECTION.
010880*
010890     CLOSE ������}�X�^ �����}�X�^   ���̃}�X�^  ��f�ҏ��e
010891           �����f�[�^�e   ���������e   �{�p�L�^�e  �{�p�����}�X�^
010901           �s�����}�X�^   ��v�f�[�^�e ���Z�v�g�e  ���ʃ}�X�^  
                 �����p���҂e   ��ƃt�@�C���P.
010910*================================================================*
010920 �I������ SECTION.
010930*
010940     PERFORM �t�@�C����.
010941*================================================================*
010942 �G���[�\���q SECTION.
010943*
010944     DISPLAY NC"�t�@�C���Ǎ��G���[" �t�@�C����     UPON CONS.
010945     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
010946     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
010947     ACCEPT  �L�[���� FROM CONS.
010948     PERFORM �t�@�C����.
010949     MOVE 99 TO PROGRAM-STATUS.
010950     EXIT PROGRAM.
010951*================================================================*
010960 �G���[�\�� SECTION.
010970*
010980     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
010990     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
011000     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
011010     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
011020     ACCEPT  �L�[���� FROM CONS.
011030     PERFORM �t�@�C����.
011040     MOVE 99 TO PROGRAM-STATUS.
011050     EXIT PROGRAM.
011051*================================================================*
011052 ������擾 SECTION.
011053*
011054     MOVE ZEROS TO ���|����敪.
011055     READ ������}�X�^
011056     NOT INVALID KEY
011058         MOVE ���|���Z������������敪 TO ������������敪�v
011059         MOVE ���|���Z�������R����敪 TO �������R����敪�v
011062     END-READ.
011063*
011064*================================================================*
011330 ��ƃt�@�C���P�쐬 SECTION.
011340*
           MOVE 01 TO ��P�|���R�[�h�敪.
           PERFORM �{�p�����擾.
           PERFORM �����擾.
           MOVE �����o�N���v TO ��P�|�����N��.
           MOVE ������N���v TO ��P�|�f�ÔN��.
           MOVE "B"            TO ��P�|���R�[�h�h�c.
           PERFORM ��P�t�@�C������.
012457*
012420*================================================================*
       �����擾 SECTION.
      *
           MOVE ������N�v TO �����o�N�v.
           COMPUTE �����o���v = ��������v + 1
           IF �����o���v > 12
               MOVE 01 TO �����o���v
               COMPUTE �����o�N�v = �����o�N�v + 1
           END-IF.
012420*================================================================*
009510 �{�p�����擾 SECTION.
009520*
009530     MOVE ZERO  TO �{��|�{�p���ԍ�.
009540     READ �{�p�����}�X�^
009550     INVALID KEY
009560          MOVE  NC"�{�p�����}�X�^�ɓo�^��A���s���ĉ�����" TO �A���|���b�Z�[�W
009570          CALL   "MSG001"
009580          CANCEL "MSG001"
009590          PERFORM �t�@�C����
009600          MOVE 99 TO PROGRAM-STATUS
009610          EXIT PROGRAM
008970     NOT INVALID KEY
009060         MOVE �{��|�V�_���t�ԍ�(5:5)  TO ��P�|����ԍ�
009620     END-READ.
009630*
011064*================================================================*
011330 ��ƃt�@�C���Q�쐬 SECTION.
011340*
026580     OPEN OUTPUT ��ƃt�@�C���Q.
026590         MOVE NC"��Q" TO �t�@�C����.
026600         PERFORM �I�[�v���`�F�b�N.
           CLOSE ��ƃt�@�C���Q.
026580     OPEN I-O ��ƃt�@�C���Q.
026590         MOVE NC"��Q" TO �t�@�C����.
026600         PERFORM �I�[�v���`�F�b�N.
026710*
005420     MOVE �����a��v�q  TO ���Z�|�����a��.
005430     MOVE �����N�v�q    TO ���Z�|�����N.
005440     MOVE �������v�q    TO ���Z�|������.
005450     MOVE 1             TO ���Z�|�{�p�a��.
005460     MOVE ZERO          TO ���Z�|�{�p�N.
005470     MOVE ZERO          TO ���Z�|�{�p��.
005480     MOVE ZERO          TO ���Z�|���Ҕԍ�.
005490     MOVE LOW-VALUE     TO ���Z�|�}��.
005500     MOVE ZERO          TO ���Z�|���Z���.
005510     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
005520                                  ���Z�|�{�p�a��N��
005530                                  ���Z�|���҃R�[�h
005540                                  ���Z�|���Z���
026830     END-START.
026840     IF ��ԃL�[ = "00"
026850         MOVE SPACE  TO �I���t���O
005580         PERFORM ���Z�v�g�e�Ǎ�
005590         PERFORM UNTIL ( �I���t���O = "YES" ) OR
005600                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
005610                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
005620                       ( ���Z�|������   NOT = �������v�q   )
026910*
026920            PERFORM �f�[�^�`�F�b�N
000500*   / 1:���,2:�V�l,3:����,4:�J��,5:������,6:����,7:���ےP��,8�ی��ؖY�� /
      */��o�e����J�Ў����ӎ���ۖY��𔲂�/20180809
      *            IF ���Z�|���Z��� = 3
                  IF ���Z�|���Z��� = 3 OR 4 OR 5 OR 6 OR 7 OR 8
                      MOVE SPACE TO ���s�L�[�v
                  END-IF
026930            IF ���s�L�[�v = "YES"
                      PERFORM �ی��ҋ敪�擾
                      READ ��ƃt�@�C���Q
                      INVALID KEY
                          INITIALIZE ��Q�|���R�[�h
                          PERFORM �ی��ҋ敪�擾
                          PERFORM ��Q���R�[�h�Z�b�g
                          PERFORM ��Q�t�@�C������
                      NOT INVALID KEY
                          PERFORM ��Q���R�[�h�Z�b�g
                          PERFORM ��Q�t�@�C���ǉ�
                      END-READ
005200            END-IF
027850            PERFORM ���Z�v�g�e�Ǎ�
027860         END-PERFORM
027870     END-IF.
027880     CLOSE ��ƃt�@�C���Q.
012420*================================================================*
       �ی��ҋ敪�擾 SECTION.
      *
           EVALUATE ��|�ی����
           WHEN 01
               IF ��|�ی��Ҕԍ�(1:2) = "23"
                   MOVE 06 TO ��Q�|�ی��ҋ敪
               ELSE
                   MOVE 05 TO ��Q�|�ی��ҋ敪
               END-IF
      */�S���y�،��݁E�S�������^�C���h���ƁA�S�����Ƃ͑g��
               IF ��|�ی��Ҕԍ�(1:6) = "133033" OR "133231" OR "133280"
008480             MOVE  02 TO ��Q�|�ی��ҋ敪
               END-IF
           WHEN 08
               IF ��|�ی��Ҕԍ�(3:2) = "23"
                   MOVE 08 TO ��Q�|�ی��ҋ敪
               ELSE
                   MOVE 07 TO ��Q�|�ی��ҋ敪
               END-IF
           WHEN 02
               MOVE 01 TO ��Q�|�ی��ҋ敪
           WHEN 03
               MOVE 02 TO ��Q�|�ی��ҋ敪
           WHEN 04
               MOVE 03 TO ��Q�|�ی��ҋ敪
           WHEN 05
               IF ��|�ی��Ҕԍ�(3:2) = "23"
                   MOVE 16 TO ��Q�|�ی��ҋ敪
               ELSE
                   MOVE 15 TO ��Q�|�ی��ҋ敪
               END-IF
           WHEN 70
               MOVE 13 TO ��Q�|�ی��ҋ敪
           END-EVALUATE.
012420*================================================================*
       ��Q���R�[�h�Z�b�g SECTION.
      *
           MOVE 10 TO ��Q�|���R�[�h�敪.
011490     EVALUATE ���Z�|�����敪
           WHEN 2
               COMPUTE ��Q�|�Ԗߌ��� = ��Q�|�Ԗߌ��� + 1
               COMPUTE ��Q�|�Ԗߋ��z = ��Q�|�Ԗߋ��z + ���Z�|���v
           WHEN OTHER
               COMPUTE ��Q�|�{�p���� = ��Q�|�{�p���� + 1
               COMPUTE ��Q�|�{�p���� = ��Q�|�{�p���� + ���Z�|���v
           END-EVALUATE.
019390*================================================================*
019320 ��Q�t�@�C������ SECTION.
019330*
019340     WRITE ��Q�|���R�[�h
019350     INVALID KEY
019360         MOVE NC"��Q"  TO �t�@�C����
019370         PERFORM �G���[�\��
019380     END-WRITE.
019390*================================================================*
019320 ��Q�t�@�C���ǉ� SECTION.
019330*
019340     REWRITE ��Q�|���R�[�h
019350     INVALID KEY
019360         MOVE NC"��Q"  TO �t�@�C����
019370         PERFORM �G���[�\��
019380     END-REWRITE.
012420*================================================================*
008560 �f�[�^�`�F�b�N SECTION.
008570*
008580     MOVE SPACE          TO ���s�L�[�v.
019520* *****************************************************************
019530* * ���Z�v�g�e�̐����Ώۋ敪 = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
019540* *****************************************************************
019640     IF ( ���Z�|�����Ώۋ敪 NOT = ZERO ) AND
005778        ( ���Z�|���ҕ����敪 NOT = 1 )
              IF(���Z�|���Z��� = 3) AND ( ���Z�|����\����Ώۋ敪 = 1 )
                 CONTINUE
              ELSE
004090           MOVE ���Z�|�{�p�a��  TO ��|�{�p�a��
004100           MOVE ���Z�|�{�p�N    TO ��|�{�p�N
004110           MOVE ���Z�|�{�p��    TO ��|�{�p��
004120           MOVE ���Z�|���Ҕԍ�  TO ��|���Ҕԍ�
004130           MOVE ���Z�|�}��      TO ��|�}��
                 READ ��f�ҏ��e
                 NOT INVALID KEY
019880               MOVE "YES"  TO ���s�L�[�v
019950           END-READ
              END-IF
019960     END-IF.
009040*
020480*================================================================*
020490 �������擾 SECTION.
020500*
020510***********************************************
020520* �����f�[�^�Z�b�g                            *
020530***********************************************
020540*    ****************************************************************
020550*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
020560*    ****************************************************************
020570     INITIALIZE �����P�v�q.
020580     INITIALIZE �����Q�v�q.
020590     INITIALIZE �����R�v�q.
020600*
025620     MOVE ���Z�|������                 TO �������v.
025730     MOVE ���Z�|�������Z��             TO �������Z�v.
025770     MOVE ���Z�|���������k��           TO  ���k�x�����v.
025740     MOVE ���Z�|�Č���                 TO  �Č����v.
028240     MOVE ���Z�|�������q���Z��         TO  �������q�v.
025990     MOVE ���Z�|�{�p���񋟗�         TO  ���񋟗��v.
      */���׏����s�̐����Z�ǉ�/20221212
           MOVE ���Z�|���׏����s���Z��       TO ���׏����s�v.
           MOVE ���Z�|���׏����s���Z��       TO ���׏����s���v.
           IF ���Z�|���׏����s���Z�� NOT = ZERO
               MOVE 1                        TO ���׏����s�񐔂v
               MOVE ���Z�|�{�p��             TO ���׏����s���v
           END-IF.
      *
020620     MOVE ���Z�|���É�               TO  ���É񐔂v.
020621     MOVE ���Z�|���Ë���               TO  ���Ë����v.
023360     MOVE ���Z�|���×�                 TO  ���×��v.
020622* �P��100m
020623     COMPUTE  ���Ë����Q�v  =  ���Ë����v * 10.
025780     MOVE ���Z�|���É��Z��             TO  ���É��Z�v.
020630*
020640********************
020650* �����������Z�b�g *
020660********************
020670*    **********
020680*    * �P���� *
020690*    **********
020700     MOVE ���Z�|��É񐔂P             TO ��É񐔂P�v�q.
020710     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
020720     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
020730     MOVE ���Z�|�d�É񐔂P             TO �d�É񐔂P�v�q.
020740     MOVE 0          TO �����ʋ敪�v(1)
020750     IF ���Z�|�����������P NOT = ZERO
020760         MOVE 1      TO �����敪�v(1)
020770     ELSE
020780         MOVE 0      TO �����敪�v(1)
020790     END-IF
020800*    **********
020810*    * �Q���� *
020820*    **********
020830     MOVE ���Z�|��É񐔂Q             TO ��É񐔂Q�v�q.
020840     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
020850     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
020860     MOVE ���Z�|�d�É񐔂Q             TO �d�É񐔂Q�v�q.
020870     MOVE 0          TO �����ʋ敪�v(2)
020880     IF ���Z�|�����������Q NOT = ZERO
020890         MOVE 1      TO �����敪�v(2)
020900     ELSE
020910         MOVE 0      TO �����敪�v(2)
020920     END-IF
020930*    ****************
020940*    * �R���ʁ^�W�� *
020950*    ****************
020960     MOVE 0                              TO �����敪�v(3)
020970     MOVE 0                              TO �����敪�v(3)
020980     MOVE ���Z�|��É񐔂R�W             TO ��É񐔂R�W�v�q.
020990     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
021000     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
021010     MOVE ���Z�|�d�É񐔂R�W             TO �d�É񐔂R�W�v�q.
021020     IF ���Z�|���v�R�W NOT = ���Z�|�����ʍ����v�R�W
021030         MOVE 1        TO �����ʋ敪�v(3)
021040     END-IF
021050     IF ���Z�|�����������R�W NOT = ZERO
021060         MOVE 1        TO �����敪�v(3)
021070     END-IF
021080*    ****************
021090*    * �R���ʁ^10�� *
021100*    ****************
021110     MOVE ���Z�|��É񐔂R�O             TO ��É񐔂R�O�v�q.
021120     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
021130     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
021140     MOVE ���Z�|�d�É񐔂R�O             TO �d�É񐔂R�O�v�q.
021150     IF ���Z�|�����������R�O NOT = ZERO
021160         MOVE 1        TO �����敪�v(3)
021170     END-IF
021180*    ****************
021190*    * �R���ʁ^���v *
021200*    ****************
021210     COMPUTE ��É񐔂R�v�q      = ��É񐔂R�W�v�q   + ��É񐔂R�O�v�q.
021220     COMPUTE ��㪖@�񐔂R�v�q    = ��㪖@�񐔂R�W�v�q + ��㪖@�񐔂R�O�v�q.
021230     COMPUTE ��㪖@�񐔂R�v�q    = ��㪖@�񐔂R�W�v�q + ��㪖@�񐔂R�O�v�q.
021240     COMPUTE �d�É񐔂R�v�q      = �d�É񐔂R�W�v�q   + �d�É񐔂R�O�v�q.
021250*    ****************
021260*    * �S���ʁ^�T�� *
021270*    ****************
021280     MOVE 0                              TO �����敪�v(4)
021290     MOVE 0                              TO �����敪�v(4)
021300     MOVE ���Z�|��É񐔂S�T             TO ��É񐔂S�T�v�q.
021310     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
021320     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
021330     MOVE ���Z�|�d�É񐔂S�T             TO �d�É񐔂S�T�v�q.
021340     IF ���Z�|���v�S�T NOT = ���Z�|�����ʍ����v�S�T
021350         MOVE 1        TO �����ʋ敪�v(4)
021360     END-IF
021370     IF ���Z�|�����������S�T NOT = ZERO
021380         MOVE 1        TO �����敪�v(4)
021390     END-IF
021400*    ****************
021410*    * �S���ʁ^�W�� *
021420*    ****************
021430     MOVE ���Z�|��É񐔂S�W             TO ��É񐔂S�W�v�q.
021440     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
021450     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
021460     MOVE ���Z�|�d�É񐔂S�W             TO �d�É񐔂S�W�v�q.
021470     IF ���Z�|���v�S�W NOT = ���Z�|�����ʍ����v�S�W
021480         MOVE 1        TO �����ʋ敪�v(4)
021490     END-IF
021500     IF ���Z�|�����������S�W NOT = ZERO
021510         MOVE 1        TO �����敪�v(4)
021520     END-IF
021530*    ****************
021540*    * �S���ʁ^10�� *
021550*    ****************
021560     MOVE ���Z�|��É񐔂S�O             TO ��É񐔂S�O�v�q.
021570     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
021580     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
021590     MOVE ���Z�|�d�É񐔂S�O             TO �d�É񐔂S�O�v�q.
021600     IF ���Z�|�����������S�O NOT = ZERO
021610         MOVE 1        TO �����敪�v(4)
021620     END-IF
021630*    ****************
021640*    * �S���ʁ^���v *
021650*    ****************
021660     COMPUTE ��É񐔂S�v�q      = ��É񐔂S�T�v�q   + ��É񐔂S�W�v�q   + ��É񐔂S�O�v�q.
021670     COMPUTE ��㪖@�񐔂S�v�q    = ��㪖@�񐔂S�T�v�q + ��㪖@�񐔂S�W�v�q + ��㪖@�񐔂S�O�v�q.
021680     COMPUTE ��㪖@�񐔂S�v�q    = ��㪖@�񐔂S�T�v�q + ��㪖@�񐔂S�W�v�q + ��㪖@�񐔂S�O�v�q.
021690     COMPUTE �d�É񐔂S�v�q      = �d�É񐔂S�T�v�q   + �d�É񐔂S�W�v�q   + �d�É񐔂S�O�v�q.
021700*    *****************
021710*    * �T���ʁ^2.5�� *
021720*    *****************
021730     MOVE 0                              TO �����敪�v(5)
021740     MOVE 0                              TO �����敪�v(5)
021750     MOVE ���Z�|��É񐔂T�Q             TO ��É񐔂T�Q�v�q.
021760     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
021770     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
021780     MOVE ���Z�|�d�É񐔂T�Q             TO �d�É񐔂T�Q�v�q.
021790     IF ���Z�|���v�T�Q NOT = ���Z�|�����ʍ����v�T�Q
021800         MOVE 1        TO �����ʋ敪�v(5)
021810     END-IF
021820     IF ���Z�|�����������T�Q NOT = ZERO
021830         MOVE 1        TO �����敪�v(5)
021840     END-IF
021850*    ****************
021860*    * �T���ʁ^�T�� *
021870*    ****************
021880     MOVE ���Z�|��É񐔂T�T             TO ��É񐔂T�T�v�q.
021890     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
021900     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
021910     MOVE ���Z�|�d�É񐔂T�T             TO �d�É񐔂T�T�v�q.
021920     IF ���Z�|���v�T�T NOT = ���Z�|�����ʍ����v�T�T
021930         MOVE 1        TO �����ʋ敪�v(5)
021940     END-IF
021950     IF ���Z�|�����������T�T NOT = ZERO
021960         MOVE 1        TO �����敪�v(5)
021970     END-IF
021980*    ****************
021990*    * �T���ʁ^�W�� *
022000*    ****************
022010     MOVE ���Z�|��É񐔂T�W             TO ��É񐔂T�W�v�q.
022020     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
022030     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
022040     MOVE ���Z�|�d�É񐔂T�W             TO �d�É񐔂T�W�v�q.
022050     IF ���Z�|���v�T�W NOT = ���Z�|�����ʍ����v�T�W
022060         MOVE 1        TO �����ʋ敪�v(5)
022070     END-IF
022080     IF ���Z�|�����������T�W NOT = ZERO
022090         MOVE 1        TO �����敪�v(5)
022100     END-IF
022110*    ****************
022120*    * �T���ʁ^10�� *
022130*    ****************
022140     MOVE ���Z�|��É񐔂T�O             TO ��É񐔂T�O�v�q.
022150     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
022160     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
022170     MOVE ���Z�|�d�É񐔂T�O             TO �d�É񐔂T�O�v�q.
022180     IF ���Z�|�����������T�O NOT = ZERO
022190         MOVE 1        TO �����敪�v(5)
022200     END-IF
022210*    ****************
022220*    * �T���ʁ^���v *
022230*    ****************
022240     COMPUTE ��É񐔂T�v�q   = ��É񐔂T�Q�v�q   + ��É񐔂T�T�v�q   +
022250                                ��É񐔂T�W�v�q   + ��É񐔂T�O�v�q.
022260     COMPUTE ��㪖@�񐔂T�v�q = ��㪖@�񐔂T�Q�v�q + ��㪖@�񐔂T�T�v�q +
022270                                ��㪖@�񐔂T�W�v�q + ��㪖@�񐔂T�O�v�q.
022280     COMPUTE ��㪖@�񐔂T�v�q = ��㪖@�񐔂T�Q�v�q + ��㪖@�񐔂T�T�v�q +
022290                                ��㪖@�񐔂T�W�v�q + ��㪖@�񐔂T�O�v�q.
022300     COMPUTE �d�É񐔂T�v�q   = �d�É񐔂T�Q�v�q   + �d�É񐔂T�T�v�q   +
022310                                �d�É񐔂T�W�v�q   + �d�É񐔂T�O�v�q.
022320*
012900*================================================================*
022340 �����f�[�^�擾 SECTION.
022350*
022360     INITIALIZE �������v.
022361*
022370     MOVE �{�p�a��v�q       TO ���|�{�p�a��.
022380     MOVE �{�p�N�v�q         TO ���|�{�p�N.
022390     MOVE �{�p���v�q         TO ���|�{�p��.
022400     MOVE ���҃R�[�h�v�q     TO ���|���҃R�[�h.
022410     READ �����f�[�^�e
022420     INVALID KEY
022430         CONTINUE
022440     NOT INVALID KEY
022450         MOVE ���|���ʐ�                   TO ���ʐ��v
022460         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
022470                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
022480             MOVE ���|�������(���ʂb�m�s) TO ������ʂv(���ʂb�m�s)
022490             MOVE ���|����(���ʂb�m�s)     TO ���ʂv(���ʂb�m�s)
022500             MOVE ���|���E�敪(���ʂb�m�s) TO ���E�敪�v(���ʂb�m�s)
022510             MOVE ���|�����ʒu�ԍ�(���ʂb�m�s)
022520                                           TO �����ʒu�ԍ��v(���ʂb�m�s)
022530* �������
022540             MOVE SPACE                     TO �������̂v
022550             MOVE 03                        TO ���|�敪�R�[�h
022560             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
022570             READ ���̃}�X�^
022580             INVALID KEY
022590                 MOVE SPACE        TO �������̂v
022600             NOT INVALID KEY
022610                 MOVE ���|�������� TO �������̂v
022620             END-READ
022630* ����
022720             STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
022730                    �������̂v                    DELIMITED BY SPACE
022740                    ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
022750                    INTO �������v(���ʂb�m�s)
022760             END-STRING
022780*
022790             MOVE ���|�����a��(���ʂb�m�s)   TO �����a��v(���ʂb�m�s)
022800             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
022810             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
022820             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
022830             MOVE ���|�J�n�a��(���ʂb�m�s)   TO �����a��v(���ʂb�m�s)
022840             MOVE ���|�J�n�N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
022850             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
022860             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
022870             IF ���|�]�A�敪(���ʂb�m�s) = 9
022880                 MOVE 99                   TO �I���N�v(���ʂb�m�s)
022890                 MOVE 99                   TO �I�����v(���ʂb�m�s)
022900                 MOVE 99                   TO �I�����v(���ʂb�m�s)
022910             ELSE
022920                 MOVE ���|�I���a��(���ʂb�m�s)   TO �I���a��v(���ʂb�m�s)
022930                 MOVE ���|�I���N(���ʂb�m�s)   TO �I���N�v(���ʂb�m�s)
022940                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
022950                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
022960             END-IF
022970*
022980             MOVE ���|�]�A�敪(���ʂb�m�s) TO �]�A�敪�v(���ʂb�m�s)
022990*
023000         END-PERFORM
023010* �V�K/�p�� �`�F�b�N
023020         IF ���Z�|������  NOT = ZERO
023030             MOVE 1                   TO �V�K�敪�v
023040         ELSE
023050             MOVE 1                   TO �p���敪�v
023060         END-IF
023070         PERFORM �������ȑO�̃f�[�^����
023080* �}�Ԕ���p
023090         MOVE ���|�J�n�f�Ó��蓮�敪 TO  �J�n�f�Ó��蓮�敪�v
023100*
023110     END-READ.
023120*
023130*================================================================*
023140 �������ȑO�̃f�[�^���� SECTION.
023150*
023160*********************************************************************************
023170*  �ŏ��̏������ȑO�̓������Ɏ{�p�L�^���R�[�h����������(�����A���~)�́A�����敪��
023180*  �p���ɂ��`�F�b�N����B(�V�K�ƌp���̗���)
023190*********************************************************************************
023200** �ŏ��̏��������擾
023210     MOVE SPACE                 TO �����t���O.
023220     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
023230     MOVE �}�Ԃv�q              TO �{�L�|�}��.
023240     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
023250     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
023260     MOVE �{�p���v�q            TO �{�L�|�{�p��.
023270     MOVE ZERO                  TO �{�L�|�{�p��.
023280     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
023290                                  �{�L�|�{�p�a��N����
023300     END-START.
023310     IF ��ԃL�[ = "00"
023320         MOVE ZERO  TO �����a��v�s
023330         MOVE ZERO  TO �����N�v�s
023340         MOVE ZERO  TO �������v�s
023350         MOVE ZERO  TO �������v�s
023360         MOVE SPACE TO �I���t���O�Q
023370         PERFORM �{�p�L�^�e�Ǎ�
023380         PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
023390                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
023400                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
023410                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
023420                       ( �{�L�|�{�p��     NOT = �{�p���v�q      ) OR
023430                       ( �����t���O           = "YES"           ) 
023440               IF  �{�L�|�f�Ë敪 = 2
023450                   MOVE �{�L�|�{�p�a��           TO �����a��v�s
023460                   MOVE �{�L�|�{�p�N             TO �����N�v�s
023470                   MOVE �{�L�|�{�p��             TO �������v�s
023480                   MOVE �{�L�|�{�p��             TO �������v�s
023490                   MOVE "YES"                    TO �����t���O
023500               END-IF
023510               PERFORM �{�p�L�^�e�Ǎ�
023520         END-PERFORM
023530     END-IF.
023540*
023550* �������ȑO�̃f�[�^����
023560     IF �����t���O = "YES"
023570        MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
023580        MOVE �}�Ԃv�q              TO �{�L�|�}��
023590        MOVE �����a��v�s          TO �{�L�|�{�p�a��
023600        MOVE �����N�v�s            TO �{�L�|�{�p�N
023610        MOVE �������v�s            TO �{�L�|�{�p��
023620        MOVE �������v�s            TO �{�L�|�{�p��
023630        START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
023640                                     �{�L�|�{�p�a��N����
023650                                     REVERSED
023660        END-START
023670        IF ��ԃL�[ = "00"
023680           MOVE SPACE  TO �I���t���O�Q
023690           PERFORM �{�p�L�^�e�Ǎ�
023700           IF ( �I���t���O�Q    = SPACE        ) AND
023710              ( �{�L�|���Ҕԍ�  = ���Ҕԍ��v�q ) AND
023720              ( �{�L�|�}��      = �}�Ԃv�q     ) AND
023730              ( �{�L�|�{�p�a��  = �����a��v�s ) AND
023740              ( �{�L�|�{�p�N    = �����N�v�s   ) AND
023750              ( �{�L�|�{�p��    = �������v�s   )
023760*  �������ȑO�̓������Ɏ{�p�L�^���R�[�h����������
023770                IF �p���敪�v = ZERO
023780                   MOVE 1    TO �p���敪�v
023790                END-IF
023800           END-IF
023810         END-IF
023820     END-IF.
023830*
023840*================================================================*
023850 �{�p�L�^�擾 SECTION.
023860*
023870     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���ʐ��v
023880         IF ( �{�p�N�v�q = �����N�v(���ʂb�m�s) ) AND
023890            ( �{�p���v�q = �������v(���ʂb�m�s) )
023900             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
023910             MOVE �}�Ԃv�q              TO �{�L�|�}��
023920             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
023930             MOVE �����a��v(���ʂb�m�s)  TO �J�n�a��v(���ʂb�m�s)
023940             MOVE �����N�v(���ʂb�m�s)  TO �J�n�N�v(���ʂb�m�s) �{�L�|�{�p�N
023950             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
023960             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
023970         ELSE
023980             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
023990             MOVE �}�Ԃv�q              TO �{�L�|�}��
024000             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
024010             MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
024020             MOVE �{�p���v�q            TO �{�L�|�{�p��
024030             MOVE ZERO                  TO �{�L�|�{�p��
024040         END-IF
024050         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
024060                                      �{�L�|�{�p�a��N����
024070         END-START
024080         IF ��ԃL�[ = "00"
024090             MOVE ZERO  TO �������v(���ʂb�m�s)
024100             MOVE ZERO  TO ���񏈒u�񐔂v(���ʂb�m�s)
024110             MOVE ZERO  TO �I���a��v�s
024120             MOVE ZERO  TO �I���N�v�s
024130             MOVE ZERO  TO �I�����v�s
024140             MOVE ZERO  TO �I�����v�s
024150             MOVE SPACE TO �I���t���O�Q
024160             PERFORM �{�p�L�^�e�Ǎ�
024170             IF  ( �I���t���O�Q      = SPACE   ) AND
024180                 ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
024190                 ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
024200                 ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
024210                 ( �{�L�|�{�p��      = �{�p���v�q     ) 
024220*
024230*        ************
024240*        * �������q *
024250*        ************
024260             EVALUATE �{�L�|�������q�敪(���ʂb�m�s)
024270             WHEN 1
024280                 COMPUTE ��񐔂v = ��񐔂v + 1
024290             WHEN 2
024300                 COMPUTE ���񐔂v = ���񐔂v + 1
024310             WHEN 3
024320                 COMPUTE ���񐔂v = ���񐔂v + 1
024330             END-EVALUATE
024340*        ****************
024350*        * ���񋟉� *
024360*        ****************
024370             IF �{�L�|���񋟋敪(���ʂb�m�s) = 1
024380                 COMPUTE ���񋟗��񐔂v = ���񋟗��񐔂v + 1
024390             END-IF
024400*        *****************************************************************
024410*        * �J�n�N���� ( ���̕��ʂ����������łȂ����A
024420*                       ���������ł��}�Ԃ����鎞�́A�ŏ��̎{�p�����J�n��)*
024430*        *****************************************************************
024440                 IF ( �{�p�N�v�q NOT = �����N�v(���ʂb�m�s) ) OR
024450                    ( �{�p���v�q NOT = �������v(���ʂb�m�s) ) OR
024460                    ( �J�n�f�Ó��蓮�敪�v = 1 )
024470                     MOVE �{�L�|�{�p�a�� TO �J�n�a��v(���ʂb�m�s)
024480                     MOVE �{�L�|�{�p�N   TO �J�n�N�v(���ʂb�m�s)
024490                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
024500                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
024510                 END-IF
024520             END-IF
024530             PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
024540                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
024550                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
024560                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
024570                           ( �{�L�|�{�p��     NOT = �{�p���v�q       ) OR
024580                           ( �{�L�|�{�p��         > �I�����v(���ʂb�m�s))
024590*               **********
024600*               * ������ *
024610*               **********
024620                COMPUTE �������v(���ʂb�m�s) = �������v(���ʂb�m�s) + 1
024630                MOVE �{�L�|�{�p�a��             TO �I���a��v�s
024640                MOVE �{�L�|�{�p�N               TO �I���N�v�s
024650                MOVE �{�L�|�{�p��               TO �I�����v�s
024660                MOVE �{�L�|�{�p��               TO �I�����v�s
024670*            /�@���񏈒u�̃J�E���g�@/
024680                IF �{�L�|�����{�Ë敪(���ʂb�m�s) = 1
024690                    COMPUTE ���񏈒u�񐔂v(���ʂb�m�s) = ���񏈒u�񐔂v(���ʂb�m�s) + 1
024700                END-IF
024710*
024720                PERFORM �{�p�L�^�e�Ǎ�
024730            END-PERFORM
024740        END-IF
024750*       **************************
024760*       * �p���F�I���N�����Z�b�g *
024770*       **************************
024780        IF �]�A�敪�v(���ʂb�m�s) = 9
024790            MOVE �I���a��v�s  TO �I���a��v(���ʂb�m�s)
024800            MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
024810            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
024820            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
024830        END-IF
024840     END-PERFORM.
024841***
024850     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
024860     MOVE �}�Ԃv�q              TO �{�L�|�}��.
024870     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
024880     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
024890     MOVE �{�p���v�q            TO �{�L�|�{�p��.
024900     MOVE ZERO                  TO �{�L�|�{�p��.
024910     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
024920                                  �{�L�|�{�p�a��N����
024930     END-START.
024940     IF ��ԃL�[ = "00"
024150         MOVE SPACE TO �I���t���O�Q
024950         PERFORM �{�p�L�^�e�Ǎ�
024960         PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
024970                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
024980                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
024990                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
025000                       ( �{�L�|�{�p��     NOT = �{�p���v�q       )
025010*        ************
025020*        * ������ *
025030*        ************
025040             IF �{�L�|�����������敪 = 1
025050                 COMPUTE �����񐔂v = �����񐔂v + 1
025060             END-IF
025070*        ************
025080*        * �������Z *
025090*        ************
025100             EVALUATE �{�L�|�������Z
025110             WHEN 1
025120                 COMPUTE �������ԊO�񐔂v = �������ԊO�񐔂v + 1
025130             WHEN 2
025140                 COMPUTE �����x���񐔂v   = �����x���񐔂v + 1
025150             WHEN 3
025160                 COMPUTE �����[��񐔂v   = �����[��񐔂v + 1
025170             END-EVALUATE
025180*        ************
025190*        * �Č��� *
025200*        ************
025210             IF �{�L�|�Č������� = 1
025220                 COMPUTE �Č��񐔂v = �Č��񐔂v + 1
025230             END-IF
025240*        ************
025250*        * ���É��Z *
025260*        ************
025270             EVALUATE �{�L�|���É��Z
025280             WHEN 1
025290                 COMPUTE ���Ö�Ԃv = ���Ö�Ԃv + 1
025300             WHEN 2
025310                 COMPUTE ���Ó�H�v = ���Ó�H�v + 1
025320             WHEN 3
025330                 COMPUTE ���Ö\���v = ���Ö\���v + 1
025340             END-EVALUATE
025240*        ****************
025250*        * ���������k�� *
025260*        ****************
                   IF (�{�L�|�f�Ë敪 = 2 ) AND (�{�L�|���������k���敪 NOT = 1)
                       COMPUTE ���k�x���񐔂v = ���k�x���񐔂v + 1
                   END-IF
025240*        **********
025250*        * �{�p�� *
025260*        **********
                   MOVE 1 TO �{�p���v(�{�L�|�{�p��)
      *
025350             PERFORM �{�p�L�^�e�Ǎ�
025360         END-PERFORM
025370     END-IF.
025380*

027550*================================================================*
026930 ������ʕϊ� SECTION.
026940*
026950     MOVE ZERO  TO ������ʕϊ���v.
026960*
026970     EVALUATE ������ʕϊ��O�v
026980     WHEN  ZERO
026990        MOVE ZERO TO ������ʕϊ���v
027000* �P��
027010     WHEN  01
027020        MOVE  4   TO ������ʕϊ���v
027030* �Ŗo
027040     WHEN  02
027050        MOVE  5   TO ������ʕϊ���v
027060* ����
027070     WHEN  03
027080        MOVE  6   TO ������ʕϊ���v
027090* �E�P
027100     WHEN  04
027110        MOVE  3   TO ������ʕϊ���v
027120* ����
027130     WHEN  05
027140        MOVE  1   TO ������ʕϊ���v
027150* �s�S����
027160     WHEN  06
027170        MOVE  2   TO ������ʕϊ���v
027180* ���܁E�s�S���܍S�k
027190     WHEN  07
027200     WHEN  08
027210        MOVE  7   TO ������ʕϊ���v
027220* �������Ȃ��i���a�j
027230     WHEN  09
027240        MOVE  9   TO ������ʕϊ���v
027250     WHEN OTHER
027260        CONTINUE
027270     END-EVALUATE.
027280*
027290*================================================================*
027300 �]�A�敪�ϊ� SECTION.
027310*
027320     MOVE ZERO  TO �]�A�ϊ���v.
027330*
027340     EVALUATE �]�A�ϊ��O�v
027350     WHEN  ZERO
027360        MOVE ZERO TO �]�A�ϊ���v
027370* ����
027380     WHEN  1
027390     WHEN  2
027400        MOVE  1   TO �]�A�ϊ���v
027410* ���~
027420     WHEN  3
027430        MOVE  2   TO �]�A�ϊ���v
027440* �]��
027450     WHEN  4
027460        MOVE  3   TO �]�A�ϊ���v
027470* �p���E���R����
027480     WHEN  5
027490     WHEN  9
027500        MOVE  0   TO �]�A�ϊ���v
027510     WHEN OTHER
027520        CONTINUE
027530     END-EVALUATE.
027540*
025390*================================================================*
012910 ���Z�v�g�e�Ǎ� SECTION.
012920*
012930     READ ���Z�v�g�e NEXT
012940     AT END
012950         MOVE "YES" TO �I���t���O
012960     END-READ.
012970*
012980*================================================================*
012990 �{�p�L�^�e�Ǎ� SECTION.
013000*
013010     READ �{�p�L�^�e NEXT
013020     AT END
013030         MOVE "YES"  TO �I���t���O�Q
013040     END-READ.
013050*================================================================*
019320 ��P�t�@�C������ SECTION.
019330*
019340     WRITE ��P�|���R�[�h
019350     INVALID KEY
019360         MOVE NC"��P"  TO �t�@�C����
019370         PERFORM �G���[�\��
019380     END-WRITE.
019390*================================================================*
025390*================================================================*
025820 ����{�p�N���擾 SECTION.
025830* 
025840     MOVE ZERO          TO ����N���v  ����{�p�N���v.
025850     MOVE ��|�{�p�a��  TO ���|�����敪.
025860     READ �����}�X�^
025870     NOT INVALID KEY
025880         MOVE ���|�J�n����N TO ����N�v
025890     END-READ.
025900**
025910     IF ����N�v = ZERO
025920          MOVE  NC"�����}�X�^�ɊJ�n����N��o�^���ĉ�����" TO �A���|���b�Z�[�W
025930          CALL   "MSG001"
025940          CANCEL "MSG001"
025950          PERFORM �t�@�C����
025960          MOVE 99 TO PROGRAM-STATUS
025970          EXIT PROGRAM
025980     ELSE
025990          COMPUTE ����N�v = ����N�v + ��|�{�p�N - 1
026000          MOVE ��|�{�p�� TO ����v
026010     END-IF.
026020*
026030     MOVE ����N���v   TO  ����{�p�N���v.
026040*
026050*================================================================*
026060 ����N�����擾 SECTION.
026070*
026080     MOVE ZERO  TO �v�Z����N�����v.
026090*
026100     IF �v�Z�a��v  NOT = ZERO
026110         MOVE �v�Z�a��v    TO ���|�����敪
026120         READ �����}�X�^
026130         NOT INVALID KEY
026140             MOVE ���|�J�n����N TO �v�Z����N�v
026150         END-READ
026160**
026170         IF �v�Z����N�v = ZERO
026180              MOVE  NC"�����}�X�^�ɊJ�n����N��o�^���ĉ������R" TO �A���|���b�Z�[�W
026190              CALL   "MSG001"
026200              CANCEL "MSG001"
026210              PERFORM �t�@�C����
026220              MOVE 99 TO PROGRAM-STATUS
026230              EXIT PROGRAM
026240         ELSE
026250              COMPUTE �v�Z����N�v = �v�Z����N�v + �v�Z�N�v - 1
026260              MOVE �v�Z���v TO �v�Z����v
026270              MOVE �v�Z���v TO �v�Z������v
026280         END-IF
026290     END-IF.
026300*
026310*================================================================*
011330 ��ƃt�@�C���R�쐬 SECTION.
011340*
026580     OPEN OUTPUT ��ƃt�@�C���R.
026590         MOVE NC"��R" TO �t�@�C����.
026600         PERFORM �I�[�v���`�F�b�N.
           CLOSE ��ƃt�@�C���R.
026580     OPEN I-O ��ƃt�@�C���R.
026590         MOVE NC"��R" TO �t�@�C����.
026600         PERFORM �I�[�v���`�F�b�N.
026710*
005420     MOVE �����a��v�q  TO ���Z�|�����a��.
005430     MOVE �����N�v�q    TO ���Z�|�����N.
005440     MOVE �������v�q    TO ���Z�|������.
005450     MOVE 1             TO ���Z�|�{�p�a��.
005460     MOVE ZERO          TO ���Z�|�{�p�N.
005470     MOVE ZERO          TO ���Z�|�{�p��.
005480     MOVE ZERO          TO ���Z�|���Ҕԍ�.
005490     MOVE LOW-VALUE     TO ���Z�|�}��.
005500     MOVE ZERO          TO ���Z�|���Z���.
005510     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
005520                                  ���Z�|�{�p�a��N��
005530                                  ���Z�|���҃R�[�h
005540                                  ���Z�|���Z���
026830     END-START.
026840     IF ��ԃL�[ = "00"
026850         MOVE SPACE  TO �I���t���O
005580         PERFORM ���Z�v�g�e�Ǎ�
005590         PERFORM UNTIL ( �I���t���O = "YES" ) OR
005600                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
005610                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
005620                       ( ���Z�|������   NOT = �������v�q   )
026910*
026920            PERFORM �f�[�^�`�F�b�N
      */��o�e����J�Ў����ӎ���ۖY��𔲂�/20180809
      *            IF ���Z�|���Z��� = 3
                  IF ���Z�|���Z��� = 3 OR 4 OR 5 OR 6 OR 7 OR 8
                      MOVE SPACE TO ���s�L�[�v
                  END-IF
                  IF ��|�ی���� NOT = 02 AND 07
                      MOVE SPACE TO ���s�L�[�v
                  END-IF
026930            IF ���s�L�[�v = "YES"
                      PERFORM �ԍ��擾
                      READ ��ƃt�@�C���R
                      INVALID KEY
                          INITIALIZE ��R�|���R�[�h
                          PERFORM �ԍ��擾
                          PERFORM ��R���R�[�h�Z�b�g
                          PERFORM ��R�t�@�C������
                      NOT INVALID KEY
                          PERFORM ��R���R�[�h�Z�b�g
                          PERFORM ��R�t�@�C���ǉ�
                      END-READ
005200            END-IF
027850            PERFORM ���Z�v�g�e�Ǎ�
027860         END-PERFORM
027870     END-IF.
027880     CLOSE ��ƃt�@�C���R.
012420*================================================================*
       �ԍ��擾 SECTION.
      *
           EVALUATE TRUE
           WHEN ��|�ی��Ҕԍ� = "01230010"
               MOVE 18 TO ��R�|�ԍ�
           WHEN ��|�ی���� = 07
               MOVE 19 TO ��R�|�ԍ�
           WHEN OTHER
               MOVE 99 TO ��R�|�ԍ�
           END-EVALUATE.
012420*================================================================*
       ��R���R�[�h�Z�b�g SECTION.
      *
           MOVE 20 TO ��R�|���R�[�h�敪.
           IF ��|�{�l�Ƒ��敪 = 1
               COMPUTE ��R�|�{�l�������� = ��R�|�{�l�������� + 1
               COMPUTE ��R�|�{�l�������z = ��R�|�{�l�������z + ���Z�|�������z
           ELSE
               COMPUTE ��R�|�Ƒ��������� = ��R�|�Ƒ��������� + 1
               COMPUTE ��R�|�Ƒ��������z = ��R�|�Ƒ��������z + ���Z�|�������z
           END-IF.
012420*================================================================*
019320 ��R�t�@�C������ SECTION.
019330*
019340     WRITE ��R�|���R�[�h
019350     INVALID KEY
019360         MOVE NC"��R"  TO �t�@�C����
019370         PERFORM �G���[�\��
019380     END-WRITE.
019390*================================================================*
019320 ��R�t�@�C���ǉ� SECTION.
019330*
019340     REWRITE ��R�|���R�[�h
019350     INVALID KEY
019360         MOVE NC"��R"  TO �t�@�C����
019370         PERFORM �G���[�\��
019380     END-REWRITE.
012420*================================================================*
011330 ��ƃt�@�C���S�쐬 SECTION.
011340*
026580     OPEN OUTPUT ��ƃt�@�C���S.
026590         MOVE NC"��S" TO �t�@�C����.
026600         PERFORM �I�[�v���`�F�b�N.
           CLOSE ��ƃt�@�C���S.
026580     OPEN I-O ��ƃt�@�C���S.
026590         MOVE NC"��S" TO �t�@�C����.
026600         PERFORM �I�[�v���`�F�b�N.
026710*
005420     MOVE �����a��v�q  TO ���Z�|�����a��.
005430     MOVE �����N�v�q    TO ���Z�|�����N.
005440     MOVE �������v�q    TO ���Z�|������.
005450     MOVE 1             TO ���Z�|�{�p�a��.
005460     MOVE ZERO          TO ���Z�|�{�p�N.
005470     MOVE ZERO          TO ���Z�|�{�p��.
005480     MOVE ZERO          TO ���Z�|���Ҕԍ�.
005490     MOVE LOW-VALUE     TO ���Z�|�}��.
005500     MOVE ZERO          TO ���Z�|���Z���.
005510     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
005520                                  ���Z�|�{�p�a��N��
005530                                  ���Z�|���҃R�[�h
005540                                  ���Z�|���Z���
026830     END-START.
026840     IF ��ԃL�[ = "00"
026850         MOVE SPACE  TO �I���t���O
005580         PERFORM ���Z�v�g�e�Ǎ�
005590         PERFORM UNTIL ( �I���t���O = "YES" ) OR
005600                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
005610                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
005620                       ( ���Z�|������   NOT = �������v�q   )
026910*
026920            PERFORM �f�[�^�`�F�b�N
      *            IF ���Z�|���Z��� = 3
      *                MOVE SPACE TO ���s�L�[�v
      *            END-IF
      */��o�e����J�Ў����ӎ���ۖY��𔲂�/20180809
                  IF ���Z�|���Z��� = 4 OR 5 OR 6 OR 7 OR 8
                      MOVE SPACE TO ���s�L�[�v
                  END-IF
026930            IF ���s�L�[�v = "YES"
                      INITIALIZE ��S�|���R�[�h
000520                MOVE ��|�{�p�a��N�� TO ��S�|�{�p�a��N��
000570                MOVE ��|���҃R�[�h   TO ��S�|���҃R�[�h
                      MOVE ���Z�|���Z���   TO ��S�|���Z���
      *                READ ��ƃt�@�C���S
      *                INVALID KEY
      *                    INITIALIZE ��S�|���R�[�h�f�[�^
                          PERFORM ��S���R�[�h�Z�b�g
                          PERFORM ��S�t�@�C������
      *                NOT INVALID KEY
      *                    PERFORM ��S���R�[�h�Z�b�g
      *                    PERFORM ��S�t�@�C���ǉ�
      *                END-READ
005200            END-IF
027850            PERFORM ���Z�v�g�e�Ǎ�
027860         END-PERFORM
027870     END-IF.
027880     CLOSE ��ƃt�@�C���S.
012420*================================================================*
       ��S���R�[�h�Z�b�g SECTION.
      *
HILO  *     DISPLAY ��|���R�[�h�L�[ ��|���Ҏ���
           MOVE ���Z�|�{�p�a��N�� TO �{�p�a��N���v�q.
           MOVE ���Z�|���҃R�[�h   TO ���҃R�[�h�v�q.
015500     PERFORM �����f�[�^�擾.
015510     PERFORM �������擾.
015520     PERFORM �{�p�L�^�擾.
           MOVE 30 TO ��S�|���R�[�h�敪.
           IF ���Z�|�����敪 = 2
               MOVE 1    TO ��S�|��o�敪
           ELSE
               MOVE ZERO TO ��S�|��o�敪
           END-IF.
           MOVE ��|�ی��Ҕԍ� TO ��S�|�ی��Ҕԍ�.
           MOVE ��|�L�� TO ��S�|�ی��؋L��.
           MOVE ��|�ԍ� TO ��S�|�ی��ؔԍ�
000500*   / 1:���,2:�V�l,3:����,4:�J��,5:������,6:����,7:���ےP��,8�ی��ؖY�� /
000510     IF ���Z�|���Z��� = 3
               MOVE 3                      TO ��S�|�V�l��Ï����敪
               MOVE ��|��p���S�Ҕԍ����� TO ��S�|�s�����ԍ�
               MOVE ��|��v�Ҕԍ�����     TO ��S�|�󋋎Ҕԍ�
               PERFORM ��Ï����敪�Z�b�g
013174         MOVE ���Z�|�󋋎ҕ��S�z     TO ��S�|�ꕔ���S��
013175         MOVE ���Z�|�����������z     TO ��S�|�������z
           ELSE
               MOVE 1                      TO ��S�|�V�l��Ï����敪
013174         MOVE ���Z�|�ꕔ���S��       TO ��S�|�ꕔ���S��
013175         MOVE ���Z�|�������z         TO ��S�|�������z
           END-IF
           MOVE ��|�{�l�Ƒ��敪 TO ��S�|�{�l�Ƒ��敪
           MOVE ��|��ی��҃J�i TO ��S�|��ی��҃J�i.
           MOVE ��|��ی��Ҏ��� TO ��S�|��ی��Ҏ���.
           MOVE ��|���҃J�i     TO ��S�|���҃J�i.
           MOVE ��|���Ҏ���     TO ��S�|���Ҏ���.
           MOVE ��|���Ґ���     TO ��S�|���Ґ���
015410* ���N����
015420     MOVE ZERO               TO �v�Z�a��N�����v.
015430     MOVE ��|���Ґ��N����   TO �v�Z�a��N�����v.
015440     PERFORM ����N�����擾.
015450     MOVE �v�Z����N�����v   TO ��S�|���Ґ��N����.
013173     MOVE ���Z�|���v         TO ��S�|���v���z
014125     MOVE ���Z�|���t����     TO ��S�|���t����
HILO  *     DISPLAY "��S�|���v���z   " ��S�|���v���z  
HILO  *     DISPLAY "��S�|�ꕔ���S�� " ��S�|�ꕔ���S��
HILO  *     DISPLAY "��S�|�������z   " ��S�|�������z  
HILO  *     DISPLAY "��S�|���t����   " ��S�|���t����  
015430     MOVE ��|�{�p�a��N��   TO �v�Z�a��N�����v.
015440     PERFORM ����N�����擾.
015450     MOVE �v�Z����N�����v   TO ��S�|�f�ÔN��.
018270     MOVE ���ʐ��v           TO ��S�|���ʐ�.
           MOVE ���Z�|���Z������   TO ��S�|������.
      */140417
      *     MOVE 90                 TO ��S�|�[���敪.
           MOVE 18                 TO ��S�|�[���敪.
           EVALUATE ��|���ʋ敪
           WHEN 1
           WHEN 2
               MOVE 8 TO ��S�|���t����
           WHEN 3
               MOVE 0 TO ��S�|���t����
           WHEN 6
               MOVE 6 TO ��S�|���t����
           WHEN OTHER
               IF ��|�ی���� = 01 OR 08
                   IF ��|�{�l�Ƒ��敪 = 1
                       MOVE 2 TO ��S�|���t����
                   ELSE
                       MOVE 6 TO ��S�|���t����
                   END-IF
               END-IF
           END-EVALUATE.
018330     MOVE �V�K�敪�v             TO ��S�|�V�K�敪.
018340     MOVE �p���敪�v             TO ��S�|�p���敪.
018360     MOVE �����񐔂v             TO ��S�|������.
018370     MOVE �������ԊO�񐔂v       TO ��S�|�������ԊO���Z��.
018380     MOVE �����x���񐔂v         TO ��S�|�����x�����Z��.
018390     MOVE �����[��񐔂v         TO ��S�|�����[����Z��.
018400     MOVE �Č��񐔂v             TO ��S�|�Č���.
018410     MOVE ���Ë����Q�v           TO ��S�|���Ë���.
018420     MOVE ���É񐔂v             TO ��S�|���É�.
           MOVE �������v               TO ��S�|������.
           MOVE �������Z�v             TO ��S�|�������Z.
           MOVE �Č����v               TO ��S�|�Č���.
018420     MOVE ���×��v               TO ��S�|���×�.
           MOVE ���É��Z�v             TO ��S�|���É��Z.
018430*
018440     MOVE ���Ö�Ԃv             TO ��S�|��ԉ��Z���É�.
018450     MOVE ���Ö\���v             TO ��S�|�\���J����Z���É�.
018460     MOVE ���Ó�H�v             TO ��S�|��H���Z���É�.
018470*
      */20180612
           IF �{�p�a��N���v�q < 43006
018480         MOVE ��񐔂v           TO ��S�|�������q���
018490         MOVE ���񐔂v           TO ��S�|�������q����
018500         MOVE ���񐔂v           TO ��S�|�������q����
      *������/20180612
           ELSE
               COMPUTE �������q�񐔂v = ���Z�|�� + ���Z�|�� + ���Z�|��
               IF �������q�񐔂v > 9
                   MOVE 9 TO �������q�񐔂v
               END-IF
               MOVE �������q�񐔂v TO ��S�|�������q��
           END-IF.
           MOVE ���Z�|�^����É�     TO ��S�|�^����×���.
           MOVE ���Z�|�^����×�       TO ��S�|�^����×�    .
      *������/20180612
018160     MOVE �������q�v             TO ��S�|�������q.
018510*
018520     MOVE ���񋟗��񐔂v       TO ��S�|���񋟗���.
           MOVE ���k�x���񐔂v         TO ��S�|���k�x����.
           MOVE ���񋟗��v           TO ��S�|���񋟗�.
           MOVE ���k�x�����v           TO ��S�|���k�x����.
      */���׏����s�̐����Z�ǉ�/20221212
           MOVE ���׏����s�����v       TO ��S�|���׏����s����.
           MOVE ���׏����s�񐔂v       TO ��S�|���׏����s��.
           MOVE ���׏����s�v           TO ��S�|���׏����s��.
      *
014740*     PERFORM VARYING �J�E���^ FROM 1 BY 1
014750*             UNTIL   �J�E���^ > 8
      *         IF ���Z�|���񏈒u��(�J�E���^) NOT = ZERO
028440*             MOVE ���Z�|���񏈒u��(�J�E���^) TO ��S�|������(�J�E���^)
      *         END-IF
014810*     END-PERFORM.
014730*/�{�p��
014740     PERFORM VARYING �J�E���^ FROM 1 BY 1
014750             UNTIL   �J�E���^ > 31
014760         IF �{�p���v(�J�E���^) = ZERO
014770            MOVE ZERO               TO ��S�|�{�p��(�J�E���^)
014780         ELSE
014790            MOVE �{�p���v(�J�E���^) TO ��S�|�{�p��(�J�E���^)
014800         END-IF
014810     END-PERFORM.
      *
           PERFORM �����p���҂e�Ǎ�.
023870     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���ʐ��v
      */��ʃR�[�h
025340         MOVE ���|�������(���ʂb�m�s)     TO ���|�������
025350         MOVE ���|����(���ʂb�m�s)         TO ���|����
025360         MOVE ���|���E�敪(���ʂb�m�s)     TO ���|���E�敪
025370         MOVE ���|�����ʒu�ԍ�(���ʂb�m�s) TO ���|�����ʒu�ԍ�
027460         READ ���ʃ}�X�^
027470         NOT INVALID KEY
                   MOVE ���|��ʃR�[�h TO ��S�|��ʃR�[�h(���ʂb�m�s)
               END-READ
      */��������
               MOVE 01                             TO �����|�敪�R�[�h
               MOVE ���|���������R�[�h(���ʂb�m�s) TO �����|���������R�[�h
               READ ���������e
               NOT INVALID KEY
      *             MOVE �����|���������b�l�S��     TO ��S�|��������(���ʂb�m�s)
                   STRING �����|���������b�l(1) DELIMITED BY SPACE
                          �����|���������b�l(2) DELIMITED BY SPACE
                          �����|���������b�l(3) DELIMITED BY SPACE
                          �����|���������b�l(4) DELIMITED BY SPACE
                          �����|���������b�l(5) DELIMITED BY SPACE
                     INTO ��S�|��������(���ʂb�m�s)
                   END-STRING
               END-READ
      */�������R
               MOVE ���p�|���R��(���ʂb�m�s)       TO  ��S�|�������R(���ʂb�m�s)
           END-PERFORM.
      *
           COMPUTE ��S�|�����ʔ�p�z(1) = ���Z�|���������v�P   + ���Z�|���񏈒u��(1)
           COMPUTE ��S�|�����ʔ�p�z(2) = ���Z�|���������v�Q   + ���Z�|���񏈒u��(2)
           COMPUTE ��S�|�����ʔ�p�z(3) = ���Z�|���������v�R�W + ���Z�|���������v�R�O + ���Z�|���񏈒u��(3)
           COMPUTE ��S�|�����ʔ�p�z(4) = ���Z�|���������v�S�T + ���Z�|���������v�S�W + ���Z�|���������v�S�O 
                                         + ���Z�|���񏈒u��(4)
           COMPUTE ��S�|�����ʔ�p�z(5) = ���Z�|���������v�T�Q + ���Z�|���������v�T�T + ���Z�|���������v�T�W 
                                         + ���Z�|���������v�T�O + ���Z�|���񏈒u��(5)
           COMPUTE ��S�|�����ʔ�p�z(6) = ���Z�|���������v�U�W + ���Z�|���������v�U�O + ���Z�|���񏈒u��(6)
           COMPUTE ��S�|�����ʔ�p�z(7) = ���Z�|���������v�V�W + ���Z�|���������v�V�O + ���Z�|���񏈒u��(7)
015550* �P���ʖ�
015560     MOVE ������ʂv(1)          TO ������ʕϊ��O�v.
015570     PERFORM ������ʕϊ�.
015580     MOVE ������ʕϊ���v       TO ��S�|�����敪(1).
015590     MOVE �������v(1)            TO ��S�|������(1).
015600     MOVE ���񏈒u�񐔂v(1)      TO ��S�|���񏈒u��(1).
015610*
015620     MOVE ZERO                   TO �v�Z�a��N�����v.
015630     MOVE �����N�����v(1)        TO �v�Z�a��N�����v.
015640     PERFORM ����N�����擾.
015650     MOVE �v�Z����N�����v       TO ��S�|�����N����(1).
015660*
015670     MOVE ZERO                   TO �v�Z�a��N�����v.
015680     MOVE �����N�����v(1)        TO �v�Z�a��N�����v.
015690     PERFORM ����N�����擾.
015700     MOVE �v�Z����N�����v       TO ��S�|�����N����(1).
015710*
015720     MOVE ZERO                   TO �v�Z�a��N�����v.
015730     MOVE �J�n�N�����v(1)        TO �v�Z�a��N�����v.
015740     PERFORM ����N�����擾.
015750     MOVE �v�Z����N�����v       TO ��S�|�{�p�J�n�N����(1).
015760*
015770     MOVE ZERO                   TO �v�Z�a��N�����v.
015780     MOVE �I���N�����v(1)        TO �v�Z�a��N�����v.
015790     PERFORM ����N�����擾.
015800     MOVE �v�Z����N�����v       TO ��S�|�{�p�I���N����(1).
015810*
015820     MOVE �������v(1)            TO ��S�|���ʎ�����(1).
015830*
015840     MOVE �]�A�敪�v(1)          TO �]�A�ϊ��O�v.
015850     PERFORM �]�A�敪�ϊ�.
015860     MOVE �]�A�ϊ���v           TO ��S�|�]�A�敪(1).
015870*
015880     MOVE ��É񐔂P�v�q         TO ��S�|��É�(1).
015890*
015900     MOVE ��㪖@�񐔂P�v�q       TO ��S�|��㪖@��(1).
015910*
015920     MOVE ��㪖@�񐔂P�v�q       TO ��S�|��㪖@��(1).
015930*
015940     MOVE �d�É񐔂P�v�q         TO ��S�|�d�É�(1).
015950*
015960     MOVE �����ʋ敪�v(1)        TO ��S�|�����ʒ����敪(1).
015970*
015980     MOVE �����敪�v(1)          TO ��S�|���������敪(1).
      *
           MOVE ���Z�|��×��P         TO ��S�|��×�(1).
           MOVE ���Z�|��㪖@���P       TO ��S�|��㪖@��(1).
           MOVE ���Z�|��㪖@���P       TO ��S�|��㪖@��(1).
           MOVE ���Z�|�d�×��P         TO ��S�|�d�×�(1).
015990*
016000* �Q���ʖ�
016010     MOVE ������ʂv(2)          TO ������ʕϊ��O�v.
016020     PERFORM ������ʕϊ�.
016030     MOVE ������ʕϊ���v       TO ��S�|�����敪(2).
016040     MOVE �������v(2)            TO ��S�|������(2).
016050     MOVE ���񏈒u�񐔂v(2)      TO ��S�|���񏈒u��(2).
016060*
016070     MOVE ZERO                   TO �v�Z�a��N�����v.
016080     MOVE �����N�����v(2)        TO �v�Z�a��N�����v.
016090     PERFORM ����N�����擾.
016100     MOVE �v�Z����N�����v       TO ��S�|�����N����(2).
016110*
016120     MOVE ZERO                   TO �v�Z�a��N�����v.
016130     MOVE �����N�����v(2)        TO �v�Z�a��N�����v.
016140     PERFORM ����N�����擾.
016150     MOVE �v�Z����N�����v       TO ��S�|�����N����(2).
016160*
016170     MOVE ZERO                   TO �v�Z�a��N�����v.
016180     MOVE �J�n�N�����v(2)        TO �v�Z�a��N�����v.
016190     PERFORM ����N�����擾.
016200     MOVE �v�Z����N�����v       TO ��S�|�{�p�J�n�N����(2).
016210*
016220     MOVE ZERO                   TO �v�Z�a��N�����v.
016230     MOVE �I���N�����v(2)        TO �v�Z�a��N�����v.
016240     PERFORM ����N�����擾.
016250     MOVE �v�Z����N�����v       TO ��S�|�{�p�I���N����(2).
016260*
016270     MOVE �������v(2)            TO ��S�|���ʎ�����(2).
016280*
016290     MOVE �]�A�敪�v(2)          TO �]�A�ϊ��O�v.
016300     PERFORM �]�A�敪�ϊ�.
016310     MOVE �]�A�ϊ���v           TO ��S�|�]�A�敪(2).
016320*
016330     MOVE ��É񐔂Q�v�q         TO ��S�|��É�(2).
016340*
016350     MOVE ��㪖@�񐔂Q�v�q       TO ��S�|��㪖@��(2).
016360*
016370     MOVE ��㪖@�񐔂Q�v�q       TO ��S�|��㪖@��(2).
016380*
016390     MOVE �d�É񐔂Q�v�q         TO ��S�|�d�É�(2).
016400*
016410     MOVE �����ʋ敪�v(2)        TO ��S�|�����ʒ����敪(2).
016420*
016430     MOVE �����敪�v(2)          TO ��S�|���������敪(2).
      *
           MOVE ���Z�|��×��Q         TO ��S�|��×�(2).
           MOVE ���Z�|��㪖@���Q       TO ��S�|��㪖@��(2).
           MOVE ���Z�|��㪖@���Q       TO ��S�|��㪖@��(2).
           MOVE ���Z�|�d�×��Q         TO ��S�|�d�×�(2).
016440*
016450* �R���ʖ�
016460     MOVE ������ʂv(3)          TO ������ʕϊ��O�v.
016470     PERFORM ������ʕϊ�.
016480     MOVE ������ʕϊ���v       TO ��S�|�����敪(3).
016490     MOVE �������v(3)            TO ��S�|������(3).
016500     MOVE ���񏈒u�񐔂v(3)      TO ��S�|���񏈒u��(3).
016510*
016520     MOVE ZERO                   TO �v�Z�a��N�����v.
016530     MOVE �����N�����v(3)        TO �v�Z�a��N�����v.
016540     PERFORM ����N�����擾.
016550     MOVE �v�Z����N�����v       TO ��S�|�����N����(3).
016560*
016570     MOVE ZERO                   TO �v�Z�a��N�����v.
016580     MOVE �����N�����v(3)        TO �v�Z�a��N�����v.
016590     PERFORM ����N�����擾.
016600     MOVE �v�Z����N�����v       TO ��S�|�����N����(3).
016610*
016620     MOVE ZERO                   TO �v�Z�a��N�����v.
016630     MOVE �J�n�N�����v(3)        TO �v�Z�a��N�����v.
016640     PERFORM ����N�����擾.
016650     MOVE �v�Z����N�����v       TO ��S�|�{�p�J�n�N����(3).
016660*
016670     MOVE ZERO                   TO �v�Z�a��N�����v.
016680     MOVE �I���N�����v(3)        TO �v�Z�a��N�����v.
016690     PERFORM ����N�����擾.
016700     MOVE �v�Z����N�����v       TO ��S�|�{�p�I���N����(3).
016710*
016720     MOVE �������v(3)            TO ��S�|���ʎ�����(3).
016730*
016740     MOVE �]�A�敪�v(3)          TO �]�A�ϊ��O�v.
016750     PERFORM �]�A�敪�ϊ�.
016760     MOVE �]�A�ϊ���v           TO ��S�|�]�A�敪(3).
016770*
016780     MOVE ��É񐔂R�v�q         TO ��S�|��É�(3).
016790*
016800     MOVE ��㪖@�񐔂R�v�q       TO ��S�|��㪖@��(3).
016810*
016820     MOVE ��㪖@�񐔂R�v�q       TO ��S�|��㪖@��(3).
016830*
016840     MOVE �d�É񐔂R�v�q         TO ��S�|�d�É�(3).
016850*
016860     MOVE �����ʋ敪�v(3)        TO ��S�|�����ʒ����敪(3).
016870*
016880     MOVE �����敪�v(3)          TO ��S�|���������敪(3).
      *
011490     COMPUTE ��S�|��×�(3)   = ���Z�|��×��R�W   + ���Z�|��×��R�O.
011640     COMPUTE ��S�|��㪖@��(3) = ���Z�|��㪖@���R�W + ���Z�|��㪖@���R�O.
011750     COMPUTE ��S�|��㪖@��(3) = ���Z�|��㪖@���R�W + ���Z�|��㪖@���R�O.
011860     COMPUTE ��S�|�d�×�(3)   = ���Z�|�d�×��R�W   + ���Z�|�d�×��R�O.
016890*
016900* �S���ʖ�
016910     MOVE ������ʂv(4)          TO ������ʕϊ��O�v.
016920     PERFORM ������ʕϊ�.
016930     MOVE ������ʕϊ���v       TO ��S�|�����敪(4).
016940     MOVE �������v(4)            TO ��S�|������(4).
016950     MOVE ���񏈒u�񐔂v(4)      TO ��S�|���񏈒u��(4).
016960*
016970     MOVE ZERO                   TO �v�Z�a��N�����v.
016980     MOVE �����N�����v(4)        TO �v�Z�a��N�����v.
016990     PERFORM ����N�����擾.
017000     MOVE �v�Z����N�����v       TO ��S�|�����N����(4).
017010*
017020     MOVE ZERO                   TO �v�Z�a��N�����v.
017030     MOVE �����N�����v(4)        TO �v�Z�a��N�����v.
017040     PERFORM ����N�����擾.
017050     MOVE �v�Z����N�����v       TO ��S�|�����N����(4).
017060*
017070     MOVE ZERO                   TO �v�Z�a��N�����v.
017080     MOVE �J�n�N�����v(4)        TO �v�Z�a��N�����v.
017090     PERFORM ����N�����擾.
017100     MOVE �v�Z����N�����v       TO ��S�|�{�p�J�n�N����(4).
017110*
017120     MOVE ZERO                   TO �v�Z�a��N�����v.
017130     MOVE �I���N�����v(4)        TO �v�Z�a��N�����v.
017140     PERFORM ����N�����擾.
017150     MOVE �v�Z����N�����v       TO ��S�|�{�p�I���N����(4).
017160*
017170     MOVE �������v(4)            TO ��S�|���ʎ�����(4).
017180*
017190     MOVE �]�A�敪�v(4)          TO �]�A�ϊ��O�v.
017200     PERFORM �]�A�敪�ϊ�.
017210     MOVE �]�A�ϊ���v           TO ��S�|�]�A�敪(4).
017220*
017230     MOVE ��É񐔂S�v�q         TO ��S�|��É�(4).
017240*
017250     MOVE ��㪖@�񐔂S�v�q       TO ��S�|��㪖@��(4).
017260*
017270     MOVE ��㪖@�񐔂S�v�q       TO ��S�|��㪖@��(4).
017280*
017290     MOVE �d�É񐔂S�v�q         TO ��S�|�d�É�(4).
017300*
017310     MOVE �����ʋ敪�v(4)        TO ��S�|�����ʒ����敪(4).
017320*
017330     MOVE �����敪�v(4)          TO ��S�|���������敪(4).
      *
011490     COMPUTE ��S�|��×�(4)   = ���Z�|��×��S�T   + ���Z�|��×��S�W   + ���Z�|��×��S�O.
011640     COMPUTE ��S�|��㪖@��(4) = ���Z�|��㪖@���S�T + ���Z�|��㪖@���S�W + ���Z�|��㪖@���S�O.
011750     COMPUTE ��S�|��㪖@��(4) = ���Z�|��㪖@���S�T + ���Z�|��㪖@���S�W + ���Z�|��㪖@���S�O.
011860     COMPUTE ��S�|�d�×�(4)   = ���Z�|�d�×��S�T   + ���Z�|�d�×��S�W   + ���Z�|�d�×��S�O.
017340*
017350* �T���ʖ�
017360     MOVE ������ʂv(5)          TO ������ʕϊ��O�v.
017370     PERFORM ������ʕϊ�.
017380     MOVE ������ʕϊ���v       TO ��S�|�����敪(5).
017390     MOVE �������v(5)            TO ��S�|������(5).
017400     MOVE ���񏈒u�񐔂v(5)      TO ��S�|���񏈒u��(5).
017410*
017420     MOVE ZERO                   TO �v�Z�a��N�����v.
017430     MOVE �����N�����v(5)        TO �v�Z�a��N�����v.
017440     PERFORM ����N�����擾.
017450     MOVE �v�Z����N�����v       TO ��S�|�����N����(5).
017460*
017470     MOVE ZERO                   TO �v�Z�a��N�����v.
017480     MOVE �����N�����v(5)        TO �v�Z�a��N�����v.
017490     PERFORM ����N�����擾.
017500     MOVE �v�Z����N�����v       TO ��S�|�����N����(5).
017510*
017520     MOVE ZERO                   TO �v�Z�a��N�����v.
017530     MOVE �J�n�N�����v(5)        TO �v�Z�a��N�����v.
017540     PERFORM ����N�����擾.
017550     MOVE �v�Z����N�����v       TO ��S�|�{�p�J�n�N����(5).
017560*
017570     MOVE ZERO                   TO �v�Z�a��N�����v.
017580     MOVE �I���N�����v(5)        TO �v�Z�a��N�����v.
017590     PERFORM ����N�����擾.
017600     MOVE �v�Z����N�����v       TO ��S�|�{�p�I���N����(5).
017610*
017620     MOVE �������v(5)            TO ��S�|���ʎ�����(5).
017630*
017640     MOVE �]�A�敪�v(5)          TO �]�A�ϊ��O�v.
017650     PERFORM �]�A�敪�ϊ�.
017660     MOVE �]�A�ϊ���v           TO ��S�|�]�A�敪(5).
017670*
017680     MOVE ��É񐔂T�v�q         TO ��S�|��É�(5).
017690*
017700     MOVE ��㪖@�񐔂T�v�q       TO ��S�|��㪖@��(5).
017710*
017720     MOVE ��㪖@�񐔂T�v�q       TO ��S�|��㪖@��(5).
017730*
017740     MOVE �d�É񐔂T�v�q         TO ��S�|�d�É�(5).
017750*
017760     MOVE �����ʋ敪�v(5)        TO ��S�|�����ʒ����敪(5).
017770*
017780     MOVE �����敪�v(5)          TO ��S�|���������敪(5).
      *
011490     COMPUTE ��S�|��×�(5)   = ���Z�|��×��T�Q   + ���Z�|��×��T�T   +
                                       ���Z�|��×��T�W   + ���Z�|��×��T�O.
011640     COMPUTE ��S�|��㪖@��(5) = ���Z�|��㪖@���T�Q + ���Z�|��㪖@���T�T +
                                       ���Z�|��㪖@���T�W + ���Z�|��㪖@���T�O.
011750     COMPUTE ��S�|��㪖@��(5) = ���Z�|��㪖@���T�Q + ���Z�|��㪖@���T�T +
                                       ���Z�|��㪖@���T�W + ���Z�|��㪖@���T�O.
011860     COMPUTE ��S�|�d�×�(5)   = ���Z�|�d�×��T�Q   + ���Z�|�d�×��T�T   +
                                       ���Z�|�d�×��T�W   + ���Z�|�d�×��T�O.
017790*
017800* �U���ʖ�
017810     MOVE ������ʂv(6)          TO ������ʕϊ��O�v.
017820     PERFORM ������ʕϊ�.
017830     MOVE ������ʕϊ���v       TO ��S�|�����敪(6).
017840     MOVE �������v(6)            TO ��S�|������(6).
017850     MOVE ���񏈒u�񐔂v(6)      TO ��S�|���񏈒u��(6).
017860*
017870     MOVE ZERO                   TO �v�Z�a��N�����v.
017880     MOVE �����N�����v(6)        TO �v�Z�a��N�����v.
017890     PERFORM ����N�����擾.
017900     MOVE �v�Z����N�����v       TO ��S�|�����N����(6).
017910*
017920     MOVE ZERO                   TO �v�Z�a��N�����v.
017930     MOVE �����N�����v(6)        TO �v�Z�a��N�����v.
017940     PERFORM ����N�����擾.
017950     MOVE �v�Z����N�����v       TO ��S�|�����N����(6).
017960*
017970     MOVE ZERO                   TO �v�Z�a��N�����v.
017980     MOVE �J�n�N�����v(6)        TO �v�Z�a��N�����v.
017990     PERFORM ����N�����擾.
018000     MOVE �v�Z����N�����v       TO ��S�|�{�p�J�n�N����(6).
018010*
018020     MOVE ZERO                   TO �v�Z�a��N�����v.
018030     MOVE �I���N�����v(6)        TO �v�Z�a��N�����v.
018040     PERFORM ����N�����擾.
018050     MOVE �v�Z����N�����v       TO ��S�|�{�p�I���N����(6).
018060*
018070     MOVE �������v(6)            TO ��S�|���ʎ�����(6).
018080*
018090     MOVE �]�A�敪�v(6)          TO �]�A�ϊ��O�v.
018100     PERFORM �]�A�敪�ϊ�.
018110     MOVE �]�A�ϊ���v           TO ��S�|�]�A�敪(6).
018120*
018130     MOVE ZERO                   TO ��S�|��É�(6).
018140*
018150     MOVE ZERO                   TO ��S�|��㪖@��(6).
018160*
018170     MOVE ZERO                   TO ��S�|��㪖@��(6).
018180*
018190     MOVE ZERO                   TO ��S�|�d�É�(6).
018200*
018210     MOVE �����ʋ敪�v(6)        TO ��S�|�����ʒ����敪(6).
018220*
018230     MOVE �����敪�v(6)          TO ��S�|���������敪(6).
      *
011490     COMPUTE ��S�|��×�(6)   = ���Z�|��×��U�W   + ���Z�|��×��U�O.
011640     COMPUTE ��S�|��㪖@��(6) = ���Z�|��㪖@���U�W + ���Z�|��㪖@���U�O.
011750     COMPUTE ��S�|��㪖@��(6) = ���Z�|��㪖@���U�W + ���Z�|��㪖@���U�O.
011860     COMPUTE ��S�|�d�×�(6)   = ���Z�|�d�×��U�W   + ���Z�|�d�×��U�O.
017790*
017800* �V���ʖ�
017810     MOVE ������ʂv(7)          TO ������ʕϊ��O�v.
017820     PERFORM ������ʕϊ�.
017830     MOVE ������ʕϊ���v       TO ��S�|�����敪(7).
017840     MOVE �������v(7)            TO ��S�|������(7).
017850     MOVE ���񏈒u�񐔂v(7)      TO ��S�|���񏈒u��(7).
017860*
017870     MOVE ZERO                   TO �v�Z�a��N�����v.
017880     MOVE �����N�����v(7)        TO �v�Z�a��N�����v.
017890     PERFORM ����N�����擾.
017900     MOVE �v�Z����N�����v       TO ��S�|�����N����(7).
017910*
017920     MOVE ZERO                   TO �v�Z�a��N�����v.
017930     MOVE �����N�����v(7)        TO �v�Z�a��N�����v.
017940     PERFORM ����N�����擾.
017950     MOVE �v�Z����N�����v       TO ��S�|�����N����(7).
017960*
017970     MOVE ZERO                   TO �v�Z�a��N�����v.
017980     MOVE �J�n�N�����v(7)        TO �v�Z�a��N�����v.
017990     PERFORM ����N�����擾.
018000     MOVE �v�Z����N�����v       TO ��S�|�{�p�J�n�N����(7).
018010*
018020     MOVE ZERO                   TO �v�Z�a��N�����v.
018030     MOVE �I���N�����v(7)        TO �v�Z�a��N�����v.
018040     PERFORM ����N�����擾.
018050     MOVE �v�Z����N�����v       TO ��S�|�{�p�I���N����(7).
018060*
018070     MOVE �������v(7)            TO ��S�|���ʎ�����(7).
018080*
018090     MOVE �]�A�敪�v(7)          TO �]�A�ϊ��O�v.
018100     PERFORM �]�A�敪�ϊ�.
018110     MOVE �]�A�ϊ���v           TO ��S�|�]�A�敪(7).
018120*
018130     MOVE ZERO                   TO ��S�|��É�(7).
018140*
018150     MOVE ZERO                   TO ��S�|��㪖@��(7).
018160*
018170     MOVE ZERO                   TO ��S�|��㪖@��(7).
018180*
018190     MOVE ZERO                   TO ��S�|�d�É�(7).
018200*
018210     MOVE �����ʋ敪�v(7)        TO ��S�|�����ʒ����敪(7).
018220*
018230     MOVE �����敪�v(7)          TO ��S�|���������敪(7).
      *
011490     COMPUTE ��S�|��×�(7)   = ���Z�|��×��V�W   + ���Z�|��×��V�O.
011640     COMPUTE ��S�|��㪖@��(7) = ���Z�|��㪖@���V�W + ���Z�|��㪖@���V�O.
011750     COMPUTE ��S�|��㪖@��(7) = ���Z�|��㪖@���V�W + ���Z�|��㪖@���V�O.
011860     COMPUTE ��S�|�d�×�(7)   = ���Z�|�d�×��V�W   + ���Z�|�d�×��V�O.
      *
012420*================================================================*
019320 ��S�t�@�C������ SECTION.
019330*
019340     WRITE ��S�|���R�[�h
019350     INVALID KEY
019360         MOVE NC"��S"  TO �t�@�C����
019370         PERFORM �G���[�\��
019380     END-WRITE.
019390*================================================================*
019320 ��S�t�@�C���ǉ� SECTION.
019330*
019340     REWRITE ��S�|���R�[�h
019350     INVALID KEY
019360         MOVE NC"��S"  TO �t�@�C����
019370         PERFORM �G���[�\��
019380     END-REWRITE.
012420*================================================================*
       ��Ï����敪�Z�b�g SECTION.
      *
           EVALUATE ��|�������
           WHEN 52
               MOVE 4 TO ��S�|��Ï����敪
           WHEN 53
               MOVE 3 TO ��S�|��Ï����敪
           WHEN 54
               MOVE 6 TO ��S�|��Ï����敪
           WHEN 55
               MOVE 2 TO ��S�|��Ï����敪
           WHEN 60
004288         IF ��|��p���S�Ҕԍ�����(1:4) = "8923"
                   MOVE 7 TO ��S�|��Ï����敪
               END-IF
           END-EVALUATE.
012420*================================================================*
       �����p���҂e�Ǎ� SECTION.
      *
           MOVE ���Z�|�{�p�a��N�� TO ���p�|�{�p�a��N��.
000170     MOVE ���Z�|���҃R�[�h   TO ���p�|���҃R�[�h.
           READ �����p���҂e
           INVALID KEY
               MOVE SPACE TO ���p�|���R�[�h
           END-READ.
012420*================================================================*
027744******************************************************************
027745 END PROGRAM YAI101.
027746******************************************************************
