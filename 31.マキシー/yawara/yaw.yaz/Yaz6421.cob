000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAZ6421.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090* ���S�ۏᐮ��   �������Z�v�g����i�V�_����޳�ޔŁj*
000100*         MED = YAW610 YAZ6421P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2018-08-03
000130 DATE-COMPILED.          2018-08-03
      */����13�A�{��04�̏ꍇ�A�O������҂P���́A���t�������W���ɂ���B(�����P�����S���邽�߁A���҂P���A�ی��҂W���A���P���ƂȂ�)������/160817
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
      */���׏����s���Z��K�p�Q�ɒǉ�/2022
      */2024.10  �����p���K�p�ɒǉ�/2407
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
000260     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  �ہ|�ی����
000300                                                          �ہ|�ی��Ҕԍ�
000310* �����́A�L�[���ڂ̕ی��Җ��̂�ی��҃J�i�ɂ���
000320                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000330                                                          �ہ|�ی��Җ���
000340                                                          �ہ|�ی��Ҕԍ�
000350                             FILE STATUS              IS  ��ԃL�[
000360                             LOCK        MODE         IS  AUTOMATIC.
000370     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000380                             ORGANIZATION             IS  INDEXED
000390                             ACCESS MODE              IS  DYNAMIC
000400                             RECORD KEY               IS  ���|�����敪
000410                             FILE STATUS              IS  ��ԃL�[
000420                             LOCK        MODE         IS  AUTOMATIC.
000430     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000440                             ORGANIZATION             IS  INDEXED
000450                             ACCESS MODE              IS  DYNAMIC
000460                             RECORD KEY               IS  ���|�敪�R�[�h
000470                                                          ���|���̃R�[�h
000480                             FILE STATUS              IS  ��ԃL�[
000490                             LOCK        MODE         IS  AUTOMATIC.
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
000560     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000570                             ORGANIZATION             IS  INDEXED
000580                             ACCESS MODE              IS  DYNAMIC
000590                             RECORD KEY               IS  ���|����敪
000600                             FILE STATUS              IS  ��ԃL�[
000610                             LOCK        MODE         IS  AUTOMATIC.
000620     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000630                             ORGANIZATION             IS  INDEXED
000640                             ACCESS MODE              IS  DYNAMIC
000650                             RECORD KEY               IS  �{��|�{�p���ԍ�
000660                             FILE STATUS              IS  ��ԃL�[
000670                             LOCK        MODE         IS  AUTOMATIC.
000750     SELECT  �o�߃}�X�^      ASSIGN      TO        KEIKAL
000760                             ORGANIZATION             IS  INDEXED
000770                             ACCESS MODE              IS  DYNAMIC
000780                             RECORD KEY               IS  �o�|�敪�R�[�h
000790                                                          �o�|�o�߃R�[�h
000800                             FILE STATUS              IS  ��ԃL�[
000810                             LOCK        MODE         IS  AUTOMATIC.
000820     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000830                             ORGANIZATION             IS  INDEXED
000840                             ACCESS MODE              IS  DYNAMIC
000850                             RECORD KEY               IS  ��|�{�p�a��N��
000860                                                          ��|���҃R�[�h
000870                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000880                                                          ��|���҃J�i
000890                                                          ��|���҃R�[�h
000900                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000910                                                          ��|�{�p�a��N��
000920                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000930                                                          ��|�ی����
000940                                                          ��|�ی��Ҕԍ�
000950                                                          ��|���҃R�[�h
000960                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000970                                                          ��|������
000980                                                          ��|��p���S�Ҕԍ�
000990                                                          ��|���҃R�[�h
001000                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001010                                                          ��|�������
001020                                                          ��|��p���S�Ҕԍ�����
001030                                                          ��|���҃R�[�h
001040                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
001050                                                          ��|�{�p�a��N��
001060                                                          ��|���҃R�[�h
001070                             FILE STATUS              IS  ��ԃL�[
001080                             LOCK        MODE         IS  AUTOMATIC.
000180     SELECT  ��f�ҏ��Q�e  ASSIGN      TO        JUSINJ2L
000190                             ORGANIZATION             IS INDEXED
000200                             ACCESS MODE              IS DYNAMIC
000210                             RECORD KEY               IS ��Q�|�{�p�a��N��
000220                                                         ��Q�|���҃R�[�h
000230                             ALTERNATE RECORD KEY     IS ��Q�|�����Ώۋ敪
000240                                                         ��Q�|�����a��N��
000250                                                         ��Q�|�{�p�a��N��
000260                                                         ��Q�|���҃R�[�h
000270                             ALTERNATE RECORD KEY     IS ��Q�|���������Ώۋ敪
000280                                                         ��Q�|���������a��N��
000290                                                         ��Q�|�{�p�a��N��
000300                                                         ��Q�|���҃R�[�h
000310                             FILE STATUS              IS  ��ԃL�[
000320                             LOCK        MODE         IS  AUTOMATIC.
001090     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
001100                             ORGANIZATION             IS  INDEXED
001110                             ACCESS MODE              IS  DYNAMIC
001120                             RECORD KEY               IS  �{�L�|�{�p�a��N����
001130                                                          �{�L�|���҃R�[�h
001140                             ALTERNATE RECORD KEY     IS  �{�L�|���҃R�[�h
001150                                                          �{�L�|�{�p�a��N����
001160                             FILE STATUS              IS  ��ԃL�[
001170                             LOCK        MODE         IS  AUTOMATIC.
001180     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
001190                             ORGANIZATION             IS  INDEXED
001200                             ACCESS MODE              IS  DYNAMIC
001210                             RECORD KEY               IS  ���|�{�p�a��N��
001220                                                          ���|���҃R�[�h
001230                             ALTERNATE RECORD KEY     IS  ���|���҃R�[�h
001240                                                          ���|�{�p�a��N��
001250                             FILE STATUS              IS  ��ԃL�[
001260                             LOCK        MODE         IS  AUTOMATIC.
001270     SELECT  ���������e      ASSIGN      TO        HUGEINL
001280                             ORGANIZATION             IS  INDEXED
001290                             ACCESS MODE              IS  DYNAMIC
001300                             RECORD KEY               IS  �����|�敪�R�[�h
001310                                                          �����|���������R�[�h
001320                             FILE STATUS              IS  ��ԃL�[
001330                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  �h�c�Ǘ��}�X�^    ASSIGN      TO        IDKANRL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  �h�c�ǁ|�h�c�敪
001380                                                          �h�c�ǁ|�{�p���ԍ�
001390                                                          �h�c�ǁ|�ی����
001400                                                          �h�c�ǁ|�ی��Ҕԍ�
001410                             ALTERNATE RECORD KEY     IS  �h�c�ǁ|�{�p�h�c�ԍ�
001420                                                          �h�c�ǁ|�h�c�敪
001430                                                          �h�c�ǁ|�{�p���ԍ�
001440                                                          �h�c�ǁ|�ی����
001450                                                          �h�c�ǁ|�ی��Ҕԍ�
001460                             FILE STATUS              IS  ��ԃL�[
001470                             LOCK        MODE         IS  AUTOMATIC.
001480     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
001490                             ORGANIZATION             IS  INDEXED
001500                             ACCESS MODE              IS  DYNAMIC
001510                             RECORD KEY               IS  �s�|������
001520                                                          �s�|�s�����ԍ�
001530                             ALTERNATE RECORD KEY     IS  �s�|������
001540                                                          �s�|�s��������
001550                                                          �s�|�s�����ԍ�
001560                             FILE STATUS              IS  ��ԃL�[
001570                             LOCK        MODE         IS  AUTOMATIC.
000790     SELECT  ������}�X�^    ASSIGN      TO        SEIKYUSL
000800                             ORGANIZATION           IS  INDEXED
000810                             ACCESS MODE            IS  DYNAMIC
000820                             RECORD KEY             IS ����|�ی����
000830                                                       ����|�ی��Ҕԍ�
000840                             FILE STATUS            IS  ��ԃL�[
000850                             LOCK    MODE           IS  AUTOMATIC.
001720* ���я��󎚗p
001730     SELECT  ��ƃt�@�C���S  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001740                             ORGANIZATION             IS  INDEXED
001750                             ACCESS                   IS  DYNAMIC
001760                             RECORD      KEY          IS  ��S�|�{�p�a��N��
001770                                                          ��S�|���҃R�[�h
001780                                                          ��S�|�ی����
001790                             FILE        STATUS       IS  ��ԃL�[
001800                             LOCK        MODE         IS  AUTOMATIC.
001810*
001820     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
001830                             SYMBOLIC    DESTINATION  IS "PRT"
001840                             FORMAT                   IS  ��`�̖��o
001850                             GROUP                    IS  ���ڌQ���o
001860                             PROCESSING  MODE         IS  ������ʂo
001870                             UNIT        CONTROL      IS  �g������o
001880                             FILE        STATUS       IS  �ʒm���o.
001890******************************************************************
001900*                      DATA DIVISION                             *
001910******************************************************************
001920 DATA                    DIVISION.
001930 FILE                    SECTION.
001940*                           �m�q�k��  �R�Q�O�n
001950 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001960     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001970*                           �m�q�k��  �P�Q�W�n
001980 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001990     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002000*                           �m�q�k��  �P�Q�W�n
002010 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
002020     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
002060*                           �m�q�k��  �Q�T�U�n
002070 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
002080     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002090*                           �m�q�k��  �P�Q�W�n
002100 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002110     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
002150*                           �m�q�k��  �P�Q�W�n
002160 FD  �o�߃}�X�^          BLOCK   CONTAINS   1   RECORDS.
002170     COPY KEIKA          OF  XFDLIB  JOINING   �o   AS  PREFIX.
002180*                           �m�q�k��  �R�Q�O�n
002190 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
002200     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002560*                          �m�q�k��  1024�n
000340 FD  ��f�ҏ��Q�e        BLOCK   CONTAINS   1   RECORDS.
000350     COPY JUSINJ2          OF  XFDLIB  JOINING   ��Q   AS  PREFIX.
002210*                           �m�q�k��  �Q�T�U�n
002220 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
002230     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
002240*                           �m�q�k��  �P�Q�W�n
002250 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
002260     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002270*                           �m�q�k��  �P�Q�W�n
002280 FD  ���������e          BLOCK   CONTAINS   1   RECORDS.
002290     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
002300*                           �m�q�k��  �P�Q�W�n
002310 FD  �h�c�Ǘ��}�X�^          BLOCK   CONTAINS   1   RECORDS.
002320     COPY IDKANR    OF  XFDLIB  JOINING   �h�c��   AS  PREFIX.
002330*                           �m�q�k��  �Q�T�U�n
002340 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002350     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
002280*                           �m�q�k��  �P�Q�W�n
002290 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
002300     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
002390**
002400**
002410 FD  ��ƃt�@�C���S RECORD  CONTAINS 32 CHARACTERS.
002420 01  ��S�|���R�[�h.
002430     03  ��S�|���R�[�h�L�[.
002440         05  ��S�|�{�p�a��N��.
002450             07  ��S�|�{�p�a��            PIC 9.
002460             07  ��S�|�{�p�N              PIC 9(2).
002470             07  ��S�|�{�p��              PIC 9(2).
002480         05  ��S�|���҃R�[�h.
002490             07 ��S�|���Ҕԍ�             PIC 9(6).
002500             07 ��S�|�}��                 PIC X(1).
002510         05  ��S�|�ی����                PIC 9(2).
002520     03  ��S�|���R�[�h�f�[�^.
002530         05  ��S�|����                    PIC 9(4).
002540         05  FILLER                        PIC X(14).
002550*
002560*
002570 FD  ����t�@�C��.
002580     COPY YAZ6421P        OF  XMDLIB.
002590*----------------------------------------------------------------*
002600******************************************************************
002610*                WORKING-STORAGE SECTION                         *
002620******************************************************************
002630 WORKING-STORAGE         SECTION.
002640 01 �L�[����                           PIC X     VALUE SPACE.
002650 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
002660 01 �I���t���O                         PIC X(3)  VALUE SPACE.
002670 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
002680 01 �����t���O                         PIC X(3)  VALUE SPACE.
002690 01 �t�@�C����                         PIC N(6)  VALUE SPACE.
002700 01 ���Z�v�g�o�f�v                     PIC X(8)  VALUE SPACE.
002710 01 �O�a��v                           PIC 9     VALUE ZERO.
002720 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
002730 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002740 01 ���Ҕԍ��v                         PIC 9(6)  VALUE ZERO.
002750 01 �������̂v                         PIC N(6)  VALUE SPACE.
002760 01 ���ʖ��̂v                         PIC N(12) VALUE SPACE.
002770 01 ���ʒ��v                           PIC 9(2) VALUE 1.
001363 01 �S�p��                           PIC X(2)  VALUE X"8140".
001364 01 ���p��                           PIC X(2)  VALUE X"2020".
002780**
002790 01 �x���t���O                         PIC X(3) VALUE SPACE.
002800 01 �x���񐔂v                         PIC 9(4) VALUE ZERO.
002810 01 �x���b�m�s                         PIC 9(5) VALUE ZERO.
002820 01 �ő�o�^���v                       PIC 9 VALUE ZERO.
002830 01 �����A���o�^�v                     PIC 9 VALUE ZERO.
002840**
002850** ���������{��ϊ�
002860 01 �����v                             PIC 9(2).
002870 01 �����q REDEFINES �����v.
002880    03 �����v�P                        PIC X(1).
002890    03 �����v�Q                        PIC X(1).
002900*
002910 01 �����ԍ��v                         PIC 9.
002920 01 �����ԍ��q REDEFINES �����ԍ��v.
002930    03 �����ԍ��v�P                    PIC X.
002940*
002950 01 �S�p�����ԍ��v                     PIC N.
002960 01 �S�p�����ԍ��q REDEFINES �S�p�����ԍ��v.
002970    03 �S�p�����ԍ��v�P                PIC X(2).
002980*
002990 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
003000 01 �J�E���^�Q                         PIC 9(2)  VALUE ZERO.
003010*
003020* �ޔ�p
003030 01 �I���N�����v�s.
002980    03 �I���a��v�s                    PIC 9     VALUE ZERO.
003040    03 �I���N�v�s                      PIC 9(2)  VALUE ZERO.
003050    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
003060    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
003070* �������ޔ�p
003080 01 �����N�����v�s.
003090    03 �����a��v�s                    PIC 9     VALUE ZERO.
003100    03 �����N�v�s                      PIC 9(2)  VALUE ZERO.
003110    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003120    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003130* �������Z�����p
003140 01 �������Z�v�s.
003150    03 �������Z�J�E���g                PIC 9    VALUE ZERO.
003160    03 �ԍ��J�E���^                    PIC 9    VALUE ZERO.
003170    03 �������Z�W�c�v�s  OCCURS 3.
003180       05 �������Z�敪�v�s             PIC 9    VALUE ZERO.
003190       05 �������Z���v�s               PIC 9(2) VALUE ZERO.
003200       05 �������Z���v�s               PIC 9(2) VALUE ZERO.
003210    03 �������Z�W�c�m�v  OCCURS 3.
003220       05 ���Z��؂v                   PIC N(1) VALUE SPACE.
003230       05 ���Z���e�v                   PIC N(3) VALUE SPACE.
003240       05 �������Z���m�v�P             PIC N(1) VALUE SPACE.
003250       05 �������Z���m�v�Q             PIC N(1) VALUE SPACE.
003260       05 ���Œ�v                     PIC N(1) VALUE SPACE.
003270       05 �������Z���m�v�P             PIC N(1) VALUE SPACE.
003280       05 �������Z���m�v�Q             PIC N(1) VALUE SPACE.
003290       05 ���Œ�v                     PIC N(1) VALUE SPACE.
003300    03 �������Z�����P�v                PIC N(10) VALUE SPACE.
003310    03 �������Z�����Q�v                PIC N(10) VALUE SPACE.
003320    03 �������Z�����R�v                PIC N(10) VALUE SPACE.
003070    03 �������Z��؂v                  PIC X     VALUE SPACE.
003080    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003090    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003330* ���������p
003340 01 ���������v�s.
003350    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
003360    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
003370    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
003380    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
003390    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
003400    03 ���������i���o�[�v�s.
003410       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
003420    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
003430 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
003440 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
003450 01 ���������s�a�k.
003460    03 ���������R�[�h�s�a�k            OCCURS 9.
003470       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
003480       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
003490       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
003500 01 �����������e�v.
003510    03 �����������e�����v              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 �����������e�����w�v.
003630       05 �����������e�P�w�v           PIC X(70)  VALUE SPACE.
003640       05 �����������e�Q�w�v           PIC X(70)  VALUE SPACE.
003640       05 �����������e�R�w�v           PIC X(70)  VALUE SPACE.
003650       05 �����������e�S�w�v           PIC X(70)  VALUE SPACE.
003650       05 �����������e�T�w�v           PIC X(38)  VALUE SPACE.
003560*
003570*************
003580* ���ϔԍ��p
003590 01 ���ϘA�ԍ��W�c�v.
003600    03 ���ϘA�ԍ����v                  PIC X(14)  VALUE SPACE.
003610    03 ���ϘA�ԍ����m�v REDEFINES  ���ϘA�ԍ����v  PIC N(7).
003620    03 ���ϘA�ԍ��v                    PIC X(6)  VALUE SPACE.
003630    03 ���ϘA�ԍ��P�ʂv                PIC X(2)  VALUE SPACE.
003640    03 ���ϘA�ԍ��P�ʂm�v REDEFINES  ���ϘA�ԍ��P�ʂv  PIC N.
003650* ���q���ԍ��p
003660 01 ���q���ԍ��W�c�v.
003670    03 ���q���ԍ����v                  PIC X(8)  VALUE SPACE.
003680    03 ���q���ԍ����m�v REDEFINES  ���q���ԍ����v  PIC N(4).
003690    03 ���q���ԍ��v                    PIC X(6)  VALUE SPACE.
003700    03 ���q���ԍ��P�ʂv                PIC X(2)  VALUE SPACE.
003710    03 ���q���ԍ��P�ʂm�v REDEFINES  ���q���ԍ��P�ʂv  PIC N.
003720 01 �E�o�t���O                         PIC X(3)  VALUE SPACE.
003730*
003740* �ی��Ҕԍ�
003750 01 �ی��Ҕԍ���r�v                   PIC X(6)   VALUE SPACE.
003760*
003770** �O�������̂ݗp
003780 01 �����Č��t���O                     PIC X(3)  VALUE SPACE.
003790 01 �O���t���O                         PIC X(3)  VALUE SPACE.
003800*
003810 01 �v�Z�N�����v.
003820    03 �v�Z�a��v                      PIC 9(1)  VALUE ZERO.
003830    03 �v�Z�N�v                        PIC S9(2)  VALUE ZERO.
003840    03 �v�Z���v                        PIC S9(2)  VALUE ZERO.
003850    03 �v�Z���v                        PIC S9(2)  VALUE ZERO.
003860 01 �J�n�N�����Q�v.
003870    03 �J�n�a��Q�v                    PIC 9(1)  VALUE ZERO.
003880    03 �J�n�N�Q�v                      PIC 9(2)  VALUE ZERO.
003890    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
003900    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
003910    03 �J�n����N�v                    PIC S9(4) VALUE ZERO.
003920 01 �I���N�����Q�v.
003930    03 �I���a��Q�v                    PIC 9(1)  VALUE ZERO.
003940    03 �I���N�Q�v                      PIC 9(2)  VALUE ZERO.
003950    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
003960    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
003970    03 �I������N�v                    PIC S9(4) VALUE ZERO.
003980***
003990** ���������E�������R����敪�p
004000 01 ������������敪�v                 PIC 9 VALUE ZERO.
004010 01 �������R����敪�v                 PIC 9 VALUE ZERO.
004020*
004030** ���Z���i�̓��t�敪�p (0:�ŏI�ʉ@���A1:�������A9:�󎚂Ȃ�)
004040 01 ���Z�v�g���t�敪�v                 PIC 9 VALUE ZERO.
004050 01 ���Z�v�g���ғ��t�敪�v             PIC 9 VALUE ZERO.
004060*
004070** �������p
004080 01 �{�p����N�v                       PIC 9(4)  VALUE ZERO.
004090 01 ���v                               PIC 9(3)  VALUE ZERO.
004100 01 �]�v                               PIC 9(3)  VALUE ZERO.
004110*
004120** �}�Ԕ���p
004130 01 �J�n�f�Ó��蓮�敪�v               PIC 9    VALUE ZERO.
004140*
004150*
004160** �������Z�܂Ƃߗp
004170 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
004180 01 ������ʗ��̂v                     PIC N(4)  VALUE SPACE.
004190 01 ������ʗ��̂v�Q                   PIC N(4)  VALUE SPACE.
004200*
004210* ���Z�E�v�p( N(38)�Œ�j /
004220 01 �����̌o�߂v.
004230    03 �����̌o�ߍs�v                  PIC X(76) OCCURS 2 VALUE SPACE.
004240 01 �����̌o�߂m�v REDEFINES �����̌o�߂v.
004250    03 �����̌o�ߍs�m�v                PIC N(38) OCCURS 2.
004260*
004320*
004330* ������������敪
004340 01 ���Z������������敪�v             PIC 9    VALUE ZERO.
004440 01 ���Z�������R����敪�v             PIC 9    VALUE ZERO.
004350*
004351*
004352* �����̌o�ߌŒ�󎚗p�Ɏg�p
004353 01 �S�_�e�o�c�敪�v                   PIC 9     VALUE ZERO.
004354 01 �o�ߕ��ʐ����v                     PIC N(1)  VALUE SPACE.
      *
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
       01 �������q�b�l                       PIC X(200) VALUE SPACE.
004355*
      */�����p��̒ǉ�/2407
       01 �����p��v.
          03 �����p��b�l                    PIC X(280) VALUE SPACE.
          03 �����p��P�v�s                  PIC X(54)  VALUE SPACE.
          03 �����p��Q�v�s                  PIC X(54)  VALUE SPACE.
          03 �����p��R�v�s                  PIC X(54)  VALUE SPACE.
          03 �����p��S�v�s                  PIC X(54)  VALUE SPACE.
          03 �����p��T�v�s                  PIC X(54)  VALUE SPACE.
          03 �������v�q                      OCCURS 5.
             05 �������v�o                   PIC X(36)  VALUE SPACE.
          03 �����p��b�l�Q                  PIC X(280) VALUE SPACE.
       01 �����v                             PIC Z9     VALUE ZERO.
004730*
004770******************************
004780* �T����  �E�v����  �ҏW�p *
004790******************************
004800 01 ���ʂT�v.
004840   03 �����J�n�����T�v.
004850      05 �����J�n���T�v                PIC ZZ.
            05 ���b�l                        PIC X(2).
004870      05 �����J�n���T�v                PIC ZZ.
            05 ���b�l                        PIC X(2).
         03 ���ʂP�v                         PIC X(1).
004890   03 ��ÂT�v.
            05 ���ʂQ�v                      PIC X(1).
004900      05 ��ÒP���T�v                  PIC ZZZZ.
            05 ��Z�L���P�v                  PIC X(1).
004920      05 ��É񐔂T�v                  PIC ZZ.
            05 �C�R�[���P�v                  PIC X(1).
004940      05 ��×��T�v                    PIC ZZ,ZZZ.
         03 ���ʂR�v                         PIC X(1).
         03 ���Z�L���P�v                     PIC X(1).
         03 ���ʂS�v                         PIC X(1).
004960   03 ��㪖@�T�v.
            05 ��㪖@�P���T�v                PIC Z(2).
            05 ��Z�L���Q�v                  PIC X(1).
004970      05 ��㪖@�񐔂T�v                PIC ZZ.
            05 �C�R�[���Q�v                  PIC X(1).
004990      05 ��㪖@���T�v                  PIC ZZZZ.
         03 ���ʂT�v                         PIC X(1).
         03 ���Z�L���Q�v                     PIC X(1).
         03 ���ʂU�v                         PIC X(1).
005010   03 ��㪖@�T�v.
            05 ��㪖@�P���T�v                PIC Z(2).
            05 ��Z�L���R�v                  PIC X(1).
005020      05 ��㪖@�񐔂T�v                PIC ZZ.
            05 �C�R�[���R�v                  PIC X(1).
005040      05 ��㪖@���T�v                  PIC ZZZZ.
         03 ���ʂV�v                         PIC X(1).
         03 ���Z�L���R�v                     PIC X(1).
         03 ���ʂW�v                         PIC X(1).
005060   03 �d�ÂT�v.
            05 �d�ÒP���T�v                  PIC Z(2).
            05 ��Z�L���S�v                  PIC X(1).
005070      05 �d�É񐔂T�v                  PIC ZZ.
            05 �C�R�[���S�v                  PIC X(1).
005090      05 �d�×��T�v                    PIC ZZZZ.
            05 ���ʂX�v                      PIC X(1).
         03 ���ʂP�O�v                       PIC X(1).
         03 ��Z�L���T�v                     PIC X(1).
005130   03 �����ʗ��T�v                     PIC X(3).
         03 ��Z�L���U�v                     PIC X(1).
005170   03 �����������T�v                   PIC 9.9.
         03 �C�R�[���T�v                     PIC X(1).
005190   03 ���������v�T�v                   PIC ZZ,ZZZ.
004356*
004357*
004360****************
004370* �A�����ڑҔ� *
004380****************
004390*    ************
004400*    * ����L�[ *
004410*    ************
004420 01 �Ώۃf�[�^�v�q.
004430    03 �{�p�a��N���v�q.
004440       05 �{�p�a��v�q                  PIC 9(1)  VALUE ZERO.
004450       05 �{�p�N�v�q                    PIC 9(2)  VALUE ZERO.
004460       05 �{�p���v�q                    PIC 9(2)  VALUE ZERO.
004470    03 �ی���ʂv�q                     PIC 9(2)  VALUE ZERO.
004480    03 �ی��Ҕԍ��v�q                   PIC X(10) VALUE SPACE.
004490    03 �����ʂv�q                     PIC 9(2)  VALUE ZERO.
004500    03 ��p���S�Ҕԍ��v�q               PIC X(10) VALUE SPACE.
004510    03 ������ʂv�q                     PIC 9(2)  VALUE ZERO.
004520    03 ��p���S�Ҕԍ������v�q           PIC X(10) VALUE SPACE.
004530    03 �{�l�Ƒ��敪�v�q                 PIC 9(1)  VALUE ZERO.
004540    03 ���҃J�i�v�q                     PIC X(20) VALUE SPACE.
004550    03 ���҃R�[�h�v�q.
004560       05 ���Ҕԍ��v�q                  PIC 9(6)  VALUE ZERO.
004570       05 �}�Ԃv�q                      PIC X(1)  VALUE SPACE.
004580*    ************
004590*    * ������� *
004600*    ************
004610*    �����̗���
004620***********************
004630 01 �����P�v�q.
004640   03 �����v�q.
004650      05 ���S�����v�q               PIC 9(3)    VALUE ZERO.
004660      05 �������v�q                 PIC 9(5)    VALUE ZERO.
004670      05 �������Z���v�q             PIC 9(5)    VALUE ZERO.
         03 ���k���v�q                    PIC 9(4)    VALUE ZERO.
004680   03 �Č����v�q                    PIC 9(5)    VALUE ZERO.
004690   03 ���Âv�q.
004700      05 ���Ë����v�q               PIC 9(2)V9  VALUE ZERO.
004710      05 ���É񐔂v�q               PIC 9(2)    VALUE ZERO.
004720      05 ���×��v�q                 PIC 9(5)    VALUE ZERO.
004730      05 ���É��Z���v�q             PIC 9(5)    VALUE ZERO.
004740   03 �������q���Z���v�q            PIC 9(5)    VALUE ZERO.
004750   03 �{�p���񋟗��v�q            PIC 9(5)    VALUE ZERO.
004760   03 ���v�v�q                      PIC 9(6)    VALUE ZERO.
004770   03 �ꕔ���S���v�q                PIC 9(6)    VALUE ZERO.
004780   03 �������z�v�q                  PIC 9(6)    VALUE ZERO.
004790   03 ���t�����v�q                  PIC 9(1)    VALUE ZERO.
004800   03 �󋋎ҕ��S�z�v�q              PIC 9(6)    VALUE ZERO.
004810   03 �����������z�v�q              PIC 9(6)    VALUE ZERO.
004820*
004830* �������ʖ��̗���
004840***********************
004850 01 �����Q�v�q.
004860   03 ���񏈒u�v�q    OCCURS   9.
004870      05 ���񏈒u���v�q             PIC 9(5)    VALUE ZERO.
004880*
004890* �������̗���
004900***********************
004910 01 �����R�v�q.
004920**********
004930* �P���� *
004940**********
004950   03 ���ʂP�v�q.
004960      05 ��ÂP�v�q.
004970         07 ��ÒP���P�v�q              PIC 9(4)    VALUE ZERO.
004980         07 ��É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
004990         07 ��×��P�v�q                PIC 9(5)    VALUE ZERO.
005000      05 ��㪖@�P�v�q.
005010         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
005020         07 ��㪖@���P�v�q              PIC 9(4)    VALUE ZERO.
005030      05 ��㪖@�P�v�q.
005040         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
005050         07 ��㪖@���P�v�q              PIC 9(4)    VALUE ZERO.
005060      05 �d�ÂP�v�q.
005070         07 �d�É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
005080         07 �d�×��P�v�q                PIC 9(4)    VALUE ZERO.
005090      05 ���v�P�v�q                     PIC 9(6)    VALUE ZERO.
005100      05 �����������P�v�q               PIC 9(3)    VALUE ZERO.
005110      05 ���������v�P�v�q               PIC 9(6)    VALUE ZERO.
005120**********
005130* �Q���� *
005140**********
005150   03 ���ʂQ�v�q.
005160      05 ��ÂQ�v�q.
005170         07 ��ÒP���Q�v�q              PIC 9(4)    VALUE ZERO.
005180         07 ��É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
005190         07 ��×��Q�v�q                PIC 9(5)    VALUE ZERO.
005200      05 ��㪖@�Q�v�q.
005210         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
005220         07 ��㪖@���Q�v�q              PIC 9(4)    VALUE ZERO.
005230      05 ��㪖@�Q�v�q.
005240         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
005250         07 ��㪖@���Q�v�q              PIC 9(4)    VALUE ZERO.
005260      05 �d�ÂQ�v�q.
005270         07 �d�É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
005280         07 �d�×��Q�v�q                PIC 9(4)    VALUE ZERO.
005290      05 ���v�Q�v�q                     PIC 9(6)    VALUE ZERO.
005300      05 �����������Q�v�q               PIC 9(3)    VALUE ZERO.
005310      05 ���������v�Q�v�q               PIC 9(6)    VALUE ZERO.
005320******************
005330* �R���ʁ^�W�� *
005340******************
005350   03 ���ʂR�W�v�q.
005360      05 ��ÂR�W�v�q.
005370         07 ��ÒP���R�W�v�q              PIC 9(4)  VALUE ZERO.
005380         07 ��É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
005390         07 ��×��R�W�v�q                PIC 9(5)  VALUE ZERO.
005400      05 ��㪖@�R�W�v�q.
005410         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
005420         07 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
005430      05 ��㪖@�R�W�v�q.
005440         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
005450         07 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
005460      05 �d�ÂR�W�v�q.
005470         07 �d�É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
005480         07 �d�×��R�W�v�q                PIC 9(4)  VALUE ZERO.
005490      05 ���v�R�W�v�q                     PIC 9(6)  VALUE ZERO.
005500      05 �����ʍ����v�R�W�v�q             PIC 9(6)  VALUE ZERO.
005510      05 �����������R�W�v�q               PIC 9(3)  VALUE ZERO.
005520      05 ���������v�R�W�v�q               PIC 9(6)  VALUE ZERO.
005530******************
005540* �R���ʁ^�P�O�� *
005550******************
005560   03 ���ʂR�O�v�q.
005570      05 �����J�n�����R�O�v�q.
005580         07 �����J�n���R�O�v�q            PIC 9(2)  VALUE ZERO.
005590         07 �����J�n���R�O�v�q            PIC 9(2)  VALUE ZERO.
005600      05 ��ÂR�O�v�q.
005610         07 ��ÒP���R�O�v�q              PIC 9(4)  VALUE ZERO.
005620         07 ��É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
005630         07 ��×��R�O�v�q                PIC 9(5)  VALUE ZERO.
005640      05 ��㪖@�R�O�v�q.
005650         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
005660         07 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
005670      05 ��㪖@�R�O�v�q.
005680         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
005690         07 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
005700      05 �d�ÂR�O�v�q.
005710         07 �d�É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
005720         07 �d�×��R�O�v�q                PIC 9(4)  VALUE ZERO.
005730      05 ���v�R�O�v�q                     PIC 9(6)  VALUE ZERO.
005740      05 �����������R�O�v�q               PIC 9(3)  VALUE ZERO.
005750      05 ���������v�R�O�v�q               PIC 9(6)  VALUE ZERO.
005760****************
005770* �S���ʁ^�T�� *
005780****************
005790   03 ���ʂS�T�v�q.
005800      05 ��ÂS�T�v�q.
005810         07 ��ÒP���S�T�v�q              PIC 9(4)  VALUE ZERO.
005820         07 ��É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
005830         07 ��×��S�T�v�q                PIC 9(5)  VALUE ZERO.
005840      05 ��㪖@�S�T�v�q.
005850         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
005860         07 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
005870      05 ��㪖@�S�T�v�q.
005880         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
005890         07 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
005900      05 �d�ÂS�T�v�q.
005910         07 �d�É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
005920         07 �d�×��S�T�v�q                PIC 9(4)  VALUE ZERO.
005930      05 ���v�S�T�v�q                     PIC 9(6)  VALUE ZERO.
005940      05 �����ʍ����v�S�T�v�q             PIC 9(6)  VALUE ZERO.
005950      05 �����������S�T�v�q               PIC 9(3)  VALUE ZERO.
005960      05 ���������v�S�T�v�q               PIC 9(6)  VALUE ZERO.
005970****************
005980* �S���ʁ^�W�� *
005990****************
006000   03 ���ʂS�W�v�q.
006010      05 �����J�n�����S�W�v�q.
006020         07 �����J�n���S�W�v�q            PIC 9(2)  VALUE ZERO.
006030         07 �����J�n���S�W�v�q            PIC 9(2)  VALUE ZERO.
006040      05 ��ÂS�W�v�q.
006050         07 ��ÒP���S�W�v�q              PIC 9(4)  VALUE ZERO.
006060         07 ��É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
006070         07 ��×��S�W�v�q                PIC 9(5)  VALUE ZERO.
006080      05 ��㪖@�S�W�v�q.
006090         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
006100         07 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
006110      05 ��㪖@�S�W�v�q.
006120         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
006130         07 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
006140      05 �d�ÂS�W�v�q.
006150         07 �d�É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
006160         07 �d�×��S�W�v�q                PIC 9(4)  VALUE ZERO.
006170      05 ���v�S�W�v�q                     PIC 9(6)  VALUE ZERO.
006180      05 �����ʍ����v�S�W�v�q             PIC 9(6)  VALUE ZERO.
006190      05 �����������S�W�v�q               PIC 9(3)  VALUE ZERO.
006200      05 ���������v�S�W�v�q               PIC 9(6)  VALUE ZERO.
006210******************
006220* �S���ʁ^�P�O�� *
006230******************
006240   03 ���ʂS�O�v�q.
006250      05 �����J�n�����S�O�v�q.
006260         07 �����J�n���S�O�v�q            PIC 9(2)  VALUE ZERO.
006270         07 �����J�n���S�O�v�q            PIC 9(2)  VALUE ZERO.
006280      05 ��ÂS�O�v�q.
006290         07 ��ÒP���S�O�v�q              PIC 9(4)  VALUE ZERO.
006300         07 ��É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
006310         07 ��×��S�O�v�q                PIC 9(5)  VALUE ZERO.
006320      05 ��㪖@�S�O�v�q.
006330         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
006340         07 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
006350      05 ��㪖@�S�O�v�q.
006360         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
006370         07 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
006380      05 �d�ÂS�O�v�q.
006390         07 �d�É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
006400         07 �d�×��S�O�v�q                PIC 9(4)  VALUE ZERO.
006410      05 ���v�S�O�v�q                     PIC 9(6)  VALUE ZERO.
006420      05 �����������S�O�v�q               PIC 9(3)  VALUE ZERO.
006430      05 ���������v�S�O�v�q               PIC 9(6)  VALUE ZERO.
006440********************
006450* �T���ʁ^�Q�D�T�� *
006460********************
006470   03 ���ʂT�Q�v�q.
006480      05 ��ÂT�Q�v�q.
006490         07 ��ÒP���T�Q�v�q              PIC 9(4)  VALUE ZERO.
006500         07 ��É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
006510         07 ��×��T�Q�v�q                PIC 9(5)  VALUE ZERO.
006520      05 ��㪖@�T�Q�v�q.
006530         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
006540         07 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
006550      05 ��㪖@�T�Q�v�q.
006560         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
006570         07 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
006580      05 �d�ÂT�Q�v�q.
006590         07 �d�É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
006600         07 �d�×��T�Q�v�q                PIC 9(4)  VALUE ZERO.
006610      05 ���v�T�Q�v�q                     PIC 9(6)  VALUE ZERO.
006620      05 �����ʍ����v�T�Q�v�q             PIC 9(6)  VALUE ZERO.
006630      05 �����������T�Q�v�q               PIC 9(3)  VALUE ZERO.
006640      05 ���������v�T�Q�v�q               PIC 9(6)  VALUE ZERO.
006650****************
006660* �T���ʁ^�T�� *
006670****************
006680   03 ���ʂT�T�v�q.
006690      05 �����J�n�����T�T�v�q.
006700         07 �����J�n���T�T�v�q            PIC 9(2)  VALUE ZERO.
006710         07 �����J�n���T�T�v�q            PIC 9(2)  VALUE ZERO.
006720      05 ��ÂT�T�v�q.
006730         07 ��ÒP���T�T�v�q              PIC 9(4)  VALUE ZERO.
006740         07 ��É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
006750         07 ��×��T�T�v�q                PIC 9(5)  VALUE ZERO.
006760      05 ��㪖@�T�T�v�q.
006770         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
006780         07 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
006790      05 ��㪖@�T�T�v�q.
006800         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
006810         07 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
006820      05 �d�ÂT�T�v�q.
006830         07 �d�É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
006840         07 �d�×��T�T�v�q                PIC 9(4)  VALUE ZERO.
006850      05 ���v�T�T�v�q                     PIC 9(6)  VALUE ZERO.
006860      05 �����ʍ����v�T�T�v�q             PIC 9(6)  VALUE ZERO.
006870      05 �����������T�T�v�q               PIC 9(3)  VALUE ZERO.
006880      05 ���������v�T�T�v�q               PIC 9(6)  VALUE ZERO.
006890****************
006900* �T���ʁ^�W�� *
006910****************
006920   03 ���ʂT�W�v�q.
006930      05 �����J�n�����T�W�v�q.
006940         07 �����J�n���T�W�v�q            PIC 9(2)  VALUE ZERO.
006950         07 �����J�n���T�W�v�q            PIC 9(2)  VALUE ZERO.
006960      05 ��ÂT�W�v�q.
006970         07 ��ÒP���T�W�v�q              PIC 9(4)  VALUE ZERO.
006980         07 ��É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
006990         07 ��×��T�W�v�q                PIC 9(5)  VALUE ZERO.
007000      05 ��㪖@�T�W�v�q.
007010         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
007020         07 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
007030      05 ��㪖@�T�W�v�q.
007040         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
007050         07 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
007060      05 �d�ÂT�W�v�q.
007070         07 �d�É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
007080         07 �d�×��T�W�v�q                PIC 9(4)  VALUE ZERO.
007090      05 ���v�T�W�v�q                     PIC 9(6)  VALUE ZERO.
007100      05 �����ʍ����v�T�W�v�q             PIC 9(6)  VALUE ZERO.
007110      05 �����������T�W�v�q               PIC 9(3)  VALUE ZERO.
007120      05 ���������v�T�W�v�q               PIC 9(6)  VALUE ZERO.
007130******************
007140* �T���ʁ^�P�O�� *
007150******************
007160   03 ���ʂT�O�v�q.
007170      05 �����J�n�����T�O�v�q.
007180         07 �����J�n���T�O�v�q            PIC 9(2)  VALUE ZERO.
007190         07 �����J�n���T�O�v�q            PIC 9(2)  VALUE ZERO.
007200      05 ��ÂT�O�v�q.
007210         07 ��ÒP���T�O�v�q              PIC 9(4)  VALUE ZERO.
007220         07 ��É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
007230         07 ��×��T�O�v�q                PIC 9(5)  VALUE ZERO.
007240      05 ��㪖@�T�O�v�q.
007250         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
007260         07 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
007270      05 ��㪖@�T�O�v�q.
007280         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
007290         07 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
007300      05 �d�ÂT�O�v�q.
007310         07 �d�É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
007320         07 �d�×��T�O�v�q                PIC 9(4)  VALUE ZERO.
007330      05 ���v�T�O�v�q                     PIC 9(6)  VALUE ZERO.
007340      05 �����������T�O�v�q               PIC 9(3)  VALUE ZERO.
007350      05 ���������v�T�O�v�q               PIC 9(6)  VALUE ZERO.
008000*******************
008010*  ���׏����s���Z */202206
008020*******************
008030   03 ���׏����s���Z���v�q                PIC ZZZ   VALUE ZERO.
008030   03 ���׏����s���Z���v�q                PIC ZZ    VALUE ZERO.
007360*
007370**************
007380* �{�p����� *
007390**************
007400 01 �{�p�����v.
007410    03 �_���t�ԍ��v                    PIC X(16)  VALUE SPACE.
007420    03 �ڍ��t�����ԍ��v              PIC X(16)  VALUE SPACE.
007430    03 ��\�҃J�i�v                    PIC X(50)  VALUE SPACE.
007440    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
007450    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
          03 �s���{���i�h�r�v                PIC X(2)   VALUE SPACE.
007460    03 �{�p���Z���v.
007470       05 �{�p���Z���P�v               PIC X(50)  VALUE SPACE.
007480       05 �{�p���Z���Q�v               PIC X(50)  VALUE SPACE.
007490    03 �{�p���X�֔ԍ��v.
007500       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
007510       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
007520    03 �{�p���d�b�ԍ��v                PIC X(15)  VALUE SPACE.
007530    03 ��z���󗝔ԍ��v                PIC X(15)  VALUE SPACE.
007540    03 �󗝔N�����v.
007350       05 �󗝘a��v                   PIC 9      VALUE ZERO.
007550       05 �󗝔N�v                     PIC 9(2)   VALUE ZERO.
007560       05 �󗝌��v                     PIC 9(2)   VALUE ZERO.
007570       05 �󗝓��v                     PIC 9(2)   VALUE ZERO.
007580    03 �ŏI�ʉ@�N�����v.
007390       05 �ŏI�ʉ@�a��v               PIC 9      VALUE ZERO.
007590       05 �ŏI�ʉ@�N�v                 PIC 9(2)   VALUE ZERO.
007600       05 �ŏI�ʉ@���v                 PIC 9(2)   VALUE ZERO.
007610       05 �ŏI�ʉ@���v                 PIC 9(2)   VALUE ZERO.
007620    03 �_���t�N�����v.
007430       05 �_���t�a��v                 PIC 9      VALUE ZERO.
007630       05 �_���t�N�v                   PIC 9(2)   VALUE ZERO.
007640       05 �_���t���v                   PIC 9(2)   VALUE ZERO.
007650       05 �_���t���v                   PIC 9(2)   VALUE ZERO.
007660    03 ���҈ϔC�N�����v.
007470       05 ���҈ϔC�a��v               PIC 9      VALUE ZERO.
007670       05 ���҈ϔC�N�v                 PIC 9(2)   VALUE ZERO.
007680       05 ���҈ϔC���v                 PIC 9(2)   VALUE ZERO.
007690       05 ���҈ϔC���v                 PIC 9(2)   VALUE ZERO.
007700    03 �������v.
007710        05 ������s���v.
007720           07 ������s���P�v         PIC X(10)  VALUE SPACE.
007730           07 ������s���Q�v         PIC X(10)  VALUE SPACE.
007740           07 FILLER                   PIC X(20)  VALUE SPACE.
007750        05 ������s�x�X���v.
007760           07 ������s�x�X���P�v     PIC X(10)  VALUE SPACE.
007770           07 ������s�x�X���Q�v     PIC X(10)  VALUE SPACE.
007780           07 FILLER                   PIC X(20)  VALUE SPACE.
007790        05 �a����ʂv                  PIC 9(1)   VALUE ZERO.
007800        05 �����ԍ��v                  PIC X(10)  VALUE SPACE.
007810        05 �������`�l�v                PIC X(40)  VALUE SPACE.
007820        05 �������`�l�J�i�v            PIC X(40)  VALUE SPACE.
007830        05 �a����ʃR�����g�v          PIC N(3)   VALUE SPACE.
007840        05 �a����ʃR�����g�w�v        PIC X(4)   VALUE SPACE.
007850*
007860    03 ���{�p�h�c�v                    PIC X(15)  VALUE SPACE.
007870    03 �s�����{�p�h�c�v                PIC X(15)  VALUE SPACE.
007880    03 ���ϔԍ��v                      PIC X(28)  VALUE SPACE.
007880    03 �n���ϔԍ��v                    PIC X(28)  VALUE SPACE.
007890**************
007900* ��f�ҏ�� *
007910**************
007920 01 ��f�ҏ��v.
          03 �{�p�a��v                      PIC 9(1)   VALUE ZERO.
007930    03 �{�p�N���v.
007940       05 �{�p�N�v                     PIC 9(2)   VALUE ZERO.
007950       05 �{�p���v                     PIC 9(2)   VALUE ZERO.
007960*    03 �L���v                          PIC N(12)  VALUE SPACE.
007570    03 �L���v.
007580       05 ����L���v                   PIC N(12)  VALUE SPACE.
          03 �L���ԍ��v.
             05 �L���ԍ��w�v                 PIC X(40) VALUE SPACE.
007970*    03 �ԍ��v                          PIC X(30)  VALUE SPACE.
008770    03 �ԍ��v.
008780       05 ����ԍ��v                   PIC X(15)  VALUE SPACE.
008790       05 FILLER                       PIC X(15)  VALUE SPACE.
007980    03 �ی��Ҕԍ��v.
007990       05 ����ی��Ҕԍ��v             PIC X(8)   VALUE SPACE.
008000       05 FILLER                       PIC X(2)   VALUE SPACE.
008010    03 �s�����ԍ��v.
008020       05 ����s�����ԍ��v             PIC X(8)   VALUE SPACE.
008030       05 FILLER                       PIC X(2)   VALUE SPACE.
          03 �󋋎Ҕԍ��v.
             05 ����󋋎Ҕԍ��v             PIC X(7)  VALUE SPACE.
             05 ����󋋎Ҕԍ��Q�v           PIC X(8)  VALUE SPACE.
008040    03 �����於�̂v.
008050       05 �����於�̂P�v               PIC X(40)  VALUE SPACE.
008060       05 �����於�̂Q�v               PIC X(40)  VALUE SPACE.
008070    03 �ی���ʂv                      PIC 9(2)   VALUE ZERO.
007390    03 �ی���ʃ`�F�b�N�v.
007400       05 �Еۃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
007410       05 �D���`�F�b�N�v               PIC N(1)  VALUE SPACE.
007420       05 �g���`�F�b�N�v               PIC N(1)  VALUE SPACE.
007430       05 ���ۃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ���σ`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
             05 �ސE�`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ����`�F�b�N�v               PIC N(1)  VALUE SPACE.
          03 �{�l�`�F�b�N�v                  PIC N(1)   VALUE SPACE.
          03 �Ƒ��`�F�b�N�v                  PIC N(1)   VALUE SPACE.
          03 �P�ƃ`�F�b�N�v                  PIC N(1)   VALUE SPACE.
          03 �Q���`�F�b�N�v                  PIC N(1)   VALUE SPACE.
          03 ����`�F�b�N�v                  PIC N(1)   VALUE SPACE.
          03 ���V�`�F�b�N�v                  PIC N(1)   VALUE SPACE.
          03 �U�΃`�F�b�N�v                  PIC N(1)   VALUE SPACE.
007750    03 ���t�����`�F�b�N�v.
007760       05 �V���`�F�b�N�v               PIC N(1)  VALUE SPACE.
007770       05 �W���`�F�b�N�v               PIC N(1)  VALUE SPACE.
007780       05 �X���`�F�b�N�v               PIC N(1)  VALUE SPACE.
007790       05 �P�O���`�F�b�N�v             PIC N(1)  VALUE SPACE.
008080    03 ��ی��ҏ��v.
008090       05 ��ی��҃J�i�v               PIC X(50)  VALUE SPACE.
008100       05 ��ی��Ҏ����v               PIC X(50)  VALUE SPACE.
008110       05 �X�֔ԍ��v.
008120          07 �X�֔ԍ��P�v              PIC X(3)   VALUE SPACE.
008130          07 �X�֔ԍ��Q�v              PIC X(4)   VALUE SPACE.
008990       05 �d�b�ԍ��v                   PIC X(35)  VALUE SPACE.
008140       05 ��ی��ҏZ���v.
008150          07 ��ی��ҏZ���P�v          PIC X(50)  VALUE SPACE.
008160          07 ��ی��ҏZ���Q�v          PIC X(50)  VALUE SPACE.
008170    03 ���ҏ��v.
             05 ���ҏZ���v.
008370          07 ���ҏZ���P�v              PIC X(50)  VALUE SPACE.
008380          07 ���ҏZ���Q�v              PIC X(50)  VALUE SPACE.
008180       05 ���҃J�i�v                   PIC X(50)  VALUE SPACE.
008190       05 ���Ҏ����v                   PIC X(50)  VALUE SPACE.
008200       05 ���ʃ`�F�b�N�v.
008210          07 �j�`�F�b�N�v              PIC N(1)  VALUE SPACE.
008220          07 ���`�F�b�N�v              PIC N(1)  VALUE SPACE.
008230       05 ���Ґ��ʂv.
008240          07 ���ʂv                    PIC N(1)  VALUE SPACE.
008250       05 �a��`�F�b�N�v.
008260          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008270          07 �吳�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008280          07 ���a�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008290          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008300          07 �����v                    PIC N(2)  VALUE SPACE.
      */�����C��/������20190426
008210          07 �ߘa�`�F�b�N�v            PIC N(1)  VALUE SPACE.
                07 �ߘa�b�l�v                PIC X(4)  VALUE SPACE.
009110*          07 �����v                    PIC N(2)  VALUE SPACE.
      */�����C��/������20190426
008310       05 ���ҔN�v                     PIC 9(2)  VALUE ZERO.
008320       05 ���Ҍ��v                     PIC 9(2)  VALUE ZERO.
008330       05 ���ғ��v                     PIC 9(2)  VALUE ZERO.
008340       05 �����v.
008350          07 ��������v                PIC N(4)  VALUE SPACE.
008360          07 FILLER                    PIC X(4)  VALUE SPACE.
008370*
008380*       05 ���������v                   PIC X(80) OCCURS 29 VALUE SPACE.
      */���p�Ή�/110421
             05 ���������v OCCURS 29.
                07 ���������w�v              PIC X(70)  VALUE SPACE.
       01 ���������P���v.
          03 ���������P���v�q                OCCURS 7.
             05 ���������P���v�o             PIC X(70) VALUE SPACE.
008390*
      */�����󂪈������Ȃ��ꍇ�����遫����/20201006
008400*    03 �ی���ʖ��̂v                  PIC N(1)  VALUE SPACE.
008410*    03 ������v                        PIC N(1)  VALUE SPACE.
008420*    03 ���ʃR�����g�v                  PIC X(16) VALUE SPACE.
008400 01 �ی���ʖ��̂v                     PIC N(1)  VALUE SPACE.
008410 01 ������v                           PIC N(1)  VALUE SPACE.
008420 01 ���ʃR�����g�v                     PIC X(16) VALUE SPACE.
      */�����󂪈������Ȃ��ꍇ�����遪����/20201006
008430*
008440****************
008450* �����f�[�^�e *
008460****************
008470 01 �������v.
008480    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
008490    03 ���ʏ��v  OCCURS   9.
008500       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
008510       05 ���ʃR�[�h�v.
008520          07 ������ʂv                PIC 9(2)  VALUE ZERO.
008530          07 ���ʂv                    PIC 9(2)  VALUE ZERO.
008540          07 ���E�敪�v                PIC 9(1)  VALUE ZERO.
008550          07 �����ʒu�ԍ��v            PIC 9(2)  VALUE ZERO.
008560       05 �������v                     PIC N(18) VALUE SPACE.
008570       05 �����N�����v.
008580          07 �����N�v                  PIC 9(2)  VALUE ZERO.
008590          07 �������v                  PIC 9(2)  VALUE ZERO.
008600          07 �������v                  PIC 9(2)  VALUE ZERO.
008610       05 �����N�����v.
008620          07 �����N�v                  PIC 9(2)  VALUE ZERO.
008630          07 �������v                  PIC 9(2)  VALUE ZERO.
008640          07 �������v                  PIC 9(2)  VALUE ZERO.
008650       05 �J�n�N�����v.
008660          07 �J�n�N�v                  PIC 9(2)  VALUE ZERO.
008670          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
008680          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
008690       05 �I���N�����v.
002980          07 �I���a��v                PIC 9     VALUE ZERO.
008700          07 �I���N�v                  PIC 9(2)  VALUE ZERO.
008710          07 �I�����v                  PIC 9(2)  VALUE ZERO.
008720          07 �I�����v                  PIC 9(2)  VALUE ZERO.
008730       05 �������v                     PIC 9(2)  VALUE ZERO.
008740       05 �]�A�敪�v                   PIC 9(1)  VALUE ZERO.
008750       05 �]�A�敪�`�F�b�N�v.
008760          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008770          07 ���~�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008780          07 �]��`�F�b�N�v            PIC N(1)  VALUE SPACE.
008790       05 �J�n�N�����擾�t���O         PIC X(3)  VALUE SPACE.
008800       05 ���ʋ�؂v                   PIC X(1)  VALUE SPACE.
008810       05 �o�ߗ��̂v.
008820          07 ����o�ߗ��̂v            PIC N(6)  VALUE SPACE.
008830          07 FILLER                    PIC X(2)  VALUE SPACE.
008840    03 �o�ߕ��ʂv                      PIC N(1)  VALUE SPACE.
008850    03 �V�K�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008860    03 �p���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008870*
008880************
008890* ������� *
008900************
008910 01 �������v.
008920    03 �������Z�v.
008930       05 ���ԊO�`�F�b�N�v                PIC N(1) VALUE SPACE.
008940       05 �x���`�F�b�N�v                  PIC N(1) VALUE SPACE.
008950       05 �[��`�F�b�N�v                  PIC N(1) VALUE SPACE.
008960    03 ���É��Z�v.
008970       05 ��ԃ`�F�b�N�v                  PIC N(1) VALUE SPACE.
008980       05 ��H�`�F�b�N�v                  PIC N(1) VALUE SPACE.
008990       05 �\���J��`�F�b�N�v              PIC N(1) VALUE SPACE.
009000    03 �������q�`�F�b�N�v.
009010       05 ��`�F�b�N�v                    PIC N(1) VALUE SPACE.
009020       05 ���`�F�b�N�v                    PIC N(1) VALUE SPACE.
009030       05 ���`�F�b�N�v                    PIC N(1) VALUE SPACE.
009040    03 ���v�v                             PIC 9(7) VALUE ZERO.
009050    03 ���񏈒u�����v�v                   PIC 9(6) VALUE ZERO.
009060    03 ���񏈒u���`�F�b�N�v.
009070       05 �������`�F�b�N�v                PIC N(1) VALUE SPACE.
009080       05 �Œ藿�`�F�b�N�v                PIC N(1) VALUE SPACE.
009090       05 �{�×��`�F�b�N�v                PIC N(1) VALUE SPACE.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
          03 �����񐔂v                         PIC 9(2)  VALUE ZERO.
          03 �^���񐔂v                         PIC 9(1)  VALUE ZERO.
          03 �^�����v                           PIC 9(5)  VALUE ZERO.
009100************
009110* ���l��� *
009120************
010000 01 ���l���v.
010010    03 �K�p�P�v                        PIC N(48) VALUE SPACE.
010020    03 �K�p�Q�v                        PIC X(40) VALUE SPACE.
009250*
009260    03 �o�߃R�����g�v                  PIC N(60) VALUE SPACE.
009270*
009280*****************
009290* ���Z�v�g���я� *
009300*****************
009310 01 ���ԌŒ�v                         PIC N(1) VALUE SPACE.
009320 01 ���Ԃv                             PIC 9(4) VALUE ZERO.
009330*
003720*--- ���S���t�����p ---*
003730 01 ���S�����v                         PIC 9(2)  VALUE ZERO.
003740 01 ���t�����v                         PIC 9(2)  VALUE ZERO.
      *
       01 �E�v�{�p���v                       PIC X(100) VALUE SPACE.
       01 �{�p���v.
          03 �{�p���Q�v                      PIC X(1)  VALUE SPACE.
          03 �{�p���P�v                      PIC X(1)  VALUE SPACE.
009340*******************************************************************
009350 01 �������.
009360     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
009370     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
009380     03 ������ʂo                     PIC X(2) VALUE SPACE.
009390     03 �g������o.
009400         05 �[������o.
009410             07 �ړ������o             PIC X(1) VALUE SPACE.
009420             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
009430         05 �ڍא���o                 PIC X(2) VALUE SPACE.
009440     03 �ʒm���o                     PIC X(2) VALUE SPACE.
009450     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
009460*
009470 01 �v�Z�@����N�v                     PIC 9(2) VALUE ZERO.
009480* ���t�v�n�q�j
009490 01 �a��I���N�v                       PIC 9(4) VALUE ZERO.
009500 01 �v�Z�@����.
009510    03 �v�Z�@����N                    PIC 9(4) VALUE ZERO.
009520    03 �v�Z�@�����                  PIC 9(4) VALUE ZERO.
009530 01 �v�Z�@����q REDEFINES �v�Z�@����.
009540    03 �v�Z�@���I                      PIC 9(2).
009550    03 �v�Z�@���t                      PIC 9(6).
009560    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
009570       05 �v�Z�@�N��                   PIC 9(4).
009580       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
009590         07 �v�Z�@�N                   PIC 9(2).
009600         07 �v�Z�@��                   PIC 9(2).
009610       05 �v�Z�@��                     PIC 9(2).
009620*
      * C �A�g�p
       01  �����P�v        PIC X(4096).
       01  �����Q�v        PIC X(512).
       01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
014774*
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
009630******************************************************************
009640*                          �A������                              *
009650******************************************************************
009660**  ��ʓ��̓f�[�^
010430*
       01 �A���|�v���r���[ IS EXTERNAL.
          03 �A���|�v���r���[�敪          PIC 9.
010440*
009670 01 �A���|���̓f�[�^�ϔC��� IS EXTERNAL.
009680    03 �A���|�ϔC���                     PIC 9.
       01 �A���|���̓f�[�^�d�b��� IS EXTERNAL.
          03 �A���|�d�b���                     PIC 9.
009690*
009700** �R�J����������
009710 01 �A���ԁ|�L�[ IS EXTERNAL.
009720    03 �A���ԁ|�{�p�N��.
009730       05 �A���ԁ|�{�p�a��               PIC 9.
009740       05 �A���ԁ|�{�p�N                 PIC 9(2).
009750       05 �A���ԁ|�{�p��                 PIC 9(2).
009760    03  �A���ԁ|���҃R�[�h.
009770       05 �A���ԁ|���Ҕԍ�               PIC 9(6).
009780       05 �A���ԁ|�}��                   PIC X.
009790    03 �A���ԁ|�Ώۃt���O                PIC X(3).
009800    03 �A���ԁ|���Ԍ��v.
009810       05 �A���ԁ|���Ԃv                 PIC 9(2) OCCURS 9.
009820************
009830* ����L�[ *
009840************
009850*
009860*
009870 01 �A����|�Ώۃf�[�^ IS EXTERNAL.
009880    03 �A����|�{�p�N����.
009890       05 �A����|�{�p�a��                  PIC 9(1).
009900       05 �A����|�{�p�N                    PIC 9(2).
009910       05 �A����|�{�p��                    PIC 9(2).
009920    03 �A����|���҃R�[�h.
009930       05 �A����|���Ҕԍ�                  PIC 9(6).
009940       05 �A����|�}��                      PIC X(1).
009950    03 �A����|�ی����                     PIC 9(2).
009960    03 �A����|�ی��Ҕԍ�                   PIC X(10).
009970    03 �A����|������                     PIC 9(2).
009980    03 �A����|��p���S�Ҕԍ�               PIC X(10).
009990    03 �A����|�������                     PIC 9(2).
010000    03 �A����|��p���S�Ҕԍ�����           PIC X(10).
010010    03 �A����|���҃J�i                     PIC X(20).
010020    03 �A����|�{�l�Ƒ��敪                 PIC 9(1).
010030*
013460 01 �A���|�L�[ IS EXTERNAL.
013470    03 �A���|�ی����                  PIC 9(2).
013480*
014230************************
014240* �E�v���Z�b�g     *
014250************************
014260 01 �A�E���|�L�[ IS EXTERNAL.
014270    03 �A�E���|�{�p�N��.
014280       05 �A�E���|�{�p�a��               PIC 9.
014290       05 �A�E���|�{�p�N                 PIC 9(2).
014300       05 �A�E���|�{�p��                 PIC 9(2).
014310    03  �A�E���|���҃R�[�h.
014320       05 �A�E���|���Ҕԍ�               PIC 9(6).
014330       05 �A�E���|�}��                   PIC X.
014340    03 �A�E���|������                    PIC 9(2).
014350    03 �A�E���|�E�v��                    PIC X(126) OCCURS 30.
014340    03 �A�E���|�����敪                  PIC 9(1).
013620*
013630************************
013640* �������Z�܂Ƃ�
013650************************
013660 01 �A���Z�܂Ƃ߁|�L�[ IS EXTERNAL.
013670    03 �A���Z�܂Ƃ߁|�{�p�a��N��.
013680       05 �A���Z�܂Ƃ߁|�{�p�a��               PIC 9.
013690       05 �A���Z�܂Ƃ߁|�{�p�N��.
013700          07 �A���Z�܂Ƃ߁|�{�p�N              PIC 9(2).
013710          07 �A���Z�܂Ƃ߁|�{�p��              PIC 9(2).
013720    03 �A���Z�܂Ƃ߁|���҃R�[�h.
013730       05 �A���Z�܂Ƃ߁|���Ҕԍ�               PIC 9(6).
013740       05 �A���Z�܂Ƃ߁|�}��                   PIC X(1).
013750**-------------------------------------------------------**
013760*   1:�������Z�v�g�Ȃ��̖{�̂܂Ƃ߂̔���
013770*   2:���l�E���p�̎Еۏ������Z���̔���
013780    03 �A���Z�܂Ƃ߁|����敪                  PIC 9.
013790**-------------------------------------------------------**
013800*  / OUT /�@ 0:�ΏۊO�A1:�Ώ�
013810    03 �A���Z�܂Ƃ߁|���茋��                  PIC 9.
013820**
013821*
013822*************
013823* ��������
013824*************
013825 01 �A�������́|�L�[ IS EXTERNAL.
013826    03 �A�������́|�������             PIC 9(2).
013827    03 �A�������́|��p���S�Ҕԍ�����   PIC X(10).
013828*   / OUT /
013829    03 �A�������́|���̏W�c.
013830       05 �A�������́|�P����            PIC N.
013831       05 �A�������́|����              PIC N(4).
013832       05 �A�������́|��������          PIC N(10).
013833*
      * �Í������p
       01 �A�Í������|�Í���� IS EXTERNAL.
          03 �A�Í������|���͏��.
             05 �A�Í������|�L��               PIC X(24).
             05 �A�Í������|�ԍ�               PIC X(30).
             05 �A�Í������|�Í�������.
                07 �A�Í������|�Í����Ҕԍ�    PIC X(6).
                07 �A�Í������|�Í�����L��    PIC X.
                07 �A�Í������|�Í�����ԍ�    PIC X.
                07 �A�Í������|�Í��L��        PIC X(24).
                07 �A�Í������|�Í��ԍ�        PIC X(30).
          03 �A�Í������|�o�͏��.
             05 �A�Í������|���������L��       PIC X(24).
             05 �A�Í������|���������ԍ�       PIC X(30).
      * 
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
       01 �A���^�|�L�[ IS EXTERNAL.
          03 �A���^�|�{�p�a��N��.
             05 �A���^�|�{�p�a��                  PIC 9(1).
             05 �A���^�|�{�p�N��.
                07 �A���^�|�{�p�N                 PIC 9(2).
                07 �A���^�|�{�p��                 PIC 9(2).
          03 �A���^�|���҃R�[�h.
             05 �A���^�|���Ҕԍ�                  PIC 9(6).
             05 �A���^�|�}��                      PIC X(1).
          03 �A���^�|�ی����                     PIC 9(2).
          03 �A���^�|��R�[�h                     PIC 9(2).
          03 �A���^�|�p�����                     PIC 9(1).
          03 �A���^�|�������q.
             05 �A���^�|�������q�b�l              PIC X(200).
             05 �A���^�|�������q����              OCCURS 5.
                07 �A���^�|�������q�a��N����     OCCURS 3.
                   09 �A���^�|�������q�a��N��.
                      11 �A���^�|�������q�a��     PIC 9(1).
                      11 �A���^�|�������q�N��.
                         13 �A���^�|�������q�N    PIC 9(2).
                         13 �A���^�|�������q��    PIC 9(2).
                   09 �A���^�|�������q��          PIC 9(2).
          03 �A���^�|�^�����.
             05 �A���^�|�^����Âb�l              PIC X(100).
             05 �A���^�|�^����                    PIC 9(2)    OCCURS 5.
014761*
014762************************
014763* ���Z���������������
014764************************
014765 01 �A���Z������|�L�[ IS EXTERNAL.
014766    03 �A���Z������|�{�p�N��.
014767       05 �A���Z������|�{�p�a��               PIC 9.
014768       05 �A���Z������|�{�p�N                 PIC 9(2).
014769       05 �A���Z������|�{�p��                 PIC 9(2).
014770    03  �A���Z������|���҃R�[�h.
014771       05 �A���Z������|���Ҕԍ�               PIC 9(6).
014772       05 �A���Z������|�}��                   PIC X.
014773    03 �A���Z������|�Ώۃt���O                PIC X(3).
014774*
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
013834*
013835******************************************************************
013840*                      PROCEDURE  DIVISION                       *
013850******************************************************************
013860 PROCEDURE               DIVISION.
013870************
013880*           *
013890* ��������   *
013900*           *
013910************
002570     PERFORM �v�����^�t�@�C���쐬.
013920     PERFORM ������.
013930     PERFORM ������擾.
013940************
013950*           *
013960* �又��     *
013970*           *
013980************
013990* ���
014000     PERFORM �A�����ڑҔ�.
014010     PERFORM ����Z�b�g.
014020     PERFORM �������.
014030************
014040*           *
014050* �I������   *
014060*           *
014070************
014080     PERFORM ��f�҈���敪�X�V.
014090     PERFORM �I������.
014100*     PERFORM �x������.
014110     MOVE ZERO  TO PROGRAM-STATUS.
014120     EXIT PROGRAM.
014130*
014140*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
           MOVE "RECE"                TO �g�A����o�q�s�e�|�p�����.
002970*   �g�p����v�����^�t�@�C�����Z�b�g
002971     MOVE "PRTF002"             TO �g�A�o�q�s�e�|�t�@�C����.
002972*
002973*   �g�p���钠�[�v���O�������Z�b�g
002974     MOVE "YAZ6421"             TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪  TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014150*================================================================*
014160 ������ SECTION.
014170*
014180     PERFORM �t�@�C���I�[�v��.
014190*    /* ���ݓ��t�擾 */
014200     ACCEPT �v�Z�@���t FROM DATE.
014210*    /* 1980�`2079�N�̊ԂŐݒ� */
014220     IF ( �v�Z�@�N > 80 )
014230         MOVE 19 TO �v�Z�@���I
014240     ELSE
014250         MOVE 20 TO �v�Z�@���I
014260     END-IF.
014270     PERFORM �J�����g�����擾.
014280     PERFORM �a��I���N�擾.
014290     COMPUTE �v�Z�@����N�v = �v�Z�@����N - 1988.
014300*================================================================*
014310 �J�����g�����擾 SECTION.
014320*
014330     MOVE ZEROS TO ���|����敪.
014340     READ ������}�X�^
014350     NOT INVALID KEY
014360         MOVE ���|�J�����g����         TO �J�����g�����v
014370         MOVE ���|���Z������������敪 TO ������������敪�v
014380         MOVE ���|���Z�������R����敪 TO �������R����敪�v
014390         MOVE ���|���Z�v�g���t�敪     TO ���Z�v�g���t�敪�v
014400         MOVE ���|���Z�v�g���ғ��t�敪 TO ���Z�v�g���ғ��t�敪�v
014401         MOVE ���|�S�_�e�o�c�敪       TO �S�_�e�o�c�敪�v
014410     END-READ.
014420*
014430*================================================================*
014440 �a��I���N�擾 SECTION.
014450*
014460*     DISPLAY NC"�J�����g�����v"  �J�����g�����v UPON MSGBOX.
014470     MOVE �J�����g�����v TO ���|�����敪.
014480     READ �����}�X�^
014490     INVALID KEY
014500         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
014510         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
014520                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014530         ACCEPT  �L�[���� FROM CONS
014540         PERFORM �I������
014550         EXIT PROGRAM
014560     NOT INVALID KEY
014570         COMPUTE �O�a��v = �J�����g�����v - 1
014580         MOVE �O�a��v TO ���|�����敪
014590         READ �����}�X�^
014600         INVALID KEY
014610             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
014620             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
014630                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014640             ACCEPT  �L�[���� FROM CONS
014650             PERFORM �I������
014660             EXIT PROGRAM
014670         NOT INVALID KEY
014680             MOVE ���|�I������N TO �a��I���N�v
014690         END-READ
014700     END-READ.
014710*
014720*================================================================*
014730 �t�@�C���I�[�v�� SECTION.
014740*
014750     OPEN INPUT   �ی��҃}�X�^
014760         MOVE NC"�ی���" TO �t�@�C����.
014770         PERFORM �I�[�v���`�F�b�N.
014780     OPEN INPUT   �����}�X�^
014790         MOVE NC"����" TO �t�@�C����.
014800         PERFORM �I�[�v���`�F�b�N.
014810     OPEN INPUT   ���̃}�X�^
014820         MOVE NC"����" TO �t�@�C����.
014830         PERFORM �I�[�v���`�F�b�N.
007560     OPEN INPUT   ���Z�v�g�e
007570         MOVE NC"���Z" TO �t�@�C����.
007580         PERFORM �I�[�v���`�F�b�N.
014870     OPEN INPUT   ������}�X�^
014880         MOVE NC"������" TO �t�@�C����.
014890         PERFORM �I�[�v���`�F�b�N.
014900     OPEN INPUT   �{�p�����}�X�^
014910         MOVE NC"�{��" TO �t�@�C����.
014920         PERFORM �I�[�v���`�F�b�N.
014960     OPEN INPUT   �o�߃}�X�^
014970         MOVE NC"�o��" TO �t�@�C����.
014980         PERFORM �I�[�v���`�F�b�N.
014990     OPEN INPUT   �{�p�L�^�e.
015000         MOVE NC"�{�L�e" TO �t�@�C����.
015010         PERFORM �I�[�v���`�F�b�N.
015020     OPEN INPUT   �����f�[�^�e.
015030         MOVE NC"����" TO �t�@�C����.
015040         PERFORM �I�[�v���`�F�b�N.
015050     OPEN INPUT   ���������e.
015060         MOVE NC"��������" TO �t�@�C����.
015070         PERFORM �I�[�v���`�F�b�N.
015080     OPEN INPUT   �h�c�Ǘ��}�X�^
015090         MOVE NC"�h�c" TO �t�@�C����.
015100         PERFORM �I�[�v���`�F�b�N.
015110     OPEN INPUT �s�����}�X�^.
015120         MOVE NC"�s����" TO �t�@�C����.
015130         PERFORM �I�[�v���`�F�b�N.
015560     OPEN INPUT   ��f�ҏ��Q�e.
015570         MOVE NC"��f�ҏ��Q�e" TO �t�@�C����.
015580         PERFORM �I�[�v���`�F�b�N.
015170     OPEN INPUT  ��ƃt�@�C���S.
015170         IF ( ��ԃL�[  NOT =  "00" )
015060            OPEN OUTPUT  ��ƃt�@�C���S
                  CLOSE ��ƃt�@�C���S
015060            OPEN INPUT  ��ƃt�@�C���S
               END-IF.
015200     OPEN I-O   ��f�ҏ��e.
015210         MOVE NC"���" TO �t�@�C����.
015220         PERFORM �I�[�v���`�F�b�N.
015230     OPEN I-O   ����t�@�C��
015240         PERFORM �G���[�����o.
015410     OPEN INPUT   ������}�X�^
015420         MOVE NC"����" TO �t�@�C����.
015430         PERFORM �I�[�v���`�F�b�N.
015250*================================================================*
015260 �I�[�v���`�F�b�N SECTION.
015270*
015280     IF ( ��ԃL�[  NOT =  "00" )
015290         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
015300         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
015310         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015320                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015330         ACCEPT  �L�[���� FROM CONS
015340         PERFORM �t�@�C����
015350         EXIT PROGRAM.
015360*================================================================*
015370 ������擾 SECTION.
015380*
015390     MOVE ZERO TO ���|����敪
015400     READ ������}�X�^
015410     NOT INVALID KEY
015420         MOVE ���|�ő�o�^���ʐ� TO �ő�o�^���v
015430         MOVE ���|�����A���o�^   TO �����A���o�^�v
015440         MOVE ���|�x����       TO �x���񐔂v
015450     END-READ.
015460*
015470*================================================================*
015480 �x������ SECTION.
015490*
015500     PERFORM VARYING �x���b�m�s FROM 1 BY 1
015510                                UNTIL �x���b�m�s > �x���񐔂v
015520         MOVE SPACE TO �x���t���O
015530     END-PERFORM.
015540*
015550*================================================================*
015560 �A�����ڑҔ� SECTION.
015570*
015580     MOVE �A����|�{�p�a��           TO �{�p�a��v�q.
015590     MOVE �A����|�{�p�N             TO �{�p�N�v�q.
015600     MOVE �A����|�{�p��             TO �{�p���v�q.
015610     MOVE �A����|�ی����           TO �ی���ʂv�q.
015620     MOVE �A����|�ی��Ҕԍ�         TO �ی��Ҕԍ��v�q.
015630     MOVE �A����|������           TO �����ʂv�q.
015640     MOVE �A����|��p���S�Ҕԍ�     TO ��p���S�Ҕԍ��v�q.
015650     MOVE �A����|�������           TO ������ʂv�q.
015660     MOVE �A����|��p���S�Ҕԍ����� TO ��p���S�Ҕԍ������v�q.
015670     MOVE �A����|�{�l�Ƒ��敪       TO �{�l�Ƒ��敪�v�q.
015680     MOVE �A����|���҃J�i           TO ���҃J�i�v�q.
015690     MOVE �A����|���Ҕԍ�           TO ���Ҕԍ��v�q.
015700     MOVE �A����|�}��               TO �}�Ԃv�q.
015710*================================================================*
015720 ����Z�b�g SECTION.
015730*
015740     PERFORM ���ڏ�����.
           PERFORM ��{���擾.
015750     PERFORM �{�p�����擾.
015760     PERFORM ��������擾.
015770     PERFORM ��f�ҏ��擾.
015780     PERFORM �����f�[�^�擾.
015790     PERFORM �������擾.
015800     PERFORM �{�p�L�^�擾.
           PERFORM �J�n���擾.
015810     PERFORM ���Z�v�g���я��擾.
015820***     PERFORM ��������擾.
015840     PERFORM �������Z�����擾.
015850     PERFORM ������擾.
015860     PERFORM �ϔC�N�����擾.
           PERFORM �{�p���擾.
015870*
016791*-----------------------------------------------*
016800     IF ( ������������敪�v  NOT = 1 ) AND ( ���Z������������敪�v NOT = 1 )
016813        IF ( ������������敪�v = 3 OR 4 )
016815           PERFORM ������������Ώ۔��菈��
016817        ELSE
016820           PERFORM ���������擾
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
015920*
015930     IF ( �������R����敪�v  NOT = 1 )
               MOVE �������R����敪�v TO �A�E���|�����敪
015980     END-IF.
015990*
016000********************
016010* ��f�ҏ��Z�b�g *
016020********************
015190     MOVE �Еۃ`�F�b�N�v     TO �Еۃ`�F�b�N.
015210     MOVE �g���`�F�b�N�v     TO �g���`�F�b�N.
015220     MOVE ���ۃ`�F�b�N�v     TO ���ۃ`�F�b�N.
           MOVE ���σ`�F�b�N�v     TO ���σ`�F�b�N.
           MOVE ���`�F�b�N�v       TO ���`�F�b�N.
           IF ���`�F�b�N�v NOT = SPACE
               MOVE NC"��"         TO ���}�[�N
           END-IF.
           MOVE �ސE�`�F�b�N�v     TO �ސE�`�F�b�N.
           MOVE ����`�F�b�N�v     TO ����`�F�b�N.
015230     MOVE �V���`�F�b�N�v     TO �V���`�F�b�N.
015240     MOVE �W���`�F�b�N�v     TO �W���`�F�b�N.
015250     MOVE �X���`�F�b�N�v     TO �X���`�F�b�N.
015260     MOVE �P�O���`�F�b�N�v   TO �P�O���`�F�b�N.
      *
           MOVE �{�l�`�F�b�N�v     TO �{�l�`�F�b�N.
           MOVE �Ƒ��`�F�b�N�v     TO �Ƒ��`�F�b�N.
           MOVE �P�ƃ`�F�b�N�v     TO �P�ƃ`�F�b�N.
           MOVE �Q���`�F�b�N�v     TO �Q���`�F�b�N.
           MOVE ����`�F�b�N�v     TO ����`�F�b�N.
           MOVE ���V�`�F�b�N�v     TO ���V�`�F�b�N.
           MOVE �U�΃`�F�b�N�v     TO �U�΃`�F�b�N.
037370     MOVE �{�p�a��v         TO ���|�����敪.
037380     READ �����}�X�^
037390     NOT INVALID KEY
037400         MOVE ���|��������   TO �{�p�a��
037410     END-READ.
016030     MOVE �{�p�N�v           TO �{�p�N �{�p�N�Q.
016040     MOVE �{�p���v           TO �{�p�� �{�p���Q.
           EVALUATE �{�p�a��v
           WHEN 4
               MOVE NC"�g"         TO �{�p�a��Q
           END-EVALUATE
016050*
           IF ( ����L���v(1:1) = NC"��" )
              MOVE  SPACE          TO  �L���v
           END-IF.
           IF ( ����ԍ��v(1:1) = "*"  ) OR
              ( ����ԍ��v(1:2) = "��" )
              MOVE  SPACE          TO  �ԍ��v
           END-IF.
      *
           INSPECT �L���v  REPLACING ALL "�@" BY "  ".
           EVALUATE TRUE
           WHEN (�L���v NOT = SPACE) AND (�ԍ��v NOT = SPACE)
               MOVE SPACE TO �I���t���O�Q
               PERFORM VARYING �J�E���^ FROM 24 BY -1
                 UNTIL (�J�E���^ <= ZERO) OR (�I���t���O�Q NOT = SPACE)
                   IF �L���v(�J�E���^:1) NOT = SPACE
                       MOVE �L���v TO �L���ԍ��v
                       MOVE "�E"   TO �L���ԍ��v(�J�E���^ + 1:2)
                       MOVE �ԍ��v TO �L���ԍ��v(�J�E���^ + 3:40 - �J�E���^ - 2)
                       MOVE "YES"  TO �I���t���O�Q
                   END-IF
               END-PERFORM
               MOVE �L���ԍ��v TO �L���ԍ�
           WHEN �L���v NOT = SPACE
               MOVE �L���v TO �L���ԍ�
           WHEN �ԍ��v NOT = SPACE
               MOVE �ԍ��v TO �L���ԍ�
           END-EVALUATE.
016170*
016180     MOVE ����ی��Ҕԍ��v    TO �ی��Ҕԍ�.
016200     MOVE �����於�̂v        TO �ی��Җ��� �ی��Җ��̂Q.
016660*
016680     IF �s�����ԍ��v(1:2) = "99"
016690         MOVE SPACE        TO ����S�Ҕԍ�
016700     ELSE
016720         MOVE �s�����ԍ��v TO ����S�Ҕԍ�
016760     END-IF.
016780*
016790     IF ( ����󋋎Ҕԍ��v(1:1) = "*"  ) OR
016800        ( ����󋋎Ҕԍ��v(1:2) = "��" )
016810        MOVE  SPACE                TO �󋋎Ҕԍ�
016820     ELSE
      */�󋋎Ҕԍ����W�����ȏ�̏ꍇ�g�𖳎����Ĉ������/210331
               IF ����󋋎Ҕԍ��Q�v = SPACE
016830             MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
               ELSE
                   MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
               END-IF
016840     END-IF.
016250***     MOVE ��ی��҃J�i�v      TO ��ی��҃J�i.
016260     MOVE ��ی��Ҏ����v      TO ��ی��Ҏ���.
016300     MOVE ��ی��ҏZ���P�v    TO �Z���P.
016310     MOVE ��ی��ҏZ���Q�v    TO �Z���Q.
      */ �X�֔ԍ��E�d�b�ԍ��ǉ� /42505
           IF (�{�p�a��N���v�q >= 42505) AND (�A���|�d�b��� = 1)
              IF (��|�_���X�֓d�b�ԍ���� = 0 OR 2) AND
                 ((�X�֔ԍ��P�v NOT = SPACE) OR (�X�֔ԍ��Q�v NOT = SPACE))
017280           MOVE "��"          TO �X��
017260           MOVE �X�֔ԍ��P�v  TO �X�֔ԍ��P
017270           MOVE �X�֔ԍ��Q�v  TO �X�֔ԍ��Q
017280           MOVE "-"           TO �X�֔ԍ����
              END-IF
              IF ��|�_���X�֓d�b�ԍ���� = 0 OR 3
017260           MOVE �d�b�ԍ��v    TO �d�b�ԍ�
              END-IF
           END-IF.
      *     MOVE ���ҏZ���P�v        TO �Z���P.
      *     MOVE ���ҏZ���Q�v        TO �Z���Q.
016320***     MOVE ���҃J�i�v          TO ���҃J�i.
016330     MOVE ���Ҏ����v          TO ���Ҏ��� ���Ҏ����Q.
016340     MOVE �j�`�F�b�N�v        TO �j�`�F�b�N.
016350     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
016360*     MOVE ���ʂv               TO ����.
           MOVE "1.���@2.��@3.���@4.���@5.��"  TO ���a��b�l.
016370     MOVE �����`�F�b�N�v      TO ���a��`�F�b�N�P.
016380     MOVE �吳�`�F�b�N�v      TO ���a��`�F�b�N�Q.
016390     MOVE ���a�`�F�b�N�v      TO ���a��`�F�b�N�R.
016400     MOVE �����`�F�b�N�v      TO ���a��`�F�b�N�S.
      */�����C��������/20190426
023070     MOVE �ߘa�`�F�b�N�v     TO �ߘa�`�F�b�N.
017390*     MOVE �����v              TO ���Ҙa��.
      */�����C��������/20190426
016410*     MOVE �����v              TO ����.
016420     MOVE ���ҔN�v            TO ���ҔN.
016430     MOVE ���Ҍ��v            TO ���Ҍ�.
016440     MOVE ���ғ��v            TO ���ғ�.
016450*     MOVE ��������v          TO ����.
016460     MOVE ���������v(1)       TO ���������P.
016470     MOVE ���������v(2)       TO ���������Q.
016480     MOVE ���������v(3)       TO ���������R.
016490     MOVE ���������v(4)       TO ���������S.
016500     MOVE ���������v(5)       TO ���������T.
016500     MOVE ���������v(6)       TO ���������U.
016500     MOVE ���������v(7)       TO ���������V.
016500*     MOVE ���������v(8)       TO ���������W.
016510*
016520     MOVE ������v            TO ������.
016530***     MOVE �ی���ʖ��̂v      TO �ی����.
      *
           IF ��Q�|������ی��Ҏ��� NOT = SPACE
016940        MOVE ��Q�|������ی��Ҏ��� TO ��ی��Ҏ���
           END-IF.
016680*
016690********************
016700* �����f�[�^�Z�b�g *
016710********************
016720* �P���� *
016730**********
016740     MOVE �������v(1)       TO �������P.
016750     MOVE �����N�v(1)       TO �����N�P.
016760     MOVE �������v(1)       TO �������P.
016770     MOVE �������v(1)       TO �������P.
016780     MOVE �����N�v(1)       TO �����N�P.
016790     MOVE �������v(1)       TO �������P.
016800     MOVE �������v(1)       TO �������P.
016810     MOVE �J�n�N�v(1)       TO �J�n�N�P.
016820     MOVE �J�n���v(1)       TO �J�n���P.
016830     MOVE �J�n���v(1)       TO �J�n���P.
016840     MOVE �I���N�v(1)       TO �I���N�P.
016850     MOVE �I�����v(1)       TO �I�����P.
016860     MOVE �I�����v(1)       TO �I�����P.
016870     MOVE �������v(1)       TO �������P.
016880     MOVE �����`�F�b�N�v(1) TO �����`�F�b�N�P.
016890     MOVE ���~�`�F�b�N�v(1) TO ���~�`�F�b�N�P.
016900     MOVE �]��`�F�b�N�v(1) TO �]��`�F�b�N�P.
016910**********
016920* �Q���� *
016930**********
016940     MOVE �������v(2)       TO �������Q.
016950     MOVE �����N�v(2)       TO �����N�Q.
016960     MOVE �������v(2)       TO �������Q.
016970     MOVE �������v(2)       TO �������Q.
016980     MOVE �����N�v(2)       TO �����N�Q.
016990     MOVE �������v(2)       TO �������Q.
017000     MOVE �������v(2)       TO �������Q.
017010     MOVE �J�n�N�v(2)       TO �J�n�N�Q.
017020     MOVE �J�n���v(2)       TO �J�n���Q.
017030     MOVE �J�n���v(2)       TO �J�n���Q.
017040     MOVE �I���N�v(2)       TO �I���N�Q.
017050     MOVE �I�����v(2)       TO �I�����Q.
017060     MOVE �I�����v(2)       TO �I�����Q.
017070     MOVE �������v(2)       TO �������Q.
017080     MOVE �����`�F�b�N�v(2) TO �����`�F�b�N�Q.
017090     MOVE ���~�`�F�b�N�v(2) TO ���~�`�F�b�N�Q.
017100     MOVE �]��`�F�b�N�v(2) TO �]��`�F�b�N�Q.
017110**********
017120* �R���� *
017130**********
017140     MOVE �������v(3)       TO �������R.
017150     MOVE �����N�v(3)       TO �����N�R.
017160     MOVE �������v(3)       TO �������R.
017170     MOVE �������v(3)       TO �������R.
017180     MOVE �����N�v(3)       TO �����N�R.
017190     MOVE �������v(3)       TO �������R.
017200     MOVE �������v(3)       TO �������R.
017210     MOVE �J�n�N�v(3)       TO �J�n�N�R.
017220     MOVE �J�n���v(3)       TO �J�n���R.
017230     MOVE �J�n���v(3)       TO �J�n���R.
017240     MOVE �I���N�v(3)       TO �I���N�R.
017250     MOVE �I�����v(3)       TO �I�����R.
017260     MOVE �I�����v(3)       TO �I�����R.
017270     MOVE �������v(3)       TO �������R.
017280     MOVE �����`�F�b�N�v(3) TO �����`�F�b�N�R.
017290     MOVE ���~�`�F�b�N�v(3) TO ���~�`�F�b�N�R.
017300     MOVE �]��`�F�b�N�v(3) TO �]��`�F�b�N�R.
017310**********
017320* �S���� *
017330**********
017340     MOVE �������v(4)       TO �������S.
017350     MOVE �����N�v(4)       TO �����N�S.
017360     MOVE �������v(4)       TO �������S.
017370     MOVE �������v(4)       TO �������S.
017380     MOVE �����N�v(4)       TO �����N�S.
017390     MOVE �������v(4)       TO �������S.
017400     MOVE �������v(4)       TO �������S.
017410     MOVE �J�n�N�v(4)       TO �J�n�N�S.
017420     MOVE �J�n���v(4)       TO �J�n���S.
017430     MOVE �J�n���v(4)       TO �J�n���S.
017440     MOVE �I���N�v(4)       TO �I���N�S.
017450     MOVE �I�����v(4)       TO �I�����S.
017460     MOVE �I�����v(4)       TO �I�����S.
017470     MOVE �������v(4)       TO �������S.
017480     MOVE �����`�F�b�N�v(4) TO �����`�F�b�N�S.
017490     MOVE ���~�`�F�b�N�v(4) TO ���~�`�F�b�N�S.
017500     MOVE �]��`�F�b�N�v(4) TO �]��`�F�b�N�S.
017510**********
017520* �T���� *
017530**********
017540     MOVE �������v(5)       TO �������T.
017550     MOVE �����N�v(5)       TO �����N�T.
017560     MOVE �������v(5)       TO �������T.
017570     MOVE �������v(5)       TO �������T.
017580     MOVE �����N�v(5)       TO �����N�T.
017590     MOVE �������v(5)       TO �������T.
017600     MOVE �������v(5)       TO �������T.
017610     MOVE �J�n�N�v(5)       TO �J�n�N�T.
017620     MOVE �J�n���v(5)       TO �J�n���T.
017630     MOVE �J�n���v(5)       TO �J�n���T.
017640     MOVE �I���N�v(5)       TO �I���N�T.
017650     MOVE �I�����v(5)       TO �I�����T.
017660     MOVE �I�����v(5)       TO �I�����T.
017670     MOVE �������v(5)       TO �������T.
017680     MOVE �����`�F�b�N�v(5) TO �����`�F�b�N�T.
017690     MOVE ���~�`�F�b�N�v(5) TO ���~�`�F�b�N�T.
017700     MOVE �]��`�F�b�N�v(5) TO �]��`�F�b�N�T.
017710**************
017720* �o�߃Z�b�g *
017730**************
017740     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ( ���ʂb�m�s > 5 )
017750***             UNTIL ( ���ʂb�m�s > ���ʐ��v )
017760***         MOVE ���ʂb�m�s�v(���ʂb�m�s)   TO �o�ߕ��ʂb�m�s(���ʂb�m�s)
017770***         MOVE ���ʋ�؂v(���ʂb�m�s)     TO ���ʋ��(���ʂb�m�s)
017780         MOVE ����o�ߗ��̂v(���ʂb�m�s) TO �o�ߗ���(���ʂb�m�s)
017790     END-PERFORM.
017800*****************************************
017810*     �V�K�E�p���`�F�b�N�ɂ���        *
017820*   ���V�K...�����L�� ���p��...�����Ȃ� *
017830*****************************************
017840     MOVE �V�K�`�F�b�N�v    TO �V�K�`�F�b�N.
017850     MOVE �p���`�F�b�N�v    TO �p���`�F�b�N.
017860********************
017870* �����f�[�^�Z�b�g *
017880********************
017890*    ****************************************************************
017900*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
017910*    ****************************************************************
017920     MOVE �������v�q                   TO  ������.
017930     MOVE ���ԊO�`�F�b�N�v             TO  ���ԊO�`�F�b�N.
017940     MOVE �x���`�F�b�N�v               TO  �x���`�F�b�N.
017950     MOVE �[��`�F�b�N�v               TO  �[��`�F�b�N.
017960     MOVE �������Z���v�q               TO  �������Z��.
      *
           IF (���ԊO�`�F�b�N�v NOT = SPACE) OR (�[��`�F�b�N�v NOT = SPACE) OR
              (�x���`�F�b�N�v NOT = SPACE)
              MOVE �������Z���v                 TO  �������Z��
              MOVE �������Z��؂v               TO  �������Z���
              MOVE �������Z���v                 TO  �������Z��
           END-IF.
      *
017970     MOVE �Č����v�q                   TO  �Č���.
017980     MOVE ���Ë����v�q                 TO  ���Ë���.
017990     MOVE ���É񐔂v�q                 TO  ���É�.
018000     MOVE ���×��v�q                   TO  ���×�.
018010     MOVE ��ԃ`�F�b�N�v               TO  ��ԃ`�F�b�N.
018020     MOVE ��H�`�F�b�N�v               TO  ��H�`�F�b�N.
018030     MOVE �\���J��`�F�b�N�v           TO  �\���J��`�F�b�N.
018040     MOVE ���É��Z���v�q               TO  ���É��Z��.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
           MOVE �����񐔂v                   TO  ������.
018160     MOVE �������q���Z���v�q           TO  �������q���Z��.
           MOVE �^���񐔂v                   TO  �^����.
           MOVE �^�����v                     TO  �^����×�.
018090     MOVE �{�p���񋟗��v�q           TO  �{�p���񋟗�.
018100     MOVE ���v�v                       TO ���v.
           MOVE ���k���v�q                   TO ���������k��.
018110********************
018120* ���񏈒u���Z�b�g *
018130********************
018140     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ( ���ʂb�m�s > 5 )
018150***             UNTIL ( ���ʂb�m�s > ���ʐ��v )
018160         MOVE ���񏈒u���v�q(���ʂb�m�s) TO ���񏈒u��(���ʂb�m�s)
018170     END-PERFORM.
018180     MOVE ���񏈒u�����v�v         TO ���񏈒u�����v
018190*
018200     MOVE �{�×��`�F�b�N�v            TO �{�×��`�F�b�N.
018210     MOVE �������`�F�b�N�v            TO �������`�F�b�N.
018220     MOVE �Œ藿�`�F�b�N�v            TO �Œ藿�`�F�b�N.
018230********************
018240* �����������Z�b�g *
018250********************
018260*    **********
018270*    * �P���� *
018280*    **********
018290     MOVE ��ÒP���P�v�q             TO ��ÒP���P.
018300     MOVE ��É񐔂P�v�q             TO ��É񐔂P.
018310     MOVE ��×��P�v�q               TO ��×��P.
018320     MOVE ��㪖@�񐔂P�v�q           TO ��㪖@�񐔂P.
018330     MOVE ��㪖@���P�v�q             TO ��㪖@���P.
018340     MOVE ��㪖@�񐔂P�v�q           TO ��㪖@�񐔂P.
018350     MOVE ��㪖@���P�v�q             TO ��㪖@���P.
018360     MOVE �d�É񐔂P�v�q             TO �d�É񐔂P.
018370     MOVE �d�×��P�v�q               TO �d�×��P.
018380     MOVE ���v�P�v�q                 TO ���v�P.
018390     IF ( �����������P�v�q NOT = ZERO )
018400         COMPUTE �����������P = �����������P�v�q / 100
018410     END-IF.
018420     MOVE ���������v�P�v�q           TO ���������v�P.
018430*    **********
018440*    * �Q���� *
018450*    **********
018460     MOVE ��ÒP���Q�v�q             TO ��ÒP���Q.
018470     MOVE ��É񐔂Q�v�q             TO ��É񐔂Q.
018480     MOVE ��×��Q�v�q               TO ��×��Q.
018490     MOVE ��㪖@�񐔂Q�v�q           TO ��㪖@�񐔂Q.
018500     MOVE ��㪖@���Q�v�q             TO ��㪖@���Q.
018510     MOVE ��㪖@�񐔂Q�v�q           TO ��㪖@�񐔂Q.
018520     MOVE ��㪖@���Q�v�q             TO ��㪖@���Q.
018530     MOVE �d�É񐔂Q�v�q             TO �d�É񐔂Q.
018540     MOVE �d�×��Q�v�q               TO �d�×��Q.
018550     MOVE ���v�Q�v�q                 TO ���v�Q.
018560     IF ( �����������Q�v�q NOT = ZERO )
018570         COMPUTE �����������Q = �����������Q�v�q / 100
018580     END-IF.
018590     MOVE ���������v�Q�v�q           TO ���������v�Q.
018600*    ****************
018610*    * �R���ʁ^�W�� *
018620*    ****************
018630     IF ( ���������v�R�W�v�q NOT = ZERO )
018670     MOVE ��ÒP���R�W�v�q             TO ��ÒP���R�W.
018680     MOVE ��É񐔂R�W�v�q             TO ��É񐔂R�W.
018690     MOVE ��×��R�W�v�q               TO ��×��R�W.
018700     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
018710     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
018720     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
018730     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
018740     MOVE �d�É񐔂R�W�v�q             TO �d�É񐔂R�W.
018750     MOVE �d�×��R�W�v�q               TO �d�×��R�W.
018760     MOVE ���v�R�W�v�q                 TO ���v�R�W.
018770     MOVE �����ʍ����v�R�W�v�q         TO �����ʍ����v�R�W.
018780     IF ( �����������R�W�v�q NOT = ZERO )
018790         COMPUTE �����������R�W = �����������R�W�v�q / 100
018800     END-IF.
018810     MOVE ���������v�R�W�v�q           TO ���������v�R�W.
      */25�N06�����V�p���ɋ����؂�ւ��̈ג��������Ȃ�/130614
      **/ ������ 0.7��0.6 /42505
      *     IF (�{�p�a��N���v�q >= 42505)
      *        MOVE "60"                      TO �����R�W
      *        MOVE "0.6"                     TO �����ʂR�W
      *        MOVE "==="                     TO ���������R�W �����ʒ����R�W
      *     END-IF.
018820*    ****************
018830*    * �R���ʁ^10�� *
018840*    ****************
018880     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
018890     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
018900     MOVE ��ÒP���R�O�v�q             TO ��ÒP���R�O.
018910     MOVE ��É񐔂R�O�v�q             TO ��É񐔂R�O.
018920     MOVE ��×��R�O�v�q               TO ��×��R�O.
018930     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
018940     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
018950     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
018960     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
018970     MOVE �d�É񐔂R�O�v�q             TO �d�É񐔂R�O.
018980     MOVE �d�×��R�O�v�q               TO �d�×��R�O.
018990     MOVE ���v�R�O�v�q                 TO ���v�R�O.
019000     IF ( �����������R�O�v�q NOT = ZERO )
019010         COMPUTE �����������R�O = �����������R�O�v�q / 100
019020     END-IF.
019030     MOVE ���������v�R�O�v�q           TO ���������v�R�O.
019040*    ****************
019050*    * �S���ʁ^�T�� *
019060*    ****************
019070*     IF ( ���������v�S�T�v�q NOT = ZERO )
019080*        MOVE "33"                      TO �����S�T
019090*        MOVE "0.33"                    TO �����S�T����
019100*     END-IF.
019110*     MOVE ��ÒP���S�T�v�q             TO ��ÒP���S�T.
019120*     MOVE ��É񐔂S�T�v�q             TO ��É񐔂S�T.
019130*     MOVE ��×��S�T�v�q               TO ��×��S�T.
019140*     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
019150*     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
019160*     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
019170*     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
019180*     MOVE �d�É񐔂S�T�v�q             TO �d�É񐔂S�T.
019190*     MOVE �d�×��S�T�v�q               TO �d�×��S�T.
019200*     MOVE ���v�S�T�v�q                 TO ���v�S�T.
019210*     MOVE �����ʍ����v�S�T�v�q         TO �����ʍ����v�S�T.
019220*     IF ( �����������S�T�v�q NOT = ZERO )
019230*         COMPUTE �����������S�T = �����������S�T�v�q / 100
019240*     END-IF.
019250*     MOVE ���������v�S�T�v�q           TO ���������v�S�T.
019260*    ****************
019270*    * �S���ʁ^�W�� *
019280*    ****************
019290     IF ( ���������v�S�W�v�q NOT = ZERO )
019330     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
019340     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
019350     MOVE ��ÒP���S�W�v�q             TO ��ÒP���S�W.
019360     MOVE ��É񐔂S�W�v�q             TO ��É񐔂S�W.
019370     MOVE ��×��S�W�v�q               TO ��×��S�W.
019380     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
019390     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
019400     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
019410     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
019420     MOVE �d�É񐔂S�W�v�q             TO �d�É񐔂S�W.
019430     MOVE �d�×��S�W�v�q               TO �d�×��S�W.
019440     MOVE ���v�S�W�v�q                 TO ���v�S�W.
019450     MOVE �����ʍ����v�S�W�v�q         TO �����ʍ����v�S�W.
019460     IF ( �����������S�W�v�q NOT = ZERO )
019470         COMPUTE �����������S�W = �����������S�W�v�q / 100
019480     END-IF.
019490     MOVE ���������v�S�W�v�q           TO ���������v�S�W.
      */25�N06�����V�p���ɋ����؂�ւ��̈ג��������Ȃ�/130614
      **/ ������ 0.7��0.6 /42505
      *     IF (�{�p�a��N���v�q >= 42505)
      *        MOVE "60"                      TO �����S�W
      *        MOVE "0.6"                     TO �����ʂS�W
      *        MOVE "==="                     TO ���������S�W �����ʒ����S�W
      *     END-IF.
019500*    ****************
019510*    * �S���ʁ^10�� *
019520*    ****************
019560     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
019570     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
019580     MOVE ��ÒP���S�O�v�q             TO ��ÒP���S�O.
019590     MOVE ��É񐔂S�O�v�q             TO ��É񐔂S�O.
019600     MOVE ��×��S�O�v�q               TO ��×��S�O.
019610     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
019620     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
019630     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
019640     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
019650     MOVE �d�É񐔂S�O�v�q             TO �d�É񐔂S�O.
019660     MOVE �d�×��S�O�v�q               TO �d�×��S�O.
019670     MOVE ���v�S�O�v�q                 TO ���v�S�O.
019680     IF ( �����������S�O�v�q NOT = ZERO )
019690         COMPUTE �����������S�O = �����������S�O�v�q / 100
019700     END-IF.
019710     MOVE ���������v�S�O�v�q           TO ���������v�S�O.
019720*
019730*��***********************************************************************
019740* �T���ʁ^2.5���̈󎚂͕K�v�Ȃ��B
019750*------------------------------------------------------------------------*
019760*    *****************
019770*    * �T���ʁ^2.5�� *
019780*    *****************
019790*     MOVE ��ÒP���T�Q�v�q             TO ��ÒP���T�Q.
019800*     MOVE ��É񐔂T�Q�v�q             TO ��É񐔂T�Q.
019810*     MOVE ��×��T�Q�v�q               TO ��×��T�Q.
019820*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
019830*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
019840*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
019850*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
019860*     MOVE �d�É񐔂T�Q�v�q             TO �d�É񐔂T�Q.
019870*     MOVE �d�×��T�Q�v�q               TO �d�×��T�Q.
019880*     MOVE ���v�T�Q�v�q                 TO ���v�T�Q.
019890*     MOVE �����ʍ����v�T�Q�v�q         TO �����ʍ����v�T�Q.
019900*     IF ( �����������T�Q�v�q NOT = ZERO )
019910*         COMPUTE �����������T�Q = �����������T�Q�v�q / 100
019920*     END-IF.
019930*     MOVE ���������v�T�Q�v�q           TO ���������v�T�Q.
019940*��***********************************************************************
019950*
019960*    ****************
019970*    * �T���ʁ^�T�� *
019980*    ****************
019990*     IF ( ���������v�T�T�v�q NOT = ZERO )
020000*        MOVE "33"                      TO �����T�T
020010*        MOVE "0.33"                    TO �����T�T����
020020*     END-IF.
020030*     MOVE �����J�n���T�T�v�q           TO �����J�n���T�T.
020040*     MOVE �����J�n���T�T�v�q           TO �����J�n���T�T.
020050*     MOVE ��ÒP���T�T�v�q             TO ��ÒP���T�T.
020060*     MOVE ��É񐔂T�T�v�q             TO ��É񐔂T�T.
020070*     MOVE ��×��T�T�v�q               TO ��×��T�T.
020080*     MOVE ��㪖@�񐔂T�T�v�q           TO ��㪖@�񐔂T�T.
020090*     MOVE ��㪖@���T�T�v�q             TO ��㪖@���T�T.
020100*     MOVE ��㪖@�񐔂T�T�v�q           TO ��㪖@�񐔂T�T.
020110*     MOVE ��㪖@���T�T�v�q             TO ��㪖@���T�T.
020120*     MOVE �d�É񐔂T�T�v�q             TO �d�É񐔂T�T.
020130*     MOVE �d�×��T�T�v�q               TO �d�×��T�T.
020140*     MOVE ���v�T�T�v�q                 TO ���v�T�T.
020150*     MOVE �����ʍ����v�T�T�v�q         TO �����ʍ����v�T�T.
020160*     IF ( �����������T�T�v�q NOT = ZERO )
020170*         COMPUTE �����������T�T = �����������T�T�v�q / 100
020180*     END-IF.
020190*     MOVE ���������v�T�T�v�q           TO ���������v�T�T.
020200*    ****************
020210*    * �T���ʁ^�W�� *
020220*    ****************
021220     MOVE SPACE TO ���ʂT�v.
021230     IF ���v�T�W�v�q NOT = ZERO
      */���t
021560        MOVE �����J�n���T�W�v�q           TO �����J�n���T�v
              MOVE "��"                         TO ���b�l
021570        MOVE �����J�n���T�W�v�q           TO �����J�n���T�v
              MOVE "��"                         TO ���b�l
              MOVE "("                          TO ���ʂP�v
      */��×�
              IF ��×��T�W�v�q NOT = ZERO
                  MOVE "("                      TO ���ʂQ�v
021580            MOVE ��ÒP���T�W�v�q         TO ��ÒP���T�v
                  MOVE "x"                      TO ��Z�L���P�v
021590            MOVE ��É񐔂T�W�v�q         TO ��É񐔂T�v
                  MOVE "="                      TO �C�R�[���P�v
021600            MOVE ��×��T�W�v�q           TO ��×��T�v
                  MOVE ")"                      TO ���ʂR�v
              END-IF
      */��㪖@
              IF ��㪖@���T�W�v�q NOT = ZERO
                  MOVE "+"                      TO ���Z�L���P�v
                  MOVE "("                      TO ���ʂS�v
                  COMPUTE ��㪖@�P���T�v        =  ��㪖@���T�W�v�q / ��㪖@�񐔂T�W�v�q
                  MOVE "x"                      TO ��Z�L���Q�v
021610            MOVE ��㪖@�񐔂T�W�v�q       TO ��㪖@�񐔂T�v
                  MOVE "="                      TO �C�R�[���Q�v
021620            MOVE ��㪖@���T�W�v�q         TO ��㪖@���T�v
                  MOVE ")"                      TO ���ʂT�v
              END-IF
      */��㪖@
              IF ��㪖@���T�W�v�q NOT = ZERO
                  MOVE "+"                      TO ���Z�L���Q�v
                  MOVE "("                      TO ���ʂU�v
                  COMPUTE ��㪖@�P���T�v        =  ��㪖@���T�W�v�q / ��㪖@�񐔂T�W�v�q
                  MOVE "x"                      TO ��Z�L���R�v
021630            MOVE ��㪖@�񐔂T�W�v�q       TO ��㪖@�񐔂T�v
                  MOVE "="                      TO �C�R�[���R�v
021640            MOVE ��㪖@���T�W�v�q         TO ��㪖@���T�v
                  MOVE ")"                      TO ���ʂV�v
              END-IF
      */�d�×�
              IF �d�×��T�W�v�q NOT = ZERO
                  MOVE "+"                      TO ���Z�L���R�v
                  MOVE "("                      TO ���ʂW�v
                  COMPUTE �d�ÒP���T�v          =  �d�×��T�W�v�q / �d�É񐔂T�W�v�q
                  MOVE "x"                      TO ��Z�L���S�v
021650            MOVE �d�É񐔂T�W�v�q         TO �d�É񐔂T�v
                  MOVE "="                      TO �C�R�[���S�v
021660            MOVE �d�×��T�W�v�q           TO �d�×��T�v
                  MOVE ")"                      TO ���ʂX�v
              END-IF
      *
              MOVE ")"                          TO ���ʂP�O�v
      */������
              MOVE "x"                          TO ��Z�L���T�v
      */ ������ 0.7��0.6 /42505
              IF (�{�p�a��N���v�q >= 42505)
021290           MOVE "0.6 "                    TO �����ʗ��T�v
              ELSE
021290           MOVE "0.7 "                    TO �����ʗ��T�v
              END-IF
      */����
021680        IF �����������T�W�v�q NOT = ZERO
                 MOVE "x"                       TO ��Z�L���U�v
021690           COMPUTE �����������T�v = �����������T�W�v�q / 100
021700        END-IF
      */���v
              MOVE "="                          TO �C�R�[���T�v
021710        MOVE ���������v�T�W�v�q           TO ���������v�T�v
021720        MOVE ���ʂT�v                     TO ���ʂT�W�Q
021490     END-IF.
020440*    ****************
020450*    * �T���ʁ^10�� *
020460*    ****************
021530     MOVE SPACE TO ���ʂT�v.
021540     IF ���v�T�O�v�q NOT = ZERO
      */���t
021560        MOVE �����J�n���T�O�v�q           TO �����J�n���T�v
              MOVE "��"                         TO ���b�l
021570        MOVE �����J�n���T�O�v�q           TO �����J�n���T�v
              MOVE "��"                         TO ���b�l
              MOVE "("                          TO ���ʂP�v
      */��×�
              IF ��×��T�O�v�q NOT = ZERO
                  MOVE "("                      TO ���ʂQ�v
021580            MOVE ��ÒP���T�O�v�q         TO ��ÒP���T�v
                  MOVE "x"                      TO ��Z�L���P�v
021590            MOVE ��É񐔂T�O�v�q         TO ��É񐔂T�v
                  MOVE "="                      TO �C�R�[���P�v
021600            MOVE ��×��T�O�v�q           TO ��×��T�v
                  MOVE ")"                      TO ���ʂR�v
              END-IF
      */��㪖@
              IF ��㪖@���T�O�v�q NOT = ZERO
                  MOVE "+"                      TO ���Z�L���P�v
                  MOVE "("                      TO ���ʂS�v
                  COMPUTE ��㪖@�P���T�v        =  ��㪖@���T�O�v�q / ��㪖@�񐔂T�O�v�q
                  MOVE "x"                      TO ��Z�L���Q�v
021610            MOVE ��㪖@�񐔂T�O�v�q       TO ��㪖@�񐔂T�v
                  MOVE "="                      TO �C�R�[���Q�v
021620            MOVE ��㪖@���T�O�v�q         TO ��㪖@���T�v
                  MOVE ")"                      TO ���ʂT�v
              END-IF
      */��㪖@
              IF ��㪖@���T�O�v�q NOT = ZERO
                  MOVE "+"                      TO ���Z�L���Q�v
                  MOVE "("                      TO ���ʂU�v
                  COMPUTE ��㪖@�P���T�v        =  ��㪖@���T�O�v�q / ��㪖@�񐔂T�O�v�q
                  MOVE "x"                      TO ��Z�L���R�v
021630            MOVE ��㪖@�񐔂T�O�v�q       TO ��㪖@�񐔂T�v
                  MOVE "="                      TO �C�R�[���R�v
021640            MOVE ��㪖@���T�O�v�q         TO ��㪖@���T�v
                  MOVE ")"                      TO ���ʂV�v
              END-IF
      */�d�×�
              IF �d�×��T�O�v�q NOT = ZERO
                  MOVE "+"                      TO ���Z�L���R�v
                  MOVE "("                      TO ���ʂW�v
                  COMPUTE �d�ÒP���T�v          =  �d�×��T�O�v�q / �d�É񐔂T�O�v�q
                  MOVE "x"                      TO ��Z�L���S�v
021650            MOVE �d�É񐔂T�O�v�q         TO �d�É񐔂T�v
                  MOVE "="                      TO �C�R�[���S�v
021660            MOVE �d�×��T�O�v�q           TO �d�×��T�v
                  MOVE ")"                      TO ���ʂX�v
              END-IF
      *
              MOVE ")"                          TO ���ʂP�O�v
      */������
      *        ��Z�L���T�v �����ʗ��T�v
      */����
021680        IF �����������T�O�v�q NOT = ZERO
                 MOVE "x"                       TO ��Z�L���U�v
021690           COMPUTE �����������T�v = �����������T�O�v�q / 100
021700        END-IF
      */���v
              MOVE "="                          TO �C�R�[���T�v
021710        MOVE ���������v�T�O�v�q           TO ���������v�T�v
021720        MOVE ���ʂT�v                     TO ���ʂT�O�Q
021730     END-IF.
020690*
021750     MOVE �K�p�P�v                       TO �K�p�P.
021760     MOVE �K�p�Q�v                       TO �K�p�Q.
      *
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
           IF ( �{�p�a��N���v�q >= 43006 )
              INITIALIZE �A���^�|�L�[
019550        MOVE �{�p�a��v�q TO �A���^�|�{�p�a��
019560        MOVE �{�p�N�v�q   TO �A���^�|�{�p�N
019570        MOVE �{�p���v�q   TO �A���^�|�{�p��
019580        MOVE ���Ҕԍ��v�q TO �A���^�|���Ҕԍ�
019590        MOVE �}�Ԃv�q     TO �A���^�|�}��
              MOVE ������ʂv�q TO �A���^�|�ی����
              MOVE 36           TO �A���^�|��R�[�h
              MOVE 1            TO �A���^�|�p�����
              CALL "KINUNRYO"
              CANCEL "KINUNRYO"
              MOVE �A���^�|�������q�b�l           TO �������q�b�l
              IF ( �������q���Z���v�q NOT = ZERO )
                 MOVE �������q�b�l                TO �������q
              END-IF
              PERFORM VARYING �J�E���^ FROM 1 BY 1
                        UNTIL �J�E���^ > 3
                 MOVE �A���^�|�������q��(1 �J�E���^) TO ������(�J�E���^)
              END-PERFORM
              PERFORM VARYING �J�E���^ FROM 1 BY 1
                        UNTIL �J�E���^ > 5
                 MOVE �A���^�|�^����(�J�E���^)     TO �^����(�J�E���^)
              END-PERFORM
           END-IF.
      *
020740     MOVE ���Z�|���v                     TO ���v.
020750     MOVE ���Z�|�ꕔ���S��               TO �ꕔ���S��.
020760     MOVE ���Z�|�������z                 TO �������z.
021370     MOVE ���Z�|�󋋎ҕ��S�z             TO �󋋎ҕ��S�z.
021380     MOVE ���Z�|�����������z             TO ���������z.
020770*
020780**------------------------------------------------------------------------------------*
020790** ���ʁi�������Z�Ȃ��ŁA�{�̃��Z�ɂ܂Ƃ߂鎞�A���z�͏������݁E�K�p�Q�ɏ�����ʈ󎚁j
020800*     IF ( �������Z�܂Ƃ߃t���O = "YES" )
020810*         PERFORM ���������v�Z
020820*         MOVE ���Z�|���v                 TO ���v
020830*         MOVE ���Z�|�󋋎ҕ��S�z         TO �ꕔ���S��
020840*     / �����Z����/
020850*         COMPUTE �������z = ���Z�|���v - ���Z�|�󋋎ҕ��S�z
020860**
020870**/�[�Q��̋󔒂ɃX�g�����O���Ă��܂�����NOT SPACE�̎��͍Ō�ɓ]�L����B
021910**/�������Z���R��̎��͗]�������]�L�����B
021920*         IF ������ʗ��̂v NOT = SPACE
021930*            IF �K�p�Q�v NOT = SPACE
021940*                MOVE SPACE TO ������ʗ��̂v�Q
021950*                STRING NC"��"             DELIMITED BY SIZE
021960*                       ������ʗ��̂v     DELIMITED BY SPACE
021970*                       INTO ������ʗ��̂v�Q
021980*                END-STRING
021990*                MOVE ������ʗ��̂v�Q TO �K�p�Q(35:4)
022000*            ELSE
022010*                STRING �K�p�Q�v           DELIMITED BY SPACE
022020*                       NC"��"             DELIMITED BY SIZE
022030*                       ������ʗ��̂v     DELIMITED BY SPACE
022040*                       INTO �K�p�Q
022050*                END-STRING
022060*            END-IF
022070*         END-IF
021050*     END-IF.
022410*------------------------------------------------------------------------*
022420* �����p��̎��A�E�v���ɓ��e���L��
      *
           MOVE SPACE                     TO �����p��v.
      *     IF (���Z�|���ʌp������(1) > 5) OR (���Z�|���ʌp������(2) > 5) OR
      *        (���Z�|���ʌp������(3) > 5) OR (���Z�|���ʌp������(4) > 5) OR
      *        (���Z�|���ʌp������(5) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l
      *     END-IF.
           IF (���Z�|���ʌp������(1) >= 1) OR (���Z�|���ʌp������(2) >= 1) OR
              (���Z�|���ʌp������(3) >= 1) OR (���Z�|���ʌp������(4) >= 1) OR
              (���Z�|���ʌp������(5) >= 1)
              MOVE "�����p��Y���F"       TO �����p��b�l
           END-IF.
           MOVE SPACE                     TO �����p��b�l�Q.
      *     IF (���Z�|���ʌp������(1) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l�Q
      *     END-IF.
           IF (���Z�|���ʌp������(1) > 0)
              MOVE ���Z�|���ʌp������(1)  TO �����v
              MOVE �������v(1)            TO �������v�q(1)
              STRING �����p��b�l�Q       DELIMITED BY SPACE
                     "(1)"                DELIMITED BY SIZE
                     �������v�o(1)        DELIMITED BY "�@"
                     "�A�p������"         DELIMITED BY SIZE
                     �����v               DELIMITED BY SIZE
                     "��"                 DELIMITED BY SIZE
                INTO �����p��P�v�s
              END-STRING
           END-IF.
           MOVE SPACE                     TO �����p��b�l�Q.
      *     IF (���Z�|���ʌp������(2) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l�Q
      *     END-IF.
           IF (���Z�|���ʌp������(2) > 0)
              MOVE ���Z�|���ʌp������(2)  TO �����v
              MOVE �������v(2)            TO �������v�q(2)
              STRING �����p��b�l�Q       DELIMITED BY SPACE
                     "(2)"                DELIMITED BY SIZE
                     �������v�o(2)        DELIMITED BY "�@"
                     "�A�p������"         DELIMITED BY SIZE
                     �����v               DELIMITED BY SIZE
                     "��"                 DELIMITED BY SIZE
                INTO �����p��Q�v�s
              END-STRING
           END-IF.
           MOVE SPACE                     TO �����p��b�l�Q.
      *     IF (���Z�|���ʌp������(3) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l�Q
      *     END-IF.
           IF (���Z�|���ʌp������(3) > 0)
              MOVE ���Z�|���ʌp������(3)  TO �����v
              MOVE �������v(3)            TO �������v�q(3)
              STRING �����p��b�l�Q       DELIMITED BY SPACE
                     "(3)"                DELIMITED BY SIZE
                     �������v�o(3)        DELIMITED BY "�@"
                     "�A�p������"         DELIMITED BY SIZE
                     �����v               DELIMITED BY SIZE
                     "��"                 DELIMITED BY SIZE
                INTO �����p��R�v�s
              END-STRING
           END-IF.
           MOVE SPACE                     TO �����p��b�l�Q.
      *     IF (���Z�|���ʌp������(4) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l�Q
      *     END-IF.
           IF (���Z�|���ʌp������(4) > 0)
              MOVE ���Z�|���ʌp������(4)  TO �����v
              MOVE �������v(4)            TO �������v�q(4)
              STRING �����p��b�l�Q       DELIMITED BY SPACE
                     "(4)"                DELIMITED BY SIZE
                     �������v�o(4)        DELIMITED BY "�@"
                     "�A�p������"         DELIMITED BY SIZE
                     �����v               DELIMITED BY SIZE
                     "��"                 DELIMITED BY SIZE
                INTO �����p��S�v�s
              END-STRING
           END-IF.
           MOVE SPACE                     TO �����p��b�l�Q.
      *     IF (���Z�|���ʌp������(5) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l�Q
      *     END-IF.
           IF (���Z�|���ʌp������(5) > 0)
              MOVE ���Z�|���ʌp������(5)  TO �����v
              MOVE �������v(5)            TO �������v�q(5)
              STRING �����p��b�l�Q       DELIMITED BY SPACE
                     "(5)"                DELIMITED BY SIZE
                     �������v�o(5)        DELIMITED BY "�@"
                     "�A�p������"         DELIMITED BY SIZE
                     �����v               DELIMITED BY SIZE
                     "��"                 DELIMITED BY SIZE
                INTO �����p��T�v�s
              END-STRING
           END-IF.
           MOVE �����p��b�l   TO �����P�v.
           MOVE �����p��P�v�s TO �����Q�v.
           CALL �v���O�������v WITH C LINKAGE
                         USING BY REFERENCE �����P�v
                               BY REFERENCE �����Q�v.
           MOVE �����p��Q�v�s TO �����Q�v.
           CALL �v���O�������v WITH C LINKAGE
                         USING BY REFERENCE �����P�v
                               BY REFERENCE �����Q�v.
           MOVE �����p��R�v�s TO �����Q�v.
           CALL �v���O�������v WITH C LINKAGE
                         USING BY REFERENCE �����P�v
                               BY REFERENCE �����Q�v.
           MOVE �����p��S�v�s TO �����Q�v.
           CALL �v���O�������v WITH C LINKAGE
                         USING BY REFERENCE �����P�v
                               BY REFERENCE �����Q�v.
           MOVE �����p��T�v�s TO �����Q�v.
           CALL �v���O�������v WITH C LINKAGE
                         USING BY REFERENCE �����P�v
                               BY REFERENCE �����Q�v.
           MOVE �����P�v       TO �����p��.
      *
021060**------------------------------------------------------------------------------------*
021087*
021088**********************
021090* �{�p���f�[�^�Z�b�g *
021100**********************
           MOVE �s���{���i�h�r�v       TO �s���{���ԍ�.
021110     MOVE �_���t�ԍ��v           TO �_���t�ԍ�.
021120     MOVE �ڍ��t�����ԍ��v     TO �ڍ��t�����ԍ�.
021130***     MOVE ��z���󗝔ԍ��v       TO ��z���󗝔ԍ�.
021140     MOVE �{�p���X�֔ԍ��P�v     TO �{�p���X�֔ԍ��P.
021150     MOVE �{�p���X�֔ԍ��Q�v     TO �{�p���X�֔ԍ��Q.
021160***     MOVE �{�p���Z���v           TO �{�p���Z���P.
021170     MOVE �{�p���Z���P�v         TO �{�p���Z���P.
021180     MOVE �{�p���Z���Q�v         TO �{�p���Z���Q.
021190     MOVE ��\�҃J�i�v           TO ��\�҃J�i.
021200     MOVE ��\�Җ��v             TO ��\�Җ�.
021210     MOVE �{�p���d�b�ԍ��v       TO �{�p���d�b�ԍ�.
021220*
021230     MOVE �ڍ��@���v             TO �ڍ��@��.
021240*
021250*     IF ( ������s���Q�v = SPACE )
021260*        MOVE SPACE               TO ��s���P
021270*        MOVE ������s���P�v    TO ��s���Q
021280*     ELSE
021290*        MOVE ������s���P�v    TO ��s���P
021300*        MOVE ������s���Q�v    TO ��s���Q
021310*     END-IF.
021320*     IF ( ������s�x�X���Q�v = SPACE )
021330*        MOVE SPACE                TO ��s�x�X���P
021340*        MOVE ������s�x�X���P�v TO ��s�x�X���Q
021350*     ELSE
021360*        MOVE ������s�x�X���P�v TO ��s�x�X���P
021370*        MOVE ������s�x�X���Q�v TO ��s�x�X���Q
021380*     END-IF.
           MOVE ������s���P�v       TO ��s���P.
           MOVE ������s���Q�v       TO ��s���Q.
           MOVE ������s�x�X���P�v   TO ��s�x�X���P.
           MOVE ������s�x�X���Q�v   TO ��s�x�X���Q.
021390***     MOVE �a����ʃR�����g�v     TO �a�����.
021400     MOVE �����ԍ��v             TO �����ԍ�.
021410***     MOVE �������`�l�J�i�v       TO �������`�l�J�i.
021420***     MOVE �������`�l�v           TO �������`�l.
021430*
021440* / �_���t�E���҈ϔC�� /
037370     MOVE �_���t�a��v           TO ���|�����敪.
037380     READ �����}�X�^
037390     NOT INVALID KEY
037400         MOVE ���|��������       TO �󗝘a��
037410     END-READ.
021450     MOVE �_���t�N�v             TO �󗝔N.
021460     MOVE �_���t���v             TO �󗝌�.
021470     MOVE �_���t���v             TO �󗝓�.
021480* ( �ϔC�N���� ������邩 )
021490     IF ( �A���|�ϔC���  = ZERO )
037370         MOVE ���҈ϔC�a��v     TO ���|�����敪
037380         READ �����}�X�^
037390         NOT INVALID KEY
037400             MOVE ���|��������   TO �ϔC�a��
037410         END-READ
021500         MOVE ���҈ϔC�N�v       TO �ϔC�N
021510         MOVE ���҈ϔC���v       TO �ϔC��
021520         MOVE ���҈ϔC���v       TO �ϔC��
021530     END-IF.
021540*
021550* �{�pID
021560     MOVE ���{�p�h�c�v           TO ���{�p�h�c.
021570*
021580* ���ϔԍ�
021590     MOVE ���ϔԍ��v             TO ���ϔԍ�.
021590     MOVE �n���ϔԍ��v           TO �n���ϔԍ�.
021600*
021610************************
021620* ���Z�v�g���я��Z�b�g *
021630************************
021640*     MOVE ���ԌŒ�v          TO ���ԌŒ�.
021650*     MOVE ���Ԃv              TO ����.
021660*     MOVE ���Ҕԍ��v�q        TO ���Ҕԍ�.
021670*     MOVE �}�Ԃv�q            TO �}��.
021660     MOVE ���Ҕԍ��v�q        TO ���Ҕԍ��Q.
021670     MOVE �}�Ԃv�q            TO �}�ԂQ.
021680*
021690*
021700* ���ʃR�����g
021710*     MOVE ���ʃR�����g�v      TO ���ʃR�����g.
021720*
021730*-------------------------------------------------------------------------*
021740*--- �� ���Z�E�v�ăZ�b�g�́A���̈���Z�b�gSECTION �̍Ō�ɂ�邱�ƁI -----*
021750     PERFORM ���Z�E�v�ăZ�b�g.
021760*-------------------------------------------------------------------------*
021770*
021772*-------------------------------------------------------------------------*
021773*--- �� �n����L�����́A���̈���Z�b�gSECTION �̍Ō�ɂ�邱�ƁI   �@-----*
021774*     PERFORM �n����L����.
021775*-------------------------------------------------------------------------*
021776*
021780****     PERFORM �e�X�g�󎚏���.
021790*
021800*================================================================*
021810 ���ڏ����� SECTION.
021820*
021830     INITIALIZE �{�p�����v.
021840     INITIALIZE ��f�ҏ��v.
021850     INITIALIZE �������v.
021860     INITIALIZE ���l���v.
021870     INITIALIZE �����P�v�q.
021880     INITIALIZE �����Q�v�q.
021890     INITIALIZE �����R�v�q.
021910     INITIALIZE YAZ6421P.
021900     MOVE SPACE TO YAZ6421P.
021920*================================================================*
021930 ��{���擾 SECTION.
023130*
           EVALUATE �����ʂv�q
           WHEN 05
               MOVE 2          TO ���Z�|���Z���
           WHEN OTHER
               MOVE 1          TO ���Z�|���Z���
           END-EVALUATE.
019550     MOVE �{�p�a��v�q   TO ���Z�|�{�p�a��.
019560     MOVE �{�p�N�v�q     TO ���Z�|�{�p�N.
019570     MOVE �{�p���v�q     TO ���Z�|�{�p��.
019580     MOVE ���Ҕԍ��v�q   TO ���Z�|���Ҕԍ�.
019590     MOVE �}�Ԃv�q       TO ���Z�|�}��.
019600     READ ���Z�v�g�e
019630     INVALID KEY
              MOVE SPACE     TO ���Z�|���R�[�h
              INITIALIZE        ���Z�|���R�[�h
           END-READ.
      *
028780     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
028790     MOVE �{�p�N�v�q         TO ��|�{�p�N.
028800     MOVE �{�p���v�q         TO ��|�{�p��.
028810     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
028820     READ ��f�ҏ��e
019630     INVALID KEY
              MOVE SPACE     TO ��|���R�[�h
              INITIALIZE        ��|���R�[�h
           END-READ.
026460     MOVE �{�p�a��v�q       TO ��Q�|�{�p�a��.
026470     MOVE �{�p�N�v�q         TO ��Q�|�{�p�N.
026480     MOVE �{�p���v�q         TO ��Q�|�{�p��.
026490     MOVE ���҃R�[�h�v�q     TO ��Q�|���҃R�[�h.
026500     READ ��f�ҏ��Q�e
           INVALID KEY
              MOVE SPACE           TO ��Q�|���R�[�h
           END-READ.
      *
      *
027790     MOVE �{�p�a��v�q       TO ���|�{�p�a��.
027800     MOVE �{�p�N�v�q         TO ���|�{�p�N.
027810     MOVE �{�p���v�q         TO ���|�{�p��.
027820     MOVE ���҃R�[�h�v�q     TO ���|���҃R�[�h.
027830     READ �����f�[�^�e
019630     INVALID KEY
              MOVE SPACE     TO ���|���R�[�h
              INITIALIZE        ���|���R�[�h
027870     NOT INVALID KEY
027900         MOVE ���|���ʐ�                   TO ���ʐ��v
           END-READ.
021940*
021920*================================================================*
021930 �������擾 SECTION.
021940*
021950********************
021960* �����f�[�^�Z�b�g *
021970********************
021980*    ****************************************************************
021990*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
022000*    ****************************************************************
022010     MOVE ���Z�|������                 TO �������v�q.
022020     IF ( ���Z�|���ԊO = 1 )
022030         MOVE NC"��"                   TO ���ԊO�`�F�b�N�v
022040     END-IF.
022050     IF ( ���Z�|�x�� = 1 )
022060         MOVE NC"��"                   TO �x���`�F�b�N�v
022070     END-IF.
022080     IF ( ���Z�|�[�� = 1 )
022090         MOVE NC"��"                   TO �[��`�F�b�N�v
022100     END-IF.
022110*
022120     MOVE ���Z�|�������Z��             TO �������Z���v�q.
022130     MOVE ���Z�|�Č���                 TO �Č����v�q.
022140     MOVE ���Z�|���Ë���               TO ���Ë����v�q.
022150     MOVE ���Z�|���É�               TO ���É񐔂v�q.
022160     MOVE ���Z�|���×�                 TO ���×��v�q.
022170     MOVE ���Z�|���É��Z��             TO ���É��Z���v�q.
           MOVE ���Z�|���������k��           TO ���k���v�q.
022180*
022190     IF ( ���Z�|��� = 1 )
022200         MOVE NC"��"                   TO ��ԃ`�F�b�N�v
022210     END-IF.
022220     IF ( ���Z�|��H = 1 )
022230         MOVE NC"��"                   TO ��H�`�F�b�N�v
022240     END-IF.
022250     IF ( ���Z�|�\���J�� = 1 )
022260         MOVE NC"��"                   TO �\���J��`�F�b�N�v
022270     END-IF.
022280*
022290     MOVE ���Z�|�������q���Z��         TO �������q���Z���v�q.
022300*
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
           MOVE ���Z�|�������q��            TO �����񐔂v.
           MOVE ���Z�|�^����É�            TO �^���񐔂v.
           MOVE ���Z�|�^����×�              TO �^�����v.
022400*
022410     MOVE ���Z�|�{�p���񋟗�         TO �{�p���񋟗��v�q.
022420* ���v
022420     COMPUTE ���v�v = ���Z�|���v + ���Z�|�^����×�.
022440********************
022450* ���񏈒u���Z�b�g *
022460********************
022470     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
022480             UNTIL ( ���ʂb�m�s > ���ʐ��v )
022490         MOVE ���Z�|���񏈒u��(���ʂb�m�s) TO ���񏈒u���v�q(���ʂb�m�s)
022500         IF ( ���Z�|���񏈒u��(���ʂb�m�s) NOT = ZERO )
022510            EVALUATE ���|�������(���ʂb�m�s)
022520* �P���E�Ŗo�E����
022530            WHEN 1
022540            WHEN 2
022550            WHEN 3
022560                MOVE NC"��"       TO �{�×��`�F�b�N�v
022570* �E�P�E���܁E���܍S�k
022580            WHEN 4
022590            WHEN 5
022600            WHEN 7
022610                MOVE NC"��"       TO �������`�F�b�N�v
022620* �s�S���܁E�s�S���܍S�k
022630            WHEN 6
022640            WHEN 8
022650                MOVE NC"��"       TO �Œ藿�`�F�b�N�v
022660            END-EVALUATE
022670         END-IF
022680     END-PERFORM.
022690*
022700     MOVE ���Z�|���񏈒u�����v    TO ���񏈒u�����v�v.
022710********************
022720* �����������Z�b�g *
022730********************
022740*    **********
022750*    * �P���� *
022760*    **********
022770     MOVE ���Z�|��ÒP���P             TO ��ÒP���P�v�q.
022780     MOVE ���Z�|��É񐔂P             TO ��É񐔂P�v�q.
022790     MOVE ���Z�|��×��P               TO ��×��P�v�q.
022800     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
022810     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
022820     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
022830     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
022840     MOVE ���Z�|�d�É񐔂P             TO �d�É񐔂P�v�q.
022850     MOVE ���Z�|�d�×��P               TO �d�×��P�v�q.
022860     MOVE ���Z�|���v�P                 TO ���v�P�v�q.
           IF ���Z�|�����p��������P NOT = ZERO
023850         MOVE ���Z�|�����p��������P   TO �����������P�v�q
           ELSE
024000         MOVE ���Z�|�����������P       TO �����������P�v�q
           END-IF.
022880     MOVE ���Z�|���������v�P           TO ���������v�P�v�q.
022890*    **********
022900*    * �Q���� *
022910*    **********
022920     MOVE ���Z�|��ÒP���Q             TO ��ÒP���Q�v�q.
022930     MOVE ���Z�|��É񐔂Q             TO ��É񐔂Q�v�q.
022940     MOVE ���Z�|��×��Q               TO ��×��Q�v�q.
022950     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
022960     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
022970     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
022980     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
022990     MOVE ���Z�|�d�É񐔂Q             TO �d�É񐔂Q�v�q.
023000     MOVE ���Z�|�d�×��Q               TO �d�×��Q�v�q.
023010     MOVE ���Z�|���v�Q                 TO ���v�Q�v�q.
           IF ���Z�|�����p��������Q NOT = ZERO
023850         MOVE ���Z�|�����p��������Q   TO �����������Q�v�q
           ELSE
024000         MOVE ���Z�|�����������Q       TO �����������Q�v�q
           END-IF.
023030     MOVE ���Z�|���������v�Q           TO ���������v�Q�v�q.
023040*    ****************
023050*    * �R���ʁ^�W�� *
023060*    ****************
023070     MOVE ���Z�|��ÒP���R�W             TO ��ÒP���R�W�v�q.
023080     MOVE ���Z�|��É񐔂R�W             TO ��É񐔂R�W�v�q.
023090     MOVE ���Z�|��×��R�W               TO ��×��R�W�v�q.
023100     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
023110     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
023120     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
023130     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
023140     MOVE ���Z�|�d�É񐔂R�W             TO �d�É񐔂R�W�v�q.
023150     MOVE ���Z�|�d�×��R�W               TO �d�×��R�W�v�q.
023160     MOVE ���Z�|���v�R�W                 TO ���v�R�W�v�q.
023170     MOVE ���Z�|�����ʍ����v�R�W         TO �����ʍ����v�R�W�v�q.
           IF ���Z�|�����p��������R�W NOT = ZERO
023850         MOVE ���Z�|�����p��������R�W   TO �����������R�W�v�q
           ELSE
024160         MOVE ���Z�|�����������R�W       TO �����������R�W�v�q
           END-IF.
023190     MOVE ���Z�|���������v�R�W           TO ���������v�R�W�v�q.
023200*    ****************
023210*    * �R���ʁ^10�� *
023220*    ****************
023230     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
023240     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
023250     MOVE ���Z�|��ÒP���R�O             TO ��ÒP���R�O�v�q.
023260     MOVE ���Z�|��É񐔂R�O             TO ��É񐔂R�O�v�q.
023270     MOVE ���Z�|��×��R�O               TO ��×��R�O�v�q.
023280     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
023290     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
023300     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
023310     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
023320     MOVE ���Z�|�d�É񐔂R�O             TO �d�É񐔂R�O�v�q.
023330     MOVE ���Z�|�d�×��R�O               TO �d�×��R�O�v�q.
023340     MOVE ���Z�|���v�R�O                 TO ���v�R�O�v�q.
           IF ���Z�|�����p��������R�O NOT = ZERO
023850         MOVE ���Z�|�����p��������R�O   TO �����������R�O�v�q
           ELSE
024330         MOVE ���Z�|�����������R�O       TO �����������R�O�v�q
           END-IF.
023360     MOVE ���Z�|���������v�R�O           TO ���������v�R�O�v�q.
023370*    ****************
023380*    * �S���ʁ^�T�� *
023390*    ****************
023400     MOVE ���Z�|��ÒP���S�T             TO ��ÒP���S�T�v�q.
023410     MOVE ���Z�|��É񐔂S�T             TO ��É񐔂S�T�v�q.
023420     MOVE ���Z�|��×��S�T               TO ��×��S�T�v�q.
023430     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
023440     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
023450     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
023460     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
023470     MOVE ���Z�|�d�É񐔂S�T             TO �d�É񐔂S�T�v�q.
023480     MOVE ���Z�|�d�×��S�T               TO �d�×��S�T�v�q.
023490     MOVE ���Z�|���v�S�T                 TO ���v�S�T�v�q.
023500     MOVE ���Z�|�����ʍ����v�S�T         TO �����ʍ����v�S�T�v�q.
023510     MOVE ���Z�|�����������S�T           TO �����������S�T�v�q.
023520     MOVE ���Z�|���������v�S�T           TO ���������v�S�T�v�q.
023530*    ****************
023540*    * �S���ʁ^�W�� *
023550*    ****************
023560     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
023570     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
023580     MOVE ���Z�|��ÒP���S�W             TO ��ÒP���S�W�v�q.
023590     MOVE ���Z�|��É񐔂S�W             TO ��É񐔂S�W�v�q.
023600     MOVE ���Z�|��×��S�W               TO ��×��S�W�v�q.
023610     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
023620     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
023630     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
023640     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
023650     MOVE ���Z�|�d�É񐔂S�W             TO �d�É񐔂S�W�v�q.
023660     MOVE ���Z�|�d�×��S�W               TO �d�×��S�W�v�q.
023670     MOVE ���Z�|���v�S�W                 TO ���v�S�W�v�q.
023680     MOVE ���Z�|�����ʍ����v�S�W         TO �����ʍ����v�S�W�v�q.
           IF ���Z�|�����p��������S�W NOT = ZERO
023850         MOVE ���Z�|�����p��������S�W   TO �����������S�W�v�q
           ELSE
024670         MOVE ���Z�|�����������S�W       TO �����������S�W�v�q
           END-IF.
023700     MOVE ���Z�|���������v�S�W           TO ���������v�S�W�v�q.
023710*    ****************
023720*    * �S���ʁ^10�� *
023730*    ****************
023740     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
023750     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
023760     MOVE ���Z�|��ÒP���S�O             TO ��ÒP���S�O�v�q.
023770     MOVE ���Z�|��É񐔂S�O             TO ��É񐔂S�O�v�q.
023780     MOVE ���Z�|��×��S�O               TO ��×��S�O�v�q.
023790     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
023800     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
023810     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
023820     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
023830     MOVE ���Z�|�d�É񐔂S�O             TO �d�É񐔂S�O�v�q.
023840     MOVE ���Z�|�d�×��S�O               TO �d�×��S�O�v�q.
023850     MOVE ���Z�|���v�S�O                 TO ���v�S�O�v�q.
           IF ���Z�|�����p��������S�O NOT = ZERO
023850         MOVE ���Z�|�����p��������S�O   TO �����������S�O�v�q
           ELSE
024840         MOVE ���Z�|�����������S�O       TO �����������S�O�v�q
           END-IF.
023870     MOVE ���Z�|���������v�S�O           TO ���������v�S�O�v�q.
023880*    *****************
023890*    * �T���ʁ^2.5�� *
023900*    *****************
023910     MOVE ���Z�|��ÒP���T�Q             TO ��ÒP���T�Q�v�q.
023920     MOVE ���Z�|��É񐔂T�Q             TO ��É񐔂T�Q�v�q.
023930     MOVE ���Z�|��×��T�Q               TO ��×��T�Q�v�q.
023940     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
023950     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
023960     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
023970     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
023980     MOVE ���Z�|�d�É񐔂T�Q             TO �d�É񐔂T�Q�v�q.
023990     MOVE ���Z�|�d�×��T�Q               TO �d�×��T�Q�v�q.
024000     MOVE ���Z�|���v�T�Q                 TO ���v�T�Q�v�q.
024010     MOVE ���Z�|�����ʍ����v�T�Q         TO �����ʍ����v�T�Q�v�q.
024020     MOVE ���Z�|�����������T�Q           TO �����������T�Q�v�q.
024030     MOVE ���Z�|���������v�T�Q           TO ���������v�T�Q�v�q.
024040*    ****************
024050*    * �T���ʁ^�T�� *
024060*    ****************
024070     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
024080     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
024090     MOVE ���Z�|��ÒP���T�T             TO ��ÒP���T�T�v�q.
024100     MOVE ���Z�|��É񐔂T�T             TO ��É񐔂T�T�v�q.
024110     MOVE ���Z�|��×��T�T               TO ��×��T�T�v�q.
024120     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
024130     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
024140     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
024150     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
024160     MOVE ���Z�|�d�É񐔂T�T             TO �d�É񐔂T�T�v�q.
024170     MOVE ���Z�|�d�×��T�T               TO �d�×��T�T�v�q.
024180     MOVE ���Z�|���v�T�T                 TO ���v�T�T�v�q.
024190     MOVE ���Z�|�����ʍ����v�T�T         TO �����ʍ����v�T�T�v�q.
024200     MOVE ���Z�|�����������T�T           TO �����������T�T�v�q.
024210     MOVE ���Z�|���������v�T�T           TO ���������v�T�T�v�q.
024220*    ****************
024230*    * �T���ʁ^�W�� *
024240*    ****************
024250     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
024260     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
024270     MOVE ���Z�|��ÒP���T�W             TO ��ÒP���T�W�v�q.
024280     MOVE ���Z�|��É񐔂T�W             TO ��É񐔂T�W�v�q.
024290     MOVE ���Z�|��×��T�W               TO ��×��T�W�v�q.
024300     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
024310     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
024320     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
024330     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
024340     MOVE ���Z�|�d�É񐔂T�W             TO �d�É񐔂T�W�v�q.
024350     MOVE ���Z�|�d�×��T�W               TO �d�×��T�W�v�q.
024360     MOVE ���Z�|���v�T�W                 TO ���v�T�W�v�q.
024370     MOVE ���Z�|�����ʍ����v�T�W         TO �����ʍ����v�T�W�v�q.
           IF ���Z�|�����p��������T�W NOT = ZERO
023850         MOVE ���Z�|�����p��������T�W   TO �����������T�W�v�q
           ELSE
025360         MOVE ���Z�|�����������T�W       TO �����������T�W�v�q
           END-IF.
024390     MOVE ���Z�|���������v�T�W           TO ���������v�T�W�v�q.
024400*    ****************
024410*    * �T���ʁ^10�� *
024420*    ****************
024430     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
024440     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
024450     MOVE ���Z�|��ÒP���T�O             TO ��ÒP���T�O�v�q.
024460     MOVE ���Z�|��É񐔂T�O             TO ��É񐔂T�O�v�q.
024470     MOVE ���Z�|��×��T�O               TO ��×��T�O�v�q.
024480     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
024490     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
024500     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
024510     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
024520     MOVE ���Z�|�d�É񐔂T�O             TO �d�É񐔂T�O�v�q.
024530     MOVE ���Z�|�d�×��T�O               TO �d�×��T�O�v�q.
024540     MOVE ���Z�|���v�T�O                 TO ���v�T�O�v�q.
           IF ���Z�|�����p��������T�O NOT = ZERO
023850         MOVE ���Z�|�����p��������T�O   TO �����������T�O�v�q
           ELSE
025530         MOVE ���Z�|�����������T�O       TO �����������T�O�v�q
           END-IF.
024560     MOVE ���Z�|���������v�T�O           TO ���������v�T�O�v�q.
      */2022
           MOVE ���Z�|���׏����s���Z��         TO ���׏����s���Z���v�q.
           MOVE ���Z�|���׏����s���Z��         TO ���׏����s���Z���v�q.
           IF ���Z�|���׏����s���Z�� NOT = ZERO
               STRING "���׏����s�̐����Z"     DELIMITED BY SIZE
                      ���׏����s���Z���v�q     DELIMITED BY SIZE
                      "�~ ���Z��"              DELIMITED BY SIZE
                      ���׏����s���Z���v�q     DELIMITED BY SIZE
                      "��"                     DELIMITED BY SIZE
                 INTO �K�p�Q�v
               END-STRING
           END-IF.
024570*
024580*================================================================*
024590 �{�p�����擾 SECTION.
024600*
024610**************************************************
024620* �{�@�f�[�^���g�p���A�ȉ��̏����擾           *
024630* �� �_���t�ԍ�.. �_���t�ԍ��v�Ɋi�[             *
024640* �� ����ԍ� ... �ڍ��t�����ԍ��v�Ɋi�[       *
024650* �� ��\�Җ� ... ��\�Җ��v�Ɋi�[               *
024660* �� �Z��1,2   ...�{�p���Z��1,2�v�Ɋi�[          *
024670* �� �d�b�ԍ� ... �{�p���d�b�ԍ��v�Ɋi�[         *
024680**************************************************
024690     MOVE ZERO  TO �{��|�{�p���ԍ�.
024700     READ �{�p�����}�X�^
024710     INVALID KEY
024720         CONTINUE
024730     NOT INVALID KEY
024740*
               MOVE �{��|�s���{���i�h�r    TO �s���{���i�h�r�v
024741         MOVE �{��|�V�_���t�ԍ�      TO �_���t�ԍ��v
024741         MOVE �{��|�ڍ��t�����ԍ�  TO �ڍ��t�����ԍ��v
024800*
024830         MOVE �{��|�X�֔ԍ��P        TO �{�p���X�֔ԍ��P�v
024840         MOVE �{��|�X�֔ԍ��Q        TO �{�p���X�֔ԍ��Q�v
024850         MOVE �{��|��\�҃J�i        TO ��\�҃J�i�v
024860         MOVE �{��|��\�Җ�          TO ��\�Җ��v
024870*
024880         MOVE �{��|�ڍ��@��          TO �ڍ��@���v
024890*
024900         MOVE �{��|�Z���P            TO �{�p���Z���P�v
024910         MOVE �{��|�Z���Q            TO �{�p���Z���Q�v
024920***         STRING �{��|�Z���P  DELIMITED BY SPACE
024930***                �{��|�Z���Q  DELIMITED BY SPACE
024940***           INTO �{�p���Z���v
024950***         END-STRING
024960*
024970         MOVE �{��|�d�b�ԍ�          TO �{�p���d�b�ԍ��v
024980*
024990***         MOVE �{��|������s��      TO ������s���v
025000***         MOVE �{��|������s�x�X��  TO ������s�x�X���v
025010***         MOVE �{��|�a�����          TO �a����ʂv
025020***         MOVE �{��|�����ԍ�          TO �����ԍ��v
025030***         MOVE �{��|�������`�l�J�i    TO �������`�l�J�i�v
025040***         MOVE �{��|�������`�l        TO �������`�l�v
025050*
025060***         EVALUATE �a����ʂv
025070***         WHEN 1
025080***             MOVE NC"�i���j" TO �a����ʃR�����g�v
025090***         WHEN 2
025100***             MOVE NC"�i���j" TO �a����ʃR�����g�v
025110***         WHEN OTHER
025120***             MOVE SPACE      TO �a����ʃR�����g�v
025130***         END-EVALUATE
025140*/ �������Œ� /*
025210*
025330*         MOVE "5610002" TO �����ԍ��v
               PERFORM �����ԍ��擾
      */�@�փR�[�h�R�����g������/20230817
025350*------------------------------------------------------------------------*
025360*         EVALUATE �ی���ʂv�q
025370*         WHEN 01
025380*             MOVE �ی��Ҕԍ��v�q       TO �ی��Ҕԍ���r�v
025390*             PERFORM ���{�p�h�c�Z�b�g
025400*         WHEN 08
      *         WHEN 05
025410*             MOVE �ی��Ҕԍ��v�q(3:6)  TO �ی��Ҕԍ���r�v
025420*             PERFORM ���{�p�h�c�Z�b�g
025430*         WHEN 04
025440*             PERFORM ���ϔԍ��Z�b�g
025450*         WHEN 09
025460*             PERFORM ���q���ԍ��Z�b�g
025470*         END-EVALUATE
      */�@�փR�[�h�R�����g������/20230817
025480*
025490     END-READ.
025500*
      */�@�փR�[�h�l�Q�l������/20230817
025840*
025850*********************************************
025860** �h�c�Ǘ��}�X�^���@�{�p�h�c���擾����B
025870*********************************************
025880** ���{�pID
025890     MOVE 01                   TO �h�c�ǁ|�h�c�敪.
025900     MOVE ZERO                 TO �h�c�ǁ|�{�p���ԍ�.
025910     MOVE ��p���S�Ҕԍ������v�q(3:2)  TO �h�c�ǁ|�ی����.
025920     MOVE SPACE                TO �h�c�ǁ|�ی��Ҕԍ�.
025930     READ �h�c�Ǘ��}�X�^
025940     NOT INVALID KEY
025950          MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO ���{�p�h�c�v
025960     END-READ.
025970*
025980** �s����ID
025990     MOVE 02                     TO �h�c�ǁ|�h�c�敪.
026000     MOVE ZERO                   TO �h�c�ǁ|�{�p���ԍ�.
026010     MOVE ������ʂv�q           TO �h�c�ǁ|�ی����.
026020     MOVE ��p���S�Ҕԍ������v�q TO �h�c�ǁ|�ی��Ҕԍ�.
      */���s�s�̏d�x��Q/120711
           IF ��p���S�Ҕԍ������v�q(1:5) = "39261"
026020         MOVE "264"              TO �h�c�ǁ|�ی��Ҕԍ�
           END-IF.
      *
026030     READ �h�c�Ǘ��}�X�^
           INVALID KEY
              IF ��p���S�Ҕԍ������v�q(1:5) = "39261"
025890           MOVE 01                   TO �h�c�ǁ|�h�c�敪
025900           MOVE ZERO                 TO �h�c�ǁ|�{�p���ԍ�
025910           MOVE 50                   TO �h�c�ǁ|�ی����
025920           MOVE SPACE                TO �h�c�ǁ|�ی��Ҕԍ�
025930           READ �h�c�Ǘ��}�X�^
025940           NOT INVALID KEY
026050              MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO �s�����{�p�h�c�v
                 END-READ
              END-IF
026040     NOT INVALID KEY
026050        MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO �s�����{�p�h�c�v
026060     END-READ.
      */�@�փR�[�h�l�Q�l������/20230817
026070*
026080***
025510*================================================================*
025520 ���{�p�h�c�Z�b�g SECTION.
025530*
025540*********************************************
025550** �h�c�Ǘ��}�X�^���  ���{�p�h�c���擾����B
025561*   (���ۑg���́A�ΏۊO�@���@�ΏہI2005/09 )
025570*********************************************
025580**   / ���{�pID /
025600     MOVE 01                     TO �h�c�ǁ|�h�c�敪.
025610     MOVE ZERO                   TO �h�c�ǁ|�{�p���ԍ�.
025620     MOVE �ی��Ҕԍ���r�v(1:2)  TO �h�c�ǁ|�ی����.
025630     MOVE SPACE                  TO �h�c�ǁ|�ی��Ҕԍ�.
025640     READ �h�c�Ǘ��}�X�^
025650     NOT INVALID KEY
025660         MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO ���{�p�h�c�v
025670     END-READ.
025690*
025700*================================================================*
025710 ���ϔԍ��Z�b�g SECTION.
025720*
025730**************************************************************
025740* �ی��Ҕԍ��ɂ��A���ς̔ԍ����󎚂��邩����
025750* �������L �ǉ� 99/10
025760**************************************************************
025770** 1.���ϑg���A��
025780     MOVE SPACE  TO  �E�o�t���O.
025790     IF ( �{��|���ϘA�ԍ� NOT = ZERO )
025800** ����(�ی��Ҕԍ�)
025810*        IF ( �ی��Ҕԍ��v�q(1:2) = "31" )  OR
025820*           ( �ی��Ҕԍ��v�q = "34130021" )
025830*
025840           MOVE  NC"���ϑg���A����"   TO ���ϘA�ԍ����m�v 
025850           MOVE  NC"��"               TO ���ϘA�ԍ��P�ʂm�v 
025860           MOVE  �{��|���ϘA�ԍ�     TO ���ϘA�ԍ��v
025870           IF ( ���ϘA�ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
025880                 MOVE SPACE TO  ���ϘA�ԍ��v(1:1)
025890           ELSE
025900                 MOVE "YES" TO  �E�o�t���O
025910           END-IF
025920           IF ( ���ϘA�ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
025930                 MOVE SPACE TO  ���ϘA�ԍ��v(2:1)
025940           ELSE
025950                 MOVE "YES" TO  �E�o�t���O
025960           END-IF
025970           IF ( ���ϘA�ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
025980                 MOVE SPACE TO  ���ϘA�ԍ��v(3:1)
025990           ELSE
026000                 MOVE "YES" TO  �E�o�t���O
026010           END-IF
026020           IF ( ���ϘA�ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
026030                 MOVE SPACE TO  ���ϘA�ԍ��v(4:1)
026040           ELSE
026050                 MOVE "YES" TO  �E�o�t���O
026060           END-IF
026070           IF ( ���ϘA�ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
026080                 MOVE SPACE TO  ���ϘA�ԍ��v(5:1)
026090           ELSE
026100                 MOVE "YES" TO  �E�o�t���O
026110           END-IF
026120           IF ( ���ϘA�ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
026130                 MOVE SPACE TO  ���ϘA�ԍ��v(6:1)
026140           ELSE
026150                 MOVE "YES" TO  �E�o�t���O
026160           END-IF
026170           MOVE  ���ϘA�ԍ��W�c�v     TO ���ϔԍ��v
026180*        END-IF
026190     END-IF.
026200*
026210** 2. �n���ϋ��c��
026220     MOVE SPACE  TO  �E�o�t���O.
026230     IF ( �{��|�n���ϘA�ԍ� NOT = ZERO )
026240** ����(�ی��Ҕԍ�)
026250*        IF ( �ی��Ҕԍ��v�q(1:2) = "32" OR "33" OR "34" )  AND
026260*           ( �ی��Ҕԍ��v�q NOT = "34130021" )
026270*
026280           MOVE  NC"�n���ϋ��c���"   TO ���ϘA�ԍ����m�v 
026290           MOVE  NC"��"               TO ���ϘA�ԍ��P�ʂm�v 
026300           MOVE  �{��|�n���ϘA�ԍ�   TO ���ϘA�ԍ��v
026310           IF ( ���ϘA�ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
026320                 MOVE SPACE TO  ���ϘA�ԍ��v(1:1)
026330           ELSE
026340                 MOVE "YES" TO  �E�o�t���O
026350           END-IF
026360           IF ( ���ϘA�ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
026370                 MOVE SPACE TO  ���ϘA�ԍ��v(2:1)
026380           ELSE
026390                 MOVE "YES" TO  �E�o�t���O
026400           END-IF
026410           IF ( ���ϘA�ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
026420                 MOVE SPACE TO  ���ϘA�ԍ��v(3:1)
026430           ELSE
026440                 MOVE "YES" TO  �E�o�t���O
026450           END-IF
026460           IF ( ���ϘA�ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
026470                 MOVE SPACE TO  ���ϘA�ԍ��v(4:1)
026480           ELSE
026490                 MOVE "YES" TO  �E�o�t���O
026500           END-IF
026510           IF ( ���ϘA�ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
026520                 MOVE SPACE TO  ���ϘA�ԍ��v(5:1)
026530           ELSE
026540                 MOVE "YES" TO  �E�o�t���O
026550           END-IF
026560           IF ( ���ϘA�ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
026570                 MOVE SPACE TO  ���ϘA�ԍ��v(6:1)
026580           ELSE
026590                 MOVE "YES" TO  �E�o�t���O
026600           END-IF
026610           MOVE  ���ϘA�ԍ��W�c�v     TO �n���ϔԍ��v
026620*        END-IF
027050     END-IF.
027060*
027070*================================================================*
027080 ���q���ԍ��Z�b�g SECTION.
027090*
027100     MOVE SPACE  TO  �E�o�t���O.
027110     IF ( �{��|���q���ԍ� NOT = ZERO )
027111           IF �{��|�h�q�ȋ敪 = 1
027112              MOVE  NC"�h�q�ȑ�"      TO ���q���ԍ����m�v 
027113           ELSE
027114              MOVE  NC"�h�q����"      TO ���q���ԍ����m�v 
027115           END-IF
027120*           MOVE  NC"�h�q����"         TO ���q���ԍ����m�v 
027130           MOVE  NC"��"               TO ���q���ԍ��P�ʂm�v 
027140           MOVE  �{��|���q���ԍ�     TO ���q���ԍ��v
027150           IF ( ���q���ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
027160                 MOVE SPACE TO  ���q���ԍ��v(1:1)
027170           ELSE
027180                 MOVE "YES" TO  �E�o�t���O
027190           END-IF
027200           IF ( ���q���ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
027210                 MOVE SPACE TO  ���q���ԍ��v(2:1)
027220           ELSE
027230                 MOVE "YES" TO  �E�o�t���O
027240           END-IF
027250           IF ( ���q���ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
027260                 MOVE SPACE TO  ���q���ԍ��v(3:1)
027270           ELSE
027280                 MOVE "YES" TO  �E�o�t���O
027290           END-IF
027300           IF ( ���q���ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
027310                 MOVE SPACE TO  ���q���ԍ��v(4:1)
027320           ELSE
027330                 MOVE "YES" TO  �E�o�t���O
027340           END-IF
027350           IF ( ���q���ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
027360                 MOVE SPACE TO  ���q���ԍ��v(5:1)
027370           ELSE
027380                 MOVE "YES" TO  �E�o�t���O
027390           END-IF
027400           IF ( ���q���ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
027410                 MOVE SPACE TO  ���q���ԍ��v(6:1)
027420           ELSE
027430                 MOVE "YES" TO  �E�o�t���O
027440           END-IF
027450           MOVE  ���q���ԍ��W�c�v     TO ���ϔԍ��v
027460     END-IF.
027470*
027480*================================================================*
027490 ��f�ҏ��擾 SECTION.
027500*
027510**************************************************
027520* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
027530* �� �{�p�N ..... �{�p�N�v�Ɋi�[                 *
027540* �� �{�p�� ..... �{�p���v�Ɋi�[                 *
027550* �� ���Ҕԍ�.... ���Ҕԍ��v�Ɋi�[���e�c�A�ԗp   *
027560* �� �L�� ....... �L���v�Ɋi�[                   *
027570* �� �ԍ� ....... �ԍ��v�Ɋi�[                   *
027580* �� �ی��Ҕԍ� . �ی��Ҕԍ��v�Ɋi�[             *
027590* �� �ی���� ... �ی���ʂv�Ɋi�[               *
027600* �� ��ی��҃J�i.��ی��҃J�i�v�Ɋi�[           *
027610* �� ��ی��Ҏ���.��ی��Ҏ����v�Ɋi�[           *
027620* �� �Z���P ......��ی��ҏZ���P�v�Ɋi�[         *
027630* �� �Z���Q ......��ی��ҏZ���Q�v�Ɋi�[         *
027640* �� ���҃J�i ....���҃J�i�v�Ɋi�[               *
027650* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
027660* �� ���Ґ��� ....�敪�ɂ��`�F�b�N��"��"���i�[ *
027670* �� ���Ҙa�� ....�a��ɂ��`�F�b�N��"��"���i�[ *
027680* �� ���ҔN ......���ҔN�v�Ɋi�[                 *
027690* �� ���Ҍ� ......���Ҍ��v�Ɋi�[                 *
027700* �� ���ғ� ......���ғ��v�Ɋi�[                 *
027710* �� ���� ........���̃}�X�^��葱���v�Ɏ擾     *
027720**************************************************
           IF ��|���R�[�h NOT = SPACE
022660         EVALUATE ��|�ی����
022670         WHEN 01
022690            MOVE NC"��"        TO ���ۃ`�F�b�N�v
022700         WHEN 02
022710         WHEN 06
022750         WHEN 07
022720            MOVE NC"��"        TO �Еۃ`�F�b�N�v
022730         WHEN 03
022740            MOVE NC"��"        TO �g���`�F�b�N�v
022750*         WHEN 07
022760*            MOVE NC"��"        TO �D���`�F�b�N�v
               WHEN 04
      *         WHEN 09
                  MOVE NC"��"        TO ���σ`�F�b�N�v
               WHEN 09
                  MOVE NC"��"        TO ���`�F�b�N�v
               WHEN 08
                  MOVE NC"��"        TO �ސE�`�F�b�N�v
               WHEN 05
                  MOVE NC"��"        TO ����`�F�b�N�v
022770         END-EVALUATE
      *
               IF ��|������� = ZERO
                   MOVE NC"��" TO �P�ƃ`�F�b�N�v
               ELSE
                   MOVE NC"��" TO �Q���`�F�b�N�v
               END-IF
      */�������Z�͖{�l�݂̂Ɂ�
      *         MOVE NC"��" TO �{�l�`�F�b�N�v
      */�{�Ƌ敪�͂ǂꂩ�P�Ɂ�������B
               IF ��|�ی���� = 05
                   EVALUATE ��|���ʋ敪
                   WHEN 1
                   WHEN 2
                       MOVE NC"��" TO ����`�F�b�N�v
                   WHEN 3
                       MOVE NC"��" TO ���V�`�F�b�N�v
                   END-EVALUATE
               ELSE
028984             EVALUATE ��|���ʋ敪
                   WHEN 1
                   WHEN 2
                       MOVE NC"��" TO ����`�F�b�N�v
                   WHEN 3
                       MOVE NC"��" TO ���V�`�F�b�N�v
028991             WHEN 6
                       MOVE NC"��" TO �U�΃`�F�b�N�v
                   WHEN OTHER
                       IF ��|�{�l�Ƒ��敪 = 1
                           MOVE NC"��" TO �{�l�`�F�b�N�v
                       ELSE
                           MOVE NC"��" TO �Ƒ��`�F�b�N�v
                       END-IF
028999             END-EVALUATE
               END-IF
               EVALUATE ���Z�|���t����
               WHEN 10
                   MOVE NC"��" TO �P�O���`�F�b�N�v
               WHEN 9
                   MOVE NC"��" TO �X���`�F�b�N�v
      */���t���ύX/160318
      */�O������P���͂W�����t�Ɂ�/110721
      *             IF (��|�ی���� NOT = 05 ) AND (��|���ʋ敪 = 1)
      *                 MOVE SPACE  TO �X���`�F�b�N�v
      *                 MOVE NC"��" TO �W���`�F�b�N�v
      *             END-IF
      */����13�A�{��04�̏ꍇ�A�O������҂P���́A���t�������W���ɂ���B(�����P�����S���邽�߁A���҂P���A�ی��҂W���A���P���ƂȂ�)/160817
                   IF ((��|�ی����     = 01) AND (��|�ی��Ҕԍ�(1:2) = "13" OR "04")) OR
                      ((��|�ی���� NOT = 01) AND (��|�ی��Ҕԍ�(3:2) = "13" OR "04"))
                       IF (��|�ی���� NOT = 05 ) AND (��|���ʋ敪 = 1)
                           MOVE SPACE  TO �X���`�F�b�N�v
                           MOVE NC"��" TO �W���`�F�b�N�v
                       END-IF
                   END-IF
               WHEN 8
                   MOVE NC"��" TO �W���`�F�b�N�v
               WHEN 7
                   MOVE NC"��" TO �V���`�F�b�N�v
               END-EVALUATE
               MOVE ��|�{�p�a��     TO �{�p�a��v
027820         MOVE ��|�{�p�N       TO �{�p�N�v
027830         MOVE ��|�{�p��       TO �{�p���v
027840         MOVE ��|���Ҕԍ�     TO ���Ҕԍ��v
027850*         MOVE ��|�L��         TO �L���v
027860*         MOVE ��|�ԍ�         TO �ԍ��v
               MOVE SPACE TO �A�Í������|�Í����
      *
      *    / �A�Í������|���͏��Z�b�g /
               MOVE ��|�L��       TO �A�Í������|�L��
               MOVE ��|�ԍ�       TO �A�Í������|�ԍ�
               MOVE ��|�Í������� TO �A�Í������|�Í�������
      *     
               CALL   �����v���O�������v
               CANCEL �����v���O�������v
      *
               MOVE �A�Í������|���������L�� TO �L���v
               MOVE �A�Í������|���������ԍ� TO �ԍ��v
027870         MOVE ��|�ی��Ҕԍ�   TO �ی��Ҕԍ��v
027880         MOVE ��|�ی����     TO �ی���ʂv
027890** �S���y�؂̎}�ԍ폜
027900         IF ( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(1:6) = "133033" )
027910            MOVE ��|�ی��Ҕԍ�(1:6)  TO �ی��Ҕԍ��v
027920         END-IF
027930**
027940         MOVE ��|��ی��҃J�i TO ��ی��҃J�i�v
027950         MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ����v
027980         MOVE ��|�Z���P       TO ��ی��ҏZ���P�v
027990         MOVE ��|�Z���Q       TO ��ی��ҏZ���Q�v
027960         MOVE ��|���җX�֔ԍ��P   TO �X�֔ԍ��P�v
027970         MOVE ��|���җX�֔ԍ��Q   TO �X�֔ԍ��Q�v
027980         MOVE ��|���ҏZ���P       TO ���ҏZ���P�v
027990         MOVE ��|���ҏZ���Q       TO ���ҏZ���Q�v
      */ �d�b�ԍ��ǉ� /42505
               IF ��|���ғd�b�ԍ� NOT = SPACE
                  STRING "�d�b:"            DELIMITED BY SIZE
                         ��|���ғd�b�ԍ�   DELIMITED BY SPACE
                    INTO �d�b�ԍ��v
                  END-STRING
               END-IF
028000         MOVE ��|���҃J�i     TO ���҃J�i�v
028010         MOVE ��|���Ҏ���     TO ���Ҏ����v
028020         MOVE ��|��p���S�Ҕԍ����� TO �s�����ԍ��v
               MOVE ��|��v�Ҕԍ�����     TO �󋋎Ҕԍ��v
028030*
028040         EVALUATE ��|���Ґ���
028050         WHEN 1
028060             MOVE NC"�j"  TO ���ʂv
028070             MOVE NC"��"  TO �j�`�F�b�N�v
028080         WHEN 2
028090             MOVE NC"��"  TO ���ʂv
028100             MOVE NC"��"  TO ���`�F�b�N�v
028110         END-EVALUATE
028120*
028130         EVALUATE ��|���Ҙa��
028140         WHEN 1
028150             MOVE NC"����"  TO �����v
028160             MOVE NC"��"    TO �����`�F�b�N�v
028170         WHEN 2
028180             MOVE NC"�吳"  TO �����v
028190             MOVE NC"��"    TO �吳�`�F�b�N�v
028200         WHEN 3
028210             MOVE NC"���a"  TO �����v
028220             MOVE NC"��"    TO ���a�`�F�b�N�v
028230         WHEN 4
028240             MOVE NC"����"  TO �����v
028250             MOVE NC"��"    TO �����`�F�b�N�v
      */�����C��/20190426
023060         WHEN 5
                   MOVE "5��"   TO �ߘa�b�l�v
023070             MOVE NC"��"  TO �ߘa�`�F�b�N�v
028260         END-EVALUATE
028270*
      */�����C��/������20190426
029310         IF ��|���Ҙa�� > 4
037370             MOVE ��|���Ҙa��     TO ���|�����敪
037380             READ �����}�X�^
037390             NOT INVALID KEY
037400                 MOVE ���|�������� TO �����v
037410             END-READ
029330         END-IF
      */�����C��/������20190426
028280         MOVE ��|���ҔN  TO ���ҔN�v
028290         MOVE ��|���Ҍ�  TO ���Ҍ��v
028300         MOVE ��|���ғ�  TO ���ғ��v
028310*
028320* �����ݒ�
028330         IF ( �{�l�Ƒ��敪�v�q = 1 )
028340            MOVE NC"�{�l"    TO �����v
028350         ELSE
028360            MOVE 05          TO ���|�敪�R�[�h
028370            MOVE ��|����    TO ���|���̃R�[�h
028380            READ ���̃}�X�^
028390            INVALID KEY
028400                MOVE SPACE    TO �����v
028410            NOT INVALID KEY
028420                MOVE ���|���� TO �����v
028430            END-READ
028440         END-IF
028520**
028530* 14/10�`�@���ʋ敪�R�����g��
028540         IF ( ��|�{�p�a��N�� >= 41410 )
028550             IF ( ��|������ = ZERO )
028560                EVALUATE ��|���ʋ敪
028570                WHEN 1
028580                   MOVE "70�ˈȏ� 1��"  TO ���ʃR�����g�v
028590                WHEN 2
028600                   MOVE "70�ˈȏ� 2��"  TO ���ʃR�����g�v
028601                WHEN 3
028602                   MOVE "70�ˈȏ� 3��"  TO ���ʃR�����g�v
028610                WHEN 6
028622                   IF ��|�{�p�a��N�� < 42004
028624                      MOVE "3�˖���"       TO ���ʃR�����g�v
028625                   ELSE
028626                      MOVE "�`������A�w�O"  TO ���ʃR�����g�v
028628                   END-IF
028631                END-EVALUATE
028640             END-IF
028650         END-IF
028660*
028670     END-IF.
028680*
028690     EVALUATE �ی���ʂv�q
028700     WHEN 01
028710         MOVE NC"��" TO �ی���ʖ��̂v
028720     WHEN 02
028730         MOVE NC"��" TO �ی���ʖ��̂v
028740     WHEN 03
028750         MOVE NC"�g" TO �ی���ʖ��̂v
028760     WHEN 04
028770         MOVE NC"��" TO �ی���ʖ��̂v
028780     WHEN 06
028790         MOVE NC"��" TO �ی���ʖ��̂v
028800     WHEN 07
028810         MOVE NC"�D" TO �ی���ʖ��̂v
028820     WHEN 08
028830         MOVE NC"��" TO �ی���ʖ��̂v
028840     WHEN 09
028850         MOVE NC"��" TO �ی���ʖ��̂v
028860     END-EVALUATE.
028870*================================================================*
028880* ��������擾 SECTION.
028890**
028900*****************************************************
028910** �A���f�[�^����ی��҃}�X�^��萿������擾����B *
028920** ���ہ|��������敪=1�̏ꍇ������}�X�^���g�p   *
028930** �� ������...... �����於�̂v�Ɋi�[               *
028940*****************************************************
028950*     MOVE �ی���ʂv�q   TO �ہ|�ی����.
028960*     MOVE �ی��Ҕԍ��v�q TO �ہ|�ی��Ҕԍ�.
028970*     READ �ی��҃}�X�^
028980*     INVALID KEY
      *         IF ( �ی���ʂv�q = 05 ) AND ( �{�p�a��N���v�q >= 42004 )
030800*             MOVE �ی���ʂv�q   TO �s�|������
030810*             MOVE �ی��Ҕԍ��v�q TO �s�|�s�����ԍ�
030820*             READ �s�����}�X�^
030830*             INVALID KEY
030840*                 MOVE SPACE      TO �����於�̂v
030850*             NOT INVALID KEY
031330*                 MOVE �s�|�s��������    TO �����於�̂v
      *             END-READ
      *         ELSE
030840*             MOVE SPACE      TO �����於�̂v
      *         END-IF
029000*     NOT INVALID KEY
029010** �ЕہA���ق́u�Љ�ی��������v������
029020*                 EVALUATE �ی���ʂv�q 
029030*                 WHEN  02
029040*                 WHEN  06
029050*                     IF ( �ہ|�ڔ���敪 = 1 )
029060*                        MOVE �ہ|�ی��Җ���    TO �����於�̂v
029070*                     ELSE
029080*                        STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
029090*                               "�Љ�ی�������"  DELIMITED BY SIZE
029100*                               INTO �����於�̂v
029110*                        END-STRING
029120*                     END-IF
029130** �g���͎x�����܂ň�
029140*                 WHEN  03
029150*                     STRING �ہ|�ی��Җ���  DELIMITED BY SPACE
029160*                            "���N�ی��g��"  DELIMITED BY SIZE
029170*                            "  "            DELIMITED BY SIZE
029180*                            �ہ|�x��������  DELIMITED BY SPACE
029190*                            INTO �����於�̂v
029200*                     END-STRING
029210** ���ς͎x�����܂ň�
029220*                 WHEN  04
029230*                     STRING �ہ|�ی��Җ���  DELIMITED BY SPACE
029240*                            "���ϑg��"      DELIMITED BY SIZE
029250*                            "  "            DELIMITED BY SIZE
029260*                            �ہ|�x��������  DELIMITED BY SPACE
029270*                            INTO �����於�̂v
029280*                     END-STRING
029290*                 WHEN OTHER
029300*                     MOVE �ہ|�ی��Җ���    TO �����於�̂v
029310*                 END-EVALUATE
029320*     END-READ.
029330**
029340*================================================================*
029350 �����f�[�^�擾 SECTION.
029360*
029370**************************************************
029380* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾 *
029390* �� ������...���ʁ{������ʂɂĉ��H���Ċi�[     *
029400* �� �����N.......�����N�v                       *
029410* �� ������.......�������v                       *
029420* �� ������.......�������v                       *
029430* �� �J�n�N.......�����N�v                       *
029440* �� �J�n��.......�������v                       *
029450* �� �J�n��.......�������v                       *
029460* �� �I���N.......�I���N�v                       *
029470* �� �I����.......�I�����v                       *
029480* �� �I����.......�I�����v                       *
029490* �� ������.......�������v                       *
029500* �� �]�A�敪 ....�敪�ɂ��`�F�b�N��"��"���i�[ *
029510* �� �������q ....�敪�ɂ��`�F�b�N��"��"���i�[ *
029520* �� �o�߃R�[�h...�o�߃}�X�^���擾             *
029530**************************************************
           IF ���|���R�[�h NOT = SPACE
029630         MOVE ���|���ʐ�                   TO ���ʐ��v
029640         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
029650                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
029660             MOVE ���|�������(���ʂb�m�s) TO ������ʂv(���ʂb�m�s)
029670             MOVE ���|����(���ʂb�m�s)     TO ���ʂv(���ʂb�m�s)
029680             MOVE ���|���E�敪(���ʂb�m�s) TO ���E�敪�v(���ʂb�m�s)
029690             MOVE ���|�����ʒu�ԍ�(���ʂb�m�s)
029700                                           TO �����ʒu�ԍ��v(���ʂb�m�s)
029710*********************************************
029720* ���j�S�_...������ʁ{���ʂɂĉ��H���Ċi�[ *
029730*********************************************
029740* �������
029750             MOVE SPACE                     TO �������̂v
029760             MOVE 03                        TO ���|�敪�R�[�h
029770             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
029780             READ ���̃}�X�^
029790             INVALID KEY
029800                 MOVE SPACE        TO �������̂v
029810             NOT INVALID KEY
029820                 MOVE ���|�������� TO �������̂v
029830             END-READ
029840* ����
020710             MOVE SPACE                    TO �������v(���ʂb�m�s)
032680*
032690             PERFORM ���ʖ��̖�������
030030*
030040             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
030050             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
030060             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
030070             MOVE ���|�J�n�N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
030080             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
030090             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
030100             IF ( ���|�]�A�敪(���ʂb�m�s) = 9 )
      */�󗝘a��E�ϔC�a��������Ȃ�/20181207
030150                 MOVE 9                    TO �I���a��v(���ʂb�m�s)
030110                 MOVE 99                   TO �I���N�v(���ʂb�m�s)
030120                 MOVE 99                   TO �I�����v(���ʂb�m�s)
030130                 MOVE 99                   TO �I�����v(���ʂb�m�s)
030140             ELSE
      */�󗝘a��E�ϔC�a��������Ȃ�/20181207
030150                 MOVE ���|�I���a��(���ʂb�m�s) TO �I���a��v(���ʂb�m�s)
030150                 MOVE ���|�I���N(���ʂb�m�s)   TO �I���N�v(���ʂb�m�s)
030160                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
030170                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
030180             END-IF
030190* �o�ߗ��̎擾
030200             MOVE 01                         TO �o�|�敪�R�[�h
030210             MOVE ���|�o�߃R�[�h(���ʂb�m�s) TO �o�|�o�߃R�[�h
030220             READ �o�߃}�X�^
030230             INVALID KEY
030240                 MOVE ZERO            TO ���ʂb�m�s�v(���ʂb�m�s)
030250                 MOVE SPACE           TO ���ʋ�؂v(���ʂb�m�s)
030260                 MOVE SPACE           TO �o�ߗ��̂v(���ʂb�m�s)
030270             NOT INVALID KEY
030280*
030290                 EVALUATE ���ʂb�m�s
030300                 WHEN 1
030310                     MOVE NC"�@" TO �o�ߕ��ʂv
030320                 WHEN 2
030330                     MOVE NC"�A" TO �o�ߕ��ʂv
030340                 WHEN 3
030350                     MOVE NC"�B" TO �o�ߕ��ʂv
030360                 WHEN 4
030370                     MOVE NC"�C" TO �o�ߕ��ʂv
030380                 WHEN 5
030390                     MOVE NC"�D" TO �o�ߕ��ʂv
030400                 END-EVALUATE
030410                 STRING  �o�ߕ��ʂv     DELIMITED BY SPACE
030420                         �o�|�o�ߗ���   DELIMITED BY SPACE
030430                        INTO ����o�ߗ��̂v(���ʂb�m�s)
030440                 END-STRING
030450*
030460             END-READ
030470*
030480             MOVE ���|�]�A�敪(���ʂb�m�s) TO �]�A�敪�v(���ʂb�m�s)
030490             EVALUATE ���|�]�A�敪(���ʂb�m�s)
030500             WHEN 1
030510             WHEN 2
030520                 MOVE NC"��"               TO �����`�F�b�N�v(���ʂb�m�s)
030530             WHEN 3
030540                 MOVE NC"��"               TO ���~�`�F�b�N�v(���ʂb�m�s)
030550             WHEN 4
030560                 MOVE NC"��"               TO �]��`�F�b�N�v(���ʂb�m�s)
030570             END-EVALUATE
030580*
030590         END-PERFORM
033370* �V�K/�p�� �`�F�b�N
033380         EVALUATE ���Z�|���Z�����敪
               WHEN 1
033390             MOVE NC"��"                   TO �V�K�`�F�b�N�v
               WHEN 2
033410             MOVE NC"��"                   TO �p���`�F�b�N�v
033400         WHEN 3
033390             MOVE NC"��"                   TO �V�K�`�F�b�N�v
033410             MOVE NC"��"                   TO �p���`�F�b�N�v
               WHEN OTHER
033410             MOVE NC"��"                   TO �p���`�F�b�N�v
033420         END-EVALUATE
030660*
030670* �}�Ԕ���p
030680         MOVE ���|�J�n�f�Ó��蓮�敪   TO �J�n�f�Ó��蓮�敪�v
030690* ������������敪
030700         MOVE ���|���Z������������敪 TO ���Z������������敪�v
027880         MOVE ���|���Z�������R����敪 TO ���Z�������R����敪�v
030710*
030720     END-IF.
030730*================================================================*
030740*================================================================*
030750 �{�p�L�^�擾 SECTION.
030760*
030770************************************************************
030780* ��P�f�[�^���畉���f�[�^�e���ȉ��̏����擾           *
030790* �� �������Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
030800* �� ���É��Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
030810************************************************************
030820     MOVE  SPACE  TO  �����Č��t���O.
030830     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���ʐ��v
030840         IF ( �{�p�N�v = �����N�v(���ʂb�m�s) ) AND
030850            ( �{�p���v = �������v(���ʂb�m�s) )
030860             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
030870             MOVE �}�Ԃv�q              TO �{�L�|�}��
030880             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
030890             MOVE �����N�v(���ʂb�m�s)  TO �J�n�N�v(���ʂb�m�s) �{�L�|�{�p�N
030900             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
030910             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
030920         ELSE
030930             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
030940             MOVE �}�Ԃv�q              TO �{�L�|�}��
030950             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
030960             MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
030970             MOVE �{�p���v�q            TO �{�L�|�{�p��
030980             MOVE ZERO                  TO �{�L�|�{�p��
030990         END-IF
031000         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
031010                                      �{�L�|�{�p�a��N����
031020         END-START
031030         IF ( ��ԃL�[ = "00" )
031040             MOVE ZERO  TO �������v(���ʂb�m�s)
031050             MOVE ZERO  TO �I���N�v�s
031060             MOVE ZERO  TO �I�����v�s
031070             MOVE ZERO  TO �I�����v�s
031080             MOVE SPACE TO �I���t���O�Q
031090             PERFORM �{�p�L�^�e�Ǎ�
031100             IF ( �I���t���O�Q      = SPACE   ) AND
031110                ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
031120                ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
031130                ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
031140                ( �{�L�|�{�p��      = �{�p���v�q     ) 
031150*
031160*        *****************************************************************
031170*        * �J�n�N���� ( ���̕��ʂ����������łȂ����A
031180*                       ���������ł��}�Ԃ����鎞�́A�ŏ��̎{�p�����J�n��)*
031190*        *****************************************************************
031200                 IF ( �{�p�N�v NOT = �����N�v(���ʂb�m�s) ) OR
031210                    ( �{�p���v NOT = �������v(���ʂb�m�s) ) OR
031220                    ( �J�n�f�Ó��蓮�敪�v = 1 )
031230                     MOVE �{�L�|�{�p�N   TO �J�n�N�v(���ʂb�m�s)
031240                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
031250                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
031260                 END-IF
031270             END-IF
031280             PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
031290                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
031300                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
031310                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
031320                           ( �{�L�|�{�p��     NOT = �{�p���v�q       ) OR
031330                           ( �{�L�|�{�p��         > �I�����v(���ʂb�m�s))
031340*               **********
031350*               * ������ *
031360*               **********
      */�������������Ȃ����̓J�E���g���Ȃ�/121024
                      IF (�{�L�|�����{�Ë敪  (���ʂb�m�s) NOT = ZERO) OR
                         (�{�L�|㪖@�敪      (���ʂb�m�s) NOT = ZERO) OR
                         (�{�L�|�d�Ë敪      (���ʂb�m�s) NOT = ZERO) OR
                         (�{�L�|��×������敪(���ʂb�m�s) NOT = ZERO) OR
                         (�{�L�|�������q�敪  (���ʂb�m�s) NOT = ZERO) OR
                         (�{�L�|���񋟋敪  (���ʂb�m�s) NOT = ZERO)
031370                    COMPUTE �������v(���ʂb�m�s) = �������v(���ʂb�m�s) + 1
                      END-IF
031980                MOVE �{�L�|�{�p�a��             TO �I���a��v�s
031380                MOVE �{�L�|�{�p�N               TO �I���N�v�s
031390                MOVE �{�L�|�{�p��               TO �I�����v�s
031400                MOVE �{�L�|�{�p��               TO �I�����v�s
031410*
031420                PERFORM �{�p�L�^�e�Ǎ�
031430            END-PERFORM
031440        END-IF
031160*       ********************************************************************
031170*       * ���������a�ŁA���̕��ʂ����������̎��́A���������P�ɂ���/20150908*
031190*       ********************************************************************
031200        IF ( �{�p�N�v = �����N�v(���ʂb�m�s) ) AND
031210           ( �{�p���v = �������v(���ʂb�m�s) ) AND
                 ( ���|�������(���ʂb�m�s) = 9)
                  MOVE 1             TO �������v(���ʂb�m�s)
              END-IF
031450*       **************************
031460*       * �p���F�I���N�����Z�b�g *
031470*       **************************
031480        IF ( �]�A�敪�v(���ʂb�m�s) = 9 )
032090            MOVE �I���a��v�s  TO �I���a��v(���ʂb�m�s)
031490            MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
031500            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
031510            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
031520        END-IF
031530        IF ( �I���N�����v(���ʂb�m�s) > �󗝔N�����v )
032140            MOVE �I���a��v(���ʂb�m�s) TO �󗝘a��v
031540            MOVE �I���N�v(���ʂb�m�s) TO �󗝔N�v
031550            MOVE �I�����v(���ʂb�m�s) TO �󗝌��v
031560            MOVE �I�����v(���ʂb�m�s) TO �󗝓��v
031570        END-IF
031580     END-PERFORM.
031590*
031600** ----- �O�������݂̂��𔻒� -----------*
031610*
031620*     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
031630*     MOVE �}�Ԃv�q              TO �{�L�|�}��.
031640*     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
031650*     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
031660*     MOVE �{�p���v�q            TO �{�L�|�{�p��.
031670*     MOVE ZERO                  TO �{�L�|�{�p��.
031680*     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
031690*                                  �{�L�|�{�p�a��N����
031700*     END-START.
031710*     IF ( ��ԃL�[ = "00" )
031720*             MOVE SPACE TO �I���t���O�Q
031730*             PERFORM �{�p�L�^�e�Ǎ�
031740*             IF ( �I���t���O�Q      = SPACE   ) AND
031750*                ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
031760*                ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
031770*                ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
031780*                ( �{�L�|�{�p��      = �{�p���v�q     ) 
031790** �����{�p�J�n�����Č����ǂ�������
031800*                 IF ( �{�L�|�Č������� = 1 )
031810*                      MOVE "YES"  TO  �����Č��t���O
031820*                 END-IF
031830**
031840*             END-IF
031850*     END-IF.
031860*     IF ( �����Č��t���O = "YES" )
031870*        PERFORM �O�������̂ݔ���
031880*     END-IF.
031890*
031900*================================================================*
031910*================================================================*
031920 ���Z�v�g���я��擾 SECTION.
031930*================================================================*
031940     MOVE �{�p�a��v�q       TO ��S�|�{�p�a��.
031950     MOVE �{�p�N�v�q         TO ��S�|�{�p�N.
031960     MOVE �{�p���v�q         TO ��S�|�{�p��.
031970     MOVE ���҃R�[�h�v�q     TO ��S�|���҃R�[�h.
031980     MOVE �ی���ʂv�q       TO ��S�|�ی����.
031990     READ ��ƃt�@�C���S
032000     NOT INVALID KEY
032010          MOVE NC"��"        TO ���ԌŒ�v
032020          MOVE ��S�|����    TO ���Ԃv
032030     END-READ.
032040*
032050*================================================================*
032060 �{�p�L�^�e�Ǎ� SECTION.
032070*
032080     READ �{�p�L�^�e NEXT
032090     AT END
032100         MOVE "YES" TO �I���t���O�Q
032110     END-READ.
032120*================================================================*
032130 ������� SECTION.
032140*
032150     MOVE "YAZ6421P"  TO  ��`�̖��o.
032160     MOVE "SCREEN"   TO  ���ڌQ���o.
032170     WRITE YAZ6421P.
032180***     WRITE ������R�[�h.
032190     PERFORM �G���[�����o.
032200*================================================================*
032210 �G���[�����o SECTION.
032220*
032230     IF ( �ʒm���o NOT = "00" )
032240         DISPLAY NC"���[�G���["              UPON CONS
032250         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
032260         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
032270         DISPLAY NC"�g������o�F" �g������o UPON CONS
032280         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
032290                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
032300         ACCEPT  �L�[���� FROM CONS
032310         PERFORM �t�@�C����
032320         MOVE 99 TO PROGRAM-STATUS
032330         EXIT PROGRAM
032340     END-IF.
032350*================================================================*
032360 ���ʖ��̖������� SECTION.
032370*
006490     STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
009980            �������̂v                    DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
006520       INTO �������v(���ʂb�m�s)
006570     END-STRING.
032570*
033290*================================================================*
033300 ��������擾 SECTION.
033310*
033320* �R�J���ȏ�̒�������� "CHOUKI" ���Ă�. 
033330     MOVE  SPACE TO  �A���ԁ|�L�[.
033340     INITIALIZE      �A���ԁ|�L�[.
033350     MOVE �{�p�a��v�q  TO  �A���ԁ|�{�p�a��.
033360     MOVE �{�p�N�v�q    TO  �A���ԁ|�{�p�N.
033370     MOVE �{�p���v�q    TO  �A���ԁ|�{�p��.
033380     MOVE ���Ҕԍ��v�q  TO  �A���ԁ|���Ҕԍ�.
033390     MOVE �}�Ԃv�q      TO  �A���ԁ|�}��.
033400*
033410     CALL   "CHOUKI".
033420     CANCEL "CHOUKI".
033430*
033440**** �K�p�P���g�p (�u�O�������̂݁v�����鎞�́A��������)
033450     IF ( �A���ԁ|�Ώۃt���O  = "YES" )
033460        IF ( �K�p�P�v  = SPACE )
033470           MOVE NC"�������{�p�p�����R���ʂɋL��"  TO �K�p�P�v
033480        ELSE
033490           STRING �K�p�P�v           DELIMITED BY SPACE
033500                  NC"�C"             DELIMITED BY SIZE
033510                  NC"�������{�p�p�����R���ʂɋL��"   DELIMITED BY SIZE
033520                  INTO �K�p�P�v
033530           END-STRING
033540        END-IF
033550     END-IF.
033560*
033570*================================================================*
033580 �������Z�����擾 SECTION.
033590*****************************************************************
033600** �������Z�����ԊO�Ɛ[��̎��A�K�p�Ɂu��t���ԁv���󎚂���B
033610**   �����̈󎚂͌�3��܂ŉ\
033620*****************************************************************
033630     IF ( ���Z�|���ԊO = 1 ) OR ( ���Z�|�[�� = 1 ) OR ( ���Z�|�x�� = 1 )
033640*
033650         MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
033660         MOVE �}�Ԃv�q              TO �{�L�|�}��
033670         MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
033680         MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
033690         MOVE �{�p���v�q            TO �{�L�|�{�p��
033700         MOVE ZERO                  TO �{�L�|�{�p��
033710         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
033720                                      �{�L�|�{�p�a��N����
033730         END-START
033740         IF ( ��ԃL�[ = "00" )
033750             MOVE ZERO  TO �������Z�J�E���g
033760             MOVE SPACE TO �I���t���O�Q
033770             PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
033780                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
033790                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
033800                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
033810                           ( �{�L�|�{�p��     NOT = �{�p���v�q      ) 
033820               IF ( �{�L�|�������Z = 1 OR 2 OR 3 ) AND ( �{�L�|�f�Ë敪 = 2 )
033830                  COMPUTE �������Z�J�E���g = �������Z�J�E���g  + 1
037200                  IF  �������Z�J�E���g <= 3
037210                      MOVE �{�L�|�������Z TO �������Z�敪�v�s(�������Z�J�E���g)
037220                      MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
037230                      MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
033880                  END-IF
033890               END-IF
033900               PERFORM �{�p�L�^�e�Ǎ�
033910            END-PERFORM
037280** �������Z�̎������Z�b�g
033380            IF ( �������Z���v�s(1) NOT = ZERO ) OR ( �������Z���v�s(1) NOT = ZERO ) 
                      MOVE �������Z���v�s(1) TO �������Z���v
                      MOVE ":"               TO �������Z��؂v
                      MOVE �������Z���v�s(1) TO �������Z���v
                  END-IF
033380            IF ( �������Z���v�s(2) NOT = ZERO ) OR ( �������Z���v�s(2) NOT = ZERO ) 
031910               PERFORM �������Z�K�p�Z�b�g
                  END-IF
033940         END-IF
033950*
033960     END-IF.
033970*
037340*================================================================*
037350 �������Z�K�p�Z�b�g SECTION.
037360*
037370     PERFORM VARYING �ԍ��J�E���^ FROM 1 BY 1
037380              UNTIL  �ԍ��J�E���^ > 3
037390         IF ( �������Z���v�s(�ԍ��J�E���^)  = ZERO )  AND 
037400            ( �������Z���v�s(�ԍ��J�E���^)  = ZERO ) 
037410             CONTINUE
037420         ELSE
037430* �Œ荀��
037440             EVALUATE �������Z�敪�v�s(�ԍ��J�E���^) 
037450             WHEN 1
037460                MOVE NC"���ԊO"   TO ���Z���e�v(�ԍ��J�E���^)
033320             WHEN 2
033330                MOVE NC"�x�@��"   TO ���Z���e�v(�ԍ��J�E���^)
037470             WHEN 3
037480                MOVE NC"�[�@��"   TO ���Z���e�v(�ԍ��J�E���^)
037490             END-EVALUATE
037500*
037510             MOVE NC"�F"          TO ���Z��؂v(�ԍ��J�E���^)
037520             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
037530             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
037540*
037550**** ���������{��ϊ�
037560* ����
037570             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
037580             IF �����v >= 10
037590                 MOVE �����v�P    TO �����ԍ��v�P
037600                 PERFORM ���{��ϊ�
037610                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
037620                 MOVE �����v�Q    TO �����ԍ��v�P
037630                 PERFORM ���{��ϊ�
037640                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
037650             ELSE
037660                 MOVE �����v�Q    TO �����ԍ��v�P
037670                 PERFORM ���{��ϊ�
037680                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
037690             END-IF
037700* ��
037710             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
037720             MOVE �����v�P    TO �����ԍ��v�P
037730             PERFORM ���{��ϊ�
037740             MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
037750             MOVE �����v�Q    TO �����ԍ��v�P
037760             PERFORM ���{��ϊ�
037770             MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
037780** 
037790        END-IF
037800     END-PERFORM.
037810*
037820     MOVE  �������Z�W�c�m�v(1)   TO �������Z�����P�v. 
037830     MOVE  �������Z�W�c�m�v(2)   TO �������Z�����Q�v. 
037840     MOVE  �������Z�W�c�m�v(3)   TO �������Z�����R�v. 
037850*
037860**** �K�p�P���Q���g�p�i�������R�L�ڂœK�p�P���g���Ă��鎞�́A�K�p�Q�j
037870     IF ( �������Z���v�s(2)  = ZERO ) AND ( �������Z���v�s(2)  = ZERO ) 
037880         CONTINUE
037890     ELSE
037900         IF �K�p�P�v  = SPACE
037910               STRING NC"�������Z"       DELIMITED BY SIZE
037920                      �������Z�����P�v   DELIMITED BY SIZE
037930                      �������Z�����Q�v   DELIMITED BY SIZE
037940                      �������Z�����R�v   DELIMITED BY SIZE
037950                      INTO �K�p�P�v
037960               END-STRING
037970         ELSE
033830               STRING �K�p�P�v           DELIMITED BY SPACE
036850                      NC"�C"             DELIMITED BY SIZE
036860                      NC"�������Z"       DELIMITED BY SIZE
033840                      �������Z�����P�v   DELIMITED BY SIZE
033850                      �������Z�����Q�v   DELIMITED BY SIZE
033860                      �������Z�����R�v   DELIMITED BY SIZE
033870                      INTO �K�p�P�v
038030               END-STRING
038040         END-IF
038050     END-IF.
038060*
038070*================================================================*
038080 ���{��ϊ� SECTION.
038090*
038100     MOVE NC"�O"     TO �S�p�����ԍ��v.
038110     CALL "htoz" WITH C LINKAGE
038120                        USING �����ԍ��v�P �S�p�����ԍ��v�P.
038130*
034790*================================================================*
034800 ���������擾 SECTION.
034810*
034820********************************************************************
034830*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
034840*  ��: �@�A �Ƃœ]��.
034850*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
034860*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
034870********************************************************************
034880     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
034890     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
034900             UNTIL ( ���ʂb�m�s > ���ʐ��v )
034910*
034920****        IF ( ���|�������Ҕԍ�(���ʂb�m�s)  NOT = ZERO )  AND
034930        IF ( ���|�����A��(���ʂb�m�s)      NOT = ZERO )
034940*
034950           IF ( �J�E���^ = ZERO )
034960               MOVE 1   TO  �J�E���^ �J�E���^�Q
034970               MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
034980               MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
034990               MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
035000           ELSE
035010              IF ( ���|�������Ҕԍ�(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
035020                 ( ���|�����A��(���ʂb�m�s)      = �����A�Ԃb�v     )
035030                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
035040                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
035050              ELSE
035060                 COMPUTE �J�E���^ = �J�E���^  +  1
035070                 MOVE 1   TO  �J�E���^�Q
035080                 MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
035090                 MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
035100                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
035110              END-IF
035120           END-IF
035130        END-IF
035140     END-PERFORM.
035150**************************************************************************
035160*  ���������}�X�^��蕶�͎擾
035170**************************************************************************
035180     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
035190     PERFORM VARYING �J�E���^ FROM 1 BY 1
035200             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
035210** ���ۂ� �敪 01
035220         MOVE 01                        TO �����|�敪�R�[�h
035230         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
035240         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
035250         READ ���������e
035260         NOT INVALID KEY
035270             INITIALIZE ���������v�s
035280             MOVE �����|���������b�l(1) TO  ���������P�v�s
035290             MOVE �����|���������b�l(2) TO  ���������Q�v�s
035300             MOVE �����|���������b�l(3) TO  ���������R�v�s
035310             MOVE �����|���������b�l(4) TO  ���������S�v�s
035320             MOVE �����|���������b�l(5) TO  ���������T�v�s
035330             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
035340                     UNTIL ( �J�E���^�Q > 9 )  OR 
035350                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
035360                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
035370                WHEN 1
035380                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035390                WHEN 2
035400                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035410                WHEN 3
035420                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035430                WHEN 4
035440                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035450                WHEN 5
035460                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035430                WHEN 6
035440                   MOVE "�E"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035450                WHEN 7
035460                   MOVE "�F"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035470                WHEN OTHER
035480                   CONTINUE
035490                END-EVALUATE
035500             END-PERFORM
035510*
035520             IF �����|�����������͋敪 = 1
035530                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
035540                        ���������P�v�s  DELIMITED BY SIZE
035550                        ���������Q�v�s  DELIMITED BY SIZE
035560                        ���������R�v�s  DELIMITED BY SIZE
035570                        ���������S�v�s  DELIMITED BY SIZE
035580                        ���������T�v�s  DELIMITED BY SIZE
035590                        INTO �����������e�����v(�J�E���^)
035600                 END-STRING
035610             ELSE
005946                 INSPECT ���������v�s REPLACING ALL �S�p�� BY ���p��
                       MOVE SPACE TO �����P�v �����Q�v
                       MOVE ���������i���o�[�m�v TO �����P�v
                       MOVE ���������P�v�s       TO �����Q�v
                       CALL �v���O�������v WITH C LINKAGE
                            USING BY REFERENCE �����P�v
                                  BY REFERENCE �����Q�v
                       MOVE ���������Q�v�s       TO �����Q�v
                       CALL �v���O�������v WITH C LINKAGE
                            USING BY REFERENCE �����P�v
                                  BY REFERENCE �����Q�v
                       MOVE ���������R�v�s       TO �����Q�v
                       CALL �v���O�������v WITH C LINKAGE
                            USING BY REFERENCE �����P�v
                                  BY REFERENCE �����Q�v
                       MOVE ���������S�v�s       TO �����Q�v
                       CALL �v���O�������v WITH C LINKAGE
                            USING BY REFERENCE �����P�v
                                  BY REFERENCE �����Q�v
                       MOVE ���������T�v�s       TO �����Q�v
                       CALL �v���O�������v WITH C LINKAGE
                            USING BY REFERENCE �����P�v
                                  BY REFERENCE �����Q�v
                        MOVE �����P�v TO �����������e�����v(�J�E���^)
035700             END-IF
035710*
035720         END-READ
035730     END-PERFORM.
035740*
035750*     PERFORM ���������Z�b�g.
035680     PERFORM �S�����������̃Z�b�g.
035760*
035770*================================================================*
035780 ���������Z�b�g SECTION.
035790*
035800**************************************************************************
035810*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
035820**************************************************************************
035830     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
035840     PERFORM VARYING �J�E���^ FROM 1 BY 1
035850             UNTIL ( �J�E���^ > 9 )  OR ( �����������e�����v(�J�E���^) = SPACE )
035860*
035870          INITIALIZE �����������e�����w�v
035880          MOVE �����������e�����v(�J�E���^)   TO �����������e�����w�v
035890          IF ( �����������e�P�w�v  NOT = SPACE )
035900              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
035910              MOVE �����������e�P�w�v  TO ���������v(�J�E���^�Q)
035920          END-IF
035930          IF ( �����������e�Q�w�v  NOT = SPACE )
035940              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
035950              MOVE �����������e�Q�w�v  TO ���������v(�J�E���^�Q)
035960          END-IF
035970          IF ( �����������e�R�w�v  NOT = SPACE )
035980              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
035990              MOVE �����������e�R�w�v  TO ���������v(�J�E���^�Q)
036000          END-IF
034690          IF  �����������e�S�w�v  NOT = SPACE
034700              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
034710              MOVE �����������e�S�w�v  TO ���������v(�J�E���^�Q)
034720          END-IF
036010*
036020     END-PERFORM.
035700*================================================================*
035710 �S�����������̃Z�b�g SECTION.
035720*
035730**************************************************************************
035740*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
035750**************************************************************************
           MOVE �����������e�����v(1) TO �����P�v.
007270     PERFORM VARYING �J�E���^ FROM 2 BY 1
007280             UNTIL ( �J�E���^ > 9 )  OR  ( �����������e�����v(�J�E���^) = SPACE )
               MOVE �����������e�����v(�J�E���^) TO �����Q�v
006966         CALL �v���O�������v WITH C LINKAGE
006967                             USING BY REFERENCE �����P�v
006968                                   BY REFERENCE �����Q�v
           END-PERFORM.
035760     MOVE  �����P�v   TO  ���������P���v.
035760     MOVE  ZERO   TO  �J�E���^.
035770     PERFORM VARYING �J�E���^ FROM 1 BY 1
035780             UNTIL ( �J�E���^ > 7 )
035790*
035910        MOVE ���������P���v�o(�J�E���^)  TO ���������v(�J�E���^)
035980*
035990     END-PERFORM.
036000*
036030*================================================================*
036040 ������擾 SECTION.
036050*
036060* 2006/04 �ύX
036070* ������� "JOSEIMEI" ���Ă�. 
036080     MOVE SPACE TO  �A�������́|�L�[.
036090     INITIALIZE     �A�������́|�L�[.
036100     MOVE ������ʂv�q           TO �A�������́|�������.
036110     MOVE ��p���S�Ҕԍ������v�q TO �A�������́|��p���S�Ҕԍ�����.
036120*
036130     CALL   "JOSEIMEI".
036140     CANCEL "JOSEIMEI".
036150*
036160     MOVE �A�������́|�P���� TO ������v.
036170*
036430*
036440*================================================================*
036450 �O�������̂ݔ��� SECTION.
036460*
036470*** �O���̒ʉ@�������������� 
036480     MOVE  SPACE            TO �O���t���O.
036490     MOVE ��|���҃R�[�h    TO �{�L�|���҃R�[�h.
036500     MOVE ��|�{�p�a��      TO �{�L�|�{�p�a��.
036510     MOVE ��|�{�p�N        TO �{�L�|�{�p�N.
036520     MOVE ��|�{�p��        TO �{�L�|�{�p��.
036530     MOVE 1                 TO �{�L�|�{�p��.
036540     START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
036550                                  �{�L�|�{�p�a��N����
036560                                  REVERSED
036570     END-START.
036580     IF ( ��ԃL�[ = "00" )
036590         MOVE SPACE  TO �I���t���O�Q
036600         PERFORM �{�p�L�^�e�Ǎ�
036610         IF ( �I���t���O�Q      = SPACE  ) AND
036620            ( �{�L�|���҃R�[�h  = ��|���҃R�[�h ) AND
036630            ( �{�L�|�f�Ë敪    = 2 ) 
036640*
036650            PERFORM �O������
036660**** �K�p�P���g�p
036670            IF ( �O���t���O = "YES" )
036680               MOVE NC"���O�������̂�"    TO  �K�p�P�v
036690            END-IF
036700**
036710         END-IF
036720     END-IF.
036730*
036740*================================================================*
036750 �O������  SECTION.
036760* 
036770*** �ǂݍ��񂾎{�p�L�^�̔N�����A�O�����ǂ������� (�N���̍��� 1 ��?)
036780      MOVE  SPACE  TO  �O���t���O.
036790      INITIALIZE  �v�Z�N�����v �J�n�N�����Q�v �I���N�����Q�v.
036800**
036810      MOVE ��|�{�p�a��    TO �I���a��Q�v.
036820      MOVE ��|�{�p�N      TO �I���N�Q�v.
036830      MOVE ��|�{�p��      TO �I�����Q�v.
036840      MOVE �{�L�|�{�p�a��  TO �J�n�a��Q�v.
036850      MOVE �{�L�|�{�p�N    TO �J�n�N�Q�v.
036860      MOVE �{�L�|�{�p��    TO �J�n���Q�v.
036870*
036880      EVALUATE TRUE
036890       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v = �I���N�Q�v)
036900            PERFORM  �O����r��
036910       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v NOT = �I���N�Q�v)
036920            PERFORM  �O����r�N
036930       WHEN  �J�n�a��Q�v NOT = �I���a��Q�v 
036940            PERFORM  �O����r����
036950      END-EVALUATE.
036960*
036970      IF ( �v�Z���v = 1 )
036980         MOVE  "YES"  TO  �O���t���O
036990      END-IF.
037000*
037010*================================================================*
037020 �O����r��  SECTION.
037030*
037040     IF ( �I�����Q�v >  �J�n���Q�v )
037050         COMPUTE �v�Z���v = �I�����Q�v - �J�n���Q�v
037060     ELSE
037070        MOVE ZERO TO �v�Z���v
037080     END-IF.
037090*
037100*================================================================*
037110 �O����r�N  SECTION.
037120*
037130     IF ( �I���N�Q�v >  �J�n�N�Q�v )
037140         COMPUTE �v�Z�N�v = �I���N�Q�v - �J�n�N�Q�v
037150         COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
037160     ELSE
037170        MOVE ZERO TO �v�Z���v
037180     END-IF.
037190*
037200*================================================================*
037210 �O����r����  SECTION.
037220*
037230     MOVE �J�n�a��Q�v TO ���|�����敪.
037240     READ �����}�X�^
037250     NOT INVALID KEY
037260         MOVE ���|�J�n����N TO �J�n����N�v
037270     END-READ.
037280     MOVE �I���a��Q�v TO ���|�����敪.
037290     READ �����}�X�^
037300     NOT INVALID KEY
037310         MOVE ���|�J�n����N TO �I������N�v
037320     END-READ.
037330**
037340     IF ( �J�n����N�v NOT = ZERO ) AND ( �I������N�v NOT = ZERO )
037350        COMPUTE �J�n����N�v = �J�n����N�v + �J�n�N�Q�v - 1
037360        COMPUTE �I������N�v = �I������N�v + �I���N�Q�v - 1
037370*
037380        IF ( �I������N�v =  �J�n����N�v )
037390           PERFORM  �O����r��
037400        ELSE
037410           IF ( �I������N�v >  �J�n����N�v )
037420               COMPUTE �v�Z�N�v = �I������N�v - �J�n����N�v
037430               COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
037440           ELSE
037450               MOVE ZERO TO �v�Z���v
037460           END-IF
037470        END-IF
037480     ELSE
037490        MOVE ZERO TO �v�Z���v
037500     END-IF.
040580*================================================================*
040590 �E�v���擾 SECTION.
040600*
040610* �E�v���擾�� "TEKIYBUN" ���Ă�. 
040620     MOVE  SPACE TO  �A�E���|�L�[.
040630     INITIALIZE      �A�E���|�L�[.
040640     MOVE �{�p�a��v�q  TO  �A�E���|�{�p�a��.
040650     MOVE �{�p�N�v�q    TO  �A�E���|�{�p�N.
040660     MOVE �{�p���v�q    TO  �A�E���|�{�p��.
040670     MOVE ���Ҕԍ��v�q  TO  �A�E���|���Ҕԍ�.
040680     MOVE �}�Ԃv�q      TO  �A�E���|�}��.
040700*     MOVE 63            TO  �A�E���|������.
039370*     MOVE 56            TO  �A�E���|������.
039370     MOVE 52            TO  �A�E���|������.
015000     IF (���Z�������R����敪�v NOT = 1 )
               MOVE �������R����敪�v TO �A�E���|�����敪
           ELSE
               MOVE 1                  TO �A�E���|�����敪
015050     END-IF.
040710*
040720     CALL   "TEKIYBUN".
040730     CANCEL "TEKIYBUN".
040740*
037680*================================================================*
037690 ��f�҈���敪�X�V SECTION.
037700*
037710** //  ��f�ҏ��e�̈���敪�ɂP���Z�b�g���A�X�V����B//  
037720*
037730     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
037740     MOVE �{�p�N�v�q         TO ��|�{�p�N.
037750     MOVE �{�p���v�q         TO ��|�{�p��.
037760     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
037770     READ ��f�ҏ��e
037780     NOT INVALID KEY
037790         MOVE  1  TO  ��|���Z����敪����
037800         REWRITE  ��|���R�[�h
037810         END-REWRITE
037820         IF ( ��ԃL�[ NOT = "00" )
037830            MOVE NC"��f��" TO �t�@�C����
037840            PERFORM �G���[�\��
037850         END-IF
037860     END-READ.
037870*
037880*================================================================*
037890 �������擾 SECTION.
037900*
037350     MOVE �{�p�a��v�q TO �󗝘a��v.
037910     MOVE �{�p�N�v�q   TO �󗝔N�v.
037920     MOVE �{�p���v�q   TO �󗝌��v.
037930     MOVE �{�p�a��v�q TO ���|�����敪.
037940     READ �����}�X�^
037950     NOT INVALID KEY
037960         MOVE ���|�J�n����N TO �{�p����N�v
037970     END-READ.
037980     IF ( �{�p����N�v NOT = ZERO )
037990        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
038000     END-IF.
038010*
038020     EVALUATE �{�p���v�q
038030     WHEN 4
038040     WHEN 6
038050     WHEN 9
038060     WHEN 11
038070         MOVE 30 TO �󗝓��v
038080     WHEN 2
038090         DIVIDE 4 INTO �{�p����N�v GIVING    ���v
038100                                    REMAINDER �]�v
038110         END-DIVIDE
038120         IF ( �]�v = ZERO )
038130             MOVE 29 TO �󗝓��v
038140         ELSE
038150             MOVE 28 TO �󗝓��v
038160         END-IF
038170     WHEN 1
038180     WHEN 3
038190     WHEN 5
038200     WHEN 7
038210     WHEN 8
038220     WHEN 10
038230     WHEN 12
038240         MOVE 31 TO �󗝓��v
038250     WHEN OTHER
038260          CONTINUE
038270     END-EVALUATE.
038280*
038290*================================================================*
038300 �ϔC�N�����擾 SECTION.
038310*
038320** ---// �����̎󗝔N�ɂ́A�ŏI�ʉ@���������Ă���ׁA�ޔ����� //----
036770     MOVE �󗝘a��v TO �ŏI�ʉ@�a��v.
038330     MOVE �󗝔N�v   TO �ŏI�ʉ@�N�v.
038340     MOVE �󗝌��v   TO �ŏI�ʉ@���v.
038350     MOVE �󗝓��v   TO �ŏI�ʉ@���v.
038360***
038370* (�_���t��)
038380     EVALUATE ���Z�v�g���t�敪�v 
038390*    /  �ŏI�ʉ@�� /
038400     WHEN ZERO
036850         MOVE �ŏI�ʉ@�a��v TO �_���t�a��v
038410         MOVE �ŏI�ʉ@�N�v   TO �_���t�N�v
038420         MOVE �ŏI�ʉ@���v   TO �_���t���v
038430         MOVE �ŏI�ʉ@���v   TO �_���t���v
038440*    /  ������ /
038450     WHEN 1 
038460         PERFORM �������擾
036910         MOVE �󗝘a��v     TO �_���t�a��v
038470         MOVE �󗝔N�v       TO �_���t�N�v
038480         MOVE �󗝌��v       TO �_���t���v
038490         MOVE �󗝓��v       TO �_���t���v
038500*    /  �󎚂Ȃ� /
038510     WHEN 9
036960         MOVE ZERO           TO �_���t�a��v
038520         MOVE ZERO           TO �_���t�N�v
038530         MOVE ZERO           TO �_���t���v
038540         MOVE ZERO           TO �_���t���v
038550*    /  ���̑��́A�ŏI�ʉ@�� /
038560     WHEN OTHER
037010         MOVE �ŏI�ʉ@�a��v TO �_���t�a��v
038570         MOVE �ŏI�ʉ@�N�v   TO �_���t�N�v
038580         MOVE �ŏI�ʉ@���v   TO �_���t���v
038590         MOVE �ŏI�ʉ@���v   TO �_���t���v
038600     END-EVALUATE.
038610**
038620* (���ґ�)
038630     EVALUATE ���Z�v�g���ғ��t�敪�v 
038640*    /  �ŏI�ʉ@�� /
038650     WHEN ZERO
037100         MOVE �ŏI�ʉ@�a��v TO ���҈ϔC�a��v
038660         MOVE �ŏI�ʉ@�N�v   TO ���҈ϔC�N�v
038670         MOVE �ŏI�ʉ@���v   TO ���҈ϔC���v
038680         MOVE �ŏI�ʉ@���v   TO ���҈ϔC���v
038690*    /  ������ /
038700     WHEN 1 
038710         PERFORM �������擾
037160         MOVE �󗝘a��v     TO ���҈ϔC�a��v
003872         MOVE �󗝔N�v       TO ���҈ϔC�N�v
038730         MOVE �󗝌��v       TO ���҈ϔC���v
038740         MOVE �󗝓��v       TO ���҈ϔC���v
038750*    /  �󎚂Ȃ� /
038760     WHEN 9
037210         MOVE ZERO           TO ���҈ϔC�a��v
038770         MOVE ZERO           TO ���҈ϔC�N�v
038780         MOVE ZERO           TO ���҈ϔC���v
038790         MOVE ZERO           TO ���҈ϔC���v
038800*    /  ���̑��́A�ŏI�ʉ@�� /
038810     WHEN OTHER
037260         MOVE �ŏI�ʉ@�a��v TO ���҈ϔC�a��v
038820         MOVE �ŏI�ʉ@�N�v   TO ���҈ϔC�N�v
038830         MOVE �ŏI�ʉ@���v   TO ���҈ϔC���v
038840         MOVE �ŏI�ʉ@���v   TO ���҈ϔC���v
038850     END-EVALUATE.
038860*
039790*================================================================*
039800 ���Z�E�v�ăZ�b�g SECTION.
043230*---------------------------------------------------------------*
043240* �E�v�t�@�C��������Β������R�̑O�ɍăZ�b�g����B
043250* �i������Ή������Ȃ��A�܂蒷�����R�͂��̂܂܁j
043260*---------------------------------------------------------------*
           PERFORM �E�v���擾.
           MOVE �A�E���|�E�v��(1)    TO �������R���P.
           MOVE �A�E���|�E�v��(2)    TO �������R���Q.
           MOVE �A�E���|�E�v��(3)    TO �������R���R.
           MOVE �A�E���|�E�v��(4)    TO �������R���S.
           MOVE �A�E���|�E�v��(5)    TO �������R���T.
           MOVE �A�E���|�E�v��(6)    TO �������R���U.
           MOVE �A�E���|�E�v��(7)    TO �������R���V.
      *     MOVE �A�E���|�E�v��(8)    TO �������R���W.
      *     MOVE �A�E���|�E�v��(9)    TO �������R���X.
      *     MOVE �A�E���|�E�v��(10)   TO �������R���P�O.
040000*
044960*================================================================*
044961 ������������Ώ۔��菈�� SECTION.
044963*------------------------------------------------------------------------------------*
044964* ����}�X�^�́u������������敪�v�� 3 �i�R���ʈȏ����j�̎��A�R���ʈȏォ���肵�āA
044965* ���̎��̂݁A�����������������B
044966*------------------------------------------------------------------------------------*
044967*
044979     MOVE  SPACE TO  �A���Z������|�L�[.
044980     INITIALIZE      �A���Z������|�L�[.
044981     MOVE �{�p�a��v�q  TO  �A���Z������|�{�p�a��.
044982     MOVE �{�p�N�v�q    TO  �A���Z������|�{�p�N.
044983     MOVE �{�p���v�q    TO  �A���Z������|�{�p��.
044984     MOVE ���Ҕԍ��v�q  TO  �A���Z������|���Ҕԍ�.
044985     MOVE �}�Ԃv�q      TO  �A���Z������|�}��.
044986     CALL   "RECEHUGE".
044987     CANCEL "RECEHUGE".
044989*
044990     IF �A���Z������|�Ώۃt���O = "YES"
044991        PERFORM ���������擾
044992     END-IF.
044993*
040010*================================================================*
040011*================================================================*
040012 �n����L���� SECTION.
040013*
040014*--------------------------------------------------------*
040015*  �������F�o�ߗ��̌Œ�� (�S�_�e�o�c�敪�v 1 �g�p)
040016*  �����ȊO�̕��ʂ́A�u�����v
040017*  �����̕��ʂ́A�u�ɖ��v
040018*--------------------------------------------------------*
040019*
040020     IF �S�_�e�o�c�敪�v = 1
040021*      �܂��u�����v�Z�b�g
040022        PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
040023                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
040024*
040025                 EVALUATE ���ʂb�m�s
040026                 WHEN 1
040027                     MOVE NC"�@" TO �o�ߕ��ʐ����v
040028                 WHEN 2
040029                     MOVE NC"�A" TO �o�ߕ��ʐ����v
040030                 WHEN 3
040031                     MOVE NC"�B" TO �o�ߕ��ʐ����v
040032                 WHEN 4
040033                     MOVE NC"�C" TO �o�ߕ��ʐ����v
040034                 WHEN 5
040035                     MOVE NC"�D" TO �o�ߕ��ʐ����v
040036                 END-EVALUATE
040037                 MOVE SPACE TO �o�ߗ���(���ʂb�m�s)
040038                 STRING  �o�ߕ��ʐ����v   DELIMITED BY SPACE
040039                         NC"����"         DELIMITED BY SPACE
040040                        INTO �o�ߗ���(���ʂb�m�s)
040041                 END-STRING
040042        END-PERFORM
040043*
040044*      ���ɁA�R�J���ȏ�̒�������
040045        MOVE  SPACE TO  �A���ԁ|�L�[
040046        INITIALIZE      �A���ԁ|�L�[
040047        MOVE �{�p�a��v�q  TO  �A���ԁ|�{�p�a��
040048        MOVE �{�p�N�v�q    TO  �A���ԁ|�{�p�N
040049        MOVE �{�p���v�q    TO  �A���ԁ|�{�p��
040050        MOVE ���Ҕԍ��v�q  TO  �A���ԁ|���Ҕԍ�
040051        MOVE �}�Ԃv�q      TO  �A���ԁ|�}��
040052        CALL   "CHOUKI"
040053        CANCEL "CHOUKI"
040054*
040055        IF �A���ԁ|�Ώۃt���O  = "YES"
040056           PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
040057                    UNTIL ( ���ʂb�m�s > ���ʐ��v )
040058*
040059               IF �A���ԁ|���Ԃv(���ʂb�m�s)  >  ZERO
040060
040061                   EVALUATE ���ʂb�m�s
040062                   WHEN 1
040063                       MOVE NC"�@" TO �o�ߕ��ʐ����v
040064                   WHEN 2
040065                       MOVE NC"�A" TO �o�ߕ��ʐ����v
040066                   WHEN 3
040067                       MOVE NC"�B" TO �o�ߕ��ʐ����v
040068                   WHEN 4
040069                       MOVE NC"�C" TO �o�ߕ��ʐ����v
040070                   WHEN 5
040071                       MOVE NC"�D" TO �o�ߕ��ʐ����v
040072                   END-EVALUATE
040073                   MOVE SPACE TO �o�ߗ���(���ʂb�m�s)
040074                   STRING  �o�ߕ��ʐ����v   DELIMITED BY SPACE
040075                           NC"�ɖ�"         DELIMITED BY SPACE
040076                          INTO �o�ߗ���(���ʂb�m�s)
040077                   END-STRING
040078               END-IF
040079           END-PERFORM
040080        END-IF
040081*
040082     END-IF.
040083*
040084*
040085*================================================================*
040086*================================================================*
040087*================================================================*
040088 �G���[�\�� SECTION.
040089*
040090     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
040091     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
040092     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
040093     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
040100                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
040110     ACCEPT  �L�[���� FROM CONS
040120     PERFORM �t�@�C����.
040130     EXIT PROGRAM.
040140*================================================================*
040150*================================================================*
040160 �t�@�C���� SECTION.
040170*
040180     CLOSE ����t�@�C��.
040190     CLOSE �ی��҃}�X�^     �����}�X�^          ���̃}�X�^
040200           ���Z�v�g�e       ������}�X�^      �{�p�����}�X�^
040210           �o�߃}�X�^       ��f�ҏ��e        ��f�ҏ��Q�e
040220           �{�p�L�^�e       �����f�[�^�e        ���������e
040230           �h�c�Ǘ��}�X�^   �s�����}�X�^
040240           ��ƃt�@�C���S   ������}�X�^.
040250*================================================================*
040260 �I������ SECTION.
040270*
040280     PERFORM �t�@�C����.
040290*================================================================*
040300*================================================================*
040310 �e�X�g�󎚏��� SECTION.
040320*
           MOVE ALL "9" TO
           �s���{���ԍ� �{�p�� �{�p�N ���ҔN ���Ҍ� ���ғ� �J�n�N�P �J�n���P �J�n���P �I���N�P 
           �I�����P �I�����P �����N�P �������P �������P �����N�P �������P �������P �������P 
           �J�n�N�Q �J�n���Q �J�n���Q �I���N�Q �I�����Q �I�����Q �����N�Q �������Q �������Q 
           �����N�Q �������Q �������Q �������Q �J�n�N�R �J�n���R �J�n���R �I���N�R �I�����R 
           �I�����R �����N�R �������R �������R �����N�R �������R �������R �������R �J�n�N�S 
           �J�n���S �J�n���S �I���N�S �I�����S �I�����S �����N�S �������S �������S �����N�S 
           �������S �������S �������S �J�n�N�T �J�n���T �J�n���T �I���N�T �I�����T �I�����T 
           �����N�T �������T �������T �����N�T �������T �������T �������T ������ ���������k�� 
           ���Ë��� �Č��� �������q���Z�� ���É� ���×� ���v �������Z�� �{�p���񋟗� 
           ���É��Z�� �������Z�� �������Z�� �������Z��� ���񏈒u��(1) ���񏈒u��(2) 
           ���񏈒u��(3) ���񏈒u��(4) ���񏈒u��(5) ���񏈒u�����v ��ÒP���P ��É񐔂P 
           ��×��P ��㪖@�񐔂P ��㪖@���P ��㪖@�񐔂P ��㪖@���P �d�É񐔂P �d�×��P ���v�P 
           �����������P ���������v�P ��ÒP���Q ��É񐔂Q ��×��Q ��㪖@�񐔂Q ��㪖@���Q 
           ��㪖@�񐔂Q ��㪖@���Q �d�É񐔂Q �d�×��Q ���v�Q �����������Q ���������v�Q 
           ��ÒP���R�W ��É񐔂R�W ��×��R�W ��㪖@�񐔂R�W ��㪖@���R�W ��㪖@�񐔂R�W 
           ��㪖@���R�W �d�É񐔂R�W �d�×��R�W ���v�R�W �����ʍ����v�R�W �����������R�W 
           ���������v�R�W �����J�n���R�O �����J�n���R�O ��ÒP���R�O ��É񐔂R�O ��×��R�O 
           ��㪖@�񐔂R�O ��㪖@���R�O ��㪖@�񐔂R�O ��㪖@���R�O �d�É񐔂R�O �d�×��R�O ���v�R�O
           �����������R�O ���������v�R�O �����J�n���S�W �����J�n���S�W ��ÒP���S�W ��É񐔂S�W 
           ��×��S�W ��㪖@�񐔂S�W ��㪖@���S�W ��㪖@�񐔂S�W ��㪖@���S�W �d�É񐔂S�W �d�×��S�W 
           ���v�S�W �����ʍ����v�S�W �����������S�W ���������v�S�W �����J�n���S�O �����J�n���S�O 
           ��ÒP���S�O ��É񐔂S�O ��×��S�O ��㪖@�񐔂S�O ��㪖@���S�O ��㪖@�񐔂S�O ��㪖@���S�O 
           �d�É񐔂S�O �d�×��S�O ���v�S�O �����������S�O ���������v�S�O ���v �ꕔ���S�� �������z 
           �󗝔N �󗝌� �󗝓� �ϔC�N �ϔC�� �ϔC�� �󋋎ҕ��S�z ���������z
           .
           MOVE ALL "X" TO
           ���ϔԍ� �n���ϔԍ� ���{�p�h�c �ی��Ҕԍ� �L���ԍ� ����S�Ҕԍ� �󋋎Ҕԍ� �Z���P �Z���Q 
           �_���t�ԍ� �����ԍ� �ڍ��t�����ԍ�
      *     ���Z�@�֖��P ���Z�@�֖��Q ���Z�@�֖��R ���Z�@�֖��S �x�X���P �x�X���Q �x�X���R �x�X���S
      *     �������`�l�J�i�P �������`�l
           �{�p���X�֔ԍ��P �{�p���X�֔ԍ��Q 
           �{�p���Z���P �{�p���Z���Q �{�p���d�b�ԍ� ��\�҃J�i ��\�Җ� �ی��Җ��� �ی��Җ��̂Q
           ���������P ���������Q ���������R ���������S ���������T ���������U ���������V ���������W
           �������R���P �������R���Q �������R���R �������R���S �������R���T
           �������R���U �������R���V ���ʂT�W�Q ���ʂT�O�Q
           �ڍ��@�� ��\�Җ� ��ی��Ҏ��� ���Ҏ��� �K�p�Q �����p�� 
           .
           MOVE ALL NC"�m" TO
           �������P �������Q �������R �������S �������T �o�ߗ���(1) �o�ߗ���(2) �o�ߗ���(3) 
           �o�ߗ���(4) �o�ߗ���(5) �K�p�P
           .
           MOVE NC"��" TO
           �P�ƃ`�F�b�N �{�l�`�F�b�N ����`�F�b�N ���σ`�F�b�N ���`�F�b�N �Еۃ`�F�b�N 
           �g���`�F�b�N �P�O���`�F�b�N �X���`�F�b�N �Q���`�F�b�N �U�΃`�F�b�N �W���`�F�b�N 
           �V���`�F�b�N ����`�F�b�N �ސE�`�F�b�N ���ۃ`�F�b�N �Ƒ��`�F�b�N ���V�`�F�b�N 
           �j�`�F�b�N ���`�F�b�N ���a��`�F�b�N�P ���a��`�F�b�N�Q ���a��`�F�b�N�R ���a��`�F�b�N�S
           �����`�F�b�N�P ���~�`�F�b�N�P �]��`�F�b�N�P �����`�F�b�N�Q ���~�`�F�b�N�Q 
           �]��`�F�b�N�Q �����`�F�b�N�R ���~�`�F�b�N�R �]��`�F�b�N�R �����`�F�b�N�S 
           ���~�`�F�b�N�S �]��`�F�b�N�S �����`�F�b�N�T ���~�`�F�b�N�T �]��`�F�b�N�T �V�K�`�F�b�N 
           �p���`�F�b�N �[��`�F�b�N ���ԊO�`�F�b�N �x���`�F�b�N �Œ藿�`�F�b�N �������`�F�b�N 
           �{�×��`�F�b�N ��ԃ`�F�b�N �\���J��`�F�b�N ��H�`�F�b�N 
      *     ���ʃ`�F�b�N �U���`�F�b�N �����`�F�b�N ��s�`�F�b�N ���Ƀ`�F�b�N �_���`�F�b�N 
      *     �{�X�`�F�b�N �x�X�`�F�b�N �{�x���`�F�b�N
           .
041760*
041770*================================================================*
       �{�p���擾 SECTION.
      *
028350     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
028360     MOVE �}�Ԃv�q              TO �{�L�|�}��
028370     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
028380     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
028390     MOVE �{�p���v�q            TO �{�L�|�{�p��
028400     MOVE ZERO                  TO �{�L�|�{�p��
028420     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
028430                                  �{�L�|�{�p�a��N����
028440     END-START
028450     IF ��ԃL�[ = "00"
030910         MOVE SPACE TO �I���t���O�Q
030920         PERFORM �{�p�L�^�e�Ǎ�
030930         PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
030940                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
030950                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
030960                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
030970                       ( �{�L�|�{�p��     NOT = �{�p���v�q      )
                   MOVE NC"��" TO �{�p���`�F�b�N(�{�L�|�{�p��)
                   PERFORM �{�p�L�^�e�Ǎ�
               END-PERFORM
           END-IF.
           PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 31
               MOVE �J�E���^ TO �{�p��(�J�E���^)
           END-PERFORM.
037520*================================================================*
       �����ԍ��擾 SECTION.
      *
           EVALUATE ��|��p���S�Ҕԍ�����
      */����25�N11���{�p�����ύX������/131126
           WHEN "83121095"   MOVE "5610002" TO �����ԍ��v
           WHEN "120014"     MOVE "5610010" TO �����ԍ��v
           WHEN "120022"     MOVE "5610010" TO �����ԍ��v
           WHEN "120030"     MOVE "5610010" TO �����ԍ��v
           WHEN "120048"     MOVE "5610010" TO �����ԍ��v
           WHEN "120055"     MOVE "5610010" TO �����ԍ��v
           WHEN "120063"     MOVE "5610010" TO �����ԍ��v
           WHEN "120071"     MOVE "5610010" TO �����ԍ��v
           WHEN "120089"     MOVE "5610010" TO �����ԍ��v
           WHEN "120097"     MOVE "5610010" TO �����ԍ��v
           WHEN "120105"     MOVE "5610010" TO �����ԍ��v
           WHEN "120113"     MOVE "5610010" TO �����ԍ��v
           WHEN "120121"     MOVE "5610010" TO �����ԍ��v
           WHEN "120139"     MOVE "5610010" TO �����ԍ��v
           WHEN "120147"     MOVE "5610010" TO �����ԍ��v
           WHEN "120162"     MOVE "5610010" TO �����ԍ��v
           WHEN "120170"     MOVE "5610010" TO �����ԍ��v
           WHEN "120188"     MOVE "5610010" TO �����ԍ��v
           WHEN "120196"     MOVE "5610010" TO �����ԍ��v
           WHEN "120204"     MOVE "5610010" TO �����ԍ��v
           WHEN "120212"     MOVE "5610010" TO �����ԍ��v
           WHEN "120220"     MOVE "5610010" TO �����ԍ��v
           WHEN "120238"     MOVE "5610010" TO �����ԍ��v
           WHEN "120246"     MOVE "5610010" TO �����ԍ��v
           WHEN "120253"     MOVE "5610010" TO �����ԍ��v
           WHEN "120261"     MOVE "5610010" TO �����ԍ��v
           WHEN "120451"     MOVE "5610010" TO �����ԍ��v
           WHEN "120519"     MOVE "5610010" TO �����ԍ��v
           WHEN "120527"     MOVE "5610010" TO �����ԍ��v
           WHEN "120535"     MOVE "5610010" TO �����ԍ��v
           WHEN "120543"     MOVE "5610010" TO �����ԍ��v
           WHEN "120550"     MOVE "5610010" TO �����ԍ��v
           WHEN "120568"     MOVE "5610010" TO �����ԍ��v
           WHEN "120576"     MOVE "5610010" TO �����ԍ��v
           WHEN "120584"     MOVE "5610010" TO �����ԍ��v
           WHEN "120592"     MOVE "5610010" TO �����ԍ��v
           WHEN "120600"     MOVE "5610010" TO �����ԍ��v
           WHEN "120618"     MOVE "5610010" TO �����ԍ��v
           WHEN "120626"     MOVE "5610010" TO �����ԍ��v
           WHEN "120634"     MOVE "5610010" TO �����ԍ��v
           WHEN "120642"     MOVE "5610010" TO �����ԍ��v
           WHEN "120659"     MOVE "5610010" TO �����ԍ��v
           WHEN "120667"     MOVE "5610010" TO �����ԍ��v
           WHEN "120675"     MOVE "5610010" TO �����ԍ��v
           WHEN "120683"     MOVE "5610010" TO �����ԍ��v
           WHEN "120691"     MOVE "5610010" TO �����ԍ��v
           WHEN "120709"     MOVE "5610010" TO �����ԍ��v
           WHEN "120717"     MOVE "5610010" TO �����ԍ��v
           WHEN "120725"     MOVE "5610010" TO �����ԍ��v
           WHEN "120733"     MOVE "5610010" TO �����ԍ��v
           WHEN "120741"     MOVE "5610010" TO �����ԍ��v
           WHEN "120758"     MOVE "5610010" TO �����ԍ��v
           WHEN "120766"     MOVE "5610010" TO �����ԍ��v
           WHEN "120774"     MOVE "5610010" TO �����ԍ��v
           WHEN "120782"     MOVE "5610010" TO �����ԍ��v
           WHEN "120790"     MOVE "5610010" TO �����ԍ��v
           WHEN "120808"     MOVE "5610010" TO �����ԍ��v
           WHEN "120816"     MOVE "5610010" TO �����ԍ��v
           WHEN "120824"     MOVE "5610010" TO �����ԍ��v
           WHEN "120832"     MOVE "5610010" TO �����ԍ��v
           WHEN "120840"     MOVE "5610010" TO �����ԍ��v
           WHEN "120857"     MOVE "5610010" TO �����ԍ��v
           WHEN "120865"     MOVE "5610010" TO �����ԍ��v
           WHEN "120873"     MOVE "5610010" TO �����ԍ��v
           WHEN "120881"     MOVE "5610010" TO �����ԍ��v
           WHEN "120899"     MOVE "5610010" TO �����ԍ��v
           WHEN "120907"     MOVE "5610010" TO �����ԍ��v
           WHEN "120915"     MOVE "5610010" TO �����ԍ��v
           WHEN "120923"     MOVE "5610010" TO �����ԍ��v
           WHEN "120931"     MOVE "5610010" TO �����ԍ��v
           WHEN "120949"     MOVE "5610010" TO �����ԍ��v
           WHEN "120956"     MOVE "5610010" TO �����ԍ��v
           WHEN "120964"     MOVE "5610010" TO �����ԍ��v
           WHEN "120972"     MOVE "5610010" TO �����ԍ��v
           WHEN "120980"     MOVE "5610010" TO �����ԍ��v
           WHEN "120998"     MOVE "5610010" TO �����ԍ��v
           WHEN "121004"     MOVE "5610010" TO �����ԍ��v
           WHEN "121012"     MOVE "5610010" TO �����ԍ��v
           WHEN "121020"     MOVE "5610010" TO �����ԍ��v
           WHEN "121038"     MOVE "5610010" TO �����ԍ��v
           WHEN "121046"     MOVE "5610010" TO �����ԍ��v
           WHEN "124008"     MOVE "5610010" TO �����ԍ��v
           WHEN "124016"     MOVE "5610010" TO �����ԍ��v
           WHEN "124024"     MOVE "5610010" TO �����ԍ��v
           WHEN "124032"     MOVE "5610010" TO �����ԍ��v
           WHEN "124040"     MOVE "5610010" TO �����ԍ��v
           WHEN "124057"     MOVE "5610010" TO �����ԍ��v
           WHEN "124065"     MOVE "5610010" TO �����ԍ��v
           WHEN "123018"     MOVE "5610010" TO �����ԍ��v
           WHEN "123026"     MOVE "5610010" TO �����ԍ��v
           WHEN "123034"     MOVE "5610010" TO �����ԍ��v
           WHEN "67120014"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120022"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120030"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120048"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120055"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120063"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120071"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120089"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120097"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120105"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120113"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120121"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120139"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120147"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120154"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120162"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120170"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120188"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120196"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120204"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120212"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120220"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120238"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120246"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120253"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120261"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120519"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120527"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120535"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120543"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120550"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120568"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120576"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120584"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120592"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120600"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120618"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120626"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120634"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120642"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120659"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120667"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120675"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120683"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120691"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120709"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120717"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120725"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120733"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120741"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120758"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120766"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120774"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120782"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120790"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120808"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120816"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120824"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120832"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120840"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120857"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120865"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120873"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120881"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120899"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120907"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120915"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120923"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120931"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120949"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120956"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120964"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120972"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120980"   MOVE "5610010" TO �����ԍ��v
           WHEN "67120998"   MOVE "5610010" TO �����ԍ��v
           WHEN "67121004"   MOVE "5610010" TO �����ԍ��v
           WHEN "67121012"   MOVE "5610010" TO �����ԍ��v
           WHEN "67121020"   MOVE "5610010" TO �����ԍ��v
           WHEN "67121038"   MOVE "5610010" TO �����ԍ��v
           WHEN "67121046"   MOVE "5610010" TO �����ԍ��v
           WHEN "67124016"   MOVE "5610010" TO �����ԍ��v
           WHEN "67124024"   MOVE "5610010" TO �����ԍ��v
           WHEN "67124032"   MOVE "5610010" TO �����ԍ��v
           WHEN "67124040"   MOVE "5610010" TO �����ԍ��v
           WHEN "67124057"   MOVE "5610010" TO �����ԍ��v
           WHEN "67124065"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120013"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120021"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120039"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120047"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120054"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120062"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120070"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120088"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120096"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120104"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120112"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120120"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120138"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120146"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120153"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120161"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120179"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120187"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120195"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120203"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120211"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120229"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120237"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120245"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120252"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120260"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120419"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120518"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120526"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120534"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120542"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120559"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120567"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120575"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120583"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120591"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120609"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120617"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120625"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120633"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120641"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120658"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120666"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120674"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120682"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120690"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120708"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120716"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120724"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120732"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120740"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120757"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120765"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120773"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120781"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120799"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120807"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120815"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120823"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120831"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120849"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120856"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120864"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120872"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120880"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120898"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120906"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120914"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120922"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120930"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120948"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120955"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120963"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120971"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120989"   MOVE "5610010" TO �����ԍ��v
           WHEN "27120997"   MOVE "5610010" TO �����ԍ��v
           WHEN "27121003"   MOVE "5610010" TO �����ԍ��v
           WHEN "27121011"   MOVE "5610010" TO �����ԍ��v
           WHEN "27121029"   MOVE "5610010" TO �����ԍ��v
           WHEN "27121037"   MOVE "5610010" TO �����ԍ��v
           WHEN "27121045"   MOVE "5610010" TO �����ԍ��v
           WHEN "27124007"   MOVE "5610010" TO �����ԍ��v
           WHEN "27124015"   MOVE "5610010" TO �����ԍ��v
           WHEN "27124023"   MOVE "5610010" TO �����ԍ��v
           WHEN "27124031"   MOVE "5610010" TO �����ԍ��v
           WHEN "27124049"   MOVE "5610010" TO �����ԍ��v
           WHEN "27124056"   MOVE "5610010" TO �����ԍ��v
           WHEN "27124064"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120023"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120031"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120049"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120056"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120064"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120072"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120080"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120098"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120106"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120114"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120122"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120130"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120148"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120155"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120163"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120171"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120189"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120197"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120205"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120213"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120221"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120239"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120247"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120254"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120262"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120510"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120528"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120536"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120544"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120551"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120569"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120577"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120585"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120593"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120601"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120619"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120627"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120635"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120643"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120650"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120668"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120676"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120684"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120692"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120700"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120718"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120726"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120734"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120742"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120759"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120767"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120775"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120783"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120791"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120809"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120817"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120825"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120833"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120841"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120858"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120866"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120874"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120882"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120890"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120908"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120916"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120924"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120932"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120940"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120957"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120965"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120973"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120981"   MOVE "5610010" TO �����ԍ��v
           WHEN "41120999"   MOVE "5610010" TO �����ԍ��v
           WHEN "41121005"   MOVE "5610010" TO �����ԍ��v
           WHEN "41121013"   MOVE "5610010" TO �����ԍ��v
           WHEN "41121021"   MOVE "5610010" TO �����ԍ��v
           WHEN "41121039"   MOVE "5610010" TO �����ԍ��v
           WHEN "41121047"   MOVE "5610010" TO �����ԍ��v
           WHEN "41124009"   MOVE "5610010" TO �����ԍ��v
           WHEN "41124017"   MOVE "5610010" TO �����ԍ��v
           WHEN "41124025"   MOVE "5610010" TO �����ԍ��v
           WHEN "41124033"   MOVE "5610010" TO �����ԍ��v
           WHEN "41124041"   MOVE "5610010" TO �����ԍ��v
           WHEN "41124058"   MOVE "5610010" TO �����ԍ��v
           WHEN "41124066"   MOVE "5610010" TO �����ԍ��v
           WHEN "19126010"   MOVE "5610010" TO �����ԍ��v
           WHEN "110684"     MOVE "5610029" TO �����ԍ��v
           WHEN "110700"     MOVE "5610037" TO �����ԍ��v
           WHEN "110783"     MOVE "5610045" TO �����ԍ��v
           WHEN "138529"     MOVE "5610053" TO �����ԍ��v
           WHEN "138586"     MOVE "5610061" TO �����ԍ��v
           WHEN "114108"     MOVE "5610088" TO �����ԍ��v
           WHEN "06139521"   MOVE "5610096" TO �����ԍ��v
           WHEN "120279"     MOVE "5610110" TO �����ԍ��v
           WHEN "110015"     MOVE "5610126" TO �����ԍ��v
           WHEN "110023"     MOVE "5610134" TO �����ԍ��v
           WHEN "110031"     MOVE "5610142" TO �����ԍ��v
           WHEN "67110031"   MOVE "5610142" TO �����ԍ��v
           WHEN "110080"     MOVE "5610150" TO �����ԍ��v
           WHEN "110098"     MOVE "5610169" TO �����ԍ��v
           WHEN "110106"     MOVE "5610177" TO �����ԍ��v
           WHEN "06210595"   MOVE "5610185" TO �����ԍ��v
           WHEN "110148"     MOVE "5610193" TO �����ԍ��v
           WHEN "110213"     MOVE "5610207" TO �����ԍ��v
           WHEN "110221"     MOVE "5610215" TO �����ԍ��v
           WHEN "110296"     MOVE "5610223" TO �����ԍ��v
           WHEN "110346"     MOVE "5610231" TO �����ԍ��v
           WHEN "110353"     MOVE "5610258" TO �����ԍ��v
           WHEN "110361"     MOVE "5610266" TO �����ԍ��v
           WHEN "110379"     MOVE "5610274" TO �����ԍ��v
           WHEN "110403"     MOVE "5610282" TO �����ԍ��v
           WHEN "110411"     MOVE "5610290" TO �����ԍ��v
           WHEN "110429"     MOVE "5610304" TO �����ԍ��v
           WHEN "67110429"   MOVE "5610304" TO �����ԍ��v
           WHEN "27110428"   MOVE "5610304" TO �����ԍ��v
           WHEN "41110420"   MOVE "5610304" TO �����ԍ��v
           WHEN "110437"     MOVE "5610312" TO �����ԍ��v
           WHEN "06380257"   MOVE "5610320" TO �����ԍ��v
           WHEN "06400113"   MOVE "5610339" TO �����ԍ��v
           WHEN "06401095"   MOVE "5610347" TO �����ԍ��v
           WHEN "110841"     MOVE "5610355" TO �����ԍ��v
           WHEN "110890"     MOVE "5610363" TO �����ԍ��v
           WHEN "110908"     MOVE "5610371" TO �����ԍ��v
           WHEN "06330393"   MOVE "5610398" TO �����ԍ��v
           WHEN "06340061"   MOVE "5610401" TO �����ԍ��v
           WHEN "138016"     MOVE "5610428" TO �����ԍ��v
           WHEN "138024"     MOVE "5610436" TO �����ԍ��v
           WHEN "138032"     MOVE "5610444" TO �����ԍ��v
           WHEN "138040"     MOVE "5610452" TO �����ԍ��v
           WHEN "138057"     MOVE "5610460" TO �����ԍ��v
           WHEN "138065"     MOVE "5610479" TO �����ԍ��v
           WHEN "138073"     MOVE "5610487" TO �����ԍ��v
           WHEN "138081"     MOVE "5610495" TO �����ԍ��v
           WHEN "138099"     MOVE "5610509" TO �����ԍ��v
           WHEN "138107"     MOVE "5610517" TO �����ԍ��v
           WHEN "138115"     MOVE "5610525" TO �����ԍ��v
           WHEN "138123"     MOVE "5610533" TO �����ԍ��v
           WHEN "138131"     MOVE "5610541" TO �����ԍ��v
           WHEN "138149"     MOVE "5610568" TO �����ԍ��v
           WHEN "138156"     MOVE "5610576" TO �����ԍ��v
           WHEN "67138156"   MOVE "5610576" TO �����ԍ��v
           WHEN "138164"     MOVE "5610584" TO �����ԍ��v
           WHEN "138172"     MOVE "5610592" TO �����ԍ��v
           WHEN "138180"     MOVE "5610606" TO �����ԍ��v
           WHEN "138198"     MOVE "5610614" TO �����ԍ��v
           WHEN "67138198"   MOVE "5610614" TO �����ԍ��v
           WHEN "138206"     MOVE "5610622" TO �����ԍ��v
           WHEN "138214"     MOVE "5610630" TO �����ԍ��v
           WHEN "138222"     MOVE "5610649" TO �����ԍ��v
           WHEN "67138222"   MOVE "5610649" TO �����ԍ��v
           WHEN "27138221"   MOVE "5610649" TO �����ԍ��v
           WHEN "81136228"   MOVE "5610649" TO �����ԍ��v
           WHEN "81137226"   MOVE "5610649" TO �����ԍ��v
           WHEN "138230"     MOVE "5610657" TO �����ԍ��v
           WHEN "67138230"   MOVE "5610657" TO �����ԍ��v
           WHEN "27138239"   MOVE "5610657" TO �����ԍ��v
           WHEN "81136236"   MOVE "5610657" TO �����ԍ��v
           WHEN "81137234"   MOVE "5610657" TO �����ԍ��v
           WHEN "138248"     MOVE "5610665" TO �����ԍ��v
           WHEN "138313"     MOVE "5610673" TO �����ԍ��v
           WHEN "138321"     MOVE "5610681" TO �����ԍ��v
           WHEN "138347"     MOVE "5610703" TO �����ԍ��v
           WHEN "138354"     MOVE "5610711" TO �����ԍ��v
           WHEN "138396"     MOVE "5610738" TO �����ԍ��v
           WHEN "138479"     MOVE "5610746" TO �����ԍ��v
           WHEN "138487"     MOVE "5610754" TO �����ԍ��v
           WHEN "140038"     MOVE "5610762" TO �����ԍ��v
           WHEN "140046"     MOVE "5610762" TO �����ԍ��v
           WHEN "140053"     MOVE "5610762" TO �����ԍ��v
           WHEN "140061"     MOVE "5610762" TO �����ԍ��v
           WHEN "140079"     MOVE "5610762" TO �����ԍ��v
           WHEN "140087"     MOVE "5610762" TO �����ԍ��v
           WHEN "140095"     MOVE "5610762" TO �����ԍ��v
           WHEN "140103"     MOVE "5610762" TO �����ԍ��v
           WHEN "140111"     MOVE "5610762" TO �����ԍ��v
           WHEN "140129"     MOVE "5610762" TO �����ԍ��v
           WHEN "140137"     MOVE "5610762" TO �����ԍ��v
           WHEN "140145"     MOVE "5610762" TO �����ԍ��v
           WHEN "140152"     MOVE "5610762" TO �����ԍ��v
           WHEN "140160"     MOVE "5610762" TO �����ԍ��v
           WHEN "140178"     MOVE "5610762" TO �����ԍ��v
           WHEN "140186"     MOVE "5610762" TO �����ԍ��v
           WHEN "140517"     MOVE "5610762" TO �����ԍ��v
           WHEN "140525"     MOVE "5610762" TO �����ԍ��v
           WHEN "140533"     MOVE "5610762" TO �����ԍ��v
           WHEN "140541"     MOVE "5610762" TO �����ԍ��v
           WHEN "140558"     MOVE "5610762" TO �����ԍ��v
           WHEN "140566"     MOVE "5610762" TO �����ԍ��v
           WHEN "140574"     MOVE "5610762" TO �����ԍ��v
           WHEN "140582"     MOVE "5610762" TO �����ԍ��v
           WHEN "140590"     MOVE "5610762" TO �����ԍ��v
           WHEN "140608"     MOVE "5610762" TO �����ԍ��v
           WHEN "140616"     MOVE "5610762" TO �����ԍ��v
           WHEN "140624"     MOVE "5610762" TO �����ԍ��v
           WHEN "140632"     MOVE "5610762" TO �����ԍ��v
           WHEN "140640"     MOVE "5610762" TO �����ԍ��v
           WHEN "140657"     MOVE "5610762" TO �����ԍ��v
           WHEN "140665"     MOVE "5610762" TO �����ԍ��v
           WHEN "140673"     MOVE "5610762" TO �����ԍ��v
           WHEN "140681"     MOVE "5610762" TO �����ԍ��v
           WHEN "140699"     MOVE "5610762" TO �����ԍ��v
           WHEN "144006"     MOVE "5610762" TO �����ԍ��v
           WHEN "144014"     MOVE "5610762" TO �����ԍ��v
           WHEN "144022"     MOVE "5610762" TO �����ԍ��v
           WHEN "144030"     MOVE "5610762" TO �����ԍ��v
           WHEN "144048"     MOVE "5610762" TO �����ԍ��v
           WHEN "144055"     MOVE "5610762" TO �����ԍ��v
           WHEN "144063"     MOVE "5610762" TO �����ԍ��v
           WHEN "144071"     MOVE "5610762" TO �����ԍ��v
           WHEN "144089"     MOVE "5610762" TO �����ԍ��v
           WHEN "144097"     MOVE "5610762" TO �����ԍ��v
           WHEN "144105"     MOVE "5610762" TO �����ԍ��v
           WHEN "144113"     MOVE "5610762" TO �����ԍ��v
           WHEN "144121"     MOVE "5610762" TO �����ԍ��v
           WHEN "144139"     MOVE "5610762" TO �����ԍ��v
           WHEN "144147"     MOVE "5610762" TO �����ԍ��v
           WHEN "144154"     MOVE "5610762" TO �����ԍ��v
           WHEN "144162"     MOVE "5610762" TO �����ԍ��v
           WHEN "144170"     MOVE "5610762" TO �����ԍ��v
           WHEN "144188"     MOVE "5610762" TO �����ԍ��v
           WHEN "145003"     MOVE "5610762" TO �����ԍ��v
           WHEN "145011"     MOVE "5610762" TO �����ԍ��v
           WHEN "145029"     MOVE "5610762" TO �����ԍ��v
           WHEN "145037"     MOVE "5610762" TO �����ԍ��v
           WHEN "145045"     MOVE "5610762" TO �����ԍ��v
           WHEN "145052"     MOVE "5610762" TO �����ԍ��v
           WHEN "145060"     MOVE "5610762" TO �����ԍ��v
           WHEN "145078"     MOVE "5610762" TO �����ԍ��v
           WHEN "146001"     MOVE "5610762" TO �����ԍ��v
           WHEN "146019"     MOVE "5610762" TO �����ԍ��v
           WHEN "146027"     MOVE "5610762" TO �����ԍ��v
           WHEN "146035"     MOVE "5610762" TO �����ԍ��v
           WHEN "143016"     MOVE "5610762" TO �����ԍ��v
           WHEN "143024"     MOVE "5610762" TO �����ԍ��v
           WHEN "143032"     MOVE "5610762" TO �����ԍ��v
           WHEN "143040"     MOVE "5610762" TO �����ԍ��v
           WHEN "143057"     MOVE "5610762" TO �����ԍ��v
           WHEN "143065"     MOVE "5610762" TO �����ԍ��v
           WHEN "67140038"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140046"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140053"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140061"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140079"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140087"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140095"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140103"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140111"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140129"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140137"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140145"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140152"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140160"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140178"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140186"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140517"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140525"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140533"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140541"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140558"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140566"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140574"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140582"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140590"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140608"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140616"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140624"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140632"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140640"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140657"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140665"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140673"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140681"   MOVE "5610762" TO �����ԍ��v
           WHEN "67140699"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144014"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144022"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144030"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144048"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144055"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144063"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144071"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144089"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144097"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144105"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144113"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144121"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144139"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144147"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144154"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144162"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144170"   MOVE "5610762" TO �����ԍ��v
           WHEN "67144188"   MOVE "5610762" TO �����ԍ��v
           WHEN "67145011"   MOVE "5610762" TO �����ԍ��v
           WHEN "67145029"   MOVE "5610762" TO �����ԍ��v
           WHEN "67145037"   MOVE "5610762" TO �����ԍ��v
           WHEN "67145045"   MOVE "5610762" TO �����ԍ��v
           WHEN "67145060"   MOVE "5610762" TO �����ԍ��v
           WHEN "67145078"   MOVE "5610762" TO �����ԍ��v
           WHEN "67146019"   MOVE "5610762" TO �����ԍ��v
           WHEN "67146027"   MOVE "5610762" TO �����ԍ��v
           WHEN "67146035"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140037"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140045"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140052"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140060"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140078"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140086"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140094"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140102"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140110"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140128"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140136"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140144"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140151"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140169"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140177"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140185"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140516"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140524"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140532"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140540"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140557"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140565"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140573"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140581"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140599"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140607"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140615"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140623"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140631"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140649"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140656"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140664"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140672"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140680"   MOVE "5610762" TO �����ԍ��v
           WHEN "27140698"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144005"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144013"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144021"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144039"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144047"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144054"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144062"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144070"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144088"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144096"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144104"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144112"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144120"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144138"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144146"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144153"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144161"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144179"   MOVE "5610762" TO �����ԍ��v
           WHEN "27144187"   MOVE "5610762" TO �����ԍ��v
           WHEN "27145002"   MOVE "5610762" TO �����ԍ��v
           WHEN "27145010"   MOVE "5610762" TO �����ԍ��v
           WHEN "27145028"   MOVE "5610762" TO �����ԍ��v
           WHEN "27145036"   MOVE "5610762" TO �����ԍ��v
           WHEN "27145044"   MOVE "5610762" TO �����ԍ��v
           WHEN "27145051"   MOVE "5610762" TO �����ԍ��v
           WHEN "27145069"   MOVE "5610762" TO �����ԍ��v
           WHEN "27145077"   MOVE "5610762" TO �����ԍ��v
           WHEN "1914601"    MOVE "5610762" TO �����ԍ��v
           WHEN "19146018"   MOVE "5610762" TO �����ԍ��v
           WHEN "113027"     MOVE "5610770" TO �����ԍ��v
           WHEN "113043"     MOVE "5610789" TO �����ԍ��v
           WHEN "113050"     MOVE "5610797" TO �����ԍ��v
           WHEN "113068"     MOVE "5610800" TO �����ԍ��v
           WHEN "133033"     MOVE "5610819" TO �����ԍ��v
           WHEN "133041"     MOVE "5610827" TO �����ԍ��v
           WHEN "133066"     MOVE "5610835" TO �����ԍ��v
           WHEN "133074"     MOVE "5610843" TO �����ԍ��v
           WHEN "133090"     MOVE "5610851" TO �����ԍ��v
           WHEN "133132"     MOVE "5610878" TO �����ԍ��v
           WHEN "133140"     MOVE "5610886" TO �����ԍ��v
           WHEN "133157"     MOVE "5610894" TO �����ԍ��v
           WHEN "133165"     MOVE "5610908" TO �����ԍ��v
           WHEN "133173"     MOVE "5610916" TO �����ԍ��v
           WHEN "133199"     MOVE "5610924" TO �����ԍ��v
           WHEN "133207"     MOVE "5610932" TO �����ԍ��v
           WHEN "133223"     MOVE "5610940" TO �����ԍ��v
           WHEN "133231"     MOVE "5610959" TO �����ԍ��v
           WHEN "133249"     MOVE "5610967" TO �����ԍ��v
           WHEN "133256"     MOVE "5610975" TO �����ԍ��v
           WHEN "133264"     MOVE "5610983" TO �����ԍ��v
           WHEN "133272"     MOVE "5610991" TO �����ԍ��v
           WHEN "133298"     MOVE "5611009" TO �����ԍ��v
           WHEN "67110106"   MOVE "5611017" TO �����ԍ��v
           WHEN "67110148"   MOVE "5611025" TO �����ԍ��v
           WHEN "67110213"   MOVE "5611033" TO �����ԍ��v
           WHEN "67110221"   MOVE "5611041" TO �����ԍ��v
           WHEN "06272512"   MOVE "5611068" TO �����ԍ��v
           WHEN "67110361"   MOVE "5611076" TO �����ԍ��v
           WHEN "67110387"   MOVE "5611084" TO �����ԍ��v
           WHEN "06272843"   MOVE "5611092" TO �����ԍ��v
           WHEN "67110841"   MOVE "5611106" TO �����ԍ��v
           WHEN "06272868"   MOVE "5611114" TO �����ԍ��v
           WHEN "06273262"   MOVE "5611122" TO �����ԍ��v
           WHEN "67138016"   MOVE "5611130" TO �����ԍ��v
           WHEN "67138024"   MOVE "5611149" TO �����ԍ��v
           WHEN "67138032"   MOVE "5611157" TO �����ԍ��v
           WHEN "67138040"   MOVE "5611165" TO �����ԍ��v
           WHEN "67138057"   MOVE "5611173" TO �����ԍ��v
           WHEN "67138065"   MOVE "5611181" TO �����ԍ��v
           WHEN "67138073"   MOVE "5611203" TO �����ԍ��v
           WHEN "67138081"   MOVE "5611211" TO �����ԍ��v
           WHEN "67138099"   MOVE "5611238" TO �����ԍ��v
           WHEN "67138107"   MOVE "5611246" TO �����ԍ��v
           WHEN "67138115"   MOVE "5611254" TO �����ԍ��v
           WHEN "67138123"   MOVE "5611262" TO �����ԍ��v
           WHEN "67138131"   MOVE "5611270" TO �����ԍ��v
           WHEN "67138149"   MOVE "5611289" TO �����ԍ��v
           WHEN "67138164"   MOVE "5611297" TO �����ԍ��v
           WHEN "67138172"   MOVE "5611300" TO �����ԍ��v
           WHEN "67138180"   MOVE "5611319" TO �����ԍ��v
           WHEN "67138206"   MOVE "5611327" TO �����ԍ��v
           WHEN "67138214"   MOVE "5611335" TO �����ԍ��v
           WHEN "67110023"   MOVE "5611343" TO �����ԍ��v
           WHEN "67110072"   MOVE "5611351" TO �����ԍ��v
           WHEN "67138313"   MOVE "5611378" TO �����ԍ��v
           WHEN "67138354"   MOVE "5611386" TO �����ԍ��v
           WHEN "67138362"   MOVE "5611394" TO �����ԍ��v
           WHEN "67138479"   MOVE "5611408" TO �����ԍ��v
           WHEN "67138487"   MOVE "5611416" TO �����ԍ��v
           WHEN "06141519"   MOVE "5611424" TO �����ԍ��v
           WHEN "67110098"   MOVE "5611432" TO �����ԍ��v
           WHEN "67110320"   MOVE "5611440" TO �����ԍ��v
           WHEN "67110445"   MOVE "5611459" TO �����ԍ��v
           WHEN "67110551"   MOVE "5611467" TO �����ԍ��v
           WHEN "06110084"   MOVE "5611475" TO �����ԍ��v
           WHEN "06110449"   MOVE "5611483" TO �����ԍ��v
           WHEN "06120018"   MOVE "5611491" TO �����ԍ��v
           WHEN "06120695"   MOVE "5611505" TO �����ԍ��v
           WHEN "63120695"   MOVE "5611505" TO �����ԍ��v
           WHEN "06120760"   MOVE "5611513" TO �����ԍ��v
           WHEN "06130173"   MOVE "5611521" TO �����ԍ��v
           WHEN "06130538"   MOVE "5611548" TO �����ԍ��v
           WHEN "06130835"   MOVE "5611556" TO �����ԍ��v
           WHEN "27110378"   MOVE "5611564" TO �����ԍ��v
           WHEN "06131320"   MOVE "5611580" TO �����ԍ��v
           WHEN "06271696"   MOVE "5611599" TO �����ԍ��v
           WHEN "06272587"   MOVE "5611602" TO �����ԍ��v
           WHEN "06273148"   MOVE "5611610" TO �����ԍ��v
           WHEN "06280838"   MOVE "5611629" TO �����ԍ��v
           WHEN "06281281"   MOVE "5611637" TO �����ԍ��v
           WHEN "06281448"   MOVE "5611645" TO �����ԍ��v
           WHEN "06281596"   MOVE "5611653" TO �����ԍ��v
           WHEN "06230023"   MOVE "5611661" TO �����ԍ��v
           WHEN "06230205"   MOVE "5611688" TO �����ԍ��v
           WHEN "06230221"   MOVE "5611696" TO �����ԍ��v
           WHEN "06230239"   MOVE "5611718" TO �����ԍ��v
           WHEN "06230395"   MOVE "5611726" TO �����ԍ��v
           WHEN "06230684"   MOVE "5611734" TO �����ԍ��v
           WHEN "06230692"   MOVE "5611742" TO �����ԍ��v
           WHEN "06230890"   MOVE "5611750" TO �����ԍ��v
           WHEN "06231518"   MOVE "5611769" TO �����ԍ��v
           WHEN "06231534"   MOVE "5611777" TO �����ԍ��v
           WHEN "06231591"   MOVE "5611785" TO �����ԍ��v
           WHEN "06231625"   MOVE "5611793" TO �����ԍ��v
           WHEN "06231773"   MOVE "5611807" TO �����ԍ��v
           WHEN "27138213"   MOVE "5611815" TO �����ԍ��v
           WHEN "81136210"   MOVE "5611815" TO �����ԍ��v
           WHEN "81137218"   MOVE "5611815" TO �����ԍ��v
           WHEN "88132212"   MOVE "5611815" TO �����ԍ��v
           WHEN "88138219"   MOVE "5611815" TO �����ԍ��v
           WHEN "67110726"   MOVE "5611823" TO �����ԍ��v
           WHEN "27138320"   MOVE "5611831" TO �����ԍ��v
           WHEN "27138353"   MOVE "5611858" TO �����ԍ��v
           WHEN "27138395"   MOVE "5611866" TO �����ԍ��v
           WHEN "27138478"   MOVE "5611874" TO �����ԍ��v
           WHEN "67138248"   MOVE "5611882" TO �����ԍ��v
           WHEN "67138255"   MOVE "5611890" TO �����ԍ��v
           WHEN "41140104"   MOVE "5611904" TO �����ԍ��v
           WHEN "80140106"   MOVE "5611904" TO �����ԍ��v
           WHEN "67138297"   MOVE "5611912" TO �����ԍ��v
           WHEN "67138305"   MOVE "5611920" TO �����ԍ��v
           WHEN "67138453"   MOVE "5611939" TO �����ԍ��v
           WHEN "67138602"   MOVE "5611947" TO �����ԍ��v
           WHEN "67145052"   MOVE "5611955" TO �����ԍ��v
           WHEN "110155"     MOVE "5611963" TO �����ԍ��v
           WHEN "110197"     MOVE "5611971" TO �����ԍ��v
           WHEN "110239"     MOVE "5611998" TO �����ԍ��v
           WHEN "110247"     MOVE "5612005" TO �����ԍ��v
           WHEN "110288"     MOVE "5612013" TO �����ԍ��v
           WHEN "110320"     MOVE "5612021" TO �����ԍ��v
           WHEN "110858"     MOVE "5612048" TO �����ԍ��v
           WHEN "110866"     MOVE "5612056" TO �����ԍ��v
           WHEN "110882"     MOVE "5612064" TO �����ԍ��v
           WHEN "110916"     MOVE "5612072" TO �����ԍ��v
           WHEN "67110916"   MOVE "5612072" TO �����ԍ��v
           WHEN "110924"     MOVE "5612080" TO �����ԍ��v
           WHEN "133280"     MOVE "5612099" TO �����ԍ��v
           WHEN "138305"     MOVE "5612102" TO �����ԍ��v
           WHEN "138370"     MOVE "5612110" TO �����ԍ��v
           WHEN "138420"     MOVE "5612129" TO �����ԍ��v
           WHEN "138644"     MOVE "5612137" TO �����ԍ��v
           WHEN "06120212"   MOVE "5612145" TO �����ԍ��v
           WHEN "06132104"   MOVE "5612153" TO �����ԍ��v
           WHEN "06133003"   MOVE "5612188" TO �����ԍ��v
           WHEN "06133524"   MOVE "5612196" TO �����ԍ��v
           WHEN "06135479"   MOVE "5612218" TO �����ԍ��v
           WHEN "06136360"   MOVE "5612226" TO �����ԍ��v
           WHEN "06136618"   MOVE "5612234" TO �����ԍ��v
           WHEN "06136907"   MOVE "5612242" TO �����ԍ��v
           WHEN "67110262"   MOVE "5612250" TO �����ԍ��v
           WHEN "67110940"   MOVE "5612269" TO �����ԍ��v
           WHEN "67114041"   MOVE "5612277" TO �����ԍ��v
           WHEN "67138321"   MOVE "5612285" TO �����ԍ��v
           WHEN "110064"     MOVE "5612293" TO �����ԍ��v
           WHEN "110122"     MOVE "5612307" TO �����ԍ��v
           WHEN "110254"     MOVE "5612315" TO �����ԍ��v
           WHEN "110262"     MOVE "5612323" TO �����ԍ��v
           WHEN "110270"     MOVE "5612331" TO �����ԍ��v
           WHEN "110304"     MOVE "5612358" TO �����ԍ��v
           WHEN "110312"     MOVE "5612366" TO �����ԍ��v
           WHEN "110387"     MOVE "5612374" TO �����ԍ��v
           WHEN "110395"     MOVE "5612382" TO �����ԍ��v
           WHEN "110478"     MOVE "5612390" TO �����ԍ��v
           WHEN "110510"     MOVE "5612404" TO �����ԍ��v
           WHEN "06273718"   MOVE "5612412" TO �����ԍ��v
           WHEN "06280119"   MOVE "5612420" TO �����ԍ��v
           WHEN "06280127"   MOVE "5612439" TO �����ԍ��v
           WHEN "114025"     MOVE "5612447" TO �����ԍ��v
           WHEN "114033"     MOVE "5612455" TO �����ԍ��v
           WHEN "114041"     MOVE "5612463" TO �����ԍ��v
           WHEN "114058"     MOVE "5612471" TO �����ԍ��v
           WHEN "114066"     MOVE "5612498" TO �����ԍ��v
           WHEN "114074"     MOVE "5612501" TO �����ԍ��v
           WHEN "114082"     MOVE "5612528" TO �����ԍ��v
           WHEN "114090"     MOVE "5612536" TO �����ԍ��v
           WHEN "138255"     MOVE "5612544" TO �����ԍ��v
           WHEN "138263"     MOVE "5612552" TO �����ԍ��v
           WHEN "138271"     MOVE "5612560" TO �����ԍ��v
           WHEN "138289"     MOVE "5612579" TO �����ԍ��v
           WHEN "138297"     MOVE "5612587" TO �����ԍ��v
           WHEN "138339"     MOVE "5612595" TO �����ԍ��v
           WHEN "138412"     MOVE "5612609" TO �����ԍ��v
           WHEN "138438"     MOVE "5612617" TO �����ԍ��v
           WHEN "138453"     MOVE "5612625" TO �����ԍ��v
           WHEN "138503"     MOVE "5612633" TO �����ԍ��v
           WHEN "138552"     MOVE "5612641" TO �����ԍ��v
           WHEN "138602"     MOVE "5612668" TO �����ԍ��v
           WHEN "06137715"   MOVE "5612676" TO �����ԍ��v
           WHEN "06137780"   MOVE "5612684" TO �����ԍ��v
           WHEN "27110220"   MOVE "5612692" TO �����ԍ��v
           WHEN "06137988"   MOVE "5612706" TO �����ԍ��v
           WHEN "06138275"   MOVE "5612714" TO �����ԍ��v
           WHEN "06139166"   MOVE "5612722" TO �����ԍ��v
           WHEN "06139299"   MOVE "5612730" TO �����ԍ��v
           WHEN "06139356"   MOVE "5612749" TO �����ԍ��v
           WHEN "63139356"   MOVE "5612749" TO �����ԍ��v
           WHEN "06139406"   MOVE "5612757" TO �����ԍ��v
           WHEN "06139414"   MOVE "5612765" TO �����ԍ��v
           WHEN "67114108"   MOVE "5612773" TO �����ԍ��v
           WHEN "06139547"   MOVE "5612781" TO �����ԍ��v
           WHEN "06139554"   MOVE "5612803" TO �����ԍ��v
           WHEN "06140156"   MOVE "5612811" TO �����ԍ��v
           WHEN "06140248"   MOVE "5612838" TO �����ԍ��v
           WHEN "63140248"   MOVE "5612838" TO �����ԍ��v
           WHEN "06140305"   MOVE "5612846" TO �����ԍ��v
           WHEN "63140305"   MOVE "5612846" TO �����ԍ��v
           WHEN "06140859"   MOVE "5612854" TO �����ԍ��v
           WHEN "06141261"   MOVE "5612862" TO �����ԍ��v
           WHEN "06141303"   MOVE "5612870" TO �����ԍ��v
           WHEN "06141493"   MOVE "5612889" TO �����ԍ��v
           WHEN "06141550"   MOVE "5612897" TO �����ԍ��v
           WHEN "27138247"   MOVE "5612900" TO �����ԍ��v
           WHEN "27138312"   MOVE "5612919" TO �����ԍ��v
           WHEN "67110122"   MOVE "5612927" TO �����ԍ��v
           WHEN "67110130"   MOVE "5612935" TO �����ԍ��v
           WHEN "67110155"   MOVE "5612943" TO �����ԍ��v
           WHEN "67110197"   MOVE "5612951" TO �����ԍ��v
           WHEN "67110247"   MOVE "5612978" TO �����ԍ��v
           WHEN "67110270"   MOVE "5612986" TO �����ԍ��v
           WHEN "67110288"   MOVE "5612994" TO �����ԍ��v
           WHEN "67110304"   MOVE "5613001" TO �����ԍ��v
           WHEN "67110346"   MOVE "5613028" TO �����ԍ��v
           WHEN "67110353"   MOVE "5613036" TO �����ԍ��v
           WHEN "67110379"   MOVE "5613044" TO �����ԍ��v
           WHEN "67110437"   MOVE "5613052" TO �����ԍ��v
           WHEN "67110908"   MOVE "5613060" TO �����ԍ��v
           WHEN "67110924"   MOVE "5613079" TO �����ԍ��v
           WHEN "67114017"   MOVE "5613087" TO �����ԍ��v
           WHEN "67114025"   MOVE "5613095" TO �����ԍ��v
           WHEN "67114033"   MOVE "5613109" TO �����ԍ��v
           WHEN "67114058"   MOVE "5613117" TO �����ԍ��v
           WHEN "67114066"   MOVE "5613125" TO �����ԍ��v
           WHEN "67114074"   MOVE "5613133" TO �����ԍ��v
           WHEN "67114082"   MOVE "5613141" TO �����ԍ��v
           WHEN "67138347"   MOVE "5613176" TO �����ԍ��v
           WHEN "67138370"   MOVE "5613184" TO �����ԍ��v
           WHEN "67138461"   MOVE "5613192" TO �����ԍ��v
           WHEN "67138511"   MOVE "5613206" TO �����ԍ��v
           WHEN "3102"       MOVE "5613214" TO �����ԍ��v
           WHEN "02110104"   MOVE "5613222" TO �����ԍ��v
           WHEN "110072"     MOVE "5613230" TO �����ԍ��v
           WHEN "110114"     MOVE "5613249" TO �����ԍ��v
           WHEN "110163"     MOVE "5613257" TO �����ԍ��v
           WHEN "110171"     MOVE "5613265" TO �����ԍ��v
           WHEN "67110171"   MOVE "5613265" TO �����ԍ��v
           WHEN "110189"     MOVE "5613273" TO �����ԍ��v
           WHEN "110338"     MOVE "5613281" TO �����ԍ��v
           WHEN "110445"     MOVE "5613303" TO �����ԍ��v
           WHEN "110452"     MOVE "5613311" TO �����ԍ��v
           WHEN "110460"     MOVE "5613338" TO �����ԍ��v
           WHEN "06340186"   MOVE "5613346" TO �����ԍ��v
           WHEN "110494"     MOVE "5613354" TO �����ԍ��v
           WHEN "110502"     MOVE "5613362" TO �����ԍ��v
           WHEN "110528"     MOVE "5613370" TO �����ԍ��v
           WHEN "06340319"   MOVE "5613389" TO �����ԍ��v
           WHEN "110544"     MOVE "5613397" TO �����ԍ��v
           WHEN "110551"     MOVE "5613400" TO �����ԍ��v
           WHEN "110569"     MOVE "5613419" TO �����ԍ��v
           WHEN "06350086"   MOVE "5613427" TO �����ԍ��v
           WHEN "06360119"   MOVE "5613435" TO �����ԍ��v
           WHEN "06380208"   MOVE "5613443" TO �����ԍ��v
           WHEN "113019"     MOVE "5613451" TO �����ԍ��v
           WHEN "113035"     MOVE "5613478" TO �����ԍ��v
           WHEN "133116"     MOVE "5613486" TO �����ԍ��v
           WHEN "138362"     MOVE "5613494" TO �����ԍ��v
           WHEN "138446"     MOVE "5613508" TO �����ԍ��v
           WHEN "138461"     MOVE "5613516" TO �����ԍ��v
           WHEN "138495"     MOVE "5613524" TO �����ԍ��v
           WHEN "06260038"   MOVE "5613532" TO �����ԍ��v
           WHEN "27110196"   MOVE "5613540" TO �����ԍ��v
           WHEN "06141659"   MOVE "5613559" TO �����ԍ��v
           WHEN "06141881"   MOVE "5613567" TO �����ԍ��v
           WHEN "06142095"   MOVE "5613575" TO �����ԍ��v
           WHEN "06142152"   MOVE "5613583" TO �����ԍ��v
           WHEN "06160295"   MOVE "5613591" TO �����ԍ��v
           WHEN "27138262"   MOVE "5613605" TO �����ԍ��v
           WHEN "06221071"   MOVE "5613613" TO �����ԍ��v
           WHEN "67110015"   MOVE "5613621" TO �����ԍ��v
           WHEN "67110080"   MOVE "5613648" TO �����ԍ��v
           WHEN "67110296"   MOVE "5613656" TO �����ԍ��v
           WHEN "67110494"   MOVE "5613664" TO �����ԍ��v
           WHEN "67110544"   MOVE "5613672" TO �����ԍ��v
           WHEN "67110569"   MOVE "5613680" TO �����ԍ��v
           WHEN "06231948"   MOVE "5613699" TO �����ԍ��v
           WHEN "67110858"   MOVE "5613702" TO �����ԍ��v
           WHEN "67138271"   MOVE "5613710" TO �����ԍ��v
           WHEN "67138396"   MOVE "5613729" TO �����ԍ��v
           WHEN "67138420"   MOVE "5613737" TO �����ԍ��v
           WHEN "06139224"   MOVE "5613745" TO �����ԍ��v
           WHEN "06139240"   MOVE "5613753" TO �����ԍ��v
           WHEN "06120786"   MOVE "5613761" TO �����ԍ��v
           WHEN "06139273"   MOVE "5613788" TO �����ԍ��v
           WHEN "31130552"   MOVE "5613796" TO �����ԍ��v
           WHEN "31110364"   MOVE "5613818" TO �����ԍ��v
           WHEN "06250054"   MOVE "5613826" TO �����ԍ��v
           WHEN "27110212"   MOVE "5613834" TO �����ԍ��v
           WHEN "06231013"   MOVE "5613842" TO �����ԍ��v
           WHEN "63231013"   MOVE "5613842" TO �����ԍ��v
           WHEN "06230627"   MOVE "5613850" TO �����ԍ��v
           WHEN "06231062"   MOVE "5613869" TO �����ԍ��v
           WHEN "06231856"   MOVE "5613877" TO �����ԍ��v
           WHEN "06260384"   MOVE "5613885" TO �����ԍ��v
           WHEN "63260384"   MOVE "5613885" TO �����ԍ��v
           WHEN "27138296"   MOVE "5613893" TO �����ԍ��v
           WHEN "06260616"   MOVE "5613907" TO �����ԍ��v
           WHEN "06270052"   MOVE "5613915" TO �����ԍ��v
           WHEN "06270326"   MOVE "5613923" TO �����ԍ��v
           WHEN "06400634"   MOVE "5613168" TO �����ԍ��v
           WHEN "06400477"   MOVE "5613931" TO �����ԍ��v
           WHEN "06401160"   MOVE "5613958" TO �����ԍ��v
           WHEN "06450019"   MOVE "5613966" TO �����ԍ��v
           WHEN "31110281"   MOVE "5613974" TO �����ԍ��v
           WHEN "32140311"   MOVE "5613982" TO �����ԍ��v
           WHEN "32400327"   MOVE "5613990" TO �����ԍ��v
           WHEN "32270415"   MOVE "5614008" TO �����ԍ��v
           WHEN "06271704"   MOVE "5614016" TO �����ԍ��v
           WHEN "06271761"   MOVE "5614024" TO �����ԍ��v
           WHEN "06272017"   MOVE "5614032" TO �����ԍ��v
           WHEN "06272165"   MOVE "5614040" TO �����ԍ��v
           WHEN "06272272"   MOVE "5614059" TO �����ԍ��v
           WHEN "06272439"   MOVE "5614067" TO �����ԍ��v
           WHEN "06272454"   MOVE "5614075" TO �����ԍ��v
           WHEN "39011002"   MOVE "5614083" TO �����ԍ��v
           WHEN "39011010"   MOVE "5614083" TO �����ԍ��v
           WHEN "39011028"   MOVE "5614083" TO �����ԍ��v
           WHEN "39011036"   MOVE "5614083" TO �����ԍ��v
           WHEN "39011044"   MOVE "5614083" TO �����ԍ��v
           WHEN "39011051"   MOVE "5614083" TO �����ԍ��v
           WHEN "39011069"   MOVE "5614083" TO �����ԍ��v
           WHEN "39011077"   MOVE "5614083" TO �����ԍ��v
           WHEN "39011085"   MOVE "5614083" TO �����ԍ��v
           WHEN "39011093"   MOVE "5614083" TO �����ԍ��v
           WHEN "39011101"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012026"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012034"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012042"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012059"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012067"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012075"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012083"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012091"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012109"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012117"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012125"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012133"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012141"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012158"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012166"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012174"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012182"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012190"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012208"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012216"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012224"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012232"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012240"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012257"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012265"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012273"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012281"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012299"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012307"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012315"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012331"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012349"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012356"   MOVE "5614083" TO �����ԍ��v
           WHEN "39012364"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013032"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013040"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013313"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013321"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013339"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013347"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013370"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013438"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013453"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013461"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013479"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013610"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013628"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013636"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013644"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013677"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013701"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013719"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013917"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013925"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013933"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013941"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013958"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013966"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013974"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013982"   MOVE "5614083" TO �����ԍ��v
           WHEN "39013990"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014006"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014014"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014022"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014030"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014048"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014055"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014063"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014071"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014089"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014097"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014238"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014246"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014253"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014279"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014287"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014295"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014303"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014311"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014329"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014337"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014345"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014360"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014378"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014386"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014394"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014527"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014535"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014543"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014550"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014568"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014576"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014584"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014592"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014600"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014618"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014626"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014634"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014642"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014659"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014683"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014691"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014709"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014717"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014816"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014824"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014832"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014840"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014857"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014865"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014873"   MOVE "5614083" TO �����ԍ��v
           WHEN "39014881"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015110"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015128"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015136"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015144"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015169"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015177"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015185"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015193"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015433"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015441"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015458"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015466"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015474"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015490"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015508"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015524"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015557"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015581"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015599"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015607"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015615"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015623"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015631"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015649"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015714"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015755"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015789"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015813"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015847"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015854"   MOVE "5614083" TO �����ԍ��v
           WHEN "39015862"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016019"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016027"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016043"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016076"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016084"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016092"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016100"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016316"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016324"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016332"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016340"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016357"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016365"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016373"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016381"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016399"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016415"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016423"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016431"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016449"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016456"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016464"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016472"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016480"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016498"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016613"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016621"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016639"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016647"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016654"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016670"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016688"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016910"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016928"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016936"   MOVE "5614083" TO �����ԍ��v
           WHEN "39016944"   MOVE "5614083" TO �����ԍ��v
           WHEN "39041017"   MOVE "5614091" TO �����ԍ��v
           WHEN "39041025"   MOVE "5614091" TO �����ԍ��v
           WHEN "39041033"   MOVE "5614091" TO �����ԍ��v
           WHEN "39041041"   MOVE "5614091" TO �����ԍ��v
           WHEN "39041058"   MOVE "5614091" TO �����ԍ��v
           WHEN "39042023"   MOVE "5614091" TO �����ԍ��v
           WHEN "39042031"   MOVE "5614091" TO �����ԍ��v
           WHEN "39042056"   MOVE "5614091" TO �����ԍ��v
           WHEN "39042064"   MOVE "5614091" TO �����ԍ��v
           WHEN "39042072"   MOVE "5614091" TO �����ԍ��v
           WHEN "39042080"   MOVE "5614091" TO �����ԍ��v
           WHEN "39042098"   MOVE "5614091" TO �����ԍ��v
           WHEN "39042114"   MOVE "5614091" TO �����ԍ��v
           WHEN "39042122"   MOVE "5614091" TO �����ԍ��v
           WHEN "39042130"   MOVE "5614091" TO �����ԍ��v
           WHEN "39042148"   MOVE "5614091" TO �����ԍ��v
           WHEN "39042155"   MOVE "5614091" TO �����ԍ��v
           WHEN "39043013"   MOVE "5614091" TO �����ԍ��v
           WHEN "39043021"   MOVE "5614091" TO �����ԍ��v
           WHEN "39043211"   MOVE "5614091" TO �����ԍ��v
           WHEN "39043229"   MOVE "5614091" TO �����ԍ��v
           WHEN "39043237"   MOVE "5614091" TO �����ԍ��v
           WHEN "39043245"   MOVE "5614091" TO �����ԍ��v
           WHEN "39043419"   MOVE "5614091" TO �����ԍ��v
           WHEN "39043617"   MOVE "5614091" TO �����ԍ��v
           WHEN "39043625"   MOVE "5614091" TO �����ԍ��v
           WHEN "39044011"   MOVE "5614091" TO �����ԍ��v
           WHEN "39044045"   MOVE "5614091" TO �����ԍ��v
           WHEN "39044060"   MOVE "5614091" TO �����ԍ��v
           WHEN "39044219"   MOVE "5614091" TO �����ԍ��v
           WHEN "39044227"   MOVE "5614091" TO �����ԍ��v
           WHEN "39044235"   MOVE "5614091" TO �����ԍ��v
           WHEN "39044243"   MOVE "5614091" TO �����ԍ��v
           WHEN "39044441"   MOVE "5614091" TO �����ԍ��v
           WHEN "39044458"   MOVE "5614091" TO �����ԍ��v
           WHEN "39045018"   MOVE "5614091" TO �����ԍ��v
           WHEN "39045059"   MOVE "5614091" TO �����ԍ��v
           WHEN "39045810"   MOVE "5614091" TO �����ԍ��v
           WHEN "39046032"   MOVE "5614091" TO �����ԍ��v
           WHEN "39046065"   MOVE "5614091" TO �����ԍ��v
           WHEN "39062013"   MOVE "5614105" TO �����ԍ��v
           WHEN "39062021"   MOVE "5614105" TO �����ԍ��v
           WHEN "39062039"   MOVE "5614105" TO �����ԍ��v
           WHEN "39062047"   MOVE "5614105" TO �����ԍ��v
           WHEN "39062054"   MOVE "5614105" TO �����ԍ��v
           WHEN "39062062"   MOVE "5614105" TO �����ԍ��v
           WHEN "39062070"   MOVE "5614105" TO �����ԍ��v
           WHEN "39062088"   MOVE "5614105" TO �����ԍ��v
           WHEN "39062096"   MOVE "5614105" TO �����ԍ��v
           WHEN "39062104"   MOVE "5614105" TO �����ԍ��v
           WHEN "39062112"   MOVE "5614105" TO �����ԍ��v
           WHEN "39062120"   MOVE "5614105" TO �����ԍ��v
           WHEN "39062138"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063011"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063029"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063219"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063227"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063235"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063243"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063417"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063615"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063623"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063631"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063649"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063656"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063664"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063672"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063813"   MOVE "5614105" TO �����ԍ��v
           WHEN "39063821"   MOVE "5614105" TO �����ԍ��v
           WHEN "39064019"   MOVE "5614105" TO �����ԍ��v
           WHEN "39064027"   MOVE "5614105" TO �����ԍ��v
           WHEN "39064035"   MOVE "5614105" TO �����ԍ��v
           WHEN "39064266"   MOVE "5614105" TO �����ԍ��v
           WHEN "39064282"   MOVE "5614105" TO �����ԍ��v
           WHEN "39064613"   MOVE "5614105" TO �����ԍ��v
           WHEN "39072012"   MOVE "5614113" TO �����ԍ��v
           WHEN "39072020"   MOVE "5614113" TO �����ԍ��v
           WHEN "39072038"   MOVE "5614113" TO �����ԍ��v
           WHEN "39072046"   MOVE "5614113" TO �����ԍ��v
           WHEN "39072053"   MOVE "5614113" TO �����ԍ��v
           WHEN "39072079"   MOVE "5614113" TO �����ԍ��v
           WHEN "39072087"   MOVE "5614113" TO �����ԍ��v
           WHEN "39072095"   MOVE "5614113" TO �����ԍ��v
           WHEN "39072103"   MOVE "5614113" TO �����ԍ��v
           WHEN "39072111"   MOVE "5614113" TO �����ԍ��v
           WHEN "39072129"   MOVE "5614113" TO �����ԍ��v
           WHEN "39072137"   MOVE "5614113" TO �����ԍ��v
           WHEN "39072145"   MOVE "5614113" TO �����ԍ��v
           WHEN "39073010"   MOVE "5614113" TO �����ԍ��v
           WHEN "39073036"   MOVE "5614113" TO �����ԍ��v
           WHEN "39073085"   MOVE "5614113" TO �����ԍ��v
           WHEN "39073093"   MOVE "5614113" TO �����ԍ��v
           WHEN "39073226"   MOVE "5614113" TO �����ԍ��v
           WHEN "39073424"   MOVE "5614113" TO �����ԍ��v
           WHEN "39073440"   MOVE "5614113" TO �����ԍ��v
           WHEN "39073622"   MOVE "5614113" TO �����ԍ��v
           WHEN "39073648"   MOVE "5614113" TO �����ԍ��v
           WHEN "39073671"   MOVE "5614113" TO �����ԍ��v
           WHEN "39073689"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074026"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074059"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074075"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074083"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074216"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074224"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074232"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074448"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074455"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074463"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074471"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074612"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074646"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074653"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074661"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074810"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074828"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074836"   MOVE "5614113" TO �����ԍ��v
           WHEN "39074844"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075015"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075023"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075031"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075049"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075056"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075213"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075221"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075411"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075429"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075437"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075445"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075452"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075460"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075478"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075486"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075619"   MOVE "5614113" TO �����ԍ��v
           WHEN "39075643"   MOVE "5614113" TO �����ԍ��v
           WHEN "39082011"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082029"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082037"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082045"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082052"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082078"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082086"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082102"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082110"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082128"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082144"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082151"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082169"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082177"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082193"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082201"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082219"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082227"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082235"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082243"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082250"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082268"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082276"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082284"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082292"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082300"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082318"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082326"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082334"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082342"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082359"   MOVE "5614121" TO �����ԍ��v
           WHEN "39082367"   MOVE "5614121" TO �����ԍ��v
           WHEN "39083027"   MOVE "5614121" TO �����ԍ��v
           WHEN "39083092"   MOVE "5614121" TO �����ԍ��v
           WHEN "39083100"   MOVE "5614121" TO �����ԍ��v
           WHEN "39083415"   MOVE "5614121" TO �����ԍ��v
           WHEN "39083647"   MOVE "5614121" TO �����ԍ��v
           WHEN "39084421"   MOVE "5614121" TO �����ԍ��v
           WHEN "39084439"   MOVE "5614121" TO �����ԍ��v
           WHEN "39084470"   MOVE "5614121" TO �����ԍ��v
           WHEN "39085212"   MOVE "5614121" TO �����ԍ��v
           WHEN "39085428"   MOVE "5614121" TO �����ԍ��v
           WHEN "39085469"   MOVE "5614121" TO �����ԍ��v
           WHEN "39085642"   MOVE "5614121" TO �����ԍ��v
           WHEN "39092010"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092028"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092036"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092044"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092051"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092069"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092085"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092093"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092101"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092119"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092135"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092143"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092150"   MOVE "5614148" TO �����ԍ��v
           WHEN "39092168"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093018"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093216"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093414"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093422"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093430"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093448"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093455"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093612"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093646"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093653"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093661"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093679"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093687"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093844"   MOVE "5614148" TO �����ԍ��v
           WHEN "39093869"   MOVE "5614148" TO �����ԍ��v
           WHEN "39094073"   MOVE "5614148" TO �����ԍ��v
           WHEN "39094115"   MOVE "5614148" TO �����ԍ��v
           WHEN "39102017"   MOVE "5614156" TO �����ԍ��v
           WHEN "39102025"   MOVE "5614156" TO �����ԍ��v
           WHEN "39102033"   MOVE "5614156" TO �����ԍ��v
           WHEN "39102041"   MOVE "5614156" TO �����ԍ��v
           WHEN "39102058"   MOVE "5614156" TO �����ԍ��v
           WHEN "39102066"   MOVE "5614156" TO �����ԍ��v
           WHEN "39102074"   MOVE "5614156" TO �����ԍ��v
           WHEN "39102082"   MOVE "5614156" TO �����ԍ��v
           WHEN "39102090"   MOVE "5614156" TO �����ԍ��v
           WHEN "39102108"   MOVE "5614156" TO �����ԍ��v
           WHEN "39102116"   MOVE "5614156" TO �����ԍ��v
           WHEN "39102124"   MOVE "5614156" TO �����ԍ��v
           WHEN "39103031"   MOVE "5614156" TO �����ԍ��v
           WHEN "39103445"   MOVE "5614156" TO �����ԍ��v
           WHEN "39103452"   MOVE "5614156" TO �����ԍ��v
           WHEN "39103635"   MOVE "5614156" TO �����ԍ��v
           WHEN "39103668"   MOVE "5614156" TO �����ԍ��v
           WHEN "39103676"   MOVE "5614156" TO �����ԍ��v
           WHEN "39103825"   MOVE "5614156" TO �����ԍ��v
           WHEN "39103833"   MOVE "5614156" TO �����ԍ��v
           WHEN "39103841"   MOVE "5614156" TO �����ԍ��v
           WHEN "39104211"   MOVE "5614156" TO �����ԍ��v
           WHEN "39104245"   MOVE "5614156" TO �����ԍ��v
           WHEN "39104252"   MOVE "5614156" TO �����ԍ��v
           WHEN "39104260"   MOVE "5614156" TO �����ԍ��v
           WHEN "39104278"   MOVE "5614156" TO �����ԍ��v
           WHEN "39104286"   MOVE "5614156" TO �����ԍ��v
           WHEN "39104294"   MOVE "5614156" TO �����ԍ��v
           WHEN "39104435"   MOVE "5614156" TO �����ԍ��v
           WHEN "39104443"   MOVE "5614156" TO �����ԍ��v
           WHEN "39104484"   MOVE "5614156" TO �����ԍ��v
           WHEN "39104492"   MOVE "5614156" TO �����ԍ��v
           WHEN "39104641"   MOVE "5614156" TO �����ԍ��v
           WHEN "39105218"   MOVE "5614156" TO �����ԍ��v
           WHEN "39105226"   MOVE "5614156" TO �����ԍ��v
           WHEN "39105234"   MOVE "5614156" TO �����ԍ��v
           WHEN "39105242"   MOVE "5614156" TO �����ԍ��v
           WHEN "39105259"   MOVE "5614156" TO �����ԍ��v
           WHEN "39121017"   MOVE "5614164" TO �����ԍ��v
           WHEN "39121025"   MOVE "5614164" TO �����ԍ��v
           WHEN "39121033"   MOVE "5614164" TO �����ԍ��v
           WHEN "39121041"   MOVE "5614164" TO �����ԍ��v
           WHEN "39121058"   MOVE "5614164" TO �����ԍ��v
           WHEN "39121066"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122023"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122031"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122049"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122056"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122064"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122072"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122080"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122106"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122114"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122122"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122130"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122155"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122163"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122171"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122189"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122197"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122205"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122213"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122221"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122239"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122247"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122254"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122262"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122270"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122288"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122296"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122304"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122312"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122320"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122338"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122346"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122353"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122361"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122379"   MOVE "5614164" TO �����ԍ��v
           WHEN "39122387"   MOVE "5614164" TO �����ԍ��v
           WHEN "39123229"   MOVE "5614164" TO �����ԍ��v
           WHEN "39123252"   MOVE "5614164" TO �����ԍ��v
           WHEN "39123286"   MOVE "5614164" TO �����ԍ��v
           WHEN "39123294"   MOVE "5614164" TO �����ԍ��v
           WHEN "39123427"   MOVE "5614164" TO �����ԍ��v
           WHEN "39123476"   MOVE "5614164" TO �����ԍ��v
           WHEN "39123492"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124029"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124037"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124094"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124102"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124219"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124227"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124235"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124243"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124268"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124276"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124417"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124433"   MOVE "5614164" TO �����ԍ��v
           WHEN "39124631"   MOVE "5614164" TO �����ԍ��v
           WHEN "39141015"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141023"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141031"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141049"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141056"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141064"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141072"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141080"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141098"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141106"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141114"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141122"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141130"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141148"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141155"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141163"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141171"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141189"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141312"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141320"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141338"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141346"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141353"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141361"   MOVE "5614172" TO �����ԍ��v
           WHEN "39141379"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142013"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142039"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142047"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142054"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142062"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142070"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142088"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142096"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142104"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142112"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142120"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142138"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142146"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142153"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142161"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142179"   MOVE "5614172" TO �����ԍ��v
           WHEN "39142187"   MOVE "5614172" TO �����ԍ��v
           WHEN "39143011"   MOVE "5614172" TO �����ԍ��v
           WHEN "39143219"   MOVE "5614172" TO �����ԍ��v
           WHEN "39143417"   MOVE "5614172" TO �����ԍ��v
           WHEN "39143425"   MOVE "5614172" TO �����ԍ��v
           WHEN "39143615"   MOVE "5614172" TO �����ԍ��v
           WHEN "39143623"   MOVE "5614172" TO �����ԍ��v
           WHEN "39143631"   MOVE "5614172" TO �����ԍ��v
           WHEN "39143649"   MOVE "5614172" TO �����ԍ��v
           WHEN "39143664"   MOVE "5614172" TO �����ԍ��v
           WHEN "39143821"   MOVE "5614172" TO �����ԍ��v
           WHEN "39143839"   MOVE "5614172" TO �����ԍ��v
           WHEN "39143847"   MOVE "5614172" TO �����ԍ��v
           WHEN "39144019"   MOVE "5614172" TO �����ԍ��v
           WHEN "39144027"   MOVE "5614172" TO �����ԍ��v
           WHEN "39192018"   MOVE "5614180" TO �����ԍ��v
           WHEN "39192026"   MOVE "5614180" TO �����ԍ��v
           WHEN "39192042"   MOVE "5614180" TO �����ԍ��v
           WHEN "39192059"   MOVE "5614180" TO �����ԍ��v
           WHEN "39192067"   MOVE "5614180" TO �����ԍ��v
           WHEN "39192075"   MOVE "5614180" TO �����ԍ��v
           WHEN "39192083"   MOVE "5614180" TO �����ԍ��v
           WHEN "39192091"   MOVE "5614180" TO �����ԍ��v
           WHEN "39192109"   MOVE "5614180" TO �����ԍ��v
           WHEN "39192117"   MOVE "5614180" TO �����ԍ��v
           WHEN "39192125"   MOVE "5614180" TO �����ԍ��v
           WHEN "39192133"   MOVE "5614180" TO �����ԍ��v
           WHEN "39192141"   MOVE "5614180" TO �����ԍ��v
           WHEN "39193461"   MOVE "5614180" TO �����ԍ��v
           WHEN "39193610"   MOVE "5614180" TO �����ԍ��v
           WHEN "39193628"   MOVE "5614180" TO �����ԍ��v
           WHEN "39193644"   MOVE "5614180" TO �����ԍ��v
           WHEN "39193651"   MOVE "5614180" TO �����ԍ��v
           WHEN "39193669"   MOVE "5614180" TO �����ԍ��v
           WHEN "39193842"   MOVE "5614180" TO �����ԍ��v
           WHEN "39194220"   MOVE "5614180" TO �����ԍ��v
           WHEN "39194238"   MOVE "5614180" TO �����ԍ��v
           WHEN "39194246"   MOVE "5614180" TO �����ԍ��v
           WHEN "39194253"   MOVE "5614180" TO �����ԍ��v
           WHEN "39194295"   MOVE "5614180" TO �����ԍ��v
           WHEN "39194303"   MOVE "5614180" TO �����ԍ��v
           WHEN "39194428"   MOVE "5614180" TO �����ԍ��v
           WHEN "39194436"   MOVE "5614180" TO �����ԍ��v
           WHEN "39202015"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202023"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202031"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202049"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202056"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202064"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202072"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202080"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202098"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202106"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202114"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202122"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202130"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202148"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202155"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202171"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202189"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202197"   MOVE "5614199" TO �����ԍ��v
           WHEN "39202205"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203039"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203047"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203054"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203062"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203070"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203096"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203211"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203237"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203245"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203492"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203500"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203617"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203625"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203633"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203823"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203831"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203849"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203856"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203864"   MOVE "5614199" TO �����ԍ��v
           WHEN "39203880"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204029"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204037"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204045"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204060"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204078"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204094"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204102"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204110"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204128"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204136"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204144"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204151"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204169"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204177"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204227"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204235"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204250"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204292"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204300"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204326"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204466"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204482"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204490"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204508"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204516"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204524"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204813"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204821"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204854"   MOVE "5614199" TO �����ԍ��v
           WHEN "39204862"   MOVE "5614199" TO �����ԍ��v
           WHEN "39205216"   MOVE "5614199" TO �����ԍ��v
           WHEN "39205414"   MOVE "5614199" TO �����ԍ��v
           WHEN "39205430"   MOVE "5614199" TO �����ԍ��v
           WHEN "39205612"   MOVE "5614199" TO �����ԍ��v
           WHEN "39205620"   MOVE "5614199" TO �����ԍ��v
           WHEN "39205638"   MOVE "5614199" TO �����ԍ��v
           WHEN "39205810"   MOVE "5614199" TO �����ԍ��v
           WHEN "39205836"   MOVE "5614199" TO �����ԍ��v
           WHEN "39205885"   MOVE "5614199" TO �����ԍ��v
           WHEN "39205893"   MOVE "5614199" TO �����ԍ��v
           WHEN "39205901"   MOVE "5614199" TO �����ԍ��v
           WHEN "39206024"   MOVE "5614199" TO �����ԍ��v
           WHEN "39212014"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212022"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212030"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212048"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212055"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212063"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212071"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212089"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212097"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212105"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212113"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212121"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212139"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212147"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212154"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212162"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212170"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212188"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212196"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212204"   MOVE "5614202" TO �����ԍ��v
           WHEN "39212212"   MOVE "5614202" TO �����ԍ��v
           WHEN "39213020"   MOVE "5614202" TO �����ԍ��v
           WHEN "39213038"   MOVE "5614202" TO �����ԍ��v
           WHEN "39213418"   MOVE "5614202" TO �����ԍ��v
           WHEN "39213616"   MOVE "5614202" TO �����ԍ��v
           WHEN "39213624"   MOVE "5614202" TO �����ԍ��v
           WHEN "39213814"   MOVE "5614202" TO �����ԍ��v
           WHEN "39213822"   MOVE "5614202" TO �����ԍ��v
           WHEN "39213830"   MOVE "5614202" TO �����ԍ��v
           WHEN "39214010"   MOVE "5614202" TO �����ԍ��v
           WHEN "39214036"   MOVE "5614202" TO �����ԍ��v
           WHEN "39214044"   MOVE "5614202" TO �����ԍ��v
           WHEN "39214218"   MOVE "5614202" TO �����ԍ��v
           WHEN "39215017"   MOVE "5614202" TO �����ԍ��v
           WHEN "39215025"   MOVE "5614202" TO �����ԍ��v
           WHEN "39215033"   MOVE "5614202" TO �����ԍ��v
           WHEN "39215041"   MOVE "5614202" TO �����ԍ��v
           WHEN "39215058"   MOVE "5614202" TO �����ԍ��v
           WHEN "39215066"   MOVE "5614202" TO �����ԍ��v
           WHEN "39215074"   MOVE "5614202" TO �����ԍ��v
           WHEN "39215215"   MOVE "5614202" TO �����ԍ��v
           WHEN "39216049"   MOVE "5614202" TO �����ԍ��v
           WHEN "39231014"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231022"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231030"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231048"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231055"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231063"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231071"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231089"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231097"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231105"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231113"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231121"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231139"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231147"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231154"   MOVE "5614210" TO �����ԍ��v
           WHEN "39231162"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232012"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232020"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232038"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232046"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232053"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232061"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232079"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232087"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232095"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232103"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232111"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232129"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232137"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232145"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232152"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232160"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232178"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232194"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232202"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232210"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232228"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232236"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232244"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232251"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232269"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232277"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232285"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232293"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232301"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232319"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232327"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232335"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232343"   MOVE "5614210" TO �����ԍ��v
           WHEN "39232350"   MOVE "5614210" TO �����ԍ��v
           WHEN "39233028"   MOVE "5614210" TO �����ԍ��v
           WHEN "39233044"   MOVE "5614210" TO �����ԍ��v
           WHEN "39233424"   MOVE "5614210" TO �����ԍ��v
           WHEN "39233457"   MOVE "5614210" TO �����ԍ��v
           WHEN "39233614"   MOVE "5614210" TO �����ԍ��v
           WHEN "39233622"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234216"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234224"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234232"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234240"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234257"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234273"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234414"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234422"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234455"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234463"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234471"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234810"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234828"   MOVE "5614210" TO �����ԍ��v
           WHEN "39234836"   MOVE "5614210" TO �����ԍ��v
           WHEN "39235015"   MOVE "5614210" TO �����ԍ��v
           WHEN "39235213"   MOVE "5614210" TO �����ԍ��v
           WHEN "39235619"   MOVE "5614210" TO �����ԍ��v
           WHEN "39235627"   MOVE "5614210" TO �����ԍ��v
           WHEN "39235635"   MOVE "5614210" TO �����ԍ��v
           WHEN "39236039"   MOVE "5614210" TO �����ԍ��v
           WHEN "39271028"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271036"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271044"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271069"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271077"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271085"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271093"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271119"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271135"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271143"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271150"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271168"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271176"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271184"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271192"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271200"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271218"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271226"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271234"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271242"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271259"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271267"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271275"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271283"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271416"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271424"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271432"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271440"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271457"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271465"   MOVE "5614229" TO �����ԍ��v
           WHEN "39271473"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272026"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272034"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272042"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272059"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272067"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272075"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272083"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272091"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272109"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272117"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272125"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272133"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272141"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272158"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272166"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272174"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272182"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272190"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272208"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272216"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272224"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272232"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272240"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272257"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272265"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272273"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272281"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272299"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272307"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272315"   MOVE "5614229" TO �����ԍ��v
           WHEN "39272323"   MOVE "5614229" TO �����ԍ��v
           WHEN "39273016"   MOVE "5614229" TO �����ԍ��v
           WHEN "39273214"   MOVE "5614229" TO �����ԍ��v
           WHEN "39273222"   MOVE "5614229" TO �����ԍ��v
           WHEN "39273412"   MOVE "5614229" TO �����ԍ��v
           WHEN "39273610"   MOVE "5614229" TO �����ԍ��v
           WHEN "39273628"   MOVE "5614229" TO �����ԍ��v
           WHEN "39273669"   MOVE "5614229" TO �����ԍ��v
           WHEN "39273818"   MOVE "5614229" TO �����ԍ��v
           WHEN "39273826"   MOVE "5614229" TO �����ԍ��v
           WHEN "39273834"   MOVE "5614229" TO �����ԍ��v
           WHEN "39401013"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401039"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401054"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401062"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401070"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401088"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401096"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401310"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401328"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401336"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401344"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401351"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401369"   MOVE "5614237" TO �����ԍ��v
           WHEN "39401377"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402029"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402037"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402045"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402052"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402060"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402078"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402102"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402110"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402128"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402136"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402144"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402151"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402169"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402177"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402185"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402193"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402201"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402219"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402227"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402235"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402243"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402250"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402268"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402276"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402284"   MOVE "5614237" TO �����ԍ��v
           WHEN "39402292"   MOVE "5614237" TO �����ԍ��v
           WHEN "39403050"   MOVE "5614237" TO �����ԍ��v
           WHEN "39403415"   MOVE "5614237" TO �����ԍ��v
           WHEN "39403423"   MOVE "5614237" TO �����ԍ��v
           WHEN "39403431"   MOVE "5614237" TO �����ԍ��v
           WHEN "39403449"   MOVE "5614237" TO �����ԍ��v
           WHEN "39403456"   MOVE "5614237" TO �����ԍ��v
           WHEN "39403480"   MOVE "5614237" TO �����ԍ��v
           WHEN "39403498"   MOVE "5614237" TO �����ԍ��v
           WHEN "39403811"   MOVE "5614237" TO �����ԍ��v
           WHEN "39403829"   MOVE "5614237" TO �����ԍ��v
           WHEN "39403837"   MOVE "5614237" TO �����ԍ��v
           WHEN "39403845"   MOVE "5614237" TO �����ԍ��v
           WHEN "39404017"   MOVE "5614237" TO �����ԍ��v
           WHEN "39404025"   MOVE "5614237" TO �����ԍ��v
           WHEN "39404215"   MOVE "5614237" TO �����ԍ��v
           WHEN "39404470"   MOVE "5614237" TO �����ԍ��v
           WHEN "39404488"   MOVE "5614237" TO �����ԍ��v
           WHEN "39404629"   MOVE "5614237" TO �����ԍ��v
           WHEN "39404637"   MOVE "5614237" TO �����ԍ��v
           WHEN "39405030"   MOVE "5614237" TO �����ԍ��v
           WHEN "39405220"   MOVE "5614237" TO �����ԍ��v
           WHEN "39405410"   MOVE "5614237" TO �����ԍ��v
           WHEN "39405436"   MOVE "5614237" TO �����ԍ��v
           WHEN "39405444"   MOVE "5614237" TO �����ԍ��v
           WHEN "39405451"   MOVE "5614237" TO �����ԍ��v
           WHEN "39405469"   MOVE "5614237" TO �����ԍ��v
           WHEN "39406012"   MOVE "5614237" TO �����ԍ��v
           WHEN "39406020"   MOVE "5614237" TO �����ԍ��v
           WHEN "39406046"   MOVE "5614237" TO �����ԍ��v
           WHEN "39406053"   MOVE "5614237" TO �����ԍ��v
           WHEN "39406087"   MOVE "5614237" TO �����ԍ��v
           WHEN "39406095"   MOVE "5614237" TO �����ԍ��v
           WHEN "39406103"   MOVE "5614237" TO �����ԍ��v
           WHEN "39406210"   MOVE "5614237" TO �����ԍ��v
           WHEN "39406251"   MOVE "5614237" TO �����ԍ��v
           WHEN "39406426"   MOVE "5614237" TO �����ԍ��v
           WHEN "39406467"   MOVE "5614237" TO �����ԍ��v
           WHEN "39406475"   MOVE "5614237" TO �����ԍ��v
           WHEN "39422019"   MOVE "5614245" TO �����ԍ��v
           WHEN "39422027"   MOVE "5614245" TO �����ԍ��v
           WHEN "39422035"   MOVE "5614245" TO �����ԍ��v
           WHEN "39422043"   MOVE "5614245" TO �����ԍ��v
           WHEN "39422050"   MOVE "5614245" TO �����ԍ��v
           WHEN "39422076"   MOVE "5614245" TO �����ԍ��v
           WHEN "39422084"   MOVE "5614245" TO �����ԍ��v
           WHEN "39422092"   MOVE "5614245" TO �����ԍ��v
           WHEN "39422100"   MOVE "5614245" TO �����ԍ��v
           WHEN "39422118"   MOVE "5614245" TO �����ԍ��v
           WHEN "39422126"   MOVE "5614245" TO �����ԍ��v
           WHEN "39422134"   MOVE "5614245" TO �����ԍ��v
           WHEN "39422142"   MOVE "5614245" TO �����ԍ��v
           WHEN "39423074"   MOVE "5614245" TO �����ԍ��v
           WHEN "39423082"   MOVE "5614245" TO �����ԍ��v
           WHEN "39423215"   MOVE "5614245" TO �����ԍ��v
           WHEN "39423223"   MOVE "5614245" TO �����ԍ��v
           WHEN "39423231"   MOVE "5614245" TO �����ԍ��v
           WHEN "39423835"   MOVE "5614245" TO �����ԍ��v
           WHEN "39423884"   MOVE "5614245" TO �����ԍ��v
           WHEN "39423892"   MOVE "5614245" TO �����ԍ��v
           WHEN "39423918"   MOVE "5614245" TO �����ԍ��v
           WHEN "39424114"   MOVE "5614245" TO �����ԍ��v
           WHEN "39432018"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432026"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432034"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432042"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432059"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432067"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432083"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432109"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432117"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432125"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432133"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432141"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432158"   MOVE "5614253" TO �����ԍ��v
           WHEN "39432166"   MOVE "5614253" TO �����ԍ��v
           WHEN "39433412"   MOVE "5614253" TO �����ԍ��v
           WHEN "39433420"   MOVE "5614253" TO �����ԍ��v
           WHEN "39433487"   MOVE "5614253" TO �����ԍ��v
           WHEN "39433644"   MOVE "5614253" TO �����ԍ��v
           WHEN "39433677"   MOVE "5614253" TO �����ԍ��v
           WHEN "39433685"   MOVE "5614253" TO �����ԍ��v
           WHEN "39433693"   MOVE "5614253" TO �����ԍ��v
           WHEN "39433859"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434030"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434048"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434238"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434246"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434253"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434287"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434329"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434337"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434410"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434428"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434436"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434444"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434477"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434683"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434824"   MOVE "5614253" TO �����ԍ��v
           WHEN "39434840"   MOVE "5614253" TO �����ԍ��v
           WHEN "39435011"   MOVE "5614253" TO �����ԍ��v
           WHEN "39435052"   MOVE "5614253" TO �����ԍ��v
           WHEN "39435060"   MOVE "5614253" TO �����ԍ��v
           WHEN "39435078"   MOVE "5614253" TO �����ԍ��v
           WHEN "39435102"   MOVE "5614253" TO �����ԍ��v
           WHEN "39435110"   MOVE "5614253" TO �����ԍ��v
           WHEN "39435128"   MOVE "5614253" TO �����ԍ��v
           WHEN "39435136"   MOVE "5614253" TO �����ԍ��v
           WHEN "39435144"   MOVE "5614253" TO �����ԍ��v
           WHEN "39435318"   MOVE "5614253" TO �����ԍ��v
           WHEN "39442017"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442025"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442033"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442041"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442058"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442066"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442074"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442082"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442090"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442108"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442116"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442124"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442132"   MOVE "5614261" TO �����ԍ��v
           WHEN "39442140"   MOVE "5614261" TO �����ԍ��v
           WHEN "39443221"   MOVE "5614261" TO �����ԍ��v
           WHEN "39443411"   MOVE "5614261" TO �����ԍ��v
           WHEN "39444617"   MOVE "5614261" TO �����ԍ��v
           WHEN "39444625"   MOVE "5614261" TO �����ԍ��v
           WHEN "39452016"   MOVE "5614288" TO �����ԍ��v
           WHEN "39452024"   MOVE "5614288" TO �����ԍ��v
           WHEN "39452032"   MOVE "5614288" TO �����ԍ��v
           WHEN "39452040"   MOVE "5614288" TO �����ԍ��v
           WHEN "39452057"   MOVE "5614288" TO �����ԍ��v
           WHEN "39452065"   MOVE "5614288" TO �����ԍ��v
           WHEN "39452073"   MOVE "5614288" TO �����ԍ��v
           WHEN "39452081"   MOVE "5614288" TO �����ԍ��v
           WHEN "39452099"   MOVE "5614288" TO �����ԍ��v
           WHEN "39453014"   MOVE "5614288" TO �����ԍ��v
           WHEN "39453212"   MOVE "5614288" TO �����ԍ��v
           WHEN "39453220"   MOVE "5614288" TO �����ԍ��v
           WHEN "39453410"   MOVE "5614288" TO �����ԍ��v
           WHEN "39453618"   MOVE "5614288" TO �����ԍ��v
           WHEN "39453626"   MOVE "5614288" TO �����ԍ��v
           WHEN "39453824"   MOVE "5614288" TO �����ԍ��v
           WHEN "39453832"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454012"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454020"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454038"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454046"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454053"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454061"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454210"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454293"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454301"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454319"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454418"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454426"   MOVE "5614288" TO �����ԍ��v
           WHEN "39454434"   MOVE "5614288" TO �����ԍ��v
           WHEN "39462015"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462031"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462049"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462064"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462080"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462098"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462106"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462130"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462148"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462155"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462163"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462171"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462189"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462197"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462205"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462213"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462221"   MOVE "5614296" TO �����ԍ��v
           WHEN "39462239"   MOVE "5614296" TO �����ԍ��v
           WHEN "39463039"   MOVE "5614296" TO �����ԍ��v
           WHEN "39463047"   MOVE "5614296" TO �����ԍ��v
           WHEN "39463922"   MOVE "5614296" TO �����ԍ��v
           WHEN "39464045"   MOVE "5614296" TO �����ԍ��v
           WHEN "39464219"   MOVE "5614296" TO �����ԍ��v
           WHEN "39464417"   MOVE "5614296" TO �����ԍ��v
           WHEN "39464425"   MOVE "5614296" TO �����ԍ��v
           WHEN "39464433"   MOVE "5614296" TO �����ԍ��v
           WHEN "39464524"   MOVE "5614296" TO �����ԍ��v
           WHEN "39464680"   MOVE "5614296" TO �����ԍ��v
           WHEN "39464821"   MOVE "5614296" TO �����ԍ��v
           WHEN "39464904"   MOVE "5614296" TO �����ԍ��v
           WHEN "39464912"   MOVE "5614296" TO �����ԍ��v
           WHEN "39464920"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465018"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465026"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465059"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465232"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465240"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465257"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465273"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465299"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465307"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465315"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465323"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465331"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465349"   MOVE "5614296" TO �����ԍ��v
           WHEN "39465356"   MOVE "5614296" TO �����ԍ��v
           WHEN "39111018"   MOVE "5614318" TO �����ԍ��v
           WHEN "39111026"   MOVE "5614318" TO �����ԍ��v
           WHEN "39111034"   MOVE "5614318" TO �����ԍ��v
           WHEN "39111042"   MOVE "5614318" TO �����ԍ��v
           WHEN "39111059"   MOVE "5614318" TO �����ԍ��v
           WHEN "39111067"   MOVE "5614318" TO �����ԍ��v
           WHEN "39111075"   MOVE "5614318" TO �����ԍ��v
           WHEN "39111083"   MOVE "5614318" TO �����ԍ��v
           WHEN "39111091"   MOVE "5614318" TO �����ԍ��v
           WHEN "39111109"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112016"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112024"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112032"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112065"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112073"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112081"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112099"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112107"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112115"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112123"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112149"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112156"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112164"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112172"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112180"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112198"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112214"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112222"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112230"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112248"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112255"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112263"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112271"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112289"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112297"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112305"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112313"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112321"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112339"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112347"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112354"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112370"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112388"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112396"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112404"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112412"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112420"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112438"   MOVE "5614318" TO �����ԍ��v
           WHEN "39112453"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113014"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113246"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113261"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113279"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113410"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113428"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113436"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113469"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113477"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113485"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113493"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113618"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113626"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113634"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113659"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113691"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113816"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113832"   MOVE "5614318" TO �����ԍ��v
           WHEN "39113857"   MOVE "5614318" TO �����ԍ��v
           WHEN "39114087"   MOVE "5614318" TO �����ԍ��v
           WHEN "39114210"   MOVE "5614318" TO �����ԍ��v
           WHEN "39114244"   MOVE "5614318" TO �����ԍ��v
           WHEN "39114251"   MOVE "5614318" TO �����ԍ��v
           WHEN "39114426"   MOVE "5614318" TO �����ԍ��v
           WHEN "39114459"   MOVE "5614318" TO �����ԍ��v
           WHEN "39114467"   MOVE "5614318" TO �����ԍ��v
           WHEN "39114616"   MOVE "5614318" TO �����ԍ��v
           WHEN "39114624"   MOVE "5614318" TO �����ԍ��v
           WHEN "39114640"   MOVE "5614318" TO �����ԍ��v
           WHEN "39114657"   MOVE "5614318" TO �����ԍ��v
           WHEN "01010016"   MOVE "5614326" TO �����ԍ��v
           WHEN "03010014"   MOVE "5614326" TO �����ԍ��v
           WHEN "04010013"   MOVE "5614326" TO �����ԍ��v
           WHEN "01020015"   MOVE "5614334" TO �����ԍ��v
           WHEN "03020013"   MOVE "5614334" TO �����ԍ��v
           WHEN "04020012"   MOVE "5614334" TO �����ԍ��v
           WHEN "01030014"   MOVE "5614342" TO �����ԍ��v
           WHEN "03030012"   MOVE "5614342" TO �����ԍ��v
           WHEN "04030011"   MOVE "5614342" TO �����ԍ��v
           WHEN "01040013"   MOVE "5614350" TO �����ԍ��v
           WHEN "03040011"   MOVE "5614350" TO �����ԍ��v
           WHEN "04040010"   MOVE "5614350" TO �����ԍ��v
           WHEN "01050012"   MOVE "5614369" TO �����ԍ��v
           WHEN "03050010"   MOVE "5614369" TO �����ԍ��v
           WHEN "04050019"   MOVE "5614369" TO �����ԍ��v
           WHEN "01060011"   MOVE "5614377" TO �����ԍ��v
           WHEN "03060019"   MOVE "5614377" TO �����ԍ��v
           WHEN "04060018"   MOVE "5614377" TO �����ԍ��v
           WHEN "01070010"   MOVE "5614385" TO �����ԍ��v
           WHEN "03070018"   MOVE "5614385" TO �����ԍ��v
           WHEN "04070017"   MOVE "5614385" TO �����ԍ��v
           WHEN "01080019"   MOVE "5614393" TO �����ԍ��v
           WHEN "03080017"   MOVE "5614393" TO �����ԍ��v
           WHEN "04080016"   MOVE "5614393" TO �����ԍ��v
           WHEN "06272520"   MOVE "5614407" TO �����ԍ��v
           WHEN "06272595"   MOVE "5614415" TO �����ԍ��v
           WHEN "06273015"   MOVE "5614423" TO �����ԍ��v
           WHEN "06273478"   MOVE "5614431" TO �����ԍ��v
           WHEN "06273742"   MOVE "5614458" TO �����ԍ��v
           WHEN "06281380"   MOVE "5614466" TO �����ԍ��v
           WHEN "06133706"   MOVE "5614474" TO �����ԍ��v
           WHEN "06134290"   MOVE "5614482" TO �����ԍ��v
           WHEN "06135362"   MOVE "5614490" TO �����ԍ��v
           WHEN "06135412"   MOVE "5614504" TO �����ԍ��v
           WHEN "06137095"   MOVE "5614512" TO �����ԍ��v
           WHEN "06135461"   MOVE "5614520" TO �����ԍ��v
           WHEN "06138747"   MOVE "5614539" TO �����ԍ��v
           WHEN "27114107"   MOVE "5614547" TO �����ԍ��v
           WHEN "27114016"   MOVE "5614555" TO �����ԍ��v
           WHEN "01090018"   MOVE "5614563" TO �����ԍ��v
           WHEN "03090016"   MOVE "5614563" TO �����ԍ��v
           WHEN "04090015"   MOVE "5614563" TO �����ԍ��v
           WHEN "01100015"   MOVE "5614571" TO �����ԍ��v
           WHEN "03100013"   MOVE "5614571" TO �����ԍ��v
           WHEN "04100012"   MOVE "5614571" TO �����ԍ��v
           WHEN "01110014"   MOVE "5614598" TO �����ԍ��v
           WHEN "03110012"   MOVE "5614598" TO �����ԍ��v
           WHEN "04110011"   MOVE "5614598" TO �����ԍ��v
           WHEN "01120013"   MOVE "5614601" TO �����ԍ��v
           WHEN "03120011"   MOVE "5614601" TO �����ԍ��v
           WHEN "04120010"   MOVE "5614601" TO �����ԍ��v
           WHEN "01130012"   MOVE "5614628" TO �����ԍ��v
           WHEN "03130010"   MOVE "5614628" TO �����ԍ��v
           WHEN "04130019"   MOVE "5614628" TO �����ԍ��v
           WHEN "41140138"   MOVE "5614636" TO �����ԍ��v
           WHEN "06130058"   MOVE "5614644" TO �����ԍ��v
           WHEN "06130066"   MOVE "5614652" TO �����ԍ��v
           WHEN "06130074"   MOVE "5614660" TO �����ԍ��v
           WHEN "06130082"   MOVE "5614679" TO �����ԍ��v
           WHEN "06130090"   MOVE "5614687" TO �����ԍ��v
           WHEN "06130108"   MOVE "5614695" TO �����ԍ��v
           WHEN "63130108"   MOVE "5614695" TO �����ԍ��v
           WHEN "06130116"   MOVE "5614709" TO �����ԍ��v
           WHEN "06130124"   MOVE "5614717" TO �����ԍ��v
           WHEN "63130124"   MOVE "5614717" TO �����ԍ��v
           WHEN "06130132"   MOVE "5614725" TO �����ԍ��v
           WHEN "06130181"   MOVE "5614733" TO �����ԍ��v
           WHEN "06130199"   MOVE "5614741" TO �����ԍ��v
           WHEN "06130231"   MOVE "5614768" TO �����ԍ��v
           WHEN "06130298"   MOVE "5614776" TO �����ԍ��v
           WHEN "63130298"   MOVE "5614776" TO �����ԍ��v
           WHEN "06130306"   MOVE "5614784" TO �����ԍ��v
           WHEN "06130389"   MOVE "5614792" TO �����ԍ��v
           WHEN "63130389"   MOVE "5614792" TO �����ԍ��v
           WHEN "06130405"   MOVE "5614806" TO �����ԍ��v
           WHEN "06130439"   MOVE "5614814" TO �����ԍ��v
           WHEN "06130447"   MOVE "5614822" TO �����ԍ��v
           WHEN "06130454"   MOVE "5614830" TO �����ԍ��v
           WHEN "06130488"   MOVE "5614849" TO �����ԍ��v
           WHEN "06130553"   MOVE "5614857" TO �����ԍ��v
           WHEN "06130587"   MOVE "5614865" TO �����ԍ��v
           WHEN "06130637"   MOVE "5614873" TO �����ԍ��v
           WHEN "06130645"   MOVE "5614881" TO �����ԍ��v
           WHEN "06130660"   MOVE "5614903" TO �����ԍ��v
           WHEN "63130660"   MOVE "5614903" TO �����ԍ��v
           WHEN "06130686"   MOVE "5614911" TO �����ԍ��v
           WHEN "06130702"   MOVE "5614938" TO �����ԍ��v
           WHEN "63130702"   MOVE "5614938" TO �����ԍ��v
           WHEN "06130710"   MOVE "5614946" TO �����ԍ��v
           WHEN "06130728"   MOVE "5614954" TO �����ԍ��v
           WHEN "06130736"   MOVE "5614962" TO �����ԍ��v
           WHEN "06130769"   MOVE "5614970" TO �����ԍ��v
           WHEN "63130769"   MOVE "5614970" TO �����ԍ��v
           WHEN "06130777"   MOVE "5614989" TO �����ԍ��v
           WHEN "63130777"   MOVE "5614989" TO �����ԍ��v
           WHEN "06130785"   MOVE "5614997" TO �����ԍ��v
           WHEN "06130843"   MOVE "5615012" TO �����ԍ��v
           WHEN "06130868"   MOVE "5615020" TO �����ԍ��v
           WHEN "06130892"   MOVE "5615039" TO �����ԍ��v
           WHEN "63130892"   MOVE "5615039" TO �����ԍ��v
           WHEN "06130900"   MOVE "5615047" TO �����ԍ��v
           WHEN "06130926"   MOVE "5615055" TO �����ԍ��v
           WHEN "06130934"   MOVE "5615063" TO �����ԍ��v
           WHEN "06130975"   MOVE "5615071" TO �����ԍ��v
           WHEN "06131064"   MOVE "5615098" TO �����ԍ��v
           WHEN "06131114"   MOVE "5615101" TO �����ԍ��v
           WHEN "06131163"   MOVE "5615128" TO �����ԍ��v
           WHEN "06131189"   MOVE "5615136" TO �����ԍ��v
           WHEN "06131213"   MOVE "5615144" TO �����ԍ��v
           WHEN "06131288"   MOVE "5615152" TO �����ԍ��v
           WHEN "06131296"   MOVE "5615160" TO �����ԍ��v
           WHEN "06131338"   MOVE "5615179" TO �����ԍ��v
           WHEN "06131346"   MOVE "5615187" TO �����ԍ��v
           WHEN "06131379"   MOVE "5615195" TO �����ԍ��v
           WHEN "06131429"   MOVE "5615209" TO �����ԍ��v
           WHEN "06131452"   MOVE "5615217" TO �����ԍ��v
           WHEN "06131460"   MOVE "5615225" TO �����ԍ��v
           WHEN "06131551"   MOVE "5615233" TO �����ԍ��v
           WHEN "63131551"   MOVE "5615233" TO �����ԍ��v
           WHEN "06131569"   MOVE "5615241" TO �����ԍ��v
           WHEN "06131577"   MOVE "5615268" TO �����ԍ��v
           WHEN "06131585"   MOVE "5615276" TO �����ԍ��v
           WHEN "06131635"   MOVE "5615284" TO �����ԍ��v
           WHEN "06131668"   MOVE "5615292" TO �����ԍ��v
           WHEN "63131668"   MOVE "5615292" TO �����ԍ��v
           WHEN "06131676"   MOVE "5615306" TO �����ԍ��v
           WHEN "06131742"   MOVE "5615314" TO �����ԍ��v
           WHEN "06131783"   MOVE "5615322" TO �����ԍ��v
           WHEN "06131791"   MOVE "5615330" TO �����ԍ��v
           WHEN "06131817"   MOVE "5615349" TO �����ԍ��v
           WHEN "06131841"   MOVE "5615357" TO �����ԍ��v
           WHEN "06131882"   MOVE "5615365" TO �����ԍ��v
           WHEN "06131924"   MOVE "5615373" TO �����ԍ��v
           WHEN "06131932"   MOVE "5615381" TO �����ԍ��v
           WHEN "06131999"   MOVE "5615403" TO �����ԍ��v
           WHEN "63131999"   MOVE "5615403" TO �����ԍ��v
           WHEN "06132013"   MOVE "5615411" TO �����ԍ��v
           WHEN "06132039"   MOVE "5615438" TO �����ԍ��v
           WHEN "06132054"   MOVE "5615446" TO �����ԍ��v
           WHEN "06132088"   MOVE "5615454" TO �����ԍ��v
           WHEN "63132088"   MOVE "5615454" TO �����ԍ��v
           WHEN "06132112"   MOVE "5615462" TO �����ԍ��v
           WHEN "63132112"   MOVE "5615462" TO �����ԍ��v
           WHEN "06132120"   MOVE "5615470" TO �����ԍ��v
           WHEN "06132146"   MOVE "5615489" TO �����ԍ��v
           WHEN "63132146"   MOVE "5615489" TO �����ԍ��v
           WHEN "06132161"   MOVE "5615497" TO �����ԍ��v
           WHEN "06132179"   MOVE "5615500" TO �����ԍ��v
           WHEN "06132211"   MOVE "5615519" TO �����ԍ��v
           WHEN "06132229"   MOVE "5615527" TO �����ԍ��v
           WHEN "06132260"   MOVE "5615535" TO �����ԍ��v
           WHEN "63132260"   MOVE "5615535" TO �����ԍ��v
           WHEN "06132294"   MOVE "5615543" TO �����ԍ��v
           WHEN "06132302"   MOVE "5615551" TO �����ԍ��v
           WHEN "06132310"   MOVE "5615578" TO �����ԍ��v
           WHEN "06132328"   MOVE "5615586" TO �����ԍ��v
           WHEN "06132336"   MOVE "5615594" TO �����ԍ��v
           WHEN "06132344"   MOVE "5615608" TO �����ԍ��v
           WHEN "06132369"   MOVE "5615616" TO �����ԍ��v
           WHEN "06132377"   MOVE "5615624" TO �����ԍ��v
           WHEN "06132393"   MOVE "5615632" TO �����ԍ��v
           WHEN "06132419"   MOVE "5615640" TO �����ԍ��v
           WHEN "06132427"   MOVE "5615659" TO �����ԍ��v
           WHEN "06132443"   MOVE "5615667" TO �����ԍ��v
           WHEN "06132468"   MOVE "5615675" TO �����ԍ��v
           WHEN "06137673"   MOVE "5615675" TO �����ԍ��v
           WHEN "06137806"   MOVE "5615675" TO �����ԍ��v
           WHEN "06138671"   MOVE "5615675" TO �����ԍ��v
           WHEN "06132476"   MOVE "5615683" TO �����ԍ��v
           WHEN "06132484"   MOVE "5615691" TO �����ԍ��v
           WHEN "06132500"   MOVE "5615705" TO �����ԍ��v
           WHEN "06132518"   MOVE "5615713" TO �����ԍ��v
           WHEN "06132559"   MOVE "5615721" TO �����ԍ��v
           WHEN "06132567"   MOVE "5615748" TO �����ԍ��v
           WHEN "06132583"   MOVE "5615756" TO �����ԍ��v
           WHEN "63132583"   MOVE "5615756" TO �����ԍ��v
           WHEN "06132658"   MOVE "5615764" TO �����ԍ��v
           WHEN "06132682"   MOVE "5615772" TO �����ԍ��v
           WHEN "06132690"   MOVE "5615780" TO �����ԍ��v
           WHEN "06132765"   MOVE "5615799" TO �����ԍ��v
           WHEN "63132765"   MOVE "5615799" TO �����ԍ��v
           WHEN "06132773"   MOVE "5615802" TO �����ԍ��v
           WHEN "63132773"   MOVE "5615802" TO �����ԍ��v
           WHEN "06132781"   MOVE "5615810" TO �����ԍ��v
           WHEN "06132799"   MOVE "5615829" TO �����ԍ��v
           WHEN "06132807"   MOVE "5615837" TO �����ԍ��v
           WHEN "06132831"   MOVE "5615845" TO �����ԍ��v
           WHEN "06132849"   MOVE "5615853" TO �����ԍ��v
           WHEN "06132856"   MOVE "5615861" TO �����ԍ��v
           WHEN "06132864"   MOVE "5615888" TO �����ԍ��v
           WHEN "06132922"   MOVE "5615896" TO �����ԍ��v
           WHEN "63132922"   MOVE "5615896" TO �����ԍ��v
           WHEN "06132930"   MOVE "5615918" TO �����ԍ��v
           WHEN "06132948"   MOVE "5615926" TO �����ԍ��v
           WHEN "63132948"   MOVE "5615926" TO �����ԍ��v
           WHEN "06132963"   MOVE "5615934" TO �����ԍ��v
           WHEN "06132971"   MOVE "5615942" TO �����ԍ��v
           WHEN "63132971"   MOVE "5615942" TO �����ԍ��v
           WHEN "06133029"   MOVE "5615950" TO �����ԍ��v
           WHEN "06090419"   MOVE "5615969" TO �����ԍ��v
           WHEN "06133086"   MOVE "5615969" TO �����ԍ��v
           WHEN "63090419"   MOVE "5615969" TO �����ԍ��v
           WHEN "63133086"   MOVE "5615969" TO �����ԍ��v
           WHEN "06133094"   MOVE "5615977" TO �����ԍ��v
           WHEN "06133102"   MOVE "5615985" TO �����ԍ��v
           WHEN "06133110"   MOVE "5615993" TO �����ԍ��v
           WHEN "06133169"   MOVE "5616000" TO �����ԍ��v
           WHEN "63133169"   MOVE "5616000" TO �����ԍ��v
           WHEN "06133177"   MOVE "5616019" TO �����ԍ��v
           WHEN "06133185"   MOVE "5616027" TO �����ԍ��v
           WHEN "06133243"   MOVE "5616035" TO �����ԍ��v
           WHEN "06133250"   MOVE "5616043" TO �����ԍ��v
           WHEN "06133276"   MOVE "5616051" TO �����ԍ��v
           WHEN "06133300"   MOVE "5616078" TO �����ԍ��v
           WHEN "06133342"   MOVE "5616086" TO �����ԍ��v
           WHEN "63133342"   MOVE "5616086" TO �����ԍ��v
           WHEN "06133375"   MOVE "5616094" TO �����ԍ��v
           WHEN "06133391"   MOVE "5616108" TO �����ԍ��v
           WHEN "06133417"   MOVE "5616116" TO �����ԍ��v
           WHEN "63133417"   MOVE "5616116" TO �����ԍ��v
           WHEN "06133425"   MOVE "5616124" TO �����ԍ��v
           WHEN "06133433"   MOVE "5616132" TO �����ԍ��v
           WHEN "06133458"   MOVE "5616140" TO �����ԍ��v
           WHEN "63133458"   MOVE "5616140" TO �����ԍ��v
           WHEN "06133474"   MOVE "5616159" TO �����ԍ��v
           WHEN "06133516"   MOVE "5616167" TO �����ԍ��v
           WHEN "06133540"   MOVE "5616175" TO �����ԍ��v
           WHEN "06133565"   MOVE "5616183" TO �����ԍ��v
           WHEN "06133573"   MOVE "5616191" TO �����ԍ��v
           WHEN "06133607"   MOVE "5616205" TO �����ԍ��v
           WHEN "06133615"   MOVE "5616213" TO �����ԍ��v
           WHEN "06133623"   MOVE "5616221" TO �����ԍ��v
           WHEN "06133631"   MOVE "5616248" TO �����ԍ��v
           WHEN "06133649"   MOVE "5616256" TO �����ԍ��v
           WHEN "06133672"   MOVE "5616264" TO �����ԍ��v
           WHEN "06133714"   MOVE "5616272" TO �����ԍ��v
           WHEN "06133730"   MOVE "5616280" TO �����ԍ��v
           WHEN "06141766"   MOVE "5616280" TO �����ԍ��v
           WHEN "06231104"   MOVE "5616280" TO �����ԍ��v
           WHEN "63133730"   MOVE "5616280" TO �����ԍ��v
           WHEN "06133771"   MOVE "5616299" TO �����ԍ��v
           WHEN "06133821"   MOVE "5616302" TO �����ԍ��v
           WHEN "06133862"   MOVE "5616310" TO �����ԍ��v
           WHEN "06133870"   MOVE "5616329" TO �����ԍ��v
           WHEN "06133888"   MOVE "5616337" TO �����ԍ��v
           WHEN "06133920"   MOVE "5616345" TO �����ԍ��v
           WHEN "06133938"   MOVE "5616353" TO �����ԍ��v
           WHEN "63133938"   MOVE "5616353" TO �����ԍ��v
           WHEN "06133946"   MOVE "5616361" TO �����ԍ��v
           WHEN "63133946"   MOVE "5616361" TO �����ԍ��v
           WHEN "06133961"   MOVE "5616388" TO �����ԍ��v
           WHEN "06134001"   MOVE "5616396" TO �����ԍ��v
           WHEN "06134019"   MOVE "5616418" TO �����ԍ��v
           WHEN "06134035"   MOVE "5616426" TO �����ԍ��v
           WHEN "06134050"   MOVE "5616434" TO �����ԍ��v
           WHEN "06134076"   MOVE "5616442" TO �����ԍ��v
           WHEN "06134084"   MOVE "5616450" TO �����ԍ��v
           WHEN "06134134"   MOVE "5616469" TO �����ԍ��v
           WHEN "63134134"   MOVE "5616469" TO �����ԍ��v
           WHEN "06134159"   MOVE "5616477" TO �����ԍ��v
           WHEN "06134175"   MOVE "5616485" TO �����ԍ��v
           WHEN "06134183"   MOVE "5616493" TO �����ԍ��v
           WHEN "63134183"   MOVE "5616493" TO �����ԍ��v
           WHEN "06134217"   MOVE "5616507" TO �����ԍ��v
           WHEN "06134340"   MOVE "5616515" TO �����ԍ��v
           WHEN "06134357"   MOVE "5616523" TO �����ԍ��v
           WHEN "06134365"   MOVE "5616531" TO �����ԍ��v
           WHEN "06134373"   MOVE "5616558" TO �����ԍ��v
           WHEN "06134381"   MOVE "5616566" TO �����ԍ��v
           WHEN "06134431"   MOVE "5616574" TO �����ԍ��v
           WHEN "63134431"   MOVE "5616574" TO �����ԍ��v
           WHEN "06134464"   MOVE "5616582" TO �����ԍ��v
           WHEN "06134498"   MOVE "5616590" TO �����ԍ��v
           WHEN "06134522"   MOVE "5616604" TO �����ԍ��v
           WHEN "06134530"   MOVE "5616612" TO �����ԍ��v
           WHEN "06134548"   MOVE "5616620" TO �����ԍ��v
           WHEN "06134555"   MOVE "5616639" TO �����ԍ��v
           WHEN "63134555"   MOVE "5616639" TO �����ԍ��v
           WHEN "06134571"   MOVE "5616647" TO �����ԍ��v
           WHEN "06134613"   MOVE "5616655" TO �����ԍ��v
           WHEN "06134621"   MOVE "5616663" TO �����ԍ��v
           WHEN "06134688"   MOVE "5616671" TO �����ԍ��v
           WHEN "06134795"   MOVE "5616698" TO �����ԍ��v
           WHEN "06134803"   MOVE "5616701" TO �����ԍ��v
           WHEN "06134845"   MOVE "5616728" TO �����ԍ��v
           WHEN "06134886"   MOVE "5616736" TO �����ԍ��v
           WHEN "06134902"   MOVE "5616744" TO �����ԍ��v
           WHEN "06134910"   MOVE "5616752" TO �����ԍ��v
           WHEN "06134928"   MOVE "5616760" TO �����ԍ��v
           WHEN "06231807"   MOVE "5616779" TO �����ԍ��v
           WHEN "06134969"   MOVE "5616787" TO �����ԍ��v
           WHEN "06135024"   MOVE "5616795" TO �����ԍ��v
           WHEN "06135040"   MOVE "5616809" TO �����ԍ��v
           WHEN "63135040"   MOVE "5616809" TO �����ԍ��v
           WHEN "06135057"   MOVE "5616817" TO �����ԍ��v
           WHEN "06135123"   MOVE "5616825" TO �����ԍ��v
           WHEN "06135172"   MOVE "5616833" TO �����ԍ��v
           WHEN "06135180"   MOVE "5616841" TO �����ԍ��v
           WHEN "06135222"   MOVE "5616868" TO �����ԍ��v
           WHEN "06135248"   MOVE "5616876" TO �����ԍ��v
           WHEN "06135255"   MOVE "5616884" TO �����ԍ��v
           WHEN "06135354"   MOVE "5616892" TO �����ԍ��v
           WHEN "06135370"   MOVE "5616906" TO �����ԍ��v
           WHEN "06135388"   MOVE "5616914" TO �����ԍ��v
           WHEN "06135396"   MOVE "5616922" TO �����ԍ��v
           WHEN "06135404"   MOVE "5616930" TO �����ԍ��v
           WHEN "06135438"   MOVE "5616949" TO �����ԍ��v
           WHEN "06135453"   MOVE "5616957" TO �����ԍ��v
           WHEN "06135487"   MOVE "5616965" TO �����ԍ��v
           WHEN "63135487"   MOVE "5616965" TO �����ԍ��v
           WHEN "06135503"   MOVE "5616973" TO �����ԍ��v
           WHEN "06135545"   MOVE "5616981" TO �����ԍ��v
           WHEN "06135552"   MOVE "5617007" TO �����ԍ��v
           WHEN "06135578"   MOVE "5617015" TO �����ԍ��v
           WHEN "06135628"   MOVE "5617023" TO �����ԍ��v
           WHEN "06135669"   MOVE "5617031" TO �����ԍ��v
           WHEN "06135719"   MOVE "5617058" TO �����ԍ��v
           WHEN "06135727"   MOVE "5617066" TO �����ԍ��v
           WHEN "06135750"   MOVE "5617074" TO �����ԍ��v
           WHEN "06135768"   MOVE "5617082" TO �����ԍ��v
           WHEN "06135776"   MOVE "5617090" TO �����ԍ��v
           WHEN "06135784"   MOVE "5617104" TO �����ԍ��v
           WHEN "06135834"   MOVE "5617112" TO �����ԍ��v
           WHEN "06135859"   MOVE "5617120" TO �����ԍ��v
           WHEN "06135891"   MOVE "5617139" TO �����ԍ��v
           WHEN "06135909"   MOVE "5617147" TO �����ԍ��v
           WHEN "06135917"   MOVE "5617155" TO �����ԍ��v
           WHEN "06135990"   MOVE "5617163" TO �����ԍ��v
           WHEN "06136006"   MOVE "5617171" TO �����ԍ��v
           WHEN "06136063"   MOVE "5617198" TO �����ԍ��v
           WHEN "06136097"   MOVE "5617201" TO �����ԍ��v
           WHEN "06136162"   MOVE "5617228" TO �����ԍ��v
           WHEN "06136196"   MOVE "5617236" TO �����ԍ��v
           WHEN "63136196"   MOVE "5617236" TO �����ԍ��v
           WHEN "06136246"   MOVE "5617244" TO �����ԍ��v
           WHEN "06136279"   MOVE "5617252" TO �����ԍ��v
           WHEN "06136287"   MOVE "5617260" TO �����ԍ��v
           WHEN "63136287"   MOVE "5617260" TO �����ԍ��v
           WHEN "06136295"   MOVE "5617279" TO �����ԍ��v
           WHEN "06136345"   MOVE "5617287" TO �����ԍ��v
           WHEN "06136378"   MOVE "5617295" TO �����ԍ��v
           WHEN "06136394"   MOVE "5617309" TO �����ԍ��v
           WHEN "06136410"   MOVE "5617317" TO �����ԍ��v
           WHEN "06136428"   MOVE "5617325" TO �����ԍ��v
           WHEN "06136436"   MOVE "5617333" TO �����ԍ��v
           WHEN "06136477"   MOVE "5617341" TO �����ԍ��v
           WHEN "63136477"   MOVE "5617341" TO �����ԍ��v
           WHEN "06136493"   MOVE "5617368" TO �����ԍ��v
           WHEN "06136501"   MOVE "5617376" TO �����ԍ��v
           WHEN "06136519"   MOVE "5617384" TO �����ԍ��v
           WHEN "06136550"   MOVE "5617392" TO �����ԍ��v
           WHEN "06136568"   MOVE "5617406" TO �����ԍ��v
           WHEN "06136634"   MOVE "5617422" TO �����ԍ��v
           WHEN "06136642"   MOVE "5617430" TO �����ԍ��v
           WHEN "06136659"   MOVE "5617449" TO �����ԍ��v
           WHEN "06136709"   MOVE "5617457" TO �����ԍ��v
           WHEN "06136717"   MOVE "5617465" TO �����ԍ��v
           WHEN "06136741"   MOVE "5617473" TO �����ԍ��v
           WHEN "06136758"   MOVE "5617481" TO �����ԍ��v
           WHEN "06136774"   MOVE "5617503" TO �����ԍ��v
           WHEN "06136790"   MOVE "5617511" TO �����ԍ��v
           WHEN "06136881"   MOVE "5617538" TO �����ԍ��v
           WHEN "06136915"   MOVE "5617546" TO �����ԍ��v
           WHEN "06136923"   MOVE "5617554" TO �����ԍ��v
           WHEN "06136956"   MOVE "5617562" TO �����ԍ��v
           WHEN "06137079"   MOVE "5617570" TO �����ԍ��v
           WHEN "06137087"   MOVE "5617589" TO �����ԍ��v
           WHEN "06137103"   MOVE "5617597" TO �����ԍ��v
           WHEN "06137202"   MOVE "5617600" TO �����ԍ��v
           WHEN "06137210"   MOVE "5617619" TO �����ԍ��v
           WHEN "06137236"   MOVE "5617627" TO �����ԍ��v
           WHEN "06137251"   MOVE "5617635" TO �����ԍ��v
           WHEN "06137277"   MOVE "5617643" TO �����ԍ��v
           WHEN "06137301"   MOVE "5617651" TO �����ԍ��v
           WHEN "06137327"   MOVE "5617678" TO �����ԍ��v
           WHEN "06137335"   MOVE "5617686" TO �����ԍ��v
           WHEN "06137350"   MOVE "5617694" TO �����ԍ��v
           WHEN "06137368"   MOVE "5617708" TO �����ԍ��v
           WHEN "06137376"   MOVE "5617716" TO �����ԍ��v
           WHEN "06137384"   MOVE "5617724" TO �����ԍ��v
           WHEN "06137418"   MOVE "5617732" TO �����ԍ��v
           WHEN "06137442"   MOVE "5617740" TO �����ԍ��v
           WHEN "06137491"   MOVE "5617759" TO �����ԍ��v
           WHEN "06137525"   MOVE "5617767" TO �����ԍ��v
           WHEN "06137566"   MOVE "5617775" TO �����ԍ��v
           WHEN "06137582"   MOVE "5617783" TO �����ԍ��v
           WHEN "06137590"   MOVE "5617791" TO �����ԍ��v
           WHEN "06137608"   MOVE "5617805" TO �����ԍ��v
           WHEN "06137640"   MOVE "5617813" TO �����ԍ��v
           WHEN "06137665"   MOVE "5617821" TO �����ԍ��v
           WHEN "06137681"   MOVE "5617848" TO �����ԍ��v
           WHEN "06137723"   MOVE "5617856" TO �����ԍ��v
           WHEN "06137772"   MOVE "5617864" TO �����ԍ��v
           WHEN "06137798"   MOVE "5617880" TO �����ԍ��v
           WHEN "06137855"   MOVE "5617899" TO �����ԍ��v
           WHEN "63137855"   MOVE "5617899" TO �����ԍ��v
           WHEN "06137863"   MOVE "5617902" TO �����ԍ��v
           WHEN "06137897"   MOVE "5617910" TO �����ԍ��v
           WHEN "06137913"   MOVE "5617929" TO �����ԍ��v
           WHEN "06137947"   MOVE "5617937" TO �����ԍ��v
           WHEN "06137996"   MOVE "5617945" TO �����ԍ��v
           WHEN "06138010"   MOVE "5617953" TO �����ԍ��v
           WHEN "06138051"   MOVE "5617961" TO �����ԍ��v
           WHEN "06138077"   MOVE "5617988" TO �����ԍ��v
           WHEN "06138085"   MOVE "5617996" TO �����ԍ��v
           WHEN "06138093"   MOVE "5618003" TO �����ԍ��v
           WHEN "06138119"   MOVE "5618011" TO �����ԍ��v
           WHEN "06138127"   MOVE "5618038" TO �����ԍ��v
           WHEN "06138143"   MOVE "5618046" TO �����ԍ��v
           WHEN "06138150"   MOVE "5618054" TO �����ԍ��v
           WHEN "63138150"   MOVE "5618054" TO �����ԍ��v
           WHEN "06138168"   MOVE "5618062" TO �����ԍ��v
           WHEN "06138192"   MOVE "5618070" TO �����ԍ��v
           WHEN "06138226"   MOVE "5618089" TO �����ԍ��v
           WHEN "06231930"   MOVE "5618097" TO �����ԍ��v
           WHEN "06138309"   MOVE "5618100" TO �����ԍ��v
           WHEN "06138341"   MOVE "5618119" TO �����ԍ��v
           WHEN "06138424"   MOVE "5618127" TO �����ԍ��v
           WHEN "06138432"   MOVE "5618135" TO �����ԍ��v
           WHEN "06138440"   MOVE "5618143" TO �����ԍ��v
           WHEN "06138457"   MOVE "5618151" TO �����ԍ��v
           WHEN "06138465"   MOVE "5618178" TO �����ԍ��v
           WHEN "06138481"   MOVE "5618186" TO �����ԍ��v
           WHEN "06138499"   MOVE "5618194" TO �����ԍ��v
           WHEN "06138515"   MOVE "5618208" TO �����ԍ��v
           WHEN "06138549"   MOVE "5618216" TO �����ԍ��v
           WHEN "06138564"   MOVE "5618224" TO �����ԍ��v
           WHEN "06138580"   MOVE "5618232" TO �����ԍ��v
           WHEN "63138580"   MOVE "5618232" TO �����ԍ��v
           WHEN "06138663"   MOVE "5618240" TO �����ԍ��v
           WHEN "06138689"   MOVE "5618259" TO �����ԍ��v
           WHEN "06138705"   MOVE "5618267" TO �����ԍ��v
           WHEN "06138713"   MOVE "5618275" TO �����ԍ��v
           WHEN "06138721"   MOVE "5618283" TO �����ԍ��v
           WHEN "06138796"   MOVE "5618291" TO �����ԍ��v
           WHEN "63138796"   MOVE "5618291" TO �����ԍ��v
           WHEN "06139067"   MOVE "5618305" TO �����ԍ��v
           WHEN "06139083"   MOVE "5618313" TO �����ԍ��v
           WHEN "06139117"   MOVE "5618321" TO �����ԍ��v
           WHEN "06139216"   MOVE "5618348" TO �����ԍ��v
           WHEN "06271191"   MOVE "5618348" TO �����ԍ��v
           WHEN "06130025"   MOVE "5618356" TO �����ԍ��v
           WHEN "06130157"   MOVE "5618364" TO �����ԍ��v
           WHEN "06130512"   MOVE "5618372" TO �����ԍ��v
           WHEN "06131254"   MOVE "5618380" TO �����ԍ��v
           WHEN "06132815"   MOVE "5618399" TO �����ԍ��v
           WHEN "06133409"   MOVE "5618402" TO �����ԍ��v
           WHEN "06133532"   MOVE "5618410" TO �����ԍ��v
           WHEN "06133722"   MOVE "5618429" TO �����ԍ��v
           WHEN "06133854"   MOVE "5618437" TO �����ԍ��v
           WHEN "06134753"   MOVE "5618445" TO �����ԍ��v
           WHEN "06134787"   MOVE "5618453" TO �����ԍ��v
           WHEN "06231971"   MOVE "5618461" TO �����ԍ��v
           WHEN "06136535"   MOVE "5618488" TO �����ԍ��v
           WHEN "06137467"   MOVE "5618496" TO �����ԍ��v
           WHEN "06137905"   MOVE "5618518" TO �����ԍ��v
           WHEN "06138184"   MOVE "5618526" TO �����ԍ��v
           WHEN "06138382"   MOVE "5618534" TO �����ԍ��v
           WHEN "06138572"   MOVE "5618542" TO �����ԍ��v
           WHEN "06139075"   MOVE "5618550" TO �����ԍ��v
           WHEN "06139141"   MOVE "5618569" TO �����ԍ��v
           WHEN "06131528"   MOVE "5618577" TO �����ԍ��v
           WHEN "06133136"   MOVE "5618585" TO �����ԍ��v
           WHEN "06133334"   MOVE "5618593" TO �����ԍ��v
           WHEN "06134241"   MOVE "5618607" TO �����ԍ��v
           WHEN "06134670"   MOVE "5618615" TO �����ԍ��v
           WHEN "06134829"   MOVE "5618623" TO �����ԍ��v
           WHEN "06135636"   MOVE "5618631" TO �����ԍ��v
           WHEN "06135974"   MOVE "5618658" TO �����ԍ��v
           WHEN "06136691"   MOVE "5618666" TO �����ԍ��v
           WHEN "06136808"   MOVE "5618674" TO �����ԍ��v
           WHEN "06137004"   MOVE "5618682" TO �����ԍ��v
           WHEN "06137400"   MOVE "5618690" TO �����ԍ��v
           WHEN "06137541"   MOVE "5618704" TO �����ԍ��v
           WHEN "06137822"   MOVE "5618712" TO �����ԍ��v
           WHEN "06138523"   MOVE "5618720" TO �����ԍ��v
           WHEN "06139190"   MOVE "5618739" TO �����ԍ��v
           WHEN "31130016"   MOVE "5618747" TO �����ԍ��v
           WHEN "31130032"   MOVE "5618755" TO �����ԍ��v
           WHEN "31130073"   MOVE "5618763" TO �����ԍ��v
           WHEN "31130131"   MOVE "5618771" TO �����ԍ��v
           WHEN "31130222"   MOVE "5618798" TO �����ԍ��v
           WHEN "31130248"   MOVE "5618801" TO �����ԍ��v
           WHEN "31130305"   MOVE "5618828" TO �����ԍ��v
           WHEN "31130479"   MOVE "5618836" TO �����ԍ��v
           WHEN "31130511"   MOVE "5618844" TO �����ԍ��v
           WHEN "31130537"   MOVE "5618852" TO �����ԍ��v
           WHEN "31130594"   MOVE "5618860" TO �����ԍ��v
           WHEN "31130685"   MOVE "5618879" TO �����ԍ��v
           WHEN "31130842"   MOVE "5618887" TO �����ԍ��v
           WHEN "31110257"   MOVE "5618895" TO �����ԍ��v
           WHEN "31131105"   MOVE "5618895" TO �����ԍ��v
           WHEN "31170178"   MOVE "5618895" TO �����ԍ��v
           WHEN "31430192"   MOVE "5618895" TO �����ԍ��v
           WHEN "31131147"   MOVE "5618909" TO �����ԍ��v
           WHEN "31131188"   MOVE "5618917" TO �����ԍ��v
           WHEN "31131261"   MOVE "5618925" TO �����ԍ��v
           WHEN "31131295"   MOVE "5618933" TO �����ԍ��v
           WHEN "31131311"   MOVE "5618941" TO �����ԍ��v
           WHEN "31131394"   MOVE "5618968" TO �����ԍ��v
           WHEN "31131444"   MOVE "5618976" TO �����ԍ��v
           WHEN "31131535"   MOVE "5618984" TO �����ԍ��v
           WHEN "32130213"   MOVE "5618992" TO �����ԍ��v
           WHEN "32130411"   MOVE "5619018" TO �����ԍ��v
           WHEN "33130014"   MOVE "5619026" TO �����ԍ��v
           WHEN "33130030"   MOVE "5619034" TO �����ԍ��v
           WHEN "34130013"   MOVE "5619042" TO �����ԍ��v
           WHEN "34130021"   MOVE "5619050" TO �����ԍ��v
           WHEN "31131410"   MOVE "5619069" TO �����ԍ��v
           WHEN "31131741"   MOVE "5619077" TO �����ԍ��v
           WHEN "31131774"   MOVE "5619085" TO �����ԍ��v
           WHEN "06232193"   MOVE "5619093" TO �����ԍ��v
           WHEN "06271795"   MOVE "5619107" TO �����ԍ��v
           WHEN "06271829"   MOVE "5619115" TO �����ԍ��v
           WHEN "06271936"   MOVE "5619123" TO �����ԍ��v
           WHEN "06272322"   MOVE "5619131" TO �����ԍ��v
           WHEN "41145004"   MOVE "5619158" TO �����ԍ��v
           WHEN "41145012"   MOVE "5619158" TO �����ԍ��v
           WHEN "41145020"   MOVE "5619158" TO �����ԍ��v
           WHEN "41145038"   MOVE "5619158" TO �����ԍ��v
           WHEN "41145046"   MOVE "5619158" TO �����ԍ��v
           WHEN "41145053"   MOVE "5619158" TO �����ԍ��v
           WHEN "41145061"   MOVE "5619158" TO �����ԍ��v
           WHEN "41145079"   MOVE "5619158" TO �����ԍ��v
           WHEN "06270342"   MOVE "5619166" TO �����ԍ��v
           WHEN "63270342"   MOVE "5619166" TO �����ԍ��v
           WHEN "06270524"   MOVE "5619174" TO �����ԍ��v
           WHEN "06260533"   MOVE "5619182" TO �����ԍ��v
           WHEN "06270680"   MOVE "5619190" TO �����ԍ��v
           WHEN "06270748"   MOVE "5619204" TO �����ԍ��v
           WHEN "06270797"   MOVE "5619212" TO �����ԍ��v
           WHEN "63270797"   MOVE "5619212" TO �����ԍ��v
           WHEN "06270896"   MOVE "5619220" TO �����ԍ��v
           WHEN "06270912"   MOVE "5619239" TO �����ԍ��v
           WHEN "06270953"   MOVE "5619247" TO �����ԍ��v
           WHEN "06271225"   MOVE "5619255" TO �����ԍ��v
           WHEN "06271274"   MOVE "5619263" TO �����ԍ��v
           WHEN "06271563"   MOVE "5619271" TO �����ԍ��v
           WHEN "63271563"   MOVE "5619271" TO �����ԍ��v
           WHEN "06271654"   MOVE "5619298" TO �����ԍ��v
           WHEN "06271787"   MOVE "5619301" TO �����ԍ��v
           WHEN "63271787"   MOVE "5619301" TO �����ԍ��v
           WHEN "41140062"   MOVE "5619328" TO �����ԍ��v
           WHEN "41140179"   MOVE "5619336" TO �����ԍ��v
           WHEN "06132724"   MOVE "5619344" TO �����ԍ��v
           WHEN "06136220"   MOVE "5619352" TO �����ԍ��v
           WHEN "06139182"   MOVE "5619360" TO �����ԍ��v
           WHEN "67110312"   MOVE "5619379" TO �����ԍ��v
           WHEN "67110338"   MOVE "5619387" TO �����ԍ��v
           WHEN "67110478"   MOVE "5619395" TO �����ԍ��v
           WHEN "67110502"   MOVE "5619409" TO �����ԍ��v
           WHEN "67110791"   MOVE "5619417" TO �����ԍ��v
           WHEN "06273312"   MOVE "5619425" TO �����ԍ��v
           WHEN "67138263"   MOVE "5619433" TO �����ԍ��v
           WHEN "67138339"   MOVE "5619441" TO �����ԍ��v
           WHEN "67138495"   MOVE "5619468" TO �����ԍ��v
           WHEN "06139307"   MOVE "5619476" TO �����ԍ��v
           WHEN "06160444"   MOVE "5619484" TO �����ԍ��v
           WHEN "06170229"   MOVE "5619492" TO �����ԍ��v
           WHEN "06220057"   MOVE "5619506" TO �����ԍ��v
           WHEN "06220834"   MOVE "5619514" TO �����ԍ��v
           WHEN "27138304"   MOVE "5619522" TO �����ԍ��v
           WHEN "27138361"   MOVE "5619530" TO �����ԍ��v
           WHEN "81136244"   MOVE "5619549" TO �����ԍ��v
           WHEN "81137242"   MOVE "5619549" TO �����ԍ��v
           WHEN "88131248"   MOVE "5619549" TO �����ԍ��v
           WHEN "88138243"   MOVE "5619549" TO �����ԍ��v
           WHEN "81137358"   MOVE "5619557" TO �����ԍ��v
           WHEN "88131354"   MOVE "5619557" TO �����ԍ��v
           WHEN "88138359"   MOVE "5619557" TO �����ԍ��v
           WHEN "41139015"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139023"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139031"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139049"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139056"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139064"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139072"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139080"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139098"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139106"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139114"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139122"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139130"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139148"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139155"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139163"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139171"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139189"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139197"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139205"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139213"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139221"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139239"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139247"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139254"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139262"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139270"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139288"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139296"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139304"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139312"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139320"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139338"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139346"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139353"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139361"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139379"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139387"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139395"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139403"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139411"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139429"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139437"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139445"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139452"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139460"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139478"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139486"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139494"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139502"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139510"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139528"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139536"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139544"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139551"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139569"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139577"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139585"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139593"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139601"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139619"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139627"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139635"   MOVE "5619565" TO �����ԍ��v
           WHEN "41139643"   MOVE "5619565" TO �����ԍ��v
           WHEN "8013500"    MOVE "5619565" TO �����ԍ��v
           WHEN "80135015"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135023"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135031"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135106"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135122"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135148"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135155"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135171"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135197"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135213"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135221"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135239"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135353"   MOVE "5619565" TO �����ԍ��v
           WHEN "80135478"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136039"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136047"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136054"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136062"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136070"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136088"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136096"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136104"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136112"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136120"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136138"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136161"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136179"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136187"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136195"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136211"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136229"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136237"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136278"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136294"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136328"   MOVE "5619565" TO �����ԍ��v
           WHEN "80136427"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137028"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137029"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137037"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137045"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137052"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137060"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137078"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137086"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137094"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137110"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137128"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137144"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137169"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137177"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137185"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137193"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137201"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137219"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137227"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137235"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137250"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137276"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137318"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137326"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137342"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137359"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137383"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137391"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137425"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137433"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137458"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137474"   MOVE "5619565" TO �����ԍ��v
           WHEN "80137482"   MOVE "5619565" TO �����ԍ��v
           WHEN "81136293"   MOVE "5619573" TO �����ԍ��v
           WHEN "81137291"   MOVE "5619573" TO �����ԍ��v
           WHEN "88132295"   MOVE "5619573" TO �����ԍ��v
           WHEN "88138292"   MOVE "5619573" TO �����ԍ��v
           WHEN "81136459"   MOVE "5619581" TO �����ԍ��v
           WHEN "81137457"   MOVE "5619581" TO �����ԍ��v
           WHEN "88131453"   MOVE "5619581" TO �����ԍ��v
           WHEN "88138458"   MOVE "5619581" TO �����ԍ��v
           WHEN "81136186"   MOVE "5619603" TO �����ԍ��v
           WHEN "81137184"   MOVE "5619603" TO �����ԍ��v
           WHEN "88135181"   MOVE "5619603" TO �����ԍ��v
           WHEN "88138185"   MOVE "5619603" TO �����ԍ��v
           WHEN "81136178"   MOVE "5619611" TO �����ԍ��v
           WHEN "81137176"   MOVE "5619611" TO �����ԍ��v
           WHEN "88132170"   MOVE "5619611" TO �����ԍ��v
           WHEN "88133178"   MOVE "5619611" TO �����ԍ��v
           WHEN "88138177"   MOVE "5619611" TO �����ԍ��v
           WHEN "81137010"   MOVE "5619638" TO �����ԍ��v
           WHEN "88133012"   MOVE "5619638" TO �����ԍ��v
           WHEN "88138011"   MOVE "5619638" TO �����ԍ��v
           WHEN "41405069"   MOVE "5619646" TO �����ԍ��v
           WHEN "80405012"   MOVE "5619646" TO �����ԍ��v
           WHEN "80405020"   MOVE "5619646" TO �����ԍ��v
           WHEN "80405038"   MOVE "5619646" TO �����ԍ��v
           WHEN "80405046"   MOVE "5619646" TO �����ԍ��v
           WHEN "80405053"   MOVE "5619646" TO �����ԍ��v
           WHEN "80405061"   MOVE "5619646" TO �����ԍ��v
           WHEN "80405079"   MOVE "5619646" TO �����ԍ��v
           WHEN "80405087"   MOVE "5619646" TO �����ԍ��v
           WHEN "90405010"   MOVE "5619646" TO �����ԍ��v
           WHEN "90405028"   MOVE "5619646" TO �����ԍ��v
           WHEN "90405036"   MOVE "5619646" TO �����ԍ��v
           WHEN "90405044"   MOVE "5619646" TO �����ԍ��v
           WHEN "90405051"   MOVE "5619646" TO �����ԍ��v
           WHEN "90405069"   MOVE "5619646" TO �����ԍ��v
           WHEN "90405077"   MOVE "5619646" TO �����ԍ��v
           WHEN "90405085"   MOVE "5619646" TO �����ԍ��v
           WHEN "90405093"   MOVE "5619646" TO �����ԍ��v
           WHEN "81405029"   MOVE "5619646" TO �����ԍ��v
           WHEN "01140011"   MOVE "5619654" TO �����ԍ��v
           WHEN "03140019"   MOVE "5619654" TO �����ԍ��v
           WHEN "04140018"   MOVE "5619654" TO �����ԍ��v
           WHEN "01150010"   MOVE "5619662" TO �����ԍ��v
           WHEN "03150018"   MOVE "5619662" TO �����ԍ��v
           WHEN "04150017"   MOVE "5619662" TO �����ԍ��v
           WHEN "01160019"   MOVE "5619670" TO �����ԍ��v
           WHEN "03160017"   MOVE "5619670" TO �����ԍ��v
           WHEN "04160016"   MOVE "5619670" TO �����ԍ��v
           WHEN "01170018"   MOVE "5619689" TO �����ԍ��v
           WHEN "03170016"   MOVE "5619689" TO �����ԍ��v
           WHEN "04170015"   MOVE "5619689" TO �����ԍ��v
           WHEN "01180017"   MOVE "5619697" TO �����ԍ��v
           WHEN "03180015"   MOVE "5619697" TO �����ԍ��v
           WHEN "04180014"   MOVE "5619697" TO �����ԍ��v
           WHEN "01190016"   MOVE "5619700" TO �����ԍ��v
           WHEN "03190014"   MOVE "5619700" TO �����ԍ��v
           WHEN "04190013"   MOVE "5619700" TO �����ԍ��v
           WHEN "01200013"   MOVE "5619719" TO �����ԍ��v
           WHEN "03200011"   MOVE "5619719" TO �����ԍ��v
           WHEN "04200010"   MOVE "5619719" TO �����ԍ��v
           WHEN "01210012"   MOVE "5619727" TO �����ԍ��v
           WHEN "03210010"   MOVE "5619727" TO �����ԍ��v
           WHEN "04210019"   MOVE "5619727" TO �����ԍ��v
           WHEN "01220011"   MOVE "5619735" TO �����ԍ��v
           WHEN "03220019"   MOVE "5619735" TO �����ԍ��v
           WHEN "04220018"   MOVE "5619735" TO �����ԍ��v
           WHEN "01230010"   MOVE "5619743" TO �����ԍ��v
           WHEN "03230018"   MOVE "5619743" TO �����ԍ��v
           WHEN "04230017"   MOVE "5619743" TO �����ԍ��v
           WHEN "01240019"   MOVE "5619751" TO �����ԍ��v
           WHEN "03240017"   MOVE "5619751" TO �����ԍ��v
           WHEN "04240016"   MOVE "5619751" TO �����ԍ��v
           WHEN "01250018"   MOVE "5619778" TO �����ԍ��v
           WHEN "03250016"   MOVE "5619778" TO �����ԍ��v
           WHEN "04250015"   MOVE "5619778" TO �����ԍ��v
           WHEN "01260017"   MOVE "5619786" TO �����ԍ��v
           WHEN "03260015"   MOVE "5619786" TO �����ԍ��v
           WHEN "04260014"   MOVE "5619786" TO �����ԍ��v
           WHEN "01270016"   MOVE "5619794" TO �����ԍ��v
           WHEN "03270014"   MOVE "5619794" TO �����ԍ��v
           WHEN "04270013"   MOVE "5619794" TO �����ԍ��v
           WHEN "01280015"   MOVE "5619808" TO �����ԍ��v
           WHEN "03280013"   MOVE "5619808" TO �����ԍ��v
           WHEN "04280012"   MOVE "5619808" TO �����ԍ��v
           WHEN "01290014"   MOVE "5619816" TO �����ԍ��v
           WHEN "03290012"   MOVE "5619816" TO �����ԍ��v
           WHEN "04290011"   MOVE "5619816" TO �����ԍ��v
           WHEN "01300011"   MOVE "5619824" TO �����ԍ��v
           WHEN "03300019"   MOVE "5619824" TO �����ԍ��v
           WHEN "04300018"   MOVE "5619824" TO �����ԍ��v
           WHEN "01310010"   MOVE "5619832" TO �����ԍ��v
           WHEN "03310018"   MOVE "5619832" TO �����ԍ��v
           WHEN "04310017"   MOVE "5619832" TO �����ԍ��v
           WHEN "01320019"   MOVE "5619840" TO �����ԍ��v
           WHEN "03320017"   MOVE "5619840" TO �����ԍ��v
           WHEN "04320016"   MOVE "5619840" TO �����ԍ��v
           WHEN "01330018"   MOVE "5619859" TO �����ԍ��v
           WHEN "03330016"   MOVE "5619859" TO �����ԍ��v
           WHEN "04330015"   MOVE "5619859" TO �����ԍ��v
           WHEN "01340017"   MOVE "5619867" TO �����ԍ��v
           WHEN "03340015"   MOVE "5619867" TO �����ԍ��v
           WHEN "04340014"   MOVE "5619867" TO �����ԍ��v
           WHEN "01350016"   MOVE "5619875" TO �����ԍ��v
           WHEN "03350014"   MOVE "5619875" TO �����ԍ��v
           WHEN "04350013"   MOVE "5619875" TO �����ԍ��v
           WHEN "01360015"   MOVE "5619883" TO �����ԍ��v
           WHEN "03360013"   MOVE "5619883" TO �����ԍ��v
           WHEN "04360012"   MOVE "5619883" TO �����ԍ��v
           WHEN "01370014"   MOVE "5619891" TO �����ԍ��v
           WHEN "03370012"   MOVE "5619891" TO �����ԍ��v
           WHEN "04370011"   MOVE "5619891" TO �����ԍ��v
           WHEN "01380013"   MOVE "5619905" TO �����ԍ��v
           WHEN "03380011"   MOVE "5619905" TO �����ԍ��v
           WHEN "04380010"   MOVE "5619905" TO �����ԍ��v
           WHEN "01390012"   MOVE "5619913" TO �����ԍ��v
           WHEN "03390010"   MOVE "5619913" TO �����ԍ��v
           WHEN "04390019"   MOVE "5619913" TO �����ԍ��v
           WHEN "01400019"   MOVE "5619921" TO �����ԍ��v
           WHEN "03400017"   MOVE "5619921" TO �����ԍ��v
           WHEN "04400016"   MOVE "5619921" TO �����ԍ��v
           WHEN "01410018"   MOVE "5619948" TO �����ԍ��v
           WHEN "03410016"   MOVE "5619948" TO �����ԍ��v
           WHEN "04410015"   MOVE "5619948" TO �����ԍ��v
           WHEN "01420017"   MOVE "5619956" TO �����ԍ��v
           WHEN "03420015"   MOVE "5619956" TO �����ԍ��v
           WHEN "04420014"   MOVE "5619956" TO �����ԍ��v
           WHEN "01430016"   MOVE "5619964" TO �����ԍ��v
           WHEN "03430014"   MOVE "5619964" TO �����ԍ��v
           WHEN "04430013"   MOVE "5619964" TO �����ԍ��v
           WHEN "01440015"   MOVE "5619972" TO �����ԍ��v
           WHEN "03440013"   MOVE "5619972" TO �����ԍ��v
           WHEN "04440012"   MOVE "5619972" TO �����ԍ��v
           WHEN "01450014"   MOVE "5619980" TO �����ԍ��v
           WHEN "03450012"   MOVE "5619980" TO �����ԍ��v
           WHEN "04450011"   MOVE "5619980" TO �����ԍ��v
           WHEN "01460013"   MOVE "5619999" TO �����ԍ��v
           WHEN "03460011"   MOVE "5619999" TO �����ԍ��v
           WHEN "04460010"   MOVE "5619999" TO �����ԍ��v
           WHEN "01470012"   MOVE "5620008" TO �����ԍ��v
           WHEN "03470010"   MOVE "5620008" TO �����ԍ��v
           WHEN "04470019"   MOVE "5620008" TO �����ԍ��v
           WHEN "39131016"   MOVE "5620326" TO �����ԍ��v
           WHEN "39131024"   MOVE "5620334" TO �����ԍ��v
           WHEN "39131032"   MOVE "5620342" TO �����ԍ��v
           WHEN "39131040"   MOVE "5620350" TO �����ԍ��v
           WHEN "39131057"   MOVE "5620369" TO �����ԍ��v
           WHEN "39131065"   MOVE "5620377" TO �����ԍ��v
           WHEN "39131073"   MOVE "5620385" TO �����ԍ��v
           WHEN "39131081"   MOVE "5620393" TO �����ԍ��v
           WHEN "39131099"   MOVE "5620407" TO �����ԍ��v
           WHEN "39131107"   MOVE "5620415" TO �����ԍ��v
           WHEN "39131115"   MOVE "5620423" TO �����ԍ��v
           WHEN "39131123"   MOVE "5620431" TO �����ԍ��v
           WHEN "39131131"   MOVE "5620458" TO �����ԍ��v
           WHEN "39131149"   MOVE "5620466" TO �����ԍ��v
           WHEN "39131156"   MOVE "5620474" TO �����ԍ��v
           WHEN "39131164"   MOVE "5620482" TO �����ԍ��v
           WHEN "39131172"   MOVE "5620490" TO �����ԍ��v
           WHEN "39131180"   MOVE "5620504" TO �����ԍ��v
           WHEN "39131198"   MOVE "5620512" TO �����ԍ��v
           WHEN "39131206"   MOVE "5620520" TO �����ԍ��v
           WHEN "39131214"   MOVE "5620539" TO �����ԍ��v
           WHEN "39131222"   MOVE "5620547" TO �����ԍ��v
           WHEN "39131230"   MOVE "5620555" TO �����ԍ��v
           WHEN "39132014"   MOVE "5620563" TO �����ԍ��v
           WHEN "39132022"   MOVE "5620571" TO �����ԍ��v
           WHEN "39132030"   MOVE "5620598" TO �����ԍ��v
           WHEN "39132048"   MOVE "5620601" TO �����ԍ��v
           WHEN "39132055"   MOVE "5620628" TO �����ԍ��v
           WHEN "39132063"   MOVE "5620636" TO �����ԍ��v
           WHEN "39132071"   MOVE "5620644" TO �����ԍ��v
           WHEN "39132089"   MOVE "5620652" TO �����ԍ��v
           WHEN "39132097"   MOVE "5620660" TO �����ԍ��v
           WHEN "39132105"   MOVE "5620679" TO �����ԍ��v
           WHEN "39132113"   MOVE "5620687" TO �����ԍ��v
           WHEN "39132121"   MOVE "5620695" TO �����ԍ��v
           WHEN "39132139"   MOVE "5620709" TO �����ԍ��v
           WHEN "39132147"   MOVE "5620717" TO �����ԍ��v
           WHEN "39132154"   MOVE "5620725" TO �����ԍ��v
           WHEN "39132188"   MOVE "5620733" TO �����ԍ��v
           WHEN "39132196"   MOVE "5620741" TO �����ԍ��v
           WHEN "39132204"   MOVE "5620768" TO �����ԍ��v
           WHEN "39132212"   MOVE "5620776" TO �����ԍ��v
           WHEN "39132220"   MOVE "5620784" TO �����ԍ��v
           WHEN "39132238"   MOVE "5620792" TO �����ԍ��v
           WHEN "39132246"   MOVE "5620806" TO �����ԍ��v
           WHEN "39132253"   MOVE "5620814" TO �����ԍ��v
           WHEN "39132279"   MOVE "5620822" TO �����ԍ��v
           WHEN "39132287"   MOVE "5620830" TO �����ԍ��v
           WHEN "39132295"   MOVE "5620849" TO �����ԍ��v
           WHEN "39133038"   MOVE "5620857" TO �����ԍ��v
           WHEN "39133053"   MOVE "5620865" TO �����ԍ��v
           WHEN "39133079"   MOVE "5620873" TO �����ԍ��v
           WHEN "39133087"   MOVE "5620881" TO �����ԍ��v
           WHEN "39352018"   MOVE "5620946" TO �����ԍ��v
           WHEN "39352026"   MOVE "5620946" TO �����ԍ��v
           WHEN "39352034"   MOVE "5620946" TO �����ԍ��v
           WHEN "39352042"   MOVE "5620946" TO �����ԍ��v
           WHEN "39352067"   MOVE "5620946" TO �����ԍ��v
           WHEN "39352075"   MOVE "5620946" TO �����ԍ��v
           WHEN "39352083"   MOVE "5620946" TO �����ԍ��v
           WHEN "39352109"   MOVE "5620946" TO �����ԍ��v
           WHEN "39352117"   MOVE "5620946" TO �����ԍ��v
           WHEN "39352125"   MOVE "5620946" TO �����ԍ��v
           WHEN "39352133"   MOVE "5620946" TO �����ԍ��v
           WHEN "39352158"   MOVE "5620946" TO �����ԍ��v
           WHEN "39352166"   MOVE "5620946" TO �����ԍ��v
           WHEN "39353057"   MOVE "5620946" TO �����ԍ��v
           WHEN "39353214"   MOVE "5620946" TO �����ԍ��v
           WHEN "39353412"   MOVE "5620946" TO �����ԍ��v
           WHEN "39353438"   MOVE "5620946" TO �����ԍ��v
           WHEN "39353446"   MOVE "5620946" TO �����ԍ��v
           WHEN "39355029"   MOVE "5620946" TO �����ԍ��v
           WHEN "39355045"   MOVE "5620946" TO �����ԍ��v
           WHEN "39412010"   MOVE "5620954" TO �����ԍ��v
           WHEN "39412028"   MOVE "5620954" TO �����ԍ��v
           WHEN "39412036"   MOVE "5620954" TO �����ԍ��v
           WHEN "39412044"   MOVE "5620954" TO �����ԍ��v
           WHEN "39412051"   MOVE "5620954" TO �����ԍ��v
           WHEN "39412069"   MOVE "5620954" TO �����ԍ��v
           WHEN "39412077"   MOVE "5620954" TO �����ԍ��v
           WHEN "39412085"   MOVE "5620954" TO �����ԍ��v
           WHEN "39412093"   MOVE "5620954" TO �����ԍ��v
           WHEN "39412101"   MOVE "5620954" TO �����ԍ��v
           WHEN "39413273"   MOVE "5620954" TO �����ԍ��v
           WHEN "39413414"   MOVE "5620954" TO �����ԍ��v
           WHEN "39413455"   MOVE "5620954" TO �����ԍ��v
           WHEN "39413463"   MOVE "5620954" TO �����ԍ��v
           WHEN "39413877"   MOVE "5620954" TO �����ԍ��v
           WHEN "39414016"   MOVE "5620954" TO �����ԍ��v
           WHEN "39414230"   MOVE "5620954" TO �����ԍ��v
           WHEN "39414248"   MOVE "5620954" TO �����ԍ��v
           WHEN "39414255"   MOVE "5620954" TO �����ԍ��v
           WHEN "39414412"   MOVE "5620954" TO �����ԍ��v
           WHEN "07010135"   MOVE "0115697" TO �����ԍ��v
           WHEN "07010150"   MOVE "0115697" TO �����ԍ��v
           WHEN "07080120"   MOVE "0115697" TO �����ԍ��v
           WHEN "07090103"   MOVE "0115697" TO �����ԍ��v
           WHEN "07100118"   MOVE "0115697" TO �����ԍ��v
           WHEN "07110026"   MOVE "0115697" TO �����ԍ��v
           WHEN "07110117"   MOVE "0115697" TO �����ԍ��v
           WHEN "07110604"   MOVE "0115697" TO �����ԍ��v
           WHEN "07110612"   MOVE "0115697" TO �����ԍ��v
           WHEN "07120017"   MOVE "0115697" TO �����ԍ��v
           WHEN "07120108"   MOVE "0115697" TO �����ԍ��v
           WHEN "07120504"   MOVE "0115697" TO �����ԍ��v
           WHEN "07120603"   MOVE "0115697" TO �����ԍ��v
           WHEN "07130107"   MOVE "0115697" TO �����ԍ��v
           WHEN "07130123"   MOVE "0115697" TO �����ԍ��v
           WHEN "07130198"   MOVE "0115697" TO �����ԍ��v
           WHEN "07130511"   MOVE "0115697" TO �����ԍ��v
           WHEN "07130610"   MOVE "0115697" TO �����ԍ��v
           WHEN "07130636"   MOVE "0115697" TO �����ԍ��v
           WHEN "07140114"   MOVE "0115697" TO �����ԍ��v
           WHEN "07140122"   MOVE "0115697" TO �����ԍ��v
           WHEN "07140536"   MOVE "0115697" TO �����ԍ��v
           WHEN "07140544"   MOVE "0115697" TO �����ԍ��v
           WHEN "07350515"   MOVE "0115697" TO �����ԍ��v
           WHEN "07380017"   MOVE "0115697" TO �����ԍ��v
           WHEN "07420516"   MOVE "0115697" TO �����ԍ��v
           WHEN "07470115"   MOVE "0115697" TO �����ԍ��v
      */����25�N11���{�p�����ύX������/131126
      *     WHEN "120014"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120022"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120030"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120048"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120055"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120063"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120071"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120089"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120097"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120105"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120113"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120121"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120139"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120147"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120154"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120162"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120170"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120188"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120196"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120204"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120212"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120220"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120238"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120246"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120253"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120261"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120451"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120519"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120527"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120535"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120543"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120550"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120568"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120576"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120584"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120592"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120600"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120618"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120626"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120634"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120642"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120659"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120667"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120675"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120683"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120691"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120709"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120717"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120725"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120733"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120741"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120758"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120766"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120774"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120782"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120790"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120808"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120816"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120824"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120832"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120840"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120857"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120865"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120873"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120881"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120899"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120907"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120915"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120923"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120931"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120949"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120956"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120964"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120972"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120980"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "120998"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "121004"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "121012"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "121020"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "121038"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "121046"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "123018"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "123026"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "123034"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "124008"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "124016"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "124024"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "124032"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "124040"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "124057"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "124065"     MOVE "5610010" TO �����ԍ��v
      *     WHEN "19126010"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120013"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120021"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120039"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120047"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120054"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120062"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120070"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120088"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120096"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120104"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120112"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120120"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120138"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120146"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120153"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120161"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120179"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120187"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120195"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120203"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120211"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120229"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120237"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120245"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120252"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120260"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120419"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120518"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120526"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120534"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120542"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120559"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120567"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120575"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120583"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120591"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120609"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120617"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120625"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120633"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120641"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120658"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120666"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120674"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120682"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120690"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120708"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120716"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120724"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120732"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120740"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120757"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120765"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120773"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120781"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120799"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120807"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120815"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120823"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120831"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120849"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120856"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120864"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120872"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120880"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120898"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120906"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120914"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120922"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120930"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120948"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120955"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120963"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120971"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120989"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27120997"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27121003"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27121011"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27121029"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27121037"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27121045"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27124007"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27124015"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27124023"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27124031"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27124049"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27124056"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "27124064"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120023"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120031"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120049"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120056"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120064"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120072"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120080"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120098"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120106"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120114"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120122"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120130"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120148"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120155"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120163"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120171"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120189"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120197"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120205"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120213"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120221"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120239"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120247"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120254"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120262"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120510"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120528"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120536"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120544"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120551"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120569"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120577"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120585"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120593"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120601"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120619"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120627"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120635"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120643"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120650"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120668"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120676"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120684"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120692"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120700"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120718"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120726"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120734"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120742"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120759"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120767"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120775"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120783"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120791"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120809"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120817"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120825"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120833"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120841"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120858"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120866"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120874"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120882"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120890"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120908"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120916"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120924"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120932"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120940"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120957"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120965"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120973"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120981"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41120999"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41121005"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41121013"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41121021"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41121039"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41121047"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41124009"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41124017"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41124025"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41124033"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41124041"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41124058"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "41124066"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120014"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120022"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120030"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120048"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120055"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120063"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120071"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120089"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120097"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120105"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120113"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120121"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120139"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120147"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120154"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120162"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120170"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120188"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120196"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120204"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120212"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120220"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120238"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120246"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120253"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120261"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120519"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120527"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120535"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120543"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120550"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120568"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120576"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120584"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120592"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120600"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120618"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120626"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120634"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120642"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120659"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120667"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120675"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120683"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120691"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120709"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120717"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120725"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120733"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120741"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120758"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120766"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120774"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120782"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120790"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120808"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120816"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120824"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120832"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120840"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120857"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120865"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120873"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120881"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120899"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120907"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120915"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120923"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120931"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120949"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120956"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120964"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120972"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120980"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67120998"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67121004"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67121012"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67121020"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67121038"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67121046"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67124016"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67124024"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67124032"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67124040"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67124057"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "67124065"   MOVE "5610010" TO �����ԍ��v
      *     WHEN "110684"     MOVE "5610029" TO �����ԍ��v
      *     WHEN "110700"     MOVE "5610037" TO �����ԍ��v
      *     WHEN "110783"     MOVE "5610045" TO �����ԍ��v
      *     WHEN "138529"     MOVE "5610053" TO �����ԍ��v
      *     WHEN "138586"     MOVE "5610061" TO �����ԍ��v
      *     WHEN "114108"     MOVE "5610088" TO �����ԍ��v
      *     WHEN "138293"     MOVE "5610096" TO �����ԍ��v
      *     WHEN "120279"     MOVE "5610118" TO �����ԍ��v
      *     WHEN "110015"     MOVE "5610126" TO �����ԍ��v
      *     WHEN "110023"     MOVE "5610134" TO �����ԍ��v
      *     WHEN "110031"     MOVE "5610142" TO �����ԍ��v
      *     WHEN "67110031"   MOVE "5610142" TO �����ԍ��v
      *     WHEN "110080"     MOVE "5610150" TO �����ԍ��v
      *     WHEN "110098"     MOVE "5610169" TO �����ԍ��v
      *     WHEN "110106"     MOVE "5610177" TO �����ԍ��v
      *     WHEN "110130"     MOVE "5610185" TO �����ԍ��v
      *     WHEN "110148"     MOVE "5610193" TO �����ԍ��v
      *     WHEN "110213"     MOVE "5610207" TO �����ԍ��v
      *     WHEN "110221"     MOVE "5610215" TO �����ԍ��v
      *     WHEN "110296"     MOVE "5610223" TO �����ԍ��v
      *     WHEN "110346"     MOVE "5610231" TO �����ԍ��v
      *     WHEN "110353"     MOVE "5610258" TO �����ԍ��v
      *     WHEN "110361"     MOVE "5610266" TO �����ԍ��v
      *     WHEN "110379"     MOVE "5610274" TO �����ԍ��v
      *     WHEN "110403"     MOVE "5610282" TO �����ԍ��v
      *     WHEN "110411"     MOVE "5610290" TO �����ԍ��v
      *     WHEN "110429"     MOVE "5610304" TO �����ԍ��v
      *     WHEN "27110428"   MOVE "5610304" TO �����ԍ��v
      *     WHEN "41110420"   MOVE "5610304" TO �����ԍ��v
      *     WHEN "67110429"   MOVE "5610304" TO �����ԍ��v
      *     WHEN "110437"     MOVE "5610312" TO �����ԍ��v
      *     WHEN "110759"     MOVE "5610320" TO �����ԍ��v
      *     WHEN "110791"     MOVE "5610339" TO �����ԍ��v
      *     WHEN "110833"     MOVE "5610347" TO �����ԍ��v
      *     WHEN "110841"     MOVE "5610355" TO �����ԍ��v
      *     WHEN "110890"     MOVE "5610363" TO �����ԍ��v
      *     WHEN "110908"     MOVE "5610371" TO �����ԍ��v
      *     WHEN "110932"     MOVE "5610398" TO �����ԍ��v
      *     WHEN "110940"     MOVE "5610401" TO �����ԍ��v
      *     WHEN "138016"     MOVE "5610428" TO �����ԍ��v
      *     WHEN "138024"     MOVE "5610436" TO �����ԍ��v
      *     WHEN "138032"     MOVE "5610444" TO �����ԍ��v
      *     WHEN "138040"     MOVE "5610452" TO �����ԍ��v
      *     WHEN "138057"     MOVE "5610460" TO �����ԍ��v
      *     WHEN "138065"     MOVE "5610479" TO �����ԍ��v
      *     WHEN "138073"     MOVE "5610487" TO �����ԍ��v
      *     WHEN "138081"     MOVE "5610495" TO �����ԍ��v
      *     WHEN "138099"     MOVE "5610509" TO �����ԍ��v
      *     WHEN "138107"     MOVE "5610517" TO �����ԍ��v
      *     WHEN "138115"     MOVE "5610525" TO �����ԍ��v
      *     WHEN "138123"     MOVE "5610533" TO �����ԍ��v
      *     WHEN "138131"     MOVE "5610541" TO �����ԍ��v
      *     WHEN "138149"     MOVE "5610568" TO �����ԍ��v
      *     WHEN "138156"     MOVE "5610576" TO �����ԍ��v
      *     WHEN "67138156"   MOVE "5610576" TO �����ԍ��v
      *     WHEN "138164"     MOVE "5610584" TO �����ԍ��v
      *     WHEN "138172"     MOVE "5610592" TO �����ԍ��v
      *     WHEN "138180"     MOVE "5610606" TO �����ԍ��v
      *     WHEN "138198"     MOVE "5610614" TO �����ԍ��v
      *     WHEN "67138198"   MOVE "5610614" TO �����ԍ��v
      *     WHEN "138206"     MOVE "5610622" TO �����ԍ��v
      *     WHEN "138214"     MOVE "5610630" TO �����ԍ��v
      *     WHEN "138222"     MOVE "5610649" TO �����ԍ��v
      *     WHEN "27138221"   MOVE "5610649" TO �����ԍ��v
      *     WHEN "67138222"   MOVE "5610649" TO �����ԍ��v
      *     WHEN "81136228"   MOVE "5610649" TO �����ԍ��v
      *     WHEN "81137226"   MOVE "5610649" TO �����ԍ��v
      *     WHEN "138230"     MOVE "5610657" TO �����ԍ��v
      *     WHEN "27138239"   MOVE "5610657" TO �����ԍ��v
      *     WHEN "67138230"   MOVE "5610657" TO �����ԍ��v
      *     WHEN "81136236"   MOVE "5610657" TO �����ԍ��v
      *     WHEN "81137234"   MOVE "5610657" TO �����ԍ��v
      *     WHEN "138248"     MOVE "5610665" TO �����ԍ��v
      *     WHEN "138313"     MOVE "5610673" TO �����ԍ��v
      *     WHEN "138321"     MOVE "5610681" TO �����ԍ��v
      *     WHEN "138347"     MOVE "5610703" TO �����ԍ��v
      *     WHEN "138354"     MOVE "5610711" TO �����ԍ��v
      *     WHEN "138396"     MOVE "5610738" TO �����ԍ��v
      *     WHEN "138479"     MOVE "5610746" TO �����ԍ��v
      *     WHEN "138487"     MOVE "5610754" TO �����ԍ��v
      *     WHEN "140038"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140046"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140053"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140061"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140079"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140087"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140095"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140103"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140111"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140129"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140137"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140145"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140152"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140160"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140178"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140186"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140517"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140525"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140533"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140541"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140558"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140566"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140574"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140582"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140590"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140608"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140616"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140624"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140632"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140640"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140657"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140665"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140673"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140681"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "140699"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "143016"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "143024"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "143032"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "143040"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "143057"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "143065"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144006"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144014"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144022"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144030"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144048"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144055"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144063"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144071"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144089"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144097"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144105"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144113"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144121"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144139"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144147"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144154"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144162"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144170"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "144188"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "145003"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "145011"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "145029"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "145037"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "145045"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "145052"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "145060"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "145078"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "1914601"    MOVE "5610762" TO �����ԍ��v
      *     WHEN "19146018"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140037"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140045"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140052"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140060"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140078"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140086"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140094"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140102"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140110"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140128"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140136"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140144"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140151"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140169"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140177"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140185"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140516"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140524"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140532"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140540"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140557"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140565"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140573"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140581"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140599"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140607"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140615"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140623"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140631"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140649"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140656"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140664"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140672"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140680"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27140698"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144005"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144013"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144021"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144039"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144047"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144054"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144062"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144070"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144088"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144096"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144104"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144112"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144120"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144138"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144146"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144153"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144161"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144179"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27144187"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27145002"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27145010"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27145028"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27145036"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27145044"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27145051"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27145069"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "27145077"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140038"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140046"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140053"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140061"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140079"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140087"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140095"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140103"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140111"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140129"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140137"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140145"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140152"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140160"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140178"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140186"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140517"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140525"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140533"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140541"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140558"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140566"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140574"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140582"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140590"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140608"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140616"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140624"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140632"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140640"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140657"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140665"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140673"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140681"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67140699"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144014"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144022"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144030"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144048"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144055"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144063"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144071"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144089"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144097"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144105"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144113"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144121"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144139"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144147"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144154"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144162"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144170"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67144188"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67145011"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67145029"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67145037"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67145045"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67145060"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67145078"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "113027"     MOVE "5610770" TO �����ԍ��v
      *     WHEN "113043"     MOVE "5610789" TO �����ԍ��v
      *     WHEN "113050"     MOVE "5610797" TO �����ԍ��v
      *     WHEN "113068"     MOVE "5610800" TO �����ԍ��v
      *     WHEN "133033"     MOVE "5610819" TO �����ԍ��v
      *     WHEN "133041"     MOVE "5610827" TO �����ԍ��v
      *     WHEN "133066"     MOVE "5610835" TO �����ԍ��v
      *     WHEN "133074"     MOVE "5610843" TO �����ԍ��v
      *     WHEN "133090"     MOVE "5610851" TO �����ԍ��v
      *     WHEN "133132"     MOVE "5610878" TO �����ԍ��v
      *     WHEN "133140"     MOVE "5610886" TO �����ԍ��v
      *     WHEN "133157"     MOVE "5610894" TO �����ԍ��v
      *     WHEN "133165"     MOVE "5610908" TO �����ԍ��v
      *     WHEN "133173"     MOVE "5610916" TO �����ԍ��v
      *     WHEN "133199"     MOVE "5610924" TO �����ԍ��v
      *     WHEN "133207"     MOVE "5610932" TO �����ԍ��v
      *     WHEN "133223"     MOVE "5610940" TO �����ԍ��v
      *     WHEN "133231"     MOVE "5610959" TO �����ԍ��v
      *     WHEN "133249"     MOVE "5610967" TO �����ԍ��v
      *     WHEN "133256"     MOVE "5610975" TO �����ԍ��v
      *     WHEN "133264"     MOVE "5610983" TO �����ԍ��v
      *     WHEN "133272"     MOVE "5610991" TO �����ԍ��v
      *     WHEN "133298"     MOVE "5611009" TO �����ԍ��v
      *     WHEN "67110106"   MOVE "5611017" TO �����ԍ��v
      *     WHEN "67110148"   MOVE "5611025" TO �����ԍ��v
      *     WHEN "67110213"   MOVE "5611033" TO �����ԍ��v
      *     WHEN "67110221"   MOVE "5611041" TO �����ԍ��v
      *     WHEN "67110239"   MOVE "5611068" TO �����ԍ��v
      *     WHEN "67110361"   MOVE "5611076" TO �����ԍ��v
      *     WHEN "67110387"   MOVE "5611084" TO �����ԍ��v
      *     WHEN "67110411"   MOVE "5611092" TO �����ԍ��v
      *     WHEN "67110841"   MOVE "5611106" TO �����ԍ��v
      *     WHEN "67110890"   MOVE "5611114" TO �����ԍ��v
      *     WHEN "67110932"   MOVE "5611122" TO �����ԍ��v
      *     WHEN "67138016"   MOVE "5611130" TO �����ԍ��v
      *     WHEN "67138024"   MOVE "5611149" TO �����ԍ��v
      *     WHEN "67138032"   MOVE "5611157" TO �����ԍ��v
      *     WHEN "67138040"   MOVE "5611165" TO �����ԍ��v
      *     WHEN "67138057"   MOVE "5611173" TO �����ԍ��v
      *     WHEN "67138065"   MOVE "5611181" TO �����ԍ��v
      *     WHEN "67138073"   MOVE "5611203" TO �����ԍ��v
      *     WHEN "67138081"   MOVE "5611211" TO �����ԍ��v
      *     WHEN "67138099"   MOVE "5611238" TO �����ԍ��v
      *     WHEN "67138107"   MOVE "5611246" TO �����ԍ��v
      *     WHEN "67138115"   MOVE "5611254" TO �����ԍ��v
      *     WHEN "67138123"   MOVE "5611262" TO �����ԍ��v
      *     WHEN "67138131"   MOVE "5611270" TO �����ԍ��v
      *     WHEN "67138149"   MOVE "5611289" TO �����ԍ��v
      *     WHEN "67138164"   MOVE "5611297" TO �����ԍ��v
      *     WHEN "67138172"   MOVE "5611300" TO �����ԍ��v
      *     WHEN "67138180"   MOVE "5611319" TO �����ԍ��v
      *     WHEN "67138206"   MOVE "5611327" TO �����ԍ��v
      *     WHEN "67138214"   MOVE "5611335" TO �����ԍ��v
      *     WHEN "67110023"   MOVE "5611343" TO �����ԍ��v
      *     WHEN "67110072"   MOVE "5611351" TO �����ԍ��v
      *     WHEN "67138313"   MOVE "5611378" TO �����ԍ��v
      *     WHEN "67138354"   MOVE "5611386" TO �����ԍ��v
      *     WHEN "67138362"   MOVE "5611394" TO �����ԍ��v
      *     WHEN "67138479"   MOVE "5611408" TO �����ԍ��v
      *     WHEN "67138487"   MOVE "5611416" TO �����ԍ��v
      *     WHEN "67138586"   MOVE "5611424" TO �����ԍ��v
      *     WHEN "67110098"   MOVE "5611432" TO �����ԍ��v
      *     WHEN "67110320"   MOVE "5611440" TO �����ԍ��v
      *     WHEN "67110445"   MOVE "5611459" TO �����ԍ��v
      *     WHEN "67110551"   MOVE "5611467" TO �����ԍ��v
      *     WHEN "27110014"   MOVE "5611475" TO �����ԍ��v
      *     WHEN "27110089"   MOVE "5611483" TO �����ԍ��v
      *     WHEN "27110105"   MOVE "5611491" TO �����ԍ��v
      *     WHEN "27110139"   MOVE "5611505" TO �����ԍ��v
      *     WHEN "27110147"   MOVE "5611513" TO �����ԍ��v
      *     WHEN "27110345"   MOVE "5611521" TO �����ԍ��v
      *     WHEN "27110352"   MOVE "5611548" TO �����ԍ��v
      *     WHEN "27110360"   MOVE "5611556" TO �����ԍ��v
      *     WHEN "27110378"   MOVE "5611564" TO �����ԍ��v
      *     WHEN "27110840"   MOVE "5611572" TO �����ԍ��v
      *     WHEN "27110931"   MOVE "5611580" TO �����ԍ��v
      *     WHEN "27138015"   MOVE "5611599" TO �����ԍ��v
      *     WHEN "27138023"   MOVE "5611602" TO �����ԍ��v
      *     WHEN "27138031"   MOVE "5611610" TO �����ԍ��v
      *     WHEN "27138049"   MOVE "5611629" TO �����ԍ��v
      *     WHEN "27138056"   MOVE "5611637" TO �����ԍ��v
      *     WHEN "27138064"   MOVE "5611645" TO �����ԍ��v
      *     WHEN "27138072"   MOVE "5611653" TO �����ԍ��v
      *     WHEN "27138080"   MOVE "5611661" TO �����ԍ��v
      *     WHEN "27138098"   MOVE "5611688" TO �����ԍ��v
      *     WHEN "27138106"   MOVE "5611696" TO �����ԍ��v
      *     WHEN "27138114"   MOVE "5611718" TO �����ԍ��v
      *     WHEN "27138122"   MOVE "5611726" TO �����ԍ��v
      *     WHEN "27138130"   MOVE "5611734" TO �����ԍ��v
      *     WHEN "27138148"   MOVE "5611742" TO �����ԍ��v
      *     WHEN "27138155"   MOVE "5611750" TO �����ԍ��v
      *     WHEN "27138163"   MOVE "5611769" TO �����ԍ��v
      *     WHEN "27138171"   MOVE "5611777" TO �����ԍ��v
      *     WHEN "27138189"   MOVE "5611785" TO �����ԍ��v
      *     WHEN "27138197"   MOVE "5611793" TO �����ԍ��v
      *     WHEN "27138205"   MOVE "5611807" TO �����ԍ��v
      *     WHEN "27138213"   MOVE "5611815" TO �����ԍ��v
      *     WHEN "81136210"   MOVE "5611815" TO �����ԍ��v
      *     WHEN "81137218"   MOVE "5611815" TO �����ԍ��v
      *     WHEN "88132212"   MOVE "5611815" TO �����ԍ��v
      *     WHEN "88138219"   MOVE "5611815" TO �����ԍ��v
      *     WHEN "67110726"   MOVE "5611823" TO �����ԍ��v
      *     WHEN "27138320"   MOVE "5611831" TO �����ԍ��v
      *     WHEN "27138353"   MOVE "5611858" TO �����ԍ��v
      *     WHEN "27138395"   MOVE "5611866" TO �����ԍ��v
      *     WHEN "27138478"   MOVE "5611874" TO �����ԍ��v
      *     WHEN "67138248"   MOVE "5611882" TO �����ԍ��v
      *     WHEN "67138255"   MOVE "5611890" TO �����ԍ��v
      *     WHEN "41140104"   MOVE "5611904" TO �����ԍ��v
      *     WHEN "80140106"   MOVE "5611904" TO �����ԍ��v
      *     WHEN "67138297"   MOVE "5611912" TO �����ԍ��v
      *     WHEN "67138305"   MOVE "5611920" TO �����ԍ��v
      *     WHEN "67138453"   MOVE "5611939" TO �����ԍ��v
      *     WHEN "67138602"   MOVE "5611947" TO �����ԍ��v
      *     WHEN "67145052"   MOVE "5611955" TO �����ԍ��v
      *     WHEN "110155"     MOVE "5611963" TO �����ԍ��v
      *     WHEN "110197"     MOVE "5611971" TO �����ԍ��v
      *     WHEN "110239"     MOVE "5611998" TO �����ԍ��v
      *     WHEN "110247"     MOVE "5612005" TO �����ԍ��v
      *     WHEN "110288"     MOVE "5612013" TO �����ԍ��v
      *     WHEN "110320"     MOVE "5612021" TO �����ԍ��v
      *     WHEN "110858"     MOVE "5612048" TO �����ԍ��v
      *     WHEN "110866"     MOVE "5612056" TO �����ԍ��v
      *     WHEN "110882"     MOVE "5612064" TO �����ԍ��v
      *     WHEN "110916"     MOVE "5612072" TO �����ԍ��v
      *     WHEN "67110916"   MOVE "5612072" TO �����ԍ��v
      *     WHEN "110924"     MOVE "5612080" TO �����ԍ��v
      *     WHEN "133280"     MOVE "5612099" TO �����ԍ��v
      *     WHEN "138305"     MOVE "5612102" TO �����ԍ��v
      *     WHEN "138370"     MOVE "5612110" TO �����ԍ��v
      *     WHEN "138420"     MOVE "5612129" TO �����ԍ��v
      *     WHEN "138644"     MOVE "5612137" TO �����ԍ��v
      *     WHEN "06120212"   MOVE "5612145" TO �����ԍ��v
      *     WHEN "27110030"   MOVE "5612153" TO �����ԍ��v
      *     WHEN "27110238"   MOVE "5612161" TO �����ԍ��v
      *     WHEN "27110915"   MOVE "5612188" TO �����ԍ��v
      *     WHEN "27114024"   MOVE "5612196" TO �����ԍ��v
      *     WHEN "27114032"   MOVE "5612218" TO �����ԍ��v
      *     WHEN "27114040"   MOVE "5612226" TO �����ԍ��v
      *     WHEN "27138270"   MOVE "5612234" TO �����ԍ��v
      *     WHEN "27138338"   MOVE "5612242" TO �����ԍ��v
      *     WHEN "67110262"   MOVE "5612250" TO �����ԍ��v
      *     WHEN "67110940"   MOVE "5612269" TO �����ԍ��v
      *     WHEN "67114041"   MOVE "5612277" TO �����ԍ��v
      *     WHEN "67138321"   MOVE "5612285" TO �����ԍ��v
      *     WHEN "110064"     MOVE "5612293" TO �����ԍ��v
      *     WHEN "110122"     MOVE "5612307" TO �����ԍ��v
      *     WHEN "110254"     MOVE "5612315" TO �����ԍ��v
      *     WHEN "110262"     MOVE "5612323" TO �����ԍ��v
      *     WHEN "110270"     MOVE "5612331" TO �����ԍ��v
      *     WHEN "110304"     MOVE "5612358" TO �����ԍ��v
      *     WHEN "110312"     MOVE "5612366" TO �����ԍ��v
      *     WHEN "110387"     MOVE "5612374" TO �����ԍ��v
      *     WHEN "110395"     MOVE "5612382" TO �����ԍ��v
      *     WHEN "110478"     MOVE "5612390" TO �����ԍ��v
      *     WHEN "110510"     MOVE "5612404" TO �����ԍ��v
      *     WHEN "110726"     MOVE "5612412" TO �����ԍ��v
      *     WHEN "110734"     MOVE "5612420" TO �����ԍ��v
      *     WHEN "110767"     MOVE "5612439" TO �����ԍ��v
      *     WHEN "114025"     MOVE "5612447" TO �����ԍ��v
      *     WHEN "114033"     MOVE "5612455" TO �����ԍ��v
      *     WHEN "114041"     MOVE "5612463" TO �����ԍ��v
      *     WHEN "114058"     MOVE "5612471" TO �����ԍ��v
      *     WHEN "114066"     MOVE "5612498" TO �����ԍ��v
      *     WHEN "114074"     MOVE "5612501" TO �����ԍ��v
      *     WHEN "114082"     MOVE "5612528" TO �����ԍ��v
      *     WHEN "114090"     MOVE "5612536" TO �����ԍ��v
      *     WHEN "138255"     MOVE "5612544" TO �����ԍ��v
      *     WHEN "138263"     MOVE "5612552" TO �����ԍ��v
      *     WHEN "138271"     MOVE "5612560" TO �����ԍ��v
      *     WHEN "138289"     MOVE "5612579" TO �����ԍ��v
      *     WHEN "138297"     MOVE "5612587" TO �����ԍ��v
      *     WHEN "138339"     MOVE "5612595" TO �����ԍ��v
      *     WHEN "138412"     MOVE "5612609" TO �����ԍ��v
      *     WHEN "138438"     MOVE "5612617" TO �����ԍ��v
      *     WHEN "138453"     MOVE "5612625" TO �����ԍ��v
      *     WHEN "138503"     MOVE "5612633" TO �����ԍ��v
      *     WHEN "138552"     MOVE "5612641" TO �����ԍ��v
      *     WHEN "138602"     MOVE "5612668" TO �����ԍ��v
      *     WHEN "27110154"   MOVE "5612676" TO �����ԍ��v
      *     WHEN "27110162"   MOVE "5612684" TO �����ԍ��v
      *     WHEN "27110220"   MOVE "5612692" TO �����ԍ��v
      *     WHEN "27110261"   MOVE "5612706" TO �����ԍ��v
      *     WHEN "27110279"   MOVE "5612714" TO �����ԍ��v
      *     WHEN "27110287"   MOVE "5612722" TO �����ԍ��v
      *     WHEN "27110295"   MOVE "5612730" TO �����ԍ��v
      *     WHEN "27110303"   MOVE "5612749" TO �����ԍ��v
      *     WHEN "27110329"   MOVE "5612757" TO �����ԍ��v
      *     WHEN "27110337"   MOVE "5612765" TO �����ԍ��v
      *     WHEN "67114108"   MOVE "5612773" TO �����ԍ��v
      *     WHEN "27110436"   MOVE "5612781" TO �����ԍ��v
      *     WHEN "27110477"   MOVE "5612803" TO �����ԍ��v
      *     WHEN "27110824"   MOVE "5612811" TO �����ԍ��v
      *     WHEN "27110899"   MOVE "5612838" TO �����ԍ��v
      *     WHEN "27110907"   MOVE "5612846" TO �����ԍ��v
      *     WHEN "27110923"   MOVE "5612854" TO �����ԍ��v
      *     WHEN "27110949"   MOVE "5612862" TO �����ԍ��v
      *     WHEN "27114073"   MOVE "5612870" TO �����ԍ��v
      *     WHEN "27114081"   MOVE "5612889" TO �����ԍ��v
      *     WHEN "27114099"   MOVE "5612897" TO �����ԍ��v
      *     WHEN "27138247"   MOVE "5612900" TO �����ԍ��v
      *     WHEN "27138312"   MOVE "5612919" TO �����ԍ��v
      *     WHEN "67110122"   MOVE "5612927" TO �����ԍ��v
      *     WHEN "67110130"   MOVE "5612935" TO �����ԍ��v
      *     WHEN "67110155"   MOVE "5612943" TO �����ԍ��v
      *     WHEN "67110197"   MOVE "5612951" TO �����ԍ��v
      *     WHEN "67110247"   MOVE "5612978" TO �����ԍ��v
      *     WHEN "67110270"   MOVE "5612986" TO �����ԍ��v
      *     WHEN "67110288"   MOVE "5612994" TO �����ԍ��v
      *     WHEN "67110304"   MOVE "5613001" TO �����ԍ��v
      *     WHEN "67110346"   MOVE "5613028" TO �����ԍ��v
      *     WHEN "67110353"   MOVE "5613036" TO �����ԍ��v
      *     WHEN "67110379"   MOVE "5613044" TO �����ԍ��v
      *     WHEN "67110437"   MOVE "5613052" TO �����ԍ��v
      *     WHEN "67110908"   MOVE "5613060" TO �����ԍ��v
      *     WHEN "67110924"   MOVE "5613079" TO �����ԍ��v
      *     WHEN "67114017"   MOVE "5613087" TO �����ԍ��v
      *     WHEN "67114025"   MOVE "5613095" TO �����ԍ��v
      *     WHEN "67114033"   MOVE "5613109" TO �����ԍ��v
      *     WHEN "67114058"   MOVE "5613117" TO �����ԍ��v
      *     WHEN "67114066"   MOVE "5613125" TO �����ԍ��v
      *     WHEN "67114074"   MOVE "5613133" TO �����ԍ��v
      *     WHEN "67114082"   MOVE "5613141" TO �����ԍ��v
      *     WHEN "67114090"   MOVE "5613168" TO �����ԍ��v
      *     WHEN "67138347"   MOVE "5613176" TO �����ԍ��v
      *     WHEN "67138370"   MOVE "5613184" TO �����ԍ��v
      *     WHEN "67138461"   MOVE "5613192" TO �����ԍ��v
      *     WHEN "67138511"   MOVE "5613206" TO �����ԍ��v
      *     WHEN "3102"       MOVE "5613214" TO �����ԍ��v
      *     WHEN "02110104"   MOVE "5613222" TO �����ԍ��v
      *     WHEN "110072"     MOVE "5613230" TO �����ԍ��v
      *     WHEN "110114"     MOVE "5613249" TO �����ԍ��v
      *     WHEN "110163"     MOVE "5613257" TO �����ԍ��v
      *     WHEN "110171"     MOVE "5613265" TO �����ԍ��v
      *     WHEN "67110171"   MOVE "5613265" TO �����ԍ��v
      *     WHEN "110189"     MOVE "5613273" TO �����ԍ��v
      *     WHEN "110338"     MOVE "5613281" TO �����ԍ��v
      *     WHEN "110445"     MOVE "5613303" TO �����ԍ��v
      *     WHEN "110452"     MOVE "5613311" TO �����ԍ��v
      *     WHEN "110460"     MOVE "5613338" TO �����ԍ��v
      *     WHEN "110486"     MOVE "5613346" TO �����ԍ��v
      *     WHEN "110494"     MOVE "5613354" TO �����ԍ��v
      *     WHEN "110502"     MOVE "5613362" TO �����ԍ��v
      *     WHEN "110528"     MOVE "5613370" TO �����ԍ��v
      *     WHEN "110536"     MOVE "5613389" TO �����ԍ��v
      *     WHEN "110544"     MOVE "5613397" TO �����ԍ��v
      *     WHEN "110551"     MOVE "5613400" TO �����ԍ��v
      *     WHEN "110569"     MOVE "5613419" TO �����ԍ��v
      *     WHEN "110809"     MOVE "5613427" TO �����ԍ��v
      *     WHEN "110817"     MOVE "5613435" TO �����ԍ��v
      *     WHEN "110825"     MOVE "5613443" TO �����ԍ��v
      *     WHEN "113019"     MOVE "5613451" TO �����ԍ��v
      *     WHEN "113035"     MOVE "5613478" TO �����ԍ��v
      *     WHEN "133116"     MOVE "5613486" TO �����ԍ��v
      *     WHEN "138362"     MOVE "5613494" TO �����ԍ��v
      *     WHEN "138446"     MOVE "5613508" TO �����ԍ��v
      *     WHEN "138461"     MOVE "5613516" TO �����ԍ��v
      *     WHEN "138495"     MOVE "5613524" TO �����ԍ��v
      *     WHEN "27110121"   MOVE "5613532" TO �����ԍ��v
      *     WHEN "27110196"   MOVE "5613540" TO �����ԍ��v
      *     WHEN "27110246"   MOVE "5613559" TO �����ԍ��v
      *     WHEN "27110386"   MOVE "5613567" TO �����ԍ��v
      *     WHEN "27110543"   MOVE "5613575" TO �����ԍ��v
      *     WHEN "27110550"   MOVE "5613583" TO �����ԍ��v
      *     WHEN "27110568"   MOVE "5613591" TO �����ԍ��v
      *     WHEN "27138262"   MOVE "5613605" TO �����ԍ��v
      *     WHEN "27138429"   MOVE "5613613" TO �����ԍ��v
      *     WHEN "67110015"   MOVE "5613621" TO �����ԍ��v
      *     WHEN "67110080"   MOVE "5613648" TO �����ԍ��v
      *     WHEN "67110296"   MOVE "5613656" TO �����ԍ��v
      *     WHEN "67110494"   MOVE "5613664" TO �����ԍ��v
      *     WHEN "67110544"   MOVE "5613672" TO �����ԍ��v
      *     WHEN "67110569"   MOVE "5613680" TO �����ԍ��v
      *     WHEN "67110767"   MOVE "5613699" TO �����ԍ��v
      *     WHEN "67110858"   MOVE "5613702" TO �����ԍ��v
      *     WHEN "67138271"   MOVE "5613710" TO �����ԍ��v
      *     WHEN "67138396"   MOVE "5613729" TO �����ԍ��v
      *     WHEN "67138420"   MOVE "5613737" TO �����ԍ��v
      *     WHEN "06139224"   MOVE "5613745" TO �����ԍ��v
      *     WHEN "06139240"   MOVE "5613753" TO �����ԍ��v
      *     WHEN "06139257"   MOVE "5613761" TO �����ԍ��v
      *     WHEN "06139273"   MOVE "5613788" TO �����ԍ��v
      *     WHEN "31130552"   MOVE "5613796" TO �����ԍ��v
      *     WHEN "31110364"   MOVE "5613818" TO �����ԍ��v
      *     WHEN "27110170"   MOVE "5613826" TO �����ԍ��v
      *     WHEN "27110212"   MOVE "5613834" TO �����ԍ��v
      *     WHEN "27110410"   MOVE "5613842" TO �����ԍ��v
      *     WHEN "27110444"   MOVE "5613850" TO �����ԍ��v
      *     WHEN "27110790"   MOVE "5613869" TO �����ԍ��v
      *     WHEN "27110857"   MOVE "5613877" TO �����ԍ��v
      *     WHEN "27138254"   MOVE "5613885" TO �����ԍ��v
      *     WHEN "27138296"   MOVE "5613893" TO �����ԍ��v
      *     WHEN "27138379"   MOVE "5613907" TO �����ԍ��v
      *     WHEN "27138387"   MOVE "5613915" TO �����ԍ��v
      *     WHEN "27138437"   MOVE "5613923" TO �����ԍ��v
      *     WHEN "03110103"   MOVE "5613931" TO �����ԍ��v
      *     WHEN "04110102"   MOVE "5613931" TO �����ԍ��v
      *     WHEN "1101"       MOVE "5613931" TO �����ԍ��v
      *     WHEN "03110202"   MOVE "5613958" TO �����ԍ��v
      *     WHEN "04110201"   MOVE "5613958" TO �����ԍ��v
      *     WHEN "1102"       MOVE "5613958" TO �����ԍ��v
      *     WHEN "03110400"   MOVE "5613966" TO �����ԍ��v
      *     WHEN "04110409"   MOVE "5613966" TO �����ԍ��v
      *     WHEN "1104"       MOVE "5613966" TO �����ԍ��v
      *     WHEN "03110509"   MOVE "5613974" TO �����ԍ��v
      *     WHEN "04110508"   MOVE "5613974" TO �����ԍ��v
      *     WHEN "1105"       MOVE "5613974" TO �����ԍ��v
      *     WHEN "03110806"   MOVE "5613982" TO �����ԍ��v
      *     WHEN "04110805"   MOVE "5613982" TO �����ԍ��v
      *     WHEN "1108"       MOVE "5613982" TO �����ԍ��v
      *     WHEN "03113107"   MOVE "5613990" TO �����ԍ��v
      *     WHEN "04113106"   MOVE "5613990" TO �����ԍ��v
      *     WHEN "1131"       MOVE "5613990" TO �����ԍ��v
      *     WHEN "03113305"   MOVE "5614008" TO �����ԍ��v
      *     WHEN "04113304"   MOVE "5614008" TO �����ԍ��v
      *     WHEN "1133"       MOVE "5614008" TO �����ԍ��v
      *     WHEN "03120102"   MOVE "5614016" TO �����ԍ��v
      *     WHEN "04120101"   MOVE "5614016" TO �����ԍ��v
      *     WHEN "1201"       MOVE "5614016" TO �����ԍ��v
      *     WHEN "03120409"   MOVE "5614024" TO �����ԍ��v
      *     WHEN "04120408"   MOVE "5614024" TO �����ԍ��v
      *     WHEN "1204"       MOVE "5614024" TO �����ԍ��v
      *     WHEN "03120607"   MOVE "5614032" TO �����ԍ��v
      *     WHEN "04120606"   MOVE "5614032" TO �����ԍ��v
      *     WHEN "1206"       MOVE "5614032" TO �����ԍ��v
      *     WHEN "03120706"   MOVE "5614040" TO �����ԍ��v
      *     WHEN "04120705"   MOVE "5614040" TO �����ԍ��v
      *     WHEN "1207"       MOVE "5614040" TO �����ԍ��v
      *     WHEN "03120904"   MOVE "5614059" TO �����ԍ��v
      *     WHEN "04120903"   MOVE "5614059" TO �����ԍ��v
      *     WHEN "1209"       MOVE "5614059" TO �����ԍ��v
      *     WHEN "03124708"   MOVE "5614067" TO �����ԍ��v
      *     WHEN "04124707"   MOVE "5614067" TO �����ԍ��v
      *     WHEN "1247"       MOVE "5614067" TO �����ԍ��v
      *     WHEN "03130101"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03130507"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03131109"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03131505"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03132107"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03132503"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03132602"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03133105"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03133204"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03133501"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03133600"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03134103"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03134202"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03134509"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03134608"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03135100"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03135209"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03135506"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03135605"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03135704"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03136108"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03136207"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03136306"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03136504"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03137106"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03137502"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03137601"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03137700"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03138906"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "03139805"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "04130100"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "04130506"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "04131108"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "04131504"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "04132106"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "04132502"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "04135505"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "04135703"   MOVE "5614075" TO �����ԍ��v
      *     WHEN "2101"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2105"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2111"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2115"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2121"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2125"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2126"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2131"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2132"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2135"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2136"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2141"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2142"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2145"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2146"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2151"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2152"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2155"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2156"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2157"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2161"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2162"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2163"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2165"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2171"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2175"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2176"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2177"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2189"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "2198"       MOVE "5614075" TO �����ԍ��v
      *     WHEN "39011002"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39011010"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39011028"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39011036"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39011044"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39011051"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39011069"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39011077"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39011085"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39011093"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39011101"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012026"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012034"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012042"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012059"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012067"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012075"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012083"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012091"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012109"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012117"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012125"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012133"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012141"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012158"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012166"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012174"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012182"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012190"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012208"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012216"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012224"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012232"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012240"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012257"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012265"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012273"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012281"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012299"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012307"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012315"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012331"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012349"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012356"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39012364"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013032"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013040"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013313"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013321"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013339"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013347"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013370"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013438"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013453"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013461"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013479"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013610"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013628"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013636"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013644"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013677"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013701"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013719"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013917"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013925"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013933"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013941"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013958"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013966"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013974"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013982"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39013990"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014006"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014014"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014022"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014030"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014048"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014055"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014063"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014071"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014089"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014097"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014238"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014246"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014253"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014279"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014287"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014295"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014303"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014311"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014329"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014337"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014345"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014360"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014378"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014386"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014394"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014527"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014535"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014543"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014550"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014568"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014576"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014584"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014592"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014600"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014618"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014626"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014634"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014642"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014659"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014683"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014691"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014709"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014717"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014816"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014824"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014832"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014840"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014857"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014865"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014873"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39014881"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015110"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015128"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015136"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015144"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015169"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015177"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015185"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015193"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015433"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015441"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015458"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015466"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015474"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015490"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015508"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015524"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015557"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015581"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015599"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015607"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015615"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015623"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015631"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015649"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015714"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015755"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015789"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015813"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015847"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015854"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39015862"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016019"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016027"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016043"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016076"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016084"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016092"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016100"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016316"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016324"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016332"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016340"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016357"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016365"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016373"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016381"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016399"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016415"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016423"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016431"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016449"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016456"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016464"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016472"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016480"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016498"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016613"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016621"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016639"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016647"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016654"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016670"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016688"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016910"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016928"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016936"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39016944"   MOVE "5614083" TO �����ԍ��v
      *     WHEN "39041017"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39041025"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39041033"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39041041"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39041058"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39042023"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39042031"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39042056"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39042064"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39042072"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39042080"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39042098"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39042114"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39042122"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39042130"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39042148"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39042155"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39043013"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39043021"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39043211"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39043229"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39043237"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39043245"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39043419"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39043617"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39043625"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39044011"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39044045"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39044060"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39044219"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39044227"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39044235"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39044243"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39044441"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39044458"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39045018"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39045059"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39045810"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39046032"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39046065"   MOVE "5614091" TO �����ԍ��v
      *     WHEN "39062013"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39062021"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39062039"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39062047"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39062054"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39062062"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39062070"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39062088"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39062096"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39062104"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39062112"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39062120"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39062138"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063011"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063029"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063219"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063227"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063235"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063243"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063417"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063615"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063623"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063631"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063649"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063656"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063664"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063672"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063813"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39063821"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39064019"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39064027"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39064035"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39064266"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39064282"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39064613"   MOVE "5614105" TO �����ԍ��v
      *     WHEN "39072012"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39072020"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39072038"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39072046"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39072053"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39072079"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39072087"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39072095"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39072103"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39072111"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39072129"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39072137"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39072145"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39073010"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39073036"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39073085"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39073093"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39073226"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39073424"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39073440"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39073622"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39073648"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39073671"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39073689"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074026"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074059"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074075"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074083"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074216"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074224"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074232"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074448"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074455"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074463"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074471"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074612"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074646"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074653"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074661"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074810"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074828"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074836"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39074844"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075015"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075023"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075031"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075049"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075056"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075213"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075221"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075411"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075429"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075437"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075445"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075452"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075460"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075478"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075486"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075619"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39075643"   MOVE "5614113" TO �����ԍ��v
      *     WHEN "39082011"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082029"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082037"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082045"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082052"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082078"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082086"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082102"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082110"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082128"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082144"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082151"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082169"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082177"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082193"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082201"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082219"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082227"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082235"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082243"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082250"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082268"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082276"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082284"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082292"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082300"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082318"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082326"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082334"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082342"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082359"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39082367"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39083027"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39083092"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39083100"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39083415"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39083647"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39084421"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39084439"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39084470"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39085212"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39085428"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39085469"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39085642"   MOVE "5614121" TO �����ԍ��v
      *     WHEN "39092010"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092028"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092036"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092044"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092051"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092069"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092085"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092093"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092101"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092119"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092135"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092143"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092150"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39092168"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093018"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093216"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093414"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093422"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093430"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093448"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093455"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093612"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093646"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093653"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093661"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093679"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093687"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093844"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39093869"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39094073"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39094115"   MOVE "5614148" TO �����ԍ��v
      *     WHEN "39102017"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39102025"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39102033"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39102041"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39102058"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39102066"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39102074"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39102082"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39102090"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39102108"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39102116"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39102124"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39103031"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39103445"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39103452"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39103635"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39103668"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39103676"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39103825"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39103833"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39103841"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39104211"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39104245"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39104252"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39104260"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39104278"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39104286"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39104294"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39104435"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39104443"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39104484"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39104492"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39104641"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39105218"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39105226"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39105234"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39105242"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39105259"   MOVE "5614156" TO �����ԍ��v
      *     WHEN "39121017"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39121025"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39121033"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39121041"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39121058"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39121066"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122023"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122031"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122049"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122056"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122064"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122072"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122080"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122106"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122114"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122122"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122130"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122155"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122163"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122171"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122189"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122197"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122205"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122213"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122221"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122239"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122247"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122254"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122262"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122270"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122288"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122296"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122304"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122312"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122320"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122338"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122346"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122353"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122361"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122379"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39122387"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39123229"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39123252"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39123286"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39123294"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39123427"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39123476"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39123492"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124029"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124037"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124094"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124102"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124219"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124227"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124235"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124243"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124268"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124276"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124417"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124433"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39124631"   MOVE "5614164" TO �����ԍ��v
      *     WHEN "39141015"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141023"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141031"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141049"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141056"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141064"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141072"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141080"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141098"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141106"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141114"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141122"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141130"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141148"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141155"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141163"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141171"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141189"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141312"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141320"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141338"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141346"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141353"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141361"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39141379"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142013"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142039"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142047"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142054"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142062"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142070"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142088"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142096"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142104"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142112"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142120"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142138"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142146"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142153"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142161"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142179"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39142187"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39143011"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39143219"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39143417"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39143425"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39143615"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39143623"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39143631"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39143649"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39143664"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39143821"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39143839"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39143847"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39144019"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39144027"   MOVE "5614172" TO �����ԍ��v
      *     WHEN "39192018"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39192026"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39192042"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39192059"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39192067"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39192075"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39192083"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39192091"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39192109"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39192117"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39192125"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39192133"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39192141"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39193461"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39193610"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39193628"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39193644"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39193651"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39193669"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39193842"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39194220"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39194238"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39194246"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39194253"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39194295"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39194303"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39194428"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39194436"   MOVE "5614180" TO �����ԍ��v
      *     WHEN "39202015"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202023"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202031"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202049"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202056"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202064"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202072"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202080"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202098"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202106"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202114"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202122"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202130"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202148"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202155"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202171"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202189"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202197"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39202205"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203039"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203047"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203054"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203062"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203070"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203096"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203211"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203237"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203245"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203492"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203500"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203617"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203625"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203633"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203823"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203831"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203849"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203856"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203864"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39203880"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204029"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204037"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204045"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204060"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204078"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204094"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204102"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204110"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204128"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204136"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204144"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204151"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204169"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204177"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204227"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204235"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204250"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204292"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204300"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204326"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204466"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204482"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204490"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204508"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204516"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204524"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204813"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204821"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204854"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39204862"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39205216"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39205414"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39205430"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39205612"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39205620"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39205638"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39205810"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39205836"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39205885"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39205893"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39205901"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39206024"   MOVE "5614199" TO �����ԍ��v
      *     WHEN "39212014"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212022"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212030"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212048"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212055"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212063"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212071"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212089"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212097"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212105"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212113"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212121"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212139"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212147"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212154"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212162"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212170"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212188"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212196"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212204"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39212212"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39213020"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39213038"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39213418"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39213616"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39213624"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39213814"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39213822"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39213830"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39214010"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39214036"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39214044"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39214218"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39215017"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39215025"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39215033"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39215041"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39215058"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39215066"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39215074"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39215215"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39216049"   MOVE "5614202" TO �����ԍ��v
      *     WHEN "39231014"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231022"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231030"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231048"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231055"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231063"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231071"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231089"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231097"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231105"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231113"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231121"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231139"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231147"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231154"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39231162"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232012"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232020"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232038"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232046"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232053"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232061"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232079"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232087"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232095"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232103"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232111"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232129"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232137"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232145"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232152"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232160"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232178"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232194"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232202"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232210"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232228"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232236"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232244"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232251"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232269"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232277"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232285"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232293"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232301"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232319"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232327"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232335"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232343"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39232350"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39233028"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39233044"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39233424"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39233457"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39233614"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39233622"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234216"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234224"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234232"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234240"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234257"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234273"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234414"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234422"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234455"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234463"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234471"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234810"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234828"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39234836"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39235015"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39235213"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39235619"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39235627"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39235635"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39236039"   MOVE "5614210" TO �����ԍ��v
      *     WHEN "39271028"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271036"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271044"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271069"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271077"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271085"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271093"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271119"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271135"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271143"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271150"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271168"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271176"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271184"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271192"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271200"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271218"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271226"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271234"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271242"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271259"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271267"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271275"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271283"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271416"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271424"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271432"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271440"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271457"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271465"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39271473"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272026"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272034"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272042"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272059"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272067"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272075"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272083"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272091"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272109"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272117"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272125"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272133"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272141"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272158"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272166"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272174"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272182"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272190"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272208"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272216"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272224"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272232"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272240"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272257"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272265"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272273"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272281"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272299"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272307"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272315"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39272323"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39273016"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39273214"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39273222"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39273412"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39273610"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39273628"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39273669"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39273818"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39273826"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39273834"   MOVE "5614229" TO �����ԍ��v
      *     WHEN "39401013"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401039"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401054"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401062"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401070"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401088"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401096"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401310"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401328"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401336"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401344"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401351"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401369"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39401377"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402029"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402037"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402045"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402052"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402060"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402078"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402102"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402110"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402128"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402136"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402144"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402151"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402169"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402177"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402185"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402193"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402201"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402219"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402227"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402235"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402243"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402250"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402268"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402276"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402284"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39402292"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39403050"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39403415"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39403423"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39403431"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39403449"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39403456"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39403480"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39403498"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39403811"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39403829"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39403837"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39403845"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39404017"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39404025"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39404215"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39404470"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39404488"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39404629"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39404637"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39405030"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39405220"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39405410"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39405436"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39405444"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39405451"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39405469"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39406012"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39406020"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39406046"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39406053"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39406087"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39406095"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39406103"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39406210"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39406251"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39406426"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39406467"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39406475"   MOVE "5614237" TO �����ԍ��v
      *     WHEN "39422019"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39422027"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39422035"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39422043"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39422050"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39422076"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39422084"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39422092"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39422100"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39422118"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39422126"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39422134"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39422142"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39423074"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39423082"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39423215"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39423223"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39423231"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39423835"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39423884"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39423892"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39423918"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39424114"   MOVE "5614245" TO �����ԍ��v
      *     WHEN "39432018"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432026"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432034"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432042"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432059"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432067"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432083"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432109"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432117"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432125"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432133"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432141"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432158"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39432166"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39433412"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39433420"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39433487"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39433644"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39433677"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39433685"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39433693"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39433859"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434030"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434048"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434238"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434246"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434253"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434287"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434329"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434337"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434410"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434428"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434436"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434444"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434477"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434683"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434824"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39434840"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39435011"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39435052"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39435060"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39435078"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39435102"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39435110"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39435128"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39435136"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39435144"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39435318"   MOVE "5614253" TO �����ԍ��v
      *     WHEN "39442017"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442025"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442033"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442041"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442058"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442066"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442074"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442082"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442090"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442108"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442116"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442124"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442132"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39442140"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39443221"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39443411"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39444617"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39444625"   MOVE "5614261" TO �����ԍ��v
      *     WHEN "39452016"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39452024"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39452032"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39452040"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39452057"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39452065"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39452073"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39452081"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39452099"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39453014"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39453212"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39453220"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39453410"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39453618"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39453626"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39453824"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39453832"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454012"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454020"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454038"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454046"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454053"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454061"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454210"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454293"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454301"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454319"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454418"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454426"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39454434"   MOVE "5614288" TO �����ԍ��v
      *     WHEN "39462015"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462031"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462049"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462064"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462080"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462098"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462106"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462130"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462148"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462155"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462163"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462171"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462189"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462197"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462205"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462213"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462221"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39462239"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39463039"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39463047"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39463922"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39464045"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39464219"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39464417"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39464425"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39464433"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39464524"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39464680"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39464821"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39464904"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39464912"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39464920"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465018"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465026"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465059"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465232"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465240"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465257"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465273"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465299"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465307"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465315"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465323"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465331"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465349"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39465356"   MOVE "5614296" TO �����ԍ��v
      *     WHEN "39111018"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39111026"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39111034"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39111042"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39111059"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39111067"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39111075"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39111083"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39111091"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39111109"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112016"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112024"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112032"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112065"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112073"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112081"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112099"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112107"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112115"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112123"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112149"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112156"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112164"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112172"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112180"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112198"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112214"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112222"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112230"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112248"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112255"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112263"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112271"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112289"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112297"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112305"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112313"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112321"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112339"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112347"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112354"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112370"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112388"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112396"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112404"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112412"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112420"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112438"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39112453"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113014"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113246"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113261"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113279"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113410"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113428"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113436"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113469"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113477"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113485"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113493"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113618"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113626"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113634"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113659"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113691"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113816"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113832"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39113857"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39114087"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39114210"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39114244"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39114251"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39114426"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39114459"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39114467"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39114616"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39114624"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39114640"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "39114657"   MOVE "5614318" TO �����ԍ��v
      *     WHEN "01010016"   MOVE "5614326" TO �����ԍ��v
      *     WHEN "03010014"   MOVE "5614326" TO �����ԍ��v
      *     WHEN "04010013"   MOVE "5614326" TO �����ԍ��v
      *     WHEN "01020015"   MOVE "5614334" TO �����ԍ��v
      *     WHEN "03020013"   MOVE "5614334" TO �����ԍ��v
      *     WHEN "04020012"   MOVE "5614334" TO �����ԍ��v
      *     WHEN "01030014"   MOVE "5614342" TO �����ԍ��v
      *     WHEN "03030012"   MOVE "5614342" TO �����ԍ��v
      *     WHEN "04030011"   MOVE "5614342" TO �����ԍ��v
      *     WHEN "01040013"   MOVE "5614350" TO �����ԍ��v
      *     WHEN "03040011"   MOVE "5614350" TO �����ԍ��v
      *     WHEN "04040010"   MOVE "5614350" TO �����ԍ��v
      *     WHEN "01050012"   MOVE "5614369" TO �����ԍ��v
      *     WHEN "03050010"   MOVE "5614369" TO �����ԍ��v
      *     WHEN "04050019"   MOVE "5614369" TO �����ԍ��v
      *     WHEN "01060011"   MOVE "5614377" TO �����ԍ��v
      *     WHEN "03060019"   MOVE "5614377" TO �����ԍ��v
      *     WHEN "04060018"   MOVE "5614377" TO �����ԍ��v
      *     WHEN "01070010"   MOVE "5614385" TO �����ԍ��v
      *     WHEN "03070018"   MOVE "5614385" TO �����ԍ��v
      *     WHEN "04070017"   MOVE "5614385" TO �����ԍ��v
      *     WHEN "01080019"   MOVE "5614393" TO �����ԍ��v
      *     WHEN "03080017"   MOVE "5614393" TO �����ԍ��v
      *     WHEN "04080016"   MOVE "5614393" TO �����ԍ��v
      *     WHEN "03140100"   MOVE "5614407" TO �����ԍ��v
      *     WHEN "04140109"   MOVE "5614407" TO �����ԍ��v
      *     WHEN "3101"       MOVE "5614407" TO �����ԍ��v
      *     WHEN "03140308"   MOVE "5614415" TO �����ԍ��v
      *     WHEN "04140307"   MOVE "5614415" TO �����ԍ��v
      *     WHEN "3103"       MOVE "5614415" TO �����ԍ��v
      *     WHEN "03141108"   MOVE "5614423" TO �����ԍ��v
      *     WHEN "04141107"   MOVE "5614423" TO �����ԍ��v
      *     WHEN "3111"       MOVE "5614423" TO �����ԍ��v
      *     WHEN "03141405"   MOVE "5614431" TO �����ԍ��v
      *     WHEN "04141404"   MOVE "5614431" TO �����ԍ��v
      *     WHEN "3114"       MOVE "5614431" TO �����ԍ��v
      *     WHEN "03142106"   MOVE "5614458" TO �����ԍ��v
      *     WHEN "04142105"   MOVE "5614458" TO �����ԍ��v
      *     WHEN "3121"       MOVE "5614458" TO �����ԍ��v
      *     WHEN "03142304"   MOVE "5614466" TO �����ԍ��v
      *     WHEN "04142303"   MOVE "5614466" TO �����ԍ��v
      *     WHEN "3123"       MOVE "5614466" TO �����ԍ��v
      *     WHEN "03143104"   MOVE "5614474" TO �����ԍ��v
      *     WHEN "04143103"   MOVE "5614474" TO �����ԍ��v
      *     WHEN "3131"       MOVE "5614474" TO �����ԍ��v
      *     WHEN "03143500"   MOVE "5614482" TO �����ԍ��v
      *     WHEN "04143509"   MOVE "5614482" TO �����ԍ��v
      *     WHEN "3135"       MOVE "5614482" TO �����ԍ��v
      *     WHEN "03145109"   MOVE "5614490" TO �����ԍ��v
      *     WHEN "04145108"   MOVE "5614490" TO �����ԍ��v
      *     WHEN "3151"       MOVE "5614490" TO �����ԍ��v
      *     WHEN "3152"       MOVE "5614504" TO �����ԍ��v
      *     WHEN "03145307"   MOVE "5614512" TO �����ԍ��v
      *     WHEN "04145306"   MOVE "5614512" TO �����ԍ��v
      *     WHEN "3153"       MOVE "5614512" TO �����ԍ��v
      *     WHEN "03145505"   MOVE "5614520" TO �����ԍ��v
      *     WHEN "04145504"   MOVE "5614520" TO �����ԍ��v
      *     WHEN "3155"       MOVE "5614520" TO �����ԍ��v
      *     WHEN "03145703"   MOVE "5614539" TO �����ԍ��v
      *     WHEN "04145702"   MOVE "5614539" TO �����ԍ��v
      *     WHEN "3157"       MOVE "5614539" TO �����ԍ��v
      *     WHEN "27114107"   MOVE "5614547" TO �����ԍ��v
      *     WHEN "27114016"   MOVE "5614555" TO �����ԍ��v
      *     WHEN "01090018"   MOVE "5614563" TO �����ԍ��v
      *     WHEN "03090016"   MOVE "5614563" TO �����ԍ��v
      *     WHEN "04090015"   MOVE "5614563" TO �����ԍ��v
      *     WHEN "01100015"   MOVE "5614571" TO �����ԍ��v
      *     WHEN "03100013"   MOVE "5614571" TO �����ԍ��v
      *     WHEN "04100012"   MOVE "5614571" TO �����ԍ��v
      *     WHEN "01110014"   MOVE "5614598" TO �����ԍ��v
      *     WHEN "03110012"   MOVE "5614598" TO �����ԍ��v
      *     WHEN "04110011"   MOVE "5614598" TO �����ԍ��v
      *     WHEN "01120013"   MOVE "5614601" TO �����ԍ��v
      *     WHEN "03120011"   MOVE "5614601" TO �����ԍ��v
      *     WHEN "04120010"   MOVE "5614601" TO �����ԍ��v
      *     WHEN "01130012"   MOVE "5614628" TO �����ԍ��v
      *     WHEN "03130010"   MOVE "5614628" TO �����ԍ��v
      *     WHEN "04130019"   MOVE "5614628" TO �����ԍ��v
      *     WHEN "41140138"   MOVE "5614636" TO �����ԍ��v
      *     WHEN "06130058"   MOVE "5614644" TO �����ԍ��v
      *     WHEN "06130066"   MOVE "5614652" TO �����ԍ��v
      *     WHEN "06130074"   MOVE "5614660" TO �����ԍ��v
      *     WHEN "06130082"   MOVE "5614679" TO �����ԍ��v
      *     WHEN "06130090"   MOVE "5614687" TO �����ԍ��v
      *     WHEN "06130108"   MOVE "5614695" TO �����ԍ��v
      *     WHEN "63130108"   MOVE "5614695" TO �����ԍ��v
      *     WHEN "06130116"   MOVE "5614709" TO �����ԍ��v
      *     WHEN "06130124"   MOVE "5614717" TO �����ԍ��v
      *     WHEN "63130124"   MOVE "5614717" TO �����ԍ��v
      *     WHEN "06130132"   MOVE "5614725" TO �����ԍ��v
      *     WHEN "06130181"   MOVE "5614733" TO �����ԍ��v
      *     WHEN "06130199"   MOVE "5614741" TO �����ԍ��v
      *     WHEN "06130231"   MOVE "5614768" TO �����ԍ��v
      *     WHEN "06130298"   MOVE "5614776" TO �����ԍ��v
      *     WHEN "63130298"   MOVE "5614776" TO �����ԍ��v
      *     WHEN "06130306"   MOVE "5614784" TO �����ԍ��v
      *     WHEN "06130389"   MOVE "5614792" TO �����ԍ��v
      *     WHEN "63130389"   MOVE "5614792" TO �����ԍ��v
      *     WHEN "06130405"   MOVE "5614806" TO �����ԍ��v
      *     WHEN "06130439"   MOVE "5614814" TO �����ԍ��v
      *     WHEN "06130447"   MOVE "5614822" TO �����ԍ��v
      *     WHEN "06130454"   MOVE "5614830" TO �����ԍ��v
      *     WHEN "06130488"   MOVE "5614849" TO �����ԍ��v
      *     WHEN "06130553"   MOVE "5614857" TO �����ԍ��v
      *     WHEN "06130587"   MOVE "5614865" TO �����ԍ��v
      *     WHEN "06130637"   MOVE "5614873" TO �����ԍ��v
      *     WHEN "06130645"   MOVE "5614881" TO �����ԍ��v
      *     WHEN "06130660"   MOVE "5614903" TO �����ԍ��v
      *     WHEN "63130660"   MOVE "5614903" TO �����ԍ��v
      *     WHEN "06130686"   MOVE "5614911" TO �����ԍ��v
      *     WHEN "06130702"   MOVE "5614938" TO �����ԍ��v
      *     WHEN "63130702"   MOVE "5614938" TO �����ԍ��v
      *     WHEN "06130710"   MOVE "5614946" TO �����ԍ��v
      *     WHEN "06130728"   MOVE "5614954" TO �����ԍ��v
      *     WHEN "06130736"   MOVE "5614962" TO �����ԍ��v
      *     WHEN "06130769"   MOVE "5614970" TO �����ԍ��v
      *     WHEN "63130769"   MOVE "5614970" TO �����ԍ��v
      *     WHEN "06130777"   MOVE "5614989" TO �����ԍ��v
      *     WHEN "63130777"   MOVE "5614989" TO �����ԍ��v
      *     WHEN "06130785"   MOVE "5614997" TO �����ԍ��v
      *     WHEN "06130835"   MOVE "5615004" TO �����ԍ��v
      *     WHEN "06130843"   MOVE "5615012" TO �����ԍ��v
      *     WHEN "06130868"   MOVE "5615020" TO �����ԍ��v
      *     WHEN "06130892"   MOVE "5615039" TO �����ԍ��v
      *     WHEN "63130892"   MOVE "5615039" TO �����ԍ��v
      *     WHEN "06130900"   MOVE "5615047" TO �����ԍ��v
      *     WHEN "06130926"   MOVE "5615055" TO �����ԍ��v
      *     WHEN "06130934"   MOVE "5615063" TO �����ԍ��v
      *     WHEN "06130975"   MOVE "5615071" TO �����ԍ��v
      *     WHEN "06131064"   MOVE "5615098" TO �����ԍ��v
      *     WHEN "06131114"   MOVE "5615101" TO �����ԍ��v
      *     WHEN "06131163"   MOVE "5615128" TO �����ԍ��v
      *     WHEN "06131189"   MOVE "5615136" TO �����ԍ��v
      *     WHEN "06131213"   MOVE "5615144" TO �����ԍ��v
      *     WHEN "06131288"   MOVE "5615152" TO �����ԍ��v
      *     WHEN "06131296"   MOVE "5615160" TO �����ԍ��v
      *     WHEN "06131338"   MOVE "5615179" TO �����ԍ��v
      *     WHEN "06131346"   MOVE "5615187" TO �����ԍ��v
      *     WHEN "06131379"   MOVE "5615195" TO �����ԍ��v
      *     WHEN "06131429"   MOVE "5615209" TO �����ԍ��v
      *     WHEN "06131452"   MOVE "5615217" TO �����ԍ��v
      *     WHEN "06131460"   MOVE "5615225" TO �����ԍ��v
      *     WHEN "06131551"   MOVE "5615233" TO �����ԍ��v
      *     WHEN "63131551"   MOVE "5615233" TO �����ԍ��v
      *     WHEN "06131569"   MOVE "5615241" TO �����ԍ��v
      *     WHEN "06131577"   MOVE "5615268" TO �����ԍ��v
      *     WHEN "06131585"   MOVE "5615276" TO �����ԍ��v
      *     WHEN "06131635"   MOVE "5615284" TO �����ԍ��v
      *     WHEN "06131668"   MOVE "5615292" TO �����ԍ��v
      *     WHEN "63131668"   MOVE "5615292" TO �����ԍ��v
      *     WHEN "06131676"   MOVE "5615306" TO �����ԍ��v
      *     WHEN "06131742"   MOVE "5615314" TO �����ԍ��v
      *     WHEN "06131783"   MOVE "5615322" TO �����ԍ��v
      *     WHEN "06131791"   MOVE "5615330" TO �����ԍ��v
      *     WHEN "06131817"   MOVE "5615349" TO �����ԍ��v
      *     WHEN "06131841"   MOVE "5615357" TO �����ԍ��v
      *     WHEN "06131882"   MOVE "5615365" TO �����ԍ��v
      *     WHEN "06131924"   MOVE "5615373" TO �����ԍ��v
      *     WHEN "06131932"   MOVE "5615381" TO �����ԍ��v
      *     WHEN "06131999"   MOVE "5615403" TO �����ԍ��v
      *     WHEN "63131999"   MOVE "5615403" TO �����ԍ��v
      *     WHEN "06132013"   MOVE "5615411" TO �����ԍ��v
      *     WHEN "06132039"   MOVE "5615438" TO �����ԍ��v
      *     WHEN "06132054"   MOVE "5615446" TO �����ԍ��v
      *     WHEN "06132088"   MOVE "5615454" TO �����ԍ��v
      *     WHEN "63132088"   MOVE "5615454" TO �����ԍ��v
      *     WHEN "06132112"   MOVE "5615462" TO �����ԍ��v
      *     WHEN "63132112"   MOVE "5615462" TO �����ԍ��v
      *     WHEN "06132120"   MOVE "5615470" TO �����ԍ��v
      *     WHEN "06132146"   MOVE "5615489" TO �����ԍ��v
      *     WHEN "63132146"   MOVE "5615489" TO �����ԍ��v
      *     WHEN "06132161"   MOVE "5615497" TO �����ԍ��v
      *     WHEN "06132179"   MOVE "5615500" TO �����ԍ��v
      *     WHEN "06132211"   MOVE "5615519" TO �����ԍ��v
      *     WHEN "06132229"   MOVE "5615527" TO �����ԍ��v
      *     WHEN "06132260"   MOVE "5615535" TO �����ԍ��v
      *     WHEN "63132260"   MOVE "5615535" TO �����ԍ��v
      *     WHEN "06132294"   MOVE "5615543" TO �����ԍ��v
      *     WHEN "06132302"   MOVE "5615551" TO �����ԍ��v
      *     WHEN "06132310"   MOVE "5615578" TO �����ԍ��v
      *     WHEN "06132328"   MOVE "5615586" TO �����ԍ��v
      *     WHEN "06132336"   MOVE "5615594" TO �����ԍ��v
      *     WHEN "06132344"   MOVE "5615608" TO �����ԍ��v
      *     WHEN "06132369"   MOVE "5615616" TO �����ԍ��v
      *     WHEN "06132377"   MOVE "5615624" TO �����ԍ��v
      *     WHEN "06132393"   MOVE "5615632" TO �����ԍ��v
      *     WHEN "06132419"   MOVE "5615640" TO �����ԍ��v
      *     WHEN "06132427"   MOVE "5615659" TO �����ԍ��v
      *     WHEN "06132443"   MOVE "5615667" TO �����ԍ��v
      *     WHEN "06132468"   MOVE "5615675" TO �����ԍ��v
      *     WHEN "06137673"   MOVE "5615675" TO �����ԍ��v
      *     WHEN "06137806"   MOVE "5615675" TO �����ԍ��v
      *     WHEN "06138671"   MOVE "5615675" TO �����ԍ��v
      *     WHEN "06132476"   MOVE "5615683" TO �����ԍ��v
      *     WHEN "06132484"   MOVE "5615691" TO �����ԍ��v
      *     WHEN "06132500"   MOVE "5615705" TO �����ԍ��v
      *     WHEN "06132518"   MOVE "5615713" TO �����ԍ��v
      *     WHEN "06132559"   MOVE "5615721" TO �����ԍ��v
      *     WHEN "06132567"   MOVE "5615748" TO �����ԍ��v
      *     WHEN "06132583"   MOVE "5615756" TO �����ԍ��v
      *     WHEN "63132583"   MOVE "5615756" TO �����ԍ��v
      *     WHEN "06132658"   MOVE "5615764" TO �����ԍ��v
      *     WHEN "06132682"   MOVE "5615772" TO �����ԍ��v
      *     WHEN "06132690"   MOVE "5615780" TO �����ԍ��v
      *     WHEN "06132765"   MOVE "5615799" TO �����ԍ��v
      *     WHEN "63132765"   MOVE "5615799" TO �����ԍ��v
      *     WHEN "06132773"   MOVE "5615802" TO �����ԍ��v
      *     WHEN "63132773"   MOVE "5615802" TO �����ԍ��v
      *     WHEN "06132781"   MOVE "5615810" TO �����ԍ��v
      *     WHEN "06132799"   MOVE "5615829" TO �����ԍ��v
      *     WHEN "06132807"   MOVE "5615837" TO �����ԍ��v
      *     WHEN "06132831"   MOVE "5615845" TO �����ԍ��v
      *     WHEN "06132849"   MOVE "5615853" TO �����ԍ��v
      *     WHEN "06132856"   MOVE "5615861" TO �����ԍ��v
      *     WHEN "06132864"   MOVE "5615888" TO �����ԍ��v
      *     WHEN "06132922"   MOVE "5615896" TO �����ԍ��v
      *     WHEN "63132922"   MOVE "5615896" TO �����ԍ��v
      *     WHEN "06132930"   MOVE "5615918" TO �����ԍ��v
      *     WHEN "06132948"   MOVE "5615926" TO �����ԍ��v
      *     WHEN "63132948"   MOVE "5615926" TO �����ԍ��v
      *     WHEN "06132963"   MOVE "5615934" TO �����ԍ��v
      *     WHEN "06132971"   MOVE "5615942" TO �����ԍ��v
      *     WHEN "63132971"   MOVE "5615942" TO �����ԍ��v
      *     WHEN "06133029"   MOVE "5615950" TO �����ԍ��v
      *     WHEN "06090419"   MOVE "5615969" TO �����ԍ��v
      *     WHEN "06133086"   MOVE "5615969" TO �����ԍ��v
      *     WHEN "63090419"   MOVE "5615969" TO �����ԍ��v
      *     WHEN "63133086"   MOVE "5615969" TO �����ԍ��v
      *     WHEN "06133094"   MOVE "5615977" TO �����ԍ��v
      *     WHEN "06133102"   MOVE "5615985" TO �����ԍ��v
      *     WHEN "06133110"   MOVE "5615993" TO �����ԍ��v
      *     WHEN "06133169"   MOVE "5616000" TO �����ԍ��v
      *     WHEN "63133169"   MOVE "5616000" TO �����ԍ��v
      *     WHEN "06133177"   MOVE "5616019" TO �����ԍ��v
      *     WHEN "06133185"   MOVE "5616027" TO �����ԍ��v
      *     WHEN "06133243"   MOVE "5616035" TO �����ԍ��v
      *     WHEN "06133250"   MOVE "5616043" TO �����ԍ��v
      *     WHEN "06133276"   MOVE "5616051" TO �����ԍ��v
      *     WHEN "06133300"   MOVE "5616078" TO �����ԍ��v
      *     WHEN "06133342"   MOVE "5616086" TO �����ԍ��v
      *     WHEN "63133342"   MOVE "5616086" TO �����ԍ��v
      *     WHEN "06133375"   MOVE "5616094" TO �����ԍ��v
      *     WHEN "06133391"   MOVE "5616108" TO �����ԍ��v
      *     WHEN "06133417"   MOVE "5616116" TO �����ԍ��v
      *     WHEN "63133417"   MOVE "5616116" TO �����ԍ��v
      *     WHEN "06133425"   MOVE "5616124" TO �����ԍ��v
      *     WHEN "06133433"   MOVE "5616132" TO �����ԍ��v
      *     WHEN "06133458"   MOVE "5616140" TO �����ԍ��v
      *     WHEN "63133458"   MOVE "5616140" TO �����ԍ��v
      *     WHEN "06133474"   MOVE "5616159" TO �����ԍ��v
      *     WHEN "06133516"   MOVE "5616167" TO �����ԍ��v
      *     WHEN "06133540"   MOVE "5616175" TO �����ԍ��v
      *     WHEN "06133565"   MOVE "5616183" TO �����ԍ��v
      *     WHEN "06133573"   MOVE "5616191" TO �����ԍ��v
      *     WHEN "06133607"   MOVE "5616205" TO �����ԍ��v
      *     WHEN "06133615"   MOVE "5616213" TO �����ԍ��v
      *     WHEN "06133623"   MOVE "5616221" TO �����ԍ��v
      *     WHEN "06133631"   MOVE "5616248" TO �����ԍ��v
      *     WHEN "06133649"   MOVE "5616256" TO �����ԍ��v
      *     WHEN "06133672"   MOVE "5616264" TO �����ԍ��v
      *     WHEN "06133714"   MOVE "5616272" TO �����ԍ��v
      *     WHEN "06133730"   MOVE "5616280" TO �����ԍ��v
      *     WHEN "06141766"   MOVE "5616280" TO �����ԍ��v
      *     WHEN "06231104"   MOVE "5616280" TO �����ԍ��v
      *     WHEN "63133730"   MOVE "5616280" TO �����ԍ��v
      *     WHEN "06133771"   MOVE "5616299" TO �����ԍ��v
      *     WHEN "06133821"   MOVE "5616302" TO �����ԍ��v
      *     WHEN "06133862"   MOVE "5616310" TO �����ԍ��v
      *     WHEN "06133870"   MOVE "5616329" TO �����ԍ��v
      *     WHEN "06133888"   MOVE "5616337" TO �����ԍ��v
      *     WHEN "06133920"   MOVE "5616345" TO �����ԍ��v
      *     WHEN "06133938"   MOVE "5616353" TO �����ԍ��v
      *     WHEN "63133938"   MOVE "5616353" TO �����ԍ��v
      *     WHEN "06133946"   MOVE "5616361" TO �����ԍ��v
      *     WHEN "63133946"   MOVE "5616361" TO �����ԍ��v
      *     WHEN "06133961"   MOVE "5616388" TO �����ԍ��v
      *     WHEN "06134001"   MOVE "5616396" TO �����ԍ��v
      *     WHEN "06134019"   MOVE "5616418" TO �����ԍ��v
      *     WHEN "06134035"   MOVE "5616426" TO �����ԍ��v
      *     WHEN "06134050"   MOVE "5616434" TO �����ԍ��v
      *     WHEN "06134076"   MOVE "5616442" TO �����ԍ��v
      *     WHEN "06134084"   MOVE "5616450" TO �����ԍ��v
      *     WHEN "06134134"   MOVE "5616469" TO �����ԍ��v
      *     WHEN "63134134"   MOVE "5616469" TO �����ԍ��v
      *     WHEN "06134159"   MOVE "5616477" TO �����ԍ��v
      *     WHEN "06134175"   MOVE "5616485" TO �����ԍ��v
      *     WHEN "06134183"   MOVE "5616493" TO �����ԍ��v
      *     WHEN "63134183"   MOVE "5616493" TO �����ԍ��v
      *     WHEN "06134217"   MOVE "5616507" TO �����ԍ��v
      *     WHEN "06134340"   MOVE "5616515" TO �����ԍ��v
      *     WHEN "06134357"   MOVE "5616523" TO �����ԍ��v
      *     WHEN "06134365"   MOVE "5616531" TO �����ԍ��v
      *     WHEN "06134373"   MOVE "5616558" TO �����ԍ��v
      *     WHEN "06134381"   MOVE "5616566" TO �����ԍ��v
      *     WHEN "06134431"   MOVE "5616574" TO �����ԍ��v
      *     WHEN "63134431"   MOVE "5616574" TO �����ԍ��v
      *     WHEN "06134464"   MOVE "5616582" TO �����ԍ��v
      *     WHEN "06134498"   MOVE "5616590" TO �����ԍ��v
      *     WHEN "06134522"   MOVE "5616604" TO �����ԍ��v
      *     WHEN "06134530"   MOVE "5616612" TO �����ԍ��v
      *     WHEN "06134548"   MOVE "5616620" TO �����ԍ��v
      *     WHEN "06134555"   MOVE "5616639" TO �����ԍ��v
      *     WHEN "63134555"   MOVE "5616639" TO �����ԍ��v
      *     WHEN "06134571"   MOVE "5616647" TO �����ԍ��v
      *     WHEN "06134613"   MOVE "5616655" TO �����ԍ��v
      *     WHEN "06134621"   MOVE "5616663" TO �����ԍ��v
      *     WHEN "06134688"   MOVE "5616671" TO �����ԍ��v
      *     WHEN "06134795"   MOVE "5616698" TO �����ԍ��v
      *     WHEN "06134803"   MOVE "5616701" TO �����ԍ��v
      *     WHEN "06134845"   MOVE "5616728" TO �����ԍ��v
      *     WHEN "06134886"   MOVE "5616736" TO �����ԍ��v
      *     WHEN "06134902"   MOVE "5616744" TO �����ԍ��v
      *     WHEN "06134910"   MOVE "5616752" TO �����ԍ��v
      *     WHEN "06134928"   MOVE "5616760" TO �����ԍ��v
      *     WHEN "06134944"   MOVE "5616779" TO �����ԍ��v
      *     WHEN "06134969"   MOVE "5616787" TO �����ԍ��v
      *     WHEN "06135024"   MOVE "5616795" TO �����ԍ��v
      *     WHEN "06135040"   MOVE "5616809" TO �����ԍ��v
      *     WHEN "63135040"   MOVE "5616809" TO �����ԍ��v
      *     WHEN "06135057"   MOVE "5616817" TO �����ԍ��v
      *     WHEN "06135123"   MOVE "5616825" TO �����ԍ��v
      *     WHEN "06135172"   MOVE "5616833" TO �����ԍ��v
      *     WHEN "06135180"   MOVE "5616841" TO �����ԍ��v
      *     WHEN "06135222"   MOVE "5616868" TO �����ԍ��v
      *     WHEN "06135248"   MOVE "5616876" TO �����ԍ��v
      *     WHEN "06135255"   MOVE "5616884" TO �����ԍ��v
      *     WHEN "06135354"   MOVE "5616892" TO �����ԍ��v
      *     WHEN "06135370"   MOVE "5616906" TO �����ԍ��v
      *     WHEN "06135388"   MOVE "5616914" TO �����ԍ��v
      *     WHEN "06135396"   MOVE "5616922" TO �����ԍ��v
      *     WHEN "06135404"   MOVE "5616930" TO �����ԍ��v
      *     WHEN "06135438"   MOVE "5616949" TO �����ԍ��v
      *     WHEN "06135453"   MOVE "5616957" TO �����ԍ��v
      *     WHEN "06135487"   MOVE "5616965" TO �����ԍ��v
      *     WHEN "63135487"   MOVE "5616965" TO �����ԍ��v
      *     WHEN "06135503"   MOVE "5616973" TO �����ԍ��v
      *     WHEN "06135545"   MOVE "5616981" TO �����ԍ��v
      *     WHEN "06135552"   MOVE "5617007" TO �����ԍ��v
      *     WHEN "06135578"   MOVE "5617015" TO �����ԍ��v
      *     WHEN "06135628"   MOVE "5617023" TO �����ԍ��v
      *     WHEN "06135669"   MOVE "5617031" TO �����ԍ��v
      *     WHEN "06135719"   MOVE "5617058" TO �����ԍ��v
      *     WHEN "06135727"   MOVE "5617066" TO �����ԍ��v
      *     WHEN "06135750"   MOVE "5617074" TO �����ԍ��v
      *     WHEN "06135768"   MOVE "5617082" TO �����ԍ��v
      *     WHEN "06135776"   MOVE "5617090" TO �����ԍ��v
      *     WHEN "06135784"   MOVE "5617104" TO �����ԍ��v
      *     WHEN "06135834"   MOVE "5617112" TO �����ԍ��v
      *     WHEN "06135859"   MOVE "5617120" TO �����ԍ��v
      *     WHEN "06135891"   MOVE "5617139" TO �����ԍ��v
      *     WHEN "06135909"   MOVE "5617147" TO �����ԍ��v
      *     WHEN "06135917"   MOVE "5617155" TO �����ԍ��v
      *     WHEN "06135990"   MOVE "5617163" TO �����ԍ��v
      *     WHEN "06136006"   MOVE "5617171" TO �����ԍ��v
      *     WHEN "06136063"   MOVE "5617198" TO �����ԍ��v
      *     WHEN "06136097"   MOVE "5617201" TO �����ԍ��v
      *     WHEN "06136162"   MOVE "5617228" TO �����ԍ��v
      *     WHEN "06136196"   MOVE "5617236" TO �����ԍ��v
      *     WHEN "63136196"   MOVE "5617236" TO �����ԍ��v
      *     WHEN "06136246"   MOVE "5617244" TO �����ԍ��v
      *     WHEN "06136279"   MOVE "5617252" TO �����ԍ��v
      *     WHEN "06136287"   MOVE "5617260" TO �����ԍ��v
      *     WHEN "63136287"   MOVE "5617260" TO �����ԍ��v
      *     WHEN "06136295"   MOVE "5617279" TO �����ԍ��v
      *     WHEN "06136345"   MOVE "5617287" TO �����ԍ��v
      *     WHEN "06136378"   MOVE "5617295" TO �����ԍ��v
      *     WHEN "06136394"   MOVE "5617309" TO �����ԍ��v
      *     WHEN "06136410"   MOVE "5617317" TO �����ԍ��v
      *     WHEN "06136428"   MOVE "5617325" TO �����ԍ��v
      *     WHEN "06136436"   MOVE "5617333" TO �����ԍ��v
      *     WHEN "06136477"   MOVE "5617341" TO �����ԍ��v
      *     WHEN "63136477"   MOVE "5617341" TO �����ԍ��v
      *     WHEN "06136493"   MOVE "5617368" TO �����ԍ��v
      *     WHEN "06136501"   MOVE "5617376" TO �����ԍ��v
      *     WHEN "06136519"   MOVE "5617384" TO �����ԍ��v
      *     WHEN "06136550"   MOVE "5617392" TO �����ԍ��v
      *     WHEN "06136568"   MOVE "5617406" TO �����ԍ��v
      *     WHEN "06136618"   MOVE "5617414" TO �����ԍ��v
      *     WHEN "06136634"   MOVE "5617422" TO �����ԍ��v
      *     WHEN "06136642"   MOVE "5617430" TO �����ԍ��v
      *     WHEN "06136659"   MOVE "5617449" TO �����ԍ��v
      *     WHEN "06136709"   MOVE "5617457" TO �����ԍ��v
      *     WHEN "06136717"   MOVE "5617465" TO �����ԍ��v
      *     WHEN "06136741"   MOVE "5617473" TO �����ԍ��v
      *     WHEN "06136758"   MOVE "5617481" TO �����ԍ��v
      *     WHEN "06136774"   MOVE "5617503" TO �����ԍ��v
      *     WHEN "06136790"   MOVE "5617511" TO �����ԍ��v
      *     WHEN "06136881"   MOVE "5617538" TO �����ԍ��v
      *     WHEN "06136915"   MOVE "5617546" TO �����ԍ��v
      *     WHEN "06136923"   MOVE "5617554" TO �����ԍ��v
      *     WHEN "06136956"   MOVE "5617562" TO �����ԍ��v
      *     WHEN "06137079"   MOVE "5617570" TO �����ԍ��v
      *     WHEN "06137087"   MOVE "5617589" TO �����ԍ��v
      *     WHEN "06137103"   MOVE "5617597" TO �����ԍ��v
      *     WHEN "06137202"   MOVE "5617600" TO �����ԍ��v
      *     WHEN "06137210"   MOVE "5617619" TO �����ԍ��v
      *     WHEN "06137236"   MOVE "5617627" TO �����ԍ��v
      *     WHEN "06137251"   MOVE "5617635" TO �����ԍ��v
      *     WHEN "06137277"   MOVE "5617643" TO �����ԍ��v
      *     WHEN "06137301"   MOVE "5617651" TO �����ԍ��v
      *     WHEN "06137327"   MOVE "5617678" TO �����ԍ��v
      *     WHEN "06137335"   MOVE "5617686" TO �����ԍ��v
      *     WHEN "06137350"   MOVE "5617694" TO �����ԍ��v
      *     WHEN "06137368"   MOVE "5617708" TO �����ԍ��v
      *     WHEN "06137376"   MOVE "5617716" TO �����ԍ��v
      *     WHEN "06137384"   MOVE "5617724" TO �����ԍ��v
      *     WHEN "06137418"   MOVE "5617732" TO �����ԍ��v
      *     WHEN "06137442"   MOVE "5617740" TO �����ԍ��v
      *     WHEN "06137491"   MOVE "5617759" TO �����ԍ��v
      *     WHEN "06137525"   MOVE "5617767" TO �����ԍ��v
      *     WHEN "06137566"   MOVE "5617775" TO �����ԍ��v
      *     WHEN "06137582"   MOVE "5617783" TO �����ԍ��v
      *     WHEN "06137590"   MOVE "5617791" TO �����ԍ��v
      *     WHEN "06137608"   MOVE "5617805" TO �����ԍ��v
      *     WHEN "06137640"   MOVE "5617813" TO �����ԍ��v
      *     WHEN "06137665"   MOVE "5617821" TO �����ԍ��v
      *     WHEN "06137681"   MOVE "5617848" TO �����ԍ��v
      *     WHEN "06137723"   MOVE "5617856" TO �����ԍ��v
      *     WHEN "06137772"   MOVE "5617864" TO �����ԍ��v
      *     WHEN "06137780"   MOVE "5617872" TO �����ԍ��v
      *     WHEN "06137798"   MOVE "5617880" TO �����ԍ��v
      *     WHEN "06137855"   MOVE "5617899" TO �����ԍ��v
      *     WHEN "63137855"   MOVE "5617899" TO �����ԍ��v
      *     WHEN "06137863"   MOVE "5617902" TO �����ԍ��v
      *     WHEN "06137897"   MOVE "5617910" TO �����ԍ��v
      *     WHEN "06137913"   MOVE "5617929" TO �����ԍ��v
      *     WHEN "06137947"   MOVE "5617937" TO �����ԍ��v
      *     WHEN "06137996"   MOVE "5617945" TO �����ԍ��v
      *     WHEN "06138010"   MOVE "5617953" TO �����ԍ��v
      *     WHEN "06138051"   MOVE "5617961" TO �����ԍ��v
      *     WHEN "06138077"   MOVE "5617988" TO �����ԍ��v
      *     WHEN "06138085"   MOVE "5617996" TO �����ԍ��v
      *     WHEN "06138093"   MOVE "5618003" TO �����ԍ��v
      *     WHEN "06138119"   MOVE "5618011" TO �����ԍ��v
      *     WHEN "06138127"   MOVE "5618038" TO �����ԍ��v
      *     WHEN "06138143"   MOVE "5618046" TO �����ԍ��v
      *     WHEN "06138150"   MOVE "5618054" TO �����ԍ��v
      *     WHEN "63138150"   MOVE "5618054" TO �����ԍ��v
      *     WHEN "06138168"   MOVE "5618062" TO �����ԍ��v
      *     WHEN "06138192"   MOVE "5618070" TO �����ԍ��v
      *     WHEN "06138226"   MOVE "5618089" TO �����ԍ��v
      *     WHEN "06138242"   MOVE "5618097" TO �����ԍ��v
      *     WHEN "06138309"   MOVE "5618100" TO �����ԍ��v
      *     WHEN "06138341"   MOVE "5618119" TO �����ԍ��v
      *     WHEN "06138424"   MOVE "5618127" TO �����ԍ��v
      *     WHEN "06138432"   MOVE "5618135" TO �����ԍ��v
      *     WHEN "06138440"   MOVE "5618143" TO �����ԍ��v
      *     WHEN "06138457"   MOVE "5618151" TO �����ԍ��v
      *     WHEN "06138465"   MOVE "5618178" TO �����ԍ��v
      *     WHEN "06138481"   MOVE "5618186" TO �����ԍ��v
      *     WHEN "06138499"   MOVE "5618194" TO �����ԍ��v
      *     WHEN "06138515"   MOVE "5618208" TO �����ԍ��v
      *     WHEN "06138549"   MOVE "5618216" TO �����ԍ��v
      *     WHEN "06138564"   MOVE "5618224" TO �����ԍ��v
      *     WHEN "06138580"   MOVE "5618232" TO �����ԍ��v
      *     WHEN "63138580"   MOVE "5618232" TO �����ԍ��v
      *     WHEN "06138663"   MOVE "5618240" TO �����ԍ��v
      *     WHEN "06138689"   MOVE "5618259" TO �����ԍ��v
      *     WHEN "06138705"   MOVE "5618267" TO �����ԍ��v
      *     WHEN "06138713"   MOVE "5618275" TO �����ԍ��v
      *     WHEN "06138721"   MOVE "5618283" TO �����ԍ��v
      *     WHEN "06138796"   MOVE "5618291" TO �����ԍ��v
      *     WHEN "63138796"   MOVE "5618291" TO �����ԍ��v
      *     WHEN "06139067"   MOVE "5618305" TO �����ԍ��v
      *     WHEN "06139083"   MOVE "5618313" TO �����ԍ��v
      *     WHEN "06139117"   MOVE "5618321" TO �����ԍ��v
      *     WHEN "06139216"   MOVE "5618348" TO �����ԍ��v
      *     WHEN "06271191"   MOVE "5618348" TO �����ԍ��v
      *     WHEN "06130025"   MOVE "5618356" TO �����ԍ��v
      *     WHEN "06130157"   MOVE "5618364" TO �����ԍ��v
      *     WHEN "06130512"   MOVE "5618372" TO �����ԍ��v
      *     WHEN "06131254"   MOVE "5618380" TO �����ԍ��v
      *     WHEN "06132815"   MOVE "5618399" TO �����ԍ��v
      *     WHEN "06133409"   MOVE "5618402" TO �����ԍ��v
      *     WHEN "06133532"   MOVE "5618410" TO �����ԍ��v
      *     WHEN "06133722"   MOVE "5618429" TO �����ԍ��v
      *     WHEN "06133854"   MOVE "5618437" TO �����ԍ��v
      *     WHEN "06134753"   MOVE "5618445" TO �����ԍ��v
      *     WHEN "06134787"   MOVE "5618453" TO �����ԍ��v
      *     WHEN "06135933"   MOVE "5618461" TO �����ԍ��v
      *     WHEN "06136535"   MOVE "5618488" TO �����ԍ��v
      *     WHEN "06137467"   MOVE "5618496" TO �����ԍ��v
      *     WHEN "06137905"   MOVE "5618518" TO �����ԍ��v
      *     WHEN "06138184"   MOVE "5618526" TO �����ԍ��v
      *     WHEN "06138382"   MOVE "5618534" TO �����ԍ��v
      *     WHEN "06138572"   MOVE "5618542" TO �����ԍ��v
      *     WHEN "06139075"   MOVE "5618550" TO �����ԍ��v
      *     WHEN "06139141"   MOVE "5618569" TO �����ԍ��v
      *     WHEN "06131528"   MOVE "5618577" TO �����ԍ��v
      *     WHEN "06133136"   MOVE "5618585" TO �����ԍ��v
      *     WHEN "06133334"   MOVE "5618593" TO �����ԍ��v
      *     WHEN "06134241"   MOVE "5618607" TO �����ԍ��v
      *     WHEN "06134670"   MOVE "5618615" TO �����ԍ��v
      *     WHEN "06134829"   MOVE "5618623" TO �����ԍ��v
      *     WHEN "06135636"   MOVE "5618631" TO �����ԍ��v
      *     WHEN "06135974"   MOVE "5618658" TO �����ԍ��v
      *     WHEN "06136691"   MOVE "5618666" TO �����ԍ��v
      *     WHEN "06136808"   MOVE "5618674" TO �����ԍ��v
      *     WHEN "06137004"   MOVE "5618682" TO �����ԍ��v
      *     WHEN "06137400"   MOVE "5618690" TO �����ԍ��v
      *     WHEN "06137541"   MOVE "5618704" TO �����ԍ��v
      *     WHEN "06137822"   MOVE "5618712" TO �����ԍ��v
      *     WHEN "06138523"   MOVE "5618720" TO �����ԍ��v
      *     WHEN "06139190"   MOVE "5618739" TO �����ԍ��v
      *     WHEN "31130016"   MOVE "5618747" TO �����ԍ��v
      *     WHEN "31130032"   MOVE "5618755" TO �����ԍ��v
      *     WHEN "31130073"   MOVE "5618763" TO �����ԍ��v
      *     WHEN "31130131"   MOVE "5618771" TO �����ԍ��v
      *     WHEN "31130222"   MOVE "5618798" TO �����ԍ��v
      *     WHEN "31130248"   MOVE "5618801" TO �����ԍ��v
      *     WHEN "31130305"   MOVE "5618828" TO �����ԍ��v
      *     WHEN "31130479"   MOVE "5618836" TO �����ԍ��v
      *     WHEN "31130511"   MOVE "5618844" TO �����ԍ��v
      *     WHEN "31130537"   MOVE "5618852" TO �����ԍ��v
      *     WHEN "31130594"   MOVE "5618860" TO �����ԍ��v
      *     WHEN "31130685"   MOVE "5618879" TO �����ԍ��v
      *     WHEN "31130842"   MOVE "5618887" TO �����ԍ��v
      *     WHEN "31110257"   MOVE "5618895" TO �����ԍ��v
      *     WHEN "31131105"   MOVE "5618895" TO �����ԍ��v
      *     WHEN "31170178"   MOVE "5618895" TO �����ԍ��v
      *     WHEN "31430192"   MOVE "5618895" TO �����ԍ��v
      *     WHEN "31131147"   MOVE "5618909" TO �����ԍ��v
      *     WHEN "31131188"   MOVE "5618917" TO �����ԍ��v
      *     WHEN "31131261"   MOVE "5618925" TO �����ԍ��v
      *     WHEN "31131295"   MOVE "5618933" TO �����ԍ��v
      *     WHEN "31131311"   MOVE "5618941" TO �����ԍ��v
      *     WHEN "31131394"   MOVE "5618968" TO �����ԍ��v
      *     WHEN "31131444"   MOVE "5618976" TO �����ԍ��v
      *     WHEN "31131535"   MOVE "5618984" TO �����ԍ��v
      *     WHEN "32130213"   MOVE "5618992" TO �����ԍ��v
      *     WHEN "32130411"   MOVE "5619018" TO �����ԍ��v
      *     WHEN "33130014"   MOVE "5619026" TO �����ԍ��v
      *     WHEN "33130030"   MOVE "5619034" TO �����ԍ��v
      *     WHEN "34130013"   MOVE "5619042" TO �����ԍ��v
      *     WHEN "34130021"   MOVE "5619050" TO �����ԍ��v
      *     WHEN "31131410"   MOVE "5619069" TO �����ԍ��v
      *     WHEN "31131741"   MOVE "5619077" TO �����ԍ��v
      *     WHEN "31131774"   MOVE "5619085" TO �����ԍ��v
      *     WHEN "41110149"   MOVE "5619093" TO �����ԍ��v
      *     WHEN "41110362"   MOVE "5619107" TO �����ԍ��v
      *     WHEN "41110412"   MOVE "5619115" TO �����ԍ��v
      *     WHEN "41110891"   MOVE "5619123" TO �����ԍ��v
      *     WHEN "41110933"   MOVE "5619131" TO �����ԍ��v
      *     WHEN "41145004"   MOVE "5619158" TO �����ԍ��v
      *     WHEN "41145012"   MOVE "5619158" TO �����ԍ��v
      *     WHEN "41145020"   MOVE "5619158" TO �����ԍ��v
      *     WHEN "41145038"   MOVE "5619158" TO �����ԍ��v
      *     WHEN "41145046"   MOVE "5619158" TO �����ԍ��v
      *     WHEN "41145053"   MOVE "5619158" TO �����ԍ��v
      *     WHEN "41145061"   MOVE "5619158" TO �����ԍ��v
      *     WHEN "41145079"   MOVE "5619158" TO �����ԍ��v
      *     WHEN "41110032"   MOVE "5619166" TO �����ԍ��v
      *     WHEN "41110842"   MOVE "5619174" TO �����ԍ��v
      *     WHEN "41114042"   MOVE "5619182" TO �����ԍ��v
      *     WHEN "41110016"   MOVE "5619190" TO �����ԍ��v
      *     WHEN "41110156"   MOVE "5619204" TO �����ԍ��v
      *     WHEN "41110222"   MOVE "5619212" TO �����ԍ��v
      *     WHEN "41110230"   MOVE "5619220" TO �����ԍ��v
      *     WHEN "41110263"   MOVE "5619239" TO �����ԍ��v
      *     WHEN "41110305"   MOVE "5619247" TO �����ԍ��v
      *     WHEN "41110370"   MOVE "5619255" TO �����ԍ��v
      *     WHEN "41110925"   MOVE "5619263" TO �����ԍ��v
      *     WHEN "41114026"   MOVE "5619271" TO �����ԍ��v
      *     WHEN "41114034"   MOVE "5619298" TO �����ԍ��v
      *     WHEN "41114083"   MOVE "5619301" TO �����ԍ��v
      *     WHEN "41140062"   MOVE "5619328" TO �����ԍ��v
      *     WHEN "41140179"   MOVE "5619336" TO �����ԍ��v
      *     WHEN "06132724"   MOVE "5619344" TO �����ԍ��v
      *     WHEN "06136220"   MOVE "5619352" TO �����ԍ��v
      *     WHEN "06139182"   MOVE "5619360" TO �����ԍ��v
      *     WHEN "67110312"   MOVE "5619379" TO �����ԍ��v
      *     WHEN "67110338"   MOVE "5619387" TO �����ԍ��v
      *     WHEN "67110478"   MOVE "5619395" TO �����ԍ��v
      *     WHEN "67110502"   MOVE "5619409" TO �����ԍ��v
      *     WHEN "67110791"   MOVE "5619417" TO �����ԍ��v
      *     WHEN "67110866"   MOVE "5619425" TO �����ԍ��v
      *     WHEN "67138263"   MOVE "5619433" TO �����ԍ��v
      *     WHEN "67138339"   MOVE "5619441" TO �����ԍ��v
      *     WHEN "67138495"   MOVE "5619468" TO �����ԍ��v
      *     WHEN "06139307"   MOVE "5619476" TO �����ԍ��v
      *     WHEN "27110501"   MOVE "5619484" TO �����ԍ��v
      *     WHEN "27110519"   MOVE "5619492" TO �����ԍ��v
      *     WHEN "27110527"   MOVE "5619506" TO �����ԍ��v
      *     WHEN "27114065"   MOVE "5619514" TO �����ԍ��v
      *     WHEN "27138304"   MOVE "5619522" TO �����ԍ��v
      *     WHEN "27138361"   MOVE "5619530" TO �����ԍ��v
      *     WHEN "81137242"   MOVE "5619549" TO �����ԍ��v
      *     WHEN "88131248"   MOVE "5619549" TO �����ԍ��v
      *     WHEN "88138243"   MOVE "5619549" TO �����ԍ��v
      *     WHEN "81137358"   MOVE "5619557" TO �����ԍ��v
      *     WHEN "88131354"   MOVE "5619557" TO �����ԍ��v
      *     WHEN "88138359"   MOVE "5619557" TO �����ԍ��v
      *     WHEN "41139015"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139023"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139031"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139049"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139056"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139064"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139072"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139080"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139098"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139106"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139114"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139122"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139130"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139148"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139155"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139163"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139171"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139189"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139197"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139205"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139213"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139221"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139239"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139247"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139254"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139262"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139270"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139288"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139296"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139304"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139312"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139320"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139338"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139346"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139353"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139361"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139379"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139387"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139395"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139403"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139411"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139429"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139437"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139445"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139452"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139460"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139478"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139486"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139494"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139502"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139510"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139528"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139536"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139544"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139551"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139569"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139577"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139585"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139593"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139601"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139619"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139627"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139635"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "41139643"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "8013500"    MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135015"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135023"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135031"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135106"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135122"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135148"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135155"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135171"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135197"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135213"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135221"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135239"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135353"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80135478"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136039"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136047"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136054"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136062"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136070"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136088"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136096"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136104"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136112"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136120"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136138"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136161"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136179"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136187"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136195"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136211"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136229"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136237"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136278"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136294"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136328"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80136427"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137028"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137029"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137037"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137045"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137052"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137060"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137078"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137086"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137094"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137110"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137128"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137144"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137169"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137177"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137185"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137193"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137201"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137219"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137227"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137235"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137250"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137276"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137318"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137326"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137342"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137359"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137383"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137391"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137425"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137433"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137458"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137474"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "80137482"   MOVE "5619565" TO �����ԍ��v
      *     WHEN "81136293"   MOVE "5619573" TO �����ԍ��v
      *     WHEN "81137291"   MOVE "5619573" TO �����ԍ��v
      *     WHEN "88138292"   MOVE "5619573" TO �����ԍ��v
      *     WHEN "81136459"   MOVE "5619581" TO �����ԍ��v
      *     WHEN "81137457"   MOVE "5619581" TO �����ԍ��v
      *     WHEN "88131453"   MOVE "5619581" TO �����ԍ��v
      *     WHEN "88138458"   MOVE "5619581" TO �����ԍ��v
      *     WHEN "81136186"   MOVE "5619603" TO �����ԍ��v
      *     WHEN "81137184"   MOVE "5619603" TO �����ԍ��v
      *     WHEN "88135181"   MOVE "5619603" TO �����ԍ��v
      *     WHEN "88138185"   MOVE "5619603" TO �����ԍ��v
      *     WHEN "81136178"   MOVE "5619611" TO �����ԍ��v
      *     WHEN "81137176"   MOVE "5619611" TO �����ԍ��v
      *     WHEN "88132170"   MOVE "5619611" TO �����ԍ��v
      *     WHEN "88133178"   MOVE "5619611" TO �����ԍ��v
      *     WHEN "88138177"   MOVE "5619611" TO �����ԍ��v
      *     WHEN "81137010"   MOVE "5619638" TO �����ԍ��v
      *     WHEN "88133012"   MOVE "5619638" TO �����ԍ��v
      *     WHEN "88138011"   MOVE "5619638" TO �����ԍ��v
      *     WHEN "80405020"   MOVE "5619646" TO �����ԍ��v
      **     WHEN "904050"     MOVE "5619646" TO �����ԍ��v
      *     WHEN "90405028"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "90405036"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "90405069"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "90405085"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "01140011"   MOVE "5619654" TO �����ԍ��v
      *     WHEN "03140019"   MOVE "5619654" TO �����ԍ��v
      *     WHEN "04140018"   MOVE "5619654" TO �����ԍ��v
      *     WHEN "01150010"   MOVE "5619662" TO �����ԍ��v
      *     WHEN "03150018"   MOVE "5619662" TO �����ԍ��v
      *     WHEN "04150017"   MOVE "5619662" TO �����ԍ��v
      *     WHEN "01160019"   MOVE "5619670" TO �����ԍ��v
      *     WHEN "03160017"   MOVE "5619670" TO �����ԍ��v
      *     WHEN "04160016"   MOVE "5619670" TO �����ԍ��v
      *     WHEN "01170018"   MOVE "5619689" TO �����ԍ��v
      *     WHEN "03170016"   MOVE "5619689" TO �����ԍ��v
      *     WHEN "04170015"   MOVE "5619689" TO �����ԍ��v
      *     WHEN "01180017"   MOVE "5619697" TO �����ԍ��v
      *     WHEN "03180015"   MOVE "5619697" TO �����ԍ��v
      *     WHEN "04180014"   MOVE "5619697" TO �����ԍ��v
      *     WHEN "01190016"   MOVE "5619700" TO �����ԍ��v
      *     WHEN "03190014"   MOVE "5619700" TO �����ԍ��v
      *     WHEN "04190013"   MOVE "5619700" TO �����ԍ��v
      *     WHEN "01200013"   MOVE "5619719" TO �����ԍ��v
      *     WHEN "03200011"   MOVE "5619719" TO �����ԍ��v
      *     WHEN "04200010"   MOVE "5619719" TO �����ԍ��v
      *     WHEN "01210012"   MOVE "5619727" TO �����ԍ��v
      *     WHEN "03210010"   MOVE "5619727" TO �����ԍ��v
      *     WHEN "04210019"   MOVE "5619727" TO �����ԍ��v
      *     WHEN "01220011"   MOVE "5619735" TO �����ԍ��v
      *     WHEN "03220019"   MOVE "5619735" TO �����ԍ��v
      *     WHEN "04220018"   MOVE "5619735" TO �����ԍ��v
      *     WHEN "01230010"   MOVE "5619743" TO �����ԍ��v
      *     WHEN "03230018"   MOVE "5619743" TO �����ԍ��v
      *     WHEN "04230017"   MOVE "5619743" TO �����ԍ��v
      *     WHEN "01240019"   MOVE "5619751" TO �����ԍ��v
      *     WHEN "03240017"   MOVE "5619751" TO �����ԍ��v
      *     WHEN "04240016"   MOVE "5619751" TO �����ԍ��v
      *     WHEN "01250018"   MOVE "5619778" TO �����ԍ��v
      *     WHEN "03250016"   MOVE "5619778" TO �����ԍ��v
      *     WHEN "04250015"   MOVE "5619778" TO �����ԍ��v
      *     WHEN "01260017"   MOVE "5619786" TO �����ԍ��v
      *     WHEN "03260015"   MOVE "5619786" TO �����ԍ��v
      *     WHEN "04260014"   MOVE "5619786" TO �����ԍ��v
      *     WHEN "01270016"   MOVE "5619794" TO �����ԍ��v
      *     WHEN "03270014"   MOVE "5619794" TO �����ԍ��v
      *     WHEN "04270013"   MOVE "5619794" TO �����ԍ��v
      *     WHEN "01280015"   MOVE "5619808" TO �����ԍ��v
      *     WHEN "03280013"   MOVE "5619808" TO �����ԍ��v
      *     WHEN "04280012"   MOVE "5619808" TO �����ԍ��v
      *     WHEN "01290014"   MOVE "5619816" TO �����ԍ��v
      *     WHEN "03290012"   MOVE "5619816" TO �����ԍ��v
      *     WHEN "04290011"   MOVE "5619816" TO �����ԍ��v
      *     WHEN "01300011"   MOVE "5619824" TO �����ԍ��v
      *     WHEN "03300019"   MOVE "5619824" TO �����ԍ��v
      *     WHEN "04300018"   MOVE "5619824" TO �����ԍ��v
      *     WHEN "01310010"   MOVE "5619832" TO �����ԍ��v
      *     WHEN "03310018"   MOVE "5619832" TO �����ԍ��v
      *     WHEN "04310017"   MOVE "5619832" TO �����ԍ��v
      *     WHEN "01320019"   MOVE "5619840" TO �����ԍ��v
      *     WHEN "03320017"   MOVE "5619840" TO �����ԍ��v
      *     WHEN "04320016"   MOVE "5619840" TO �����ԍ��v
      *     WHEN "01330018"   MOVE "5619859" TO �����ԍ��v
      *     WHEN "03330016"   MOVE "5619859" TO �����ԍ��v
      *     WHEN "04330015"   MOVE "5619859" TO �����ԍ��v
      *     WHEN "01340017"   MOVE "5619867" TO �����ԍ��v
      *     WHEN "03340015"   MOVE "5619867" TO �����ԍ��v
      *     WHEN "04340014"   MOVE "5619867" TO �����ԍ��v
      *     WHEN "01350016"   MOVE "5619875" TO �����ԍ��v
      *     WHEN "03350014"   MOVE "5619875" TO �����ԍ��v
      *     WHEN "04350013"   MOVE "5619875" TO �����ԍ��v
      *     WHEN "01360015"   MOVE "5619883" TO �����ԍ��v
      *     WHEN "03360013"   MOVE "5619883" TO �����ԍ��v
      *     WHEN "04360012"   MOVE "5619883" TO �����ԍ��v
      *     WHEN "01370014"   MOVE "5619891" TO �����ԍ��v
      *     WHEN "03370012"   MOVE "5619891" TO �����ԍ��v
      *     WHEN "04370011"   MOVE "5619891" TO �����ԍ��v
      *     WHEN "01380013"   MOVE "5619905" TO �����ԍ��v
      *     WHEN "03380011"   MOVE "5619905" TO �����ԍ��v
      *     WHEN "04380010"   MOVE "5619905" TO �����ԍ��v
      *     WHEN "01390012"   MOVE "5619913" TO �����ԍ��v
      *     WHEN "03390010"   MOVE "5619913" TO �����ԍ��v
      *     WHEN "04390019"   MOVE "5619913" TO �����ԍ��v
      *     WHEN "01400019"   MOVE "5619921" TO �����ԍ��v
      *     WHEN "03400017"   MOVE "5619921" TO �����ԍ��v
      *     WHEN "04400016"   MOVE "5619921" TO �����ԍ��v
      *     WHEN "01410018"   MOVE "5619948" TO �����ԍ��v
      *     WHEN "03410016"   MOVE "5619948" TO �����ԍ��v
      *     WHEN "04410015"   MOVE "5619948" TO �����ԍ��v
      *     WHEN "01420017"   MOVE "5619956" TO �����ԍ��v
      *     WHEN "03420015"   MOVE "5619956" TO �����ԍ��v
      *     WHEN "04420014"   MOVE "5619956" TO �����ԍ��v
      *     WHEN "01430016"   MOVE "5619964" TO �����ԍ��v
      *     WHEN "03430014"   MOVE "5619964" TO �����ԍ��v
      *     WHEN "04430013"   MOVE "5619964" TO �����ԍ��v
      *     WHEN "01440015"   MOVE "5619972" TO �����ԍ��v
      *     WHEN "03440013"   MOVE "5619972" TO �����ԍ��v
      *     WHEN "04440012"   MOVE "5619972" TO �����ԍ��v
      *     WHEN "01450014"   MOVE "5619980" TO �����ԍ��v
      *     WHEN "03450012"   MOVE "5619980" TO �����ԍ��v
      *     WHEN "04450011"   MOVE "5619980" TO �����ԍ��v
      *     WHEN "01460013"   MOVE "5619999" TO �����ԍ��v
      *     WHEN "03460011"   MOVE "5619999" TO �����ԍ��v
      *     WHEN "04460010"   MOVE "5619999" TO �����ԍ��v
      *     WHEN "01470012"   MOVE "5620008" TO �����ԍ��v
      *     WHEN "03470010"   MOVE "5620008" TO �����ԍ��v
      *     WHEN "04470019"   MOVE "5620008" TO �����ԍ��v
      *     WHEN "39131016"   MOVE "5620326" TO �����ԍ��v
      *     WHEN "39131024"   MOVE "5620334" TO �����ԍ��v
      *     WHEN "39131032"   MOVE "5620342" TO �����ԍ��v
      *     WHEN "39131040"   MOVE "5620350" TO �����ԍ��v
      *     WHEN "39131057"   MOVE "5620369" TO �����ԍ��v
      *     WHEN "39131065"   MOVE "5620377" TO �����ԍ��v
      *     WHEN "39131073"   MOVE "5620385" TO �����ԍ��v
      *     WHEN "39131081"   MOVE "5620393" TO �����ԍ��v
      *     WHEN "39131099"   MOVE "5620407" TO �����ԍ��v
      *     WHEN "39131107"   MOVE "5620415" TO �����ԍ��v
      *     WHEN "39131115"   MOVE "5620423" TO �����ԍ��v
      *     WHEN "39131123"   MOVE "5620431" TO �����ԍ��v
      *     WHEN "39131131"   MOVE "5620458" TO �����ԍ��v
      *     WHEN "39131149"   MOVE "5620466" TO �����ԍ��v
      *     WHEN "39131156"   MOVE "5620474" TO �����ԍ��v
      *     WHEN "39131164"   MOVE "5620482" TO �����ԍ��v
      *     WHEN "39131172"   MOVE "5620490" TO �����ԍ��v
      *     WHEN "39131180"   MOVE "5620504" TO �����ԍ��v
      *     WHEN "39131198"   MOVE "5620512" TO �����ԍ��v
      *     WHEN "39131206"   MOVE "5620520" TO �����ԍ��v
      *     WHEN "39131214"   MOVE "5620539" TO �����ԍ��v
      *     WHEN "39131222"   MOVE "5620547" TO �����ԍ��v
      *     WHEN "39131230"   MOVE "5620555" TO �����ԍ��v
      *     WHEN "39132014"   MOVE "5620563" TO �����ԍ��v
      *     WHEN "39132022"   MOVE "5620571" TO �����ԍ��v
      *     WHEN "39132030"   MOVE "5620598" TO �����ԍ��v
      *     WHEN "39132048"   MOVE "5620601" TO �����ԍ��v
      *     WHEN "39132055"   MOVE "5620628" TO �����ԍ��v
      *     WHEN "39132063"   MOVE "5620636" TO �����ԍ��v
      *     WHEN "39132071"   MOVE "5620644" TO �����ԍ��v
      *     WHEN "39132089"   MOVE "5620652" TO �����ԍ��v
      *     WHEN "39132097"   MOVE "5620660" TO �����ԍ��v
      *     WHEN "39132105"   MOVE "5620679" TO �����ԍ��v
      *     WHEN "39132113"   MOVE "5620687" TO �����ԍ��v
      *     WHEN "39132121"   MOVE "5620695" TO �����ԍ��v
      *     WHEN "39132139"   MOVE "5620709" TO �����ԍ��v
      *     WHEN "39132147"   MOVE "5620717" TO �����ԍ��v
      *     WHEN "39132154"   MOVE "5620725" TO �����ԍ��v
      *     WHEN "39132188"   MOVE "5620733" TO �����ԍ��v
      *     WHEN "39132196"   MOVE "5620741" TO �����ԍ��v
      *     WHEN "39132204"   MOVE "5620768" TO �����ԍ��v
      *     WHEN "39132212"   MOVE "5620776" TO �����ԍ��v
      *     WHEN "39132220"   MOVE "5620784" TO �����ԍ��v
      *     WHEN "39132238"   MOVE "5620792" TO �����ԍ��v
      *     WHEN "39132246"   MOVE "5620806" TO �����ԍ��v
      *     WHEN "39132253"   MOVE "5620814" TO �����ԍ��v
      *     WHEN "39132279"   MOVE "5620822" TO �����ԍ��v
      *     WHEN "39132287"   MOVE "5620830" TO �����ԍ��v
      *     WHEN "39132295"   MOVE "5620849" TO �����ԍ��v
      *     WHEN "39133038"   MOVE "5620857" TO �����ԍ��v
      *     WHEN "39133053"   MOVE "5620865" TO �����ԍ��v
      *     WHEN "39133079"   MOVE "5620873" TO �����ԍ��v
      *     WHEN "39133087"   MOVE "5620881" TO �����ԍ��v
      *     WHEN "39352018"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39352026"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39352034"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39352042"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39352067"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39352075"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39352083"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39352109"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39352117"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39352125"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39352133"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39352158"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39352166"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39353057"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39353214"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39353412"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39353438"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39353446"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39355029"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39355045"   MOVE "5620946" TO �����ԍ��v
      *     WHEN "39412010"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39412028"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39412036"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39412044"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39412051"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39412069"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39412077"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39412085"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39412093"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39412101"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39413273"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39413414"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39413455"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39413463"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39413877"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39414016"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39414230"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39414248"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39414255"   MOVE "5620954" TO �����ԍ��v
      *     WHEN "39414412"   MOVE "5620954" TO �����ԍ��v
      **/����25�N5���{�p�����ǉ�������/130520
      *     WHEN "146001"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "146019"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "146027"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "146035"     MOVE "5610762" TO �����ԍ��v
      *     WHEN "67146019"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67146027"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "67146035"   MOVE "5610762" TO �����ԍ��v
      *     WHEN "41405069"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "80405012"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "80405038"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "80405046"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "80405053"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "80405061"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "80405079"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "80405087"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "90405010"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "90405044"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "90405051"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "90405077"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "90405093"   MOVE "5619646" TO �����ԍ��v
      *     WHEN "81405029"   MOVE "5619646" TO �����ԍ��v
      **/����25�N5���{�p�����ǉ�������/130520
           WHEN OTHER        MOVE "5610002" TO �����ԍ��v
           END-EVALUATE.
037520*================================================================*
       �J�n���擾 SECTION.
      *
      */�������ȍ~�ōŏ��Ƀt���O�������Ă���������Z�v�g�̊J�n���Ƃ���B
030830     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���ʐ��v
030840         IF ( �{�p�N�v = �����N�v(���ʂb�m�s) ) AND
030850            ( �{�p���v = �������v(���ʂb�m�s) )
030860             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
030870             MOVE �}�Ԃv�q              TO �{�L�|�}��
030880             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
030890             MOVE �����N�v(���ʂb�m�s)  TO �{�L�|�{�p�N
030900             MOVE �������v(���ʂb�m�s)  TO �{�L�|�{�p��
030910             MOVE �������v(���ʂb�m�s)  TO �{�L�|�{�p��
030920         ELSE
030930             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
030940             MOVE �}�Ԃv�q              TO �{�L�|�}��
030950             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
030960             MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
030970             MOVE �{�p���v�q            TO �{�L�|�{�p��
030980             MOVE ZERO                  TO �{�L�|�{�p��
030990         END-IF
031000         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
031010                                      �{�L�|�{�p�a��N����
031020         END-START
031030         IF ��ԃL�[ = "00"
                  MOVE SPACE TO �I���t���O�Q
                  PERFORM �{�p�L�^�e�Ǎ�
                  PERFORM UNTIL (�{�L�|���҃R�[�h   NOT = ���҃R�[�h�v�q  ) OR
                                (�{�L�|�{�p�a��N�� NOT = �{�p�a��N���v�q) OR
                                (�I���t���O�Q           = "YES"           )
                      IF (�{�L�|�����{�Ë敪  (���ʂb�m�s) NOT = ZERO) OR
                         (�{�L�|㪖@�敪      (���ʂb�m�s) NOT = ZERO) OR
                         (�{�L�|�d�Ë敪      (���ʂb�m�s) NOT = ZERO) OR
                         (�{�L�|��×������敪(���ʂb�m�s) NOT = ZERO) OR
                         (�{�L�|�������q�敪  (���ʂb�m�s) NOT = ZERO) OR
                         (�{�L�|���񋟋敪  (���ʂb�m�s) NOT = ZERO)
                          MOVE �{�L�|�{�p�N TO �J�n�N�v(���ʂb�m�s)
                          MOVE �{�L�|�{�p�� TO �J�n���v(���ʂb�m�s)
                          MOVE �{�L�|�{�p�� TO �J�n���v(���ʂb�m�s)
                          MOVE "YES" TO �I���t���O�Q
                      END-IF
                      PERFORM �{�p�L�^�e�Ǎ�
                  END-PERFORM
               END-IF
           END-PERFORM.
037520*================================================================*
      */20230817
028370*================================================================*
028380 ��������擾 SECTION.
028390*
028400****************************************************
028410* �A���f�[�^����ی��҃}�X�^��萿������擾����B *
028420* ���s�|��������敪=1�̏ꍇ������}�X�^���g�p   *
028430* �� ������...... �����於�̂v�Ɋi�[               *
028440****************************************************
028450     MOVE ������ʂv�q           TO �s�|������.
028460     MOVE ��p���S�Ҕԍ������v�q TO �s�|�s�����ԍ�.
028470*
028480     READ �s�����}�X�^
028490     INVALID KEY
028500         MOVE SPACE              TO �����於�̂v
028510     NOT INVALID KEY
028520         IF �s�|������敪 = 1
028530             MOVE ������ʂv�q           TO ����|�ی����
028540             MOVE ��p���S�Ҕԍ������v�q TO ����|�ی��Ҕԍ�
028550             READ ������}�X�^
028560             INVALID KEY
028570                 MOVE SPACE        TO �����於�̂v
028580             NOT INVALID KEY
028590                 MOVE ����|�ی��Җ���  TO �����於�̂v
028600             END-READ
028610         ELSE
028620             MOVE �s�|�s��������  TO �����於�̂v
028630         END-IF
028640     END-READ.
028650*
           STRING �����於�̂v DELIMITED BY SPACE
                  "�@�a"       DELIMITED BY SIZE
                  INTO �����於�̂v
           END-STRING.
027590*================================================================*
041780******************************************************************
041790 END PROGRAM YAZ6421.
041800******************************************************************
