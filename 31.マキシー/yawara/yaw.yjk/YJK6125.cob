000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJK6125.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*      ���{�_�����ω�   ��ʃ��Z�v�g����i�_+����޳�ޔŁj        *
000100*         MED = YAW610 YJK6125P                                  *
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2012-07-24
000130 DATE-COMPILED.          2012-07-24
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
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
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
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
002580     COPY YJK6125P        OF  XMDLIB.
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
003630       05 �����������e�P�w�v           PIC X(80)  VALUE SPACE.
003640       05 �����������e�Q�w�v           PIC X(80)  VALUE SPACE.
003640       05 �����������e�R�w�v           PIC X(80)  VALUE SPACE.
003650       05 �����������e�S�w�v           PIC X(78)  VALUE SPACE.
003550*
003560*************
003570* ���ϔԍ��p
003580 01 ���ϘA�ԍ��W�c�v.
003590    03 ���ϘA�ԍ����v                  PIC X(14)  VALUE SPACE.
003600    03 ���ϘA�ԍ����m�v REDEFINES  ���ϘA�ԍ����v  PIC N(7).
003610    03 ���ϘA�ԍ��v                    PIC X(6)  VALUE SPACE.
003620    03 ���ϘA�ԍ��P�ʂv                PIC X(2)  VALUE SPACE.
003630    03 ���ϘA�ԍ��P�ʂm�v REDEFINES  ���ϘA�ԍ��P�ʂv  PIC N.
003640* ���q���ԍ��p
003650 01 ���q���ԍ��W�c�v.
003660    03 ���q���ԍ����v                  PIC X(8)  VALUE SPACE.
003670    03 ���q���ԍ����m�v REDEFINES  ���q���ԍ����v  PIC N(4).
003680    03 ���q���ԍ��v                    PIC X(6)  VALUE SPACE.
003690    03 ���q���ԍ��P�ʂv                PIC X(2)  VALUE SPACE.
003700    03 ���q���ԍ��P�ʂm�v REDEFINES  ���q���ԍ��P�ʂv  PIC N.
003710 01 �E�o�t���O                         PIC X(3)  VALUE SPACE.
003720*
003730* �ی��Ҕԍ�
003740 01 �ی��Ҕԍ���r�v                   PIC X(6)   VALUE SPACE.
003750*
003760** �O�������̂ݗp
003770 01 �����Č��t���O                     PIC X(3)  VALUE SPACE.
003780 01 �O���t���O                         PIC X(3)  VALUE SPACE.
003790*
003800 01 �v�Z�N�����v.
003810    03 �v�Z�a��v                      PIC 9(1)  VALUE ZERO.
003820    03 �v�Z�N�v                        PIC S9(2)  VALUE ZERO.
003830    03 �v�Z���v                        PIC S9(2)  VALUE ZERO.
003840    03 �v�Z���v                        PIC S9(2)  VALUE ZERO.
003850 01 �J�n�N�����Q�v.
003860    03 �J�n�a��Q�v                    PIC 9(1)  VALUE ZERO.
003870    03 �J�n�N�Q�v                      PIC 9(2)  VALUE ZERO.
003880    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
003890    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
003900    03 �J�n����N�v                    PIC S9(4) VALUE ZERO.
003910 01 �I���N�����Q�v.
003920    03 �I���a��Q�v                    PIC 9(1)  VALUE ZERO.
003930    03 �I���N�Q�v                      PIC 9(2)  VALUE ZERO.
003940    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
003950    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
003960    03 �I������N�v                    PIC S9(4) VALUE ZERO.
003970***
003980** ���������E�������R����敪�p
003990 01 ������������敪�v                 PIC 9 VALUE ZERO.
004000 01 �������R����敪�v                 PIC 9 VALUE ZERO.
004010*
004020** ���Z���i�̓��t�敪�p (0:�ŏI�ʉ@���A1:�������A9:�󎚂Ȃ�)
004030 01 ���Z�v�g���t�敪�v                 PIC 9 VALUE ZERO.
004040 01 ���Z�v�g���ғ��t�敪�v             PIC 9 VALUE ZERO.
004050*
004060** �������p
004070 01 �{�p����N�v                       PIC 9(4)  VALUE ZERO.
004080 01 ���v                               PIC 9(3)  VALUE ZERO.
004090 01 �]�v                               PIC 9(3)  VALUE ZERO.
004100*
004110** �}�Ԕ���p
004120 01 �J�n�f�Ó��蓮�敪�v               PIC 9    VALUE ZERO.
004130*
004140*
004150** �������Z�܂Ƃߗp
004160 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
004170 01 ������ʗ��̂v                     PIC N(4)  VALUE SPACE.
004180 01 ������ʗ��̂v�Q                   PIC N(4)  VALUE SPACE.
004190*
004200** ���Z�E�v�p( N(38)�Œ�j /
004210 01 �����̌o�߂Q�v.
004220    03 �����̌o�ߍs�Q�v                PIC X(76) OCCURS 2 VALUE SPACE.
004230 01 �����̌o�߂Q�m�v REDEFINES �����̌o�߂Q�v.
004240    03 �����̌o�ߍs�Q�m�v              PIC N(38) OCCURS 2.
004250*
004260** ���Z�E�v�p( N(19)�Œ�j /
004270 01 �����̌o�߂v.
004280    03 �����̌o�ߍs�v                  PIC X(76) OCCURS 4 VALUE SPACE.
004290 01 �����̌o�߂m�v REDEFINES �����̌o�߂v.
004300    03 �����̌o�ߍs�m�v                PIC N(38) OCCURS 4.
004310*
004320* ������������敪
004330 01 ���Z������������敪�v             PIC 9    VALUE ZERO.
002580 01 ���Z�������R����敪�v             PIC 9    VALUE ZERO.
      *
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
       01 �������q�b�l                       PIC X(200) VALUE SPACE.
       01 �^����Âb�l                       PIC X(68)  VALUE SPACE.
004340*
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
004350****************
004360* �A�����ڑҔ� *
004370****************
004380*    ************
004390*    * ����L�[ *
004400*    ************
004410 01 �Ώۃf�[�^�v�q.
004420    03 �{�p�a��N���v�q.
004430       05 �{�p�a��v�q                  PIC 9(1)  VALUE ZERO.
004440       05 �{�p�N�v�q                    PIC 9(2)  VALUE ZERO.
004450       05 �{�p���v�q                    PIC 9(2)  VALUE ZERO.
004460    03 �{�p�a��̂v                   PIC N(2)  VALUE SPACE.
004470    03 �ی���ʂv�q                     PIC 9(2)  VALUE ZERO.
004480    03 �ی��Ҕԍ��v�q                   PIC X(10) VALUE SPACE.
004490    03 �����ʂv�q                     PIC 9(2)  VALUE ZERO.
004500    03 ��p���S�Ҕԍ��v�q               PIC X(10) VALUE SPACE.
004510    03 ������ʂv�q                     PIC 9(2)  VALUE ZERO.
004520    03 ��p���S�Ҕԍ������v�q           PIC X(10) VALUE SPACE.
004530    03 �{�l�Ƒ��敪�v�q                 PIC 9(1)  VALUE ZERO.
004540    03 ���҃J�i�v�q                     PIC X(50) VALUE SPACE.
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
         03 ���������k���v�q              PIC 9(4)    VALUE ZERO.
004680   03 �Č����v�q                    PIC 9(5)    VALUE ZERO.
004690   03 ���Âv�q.
004700      05 ���Ë����v�q               PIC 9(2)V9  VALUE ZERO.
004710      05 ���É񐔂v�q               PIC 9(2)    VALUE ZERO.
004720      05 ���×��v�q                 PIC 9(5)    VALUE ZERO.
004730      05 ���É��Z���v�q             PIC 9(5)    VALUE ZERO.
004740   03 �������q���Z���v�q            PIC 9(5)    VALUE ZERO.
004750   03 �{�p���񋟗��v�q            PIC 9(5)    VALUE ZERO.
004760   03 ���v�v�q                      PIC 9(7)    VALUE ZERO.
004770   03 �ꕔ���S���v�q                PIC 9(6)    VALUE ZERO.
004780   03 �������z�v�q                  PIC 9(7)    VALUE ZERO.
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
007550       05 �󗝔N�v                     PIC 9(2)   VALUE ZERO.
007560       05 �󗝌��v                     PIC 9(2)   VALUE ZERO.
007570       05 �󗝓��v                     PIC 9(2)   VALUE ZERO.
007580    03 �ŏI�ʉ@�N�����v.
007590       05 �ŏI�ʉ@�N�v                 PIC 9(2)   VALUE ZERO.
007600       05 �ŏI�ʉ@���v                 PIC 9(2)   VALUE ZERO.
007610       05 �ŏI�ʉ@���v                 PIC 9(2)   VALUE ZERO.
007620    03 �_���t�N�����v.
007630       05 �_���t�N�v                   PIC 9(2)   VALUE ZERO.
007640       05 �_���t���v                   PIC 9(2)   VALUE ZERO.
007650       05 �_���t���v                   PIC 9(2)   VALUE ZERO.
007660    03 ���҈ϔC�N�����v.
007670       05 ���҈ϔC�N�v                 PIC 9(2)   VALUE ZERO.
007680       05 ���҈ϔC���v                 PIC 9(2)   VALUE ZERO.
007690       05 ���҈ϔC���v                 PIC 9(2)   VALUE ZERO.
007700    03 �������v.
007710        05 ������s���v.
007720           07 ������s���P�v         PIC X(12)  VALUE SPACE.
007730           07 ������s���Q�v         PIC X(12)  VALUE SPACE.
007740           07 FILLER                   PIC X(16)  VALUE SPACE.
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
007890**************
007900* ��f�ҏ�� *
007910**************
007920 01 ��f�ҏ��v.
      */�����C��/20190426
          03 �{�p�a��v                      PIC 9(1)   VALUE ZERO.
007930    03 �{�p�N���v.
007940       05 �{�p�N�v                     PIC 9(2)   VALUE ZERO.
007950       05 �{�p���v                     PIC 9(2)   VALUE ZERO.
007960*    03 �L���v                          PIC N(12)  VALUE SPACE.
007570    03 �L���v.
007580       05 ����L���v                   PIC N(12)  VALUE SPACE.
          03 �L���ԍ��v.
             05 �L���ԍ��w�v                 PIC X(40) VALUE SPACE.
008770    03 �ԍ��v.
008780       05 ����ԍ��v                   PIC X(15)  VALUE SPACE.
008790       05 FILLER                       PIC X(15)  VALUE SPACE.
007970*    03 �ԍ��v                          PIC X(30)  VALUE SPACE.
007980    03 �ی��Ҕԍ��v�s.
007990       05 ���ʂR                       PIC X(1)   VALUE "[".
008000       05 �ی��Ҕԍ��v�o               PIC X(8)   VALUE SPACE.
008010       05 ���ʂS                       PIC X(1)   VALUE "]".
008020    03 �ی��Ҕԍ��v.
008030       05 ����ی��Ҕԍ��v             PIC X(8)   VALUE SPACE.
008040       05 FILLER                       PIC X(2)   VALUE SPACE.
008050    03 �s�����ԍ��v.
008060       05 ����s�����ԍ��v             PIC X(8)   VALUE SPACE.
008070       05 FILLER                       PIC X(2)   VALUE SPACE.
008080*    03 �󋋎Ҕԍ��v.
008090*       05 ����󋋎Ҕԍ��v             PIC X(7)   VALUE SPACE.
008100*       05 FILLER                       PIC X(13).
          03 �󋋎Ҕԍ��v.
             05 ����󋋎Ҕԍ��v             PIC X(7)  VALUE SPACE.
             05 ����󋋎Ҕԍ��Q�v           PIC X(8)  VALUE SPACE.
008110    03 �����於�̂v.
008120       05 �����於�̂P�v               PIC X(54)  VALUE SPACE.
008130       05 �����於�̂Q�v               PIC X(32)  VALUE SPACE.
008140    03 �����於�̂v�s.
008150       05 �����於�̂P�v�s             PIC X(40)  VALUE SPACE.
008160       05 �����於�̂Q�v�s             PIC X(30)  VALUE SPACE.
008170    03 �ی���ʂv                      PIC 9(2)   VALUE ZERO.
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
008180    03 ��ی��ҏ��v.
008190       05 ��ی��҃J�i�v               PIC X(50)  VALUE SPACE.
008200       05 ��ی��Ҏ����v               PIC X(50)  VALUE SPACE.
008210       05 �X�֔ԍ��v.
008220          07 �X�֔ԍ��P�v              PIC X(3)   VALUE SPACE.
008230          07 �X�֔ԍ��Q�v              PIC X(4)   VALUE SPACE.
008240       05 ��ی��ҏZ���v.
008250          07 ��ی��ҏZ���P�v          PIC X(50)  VALUE SPACE.
008260          07 ��ی��ҏZ���Q�v          PIC X(50)  VALUE SPACE.
008990       05 �d�b�ԍ��v                   PIC X(35)  VALUE SPACE.
008270    03 ���ҏ��v.
008280       05 ���҃J�i�v                   PIC X(50)  VALUE SPACE.
008290       05 ���Ҏ����v                   PIC X(50)  VALUE SPACE.
008300       05 ���ʃ`�F�b�N�v.
008310          07 �j�`�F�b�N�v              PIC N(1)  VALUE SPACE.
008320          07 ���`�F�b�N�v              PIC N(1)  VALUE SPACE.
008330       05 ���Ґ��ʂv.
008340          07 ���ʂv                    PIC N(1)  VALUE SPACE.
008350       05 �a��`�F�b�N�v.
008360          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008370          07 �吳�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008380          07 ���a�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008390          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008400          07 �����v                    PIC N(2)  VALUE SPACE.
      */�����C��/������20190426
008210          07 �ߘa�`�F�b�N�v            PIC N(1)  VALUE SPACE.
                07 �ߘa�b�l�v                PIC X(4)  VALUE SPACE.
009110*          07 �����v                    PIC N(2)  VALUE SPACE.
      */�����C��/������20190426
008410       05 ���ҔN�v                     PIC 9(2)  VALUE ZERO.
008420       05 ���Ҍ��v                     PIC 9(2)  VALUE ZERO.
008430       05 ���ғ��v                     PIC 9(2)  VALUE ZERO.
008440       05 �����v.
008450          07 ��������v                PIC N(4)  VALUE SPACE.
008460          07 FILLER                    PIC X(4)  VALUE SPACE.
008470*       05 �{�l�`�F�b�N�v               PIC N(1)  VALUE SPACE.
008480*       05 �Ƒ��`�F�b�N�v               PIC N(1)  VALUE SPACE.
008490*
008500*       05 ���������v                   PIC N(40) OCCURS 29 VALUE SPACE.
      */���p�Ή�/110421
             05 ���������v OCCURS 29.
                07 ���������w�v              PIC X(80)  VALUE SPACE.
008510*
008520    03 �ی���ʖ��̂v                  PIC N(2)  VALUE SPACE.
008530    03 ������v                        PIC N(1)  VALUE SPACE.
008540    03 ���ʃR�����g�v                  PIC X(16) VALUE SPACE.
008550*    03 �Еۃ`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008560*    03 �D���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008570*    03 �g���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008580*    03 ���q�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008590*    03 ���σ`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008600*    03 ���ۃ`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008610*    03 �ސE�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008620*    03 �V�l�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008630*    03 ����`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008640*    03 �R�΃`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008650*    03 �S�P�V�`�F�b�N�v                PIC N(1)  VALUE SPACE.
008660*    03 ��Q�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008670*    03 �픚�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008680*    03 ��q�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008690*    03 �����`�F�b�N�v                  PIC N(1)  VALUE SPACE.
      *    03 �q�`�F�b�N�v                    PIC N(1)  VALUE SPACE.
      *    03 �q�v                            PIC N(1)  VALUE SPACE.
008700*
008710    03 �P�O���`�F�b�N�v                PIC N(1)  VALUE SPACE.
008720    03 �X���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008730    03 �W���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008740    03 �V���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008750    03 ���S���v                        PIC 9(3)  VALUE ZERO.
008760*
008770****************
008780* �����f�[�^�e *
008790****************
008800 01 �������v.
008810    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
008820    03 ���ʏ��v  OCCURS   9.
008830       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
008840       05 ���ʃR�[�h�v.
008850          07 ������ʂv                PIC 9(2)  VALUE ZERO.
008860          07 ���ʂv                    PIC 9(2)  VALUE ZERO.
008870          07 ���E�敪�v                PIC 9(1)  VALUE ZERO.
008880          07 �����ʒu�ԍ��v            PIC 9(2)  VALUE ZERO.
008890       05 �������v                     PIC N(18) VALUE SPACE.
008900       05 �����N�����v.
008910          07 �����N�v                  PIC 9(2)  VALUE ZERO.
008920          07 �������v                  PIC 9(2)  VALUE ZERO.
008930          07 �������v                  PIC 9(2)  VALUE ZERO.
008940       05 �����N�����v.
008950          07 �����N�v                  PIC 9(2)  VALUE ZERO.
008960          07 �������v                  PIC 9(2)  VALUE ZERO.
008970          07 �������v                  PIC 9(2)  VALUE ZERO.
008980       05 �J�n�N�����v.
008990          07 �J�n�N�v                  PIC 9(2)  VALUE ZERO.
009000          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
009010          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
009020       05 �I���N�����v.
009030          07 �I���N�v                  PIC 9(2)  VALUE ZERO.
009040          07 �I�����v                  PIC 9(2)  VALUE ZERO.
009050          07 �I�����v                  PIC 9(2)  VALUE ZERO.
009060       05 �������v                     PIC 9(2)  VALUE ZERO.
009070       05 �]�A�敪�v                   PIC 9(1)  VALUE ZERO.
009080       05 �]�A�敪�`�F�b�N�v.
009090          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
009100          07 ���~�`�F�b�N�v            PIC N(1)  VALUE SPACE.
009110          07 �]��`�F�b�N�v            PIC N(1)  VALUE SPACE.
009120       05 �J�n�N�����擾�t���O         PIC X(3)  VALUE SPACE.
009130       05 ���ʋ�؂v                   PIC X(1)  VALUE SPACE.
009140       05 �o�ߗ��̂v.
009150          07 ����o�ߗ��̂v            PIC N(6)  VALUE SPACE.
009160          07 FILLER                    PIC X(2)  VALUE SPACE.
009170    03 �o�ߕ��ʂv                      PIC N(1)  VALUE SPACE.
009180    03 �V�K�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
009190    03 �p���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
009200*
009210************
009220* ������� *
009230************
009240 01 �������v.
009250    03 �������Z�v.
009260       05 ���ԊO�`�F�b�N�v                PIC N(1) VALUE SPACE.
009270       05 �x���`�F�b�N�v                  PIC N(1) VALUE SPACE.
009280       05 �[��`�F�b�N�v                  PIC N(1) VALUE SPACE.
009290    03 ���É��Z�v.
009300       05 ��ԃ`�F�b�N�v                  PIC N(1) VALUE SPACE.
009310       05 ��H�`�F�b�N�v                  PIC N(1) VALUE SPACE.
009320       05 �\���J��`�F�b�N�v              PIC N(1) VALUE SPACE.
009330    03 �������q�`�F�b�N�v.
009340       05 ��`�F�b�N�v                    PIC N(1) VALUE SPACE.
009350       05 ���`�F�b�N�v                    PIC N(1) VALUE SPACE.
009360       05 ���`�F�b�N�v                    PIC N(1) VALUE SPACE.
009370    03 ���v�v                             PIC 9(7) VALUE ZERO.
009380    03 ���񏈒u�����v�v                   PIC 9(6) VALUE ZERO.
009390    03 ���񏈒u���`�F�b�N�v.
009400       05 �������`�F�b�N�v                PIC N(1) VALUE SPACE.
009410       05 �Œ藿�`�F�b�N�v                PIC N(1) VALUE SPACE.
009420       05 �{�×��`�F�b�N�v                PIC N(1) VALUE SPACE.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
          03 �����񐔂v                         PIC 9(2)  VALUE ZERO.
          03 �^�����v                           PIC 9(4)  VALUE ZERO.
009430************
009440* ���l��� *
009450************
009460 01 ���l���v.
009470    03 �K�p�P�v                        PIC N(38) VALUE SPACE.
009480    03 �K�p�P�v�q REDEFINES �K�p�P�v.
009490       05 �K�p�P�P�v                   PIC N(19).
009500       05 �K�p�P�Q�v                   PIC N(19).
009510*
009520    03 �K�p�Q�v                        PIC N(38) VALUE SPACE.
009530    03 �K�p�Q�v�q REDEFINES �K�p�Q�v.
009540       05 �K�p�Q�P�v                   PIC N(19).
009550       05 �K�p�Q�Q�v                   PIC N(19).
009560*    03 �K�p�R�v                        PIC N(38) VALUE SPACE.
009570*    03 �K�p�S�v                        PIC N(38) VALUE SPACE.
009580*
009590    03 �o�߃R�����g�v                  PIC N(60) VALUE SPACE.
009600*
003720*--- ���S���t�����p ---*
003730 01 ���S�����v                         PIC 9(2)  VALUE ZERO.
003740 01 ���t�����v                         PIC 9(2)  VALUE ZERO.
009610*****************
009620* ���Z�v�g���я� *
009630*****************
009640 01 ���ԌŒ�v                         PIC X(10) VALUE SPACE.
009650 01 ���Ԃv                             PIC 9(4) VALUE ZERO.
009660*
       01 �E�v�{�p���v                       PIC X(100) VALUE SPACE.
       01 �{�p���v.
          03 �{�p���Q�v                      PIC X(1)  VALUE SPACE.
          03 �{�p���P�v                      PIC X(1)  VALUE SPACE.
009670*******************************************************************
009680 01 �������.
009690     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
009700     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
009710     03 ������ʂo                     PIC X(2) VALUE SPACE.
009720     03 �g������o.
009730         05 �[������o.
009740             07 �ړ������o             PIC X(1) VALUE SPACE.
009750             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
009760         05 �ڍא���o                 PIC X(2) VALUE SPACE.
009770     03 �ʒm���o                     PIC X(2) VALUE SPACE.
009780     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
009790*
009800 01 �v�Z�@����N�v                     PIC 9(2) VALUE ZERO.
009810* ���t�v�n�q�j
009820 01 �a��I���N�v                       PIC 9(4) VALUE ZERO.
009830 01 �v�Z�@����.
009840    03 �v�Z�@����N                    PIC 9(4) VALUE ZERO.
009850    03 �v�Z�@�����                  PIC 9(4) VALUE ZERO.
009860 01 �v�Z�@����q REDEFINES �v�Z�@����.
009870    03 �v�Z�@���I                      PIC 9(2).
009880    03 �v�Z�@���t                      PIC 9(6).
009890    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
009900       05 �v�Z�@�N��                   PIC 9(4).
009910       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
009920         07 �v�Z�@�N                   PIC 9(2).
009930         07 �v�Z�@��                   PIC 9(2).
009940       05 �v�Z�@��                     PIC 9(2).
009950*
      * C �A�g�p
       01  �����P�v        PIC X(4096).
       01  �����Q�v        PIC X(512).
       01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
      *
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
009960******************************************************************
009970*                          �A������                              *
009980******************************************************************
009990**  ��ʓ��̓f�[�^
010000 01 �A���|���̓f�[�^�ϔC��� IS EXTERNAL.
010010    03 �A���|�ϔC���                     PIC 9.
       01 �A���|���̓f�[�^�d�b��� IS EXTERNAL.
          03 �A���|�d�b���                     PIC 9.
009190*
       01 �A���|�v���r���[ IS EXTERNAL.
          03 �A���|�v���r���[�敪          PIC 9.
010020*
010030** �R�J����������
010040 01 �A���ԁ|�L�[ IS EXTERNAL.
010050    03 �A���ԁ|�{�p�N��.
010060       05 �A���ԁ|�{�p�a��               PIC 9.
010070       05 �A���ԁ|�{�p�N                 PIC 9(2).
010080       05 �A���ԁ|�{�p��                 PIC 9(2).
010090    03  �A���ԁ|���҃R�[�h.
010100       05 �A���ԁ|���Ҕԍ�               PIC 9(6).
010110       05 �A���ԁ|�}��                   PIC X.
010120    03 �A���ԁ|�Ώۃt���O                PIC X(3).
010130    03 �A���ԁ|���Ԍ��v.
010140       05 �A���ԁ|���Ԃv                 PIC 9(2) OCCURS 9.
010150************
010160* ����L�[ *
010170************
010190*
010200 01 �A����|�Ώۃf�[�^ IS EXTERNAL.
010210    03 �A����|�{�p�N����.
010220       05 �A����|�{�p�a��                  PIC 9(1).
010230       05 �A����|�{�p�N                    PIC 9(2).
010240       05 �A����|�{�p��                    PIC 9(2).
010250    03 �A����|���҃R�[�h.
010260       05 �A����|���Ҕԍ�                  PIC 9(6).
010270       05 �A����|�}��                      PIC X(1).
010280    03 �A����|�ی����                     PIC 9(2).
010290    03 �A����|�ی��Ҕԍ�                   PIC X(10).
010300    03 �A����|������                     PIC 9(2).
010310    03 �A����|��p���S�Ҕԍ�               PIC X(10).
010320    03 �A����|�������                     PIC 9(2).
010330    03 �A����|��p���S�Ҕԍ�����           PIC X(10).
010340    03 �A����|���҃J�i                     PIC X(20).
010350    03 �A����|�{�l�Ƒ��敪                 PIC 9(1).
013780*
013790 01 �A���|�L�[ IS EXTERNAL.
013800    03 �A���|�ی����                  PIC 9(2).
013810*
013820************************
013830* �������R���Z�b�g     *
013840************************
013850 01 �A�����|�L�[ IS EXTERNAL.
013860    03 �A�����|�{�p�N��.
013870       05 �A�����|�{�p�a��               PIC 9.
013880       05 �A�����|�{�p�N                 PIC 9(2).
013890       05 �A�����|�{�p��                 PIC 9(2).
013900    03  �A�����|���҃R�[�h.
013910       05 �A�����|���Ҕԍ�               PIC 9(6).
013920       05 �A�����|�}��                   PIC X.
013930    03 �A�����|������                    PIC 9(2).
013940    03 �A�����|���R��                    PIC N(63) OCCURS 15.
013950*
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
014370*
013960************************
013970* �������Z�܂Ƃ�
013980************************
013990 01 �A���Z�܂Ƃ߁|�L�[ IS EXTERNAL.
014000    03 �A���Z�܂Ƃ߁|�{�p�a��N��.
014010       05 �A���Z�܂Ƃ߁|�{�p�a��               PIC 9.
014020       05 �A���Z�܂Ƃ߁|�{�p�N��.
014030          07 �A���Z�܂Ƃ߁|�{�p�N              PIC 9(2).
014040          07 �A���Z�܂Ƃ߁|�{�p��              PIC 9(2).
014050    03 �A���Z�܂Ƃ߁|���҃R�[�h.
014060       05 �A���Z�܂Ƃ߁|���Ҕԍ�               PIC 9(6).
014070       05 �A���Z�܂Ƃ߁|�}��                   PIC X(1).
014080**-------------------------------------------------------**
014090*   1:�������Z�v�g�Ȃ��̖{�̂܂Ƃ߂̔���
014100*   2:���l�E���p�̎Еۏ������Z���̔���
014110    03 �A���Z�܂Ƃ߁|����敪                  PIC 9.
014120**-------------------------------------------------------**
014130*  / OUT /�@ 0:�ΏۊO�A1:�Ώ�
014140    03 �A���Z�܂Ƃ߁|���茋��                  PIC 9.
014150**
014160*
014170*************
014180* ��������
014190*************
014200 01 �A�������́|�L�[ IS EXTERNAL.
014210    03 �A�������́|�������             PIC 9(2).
014220    03 �A�������́|��p���S�Ҕԍ�����   PIC X(10).
014230*   / OUT /
014240    03 �A�������́|���̏W�c.
014250       05 �A�������́|�P����            PIC N.
014260       05 �A�������́|����              PIC N(4).
014270       05 �A�������́|��������          PIC N(10).
014280*
014290* ���S���擾�p14/10�`
014300 01 �A���|���S���擾�L�[ IS EXTERNAL.
014310    03 �A���|�{�p�a��N��.
014320       05 �A���|�{�p�a��               PIC 9.
014330       05 �A���|�{�p�N��.
014340          07 �A���|�{�p�N              PIC 9(2).
014350          07 �A���|�{�p��              PIC 9(2).
014360    03 �A���|���҃R�[�h.
014370       05 �A���|���Ҕԍ�               PIC 9(6).
014380       05 �A���|�}��                   PIC X.
014390    03 �A���|���ە��S��                PIC 9(3).
014400    03 �A���|���ۖ{�̕��S��            PIC 9(3).
014410    03 �A���|���ە��S��                PIC 9(3).
014420    03 �A���|�Q�V�V���S��              PIC 9(3).
014430    03 �A���|�������S��                PIC 9(3).
014440    03 �A���|���ʗp���S��              PIC 9(3).
014450*
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
006490*
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
      * 
014460******************************************************************
014470*                      PROCEDURE  DIVISION                       *
014480******************************************************************
014490 PROCEDURE               DIVISION.
014500************
014510*           *
014520* ��������   *
014530*           *
014540************
002570     PERFORM �v�����^�t�@�C���쐬.
014550     PERFORM ������.
014560     PERFORM ������擾.
014570************
014580*           *
014590* �又��     *
014600*           *
014610************
014620* ���
014630     PERFORM �A�����ڑҔ�.
014640     PERFORM ����Z�b�g.
014650     PERFORM �������.
014660************
014670*           *
014680* �I������   *
014690*           *
014700************
014710     PERFORM ��f�҈���敪�X�V.
014720     PERFORM �I������.
014730*     PERFORM �x������.
014740     MOVE ZERO  TO PROGRAM-STATUS.
014750     EXIT PROGRAM.
014760*
014770*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YJK6125"             TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪  TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014780*================================================================*
014790 ������ SECTION.
014800*
014810     PERFORM �t�@�C���I�[�v��.
014820*    /* ���ݓ��t�擾 */
014830     ACCEPT �v�Z�@���t FROM DATE.
014840*    /* 1980�`2079�N�̊ԂŐݒ� */
014850     IF ( �v�Z�@�N > 80 )
014860         MOVE 19 TO �v�Z�@���I
014870     ELSE
014880         MOVE 20 TO �v�Z�@���I
014890     END-IF.
014900     PERFORM �J�����g�����擾.
014910     PERFORM �a��I���N�擾.
014920     COMPUTE �v�Z�@����N�v = �v�Z�@����N - 1988.
014930*================================================================*
014940 �J�����g�����擾 SECTION.
014950*
014960     MOVE ZEROS TO ���|����敪.
014970     READ ������}�X�^
014980     NOT INVALID KEY
014990         MOVE ���|�J�����g����         TO �J�����g�����v
015000         MOVE ���|���Z������������敪 TO ������������敪�v
015010         MOVE ���|���Z�������R����敪 TO �������R����敪�v
015020         MOVE ���|���Z�v�g���t�敪     TO ���Z�v�g���t�敪�v
015030         MOVE ���|���Z�v�g���ғ��t�敪 TO ���Z�v�g���ғ��t�敪�v
015040     END-READ.
015050*
015060*================================================================*
015070 �a��I���N�擾 SECTION.
015080*
015090*     DISPLAY NC"�J�����g�����v"  �J�����g�����v UPON MSGBOX.
015100     MOVE �J�����g�����v TO ���|�����敪.
015110     READ �����}�X�^
015120     INVALID KEY
015130         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
015140         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015150                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015160         ACCEPT  �L�[���� FROM CONS
015170         PERFORM �I������
015180         EXIT PROGRAM
015190     NOT INVALID KEY
015200         COMPUTE �O�a��v = �J�����g�����v - 1
015210         MOVE �O�a��v TO ���|�����敪
015220         READ �����}�X�^
015230         INVALID KEY
015240             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
015250             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015260                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015270             ACCEPT  �L�[���� FROM CONS
015280             PERFORM �I������
015290             EXIT PROGRAM
015300         NOT INVALID KEY
015310             MOVE ���|�I������N TO �a��I���N�v
015320         END-READ
015330     END-READ.
015340*
015350*================================================================*
015360 �t�@�C���I�[�v�� SECTION.
015370*
015380     OPEN INPUT   �ی��҃}�X�^
015390         MOVE NC"�ی���" TO �t�@�C����.
015400         PERFORM �I�[�v���`�F�b�N.
015410     OPEN INPUT   �����}�X�^
015420         MOVE NC"����" TO �t�@�C����.
015430         PERFORM �I�[�v���`�F�b�N.
015440     OPEN INPUT   ���̃}�X�^
015450         MOVE NC"����" TO �t�@�C����.
015460         PERFORM �I�[�v���`�F�b�N.
015500     OPEN INPUT   ������}�X�^
015510         MOVE NC"������" TO �t�@�C����.
015520         PERFORM �I�[�v���`�F�b�N.
015530     OPEN INPUT   �{�p�����}�X�^
015540         MOVE NC"�{��" TO �t�@�C����.
015550         PERFORM �I�[�v���`�F�b�N.
015590     OPEN INPUT   �o�߃}�X�^
015600         MOVE NC"�o��" TO �t�@�C����.
015610         PERFORM �I�[�v���`�F�b�N.
015620     OPEN INPUT   �{�p�L�^�e.
015630         MOVE NC"�{�L�e" TO �t�@�C����.
015640         PERFORM �I�[�v���`�F�b�N.
015650     OPEN INPUT   �����f�[�^�e.
015660         MOVE NC"����" TO �t�@�C����.
015670         PERFORM �I�[�v���`�F�b�N.
015680     OPEN INPUT   ���������e.
015690         MOVE NC"��������" TO �t�@�C����.
015700         PERFORM �I�[�v���`�F�b�N.
015710     OPEN INPUT   �h�c�Ǘ��}�X�^
015720         MOVE NC"�h�c" TO �t�@�C����.
015730         PERFORM �I�[�v���`�F�b�N.
015740     OPEN INPUT �s�����}�X�^.
015750         MOVE NC"�s����" TO �t�@�C����.
015760         PERFORM �I�[�v���`�F�b�N.
007560     OPEN INPUT   ���Z�v�g�e
007570         MOVE NC"���Z" TO �t�@�C����.
007580         PERFORM �I�[�v���`�F�b�N.
015800     OPEN INPUT  ��ƃt�@�C���S.
015810         MOVE NC"��S" TO �t�@�C����.
015820         PERFORM �I�[�v���`�F�b�N.
015830     OPEN I-O   ��f�ҏ��e.
015840         MOVE NC"���" TO �t�@�C����.
015850         PERFORM �I�[�v���`�F�b�N.
015860     OPEN I-O   ����t�@�C��
015870         PERFORM �G���[�����o.
015880*================================================================*
015890 �I�[�v���`�F�b�N SECTION.
015900*
015910     IF ( ��ԃL�[  NOT =  "00" )
015920         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
015930         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
015940         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015950                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015960         ACCEPT  �L�[���� FROM CONS
015970         PERFORM �t�@�C����
015980         EXIT PROGRAM.
015990*================================================================*
016000 ������擾 SECTION.
016010*
016020     MOVE ZERO TO ���|����敪
016030     READ ������}�X�^
016040     NOT INVALID KEY
016050         MOVE ���|�ő�o�^���ʐ� TO �ő�o�^���v
016060         MOVE ���|�����A���o�^   TO �����A���o�^�v
016070         MOVE ���|�x����       TO �x���񐔂v
016080     END-READ.
016090*
016100*================================================================*
016110 �x������ SECTION.
016120*
016130     PERFORM VARYING �x���b�m�s FROM 1 BY 1
016140                                UNTIL �x���b�m�s > �x���񐔂v
016150         MOVE SPACE TO �x���t���O
016160     END-PERFORM.
016170*
016180*================================================================*
016190 �A�����ڑҔ� SECTION.
016200*
016210     MOVE �A����|�{�p�a��           TO �{�p�a��v�q.
016220     MOVE �A����|�{�p�N             TO �{�p�N�v�q.
016230     MOVE �A����|�{�p��             TO �{�p���v�q.
016240     MOVE �A����|�ی����           TO �ی���ʂv�q.
016250     MOVE �A����|�ی��Ҕԍ�         TO �ی��Ҕԍ��v�q.
016260     MOVE �A����|������           TO �����ʂv�q.
016270     MOVE �A����|��p���S�Ҕԍ�     TO ��p���S�Ҕԍ��v�q.
016280     MOVE �A����|�������           TO ������ʂv�q.
016290     MOVE �A����|��p���S�Ҕԍ����� TO ��p���S�Ҕԍ������v�q.
016300     MOVE �A����|�{�l�Ƒ��敪       TO �{�l�Ƒ��敪�v�q.
016310     MOVE �A����|���҃J�i           TO ���҃J�i�v�q.
016320     MOVE �A����|���Ҕԍ�           TO ���Ҕԍ��v�q.
016330     MOVE �A����|�}��               TO �}�Ԃv�q.
016340*================================================================*
016350 ����Z�b�g SECTION.
016360*
016370     PERFORM ���ڏ�����.
           PERFORM ��{���擾.
016380     PERFORM �{�p�����擾.
016390     PERFORM ��������擾.
016400     PERFORM ��f�ҏ��擾.
016410     PERFORM �����f�[�^�擾.
016420     PERFORM �������擾.
016430     PERFORM �{�p�L�^�擾.
016440     PERFORM ���Z�v�g���я��擾.
016460***     PERFORM �������ȑO�̃f�[�^����.
016470     PERFORM �������Z�����擾.
016480*     PERFORM ������擾.
016490     PERFORM �ϔC�N�����擾.
           PERFORM �{�p���擾.
016500*
016791*-----------------------------------------------*
016800     IF ( ������������敪�v  NOT = 1 ) AND ( ���Z������������敪�v NOT = 1 )
016813        IF ( ������������敪�v = 3 OR 4 )
016815           PERFORM ������������Ώ۔��菈��
016817        ELSE
016820           PERFORM ���������擾
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
016550*
015930* �����Ώۂ̎��̂�
015940     IF ( �������R����敪�v NOT = 1 )
               MOVE �������R����敪�v TO �A�E���|�����敪
016000     END-IF.
016620*
016630********************
016640* ��f�ҏ��Z�b�g *
016650********************
      */�����C��/������20190426
037370     IF �{�p�a��v > 4
              MOVE �{�p�a��v         TO ���|�����敪
037380        READ �����}�X�^
037390        NOT INVALID KEY
037400            MOVE ���|��������   TO �{�p�a��
037410        END-READ
              MOVE "===="             TO �{�p�a�����
           END-IF.
      */�����C��/������20190426
016660     MOVE �{�p�N�v            TO �{�p�N.
016670     MOVE �{�p���v            TO �{�p��.
016680*
015190     MOVE �Еۃ`�F�b�N�v     TO �Еۃ`�F�b�N.
015210     MOVE �g���`�F�b�N�v     TO �g���`�F�b�N.
015220     MOVE ���ۃ`�F�b�N�v     TO ���ۃ`�F�b�N.
           MOVE ���σ`�F�b�N�v     TO ���σ`�F�b�N.
           MOVE ���`�F�b�N�v       TO ���`�F�b�N.
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
016690*     MOVE �Еۃ`�F�b�N�v      TO �Еۃ`�F�b�N.
016700*     MOVE �D���`�F�b�N�v      TO �D���`�F�b�N.
016710*     MOVE �g���`�F�b�N�v      TO �g���`�F�b�N.
016720*     MOVE ���q�`�F�b�N�v      TO ���q�`�F�b�N.
016730*     MOVE ���σ`�F�b�N�v      TO ���σ`�F�b�N.
016740*     MOVE ���ۃ`�F�b�N�v      TO ���ۃ`�F�b�N.
016750*     MOVE �ސE�`�F�b�N�v      TO �ސE�`�F�b�N.
016760*     MOVE �V�l�`�F�b�N�v      TO �V�l�`�F�b�N.
016770*     MOVE �{�l�`�F�b�N�v      TO �{�l�`�F�b�N.
016780*     MOVE �Ƒ��`�F�b�N�v      TO �Ƒ��`�F�b�N.
016790**
016800*     MOVE ����`�F�b�N�v      TO ����`�F�b�N.
016810*     MOVE �R�΃`�F�b�N�v      TO �R�΃`�F�b�N.
016820*     MOVE �S�P�V�`�F�b�N�v    TO �S�P�V�`�F�b�N.
016830*     MOVE ��Q�`�F�b�N�v      TO ��Q�`�F�b�N.
016840*     MOVE �픚�`�F�b�N�v      TO �픚�`�F�b�N.
016850*     MOVE ��q�`�F�b�N�v      TO ��q�`�F�b�N.
016860*     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
      *     MOVE �q�`�F�b�N�v        TO �q�`�F�b�N�P.
      *     MOVE �q�v                TO �q�P.
016870*
016880*     IF ( �L���v(1:1) = NC"��" )
016890*        MOVE  SPACE    TO  �L��
016900*     ELSE
016910*        MOVE �L���v    TO  �L��
016920*     END-IF.
016930*     IF ( �ԍ��v(1:1) = "*"  ) OR
016940*        ( �ԍ��v(1:2) = "��" )
016950*        MOVE SPACE     TO  �ԍ�
016960*     ELSE
016970*        MOVE �ԍ��v    TO  �ԍ�
016980*     END-IF.
016990*     IF (�L�� NOT = SPACE) OR (�ԍ� NOT = SPACE)
017000*         MOVE "/"   TO  ��؂�
017010*     END-IF.
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
017020*
017030*     IF ( ����s�����ԍ��v(1:2) = "99" )
017040*         MOVE SPACE              TO �s�����ԍ�
017050*     ELSE
017060*         MOVE ����s�����ԍ��v   TO �s�����ԍ�
017070*     END-IF.
017080*
017090*     IF ( ����󋋎Ҕԍ��v(1:1) = "*"  ) OR
017100*        ( ����󋋎Ҕԍ��v(1:2) = "��" )
017110*        MOVE  SPACE              TO �󋋎Ҕԍ�
017120*     ELSE
017130*        MOVE �󋋎Ҕԍ��v        TO �󋋎Ҕԍ�
017140*     END-IF.
017150     MOVE ����ی��Ҕԍ��v    TO �ی��Ҕԍ�.
017160     MOVE "["                TO ���ʂR.
017170     MOVE "]"                TO ���ʂS.
017180     MOVE �ی��Ҕԍ��v�s      TO �ی��Ҕԍ��^�C�g��.
017190*     IF ( �����於�̂Q�v = SPACE )
017200*        MOVE �����於�̂v     TO �ی��Җ���
017210*     ELSE
017220*        MOVE �����於�̂P�v   TO �ی��Җ��̂P.
017230*        MOVE �����於�̂Q�v   TO �ی��Җ��̂Q.
              MOVE �����於�̂P�v�s TO �ی��Җ��̂P.
              MOVE �����於�̂Q�v�s TO �ی��Җ��̂Q.
017240*     END-IF.
017250***     MOVE ��ی��҃J�i�v      TO ��ی��҃J�i.
017280     MOVE ��ی��Ҏ����v      TO ��ی��Ҏ���
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
017340     MOVE ��ی��ҏZ���P�v    TO �Z���P.
017350     MOVE ��ی��ҏZ���Q�v    TO �Z���Q.
017280*     MOVE ��ی��Ҏ����v      TO ��f�Җ�.
017280     MOVE ��ی��Ҏ����v      TO ��ی��Ҏ����Q.
017360*     MOVE ���҃J�i�v          TO ���҃J�i.
017370     MOVE ���Ҏ����v          TO ���Ҏ���.
017380     MOVE �j�`�F�b�N�v        TO �j�`�F�b�N.
017390     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
017400***     MOVE ���ʂv               TO ����.
017410     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
017420     MOVE �吳�`�F�b�N�v      TO �吳�`�F�b�N.
017430     MOVE ���a�`�F�b�N�v      TO ���a�`�F�b�N.
017440     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
017450***     MOVE �����v              TO ����.
      */�����C��������/20190426
           MOVE �ߘa�b�l�v         TO �ߘa�b�l.
023070     MOVE �ߘa�`�F�b�N�v     TO �ߘa�`�F�b�N.
017390*     MOVE �����v              TO ���Ҙa��.
      */�����C��������/20190426
017460     MOVE ���ҔN�v            TO ���ҔN.
017470     MOVE ���Ҍ��v            TO ���Ҍ�.
017480     MOVE ���ғ��v            TO ���ғ�.
017490     MOVE ��������v          TO ����.
017500*     MOVE NC"�Ɩ��ЊQ�ʋ΍ЊQ���͑�O�ҍs�׈ȊO�̌����ɂ��B" TO ��������.
017510*     IF ���������v(1) NOT = SPACE
017520*         MOVE NC"�i���������j"  TO ���������Œ�
017530*     END-IF.
      *
017540     MOVE ���������v(1)       TO ���������P.
017550     MOVE ���������v(2)       TO ���������Q.
017560     MOVE ���������v(3)       TO ���������R.
017570     MOVE ���������v(4)       TO ���������S.
017580     MOVE ���������v(5)       TO ���������T.
017580     MOVE ���������v(6)       TO ���������U.
      *
017590*
017600***     MOVE ������v            TO ������.
017610     MOVE �ی���ʖ��̂v      TO �ی����.
017620*
017770********************
017780* �����f�[�^�Z�b�g *
017790********************
017800* �P���� *
017810**********
017820     MOVE �������v(1)       TO �������P.
017830     MOVE �����N�v(1)       TO �����N�P.
017840     MOVE �������v(1)       TO �������P.
017850     MOVE �������v(1)       TO �������P.
017860     MOVE �����N�v(1)       TO �����N�P.
017870     MOVE �������v(1)       TO �������P.
017880     MOVE �������v(1)       TO �������P.
017890     MOVE �J�n�N�v(1)       TO �J�n�N�P.
017900     MOVE �J�n���v(1)       TO �J�n���P.
017910     MOVE �J�n���v(1)       TO �J�n���P.
017920     MOVE �I���N�v(1)       TO �I���N�P.
017930     MOVE �I�����v(1)       TO �I�����P.
017940     MOVE �I�����v(1)       TO �I�����P.
017950     MOVE �������v(1)       TO �������P.
017960*     IF �������v(1) NOT = ZERO
017970*         MOVE NC"��"        TO ���P
017980*     END-IF.
017990     MOVE �����`�F�b�N�v(1) TO �����`�F�b�N�P.
018000     MOVE ���~�`�F�b�N�v(1) TO ���~�`�F�b�N�P.
018010     MOVE �]��`�F�b�N�v(1) TO �]��`�F�b�N�P.
018020**********
018030* �Q���� *
018040**********
018050     MOVE �������v(2)       TO �������Q.
018060     MOVE �����N�v(2)       TO �����N�Q.
018070     MOVE �������v(2)       TO �������Q.
018080     MOVE �������v(2)       TO �������Q.
018090     MOVE �����N�v(2)       TO �����N�Q.
018100     MOVE �������v(2)       TO �������Q.
018110     MOVE �������v(2)       TO �������Q.
018120     MOVE �J�n�N�v(2)       TO �J�n�N�Q.
018130     MOVE �J�n���v(2)       TO �J�n���Q.
018140     MOVE �J�n���v(2)       TO �J�n���Q.
018150     MOVE �I���N�v(2)       TO �I���N�Q.
018160     MOVE �I�����v(2)       TO �I�����Q.
018170     MOVE �I�����v(2)       TO �I�����Q.
018180     MOVE �������v(2)       TO �������Q.
018190*     IF �������v(2) NOT = ZERO
018200*         MOVE NC"��"        TO ���Q
018210*     END-IF.
018220     MOVE �����`�F�b�N�v(2) TO �����`�F�b�N�Q.
018230     MOVE ���~�`�F�b�N�v(2) TO ���~�`�F�b�N�Q.
018240     MOVE �]��`�F�b�N�v(2) TO �]��`�F�b�N�Q.
018250**********
018260* �R���� *
018270**********
018280     MOVE �������v(3)       TO �������R.
018290     MOVE �����N�v(3)       TO �����N�R.
018300     MOVE �������v(3)       TO �������R.
018310     MOVE �������v(3)       TO �������R.
018320     MOVE �����N�v(3)       TO �����N�R.
018330     MOVE �������v(3)       TO �������R.
018340     MOVE �������v(3)       TO �������R.
018350     MOVE �J�n�N�v(3)       TO �J�n�N�R.
018360     MOVE �J�n���v(3)       TO �J�n���R.
018370     MOVE �J�n���v(3)       TO �J�n���R.
018380     MOVE �I���N�v(3)       TO �I���N�R.
018390     MOVE �I�����v(3)       TO �I�����R.
018400     MOVE �I�����v(3)       TO �I�����R.
018410     MOVE �������v(3)       TO �������R.
018420*     IF �������v(3) NOT = ZERO
018430*         MOVE NC"��"        TO ���R
018440*     END-IF.
018450     MOVE �����`�F�b�N�v(3) TO �����`�F�b�N�R.
018460     MOVE ���~�`�F�b�N�v(3) TO ���~�`�F�b�N�R.
018470     MOVE �]��`�F�b�N�v(3) TO �]��`�F�b�N�R.
018480**********
018490* �S���� *
018500**********
018510     MOVE �������v(4)       TO �������S.
018520     MOVE �����N�v(4)       TO �����N�S.
018530     MOVE �������v(4)       TO �������S.
018540     MOVE �������v(4)       TO �������S.
018550     MOVE �����N�v(4)       TO �����N�S.
018560     MOVE �������v(4)       TO �������S.
018570     MOVE �������v(4)       TO �������S.
018580     MOVE �J�n�N�v(4)       TO �J�n�N�S.
018590     MOVE �J�n���v(4)       TO �J�n���S.
018600     MOVE �J�n���v(4)       TO �J�n���S.
018610     MOVE �I���N�v(4)       TO �I���N�S.
018620     MOVE �I�����v(4)       TO �I�����S.
018630     MOVE �I�����v(4)       TO �I�����S.
018640     MOVE �������v(4)       TO �������S.
018650*     IF �������v(4) NOT = ZERO
018660*         MOVE NC"��"        TO ���S
018670*     END-IF.
018680     MOVE �����`�F�b�N�v(4) TO �����`�F�b�N�S.
018690     MOVE ���~�`�F�b�N�v(4) TO ���~�`�F�b�N�S.
018700     MOVE �]��`�F�b�N�v(4) TO �]��`�F�b�N�S.
018710**********
018720* �T���� *
018730**********
018740     MOVE �������v(5)       TO �������T.
018750     MOVE �����N�v(5)       TO �����N�T.
018760     MOVE �������v(5)       TO �������T.
018770     MOVE �������v(5)       TO �������T.
018780     MOVE �����N�v(5)       TO �����N�T.
018790     MOVE �������v(5)       TO �������T.
018800     MOVE �������v(5)       TO �������T.
018810     MOVE �J�n�N�v(5)       TO �J�n�N�T.
018820     MOVE �J�n���v(5)       TO �J�n���T.
018830     MOVE �J�n���v(5)       TO �J�n���T.
018840     MOVE �I���N�v(5)       TO �I���N�T.
018850     MOVE �I�����v(5)       TO �I�����T.
018860     MOVE �I�����v(5)       TO �I�����T.
018870     MOVE �������v(5)       TO �������T.
018880*     IF �������v(5) NOT = ZERO
018890*         MOVE NC"��"        TO ���T
018900*     END-IF.
018910     MOVE �����`�F�b�N�v(5) TO �����`�F�b�N�T.
018920     MOVE ���~�`�F�b�N�v(5) TO ���~�`�F�b�N�T.
018930     MOVE �]��`�F�b�N�v(5) TO �]��`�F�b�N�T.
018940**************
018950* �o�߃Z�b�g *
018960**************
018970     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ( ���ʂb�m�s > 5 )
018980***             UNTIL ( ���ʂb�m�s > ���ʐ��v )
018990***         MOVE ���ʂb�m�s�v(���ʂb�m�s)   TO �o�ߕ��ʂb�m�s(���ʂb�m�s)
019000***         MOVE ���ʋ�؂v(���ʂb�m�s)     TO ���ʋ��(���ʂb�m�s)
019010         MOVE ����o�ߗ��̂v(���ʂb�m�s) TO �o�ߗ���(���ʂb�m�s)
019020     END-PERFORM.
019030*****************************************
019040*     �V�K�E�p���`�F�b�N�ɂ���        *
019050*   ���V�K...�����L�� ���p��...�����Ȃ� *
019060*****************************************
019070     MOVE �V�K�`�F�b�N�v    TO �V�K�`�F�b�N.
019080     MOVE �p���`�F�b�N�v    TO �p���`�F�b�N.
019090********************
019100* �����f�[�^�Z�b�g *
019110********************
019120*    ****************************************************************
019130*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
019140*    ****************************************************************
019150     MOVE �������v�q                   TO  ������.
           MOVE ���������k���v�q             TO  ���������k��.
019160     MOVE ���ԊO�`�F�b�N�v             TO  ���ԊO�`�F�b�N.
019170     MOVE �x���`�F�b�N�v               TO  �x���`�F�b�N.
019180     MOVE �[��`�F�b�N�v               TO  �[��`�F�b�N.
019190     MOVE �������Z���v�q               TO  �������Z��.
019110     IF ( �������Z���v�s(1) NOT = ZERO ) OR
019120        ( �������Z���v�s(1) NOT = ZERO )
019130        MOVE �������Z���v�s(1)         TO  �������Z��
019140        MOVE �������Z���v�s(1)         TO  �������Z��
              MOVE "�{�p����"                TO �������Z�b�l
              MOVE ":"                       TO �������Z���
019150     END-IF.
019200     MOVE �Č����v�q                   TO  �Č���.
019210     MOVE ���Ë����v�q                 TO  ���Ë���.
019220     MOVE ���É񐔂v�q                 TO  ���É�.
019230     MOVE ���×��v�q                   TO  ���×�.
019240     MOVE ��ԃ`�F�b�N�v               TO  ��ԃ`�F�b�N.
019250     MOVE ��H�`�F�b�N�v               TO  ��H�`�F�b�N.
019260     MOVE �\���J��`�F�b�N�v           TO  �\���J��`�F�b�N.
019270     MOVE ���É��Z���v�q               TO  ���É��Z��.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
           IF ( �{�p�a��N���v�q < 43006 )
018050        MOVE ��`�F�b�N�v              TO  ��`�F�b�N
018060        MOVE ���`�F�b�N�v              TO  ���`�F�b�N
018070        MOVE ���`�F�b�N�v              TO  ���`�F�b�N
           END-IF.
           IF ( �{�p�a��N���v�q >= 43006 ) AND ( �������q���Z���v�q NOT = ZERO )
              MOVE ALL NC"��"                TO  ��������
      *        MOVE �����񐔂v                TO  ������
      *        MOVE NC"��"                    TO  ������
           END-IF.
019310     MOVE �������q���Z���v�q           TO  �������q���Z��.
019320     MOVE �{�p���񋟗��v�q           TO  �{�p���񋟗�.
019330     MOVE ���v�v                       TO ���v.
019340********************
019350* ���񏈒u���Z�b�g *
019360********************
019370     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ( ���ʂb�m�s > 5 )
019380***             UNTIL ( ���ʂb�m�s > ���ʐ��v )
019390         MOVE ���񏈒u���v�q(���ʂb�m�s) TO ���񏈒u��(���ʂb�m�s)
019400     END-PERFORM.
019410     MOVE ���񏈒u�����v�v         TO ���񏈒u�����v
019420*
019430     MOVE �{�×��`�F�b�N�v            TO �{�×��`�F�b�N.
019440     MOVE �������`�F�b�N�v            TO �������`�F�b�N.
019450     MOVE �Œ藿�`�F�b�N�v            TO �Œ藿�`�F�b�N.
019460********************
019470* �����������Z�b�g *
019480********************
019490*    **********
019500*    * �P���� *
019510*    **********
019520     MOVE ��ÒP���P�v�q             TO ��ÒP���P.
019530     MOVE ��É񐔂P�v�q             TO ��É񐔂P.
019540     MOVE ��×��P�v�q               TO ��×��P.
019550     MOVE ��㪖@�񐔂P�v�q           TO ��㪖@�񐔂P.
019560     MOVE ��㪖@���P�v�q             TO ��㪖@���P.
019570     MOVE ��㪖@�񐔂P�v�q           TO ��㪖@�񐔂P.
019580     MOVE ��㪖@���P�v�q             TO ��㪖@���P.
019590     MOVE �d�É񐔂P�v�q             TO �d�É񐔂P.
019600     MOVE �d�×��P�v�q               TO �d�×��P.
019610     MOVE ���v�P�v�q                 TO ���v�P.
019620     IF ( �����������P�v�q NOT = ZERO )
019630         COMPUTE �����������P = �����������P�v�q / 100
019640     END-IF.
019650     MOVE ���������v�P�v�q           TO ���������v�P.
019660*    **********
019670*    * �Q���� *
019680*    **********
019690     MOVE ��ÒP���Q�v�q             TO ��ÒP���Q.
019700     MOVE ��É񐔂Q�v�q             TO ��É񐔂Q.
019710     MOVE ��×��Q�v�q               TO ��×��Q.
019720     MOVE ��㪖@�񐔂Q�v�q           TO ��㪖@�񐔂Q.
019730     MOVE ��㪖@���Q�v�q             TO ��㪖@���Q.
019740     MOVE ��㪖@�񐔂Q�v�q           TO ��㪖@�񐔂Q.
019750     MOVE ��㪖@���Q�v�q             TO ��㪖@���Q.
019760     MOVE �d�É񐔂Q�v�q             TO �d�É񐔂Q.
019770     MOVE �d�×��Q�v�q               TO �d�×��Q.
019780     MOVE ���v�Q�v�q                 TO ���v�Q.
019790     IF ( �����������Q�v�q NOT = ZERO )
019800         COMPUTE �����������Q = �����������Q�v�q / 100
019810     END-IF.
019820     MOVE ���������v�Q�v�q           TO ���������v�Q.
019830*    ****************
019840*    * �R���ʁ^�W�� *
019850*    ****************
019860     MOVE ��ÒP���R�W�v�q             TO ��ÒP���R�W.
019870     MOVE ��É񐔂R�W�v�q             TO ��É񐔂R�W.
019880     MOVE ��×��R�W�v�q               TO ��×��R�W.
019890     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
019900     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
019910     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
019920     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
019930     MOVE �d�É񐔂R�W�v�q             TO �d�É񐔂R�W.
019940     MOVE �d�×��R�W�v�q               TO �d�×��R�W.
019950     MOVE ���v�R�W�v�q                 TO ���v�R�W.
019960     MOVE �����ʍ����v�R�W�v�q         TO �����ʍ����v�R�W.
019970     IF ( �����������R�W�v�q NOT = ZERO )
019980         COMPUTE �����������R�W = �����������R�W�v�q / 100
019990     END-IF.
020000     MOVE ���������v�R�W�v�q           TO ���������v�R�W.
      */ ������ 0.7��0.6 /42505
           IF (�{�p�a��N���v�q >= 42505)
              MOVE "60"                      TO �����R�W
              MOVE "0.6"                     TO �����ʂR�W
              MOVE "==="                     TO ���������R�W �����ʒ����R�W
           END-IF.
020010*    ****************
020020*    * �R���ʁ^10�� *
020030*    ****************
020040     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
020050     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
020060     MOVE ��ÒP���R�O�v�q             TO ��ÒP���R�O.
020070     MOVE ��É񐔂R�O�v�q             TO ��É񐔂R�O.
020080     MOVE ��×��R�O�v�q               TO ��×��R�O.
020090     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
020100     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
020110     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
020120     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
020130     MOVE �d�É񐔂R�O�v�q             TO �d�É񐔂R�O.
020140     MOVE �d�×��R�O�v�q               TO �d�×��R�O.
020150     MOVE ���v�R�O�v�q                 TO ���v�R�O.
020160     IF ( �����������R�O�v�q NOT = ZERO )
020170         COMPUTE �����������R�O = �����������R�O�v�q / 100
020180     END-IF.
020190     MOVE ���������v�R�O�v�q           TO ���������v�R�O.
020200*    ****************
020210*    * �S���ʁ^�T�� *
020220*    ****************
020230*     MOVE ��ÒP���S�T�v�q             TO ��ÒP���S�T.
020240*     MOVE ��É񐔂S�T�v�q             TO ��É񐔂S�T.
020250*     MOVE ��×��S�T�v�q               TO ��×��S�T.
020260*     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
020270*     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
020280*     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
020290*     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
020300*     MOVE �d�É񐔂S�T�v�q             TO �d�É񐔂S�T.
020310*     MOVE �d�×��S�T�v�q               TO �d�×��S�T.
020320*     MOVE ���v�S�T�v�q                 TO ���v�S�T.
020330*     MOVE �����ʍ����v�S�T�v�q         TO �����ʍ����v�S�T.
020340*     IF ( �����������S�T�v�q NOT = ZERO )
020350*         COMPUTE �����������S�T = �����������S�T�v�q / 100
020360*     END-IF.
020370*     MOVE ���������v�S�T�v�q           TO ���������v�S�T.
020380*    ****************
020390*    * �S���ʁ^�W�� *
020400*    ****************
020410     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
020420     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
020430     MOVE ��ÒP���S�W�v�q             TO ��ÒP���S�W.
020440     MOVE ��É񐔂S�W�v�q             TO ��É񐔂S�W.
020450     MOVE ��×��S�W�v�q               TO ��×��S�W.
020460     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
020470     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
020480     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
020490     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
020500     MOVE �d�É񐔂S�W�v�q             TO �d�É񐔂S�W.
020510     MOVE �d�×��S�W�v�q               TO �d�×��S�W.
020520     MOVE ���v�S�W�v�q                 TO ���v�S�W.
020530     MOVE �����ʍ����v�S�W�v�q         TO �����ʍ����v�S�W.
020540     IF ( �����������S�W�v�q NOT = ZERO )
020550         COMPUTE �����������S�W = �����������S�W�v�q / 100
020560     END-IF.
020570     MOVE ���������v�S�W�v�q           TO ���������v�S�W.
      */ ������ 0.7��0.6 /42505
           IF (�{�p�a��N���v�q >= 42505)
              MOVE "60"                      TO �����S�W
              MOVE "0.6"                     TO �����ʂS�W
              MOVE "==="                     TO ���������S�W �����ʒ����S�W
           END-IF.
020580*    ****************
020590*    * �S���ʁ^10�� *
020600*    ****************
020610     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
020620     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
020630     MOVE ��ÒP���S�O�v�q             TO ��ÒP���S�O.
020640     MOVE ��É񐔂S�O�v�q             TO ��É񐔂S�O.
020650     MOVE ��×��S�O�v�q               TO ��×��S�O.
020660     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
020670     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
020680     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
020690     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
020700     MOVE �d�É񐔂S�O�v�q             TO �d�É񐔂S�O.
020710     MOVE �d�×��S�O�v�q               TO �d�×��S�O.
020720     MOVE ���v�S�O�v�q                 TO ���v�S�O.
020730     IF ( �����������S�O�v�q NOT = ZERO )
020740         COMPUTE �����������S�O = �����������S�O�v�q / 100
020750     END-IF.
020760     MOVE ���������v�S�O�v�q           TO ���������v�S�O.
020770*
020780*��***********************************************************************
020790* �T���ʂ̈󎚘g�Ȃ��B
020800*------------------------------------------------------------------------*
020810* �T���ʁ^2.5���̈󎚂͕K�v�Ȃ��B
020820*------------------------------------------------------------------------*
020830*    *****************
020840*    * �T���ʁ^2.5�� *
020850*    *****************
020860*     MOVE ��ÒP���T�Q�v�q             TO ��ÒP���T�Q.
020870*     MOVE ��É񐔂T�Q�v�q             TO ��É񐔂T�Q.
020880*     MOVE ��×��T�Q�v�q               TO ��×��T�Q.
020890*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
020900*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
020910*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
020920*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
020930*     MOVE �d�É񐔂T�Q�v�q             TO �d�É񐔂T�Q.
020940*     MOVE �d�×��T�Q�v�q               TO �d�×��T�Q.
020950*     MOVE ���v�T�Q�v�q                 TO ���v�T�Q.
020960*     MOVE �����ʍ����v�T�Q�v�q         TO �����ʍ����v�T�Q.
020970*     IF ( �����������T�Q�v�q NOT = ZERO )
020980*         COMPUTE �����������T�Q = �����������T�Q�v�q / 100
020990*     END-IF.
021000*     MOVE ���������v�T�Q�v�q           TO ���������v�T�Q.
021010*
021020*    ****************
021030*    * �T���ʁ^�T�� *
021040*    ****************
021050*     IF ( ���������v�T�T�v�q NOT = ZERO )
021060*        MOVE "33"                      TO �����T�T
021070*        MOVE "0.33"                    TO �����T�T����
021080*     END-IF.
021090*     MOVE �����J�n���T�T�v�q           TO �����J�n���T�T.
021100*     MOVE �����J�n���T�T�v�q           TO �����J�n���T�T.
021110*     MOVE ��ÒP���T�T�v�q             TO ��ÒP���T�T.
021120*     MOVE ��É񐔂T�T�v�q             TO ��É񐔂T�T.
021130*     MOVE ��×��T�T�v�q               TO ��×��T�T.
021140*     MOVE ��㪖@�񐔂T�T�v�q           TO ��㪖@�񐔂T�T.
021150*     MOVE ��㪖@���T�T�v�q             TO ��㪖@���T�T.
021160*     MOVE ��㪖@�񐔂T�T�v�q           TO ��㪖@�񐔂T�T.
021170*     MOVE ��㪖@���T�T�v�q             TO ��㪖@���T�T.
021180*     MOVE �d�É񐔂T�T�v�q             TO �d�É񐔂T�T.
021190*     MOVE �d�×��T�T�v�q               TO �d�×��T�T.
021200*     MOVE ���v�T�T�v�q                 TO ���v�T�T.
021210*     MOVE �����ʍ����v�T�T�v�q         TO �����ʍ����v�T�T.
021220*     IF ( �����������T�T�v�q NOT = ZERO )
021230*         COMPUTE �����������T�T = �����������T�T�v�q / 100
021240*     END-IF.
021250*     MOVE ���������v�T�T�v�q           TO ���������v�T�T.
021260*    ****************
021270*    * �T���ʁ^�W�� *
021280*    ****************
021290*     IF ( ���������v�T�W�v�q NOT = ZERO )
021300*        MOVE "80"                      TO �����T�W
021310*        MOVE "0.8"                     TO �����T�W����
021320*     END-IF.
021330*     MOVE �����J�n���T�W�v�q           TO �����J�n���T�W.
021340*     MOVE �����J�n���T�W�v�q           TO �����J�n���T�W.
021350*     MOVE ��ÒP���T�W�v�q             TO ��ÒP���T�W.
021360*     MOVE ��É񐔂T�W�v�q             TO ��É񐔂T�W.
021370*     MOVE ��×��T�W�v�q               TO ��×��T�W.
021380*     MOVE ��㪖@�񐔂T�W�v�q           TO ��㪖@�񐔂T�W.
021390*     MOVE ��㪖@���T�W�v�q             TO ��㪖@���T�W.
021400*     MOVE ��㪖@�񐔂T�W�v�q           TO ��㪖@�񐔂T�W.
021410*     MOVE ��㪖@���T�W�v�q             TO ��㪖@���T�W.
021420*     MOVE �d�É񐔂T�W�v�q             TO �d�É񐔂T�W.
021430*     MOVE �d�×��T�W�v�q               TO �d�×��T�W.
021440*     MOVE ���v�T�W�v�q                 TO ���v�T�W.
021450*     MOVE �����ʍ����v�T�W�v�q         TO �����ʍ����v�T�W.
021460*     IF ( �����������T�W�v�q NOT = ZERO )
021470*         COMPUTE �����������T�W = �����������T�W�v�q / 100
021480*     END-IF.
021490*     MOVE ���������v�T�W�v�q           TO ���������v�T�W.
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
021720        MOVE ���ʂT�v                     TO ���ʂT�W
021490     END-IF.
021500*    ****************
021510*    * �T���ʁ^10�� *
021520*    ****************
021530*     IF ( ���������v�T�O�v�q NOT = ZERO )
021540*        MOVE "100"                     TO �����T�O
021550*        MOVE "�|"                      TO �����T�O����
021560*        MOVE ALL "�\"                  TO �����ʍ����v�T�O
021570*     END-IF.
021580*     MOVE �����J�n���T�O�v�q           TO �����J�n���T�O.
021590*     MOVE �����J�n���T�O�v�q           TO �����J�n���T�O.
021600*     MOVE ��ÒP���T�O�v�q             TO ��ÒP���T�O.
021610*     MOVE ��É񐔂T�O�v�q             TO ��É񐔂T�O.
021620*     MOVE ��×��T�O�v�q               TO ��×��T�O.
021630*     MOVE ��㪖@�񐔂T�O�v�q           TO ��㪖@�񐔂T�O.
021640*     MOVE ��㪖@���T�O�v�q             TO ��㪖@���T�O.
021650*     MOVE ��㪖@�񐔂T�O�v�q           TO ��㪖@�񐔂T�O.
021660*     MOVE ��㪖@���T�O�v�q             TO ��㪖@���T�O.
021670*     MOVE �d�É񐔂T�O�v�q             TO �d�É񐔂T�O.
021680*     MOVE �d�×��T�O�v�q               TO �d�×��T�O.
021690*     MOVE ���v�T�O�v�q                 TO ���v�T�O.
021700*     MOVE �����������T�O�v�q           TO �����������T�O.
021710*     IF ( �����������T�O�v�q NOT = ZERO )
021720*         COMPUTE �����������T�O = �����������T�O�v�q / 100
021730*     END-IF.
021740*     MOVE ���������v�T�O�v�q           TO ���������v�T�O.
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
021720        MOVE ���ʂT�v                     TO ���ʂT�O
021730     END-IF.
021750*��***********************************************************************
021760*
021770     MOVE �K�p�P�v                      TO �K�p�P.
021780     MOVE �K�p�Q�v                      TO �K�p�Q.
021770*     MOVE �K�p�P�P�v                     TO �K�p�P�P.
021780*     MOVE �K�p�P�Q�v                     TO �K�p�P�Q.
021790*     MOVE �K�p�Q�P�v                     TO �K�p�Q�P.
021800*     MOVE �K�p�Q�Q�v                     TO �K�p�Q�Q.
      *
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
           IF ( �{�p�a��N���v�q >= 43006 )
              INITIALIZE �A���^�|�L�[
019550        MOVE �{�p�a��v�q TO �A���^�|�{�p�a��
019560        MOVE �{�p�N�v�q   TO �A���^�|�{�p�N
019570        MOVE �{�p���v�q   TO �A���^�|�{�p��
019580        MOVE ���Ҕԍ��v�q TO �A���^�|���Ҕԍ�
019590        MOVE �}�Ԃv�q     TO �A���^�|�}��
              MOVE �ی���ʂv�q TO �A���^�|�ی����
              MOVE 31           TO �A���^�|��R�[�h
              MOVE ZERO         TO �A���^�|�p�����
              CALL "KINUNRYO"
              CANCEL "KINUNRYO"
              MOVE �A���^�|�������q�b�l           TO �������q�b�l
              MOVE �A���^�|�^����Âb�l           TO �^����Âb�l
              IF ( �������q���Z���v�q NOT = ZERO )
                 MOVE �������q�b�l                TO �������q
              END-IF
              IF ( �^�����v NOT = ZERO )
                 MOVE �^����Âb�l                TO �^�����
              END-IF
           END-IF.
      *
021810     MOVE ���Z�|���v                     TO ���v.
021820     MOVE ���Z�|�ꕔ���S��               TO �ꕔ���S��.
021830     MOVE ���Z�|�������z                 TO �������z.
021840*
021850*     PERFORM ���S���擾�P�S�P�O.
021860*     EVALUATE ���S���v
021870*     WHEN 0
021880*         MOVE NC"��"                     TO �P�O���`�F�b�N
021890*     WHEN 10
021900*         MOVE NC"��"                     TO �X���`�F�b�N
      **/�O������҂P���́A���t�������W���ɂ���B(�����P�����S���邽�߁A���҂P���A�ی��҂W���A���P���ƂȂ�)
      *         IF (��|�ی���� NOT = 05 ) AND (��|���ʋ敪 = 1) AND (��|�{�p�a��N�� >= 42004)
      **/�_�ސ쌧�̍��ہA�ސE�ݑΏۂɕύX/080724
      *             IF ((��|�ی���� = 01 ) AND (��|�ی��Ҕԍ�(1:2) = "14")) OR
      *                ((��|�ی���� = 08 ) AND (��|�ی��Ҕԍ�(3:2) = "14"))
      *                 MOVE SPACE  TO �X���`�F�b�N
      *                 MOVE NC"��" TO �W���`�F�b�N
      *             END-IF
      *         END-IF
021910*     WHEN 20
021920*         MOVE NC"��"                     TO �W���`�F�b�N
021930*     WHEN 30
021940*         MOVE NC"��"                     TO �V���`�F�b�N
021950*     END-EVALUATE.
021960*
021970*------------------------------------------------------------------------------------*
021980* ���ʁi�������Z�Ȃ��ŁA�{�̃��Z�ɂ܂Ƃ߂鎞�A���z�͏������݁E�K�p�Q�ɏ�����ʈ󎚁j
021990     IF ( �������Z�܂Ƃ߃t���O = "YES" )
022010         MOVE ���Z�|���v         TO ���v
      */���Z�܂Ƃߎ��͖{�̂̋��z���L�ڂ��遫����/130725
022020**     / �����Z����/
022030*         COMPUTE �������z = ���Z�|���v - ���Z�|�����������z
022040*         MOVE ���Z�|�����������z         TO �ꕔ���S��
021820         MOVE ���Z�|�ꕔ���S��           TO �ꕔ���S��
021830         MOVE ���Z�|�������z             TO �������z
      */���Z�܂Ƃߎ��͖{�̂̋��z���L�ڂ��遪����/130725
022050*
022060*/�[�Q��̋󔒂ɃX�g�����O���Ă��܂�����NOT SPACE�̎��͍Ō�ɓ]�L����B
021920         IF ������ʗ��̂v NOT = SPACE
021930            IF �K�p�Q�v NOT = SPACE
021940                MOVE SPACE TO ������ʗ��̂v�Q
021950                STRING NC"��"             DELIMITED BY SIZE
021960                       ������ʗ��̂v     DELIMITED BY SPACE
021970                       INTO ������ʗ��̂v�Q
021980                END-STRING
021990                MOVE ������ʗ��̂v�Q TO �K�p�Q(35:4)
022000            ELSE
022010                STRING �K�p�Q�v           DELIMITED BY SPACE
022020                       NC"��"             DELIMITED BY SIZE
022030                       ������ʗ��̂v     DELIMITED BY SPACE
022040                       INTO �K�p�Q
022050                END-STRING
022060            END-IF
022070         END-IF
022070*         IF ( ������ʗ��̂v NOT = SPACE )
022080*            MOVE SPACE TO ������ʗ��̂v�Q
022090*            STRING NC"��"             DELIMITED BY SIZE
022100*                   ������ʗ��̂v     DELIMITED BY SPACE
022110*                   INTO ������ʗ��̂v�Q
022120*            END-STRING
022130**
022140*            IF ( �K�p�Q�P�v = SPACE )
022150*               MOVE ������ʗ��̂v�Q TO �K�p�Q�P
022160*            ELSE
022170*               IF ( �K�p�Q�Q�v = SPACE )
022180*                  MOVE ������ʗ��̂v�Q TO �K�p�Q�Q
022190*               ELSE
022200*                  MOVE ������ʗ��̂v�Q TO �K�p�Q�Q(16:4)
022210*               END-IF
022220*            END-IF
022230*         END-IF
022240     END-IF.
022250*------------------------------------------------------------------------------------*
022480*
022490**********************
022500* �{�p���f�[�^�Z�b�g *
022510**********************
           MOVE �s���{���i�h�r�v       TO �s���{���ԍ�.
022520     MOVE �_���t�ԍ��v           TO �_���t�ԍ�.
022530*     MOVE �ڍ��t�����ԍ��v     TO �ڍ��t�����ԍ�.
022540***     MOVE ��z���󗝔ԍ��v       TO ��z���󗝔ԍ�.
022550     MOVE �{�p���X�֔ԍ��P�v     TO �{�p���X�֔ԍ��P.
022560     MOVE �{�p���X�֔ԍ��Q�v     TO �{�p���X�֔ԍ��Q.
022570*     MOVE �{�p���Z���v           TO �{�p���Z��.
022580     MOVE �{�p���Z���P�v         TO �{�p���Z���P.
022590     MOVE �{�p���Z���Q�v         TO �{�p���Z���Q.
022600     MOVE ��\�҃J�i�v           TO ��\�҃J�i.
022610     MOVE ��\�Җ��v             TO ��\�Җ�.
022620     MOVE �{�p���d�b�ԍ��v       TO �{�p���d�b�ԍ�.
022630*
022640     MOVE �ڍ��@���v             TO �ڍ��@��.
022650*
      *     MOVE "��160-0004�����s�V�h��l�J2-10 ���{��501" TO �㗝�l�Z��.
022680*     MOVE "�O��Z�F��s �~�c�x�X(��)8675863"         TO ��s���x�X��.
022690*     MOVE "���{�_������ � �א� ��j(��ݼޭ�������� ����� ο�� ϻ��)" TO �������`�l.
022660*     MOVE "���{�_������ � �א� ��j" TO �㗝�l����.
      *�x���@�֗�(��̌���)
           MOVE "8675863"                        TO �����ԍ�.
           MOVE "���{�_������ � �א� ��j"    TO �������`�l.
           MOVE "��ݼޭ�������� ����� ο�� ϻ��" TO �������`�l�J�i.
           MOVE "�O��Z�F"                       TO ���Z�@�֖��P
           MOVE "�~�c"                           TO �x�X���P.
           MOVE NC"��" TO �U���`�F�b�N.
           MOVE NC"��" TO ���ʃ`�F�b�N.
           MOVE NC"��" TO ��s�`�F�b�N.
           MOVE NC"��" TO �x�X�`�F�b�N.
           MOVE "�×{��̎�̂���{�_������ � �א� ��j"         TO ��ϔC�R�����g�P
      *     MOVE "(�����s�V�h��l�J2-10 ���{��501)�ɈϔC���܂��B"    TO ��ϔC�R�����g�Q
           MOVE "(���{���s����k�x�]�꒚��20-15���x����r��902)" TO ��ϔC�R�����g�Q
           MOVE "�ɈϔC���܂��B"                                     TO ��ϔC�R�����g�R
022700*
022710*     IF ( ������s���Q�v = SPACE )
022720*        MOVE SPACE               TO ��s���P
022730*        MOVE ������s���P�v    TO ��s���Q
022740*     ELSE
022750*        MOVE ������s���P�v    TO ��s���P
022760*        MOVE ������s���Q�v    TO ��s���Q
022770*     END-IF.
022780*     IF ( ������s�x�X���Q�v = SPACE )
022790*        MOVE SPACE                TO ��s�x�X���P
022800*        MOVE ������s�x�X���P�v TO ��s�x�X���Q
022810*     ELSE
022820*        MOVE ������s�x�X���P�v TO ��s�x�X���P
022830*        MOVE ������s�x�X���Q�v TO ��s�x�X���Q
022840*     END-IF.
022850***     MOVE �a����ʃR�����g�v     TO �a�����.
022860*     MOVE �����ԍ��v             TO �����ԍ�.
022870***     MOVE �������`�l�J�i�v       TO �������`�l�J�i.
022880***     MOVE �������`�l�v           TO �������`�l.
022890*
022900      MOVE �{�p�a��v�q TO ���|�����敪
022910      READ �����}�X�^
022920      NOT INVALID KEY
022930          MOVE ���|�������� TO �{�p�a��̂v
022940      END-READ
022950* / �_���t�E���҈ϔC�� /
022960*     MOVE �{�p�a��̂v         TO �󗝘a��.
      */�����C��/������20190426
037370     IF �{�p�a��v > 4
               MOVE �{�p�a��v         TO ���|�����敪
037380         READ �����}�X�^
037390         NOT INVALID KEY
037400             MOVE ���|��������   TO �󗝘a��
037410         END-READ
               MOVE "===="             TO �󗝘a�����
           END-IF.
      */�����C��/������20190426
022970     MOVE �_���t�N�v             TO �󗝔N.
022980     MOVE �_���t���v             TO �󗝌�.
022990     MOVE �_���t���v             TO �󗝓�.
023000* ( �ϔC�N���� ������邩 )
023010     IF ( �A���|�ϔC���  = ZERO )
023020*         MOVE �{�p�a��̂v     TO �ϔC�a��
      */�����C��/������20190426
037370         IF �{�p�a��v > 4
                   MOVE �{�p�a��v         TO ���|�����敪
037380             READ �����}�X�^
037390             NOT INVALID KEY
037400                 MOVE ���|��������   TO �ϔC�a��
037410             END-READ
                   MOVE "===="             TO �ϔC�a�����
               END-IF
      */�����C��/������20190426
023030         MOVE ���҈ϔC�N�v       TO �ϔC�N
023040         MOVE ���҈ϔC���v       TO �ϔC��
023050         MOVE ���҈ϔC���v       TO �ϔC��
023060*         MOVE �����於�̂v�s     TO �ی��Җ��̈ϔC
023070*         MOVE NC"�a"             TO �a
023080     END-IF.
023090*
      */�������Ή�/100618
023100* �{�pID
023110     MOVE ���{�p�h�c�v           TO ���{�p�h�c.
023120*
023130* ���ϔԍ�
023140     MOVE ���ϔԍ��v             TO ���ϔԍ�.
023150*
023160************************
023170* ���Z�v�g���я��Z�b�g *
023180************************
023190     MOVE ���ԌŒ�v          TO ���ԌŒ�.
023200     MOVE ���Ԃv              TO ����.
023210     MOVE "("                 TO ���ʂP.
023220     MOVE ���Ҕԍ��v�q        TO ���Ҕԍ�.
023230     MOVE �}�Ԃv�q            TO �}��.
023240     MOVE ")"                 TO ���ʂQ.
023250*
023260*
023270* ���ʃR�����g
023280*     MOVE ���ʃR�����g�v      TO ���ʃR�����g.
023300*-------------------------------------------------------------------------*
023310*--- �� ���Z�E�v�ăZ�b�g�́A���̈���Z�b�gSECTION �̍Ō�ɂ�邱�ƁI -----*
023320     PERFORM ���Z�E�v�ăZ�b�g.
023330*-------------------------------------------------------------------------*
023340*
023350***    PERFORM �e�X�g�󎚏���.
023360*
023370*================================================================*
023380 ���ڏ����� SECTION.
023390*
023400     INITIALIZE �{�p�����v.
023410     INITIALIZE ��f�ҏ��v.
023420     INITIALIZE �������v.
023430     INITIALIZE ���l���v.
023440     INITIALIZE �����P�v�q.
023450     INITIALIZE �����Q�v�q.
023460     INITIALIZE �����R�v�q.
023480     INITIALIZE YJK6125P.
023470     MOVE SPACE TO YJK6125P.
021920*================================================================*
021930 ��{���擾 SECTION.
021920*================================================================*
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
023490*================================================================*
023500 �������擾 SECTION.
023510*
023520********************
023530* �����f�[�^�Z�b�g *
023540********************
023550*    ****************************************************************
023560*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
023570*    ****************************************************************
023580     MOVE ���Z�|������                 TO �������v�q.
023590     IF ( ���Z�|���ԊO = 1 )
023600         MOVE NC"��"                   TO ���ԊO�`�F�b�N�v
023610     END-IF.
023620     IF ( ���Z�|�x�� = 1 )
023630         MOVE NC"��"                   TO �x���`�F�b�N�v
023640     END-IF.
023650     IF ( ���Z�|�[�� = 1 )
023660         MOVE NC"��"                   TO �[��`�F�b�N�v
023670     END-IF.
023680*
023690     MOVE ���Z�|�������Z��             TO �������Z���v�q.
           MOVE ���Z�|���������k��           TO ���������k���v�q.
023700     MOVE ���Z�|�Č���                 TO �Č����v�q.
023710     MOVE ���Z�|���Ë���               TO ���Ë����v�q.
023720     MOVE ���Z�|���É�               TO ���É񐔂v�q.
023730     MOVE ���Z�|���×�                 TO ���×��v�q.
023740     MOVE ���Z�|���É��Z��             TO ���É��Z���v�q.
023750*
023760     IF ( ���Z�|��� = 1 )
023770         MOVE NC"��"                   TO ��ԃ`�F�b�N�v
023780     END-IF.
023790     IF ( ���Z�|��H = 1 )
023800         MOVE NC"��"                   TO ��H�`�F�b�N�v
023810     END-IF.
023820     IF ( ���Z�|�\���J�� = 1 )
023830         MOVE NC"��"                   TO �\���J��`�F�b�N�v
023840     END-IF.
023850*
023860     MOVE ���Z�|�������q���Z��         TO �������q���Z���v�q.
023870*
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
021850     IF ( ���Z�|�� >= 1 )
021860         MOVE NC"��"                   TO ��`�F�b�N�v
021870     END-IF.
021880     IF ( ���Z�|�� >= 1 )
021890         MOVE NC"��"                   TO ���`�F�b�N�v
021900     END-IF.
021910     IF ( ���Z�|�� >= 1 )
021920         MOVE NC"��"                   TO ���`�F�b�N�v
021930     END-IF.
           IF ( �{�p�a��N���v�q >= 43006 )
              MOVE ���Z�|�������q��        TO �����񐔂v
           END-IF.
           MOVE ���Z�|�^����×�              TO �^�����v.
023970*
023980     MOVE ���Z�|�{�p���񋟗�         TO �{�p���񋟗��v�q.
023990* ���v
024000     MOVE ���Z�|���v                   TO ���v�v.
024010********************
024020* ���񏈒u���Z�b�g *
024030********************
024040     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
024050             UNTIL ( ���ʂb�m�s > ���ʐ��v )
024060         MOVE ���Z�|���񏈒u��(���ʂb�m�s) TO ���񏈒u���v�q(���ʂb�m�s)
024070         IF ( ���Z�|���񏈒u��(���ʂb�m�s) NOT = ZERO )
024080            EVALUATE ���|�������(���ʂb�m�s)
024090* �P���E�Ŗo�E����
024100            WHEN 1
024110            WHEN 2
024120            WHEN 3
024130                MOVE NC"��"       TO �{�×��`�F�b�N�v
024140* �E�P�E���܁E���܍S�k
024150            WHEN 4
024160            WHEN 5
024170            WHEN 7
024180                MOVE NC"��"       TO �������`�F�b�N�v
024190* �s�S���܁E�s�S���܍S�k
024200            WHEN 6
024210            WHEN 8
024220                MOVE NC"��"       TO �Œ藿�`�F�b�N�v
024230            END-EVALUATE
024240         END-IF
024250     END-PERFORM.
024260*
024270     MOVE ���Z�|���񏈒u�����v    TO ���񏈒u�����v�v.
024280********************
024290* �����������Z�b�g *
024300********************
024310*    **********
024320*    * �P���� *
024330*    **********
024340     MOVE ���Z�|��ÒP���P             TO ��ÒP���P�v�q.
024350     MOVE ���Z�|��É񐔂P             TO ��É񐔂P�v�q.
024360     MOVE ���Z�|��×��P               TO ��×��P�v�q.
024370     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
024380     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
024390     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
024400     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
024410     MOVE ���Z�|�d�É񐔂P             TO �d�É񐔂P�v�q.
024420     MOVE ���Z�|�d�×��P               TO �d�×��P�v�q.
024430     MOVE ���Z�|���v�P                 TO ���v�P�v�q.
024440     MOVE ���Z�|�����������P           TO �����������P�v�q.
024450     MOVE ���Z�|���������v�P           TO ���������v�P�v�q.
024460*    **********
024470*    * �Q���� *
024480*    **********
024490     MOVE ���Z�|��ÒP���Q             TO ��ÒP���Q�v�q.
024500     MOVE ���Z�|��É񐔂Q             TO ��É񐔂Q�v�q.
024510     MOVE ���Z�|��×��Q               TO ��×��Q�v�q.
024520     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
024530     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
024540     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
024550     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
024560     MOVE ���Z�|�d�É񐔂Q             TO �d�É񐔂Q�v�q.
024570     MOVE ���Z�|�d�×��Q               TO �d�×��Q�v�q.
024580     MOVE ���Z�|���v�Q                 TO ���v�Q�v�q.
024590     MOVE ���Z�|�����������Q           TO �����������Q�v�q.
024600     MOVE ���Z�|���������v�Q           TO ���������v�Q�v�q.
024610*    ****************
024620*    * �R���ʁ^�W�� *
024630*    ****************
024640     MOVE ���Z�|��ÒP���R�W             TO ��ÒP���R�W�v�q.
024650     MOVE ���Z�|��É񐔂R�W             TO ��É񐔂R�W�v�q.
024660     MOVE ���Z�|��×��R�W               TO ��×��R�W�v�q.
024670     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
024680     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
024690     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
024700     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
024710     MOVE ���Z�|�d�É񐔂R�W             TO �d�É񐔂R�W�v�q.
024720     MOVE ���Z�|�d�×��R�W               TO �d�×��R�W�v�q.
024730     MOVE ���Z�|���v�R�W                 TO ���v�R�W�v�q.
024740     MOVE ���Z�|�����ʍ����v�R�W         TO �����ʍ����v�R�W�v�q.
024750     MOVE ���Z�|�����������R�W           TO �����������R�W�v�q.
024760     MOVE ���Z�|���������v�R�W           TO ���������v�R�W�v�q.
024770*    ****************
024780*    * �R���ʁ^10�� *
024790*    ****************
024800     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
024810     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
024820     MOVE ���Z�|��ÒP���R�O             TO ��ÒP���R�O�v�q.
024830     MOVE ���Z�|��É񐔂R�O             TO ��É񐔂R�O�v�q.
024840     MOVE ���Z�|��×��R�O               TO ��×��R�O�v�q.
024850     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
024860     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
024870     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
024880     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
024890     MOVE ���Z�|�d�É񐔂R�O             TO �d�É񐔂R�O�v�q.
024900     MOVE ���Z�|�d�×��R�O               TO �d�×��R�O�v�q.
024910     MOVE ���Z�|���v�R�O                 TO ���v�R�O�v�q.
024920     MOVE ���Z�|�����������R�O           TO �����������R�O�v�q.
024930     MOVE ���Z�|���������v�R�O           TO ���������v�R�O�v�q.
024940*    ****************
024950*    * �S���ʁ^�T�� *
024960*    ****************
024970     MOVE ���Z�|��ÒP���S�T             TO ��ÒP���S�T�v�q.
024980     MOVE ���Z�|��É񐔂S�T             TO ��É񐔂S�T�v�q.
024990     MOVE ���Z�|��×��S�T               TO ��×��S�T�v�q.
025000     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
025010     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
025020     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
025030     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
025040     MOVE ���Z�|�d�É񐔂S�T             TO �d�É񐔂S�T�v�q.
025050     MOVE ���Z�|�d�×��S�T               TO �d�×��S�T�v�q.
025060     MOVE ���Z�|���v�S�T                 TO ���v�S�T�v�q.
025070     MOVE ���Z�|�����ʍ����v�S�T         TO �����ʍ����v�S�T�v�q.
025080     MOVE ���Z�|�����������S�T           TO �����������S�T�v�q.
025090     MOVE ���Z�|���������v�S�T           TO ���������v�S�T�v�q.
025100*    ****************
025110*    * �S���ʁ^�W�� *
025120*    ****************
025130     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
025140     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
025150     MOVE ���Z�|��ÒP���S�W             TO ��ÒP���S�W�v�q.
025160     MOVE ���Z�|��É񐔂S�W             TO ��É񐔂S�W�v�q.
025170     MOVE ���Z�|��×��S�W               TO ��×��S�W�v�q.
025180     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
025190     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
025200     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
025210     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
025220     MOVE ���Z�|�d�É񐔂S�W             TO �d�É񐔂S�W�v�q.
025230     MOVE ���Z�|�d�×��S�W               TO �d�×��S�W�v�q.
025240     MOVE ���Z�|���v�S�W                 TO ���v�S�W�v�q.
025250     MOVE ���Z�|�����ʍ����v�S�W         TO �����ʍ����v�S�W�v�q.
025260     MOVE ���Z�|�����������S�W           TO �����������S�W�v�q.
025270     MOVE ���Z�|���������v�S�W           TO ���������v�S�W�v�q.
025280*    ****************
025290*    * �S���ʁ^10�� *
025300*    ****************
025310     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
025320     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
025330     MOVE ���Z�|��ÒP���S�O             TO ��ÒP���S�O�v�q.
025340     MOVE ���Z�|��É񐔂S�O             TO ��É񐔂S�O�v�q.
025350     MOVE ���Z�|��×��S�O               TO ��×��S�O�v�q.
025360     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
025370     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
025380     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
025390     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
025400     MOVE ���Z�|�d�É񐔂S�O             TO �d�É񐔂S�O�v�q.
025410     MOVE ���Z�|�d�×��S�O               TO �d�×��S�O�v�q.
025420     MOVE ���Z�|���v�S�O                 TO ���v�S�O�v�q.
025430     MOVE ���Z�|�����������S�O           TO �����������S�O�v�q.
025440     MOVE ���Z�|���������v�S�O           TO ���������v�S�O�v�q.
025450*    *****************
025460*    * �T���ʁ^2.5�� *
025470*    *****************
025480     MOVE ���Z�|��ÒP���T�Q             TO ��ÒP���T�Q�v�q.
025490     MOVE ���Z�|��É񐔂T�Q             TO ��É񐔂T�Q�v�q.
025500     MOVE ���Z�|��×��T�Q               TO ��×��T�Q�v�q.
025510     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
025520     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
025530     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
025540     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
025550     MOVE ���Z�|�d�É񐔂T�Q             TO �d�É񐔂T�Q�v�q.
025560     MOVE ���Z�|�d�×��T�Q               TO �d�×��T�Q�v�q.
025570     MOVE ���Z�|���v�T�Q                 TO ���v�T�Q�v�q.
025580     MOVE ���Z�|�����ʍ����v�T�Q         TO �����ʍ����v�T�Q�v�q.
025590     MOVE ���Z�|�����������T�Q           TO �����������T�Q�v�q.
025600     MOVE ���Z�|���������v�T�Q           TO ���������v�T�Q�v�q.
025610*    ****************
025620*    * �T���ʁ^�T�� *
025630*    ****************
025640     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
025650     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
025660     MOVE ���Z�|��ÒP���T�T             TO ��ÒP���T�T�v�q.
025670     MOVE ���Z�|��É񐔂T�T             TO ��É񐔂T�T�v�q.
025680     MOVE ���Z�|��×��T�T               TO ��×��T�T�v�q.
025690     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
025700     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
025710     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
025720     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
025730     MOVE ���Z�|�d�É񐔂T�T             TO �d�É񐔂T�T�v�q.
025740     MOVE ���Z�|�d�×��T�T               TO �d�×��T�T�v�q.
025750     MOVE ���Z�|���v�T�T                 TO ���v�T�T�v�q.
025760     MOVE ���Z�|�����ʍ����v�T�T         TO �����ʍ����v�T�T�v�q.
025770     MOVE ���Z�|�����������T�T           TO �����������T�T�v�q.
025780     MOVE ���Z�|���������v�T�T           TO ���������v�T�T�v�q.
025790*    ****************
025800*    * �T���ʁ^�W�� *
025810*    ****************
025820     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
025830     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
025840     MOVE ���Z�|��ÒP���T�W             TO ��ÒP���T�W�v�q.
025850     MOVE ���Z�|��É񐔂T�W             TO ��É񐔂T�W�v�q.
025860     MOVE ���Z�|��×��T�W               TO ��×��T�W�v�q.
025870     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
025880     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
025890     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
025900     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
025910     MOVE ���Z�|�d�É񐔂T�W             TO �d�É񐔂T�W�v�q.
025920     MOVE ���Z�|�d�×��T�W               TO �d�×��T�W�v�q.
025930     MOVE ���Z�|���v�T�W                 TO ���v�T�W�v�q.
025940     MOVE ���Z�|�����ʍ����v�T�W         TO �����ʍ����v�T�W�v�q.
025950     MOVE ���Z�|�����������T�W           TO �����������T�W�v�q.
025960     MOVE ���Z�|���������v�T�W           TO ���������v�T�W�v�q.
025970*    ****************
025980*    * �T���ʁ^10�� *
025990*    ****************
026000     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
026010     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
026020     MOVE ���Z�|��ÒP���T�O             TO ��ÒP���T�O�v�q.
026030     MOVE ���Z�|��É񐔂T�O             TO ��É񐔂T�O�v�q.
026040     MOVE ���Z�|��×��T�O               TO ��×��T�O�v�q.
026050     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
026060     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
026070     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
026080     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
026090     MOVE ���Z�|�d�É񐔂T�O             TO �d�É񐔂T�O�v�q.
026100     MOVE ���Z�|�d�×��T�O               TO �d�×��T�O�v�q.
026110     MOVE ���Z�|���v�T�O                 TO ���v�T�O�v�q.
026120     MOVE ���Z�|�����������T�O           TO �����������T�O�v�q.
026130     MOVE ���Z�|���������v�T�O           TO ���������v�T�O�v�q.
026140*
026150*================================================================*
026160 �{�p�����擾 SECTION.
026170*
026180**************************************************
026190* �{�@�f�[�^���g�p���A�ȉ��̏����擾           *
026200* �� �_���t�ԍ�.. �_���t�ԍ��v�Ɋi�[             *
026210* �� ����ԍ� ... �ڍ��t�����ԍ��v�Ɋi�[       *
026220* �� ��\�Җ� ... ��\�Җ��v�Ɋi�[               *
026230* �� �Z��1,2   ...�{�p���Z��1,2�v�Ɋi�[          *
026240* �� �d�b�ԍ� ... �{�p���d�b�ԍ��v�Ɋi�[         *
026250**************************************************
026260     MOVE ZERO  TO �{��|�{�p���ԍ�.
026270     READ �{�p�����}�X�^
026280     INVALID KEY
026290         CONTINUE
026300     NOT INVALID KEY
026310*
               MOVE �{��|�s���{���i�h�r    TO �s���{���i�h�r�v
026320         MOVE �{��|�V�_���t�ԍ�      TO �_���t�ԍ��v
026330*
026340         STRING "JM-"                  DELIMITED BY SIZE
026350                �{��|�ڍ��t�����ԍ� DELIMITED BY SIZE
026360           INTO �ڍ��t�����ԍ��v
026370         END-STRING
026380*
026390         MOVE �{��|�X�֔ԍ��P        TO �{�p���X�֔ԍ��P�v
026400         MOVE �{��|�X�֔ԍ��Q        TO �{�p���X�֔ԍ��Q�v
026410         MOVE �{��|��\�҃J�i        TO ��\�҃J�i�v
026420         MOVE �{��|��\�Җ�          TO ��\�Җ��v
026430*
026440         MOVE �{��|�ڍ��@��          TO �ڍ��@���v
026450*
026460*         MOVE �{��|�Z���P            TO �{�p���Z���P�v
026470*         MOVE �{��|�Z���Q            TO �{�p���Z���Q�v
026480         STRING �{��|�Z���P  DELIMITED BY SPACE
026490                �{��|�Z���Q  DELIMITED BY SPACE
026500           INTO �{�p���Z���v
026510         END-STRING
026520*
026530         MOVE �{��|�d�b�ԍ�          TO �{�p���d�b�ԍ��v
026540*
026550***         MOVE �{��|������s��      TO ������s���v
026560***         MOVE �{��|������s�x�X��  TO ������s�x�X���v
026570***         MOVE �{��|�a�����          TO �a����ʂv
026580***         MOVE �{��|�����ԍ�          TO �����ԍ��v
026590***         MOVE �{��|�������`�l�J�i    TO �������`�l�J�i�v
026600***         MOVE �{��|�������`�l        TO �������`�l�v
026610*
026620***         EVALUATE �a����ʂv
026630***         WHEN 1
026640***             MOVE NC"�i���j" TO �a����ʃR�����g�v
026650***         WHEN 2
026660***             MOVE NC"�i���j" TO �a����ʃR�����g�v
026670***         WHEN OTHER
026680***             MOVE SPACE      TO �a����ʃR�����g�v
026690***         END-EVALUATE
026700*
026710*------------------------------------------------------------------------*
026720         EVALUATE �ی���ʂv�q
026730         WHEN 01
026740             MOVE �ی��Ҕԍ��v�q       TO �ی��Ҕԍ���r�v
026750             PERFORM ���{�p�h�c�Z�b�g
               WHEN 05
026760         WHEN 08
026770             MOVE �ی��Ҕԍ��v�q(3:6)  TO �ی��Ҕԍ���r�v
026780             PERFORM ���{�p�h�c�Z�b�g
026790         WHEN 04
026800             PERFORM ���ϔԍ��Z�b�g
026810         WHEN 09
026820             PERFORM ���q���ԍ��Z�b�g
026830         END-EVALUATE
026840*
026850     END-READ.
026860*
026870*================================================================*
026880 ���{�p�h�c�Z�b�g SECTION.
026890*
026900*********************************************
026910** �h�c�Ǘ��}�X�^���  ���{�p�h�c���擾����B
026920*   (���ۑg���́A�ΏۊO�@���@�ΏہI2005/09 )
026930*********************************************
026940**   / ���{�pID /
026950     MOVE 01                     TO �h�c�ǁ|�h�c�敪.
026960     MOVE ZERO                   TO �h�c�ǁ|�{�p���ԍ�.
026970     MOVE �ی��Ҕԍ���r�v(1:2)  TO �h�c�ǁ|�ی����.
026980     MOVE SPACE                  TO �h�c�ǁ|�ی��Ҕԍ�.
026990     READ �h�c�Ǘ��}�X�^
027000     NOT INVALID KEY
027010         MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO ���{�p�h�c�v
027020     END-READ.
027030*
027040*================================================================*
027050 ���ϔԍ��Z�b�g SECTION.
027060*
027070**************************************************************
027080* �ی��Ҕԍ��ɂ��A���ς̔ԍ����󎚂��邩����
027090* �������L �ǉ� 99/10
027100**************************************************************
027110** 1.���ϑg���A��
027120     MOVE SPACE  TO  �E�o�t���O.
027130     IF ( �{��|���ϘA�ԍ� NOT = ZERO )
027140** ����(�ی��Ҕԍ�)
027150        IF ( �ی��Ҕԍ��v�q(1:2) = "31" )  OR
027160           ( �ی��Ҕԍ��v�q = "34130021" )
027170*
027180           MOVE  NC"���ϑg���A����"   TO ���ϘA�ԍ����m�v 
027190           MOVE  NC"��"               TO ���ϘA�ԍ��P�ʂm�v 
027200           MOVE  �{��|���ϘA�ԍ�     TO ���ϘA�ԍ��v
027210           IF ( ���ϘA�ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
027220                 MOVE SPACE TO  ���ϘA�ԍ��v(1:1)
027230           ELSE
027240                 MOVE "YES" TO  �E�o�t���O
027250           END-IF
027260           IF ( ���ϘA�ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
027270                 MOVE SPACE TO  ���ϘA�ԍ��v(2:1)
027280           ELSE
027290                 MOVE "YES" TO  �E�o�t���O
027300           END-IF
027310           IF ( ���ϘA�ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
027320                 MOVE SPACE TO  ���ϘA�ԍ��v(3:1)
027330           ELSE
027340                 MOVE "YES" TO  �E�o�t���O
027350           END-IF
027360           IF ( ���ϘA�ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
027370                 MOVE SPACE TO  ���ϘA�ԍ��v(4:1)
027380           ELSE
027390                 MOVE "YES" TO  �E�o�t���O
027400           END-IF
027410           IF ( ���ϘA�ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
027420                 MOVE SPACE TO  ���ϘA�ԍ��v(5:1)
027430           ELSE
027440                 MOVE "YES" TO  �E�o�t���O
027450           END-IF
027460           IF ( ���ϘA�ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
027470                 MOVE SPACE TO  ���ϘA�ԍ��v(6:1)
027480           ELSE
027490                 MOVE "YES" TO  �E�o�t���O
027500           END-IF
027510           MOVE  ���ϘA�ԍ��W�c�v     TO ���ϔԍ��v
027520        END-IF
027530     END-IF.
027540*
027550** 2. �n���ϋ��c��
027560     MOVE SPACE  TO  �E�o�t���O.
027570     IF ( �{��|�n���ϘA�ԍ� NOT = ZERO )
027580** ����(�ی��Ҕԍ�)
027590        IF ( �ی��Ҕԍ��v�q(1:2) = "32" OR "33" OR "34" )  AND
027600           ( �ی��Ҕԍ��v�q NOT = "34130021" )
027610*
027620           MOVE  NC"�n���ϋ��c���"   TO ���ϘA�ԍ����m�v 
027630           MOVE  NC"��"               TO ���ϘA�ԍ��P�ʂm�v 
027640           MOVE  �{��|�n���ϘA�ԍ�   TO ���ϘA�ԍ��v
027650           IF ( ���ϘA�ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
027660                 MOVE SPACE TO  ���ϘA�ԍ��v(1:1)
027670           ELSE
027680                 MOVE "YES" TO  �E�o�t���O
027690           END-IF
027700           IF ( ���ϘA�ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
027710                 MOVE SPACE TO  ���ϘA�ԍ��v(2:1)
027720           ELSE
027730                 MOVE "YES" TO  �E�o�t���O
027740           END-IF
027750           IF ( ���ϘA�ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
027760                 MOVE SPACE TO  ���ϘA�ԍ��v(3:1)
027770           ELSE
027780                 MOVE "YES" TO  �E�o�t���O
027790           END-IF
027800           IF ( ���ϘA�ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
027810                 MOVE SPACE TO  ���ϘA�ԍ��v(4:1)
027820           ELSE
027830                 MOVE "YES" TO  �E�o�t���O
027840           END-IF
027850           IF ( ���ϘA�ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
027860                 MOVE SPACE TO  ���ϘA�ԍ��v(5:1)
027870           ELSE
027880                 MOVE "YES" TO  �E�o�t���O
027890           END-IF
027900           IF ( ���ϘA�ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
027910                 MOVE SPACE TO  ���ϘA�ԍ��v(6:1)
027920           ELSE
027930                 MOVE "YES" TO  �E�o�t���O
027940           END-IF
027950           MOVE  ���ϘA�ԍ��W�c�v     TO ���ϔԍ��v
027960        END-IF
027970**
027980**------/  �n���ϘA�ԍ��������͂ŁA���ϘA�ԍ������͂���Ă��鎞�́A�������ɋ��ϘA�ԍ�
027990**         ���Z�b�g����B(�������L)   /
028000     ELSE
028010        IF ( �{��|���ϘA�ԍ� NOT = ZERO ) AND ( ���ϔԍ��v = SPACE )
028020*
028030           MOVE  NC"���ϑg���A����"   TO ���ϘA�ԍ����m�v 
028040           MOVE  NC"��"               TO ���ϘA�ԍ��P�ʂm�v 
028050           MOVE  �{��|���ϘA�ԍ�     TO ���ϘA�ԍ��v
028060           IF ( ���ϘA�ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
028070                 MOVE SPACE TO  ���ϘA�ԍ��v(1:1)
028080           ELSE
028090                 MOVE "YES" TO  �E�o�t���O
028100           END-IF
028110           IF ( ���ϘA�ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
028120                 MOVE SPACE TO  ���ϘA�ԍ��v(2:1)
028130           ELSE
028140                 MOVE "YES" TO  �E�o�t���O
028150           END-IF
028160           IF ( ���ϘA�ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
028170                 MOVE SPACE TO  ���ϘA�ԍ��v(3:1)
028180           ELSE
028190                 MOVE "YES" TO  �E�o�t���O
028200           END-IF
028210           IF ( ���ϘA�ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
028220                 MOVE SPACE TO  ���ϘA�ԍ��v(4:1)
028230           ELSE
028240                 MOVE "YES" TO  �E�o�t���O
028250           END-IF
028260           IF ( ���ϘA�ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
028270                 MOVE SPACE TO  ���ϘA�ԍ��v(5:1)
028280           ELSE
028290                 MOVE "YES" TO  �E�o�t���O
028300           END-IF
028310           IF ( ���ϘA�ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
028320                 MOVE SPACE TO  ���ϘA�ԍ��v(6:1)
028330           ELSE
028340                 MOVE "YES" TO  �E�o�t���O
028350           END-IF
028360           MOVE  ���ϘA�ԍ��W�c�v     TO ���ϔԍ��v
028370        END-IF
028380*
028390     END-IF.
028400*
028410*================================================================*
028420 ���q���ԍ��Z�b�g SECTION.
028430*
028440     MOVE SPACE  TO  �E�o�t���O.
028450     IF ( �{��|���q���ԍ� NOT = ZERO )
028451           IF �{��|�h�q�ȋ敪 = 1
028452              MOVE  NC"�h�q�ȑ�"      TO ���q���ԍ����m�v 
028453           ELSE
028454              MOVE  NC"�h�q����"      TO ���q���ԍ����m�v 
028455           END-IF
028460*           MOVE  NC"�h�q����"         TO ���q���ԍ����m�v 
028470           MOVE  NC"��"               TO ���q���ԍ��P�ʂm�v 
028480           MOVE  �{��|���q���ԍ�     TO ���q���ԍ��v
028490           IF ( ���q���ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
028500                 MOVE SPACE TO  ���q���ԍ��v(1:1)
028510           ELSE
028520                 MOVE "YES" TO  �E�o�t���O
028530           END-IF
028540           IF ( ���q���ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
028550                 MOVE SPACE TO  ���q���ԍ��v(2:1)
028560           ELSE
028570                 MOVE "YES" TO  �E�o�t���O
028580           END-IF
028590           IF ( ���q���ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
028600                 MOVE SPACE TO  ���q���ԍ��v(3:1)
028610           ELSE
028620                 MOVE "YES" TO  �E�o�t���O
028630           END-IF
028640           IF ( ���q���ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
028650                 MOVE SPACE TO  ���q���ԍ��v(4:1)
028660           ELSE
028670                 MOVE "YES" TO  �E�o�t���O
028680           END-IF
028690           IF ( ���q���ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
028700                 MOVE SPACE TO  ���q���ԍ��v(5:1)
028710           ELSE
028720                 MOVE "YES" TO  �E�o�t���O
028730           END-IF
028740           IF ( ���q���ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
028750                 MOVE SPACE TO  ���q���ԍ��v(6:1)
028760           ELSE
028770                 MOVE "YES" TO  �E�o�t���O
028780           END-IF
028790           MOVE  ���q���ԍ��W�c�v     TO ���ϔԍ��v
028800     END-IF.
028810*
028820*================================================================*
028830 ��f�ҏ��擾 SECTION.
028840*
028850**************************************************
028860* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
028870* �� �{�p�N ..... �{�p�N�v�Ɋi�[                 *
028880* �� �{�p�� ..... �{�p���v�Ɋi�[                 *
028890* �� ���Ҕԍ�.... ���Ҕԍ��v�Ɋi�[���e�c�A�ԗp   *
028900* �� �L�� ....... �L���v�Ɋi�[                   *
028910* �� �ԍ� ....... �ԍ��v�Ɋi�[                   *
028920* �� �ی��Ҕԍ� . �ی��Ҕԍ��v�Ɋi�[             *
028930* �� �ی���� ... �ی���ʂv�Ɋi�[               *
028940* �� ��ی��҃J�i.��ی��҃J�i�v�Ɋi�[           *
028950* �� ��ی��Ҏ���.��ی��Ҏ����v�Ɋi�[           *
028960* �� �Z���P ......��ی��ҏZ���P�v�Ɋi�[         *
028970* �� �Z���Q ......��ی��ҏZ���Q�v�Ɋi�[         *
028980* �� ���҃J�i ....���҃J�i�v�Ɋi�[               *
028990* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
029000* �� ���Ґ��� ....�敪�ɂ��`�F�b�N��"��"���i�[ *
029010* �� ���Ҙa�� ....�a��ɂ��`�F�b�N��"��"���i�[ *
029020* �� ���ҔN ......���ҔN�v�Ɋi�[                 *
029030* �� ���Ҍ� ......���Ҍ��v�Ɋi�[                 *
029040* �� ���ғ� ......���ғ��v�Ɋi�[                 *
029050* �� ���� ........���̃}�X�^��葱���v�Ɏ擾     *
029060**************************************************
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
               WHEN 04
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
      */�{�Ƌ敪�͂ǂꂩ�P�Ɂ�������B
               IF ��|�ی���� = 05
                   EVALUATE ��|���ʋ敪
                   WHEN 1
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
      *         IF ��|�ی���� = 01 OR 08
                   EVALUATE ���Z�|���S����
                   WHEN ZERO
                       MOVE NC"��" TO �P�O���`�F�b�N�v
                   WHEN 1
                       MOVE NC"��" TO �X���`�F�b�N�v
      */�_�ސ쌧�̏ꍇ�A�O������҂P���́A���t�������W���ɂ���B(�����P�����S���邽�߁A���҂P���A�ی��҂W���A���P���ƂȂ�)
                       IF (��|�ی����     = 01 AND ��|�ی��Ҕԍ�(1:2) = "14") OR
                          (��|�ی���� NOT = 01 AND ��|�ی��Ҕԍ�(3:2) = "14")
                           IF (��|�ی���� NOT = 05 ) AND (��|���ʋ敪 = 1)
                               MOVE SPACE  TO �X���`�F�b�N�v
                               MOVE NC"��" TO �W���`�F�b�N�v
                           END-IF
                       END-IF
                   WHEN 2
                       MOVE NC"��" TO �W���`�F�b�N�v
                   WHEN 3
                       MOVE NC"��" TO �V���`�F�b�N�v
                   END-EVALUATE
      *         END-IF
      */�����C��/20190426
               MOVE ��|�{�p�a��     TO �{�p�a��v
029160         MOVE ��|�{�p�N       TO �{�p�N�v
029170         MOVE ��|�{�p��       TO �{�p���v
029180         MOVE ��|���Ҕԍ�     TO ���Ҕԍ��v
027850*         MOVE ��|�L��         TO �L���v
029200*         MOVE ��|�ԍ�         TO �ԍ��v
      *                                          
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
      *
029210         MOVE ��|�ی��Ҕԍ�   TO �ی��Ҕԍ��v �ی��Ҕԍ��v�o
029220         MOVE ��|�ی����     TO �ی���ʂv
029230         MOVE ��|������     TO �����ʂv�q
029240         MOVE ��|�������     TO ������ʂv�q
029250** �S���y�؂̎}�ԍ폜
029260         IF ( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(1:6) = "133033" )
029270            MOVE ��|�ی��Ҕԍ�(1:6)  TO �ی��Ҕԍ��v �ی��Ҕԍ��v�o
029280         END-IF
029290**
029300         MOVE ��|��ی��҃J�i TO ��ی��҃J�i�v
029310         MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ����v
029320         MOVE ��|�X�֔ԍ��P   TO �X�֔ԍ��P�v
029330         MOVE ��|�X�֔ԍ��Q   TO �X�֔ԍ��Q�v
029340         MOVE ��|�Z���P       TO ��ی��ҏZ���P�v
029350         MOVE ��|�Z���Q       TO ��ی��ҏZ���Q�v
029360*         STRING ��|�Z���P    DELIMITED BY SPACE
029370*                ��|�Z���Q    DELIMITED BY SPACE
029380*                INTO ��ی��ҏZ���v
029390*         END-STRING
      */ �d�b�ԍ��ǉ� /42505
               IF ��|�d�b�ԍ� NOT = SPACE
                  STRING "�d�b:"        DELIMITED BY SIZE
                         ��|�d�b�ԍ�   DELIMITED BY SPACE
                    INTO �d�b�ԍ��v
                  END-STRING
               ELSE
                  IF ��|���ғd�b�ԍ� NOT = SPACE
                     STRING "�d�b:"            DELIMITED BY SIZE
                            ��|���ғd�b�ԍ�   DELIMITED BY SPACE
                       INTO �d�b�ԍ��v
                     END-STRING
                  END-IF
               END-IF
029400         MOVE ��|���҃J�i     TO ���҃J�i�v
029410         MOVE ��|���Ҏ���     TO ���Ҏ����v
029420         MOVE ��|��p���S�Ҕԍ� TO �s�����ԍ��v
029430         MOVE ��|��v�Ҕԍ��V�l TO �󋋎Ҕԍ��v
029440*
029450         EVALUATE ��|���Ґ���
029460         WHEN 1
029470*             MOVE NC"�j"  TO ���ʂv
029480             MOVE NC"��"  TO �j�`�F�b�N�v
029490         WHEN 2
029500*             MOVE NC"��"  TO ���ʂv
029510             MOVE NC"��"  TO ���`�F�b�N�v
029520         END-EVALUATE
029530*
029540         EVALUATE ��|���Ҙa��
029550         WHEN 1
029560*             MOVE NC"����"  TO �����v
029570             MOVE NC"��"    TO �����`�F�b�N�v
029580         WHEN 2
029590*             MOVE NC"�吳"  TO �����v
029600             MOVE NC"��"    TO �吳�`�F�b�N�v
029610         WHEN 3
029620*             MOVE NC"���a"  TO �����v
029630             MOVE NC"��"    TO ���a�`�F�b�N�v
029640         WHEN 4
029650*             MOVE NC"����"  TO �����v
029660             MOVE NC"��"    TO �����`�F�b�N�v
      */�����C��/20190426
023060         WHEN 5
                   MOVE "5��"   TO �ߘa�b�l�v
023070             MOVE NC"��"  TO �ߘa�`�F�b�N�v
029670         END-EVALUATE
029680*
      */�����C��/������20190426
029310         IF ��|���Ҙa�� > 4
037370             MOVE ��|���Ҙa��     TO ���|�����敪
037380             READ �����}�X�^
037390             NOT INVALID KEY
037400                 MOVE ���|�������� TO �����v
037410             END-READ
029330         END-IF
      */�����C��/������20190426
029690         MOVE ��|���ҔN  TO ���ҔN�v
029700         MOVE ��|���Ҍ�  TO ���Ҍ��v
029710         MOVE ��|���ғ�  TO ���ғ��v
029720*
029730* �����ݒ�
029740         IF ( �{�l�Ƒ��敪�v�q = 1 )
029750            MOVE NC"�{�l"    TO �����v
029770         ELSE
029780            MOVE NC"�Ƒ�"    TO �����v
029880         END-IF
029890**
029900         IF ( ��|�ی���� = 01 OR 08 OR 05) AND
029910            ( ��|������� NOT = ZERO )
029920            PERFORM �������Z�܂Ƃߔ���
029930         ELSE
029940            MOVE SPACE TO �������Z�܂Ƃ߃t���O
029950         END-IF
030250     END-IF.
030260*
030270     EVALUATE �ی���ʂv�q
030280     WHEN 01
030290         IF ��|�ی��Ҕԍ�(3:1) = "3"
030300             MOVE NC"���g" TO �ی���ʖ��̂v
030310         ELSE
030320             MOVE NC"����" TO �ی���ʖ��̂v
030330         END-IF
030340*         MOVE NC"��" TO ���ۃ`�F�b�N�v
030350     WHEN 02
030360         MOVE NC"�Е�" TO �ی���ʖ��̂v
030370*         MOVE NC"��" TO �Еۃ`�F�b�N�v
030380     WHEN 03
030390         MOVE NC"�g��" TO �ی���ʖ��̂v
030400*         MOVE NC"��" TO �g���`�F�b�N�v
030410     WHEN 04
030420         MOVE NC"����" TO �ی���ʖ��̂v
030430*         MOVE NC"��" TO ���σ`�F�b�N�v
030440     WHEN 06
030450         MOVE NC"�Е�" TO �ی���ʖ��̂v
030460*         MOVE NC"��" TO �Еۃ`�F�b�N�v
030440     WHEN 05
029980         IF ( ��|�{�p�a��N�� >= 42004 )
030450             MOVE NC"�㍂" TO �ی���ʖ��̂v
030460*             MOVE NC"��" TO �V�l�`�F�b�N�v
               END-IF
030470     WHEN 07
030480         MOVE NC"�D��" TO �ی���ʖ��̂v
030490*         MOVE NC"��" TO �D���`�F�b�N�v
030500     WHEN 08
030510         MOVE NC"�ލ�" TO �ی���ʖ��̂v
030520*         MOVE NC"��" TO �ސE�`�F�b�N�v
030530     WHEN 09
030540         MOVE NC"���q" TO �ی���ʖ��̂v
030550*         MOVE NC"��" TO ���q�`�F�b�N�v
030560     END-EVALUATE.
030720*================================================================*
030730 ��������擾 SECTION.
030740*
030750****************************************************
030760* �A���f�[�^����ی��҃}�X�^��萿������擾����B *
030780* �� ������...... �����於�̂v�Ɋi�[               *
030790****************************************************
030800     MOVE �ی���ʂv�q   TO �ہ|�ی����.
030810     MOVE �ی��Ҕԍ��v�q TO �ہ|�ی��Ҕԍ�.
030820     READ �ی��҃}�X�^
030830     INVALID KEY
               IF ( �ی���ʂv�q = 05 ) AND ( �{�p�a��N���v�q >= 42004 )
030800             MOVE �ی���ʂv�q   TO �s�|������
030810             MOVE �ی��Ҕԍ��v�q TO �s�|�s�����ԍ�
030820             READ �s�����}�X�^
030830             INVALID KEY
030840                 MOVE SPACE      TO �����於�̂v �����於�̂v�s
030850             NOT INVALID KEY
031330                 MOVE �s�|�s��������    TO �����於�̂v
030920                 STRING �s�|�s��������      DELIMITED BY SPACE
030930                        "��"                DELIMITED BY SIZE
                            "�@�a"                DELIMITED BY SIZE
030940                        INTO �����於�̂v�s
030950                 END-STRING
                   END-READ
               ELSE
030840             MOVE SPACE      TO �����於�̂v �����於�̂v�s
               END-IF
030850     NOT INVALID KEY
030870                 EVALUATE �ی���ʂv�q 
030880                 WHEN  01
030890                 WHEN  07
030900                 WHEN  08
030910                     MOVE �ہ|�ی��Җ���    TO �����於�̂v
030920                     STRING �ہ|�ی��Җ���      DELIMITED BY SPACE
030930                            "��"                DELIMITED BY SIZE
                                "�@�a"                DELIMITED BY SIZE
030940                            INTO �����於�̂v�s
030950                     END-STRING
030860* �ЕہA���ق́u�Љ�ی��������v������
030960                 WHEN  02
030970                 WHEN  06
030980                     IF ( �ہ|�ڔ���敪 = 1 )
030990                        MOVE �ہ|�ی��Җ���    TO �����於�̂v
031000                        STRING �ہ|�ی��Җ���      DELIMITED BY SPACE
031010                               "��"                DELIMITED BY SIZE
                                   "�@�a"                DELIMITED BY SIZE
031020                               INTO �����於�̂v�s
031030                        END-STRING
031040                     ELSE
031050*                        STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
031060*                               "�Љ�ی�������"  DELIMITED BY SIZE
031070*                               INTO �����於�̂v
031080*                        END-STRING
031090                        STRING �ہ|�ی��Җ���      DELIMITED BY SPACE
031100                               "�Љ�ی���������"  DELIMITED BY SIZE
                                   "�@�a"                DELIMITED BY SIZE
031110                               INTO �����於�̂v�s
031120                        END-STRING
031130                     END-IF
031140* �g���͎x�����܂ň�
031150                 WHEN  03
031160                     STRING �ہ|�ی��Җ���  DELIMITED BY SPACE
031170                            "���N�ی��g��"  DELIMITED BY SIZE
031180                            "  "            DELIMITED BY SIZE
031190                            �ہ|�x��������  DELIMITED BY SPACE
                               "�@�a"             DELIMITED BY SIZE
031200                            INTO �����於�̂v
031210                     END-STRING
031220                     MOVE �����於�̂v  TO �����於�̂v�s
031230* ���ς͎x�����܂ň�
031240                 WHEN  04
031250                     STRING �ہ|�ی��Җ���  DELIMITED BY SPACE
031260                            "���ϑg��"      DELIMITED BY SIZE
031270                            "  "            DELIMITED BY SIZE
031280                            �ہ|�x��������  DELIMITED BY SPACE
                               "�@�a"             DELIMITED BY SIZE
031290                            INTO �����於�̂v
031300                     END-STRING
031310                     MOVE �����於�̂v  TO �����於�̂v�s
031320                 WHEN OTHER
031330                     MOVE �ہ|�ی��Җ���    TO �����於�̂v �����於�̂v�s
                           STRING �����於�̂v DELIMITED BY SPACE
                                  "�@�a"       DELIMITED BY SIZE
                                  INTO �����於�̂v
                           END-STRING
031310                     MOVE �����於�̂v  TO �����於�̂v�s
031340                 END-EVALUATE
031350     END-READ.
031360*
031370*================================================================*
031380 �����f�[�^�擾 SECTION.
031390*
031400**************************************************
031410* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾 *
031420* �� ������...���ʁ{������ʂɂĉ��H���Ċi�[     *
031430* �� �����N.......�����N�v                       *
031440* �� ������.......�������v                       *
031450* �� ������.......�������v                       *
031460* �� �J�n�N.......�����N�v                       *
031470* �� �J�n��.......�������v                       *
031480* �� �J�n��.......�������v                       *
031490* �� �I���N.......�I���N�v                       *
031500* �� �I����.......�I�����v                       *
031510* �� �I����.......�I�����v                       *
031520* �� ������.......�������v                       *
031530* �� �]�A�敪 ....�敪�ɂ��`�F�b�N��"��"���i�[ *
031540* �� �������q ....�敪�ɂ��`�F�b�N��"��"���i�[ *
031550* �� �o�߃R�[�h...�o�߃}�X�^���擾             *
031560**************************************************
           IF ���|���R�[�h NOT = SPACE
031660         MOVE ���|���ʐ�                   TO ���ʐ��v
031670         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
031680                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
031690             MOVE ���|�������(���ʂb�m�s) TO ������ʂv(���ʂb�m�s)
031700             MOVE ���|����(���ʂb�m�s)     TO ���ʂv(���ʂb�m�s)
031710             MOVE ���|���E�敪(���ʂb�m�s) TO ���E�敪�v(���ʂb�m�s)
031720             MOVE ���|�����ʒu�ԍ�(���ʂb�m�s)
031730                                           TO �����ʒu�ԍ��v(���ʂb�m�s)
031740*********************************************
031750* ���j�S�_...������ʁ{���ʂɂĉ��H���Ċi�[ *
031760*********************************************
031770* �������
031780             MOVE SPACE                     TO �������̂v
031790             MOVE 03                        TO ���|�敪�R�[�h
031800             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
031810             READ ���̃}�X�^
031820             INVALID KEY
031830                 MOVE SPACE        TO �������̂v
031840             NOT INVALID KEY
031850                 MOVE ���|�������� TO �������̂v
031860             END-READ
031870* ����
020710             MOVE SPACE                    TO �������v(���ʂb�m�s)
032680*
032690             PERFORM ���ʖ��̖�������
030170*
032070             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
032080             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
032090             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
032100             MOVE ���|�J�n�N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
032110             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
032120             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
032130             IF ( ���|�]�A�敪(���ʂb�m�s) = 9 )
032140                 MOVE 99                   TO �I���N�v(���ʂb�m�s)
032150                 MOVE 99                   TO �I�����v(���ʂb�m�s)
032160                 MOVE 99                   TO �I�����v(���ʂb�m�s)
032170             ELSE
032180                 MOVE ���|�I���N(���ʂb�m�s)   TO �I���N�v(���ʂb�m�s)
032190                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
032200                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
032210             END-IF
032220* �o�ߗ��̎擾
032230             MOVE 01                         TO �o�|�敪�R�[�h
032240             MOVE ���|�o�߃R�[�h(���ʂb�m�s) TO �o�|�o�߃R�[�h
032250             READ �o�߃}�X�^
032260             INVALID KEY
032270                 MOVE ZERO            TO ���ʂb�m�s�v(���ʂb�m�s)
032280                 MOVE SPACE           TO ���ʋ�؂v(���ʂb�m�s)
032290                 MOVE SPACE           TO �o�ߗ��̂v(���ʂb�m�s)
032300             NOT INVALID KEY
032310*
032320                 EVALUATE ���ʂb�m�s
032330                 WHEN 1
032340                     MOVE NC"�@" TO �o�ߕ��ʂv
032350                 WHEN 2
032360                     MOVE NC"�A" TO �o�ߕ��ʂv
032370                 WHEN 3
032380                     MOVE NC"�B" TO �o�ߕ��ʂv
032390                 WHEN 4
032400                     MOVE NC"�C" TO �o�ߕ��ʂv
032410                 WHEN 5
032420                     MOVE NC"�D" TO �o�ߕ��ʂv
032430                 END-EVALUATE
032440                 STRING  �o�ߕ��ʂv     DELIMITED BY SPACE
032450                         �o�|�o�ߗ���   DELIMITED BY SPACE
032460                        INTO ����o�ߗ��̂v(���ʂb�m�s)
032470                 END-STRING
032480*
032490             END-READ
032500*
032510             MOVE ���|�]�A�敪(���ʂb�m�s) TO �]�A�敪�v(���ʂb�m�s)
032520             EVALUATE ���|�]�A�敪(���ʂb�m�s)
032530             WHEN 1
032540             WHEN 2
032550                 MOVE NC"��"               TO �����`�F�b�N�v(���ʂb�m�s)
032560             WHEN 3
032570                 MOVE NC"��"               TO ���~�`�F�b�N�v(���ʂb�m�s)
032580             WHEN 4
032590                 MOVE NC"��"               TO �]��`�F�b�N�v(���ʂb�m�s)
032600             END-EVALUATE
032610*
                   MOVE ���Z�|���ʎ�����(���ʂb�m�s) TO �������v(���ʂb�m�s)
032620         END-PERFORM
032630* �V�K/�p�� �`�F�b�N
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
032690*
032700* �}�Ԕ���p
032710         MOVE ���|�J�n�f�Ó��蓮�敪   TO �J�n�f�Ó��蓮�敪�v
032720* ������������敪
032730         MOVE ���|���Z������������敪 TO ���Z������������敪�v
027880         MOVE ���|���Z�������R����敪 TO ���Z�������R����敪�v
032740*
032750     END-IF.
032760*================================================================*
030910 ���ʖ��̖������� SECTION.
030920*
006490     STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
009980            �������̂v                    DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
006520       INTO �������v(���ʂb�m�s)
006570     END-STRING.
031050*
032770*================================================================*
032780 �{�p�L�^�擾 SECTION.
032790*
032800************************************************************
032810* ��P�f�[�^���畉���f�[�^�e���ȉ��̏����擾           *
032820* �� �������Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
032830* �� ���É��Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
032840************************************************************
032850     MOVE  SPACE  TO  �����Č��t���O.
032860     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���ʐ��v
032870         IF ( �{�p�N�v = �����N�v(���ʂb�m�s) ) AND
032880            ( �{�p���v = �������v(���ʂb�m�s) )
032890             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
032900             MOVE �}�Ԃv�q              TO �{�L�|�}��
032910             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
032920             MOVE �����N�v(���ʂb�m�s)  TO �J�n�N�v(���ʂb�m�s) �{�L�|�{�p�N
032930             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
032940             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
032950         ELSE
032960             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
032970             MOVE �}�Ԃv�q              TO �{�L�|�}��
032980             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
032990             MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
033000             MOVE �{�p���v�q            TO �{�L�|�{�p��
033010             MOVE ZERO                  TO �{�L�|�{�p��
033020         END-IF
033030         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
033040                                      �{�L�|�{�p�a��N����
033050         END-START
033060         IF ( ��ԃL�[ = "00" )
033080             MOVE ZERO  TO �I���N�v�s
033090             MOVE ZERO  TO �I�����v�s
033100             MOVE ZERO  TO �I�����v�s
033110             MOVE SPACE TO �I���t���O�Q
033120             PERFORM �{�p�L�^�e�Ǎ�
033130             IF ( �I���t���O�Q      = SPACE   ) AND
033140                ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
033150                ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
033160                ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
033170                ( �{�L�|�{�p��      = �{�p���v�q     ) 
033180*
033190*        *****************************************************************
033200*        * �J�n�N���� ( ���̕��ʂ����������łȂ����A
033210*                       ���������ł��}�Ԃ����鎞�́A�ŏ��̎{�p�����J�n��)*
033220*        *****************************************************************
033230                 IF ( �{�p�N�v NOT = �����N�v(���ʂb�m�s) ) OR
033240                    ( �{�p���v NOT = �������v(���ʂb�m�s) ) OR
033250                    ( �J�n�f�Ó��蓮�敪�v = 1 )
033260                     MOVE �{�L�|�{�p�N   TO �J�n�N�v(���ʂb�m�s)
033270                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
033280                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
033290                 END-IF
033300             END-IF
033310             PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
033320                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
033330                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
033340                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
033350                           ( �{�L�|�{�p��     NOT = �{�p���v�q       ) OR
033360                           ( �{�L�|�{�p��         > �I�����v(���ʂb�m�s))
033370*               **********
033380*               * ������ *
033390*               **********
033410                MOVE �{�L�|�{�p�N               TO �I���N�v�s
033420                MOVE �{�L�|�{�p��               TO �I�����v�s
033430                MOVE �{�L�|�{�p��               TO �I�����v�s
033440*
033450                PERFORM �{�p�L�^�e�Ǎ�
033460            END-PERFORM
033470        END-IF
033480*       **************************
033490*       * �p���F�I���N�����Z�b�g *
033500*       **************************
033510        IF ( �]�A�敪�v(���ʂb�m�s) = 9 )
033520            MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
033530            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
033540            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
033550        END-IF
033560        IF ( �I���N�����v(���ʂb�m�s) > �󗝔N�����v )
033570            MOVE �I���N�v(���ʂb�m�s) TO �󗝔N�v
033580            MOVE �I�����v(���ʂb�m�s) TO �󗝌��v
033590            MOVE �I�����v(���ʂb�m�s) TO �󗝓��v
033600        END-IF
033610     END-PERFORM.
033620*
033630** ----- �O�������݂̂��𔻒� -----------*
033640*
033650*     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
033660*     MOVE �}�Ԃv�q              TO �{�L�|�}��.
033670*     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
033680*     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
033690*     MOVE �{�p���v�q            TO �{�L�|�{�p��.
033700*     MOVE ZERO                  TO �{�L�|�{�p��.
033710*     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
033720*                                  �{�L�|�{�p�a��N����
033730*     END-START.
033740*     IF ( ��ԃL�[ = "00" )
033750*             MOVE SPACE TO �I���t���O�Q
033760*             PERFORM �{�p�L�^�e�Ǎ�
033770*             IF ( �I���t���O�Q      = SPACE   ) AND
033780*                ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
033790*                ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
033800*                ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
033810*                ( �{�L�|�{�p��      = �{�p���v�q     ) 
033820** �����{�p�J�n�����Č����ǂ�������
033830*                 IF ( �{�L�|�Č������� = 1 )
033840*                      MOVE "YES"  TO  �����Č��t���O
033850*                 END-IF
033860**
033870*             END-IF
033880*     END-IF.
033890*     IF ( �����Č��t���O = "YES" )
033900*        PERFORM �O�������̂ݔ���
033910*     END-IF.
033920*
033930*================================================================*
033940*================================================================*
033950 ���Z�v�g���я��擾 SECTION.
033960*================================================================*
033970     MOVE �{�p�a��v�q       TO ��S�|�{�p�a��.
033980     MOVE �{�p�N�v�q         TO ��S�|�{�p�N.
033990     MOVE �{�p���v�q         TO ��S�|�{�p��.
034000     MOVE ���҃R�[�h�v�q     TO ��S�|���҃R�[�h.
034010     MOVE �ی���ʂv�q       TO ��S�|�ی����.
034020     READ ��ƃt�@�C���S
034030     NOT INVALID KEY
034040          MOVE ��S�|����    TO ���Ԃv
034050     END-READ.
034060     MOVE "DNo.=    :"       TO ���ԌŒ�v.
034070*
034080*================================================================*
034090 �{�p�L�^�e�Ǎ� SECTION.
034100*
034110     READ �{�p�L�^�e NEXT
034120     AT END
034130         MOVE "YES" TO �I���t���O�Q
034140     END-READ.
034150*================================================================*
034160 ������� SECTION.
034170*
034180     MOVE "YJK6125P"  TO  ��`�̖��o.
034190     MOVE "SCREEN"   TO  ���ڌQ���o.
034200     WRITE YJK6125P.
034210***     WRITE ������R�[�h.
034220     PERFORM �G���[�����o.
034230*================================================================*
034240 �G���[�����o SECTION.
034250*
034260     IF ( �ʒm���o NOT = "00" )
034270         DISPLAY NC"���[�G���["              UPON CONS
034280         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
034290         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
034300         DISPLAY NC"�g������o�F" �g������o UPON CONS
034310         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
034320                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
034330         ACCEPT  �L�[���� FROM CONS
034340         PERFORM �t�@�C����
034350         MOVE 99 TO PROGRAM-STATUS
034360         EXIT PROGRAM
034370     END-IF.
034610*================================================================*
034620 �������ȑO�̃f�[�^���� SECTION.
034630*
034640*********************************************************************************
034650*  �ŏ��̏������ȑO�̓������Ɏ{�p�L�^���R�[�h����������(�����A���~)�́A�����敪��
034660*  �p���ɂ��`�F�b�N����B(�V�K�ƌp���̗���)
034670*********************************************************************************
034680** �ŏ��̏��������擾
034690     MOVE SPACE                 TO �����t���O.
034700     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
034710     MOVE �}�Ԃv�q              TO �{�L�|�}��.
034720     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
034730     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
034740     MOVE �{�p���v�q            TO �{�L�|�{�p��.
034750     MOVE ZERO                  TO �{�L�|�{�p��.
034760     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
034770                                  �{�L�|�{�p�a��N����
034780     END-START.
034790     IF ( ��ԃL�[ = "00" )
034800         MOVE ZERO  TO �����a��v�s
034810         MOVE ZERO  TO �����N�v�s
034820         MOVE ZERO  TO �������v�s
034830         MOVE ZERO  TO �������v�s
034840         MOVE SPACE TO �I���t���O�Q
034850         PERFORM �{�p�L�^�e�Ǎ�
034860         PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
034870                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
034880                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
034890                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
034900                       ( �{�L�|�{�p��     NOT = �{�p���v�q      ) OR
034910                       ( �����t���O           = "YES"           ) 
034920               IF ( �{�L�|�f�Ë敪 = 2 )
034930                   MOVE �{�L�|�{�p�a��           TO �����a��v�s
034940                   MOVE �{�L�|�{�p�N             TO �����N�v�s
034950                   MOVE �{�L�|�{�p��             TO �������v�s
034960                   MOVE �{�L�|�{�p��             TO �������v�s
034970                   MOVE "YES"                    TO �����t���O
034980               END-IF
034990               PERFORM �{�p�L�^�e�Ǎ�
035000         END-PERFORM
035010     END-IF.
035020*
035030* �������ȑO�̃f�[�^����
035040     IF ( �����t���O = "YES" )
035050        MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
035060        MOVE �}�Ԃv�q              TO �{�L�|�}��
035070        MOVE �����a��v�s          TO �{�L�|�{�p�a��
035080        MOVE �����N�v�s            TO �{�L�|�{�p�N
035090        MOVE �������v�s            TO �{�L�|�{�p��
035100        MOVE �������v�s            TO �{�L�|�{�p��
035110        START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
035120                                     �{�L�|�{�p�a��N����
035130                                     REVERSED
035140        END-START
035150        IF ( ��ԃL�[ = "00" )
035160           MOVE SPACE  TO �I���t���O�Q
035170           PERFORM �{�p�L�^�e�Ǎ�
035180           IF ( �I���t���O�Q    = SPACE        ) AND
035190              ( �{�L�|���Ҕԍ�  = ���Ҕԍ��v�q ) AND
035200              ( �{�L�|�}��      = �}�Ԃv�q     ) AND
035210              ( �{�L�|�{�p�a��  = �����a��v�s ) AND
035220              ( �{�L�|�{�p�N    = �����N�v�s   ) AND
035230              ( �{�L�|�{�p��    = �������v�s   )
035240*  �������ȑO�̓������Ɏ{�p�L�^���R�[�h����������
035250                IF ( �p���`�F�b�N�v = SPACE )
035260                   MOVE NC"��"    TO �p���`�F�b�N�v
035270                END-IF
035280           END-IF
035290         END-IF
035300     END-IF.
035310*
035320*================================================================*
035330 ��������擾 SECTION.
035340*
035350* �R�J���ȏ�̒�������� "CHOUKI" ���Ă�. 
035360     MOVE  SPACE TO  �A���ԁ|�L�[.
035370     INITIALIZE      �A���ԁ|�L�[.
035380     MOVE �{�p�a��v�q  TO  �A���ԁ|�{�p�a��.
035390     MOVE �{�p�N�v�q    TO  �A���ԁ|�{�p�N.
035400     MOVE �{�p���v�q    TO  �A���ԁ|�{�p��.
035410     MOVE ���Ҕԍ��v�q  TO  �A���ԁ|���Ҕԍ�.
035420     MOVE �}�Ԃv�q      TO  �A���ԁ|�}��.
035430*
035440     CALL   "CHOUKI".
035450     CANCEL "CHOUKI".
035460*
035470**** �K�p�P���g�p (�u�O�������̂݁v�����鎞�́A��������)
035480     IF ( �A���ԁ|�Ώۃt���O  = "YES" )
035490        IF ( �K�p�P�v  = SPACE )
035500           MOVE NC"�������{�p�p�����R���ʂɋL��"  TO �K�p�P�v
035510        ELSE
035520           STRING �K�p�P�v           DELIMITED BY SPACE
035530                  NC"�C"             DELIMITED BY SIZE
035540                  NC"�������{�p�p�����R���ʂɋL��"   DELIMITED BY SIZE
035550                  INTO �K�p�P�v
035560           END-STRING
035570        END-IF
035580     END-IF.
035590*
035600*================================================================*
035610 �������Z�����擾 SECTION.
035620*****************************************************************
035630** �������Z�����ԊO�Ɛ[��̎��A�K�p�Ɂu��t���ԁv���󎚂���B
035640**   �����̈󎚂͌�3��܂ŉ\
035650*****************************************************************
035660     IF ( ���Z�|���ԊO = 1 ) OR ( ���Z�|�[�� = 1 ) OR ( ���Z�|�x�� = 1 )
035670*
035680         MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
035690         MOVE �}�Ԃv�q              TO �{�L�|�}��
035700         MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
035710         MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
035720         MOVE �{�p���v�q            TO �{�L�|�{�p��
035730         MOVE ZERO                  TO �{�L�|�{�p��
035740         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
035750                                      �{�L�|�{�p�a��N����
035760         END-START
035770         IF ( ��ԃL�[ = "00" )
035780             MOVE ZERO  TO �������Z�J�E���g
035790             MOVE SPACE TO �I���t���O�Q
035800             PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
035810                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
035820                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
035830                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
035840                           ( �{�L�|�{�p��     NOT = �{�p���v�q      ) 
035850               IF ( �{�L�|�������Z = 1 OR 2 OR 3 ) AND ( �{�L�|�f�Ë敪 = 2 )
035860                  COMPUTE �������Z�J�E���g = �������Z�J�E���g  + 1
035870                  IF ( �������Z�J�E���g <= 3 )
035880                     MOVE �{�L�|�������Z TO �������Z�敪�v�s(�������Z�J�E���g)
035890                     MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
035900                     MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
035910                  END-IF
035920               END-IF
035930               PERFORM �{�p�L�^�e�Ǎ�
035940            END-PERFORM
035950** �������Z�̎�����K�p�ɃZ�b�g
033380            IF ( �������Z���v�s(1) NOT = ZERO ) OR ( �������Z���v�s(1) NOT = ZERO ) 
                     MOVE �������Z���v�s(1) TO �������Z���v
                     MOVE ":"               TO �������Z��؂v
                     MOVE �������Z���v�s(1) TO �������Z���v
                  END-IF
033380            IF ( �������Z���v�s(2) NOT = ZERO ) OR ( �������Z���v�s(2) NOT = ZERO ) 
031910               PERFORM �������Z�K�p�Z�b�g
                  END-IF
035970         END-IF
035980*
035990     END-IF.
036000*
036010*================================================================*
036020 �������Z�K�p�Z�b�g SECTION.
036030*
036040     PERFORM VARYING �ԍ��J�E���^ FROM 1 BY 1
036050              UNTIL  �ԍ��J�E���^ > 3
036060         IF ( �������Z���v�s(�ԍ��J�E���^)  = ZERO )  AND 
036070            ( �������Z���v�s(�ԍ��J�E���^)  = ZERO ) 
036080             CONTINUE
036090         ELSE
036100* �Œ荀��
036110             EVALUATE �������Z�敪�v�s(�ԍ��J�E���^) 
036120             WHEN 1
036130                MOVE NC"���ԊO"   TO ���Z���e�v(�ԍ��J�E���^)
033320             WHEN 2
033330                MOVE NC"�x�@��"   TO ���Z���e�v(�ԍ��J�E���^)
036140             WHEN 3
036150                MOVE NC"�[�@��"   TO ���Z���e�v(�ԍ��J�E���^)
036160             END-EVALUATE
036170*
036180             MOVE NC"�F"          TO ���Z��؂v(�ԍ��J�E���^)
036190             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
036200             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
036210*
036220**** ���������{��ϊ�
036230* ����
036240             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
036250             IF ( �����v >= 10 )
036260                 MOVE �����v�P    TO �����ԍ��v�P
036270                 PERFORM ���{��ϊ�
036280                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
036290                 MOVE �����v�Q    TO �����ԍ��v�P
036300                 PERFORM ���{��ϊ�
036310                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
036320             ELSE
036330                 MOVE �����v�Q    TO �����ԍ��v�P
036340                 PERFORM ���{��ϊ�
036350                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
036360             END-IF
036370* ��
036380             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
036390             MOVE �����v�P    TO �����ԍ��v�P
036400             PERFORM ���{��ϊ�
036410             MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
036420             MOVE �����v�Q    TO �����ԍ��v�P
036430             PERFORM ���{��ϊ�
036440             MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
036450** 
036460        END-IF
036470     END-PERFORM.
036480*
036490     MOVE  �������Z�W�c�m�v(1)   TO �������Z�����P�v. 
036500     MOVE  �������Z�W�c�m�v(2)   TO �������Z�����Q�v. 
036510     MOVE  �������Z�W�c�m�v(3)   TO �������Z�����R�v. 
036520*
036530**** �K�p�P���Q���g�p�i�������R�L�ڂœK�p�P���g���Ă��鎞�́A�K�p�Q�j
036540     IF ( �������Z���v�s(2)  = ZERO ) AND ( �������Z���v�s(2)  = ZERO ) 
036550         CONTINUE
036560     ELSE
036570         IF ( �K�p�P�v  = SPACE )
036580               STRING NC"�������Z"       DELIMITED BY SIZE
036590                      �������Z�����P�v   DELIMITED BY SIZE
036600                      �������Z�����Q�v   DELIMITED BY SIZE
036610                      �������Z�����R�v   DELIMITED BY SIZE
036620                      INTO �K�p�P�v
036630               END-STRING
036640         ELSE
036650               STRING NC"�������Z"       DELIMITED BY SIZE
036660                      �������Z�����P�v   DELIMITED BY SIZE
036670                      �������Z�����Q�v   DELIMITED BY SIZE
036680                      �������Z�����R�v   DELIMITED BY SIZE
036690                      INTO �K�p�Q�v
036700               END-STRING
036710         END-IF
036720     END-IF.
036730*
036740*================================================================*
036750 ���{��ϊ� SECTION.
036760*
036770     MOVE NC"�O"     TO �S�p�����ԍ��v.
036780     CALL "htoz" WITH C LINKAGE
036790                        USING �����ԍ��v�P �S�p�����ԍ��v�P.
036800*
036810*================================================================*
036820*================================================================*
036830 ���������擾 SECTION.
036840*
036850********************************************************************
036860*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
036870*  ��: �@�A �Ƃœ]��.
036880*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
036890*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
036900********************************************************************
036910     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
036920     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
036930             UNTIL ( ���ʂb�m�s > ���ʐ��v )
036940*
036950****        IF ( ���|�������Ҕԍ�(���ʂb�m�s)  NOT = ZERO )  AND
036960        IF ( ���|�����A��(���ʂb�m�s)      NOT = ZERO )
036970*
036980           IF ( �J�E���^ = ZERO )
036990               MOVE 1   TO  �J�E���^ �J�E���^�Q
037000               MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
037010               MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
037020               MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
037030           ELSE
037040              IF ( ���|�������Ҕԍ�(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
037050                 ( ���|�����A��(���ʂb�m�s)      = �����A�Ԃb�v     )
037060                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
037070                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
037080              ELSE
037090                 COMPUTE �J�E���^ = �J�E���^  +  1
037100                 MOVE 1   TO  �J�E���^�Q
037110                 MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
037120                 MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
037130                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
037140              END-IF
037150           END-IF
037160        END-IF
037170     END-PERFORM.
037180**************************************************************************
037190*  ���������}�X�^��蕶�͎擾
037200**************************************************************************
037210     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
037220     PERFORM VARYING �J�E���^ FROM 1 BY 1
037230             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
037240** ���ۂ� �敪 01
037250         MOVE 01                        TO �����|�敪�R�[�h
037260         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
037270         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
037280         READ ���������e
037290         NOT INVALID KEY
037300             INITIALIZE ���������v�s
037310             MOVE �����|���������b�l(1) TO  ���������P�v�s
037320             MOVE �����|���������b�l(2) TO  ���������Q�v�s
037330             MOVE �����|���������b�l(3) TO  ���������R�v�s
037340             MOVE �����|���������b�l(4) TO  ���������S�v�s
037350             MOVE �����|���������b�l(5) TO  ���������T�v�s
037360             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
037370                     UNTIL ( �J�E���^�Q > 9 )  OR 
037380                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
037390                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
037400                WHEN 1
037410                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037420                WHEN 2
037430                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037440                WHEN 3
037450                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037460                WHEN 4
037470                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037480                WHEN 5
037490                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037460                WHEN 6
037470                   MOVE "�E"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037480                WHEN 7
037490                   MOVE "�F"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037500                WHEN OTHER
037510                   CONTINUE
037520                END-EVALUATE
037530             END-PERFORM
037540*
037550             IF �����|�����������͋敪 = 1
037560                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
037570                        ���������P�v�s  DELIMITED BY SIZE
037580                        ���������Q�v�s  DELIMITED BY SIZE
037590                        ���������R�v�s  DELIMITED BY SIZE
037600                        ���������S�v�s  DELIMITED BY SIZE
037610                        ���������T�v�s  DELIMITED BY SIZE
037620                        INTO �����������e�����v(�J�E���^)
037630                 END-STRING
037640             ELSE
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
037730             END-IF
037740*
037750         END-READ
037760     END-PERFORM.
037770*
037780     PERFORM ���������Z�b�g.
037790*
037800*================================================================*
037810 ���������Z�b�g SECTION.
037820*
037830**************************************************************************
037840*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
037850**************************************************************************
037860     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
037870     PERFORM VARYING �J�E���^ FROM 1 BY 1
037880             UNTIL ( �J�E���^ > 9 )  OR ( �����������e�����v(�J�E���^) = SPACE )
037890*
040520        INITIALIZE �����������e�����w�v
040530        MOVE �����������e�����v(�J�E���^)   TO �����������e�����w�v
040540        IF ( �����������e�P�w�v  NOT = SPACE )
040550           COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
040560           MOVE �����������e�P�w�v  TO ���������v(�J�E���^�Q)
040570        END-IF
040580        IF ( �����������e�Q�w�v  NOT = SPACE )
040590           COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
040600           MOVE �����������e�Q�w�v  TO ���������v(�J�E���^�Q)
040610        END-IF
034690        IF  �����������e�R�w�v  NOT = SPACE
034700            COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
034710            MOVE �����������e�R�w�v  TO ���������v(�J�E���^�Q)
034720        END-IF
034690        IF  �����������e�S�w�v  NOT = SPACE
034700            COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
034710            MOVE �����������e�S�w�v  TO ���������v(�J�E���^�Q)
034720        END-IF
038000*
038010     END-PERFORM.
038180*================================================================*
038190 �O�������̂ݔ��� SECTION.
038200*
038210*** �O���̒ʉ@�������������� 
038220     MOVE  SPACE            TO �O���t���O.
038230     MOVE ��|���҃R�[�h    TO �{�L�|���҃R�[�h.
038240     MOVE ��|�{�p�a��      TO �{�L�|�{�p�a��.
038250     MOVE ��|�{�p�N        TO �{�L�|�{�p�N.
038260     MOVE ��|�{�p��        TO �{�L�|�{�p��.
038270     MOVE 1                 TO �{�L�|�{�p��.
038280     START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
038290                                  �{�L�|�{�p�a��N����
038300                                  REVERSED
038310     END-START.
038320     IF ( ��ԃL�[ = "00" )
038330         MOVE SPACE  TO �I���t���O�Q
038340         PERFORM �{�p�L�^�e�Ǎ�
038350         IF ( �I���t���O�Q      = SPACE  ) AND
038360            ( �{�L�|���҃R�[�h  = ��|���҃R�[�h ) AND
038370            ( �{�L�|�f�Ë敪    = 2 ) 
038380*
038390            PERFORM �O������
038400**** �K�p�P���g�p
038410            IF ( �O���t���O = "YES" )
038420               MOVE NC"���O�������̂�"    TO  �K�p�P�v
038430            END-IF
038440**
038450         END-IF
038460     END-IF.
038470*
038480*================================================================*
038490 �O������  SECTION.
038500* 
038510*** �ǂݍ��񂾎{�p�L�^�̔N�����A�O�����ǂ������� (�N���̍��� 1 ��?)
038520      MOVE  SPACE  TO  �O���t���O.
038530      INITIALIZE  �v�Z�N�����v �J�n�N�����Q�v �I���N�����Q�v.
038540**
038550      MOVE ��|�{�p�a��    TO �I���a��Q�v.
038560      MOVE ��|�{�p�N      TO �I���N�Q�v.
038570      MOVE ��|�{�p��      TO �I�����Q�v.
038580      MOVE �{�L�|�{�p�a��  TO �J�n�a��Q�v.
038590      MOVE �{�L�|�{�p�N    TO �J�n�N�Q�v.
038600      MOVE �{�L�|�{�p��    TO �J�n���Q�v.
038610*
038620      EVALUATE TRUE
038630       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v = �I���N�Q�v)
038640            PERFORM  �O����r��
038650       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v NOT = �I���N�Q�v)
038660            PERFORM  �O����r�N
038670       WHEN  �J�n�a��Q�v NOT = �I���a��Q�v 
038680            PERFORM  �O����r����
038690      END-EVALUATE.
038700*
038710      IF ( �v�Z���v = 1 )
038720         MOVE  "YES"  TO  �O���t���O
038730      END-IF.
038740*
038750*================================================================*
038760 �O����r��  SECTION.
038770*
038780     IF ( �I�����Q�v >  �J�n���Q�v )
038790         COMPUTE �v�Z���v = �I�����Q�v - �J�n���Q�v
038800     ELSE
038810        MOVE ZERO TO �v�Z���v
038820     END-IF.
038830*
038840*================================================================*
038850 �O����r�N  SECTION.
038860*
038870     IF ( �I���N�Q�v >  �J�n�N�Q�v )
038880         COMPUTE �v�Z�N�v = �I���N�Q�v - �J�n�N�Q�v
038890         COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
038900     ELSE
038910        MOVE ZERO TO �v�Z���v
038920     END-IF.
038930*
038940*================================================================*
038950 �O����r����  SECTION.
038960*
038970     MOVE �J�n�a��Q�v TO ���|�����敪.
038980     READ �����}�X�^
038990     NOT INVALID KEY
039000         MOVE ���|�J�n����N TO �J�n����N�v
039010     END-READ.
039020     MOVE �I���a��Q�v TO ���|�����敪.
039030     READ �����}�X�^
039040     NOT INVALID KEY
039050         MOVE ���|�J�n����N TO �I������N�v
039060     END-READ.
039070**
039080     IF ( �J�n����N�v NOT = ZERO ) AND ( �I������N�v NOT = ZERO )
039090        COMPUTE �J�n����N�v = �J�n����N�v + �J�n�N�Q�v - 1
039100        COMPUTE �I������N�v = �I������N�v + �I���N�Q�v - 1
039110*
039120        IF ( �I������N�v =  �J�n����N�v )
039130           PERFORM  �O����r��
039140        ELSE
039150           IF ( �I������N�v >  �J�n����N�v )
039160               COMPUTE �v�Z�N�v = �I������N�v - �J�n����N�v
039170               COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
039180           ELSE
039190               MOVE ZERO TO �v�Z���v
039200           END-IF
039210        END-IF
039220     ELSE
039230        MOVE ZERO TO �v�Z���v
039240     END-IF.
039250*================================================================*
039260 �������R���擾 SECTION.
039270*
039280* �������R���擾�� "CHOUBUN" ���Ă�. 
039290     MOVE  SPACE TO  �A�����|�L�[.
039300     INITIALIZE      �A�����|�L�[.
039310     MOVE �{�p�a��v�q  TO  �A�����|�{�p�a��.
039320     MOVE �{�p�N�v�q    TO  �A�����|�{�p�N.
039330     MOVE �{�p���v�q    TO  �A�����|�{�p��.
039340     MOVE ���Ҕԍ��v�q  TO  �A�����|���Ҕԍ�.
039350     MOVE �}�Ԃv�q      TO  �A�����|�}��.
039370     MOVE 61            TO  �A�����|������.
039370     MOVE 56            TO  �A�����|������.
039380*
039390     CALL   "CHOUBUN".
039400     CANCEL "CHOUBUN".
039410*
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
039370     MOVE 56            TO  �A�E���|������.
015000     IF (���Z�������R����敪�v NOT = 1 )
               MOVE �������R����敪�v TO �A�E���|�����敪
           ELSE
               MOVE 1                  TO �A�E���|�����敪
015050     END-IF.
040710*
040720     CALL   "TEKIYBUN".
040730     CANCEL "TEKIYBUN".
040740*
046490*================================================================*
046500 ���Z�E�v�ăZ�b�g SECTION.
046510*================================================================*
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
           MOVE �A�E���|�E�v��(8)    TO �������R���W.
046680*
039420*================================================================*
039430 ��f�҈���敪�X�V SECTION.
039440*
039450** //  ��f�ҏ��e�̈���敪�ɂP���Z�b�g���A�X�V����B//  
039460*
039470     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
039480     MOVE �{�p�N�v�q         TO ��|�{�p�N.
039490     MOVE �{�p���v�q         TO ��|�{�p��.
039500     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
039510     READ ��f�ҏ��e
039520     NOT INVALID KEY
039530         MOVE  1  TO  ��|���Z����敪
039540         REWRITE  ��|���R�[�h
039550         END-REWRITE
039560         IF ( ��ԃL�[ NOT = "00" )
039570            MOVE NC"��f��" TO �t�@�C����
039580            PERFORM �G���[�\��
039590         END-IF
039600     END-READ.
039610*
039620*================================================================*
039630 �������擾 SECTION.
039640*
039650     MOVE �{�p�N�v�q   TO �󗝔N�v.
039660     MOVE �{�p���v�q   TO �󗝌��v.
039670     MOVE �{�p�a��v�q TO ���|�����敪.
039680     READ �����}�X�^
039690     NOT INVALID KEY
039700         MOVE ���|�J�n����N TO �{�p����N�v
039710     END-READ.
039720     IF ( �{�p����N�v NOT = ZERO )
039730        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
039740     END-IF.
039750*
039760     EVALUATE �{�p���v�q
039770     WHEN 4
039780     WHEN 6
039790     WHEN 9
039800     WHEN 11
039810         MOVE 30 TO �󗝓��v
039820     WHEN 2
039830         DIVIDE 4 INTO �{�p����N�v GIVING    ���v
039840                                    REMAINDER �]�v
039850         END-DIVIDE
039860         IF ( �]�v = ZERO )
039870             MOVE 29 TO �󗝓��v
039880         ELSE
039890             MOVE 28 TO �󗝓��v
039900         END-IF
039910     WHEN 1
039920     WHEN 3
039930     WHEN 5
039940     WHEN 7
039950     WHEN 8
039960     WHEN 10
039970     WHEN 12
039980         MOVE 31 TO �󗝓��v
039990     WHEN OTHER
040000          CONTINUE
040010     END-EVALUATE.
040020*
040030*================================================================*
040040 �ϔC�N�����擾 SECTION.
040050*
040060** ---// �����̎󗝔N�ɂ́A�ŏI�ʉ@���������Ă���ׁA�ޔ����� //----
040070     MOVE �󗝔N�v   TO �ŏI�ʉ@�N�v.
040080     MOVE �󗝌��v   TO �ŏI�ʉ@���v.
040090     MOVE �󗝓��v   TO �ŏI�ʉ@���v.
040100***
040110* (�_���t��)
040120     EVALUATE ���Z�v�g���t�敪�v 
040130*    /  �ŏI�ʉ@�� /
040140     WHEN ZERO
040150         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
040160         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
040170         MOVE �ŏI�ʉ@���v TO �_���t���v
040180         MOVE �ŏI�ʉ@���v TO �_���t���v
040190*    /  ������ /
040200     WHEN 1 
040210         PERFORM �������擾
040220         MOVE �󗝔N�v     TO �_���t�N�v
040230         MOVE �󗝌��v     TO �_���t���v
040240         MOVE �󗝓��v     TO �_���t���v
040250*    /  �󎚂Ȃ� /
040260     WHEN 9
040270         MOVE ZERO         TO �_���t�N�v
040280         MOVE ZERO         TO �_���t���v
040290         MOVE ZERO         TO �_���t���v
040300*    /  ���̑��́A�ŏI�ʉ@�� /
040310     WHEN OTHER
040320         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
040330         MOVE �ŏI�ʉ@���v TO �_���t���v
040340         MOVE �ŏI�ʉ@���v TO �_���t���v
040350     END-EVALUATE.
040360**
040370* (���ґ�)
040380     EVALUATE ���Z�v�g���ғ��t�敪�v 
040390*    /  �ŏI�ʉ@�� /
040400     WHEN ZERO
040410         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
040420         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
040430         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
040440*    /  ������ /
040450     WHEN 1 
040460         PERFORM �������擾
040470         MOVE �󗝔N�v     TO ���҈ϔC�N�v
040480         MOVE �󗝌��v     TO ���҈ϔC���v
040490         MOVE �󗝓��v     TO ���҈ϔC���v
040500*    /  �󎚂Ȃ� /
040510     WHEN 9
040520         MOVE ZERO         TO ���҈ϔC�N�v
040530         MOVE ZERO         TO ���҈ϔC���v
040540         MOVE ZERO         TO ���҈ϔC���v
040550*    /  ���̑��́A�ŏI�ʉ@�� /
040560     WHEN OTHER
040570         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
040580         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
040590         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
040600     END-EVALUATE.
040610*
040620*================================================================*
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
040630*================================================================*
040640 �������Z�܂Ƃߔ��� SECTION.
040650*---------------------------------------------------------------------------*
009194* �{�̂܂Ƃߋ敪���P
040670* �̎��́A�t���OYES (���z���������݂ň󎚁j
040680*�i��F���l�s�̏�Q�́A�{�̕ی��i���یn�j�̃��Z�v�g�P���Ő����A�������Z�͂Ȃ��j
040690*---------------------------------------------------------------------------*
040700*
040710     MOVE SPACE TO �������Z�܂Ƃ߃t���O.
007750*
009201     IF ���Z�|�{�̂܂Ƃߋ敪 = 1 
009202        MOVE "YES" TO �������Z�܂Ƃ߃t���O
009203     END-IF.
041100*
041110*----------------------------------------------------------------------*
041120** / �_�ސ쌧�ŗL�F�E�v�ɕ��S�Ҕԍ��Ǝ󋋎Ҕԍ� /
041130     IF ( �������Z�܂Ƃ߃t���O = "YES" ) AND
041140        ( ��|��p���S�Ҕԍ�����(3:2) = "14" )
041150        IF ( ��|��p���S�Ҕԍ�����(1:2) NOT = "99" )
041160*            MOVE ALL NC"�P" TO �����P �����Q �����R
041170*            MOVE ALL NC"�b" TO �c���P �c���Q
041180*            MOVE NC"�b"     TO �c���R �c���S
041190*            MOVE NC"����S�Ҕԍ�"     TO �_�ސ�Œ�P
041200*            MOVE NC"�󋋎Ҕԍ�"         TO �_�ސ�Œ�Q
041210*            MOVE NC"�^"                 TO �_�ސ�Œ�R
041220            MOVE ��|��p���S�Ҕԍ����� TO ����S�Ҕԍ�
041230*            MOVE ��|��v�Ҕԍ�����     TO �󋋎Ҕԍ�
      */�󋋎Ҕԍ����W�����ȏ�̏ꍇ�g�𖳎����Ĉ������/110426
                  MOVE ��|��v�Ҕԍ�����   TO �󋋎Ҕԍ��v
                  IF ����󋋎Ҕԍ��Q�v = SPACE
016830                MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
                  ELSE
                      MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
                  END-IF
041240        END-IF
041250     END-IF.
041260*
041270*================================================================*
042020*================================================================*
042030 �G���[�\�� SECTION.
042040*
042050     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
042060     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
042070     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
042080     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
042090                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
042100     ACCEPT  �L�[���� FROM CONS
042110     PERFORM �t�@�C����.
042120     EXIT PROGRAM.
042130*================================================================*
042140*================================================================*
042150 �t�@�C���� SECTION.
042160*
042170     CLOSE ����t�@�C��.
042180     CLOSE �ی��҃}�X�^     �����}�X�^          ���̃}�X�^
042190           ������}�X�^   �{�p�����}�X�^    �{�p�L�^�e
042200           �o�߃}�X�^       ��f�ҏ��e        �����f�[�^�e
042220           �h�c�Ǘ��}�X�^   �s�����}�X�^        ���Z�v�g�e
042230            ���������e      ��ƃt�@�C���S.
042240*================================================================*
042250 �I������ SECTION.
042260*
042270     PERFORM �t�@�C����.
042280*================================================================*
042290*================================================================*
042300 �e�X�g�󎚏��� SECTION.
042310*
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
           �󗝔N �󗝌� �󗝓� �ϔC�N �ϔC�� �ϔC��
           .
           MOVE ALL "X" TO
           ���ϔԍ� ���{�p�h�c �ی��Ҕԍ� �L���ԍ� ����S�Ҕԍ� �󋋎Ҕԍ� �Z���P �Z���Q 
           �������`�l �_���t�ԍ� �����ԍ� ���Z�@�֖��P ���Z�@�֖��Q ���Z�@�֖��R 
           ���Z�@�֖��S �x�X���P �x�X���Q �x�X���R �x�X���S �{�p���X�֔ԍ��P �{�p���X�֔ԍ��Q 
           �{�p���Z���P �{�p���Z���Q �{�p���d�b�ԍ� ��\�҃J�i �ی��Җ��̂P �ی��Җ��̂Q
           .
           MOVE ALL "�m" TO
           ��ی��Ҏ��� ���Ҏ��� �ڍ��@�� ��\�Җ�
           ���������P ���������Q ���������R ���������S ���������T ���������U 
           �������R���P  �������R���Q �������R���R �������R���S �������R���T
           �������R���U �������R���V
           .
           MOVE ALL NC"�m" TO
           �������P �������Q �������R �������S �������T �o�ߗ���(1) �o�ߗ���(2) �o�ߗ���(3) 
           �o�ߗ���(4) �o�ߗ���(5) �K�p�P �K�p�Q
           .
           MOVE NC"��" TO
           �P�ƃ`�F�b�N �{�l�`�F�b�N ����`�F�b�N ���σ`�F�b�N ���`�F�b�N �Еۃ`�F�b�N 
           �g���`�F�b�N �P�O���`�F�b�N �X���`�F�b�N �Q���`�F�b�N �U�΃`�F�b�N �W���`�F�b�N 
           �V���`�F�b�N ����`�F�b�N �ސE�`�F�b�N ���ۃ`�F�b�N �Ƒ��`�F�b�N ���V�`�F�b�N 
           �j�`�F�b�N �����`�F�b�N �吳�`�F�b�N ���`�F�b�N ���a�`�F�b�N �����`�F�b�N 
           �����`�F�b�N�P ���~�`�F�b�N�P �]��`�F�b�N�P �����`�F�b�N�Q ���~�`�F�b�N�Q 
           �]��`�F�b�N�Q �����`�F�b�N�R ���~�`�F�b�N�R �]��`�F�b�N�R �����`�F�b�N�S 
           ���~�`�F�b�N�S �]��`�F�b�N�S �����`�F�b�N�T ���~�`�F�b�N�T �]��`�F�b�N�T �V�K�`�F�b�N 
           �p���`�F�b�N �[��`�F�b�N ���ԊO�`�F�b�N �x���`�F�b�N �Œ藿�`�F�b�N �������`�F�b�N 
           �{�×��`�F�b�N ��ԃ`�F�b�N �\���J��`�F�b�N ��H�`�F�b�N ��`�F�b�N ���`�F�b�N 
           ���`�F�b�N ���ʃ`�F�b�N �U���`�F�b�N �����`�F�b�N ��s�`�F�b�N ���Ƀ`�F�b�N �_���`�F�b�N 
           �{�X�`�F�b�N �x�X�`�F�b�N �{�x���`�F�b�N
           .
043590*
043600*================================================================*
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
043610******************************************************************
043620 END PROGRAM YJK6125.
043630******************************************************************
