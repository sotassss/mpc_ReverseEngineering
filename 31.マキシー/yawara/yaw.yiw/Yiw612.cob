000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YIW612.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090* �A�C��   ���Z�v�g����i�_�{����޳�ޔŁj*
000100*         MED = YAW610 YIW612P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2015-09-10
000130 DATE-COMPILED.          2015-09-10
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
           SELECT  �����}�X�^      ASSIGN      TO        RYOUKINL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  ���|�敪�R�[�h
                                                                ���|���ʃR�[�h
                                                                ���|�J�n�a��N��
                                   FILE STATUS              IS  ��ԃL�[
                                   LOCK        MODE         IS  AUTOMATIC.
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
001410     SELECT  ����}�X�^    ASSIGN      TO        KAIJOHOL
001420                             ORGANIZATION             IS  INDEXED
001430                             ACCESS MODE              IS  DYNAMIC
000130                             RECORD KEY               IS  ���|�_���I���敪
000131                                                          ���|����R�[�h
000132                                                          ���|�ی����
000133                                                          ���|�ύX�a��N��
000134                             ALTERNATE RECORD KEY     IS  ���|�_���I���敪
000135                                                          ���|�ڍ��t��J�i
000136                                                          ���|����R�[�h
000137                                                          ���|�ی����
000138                                                          ���|�ύX�a��N��
000151                             FILE STATUS              IS  ��ԃL�[
001520                             LOCK        MODE         IS  AUTOMATIC.
001739*  �U������
001740     SELECT �U�������e       ASSIGN      TO     "C:\MAKISHISYS\YAWOBJ\IWKOUZA.DAT"
001741                             ORGANIZATION             IS  LINE SEQUENTIAL
001742                             ACCESS MODE              IS  SEQUENTIAL
001743                             FILE STATUS              IS  ��ԃL�[
001744                             LOCK        MODE         IS  AUTOMATIC.
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
002210*                           �m�q�k��  �Q�T�U�n
002220 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
002230     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
002240*                           �m�q�k��  �P�Q�W�n
002250 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
002260     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *                           �m�q�k��  �R�Q�O�n
       FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
           COPY RYOUKIN         OF  XFDLIB  JOINING   ��   AS  PREFIX.
           COPY RYOUKNA         OF  XFDLIB  JOINING   ���` AS  PREFIX.
           COPY RYOUKNB         OF  XFDLIB  JOINING   ���a AS  PREFIX.
           COPY RYOUKNC         OF  XFDLIB  JOINING   ���b AS  PREFIX.
           COPY RYOUKND         OF  XFDLIB  JOINING   ���c AS  PREFIX.
           COPY RYOUKNE         OF  XFDLIB  JOINING   ���d AS  PREFIX.
           COPY RYOUKNF         OF  XFDLIB  JOINING   ���e AS  PREFIX.
002270*                           �m�q�k��  �P�Q�W�n
002280 FD  ���������e          BLOCK   CONTAINS   1   RECORDS.
002290     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
002300*                           �m�q�k��  �P�Q�W�n
002310 FD  �h�c�Ǘ��}�X�^          BLOCK   CONTAINS   1   RECORDS.
002320     COPY IDKANR    OF  XFDLIB  JOINING   �h�c��   AS  PREFIX.
002330*                           �m�q�k��  �Q�T�U�n
002340 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002350     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
002470*                           �m�q�k��  �U�S�O�n
002480 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
002490     COPY KAIJOHO         OF  XFDLIB  JOINING   ���   AS  PREFIX.
002390**
002294 FD  �U�������e      BLOCK   CONTAINS   1   RECORDS.
002295 01  �����|���R�[�h.
002296     03  �����|���R�[�h�f�[�^               PIC X(128).
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
002570 FD  ����t�@�C��.
002580     COPY YIW612P        OF  XMDLIB.
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
       01 �������q�b�l                       PIC X(140) VALUE SPACE.
       01 �^����Âb�l                       PIC X(68)  VALUE SPACE.
004355*
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
      */�≷㪖@�d�×��P����/110824*
          03 ��㪖@���P���v                  PIC 9(4)  VALUE ZERO.
          03 ��㪖@���P���v                  PIC 9(4)  VALUE ZERO.
          03 �d�×��P���v                    PIC 9(4)  VALUE ZERO.
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
          03 �x���@��.
             05 ���Z�@�֖��v.
                07 ���Z�@�֖��P�v            PIC X(8)  VALUE SPACE.
                07 ���Z�@�֖��Q�v            PIC X(8)  VALUE SPACE.
                07 ���Z�@�֖��R�v            PIC X(8)  VALUE SPACE.
                07 ���Z�@�֖��S�v            PIC X(8)  VALUE SPACE.
                07 ���Z�@�֖��T�v            PIC X(8)  VALUE SPACE.
             05 �x�X���v.
                07 �x�X���P�v                PIC X(8) VALUE SPACE.
                07 �x�X���Q�v                PIC X(8) VALUE SPACE.
                07 �x�X���R�v                PIC X(8) VALUE SPACE.
                07 �x�X���S�v                PIC X(8) VALUE SPACE.
             05 �U���`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ���ʃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �����`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ��s�`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ���Ƀ`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �_���`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �{�X�`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �x�X�`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �{�x���`�F�b�N�v             PIC N(1)  VALUE SPACE.
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
          03 �󋋎Ҕԍ��v                    PIC X(15) VALUE SPACE.
008040    03 �����於�̂v.
008050       05 �����於�̂P�v               PIC X(40)  VALUE SPACE.
008060       05 �����於�̂Q�v               PIC X(40)  VALUE SPACE.
008070    03 �ی���ʂv                      PIC 9(2)   VALUE ZERO.
008070    03 �����ʂv                      PIC 9(2)   VALUE ZERO.
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
008140       05 ��ی��ҏZ���v.
008150          07 ��ی��ҏZ���P�v          PIC X(50)  VALUE SPACE.
008160          07 ��ی��ҏZ���Q�v          PIC X(50)  VALUE SPACE.
008990       05 �d�b�ԍ��v                   PIC X(35)  VALUE SPACE.
008170    03 ���ҏ��v.
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
008290          07 �ߘa�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008300          07 �����v                    PIC N(2)  VALUE SPACE.
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
                07 ���������w�v              PIC X(80)  VALUE SPACE.
008390*
008400    03 �ی���ʖ��̂v                  PIC N(1)  VALUE SPACE.
008410    03 ������v                        PIC N(1)  VALUE SPACE.
008420    03 ���ʃR�����g�v                  PIC X(16) VALUE SPACE.
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
          03 �^�����v                           PIC 9(4)  VALUE ZERO.
009100************
009110* ���l��� *
009120************
010000 01 ���l���v.
010010    03 �K�p�P�v                        PIC N(38) VALUE SPACE.
010020    03 �K�p�Q�v                        PIC N(38) VALUE SPACE.
010020    03 �K�p�R�v                        PIC X(40) VALUE SPACE.
009250*
009260    03 �o�߃R�����g�v                  PIC N(60) VALUE SPACE.
      *
       01 �{�p�a��N�����b�v.
         03 �{�p�a��N���b�v.
           05 �{�p�a��b�v                   PIC 9.
           05 �{�p�N���b�v.
              07 �{�p�N�b�v                  PIC 9(2).
              07 �{�p���b�v                  PIC 9(2).
         03 �{�p���b�v                       PIC 9(2).
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
005260 01 �U�������v.
005261    03 �����ی��Ҕԍ��v                PIC X(10)  VALUE SPACE.
005262    03 �����ی��Җ��v                  PIC X(100) VALUE SPACE.
005263    03 ���������ԍ��v                  PIC X(10)  VALUE SPACE.
005263    03 ���Z�@�փR�[�h�v                PIC X(8)   VALUE SPACE.
005261 01 ��r�ی��Ҕԍ��v                   PIC X(10)  VALUE SPACE.
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
002974     MOVE "YIW612"             TO �g�A�o�q�s�e�|���[�v���O������.
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
           OPEN INPUT �����}�X�^.
               MOVE NC"����" TO �t�@�C����.
               PERFORM �I�[�v���`�F�b�N.
015050     OPEN INPUT   ���������e.
015060         MOVE NC"��������" TO �t�@�C����.
015070         PERFORM �I�[�v���`�F�b�N.
015080     OPEN INPUT   �h�c�Ǘ��}�X�^
015090         MOVE NC"�h�c" TO �t�@�C����.
015100         PERFORM �I�[�v���`�F�b�N.
015110     OPEN INPUT �s�����}�X�^.
015120         MOVE NC"�s����" TO �t�@�C����.
015130         PERFORM �I�[�v���`�F�b�N.
015160     OPEN INPUT   ����}�X�^.
015170         MOVE NC"���" TO �t�@�C����.
015180         PERFORM �I�[�v���`�F�b�N.
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
007556     PERFORM �U�������Z�b�g.
026961**
026962     IF ��|������� NOT = ZERO
026963        PERFORM �������Z�܂Ƃߔ���
026964     ELSE
026965        MOVE SPACE TO �������Z�܂Ƃ߃t���O
026966     END-IF.
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
           IF ( ������ʂv�q NOT = ZERO )
              MOVE NC"��"              TO ������
016450        MOVE ������v            TO ������
           END-IF.
016000********************
016010* ��f�ҏ��Z�b�g *
016020********************
      */��t���q�ǂ���Ô������
           IF (�A���|�ی���� >=  50  ) AND
              (��|�������    = "60" ) AND
              (��|��p���S�Ҕԍ�����(1:4) = "8312" )
               MOVE "��t���q�ǂ���Ô������" TO �^�C�g���Q
           END-IF
      */��t���d�x�S�g��Q��Ô������ �{�̂��d�S������150914
      *     IF (�A���|�ی���� >=  50  ) AND
           IF (��|�������    = "53" ) AND
              (��|��p���S�Ҕԍ�����(1:4) = "8112" )
               IF (�A���|�ی���� >=  50  )
                   MOVE "��t���d�x�S�g��Q�ҁi���j��Ô��" TO �^�C�g���Q
               END-IF
               MOVE NC"�d�S"         TO �d�S
               MOVE NC"��"           TO �d�S��
           END-IF
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
016030     MOVE �{�p�N�v           TO �{�p�N.
016040     MOVE �{�p���v           TO �{�p��.
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
016200     MOVE �����於�̂v        TO �ی��Җ���.
016190*     IF ( �����於�̂Q�v = SPACE )
016200*        MOVE �����於�̂v     TO �ی��Җ��� �ی��Җ��̂Q.
016210*     ELSE
016220        MOVE �����於�̂P�v   TO �ی��Җ��̂P
016230        MOVE �����於�̂Q�v   TO �ی��Җ��̂Q
016240*     END-IF.
           IF �A���|�ی���� > 50
               IF �s�����ԍ��v(1:2) = "99"
                   MOVE SPACE            TO ����S�Ҕԍ�
               ELSE
                   MOVE �s�����ԍ��v     TO ����S�Ҕԍ�
               END-IF
               MOVE �󋋎Ҕԍ��v         TO �󋋎Ҕԍ�
           END-IF.
016250***     MOVE ��ی��҃J�i�v      TO ��ی��҃J�i.
016260     MOVE ��ی��Ҏ����v      TO ��ی��Ҏ���.
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
016300     MOVE ��ی��ҏZ���P�v    TO �Z���P.
016310     MOVE ��ی��ҏZ���Q�v    TO �Z���Q.
016320***     MOVE ���҃J�i�v          TO ���҃J�i.
016330     MOVE ���Ҏ����v          TO ���Ҏ���.
016340     MOVE �j�`�F�b�N�v        TO �j�`�F�b�N.
016350     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
016360*     MOVE ���ʂv               TO ����.
016370     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
016380     MOVE �吳�`�F�b�N�v      TO �吳�`�F�b�N.
016390     MOVE ���a�`�F�b�N�v      TO ���a�`�F�b�N.
016400     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
016400     MOVE �ߘa�`�F�b�N�v      TO �ߘa�`�F�b�N.
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
016500     MOVE ���������v(8)       TO ���������W.
016510*
016520***     MOVE ������v            TO ������.
016530***     MOVE �ی���ʖ��̂v      TO �ی����.
016540*
      */�����̕��S�Ҕԍ��A�󋋎Ҕԍ����������/131004
           IF ( �s�����ԍ��v(1:2) NOT = "99" )
               MOVE �s�����ԍ��v TO ����S�Ҕԍ�
           END-IF.
           IF ( �󋋎Ҕԍ��v(1:1) = "*"  ) OR
              ( �󋋎Ҕԍ��v(1:2) = "��" )
               MOVE SPACE        TO �󋋎Ҕԍ�
           ELSE
               MOVE �󋋎Ҕԍ��v TO �󋋎Ҕԍ�
           END-IF.
      **/���{���̍��ۑސE�������{�����̏ꍇ�͕��S�Ҏ󋋎Ҕԍ����L�ڂ���
      *     IF (��|�ی���� = 01      ) AND (��|�ی��Ҕԍ�(1:2) = "27") OR
      *        (��|�ی���� = 05 OR 08) AND (��|�ی��Ҕԍ�(3:2) = "27")
      *         IF ( �s�����ԍ��v(1:2) NOT = "99" )
      *             MOVE �s�����ԍ��v TO ����S�Ҕԍ�
      *         END-IF
      *         IF ( �󋋎Ҕԍ��v(1:1) = "*"  ) OR
      *            ( �󋋎Ҕԍ��v(1:2) = "��" )
      *             MOVE SPACE        TO �󋋎Ҕԍ�
      *         ELSE
      *             MOVE �󋋎Ҕԍ��v TO �󋋎Ҕԍ�
      *         END-IF
      *     END-IF.
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
019380     MOVE �������q���Z���v�q           TO  �������q���Z��.
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
      ********************************
      */�≷㪖@�d�×��P����      /*
      ********************************
           MOVE ��㪖@���P���v            TO ��㪖@�P��.
           MOVE ��㪖@���P���v            TO ��㪖@�P��.
           MOVE �d�×��P���v              TO �d�ÒP��.
      *
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
021720        MOVE ���ʂT�v                     TO ���ʂT�W
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
021720        MOVE ���ʂT�v                     TO ���ʂT�O
021730     END-IF.
020690*
021750     MOVE �K�p�P�v                       TO �K�p�P.
021760     MOVE �K�p�Q�v                       TO �K�p�Q.
021760     MOVE �K�p�R�v                       TO �K�p�R.
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
              MOVE 48           TO �A���^�|��R�[�h
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
020740     MOVE ���Z�|���v                     TO ���v.
           MOVE ���Z�|�ꕔ���S��               TO �ꕔ���S��.
           MOVE ���Z�|�������z                 TO �������z.
020770*
      */�������Z���̋��z��
           IF �A���|�ی���� > 50
               EVALUATE TRUE
      */��t���̎q�ǂ���Ô��/120314
      */��t���̏d�x�S�g��Q��Ô��/150703
               WHEN ((������ʂv�q = 60) AND (��p���S�Ҕԍ������v�q(1:4) =  "8312")) OR
                    ((������ʂv�q = 53) AND (��p���S�Ҕԍ������v�q(1:4) =  "8112"))
                   MOVE "X" TO EDIT-MODE OF           �ꕔ���S��
                   MOVE ���Z�|�ꕔ���S��   TO �ꕔ���S���Q
                   MOVE ���Z�|�󋋎ҕ��S�z TO �󋋎ҕ��S�z�Q
                   MOVE ���Z�|�����������z TO �������z
      *             MOVE "�\�\�\�\�\�\�\�\" TO ����
               WHEN OTHER
      */�ʏ���z��
                   MOVE "�ꕔ���S�������z�i��Ï�����j"  TO �󋋎ҕ��S�z�b�l
                   MOVE "�������z�i��Ï�����j"          TO ���������z�b�l
                   MOVE "�~�A"               TO �~�P
                   MOVE "�~"                 TO �~�Q
                   MOVE ���Z�|�󋋎ҕ��S�z   TO �󋋎ҕ��S�z
                   MOVE ���Z�|�����������z   TO ���������z
               END-EVALUATE
      *     ELSE
      */��t���̏d�x�S�g��Q��Ô���̖{�̂ɏ����̋��z/150914
      *         IF ((������ʂv�q = 53) AND (��p���S�Ҕԍ������v�q(1:4) =  "8112"))
      *             MOVE "���ȕ��S���z�F"     TO ���ȕ��S���b�l
      *             MOVE "�~"                 TO �~�b�l�P
      *             MOVE ���Z�|�󋋎ҕ��S�z   TO ���ȕ��S��
      *             MOVE "����S���z�F"     TO �ꕔ���S���b�l
      *             MOVE "�~"                 TO �~�b�l
      *             MOVE ���Z�|�����������z   TO �󋋎ҕ��S�z
      *         END-IF
           END-IF.
020780**------------------------------------------------------------------------------------*
020790** ���ʁi�������Z�Ȃ��ŁA�{�̃��Z�ɂ܂Ƃ߂鎞�A���z�͏������݁E�K�p�Q�ɏ�����ʈ󎚁j
020800*     IF ( �������Z�܂Ƃ߃t���O = "YES" )
020810*         PERFORM ���������v�Z
020820*         MOVE �A�v�|��p�z             TO ���v
020830*         MOVE �A�v�|���S�z����         TO �ꕔ���S��
020840*     / �����Z����/
020850*         COMPUTE �������z = �A�v�|��p�z - �A�v�|���S�z����
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
021060**------------------------------------------------------------------------------------*
021087*
021088**********************
021090* �{�p���f�[�^�Z�b�g *
021100**********************
           MOVE �s���{���i�h�r�v       TO �s���{���ԍ�.
021110     MOVE �_���t�ԍ��v           TO �_���t�ԍ�.
021120     STRING "���-"                    DELIMITED BY SIZE
                  �ڍ��t�����ԍ��v(1:4)   DELIMITED BY SIZE
             INTO ����ԍ�
           END-STRING.
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
           MOVE ���Z�@�֖��P�v           TO ��s���P.
           MOVE ���Z�@�֖��Q�v           TO ��s���Q.
           MOVE �x�X���P�v               TO �x�X���P.
           MOVE �x�X���Q�v               TO �x�X���Q.
           MOVE �U���`�F�b�N�v           TO �U���`�F�b�N.
           MOVE ���ʃ`�F�b�N�v           TO ���ʃ`�F�b�N.
           MOVE �����`�F�b�N�v           TO �����`�F�b�N.
           MOVE ��s�`�F�b�N�v           TO ��s�`�F�b�N.
           MOVE ���Ƀ`�F�b�N�v           TO ���Ƀ`�F�b�N.
           MOVE �_���`�F�b�N�v           TO �_���`�F�b�N.
           MOVE �{�X�`�F�b�N�v           TO �{�X�`�F�b�N.
           MOVE �x�X�`�F�b�N�v           TO �x�X�`�F�b�N.
           MOVE �{�x���`�F�b�N�v         TO �{�x���`�F�b�N.
021660     MOVE �������`�l�J�i�v         TO �������`�l�J�i�P.
021670     MOVE �������`�l�v             TO �������`�l.
021390***     MOVE �a����ʃR�����g�v     TO �a�����.
021400     MOVE �����ԍ��v             TO �����ԍ�.
021430*
021440* / �_���t�E���҈ϔC�� /
021450     MOVE �_���t�N�v             TO �󗝔N.
021460     MOVE �_���t���v             TO �󗝌�.
021470     MOVE �_���t���v             TO �󗝓�.
021480* ( �ϔC�N���� ������邩 )
021490     IF ( �A���|�ϔC���  = ZERO )
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
021660     MOVE ���Ҕԍ��v�q        TO ���Ҕԍ�.
021670     MOVE �}�Ԃv�q            TO �}��.
021660*     MOVE ���Ҕԍ��v�q        TO ���Ҕԍ��Q.
021670*     MOVE �}�Ԃv�q            TO �}�ԂQ.
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
021780*******     PERFORM �e�X�g�󎚏���.
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
021910     INITIALIZE YIW612P.
021900     MOVE SPACE TO YIW612P.
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
22400*
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
      ********************************
      */�≷㪖@�d�×��P����      /*
      ********************************
           MOVE 01             TO ���|�敪�R�[�h.
           MOVE ZEROS          TO ���|�������.
           MOVE ZEROS          TO ���|����.
           MOVE ZEROS          TO ���|���E�敪.
           MOVE ZEROS          TO ���|�����ʒu�ԍ�.
           MOVE �{�p�a��v�q   TO ���|�J�n�a�� �{�p�a��b�v.
           MOVE �{�p�N�v�q     TO ���|�J�n�N   �{�p�N�b�v.
           MOVE �{�p���v�q     TO ���|�J�n��   �{�p���b�v.
      *
           START �����}�X�^ KEY IS <= ���|�敪�R�[�h 
                                      ���|���ʃR�[�h
                                      ���|�J�n�a��N��
                                      REVERSED
           END-START.
      *
           IF ��ԃL�[ = "00"
               READ �����}�X�^ NEXT
               AT END
      */�G���[�\���̏C��
                   DISPLAY "�{�p�N���ɑΉ������������݂���܂���"
                           " ��f�҇�=" ���Z�|���҃R�[�h
                           " �{�p�N��=" ���Z�|�{�p�N ���Z�|�{�p��   UPON CONS
                   PERFORM �I������
                   MOVE ZERO TO PROGRAM-STATUS
                   EXIT PROGRAM
               NOT AT END
      *
                   IF ( �{�p�a��N���b�v >= ���`�|�J�n�a��N�� ) AND
                      ( �{�p�a��N���b�v <= ���`�|�I���a��N�� )
                       MOVE ���`�|��㪖@��        TO ��㪖@���P���v
                       MOVE ���`�|��㪖@��        TO ��㪖@���P���v
                       MOVE ���`�|�d�×�          TO �d�×��P���v
                   ELSE
                       DISPLAY "�{�p�N���ɑΉ������������݂���܂���"
                               " ��f�҇�=" ���Z�|���҃R�[�h
                               " �{�p�N��=" ���Z�|�{�p�N ���Z�|�{�p��   UPON CONS
                       PERFORM �I������
                       MOVE ZERO TO PROGRAM-STATUS
                       EXIT PROGRAM
                   END-IF
               END-READ
           END-IF.
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
022870     MOVE ���Z�|�����������P           TO �����������P�v�q.
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
023020     MOVE ���Z�|�����������Q           TO �����������Q�v�q.
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
023180     MOVE ���Z�|�����������R�W           TO �����������R�W�v�q.
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
023350     MOVE ���Z�|�����������R�O           TO �����������R�O�v�q.
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
023690     MOVE ���Z�|�����������S�W           TO �����������S�W�v�q.
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
023860     MOVE ���Z�|�����������S�O           TO �����������S�O�v�q.
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
024380     MOVE ���Z�|�����������T�W           TO �����������T�W�v�q.
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
024550     MOVE ���Z�|�����������T�O           TO �����������T�O�v�q.
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
                 INTO �K�p�R�v
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
024820*
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
025350*------------------------------------------------------------------------*
025360         EVALUATE �ی���ʂv�q
025370         WHEN 01
025380             MOVE �ی��Ҕԍ��v�q       TO �ی��Ҕԍ���r�v
025390             PERFORM ���{�p�h�c�Z�b�g
025400         WHEN 08
               WHEN 05
025410             MOVE �ی��Ҕԍ��v�q(3:6)  TO �ی��Ҕԍ���r�v
025420             PERFORM ���{�p�h�c�Z�b�g
025430         WHEN 04
025440             PERFORM ���ϔԍ��Z�b�g
025450         WHEN 09
025460             PERFORM ���q���ԍ��Z�b�g
025470         END-EVALUATE
025480*
025490     END-READ.
023500** �U������  / ����}�X�^���U��������擾 /
023520     MOVE ZERO  TO  ���|�_���I���敪
023510     MOVE 48    TO  ���|����R�[�h
023520     MOVE ZERO  TO  ���|�ی����
023530     MOVE ZERO  TO  ���|�ύX�a��N��
023540     READ ����}�X�^
023550     NOT INVALID KEY
023560*         MOVE ���|������s��      TO ������s���v
023570*         MOVE ���|������s�x�X��  TO ������s�x�X���v
023580*         MOVE ���|�a�����          TO �a����ʂv
023590*         MOVE ���|�����ԍ�          TO �����ԍ��v
023600         MOVE ���|�������`�l        TO �������`�l�v
023610         MOVE ���|�������`�l�J�i    TO �������`�l�J�i�v
023780     END-READ.
023030*--------------------------------------------------------------------------*      */����͐U���̂ݑΉ�
           MOVE NC"��" TO �U���`�F�b�N�v.
      *
           EVALUATE �a����ʂv
           WHEN 1
               MOVE NC"��" TO ���ʃ`�F�b�N�v
           WHEN 2
               MOVE NC"��" TO �����`�F�b�N�v
           END-EVALUATE.
      *
009745     IF ������s���v NOT = SPACE
009746        PERFORM VARYING �J�E���^ FROM 40 BY -1
009747                  UNTIL (������s���v(�J�E���^:1) NOT = SPACE) OR
009748                        (�J�E���^ <= ZERO)
009749            CONTINUE
009750        END-PERFORM
009751        IF �J�E���^ > 4
009752           IF ������s���v(�J�E���^ - 3 : 4)  = "��s"
009753              MOVE  ������s���v(1:�J�E���^ - 4)   TO ���Z�@�֖��v
009754              MOVE NC"��" TO ��s�`�F�b�N�v
009755           ELSE
009756              IF ������s���v(�J�E���^ - 3 : 4)  = "����"
009757                 MOVE  ������s���v(1:�J�E���^ - 4)   TO ���Z�@�֖��v
009758                 MOVE NC"��" TO ���Ƀ`�F�b�N�v
009759              ELSE
009760                 IF ������s���v(�J�E���^ - 3 : 4)  = "�_��"
009761                    MOVE  ������s���v(1:�J�E���^ - 4)   TO ���Z�@�֖��v
009762                    MOVE NC"��" TO �_���`�F�b�N�v
009763                 ELSE
009764                    MOVE  ������s���v  TO ���Z�@�֖��v
      */�ȗ����͋�s�Ƃ���
                          MOVE NC"��" TO ��s�`�F�b�N�v
009765                 END-IF
009766              END-IF
009767           END-IF
009768        ELSE
009769           MOVE  ������s���v  TO ���Z�@�֖��v
      */�ȗ����͋�s�Ƃ���
                 MOVE NC"��" TO ��s�`�F�b�N�v
009770        END-IF
009771     END-IF.
009779*
009780     IF ������s�x�X���v NOT = SPACE
009781        PERFORM VARYING �J�E���^ FROM 40 BY -1
009782                  UNTIL (������s�x�X���v(�J�E���^:1) NOT = SPACE) OR
009783                        (�J�E���^ <= ZERO)
009784            CONTINUE
009785        END-PERFORM
009786        IF �J�E���^ >= 4
009787           IF ������s�x�X���v(�J�E���^ - 3 : 4)  = "�{�X"
009788              MOVE  ������s�x�X���v(1:�J�E���^ - 4)   TO �x�X���v
009789              MOVE NC"��" TO �{�X�`�F�b�N�v
009790           ELSE
009791              IF ������s�x�X���v(�J�E���^ - 3 : 4)  = "�x�X"
009792                 MOVE  ������s�x�X���v(1:�J�E���^ - 4)   TO �x�X���v
009793                 MOVE NC"��" TO �x�X�`�F�b�N�v
009794              ELSE
009791                 IF ������s�x�X���v(�J�E���^ - 3 : 4)  = "�x��"
009792                    MOVE  ������s�x�X���v(1:�J�E���^ - 4)   TO �x�X���v
009793                    MOVE NC"��" TO �{�x���`�F�b�N�v
009794                 ELSE
009791                     IF ������s�x�X���v(�J�E���^ - 3 : 4)  = "�{��"
009792                        MOVE  ������s�x�X���v(1:�J�E���^ - 4)   TO �x�X���v
009793                        MOVE NC"��" TO �{�x���`�F�b�N�v
009794                     ELSE
009800                         MOVE  ������s�x�X���v  TO �x�X���v
      */�ȗ����͎x�X�Ƃ���
                               MOVE NC"��" TO �x�X�`�F�b�N�v
009801                     END-IF
009804                 END-IF
009805              END-IF
009806           END-IF
009807        ELSE
009808           MOVE  ������s�x�X���v  TO �x�X���v
      */�ȗ����͎x�X�Ƃ���
                 MOVE NC"��" TO �x�X�`�F�b�N�v
009809        END-IF
009810     END-IF.
025500*
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
025810        IF ( �ی��Ҕԍ��v�q(1:2) = "31" )  OR
025820           ( �ی��Ҕԍ��v�q = "34130021" )
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
026180        END-IF
026190     END-IF.
026200*
026210** 2. �n���ϋ��c��
026220     MOVE SPACE  TO  �E�o�t���O.
026230     IF ( �{��|�n���ϘA�ԍ� NOT = ZERO )
026240** ����(�ی��Ҕԍ�)
026250        IF ( �ی��Ҕԍ��v�q(1:2) = "32" OR "33" OR "34" )  AND
026260           ( �ی��Ҕԍ��v�q NOT = "34130021" )
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
026620        END-IF
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
      */�O������P���͂W�����t�Ɂ�/110721
                   IF (��|�ی���� NOT = 05 ) AND (��|���ʋ敪 = 1)
                       MOVE SPACE  TO �X���`�F�b�N�v
                       MOVE NC"��" TO �W���`�F�b�N�v
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
027880         MOVE ��|������     TO �����ʂv
027890** �S���y�؂̎}�ԍ폜
027900         IF ( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(1:6) = "133033" )
027910            MOVE ��|�ی��Ҕԍ�(1:6)  TO �ی��Ҕԍ��v
027920         END-IF
027930**
027940         MOVE ��|��ی��҃J�i TO ��ی��҃J�i�v
027950         MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ����v
027960         MOVE ��|�X�֔ԍ��P   TO �X�֔ԍ��P�v
027970         MOVE ��|�X�֔ԍ��Q   TO �X�֔ԍ��Q�v
027980         MOVE ��|�Z���P       TO ��ی��ҏZ���P�v
027990         MOVE ��|�Z���Q       TO ��ی��ҏZ���Q�v
      */ �d�b�ԍ��ǉ� /42505
               IF ��|�d�b�ԍ� NOT = SPACE
                  STRING "�d�b:"            DELIMITED BY SIZE
                         ��|�d�b�ԍ�       DELIMITED BY SPACE
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
028230         WHEN 5
028240             MOVE NC"�ߘa"  TO �����v
028250             MOVE NC"��"    TO �ߘa�`�F�b�N�v
028260         END-EVALUATE
028270*
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
028450**
028460         IF ( ��|�ی���� = 01 OR 08 OR 05) AND
028470            ( ��|������� NOT = ZERO )
028480            PERFORM �������Z�܂Ƃߔ���
028490         ELSE
028500            MOVE SPACE TO �������Z�܂Ƃ߃t���O
028510         END-IF
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
028880 ��������擾 SECTION.
028890*
028900****************************************************
028910* �A���f�[�^����ی��҃}�X�^��萿������擾����B *
028920* ���ہ|��������敪=1�̏ꍇ������}�X�^���g�p   *
028930* �� ������...... �����於�̂v�Ɋi�[               *
028940****************************************************
028950     MOVE �ی���ʂv�q   TO �ہ|�ی����.
028960     MOVE �ی��Ҕԍ��v�q TO �ہ|�ی��Ҕԍ�.
028970     READ �ی��҃}�X�^
028980     INVALID KEY
               IF ( �ی���ʂv�q = 05 ) AND ( �{�p�a��N���v�q >= 42004 )
030800             MOVE �ی���ʂv�q   TO �s�|������
030810             MOVE �ی��Ҕԍ��v�q TO �s�|�s�����ԍ�
030820             READ �s�����}�X�^
030830             INVALID KEY
030840                 MOVE SPACE      TO �����於�̂v
030850             NOT INVALID KEY
031330                 MOVE �s�|�s��������    TO �����於�̂v
                   END-READ
               ELSE
030840             MOVE SPACE      TO �����於�̂v
               END-IF
029000     NOT INVALID KEY
029010* �ЕہA���ق́u�Љ�ی��������v������
029020                 EVALUATE �ی���ʂv�q 
029030                 WHEN  02
029040                 WHEN  06
029050                     IF ( �ہ|�ڔ���敪 = 1 )
029060                        MOVE �ہ|�ی��Җ���    TO �����於�̂v
029070                     ELSE
029080                        STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
029090                               "�Љ�ی�������"  DELIMITED BY SIZE
029100                               INTO �����於�̂v
029110                        END-STRING
029120                     END-IF
029130* �g���͎x�����܂ň�
029140                 WHEN  03
029150                     STRING �ہ|�ی��Җ���  DELIMITED BY SPACE
029160                            "���N�ی��g��"  DELIMITED BY SIZE
029180                            �ہ|�x��������  DELIMITED BY SPACE
029190                            INTO �����於�̂v
029200                     END-STRING
029210* ���ς͎x�����܂ň�
029220                 WHEN  04
                           IF ��|�ی��Ҕԍ� = "34130021"
                               MOVE �ہ|�ی��Җ��� TO �����於�̂v
                           ELSE
029230                         STRING �ہ|�ی��Җ���  DELIMITED BY SPACE
029240                                "���ϑg��"      DELIMITED BY SIZE
029260                                �ہ|�x��������  DELIMITED BY SPACE
029270                                INTO �����於�̂v
029280                         END-STRING
                           END-IF
029290                 WHEN OTHER
029300                     MOVE �ہ|�ی��Җ���    TO �����於�̂v
029310                 END-EVALUATE
029320     END-READ.
           STRING �����於�̂v DELIMITED BY SPACE
                  "�a"         DELIMITED BY SIZE
             INTO �����於�̂v
           END-STRING. 
029330*
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
030110                 MOVE 99                   TO �I���N�v(���ʂb�m�s)
030120                 MOVE 99                   TO �I�����v(���ʂb�m�s)
030130                 MOVE 99                   TO �I�����v(���ʂb�m�s)
030140             ELSE
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
031490            MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
031500            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
031510            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
031520        END-IF
031530        IF ( �I���N�����v(���ʂb�m�s) > �󗝔N�����v )
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
032150     MOVE "YIW612P"  TO  ��`�̖��o.
032160     MOVE "SCREEN"   TO  ���ڌQ���o.
032170     WRITE YIW612P.
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
034780*================================================================*
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
037980               STRING NC"�������Z"       DELIMITED BY SIZE
037990                      �������Z�����P�v   DELIMITED BY SIZE
038000                      �������Z�����Q�v   DELIMITED BY SIZE
038010                      �������Z�����R�v   DELIMITED BY SIZE
038020                      INTO �K�p�Q�v
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
035750     PERFORM ���������Z�b�g.
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
039370     MOVE 56            TO  �A�E���|������.
039370*     MOVE 52            TO  �A�E���|������.
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
               IF �A���|�ی���� > 50
036620             MOVE  1  TO  ��|���Z����敪����
               ELSE
036620             MOVE  1  TO  ��|���Z����敪
               END-IF
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
038330     MOVE �󗝔N�v   TO �ŏI�ʉ@�N�v.
038340     MOVE �󗝌��v   TO �ŏI�ʉ@���v.
038350     MOVE �󗝓��v   TO �ŏI�ʉ@���v.
038360***
038370* (�_���t��)
038380     EVALUATE ���Z�v�g���t�敪�v 
038390*    /  �ŏI�ʉ@�� /
038400     WHEN ZERO
038410         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
038420         MOVE �ŏI�ʉ@���v TO �_���t���v
038430         MOVE �ŏI�ʉ@���v TO �_���t���v
038440*    /  ������ /
038450     WHEN 1 
038460         PERFORM �������擾
038470         MOVE �󗝔N�v     TO �_���t�N�v
038480         MOVE �󗝌��v     TO �_���t���v
038490         MOVE �󗝓��v     TO �_���t���v
038500*    /  �󎚂Ȃ� /
038510     WHEN 9
038520         MOVE ZERO         TO �_���t�N�v
038530         MOVE ZERO         TO �_���t���v
038540         MOVE ZERO         TO �_���t���v
038550*    /  ���̑��́A�ŏI�ʉ@�� /
038560     WHEN OTHER
038570         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
038580         MOVE �ŏI�ʉ@���v TO �_���t���v
038590         MOVE �ŏI�ʉ@���v TO �_���t���v
038600     END-EVALUATE.
038610**
038620* (���ґ�)
038630     EVALUATE ���Z�v�g���ғ��t�敪�v 
038640*    /  �ŏI�ʉ@�� /
038650     WHEN ZERO
038660         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
038670         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
038680         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
038690*    /  ������ /
038700     WHEN 1 
038710         PERFORM �������擾
038720         MOVE �󗝔N�v     TO ���҈ϔC�N�v
038730         MOVE �󗝌��v     TO ���҈ϔC���v
038740         MOVE �󗝓��v     TO ���҈ϔC���v
038750*    /  �󎚂Ȃ� /
038760     WHEN 9
038770         MOVE ZERO         TO ���҈ϔC�N�v
038780         MOVE ZERO         TO ���҈ϔC���v
038790         MOVE ZERO         TO ���҈ϔC���v
038800*    /  ���̑��́A�ŏI�ʉ@�� /
038810     WHEN OTHER
038820         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
038830         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
038840         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
038850     END-EVALUATE.
038860*
038870*================================================================*
038880*================================================================*
038890 �������Z�܂Ƃߔ��� SECTION.
038900**---------------------------------------------------------------------------*
038910** �s�����}�X�^��ǂ݁A���Z�܂Ƃߋ敪���P�ł��A�{�̕ی������ہE�ސE
038920** �̎��́A�t���OYES (���z���������݂ň󎚁j
038930**�i��F���l�s�̏�Q�́A�{�̕ی��i���یn�j�̃��Z�v�g�P���Ő����A�������Z�͂Ȃ��j
038940**---------------------------------------------------------------------------*
038950**
038960     MOVE SPACE TO �������Z�܂Ƃ߃t���O.
038970*     MOVE SPACE TO ������ʗ��̂v.
038980**
038990*     MOVE ��|�������           TO �s�|������.
039000*     MOVE ��|��p���S�Ҕԍ����� TO �s�|�s�����ԍ�.
039010*     READ �s�����}�X�^
039020*     NOT INVALID KEY
039030*         IF ( �s�|���Z�܂Ƃߋ敪 = 1 )
039040*            IF (( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(3:1) NOT = "3" )) OR
039050*               ( ��|�ی���� = 08 ) 
039060*                MOVE "YES" TO �������Z�܂Ƃ߃t���O
039070**
039080*                MOVE 02            TO ���|�敪�R�[�h
039090*                MOVE ��|�������  TO ���|���̃R�[�h
039100*                READ ���̃}�X�^
039110*                NOT INVALID KEY
039120*                    MOVE ���|����  TO ������ʗ��̂v
039130*                END-READ
039140*            END-IF
039150*         END-IF
039160*     END-READ.
039170**
039180*** / CALL JRECEOFF /
039190*     IF ( �������Z�܂Ƃ߃t���O = SPACE )
039200*        INITIALIZE �A���Z�܂Ƃ߁|�L�[
039210*        MOVE �{�p�a��v�q TO �A���Z�܂Ƃ߁|�{�p�a��
039220*        MOVE �{�p�N�v�q   TO �A���Z�܂Ƃ߁|�{�p�N
039230*        MOVE �{�p���v�q   TO �A���Z�܂Ƃ߁|�{�p��
039240*        MOVE ���Ҕԍ��v�q TO �A���Z�܂Ƃ߁|���Ҕԍ�
039250*        MOVE �}�Ԃv�q     TO �A���Z�܂Ƃ߁|�}��
039260**       1:�������Z�v�g�Ȃ��̖{�̂܂Ƃ߂̔���
039270*        MOVE 1            TO �A���Z�܂Ƃ߁|����敪
039280*        CALL   "JRECEOFF"
039290*        CANCEL "JRECEOFF"
039300**
039310*        IF ( �A���Z�܂Ƃ߁|���茋�� = 1 )
           IF ( ���Z�|�{�̂܂Ƃߋ敪 = 1 )
039320           MOVE "YES" TO �������Z�܂Ƃ߃t���O
039330*        END-IF
039340     END-IF.
039350*
039360*----------------------------------------------------------------------*
039370** / �_�ސ쌧�ŗL�F�E�v�ɕ��S�Ҕԍ��Ǝ󋋎Ҕԍ� /
039380     IF ( �������Z�܂Ƃ߃t���O = "YES" ) AND
039390        ( ��|��p���S�Ҕԍ�����(3:2) = "14" )
039400        IF ( ��|��p���S�Ҕԍ�����(1:2) NOT = "99" )
039410*            MOVE ALL NC"�P" TO �����P �����Q �����R
039420*            MOVE ALL NC"�b" TO �c���P �c���Q
039430*            MOVE NC"�b"     TO �c���R �c���S
039440*            MOVE NC"����S�Ҕԍ�"     TO �_�ސ�Œ�P
039450*            MOVE NC"�󋋎Ҕԍ�"         TO �_�ސ�Œ�Q
039460*            MOVE NC"�^"                 TO �_�ސ�Œ�R
039470            MOVE ��|��p���S�Ҕԍ����� TO ����S�Ҕԍ�
039480            MOVE ��|��v�Ҕԍ�����     TO �󋋎Ҕԍ�
039490        END-IF
039500     END-IF.
039510*
039520*================================================================*
039530*================================================================*
039540 ���������v�Z SECTION.
           EVALUATE ��|�ی����
           WHEN 05
               MOVE 2          TO ���Z�|���Z���
           WHEN OTHER
               MOVE 1          TO ���Z�|���Z���
           END-EVALUATE.
019550     MOVE ��|�{�p�a�� TO ���Z�|�{�p�a��.
019560     MOVE ��|�{�p�N   TO ���Z�|�{�p�N.
019570     MOVE ��|�{�p��   TO ���Z�|�{�p��.
019580     MOVE ��|���Ҕԍ� TO ���Z�|���Ҕԍ�.
019590     MOVE ��|�}��     TO ���Z�|�}��.
019600     READ ���Z�v�g�e
019630     INVALID KEY
              MOVE SPACE     TO ���Z�|���R�[�h
              INITIALIZE        ���Z�|���R�[�h
           END-READ.
039780*
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
           MOVE �A�E���|�E�v��(8)    TO �������R���W.
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
040210           �o�߃}�X�^       ��f�ҏ��e        �����}�X�^
040220           �{�p�L�^�e       �����f�[�^�e        ���������e
040230           �h�c�Ǘ��}�X�^   �s�����}�X�^
040240           ��ƃt�@�C���S.
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
           �󗝔N �󗝌� �󗝓� �ϔC�N �ϔC�� �ϔC�� �^����×� ������ �^����
           .
           MOVE ALL "X" TO
           ���ϔԍ� �n���ϔԍ� ���{�p�h�c �ی��Ҕԍ� �L���ԍ� ����S�Ҕԍ� �󋋎Ҕԍ� �Z���P �Z���Q 
           �_���t�ԍ� �����ԍ�
           ��s���P ��s���Q �x�X���P �x�X���Q �������`�l�J�i�P �������`�l
           �{�p���X�֔ԍ��P �{�p���X�֔ԍ��Q 
           �{�p���Z���P �{�p���Z���Q �{�p���d�b�ԍ� ��\�҃J�i ��\�Җ�
           ���������P ���������Q ���������R ���������S ���������T ���������U ���������V ���������W
           �������R���P �������R���Q �������R���R �������R���S �������R���T ���ʂT�W ���ʂT�O
           �������R���U �������R���V �������R���W �K�p�R
           �ڍ��@�� ��\�Җ� ��ی��Ҏ��� ���Ҏ���  �ی��Җ��� �ی��Җ��̂P �ی��Җ��̂Q
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
           �{�×��`�F�b�N ��ԃ`�F�b�N �\���J��`�F�b�N ��H�`�F�b�N 
           ���ʃ`�F�b�N �U���`�F�b�N �����`�F�b�N ��s�`�F�b�N ���Ƀ`�F�b�N �_���`�F�b�N 
           �{�X�`�F�b�N �x�X�`�F�b�N �{�x���`�F�b�N �ߘa�`�F�b�N
           .
           MOVE "�󋋎ҕ��S�z"                 TO �󋋎ҕ��S�z�b�l.
           MOVE "���������z"                   TO ���������z�b�l.
           MOVE "�~"                           TO �~�P �~�Q.
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
023680*================================================================*
023691 �U�������Z�b�g SECTION.
023692*
023693*****************************************
023694*  �ی��ҕʂɐU��������ݒ肷��
023695*****************************************
023696*
023702     MOVE SPACE    TO �I���t���O.
023703*
023716     OPEN INPUT �U�������e.
023717             MOVE NC"�U��" TO �t�@�C����.
023718             PERFORM �I�[�v���`�F�b�N.
023719*
023722     PERFORM �U�������e�Ǎ�.
023723     PERFORM UNTIL �I���t���O NOT = SPACE
023724*        ����������񕪉�
023725         UNSTRING �����|���R�[�h�f�[�^  DELIMITED BY ","
023726             INTO �����ی��Ҕԍ��v �����ی��Җ��v ���������ԍ��v ���Z�@�փR�[�h�v
023728         END-UNSTRING
      *        ���Z�@�փR�[�h(2004-135)�̎��͌����ԍ�(1029444)�Œ�
023731*        ��������ی��Ҕԍ��ƃ}�b�`���邩�i�擪�̕ی��Ҕԍ�0�͖��o�^���p�Ȃ̂Ŗ������Z�b�g�j
               IF �����ی��Ҕԍ��v = �ی��Ҕԍ��v
                   IF ���Z�@�փR�[�h�v = "2004-135"
023746                 MOVE "1029444"        TO �����ԍ��v
                   ELSE
023746                 MOVE ���������ԍ��v   TO �����ԍ��v
023747             END-IF
                   MOVE "YES"                TO �I���t���O
               ELSE
                   MOVE "3620000"        TO �����ԍ��v
023747         END-IF
023748         PERFORM �U�������e�Ǎ�
023749     END-PERFORM.
023719*
023752     CLOSE �U�������e.
023719*
023703*/�ی��Ҕԍ�����v���Ȃ��������A���܂ł̑O����v�`�F�b�N/
023719*
           IF �����ԍ��v = "3620000"
023702         MOVE SPACE    TO �I���t���O
023716         OPEN INPUT �U�������e
023717             MOVE NC"�U��" TO �t�@�C����
023718             PERFORM �I�[�v���`�F�b�N
023719*
023722         PERFORM �U�������e�Ǎ�
023723         PERFORM UNTIL �I���t���O NOT = SPACE
023724*        ����������񕪉�
023725             UNSTRING �����|���R�[�h�f�[�^  DELIMITED BY ","
023726                INTO �����ی��Ҕԍ��v �����ی��Җ��v ���������ԍ��v ���Z�@�փR�[�h�v
023728             END-UNSTRING
      *
                   PERFORM VARYING �J�E���^ FROM 1 BY 1
                           UNTIL (�����ی��Ҕԍ��v(�J�E���^:1) = "@") OR
                                 (�J�E���^ > 10)
                       CONTINUE
                   END-PERFORM
                   IF (�����ی��Ҕԍ��v(1:1) NOT = "@") AND
                      (�����ی��Ҕԍ��v(�J�E���^:1) = "@") AND
                      (�����ی��Ҕԍ��v(1:�J�E���^ - 1) = �ی��Ҕԍ��v(1:�J�E���^ - 1))
023746                 MOVE ���������ԍ��v   TO �����ԍ��v
                       MOVE "YES"            TO �I���t���O
                   ELSE
                       MOVE "3620000"        TO �����ԍ��v
                   END-IF
023748             PERFORM �U�������e�Ǎ�
               END-PERFORM
023719*
023752         CLOSE �U�������e
           END-IF.
023719*
023703*/�ی��Ҕԍ�����v���Ȃ��������A���I������̌����v�`�F�b�N/
023719*
           IF �����ԍ��v = "3620000"
023702         MOVE SPACE    TO �I���t���O
023716         OPEN INPUT �U�������e
023717             MOVE NC"�U��" TO �t�@�C����
023718             PERFORM �I�[�v���`�F�b�N
023719*
023722         PERFORM �U�������e�Ǎ�
023723         PERFORM UNTIL �I���t���O NOT = SPACE
023724*        ����������񕪉�
023725             UNSTRING �����|���R�[�h�f�[�^  DELIMITED BY ","
023726                INTO �����ی��Ҕԍ��v �����ی��Җ��v ���������ԍ��v ���Z�@�փR�[�h�v
023728             END-UNSTRING
      *
                   PERFORM VARYING �J�E���^ FROM 1 BY 1
                           UNTIL (�����ی��Ҕԍ��v(�J�E���^:1) NOT = "@") OR
                                 (�J�E���^ > 10)
                       CONTINUE
                   END-PERFORM
                   IF (�����ی��Ҕԍ��v(1:1) = "@") AND
                      (�����ی��Ҕԍ��v(�J�E���^:10 - �J�E���^) = �ی��Ҕԍ��v(�J�E���^:10 - �J�E���^))
023746                 MOVE ���������ԍ��v   TO �����ԍ��v
                       MOVE "YES"            TO �I���t���O
                   ELSE
                       MOVE "3620000"        TO �����ԍ��v
                   END-IF
023748             PERFORM �U�������e�Ǎ�
               END-PERFORM
023719*
023752         CLOSE �U�������e
           END-IF.
      *
           IF ���Z�@�փR�[�h�v = "2004-135"
023560         MOVE "���H�g������"      TO ���Z�@�֖��v
009758         MOVE NC"��"              TO ���Ƀ`�F�b�N�v
023570         MOVE "����"              TO �x�X���v
               MOVE NC"��"              TO �x�X�`�F�b�N�v
               MOVE NC"��"              TO ���ʃ`�F�b�N�v
           ELSE
023560         MOVE "��t"              TO ���Z�@�֖��v
009758         MOVE NC"��"              TO ��s�`�F�b�N�v
023570         MOVE "�Ђ܂����"      TO �x�X���v
               MOVE NC"��"              TO �x�X�`�F�b�N�v
               MOVE NC"��"              TO ���ʃ`�F�b�N�v
           END-IF.
023754*
023755*================================================================*
023756 �U�������e�Ǎ� SECTION.
023757*
023761     READ �U�������e
023762     AT END
023763         MOVE "YES"  TO �I���t���O
023764     END-READ.
023767*
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
041780******************************************************************
041790 END PROGRAM YIW612.
041800******************************************************************
