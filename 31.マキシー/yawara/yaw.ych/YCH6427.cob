000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH6427.
000060 AUTHOR.                 �R�c �_�V
000070*
000080*----------------------------------------------------------------*
000090*         ���� �������Z�v�g����i�_+����޳�ޔŁj
000100*         MED = YAW610 YCH6427P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2024-10-05
000130 DATE-COMPILED.          2024
      */�������̓��Z�|���ʎ�������]�L����/160816
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
      */���׏����s���Z��K�p�ɒǉ�/2022
      */2022.11���V�p���ɐ؂�ւ�/2022
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
000370     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000380                             ORGANIZATION             IS  INDEXED
000390                             ACCESS MODE              IS  DYNAMIC
000400                             RECORD KEY               IS  �s�|������
000410                                                          �s�|�s�����ԍ�
000420                             ALTERNATE RECORD KEY     IS  �s�|������
000430                                                          �s�|�s��������
000440                                                          �s�|�s�����ԍ�
000450                             FILE STATUS              IS  ��ԃL�[
000460                             LOCK        MODE         IS  AUTOMATIC.
000470     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000480                             ORGANIZATION             IS  INDEXED
000490                             ACCESS MODE              IS  DYNAMIC
000500                             RECORD KEY               IS  ���|�����敪
000510                             FILE STATUS              IS  ��ԃL�[
000520                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS MODE              IS  DYNAMIC
000560                             RECORD KEY               IS  ���|�敪�R�[�h
000570                                                          ���|���̃R�[�h
000580                             FILE STATUS              IS  ��ԃL�[
000590                             LOCK        MODE         IS  AUTOMATIC.
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
000660     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000670                             ORGANIZATION             IS  INDEXED
000680                             ACCESS MODE              IS  DYNAMIC
000690                             RECORD KEY               IS  ���|����敪
000700                             FILE STATUS              IS  ��ԃL�[
000710                             LOCK        MODE         IS  AUTOMATIC.
000720     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000730                             ORGANIZATION             IS  INDEXED
000740                             ACCESS MODE              IS  DYNAMIC
000750                             RECORD KEY               IS  �{��|�{�p���ԍ�
000760                             FILE STATUS              IS  ��ԃL�[
000770                             LOCK        MODE         IS  AUTOMATIC.
000780     SELECT  ������}�X�^    ASSIGN      TO        SEIKYUSL
000790                             ORGANIZATION             IS  INDEXED
000800                             ACCESS MODE              IS  DYNAMIC
000810                             RECORD KEY               IS  ����|�ی����
000820                                                          ����|�ی��Ҕԍ�
000830                             FILE STATUS              IS  ��ԃL�[
000840                             LOCK    MODE             IS  AUTOMATIC.
000850     SELECT  �o�߃}�X�^      ASSIGN      TO        KEIKAL
000860                             ORGANIZATION             IS  INDEXED
000870                             ACCESS MODE              IS  DYNAMIC
000880                             RECORD KEY               IS  �o�|�敪�R�[�h
000890                                                          �o�|�o�߃R�[�h
000900                             FILE STATUS              IS  ��ԃL�[
000910                             LOCK        MODE         IS  AUTOMATIC.
000920     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000930                             ORGANIZATION             IS  INDEXED
000940                             ACCESS MODE              IS  DYNAMIC
000950                             RECORD KEY               IS  ��|�{�p�a��N��
000960                                                          ��|���҃R�[�h
000970                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000980                                                          ��|���҃J�i
000990                                                          ��|���҃R�[�h
001000                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
001010                                                          ��|�{�p�a��N��
001020                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001030                                                          ��|�ی����
001040                                                          ��|�ی��Ҕԍ�
001050                                                          ��|���҃R�[�h
001060                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001070                                                          ��|������
001080                                                          ��|��p���S�Ҕԍ�
001090                                                          ��|���҃R�[�h
001100                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001110                                                          ��|�������
001120                                                          ��|��p���S�Ҕԍ�����
001130                                                          ��|���҃R�[�h
001140                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
001150                                                          ��|�{�p�a��N��
001160                                                          ��|���҃R�[�h
001170                             FILE STATUS              IS  ��ԃL�[
001180                             LOCK        MODE         IS  AUTOMATIC.
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
001190     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
001200                             ORGANIZATION             IS  INDEXED
001210                             ACCESS MODE              IS  DYNAMIC
001220                             RECORD KEY               IS  �{�L�|�{�p�a��N����
001230                                                          �{�L�|���҃R�[�h
001240                             ALTERNATE RECORD KEY     IS  �{�L�|���҃R�[�h
001250                                                          �{�L�|�{�p�a��N����
001260                             FILE STATUS              IS  ��ԃL�[
001270                             LOCK        MODE         IS  AUTOMATIC.
001280     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
001290                             ORGANIZATION             IS  INDEXED
001300                             ACCESS MODE              IS  DYNAMIC
001310                             RECORD KEY               IS  ���|�{�p�a��N��
001320                                                          ���|���҃R�[�h
001330                             ALTERNATE RECORD KEY     IS  ���|���҃R�[�h
001340                                                          ���|�{�p�a��N��
001350                             FILE STATUS              IS  ��ԃL�[
001360                             LOCK        MODE         IS  AUTOMATIC.
001370     SELECT  ���������e      ASSIGN      TO        HUGEINL
001380                             ORGANIZATION             IS  INDEXED
001390                             ACCESS MODE              IS  DYNAMIC
001400                             RECORD KEY               IS  �����|�敪�R�[�h
001410                                                          �����|���������R�[�h
001420                             FILE STATUS              IS  ��ԃL�[
001430                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  ����}�X�^    ASSIGN      TO        KAIJOHOL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  ���|�_���I���敪
                                                                ���|����R�[�h
001380                                                          ���|�ی����
001390                                                          ���|�ύX�a��N��
001400                             ALTERNATE RECORD KEY     IS  ���|�_���I���敪
                                                                ���|�ڍ��t��J�i
001410                                                          ���|����R�[�h
001420                                                          ���|�ی����
001430                                                          ���|�ύX�a��N��
001440                             FILE STATUS              IS  ��ԃL�[
001450                             LOCK        MODE         IS  AUTOMATIC.
001560     SELECT  �h�c�Ǘ��}�X�^    ASSIGN      TO        IDKANRL
001570                             ORGANIZATION             IS  INDEXED
001580                             ACCESS MODE              IS  DYNAMIC
001590                             RECORD KEY               IS  �h�c�ǁ|�h�c�敪
001600                                                          �h�c�ǁ|�{�p���ԍ�
001610                                                          �h�c�ǁ|�ی����
001620                                                          �h�c�ǁ|�ی��Ҕԍ�
001630                             ALTERNATE RECORD KEY     IS  �h�c�ǁ|�{�p�h�c�ԍ�
001640                                                          �h�c�ǁ|�h�c�敪
001650                                                          �h�c�ǁ|�{�p���ԍ�
001660                                                          �h�c�ǁ|�ی����
001670                                                          �h�c�ǁ|�ی��Ҕԍ�
001680                             FILE STATUS              IS  ��ԃL�[
001690                             LOCK        MODE         IS  AUTOMATIC.
001700     SELECT  �����t�@�C��    ASSIGN      TO        MEMOL
001710                             ORGANIZATION             IS  INDEXED
001720                             ACCESS MODE              IS  DYNAMIC
001730                             RECORD KEY               IS  �����|����敪
001740                                                          �����|���҃R�[�h
001750                                                          �����|�{�p�a��N����
001760                             ALTERNATE RECORD KEY     IS  �����|����敪
001770                                                          �����|�{�p�a��N����
001780                                                          �����|���҃R�[�h
001790                             ALTERNATE RECORD KEY     IS  �����|���҃R�[�h
001800                                                          �����|�{�p�a��N����
001810                                                          �����|����敪
001820                             FILE STATUS              IS  ��ԃL�[
001830                             LOCK        MODE         IS  AUTOMATIC.
001700* ���я��󎚗p
001710     SELECT  ��ƃt�@�C���S  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001720                             ORGANIZATION             IS  INDEXED
001730                             ACCESS                   IS  DYNAMIC
001740                             RECORD      KEY          IS  ��S�|�{�p�a��N��
001750                                                          ��S�|���҃R�[�h
001760                                                          ��S�|�ی����
001770                             FILE        STATUS       IS  ��ԃL�[
001780                             LOCK        MODE         IS  AUTOMATIC.
001790*
001800     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
001810                             SYMBOLIC    DESTINATION  IS "PRT"
001820                             FORMAT                   IS  ��`�̖��o
001830                             GROUP                    IS  ���ڌQ���o
001840                             PROCESSING  MODE         IS  ������ʂo
001850                             UNIT        CONTROL      IS  �g������o
001860                             FILE        STATUS       IS  �ʒm���o.
001870******************************************************************
001880*                      DATA DIVISION                             *
001890******************************************************************
001900 DATA                    DIVISION.
001910 FILE                    SECTION.
001920*                           �m�q�k��  �R�Q�O�n
001930 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001940     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001950*                           �m�q�k��  �Q�T�U�n
001960 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001970     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
001980*                           �m�q�k��  �P�Q�W�n
001990 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002000     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002010*                           �m�q�k��  �P�Q�W�n
002020 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
002030     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
002070*                           �m�q�k��  �Q�T�U�n
002080 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
002090     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002100*                           �m�q�k��  �P�Q�W�n
002110 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002120     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
002130*                           �m�q�k��  �P�Q�W�n
002140 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
002150     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
002160*                           �m�q�k��  �P�Q�W�n
002170 FD  �o�߃}�X�^          BLOCK   CONTAINS   1   RECORDS.
002180     COPY KEIKA          OF  XFDLIB  JOINING   �o   AS  PREFIX.
002190*                           �m�q�k��  �R�Q�O�n
002200 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
002210     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002560*                          �m�q�k��  1024�n
000340 FD  ��f�ҏ��Q�e        BLOCK   CONTAINS   1   RECORDS.
000350     COPY JUSINJ2          OF  XFDLIB  JOINING   ��Q   AS  PREFIX.
002220*                           �m�q�k��  �Q�T�U�n
002230 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
002240     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
002250*                           �m�q�k��  �P�Q�W�n
002260 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
002270     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002280*                           �m�q�k��  �P�Q�W�n
002290 FD  ���������e         BLOCK   CONTAINS   1   RECORDS.
002300     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
002310*                           �m�q�k��  �U�S�O�n
002320 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
002330     COPY KAIJOHO         OF  XFDLIB  JOINING   ���   AS  PREFIX.
002340*                           �m�q�k��  �P�Q�W�n
002350 FD  �h�c�Ǘ��}�X�^          BLOCK   CONTAINS   1   RECORDS.
002360     COPY IDKANR    OF  XFDLIB  JOINING   �h�c��   AS  PREFIX.
002510*                           �m�q�k��  �W�R�Q�n
002520 FD  �����t�@�C��        BLOCK CONTAINS 1     RECORDS.
002530     COPY MEMO           OF    XFDLIB JOINING ���� AS PREFIX.
002370**
002380 FD  ��ƃt�@�C���S RECORD  CONTAINS 32 CHARACTERS.
002390 01  ��S�|���R�[�h.
002400     03  ��S�|���R�[�h�L�[.
002410         05  ��S�|�{�p�a��N��.
002420             07  ��S�|�{�p�a��            PIC 9.
002430             07  ��S�|�{�p�N              PIC 9(2).
002440             07  ��S�|�{�p��              PIC 9(2).
002450         05  ��S�|���҃R�[�h.
002460             07 ��S�|���Ҕԍ�             PIC 9(6).
002470             07 ��S�|�}��                 PIC X(1).
002480         05  ��S�|�ی����                PIC 9(2).
002490     03  ��S�|���R�[�h�f�[�^.
002500         05  ��S�|����                    PIC 9(4).
002510         05  FILLER                        PIC X(14).
002520*
002530 FD  ����t�@�C��.
002540     COPY YCH6427P       OF  XMDLIB.
002550*----------------------------------------------------------------*
002560******************************************************************
002570*                WORKING-STORAGE SECTION                         *
002580******************************************************************
002590 WORKING-STORAGE         SECTION.
002600 01 �L�[����                           PIC X     VALUE SPACE.
002610 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
002620 01 �I���t���O                         PIC X(3)  VALUE SPACE.
002630 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
002820 01 �I���t���O�R                       PIC X(3)  VALUE SPACE.
002640 01 �����t���O                         PIC X(3)  VALUE SPACE.
002650 01 �t�@�C����                         PIC N(6)  VALUE SPACE.
002660 01 ���Z�v�g�o�f�v                     PIC X(8)  VALUE SPACE.
002670 01 �O�a��v                           PIC 9     VALUE ZERO.
002680 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
002690 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002700 01 ���Ҕԍ��v                         PIC 9(6)  VALUE ZERO.
002710 01 �������̂v                         PIC N(6)  VALUE SPACE.
002720 01 ���ʖ��̂v                         PIC N(12) VALUE SPACE.
002730 01 ���ʒ��v                           PIC 9(2) VALUE 1.
001363 01 �S�p��                           PIC X(2)  VALUE X"8140".
001364 01 ���p��                           PIC X(2)  VALUE X"2020".
002870 01 �p����ʂv                         PIC 9(1)  VALUE ZERO.
002740** ���������{��ϊ�
002750 01 �����v                             PIC 9(2).
002760 01 �����q REDEFINES �����v.
002770    03 �����v�P                        PIC X(1).
002780    03 �����v�Q                        PIC X(1).
002790*
002800 01 �����ԍ��v                         PIC 9.
002810 01 �����ԍ��q REDEFINES �����ԍ��v.
002820    03 �����ԍ��v�P                    PIC X.
002830*
002840 01 �S�p�����ԍ��v                     PIC N.
002850 01 �S�p�����ԍ��q REDEFINES �S�p�����ԍ��v.
002860    03 �S�p�����ԍ��v�P                PIC X(2).
002870*
002880 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
002890 01 �J�E���^�Q                         PIC 9(2)  VALUE ZERO.
002900*
002910 01 �s���{���v                         PIC X(2)  VALUE SPACE.
002920*
002930* �ޔ�p
002940 01 �I���N�����v�s.
002950    03 �I���N�v�s                      PIC 9(2)  VALUE ZERO.
002960    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
002970    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
002980* �������ޔ�p
002990 01 �����N�����v�s.
003000    03 �����a��v�s                    PIC 9     VALUE ZERO.
003010    03 �����N�v�s                      PIC 9(2)  VALUE ZERO.
003020    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003030    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003040* �������Z�����p
003050 01 �������Z�v�s.
003060    03 �������Z�J�E���g                PIC 9    VALUE ZERO.
003070    03 �ԍ��J�E���^                    PIC 9    VALUE ZERO.
003080    03 �������Z�W�c�v�s  OCCURS 3.
003090       05 �������Z�敪�v�s             PIC 9    VALUE ZERO.
003100       05 �������Z���v�s               PIC 9(2) VALUE ZERO.
003110       05 �������Z���v�s               PIC 9(2) VALUE ZERO.
003120    03 �������Z�W�c�m�v  OCCURS 3.
003130       05 ���Z��؂v                   PIC N(1) VALUE SPACE.
003140       05 ���Z���e�v                   PIC N(3) VALUE SPACE.
003150       05 �������Z���m�v�P             PIC N(1) VALUE SPACE.
003160       05 �������Z���m�v�Q             PIC N(1) VALUE SPACE.
003170       05 ���Œ�v                     PIC N(1) VALUE SPACE.
003180       05 �������Z���m�v�P             PIC N(1) VALUE SPACE.
003190       05 �������Z���m�v�Q             PIC N(1) VALUE SPACE.
003200       05 ���Œ�v                     PIC N(1) VALUE SPACE.
003210    03 �������Z�����P�v                PIC N(10) VALUE SPACE.
003220    03 �������Z�����Q�v                PIC N(10) VALUE SPACE.
003230    03 �������Z�����R�v                PIC N(10) VALUE SPACE.
003070    03 �������Z��؂v                  PIC X     VALUE SPACE.
003080    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003090    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003240* ���������p
003250 01 ���������v�s.
003260    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
003270    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
003280    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
003290    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
003300    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
003310    03 ���������i���o�[�v�s.
003320       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
003330    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
003340 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
003350 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
003360 01 ���������s�a�k.
003370    03 ���������R�[�h�s�a�k            OCCURS 9.
003380       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
003390       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
003400       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
003410 01 �����������e�v.
003420    03 �����������e�����v              PIC X(318) OCCURS 9 VALUE SPACE.
003430    03 �����������e�����w�v.
003440       05 �����������e�P�w�v           PIC X(80)  VALUE SPACE.
003450       05 �����������e�Q�w�v           PIC X(80)  VALUE SPACE.
003460       05 �����������e�R�w�v           PIC X(80)  VALUE SPACE.
003700       05 �����������e�S�w�v           PIC X(78)  VALUE SPACE.
003470*
003480*
003490* �S�p�������o�p
003500 01 �S�p�����v�s.
003510     03 ���ݕ����S�̂v.
003520        05 ���ݕ����S�̂P�v            PIC X(2) OCCURS 8 VALUE SPACE.
003530     03 ���ݕ����v.
003540        05 ���ݕ����P�v                PIC X(2) OCCURS 8 VALUE SPACE.
003550     03 �S�p�����v.
003560        05 �S�p�����P�v                PIC N(1) OCCURS 8 VALUE SPACE.
003570     03 �J�E���^�R                     PIC 9(2)  VALUE ZERO.
003580* �V�_���t�ԍ����o�p
003590 01 �V�_���t�ԍ��v�s.
003600     03 �V�_���t�ԍ��P�v               PIC X(2)  VALUE SPACE.
003610     03 �V�_���t�ԍ��Q�v               PIC X(3)  VALUE SPACE.
003620     03 �V�_���t�ԍ��R�v.
003630        05 �V�_���t�ԍ��R�P�v          PIC X OCCURS 4 VALUE SPACE.
003640     03 �V�_���t�ԍ��S�v               PIC X(4)  VALUE SPACE.
003650*
003660* �����U���� ����p(�T�����j
003670 01 ����v                           PIC N(5)   VALUE SPACE.
003680*
003690** �O�������̂ݗp
003700 01 �����Č��t���O                     PIC X(3)  VALUE SPACE.
003710 01 �O���t���O                         PIC X(3)  VALUE SPACE.
003720*
003730 01 �v�Z�N�����v.
003740    03 �v�Z�a��v                      PIC 9(1)  VALUE ZERO.
003750    03 �v�Z�N�v                        PIC S9(2)  VALUE ZERO.
003760    03 �v�Z���v                        PIC S9(2)  VALUE ZERO.
003770    03 �v�Z���v                        PIC S9(2)  VALUE ZERO.
003780 01 �J�n�N�����Q�v.
003790    03 �J�n�a��Q�v                    PIC 9(1)  VALUE ZERO.
003800    03 �J�n�N�Q�v                      PIC 9(2)  VALUE ZERO.
003810    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
003820    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
003830    03 �J�n����N�v                    PIC S9(4) VALUE ZERO.
003840 01 �I���N�����Q�v.
003850    03 �I���a��Q�v                    PIC 9(1)  VALUE ZERO.
003860    03 �I���N�Q�v                      PIC 9(2)  VALUE ZERO.
003870    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
003880    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
003890    03 �I������N�v                    PIC S9(4) VALUE ZERO.
003900***
003910** ���������E�������R����敪�p
003920 01 ������������敪�v                 PIC 9 VALUE ZERO.
003930 01 �������R����敪�v                 PIC 9 VALUE ZERO.
003940*
003950** ���Z���i�̓��t�敪�p (0:�ŏI�ʉ@���A1:�������A9:�󎚂Ȃ�)
003960 01 ���Z�v�g���t�敪�v                 PIC 9 VALUE ZERO.
003970 01 ���Z�v�g���ғ��t�敪�v             PIC 9 VALUE ZERO.
003980*
003990** �������p
004000 01 �{�p����N�v                       PIC 9(4)  VALUE ZERO.
004010 01 ���v                               PIC 9(3)  VALUE ZERO.
004020 01 �]�v                               PIC 9(3)  VALUE ZERO.
004030*
004040** �}�Ԕ���p
004050 01 �J�n�f�Ó��蓮�敪�v               PIC 9    VALUE ZERO.
004060*
004210** ���Z�E�v�p( N(38)�Œ�j /
004220 01 �����̌o�߂v.
004230    03 �����̌o�ߍs�v                  PIC X(76) OCCURS 2 VALUE SPACE.
004240 01 �����̌o�߂m�v REDEFINES �����̌o�߂v.
004250    03 �����̌o�ߍs�m�v                PIC N(38) OCCURS 2.
004070*
004080* ������������敪
004090 01 ���Z������������敪�v             PIC 9    VALUE ZERO.
004100*
003460 01 �������R����敪�e                 PIC 9 VALUE ZERO.
004101*
004102* �����̌o�ߌŒ�󎚗p�Ɏg�p
004103 01 �S�_�e�o�c�敪�v                   PIC 9     VALUE ZERO.
004104 01 �o�ߕ��ʐ����v                     PIC N(1)  VALUE SPACE.
      *
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
       01 �������q�b�l                       PIC X(200) VALUE SPACE.
       01 �^����Âb�l                       PIC X(68)  VALUE SPACE.
004105*
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
004106*
004110****************
004120* �A�����ڑҔ� *
004130****************
004140*    ************
004150*    * ����L�[ *
004160*    ************
004170 01 �Ώۃf�[�^�v�q.
004180    03 �{�p�a��N���v�q.
004190       05 �{�p�a��v�q                  PIC 9(1)  VALUE ZERO.
004200       05 �{�p�N�v�q                    PIC 9(2)  VALUE ZERO.
004210       05 �{�p���v�q                    PIC 9(2)  VALUE ZERO.
004220    03 �ی���ʂv�q                     PIC 9(2)  VALUE ZERO.
004230    03 �ی��Ҕԍ��v�q                   PIC X(10) VALUE SPACE.
004240    03 �����ʂv�q                     PIC 9(2)  VALUE ZERO.
004250    03 ��p���S�Ҕԍ��v�q               PIC X(10) VALUE SPACE.
004260    03 ������ʂv�q                     PIC 9(2)  VALUE ZERO.
004270    03 ��p���S�Ҕԍ������v�q           PIC X(10) VALUE SPACE.
004280    03 �{�l�Ƒ��敪�v�q                 PIC 9(1)  VALUE ZERO.
004290    03 ���҃J�i�v�q                     PIC X(50) VALUE SPACE.
004300    03 ���҃R�[�h�v�q.
004310       05 ���Ҕԍ��v�q                  PIC 9(6)  VALUE ZERO.
004320       05 �}�Ԃv�q                      PIC X(1)  VALUE SPACE.
004330*    ************
004340*    * ������� *
004350*    ************
004360*    �����̗���
004370***********************
004380 01 �����P�v�q.
004390   03 �����v�q.
004400      05 ���S�����v�q               PIC 9(3)    VALUE ZERO.
004410      05 �������v�q                 PIC 9(5)    VALUE ZERO.
004420      05 �������Z���v�q             PIC 9(5)    VALUE ZERO.
         03 ���������k���v�q              PIC 9(4)    VALUE ZERO.
004430   03 �Č����v�q                    PIC 9(5)    VALUE ZERO.
004440   03 ���Âv�q.
004450      05 ���Ë����v�q               PIC 9(2)V9  VALUE ZERO.
004460      05 ���É񐔂v�q               PIC 9(2)    VALUE ZERO.
004470      05 ���×��v�q                 PIC 9(6)    VALUE ZERO.
004480      05 ���É��Z���v�q             PIC 9(5)    VALUE ZERO.
004490   03 �������q���Z���v�q            PIC 9(5)    VALUE ZERO.
004500   03 �{�p���񋟗��v�q            PIC 9(5)    VALUE ZERO.
004510   03 ���v�v�q                      PIC 9(6)    VALUE ZERO.
004520   03 �ꕔ���S���v�q                PIC 9(6)    VALUE ZERO.
004530   03 �������z�v�q                  PIC 9(6)    VALUE ZERO.
004540   03 ���t�����v�q                  PIC 9(1)    VALUE ZERO.
004550   03 �󋋎ҕ��S�z�v�q              PIC 9(6)    VALUE ZERO.
004560   03 �����������z�v�q              PIC 9(6)    VALUE ZERO.
004570*
004580* �������ʖ��̗���
004590***********************
004600 01 �����Q�v�q.
004610   03 ���񏈒u�v�q    OCCURS   9.
004620      05 ���񏈒u���v�q             PIC 9(5)    VALUE ZERO.
004630*
004640* �������̗���
004650***********************
004660 01 �����R�v�q.
004670**********
004680* �P���� *
004690**********
004700   03 ���ʂP�v�q.
004710      05 ��ÂP�v�q.
004720         07 ��ÒP���P�v�q              PIC 9(4)    VALUE ZERO.
004730         07 ��É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
004740         07 ��×��P�v�q                PIC 9(5)    VALUE ZERO.
004750      05 ��㪖@�P�v�q.
004760         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
004770         07 ��㪖@���P�v�q              PIC 9(4)    VALUE ZERO.
004780      05 ��㪖@�P�v�q.
004790         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
004800         07 ��㪖@���P�v�q              PIC 9(4)    VALUE ZERO.
004810      05 �d�ÂP�v�q.
004820         07 �d�É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
004830         07 �d�×��P�v�q                PIC 9(4)    VALUE ZERO.
004840      05 ���v�P�v�q                     PIC 9(6)    VALUE ZERO.
004850      05 �����������P�v�q               PIC 9(3)    VALUE ZERO.
004860      05 ���������v�P�v�q               PIC 9(6)    VALUE ZERO.
004870**********
004880* �Q���� *
004890**********
004900   03 ���ʂQ�v�q.
004910      05 ��ÂQ�v�q.
004920         07 ��ÒP���Q�v�q              PIC 9(4)    VALUE ZERO.
004930         07 ��É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
004940         07 ��×��Q�v�q                PIC 9(5)    VALUE ZERO.
004950      05 ��㪖@�Q�v�q.
004960         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
004970         07 ��㪖@���Q�v�q              PIC 9(4)    VALUE ZERO.
004980      05 ��㪖@�Q�v�q.
004990         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
005000         07 ��㪖@���Q�v�q              PIC 9(4)    VALUE ZERO.
005010      05 �d�ÂQ�v�q.
005020         07 �d�É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
005030         07 �d�×��Q�v�q                PIC 9(4)    VALUE ZERO.
005040      05 ���v�Q�v�q                     PIC 9(6)    VALUE ZERO.
005050      05 �����������Q�v�q               PIC 9(3)    VALUE ZERO.
005060      05 ���������v�Q�v�q               PIC 9(6)    VALUE ZERO.
005070******************
005080* �R���ʁ^�W�� *
005090******************
005100   03 ���ʂR�W�v�q.
005110      05 ��ÂR�W�v�q.
005120         07 ��ÒP���R�W�v�q              PIC 9(4)  VALUE ZERO.
005130         07 ��É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
005140         07 ��×��R�W�v�q                PIC 9(5)  VALUE ZERO.
005150      05 ��㪖@�R�W�v�q.
005160         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
005170         07 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
005180      05 ��㪖@�R�W�v�q.
005190         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
005200         07 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
005210      05 �d�ÂR�W�v�q.
005220         07 �d�É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
005230         07 �d�×��R�W�v�q                PIC 9(4)  VALUE ZERO.
005240      05 ���v�R�W�v�q                     PIC 9(6)  VALUE ZERO.
005250      05 �����ʍ����v�R�W�v�q             PIC 9(6)  VALUE ZERO.
005260      05 �����������R�W�v�q               PIC 9(3)  VALUE ZERO.
005270      05 ���������v�R�W�v�q               PIC 9(6)  VALUE ZERO.
005280******************
005290* �R���ʁ^�P�O�� *
005300******************
005310   03 ���ʂR�O�v�q.
005320      05 �����J�n�����R�O�v�q.
005330         07 �����J�n���R�O�v�q            PIC 9(2)  VALUE ZERO.
005340         07 �����J�n���R�O�v�q            PIC 9(2)  VALUE ZERO.
005350      05 ��ÂR�O�v�q.
005360         07 ��ÒP���R�O�v�q              PIC 9(4)  VALUE ZERO.
005370         07 ��É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
005380         07 ��×��R�O�v�q                PIC 9(5)  VALUE ZERO.
005390      05 ��㪖@�R�O�v�q.
005400         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
005410         07 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
005420      05 ��㪖@�R�O�v�q.
005430         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
005440         07 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
005450      05 �d�ÂR�O�v�q.
005460         07 �d�É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
005470         07 �d�×��R�O�v�q                PIC 9(4)  VALUE ZERO.
005480      05 ���v�R�O�v�q                     PIC 9(6)  VALUE ZERO.
005490      05 �����������R�O�v�q               PIC 9(3)  VALUE ZERO.
005500      05 ���������v�R�O�v�q               PIC 9(6)  VALUE ZERO.
005510****************
005520* �S���ʁ^�T�� *
005530****************
005540   03 ���ʂS�T�v�q.
005550      05 ��ÂS�T�v�q.
005560         07 ��ÒP���S�T�v�q              PIC 9(4)  VALUE ZERO.
005570         07 ��É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
005580         07 ��×��S�T�v�q                PIC 9(5)  VALUE ZERO.
005590      05 ��㪖@�S�T�v�q.
005600         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
005610         07 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
005620      05 ��㪖@�S�T�v�q.
005630         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
005640         07 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
005650      05 �d�ÂS�T�v�q.
005660         07 �d�É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
005670         07 �d�×��S�T�v�q                PIC 9(4)  VALUE ZERO.
005680      05 ���v�S�T�v�q                     PIC 9(6)  VALUE ZERO.
005690      05 �����ʍ����v�S�T�v�q             PIC 9(6)  VALUE ZERO.
005700      05 �����������S�T�v�q               PIC 9(3)  VALUE ZERO.
005710      05 ���������v�S�T�v�q               PIC 9(6)  VALUE ZERO.
005720****************
005730* �S���ʁ^�W�� *
005740****************
005750   03 ���ʂS�W�v�q.
005760      05 �����J�n�����S�W�v�q.
005770         07 �����J�n���S�W�v�q            PIC 9(2)  VALUE ZERO.
005780         07 �����J�n���S�W�v�q            PIC 9(2)  VALUE ZERO.
005790      05 ��ÂS�W�v�q.
005800         07 ��ÒP���S�W�v�q              PIC 9(4)  VALUE ZERO.
005810         07 ��É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
005820         07 ��×��S�W�v�q                PIC 9(5)  VALUE ZERO.
005830      05 ��㪖@�S�W�v�q.
005840         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
005850         07 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
005860      05 ��㪖@�S�W�v�q.
005870         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
005880         07 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
005890      05 �d�ÂS�W�v�q.
005900         07 �d�É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
005910         07 �d�×��S�W�v�q                PIC 9(4)  VALUE ZERO.
005920      05 ���v�S�W�v�q                     PIC 9(6)  VALUE ZERO.
005930      05 �����ʍ����v�S�W�v�q             PIC 9(6)  VALUE ZERO.
005940      05 �����������S�W�v�q               PIC 9(3)  VALUE ZERO.
005950      05 ���������v�S�W�v�q               PIC 9(6)  VALUE ZERO.
005960******************
005970* �S���ʁ^�P�O�� *
005980******************
005990   03 ���ʂS�O�v�q.
006000      05 �����J�n�����S�O�v�q.
006010         07 �����J�n���S�O�v�q            PIC 9(2)  VALUE ZERO.
006020         07 �����J�n���S�O�v�q            PIC 9(2)  VALUE ZERO.
006030      05 ��ÂS�O�v�q.
006040         07 ��ÒP���S�O�v�q              PIC 9(4)  VALUE ZERO.
006050         07 ��É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
006060         07 ��×��S�O�v�q                PIC 9(5)  VALUE ZERO.
006070      05 ��㪖@�S�O�v�q.
006080         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
006090         07 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
006100      05 ��㪖@�S�O�v�q.
006110         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
006120         07 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
006130      05 �d�ÂS�O�v�q.
006140         07 �d�É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
006150         07 �d�×��S�O�v�q                PIC 9(4)  VALUE ZERO.
006160      05 ���v�S�O�v�q                     PIC 9(6)  VALUE ZERO.
006170      05 �����������S�O�v�q               PIC 9(3)  VALUE ZERO.
006180      05 ���������v�S�O�v�q               PIC 9(6)  VALUE ZERO.
006190********************
006200* �T���ʁ^�Q�D�T�� *
006210********************
006220   03 ���ʂT�Q�v�q.
006230      05 ��ÂT�Q�v�q.
006240         07 ��ÒP���T�Q�v�q              PIC 9(4)  VALUE ZERO.
006250         07 ��É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
006260         07 ��×��T�Q�v�q                PIC 9(5)  VALUE ZERO.
006270      05 ��㪖@�T�Q�v�q.
006280         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
006290         07 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
006300      05 ��㪖@�T�Q�v�q.
006310         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
006320         07 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
006330      05 �d�ÂT�Q�v�q.
006340         07 �d�É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
006350         07 �d�×��T�Q�v�q                PIC 9(4)  VALUE ZERO.
006360      05 ���v�T�Q�v�q                     PIC 9(6)  VALUE ZERO.
006370      05 �����ʍ����v�T�Q�v�q             PIC 9(6)  VALUE ZERO.
006380      05 �����������T�Q�v�q               PIC 9(3)  VALUE ZERO.
006390      05 ���������v�T�Q�v�q               PIC 9(6)  VALUE ZERO.
006400****************
006410* �T���ʁ^�T�� *
006420****************
006430   03 ���ʂT�T�v�q.
006440      05 �����J�n�����T�T�v�q.
006450         07 �����J�n���T�T�v�q            PIC 9(2)  VALUE ZERO.
006460         07 �����J�n���T�T�v�q            PIC 9(2)  VALUE ZERO.
006470      05 ��ÂT�T�v�q.
006480         07 ��ÒP���T�T�v�q              PIC 9(4)  VALUE ZERO.
006490         07 ��É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
006500         07 ��×��T�T�v�q                PIC 9(5)  VALUE ZERO.
006510      05 ��㪖@�T�T�v�q.
006520         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
006530         07 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
006540      05 ��㪖@�T�T�v�q.
006550         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
006560         07 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
006570      05 �d�ÂT�T�v�q.
006580         07 �d�É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
006590         07 �d�×��T�T�v�q                PIC 9(4)  VALUE ZERO.
006600      05 ���v�T�T�v�q                     PIC 9(6)  VALUE ZERO.
006610      05 �����ʍ����v�T�T�v�q             PIC 9(6)  VALUE ZERO.
006620      05 �����������T�T�v�q               PIC 9(3)  VALUE ZERO.
006630      05 ���������v�T�T�v�q               PIC 9(6)  VALUE ZERO.
006640****************
006650* �T���ʁ^�W�� *
006660****************
006670   03 ���ʂT�W�v�q.
006680      05 �����J�n�����T�W�v�q.
006690         07 �����J�n���T�W�v�q            PIC 9(2)  VALUE ZERO.
006700         07 �����J�n���T�W�v�q            PIC 9(2)  VALUE ZERO.
006710      05 ��ÂT�W�v�q.
006720         07 ��ÒP���T�W�v�q              PIC 9(4)  VALUE ZERO.
006730         07 ��É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
006740         07 ��×��T�W�v�q                PIC 9(5)  VALUE ZERO.
006750      05 ��㪖@�T�W�v�q.
006760         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
006770         07 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
006780      05 ��㪖@�T�W�v�q.
006790         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
006800         07 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
006810      05 �d�ÂT�W�v�q.
006820         07 �d�É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
006830         07 �d�×��T�W�v�q                PIC 9(4)  VALUE ZERO.
006840      05 ���v�T�W�v�q                     PIC 9(6)  VALUE ZERO.
006850      05 �����ʍ����v�T�W�v�q             PIC 9(6)  VALUE ZERO.
006860      05 �����������T�W�v�q               PIC 9(3)  VALUE ZERO.
006870      05 ���������v�T�W�v�q               PIC 9(6)  VALUE ZERO.
006880******************
006890* �T���ʁ^�P�O�� *
006900******************
006910   03 ���ʂT�O�v�q.
006920      05 �����J�n�����T�O�v�q.
006930         07 �����J�n���T�O�v�q            PIC 9(2)  VALUE ZERO.
006940         07 �����J�n���T�O�v�q            PIC 9(2)  VALUE ZERO.
006950      05 ��ÂT�O�v�q.
006960         07 ��ÒP���T�O�v�q              PIC 9(4)  VALUE ZERO.
006970         07 ��É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
006980         07 ��×��T�O�v�q                PIC 9(5)  VALUE ZERO.
006990      05 ��㪖@�T�O�v�q.
007000         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
007010         07 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
007020      05 ��㪖@�T�O�v�q.
007030         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
007040         07 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
007050      05 �d�ÂT�O�v�q.
007060         07 �d�É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
007070         07 �d�×��T�O�v�q                PIC 9(4)  VALUE ZERO.
007080      05 ���v�T�O�v�q                     PIC 9(6)  VALUE ZERO.
007090      05 �����������T�O�v�q               PIC 9(3)  VALUE ZERO.
007100      05 ���������v�T�O�v�q               PIC 9(6)  VALUE ZERO.
008000*******************
008010*  ���׏����s���Z */202206
008020*******************
008030   03 ���׏����s���Z���v�q                PIC ZZZ   VALUE ZERO.
008030   03 ���׏����s���Z���v�q                PIC ZZ    VALUE ZERO.
007110*
007120**************
007130* �{�p����� *
007140**************
007150 01 �{�p�����v.
007160    03 �_���t�ԍ��v                    PIC X(16)  VALUE SPACE.
007170*    03 �_���t�ԍ��P�v                  PIC X(6)   VALUE SPACE.
007180*    03 �_���t�ԍ��Q�v                  PIC N(4)   VALUE SPACE.
007190*    03 �_���t�ԍ��R�v                  PIC X(4)   VALUE SPACE.
007200    03 �ڍ��t�����ԍ��v              PIC X(16)  VALUE SPACE.
007210    03 ��\�҃J�i�v                    PIC X(50)  VALUE SPACE.
007220    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
007230    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
          03 �s���{���i�h�r�v                PIC X(2)   VALUE SPACE.
007240    03 �{�p���Z���v.
007250       05 �{�p���Z���P�v               PIC X(50)  VALUE SPACE.
007260       05 �{�p���Z���Q�v               PIC X(50)  VALUE SPACE.
007270    03 �{�p���X�֔ԍ��v.
007280       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
007290       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
007300    03 �{�p���d�b�ԍ��v                PIC X(20)  VALUE SPACE.
007310    03 ��z���󗝔ԍ��v                PIC X(15)  VALUE SPACE.
007320    03 �󗝔N�����v.
007330       05 �󗝔N�v                     PIC 9(2)   VALUE ZERO.
007340       05 �󗝌��v                     PIC 9(2)   VALUE ZERO.
007350       05 �󗝓��v                     PIC 9(2)   VALUE ZERO.
007360    03 �ŏI�ʉ@�N�����v.
007370       05 �ŏI�ʉ@�N�v                 PIC 9(2)   VALUE ZERO.
007380       05 �ŏI�ʉ@���v                 PIC 9(2)   VALUE ZERO.
007390       05 �ŏI�ʉ@���v                 PIC 9(2)   VALUE ZERO.
007400    03 �_���t�N�����v.
007410       05 �_���t�N�v                   PIC 9(2)   VALUE ZERO.
007420       05 �_���t���v                   PIC 9(2)   VALUE ZERO.
007430       05 �_���t���v                   PIC 9(2)   VALUE ZERO.
007440    03 ���҈ϔC�N�����v.
007450       05 ���҈ϔC�N�v                 PIC 9(2)   VALUE ZERO.
007460       05 ���҈ϔC���v                 PIC 9(2)   VALUE ZERO.
007470       05 ���҈ϔC���v                 PIC 9(2)   VALUE ZERO.
007480    03 �������v.
007490        05 ������s���v              PIC X(40)  VALUE SPACE.
007500        05 ������s�x�X���v          PIC X(40)  VALUE SPACE.
007510        05 �a����ʂv                  PIC 9(1)   VALUE ZERO.
007520        05 �����ԍ��v                  PIC X(10)  VALUE SPACE.
007530        05 �������`�l�v                PIC X(40)  VALUE SPACE.
007540        05 �������`�l�J�i�v            PIC X(40)  VALUE SPACE.
007550* �������Z
007560        05 �������`�l�ƃJ�i�v.
007570           07 �������`�l�ƃJ�i�P�v     PIC X(38)  VALUE SPACE.
007580           07 �������`�l�ƃJ�i�Q�v     PIC X(30)  VALUE SPACE.
007590*
007600        05 ��s���x�X���v              PIC X(60)  VALUE SPACE.
007610        05 �a����ʃR�����g�v          PIC N(3)   VALUE SPACE.
007620        05 �a����ʃR�����g�w�v        PIC X(4)   VALUE SPACE.
          03 �x���@��.
             05 ���Z�@�֖��v.
                07 ���Z�@�֖��P�v            PIC X(12)  VALUE SPACE.
                07 ���Z�@�֖��Q�v            PIC X(12)  VALUE SPACE.
      *          07 ���Z�@�֖��R�v            PIC X(8)  VALUE SPACE.
      *          07 ���Z�@�֖��S�v            PIC X(8)  VALUE SPACE.
      *          07 ���Z�@�֖��T�v            PIC X(8)  VALUE SPACE.
             05 �x�X���v.
                07 �x�X���P�v                PIC X(12) VALUE SPACE.
                07 �x�X���Q�v                PIC X(12) VALUE SPACE.
      *          07 �x�X���R�v                PIC X(12) VALUE SPACE.
      *          07 �x�X���S�v                PIC X(12) VALUE SPACE.
             05 �U���`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ���ʃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �����`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ��s�`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ���Ƀ`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �_���`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �{�X�`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �x�X�`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �{�x���`�F�b�N�v             PIC N(1)  VALUE SPACE.
007630    03 �R�����g�v.
007640        05 �R�����g�P�v                PIC X(54)  VALUE SPACE.
007650        05 �R�����g�Q�v                PIC X(54)  VALUE SPACE.
007660        05 �R�����g�R�v                PIC X(54)  VALUE SPACE.
007670        05 �R�����g�S�v                PIC X(54)  VALUE SPACE.
007680        05 �R�����g�T�v                PIC N(27)  VALUE SPACE.
007690    03 ���{�p�h�c�v                    PIC X(15)  VALUE SPACE.
007700    03 �s�����{�p�h�c�v                PIC X(15)  VALUE SPACE.
007710**************
007720* ��f�ҏ�� *
007730**************
007740 01 ��f�ҏ��v.
      */�����C��/20190408
          03 �{�p�a��v                      PIC 9(1)   VALUE ZERO.
007750    03 �{�p�N���v.
007760       05 �{�p�N�v                     PIC 9(2)   VALUE ZERO.
007770       05 �{�p���v                     PIC 9(2)   VALUE ZERO.
007780*    03 �L���v                          PIC N(12)  VALUE SPACE.
007570    03 �L���v.
007580       05 ����L���v                   PIC N(12)  VALUE SPACE.
          03 �L���ԍ��v.
             05 �L���ԍ��w�v                 PIC X(40) VALUE SPACE.
007790    03 �ԍ��v.
007800       05 ����ԍ��v                   PIC X(15)  VALUE SPACE.
007810       05 FILLER                       PIC X(15)  VALUE SPACE.
007820    03 �ی��Ҕԍ��v.
007830       05 ����ی��Ҕԍ��v             PIC X(8)   VALUE SPACE.
007840       05 FILLER                       PIC X(2)   VALUE SPACE.
007850    03 �s�����ԍ��v.
007860       05 ����s�����ԍ��v             PIC X(8)   VALUE SPACE.
007870       05 FILLER                       PIC X(2).
007880    03 �����於�̂v.
007890       05 ��������於�̂P�v           PIC X(40)  VALUE SPACE.
007900       05 ��������於�̂Q�v           PIC X(40)  VALUE SPACE.
007910    03 �󋋎Ҕԍ��v.
007920       05 ����󋋎Ҕԍ��v             PIC X(15)  VALUE SPACE.
007930*       05 FILLER                       PIC X(13).
007940**    03 �����s�����ԍ��v                PIC X(8)   VALUE SPACE.
007950    03 �ی���ʂv                      PIC 9(2)   VALUE ZERO.
007960    03 ��ی��ҏ��v.
007970       05 ��ی��҃J�i�v               PIC X(50)  VALUE SPACE.
007980       05 ��ی��Ҏ����v               PIC X(50)  VALUE SPACE.
007990       05 �X�֔ԍ��v.
008000          07 �X�֔ԍ��P�v              PIC X(3)   VALUE SPACE.
008010          07 �X�֔ԍ��Q�v              PIC X(4)   VALUE SPACE.
008020       05 ��ی��ҏZ���v               PIC X(80)  VALUE SPACE.
008030       05 ��ی��ҏZ���P�v             PIC X(50)  VALUE SPACE.
008040       05 ��ی��ҏZ���Q�v             PIC X(50)  VALUE SPACE.
008990       05 �d�b�ԍ��v                   PIC X(35)  VALUE SPACE.
008050    03 ���ҏ��v.
008060       05 ���ҏZ���v                   PIC X(80)  VALUE SPACE.
008070       05 ���ҏZ���P�v                 PIC X(50)  VALUE SPACE.
008080       05 ���ҏZ���Q�v                 PIC X(50)  VALUE SPACE.
008090       05 ���҃J�i�v                   PIC X(50)  VALUE SPACE.
008100       05 ���Ҏ����v                   PIC X(50)  VALUE SPACE.
008110       05 ���ʃ`�F�b�N�v.
008120          07 �j�`�F�b�N�v              PIC N(1)  VALUE SPACE.
008130          07 ���`�F�b�N�v              PIC N(1)  VALUE SPACE.
008140       05 ���Ґ��ʂv.
008150          07 ���ʂv                    PIC N(1)  VALUE SPACE.
008160       05 �a��`�F�b�N�v.
008170          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008180          07 �吳�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008190          07 ���a�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008200          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008210          07 �����v                    PIC N(2)  VALUE SPACE.
      */�����C��/������20190408
008210          07 �ߘa�`�F�b�N�v            PIC N(1)  VALUE SPACE.
                07 �ߘa�b�l�v                PIC X(4)  VALUE SPACE.
009110*          07 �����v                    PIC N(2)  VALUE SPACE.
      */�����C��/������20190408
008220       05 ���ҔN�v                     PIC 9(2)  VALUE ZERO.
008230       05 ���Ҍ��v                     PIC 9(2)  VALUE ZERO.
008240       05 ���ғ��v                     PIC 9(2)  VALUE ZERO.
008250       05 �����v.
008260          07 ��������v                PIC N(4)  VALUE SPACE.
008270          07 FILLER                    PIC X(4)  VALUE SPACE.
008280       05 ���������P�v                 PIC N(37) VALUE SPACE.
008290       05 ���������Q�v                 PIC N(37) VALUE SPACE.
008300       05 ���������R�v                 PIC N(37) VALUE SPACE.
008310       05 ���������S�v                 PIC N(37) VALUE SPACE.
008320       05 ���������T�v                 PIC N(37) VALUE SPACE.
008330*
008370*       05 ���������v                   PIC N(40) OCCURS 29 VALUE SPACE.
      */���p�Ή�/110421
             05 ���������v OCCURS 29.
                07 ���������w�v              PIC X(80)  VALUE SPACE.
008350*
008360    03 ������v                        PIC N(1)  VALUE SPACE.
008370*    03 ��ʃ`�F�b�N�v.
008380*       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008390*       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008400*       05 �D�`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008410*       05 �g�`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008420*       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008430*       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008440*       05 �ރ`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008440*       05 �㍂�`�F�b�N�v               PIC N(1)  VALUE SPACE.
008440*       05 �㍂�P�v                     PIC N(1)  VALUE SPACE.
008450    03 ���ʃ}�[�N�v                    PIC N(1)  VALUE SPACE.
008460    03 ���ʃR�����g�v                  PIC X(16) VALUE SPACE.
007390    03 �ی���ʃ`�F�b�N�v.
007400       05 �Еۃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
007410       05 �D���`�F�b�N�v               PIC N(1)  VALUE SPACE.
007420       05 �g���`�F�b�N�v               PIC N(1)  VALUE SPACE.
007430       05 ���ۃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ���σ`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
             05 �ސE�`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ����`�F�b�N�v               PIC N(1)  VALUE SPACE.
          03 �{�l�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
          03 �Ƒ��`�F�b�N�v                  PIC N(1)  VALUE SPACE.
          03 �P�ƃ`�F�b�N�v                  PIC N(1)  VALUE SPACE.
          03 �Q���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
          03 ����`�F�b�N�v                  PIC N(1)  VALUE SPACE.
          03 ���V�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
          03 �U�΃`�F�b�N�v                  PIC N(1)  VALUE SPACE.
          03 ���t�����`�F�b�N�v.
             05 �V���`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �W���`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �X���`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �P�O���`�F�b�N�v             PIC N(1)  VALUE SPACE.
008470*
008480****************
008490* �����f�[�^�e *
008500****************
008510 01 �������v.
008520    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
008530    03 ���ʏ��v  OCCURS   9.
008540       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
008550       05 ���ʃR�[�h�v.
008560          07 ������ʂv                PIC 9(2)  VALUE ZERO.
008570          07 ���ʂv                    PIC 9(2)  VALUE ZERO.
008580          07 ���E�敪�v                PIC 9(1)  VALUE ZERO.
008590          07 �����ʒu�ԍ��v            PIC 9(2)  VALUE ZERO.
008600       05 �������v                     PIC N(18) VALUE SPACE.
008610       05 �����N�����v.
008620          07 �����N�v                  PIC 9(2)  VALUE ZERO.
008630          07 �������v                  PIC 9(2)  VALUE ZERO.
008640          07 �������v                  PIC 9(2)  VALUE ZERO.
008650       05 �����N�����v.
008660          07 �����N�v                  PIC 9(2)  VALUE ZERO.
008670          07 �������v                  PIC 9(2)  VALUE ZERO.
008680          07 �������v                  PIC 9(2)  VALUE ZERO.
008690       05 �J�n�N�����v.
008700          07 �J�n�N�v                  PIC 9(2)  VALUE ZERO.
008710          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
008720          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
008730       05 �I���N�����v.
008740          07 �I���N�v                  PIC 9(2)  VALUE ZERO.
008750          07 �I�����v                  PIC 9(2)  VALUE ZERO.
008760          07 �I�����v                  PIC 9(2)  VALUE ZERO.
008770       05 �������v                     PIC 9(2)  VALUE ZERO.
             05 ���ʌp�������v               PIC 9(3)  VALUE ZERO.
008780       05 �]�A�敪�v                   PIC 9(1)  VALUE ZERO.
008790       05 �]�A�敪�`�F�b�N�v.
008800          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008810          07 ���~�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008820          07 �]��`�F�b�N�v            PIC N(1)  VALUE SPACE.
008830       05 �J�n�N�����擾�t���O         PIC X(3)  VALUE SPACE.
008840       05 ���ʋ�؂v                   PIC X(1)  VALUE SPACE.
008850       05 �o�ߗ��̂v.
008860          07 ����o�ߗ��̂v            PIC N(5)  VALUE SPACE.
008870          07 FILLER                    PIC X(2)  VALUE SPACE.
008880    03 �o�ߕ��ʂv                      PIC N(1)  VALUE SPACE.
009030    03 �o�߂b�l                        PIC N(4)  VALUE SPACE.
008890    03 �V�K�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008900    03 �p���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
          03 �{�p���v.
             05 �{�p���`�F�b�N�v   OCCURS 31 PIC N(1)  VALUE SPACE.
008910****************
008920* �n�b�q�R�[�h *
008930****************
008940* 01 �n�b�q�R�[�h�v.
008950*    03 �n�b�q����ԍ��v                PIC X(6)    VALUE ZERO.
008960*    03 �n�b�q�{�p�N���v                PIC 9(4)    VALUE ZERO.
008970*    03 �n�b�q�ی���ʂv                PIC 9(2)    VALUE ZERO.
008980*    03 �n�b�q�e�Ђh�c�v                PIC 9(2)    VALUE ZERO.
008990*    03 �n�b�q�e�c�A�Ԃv.
009000*       05 �e�c�A�Ԋ��Ҕԍ��v           PIC 9(6)    VALUE ZERO.
009010*       05 �e�c�A�Ԍ��ۂh�c�v           PIC 9(1)    VALUE ZERO.
009020*    03 �n�b�q�������z�v                PIC 9(6)    VALUE ZERO.
009030*    03 �n�b�q���S�����v                PIC 9(1)    VALUE ZERO.
009040*    03 �n�b�q�ڍ��t�����ԍ��v        PIC 9(10)   VALUE ZERO.
009050*
009060********************
009070* ���t�����`�F�b�N *
009080********************
009090* 01 ���t�������v.
009100*    03 ���t�V���`�F�b�N�v                PIC N(1)  VALUE SPACE.
009110*    03 ���t�W���`�F�b�N�v                PIC N(1)  VALUE SPACE.
009120*    03 ���t�X���`�F�b�N�v                PIC N(1)  VALUE SPACE.
009130*    03 �V�l���t�`�F�b�N�v                PIC N(1)  VALUE SPACE.
009130*    03 �㍂���t�`�F�b�N�v                PIC N(1)  VALUE SPACE.
009130*    03 �㍂�Q�v                          PIC N(1)  VALUE SPACE.
009140************
009150* ������� *
009160************
009170 01 �������v.
009180    03 �������Z�v.
009190       05 ���ԊO�`�F�b�N�v                PIC N(1) VALUE SPACE.
009200       05 �x���`�F�b�N�v                  PIC N(1) VALUE SPACE.
009210       05 �[��`�F�b�N�v                  PIC N(1) VALUE SPACE.
009220    03 ���É��Z�v.
009230       05 ��ԃ`�F�b�N�v                  PIC N(1) VALUE SPACE.
009240       05 �\���J��`�F�b�N�v              PIC N(1) VALUE SPACE.
009250    03 �������q�`�F�b�N�v.
009260       05 ��`�F�b�N�v                    PIC N(1) VALUE SPACE.
009270       05 ���`�F�b�N�v                    PIC N(1) VALUE SPACE.
009280       05 ���`�F�b�N�v                    PIC N(1) VALUE SPACE.
009290    03 ���v�v                             PIC 9(7) VALUE ZERO.
009300    03 ���񏈒u�����v�v                   PIC 9(6) VALUE ZERO.
009310    03 ���񏈒u���`�F�b�N�v.
009320       05 �������`�F�b�N�v                PIC N(1) VALUE SPACE.
009330       05 �Œ藿�`�F�b�N�v                PIC N(1) VALUE SPACE.
009340       05 �{�×��`�F�b�N�v                PIC N(1) VALUE SPACE.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
          03 �����񐔂v                         PIC 9(2)  VALUE ZERO.
          03 �^���񐔂v                         PIC 9(2)  VALUE ZERO.
          03 �^�����v                           PIC 9(4)  VALUE ZERO.
009350************
009360* ���l��� * 
009370************
009380 01 ���l���v.
009390    03 �󋋎ҕ��S�z����v      OCCURS  10.
009400       05 ������S�񐔂v               PIC N(1)  VALUE SPACE.
009410       05 ���������S�z�v               PIC 9(4)  VALUE ZERO.
009420    03 �K�p�P�v                        PIC N(34) VALUE SPACE.
009430    03 �K�p�Q�v                        PIC N(34) VALUE SPACE.
008830    03 �K�p�R�v                        PIC X(40) VALUE SPACE.
009440    03 �o�߃R�����g�v                  PIC N(60) VALUE SPACE.
009450*
009460*****************
009470* ���Z�v�g���я� *
009480*****************
009490 01 ���ԌŒ�v                         PIC N(1) VALUE SPACE.
009500 01 ���Ԃv                             PIC 9(4) VALUE ZERO.
      *
       01 �E�v�{�p���v                       PIC X(100) VALUE SPACE.
       01 �{�p���v.
          03 �{�p���Q�v                      PIC X(1)  VALUE SPACE.
          03 �{�p���P�v                      PIC X(1)  VALUE SPACE.
002790** ���S�����p
002800 01 ���S�����v                         PIC 9(2)  VALUE ZERO.
002810 01 ���t�����v                         PIC 9(2)  VALUE ZERO.
009510*******************************************************************
009520 01 �������.
009530     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
009540     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
009550     03 ������ʂo                     PIC X(2) VALUE SPACE.
009560     03 �g������o.
009570         05 �[������o.
009580             07 �ړ������o             PIC X(1) VALUE SPACE.
009590             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
009600         05 �ڍא���o                 PIC X(2) VALUE SPACE.
009610     03 �ʒm���o                     PIC X(2) VALUE SPACE.
009620     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
009630*
009640 01 �v�Z�@����N�v                     PIC 9(2) VALUE ZERO.
009650* ���t�v�n�q�j
009660 01 �a��I���N�v                       PIC 9(4) VALUE ZERO.
009670 01 �v�Z�@����.
009680    03 �v�Z�@����N                    PIC 9(4) VALUE ZERO.
009690    03 �v�Z�@�����                  PIC 9(4) VALUE ZERO.
009700 01 �v�Z�@����q REDEFINES �v�Z�@����.
009710    03 �v�Z�@���I                      PIC 9(2).
009720    03 �v�Z�@���t                      PIC 9(6).
009730    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
009740       05 �v�Z�@�N��                   PIC 9(4).
009750       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
009760         07 �v�Z�@�N                   PIC 9(2).
009770         07 �v�Z�@��                   PIC 9(2).
009780       05 �v�Z�@��                     PIC 9(2).
009790*
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
009800******************************************************************
009810*                          �A������                              *
009820******************************************************************
009830**  ��ʓ��̓f�[�^
009840 01 �A���|���̓f�[�^�ϔC��� IS EXTERNAL.
009850    03 �A���|�ϔC���                     PIC 9.
014620*
       01 �A���|���̓f�[�^�d�b��� IS EXTERNAL.
          03 �A���|�d�b���                     PIC 9.
009190*
       01 �A���|�v���r���[ IS EXTERNAL.
          03 �A���|�v���r���[�敪          PIC 9.
009860*
009870** �R�J����������
009880 01 �A���ԁ|�L�[ IS EXTERNAL.
009890    03 �A���ԁ|�{�p�N��.
009900       05 �A���ԁ|�{�p�a��               PIC 9.
009910       05 �A���ԁ|�{�p�N                 PIC 9(2).
009920       05 �A���ԁ|�{�p��                 PIC 9(2).
009930    03  �A���ԁ|���҃R�[�h.
009940       05 �A���ԁ|���Ҕԍ�               PIC 9(6).
009950       05 �A���ԁ|�}��                   PIC X.
009960    03 �A���ԁ|�Ώۃt���O                PIC X(3).
009970    03 �A���ԁ|���Ԍ��v.
009980       05 �A���ԁ|���Ԃv                 PIC 9(2) OCCURS 9.
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
009990************
010000* ����L�[ *
010010************
010040 01 �A����|�Ώۃf�[�^ IS EXTERNAL.
010050    03 �A����|�{�p�N����.
010060       05 �A����|�{�p�a��                  PIC 9(1).
010070       05 �A����|�{�p�N                    PIC 9(2).
010080       05 �A����|�{�p��                    PIC 9(2).
010090    03 �A����|���҃R�[�h.
010100       05 �A����|���Ҕԍ�                  PIC 9(6).
010110       05 �A����|�}��                      PIC X(1).
010120    03 �A����|�ی����                     PIC 9(2).
010130    03 �A����|�ی��Ҕԍ�                   PIC X(10).
010140    03 �A����|������                     PIC 9(2).
010150    03 �A����|��p���S�Ҕԍ�               PIC X(10).
010160    03 �A����|�������                     PIC 9(2).
010170    03 �A����|��p���S�Ҕԍ�����           PIC X(10).
010180    03 �A����|���҃J�i                     PIC X(20).
010190    03 �A����|�{�l�Ƒ��敪                 PIC 9(1).
010200*
013630 01 �A���|�L�[ IS EXTERNAL.
013640    03 �A���|�ی����                  PIC 9(2).
013650*
013660************************
013670* �������R���Z�b�g     *
013680************************
013690 01 �A�����|�L�[ IS EXTERNAL.
013700    03 �A�����|�{�p�N��.
013710       05 �A�����|�{�p�a��               PIC 9.
013720       05 �A�����|�{�p�N                 PIC 9(2).
013730       05 �A�����|�{�p��                 PIC 9(2).
013740    03  �A�����|���҃R�[�h.
013750       05 �A�����|���Ҕԍ�               PIC 9(6).
013760       05 �A�����|�}��                   PIC X.
013770    03 �A�����|������                    PIC 9(2).
013780    03 �A�����|���R��                    PIC N(63) OCCURS 15.
013790*
013792*************
013793* ��������
013794*************
013795 01 �A�������́|�L�[ IS EXTERNAL.
013796    03 �A�������́|�������             PIC 9(2).
013797    03 �A�������́|��p���S�Ҕԍ�����   PIC X(10).
013798*   / OUT /
013799    03 �A�������́|���̏W�c.
013800       05 �A�������́|�P����            PIC N.
013801       05 �A�������́|����              PIC N(4).
013802       05 �A�������́|��������          PIC N(10).
013803*
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
      * C �A�g�p
       01  �����P�v        PIC X(4096).
       01  �����Q�v        PIC X(512).
       01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
      *
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
       01 �A���^�Q�|�L�[ IS EXTERNAL.
           03 �A���^�Q�|�������q�b�l.
               05 �A���^�Q�|�������q�b�l�P        PIC X(130).
               05 �A���^�Q�|�������q�b�l�Q        PIC X(170).
      * 
013804*
013805******************************************************************
013810*                      PROCEDURE  DIVISION                       *
013820******************************************************************
013830 PROCEDURE               DIVISION.
013840************
013850*           *
013860* ��������   *
013870*           *
013880************
002570     PERFORM �v�����^�t�@�C���쐬.
013890     PERFORM ������.
013900************
013910*           *
013920* �又��     *
013930*           *
013940************
013950* ���
013960     PERFORM �A�����ڑҔ�.
013970     PERFORM ����Z�b�g.
013980     PERFORM �������.
013990************
014000*           *
014010* �I������   *
014020*           *
014030************
014040     PERFORM ��f�҈���敪�X�V.
014050     PERFORM �I������.
014060     MOVE ZERO  TO PROGRAM-STATUS.
014070     EXIT PROGRAM.
014080*
014090*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCH6427"             TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪  TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014100*================================================================*
014110 ������ SECTION.
014120*
014130     PERFORM �t�@�C���I�[�v��.
014140*    /* ���ݓ��t�擾 */
014150     ACCEPT �v�Z�@���t FROM DATE.
014160*    /* 1980�`2079�N�̊ԂŐݒ� */
014170     IF ( �v�Z�@�N > 80 )
014180         MOVE 19 TO �v�Z�@���I
014190     ELSE
014200         MOVE 20 TO �v�Z�@���I
014210     END-IF.
014220     PERFORM �J�����g�����擾.
014230     PERFORM �a��I���N�擾.
014240     COMPUTE �v�Z�@����N�v = �v�Z�@����N - �a��I���N�v.
014250*================================================================*
014260 �J�����g�����擾 SECTION.
014270*
014280     MOVE ZEROS TO ���|����敪.
014290     READ ������}�X�^
014300     NOT INVALID KEY
014310         MOVE ���|�J�����g����         TO �J�����g�����v
014320         MOVE ���|���Z������������敪 TO ������������敪�v
014330         MOVE ���|���Z�������R����敪 TO �������R����敪�v
014340         MOVE ���|���Z�v�g���t�敪     TO ���Z�v�g���t�敪�v
014350         MOVE ���|���Z�v�g���ғ��t�敪 TO ���Z�v�g���ғ��t�敪�v
014351         MOVE ���|�S�_�e�o�c�敪       TO �S�_�e�o�c�敪�v
015320         MOVE ���|�������Z             TO �p����ʂv
014360     END-READ.
014370*
014380*================================================================*
014390 �a��I���N�擾 SECTION.
014400*
014410*     DISPLAY NC"�J�����g�����v"  �J�����g�����v UPON MSGBOX.
014420     MOVE �J�����g�����v TO ���|�����敪.
014430     READ �����}�X�^
014440     INVALID KEY
014450         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
014460         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
014470                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014480         ACCEPT  �L�[���� FROM CONS
014490         PERFORM �I������
014500         EXIT PROGRAM
014510     NOT INVALID KEY
014520         COMPUTE �O�a��v = �J�����g�����v - 1
014530         MOVE �O�a��v TO ���|�����敪
014540         READ �����}�X�^
014550         INVALID KEY
014560             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
014570             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
014580                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014590             ACCEPT  �L�[���� FROM CONS
014600             PERFORM �I������
014610             EXIT PROGRAM
014620         NOT INVALID KEY
014630             MOVE ���|�I������N TO �a��I���N�v
014640         END-READ
014650     END-READ.
014660*
014670*================================================================*
014680 �t�@�C���I�[�v�� SECTION.
014690*
014700     OPEN INPUT   �ی��҃}�X�^
014710         MOVE NC"�ی���" TO �t�@�C����.
014720         PERFORM �I�[�v���`�F�b�N.
014730     OPEN INPUT   �s�����}�X�^
014740         MOVE NC"�s����" TO �t�@�C����.
014750         PERFORM �I�[�v���`�F�b�N.
014760     OPEN INPUT   �����}�X�^
014770         MOVE NC"����" TO �t�@�C����.
014780         PERFORM �I�[�v���`�F�b�N.
014790     OPEN INPUT   ���̃}�X�^
014800         MOVE NC"����" TO �t�@�C����.
014810         PERFORM �I�[�v���`�F�b�N.
007560     OPEN INPUT   ���Z�v�g�e
007570         MOVE NC"���Z" TO �t�@�C����.
007580         PERFORM �I�[�v���`�F�b�N.
014850     OPEN INPUT   ������}�X�^
014860         MOVE NC"������" TO �t�@�C����.
014870         PERFORM �I�[�v���`�F�b�N.
014880     OPEN INPUT   �{�p�����}�X�^
014890         MOVE NC"�{��" TO �t�@�C����.
014900         PERFORM �I�[�v���`�F�b�N.
014910     OPEN INPUT   ������}�X�^
014920         MOVE NC"����" TO �t�@�C����.
014930         PERFORM �I�[�v���`�F�b�N.
014940     OPEN INPUT   �o�߃}�X�^
014950         MOVE NC"�o��" TO �t�@�C����.
014960         PERFORM �I�[�v���`�F�b�N.
014970     OPEN INPUT   �{�p�L�^�e.
014980         MOVE NC"�{�L�e" TO �t�@�C����.
014990         PERFORM �I�[�v���`�F�b�N.
015000     OPEN INPUT   �����f�[�^�e.
015010         MOVE NC"����" TO �t�@�C����.
015020         PERFORM �I�[�v���`�F�b�N.
015030     OPEN INPUT   ���������e.
015040         MOVE NC"��������" TO �t�@�C����.
015050         PERFORM �I�[�v���`�F�b�N.
015060     OPEN INPUT   ����}�X�^.
015070         MOVE NC"����}�X�^" TO �t�@�C����.
015080         PERFORM �I�[�v���`�F�b�N.
015090     OPEN INPUT   �h�c�Ǘ��}�X�^
015100         MOVE NC"�h�c" TO �t�@�C����.
015110         PERFORM �I�[�v���`�F�b�N.
015030     OPEN INPUT �����t�@�C��.
015040         MOVE NC"����" TO �t�@�C����.
015050         PERFORM �I�[�v���`�F�b�N.
015560     OPEN INPUT   ��f�ҏ��Q�e.
015570         MOVE NC"��f�ҏ��Q�e" TO �t�@�C����.
015580         PERFORM �I�[�v���`�F�b�N.
015120     OPEN INPUT   ��ƃt�@�C���S.
015170         IF ( ��ԃL�[  NOT =  "00" )
015060            OPEN OUTPUT  ��ƃt�@�C���S
                  CLOSE ��ƃt�@�C���S
015060            OPEN INPUT  ��ƃt�@�C���S
               END-IF.
015150     OPEN I-O   ��f�ҏ��e.
015160         MOVE NC"���" TO �t�@�C����.
015170         PERFORM �I�[�v���`�F�b�N.
015180     OPEN I-O   ����t�@�C��
015190         PERFORM �G���[�����o.
015200*================================================================*
015210 �I�[�v���`�F�b�N SECTION.
015220*
015230     IF ( ��ԃL�[  NOT =  "00" )
015240         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
015250         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
015260         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015270                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015280         ACCEPT  �L�[���� FROM CONS
015290         PERFORM �t�@�C����
015300         EXIT PROGRAM.
015310*================================================================*
015320 �A�����ڑҔ� SECTION.
015330*
015340     MOVE �A����|�{�p�a��           TO �{�p�a��v�q.
015350     MOVE �A����|�{�p�N             TO �{�p�N�v�q.
015360     MOVE �A����|�{�p��             TO �{�p���v�q.
015370     MOVE �A����|�ی����           TO �ی���ʂv�q.
015380     MOVE �A����|�ی��Ҕԍ�         TO �ی��Ҕԍ��v�q.
015390     MOVE �A����|������           TO �����ʂv�q.
015400     MOVE �A����|��p���S�Ҕԍ�     TO ��p���S�Ҕԍ��v�q.
015410     MOVE �A����|�������           TO ������ʂv�q.
015420     MOVE �A����|��p���S�Ҕԍ����� TO ��p���S�Ҕԍ������v�q.
015430     MOVE �A����|�{�l�Ƒ��敪       TO �{�l�Ƒ��敪�v�q.
015440     MOVE �A����|���҃J�i           TO ���҃J�i�v�q.
015450     MOVE �A����|���Ҕԍ�           TO ���Ҕԍ��v�q.
015460     MOVE �A����|�}��               TO �}�Ԃv�q.
015470*================================================================*
015480 ����Z�b�g SECTION.
015490*
015500     PERFORM ���ڏ�����.
014800     PERFORM �����Ǎ�.
015550     PERFORM �������擾.
015510     PERFORM �{�p�����擾.
015520     PERFORM ��������擾.
015530     PERFORM ��f�ҏ��擾.
015540     PERFORM �����f�[�^�擾.
015560     PERFORM �{�p�L�^�擾.
015570     PERFORM ���Z�v�g���я��擾.
015580*     PERFORM �n�b�q���擾.
015590*     PERFORM ��������擾.
015600*     PERFORM �������ȑO�̃f�[�^����.
015610     PERFORM �������Z�����擾.
015620     PERFORM ������擾.
015630*     PERFORM ���t�����`�F�b�N�擾.
015640     PERFORM �ϔC�N�����擾.
           PERFORM �{�p���擾.
015650*
      */��t���ЂƂ�e��Ô������/201001
           IF (��|������� = "52" ) AND (��|��p���S�Ҕԍ�����(1:4) = "8512" )
               MOVE "��t���ЂƂ�e�ƒ듙��Ô��������" TO �^�C�g��
           END-IF.
      */��t���q�ǂ���Ô������/120525
           IF (��|������� = "60" ) AND (��|��p���S�Ҕԍ�����(1:4) = "8312" )
               MOVE "��t���q�ǂ���Ô������" TO �^�C�g��
           END-IF.
      */��t���d�x�S�g��Q��Ô������
           IF (��|������� = "53" ) AND (��|��p���S�Ҕԍ�����(1:4) = "8112" )
               MOVE "��t���d�x�S�g��Q�ҁi���j��Ô��"         TO �^�C�g��
               MOVE NC"�d�S"         TO �d�S
               MOVE NC"��"           TO �d�S��
           END-IF.
016791*-----------------------------------------------*
016800     IF ( ������������敪�v  NOT = 1 ) AND ( ���Z������������敪�v NOT = 1 )
016813        IF ( ������������敪�v = 3 OR 4 )
016815           PERFORM ������������Ώ۔��菈��
016817        ELSE
016820           PERFORM ���������擾
016821        END-IF
016830     END-IF.
015780*
015790**********************
015800* �n�b�q�R�[�h�Z�b�g *
015810**********************
015820*
015830*     MOVE �n�b�q�ڍ��t�����ԍ��v     TO �n�b�q����ԍ�.
015840*     MOVE �n�b�q�{�p�N���v     TO �n�b�q�{�p�N��.
015850*     MOVE �n�b�q�ی���ʂv     TO �n�b�q�ی����.
015860*     MOVE �n�b�q�e�Ђh�c�v     TO �n�b�q�e�Ђh�c.
015870*     MOVE �n�b�q�e�c�A�Ԃv     TO �n�b�q�e�c�A��.
015880*     MOVE �n�b�q�������z�v     TO �n�b�q�������z.
015890*     MOVE �n�b�q���S�����v     TO �n�b�q���S����.
015900*
015910**********************
015920* ���t�����`�F�b�N   *
015930**********************
015940*     MOVE ���t�V���`�F�b�N�v     TO  ���t�V���`�F�b�N.
015950*     MOVE ���t�W���`�F�b�N�v     TO  ���t�W���`�F�b�N.
015960*     MOVE ���t�X���`�F�b�N�v     TO  ���t�X���`�F�b�N.
015970*     MOVE �V�l���t�`�F�b�N�v     TO  �V�l���t�`�F�b�N.
015970*     MOVE �㍂���t�`�F�b�N�v     TO  �㍂���t�`�F�b�N.
      *     MOVE �㍂�Q�v               TO  �㍂�Q.
015980**********************
015990* �e�ی��`�F�b�N   *
016000**********************
016010*     MOVE ���`�F�b�N�v        TO ���{�`�F�b�N.
016020*     MOVE �g�`�F�b�N�v        TO �g���`�F�b�N.
016030*     MOVE ���`�F�b�N�v        TO ���ك`�F�b�N.
016040*     MOVE �D�`�F�b�N�v        TO �D���`�F�b�N.
016050*     MOVE ���`�F�b�N�v        TO ���σ`�F�b�N.
016060*     MOVE ���`�F�b�N�v        TO ���ۃ`�F�b�N.
016070*     MOVE �ރ`�F�b�N�v        TO �ސE�`�F�b�N.
016070*     MOVE �㍂�`�F�b�N�v      TO �㍂�`�F�b�N.
      *     MOVE �㍂�P�v            TO �㍂�P.
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
      */�����C��/������20190408
037370     IF (�{�p�a��v > 4) OR (�p����ʂv > 1)
              MOVE �{�p�a��v         TO ���|�����敪
037380        READ �����}�X�^
037390        NOT INVALID KEY
037400            MOVE ���|��������   TO �{�p�a��
037410        END-READ
      *        MOVE "===="             TO �{�p�a�����
           END-IF.
      */�����C��/������20190408
016080********************
016090* ��f�ҏ��Z�b�g *
016100********************
016110     MOVE �{�p�N�v            TO �{�p�N.
016120     MOVE �{�p���v            TO �{�p��.
016130*
016140*
016150*     IF ( �L���v(1:1) = NC"��" )
016160*        MOVE  SPACE    TO  �L��
016170*     ELSE
016180*        MOVE �L���v    TO  �L��
016190*     END-IF.
016200*     IF ( ����ԍ��v(1:1) = "*"  ) OR
016210*        ( ����ԍ��v(1:2) = "��" )
016220*        MOVE  SPACE      TO  �ԍ�
016230*     ELSE
016240*        MOVE ����ԍ��v  TO  �ԍ�
016250*     END-IF.
016260*
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
016270**
016322     IF �s�����ԍ��v(1:2) = "99"
016323         MOVE SPACE        TO ����S�Ҕԍ�
016324     ELSE
016328         MOVE �s�����ԍ��v TO ����S�Ҕԍ�
016330     END-IF.
016334**
016335*
016340     IF ( ����󋋎Ҕԍ��v(1:1) = "*"  ) OR
016350        ( ����󋋎Ҕԍ��v(1:2) = "��" )
016360        MOVE  SPACE              TO �󋋎Ҕԍ�
016370     ELSE
016380        MOVE �󋋎Ҕԍ��v        TO �󋋎Ҕԍ�
016390     END-IF.
016400**
016410     MOVE ����ی��Ҕԍ��v    TO �ی��Ҕԍ�.
016420**
016430*     MOVE ��������於�̂P�v  TO �ی��Җ���.
016440*     MOVE ��������於�̂Q�v  TO �ی��Җ��̂Q.
016450*
016460*     MOVE �����於�̂v        TO �ی��Җ���.
016470*     MOVE ��ی��҃J�i�v      TO ��ی��҃J�i.
016480     MOVE ��ی��Ҏ����v      TO ��ی��Ҏ���.
016490*     MOVE �X�֔ԍ��P�v        TO �X�֔ԍ��P.
016500*     MOVE �X�֔ԍ��Q�v        TO �X�֔ԍ��Q.
016510*     MOVE "-"                 TO �X�֋��.
016520     MOVE ��ی��ҏZ���P�v    TO �Z���P.
016530     MOVE ��ی��ҏZ���Q�v    TO �Z���Q.
      */���m���̏����͎�f�Ҏ����Z�����L�ڂ���/110519
           IF �s�����ԍ��v(3:2) = "23"
               MOVE ���Ҏ����v      TO ��ی��Ҏ���
               MOVE ���ҏZ���P�v    TO �Z���P
               MOVE ���ҏZ���Q�v    TO �Z���Q
           END-IF.
      */ �X�֔ԍ��E�d�b�ԍ��ǉ� /42505
           IF (�{�p�a��N���v�q >= 42505) AND (�A���|�d�b��� = 1)
              IF (��|�_���X�֓d�b�ԍ���� = 0 OR 2) AND
                 ((�X�֔ԍ��P�v NOT = SPACE) OR (�X�֔ԍ��Q�v NOT = SPACE))
017280           MOVE "��"          TO �X��
017260           MOVE �X�֔ԍ��P�v  TO �X�֔ԍ��P
017270           MOVE �X�֔ԍ��Q�v  TO �X�֔ԍ��Q
017280           MOVE "-"           TO �X�֋��
              END-IF
              IF ��|�_���X�֓d�b�ԍ���� = 0 OR 3
017260           MOVE �d�b�ԍ��v    TO �d�b�ԍ�
              END-IF
           END-IF.
016540**     MOVE ��ی��ҏZ���v      TO �Z��.
016550*     MOVE ���ҏZ���v          TO �Z��.
016550*     MOVE ���ҏZ���P�v          TO �Z���P.
016550*     MOVE ���ҏZ���Q�v          TO �Z���Q.
016560*     MOVE ���҃J�i�v          TO ���҃J�i.
016570     MOVE ���Ҏ����v          TO ���Ҏ���.
016580     MOVE �j�`�F�b�N�v        TO �j�`�F�b�N.
016590     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
016600*     MOVE ���ʂv               TO ����.
016610     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
016620     MOVE �吳�`�F�b�N�v      TO �吳�`�F�b�N.
016630     MOVE ���a�`�F�b�N�v      TO ���a�`�F�b�N.
016640     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
016650*     MOVE �����v              TO ����.
023070     MOVE �ߘa�`�F�b�N�v     TO �ߘa�`�F�b�N.
017390*     MOVE �����v              TO ���Ҙa��.
016660     MOVE ���ҔN�v            TO ���ҔN.
016670     MOVE ���Ҍ��v            TO ���Ҍ�.
016680     MOVE ���ғ��v            TO ���ғ�.
016690*     MOVE ��������v          TO ����.
      *
           IF ��Q�|������ی��Ҏ��� NOT = SPACE
016940        MOVE ��Q�|������ی��Ҏ��� TO ��ی��Ҏ���
           END-IF.
017170* 
016960     MOVE "�E�Ɩ��ЊQ�A�ʋ΍ЊQ���͑�O�ҍs�׈ȊO�̌����ɂ��B" TO ��������.
016700     MOVE ���������v(1)       TO ���������P.
016710     MOVE ���������v(2)       TO ���������Q.
016720     MOVE ���������v(3)       TO ���������R.
016730     MOVE ���������v(4)       TO ���������S.
016740     MOVE ���������v(5)       TO ���������T.
016480     MOVE ���������v(6)       TO ���������U.
016750*
016760     MOVE ������v            TO ������.
016770*
016780********************
016790* �������R���Z�b�g *
016800********************
016810*     MOVE �A�����|���R��(1)    TO �������R���P.
016820*     MOVE �A�����|���R��(2)    TO �������R���Q.
016830*     MOVE �A�����|���R��(3)    TO �������R���R.
016840*     MOVE �A�����|���R��(4)    TO �������R���S.
016850*     MOVE �A�����|���R��(5)    TO �������R���T.
016860*     MOVE �A�����|���R��(6)    TO �������R���U.
016870*     MOVE �A�����|���R��(7)    TO �������R���V.
016880*     IF ( �A�����|���R��(1)  NOT = SPACE )
016890*          MOVE NC"�i�������R�j"  TO �������R�Œ�
016900*     END-IF.
016910*
016920********************
016930* �����f�[�^�Z�b�g *
016940********************
016950* �P���� *
016960**********
016970     MOVE �������v(1)       TO �������P.
016980     MOVE �����N�v(1)       TO �����N�P.
016990     MOVE �������v(1)       TO �������P.
017000     MOVE �������v(1)       TO �������P.
017010     MOVE �����N�v(1)       TO �����N�P.
017020     MOVE �������v(1)       TO �������P.
017030     MOVE �������v(1)       TO �������P.
017040     MOVE �J�n�N�v(1)       TO �J�n�N�P.
017050     MOVE �J�n���v(1)       TO �J�n���P.
017060     MOVE �J�n���v(1)       TO �J�n���P.
017070     MOVE �I���N�v(1)       TO �I���N�P.
017080     MOVE �I�����v(1)       TO �I�����P.
017090     MOVE �I�����v(1)       TO �I�����P.
017100     MOVE �������v(1)       TO �������P.
017110     MOVE �����`�F�b�N�v(1) TO �����`�F�b�N�P.
017120     MOVE ���~�`�F�b�N�v(1) TO ���~�`�F�b�N�P.
017130     MOVE �]��`�F�b�N�v(1) TO �]��`�F�b�N�P.
017140**********
017150* �Q���� *
017160**********
017170     MOVE �������v(2)       TO �������Q.
017180     MOVE �����N�v(2)       TO �����N�Q.
017190     MOVE �������v(2)       TO �������Q.
017200     MOVE �������v(2)       TO �������Q.
017210     MOVE �����N�v(2)       TO �����N�Q.
017220     MOVE �������v(2)       TO �������Q.
017230     MOVE �������v(2)       TO �������Q.
017240     MOVE �J�n�N�v(2)       TO �J�n�N�Q.
017250     MOVE �J�n���v(2)       TO �J�n���Q.
017260     MOVE �J�n���v(2)       TO �J�n���Q.
017270     MOVE �I���N�v(2)       TO �I���N�Q.
017280     MOVE �I�����v(2)       TO �I�����Q.
017290     MOVE �I�����v(2)       TO �I�����Q.
017300     MOVE �������v(2)       TO �������Q.
017310     MOVE �����`�F�b�N�v(2) TO �����`�F�b�N�Q.
017320     MOVE ���~�`�F�b�N�v(2) TO ���~�`�F�b�N�Q.
017330     MOVE �]��`�F�b�N�v(2) TO �]��`�F�b�N�Q.
017340**********
017350* �R���� *
017360**********
017370     MOVE �������v(3)       TO �������R.
017380     MOVE �����N�v(3)       TO �����N�R.
017390     MOVE �������v(3)       TO �������R.
017400     MOVE �������v(3)       TO �������R.
017410     MOVE �����N�v(3)       TO �����N�R.
017420     MOVE �������v(3)       TO �������R.
017430     MOVE �������v(3)       TO �������R.
017440     MOVE �J�n�N�v(3)       TO �J�n�N�R.
017450     MOVE �J�n���v(3)       TO �J�n���R.
017460     MOVE �J�n���v(3)       TO �J�n���R.
017470     MOVE �I���N�v(3)       TO �I���N�R.
017480     MOVE �I�����v(3)       TO �I�����R.
017490     MOVE �I�����v(3)       TO �I�����R.
017500     MOVE �������v(3)       TO �������R.
017510     MOVE �����`�F�b�N�v(3) TO �����`�F�b�N�R.
017520     MOVE ���~�`�F�b�N�v(3) TO ���~�`�F�b�N�R.
017530     MOVE �]��`�F�b�N�v(3) TO �]��`�F�b�N�R.
017540**********
017550* �S���� *
017560**********
017570     MOVE �������v(4)       TO �������S.
017580     MOVE �����N�v(4)       TO �����N�S.
017590     MOVE �������v(4)       TO �������S.
017600     MOVE �������v(4)       TO �������S.
017610     MOVE �����N�v(4)       TO �����N�S.
017620     MOVE �������v(4)       TO �������S.
017630     MOVE �������v(4)       TO �������S.
017640     MOVE �J�n�N�v(4)       TO �J�n�N�S.
017650     MOVE �J�n���v(4)       TO �J�n���S.
017660     MOVE �J�n���v(4)       TO �J�n���S.
017670     MOVE �I���N�v(4)       TO �I���N�S.
017680     MOVE �I�����v(4)       TO �I�����S.
017690     MOVE �I�����v(4)       TO �I�����S.
017700     MOVE �������v(4)       TO �������S.
017710     MOVE �����`�F�b�N�v(4) TO �����`�F�b�N�S.
017720     MOVE ���~�`�F�b�N�v(4) TO ���~�`�F�b�N�S.
017730     MOVE �]��`�F�b�N�v(4) TO �]��`�F�b�N�S.
017740**********
017750* �T���� *
017760**********
017770     MOVE �������v(5)       TO �������T.
017780     MOVE �����N�v(5)       TO �����N�T.
017790     MOVE �������v(5)       TO �������T.
017800     MOVE �������v(5)       TO �������T.
017810     MOVE �����N�v(5)       TO �����N�T.
017820     MOVE �������v(5)       TO �������T.
017830     MOVE �������v(5)       TO �������T.
017840     MOVE �J�n�N�v(5)       TO �J�n�N�T.
017850     MOVE �J�n���v(5)       TO �J�n���T.
017860     MOVE �J�n���v(5)       TO �J�n���T.
017870     MOVE �I���N�v(5)       TO �I���N�T.
017880     MOVE �I�����v(5)       TO �I�����T.
017890     MOVE �I�����v(5)       TO �I�����T.
017900     MOVE �������v(5)       TO �������T.
017910     MOVE �����`�F�b�N�v(5) TO �����`�F�b�N�T.
017920     MOVE ���~�`�F�b�N�v(5) TO ���~�`�F�b�N�T.
017930     MOVE �]��`�F�b�N�v(5) TO �]��`�F�b�N�T.
017940**************
017950* �o�߃Z�b�g *
017960**************
017970     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
017980             UNTIL ( ���ʂb�m�s > ���ʐ��v ) OR
017990                   ( ���ʂb�m�s > 5 )
018000*         MOVE ���ʂb�m�s�v(���ʂb�m�s)   TO �o�ߕ��ʂb�m�s(���ʂb�m�s)
018010*         MOVE ���ʋ�؂v(���ʂb�m�s)     TO ���ʋ��(���ʂb�m�s)
018020         MOVE ����o�ߗ��̂v(���ʂb�m�s) TO �o�ߗ���(���ʂb�m�s)
018030     END-PERFORM.
018040*****************************************
018050*     �V�K�E�p���`�F�b�N�ɂ���        *
018060*   ���V�K...�����L�� ���p��...�����Ȃ� *
018070*****************************************
018080     MOVE �V�K�`�F�b�N�v    TO �V�K�`�F�b�N.
018090     MOVE �p���`�F�b�N�v    TO �p���`�F�b�N.
018100********************
018110* �����f�[�^�Z�b�g *
018120********************
018130*    ****************************************************************
018140*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
018150*    ****************************************************************
018160     MOVE �������v�q                   TO  ������.
018170     MOVE ���ԊO�`�F�b�N�v             TO  ���ԊO�`�F�b�N.
018180     MOVE �x���`�F�b�N�v               TO  �x���`�F�b�N.
018190     MOVE �[��`�F�b�N�v               TO  �[��`�F�b�N.
018200     MOVE �������Z���v�q               TO  �������Z��.
      *
           IF ((���ԊO�`�F�b�N�v NOT = SPACE) OR (�[��`�F�b�N�v NOT = SPACE) OR
              (�x���`�F�b�N�v NOT = SPACE)) AND
              ((�������Z���v NOT = ZERO) OR (�������Z���v NOT = ZERO))
              MOVE �������Z���v                 TO  �������Z��
              MOVE �������Z��؂v               TO  �������Z���
              MOVE �������Z���v                 TO  �������Z��
           END-IF.
      *
           MOVE ���������k���v�q             TO  ���������k��.
018210     MOVE �Č����v�q                   TO  �Č���.
018220     MOVE ���Ë����v�q                 TO  ���Ë���.
018230     MOVE ���É񐔂v�q                 TO  ���É�.
018240     MOVE ���×��v�q                   TO  ���×�.
018250     MOVE ��ԃ`�F�b�N�v               TO  ��ԃ`�F�b�N.
018260     MOVE �\���J��`�F�b�N�v           TO  �\���J��`�F�b�N.
018270     MOVE ���É��Z���v�q               TO  ���É��Z��.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
           MOVE �����񐔂v                   TO  ������.
019380     MOVE �������q���Z���v�q           TO  �������q���Z��.
           MOVE �^���񐔂v                   TO  �^����.
           MOVE �^�����v                     TO  �^����×�.
018090     MOVE �{�p���񋟗��v�q           TO  �{�p���񋟗�.
018090     MOVE ���׏����s���Z���v�q         TO  ���׏����s���Z��.
018090     MOVE ���׏����s���Z���v�q         TO  ���׏����s���Z��.
018330     MOVE ���v�v                       TO  ���v.
018340********************
018350* ���񏈒u���Z�b�g *
018360********************
018370     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018380             UNTIL ( ���ʂb�m�s > ���ʐ��v ) OR
018390                   ( ���ʂb�m�s > 5 )
018400         MOVE ���񏈒u���v�q(���ʂb�m�s) TO ���񏈒u��(���ʂb�m�s)
018410     END-PERFORM.
018420     MOVE ���񏈒u�����v�v         TO ���񏈒u�����v
018430*
018440     MOVE �{�×��`�F�b�N�v            TO �{�×��`�F�b�N.
018450     MOVE �������`�F�b�N�v            TO �������`�F�b�N.
018460     MOVE �Œ藿�`�F�b�N�v            TO �Œ藿�`�F�b�N.
018470********************
018480* �����������Z�b�g *
018490********************
018500*    **********
018510*    * �P���� *
018520*    **********
018530     MOVE ��ÒP���P�v�q             TO ��ÒP���P.
018540     MOVE ��É񐔂P�v�q             TO ��É񐔂P.
018550     MOVE ��×��P�v�q               TO ��×��P.
018560     MOVE ��㪖@�񐔂P�v�q           TO ��㪖@�񐔂P.
018570     MOVE ��㪖@���P�v�q             TO ��㪖@���P.
018580     MOVE ��㪖@�񐔂P�v�q           TO ��㪖@�񐔂P.
018590     MOVE ��㪖@���P�v�q             TO ��㪖@���P.
018600     MOVE �d�É񐔂P�v�q             TO �d�É񐔂P.
018610     MOVE �d�×��P�v�q               TO �d�×��P.
018620     MOVE ���v�P�v�q                 TO ���v�P.
018630     IF ( �����������P�v�q NOT = ZERO )
018640         COMPUTE �����������P = �����������P�v�q / 100
018650     END-IF.
018660     MOVE ���������v�P�v�q           TO ���������v�P.
018670*    **********
018680*    * �Q���� *
018690*    **********
018700     MOVE ��ÒP���Q�v�q             TO ��ÒP���Q.
018710     MOVE ��É񐔂Q�v�q             TO ��É񐔂Q.
018720     MOVE ��×��Q�v�q               TO ��×��Q.
018730     MOVE ��㪖@�񐔂Q�v�q           TO ��㪖@�񐔂Q.
018740     MOVE ��㪖@���Q�v�q             TO ��㪖@���Q.
018750     MOVE ��㪖@�񐔂Q�v�q           TO ��㪖@�񐔂Q.
018760     MOVE ��㪖@���Q�v�q             TO ��㪖@���Q.
018770     MOVE �d�É񐔂Q�v�q             TO �d�É񐔂Q.
018780     MOVE �d�×��Q�v�q               TO �d�×��Q.
018790     MOVE ���v�Q�v�q                 TO ���v�Q.
018800     IF ( �����������Q�v�q NOT = ZERO )
018810         COMPUTE �����������Q = �����������Q�v�q / 100
018820     END-IF.
018830     MOVE ���������v�Q�v�q           TO ���������v�Q.
018840*    ****************
018850*    * �R���ʁ^�W�� *
018860*    ****************
018870     MOVE ��ÒP���R�W�v�q             TO ��ÒP���R�W.
018880     MOVE ��É񐔂R�W�v�q             TO ��É񐔂R�W.
018890     MOVE ��×��R�W�v�q               TO ��×��R�W.
018900     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
018910     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
018920     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
018930     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
018940     MOVE �d�É񐔂R�W�v�q             TO �d�É񐔂R�W.
018950     MOVE �d�×��R�W�v�q               TO �d�×��R�W.
018960     MOVE ���v�R�W�v�q                 TO ���v�R�W.
018970     MOVE �����ʍ����v�R�W�v�q         TO �����ʍ����v�R�W.
018980     IF ( �����������R�W�v�q NOT = ZERO )
018990         COMPUTE �����������R�W = �����������R�W�v�q / 100
019000     END-IF.
019010     MOVE ���������v�R�W�v�q           TO ���������v�R�W.
      */ ������ 0.7��0.6 /42505
           IF (�{�p�a��N���v�q >= 42505)
              MOVE "60"                      TO �����R�W
              MOVE "0.6"                     TO �����ʂR�W
           END-IF.
019020*    ****************
019030*    * �R���ʁ^10�� *
019040*    ****************
019050     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
019060     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
019070     MOVE ��ÒP���R�O�v�q             TO ��ÒP���R�O.
019080     MOVE ��É񐔂R�O�v�q             TO ��É񐔂R�O.
019090     MOVE ��×��R�O�v�q               TO ��×��R�O.
019100     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
019110     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
019120     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
019130     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
019140     MOVE �d�É񐔂R�O�v�q             TO �d�É񐔂R�O.
019150     MOVE �d�×��R�O�v�q               TO �d�×��R�O.
019160     MOVE ���v�R�O�v�q                 TO ���v�R�O.
019170     IF ( �����������R�O�v�q NOT = ZERO )
019180         COMPUTE �����������R�O = �����������R�O�v�q / 100
019190     END-IF.
019200     MOVE ���������v�R�O�v�q           TO ���������v�R�O.
019210*    ****************
019220*    * �S���ʁ^�T�� *
019230*    ****************
019240*     MOVE ��ÒP���S�T�v�q             TO ��ÒP���S�T.
019250*     MOVE ��É񐔂S�T�v�q             TO ��É񐔂S�T.
019260*     MOVE ��×��S�T�v�q               TO ��×��S�T.
019270*     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
019280*     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
019290*     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
019300*     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
019310*     MOVE �d�É񐔂S�T�v�q             TO �d�É񐔂S�T.
019320*     MOVE �d�×��S�T�v�q               TO �d�×��S�T.
019330*     MOVE ���v�S�T�v�q                 TO ���v�S�T.
019340*     MOVE �����ʍ����v�S�T�v�q         TO �����ʍ����v�S�T.
019350*     IF ( �����������S�T�v�q NOT = ZERO )
019360*         COMPUTE �����������S�T = �����������S�T�v�q / 100
019370*     END-IF.
019380*     MOVE ���������v�S�T�v�q           TO ���������v�S�T.
019390*    ****************
019400*    * �S���ʁ^�W�� *
019410*    ****************
019420     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
019430     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
019440     MOVE ��ÒP���S�W�v�q             TO ��ÒP���S�W.
019450     MOVE ��É񐔂S�W�v�q             TO ��É񐔂S�W.
019460     MOVE ��×��S�W�v�q               TO ��×��S�W.
019470     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
019480     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
019490     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
019500     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
019510     MOVE �d�É񐔂S�W�v�q             TO �d�É񐔂S�W.
019520     MOVE �d�×��S�W�v�q               TO �d�×��S�W.
019530     MOVE ���v�S�W�v�q                 TO ���v�S�W.
019540     MOVE �����ʍ����v�S�W�v�q         TO �����ʍ����v�S�W.
019550     IF ( �����������S�W�v�q NOT = ZERO )
019560         COMPUTE �����������S�W = �����������S�W�v�q / 100
019570     END-IF.
019580     MOVE ���������v�S�W�v�q           TO ���������v�S�W.
      */ ������ 0.7��0.6 /42505
           IF (�{�p�a��N���v�q >= 42505)
              MOVE "60"                      TO �����S�W
              MOVE "0.6"                     TO �����ʂS�W
           END-IF.
019590*    ****************
019600*    * �S���ʁ^10�� *
019610*    ****************
019620     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
019630     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
019640     MOVE ��ÒP���S�O�v�q             TO ��ÒP���S�O.
019650     MOVE ��É񐔂S�O�v�q             TO ��É񐔂S�O.
019660     MOVE ��×��S�O�v�q               TO ��×��S�O.
019670     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
019680     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
019690     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
019700     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
019710     MOVE �d�É񐔂S�O�v�q             TO �d�É񐔂S�O.
019720     MOVE �d�×��S�O�v�q               TO �d�×��S�O.
019730     MOVE ���v�S�O�v�q                 TO ���v�S�O.
019740     IF ( �����������S�O�v�q NOT = ZERO )
019750         COMPUTE �����������S�O = �����������S�O�v�q / 100
019760     END-IF.
019770     MOVE ���������v�S�O�v�q           TO ���������v�S�O.
019780*
019790*��***********************************************************************
019800* �T���ʁ^2.5���̈󎚂͕K�v�Ȃ��B
019810*------------------------------------------------------------------------*
019820*    *****************
019830*    * �T���ʁ^2.5�� *
019840*    *****************
019850*     MOVE ��ÒP���T�Q�v�q             TO ��ÒP���T�Q.
019860*     MOVE ��É񐔂T�Q�v�q             TO ��É񐔂T�Q.
019870*     MOVE ��×��T�Q�v�q               TO ��×��T�Q.
019880*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
019890*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
019900*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
019910*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
019920*     MOVE �d�É񐔂T�Q�v�q             TO �d�É񐔂T�Q.
019930*     MOVE �d�×��T�Q�v�q               TO �d�×��T�Q.
019940*     MOVE ���v�T�Q�v�q                 TO ���v�T�Q.
019950*     MOVE �����ʍ����v�T�Q�v�q         TO �����ʍ����v�T�Q.
019960*     IF ( �����������T�Q�v�q NOT = ZERO )
019970*         COMPUTE �����������T�Q = �����������T�Q�v�q / 100
019980*     END-IF.
019990*     MOVE ���������v�T�Q�v�q           TO ���������v�T�Q.
020000*��***********************************************************************
020010*
020020*    ****************
020030*    * �T���ʁ^�T�� *
020040*    ****************
020050*     MOVE �����J�n���T�T�v�q           TO �����J�n���T�T.
020060*     MOVE �����J�n���T�T�v�q           TO �����J�n���T�T.
020070*     MOVE ��ÒP���T�T�v�q             TO ��ÒP���T�T.
020080*     MOVE ��É񐔂T�T�v�q             TO ��É񐔂T�T.
020090*     MOVE ��×��T�T�v�q               TO ��×��T�T.
020100*     MOVE ��㪖@�񐔂T�T�v�q           TO ��㪖@�񐔂T�T.
020110*     MOVE ��㪖@���T�T�v�q             TO ��㪖@���T�T.
020120*     MOVE ��㪖@�񐔂T�T�v�q           TO ��㪖@�񐔂T�T.
020130*     MOVE ��㪖@���T�T�v�q             TO ��㪖@���T�T.
020140*     MOVE �d�É񐔂T�T�v�q             TO �d�É񐔂T�T.
020150*     MOVE �d�×��T�T�v�q               TO �d�×��T�T.
020160*     MOVE ���v�T�T�v�q                 TO ���v�T�T.
020170*     MOVE �����ʍ����v�T�T�v�q         TO �����ʍ����v�T�T.
020180*     IF ( �����������T�T�v�q NOT = ZERO )
020190*         COMPUTE �����������T�T = �����������T�T�v�q / 100
020200*     END-IF.
020210*     MOVE ���������v�T�T�v�q           TO ���������v�T�T.
020220*    ****************
020230*    * �T���ʁ^�W�� *
020240*    ****************
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
020420*    ****************
020430*    * �T���ʁ^10�� *
020440*    ****************
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
020620*
020630* ********************
020640* * �󋋎ҕ��S�z���� *
020650* ********************
020660*     IF ( �A���|�󋋎ҕ��S�z  NOT = ZERO )
020670*         MOVE NC"�󋋎ҕ��S�z"   TO ���S�z����莚
020680*     END-IF.
020690*     PERFORM VARYING �񐔂b�m�s FROM 1 BY 1 UNTIL �񐔂b�m�s > 4
020700*         MOVE ������S�񐔂v(�񐔂b�m�s) TO ������S��(�񐔂b�m�s)
020710*         MOVE ���������S�z�v(�񐔂b�m�s) TO ���������S�z(�񐔂b�m�s)
020720*         IF ( ������S�񐔂v(�񐔂b�m�s) NOT = SPACE )
020730*              MOVE NC"�~"                TO ���������S�z�P��(�񐔂b�m�s)
020740*         END-IF
020750*     END-PERFORM.
020760* 
020770     MOVE �K�p�P�v                       TO �K�p�P.
020780     MOVE �K�p�Q�v                       TO �K�p�Q.
019660*     MOVE �K�p�R�v                       TO �K�p�R.
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
              MOVE 13           TO �A���^�|��R�[�h
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
                 MOVE �A���^�|�������q��(1 �J�E���^) TO ������(�J�E���^)
                 IF �A���^�|�������q��(1 �J�E���^) NOT = ZERO
                    MOVE "��"                        TO ��(�J�E���^)
                 END-IF
              END-PERFORM
              PERFORM VARYING �J�E���^ FROM 1 BY 1
                        UNTIL �J�E���^ > 5
                 MOVE �A���^�|�^����(�J�E���^)     TO �^����(�J�E���^)
              END-PERFORM
           END-IF.
      *
020790******
020440     MOVE ���Z�|���v                     TO ���v.
020450     MOVE ���Z�|�ꕔ���S��               TO �ꕔ���S��.
020460     MOVE ���Z�|�������z                 TO �������z.
      *
           EVALUATE TRUE
      */���m��(�R�g�̂ݎg�p�B��p�A���S�͖{�́B�����z�ɏ���)
           WHEN �s�����ԍ��v(3:2) = "23"
               MOVE ���Z�|�����������z TO �������z
      */���{(�R�g�̂ݎg�p�B��p�͖{�́B���S�A�����z�ɏ���)
      */���s�����l
           WHEN (�s�����ԍ��v(3:2)  = "27") OR
                ((�s�����ԍ��v(3:2) = "26") AND (������ʂv�q NOT = 54))
               MOVE ���Z�|�󋋎ҕ��S�z TO �ꕔ���S��
               MOVE ���Z�|�����������z TO �������z
      */��t���̎q�ǂ���Ô��/120404
      */��t���̏d�x�S�g��Q��Ô��/150703
      */��t���̂ЂƂ�e��Ô��/201001
           WHEN ((������ʂv�q = 60) AND (�s�����ԍ��v(1:4) =  "8312")) OR
                ((������ʂv�q = 53) AND (�s�����ԍ��v(1:4) =  "8112")) OR
                ((������ʂv�q = 52) AND (�s�����ԍ��v(1:4) =  "8512"))
               MOVE "X" TO EDIT-MODE OF   �ꕔ���S��
               MOVE ���Z�|�ꕔ���S��   TO �ꕔ���S���Q
               MOVE ���Z�|�󋋎ҕ��S�z TO �󋋎ҕ��S�z�R
               MOVE ���Z�|�����������z TO �������z
           WHEN OTHER
020830         MOVE ���Z�|�󋋎ҕ��S�z TO �󋋎ҕ��S�z
020840         MOVE ���Z�|�����������z TO ���������z
           END-EVALUATE.
020850*
022410*------------------------------------------------------------------------*
      */�����p��20241007/������
           MOVE ���ʌp�������v(1) TO �p�������P.
           MOVE ���ʌp�������v(2) TO �p�������Q.
           MOVE ���ʌp�������v(3) TO �p�������R.
           MOVE ���ʌp�������v(4) TO �p�������S.
           MOVE ���ʌp�������v(5) TO �p�������T.
      *
           IF ���Z�|�����p��������P NOT = ZERO
               MOVE ZERO TO �����������P
018410         COMPUTE �p��������P = �����������P�v�q / 100
           END-IF
           IF ���Z�|�����p��������Q NOT = ZERO
               MOVE ZERO TO �����������Q
018410         COMPUTE �p��������Q = �����������Q�v�q / 100
           END-IF
           IF ���Z�|�����p��������R�W NOT = ZERO
               MOVE ZERO TO �����������R�W
018410         COMPUTE �p��������R�W = �����������R�W�v�q / 100
           END-IF
           IF ���Z�|�����p��������R�O NOT = ZERO
               MOVE ZERO TO �����������R�O
018410         COMPUTE �p��������R�O = �����������R�O�v�q / 100
           END-IF
           IF ���Z�|�����p��������S�W NOT = ZERO
               MOVE ZERO TO �����������S�W
018410         COMPUTE �p��������S�W = �����������S�W�v�q / 100
           END-IF
           IF ���Z�|�����p��������S�O NOT = ZERO
               MOVE ZERO TO �����������S�O
018410         COMPUTE �p��������S�O = �����������S�O�v�q / 100
           END-IF
      */�����p��20241007/������
      */�����p��R�����g/20241007������
022420** �����p��̎��A�E�v���ɓ��e���L��
      **
      *     MOVE SPACE                     TO �����p��v.
      *     IF (���Z�|���ʌp������(1) > 5) OR (���Z�|���ʌp������(2) > 5) OR
      *        (���Z�|���ʌp������(3) > 5) OR (���Z�|���ʌp������(4) > 5) OR
      *        (���Z�|���ʌp������(5) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l
      *     END-IF.
      *     IF (���Z�|���ʌp������(1) > 5)
      *        MOVE ���Z�|���ʌp������(1)  TO �����v
      *        MOVE �������v(1)            TO �������v�q(1)
      *        STRING "(1)"                DELIMITED BY SIZE
      *               �������v�o(1)        DELIMITED BY "�@"
      *               "�A�p������"         DELIMITED BY SIZE
      *               �����v               DELIMITED BY SIZE
      *               "��"                 DELIMITED BY SIZE
      *          INTO �����p��P�v�s
      *        END-STRING
      *     END-IF.
      *     IF (���Z�|���ʌp������(2) > 5)
      *        MOVE ���Z�|���ʌp������(2)  TO �����v
      *        MOVE �������v(2)            TO �������v�q(2)
      *        STRING "(2)"                DELIMITED BY SIZE
      *               �������v�o(2)        DELIMITED BY "�@"
      *               "�A�p������"         DELIMITED BY SIZE
      *               �����v               DELIMITED BY SIZE
      *               "��"                 DELIMITED BY SIZE
      *          INTO �����p��Q�v�s
      *        END-STRING
      *     END-IF.
      *     IF (���Z�|���ʌp������(3) > 5)
      *        MOVE ���Z�|���ʌp������(3)  TO �����v
      *        MOVE �������v(3)            TO �������v�q(3)
      *        STRING "(3)"                DELIMITED BY SIZE
      *               �������v�o(3)        DELIMITED BY "�@"
      *               "�A�p������"         DELIMITED BY SIZE
      *               �����v               DELIMITED BY SIZE
      *               "��"                 DELIMITED BY SIZE
      *          INTO �����p��R�v�s
      *        END-STRING
      *     END-IF.
      *     IF (���Z�|���ʌp������(4) > 5)
      *        MOVE ���Z�|���ʌp������(4)  TO �����v
      *        MOVE �������v(4)            TO �������v�q(4)
      *        STRING "(4)"                DELIMITED BY SIZE
      *               �������v�o(4)        DELIMITED BY "�@"
      *               "�A�p������"         DELIMITED BY SIZE
      *               �����v               DELIMITED BY SIZE
      *               "��"                 DELIMITED BY SIZE
      *          INTO �����p��S�v�s
      *        END-STRING
      *     END-IF.
      *     IF (���Z�|���ʌp������(5) > 5)
      *        MOVE ���Z�|���ʌp������(5)  TO �����v
      *        MOVE �������v(5)            TO �������v�q(5)
      *        STRING "(5)"                DELIMITED BY SIZE
      *               �������v�o(5)        DELIMITED BY "�@"
      *               "�A�p������"         DELIMITED BY SIZE
      *               �����v               DELIMITED BY SIZE
      *               "��"                 DELIMITED BY SIZE
      *          INTO �����p��T�v�s
      *        END-STRING
      *     END-IF.
      *     MOVE �����p��b�l   TO �����P�v.
      *     MOVE �����p��P�v�s TO �����Q�v.
      *     CALL �v���O�������v WITH C LINKAGE
      *                   USING BY REFERENCE �����P�v
      *                         BY REFERENCE �����Q�v.
      *     MOVE �����p��Q�v�s TO �����Q�v.
      *     CALL �v���O�������v WITH C LINKAGE
      *                   USING BY REFERENCE �����P�v
      *                         BY REFERENCE �����Q�v.
      *     MOVE �����p��R�v�s TO �����Q�v.
      *     CALL �v���O�������v WITH C LINKAGE
      *                   USING BY REFERENCE �����P�v
      *                         BY REFERENCE �����Q�v.
      *     MOVE �����p��S�v�s TO �����Q�v.
      *     CALL �v���O�������v WITH C LINKAGE
      *                   USING BY REFERENCE �����P�v
      *                         BY REFERENCE �����Q�v.
      *     MOVE �����p��T�v�s TO �����Q�v.
      *     CALL �v���O�������v WITH C LINKAGE
      *                   USING BY REFERENCE �����P�v
      *                         BY REFERENCE �����Q�v.
      *     MOVE �����P�v       TO �����p��.
      **
      */�����p��R�����g/20241007������
020878**********************
020880* �{�p���f�[�^�Z�b�g *
020890**********************
           MOVE �s���{���i�h�r�v       TO �s���{���ԍ�.
020900     MOVE �_���t�ԍ��v           TO �_���t�ԍ�.
020910*     MOVE �_���t�ԍ��P�v           TO �_���t�ԍ��P.
020920*     MOVE �_���t�ԍ��Q�v           TO �_���t�ԍ��Q.
020930*     MOVE �_���t�ԍ��R�v           TO �_���t�ԍ��R.
020940*     MOVE ��z���󗝔ԍ��v       TO ��z���󗝔ԍ�.
020950     MOVE �{�p���X�֔ԍ��P�v     TO �{�p���X�֔ԍ��P.
020960     MOVE �{�p���X�֔ԍ��Q�v     TO �{�p���X�֔ԍ��Q.
020980     MOVE �{�p���Z���P�v         TO �{�p���Z���P.
020990     MOVE �{�p���Z���Q�v         TO �{�p���Z���Q.
021000     MOVE �ڍ��t�����ԍ��v     TO �ڍ��t�����ԍ�.
021010     MOVE ��\�҃J�i�v           TO ��\�҃J�i.
021020     MOVE ��\�Җ��v             TO ��\�Җ�.
021030     MOVE �ڍ��t�����ԍ��v     TO �ڍ��t�����ԍ�.
021040     MOVE �{�p���d�b�ԍ��v       TO �{�p���d�b�ԍ�.
021050*
021060     MOVE �ڍ��@���v             TO �ڍ��@��.
021070*
021080*     MOVE ��s���x�X���v         TO ��s���x�X��.
021090*     MOVE �a����ʃR�����g�v     TO �a�����.
021100     MOVE �����ԍ��v             TO �����ԍ�.
021110     MOVE �������`�l�J�i�v       TO �������`�l�J�i.
021120*     MOVE �������`�l�ƃJ�i�P�v   TO �������`�l.
021130*     MOVE �������`�l�ƃJ�i�Q�v   TO �������`�l�Q.
           MOVE �������`�l�v           TO �������`�l.
021140     MOVE �R�����g�P�v           TO �R�����g�P.
021150     MOVE �R�����g�Q�v           TO �R�����g�Q.
021160     MOVE �R�����g�R�v           TO �R�����g�R.
021170     MOVE �R�����g�S�v           TO �R�����g�S.
021180*     MOVE �R�����g�T�v           TO �R�����g�T.
021190*
           MOVE ���Z�@�֖��P�v         TO ���Z�@�֖��P.
           MOVE ���Z�@�֖��Q�v         TO ���Z�@�֖��Q.
      *     MOVE ���Z�@�֖��R�v         TO ���Z�@�֖��R.
      *     MOVE ���Z�@�֖��S�v         TO ���Z�@�֖��S.
           MOVE �x�X���P�v             TO �x�X���P.
           MOVE �x�X���Q�v             TO �x�X���Q.
      *     MOVE �x�X���R�v             TO �x�X���R.
      *     MOVE �x�X���S�v             TO �x�X���S.
           MOVE �U���`�F�b�N�v         TO �U���`�F�b�N.
           MOVE ���ʃ`�F�b�N�v         TO ���ʃ`�F�b�N.
           MOVE �����`�F�b�N�v         TO �����`�F�b�N.
           MOVE ��s�`�F�b�N�v         TO ��s�`�F�b�N.
           MOVE ���Ƀ`�F�b�N�v         TO ���Ƀ`�F�b�N.
           MOVE �_���`�F�b�N�v         TO �_���`�F�b�N.
           MOVE �{�X�`�F�b�N�v         TO �{�X�`�F�b�N.
           MOVE �x�X�`�F�b�N�v         TO �x�X�`�F�b�N.
           MOVE �{�x���`�F�b�N�v       TO �{�x���`�F�b�N.

021200* / �_���t�E���҈ϔC�� /
      */�����C��/������20190408
037370     IF (�{�p�a��v > 4) OR (�p����ʂv > 1)
               MOVE �{�p�a��v         TO ���|�����敪
037380         READ �����}�X�^
037390         NOT INVALID KEY
037400             MOVE ���|��������   TO �󗝘a��
037410         END-READ
      *         MOVE "===="             TO �󗝘a�����
           END-IF.
      */�����C��/������20190408
021210     MOVE �_���t�N�v             TO �󗝔N.
021220     MOVE �_���t���v             TO �󗝌�.
021230     MOVE �_���t���v             TO �󗝓�.
021240* ( �ϔC�N���� ������邩 )
021250     IF ( �A���|�ϔC���  = ZERO )
037370     IF (�{�p�a��v > 4) OR (�p����ʂv > 1)
037370         IF �{�p�a��v > 4
                   MOVE �{�p�a��v         TO ���|�����敪
037380             READ �����}�X�^
037390             NOT INVALID KEY
037400                 MOVE ���|��������   TO �ϔC�a��
037410             END-READ
      *             MOVE "===="             TO �ϔC�a�����
               END-IF
      */�����C��/������20190408
021260         MOVE ���҈ϔC�N�v       TO �ϔC�N
021270         MOVE ���҈ϔC���v       TO �ϔC��
021280         MOVE ���҈ϔC���v       TO �ϔC��
021290     END-IF.
021300*
021310* �{�pID
021320     MOVE ���{�p�h�c�v           TO ���{�p�h�c.
021330*
021340************************
021350* ���Z�v�g���я��Z�b�g *
021360************************
021370     MOVE ���ԌŒ�v          TO ���ԌŒ�.
021380     MOVE ���Ԃv              TO ����.
021390     MOVE ���Ҕԍ��v�q        TO ���Ҕԍ�.
021400     MOVE �}�Ԃv�q            TO �}��.
021410*
021420*
021430* �����s�@�E��Ɂu�O�v�󎚁i����ҁj 14/10�`
021440*     MOVE ���ʃ}�[�N�v           TO ���ʃ}�[�N.
021450*
021460* ���m���@���ʃR�����g�i�S�P�V�j14/10�`
021470*     MOVE ���ʃR�����g�v         TO ���ʃR�����g.
021310*-------------------------------------------------------------------------*
021320*--- �� ���Z�E�v�ăZ�b�g�́A���̈���Z�b�gSECTION �̍Ō�ɂ�邱�ƁI -----*
021330     PERFORM ���Z�E�v�ăZ�b�g.
021340*-------------------------------------------------------------------------*
021480*
021492*-------------------------------------------------------------------------*
021493*--- �� �n����L�����́A���̈���Z�b�gSECTION �̍Ō�ɂ�邱�ƁI   �@-----*
021494     PERFORM �n����L����.
021495*-------------------------------------------------------------------------*
021496*
021500********     PERFORM �e�X�g�󎚏���.
021510*
021520*================================================================*
021530 ���ڏ����� SECTION.
021540*
021550     INITIALIZE �{�p�����v.
021560     INITIALIZE ��f�ҏ��v.
021570     INITIALIZE �������v.
021580     INITIALIZE ���l���v.
021590*     INITIALIZE �n�b�q�R�[�h�v.
021600     INITIALIZE �����P�v�q.
021610     INITIALIZE �����Q�v�q.
021620     INITIALIZE �����R�v�q.
021640     INITIALIZE YCH6427P.
021630     MOVE SPACE TO YCH6427P.
021650*================================================================*
021660 �������擾 SECTION.
021670*
           MOVE 3            TO ���Z�|���Z���.
019550     MOVE �{�p�a��v�q TO ���Z�|�{�p�a��.
019560     MOVE �{�p�N�v�q   TO ���Z�|�{�p�N.
019570     MOVE �{�p���v�q   TO ���Z�|�{�p��.
019580     MOVE ���Ҕԍ��v�q TO ���Z�|���Ҕԍ�.
019590     MOVE �}�Ԃv�q     TO ���Z�|�}��.
019600     READ ���Z�v�g�e
019630     INVALID KEY
              MOVE SPACE     TO ���Z�|���R�[�h
              INITIALIZE        ���Z�|���R�[�h
           END-READ.
021680********************
021690* �����f�[�^�Z�b�g *
021700********************
021710*    ****************************************************************
021720*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
021730*    ****************************************************************
021740     MOVE ���Z�|������                 TO �������v�q.
021750     IF ( ���Z�|���ԊO = 1 )
021760         MOVE NC"��"                   TO ���ԊO�`�F�b�N�v
021770     END-IF.
021780     IF ( ���Z�|�x�� = 1 )
021790         MOVE NC"��"                   TO �x���`�F�b�N�v
021800     END-IF.
021810     IF ( ���Z�|�[�� = 1 )
021820         MOVE NC"��"                   TO �[��`�F�b�N�v
021830     END-IF.
021840*
021850     MOVE ���Z�|�������Z��             TO  �������Z���v�q.
           MOVE ���Z�|���������k��           TO  ���������k���v�q.
021860     MOVE ���Z�|�Č���                 TO  �Č����v�q.
021870     MOVE ���Z�|���Ë���               TO  ���Ë����v�q.
021880     MOVE ���Z�|���É�               TO  ���É񐔂v�q.
021890     MOVE ���Z�|���×�                 TO  ���×��v�q.
021900     MOVE ���Z�|���É��Z��             TO  ���É��Z���v�q.
021910*
021920     IF ( ���Z�|��� = 1 )
021930         MOVE NC"��"                   TO ��ԃ`�F�b�N�v
021940     END-IF.
021950     IF ( ���Z�|�\���J�� = 1 )
021960         MOVE NC"��"                   TO �\���J��`�F�b�N�v
021970     END-IF.
021980*
021990     MOVE ���Z�|�������q���Z��         TO  �������q���Z���v�q.
022000*
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
           MOVE ���Z�|�������q��            TO �����񐔂v.
           MOVE ���Z�|�^����É�            TO �^���񐔂v.
           MOVE ���Z�|�^����×�              TO �^�����v.
021940*
021950     MOVE ���Z�|�{�p���񋟗�          TO  �{�p���񋟗��v�q.
      */2022
           MOVE ���Z�|���׏����s���Z��         TO ���׏����s���Z���v�q.
           MOVE ���Z�|���׏����s���Z��         TO ���׏����s���Z���v�q.
021960* ���v
022420     COMPUTE ���v�v = ���Z�|���v + ���Z�|�^����×� + ���Z�|���׏����s���Z��.
022140********************
022150* ���񏈒u���Z�b�g *
022160********************
022170     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
022180             UNTIL ( ���ʂb�m�s > ���ʐ��v )
022190         MOVE ���Z�|���񏈒u��(���ʂb�m�s) TO ���񏈒u���v�q(���ʂb�m�s)
022200         IF ( ���Z�|���񏈒u��(���ʂb�m�s) NOT = ZERO )
022210            EVALUATE ���|�������(���ʂb�m�s)
022220* �P���E�Ŗo�E����
022230            WHEN 1
022240            WHEN 2
022250            WHEN 3
022260                MOVE NC"��"       TO �{�×��`�F�b�N�v
022270* �E�P�E���܁E���܍S�k
022280            WHEN 4
022290            WHEN 5
022300            WHEN 7
022310                MOVE NC"��"       TO �������`�F�b�N�v
022320* �s�S���܁E�s�S���܍S�k
022330            WHEN 6
022340            WHEN 8
022350                MOVE NC"��"       TO �Œ藿�`�F�b�N�v
022360            END-EVALUATE
022370         END-IF
022380     END-PERFORM.
022390     MOVE ���Z�|���񏈒u�����v         TO ���񏈒u�����v�v.
022400********************
022410* �����������Z�b�g *
022420********************
022430*    **********
022440*    * �P���� *
022450*    **********
022460     MOVE ���Z�|��ÒP���P             TO ��ÒP���P�v�q.
022470     MOVE ���Z�|��É񐔂P             TO ��É񐔂P�v�q.
022480     MOVE ���Z�|��×��P               TO ��×��P�v�q.
022490     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
022500     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
022510     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
022520     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
022530     MOVE ���Z�|�d�É񐔂P             TO �d�É񐔂P�v�q.
022540     MOVE ���Z�|�d�×��P               TO �d�×��P�v�q.
022550     MOVE ���Z�|���v�P                 TO ���v�P�v�q.
           IF ���Z�|�����p��������P NOT = ZERO
023850         MOVE ���Z�|�����p��������P   TO �����������P�v�q
           ELSE
024000         MOVE ���Z�|�����������P       TO �����������P�v�q
           END-IF.
022570     MOVE ���Z�|���������v�P           TO ���������v�P�v�q.
022580*    **********
022590*    * �Q���� *
022600*    **********
022610     MOVE ���Z�|��ÒP���Q             TO ��ÒP���Q�v�q.
022620     MOVE ���Z�|��É񐔂Q             TO ��É񐔂Q�v�q.
022630     MOVE ���Z�|��×��Q               TO ��×��Q�v�q.
022640     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
022650     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
022660     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
022670     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
022680     MOVE ���Z�|�d�É񐔂Q             TO �d�É񐔂Q�v�q.
022690     MOVE ���Z�|�d�×��Q               TO �d�×��Q�v�q.
022700     MOVE ���Z�|���v�Q                 TO ���v�Q�v�q.
           IF ���Z�|�����p��������Q NOT = ZERO
023850         MOVE ���Z�|�����p��������Q   TO �����������Q�v�q
           ELSE
024000         MOVE ���Z�|�����������Q       TO �����������Q�v�q
           END-IF.
022720     MOVE ���Z�|���������v�Q           TO ���������v�Q�v�q.
022730*    ****************
022740*    * �R���ʁ^�W�� *
022750*    ****************
022760     MOVE ���Z�|��ÒP���R�W             TO ��ÒP���R�W�v�q.
022770     MOVE ���Z�|��É񐔂R�W             TO ��É񐔂R�W�v�q.
022780     MOVE ���Z�|��×��R�W               TO ��×��R�W�v�q.
022790     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
022800     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
022810     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
022820     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
022830     MOVE ���Z�|�d�É񐔂R�W             TO �d�É񐔂R�W�v�q.
022840     MOVE ���Z�|�d�×��R�W               TO �d�×��R�W�v�q.
022850     MOVE ���Z�|���v�R�W                 TO ���v�R�W�v�q.
022860     MOVE ���Z�|�����ʍ����v�R�W         TO �����ʍ����v�R�W�v�q.
           IF ���Z�|�����p��������R�W NOT = ZERO
023850         MOVE ���Z�|�����p��������R�W   TO �����������R�W�v�q
           ELSE
024160         MOVE ���Z�|�����������R�W       TO �����������R�W�v�q
           END-IF.
022880     MOVE ���Z�|���������v�R�W           TO ���������v�R�W�v�q.
022890*    ****************
022900*    * �R���ʁ^10�� *
022910*    ****************
022920     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
022930     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
022940     MOVE ���Z�|��ÒP���R�O             TO ��ÒP���R�O�v�q.
022950     MOVE ���Z�|��É񐔂R�O             TO ��É񐔂R�O�v�q.
022960     MOVE ���Z�|��×��R�O               TO ��×��R�O�v�q.
022970     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
022980     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
022990     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
023000     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
023010     MOVE ���Z�|�d�É񐔂R�O             TO �d�É񐔂R�O�v�q.
023020     MOVE ���Z�|�d�×��R�O               TO �d�×��R�O�v�q.
023030     MOVE ���Z�|���v�R�O                 TO ���v�R�O�v�q.
           IF ���Z�|�����p��������R�O NOT = ZERO
023850         MOVE ���Z�|�����p��������R�O   TO �����������R�O�v�q
           ELSE
024330         MOVE ���Z�|�����������R�O       TO �����������R�O�v�q
           END-IF.
023050     MOVE ���Z�|���������v�R�O           TO ���������v�R�O�v�q.
023060*    ****************
023070*    * �S���ʁ^�T�� *
023080*    ****************
023090     MOVE ���Z�|��ÒP���S�T             TO ��ÒP���S�T�v�q.
023100     MOVE ���Z�|��É񐔂S�T             TO ��É񐔂S�T�v�q.
023110     MOVE ���Z�|��×��S�T               TO ��×��S�T�v�q.
023120     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
023130     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
023140     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
023150     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
023160     MOVE ���Z�|�d�É񐔂S�T             TO �d�É񐔂S�T�v�q.
023170     MOVE ���Z�|�d�×��S�T               TO �d�×��S�T�v�q.
023180     MOVE ���Z�|���v�S�T                 TO ���v�S�T�v�q.
023190     MOVE ���Z�|�����ʍ����v�S�T         TO �����ʍ����v�S�T�v�q.
023200     MOVE ���Z�|�����������S�T           TO �����������S�T�v�q.
023210     MOVE ���Z�|���������v�S�T           TO ���������v�S�T�v�q.
023220*    ****************
023230*    * �S���ʁ^�W�� *
023240*    ****************
023250     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
023260     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
023270     MOVE ���Z�|��ÒP���S�W             TO ��ÒP���S�W�v�q.
023280     MOVE ���Z�|��É񐔂S�W             TO ��É񐔂S�W�v�q.
023290     MOVE ���Z�|��×��S�W               TO ��×��S�W�v�q.
023300     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
023310     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
023320     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
023330     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
023340     MOVE ���Z�|�d�É񐔂S�W             TO �d�É񐔂S�W�v�q.
023350     MOVE ���Z�|�d�×��S�W               TO �d�×��S�W�v�q.
023360     MOVE ���Z�|���v�S�W                 TO ���v�S�W�v�q.
023370     MOVE ���Z�|�����ʍ����v�S�W         TO �����ʍ����v�S�W�v�q.
           IF ���Z�|�����p��������S�W NOT = ZERO
023850         MOVE ���Z�|�����p��������S�W   TO �����������S�W�v�q
           ELSE
024670         MOVE ���Z�|�����������S�W       TO �����������S�W�v�q
           END-IF.
023390     MOVE ���Z�|���������v�S�W           TO ���������v�S�W�v�q.
023400*    ****************
023410*    * �S���ʁ^10�� *
023420*    ****************
023430     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
023440     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
023450     MOVE ���Z�|��ÒP���S�O             TO ��ÒP���S�O�v�q.
023460     MOVE ���Z�|��É񐔂S�O             TO ��É񐔂S�O�v�q.
023470     MOVE ���Z�|��×��S�O               TO ��×��S�O�v�q.
023480     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
023490     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
023500     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
023510     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
023520     MOVE ���Z�|�d�É񐔂S�O             TO �d�É񐔂S�O�v�q.
023530     MOVE ���Z�|�d�×��S�O               TO �d�×��S�O�v�q.
023540     MOVE ���Z�|���v�S�O                 TO ���v�S�O�v�q.
           IF ���Z�|�����p��������S�O NOT = ZERO
023850         MOVE ���Z�|�����p��������S�O   TO �����������S�O�v�q
           ELSE
024840         MOVE ���Z�|�����������S�O       TO �����������S�O�v�q
           END-IF.
023560     MOVE ���Z�|���������v�S�O           TO ���������v�S�O�v�q.
023570*    *****************
023580*    * �T���ʁ^2.5�� *
023590*    *****************
023600     MOVE ���Z�|��ÒP���T�Q             TO ��ÒP���T�Q�v�q.
023610     MOVE ���Z�|��É񐔂T�Q             TO ��É񐔂T�Q�v�q.
023620     MOVE ���Z�|��×��T�Q               TO ��×��T�Q�v�q.
023630     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
023640     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
023650     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
023660     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
023670     MOVE ���Z�|�d�É񐔂T�Q             TO �d�É񐔂T�Q�v�q.
023680     MOVE ���Z�|�d�×��T�Q               TO �d�×��T�Q�v�q.
023690     MOVE ���Z�|���v�T�Q                 TO ���v�T�Q�v�q.
023700     MOVE ���Z�|�����ʍ����v�T�Q         TO �����ʍ����v�T�Q�v�q.
023710     MOVE ���Z�|�����������T�Q           TO �����������T�Q�v�q.
023720     MOVE ���Z�|���������v�T�Q           TO ���������v�T�Q�v�q.
023730*    ****************
023740*    * �T���ʁ^�T�� *
023750*    ****************
023760     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
023770     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
023780     MOVE ���Z�|��ÒP���T�T             TO ��ÒP���T�T�v�q.
023790     MOVE ���Z�|��É񐔂T�T             TO ��É񐔂T�T�v�q.
023800     MOVE ���Z�|��×��T�T               TO ��×��T�T�v�q.
023810     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
023820     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
023830     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
023840     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
023850     MOVE ���Z�|�d�É񐔂T�T             TO �d�É񐔂T�T�v�q.
023860     MOVE ���Z�|�d�×��T�T               TO �d�×��T�T�v�q.
023870     MOVE ���Z�|���v�T�T                 TO ���v�T�T�v�q.
023880     MOVE ���Z�|�����ʍ����v�T�T         TO �����ʍ����v�T�T�v�q.
023890     MOVE ���Z�|�����������T�T           TO �����������T�T�v�q.
023900     MOVE ���Z�|���������v�T�T           TO ���������v�T�T�v�q.
023910*    ****************
023920*    * �T���ʁ^�W�� *
023930*    ****************
023940     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
023950     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
023960     MOVE ���Z�|��ÒP���T�W             TO ��ÒP���T�W�v�q.
023970     MOVE ���Z�|��É񐔂T�W             TO ��É񐔂T�W�v�q.
023980     MOVE ���Z�|��×��T�W               TO ��×��T�W�v�q.
023990     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
024000     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
024010     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
024020     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
024030     MOVE ���Z�|�d�É񐔂T�W             TO �d�É񐔂T�W�v�q.
024040     MOVE ���Z�|�d�×��T�W               TO �d�×��T�W�v�q.
024050     MOVE ���Z�|���v�T�W                 TO ���v�T�W�v�q.
024060     MOVE ���Z�|�����ʍ����v�T�W         TO �����ʍ����v�T�W�v�q.
           IF ���Z�|�����p��������T�W NOT = ZERO
023850         MOVE ���Z�|�����p��������T�W   TO �����������T�W�v�q
           ELSE
025360         MOVE ���Z�|�����������T�W       TO �����������T�W�v�q
           END-IF.
024080     MOVE ���Z�|���������v�T�W           TO ���������v�T�W�v�q.
024090*    ****************
024100*    * �T���ʁ^10�� *
024110*    ****************
024120     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
024130     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
024140     MOVE ���Z�|��ÒP���T�O             TO ��ÒP���T�O�v�q.
024150     MOVE ���Z�|��É񐔂T�O             TO ��É񐔂T�O�v�q.
024160     MOVE ���Z�|��×��T�O               TO ��×��T�O�v�q.
024170     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
024180     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
024190     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
024200     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
024210     MOVE ���Z�|�d�É񐔂T�O             TO �d�É񐔂T�O�v�q.
024220     MOVE ���Z�|�d�×��T�O               TO �d�×��T�O�v�q.
024230     MOVE ���Z�|���v�T�O                 TO ���v�T�O�v�q.
           IF ���Z�|�����p��������T�O NOT = ZERO
023850         MOVE ���Z�|�����p��������T�O   TO �����������T�O�v�q
           ELSE
025530         MOVE ���Z�|�����������T�O       TO �����������T�O�v�q
           END-IF.
024250     MOVE ���Z�|���������v�T�O           TO ���������v�T�O�v�q.
      */2022
           MOVE ���Z�|���׏����s���Z��         TO ���׏����s���Z���v�q.
           MOVE ���Z�|���׏����s���Z��         TO ���׏����s���Z���v�q.
024260*
024270****************************************
024280* �K�p���u�󋋎ҕ��S�z�̓���v�̃Z�b�g *
024290****************************************
024300*     PERFORM VARYING �񐔂b�m�s FROM 1 BY 1
024310*             UNTIL ( �񐔂b�m�s > 10 ) OR
024320*                   ( ���Z�|���������S�z(�񐔂b�m�s) = ZERO )
024330*         EVALUATE �񐔂b�m�s
024340*         WHEN 1
024350*             MOVE NC"�@"                     TO ������S�񐔂v(�񐔂b�m�s)
024360*         WHEN 2
024370*             MOVE NC"�A"                     TO ������S�񐔂v(�񐔂b�m�s)
024380*         WHEN 3
024390*             MOVE NC"�B"                     TO ������S�񐔂v(�񐔂b�m�s)
024400*         WHEN 4
024410*             MOVE NC"�C"                     TO ������S�񐔂v(�񐔂b�m�s)
024420*         WHEN 5
024430*             MOVE NC"�D"                     TO ������S�񐔂v(�񐔂b�m�s)
024440*         WHEN 6
024450*             MOVE NC"�E"                     TO ������S�񐔂v(�񐔂b�m�s)
024460*         WHEN 7
024470*             MOVE NC"�F"                     TO ������S�񐔂v(�񐔂b�m�s)
024480*         WHEN 8
024490*             MOVE NC"�G"                     TO ������S�񐔂v(�񐔂b�m�s)
024500*         WHEN 9
024510*             MOVE NC"�H"                     TO ������S�񐔂v(�񐔂b�m�s)
024520*         WHEN 10
024530*             MOVE NC"�I"                     TO ������S�񐔂v(�񐔂b�m�s)
024540*         END-EVALUATE
024550*         MOVE ���Z�|���������S�z(�񐔂b�m�s) TO ���������S�z�v(�񐔂b�m�s)
024560*     END-PERFORM.
024570**
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
               MOVE �{��|�s���{���i�h�r     TO �s���{���i�h�r�v
024320         MOVE �{��|�V�_���t�ԍ� TO �_���t�ԍ��v
024750*         IF ( �{�p�a��N���v�q < �{��|�J�n�a��N�� )
024760*             PERFORM �_���t�S�p�����擾
024770*         ELSE
024780*             PERFORM �V�_���t�S�p�����擾
024790*         END-IF
024800*
024810*         MOVE �{��|�ڍ��t�����ԍ�  TO �n�b�q�ڍ��t�����ԍ��v
024820*
024830         STRING "����-"                DELIMITED BY SIZE
024840                �{��|�ڍ��t�����ԍ� DELIMITED BY SPACE
024850           INTO �ڍ��t�����ԍ��v
024860         END-STRING
024870*
024880         MOVE �{��|�X�֔ԍ��P        TO �{�p���X�֔ԍ��P�v
024890         MOVE �{��|�X�֔ԍ��Q        TO �{�p���X�֔ԍ��Q�v
024900         MOVE �{��|��\�҃J�i        TO ��\�҃J�i�v
024910         MOVE �{��|��\�Җ�          TO ��\�Җ��v
024920*
024930         MOVE �{��|�ڍ��@��          TO �ڍ��@���v
024940         MOVE �{��|�Z���P            TO �{�p���Z���P�v
024950         MOVE �{��|�Z���Q            TO �{�p���Z���Q�v
025000*
025010         MOVE �{��|�d�b�ԍ�          TO �{�p���d�b�ԍ��v
025020         MOVE �{��|�s���{���i�h�r    TO �s���{���v
025030**
025040** �U������
025050** �����s�̏�E�e�E���E�q�E�픚�ŁAJIS ����(13)�̎�
      ** ��t���̏�E�e�E���E�q�ŁAJIS ��t(12)�̎�
      ** ���茧�̓��i����s�������j�ŁAJIS ����(42)�̎�
               IF (((( ������ʂv�q = 53 ) AND ( ��p���S�Ҕԍ������v�q(1:4) = "8013" )) OR
                    (( ������ʂv�q = 52 ) AND ( ��p���S�Ҕԍ������v�q(1:4) = "8113" )) OR
                    (( ������ʂv�q = 54 ) AND ( ��p���S�Ҕԍ������v�q(1:4) = "1913" )) OR
                    (( ������ʂv�q = 60 OR 55 ) AND ( ��p���S�Ҕԍ������v�q(1:4) = "8813" )) OR
                    (( ������ʂv�q = 60 ) AND ( ��p���S�Ҕԍ������v�q(1:4) = "8913" ))) AND
                   ( �{��|�s���{���i�h�r = "13" )) OR
                  (((( ������ʂv�q = 60 OR 55 ) AND (��p���S�Ҕԍ������v�q(1:4) = "8312" )) OR
                    (( ������ʂv�q = 53 ) AND (��p���S�Ҕԍ������v�q(1:4) = "8112")) OR
                    ((������ʂv�q = 52) AND (��p���S�Ҕԍ������v�q(1:4) =  "8512"))) AND
                   ( �{��|�s���{���i�h�r = "12" )) OR
                  ((((������ʂv�q = 55) AND (��p���S�Ҕԍ������v�q(1:4) =  "8042")) AND
                    (��p���S�Ҕԍ������v�q NOT =  "80420011")) AND
                   ( �{��|�s���{���i�h�r = "42" ))
024700             MOVE ZERO             TO  ���|�_���I���敪
024690             MOVE 13               TO  ���|����R�[�h
024700             MOVE ZERO             TO  ���|�ی����
024710             MOVE �{�p�a��N���v�q TO  ���|�ύX�a��N��
024720             START ����}�X�^ KEY IS <  ���|�_���I���敪
034410                                          ���|����R�[�h
                                                ���|�ی����
                                                ���|�ύX�a��N��
034420                                          REVERSED
034430             END-START
034440             IF ( ��ԃL�[ = "00" )
034450                 MOVE SPACE  TO �I���t���O�R
034460                 READ ����}�X�^ NEXT
                       END-READ
025130                 MOVE ���|������s��      TO ������s���v
025140                 MOVE ���|������s�x�X��  TO ������s�x�X���v
025150                 MOVE ���|�a�����          TO �a����ʂv
025160*                 MOVE ���|�����ԍ�          TO �����ԍ��v
                       MOVE "6639305"               TO �����ԍ��v
024780                 MOVE ���|�������`�l        TO �������`�l�v
025180                 MOVE ���|�������`�l�J�i    TO �������`�l�J�i�v
025370             END-IF
025380* �Œ��
025080             MOVE "�܂��A�����擾������L���z�̎�̂��A" TO  �R�����g�P�v
      */�ߘa�T�N�W����o����藝�����ύX/20230628
025090*             MOVE "(��)�����ڍ��t����� ���� �i"       TO  �R�����g�Q�v
025090             MOVE "(��)�����ڍ��t����� ���� ��"       TO  �R�����g�Q�v
025100             MOVE "(�����s���搬��2-9-5)�ɈϔC"        TO  �R�����g�R�v
025100             MOVE "���܂��B"                             TO  �R�����g�S�v
025520*
025522*
025530** �ȊO
025540         ELSE
025550             MOVE �{��|������s��      TO ������s���v
025560             MOVE �{��|������s�x�X��  TO ������s�x�X���v
025570             MOVE �{��|�a�����          TO �a����ʂv
025580             MOVE �{��|�����ԍ�          TO �����ԍ��v
025590             MOVE �{��|�������`�l        TO �������`�l�v
                   MOVE �{��|�������`�l�J�i    TO �������`�l�J�i�v
025780         END-IF
      */����͐U���̂ݑΉ�
               MOVE NC"��" TO �U���`�F�b�N�v
      *
               EVALUATE �a����ʂv
               WHEN 1
                   MOVE NC"��" TO ���ʃ`�F�b�N�v
               WHEN 2
                   MOVE NC"��" TO �����`�F�b�N�v
               END-EVALUATE
      *
009745         IF ������s���v NOT = SPACE
009746            PERFORM VARYING �J�E���^ FROM 40 BY -1
009747                      UNTIL (������s���v(�J�E���^:1) NOT = SPACE) OR
009748                            (�J�E���^ <= ZERO)
009749                CONTINUE
009750            END-PERFORM
009751            IF �J�E���^ > 4
009752               IF ������s���v(�J�E���^ - 3 : 4)  = "��s"
009753                  MOVE  ������s���v(1:�J�E���^ - 4)   TO ���Z�@�֖��v
009754                  MOVE NC"��" TO ��s�`�F�b�N�v
009755               ELSE
009756                  IF ������s���v(�J�E���^ - 3 : 4)  = "����"
009757                     MOVE  ������s���v(1:�J�E���^ - 4)   TO ���Z�@�֖��v
009758                     MOVE NC"��" TO ���Ƀ`�F�b�N�v
009759                  ELSE
009760                     IF ������s���v(�J�E���^ - 3 : 4)  = "�_��"
009761                        MOVE  ������s���v(1:�J�E���^ - 4)   TO ���Z�@�֖��v
009762                        MOVE NC"��" TO �_���`�F�b�N�v
009763                     ELSE
009764                        MOVE  ������s���v  TO ���Z�@�֖��v
009765                     END-IF
009766                  END-IF
009767               END-IF
009768            ELSE
009769               MOVE  ������s���v  TO ���Z�@�֖��v
009770            END-IF
009771         END-IF
009779*
009780         IF ������s�x�X���v NOT = SPACE
009781            PERFORM VARYING �J�E���^ FROM 40 BY -1
009782                      UNTIL (������s�x�X���v(�J�E���^:1) NOT = SPACE) OR
009783                            (�J�E���^ <= ZERO)
009784                CONTINUE
009785            END-PERFORM
009786            IF �J�E���^ >= 4
009787               IF ������s�x�X���v(�J�E���^ - 3 : 4)  = "�{�X"
009788                  MOVE  ������s�x�X���v(1:�J�E���^ - 4)   TO �x�X���v
009789                  MOVE NC"��" TO �{�X�`�F�b�N�v
009790               ELSE
009791                  IF ������s�x�X���v(�J�E���^ - 3 : 4)  = "�x�X"
009792                     MOVE  ������s�x�X���v(1:�J�E���^ - 4)   TO �x�X���v
009793                     MOVE NC"��" TO �x�X�`�F�b�N�v
009794                  ELSE
009791                     IF ������s�x�X���v(�J�E���^ - 3 : 4)  = "�x��"
009792                        MOVE  ������s�x�X���v(1:�J�E���^ - 4)   TO �x�X���v
009793                        MOVE NC"��" TO �{�x���`�F�b�N�v
009794                     ELSE
009791                         IF ������s�x�X���v(�J�E���^ - 3 : 4)  = "�{��"
009792                            MOVE  ������s�x�X���v(1:�J�E���^ - 4)   TO �x�X���v
009793                            MOVE NC"��" TO �{�x���`�F�b�N�v
009794                         ELSE
009800                             MOVE  ������s�x�X���v  TO �x�X���v
009801                         END-IF
009804                     END-IF
009805                  END-IF
009806               END-IF
009807            ELSE
009808               MOVE  ������s�x�X���v  TO �x�X���v
009809            END-IF
009810         END-IF
025790*
025800     END-READ.
025810*
025820*********************************************
025830** �h�c�Ǘ��}�X�^���@���{�p�h�c���擾����B
025840*********************************************
025850** ���{�pID
025860     MOVE 01                   TO �h�c�ǁ|�h�c�敪
025870     MOVE ZERO                 TO �h�c�ǁ|�{�p���ԍ�
025880     MOVE ��p���S�Ҕԍ������v�q(3:2)  TO �h�c�ǁ|�ی����
025890     MOVE SPACE                TO �h�c�ǁ|�ی��Ҕԍ�
025900     READ �h�c�Ǘ��}�X�^
025910     NOT INVALID KEY
025920          MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO ���{�p�h�c�v
025930     END-READ.
025940*
025950*================================================================*
025960 ��f�ҏ��擾 SECTION.
025970*
025980**************************************************
025990* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
026000* �� �{�p�N ..... �{�p�N�v�Ɋi�[                 *
026010* �� �{�p�� ..... �{�p���v�Ɋi�[                 *
026020* �� ���Ҕԍ�.... ���Ҕԍ��v�Ɋi�[���e�c�A�ԗp   *
026030* �� �L�� ....... �L���v�Ɋi�[                   *
026040* �� �ԍ� ....... �ԍ��v�Ɋi�[                   *
026050* �� �ی��Ҕԍ� . �ی��Ҕԍ��v�Ɋi�[             *
026060* �� �ی���� ... �ی���ʂv�Ɋi�[               *
026070* �� ��ی��҃J�i.��ی��҃J�i�v�Ɋi�[           *
026080* �� ��ی��Ҏ���.��ی��Ҏ����v�Ɋi�[           *
026090* �� �Z���P ......��ی��ҏZ���P�v�Ɋi�[         *
026100* �� �Z���Q ......��ی��ҏZ���Q�v�Ɋi�[         *
026110* �� ���҃J�i ....���҃J�i�v�Ɋi�[               *
026120* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
026130* �� ���Ґ��� ....�敪�ɂ��`�F�b�N��"��"���i�[ *
026140* �� ���Ҙa�� ....�a��ɂ��`�F�b�N��"��"���i�[ *
026150* �� ���ҔN ......���ҔN�v�Ɋi�[                 *
026160* �� ���Ҍ� ......���Ҍ��v�Ɋi�[                 *
026170* �� ���ғ� ......���ғ��v�Ɋi�[                 *
026180* �� ���� ........���̃}�X�^��葱���v�Ɏ擾     *
026190**************************************************
026460     MOVE �{�p�a��v�q       TO ��Q�|�{�p�a��.
026470     MOVE �{�p�N�v�q         TO ��Q�|�{�p�N.
026480     MOVE �{�p���v�q         TO ��Q�|�{�p��.
026490     MOVE ���҃R�[�h�v�q     TO ��Q�|���҃R�[�h.
026500     READ ��f�ҏ��Q�e
           INVALID KEY
              MOVE SPACE           TO ��Q�|���R�[�h
           END-READ.
026200     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
026210     MOVE �{�p�N�v�q         TO ��|�{�p�N.
026220     MOVE �{�p���v�q         TO ��|�{�p��.
026230     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
026240     READ ��f�ҏ��e
026250     INVALID KEY
026260         CONTINUE
026270*            /* ���肦�Ȃ� */
026280     NOT INVALID KEY
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
                  MOVE NC"��"        TO ���σ`�F�b�N�v
               WHEN 09
                  MOVE NC"��"        TO ���`�F�b�N�v
               WHEN 08
                  MOVE NC"��"        TO �ސE�`�F�b�N�v
               WHEN 05
                  MOVE NC"��"        TO ����`�F�b�N�v
022770         END-EVALUATE
      */�{�Ƌ敪�͂ǂꂩ�P�Ɂ�������B
               IF ��|������� = ZERO
                   MOVE NC"��" TO �P�ƃ`�F�b�N�v
               ELSE
                   MOVE NC"��" TO �Q���`�F�b�N�v
               END-IF
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
               EVALUATE ��Q�|�����{�l�Ƒ��敪
               WHEN 1
                   MOVE NC"��" TO �{�l�`�F�b�N�v
                   MOVE SPACE  TO �Ƒ��`�F�b�N�v
               WHEN 2
                   MOVE SPACE  TO �{�l�`�F�b�N�v
                   MOVE NC"��" TO �Ƒ��`�F�b�N�v
               END-EVALUATE
      */�������Z�͕K�����t��
      *         IF ( ��|�ی���� = 01 OR 08) OR
      *            ((��|������� = 54) AND (��|��p���S�Ҕԍ�����(1:2) = "19"))
                   EVALUATE ���Z�|���t����
                   WHEN 10
                       MOVE NC"��" TO �P�O���`�F�b�N�v
                   WHEN 9
                       MOVE NC"��" TO �X���`�F�b�N�v
      */�����̑O������P���͂W�����t�Ɂ�/110721
                       IF (��|�ی���� NOT = 05 ) AND (��|���ʋ敪 = 1)
                           MOVE SPACE  TO �X���`�F�b�N�v
                           MOVE NC"��" TO �W���`�F�b�N�v
                       END-IF
                   WHEN 8
                       MOVE NC"��" TO �W���`�F�b�N�v
                   WHEN 7
                       MOVE NC"��" TO �V���`�F�b�N�v
                   END-EVALUATE
      *         END-IF
      */�����C��/20190408
               MOVE ��|�{�p�a��     TO �{�p�a��v
026290         MOVE ��|�{�p�N       TO �{�p�N�v
026300         MOVE ��|�{�p��       TO �{�p���v
026310         MOVE ��|���Ҕԍ�     TO ���Ҕԍ��v
026320*         MOVE ��|�L��         TO �L���v
026330*         MOVE ��|�ԍ�         TO �ԍ��v
      *-----------------------------------------------------------------*
               MOVE SPACE TO �A�Í������|�Í����
      *
      *        / �A�Í������|���͏��Z�b�g /
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
      *-----------------------------------------------------------------*
026340         MOVE ��|�ی��Ҕԍ�   TO �ی��Ҕԍ��v
026350         MOVE ��|�ی����     TO �ی���ʂv
026360** �S���y�؂̎}�ԍ폜
026370         IF ( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(1:6) = "133033" )
026380            MOVE ��|�ی��Ҕԍ�(1:6)  TO �ی��Ҕԍ��v
026390         END-IF
026400**
026410         MOVE ��|��ی��҃J�i TO ��ی��҃J�i�v
026420         MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ����v
026450         MOVE ��|�Z���P       TO ��ی��ҏZ���P�v
026460         MOVE ��|�Z���Q       TO ��ی��ҏZ���Q�v
026470*         STRING ��|�Z���P   DELIMITED BY SPACE
026480*                ��|�Z���Q   DELIMITED BY SPACE
026490*                INTO ��ی��ҏZ���v
026500*         END-STRING
026510*         STRING ��|���ҏZ���P   DELIMITED BY SPACE
026520*                ��|���ҏZ���Q   DELIMITED BY SPACE
026530*                INTO ���ҏZ���v
026540*         END-STRING
               MOVE ��|���ҏZ���P TO ���ҏZ���P�v
               MOVE ��|���ҏZ���Q TO ���ҏZ���Q�v
026550         MOVE ��|���҃J�i     TO ���҃J�i�v
026560         MOVE ��|���Ҏ���     TO ���Ҏ����v
      */ �X�֔ԍ��E�d�b�ԍ��ǉ� /42505
               IF �s�����ԍ��v(3:2) = "23"
026430            MOVE ��|���җX�֔ԍ��P   TO �X�֔ԍ��P�v
026440            MOVE ��|���җX�֔ԍ��Q   TO �X�֔ԍ��Q�v
                  IF ��|���ғd�b�ԍ� NOT = SPACE
                     STRING "�d�b:"            DELIMITED BY SIZE
                            ��|���ғd�b�ԍ�   DELIMITED BY SPACE
                       INTO �d�b�ԍ��v
                     END-STRING
                  END-IF
               ELSE
026430            MOVE ��|�X�֔ԍ��P   TO �X�֔ԍ��P�v
026440            MOVE ��|�X�֔ԍ��Q   TO �X�֔ԍ��Q�v
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
               END-IF
026570*
026580         MOVE ��|��p���S�Ҕԍ����� TO �s�����ԍ��v
026590         MOVE ��|��v�Ҕԍ�����     TO �󋋎Ҕԍ��v
026600*
026610         EVALUATE ��|���Ґ���
026620         WHEN 1
026630             MOVE NC"�j"  TO ���ʂv
026640             MOVE NC"��"  TO �j�`�F�b�N�v
026650         WHEN 2
026660             MOVE NC"��"  TO ���ʂv
026670             MOVE NC"��"  TO ���`�F�b�N�v
026680         END-EVALUATE
026690*
026700         EVALUATE ��|���Ҙa��
026710         WHEN 1
026720             MOVE NC"����"  TO �����v
026730             MOVE NC"��"    TO �����`�F�b�N�v
026740         WHEN 2
026750             MOVE NC"�吳"  TO �����v
026760             MOVE NC"��"    TO �吳�`�F�b�N�v
026770         WHEN 3
026780             MOVE NC"���a"  TO �����v
026790             MOVE NC"��"    TO ���a�`�F�b�N�v
026800         WHEN 4
026810             MOVE NC"����"  TO �����v
026820             MOVE NC"��"    TO �����`�F�b�N�v
      */�����C��/20190408
023060         WHEN 5
                   MOVE "5��"   TO �ߘa�b�l�v
023070             MOVE NC"��"  TO �ߘa�`�F�b�N�v
026830         END-EVALUATE
026840*
      */�����C��/������20190408
029310         IF ��|���Ҙa�� > 4
037370             MOVE ��|���Ҙa��     TO ���|�����敪
037380             READ �����}�X�^
037390             NOT INVALID KEY
037400                 MOVE ���|�������� TO �����v
037410             END-READ
029330         END-IF
      */�����C��/������20190408
026850         MOVE ��|���ҔN  TO ���ҔN�v
026860         MOVE ��|���Ҍ�  TO ���Ҍ��v
026870         MOVE ��|���ғ�  TO ���ғ��v
026880*** �e�ی����
026890*         EVALUATE ��|�ی����
026900*         WHEN 02
026910*             MOVE NC"��"       TO ���`�F�b�N�v
026920*         WHEN 03
026930*             MOVE NC"��"       TO �g�`�F�b�N�v
026940*         WHEN 06
026950*             MOVE NC"��"       TO ���`�F�b�N�v
026960*         WHEN 07
026970*             MOVE NC"��"       TO �D�`�F�b�N�v
026980*         WHEN 04
026990*         WHEN 09
027000*             MOVE NC"��"       TO ���`�F�b�N�v
027010*         WHEN 01
027020*             MOVE NC"��"       TO ���`�F�b�N�v
027030*         WHEN 08
027040*             MOVE NC"��"       TO �ރ`�F�b�N�v
027030*         WHEN 05
027040*             MOVE NC"��"       TO �㍂�`�F�b�N�v
027040*             MOVE NC"��"       TO �㍂�P�v
027050*         WHEN OTHER
027060*             CONTINUE
027070*         END-EVALUATE
027080* �����Ȃ�
027090*         IF ( �{�l�Ƒ��敪�v�q = 1 )
027100*             MOVE SPACE       TO �����v
027110*         ELSE
027120*             MOVE 05          TO ���|�敪�R�[�h
027130*             MOVE ��|����    TO ���|���̃R�[�h
027140*             READ ���̃}�X�^
027150*             INVALID KEY
027160*                 MOVE SPACE    TO �����v
027170*             NOT INVALID KEY
027180*                 MOVE ���|���� TO �����v
027190*             END-READ
027200*         END-IF
027210*
027220**
027230*---  �s�����Ǝ��d�l -----*
027240* 14/10�`�@�����s�̂݁� ���ʋ敪1,2,3(����ҁj�̎��A�u�O�v���E��Ɉ�
027250*                       �e���V�l�̎��A�ی��Ҕԍ����ɂ́A�Q�V�ԍ�����
027260         IF ( ��|�{�p�a��N�� >= 41410 )
027270            IF ( ��|��p���S�Ҕԍ�����(3:2) = "13" ) AND
027280               ( �s���{���v = "13" )
027290               IF ( ��|������ = ZERO )
027300                  IF ( ��|���ʋ敪 = 1 OR 2 OR 3)
027310                     MOVE NC"�O" TO ���ʃ}�[�N�v
027320                  END-IF
027330               ELSE
027340                  MOVE ��|��p���S�Ҕԍ�  TO �ی��Ҕԍ��v
027350               END-IF
027360            END-IF
027370         END-IF
027380*
027390* 14/10�`�@���m���̂݁� 41�V�l�̕��S�����E��Ɉ�
027400*         IF ( ��|�{�p�a��N�� >= 41410 )
027410*            IF ( ��|��p���S�Ҕԍ�����(3:2) = "23" ) AND
027420*               ( ��|������� = 51 ) AND ( �s���{���v = "23" )
027473*               EVALUATE ��|�������S���Ə�
027474*               WHEN 2
027476*                  MOVE "41�V�l �Q��"   TO ���ʃR�����g�v
027477*               WHEN 3
027479*                  MOVE "41�V�l �R��"   TO ���ʃR�����g�v
027480*               WHEN OTHER
027482*                  MOVE "41�V�l �P��"   TO ���ʃR�����g�v
027483*               END-EVALUATE
027486*            END-IF
027490*         END-IF
027500*
027510*
027520     END-READ.
027530*================================================================*
027540 ��������擾 SECTION.
027550*
027560****************************************************
027570* �A���f�[�^����ی��҃}�X�^��萿������擾����B *
027580* ���ہ|��������敪=1�̏ꍇ������}�X�^���g�p   *
027590* �� ������...... �����於�̂v�Ɋi�[               *
027600* ������  �e�ی��̕ی��Җ���(�����於��)���Z�b�g����!! *
027610********************************************************
027620     MOVE �ی���ʂv�q   TO �ہ|�ی����.
027630     MOVE �ی��Ҕԍ��v�q TO �ہ|�ی��Ҕԍ�.
027640     READ �ی��҃}�X�^
027650     INVALID KEY
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
027670     NOT INVALID KEY
027680* �ЕہA���ق́u�Љ�ی��������v������
027690                 EVALUATE �ی���ʂv�q 
027700                 WHEN  02
027710                 WHEN  06
027720                     IF ( �ہ|�ڔ���敪 = 1 )
027730                        MOVE �ہ|�ی��Җ���    TO �����於�̂v
027740                     ELSE
027750                        STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
027760                               "�Љ�ی�������"  DELIMITED BY SIZE
027770                               INTO �����於�̂v
027780                        END-STRING
027790                     END-IF
027800** �g���͎x�����܂ň�
027810                 WHEN  03
027820                     STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
027830                           "���N�ی��g��"     DELIMITED BY SIZE
027840                            "  "              DELIMITED BY SIZE
027850                            �ہ|�x��������    DELIMITED BY SPACE
027860                            INTO �����於�̂v
027870                     END-STRING
027880** ���ς͎x�����܂ň�
027890                 WHEN  04
027900                     STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
027910                           "���ϑg��"         DELIMITED BY SIZE
027920                            "  "              DELIMITED BY SIZE
027930                            �ہ|�x��������    DELIMITED BY SPACE
027940                            INTO �����於�̂v
027950                     END-STRING
027960                 WHEN OTHER
027970                     MOVE �ہ|�ی��Җ���    TO �����於�̂v
027980                 END-EVALUATE
027990     END-READ.
028000*
028010****************************************************
028020*     MOVE ������ʂv�q           TO �s�|������.
028030*     MOVE �s�����ԍ��v           TO �s�|�s�����ԍ�.
028040*
028050*     READ �s�����}�X�^
028060*     INVALID KEY
028070*         MOVE SPACE              TO �����於�̂v
028080*     NOT INVALID KEY
028090*         IF ( �s�|������敪 = 1 )
028100*             MOVE ������ʂv�q     TO ����|�ی����
028110*             MOVE �s�����ԍ��v     TO ����|�ی��Ҕԍ�
028120*             READ ������}�X�^
028130*             INVALID KEY
028140*                 MOVE SPACE        TO �����於�̂v
028150*             NOT INVALID KEY
028160*                 MOVE ����|�ی��Җ���  TO �����於�̂v
028170*             END-READ
028180*         ELSE
028190*             MOVE �s�|�s��������  TO �����於�̂v
028200*         END-IF
028210*     END-READ.
028220*
027590*================================================================*
027600 �����Ǎ� SECTION.
027610*
027790     MOVE �{�p�a��v�q       TO ���|�{�p�a��.
027800     MOVE �{�p�N�v�q         TO ���|�{�p�N.
027810     MOVE �{�p���v�q         TO ���|�{�p��.
027820     MOVE ���҃R�[�h�v�q     TO ���|���҃R�[�h.
027830     READ �����f�[�^�e
027870     NOT INVALID KEY
027900         MOVE ���|���ʐ�                   TO ���ʐ��v
           END-READ.
028230*================================================================*
028240 �����f�[�^�擾 SECTION.
028250*
028260**************************************************
028270* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾 *
028280* �� ������...���ʁ{������ʂɂĉ��H���Ċi�[     *
028290* �� �����N.......�����N�v                       *
028300* �� ������.......�������v                       *
028310* �� ������.......�������v                       *
028320* �� �J�n�N.......�����N�v                       *
028330* �� �J�n��.......�������v                       *
028340* �� �J�n��.......�������v                       *
028350* �� �I���N.......�I���N�v                       *
028360* �� �I����.......�I�����v                       *
028370* �� �I����.......�I�����v                       *
028380* �� ������.......�������v                       *
028390* �� �]�A�敪 ....�敪�ɂ��`�F�b�N��"��"���i�[ *
028400* �� �������q ....�敪�ɂ��`�F�b�N��"��"���i�[ *
028410* �� �o�߃R�[�h...�o�߃}�X�^���擾             *
028420**************************************************
028430*     MOVE �{�p�a��v�q       TO ���|�{�p�a��.
028440*     MOVE �{�p�N�v�q         TO ���|�{�p�N.
028450*     MOVE �{�p���v�q         TO ���|�{�p��.
028460*     MOVE ���҃R�[�h�v�q     TO ���|���҃R�[�h.
028470*     READ �����f�[�^�e
028480*     INVALID KEY
028490*         CONTINUE
028500**            /* ���肦�Ȃ� */
028510*     NOT INVALID KEY
028520*         MOVE ���|���ʐ�                   TO ���ʐ��v
028530         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
028540                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
028550             MOVE ���|�������(���ʂb�m�s) TO ������ʂv(���ʂb�m�s)
028560             MOVE ���|����(���ʂb�m�s)     TO ���ʂv(���ʂb�m�s)
028570             MOVE ���|���E�敪(���ʂb�m�s) TO ���E�敪�v(���ʂb�m�s)
028580             MOVE ���|�����ʒu�ԍ�(���ʂb�m�s)
028590                                           TO �����ʒu�ԍ��v(���ʂb�m�s)
028600*********************************************
028610* ���j�S�_...������ʁ{���ʂɂĉ��H���Ċi�[ *
028620*********************************************
028630* �������
028640             MOVE SPACE                     TO �������̂v
028650             MOVE 03                        TO ���|�敪�R�[�h
028660             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
028670             READ ���̃}�X�^
028680             INVALID KEY
028690                 MOVE SPACE        TO �������̂v
028700             NOT INVALID KEY
028710                 MOVE ���|�������� TO �������̂v
028720             END-READ
028730* ����
020710             MOVE SPACE                    TO �������v(���ʂb�m�s)
028160*
028170             PERFORM ���ʖ��̖�������
028920*
028930             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
028940             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
028950             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
028960             MOVE ���|�J�n�N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
028970             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
028980             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
028990             IF ( ���|�]�A�敪(���ʂb�m�s) = 9 )
029000                 MOVE 99                   TO �I���N�v(���ʂb�m�s)
029010                 MOVE 99                   TO �I�����v(���ʂb�m�s)
029020                 MOVE 99                   TO �I�����v(���ʂb�m�s)
029030             ELSE
029040                 MOVE ���|�I���N(���ʂb�m�s)   TO �I���N�v(���ʂb�m�s)
029050                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
029060                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
029070             END-IF
029080* �o�ߗ��̎擾
029090             MOVE 01                         TO �o�|�敪�R�[�h
029100             MOVE ���|�o�߃R�[�h(���ʂb�m�s) TO �o�|�o�߃R�[�h
029110             READ �o�߃}�X�^
029120             INVALID KEY
029130                 MOVE ZERO            TO ���ʂb�m�s�v(���ʂb�m�s)
029140                 MOVE SPACE           TO ���ʋ�؂v(���ʂb�m�s)
029150                 MOVE SPACE           TO �o�ߗ��̂v(���ʂb�m�s)
029160             NOT INVALID KEY
029170*
029180                 EVALUATE ���ʂb�m�s
029190                 WHEN 1
029200                     MOVE NC"�@" TO �o�ߕ��ʂv
029210                 WHEN 2
029220                     MOVE NC"�A" TO �o�ߕ��ʂv
029230                 WHEN 3
029240                     MOVE NC"�B" TO �o�ߕ��ʂv
029250                 WHEN 4
029260                     MOVE NC"�C" TO �o�ߕ��ʂv
029270                 WHEN 5
029280                     MOVE NC"�D" TO �o�ߕ��ʂv
029290                 END-EVALUATE
029300                 STRING  �o�ߕ��ʂv     DELIMITED BY SPACE
029310                         �o�|�o�ߗ���   DELIMITED BY SPACE
029320                        INTO ����o�ߗ��̂v(���ʂb�m�s)
029330                 END-STRING
029340*
029350             END-READ
029360*
029370             MOVE ���|�]�A�敪(���ʂb�m�s) TO �]�A�敪�v(���ʂb�m�s)
029380             EVALUATE ���|�]�A�敪(���ʂb�m�s)
029390             WHEN 1
029400             WHEN 2
029410                 MOVE NC"��"               TO �����`�F�b�N�v(���ʂb�m�s)
029420             WHEN 3
029430                 MOVE NC"��"               TO ���~�`�F�b�N�v(���ʂb�m�s)
029440             WHEN 4
029450                 MOVE NC"��"               TO �]��`�F�b�N�v(���ʂb�m�s)
029460             END-EVALUATE
029470*
      */�������̓��Z�|���ʎ�������]�L����/160816
031230             MOVE ���Z�|���ʎ�����(���ʂb�m�s) TO �������v(���ʂb�m�s)
      */�����p��20241007/
                   MOVE ���Z�|���ʌp������(���ʂb�m�s) TO ���ʌp�������v(���ʂb�m�s)
029480         END-PERFORM.
029490* �V�K/�p�� �`�F�b�N
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
033420         END-EVALUATE.
029550* �}�Ԕ���p
029560         MOVE ���|�J�n�f�Ó��蓮�敪 TO  �J�n�f�Ó��蓮�敪�v.
029570*
029580* ������������敪
029590         MOVE ���|���Z������������敪 TO ���Z������������敪�v.
028370* �������R����敪
027880         MOVE ���|���Z�������R����敪 TO �������R����敪�e.
029600*
029610*     END-READ.
029620*================================================================*
029630*================================================================*
029640 �{�p�L�^�擾 SECTION.
029650*
029660************************************************************
029670* ��P�f�[�^���畉���f�[�^�e���ȉ��̏����擾           *
029680* �� �������Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
029690* �� ���É��Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
029700************************************************************
029710     MOVE  SPACE  TO  �����Č��t���O.
029720     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���ʐ��v
029730         IF ( �{�p�N�v = �����N�v(���ʂb�m�s) ) AND
029740            ( �{�p���v = �������v(���ʂb�m�s) )
029750             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
029760             MOVE �}�Ԃv�q              TO �{�L�|�}��
029770             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
029780             MOVE �����N�v(���ʂb�m�s)  TO �J�n�N�v(���ʂb�m�s) �{�L�|�{�p�N
029790             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
029800             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
029810         ELSE
029820             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
029830             MOVE �}�Ԃv�q              TO �{�L�|�}��
029840             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
029850             MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
029860             MOVE �{�p���v�q            TO �{�L�|�{�p��
029870             MOVE ZERO                  TO �{�L�|�{�p��
029880         END-IF
      *------------------------------------------------------------------------*
               IF ( �A���|�ی���� > 50 ) AND ( ���Z�|�������r���Ώ� = 1 )
                  IF �J�n���v(���ʂb�m�s) < ��|�������r���J�n��
                     MOVE ��|�������r���J�n��  TO  �J�n���v(���ʂb�m�s) �{�L�|�{�p��
                  END-IF
               END-IF
      *------------------------------------------------------------------------*
029890         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
029900                                      �{�L�|�{�p�a��N����
029910         END-START
029920         IF ( ��ԃL�[ = "00" )
      */�������̓��Z�|���ʎ�������]�L����/160816
029930*             MOVE ZERO  TO �������v(���ʂb�m�s)
029940             MOVE ZERO  TO �I���N�v�s
029950             MOVE ZERO  TO �I�����v�s
029960             MOVE ZERO  TO �I�����v�s
029970             MOVE SPACE TO �I���t���O�Q
029980             PERFORM �{�p�L�^�e�Ǎ�
029990             IF ( �I���t���O�Q      = SPACE   ) AND
030000                ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
030010                ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
030020                ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
030030                ( �{�L�|�{�p��      = �{�p���v�q     ) 
030040*
030050*        *****************************************************************
030060*        * �J�n�N���� ( ���̕��ʂ����������łȂ����A
030070*                       ���������ł��}�Ԃ����鎞�́A�ŏ��̎{�p�����J�n��)*
030080*        *****************************************************************
030090                 IF ( �{�p�N�v NOT = �����N�v(���ʂb�m�s) ) OR
030100                    ( �{�p���v NOT = �������v(���ʂb�m�s) ) OR
030110                    ( �J�n�f�Ó��蓮�敪�v = 1 )
030120                     MOVE �{�L�|�{�p�N   TO �J�n�N�v(���ʂb�m�s)
030130                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
030140                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
030150                 END-IF
030160             END-IF
030170             PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
030180                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
030190                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
030200                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
030210                           ( �{�L�|�{�p��     NOT = �{�p���v�q       ) OR
030220                           ( �{�L�|�{�p��         > �I�����v(���ʂb�m�s))
030230*               **********
030240*               * ������ *
030250*               **********
      */�������̓��Z�|���ʎ�������]�L����/160816
030260*                COMPUTE �������v(���ʂb�m�s) = �������v(���ʂb�m�s) + 1
030270                MOVE �{�L�|�{�p�N               TO �I���N�v�s
030280                MOVE �{�L�|�{�p��               TO �I�����v�s
030290                MOVE �{�L�|�{�p��               TO �I�����v�s
030300*
030310                PERFORM �{�p�L�^�e�Ǎ�
030320            END-PERFORM
030330        END-IF
030340*       **************************
030350*       * �p���F�I���N�����Z�b�g *
030360*       **************************
030370        IF ( �]�A�敪�v(���ʂb�m�s) = 9 )
030380            MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
030390            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
030400            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
030410        END-IF
030420        IF ( �I���N�����v(���ʂb�m�s) > �󗝔N�����v )
030430            MOVE �I���N�v(���ʂb�m�s) TO �󗝔N�v
030440            MOVE �I�����v(���ʂb�m�s) TO �󗝌��v
030450            MOVE �I�����v(���ʂb�m�s) TO �󗝓��v
030460        END-IF
030470     END-PERFORM.
030480*
030490** ----- �O�������݂̂��𔻒� -----------*
030500*
030510*     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
030520*     MOVE �}�Ԃv�q              TO �{�L�|�}��.
030530*     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
030540*     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
030550*     MOVE �{�p���v�q            TO �{�L�|�{�p��.
030560*     MOVE ZERO                  TO �{�L�|�{�p��.
030570*     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
030580*                                  �{�L�|�{�p�a��N����
030590*     END-START.
030600*     IF ( ��ԃL�[ = "00" )
030610*             MOVE SPACE TO �I���t���O�Q
030620*             PERFORM �{�p�L�^�e�Ǎ�
030630*             IF ( �I���t���O�Q      = SPACE   ) AND
030640*                ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
030650*                ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
030660*                ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
030670*                ( �{�L�|�{�p��      = �{�p���v�q     ) 
030680** �����{�p�J�n�����Č����ǂ�������
030690*                 IF ( �{�L�|�Č������� = 1 )
030700*                      MOVE "YES"  TO  �����Č��t���O
030710*                 END-IF
030720**
030730*             END-IF
030740*     END-IF.
030750*     IF ( �����Č��t���O = "YES" )
030760*        PERFORM �O�������̂ݔ���
030770*     END-IF.
030780*
030790*================================================================*
030800*================================================================*
030810 ���Z�v�g���я��擾 SECTION.
030820*================================================================*
030830     MOVE �{�p�a��v�q       TO ��S�|�{�p�a��.
030840     MOVE �{�p�N�v�q         TO ��S�|�{�p�N.
030850     MOVE �{�p���v�q         TO ��S�|�{�p��.
030860     MOVE ���҃R�[�h�v�q     TO ��S�|���҃R�[�h.
030870     MOVE ������ʂv�q       TO ��S�|�ی����.
030880     READ ��ƃt�@�C���S
030890     NOT INVALID KEY
030900          MOVE NC"��"        TO ���ԌŒ�v
030910          MOVE ��S�|����    TO ���Ԃv
030920     END-READ.
030930*
030940*================================================================*
030950*================================================================*
030960* �n�b�q���擾 SECTION.
030970*
030980****************************************************************
030990* ��     �ڍ��t�����ԍ� .....�{�p�����}�X�^�����Ɏ擾    *
031000* ��     �{�p�N��         .....��f�ҏ��e�����Ɏ擾        *
031010* �� �@�@�ی����         .....�S�_�p���ۃR�[�h42���Z�b�g    �@*
031020*                              �V�K�}�X�^�ɂ��Ή��̉\���L��*
031030* ��     �w��e�Ђh�c     .....���Ђh�c"21"���Z�b�g            *
031040* ��     �e�c�A��         .....�����Q��                        *
031050* �� �@�@�������z         .....���Z�|�������z���Z�b�g        *
031060* ���@�@ ���S����         .....���S���� = ���Z�|���S���� / 10�@*
031070****************************************************************
031080* �ڍ��t�����ԍ�
031090*     MOVE �n�b�q�ڍ��t�����ԍ��v TO �n�b�q����ԍ��v.
031100* �{�p�N��
031110*     MOVE �{�p�N���v         TO �n�b�q�{�p�N���v.
031120* �ی���� = �S�_�R�[�h = 42
031130*     MOVE 42                 TO �n�b�q�ی���ʂv.
031140* �w��e�Ђh�c = ���Ђh�c = 21
031150*     MOVE "21"               TO �n�b�q�e�Ђh�c�v.
031160* �e�c�A�� = �e�c�A�Ԋ��Ҕԍ��v + �e�c�A�Ԍ��ۂh�c�v
031170*     MOVE ���Ҕԍ��v         TO �e�c�A�Ԋ��Ҕԍ��v.
031180*     MOVE 1                  TO �e�c�A�Ԍ��ۂh�c�v.
031190* �������z
031200*     MOVE ���Z�|�����z       TO �n�b�q�������z�v.
031210* ���S����
031220*     MOVE ���Z�|���S��     TO ���S�����v�q.
031230*     COMPUTE �n�b�q���S�����v = ���S�����v�q / 10.
031240*================================================================*
031250 �{�p�L�^�e�Ǎ� SECTION.
031260*
031270     READ �{�p�L�^�e NEXT
031280     AT END
031290         MOVE "YES" TO �I���t���O�Q
031300     END-READ.
031310*================================================================*
031320 ������� SECTION.
031330*
031340     MOVE "YCH6427P" TO  ��`�̖��o.
031350     MOVE "SCREEN"   TO  ���ڌQ���o.
031360     WRITE YCH6427P.
031370***     WRITE ������R�[�h.
031380     PERFORM �G���[�����o.
031390*================================================================*
031400 �G���[�����o SECTION.
031410*
031420     IF ( �ʒm���o NOT = "00" )
031430         DISPLAY NC"���[�G���["              UPON CONS
031440         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
031450         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
031460         DISPLAY NC"�g������o�F" �g������o UPON CONS
031470         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
031480                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
031490         ACCEPT  �L�[���� FROM CONS
031500         PERFORM �t�@�C����
031510         MOVE 99 TO PROGRAM-STATUS
031520         EXIT PROGRAM
031530     END-IF.
031540*================================================================*
031550 ���ʖ��̖������� SECTION.
031560*
006490     STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
009980            �������̂v                    DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
006520       INTO �������v(���ʂb�m�s)
006570     END-STRING.
031760*
031770*================================================================*
031780 �������ȑO�̃f�[�^���� SECTION.
031790*
031800*********************************************************************************
031810*  �ŏ��̏������ȑO�̓������Ɏ{�p�L�^���R�[�h����������(�����A���~)�́A�����敪��
031820*  �p���ɂ��`�F�b�N����B(�V�K�ƌp���̗���)
031830*********************************************************************************
031840** �ŏ��̏��������擾
031850     MOVE SPACE                 TO �����t���O.
031860     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
031870     MOVE �}�Ԃv�q              TO �{�L�|�}��.
031880     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
031890     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
031900     MOVE �{�p���v�q            TO �{�L�|�{�p��.
031910     MOVE ZERO                  TO �{�L�|�{�p��.
031920     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
031930                                  �{�L�|�{�p�a��N����
031940     END-START.
031950     IF ( ��ԃL�[ = "00" )
031960         MOVE ZERO  TO �����a��v�s
031970         MOVE ZERO  TO �����N�v�s
031980         MOVE ZERO  TO �������v�s
031990         MOVE ZERO  TO �������v�s
032000         MOVE SPACE TO �I���t���O�Q
032010         PERFORM �{�p�L�^�e�Ǎ�
032020         PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
032030                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
032040                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
032050                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
032060                       ( �{�L�|�{�p��     NOT = �{�p���v�q      ) OR
032070                       ( �����t���O           = "YES"           ) 
032080               IF ( �{�L�|�f�Ë敪 = 2 )
032090                   MOVE �{�L�|�{�p�a��           TO �����a��v�s
032100                   MOVE �{�L�|�{�p�N             TO �����N�v�s
032110                   MOVE �{�L�|�{�p��             TO �������v�s
032120                   MOVE �{�L�|�{�p��             TO �������v�s
032130                   MOVE "YES"                    TO �����t���O
032140               END-IF
032150               PERFORM �{�p�L�^�e�Ǎ�
032160         END-PERFORM
032170     END-IF.
032180*
032190* �������ȑO�̃f�[�^����
032200     IF ( �����t���O = "YES" )
032210        MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
032220        MOVE �}�Ԃv�q              TO �{�L�|�}��
032230        MOVE �����a��v�s          TO �{�L�|�{�p�a��
032240        MOVE �����N�v�s            TO �{�L�|�{�p�N
032250        MOVE �������v�s            TO �{�L�|�{�p��
032260        MOVE �������v�s            TO �{�L�|�{�p��
032270        START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
032280                                     �{�L�|�{�p�a��N����
032290                                     REVERSED
032300        END-START
032310        IF ( ��ԃL�[ = "00" )
032320           MOVE SPACE  TO �I���t���O�Q
032330           PERFORM �{�p�L�^�e�Ǎ�
032340           IF ( �I���t���O�Q    = SPACE        ) AND
032350              ( �{�L�|���Ҕԍ�  = ���Ҕԍ��v�q ) AND
032360              ( �{�L�|�}��      = �}�Ԃv�q     ) AND
032370              ( �{�L�|�{�p�a��  = �����a��v�s ) AND
032380              ( �{�L�|�{�p�N    = �����N�v�s   ) AND
032390              ( �{�L�|�{�p��    = �������v�s   )
032400*  �������ȑO�̓������Ɏ{�p�L�^���R�[�h����������
032410                IF ( �p���`�F�b�N�v = SPACE )
032420                   MOVE NC"��"    TO �p���`�F�b�N�v
032430                END-IF
032440           END-IF
032450         END-IF
032460     END-IF.
032470*
032480*================================================================*
032490 ��������擾 SECTION.
032500*
032510* �R�J���ȏ�̒�������� "CHOUKI" ���Ă�. 
032520     MOVE  SPACE TO  �A���ԁ|�L�[.
032530     INITIALIZE      �A���ԁ|�L�[.
032540     MOVE �{�p�a��v�q  TO  �A���ԁ|�{�p�a��.
032550     MOVE �{�p�N�v�q    TO  �A���ԁ|�{�p�N.
032560     MOVE �{�p���v�q    TO  �A���ԁ|�{�p��.
032570     MOVE ���Ҕԍ��v�q  TO  �A���ԁ|���Ҕԍ�.
032580     MOVE �}�Ԃv�q      TO  �A���ԁ|�}��.
032590*
032600     CALL   "CHOUKI".
032610     CANCEL "CHOUKI".
032620*
032630**** �K�p�P���g�p (�u�O�������̂݁v�����鎞�́A��������)
032640     IF ( �A���ԁ|�Ώۃt���O  = "YES" )
032650        IF ( �K�p�P�v  = SPACE )
032660           MOVE NC"�������{�p�p�����R���ʂɋL��"  TO �K�p�P�v
032670        ELSE
032680           STRING �K�p�P�v           DELIMITED BY SPACE
032690                  NC"�C"             DELIMITED BY SIZE
032700                  NC"�������{�p�p�����R���ʂɋL��"   DELIMITED BY SIZE
032710                  INTO �K�p�P�v
032720           END-STRING
032730        END-IF
032740     END-IF.
032750*
032760*================================================================*
032770 �������Z�����擾 SECTION.
032780*****************************************************************
032790** �������Z�����ԊO�Ɛ[��̎��A�K�p�Ɂu��t���ԁv���󎚂���B
032800**   �����̈󎚂͌�3��܂ŉ\
032810*****************************************************************
032820     IF ( ���Z�|���ԊO = 1 ) OR ( ���Z�|�[�� = 1 ) OR ( ���Z�|�x�� = 1 )
032830*
032840         MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
032850         MOVE �}�Ԃv�q              TO �{�L�|�}��
032860         MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
032870         MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
032880         MOVE �{�p���v�q            TO �{�L�|�{�p��
032890         MOVE ZERO                  TO �{�L�|�{�p��
032900         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
032910                                      �{�L�|�{�p�a��N����
032920         END-START
032930         IF ( ��ԃL�[ = "00" )
032940             MOVE ZERO  TO �������Z�J�E���g
032950             MOVE SPACE TO �I���t���O�Q
032960             PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
032970                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
032980                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
032990                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
033000                           ( �{�L�|�{�p��     NOT = �{�p���v�q      ) 
033010               IF ( �{�L�|�������Z = 1 OR 2 OR 3 ) AND ( �{�L�|�f�Ë敪 = 2 )
033020                  COMPUTE �������Z�J�E���g = �������Z�J�E���g  + 1
033030                  IF ( �������Z�J�E���g <= 3 )
033040                     MOVE �{�L�|�������Z TO �������Z�敪�v�s(�������Z�J�E���g)
033050                     MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
033060                     MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
033070                  END-IF
033080               END-IF
033090               PERFORM �{�p�L�^�e�Ǎ�
033100            END-PERFORM
033110** �������Z�̎�����K�p�ɃZ�b�g
033380            IF ( �������Z���v�s(1) NOT = ZERO ) OR ( �������Z���v�s(1) NOT = ZERO ) 
                      MOVE �������Z���v�s(1) TO �������Z���v
                      MOVE ":"               TO �������Z��؂v
                      MOVE �������Z���v�s(1) TO �������Z���v
                  END-IF
033380            IF ( �������Z���v�s(2) NOT = ZERO ) OR ( �������Z���v�s(2) NOT = ZERO ) 
031910                PERFORM �������Z�K�p�Z�b�g
                  END-IF
033130         END-IF
033140*
033150     END-IF.
033160*
033170*================================================================*
033180 �������Z�K�p�Z�b�g SECTION.
033190*
033200     PERFORM VARYING �ԍ��J�E���^ FROM 1 BY 1
033210              UNTIL  �ԍ��J�E���^ > 3
033220         IF ( �������Z���v�s(�ԍ��J�E���^)  = ZERO )  AND 
033230            ( �������Z���v�s(�ԍ��J�E���^)  = ZERO ) 
033240             CONTINUE
033250         ELSE
033260* �Œ荀��
033270             EVALUATE �������Z�敪�v�s(�ԍ��J�E���^) 
033280             WHEN 1
033290                MOVE NC"���ԊO"   TO ���Z���e�v(�ԍ��J�E���^)
033320             WHEN 2
033330                MOVE NC"�x�@��"   TO ���Z���e�v(�ԍ��J�E���^)
033300             WHEN 3
033310                MOVE NC"�[�@��"   TO ���Z���e�v(�ԍ��J�E���^)
033320             END-EVALUATE
033330*
033340             MOVE NC"�F"          TO ���Z��؂v(�ԍ��J�E���^)
033350             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
033360             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
033370*
033380**** ���������{��ϊ�
033390* ����
033400             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
033410             IF ( �����v >= 10 )
033420                 MOVE �����v�P    TO �����ԍ��v�P
033430                 PERFORM ���{��ϊ�
033440                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
033450                 MOVE �����v�Q    TO �����ԍ��v�P
033460                 PERFORM ���{��ϊ�
033470                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
033480             ELSE
033490                 MOVE �����v�Q    TO �����ԍ��v�P
033500                 PERFORM ���{��ϊ�
033510                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
033520             END-IF
033530* ��
033540             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
033550             MOVE �����v�P    TO �����ԍ��v�P
033560             PERFORM ���{��ϊ�
033570             MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
033580             MOVE �����v�Q    TO �����ԍ��v�P
033590             PERFORM ���{��ϊ�
033600             MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
033610** 
033620        END-IF
033630     END-PERFORM.
033640*
033650     MOVE  �������Z�W�c�m�v(1)   TO �������Z�����P�v. 
033660     MOVE  �������Z�W�c�m�v(2)   TO �������Z�����Q�v. 
033670     MOVE  �������Z�W�c�m�v(3)   TO �������Z�����R�v. 
033680*
033690**** �K�p�P���Q���g�p�i�������R�L�ڂœK�p�P���g���Ă��鎞�́A�K�p�Q�j
033700     IF ( �������Z���v�s(2)  = ZERO ) AND ( �������Z���v�s(2)  = ZERO ) 
033710         CONTINUE
033720     ELSE
033730         IF ( �K�p�P�v  = SPACE )
033740               STRING NC"�������Z"       DELIMITED BY SIZE
033750                      �������Z�����P�v   DELIMITED BY SIZE
033760                      �������Z�����Q�v   DELIMITED BY SIZE
033770                      �������Z�����R�v   DELIMITED BY SIZE
033780                      INTO �K�p�P�v
033790               END-STRING
033800         ELSE
033810               STRING NC"�������Z"       DELIMITED BY SIZE
033820                      �������Z�����P�v   DELIMITED BY SIZE
033830                      �������Z�����Q�v   DELIMITED BY SIZE
033840                      �������Z�����R�v   DELIMITED BY SIZE
033850                      INTO �K�p�Q�v
033860               END-STRING
033870         END-IF
033880     END-IF.
033890*
033900*================================================================*
033910 ���{��ϊ� SECTION.
033920*
033930     MOVE NC"�O"     TO �S�p�����ԍ��v.
033940     CALL "htoz" WITH C LINKAGE
033950                        USING �����ԍ��v�P �S�p�����ԍ��v�P.
033960*
033970*================================================================*
033980*================================================================*
033990 ���������擾 SECTION.
034000*
034010********************************************************************
034020*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
034030*  ��: �@�A �Ƃœ]��.
034040*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
034050*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
034060********************************************************************
034070     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
034080     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
034090             UNTIL ( ���ʂb�m�s > ���ʐ��v )
034100*
034110****        IF ( ���|�������Ҕԍ�(���ʂb�m�s)  NOT = ZERO )  AND
034120        IF ( ���|�����A��(���ʂb�m�s)      NOT = ZERO )
034130*
034140           IF ( �J�E���^ = ZERO )
034150               MOVE 1   TO  �J�E���^ �J�E���^�Q
034160               MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
034170               MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
034180               MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
034190           ELSE
034200              IF ( ���|�������Ҕԍ�(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
034210                 ( ���|�����A��(���ʂb�m�s)      = �����A�Ԃb�v     )
034220                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
034230                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
034240              ELSE
034250                 COMPUTE �J�E���^ = �J�E���^  +  1
034260                 MOVE 1   TO  �J�E���^�Q
034270                 MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
034280                 MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
034290                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
034300              END-IF
034310           END-IF
034320        END-IF
034330     END-PERFORM.
034340**************************************************************************
034350*  ���������}�X�^��蕶�͎擾
034360**************************************************************************
034370     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
034380     PERFORM VARYING �J�E���^ FROM 1 BY 1
034390             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
034400** ���ۂ� �敪 01
034410         MOVE 01                        TO �����|�敪�R�[�h
034420         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
034430         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
034440         READ ���������e
034450         NOT INVALID KEY
034460             INITIALIZE ���������v�s
034470             MOVE �����|���������b�l(1) TO  ���������P�v�s
034480             MOVE �����|���������b�l(2) TO  ���������Q�v�s
034490             MOVE �����|���������b�l(3) TO  ���������R�v�s
034500             MOVE �����|���������b�l(4) TO  ���������S�v�s
034510             MOVE �����|���������b�l(5) TO  ���������T�v�s
034520             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
034530                     UNTIL ( �J�E���^�Q > 9 )  OR 
034540                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
034550                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
034560                WHEN 1
034570                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034580                WHEN 2
034590                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034600                WHEN 3
034610                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034620                WHEN 4
034630                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034640                WHEN 5
034650                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034620                WHEN 6
034630                   MOVE "�E"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034640                WHEN 7
034650                   MOVE "�F"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034660                WHEN OTHER
034670                   CONTINUE
034680                END-EVALUATE
034690             END-PERFORM
034700*
034782             IF �����|�����������͋敪 = 1
034783                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
034784                        ���������P�v�s  DELIMITED BY SIZE
034785                        ���������Q�v�s  DELIMITED BY SIZE
034786                        ���������R�v�s  DELIMITED BY SIZE
034787                        ���������S�v�s  DELIMITED BY SIZE
034788                        ���������T�v�s  DELIMITED BY SIZE
034789                        INTO �����������e�����v(�J�E���^)
034790                 END-STRING
034791             ELSE
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
034800             END-IF
034801*
034802         END-READ
034803     END-PERFORM.
034810*
034820     PERFORM ���������Z�b�g.
034830*
034840*================================================================*
034850 ���������Z�b�g SECTION.
034860*
034870**************************************************************************
034880*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
034890**************************************************************************
034900     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
034910     PERFORM VARYING �J�E���^ FROM 1 BY 1
034920             UNTIL ( �J�E���^ > 9 )  OR ( �����������e�����v(�J�E���^) = SPACE )
034930*
034940          INITIALIZE �����������e�����w�v
034950          MOVE �����������e�����v(�J�E���^)   TO �����������e�����w�v
034960          IF ( �����������e�P�w�v  NOT = SPACE )
034970              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
034980              MOVE �����������e�P�w�v  TO ���������v(�J�E���^�Q)
034990          END-IF
035000          IF ( �����������e�Q�w�v  NOT = SPACE )
035010              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
035020              MOVE �����������e�Q�w�v  TO ���������v(�J�E���^�Q)
035030          END-IF
035040          IF ( �����������e�R�w�v  NOT = SPACE )
035050              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
035060              MOVE �����������e�R�w�v  TO ���������v(�J�E���^�Q)
035070          END-IF
033830          IF ( �����������e�S�w�v  NOT = SPACE )
033840              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
033850              MOVE �����������e�S�w�v  TO ���������v(�J�E���^�Q)
033860          END-IF
035080*
035090     END-PERFORM.
035100*================================================================*
035110 ������擾 SECTION.
035120*
035130* 2006/04 �ύX
035140* ������� "JOSEIMEI" ���Ă�. 
035150     MOVE SPACE TO  �A�������́|�L�[.
035160     INITIALIZE     �A�������́|�L�[.
035170     MOVE ������ʂv�q           TO �A�������́|�������.
035180     MOVE ��p���S�Ҕԍ������v�q TO �A�������́|��p���S�Ҕԍ�����.
035190*
035200     CALL   "JOSEIMEI".
035210     CANCEL "JOSEIMEI".
035220*
035230     MOVE �A�������́|�P���� TO ������v.
035240*
035250***
035500*
035510**================================================================*
035520* ���t�����`�F�b�N�擾 SECTION.
035530**
035540**** �Q�V�g��A�픚�i�R�y�A�j�̎��́A�V�l���t�`�F�b�N�Ɂ�
035550*     IF ( �����ʂv�q NOT = ZERO )  AND
035560*        ( ������ʂv�q NOT = ZERO )
      *         IF ( ��|�{�p�a��N�� < 42004 )
035570*             MOVE NC"��"   TO  �V�l���t�`�F�b�N�v 
      *         ELSE
035570*             MOVE NC"��"   TO  �㍂���t�`�F�b�N�v 
      *             MOVE NC"��"   TO  �㍂�Q�v
      **/�������{�L���A����̔픚�͖{�̂̕��S�����ɂ��`�F�b�N������/080922
      *             
      *             IF (������ʂv�q = 54) AND
      *                (��p���S�Ҕԍ������v�q(3:2) = "34" OR "42")
      *                 MOVE �{�p�a��v�q TO �A���|�{�p�a��
      *                 MOVE �{�p�N�v�q   TO �A���|�{�p�N
      *                 MOVE �{�p���v�q   TO �A���|�{�p��
      *                 MOVE ���Ҕԍ��v�q TO �A���|���Ҕԍ�
      *                 MOVE �}�Ԃv�q     TO �A���|�}��
      *                 CALL   "HUTANRIT"
      *                 CANCEL "HUTANRIT"
035600*                 COMPUTE ���S�����v�q = �A���|���ۖ{�̕��S�� / 10
035610*                 COMPUTE ���t�����v�q = 10 - ���S�����v�q
035620*                 EVALUATE  ���t�����v�q
035630*                 WHEN  7
035640*                    MOVE NC"��"   TO  ���t�V���`�F�b�N�v 
035650*                 WHEN  8
035660*                    MOVE NC"��"   TO  ���t�W���`�F�b�N�v 
035670*                 WHEN  9
035680*                    MOVE NC"��"   TO  ���t�X���`�F�b�N�v 
035690*                 WHEN  OTHER
035700*                    CONTINUE
035710*                 END-EVALUATE
      *             END-IF
      *         END-IF
035580**
035590*     ELSE
035600*         COMPUTE ���S�����v�q = �A�v�|���S�� / 10
035610*         COMPUTE ���t�����v�q = 10 - ���S�����v�q
035620*         EVALUATE  ���t�����v�q
035630*         WHEN  7
035640*            MOVE NC"��"   TO  ���t�V���`�F�b�N�v 
035650*         WHEN  8
035660*            MOVE NC"��"   TO  ���t�W���`�F�b�N�v 
035670*         WHEN  9
035680*            MOVE NC"��"   TO  ���t�X���`�F�b�N�v 
035690*         WHEN  OTHER
035700*            CONTINUE
035710*         END-EVALUATE
035720*     END-IF.
035730**
035740*================================================================*
035750* �_���t�S�p�����擾 SECTION.
035760** 
035770** 99/12���܂�
035780*     MOVE SPACE                TO ���ݕ����S�̂v.
035790*     MOVE SPACE                TO ���ݕ����v.
035800*     MOVE SPACE                TO �S�p�����v.
035810*     MOVE �{��|�_���t�ԍ�     TO ���ݕ����S�̂v.
035820*     MOVE 1                    TO �J�E���^�R.
035830**
035840*     PERFORM VARYING �J�E���^ FROM 1 BY 1
035850*             UNTIL ( �J�E���^ > 8 ) 
035860*          EVALUATE ���ݕ����S�̂P�v(�J�E���^)
035870*          WHEN "�P"
035880*              MOVE NC"�P"   TO �S�p�����P�v(�J�E���^�R)
035890*              COMPUTE �J�E���^�R = �J�E���^�R + 1
035900*          WHEN "�Q"
035910*              MOVE NC"�Q"   TO �S�p�����P�v(�J�E���^�R)
035920*              COMPUTE �J�E���^�R = �J�E���^�R + 1
035930*          WHEN "�R"
035940*              MOVE NC"�R"   TO �S�p�����P�v(�J�E���^�R)
035950*              COMPUTE �J�E���^�R = �J�E���^�R + 1
035960*          WHEN "�S"
035970*              MOVE NC"�S"   TO �S�p�����P�v(�J�E���^�R)
035980*              COMPUTE �J�E���^�R = �J�E���^�R + 1
035990*          WHEN "�T"
036000*              MOVE NC"�T"   TO �S�p�����P�v(�J�E���^�R)
036010*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036020*          WHEN "�U"
036030*              MOVE NC"�U"   TO �S�p�����P�v(�J�E���^�R)
036040*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036050*          WHEN "�V"
036060*              MOVE NC"�V"   TO �S�p�����P�v(�J�E���^�R)
036070*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036080*          WHEN "�W"
036090*              MOVE NC"�W"   TO �S�p�����P�v(�J�E���^�R)
036100*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036110*          WHEN "�X"
036120*              MOVE NC"�X"   TO �S�p�����P�v(�J�E���^�R)
036130*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036140*          WHEN "�O"
036150*              MOVE NC"�O"   TO �S�p�����P�v(�J�E���^�R)
036160*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036170*          WHEN OTHER
036180*              MOVE ���ݕ����S�̂P�v(�J�E���^)  TO ���ݕ����P�v(�J�E���^)
036190*          END-EVALUATE
036200*     END-PERFORM.
036210**
036220*     MOVE ���ݕ����v  TO  �_���t�ԍ��P�v.
036230*     MOVE �S�p�����v  TO  �_���t�ԍ��Q�v.
036240**
036250**================================================================*
036260* �V�_���t�S�p�����擾 SECTION.
036270**
036280** �V�_���t�ԍ����u�_XXX XXXX -X-X �v�ŕ����B2�߂�XXXX ����{��^�C�v�ɂ���B
036290** 2000/01����
036300**
036310*     MOVE SPACE                TO �V�_���t�ԍ��v�s.
036320*     MOVE SPACE                TO �S�p�����v.
036330**
036340*     MOVE �{��|�V�_���t�ԍ�   TO �V�_���t�ԍ��v�s.
036350*     MOVE 1                    TO �J�E���^�R.
036360**
036370*     PERFORM VARYING �J�E���^ FROM 1 BY 1
036380*             UNTIL ( �J�E���^ > 4 ) 
036390*          EVALUATE �V�_���t�ԍ��R�P�v(�J�E���^)
036400*          WHEN "1"
036410*              MOVE NC"�P"   TO �S�p�����P�v(�J�E���^�R)
036420*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036430*          WHEN "2"
036440*              MOVE NC"�Q"   TO �S�p�����P�v(�J�E���^�R)
036450*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036460*          WHEN "3"
036470*              MOVE NC"�R"   TO �S�p�����P�v(�J�E���^�R)
036480*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036490*          WHEN "4"
036500*              MOVE NC"�S"   TO �S�p�����P�v(�J�E���^�R)
036510*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036520*          WHEN "5"
036530*              MOVE NC"�T"   TO �S�p�����P�v(�J�E���^�R)
036540*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036550*          WHEN "6"
036560*              MOVE NC"�U"   TO �S�p�����P�v(�J�E���^�R)
036570*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036580*          WHEN "7"
036590*              MOVE NC"�V"   TO �S�p�����P�v(�J�E���^�R)
036600*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036610*          WHEN "8"
036620*              MOVE NC"�W"   TO �S�p�����P�v(�J�E���^�R)
036630*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036640*          WHEN "9"
036650*              MOVE NC"�X"   TO �S�p�����P�v(�J�E���^�R)
036660*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036670*          WHEN "0"
036680*              MOVE NC"�O"   TO �S�p�����P�v(�J�E���^�R)
036690*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036700*          WHEN OTHER
036710*              MOVE SPACE    TO �S�p�����P�v(�J�E���^�R)
036720*              COMPUTE �J�E���^�R = �J�E���^�R + 1
036730*          END-EVALUATE
036740*     END-PERFORM.
036750**
036760** ( �_XXX )
036770*     STRING �V�_���t�ԍ��P�v  DELIMITED BY SIZE
036780*            �V�_���t�ԍ��Q�v  DELIMITED BY SIZE
036790*            INTO �_���t�ԍ��P�v
036800*     END-STRING.
036810** ( XXXX )
036820*     MOVE �S�p�����v        TO  �_���t�ԍ��Q�v.
036830** ( -X-X )
036840*     MOVE �V�_���t�ԍ��S�v  TO  �_���t�ԍ��R�v.
036850**
036860*================================================================*
036870 �O�������̂ݔ��� SECTION.
036880*
036890*** �O���̒ʉ@�������������� 
036900     MOVE  SPACE            TO �O���t���O.
036910     MOVE ��|���҃R�[�h    TO �{�L�|���҃R�[�h.
036920     MOVE ��|�{�p�a��      TO �{�L�|�{�p�a��.
036930     MOVE ��|�{�p�N        TO �{�L�|�{�p�N.
036940     MOVE ��|�{�p��        TO �{�L�|�{�p��.
036950     MOVE 1                 TO �{�L�|�{�p��.
036960     START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
036970                                  �{�L�|�{�p�a��N����
036980                                  REVERSED
036990     END-START.
037000     IF ( ��ԃL�[ = "00" )
037010         MOVE SPACE  TO �I���t���O�Q
037020         PERFORM �{�p�L�^�e�Ǎ�
037030         IF ( �I���t���O�Q      = SPACE  ) AND
037040            ( �{�L�|���҃R�[�h  = ��|���҃R�[�h ) AND
037050            ( �{�L�|�f�Ë敪    = 2 ) 
037060*
037070            PERFORM �O������
037080**** �K�p�P���g�p
037090            IF ( �O���t���O = "YES" )
037100               MOVE NC"���O�������̂�"    TO  �K�p�P�v
037110            END-IF
037120**
037130         END-IF
037140     END-IF.
037150*
037160*================================================================*
037170 �O������  SECTION.
037180* 
037190*** �ǂݍ��񂾎{�p�L�^�̔N�����A�O�����ǂ������� (�N���̍��� 1 ��?)
037200      MOVE  SPACE  TO  �O���t���O.
037210      INITIALIZE  �v�Z�N�����v �J�n�N�����Q�v �I���N�����Q�v.
037220**
037230      MOVE ��|�{�p�a��    TO �I���a��Q�v.
037240      MOVE ��|�{�p�N      TO �I���N�Q�v.
037250      MOVE ��|�{�p��      TO �I�����Q�v.
037260      MOVE �{�L�|�{�p�a��  TO �J�n�a��Q�v.
037270      MOVE �{�L�|�{�p�N    TO �J�n�N�Q�v.
037280      MOVE �{�L�|�{�p��    TO �J�n���Q�v.
037290*
037300      EVALUATE TRUE
037310       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v = �I���N�Q�v)
037320            PERFORM  �O����r��
037330       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v NOT = �I���N�Q�v)
037340            PERFORM  �O����r�N
037350       WHEN  �J�n�a��Q�v NOT = �I���a��Q�v 
037360            PERFORM  �O����r����
037370      END-EVALUATE.
037380*
037390      IF ( �v�Z���v = 1 )
037400         MOVE  "YES"  TO  �O���t���O
037410      END-IF.
037420*
037430*================================================================*
037440 �O����r��  SECTION.
037450*
037460     IF ( �I�����Q�v >  �J�n���Q�v )
037470         COMPUTE �v�Z���v = �I�����Q�v - �J�n���Q�v
037480     ELSE
037490        MOVE ZERO TO �v�Z���v
037500     END-IF.
037510*
037520*================================================================*
037530 �O����r�N  SECTION.
037540*
037550     IF ( �I���N�Q�v >  �J�n�N�Q�v )
037560         COMPUTE �v�Z�N�v = �I���N�Q�v - �J�n�N�Q�v
037570         COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
037580     ELSE
037590        MOVE ZERO TO �v�Z���v
037600     END-IF.
037610*
037620*================================================================*
037630 �O����r����  SECTION.
037640*
037650     MOVE �J�n�a��Q�v TO ���|�����敪.
037660     READ �����}�X�^
037670     NOT INVALID KEY
037680         MOVE ���|�J�n����N TO �J�n����N�v
037690     END-READ.
037700     MOVE �I���a��Q�v TO ���|�����敪.
037710     READ �����}�X�^
037720     NOT INVALID KEY
037730         MOVE ���|�J�n����N TO �I������N�v
037740     END-READ.
037750**
037760     IF ( �J�n����N�v NOT = ZERO ) AND ( �I������N�v NOT = ZERO )
037770        COMPUTE �J�n����N�v = �J�n����N�v + �J�n�N�Q�v - 1
037780        COMPUTE �I������N�v = �I������N�v + �I���N�Q�v - 1
037790*
037800        IF ( �I������N�v =  �J�n����N�v )
037810           PERFORM  �O����r��
037820        ELSE
037830           IF ( �I������N�v >  �J�n����N�v )
037840               COMPUTE �v�Z�N�v = �I������N�v - �J�n����N�v
037850               COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
037860           ELSE
037870               MOVE ZERO TO �v�Z���v
037880           END-IF
037890        END-IF
037900     ELSE
037910        MOVE ZERO TO �v�Z���v
037920     END-IF.
037930*
038110*================================================================*
038120 ��f�҈���敪�X�V SECTION.
038130*
038140** //  ��f�ҏ��e�̈���敪�ɂP���Z�b�g���A�X�V����B//  
038150*      ( ����敪����)
038160*
038170     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
038180     MOVE �{�p�N�v�q         TO ��|�{�p�N.
038190     MOVE �{�p���v�q         TO ��|�{�p��.
038200     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
038210     READ ��f�ҏ��e
038220     NOT INVALID KEY
038230         MOVE  1  TO  ��|���Z����敪����
038240         REWRITE  ��|���R�[�h
038250         END-REWRITE
038260         IF ( ��ԃL�[ NOT = "00" )
038270            MOVE NC"��f��" TO �t�@�C����
038280            PERFORM �G���[�\��
038290         END-IF
038300     END-READ.
038310*
038320*================================================================*
038330 �������擾 SECTION.
038340*
038350     MOVE �{�p�N�v�q   TO �󗝔N�v.
038360     MOVE �{�p���v�q   TO �󗝌��v.
038370     MOVE �{�p�a��v�q TO ���|�����敪.
038380     READ �����}�X�^
038390     NOT INVALID KEY
038400         MOVE ���|�J�n����N TO �{�p����N�v
038410     END-READ.
038420     IF ( �{�p����N�v NOT = ZERO )
038430        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
038440     END-IF.
038450*
038460     EVALUATE �{�p���v�q
038470     WHEN 4
038480     WHEN 6
038490     WHEN 9
038500     WHEN 11
038510         MOVE 30 TO �󗝓��v
038520     WHEN 2
038530         DIVIDE 4 INTO �{�p����N�v GIVING    ���v
038540                                    REMAINDER �]�v
038550         END-DIVIDE
038560         IF ( �]�v = ZERO )
038570             MOVE 29 TO �󗝓��v
038580         ELSE
038590             MOVE 28 TO �󗝓��v
038600         END-IF
038610     WHEN 1
038620     WHEN 3
038630     WHEN 5
038640     WHEN 7
038650     WHEN 8
038660     WHEN 10
038670     WHEN 12
038680         MOVE 31 TO �󗝓��v
038690     WHEN OTHER
038700          CONTINUE
038710     END-EVALUATE.
038720*
038730*================================================================*
038740 �ϔC�N�����擾 SECTION.
038750*
038760** ---// �����̎󗝔N�ɂ́A�ŏI�ʉ@���������Ă���ׁA�ޔ����� //----
038770     MOVE �󗝔N�v   TO �ŏI�ʉ@�N�v.
038780     MOVE �󗝌��v   TO �ŏI�ʉ@���v.
038790     MOVE �󗝓��v   TO �ŏI�ʉ@���v.
038800***
038810* (�_���t��)
038820     EVALUATE ���Z�v�g���t�敪�v 
038830*    /  �ŏI�ʉ@�� /
038840     WHEN ZERO
038850         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
038860         MOVE �ŏI�ʉ@���v TO �_���t���v
038870         MOVE �ŏI�ʉ@���v TO �_���t���v
038880*    /  ������ /
038890     WHEN 1 
038900         PERFORM �������擾
038910         MOVE �󗝔N�v     TO �_���t�N�v
038920         MOVE �󗝌��v     TO �_���t���v
038930         MOVE �󗝓��v     TO �_���t���v
038940*    /  �󎚂Ȃ� /
038950     WHEN 9
038960         MOVE ZERO         TO �_���t�N�v
038970         MOVE ZERO         TO �_���t���v
038980         MOVE ZERO         TO �_���t���v
038990*    /  ���̑��́A�ŏI�ʉ@�� /
039000     WHEN OTHER
039010         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
039020         MOVE �ŏI�ʉ@���v TO �_���t���v
039030         MOVE �ŏI�ʉ@���v TO �_���t���v
039040     END-EVALUATE.
039050**
039060* (���ґ�)
039070     EVALUATE ���Z�v�g���ғ��t�敪�v 
039080*    /  �ŏI�ʉ@�� /
039090     WHEN ZERO
039100         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
039110         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
039120         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
039130*    /  ������ /
039140     WHEN 1 
039150         PERFORM �������擾
039160         MOVE �󗝔N�v     TO ���҈ϔC�N�v
039170         MOVE �󗝌��v     TO ���҈ϔC���v
039180         MOVE �󗝓��v     TO ���҈ϔC���v
039190*    /  �󎚂Ȃ� /
039200     WHEN 9
039210         MOVE ZERO         TO ���҈ϔC�N�v
039220         MOVE ZERO         TO ���҈ϔC���v
039230         MOVE ZERO         TO ���҈ϔC���v
039240*    /  ���̑��́A�ŏI�ʉ@�� /
039250     WHEN OTHER
039260         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
039270         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
039280         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
039290     END-EVALUATE.
039300*
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
039310*================================================================*
039311*================================================================*
039312 �n����L���� SECTION.
039313*
039314*--------------------------------------------------------*
039315*  �������F�o�ߗ��̌Œ�� (�S�_�e�o�c�敪�v 1 �g�p)
039316*  �����ȊO�̕��ʂ́A�u�����v
039317*  �����̕��ʂ́A�u�ɖ��v
039318*--------------------------------------------------------*
039319*
039320     IF �S�_�e�o�c�敪�v = 1
039321*      �܂��u�����v�Z�b�g
039322        PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
034833                 UNTIL ( ���ʂb�m�s > ���ʐ��v ) OR
                             ( ���ʂb�m�s > 5 )
039324*
039325                 EVALUATE ���ʂb�m�s
039326                 WHEN 1
039327                     MOVE NC"�@" TO �o�ߕ��ʐ����v
039328                 WHEN 2
039329                     MOVE NC"�A" TO �o�ߕ��ʐ����v
039330                 WHEN 3
039331                     MOVE NC"�B" TO �o�ߕ��ʐ����v
039332                 WHEN 4
039333                     MOVE NC"�C" TO �o�ߕ��ʐ����v
039334                 WHEN 5
039335                     MOVE NC"�D" TO �o�ߕ��ʐ����v
039336                 END-EVALUATE
039337                 MOVE SPACE TO �o�ߗ���(���ʂb�m�s)
039338                 STRING  �o�ߕ��ʐ����v   DELIMITED BY SPACE
039339                         NC"����"         DELIMITED BY SPACE
039340                        INTO �o�ߗ���(���ʂb�m�s)
039341                 END-STRING
039342        END-PERFORM
039343*
039344*      ���ɁA�R�J���ȏ�̒�������
039345        MOVE  SPACE TO  �A���ԁ|�L�[
039346        INITIALIZE      �A���ԁ|�L�[
039347        MOVE �{�p�a��v�q  TO  �A���ԁ|�{�p�a��
039348        MOVE �{�p�N�v�q    TO  �A���ԁ|�{�p�N
039349        MOVE �{�p���v�q    TO  �A���ԁ|�{�p��
039350        MOVE ���Ҕԍ��v�q  TO  �A���ԁ|���Ҕԍ�
039351        MOVE �}�Ԃv�q      TO  �A���ԁ|�}��
039352        CALL   "CHOUKI"
039353        CANCEL "CHOUKI"
039354*
039355        IF �A���ԁ|�Ώۃt���O  = "YES"
039356           PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
034833                   UNTIL ( ���ʂb�m�s > ���ʐ��v ) OR
                               ( ���ʂb�m�s > 5 )
039358*
039359               IF �A���ԁ|���Ԃv(���ʂb�m�s)  >  ZERO
039360
039361                   EVALUATE ���ʂb�m�s
039362                   WHEN 1
039363                       MOVE NC"�@" TO �o�ߕ��ʐ����v
039364                   WHEN 2
039365                       MOVE NC"�A" TO �o�ߕ��ʐ����v
039366                   WHEN 3
039367                       MOVE NC"�B" TO �o�ߕ��ʐ����v
039368                   WHEN 4
039369                       MOVE NC"�C" TO �o�ߕ��ʐ����v
039370                   WHEN 5
039371                       MOVE NC"�D" TO �o�ߕ��ʐ����v
039372                   END-EVALUATE
039373                   MOVE SPACE TO �o�ߗ���(���ʂb�m�s)
039374                   STRING  �o�ߕ��ʐ����v   DELIMITED BY SPACE
039375                           NC"�ɖ�"         DELIMITED BY SPACE
039376                          INTO �o�ߗ���(���ʂb�m�s)
039377                   END-STRING
039378               END-IF
039379           END-PERFORM
039380        END-IF
039381*
039382     END-IF.
039383*
      */����Ōo�߂����͂���ĂȂ����́A�o�߂�����/160610
           IF (��p���S�Ҕԍ������v�q(3:2) = "20")
016020        PERFORM �o�ߎ擾
           END-IF.
018770     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018790             UNTIL ( ���ʂb�m�s > 5 )
018820         MOVE ����o�ߗ��̂v(���ʂb�m�s) TO �o�ߗ���(���ʂb�m�s)
018830     END-PERFORM.
039384*
039385*================================================================*
040830 �o�ߎ擾 SECTION.
040840*
036040     MOVE  SPACE TO  �A���ԁ|�L�[.
036050     INITIALIZE      �A���ԁ|�L�[.
036060     MOVE �{�p�a��v�q  TO  �A���ԁ|�{�p�a��.
036070     MOVE �{�p�N�v�q    TO  �A���ԁ|�{�p�N.
036080     MOVE �{�p���v�q    TO  �A���ԁ|�{�p��.
036090     MOVE ���Ҕԍ��v�q  TO  �A���ԁ|���Ҕԍ�.
036100     MOVE �}�Ԃv�q      TO  �A���ԁ|�}��.
036110*
036120     CALL   "CHOUKI".
036130     CANCEL "CHOUKI".
036140*
      */�P���E�Ŗo�E�����͓]�A�ɂ�����炸�R�����ȏ�́u���ǍD�v�A
      */����ȊO�͂��ׂāu�ǍD�v�ɂ���@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@/151217
031620     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
031630             UNTIL ( ���ʂb�m�s > ���ʐ��v )
               IF  (���|�o�߃R�[�h(���ʂb�m�s) = ZERO)
028850             EVALUATE ���ʂb�m�s
028860             WHEN 1
028870                 MOVE NC"�@" TO �o�ߕ��ʂv
028880             WHEN 2
028890                 MOVE NC"�A" TO �o�ߕ��ʂv
028900             WHEN 3
028910                 MOVE NC"�B" TO �o�ߕ��ʂv
028920             WHEN 4
028930                 MOVE NC"�C" TO �o�ߕ��ʂv
028940             WHEN 5
028950                 MOVE NC"�D" TO �o�ߕ��ʂv
028960             END-EVALUATE
                   IF ���|�������(���ʂb�m�s) = 01 OR 02 OR 03
040850*              IF ( ���|�]�A�敪(���ʂb�m�s) NOT = 1 AND 2)
040900                 IF ( �A���ԁ|���Ԃv(���ʂb�m�s)  >= 3 )
040910                     MOVE NC"���ǍD" TO  �o�߂b�l
040920                 ELSE
040930                     MOVE NC"�ǍD"     TO  �o�߂b�l
040940                 END-IF
                   ELSE
040930                 MOVE NC"�ǍD"     TO  �o�߂b�l
040950*              END-IF
                   END-IF
                   MOVE SPACE      TO  ����o�ߗ��̂v(���ʂb�m�s)
028970             STRING  �o�ߕ��ʂv     DELIMITED BY SPACE
028980                     �o�߂b�l       DELIMITED BY SPACE
028990                INTO ����o�ߗ��̂v(���ʂb�m�s)
029000             END-STRING
               END-IF
           END-PERFORM.
040960*
039386*================================================================*
039387 �G���[�\�� SECTION.
039388*
039389     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
039390     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
039391     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
039392     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
039393                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
039400     ACCEPT  �L�[���� FROM CONS
039410     PERFORM �t�@�C����.
039420     EXIT PROGRAM.
039430*================================================================*
039440*================================================================*
039450 �t�@�C���� SECTION.
039460*
039470     CLOSE �ی��҃}�X�^     �����}�X�^          ���̃}�X�^
039480           ���Z�v�g�e       ������}�X�^      �{�p�����}�X�^
039490           ������}�X�^     �o�߃}�X�^          ��f�ҏ��e
039500           �{�p�L�^�e       �����f�[�^�e        ����t�@�C��
039510           ���������e       �s�����}�X�^        ��f�ҏ��Q�e
039520           ����}�X�^     �h�c�Ǘ��}�X�^      ��ƃt�@�C���S
                 �����t�@�C��.
039530*================================================================*
039540 �I������ SECTION.
039550*
039560     PERFORM �t�@�C����.
039570*================================================================*
039580*================================================================*
039590 �e�X�g�󎚏��� SECTION.
039600*
           MOVE ALL "9" TO
           �{�p�� �{�p�N �s���{���ԍ� �J�n�N�P �J�n���P �J�n���P �I���N�P �I�����P �I�����P
           �����N�P �������P �������P �����N�P �������P �������P �������P �J�n�N�Q �J�n���Q
           �J�n���Q �I���N�Q �I�����Q �I�����Q �����N�Q �������Q �������Q �����N�Q �������Q
           �������Q �������Q �J�n�N�R �J�n���R �J�n���R �I���N�R �I�����R �I�����R �����N�R
           �������R �������R �����N�R �������R �������R �������R �J�n�N�S �J�n���S �J�n���S 
           �I���N�S �I�����S �I�����S �����N�S �������S �������S �����N�S �������S �������S 
           �������S �J�n�N�T �J�n���T �J�n���T �I���N�T �I�����T �I�����T �����N�T �������T 
           �������T �����N�T �������T �������T �������T ������ ���������k�� ���Ë��� �Č��� 
           �������q���Z�� ���É� ���×� ���v �������Z�� �{�p���񋟗� ���É��Z�� �������Z�� 
           �������Z�� ���񏈒u��(1) ���񏈒u��(2) ���񏈒u��(3) ���񏈒u��(4) ���񏈒u��(5) 
           ���񏈒u�����v ��ÒP���P 
      *     ��㪖@�P�� ��㪖@�P�� �d�ÒP�� 
           ��É񐔂P ��×��P ��㪖@�񐔂P ��㪖@���P ��㪖@�񐔂P ��㪖@���P �d�É񐔂P 
           �d�×��P ���v�P �����������P ���������v�P ��ÒP���Q ��É񐔂Q ��×��Q ��㪖@�񐔂Q 
           ��㪖@���Q ��㪖@�񐔂Q ��㪖@���Q �d�É񐔂Q �d�×��Q ���v�Q �����������Q ���������v�Q
           ��ÒP���R�W ��É񐔂R�W ��×��R�W ��㪖@�񐔂R�W ��㪖@���R�W ��㪖@�񐔂R�W 
           ��㪖@���R�W �d�É񐔂R�W �d�×��R�W ���v�R�W �����ʍ����v�R�W �����������R�W 
           ���������v�R�W �����J�n���R�O �����J�n���R�O ��ÒP���R�O ��É񐔂R�O ��×��R�O 
           ��㪖@�񐔂R�O ��㪖@���R�O ��㪖@�񐔂R�O ��㪖@���R�O �d�É񐔂R�O �d�×��R�O 
           ���v�R�O �����������R�O ���������v�R�O �����J�n���S�W �����J�n���S�W ��ÒP���S�W 
           ��É񐔂S�W ��×��S�W ��㪖@�񐔂S�W ��㪖@���S�W ��㪖@�񐔂S�W ��㪖@���S�W 
           �d�É񐔂S�W �d�×��S�W ���v�S�W �����ʍ����v�S�W �����������S�W ���������v�S�W 
           �����J�n���S�O �����J�n���S�O ��ÒP���S�O ��É񐔂S�O ��×��S�O ��㪖@�񐔂S�O 
           ��㪖@���S�O ��㪖@�񐔂S�O ��㪖@���S�O �d�É񐔂S�O �d�×��S�O ���v�S�O 
           �����������S�O ���������v�S�O ���v �ꕔ���S�� ���S���� �������z �󗝔N �󗝌� �󗝓� 
           �ϔC�N �ϔC�� �ϔC�� ���׏����s���Z�� ���׏����s���Z��
           ������(1) ������(2) ������(3) ��(1) ��(2) ��(3) ������(1) ������(2) ������(3)
           �^����×� ������ �^���� �^����(1) �^����(2) �^����(3) �^����(4) �^����(5)
           .
      *
           MOVE ALL "X" TO 
           ����S�Ҕԍ� �󋋎Ҕԍ� ���{�p�h�c �L���ԍ� �Z���P �Z���Q
           �������`�l�J�i �������`�l �_���t�ԍ� �����ԍ� �ی��Җ��� �{�p���X�֔ԍ��P  
           �{�p���X�֔ԍ��Q �{�p���Z���P �{�p���Z���Q �{�p���d�b�ԍ� ��\�҃J�i �ڍ��t�����ԍ�
           .
      *
           MOVE ALL NC"�m" TO
           �������P �������Q �������R �������S �������T �o�ߗ���(1) ������
           �o�ߗ���(2) �o�ߗ���(3) �o�ߗ���(4) �o�ߗ���(5) �K�p�P �K�p�Q ���ʂT�K�p 
           .
      *
           MOVE ALL "�m" TO
           ��ی��Ҏ��� ���Ҏ��� ��\�Җ� �ڍ��@�� �������q �����p��
           �������R���P �������R���Q �������R���R �������R���S �������R���T �������R���U
           �������R���V �������R���W ���������P ���������Q ���������R ���������S ���������T ���������U
           .
      *
           MOVE NC"��" TO
           ���ʃ`�F�b�N �U���`�F�b�N �����`�F�b�N �{�X�`�F�b�N �x�X�`�F�b�N �{�x���`�F�b�N 
           ��s�`�F�b�N ���Ƀ`�F�b�N �_���`�F�b�N �{�p���`�F�b�N�P �{�p���`�F�b�N�Q 
           �{�p���`�F�b�N�R �{�p���`�F�b�N�S �{�p���`�F�b�N�T �{�p���`�F�b�N�U �{�p���`�F�b�N�V 
           �{�p���`�F�b�N�W �{�p���`�F�b�N�X �{�p���`�F�b�N�P�O �{�p���`�F�b�N�P�P �{�p���`�F�b�N�P�Q 
           �{�p���`�F�b�N�P�R �{�p���`�F�b�N�P�S �{�p���`�F�b�N�P�T �{�p���`�F�b�N�P�U 
           �{�p���`�F�b�N�P�V �{�p���`�F�b�N�P�W �{�p���`�F�b�N�P�X �{�p���`�F�b�N�Q�O 
           �{�p���`�F�b�N�Q�P �{�p���`�F�b�N�Q�Q �{�p���`�F�b�N�Q�R �{�p���`�F�b�N�Q�S 
           �{�p���`�F�b�N�Q�T �{�p���`�F�b�N�Q�U �{�p���`�F�b�N�Q�V �{�p���`�F�b�N�Q�W 
           �{�p���`�F�b�N�Q�X �{�p���`�F�b�N�R�O �{�p���`�F�b�N�R�P �[��`�F�b�N ���ԊO�`�F�b�N 
           �x���`�F�b�N �Œ藿�`�F�b�N �������`�F�b�N �{�×��`�F�b�N ��ԃ`�F�b�N �\���J��`�F�b�N 
           ��H�`�F�b�N �����`�F�b�N�P ���~�`�F�b�N�P �]��`�F�b�N�P �ߘa�`�F�b�N
           �����`�F�b�N�Q ���~�`�F�b�N�Q �]��`�F�b�N�Q �����`�F�b�N�R ���~�`�F�b�N�R �]��`�F�b�N�R 
           �����`�F�b�N�S ���~�`�F�b�N�S �]��`�F�b�N�S �����`�F�b�N�T ���~�`�F�b�N�T �]��`�F�b�N�T 
           �V�K�`�F�b�N �p���`�F�b�N �j�`�F�b�N �����`�F�b�N �吳�`�F�b�N ���`�F�b�N ���a�`�F�b�N 
           �����`�F�b�N �P�ƃ`�F�b�N �{�l�`�F�b�N ����`�F�b�N ���σ`�F�b�N ���`�F�b�N �Еۃ`�F�b�N 
           �g���`�F�b�N �P�O���`�F�b�N �X���`�F�b�N �Q���`�F�b�N �U�΃`�F�b�N �W���`�F�b�N �V���`�F�b�N 
           ����`�F�b�N �ސE�`�F�b�N ���ۃ`�F�b�N �Ƒ��`�F�b�N ���V�`�F�b�N
           .
040870*
040880*================================================================*
       �{�p���擾 SECTION.
      *
      *     MOVE SPACE TO �{�p���v.
028350     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
028360     MOVE �}�Ԃv�q              TO �{�L�|�}��.
028370     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
028380     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
028390     MOVE �{�p���v�q            TO �{�L�|�{�p��.
      *------------------------------------------------------------------------*
           IF ( �A���|�ی���� > 50 ) AND ( ���Z�|�������r���Ώ� = 1 )
               MOVE ��|�������r���J�n��  TO �{�L�|�{�p��
           ELSE
               MOVE ZERO                  TO �{�L�|�{�p��
           END-IF.
      *------------------------------------------------------------------------*
028420     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
028430                                  �{�L�|�{�p�a��N����
028440     END-START.
028450     IF ��ԃL�[ = "00"
030910         MOVE SPACE TO �I���t���O�Q
030920         PERFORM �{�p�L�^�e�Ǎ�
030930         PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
030940                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
030950                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
030960                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
030970                       ( �{�L�|�{�p��     NOT = �{�p���v�q      )
                   MOVE NC"��" TO �{�p���`�F�b�N�v(�{�L�|�{�p��)
                   PERFORM �{�p�L�^�e�Ǎ�
               END-PERFORM
           END-IF.
           MOVE �{�p���`�F�b�N�v(1)  TO �{�p���`�F�b�N�P.
           MOVE �{�p���`�F�b�N�v(2)  TO �{�p���`�F�b�N�Q.
           MOVE �{�p���`�F�b�N�v(3)  TO �{�p���`�F�b�N�R.
           MOVE �{�p���`�F�b�N�v(4)  TO �{�p���`�F�b�N�S.
           MOVE �{�p���`�F�b�N�v(5)  TO �{�p���`�F�b�N�T.
           MOVE �{�p���`�F�b�N�v(6)  TO �{�p���`�F�b�N�U.
           MOVE �{�p���`�F�b�N�v(7)  TO �{�p���`�F�b�N�V.
           MOVE �{�p���`�F�b�N�v(8)  TO �{�p���`�F�b�N�W.
           MOVE �{�p���`�F�b�N�v(9)  TO �{�p���`�F�b�N�X.
           MOVE �{�p���`�F�b�N�v(10) TO �{�p���`�F�b�N�P�O.
           MOVE �{�p���`�F�b�N�v(11) TO �{�p���`�F�b�N�P�P.
           MOVE �{�p���`�F�b�N�v(12) TO �{�p���`�F�b�N�P�Q.
           MOVE �{�p���`�F�b�N�v(13) TO �{�p���`�F�b�N�P�R.
           MOVE �{�p���`�F�b�N�v(14) TO �{�p���`�F�b�N�P�S.
           MOVE �{�p���`�F�b�N�v(15) TO �{�p���`�F�b�N�P�T.
           MOVE �{�p���`�F�b�N�v(16) TO �{�p���`�F�b�N�P�U.
           MOVE �{�p���`�F�b�N�v(17) TO �{�p���`�F�b�N�P�V.
           MOVE �{�p���`�F�b�N�v(18) TO �{�p���`�F�b�N�P�W.
           MOVE �{�p���`�F�b�N�v(19) TO �{�p���`�F�b�N�P�X.
           MOVE �{�p���`�F�b�N�v(20) TO �{�p���`�F�b�N�Q�O.
           MOVE �{�p���`�F�b�N�v(21) TO �{�p���`�F�b�N�Q�P.
           MOVE �{�p���`�F�b�N�v(22) TO �{�p���`�F�b�N�Q�Q.
           MOVE �{�p���`�F�b�N�v(23) TO �{�p���`�F�b�N�Q�R.
           MOVE �{�p���`�F�b�N�v(24) TO �{�p���`�F�b�N�Q�S.
           MOVE �{�p���`�F�b�N�v(25) TO �{�p���`�F�b�N�Q�T.
           MOVE �{�p���`�F�b�N�v(26) TO �{�p���`�F�b�N�Q�U.
           MOVE �{�p���`�F�b�N�v(27) TO �{�p���`�F�b�N�Q�V.
           MOVE �{�p���`�F�b�N�v(28) TO �{�p���`�F�b�N�Q�W.
           MOVE �{�p���`�F�b�N�v(29) TO �{�p���`�F�b�N�Q�X.
           MOVE �{�p���`�F�b�N�v(30) TO �{�p���`�F�b�N�R�O.
           MOVE �{�p���`�F�b�N�v(31) TO �{�p���`�F�b�N�R�P.
           PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 31
               MOVE �J�E���^ TO �{�p��(�J�E���^)
           END-PERFORM.
      *================================================================*
037310 ���Z�E�v�ăZ�b�g SECTION.
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
037490*
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
015000     IF (�������R����敪�e NOT = 1 )
               MOVE �������R����敪�v TO �A�E���|�����敪
           ELSE
               MOVE 1                  TO �A�E���|�����敪
015050     END-IF.
040710*
040720     CALL   "TEKIYBUN".
040730     CANCEL "TEKIYBUN".
040740*
044960*================================================================*
040890******************************************************************
040900 END PROGRAM YCH6427.
040910******************************************************************
