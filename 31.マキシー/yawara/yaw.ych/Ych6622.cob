000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH6622.
000060 AUTHOR.                 ���@�^�R��
000070*
000080*----------------------------------------------------------------*
000090*         �����p
000100*�@�@�@�@�@�J���e�J�Ј���i�_����޳��95�Łj�Đ���
000110*         MED = YAW6622G YCH6622P
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2000-03-21
000140 DATE-COMPILED.          2000-03-21
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
000270     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000280                             ORGANIZATION             IS  INDEXED
000290                             ACCESS MODE              IS  DYNAMIC
000300                             RECORD KEY               IS  �ہ|�ی����
000310                                                          �ہ|�ی��Ҕԍ�
000320* �����́A�L�[���ڂ̕ی��Җ��̂�ی��҃J�i�ɂ���
000330                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000340                                                          �ہ|�ی��Җ���
000350                                                          �ہ|�ی��Ҕԍ�
000360                             FILE STATUS              IS  ��ԃL�[
000370                             LOCK        MODE         IS  AUTOMATIC.
000380     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS  ���|�����敪
000420                             FILE STATUS              IS  ��ԃL�[
000430                             LOCK        MODE         IS  AUTOMATIC.
000440     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000450                             ORGANIZATION             IS  INDEXED
000460                             ACCESS MODE              IS  DYNAMIC
000470                             RECORD KEY               IS  ���|�敪�R�[�h
000480                                                          ���|���̃R�[�h
000490                             FILE STATUS              IS  ��ԃL�[
000500                             LOCK        MODE         IS  AUTOMATIC.
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
000570     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000580                             ORGANIZATION             IS  INDEXED
000590                             ACCESS MODE              IS  DYNAMIC
000600                             RECORD KEY               IS  ���|����敪
000610                             FILE STATUS              IS  ��ԃL�[
000620                             LOCK        MODE         IS  AUTOMATIC.
000690     SELECT  ������}�X�^    ASSIGN      TO        SEIKYUSL
000700                             ORGANIZATION             IS  INDEXED
000710                             ACCESS MODE              IS  DYNAMIC
000720                             RECORD KEY               IS  ����|�ی����
000730                                                          ����|�ی��Ҕԍ�
000740                             FILE STATUS              IS  ��ԃL�[
000750                             LOCK    MODE             IS  AUTOMATIC.
000830     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000840                             ORGANIZATION             IS  INDEXED
000850                             ACCESS MODE              IS  DYNAMIC
000860                             RECORD KEY               IS  ��|�{�p�a��N��
000870                                                          ��|���҃R�[�h
000880                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000890                                                          ��|���҃J�i
000900                                                          ��|���҃R�[�h
000910                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000920                                                          ��|�{�p�a��N��
000930                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000940                                                          ��|�ی����
000950                                                          ��|�ی��Ҕԍ�
000960                                                          ��|���҃R�[�h
000970                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000980                                                          ��|������
000990                                                          ��|��p���S�Ҕԍ�
001000                                                          ��|���҃R�[�h
001010                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001020                                                          ��|�������
001030                                                          ��|��p���S�Ҕԍ�����
001040                                                          ��|���҃R�[�h
001050                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
001060                                                          ��|�{�p�a��N��
001070                                                          ��|���҃R�[�h
001080                             FILE STATUS              IS  ��ԃL�[
001090                             LOCK        MODE         IS  AUTOMATIC.
001100     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
001110                             ORGANIZATION             IS  INDEXED
001120                             ACCESS MODE              IS  DYNAMIC
001130                             RECORD KEY               IS  �{�L�|�{�p�a��N����
001140                                                          �{�L�|���҃R�[�h
001150                             ALTERNATE RECORD KEY     IS  �{�L�|���҃R�[�h
001160                                                          �{�L�|�{�p�a��N����
001170                             FILE STATUS              IS  ��ԃL�[
001180                             LOCK        MODE         IS  AUTOMATIC.
001190     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
001200                             ORGANIZATION             IS  INDEXED
001210                             ACCESS MODE              IS  DYNAMIC
001220                             RECORD KEY               IS ���|�{�p�a��N��
001230                                                         ���|���҃R�[�h
001240                             ALTERNATE RECORD KEY     IS ���|���҃R�[�h
001250                                                         ���|�{�p�a��N��
001260                             FILE STATUS              IS  ��ԃL�[
001270                             LOCK        MODE         IS  AUTOMATIC.
001350     SELECT  ���������e      ASSIGN      TO        HUGEINL
001360                             ORGANIZATION             IS  INDEXED
001370                             ACCESS MODE              IS  DYNAMIC
001380                             RECORD KEY               IS  �����|�敪�R�[�h
001390                                                          �����|���������R�[�h
001400                             FILE STATUS              IS  ��ԃL�[
001410                             LOCK        MODE         IS  AUTOMATIC.
           SELECT  ���Ə��}�X�^    ASSIGN      TO        JIGYOSL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  ���|�ی����
                                                                ���|�ی��Ҕԍ�
                                                                ���|�L��
                                   FILE STATUS              IS  ��ԃL�[
                                   LOCK        MODE         IS  AUTOMATIC.
000241     SELECT  �J�Џ��e      ASSIGN      TO        ROUSAIJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS �J�Ё|�{�p�a��N��
000245                                                         �J�Ё|���҃R�[�h
000255                             ALTERNATE RECORD KEY     IS �J�Ё|���҃R�[�h
000265                                                         �J�Ё|�{�p�a��N��
000277                             FILE STATUS              IS ��ԃL�[
000278                             LOCK        MODE         IS AUTOMATIC.
001420     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
001430                             SYMBOLIC    DESTINATION  IS "PRT"
001440                             FORMAT                   IS  ��`�̖��o
001450                             GROUP                    IS  ���ڌQ���o
001460                             PROCESSING  MODE         IS  ������ʂo
001470                             UNIT        CONTROL      IS  �g������o
001480                             FILE        STATUS       IS  �ʒm���o.
001490******************************************************************
001500*                      DATA DIVISION                             *
001510******************************************************************
001520 DATA                    DIVISION.
001530 FILE                    SECTION.
001540*                           �m�q�k��  �R�Q�O�n
001550 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001560     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001570*                           �m�q�k��  �P�Q�W�n
001580 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001590     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001600*                           �m�q�k��  �P�Q�W�n
001610 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001620     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001660*                           �m�q�k��  �Q�T�U�n
001670 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001680     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001720*                           �m�q�k��  �P�Q�W�n
001730 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001740     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
001780*                           �m�q�k��  �R�Q�O�n
001790 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001800     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001810*                           �m�q�k��  �Q�T�U�n
001820 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
001830     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
001840*                           �m�q�k��  �P�Q�W�n
001850 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001860     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001900*                           �m�q�k��  �P�Q�W�n
001910 FD  ���������e          BLOCK   CONTAINS   1   RECORDS.
001920     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
002140*
       FD  ���Ə��}�X�^        BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY JIGYOS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001700*
000280 FD  �J�Џ��e          BLOCK   CONTAINS   1   RECORDS.
000281     COPY ROUSAIJ         OF  XFDLIB  JOINING   �J��   AS  PREFIX.
001930*
001940 FD  ����t�@�C��.
001950     COPY YCH6622P        OF  XMDLIB.
001960*----------------------------------------------------------------*
001970******************************************************************
001980*                WORKING-STORAGE SECTION                         *
001990******************************************************************
002000 WORKING-STORAGE         SECTION.
002010 01 �L�[����                           PIC X     VALUE SPACE.
002020 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
002030 01 �I���t���O                         PIC X(3)  VALUE SPACE.
002040 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
002050 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002060 01 �����b�m�s                         PIC 9(2)  VALUE ZERO.
002070 01 �����t���O                         PIC X(3)  VALUE SPACE.
002080 01 �t�@�C����                         PIC N(2)  VALUE SPACE.
002090 01 �O�a��v                           PIC 9     VALUE ZERO.
002100 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
001363 01 �S�p��                           PIC X(2)  VALUE X"8140".
001364 01 ���p��                           PIC X(2)  VALUE X"2020".
002110 01 �������̂v                         PIC N(6)  VALUE SPACE.
002120 01 ���ʖ��̂v                         PIC N(10) VALUE SPACE.
002130 01 �����ԍ��v                         PIC 9     VALUE ZERO.
002140 01 �����ԍ��q REDEFINES �����ԍ��v.
002150    03 �����ԍ��v�P                    PIC X.
002160*
002170 01 �S�p�����ԍ��v                     PIC N     VALUE SPACE.
002180 01 �S�p�����ԍ��q REDEFINES �S�p�����ԍ��v.
002190    03 �S�p�����ԍ��v�P                PIC X(2).
002200*
002210 01 ���S���v                           PIC 9(3) VALUE ZERO.
002220 01 �b���敪�v                         PIC 9    VALUE ZERO.
002230 01 �{�l���S���v                       PIC 9(3) VALUE ZERO.
002240 01 �Ƒ����S���v                       PIC 9(3) VALUE ZERO.
002250*
002260 01 �{�p�a��N�����b�v.
002270   03 �{�p�a��N���b�v.
002280     05 �{�p�a��b�v                   PIC 9    VALUE ZERO.
002290     05 �{�p�N���b�v.
002300        07 �{�p�N�b�v                  PIC 9(2) VALUE ZERO.
002310        07 �{�p���b�v                  PIC 9(2) VALUE ZERO.
002320   03 �{�p���b�v                       PIC 9(2) VALUE ZERO.
002330 01 �{�p����N�v                       PIC 9(4) VALUE ZERO.
002340 01 ���Ґ���N�v                       PIC 9(4) VALUE ZERO.
002350 01 ��ی��Ґ���N�v                   PIC 9(4) VALUE ZERO.
002360*
002370** ���������p
002380*
002390 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
002400 01 �J�E���^�Q                         PIC 9(2)  VALUE ZERO.
002410*
002420 01 ���������v�s.
002430    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
002440    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
002450    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
002460    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
002470    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
002480    03 ���������i���o�[�v�s.
002490       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
002500    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
002510 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
002520 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
002530 01 ���������s�a�k.
002540    03 ���������R�[�h�s�a�k            OCCURS 9.
002550       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
002560       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
002570       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
002580 01 �����������e�v.
002590    03 �����������e�����v              PIC X(316) OCCURS 9 VALUE SPACE.
002600    03 �����������e�����w�v.
002610       05 �����������e�P�w�v           PIC X(210)  VALUE SPACE.
002620       05 �����������e�Q�w�v           PIC X(106)   VALUE SPACE.
002660*
002670** ������������敪�p
002680 01 ������������敪�v                 PIC 9 VALUE ZERO.
002690*
002700** �o�[�R�[�h�敪�p
002710 01 �o�[�R�[�h�g�p�敪�v               PIC 9 VALUE ZERO.
002720*
002730****************
002740* �A�����ڑҔ� *
002750****************
002760*    ************
002770*    * ����L�[ *
002780*    ************
002790 01 �Ώۃf�[�^�v�q.
002800    03 �{�p�a��N���v�q.
002810       05 �{�p�a��v�q                  PIC 9(1)  VALUE ZERO.
002820       05 �{�p�N�v�q                    PIC 9(2)  VALUE ZERO.
002830       05 �{�p���v�q                    PIC 9(2)  VALUE ZERO.
002840    03 �ی���ʂv�q                     PIC 9(2)  VALUE ZERO.
002850    03 �ی��Ҕԍ��v�q                   PIC X(10) VALUE SPACE.
002860    03 �{�l�Ƒ��敪�v�q                 PIC 9(1)  VALUE ZERO.
002870    03 ��ی��҃J�i�v�q                 PIC X(20) VALUE SPACE.
002880    03 ���҃R�[�h�v�q.
002890       05 ���Ҕԍ��v�q                  PIC 9(6)  VALUE ZERO.
002900       05 �}�Ԃv�q                      PIC X(1)  VALUE SPACE.
002910    03 ������[�h�e�v�q                 PIC 9(1)  VALUE ZERO.
002920    03 ��o�N�����v�q.
002930       05 ��o�N�v�q                    PIC 9(2)  VALUE ZERO.
002940       05 ��o���v�q                    PIC 9(2)  VALUE ZERO.
002950       05 ��o���v�q                    PIC 9(2)  VALUE ZERO.
002960    03 �i���v�q                         PIC 9(1)  VALUE ZERO.
002970************
002980* ������� *
002990************
003000*
003010 01 ������S���v                        PIC 9(1)  VALUE ZERO.
003020**************
003030* ��f�ҏ�� *
003040**************
003050 01 ��f�ҏ��v.
003060    03 ���҃R�[�h�v.
003070       05 ���Ҕԍ��v                   PIC 9(6)  VALUE ZERO.
003080       05 �}�Ԃv                       PIC X(1)  VALUE SPACE.
003090    03 �s�����ԍ��v.
003100       05 ����s�����ԍ��v             PIC X(8)  VALUE SPACE.
003110       05 FILLER                       PIC X(2).
003120    03 ��v�Ҕԍ��v.
003130       05 �����v�Ҕԍ��v             PIC X(7)  VALUE SPACE.
003140       05 FILLER                       PIC X(13).
003150    03 ��p���S�Ҕԍ��v.
003160       05 �����p���S�Ҕԍ��v         PIC X(8)  VALUE SPACE.
003170       05 FILLER                       PIC X(2).
003180    03 ��v�Ҕԍ������v.
003190       05 �����v�Ҕԍ������v         PIC X(7)  VALUE SPACE.
003200       05 FILLER                       PIC X(13).
003210    03 �L���v                          PIC X(24)  VALUE SPACE.
003220    03 �ԍ��v.
003230       05 ����ԍ��v                   PIC X(30)  VALUE SPACE.
003240*       05 FILLER                       PIC X(18).
003250    03 ��ی��ҏ��v.
003260       05 ��ی��҃J�i�v               PIC X(50)  VALUE SPACE.
003270       05 ��ی��Ҏ����v               PIC X(50)  VALUE SPACE.
003280*
003290*       05 ��ی��Ҙa��`�F�b�N�v.
003300*          07 ��ی��Җ����v            PIC N(1)  VALUE SPACE.
003310*          07 ��ی��ґ吳�v            PIC N(1)  VALUE SPACE.
003320*          07 ��ی��ҏ��a�v            PIC N(1)  VALUE SPACE.
003330       05 ��ی��Ґ��ʃ`�F�b�N�v.
003340          07 ��ی��Ғj�`�F�b�N�v      PIC N(1)  VALUE SPACE.
003350          07 ��ی��ҏ��`�F�b�N�v      PIC N(1)  VALUE SPACE.
003360       05 ��ی��Ґ��ʂv               PIC 9     VALUE ZERO.
003370       05 ��ی��Ҙa��`�F�b�N�v.
003380          07 ��ی��Җ����`�F�b�N�v    PIC N(1)  VALUE SPACE.
003390          07 ��ی��ґ吳�`�F�b�N�v    PIC N(1)  VALUE SPACE.
003400          07 ��ی��ҏ��a�`�F�b�N�v    PIC N(1)  VALUE SPACE.
003410          07 ��ی��ҕ����`�F�b�N�v    PIC N(1)  VALUE SPACE.
003420       05 ��ی��Ґ��N�����v.
003430          07 ��ی��Ҙa��v            PIC 9     VALUE ZERO.
003440          07 ��ی��ҔN�v              PIC 9(2)  VALUE ZERO.
003450          07 ��ی��Ҍ��v              PIC 9(2)  VALUE ZERO.
003460          07 ��ی��ғ��v              PIC 9(2)  VALUE ZERO.
003470       05 ��ی��ҔN��v               PIC 9(3)  VALUE ZERO.
003480*
003490       05 ��ی��җX�֔ԍ��v.
003500          07 ��ی��җX�֔ԍ��P�v      PIC X(3)  VALUE SPACE.
003510          07 ��ی��җX�֔ԍ��Q�v      PIC X(4)  VALUE SPACE.
003520       05 ��ی��ҏZ���v.
003530          07 �����ی��ҏZ���v        PIC X(80) VALUE SPACE.
003540       05 ��ی��ғd�b�ԍ��v           PIC X(15) VALUE SPACE.
003550       05 �����d�b�ԍ��v.
003560          07 �����d�b�ԍ��P�v          PIC X(5)  VALUE SPACE.
003570          07 �����d�b�ԍ��Q�v          PIC X(5)  VALUE SPACE.
003580          07 �����d�b�ԍ��R�v          PIC X(5)  VALUE SPACE.
003590*          07 �s�O�ǔԂv                PIC X(5)  VALUE SPACE.
003600*          07 �����ǔԂv                PIC X(5)  VALUE SPACE.
003610*          07 �����ԍ��v                PIC X(5)  VALUE SPACE.
003620    03 ���i�擾�N�����v.
003630       05 ���i�擾�����v               PIC N(2)  VALUE SPACE.
003640       05 ���i�擾�a��v               PIC 9(1)  VALUE ZERO.
003650       05 ���i�擾�N�v                 PIC 9(2)  VALUE ZERO.
003660       05 ���i�擾���v                 PIC 9(2)  VALUE ZERO.
003670       05 ���i�擾���v                 PIC 9(2)  VALUE ZERO.
003680    03 �L�������N�����v.
003690       05 �L�����������v               PIC N(2)  VALUE SPACE.
003700       05 �L�������a��v               PIC 9(1)  VALUE ZERO.
003710       05 �L�������N�v                 PIC 9(2)  VALUE ZERO.
003720       05 �L���������v                 PIC 9(2)  VALUE ZERO.
003730       05 �L���������v                 PIC 9(2)  VALUE ZERO.
003740       05 ���i�a��`�F�b�N�v.
003750          07 ���i���a�`�F�b�N�v        PIC N(1)  VALUE SPACE.
003760          07 ���i�����`�F�b�N�v        PIC N(1)  VALUE SPACE.
003770    03 ���ҏ��v.
003780       05 ���҃J�i�v                   PIC X(50)  VALUE SPACE.
003790       05 ���Ҏ����v                   PIC X(50)  VALUE SPACE.
003800       05 ���Ґ��ʃ`�F�b�N�v.
003810          07 ���Ғj�`�F�b�N�v          PIC N(1)  VALUE SPACE.
003820          07 ���ҏ��`�F�b�N�v          PIC N(1)  VALUE SPACE.
003830       05 �����v.
003840          07 ��������v                PIC N(4)  VALUE SPACE.
003850          07 FILLER                    PIC X(4).
003860       05 �a��`�F�b�N�v.
003870          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
003880          07 �吳�`�F�b�N�v            PIC N(1)  VALUE SPACE.
003890          07 ���a�`�F�b�N�v            PIC N(1)  VALUE SPACE.
003900          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
003900          07 �ߘa�`�F�b�N�v            PIC N(1)  VALUE SPACE.
003910       05 ���ҔN�v                     PIC 9(2) VALUE ZERO.
003920       05 ���Ҍ��v                     PIC 9(2) VALUE ZERO.
003930       05 ���ғ��v                     PIC 9(2) VALUE ZERO.
003940       05 ���ҔN��v                   PIC 9(3) VALUE ZERO.
003950       05 ���Ґ��ʂv                   PIC N(4) VALUE SPACE.
003960    03 �ی���ʃ`�F�b�N�v.
003970       05 �Еۃ`�F�b�N�v               PIC N(1) VALUE SPACE.
003980       05 �g���`�F�b�N�v               PIC N(1) VALUE SPACE.
003990       05 ���σ`�F�b�N�v               PIC N(1) VALUE SPACE.
004000       05 ���ۃ`�F�b�N�v               PIC N(1) VALUE SPACE.
004010       05 ���ك`�F�b�N�v               PIC N(1) VALUE SPACE.
004020       05 �D���`�F�b�N�v               PIC N(1) VALUE SPACE.
004030       05 �V�l�`�F�b�N�v               PIC N(1) VALUE SPACE.
004040       05 ���`�F�b�N�v                 PIC N(1) VALUE SPACE.
004050       05 �g��`�F�b�N�v               PIC N(1) VALUE SPACE.
004060       05 �ސE�`�F�b�N�v               PIC N(1) VALUE SPACE.
004070       05 ��q�`�F�b�N�v               PIC N(1) VALUE SPACE.
004080       05 ���c���`�F�b�N�v             PIC N(1) VALUE SPACE.
004090       05 ���ۑg���`�F�b�N�v           PIC N(1) VALUE SPACE.
004100       05 �J�Ѓ`�F�b�N�v               PIC N(1) VALUE SPACE.
004110       05 �����`�F�b�N�v               PIC N(1) VALUE SPACE.
004120       05 ���ۃ`�F�b�N�v               PIC N(1) VALUE SPACE.
004130       05 �����`�F�b�N�v               PIC N(1) VALUE SPACE.
004140       05 ���җX�֔ԍ��v.
004150          07 ���җX�֔ԍ��P�v          PIC X(3)  VALUE SPACE.
004160          07 ���җX�֔ԍ��Q�v          PIC X(4)  VALUE SPACE.
004170       05 ���ҏZ���v.
004180          07 ���ҏZ���P�v              PIC X(40) VALUE SPACE.
004190          07 ���ҏZ���Q�v              PIC X(40) VALUE SPACE.
004200       05 ���ғd�b�ԍ��v               PIC X(15) VALUE SPACE.
004210    03 ���������v                      PIC X(210) OCCURS 27 VALUE SPACE.
004220**************
004230* ���Ə���� *
004240**************
004250 01 ���Ə����v.
004260    03 ���Ə����̂v.
004270       05 ������Ə����̂v             PIC X(60)  VALUE SPACE.
004280    03 ���Ə��X�֔ԍ��v.
004290       05 ���Ə��X�֔ԍ��P�v           PIC X(3)  VALUE SPACE.
004300       05 ���Ə��X�֔ԍ��Q�v           PIC X(4)  VALUE SPACE.
004310    03 ���Ə��Z���v.
004320       05 ������Ə��Z���v             PIC X(50)  VALUE SPACE.
004320       05 ������Ə��Z���Q�v           PIC X(50)  VALUE SPACE.
004340**************
004350* �������� *
004360**************
004370 01 ��������v.
004380    03 �ی��Ҕԍ��v.
004390       05 ����ی��Ҕԍ��v             PIC X(8)  VALUE SPACE.
004400       05 FILLER                       PIC X(2).
004410    03 �����於�̂v.
004420       05 ��������於�̂v             PIC X(40) VALUE SPACE.
004430    03 �x�����v.
004440       05 ����x�����v                 PIC X(40) VALUE SPACE.
004450    03 �����於�x�����v.
004460       05 ��������於�x�����v         PIC X(54) VALUE SPACE.
004470       05 FILLER                       PIC X(26).
004480    03 �ی��ҌĖ��v.
004490*       05 ����ی��ҌĖ��v             PIC N(7)  VALUE SPACE.
004500       05 ����ی��ҌĖ��v             PIC X(14)  VALUE SPACE.
004510    03 ������X�֔ԍ��v.
004520       05 ������X�֔ԍ��P�v           PIC X(3)  VALUE SPACE.
004530       05 ������X�֔ԍ��Q�v           PIC X(4)  VALUE SPACE.
004540    03 ������Z���P�v.
004550       05 ���������Z���P�v           PIC X(40) VALUE SPACE.
004560    03 ������Z���Q�v.
004570       05 ���������Z���Q�v           PIC X(35) VALUE SPACE.
004580       05 FILLER                       PIC X(5).
004590****************
004600* �����f�[�^�e *
004610****************
004620 01 �������v.
004630    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
004640    03 �����f�[�^���v  OCCURS   9.
004650       05 ��ʃR�[�h�v               PIC X(4)  VALUE SPACE.
004660       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
004670       05 ������ʂv                   PIC 9(2)  VALUE ZERO.
004680       05 ���ʂv                       PIC 9(2)  VALUE ZERO.
004690       05 ���E�敪�v                   PIC 9(1)  VALUE ZERO.
004700       05 �����ʒu�ԍ��v               PIC 9(2)  VALUE ZERO.
004710       05 �������v                     PIC N(18) VALUE SPACE.
004720       05 �����N�����v.
004730          07 �����a��v                PIC 9(1)  VALUE ZERO.
004740          07 �����N�v                  PIC 9(2)  VALUE ZERO.
004750          07 �������v                  PIC 9(2)  VALUE ZERO.
004760          07 �������v                  PIC 9(2)  VALUE ZERO.
004770          07 �����N������؂v          PIC X(1)  VALUE SPACE.
004780       05 �{�p�J�n�N�����v.
004790          07 �{�p�J�n�a��v            PIC 9(1)  VALUE ZERO.
004800          07 �{�p�J�n�N�v              PIC 9(2)  VALUE ZERO.
004810          07 �{�p�J�n���v              PIC 9(2)  VALUE ZERO.
004820          07 �{�p�J�n���v              PIC 9(2)  VALUE ZERO.
004830          07 �{�J�N������؂v          PIC X(1)  VALUE SPACE.
004840       05 �{�p�I���N�����v.
004850          07 �{�p�I���a��v            PIC 9(1)  VALUE ZERO.
004860          07 �{�p�I���N�v              PIC 9(2)  VALUE ZERO.
004870          07 �{�p�I�����v              PIC 9(2)  VALUE ZERO.
004880          07 �{�p�I�����v              PIC 9(2)  VALUE ZERO.
004890          07 �{�I�N������؂v          PIC X(1)  VALUE SPACE.
004900       05 �J�n�N�����擾�t���O         PIC X(3)  VALUE SPACE.
004910       05 �]�A�v                       PIC N(4)  VALUE SPACE.
004920       05 �]�A�`�F�b�N�v.
004930          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
004940          07 ���~�`�F�b�N�v            PIC N(1)  VALUE SPACE.
004950          07 �]��`�F�b�N�v            PIC N(1)  VALUE SPACE.
004960*
004970******************
004980* ���S���`�F�b�N *
004990******************
005000 01 ���S���`�F�b�N�v.
005010    03 �O���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
005020    03 �P���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
005030    03 �Q���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
005040    03 �R���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
005050**********
005060* ���v�s *
005070**********
005080 01 ���v�s�v.
005090    03 �����I�����v                    PIC 9(2) VALUE ZERO.
005100    03 �Ώې���v                      PIC 9(4) VALUE ZERO.
005110    03 ���v                            PIC 9(3) VALUE ZERO.
005120    03 �]�v                            PIC 9(3) VALUE ZERO.
005130*/�N�������ی��҃f�[�^�̃��[�N
005140 01 �ە��S���敪�v                     PIC 9    VALUE ZERO.
005150 01 �ۖ{�l���S���v                     PIC 9(3) VALUE ZERO.
005160 01 �ۉƑ����S���v                     PIC 9(3) VALUE ZERO.
005170 01 �ۖ{�l���S�����v                   PIC 9(3) VALUE ZERO.
005180 01 �ۉƑ����S�����v                   PIC 9(3) VALUE ZERO.
005190*
005200 01 ��r�a��N���v.
005210    03 ��r�a��v                      PIC 9    VALUE ZERO.
005220    03 ��r�N�v                        PIC 9(2) VALUE ZERO.
005230    03 ��r���v                        PIC 9(2) VALUE ZERO.
005240*
005250 01 �������.
005260     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
005270     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
005280     03 ������ʂo                     PIC X(2) VALUE SPACE.
005290     03 �g������o.
005300         05 �[������o.
005310             07 �ړ������o             PIC X(1)  VALUE SPACE.
005320             07 �ړ��s���o             PIC 9(3)  VALUE ZERO.
005330         05 �ڍא���o                 PIC X(2)  VALUE SPACE.
005340     03 �ʒm���o                     PIC X(2)  VALUE SPACE.
005350     03 ���j�b�g���o                   PIC X(8)  VALUE SPACE.
005360*
005370 01 �v�Z�@����N�v                     PIC 9(2)  VALUE ZERO.
005380* ���t�v�n�q�j
005390 01 �a��I���N�v                       PIC 9(4)  VALUE ZERO.
005400 01 �v�Z�@����.
005410    03 �v�Z�@����N                    PIC 9(4)  VALUE ZERO.
005420    03 �v�Z�@�����                  PIC 9(4)  VALUE ZERO.
005430 01 �v�Z�@����q REDEFINES �v�Z�@����.
005440    03 �v�Z�@���I                      PIC 9(2).
005450    03 �v�Z�@���t                      PIC 9(6).
005460    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
005470       05 �v�Z�@�N��                   PIC 9(4).
005480       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
005490         07 �v�Z�@�N                   PIC 9(2).
005500         07 �v�Z�@��                   PIC 9(2).
005510       05 �v�Z�@��                     PIC 9(2).
005520*
      * C �A�g�p
       01  �����P�v        PIC X(4096).
       01  �����Q�v        PIC X(512).
       01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
      *
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
005530******************************************************************
005540*                          �A������                              *
005550******************************************************************
005560*
005570**********************
005580* ���b�Z�[�W�\���L�[ *
005590**********************
005600 01 �A���|�L�[ IS EXTERNAL.
005610    03  �A���|���b�Z�[�W                 PIC N(20).
005620*
005630****************
005640* �A�����ڑҔ� *
005650****************
005660*    ************
005670*    * ������� *
005680*    ************
005690*    �����̗���
005700***********************
005710 01 �A�{�|�����P IS EXTERNAL.
005720   03 �A�{�|�{�l���S��                 PIC 9(3).
005730************
005740* ����L�[ *
005750************
005760 01 �A��|�Ώۃf�[�^ IS EXTERNAL.
005770    03 �A��|�{�p�a��N��.
005780       05 �A��|�{�p�a��                  PIC 9(1).
005790       05 �A��|�{�p�N                    PIC 9(2).
005800       05 �A��|�{�p��                    PIC 9(2).
005810    03 �A��|�ی����                     PIC 9(2).
005820    03 �A��|�ی��Ҕԍ�                   PIC X(10).
005830    03 �A��|�{�l�Ƒ��敪                 PIC 9(1).
005840    03 �A��|��ی��҃J�i                 PIC X(20).
005850    03 �A��|���҃R�[�h.
005860       05 �A��|���Ҕԍ�                  PIC 9(6).
005870       05 �A��|�}��                      PIC X(1).
005880    03 �A��|������[�h�e                 PIC 9(1).
005940*/�ڍ�
005950    03 �A��|�ی��؈���敪               PIC 9(1).
005980    03 �A��|�����ڍ� OCCURS 7.
005990       05 �A��|��������s                PIC 9(1).
006000       05 �A��|���ʈ���敪              PIC 9(1).
006010       05 �A��|�]�A����敪              PIC 9(1).
006020       05 �A��|��������敪              PIC 9(1).
      */
       01 �A��|�Ώۃf�[�^���� IS EXTERNAL GLOBAL.
          03 �A��|���z OCCURS 4.
             05 �A��|���z�a��                  PIC 9.
             05 �A��|���z�N                    PIC 9(2).
             05 �A��|���z��                    PIC 9(2).
             05 �A��|���z�����N����.
                07 �A��|���z�����N             PIC 9(2).
                07 �A��|���z������             PIC 9(2).
                07 �A��|���z������             PIC 9(2).
          03 �A��|��o�N����.
             05 �A��|��o�N                    PIC 9(2).
             05 �A��|��o��                    PIC 9(2).
             05 �A��|��o��                    PIC 9(2).
          03 �A��|�i��                         PIC 9(1).
006110*
       01 �A���|�\���t���O�U�U�O IS EXTERNAL GLOBAL.
          03 �A���|�v���r���[�敪               PIC 9(1).
006330* ���S���擾�p14/10�`
006340 01 �A���|���S���擾�L�[ IS EXTERNAL.
006350    03 �A���|�{�p�a��N��.
006360       05 �A���|�{�p�a��               PIC 9.
006370       05 �A���|�{�p�N��.
006380          07 �A���|�{�p�N              PIC 9(2).
006390          07 �A���|�{�p��              PIC 9(2).
006400    03 �A���|���҃R�[�h.
006410       05 �A���|���Ҕԍ�               PIC 9(6).
006420       05 �A���|�}��                   PIC X.
006430    03 �A���|���ە��S��                PIC 9(3).
006440    03 �A���|���ۖ{�̕��S��            PIC 9(3).
006450    03 �A���|���ە��S��                PIC 9(3).
006460    03 �A���|�Q�V�V���S��              PIC 9(3).
006470    03 �A���|�������S��                PIC 9(3).
006480    03 �A���|���ʗp���S��              PIC 9(3).
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
006440******************************************************************
006450*                      PROCEDURE  DIVISION                       *
006460******************************************************************
006470 PROCEDURE               DIVISION.
006480************
006490*           *
006500* ��������   *
006510*           *
006520************
002570     PERFORM �v�����^�t�@�C���쐬.
006530     PERFORM ������.
006540************
006550*           *
006560* �又��     *
006570*           *
006580************
006590* ���
006600     PERFORM �A�����ڑҔ�.
006610     PERFORM ����Z�b�g
006620     PERFORM �������
006630************
006640*           *
006650* �I������   *
006660*           *
006670************
006680     PERFORM �I������.
006690     EXIT PROGRAM.
006700*
006710*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCH6622"             TO �g�A�o�q�s�e�|���[�v���O������.
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
006720*================================================================*
006730 ������ SECTION.
006740*
006750     PERFORM �t�@�C���I�[�v��.
006760*    /* ���ݓ��t�擾 */
006770     ACCEPT �v�Z�@���t FROM DATE.
006780*    /* 1980�`2079�N�̊ԂŐݒ� */
006790     IF �v�Z�@�N > 80
006800         MOVE 19 TO �v�Z�@���I
006810     ELSE
006820         MOVE 20 TO �v�Z�@���I
006830     END-IF.
006840     PERFORM �J�����g�����擾.
006850     PERFORM �a��I���N�擾.
006860     COMPUTE �v�Z�@����N�v = �v�Z�@����N - 1988.
006870*================================================================*
006880 �J�����g�����擾 SECTION.
006890*
006900     MOVE ZEROS TO ���|����敪.
006910     READ ������}�X�^
006920     NOT INVALID KEY
006930         MOVE ���|�J�����g����           TO �J�����g�����v
006940         MOVE ���|�J���e������������敪 TO ������������敪�v
006950         MOVE ���|�o�[�R�[�h�Q�R�Q�b�g�p�敪  TO �o�[�R�[�h�g�p�敪�v
006960     END-READ.
006970*
006980*================================================================*
006990 �a��I���N�擾 SECTION.
007000*
007010     MOVE �J�����g�����v TO ���|�����敪.
007020     READ �����}�X�^
007030     INVALID KEY
007040         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
007050         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
007060                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007070         ACCEPT  �L�[���� FROM CONS
007080         PERFORM �I������
007090         EXIT PROGRAM
007100     NOT INVALID KEY
007110         COMPUTE �O�a��v = �J�����g�����v - 1
007120         MOVE �O�a��v TO ���|�����敪
007130         READ �����}�X�^
007140         INVALID KEY
007150             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
007160             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
007170                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007180             ACCEPT  �L�[���� FROM CONS
007190             PERFORM �I������
007200             EXIT PROGRAM
007210         NOT INVALID KEY
007220             MOVE ���|�I������N TO �a��I���N�v
007230         END-READ
007240     END-READ.
007250*
007260*================================================================*
007270 �A�����ڑҔ� SECTION.
007280*
007290     INITIALIZE �Ώۃf�[�^�v�q.
007300     MOVE �A��|�{�p�a��      TO �{�p�a��v�q.
007310     MOVE �A��|�{�p�N        TO �{�p�N�v�q.
007320     MOVE �A��|�{�p��        TO �{�p���v�q.
007330     MOVE �A��|�ی����      TO �ی���ʂv�q.
007340     MOVE �A��|�ی��Ҕԍ�    TO �ی��Ҕԍ��v�q.
007350     MOVE �A��|�{�l�Ƒ��敪  TO �{�l�Ƒ��敪�v�q.
007360     MOVE �A��|��ی��҃J�i  TO ��ی��҃J�i�v�q.
007370     MOVE �A��|���Ҕԍ�      TO ���Ҕԍ��v�q.
007380     MOVE �A��|�}��          TO �}�Ԃv�q.
007390     MOVE �A��|������[�h�e  TO ������[�h�e�v�q.
007400     MOVE �A��|��o�N        TO ��o�N�v�q.
007410     MOVE �A��|��o��        TO ��o���v�q.
007420     MOVE �A��|��o��        TO ��o���v�q.
007430     MOVE �A��|�i��          TO �i���v�q.
007440*================================================================*
007450 �t�@�C���I�[�v�� SECTION.
007460*
007470     OPEN INPUT   �ی��҃}�X�^
007480         MOVE NC"�ی���" TO �t�@�C����.
007490         PERFORM �I�[�v���`�F�b�N.
007500     OPEN INPUT   �����}�X�^
007510         MOVE NC"����" TO �t�@�C����.
007520         PERFORM �I�[�v���`�F�b�N.
007530     OPEN INPUT   ���̃}�X�^
007540         MOVE NC"����" TO �t�@�C����.
007550         PERFORM �I�[�v���`�F�b�N.
007560     OPEN INPUT   ���Z�v�g�e
007570         MOVE NC"���Z" TO �t�@�C����.
007580         PERFORM �I�[�v���`�F�b�N.
007590     OPEN INPUT   ������}�X�^
007600         MOVE NC"������" TO �t�@�C����.
007610         PERFORM �I�[�v���`�F�b�N.
007650     OPEN INPUT   ������}�X�^
007660         MOVE NC"����" TO �t�@�C����.
007670         PERFORM �I�[�v���`�F�b�N.
007710     OPEN INPUT   ��f�ҏ��e.
007720         MOVE NC"���" TO �t�@�C����.
007730         PERFORM �I�[�v���`�F�b�N.
007740     OPEN INPUT   �{�p�L�^�e.
007750         MOVE NC"�{�L�e" TO �t�@�C����.
007760         PERFORM �I�[�v���`�F�b�N.
007770     OPEN INPUT   �����f�[�^�e.
007780         MOVE NC"����" TO �t�@�C����.
007790         PERFORM �I�[�v���`�F�b�N.
007830     OPEN INPUT   ���������e.
007840         MOVE NC"��������" TO �t�@�C����.
007850         PERFORM �I�[�v���`�F�b�N.
007900     OPEN INPUT ���Ə��}�X�^.
007910         MOVE NC"���Ə�" TO �t�@�C����.
007920         PERFORM �I�[�v���`�F�b�N.
007900     OPEN INPUT �J�Џ��e.
007910         MOVE NC"�J�Џ��e" TO �t�@�C����.
007920         PERFORM �I�[�v���`�F�b�N.
007860     OPEN I-O   ����t�@�C��
007870         MOVE NC"���" TO �t�@�C����.
007880         PERFORM �G���[�����o.
007890*================================================================*
007900 �I�[�v���`�F�b�N SECTION.
007910*
007920     IF ��ԃL�[  NOT =  "00"
007930         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
007940         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
007950         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
007960                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007970         ACCEPT  �L�[���� FROM CONS
007980         PERFORM �t�@�C����
007990         EXIT PROGRAM.
008000*================================================================*
008010 �t�@�C���� SECTION.
008020*
008030     CLOSE ����t�@�C��    �ی��҃}�X�^  �����}�X�^
008040           ������}�X�^  ������}�X�^  ���̃}�X�^
008050           ��f�ҏ��e    �{�p�L�^�e    ���Z�v�g�e
008060           �����f�[�^�e    ���Ə��}�X�^  �J�Џ��e
008070           ���������e.
008080*================================================================*
008090 �I������ SECTION.
008100*
008110     PERFORM �t�@�C����.
008120*================================================================*
008130 ����Z�b�g SECTION.
008140*
008150     EVALUATE ������[�h�e�v�q
008160     WHEN 0
008170         PERFORM ����Z�b�g�P
008180     WHEN 1
008190         PERFORM ����Z�b�g�Q
008200     WHEN 2
008210         PERFORM ����Z�b�g�R
008240     END-EVALUATE.
008250*
008260*================================================================*
008270 ����Z�b�g�P SECTION.
008280*
008290     INITIALIZE YCH6622P.
008300     MOVE "X" TO EDIT-MODE OF ���҃R�[�h.
008310     INITIALIZE ��f�ҏ��v.
008320     INITIALIZE ���Ə����v.
008330     INITIALIZE ��������v.
008340     INITIALIZE �������v.
008350     INITIALIZE ���S���`�F�b�N�v.
008360     PERFORM ���S���擾.
008370     PERFORM ��������擾.
008380     PERFORM ��f�ҏ��擾.
008390     PERFORM �����f�[�^�擾.
008400*
008410     IF ������������敪�v  NOT = 1 
008420        PERFORM ���������擾
008430     END-IF.
008440*
008450     PERFORM ����㕔�Z�b�g.
008460     PERFORM ��������Z�b�g.
008470*================================================================*
008480 ����Z�b�g�Q SECTION.
008490*
008500     INITIALIZE YCH6622P.
008510     MOVE "X" TO EDIT-MODE OF ���҃R�[�h.
008520     INITIALIZE ��f�ҏ��v.
008530     INITIALIZE ���Ə����v.
008540     INITIALIZE ��������v.
008550     INITIALIZE �������v.
008560     INITIALIZE ���S���`�F�b�N�v.
008570     PERFORM ���S���擾.
008580     PERFORM ��������擾.
008590     PERFORM ��f�ҏ��擾.
008600     PERFORM ����㕔�Z�b�g.
008610*================================================================*
008620 ����Z�b�g�R SECTION.
008630*
008640     INITIALIZE YCH6622P.
008650     MOVE "X" TO EDIT-MODE OF ���҃R�[�h.
008660     INITIALIZE ��f�ҏ��v.
008670     INITIALIZE ���Ə����v.
008680     INITIALIZE ��������v.
008690     INITIALIZE �������v.
008590     PERFORM ��f�ҏ��擾.
008700     PERFORM �����f�[�^�擾.
008710*
008720     IF ������������敪�v  NOT = 1 
008730        PERFORM ���������擾
008740     END-IF.
008750*
008760     PERFORM ��������Z�b�g.
008920*================================================================*
008930 ����㕔�Z�b�g SECTION.
008940*
008950     IF �o�[�R�[�h�g�p�敪�v = 1
008960        MOVE ���҃R�[�h�v   TO ���҃R�[�h
008970        MOVE SPACE          TO EDIT-MODE OF ���҃R�[�h
008980     END-IF.
008990*
009000     MOVE ���Ҕԍ��v                 TO ���Ҕԍ�.
009010     MOVE �}�Ԃv                     TO �}��.
009020**************************
009030* ��ی��ҏ؏��Z�b�g   *
009040**************************
009050*     IF �L���v(1:2) = "��" 
009060*        MOVE  SPACE    TO  �L��
009070*     ELSE
009080*        MOVE �L���v    TO  �L��
009090*     END-IF.
009100*     IF ( ����ԍ��v(1:1) = "*"  ) OR
009110*        ( ����ԍ��v(1:2) = "��" )
009120*        MOVE  SPACE      TO  �ԍ�
009130*     ELSE
009140*        MOVE ����ԍ��v  TO  �ԍ�
009150*     END-IF.
009160************************
009170* ��ی��ҏ��Z�b�g   *
009180************************
009190*     MOVE ��ی��Ҏ����v             TO ��ی��Ҏ���.
009200*     MOVE ��ی��҃J�i�v             TO ��ی��҃J�i.
009210*     MOVE ���i�擾�����v             TO ���i�擾����.
009220*     MOVE ���i�擾�N�v               TO ���i�擾�N.
009230*     MOVE ���i�擾���v               TO ���i�擾��.
009240*     MOVE ���i�擾���v               TO ���i�擾��.
009250*     MOVE ���i���a�`�F�b�N�v         TO ���i���a�`�F�b�N.
009260*     MOVE ���i�����`�F�b�N�v         TO ���i�����`�F�b�N.
009270*/�L������
009280*     MOVE �L�������N�v               TO �L���N.
009290*     MOVE �L���������v               TO �L����.
009300*     MOVE �L���������v               TO �L����.
009310*    IF ( �L�������N�v = ZERO ) AND
009320*        ( �L���������v = ZERO ) AND
009330*        ( �L���������v = ZERO )
009340*         MOVE SPACE          TO �L������
009350*         MOVE SPACE          TO �L���N�����\��
009360*         MOVE SPACE          TO �L���N�\��
009370*         MOVE SPACE          TO �L�����\��
009380*         MOVE SPACE          TO �L�����\��
009390*     ELSE
009400*         MOVE �L�����������v TO �L������
009410*         MOVE NC"�L���N����" TO �L���N�����\��
009420*         MOVE NC"�N"         TO �L���N�\��
009430*         MOVE NC"��"         TO �L�����\��
009440*         MOVE NC"��"         TO �L�����\��
009450*     END-IF.
009460*     MOVE ��ی��Җ����`�F�b�N�v     TO ��ی��Җ����`�F�b�N.
009470*     MOVE ��ی��ґ吳�`�F�b�N�v     TO ��ی��ґ吳�`�F�b�N.
009480*     MOVE ��ی��ҏ��a�`�F�b�N�v     TO ��ی��ҏ��a�`�F�b�N.
009490*     MOVE ��ی��ҕ����`�F�b�N�v     TO ��ی��ҕ����`�F�b�N.
009500*     MOVE ��ی��ҔN�v               TO ��ی��ҔN.
009510*     MOVE ��ی��Ҍ��v               TO ��ی��Ҍ�.
009520*     MOVE ��ی��ғ��v               TO ��ی��ғ�.
009530*     MOVE ��ی��ҔN��v             TO ��ی��ҔN��.
009540********************
009550* ���ҏ��Z�b�g   *
009560********************
009570*     MOVE �O���`�F�b�N�v             TO �O���`�F�b�N.
009580*     MOVE �P���`�F�b�N�v             TO �P���`�F�b�N.
009590*     MOVE �Q���`�F�b�N�v             TO �Q���`�F�b�N.
009600*     MOVE �R���`�F�b�N�v             TO �R���`�F�b�N.
009610*     IF  ����s�����ԍ��v(1:2) = "99"
009620*         MOVE SPACE                  TO �s�����ԍ�
009630*     ELSE
009640*         MOVE ����s�����ԍ��v       TO �s�����ԍ�
009650*     END-IF.
009660*     IF (�����v�Ҕԍ��v(1:1) = "*") OR
009670*        (�����v�Ҕԍ��v(1:2) = "��") 
009680*         MOVE SPACE                  TO ��v�Ҕԍ�
009690*     ELSE
009700*         MOVE �����v�Ҕԍ��v       TO ��v�Ҕԍ�
009710*     END-IF
009720*
009730*     IF  �����p���S�Ҕԍ��v(1:2) = "99"
009740*         MOVE SPACE                  TO ����S�Ҕԍ�
009750*     ELSE
009760*         MOVE �����p���S�Ҕԍ��v   TO ����S�Ҕԍ�
009770*     END-IF.
009780*
009790*     IF (�����v�Ҕԍ������v(1:1) = "*") OR
009800*        (�����v�Ҕԍ������v(1:2) = "��")
009810*         MOVE SPACE                  TO ��v�Ҕԍ�����
009820*     ELSE
009830*         MOVE �����v�Ҕԍ������v   TO ��v�Ҕԍ�����
009840*     END-IF.
009850     MOVE ���҃J�i�v                 TO ���҃J�i.
009860     MOVE ���Ҏ����v                 TO ���Ҏ���.
009870*     MOVE ��ی��Ғj�`�F�b�N�v       TO ��ی��Ғj�`�F�b�N.
009880*     MOVE ��ی��ҏ��`�F�b�N�v       TO ��ی��ҏ��`�F�b�N.
009890*     MOVE ��ی��җX�֔ԍ��P�v       TO ��ی��җX�֔ԍ��P.
009900*     MOVE ��ی��җX�֔ԍ��Q�v       TO ��ی��җX�֔ԍ��Q.
009910*     MOVE "-"                        TO ��ی��җX�֋��.
009920*     MOVE ��ی��ҏZ���v             TO ��ی��ҏZ��.
009930*     MOVE ��ی��ғd�b�ԍ��v         TO ��ی��ғd�b�ԍ�.
009940     MOVE ���Ғj�`�F�b�N�v           TO ���Ғj�`�F�b�N.
009950     MOVE ���ҏ��`�F�b�N�v           TO ���ҏ��`�F�b�N.
009960     MOVE �����`�F�b�N�v             TO �����`�F�b�N.
009970     MOVE �吳�`�F�b�N�v             TO �吳�`�F�b�N.
009980     MOVE ���a�`�F�b�N�v             TO ���a�`�F�b�N.
009990     MOVE �����`�F�b�N�v             TO �����`�F�b�N.
           IF �ߘa�`�F�b�N�v NOT = SPACE
              MOVE NC"�ߘa"                TO �����b�l
              MOVE �ߘa�`�F�b�N�v          TO �����`�F�b�N
           END-IF.
010000     MOVE ���ҔN�v                   TO ���ҔN.
010010     MOVE ���Ҍ��v                   TO ���Ҍ�.
010020     MOVE ���ғ��v                   TO ���ғ�.
010030     MOVE ���ҔN��v                 TO ���ҔN��.
010040*     MOVE ��������v                 TO ����.
010050     MOVE ���җX�֔ԍ��P�v           TO ���җX�֔ԍ��P.
010060     MOVE ���җX�֔ԍ��Q�v           TO ���җX�֔ԍ��Q.
010070     MOVE "-"                        TO ���җX�֋��.
010080     MOVE ���ҏZ���v                 TO ���ҏZ��.
010090     MOVE ���ғd�b�ԍ��v             TO ���ғd�b�ԍ�.
010100*     MOVE ���ҏZ���P�v               TO ���ҏZ���P.
010110*     MOVE ���ҏZ���Q�v               TO ���ҏZ���Q.
010120********************
010130* ���Ə����Z�b�g *
010140********************
010150     MOVE ���Ə����̂v               TO ���Ə�����.
010160*     MOVE ���Ə��X�֔ԍ��P�v         TO ���Ə��X�ւP.
010170*     MOVE "-"                        TO ���Ə��X�ւQ.
010180*     MOVE ���Ə��X�֔ԍ��Q�v         TO ���Ə��X�ւR.
010190     MOVE ������Ə��Z���v           TO ���Ə��Z��.
010190     MOVE ������Ə��Z���Q�v         TO ���Ə��Z���Q.
010200************************************
010210* �s���{���E���N�ی��g�����Z�b�g *
010220************************************
010230*�J�ЁA�����A���ۂ̎��͕ی��ҏ��͈󎚂��Ȃ�
010240*     IF �ی���ʂv�q = 70 OR 80 OR 90
010250*         MOVE SPACE                  TO �ی��Ҕԍ�
010260*         MOVE SPACE                  TO �����於�x����
010270*     ELSE
010280*         MOVE ����ی��Ҕԍ��v       TO �ی��Ҕԍ�
010290*         MOVE ��������於�x�����v   TO �����於�x����
010300*     END-IF.
010310**************************
010320* �ی���ʃ`�F�b�N�Z�b�g *
010330**************************
010340*     MOVE �Еۃ`�F�b�N�v             TO �Еۃ`�F�b�N.
010350*     MOVE �g���`�F�b�N�v             TO �g���`�F�b�N.
010360*     MOVE ���ك`�F�b�N�v             TO ���ك`�F�b�N.
010370*     MOVE �D���`�F�b�N�v             TO �D���`�F�b�N.
010380*     MOVE ���σ`�F�b�N�v             TO ���σ`�F�b�N.
010390*     MOVE ���ۃ`�F�b�N�v             TO ���ۃ`�F�b�N.
010400*     MOVE �ސE�`�F�b�N�v             TO �ސE�`�F�b�N.
010410*     MOVE �V�l�`�F�b�N�v             TO �V�l�`�F�b�N.
010420*     MOVE ���c���`�F�b�N�v           TO ���c���`�F�b�N.
010430*     MOVE �����`�F�b�N�v             TO �����`�F�b�N.
010440*     MOVE ���ۃ`�F�b�N�v             TO ���ۃ`�F�b�N.
010450*     MOVE ���`�F�b�N�v               TO ���`�F�b�N.
010460*     MOVE �g��`�F�b�N�v             TO �g��`�F�b�N.
010470*     MOVE ��q�`�F�b�N�v             TO ��q�`�F�b�N.
010480*     MOVE ���ۃ`�F�b�N�v             TO ���ۃ`�F�b�N.
010490*     MOVE �����`�F�b�N�v             TO �����`�F�b�N.
010500*================================================================*
010510 ��������Z�b�g SECTION.
010520*
010530********************
010540* �����f�[�^�Z�b�g *
010550********************
010560* �P���� *
010570**********
010580*     MOVE ��ʃR�[�h�v(1)   TO �����R�[�h�P.
010590     MOVE �������v(1)         TO �������P.
010600     MOVE �����N�v(1)         TO �����N�P.
010610     MOVE �������v(1)         TO �������P.
010620     MOVE �������v(1)         TO �������P.
010630     MOVE �{�p�J�n�N�v(1)     TO �{�p�J�n�N�P.
010640     MOVE �{�p�J�n���v(1)     TO �{�p�J�n���P.
010650     MOVE �{�p�J�n���v(1)     TO �{�p�J�n���P.
010660     MOVE �{�p�I���N�v(1)     TO �{�p�I���N�P.
010670     MOVE �{�p�I�����v(1)     TO �{�p�I�����P.
010680     MOVE �{�p�I�����v(1)     TO �{�p�I�����P.
010690*     MOVE �����N������؂v(1) TO ��؂P�P ��؂P�Q.
010700*     MOVE �{�J�N������؂v(1) TO ��؂P�R ��؂P�S.
010710*     MOVE �{�I�N������؂v(1) TO ��؂P�T ��؂P�U.
010720*     MOVE �]�A�v(1)           TO �]�A�P.
010730     MOVE �����`�F�b�N�v(1)   TO �����`�F�b�N�P.
010740     MOVE ���~�`�F�b�N�v(1)   TO ���~�`�F�b�N�P.
010750     MOVE �]��`�F�b�N�v(1)   TO �]��`�F�b�N�P.
010760*     ******************
010770*     * �����͈ꎞ�ۗ� *
010780*     ******************
010790*     **********************
010800*     * �{�p�񐔂͈ꎞ�ۗ� *
010810*     **********************
010820*     **********************
010830*     * �]�A���͈̂ꎞ�ۗ� *
010840*     **********************
010850**********
010860* �Q���� *
010870**********
010880*     MOVE ��ʃR�[�h�v(2)   TO �����R�[�h�Q.
010890     MOVE �������v(2)         TO �������Q.
010900     MOVE �����N�v(2)         TO �����N�Q.
010910     MOVE �������v(2)         TO �������Q.
010920     MOVE �������v(2)         TO �������Q.
010930     MOVE �{�p�J�n�N�v(2)     TO �{�p�J�n�N�Q.
010940     MOVE �{�p�J�n���v(2)     TO �{�p�J�n���Q.
010950     MOVE �{�p�J�n���v(2)     TO �{�p�J�n���Q.
010960     MOVE �{�p�I���N�v(2)     TO �{�p�I���N�Q.
010970     MOVE �{�p�I�����v(2)     TO �{�p�I�����Q.
010980     MOVE �{�p�I�����v(2)     TO �{�p�I�����Q.
010990*     MOVE �����N������؂v(2) TO ��؂Q�P ��؂Q�Q.
011000*     MOVE �{�J�N������؂v(2) TO ��؂Q�R ��؂Q�S.
011010*     MOVE �{�I�N������؂v(2) TO ��؂Q�T ��؂Q�U.
011020*     MOVE �]�A�v(2)           TO �]�A�Q.
011030     MOVE �����`�F�b�N�v(2)   TO �����`�F�b�N�Q.
011040     MOVE ���~�`�F�b�N�v(2)   TO ���~�`�F�b�N�Q.
011050     MOVE �]��`�F�b�N�v(2)   TO �]��`�F�b�N�Q.
011060*     ******************
011070*     * �����͈ꎞ�ۗ� *
011080*     ******************
011090*     **********************
011100*     * �{�p�񐔂͈ꎞ�ۗ� *
011110*     **********************
011120*     **********************
011130*     * �]�A���͈̂ꎞ�ۗ� *
011140*     **********************
011150**********
011160* �R���� *
011170**********
011180*     MOVE ��ʃR�[�h�v(3)   TO �����R�[�h�R.
011190     MOVE �������v(3)         TO �������R.
011200     MOVE �����N�v(3)         TO �����N�R.
011210     MOVE �������v(3)         TO �������R.
011220     MOVE �������v(3)         TO �������R.
011230     MOVE �{�p�J�n�N�v(3)     TO �{�p�J�n�N�R.
011240     MOVE �{�p�J�n���v(3)     TO �{�p�J�n���R.
011250     MOVE �{�p�J�n���v(3)     TO �{�p�J�n���R.
011260     MOVE �{�p�I���N�v(3)     TO �{�p�I���N�R.
011270     MOVE �{�p�I�����v(3)     TO �{�p�I�����R.
011280     MOVE �{�p�I�����v(3)     TO �{�p�I�����R.
011290*     MOVE �����N������؂v(3) TO ��؂R�P ��؂R�Q.
011300*     MOVE �{�J�N������؂v(3) TO ��؂R�R ��؂R�S.
011310*     MOVE �{�I�N������؂v(3) TO ��؂R�T ��؂R�U.
011320*     MOVE �]�A�v(3)           TO �]�A�R.
011330     MOVE �����`�F�b�N�v(3)   TO �����`�F�b�N�R.
011340     MOVE ���~�`�F�b�N�v(3)   TO ���~�`�F�b�N�R.
011350     MOVE �]��`�F�b�N�v(3)   TO �]��`�F�b�N�R.
011360*     ******************
011370*     * �����͈ꎞ�ۗ� *
011380*     ******************
011390*     **********************
011400*     * �{�p�񐔂͈ꎞ�ۗ� *
011410*     **********************
011420*     **********************
011430*     * �]�A���͈̂ꎞ�ۗ� *
011440*     **********************
011450**********
011460* �S���� *
011470**********
011480*     MOVE ��ʃR�[�h�v(4)   TO �����R�[�h�S.
011490     MOVE �������v(4)         TO �������S.
011500     MOVE �����N�v(4)         TO �����N�S.
011510     MOVE �������v(4)         TO �������S.
011520     MOVE �������v(4)         TO �������S.
011530     MOVE �{�p�J�n�N�v(4)     TO �{�p�J�n�N�S.
011540     MOVE �{�p�J�n���v(4)     TO �{�p�J�n���S.
011550     MOVE �{�p�J�n���v(4)     TO �{�p�J�n���S.
011560     MOVE �{�p�I���N�v(4)     TO �{�p�I���N�S.
011570     MOVE �{�p�I�����v(4)     TO �{�p�I�����S.
011580     MOVE �{�p�I�����v(4)     TO �{�p�I�����S.
011590*     MOVE �����N������؂v(4) TO ��؂S�P ��؂S�Q.
011600*     MOVE �{�J�N������؂v(4) TO ��؂S�R ��؂S�S.
011610*     MOVE �{�I�N������؂v(4) TO ��؂S�T ��؂S�U.
011620*     MOVE �]�A�v(4)           TO �]�A�S.
011630     MOVE �����`�F�b�N�v(4)   TO �����`�F�b�N�S.
011640     MOVE ���~�`�F�b�N�v(4)   TO ���~�`�F�b�N�S.
011650     MOVE �]��`�F�b�N�v(4)   TO �]��`�F�b�N�S.
011660*     ******************
011670*     * �����͈ꎞ�ۗ� *
011680*     ******************
011690*     **********************
011700*     * �{�p�񐔂͈ꎞ�ۗ� *
011710*     **********************
011720*     **********************
011730*     * �]�A���͈̂ꎞ�ۗ� *
011740*     **********************
011750**********
011760* �T���� *
011770**********
011780*     MOVE ��ʃR�[�h�v(5)   TO �����R�[�h�T.
011790     MOVE �������v(5)         TO �������T.
011800     MOVE �����N�v(5)         TO �����N�T.
011810     MOVE �������v(5)         TO �������T.
011820     MOVE �������v(5)         TO �������T.
011830     MOVE �{�p�J�n�N�v(5)     TO �{�p�J�n�N�T.
011840     MOVE �{�p�J�n���v(5)     TO �{�p�J�n���T.
011850     MOVE �{�p�J�n���v(5)     TO �{�p�J�n���T.
011860     MOVE �{�p�I���N�v(5)     TO �{�p�I���N�T.
011870     MOVE �{�p�I�����v(5)     TO �{�p�I�����T.
011880     MOVE �{�p�I�����v(5)     TO �{�p�I�����T.
011890*     MOVE �����N������؂v(5) TO ��؂T�P ��؂T�Q.
011900*     MOVE �{�J�N������؂v(5) TO ��؂T�R ��؂T�S.
011910*     MOVE �{�I�N������؂v(5) TO ��؂T�T ��؂T�U.
011920*     MOVE �]�A�v(5)           TO �]�A�T.
011930     MOVE �����`�F�b�N�v(5)   TO �����`�F�b�N�T.
011940     MOVE ���~�`�F�b�N�v(5)   TO ���~�`�F�b�N�T.
011950     MOVE �]��`�F�b�N�v(5)   TO �]��`�F�b�N�T.
011960*     ******************
011970*     * �����͈ꎞ�ۗ� *
011980*     ******************
011990*     **********************
012000*     * �{�p�񐔂͈ꎞ�ۗ� *
012010*     **********************
012020*     **********************
012030*     * �]�A���͈̂ꎞ�ۗ� *
012040*     **********************
012050************
012060* �������� *
012070************
012080     MOVE ���������v(1)       TO ���������P.
012090     MOVE ���������v(2)       TO ���������Q.
012100     MOVE ���������v(3)       TO ���������R.
012110*     MOVE ���������v(4)       TO ���������S.
012120*     MOVE ���������v(5)       TO ���������T.
012130*     MOVE ���������v(6)       TO ���������U.
012140*     MOVE ���������v(7)       TO ���������V.
012150*     MOVE ���������v(8)       TO ���������W.
012160*     MOVE ���������v(9)       TO ���������X.
012170*     MOVE ���������v(10)      TO ���������P�O.
012180*     MOVE ���������v(11)      TO ���������P�P.
012190*     MOVE ���������v(12)      TO ���������P�Q.
012200*     MOVE ���������v(13)      TO ���������P�R.
012210*     MOVE ���������v(14)      TO ���������P�S.
012220*     MOVE ���������v(15)      TO ���������P�T.
012230*
012240*================================================================*
012250 ���S���擾 SECTION.
012260*
012270     MOVE ZERO TO �{�l���S���v.
012280     MOVE ZERO TO �Ƒ����S���v.
012290*
012300     MOVE �{�p�a��v�q   TO ��|�{�p�a��.
012310     MOVE �{�p�N�v�q     TO ��|�{�p�N.
012320     MOVE �{�p���v�q     TO ��|�{�p��.
012330     MOVE ���Ҕԍ��v�q   TO ��|���Ҕԍ�.
012340     MOVE �}�Ԃv�q       TO ��|�}��.
012350     READ ��f�ҏ��e
012360     INVALID KEY
012370         MOVE  NC"�{�p���̎�f�ҏ�񂪂���܂���" TO �A���|���b�Z�[�W
012380         CALL   "MSG001"
012390         CANCEL "MSG001"
012400         PERFORM �t�@�C����
012410         MOVE ZERO TO PROGRAM-STATUS
012420         EXIT PROGRAM
012430     NOT INVALID KEY
013360* 14/10�`�@�T�u���[�`������
013370         IF ��|�{�p�a��N�� >= 41410
013380            PERFORM ���S���擾�P�S�P�O
013530         END-IF
013540     END-READ.
013550     EVALUATE ���S���v
013560     WHEN 0
013570         MOVE NC"��" TO �O���`�F�b�N�v
013580     WHEN 10
013590         MOVE NC"��" TO �P���`�F�b�N�v
013600     WHEN 20
013610         MOVE NC"��" TO �Q���`�F�b�N�v
013620     WHEN 30
013630         MOVE NC"��" TO �R���`�F�b�N�v
013640     END-EVALUATE.
014600*================================================================*
014610 ���S���擾�P�S�P�O SECTION.
014620*
014630* ��f�҂e READ��
014640* ����14/10�`
014650     MOVE ZERO  TO ���S���v.
014670*
014680     MOVE SPACE TO �A���|���S���擾�L�[.
014690     INITIALIZE �A���|���S���擾�L�[.
014700     MOVE ��|�{�p�a��N�� TO �A���|�{�p�a��N��.
014710     MOVE ��|���҃R�[�h   TO �A���|���҃R�[�h.
014720*
014730     CALL   "HUTANRIT".
014740     CANCEL "HUTANRIT".
014750*
014760* / �{�� /
014770     MOVE �A���|���ۖ{�̕��S�� TO ���S���v.
014780*
013830*================================================================*
013840 ��f�ҏ��擾 SECTION.
013850*
013860**************************************************
013870* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
013880* �� ���Ҕԍ�.... ���Ҕԍ��v                     *
013890* �� �}��........ �}�Ԃv                         *
013900* �� �L�� ....... �L���v�Ɋi�[                   *
013910* �� �ԍ� ....... �ԍ��v�Ɋi�[                   *
013920* �� ��ی��҃J�i.��ی��҃J�i�v�Ɋi�[           *
013930* �� ��ی��Ҏ���.��ی��Ҏ����v�Ɋi�[           *
013940* �� �X�֔ԍ��P ..�X�֔ԍ��P�v�Ɋi�[             *
013950* �� �X�֔ԍ��Q ..�X�֔ԍ��Q�v�Ɋi�[             *
013960* �� �Z���P ......�Z���P�v�Ɋi�[                 *
013970* �� �Z���Q ......�Z���Q�v�Ɋi�[                 *
013980* �� �d�b�ԍ�.....�d�b�ԍ��v�Ɋi�[               *
013990* �� ���҃J�i ....���҃J�i�v�Ɋi�[               *
014000* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
014010* �� ���� ........���̃}�X�^��葱���v�Ɏ擾     *
014020* �� ���Ҙa�� ....�a��ɂ��`�F�b�N��"��"���i�[ *
014030* �� ���ҔN ......���ҔN�v�Ɋi�[                 *
014040* �� ���Ҍ� ......���Ҍ��v�Ɋi�[                 *
014050* �� ���ғ� ......���ғ��v�Ɋi�[                 *
014060**************************************************
014070     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
014080     MOVE �{�p�N�v�q         TO ��|�{�p�N.
014090     MOVE �{�p���v�q         TO ��|�{�p��.
014100     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
014110     READ ��f�ҏ��e
014120     INVALID KEY
014130         CONTINUE
014140*            /* ���肦�Ȃ� */
014150     NOT INVALID KEY
014160         MOVE ��|���Ҕԍ�     TO ���Ҕԍ��v
014170         MOVE ��|�}��         TO �}�Ԃv
014180*         MOVE ��|�L��         TO �L���v
014190*         MOVE ��|�ԍ�         TO �ԍ��v
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
014270         MOVE ��|��ی��҃J�i TO ��ی��҃J�i�v
014280         MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ����v
014290*
014300         EVALUATE ��|��ی��Ґ���
014310         WHEN 1
014320             MOVE NC"��"  TO ��ی��Ғj�`�F�b�N�v
014330         WHEN 2
014340             MOVE NC"��"  TO ��ی��ҏ��`�F�b�N�v
014350         END-EVALUATE
014360         EVALUATE ��|���Ґ���
014370         WHEN 1
014380             MOVE NC"��"  TO ���Ғj�`�F�b�N�v
014390         WHEN 2
014400             MOVE NC"��"  TO ���ҏ��`�F�b�N�v
014410         END-EVALUATE
014420*
014430         EVALUATE ��|��ی��Ҙa��
014440         WHEN 1
014450             MOVE NC"��"  TO ��ی��Җ����`�F�b�N�v
014460         WHEN 2
014470             MOVE NC"��"  TO ��ی��ґ吳�`�F�b�N�v
014480         WHEN 3
014490             MOVE NC"��"  TO ��ی��ҏ��a�`�F�b�N�v
014500         WHEN 4
014510             MOVE NC"��"  TO ��ی��ҕ����`�F�b�N�v
014520         END-EVALUATE
014530         MOVE ��|��ی��ҔN   TO ��ی��ҔN�v
014540         MOVE ��|��ی��Ҍ�   TO ��ی��Ҍ��v
014550         MOVE ��|��ی��ғ�   TO ��ی��ғ��v
014560
014570*��ی��ҔN��
014580     MOVE �A��|�{�p�a�� TO ���|�����敪
014590     READ �����}�X�^
014600     NOT INVALID KEY
014610         COMPUTE �{�p����N�v = ���|�J�n����N + ( �A��|�{�p�N - 1 )
014620     END-READ
014630     IF ( ��|��ی��Ҙa�� NOT = ZERO ) AND
014640        ( ��|��ی��ҔN   NOT = ZERO ) AND
014650        ( ��|��ی��Ҍ�   NOT = ZERO ) AND
014660        ( ��|��ی��ғ�   NOT = ZERO )
014670
014680         MOVE ZERO TO ��ی��ҔN��v
014690*
014700         MOVE ��|��ی��Ҙa�� TO ���|�����敪
014710         READ �����}�X�^
014720         NOT INVALID KEY
014730             COMPUTE ��ی��Ґ���N�v = ���|�J�n����N + ( ��|��ی��ҔN - 1 )
014740         END-READ
014750*
014760         COMPUTE ��ی��ҔN��v   = �{�p����N�v - ��ی��Ґ���N�v
014770*
014780         IF �A��|�{�p�� < ��|��ی��Ҍ�
014790             COMPUTE ��ی��ҔN��v = ��ی��ҔN��v - 1
014800         END-IF
014810*
014820*         MOVE ��ی��ҔN��v     TO ��ی��ҔN��
014830     END-IF
014840*
014850*/�X�֔ԍ� �[���̎��̓X�y�[�X
014860         IF ��|�X�֔ԍ��P = "000"
014870             MOVE SPACE          TO ��ی��җX�֔ԍ��P�v
014880             MOVE SPACE          TO ��ی��җX�֔ԍ��Q�v
014890         ELSE
014900             MOVE ��|�X�֔ԍ��P TO ��ی��җX�֔ԍ��P�v
014910             MOVE ��|�X�֔ԍ��Q TO ��ی��җX�֔ԍ��Q�v
014920         END-IF
014930         MOVE ��|�d�b�ԍ�     TO ��ی��ғd�b�ԍ��v
014940         STRING ��|�Z���P  DELIMITED BY SPACE
014950                ��|�Z���Q  DELIMITED BY SPACE
014960           INTO ��ی��ҏZ���v
014970         END-STRING
014980*
014990*/�X�֔ԍ� �[���̎��̓X�y�[�X
015000         IF ��|���җX�֔ԍ��P = "000"
015010             MOVE SPACE              TO ���җX�֔ԍ��P�v
015020             MOVE SPACE              TO ���җX�֔ԍ��Q�v
015030         ELSE
015040             MOVE ��|���җX�֔ԍ��P TO ���җX�֔ԍ��P�v
015050             MOVE ��|���җX�֔ԍ��Q TO ���җX�֔ԍ��Q�v
015060         END-IF
015070*         MOVE ��|���ҏZ���P     TO ���ҏZ���P�v
015080*         MOVE ��|���ҏZ���Q     TO ���ҏZ���Q�v
015090         STRING ��|���ҏZ���P  DELIMITED BY SPACE
015100                ��|���ҏZ���Q  DELIMITED BY SPACE
015110           INTO ���ҏZ���v
015120         END-STRING
015130         MOVE ��|���ғd�b�ԍ�   TO ���ғd�b�ԍ��v
015140*
015150*�d�b�ԍ�����
015160*         UNSTRING �d�b�ԍ��v  DELIMITED BY "-"
015170*             INTO �����d�b�ԍ��P�v
015180*                  �����d�b�ԍ��Q�v
015190*                  �����d�b�ԍ��R�v
015200*         END-UNSTRING
015210*�d�b�ԍ��E�l����
015220*         IF �����d�b�ԍ��R�v = SPACE
015230*             MOVE �����d�b�ԍ��Q�v TO �����d�b�ԍ��R�v
015240*             MOVE �����d�b�ԍ��P�v TO �����d�b�ԍ��Q�v
015250*             MOVE SPACE            TO �����d�b�ԍ��P�v
015260*         END-IF
015270*
015280         MOVE ��|���҃J�i     TO ���҃J�i�v
015290         MOVE ��|���Ҏ���     TO ���Ҏ����v
015300*
               MOVE ��|�ی����     TO ���|�ی����
               MOVE ��|�ی��Ҕԍ�   TO ���|�ی��Ҕԍ�
      *         MOVE ��|�L��         TO ���|�L��
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
               MOVE �A�Í������|���������L�� TO ���|�L��
      *
      *-----------------------------------------------------------------*
      *
               READ ���Ə��}�X�^
               INVALID KEY
                  MOVE SPACE TO ���|���R�[�h
                  INITIALIZE    ���|���R�[�h
               END-READ
               MOVE ���|���Ə�����   TO ���Ə����̂v
015540*         STRING ���|���Ə��Z���P  DELIMITED BY SPACE
015550*                ���|���Ə��Z���Q  DELIMITED BY SPACE
015560*           INTO ���Ə��Z���v
015570*         END-STRING
               MOVE ���|���Ə��Z���P   TO ������Ə��Z���v
               MOVE ���|���Ə��Z���Q   TO ������Ə��Z���Q�v
015360* ����
015370         IF ��|�{�l�Ƒ��敪 = 1
015380             MOVE NC"�{�l"     TO �����v
015390         ELSE
015400             MOVE 05          TO ���|�敪�R�[�h
015410             MOVE ��|����    TO ���|���̃R�[�h
015420             READ ���̃}�X�^
015430             INVALID KEY
015440                 MOVE SPACE   TO �����v
015450             NOT INVALID KEY
015460                 MOVE ���|����  TO �����v
015470             END-READ
015480         END-IF
015490* ���Ҙa��N����
015500         EVALUATE ��|���Ҙa��
015510         WHEN 1
015520             MOVE NC"��"  TO �����`�F�b�N�v
015530         WHEN 2
015540             MOVE NC"��"  TO �吳�`�F�b�N�v
015550         WHEN 3
015560             MOVE NC"��"  TO ���a�`�F�b�N�v
015570         WHEN 4
015580             MOVE NC"��"  TO �����`�F�b�N�v
015570         WHEN 5
015580             MOVE NC"��"  TO �ߘa�`�F�b�N�v
015590         END-EVALUATE
015600         MOVE ��|���ҔN  TO ���ҔN�v
015610         MOVE ��|���Ҍ�  TO ���Ҍ��v
015620         MOVE ��|���ғ�  TO ���ғ��v
015630*���ҔN��
015640     MOVE �A��|�{�p�a�� TO ���|�����敪
015650     READ �����}�X�^
015660     NOT INVALID KEY
015670         COMPUTE �{�p����N�v = ���|�J�n����N + ( �A��|�{�p�N - 1 )
015680     END-READ
015690     IF ( ��|���Ҙa�� NOT = ZERO ) AND
015700        ( ��|���ҔN   NOT = ZERO ) AND
015710        ( ��|���Ҍ�   NOT = ZERO ) AND
015720        ( ��|���ғ�   NOT = ZERO )
015730*
015740         MOVE ZERO TO ���ҔN��v
015750*
015760         MOVE ��|���Ҙa�� TO ���|�����敪
015770         READ �����}�X�^
015780         NOT INVALID KEY
015790             COMPUTE ���Ґ���N�v = ���|�J�n����N + ( ��|���ҔN - 1 )
015800         END-READ
015810*
015820         COMPUTE ���ҔN��v   = �{�p����N�v - ���Ґ���N�v
015830*
015840         IF �A��|�{�p�� < ��|���Ҍ�
015850             COMPUTE ���ҔN��v = ���ҔN��v - 1
015860         END-IF
015870*
015880         MOVE ���ҔN��v     TO ���ҔN��
015890     END-IF
015900* ���Ґ���
015910*         EVALUATE ��|���Ґ���
015920*         WHEN 1
015930*             MOVE NC"�i�j�j" TO ���Ґ��ʂv
015940*         WHEN 2
015950*             MOVE NC"�i���j" TO ���Ґ��ʂv
015960*         END-EVALUATE
015970*/�L������
015980         MOVE ��|�L���a��   TO �L�������a��v
015990         MOVE ��|�L���N     TO �L�������N�v
016000         MOVE ��|�L����     TO �L���������v
016010         MOVE ��|�L����     TO �L���������v
016020         MOVE �L�������a��v TO ���|�����敪
016030         READ �����}�X�^
016040         INVALID KEY
016050             MOVE SPACE        TO �L�����������v
016060         NOT INVALID KEY
016070             MOVE ���|�������� TO �L�����������v
016080         END-READ
016090*/���i�擾�N����
016100         MOVE ��|���i�a�� TO ���i�擾�a��v
016110         MOVE ��|���i�N   TO ���i�擾�N�v
016120         MOVE ��|���i��   TO ���i�擾���v
016130         MOVE ��|���i��   TO ���i�擾���v
016140         IF (���i�擾�a��v NOT = ZERO) AND
016150            (���i�擾�N�v   NOT = ZERO) AND
016160            (���i�擾���v   NOT = ZERO) AND
016170            (���i�擾���v   NOT = ZERO)
016180             EVALUATE ���i�擾�a��v
016190             WHEN 3
016200                 MOVE NC"��"  TO ���i���a�`�F�b�N�v
016210             WHEN 4
016220                 MOVE NC"��"  TO ���i�����`�F�b�N�v
016230             END-EVALUATE
016240         END-IF
016250**�s�������ۤ�ސE�̏ꍇ�͗L���������o��
016260**�Ǝ�ʍ���(���ۑg��)����Ϥ���٤�D����Еۤ�g������q���͗L�����������o�͂���
016270*         IF (( ��|�ی���� = 08 ) OR
016280*            (( ��|�ی���� = 01 ) AND
016290*            (  ��|�ی��Ҕԍ�(3:1) NOT = "3" )))
016300*             MOVE ��|�L���a�� TO ���i�擾�a��v
016310*             MOVE ��|�L���N   TO ���i�擾�N�v
016320*             MOVE ��|�L����   TO ���i�擾���v
016330*             MOVE ��|�L����   TO ���i�擾���v
016340*         ELSE
016350*             MOVE ��|���i�a�� TO ���i�擾�a��v
016360*             MOVE ��|���i�N   TO ���i�擾�N�v
016370*             MOVE ��|���i��   TO ���i�擾���v
016380*             MOVE ��|���i��   TO ���i�擾���v
016390*         END-IF
016400*         IF (���i�擾�a��v NOT = ZERO) AND
016410*            (���i�擾�N�v   NOT = ZERO) AND
016420*            (���i�擾���v   NOT = ZERO) AND
016430*            (���i�擾���v   NOT = ZERO)
016440*             EVALUATE ���i�擾�a��v
016450*             WHEN 3
016460*                 MOVE NC"��"  TO ���i���a�`�F�b�N�v
016470*             WHEN 4
016480*                 MOVE NC"��"  TO ���i�����`�F�b�N�v
016490*             END-EVALUATE
016500*         END-IF
016510* �s�����ԍ� �󋋎Ҕԍ�
016520         IF ��|������� NOT = ZERO
016530             MOVE ��|��p���S�Ҕԍ����� TO ��p���S�Ҕԍ��v
016540             MOVE ��|��v�Ҕԍ�����     TO ��v�Ҕԍ������v
016550         END-IF
016560         IF ��|������ = 05
016570             MOVE ��|��p���S�Ҕԍ�     TO �s�����ԍ��v
016580             MOVE ��|��v�Ҕԍ��V�l     TO ��v�Ҕԍ��v
016590         END-IF
      * �J�Џ��
               IF ��|�ی���� = 70
                  MOVE ��|�{�p�a��N�� TO �J�Ё|�{�p�a��N��
                  MOVE ��|���҃R�[�h   TO �J�Ё|���҃R�[�h
                  READ �J�Џ��e
                  INVALID KEY
                     MOVE SPACE TO �J�Ё|���R�[�h
                     INITIALIZE    �J�Ё|���R�[�h
                  END-READ
                  MOVE �J�Ё|�J�Ў��Ə�����       TO ���Ə����̂v
014780            MOVE �J�Ё|�J�Ў��Ə��X�֔ԍ��P TO ���Ə��X�֔ԍ��P�v
014790            MOVE �J�Ё|�J�Ў��Ə��X�֔ԍ��Q TO ���Ə��X�֔ԍ��Q�v
      *            STRING �J�Ё|�J�Ў��Ə��Z���P DELIMITED BY SPACE
014790*                   �J�Ё|�J�Ў��Ə��Z���Q DELIMITED BY SPACE
      *               INTO ���Ə��Z���v
      *            END-STRING
                  MOVE �J�Ё|�J�Ў��Ə��Z���P     TO ������Ə��Z���v
                  MOVE �J�Ё|�J�Ў��Ə��Z���Q     TO ������Ə��Z���Q�v
012130            MOVE �J�Ё|�J���ی��ԍ�         TO �ԍ��v
               END-IF
      *
016600*�ی����
016610         EVALUATE ��|�������
016620         WHEN 50
016630             MOVE NC"��"  TO ���ۃ`�F�b�N�v
016640         WHEN 51
016650*/��4���� "4113"���� "4108"��� "4132"���� �̎��́A�u���v�B����ȊO�́u�V�v
016660             IF  ��|��p���S�Ҕԍ�����(1:4) = "4113" OR
016670                 ��|��p���S�Ҕԍ�����(1:4) = "4108" OR
016680                 ��|��p���S�Ҕԍ�����(1:4) = "4132"
016690                 MOVE NC"��"    TO ���`�F�b�N�v
016700             ELSE
016710                 MOVE NC"��"    TO �V�l�`�F�b�N�v
016720             END-IF
016730         WHEN 52
016740             MOVE NC"��"  TO ��q�`�F�b�N�v
016750         WHEN 53
016760             MOVE NC"��"  TO �g��`�F�b�N�v
016770         WHEN 54
016780             MOVE NC"��"  TO �����`�F�b�N�v
016790         WHEN 55
016800             MOVE NC"��"  TO ���c���`�F�b�N�v
016810         END-EVALUATE
016820
016830         IF ��|������ = 05
016840             MOVE NC"��"  TO �V�l�`�F�b�N�v
016850         ELSE
016860             EVALUATE ��|�ی����
016870             WHEN 01
016880                 MOVE NC"��"  TO ���ۃ`�F�b�N�v
016890             WHEN 02
016900                 MOVE NC"��"  TO �Еۃ`�F�b�N�v
016910             WHEN 03
016920                 MOVE NC"��"  TO �g���`�F�b�N�v
016930             WHEN 04
016940                 MOVE NC"��"  TO ���σ`�F�b�N�v
016950             WHEN 06
016960                 MOVE NC"��"  TO ���ك`�F�b�N�v
016970             WHEN 07
016980                 MOVE NC"��"  TO �D���`�F�b�N�v
016990             WHEN 08
017000                 MOVE NC"��"  TO �ސE�`�F�b�N�v
017010             WHEN 09
017020                 MOVE NC"��"  TO ���σ`�F�b�N�v
017030*             WHEN 70
017040*                 MOVE NC"��"  TO �J�Ѓ`�F�b�N�v
017050             WHEN 80
017060                 MOVE NC"��"  TO �����`�F�b�N�v
017070             END-EVALUATE
017080         END-IF
017090     END-READ.
017100*HILO �e�X�g�|�|�|�|�|�|�|�|�|�|�|
017110*      MOVE NC"��"  TO ��ی��Җ����v.
017120*      MOVE NC"��"  TO ��ی��ґ吳�v.
017130*      MOVE NC"��"  TO ��ی��ҏ��a�v.
017140*      MOVE NC"��"  TO ��ی��Ғj�`�F�b�N�v.
017150*      MOVE NC"��"  TO ��ی��ҏ��`�F�b�N�v.
017160*      MOVE NC"��"  TO ���Ғj�`�F�b�N�v.
017170*      MOVE NC"��"  TO ���ҏ��`�F�b�N�v.
017180*      MOVE NC"��"  TO �����`�F�b�N�v.
017190*      MOVE NC"��"  TO �吳�`�F�b�N�v.
017200*      MOVE NC"��"  TO ���a�`�F�b�N�v.
017210*      MOVE NC"��"  TO �����`�F�b�N�v.
017220*      MOVE NC"��"  TO �Еۃ`�F�b�N�v    .
017230*      MOVE NC"��"  TO �g���`�F�b�N�v    .
017240*      MOVE NC"��"  TO ���σ`�F�b�N�v    .
017250*      MOVE NC"��"  TO ���ۃ`�F�b�N�v    .
017260*      MOVE NC"��"  TO ���ك`�F�b�N�v    .
017270*      MOVE NC"��"  TO �D���`�F�b�N�v    .
017280*      MOVE NC"��"  TO �V�l�`�F�b�N�v    .
017290*      MOVE NC"��"  TO ���`�F�b�N�v    .
017300*      MOVE NC"��"  TO �g��`�F�b�N�v    .
017310*      MOVE NC"��"  TO �ސE�`�F�b�N�v    .
017320*      MOVE NC"��"  TO ��q�`�F�b�N�v    .
017330*      MOVE NC"��"  TO ���c���`�F�b�N�v  .
017340*      MOVE NC"��"  TO ���ۑg���`�F�b�N�v.
017350*      MOVE NC"��"  TO �J�Ѓ`�F�b�N�v    .
017360*      MOVE NC"��"  TO ���ۃ`�F�b�N�v    .
017370*      MOVE NC"��"  TO �����`�F�b�N�v    .
017380*      MOVE NC"��"  TO �����`�F�b�N�v    .
017390*      MOVE NC"��" TO �O���`�F�b�N�v.
017400*      MOVE NC"��" TO �P���`�F�b�N�v.
017410*      MOVE NC"��" TO �Q���`�F�b�N�v.
017420*      MOVE NC"��" TO �R���`�F�b�N�v.
017430*      MOVE NC"��"  TO ���i���a�`�F�b�N�v.
017440*      MOVE NC"��"  TO ���i�����`�F�b�N�v.
017450*      MOVE NC"��"  TO ��ی��Җ����`�F�b�N�v.
017460*      MOVE NC"��"  TO ��ی��ґ吳�`�F�b�N�v.
017470*      MOVE NC"��"  TO ��ی��ҏ��a�`�F�b�N�v.
017480*      MOVE NC"��"  TO ��ی��ҕ����`�F�b�N�v.
017490*-------------------------------------
017500*================================================================*
017510 ��������擾 SECTION.
017520*
017530****************************************************
017540* �A���f�[�^����ی��҃}�X�^��萿������擾����B *
017550* ���ہ|��������敪=1�̏ꍇ������}�X�^���g�p   *
017560* �� �ی��Ҕԍ�...�ی��Ҕԍ��v�Ɋi�[               *
017570* �� ����........ �����於�̂v�Ɋi�[               *
017580* �� �X�֔ԍ��P.. ������X�֔ԍ��P�v�Ɋi�[         *
017590* �� �X�֔ԍ��Q.. ������X�֔ԍ��Q�v�Ɋi�[         *
017600* �� �Z���P.......������Z���P�v�Ɋi�[             *
017610* �� �Z���Q.......������Z���Q�v�Ɋi�[             *
017620****************************************************
017630     MOVE SPACE          TO �ی��Ҕԍ��v.
017640     MOVE SPACE          TO �����於�̂v.
017650     MOVE SPACE          TO �x�����v.
017660     MOVE SPACE          TO ������X�֔ԍ��P�v.
017670     MOVE SPACE          TO ������X�֔ԍ��Q�v.
017680     MOVE SPACE          TO ������Z���P�v.
017690     MOVE SPACE          TO ������Z���Q�v.
017700     MOVE SPACE          TO ����ی��ҌĖ��v.
017710     MOVE SPACE          TO �����於�x�����v
017720*
017730     MOVE �ی���ʂv�q   TO �ہ|�ی����.
017740     MOVE �ی��Ҕԍ��v�q TO �ہ|�ی��Ҕԍ�.
017750     READ �ی��҃}�X�^
017760     INVALID KEY
017770         MOVE SPACE      TO �����於�̂v
017780         MOVE SPACE      TO �x�����v
017790     NOT INVALID KEY
017800         MOVE �ہ|�ی��Ҕԍ�  TO �ی��Ҕԍ��v
017810         MOVE �ہ|�ی��Җ���  TO �����於�̂v
017820         MOVE �ہ|�x��������  TO �x�����v
017830         MOVE �ہ|�X�֔ԍ��P  TO ������X�֔ԍ��P�v
017840         MOVE �ہ|�X�֔ԍ��Q  TO ������X�֔ԍ��Q�v
017850         MOVE �ہ|�Z���P      TO ������Z���P�v
017860         MOVE �ہ|�Z���Q      TO ������Z���Q�v
017870     END-READ.
017880     EVALUATE �ی���ʂv�q
017890     WHEN 2
017900     WHEN 6
017910         MOVE "�Љ�ی�������" TO ����ی��ҌĖ��v
017920     WHEN 3
017930         MOVE "���N�ی��g��"   TO ����ی��ҌĖ��v
017940     WHEN OTHER
017950         MOVE SPACE TO ����ی��ҌĖ��v
017960     END-EVALUATE.
017970     IF ����ی��ҌĖ��v NOT = SPACE
017980         STRING �����於�̂v DELIMITED BY SPACE
017990                �ی��ҌĖ��v DELIMITED BY SPACE
018000           INTO �����於�̂v
018010         END-STRING
018020     END-IF.
018030*�g���Ƌ��ς͎x�������󎚂���
018040     EVALUATE �ی���ʂv�q
018050     WHEN 03
018060     WHEN 04
018070         STRING �����於�̂v DELIMITED BY SPACE
018080                "  "         DELIMITED BY SIZE
018090                �x�����v     DELIMITED BY SPACE
018100           INTO �����於�x�����v
018110         END-STRING
018120     WHEN OTHER
018130         MOVE �����於�̂v TO �����於�x�����v
018140     END-EVALUATE.
018150*================================================================*
018160 �����f�[�^�擾 SECTION.
018170*
018180**************************************************
018190* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾 *
018200* �� ������...���ʁ{������ʂɂĉ��H���Ċi�[     *
018210* �� �����N.......�����N�v                       *
018220* �� ������.......�������v                       *
018230* �� ������.......�������v                       *
018240* �� �{�p�I���N...�I���N�v                       *
018250* �� �{�p�I����...�I�����v                       *
018260* �� �{�p�I����...�I�����v                       *
018270**************************************************
018280     MOVE �{�p�a��v�q       TO ���|�{�p�a��.
018290     MOVE �{�p�N�v�q         TO ���|�{�p�N.
018300     MOVE �{�p���v�q         TO ���|�{�p��.
018310     MOVE ���҃R�[�h�v�q     TO ���|���҃R�[�h.
018320     READ �����f�[�^�e
018330     INVALID KEY
018340         CONTINUE
018350*            /* ���肦�Ȃ� */
018360     NOT INVALID KEY
018370         MOVE ZERO                         TO ���ʐ��v
018380         MOVE ���|���ʐ�                   TO ���ʐ��v
018390         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018400                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
018410*********************************************
018420* ���j�S�_...������ʁ{���ʂɂĉ��H���Ċi�[ *
018430*********************************************
018440* �������
018450             MOVE SPACE                     TO �������̂v
018460             MOVE 03                        TO ���|�敪�R�[�h
018470             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
018480             READ ���̃}�X�^
018490             INVALID KEY
018500                 MOVE SPACE    TO �������̂v
018510             NOT INVALID KEY
018520*                 MOVE ���|���� TO �������̂v
018530                 MOVE ���|�������� TO �������̂v
018540             END-READ
018550* ����
020710             MOVE SPACE                    TO �������v(���ʂb�m�s)
018570             PERFORM ���̖�������
018630             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
018640             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
018650             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
018660*             IF ���|�����N(���ʂb�m�s) NOT = ZERO 
018670*                MOVE "."                   TO �����N������؂v(���ʂb�m�s)
018680*             END-IF
018690*
018700             MOVE ���|�J�n�N(���ʂb�m�s)   TO �{�p�J�n�N�v(���ʂb�m�s)
018710             MOVE ���|�J�n��(���ʂb�m�s)   TO �{�p�J�n���v(���ʂb�m�s)
018720             MOVE ���|�J�n��(���ʂb�m�s)   TO �{�p�J�n���v(���ʂb�m�s)
018730*             IF ���|�J�n�N(���ʂb�m�s) NOT = ZERO 
018740*                MOVE "."                   TO �{�J�N������؂v(���ʂb�m�s)
018750*             END-IF
018760*
018770             MOVE ���|�I���N(���ʂb�m�s)   TO �{�p�I���N�v(���ʂb�m�s)
018780             MOVE ���|�I����(���ʂb�m�s)   TO �{�p�I�����v(���ʂb�m�s)
018790             MOVE ���|�I����(���ʂb�m�s)   TO �{�p�I�����v(���ʂb�m�s)
018800*             IF ���|�I���N(���ʂb�m�s) NOT = ZERO 
018810*                MOVE "."                   TO �{�I�N������؂v(���ʂb�m�s)
018820*             END-IF
018830*�]�A
018840             EVALUATE ���|�]�A�敪(���ʂb�m�s)
018850             WHEN 1
018860             WHEN 2
018870                 MOVE NC"��"               TO �����`�F�b�N�v(���ʂb�m�s)
018880             WHEN 3
018890                 MOVE NC"��"               TO ���~�`�F�b�N�v(���ʂb�m�s)
018900             WHEN 4
018910                 MOVE NC"��"               TO �]��`�F�b�N�v(���ʂb�m�s)
018920             END-EVALUATE
018930*             IF ���|�]�A�敪(���ʂb�m�s) NOT = 9
018940*                 MOVE 04                        TO ���|�敪�R�[�h
018950*                 MOVE ���|�]�A�敪(���ʂb�m�s)  TO ���|���̃R�[�h
018960*                 READ ���̃}�X�^
018970*                 INVALID KEY
018980*                     MOVE SPACE    TO �]�A�v(���ʂb�m�s)
018990*                 NOT INVALID KEY
019000*                     MOVE ���|���� TO �]�A�v(���ʂb�m�s)
019010*                 END-READ
019020*             END-IF
019030*hilo---------------------------------
019040*         MOVE NC"��"               TO �����`�F�b�N�v(���ʂb�m�s)
019050*         MOVE NC"��"               TO ���~�`�F�b�N�v(���ʂb�m�s)
019060*         MOVE NC"��"               TO �]��`�F�b�N�v(���ʂb�m�s)
019070*-------------------------------------
019080         END-PERFORM
019090     END-READ.
019100*================================================================*
019110 �{�p�L�^�擾 SECTION.
019120*
019130************************************************************
019140* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾           *
019150* �� �{�p�J�n�N����...�Y�����镔�ʂɑ΂��ē����ŏ��̎{�p�� *
019160************************************************************
019170     MOVE �{�p�a��v�q  TO �{�L�|�{�p�a��.
019180     MOVE �{�p�N�v�q    TO �{�L�|�{�p�N.
019190     MOVE �{�p���v�q    TO �{�L�|�{�p��.
019200     MOVE ZERO            TO �{�L�|�{�p��.
019210     MOVE ZERO            TO �{�L�|���Ҕԍ�.
019220     MOVE SPACE           TO �{�L�|�}��.
019230     START �{�p�L�^�e   KEY IS >= �{�L�|�{�p�a��N����
019240                                  �{�L�|���҃R�[�h.
019250     IF ��ԃL�[ = "00"
019260         MOVE SPACE  TO �I���t���O�Q
019270         PERFORM �{�p�L�^�e�Ǎ�
019280         PERFORM UNTIL ( �I���t���O�Q       = "YES" ) OR
019290                       ( �{�L�|�{�p�a�� NOT = �{�p�a��v�q ) OR
019300                       ( �{�L�|�{�p�N   NOT = �{�p�N�v�q   ) OR
019310                       ( �{�L�|�{�p��   NOT = �{�p���v�q   )
019320*            **************
019330*            * �J�n�N���� *
019340*            **************
019350             PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
019360                     UNTIL ( ���ʂb�m�s > ���ʐ��v )
019370            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
019380            WHEN �{�L�|�������(���ʂb�m�s) = ������ʂv(���ʂb�m�s) ALSO
019390                 �{�L�|����(���ʂb�m�s)     = ���ʂv(���ʂb�m�s)     ALSO
019400                 �{�L�|���E�敪(���ʂb�m�s) = ���E�敪�v(���ʂb�m�s) ALSO
019410                 �{�L�|�����ʒu�ԍ�(���ʂb�m�s)
019420                                       = �����ʒu�ԍ��v(���ʂb�m�s)  ALSO
019430                 �J�n�N�����擾�t���O(���ʂb�m�s) = SPACE
019440                   MOVE �{�L�|�{�p�N     TO �{�p�J�n�N�v(���ʂb�m�s)
019450                   MOVE �{�L�|�{�p��     TO �{�p�J�n���v(���ʂb�m�s)
019460                   MOVE �{�L�|�{�p��     TO �{�p�J�n���v(���ʂb�m�s)
019470                   MOVE "YES"       TO �J�n�N�����擾�t���O(���ʂb�m�s)
019480*
019490            WHEN OTHER
019500                CONTINUE
019510            END-EVALUATE
019520             END-PERFORM
019530             PERFORM �{�p�L�^�e�Ǎ�
019540         END-PERFORM
019550     END-IF.
019560*================================================================*
019570 �{�p�L�^�e�Ǎ� SECTION.
019580*
019590     READ �{�p�L�^�e NEXT
019600     AT END
019610         MOVE "YES" TO �I���t���O�Q
019620     END-READ.
019630*
019640*================================================================*
019650 ���������擾 SECTION.
019660*
019670********************************************************************
019680*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
019690*  ��: �@�A �Ƃœ]��.
019700*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
019710*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
019720********************************************************************
019730     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
019740     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
019750             UNTIL ( ���ʂb�m�s > ���ʐ��v )
019760*
019770****        IF ( ���|�������Ҕԍ�(���ʂb�m�s)  NOT = ZERO )  AND
019780        IF ( ���|�����A��(���ʂb�m�s)      NOT = ZERO )
019790*
019800           IF �J�E���^ = ZERO
019810               MOVE 1   TO  �J�E���^ �J�E���^�Q
019820               MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
019830               MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
019840               MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
019850           ELSE
019860              IF ( ���|�������Ҕԍ�(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
019870                 ( ���|�����A��(���ʂb�m�s)      = �����A�Ԃb�v     )
019880                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
019890                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
019900              ELSE
019910                 COMPUTE �J�E���^ = �J�E���^  +  1
019920                 MOVE 1   TO  �J�E���^�Q
019930                 MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
019940                 MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
019950                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
019960              END-IF
019970           END-IF
019980        END-IF
019990     END-PERFORM.
020000**************************************************************************
020010*  ���������}�X�^��蕶�͎擾
020020**************************************************************************
020030     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
020040     PERFORM VARYING �J�E���^ FROM 1 BY 1
020050             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
020060** �J�Ђ� �敪 02
020070         MOVE 02                        TO �����|�敪�R�[�h
020080         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
020090         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
020100         READ ���������e
020110         NOT INVALID KEY
020120             INITIALIZE ���������v�s
020130             MOVE �����|���������b�l(1) TO  ���������P�v�s
020140             MOVE �����|���������b�l(2) TO  ���������Q�v�s
020150             MOVE �����|���������b�l(3) TO  ���������R�v�s
020160             MOVE �����|���������b�l(4) TO  ���������S�v�s
020170             MOVE �����|���������b�l(5) TO  ���������T�v�s
020180             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
020190                     UNTIL ( �J�E���^�Q > 9 )  OR 
020200                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
020210                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
020220                WHEN 1
020230                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020240                WHEN 2
020250                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020260                WHEN 3
020270                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020280                WHEN 4
020290                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020300                WHEN 5
020310                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020320                WHEN OTHER
020330                   CONTINUE
020340                END-EVALUATE
020350             END-PERFORM
020360*
020442             IF �����|�����������͋敪 = 1
020443                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
020444                        ���������P�v�s  DELIMITED BY SIZE
020445                        ���������Q�v�s  DELIMITED BY SIZE
020446                        ���������R�v�s  DELIMITED BY SIZE
020447                        ���������S�v�s  DELIMITED BY SIZE
020448                        ���������T�v�s  DELIMITED BY SIZE
020449                        INTO �����������e�����v(�J�E���^)
020450                 END-STRING
020451             ELSE
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
020460             END-IF
020461*
020462         END-READ
020463     END-PERFORM.
020470*
020480     PERFORM ���������Z�b�g.
020490*
020500*================================================================*
020510 ���������Z�b�g SECTION.
020520*
020530**************************************************************************
020540*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
020550**************************************************************************
020560     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
020570     PERFORM VARYING �J�E���^ FROM 1 BY 1
020580             UNTIL ( �J�E���^ > 9 )  OR ( �����������e�����v(�J�E���^) = SPACE )
020590*
020600          INITIALIZE �����������e�����w�v
020610          MOVE �����������e�����v(�J�E���^)   TO �����������e�����w�v
020620          IF  �����������e�P�w�v  NOT = SPACE
020630              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
020640              MOVE �����������e�P�w�v  TO ���������v(�J�E���^�Q)
020650          END-IF
020660          IF  �����������e�Q�w�v  NOT = SPACE
020670              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
020680              MOVE �����������e�Q�w�v  TO ���������v(�J�E���^�Q)
020690          END-IF
020700*          IF  �����������e�R�w�v  NOT = SPACE
020710*              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
020720*              MOVE �����������e�R�w�v  TO ���������v(�J�E���^�Q)
020730*          END-IF
020820*
020830     END-PERFORM.
020840*
020850*================================================================*
020860 ������� SECTION.
020870*
020880     EVALUATE ������[�h�e�v�q
020890     WHEN 0
020900         PERFORM ��������Q
020910         PERFORM ��������R
020920     WHEN 1
020930         PERFORM ��������Q
020940     WHEN 2
020950         PERFORM ��������R
020980     END-EVALUATE.
021080*================================================================*
021090 ��������Q SECTION.
021100*
021110     MOVE "YCH6622P"  TO  ��`�̖��o.
021120     MOVE "GRP001"   TO  ���ڌQ���o.
021130     WRITE YCH6622P.
021140*     WRITE ������R�[�h.
021150     PERFORM �G���[�����o.
021160*================================================================*
021170 ��������R SECTION.
021180*
021190     MOVE "YCH6622P"  TO  ��`�̖��o.
021200     MOVE "GRP002"   TO  ���ڌQ���o.
021210     WRITE YCH6622P.
021220*     WRITE ������R�[�h.
021230     PERFORM �G���[�����o.
021310*================================================================*
021320 �G���[�����o SECTION.
021330*
021340     IF �ʒm���o NOT = "00"
021350         DISPLAY NC"���[�G���["              UPON CONS
021360         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
021370         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
021380         DISPLAY NC"�g������o�F" �g������o UPON CONS
021390         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
021400                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
021410         ACCEPT  �L�[���� FROM CONS
021420         PERFORM �t�@�C����
021430         EXIT PROGRAM
021440     END-IF.
021450*================================================================*
021460 ���̖������� SECTION.
021470*
           EVALUATE ��|�ی����
           WHEN 05
               MOVE 2          TO ���Z�|���Z���
           WHEN 70
               MOVE 4          TO ���Z�|���Z���
           WHEN 80
               MOVE 5          TO ���Z�|���Z���
           WHEN 85
               MOVE 7          TO ���Z�|���Z���
           WHEN 90
               MOVE 6          TO ���Z�|���Z���
           WHEN 91
               MOVE 8          TO ���Z�|���Z���
           WHEN OTHER
               MOVE 1          TO ���Z�|���Z���
           END-EVALUATE.
019550     MOVE ��|�{�p�a�� TO ���Z�|�{�p�a��.
019560     MOVE ��|�{�p�N   TO ���Z�|�{�p�N.
019570     MOVE ��|�{�p��   TO ���Z�|�{�p��.
019580     MOVE ��|���Ҕԍ� TO ���Z�|���Ҕԍ�.
019590     MOVE ��|�}��     TO ���Z�|�}��.
019600     READ ���Z�v�g�e
019630     NOT INVALID KEY
006480*
006490         STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
006500                ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
009980                �������̂v                    DELIMITED BY SPACE
006520           INTO �������v(���ʂb�m�s)
006570         END-STRING
021580     END-READ.
021590*
022010*================================================================*
022020  �������Ԏ擾 SECTION.
022030*
022040     EVALUATE �{�p���v�q
022050     WHEN 4
022060     WHEN 6
022070     WHEN 9
022080     WHEN 11
022090         MOVE 30 TO �����I�����v
022100     WHEN 2
022110         COMPUTE �Ώې���v = �a��I���N�v + �{�p�N�v�q
022120         DIVIDE 4 INTO �Ώې���v GIVING    ���v
022130                                  REMAINDER �]�v
022140         END-DIVIDE
022150         IF �]�v = ZERO
022160             MOVE 29 TO �����I�����v
022170         ELSE
022180             MOVE 28 TO �����I�����v
022190         END-IF
022200     WHEN 1
022210     WHEN 3
022220     WHEN 5
022230     WHEN 7
022240     WHEN 8
022250     WHEN 10
022260     WHEN 12
022270         MOVE 31 TO �����I�����v
022280     WHEN OTHER
022290         MOVE  NC"���m�̃G���[����" TO �A���|���b�Z�[�W
022300         CALL   "MSG001"
022310         CANCEL "MSG001"
022320     END-EVALUATE.
022330*================================================================*
022340******************************************************************
022350 END PROGRAM YCH6622.
022360******************************************************************
