000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YIW662.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*         �A�C���p 
000100*         �J���e����i�_+����޳�ޔŁj
000110*         MED = YAW660 YIW662P
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2015-09-15
000140 DATE-COMPILED.          2015-09-15
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
000510     SELECT  ���Z�v�g�e      ASSIGN      TO        RECEPTL
000520                             ORGANIZATION             IS  INDEXED
000530                             ACCESS MODE              IS  DYNAMIC
000540                             RECORD KEY               IS  ���Z�|�{�p�a��N��
000550                                                          ���Z�|���҃R�[�h
000560                                                          ���Z�|���Z���
000570                             ALTERNATE RECORD KEY     IS  ���Z�|���҃R�[�h
000580                                                          ���Z�|�{�p�a��N��
000590                                                          ���Z�|���Z���
000600                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000610                                                          ���Z�|�{�p�a��N��
000620                                                          ���Z�|���҃R�[�h
000630                                                          ���Z�|���Z���
000640                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000650                                                          ���Z�|���Z���
000660                                                          ���Z�|�����ی��Ҕԍ�
000670                                                          ���Z�|���҃R�[�h
000680                                                          ���Z�|�{�p�a��N��
000690                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000700                                                          ���Z�|�����ی��Ҕԍ�
000710                                                          ���Z�|���҃R�[�h
000720                                                          ���Z�|���Z���
000730                                                          ���Z�|�{�p�a��N��
000740                             FILE STATUS              IS  ��ԃL�[
000750                             LOCK        MODE         IS  AUTOMATIC.
000760     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000770                             ORGANIZATION             IS  INDEXED
000780                             ACCESS MODE              IS  DYNAMIC
000790                             RECORD KEY               IS  ���|����敪
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
001210                             RECORD KEY               IS ���|�{�p�a��N��
001220                                                         ���|���҃R�[�h
001230                             ALTERNATE RECORD KEY     IS ���|���҃R�[�h
001240                                                         ���|�{�p�a��N��
001250                             FILE STATUS              IS  ��ԃL�[
001260                             LOCK        MODE         IS  AUTOMATIC.
001270     SELECT  ���������e      ASSIGN      TO        HUGEINL
001280                             ORGANIZATION             IS  INDEXED
001290                             ACCESS MODE              IS  DYNAMIC
001300                             RECORD KEY               IS  �����|�敪�R�[�h
001310                                                          �����|���������R�[�h
001320                             FILE STATUS              IS  ��ԃL�[
001330                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  �s�|������
001380                                                          �s�|�s�����ԍ�
001390                             ALTERNATE RECORD KEY     IS  �s�|������
001400                                                          �s�|�s��������
001410                                                          �s�|�s�����ԍ�
001420                             FILE STATUS              IS  ��ԃL�[
001430                             LOCK        MODE         IS  AUTOMATIC.
000241     SELECT  ���ۏ��e      ASSIGN      TO        SEIHOJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS ���ہ|�{�p�a��N��
000245                                                         ���ہ|���҃R�[�h
000255                             ALTERNATE RECORD KEY     IS ���ہ|���҃R�[�h
000265                                                         ���ہ|�{�p�a��N��
000277                             FILE STATUS              IS ��ԃL�[
000278                             LOCK        MODE         IS AUTOMATIC.
000241     SELECT  �����ӏ��e    ASSIGN      TO        JIBAIJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS �����|�{�p�a��N��
000245                                                         �����|���҃R�[�h
000277                             ALTERNATE RECORD KEY     IS �����|���҃R�[�h
000278                                                         �����|�{�p�a��N��
000279                             FILE STATUS              IS ��ԃL�[
000280                             LOCK        MODE         IS AUTOMATIC.
000102     SELECT  �ی���Ѓ}�X�^  ASSIGN      TO           HOKCOML
000103                             ORGANIZATION             IS  INDEXED
000104                             ACCESS MODE              IS  DYNAMIC
000105                             RECORD KEY               IS  �ی���|�ی���Дԍ�
000108                             ALTERNATE RECORD KEY     IS  �ی���|�ی���ЃJ�i
000110                                                          �ی���|�ی���Дԍ�
000112                             FILE STATUS              IS  ��ԃL�[
000113                             LOCK        MODE         IS  AUTOMATIC.
001440     SELECT  ���Ə��}�X�^    ASSIGN      TO        JIGYOSL
001450                             ORGANIZATION             IS  INDEXED
001460                             ACCESS MODE              IS  DYNAMIC
001470                             RECORD KEY               IS  ���|�ی����
001480                                                          ���|�ی��Ҕԍ�
001490                                                          ���|�L��
001500                             FILE STATUS              IS  ��ԃL�[
001510                             LOCK        MODE         IS  AUTOMATIC.
001520     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
001530                             SYMBOLIC    DESTINATION  IS "PRT"
001540                             FORMAT                   IS  ��`�̖��o
001550                             GROUP                    IS  ���ڌQ���o
001560                             PROCESSING  MODE         IS  ������ʂo
001570                             UNIT        CONTROL      IS  �g������o
001580                             FILE        STATUS       IS  �ʒm���o.
001590******************************************************************
001600*                      DATA DIVISION                             *
001610******************************************************************
001620 DATA                    DIVISION.
001630 FILE                    SECTION.
001640*                           �m�q�k��  �R�Q�O�n
001650 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001660     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001670*                           �m�q�k��  �P�Q�W�n
001680 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001690     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001700*                           �m�q�k��  �P�Q�W�n
001710 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001720     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001730*                          �m�q�k��  �P�T�R�U�n
001740 FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
001750     COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001760*                           �m�q�k��  �Q�T�U�n
001770 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001780     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001790*                           �m�q�k��  �R�Q�O�n
001800 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001810     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001820*                           �m�q�k��  �Q�T�U�n
001830 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
001840     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
001850*                           �m�q�k��  �P�Q�W�n
001860 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001870     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001880*                           �m�q�k��  �P�Q�W�n
001890 FD  ���������e          BLOCK   CONTAINS   1   RECORDS.
001900     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
001910*                           �m�q�k��  �Q�T�U�n
001920 FD  �s�����}�X�^        BLOCK   CONTAINS   1   RECORDS.
001930     COPY SITYOSN         OF  XFDLIB  JOINING   �s   AS  PREFIX.
001080* 
000280 FD  ���ۏ��e          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   ����   AS  PREFIX.
000284*
000282 FD  �����ӏ��e        BLOCK   CONTAINS   1   RECORDS.
000283     COPY JIBAIJ      OF  XFDLIB  JOINING   ����   AS  PREFIX.
000114*
000115 FD  �ی���Ѓ}�X�^   BLOCK   CONTAINS   1   RECORDS.
000116     COPY HOKENCOM    OF  XFDLIB  JOINING   �ی���   AS  PREFIX.
001940*
001950 FD  ���Ə��}�X�^        BLOCK   CONTAINS   1   RECORDS GLOBAL.
001960     COPY JIGYOS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001970*
001980 FD  ����t�@�C��.
001990     COPY YIW662P        OF  XMDLIB.
002000*----------------------------------------------------------------*
002010******************************************************************
002020*                WORKING-STORAGE SECTION                         *
002030******************************************************************
002040 WORKING-STORAGE         SECTION.
002050 01 �L�[����                           PIC X     VALUE SPACE.
002060 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
002070 01 �I���t���O                         PIC X(3)  VALUE SPACE.
002080 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
002090 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002100 01 �ڍׂb�m�s                         PIC 9     VALUE ZERO.
002110 01 �����b�m�s                         PIC 9(2)  VALUE ZERO.
002120 01 �����t���O                         PIC X(3)  VALUE SPACE.
002130 01 �t�@�C����                         PIC N(2)  VALUE SPACE.
002140 01 �O�a��v                           PIC 9     VALUE ZERO.
002150 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
002160 01 �S�p��                           PIC X(2)  VALUE X"8140".
002170 01 ���p��                           PIC X(2)  VALUE X"2020".
002180 01 �������̂v                         PIC N(6)  VALUE SPACE.
002190 01 ���ʖ��̂v                         PIC N(10) VALUE SPACE.
002200 01 �����ԍ��v                         PIC 9     VALUE ZERO.
002210 01 �����ԍ��q REDEFINES �����ԍ��v.
002220    03 �����ԍ��v�P                    PIC X.
002230*
002240 01 �S�p�����ԍ��v                     PIC N     VALUE SPACE.
002250 01 �S�p�����ԍ��q REDEFINES �S�p�����ԍ��v.
002260    03 �S�p�����ԍ��v�P                PIC X(2).
002270*
002280 01 �o�[�R�[�h�敪�v                   PIC 9(1)  VALUE ZERO.
002290 01 ���S���v                           PIC 9(3) VALUE ZERO.
002300 01 �b���敪�v                         PIC 9    VALUE ZERO.
002310 01 �{�l���S���v                       PIC 9(3) VALUE ZERO.
002320 01 �Ƒ����S���v                       PIC 9(3) VALUE ZERO.
002330*
002340 01 �{�p�a��N�����b�v.
002350   03 �{�p�a��N���b�v.
002360     05 �{�p�a��b�v                   PIC 9    VALUE ZERO.
002370     05 �{�p�N���b�v.
002380        07 �{�p�N�b�v                  PIC 9(2) VALUE ZERO.
002390        07 �{�p���b�v                  PIC 9(2) VALUE ZERO.
002400   03 �{�p���b�v                       PIC 9(2) VALUE ZERO.
002410 01 �{�p����N�v                       PIC 9(4) VALUE ZERO.
002420 01 ���Ґ���N�v                       PIC 9(4) VALUE ZERO.
002430 01 ��ی��Ґ���N�v                   PIC 9(4) VALUE ZERO.
002440*
002450** ���������p
002460*
002470 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
002480 01 �J�E���^�Q                         PIC 9(2)  VALUE ZERO.
002490*
002500 01 ���������v�s.
002510    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
002520    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
002530    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
002540    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
002550    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
002560    03 ���������i���o�[�v�s.
002570       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
002580    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
002590 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
002600 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
002610 01 ���������s�a�k.
002620    03 ���������R�[�h�s�a�k            OCCURS 9.
002630       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
002640       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
002650       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
002660 01 �����������e�v.
002670    03 �����������e�����v              PIC X(318) OCCURS 9 VALUE SPACE.
002680    03 �����������e�����w�v.
002690       05 �����������e�P�w�v           PIC X(64)  VALUE SPACE.
002700       05 �����������e�Q�w�v           PIC X(64)  VALUE SPACE.
002710       05 �����������e�R�w�v           PIC X(64)  VALUE SPACE.
002720       05 �����������e�S�w�v           PIC X(64)  VALUE SPACE.
002720       05 �����������e�T�w�v           PIC X(62)  VALUE SPACE.
002730*
002740*/���������̏ڍ׈��/0610
002750 01 ���������v�e�[�u���Q.
002760    03 ���������v�Q                    PIC X(64) OCCURS 45 VALUE SPACE.
002770** ������������敪�p
002780 01 ������������敪�v                 PIC 9 VALUE ZERO.
002790*
002800 01 �����������v.
002810    03 ���������R�[�h�v  OCCURS   9.
002820       09 �����������Ҕԍ��v           PIC 9(6) VALUE ZERO.
002830       09 ���������A�Ԃv               PIC 9(4) VALUE ZERO.
002840*
002850* �Еۗp
002860 01 �ڔ���敪�v                       PIC 9     VALUE ZERO.
002870*
002880****************
002890* �A�����ڑҔ� *
002900****************
002910*    ************
002920*    * ����L�[ *
002930*    ************
002940 01 �Ώۃf�[�^�v�q.
002950    03 �{�p�a��N���v�q.
002960       05 �{�p�a��v�q                  PIC 9(1)  VALUE ZERO.
002970       05 �{�p�N�v�q                    PIC 9(2)  VALUE ZERO.
002980       05 �{�p���v�q                    PIC 9(2)  VALUE ZERO.
002990    03 �ی���ʂv�q                     PIC 9(2)  VALUE ZERO.
003000    03 �ی��Ҕԍ��v�q                   PIC X(10) VALUE SPACE.
003010    03 �{�l�Ƒ��敪�v�q                 PIC 9(1)  VALUE ZERO.
003020    03 ��ی��҃J�i�v�q                 PIC X(20) VALUE SPACE.
003030    03 ���҃R�[�h�v�q.
003040       05 ���Ҕԍ��v�q                  PIC 9(6)  VALUE ZERO.
003050       05 �}�Ԃv�q                      PIC X(1)  VALUE SPACE.
003060    03 ������[�h�e�v�q                 PIC 9(1)  VALUE ZERO.
003070    03 ��o�N�����v�q.
003080       05 ��o�N�v�q                    PIC 9(2)  VALUE ZERO.
003090       05 ��o���v�q                    PIC 9(2)  VALUE ZERO.
003100       05 ��o���v�q                    PIC 9(2)  VALUE ZERO.
003110    03 �i���v�q                         PIC 9(1)  VALUE ZERO.
003120************
003130* ������� *
003140************
003150*
003160 01 ���S�����v                          PIC Z9    VALUE ZERO.
003160 01 ���S�v                              PIC X(8)  VALUE SPACE.
003170**************
003180* ��f�ҏ�� *
003190**************
003200 01 ��f�ҏ��v.
003210    03 ���҃R�[�h�v.
003220       05 ���Ҕԍ��v                   PIC 9(6)  VALUE ZERO.
003230       05 �}�Ԃv                       PIC X(1)  VALUE SPACE.
003240    03 �ی���ʖ��̂v                  PIC N(4)  VALUE SPACE.
003330    03 �s�����ԍ��v.
003340       05 ����s�����ԍ��v             PIC X(8)  VALUE SPACE.
003350       05 FILLER                       PIC X(2).
003380    03 ��v�Ҕԍ��v.
003390       05 �����v�Ҕԍ��v             PIC X(7)  VALUE SPACE.
003400       05 FILLER                       PIC X(13).
003360    03 ��p���S�Ҕԍ��v.
003370       05 �����p���S�Ҕԍ��v         PIC X(8)  VALUE SPACE.
003380       05 FILLER                       PIC X(2).
003390    03 ��v�Ҕԍ������v.
003400       05 �����v�Ҕԍ������v         PIC X(8)  VALUE SPACE.
003410       05 FILLER                       PIC X(12).
003420    03 �L���v                          PIC X(24)  VALUE SPACE.
003430    03 �ԍ��v.
003440       05 ����ԍ��v                   PIC X(30)  VALUE SPACE.
003450*       05 FILLER                       PIC X(18).
003460    03 ��ی��ҏ��v.
003470       05 ��ی��҃J�i�v               PIC X(50)  VALUE SPACE.
003480       05 ��ی��Ҏ����v               PIC X(50)  VALUE SPACE.
003490*
003500       05 ��ی��Ґ��ʃ`�F�b�N�v.
003510          07 ��ی��Ғj�`�F�b�N�v      PIC N(1)  VALUE SPACE.
003520          07 ��ی��ҏ��`�F�b�N�v      PIC N(1)  VALUE SPACE.
003530*       05 ��ی��Ґ��ʂv               PIC 9     VALUE ZERO.
003540       05 ��ی��Ҙa��`�F�b�N�v.
003550          07 ��ی��Җ����`�F�b�N�v    PIC N(1)  VALUE SPACE.
003560          07 ��ی��ґ吳�`�F�b�N�v    PIC N(1)  VALUE SPACE.
003570          07 ��ی��ҏ��a�`�F�b�N�v    PIC N(1)  VALUE SPACE.
003580          07 ��ی��ҕ����`�F�b�N�v    PIC N(1)  VALUE SPACE.
003580          07 ��ی��җߘa�`�F�b�N�v    PIC N(1)  VALUE SPACE.
003590       05 ��ی��Ґ��N�����v.
003600          07 ��ی��Ҙa��v            PIC 9     VALUE ZERO.
003610          07 ��ی��ҔN�v              PIC 9(2)  VALUE ZERO.
003620          07 ��ی��Ҍ��v              PIC 9(2)  VALUE ZERO.
003630          07 ��ی��ғ��v              PIC 9(2)  VALUE ZERO.
003640       05 ��ی��ҔN��v               PIC 9(3)   VALUE ZERO.
003640       05 �����ی��ҔN��v           PIC X(7)  VALUE SPACE.
003650*
003660       05 ��ی��җX�֔ԍ��v.
003670          07 ��ی��җX�֔ԍ��P�v      PIC X(3)  VALUE SPACE.
003680          07 ��ی��җX�֔ԍ��Q�v      PIC X(4)  VALUE SPACE.
003690*       05 ��ی��ҏZ���v.
003700*          07 �����ی��ҏZ���v        PIC X(80) VALUE SPACE.
003710       05 ��ی��ҏZ���v.
003720          07 �����ی��ҏZ���P�v      PIC X(40) VALUE SPACE.
003730          07 �����ی��ҏZ���Q�v      PIC X(40) VALUE SPACE.
003740       05 ��ی��ғd�b�ԍ��v           PIC X(15) VALUE SPACE.
003750       05 �����d�b�ԍ��v.
003760          07 �����d�b�ԍ��P�v          PIC X(5)  VALUE SPACE.
003770          07 �����d�b�ԍ��Q�v          PIC X(5)  VALUE SPACE.
003780          07 �����d�b�ԍ��R�v          PIC X(5)  VALUE SPACE.
003790*          07 �s�O�ǔԂv                PIC X(5)  VALUE SPACE.
003800*          07 �����ǔԂv                PIC X(5)  VALUE SPACE.
003810*          07 �����ԍ��v                PIC X(5)  VALUE SPACE.
003820    03 ���i�擾�N�����v.
003830       05 ���i�擾�����v               PIC N(2)  VALUE SPACE.
003840       05 ���i�擾�a��v               PIC 9(1)  VALUE ZERO.
003850       05 ���i�擾�N�v                 PIC 9(2)  VALUE ZERO.
003860       05 ���i�擾���v                 PIC 9(2)  VALUE ZERO.
003870       05 ���i�擾���v                 PIC 9(2)  VALUE ZERO.
003880    03 �L�������N�����v.
003890       05 �L�����������v               PIC N(2)  VALUE SPACE.
003900       05 �L�������a��v               PIC 9(1)  VALUE ZERO.
003910       05 �L�������N�v                 PIC 9(2)  VALUE ZERO.
003920       05 �L���������v                 PIC 9(2)  VALUE ZERO.
003930       05 �L���������v                 PIC 9(2)  VALUE ZERO.
003940       05 ���i�a��`�F�b�N�v.
003950          07 ���i���a�`�F�b�N�v        PIC N(1)  VALUE SPACE.
003960          07 ���i�����`�F�b�N�v        PIC N(1)  VALUE SPACE.
003970    03 ���ҏ��v.
003980       05 ���҃J�i�v                   PIC X(50)  VALUE SPACE.
003990       05 ���Ҏ����v                   PIC X(50)  VALUE SPACE.
004000       05 ���Ґ��ʃ`�F�b�N�v.
004010          07 ���Ғj�`�F�b�N�v          PIC N(1)  VALUE SPACE.
004020          07 ���ҏ��`�F�b�N�v          PIC N(1)  VALUE SPACE.
004030       05 �����v.
004040          07 ��������v                PIC N(4)  VALUE SPACE.
004050          07 FILLER                    PIC X(4).
004060       05 �����`�F�b�N�v.
004070          07 �{�l�`�F�b�N�v            PIC N(1)  VALUE SPACE.
004080          07 �Ƒ��`�F�b�N�v            PIC N(1)  VALUE SPACE.
004090       05 �a��`�F�b�N�v.
004100          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
004110          07 �吳�`�F�b�N�v            PIC N(1)  VALUE SPACE.
004120          07 ���a�`�F�b�N�v            PIC N(1)  VALUE SPACE.
004130          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
004130          07 �ߘa�`�F�b�N�v            PIC N(1)  VALUE SPACE.
004140       05 ���ҔN�v                     PIC 9(2) VALUE ZERO.
004150       05 ���Ҍ��v                     PIC 9(2) VALUE ZERO.
004160       05 ���ғ��v                     PIC 9(2) VALUE ZERO.
004170       05 ���ҔN��v                   PIC 9(3)  VALUE ZERO.
003640       05 ������ҔN��v               PIC X(7)  VALUE SPACE.
004180       05 ���Ґ��ʂv                   PIC N(4) VALUE SPACE.
004190*
004200       05 ���җX�֔ԍ��v.
004210          07 ���җX�֔ԍ��P�v          PIC X(3)  VALUE SPACE.
004220          07 ���җX�֔ԍ��Q�v          PIC X(4)  VALUE SPACE.
004230       05 ���ҏZ���v.
004240          07 ���ҏZ���P�v              PIC X(40) VALUE SPACE.
004250          07 ���ҏZ���Q�v              PIC X(40) VALUE SPACE.
004260       05 ���ғd�b�ԍ��v               PIC X(15) VALUE SPACE.
004270    03 ���������v                      PIC X(64) OCCURS 45 VALUE SPACE.
004170    03 �N��v                          PIC ZZ9   VALUE ZERO.
004220    03 �ی���ʃ`�F�b�N�v.
004260       05 ���ۃ`�F�b�N�v               PIC N(1) VALUE SPACE.
004230       05 �Еۃ`�F�b�N�v               PIC N(1) VALUE SPACE.
004280       05 �D���`�F�b�N�v               PIC N(1) VALUE SPACE.
004270       05 ���ك`�F�b�N�v               PIC N(1) VALUE SPACE.
004240       05 �g���`�F�b�N�v               PIC N(1) VALUE SPACE.
004250       05 ���σ`�F�b�N�v               PIC N(1) VALUE SPACE.
004340       05 ���q���`�F�b�N�v             PIC N(1) VALUE SPACE.
004290       05 �㍂�`�F�b�N�v               PIC N(1) VALUE SPACE.
004320       05 �����`�F�b�N�v               PIC N(1) VALUE SPACE.
004330       05 ����`�F�b�N�v               PIC N(1) VALUE SPACE.
005230******************
005240* ���S���`�F�b�N *
005250******************
005260 01 ���S���`�F�b�N�v.
005270    03 �O���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
005280    03 �P���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
005290    03 �Q���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
005300    03 �R���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
004280**************
004290* ���Ə���� *
004300**************
004310 01 ���Ə����v.
004320    03 ���Ə����̂v.
004330       05 ������Ə����̂v             PIC X(60)  VALUE SPACE.
004340    03 ���Ə��X�֔ԍ��v.
004350       05 ���Ə��X�֔ԍ��P�v           PIC X(3)  VALUE SPACE.
004360       05 ���Ə��X�֔ԍ��Q�v           PIC X(4)  VALUE SPACE.
004370*    03 ���Ə��Z���v.
004380*       05 ������Ə��Z���v             PIC X(70)  VALUE SPACE.
004390*       05 FILLER                       PIC X(10).
004400    03 ���Ə��Z���v.
004410       05 ������Ə��Z���P�v           PIC X(50)  VALUE SPACE.
004420       05 ������Ə��Z���Q�v           PIC X(50)  VALUE SPACE.
004430**************
004440* �������� *
004450**************
004460 01 ��������v.
004470    03 �ی��Ҕԍ��v.
004480       05 ����ی��Ҕԍ��v             PIC X(8)  VALUE SPACE.
004490       05 FILLER                       PIC X(2).
004500    03 �����於�̂v.
004510*       05 ��������於�̂v             PIC X(40) VALUE SPACE.
004520       05 ��������於�̂v             PIC X(54) VALUE SPACE.
004530    03 �x�����v.
004540       05 ����x�����v                 PIC X(40) VALUE SPACE.
004550*    03 �����於�x�����v.
004560*       05 ��������於�x�����v         PIC X(54) VALUE SPACE.
004570*       05 FILLER                       PIC X(26).
004580    03 �����於�x�����v.
004590       05 ��������於�x�����P�v       PIC X(40) VALUE SPACE.
004600       05 ��������於�x�����Q�v       PIC X(40) VALUE SPACE.
004610    03 �ی��ҌĖ��v.
004620*       05 ����ی��ҌĖ��v             PIC N(7)  VALUE SPACE.
004630       05 ����ی��ҌĖ��v             PIC X(14)  VALUE SPACE.
004640    03 ������X�֔ԍ��v.
004650       05 ������X�֔ԍ��P�v           PIC X(3)  VALUE SPACE.
004660       05 ������X�֔ԍ��Q�v           PIC X(4)  VALUE SPACE.
004670    03 ������Z���P�v.
004680       05 ���������Z���P�v           PIC X(40) VALUE SPACE.
004690    03 ������Z���Q�v.
004700       05 ���������Z���Q�v           PIC X(35) VALUE SPACE.
004710       05 FILLER                       PIC X(5).
004720    03 ������d�b�ԍ��v                PIC X(15) VALUE SPACE.
004730****************
004740* �ڍחp���[�N *
004750****************
004760* �A��|��������s�p���[�N
004770 01 ����s�r�v                           PIC 9(1) VALUE ZERO.
004780*
004790 01 �����������r�v.
004800    03 ���������R�[�h�r�v  OCCURS   9.
004810       09 �������Ҕԍ��r�v               PIC 9(6) VALUE ZERO.
004820       09 �����A�Ԃr�v                   PIC 9(4) VALUE ZERO.
004830*
004840 01 �������r�v.
004850    03 �����f�[�^�r�v  OCCURS   9.
004860       05 ��ʃR�[�h�r�v               PIC X(4)  VALUE SPACE.
004870       05 ���ʂb�m�s�r�v                 PIC 9(1)  VALUE ZERO.
004880       05 ������ʂr�v                   PIC 9(2)  VALUE ZERO.
004890       05 ���ʂr�v                       PIC 9(2)  VALUE ZERO.
004900       05 ���E�敪�r�v                   PIC 9(1)  VALUE ZERO.
004910       05 �����ʒu�ԍ��r�v               PIC 9(2)  VALUE ZERO.
004920       05 �������r�v                     PIC N(18) VALUE SPACE.
004930       05 �����N�����r�v.
004940          07 �����a��r�v                PIC 9(1)  VALUE ZERO.
004950          07 �����N�r�v                  PIC 9(2)  VALUE ZERO.
004960          07 �������r�v                  PIC 9(2)  VALUE ZERO.
004970          07 �������r�v                  PIC 9(2)  VALUE ZERO.
004980          07 �����N������؂r�v          PIC X(1)  VALUE SPACE.
004990       05 �{�p�J�n�N�����r�v.
005000          07 �{�p�J�n�a��r�v            PIC 9(1)  VALUE ZERO.
005010          07 �{�p�J�n�N�r�v              PIC 9(2)  VALUE ZERO.
005020          07 �{�p�J�n���r�v              PIC 9(2)  VALUE ZERO.
005030          07 �{�p�J�n���r�v              PIC 9(2)  VALUE ZERO.
005040          07 �{�J�N������؂r�v          PIC X(1)  VALUE SPACE.
005050       05 �{�p�I���N�����r�v.
005060          07 �{�p�I���a��r�v            PIC 9(1)  VALUE ZERO.
005070          07 �{�p�I���N�r�v              PIC 9(2)  VALUE ZERO.
005080          07 �{�p�I�����r�v              PIC 9(2)  VALUE ZERO.
005090          07 �{�p�I�����r�v              PIC 9(2)  VALUE ZERO.
005100          07 �{�I�N������؂r�v          PIC X(1)  VALUE SPACE.
005110       05 �J�n�N�����擾�t���O�r         PIC X(3)  VALUE SPACE.
005120       05 �]�A�r�v                       PIC N(4)  VALUE SPACE.
005130       05 �]�A�`�F�b�N�r�v.
005140          07 �����`�F�b�N�r�v            PIC N(1)  VALUE SPACE.
005150          07 ���~�`�F�b�N�r�v            PIC N(1)  VALUE SPACE.
005160          07 �p���`�F�b�N�r�v            PIC N(1)  VALUE SPACE.
005170          07 �]��`�F�b�N�r�v            PIC N(1)  VALUE SPACE.
005180       05 �{�p�݌v�񐔂r�v               PIC 9(4)  VALUE ZERO.
005280       05 �����r�v                       PIC 9(2)  VALUE ZERO.
005280       05 �񐔂r�v                       PIC 9(2)  VALUE ZERO.
005190****************
005200* �����f�[�^�e *
005210****************
005220 01 �������v.
005230    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
005240    03 �����f�[�^���v  OCCURS   9.
005250       05 ��ʃR�[�h�v               PIC X(4)  VALUE SPACE.
005260       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
005270       05 ������ʂv                   PIC 9(2)  VALUE ZERO.
005280       05 ���ʂv                       PIC 9(2)  VALUE ZERO.
005290       05 ���E�敪�v                   PIC 9(1)  VALUE ZERO.
005300       05 �����ʒu�ԍ��v               PIC 9(2)  VALUE ZERO.
005310       05 �������v                     PIC N(18) VALUE SPACE.
005320       05 �����N�����v.
005330          07 �����a��v                PIC 9(1)  VALUE ZERO.
005340          07 �����N�v                  PIC 9(2)  VALUE ZERO.
005350          07 �������v                  PIC 9(2)  VALUE ZERO.
005360          07 �������v                  PIC 9(2)  VALUE ZERO.
005370          07 �����N������؂v          PIC X(1)  VALUE SPACE.
005380       05 �{�p�����N�����v.
005390          07 �{�p�����a��v            PIC 9(1)  VALUE ZERO.
005400          07 �{�p�����N�v              PIC 9(2)  VALUE ZERO.
005410          07 �{�p�������v              PIC 9(2)  VALUE ZERO.
005420          07 �{�p�������v              PIC 9(2)  VALUE ZERO.
005430          07 �{���N������؂v          PIC X(1)  VALUE SPACE.
005380       05 �{�p�J�n�N�����v.
005390          07 �{�p�J�n�a��v            PIC 9(1)  VALUE ZERO.
005400          07 �{�p�J�n�N�v              PIC 9(2)  VALUE ZERO.
005410          07 �{�p�J�n���v              PIC 9(2)  VALUE ZERO.
005420          07 �{�p�J�n���v              PIC 9(2)  VALUE ZERO.
005430          07 �{�J�N������؂v          PIC X(1)  VALUE SPACE.
005440       05 �{�p�I���N�����v.
005450          07 �{�p�I���a��v            PIC 9(1)  VALUE ZERO.
005460          07 �{�p�I���N�v              PIC 9(2)  VALUE ZERO.
005470          07 �{�p�I�����v              PIC 9(2)  VALUE ZERO.
005480          07 �{�p�I�����v              PIC 9(2)  VALUE ZERO.
005490          07 �{�I�N������؂v          PIC X(1)  VALUE SPACE.
005500       05 �J�n�N�����擾�t���O         PIC X(3)  VALUE SPACE.
005510       05 �]�A�v                       PIC N(4)  VALUE SPACE.
005520       05 �]�A�`�F�b�N�v.
005530          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
005540          07 ���~�`�F�b�N�v            PIC N(1)  VALUE SPACE.
005550          07 �p���`�F�b�N�v            PIC N(1)  VALUE SPACE.
005560          07 �]��`�F�b�N�v            PIC N(1)  VALUE SPACE.
005280       05 �����v                       PIC 9(2)  VALUE ZERO.
005280       05 �񐔂v                       PIC 9(2)  VALUE ZERO.
005570*
005660**********
005670* ���v�s *
005680**********
005690 01 ���v�s�v.
005700    03 �����I�����v                    PIC 9(2) VALUE ZERO.
005710    03 �Ώې���v                      PIC 9(4) VALUE ZERO.
005720    03 ���v                            PIC 9(3) VALUE ZERO.
005730    03 �]�v                            PIC 9(3) VALUE ZERO.
005740*/�N�������ی��҃f�[�^�̃��[�N
005750 01 �ە��S���敪�v                     PIC 9    VALUE ZERO.
005760 01 �ۖ{�l���S���v                     PIC 9(3) VALUE ZERO.
005770 01 �ۉƑ����S���v                     PIC 9(3) VALUE ZERO.
005780 01 �ۖ{�l���S�����v                   PIC 9(3) VALUE ZERO.
005790 01 �ۉƑ����S�����v                   PIC 9(3) VALUE ZERO.
005800*
005810 01 ��r�a��N���v.
005820    03 ��r�a��v                      PIC 9    VALUE ZERO.
005830    03 ��r�N�v                        PIC 9(2) VALUE ZERO.
005840    03 ��r���v                        PIC 9(2) VALUE ZERO.
005850*
005860*/�V�l�ی������̑Ή�0101
005870 01 �V�l���S�v�Z�敪�v                 PIC 9    VALUE ZERO.
005880 01 ���S���p��ʂv                     PIC 9(2) VALUE ZERO.
005890 01 �������S�敪�v                     PIC 9(2) VALUE ZERO.
005900*
005910** �������S���p(14/10�`)
005920 01 �������S���v�v.
005930    03 �������S���v                    PIC 9(3) VALUE ZERO.
005940    03 �������S���v�P                  PIC 9    VALUE ZERO.
005950    03 �������S�������v�P              PIC X    VALUE SPACE.
005960    03 �������S���\���v                PIC X(6) VALUE SPACE.
005970*
005980******************************************************************
005990 01 �������.
006000     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
006010     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
006020     03 ������ʂo                     PIC X(2) VALUE SPACE.
006030     03 �g������o.
006040         05 �[������o.
006050             07 �ړ������o             PIC X(1)  VALUE SPACE.
006060             07 �ړ��s���o             PIC 9(3)  VALUE ZERO.
006070         05 �ڍא���o                 PIC X(2)  VALUE SPACE.
006080     03 �ʒm���o                     PIC X(2)  VALUE SPACE.
006090     03 ���j�b�g���o                   PIC X(8)  VALUE SPACE.
006260*
006270* C �A�g�p
006280 01  �����P�v        PIC X(4096).
006290 01  �����Q�v        PIC X(512).
006300 01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
006301*
006302 01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
006303*
006310******************************************************************
006320*                          �A������                              *
006330******************************************************************
006340*
006350**********************
006360* ���b�Z�[�W�\���L�[ *
006370**********************
006380 01 �A���|�L�[ IS EXTERNAL.
006390    03  �A���|���b�Z�[�W                 PIC N(20).
006400*
006410************
006420* ����L�[ *
006430************
006440 01 �A��|�Ώۃf�[�^ IS EXTERNAL.
006450    03 �A��|�{�p�a��N��.
006460       05 �A��|�{�p�a��                  PIC 9(1).
006470       05 �A��|�{�p�N                    PIC 9(2).
006480       05 �A��|�{�p��                    PIC 9(2).
006490    03 �A��|�ی����                     PIC 9(2).
006500    03 �A��|�ی��Ҕԍ�                   PIC X(10).
006510    03 �A��|�{�l�Ƒ��敪                 PIC 9(1).
006520    03 �A��|��ی��҃J�i                 PIC X(20).
006530    03 �A��|���҃R�[�h.
006540       05 �A��|���Ҕԍ�                  PIC 9(6).
006550       05 �A��|�}��                      PIC X(1).
006560    03 �A��|������[�h�e                 PIC 9(1).
006570*/�ڍ�
006580    03 �A��|�ی��؈���敪               PIC 9(1).
006590    03 �A��|�����ڍ� OCCURS 7.
006600       05 �A��|��������s                PIC 9(1).
006610       05 �A��|���ʈ���敪              PIC 9(1).
006620       05 �A��|�]�A����敪              PIC 9(1).
006630       05 �A��|��������敪              PIC 9(1).
006640*/
006650 01 �A��|�Ώۃf�[�^�ǉ� IS EXTERNAL GLOBAL.
006660    03 �A��|���ʊەt��                   PIC 9(1).
006670*/�N����0304
006680*/���͉��(660,6601)�ƐV�p��(6621)�Ƃ̘A�����ځB(���p���A���ʗp�����̂o�f��ς����������ׁA�V���ɍ��܂�)
006690    03 �A��|�N�����敪                 PIC 9(1).
006700*/���������̏ڍ׃��[�h�Ή�/0610
006710    03 �A��|�����J�n�s                   PIC 9(2).
006720*/���k�x���L�ڒǉ�/080812
006730    03 �A��|���k�x���L��                 PIC 9(1).
006740*/���N���ƈ�� �O�F�g�Ȃ� �P�F���N���� �Q�F���N�̂� /0402
006750    03 �A��|�g����e                     PIC 9(1).
006760*/�������R0311
006770    03 �A��|��������e                   PIC 9(1).
006780*
006790 01 �A���|�\���t���O�U�U�O IS EXTERNAL GLOBAL.
006800    03 �A���|�v���r���[�敪               PIC 9(1).
006810* ���S���擾�p14/10�`
006820 01 �A���|���S���擾�L�[ IS EXTERNAL.
006830    03 �A���|�{�p�a��N��.
006840       05 �A���|�{�p�a��               PIC 9.
006850       05 �A���|�{�p�N��.
006860          07 �A���|�{�p�N              PIC 9(2).
006870          07 �A���|�{�p��              PIC 9(2).
006880    03 �A���|���҃R�[�h.
006890       05 �A���|���Ҕԍ�               PIC 9(6).
006900       05 �A���|�}��                   PIC X.
006910    03 �A���|���ە��S��                PIC 9(3).
006920    03 �A���|���ۖ{�̕��S��            PIC 9(3).
006930    03 �A���|���ە��S��                PIC 9(3).
006940    03 �A���|�Q�V�V���S��              PIC 9(3).
006950    03 �A���|�������S��                PIC 9(3).
006960    03 �A���|���ʗp���S��              PIC 9(3).
006970************************************
006980* �v�����^�t�@�C���쐬�p           *
006990************************************
007000 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
007010     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
007020     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
007030     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
007040     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
007050************************************
007060* �v�����^�t�@�C���쐬����p       *
007070************************************
007080 01 �g�A����o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
007090     03 �g�A����o�q�s�e�|�p�����         PIC X(8).
007100*
007102*---------------------------------------------------------*
007103* �Í������p
007104 01 �A�Í������|�Í���� IS EXTERNAL.
007105    03 �A�Í������|���͏��.
007106       05 �A�Í������|�L��               PIC X(24).
007107       05 �A�Í������|�ԍ�               PIC X(30).
007108       05 �A�Í������|�Í�������.
007109         07 �A�Í������|�Í����Ҕԍ�     PIC X(6).
007110         07 �A�Í������|�Í�����L��     PIC X.
007111         07 �A�Í������|�Í�����ԍ�     PIC X.
007112         07 �A�Í������|�Í��L��         PIC X(24).
007113         07 �A�Í������|�Í��ԍ�         PIC X(30).
007114    03 �A�Í������|�o�͏��.
007115       05 �A�Í������|���������L��       PIC X(24).
007116       05 �A�Í������|���������ԍ�       PIC X(30).
007117*---------------------------------------------------------*
007118* 
007120******************************************************************
007121*                      PROCEDURE  DIVISION                       *
007130******************************************************************
007140 PROCEDURE               DIVISION.
007150************
007160*           *
007170* ��������   *
007180*           *
007190************
007200     PERFORM �v�����^�t�@�C���쐬.
007210     PERFORM ������.
007220************
007230*           *
007240* �又��     *
007250*           *
007260************
007270* ���
007280     PERFORM �A�����ڑҔ�.
007290     PERFORM ����Z�b�g.
007300*****     PERFORM �e�X�g���.
007310     PERFORM �������.
007320************
007330*           *
007340* �I������   *
007350*           *
007360************
007370     PERFORM �I������.
007380     EXIT PROGRAM.
007390*
007400*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
007410*================================================================*
007420 �v�����^�t�@�C���쐬 SECTION.
007430*================================================================*
007440*   / ������ /
007450     MOVE SPACE TO �g�A�o�q�s�e�|�쐬�f�[�^.
007460     INITIALIZE �g�A�o�q�s�e�|�쐬�f�[�^.
007470     MOVE SPACE TO �g�A����o�q�s�e�|�쐬�f�[�^.
007480     INITIALIZE �g�A����o�q�s�e�|�쐬�f�[�^.
007490*
007500*
007510*--���� �ύX�ӏ� ����--------------------------------------*
007520*   �g�p����p����ʃZ�b�g
007530     MOVE "KARUTE"              TO �g�A����o�q�s�e�|�p�����.
007540*   �g�p����v�����^�t�@�C�����Z�b�g
007550     MOVE "PRTF002"             TO �g�A�o�q�s�e�|�t�@�C����.
007560*
007570*   �g�p���钠�[�v���O�������Z�b�g
007580     MOVE "YIW662"             TO �g�A�o�q�s�e�|���[�v���O������.
007590*
007600*--����-----------------------------------------------------*
007610*
007620*   / �v���r���[�敪�Z�b�g /
007630     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
007640*     MOVE 1 TO �g�A�o�q�s�e�|�v���r���[�敪.
007650*
007660     CALL   "CRTPRTF".
007670     CANCEL "CRTPRTF".
007680*
007690*================================================================*
007700 ������ SECTION.
007710*
007720     PERFORM �t�@�C���I�[�v��.
007810     PERFORM �J�����g�����擾.
007840*================================================================*
007850 �J�����g�����擾 SECTION.
007860*
007870     MOVE ZEROS TO ���|����敪.
007880     READ ������}�X�^
007890     NOT INVALID KEY
007910         MOVE ���|�J���e������������敪 TO ������������敪�v
007920         MOVE ���|�o�[�R�[�h�Q�R�Q�b�g�p�敪 TO �o�[�R�[�h�敪�v
007930     END-READ.
007940*
008290*================================================================*
008300 �A�����ڑҔ� SECTION.
008310*
008320     INITIALIZE �Ώۃf�[�^�v�q.
008330     MOVE �A��|�{�p�a��      TO �{�p�a��v�q.
008340     MOVE �A��|�{�p�N        TO �{�p�N�v�q.
008350     MOVE �A��|�{�p��        TO �{�p���v�q.
008360     MOVE �A��|�ی����      TO �ی���ʂv�q.
008370     MOVE �A��|�ی��Ҕԍ�    TO �ی��Ҕԍ��v�q.
008380     MOVE �A��|�{�l�Ƒ��敪  TO �{�l�Ƒ��敪�v�q.
008390     MOVE �A��|��ی��҃J�i  TO ��ی��҃J�i�v�q.
008400     MOVE �A��|���Ҕԍ�      TO ���Ҕԍ��v�q.
008410     MOVE �A��|�}��          TO �}�Ԃv�q.
008420     MOVE �A��|������[�h�e  TO ������[�h�e�v�q.
008430*================================================================*
008440 �t�@�C���I�[�v�� SECTION.
008450*
008460     OPEN INPUT   �ی��҃}�X�^
008470         MOVE NC"�ی���" TO �t�@�C����.
008480         PERFORM �I�[�v���`�F�b�N.
008490     OPEN INPUT   �����}�X�^
008500         MOVE NC"����" TO �t�@�C����.
008510         PERFORM �I�[�v���`�F�b�N.
008520     OPEN INPUT   ���̃}�X�^
008530         MOVE NC"����" TO �t�@�C����.
008540         PERFORM �I�[�v���`�F�b�N.
008550     OPEN INPUT   ���Z�v�g�e
008560         MOVE NC"���Z" TO �t�@�C����.
008570         PERFORM �I�[�v���`�F�b�N.
008580     OPEN INPUT   ������}�X�^
008590         MOVE NC"������" TO �t�@�C����.
008600         PERFORM �I�[�v���`�F�b�N.
008610     OPEN INPUT   ��f�ҏ��e.
008620         MOVE NC"���" TO �t�@�C����.
008630         PERFORM �I�[�v���`�F�b�N.
008640     OPEN INPUT   �{�p�L�^�e.
008650         MOVE NC"�{�L�e" TO �t�@�C����.
008660         PERFORM �I�[�v���`�F�b�N.
008670     OPEN INPUT   �����f�[�^�e.
008680         MOVE NC"����" TO �t�@�C����.
008690         PERFORM �I�[�v���`�F�b�N.
008700     OPEN INPUT   ���������e.
008710         MOVE NC"��������" TO �t�@�C����.
008720         PERFORM �I�[�v���`�F�b�N.
008730     OPEN INPUT �s�����}�X�^.
008740         MOVE NC"�s����" TO �t�@�C����.
008750         PERFORM �I�[�v���`�F�b�N.
006630     OPEN INPUT ���ۏ��e.
006640         MOVE NC"����" TO �t�@�C����.
006650         PERFORM �I�[�v���`�F�b�N.
009290     OPEN INPUT �����ӏ��e.
009300         MOVE NC"����" TO �t�@�C����.
009310         PERFORM �I�[�v���`�F�b�N.
009290     OPEN INPUT �ی���Ѓ}�X�^.
009300         MOVE NC"�ۉ�" TO �t�@�C����.
009310         PERFORM �I�[�v���`�F�b�N.
008760     OPEN INPUT ���Ə��}�X�^.
008770         MOVE NC"���Ə�" TO �t�@�C����.
008780         PERFORM �I�[�v���`�F�b�N.
008790     OPEN I-O   ����t�@�C��
008800         MOVE NC"���" TO �t�@�C����.
008810         PERFORM �G���[�����o.
008820*================================================================*
008830 �I�[�v���`�F�b�N SECTION.
008840*
008850     IF ��ԃL�[  NOT =  "00"
008860         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
008870         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
008880         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
008890                                                 UPON CONS
008900*-----------------------------------------*
008910         CALL "actcshm"  WITH C LINKAGE
008920*-----------------------------------------*
008930         ACCEPT  �L�[���� FROM CONS
008940         PERFORM �t�@�C����
008950         EXIT PROGRAM.
008960*================================================================*
008970 �t�@�C���� SECTION.
008980*
008990     CLOSE ����t�@�C��   �ی��҃}�X�^ �����}�X�^  
009000           ������}�X�^ ���̃}�X�^   �s�����}�X�^
009010           ���Z�v�g�e     ��f�ҏ��e �{�p�L�^�e
009020           �����f�[�^�e   ���Ə��}�X�^ ���������e
                 ���ۏ��e     �����ӏ��e �ی���Ѓ}�X�^.
009030*================================================================*
009040 �I������ SECTION.
009050*
009060     PERFORM �t�@�C����.
009070*================================================================*
009080 ����Z�b�g SECTION.
009090*
009100     EVALUATE ������[�h�e�v�q
009110     WHEN 0
009120         PERFORM ����Z�b�g�P
009130     WHEN 1
009140         PERFORM ����Z�b�g�Q
009150     WHEN 2
009160         PERFORM ����Z�b�g�R
009170*/�ڍ׃��[�h�p
009180     WHEN 4
009190         PERFORM ����Z�b�g�T
009200     END-EVALUATE.
009210*
009220*================================================================*
009230 ����Z�b�g�P SECTION.
009240*
009250     INITIALIZE YIW662P.
009260     INITIALIZE ��f�ҏ��v.
009270     INITIALIZE ���Ə����v.
009280     INITIALIZE ��������v.
009290     INITIALIZE �������v.
009320     PERFORM ��������擾.
009330     PERFORM ��f�ҏ��擾.
009860     PERFORM ���S���擾.
009340     PERFORM �����f�[�^�擾.
009350*
009360     IF ������������敪�v  NOT = 1 
009370        PERFORM ���������擾
009380     END-IF.
009390*
009400     PERFORM ����㕔�Z�b�g.
009410     PERFORM ��������Z�b�g.
009420*================================================================*
009430 ����Z�b�g�Q SECTION.
009440*
009450     INITIALIZE YIW662P.
009460     INITIALIZE ��f�ҏ��v.
009470     INITIALIZE ���Ə����v.
009480     INITIALIZE ��������v.
009490     INITIALIZE �������v.
009520     PERFORM ��������擾.
009530     PERFORM ��f�ҏ��擾.
009860     PERFORM ���S���擾.
009340     PERFORM �����f�[�^�擾.
009540     PERFORM ����㕔�Z�b�g.
009550*================================================================*
009560 ����Z�b�g�R SECTION.
009570*
009580     INITIALIZE YIW662P.
009590     INITIALIZE ��f�ҏ��v.
009600     INITIALIZE ���Ə����v.
009610     INITIALIZE ��������v.
009620     INITIALIZE �������v.
009630     PERFORM ��f�ҏ��擾.
009860     PERFORM ���S���擾.
009640     PERFORM �����f�[�^�擾.
009650*
009660     IF ������������敪�v  NOT = 1 
009670        PERFORM ���������擾
009680     END-IF.
009690*
009700     PERFORM ��������Z�b�g.
009710*================================================================*
009720 ����Z�b�g�T SECTION.
009730*
009740*/�ڍ׃��[�h�p
009750     INITIALIZE YIW662P.
009760     INITIALIZE ��f�ҏ��v.
009770     INITIALIZE ���Ə����v.
009780     INITIALIZE ��������v.
009790     INITIALIZE �������v.
009800     INITIALIZE �����������v.
009820     PERFORM ��������擾.
009830     PERFORM ��f�ҏ��擾.
009860     PERFORM ���S���擾.
009840     PERFORM �����f�[�^�擾.
009850*
009860     IF ������������敪�v  NOT = 1
009870        PERFORM �ڍ׃��[�h������������
009880        PERFORM ���������擾
009890     END-IF.
009900*/���������̏ڍ׈��/0610
009910     PERFORM �����s�Z�b�g.
009920*
009930     IF �A��|�ی��؈���敪 = 1
009940         PERFORM ����㕔�Z�b�g
009950     END-IF.
009960*
009970     PERFORM �ڍ׃��[�h��������.
009980     PERFORM ��������Z�b�g.
009990*
010000*================================================================*
010010 ����㕔�Z�b�g SECTION.
010020*
010030     IF �o�[�R�[�h�敪�v NOT = ZERO
010040         MOVE ���҃R�[�h�v�q         TO ���҃R�[�h
010050         MOVE " " TO EDIT-MODE OF ���҃R�[�h
010060     ELSE
010070         MOVE "X" TO EDIT-MODE OF ���҃R�[�h
010080     END-IF.
016640     MOVE �{�p�a��v�q               TO ���|�����敪.
016650     READ �����}�X�^
016680     NOT INVALID KEY
016690         MOVE ���|��������           TO �{�p�a��
016700     END-READ.
           MOVE �{�p�N�v�q                 TO �{�p�N.
           EVALUATE �{�p���v�q
           WHEN 1
               MOVE NC"��"                 TO �P��
           WHEN 2
               MOVE NC"��"                 TO �Q��
           WHEN 3
               MOVE NC"��"                 TO �R��
           WHEN 4
               MOVE NC"��"                 TO �S��
           WHEN 5
               MOVE NC"��"                 TO �T��
           WHEN 6
               MOVE NC"��"                 TO �U��
           WHEN 7
               MOVE NC"��"                 TO �V��
           WHEN 8
               MOVE NC"��"                 TO �W��
           WHEN 9
               MOVE NC"��"                 TO �X��
           WHEN 10
               MOVE NC"��"                 TO �P�O��
           WHEN 11
               MOVE NC"��"                 TO �P�P��
           WHEN 12
               MOVE NC"��"                 TO �P�Q��
           END-EVALUATE.
010090     MOVE ���Ҕԍ��v                 TO ���Ҕԍ�.
010100     MOVE �}�Ԃv                     TO �}��.
010840*
010850     IF  ����s�����ԍ��v(1:2) = "99"
010860         MOVE SPACE                  TO ���S�Ҕԍ�
010870     ELSE
010880         MOVE ����s�����ԍ��v       TO ���S�Ҕԍ�
010890     END-IF.
010900     IF (�����v�Ҕԍ��v(1:1) = "*") OR
010910        (�����v�Ҕԍ��v(1:2) = "��") 
010920         MOVE SPACE                  TO �󋋎Ҕԍ�
010930     ELSE
010940         MOVE �����v�Ҕԍ��v       TO �󋋎Ҕԍ�
010950     END-IF
010960*
010970     IF  �����p���S�Ҕԍ��v(1:2) = "99"
010980         MOVE SPACE                  TO ����S�Ҕԍ�
010990     ELSE
011000         MOVE �����p���S�Ҕԍ��v   TO ����S�Ҕԍ�
011010     END-IF.
011020*
011030     IF (�����v�Ҕԍ������v(1:1) = "*") OR
011040        (�����v�Ҕԍ������v(1:2) = "��")
011050         MOVE SPACE                  TO ��v�Ҕԍ�����
011060     ELSE
011070         MOVE �����v�Ҕԍ������v   TO ��v�Ҕԍ�����
011080     END-IF.
010110*
011540**************************
011550* �ی���ʃ`�F�b�N�Z�b�g *
011560**************************
011620     MOVE ���ۃ`�F�b�N�v             TO ���ۃ`�F�b�N.
011570     MOVE �Еۃ`�F�b�N�v             TO �Еۃ`�F�b�N.
011600     MOVE �D���`�F�b�N�v             TO �D���`�F�b�N.
011590     MOVE ���ك`�F�b�N�v             TO ���ك`�F�b�N.
011580     MOVE �g���`�F�b�N�v             TO �g���`�F�b�N.
011610     MOVE ���σ`�F�b�N�v             TO ���σ`�F�b�N.
011630     MOVE ���q���`�F�b�N�v           TO ���q���`�F�b�N.
011640     MOVE �㍂�`�F�b�N�v             TO �㍂�`�F�b�N.
011650     MOVE �����`�F�b�N�v             TO �����`�F�b�N.
011660     MOVE ����`�F�b�N�v             TO ����`�F�b�N.
      *
010770     MOVE �O���`�F�b�N�v             TO �O���`�F�b�N.
010780     MOVE �P���`�F�b�N�v             TO �P���`�F�b�N.
010790     MOVE �Q���`�F�b�N�v             TO �Q���`�F�b�N.
010800     MOVE �R���`�F�b�N�v             TO �R���`�F�b�N.
010240**************************
010250* ��ی��ҏ؏��Z�b�g   *
010260**************************
010270     IF �L���v(1:2) = "��" 
010280        MOVE  SPACE    TO  �L��
010290     ELSE
010300        MOVE �L���v    TO  �L��
010310     END-IF.
010320     IF ( ����ԍ��v(1:1) = "*"  ) OR
010330        ( ����ԍ��v(1:2) = "��" )
010340        MOVE  SPACE      TO  �ԍ�
010350     ELSE
010080        MOVE ����ԍ��v  TO  �ԍ�
010370     END-IF.
010380************************
010390* ��ی��ҏ��Z�b�g   *
010400************************
010410     MOVE ��ی��Ҏ����v             TO ��ی��Ҏ���.
010420     MOVE ��ی��҃J�i�v             TO ��ی��҃J�i.
010430*     MOVE ��ی��Ғj�`�F�b�N�v       TO ��ی��Ғj.
010440*     MOVE ��ی��ҏ��`�F�b�N�v       TO ��ی��ҏ�.
010770*     MOVE ��ی��Җ����`�F�b�N�v     TO ��ی��Җ����`�F�b�N.
010780*     MOVE ��ی��ґ吳�`�F�b�N�v     TO ��ی��ґ吳�`�F�b�N.
010790*     MOVE ��ی��ҏ��a�`�F�b�N�v     TO ��ی��ҏ��a�`�F�b�N.
010800*     MOVE ��ی��ҕ����`�F�b�N�v     TO ��ی��ҕ����`�F�b�N.
010800*     MOVE ��ی��җߘa�`�F�b�N�v     TO ��ی��җߘa�`�F�b�N.
010810*     MOVE ��ی��ҔN�v               TO ��ی��ҔN.
010820*     MOVE ��ی��Ҍ��v               TO ��ی��Ҍ�.
010830*     MOVE ��ی��ғ��v               TO ��ی��ғ�.
010840*     MOVE �����ی��ҔN��v          TO ��ی��ҔN��.
010450     MOVE ��ی��җX�֔ԍ��P�v       TO ��ی��җX�ւP.
010460     MOVE ��ی��җX�֔ԍ��Q�v       TO ��ی��җX�ւQ.
010470     MOVE "��"                       TO ��ی��җX�֋L��.
010470     MOVE "-"                        TO ��ی��җX�֋��.
010480*     MOVE ��ی��ҏZ���v             TO ��ی��ҏZ��.
010490     MOVE �����ی��ҏZ���P�v       TO ��ی��ҏZ���P.
010500     MOVE �����ی��ҏZ���Q�v       TO ��ی��ҏZ���Q.
010510*     MOVE ��ی��ғd�b�ԍ��v         TO ��ی��ғd�b�ԍ�.
010520     MOVE ���i�擾�����v             TO ���i�a��.
010530     MOVE ���i�擾�N�v               TO ���i�擾�N.
010540     MOVE ���i�擾���v               TO ���i�擾��.
010550     MOVE ���i�擾���v               TO ���i�擾��.
010560*     MOVE ���i���a�`�F�b�N�v         TO ���i���a�`�F�b�N.
010570*     MOVE ���i�����`�F�b�N�v         TO ���i�����`�F�b�N.
010580*/�L������
010520     MOVE �L�����������v             TO �L���a��.
010590     MOVE �L�������N�v               TO �L���N.
010600     MOVE �L���������v               TO �L����.
010610     MOVE �L���������v               TO �L����.
010850********************
010860* ���ҏ��Z�b�g   *
010870********************
010990*
011120     MOVE ���҃J�i�v                 TO ���҃J�i.
011130     MOVE ���Ҏ����v                 TO ���Ҏ���.
011140     MOVE ���Ғj�`�F�b�N�v           TO ���Ғj�`�F�b�N.
011150     MOVE ���ҏ��`�F�b�N�v           TO ���ҏ��`�F�b�N.
011160     MOVE �����`�F�b�N�v             TO �����`�F�b�N.
011170     MOVE �吳�`�F�b�N�v             TO �吳�`�F�b�N.
011180     MOVE ���a�`�F�b�N�v             TO ���a�`�F�b�N.
011190     MOVE �����`�F�b�N�v             TO �����`�F�b�N.
011190     MOVE �ߘa�`�F�b�N�v             TO �ߘa�`�F�b�N.
011200     MOVE ���ҔN�v                   TO ���ҔN.
011210     MOVE ���Ҍ��v                   TO ���Ҍ�.
011220     MOVE ���ғ��v                   TO ���ғ�.
011230*     MOVE ������ҔN��v             TO ���ҔN��.
011240*     MOVE �{�l�`�F�b�N�v             TO �{�l�`�F�b�N.
011250*     MOVE �Ƒ��`�F�b�N�v             TO �Ƒ��`�F�b�N.
011260     MOVE ��������v                 TO ����.
010470     MOVE "��"                       TO ���җX�֋L��.
011270     MOVE ���җX�֔ԍ��P�v           TO ���җX�֔ԍ��P.
011280     MOVE ���җX�֔ԍ��Q�v           TO ���җX�֔ԍ��Q.
011290     MOVE "-"                        TO ���җX�֋��.
011300*     MOVE ���ҏZ���v                 TO ���ҏZ��.
011310     MOVE ���ғd�b�ԍ��v             TO ���ғd�b�ԍ�.
011320     MOVE ���ҏZ���P�v               TO ���ҏZ���P.
011330     MOVE ���ҏZ���Q�v               TO ���ҏZ���Q.
011340********************
011350* ���Ə����Z�b�g *
011360********************
011370     MOVE ���Ə����̂v               TO ���Ə�����.
011380*     MOVE ���Ə��X�֔ԍ��P�v         TO ���Ə��X�֔ԍ��P.
011390*     MOVE ���Ə��X�֔ԍ��Q�v         TO ���Ə��X�֔ԍ��Q.
011400*     IF ���Ə��X�֔ԍ��P�v NOT = SPACE
011410*         MOVE "-"                    TO ���Ə��X�֋��
011420*     END-IF.
011430*     MOVE ������Ə��Z���v           TO ���Ə��Z��.
011440     MOVE ������Ə��Z���P�v         TO ���Ə��Z���P.
011450     MOVE ������Ə��Z���Q�v         TO ���Ə��Z���Q.
011460************************************
011470* �s���{���E���N�ی��g�����Z�b�g *
011480************************************
011490*�J�ЁA����̎��͕ی��ҏ��͈󎚂��Ȃ�
011500     IF �ی���ʂv�q = 70 OR 90
011510         MOVE SPACE                  TO �ی��Ҕԍ�
011520*         MOVE SPACE                  TO �����於�x����
011530         MOVE SPACE                  TO �����於�x�����P
011540*         MOVE SPACE                  TO �����於�x�����Q
011550*         MOVE SPACE                  TO �ی��җX�֔ԍ��P
011560*         MOVE SPACE                  TO �ی��җX�֔ԍ��Q
011570*         MOVE SPACE                  TO �ی��җX�֋��
011580         MOVE SPACE                  TO �ی��ҏZ���P
011590         MOVE SPACE                  TO �ی��ҏZ���Q
011600     ELSE
011610         MOVE ����ی��Ҕԍ��v       TO �ی��Ҕԍ�
011620*         MOVE ��������於�x�����v   TO �����於�x����
011630         MOVE ��������於�x�����P�v TO �����於�x�����P
011640*         MOVE ��������於�x�����Q�v TO �����於�x�����Q
011650*         MOVE ������X�֔ԍ��P�v     TO �ی��җX�֔ԍ��P
011660*         MOVE ������X�֔ԍ��Q�v     TO �ی��җX�֔ԍ��Q
011670*         MOVE "-"                    TO �ی��җX�֋��
011680         MOVE ������Z���P�v         TO �ی��ҏZ���P
011690         MOVE ������Z���Q�v         TO �ی��ҏZ���Q
011700*         MOVE ������d�b�ԍ��v       TO �ی��ғd�b�ԍ�
011710     END-IF.
011720*================================================================*
011730 ��������Z�b�g SECTION.
011740*
011750********************
011760* �����f�[�^�Z�b�g *
011770********************
011780* �P���� *
011790**********
011800*     MOVE ��ʃR�[�h�v(1)   TO �����R�[�h�P.
011810     MOVE �������v(1)         TO �������P.
011820     MOVE �����N�v(1)         TO �����N�P.
011830     MOVE �������v(1)         TO �������P.
011840     MOVE �������v(1)         TO �������P.
011850     MOVE �{�p�����N�v(1)     TO �{�p�����N�P.
011860     MOVE �{�p�������v(1)     TO �{�p�������P.
011870     MOVE �{�p�������v(1)     TO �{�p�������P.
           IF �{�p�����N�v(1) NOT = ZERO
              MOVE "�N"             TO �N�P�Q
              MOVE "��"             TO ���P�Q
              MOVE "��"             TO ���P�Q
           END-IF
011850     MOVE �{�p�J�n�N�v(1)     TO �{�p�J�n�N�P.
011860     MOVE �{�p�J�n���v(1)     TO �{�p�J�n���P.
011870     MOVE �{�p�J�n���v(1)     TO �{�p�J�n���P.
           IF �{�p�J�n�N�v(1) NOT = ZERO
              MOVE "�N"             TO �N�P�R
              MOVE "��"             TO ���P�R
              MOVE "��"             TO ���P�R
           END-IF
011880     MOVE �{�p�I���N�v(1)     TO �{�p�I���N�P.
011890     MOVE �{�p�I�����v(1)     TO �{�p�I�����P.
011900     MOVE �{�p�I�����v(1)     TO �{�p�I�����P.
           IF �{�p�I���N�v(1) NOT = ZERO
              MOVE "�N"             TO �N�P�S
              MOVE "��"             TO ���P�S
              MOVE "��"             TO ���P�S
           END-IF
011950     MOVE �����`�F�b�N�v(1)   TO �����`�F�b�N�P.
011960     MOVE ���~�`�F�b�N�v(1)   TO ���~�`�F�b�N�P.
011980     MOVE �]��`�F�b�N�v(1)   TO �]��`�F�b�N�P.
           MOVE �����v(1)           TO �����P.
           MOVE �񐔂v(1)           TO �񐔂P.
012050*     **********************
012060*     * �]�A���͈̂ꎞ�ۗ� *
012070*     **********************
012080**********
012090* �Q���� *
012100**********
012110*     MOVE ��ʃR�[�h�v(2)   TO �����R�[�h�Q.
012120     MOVE �������v(2)         TO �������Q.
012130     MOVE �����N�v(2)         TO �����N�Q.
012140     MOVE �������v(2)         TO �������Q.
012150     MOVE �������v(2)         TO �������Q.
011850     MOVE �{�p�����N�v(2)     TO �{�p�����N�Q.
011860     MOVE �{�p�������v(2)     TO �{�p�������Q.
011870     MOVE �{�p�������v(2)     TO �{�p�������Q.
           IF �{�p�����N�v(2) NOT = ZERO
              MOVE "�N"             TO �N�Q�Q
              MOVE "��"             TO ���Q�Q
              MOVE "��"             TO ���Q�Q
           END-IF
012160     MOVE �{�p�J�n�N�v(2)     TO �{�p�J�n�N�Q.
012170     MOVE �{�p�J�n���v(2)     TO �{�p�J�n���Q.
012180     MOVE �{�p�J�n���v(2)     TO �{�p�J�n���Q.
           IF �{�p�J�n�N�v(2) NOT = ZERO
              MOVE "�N"             TO �N�Q�R
              MOVE "��"             TO ���Q�R
              MOVE "��"             TO ���Q�R
           END-IF
012190     MOVE �{�p�I���N�v(2)     TO �{�p�I���N�Q.
012200     MOVE �{�p�I�����v(2)     TO �{�p�I�����Q.
012210     MOVE �{�p�I�����v(2)     TO �{�p�I�����Q.
           IF �{�p�I���N�v(2) NOT = ZERO
              MOVE "�N"             TO �N�Q�S
              MOVE "��"             TO ���Q�S
              MOVE "��"             TO ���Q�S
           END-IF
012260     MOVE �����`�F�b�N�v(2)   TO �����`�F�b�N�Q.
012270     MOVE ���~�`�F�b�N�v(2)   TO ���~�`�F�b�N�Q.
012290     MOVE �]��`�F�b�N�v(2)   TO �]��`�F�b�N�Q.
           MOVE �����v(2)           TO �����Q.
           MOVE �񐔂v(2)           TO �񐔂Q.
012360*     **********************
012370*     * �]�A���͈̂ꎞ�ۗ� *
012380*     **********************
012390**********
012400* �R���� *
012410**********
012420*     MOVE ��ʃR�[�h�v(3)   TO �����R�[�h�R.
012430     MOVE �������v(3)         TO �������R.
012440     MOVE �����N�v(3)         TO �����N�R.
012450     MOVE �������v(3)         TO �������R.
012460     MOVE �������v(3)         TO �������R.
011850     MOVE �{�p�����N�v(3)     TO �{�p�����N�R.
011860     MOVE �{�p�������v(3)     TO �{�p�������R.
011870     MOVE �{�p�������v(3)     TO �{�p�������R.
           IF �{�p�����N�v(3) NOT = ZERO
              MOVE "�N"             TO �N�R�Q
              MOVE "��"             TO ���R�Q
              MOVE "��"             TO ���R�Q
           END-IF
012470     MOVE �{�p�J�n�N�v(3)     TO �{�p�J�n�N�R.
012480     MOVE �{�p�J�n���v(3)     TO �{�p�J�n���R.
012490     MOVE �{�p�J�n���v(3)     TO �{�p�J�n���R.
           IF �{�p�J�n�N�v(3) NOT = ZERO
              MOVE "�N"             TO �N�R�R
              MOVE "��"             TO ���R�R
              MOVE "��"             TO ���R�R
           END-IF
012500     MOVE �{�p�I���N�v(3)     TO �{�p�I���N�R.
012510     MOVE �{�p�I�����v(3)     TO �{�p�I�����R.
012520     MOVE �{�p�I�����v(3)     TO �{�p�I�����R.
           IF �{�p�I���N�v(3) NOT = ZERO
              MOVE "�N"             TO �N�R�S
              MOVE "��"             TO ���R�S
              MOVE "��"             TO ���R�S
           END-IF
012570     MOVE �����`�F�b�N�v(3)   TO �����`�F�b�N�R.
012580     MOVE ���~�`�F�b�N�v(3)   TO ���~�`�F�b�N�R.
012600     MOVE �]��`�F�b�N�v(3)   TO �]��`�F�b�N�R.
           MOVE �����v(3)           TO �����R.
           MOVE �񐔂v(3)           TO �񐔂R.
012670*     **********************
012680*     * �]�A���͈̂ꎞ�ۗ� *
012690*     **********************
012700**********
012710* �S���� *
012720**********
012730*     MOVE ��ʃR�[�h�v(4)   TO �����R�[�h�S.
012740     MOVE �������v(4)         TO �������S.
012750     MOVE �����N�v(4)         TO �����N�S.
012760     MOVE �������v(4)         TO �������S.
012770     MOVE �������v(4)         TO �������S.
011850     MOVE �{�p�����N�v(4)     TO �{�p�����N�S.
011860     MOVE �{�p�������v(4)     TO �{�p�������S.
011870     MOVE �{�p�������v(4)     TO �{�p�������S.
           IF �{�p�����N�v(4) NOT = ZERO
              MOVE "�N"             TO �N�S�Q
              MOVE "��"             TO ���S�Q
              MOVE "��"             TO ���S�Q
           END-IF
012780     MOVE �{�p�J�n�N�v(4)     TO �{�p�J�n�N�S.
012790     MOVE �{�p�J�n���v(4)     TO �{�p�J�n���S.
012800     MOVE �{�p�J�n���v(4)     TO �{�p�J�n���S.
           IF �{�p�J�n�N�v(4) NOT = ZERO
              MOVE "�N"             TO �N�S�R
              MOVE "��"             TO ���S�R
              MOVE "��"             TO ���S�R
           END-IF
012810     MOVE �{�p�I���N�v(4)     TO �{�p�I���N�S.
012820     MOVE �{�p�I�����v(4)     TO �{�p�I�����S.
012830     MOVE �{�p�I�����v(4)     TO �{�p�I�����S.
           IF �{�p�I���N�v(4) NOT = ZERO
              MOVE "�N"             TO �N�S�S
              MOVE "��"             TO ���S�S
              MOVE "��"             TO ���S�S
           END-IF
012880     MOVE �����`�F�b�N�v(4)   TO �����`�F�b�N�S.
012890     MOVE ���~�`�F�b�N�v(4)   TO ���~�`�F�b�N�S.
012910     MOVE �]��`�F�b�N�v(4)   TO �]��`�F�b�N�S.
           MOVE �����v(4)           TO �����S.
           MOVE �񐔂v(4)           TO �񐔂S.
012980*     **********************
012990*     * �]�A���͈̂ꎞ�ۗ� *
013000*     **********************
012700**********
012710* �T���� *
012720**********
012730*     MOVE ��ʃR�[�h�v(5)   TO �����R�[�h�T.
012740     MOVE �������v(5)         TO �������T.
012750     MOVE �����N�v(5)         TO �����N�T.
012760     MOVE �������v(5)         TO �������T.
012770     MOVE �������v(5)         TO �������T.
011850     MOVE �{�p�����N�v(5)     TO �{�p�����N�T.
011860     MOVE �{�p�������v(5)     TO �{�p�������T.
011870     MOVE �{�p�������v(5)     TO �{�p�������T.
           IF �{�p�����N�v(5) NOT = ZERO
              MOVE "�N"             TO �N�T�Q
              MOVE "��"             TO ���T�Q
              MOVE "��"             TO ���T�Q
           END-IF
012780     MOVE �{�p�J�n�N�v(5)     TO �{�p�J�n�N�T.
012790     MOVE �{�p�J�n���v(5)     TO �{�p�J�n���T.
012800     MOVE �{�p�J�n���v(5)     TO �{�p�J�n���T.
           IF �{�p�J�n�N�v(5) NOT = ZERO
              MOVE "�N"             TO �N�T�R
              MOVE "��"             TO ���T�R
              MOVE "��"             TO ���T�R
           END-IF
012810     MOVE �{�p�I���N�v(5)     TO �{�p�I���N�T.
012820     MOVE �{�p�I�����v(5)     TO �{�p�I�����T.
012830     MOVE �{�p�I�����v(5)     TO �{�p�I�����T.
           IF �{�p�I���N�v(5) NOT = ZERO
              MOVE "�N"             TO �N�T�S
              MOVE "��"             TO ���T�S
              MOVE "��"             TO ���T�S
           END-IF
012880     MOVE �����`�F�b�N�v(5)   TO �����`�F�b�N�T.
012890     MOVE ���~�`�F�b�N�v(5)   TO ���~�`�F�b�N�T.
012910     MOVE �]��`�F�b�N�v(5)   TO �]��`�F�b�N�T.
           MOVE �����v(5)           TO �����T.
           MOVE �񐔂v(5)           TO �񐔂T.
012980*     **********************
012990*     * �]�A���͈̂ꎞ�ۗ� *
013000*     **********************
013320************
013330* �������� *
013340************
013350     MOVE ���������v(1)       TO ���������P.
013360     MOVE ���������v(2)       TO ���������Q.
013370     MOVE ���������v(3)       TO ���������R.
013380     MOVE ���������v(4)       TO ���������S.
013390     MOVE ���������v(5)       TO ���������T.
013400     MOVE ���������v(6)       TO ���������U.
013410     MOVE ���������v(7)       TO ���������V.
013420     MOVE ���������v(8)       TO ���������W.
013430     MOVE ���������v(9)       TO ���������X.
013440     MOVE ���������v(10)      TO ���������P�O.
013350     MOVE ���������v(11)      TO ���������P�P.
013360     MOVE ���������v(12)      TO ���������P�Q.
013370     MOVE ���������v(13)      TO ���������P�R.
013380     MOVE ���������v(14)      TO ���������P�S.
013390     MOVE ���������v(15)      TO ���������P�T.
013450*
014190*================================================================*
014200 ��f�ҏ��擾 SECTION.
014210*
014220**************************************************
014230* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
014240* �� ���Ҕԍ�.... ���Ҕԍ��v                     *
014250* �� �}��........ �}�Ԃv                         *
014260* �� �L�� ....... �L���v�Ɋi�[                   *
014270* �� �ԍ� ....... �ԍ��v�Ɋi�[                   *
014280* �� ��ی��҃J�i.��ی��҃J�i�v�Ɋi�[           *
014290* �� ��ی��Ҏ���.��ی��Ҏ����v�Ɋi�[           *
014300* �� �X�֔ԍ��P ..�X�֔ԍ��P�v�Ɋi�[             *
014310* �� �X�֔ԍ��Q ..�X�֔ԍ��Q�v�Ɋi�[             *
014320* �� �Z���P ......�Z���P�v�Ɋi�[                 *
014330* �� �Z���Q ......�Z���Q�v�Ɋi�[                 *
014340* �� �d�b�ԍ�.....�d�b�ԍ��v�Ɋi�[               *
014350* �� ���҃J�i ....���҃J�i�v�Ɋi�[               *
014360* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
014370* �� ���� ........���̃}�X�^��葱���v�Ɏ擾     *
014380* �� ���Ҙa�� ....�a��ɂ��`�F�b�N��"��"���i�[ *
014390* �� ���ҔN ......���ҔN�v�Ɋi�[                 *
014400* �� ���Ҍ� ......���Ҍ��v�Ɋi�[                 *
014410* �� ���ғ� ......���ғ��v�Ɋi�[                 *
014420**************************************************
014430     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
014440     MOVE �{�p�N�v�q         TO ��|�{�p�N.
014450     MOVE �{�p���v�q         TO ��|�{�p��.
014460     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
014470     READ ��f�ҏ��e
014480     INVALID KEY
014490         CONTINUE
014500*            /* ���肦�Ȃ� */
014510     NOT INVALID KEY
014520         MOVE ��|���Ҕԍ�     TO ���Ҕԍ��v
014530         MOVE ��|�}��         TO �}�Ԃv
014540*
039080         MOVE 02            TO ���|�敪�R�[�h
039090         MOVE ��|�ی����  TO ���|���̃R�[�h
039100         READ ���̃}�X�^
039110         NOT INVALID KEY
039120             MOVE ���|����  TO �ی���ʖ��̂v
039130         END-READ
014771*
014792*-----------------------------------------------------------------*
014793         MOVE SPACE TO �A�Í������|�Í����
014794*
014795*        / �A�Í������|���͏��Z�b�g /
014796         MOVE ��|�L��       TO �A�Í������|�L��
014797         MOVE ��|�ԍ�       TO �A�Í������|�ԍ�
014798         MOVE ��|�Í������� TO �A�Í������|�Í�������
014799*
014800         CALL   �����v���O�������v
014801         CANCEL �����v���O�������v
014802*
014803         MOVE �A�Í������|���������L�� TO �L���v
014804         MOVE �A�Í������|���������ԍ� TO �ԍ��v
014807*-----------------------------------------------------------------*
014808*
014814         MOVE ��|��ی��҃J�i TO ��ی��҃J�i�v
014815         MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ����v
014820*
014830         EVALUATE ��|��ی��Ґ���
014840         WHEN 1
014850             MOVE NC"��"  TO ��ی��Ғj�`�F�b�N�v
014860         WHEN 2
014870             MOVE NC"��"  TO ��ی��ҏ��`�F�b�N�v
014880         END-EVALUATE
014890*
014900         EVALUATE ��|��ی��Ҙa��
014910         WHEN 1
014920             MOVE NC"��"  TO ��ی��Җ����`�F�b�N�v
014930         WHEN 2
014940             MOVE NC"��"  TO ��ی��ґ吳�`�F�b�N�v
014950         WHEN 3
014960             MOVE NC"��"  TO ��ی��ҏ��a�`�F�b�N�v
014970         WHEN 4
014980             MOVE NC"��"  TO ��ی��ҕ����`�F�b�N�v
014970         WHEN 5
014980             MOVE NC"��"  TO ��ی��җߘa�`�F�b�N�v
014990         END-EVALUATE
015000         MOVE ��|��ی��ҔN   TO ��ی��ҔN�v
015010         MOVE ��|��ی��Ҍ�   TO ��ی��Ҍ��v
015020         MOVE ��|��ی��ғ�   TO ��ی��ғ��v
015030
015040*��ی��ҔN��
015050         MOVE �A��|�{�p�a�� TO ���|�����敪
015060         READ �����}�X�^
015070         NOT INVALID KEY
015080             COMPUTE �{�p����N�v = ���|�J�n����N + ( �A��|�{�p�N - 1 )
015090         END-READ
015100         IF ( ��|��ی��Ҙa�� NOT = ZERO ) AND
015110            ( ��|��ی��ҔN   NOT = ZERO ) AND
015120            ( ��|��ی��Ҍ�   NOT = ZERO ) AND
015130            ( ��|��ی��ғ�   NOT = ZERO )
015140
015150             MOVE ZERO TO ��ی��ҔN��v
015160*
015170             MOVE ��|��ی��Ҙa�� TO ���|�����敪
015180             READ �����}�X�^
015190             NOT INVALID KEY
015200                 COMPUTE ��ی��Ґ���N�v = ���|�J�n����N + ( ��|��ی��ҔN - 1 )
015210             END-READ
015220*
015230             COMPUTE ��ی��ҔN��v   = �{�p����N�v - ��ی��Ґ���N�v
015240*
015250             IF �A��|�{�p�� < ��|��ی��Ҍ�
015260                 COMPUTE ��ی��ҔN��v = ��ی��ҔN��v - 1
015270             END-IF
                   MOVE ��ی��ҔN��v TO �N��v
                   STRING "("              DELIMITED BY SIZE
                          �N��v           DELIMITED BY SIZE
                          "��)"            DELIMITED BY SIZE
                     INTO �����ی��ҔN��v
                   END-STRING
015280*
015290         END-IF
015300*
015310*/�X�֔ԍ� �[���̎��̓X�y�[�X
015320         IF ��|�X�֔ԍ��P = "000"
015330             MOVE SPACE          TO ��ی��җX�֔ԍ��P�v
015340             MOVE SPACE          TO ��ی��җX�֔ԍ��Q�v
015350         ELSE
015360             MOVE ��|�X�֔ԍ��P TO ��ی��җX�֔ԍ��P�v
015370             MOVE ��|�X�֔ԍ��Q TO ��ی��җX�֔ԍ��Q�v
015380         END-IF
015390         MOVE ��|�d�b�ԍ�     TO ��ی��ғd�b�ԍ��v
015400         MOVE ��|�Z���P       TO �����ی��ҏZ���P�v
015410         MOVE ��|�Z���Q       TO �����ی��ҏZ���Q�v
015420*         STRING ��|�Z���P  DELIMITED BY SPACE
015430*                ��|�Z���Q  DELIMITED BY SPACE
015440*           INTO ��ی��ҏZ���v
015450*         END-STRING
               IF ��|�ی���� = 70 OR 80 OR 85 OR 90
018370             MOVE ��|���҃J�i       TO ��ی��҃J�i�v
018380             MOVE ��|���Ҏ���       TO ��ی��Ҏ����v
018000             EVALUATE ��|���Ґ���
018010             WHEN 1
018020                 MOVE NC"��"  TO ��ی��Ғj�`�F�b�N�v
018030             WHEN 2
018040                 MOVE NC"��"  TO ��ی��ҏ��`�F�b�N�v
018050             END-EVALUATE
018070             EVALUATE ��|���Ҙa��
018080             WHEN 1
018090                 MOVE NC"��"  TO ��ی��Җ����`�F�b�N�v
018100             WHEN 2
018110                 MOVE NC"��"  TO ��ی��ґ吳�`�F�b�N�v
018120             WHEN 3
018130                 MOVE NC"��"  TO ��ی��ҏ��a�`�F�b�N�v
      */��ی��ҕ�����ǉ�/080606
                   WHEN 4
                       MOVE NC"��"  TO ��ی��ҕ����`�F�b�N�v
                   WHEN 5
                       MOVE NC"��"  TO ��ی��җߘa�`�F�b�N�v
018140             END-EVALUATE
018150             MOVE ��|���ҔN   TO ��ی��ҔN�v
018160             MOVE ��|���Ҍ�   TO ��ی��Ҍ��v
018170             MOVE ��|���ғ�   TO ��ی��ғ��v
019210             MOVE ��|���җX�֔ԍ��P TO ��ی��җX�֔ԍ��P�v
019220             MOVE ��|���җX�֔ԍ��Q TO ��ی��җX�֔ԍ��Q�v
016980             MOVE ��|���ҏZ���P     TO �����ی��ҏZ���P�v
016990             MOVE ��|���ҏZ���Q     TO �����ی��ҏZ���Q�v
018220             MOVE ��|���ғd�b�ԍ�   TO ��ی��ғd�b�ԍ��v
016040             IF ( ��|���Ҙa�� NOT = ZERO ) AND
016050                ( ��|���ҔN   NOT = ZERO ) AND
016060                ( ��|���Ҍ�   NOT = ZERO ) AND
016070                ( ��|���ғ�   NOT = ZERO )
016080
016090                 MOVE ZERO TO ��ی��ҔN��v
016100*
016110                 MOVE ��|���Ҙa�� TO ���|�����敪
016120                 READ �����}�X�^
016130                 NOT INVALID KEY
016140                     COMPUTE ��ی��Ґ���N�v = ���|�J�n����N + ( ��|���ҔN - 1 )
016150                 END-READ
016160*
016170                 COMPUTE ��ی��ҔN��v   = �{�p����N�v - ��ی��Ґ���N�v
016180*
016190                 IF �A��|�{�p�� < ��|���Ҍ�
016200                     COMPUTE ��ی��ҔN��v = ��ی��ҔN��v - 1
016210                 END-IF
016220*
                       MOVE ��ی��ҔN��v TO �N��v
                       STRING "("              DELIMITED BY SIZE
                              �N��v           DELIMITED BY SIZE
                              "��)"            DELIMITED BY SIZE
                         INTO �����ی��ҔN��v
                       END-STRING
016240             END-IF
               END-IF
015460*
015470*/�X�֔ԍ� �[���̎��̓X�y�[�X
015480         IF ��|���җX�֔ԍ��P = "000"
015490             MOVE SPACE              TO ���җX�֔ԍ��P�v
015500             MOVE SPACE              TO ���җX�֔ԍ��Q�v
015510         ELSE
015520             MOVE ��|���җX�֔ԍ��P TO ���җX�֔ԍ��P�v
015530             MOVE ��|���җX�֔ԍ��Q TO ���җX�֔ԍ��Q�v
015540         END-IF
015550         MOVE ��|���ҏZ���P     TO ���ҏZ���P�v
015560         MOVE ��|���ҏZ���Q     TO ���ҏZ���Q�v
015570*         STRING ��|���ҏZ���P  DELIMITED BY SPACE
015580*                ��|���ҏZ���Q  DELIMITED BY SPACE
015590*           INTO ���ҏZ���v
015600*         END-STRING
015610         MOVE ��|���ғd�b�ԍ�   TO ���ғd�b�ԍ��v
015620*
015630*�d�b�ԍ�����
015640*         UNSTRING �d�b�ԍ��v  DELIMITED BY "-"
015650*             INTO �����d�b�ԍ��P�v
015660*                  �����d�b�ԍ��Q�v
015670*                  �����d�b�ԍ��R�v
015680*         END-UNSTRING
015690*�d�b�ԍ��E�l����
015700*         IF �����d�b�ԍ��R�v = SPACE
015710*             MOVE �����d�b�ԍ��Q�v TO �����d�b�ԍ��R�v
015720*             MOVE �����d�b�ԍ��P�v TO �����d�b�ԍ��Q�v
015730*             MOVE SPACE            TO �����d�b�ԍ��P�v
015740*         END-IF
015750*
015760         MOVE ��|���҃J�i     TO ���҃J�i�v
015770         MOVE ��|���Ҏ���     TO ���Ҏ����v
015780*
015790         MOVE ��|�ی����     TO ���|�ی����
015800         MOVE ��|�ی��Ҕԍ�   TO ���|�ی��Ҕԍ�
015810         MOVE �L���v           TO ���|�L��
015820*
015830         READ ���Ə��}�X�^
015840         INVALID KEY
015850            MOVE SPACE TO ���|���R�[�h
015860            INITIALIZE    ���|���R�[�h
015870         END-READ
015880         MOVE ���|���Ə�����   TO ���Ə����̂v
015890         MOVE ���|���Ə��Z���P TO ������Ə��Z���P�v
015900         MOVE ���|���Ə��Z���Q TO ������Ə��Z���Q�v
015910* ����
015920         IF ��|�{�l�Ƒ��敪 = 1
015930             MOVE NC"�{�l"     TO �����v
015940             MOVE NC"��"       TO �{�l�`�F�b�N�v
015950         ELSE
015960             MOVE 05          TO ���|�敪�R�[�h
015970             MOVE ��|����    TO ���|���̃R�[�h
015980             READ ���̃}�X�^
015990             INVALID KEY
016000                 MOVE SPACE   TO �����v
016010             NOT INVALID KEY
016020                 MOVE ���|����  TO �����v
016030             END-READ
016040             MOVE NC"��"       TO �Ƒ��`�F�b�N�v
016050         END-IF
016060* ���Ҙa��N����
016070         EVALUATE ��|���Ҙa��
016080         WHEN 1
016090             MOVE NC"��"  TO �����`�F�b�N�v
016100         WHEN 2
016110             MOVE NC"��"  TO �吳�`�F�b�N�v
016120         WHEN 3
016130             MOVE NC"��"  TO ���a�`�F�b�N�v
016140         WHEN 4
016150             MOVE NC"��"  TO �����`�F�b�N�v
016140         WHEN 5
016150             MOVE NC"��"  TO �ߘa�`�F�b�N�v
016160         END-EVALUATE
016170         MOVE ��|���ҔN  TO ���ҔN�v
016180         MOVE ��|���Ҍ�  TO ���Ҍ��v
016190         MOVE ��|���ғ�  TO ���ғ��v
016200*���ҔN��
016210         MOVE �A��|�{�p�a�� TO ���|�����敪
016220         READ �����}�X�^
016230         NOT INVALID KEY
016240             COMPUTE �{�p����N�v = ���|�J�n����N + ( �A��|�{�p�N - 1 )
016250         END-READ
016260         IF ( ��|���Ҙa�� NOT = ZERO ) AND
016270            ( ��|���ҔN   NOT = ZERO ) AND
016280            ( ��|���Ҍ�   NOT = ZERO ) AND
016290            ( ��|���ғ�   NOT = ZERO )
016300*
016310             MOVE ZERO TO ���ҔN��v
016320*
016330             MOVE ��|���Ҙa�� TO ���|�����敪
016340             READ �����}�X�^
016350             NOT INVALID KEY
016360                 COMPUTE ���Ґ���N�v = ���|�J�n����N + ( ��|���ҔN - 1 )
016370             END-READ
016380*
016390             COMPUTE ���ҔN��v   = �{�p����N�v - ���Ґ���N�v
016400*
016410             IF �A��|�{�p�� < ��|���Ҍ�
016420                 COMPUTE ���ҔN��v = ���ҔN��v - 1
016430             END-IF
                   MOVE ���ҔN��v    TO �N��v
                   STRING "("              DELIMITED BY SIZE
                          �N��v           DELIMITED BY SIZE
                          "��)"            DELIMITED BY SIZE
                     INTO ������ҔN��v
                   END-STRING
016440*
016450         END-IF
016460* ���Ґ���
016470*         EVALUATE ��|���Ґ���
016480*         WHEN 1
016490*             MOVE NC"�i�j�j" TO ���Ґ��ʂv
016500*         WHEN 2
016510*             MOVE NC"�i���j" TO ���Ґ��ʂv
016520*         END-EVALUATE
016530         EVALUATE ��|���Ґ���
016540         WHEN 1
016550             MOVE NC"��"  TO ���Ғj�`�F�b�N�v
016560         WHEN 2
016570             MOVE NC"��"  TO ���ҏ��`�F�b�N�v
016580         END-EVALUATE
016590*/�L������
016600         MOVE ��|�L���a��   TO �L�������a��v
016610         MOVE ��|�L���N     TO �L�������N�v
016620         MOVE ��|�L����     TO �L���������v
016630         MOVE ��|�L����     TO �L���������v
016760         IF (�L�������a��v NOT = ZERO) AND
016770            (�L�������N�v   NOT = ZERO) AND
016780            (�L���������v   NOT = ZERO) AND
016790            (�L���������v   NOT = ZERO)
016640             MOVE �L�������a��v TO ���|�����敪
016650             READ �����}�X�^
016660             INVALID KEY
016670                 MOVE SPACE        TO �L�����������v
016680             NOT INVALID KEY
016690                 MOVE ���|�������� TO �L�����������v
016700             END-READ
016860         END-IF
016710*/���i�擾�N����
016720         MOVE ��|���i�a�� TO ���i�擾�a��v
016730         MOVE ��|���i�N   TO ���i�擾�N�v
016740         MOVE ��|���i��   TO ���i�擾���v
016750         MOVE ��|���i��   TO ���i�擾���v
016760         IF (���i�擾�a��v NOT = ZERO) AND
016770            (���i�擾�N�v   NOT = ZERO) AND
016780            (���i�擾���v   NOT = ZERO) AND
016790            (���i�擾���v   NOT = ZERO)
016800             EVALUATE ���i�擾�a��v
016810             WHEN 3
016820                 MOVE NC"��"  TO ���i���a�`�F�b�N�v
016830             WHEN 4
016840                 MOVE NC"��"  TO ���i�����`�F�b�N�v
016850             END-EVALUATE
016640             MOVE ���i�擾�a��v TO ���|�����敪
016650             READ �����}�X�^
016660             INVALID KEY
016670                 MOVE SPACE        TO ���i�擾�����v
016680             NOT INVALID KEY
016690                 MOVE ���|�������� TO ���i�擾�����v
016700             END-READ
016860         END-IF
017130* �s�����ԍ� �󋋎Ҕԍ�
017140         IF ��|������� NOT = ZERO
017150             MOVE ��|��p���S�Ҕԍ����� TO ��p���S�Ҕԍ��v
017160             MOVE ��|��v�Ҕԍ�����     TO ��v�Ҕԍ������v
017170         END-IF
017180         IF ( ��|������ = 05 )
017190             MOVE ��|��p���S�Ҕԍ�     TO �s�����ԍ��v
017200*             MOVE ��|��v�Ҕԍ��V�l     TO ��v�Ҕԍ��v
017210         END-IF
      *�J�ЁE�����E���ۂ̎��A��ی��җ��Ɋ��ҏ�������
014790         IF ��|�ی���� = 70 OR 80 OR 85
014830             EVALUATE ��|���Ґ���
014840             WHEN 1
014850                 MOVE NC"��"   TO ��ی��Ғj�`�F�b�N�v
014860             WHEN 2
014870                 MOVE NC"��"   TO ��ی��ҏ��`�F�b�N�v
014880             END-EVALUATE
014814             MOVE ��|���҃J�i TO ��ی��҃J�i�v
014815             MOVE ��|���Ҏ��� TO ��ی��Ҏ����v
014900             EVALUATE ��|���Ҙa��
014910             WHEN 1
014920                 MOVE NC"��"   TO ��ی��Җ����`�F�b�N�v
014930             WHEN 2
014940                 MOVE NC"��"   TO ��ی��ґ吳�`�F�b�N�v
014950             WHEN 3
014960                 MOVE NC"��"   TO ��ی��ҏ��a�`�F�b�N�v
014970             WHEN 4
014980                 MOVE NC"��"   TO ��ی��ҕ����`�F�b�N�v
014990             END-EVALUATE
015000             MOVE ��|���ҔN   TO ��ی��ҔN�v
015010             MOVE ��|���Ҍ�   TO ��ی��Ҍ��v
015020             MOVE ��|���ғ�   TO ��ی��ғ��v
015320             IF ��|���җX�֔ԍ��P = "000"
015330                 MOVE SPACE              TO ��ی��җX�֔ԍ��P�v
015340                 MOVE SPACE              TO ��ی��җX�֔ԍ��Q�v
015350             ELSE
015360                 MOVE ��|���җX�֔ԍ��P TO ��ی��җX�֔ԍ��P�v
015370                 MOVE ��|���җX�֔ԍ��Q TO ��ی��җX�֔ԍ��Q�v
015380             END-IF
015390             MOVE ��|���ғd�b�ԍ�       TO ��ی��ғd�b�ԍ��v
015400             MOVE ��|���ҏZ���P         TO �����ی��ҏZ���P�v
015410             MOVE ��|���ҏZ���Q         TO �����ی��ҏZ���Q�v
               END-IF
018330*�ی����
018340         IF ��|������� > 50
018360             MOVE NC"��"  TO �����`�F�b�N�v
018540         END-IF
018550
018560         IF ��|������ = 05
018570             MOVE NC"��"  TO �㍂�`�F�b�N�v
018580         ELSE
018590             EVALUATE ��|�ی����
018600             WHEN 01
018610                 MOVE NC"��"  TO ���ۃ`�F�b�N�v
018620             WHEN 02
018630                 MOVE NC"��"  TO �Еۃ`�F�b�N�v
018640             WHEN 03
018650                 MOVE NC"��"  TO �g���`�F�b�N�v
018660             WHEN 04
018670                 MOVE NC"��"  TO ���σ`�F�b�N�v
018680             WHEN 06
018690                 MOVE NC"��"  TO ���ك`�F�b�N�v
018700             WHEN 07
018710                 MOVE NC"��"  TO �D���`�F�b�N�v
018720             WHEN 08
018730                 MOVE NC"��"  TO ���ۃ`�F�b�N�v
018740             WHEN 09
018750                 MOVE NC"��"  TO ���q���`�F�b�N�v
018780             WHEN 90
018790                 MOVE NC"��"  TO ����`�F�b�N�v
018800             END-EVALUATE
018810         END-IF
018820     END-READ.
017230*================================================================*
017240 ��������擾 SECTION.
017250*
017260****************************************************
017270* �A���f�[�^����ی��҃}�X�^��萿������擾����B *
017280* ���ہ|��������敪=1�̏ꍇ������}�X�^���g�p   *
017290* �� �ی��Ҕԍ�...�ی��Ҕԍ��v�Ɋi�[               *
017300* �� ����........ �����於�̂v�Ɋi�[               *
017310* �� �X�֔ԍ��P.. ������X�֔ԍ��P�v�Ɋi�[         *
017320* �� �X�֔ԍ��Q.. ������X�֔ԍ��Q�v�Ɋi�[         *
017330* �� �Z���P.......������Z���P�v�Ɋi�[             *
017340* �� �Z���Q.......������Z���Q�v�Ɋi�[             *
017350****************************************************
017360     MOVE SPACE          TO �ی��Ҕԍ��v.
017370     MOVE SPACE          TO �����於�̂v.
017380     MOVE SPACE          TO �x�����v.
017390     MOVE SPACE          TO ������X�֔ԍ��P�v.
017400     MOVE SPACE          TO ������X�֔ԍ��Q�v.
017410     MOVE SPACE          TO ������Z���P�v.
017420     MOVE SPACE          TO ������Z���Q�v.
017430     MOVE SPACE          TO ������d�b�ԍ��v.
017440     MOVE SPACE          TO ����ی��ҌĖ��v.
017450     MOVE SPACE          TO �����於�x�����v
017460*
017470     MOVE �ی���ʂv�q   TO �ہ|�ی����.
017480     MOVE �ی��Ҕԍ��v�q TO �ہ|�ی��Ҕԍ�.
017490     READ �ی��҃}�X�^
017500     INVALID KEY
017510         IF ( �{�p�a��N���v�q >= 42004 ) AND ( �ی���ʂv�q = 05 )
017520             MOVE �ی���ʂv�q   TO �s�|������
017530             MOVE �ی��Ҕԍ��v�q TO �s�|�s�����ԍ�
017540             READ �s�����}�X�^
017550             INVALID KEY
017560                 MOVE SPACE      TO �����於�̂v
017570                 MOVE SPACE      TO �x�����v
017580                 MOVE ZERO       TO �ڔ���敪�v
017590             NOT INVALID KEY
017600                 MOVE �s�|�s�����ԍ�  TO �ی��Ҕԍ��v
017610                 MOVE �s�|�s��������  TO �����於�̂v
017620                 MOVE �s�|�x��������  TO �x�����v
017630                 MOVE �s�|�X�֔ԍ��P  TO ������X�֔ԍ��P�v
017640                 MOVE �s�|�X�֔ԍ��Q  TO ������X�֔ԍ��Q�v
017650                 MOVE �s�|�Z���P      TO ������Z���P�v
017660                 MOVE �s�|�Z���Q      TO ������Z���Q�v
017670                 MOVE �s�|�d�b�ԍ�    TO ������d�b�ԍ��v
017680                 MOVE ZERO            TO �ڔ���敪�v
017690             END-READ
017700         ELSE
017710             MOVE SPACE      TO �����於�̂v
017720             MOVE SPACE      TO �x�����v
017730             MOVE ZERO       TO �ڔ���敪�v
017740         END-IF
017750     NOT INVALID KEY
017760         MOVE �ہ|�ی��Ҕԍ�  TO �ی��Ҕԍ��v
017770         MOVE �ہ|�ی��Җ���  TO �����於�̂v
017780         MOVE �ہ|�x��������  TO �x�����v
017790         MOVE �ہ|�X�֔ԍ��P  TO ������X�֔ԍ��P�v
017800         MOVE �ہ|�X�֔ԍ��Q  TO ������X�֔ԍ��Q�v
017810         MOVE �ہ|�Z���P      TO ������Z���P�v
017820         MOVE �ہ|�Z���Q      TO ������Z���Q�v
017830         MOVE �ہ|�d�b�ԍ�    TO ������d�b�ԍ��v
017840         MOVE �ہ|�ڔ���敪  TO �ڔ���敪�v
017850     END-READ.
           IF �ی���ʂv�q = 85
              MOVE �{�p�a��N���v�q TO ���ہ|�{�p�a��N��
              MOVE ���҃R�[�h�v�q   TO ���ہ|���҃R�[�h
              READ ���ۏ��e
              NOT INVALID KEY
019480           MOVE ���ہ|���S�Ҕԍ�    TO �ی��Ҕԍ��v
019490           MOVE ���ہ|���ێs������  TO �����於�̂v
019500           MOVE SPACE               TO �x�����v
019510           MOVE ���ہ|���ۑ��t��X�֔ԍ��P  TO ������X�֔ԍ��P�v
019520           MOVE ���ہ|���ۑ��t��X�֔ԍ��Q  TO ������X�֔ԍ��Q�v
020220           MOVE ���ہ|���ۑ��t��Z���P      TO ������Z���P�v
020230           MOVE ���ہ|���ۑ��t��Z���Q      TO ������Z���Q�v
                 MOVE SPACE              TO �L���v
                 MOVE ���ہ|���ۋL���ԍ� TO �ԍ��v
              END-READ
           END-IF.
      *
022000* �����ӏ��
022010     IF �ی���ʂv�q = 80
022020        MOVE �{�p�a��N���v�q TO �����|�{�p�a��N��
022030        MOVE ���҃R�[�h�v�q   TO �����|���҃R�[�h
022040        READ �����ӏ��e
022050        INVALID KEY
022060           MOVE SPACE TO �����|���R�[�h
022070           INITIALIZE    �����|���R�[�h
022080        END-READ
022090        MOVE �����|�ی���Дԍ�    TO �ی���|�ی���Дԍ�
022040        READ �ی���Ѓ}�X�^
022050        NOT INVALID KEY
022060           MOVE �ی���|�ی���Ж� TO �����於�̂v
019500           MOVE SPACE              TO �x�����v
                 IF �����|�ی���Б��t��Z���P = SPACE
022450               MOVE �ی���|�X�֔ԍ��P TO ������X�֔ԍ��P�v
022460               MOVE �ی���|�X�֔ԍ��Q TO ������X�֔ԍ��Q�v
022470               MOVE �ی���|�Z���P     TO ������Z���P�v
022480               MOVE �ی���|�Z���Q     TO ������Z���Q�v
                  ELSE
022450               MOVE �����|�ی���Б��t��X�֔ԍ��P TO ������X�֔ԍ��P�v
022460               MOVE �����|�ی���Б��t��X�֔ԍ��Q TO ������X�֔ԍ��Q�v
019580               MOVE �����|�ی���Б��t��Z���P     TO ������Z���P�v
019590               MOVE �����|�ی���Б��t��Z���Q     TO ������Z���Q�v
                  END-IF
022080        END-READ
022170     END-IF.
017490*
017860     EVALUATE �ی���ʂv�q
017870     WHEN 2
017880     WHEN 6
017890         IF �ڔ���敪�v = 1
017900            MOVE SPACE              TO ����ی��ҌĖ��v
017910         ELSE
017920            MOVE "�Љ�ی�������"   TO ����ی��ҌĖ��v
017930         END-IF
017940     WHEN 3
017950         MOVE "���N�ی��g��"   TO ����ی��ҌĖ��v
017960     WHEN OTHER
017970         MOVE SPACE TO ����ی��ҌĖ��v
017980     END-EVALUATE.
017990     IF ����ی��ҌĖ��v NOT = SPACE
018000         STRING �����於�̂v DELIMITED BY SPACE
018010                �ی��ҌĖ��v DELIMITED BY SPACE
018020           INTO �����於�̂v
018030         END-STRING
018040     END-IF.
018050*�g���Ƌ��ς͎x�������󎚂���
018060     EVALUATE �ی���ʂv�q
018070     WHEN 03
018080     WHEN 04
018090         STRING �����於�̂v DELIMITED BY SPACE
018100                "  "         DELIMITED BY SIZE
018110                �x�����v     DELIMITED BY SPACE
018120           INTO �����於�x�����v
018130         END-STRING
018140     WHEN OTHER
018150         MOVE �����於�̂v TO �����於�x�����v
018160     END-EVALUATE.
018170*================================================================*
018180 �����f�[�^�擾 SECTION.
018190*
018200**************************************************
018210* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾 *
018220* �� ������...���ʁ{������ʂɂĉ��H���Ċi�[     *
018230* �� �����N.......�����N�v                       *
018240* �� ������.......�������v                       *
018250* �� ������.......�������v                       *
018260* �� �{�p�I���N...�I���N�v                       *
018270* �� �{�p�I����...�I�����v                       *
018280* �� �{�p�I����...�I�����v                       *
018290**************************************************
018300     MOVE �{�p�a��v�q       TO ���|�{�p�a��.
018310     MOVE �{�p�N�v�q         TO ���|�{�p�N.
018320     MOVE �{�p���v�q         TO ���|�{�p��.
018330     MOVE ���҃R�[�h�v�q     TO ���|���҃R�[�h.
018340     READ �����f�[�^�e
018350     INVALID KEY
018360         CONTINUE
018370*            /* ���肦�Ȃ� */
018380     NOT INVALID KEY
018390         MOVE ZERO                         TO ���ʐ��v
018400         MOVE ���|���ʐ�                   TO ���ʐ��v
018410         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018420                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
018430*********************************************
018440* ���j�S�_...������ʁ{���ʂɂĉ��H���Ċi�[ *
018450*********************************************
018460* �������
018470             MOVE SPACE                     TO �������̂v
018480             MOVE 03                        TO ���|�敪�R�[�h
018490             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
018500             READ ���̃}�X�^
018510             INVALID KEY
018520                 MOVE SPACE    TO �������̂v
018530             NOT INVALID KEY
018540*                 MOVE ���|���� TO �������̂v
018550                 MOVE ���|�������� TO �������̂v
018560             END-READ
018570* ����
018580             MOVE SPACE                    TO �������v(���ʂb�m�s)
018590             PERFORM ���̖�������
018600             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
018610             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
018620             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
018630*             IF ���|�����N(���ʂb�m�s) NOT = ZERO 
018640*                MOVE "."                   TO �����N������؂v(���ʂb�m�s)
018650*             END-IF
018660*
018670             MOVE ���|�J�n�N(���ʂb�m�s)   TO �{�p�����N�v(���ʂb�m�s)
018680             MOVE ���|�J�n��(���ʂb�m�s)   TO �{�p�������v(���ʂb�m�s)
018690             MOVE ���|�J�n��(���ʂb�m�s)   TO �{�p�������v(���ʂb�m�s)
018700*             IF ���|�����N(���ʂb�m�s) NOT = ZERO 
018710*                MOVE "."                   TO �{���N������؂v(���ʂb�m�s)
018720*             END-IF
018660*
      */�J�n�N���͓����̍ŏ��̎{�p��
018670             MOVE ���|�J�n�N(���ʂb�m�s)   TO �{�p�J�n�N�v(���ʂb�m�s)
018680             MOVE ���|�J�n��(���ʂb�m�s)   TO �{�p�J�n���v(���ʂb�m�s)
018690             MOVE ���|�J�n��(���ʂb�m�s)   TO �{�p�J�n���v(���ʂb�m�s)
018700*             IF ���|�J�n�N(���ʂb�m�s) NOT = ZERO 
018710*                MOVE "."                   TO �{�J�N������؂v(���ʂb�m�s)
018720*             END-IF
                   PERFORM �{�p�L�^�擾
018730*
018740             MOVE ���|�I���N(���ʂb�m�s)   TO �{�p�I���N�v(���ʂb�m�s)
018750             MOVE ���|�I����(���ʂb�m�s)   TO �{�p�I�����v(���ʂb�m�s)
018760             MOVE ���|�I����(���ʂb�m�s)   TO �{�p�I�����v(���ʂb�m�s)
018770*             IF ���|�I���N(���ʂb�m�s) NOT = ZERO 
018780*                MOVE "."                   TO �{�I�N������؂v(���ʂb�m�s)
018790*             END-IF
018800*�]�A
018810             EVALUATE ���|�]�A�敪(���ʂb�m�s)
018820             WHEN 1
018830             WHEN 2
018840                 MOVE NC"��"               TO �����`�F�b�N�v(���ʂb�m�s)
018850             WHEN 3
018860                 MOVE NC"��"               TO ���~�`�F�b�N�v(���ʂb�m�s)
018870             WHEN 4
018880                 MOVE NC"��"               TO �]��`�F�b�N�v(���ʂb�m�s)
018890             WHEN 9
018900                 MOVE NC"��"               TO �p���`�F�b�N�v(���ʂb�m�s)
018910             END-EVALUATE
018920*             IF ���|�]�A�敪(���ʂb�m�s) NOT = 9
018930*                 MOVE 04                        TO ���|�敪�R�[�h
018940*                 MOVE ���|�]�A�敪(���ʂb�m�s)  TO ���|���̃R�[�h
018950*                 READ ���̃}�X�^
018960*                 INVALID KEY
018970*                     MOVE SPACE    TO �]�A�v(���ʂb�m�s)
018980*                 NOT INVALID KEY
018990*                     MOVE ���|���� TO �]�A�v(���ʂb�m�s)
019000*                 END-READ
019010*             END-IF
019020             PERFORM �����f�[�^�ޔ�
019030         END-PERFORM
019040     END-READ.
019050*================================================================*
019060 �{�p�L�^�擾 SECTION.
019070*
019080************************************************************
019090* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾           *
019100* �� �{�p�J�n�N����...�Y�����镔�ʂɑ΂��ē����ŏ��̎{�p�� *
019110************************************************************
019120     MOVE �{�p�a��v�q  TO �{�L�|�{�p�a��.
019130     MOVE �{�p�N�v�q    TO �{�L�|�{�p�N.
019140     MOVE �{�p���v�q    TO �{�L�|�{�p��.
019150     MOVE ZERO            TO �{�L�|�{�p��.
019160     MOVE ZERO            TO �{�L�|���Ҕԍ�.
019170     MOVE SPACE           TO �{�L�|�}��.
019180     START �{�p�L�^�e   KEY IS >= �{�L�|�{�p�a��N����
019190                                  �{�L�|���҃R�[�h.
019200     IF ��ԃL�[ = "00"
019210         MOVE SPACE  TO �I���t���O�Q
019220         PERFORM �{�p�L�^�e�Ǎ�
019230         PERFORM UNTIL ( �I���t���O�Q       = "YES" ) OR
019240                       ( �{�L�|�{�p�a�� NOT = �{�p�a��v�q ) OR
019250                       ( �{�L�|�{�p�N   NOT = �{�p�N�v�q   ) OR
019260                       ( �{�L�|�{�p��   NOT = �{�p���v�q   ) OR
                             ( �J�n�N�����擾�t���O(���ʂb�m�s) NOT = SPACE )
019270*            **************
019280*            * �J�n�N���� *
019290*            **************
019300*             PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
019310*                     UNTIL ( ���ʂb�m�s > ���ʐ��v )
031200             IF ( �{�p�N�v�q NOT = �{�p�����N�v(���ʂb�m�s) ) OR
031210                ( �{�p���v�q NOT = �{�p�������v(���ʂb�m�s) ) OR
031220                ( ���|�J�n�f�Ó��蓮�敪 = 1 )
019390                   MOVE �{�L�|�{�p�N     TO �{�p�J�n�N�v(���ʂb�m�s)
019400                   MOVE �{�L�|�{�p��     TO �{�p�J�n���v(���ʂb�m�s)
019410                   MOVE �{�L�|�{�p��     TO �{�p�J�n���v(���ʂb�m�s)
019420                   MOVE "YES"       TO �J�n�N�����擾�t���O(���ʂb�m�s)
019430*
019460             END-IF
019470*             END-PERFORM
019480             PERFORM �{�p�L�^�e�Ǎ�
019490         END-PERFORM
019500     END-IF.
019510*================================================================*
019520 �{�p�L�^�e�Ǎ� SECTION.
019530*
019540     READ �{�p�L�^�e NEXT
019550     AT END
019560         MOVE "YES" TO �I���t���O�Q
019570     END-READ.
019580*================================================================*
019590 ���������擾 SECTION.
019600*
019610********************************************************************
019620*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
019630*  ��: �@�A �Ƃœ]��.
019640*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
019650*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
019660********************************************************************
019670     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
019680     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
019700             UNTIL ( ���ʂb�m�s > 7 )
019720*
019740        IF ( ���������A�Ԃv(���ʂb�m�s)      NOT = ZERO )
019750*
019760           IF �J�E���^ = ZERO
019770               MOVE 1   TO  �J�E���^ �J�E���^�Q
019780               MOVE �����������Ҕԍ��v(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
019790               MOVE ���������A�Ԃv(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
019800               MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
019810           ELSE
019820              IF ( �����������Ҕԍ��v(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
019830                 ( ���������A�Ԃv(���ʂb�m�s)      = �����A�Ԃb�v     )
019840                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
019850                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
019860              ELSE
019870                 COMPUTE �J�E���^ = �J�E���^  +  1
019880                 MOVE 1   TO  �J�E���^�Q
019890                 MOVE �����������Ҕԍ��v(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
019900                 MOVE ���������A�Ԃv(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
019910                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
019920              END-IF
019930           END-IF
019940        END-IF
019950     END-PERFORM.
019960**************************************************************************
019970*  ���������}�X�^��蕶�͎擾
019980**************************************************************************
019990     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
020000     PERFORM VARYING �J�E���^ FROM 1 BY 1
020010             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
020020** ���ۂ� �敪 01
020030*         MOVE 01                        TO �����|�敪�R�[�h
020040*/���ہF�O�P�A�J�ЁF�O�Q�A�����ӁF�O�R
020050         EVALUATE ��|�ی����
020060         WHEN 70
020070             MOVE 2 TO �����|�敪�R�[�h
020080         WHEN 80
020090             MOVE 3 TO �����|�敪�R�[�h
020100         WHEN OTHER
020110             MOVE 1 TO �����|�敪�R�[�h
020120         END-EVALUATE
020130         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
020140         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
020150         READ ���������e
020160         NOT INVALID KEY
020170             INITIALIZE ���������v�s
020180             MOVE �����|���������b�l(1) TO  ���������P�v�s
020190             MOVE �����|���������b�l(2) TO  ���������Q�v�s
020200             MOVE �����|���������b�l(3) TO  ���������R�v�s
020210             MOVE �����|���������b�l(4) TO  ���������S�v�s
020220             MOVE �����|���������b�l(5) TO  ���������T�v�s
020230             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
020240                     UNTIL ( �J�E���^�Q > 9 )  OR 
020250                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
020260                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
020270                WHEN 1
020280                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020290                WHEN 2
020300                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020310                WHEN 3
020320                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020330                WHEN 4
020340                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020350                WHEN 5
020360                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020370                WHEN OTHER
020380                   CONTINUE
020390                END-EVALUATE
020400             END-PERFORM
020410*
020420             IF �����|�����������͋敪 = 1
020430                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
020440                        ���������P�v�s  DELIMITED BY SIZE
020450                        ���������Q�v�s  DELIMITED BY SIZE
020460                        ���������R�v�s  DELIMITED BY SIZE
020470                        ���������S�v�s  DELIMITED BY SIZE
020480                        ���������T�v�s  DELIMITED BY SIZE
020490                        INTO �����������e�����v(�J�E���^)
020500                 END-STRING
020510             ELSE
020520                 INSPECT ���������v�s REPLACING ALL �S�p�� BY ���p��
020530                 MOVE SPACE TO �����P�v �����Q�v
020540                 MOVE ���������i���o�[�m�v TO �����P�v
020550                 MOVE ���������P�v�s       TO �����Q�v
020560                 CALL �v���O�������v WITH C LINKAGE
020570                      USING BY REFERENCE �����P�v
020580                            BY REFERENCE �����Q�v
020590                 MOVE ���������Q�v�s       TO �����Q�v
020600                 CALL �v���O�������v WITH C LINKAGE
020610                      USING BY REFERENCE �����P�v
020620                            BY REFERENCE �����Q�v
020630                 MOVE ���������R�v�s       TO �����Q�v
020640                 CALL �v���O�������v WITH C LINKAGE
020650                      USING BY REFERENCE �����P�v
020660                            BY REFERENCE �����Q�v
020670                 MOVE ���������S�v�s       TO �����Q�v
020680                 CALL �v���O�������v WITH C LINKAGE
020690                      USING BY REFERENCE �����P�v
020700                            BY REFERENCE �����Q�v
020710                 MOVE ���������T�v�s       TO �����Q�v
020720                 CALL �v���O�������v WITH C LINKAGE
020730                      USING BY REFERENCE �����P�v
020740                            BY REFERENCE �����Q�v
020750                  MOVE �����P�v TO �����������e�����v(�J�E���^)
020760             END-IF
020770*
020780         END-READ
020790     END-PERFORM.
020800*
020810     PERFORM ���������Z�b�g.
020820*
020830*================================================================*
020840 �ڍ׃��[�h������������ SECTION.
020850*
020860*******************************************************************
020870* �A��|��������s�ɂ��A���������f�[�^�̕��ёւ����s���B
020880* ( �����������v�����������A�ޔ��ς݂̕����������r�v����
020890*         ���ёւ����s���A�����������v���Đݒ肷��B)
020900*******************************************************************
020910*
020920     INITIALIZE �����������v.
020930*
020940     PERFORM VARYING �ڍׂb�m�s FROM 1 BY 1
020950                                UNTIL ( �ڍׂb�m�s > 5)
020960*
020970        IF ( �A��|��������s(�ڍׂb�m�s) NOT = ZERO) AND
020980           ( �A��|��������敪(�ڍׂb�m�s)   = 1)
020990*
021000           MOVE �A��|��������s(�ڍׂb�m�s) TO ����s�r�v
021010*
021020           MOVE �������Ҕԍ��r�v(�ڍׂb�m�s) TO �����������Ҕԍ��v(����s�r�v)
021030           MOVE �����A�Ԃr�v(�ڍׂb�m�s) TO ���������A�Ԃv(����s�r�v)
021040        END-IF
021050*
021060     END-PERFORM.
021070*
021080*================================================================*
021090 �����s�Z�b�g SECTION.
021100*/0610
021110* �w�肳�ꂽ�J�n�s����������悤�ɃZ�b�g�������B
021120* �P�O�s.
021130*
021140     MOVE SPACE                  TO ���������v�e�[�u���Q.
021150     MOVE ZERO                   TO �J�E���^.
021160     IF �A��|�����J�n�s = ZERO
021170         MOVE 1                  TO �J�E���^�Q
021180     ELSE
021190         MOVE �A��|�����J�n�s   TO �J�E���^�Q
021200     END-IF.
021210     PERFORM VARYING �J�E���^ FROM 1 BY 1
021220             UNTIL ( �J�E���^ > 10 ) OR (�J�E���^�Q > 10)
021230         MOVE ���������v(�J�E���^) TO ���������v�Q(�J�E���^�Q)
021240         COMPUTE �J�E���^�Q = �J�E���^�Q + 1
021250     END-PERFORM.
021260*
021270     PERFORM VARYING �J�E���^ FROM 1 BY 1
021280               UNTIL �J�E���^ > 10
021290         MOVE ���������v�Q(�J�E���^) TO ���������v(�J�E���^)
021300     END-PERFORM.
021310*================================================================*
021320 �ڍ׃��[�h�������� SECTION.
021330*
021340*******************************************************************
021350* �A��|��������s�ɂ��A�����f�[�^�̕��ёւ����s���B
021360* ( �������v�����������A�ޔ��ς݂̕������r�v����
021370*         ���ёւ����s���A�������v���Đݒ肷��B)
021380*******************************************************************
021390*
021400     INITIALIZE �������v.
021410*
021420     PERFORM VARYING �ڍׂb�m�s FROM 1 BY 1
021430                                UNTIL ( �ڍׂb�m�s > 5)
021440*
021450        IF �A��|��������s(�ڍׂb�m�s) NOT = ZERO
021460*
021470           MOVE �A��|��������s(�ڍׂb�m�s) TO ����s�r�v
021480*
021490           IF �A��|���ʈ���敪(�ڍׂb�m�s) = 1
021500*
021510              MOVE �������r�v(�ڍׂb�m�s) TO �������v(����s�r�v)
021520              MOVE �����N�r�v(�ڍׂb�m�s) TO �����N�v(����s�r�v)
021530              MOVE �������r�v(�ڍׂb�m�s) TO �������v(����s�r�v)
021540              MOVE �������r�v(�ڍׂb�m�s) TO �������v(����s�r�v)
021550              MOVE �{�p�J�n�N�r�v(�ڍׂb�m�s) TO �{�p�J�n�N�v(����s�r�v)
021560              MOVE �{�p�J�n���r�v(�ڍׂb�m�s) TO �{�p�J�n���v(����s�r�v)
021570              MOVE �{�p�J�n���r�v(�ڍׂb�m�s) TO �{�p�J�n���v(����s�r�v)
021570              MOVE �����r�v(�ڍׂb�m�s) TO �����v(����s�r�v)
021570              MOVE �񐔂r�v(�ڍׂb�m�s) TO �񐔂v(����s�r�v)
021580           END-IF
021590*
021600           IF �A��|�]�A����敪(�ڍׂb�m�s) = 1
021610              MOVE �{�p�I���N�r�v(�ڍׂb�m�s) TO �{�p�I���N�v(����s�r�v)
021620              MOVE �{�p�I�����r�v(�ڍׂb�m�s) TO �{�p�I�����v(����s�r�v)
021630              MOVE �{�p�I�����r�v(�ڍׂb�m�s) TO �{�p�I�����v(����s�r�v)
021640              MOVE �����`�F�b�N�r�v(�ڍׂb�m�s) TO �����`�F�b�N�v(����s�r�v)
021650              MOVE ���~�`�F�b�N�r�v(�ڍׂb�m�s) TO ���~�`�F�b�N�v(����s�r�v)
021660              MOVE �p���`�F�b�N�r�v(�ڍׂb�m�s) TO �p���`�F�b�N�v(����s�r�v)
021670              MOVE �]��`�F�b�N�r�v(�ڍׂb�m�s) TO �]��`�F�b�N�v(����s�r�v)
021680           END-IF
021690        END-IF
021700*
021710     END-PERFORM.
021720*
021730*================================================================*
021740 �����f�[�^�ޔ� SECTION.
021750*
021760*********************************************************************
021770* �@ ���������擾�̂��߁A�����f�[�^�e���畉�������v�ɑޔ�����B
021780* �A �ڍ׃��[�h�p  �A��|�����s����ɂ��A����ʒu����ёւ��邽�߁A
021790*                  �������v  ���畉�����r�v�ɁA
021800*                  ���������v  ���畉�������r�v�ɑޔ�����B
021810*********************************************************************
021820*
021830     MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �����������Ҕԍ��v(���ʂb�m�s).
021840     MOVE ���|�����A��(���ʂb�m�s)     TO ���������A�Ԃv(���ʂb�m�s).
021850*--------------------------------------------------------------------*
021860     MOVE �������v(���ʂb�m�s) TO �������r�v(���ʂb�m�s).
021870*
021880     MOVE �����N�v(���ʂb�m�s) TO �����N�r�v(���ʂb�m�s).
021890     MOVE �������v(���ʂb�m�s) TO �������r�v(���ʂb�m�s).
021900     MOVE �������v(���ʂb�m�s) TO �������r�v(���ʂb�m�s).
021910*
021920     MOVE �{�p�J�n�N�v(���ʂb�m�s) TO �{�p�J�n�N�r�v(���ʂb�m�s).
021930     MOVE �{�p�J�n���v(���ʂb�m�s) TO �{�p�J�n���r�v(���ʂb�m�s).
021940     MOVE �{�p�J�n���v(���ʂb�m�s) TO �{�p�J�n���r�v(���ʂb�m�s).
021950*
021960     MOVE �{�p�I���N�v(���ʂb�m�s) TO �{�p�I���N�r�v(���ʂb�m�s).
021970     MOVE �{�p�I�����v(���ʂb�m�s) TO �{�p�I�����r�v(���ʂb�m�s).
021980     MOVE �{�p�I�����v(���ʂb�m�s) TO �{�p�I�����r�v(���ʂb�m�s).
021990*
022030     MOVE �����v(���ʂb�m�s)         TO �����r�v(���ʂb�m�s).
022030     MOVE �񐔂v(���ʂb�m�s)         TO �񐔂r�v(���ʂb�m�s).
022050*
022000     MOVE �����`�F�b�N�v(���ʂb�m�s) TO �����`�F�b�N�r�v(���ʂb�m�s).
022010     MOVE ���~�`�F�b�N�v(���ʂb�m�s) TO ���~�`�F�b�N�r�v(���ʂb�m�s).
022020     MOVE �p���`�F�b�N�v(���ʂb�m�s) TO �p���`�F�b�N�r�v(���ʂb�m�s).
022030     MOVE �]��`�F�b�N�v(���ʂb�m�s) TO �]��`�F�b�N�r�v(���ʂb�m�s).
022040*     MOVE �{�p�݌v�񐔂v(���ʂb�m�s) TO �{�p�݌v�񐔂r�v(���ʂb�m�s).
022050*
022060     MOVE �����������Ҕԍ��v(���ʂb�m�s) TO �������Ҕԍ��r�v(���ʂb�m�s).
022070     MOVE ���������A�Ԃv(���ʂb�m�s)     TO �����A�Ԃr�v(���ʂb�m�s).
022080*
022090*================================================================*
022100 ���������Z�b�g SECTION.
022110*
022120**************************************************************************
022130*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
022140**************************************************************************
022150     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
022160     PERFORM VARYING �J�E���^ FROM 1 BY 1
022170             UNTIL ( �J�E���^ > 9 )  OR ( �����������e�����v(�J�E���^) = SPACE )
022180*
022190          INITIALIZE �����������e�����w�v
022200          MOVE �����������e�����v(�J�E���^)   TO �����������e�����w�v
022210          IF  �����������e�P�w�v  NOT = SPACE
022220              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
022230              MOVE �����������e�P�w�v  TO ���������v(�J�E���^�Q)
022240          END-IF
022250          IF  �����������e�Q�w�v  NOT = SPACE
022260              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
022270              MOVE �����������e�Q�w�v  TO ���������v(�J�E���^�Q)
022280          END-IF
022290          IF  �����������e�R�w�v  NOT = SPACE
022300              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
022310              MOVE �����������e�R�w�v  TO ���������v(�J�E���^�Q)
022320          END-IF
022330          IF  �����������e�S�w�v  NOT = SPACE
022340              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
022350              MOVE �����������e�S�w�v  TO ���������v(�J�E���^�Q)
022360          END-IF
022330          IF  �����������e�T�w�v  NOT = SPACE
022340              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
022350              MOVE �����������e�T�w�v  TO ���������v(�J�E���^�Q)
022360          END-IF
022370*
022380     END-PERFORM.
022390*
022400*================================================================*
022410 ������� SECTION.
022420*
022430     EVALUATE ������[�h�e�v�q
022440     WHEN 0
022450         PERFORM ��������Q
022460         PERFORM ��������R
022470     WHEN 1
022480         PERFORM ��������Q
022490     WHEN 2
022500         PERFORM ��������R
022510     WHEN 4
022520         IF �A��|�ی��؈���敪 = 1
022530             PERFORM ��������Q
022540         END-IF
022550             PERFORM ��������R
022560     END-EVALUATE.
022570*================================================================*
022580 ��������Q SECTION.
022590*
022600     MOVE "YIW662P"  TO  ��`�̖��o.
022610     MOVE "GRP001"   TO  ���ڌQ���o.
022620     WRITE YIW662P.
022630*     WRITE ������R�[�h.
022640     PERFORM �G���[�����o.
022650*================================================================*
022660 ��������R SECTION.
022670*
022680     MOVE "YIW662P"  TO  ��`�̖��o.
022690     MOVE "GRP002"   TO  ���ڌQ���o.
022700     WRITE YIW662P.
022710*     WRITE ������R�[�h.
022720     PERFORM �G���[�����o.
022730*================================================================*
022740 �G���[�����o SECTION.
022750*
022760     IF �ʒm���o NOT = "00"
022770         DISPLAY NC"���[�G���["              UPON CONS
022780         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
022790         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
022800         DISPLAY NC"�g������o�F" �g������o UPON CONS
022810         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
022820                                             UPON CONS
022830*-----------------------------------------*
022840         CALL "actcshm"  WITH C LINKAGE
022850*-----------------------------------------*
022860         ACCEPT  �L�[���� FROM CONS
022870         PERFORM �t�@�C����
022880         EXIT PROGRAM
022890     END-IF.
022900*================================================================*
022910 ���̖������� SECTION.
022920*
022930     EVALUATE ��|�ی����
022940     WHEN 05
022950         MOVE 2          TO ���Z�|���Z���
022960     WHEN 70
022970         MOVE 4          TO ���Z�|���Z���
022980     WHEN 80
022990         MOVE 5          TO ���Z�|���Z���
023000     WHEN 85
023010         MOVE 7          TO ���Z�|���Z���
023020     WHEN 90
023030         MOVE 6          TO ���Z�|���Z���
023040     WHEN 91
023050         MOVE 8          TO ���Z�|���Z���
023060     WHEN OTHER
023070         MOVE 1          TO ���Z�|���Z���
023080     END-EVALUATE.
023090     MOVE ��|�{�p�a�� TO ���Z�|�{�p�a��.
023100     MOVE ��|�{�p�N   TO ���Z�|�{�p�N.
023110     MOVE ��|�{�p��   TO ���Z�|�{�p��.
023120     MOVE ��|���Ҕԍ� TO ���Z�|���Ҕԍ�.
023130     MOVE ��|�}��     TO ���Z�|�}��.
023140     READ ���Z�v�g�e
023150     NOT INVALID KEY
023160*
023170         STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
023180                ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
023190                �������̂v                    DELIMITED BY SPACE
023200           INTO �������v(���ʂb�m�s)
023210         END-STRING
023220     END-READ.
023230*
015720*================================================================*
015730 ���S���擾 SECTION.
015740*
015770     MOVE ZERO  TO ���S���v.
015790*
015800     MOVE SPACE TO �A���|���S���擾�L�[.
015810     INITIALIZE �A���|���S���擾�L�[.
015820     MOVE ��|�{�p�a��N�� TO �A���|�{�p�a��N��.
015830     MOVE ��|���҃R�[�h   TO �A���|���҃R�[�h.
015840*
015850     CALL   "HUTANRIT".
015860     CANCEL "HUTANRIT".
015870*
015880* / ���� /
015190     MOVE �A���|���ۖ{�̕��S�� TO ���S���v.
015700     COMPUTE ���S�����v = ���S���v / 10.
           STRING "("          DELIMITED BY SIZE
                  ���S�����v   DELIMITED BY SIZE
                  "��)"        DELIMITED BY SIZE
             INTO ���S�v
           END-STRING.
015900*
014910     EVALUATE ���S���v
014920     WHEN 0
014930         MOVE NC"��" TO �O���`�F�b�N�v
014940     WHEN 10
014950         MOVE NC"��" TO �P���`�F�b�N�v
014960     WHEN 20
014970         MOVE NC"��" TO �Q���`�F�b�N�v
014980     WHEN 30
014990         MOVE NC"��" TO �R���`�F�b�N�v
015000     END-EVALUATE.
015010*
023240*================================================================*
023250  �e�X�g��� SECTION.
023260*
023270      MOVE ALL "9" TO ���Ҕԍ� �ی��Ҕԍ� �ԍ� ���S�Ҕԍ� �󋋎Ҕԍ�
023280                      ��ی��җX�ւP ��ی��җX�ւQ.
023290      MOVE NC"��"  TO �P�� �Q�� �R�� �S�� �T�� �U�� �V�� �W�� �X�� �P�O�� �P�P�� �P�Q��
                            ���ۃ`�F�b�N �Еۃ`�F�b�N �D���`�F�b�N ���ك`�F�b�N �g���`�F�b�N
                            ���σ`�F�b�N �㍂�`�F�b�N �����`�F�b�N ���q���`�F�b�N ����`�F�b�N
                            ���Ғj�`�F�b�N ���ҏ��`�F�b�N 
023330                      �����`�F�b�N �吳�`�F�b�N ���a�`�F�b�N �����`�F�b�N �ߘa�`�F�b�N
023360                      �����`�F�b�N�P �����`�F�b�N�Q �����`�F�b�N�R �����`�F�b�N�S �����`�F�b�N�T
023370                      ���~�`�F�b�N�P ���~�`�F�b�N�Q ���~�`�F�b�N�R ���~�`�F�b�N�S ���~�`�F�b�N�T
023380                      �]��`�F�b�N�P �]��`�F�b�N�Q �]��`�F�b�N�R �]��`�F�b�N�S �]��`�F�b�N�T
                            �O���`�F�b�N �P���`�F�b�N �Q���`�F�b�N �R���`�F�b�N.
023390      MOVE 99      TO ���ҔN ���Ҍ� ���ғ� 
023400                      �����N�P �����N�Q �����N�R �����N�S �����N�T 
023410                      �������P �������Q �������R �������S �������T 
023420                      �������P �������Q �������R �������S �������T 
023430                      �{�p�����N�P �{�p�����N�Q �{�p�����N�R �{�p�����N�S �{�p�����N�T
023440                      �{�p�������P �{�p�������Q �{�p�������R �{�p�������S �{�p�������T
023450                      �{�p�������P �{�p�������Q �{�p�������R �{�p�������S �{�p�������T
023430                      �{�p�J�n�N�P �{�p�J�n�N�Q �{�p�J�n�N�R �{�p�J�n�N�S �{�p�J�n�N�T
023440                      �{�p�J�n���P �{�p�J�n���Q �{�p�J�n���R �{�p�J�n���S �{�p�J�n���T
023450                      �{�p�J�n���P �{�p�J�n���Q �{�p�J�n���R �{�p�J�n���S �{�p�J�n���T
023460                      �{�p�I���N�P �{�p�I���N�Q �{�p�I���N�R �{�p�I���N�S �{�p�I���N�T
023470                      �{�p�I�����P �{�p�I�����Q �{�p�I�����R �{�p�I�����S �{�p�I�����T
023480                      �{�p�I�����P �{�p�I�����Q �{�p�I�����R �{�p�I�����S �{�p�I�����T
                            �����P �����Q �����R �����S �����T �񐔂P �񐔂Q �񐔂R �񐔂S �񐔂T
023490                      ���i�擾�N ���i�擾�� ���i�擾�� �L���N �L���� �L����.
023500      MOVE ALL "��" TO ���Ə��Z���P ���Ə��Z���Q ���Ə����� �ی��ҏZ���P �ی��ҏZ���Q
023510                       ��ی��ҏZ���P ��ی��ҏZ���Q �����於�x�����P
023520                       �L�� ��ی��Ҏ��� ���Ҏ��� ���ҏZ���P ���ҏZ���Q
023530                       ���������P ���������Q ���������R ���������S ���������T
                             ���������U ���������V ���������W ���������X ���������P�O
023530                       ���������P�P ���������P�Q ���������P�R ���������P�S ���������P�T.
023540      MOVE ALL NC"��" TO ���i�a�� �L���a�� ����
023550                         �������P �������Q �������R �������S �������T
                               �{�p�a�� �L���a�� .
023560*================================================================*
023570******************************************************************
023580 END PROGRAM YIW662.
023590******************************************************************
