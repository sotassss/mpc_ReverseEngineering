000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCB6125.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*     �����_���t���� ���Z�v�g����i�_+����޳�ޔŁj
000100*         MED = YAW610 YCB6125P
000110*
      */�����Q�V�N�P�O���{�p��������ԍ������/150922
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2012-11-09
000140 DATE-COMPILED.          2012-11-09
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
000460     SELECT  �o�߃}�X�^      ASSIGN      TO        KEIKAL
000470                             ORGANIZATION             IS  INDEXED
000480                             ACCESS MODE              IS  DYNAMIC
000490                             RECORD KEY               IS  �o�|�敪�R�[�h
000500                                                          �o�|�o�߃R�[�h
000510                             FILE STATUS              IS  ��ԃL�[
000520                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS MODE              IS  DYNAMIC
000560                             RECORD KEY               IS  ���|����敪
000570                             FILE STATUS              IS  ��ԃL�[
000580                             LOCK        MODE         IS  AUTOMATIC.
000590     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000600                             ORGANIZATION             IS  INDEXED
000610                             ACCESS MODE              IS  DYNAMIC
000620                             RECORD KEY               IS  �{��|�{�p���ԍ�
000630                             FILE STATUS              IS  ��ԃL�[
000640                             LOCK        MODE         IS  AUTOMATIC.
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
000770     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000780                             ORGANIZATION             IS  INDEXED
000790                             ACCESS MODE              IS  DYNAMIC
000800                             RECORD KEY               IS  �ہ|�ی����
000810                                                          �ہ|�ی��Ҕԍ�
000820* �����́A�L�[���ڂ̕ی��Җ��̂�ی��҃J�i�ɂ���
000830                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000840                                                          �ہ|�ی��Җ���
000850                                                          �ہ|�ی��Ҕԍ�
000860                             FILE STATUS              IS  ��ԃL�[
000870                             LOCK        MODE         IS  AUTOMATIC.
000950     SELECT  �h�c�Ǘ��}�X�^    ASSIGN      TO      IDKANRL
000960                             ORGANIZATION             IS  INDEXED
000970                             ACCESS MODE              IS  DYNAMIC
000980                             RECORD KEY               IS  �h�c�ǁ|�h�c�敪
000990                                                          �h�c�ǁ|�{�p���ԍ�
001000                                                          �h�c�ǁ|�ی����
001010                                                          �h�c�ǁ|�ی��Ҕԍ�
001020                             ALTERNATE RECORD KEY     IS  �h�c�ǁ|�{�p�h�c�ԍ�
001030                                                          �h�c�ǁ|�h�c�敪
001040                                                          �h�c�ǁ|�{�p���ԍ�
001050                                                          �h�c�ǁ|�ی����
001060                                                          �h�c�ǁ|�ی��Ҕԍ�
001070                             FILE STATUS              IS  ��ԃL�[
001080                             LOCK        MODE         IS  AUTOMATIC.
001090     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
001100                             ORGANIZATION             IS  INDEXED
001110                             ACCESS MODE              IS  DYNAMIC
001120                             RECORD KEY               IS  �s�|������
001130                                                          �s�|�s�����ԍ�
001140                             ALTERNATE RECORD KEY     IS  �s�|������
001150                                                          �s�|�s��������
001160                                                          �s�|�s�����ԍ�
001170                             FILE STATUS              IS  ��ԃL�[
001180                             LOCK        MODE         IS  AUTOMATIC.
001190     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
001200                             ORGANIZATION             IS  INDEXED
001210                             ACCESS MODE              IS  DYNAMIC
001220                             RECORD KEY               IS  ��|�{�p�a��N��
001230                                                          ��|���҃R�[�h
001240                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001250                                                          ��|���҃J�i
001260                                                          ��|���҃R�[�h
001270                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
001280                                                          ��|�{�p�a��N��
001290                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001300                                                          ��|�ی����
001310                                                          ��|�ی��Ҕԍ�
001320                                                          ��|���҃R�[�h
001330                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001340                                                          ��|������
001350                                                          ��|��p���S�Ҕԍ�
001360                                                          ��|���҃R�[�h
001370                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001380                                                          ��|�������
001390                                                          ��|��p���S�Ҕԍ�����
001400                                                          ��|���҃R�[�h
001410                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
001420                                                          ��|�{�p�a��N��
001430                                                          ��|���҃R�[�h
001440                             FILE STATUS              IS  ��ԃL�[
001450                             LOCK        MODE         IS  AUTOMATIC.
001460     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
001470                             ORGANIZATION             IS  INDEXED
001480                             ACCESS MODE              IS  DYNAMIC
001490                             RECORD KEY               IS  �{�L�|�{�p�a��N����
001500                                                          �{�L�|���҃R�[�h
001510                             ALTERNATE RECORD KEY     IS  �{�L�|���҃R�[�h
001520                                                          �{�L�|�{�p�a��N����
001530                             FILE STATUS              IS  ��ԃL�[
001540                             LOCK        MODE         IS  AUTOMATIC.
001550     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
001560                             ORGANIZATION             IS  INDEXED
001570                             ACCESS MODE              IS  DYNAMIC
001580                             RECORD KEY               IS  ���|�{�p�a��N��
001590                                                          ���|���҃R�[�h
001600                             ALTERNATE RECORD KEY     IS  ���|���҃R�[�h
001610                                                          ���|�{�p�a��N��
001620                             FILE STATUS              IS  ��ԃL�[
001630                             LOCK        MODE         IS  AUTOMATIC.
001640     SELECT  ���������e      ASSIGN      TO        HUGEINL
001650                             ORGANIZATION             IS  INDEXED
001660                             ACCESS MODE              IS  DYNAMIC
001670                             RECORD KEY               IS  �����|�敪�R�[�h
001680                                                          �����|���������R�[�h
001690                             FILE STATUS              IS  ��ԃL�[
001700                             LOCK        MODE         IS  AUTOMATIC.
001860* ���я��󎚗p
001870     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001880                             ORGANIZATION             IS  INDEXED
001890                             ACCESS                   IS  DYNAMIC
001900                             RECORD      KEY          IS  ��Q�|�{�p�a��N��
001910                                                          ��Q�|���҃R�[�h
001920                                                          ��Q�|�ی����
001930                             FILE        STATUS       IS  ��ԃL�[
001940                             LOCK        MODE         IS  AUTOMATIC.
001850     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
001860                             SYMBOLIC    DESTINATION  IS "PRT"
001870                             FORMAT                   IS  ��`�̖��o
001880                             GROUP                    IS  ���ڌQ���o
001890                             PROCESSING  MODE         IS  ������ʂo
001900                             UNIT        CONTROL      IS  �g������o
001910                             FILE        STATUS       IS  �ʒm���o.
001920******************************************************************
001930*                      DATA DIVISION                             *
001940******************************************************************
001950 DATA                    DIVISION.
001960 FILE                    SECTION.
001970*                           �m�q�k��  �P�Q�W�n
001980 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001990     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002000*                           �m�q�k��  �P�Q�W�n
002010 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
002020     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
002060*                           �m�q�k��  �P�Q�W�n
002070 FD  �o�߃}�X�^          BLOCK   CONTAINS   1   RECORDS.
002080     COPY KEIKA           OF  XFDLIB  JOINING   �o   AS  PREFIX.
002090*                           �m�q�k��  �Q�T�U�n
002100 FD  ������}�X�^      BLOCK   CONTAINS   1   RECORDS.
002110     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002120*                           �m�q�k��  �P�Q�W�n
002130 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
002140     COPY SEJOHO          OF  XFDLIB  JOINING   �{�� AS  PREFIX.
002150*                           �m�q�k��  �U�S�O�n
002160 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
002170     COPY KAIJOHO         OF  XFDLIB  JOINING   ��� AS  PREFIX.
002180*                           �m�q�k��  �R�Q�O�n
002190 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
002200     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002240*                           �m�q�k��  �P�Q�W�n
002250 FD  �h�c�Ǘ��}�X�^      BLOCK   CONTAINS   1   RECORDS.
002260     COPY IDKANR          OF  XFDLIB  JOINING   �h�c�� AS  PREFIX.
002270*                           �m�q�k��  �Q�T�U�n
002280 FD  �s�����}�X�^        BLOCK   CONTAINS   1   RECORDS.
002290     COPY SITYOSN         OF  XFDLIB  JOINING   �s   AS  PREFIX.
002300*                           �m�q�k��  �R�Q�O�n
002310 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
002320     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002330*                           �m�q�k��  �Q�T�U�n
002340 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
002350     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
002360*                           �m�q�k��  �P�Q�W�n
002370 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
002380     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002390*                           �m�q�k��  �P�Q�W�n
002400 FD  ���������e         BLOCK   CONTAINS    1   RECORDS.
002410     COPY HUGEIN          OF  XFDLIB  JOINING   ���� AS  PREFIX.
002570*
002580 FD  ��ƃt�@�C���Q RECORD  CONTAINS 32 CHARACTERS.
002590 01  ��Q�|���R�[�h.
002600     03  ��Q�|���R�[�h�L�[.
002610         05  ��Q�|�{�p�a��N��.
002620             07  ��Q�|�{�p�a��            PIC 9.
002630             07  ��Q�|�{�p�N              PIC 9(2).
002640             07  ��Q�|�{�p��              PIC 9(2).
002650         05  ��Q�|���҃R�[�h.
002660             07 ��Q�|���Ҕԍ�             PIC 9(6).
002670             07 ��Q�|�}��                 PIC X(1).
002680         05  ��Q�|�ی����                PIC 9(2).
002690     03  ��Q�|���R�[�h�f�[�^.
002700         05  ��Q�|����                    PIC 9(4).
002710         05  FILLER                        PIC X(14).
002450*
002460 FD  ����t�@�C��.
002470     COPY YCB6125P         OF  XMDLIB.
002480*----------------------------------------------------------------*
002490******************************************************************
002500*                WORKING-STORAGE SECTION                         *
002510******************************************************************
002520 WORKING-STORAGE         SECTION.
002530 01 �L�[����                           PIC X     VALUE SPACE.
002540 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
002550 01 �I���t���O                         PIC X(3)  VALUE SPACE.
002560 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
002570 01 �t�@�C����                         PIC N(6)  VALUE SPACE.
002580 01 �O�a��v                           PIC 9     VALUE ZERO.
001363 01 �S�p��                           PIC X(2)  VALUE X"8140".
001364 01 ���p��                           PIC X(2)  VALUE X"2020".
002590*
002600*--- ����}�X�^�ޔ� ---*
002610 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
002620*
002630** ���������E�������R����敪�p
002640 01 ������������敪�v                 PIC 9     VALUE ZERO.
002650 01 �������R����敪�v                 PIC 9     VALUE ZERO.
002660*
002670** ���Z���i�̓��t�敪�p (0:�ŏI�ʉ@���A1:�������A9:�󎚂Ȃ�)
002680 01 ���Z�v�g���t�敪�v                 PIC 9     VALUE ZERO.
002690 01 ���Z�v�g���ғ��t�敪�v             PIC 9     VALUE ZERO.
002700*
002710*--- �J�E���^ ---*
002720 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002730*
002740*--- �ی��Ҕԍ���r�p ---*
002750 01 �ی��Ҕԍ���r�v                   PIC X(6)  VALUE SPACE.
002760*
002770*--- �����f�[�^�擾�p ---*
002780 01 �������̂v                         PIC N(10) VALUE SPACE.
002790 01 ���ʖ��̂v                         PIC N(20) VALUE SPACE.
002800 01 ���ʒ��v                           PIC 9(2)  VALUE 1.
002810 01 �o�ߕ��ʂv                         PIC N(1)  VALUE SPACE.
002820*
002830** �}�Ԕ���p
002840 01 �J�n�f�Ó��蓮�敪�v               PIC 9     VALUE ZERO.
002850*
002860* ������������敪
002870 01 ���Z������������敪�v             PIC 9     VALUE ZERO.
002580 01 ���Z�������R����敪�v             PIC 9    VALUE ZERO.
002880*
002890*--- �{�p�L�^�擾�p ---*
002900 01 �����Č��t���O                     PIC X(3)  VALUE SPACE.
002910 01 �O���t���O                         PIC X(3)  VALUE SPACE.
002920*
002930 01 �I���N�����v�s.
002940    03 �I���N�v�s                      PIC 9(2)  VALUE ZERO.
002950    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
002960    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
002970*
002980** �O������p
002990 01 �v�Z�N�����v.
003000    03 �v�Z�a��v                      PIC 9(1)  VALUE ZERO.
003010    03 �v�Z�N�v                        PIC S9(2) VALUE ZERO.
003020    03 �v�Z���v                        PIC S9(2) VALUE ZERO.
003030    03 �v�Z���v                        PIC S9(2) VALUE ZERO.
003040 01 �J�n�N�����Q�v.
003050    03 �J�n�a��Q�v                    PIC 9(1)  VALUE ZERO.
003060    03 �J�n�N�Q�v                      PIC 9(2)  VALUE ZERO.
003070    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
003080    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
003090    03 �J�n����N�v                    PIC S9(4) VALUE ZERO.
003100 01 �I���N�����Q�v.
003110    03 �I���a��Q�v                    PIC 9(1)  VALUE ZERO.
003120    03 �I���N�Q�v                      PIC 9(2)  VALUE ZERO.
003130    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
003140    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
003150    03 �I������N�v                    PIC S9(4) VALUE ZERO.
003160*
003170*--- �������ޔ�p ---*
003180 01 �����t���O                         PIC X(3)  VALUE SPACE.
003190*
003200 01 �����N�����v�s.
003210    03 �����a��v�s                    PIC 9     VALUE ZERO.
003220    03 �����N�v�s                      PIC 9(2)  VALUE ZERO.
003230    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003240    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003250*
003260*--- �������Z�����p ---*
003270 01 �������Z�v�s.
003280    03 �������Z�J�E���g                PIC 9     VALUE ZERO.
003290    03 �ԍ��J�E���^                    PIC 9     VALUE ZERO.
003300    03 �������Z�W�c�v�s  OCCURS 3.
003310       05 �������Z�敪�v�s             PIC 9     VALUE ZERO.
003320       05 �������Z���v�s               PIC 9(2)  VALUE ZERO.
003330       05 �������Z���v�s               PIC 9(2)  VALUE ZERO.
003340    03 �������Z�W�c�m�v  OCCURS 3.
003350       05 ���Z��؂v                   PIC N(1)  VALUE SPACE.
003360       05 ���Z���e�v                   PIC N(3)  VALUE SPACE.
003370       05 �������Z���m�v�P             PIC N(1)  VALUE SPACE.
003380       05 �������Z���m�v�Q             PIC N(1)  VALUE SPACE.
003390       05 ���Œ�v                     PIC N(1)  VALUE SPACE.
003400       05 �������Z���m�v�P             PIC N(1)  VALUE SPACE.
003410       05 �������Z���m�v�Q             PIC N(1)  VALUE SPACE.
003420       05 ���Œ�v                     PIC N(1)  VALUE SPACE.
003430    03 �������Z�����P�v                PIC N(10) VALUE SPACE.
003440    03 �������Z�����Q�v                PIC N(10) VALUE SPACE.
003450    03 �������Z�����R�v                PIC N(10) VALUE SPACE.
003070    03 �������Z��؂v                  PIC X     VALUE SPACE.
003080    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003090    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003460*
003470** ���������{��ϊ�
003480 01 �����v                             PIC 9(2).
003490 01 �����q REDEFINES �����v.
003500    03 �����v�P                        PIC X(1).
003510    03 �����v�Q                        PIC X(1).
003520*
003530 01 �����ԍ��v                         PIC 9.
003540 01 �����ԍ��q REDEFINES �����ԍ��v.
003550    03 �����ԍ��v�P                    PIC X.
003560*
003570 01 �S�p�����ԍ��v                     PIC N.
003580 01 �S�p�����ԍ��q REDEFINES �S�p�����ԍ��v.
003590    03 �S�p�����ԍ��v�P                PIC X(2).
003600*
003610*--- ���������p ---*
003620 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
003630 01 �J�E���^�Q                         PIC 9(2)  VALUE ZERO.
003640 01 ���������v�s.
003650    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
003660    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
003670    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
003680    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
003690    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
003700    03 ���������i���o�[�v�s.
003710       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
003720    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
003730 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
003740 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
003750 01 ���������s�a�k.
003760    03 ���������R�[�h�s�a�k            OCCURS 9.
003770       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
003780       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
003790       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
003800 01 �����������e�v.
003810    03 �����������e�����v              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 �����������e�����w�v.
003630       05 �����������e�P�w�v           PIC X(80)  VALUE SPACE.
003640       05 �����������e�Q�w�v           PIC X(80)  VALUE SPACE.
003640       05 �����������e�R�w�v           PIC X(80)  VALUE SPACE.
003650       05 �����������e�S�w�v           PIC X(78)  VALUE SPACE.
003800 01 ���������v�o                       PIC N(225) VALUE SPACE.
       01 ���������v�q�o.
003810    03 ���������v�q                    PIC N(45) OCCURS 5 VALUE SPACE.
003860*
003870*--- �ϔC�N�����p ---*
003880 01 �󗝔N�����v.
003890    03 �󗝔N�v                        PIC 9(2)  VALUE ZERO.
003900    03 �󗝌��v                        PIC 9(2)  VALUE ZERO.
003910    03 �󗝓��v                        PIC 9(2)  VALUE ZERO.
003920 01 �ŏI�ʉ@�N�����v.
003930    03 �ŏI�ʉ@�N�v                    PIC 9(2)  VALUE ZERO.
003940    03 �ŏI�ʉ@���v                    PIC 9(2)  VALUE ZERO.
003950    03 �ŏI�ʉ@���v                    PIC 9(2)  VALUE ZERO.
003960** �������p
003970 01 �{�p����N�v                       PIC 9(4)  VALUE ZERO.
003980 01 ���v                               PIC 9(3)  VALUE ZERO.
003990 01 �]�v                               PIC 9(3)  VALUE ZERO.
004000*
004010*--- ��ϔC�p ---*
004020 01 ��ϔC�t���O                     PIC X(3)  VALUE SPACE.
004030 01 �������v                           PIC 9(2)  VALUE ZERO.
004040*
004050 01 ���t�ҏW�v.
004060   03 �����ҏW�v                       PIC N(2)  VALUE SPACE.
004070   03 �N�ҏW�v                         PIC ZZ    VALUE ZERO.
004080   03 FILLER                           PIC X(2)  VALUE "�N".
004090   03 ���ҏW�v                         PIC ZZ    VALUE ZERO.
004100   03 FILLER                           PIC X(2)  VALUE "��".
004110   03 ���ҏW�v                         PIC ZZ    VALUE ZERO.
004120   03 FILLER                           PIC X(2)  VALUE "��".
004130*
004140*--- ���S���t�����p ---*
004150 01 ���S�����v                         PIC 9(2)  VALUE ZERO.
004160 01 ���t�����v                         PIC 9(2)  VALUE ZERO.
004170*
004180*--- ���Z�v�g�񐔗p ---*
004190 01 �񐔂v                             PIC 9(2)  VALUE ZERO.
004200*
004210 01 �ŏ��J�n�a��N���v.
004220    03 �ŏ��J�n�a��v                  PIC 9(1)  VALUE ZERO.
004230    03 �ŏ��J�n�N�v                    PIC 9(2)  VALUE ZERO.
004240    03 �ŏ��J�n���v                    PIC 9(2)  VALUE ZERO.
004250*
004260*--- �{�p�h�c�p ---*
004270 01 �{�p�h�c�Œ�v                     PIC X(14) VALUE "�{�p�@�֔ԍ��F".
004280*
004290*--- �������Z�܂Ƃߗp ---*
004300 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
004310 01 ������ʗ��̂v                     PIC N(4)  VALUE SPACE.
004320 01 ������ʗ��̂v�Q                   PIC N(4)  VALUE SPACE.
003920*
003930*--- ���ρE���q���p ---*
003940* ���ϔԍ��p
003950 01 �E�o�t���O                         PIC X(3)  VALUE SPACE.
003960 01 ���ϘA�ԍ��W�c�v.
003970    03 ���ϘA�ԍ����v                  PIC X(14) VALUE SPACE.
003980    03 ���ϘA�ԍ����m�v REDEFINES  ���ϘA�ԍ����v  PIC N(7).
          03 ���ϘA�ԍ��v�o.
003990       05 ���ϘA�ԍ��v                 PIC X(6)  VALUE SPACE.
004000       05 ���ϘA�ԍ��P�ʂv             PIC X(2)  VALUE SPACE.
004010       05 ���ϘA�ԍ��P�ʂm�v REDEFINES  ���ϘA�ԍ��P�ʂv  PIC N.
004020* ���q���ԍ��p
004030 01 ���q���ԍ��W�c�v.
004040    03 ���q���ԍ����v                  PIC X(8)  VALUE SPACE.
004050    03 ���q���ԍ����m�v REDEFINES  ���q���ԍ����v  PIC N(4).
          03 ���q���ԍ��v�o.
004060       05 ���q���ԍ��v                 PIC X(6)  VALUE SPACE.
004070       05 ���q���ԍ��P�ʂv             PIC X(2)  VALUE SPACE.
004080       05 ���q���ԍ��P�ʂm�v REDEFINES  ���q���ԍ��P�ʂv  PIC N.
      *
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
       01 �������q�b�l                       PIC X(140) VALUE SPACE.
       01 �^����Âb�l                       PIC X(68)  VALUE SPACE.
004330*
004340**--- �T����  �E�v����  �ҏW�p ---*
004350* 01 ���ʂT�v.
004360*   03 FILLER                           PIC X(1).
004370*   03 �����Œ�T�v                     PIC X(6).
004380**   03 FILLER                           PIC X(2).
004390*   03 �����J�n�����T�v.
004400*      05 �����J�n���T�v                PIC ZZ.
004410*      05 FILLER                        PIC X(2).
004420*      05 �����J�n���T�v                PIC ZZ.
004430*   03 FILLER                           PIC X(2).
004440*   03 ��ÂT�v.
004450*      05 ��ÒP���T�v                  PIC ZZZZ.
004460*      05 FILLER                        PIC X(2).
004470*      05 ��É񐔂T�v                  PIC ZZ.
004480*      05 FILLER                        PIC X(2).
004490*      05 ��×��T�v                    PIC ZZ,ZZZ.
004500*   03 FILLER                           PIC X(3).
004510*   03 ��㪖@�T�v.
004520*      05 ��㪖@�񐔂T�v                PIC ZZ.
004530*      05 FILLER                        PIC X(2).
004540*      05 ��㪖@���T�v                  PIC ZZZZ.
004550*   03 FILLER                           PIC X(3).
004560*   03 ��㪖@�T�v.
004570*      05 ��㪖@�񐔂T�v                PIC ZZ.
004580*      05 FILLER                        PIC X(2).
004590*      05 ��㪖@���T�v                  PIC ZZZZ.
004600*   03 FILLER                           PIC X(3).
004610*   03 �d�ÂT�v.
004620*      05 �d�É񐔂T�v                  PIC ZZ.
004630*      05 FILLER                        PIC X(2).
004640*      05 �d�×��T�v                    PIC ZZZZ.
004650*   03 FILLER                           PIC X(2).
004660*   03 ���v�T�v                         PIC ZZ,ZZZ.
004670*   03 FILLER                           PIC X(1).
004680*   03 �����ʗ��T�v                     PIC X(4).
004690*   03 FILLER                           PIC X(3).
004700*   03 �����ʍ����v�T�v                 PIC ZZ,ZZZ.
004710*   03 FILLER                           PIC X(3).
004720*   03 �����������T�v                   PIC 9.9.
004730*   03 FILLER                           PIC X(3).
004740*   03 ���������v�T�v                   PIC ZZ,ZZZ.
004750*   03 FILLER                           PIC X(4).
004760*
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
005210*
004770****************
004780* �A�����ڑҔ� *
004790****************
004800*    ************
004810*    * ����L�[ *
004820*    ************
004830 01 �Ώۃf�[�^�v�q.
004840    03 �{�p�a��N���v�q.
004850       05 �{�p�a��v�q                 PIC 9(1)  VALUE ZERO.
004860       05 �{�p�N�v�q                   PIC 9(2)  VALUE ZERO.
004870       05 �{�p���v�q                   PIC 9(2)  VALUE ZERO.
004880    03 �ی���ʂv�q                    PIC 9(2)  VALUE ZERO.
004890    03 �ی��Ҕԍ��v�q                  PIC X(10) VALUE SPACE.
004900    03 �����ʂv�q                    PIC 9(2)  VALUE ZERO.
004910    03 ��p���S�Ҕԍ��v�q              PIC X(10) VALUE SPACE.
004920    03 ������ʂv�q                    PIC 9(2)  VALUE ZERO.
004930    03 ��p���S�Ҕԍ������v�q          PIC X(10) VALUE SPACE.
004940    03 �{�l�Ƒ��敪�v�q                PIC 9(1)  VALUE ZERO.
004950    03 ���҃J�i�v�q                    PIC X(50) VALUE SPACE.
004960    03 ���҃R�[�h�v�q.
004970       05 ���Ҕԍ��v�q                 PIC 9(6)  VALUE ZERO.
004980       05 �}�Ԃv�q                     PIC X(1)  VALUE SPACE.
004990*    ************
005000*    * ������� *
005010*    ************
005020*    �����̗���
005030***********************
005040 01 �����P�v�q.
005050   03 �����v�q.
005060      05 ���S�����v�q                  PIC 9(3)  VALUE ZERO.
005070      05 �������v�q                    PIC 9(5)  VALUE ZERO.
005080      05 �������Z���v�q                PIC 9(5)  VALUE ZERO.
005090   03 ���k���v�q                       PIC 9(4)  VALUE ZERO.
005100   03 �Č����v�q                       PIC 9(5)  VALUE ZERO.
005110   03 ���Âv�q.
005120      05 ���Ë����v�q                  PIC 9(2)V9 VALUE ZERO.
005130      05 ���É񐔂v�q                  PIC 9(2)  VALUE ZERO.
005140      05 ���×��v�q                    PIC 9(5)  VALUE ZERO.
005150      05 ���É��Z���v�q                PIC 9(5)  VALUE ZERO.
005160   03 �������q���Z���v�q               PIC 9(5)  VALUE ZERO.
005170   03 �{�p���񋟗��v�q               PIC 9(5)  VALUE ZERO.
005180   03 ���v�v�q                         PIC 9(6)  VALUE ZERO.
005190   03 �ꕔ���S���v�q                   PIC 9(6)  VALUE ZERO.
005200   03 �������z�v�q                     PIC 9(6)  VALUE ZERO.
005210   03 ���t�����v�q                     PIC 9(1)  VALUE ZERO.
005220   03 �󋋎ҕ��S�z�v�q                 PIC 9(6)  VALUE ZERO.
005230   03 �����������z�v�q                 PIC 9(6)  VALUE ZERO.
005240*
005250* �������ʖ��̗���
005260***********************
005270 01 �����Q�v�q.
005280   03 ���񏈒u�v�q    OCCURS   9.
005290      05 ���񏈒u���v�q                PIC 9(5)  VALUE ZERO.
005300*
005310* �������̗���
005320***********************
005330 01 �����R�v�q.
005340**********
005350* �P���� *
005360**********
005370   03 ���ʂP�v�q.
005380      05 ��ÂP�v�q.
005390         07 ��ÒP���P�v�q             PIC 9(4)  VALUE ZERO.
005400         07 ��É񐔂P�v�q             PIC 9(2)  VALUE ZERO.
005410         07 ��×��P�v�q               PIC 9(5)  VALUE ZERO.
005420      05 ��㪖@�P�v�q.
005430         07 ��㪖@�񐔂P�v�q           PIC 9(2)  VALUE ZERO.
005440         07 ��㪖@���P�v�q             PIC 9(4)  VALUE ZERO.
005450      05 ��㪖@�P�v�q.
005460         07 ��㪖@�񐔂P�v�q           PIC 9(2)  VALUE ZERO.
005470         07 ��㪖@���P�v�q             PIC 9(4)  VALUE ZERO.
005480      05 �d�ÂP�v�q.
005490         07 �d�É񐔂P�v�q             PIC 9(2)  VALUE ZERO.
005500         07 �d�×��P�v�q               PIC 9(4)  VALUE ZERO.
005510      05 ���v�P�v�q                    PIC 9(6)  VALUE ZERO.
005520      05 �����������P�v�q              PIC 9(3)  VALUE ZERO.
005530      05 ���������v�P�v�q              PIC 9(6)  VALUE ZERO.
005540**********
005550* �Q���� *
005560**********
005570   03 ���ʂQ�v�q.
005580      05 ��ÂQ�v�q.
005590         07 ��ÒP���Q�v�q             PIC 9(4)  VALUE ZERO.
005600         07 ��É񐔂Q�v�q             PIC 9(2)  VALUE ZERO.
005610         07 ��×��Q�v�q               PIC 9(5)  VALUE ZERO.
005620      05 ��㪖@�Q�v�q.
005630         07 ��㪖@�񐔂Q�v�q           PIC 9(2)  VALUE ZERO.
005640         07 ��㪖@���Q�v�q             PIC 9(4)  VALUE ZERO.
005650      05 ��㪖@�Q�v�q.
005660         07 ��㪖@�񐔂Q�v�q           PIC 9(2)  VALUE ZERO.
005670         07 ��㪖@���Q�v�q             PIC 9(4)  VALUE ZERO.
005680      05 �d�ÂQ�v�q.
005690         07 �d�É񐔂Q�v�q             PIC 9(2)  VALUE ZERO.
005700         07 �d�×��Q�v�q               PIC 9(4)  VALUE ZERO.
005710      05 ���v�Q�v�q                    PIC 9(6)  VALUE ZERO.
005720      05 �����������Q�v�q              PIC 9(3)  VALUE ZERO.
005730      05 ���������v�Q�v�q              PIC 9(6)  VALUE ZERO.
005740******************
005750* �R���ʁ^�W�� *
005760******************
005770   03 ���ʂR�W�v�q.
005780      05 ��ÂR�W�v�q.
005790         07 ��ÒP���R�W�v�q           PIC 9(4)  VALUE ZERO.
005800         07 ��É񐔂R�W�v�q           PIC 9(2)  VALUE ZERO.
005810         07 ��×��R�W�v�q             PIC 9(5)  VALUE ZERO.
005820      05 ��㪖@�R�W�v�q.
005830         07 ��㪖@�񐔂R�W�v�q         PIC 9(2)  VALUE ZERO.
005840         07 ��㪖@���R�W�v�q           PIC 9(4)  VALUE ZERO.
005850      05 ��㪖@�R�W�v�q.
005860         07 ��㪖@�񐔂R�W�v�q         PIC 9(2)  VALUE ZERO.
005870         07 ��㪖@���R�W�v�q           PIC 9(4)  VALUE ZERO.
005880      05 �d�ÂR�W�v�q.
005890         07 �d�É񐔂R�W�v�q           PIC 9(2)  VALUE ZERO.
005900         07 �d�×��R�W�v�q             PIC 9(4)  VALUE ZERO.
005910      05 ���v�R�W�v�q                  PIC 9(6)  VALUE ZERO.
005920      05 �����ʍ����v�R�W�v�q          PIC 9(6)  VALUE ZERO.
005930      05 �����������R�W�v�q            PIC 9(3)  VALUE ZERO.
005940      05 ���������v�R�W�v�q            PIC 9(6)  VALUE ZERO.
005950******************
005960* �R���ʁ^�P�O�� *
005970******************
005980   03 ���ʂR�O�v�q.
005990      05 �����J�n�����R�O�v�q.
006000         07 �����J�n���R�O�v�q         PIC 9(2)  VALUE ZERO.
006010         07 �����J�n���R�O�v�q         PIC 9(2)  VALUE ZERO.
006020      05 ��ÂR�O�v�q.
006030         07 ��ÒP���R�O�v�q           PIC 9(4)  VALUE ZERO.
006040         07 ��É񐔂R�O�v�q           PIC 9(2)  VALUE ZERO.
006050         07 ��×��R�O�v�q             PIC 9(5)  VALUE ZERO.
006060      05 ��㪖@�R�O�v�q.
006070         07 ��㪖@�񐔂R�O�v�q         PIC 9(2)  VALUE ZERO.
006080         07 ��㪖@���R�O�v�q           PIC 9(4)  VALUE ZERO.
006090      05 ��㪖@�R�O�v�q.
006100         07 ��㪖@�񐔂R�O�v�q         PIC 9(2)  VALUE ZERO.
006110         07 ��㪖@���R�O�v�q           PIC 9(4)  VALUE ZERO.
006120      05 �d�ÂR�O�v�q.
006130         07 �d�É񐔂R�O�v�q           PIC 9(2)  VALUE ZERO.
006140         07 �d�×��R�O�v�q             PIC 9(4)  VALUE ZERO.
006150      05 ���v�R�O�v�q                  PIC 9(6)  VALUE ZERO.
006160      05 �����������R�O�v�q            PIC 9(3)  VALUE ZERO.
006170      05 ���������v�R�O�v�q            PIC 9(6)  VALUE ZERO.
006180****************
006190* �S���ʁ^�T�� *
006200****************
006210   03 ���ʂS�T�v�q.
006220      05 ��ÂS�T�v�q.
006230         07 ��ÒP���S�T�v�q           PIC 9(4)  VALUE ZERO.
006240         07 ��É񐔂S�T�v�q           PIC 9(2)  VALUE ZERO.
006250         07 ��×��S�T�v�q             PIC 9(5)  VALUE ZERO.
006260      05 ��㪖@�S�T�v�q.
006270         07 ��㪖@�񐔂S�T�v�q         PIC 9(2)  VALUE ZERO.
006280         07 ��㪖@���S�T�v�q           PIC 9(4)  VALUE ZERO.
006290      05 ��㪖@�S�T�v�q.
006300         07 ��㪖@�񐔂S�T�v�q         PIC 9(2)  VALUE ZERO.
006310         07 ��㪖@���S�T�v�q           PIC 9(4)  VALUE ZERO.
006320      05 �d�ÂS�T�v�q.
006330         07 �d�É񐔂S�T�v�q           PIC 9(2)  VALUE ZERO.
006340         07 �d�×��S�T�v�q             PIC 9(4)  VALUE ZERO.
006350      05 ���v�S�T�v�q                  PIC 9(6)  VALUE ZERO.
006360      05 �����ʍ����v�S�T�v�q          PIC 9(6)  VALUE ZERO.
006370      05 �����������S�T�v�q            PIC 9(3)  VALUE ZERO.
006380      05 ���������v�S�T�v�q            PIC 9(6)  VALUE ZERO.
006390****************
006400* �S���ʁ^�W�� *
006410****************
006420   03 ���ʂS�W�v�q.
006430      05 �����J�n�����S�W�v�q.
006440         07 �����J�n���S�W�v�q         PIC 9(2)  VALUE ZERO.
006450         07 �����J�n���S�W�v�q         PIC 9(2)  VALUE ZERO.
006460      05 ��ÂS�W�v�q.
006470         07 ��ÒP���S�W�v�q           PIC 9(4)  VALUE ZERO.
006480         07 ��É񐔂S�W�v�q           PIC 9(2)  VALUE ZERO.
006490         07 ��×��S�W�v�q             PIC 9(5)  VALUE ZERO.
006500      05 ��㪖@�S�W�v�q.
006510         07 ��㪖@�񐔂S�W�v�q         PIC 9(2)  VALUE ZERO.
006520         07 ��㪖@���S�W�v�q           PIC 9(4)  VALUE ZERO.
006530      05 ��㪖@�S�W�v�q.
006540         07 ��㪖@�񐔂S�W�v�q         PIC 9(2)  VALUE ZERO.
006550         07 ��㪖@���S�W�v�q           PIC 9(4)  VALUE ZERO.
006560      05 �d�ÂS�W�v�q.
006570         07 �d�É񐔂S�W�v�q           PIC 9(2)  VALUE ZERO.
006580         07 �d�×��S�W�v�q             PIC 9(4)  VALUE ZERO.
006590      05 ���v�S�W�v�q                  PIC 9(6)  VALUE ZERO.
006600      05 �����ʍ����v�S�W�v�q          PIC 9(6)  VALUE ZERO.
006610      05 �����������S�W�v�q            PIC 9(3)  VALUE ZERO.
006620      05 ���������v�S�W�v�q            PIC 9(6)  VALUE ZERO.
006630******************
006640* �S���ʁ^�P�O�� *
006650******************
006660   03 ���ʂS�O�v�q.
006670      05 �����J�n�����S�O�v�q.
006680         07 �����J�n���S�O�v�q         PIC 9(2)  VALUE ZERO.
006690         07 �����J�n���S�O�v�q         PIC 9(2)  VALUE ZERO.
006700      05 ��ÂS�O�v�q.
006710         07 ��ÒP���S�O�v�q           PIC 9(4)  VALUE ZERO.
006720         07 ��É񐔂S�O�v�q           PIC 9(2)  VALUE ZERO.
006730         07 ��×��S�O�v�q             PIC 9(5)  VALUE ZERO.
006740      05 ��㪖@�S�O�v�q.
006750         07 ��㪖@�񐔂S�O�v�q         PIC 9(2)  VALUE ZERO.
006760         07 ��㪖@���S�O�v�q           PIC 9(4)  VALUE ZERO.
006770      05 ��㪖@�S�O�v�q.
006780         07 ��㪖@�񐔂S�O�v�q         PIC 9(2)  VALUE ZERO.
006790         07 ��㪖@���S�O�v�q           PIC 9(4)  VALUE ZERO.
006800      05 �d�ÂS�O�v�q.
006810         07 �d�É񐔂S�O�v�q           PIC 9(2)  VALUE ZERO.
006820         07 �d�×��S�O�v�q             PIC 9(4)  VALUE ZERO.
006830      05 ���v�S�O�v�q                  PIC 9(6)  VALUE ZERO.
006840      05 �����������S�O�v�q            PIC 9(3)  VALUE ZERO.
006850      05 ���������v�S�O�v�q            PIC 9(6)  VALUE ZERO.
006860********************
006870* �T���ʁ^�Q�D�T�� *
006880********************
006890   03 ���ʂT�Q�v�q.
006900      05 ��ÂT�Q�v�q.
006910         07 ��ÒP���T�Q�v�q           PIC 9(4)  VALUE ZERO.
006920         07 ��É񐔂T�Q�v�q           PIC 9(2)  VALUE ZERO.
006930         07 ��×��T�Q�v�q             PIC 9(5)  VALUE ZERO.
006940      05 ��㪖@�T�Q�v�q.
006950         07 ��㪖@�񐔂T�Q�v�q         PIC 9(2)  VALUE ZERO.
006960         07 ��㪖@���T�Q�v�q           PIC 9(4)  VALUE ZERO.
006970      05 ��㪖@�T�Q�v�q.
006980         07 ��㪖@�񐔂T�Q�v�q         PIC 9(2)  VALUE ZERO.
006990         07 ��㪖@���T�Q�v�q           PIC 9(4)  VALUE ZERO.
007000      05 �d�ÂT�Q�v�q.
007010         07 �d�É񐔂T�Q�v�q           PIC 9(2)  VALUE ZERO.
007020         07 �d�×��T�Q�v�q             PIC 9(4)  VALUE ZERO.
007030      05 ���v�T�Q�v�q                  PIC 9(6)  VALUE ZERO.
007040      05 �����ʍ����v�T�Q�v�q          PIC 9(6)  VALUE ZERO.
007050      05 �����������T�Q�v�q            PIC 9(3)  VALUE ZERO.
007060      05 ���������v�T�Q�v�q            PIC 9(6)  VALUE ZERO.
007070****************
007080* �T���ʁ^�T�� *
007090****************
007100   03 ���ʂT�T�v�q.
007110      05 �����J�n�����T�T�v�q.
007120         07 �����J�n���T�T�v�q         PIC 9(2)  VALUE ZERO.
007130         07 �����J�n���T�T�v�q         PIC 9(2)  VALUE ZERO.
007140      05 ��ÂT�T�v�q.
007150         07 ��ÒP���T�T�v�q           PIC 9(4)  VALUE ZERO.
007160         07 ��É񐔂T�T�v�q           PIC 9(2)  VALUE ZERO.
007170         07 ��×��T�T�v�q             PIC 9(5)  VALUE ZERO.
007180      05 ��㪖@�T�T�v�q.
007190         07 ��㪖@�񐔂T�T�v�q         PIC 9(2)  VALUE ZERO.
007200         07 ��㪖@���T�T�v�q           PIC 9(4)  VALUE ZERO.
007210      05 ��㪖@�T�T�v�q.
007220         07 ��㪖@�񐔂T�T�v�q         PIC 9(2)  VALUE ZERO.
007230         07 ��㪖@���T�T�v�q           PIC 9(4)  VALUE ZERO.
007240      05 �d�ÂT�T�v�q.
007250         07 �d�É񐔂T�T�v�q           PIC 9(2)  VALUE ZERO.
007260         07 �d�×��T�T�v�q             PIC 9(4)  VALUE ZERO.
007270      05 ���v�T�T�v�q                  PIC 9(6)  VALUE ZERO.
007280      05 �����ʍ����v�T�T�v�q          PIC 9(6)  VALUE ZERO.
007290      05 �����������T�T�v�q            PIC 9(3)  VALUE ZERO.
007300      05 ���������v�T�T�v�q            PIC 9(6)  VALUE ZERO.
007310****************
007320* �T���ʁ^�W�� *
007330****************
007340   03 ���ʂT�W�v�q.
007350      05 �����J�n�����T�W�v�q.
007360         07 �����J�n���T�W�v�q         PIC 9(2)  VALUE ZERO.
007370         07 �����J�n���T�W�v�q         PIC 9(2)  VALUE ZERO.
007380      05 ��ÂT�W�v�q.
007390         07 ��ÒP���T�W�v�q           PIC 9(4)  VALUE ZERO.
007400         07 ��É񐔂T�W�v�q           PIC 9(2)  VALUE ZERO.
007410         07 ��×��T�W�v�q             PIC 9(5)  VALUE ZERO.
007420      05 ��㪖@�T�W�v�q.
007430         07 ��㪖@�񐔂T�W�v�q         PIC 9(2)  VALUE ZERO.
007440         07 ��㪖@���T�W�v�q           PIC 9(4)  VALUE ZERO.
007450      05 ��㪖@�T�W�v�q.
007460         07 ��㪖@�񐔂T�W�v�q         PIC 9(2)  VALUE ZERO.
007470         07 ��㪖@���T�W�v�q           PIC 9(4)  VALUE ZERO.
007480      05 �d�ÂT�W�v�q.
007490         07 �d�É񐔂T�W�v�q           PIC 9(2)  VALUE ZERO.
007500         07 �d�×��T�W�v�q             PIC 9(4)  VALUE ZERO.
007510      05 ���v�T�W�v�q                  PIC 9(6)  VALUE ZERO.
007520      05 �����ʍ����v�T�W�v�q          PIC 9(6)  VALUE ZERO.
007530      05 �����������T�W�v�q            PIC 9(3)  VALUE ZERO.
007540      05 ���������v�T�W�v�q            PIC 9(6)  VALUE ZERO.
007550******************
007560* �T���ʁ^�P�O�� *
007570******************
007580   03 ���ʂT�O�v�q.
007590      05 �����J�n�����T�O�v�q.
007600         07 �����J�n���T�O�v�q         PIC 9(2)  VALUE ZERO.
007610         07 �����J�n���T�O�v�q         PIC 9(2)  VALUE ZERO.
007620      05 ��ÂT�O�v�q.
007630         07 ��ÒP���T�O�v�q           PIC 9(4)  VALUE ZERO.
007640         07 ��É񐔂T�O�v�q           PIC 9(2)  VALUE ZERO.
007650         07 ��×��T�O�v�q             PIC 9(5)  VALUE ZERO.
007660      05 ��㪖@�T�O�v�q.
007670         07 ��㪖@�񐔂T�O�v�q         PIC 9(2)  VALUE ZERO.
007680         07 ��㪖@���T�O�v�q           PIC 9(4)  VALUE ZERO.
007690      05 ��㪖@�T�O�v�q.
007700         07 ��㪖@�񐔂T�O�v�q         PIC 9(2)  VALUE ZERO.
007710         07 ��㪖@���T�O�v�q           PIC 9(4)  VALUE ZERO.
007720      05 �d�ÂT�O�v�q.
007730         07 �d�É񐔂T�O�v�q           PIC 9(2)  VALUE ZERO.
007740         07 �d�×��T�O�v�q             PIC 9(4)  VALUE ZERO.
007750      05 ���v�T�O�v�q                  PIC 9(6)  VALUE ZERO.
007760      05 �����������T�O�v�q            PIC 9(3)  VALUE ZERO.
007770      05 ���������v�T�O�v�q            PIC 9(6)  VALUE ZERO.
007780*
007790**************
007800* �{�p����� *
007810**************
007820 01 �{�p�����v.
007830    03 �_���t�ԍ��v                    PIC X(22) VALUE SPACE.
007840    03 �ڍ��t�����ԍ��v              PIC X(10) VALUE SPACE.
007850    03 ��\�҃J�i�v                    PIC X(50) VALUE SPACE.
007860    03 ��\�Җ��v.
007870       05 �����\�Җ��v               PIC X(50) VALUE SPACE.
007880    03 �ڍ��@���v                      PIC X(50) VALUE SPACE.
          03 �s���{���i�h�r�v                PIC X(2)   VALUE SPACE.
007890    03 �{�p���Z���v.
007900       05 �{�p���Z���P�v               PIC X(50) VALUE SPACE.
007910       05 �{�p���Z���Q�v               PIC X(50) VALUE SPACE.
007920    03 �{�p���X�֔ԍ��v.
007930       05 �{�p���X�֔ԍ��P�v           PIC X(3)  VALUE SPACE.
007940       05 �{�p���X�֔ԍ��Q�v           PIC X(4)  VALUE SPACE.
007950    03 �{�p���d�b�ԍ��v                PIC X(15) VALUE SPACE.
007960    03 �ڍ��t�����v.
007970       05 ����ڍ��t�����v         PIC N(7)  VALUE SPACE.
007980       05 FILLER                       PIC N(3)  VALUE SPACE.
007990    03 ��z���󗝔ԍ��v                PIC X(15) VALUE SPACE.
008000    03 �_���t�N�����v.
008010       05 �_���t�N�v                   PIC 9(2)  VALUE ZERO.
008020       05 �_���t���v                   PIC 9(2)  VALUE ZERO.
008030       05 �_���t���v                   PIC 9(2)  VALUE ZERO.
008040    03 ���҈ϔC�N�����v.
008050       05 ���҈ϔC�N�v                 PIC 9(2)  VALUE ZERO.
008060       05 ���҈ϔC���v                 PIC 9(2)  VALUE ZERO.
008070       05 ���҈ϔC���v                 PIC 9(2)  VALUE ZERO.
008080    03 �������v.
008090        05 ������s���v              PIC X(40) VALUE SPACE.
008100        05 ������s�x�X���v          PIC X(40) VALUE SPACE.
008110        05 �a����ʂv                  PIC 9(1)  VALUE ZERO.
008120        05 ��s�ԍ��v                  PIC X(4)  VALUE ZERO.
008130        05 �X�ԍ��v                    PIC X(3)  VALUE ZERO.
008140        05 �����ԍ��v                  PIC X(10) VALUE SPACE.
008150        05 �������`�l�v                PIC X(40) VALUE SPACE.
008160        05 �������`�l�J�i�v            PIC X(40) VALUE SPACE.
008170        05 ��s���x�X���v              PIC X(60) VALUE SPACE.
008180        05 �a����ʖ��̂v              PIC X(4)  VALUE SPACE.
008190        05 �a����ʃR�����g�v          PIC X(15) VALUE SPACE.
008200    03 ���{�p�h�c�v                    PIC X(15) VALUE SPACE.
008210    03 �s�����{�p�h�c�v                PIC X(15) VALUE SPACE.
008220    03 �R�����g�v.
008230        05 �R�����g�P�v                PIC X(40) VALUE SPACE.
008240        05 �R�����g�Q�v                PIC X(40) VALUE SPACE.
008250        05 �R�����g�R�v                PIC X(40) VALUE SPACE.
008260        05 �R�����g�S�v                PIC X(40) VALUE SPACE.
008270        05 �R�����g�T�v                PIC X(40) VALUE SPACE.
008280        05 �R�����g�U�v                PIC X(40) VALUE SPACE.
008290        05 �R�����g�V�v                PIC X(40) VALUE SPACE.
007330    03 ���ϔԍ��v                      PIC X(28) VALUE SPACE.
008300**************
008310* ��f�ҏ�� *
008320**************
008330 01 ��f�ҏ��v.
008340    03 ���Ҕԍ��v                      PIC 9(6)  VALUE ZERO.
008350    03 �{�p�N���v.
008360       05 �{�p�N�v                     PIC 9(2)  VALUE ZERO.
008370       05 �{�p���v                     PIC 9(2)  VALUE ZERO.
008380*    03 �L���v                          PIC N(12) VALUE SPACE.
007570    03 �L���v.
007580       05 ����L���v                   PIC N(12)  VALUE SPACE.
          03 �L���ԍ��v.
             05 �L���ԍ��w�v                 PIC X(40) VALUE SPACE.
008390    03 �ԍ��v.
008400       05 ����ԍ��v                   PIC X(20) VALUE SPACE.
008410       05 FILLER                       PIC X(10) VALUE SPACE.
008420    03 �ی��Ҕԍ��v.
008430       05 ����ی��Ҕԍ��v             PIC X(8)  VALUE SPACE.
008440       05 FILLER                       PIC X(2)  VALUE SPACE.
008450*
008460    03 �����於�̂v.
008470       05 ��������於�̂P�v           PIC X(40) VALUE SPACE.
008480       05 ��������於�̂Q�v           PIC X(40) VALUE SPACE.
008490*
008500    03 �ی���ʂv                      PIC 9(2)  VALUE ZERO.
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
008510    03 �ی���ʐe�v                    PIC N(2)  VALUE SPACE.
008520    03 �ی���ʕҏW�v                  PIC N(5)  VALUE SPACE.
008530    03 ��ی��ҏ��v.
008540       05 ��ی��҃J�i�v               PIC X(50) VALUE SPACE.
008550       05 ��ی��Ҏ����v               PIC X(50) VALUE SPACE.
008560       05 �X�֔ԍ��v.
008570          07 �X�֔ԍ��P�v              PIC X(3)  VALUE SPACE.
008580          07 �X�֔ԍ��Q�v              PIC X(4)  VALUE SPACE.
008590       05 ��ی��ҏZ���v.
008600          07 ��ی��ҏZ���P�v          PIC X(50) VALUE SPACE.
008610          07 ��ی��ҏZ���Q�v          PIC X(50) VALUE SPACE.
008990       05 �d�b�ԍ��v                   PIC X(35)  VALUE SPACE.
008620    03 ���ҏ��v.
008630       05 ���҃J�i�v                   PIC X(50) VALUE SPACE.
008640       05 ���Ҏ����v                   PIC X(50) VALUE SPACE.
008650       05 ���ʃ`�F�b�N�v.
008660          07 �j�`�F�b�N�v              PIC N(1)  VALUE SPACE.
008670          07 ���`�F�b�N�v              PIC N(1)  VALUE SPACE.
008680          07 ���ʂv                    PIC N(2)  VALUE SPACE.
008690       05 �a��`�F�b�N�v.
008700          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008710          07 �吳�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008720          07 ���a�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008730          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008740          07 �����v                    PIC N(2)  VALUE SPACE.
008750       05 ���ҔN�v                     PIC 9(2)  VALUE ZERO.
008760       05 ���Ҍ��v                     PIC 9(2)  VALUE ZERO.
008770       05 ���ғ��v                     PIC 9(2)  VALUE ZERO.
008780       05 �����v.
008790          07 ��������v                PIC N(4)  VALUE SPACE.
008800          07 FILLER                    PIC X(4)  VALUE SPACE.
008810*       05 �����`�F�b�N�v.
008820*          07 �{�l�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008830*          07 �Ƒ��`�F�b�N�v            PIC N(1)  VALUE SPACE.
008840*
008850*       05 ���������v                   PIC N(40) OCCURS 29 VALUE SPACE.
      */���p�Ή�/110421
             05 ���������v OCCURS 29.
                07 ���������w�v              PIC X(80)  VALUE SPACE.
008860*
008870    03 ���ʋ敪�`�F�b�N�v.
008880       05 �V�O�Έȏ�`�F�b�N�v         PIC N(1)  VALUE SPACE.
008890       05 ���A�w�`�F�b�N�v             PIC N(1)  VALUE SPACE.
008900       05 ������v                   PIC X(1)  VALUE SPACE.
008910*
008920    03 �ی���ʃ`�F�b�N�v.
008930       05 �ރ`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008940       05 �����`�F�b�N�v               PIC N(1)  VALUE SPACE.
008950*
008960    03 ������ʂv.
008970       05 ������v                     PIC N(1)  VALUE SPACE.
008980       05 ������`�F�b�N�v             PIC N(1)  VALUE SPACE.
008990       05 �����ԍ��v                   PIC X(2)  VALUE SPACE.
009000*
009010*    03 ���t�����`�F�b�N�v.
009020*       05 �V���`�F�b�N�v               PIC N(1)  VALUE SPACE.
009030*       05 �W���`�F�b�N�v               PIC N(1)  VALUE SPACE.
009040*       05 �X���`�F�b�N�v               PIC N(1)  VALUE SPACE.
009050*       05 �P�O���`�F�b�N�v             PIC N(1)  VALUE SPACE.
009060*
009070    03 ���ʃR�����g�v                  PIC X(16) VALUE SPACE.
          03 �Ђv                            PIC N(1)  VALUE SPACE.
          03 �Њۈ�v                        PIC N(1)  VALUE SPACE.
009080*
009090****************
009100* �����f�[�^�e *
009110****************
009120 01 �������v.
009130    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
009140    03 ���ʏ��v  OCCURS   9.
009150       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
009160       05 ���ʃR�[�h�v.
009170          07 ������ʂv                PIC 9(2)  VALUE ZERO.
009180          07 ���ʂv                    PIC 9(2)  VALUE ZERO.
009190          07 ���E�敪�v                PIC 9(1)  VALUE ZERO.
009200          07 �����ʒu�ԍ��v            PIC 9(2)  VALUE ZERO.
009210       05 �������v                     PIC N(18) VALUE SPACE.
009220       05 �����N�����v.
009230          07 �����N�v                  PIC 9(2)  VALUE ZERO.
009240          07 �������v                  PIC 9(2)  VALUE ZERO.
009250          07 �������v                  PIC 9(2)  VALUE ZERO.
009260       05 �����N�����v.
009270          07 �����N�v                  PIC 9(2)  VALUE ZERO.
009280          07 �������v                  PIC 9(2)  VALUE ZERO.
009290          07 �������v                  PIC 9(2)  VALUE ZERO.
009300       05 �J�n�N�����v.
009310          07 �J�n�N�v                  PIC 9(2)  VALUE ZERO.
009320          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
009330          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
009340       05 �I���N�����v.
009350          07 �I���N�v                  PIC 9(2)  VALUE ZERO.
009360          07 �I�����v                  PIC 9(2)  VALUE ZERO.
009370          07 �I�����v                  PIC 9(2)  VALUE ZERO.
009380       05 �������v                     PIC 9(2)  VALUE ZERO.
009390       05 �]�A�敪�v                   PIC 9(1)  VALUE ZERO.
009400       05 �]�A�敪�`�F�b�N�v.
009410          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
009420          07 ���~�`�F�b�N�v            PIC N(1)  VALUE SPACE.
009430          07 �]��`�F�b�N�v            PIC N(1)  VALUE SPACE.
009440       05 �J�n�N�����擾�t���O         PIC X(3)  VALUE SPACE.
009450       05 ���ʋ�؂v                   PIC X(1)  VALUE SPACE.
009460       05 �o�ߗ��̂v.
009470          07 ����o�ߗ��̂v            PIC N(5)  VALUE SPACE.
009480          07 FILLER                    PIC X(2)  VALUE SPACE.
009490    03 �V�K�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
009500    03 �p���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
          03 �{�p���v.
             05 �{�p���`�F�b�N�v   OCCURS 31 PIC N(1)  VALUE SPACE.
009510*
009520************
009530* ������� *
009540************
009550 01 �������v.
009560    03 �������Z�v.
009570       05 ���ԊO�`�F�b�N�v             PIC N(1)  VALUE SPACE.
009580       05 �x���`�F�b�N�v               PIC N(1)  VALUE SPACE.
009590       05 �[��`�F�b�N�v               PIC N(1)  VALUE SPACE.
009600    03 ���É��Z�v.
009610       05 ��ԃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
009620       05 ��H�`�F�b�N�v               PIC N(1)  VALUE SPACE.
009630       05 �\���J��`�F�b�N�v           PIC N(1)  VALUE SPACE.
009640    03 �������q�`�F�b�N�v.
009650       05 ��`�F�b�N�v                 PIC N(1)  VALUE SPACE.
009660       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
009670       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
009680    03 ���v�v                          PIC 9(7)  VALUE ZERO.
009690    03 ���񏈒u�����v�v                PIC 9(6)  VALUE ZERO.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
          03 �����񐔂v                         PIC 9(2)  VALUE ZERO.
          03 �^�����v                           PIC 9(4)  VALUE ZERO.
009700************
009710* ���l��� *
009720************
009730 01 ���l���v.
009740    03 �K�p�P�v                        PIC N(38) VALUE SPACE.
009750    03 �K�p�Q�v                        PIC N(38) VALUE SPACE.
009760    03 �o�߃R�����g�v                  PIC N(60) VALUE SPACE.
009770*
009780***************************
009790** ���Z�E�v�p( N(38)�Œ�j*
009800***************************
009810 01 �����̌o�߂v.
009820    03 �����̌o�ߍs�v                  PIC X(76) OCCURS 2 VALUE SPACE.
009830 01 �����̌o�߂m�v REDEFINES �����̌o�߂v.
009840    03 �����̌o�ߍs�m�v                PIC N(38) OCCURS 2.
009850*
       01 �E�v�{�p���v                       PIC X(100) VALUE SPACE.
       01 �{�p���v.
          03 �{�p���Q�v                      PIC X(1)  VALUE SPACE.
          03 �{�p���P�v                      PIC X(1)  VALUE SPACE.
004460* ���Z�v�g���я� *
004470 01 ���Ԃv                             PIC 9(4) VALUE ZERO.
004480*
009860*************************************************************************
009870 01 �������.
009880     03 ��`�̖��o                     PIC X(8)  VALUE SPACE.
009890     03 ���ڌQ���o                     PIC X(8)  VALUE SPACE.
009900     03 ������ʂo                     PIC X(2)  VALUE SPACE.
009910     03 �g������o.
009920         05 �[������o.
009930             07 �ړ������o             PIC X(1)  VALUE SPACE.
009940             07 �ړ��s���o             PIC 9(3)  VALUE ZERO.
009950         05 �ڍא���o                 PIC X(2)  VALUE SPACE.
009960     03 �ʒm���o                     PIC X(2)  VALUE SPACE.
009970     03 ���j�b�g���o                   PIC X(8)  VALUE SPACE.
009980*
009990 01 �v�Z�@����N�v                     PIC 9(2)  VALUE ZERO.
010000* ���t�v�n�q�j
010010 01 �a��I���N�v                       PIC 9(4)  VALUE ZERO.
010020 01 �v�Z�@����.
010030    03 �v�Z�@����N                    PIC 9(4)  VALUE ZERO.
010040    03 �v�Z�@�����                  PIC 9(4)  VALUE ZERO.
010050 01 �v�Z�@����q REDEFINES �v�Z�@����.
010060    03 �v�Z�@���I                      PIC 9(2).
010070    03 �v�Z�@���t                      PIC 9(6).
010080    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
010090       05 �v�Z�@�N��                   PIC 9(4).
010100       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
010110         07 �v�Z�@�N                   PIC 9(2).
010120         07 �v�Z�@��                   PIC 9(2).
010130       05 �v�Z�@��                     PIC 9(2).
010140*
      * C �A�g�p
       01  �����P�v        PIC X(4096).
       01  �����Q�v        PIC X(512).
       01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
      *
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
010150******************************************************************
010160*                          �A������                              *
010170******************************************************************
010180**  ��ʓ��̓f�[�^
010190 01 �A���|���̓f�[�^�ϔC��� IS EXTERNAL.
010200    03 �A���|�ϔC���                  PIC 9.
       01 �A���|���̓f�[�^�d�b��� IS EXTERNAL.
          03 �A���|�d�b���                     PIC 9.
010210*
       01 �A���|�v���r���[ IS EXTERNAL.
          03 �A���|�v���r���[�敪          PIC 9.
010300*
010220******************
010230* �R�J���������� *
010240******************
010250 01 �A���ԁ|�L�[ IS EXTERNAL.
010260    03 �A���ԁ|�{�p�N��.
010270       05 �A���ԁ|�{�p�a��             PIC 9.
010280       05 �A���ԁ|�{�p�N               PIC 9(2).
010290       05 �A���ԁ|�{�p��               PIC 9(2).
010300    03  �A���ԁ|���҃R�[�h.
010310       05 �A���ԁ|���Ҕԍ�             PIC 9(6).
010320       05 �A���ԁ|�}��                 PIC X.
010330    03 �A���ԁ|�Ώۃt���O              PIC X(3).
010340    03 �A���ԁ|���Ԍ��v.
010350       05 �A���ԁ|���Ԃv               PIC 9(2) OCCURS 9.
010360*
010370************
010380* ����L�[ *
010390************
010400*
010410*
010420 01 �A����|�Ώۃf�[�^ IS EXTERNAL.
010430    03 �A����|�{�p�N����.
010440       05 �A����|�{�p�a��             PIC 9(1).
010450       05 �A����|�{�p�N               PIC 9(2).
010460       05 �A����|�{�p��               PIC 9(2).
010470    03 �A����|���҃R�[�h.
010480       05 �A����|���Ҕԍ�             PIC 9(6).
010490       05 �A����|�}��                 PIC X(1).
010500    03 �A����|�ی����                PIC 9(2).
010510    03 �A����|�ی��Ҕԍ�              PIC X(10).
010520    03 �A����|������                PIC 9(2).
010530    03 �A����|��p���S�Ҕԍ�          PIC X(10).
010540    03 �A����|�������                PIC 9(2).
010550    03 �A����|��p���S�Ҕԍ�����      PIC X(10).
010560    03 �A����|���҃J�i                PIC X(20).
010570    03 �A����|�{�l�Ƒ��敪            PIC 9(1).
014020*
014030 01 �A���|�L�[ IS EXTERNAL.
014040    03 �A���|�ی����                  PIC 9(2).
014050*
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
014060************************
014070* �������R���Z�b�g     *
014080************************
014090 01 �A�����|�L�[ IS EXTERNAL.
014100    03 �A�����|�{�p�N��.
014110       05 �A�����|�{�p�a��             PIC 9.
014120       05 �A�����|�{�p�N               PIC 9(2).
014130       05 �A�����|�{�p��               PIC 9(2).
014140    03  �A�����|���҃R�[�h.
014150       05 �A�����|���Ҕԍ�             PIC 9(6).
014160       05 �A�����|�}��                 PIC X.
014170    03 �A�����|������                  PIC 9(2).
014180    03 �A�����|���R��                  PIC N(63) OCCURS 15.
014190*
007670* ���S���擾�p14/10�`
007680 01 �A���|���S���擾�L�[ IS EXTERNAL.
007690    03 �A���|�{�p�a��N��.
007700       05 �A���|�{�p�a��               PIC 9.
007710       05 �A���|�{�p�N��.
007720          07 �A���|�{�p�N              PIC 9(2).
007730          07 �A���|�{�p��              PIC 9(2).
007740    03 �A���|���҃R�[�h.
007750       05 �A���|���Ҕԍ�               PIC 9(6).
007760       05 �A���|�}��                   PIC X.
007770    03 �A���|���ە��S��                PIC 9(3).
007780    03 �A���|���ۖ{�̕��S��            PIC 9(3).
007790    03 �A���|���ە��S��                PIC 9(3).
007800    03 �A���|�Q�V�V���S��              PIC 9(3).
007810    03 �A���|�������S��                PIC 9(3).
007820    03 �A���|���ʗp���S��              PIC 9(3).
007100*
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
014200******************************************************************
014210*                      PROCEDURE  DIVISION                       *
014220******************************************************************
014230 PROCEDURE               DIVISION.
014240************
014250*           *
014260* ��������   *
014270*           *
014280************
002570     PERFORM �v�����^�t�@�C���쐬.
014290     PERFORM ������.
014300************
014310*           *
014320* �又��     *
014330*           *
014340************
014350* ���
014360     PERFORM �A�����ڑҔ�.
014370     PERFORM ����Z�b�g.
014380     PERFORM �������.
014390************
014400*           *
014410* �I������   *
014420*           *
014430************
014440     PERFORM ��f�҈���敪�X�V.
014450     PERFORM �I������.
014460     MOVE ZERO  TO PROGRAM-STATUS.
014470     EXIT PROGRAM.
014480*
014490*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
014500*=== �������� ===================================================*
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
002974     MOVE "YCB6125"             TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪  TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014510*================================================================*
014520 ������ SECTION.
014530*================================================================*
014540     PERFORM �t�@�C���I�[�v��.
014550*    /* ���ݓ��t�擾 */
014560     ACCEPT �v�Z�@���t FROM DATE.
014570*    /* 1980�`2079�N�̊ԂŐݒ� */
014580     IF ( �v�Z�@�N > 80 )
014590        MOVE 19 TO �v�Z�@���I
014600     ELSE
014610        MOVE 20 TO �v�Z�@���I
014620     END-IF.
014630     PERFORM �J�����g�����擾.
014640     PERFORM �a��I���N�擾.
014650     COMPUTE �v�Z�@����N�v = �v�Z�@����N - 1988.
014660*
014670*================================================================*
014680 �t�@�C���I�[�v�� SECTION.
014690*
014700     OPEN INPUT   �����}�X�^
014710         MOVE NC"����" TO �t�@�C����.
014720         PERFORM �I�[�v���`�F�b�N.
014730     OPEN INPUT   ���̃}�X�^
014740         MOVE NC"����" TO �t�@�C����.
014750         PERFORM �I�[�v���`�F�b�N.
007560     OPEN INPUT   ���Z�v�g�e
007570         MOVE NC"���Z" TO �t�@�C����.
007580         PERFORM �I�[�v���`�F�b�N.
014790     OPEN INPUT   �o�߃}�X�^
014800         MOVE NC"�o��" TO �t�@�C����.
014810         PERFORM �I�[�v���`�F�b�N.
014820     OPEN INPUT   ������}�X�^
014830         MOVE NC"������" TO �t�@�C����.
014840         PERFORM �I�[�v���`�F�b�N.
014850     OPEN INPUT   �{�p�����}�X�^
014860         MOVE NC"�{��" TO �t�@�C����.
014870         PERFORM �I�[�v���`�F�b�N.
014880     OPEN INPUT   ����}�X�^.
014890         MOVE NC"����}�X�^" TO �t�@�C����.
014900         PERFORM �I�[�v���`�F�b�N.
014910     OPEN INPUT   �ی��҃}�X�^
014920         MOVE NC"�ی���" TO �t�@�C����.
014930         PERFORM �I�[�v���`�F�b�N.
014970     OPEN INPUT   �h�c�Ǘ��}�X�^
014980         MOVE NC"�h�c" TO �t�@�C����.
014990         PERFORM �I�[�v���`�F�b�N.
015000     OPEN INPUT �s�����}�X�^.
015010         MOVE NC"�s����" TO �t�@�C����.
015020         PERFORM �I�[�v���`�F�b�N.
015030     OPEN INPUT   �{�p�L�^�e.
015040         MOVE NC"�{�L�e" TO �t�@�C����.
015050         PERFORM �I�[�v���`�F�b�N.
015060     OPEN INPUT   �����f�[�^�e.
015070         MOVE NC"����" TO �t�@�C����.
015080         PERFORM �I�[�v���`�F�b�N.
015090     OPEN INPUT   ���������e.
015100         MOVE NC"��������" TO �t�@�C����.
015110         PERFORM �I�[�v���`�F�b�N.
016210     OPEN INPUT ��ƃt�@�C���Q.
016220         MOVE NC"��Q" TO �t�@�C����.
016230         PERFORM �I�[�v���`�F�b�N.
015150*
015160     OPEN I-O   ��f�ҏ��e.
015170         MOVE NC"���" TO �t�@�C����.
015180         PERFORM �I�[�v���`�F�b�N.
015190     OPEN I-O   ����t�@�C��
015200         PERFORM �G���[�����o.
015210*
015220*================================================================*
015230 �I�[�v���`�F�b�N SECTION.
015240*
015250     IF ( ��ԃL�[  NOT =  "00" )
015260        DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
015270        DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
015280        DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015290                                                UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015300        ACCEPT  �L�[���� FROM CONS
015310        PERFORM �t�@�C����
015320        EXIT PROGRAM.
015330*
015340*================================================================*
015350 �J�����g�����擾 SECTION.
015360*
015370     MOVE ZEROS TO ���|����敪.
015380     READ ������}�X�^
015390     NOT INVALID KEY
015400         MOVE ���|�J�����g����         TO �J�����g�����v
015410         MOVE ���|���Z������������敪 TO ������������敪�v
015420         MOVE ���|���Z�������R����敪 TO �������R����敪�v
015430         MOVE ���|���Z�v�g���t�敪     TO ���Z�v�g���t�敪�v
015440         MOVE ���|���Z�v�g���ғ��t�敪 TO ���Z�v�g���ғ��t�敪�v
015450     END-READ.
015460*
015470*================================================================*
015480 �a��I���N�擾 SECTION.
015490*
015500*     DISPLAY NC"�J�����g�����v"  �J�����g�����v UPON MSGBOX.
015510     MOVE �J�����g�����v TO ���|�����敪.
015520     READ �����}�X�^
015530     INVALID KEY
015540         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
015550         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015560                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015570         ACCEPT  �L�[���� FROM CONS
015580         PERFORM �I������
015590         EXIT PROGRAM
015600     NOT INVALID KEY
015610         COMPUTE �O�a��v = �J�����g�����v - 1
015620         MOVE �O�a��v TO ���|�����敪
015630         READ �����}�X�^
015640         INVALID KEY
015650             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
015660             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015670                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015680             ACCEPT  �L�[���� FROM CONS
015690             PERFORM �I������
015700             EXIT PROGRAM
015710         NOT INVALID KEY
015720             MOVE ���|�I������N TO �a��I���N�v
015730         END-READ
015740     END-READ.
015750*
015760*=== �又�� =====================================================*
015770*================================================================*
015780 �A�����ڑҔ� SECTION.
015790*================================================================*
015800     MOVE �A����|�{�p�a��           TO �{�p�a��v�q.
015810     MOVE �A����|�{�p�N             TO �{�p�N�v�q.
015820     MOVE �A����|�{�p��             TO �{�p���v�q.
015830     MOVE �A����|�ی����           TO �ی���ʂv�q.
015840     MOVE �A����|�ی��Ҕԍ�         TO �ی��Ҕԍ��v�q.
015850     MOVE �A����|������           TO �����ʂv�q.
015860     MOVE �A����|��p���S�Ҕԍ�     TO ��p���S�Ҕԍ��v�q.
015870     MOVE �A����|�������           TO ������ʂv�q.
015880     MOVE �A����|��p���S�Ҕԍ����� TO ��p���S�Ҕԍ������v�q.
015890     MOVE �A����|�{�l�Ƒ��敪       TO �{�l�Ƒ��敪�v�q.
015900     MOVE �A����|���҃J�i           TO ���҃J�i�v�q.
015910     MOVE �A����|���Ҕԍ�           TO ���Ҕԍ��v�q.
015920     MOVE �A����|�}��               TO �}�Ԃv�q.
015930*
015940*================================================================*
015950 ����Z�b�g SECTION.
015960*================================================================*
015970     PERFORM ���ڏ�����.
           PERFORM ��{���擾.
015980     PERFORM �{�p�����擾.
015990     PERFORM ��������擾.
016010     PERFORM ��f�ҏ��擾.
016020     PERFORM �����f�[�^�擾.
016030     PERFORM �������擾.
016040     PERFORM �{�p�L�^�擾.
016050***     PERFORM ��������擾.
016070     PERFORM �������Z�����擾.
016080     PERFORM �ϔC�N�����擾.
           PERFORM �{�p���擾.
      */���я����/1105
           PERFORM ���Z�v�g���я��擾.
016090*
016100* / ����}�X�^�E�����f�[�^�e�̈���敪���m�F���擾 /
016791*-----------------------------------------------*
016800     IF ( ������������敪�v  NOT = 1 ) AND ( ���Z������������敪�v NOT = 1 )
016813        IF ( ������������敪�v = 3 OR 4 )
016815           PERFORM ������������Ώ۔��菈��
016817        ELSE
016820           PERFORM ���������擾
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
016150*
015940     IF ( �������R����敪�v NOT = 1 )
               MOVE �������R����敪�v TO �A�E���|�����敪
016210     END-IF.
016220*
016230     PERFORM �{�p�h�c�擾.
016240*     PERFORM ���Z�v�g�񐔎擾.
016250*     PERFORM ���t�����擾.
016260*
016270********************
016280* ��f�ҏ��Z�b�g *
016290********************
016300*     MOVE �񐔂v              TO ��.
016340*     MOVE ���ʃR�����g�v      TO ���ʃR�����g.
016390*     MOVE �ی���ʕҏW�v       TO �ی����.
016400*     MOVE �ی���ʐe�v         TO �ی���ʂQ.
016410*     MOVE ���A�w�`�F�b�N�v     TO ���A�w�`�F�b�N.
016420*     MOVE �V�O�Έȏ�`�F�b�N�v TO �V�O�Έȏ�`�F�b�N.
016430*     MOVE ������v           TO �����.
           MOVE �Ђv               TO ��.
           MOVE �Њۈ�v           TO �Њۈ�.
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
016440     EVALUATE ��|�ی����
016450     WHEN 04
016460         PERFORM ���ϔԍ��Z�b�g
016580*         MOVE ���ϘA�ԍ����v   TO �{�p�h�c�Œ�
016470*         MOVE ���{�p�h�c�v     TO ���{�p�h�c
               STRING ���ϘA�ԍ����v DELIMITED BY SPACE
                      ���{�p�h�c�v   DELIMITED BY SIZE
                 INTO ���ϔԍ��v
               END-STRING
               MOVE ���ϔԍ��v       TO ���ϔԍ�
016480     WHEN 09
016490         PERFORM ���q���ԍ��Z�b�g
016580*         MOVE ���q���ԍ����v   TO �{�p�h�c�Œ�
016500*         MOVE ���{�p�h�c�v     TO ���{�p�h�c
               STRING ���q���ԍ����v DELIMITED BY SPACE
                      ���{�p�h�c�v   DELIMITED BY SIZE
                 INTO ���ϔԍ��v
               END-STRING
               MOVE ���ϔԍ��v       TO ���ϔԍ�
016510     WHEN 02
016520     WHEN 03
016530     WHEN 06
016540     WHEN 07
016550         MOVE SPACE            TO ���{�p�h�c
016560     WHEN OTHER
016570         IF ( ���{�p�h�c�v NOT = SPACE )
016580*            MOVE �{�p�h�c�Œ�v   TO �{�p�h�c�Œ�
016590            MOVE ���{�p�h�c�v     TO ���{�p�h�c
016600         END-IF
016610     END-EVALUATE.
016620*
016630     MOVE �{�p�N�v            TO �{�p�N.
016640     MOVE �{�p���v            TO �{�p��.
016650*
016660*     IF ( �L���v(1:1) = NC"��" )
016670*        MOVE  SPACE           TO  �L��
016680*     ELSE
016690*        MOVE �L���v           TO  �L��
016700*     END-IF.
016710*     IF ( ����ԍ��v(1:1) = "*"  ) OR
016720*        ( ����ԍ��v(1:2) = "��" )
016730*        MOVE  SPACE           TO  �ԍ�
016740*     ELSE
016750*        MOVE ����ԍ��v       TO  �ԍ�
016760*     END-IF.
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
                       MOVE "  "   TO �L���ԍ��v(�J�E���^ + 1:2)
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
016770*
016780     MOVE ����ی��Ҕԍ��v    TO �ی��Ҕԍ�.
016790*     MOVE ��������於�̂P�v  TO �����於��.
016800***     MOVE ��������於�̂Q�v  TO �����於�̂Q.
016810***     MOVE ��ی��҃J�i�v      TO ��ی��҃J�i.
016820     MOVE ��ی��Ҏ����v      TO ��ی��Ҏ���.
      */ �X�֔ԍ��E�d�b�ԍ��ǉ� /42505
           IF (�{�p�a��N���v�q >= 42505) AND (�A���|�d�b��� = 1)
              IF (��|�_���X�֓d�b�ԍ���� = 0 OR 2) AND
                 ((�X�֔ԍ��P�v NOT = SPACE) OR (�X�֔ԍ��Q�v NOT = SPACE))
017280*           MOVE "��"          TO �X��
017260           MOVE �X�֔ԍ��P�v  TO �X�֔ԍ��P
017270           MOVE �X�֔ԍ��Q�v  TO �X�֔ԍ��Q
017280           MOVE "-"           TO �X�֔ԍ����
              END-IF
              IF ��|�_���X�֓d�b�ԍ���� = 0 OR 3
017260           MOVE �d�b�ԍ��v    TO �d�b�ԍ�
              END-IF
           END-IF.
016870     MOVE ��ی��ҏZ���P�v    TO �Z���P.
016880     MOVE ��ی��ҏZ���Q�v    TO �Z���Q.
016890***     MOVE ���҃J�i�v          TO ���҃J�i.
016900     MOVE ���Ҏ����v          TO ���Ҏ���.
016910     MOVE �j�`�F�b�N�v        TO �j�`�F�b�N.
016920     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
016930*     MOVE ���ʂv              TO ����.
016940     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
016950     MOVE �吳�`�F�b�N�v      TO �吳�`�F�b�N.
016960     MOVE ���a�`�F�b�N�v      TO ���a�`�F�b�N.
016970     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
016980*     MOVE �����v              TO ����.
016990     MOVE ���ҔN�v            TO ���ҔN.
017000     MOVE ���Ҍ��v            TO ���Ҍ�.
017010     MOVE ���ғ��v            TO ���ғ�.
017020*     MOVE NC"�N"              TO �N.
017030*     MOVE NC"��"              TO ��.
017040*     MOVE NC"��"              TO ��.
017050*     MOVE ��������v          TO ���� �����Q.
017060*
017070***     MOVE �P�O���`�F�b�N�v    TO �P�O���`�F�b�N.
017080***     MOVE �X���`�F�b�N�v      TO �X���`�F�b�N.
017090***     MOVE �W���`�F�b�N�v      TO �W���`�F�b�N.
017100***     MOVE �V���`�F�b�N�v      TO �V���`�F�b�N.
017110*     MOVE ���t�����v          TO ���t����.
017120*
017130     MOVE ���������v(1)       TO ���������P.
017140     MOVE ���������v(2)       TO ���������Q.
017150     MOVE ���������v(3)       TO ���������R.
017150     MOVE ���������v(4)       TO ���������S.
017150     MOVE ���������v(5)       TO ���������T.
017150     MOVE ���������v(6)       TO ���������U.
017330*
017340********************
017350* �����f�[�^�Z�b�g *
017360********************
017370* �P���� *
017380**********
017390     MOVE �������v(1)       TO �������P.
017400     MOVE �����N�v(1)       TO �����N�P.
017410     MOVE �������v(1)       TO �������P.
017420     MOVE �������v(1)       TO �������P.
017430     MOVE �����N�v(1)       TO �����N�P.
017440     MOVE �������v(1)       TO �������P.
017450     MOVE �������v(1)       TO �������P.
017460     MOVE �J�n�N�v(1)       TO �J�n�N�P.
017470     MOVE �J�n���v(1)       TO �J�n���P.
017480     MOVE �J�n���v(1)       TO �J�n���P.
017490     MOVE �I���N�v(1)       TO �I���N�P.
017500     MOVE �I�����v(1)       TO �I�����P.
017510     MOVE �I�����v(1)       TO �I�����P.
017520     MOVE �������v(1)       TO �������P.
017530     MOVE �����`�F�b�N�v(1) TO �����`�F�b�N�P.
017540     MOVE ���~�`�F�b�N�v(1) TO ���~�`�F�b�N�P.
017550     MOVE �]��`�F�b�N�v(1) TO �]��`�F�b�N�P.
017560**********
017570* �Q���� *
017580**********
017590     MOVE �������v(2)       TO �������Q.
017600     MOVE �����N�v(2)       TO �����N�Q.
017610     MOVE �������v(2)       TO �������Q.
017620     MOVE �������v(2)       TO �������Q.
017630     MOVE �����N�v(2)       TO �����N�Q.
017640     MOVE �������v(2)       TO �������Q.
017650     MOVE �������v(2)       TO �������Q.
017660     MOVE �J�n�N�v(2)       TO �J�n�N�Q.
017670     MOVE �J�n���v(2)       TO �J�n���Q.
017680     MOVE �J�n���v(2)       TO �J�n���Q.
017690     MOVE �I���N�v(2)       TO �I���N�Q.
017700     MOVE �I�����v(2)       TO �I�����Q.
017710     MOVE �I�����v(2)       TO �I�����Q.
017720     MOVE �������v(2)       TO �������Q.
017730     MOVE �����`�F�b�N�v(2) TO �����`�F�b�N�Q.
017740     MOVE ���~�`�F�b�N�v(2) TO ���~�`�F�b�N�Q.
017750     MOVE �]��`�F�b�N�v(2) TO �]��`�F�b�N�Q.
017760**********
017770* �R���� *
017780**********
017790     MOVE �������v(3)       TO �������R.
017800     MOVE �����N�v(3)       TO �����N�R.
017810     MOVE �������v(3)       TO �������R.
017820     MOVE �������v(3)       TO �������R.
017830     MOVE �����N�v(3)       TO �����N�R.
017840     MOVE �������v(3)       TO �������R.
017850     MOVE �������v(3)       TO �������R.
017860     MOVE �J�n�N�v(3)       TO �J�n�N�R.
017870     MOVE �J�n���v(3)       TO �J�n���R.
017880     MOVE �J�n���v(3)       TO �J�n���R.
017890     MOVE �I���N�v(3)       TO �I���N�R.
017900     MOVE �I�����v(3)       TO �I�����R.
017910     MOVE �I�����v(3)       TO �I�����R.
017920     MOVE �������v(3)       TO �������R.
017930     MOVE �����`�F�b�N�v(3) TO �����`�F�b�N�R.
017940     MOVE ���~�`�F�b�N�v(3) TO ���~�`�F�b�N�R.
017950     MOVE �]��`�F�b�N�v(3) TO �]��`�F�b�N�R.
017960**********
017970* �S���� *
017980**********
017990     MOVE �������v(4)       TO �������S.
018000     MOVE �����N�v(4)       TO �����N�S.
018010     MOVE �������v(4)       TO �������S.
018020     MOVE �������v(4)       TO �������S.
018030     MOVE �����N�v(4)       TO �����N�S.
018040     MOVE �������v(4)       TO �������S.
018050     MOVE �������v(4)       TO �������S.
018060     MOVE �J�n�N�v(4)       TO �J�n�N�S.
018070     MOVE �J�n���v(4)       TO �J�n���S.
018080     MOVE �J�n���v(4)       TO �J�n���S.
018090     MOVE �I���N�v(4)       TO �I���N�S.
018100     MOVE �I�����v(4)       TO �I�����S.
018110     MOVE �I�����v(4)       TO �I�����S.
018120     MOVE �������v(4)       TO �������S.
018130     MOVE �����`�F�b�N�v(4) TO �����`�F�b�N�S.
018140     MOVE ���~�`�F�b�N�v(4) TO ���~�`�F�b�N�S.
018150     MOVE �]��`�F�b�N�v(4) TO �]��`�F�b�N�S.
018160**********
018170* �T���� *
018180**********
018190     MOVE �������v(5)       TO �������T.
018200     MOVE �����N�v(5)       TO �����N�T.
018210     MOVE �������v(5)       TO �������T.
018220     MOVE �������v(5)       TO �������T.
018230     MOVE �����N�v(5)       TO �����N�T.
018240     MOVE �������v(5)       TO �������T.
018250     MOVE �������v(5)       TO �������T.
018260     MOVE �J�n�N�v(5)       TO �J�n�N�T.
018270     MOVE �J�n���v(5)       TO �J�n���T.
018280     MOVE �J�n���v(5)       TO �J�n���T.
018290     MOVE �I���N�v(5)       TO �I���N�T.
018300     MOVE �I�����v(5)       TO �I�����T.
018310     MOVE �I�����v(5)       TO �I�����T.
018320     MOVE �������v(5)       TO �������T.
018330     MOVE �����`�F�b�N�v(5) TO �����`�F�b�N�T.
018340     MOVE ���~�`�F�b�N�v(5) TO ���~�`�F�b�N�T.
018350     MOVE �]��`�F�b�N�v(5) TO �]��`�F�b�N�T.
018360**************
018370* �o�߃Z�b�g *
018380**************
018390***     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018400***             UNTIL ( ���ʂb�m�s > ���ʐ��v )
018410***             UNTIL ( ���ʂb�m�s > 5 )
018420**         MOVE ���ʂb�m�s�v(���ʂb�m�s)   TO �o�ߕ��ʂb�m�s(���ʂb�m�s)
018430**         MOVE ���ʋ�؂v(���ʂb�m�s)     TO ���ʋ��(���ʂb�m�s)
018440***         MOVE ����o�ߗ��̂v(���ʂb�m�s) TO �o�ߗ���(���ʂb�m�s)
018450***     END-PERFORM.
018460*****************************************
018470*     �V�K�E�p���`�F�b�N�ɂ���        *
018480*   ���V�K...�����L�� ���p��...�����Ȃ� *
018490*****************************************
018500     MOVE �V�K�`�F�b�N�v    TO �V�K�`�F�b�N.
018510     MOVE �p���`�F�b�N�v    TO �p���`�F�b�N.
018520********************
018530* �����f�[�^�Z�b�g *
018540********************
018550*    ****************************************************************
018560*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
018570*    ****************************************************************
018580     MOVE �������v�q                   TO  ������.
018590     MOVE ���k���v�q                   TO  ���������k��.
018600     MOVE ���ԊO�`�F�b�N�v             TO  ���ԊO�`�F�b�N.
018610     MOVE �x���`�F�b�N�v               TO  �x���`�F�b�N.
018620     MOVE �[��`�F�b�N�v               TO  �[��`�F�b�N.
018630     MOVE �������Z���v�q               TO  �������Z��.
           IF (���ԊO�`�F�b�N�v NOT = SPACE) OR (�[��`�F�b�N�v NOT = SPACE) OR
              (�x���`�F�b�N�v NOT = SPACE)
              MOVE �������Z���v                 TO  �������Z��
              MOVE �������Z��؂v               TO  �������Z���
              MOVE �������Z���v                 TO  �������Z��
           END-IF.
018640     MOVE �Č����v�q                   TO  �Č���.
018650     MOVE ���Ë����v�q                 TO  ���Ë���.
018660     MOVE ���É񐔂v�q                 TO  ���É�.
018670     MOVE ���×��v�q                   TO  ���×�.
018680     MOVE ��ԃ`�F�b�N�v               TO  ��ԃ`�F�b�N.
018690     MOVE ��H�`�F�b�N�v               TO  ��H�`�F�b�N.
018700     MOVE �\���J��`�F�b�N�v           TO  �\���J��`�F�b�N.
018710     MOVE ���É��Z���v�q               TO  ���É��Z��.
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
018750     MOVE �������q���Z���v�q           TO  �������q���Z��.
018760     MOVE �{�p���񋟗��v�q           TO  �{�p���񋟗�.
018770     MOVE ���v�v                       TO ���v.
018780********************
018790* ���񏈒u���Z�b�g *
018800********************
018810     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018820***             UNTIL ( ���ʂb�m�s > ���ʐ��v )
018830             UNTIL ( ���ʂb�m�s > 5 )
018840        MOVE ���񏈒u���v�q(���ʂb�m�s) TO ���񏈒u��(���ʂb�m�s)
018850     END-PERFORM.
018860     MOVE ���񏈒u�����v�v             TO ���񏈒u�����v
018870********************
018880* �����������Z�b�g *
018890********************
018900*    **********
018910*    * �P���� *
018920*    **********
018930     MOVE ��ÒP���P�v�q               TO ��ÒP���P.
018940     MOVE ��É񐔂P�v�q               TO ��É񐔂P.
018950     MOVE ��×��P�v�q                 TO ��×��P.
018960     MOVE ��㪖@�񐔂P�v�q             TO ��㪖@�񐔂P.
018970     MOVE ��㪖@���P�v�q               TO ��㪖@���P.
018980     MOVE ��㪖@�񐔂P�v�q             TO ��㪖@�񐔂P.
018990     MOVE ��㪖@���P�v�q               TO ��㪖@���P.
019000     MOVE �d�É񐔂P�v�q               TO �d�É񐔂P.
019010     MOVE �d�×��P�v�q                 TO �d�×��P.
019020     MOVE ���v�P�v�q                   TO ���v�P.
019030     IF ( �����������P�v�q NOT = ZERO )
019040        COMPUTE �����������P = �����������P�v�q / 100
019050     END-IF.
019060     MOVE ���������v�P�v�q             TO ���������v�P.
019070*    **********
019080*    * �Q���� *
019090*    **********
019100     MOVE ��ÒP���Q�v�q               TO ��ÒP���Q.
019110     MOVE ��É񐔂Q�v�q               TO ��É񐔂Q.
019120     MOVE ��×��Q�v�q                 TO ��×��Q.
019130     MOVE ��㪖@�񐔂Q�v�q             TO ��㪖@�񐔂Q.
019140     MOVE ��㪖@���Q�v�q               TO ��㪖@���Q.
019150     MOVE ��㪖@�񐔂Q�v�q             TO ��㪖@�񐔂Q.
019160     MOVE ��㪖@���Q�v�q               TO ��㪖@���Q.
019170     MOVE �d�É񐔂Q�v�q               TO �d�É񐔂Q.
019180     MOVE �d�×��Q�v�q                 TO �d�×��Q.
019190     MOVE ���v�Q�v�q                   TO ���v�Q.
019200     IF ( �����������Q�v�q NOT = ZERO )
019210        COMPUTE �����������Q = �����������Q�v�q / 100
019220     END-IF.
019230     MOVE ���������v�Q�v�q             TO ���������v�Q.
019240*    ****************
019250*    * �R���ʁ^�W�� *
019260*    ****************
019270     MOVE ��ÒP���R�W�v�q             TO ��ÒP���R�W.
019280     MOVE ��É񐔂R�W�v�q             TO ��É񐔂R�W.
019290     MOVE ��×��R�W�v�q               TO ��×��R�W.
019300     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
019310     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
019320     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
019330     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
019340     MOVE �d�É񐔂R�W�v�q             TO �d�É񐔂R�W.
019350     MOVE �d�×��R�W�v�q               TO �d�×��R�W.
019360     MOVE ���v�R�W�v�q                 TO ���v�R�W.
019370     MOVE �����ʍ����v�R�W�v�q         TO �����ʍ����v�R�W.
019380     IF ( �����������R�W�v�q NOT = ZERO )
019390        COMPUTE �����������R�W = �����������R�W�v�q / 100
019400     END-IF.
019410     MOVE ���������v�R�W�v�q           TO ���������v�R�W.
      */ ������ 0.7��0.6 /42505  /*�o���Ȃ� /42610
      *     IF (�{�p�a��N���v�q >= 42505)
      *        MOVE "60"                      TO �����R�W
      *        MOVE "0.6"                     TO �����ʂR�W
      *        MOVE "==="                     TO ���������R�W �����ʒ����R�W
      *     END-IF.
019420*    ****************
019430*    * �R���ʁ^10�� *
019440*    ****************
019450     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
019460     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
019470     MOVE ��ÒP���R�O�v�q             TO ��ÒP���R�O.
019480     MOVE ��É񐔂R�O�v�q             TO ��É񐔂R�O.
019490     MOVE ��×��R�O�v�q               TO ��×��R�O.
019500     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
019510     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
019520     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
019530     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
019540     MOVE �d�É񐔂R�O�v�q             TO �d�É񐔂R�O.
019550     MOVE �d�×��R�O�v�q               TO �d�×��R�O.
019560     MOVE ���v�R�O�v�q                 TO ���v�R�O.
019570     IF ( �����������R�O�v�q NOT = ZERO )
019580        COMPUTE �����������R�O = �����������R�O�v�q / 100
019590     END-IF.
019600     MOVE ���������v�R�O�v�q           TO ���������v�R�O.
019610*    ****************
019620*    * �S���ʁ^�T�� *
019630*    ****************
019640     MOVE ��ÒP���S�T�v�q             TO ��ÒP���S�T.
019650     MOVE ��É񐔂S�T�v�q             TO ��É񐔂S�T.
019660     MOVE ��×��S�T�v�q               TO ��×��S�T.
019670     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
019680     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
019690     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
019700     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
019710     MOVE �d�É񐔂S�T�v�q             TO �d�É񐔂S�T.
019720     MOVE �d�×��S�T�v�q               TO �d�×��S�T.
019730     MOVE ���v�S�T�v�q                 TO ���v�S�T.
019740     MOVE �����ʍ����v�S�T�v�q         TO �����ʍ����v�S�T.
019750     IF ( �����������S�T�v�q NOT = ZERO )
019760        COMPUTE �����������S�T = �����������S�T�v�q / 100
019770     END-IF.
019780     MOVE ���������v�S�T�v�q           TO ���������v�S�T.
019790*    ****************
019800*    * �S���ʁ^�W�� *
019810*    ****************
019820     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
019830     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
019840     MOVE ��ÒP���S�W�v�q             TO ��ÒP���S�W.
019850     MOVE ��É񐔂S�W�v�q             TO ��É񐔂S�W.
019860     MOVE ��×��S�W�v�q               TO ��×��S�W.
019870     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
019880     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
019890     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
019900     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
019910     MOVE �d�É񐔂S�W�v�q             TO �d�É񐔂S�W.
019920     MOVE �d�×��S�W�v�q               TO �d�×��S�W.
019930     MOVE ���v�S�W�v�q                 TO ���v�S�W.
019940     MOVE �����ʍ����v�S�W�v�q         TO �����ʍ����v�S�W.
019950     IF ( �����������S�W�v�q NOT = ZERO )
019960        COMPUTE �����������S�W = �����������S�W�v�q / 100
019970     END-IF.
019980     MOVE ���������v�S�W�v�q           TO ���������v�S�W.
      */ ������ 0.7��0.6 /42505  /*�o���Ȃ� /42610
      *     IF (�{�p�a��N���v�q >= 42505)
      *        MOVE "60"                      TO �����S�W
      *        MOVE "0.6"                     TO �����ʂS�W
      *        MOVE "==="                     TO ���������S�W �����ʒ����S�W
      *     END-IF.
019990*    ****************
020000*    * �S���ʁ^10�� *
020010*    ****************
020020     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
020030     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
020040     MOVE ��ÒP���S�O�v�q             TO ��ÒP���S�O.
020050     MOVE ��É񐔂S�O�v�q             TO ��É񐔂S�O.
020060     MOVE ��×��S�O�v�q               TO ��×��S�O.
020070     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
020080     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
020090     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
020100     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
020110     MOVE �d�É񐔂S�O�v�q             TO �d�É񐔂S�O.
020120     MOVE �d�×��S�O�v�q               TO �d�×��S�O.
020130     MOVE ���v�S�O�v�q                 TO ���v�S�O.
020140     IF ( �����������S�O�v�q NOT = ZERO )
020150        COMPUTE �����������S�O = �����������S�O�v�q / 100
020160     END-IF.
020170     MOVE ���������v�S�O�v�q           TO ���������v�S�O.
020180*
020190*��***********************************************************************
020200* �T���ʁ^2.5���̈󎚂͕K�v�Ȃ��B
020210*------------------------------------------------------------------------*
020220*    *****************
020230*    * �T���ʁ^2.5�� *
020240*    *****************
020250*     MOVE ��ÒP���T�Q�v�q             TO ��ÒP���T�Q.
020260*     MOVE ��É񐔂T�Q�v�q             TO ��É񐔂T�Q.
020270*     MOVE ��×��T�Q�v�q               TO ��×��T�Q.
020280*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
020290*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
020300*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
020310*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
020320*     MOVE �d�É񐔂T�Q�v�q             TO �d�É񐔂T�Q.
020330*     MOVE �d�×��T�Q�v�q               TO �d�×��T�Q.
020340*     MOVE ���v�T�Q�v�q                 TO ���v�T�Q.
020350*     MOVE �����ʍ����v�T�Q�v�q         TO �����ʍ����v�T�Q.
020360*     IF ( �����������T�Q�v�q NOT = ZERO )
020370*        COMPUTE �����������T�Q = �����������T�Q�v�q / 100
020380*     END-IF.
020390*     MOVE ���������v�T�Q�v�q           TO ���������v�T�Q.
020400*��***********************************************************************
020410*
020420*    ****************
020430*    * �T���ʁ^�T�� *
020440*    ****************
020450*     MOVE SPACE TO ���ʂT�v.
020460*     IF ( ���v�T�T�v�q NOT = ZERO )
020470*       MOVE "5) 33 "                  TO �����Œ�T�v
020480*       MOVE "0.33"                    TO �����ʗ��T�v
020490*       MOVE �����J�n���T�T�v�q        TO �����J�n���T�v
020500*       MOVE �����J�n���T�T�v�q        TO �����J�n���T�v
020510*       MOVE ��ÒP���T�T�v�q          TO ��ÒP���T�v
020520*       MOVE ��É񐔂T�T�v�q          TO ��É񐔂T�v
020530*       MOVE ��×��T�T�v�q            TO ��×��T�v
020540*       MOVE ��㪖@�񐔂T�T�v�q        TO ��㪖@�񐔂T�v
020550*       MOVE ��㪖@���T�T�v�q          TO ��㪖@���T�v
020560*       MOVE ��㪖@�񐔂T�T�v�q        TO ��㪖@�񐔂T�v
020570*       MOVE ��㪖@���T�T�v�q          TO ��㪖@���T�v
020580*       MOVE �d�É񐔂T�T�v�q          TO �d�É񐔂T�v
020590*       MOVE �d�×��T�T�v�q            TO �d�×��T�v
020600*       MOVE ���v�T�T�v�q              TO ���v�T�v
020610*       MOVE �����ʍ����v�T�T�v�q      TO �����ʍ����v�T�v
020620*       IF ( �����������T�T�v�q NOT = ZERO )
020630*          COMPUTE �����������T�v = �����������T�T�v�q / 100
020640*       END-IF
020650*       MOVE ���������v�T�T�v�q        TO ���������v�T�v
020660**------------------------------------------------------------------------------------*
020760**       MOVE ���ʂT�v                  TO ���ʂT�T
020770*     END-IF.
020780*    ****************
020790*    * �T���ʁ^�W�� *
020800*    ****************
020810     MOVE SPACE TO ���ʂT�v.
020820     IF ( ���v�T�W�v�q NOT = ZERO )
      */���t
021560         MOVE �����J�n���T�W�v�q           TO �����J�n���T�v
               MOVE "��"                         TO ���b�l
021570         MOVE �����J�n���T�W�v�q           TO �����J�n���T�v
               MOVE "��"                         TO ���b�l
               MOVE "("                          TO ���ʂP�v
      */��×�
               IF ��×��T�W�v�q NOT = ZERO
                   MOVE "("                      TO ���ʂQ�v
021580             MOVE ��ÒP���T�W�v�q         TO ��ÒP���T�v
                   MOVE "x"                      TO ��Z�L���P�v
021590             MOVE ��É񐔂T�W�v�q         TO ��É񐔂T�v
                   MOVE "="                      TO �C�R�[���P�v
021600             MOVE ��×��T�W�v�q           TO ��×��T�v
                   MOVE ")"                      TO ���ʂR�v
               END-IF
      */��㪖@
               IF ��㪖@���T�W�v�q NOT = ZERO
                   MOVE "+"                      TO ���Z�L���P�v
                   MOVE "("                      TO ���ʂS�v
                   COMPUTE ��㪖@�P���T�v        =  ��㪖@���T�W�v�q / ��㪖@�񐔂T�W�v�q
                   MOVE "x"                      TO ��Z�L���Q�v
021610             MOVE ��㪖@�񐔂T�W�v�q       TO ��㪖@�񐔂T�v
                   MOVE "="                      TO �C�R�[���Q�v
021620             MOVE ��㪖@���T�W�v�q         TO ��㪖@���T�v
                   MOVE ")"                      TO ���ʂT�v
               END-IF
      */��㪖@
               IF ��㪖@���T�W�v�q NOT = ZERO
                   MOVE "+"                      TO ���Z�L���Q�v
                   MOVE "("                      TO ���ʂU�v
                   COMPUTE ��㪖@�P���T�v        =  ��㪖@���T�W�v�q / ��㪖@�񐔂T�W�v�q
                   MOVE "x"                      TO ��Z�L���R�v
021630             MOVE ��㪖@�񐔂T�W�v�q       TO ��㪖@�񐔂T�v
                   MOVE "="                      TO �C�R�[���R�v
021640             MOVE ��㪖@���T�W�v�q         TO ��㪖@���T�v
                   MOVE ")"                      TO ���ʂV�v
               END-IF
      */�d�×�
               IF �d�×��T�W�v�q NOT = ZERO
                   MOVE "+"                      TO ���Z�L���R�v
                   MOVE "("                      TO ���ʂW�v
                   COMPUTE �d�ÒP���T�v          =  �d�×��T�W�v�q / �d�É񐔂T�W�v�q
                   MOVE "x"                      TO ��Z�L���S�v
021650             MOVE �d�É񐔂T�W�v�q         TO �d�É񐔂T�v
                   MOVE "="                      TO �C�R�[���S�v
021660             MOVE �d�×��T�W�v�q           TO �d�×��T�v
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
021680         IF �����������T�W�v�q NOT = ZERO
                  MOVE "x"                       TO ��Z�L���U�v
021690            COMPUTE �����������T�v = �����������T�W�v�q / 100
021700         END-IF
      */���v
               MOVE "="                          TO �C�R�[���T�v
021710         MOVE ���������v�T�W�v�q           TO ���������v�T�v
021020         MOVE ���ʂT�v                  TO ���ʂT�W
021030     END-IF.
021040*    ****************
021050*    * �T���ʁ^10�� *
021060*    ****************
021070     MOVE SPACE TO ���ʂT�v.
021080     IF ( ���v�T�O�v�q NOT = ZERO )
      */���t
021560         MOVE �����J�n���T�O�v�q           TO �����J�n���T�v
               MOVE "��"                         TO ���b�l
021570         MOVE �����J�n���T�O�v�q           TO �����J�n���T�v
               MOVE "��"                         TO ���b�l
               MOVE "("                          TO ���ʂP�v
      */��×�
               IF ��×��T�O�v�q NOT = ZERO
                   MOVE "("                      TO ���ʂQ�v
021580             MOVE ��ÒP���T�O�v�q         TO ��ÒP���T�v
                   MOVE "x"                      TO ��Z�L���P�v
021590             MOVE ��É񐔂T�O�v�q         TO ��É񐔂T�v
                   MOVE "="                      TO �C�R�[���P�v
021600             MOVE ��×��T�O�v�q           TO ��×��T�v
                   MOVE ")"                      TO ���ʂR�v
               END-IF
      */��㪖@
               IF ��㪖@���T�O�v�q NOT = ZERO
                   MOVE "+"                      TO ���Z�L���P�v
                   MOVE "("                      TO ���ʂS�v
                   COMPUTE ��㪖@�P���T�v        =  ��㪖@���T�O�v�q / ��㪖@�񐔂T�O�v�q
                   MOVE "x"                      TO ��Z�L���Q�v
021610             MOVE ��㪖@�񐔂T�O�v�q       TO ��㪖@�񐔂T�v
                   MOVE "="                      TO �C�R�[���Q�v
021620             MOVE ��㪖@���T�O�v�q         TO ��㪖@���T�v
                   MOVE ")"                      TO ���ʂT�v
               END-IF
      */��㪖@
               IF ��㪖@���T�O�v�q NOT = ZERO
                   MOVE "+"                      TO ���Z�L���Q�v
                   MOVE "("                      TO ���ʂU�v
                   COMPUTE ��㪖@�P���T�v        =  ��㪖@���T�O�v�q / ��㪖@�񐔂T�O�v�q
                   MOVE "x"                      TO ��Z�L���R�v
021630             MOVE ��㪖@�񐔂T�O�v�q       TO ��㪖@�񐔂T�v
                   MOVE "="                      TO �C�R�[���R�v
021640             MOVE ��㪖@���T�O�v�q         TO ��㪖@���T�v
                   MOVE ")"                      TO ���ʂV�v
               END-IF
      */�d�×�
               IF �d�×��T�O�v�q NOT = ZERO
                   MOVE "+"                      TO ���Z�L���R�v
                   MOVE "("                      TO ���ʂW�v
                   COMPUTE �d�ÒP���T�v          =  �d�×��T�O�v�q / �d�É񐔂T�O�v�q
                   MOVE "x"                      TO ��Z�L���S�v
021650             MOVE �d�É񐔂T�O�v�q         TO �d�É񐔂T�v
                   MOVE "="                      TO �C�R�[���S�v
021660             MOVE �d�×��T�O�v�q           TO �d�×��T�v
                   MOVE ")"                      TO ���ʂX�v
               END-IF
      *
               MOVE ")"                          TO ���ʂP�O�v
      */������
      *        ��Z�L���T�v �����ʗ��T�v
      */����
021680         IF �����������T�O�v�q NOT = ZERO
                  MOVE "x"                       TO ��Z�L���U�v
021690            COMPUTE �����������T�v = �����������T�O�v�q / 100
021700         END-IF
      */���v
               MOVE "="                          TO �C�R�[���T�v
021710         MOVE ���������v�T�O�v�q           TO ���������v�T�v
021260         MOVE ���ʂT�v                  TO ���ʂT�O
021270     END-IF.
021280*
021290     MOVE �K�p�P�v                     TO �K�p�P.
021300     MOVE �K�p�Q�v                     TO �K�p�Q.
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
              MOVE 27           TO �A���^�|��R�[�h
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
021310     MOVE ���Z�|���v                   TO ���v.
021320     MOVE ���Z�|�ꕔ���S��             TO �ꕔ���S��.
021330     MOVE ���Z�|�������z               TO �������z.
021340*
021350*------------------------------------------------------------------------------------*
021360* ���ʁi�������Z�Ȃ��ŁA�{�̃��Z�ɂ܂Ƃ߂鎞�A���z�͏������݁E�K�p�Q�ɏ�����ʈ󎚁j
021370     IF ( �������Z�܂Ƃ߃t���O = "YES" )
021390        MOVE ���Z�|���v                TO ���v
021400        MOVE ���Z�|�󋋎ҕ��S�z        TO �ꕔ���S��
021410*     / �����Z����/
021420        COMPUTE �������z = ���Z�|���v - ���Z�|�󋋎ҕ��S�z
      */���͌����̕�q�͕��S�z�������L�ڂ���/130418
      */���Ԏs�̏�Q�͕��S�z�������L�ڂ���/
      */���l���̓����͕��S�z�������L�ڂ���/170217
               IF ((��|������� = 52) AND (��|��p���S�Ҕԍ����� = "85140630")) OR
                  ((��|������� = 53) AND (��|��p���S�Ҕԍ����� = "80140171")) OR
                  ((��|������� = 55) AND (��|��p���S�Ҕԍ�����(1:5) = "81144" OR "81145"))
019830             MOVE ���Z�|�ꕔ���S�� TO �ꕔ���S��
019840             MOVE ���Z�|�������z   TO �������z
               END-IF
021430*
021440***        MOVE NC"��"  TO �P�O���`�F�b�N
021450***        MOVE SPACE   TO �X���`�F�b�N �W���`�F�b�N �V���`�F�b�N
021460*        MOVE 10      TO ���t����
021470*
021480*/�[�Q��̋󔒂ɃX�g�����O���Ă��܂�����NOT SPACE�̎��͍Ō�ɓ]�L����B
021490*/�������Z���R��̎��͗]�������]�L�����B
021500        IF ������ʗ��̂v NOT = SPACE
021510           IF ( �K�p�Q�v NOT = SPACE )
021520              MOVE SPACE TO ������ʗ��̂v�Q
021530              STRING NC"��"             DELIMITED BY SIZE
021540                     ������ʗ��̂v     DELIMITED BY SPACE
021550                     INTO ������ʗ��̂v�Q
021560              END-STRING
021570              MOVE ������ʗ��̂v�Q       TO �K�p�Q(35:4)
021580           ELSE
021590              STRING �K�p�Q�v           DELIMITED BY SPACE
021600                     NC"��"             DELIMITED BY SIZE
021610                     ������ʗ��̂v     DELIMITED BY SPACE
021620                     INTO �K�p�Q
021630              END-STRING
021640           END-IF
021650        END-IF
021660     END-IF.
021860*
021870**********************
021880* �{�p���f�[�^�Z�b�g *
021890**********************
           MOVE �s���{���i�h�r�v       TO �s���{���ԍ�.
021900     MOVE �_���t�ԍ��v           TO �_���t�ԍ�.
021910*     MOVE ��z���󗝔ԍ��v       TO ��z���󗝔ԍ�.
021920     MOVE �{�p���X�֔ԍ��P�v     TO �{�p���X�֔ԍ��P.
021930     MOVE �{�p���X�֔ԍ��Q�v     TO �{�p���X�֔ԍ��Q.
021940*     MOVE �{�p���Z���v           TO �{�p���Z���P.
021950     MOVE �{�p���Z���P�v         TO �{�p���Z���P.
021960     MOVE �{�p���Z���Q�v         TO �{�p���Z���Q.
      */�����Q�V�N�P�O���{�p��������ԍ������/150922
021970     MOVE �ڍ��t�����ԍ��v     TO �ڍ��t�����ԍ�.
021980     MOVE �ڍ��@���v             TO �ڍ��@��.
021990     MOVE ��\�҃J�i�v           TO ��\�҃J�i.
022000     MOVE ��\�Җ��v             TO ��\�Җ�.
022010     MOVE �{�p���d�b�ԍ��v       TO �{�p���d�b�ԍ�.
022020*
022030* / �_���t�E���҈ϔC�� /
022040     MOVE �_���t�N�v             TO �󗝔N.
022050     MOVE �_���t���v             TO �󗝌�.
022060     MOVE �_���t���v             TO �󗝓�.
022070* ( �ϔC�N���� ������邩 )
022080     IF ( �A���|�ϔC���  = ZERO )
022090        MOVE ���҈ϔC�N�v        TO �ϔC�N
022100        MOVE ���҈ϔC���v        TO �ϔC��
022110        MOVE ���҈ϔC���v        TO �ϔC��
022120     END-IF.
022130*
022140***     MOVE �R�����g�P�v           TO �R�����g�P.
022150***     MOVE �R�����g�Q�v           TO �R�����g�Q.
022160***     MOVE �R�����g�R�v           TO �R�����g�R.
022170***     MOVE �R�����g�S�v           TO �R�����g�S.
022180***     MOVE �R�����g�T�v           TO �R�����g�T.
022190***     MOVE �R�����g�U�v           TO �R�����g�U.
022200***     MOVE �R�����g�V�v           TO �R�����g�V.
022210*
022220***     MOVE ��s���x�X���v         TO ��s���x�X��.
022230***     MOVE �a����ʃR�����g�v     TO �a�����.
022240***     MOVE �����ԍ��v             TO �����ԍ�.
022250***     MOVE �������`�l�J�i�v       TO �������`�l�J�i.
022260***     MOVE �������`�l�v           TO �������`�l.
             MOVE NC"��"                  TO �U���`�F�b�N ���ʃ`�F�b�N.
022270*
022280* �ŉ����Ɋ��҃R�[�h
022290***     MOVE ���Ҕԍ��v�q           TO ���Ҕԍ�.
022300***     MOVE �}�Ԃv�q               TO �}��.
022310*
022750* ���Z�v�g���я��Z�b�g *
022760     MOVE ���Ԃv                 TO ����.
022770*
022320*-------------------------------------------------------------------------*
022330*--- �� ���Z�E�v�ăZ�b�g�́A���̈���Z�b�gSECTION �̍Ō�ɂ�邱�ƁI -----*
022340     PERFORM ���Z�E�v�ăZ�b�g.
022350*-------------------------------------------------------------------------*
022360*
022370*     PERFORM �e�X�g�󎚏���.
022380*
022390*=== ����Z�b�g =================================================*
022400*================================================================*
022410 ���ڏ����� SECTION.
022420*================================================================*
022430     INITIALIZE �{�p�����v.
022440     INITIALIZE ��f�ҏ��v.
022450     INITIALIZE �������v.
022460     INITIALIZE �������v.
022470     INITIALIZE ���l���v.
022480     INITIALIZE �����P�v�q.
022490     INITIALIZE �����Q�v�q.
022500     INITIALIZE �����R�v�q.
022510     MOVE SPACE TO YCB6125P.
022520*****     INITIALIZE YCB6125P.
022530*
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
022540*================================================================*
022550 �{�p�����擾 SECTION.
022560*================================================================*
022570**************************************************
022580* �{�@�f�[�^���g�p���A�ȉ��̏����擾           *
022590* �� �_���t�ԍ�.. �_���t�ԍ��v�Ɋi�[             *
022600* �� ����ԍ� ... �ڍ��t�����ԍ��v�Ɋi�[       *
022610* �� ��\�Җ� ... ��\�Җ��v�Ɋi�[               *
022620* �� �Z��1,2   ...�{�p���Z��1,2�v�Ɋi�[          *
022630* �� �d�b�ԍ� ... �{�p���d�b�ԍ��v�Ɋi�[         *
022640**************************************************
022650     MOVE ZERO  TO �{��|�{�p���ԍ�.
022660     READ �{�p�����}�X�^
022670     INVALID KEY
022680         CONTINUE
022690     NOT INVALID KEY
022700*
               MOVE �{��|�s���{���i�h�r    TO �s���{���i�h�r�v
022740         MOVE �{��|�V�_���t�ԍ�      TO �_���t�ԍ��v
022760*
022770         MOVE �{��|�ڍ��t�����ԍ�  TO �ڍ��t�����ԍ��v
022780         MOVE �{��|�X�֔ԍ��P        TO �{�p���X�֔ԍ��P�v
022790         MOVE �{��|�X�֔ԍ��Q        TO �{�p���X�֔ԍ��Q�v
022800         MOVE �{��|�ڍ��@��          TO �ڍ��@���v
022810         MOVE �{��|��\�҃J�i        TO ��\�҃J�i�v
022820         MOVE �{��|��\�Җ�          TO ��\�Җ��v
022830*
022840*         STRING �{��|�Z���P  DELIMITED BY SPACE
022850*                �{��|�Z���Q  DELIMITED BY SPACE
022860*           INTO �{�p���Z���v
022870*         END-STRING
022880         MOVE �{��|�Z���P            TO �{�p���Z���P�v
022890         MOVE �{��|�Z���Q            TO �{�p���Z���Q�v
022900         MOVE �{��|�d�b�ԍ�          TO �{�p���d�b�ԍ��v
022910* �U������
022920         MOVE �{��|������s��      TO ������s���v
022930         MOVE �{��|������s�x�X��  TO ������s�x�X���v
022940         MOVE �{��|�a�����          TO �a����ʂv
022950         MOVE �{��|��s�ԍ�          TO ��s�ԍ��v
022960         MOVE �{��|�X�ԍ�            TO �X�ԍ��v
022970         MOVE �{��|�����ԍ�          TO �����ԍ��v
022980         MOVE �{��|�������`�l        TO �������`�l�v
022990         MOVE �{��|�������`�l�J�i    TO �������`�l�J�i�v
023000****         MOVE �{��|�ڍ��t����    TO �ڍ��t�����v
023010     END-READ.
023020*
023030     PERFORM ��ϔC����.
023040*
023050     IF ( ��ϔC�t���O = "YES" )
023520        MOVE ZERO  TO  ���|�_���I���敪
023060        MOVE 27    TO  ���|����R�[�h
023070        MOVE ZERO  TO  ���|�ی����
023530        MOVE ZERO  TO  ���|�ύX�a��N��
023090        READ ����}�X�^
023100        NOT INVALID KEY
023110            MOVE ���|������s��      TO ������s���v
023120            MOVE ���|������s�x�X��  TO ������s�x�X���v
023130            MOVE ���|�a�����          TO �a����ʂv
023140            MOVE ���|��s�ԍ�          TO ��s�ԍ��v
023150            MOVE ���|�X�ԍ�            TO �X�ԍ��v
023160            MOVE ���|�����ԍ�          TO �����ԍ��v
023170            MOVE ���|�������`�l�J�i    TO �������`�l�J�i�v
023180            MOVE ���|�������`�l        TO �������`�l�v
023190            MOVE ���|�ڍ��t����    TO �ڍ��t�����v
023200        END-READ
023210     END-IF.
023220*
023230* �U������
023240     STRING ������s���v     DELIMITED BY SPACE
023250            "  "               DELIMITED BY SIZE
023260            ������s�x�X���v DELIMITED BY SPACE
023270            INTO ��s���x�X���v
023280     END-STRING.
023290     EVALUATE �a����ʂv
023300     WHEN 1
023310         MOVE "����" TO �a����ʖ��̂v
023320     WHEN 2
023330         MOVE "����" TO �a����ʖ��̂v
023340     WHEN OTHER
023350         MOVE SPACE  TO �a����ʖ��̂v
023360     END-EVALUATE.
023370     IF ( ��ϔC�t���O = "YES" )
023380        IF ( �a����ʖ��̂v NOT = SPACE )
023390           STRING �a����ʖ��̂v DELIMITED BY SPACE
023400                  "�a��"         DELIMITED BY SIZE
023410                  INTO �a����ʃR�����g�v
023420           END-STRING
023430        END-IF
023440     ELSE
023450        STRING ��s�ԍ��v     DELIMITED BY SPACE
023460               " "            DELIMITED BY SIZE
023470               �X�ԍ��v       DELIMITED BY SPACE
023480               " "            DELIMITED BY SIZE
023490               �a����ʖ��̂v DELIMITED BY SPACE
023500               INTO �a����ʃR�����g�v
023510        END-STRING
023520     END-IF.
023530*
023540* �R�����g��
023550     MOVE SPACE TO �R�����g�v.
023560     INITIALIZE    �R�����g�v.
023570*
023580     IF ( ��ϔC�t���O = "YES" )
023590        MOVE "�����擾������L���z�̎�̌���" TO �R�����g�P�v
023600        STRING "�����_���t����"     DELIMITED BY SIZE
023610               " � "             DELIMITED BY SIZE
023620               �ڍ��t�����v     DELIMITED BY SIZE
023630               INTO �R�����g�Q�v
023640        END-STRING
023650        MOVE "�ɍĈϔC���܂��B"     TO �R�����g�R�v
023660        PERFORM ���t�ҏW
023670        MOVE ���t�ҏW�v             TO �R�����g�S�v
023680        MOVE "�_�������t"           TO �R�����g�T�v
023690        STRING "(����) "            DELIMITED BY SIZE
023700               ��\�Җ��v           DELIMITED BY SIZE
023710               "      (��)"         DELIMITED BY SIZE
023720               INTO �R�����g�U�v
023730        END-STRING
023740        MOVE "(�Z��) �{�p�ؖ����Ɠ���" TO �R�����g�V�v
023750     ELSE
023760        MOVE "�y ���l �z"              TO �R�����g�P�v
023770     END-IF.
023780*
023790*================================================================*
023800 ��ϔC���� SECTION.
023810*
023820**************************************************************************
023830*  �ЕہE���فE�D���E�g���E���ρE���q���́A���ׂĉ�ϔC
023840*  ���ۂ́A���ۑg���̑S���g�D(�S���y�� 133033�A�������� 133264 )����ϔC
023850***************************************************************************
023860     MOVE  SPACE  TO  ��ϔC�t���O.
023870*
023880     IF ( �����ʂv�q = 05 )
023890        CONTINUE
023900     ELSE
023910        EVALUATE �ی���ʂv�q
023920        WHEN 02
023930        WHEN 06
023940        WHEN 07
023950        WHEN 03
023960        WHEN 04
023970        WHEN 09
023980            MOVE  "YES"  TO  ��ϔC�t���O
023990        WHEN 01
024000            IF ( �ی��Ҕԍ��v�q(1:6) = "133033" ) OR
024010               ( �ی��Ҕԍ��v�q = "133264" )
024020               MOVE  "YES"  TO  ��ϔC�t���O
024030            END-IF
024040        WHEN 08
024050            CONTINUE
024060        WHEN OTHER
024070            CONTINUE
024080        END-EVALUATE
024090     END-IF.
024100*/ ���ׂĉ�ϔC�ɕύX /0710
024110     MOVE  "YES"  TO  ��ϔC�t���O.
024120*
024130*================================================================*
024140 ���t�ҏW SECTION.
024150*
024160     MOVE �{�p�a��v�q TO ���|�����敪.
024170     READ �����}�X�^
024180     INVALID KEY
024190         MOVE SPACE TO ���|���R�[�h
024200         INITIALIZE    ���|���R�[�h
024210     NOT INVALID KEY
024220         MOVE ���|�J�n����N TO �{�p����N�v
024230     END-READ.
024240     IF ( �{�p����N�v NOT = ZERO )
024250        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
024260     END-IF.
024270*
024280     EVALUATE �{�p���v�q
024290     WHEN 4
024300     WHEN 6
024310     WHEN 9
024320     WHEN 11
024330         MOVE 30   TO �������v
024340     WHEN 2
024350         DIVIDE 4 INTO �{�p����N�v GIVING    ���v
024360                                    REMAINDER �]�v
024370         END-DIVIDE
024380         IF ( �]�v = ZERO )
024390            MOVE 29 TO �������v
024400         ELSE
024410            MOVE 28 TO �������v
024420         END-IF
024430     WHEN 1
024440     WHEN 3
024450     WHEN 5
024460     WHEN 7
024470     WHEN 8
024480     WHEN 10
024490     WHEN 12
024500         MOVE 31   TO �������v
024510     WHEN OTHER
024520         MOVE ZERO TO �������v
024530     END-EVALUATE.
024540*
024550     MOVE ���|�������� TO �����ҏW�v.
024560     MOVE �{�p�N�v�q   TO �N�ҏW�v.
024570     MOVE �{�p���v�q   TO ���ҏW�v.
024580     MOVE �������v     TO ���ҏW�v.
024590*
024600*================================================================*
024610 ��������擾 SECTION.
024620*================================================================*
024630****************************************************
024640* �A���f�[�^����ی��҃}�X�^��萿������擾����B *
024660* �� ������...... �����於�̂v�Ɋi�[               *
024670****************************************************
024680     MOVE �ی���ʂv�q   TO �ہ|�ی����.
024690     MOVE �ی��Ҕԍ��v�q TO �ہ|�ی��Ҕԍ�.
024700     READ �ی��҃}�X�^
024710     INVALID KEY
024720         IF �ی���ʂv�q = 05
024730             MOVE �ی���ʂv�q       TO �s�|������
024740             MOVE ��p���S�Ҕԍ��v�q TO �s�|�s�����ԍ�
024750             READ �s�����}�X�^
024760             INVALID KEY
024770                 MOVE SPACE          TO �����於�̂v
024780             NOT INVALID KEY
024790                 MOVE �s�|�s�������� TO �����於�̂v
024800             END-READ
024810         ELSE
024820             MOVE SPACE          TO �����於�̂v
024830         END-IF
024840     NOT INVALID KEY
024850** �g���E���ς͎x�����܂ň�
024860         EVALUATE �ی���ʂv�q
024870         WHEN 1
024880         WHEN 8
024890             MOVE �ہ|�ی��Җ���      TO �����於�̂v
024900***             STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
024910***                    "��"              DELIMITED BY SIZE
024920***                    INTO �����於�̂v
024930***             END-STRING
024940         WHEN 2
024950         WHEN 6
024960             IF ( �ہ|�ڔ���敪 = 1 )
024970                MOVE �ہ|�ی��Җ���    TO �����於�̂v
024980             ELSE
024990                STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
025000                       "�Љ�ی�������"  DELIMITED BY SIZE
025010                       INTO �����於�̂v
025020                END-STRING
025030             END-IF
025040         WHEN 3
025050             STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
025060                    "���N�ی��g��"    DELIMITED BY SIZE
025070                     �ہ|�x��������    DELIMITED BY SPACE
025080                     INTO �����於�̂v
025090             END-STRING
025100         WHEN 4
025110             STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
025120                    "���ϑg��"        DELIMITED BY SIZE
025130                    �ہ|�x��������    DELIMITED BY SPACE
025140                    INTO �����於�̂v
025150             END-STRING
025160         WHEN OTHER
025170             MOVE �ہ|�ی��Җ���      TO �����於�̂v
025180         END-EVALUATE
025190     END-READ.
025200*
025210*================================================================*
025220 ��f�ҏ��擾 SECTION.
025230*================================================================*
025240**************************************************
025250* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
025260* �� �{�p�N ..... �{�p�N�v�Ɋi�[                 *
025270* �� �{�p�� ..... �{�p���v�Ɋi�[                 *
025280* �� ���Ҕԍ�.... ���Ҕԍ��v�Ɋi�[���e�c�A�ԗp   *
025290* �� �L�� ....... �L���v�Ɋi�[                   *
025300* �� �ԍ� ....... �ԍ��v�Ɋi�[                   *
025310* �� �ی��Ҕԍ� . �ی��Ҕԍ��v�Ɋi�[             *
025320* �� �ی���� ... �ی���ʂv�Ɋi�[               *
025330* �� ��ی��҃J�i.��ی��҃J�i�v�Ɋi�[           *
025340* �� ��ی��Ҏ���.��ی��Ҏ����v�Ɋi�[           *
025350* �� �Z���P ......��ی��ҏZ���P�v�Ɋi�[         *
025360* �� �Z���Q ......��ی��ҏZ���Q�v�Ɋi�[         *
025370* �� ���҃J�i ....���҃J�i�v�Ɋi�[               *
025380* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
025390* �� ���Ґ��� ....�敪�ɂ��`�F�b�N��"��"���i�[ *
025400* �� ���Ҙa�� ....�a��ɂ��`�F�b�N��"��"���i�[ *
025410* �� ���ҔN ......���ҔN�v�Ɋi�[                 *
025420* �� ���Ҍ� ......���Ҍ��v�Ɋi�[                 *
025430* �� ���ғ� ......���ғ��v�Ɋi�[                 *
025440* �� ���� ........���̃}�X�^��葱���v�Ɏ擾     *
025450**************************************************
           IF ��|���R�[�h NOT = SPACE
      */��ЎґΉ�/110811
               IF ��|���i�ؖ��敪 = 9
                   MOVE NC"��"       TO �Ђv
                   MOVE NC"��"       TO �Њۈ�v
               END-IF
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
               WHEN 09
                  MOVE NC"��"        TO ���σ`�F�b�N�v
      *         WHEN 09
      *            MOVE NC"��"        TO ���`�F�b�N�v
               WHEN 08
                  MOVE NC"��"        TO �ސE�`�F�b�N�v
               WHEN 05
                  MOVE NC"��"        TO ����`�F�b�N�v
022770         END-EVALUATE
      */�S�ĒP��
      *         IF ��|������� = ZERO
                   MOVE NC"��" TO �P�ƃ`�F�b�N�v
      *         ELSE
      *             MOVE NC"��" TO �Q���`�F�b�N�v
      *         END-IF
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
      *
025550         MOVE ��|�{�p�N       TO �{�p�N�v
025560         MOVE ��|�{�p��       TO �{�p���v
025570         MOVE ��|���Ҕԍ�     TO ���Ҕԍ��v
025580*         MOVE ��|�L��         TO �L���v
025590*         MOVE ��|�ԍ�         TO �ԍ��v
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
025600         MOVE ��|�ی��Ҕԍ�   TO �ی��Ҕԍ��v
025610         MOVE ��|�ی����     TO �ی���ʂv
025620*         PERFORM �ی���ʕҏW
025630** �S���y�؂̎}�ԍ폜
025640         IF ( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(1:6) = "133033" )
025650            MOVE ��|�ی��Ҕԍ�(1:6)  TO �ی��Ҕԍ��v
025660         END-IF
025670**
025680         EVALUATE ��|�ی����
025690* ����
025700         WHEN 01
025710            MOVE ��|�ی��Ҕԍ�      TO �ی��Ҕԍ���r�v
025720* �ސE
025720* �㍂
               WHEN 05
025730         WHEN 08
025740            MOVE ��|�ی��Ҕԍ�(3:6) TO �ی��Ҕԍ���r�v
025750         END-EVALUATE
025760         MOVE ��|�ی��Ҕԍ�   TO �ی��Ҕԍ��v
025770**
025780         MOVE ��|��ی��҃J�i TO ��ی��҃J�i�v
025790         MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ����v
025800         MOVE ��|�X�֔ԍ��P   TO �X�֔ԍ��P�v
025810         MOVE ��|�X�֔ԍ��Q   TO �X�֔ԍ��Q�v
025820*         STRING ��|�Z���P  DELIMITED BY SPACE
025830*                ��|�Z���Q  DELIMITED BY SPACE
025840*           INTO ��ی��ҏZ���v
025850*         END-STRING
025860         MOVE ��|�Z���P       TO ��ی��ҏZ���P�v
025870         MOVE ��|�Z���Q       TO ��ی��ҏZ���Q�v
      */ �d�b�ԍ��ǉ� /42505
               IF ��|�d�b�ԍ� NOT = SPACE
                  MOVE ��|�d�b�ԍ� TO �d�b�ԍ��v
               ELSE
                  IF ��|���ғd�b�ԍ� NOT = SPACE
                  MOVE ��|���ғd�b�ԍ� TO �d�b�ԍ��v
                  END-IF
               END-IF
025880         MOVE ��|���҃J�i     TO ���҃J�i�v
025890         MOVE ��|���Ҏ���     TO ���Ҏ����v
025900         EVALUATE ��|���Ґ���
025910         WHEN 1
025920             MOVE NC"��"  TO �j�`�F�b�N�v
025930         WHEN 2
025940             MOVE NC"��"  TO ���`�F�b�N�v
025950         END-EVALUATE
025960*         EVALUATE ��|���Ґ���
025970*         WHEN 1
025980*             MOVE NC"�j"  TO ���ʂv
025990*         WHEN 2
026000*             MOVE NC"��"  TO ���ʂv
026010*         END-EVALUATE
026020         EVALUATE ��|���Ҙa��
026030         WHEN 1
026040             MOVE NC"��"  TO �����`�F�b�N�v
026050         WHEN 2
026060             MOVE NC"��"  TO �吳�`�F�b�N�v
026070         WHEN 3
026080             MOVE NC"��"  TO ���a�`�F�b�N�v
026090         WHEN 4
026100             MOVE NC"��"  TO �����`�F�b�N�v
026110         END-EVALUATE
026120         EVALUATE ��|���Ҙa��
026130         WHEN 1
026140             MOVE NC"����"  TO �����v
026150         WHEN 2
026160             MOVE NC"�吳"  TO �����v
026170         WHEN 3
026180             MOVE NC"���a"  TO �����v
026190         WHEN 4
026200             MOVE NC"����"  TO �����v
026210         END-EVALUATE
026220*
026230         MOVE ��|���ҔN  TO ���ҔN�v
026240         MOVE ��|���Ҍ�  TO ���Ҍ��v
026250         MOVE ��|���ғ�  TO ���ғ��v
026260*
026680         IF  �{�l�Ƒ��敪�v�q = 1 
026690             MOVE NC"�{�l"    TO �����v
026700         ELSE
026710             MOVE NC"�Ƒ�"    TO �����v
026720         END-IF
026730**
026740         IF ( ��|������� NOT = ZERO )
026750            PERFORM �������Z�܂Ƃߔ���
026760         ELSE
026770            MOVE SPACE TO �������Z�܂Ƃ߃t���O
026780         END-IF
026790*
027110     END-IF.
027120*
027130*================================================================*
027140 �Ƒ������Z�b�g SECTION.
027150*
027160     MOVE 05       TO ���|�敪�R�[�h.
027170     MOVE ��|���� TO ���|���̃R�[�h.
027180     READ ���̃}�X�^
027190     INVALID KEY
027200         MOVE SPACE    TO �����v
027210     NOT INVALID KEY
027220         MOVE ���|���� TO �����v
027230     END-READ.
027240*
027250*================================================================*
027260 �������Z�܂Ƃߔ��� SECTION.
027270*---------------------------------------------------------------------------*
027280* �{�̂܂Ƃߋ敪���P
027290* �̎��́A�t���OYES (���z���������݂ň�,�K�p�Q�ɏ�����ʈ󎚁j
027300*�i��F���l�s�̏�Q�́A�{�̕ی��i���یn�j�̃��Z�v�g�P���Ő����A�������Z�͂Ȃ��j
027310*---------------------------------------------------------------------------*
027320*
027330     MOVE SPACE TO �������Z�܂Ƃ߃t���O.
027340     MOVE SPACE TO ������ʗ��̂v.
009201     IF ���Z�|�{�̂܂Ƃߋ敪 = 1 
009202        MOVE "YES" TO �������Z�܂Ƃ߃t���O
027450        MOVE 02            TO ���|�敪�R�[�h
027460        MOVE ��|�������  TO ���|���̃R�[�h
027470        READ ���̃}�X�^
027480        NOT INVALID KEY
027490           MOVE ���|����  TO ������ʗ��̂v
027500        END-READ
009203     END-IF.
027540*
027550*================================================================*
028020 �����f�[�^�擾 SECTION.
028030*================================================================*
028040**************************************************
028050* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾 *
028060* �� ������...���ʁ{������ʂɂĉ��H���Ċi�[     *
028070* �� �����N.......�����N�v                       *
028080* �� ������.......�������v                       *
028090* �� ������.......�������v                       *
028100* �� �J�n�N.......�����N�v                       *
028110* �� �J�n��.......�������v                       *
028120* �� �J�n��.......�������v                       *
028130* �� �I���N.......�I���N�v                       *
028140* �� �I����.......�I�����v                       *
028150* �� �I����.......�I�����v                       *
028160* �� ������.......�������v                       *
028170* �� �]�A�敪 ....�敪�ɂ��`�F�b�N��"��"���i�[ *
028180* �� �������q ....�敪�ɂ��`�F�b�N��"��"���i�[ *
028190* �� �o�߃R�[�h...�o�߃}�X�^���擾             *
028200**************************************************
           IF ���|���R�[�h NOT = SPACE
028300         MOVE ���|���ʐ�                   TO ���ʐ��v
028310         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
028320                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
028330             MOVE ���|�������(���ʂb�m�s) TO ������ʂv(���ʂb�m�s)
028340             MOVE ���|����(���ʂb�m�s)     TO ���ʂv(���ʂb�m�s)
028350             MOVE ���|���E�敪(���ʂb�m�s) TO ���E�敪�v(���ʂb�m�s)
028360             MOVE ���|�����ʒu�ԍ�(���ʂb�m�s)
028370                                           TO �����ʒu�ԍ��v(���ʂb�m�s)
028380********************************************************
028390* ���j�S�_...���ʖ�1+������ʁ{���ʖ�2�ɂĉ��H���Ċi�[ *
028400********************************************************
028410* �������
028420             MOVE SPACE                     TO �������̂v
028430             MOVE 03                        TO ���|�敪�R�[�h
028440             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
028450             READ ���̃}�X�^
028460             INVALID KEY
028470                 MOVE SPACE        TO �������̂v
028480             NOT INVALID KEY
028490                 MOVE ���|�������� TO �������̂v
028500             END-READ
028510* ����
020710             MOVE SPACE                    TO �������v(���ʂb�m�s)
032680*
032690             PERFORM ���ʖ��̖�������
028700*
028710             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
028720             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
028730             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
028740             MOVE ���|�J�n�N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
028750             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
028760             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
028770             IF ( ���|�]�A�敪(���ʂb�m�s) = 9 )
028780                 MOVE 99                   TO �I���N�v(���ʂb�m�s)
028790                 MOVE 99                   TO �I�����v(���ʂb�m�s)
028800                 MOVE 99                   TO �I�����v(���ʂb�m�s)
028810             ELSE
028820                 MOVE ���|�I���N(���ʂb�m�s)   TO �I���N�v(���ʂb�m�s)
028830                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
028840                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
028850             END-IF
028860* �o�ߗ��̎擾
028870             MOVE 01                         TO �o�|�敪�R�[�h
028880             MOVE ���|�o�߃R�[�h(���ʂb�m�s) TO �o�|�o�߃R�[�h
028890             READ �o�߃}�X�^
028900             INVALID KEY
028910                 MOVE ZERO            TO ���ʂb�m�s�v(���ʂb�m�s)
028920                 MOVE SPACE           TO ���ʋ�؂v(���ʂb�m�s)
028930                 MOVE SPACE           TO �o�ߗ��̂v(���ʂb�m�s)
028940             NOT INVALID KEY
028950                 EVALUATE ���ʂb�m�s
028960                 WHEN 1
028970                     MOVE NC"�@" TO �o�ߕ��ʂv
028980                 WHEN 2
028990                     MOVE NC"�A" TO �o�ߕ��ʂv
029000                 WHEN 3
029010                     MOVE NC"�B" TO �o�ߕ��ʂv
029020                 WHEN 4
029030                     MOVE NC"�C" TO �o�ߕ��ʂv
029040                 WHEN 5
029050                     MOVE NC"�D" TO �o�ߕ��ʂv
029060                 END-EVALUATE
029070                 STRING  �o�ߕ��ʂv     DELIMITED BY SPACE
029080                         �o�|�o�ߗ���   DELIMITED BY SPACE
029090                        INTO ����o�ߗ��̂v(���ʂb�m�s)
029100                 END-STRING
029110             END-READ
029120*
029130             MOVE ���|�]�A�敪(���ʂb�m�s) TO �]�A�敪�v(���ʂb�m�s)
029140             EVALUATE ���|�]�A�敪(���ʂb�m�s)
029150             WHEN 1
029160             WHEN 2
029170                 MOVE NC"��"               TO �����`�F�b�N�v(���ʂb�m�s)
029180             WHEN 3
029190                 MOVE NC"��"               TO ���~�`�F�b�N�v(���ʂb�m�s)
029200             WHEN 4
029210                 MOVE NC"��"               TO �]��`�F�b�N�v(���ʂb�m�s)
029220             END-EVALUATE
029230*
                    MOVE ���Z�|���ʎ�����(���ʂb�m�s) TO �������v(���ʂb�m�s)
029240         END-PERFORM
029250* �V�K/�p�� �`�F�b�N
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
029310* �}�Ԕ���p
029320         MOVE ���|�J�n�f�Ó��蓮�敪   TO  �J�n�f�Ó��蓮�敪�v
029330*
029340* ������������敪
029350         MOVE ���|���Z������������敪 TO ���Z������������敪�v
027880         MOVE ���|���Z�������R����敪 TO ���Z�������R����敪�v
029360*
029370     END-IF.
029380*
029390*================================================================*
029400 ���ʖ��̖������� SECTION.
029410*
006490     STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
009980            �������̂v                    DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
006520       INTO �������v(���ʂb�m�s)
006570     END-STRING.
029550*
029560*================================================================*
029570 �������擾 SECTION.
029580*================================================================*
029590********************
029600* �����f�[�^�Z�b�g *
029610********************
029620*    ****************************************************************
029630*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
029640*    ****************************************************************
029650     MOVE ���Z�|������                 TO �������v�q.
029660     IF ( ���Z�|���ԊO = 1 )
029670         MOVE NC"��"                   TO ���ԊO�`�F�b�N�v
029680     END-IF.
029690     IF ( ���Z�|�x�� = 1 )
029700         MOVE NC"��"                   TO �x���`�F�b�N�v
029710     END-IF.
029720     IF ( ���Z�|�[�� = 1 )
029730         MOVE NC"��"                   TO �[��`�F�b�N�v
029740     END-IF.
029750     MOVE ���Z�|���������k��           TO ���k���v�q.
029760*
029770     MOVE ���Z�|�������Z��             TO  �������Z���v�q.
029780     MOVE ���Z�|�Č���                 TO  �Č����v�q.
029790     MOVE ���Z�|���Ë���               TO  ���Ë����v�q.
029800     MOVE ���Z�|���É�               TO  ���É񐔂v�q.
029810     MOVE ���Z�|���×�                 TO  ���×��v�q.
029820     MOVE ���Z�|���É��Z��             TO  ���É��Z���v�q.
029830*
029840     IF ( ���Z�|��� = 1 )
029850         MOVE NC"��"                   TO ��ԃ`�F�b�N�v
029860     END-IF.
029870     IF ( ���Z�|��H = 1 )
029880         MOVE NC"��"                   TO ��H�`�F�b�N�v
029890     END-IF.
029900     IF ( ���Z�|�\���J�� = 1 )
029910         MOVE NC"��"                   TO �\���J��`�F�b�N�v
029920     END-IF.
029930*
029940     MOVE ���Z�|�������q���Z��         TO  �������q���Z���v�q.
029950*
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
030050*
030060     MOVE ���Z�|�{�p���񋟗�         TO  �{�p���񋟗��v�q.
030070* ���v
030080     MOVE ���Z�|���v                   TO ���v�v.
030090********************
030100* ���񏈒u���Z�b�g *
030110********************
030120     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
030130             UNTIL ( ���ʂb�m�s > ���ʐ��v )
030140         MOVE ���Z�|���񏈒u��(���ʂb�m�s) TO ���񏈒u���v�q(���ʂb�m�s)
030150     END-PERFORM.
030160     MOVE ���Z�|���񏈒u�����v         TO ���񏈒u�����v�v.
030170********************
030180* �����������Z�b�g *
030190********************
030200*    **********
030210*    * �P���� *
030220*    **********
030230     MOVE ���Z�|��ÒP���P             TO ��ÒP���P�v�q.
030240     MOVE ���Z�|��É񐔂P             TO ��É񐔂P�v�q.
030250     MOVE ���Z�|��×��P               TO ��×��P�v�q.
030260     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
030270     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
030280     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
030290     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
030300     MOVE ���Z�|�d�É񐔂P             TO �d�É񐔂P�v�q.
030310     MOVE ���Z�|�d�×��P               TO �d�×��P�v�q.
030320     MOVE ���Z�|���v�P                 TO ���v�P�v�q.
030330     MOVE ���Z�|�����������P           TO �����������P�v�q.
030340     MOVE ���Z�|���������v�P           TO ���������v�P�v�q.
030350*    **********
030360*    * �Q���� *
030370*    **********
030380     MOVE ���Z�|��ÒP���Q             TO ��ÒP���Q�v�q.
030390     MOVE ���Z�|��É񐔂Q             TO ��É񐔂Q�v�q.
030400     MOVE ���Z�|��×��Q               TO ��×��Q�v�q.
030410     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
030420     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
030430     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
030440     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
030450     MOVE ���Z�|�d�É񐔂Q             TO �d�É񐔂Q�v�q.
030460     MOVE ���Z�|�d�×��Q               TO �d�×��Q�v�q.
030470     MOVE ���Z�|���v�Q                 TO ���v�Q�v�q.
030480     MOVE ���Z�|�����������Q           TO �����������Q�v�q.
030490     MOVE ���Z�|���������v�Q           TO ���������v�Q�v�q.
030500*    ****************
030510*    * �R���ʁ^�W�� *
030520*    ****************
030530     MOVE ���Z�|��ÒP���R�W             TO ��ÒP���R�W�v�q.
030540     MOVE ���Z�|��É񐔂R�W             TO ��É񐔂R�W�v�q.
030550     MOVE ���Z�|��×��R�W               TO ��×��R�W�v�q.
030560     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
030570     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
030580     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
030590     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
030600     MOVE ���Z�|�d�É񐔂R�W             TO �d�É񐔂R�W�v�q.
030610     MOVE ���Z�|�d�×��R�W               TO �d�×��R�W�v�q.
030620     MOVE ���Z�|���v�R�W                 TO ���v�R�W�v�q.
030630     MOVE ���Z�|�����ʍ����v�R�W         TO �����ʍ����v�R�W�v�q.
030640     MOVE ���Z�|�����������R�W           TO �����������R�W�v�q.
030650     MOVE ���Z�|���������v�R�W           TO ���������v�R�W�v�q.
030660*    ****************
030670*    * �R���ʁ^10�� *
030680*    ****************
030690     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
030700     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
030710     MOVE ���Z�|��ÒP���R�O             TO ��ÒP���R�O�v�q.
030720     MOVE ���Z�|��É񐔂R�O             TO ��É񐔂R�O�v�q.
030730     MOVE ���Z�|��×��R�O               TO ��×��R�O�v�q.
030740     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
030750     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
030760     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
030770     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
030780     MOVE ���Z�|�d�É񐔂R�O             TO �d�É񐔂R�O�v�q.
030790     MOVE ���Z�|�d�×��R�O               TO �d�×��R�O�v�q.
030800     MOVE ���Z�|���v�R�O                 TO ���v�R�O�v�q.
030810     MOVE ���Z�|�����������R�O           TO �����������R�O�v�q.
030820     MOVE ���Z�|���������v�R�O           TO ���������v�R�O�v�q.
030830*    ****************
030840*    * �S���ʁ^�T�� *
030850*    ****************
030860     MOVE ���Z�|��ÒP���S�T             TO ��ÒP���S�T�v�q.
030870     MOVE ���Z�|��É񐔂S�T             TO ��É񐔂S�T�v�q.
030880     MOVE ���Z�|��×��S�T               TO ��×��S�T�v�q.
030890     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
030900     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
030910     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
030920     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
030930     MOVE ���Z�|�d�É񐔂S�T             TO �d�É񐔂S�T�v�q.
030940     MOVE ���Z�|�d�×��S�T               TO �d�×��S�T�v�q.
030950     MOVE ���Z�|���v�S�T                 TO ���v�S�T�v�q.
030960     MOVE ���Z�|�����ʍ����v�S�T         TO �����ʍ����v�S�T�v�q.
030970     MOVE ���Z�|�����������S�T           TO �����������S�T�v�q.
030980     MOVE ���Z�|���������v�S�T           TO ���������v�S�T�v�q.
030990*    ****************
031000*    * �S���ʁ^�W�� *
031010*    ****************
031020     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
031030     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
031040     MOVE ���Z�|��ÒP���S�W             TO ��ÒP���S�W�v�q.
031050     MOVE ���Z�|��É񐔂S�W             TO ��É񐔂S�W�v�q.
031060     MOVE ���Z�|��×��S�W               TO ��×��S�W�v�q.
031070     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
031080     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
031090     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
031100     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
031110     MOVE ���Z�|�d�É񐔂S�W             TO �d�É񐔂S�W�v�q.
031120     MOVE ���Z�|�d�×��S�W               TO �d�×��S�W�v�q.
031130     MOVE ���Z�|���v�S�W                 TO ���v�S�W�v�q.
031140     MOVE ���Z�|�����ʍ����v�S�W         TO �����ʍ����v�S�W�v�q.
031150     MOVE ���Z�|�����������S�W           TO �����������S�W�v�q.
031160     MOVE ���Z�|���������v�S�W           TO ���������v�S�W�v�q.
031170*    ****************
031180*    * �S���ʁ^10�� *
031190*    ****************
031200     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
031210     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
031220     MOVE ���Z�|��ÒP���S�O             TO ��ÒP���S�O�v�q.
031230     MOVE ���Z�|��É񐔂S�O             TO ��É񐔂S�O�v�q.
031240     MOVE ���Z�|��×��S�O               TO ��×��S�O�v�q.
031250     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
031260     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
031270     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
031280     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
031290     MOVE ���Z�|�d�É񐔂S�O             TO �d�É񐔂S�O�v�q.
031300     MOVE ���Z�|�d�×��S�O               TO �d�×��S�O�v�q.
031310     MOVE ���Z�|���v�S�O                 TO ���v�S�O�v�q.
031320     MOVE ���Z�|�����������S�O           TO �����������S�O�v�q.
031330     MOVE ���Z�|���������v�S�O           TO ���������v�S�O�v�q.
031340*    *****************
031350*    * �T���ʁ^2.5�� *
031360*    *****************
031370     MOVE ���Z�|��ÒP���T�Q             TO ��ÒP���T�Q�v�q.
031380     MOVE ���Z�|��É񐔂T�Q             TO ��É񐔂T�Q�v�q.
031390     MOVE ���Z�|��×��T�Q               TO ��×��T�Q�v�q.
031400     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
031410     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
031420     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
031430     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
031440     MOVE ���Z�|�d�É񐔂T�Q             TO �d�É񐔂T�Q�v�q.
031450     MOVE ���Z�|�d�×��T�Q               TO �d�×��T�Q�v�q.
031460     MOVE ���Z�|���v�T�Q                 TO ���v�T�Q�v�q.
031470     MOVE ���Z�|�����ʍ����v�T�Q         TO �����ʍ����v�T�Q�v�q.
031480     MOVE ���Z�|�����������T�Q           TO �����������T�Q�v�q.
031490     MOVE ���Z�|���������v�T�Q           TO ���������v�T�Q�v�q.
031500*    ****************
031510*    * �T���ʁ^�T�� *
031520*    ****************
031530     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
031540     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
031550     MOVE ���Z�|��ÒP���T�T             TO ��ÒP���T�T�v�q.
031560     MOVE ���Z�|��É񐔂T�T             TO ��É񐔂T�T�v�q.
031570     MOVE ���Z�|��×��T�T               TO ��×��T�T�v�q.
031580     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
031590     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
031600     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
031610     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
031620     MOVE ���Z�|�d�É񐔂T�T             TO �d�É񐔂T�T�v�q.
031630     MOVE ���Z�|�d�×��T�T               TO �d�×��T�T�v�q.
031640     MOVE ���Z�|���v�T�T                 TO ���v�T�T�v�q.
031650     MOVE ���Z�|�����ʍ����v�T�T         TO �����ʍ����v�T�T�v�q.
031660     MOVE ���Z�|�����������T�T           TO �����������T�T�v�q.
031670     MOVE ���Z�|���������v�T�T           TO ���������v�T�T�v�q.
031680*    ****************
031690*    * �T���ʁ^�W�� *
031700*    ****************
031710     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
031720     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
031730     MOVE ���Z�|��ÒP���T�W             TO ��ÒP���T�W�v�q.
031740     MOVE ���Z�|��É񐔂T�W             TO ��É񐔂T�W�v�q.
031750     MOVE ���Z�|��×��T�W               TO ��×��T�W�v�q.
031760     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
031770     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
031780     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
031790     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
031800     MOVE ���Z�|�d�É񐔂T�W             TO �d�É񐔂T�W�v�q.
031810     MOVE ���Z�|�d�×��T�W               TO �d�×��T�W�v�q.
031820     MOVE ���Z�|���v�T�W                 TO ���v�T�W�v�q.
031830     MOVE ���Z�|�����ʍ����v�T�W         TO �����ʍ����v�T�W�v�q.
031840     MOVE ���Z�|�����������T�W           TO �����������T�W�v�q.
031850     MOVE ���Z�|���������v�T�W           TO ���������v�T�W�v�q.
031860*    ****************
031870*    * �T���ʁ^10�� *
031880*    ****************
031890     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
031900     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
031910     MOVE ���Z�|��ÒP���T�O             TO ��ÒP���T�O�v�q.
031920     MOVE ���Z�|��É񐔂T�O             TO ��É񐔂T�O�v�q.
031930     MOVE ���Z�|��×��T�O               TO ��×��T�O�v�q.
031940     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
031950     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
031960     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
031970     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
031980     MOVE ���Z�|�d�É񐔂T�O             TO �d�É񐔂T�O�v�q.
031990     MOVE ���Z�|�d�×��T�O               TO �d�×��T�O�v�q.
032000     MOVE ���Z�|���v�T�O                 TO ���v�T�O�v�q.
032010     MOVE ���Z�|�����������T�O           TO �����������T�O�v�q.
032020     MOVE ���Z�|���������v�T�O           TO ���������v�T�O�v�q.
032030*
032040*================================================================*
032050 �{�p�L�^�擾 SECTION.
032060*================================================================*
032070************************************************************
032080* ��P�f�[�^���畉���f�[�^�e���ȉ��̏����擾           *
032090* �� �������Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
032100* �� ���É��Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
032110************************************************************
032120     MOVE  SPACE  TO  �����Č��t���O.
032130     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���ʐ��v
032140         IF ( �{�p�N�v = �����N�v(���ʂb�m�s) ) AND
032150            ( �{�p���v = �������v(���ʂb�m�s) )
032160             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
032170             MOVE �}�Ԃv�q              TO �{�L�|�}��
032180             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
032190             MOVE �����N�v(���ʂb�m�s)  TO �J�n�N�v(���ʂb�m�s) �{�L�|�{�p�N
032200             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
032210             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
032220         ELSE
032230             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
032240             MOVE �}�Ԃv�q              TO �{�L�|�}��
032250             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
032260             MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
032270             MOVE �{�p���v�q            TO �{�L�|�{�p��
032280             MOVE ZERO                  TO �{�L�|�{�p��
032290         END-IF
032300         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
032310                                      �{�L�|�{�p�a��N����
032320         END-START
032330         IF ( ��ԃL�[ = "00" )
032350             MOVE ZERO  TO �I���N�v�s
032360             MOVE ZERO  TO �I�����v�s
032370             MOVE ZERO  TO �I�����v�s
032380             MOVE SPACE TO �I���t���O�Q
032390             PERFORM �{�p�L�^�e�Ǎ�
032400             IF  ( �I���t���O�Q      = SPACE   ) AND
032410                 ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
032420                 ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
032430                 ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
032440                 ( �{�L�|�{�p��      = �{�p���v�q     ) 
032450*
032460*        *****************************************************************
032470*        * �J�n�N���� ( ���̕��ʂ����������łȂ����A
032480*                       ���������ł��}�Ԃ����鎞�́A�ŏ��̎{�p�����J�n��)*
032490*        *****************************************************************
032500                 IF ( �{�p�N�v NOT = �����N�v(���ʂb�m�s) ) OR
032510                    ( �{�p���v NOT = �������v(���ʂb�m�s) ) OR
032520                    ( �J�n�f�Ó��蓮�敪�v = 1 )
032530                     MOVE �{�L�|�{�p�N   TO �J�n�N�v(���ʂb�m�s)
032540                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
032550                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
032560                 END-IF
032570             END-IF
032580             PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
032590                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
032600                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
032610                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
032620                           ( �{�L�|�{�p��     NOT = �{�p���v�q       ) OR
032630                           ( �{�L�|�{�p��         > �I�����v(���ʂb�m�s))
032680                MOVE �{�L�|�{�p�N               TO �I���N�v�s
032690                MOVE �{�L�|�{�p��               TO �I�����v�s
032700                MOVE �{�L�|�{�p��               TO �I�����v�s
032710*
032720                PERFORM �{�p�L�^�e�Ǎ�
032730            END-PERFORM
032740        END-IF
032750*       **************************
032760*       * �p���F�I���N�����Z�b�g *
032770*       **************************
032780        IF ( �]�A�敪�v(���ʂb�m�s) = 9 )
032790            MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
032800            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
032810            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
032820        END-IF
032830        IF ( �I���N�����v(���ʂb�m�s) > �󗝔N�����v )
032840            MOVE �I���N�v(���ʂb�m�s) TO �󗝔N�v
032850            MOVE �I�����v(���ʂb�m�s) TO �󗝌��v
032860            MOVE �I�����v(���ʂb�m�s) TO �󗝓��v
032870        END-IF
032880     END-PERFORM.
032890*
032900** ----- �O�������݂̂��𔻒� -----------*
032910*
032920*     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
032930*     MOVE �}�Ԃv�q              TO �{�L�|�}��.
032940*     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
032950*     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
032960*     MOVE �{�p���v�q            TO �{�L�|�{�p��.
032970*     MOVE ZERO                  TO �{�L�|�{�p��.
032980*     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
032990*                                  �{�L�|�{�p�a��N����
033000*     END-START.
033010*     IF ( ��ԃL�[ = "00" )
033020*             MOVE SPACE TO �I���t���O�Q
033030*             PERFORM �{�p�L�^�e�Ǎ�
033040*             IF  ( �I���t���O�Q      = SPACE   ) AND
033050*                 ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
033060*                 ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
033070*                 ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
033080*                 ( �{�L�|�{�p��      = �{�p���v�q     ) 
033090** �����{�p�J�n�����Č����ǂ�������
033100*                 IF   ( �{�L�|�Č������� = 1 )
033110*                      MOVE "YES"  TO  �����Č��t���O
033120*                 END-IF
033130**
033140*             END-IF
033150*     END-IF.
033160*     IF ( �����Č��t���O = "YES" )
033170*        PERFORM �O�������̂ݔ���
033180*     END-IF.
033190*
033200*================================================================*
033210 �O�������̂ݔ��� SECTION.
033220*
033230*** �O���̒ʉ@�������������� 
033240     MOVE  SPACE            TO �O���t���O.
033250     MOVE ��|���҃R�[�h    TO �{�L�|���҃R�[�h.
033260     MOVE ��|�{�p�a��      TO �{�L�|�{�p�a��.
033270     MOVE ��|�{�p�N        TO �{�L�|�{�p�N.
033280     MOVE ��|�{�p��        TO �{�L�|�{�p��.
033290     MOVE 1                 TO �{�L�|�{�p��.
033300     START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
033310                                  �{�L�|�{�p�a��N����
033320                                  REVERSED
033330     END-START.
033340     IF ( ��ԃL�[ = "00" )
033350         MOVE SPACE  TO �I���t���O�Q
033360         PERFORM �{�p�L�^�e�Ǎ�
033370         IF ( �I���t���O�Q      = SPACE  ) AND
033380            ( �{�L�|���҃R�[�h  = ��|���҃R�[�h ) AND
033390            ( �{�L�|�f�Ë敪    = 2 ) 
033400*
033410            PERFORM �O������
033420**** �K�p�P���g�p
033430            IF ( �O���t���O = "YES" )
033440               MOVE NC"���O�������̂�"    TO  �K�p�P�v
033450            END-IF
033460**
033470         END-IF
033480     END-IF.
033490*
033500*================================================================*
033510 �O������  SECTION.
033520* 
033530*** �ǂݍ��񂾎{�p�L�^�̔N�����A�O�����ǂ������� (�N���̍��� 1 ��?)
033540      MOVE  SPACE  TO  �O���t���O.
033550      INITIALIZE  �v�Z�N�����v �J�n�N�����Q�v �I���N�����Q�v.
033560**
033570      MOVE ��|�{�p�a��    TO �I���a��Q�v.
033580      MOVE ��|�{�p�N      TO �I���N�Q�v.
033590      MOVE ��|�{�p��      TO �I�����Q�v.
033600      MOVE �{�L�|�{�p�a��  TO �J�n�a��Q�v.
033610      MOVE �{�L�|�{�p�N    TO �J�n�N�Q�v.
033620      MOVE �{�L�|�{�p��    TO �J�n���Q�v.
033630*
033640      EVALUATE TRUE
033650       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v = �I���N�Q�v)
033660            PERFORM  �O����r��
033670       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v NOT = �I���N�Q�v)
033680            PERFORM  �O����r�N
033690       WHEN  �J�n�a��Q�v NOT = �I���a��Q�v 
033700            PERFORM  �O����r����
033710      END-EVALUATE.
033720*
033730      IF ( �v�Z���v = 1 )
033740         MOVE  "YES"  TO  �O���t���O
033750      END-IF.
033760*
033770*================================================================*
033780 �O����r����  SECTION.
033790*
033800     MOVE �J�n�a��Q�v TO ���|�����敪.
033810     READ �����}�X�^
033820     NOT INVALID KEY
033830         MOVE ���|�J�n����N TO �J�n����N�v
033840     END-READ.
033850     MOVE �I���a��Q�v TO ���|�����敪.
033860     READ �����}�X�^
033870     NOT INVALID KEY
033880         MOVE ���|�J�n����N TO �I������N�v
033890     END-READ.
033900**
033910     IF ( �J�n����N�v NOT = ZERO ) AND ( �I������N�v NOT = ZERO )
033920        COMPUTE �J�n����N�v = �J�n����N�v + �J�n�N�Q�v - 1
033930        COMPUTE �I������N�v = �I������N�v + �I���N�Q�v - 1
033940*
033950        IF ( �I������N�v =  �J�n����N�v )
033960           PERFORM  �O����r��
033970        ELSE
033980           IF  ( �I������N�v >  �J�n����N�v )
033990               COMPUTE �v�Z�N�v = �I������N�v - �J�n����N�v
034000               COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
034010           ELSE
034020               MOVE ZERO TO �v�Z���v
034030           END-IF
034040        END-IF
034050     ELSE
034060        MOVE ZERO TO �v�Z���v
034070     END-IF.
034080*
034090*================================================================*
034100 �O����r�N  SECTION.
034110*
034120     IF  ( �I���N�Q�v >  �J�n�N�Q�v )
034130         COMPUTE �v�Z�N�v = �I���N�Q�v - �J�n�N�Q�v
034140         COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
034150     ELSE
034160        MOVE ZERO TO �v�Z���v
034170     END-IF.
034180*
034190*================================================================*
034200 �O����r��  SECTION.
034210*
034220     IF  ( �I�����Q�v >  �J�n���Q�v )
034230         COMPUTE �v�Z���v = �I�����Q�v - �J�n���Q�v
034240     ELSE
034250        MOVE ZERO TO �v�Z���v
034260     END-IF.
034270*
034280*================================================================*
034290 ��������擾 SECTION.
034300*================================================================*
034310* �R�J���ȏ�̒�������� "CHOUKI" ���Ă�. 
034320     MOVE  SPACE TO  �A���ԁ|�L�[.
034330     INITIALIZE      �A���ԁ|�L�[.
034340     MOVE �{�p�a��v�q  TO  �A���ԁ|�{�p�a��.
034350     MOVE �{�p�N�v�q    TO  �A���ԁ|�{�p�N.
034360     MOVE �{�p���v�q    TO  �A���ԁ|�{�p��.
034370     MOVE ���Ҕԍ��v�q  TO  �A���ԁ|���Ҕԍ�.
034380     MOVE �}�Ԃv�q      TO  �A���ԁ|�}��.
034390*
034400     CALL   "CHOUKI".
034410     CANCEL "CHOUKI".
034420*
034430*
034440*     IF ( �A���ԁ|�Ώۃt���O  = "YES" )
034450*        MOVE NC"�i�����{�p�p���K�v���R�j" TO �������R�Œ�
034460*     END-IF.
034470*
035190*================================================================*
035200 �������Z�����擾 SECTION.
035210*================================================================*
035220*****************************************************************
035230** �������Z�����ԊO�Ɛ[��̎��A�K�p�Ɂu��t���ԁv���󎚂���B
035240**   �����̈󎚂͌�3��܂ŉ\
035250*****************************************************************
035260     IF ( ���Z�|���ԊO = 1 ) OR ( ���Z�|�[�� = 1 ) OR ( ���Z�|�x�� = 1 )
035270*
035280         MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
035290         MOVE �}�Ԃv�q              TO �{�L�|�}��
035300         MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
035310         MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
035320         MOVE �{�p���v�q            TO �{�L�|�{�p��
035330         MOVE ZERO                  TO �{�L�|�{�p��
035340         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
035350                                      �{�L�|�{�p�a��N����
035360         END-START
035370         IF ( ��ԃL�[ = "00" )
035380             MOVE ZERO  TO �������Z�J�E���g
035390             MOVE SPACE TO �I���t���O�Q
035400             PERFORM �{�p�L�^�e�Ǎ�
035410             PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
035420                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
035430                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
035440                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
035450                           ( �{�L�|�{�p��     NOT = �{�p���v�q      ) 
035460                   IF  ( �{�L�|�������Z = 1 OR 2 OR 3 ) AND ( �{�L�|�f�Ë敪 = 2 )
035470                       COMPUTE �������Z�J�E���g = �������Z�J�E���g  + 1
035480                       IF  �������Z�J�E���g <= 3
035490                           MOVE �{�L�|�������Z TO �������Z�敪�v�s(�������Z�J�E���g)
035500                           MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
035510                           MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
035520                       END-IF
035530                   END-IF
035540                   PERFORM �{�p�L�^�e�Ǎ�
035550             END-PERFORM
035560** �������Z�̎�����K�p�ɃZ�b�g
033380            IF ( �������Z���v�s(1) NOT = ZERO ) OR ( �������Z���v�s(1) NOT = ZERO ) 
                     MOVE �������Z���v�s(1) TO �������Z���v
                     MOVE ":"               TO �������Z��؂v
                     MOVE �������Z���v�s(1) TO �������Z���v
                  END-IF
033380            IF ( �������Z���v�s(2) NOT = ZERO ) OR ( �������Z���v�s(2) NOT = ZERO ) 
031910               PERFORM �������Z�K�p�Z�b�g
                  END-IF
035580         END-IF
035590*
035600     END-IF.
035610*
035620*================================================================*
035630 �������Z�K�p�Z�b�g SECTION.
035640*
035650     PERFORM VARYING �ԍ��J�E���^ FROM 1 BY 1
035660              UNTIL  �ԍ��J�E���^ > 3
035670         IF ( �������Z���v�s(�ԍ��J�E���^)  = ZERO )  AND 
035680            ( �������Z���v�s(�ԍ��J�E���^)  = ZERO ) 
035690             CONTINUE
035700         ELSE
035710* �Œ荀��
035720             EVALUATE �������Z�敪�v�s(�ԍ��J�E���^) 
035730             WHEN 1
035740                MOVE NC"���ԊO"   TO ���Z���e�v(�ԍ��J�E���^)
033320             WHEN 2
033330                MOVE NC"�x�@��"   TO ���Z���e�v(�ԍ��J�E���^)
035750             WHEN 3
035760                MOVE NC"�[�@��"   TO ���Z���e�v(�ԍ��J�E���^)
035770             END-EVALUATE
035780*
035790             MOVE NC"�F"          TO ���Z��؂v(�ԍ��J�E���^)
035800             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
035810             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
035820*
035830**** ���������{��ϊ�
035840* ����
035850             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
035860             IF ( �����v >= 10 )
035870                 MOVE �����v�P    TO �����ԍ��v�P
035880                 PERFORM ���{��ϊ�
035890                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
035900                 MOVE �����v�Q    TO �����ԍ��v�P
035910                 PERFORM ���{��ϊ�
035920                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
035930             ELSE
035940                 MOVE �����v�Q    TO �����ԍ��v�P
035950                 PERFORM ���{��ϊ�
035960                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
035970             END-IF
035980* ��
035990             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
036000             MOVE �����v�P    TO �����ԍ��v�P
036010             PERFORM ���{��ϊ�
036020             MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
036030             MOVE �����v�Q    TO �����ԍ��v�P
036040             PERFORM ���{��ϊ�
036050             MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
036060** 
036070        END-IF
036080     END-PERFORM.
036090*
036100     MOVE  �������Z�W�c�m�v(1)   TO �������Z�����P�v. 
036110     MOVE  �������Z�W�c�m�v(2)   TO �������Z�����Q�v. 
036120     MOVE  �������Z�W�c�m�v(3)   TO �������Z�����R�v. 
036130*
036140**** �K�p�P���Q���g�p�i�������R�L�ڂœK�p�P���g���Ă��鎞�́A�K�p�Q�j
036150     IF ( �������Z���v�s(2)  = ZERO ) AND ( �������Z���v�s(2)  = ZERO ) 
036160         CONTINUE
036170     ELSE
036180         IF ( �K�p�P�v  = SPACE )
036190               STRING NC"�������Z"       DELIMITED BY SIZE
036200                      �������Z�����P�v   DELIMITED BY SIZE
036210                      �������Z�����Q�v   DELIMITED BY SIZE
036220                      �������Z�����R�v   DELIMITED BY SIZE
036230                      INTO �K�p�P�v
036240               END-STRING
036250         ELSE
036260               STRING NC"�������Z"       DELIMITED BY SIZE
036270                      �������Z�����P�v   DELIMITED BY SIZE
036280                      �������Z�����Q�v   DELIMITED BY SIZE
036290                      �������Z�����R�v   DELIMITED BY SIZE
036300                      INTO �K�p�Q�v
036310               END-STRING
036320         END-IF
036330     END-IF.
036340*
036350*================================================================*
036360 ���{��ϊ� SECTION.
036370*
036380     MOVE NC"�O"     TO �S�p�����ԍ��v.
036390     CALL "htoz" WITH C LINKAGE
036400                        USING �����ԍ��v�P �S�p�����ԍ��v�P.
036410*
036420*================================================================*
036430 �ϔC�N�����擾 SECTION.
036440*================================================================*
036450** ---// �����̎󗝔N�ɂ́A�ŏI�ʉ@���������Ă���ׁA�ޔ����� //----
036460     MOVE �󗝔N�v   TO �ŏI�ʉ@�N�v.
036470     MOVE �󗝌��v   TO �ŏI�ʉ@���v.
036480     MOVE �󗝓��v   TO �ŏI�ʉ@���v.
036490***
036500* (�_���t��)
036510     EVALUATE ���Z�v�g���t�敪�v 
036520*    /  �ŏI�ʉ@�� /
036530     WHEN ZERO
036540         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
036550         MOVE �ŏI�ʉ@���v TO �_���t���v
036560         MOVE �ŏI�ʉ@���v TO �_���t���v
036570*    /  ������ /
036580     WHEN 1 
036590         PERFORM �������擾
036600         MOVE �󗝔N�v     TO �_���t�N�v
036610         MOVE �󗝌��v     TO �_���t���v
036620         MOVE �󗝓��v     TO �_���t���v
036630*    /  �󎚂Ȃ� /
036640     WHEN 9
036650         MOVE ZERO         TO �_���t�N�v
036660         MOVE ZERO         TO �_���t���v
036670         MOVE ZERO         TO �_���t���v
036680*    /  ���̑��́A�ŏI�ʉ@�� /
036690     WHEN OTHER
036700         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
036710         MOVE �ŏI�ʉ@���v TO �_���t���v
036720         MOVE �ŏI�ʉ@���v TO �_���t���v
036730     END-EVALUATE.
036740**
036750* (���ґ�)
036760     EVALUATE ���Z�v�g���ғ��t�敪�v 
036770*    /  �ŏI�ʉ@�� /
036780     WHEN ZERO
036790         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
036800         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
036810         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
036820*    /  ������ /
036830     WHEN 1 
036840         PERFORM �������擾
036850         MOVE �󗝔N�v     TO ���҈ϔC�N�v
036860         MOVE �󗝌��v     TO ���҈ϔC���v
036870         MOVE �󗝓��v     TO ���҈ϔC���v
036880*    /  �󎚂Ȃ� /
036890     WHEN 9
036900         MOVE ZERO         TO ���҈ϔC�N�v
036910         MOVE ZERO         TO ���҈ϔC���v
036920         MOVE ZERO         TO ���҈ϔC���v
036930*    /  ���̑��́A�ŏI�ʉ@�� /
036940     WHEN OTHER
036950         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
036960         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
036970         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
036980     END-EVALUATE.
036990*
037000*================================================================*
037010 �������擾 SECTION.
037020*
037030     MOVE �{�p�N�v�q   TO �󗝔N�v.
037040     MOVE �{�p���v�q   TO �󗝌��v.
037050     MOVE �{�p�a��v�q TO ���|�����敪.
037060     READ �����}�X�^
037070     NOT INVALID KEY
037080         MOVE ���|�J�n����N TO �{�p����N�v
037090     END-READ.
037100     IF ( �{�p����N�v NOT = ZERO )
037110        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
037120     END-IF.
037130*
037140     EVALUATE �{�p���v�q
037150     WHEN 4
037160     WHEN 6
037170     WHEN 9
037180     WHEN 11
037190         MOVE 30 TO �󗝓��v
037200     WHEN 2
037210         DIVIDE 4 INTO �{�p����N�v GIVING    ���v
037220                                    REMAINDER �]�v
037230         END-DIVIDE
037240         IF ( �]�v = ZERO )
037250             MOVE 29 TO �󗝓��v
037260         ELSE
037270             MOVE 28 TO �󗝓��v
037280         END-IF
037290     WHEN 1
037300     WHEN 3
037310     WHEN 5
037320     WHEN 7
037330     WHEN 8
037340     WHEN 10
037350     WHEN 12
037360         MOVE 31 TO �󗝓��v
037370     WHEN OTHER
037380          CONTINUE
037390     END-EVALUATE.
037400*
037410*================================================================*
037420 ���������擾 SECTION.
037430*================================================================*
037440********************************************************************
037450*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
037460*  ��: �@�A �Ƃœ]��.
037470*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
037480*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
037490********************************************************************
037500     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
037510     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
037520             UNTIL ( ���ʂb�m�s > ���ʐ��v )
037530*
037540****        IF ( ���|�������Ҕԍ�(���ʂb�m�s)  NOT = ZERO )  AND
037550        IF ( ���|�����A��(���ʂb�m�s)      NOT = ZERO )
037560*
037570           IF ( �J�E���^ = ZERO )
037580               MOVE 1   TO  �J�E���^ �J�E���^�Q
037590               MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
037600               MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
037610               MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
037620           ELSE
037630              IF ( ���|�������Ҕԍ�(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
037640                 ( ���|�����A��(���ʂb�m�s)      = �����A�Ԃb�v     )
037650                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
037660                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
037670              ELSE
037680                 COMPUTE �J�E���^ = �J�E���^  +  1
037690                 MOVE 1   TO  �J�E���^�Q
037700                 MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
037710                 MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
037720                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
037730              END-IF
037740           END-IF
037750        END-IF
037760     END-PERFORM.
037770**************************************************************************
037780*  ���������}�X�^��蕶�͎擾
037790**************************************************************************
037800     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
037810     PERFORM VARYING �J�E���^ FROM 1 BY 1
037820             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
037830** ���ۂ� �敪 01
037840         MOVE 01                        TO �����|�敪�R�[�h
037850         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
037860         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
037870         READ ���������e
037880         NOT INVALID KEY
037890             INITIALIZE ���������v�s
037900             MOVE �����|���������b�l(1) TO  ���������P�v�s
037910             MOVE �����|���������b�l(2) TO  ���������Q�v�s
037920             MOVE �����|���������b�l(3) TO  ���������R�v�s
037930             MOVE �����|���������b�l(4) TO  ���������S�v�s
037940             MOVE �����|���������b�l(5) TO  ���������T�v�s
037950             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
037960                     UNTIL ( �J�E���^�Q > 9 )  OR 
037970                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
037980                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
037990                WHEN 1
038000                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
038010                WHEN 2
038020                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
038030                WHEN 3
038040                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
038050                WHEN 4
038060                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
038070                WHEN 5
038080                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
038050                WHEN 6
038060                   MOVE "�E"  TO  ���������i���o�[�v�P(�J�E���^�Q)
038070                WHEN 7
038080                   MOVE "�F"  TO  ���������i���o�[�v�P(�J�E���^�Q)
038090                WHEN OTHER
038100                   CONTINUE
038110                END-EVALUATE
038120             END-PERFORM
038130*
038140             IF �����|�����������͋敪 = 1
038150                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
038160                        ���������P�v�s  DELIMITED BY SIZE
038170                        ���������Q�v�s  DELIMITED BY SIZE
038180                        ���������R�v�s  DELIMITED BY SIZE
038190                        ���������S�v�s  DELIMITED BY SIZE
038200                        ���������T�v�s  DELIMITED BY SIZE
038210                        INTO �����������e�����v(�J�E���^)
038220                 END-STRING
038230             ELSE
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
038320             END-IF
038330*
038340         END-READ
038350     END-PERFORM.
038360*
038370     PERFORM ���������Z�b�g.
038380*
038390*================================================================*
038400 ���������Z�b�g SECTION.
038410*
038420**************************************************************************
038430*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
038440**************************************************************************
038450     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
038460     PERFORM VARYING �J�E���^ FROM 1 BY 1
038470             UNTIL ( �J�E���^ > 9 )  OR ( �����������e�����v(�J�E���^) = SPACE )
038480*
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
038630*
038640     END-PERFORM.
038650*
038660*================================================================*
038670 �������R���擾 SECTION.
038680*================================================================*
038690* �������R���擾�� "CHOUBUN" ���Ă�. 
038700     MOVE  SPACE TO  �A�����|�L�[.
038710     INITIALIZE      �A�����|�L�[.
038720     MOVE �{�p�a��v�q  TO  �A�����|�{�p�a��.
038730     MOVE �{�p�N�v�q    TO  �A�����|�{�p�N.
038740     MOVE �{�p���v�q    TO  �A�����|�{�p��.
038750     MOVE ���Ҕԍ��v�q  TO  �A�����|���Ҕԍ�.
038760     MOVE �}�Ԃv�q      TO  �A�����|�}��.
038780     MOVE 56            TO  �A�����|������.
038790*
038800     CALL   "CHOUBUN".
038810     CANCEL "CHOUBUN".
038820*
038830*================================================================*
038840 �{�p�h�c�擾 SECTION.
038850*================================================================*
038860*********************************************
038870** �h�c�Ǘ��}�X�^���@���{�p�h�c���擾����B
038880*********************************************
038890**   / ���{�pID /
038900     MOVE 01                     TO �h�c�ǁ|�h�c�敪.
038910     MOVE ZERO                   TO �h�c�ǁ|�{�p���ԍ�.
038920     MOVE �ی��Ҕԍ���r�v(1:2)  TO �h�c�ǁ|�ی����.
038930     MOVE SPACE                  TO �h�c�ǁ|�ی��Ҕԍ�.
038940     READ �h�c�Ǘ��}�X�^
038950     NOT INVALID KEY
038960         MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO ���{�p�h�c�v
038970     END-READ.
038980*
039090*================================================================*
039100* ���Z�v�g�񐔎擾 SECTION.
039110**================================================================*
039120**************************************************************************
039130***-------- ���Z�v�g�̑� XX ��� �̉񐔂����߂�B----------**
039140**  ���ʂ̊J�n�N���ŁA��ԏ�����(�Â�)�N���Ǝ{�p�N���Ƃ̍���1�𑫂�
039150**  (��) �J�n�N��10�N7��  �Ŏ{�p�N��10�N10���́A4���
039160**  (��) �J�n�N��10�N10�� �Ŏ{�p�N��10�N10���́A1���
039170**************************************************************************
039180**
039190*     MOVE ZERO     TO �񐔂v.
039200**
039210*     PERFORM �J�n�N���ŏ��擾.
039220*     PERFORM ���̌��擾.
039230*     MOVE �v�Z���v TO �񐔂v.
039240**
039250**================================================================*
039260* �J�n�N���ŏ��擾  SECTION.
039270**
039280*** --// ���ʂ̊J�n�N���ŁA��ԏ�����(�Â�)�N�������߂�. //--**
039290**
039300*     INITIALIZE �ŏ��J�n�a��N���v.
039310** 1���ʖڂ�2���ʖڂ��r
039320*     IF ( ���|�J�n�a��N��(2) NOT = ZERO )
039330*        IF ( ���|�J�n�a��N��(1)  <  ���|�J�n�a��N��(2) )
039340*           MOVE ���|�J�n�a��N��(1) TO �ŏ��J�n�a��N���v
039350*        ELSE
039360*           MOVE ���|�J�n�a��N��(2) TO �ŏ��J�n�a��N���v
039370*        END-IF
039380*     ELSE
039390*        MOVE ���|�J�n�a��N��(1) TO �ŏ��J�n�a��N���v
039400*     END-IF.
039410** 3���ʖڈȍ~���r
039420*     PERFORM VARYING ���ʂb�m�s FROM 3 BY 1
039430*             UNTIL ( ���ʂb�m�s > ���ʐ��v )
039440*         IF ( ���|�J�n�a��N��(���ʂb�m�s) <  �ŏ��J�n�a��N���v )
039450*            MOVE ���|�J�n�a��N��(���ʂb�m�s) TO �ŏ��J�n�a��N���v
039460*         END-IF
039470*     END-PERFORM.
039480**
039490**================================================================*
039500 ���̌��擾  SECTION.
039510*********************************************************** 
039520*   �J�n�N���Ǝ{�p�N���Ƃ̍��̌������߂�B
039530*    (�O������̃��W�b�N�A�Z�N�V�����𗘗p)
039540*********************************************************** 
039550*
039560      INITIALIZE  �v�Z�N�����v �J�n�N�����Q�v �I���N�����Q�v.
039570*
039580      IF ( �ŏ��J�n�a��N���v NOT = ZERO )
039590*
039600          MOVE �{�p�a��v�q    TO �I���a��Q�v
039610          MOVE �{�p�N�v�q      TO �I���N�Q�v
039620          MOVE �{�p���v�q      TO �I�����Q�v
039630          MOVE �ŏ��J�n�a��v  TO �J�n�a��Q�v
039640          MOVE �ŏ��J�n�N�v    TO �J�n�N�Q�v
039650          MOVE �ŏ��J�n���v    TO �J�n���Q�v
039660*
039670          EVALUATE TRUE
039680           WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v = �I���N�Q�v)
039690                PERFORM  �O����r��
039700           WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v NOT = �I���N�Q�v)
039710                PERFORM  �O����r�N
039720           WHEN  �J�n�a��Q�v NOT = �I���a��Q�v 
039730                PERFORM  �O����r����
039740          END-EVALUATE
039750*
039760          COMPUTE �v�Z���v =  �v�Z���v + 1
039770*
039780      END-IF.
039790*
039800*================================================================*
039810* ������擾 SECTION.
039820**================================================================*
039830*     MOVE SPACE TO ������v ������`�F�b�N�v.
039840**
039850*     EVALUATE ������ʂv�q 
039860**** ���� (���ۂ͂��̑������ŁA�Y���Ȃ�)
039870*     WHEN  50
039880*         CONTINUE
039970**** ��q
039980*     WHEN  52
040030*            MOVE NC"��"    TO ������v
040050**** �g��
040060*     WHEN  53
040070*            MOVE NC"��"    TO ������v
040110**** ���c�� 
040120*     WHEN  55
040140*            MOVE NC"�q"    TO ������v
040150**** ���̑�
040160*     WHEN  60
040171*         IF ��p���S�Ҕԍ������v�q(1:4) = "8923"
040172*             MOVE NC"��"    TO ������v
040173*         END-IF
040180*     WHEN  OTHER
040190*            CONTINUE
040200*     END-EVALUATE.
040210**
040211*     IF (( �ی���ʂv�q = 05 ) AND ( �ی��Ҕԍ��v�q(1:5) = "39231" ) AND
040212*         ( ��|�������S���Ə� = 1 ))
040213*         MOVE NC"��"    TO ������v
040214*     END-IF.
040215**
040220*     IF ( ������v NOT = SPACE )
040230*        MOVE NC"��" TO ������`�F�b�N�v
040240*     END-IF.
040380*
040390*================================================================*
040400* �ی���ʕҏW SECTION.
040410**================================================================*
040420*     EVALUATE �ی���ʂv
040430*     WHEN 1
040440*         IF ��|�ی��Ҕԍ�(3:1) = 3
040450*             MOVE NC"���g"   TO �ی���ʐe�v
040460*         ELSE
040470*             MOVE NC"��"     TO �ی���ʐe�v
040480*         END-IF
040490*     WHEN 2
040500*         IF (��|�ی��Ҕԍ�(1:2) = 01) AND
040510*            (��|�ی��Ҕԍ�(5:4) NOT = SPACE)
040520*             MOVE NC"��"     TO �ی���ʐe�v
040530*         ELSE
040540*             MOVE NC"��"     TO �ی���ʐe�v
040550*         END-IF
040560*     WHEN 3
040570*         MOVE NC"�g"         TO �ی���ʐe�v
040580*     WHEN 4
040590*         MOVE NC"��"         TO �ی���ʐe�v
040600*     WHEN 5
040610*         MOVE NC"���"       TO �ی���ʐe�v
040620*     WHEN 6
040630*         MOVE NC"��"         TO �ی���ʐe�v
040640*     WHEN 7
040650*         MOVE NC"�D"         TO �ی���ʐe�v
040660*     WHEN 8
040670*         MOVE NC"����"       TO �ی���ʐe�v
040680*     WHEN 9
040690*         MOVE NC"��"         TO �ی���ʐe�v
040700*     END-EVALUATE.
040710**
016000*     PERFORM ������擾.
040720*     IF ������v NOT = SPACE
040730*         STRING �ی���ʐe�v   DELIMITED BY SPACE
040740*                NC"�i"         DELIMITED BY SIZE
040750*                ������v       DELIMITED BY SPACE
040760*                NC"�j"         DELIMITED BY SIZE
040770*           INTO �ی���ʕҏW�v
040780*         END-STRING
040790*     ELSE
040800*         MOVE �ی���ʐe�v   TO �ی���ʕҏW�v
040810*     END-IF.
040820**
040830*================================================================*
040840* ���t�����擾 SECTION.
040850**================================================================*
040860*     MOVE ZERO  TO ���S�����v   ���t�����v.
040870**
      **/���S���擾�o�f���g���悤�ɕύX/090404
040880**     COMPUTE ���S�����v = ( �A�v�|���S�� / 10 ).
040890**     COMPUTE ���t�����v = 10 - ( �A�v�|���S�� / 10 ).
015800*     MOVE SPACE TO �A���|���S���擾�L�[.
015810*     INITIALIZE �A���|���S���擾�L�[.
015820*     MOVE ��|�{�p�a��N�� TO �A���|�{�p�a��N��.
015830*     MOVE ��|���҃R�[�h   TO �A���|���҃R�[�h.
015840**
015850*     CALL   "HUTANRIT".
015860*     CANCEL "HUTANRIT".
040880*     COMPUTE ���S�����v = ( �A���|���ۖ{�̕��S�� / 10 ).
040890*     COMPUTE ���t�����v = 10 - ( �A���|���ۖ{�̕��S�� / 10 ).
040900**
040910*     EVALUATE ���t�����v
040920*     WHEN 7
040930*        MOVE NC"��"  TO  �V���`�F�b�N�v
040940*     WHEN 8
040950*        MOVE NC"��"  TO  �W���`�F�b�N�v
040960*     WHEN 9
040970*        MOVE NC"��"  TO  �X���`�F�b�N�v
040980*     WHEN 10
040990*        MOVE NC"��"  TO  �P�O���`�F�b�N�v
041000*     END-EVALUATE.
041010**
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
041290*================================================================*
041300 ���Z�E�v�ăZ�b�g SECTION.
041310*================================================================*
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
041490*
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
041500*================================================================*
041510 ���ϔԍ��Z�b�g SECTION.
041520*
041530**************************************************************
041540* �ی��Ҕԍ��ɂ��A���ς̔ԍ����󎚂��邩�A�_���t�ԍ�������
041550**************************************************************
041560** 1.���ϑg���A��
041570     MOVE SPACE  TO  �E�o�t���O.
041580     IF ( �{��|���ϘA�ԍ� NOT = ZERO )
041590** ����(�ی��Ҕԍ�)
041600        IF ( �ی��Ҕԍ��v�q(1:2) = "31" )  OR
041610           ( �ی��Ҕԍ��v�q = "34130021" )
041620*
041630           MOVE  NC"���ϑg���A����"   TO ���ϘA�ԍ����m�v 
041640           MOVE  NC"��"               TO ���ϘA�ԍ��P�ʂm�v 
041650           MOVE  �{��|���ϘA�ԍ�     TO ���ϘA�ԍ��v
041660           IF    (���ϘA�ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
041670                 MOVE SPACE TO  ���ϘA�ԍ��v(1:1)
041680           ELSE
041690                 MOVE "YES" TO  �E�o�t���O
041700           END-IF
041710           IF    (���ϘA�ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
041720                 MOVE SPACE TO  ���ϘA�ԍ��v(2:1)
041730           ELSE
041740                 MOVE "YES" TO  �E�o�t���O
041750           END-IF
041760           IF    (���ϘA�ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
041770                 MOVE SPACE TO  ���ϘA�ԍ��v(3:1)
041780           ELSE
041790                 MOVE "YES" TO  �E�o�t���O
041800           END-IF
041810           IF    (���ϘA�ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
041820                 MOVE SPACE TO  ���ϘA�ԍ��v(4:1)
041830           ELSE
041840                 MOVE "YES" TO  �E�o�t���O
041850           END-IF
041860           IF    (���ϘA�ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
041870                 MOVE SPACE TO  ���ϘA�ԍ��v(5:1)
041880           ELSE
041890                 MOVE "YES" TO  �E�o�t���O
041900           END-IF
041910           IF    (���ϘA�ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
041920                 MOVE SPACE TO  ���ϘA�ԍ��v(6:1)
041930           ELSE
041940                 MOVE "YES" TO  �E�o�t���O
041950           END-IF
041960           MOVE  ���ϘA�ԍ��v�o     TO ���{�p�h�c�v
041970        END-IF
041980     END-IF.
041990*
042000** 2. �n���ϋ��c��
042010     MOVE SPACE  TO  �E�o�t���O.
042020     IF ( �{��|�n���ϘA�ԍ� NOT = ZERO )
042030** ����(�ی��Ҕԍ�)
042040        IF ( �ی��Ҕԍ��v�q(1:2) = "32" OR "33" OR "34" )  AND
042050           ( �ی��Ҕԍ��v�q NOT = "34130021" )
042060*
042070           MOVE  NC"�n���ϋ��c��"     TO ���ϘA�ԍ����m�v 
042080           MOVE  NC"��"               TO ���ϘA�ԍ��P�ʂm�v 
042090           MOVE  �{��|�n���ϘA�ԍ�   TO ���ϘA�ԍ��v
042100           IF    (���ϘA�ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
042110                 MOVE SPACE TO  ���ϘA�ԍ��v(1:1)
042120           ELSE
042130                 MOVE "YES" TO  �E�o�t���O
042140           END-IF
042150           IF    (���ϘA�ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
042160                 MOVE SPACE TO  ���ϘA�ԍ��v(2:1)
042170           ELSE
042180                 MOVE "YES" TO  �E�o�t���O
042190           END-IF
042200           IF    (���ϘA�ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
042210                 MOVE SPACE TO  ���ϘA�ԍ��v(3:1)
042220           ELSE
042230                 MOVE "YES" TO  �E�o�t���O
042240           END-IF
042250           IF    (���ϘA�ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
042260                 MOVE SPACE TO  ���ϘA�ԍ��v(4:1)
042270           ELSE
042280                 MOVE "YES" TO  �E�o�t���O
042290           END-IF
042300           IF    (���ϘA�ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
042310                 MOVE SPACE TO  ���ϘA�ԍ��v(5:1)
042320           ELSE
042330                 MOVE "YES" TO  �E�o�t���O
042340           END-IF
042350           IF    (���ϘA�ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
042360                 MOVE SPACE TO  ���ϘA�ԍ��v(6:1)
042370           ELSE
042380                 MOVE "YES" TO  �E�o�t���O
042390           END-IF
042400           MOVE  ���ϘA�ԍ��v�o     TO ���{�p�h�c�v
042410        END-IF
042420     END-IF.
042430*
042440*================================================================*
042450 ���q���ԍ��Z�b�g SECTION.
042460*
042470     MOVE SPACE  TO  �E�o�t���O.
042480     IF ( �{��|���q���ԍ� NOT = ZERO )
042490           IF �{��|�h�q�ȋ敪 = 1
042500              MOVE  NC"�h�q�ȑ�"      TO ���q���ԍ����m�v 
042510           ELSE
042520              MOVE  NC"�h�q����"      TO ���q���ԍ����m�v 
042530           END-IF
042540*           MOVE  NC"�h�q����"         TO ���q���ԍ����m�v 
042550           MOVE  NC"��"               TO ���q���ԍ��P�ʂm�v 
042560           MOVE  �{��|���q���ԍ�     TO ���q���ԍ��v
042570           IF    (���q���ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
042580                 MOVE SPACE TO  ���q���ԍ��v(1:1)
042590           ELSE
042600                 MOVE "YES" TO  �E�o�t���O
042610           END-IF
042620           IF    (���q���ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
042630                 MOVE SPACE TO  ���q���ԍ��v(2:1)
042640           ELSE
042650                 MOVE "YES" TO  �E�o�t���O
042660           END-IF
042670           IF    (���q���ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
042680                 MOVE SPACE TO  ���q���ԍ��v(3:1)
042690           ELSE
042700                 MOVE "YES" TO  �E�o�t���O
042710           END-IF
042720           IF    (���q���ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
042730                 MOVE SPACE TO  ���q���ԍ��v(4:1)
042740           ELSE
042750                 MOVE "YES" TO  �E�o�t���O
042760           END-IF
042770           IF    (���q���ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
042780                 MOVE SPACE TO  ���q���ԍ��v(5:1)
042790           ELSE
042800                 MOVE "YES" TO  �E�o�t���O
042810           END-IF
042820           IF    (���q���ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
042830                 MOVE SPACE TO  ���q���ԍ��v(6:1)
042840           ELSE
042850                 MOVE "YES" TO  �E�o�t���O
042860           END-IF
042870           MOVE  ���q���ԍ��v�o     TO ���{�p�h�c�v
042880     END-IF.
042890*
042900*================================================================*
042910 �{�p�L�^�e�Ǎ� SECTION.
042920*================================================================*
042930*
042940     READ �{�p�L�^�e NEXT
042950     AT END
042960         MOVE "YES" TO �I���t���O�Q
042970     END-READ.
042980*
042990*----------------------------------------------------------------*
043000*================================================================*
043010 ������� SECTION.
043020*================================================================*
043030     MOVE "YCB6125P" TO  ��`�̖��o.
043040     MOVE "SCREEN"  TO  ���ڌQ���o.
043050     WRITE YCB6125P.
043060***     WRITE ������R�[�h.
043070     PERFORM �G���[�����o.
043080*================================================================*
043090 �G���[�����o SECTION.
043100*
043110     IF �ʒm���o NOT = "00"
043120         DISPLAY NC"���[�G���["              UPON CONS
043130         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
043140         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
043150         DISPLAY NC"�g������o�F" �g������o UPON CONS
043160         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
043170                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
043180         ACCEPT  �L�[���� FROM CONS
043190         PERFORM �t�@�C����
043200         MOVE 99  TO PROGRAM-STATUS
043210         EXIT PROGRAM
043220     END-IF.
043230*
043240*=== �I������ ===================================================*
043250*================================================================*
043260 ��f�҈���敪�X�V SECTION.
043270*================================================================*
043280** //  ��f�ҏ��e�̈���敪�ɂP���Z�b�g���A�X�V����B//  
043290*
043300     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
043310     MOVE �{�p�N�v�q         TO ��|�{�p�N.
043320     MOVE �{�p���v�q         TO ��|�{�p��.
043330     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
043340     READ ��f�ҏ��e
043350     NOT INVALID KEY
043360         MOVE  1  TO  ��|���Z����敪
043370         REWRITE  ��|���R�[�h
043380         END-REWRITE
043390         IF ( ��ԃL�[ NOT = "00" )
043400            MOVE NC"��f��" TO �t�@�C����
043410            PERFORM �G���[�\��
043420         END-IF
043430     END-READ.
043440*
043450*================================================================*
043460 �I������ SECTION.
043470*================================================================*
043480     PERFORM �t�@�C����.
043490*
043500*================================================================*
043510 �t�@�C���� SECTION.
043520*
043530     CLOSE �����}�X�^     ���̃}�X�^       ���Z�v�g�e     �o�߃}�X�^
043540           ������}�X�^ �{�p�����}�X�^ ����}�X�^
043550           �ی��҃}�X�^   �h�c�Ǘ��}�X�^   �s�����}�X�^
043560           ��f�ҏ��e   �{�p�L�^�e       �����f�[�^�e   ���������e
043570           ��ƃt�@�C���Q.
043580     CLOSE ����t�@�C��.
043590*
043600*================================================================*
043610*================================================================*
043620 �G���[�\�� SECTION.
043630*
043640     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
043650     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
043660     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
043670     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
043680                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
043690     ACCEPT  �L�[���� FROM CONS
043700     PERFORM �t�@�C����.
043710     EXIT PROGRAM.
043720*
043730*================================================================*
043750 �e�X�g�󎚏��� SECTION.
044930*
           MOVE ALL "9" TO
           �s���{���ԍ� �{�p�� �{�p�N ���ҔN ���Ҍ� ���ғ� �J�n�N�P �J�n���P �J�n���P �I���N�P 
           �I�����P �I�����P �����N�P �������P �������P �����N�P �������P �������P �������P 
           �J�n�N�Q �J�n���Q �J�n���Q �I���N�Q �I�����Q �I�����Q �����N�Q �������Q �������Q 
           �����N�Q �������Q �������Q �������Q �J�n�N�R �J�n���R �J�n���R �I���N�R �I�����R 
           �I�����R �����N�R �������R �������R �����N�R �������R �������R �������R �J�n�N�S 
           �J�n���S �J�n���S �I���N�S �I�����S �I�����S �����N�S �������S �������S �����N�S 
           �������S �������S �������S �J�n�N�T �J�n���T �J�n���T �I���N�T �I�����T �I�����T 
           �����N�T �������T �������T �����N�T �������T �������T �������T ������ ���������k�� 
           ���Ë��� �Č��� �������q���Z�� ���É� ���×� �������Z�� �{�p���񋟗� ���É��Z�� 
           �������Z�� �������Z�� �������Z���  ���v ���񏈒u�����v ��ÒP���P 
           ���񏈒u��(1) ���񏈒u��(2) ���񏈒u��(3) ���񏈒u��(4) ���񏈒u��(5)
           ��É񐔂P ��×��P ��㪖@�񐔂P ��㪖@���P ��㪖@�񐔂P ��㪖@���P �d�É񐔂P 
           �d�×��P ���v�P �����������P ���������v�P ��ÒP���Q ��É񐔂Q ��×��Q ��㪖@�񐔂Q 
           ��㪖@���Q ��㪖@�񐔂Q ��㪖@���Q �d�É񐔂Q �d�×��Q ���v�Q �����������Q 
           ���������v�Q ��ÒP���R�W ��É񐔂R�W ��×��R�W ��㪖@�񐔂R�W ��㪖@���R�W 
           ��㪖@�񐔂R�W ��㪖@���R�W �d�É񐔂R�W �d�×��R�W ���v�R�W �����ʍ����v�R�W 
           �����������R�W ���������v�R�W �����J�n���R�O �����J�n���R�O ��ÒP���R�O ��É񐔂R�O 
           ��×��R�O ��㪖@�񐔂R�O ��㪖@���R�O ��㪖@�񐔂R�O ��㪖@���R�O �d�É񐔂R�O 
           �d�×��R�O ���v�R�O �����������R�O ���������v�R�O �����J�n���S�W �����J�n���S�W 
           ��ÒP���S�W ��É񐔂S�W ��×��S�W ��㪖@�񐔂S�W ��㪖@���S�W ��㪖@�񐔂S�W 
           ��㪖@���S�W �d�É񐔂S�W �d�×��S�W ���v�S�W �����ʍ����v�S�W �����������S�W 
           ���������v�S�W �����J�n���S�O �����J�n���S�O ��ÒP���S�O ��É񐔂S�O ��×��S�O 
           ��㪖@�񐔂S�O ��㪖@���S�O ��㪖@�񐔂S�O ��㪖@���S�O �d�É񐔂S�O �d�×��S�O 
           ���v�S�O �����������S�O ���������v�S�O ���v �ꕔ���S�� ���S���� �������z 
           �󗝔N �󗝌� �󗝓� �ϔC�N �ϔC�� �ϔC��
           .
           MOVE ALL "X" TO
           ���{�p�h�c �ی��Ҕԍ� ����S�Ҕԍ� �󋋎Ҕԍ� 
           ���Z�@�֖��P ���Z�@�֖��Q ���Z�@�֖��R ���Z�@�֖��S �x�X���P �x�X���Q �x�X���R 
           �x�X���S �������`�l�J�i �������`�l �_���t�ԍ� �����ԍ� �{�p���X�֔ԍ��P  
           �{�p���X�֔ԍ��Q �{�p���Z���P �{�p���Z���Q �{�p���d�b�ԍ� ��\�҃J�i
           ���������P ���������Q ���������R ���������S ���������T ���������U
      *
           MOVE ALL NC"�m" TO
           �������P �������Q �������R �������S �������T �o�ߗ���(1) 
           �o�ߗ���(2) �o�ߗ���(3) �o�ߗ���(4) �o�ߗ���(5) �K�p�P �K�p�Q
           .
           MOVE ALL "��" TO
           �������R���P �������R���Q �������R���R �������R���S �������R���T �������R���U 
           �ڍ��@�� ��ی��Ҏ��� ���Ҏ��� ��\�Җ� 
           .
      *
           MOVE NC"��" TO
           ���ʃ`�F�b�N �U���`�F�b�N �����`�F�b�N �{�X�`�F�b�N �x�X�`�F�b�N �{�x���`�F�b�N 
           ��s�`�F�b�N ���Ƀ`�F�b�N �_���`�F�b�N �[��`�F�b�N ���ԊO�`�F�b�N 
           �x���`�F�b�N �Œ藿�`�F�b�N �������`�F�b�N �{�×��`�F�b�N ��ԃ`�F�b�N �\���J��`�F�b�N 
           ��H�`�F�b�N ��`�F�b�N ���`�F�b�N ���`�F�b�N �����`�F�b�N�P ���~�`�F�b�N�P �]��`�F�b�N�P 
           �����`�F�b�N�Q ���~�`�F�b�N�Q �]��`�F�b�N�Q �����`�F�b�N�R ���~�`�F�b�N�R �]��`�F�b�N�R 
           �����`�F�b�N�S ���~�`�F�b�N�S �]��`�F�b�N�S �����`�F�b�N�T ���~�`�F�b�N�T �]��`�F�b�N�T 
           �V�K�`�F�b�N �p���`�F�b�N �j�`�F�b�N �����`�F�b�N �吳�`�F�b�N ���`�F�b�N ���a�`�F�b�N 
           �����`�F�b�N �P�ƃ`�F�b�N �{�l�`�F�b�N ����`�F�b�N ���σ`�F�b�N ���`�F�b�N �Еۃ`�F�b�N 
           �g���`�F�b�N �P�O���`�F�b�N �X���`�F�b�N �Q���`�F�b�N �U�΃`�F�b�N �W���`�F�b�N �V���`�F�b�N 
           ����`�F�b�N �ސE�`�F�b�N ���ۃ`�F�b�N �Ƒ��`�F�b�N ���V�`�F�b�N
           .
044940*================================================================*
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
043420 ���Z�v�g���я��擾 SECTION.
043430*
043440     MOVE �{�p�a��v�q       TO ��Q�|�{�p�a��.
043450     MOVE �{�p�N�v�q         TO ��Q�|�{�p�N.
043460     MOVE �{�p���v�q         TO ��Q�|�{�p��.
043470     MOVE ���҃R�[�h�v�q     TO ��Q�|���҃R�[�h.
043480     MOVE �ی���ʂv�q       TO ��Q�|�ی����.
043490     READ ��ƃt�@�C���Q
043500     NOT INVALID KEY
043510          MOVE ��Q�|����    TO ���Ԃv
043520     END-READ.
043530*
043540*================================================================*
044950******************************************************************
044960 END PROGRAM YCB6125.
044970******************************************************************
