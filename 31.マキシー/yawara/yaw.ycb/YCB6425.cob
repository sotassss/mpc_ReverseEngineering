000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCB6425.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*     �����_���t���� �������Z�v�g����i�_+����޳�ޔŁj
000100*         MED = YAW610 YCB6425P
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
000650     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS  �ہ|�ی����
000690                                                          �ہ|�ی��Ҕԍ�
000700* �����́A�L�[���ڂ̕ی��Җ��̂�ی��҃J�i�ɂ���
000710                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000720                                                          �ہ|�ی��Җ���
000730                                                          �ہ|�ی��Ҕԍ�
000740                             FILE STATUS              IS  ��ԃL�[
000750                             LOCK        MODE         IS  AUTOMATIC.
000760     SELECT  ������}�X�^    ASSIGN      TO        SEIKYUSL
000770                             ORGANIZATION             IS  INDEXED
000780                             ACCESS MODE              IS  DYNAMIC
000790                             RECORD KEY               IS  ����|�ی����
000800                                                          ����|�ی��Ҕԍ�
000810                             FILE STATUS              IS  ��ԃL�[
000820                             LOCK    MODE             IS  AUTOMATIC.
000830     SELECT  �h�c�Ǘ��}�X�^    ASSIGN      TO      IDKANRL
000840                             ORGANIZATION             IS  INDEXED
000850                             ACCESS MODE              IS  DYNAMIC
000860                             RECORD KEY               IS  �h�c�ǁ|�h�c�敪
000870                                                          �h�c�ǁ|�{�p���ԍ�
000880                                                          �h�c�ǁ|�ی����
000890                                                          �h�c�ǁ|�ی��Ҕԍ�
000900                             ALTERNATE RECORD KEY     IS  �h�c�ǁ|�{�p�h�c�ԍ�
000910                                                          �h�c�ǁ|�h�c�敪
000920                                                          �h�c�ǁ|�{�p���ԍ�
000930                                                          �h�c�ǁ|�ی����
000940                                                          �h�c�ǁ|�ی��Ҕԍ�
000950                             FILE STATUS              IS  ��ԃL�[
000960                             LOCK        MODE         IS  AUTOMATIC.
000970     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000980                             ORGANIZATION             IS  INDEXED
000990                             ACCESS MODE              IS  DYNAMIC
001000                             RECORD KEY               IS  �s�|������
001010                                                          �s�|�s�����ԍ�
001020                             ALTERNATE RECORD KEY     IS  �s�|������
001030                                                          �s�|�s��������
001040                                                          �s�|�s�����ԍ�
001050                             FILE STATUS              IS  ��ԃL�[
001060                             LOCK        MODE         IS  AUTOMATIC.
001070     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
001080                             ORGANIZATION             IS  INDEXED
001090                             ACCESS MODE              IS  DYNAMIC
001100                             RECORD KEY               IS  ��|�{�p�a��N��
001110                                                          ��|���҃R�[�h
001120                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001130                                                          ��|���҃J�i
001140                                                          ��|���҃R�[�h
001150                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
001160                                                          ��|�{�p�a��N��
001170                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001180                                                          ��|�ی����
001190                                                          ��|�ی��Ҕԍ�
001200                                                          ��|���҃R�[�h
001210                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001220                                                          ��|������
001230                                                          ��|��p���S�Ҕԍ�
001240                                                          ��|���҃R�[�h
001250                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001260                                                          ��|�������
001270                                                          ��|��p���S�Ҕԍ�����
001280                                                          ��|���҃R�[�h
001290                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
001300                                                          ��|�{�p�a��N��
001310                                                          ��|���҃R�[�h
001320                             FILE STATUS              IS  ��ԃL�[
001330                             LOCK        MODE         IS  AUTOMATIC.
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
001340     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  �{�L�|�{�p�a��N����
001380                                                          �{�L�|���҃R�[�h
001390                             ALTERNATE RECORD KEY     IS  �{�L�|���҃R�[�h
001400                                                          �{�L�|�{�p�a��N����
001410                             FILE STATUS              IS  ��ԃL�[
001420                             LOCK        MODE         IS  AUTOMATIC.
001430     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
001440                             ORGANIZATION             IS  INDEXED
001450                             ACCESS MODE              IS  DYNAMIC
001460                             RECORD KEY               IS  ���|�{�p�a��N��
001470                                                          ���|���҃R�[�h
001480                             ALTERNATE RECORD KEY     IS  ���|���҃R�[�h
001490                                                          ���|�{�p�a��N��
001500                             FILE STATUS              IS  ��ԃL�[
001510                             LOCK        MODE         IS  AUTOMATIC.
001520     SELECT  ���������e      ASSIGN      TO        HUGEINL
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
001550                             RECORD KEY               IS  �����|�敪�R�[�h
001560                                                          �����|���������R�[�h
001570                             FILE STATUS              IS  ��ԃL�[
001580                             LOCK        MODE         IS  AUTOMATIC.
001860* ���я��󎚗p
001870     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001880                             ORGANIZATION             IS  INDEXED
001890                             ACCESS                   IS  DYNAMIC
001900                             RECORD      KEY          IS  ��Q�|�{�p�a��N��
001910                                                          ��Q�|���҃R�[�h
001920                                                          ��Q�|�ی����
001930                             FILE        STATUS       IS  ��ԃL�[
001940                             LOCK        MODE         IS  AUTOMATIC.
001730     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
001740                             SYMBOLIC    DESTINATION  IS "PRT"
001750                             FORMAT                   IS  ��`�̖��o
001760                             GROUP                    IS  ���ڌQ���o
001770                             PROCESSING  MODE         IS  ������ʂo
001780                             UNIT        CONTROL      IS  �g������o
001790                             FILE        STATUS       IS  �ʒm���o.
001800******************************************************************
001810*                      DATA DIVISION                             *
001820******************************************************************
001830 DATA                    DIVISION.
001840 FILE                    SECTION.
001850*                           �m�q�k��  �P�Q�W�n
001860 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001870     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001880*                           �m�q�k��  �P�Q�W�n
001890 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001900     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001940*                           �m�q�k��  �P�Q�W�n
001950 FD  �o�߃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001960     COPY KEIKA           OF  XFDLIB  JOINING   �o   AS  PREFIX.
001970*                           �m�q�k��  �Q�T�U�n
001980 FD  ������}�X�^      BLOCK   CONTAINS   1   RECORDS.
001990     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002000*                           �m�q�k��  �P�Q�W�n
002010 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
002020     COPY SEJOHO          OF  XFDLIB  JOINING   �{�� AS  PREFIX.
002150*                           �m�q�k��  �U�S�O�n
002160 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
002170     COPY KAIJOHO         OF  XFDLIB  JOINING   ��� AS  PREFIX.
002030*                           �m�q�k��  �R�Q�O�n
002040 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
002050     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002060*                           �m�q�k��  �P�Q�W�n
002070 FD  ������}�X�^        BLOCK   CONTAINS   1   RECORDS.
002080     COPY SEIKYUS         OF  XFDLIB  JOINING   ���� AS  PREFIX.
002090*                           �m�q�k��  �P�Q�W�n
002100 FD  �h�c�Ǘ��}�X�^      BLOCK   CONTAINS   1   RECORDS.
002110     COPY IDKANR          OF  XFDLIB  JOINING   �h�c�� AS  PREFIX.
002120*                           �m�q�k��  �Q�T�U�n
002130 FD  �s�����}�X�^        BLOCK   CONTAINS   1   RECORDS.
002140     COPY SITYOSN         OF  XFDLIB  JOINING   �s   AS  PREFIX.
002150*                           �m�q�k��  �R�Q�O�n
002160 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
002170     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002560*                          �m�q�k��  1024�n
000340 FD  ��f�ҏ��Q�e        BLOCK   CONTAINS   1   RECORDS.
000350     COPY JUSINJ2          OF  XFDLIB  JOINING   ��Q   AS  PREFIX.
002180*                           �m�q�k��  �Q�T�U�n
002190 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
002200     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
002210*                           �m�q�k��  �P�Q�W�n
002220 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
002230     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002240*                           �m�q�k��  �P�Q�W�n
002250 FD  ���������e         BLOCK   CONTAINS    1   RECORDS.
002260     COPY HUGEIN          OF  XFDLIB  JOINING   ���� AS  PREFIX.
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
002300*
002310 FD  ����t�@�C��.
002320     COPY YCB6425P         OF  XMDLIB.
002330*----------------------------------------------------------------*
002340******************************************************************
002350*                WORKING-STORAGE SECTION                         *
002360******************************************************************
002370 WORKING-STORAGE         SECTION.
002380 01 �L�[����                           PIC X     VALUE SPACE.
002390 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
002400 01 �I���t���O                         PIC X(3)  VALUE SPACE.
002410 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
002420 01 �t�@�C����                         PIC N(6)  VALUE SPACE.
002430 01 �O�a��v                           PIC 9     VALUE ZERO.
001363 01 �S�p��                           PIC X(2)  VALUE X"8140".
001364 01 ���p��                           PIC X(2)  VALUE X"2020".
002440*
002450*--- ����}�X�^�ޔ� ---*
002460 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
002470*
002480** ���������E�������R����敪�p
002490 01 ������������敪�v                 PIC 9     VALUE ZERO.
002500 01 �������R����敪�v                 PIC 9     VALUE ZERO.
002510*
002520** ���Z���i�̓��t�敪�p (0:�ŏI�ʉ@���A1:�������A9:�󎚂Ȃ�)
002530 01 ���Z�v�g���t�敪�v                 PIC 9     VALUE ZERO.
002540 01 ���Z�v�g���ғ��t�敪�v             PIC 9     VALUE ZERO.
002550*
002560*--- �J�E���^ ---*
002570 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002580*
002590*--- �����f�[�^�擾�p ---*
002600 01 �������̂v                         PIC N(10) VALUE SPACE.
002610 01 ���ʖ��̂v                         PIC N(20) VALUE SPACE.
002620 01 ���ʒ��v                           PIC 9(2)  VALUE 1.
002630 01 �o�ߕ��ʂv                         PIC N(1)  VALUE SPACE.
002640*
002650** �}�Ԕ���p
002660 01 �J�n�f�Ó��蓮�敪�v               PIC 9     VALUE ZERO.
002670*
002680* ������������敪
002690 01 ���Z������������敪�v             PIC 9     VALUE ZERO.
002580 01 ���Z�������R����敪�v             PIC 9    VALUE ZERO.
002700*
002710*--- �{�p�L�^�擾�p ---*
002720 01 �����Č��t���O                     PIC X(3)  VALUE SPACE.
002730 01 �O���t���O                         PIC X(3)  VALUE SPACE.
002740*
002750 01 �I���N�����v�s.
002760    03 �I���N�v�s                      PIC 9(2)  VALUE ZERO.
002770    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
002780    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
002790*
002800** �O������p
002810 01 �v�Z�N�����v.
002820    03 �v�Z�a��v                      PIC 9(1)  VALUE ZERO.
002830    03 �v�Z�N�v                        PIC S9(2) VALUE ZERO.
002840    03 �v�Z���v                        PIC S9(2) VALUE ZERO.
002850    03 �v�Z���v                        PIC S9(2) VALUE ZERO.
002860 01 �J�n�N�����Q�v.
002870    03 �J�n�a��Q�v                    PIC 9(1)  VALUE ZERO.
002880    03 �J�n�N�Q�v                      PIC 9(2)  VALUE ZERO.
002890    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
002900    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
002910    03 �J�n����N�v                    PIC S9(4) VALUE ZERO.
002920 01 �I���N�����Q�v.
002930    03 �I���a��Q�v                    PIC 9(1)  VALUE ZERO.
002940    03 �I���N�Q�v                      PIC 9(2)  VALUE ZERO.
002950    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
002960    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
002970    03 �I������N�v                    PIC S9(4) VALUE ZERO.
002980*
002990*--- �������ޔ�p ---*
003000 01 �����t���O                         PIC X(3)  VALUE SPACE.
003010*
003020 01 �����N�����v�s.
003030    03 �����a��v�s                    PIC 9     VALUE ZERO.
003040    03 �����N�v�s                      PIC 9(2)  VALUE ZERO.
003050    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003060    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003070*
003080*--- �������Z�����p ---*
003090 01 �������Z�v�s.
003100    03 �������Z�J�E���g                PIC 9     VALUE ZERO.
003110    03 �ԍ��J�E���^                    PIC 9     VALUE ZERO.
003120    03 �������Z�W�c�v�s  OCCURS 3.
003130       05 �������Z�敪�v�s             PIC 9     VALUE ZERO.
003140       05 �������Z���v�s               PIC 9(2)  VALUE ZERO.
003150       05 �������Z���v�s               PIC 9(2)  VALUE ZERO.
003160    03 �������Z�W�c�m�v  OCCURS 3.
003170       05 ���Z��؂v                   PIC N(1)  VALUE SPACE.
003180       05 ���Z���e�v                   PIC N(3)  VALUE SPACE.
003190       05 �������Z���m�v�P             PIC N(1)  VALUE SPACE.
003200       05 �������Z���m�v�Q             PIC N(1)  VALUE SPACE.
003210       05 ���Œ�v                     PIC N(1)  VALUE SPACE.
003220       05 �������Z���m�v�P             PIC N(1)  VALUE SPACE.
003230       05 �������Z���m�v�Q             PIC N(1)  VALUE SPACE.
003240       05 ���Œ�v                     PIC N(1)  VALUE SPACE.
003250    03 �������Z�����P�v                PIC N(10) VALUE SPACE.
003260    03 �������Z�����Q�v                PIC N(10) VALUE SPACE.
003270    03 �������Z�����R�v                PIC N(10) VALUE SPACE.
003070    03 �������Z��؂v                  PIC X     VALUE SPACE.
003080    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003090    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003280*
003290** ���������{��ϊ�
003300 01 �����v                             PIC 9(2).
003310 01 �����q REDEFINES �����v.
003320    03 �����v�P                        PIC X(1).
003330    03 �����v�Q                        PIC X(1).
003340*
003350 01 �����ԍ��v                         PIC 9.
003360 01 �����ԍ��q REDEFINES �����ԍ��v.
003370    03 �����ԍ��v�P                    PIC X.
003380*
003390 01 �S�p�����ԍ��v                     PIC N.
003400 01 �S�p�����ԍ��q REDEFINES �S�p�����ԍ��v.
003410    03 �S�p�����ԍ��v�P                PIC X(2).
003420*
003430*--- ���������p ---*
003440 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
003450 01 �J�E���^�Q                         PIC 9(2)  VALUE ZERO.
003460 01 ���������v�s.
003470    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
003480    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
003490    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
003500    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
003510    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
003520    03 ���������i���o�[�v�s.
003530       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
003540    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
003550 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
003560 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
003570 01 ���������s�a�k.
003580    03 ���������R�[�h�s�a�k            OCCURS 9.
003590       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
003600       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
003610       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
003620 01 �����������e�v.
003630    03 �����������e�����v              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 �����������e�����w�v.
003630       05 �����������e�P�w�v           PIC X(80)  VALUE SPACE.
003640       05 �����������e�Q�w�v           PIC X(80)  VALUE SPACE.
003640       05 �����������e�R�w�v           PIC X(80)  VALUE SPACE.
003650       05 �����������e�S�w�v           PIC X(78)  VALUE SPACE.
003800 01 ���������v�o                       PIC N(225) VALUE SPACE.
       01 ���������v�q�o.
003810    03 ���������v�q                    PIC N(45) OCCURS 5 VALUE SPACE.
003680*
003690*--- �ϔC�N�����p ---*
003700 01 �󗝔N�����v.
003710    03 �󗝔N�v                        PIC 9(2)  VALUE ZERO.
003720    03 �󗝌��v                        PIC 9(2)  VALUE ZERO.
003730    03 �󗝓��v                        PIC 9(2)  VALUE ZERO.
003740 01 �ŏI�ʉ@�N�����v.
003750    03 �ŏI�ʉ@�N�v                    PIC 9(2)  VALUE ZERO.
003760    03 �ŏI�ʉ@���v                    PIC 9(2)  VALUE ZERO.
003770    03 �ŏI�ʉ@���v                    PIC 9(2)  VALUE ZERO.
003780** �������p
003790 01 �{�p����N�v                       PIC 9(4)  VALUE ZERO.
003800 01 ���v                               PIC 9(3)  VALUE ZERO.
003810 01 �]�v                               PIC 9(3)  VALUE ZERO.
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
003820*
003830*--- ���S���t�����p ---*
003840 01 ���S�����v                         PIC 9(2)  VALUE ZERO.
003850 01 ���t�����v                         PIC 9(2)  VALUE ZERO.
003860 01 ���S���v                           PIC 9(3)  VALUE ZERO.
003870*
003880*--- ���Z�v�g�񐔗p ---*
003890 01 �񐔂v                             PIC 9(2)  VALUE ZERO.
003900*
003910 01 �ŏ��J�n�a��N���v.
003920    03 �ŏ��J�n�a��v                  PIC 9(1)  VALUE ZERO.
003930    03 �ŏ��J�n�N�v                    PIC 9(2)  VALUE ZERO.
003940    03 �ŏ��J�n���v                    PIC 9(2)  VALUE ZERO.
003950*
003960*--- �{�p�h�c�p ---*
003970 01 �{�p�h�c�Œ�v                     PIC X(14) VALUE "�{�p�@�֔ԍ��F".
      *
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
       01 �������q�b�l                       PIC X(140) VALUE SPACE.
       01 �^����Âb�l                       PIC X(68)  VALUE SPACE.
003980*
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
004410*
004420****************
004430* �A�����ڑҔ� *
004440****************
004450*    ************
004460*    * ����L�[ *
004470*    ************
004480 01 �Ώۃf�[�^�v�q.
004490    03 �{�p�a��N���v�q.
004500       05 �{�p�a��v�q                 PIC 9(1)  VALUE ZERO.
004510       05 �{�p�N�v�q                   PIC 9(2)  VALUE ZERO.
004520       05 �{�p���v�q                   PIC 9(2)  VALUE ZERO.
004530    03 �ی���ʂv�q                    PIC 9(2)  VALUE ZERO.
004540    03 �ی��Ҕԍ��v�q                  PIC X(10) VALUE SPACE.
004550    03 �����ʂv�q                    PIC 9(2)  VALUE ZERO.
004560    03 ��p���S�Ҕԍ��v�q              PIC X(10) VALUE SPACE.
004570    03 ������ʂv�q                    PIC 9(2)  VALUE ZERO.
004580    03 ��p���S�Ҕԍ������v�q          PIC X(10) VALUE SPACE.
004590    03 �{�l�Ƒ��敪�v�q                PIC 9(1)  VALUE ZERO.
004600    03 ���҃J�i�v�q                    PIC X(50) VALUE SPACE.
004610    03 ���҃R�[�h�v�q.
004620       05 ���Ҕԍ��v�q                 PIC 9(6)  VALUE ZERO.
004630       05 �}�Ԃv�q                     PIC X(1)  VALUE SPACE.
004640*    ************
004650*    * ������� *
004660*    ************
004670*    �����̗���
004680***********************
004690 01 �����P�v�q.
004700   03 �����v�q.
004710      05 ���S�����v�q                  PIC 9(3)  VALUE ZERO.
004720      05 �������v�q                    PIC 9(5)  VALUE ZERO.
004730      05 �������Z���v�q                PIC 9(5)  VALUE ZERO.
         03 ���k���v�q                       PIC 9(4)  VALUE ZERO.
004740   03 �Č����v�q                       PIC 9(5)  VALUE ZERO.
004750   03 ���Âv�q.
004760      05 ���Ë����v�q                  PIC 9(2)V9 VALUE ZERO.
004770      05 ���É񐔂v�q                  PIC 9(2)  VALUE ZERO.
004780      05 ���×��v�q                    PIC 9(5)  VALUE ZERO.
004790      05 ���É��Z���v�q                PIC 9(5)  VALUE ZERO.
004800   03 �������q���Z���v�q               PIC 9(5)  VALUE ZERO.
004810   03 �{�p���񋟗��v�q               PIC 9(5)  VALUE ZERO.
004820   03 ���v�v�q                         PIC 9(6)  VALUE ZERO.
004830   03 �ꕔ���S���v�q                   PIC 9(6)  VALUE ZERO.
004840   03 �������z�v�q                     PIC 9(6)  VALUE ZERO.
004850   03 ���t�����v�q                     PIC 9(1)  VALUE ZERO.
004860   03 �󋋎ҕ��S�z�v�q                 PIC 9(6)  VALUE ZERO.
004870   03 �����������z�v�q                 PIC 9(6)  VALUE ZERO.
004880*
004890* �������ʖ��̗���
004900***********************
004910 01 �����Q�v�q.
004920   03 ���񏈒u�v�q    OCCURS   9.
004930      05 ���񏈒u���v�q                PIC 9(5)  VALUE ZERO.
004940*
004950* �������̗���
004960***********************
004970 01 �����R�v�q.
004980**********
004990* �P���� *
005000**********
005010   03 ���ʂP�v�q.
005020      05 ��ÂP�v�q.
005030         07 ��ÒP���P�v�q             PIC 9(4)  VALUE ZERO.
005040         07 ��É񐔂P�v�q             PIC 9(2)  VALUE ZERO.
005050         07 ��×��P�v�q               PIC 9(5)  VALUE ZERO.
005060      05 ��㪖@�P�v�q.
005070         07 ��㪖@�񐔂P�v�q           PIC 9(2)  VALUE ZERO.
005080         07 ��㪖@���P�v�q             PIC 9(4)  VALUE ZERO.
005090      05 ��㪖@�P�v�q.
005100         07 ��㪖@�񐔂P�v�q           PIC 9(2)  VALUE ZERO.
005110         07 ��㪖@���P�v�q             PIC 9(4)  VALUE ZERO.
005120      05 �d�ÂP�v�q.
005130         07 �d�É񐔂P�v�q             PIC 9(2)  VALUE ZERO.
005140         07 �d�×��P�v�q               PIC 9(4)  VALUE ZERO.
005150      05 ���v�P�v�q                    PIC 9(6)  VALUE ZERO.
005160      05 �����������P�v�q              PIC 9(3)  VALUE ZERO.
005170      05 ���������v�P�v�q              PIC 9(6)  VALUE ZERO.
005180**********
005190* �Q���� *
005200**********
005210   03 ���ʂQ�v�q.
005220      05 ��ÂQ�v�q.
005230         07 ��ÒP���Q�v�q             PIC 9(4)  VALUE ZERO.
005240         07 ��É񐔂Q�v�q             PIC 9(2)  VALUE ZERO.
005250         07 ��×��Q�v�q               PIC 9(5)  VALUE ZERO.
005260      05 ��㪖@�Q�v�q.
005270         07 ��㪖@�񐔂Q�v�q           PIC 9(2)  VALUE ZERO.
005280         07 ��㪖@���Q�v�q             PIC 9(4)  VALUE ZERO.
005290      05 ��㪖@�Q�v�q.
005300         07 ��㪖@�񐔂Q�v�q           PIC 9(2)  VALUE ZERO.
005310         07 ��㪖@���Q�v�q             PIC 9(4)  VALUE ZERO.
005320      05 �d�ÂQ�v�q.
005330         07 �d�É񐔂Q�v�q             PIC 9(2)  VALUE ZERO.
005340         07 �d�×��Q�v�q               PIC 9(4)  VALUE ZERO.
005350      05 ���v�Q�v�q                    PIC 9(6)  VALUE ZERO.
005360      05 �����������Q�v�q              PIC 9(3)  VALUE ZERO.
005370      05 ���������v�Q�v�q              PIC 9(6)  VALUE ZERO.
005380******************
005390* �R���ʁ^�W�� *
005400******************
005410   03 ���ʂR�W�v�q.
005420      05 ��ÂR�W�v�q.
005430         07 ��ÒP���R�W�v�q           PIC 9(4)  VALUE ZERO.
005440         07 ��É񐔂R�W�v�q           PIC 9(2)  VALUE ZERO.
005450         07 ��×��R�W�v�q             PIC 9(5)  VALUE ZERO.
005460      05 ��㪖@�R�W�v�q.
005470         07 ��㪖@�񐔂R�W�v�q         PIC 9(2)  VALUE ZERO.
005480         07 ��㪖@���R�W�v�q           PIC 9(4)  VALUE ZERO.
005490      05 ��㪖@�R�W�v�q.
005500         07 ��㪖@�񐔂R�W�v�q         PIC 9(2)  VALUE ZERO.
005510         07 ��㪖@���R�W�v�q           PIC 9(4)  VALUE ZERO.
005520      05 �d�ÂR�W�v�q.
005530         07 �d�É񐔂R�W�v�q           PIC 9(2)  VALUE ZERO.
005540         07 �d�×��R�W�v�q             PIC 9(4)  VALUE ZERO.
005550      05 ���v�R�W�v�q                  PIC 9(6)  VALUE ZERO.
005560      05 �����ʍ����v�R�W�v�q          PIC 9(6)  VALUE ZERO.
005570      05 �����������R�W�v�q            PIC 9(3)  VALUE ZERO.
005580      05 ���������v�R�W�v�q            PIC 9(6)  VALUE ZERO.
005590******************
005600* �R���ʁ^�P�O�� *
005610******************
005620   03 ���ʂR�O�v�q.
005630      05 �����J�n�����R�O�v�q.
005640         07 �����J�n���R�O�v�q         PIC 9(2)  VALUE ZERO.
005650         07 �����J�n���R�O�v�q         PIC 9(2)  VALUE ZERO.
005660      05 ��ÂR�O�v�q.
005670         07 ��ÒP���R�O�v�q           PIC 9(4)  VALUE ZERO.
005680         07 ��É񐔂R�O�v�q           PIC 9(2)  VALUE ZERO.
005690         07 ��×��R�O�v�q             PIC 9(5)  VALUE ZERO.
005700      05 ��㪖@�R�O�v�q.
005710         07 ��㪖@�񐔂R�O�v�q         PIC 9(2)  VALUE ZERO.
005720         07 ��㪖@���R�O�v�q           PIC 9(4)  VALUE ZERO.
005730      05 ��㪖@�R�O�v�q.
005740         07 ��㪖@�񐔂R�O�v�q         PIC 9(2)  VALUE ZERO.
005750         07 ��㪖@���R�O�v�q           PIC 9(4)  VALUE ZERO.
005760      05 �d�ÂR�O�v�q.
005770         07 �d�É񐔂R�O�v�q           PIC 9(2)  VALUE ZERO.
005780         07 �d�×��R�O�v�q             PIC 9(4)  VALUE ZERO.
005790      05 ���v�R�O�v�q                  PIC 9(6)  VALUE ZERO.
005800      05 �����������R�O�v�q            PIC 9(3)  VALUE ZERO.
005810      05 ���������v�R�O�v�q            PIC 9(6)  VALUE ZERO.
005820****************
005830* �S���ʁ^�T�� *
005840****************
005850   03 ���ʂS�T�v�q.
005860      05 ��ÂS�T�v�q.
005870         07 ��ÒP���S�T�v�q           PIC 9(4)  VALUE ZERO.
005880         07 ��É񐔂S�T�v�q           PIC 9(2)  VALUE ZERO.
005890         07 ��×��S�T�v�q             PIC 9(5)  VALUE ZERO.
005900      05 ��㪖@�S�T�v�q.
005910         07 ��㪖@�񐔂S�T�v�q         PIC 9(2)  VALUE ZERO.
005920         07 ��㪖@���S�T�v�q           PIC 9(4)  VALUE ZERO.
005930      05 ��㪖@�S�T�v�q.
005940         07 ��㪖@�񐔂S�T�v�q         PIC 9(2)  VALUE ZERO.
005950         07 ��㪖@���S�T�v�q           PIC 9(4)  VALUE ZERO.
005960      05 �d�ÂS�T�v�q.
005970         07 �d�É񐔂S�T�v�q           PIC 9(2)  VALUE ZERO.
005980         07 �d�×��S�T�v�q             PIC 9(4)  VALUE ZERO.
005990      05 ���v�S�T�v�q                  PIC 9(6)  VALUE ZERO.
006000      05 �����ʍ����v�S�T�v�q          PIC 9(6)  VALUE ZERO.
006010      05 �����������S�T�v�q            PIC 9(3)  VALUE ZERO.
006020      05 ���������v�S�T�v�q            PIC 9(6)  VALUE ZERO.
006030****************
006040* �S���ʁ^�W�� *
006050****************
006060   03 ���ʂS�W�v�q.
006070      05 �����J�n�����S�W�v�q.
006080         07 �����J�n���S�W�v�q         PIC 9(2)  VALUE ZERO.
006090         07 �����J�n���S�W�v�q         PIC 9(2)  VALUE ZERO.
006100      05 ��ÂS�W�v�q.
006110         07 ��ÒP���S�W�v�q           PIC 9(4)  VALUE ZERO.
006120         07 ��É񐔂S�W�v�q           PIC 9(2)  VALUE ZERO.
006130         07 ��×��S�W�v�q             PIC 9(5)  VALUE ZERO.
006140      05 ��㪖@�S�W�v�q.
006150         07 ��㪖@�񐔂S�W�v�q         PIC 9(2)  VALUE ZERO.
006160         07 ��㪖@���S�W�v�q           PIC 9(4)  VALUE ZERO.
006170      05 ��㪖@�S�W�v�q.
006180         07 ��㪖@�񐔂S�W�v�q         PIC 9(2)  VALUE ZERO.
006190         07 ��㪖@���S�W�v�q           PIC 9(4)  VALUE ZERO.
006200      05 �d�ÂS�W�v�q.
006210         07 �d�É񐔂S�W�v�q           PIC 9(2)  VALUE ZERO.
006220         07 �d�×��S�W�v�q             PIC 9(4)  VALUE ZERO.
006230      05 ���v�S�W�v�q                  PIC 9(6)  VALUE ZERO.
006240      05 �����ʍ����v�S�W�v�q          PIC 9(6)  VALUE ZERO.
006250      05 �����������S�W�v�q            PIC 9(3)  VALUE ZERO.
006260      05 ���������v�S�W�v�q            PIC 9(6)  VALUE ZERO.
006270******************
006280* �S���ʁ^�P�O�� *
006290******************
006300   03 ���ʂS�O�v�q.
006310      05 �����J�n�����S�O�v�q.
006320         07 �����J�n���S�O�v�q         PIC 9(2)  VALUE ZERO.
006330         07 �����J�n���S�O�v�q         PIC 9(2)  VALUE ZERO.
006340      05 ��ÂS�O�v�q.
006350         07 ��ÒP���S�O�v�q           PIC 9(4)  VALUE ZERO.
006360         07 ��É񐔂S�O�v�q           PIC 9(2)  VALUE ZERO.
006370         07 ��×��S�O�v�q             PIC 9(5)  VALUE ZERO.
006380      05 ��㪖@�S�O�v�q.
006390         07 ��㪖@�񐔂S�O�v�q         PIC 9(2)  VALUE ZERO.
006400         07 ��㪖@���S�O�v�q           PIC 9(4)  VALUE ZERO.
006410      05 ��㪖@�S�O�v�q.
006420         07 ��㪖@�񐔂S�O�v�q         PIC 9(2)  VALUE ZERO.
006430         07 ��㪖@���S�O�v�q           PIC 9(4)  VALUE ZERO.
006440      05 �d�ÂS�O�v�q.
006450         07 �d�É񐔂S�O�v�q           PIC 9(2)  VALUE ZERO.
006460         07 �d�×��S�O�v�q             PIC 9(4)  VALUE ZERO.
006470      05 ���v�S�O�v�q                  PIC 9(6)  VALUE ZERO.
006480      05 �����������S�O�v�q            PIC 9(3)  VALUE ZERO.
006490      05 ���������v�S�O�v�q            PIC 9(6)  VALUE ZERO.
006500********************
006510* �T���ʁ^�Q�D�T�� *
006520********************
006530   03 ���ʂT�Q�v�q.
006540      05 ��ÂT�Q�v�q.
006550         07 ��ÒP���T�Q�v�q           PIC 9(4)  VALUE ZERO.
006560         07 ��É񐔂T�Q�v�q           PIC 9(2)  VALUE ZERO.
006570         07 ��×��T�Q�v�q             PIC 9(5)  VALUE ZERO.
006580      05 ��㪖@�T�Q�v�q.
006590         07 ��㪖@�񐔂T�Q�v�q         PIC 9(2)  VALUE ZERO.
006600         07 ��㪖@���T�Q�v�q           PIC 9(4)  VALUE ZERO.
006610      05 ��㪖@�T�Q�v�q.
006620         07 ��㪖@�񐔂T�Q�v�q         PIC 9(2)  VALUE ZERO.
006630         07 ��㪖@���T�Q�v�q           PIC 9(4)  VALUE ZERO.
006640      05 �d�ÂT�Q�v�q.
006650         07 �d�É񐔂T�Q�v�q           PIC 9(2)  VALUE ZERO.
006660         07 �d�×��T�Q�v�q             PIC 9(4)  VALUE ZERO.
006670      05 ���v�T�Q�v�q                  PIC 9(6)  VALUE ZERO.
006680      05 �����ʍ����v�T�Q�v�q          PIC 9(6)  VALUE ZERO.
006690      05 �����������T�Q�v�q            PIC 9(3)  VALUE ZERO.
006700      05 ���������v�T�Q�v�q            PIC 9(6)  VALUE ZERO.
006710****************
006720* �T���ʁ^�T�� *
006730****************
006740   03 ���ʂT�T�v�q.
006750      05 �����J�n�����T�T�v�q.
006760         07 �����J�n���T�T�v�q         PIC 9(2)  VALUE ZERO.
006770         07 �����J�n���T�T�v�q         PIC 9(2)  VALUE ZERO.
006780      05 ��ÂT�T�v�q.
006790         07 ��ÒP���T�T�v�q           PIC 9(4)  VALUE ZERO.
006800         07 ��É񐔂T�T�v�q           PIC 9(2)  VALUE ZERO.
006810         07 ��×��T�T�v�q             PIC 9(5)  VALUE ZERO.
006820      05 ��㪖@�T�T�v�q.
006830         07 ��㪖@�񐔂T�T�v�q         PIC 9(2)  VALUE ZERO.
006840         07 ��㪖@���T�T�v�q           PIC 9(4)  VALUE ZERO.
006850      05 ��㪖@�T�T�v�q.
006860         07 ��㪖@�񐔂T�T�v�q         PIC 9(2)  VALUE ZERO.
006870         07 ��㪖@���T�T�v�q           PIC 9(4)  VALUE ZERO.
006880      05 �d�ÂT�T�v�q.
006890         07 �d�É񐔂T�T�v�q           PIC 9(2)  VALUE ZERO.
006900         07 �d�×��T�T�v�q             PIC 9(4)  VALUE ZERO.
006910      05 ���v�T�T�v�q                  PIC 9(6)  VALUE ZERO.
006920      05 �����ʍ����v�T�T�v�q          PIC 9(6)  VALUE ZERO.
006930      05 �����������T�T�v�q            PIC 9(3)  VALUE ZERO.
006940      05 ���������v�T�T�v�q            PIC 9(6)  VALUE ZERO.
006950****************
006960* �T���ʁ^�W�� *
006970****************
006980   03 ���ʂT�W�v�q.
006990      05 �����J�n�����T�W�v�q.
007000         07 �����J�n���T�W�v�q         PIC 9(2)  VALUE ZERO.
007010         07 �����J�n���T�W�v�q         PIC 9(2)  VALUE ZERO.
007020      05 ��ÂT�W�v�q.
007030         07 ��ÒP���T�W�v�q           PIC 9(4)  VALUE ZERO.
007040         07 ��É񐔂T�W�v�q           PIC 9(2)  VALUE ZERO.
007050         07 ��×��T�W�v�q             PIC 9(5)  VALUE ZERO.
007060      05 ��㪖@�T�W�v�q.
007070         07 ��㪖@�񐔂T�W�v�q         PIC 9(2)  VALUE ZERO.
007080         07 ��㪖@���T�W�v�q           PIC 9(4)  VALUE ZERO.
007090      05 ��㪖@�T�W�v�q.
007100         07 ��㪖@�񐔂T�W�v�q         PIC 9(2)  VALUE ZERO.
007110         07 ��㪖@���T�W�v�q           PIC 9(4)  VALUE ZERO.
007120      05 �d�ÂT�W�v�q.
007130         07 �d�É񐔂T�W�v�q           PIC 9(2)  VALUE ZERO.
007140         07 �d�×��T�W�v�q             PIC 9(4)  VALUE ZERO.
007150      05 ���v�T�W�v�q                  PIC 9(6)  VALUE ZERO.
007160      05 �����ʍ����v�T�W�v�q          PIC 9(6)  VALUE ZERO.
007170      05 �����������T�W�v�q            PIC 9(3)  VALUE ZERO.
007180      05 ���������v�T�W�v�q            PIC 9(6)  VALUE ZERO.
007190******************
007200* �T���ʁ^�P�O�� *
007210******************
007220   03 ���ʂT�O�v�q.
007230      05 �����J�n�����T�O�v�q.
007240         07 �����J�n���T�O�v�q         PIC 9(2)  VALUE ZERO.
007250         07 �����J�n���T�O�v�q         PIC 9(2)  VALUE ZERO.
007260      05 ��ÂT�O�v�q.
007270         07 ��ÒP���T�O�v�q           PIC 9(4)  VALUE ZERO.
007280         07 ��É񐔂T�O�v�q           PIC 9(2)  VALUE ZERO.
007290         07 ��×��T�O�v�q             PIC 9(5)  VALUE ZERO.
007300      05 ��㪖@�T�O�v�q.
007310         07 ��㪖@�񐔂T�O�v�q         PIC 9(2)  VALUE ZERO.
007320         07 ��㪖@���T�O�v�q           PIC 9(4)  VALUE ZERO.
007330      05 ��㪖@�T�O�v�q.
007340         07 ��㪖@�񐔂T�O�v�q         PIC 9(2)  VALUE ZERO.
007350         07 ��㪖@���T�O�v�q           PIC 9(4)  VALUE ZERO.
007360      05 �d�ÂT�O�v�q.
007370         07 �d�É񐔂T�O�v�q           PIC 9(2)  VALUE ZERO.
007380         07 �d�×��T�O�v�q             PIC 9(4)  VALUE ZERO.
007390      05 ���v�T�O�v�q                  PIC 9(6)  VALUE ZERO.
007400      05 �����������T�O�v�q            PIC 9(3)  VALUE ZERO.
007410      05 ���������v�T�O�v�q            PIC 9(6)  VALUE ZERO.
007420*
007430**************
007440* �{�p����� *
007450**************
007460 01 �{�p�����v.
007470    03 �_���t�ԍ��v                    PIC X(22) VALUE SPACE.
007480    03 �ڍ��t�����ԍ��v              PIC X(10) VALUE SPACE.
007490    03 ��\�҃J�i�v                    PIC X(50) VALUE SPACE.
007500    03 ��\�Җ��v.
007510       05 �����\�Җ��v               PIC X(50) VALUE SPACE.
007520    03 �ڍ��@���v                      PIC X(50) VALUE SPACE.
          03 �s���{���i�h�r�v                PIC X(2)   VALUE SPACE.
007530    03 �{�p���Z���v.
007540       05 �{�p���Z���P�v               PIC X(50) VALUE SPACE.
007550       05 �{�p���Z���Q�v               PIC X(50) VALUE SPACE.
007560    03 �{�p���X�֔ԍ��v.
007570       05 �{�p���X�֔ԍ��P�v           PIC X(3)  VALUE SPACE.
007580       05 �{�p���X�֔ԍ��Q�v           PIC X(4)  VALUE SPACE.
007590    03 �{�p���d�b�ԍ��v                PIC X(15) VALUE SPACE.
007600    03 �ڍ��t�����v.
007610       05 ����ڍ��t�����v         PIC N(7)  VALUE SPACE.
007620       05 FILLER                       PIC N(3)  VALUE SPACE.
007630    03 ��z���󗝔ԍ��v                PIC X(15) VALUE SPACE.
007640    03 �_���t�N�����v.
007650       05 �_���t�N�v                   PIC 9(2)  VALUE ZERO.
007660       05 �_���t���v                   PIC 9(2)  VALUE ZERO.
007670       05 �_���t���v                   PIC 9(2)  VALUE ZERO.
007680    03 ���҈ϔC�N�����v.
007690       05 ���҈ϔC�N�v                 PIC 9(2)  VALUE ZERO.
007700       05 ���҈ϔC���v                 PIC 9(2)  VALUE ZERO.
007710       05 ���҈ϔC���v                 PIC 9(2)  VALUE ZERO.
007720    03 �������v.
007730        05 ������s���v              PIC X(40) VALUE SPACE.
007740        05 ������s�x�X���v          PIC X(40) VALUE SPACE.
007750        05 �a����ʂv                  PIC 9(1)  VALUE ZERO.
007760        05 ��s�ԍ��v                  PIC X(4)  VALUE ZERO.
007770        05 �X�ԍ��v                    PIC X(3)  VALUE ZERO.
007780        05 �����ԍ��v                  PIC X(10) VALUE SPACE.
007790        05 �������`�l�v                PIC X(40) VALUE SPACE.
007800        05 �������`�l�J�i�v            PIC X(40) VALUE SPACE.
007810        05 ��s���x�X���v              PIC X(60) VALUE SPACE.
007820        05 �a����ʖ��̂v              PIC X(4)  VALUE SPACE.
007830        05 �a����ʃR�����g�v          PIC X(15) VALUE SPACE.
007840    03 ���{�p�h�c�v                    PIC X(15) VALUE SPACE.
007850    03 �s�����{�p�h�c�v                PIC X(15) VALUE SPACE.
007860    03 �R�����g�v.
007870        05 �R�����g�P�v                PIC X(40) VALUE SPACE.
007880        05 �R�����g�Q�v                PIC X(40) VALUE SPACE.
007890        05 �R�����g�R�v                PIC X(40) VALUE SPACE.
007900        05 �R�����g�S�v                PIC X(40) VALUE SPACE.
007910        05 �R�����g�T�v                PIC X(40) VALUE SPACE.
007920        05 �R�����g�U�v                PIC X(40) VALUE SPACE.
007930        05 �R�����g�V�v                PIC X(40) VALUE SPACE.
007940**************
007950* ��f�ҏ�� *
007960**************
007970 01 ��f�ҏ��v.
007980    03 ���Ҕԍ��v                      PIC 9(6)  VALUE ZERO.
007990    03 �{�p�N���v.
008000       05 �{�p�N�v                     PIC 9(2)  VALUE ZERO.
008010       05 �{�p���v                     PIC 9(2)  VALUE ZERO.
008020*    03 �L���v                          PIC N(12) VALUE SPACE.
007570    03 �L���v.
007580       05 ����L���v                   PIC N(12)  VALUE SPACE.
          03 �L���ԍ��v.
             05 �L���ԍ��w�v                 PIC X(40) VALUE SPACE.
008030    03 �ԍ��v.
008040       05 ����ԍ��v                   PIC X(15) VALUE SPACE.
008050       05 FILLER                       PIC X(15) VALUE SPACE.
008060*
008070    03 �ی���ʂv                      PIC 9(2)  VALUE ZERO.
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
004880    03 �ی���ʐe�v                    PIC N(2)  VALUE SPACE.
004880    03 �ی���ʕҏW�v                  PIC N(5)  VALUE SPACE.
008080    03 �ی��Ҕԍ��v.
008090       05 ����ی��Ҕԍ��v             PIC X(8)  VALUE SPACE.
008100       05 FILLER                       PIC X(2)  VALUE SPACE.
008110    03 �ی��Җ��̂v.
008120       05 ����ی��Җ��̂P�v           PIC X(40) VALUE SPACE.
008130       05 ����ی��Җ��̂Q�v           PIC X(40) VALUE SPACE.
008140*
008150    03 �s�����ԍ��v.
008160       05 ����s�����ԍ��v             PIC X(8)  VALUE SPACE.
008170       05 FILLER                       PIC X(2).
008180    03 �s�������̂v                    PIC X(40) VALUE SPACE.
008190    03 �󋋎Ҕԍ��v.
008200       05 ����󋋎Ҕԍ��v             PIC X(15)  VALUE SPACE.
008210       05 FILLER                       PIC X(5).
008220*
008230    03 �����於�̂v.
008240       05 ��������於�̂P�v           PIC X(40) VALUE SPACE.
008250       05 ��������於�̂Q�v           PIC X(40) VALUE SPACE.
008260*
008270    03 ��ی��ҏ��v.
008280       05 ��ی��҃J�i�v               PIC X(50) VALUE SPACE.
008290       05 ��ی��Ҏ����v               PIC X(50) VALUE SPACE.
008560       05 ��ی��ҏZ���v.
008300          07 ��ی��ҏZ���P�v          PIC X(50) VALUE SPACE.
008310          07 ��ی��ҏZ���Q�v          PIC X(50) VALUE SPACE.
008990       05 �d�b�ԍ��v                   PIC X(35)  VALUE SPACE.
008320*
008330    03 ���ҏ��v.
008340       05 �X�֔ԍ��v.
008350          07 �X�֔ԍ��P�v              PIC X(3)  VALUE SPACE.
008360          07 �X�֔ԍ��Q�v              PIC X(4)  VALUE SPACE.
             05 ���ҏZ���v.
008370          07 ���ҏZ���P�v              PIC X(50)  VALUE SPACE.
008380          07 ���ҏZ���Q�v              PIC X(50)  VALUE SPACE.
008390       05 ���҃J�i�v                   PIC X(50) VALUE SPACE.
008400       05 ���Ҏ����v                   PIC X(50) VALUE SPACE.
008410       05 ���ʃ`�F�b�N�v.
008420          07 �j�`�F�b�N�v              PIC N(1)  VALUE SPACE.
008430          07 ���`�F�b�N�v              PIC N(1)  VALUE SPACE.
008690          07 ���ʂv                    PIC N(2)  VALUE SPACE.
008440       05 �a��`�F�b�N�v.
008450          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008460          07 �吳�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008470          07 ���a�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008480          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008490          07 �����v                    PIC N(2)  VALUE SPACE.
008500       05 ���ҔN�v                     PIC 9(2)  VALUE ZERO.
008510       05 ���Ҍ��v                     PIC 9(2)  VALUE ZERO.
008520       05 ���ғ��v                     PIC 9(2)  VALUE ZERO.
008530       05 �����v.
008540          07 ��������v                PIC N(4)  VALUE SPACE.
008550          07 FILLER                    PIC X(4)  VALUE SPACE.
008560*       05 �����`�F�b�N�v.
008570*          07 �{�l�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008580*          07 �Ƒ��`�F�b�N�v            PIC N(1)  VALUE SPACE.
008590*
008600*       05 ���������v                   PIC N(40) OCCURS 29 VALUE SPACE.
      */���p�Ή�/110421
             05 ���������v OCCURS 29.
                07 ���������w�v              PIC X(80)  VALUE SPACE.
008610*
008620    03 �ی���ʃ`�F�b�N�v.
008630       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008640       05 �g�`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008650       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008660       05 �D�`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008670       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008680       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008690       05 �ރ`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008690       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008700*
008710    03 ������ʂv.
008720       05 �V�`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008730       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008740       05 ��`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008750       05 ��`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008760       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008760       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
             05 �����`�F�b�N�v               PIC N(1)  VALUE SPACE.
008770*
008780    03 ���t�����`�F�b�N�v.
008790       05 ���t�V���`�F�b�N�v           PIC N(1)  VALUE SPACE.
008800       05 ���t�W���`�F�b�N�v           PIC N(1)  VALUE SPACE.
008810       05 ���t�X���`�F�b�N�v           PIC N(1)  VALUE SPACE.
008820       05 ���t�V�l�`�F�b�N�v           PIC N(1)  VALUE SPACE.
008830       05 ���t�V�l�v                   PIC N(1)  VALUE SPACE.
008810*
008820    03 ���ʋ敪�`�F�b�N�v.
008830       05 �V�O�Έȏ�`�F�b�N�v         PIC N(1)  VALUE SPACE.
008840       05 ���A�w�`�F�b�N�v             PIC N(1)  VALUE SPACE.
008890       05 ������v                   PIC X(1)  VALUE SPACE.
008840*
008850    03 ���ʃ}�[�N�v                    PIC N(1)  VALUE SPACE.
008860    03 ���ʃR�����g�v                  PIC X(16) VALUE SPACE.
008870    03 ���ʃR�����g�Q�v                PIC X(16) VALUE SPACE.
007910    03 ������v                        PIC N(1)  VALUE SPACE.
008880*
008890****************
008900* �����f�[�^�e *
008910****************
008920 01 �������v.
008930    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
008940    03 ���ʏ��v  OCCURS   9.
008950       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
008960       05 ���ʃR�[�h�v.
008970          07 ������ʂv                PIC 9(2)  VALUE ZERO.
008980          07 ���ʂv                    PIC 9(2)  VALUE ZERO.
008990          07 ���E�敪�v                PIC 9(1)  VALUE ZERO.
009000          07 �����ʒu�ԍ��v            PIC 9(2)  VALUE ZERO.
009010       05 �������v                     PIC N(18) VALUE SPACE.
009020       05 �����N�����v.
009030          07 �����N�v                  PIC 9(2)  VALUE ZERO.
009040          07 �������v                  PIC 9(2)  VALUE ZERO.
009050          07 �������v                  PIC 9(2)  VALUE ZERO.
009060       05 �����N�����v.
009070          07 �����N�v                  PIC 9(2)  VALUE ZERO.
009080          07 �������v                  PIC 9(2)  VALUE ZERO.
009090          07 �������v                  PIC 9(2)  VALUE ZERO.
009100       05 �J�n�N�����v.
009110          07 �J�n�N�v                  PIC 9(2)  VALUE ZERO.
009120          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
009130          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
009140       05 �I���N�����v.
009150          07 �I���N�v                  PIC 9(2)  VALUE ZERO.
009160          07 �I�����v                  PIC 9(2)  VALUE ZERO.
009170          07 �I�����v                  PIC 9(2)  VALUE ZERO.
009180       05 �������v                     PIC 9(2)  VALUE ZERO.
009190       05 �]�A�敪�v                   PIC 9(1)  VALUE ZERO.
009200       05 �]�A�敪�`�F�b�N�v.
009210          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
009220          07 ���~�`�F�b�N�v            PIC N(1)  VALUE SPACE.
009230          07 �]��`�F�b�N�v            PIC N(1)  VALUE SPACE.
009240       05 �J�n�N�����擾�t���O         PIC X(3)  VALUE SPACE.
009250       05 ���ʋ�؂v                   PIC X(1)  VALUE SPACE.
009260       05 �o�ߗ��̂v.
009270          07 ����o�ߗ��̂v            PIC N(5)  VALUE SPACE.
009280          07 FILLER                    PIC X(2)  VALUE SPACE.
009290    03 �V�K�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
009300    03 �p���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
009310*
009320************
009330* ������� *
009340************
009350 01 �������v.
009360    03 �������Z�v.
009370       05 ���ԊO�`�F�b�N�v             PIC N(1)  VALUE SPACE.
009380       05 �x���`�F�b�N�v               PIC N(1)  VALUE SPACE.
009390       05 �[��`�F�b�N�v               PIC N(1)  VALUE SPACE.
009400    03 ���É��Z�v.
009410       05 ��ԃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
009620       05 ��H�`�F�b�N�v               PIC N(1)  VALUE SPACE.
009420       05 �\���J��`�F�b�N�v           PIC N(1)  VALUE SPACE.
009430    03 �������q�`�F�b�N�v.
009440       05 ��`�F�b�N�v                 PIC N(1)  VALUE SPACE.
009450       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
009460       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
009470    03 ���v�v                          PIC 9(7)  VALUE ZERO.
009480    03 ���񏈒u�����v�v                PIC 9(6)  VALUE ZERO.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
          03 �����񐔂v                         PIC 9(2)  VALUE ZERO.
          03 �^�����v                           PIC 9(4)  VALUE ZERO.
009490************
009500* ���l��� *
009510************
009520 01 ���l���v.
009530    03 �K�p�P�v                        PIC N(38) VALUE SPACE.
009540    03 �K�p�Q�v                        PIC N(38) VALUE SPACE.
009550    03 �o�߃R�����g�v                  PIC N(60) VALUE SPACE.
009560*
009570***************************
009580** ���Z�E�v�p( N(38)�Œ�j*
009590***************************
009600 01 �����̌o�߂v.
009610    03 �����̌o�ߍs�v                  PIC X(76) OCCURS 2 VALUE SPACE.
009620 01 �����̌o�߂m�v REDEFINES �����̌o�߂v.
009630    03 �����̌o�ߍs�m�v                PIC N(38) OCCURS 2.
009640*
       01 �E�v�{�p���v                       PIC X(100) VALUE SPACE.
       01 �{�p���v.
          03 �{�p���Q�v                      PIC X(1)  VALUE SPACE.
          03 �{�p���P�v                      PIC X(1)  VALUE SPACE.
004460* ���Z�v�g���я� *
004470 01 ���Ԃv                             PIC 9(4) VALUE ZERO.
004480*
009650*************************************************************************
009660 01 �������.
009670     03 ��`�̖��o                     PIC X(8)  VALUE SPACE.
009680     03 ���ڌQ���o                     PIC X(8)  VALUE SPACE.
009690     03 ������ʂo                     PIC X(2)  VALUE SPACE.
009700     03 �g������o.
009710         05 �[������o.
009720             07 �ړ������o             PIC X(1)  VALUE SPACE.
009730             07 �ړ��s���o             PIC 9(3)  VALUE ZERO.
009740         05 �ڍא���o                 PIC X(2)  VALUE SPACE.
009750     03 �ʒm���o                     PIC X(2)  VALUE SPACE.
009760     03 ���j�b�g���o                   PIC X(8)  VALUE SPACE.
009770*
009780 01 �v�Z�@����N�v                     PIC 9(2)  VALUE ZERO.
009790* ���t�v�n�q�j
009800 01 �a��I���N�v                       PIC 9(4)  VALUE ZERO.
009810 01 �v�Z�@����.
009820    03 �v�Z�@����N                    PIC 9(4)  VALUE ZERO.
009830    03 �v�Z�@�����                  PIC 9(4)  VALUE ZERO.
009840 01 �v�Z�@����q REDEFINES �v�Z�@����.
009850    03 �v�Z�@���I                      PIC 9(2).
009860    03 �v�Z�@���t                      PIC 9(6).
009870    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
009880       05 �v�Z�@�N��                   PIC 9(4).
009890       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
009900         07 �v�Z�@�N                   PIC 9(2).
009910         07 �v�Z�@��                   PIC 9(2).
009920       05 �v�Z�@��                     PIC 9(2).
009930*
      * C �A�g�p
       01  �����P�v        PIC X(4096).
       01  �����Q�v        PIC X(512).
       01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
      *
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
009940******************************************************************
009950*                          �A������                              *
009960******************************************************************
009970**  ��ʓ��̓f�[�^
009980 01 �A���|���̓f�[�^�ϔC��� IS EXTERNAL.
009990    03 �A���|�ϔC���                  PIC 9.
       01 �A���|���̓f�[�^�d�b��� IS EXTERNAL.
          03 �A���|�d�b���                     PIC 9.
010000*
       01 �A���|�v���r���[ IS EXTERNAL.
          03 �A���|�v���r���[�敪          PIC 9.
010300*
010010******************
010020* �R�J���������� *
010030******************
010040 01 �A���ԁ|�L�[ IS EXTERNAL.
010050    03 �A���ԁ|�{�p�N��.
010060       05 �A���ԁ|�{�p�a��             PIC 9.
010070       05 �A���ԁ|�{�p�N               PIC 9(2).
010080       05 �A���ԁ|�{�p��               PIC 9(2).
010090    03  �A���ԁ|���҃R�[�h.
010100       05 �A���ԁ|���Ҕԍ�             PIC 9(6).
010110       05 �A���ԁ|�}��                 PIC X.
010120    03 �A���ԁ|�Ώۃt���O              PIC X(3).
010130    03 �A���ԁ|���Ԍ��v.
010140       05 �A���ԁ|���Ԃv               PIC 9(2) OCCURS 9.
010150*
010160************
010170* ����L�[ *
010180************
010190*
010200*
010210 01 �A����|�Ώۃf�[�^ IS EXTERNAL.
010220    03 �A����|�{�p�N����.
010230       05 �A����|�{�p�a��             PIC 9(1).
010240       05 �A����|�{�p�N               PIC 9(2).
010250       05 �A����|�{�p��               PIC 9(2).
010260    03 �A����|���҃R�[�h.
010270       05 �A����|���Ҕԍ�             PIC 9(6).
010280       05 �A����|�}��                 PIC X(1).
010290    03 �A����|�ی����                PIC 9(2).
010300    03 �A����|�ی��Ҕԍ�              PIC X(10).
010310    03 �A����|������                PIC 9(2).
010320    03 �A����|��p���S�Ҕԍ�          PIC X(10).
010330    03 �A����|�������                PIC 9(2).
010340    03 �A����|��p���S�Ҕԍ�����      PIC X(10).
010350    03 �A����|���҃J�i                PIC X(20).
010360    03 �A����|�{�l�Ƒ��敪            PIC 9(1).
013790*
013800 01 �A���|�L�[ IS EXTERNAL.
013810    03 �A���|�ی����                  PIC 9(2).
013820*
013830*================================================================*
013840* ���S���擾�p14/10�`
013850 01 �A���|���S���擾�L�[ IS EXTERNAL.
013860    03 �A���|�{�p�a��N��.
013870       05 �A���|�{�p�a��               PIC 9.
013880       05 �A���|�{�p�N��.
013890          07 �A���|�{�p�N              PIC 9(2).
013900          07 �A���|�{�p��              PIC 9(2).
013910    03 �A���|���҃R�[�h.
013920       05 �A���|���Ҕԍ�               PIC 9(6).
013930       05 �A���|�}��                   PIC X.
013940    03 �A���|���ە��S��                PIC 9(3).
013950    03 �A���|���ۖ{�̕��S��            PIC 9(3).
013960    03 �A���|���ە��S��                PIC 9(3).
013970    03 �A���|�Q�V�V���S��              PIC 9(3).
013980    03 �A���|�������S��                PIC 9(3).
013990    03 �A���|���ʗp���S��              PIC 9(3).
014000*
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
014010************************
014020* �������R���Z�b�g     *
014030************************
014040 01 �A�����|�L�[ IS EXTERNAL.
014050    03 �A�����|�{�p�N��.
014060       05 �A�����|�{�p�a��             PIC 9.
014070       05 �A�����|�{�p�N               PIC 9(2).
014080       05 �A�����|�{�p��               PIC 9(2).
014090    03  �A�����|���҃R�[�h.
014100       05 �A�����|���Ҕԍ�             PIC 9(6).
014110       05 �A�����|�}��                 PIC X.
014120    03 �A�����|������                  PIC 9(2).
014130    03 �A�����|���R��                  PIC N(63) OCCURS 15.
014140*
013022*************
013023* ��������
013024*************
013025 01 �A�������́|�L�[ IS EXTERNAL.
013026    03 �A�������́|�������             PIC 9(2).
013027    03 �A�������́|��p���S�Ҕԍ�����   PIC X(10).
013028*   / OUT /
013029    03 �A�������́|���̏W�c.
013030       05 �A�������́|�P����            PIC N.
013031       05 �A�������́|����              PIC N(4).
013032       05 �A�������́|��������          PIC N(10).
013033*
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
014150******************************************************************
014160*                      PROCEDURE  DIVISION                       *
014170******************************************************************
014180 PROCEDURE               DIVISION.
014190************
014200*           *
014210* ��������   *
014220*           *
014230************
002570     PERFORM �v�����^�t�@�C���쐬.
014240     PERFORM ������.
014250************
014260*           *
014270* �又��     *
014280*           *
014290************
014300* ���
014310     PERFORM �A�����ڑҔ�.
014320     PERFORM ����Z�b�g.
014330     PERFORM �������.
014340************
014350*           *
014360* �I������   *
014370*           *
014380************
014390     PERFORM ��f�҈���敪�X�V.
014400     PERFORM �I������.
014410     MOVE ZERO  TO PROGRAM-STATUS.
014420     EXIT PROGRAM.
014430*
014440*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
014450*=== �������� ===================================================*
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
002974     MOVE "YCB6425"             TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪  TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014460*================================================================*
014470 ������ SECTION.
014480*================================================================*
014490     PERFORM �t�@�C���I�[�v��.
014500*    /* ���ݓ��t�擾 */
014510     ACCEPT �v�Z�@���t FROM DATE.
014520*    /* 1980�`2079�N�̊ԂŐݒ� */
014530     IF ( �v�Z�@�N > 80 )
014540        MOVE 19 TO �v�Z�@���I
014550     ELSE
014560        MOVE 20 TO �v�Z�@���I
014570     END-IF.
014580     PERFORM �J�����g�����擾.
014590     PERFORM �a��I���N�擾.
014600     COMPUTE �v�Z�@����N�v = �v�Z�@����N - 1988.
014610*
014620*================================================================*
014630 �t�@�C���I�[�v�� SECTION.
014640*
014650     OPEN INPUT   �����}�X�^
014660         MOVE NC"����" TO �t�@�C����.
014670         PERFORM �I�[�v���`�F�b�N.
014680     OPEN INPUT   ���̃}�X�^
014690         MOVE NC"����" TO �t�@�C����.
014700         PERFORM �I�[�v���`�F�b�N.
007560     OPEN INPUT   ���Z�v�g�e
007570         MOVE NC"���Z" TO �t�@�C����.
007580         PERFORM �I�[�v���`�F�b�N.
014740     OPEN INPUT   �o�߃}�X�^
014750         MOVE NC"�o��" TO �t�@�C����.
014760         PERFORM �I�[�v���`�F�b�N.
014770     OPEN INPUT   ������}�X�^
014780         MOVE NC"������" TO �t�@�C����.
014790         PERFORM �I�[�v���`�F�b�N.
014800     OPEN INPUT   �{�p�����}�X�^
014810         MOVE NC"�{��" TO �t�@�C����.
014820         PERFORM �I�[�v���`�F�b�N.
014750     OPEN INPUT   ����}�X�^.
014760         MOVE NC"����}�X�^" TO �t�@�C����.
014770         PERFORM �I�[�v���`�F�b�N.
014830     OPEN INPUT   �ی��҃}�X�^
014840         MOVE NC"�ی���" TO �t�@�C����.
014850         PERFORM �I�[�v���`�F�b�N.
014860     OPEN INPUT   ������}�X�^
014870         MOVE NC"����" TO �t�@�C����.
014880         PERFORM �I�[�v���`�F�b�N.
014890     OPEN INPUT   �h�c�Ǘ��}�X�^
014900         MOVE NC"�h�c" TO �t�@�C����.
014910         PERFORM �I�[�v���`�F�b�N.
014920     OPEN INPUT �s�����}�X�^.
014930         MOVE NC"�s����" TO �t�@�C����.
014940         PERFORM �I�[�v���`�F�b�N.
014950     OPEN INPUT   �{�p�L�^�e.
014960         MOVE NC"�{�L�e" TO �t�@�C����.
014970         PERFORM �I�[�v���`�F�b�N.
014980     OPEN INPUT   �����f�[�^�e.
014990         MOVE NC"����" TO �t�@�C����.
015000         PERFORM �I�[�v���`�F�b�N.
015010     OPEN INPUT   ���������e.
015020         MOVE NC"��������" TO �t�@�C����.
015030         PERFORM �I�[�v���`�F�b�N.
015560     OPEN INPUT   ��f�ҏ��Q�e.
015570         MOVE NC"��f�ҏ��Q�e" TO �t�@�C����.
015580         PERFORM �I�[�v���`�F�b�N.
016210     OPEN INPUT ��ƃt�@�C���Q.
016220         MOVE NC"��Q" TO �t�@�C����.
016230         PERFORM �I�[�v���`�F�b�N.
015070*
015080     OPEN I-O   ��f�ҏ��e.
015090         MOVE NC"���" TO �t�@�C����.
015100         PERFORM �I�[�v���`�F�b�N.
015110     OPEN I-O   ����t�@�C��
015120         PERFORM �G���[�����o.
015130*
015140*================================================================*
015150 �I�[�v���`�F�b�N SECTION.
015160*
015170     IF ( ��ԃL�[  NOT =  "00" )
015180        DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
015190        DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
015200        DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015210                                                UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015220        ACCEPT  �L�[���� FROM CONS
015230        PERFORM �t�@�C����
015240        EXIT PROGRAM.
015250*
015260*================================================================*
015270 �J�����g�����擾 SECTION.
015280*
015290     MOVE ZEROS TO ���|����敪.
015300     READ ������}�X�^
015310     NOT INVALID KEY
015320         MOVE ���|�J�����g����         TO �J�����g�����v
015330         MOVE ���|���Z������������敪 TO ������������敪�v
015340         MOVE ���|���Z�������R����敪 TO �������R����敪�v
015350         MOVE ���|���Z�v�g���t�敪     TO ���Z�v�g���t�敪�v
015360         MOVE ���|���Z�v�g���ғ��t�敪 TO ���Z�v�g���ғ��t�敪�v
015370     END-READ.
015380*
015390*================================================================*
015400 �a��I���N�擾 SECTION.
015410*
015420*     DISPLAY NC"�J�����g�����v"  �J�����g�����v UPON MSGBOX.
015430     MOVE �J�����g�����v TO ���|�����敪.
015440     READ �����}�X�^
015450     INVALID KEY
015460         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
015470         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015480                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015490         ACCEPT  �L�[���� FROM CONS
015500         PERFORM �I������
015510         EXIT PROGRAM
015520     NOT INVALID KEY
015530         COMPUTE �O�a��v = �J�����g�����v - 1
015540         MOVE �O�a��v TO ���|�����敪
015550         READ �����}�X�^
015560         INVALID KEY
015570             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
015580             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015590                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015600             ACCEPT  �L�[���� FROM CONS
015610             PERFORM �I������
015620             EXIT PROGRAM
015630         NOT INVALID KEY
015640             MOVE ���|�I������N TO �a��I���N�v
015650         END-READ
015660     END-READ.
015670*
015680*=== �又�� =====================================================*
015690*================================================================*
015700 �A�����ڑҔ� SECTION.
015710*================================================================*
015720     MOVE �A����|�{�p�a��           TO �{�p�a��v�q.
015730     MOVE �A����|�{�p�N             TO �{�p�N�v�q.
015740     MOVE �A����|�{�p��             TO �{�p���v�q.
015750     MOVE �A����|�ی����           TO �ی���ʂv�q.
015760     MOVE �A����|�ی��Ҕԍ�         TO �ی��Ҕԍ��v�q.
015770     MOVE �A����|������           TO �����ʂv�q.
015780     MOVE �A����|��p���S�Ҕԍ�     TO ��p���S�Ҕԍ��v�q.
015790     MOVE �A����|�������           TO ������ʂv�q.
015800     MOVE �A����|��p���S�Ҕԍ����� TO ��p���S�Ҕԍ������v�q.
015810     MOVE �A����|�{�l�Ƒ��敪       TO �{�l�Ƒ��敪�v�q.
015820     MOVE �A����|���҃J�i           TO ���҃J�i�v�q.
015830     MOVE �A����|���Ҕԍ�           TO ���Ҕԍ��v�q.
015840     MOVE �A����|�}��               TO �}�Ԃv�q.
015850*
015860*================================================================*
015870 ����Z�b�g SECTION.
015880*================================================================*
015890     PERFORM ���ڏ�����.
           PERFORM ��{���擾.
015900     PERFORM �{�p�����擾.
015910     PERFORM ��������擾.
015920     PERFORM ��f�ҏ��擾.
015930     PERFORM �����f�[�^�擾.
015940     PERFORM �������擾.
015950     PERFORM �{�p�L�^�擾.
015960***     PERFORM ��������擾.
015980     PERFORM �������Z�����擾.
015990     PERFORM �ϔC�N�����擾.
           PERFORM �{�p���擾.
      */���я����/1105
           PERFORM ���Z�v�g���я��擾.
016000*
016010* / ����}�X�^�E�����f�[�^�e�̈���敪���m�F���擾 /
016791*-----------------------------------------------*
016800     IF ( ������������敪�v  NOT = 1 ) AND ( ���Z������������敪�v NOT = 1 )
016813        IF ( ������������敪�v = 3 OR 4 )
016815           PERFORM ������������Ώ۔��菈��
016817        ELSE
016820           PERFORM ���������擾
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
016060*
015940     IF ( �������R����敪�v NOT = 1 )
               MOVE �������R����敪�v TO �A�E���|�����敪
016120     END-IF.
016130*
016140     PERFORM �{�p�h�c�擾.
016150***     PERFORM ���Z�v�g�񐔎擾.
016160     PERFORM �ی��Җ��̎擾.
016170     PERFORM ���t�����擾.
016180*
016190********************
016200* ��f�ҏ��Z�b�g *
016210********************
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
016220     IF ( ���{�p�h�c�v NOT = SPACE )
016230*        MOVE �{�p�h�c�Œ�v   TO �{�p�h�c�Œ�
016240        MOVE ���{�p�h�c�v     TO ���{�p�h�c
016250     END-IF.
      *     MOVE �ی���ʕҏW�v       TO �ی����.
           IF ������v NOT = SPACE
               MOVE ������v             TO �ی���ʂQ
               MOVE NC"��"               TO �������
           END-IF.
      *     MOVE ���A�w�`�F�b�N�v     TO ���A�w�`�F�b�N.
      *     MOVE �V�O�Έȏ�`�F�b�N�v TO �V�O�Έȏ�`�F�b�N.
      *     MOVE ������v           TO �����.
016320*
016330     MOVE �{�p�N�v            TO �{�p�N.
016340     MOVE �{�p���v            TO �{�p��.
016350*
016360*     IF ( �L���v(1:1) = NC"��" )
016370*        MOVE  SPACE           TO  �L��
016380*     ELSE
016390*        MOVE �L���v           TO  �L��
016400*     END-IF.
016410*     IF ( ����ԍ��v(1:1) = "*"  ) OR
016420*        ( ����ԍ��v(1:2) = "��" )
016430*        MOVE  SPACE           TO  �ԍ�
016440*     ELSE
016450*        MOVE ����ԍ��v       TO  �ԍ�
016460*     END-IF.
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
016470*
016480     IF ( ����s�����ԍ��v(1:2) = "99" )
016490         MOVE SPACE            TO ����S�Ҕԍ�
016500     ELSE
      */�������89�`�̎��̂݋L�ڂ���
               IF ( ����s�����ԍ��v(1:2) = "89" )
016510             MOVE �s�����ԍ��v     TO ����S�Ҕԍ�
               END-IF
016520     END-IF.
016530***     MOVE �s�������̂v        TO �s��������.
016540     MOVE ��������於�̂P�v  TO �����於�� �����於�̂Q.
016550***     MOVE ��������於�̂Q�v  TO �����於�̂Q.
016560*
016570     IF ( ����󋋎Ҕԍ��v(1:1) = "*"  ) OR
016580        ( ����󋋎Ҕԍ��v(1:2) = "��" )
016590        MOVE  SPACE           TO �󋋎Ҕԍ�
016600     ELSE
016610        MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ�
016620     END-IF.
016630*
016640***     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
016650***     MOVE �g�`�F�b�N�v        TO �g�`�F�b�N.
016660***     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
016670***     MOVE �D�`�F�b�N�v        TO �D�`�F�b�N.
016680***     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
016690***     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
016700***     MOVE �ރ`�F�b�N�v        TO �ރ`�F�b�N.
016700***     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
016710*
016720     MOVE �ی��Ҕԍ��v        TO �ی��Ҕԍ�.
016730***     MOVE �ی��Җ��̂v        TO �ی��Җ���.
016740***     MOVE ��ی��҃J�i�v      TO ��ی��҃J�i.
      */�q�ǂ���Â̏ꍇ�͑Ώۂ̎��������L��
           IF ������ʂv�q = 55
               MOVE ���Ҏ����v      TO ��ی��Ҏ���
           ELSE
016750         MOVE ��ی��Ҏ����v  TO ��ی��Ҏ���
           END-IF.
016550*     MOVE ��ی��ҏZ���v      TO �Z���P.
           MOVE ���ҏZ���P�v        TO �Z���P.
           MOVE ���ҏZ���Q�v        TO �Z���Q.
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
016850     MOVE ���Ҏ����v          TO ���Ҏ���.
      *     MOVE ���ʂv              TO ����.
016860     MOVE �j�`�F�b�N�v        TO �j�`�F�b�N.
016870     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
016880     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
016890     MOVE �吳�`�F�b�N�v      TO �吳�`�F�b�N.
016900     MOVE ���a�`�F�b�N�v      TO ���a�`�F�b�N.
016910     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
016920*     MOVE �����v              TO ����.
016930     MOVE ���ҔN�v            TO ���ҔN.
016940     MOVE ���Ҍ��v            TO ���Ҍ�.
016950     MOVE ���ғ��v            TO ���ғ�.
      *     MOVE NC"�N"              TO �N.
      *     MOVE NC"��"              TO ��.
      *     MOVE NC"��"              TO ��.
016960*     MOVE ��������v          TO ����.
      *     MOVE ���t�����v          TO ���t����.
      *
           IF ��Q�|������ی��Ҏ��� NOT = SPACE
016940        MOVE ��Q�|������ی��Ҏ��� TO ��ی��Ҏ���
           END-IF.
016970*
016980     MOVE ���������v(1)       TO ���������P.
016990     MOVE ���������v(2)       TO ���������Q.
017000     MOVE ���������v(3)       TO ���������R.
017000     MOVE ���������v(4)       TO ���������S.
017000     MOVE ���������v(5)       TO ���������T.
017000     MOVE ���������v(6)       TO ���������U.
017040*
017190********************
017200* �����f�[�^�Z�b�g *
017210********************
017220* �P���� *
017230**********
017240     MOVE �������v(1)       TO �������P.
017250     MOVE �����N�v(1)       TO �����N�P.
017260     MOVE �������v(1)       TO �������P.
017270     MOVE �������v(1)       TO �������P.
017280     MOVE �����N�v(1)       TO �����N�P.
017290     MOVE �������v(1)       TO �������P.
017300     MOVE �������v(1)       TO �������P.
017310     MOVE �J�n�N�v(1)       TO �J�n�N�P.
017320     MOVE �J�n���v(1)       TO �J�n���P.
017330     MOVE �J�n���v(1)       TO �J�n���P.
017340     MOVE �I���N�v(1)       TO �I���N�P.
017350     MOVE �I�����v(1)       TO �I�����P.
017360     MOVE �I�����v(1)       TO �I�����P.
017370     MOVE �������v(1)       TO �������P.
017380     MOVE �����`�F�b�N�v(1) TO �����`�F�b�N�P.
017390     MOVE ���~�`�F�b�N�v(1) TO ���~�`�F�b�N�P.
017400     MOVE �]��`�F�b�N�v(1) TO �]��`�F�b�N�P.
017410**********
017420* �Q���� *
017430**********
017440     MOVE �������v(2)       TO �������Q.
017450     MOVE �����N�v(2)       TO �����N�Q.
017460     MOVE �������v(2)       TO �������Q.
017470     MOVE �������v(2)       TO �������Q.
017480     MOVE �����N�v(2)       TO �����N�Q.
017490     MOVE �������v(2)       TO �������Q.
017500     MOVE �������v(2)       TO �������Q.
017510     MOVE �J�n�N�v(2)       TO �J�n�N�Q.
017520     MOVE �J�n���v(2)       TO �J�n���Q.
017530     MOVE �J�n���v(2)       TO �J�n���Q.
017540     MOVE �I���N�v(2)       TO �I���N�Q.
017550     MOVE �I�����v(2)       TO �I�����Q.
017560     MOVE �I�����v(2)       TO �I�����Q.
017570     MOVE �������v(2)       TO �������Q.
017580     MOVE �����`�F�b�N�v(2) TO �����`�F�b�N�Q.
017590     MOVE ���~�`�F�b�N�v(2) TO ���~�`�F�b�N�Q.
017600     MOVE �]��`�F�b�N�v(2) TO �]��`�F�b�N�Q.
017610**********
017620* �R���� *
017630**********
017640     MOVE �������v(3)       TO �������R.
017650     MOVE �����N�v(3)       TO �����N�R.
017660     MOVE �������v(3)       TO �������R.
017670     MOVE �������v(3)       TO �������R.
017680     MOVE �����N�v(3)       TO �����N�R.
017690     MOVE �������v(3)       TO �������R.
017700     MOVE �������v(3)       TO �������R.
017710     MOVE �J�n�N�v(3)       TO �J�n�N�R.
017720     MOVE �J�n���v(3)       TO �J�n���R.
017730     MOVE �J�n���v(3)       TO �J�n���R.
017740     MOVE �I���N�v(3)       TO �I���N�R.
017750     MOVE �I�����v(3)       TO �I�����R.
017760     MOVE �I�����v(3)       TO �I�����R.
017770     MOVE �������v(3)       TO �������R.
017780     MOVE �����`�F�b�N�v(3) TO �����`�F�b�N�R.
017790     MOVE ���~�`�F�b�N�v(3) TO ���~�`�F�b�N�R.
017800     MOVE �]��`�F�b�N�v(3) TO �]��`�F�b�N�R.
017810**********
017820* �S���� *
017830**********
017840     MOVE �������v(4)       TO �������S.
017850     MOVE �����N�v(4)       TO �����N�S.
017860     MOVE �������v(4)       TO �������S.
017870     MOVE �������v(4)       TO �������S.
017880     MOVE �����N�v(4)       TO �����N�S.
017890     MOVE �������v(4)       TO �������S.
017900     MOVE �������v(4)       TO �������S.
017910     MOVE �J�n�N�v(4)       TO �J�n�N�S.
017920     MOVE �J�n���v(4)       TO �J�n���S.
017930     MOVE �J�n���v(4)       TO �J�n���S.
017940     MOVE �I���N�v(4)       TO �I���N�S.
017950     MOVE �I�����v(4)       TO �I�����S.
017960     MOVE �I�����v(4)       TO �I�����S.
017970     MOVE �������v(4)       TO �������S.
017980     MOVE �����`�F�b�N�v(4) TO �����`�F�b�N�S.
017990     MOVE ���~�`�F�b�N�v(4) TO ���~�`�F�b�N�S.
018000     MOVE �]��`�F�b�N�v(4) TO �]��`�F�b�N�S.
018010**********
018020* �T���� *
018030**********
018040     MOVE �������v(5)       TO �������T.
018050     MOVE �����N�v(5)       TO �����N�T.
018060     MOVE �������v(5)       TO �������T.
018070     MOVE �������v(5)       TO �������T.
018080     MOVE �����N�v(5)       TO �����N�T.
018090     MOVE �������v(5)       TO �������T.
018100     MOVE �������v(5)       TO �������T.
018110     MOVE �J�n�N�v(5)       TO �J�n�N�T.
018120     MOVE �J�n���v(5)       TO �J�n���T.
018130     MOVE �J�n���v(5)       TO �J�n���T.
018140     MOVE �I���N�v(5)       TO �I���N�T.
018150     MOVE �I�����v(5)       TO �I�����T.
018160     MOVE �I�����v(5)       TO �I�����T.
018170     MOVE �������v(5)       TO �������T.
018180     MOVE �����`�F�b�N�v(5) TO �����`�F�b�N�T.
018190     MOVE ���~�`�F�b�N�v(5) TO ���~�`�F�b�N�T.
018200     MOVE �]��`�F�b�N�v(5) TO �]��`�F�b�N�T.
018210**************
018220* �o�߃Z�b�g *
018230**************
018240     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018250***             UNTIL ( ���ʂb�m�s > ���ʐ��v )
018260             UNTIL ( ���ʂb�m�s > 5 )
018270**         MOVE ���ʂb�m�s�v(���ʂb�m�s)   TO �o�ߕ��ʂb�m�s(���ʂb�m�s)
018280**         MOVE ���ʋ�؂v(���ʂb�m�s)     TO ���ʋ��(���ʂb�m�s)
018290         MOVE ����o�ߗ��̂v(���ʂb�m�s) TO �o�ߗ���(���ʂb�m�s)
018300     END-PERFORM.
018310*****************************************
018320*     �V�K�E�p���`�F�b�N�ɂ���        *
018330*   ���V�K...�����L�� ���p��...�����Ȃ� *
018340*****************************************
018350     MOVE �V�K�`�F�b�N�v    TO �V�K�`�F�b�N.
018360     MOVE �p���`�F�b�N�v    TO �p���`�F�b�N.
018370********************
018380* �����f�[�^�Z�b�g *
018390********************
018400*    ****************************************************************
018410*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
018420*    ****************************************************************
018430     MOVE �������v�q                   TO  ������.
           MOVE ���k���v�q                   TO  ���������k��.
018440     MOVE ���ԊO�`�F�b�N�v             TO  ���ԊO�`�F�b�N.
018450     MOVE �x���`�F�b�N�v               TO  �x���`�F�b�N.
018460     MOVE �[��`�F�b�N�v               TO  �[��`�F�b�N.
018470     MOVE �������Z���v�q               TO  �������Z��.
           IF (���ԊO�`�F�b�N�v NOT = SPACE) OR (�[��`�F�b�N�v NOT = SPACE) OR
              (�x���`�F�b�N�v NOT = SPACE)
              MOVE �������Z���v              TO  �������Z��
              MOVE �������Z��؂v            TO  �������Z���
              MOVE �������Z���v              TO  �������Z��
           END-IF.
018480     MOVE �Č����v�q                   TO  �Č���.
018490     MOVE ���Ë����v�q                 TO  ���Ë���.
018500     MOVE ���É񐔂v�q                 TO  ���É�.
018510     MOVE ���×��v�q                   TO  ���×�.
018520     MOVE ��ԃ`�F�b�N�v               TO  ��ԃ`�F�b�N.
018690     MOVE ��H�`�F�b�N�v               TO  ��H�`�F�b�N.
018530     MOVE �\���J��`�F�b�N�v           TO  �\���J��`�F�b�N.
018540     MOVE ���É��Z���v�q               TO  ���É��Z��.
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
018580     MOVE �������q���Z���v�q           TO  �������q���Z��.
018590     MOVE �{�p���񋟗��v�q           TO  �{�p���񋟗�.
018600     MOVE ���v�v                       TO ���v.
018610********************
018620* ���񏈒u���Z�b�g *
018630********************
018640     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018650***             UNTIL ( ���ʂb�m�s > ���ʐ��v )
018660             UNTIL ( ���ʂb�m�s > 5 )
018670        MOVE ���񏈒u���v�q(���ʂb�m�s) TO ���񏈒u��(���ʂb�m�s)
018680     END-PERFORM.
018690     MOVE ���񏈒u�����v�v             TO ���񏈒u�����v
018700********************
018710* �����������Z�b�g *
018720********************
018730*    **********
018740*    * �P���� *
018750*    **********
018760     MOVE ��ÒP���P�v�q               TO ��ÒP���P.
018770     MOVE ��É񐔂P�v�q               TO ��É񐔂P.
018780     MOVE ��×��P�v�q                 TO ��×��P.
018790     MOVE ��㪖@�񐔂P�v�q             TO ��㪖@�񐔂P.
018800     MOVE ��㪖@���P�v�q               TO ��㪖@���P.
018810     MOVE ��㪖@�񐔂P�v�q             TO ��㪖@�񐔂P.
018820     MOVE ��㪖@���P�v�q               TO ��㪖@���P.
018830     MOVE �d�É񐔂P�v�q               TO �d�É񐔂P.
018840     MOVE �d�×��P�v�q                 TO �d�×��P.
018850     MOVE ���v�P�v�q                   TO ���v�P.
018860     IF ( �����������P�v�q NOT = ZERO )
018870        COMPUTE �����������P = �����������P�v�q / 100
018880     END-IF.
018890     MOVE ���������v�P�v�q             TO ���������v�P.
018900*    **********
018910*    * �Q���� *
018920*    **********
018930     MOVE ��ÒP���Q�v�q               TO ��ÒP���Q.
018940     MOVE ��É񐔂Q�v�q               TO ��É񐔂Q.
018950     MOVE ��×��Q�v�q                 TO ��×��Q.
018960     MOVE ��㪖@�񐔂Q�v�q             TO ��㪖@�񐔂Q.
018970     MOVE ��㪖@���Q�v�q               TO ��㪖@���Q.
018980     MOVE ��㪖@�񐔂Q�v�q             TO ��㪖@�񐔂Q.
018990     MOVE ��㪖@���Q�v�q               TO ��㪖@���Q.
019000     MOVE �d�É񐔂Q�v�q               TO �d�É񐔂Q.
019010     MOVE �d�×��Q�v�q                 TO �d�×��Q.
019020     MOVE ���v�Q�v�q                   TO ���v�Q.
019030     IF ( �����������Q�v�q NOT = ZERO )
019040        COMPUTE �����������Q = �����������Q�v�q / 100
019050     END-IF.
019060     MOVE ���������v�Q�v�q             TO ���������v�Q.
019070*    ****************
019080*    * �R���ʁ^�W�� *
019090*    ****************
019100     MOVE ��ÒP���R�W�v�q             TO ��ÒP���R�W.
019110     MOVE ��É񐔂R�W�v�q             TO ��É񐔂R�W.
019120     MOVE ��×��R�W�v�q               TO ��×��R�W.
019130     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
019140     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
019150     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
019160     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
019170     MOVE �d�É񐔂R�W�v�q             TO �d�É񐔂R�W.
019180     MOVE �d�×��R�W�v�q               TO �d�×��R�W.
019190     MOVE ���v�R�W�v�q                 TO ���v�R�W.
019200     MOVE �����ʍ����v�R�W�v�q         TO �����ʍ����v�R�W.
019210     IF ( �����������R�W�v�q NOT = ZERO )
019220        COMPUTE �����������R�W = �����������R�W�v�q / 100
019230     END-IF.
019240     MOVE ���������v�R�W�v�q           TO ���������v�R�W.
      */ ������ 0.7��0.6 /42505  /*�o���Ȃ� /42610
      *     IF (�{�p�a��N���v�q >= 42505)
      *        MOVE "60"                      TO �����R�W
      *        MOVE "0.6"                     TO �����ʂR�W
      *        MOVE "==="                     TO ���������R�W �����ʒ����R�W
      *     END-IF.
019250*    ****************
019260*    * �R���ʁ^10�� *
019270*    ****************
019280     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
019290     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
019300     MOVE ��ÒP���R�O�v�q             TO ��ÒP���R�O.
019310     MOVE ��É񐔂R�O�v�q             TO ��É񐔂R�O.
019320     MOVE ��×��R�O�v�q               TO ��×��R�O.
019330     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
019340     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
019350     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
019360     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
019370     MOVE �d�É񐔂R�O�v�q             TO �d�É񐔂R�O.
019380     MOVE �d�×��R�O�v�q               TO �d�×��R�O.
019390     MOVE ���v�R�O�v�q                 TO ���v�R�O.
019400     IF ( �����������R�O�v�q NOT = ZERO )
019410        COMPUTE �����������R�O = �����������R�O�v�q / 100
019420     END-IF.
019430     MOVE ���������v�R�O�v�q           TO ���������v�R�O.
019440*    ****************
019450*    * �S���ʁ^�T�� *
019460*    ****************
019470     MOVE ��ÒP���S�T�v�q             TO ��ÒP���S�T.
019480     MOVE ��É񐔂S�T�v�q             TO ��É񐔂S�T.
019490     MOVE ��×��S�T�v�q               TO ��×��S�T.
019500     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
019510     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
019520     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
019530     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
019540     MOVE �d�É񐔂S�T�v�q             TO �d�É񐔂S�T.
019550     MOVE �d�×��S�T�v�q               TO �d�×��S�T.
019560     MOVE ���v�S�T�v�q                 TO ���v�S�T.
019570     MOVE �����ʍ����v�S�T�v�q         TO �����ʍ����v�S�T.
019580     IF ( �����������S�T�v�q NOT = ZERO )
019590        COMPUTE �����������S�T = �����������S�T�v�q / 100
019600     END-IF.
019610     MOVE ���������v�S�T�v�q           TO ���������v�S�T.
019620*    ****************
019630*    * �S���ʁ^�W�� *
019640*    ****************
019650     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
019660     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
019670     MOVE ��ÒP���S�W�v�q             TO ��ÒP���S�W.
019680     MOVE ��É񐔂S�W�v�q             TO ��É񐔂S�W.
019690     MOVE ��×��S�W�v�q               TO ��×��S�W.
019700     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
019710     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
019720     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
019730     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
019740     MOVE �d�É񐔂S�W�v�q             TO �d�É񐔂S�W.
019750     MOVE �d�×��S�W�v�q               TO �d�×��S�W.
019760     MOVE ���v�S�W�v�q                 TO ���v�S�W.
019770     MOVE �����ʍ����v�S�W�v�q         TO �����ʍ����v�S�W.
019780     IF ( �����������S�W�v�q NOT = ZERO )
019790        COMPUTE �����������S�W = �����������S�W�v�q / 100
019800     END-IF.
019810     MOVE ���������v�S�W�v�q           TO ���������v�S�W.
      */ ������ 0.7��0.6 /42505  /*�o���Ȃ� /42610
      *     IF (�{�p�a��N���v�q >= 42505)
      *        MOVE "60"                      TO �����S�W
      *        MOVE "0.6"                     TO �����ʂS�W
      *        MOVE "==="                     TO ���������S�W �����ʒ����S�W
      *     END-IF.
019820*    ****************
019830*    * �S���ʁ^10�� *
019840*    ****************
019850     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
019860     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
019870     MOVE ��ÒP���S�O�v�q             TO ��ÒP���S�O.
019880     MOVE ��É񐔂S�O�v�q             TO ��É񐔂S�O.
019890     MOVE ��×��S�O�v�q               TO ��×��S�O.
019900     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
019910     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
019920     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
019930     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
019940     MOVE �d�É񐔂S�O�v�q             TO �d�É񐔂S�O.
019950     MOVE �d�×��S�O�v�q               TO �d�×��S�O.
019960     MOVE ���v�S�O�v�q                 TO ���v�S�O.
019970     IF ( �����������S�O�v�q NOT = ZERO )
019980        COMPUTE �����������S�O = �����������S�O�v�q / 100
019990     END-IF.
020000     MOVE ���������v�S�O�v�q           TO ���������v�S�O.
020010*
020020*��***********************************************************************
020030* �T���ʁ^2.5���̈󎚂͕K�v�Ȃ��B
020040*------------------------------------------------------------------------*
020050*    *****************
020060*    * �T���ʁ^2.5�� *
020070*    *****************
020080*     MOVE ��ÒP���T�Q�v�q             TO ��ÒP���T�Q.
020090*     MOVE ��É񐔂T�Q�v�q             TO ��É񐔂T�Q.
020100*     MOVE ��×��T�Q�v�q               TO ��×��T�Q.
020110*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
020120*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
020130*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
020140*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
020150*     MOVE �d�É񐔂T�Q�v�q             TO �d�É񐔂T�Q.
020160*     MOVE �d�×��T�Q�v�q               TO �d�×��T�Q.
020170*     MOVE ���v�T�Q�v�q                 TO ���v�T�Q.
020180*     MOVE �����ʍ����v�T�Q�v�q         TO �����ʍ����v�T�Q.
020190*     IF ( �����������T�Q�v�q NOT = ZERO )
020200*        COMPUTE �����������T�Q = �����������T�Q�v�q / 100
020210*     END-IF.
020220*     MOVE ���������v�T�Q�v�q           TO ���������v�T�Q.
020230*��***********************************************************************
020240*
020250**    ****************
020260**    * �T���ʁ^�T�� *
020270**    ****************
020280*     MOVE SPACE TO ���ʂT�v.
020290*     IF ( ���v�T�T�v�q NOT = ZERO )
020300*        MOVE "5) 33 "                  TO �����Œ�T�v
020310*        MOVE "0.33"                    TO �����ʗ��T�v
020320*        MOVE �����J�n���T�T�v�q        TO �����J�n���T�v
020330*        MOVE �����J�n���T�T�v�q        TO �����J�n���T�v
020340*        MOVE ��ÒP���T�T�v�q          TO ��ÒP���T�v
020350*        MOVE ��É񐔂T�T�v�q          TO ��É񐔂T�v
020360*        MOVE ��×��T�T�v�q            TO ��×��T�v
020370*        MOVE ��㪖@�񐔂T�T�v�q        TO ��㪖@�񐔂T�v
020380*        MOVE ��㪖@���T�T�v�q          TO ��㪖@���T�v
020390*        MOVE ��㪖@�񐔂T�T�v�q        TO ��㪖@�񐔂T�v
020400*        MOVE ��㪖@���T�T�v�q          TO ��㪖@���T�v
020410*        MOVE �d�É񐔂T�T�v�q          TO �d�É񐔂T�v
020420*        MOVE �d�×��T�T�v�q            TO �d�×��T�v
020430*        MOVE ���v�T�T�v�q              TO ���v�T�v
020440*        MOVE �����ʍ����v�T�T�v�q      TO �����ʍ����v�T�v
020450*        IF ( �����������T�T�v�q NOT = ZERO )
020460*           COMPUTE �����������T�v = �����������T�T�v�q / 100
020470*        END-IF
020480*        MOVE ���������v�T�T�v�q        TO ���������v�T�v
020490**------------------------------------------------------------------------------------*
020500** ����14�N6������4���ʖځE5���ʖڂ̒�������45��33�ɕύX�B
020510** ����ɂ��A5���ʖځi���O�j�󎚂ɂ��āA����14�N6�����O�̏ꍇ�A45��ݒ肷��B
020520**
020530*        IF ( �{�p�a��N���v�q < 41406 )
020540*           MOVE "5) 45 "               TO �����Œ�T�v
020550*           MOVE "0.45"                 TO �����ʗ��T�v
020560*        END-IF
020570**------------------------------------------------------------------------------------*
020580**
020590*        MOVE ���ʂT�v                  TO ���ʂT�W
020600*     END-IF.
020610*    ****************
020620*    * �T���ʁ^�W�� *
020630*    ****************
020640     MOVE SPACE TO ���ʂT�v.
020650     IF ( ���v�T�W�v�q NOT = ZERO )
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
020860     END-IF.
020870*    ****************
020880*    * �T���ʁ^10�� *
020890*    ****************
020900     MOVE SPACE TO ���ʂT�v.
020910     IF ( ���v�T�O�v�q NOT = ZERO )
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
021100     END-IF.
021110*
021120     MOVE �K�p�P�v                     TO �K�p�P.
021130     MOVE �K�p�Q�v                     TO �K�p�Q.
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
      *
021140     MOVE ���Z�|���v                   TO ���v.
021150***     MOVE ���Z�|�ꕔ���S��             TO �ꕔ���S��.
021160*     MOVE ���Z�|�������z               TO �������z.
021170     MOVE ���Z�|�󋋎ҕ��S�z             TO �󋋎ҕ��S�z.
021180     MOVE ���Z�|�����������z             TO �����������z.
021190*
021202**********************
021210* ���t�����`�F�b�N   *
021220**********************
021230*     MOVE ���t�V�l�v             TO  ���t�V�l.
021240*     MOVE ���t�V�l�`�F�b�N�v     TO  ���t�V�l�`�F�b�N.
021250*     MOVE ���t�V���`�F�b�N�v     TO  ���t�V���`�F�b�N.
021260*     MOVE ���t�W���`�F�b�N�v     TO  ���t�W���`�F�b�N.
021270*     MOVE ���t�X���`�F�b�N�v     TO  ���t�X���`�F�b�N.
021280*
021290**********************
021300* �{�p���f�[�^�Z�b�g *
021310**********************
           MOVE �s���{���i�h�r�v       TO �s���{���ԍ�.
021320     MOVE �_���t�ԍ��v           TO �_���t�ԍ�.
021330*     MOVE ��z���󗝔ԍ��v       TO ��z���󗝔ԍ�.
021340     MOVE �{�p���X�֔ԍ��P�v     TO �{�p���X�֔ԍ��P.
021350     MOVE �{�p���X�֔ԍ��Q�v     TO �{�p���X�֔ԍ��Q.
021360*     MOVE �{�p���Z���v           TO �{�p���Z���P.
021370     MOVE �{�p���Z���P�v         TO �{�p���Z���P.
021380     MOVE �{�p���Z���Q�v         TO �{�p���Z���Q.
      */�����Q�V�N�P�O���{�p��������ԍ������/150922
021390     MOVE �ڍ��t�����ԍ��v     TO �ڍ��t�����ԍ�.
021400     MOVE �ڍ��@���v             TO �ڍ��@��.
021410     MOVE ��\�҃J�i�v           TO ��\�҃J�i.
021420     MOVE ��\�Җ��v             TO ��\�Җ�.
021430     MOVE �{�p���d�b�ԍ��v       TO �{�p���d�b�ԍ�.
021440*
021450* / �_���t�E���҈ϔC�� /
021460     MOVE �_���t�N�v             TO �󗝔N.
021470     MOVE �_���t���v             TO �󗝌�.
021480     MOVE �_���t���v             TO �󗝓�.
021490* ( �ϔC�N���� ������邩 )
021500     IF ( �A���|�ϔC���  = ZERO )
021510        MOVE ���҈ϔC�N�v        TO �ϔC�N
021520        MOVE ���҈ϔC���v        TO �ϔC��
021530        MOVE ���҈ϔC���v        TO �ϔC��
021540     END-IF.
021550*
021560***     MOVE �R�����g�P�v           TO �R�����g�P.
021570***     MOVE �R�����g�Q�v           TO �R�����g�Q.
021580***     MOVE �R�����g�R�v           TO �R�����g�R.
021590***     MOVE �R�����g�S�v           TO �R�����g�S.
021600***     MOVE �R�����g�T�v           TO �R�����g�T.
021610***     MOVE �R�����g�U�v           TO �R�����g�U.
021620***     MOVE �R�����g�V�v           TO �R�����g�V.
021630*
021640***     MOVE ��s���x�X���v         TO ��s���x�X��.
021650***     MOVE �a����ʃR�����g�v     TO �a�����.
021660***     MOVE �����ԍ��v             TO �����ԍ�.
021670***     MOVE �������`�l�J�i�v       TO �������`�l�J�i.
021680***     MOVE �������`�l�v           TO �������`�l.
             MOVE NC"��"                  TO �U���`�F�b�N ���ʃ`�F�b�N.
021690*
021700* �ŉ����Ɋ��҃R�[�h
021710***     MOVE ���Ҕԍ��v�q           TO ���Ҕԍ�.
021720***     MOVE �}�Ԃv�q               TO �}��.
021730*
021740* ���ʃR�����g
021750*     MOVE ���ʃR�����g�v         TO ���ʃR�����g.
021760*
021770* �����s�@�E��Ɂu�O�v�󎚁i����ҁj 14/10�`
021780*     MOVE ���ʃ}�[�N�v           TO ���ʃ}�[�N.
021790*
021800* ���m���@���ʃR�����g�i�S�P�V�j14/10�`
021810*     MOVE ���ʃR�����g�Q�v       TO ���ʃR�����g�Q.
021820*
022750* ���Z�v�g���я��Z�b�g *
022760     MOVE ���Ԃv                 TO ����.
022770*
021830*-------------------------------------------------------------------------*
021840*--- �� ���Z�E�v�ăZ�b�g�́A���̈���Z�b�gSECTION �̍Ō�ɂ�邱�ƁI -----*
021850     PERFORM ���Z�E�v�ăZ�b�g.
021860*-------------------------------------------------------------------------*
021870*
021880*     PERFORM �e�X�g�󎚏���.
021890*
021900*=== ����Z�b�g =================================================*
021910*================================================================*
021920 ���ڏ����� SECTION.
021930*================================================================*
021940     INITIALIZE �{�p�����v.
021950     INITIALIZE ��f�ҏ��v.
021960     INITIALIZE �������v.
021970     INITIALIZE �������v.
021980     INITIALIZE ���l���v.
021990     INITIALIZE �����P�v�q.
022000     INITIALIZE �����Q�v�q.
022010     INITIALIZE �����R�v�q.
022020     MOVE SPACE TO YCB6425P.
022030*****     INITIALIZE YCB6425P.
022040*
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
026460     MOVE �{�p�a��v�q       TO ��Q�|�{�p�a��.
026470     MOVE �{�p�N�v�q         TO ��Q�|�{�p�N.
026480     MOVE �{�p���v�q         TO ��Q�|�{�p��.
026490     MOVE ���҃R�[�h�v�q     TO ��Q�|���҃R�[�h.
026500     READ ��f�ҏ��Q�e
           INVALID KEY
              MOVE SPACE           TO ��Q�|���R�[�h
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
022050*================================================================*
022060 �{�p�����擾 SECTION.
022070*================================================================*
022080**************************************************
022090* �{�@�f�[�^���g�p���A�ȉ��̏����擾           *
022100* �� �_���t�ԍ�.. �_���t�ԍ��v�Ɋi�[             *
022110* �� ����ԍ� ... �ڍ��t�����ԍ��v�Ɋi�[       *
022120* �� ��\�Җ� ... ��\�Җ��v�Ɋi�[               *
022130* �� �Z��1,2   ...�{�p���Z��1,2�v�Ɋi�[          *
022140* �� �d�b�ԍ� ... �{�p���d�b�ԍ��v�Ɋi�[         *
022150**************************************************
022160     MOVE ZERO  TO �{��|�{�p���ԍ�.
022170     READ �{�p�����}�X�^
022180     INVALID KEY
022190         CONTINUE
022200     NOT INVALID KEY
022210*
               MOVE �{��|�s���{���i�h�r    TO �s���{���i�h�r�v
022250         MOVE �{��|�V�_���t�ԍ�   TO �_���t�ԍ��v
022270*
022280         MOVE �{��|�ڍ��t�����ԍ�  TO �ڍ��t�����ԍ��v
022290         MOVE �{��|�X�֔ԍ��P        TO �{�p���X�֔ԍ��P�v
022300         MOVE �{��|�X�֔ԍ��Q        TO �{�p���X�֔ԍ��Q�v
022310         MOVE �{��|�ڍ��@��          TO �ڍ��@���v
022320         MOVE �{��|��\�҃J�i        TO ��\�҃J�i�v
022330         MOVE �{��|��\�Җ�          TO ��\�Җ��v
022340*
022350*         STRING �{��|�Z���P  DELIMITED BY SPACE
022360*                �{��|�Z���Q  DELIMITED BY SPACE
022370*           INTO �{�p���Z���v
022380*         END-STRING
022390         MOVE �{��|�Z���P            TO �{�p���Z���P�v
022400         MOVE �{��|�Z���Q            TO �{�p���Z���Q�v
022410         MOVE �{��|�d�b�ԍ�          TO �{�p���d�b�ԍ��v
022420* �U������
022430         MOVE �{��|������s��      TO ������s���v
022440         MOVE �{��|������s�x�X��  TO ������s�x�X���v
022450         MOVE �{��|�a�����          TO �a����ʂv
022460         MOVE �{��|��s�ԍ�          TO ��s�ԍ��v
022470         MOVE �{��|�X�ԍ�            TO �X�ԍ��v
022480         MOVE �{��|�����ԍ�          TO �����ԍ��v
022490         MOVE �{��|�������`�l        TO �������`�l�v
022500         MOVE �{��|�������`�l�J�i    TO �������`�l�J�i�v
022510****         MOVE �{��|�ڍ��t����    TO �ڍ��t�����v
022520     END-READ.
022530*
023520        MOVE ZERO  TO  ���|�_���I���敪
022460        MOVE 27    TO  ���|����R�[�h.
022470        MOVE ZERO  TO  ���|�ی����.
023530        MOVE ZERO  TO  ���|�ύX�a��N��
022490        READ ����}�X�^
022500        NOT INVALID KEY
022510            MOVE ���|������s��      TO ������s���v
022520            MOVE ���|������s�x�X��  TO ������s�x�X���v
022530            MOVE ���|�a�����          TO �a����ʂv
022540            MOVE ���|��s�ԍ�          TO ��s�ԍ��v
022550            MOVE ���|�X�ԍ�            TO �X�ԍ��v
022560            MOVE ���|�����ԍ�          TO �����ԍ��v
022570            MOVE ���|�������`�l�J�i    TO �������`�l�J�i�v
022580            MOVE ���|�������`�l        TO �������`�l�v
022590            MOVE ���|�ڍ��t����    TO �ڍ��t�����v
022600        END-READ.
022620*
022540* �U������
022550     STRING ������s���v     DELIMITED BY SPACE
022560            "  "               DELIMITED BY SIZE
022570            ������s�x�X���v DELIMITED BY SPACE
022580            INTO ��s���x�X���v
022590     END-STRING.
022600     EVALUATE �a����ʂv
022610     WHEN 1
022620         MOVE "����" TO �a����ʖ��̂v
022630     WHEN 2
022640         MOVE "����" TO �a����ʖ��̂v
022650     WHEN OTHER
022660         MOVE SPACE  TO �a����ʖ��̂v
022670     END-EVALUATE.
022680     STRING ��s�ԍ��v     DELIMITED BY SPACE
022690            " "            DELIMITED BY SIZE
022700            �X�ԍ��v       DELIMITED BY SPACE
022710            " "            DELIMITED BY SIZE
022720            �a����ʖ��̂v DELIMITED BY SPACE
022730            INTO �a����ʃR�����g�v
022740     END-STRING.
022750*
022760* �R�����g��
022770     MOVE SPACE TO �R�����g�v.
022780     INITIALIZE    �R�����g�v.
022970*
022990        MOVE "�����擾������L���z�̎�̌���" TO �R�����g�P�v.
023000        STRING "�����_���t����"     DELIMITED BY SIZE
023010               " � "             DELIMITED BY SIZE
023020               �ڍ��t�����v     DELIMITED BY SIZE
023030               INTO �R�����g�Q�v
023040        END-STRING.
023050        MOVE "�ɍĈϔC���܂��B"     TO �R�����g�R�v.
023060        PERFORM ���t�ҏW.
023070        MOVE ���t�ҏW�v             TO �R�����g�S�v.
023080        MOVE "�_�������t"           TO �R�����g�T�v.
023090        STRING "(����) "            DELIMITED BY SIZE
023100               ��\�Җ��v           DELIMITED BY SIZE
023110               "      (��)"         DELIMITED BY SIZE
023120               INTO �R�����g�U�v
023130        END-STRING.
023140        MOVE "(�Z��) �{�p�ؖ����Ɠ���" TO �R�����g�V�v.
022790*
022800*     MOVE "�y ���l �z" TO �R�����g�P�v.
022810*
023510*================================================================*
023520 ���t�ҏW SECTION.
023530*
023540     MOVE �{�p�a��v�q TO ���|�����敪.
023550     READ �����}�X�^
023560     INVALID KEY
023570         MOVE SPACE TO ���|���R�[�h
023580         INITIALIZE    ���|���R�[�h
023590     NOT INVALID KEY
023600         MOVE ���|�J�n����N TO �{�p����N�v
023610     END-READ.
023620     IF ( �{�p����N�v NOT = ZERO )
023630        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
023640     END-IF.
023650*
023660     EVALUATE �{�p���v�q
023670     WHEN 4
023680     WHEN 6
023690     WHEN 9
023700     WHEN 11
023710         MOVE 30   TO �������v
023720     WHEN 2
023730         DIVIDE 4 INTO �{�p����N�v GIVING    ���v
023740                                    REMAINDER �]�v
023750         END-DIVIDE
023760         IF ( �]�v = ZERO )
023770            MOVE 29 TO �������v
023780         ELSE
023790            MOVE 28 TO �������v
023800         END-IF
023810     WHEN 1
023820     WHEN 3
023830     WHEN 5
023840     WHEN 7
023850     WHEN 8
023860     WHEN 10
023870     WHEN 12
023880         MOVE 31   TO �������v
023890     WHEN OTHER
023900         MOVE ZERO TO �������v
023910     END-EVALUATE.
023920*
023930     MOVE ���|�������� TO �����ҏW�v.
023940     MOVE �{�p�N�v�q   TO �N�ҏW�v.
023950     MOVE �{�p���v�q   TO ���ҏW�v.
023960     MOVE �������v     TO ���ҏW�v.
023970*
022820*================================================================*
022830 ��������擾 SECTION.
022840*================================================================*
022850****************************************************
022860* �A���f�[�^����ی��҃}�X�^��萿������擾����B *
022870* ���s�|��������敪=1�̏ꍇ������}�X�^���g�p   *
022880* �� ������...... �����於�̂v�Ɋi�[               *
022890****************************************************
022900     MOVE ������ʂv�q           TO �s�|������.
022910     MOVE ��p���S�Ҕԍ������v�q TO �s�|�s�����ԍ�.
022920*
022930     READ �s�����}�X�^
022940     INVALID KEY
022950         MOVE SPACE                     TO �����於�̂v �s�������̂v
022960     NOT INVALID KEY
022970         IF ( �s�|������敪 = 1 )
022980            MOVE ������ʂv�q           TO ����|�ی����
022990            MOVE ��p���S�Ҕԍ������v�q TO ����|�ی��Ҕԍ�
023000            READ ������}�X�^
023010            INVALID KEY
023020                MOVE SPACE              TO �����於�̂v �s�������̂v
023030            NOT INVALID KEY
023040                MOVE ����|�ی��Җ���   TO �����於�̂v �s�������̂v
023050            END-READ
023060         ELSE
023070            MOVE �s�|�s��������         TO �����於�̂v �s�������̂v
023080         END-IF
023090     END-READ.
023100*
023110*================================================================*
023120 ��f�ҏ��擾 SECTION.
023130*================================================================*
023140**************************************************
023150* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
023160* �� �{�p�N ..... �{�p�N�v�Ɋi�[                 *
023170* �� �{�p�� ..... �{�p���v�Ɋi�[                 *
023180* �� ���Ҕԍ�.... ���Ҕԍ��v�Ɋi�[���e�c�A�ԗp   *
023190* �� �L�� ....... �L���v�Ɋi�[                   *
023200* �� �ԍ� ....... �ԍ��v�Ɋi�[                   *
023210* �� �ی��Ҕԍ� . �ی��Ҕԍ��v�Ɋi�[             *
023220* �� �ی���� ... �ی���ʂv�Ɋi�[               *
023230* �� ��ی��҃J�i.��ی��҃J�i�v�Ɋi�[           *
023240* �� ��ی��Ҏ���.��ی��Ҏ����v�Ɋi�[           *
023250* �� �Z���P ......��ی��ҏZ���P�v�Ɋi�[         *
023260* �� �Z���Q ......��ی��ҏZ���Q�v�Ɋi�[         *
023270* �� ���҃J�i ....���҃J�i�v�Ɋi�[               *
023280* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
023290* �� ���Ґ��� ....�敪�ɂ��`�F�b�N��"��"���i�[ *
023300* �� ���Ҙa�� ....�a��ɂ��`�F�b�N��"��"���i�[ *
023310* �� ���ҔN ......���ҔN�v�Ɋi�[                 *
023320* �� ���Ҍ� ......���Ҍ��v�Ɋi�[                 *
023330* �� ���ғ� ......���ғ��v�Ɋi�[                 *
023340* �� ���� ........���̃}�X�^��葱���v�Ɏ擾     *
023350**************************************************
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
      *         END-IF
      *
023450         MOVE ��|�{�p�N       TO �{�p�N�v
023460         MOVE ��|�{�p��       TO �{�p���v
023470         MOVE ��|���Ҕԍ�     TO ���Ҕԍ��v
023480*         MOVE ��|�L��         TO �L���v
023490*         MOVE ��|�ԍ�         TO �ԍ��v
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
023500         MOVE ��|�ی��Ҕԍ�   TO �ی��Ҕԍ��v
023510         MOVE ��|�ی����     TO �ی���ʂv
               PERFORM �ی���ʕҏW
023520** �S���y�؂̎}�ԏ�
023530         IF ( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(1:6) = "133033" )
023540            MOVE ��|�ی��Ҕԍ�(1:6)  TO �ی��Ҕԍ��v
023550         END-IF
023560**
023570         MOVE ��|��p���S�Ҕԍ����� TO �s�����ԍ��v
023580         MOVE ��|��v�Ҕԍ�����     TO �󋋎Ҕԍ��v
023590         MOVE ��|��ی��҃J�i TO ��ی��҃J�i�v
023600         MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ����v
022240         STRING ��|�Z���P  DELIMITED BY SPACE
022250                ��|�Z���Q  DELIMITED BY SPACE
022260           INTO ��ی��ҏZ���v
022270         END-STRING
023610*         MOVE ��|�Z���P       TO ��ی��ҏZ���P�v
023620*         MOVE ��|�Z���Q       TO ��ی��ҏZ���Q�v
023630         MOVE ��|���҃J�i     TO ���҃J�i�v
023640         MOVE ��|���Ҏ���     TO ���Ҏ����v
023650         MOVE ��|���җX�֔ԍ��P TO �X�֔ԍ��P�v
023660         MOVE ��|���җX�֔ԍ��Q TO �X�֔ԍ��Q�v
022240*         STRING ��|���ҏZ���P  DELIMITED BY SPACE
022250*                ��|���ҏZ���Q  DELIMITED BY SPACE
022260*           INTO ���ҏZ���v
022270*         END-STRING
023670         MOVE ��|���ҏZ���P   TO ���ҏZ���P�v
023680         MOVE ��|���ҏZ���Q   TO ���ҏZ���Q�v
      */ �d�b�ԍ��ǉ� /42505
               IF ��|���ғd�b�ԍ� NOT = SPACE
      *            STRING "�d�b:"            DELIMITED BY SIZE
      *                   ��|���ғd�b�ԍ�   DELIMITED BY SPACE
      *              INTO �d�b�ԍ��v
      *            END-STRING
                  MOVE ��|���ғd�b�ԍ� TO �d�b�ԍ��v
               END-IF
023690         EVALUATE ��|���Ґ���
023700         WHEN 1
023710             MOVE NC"��"  TO �j�`�F�b�N�v
023720         WHEN 2
023730             MOVE NC"��"  TO ���`�F�b�N�v
023740         END-EVALUATE
025110         EVALUATE ��|���Ґ���
025120         WHEN 1
025130             MOVE NC"�j"  TO ���ʂv
025140         WHEN 2
025150             MOVE NC"��"  TO ���ʂv
025160         END-EVALUATE
023750         EVALUATE ��|���Ҙa��
023760         WHEN 1
023770             MOVE NC"��"  TO �����`�F�b�N�v
023780         WHEN 2
023790             MOVE NC"��"  TO �吳�`�F�b�N�v
023800         WHEN 3
023810             MOVE NC"��"  TO ���a�`�F�b�N�v
023820         WHEN 4
023830             MOVE NC"��"  TO �����`�F�b�N�v
023840         END-EVALUATE
023850         EVALUATE ��|���Ҙa��
023860         WHEN 1
023870             MOVE NC"����"  TO �����v
023880         WHEN 2
023890             MOVE NC"�吳"  TO �����v
023900         WHEN 3
023910             MOVE NC"���a"  TO �����v
023920         WHEN 4
023930             MOVE NC"����"  TO �����v
023940         END-EVALUATE
023950*
023960         MOVE ��|���ҔN  TO ���ҔN�v
023970         MOVE ��|���Ҍ�  TO ���Ҍ��v
023980         MOVE ��|���ғ�  TO ���ғ��v
023990* ����
024000***         EVALUATE �ی���ʂv�q
024010* ���q���͖�������"�{�l"
024020***         WHEN  09
024030***             MOVE NC"�{�l" TO �����v
024040* �ސE
024050***         WHEN  08
024060***             IF ( �{�l�Ƒ��敪�v�q = 1 ) AND ( ��|���ю呱�� = 1 )
024070***                MOVE NC"���ю�" TO �����v
024080***             ELSE
024090***                PERFORM �Ƒ������Z�b�g
024100***             END-IF
024110* ���̑�
024120***         WHEN OTHER
024130***             IF ( �{�l�Ƒ��敪�v�q = 1 )
024140***                MOVE NC"�{�l"   TO �����v
024150***             ELSE
024160***                PERFORM �Ƒ������Z�b�g
024170***             END-IF
024180***         END-EVALUATE
024190**
025660         IF  �{�l�Ƒ��敪�v�q = 1 
025670             MOVE NC"�{�l"    TO �����v
025680         ELSE
025690             MOVE NC"�Ƒ�"    TO �����v
025700         END-IF
024190**
024200***         PERFORM ���ʋ敪�R�����g�Z�b�g
024210**
026580         EVALUATE ��|���ʋ敪
026590         WHEN 1
026600             MOVE NC"��"              TO �V�O�Έȏ�`�F�b�N�v
                   MOVE 1                   TO ������v
                   IF ��|�ی���� = 05
026500                 MOVE "����P�����S"  TO ���ʃR�����g�v
                   ELSE
026500                 MOVE "����P�����S"  TO ���ʃR�����g�v
                   END-IF
026610         WHEN 2
026600             MOVE NC"��"              TO �V�O�Έȏ�`�F�b�N�v
                   MOVE 2                   TO ������v
                   IF ��|�ی���� = 05
026500                 MOVE "����Q�����S"  TO ���ʃR�����g�v
                   ELSE
026500                 MOVE "����Q�����S"  TO ���ʃR�����g�v
                   END-IF
026621         WHEN 3
026600             MOVE NC"��"              TO �V�O�Έȏ�`�F�b�N�v
                   MOVE 3                   TO ������v
                   IF ��|�ی���� = 05
026500                 MOVE "����R�����S"  TO ���ʃR�����g�v
                   ELSE
026500                 MOVE "����R�����S"  TO ���ʃR�����g�v
                   END-IF
026630         WHEN 6
026600             MOVE NC"��"              TO ���A�w�`�F�b�N�v
026500             MOVE "���A�w�Q�����S"    TO ���ʃR�����g�v
026650         END-EVALUATE
024220     END-IF.
024230*
024240* �ی���ʃ`�F�b�N
024250     EVALUATE �ی���ʂv�q
024260     WHEN 02
024270         MOVE NC"��" TO ���`�F�b�N�v
024280     WHEN 03
024290         MOVE NC"��" TO �g�`�F�b�N�v
024300     WHEN 06
024310         MOVE NC"��" TO ���`�F�b�N�v
024320     WHEN 07
024330         MOVE NC"��" TO �D�`�F�b�N�v
024340     WHEN 04
024350     WHEN 09
024360         MOVE NC"��" TO ���`�F�b�N�v
024370     WHEN 01
024380         MOVE NC"��" TO ���`�F�b�N�v
024390     WHEN 08
024400         MOVE NC"��" TO �ރ`�F�b�N�v
024390     WHEN 05
024400         MOVE NC"��" TO ���`�F�b�N�v
024410     END-EVALUATE.
024420*
024430* ������ʃ`�F�b�N
024440     EVALUATE ������ʂv�q
024450     WHEN  50
024460         CONTINUE
024470     WHEN  51
024480        MOVE NC"��" TO �V�`�F�b�N�v
024490     WHEN  52
024500        MOVE NC"��" TO ��`�F�b�N�v
024510     WHEN  53
024520        MOVE NC"��" TO ��`�F�b�N�v
024530     WHEN  54
024540        MOVE NC"��" TO ���`�F�b�N�v
024550     WHEN  55
024560        MOVE NC"��" TO ���`�F�b�N�v
024570     WHEN  OTHER
024580            CONTINUE
024590     END-EVALUATE.
024600*
      ***     IF ��|������� = 60
014760***         PERFORM ������擾
      ***         IF ������v = NC"��"
      ***             MOVE NC"��" TO ���`�F�b�N�v
      ***             MOVE SPACE  TO ������v
      ***         ELSE
      ***             MOVE NC"��" TO �����`�F�b�N�v
      ***         END-IF
      ***     END-IF.
038330*================================================================*
038340 �ی���ʕҏW SECTION.
038350*================================================================*
           EVALUATE �ی���ʂv
           WHEN 1
               IF ��|�ی��Ҕԍ�(3:1) = 3
                   MOVE NC"���g"   TO �ی���ʐe�v
               ELSE
                   MOVE NC"��"     TO �ی���ʐe�v
               END-IF
           WHEN 2
               IF (��|�ی��Ҕԍ�(1:2) = 01) AND
                  (��|�ی��Ҕԍ�(5:4) NOT = SPACE)
                   MOVE NC"��"     TO �ی���ʐe�v
               ELSE
                   MOVE NC"��"     TO �ی���ʐe�v
               END-IF
           WHEN 3
               MOVE NC"�g"         TO �ی���ʐe�v
           WHEN 4
               MOVE NC"��"         TO �ی���ʐe�v
           WHEN 5
               MOVE NC"���"       TO �ی���ʐe�v
           WHEN 6
               MOVE NC"��"         TO �ی���ʐe�v
           WHEN 7
               MOVE NC"�D"         TO �ی���ʐe�v
           WHEN 8
               MOVE NC"����"       TO �ی���ʐe�v
           WHEN 9
               MOVE NC"��"         TO �ی���ʐe�v
           END-EVALUATE.
      *
           PERFORM ������擾�Q.
           IF ������v NOT = SPACE
               STRING �ی���ʐe�v   DELIMITED BY SPACE
                      NC"�i"         DELIMITED BY SIZE
                      ������v       DELIMITED BY SPACE
                      NC"�j"         DELIMITED BY SIZE
                 INTO �ی���ʕҏW�v
               END-STRING
           ELSE
               MOVE �ی���ʐe�v   TO �ی���ʕҏW�v
           END-IF.
      *
038330*================================================================*
038340 ������擾�Q SECTION.
038350*================================================================*
039830     MOVE SPACE TO ������v.
039840*
039850     EVALUATE ������ʂv�q 
039860*** ���� (���ۂ͂��̑������ŁA�Y���Ȃ�)
039870     WHEN  50
039880         CONTINUE
039970*** ��q
039980     WHEN  52
040030         MOVE NC"��"    TO ������v
040050*** �g��
040060     WHEN  53
040070            MOVE NC"��"    TO ������v
040110*** ���c�� 
040120     WHEN  55
040140            MOVE NC"�q"    TO ������v
040150*** ���̑�
040160     WHEN  60
040170***            MOVE NC"��"    TO ������v
040171         IF ��p���S�Ҕԍ������v�q(1:4) = "8923"
040172             MOVE NC"��"    TO ������v
040173         END-IF
040180     WHEN  OTHER
040190            CONTINUE
040200     END-EVALUATE.
040210*
040211     IF (( �ی���ʂv�q = 05 ) AND ( �ی��Ҕԍ��v�q(1:5) = "39231" ) AND
040212         ( ��|�������S���Ə� = 1 ))
040213         MOVE NC"��"    TO ������v
040214     END-IF.
040215*
024610*================================================================*
024620 �Ƒ������Z�b�g SECTION.
024630*
024640     MOVE 05       TO ���|�敪�R�[�h.
024650     MOVE ��|���� TO ���|���̃R�[�h.
024660     READ ���̃}�X�^
024670     INVALID KEY
024680         MOVE SPACE    TO �����v
024690     NOT INVALID KEY
024700         MOVE ���|���� TO �����v
024710     END-READ.
024720*
024730*================================================================*
024740 ���ʋ敪�R�����g�Z�b�g SECTION.
024750*----------------------------------------------------------------*
024760* 14/10�`�@���ʋ敪�R�����g��
024770*----------------------------------------------------------------*
024780     IF ( ��|�{�p�a��N�� >= 41410 )
024790        IF ( ��|������ = ZERO )
024800           EVALUATE ��|���ʋ敪
024810           WHEN 1
024820              MOVE "70�Έȏ� 1��"  TO ���ʃR�����g�v
024830           WHEN 2
024840              MOVE "70�Έȏ� 2��"  TO ���ʃR�����g�v
024841           WHEN 3
024842              MOVE "70�Έȏ� 3��"  TO ���ʃR�����g�v
024850           WHEN 6
024861              IF ��|�{�p�a��N�� < 42004
024863                 MOVE "3�Ζ���"       TO ���ʃR�����g�v
024864              ELSE
025063                 MOVE "���A�w�Q�����S"  TO ���ʃR�����g�Q�v
024867              END-IF
024870           END-EVALUATE
024880        END-IF
024890     END-IF.
024900*
024910*---  �s�����Ǝ��d�l -----*
024920* �����s�̂݁� ���ʋ敪1,2,3(����ҁj�̎��A�u�O�v���E��Ɉ�
024930*              �e���V�l�̎��A�ی��Ҕԍ����ɂ́A�Q�V�ԍ�����
024940     IF ( ��|�{�p�a��N�� >= 41410 )
024950        IF ( ��|��p���S�Ҕԍ�����(3:2) = "13" )
024960           IF ( ��|������ = ZERO )
024970              IF ( ��|���ʋ敪 = 1 OR 2 OR 3 )
024980                 MOVE NC"�O" TO ���ʃ}�[�N�v
024990              END-IF
025000           ELSE
025010              MOVE ��|��p���S�Ҕԍ�  TO �ی��Ҕԍ��v
025020           END-IF
025030        END-IF
025040     END-IF.
025050*
025060* ���m���̂݁� 41�V�l�̕��S����K�p���Ɉ�
025070     IF ( ��|�{�p�a��N�� >= 41410 )
025080        IF ( ��|��p���S�Ҕԍ�����(3:2) = "23" ) AND
025090           ( ��|������� = 51 )
025142               EVALUATE ��|�������S���Ə�
025143               WHEN 2
025144                  MOVE "41�V�l �Q��"   TO ���ʃR�����g�Q�v
025145               WHEN 3
025146                  MOVE "41�V�l �R��"   TO ���ʃR�����g�Q�v
025147               WHEN OTHER
025148                  MOVE "41�V�l �P��"   TO ���ʃR�����g�Q�v
025149               END-EVALUATE
025152        END-IF
025160     END-IF.
025170*
027472* 20/04�`�@���������ʋ敪�R�����g��
027476     IF ��|�{�p�a��N�� >= 42004
027477         IF ��|�ی���� = 05
027478            EVALUATE ��|���ʋ敪
027479            WHEN 1
027480               MOVE "����҂P��"  TO ���ʃR�����g�Q�v
027481            WHEN 2
027482               MOVE "����҂Q��"  TO ���ʃR�����g�Q�v
027483            WHEN 3
027484               MOVE "����҂R��"  TO ���ʃR�����g�Q�v
027492            END-EVALUATE
027493         END-IF
027494     END-IF.
      *
025180*================================================================*
025190 �����f�[�^�擾 SECTION.
025200*================================================================*
025210**************************************************
025220* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾 *
025230* �� ������...���ʁ{������ʂɂĉ��H���Ċi�[     *
025240* �� �����N.......�����N�v                       *
025250* �� ������.......�������v                       *
025260* �� ������.......�������v                       *
025270* �� �J�n�N.......�����N�v                       *
025280* �� �J�n��.......�������v                       *
025290* �� �J�n��.......�������v                       *
025300* �� �I���N.......�I���N�v                       *
025310* �� �I����.......�I�����v                       *
025320* �� �I����.......�I�����v                       *
025330* �� ������.......�������v                       *
025340* �� �]�A�敪 ....�敪�ɂ��`�F�b�N��"��"���i�[ *
025350* �� �������q ....�敪�ɂ��`�F�b�N��"��"���i�[ *
025360* �� �o�߃R�[�h...�o�߃}�X�^���擾             *
025370**************************************************
           IF ���|���R�[�h NOT = SPACE
025470         MOVE ���|���ʐ�                   TO ���ʐ��v
025480         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
025490                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
025500             MOVE ���|�������(���ʂb�m�s) TO ������ʂv(���ʂb�m�s)
025510             MOVE ���|����(���ʂb�m�s)     TO ���ʂv(���ʂb�m�s)
025520             MOVE ���|���E�敪(���ʂb�m�s) TO ���E�敪�v(���ʂb�m�s)
025530             MOVE ���|�����ʒu�ԍ�(���ʂb�m�s)
025540                                           TO �����ʒu�ԍ��v(���ʂb�m�s)
025550********************************************************
025560* ���j�S�_...���ʖ�1+������ʁ{���ʖ�2�ɂĉ��H���Ċi�[ *
025570********************************************************
025580* �������
025590             MOVE SPACE                     TO �������̂v
025600             MOVE 03                        TO ���|�敪�R�[�h
025610             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
025620             READ ���̃}�X�^
025630             INVALID KEY
025640                 MOVE SPACE        TO �������̂v
025650             NOT INVALID KEY
025660                 MOVE ���|�������� TO �������̂v
025670             END-READ
025680* ����
020710             MOVE SPACE                    TO �������v(���ʂb�m�s)
032680*
032690             PERFORM ���ʖ��̖�������
025870*
025880             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
025890             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
025900             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
025910             MOVE ���|�J�n�N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
025920             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
025930             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
025940             IF ( ���|�]�A�敪(���ʂb�m�s) = 9 )
025950                 MOVE 99                   TO �I���N�v(���ʂb�m�s)
025960                 MOVE 99                   TO �I�����v(���ʂb�m�s)
025970                 MOVE 99                   TO �I�����v(���ʂb�m�s)
025980             ELSE
025990                 MOVE ���|�I���N(���ʂb�m�s)   TO �I���N�v(���ʂb�m�s)
026000                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
026010                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
026020             END-IF
026030* �o�ߗ��̎擾
026040             MOVE 01                         TO �o�|�敪�R�[�h
026050             MOVE ���|�o�߃R�[�h(���ʂb�m�s) TO �o�|�o�߃R�[�h
026060             READ �o�߃}�X�^
026070             INVALID KEY
026080                 MOVE ZERO            TO ���ʂb�m�s�v(���ʂb�m�s)
026090                 MOVE SPACE           TO ���ʋ�؂v(���ʂb�m�s)
026100                 MOVE SPACE           TO �o�ߗ��̂v(���ʂb�m�s)
026110             NOT INVALID KEY
026120                 EVALUATE ���ʂb�m�s
026130                 WHEN 1
026140                     MOVE NC"�@" TO �o�ߕ��ʂv
026150                 WHEN 2
026160                     MOVE NC"�A" TO �o�ߕ��ʂv
026170                 WHEN 3
026180                     MOVE NC"�B" TO �o�ߕ��ʂv
026190                 WHEN 4
026200                     MOVE NC"�C" TO �o�ߕ��ʂv
026210                 WHEN 5
026220                     MOVE NC"�D" TO �o�ߕ��ʂv
026230                 END-EVALUATE
026240                 STRING  �o�ߕ��ʂv     DELIMITED BY SPACE
026250                         �o�|�o�ߗ���   DELIMITED BY SPACE
026260                        INTO ����o�ߗ��̂v(���ʂb�m�s)
026270                 END-STRING
026280             END-READ
026290*
026300             MOVE ���|�]�A�敪(���ʂb�m�s) TO �]�A�敪�v(���ʂb�m�s)
026310             EVALUATE ���|�]�A�敪(���ʂb�m�s)
026320             WHEN 1
026330             WHEN 2
026340                 MOVE NC"��"               TO �����`�F�b�N�v(���ʂb�m�s)
026350             WHEN 3
026360                 MOVE NC"��"               TO ���~�`�F�b�N�v(���ʂb�m�s)
026370             WHEN 4
026380                 MOVE NC"��"               TO �]��`�F�b�N�v(���ʂb�m�s)
026390             END-EVALUATE
026400*
                   MOVE ���Z�|���ʎ�����(���ʂb�m�s) TO �������v(���ʂb�m�s)
026410         END-PERFORM
026420* �V�K/�p�� �`�F�b�N
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
026480* �}�Ԕ���p
026490         MOVE ���|�J�n�f�Ó��蓮�敪   TO  �J�n�f�Ó��蓮�敪�v
026500*
026510* ������������敪
026520         MOVE ���|���Z������������敪 TO ���Z������������敪�v
027880         MOVE ���|���Z�������R����敪 TO ���Z�������R����敪�v
026530*
026540     END-IF.
026550*
026560*================================================================*
030910 ���ʖ��̖������� SECTION.
030920*
006490     STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
009980            �������̂v                    DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
006520       INTO �������v(���ʂb�m�s)
006570     END-STRING.
026720*
026730*================================================================*
026740 �������擾 SECTION.
026750*================================================================*
026760********************
026770* �����f�[�^�Z�b�g *
026780********************
026790*    ****************************************************************
026800*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
026810*    ****************************************************************
026820     MOVE ���Z�|������                 TO �������v�q.
026830     IF ( ���Z�|���ԊO = 1 )
026840         MOVE NC"��"                   TO ���ԊO�`�F�b�N�v
026850     END-IF.
026860     IF ( ���Z�|�x�� = 1 )
026870         MOVE NC"��"                   TO �x���`�F�b�N�v
026880     END-IF.
026890     IF ( ���Z�|�[�� = 1 )
026900         MOVE NC"��"                   TO �[��`�F�b�N�v
026910     END-IF.
           MOVE ���Z�|���������k��           TO ���k���v�q.
026920*
026930     MOVE ���Z�|�������Z��             TO  �������Z���v�q.
026940     MOVE ���Z�|�Č���                 TO  �Č����v�q.
026950     MOVE ���Z�|���Ë���               TO  ���Ë����v�q.
026960     MOVE ���Z�|���É�               TO  ���É񐔂v�q.
026970     MOVE ���Z�|���×�                 TO  ���×��v�q.
026980     MOVE ���Z�|���É��Z��             TO  ���É��Z���v�q.
026990*
027000     IF ( ���Z�|��� = 1 )
027010         MOVE NC"��"                   TO ��ԃ`�F�b�N�v
027020     END-IF.
029870     IF ( ���Z�|��H = 1 )
029880         MOVE NC"��"                   TO ��H�`�F�b�N�v
029890     END-IF.
027030     IF ( ���Z�|�\���J�� = 1 )
027040         MOVE NC"��"                   TO �\���J��`�F�b�N�v
027050     END-IF.
027060*
027070     MOVE ���Z�|�������q���Z��         TO  �������q���Z���v�q.
027080*
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
027180*
027190     MOVE ���Z�|�{�p���񋟗�         TO  �{�p���񋟗��v�q.
027200* ���v
027210     MOVE ���Z�|���v                   TO ���v�v.
027220********************
027230* ���񏈒u���Z�b�g *
027240********************
027250     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
027260             UNTIL ( ���ʂb�m�s > ���ʐ��v )
027270         MOVE ���Z�|���񏈒u��(���ʂb�m�s) TO ���񏈒u���v�q(���ʂb�m�s)
027280     END-PERFORM.
027290     MOVE ���Z�|���񏈒u�����v       TO ���񏈒u�����v�v.
027300********************
027310* �����������Z�b�g *
027320********************
027330*    **********
027340*    * �P���� *
027350*    **********
027360     MOVE ���Z�|��ÒP���P             TO ��ÒP���P�v�q.
027370     MOVE ���Z�|��É񐔂P             TO ��É񐔂P�v�q.
027380     MOVE ���Z�|��×��P               TO ��×��P�v�q.
027390     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
027400     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
027410     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
027420     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
027430     MOVE ���Z�|�d�É񐔂P             TO �d�É񐔂P�v�q.
027440     MOVE ���Z�|�d�×��P               TO �d�×��P�v�q.
027450     MOVE ���Z�|���v�P                 TO ���v�P�v�q.
027460     MOVE ���Z�|�����������P           TO �����������P�v�q.
027470     MOVE ���Z�|���������v�P           TO ���������v�P�v�q.
027480*    **********
027490*    * �Q���� *
027500*    **********
027510     MOVE ���Z�|��ÒP���Q             TO ��ÒP���Q�v�q.
027520     MOVE ���Z�|��É񐔂Q             TO ��É񐔂Q�v�q.
027530     MOVE ���Z�|��×��Q               TO ��×��Q�v�q.
027540     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
027550     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
027560     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
027570     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
027580     MOVE ���Z�|�d�É񐔂Q             TO �d�É񐔂Q�v�q.
027590     MOVE ���Z�|�d�×��Q               TO �d�×��Q�v�q.
027600     MOVE ���Z�|���v�Q                 TO ���v�Q�v�q.
027610     MOVE ���Z�|�����������Q           TO �����������Q�v�q.
027620     MOVE ���Z�|���������v�Q           TO ���������v�Q�v�q.
027630*    ****************
027640*    * �R���ʁ^�W�� *
027650*    ****************
027660     MOVE ���Z�|��ÒP���R�W             TO ��ÒP���R�W�v�q.
027670     MOVE ���Z�|��É񐔂R�W             TO ��É񐔂R�W�v�q.
027680     MOVE ���Z�|��×��R�W               TO ��×��R�W�v�q.
027690     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
027700     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
027710     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
027720     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
027730     MOVE ���Z�|�d�É񐔂R�W             TO �d�É񐔂R�W�v�q.
027740     MOVE ���Z�|�d�×��R�W               TO �d�×��R�W�v�q.
027750     MOVE ���Z�|���v�R�W                 TO ���v�R�W�v�q.
027760     MOVE ���Z�|�����ʍ����v�R�W         TO �����ʍ����v�R�W�v�q.
027770     MOVE ���Z�|�����������R�W           TO �����������R�W�v�q.
027780     MOVE ���Z�|���������v�R�W           TO ���������v�R�W�v�q.
027790*    ****************
027800*    * �R���ʁ^10�� *
027810*    ****************
027820     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
027830     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
027840     MOVE ���Z�|��ÒP���R�O             TO ��ÒP���R�O�v�q.
027850     MOVE ���Z�|��É񐔂R�O             TO ��É񐔂R�O�v�q.
027860     MOVE ���Z�|��×��R�O               TO ��×��R�O�v�q.
027870     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
027880     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
027890     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
027900     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
027910     MOVE ���Z�|�d�É񐔂R�O             TO �d�É񐔂R�O�v�q.
027920     MOVE ���Z�|�d�×��R�O               TO �d�×��R�O�v�q.
027930     MOVE ���Z�|���v�R�O                 TO ���v�R�O�v�q.
027940     MOVE ���Z�|�����������R�O           TO �����������R�O�v�q.
027950     MOVE ���Z�|���������v�R�O           TO ���������v�R�O�v�q.
027960*    ****************
027970*    * �S���ʁ^�T�� *
027980*    ****************
027990     MOVE ���Z�|��ÒP���S�T             TO ��ÒP���S�T�v�q.
028000     MOVE ���Z�|��É񐔂S�T             TO ��É񐔂S�T�v�q.
028010     MOVE ���Z�|��×��S�T               TO ��×��S�T�v�q.
028020     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
028030     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
028040     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
028050     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
028060     MOVE ���Z�|�d�É񐔂S�T             TO �d�É񐔂S�T�v�q.
028070     MOVE ���Z�|�d�×��S�T               TO �d�×��S�T�v�q.
028080     MOVE ���Z�|���v�S�T                 TO ���v�S�T�v�q.
028090     MOVE ���Z�|�����ʍ����v�S�T         TO �����ʍ����v�S�T�v�q.
028100     MOVE ���Z�|�����������S�T           TO �����������S�T�v�q.
028110     MOVE ���Z�|���������v�S�T           TO ���������v�S�T�v�q.
028120*    ****************
028130*    * �S���ʁ^�W�� *
028140*    ****************
028150     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
028160     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
028170     MOVE ���Z�|��ÒP���S�W             TO ��ÒP���S�W�v�q.
028180     MOVE ���Z�|��É񐔂S�W             TO ��É񐔂S�W�v�q.
028190     MOVE ���Z�|��×��S�W               TO ��×��S�W�v�q.
028200     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
028210     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
028220     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
028230     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
028240     MOVE ���Z�|�d�É񐔂S�W             TO �d�É񐔂S�W�v�q.
028250     MOVE ���Z�|�d�×��S�W               TO �d�×��S�W�v�q.
028260     MOVE ���Z�|���v�S�W                 TO ���v�S�W�v�q.
028270     MOVE ���Z�|�����ʍ����v�S�W         TO �����ʍ����v�S�W�v�q.
028280     MOVE ���Z�|�����������S�W           TO �����������S�W�v�q.
028290     MOVE ���Z�|���������v�S�W           TO ���������v�S�W�v�q.
028300*    ****************
028310*    * �S���ʁ^10�� *
028320*    ****************
028330     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
028340     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
028350     MOVE ���Z�|��ÒP���S�O             TO ��ÒP���S�O�v�q.
028360     MOVE ���Z�|��É񐔂S�O             TO ��É񐔂S�O�v�q.
028370     MOVE ���Z�|��×��S�O               TO ��×��S�O�v�q.
028380     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
028390     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
028400     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
028410     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
028420     MOVE ���Z�|�d�É񐔂S�O             TO �d�É񐔂S�O�v�q.
028430     MOVE ���Z�|�d�×��S�O               TO �d�×��S�O�v�q.
028440     MOVE ���Z�|���v�S�O                 TO ���v�S�O�v�q.
028450     MOVE ���Z�|�����������S�O           TO �����������S�O�v�q.
028460     MOVE ���Z�|���������v�S�O           TO ���������v�S�O�v�q.
028470*    *****************
028480*    * �T���ʁ^2.5�� *
028490*    *****************
028500     MOVE ���Z�|��ÒP���T�Q             TO ��ÒP���T�Q�v�q.
028510     MOVE ���Z�|��É񐔂T�Q             TO ��É񐔂T�Q�v�q.
028520     MOVE ���Z�|��×��T�Q               TO ��×��T�Q�v�q.
028530     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
028540     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
028550     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
028560     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
028570     MOVE ���Z�|�d�É񐔂T�Q             TO �d�É񐔂T�Q�v�q.
028580     MOVE ���Z�|�d�×��T�Q               TO �d�×��T�Q�v�q.
028590     MOVE ���Z�|���v�T�Q                 TO ���v�T�Q�v�q.
028600     MOVE ���Z�|�����ʍ����v�T�Q         TO �����ʍ����v�T�Q�v�q.
028610     MOVE ���Z�|�����������T�Q           TO �����������T�Q�v�q.
028620     MOVE ���Z�|���������v�T�Q           TO ���������v�T�Q�v�q.
028630*    ****************
028640*    * �T���ʁ^�T�� *
028650*    ****************
028660     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
028670     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
028680     MOVE ���Z�|��ÒP���T�T             TO ��ÒP���T�T�v�q.
028690     MOVE ���Z�|��É񐔂T�T             TO ��É񐔂T�T�v�q.
028700     MOVE ���Z�|��×��T�T               TO ��×��T�T�v�q.
028710     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
028720     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
028730     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
028740     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
028750     MOVE ���Z�|�d�É񐔂T�T             TO �d�É񐔂T�T�v�q.
028760     MOVE ���Z�|�d�×��T�T               TO �d�×��T�T�v�q.
028770     MOVE ���Z�|���v�T�T                 TO ���v�T�T�v�q.
028780     MOVE ���Z�|�����ʍ����v�T�T         TO �����ʍ����v�T�T�v�q.
028790     MOVE ���Z�|�����������T�T           TO �����������T�T�v�q.
028800     MOVE ���Z�|���������v�T�T           TO ���������v�T�T�v�q.
028810*    ****************
028820*    * �T���ʁ^�W�� *
028830*    ****************
028840     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
028850     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
028860     MOVE ���Z�|��ÒP���T�W             TO ��ÒP���T�W�v�q.
028870     MOVE ���Z�|��É񐔂T�W             TO ��É񐔂T�W�v�q.
028880     MOVE ���Z�|��×��T�W               TO ��×��T�W�v�q.
028890     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
028900     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
028910     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
028920     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
028930     MOVE ���Z�|�d�É񐔂T�W             TO �d�É񐔂T�W�v�q.
028940     MOVE ���Z�|�d�×��T�W               TO �d�×��T�W�v�q.
028950     MOVE ���Z�|���v�T�W                 TO ���v�T�W�v�q.
028960     MOVE ���Z�|�����ʍ����v�T�W         TO �����ʍ����v�T�W�v�q.
028970     MOVE ���Z�|�����������T�W           TO �����������T�W�v�q.
028980     MOVE ���Z�|���������v�T�W           TO ���������v�T�W�v�q.
028990*    ****************
029000*    * �T���ʁ^10�� *
029010*    ****************
029020     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
029030     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
029040     MOVE ���Z�|��ÒP���T�O             TO ��ÒP���T�O�v�q.
029050     MOVE ���Z�|��É񐔂T�O             TO ��É񐔂T�O�v�q.
029060     MOVE ���Z�|��×��T�O               TO ��×��T�O�v�q.
029070     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
029080     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
029090     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
029100     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
029110     MOVE ���Z�|�d�É񐔂T�O             TO �d�É񐔂T�O�v�q.
029120     MOVE ���Z�|�d�×��T�O               TO �d�×��T�O�v�q.
029130     MOVE ���Z�|���v�T�O                 TO ���v�T�O�v�q.
029140     MOVE ���Z�|�����������T�O           TO �����������T�O�v�q.
029150     MOVE ���Z�|���������v�T�O           TO ���������v�T�O�v�q.
029160*
029170*================================================================*
029180 �{�p�L�^�擾 SECTION.
029190*================================================================*
029200************************************************************
029210* ��P�f�[�^���畉���f�[�^�e���ȉ��̏����擾           *
029220* �� �������Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
029230* �� ���É��Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
029240************************************************************
029250     MOVE  SPACE  TO  �����Č��t���O.
029260     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���ʐ��v
029270         IF ( �{�p�N�v = �����N�v(���ʂb�m�s) ) AND
029280            ( �{�p���v = �������v(���ʂb�m�s) )
029290             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
029300             MOVE �}�Ԃv�q              TO �{�L�|�}��
029310             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
029320             MOVE �����N�v(���ʂb�m�s)  TO �J�n�N�v(���ʂb�m�s) �{�L�|�{�p�N
029330             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
029340             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
029350         ELSE
029360             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
029370             MOVE �}�Ԃv�q              TO �{�L�|�}��
029380             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
029390             MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
029400             MOVE �{�p���v�q            TO �{�L�|�{�p��
029410             MOVE ZERO                  TO �{�L�|�{�p��
029420         END-IF
029430         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
029440                                      �{�L�|�{�p�a��N����
029450         END-START
029460         IF ( ��ԃL�[ = "00" )
029480             MOVE ZERO  TO �I���N�v�s
029490             MOVE ZERO  TO �I�����v�s
029500             MOVE ZERO  TO �I�����v�s
029510             MOVE SPACE TO �I���t���O�Q
029520             PERFORM �{�p�L�^�e�Ǎ�
029530             IF  ( �I���t���O�Q      = SPACE   ) AND
029540                 ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
029550                 ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
029560                 ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
029570                 ( �{�L�|�{�p��      = �{�p���v�q     ) 
029580*
029590*        *****************************************************************
029600*        * �J�n�N���� ( ���̕��ʂ����������łȂ����A
029610*                       ���������ł��}�Ԃ����鎞�́A�ŏ��̎{�p�����J�n��)*
029620*        *****************************************************************
029630                 IF ( �{�p�N�v NOT = �����N�v(���ʂb�m�s) ) OR
029640                    ( �{�p���v NOT = �������v(���ʂb�m�s) ) OR
029650                    ( �J�n�f�Ó��蓮�敪�v = 1 )
029660                     MOVE �{�L�|�{�p�N   TO �J�n�N�v(���ʂb�m�s)
029670                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
029680                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
029690                 END-IF
029700             END-IF
029710             PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
029720                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
029730                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
029740                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
029750                           ( �{�L�|�{�p��     NOT = �{�p���v�q       ) OR
029760                           ( �{�L�|�{�p��         > �I�����v(���ʂb�m�s))
029770*               **********
029780*               * ������ *
029790*               **********
029810                MOVE �{�L�|�{�p�N               TO �I���N�v�s
029820                MOVE �{�L�|�{�p��               TO �I�����v�s
029830                MOVE �{�L�|�{�p��               TO �I�����v�s
029840*
029850                PERFORM �{�p�L�^�e�Ǎ�
029860            END-PERFORM
029870        END-IF
029880*       **************************
029890*       * �p���F�I���N�����Z�b�g *
029900*       **************************
029910        IF ( �]�A�敪�v(���ʂb�m�s) = 9 )
029920            MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
029930            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
029940            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
029950        END-IF
029960        IF ( �I���N�����v(���ʂb�m�s) > �󗝔N�����v )
029970            MOVE �I���N�v(���ʂb�m�s) TO �󗝔N�v
029980            MOVE �I�����v(���ʂb�m�s) TO �󗝌��v
029990            MOVE �I�����v(���ʂb�m�s) TO �󗝓��v
030000        END-IF
030010     END-PERFORM.
030020*
030030** ----- �O�������݂̂��𔻒� -----------*
030040*
030050*     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
030060*     MOVE �}�Ԃv�q              TO �{�L�|�}��.
030070*     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
030080*     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
030090*     MOVE �{�p���v�q            TO �{�L�|�{�p��.
030100*     MOVE ZERO                  TO �{�L�|�{�p��.
030110*     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
030120*                                  �{�L�|�{�p�a��N����
030130*     END-START.
030140*     IF ( ��ԃL�[ = "00" )
030150*             MOVE SPACE TO �I���t���O�Q
030160*             PERFORM �{�p�L�^�e�Ǎ�
030170*             IF  ( �I���t���O�Q      = SPACE   ) AND
030180*                 ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
030190*                 ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
030200*                 ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
030210*                 ( �{�L�|�{�p��      = �{�p���v�q     ) 
030220** �����{�p�J�n�����Č����ǂ�������
030230*                 IF   ( �{�L�|�Č������� = 1 )
030240*                      MOVE "YES"  TO  �����Č��t���O
030250*                 END-IF
030260**
030270*             END-IF
030280*     END-IF.
030290*     IF ( �����Č��t���O = "YES" )
030300*        PERFORM �O�������̂ݔ���
030310*     END-IF.
030320*
030330*================================================================*
030340 �O�������̂ݔ��� SECTION.
030350*
030360*** �O���̒ʉ@�������������� 
030370     MOVE  SPACE            TO �O���t���O.
030380     MOVE ��|���҃R�[�h    TO �{�L�|���҃R�[�h.
030390     MOVE ��|�{�p�a��      TO �{�L�|�{�p�a��.
030400     MOVE ��|�{�p�N        TO �{�L�|�{�p�N.
030410     MOVE ��|�{�p��        TO �{�L�|�{�p��.
030420     MOVE 1                 TO �{�L�|�{�p��.
030430     START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
030440                                  �{�L�|�{�p�a��N����
030450                                  REVERSED
030460     END-START.
030470     IF ( ��ԃL�[ = "00" )
030480         MOVE SPACE  TO �I���t���O�Q
030490         PERFORM �{�p�L�^�e�Ǎ�
030500         IF ( �I���t���O�Q      = SPACE  ) AND
030510            ( �{�L�|���҃R�[�h  = ��|���҃R�[�h ) AND
030520            ( �{�L�|�f�Ë敪    = 2 ) 
030530*
030540            PERFORM �O������
030550**** �K�p�P���g�p
030560            IF ( �O���t���O = "YES" )
030570               MOVE NC"���O�������̂�"    TO  �K�p�P�v
030580            END-IF
030590**
030600         END-IF
030610     END-IF.
030620*
030630*================================================================*
030640 �O������  SECTION.
030650* 
030660*** �ǂݍ��񂾎{�p�L�^�̔N�����A�O�����ǂ������� (�N���̍��� 1 ��?)
030670      MOVE  SPACE  TO  �O���t���O.
030680      INITIALIZE  �v�Z�N�����v �J�n�N�����Q�v �I���N�����Q�v.
030690**
030700      MOVE ��|�{�p�a��    TO �I���a��Q�v.
030710      MOVE ��|�{�p�N      TO �I���N�Q�v.
030720      MOVE ��|�{�p��      TO �I�����Q�v.
030730      MOVE �{�L�|�{�p�a��  TO �J�n�a��Q�v.
030740      MOVE �{�L�|�{�p�N    TO �J�n�N�Q�v.
030750      MOVE �{�L�|�{�p��    TO �J�n���Q�v.
030760*
030770      EVALUATE TRUE
030780       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v = �I���N�Q�v)
030790            PERFORM  �O����r��
030800       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v NOT = �I���N�Q�v)
030810            PERFORM  �O����r�N
030820       WHEN  �J�n�a��Q�v NOT = �I���a��Q�v 
030830            PERFORM  �O����r����
030840      END-EVALUATE.
030850*
030860      IF ( �v�Z���v = 1 )
030870         MOVE  "YES"  TO  �O���t���O
030880      END-IF.
030890*
030900*================================================================*
030910 �O����r����  SECTION.
030920*
030930     MOVE �J�n�a��Q�v TO ���|�����敪.
030940     READ �����}�X�^
030950     NOT INVALID KEY
030960         MOVE ���|�J�n����N TO �J�n����N�v
030970     END-READ.
030980     MOVE �I���a��Q�v TO ���|�����敪.
030990     READ �����}�X�^
031000     NOT INVALID KEY
031010         MOVE ���|�J�n����N TO �I������N�v
031020     END-READ.
031030**
031040     IF ( �J�n����N�v NOT = ZERO ) AND ( �I������N�v NOT = ZERO )
031050        COMPUTE �J�n����N�v = �J�n����N�v + �J�n�N�Q�v - 1
031060        COMPUTE �I������N�v = �I������N�v + �I���N�Q�v - 1
031070*
031080        IF ( �I������N�v =  �J�n����N�v )
031090           PERFORM  �O����r��
031100        ELSE
031110           IF  ( �I������N�v >  �J�n����N�v )
031120               COMPUTE �v�Z�N�v = �I������N�v - �J�n����N�v
031130               COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
031140           ELSE
031150               MOVE ZERO TO �v�Z���v
031160           END-IF
031170        END-IF
031180     ELSE
031190        MOVE ZERO TO �v�Z���v
031200     END-IF.
031210*
031220*================================================================*
031230 �O����r�N  SECTION.
031240*
031250     IF  ( �I���N�Q�v >  �J�n�N�Q�v )
031260         COMPUTE �v�Z�N�v = �I���N�Q�v - �J�n�N�Q�v
031270         COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
031280     ELSE
031290        MOVE ZERO TO �v�Z���v
031300     END-IF.
031310*
031320*================================================================*
031330 �O����r��  SECTION.
031340*
031350     IF  ( �I�����Q�v >  �J�n���Q�v )
031360         COMPUTE �v�Z���v = �I�����Q�v - �J�n���Q�v
031370     ELSE
031380        MOVE ZERO TO �v�Z���v
031390     END-IF.
031400*
031410*================================================================*
031420 ��������擾 SECTION.
031430*================================================================*
031440* �R�J���ȏ�̒�������� "CHOUKI" ���Ă�. 
031450     MOVE  SPACE TO  �A���ԁ|�L�[.
031460     INITIALIZE      �A���ԁ|�L�[.
031470     MOVE �{�p�a��v�q  TO  �A���ԁ|�{�p�a��.
031480     MOVE �{�p�N�v�q    TO  �A���ԁ|�{�p�N.
031490     MOVE �{�p���v�q    TO  �A���ԁ|�{�p��.
031500     MOVE ���Ҕԍ��v�q  TO  �A���ԁ|���Ҕԍ�.
031510     MOVE �}�Ԃv�q      TO  �A���ԁ|�}��.
031520*
031530     CALL   "CHOUKI".
031540     CANCEL "CHOUKI".
031600*
032320*================================================================*
032330 �������Z�����擾 SECTION.
032340*================================================================*
032350*****************************************************************
032360** �������Z�����ԊO�Ɛ[��̎��A�K�p�Ɂu��t���ԁv���󎚂���B
032370**   �����̈󎚂͌�3��܂ŉ\
032380*****************************************************************
032390     IF ( ���Z�|���ԊO = 1 ) OR ( ���Z�|�[�� = 1 ) OR ( ���Z�|�x�� = 1 )
032400*
032410         MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
032420         MOVE �}�Ԃv�q              TO �{�L�|�}��
032430         MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
032440         MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
032450         MOVE �{�p���v�q            TO �{�L�|�{�p��
032460         MOVE ZERO                  TO �{�L�|�{�p��
032470         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
032480                                      �{�L�|�{�p�a��N����
032490         END-START
032500         IF ( ��ԃL�[ = "00" )
032510             MOVE ZERO  TO �������Z�J�E���g
032520             MOVE SPACE TO �I���t���O�Q
032530             PERFORM �{�p�L�^�e�Ǎ�
032540             PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
032550                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
032560                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
032570                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
032580                           ( �{�L�|�{�p��     NOT = �{�p���v�q      ) 
032590                   IF  ( �{�L�|�������Z = 1 OR 2 OR 3 ) AND ( �{�L�|�f�Ë敪 = 2 )
032600                       COMPUTE �������Z�J�E���g = �������Z�J�E���g  + 1
032610                       IF  �������Z�J�E���g <= 3
032620                           MOVE �{�L�|�������Z TO �������Z�敪�v�s(�������Z�J�E���g)
032630                           MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
032640                           MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
032650                       END-IF
032660                   END-IF
032670                   PERFORM �{�p�L�^�e�Ǎ�
032680             END-PERFORM
032690** �������Z�̎�����K�p�ɃZ�b�g
033380            IF ( �������Z���v�s(1) NOT = ZERO ) OR ( �������Z���v�s(1) NOT = ZERO ) 
                     MOVE �������Z���v�s(1) TO �������Z���v
                     MOVE ":"               TO �������Z��؂v
                     MOVE �������Z���v�s(1) TO �������Z���v
                  END-IF
033380            IF ( �������Z���v�s(2) NOT = ZERO ) OR ( �������Z���v�s(2) NOT = ZERO ) 
031910               PERFORM �������Z�K�p�Z�b�g
                  END-IF
032710         END-IF
032720*
032730     END-IF.
032740*
032750*================================================================*
032760 �������Z�K�p�Z�b�g SECTION.
032770*
032780     PERFORM VARYING �ԍ��J�E���^ FROM 1 BY 1
032790              UNTIL  �ԍ��J�E���^ > 3
032800         IF ( �������Z���v�s(�ԍ��J�E���^)  = ZERO )  AND 
032810            ( �������Z���v�s(�ԍ��J�E���^)  = ZERO ) 
032820             CONTINUE
032830         ELSE
032840* �Œ荀��
032850             EVALUATE �������Z�敪�v�s(�ԍ��J�E���^) 
032860             WHEN 1
032870                MOVE NC"���ԊO"   TO ���Z���e�v(�ԍ��J�E���^)
033320             WHEN 2
033330                MOVE NC"�x�@��"   TO ���Z���e�v(�ԍ��J�E���^)
032880             WHEN 3
032890                MOVE NC"�[�@��"   TO ���Z���e�v(�ԍ��J�E���^)
032900             END-EVALUATE
032910*
032920             MOVE NC"�F"          TO ���Z��؂v(�ԍ��J�E���^)
032930             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
032940             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
032950*
032960**** ���������{��ϊ�
032970* ����
032980             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
032990             IF ( �����v >= 10 )
033000                 MOVE �����v�P    TO �����ԍ��v�P
033010                 PERFORM ���{��ϊ�
033020                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
033030                 MOVE �����v�Q    TO �����ԍ��v�P
033040                 PERFORM ���{��ϊ�
033050                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
033060             ELSE
033070                 MOVE �����v�Q    TO �����ԍ��v�P
033080                 PERFORM ���{��ϊ�
033090                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
033100             END-IF
033110* ��
033120             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
033130             MOVE �����v�P    TO �����ԍ��v�P
033140             PERFORM ���{��ϊ�
033150             MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
033160             MOVE �����v�Q    TO �����ԍ��v�P
033170             PERFORM ���{��ϊ�
033180             MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
033190** 
033200        END-IF
033210     END-PERFORM.
033220*
033230     MOVE  �������Z�W�c�m�v(1)   TO �������Z�����P�v. 
033240     MOVE  �������Z�W�c�m�v(2)   TO �������Z�����Q�v. 
033250     MOVE  �������Z�W�c�m�v(3)   TO �������Z�����R�v. 
033260*
033270**** �K�p�P���Q���g�p�i�������R�L�ڂœK�p�P���g���Ă��鎞�́A�K�p�Q�j
033280     IF ( �������Z���v�s(2)  = ZERO ) AND ( �������Z���v�s(2)  = ZERO ) 
033290         CONTINUE
033300     ELSE
033310         IF ( �K�p�P�v  = SPACE )
033320               STRING NC"�������Z"       DELIMITED BY SIZE
033330                      �������Z�����P�v   DELIMITED BY SIZE
033340                      �������Z�����Q�v   DELIMITED BY SIZE
033350                      �������Z�����R�v   DELIMITED BY SIZE
033360                      INTO �K�p�P�v
033370               END-STRING
033380         ELSE
033390               STRING NC"�������Z"       DELIMITED BY SIZE
033400                      �������Z�����P�v   DELIMITED BY SIZE
033410                      �������Z�����Q�v   DELIMITED BY SIZE
033420                      �������Z�����R�v   DELIMITED BY SIZE
033430                      INTO �K�p�Q�v
033440               END-STRING
033450         END-IF
033460     END-IF.
033470*
033480*================================================================*
033490 ���{��ϊ� SECTION.
033500*
033510     MOVE NC"�O"     TO �S�p�����ԍ��v.
033520     CALL "htoz" WITH C LINKAGE
033530                        USING �����ԍ��v�P �S�p�����ԍ��v�P.
033540*
033550*================================================================*
033560 �ϔC�N�����擾 SECTION.
033570*================================================================*
033580** ---// �����̎󗝔N�ɂ́A�ŏI�ʉ@���������Ă���ׁA�ޔ����� //----
033590     MOVE �󗝔N�v   TO �ŏI�ʉ@�N�v.
033600     MOVE �󗝌��v   TO �ŏI�ʉ@���v.
033610     MOVE �󗝓��v   TO �ŏI�ʉ@���v.
033620***
033630* (�_���t��)
033640     EVALUATE ���Z�v�g���t�敪�v 
033650*    /  �ŏI�ʉ@�� /
033660     WHEN ZERO
033670         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
033680         MOVE �ŏI�ʉ@���v TO �_���t���v
033690         MOVE �ŏI�ʉ@���v TO �_���t���v
033700*    /  ������ /
033710     WHEN 1 
033720         PERFORM �������擾
033730         MOVE �󗝔N�v     TO �_���t�N�v
033740         MOVE �󗝌��v     TO �_���t���v
033750         MOVE �󗝓��v     TO �_���t���v
033760*    /  �󎚂Ȃ� /
033770     WHEN 9
033780         MOVE ZERO         TO �_���t�N�v
033790         MOVE ZERO         TO �_���t���v
033800         MOVE ZERO         TO �_���t���v
033810*    /  ���̑��́A�ŏI�ʉ@�� /
033820     WHEN OTHER
033830         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
033840         MOVE �ŏI�ʉ@���v TO �_���t���v
033850         MOVE �ŏI�ʉ@���v TO �_���t���v
033860     END-EVALUATE.
033870**
033880* (���ґ�)
033890     EVALUATE ���Z�v�g���ғ��t�敪�v 
033900*    /  �ŏI�ʉ@�� /
033910     WHEN ZERO
033920         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
033930         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
033940         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
033950*    /  ������ /
033960     WHEN 1 
033970         PERFORM �������擾
033980         MOVE �󗝔N�v     TO ���҈ϔC�N�v
033990         MOVE �󗝌��v     TO ���҈ϔC���v
034000         MOVE �󗝓��v     TO ���҈ϔC���v
034010*    /  �󎚂Ȃ� /
034020     WHEN 9
034030         MOVE ZERO         TO ���҈ϔC�N�v
034040         MOVE ZERO         TO ���҈ϔC���v
034050         MOVE ZERO         TO ���҈ϔC���v
034060*    /  ���̑��́A�ŏI�ʉ@�� /
034070     WHEN OTHER
034080         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
034090         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
034100         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
034110     END-EVALUATE.
034120*
034130*================================================================*
034140 �������擾 SECTION.
034150*
034160     MOVE �{�p�N�v�q   TO �󗝔N�v.
034170     MOVE �{�p���v�q   TO �󗝌��v.
034180     MOVE �{�p�a��v�q TO ���|�����敪.
034190     READ �����}�X�^
034200     NOT INVALID KEY
034210         MOVE ���|�J�n����N TO �{�p����N�v
034220     END-READ.
034230     IF ( �{�p����N�v NOT = ZERO )
034240        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
034250     END-IF.
034260*
034270     EVALUATE �{�p���v�q
034280     WHEN 4
034290     WHEN 6
034300     WHEN 9
034310     WHEN 11
034320         MOVE 30 TO �󗝓��v
034330     WHEN 2
034340         DIVIDE 4 INTO �{�p����N�v GIVING    ���v
034350                                    REMAINDER �]�v
034360         END-DIVIDE
034370         IF ( �]�v = ZERO )
034380             MOVE 29 TO �󗝓��v
034390         ELSE
034400             MOVE 28 TO �󗝓��v
034410         END-IF
034420     WHEN 1
034430     WHEN 3
034440     WHEN 5
034450     WHEN 7
034460     WHEN 8
034470     WHEN 10
034480     WHEN 12
034490         MOVE 31 TO �󗝓��v
034500     WHEN OTHER
034510          CONTINUE
034520     END-EVALUATE.
034530*
034540*================================================================*
034550 ���������擾 SECTION.
034560*================================================================*
034570********************************************************************
034580*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
034590*  ��: �@�A �Ƃœ]��.
034600*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
034610*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
034620********************************************************************
034630     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
034640     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
034650             UNTIL ( ���ʂb�m�s > ���ʐ��v )
034660*
034670****        IF ( ���|�������Ҕԍ�(���ʂb�m�s)  NOT = ZERO )  AND
034680        IF ( ���|�����A��(���ʂb�m�s)      NOT = ZERO )
034690*
034700           IF ( �J�E���^ = ZERO )
034710               MOVE 1   TO  �J�E���^ �J�E���^�Q
034720               MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
034730               MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
034740               MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
034750           ELSE
034760              IF ( ���|�������Ҕԍ�(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
034770                 ( ���|�����A��(���ʂb�m�s)      = �����A�Ԃb�v     )
034780                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
034790                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
034800              ELSE
034810                 COMPUTE �J�E���^ = �J�E���^  +  1
034820                 MOVE 1   TO  �J�E���^�Q
034830                 MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
034840                 MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
034850                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
034860              END-IF
034870           END-IF
034880        END-IF
034890     END-PERFORM.
034900**************************************************************************
034910*  ���������}�X�^��蕶�͎擾
034920**************************************************************************
034930     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
034940     PERFORM VARYING �J�E���^ FROM 1 BY 1
034950             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
034960** ���ۂ� �敪 01
034970         MOVE 01                        TO �����|�敪�R�[�h
034980         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
034990         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
035000         READ ���������e
035010         NOT INVALID KEY
035020             INITIALIZE ���������v�s
035030             MOVE �����|���������b�l(1) TO  ���������P�v�s
035040             MOVE �����|���������b�l(2) TO  ���������Q�v�s
035050             MOVE �����|���������b�l(3) TO  ���������R�v�s
035060             MOVE �����|���������b�l(4) TO  ���������S�v�s
035070             MOVE �����|���������b�l(5) TO  ���������T�v�s
035080             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
035090                     UNTIL ( �J�E���^�Q > 9 )  OR 
035100                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
035110                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
035120                WHEN 1
035130                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035140                WHEN 2
035150                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035160                WHEN 3
035170                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035180                WHEN 4
035190                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035200                WHEN 5
035210                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035180                WHEN 6
035190                   MOVE "�E"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035200                WHEN 7
035210                   MOVE "�F"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035220                WHEN OTHER
035230                   CONTINUE
035240                END-EVALUATE
035250             END-PERFORM
035260*
035342             IF �����|�����������͋敪 = 1
035343                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
035344                        ���������P�v�s  DELIMITED BY SIZE
035345                        ���������Q�v�s  DELIMITED BY SIZE
035346                        ���������R�v�s  DELIMITED BY SIZE
035347                        ���������S�v�s  DELIMITED BY SIZE
035348                        ���������T�v�s  DELIMITED BY SIZE
035349                        INTO �����������e�����v(�J�E���^)
035350                 END-STRING
035351             ELSE
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
035360             END-IF
035361*
035362         END-READ
035363     END-PERFORM.
035370*
035380     PERFORM ���������Z�b�g.
035390*
035400*================================================================*
035410 ���������Z�b�g SECTION.
035420*
035430**************************************************************************
035440*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
035450**************************************************************************
035460     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
035470     PERFORM VARYING �J�E���^ FROM 1 BY 1
035480             UNTIL ( �J�E���^ > 9 )  OR ( �����������e�����v(�J�E���^) = SPACE )
035490*
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
035640*
035650     END-PERFORM.
035660*
035670*================================================================*
035680 �������R���擾 SECTION.
035690*================================================================*
035700* �������R���擾�� "CHOUBUN" ���Ă�. 
035710     MOVE  SPACE TO  �A�����|�L�[.
035720     INITIALIZE      �A�����|�L�[.
035730     MOVE �{�p�a��v�q  TO  �A�����|�{�p�a��.
035740     MOVE �{�p�N�v�q    TO  �A�����|�{�p�N.
035750     MOVE �{�p���v�q    TO  �A�����|�{�p��.
035760     MOVE ���Ҕԍ��v�q  TO  �A�����|���Ҕԍ�.
035770     MOVE �}�Ԃv�q      TO  �A�����|�}��.
035780** �����_���t����p��56��
035790     MOVE 56            TO  �A�����|������.
035800*
035810     CALL   "CHOUBUN".
035820     CANCEL "CHOUBUN".
035830*
035840*================================================================*
035850 �{�p�h�c�擾 SECTION.
035860*================================================================*
035870*********************************************
035880** �h�c�Ǘ��}�X�^���@���{�p�h�c���擾����B
035890*********************************************
035900**   / ���{�pID /
035910     MOVE 01                     TO �h�c�ǁ|�h�c�敪.
035920     MOVE ZERO                   TO �h�c�ǁ|�{�p���ԍ�.
035930     MOVE ��p���S�Ҕԍ������v�q(3:2) TO �h�c�ǁ|�ی����.
035940     MOVE SPACE                  TO �h�c�ǁ|�ی��Ҕԍ�.
035950     READ �h�c�Ǘ��}�X�^
035960     NOT INVALID KEY
035970         MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO ���{�p�h�c�v
035980     END-READ.
035990*
036000**   / �s�����{�pID /
036010*****     MOVE 02                     TO �h�c�ǁ|�h�c�敪.
036020*****     MOVE ZERO                   TO �h�c�ǁ|�{�p���ԍ�.
036030*****     MOVE ������ʂv�q           TO �h�c�ǁ|�ی����.
036040*****     MOVE ��p���S�Ҕԍ������v�q TO �h�c�ǁ|�ی��Ҕԍ�.
036050*****     READ �h�c�Ǘ��}�X�^
036060*****     NOT INVALID KEY
036070*****          MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO �s�����{�p�h�c�v
036080*****     END-READ.
036090*
036100*================================================================*
036110 ���Z�v�g�񐔎擾 SECTION.
036120*================================================================*
036130*************************************************************************
036140**-------- ���Z�v�g�̑� XX ��� �̉񐔂����߂�B----------**
036150*  ���ʂ̊J�n�N���ŁA��ԏ�����(�Â�)�N���Ǝ{�p�N���Ƃ̍���1�𑫂�
036160*  (��) �J�n�N��10�N7��  �Ŏ{�p�N��10�N10���́A4���
036170*  (��) �J�n�N��10�N10�� �Ŏ{�p�N��10�N10���́A1���
036180*************************************************************************
036190*
036200     MOVE ZERO     TO �񐔂v.
036210*
036220     PERFORM �J�n�N���ŏ��擾.
036230     PERFORM ���̌��擾.
036240     MOVE �v�Z���v TO �񐔂v.
036250*
036260*================================================================*
036270 �J�n�N���ŏ��擾  SECTION.
036280*
036290** --// ���ʂ̊J�n�N���ŁA��ԏ�����(�Â�)�N�������߂�. //--**
036300*
036310     INITIALIZE �ŏ��J�n�a��N���v.
036320* 1���ʖڂ�2���ʖڂ��r
036330     IF ( ���|�J�n�a��N��(2) NOT = ZERO )
036340        IF ( ���|�J�n�a��N��(1)  <  ���|�J�n�a��N��(2) )
036350           MOVE ���|�J�n�a��N��(1) TO �ŏ��J�n�a��N���v
036360        ELSE
036370           MOVE ���|�J�n�a��N��(2) TO �ŏ��J�n�a��N���v
036380        END-IF
036390     ELSE
036400        MOVE ���|�J�n�a��N��(1) TO �ŏ��J�n�a��N���v
036410     END-IF.
036420* 3���ʖڈȍ~���r
036430     PERFORM VARYING ���ʂb�m�s FROM 3 BY 1
036440             UNTIL ( ���ʂb�m�s > ���ʐ��v )
036450         IF ( ���|�J�n�a��N��(���ʂb�m�s) <  �ŏ��J�n�a��N���v )
036460            MOVE ���|�J�n�a��N��(���ʂb�m�s) TO �ŏ��J�n�a��N���v
036470         END-IF
036480     END-PERFORM.
036490*
036500*================================================================*
036510 ���̌��擾  SECTION.
036520*********************************************************** 
036530*   �J�n�N���Ǝ{�p�N���Ƃ̍��̌������߂�B
036540*    (�O������̃��W�b�N�A�Z�N�V�����𗘗p)
036550*********************************************************** 
036560*
036570      INITIALIZE  �v�Z�N�����v �J�n�N�����Q�v �I���N�����Q�v.
036580*
036590      IF ( �ŏ��J�n�a��N���v NOT = ZERO )
036600*
036610          MOVE �{�p�a��v�q    TO �I���a��Q�v
036620          MOVE �{�p�N�v�q      TO �I���N�Q�v
036630          MOVE �{�p���v�q      TO �I�����Q�v
036640          MOVE �ŏ��J�n�a��v  TO �J�n�a��Q�v
036650          MOVE �ŏ��J�n�N�v    TO �J�n�N�Q�v
036660          MOVE �ŏ��J�n���v    TO �J�n���Q�v
036670*
036680          EVALUATE TRUE
036690           WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v = �I���N�Q�v)
036700                PERFORM  �O����r��
036710           WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v NOT = �I���N�Q�v)
036720                PERFORM  �O����r�N
036730           WHEN  �J�n�a��Q�v NOT = �I���a��Q�v 
036740                PERFORM  �O����r����
036750          END-EVALUATE
036760*
036770          COMPUTE �v�Z���v =  �v�Z���v + 1
036780*
036790      END-IF.
036800*
036810*================================================================*
036820 �ی��Җ��̎擾 SECTION.
036830*================================================================*
036840     MOVE �ی���ʂv�q   TO �ہ|�ی����.
036850     MOVE �ی��Ҕԍ��v�q TO �ہ|�ی��Ҕԍ�.
036860     READ �ی��҃}�X�^
036870     INVALID KEY
               IF �ی���ʂv�q = 05
030800             MOVE �ی���ʂv�q   TO �s�|������
030810             MOVE �ی��Ҕԍ��v�q TO �s�|�s�����ԍ�
030820             READ �s�����}�X�^
030830             INVALID KEY
030840                 MOVE SPACE      TO �ی��Җ��̂v
030850             NOT INVALID KEY
031330                 MOVE �s�|�s��������    TO �ی��Җ��̂v
                   END-READ
               ELSE
030840             MOVE SPACE      TO �ی��Җ��̂v
               END-IF
036890     NOT INVALID KEY
036900** �g���E���ς͎x�����܂ň�
036910                 EVALUATE �ی���ʂv�q
036920                 WHEN 2
036930                 WHEN 6
036940                     IF ( �ہ|�ڔ���敪 = 1 )
036950                        MOVE �ہ|�ی��Җ���    TO �ی��Җ��̂v
036960                     ELSE
036970                        STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
036980                               "�Љ�ی�������"  DELIMITED BY SIZE
036990                               INTO �ی��Җ��̂v
037000                        END-STRING
037010                     END-IF
037020                 WHEN 3
037030                     STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
037040                            "���N�ی��g��"    DELIMITED BY SIZE
037050                            �ہ|�x��������    DELIMITED BY SPACE
037060                            INTO �ی��Җ��̂v
037070                     END-STRING
037080                 WHEN 4
037090                     STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
037100                            "���ϑg��"        DELIMITED BY SIZE
037110                            �ہ|�x��������    DELIMITED BY SPACE
037120                            INTO �ی��Җ��̂v
037130                     END-STRING
037140                 WHEN OTHER
037150                     MOVE �ہ|�ی��Җ���      TO �ی��Җ��̂v
037160                 END-EVALUATE
037170     END-READ.
037180*
037190*================================================================*
037200 ���t�����擾 SECTION.
037210*================================================================*
037220     MOVE ZERO  TO ���S�����v   ���t�����v.
037230*
037240     IF ( �����ʂv�q = 05 )
037250        IF ( �{�p�a��N���v�q >= 41410 )
037260           PERFORM ���S���擾�P�S�P�O
037270           COMPUTE ���S�����v = ( ���S���v / 10 )
037280           COMPUTE ���t�����v = ( 10 - ���S�����v )
037290        ELSE
037300           CONTINUE
037310        END-IF
037320     ELSE
037330        MOVE ���Z�|���S���� TO ���S�����v
037340        MOVE ���Z�|���t���� TO ���t�����v
037350     END-IF.
037360*
037370*     PERFORM ���t�����`�F�b�N.
037380*
037390*================================================================*
037400 ���S���擾�P�S�P�O SECTION.
037410*
037420* ����14/10�`
037430     MOVE ZERO  TO ���S���v.
037440     MOVE SPACE TO �A���|���S���擾�L�[.
037450     INITIALIZE �A���|���S���擾�L�[.
037460     MOVE �{�p�a��N���v�q TO �A���|�{�p�a��N��.
037470     MOVE ���҃R�[�h�v�q   TO �A���|���҃R�[�h.
037480*
037490     CALL   "HUTANRIT".
037500     CANCEL "HUTANRIT".
037510*
037520***     MOVE �A���|���ە��S�� TO ���S���v.
037530*
037540*** / �V�l���Z�̎��͈ȉ�
037550     MOVE �A���|�Q�V�V���S�� TO ���S���v.
037560*
037570**================================================================*
037580* ���t�����`�F�b�N SECTION.
037590**
037600**** �Q�V�g��A�픚�i�R�y�A�j�̎��́A���t�V�l�`�F�b�N�Ɂ�
037610*     IF ( �����ʂv�q NOT = ZERO )  AND
037620*        ( ������ʂv�q NOT = ZERO )
037630*        MOVE NC"�V"   TO  ���t�V�l�v 
037640*        MOVE NC"��"   TO  ���t�V�l�`�F�b�N�v 
037650*     ELSE
037660**
037670*        EVALUATE  ���t�����v
037680*        WHEN  7
037690*           MOVE NC"��"   TO  ���t�V���`�F�b�N�v 
037700*        WHEN  8
037710*           MOVE NC"��"   TO  ���t�W���`�F�b�N�v 
037720*        WHEN  9
037730*           MOVE NC"��"   TO  ���t�X���`�F�b�N�v 
037740*        WHEN  OTHER
037750*           CONTINUE
037760*        END-EVALUATE
037770*     END-IF.
037780*
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
037790*================================================================*
037800 ���Z�E�v�ăZ�b�g SECTION.
037810*================================================================*
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
037990*
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
038000*================================================================*
038010 �{�p�L�^�e�Ǎ� SECTION.
038020*================================================================*
038030*
038040     READ �{�p�L�^�e NEXT
038050     AT END
038060         MOVE "YES" TO �I���t���O�Q
038070     END-READ.
038080*
038090*----------------------------------------------------------------*
038100*================================================================*
038110 ������� SECTION.
038120*================================================================*
038130     MOVE "YCB6425P" TO  ��`�̖��o.
038140     MOVE "SCREEN"  TO  ���ڌQ���o.
038150     WRITE YCB6425P.
038160***     WRITE ������R�[�h.
038170     PERFORM �G���[�����o.
038180*================================================================*
038190 �G���[�����o SECTION.
038200*
038210     IF �ʒm���o NOT = "00"
038220         DISPLAY NC"���[�G���["              UPON CONS
038230         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
038240         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
038250         DISPLAY NC"�g������o�F" �g������o UPON CONS
038260         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
038270                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
038280         ACCEPT  �L�[���� FROM CONS
038290         PERFORM �t�@�C����
038300         MOVE 99  TO PROGRAM-STATUS
038310         EXIT PROGRAM
038320     END-IF.
038330*
038340*=== �I������ ===================================================*
038350*================================================================*
038360 ��f�҈���敪�X�V SECTION.
038370*================================================================*
038380** //  ��f�ҏ��e�̈���敪�ɂP���Z�b�g���A�X�V����B//  
038390*
038400     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
038410     MOVE �{�p�N�v�q         TO ��|�{�p�N.
038420     MOVE �{�p���v�q         TO ��|�{�p��.
038430     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
038440     READ ��f�ҏ��e
038450     NOT INVALID KEY
038460         MOVE  1  TO  ��|���Z����敪����
038470         REWRITE  ��|���R�[�h
038480         END-REWRITE
038490         IF ( ��ԃL�[ NOT = "00" )
038500            MOVE NC"��f��" TO �t�@�C����
038510            PERFORM �G���[�\��
038520         END-IF
038530     END-READ.
038540*
038550*================================================================*
038560 �I������ SECTION.
038570*================================================================*
038580     PERFORM �t�@�C����.
038590*
038600*================================================================*
038610 �t�@�C���� SECTION.
038620*
038630     CLOSE �����}�X�^     ���̃}�X�^       ���Z�v�g�e     �o�߃}�X�^
038640           ������}�X�^ �{�p�����}�X�^ ��f�ҏ��Q�e
038650           �ی��҃}�X�^   ������}�X�^     �h�c�Ǘ��}�X�^ �s�����}�X�^
038660           ��f�ҏ��e   �{�p�L�^�e       �����f�[�^�e   ���������e
038670           ��ƃt�@�C���Q.
038680     CLOSE ����t�@�C��.
038690*
038700*================================================================*
038710*================================================================*
038720 �G���[�\�� SECTION.
038730*
038740     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
038750     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
038760     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
038770     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
038780                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
038790     ACCEPT  �L�[���� FROM CONS
038800     PERFORM �t�@�C����.
038810     EXIT PROGRAM.
038820*
038830*================================================================*
038840*================================================================*
038850 �e�X�g�󎚏��� SECTION.
      *
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
           ���v�S�O �����������S�O ���������v�S�O ���v ���S����  
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
      *
           MOVE ALL "��" TO
           ��ی��Ҏ��� ���Ҏ��� �ڍ��@�� ��\�Җ�
           �������R���P �������R���Q �������R���R �������R���S �������R���T �������R���U 
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
040030*================================================================*
030430 ������擾 SECTION.
030440*
030442* ������� "JOSEIMEI" ���Ă�. 
030443     MOVE SPACE TO  �A�������́|�L�[.
030444     INITIALIZE     �A�������́|�L�[.
030445     MOVE ������ʂv�q           TO �A�������́|�������.
030446     MOVE ��p���S�Ҕԍ������v�q TO �A�������́|��p���S�Ҕԍ�����.
030447*
030448     CALL   "JOSEIMEI".
030449     CANCEL "JOSEIMEI".
030450*
030451     MOVE �A�������́|�P���� TO ������v.
030452*
030420*================================================================*
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
043540*================================================================*
043420 ���Z�v�g���я��擾 SECTION.
043430*
043440     MOVE �{�p�a��v�q       TO ��Q�|�{�p�a��.
043450     MOVE �{�p�N�v�q         TO ��Q�|�{�p�N.
043460     MOVE �{�p���v�q         TO ��Q�|�{�p��.
043470     MOVE ���҃R�[�h�v�q     TO ��Q�|���҃R�[�h.
039550** �����́A������ʂ��Z�b�g
039560     MOVE ������ʂv�q       TO ��Q�|�ی����.
043490     READ ��ƃt�@�C���Q
043500     NOT INVALID KEY
043510          MOVE ��Q�|����    TO ���Ԃv
043520     END-READ.
043530*
043540*================================================================*
040040******************************************************************
040050 END PROGRAM YCB6425.
040060******************************************************************
