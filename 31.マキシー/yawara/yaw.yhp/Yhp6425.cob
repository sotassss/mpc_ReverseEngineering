000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHP6425.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*    �z�[�v  ���� ���Z�v�g����i�_+����޳�ޔŁj
000100*         MED = YAW610 YHP6425P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2012-08-07
000130 DATE-COMPILED.          2012-08-07
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
000360     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000370                             ORGANIZATION             IS  INDEXED
000380                             ACCESS MODE              IS  DYNAMIC
000390                             RECORD KEY               IS  �s�|������
000400                                                          �s�|�s�����ԍ�
000410                             ALTERNATE RECORD KEY     IS  �s�|������
000420                                                          �s�|�s��������
000430                                                          �s�|�s�����ԍ�
000440                             FILE STATUS              IS  ��ԃL�[
000450                             LOCK        MODE         IS  AUTOMATIC.
000460     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000470                             ORGANIZATION             IS  INDEXED
000480                             ACCESS MODE              IS  DYNAMIC
000490                             RECORD KEY               IS  ���|�����敪
000500                             FILE STATUS              IS  ��ԃL�[
000510                             LOCK        MODE         IS  AUTOMATIC.
000520     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000530                             ORGANIZATION             IS  INDEXED
000540                             ACCESS MODE              IS  DYNAMIC
000550                             RECORD KEY               IS  ���|�敪�R�[�h
000560                                                          ���|���̃R�[�h
000570                             FILE STATUS              IS  ��ԃL�[
000580                             LOCK        MODE         IS  AUTOMATIC.
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
000650     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS  ���|����敪
000690                             FILE STATUS              IS  ��ԃL�[
000700                             LOCK        MODE         IS  AUTOMATIC.
000710     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000720                             ORGANIZATION             IS  INDEXED
000730                             ACCESS MODE              IS  DYNAMIC
000740                             RECORD KEY               IS �{��|�{�p���ԍ�
000750                             FILE STATUS              IS  ��ԃL�[
000760                             LOCK        MODE         IS  AUTOMATIC.
000770     SELECT  ������}�X�^    ASSIGN      TO        SEIKYUSL
000780                             ORGANIZATION           IS  INDEXED
000790                             ACCESS MODE            IS  DYNAMIC
000800                             RECORD KEY             IS ����|�ی����
000810                                                       ����|�ی��Ҕԍ�
000820                             FILE STATUS            IS  ��ԃL�[
000830                             LOCK    MODE           IS  AUTOMATIC.
000840     SELECT  �o�߃}�X�^      ASSIGN      TO        KEIKAL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS  �o�|�敪�R�[�h
000880                                                          �o�|�o�߃R�[�h
000890                             FILE STATUS              IS  ��ԃL�[
000900                             LOCK        MODE         IS  AUTOMATIC.
000910     SELECT  ���������e      ASSIGN      TO        HUGEINL
000920                             ORGANIZATION             IS  INDEXED
000930                             ACCESS MODE              IS  DYNAMIC
000940                             RECORD KEY               IS  �����|�敪�R�[�h
000950                                                          �����|���������R�[�h
000960                             FILE STATUS              IS  ��ԃL�[
000970                             LOCK        MODE         IS  AUTOMATIC.
000980     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000990                             ORGANIZATION             IS  INDEXED
001000                             ACCESS MODE              IS  DYNAMIC
001010                             RECORD KEY               IS ��|�{�p�a��N��
001020                                                          ��|���҃R�[�h
001030                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
001040                                                          ��|���҃J�i
001050                                                          ��|���҃R�[�h
001060                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
001070                                                         ��|�{�p�a��N��
001080                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
001090                                                          ��|�ی����
001100                                                          ��|�ی��Ҕԍ�
001110                                                          ��|���҃R�[�h
001120                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
001130                                                          ��|������
001140                                                     ��|��p���S�Ҕԍ�
001150                                                          ��|���҃R�[�h
001160                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
001170                                                          ��|�������
001180                                                  ��|��p���S�Ҕԍ�����
001190                                                          ��|���҃R�[�h
001200                             ALTERNATE RECORD KEY  IS ��|�����a��N��
001210                                                      ��|�{�p�a��N��
001220                                                      ��|���҃R�[�h
001230                             FILE STATUS              IS  ��ԃL�[
001240                             LOCK        MODE         IS  AUTOMATIC.
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
001250     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
001260                             ORGANIZATION             IS  INDEXED
001270                             ACCESS MODE              IS  DYNAMIC
001280                             RECORD KEY           IS �{�L�|�{�p�a��N����
001290                                                     �{�L�|���҃R�[�h
001300                             ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
001310                                                     �{�L�|�{�p�a��N����
001320                             FILE STATUS              IS  ��ԃL�[
001330                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS ���|�{�p�a��N��
001380                                                         ���|���҃R�[�h
001390                             ALTERNATE RECORD KEY     IS ���|���҃R�[�h
001400                                                         ���|�{�p�a��N��
001410                             FILE STATUS              IS  ��ԃL�[
001420                             LOCK        MODE         IS  AUTOMATIC.
001430     SELECT  �����}�X�^      ASSIGN      TO        RYOUKINL
001440                             ORGANIZATION             IS  INDEXED
001450                             ACCESS MODE              IS  DYNAMIC
001460                             RECORD KEY               IS  ���|�敪�R�[�h
001470                                                          ���|���ʃR�[�h
001480                                                          ���|�J�n�a��N��.
001490     SELECT  ����}�X�^    ASSIGN      TO        KAIJOHOL
001500                             ORGANIZATION             IS  INDEXED
001510                             ACCESS MODE              IS  DYNAMIC
000130                             RECORD KEY               IS  ���|�_���I���敪
000131                                                          ���|����R�[�h
000132                                                          ���|�ی����
000133                                                          ���|�ύX�a��N��
000134                             ALTERNATE RECORD KEY     IS  ���|�_���I���敪
000135                                                          ���|�ڍ��t��J�i
000136                                                          ���|����R�[�h
000137                                                          ���|�ی����
000138                                                          ���|�ύX�a��N��
001590                             FILE STATUS              IS  ��ԃL�[
001600                             LOCK        MODE         IS  AUTOMATIC.
001610     SELECT  �h�c�Ǘ��}�X�^    ASSIGN      TO        IDKANRL
001620                             ORGANIZATION             IS  INDEXED
001630                             ACCESS MODE              IS  DYNAMIC
001640                             RECORD KEY               IS  �h�c�ǁ|�h�c�敪
001650                                                          �h�c�ǁ|�{�p���ԍ�
001660                                                          �h�c�ǁ|�ی����
001670                                                          �h�c�ǁ|�ی��Ҕԍ�
001680                             ALTERNATE RECORD KEY     IS  �h�c�ǁ|�{�p�h�c�ԍ�
001690                                                          �h�c�ǁ|�h�c�敪
001700                                                          �h�c�ǁ|�{�p���ԍ�
001710                                                          �h�c�ǁ|�ی����
001720                                                          �h�c�ǁ|�ی��Ҕԍ�
001730                             FILE STATUS              IS  ��ԃL�[
001740                             LOCK        MODE         IS  AUTOMATIC.
001755* ���я��󎚗p
001760     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001770                             ORGANIZATION             IS  INDEXED
001780                             ACCESS                   IS  DYNAMIC
001790                             RECORD      KEY          IS  ��Q�|�{�p�a��N��
001800                                                          ��Q�|���҃R�[�h
001810                                                          ��Q�|�ی����
001820                             FILE        STATUS       IS  ��ԃL�[
001830                             LOCK        MODE         IS  AUTOMATIC.
001840     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
001850                             SYMBOLIC    DESTINATION  IS "PRT"
001860                             FORMAT                   IS  ��`�̖��o
001870                             GROUP                    IS  ���ڌQ���o
001880                             PROCESSING  MODE         IS  ������ʂo
001890                             UNIT        CONTROL      IS  �g������o
001900                             FILE        STATUS       IS  �ʒm���o.
001910******************************************************************
001920*                      DATA DIVISION                             *
001930******************************************************************
001940 DATA                    DIVISION.
001950 FILE                    SECTION.
001990*                           �m�q�k��  �Q�T�U�n
002000 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002010     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
002020*                           �m�q�k��  �P�Q�W�n
002030 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002040     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002050*                           �m�q�k��  �P�Q�W�n
002060 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
002070     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
002110*                           �m�q�k��  �Q�T�U�n
002120 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
002130     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002140*                           �m�q�k��  �P�Q�W�n
002150 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002160     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
002170*                           �m�q�k��  �P�Q�W�n
002180 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
002190     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
002200*                           �m�q�k��  �P�Q�W�n
002210 FD  �o�߃}�X�^          BLOCK   CONTAINS   1   RECORDS.
002220     COPY KEIKA          OF  XFDLIB  JOINING   �o   AS  PREFIX.
002230*                           �m�q�k��  �R�Q�O�n
002240 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
002250     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002560*                          �m�q�k��  1024�n
000340 FD  ��f�ҏ��Q�e        BLOCK   CONTAINS   1   RECORDS.
000350     COPY JUSINJ2          OF  XFDLIB  JOINING   ��Q   AS  PREFIX.
002260*                           �m�q�k��  �Q�T�U�n
002270 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
002280     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
002290*                           �m�q�k��  �P�Q�W�n
002300 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
002310     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002320*                           �m�q�k��  �P�Q�W�n
002330 FD  ���������e         BLOCK   CONTAINS   1   RECORDS.
002340     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
002350*
002360 FD  �����}�X�^         BLOCK   CONTAINS   1   RECORDS.
002370     COPY RYOUKIN         OF  XFDLIB  JOINING   ��   AS  PREFIX.
002380     COPY RYOUKNA         OF  XFDLIB  JOINING   ���` AS  PREFIX.
002390     COPY RYOUKNB         OF  XFDLIB  JOINING   ���a AS  PREFIX.
002400     COPY RYOUKNC         OF  XFDLIB  JOINING   ���b AS  PREFIX.
002410     COPY RYOUKND         OF  XFDLIB  JOINING   ���c AS  PREFIX.
002420     COPY RYOUKNE         OF  XFDLIB  JOINING   ���d AS  PREFIX.
002430     COPY RYOUKNF         OF  XFDLIB  JOINING   ���e AS  PREFIX.
002440*                           �m�q�k��  �U�S�O�n
002450 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
002460     COPY KAIJOHO         OF  XFDLIB  JOINING   ���   AS  PREFIX.
002470*                           �m�q�k��  �P�Q�W�n
002480 FD  �h�c�Ǘ��}�X�^          BLOCK   CONTAINS   1   RECORDS.
002490     COPY IDKANR    OF  XFDLIB  JOINING   �h�c��   AS  PREFIX.
002500**
002510 FD  ��ƃt�@�C���Q RECORD  CONTAINS 32 CHARACTERS.
002520 01  ��Q�|���R�[�h.
002530     03  ��Q�|���R�[�h�L�[.
002540         05  ��Q�|�{�p�a��N��.
002550             07  ��Q�|�{�p�a��            PIC 9.
002560             07  ��Q�|�{�p�N              PIC 9(2).
002570             07  ��Q�|�{�p��              PIC 9(2).
002580         05  ��Q�|���҃R�[�h.
002590             07 ��Q�|���Ҕԍ�             PIC 9(6).
002600             07 ��Q�|�}��                 PIC X(1).
002610         05  ��Q�|�ی����                PIC 9(2).
002620     03  ��Q�|���R�[�h�f�[�^.
002630         05  ��Q�|����                    PIC 9(4).
002640         05  FILLER                        PIC X(14).
002650*
002660 FD  ����t�@�C��.
002670     COPY YHP6425P        OF  XMDLIB.
002680*----------------------------------------------------------------*
002690******************************************************************
002700*                WORKING-STORAGE SECTION                         *
002710******************************************************************
002720 WORKING-STORAGE         SECTION.
002730 01 �L�[����                           PIC X     VALUE SPACE.
002740 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
002750 01 �I���t���O                         PIC X(3)  VALUE SPACE.
002760 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
002770 01 �����t���O                         PIC X(3)  VALUE SPACE.
002780 01 �p���t���O                         PIC X(3)  VALUE SPACE.
002790 01 �t�@�C����                         PIC N(6)  VALUE SPACE.
002800 01 ���Z�v�g�o�f�v                     PIC X(8)  VALUE SPACE.
002810 01 �O�a��v                           PIC 9     VALUE ZERO.
002820 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
002830 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002840 01 ���Ҕԍ��v                         PIC 9(6)  VALUE ZERO.
002850 01 �������̂v                         PIC N(6)  VALUE SPACE.
002860 01 ���ʖ��̂v                         PIC N(12) VALUE SPACE.
002870 01 ���ʒ��v                           PIC 9(2) VALUE 1.
002880 01 �E�o�t���O                         PIC X(3)  VALUE SPACE.
002890 01 �󔒂v                             PIC X(2)  VALUE SPACE.
001363 01 �S�p��                           PIC X(2)  VALUE X"8140".
001364 01 ���p��                           PIC X(2)  VALUE X"2020".
002900*
002910** ���������{��ϊ�
002920 01 �����v                             PIC 9(2).
002930 01 �����q REDEFINES �����v.
002940    03 �����v�P                        PIC X(1).
002950    03 �����v�Q                        PIC X(1).
002960*
002970 01 �����ԍ��v                         PIC 9.
002980 01 �����ԍ��q REDEFINES �����ԍ��v.
002990    03 �����ԍ��v�P                    PIC X.
003000*
003010 01 �S�p�����ԍ��v                     PIC N.
003020 01 �S�p�����ԍ��q REDEFINES �S�p�����ԍ��v.
003030    03 �S�p�����ԍ��v�P                PIC X(2).
003040*
003050 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
003060 01 �J�E���^�Q                         PIC 9(2)  VALUE ZERO.
003070 01 �ی����̂v                         PIC N(12) VALUE SPACE.
003080*
003090* �ޔ�p
003100 01 �I���N�����v�s.
003110    03 �I���N�v�s                      PIC 9(2)  VALUE ZERO.
003120    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
003130    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
003140* �������ޔ�p
003150 01 �����N�����v�s.
003160    03 �����a��v�s                    PIC 9     VALUE ZERO.
003170    03 �����N�v�s                      PIC 9(2)  VALUE ZERO.
003180    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003190    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003200* ���������p
003210 01 ���������v�s.
003220    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
003230    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
003240    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
003250    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
003260    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
003270    03 ���������i���o�[�v�s.
003280       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
003290    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
003300 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
003310 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
003320 01 ���������s�a�k.
003330    03 ���������R�[�h�s�a�k            OCCURS 9.
003340       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
003350       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
003360       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
003370 01 �����������e�v.
003380    03 �����������e�����v              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 �����������e�����w�v.
003630       05 �����������e�P�w�v           PIC X(80)  VALUE SPACE.
003640       05 �����������e�Q�w�v           PIC X(80)  VALUE SPACE.
003640       05 �����������e�R�w�v           PIC X(80)  VALUE SPACE.
003650       05 �����������e�S�w�v           PIC X(78)  VALUE SPACE.
003430*
003440* �������Z�����p
003450 01 �������Z�v�s.
003460    03 �������Z�J�E���g                PIC 9    VALUE ZERO.
003470    03 �ԍ��J�E���^                    PIC 9    VALUE ZERO.
003480    03 �������Z�W�c�v�s  OCCURS 3.
003490       05 �������Z�敪�v�s             PIC 9    VALUE ZERO.
003500       05 �������Z���v�s               PIC 9(2) VALUE ZERO.
003510       05 �������Z���v�s               PIC 9(2) VALUE ZERO.
003520    03 �������Z�W�c�m�v  OCCURS 3.
003530       05 ���Z��؂v                   PIC N(1) VALUE SPACE.
003540       05 ���Z���e�v                   PIC N(3) VALUE SPACE.
003550       05 �������Z���m�v�P             PIC N(1) VALUE SPACE.
003560       05 �������Z���m�v�Q             PIC N(1) VALUE SPACE.
003570       05 ���Œ�v                     PIC N(1) VALUE SPACE.
003580       05 �������Z���m�v�P             PIC N(1) VALUE SPACE.
003590       05 �������Z���m�v�Q             PIC N(1) VALUE SPACE.
003600       05 ���Œ�v                     PIC N(1) VALUE SPACE.
003610    03 �������Z�����P�v                PIC N(10) VALUE SPACE.
003620    03 �������Z�����Q�v                PIC N(10) VALUE SPACE.
003630    03 �������Z�����R�v                PIC N(10) VALUE SPACE.
003070    03 �������Z��؂v                  PIC X     VALUE SPACE.
003080    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003090    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003640*
003650** �O�������̂ݗp
003660 01 �����Č��t���O                     PIC X(3)  VALUE SPACE.
003670 01 �O���t���O                         PIC X(3)  VALUE SPACE.
003680*
003690 01 �v�Z�N�����v.
003700    03 �v�Z�a��v                      PIC 9(1)  VALUE ZERO.
003710    03 �v�Z�N�v                        PIC S9(2)  VALUE ZERO.
003720    03 �v�Z���v                        PIC S9(2)  VALUE ZERO.
003730    03 �v�Z���v                        PIC S9(2)  VALUE ZERO.
003740 01 �J�n�N�����Q�v.
003750    03 �J�n�a��Q�v                    PIC 9(1)  VALUE ZERO.
003760    03 �J�n�N�Q�v                      PIC 9(2)  VALUE ZERO.
003770    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
003780    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
003790    03 �J�n����N�v                    PIC S9(4) VALUE ZERO.
003800 01 �I���N�����Q�v.
003810    03 �I���a��Q�v                    PIC 9(1)  VALUE ZERO.
003820    03 �I���N�Q�v                      PIC 9(2)  VALUE ZERO.
003830    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
003840    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
003850    03 �I������N�v                    PIC S9(4) VALUE ZERO.
003860***
003870** ���������E�������R����敪�p
003880 01 ������������敪�v                 PIC 9 VALUE ZERO.
003890 01 �������R����敪�v                 PIC 9 VALUE ZERO.
003900*
003910** ���Z���i�̓��t�敪�p (0:�ŏI�ʉ@���A1:�������A9:�󎚂Ȃ�)
003920 01 ���Z�v�g���t�敪�v                 PIC 9 VALUE ZERO.
003930 01 ���Z�v�g���ғ��t�敪�v             PIC 9 VALUE ZERO.
003940*
003950** �������p
003960 01 �{�p����N�v                       PIC 9(4)  VALUE ZERO.
003970 01 ���v                               PIC 9(3)  VALUE ZERO.
003980 01 �]�v                               PIC 9(3)  VALUE ZERO.
003990*
004000** ���t�����p
004010 01 ���S���������v                     PIC 9     VALUE ZERO.
004020 01 ���t�����v�o.
          03 �������v                        PIC X     VALUE SPACE.
          03 ���t�����v                      PIC X     VALUE SPACE.
004030 01 ���S�����v                         PIC 9(2)  VALUE ZERO.
004040 01 �����Œ�v                         PIC N     VALUE SPACE.
       01 ��󎚂v                           PIC N     VALUE SPACE.
004050*
004060** �}�Ԕ���p
004070 01 �J�n�f�Ó��蓮�敪�v               PIC 9    VALUE ZERO.
004080*
004090** �����於�̗p
004100 01 �����於�̂s�a�k.
004110    03 �����於�̂v�s                  PIC X(2)  OCCURS 20 VALUE SPACE.
004120 01 �����於�̂v�s�P                   PIC X(2)  VALUE SPACE.
004130 01 �x���������v                       PIC X(40) VALUE SPACE.
004140*
004150* ���[�Œ�󎚗p
004160 01 ���N�����Œ�v                     PIC N(4)   VALUE SPACE.
004170 01 �����όŒ�v                       PIC N(15)  VALUE SPACE.
004180*
004190* ��ϔC���p
004200* 01 ��ϔC���P�v                     PIC N(18)  VALUE SPACE.
004210* 01 ��ϔC���Q�v.
004220*    03 ��ϔC���Q�P�v                PIC N(4)   VALUE SPACE.
004230*    03 ����v                        PIC N(5)   VALUE SPACE.
004240*    03 ��ϔC���Q�Q�v                PIC N(9)   VALUE SPACE.
       01 ��ϔC���v.
          03 ��ϔC���P�v                  PIC X(45)  VALUE SPACE.
          03 ��ϔC���Q�v                  PIC X(45)  VALUE SPACE.
          03 ��ϔC���R�v                  PIC X(45)  VALUE SPACE.
004250*
004251*
004252** ���Z�E�v�p( N(38)�Œ�j /
004253 01 �����̌o�߂v.
004254*    03 �����̌o�ߍs�v                  PIC X(76) OCCURS 2 VALUE SPACE.
004254    03 �����̌o�ߍs�v                  PIC X(64) OCCURS 2 VALUE SPACE.
004255 01 �����̌o�߂m�v REDEFINES �����̌o�߂v.
004256*    03 �����̌o�ߍs�m�v                PIC N(38) OCCURS 2.
004256    03 �����̌o�ߍs�m�v                PIC N(32) OCCURS 2.
004257*
004258* ������������敪
004259 01 ���Z������������敪�v             PIC 9    VALUE ZERO.
002580 01 ���Z�������R����敪�v             PIC 9    VALUE ZERO.
      *
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
       01 �������q�b�l                       PIC X(200) VALUE SPACE.
       01 �^����Âb�l                       PIC X(68)  VALUE SPACE.
004260*
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
004261****************
004270* �A�����ڑҔ� *
004280****************
004290*    ************
004300*    * ����L�[ *
004310*    ************
004320 01 �Ώۃf�[�^�v�q.
004330    03 �{�p�a��N���v�q.
004340       05 �{�p�a��v�q                  PIC 9(1)  VALUE ZERO.
004350       05 �{�p�N�v�q                    PIC 9(2)  VALUE ZERO.
004360       05 �{�p���v�q                    PIC 9(2)  VALUE ZERO.
004370    03 �ی���ʂv�q                     PIC 9(2)  VALUE ZERO.
004380    03 �ی��Ҕԍ��v�q                   PIC X(10) VALUE SPACE.
004390    03 �����ʂv�q                     PIC 9(2)  VALUE ZERO.
004400    03 ��p���S�Ҕԍ��v�q               PIC X(10) VALUE SPACE.
004410    03 ������ʂv�q                     PIC 9(2)  VALUE ZERO.
004420    03 ��p���S�Ҕԍ������v�q           PIC X(10) VALUE SPACE.
004430    03 �{�l�Ƒ��敪�v�q                 PIC 9(1)  VALUE ZERO.
004440    03 ���҃J�i�v�q                     PIC X(50) VALUE SPACE.
004450    03 ���҃R�[�h�v�q.
004460       05 ���Ҕԍ��v�q                  PIC 9(6)  VALUE ZERO.
004470       05 �}�Ԃv�q                      PIC X(1)  VALUE SPACE.
004480*
004490*    ****************
004500*    * ��{������� *
004510*    ****************
004520 01 ��{�����v.
004530   03 ��㪖@�P���v                      PIC 9(4)  VALUE ZERO.
004540   03 ��㪖@�P���v                      PIC 9(4)  VALUE ZERO.
004550   03 �d�ÒP���v                        PIC 9(4)  VALUE ZERO.
004560*    ************
004570*    * ������� *
004580*    ************
004590*    �����̗���
004600***********************
004610 01 �����P�v�q.
004620   03 �����v�q.
004630      05 ���S�����v�q               PIC 9(3)    VALUE ZERO.
004640      05 �������v�q                 PIC 9(5)    VALUE ZERO.
004650      05 �������Z���v�q             PIC 9(5)    VALUE ZERO.
         03 ���������k���v�q              PIC 9(4)    VALUE ZERO.
004660   03 �Č����v�q                    PIC 9(5)    VALUE ZERO.
004670   03 ���Âv�q.
004680      05 ���Ë����v�q               PIC 9(2)V9  VALUE ZERO.
004690      05 ���É񐔂v�q               PIC 9(2)    VALUE ZERO.
004700      05 ���×��v�q                 PIC 9(5)    VALUE ZERO.
004710      05 ���É��Z���v�q             PIC 9(5)    VALUE ZERO.
004720   03 �������q���Z���v�q            PIC 9(5)    VALUE ZERO.
004730   03 �{�p���񋟗��v�q            PIC 9(5)    VALUE ZERO.
004740   03 ���v�v�q                      PIC 9(6)    VALUE ZERO.
004750   03 �ꕔ���S���v�q                PIC 9(6)    VALUE ZERO.
004760   03 �������z�v�q                  PIC 9(6)    VALUE ZERO.
004770   03 ���t�����v�q                  PIC 9(1)    VALUE ZERO.
004780   03 �󋋎ҕ��S�z�v�q              PIC 9(6)    VALUE ZERO.
004790   03 �����������z�v�q              PIC 9(6)    VALUE ZERO.
004800*
004810* �������ʖ��̗���
004820***********************
004830 01 �����Q�v�q.
004840   03 ���񏈒u�v�q    OCCURS   9.
004850      05 ���񏈒u���v�q             PIC 9(5)    VALUE ZERO.
004860*
004870* �������̗���
004880***********************
004890 01 �����R�v�q.
004900**********
004910* �P���� *
004920**********
004930   03 ���ʂP�v�q.
004940      05 ��ÂP�v�q.
004950         07 ��ÒP���P�v�q              PIC 9(4)    VALUE ZERO.
004960         07 ��É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
004970         07 ��×��P�v�q                PIC 9(5)    VALUE ZERO.
004980      05 ��㪖@�P�v�q.
004990         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
005000         07 ��㪖@���P�v�q              PIC 9(4)    VALUE ZERO.
005010      05 ��㪖@�P�v�q.
005020         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
005030         07 ��㪖@���P�v�q              PIC 9(4)    VALUE ZERO.
005040      05 �d�ÂP�v�q.
005050         07 �d�É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
005060         07 �d�×��P�v�q                PIC 9(4)    VALUE ZERO.
005070      05 ���v�P�v�q                     PIC 9(6)    VALUE ZERO.
005080      05 �����������P�v�q               PIC 9(3)    VALUE ZERO.
005090      05 ���������v�P�v�q               PIC 9(6)    VALUE ZERO.
005100**********
005110* �Q���� *
005120**********
005130   03 ���ʂQ�v�q.
005140      05 ��ÂQ�v�q.
005150         07 ��ÒP���Q�v�q              PIC 9(4)    VALUE ZERO.
005160         07 ��É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
005170         07 ��×��Q�v�q                PIC 9(5)    VALUE ZERO.
005180      05 ��㪖@�Q�v�q.
005190         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
005200         07 ��㪖@���Q�v�q              PIC 9(4)    VALUE ZERO.
005210      05 ��㪖@�Q�v�q.
005220         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
005230         07 ��㪖@���Q�v�q              PIC 9(4)    VALUE ZERO.
005240      05 �d�ÂQ�v�q.
005250         07 �d�É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
005260         07 �d�×��Q�v�q                PIC 9(4)    VALUE ZERO.
005270      05 ���v�Q�v�q                     PIC 9(6)    VALUE ZERO.
005280      05 �����������Q�v�q               PIC 9(3)    VALUE ZERO.
005290      05 ���������v�Q�v�q               PIC 9(6)    VALUE ZERO.
005300******************
005310* �R���ʁ^�W�� *
005320******************
005330   03 ���ʂR�W�v�q.
005340      05 ��ÂR�W�v�q.
005350         07 ��ÒP���R�W�v�q              PIC 9(4)  VALUE ZERO.
005360         07 ��É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
005370         07 ��×��R�W�v�q                PIC 9(5)  VALUE ZERO.
005380      05 ��㪖@�R�W�v�q.
005390         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
005400         07 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
005410      05 ��㪖@�R�W�v�q.
005420         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
005430         07 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
005440      05 �d�ÂR�W�v�q.
005450         07 �d�É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
005460         07 �d�×��R�W�v�q                PIC 9(4)  VALUE ZERO.
005470      05 ���v�R�W�v�q                     PIC 9(6)  VALUE ZERO.
005480      05 �����ʍ����v�R�W�v�q             PIC 9(6)  VALUE ZERO.
005490      05 �����������R�W�v�q               PIC 9(3)  VALUE ZERO.
005500      05 ���������v�R�W�v�q               PIC 9(6)  VALUE ZERO.
005510******************
005520* �R���ʁ^�P�O�� *
005530******************
005540   03 ���ʂR�O�v�q.
005550      05 �����J�n�����R�O�v�q.
005560         07 �����J�n���R�O�v�q            PIC 9(2)  VALUE ZERO.
005570         07 �����J�n���R�O�v�q            PIC 9(2)  VALUE ZERO.
005580      05 ��ÂR�O�v�q.
005590         07 ��ÒP���R�O�v�q              PIC 9(4)  VALUE ZERO.
005600         07 ��É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
005610         07 ��×��R�O�v�q                PIC 9(5)  VALUE ZERO.
005620      05 ��㪖@�R�O�v�q.
005630         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
005640         07 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
005650      05 ��㪖@�R�O�v�q.
005660         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
005670         07 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
005680      05 �d�ÂR�O�v�q.
005690         07 �d�É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
005700         07 �d�×��R�O�v�q                PIC 9(4)  VALUE ZERO.
005710      05 ���v�R�O�v�q                     PIC 9(6)  VALUE ZERO.
005720      05 �����������R�O�v�q               PIC 9(3)  VALUE ZERO.
005730      05 ���������v�R�O�v�q               PIC 9(6)  VALUE ZERO.
005740****************
005750* �S���ʁ^�T�� *
005760****************
005770   03 ���ʂS�T�v�q.
005780      05 ��ÂS�T�v�q.
005790         07 ��ÒP���S�T�v�q              PIC 9(4)  VALUE ZERO.
005800         07 ��É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
005810         07 ��×��S�T�v�q                PIC 9(5)  VALUE ZERO.
005820      05 ��㪖@�S�T�v�q.
005830         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
005840         07 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
005850      05 ��㪖@�S�T�v�q.
005860         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
005870         07 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
005880      05 �d�ÂS�T�v�q.
005890         07 �d�É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
005900         07 �d�×��S�T�v�q                PIC 9(4)  VALUE ZERO.
005910      05 ���v�S�T�v�q                     PIC 9(6)  VALUE ZERO.
005920      05 �����ʍ����v�S�T�v�q             PIC 9(6)  VALUE ZERO.
005930      05 �����������S�T�v�q               PIC 9(3)  VALUE ZERO.
005940      05 ���������v�S�T�v�q               PIC 9(6)  VALUE ZERO.
005950****************
005960* �S���ʁ^�W�� *
005970****************
005980   03 ���ʂS�W�v�q.
005990      05 �����J�n�����S�W�v�q.
006000         07 �����J�n���S�W�v�q            PIC 9(2)  VALUE ZERO.
006010         07 �����J�n���S�W�v�q            PIC 9(2)  VALUE ZERO.
006020      05 ��ÂS�W�v�q.
006030         07 ��ÒP���S�W�v�q              PIC 9(4)  VALUE ZERO.
006040         07 ��É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
006050         07 ��×��S�W�v�q                PIC 9(5)  VALUE ZERO.
006060      05 ��㪖@�S�W�v�q.
006070         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
006080         07 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
006090      05 ��㪖@�S�W�v�q.
006100         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
006110         07 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
006120      05 �d�ÂS�W�v�q.
006130         07 �d�É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
006140         07 �d�×��S�W�v�q                PIC 9(4)  VALUE ZERO.
006150      05 ���v�S�W�v�q                     PIC 9(6)  VALUE ZERO.
006160      05 �����ʍ����v�S�W�v�q             PIC 9(6)  VALUE ZERO.
006170      05 �����������S�W�v�q               PIC 9(3)  VALUE ZERO.
006180      05 ���������v�S�W�v�q               PIC 9(6)  VALUE ZERO.
006190******************
006200* �S���ʁ^�P�O�� *
006210******************
006220   03 ���ʂS�O�v�q.
006230      05 �����J�n�����S�O�v�q.
006240         07 �����J�n���S�O�v�q            PIC 9(2)  VALUE ZERO.
006250         07 �����J�n���S�O�v�q            PIC 9(2)  VALUE ZERO.
006260      05 ��ÂS�O�v�q.
006270         07 ��ÒP���S�O�v�q              PIC 9(4)  VALUE ZERO.
006280         07 ��É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
006290         07 ��×��S�O�v�q                PIC 9(5)  VALUE ZERO.
006300      05 ��㪖@�S�O�v�q.
006310         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
006320         07 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
006330      05 ��㪖@�S�O�v�q.
006340         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
006350         07 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
006360      05 �d�ÂS�O�v�q.
006370         07 �d�É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
006380         07 �d�×��S�O�v�q                PIC 9(4)  VALUE ZERO.
006390      05 ���v�S�O�v�q                     PIC 9(6)  VALUE ZERO.
006400      05 �����������S�O�v�q               PIC 9(3)  VALUE ZERO.
006410      05 ���������v�S�O�v�q               PIC 9(6)  VALUE ZERO.
006420********************
006430* �T���ʁ^�Q�D�T�� *
006440********************
006450   03 ���ʂT�Q�v�q.
006460      05 ��ÂT�Q�v�q.
006470         07 ��ÒP���T�Q�v�q              PIC 9(4)  VALUE ZERO.
006480         07 ��É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
006490         07 ��×��T�Q�v�q                PIC 9(5)  VALUE ZERO.
006500      05 ��㪖@�T�Q�v�q.
006510         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
006520         07 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
006530      05 ��㪖@�T�Q�v�q.
006540         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
006550         07 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
006560      05 �d�ÂT�Q�v�q.
006570         07 �d�É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
006580         07 �d�×��T�Q�v�q                PIC 9(4)  VALUE ZERO.
006590      05 ���v�T�Q�v�q                     PIC 9(6)  VALUE ZERO.
006600      05 �����ʍ����v�T�Q�v�q             PIC 9(6)  VALUE ZERO.
006610      05 �����������T�Q�v�q               PIC 9(3)  VALUE ZERO.
006620      05 ���������v�T�Q�v�q               PIC 9(6)  VALUE ZERO.
006630****************
006640* �T���ʁ^�T�� *
006650****************
006660   03 ���ʂT�T�v�q.
006670      05 �����J�n�����T�T�v�q.
006680         07 �����J�n���T�T�v�q            PIC 9(2)  VALUE ZERO.
006690         07 �����J�n���T�T�v�q            PIC 9(2)  VALUE ZERO.
006700      05 ��ÂT�T�v�q.
006710         07 ��ÒP���T�T�v�q              PIC 9(4)  VALUE ZERO.
006720         07 ��É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
006730         07 ��×��T�T�v�q                PIC 9(5)  VALUE ZERO.
006740      05 ��㪖@�T�T�v�q.
006750         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
006760         07 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
006770      05 ��㪖@�T�T�v�q.
006780         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
006790         07 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
006800      05 �d�ÂT�T�v�q.
006810         07 �d�É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
006820         07 �d�×��T�T�v�q                PIC 9(4)  VALUE ZERO.
006830      05 ���v�T�T�v�q                     PIC 9(6)  VALUE ZERO.
006840      05 �����ʍ����v�T�T�v�q             PIC 9(6)  VALUE ZERO.
006850      05 �����������T�T�v�q               PIC 9(3)  VALUE ZERO.
006860      05 ���������v�T�T�v�q               PIC 9(6)  VALUE ZERO.
006870****************
006880* �T���ʁ^�W�� *
006890****************
006900   03 ���ʂT�W�v�q.
006910      05 �����J�n�����T�W�v�q.
006920         07 �����J�n���T�W�v�q            PIC 9(2)  VALUE ZERO.
006930         07 �����J�n���T�W�v�q            PIC 9(2)  VALUE ZERO.
006940      05 ��ÂT�W�v�q.
006950         07 ��ÒP���T�W�v�q              PIC 9(4)  VALUE ZERO.
006960         07 ��É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
006970         07 ��×��T�W�v�q                PIC 9(5)  VALUE ZERO.
006980      05 ��㪖@�T�W�v�q.
006990         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
007000         07 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
007010      05 ��㪖@�T�W�v�q.
007020         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
007030         07 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
007040      05 �d�ÂT�W�v�q.
007050         07 �d�É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
007060         07 �d�×��T�W�v�q                PIC 9(4)  VALUE ZERO.
007070      05 ���v�T�W�v�q                     PIC 9(6)  VALUE ZERO.
007080      05 �����ʍ����v�T�W�v�q             PIC 9(6)  VALUE ZERO.
007090      05 �����������T�W�v�q               PIC 9(3)  VALUE ZERO.
007100      05 ���������v�T�W�v�q               PIC 9(6)  VALUE ZERO.
007110******************
007120* �T���ʁ^�P�O�� *
007130******************
007140   03 ���ʂT�O�v�q.
007150      05 �����J�n�����T�O�v�q.
007160         07 �����J�n���T�O�v�q            PIC 9(2)  VALUE ZERO.
007170         07 �����J�n���T�O�v�q            PIC 9(2)  VALUE ZERO.
007180      05 ��ÂT�O�v�q.
007190         07 ��ÒP���T�O�v�q              PIC 9(4)  VALUE ZERO.
007200         07 ��É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
007210         07 ��×��T�O�v�q                PIC 9(5)  VALUE ZERO.
007220      05 ��㪖@�T�O�v�q.
007230         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
007240         07 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
007250      05 ��㪖@�T�O�v�q.
007260         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
007270         07 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
007280      05 �d�ÂT�O�v�q.
007290         07 �d�É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
007300         07 �d�×��T�O�v�q                PIC 9(4)  VALUE ZERO.
007310      05 ���v�T�O�v�q                     PIC 9(6)  VALUE ZERO.
007320      05 �����������T�O�v�q               PIC 9(3)  VALUE ZERO.
007330      05 ���������v�T�O�v�q               PIC 9(6)  VALUE ZERO.
007340*
007350**************
007360* �{�p����� *
007370**************
007380 01 �{�p�����v.
007390    03 �_���t�ԍ��v                    PIC X(22)  VALUE SPACE.
007400    03 ����ڍ��t�����ԍ��v.
007410       05 �ڍ��t��v                 PIC X(8)   VALUE SPACE.
007420       05 �ڍ��t�����ԍ��v           PIC X(10)  VALUE SPACE.
007430    03 ��\�҃J�i�v                    PIC X(50)  VALUE SPACE.
007440    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
007450    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
          03 �s���{���i�h�r�v                PIC X(2)   VALUE SPACE.
007460    03 �{�p���Z���v.
007470       05 �{�p���Z���P�v               PIC X(50)  VALUE SPACE.
007480       05 �{�p���Z���Q�v               PIC X(50)  VALUE SPACE.
007490*    03 �{�p���Z���v.
007500*       05 �{�p���Z���P�v               PIC X(28)  VALUE SPACE.
007510*       05 �{�p���Z���Q�v               PIC X(28)  VALUE SPACE.
007520*       05 �{�p���Z���R�v               PIC X(28)  VALUE SPACE.
007530*
007540    03 �{�p���X�֔ԍ��v.
007550       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
007560       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
007570    03 �{�p���d�b�ԍ��v                PIC X(15)  VALUE SPACE.
007580    03 ��z���󗝔ԍ��v                PIC X(15)  VALUE SPACE.
007590    03 �󗝔N�����v.
007600       05 �󗝔N�v                     PIC 9(2)   VALUE ZERO.
007610       05 �󗝌��v                     PIC 9(2)   VALUE ZERO.
007620       05 �󗝓��v                     PIC 9(2)   VALUE ZERO.
007630    03 �ŏI�ʉ@�N�����v.
007640       05 �ŏI�ʉ@�N�v                 PIC 9(2)   VALUE ZERO.
007650       05 �ŏI�ʉ@���v                 PIC 9(2)   VALUE ZERO.
007660       05 �ŏI�ʉ@���v                 PIC 9(2)   VALUE ZERO.
007670    03 �_���t�N�����v.
007680       05 �_���t�N�v                   PIC 9(2)   VALUE ZERO.
007690       05 �_���t���v                   PIC 9(2)   VALUE ZERO.
007700       05 �_���t���v                   PIC 9(2)   VALUE ZERO.
007710    03 ���҈ϔC�N�����v.
007720       05 ���҈ϔC�N�v                 PIC 9(2)   VALUE ZERO.
007730       05 ���҈ϔC���v                 PIC 9(2)   VALUE ZERO.
007740       05 ���҈ϔC���v                 PIC 9(2)   VALUE ZERO.
007750    03 �������v.
007760       05 ������s���v               PIC X(40)  VALUE SPACE.
007770       05 ������s�x�X���v           PIC X(40)  VALUE SPACE.
007780       05 �a����ʂv                   PIC 9(1)   VALUE ZERO.
007790       05 �����ԍ��v                   PIC X(10)  VALUE SPACE.
007800       05 �������`�l�v                 PIC X(40)  VALUE SPACE.
007810       05 �������`�l�J�i�v             PIC X(40)  VALUE SPACE.
007820       05 ��s���x�X���v               PIC X(60)  VALUE SPACE.
007830       05 �a����ʃR�����g�v           PIC N(4)   VALUE SPACE.
          03 �x���@��.
             05 ���Z�@�֖��v.
                07 ���Z�@�֖��P�v            PIC X(8) VALUE SPACE.
                07 ���Z�@�֖��Q�v            PIC X(8) VALUE SPACE.
                07 ���Z�@�֖��R�v            PIC X(8) VALUE SPACE.
                07 ���Z�@�֖��S�v            PIC X(8) VALUE SPACE.
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
007840    03 ���{�p�h�c�v                    PIC X(15)  VALUE SPACE.
007850    03 �s�����{�p�h�c�v                PIC X(15)  VALUE SPACE.
007860**************
007870* ��f�ҏ�� *
007880**************
007890 01 ��f�ҏ��v.
007900*    03 �{�p�a��v                      PIC N(2)  VALUE SPACE.
      */�����C��/20190408
          03 �{�p�a��v                      PIC 9(1)   VALUE ZERO.
007910    03 �{�p�N���v.
007920       05 �{�p�N�v                     PIC 9(2)   VALUE ZERO.
007930       05 �{�p���v                     PIC 9(2)   VALUE ZERO.
007940*    03 �L���v                          PIC N(12)  VALUE SPACE.
007570    03 �L���v.
007580       05 ����L���v                   PIC N(12)  VALUE SPACE.
007950*
007960    03 �ԍ��v.
007970       05 ����ԍ��v                   PIC X(15)  VALUE SPACE.
007980       05 FILLER                       PIC X(15)  VALUE SPACE.
007990*    03 �ԍ��v.
008000*       05 ����ԍ��P�v                 PIC X(10)  VALUE SPACE.
008010*       05 ����ԍ��Q�v                 PIC X(10)  VALUE SPACE.
008020*       05 FILLER                       PIC X(10)  VALUE SPACE.
008030*
          03 �L���ԍ��v.
             05 �L���ԍ��w�v                 PIC X(40) VALUE SPACE.
008040    03 �ی��Ҕԍ��v.
008050       05 ����ی��Ҕԍ��v             PIC X(8)   VALUE SPACE.
008060       05 FILLER                       PIC X(2)   VALUE SPACE.
008070*
008080    03 �s�����ԍ��v.
008090       05 ����s�����ԍ��v             PIC X(8)   VALUE SPACE.
008100       05 FILLER                       PIC X(2)   VALUE SPACE.
008110*    03 �󋋎Ҕԍ��v.
008120*       05 ����󋋎Ҕԍ��v             PIC X(8)   VALUE SPACE.
008130*       05 FILLER                       PIC X(12).
           03 �󋋎Ҕԍ��v.
              05 ����󋋎Ҕԍ��v            PIC X(7)  VALUE SPACE.
              05 ����󋋎Ҕԍ��Q�v          PIC X(8)  VALUE SPACE.
008140*
008150    03 �����於�̂v.
008160       05 ��������於�̂P�v           PIC X(48)  VALUE SPACE.
008170       05 ��������於�̂Q�v           PIC X(48)  VALUE SPACE.
008180*
008190    03 �ی���ʂv                      PIC 9(2)   VALUE ZERO.
008200    03 ��ی��ҏ��v.
008210       05 ��ی��҃J�i�v               PIC X(50)  VALUE SPACE.
008220       05 ��ی��Ҏ����v               PIC X(50)  VALUE SPACE.
008230       05 ��ی��Ґ��ʂv               PIC N(1)   VALUE SPACE.
008240       05 �X�֔ԍ��v.
008250          07 �X�֔ԍ��P�v              PIC X(3)   VALUE SPACE.
008260          07 �X�֔ԍ��Q�v              PIC X(4)   VALUE SPACE.
008270       05 ��ی��ҏZ���P�v             PIC X(50)  VALUE SPACE.
008280       05 ��ی��ҏZ���Q�v             PIC X(50)  VALUE SPACE.
008290    03 ���ҏ��v.
008300       05 ���ҏZ���v.
008310          07 ���ҏZ���P�v              PIC X(50)  VALUE SPACE.
008320          07 ���ҏZ���Q�v              PIC X(50)  VALUE SPACE.
008990       05 �d�b�ԍ��v                   PIC X(35)  VALUE SPACE.
008330       05 ���҃J�i�v                   PIC X(50)  VALUE SPACE.
008340       05 ���Ҏ����v                   PIC X(50)  VALUE SPACE.
008350       05 ���Ґ��ʂv                   PIC X(4)   VALUE SPACE.
008360       05 ���ʃ`�F�b�N�v.
008370          07 �j�`�F�b�N�v              PIC N(1)  VALUE SPACE.
008380          07 ���`�F�b�N�v              PIC N(1)  VALUE SPACE.
008390       05 �a��`�F�b�N�v.
008400          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008410          07 �吳�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008420          07 ���a�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008430          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008440          07 �����v                    PIC N(2)  VALUE SPACE.
      */�����C��/������20190408
008210          07 �ߘa�`�F�b�N�v            PIC N(1)  VALUE SPACE.
                07 �ߘa�b�l�v                PIC X(4)  VALUE SPACE.
009110*          07 �����v                    PIC N(2)  VALUE SPACE.
      */�����C��/������20190408
008450       05 ���ҔN�v                     PIC 9(2)  VALUE ZERO.
008460       05 ���Ҍ��v                     PIC 9(2)  VALUE ZERO.
008470       05 ���ғ��v                     PIC 9(2)  VALUE ZERO.
008480       05 �����v.
008490          07 ��������v                PIC N(4)  VALUE SPACE.
008500          07 FILLER                    PIC X(4)  VALUE SPACE.
008510*
008520*       05 ���������v                   PIC N(40) OCCURS 27 VALUE SPACE.
      */���p�Ή�/110421
             05 ���������v OCCURS 29.
                07 ���������w�v              PIC X(80)  VALUE SPACE.
008530*
008540       05 �ی���ʖ��̂v               PIC N(3)  VALUE SPACE.
008540       05 �ی���ʖ��̂Q�v             PIC N(5)  VALUE SPACE.
008910       05 �ی���ʃ`�F�b�N�v.
                07 ���ۃ`�F�b�N�v            PIC N(1)   VALUE SPACE.
                07 ����`�F�b�N�v            PIC N(1)   VALUE SPACE.
                07 �g���`�F�b�N�v            PIC N(1)   VALUE SPACE.
                07 ���σ`�F�b�N�v            PIC N(1)   VALUE SPACE.
                07 ����`�F�b�N�v            PIC N(1)   VALUE SPACE.
                07 �ސE�`�F�b�N�v            PIC N(1)   VALUE SPACE.
             05 �{�l�`�F�b�N�v               PIC N(1)   VALUE SPACE.
             05 �Ƒ��`�F�b�N�v               PIC N(1)   VALUE SPACE.
             05 �P�ƃ`�F�b�N�v               PIC N(1)   VALUE SPACE.
             05 �Q���`�F�b�N�v               PIC N(1)   VALUE SPACE.
             05 ����`�F�b�N�v               PIC N(1)   VALUE SPACE.
             05 ���V�`�F�b�N�v               PIC N(1)   VALUE SPACE.
             05 �U�΃`�F�b�N�v               PIC N(1)   VALUE SPACE.
             05 �V���`�F�b�N�v               PIC N(1)   VALUE SPACE.
             05 �W���`�F�b�N�v               PIC N(1)   VALUE SPACE.
             05 �X���`�F�b�N�v               PIC N(1)   VALUE SPACE.
             05 �P�O���`�F�b�N�v             PIC N(1)   VALUE SPACE.
008550*
008560    03 ������v                        PIC N(1)  VALUE SPACE.
008561    03 ���ʃ}�[�N�v                    PIC N(1)  VALUE SPACE.
008562    03 ���ʃR�����g�v                  PIC X(16) VALUE SPACE.
008570*
008580****************
008590* �����f�[�^�e *
008600****************
008610 01 �������v.
008620    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
008630    03 ���ʏ��v  OCCURS   9.
008640       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
008650       05 ���ʃR�[�h�v.
008660          07 ������ʂv                PIC 9(2)  VALUE ZERO.
008670          07 ���ʂv                    PIC 9(2)  VALUE ZERO.
008680          07 ���E�敪�v                PIC 9(1)  VALUE ZERO.
008690          07 �����ʒu�ԍ��v            PIC 9(2)  VALUE ZERO.
008700       05 �������v                     PIC N(18) VALUE SPACE.
008710       05 �����N�����v.
008720          07 �����N�v                  PIC 9(2)  VALUE ZERO.
008730          07 �������v                  PIC 9(2)  VALUE ZERO.
008740          07 �������v                  PIC 9(2)  VALUE ZERO.
008750       05 �����N�����v.
008760          07 �����N�v                  PIC 9(2)  VALUE ZERO.
008770          07 �������v                  PIC 9(2)  VALUE ZERO.
008780          07 �������v                  PIC 9(2)  VALUE ZERO.
008790       05 �J�n�N�����v.
008800          07 �J�n�N�v                  PIC 9(2)  VALUE ZERO.
008810          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
008820          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
008830       05 �I���N�����v.
008840          07 �I���N�v                  PIC 9(2)  VALUE ZERO.
008850          07 �I�����v                  PIC 9(2)  VALUE ZERO.
008860          07 �I�����v                  PIC 9(2)  VALUE ZERO.
008870       05 �������v                     PIC 9(2)  VALUE ZERO.
008880       05 �]�A�敪�v                   PIC 9(1)  VALUE ZERO.
008890       05 �]�A�敪�`�F�b�N�v.
008900          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008910          07 ���~�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008920          07 �]��`�F�b�N�v            PIC N(1)  VALUE SPACE.
008930       05 �]�A�v                       PIC N(2)  VALUE SPACE.
008940       05 �J�n�N�����擾�t���O         PIC X(3)  VALUE SPACE.
008950       05 ���ʋ�؂v                   PIC X(1)  VALUE SPACE.
008960       05 �o�ߗ��̂v.
008970          07 ����o�ߗ��̂v            PIC N(5)  VALUE SPACE.
008980          07 FILLER                    PIC X(2)  VALUE SPACE.
008990    03 �o�ߕ��ʂv                      PIC N(1)  VALUE SPACE.
009000    03 �V�K�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
009010    03 �p���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
009020    03 �����敪�v                      PIC N(2)  VALUE SPACE.
009030*
009040************
009050* ������� *
009060************
009070 01 �������v.
009080    03 �������Z�v.
009090       05 ���ԊO�`�F�b�N�v                PIC N(1) VALUE SPACE.
009100       05 �x���`�F�b�N�v                  PIC N(1) VALUE SPACE.
009110       05 �[��`�F�b�N�v                  PIC N(1) VALUE SPACE.
009120       05 ���ԊO�v                        PIC N(3) VALUE SPACE.
009130       05 �x���v                          PIC N(2) VALUE SPACE.
009140       05 �[��v                          PIC N(2) VALUE SPACE.
009150       05 �������Z���e�v                  PIC N(10) VALUE SPACE.
009160    03 ���É��Z�v.
009170       05 ��ԃ`�F�b�N�v                  PIC N(1) VALUE SPACE.
009180       05 ���Ð[��`�F�b�N�v              PIC N(1) VALUE SPACE.
009190       05 ��H�`�F�b�N�v                  PIC N(1) VALUE SPACE.
009200       05 �\���J��`�F�b�N�v              PIC N(1) VALUE SPACE.
009210    03 �������q�`�F�b�N�v.
009220       05 ��`�F�b�N�v                    PIC N(1) VALUE SPACE.
009230       05 ���`�F�b�N�v                    PIC N(1) VALUE SPACE.
009240       05 ���`�F�b�N�v                    PIC N(1) VALUE SPACE.
009250       05 ������v                        PIC N(1) VALUE SPACE.
009260       05 �������v                        PIC N(1) VALUE SPACE.
009270       05 �������v                        PIC N(1) VALUE SPACE.
009280    03 ���v�v                             PIC 9(7) VALUE ZERO.
009290    03 ���񏈒u�����v�v                   PIC 9(6) VALUE ZERO.
009300    03 ���񏈒u���`�F�b�N�v.
009310       05 �������`�F�b�N�v                PIC N(1) VALUE SPACE.
009320       05 �Œ藿�`�F�b�N�v                PIC N(1) VALUE SPACE.
009330       05 �{�×��`�F�b�N�v                PIC N(1) VALUE SPACE.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
          03 �����񐔂v                         PIC 9(2)  VALUE ZERO.
          03 �^�����v                           PIC 9(4)  VALUE ZERO.
009340************
009350* ���l��� *
009360************
009370 01 ���l���v.
009380    03 �K�p�P�v                           PIC N(38) VALUE SPACE.
009390    03 �K�p�Q�v                           PIC N(38) VALUE SPACE.
009400*    03 �K�p�R�v                        PIC N(38) VALUE SPACE.
009410*    03 �K�p�S�v                        PIC N(38) VALUE SPACE.
009420*    03 �o�߃R�����g�v                     PIC N(60) VALUE SPACE.
009430*
009440* ���O���� *
009450    03 ���Z�v�g�Ǘ��N�v.
009460       05 ���Z�Ǘ����I�v                  PIC 9(2)  VALUE ZERO.
009470       05 ���Z�Ǘ�����v                  PIC 9(2)  VALUE ZERO.
009480    03 �����\���Ԃv                       PIC 9(4)  VALUE ZERO.
009870*
002060** ����}�X�^�p
002140 01 ���Z�v�g�V���敪.
002150    03 �������Z�v                      PIC 9 VALUE ZERO.
009490***
009500 01 �������.
009510     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
009520     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
009530     03 ������ʂo                     PIC X(2) VALUE SPACE.
009540     03 �g������o.
009550         05 �[������o.
009560             07 �ړ������o             PIC X(1) VALUE SPACE.
009570             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
009580         05 �ڍא���o                 PIC X(2) VALUE SPACE.
009590     03 �ʒm���o                     PIC X(2) VALUE SPACE.
009600     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
009610*
009620 01 �v�Z�@����N�v                     PIC 9(2) VALUE ZERO.
009630* ���t�v�n�q�j
009640 01 �a��I���N�v                       PIC 9(4) VALUE ZERO.
009650 01 �v�Z�@����.
009660    03 �v�Z�@����N                    PIC 9(4) VALUE ZERO.
009670    03 �v�Z�@�����                  PIC 9(4) VALUE ZERO.
009680 01 �v�Z�@����q REDEFINES �v�Z�@����.
009690    03 �v�Z�@���I                      PIC 9(2).
009700    03 �v�Z�@���t                      PIC 9(6).
009710    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
009720       05 �v�Z�@�N��                   PIC 9(4).
009730       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
009740         07 �v�Z�@�N                   PIC 9(2).
009750         07 �v�Z�@��                   PIC 9(2).
009760       05 �v�Z�@��                     PIC 9(2).
009770*
      * C �A�g�p
       01  �����P�v        PIC X(4096).
       01  �����Q�v        PIC X(512).
       01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
      *
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
009780******************************************************************
009790*                          �A������                              *
009800******************************************************************
009810*
009820**  ��ʓ��̓f�[�^
009830 01 �A���|���̓f�[�^�ϔC��� IS EXTERNAL.
009840    03 �A���|�ϔC���                     PIC 9.
014620*
       01 �A���|���̓f�[�^�d�b��� IS EXTERNAL.
          03 �A���|�d�b���                     PIC 9.
009190*
       01 �A���|�v���r���[ IS EXTERNAL.
          03 �A���|�v���r���[�敪          PIC 9.
009860*
009870************
009880* ����L�[ *
009890************
009900*
009910 01 �A����|�Ώۃf�[�^ IS EXTERNAL.
009920    03 �A����|�{�p�N����.
009930       05 �A����|�{�p�a��                  PIC 9(1).
009940       05 �A����|�{�p�N                    PIC 9(2).
009950       05 �A����|�{�p��                    PIC 9(2).
009960    03 �A����|���҃R�[�h.
009970       05 �A����|���Ҕԍ�                  PIC 9(6).
009980       05 �A����|�}��                      PIC X(1).
009990    03 �A����|�ی����                     PIC 9(2).
010000    03 �A����|�ی��Ҕԍ�                   PIC X(10).
010010    03 �A����|������                     PIC 9(2).
010020    03 �A����|��p���S�Ҕԍ�               PIC X(10).
010030    03 �A����|�������                     PIC 9(2).
010040    03 �A����|��p���S�Ҕԍ�����           PIC X(10).
010050    03 �A����|���҃J�i                     PIC X(20).
010060    03 �A����|�{�l�Ƒ��敪                 PIC 9(1).
013490*
013500 01 �A���|�L�[ IS EXTERNAL.
013510    03 �A���|�ی����                  PIC 9(2).
013520************************
013530** �R�J����������
013540************************
013550 01 �A���ԁ|�L�[ IS EXTERNAL.
013560    03 �A���ԁ|�{�p�N��.
013570       05 �A���ԁ|�{�p�a��               PIC 9.
013580       05 �A���ԁ|�{�p�N                 PIC 9(2).
013590       05 �A���ԁ|�{�p��                 PIC 9(2).
013600    03  �A���ԁ|���҃R�[�h.
013610       05 �A���ԁ|���Ҕԍ�               PIC 9(6).
013620       05 �A���ԁ|�}��                   PIC X.
013630    03 �A���ԁ|�Ώۃt���O                PIC X(3).
013640    03 �A���ԁ|���Ԍ��v.
013650       05 �A���ԁ|���Ԃv                 PIC 9(2) OCCURS 9.
013660*
013670************************
013680* �������R���Z�b�g     *
013690************************
013700 01 �A�����|�L�[ IS EXTERNAL.
013710    03 �A�����|�{�p�N��.
013720       05 �A�����|�{�p�a��               PIC 9.
013730       05 �A�����|�{�p�N                 PIC 9(2).
013740       05 �A�����|�{�p��                 PIC 9(2).
013750    03  �A�����|���҃R�[�h.
013760       05 �A�����|���Ҕԍ�               PIC 9(6).
013770       05 �A�����|�}��                   PIC X.
013780    03 �A�����|������                    PIC 9(2).
013790    03 �A�����|���R��                    PIC N(63) OCCURS 15.
013800*
016640* ���S���擾�p14/10�`
016650 01 �A���|���S���擾�L�[ IS EXTERNAL.
016660    03 �A���|�{�p�a��N��.
016670       05 �A���|�{�p�a��               PIC 9.
016680       05 �A���|�{�p�N��.
016690          07 �A���|�{�p�N              PIC 9(2).
016700          07 �A���|�{�p��              PIC 9(2).
016710    03 �A���|���҃R�[�h.
016720       05 �A���|���Ҕԍ�               PIC 9(6).
016730       05 �A���|�}��                   PIC X.
016740    03 �A���|���ە��S��                PIC 9(3).
016750    03 �A���|���ۖ{�̕��S��            PIC 9(3).
016760    03 �A���|���ە��S��                PIC 9(3).
016770    03 �A���|�Q�V�V���S��              PIC 9(3).
016780    03 �A���|�������S��                PIC 9(3).
016790    03 �A���|���ʗp���S��              PIC 9(3).
016800*
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
013702*************
013703* ��������
013704*************
013705 01 �A�������́|�L�[ IS EXTERNAL.
013706    03 �A�������́|�������             PIC 9(2).
013707    03 �A�������́|��p���S�Ҕԍ�����   PIC X(10).
013708*   / OUT /
013709    03 �A�������́|���̏W�c.
013710       05 �A�������́|�P����            PIC N.
013711       05 �A�������́|����              PIC N(4).
013712       05 �A�������́|��������          PIC N(10).
013713*
013810******************************************************************
013820*                      PROCEDURE  DIVISION                       *
013830******************************************************************
013840 PROCEDURE               DIVISION.
013850************
013860*           *
013870* ��������   *
013880*           *
013890************
002570     PERFORM �v�����^�t�@�C���쐬.
013900     PERFORM ������.
013910************
013920*           *
013930* �又��     *
013940*           *
013950************
013960* ���
013970     PERFORM �A�����ڑҔ�.
013980     PERFORM ����Z�b�g.
013990     PERFORM �������.
014000************
014010*           *
014020* �I������   *
014030*           *
014040************
014050     PERFORM ��f�҈���敪�X�V.
014060     PERFORM �I������.
014070     MOVE ZERO  TO PROGRAM-STATUS.
014080     EXIT PROGRAM.
014090*
014100*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YHP6425"             TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪  TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014110*================================================================*
014120 ������ SECTION.
014130*================================================================*
014140*
014150     PERFORM �t�@�C���I�[�v��.
014160*    /* ���ݓ��t�擾 */
014170     ACCEPT �v�Z�@���t FROM DATE.
014180*    /* 1980�`2079�N�̊ԂŐݒ� */
014190     IF �v�Z�@�N > 80
014200         MOVE 19 TO �v�Z�@���I
014210     ELSE
014220         MOVE 20 TO �v�Z�@���I
014230     END-IF.
014240     PERFORM �J�����g�����擾.
014250     PERFORM �a��I���N�擾.
014260     COMPUTE �v�Z�@����N�v = �v�Z�@����N - 1988.
014270*================================================================*
014280 �t�@�C���I�[�v�� SECTION.
014290*
014330     OPEN INPUT   �s�����}�X�^
014340         MOVE NC"�s����" TO �t�@�C����.
014350     OPEN INPUT   �����}�X�^
014360         MOVE NC"����" TO �t�@�C����.
014370         PERFORM �I�[�v���`�F�b�N.
014380     OPEN INPUT   ���̃}�X�^
014390         MOVE NC"����" TO �t�@�C����.
014400         PERFORM �I�[�v���`�F�b�N.
007560     OPEN INPUT   ���Z�v�g�e
007570         MOVE NC"���Z" TO �t�@�C����.
007580         PERFORM �I�[�v���`�F�b�N.
014440     OPEN INPUT   ������}�X�^
014450         MOVE NC"������" TO �t�@�C����.
014460         PERFORM �I�[�v���`�F�b�N.
014470     OPEN INPUT   �{�p�����}�X�^
014480         MOVE NC"�{��" TO �t�@�C����.
014490         PERFORM �I�[�v���`�F�b�N.
014500     OPEN INPUT   ������}�X�^
014510         MOVE NC"����" TO �t�@�C����.
014520         PERFORM �I�[�v���`�F�b�N.
014530     OPEN INPUT   �o�߃}�X�^
014540         MOVE NC"�o��" TO �t�@�C����.
014550         PERFORM �I�[�v���`�F�b�N.
014560     OPEN INPUT   �{�p�L�^�e.
014570         MOVE NC"�{�L�e" TO �t�@�C����.
014580         PERFORM �I�[�v���`�F�b�N.
014590     OPEN INPUT   �����f�[�^�e.
014600         MOVE NC"����" TO �t�@�C����.
014610         PERFORM �I�[�v���`�F�b�N.
014620     OPEN INPUT   ���������e.
014630         MOVE NC"��������" TO �t�@�C����.
014640         PERFORM �I�[�v���`�F�b�N.
014650     OPEN INPUT  �h�c�Ǘ��}�X�^.
014660         MOVE NC"�h�c" TO �t�@�C����.
014670         PERFORM �I�[�v���`�F�b�N.
014680     OPEN I-O   ��f�ҏ��e.
014690         MOVE NC"���" TO �t�@�C����.
014700         PERFORM �I�[�v���`�F�b�N.
014710     OPEN INPUT �����}�X�^.
014720         MOVE NC"����" TO �t�@�C����.
014730         PERFORM �I�[�v���`�F�b�N.
014740     OPEN INPUT   ����}�X�^.
014750         MOVE NC"���" TO �t�@�C����.
014760         PERFORM �I�[�v���`�F�b�N.
015560     OPEN INPUT   ��f�ҏ��Q�e.
015570         MOVE NC"��f�ҏ��Q�e" TO �t�@�C����.
015580         PERFORM �I�[�v���`�F�b�N.
014770     OPEN INPUT   ��ƃt�@�C���Q.
014780         MOVE NC"��Q" TO �t�@�C����.
014790         PERFORM �I�[�v���`�F�b�N.
014800     OPEN I-O   ����t�@�C��
014810         PERFORM �G���[�����o.
014820*================================================================*
014830 �I�[�v���`�F�b�N SECTION.
014840*
014850     IF ��ԃL�[  NOT =  "00"
014860         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
014870         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
014880         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
014890                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014900         ACCEPT  �L�[���� FROM CONS
014910         PERFORM �t�@�C����
014920         EXIT PROGRAM.
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
017300         MOVE ���|�������Z             TO �������Z�v
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
015360 �A�����ڑҔ� SECTION.
015370*
015380     MOVE �A����|�{�p�a��           TO �{�p�a��v�q.
015390     MOVE �A����|�{�p�N             TO �{�p�N�v�q.
015400     MOVE �A����|�{�p��             TO �{�p���v�q.
015410     MOVE �A����|�ی����           TO �ی���ʂv�q.
015420     MOVE �A����|�ی��Ҕԍ�         TO �ی��Ҕԍ��v�q.
015430     MOVE �A����|������           TO �����ʂv�q.
015440     MOVE �A����|��p���S�Ҕԍ�     TO ��p���S�Ҕԍ��v�q.
015450     MOVE �A����|�������           TO ������ʂv�q.
015460     MOVE �A����|��p���S�Ҕԍ����� TO ��p���S�Ҕԍ������v�q.
015470     MOVE �A����|�{�l�Ƒ��敪       TO �{�l�Ƒ��敪�v�q.
015480     MOVE �A����|���҃J�i           TO ���҃J�i�v�q.
015490     MOVE �A����|���Ҕԍ�           TO ���Ҕԍ��v�q.
015500     MOVE �A����|�}��               TO �}�Ԃv�q.
015510*================================================================*
015520 ����Z�b�g SECTION.
015530*
015540     PERFORM ���ڏ�����.
           PERFORM ��{���擾.
015550     PERFORM �{�p�����擾.
015560     PERFORM ��������擾.
015570     PERFORM ��f�ҏ��擾.
015580     PERFORM �����f�[�^�擾.
015590     PERFORM �������擾.
015600     PERFORM �{�p�L�^�擾.
015610*******     PERFORM ��������擾.
015620*******     PERFORM �������ȑO�̃f�[�^����.
015630     PERFORM �������Z�����擾.
015640*
015650     PERFORM ���t�����擾.
015660     PERFORM ���S�����擾.
015670     PERFORM ������擾.
015680     PERFORM ��{���擾.
015690     PERFORM �{�p����N�擾.
015700     PERFORM ���Z�v�g���я��擾.
015710     PERFORM �ϔC�N�����擾.
           PERFORM �{�p���擾.
016791*-----------------------------------------------*
016800     IF ( ������������敪�v  NOT = 1 ) AND ( ���Z������������敪�v NOT = 1 )
016813        IF ( ������������敪�v = 3 OR 4 )
016815           PERFORM ������������Ώ۔��菈��
016817        ELSE
016820           PERFORM ���������擾
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
015940     IF ( �������R����敪�v NOT = 1 ) AND
015950        ( �A���ԁ|�Ώۃt���O = "YES" )
               MOVE �������R����敪�v TO �A�E���|�����敪
015820     END-IF.
015830**
015840********************
015850* ��f�ҏ��Z�b�g *
015860********************
015870*
015880*     MOVE �{�p�a��v          TO �{�p�a��.
015890     MOVE �{�p�N�v            TO �{�p�N.
015900     MOVE �{�p���v            TO �{�p��.
016130*
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
           MOVE ���ۃ`�F�b�N�v   TO ���ۃ`�F�b�N.
           MOVE ����`�F�b�N�v   TO ����`�F�b�N.
           MOVE �g���`�F�b�N�v   TO �g���`�F�b�N.
           MOVE ���σ`�F�b�N�v   TO ���σ`�F�b�N.
           MOVE ����`�F�b�N�v   TO ����`�F�b�N.
           MOVE �ސE�`�F�b�N�v   TO �ސE�`�F�b�N.
           MOVE �{�l�`�F�b�N�v   TO �{�l�`�F�b�N.
           MOVE �Ƒ��`�F�b�N�v   TO �Ƒ��`�F�b�N.
           MOVE �P�ƃ`�F�b�N�v   TO �P�ƃ`�F�b�N.
           MOVE �Q���`�F�b�N�v   TO �Q���`�F�b�N.
           MOVE ����`�F�b�N�v   TO ����`�F�b�N.
           MOVE ���V�`�F�b�N�v   TO ���V�`�F�b�N.
           MOVE �U�΃`�F�b�N�v   TO �U�΃`�F�b�N.
           MOVE �V���`�F�b�N�v   TO �V���`�F�b�N.
           MOVE �W���`�F�b�N�v   TO �W���`�F�b�N.
           MOVE �X���`�F�b�N�v   TO �X���`�F�b�N.
           MOVE �P�O���`�F�b�N�v TO �P�O���`�F�b�N.
      */�����C��/������20190408
037370     IF �{�p�a��v > 4
              MOVE �{�p�a��v         TO ���|�����敪
037380        READ �����}�X�^
037390        NOT INVALID KEY
037400            MOVE ���|��������   TO �{�p�a��
037410        END-READ
              MOVE "===="             TO �{�p�a�����
           END-IF.
      */�����C��/������20190408
016140     IF  ����s�����ԍ��v(1:2) = "99"
016150         MOVE SPACE              TO ����S�Ҕԍ�
016160     ELSE
016170         MOVE ����s�����ԍ��v   TO ����S�Ҕԍ�
016180     END-IF.
016190*
016200     IF ( ����󋋎Ҕԍ��v(1:1) = "*"  ) OR
016210        ( ����󋋎Ҕԍ��v(1:2) = "��" )
016220        MOVE  SPACE              TO �󋋎Ҕԍ�
016230     ELSE
      */�󋋎Ҕԍ����W�����ȏ�̏ꍇ�g�𖳎����Ĉ������/110425
               IF ����󋋎Ҕԍ��Q�v = SPACE
016830             MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
               ELSE
                   MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
               END-IF
016250     END-IF.
016260*
016270     MOVE ����ی��Ҕԍ��v    TO �ی��Ҕԍ�.
016280*     MOVE �����於�̂v        TO �ی��Җ���.
016290     MOVE ��������於�̂P�v  TO �ی��Җ��̂P.
016300     MOVE ��������於�̂Q�v  TO �ی��Җ��̂Q.
016310*     MOVE �ی���ʖ��̂v      TO �ی����.
016310*     MOVE �ی���ʖ��̂Q�v    TO �ی���ʂQ.
016320*     MOVE ��ی��҃J�i�v      TO ��ی��҃J�i.
016330*     MOVE ��ی��Ґ��ʂv      TO ��ی��Ґ���.
016340     MOVE ��ی��Ҏ����v      TO ��ی��Ҏ���.
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
016400     MOVE ���ҏZ���P�v        TO �Z���P.
016410     MOVE ���ҏZ���Q�v        TO �Z���Q.
016420*     MOVE ���҃J�i�v          TO �󋋎҃J�i.
016420     MOVE ���҃J�i�v          TO ���҃J�i.
016430     MOVE ���Ҏ����v          TO ���Ҏ���.
016440*     MOVE ���Ґ��ʂv          TO ���Ґ���.
016450     MOVE �j�`�F�b�N�v        TO �j�`�F�b�N.
016460     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
016470     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
016480     MOVE �吳�`�F�b�N�v      TO �吳�`�F�b�N.
016490     MOVE ���a�`�F�b�N�v      TO ���a�`�F�b�N.
016500     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
      */�����C��������/20190408
           MOVE �ߘa�b�l�v         TO �ߘa�b�l.
023070     MOVE �ߘa�`�F�b�N�v     TO �ߘa�`�F�b�N.
017390*     MOVE �����v              TO ���Ҙa��.
      */�����C��������/20190408
016510*     MOVE ���N�����Œ�v      TO ���N�����Œ�.
016520*     MOVE �����v              TO �󋋎Ҍ���   ����.
016530*     MOVE ���ҔN�v            TO �󋋎ҔN     ���ҔN.
016540*     MOVE NC"�N"              TO �󋋎ҔN�Œ� ���ҔN�Œ�.
016550*     MOVE ���Ҍ��v            TO �󋋎Ҍ�     ���Ҍ�.
016560*     MOVE NC"��"              TO �󋋎Ҍ��Œ� ���Ҍ��Œ�.
016570*     MOVE ���ғ��v            TO �󋋎ғ�     ���ғ�.
016580*     MOVE NC"��"              TO �󋋎ғ��Œ� ���ғ��Œ�.
016920     MOVE ���ҔN�v            TO ���ҔN.
016940     MOVE ���Ҍ��v            TO ���Ҍ�.
016960     MOVE ���ғ��v            TO ���ғ�.
      *
           IF ��Q�|������ی��Ҏ��� NOT = SPACE
016940        MOVE ��Q�|������ی��Ҏ��� TO ��ی��Ҏ���
           END-IF.
016590*
016600*     MOVE ��������v          TO ����.
016610* 
016620*     MOVE NC"�i�Ɩ��ЊQ�E�ʋ΍ЊQ���͑�O�ҍs�׈ȊO�̌����ɂ��j" 
016630*                              TO ���������O.
016640     MOVE ���������v(1)       TO ���������P.
016650     MOVE ���������v(2)       TO ���������Q.
016660     MOVE ���������v(3)       TO ���������R.
016670     MOVE ���������v(4)       TO ���������S.
016680     MOVE ���������v(5)       TO ���������T.
016680     MOVE ���������v(6)       TO ���������U.
016690*
016700*     MOVE ������v            TO ������.
016710*
016850********************
016860* �����f�[�^�Z�b�g *
016870********************
016880* �P���� *
016890**********
016900     MOVE �������v(1)       TO �������P.
016910     MOVE �����N�v(1)       TO �����N�P.
016920     MOVE �������v(1)       TO �������P.
016930     MOVE �������v(1)       TO �������P.
016940     MOVE �����N�v(1)       TO �����N�P.
016950     MOVE �������v(1)       TO �������P.
016960     MOVE �������v(1)       TO �������P.
016970     MOVE �J�n�N�v(1)       TO �J�n�N�P.
016980     MOVE �J�n���v(1)       TO �J�n���P.
016990     MOVE �J�n���v(1)       TO �J�n���P.
017000     MOVE �I���N�v(1)       TO �I���N�P.
017010     MOVE �I�����v(1)       TO �I�����P.
017020     MOVE �I�����v(1)       TO �I�����P.
017030     MOVE �������v(1)       TO �������P.
017040     MOVE �����`�F�b�N�v(1) TO �����`�F�b�N�P.
017050     MOVE ���~�`�F�b�N�v(1) TO ���~�`�F�b�N�P.
017060     MOVE �]��`�F�b�N�v(1) TO �]��`�F�b�N�P.
017070*
017080*     MOVE �]�A�v(1)         TO �]�A�P.
017090*     IF �����N�v(1) NOT = ZERO
017100*        MOVE "."            TO ��؂P�P ��؂P�Q
017110*     END-IF.
017120*     IF �����N�v(1) NOT = ZERO
017130*        MOVE "."            TO ��؂P�R ��؂P�S
017140*     END-IF.
017150*     IF �J�n�N�v(1) NOT = ZERO
017160*        MOVE "."            TO ��؂P�T ��؂P�U
017170*     END-IF.
017180*     IF �I���N�v(1) NOT = ZERO
017190*        MOVE "."            TO ��؂P�V ��؂P�W
017200*     END-IF.
017210**********
017220* �Q���� *
017230**********
017240     MOVE �������v(2)       TO �������Q.
017250     MOVE �����N�v(2)       TO �����N�Q.
017260     MOVE �������v(2)       TO �������Q.
017270     MOVE �������v(2)       TO �������Q.
017280     MOVE �����N�v(2)       TO �����N�Q.
017290     MOVE �������v(2)       TO �������Q.
017300     MOVE �������v(2)       TO �������Q.
017310     MOVE �J�n�N�v(2)       TO �J�n�N�Q.
017320     MOVE �J�n���v(2)       TO �J�n���Q.
017330     MOVE �J�n���v(2)       TO �J�n���Q.
017340     MOVE �I���N�v(2)       TO �I���N�Q.
017350     MOVE �I�����v(2)       TO �I�����Q.
017360     MOVE �I�����v(2)       TO �I�����Q.
017370     MOVE �������v(2)       TO �������Q.
017380     MOVE �����`�F�b�N�v(2) TO �����`�F�b�N�Q.
017390     MOVE ���~�`�F�b�N�v(2) TO ���~�`�F�b�N�Q.
017400     MOVE �]��`�F�b�N�v(2) TO �]��`�F�b�N�Q.
017410*     MOVE �]�A�v(2)         TO �]�A�Q.
017420*     IF �����N�v(2) NOT = ZERO
017430*        MOVE "."            TO ��؂Q�P ��؂Q�Q
017440*     END-IF.
017450*     IF �����N�v(2) NOT = ZERO
017460*        MOVE "."            TO ��؂Q�R ��؂Q�S
017470*     END-IF.
017480*     IF �J�n�N�v(2) NOT = ZERO
017490*        MOVE "."            TO ��؂Q�T ��؂Q�U
017500*     END-IF.
017510*     IF �I���N�v(2) NOT = ZERO
017520*        MOVE "."            TO ��؂Q�V ��؂Q�W
017530*     END-IF.
017540**********
017550* �R���� *
017560**********
017570     MOVE �������v(3)       TO �������R.
017580     MOVE �����N�v(3)       TO �����N�R.
017590     MOVE �������v(3)       TO �������R.
017600     MOVE �������v(3)       TO �������R.
017610     MOVE �����N�v(3)       TO �����N�R.
017620     MOVE �������v(3)       TO �������R.
017630     MOVE �������v(3)       TO �������R.
017640     MOVE �J�n�N�v(3)       TO �J�n�N�R.
017650     MOVE �J�n���v(3)       TO �J�n���R.
017660     MOVE �J�n���v(3)       TO �J�n���R.
017670     MOVE �I���N�v(3)       TO �I���N�R.
017680     MOVE �I�����v(3)       TO �I�����R.
017690     MOVE �I�����v(3)       TO �I�����R.
017700     MOVE �������v(3)       TO �������R.
017710     MOVE �����`�F�b�N�v(3) TO �����`�F�b�N�R.
017720     MOVE ���~�`�F�b�N�v(3) TO ���~�`�F�b�N�R.
017730     MOVE �]��`�F�b�N�v(3) TO �]��`�F�b�N�R.
017740*     MOVE �]�A�v(3)         TO �]�A�R.
017750*     IF �����N�v(3) NOT = ZERO
017760*        MOVE "."            TO ��؂R�P ��؂R�Q
017770*     END-IF.
017780*     IF �����N�v(3) NOT = ZERO
017790*        MOVE "."            TO ��؂R�R ��؂R�S
017800*     END-IF.
017810*     IF �J�n�N�v(3) NOT = ZERO
017820*        MOVE "."            TO ��؂R�T ��؂R�U
017830*     END-IF.
017840*     IF �I���N�v(3) NOT = ZERO
017850*        MOVE "."            TO ��؂R�V ��؂R�W
017860*     END-IF.
017870**********
017880* �S���� *
017890**********
017900     MOVE �������v(4)       TO �������S.
017910     MOVE �����N�v(4)       TO �����N�S.
017920     MOVE �������v(4)       TO �������S.
017930     MOVE �������v(4)       TO �������S.
017940     MOVE �����N�v(4)       TO �����N�S.
017950     MOVE �������v(4)       TO �������S.
017960     MOVE �������v(4)       TO �������S.
017970     MOVE �J�n�N�v(4)       TO �J�n�N�S.
017980     MOVE �J�n���v(4)       TO �J�n���S.
017990     MOVE �J�n���v(4)       TO �J�n���S.
018000     MOVE �I���N�v(4)       TO �I���N�S.
018010     MOVE �I�����v(4)       TO �I�����S.
018020     MOVE �I�����v(4)       TO �I�����S.
018030     MOVE �������v(4)       TO �������S.
018040     MOVE �����`�F�b�N�v(4) TO �����`�F�b�N�S.
018050     MOVE ���~�`�F�b�N�v(4) TO ���~�`�F�b�N�S.
018060     MOVE �]��`�F�b�N�v(4) TO �]��`�F�b�N�S.
018070*     MOVE �]�A�v(4)         TO �]�A�S.
018080*     IF �����N�v(4) NOT = ZERO
018090*        MOVE "."            TO ��؂S�P ��؂S�Q
018100*     END-IF.
018110*     IF �����N�v(4) NOT = ZERO
018120*        MOVE "."            TO ��؂S�R ��؂S�S
018130*     END-IF.
018140*     IF �J�n�N�v(4) NOT = ZERO
018150*        MOVE "."            TO ��؂S�T ��؂S�U
018160*     END-IF.
018170*     IF �I���N�v(4) NOT = ZERO
018180*        MOVE "."            TO ��؂S�V ��؂S�W
018190*     END-IF.
018200**********
018210* �T���� *
018220**********
018230     MOVE �������v(5)       TO �������T.
018240     MOVE �����N�v(5)       TO �����N�T.
018250     MOVE �������v(5)       TO �������T.
018260     MOVE �������v(5)       TO �������T.
018270     MOVE �����N�v(5)       TO �����N�T.
018280     MOVE �������v(5)       TO �������T.
018290     MOVE �������v(5)       TO �������T.
018300     MOVE �J�n�N�v(5)       TO �J�n�N�T.
018310     MOVE �J�n���v(5)       TO �J�n���T.
018320     MOVE �J�n���v(5)       TO �J�n���T.
018330     MOVE �I���N�v(5)       TO �I���N�T.
018340     MOVE �I�����v(5)       TO �I�����T.
018350     MOVE �I�����v(5)       TO �I�����T.
018360     MOVE �������v(5)       TO �������T.
018370     MOVE �����`�F�b�N�v(5) TO �����`�F�b�N�T.
018380     MOVE ���~�`�F�b�N�v(5) TO ���~�`�F�b�N�T.
018390     MOVE �]��`�F�b�N�v(5) TO �]��`�F�b�N�T.
018400*     MOVE �]�A�v(5)         TO �]�A�T.
018410*     IF �����N�v(5) NOT = ZERO
018420*        MOVE "."            TO ��؂T�P ��؂T�Q
018430*     END-IF.
018440*     IF �����N�v(5) NOT = ZERO
018450*        MOVE "."            TO ��؂T�R ��؂T�S
018460*     END-IF.
018470*     IF �J�n�N�v(5) NOT = ZERO
018480*        MOVE "."            TO ��؂T�T ��؂T�U
018490*     END-IF.
018500*     IF �I���N�v(5) NOT = ZERO
018510*        MOVE "."            TO ��؂T�V ��؂T�W
018520*     END-IF.
018530**************
018540* �o�߃Z�b�g *
018550**************
018560     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018570***             UNTIL ( ���ʂb�m�s > ���ʐ��v )
018580             UNTIL ( ���ʂb�m�s > 5 )
018590**         MOVE ���ʂb�m�s�v(���ʂb�m�s)   TO �o�ߕ��ʂb�m�s(���ʂb�m�s)
018600**         MOVE ���ʋ�؂v(���ʂb�m�s)     TO ���ʋ��(���ʂb�m�s)
018610         MOVE ����o�ߗ��̂v(���ʂb�m�s) TO �o�ߗ���(���ʂb�m�s)
018620     END-PERFORM.
018630*****************************************
018640*     �V�K�E�p���`�F�b�N�ɂ���        *
018650*   ���V�K...�����L�� ���p��...�����Ȃ� *
018660*****************************************
018670     MOVE �V�K�`�F�b�N�v    TO �V�K�`�F�b�N.
018680     MOVE �p���`�F�b�N�v    TO �p���`�F�b�N.
018690*     MOVE �����敪�v        TO  �����敪.
018700********************
018710* �����f�[�^�Z�b�g *
018720********************
018730*    ****************************************************************
018740*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
018750*    ****************************************************************
018760     MOVE �������v�q                   TO  ������.
018770     MOVE ���ԊO�`�F�b�N�v             TO  ���ԊO�`�F�b�N.
018780     MOVE �x���`�F�b�N�v               TO  �x���`�F�b�N.
018790     MOVE �[��`�F�b�N�v               TO  �[��`�F�b�N.
018800*     MOVE �������Z���e�v               TO  �������Z���e.
018810     MOVE �������Z���v�q               TO  �������Z��.
           MOVE ���������k���v�q             TO  ���������k��.
           IF (���ԊO�`�F�b�N�v NOT = SPACE) OR (�[��`�F�b�N�v NOT = SPACE) OR
              (�x���`�F�b�N�v NOT = SPACE)
              MOVE �������Z���v                 TO  �������Z��
              MOVE �������Z��؂v               TO  �������Z���
              MOVE �������Z���v                 TO  �������Z��
           END-IF.
019150*     END-IF.
018820     MOVE �Č����v�q                   TO  �Č���.
018830     MOVE ���Ë����v�q                 TO  ���Ë���.
018840     MOVE ���É񐔂v�q                 TO  ���É�.
018850     MOVE ���×��v�q                   TO  ���×�.
018860     MOVE ��ԃ`�F�b�N�v               TO  ��ԃ`�F�b�N.
018870     MOVE ��H�`�F�b�N�v               TO  ��H�`�F�b�N.
018880*     MOVE ���Ð[��`�F�b�N�v           TO  ���Ð[��`�F�b�N.
018890     MOVE �\���J��`�F�b�N�v           TO  �\���J��`�F�b�N.
018900     MOVE ���É��Z���v�q               TO  ���É��Z��.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
           IF ( �{�p�a��N���v�q < 43006 )
018050        MOVE ��`�F�b�N�v              TO  ��`�F�b�N
018060        MOVE ���`�F�b�N�v              TO  ���`�F�b�N
018070        MOVE ���`�F�b�N�v              TO  ���`�F�b�N
           ELSE
              MOVE ALL NC"��"                TO  ��������
           END-IF.
      *     IF ( �{�p�a��N���v�q >= 43006 ) AND ( �������q���Z���v�q NOT = ZERO )
      *        MOVE �����񐔂v                TO  ������
      *        MOVE NC"��"                    TO  ������
      *     END-IF.
018940*     MOVE ������v                     TO  ������.
018950*     MOVE �������v                     TO  ������.
018960*     MOVE �������v                     TO  ������.
018970     MOVE �������q���Z���v�q           TO  �������q���Z��.
018980     MOVE �{�p���񋟗��v�q           TO  �{�p���񋟗�.
018990     MOVE ���v�v                       TO  ���v.
019000********************
019010* ���񏈒u���Z�b�g *
019020********************
019030     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
019040***             UNTIL ( ���ʂb�m�s > ���ʐ��v )
019050             UNTIL ( ���ʂb�m�s > 5 )
019060         MOVE ���񏈒u���v�q(���ʂb�m�s) TO ���񏈒u��(���ʂb�m�s)
019070     END-PERFORM.
019080     MOVE ���񏈒u�����v�v         TO ���񏈒u�����v.
019090*
019100     MOVE �{�×��`�F�b�N�v            TO �{�×��`�F�b�N.
019110     MOVE �������`�F�b�N�v            TO �������`�F�b�N.
019120     MOVE �Œ藿�`�F�b�N�v            TO �Œ藿�`�F�b�N.
019130******************
019140* ��{�����Z�b�g *
019150******************
019160*     MOVE ��㪖@�P���v                TO  ��㪖@�P��.
019170*     MOVE ��㪖@�P���v                TO  ��㪖@�P��.
019180*     MOVE �d�ÒP���v                  TO  �d�ÒP��.
019190********************
019200* �����������Z�b�g *
019210********************
019220*    **********
019230*    * �P���� *
019240*    **********
019250     MOVE ��ÒP���P�v�q             TO ��ÒP���P.
019260     MOVE ��É񐔂P�v�q             TO ��É񐔂P.
019270     MOVE ��×��P�v�q               TO ��×��P.
019280     MOVE ��㪖@�񐔂P�v�q           TO ��㪖@�񐔂P.
019290     MOVE ��㪖@���P�v�q             TO ��㪖@���P.
019300     MOVE ��㪖@�񐔂P�v�q           TO ��㪖@�񐔂P.
019310     MOVE ��㪖@���P�v�q             TO ��㪖@���P.
019320     MOVE �d�É񐔂P�v�q             TO �d�É񐔂P.
019330     MOVE �d�×��P�v�q               TO �d�×��P.
019340     MOVE ���v�P�v�q                 TO ���v�P.
019350     IF �����������P�v�q NOT = ZERO
019360         COMPUTE �����������P = �����������P�v�q / 100
019370     END-IF.
019380     MOVE ���������v�P�v�q           TO ���������v�P.
019390*    **********
019400*    * �Q���� *
019410*    **********
019420     MOVE ��ÒP���Q�v�q             TO ��ÒP���Q.
019430     MOVE ��É񐔂Q�v�q             TO ��É񐔂Q.
019440     MOVE ��×��Q�v�q               TO ��×��Q.
019450     MOVE ��㪖@�񐔂Q�v�q           TO ��㪖@�񐔂Q.
019460     MOVE ��㪖@���Q�v�q             TO ��㪖@���Q.
019470     MOVE ��㪖@�񐔂Q�v�q           TO ��㪖@�񐔂Q.
019480     MOVE ��㪖@���Q�v�q             TO ��㪖@���Q.
019490     MOVE �d�É񐔂Q�v�q             TO �d�É񐔂Q.
019500     MOVE �d�×��Q�v�q               TO �d�×��Q.
019510     MOVE ���v�Q�v�q                 TO ���v�Q.
019520     IF �����������Q�v�q NOT = ZERO
019530         COMPUTE �����������Q = �����������Q�v�q / 100
019540     END-IF.
019550     MOVE ���������v�Q�v�q           TO ���������v�Q.
019560*    ****************
019570*    * �R���ʁ^�W�� *
019580*    ****************
019590     MOVE ��ÒP���R�W�v�q             TO ��ÒP���R�W.
019600     MOVE ��É񐔂R�W�v�q             TO ��É񐔂R�W.
019610     MOVE ��×��R�W�v�q               TO ��×��R�W.
019620     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
019630     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
019640     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
019650     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
019660     MOVE �d�É񐔂R�W�v�q             TO �d�É񐔂R�W.
019670     MOVE �d�×��R�W�v�q               TO �d�×��R�W.
019680     MOVE ���v�R�W�v�q                 TO ���v�R�W.
019690     MOVE �����ʍ����v�R�W�v�q         TO �����ʍ����v�R�W.
019700     IF �����������R�W�v�q NOT = ZERO
019710         COMPUTE �����������R�W = �����������R�W�v�q / 100
019720     END-IF.
019730     MOVE ���������v�R�W�v�q           TO ���������v�R�W.
      */ ������ 0.7��0.6 /42505
           IF (�{�p�a��N���v�q >= 42505)
              MOVE "60"                      TO �����R�W
              MOVE "0.6"                     TO �����ʂR�W
      *        MOVE "==="                     TO ���������R�W �����ʒ����R�W
           END-IF.
019740*    ****************
019750*    * �R���ʁ^10�� *
019760*    ****************
019770     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
019780     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
019790     MOVE ��ÒP���R�O�v�q             TO ��ÒP���R�O.
019800     MOVE ��É񐔂R�O�v�q             TO ��É񐔂R�O.
019810     MOVE ��×��R�O�v�q               TO ��×��R�O.
019820     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
019830     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
019840     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
019850     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
019860     MOVE �d�É񐔂R�O�v�q             TO �d�É񐔂R�O.
019870     MOVE �d�×��R�O�v�q               TO �d�×��R�O.
019880     MOVE ���v�R�O�v�q                 TO ���v�R�O.
019890     IF �����������R�O�v�q NOT = ZERO
019900         COMPUTE �����������R�O = �����������R�O�v�q / 100
019910     END-IF.
019920     MOVE ���������v�R�O�v�q           TO ���������v�R�O.
019930*    ****************
019940*    * �S���ʁ^�T�� *
019950*    ****************
019960*     MOVE ��ÒP���S�T�v�q             TO ��ÒP���S�T.
019970*     MOVE ��É񐔂S�T�v�q             TO ��É񐔂S�T.
019980*     MOVE ��×��S�T�v�q               TO ��×��S�T.
019990*     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
020000*     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
020010*     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
020020*     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
020030*     MOVE �d�É񐔂S�T�v�q             TO �d�É񐔂S�T.
020040*     MOVE �d�×��S�T�v�q               TO �d�×��S�T.
020050*     MOVE ���v�S�T�v�q                 TO ���v�S�T.
020060*     MOVE �����ʍ����v�S�T�v�q         TO �����ʍ����v�S�T.
020070*     IF �����������S�T�v�q NOT = ZERO
020080*         COMPUTE �����������S�T = �����������S�T�v�q / 100
020090*     END-IF.
020100*     MOVE ���������v�S�T�v�q           TO ���������v�S�T.
020110*    ****************
020120*    * �S���ʁ^�W�� *
020130*    ****************
020140     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
020150     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
020160     MOVE ��ÒP���S�W�v�q             TO ��ÒP���S�W.
020170     MOVE ��É񐔂S�W�v�q             TO ��É񐔂S�W.
020180     MOVE ��×��S�W�v�q               TO ��×��S�W.
020190     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
020200     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
020210     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
020220     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
020230     MOVE �d�É񐔂S�W�v�q             TO �d�É񐔂S�W.
020240     MOVE �d�×��S�W�v�q               TO �d�×��S�W.
020250     MOVE ���v�S�W�v�q                 TO ���v�S�W.
020260     MOVE �����ʍ����v�S�W�v�q         TO �����ʍ����v�S�W.
020270     IF �����������S�W�v�q NOT = ZERO
020280         COMPUTE �����������S�W = �����������S�W�v�q / 100
020290     END-IF.
020300     MOVE ���������v�S�W�v�q           TO ���������v�S�W.
      */ ������ 0.7��0.6 /42505
           IF (�{�p�a��N���v�q >= 42505)
              MOVE "60"                      TO �����S�W
              MOVE "0.6"                     TO �����ʂS�W
      *        MOVE "==="                     TO ���������S�W �����ʒ����S�W
           END-IF.
020310*    ****************
020320*    * �S���ʁ^10�� *
020330*    ****************
020340     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
020350     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
020360     MOVE ��ÒP���S�O�v�q             TO ��ÒP���S�O.
020370     MOVE ��É񐔂S�O�v�q             TO ��É񐔂S�O.
020380     MOVE ��×��S�O�v�q               TO ��×��S�O.
020390     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
020400     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
020410     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
020420     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
020430     MOVE �d�É񐔂S�O�v�q             TO �d�É񐔂S�O.
020440     MOVE �d�×��S�O�v�q               TO �d�×��S�O.
020450     MOVE ���v�S�O�v�q                 TO ���v�S�O.
020460     IF �����������S�O�v�q NOT = ZERO
020470         COMPUTE �����������S�O = �����������S�O�v�q / 100
020480     END-IF.
020490     MOVE ���������v�S�O�v�q           TO ���������v�S�O.
020500*
020510*��***********************************************************************
020520* ���[��`�̕ύX�ɂ��A�T���ʂ̈󎚂͕K�v�Ȃ��B
020530*------------------------------------------------------------------------*
020540*    *****************
020550*    * �T���ʁ^2.5�� *
020560*    *****************
020570*     MOVE ��ÒP���T�Q�v�q             TO ��ÒP���T�Q.
020580*     MOVE ��É񐔂T�Q�v�q             TO ��É񐔂T�Q.
020590*     MOVE ��×��T�Q�v�q               TO ��×��T�Q.
020600*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
020610*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
020620*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
020630*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
020640*     MOVE �d�É񐔂T�Q�v�q             TO �d�É񐔂T�Q.
020650*     MOVE �d�×��T�Q�v�q               TO �d�×��T�Q.
020660*     MOVE ���v�T�Q�v�q                 TO ���v�T�Q.
020670*     MOVE �����ʍ����v�T�Q�v�q         TO �����ʍ����v�T�Q.
020680*     IF �����������T�Q�v�q NOT = ZERO
020690*         COMPUTE �����������T�Q = �����������T�Q�v�q / 100
020700*     END-IF.
020710*     MOVE ���������v�T�Q�v�q           TO ���������v�T�Q.
020720*    ****************
020730*    * �T���ʁ^�T�� *
020740*    ****************
020750*     MOVE �����J�n���T�T�v�q           TO �����J�n���T�T.
020760*     MOVE �����J�n���T�T�v�q           TO �����J�n���T�T.
020770*     MOVE ��ÒP���T�T�v�q             TO ��ÒP���T�T.
020780*     MOVE ��É񐔂T�T�v�q             TO ��É񐔂T�T.
020790*     MOVE ��×��T�T�v�q               TO ��×��T�T.
020800*     MOVE ��㪖@�񐔂T�T�v�q           TO ��㪖@�񐔂T�T.
020810*     MOVE ��㪖@���T�T�v�q             TO ��㪖@���T�T.
020820*     MOVE ��㪖@�񐔂T�T�v�q           TO ��㪖@�񐔂T�T.
020830*     MOVE ��㪖@���T�T�v�q             TO ��㪖@���T�T.
020840*     MOVE �d�É񐔂T�T�v�q             TO �d�É񐔂T�T.
020850*     MOVE �d�×��T�T�v�q               TO �d�×��T�T.
020860*     MOVE ���v�T�T�v�q                 TO ���v�T�T.
020870*     MOVE �����ʍ����v�T�T�v�q         TO �����ʍ����v�T�T.
020880*     IF �����������T�T�v�q NOT = ZERO
020890*         COMPUTE �����������T�T = �����������T�T�v�q / 100
020900*     END-IF.
020910*     MOVE ���������v�T�T�v�q           TO ���������v�T�T.
020920*    ****************
020930*    * �T���ʁ^�W�� *
020940*    ****************
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
021120*    ****************
021130*    * �T���ʁ^10�� *
021140*    ****************
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
021320*
021330*------------------------------------------------------------------------------------*
021340** ���ʁi�T���ʖڂ̖��ׂ�����ꍇ�́A�K�p���ɃR�����g���󎚂���j
021350*     IF ( ���v�T�T�v�q NOT = ZERO ) OR
021360*        ( ���v�T�W�v�q NOT = ZERO ) OR
021370*        ( ���v�T�O�v�q NOT = ZERO )
021380*        MOVE  NC"���T���ʖڐ�������" TO ���ʂT�K�p
021390*     END-IF.
021400*------------------------------------------------------------------------------------*
021410*
021420     MOVE �K�p�P�v                     TO �K�p�P.
021430     MOVE �K�p�Q�v                     TO �K�p�Q.
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
              MOVE 37           TO �A���^�|��R�[�h
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
021440     MOVE ���Z�|���v                   TO ���v.
021450*     MOVE ���Z�|�ꕔ���S��             TO �ꕔ���S��.
021460     MOVE ���Z�|�������z               TO �������z.
021470     MOVE ���Z�|�󋋎ҕ��S�z           TO �󋋎ҕ��S�z.
021480     MOVE ���Z�|�����������z           TO ���������z.
           MOVE NC"�������z"                 TO �������z�b�l.
           MOVE NC"�󋋎ҕ��S��"             TO �󋋎ҕ��S���b�l.
           MOVE NC"�������z"                 TO �������z�b�l.
021490*
021500**********************
021510* ���t�����Z�b�g *
021520**********************
021530     MOVE ���t�����v             TO ���t����.
021540*     MOVE ���S�����v             TO ���S����.
021550*     MOVE �����Œ�v             TO �����Œ�.
      *     MOVE ��󎚂v               TO ���.
      *
      */�s���{���ʏ���
           EVALUATE TRUE
      */�����s
           WHEN �s�����ԍ��v(3:2) = "13"
021370         MOVE ���Z�|�󋋎ҕ��S�z               TO �󋋎ҕ��S�z�Q
021380         MOVE ���Z�|�����������z               TO ���������z�Q
               MOVE "�ꕔ���S�������z�i��Ï�����j" TO �ꕔ���S���b�l
               MOVE "�������z�i��Ï�����j"         TO �������z�b�l�Q
               MOVE "�~"                             TO �ꕔ���S���~�b�l �������z�~�b�l
               MOVE NC"�ꕔ���S��"                   TO �����p�ꕔ���S���b�l
               MOVE NC"�i��Õی��j"                 TO �����p�ꕔ���S���b�l�Q �����p�󋋎ҕ��S���b�l�Q
               MOVE NC"�������z"                     TO �����p�󋋎ҕ��S���b�l
               MOVE SPACE                            TO ���t���� �������z�b�l �������z�b�l �󋋎ҕ��S���b�l
               MOVE ZERO                             TO ���������z
021450         MOVE ���Z�|�ꕔ���S��                 TO �������z
021460         MOVE ���Z�|�������z                   TO �󋋎ҕ��S�z
      */������
017080         MOVE ������v                         TO ������P ������Q
017090         IF ������v NOT = SPACE
017100            MOVE NC"��"                        TO ������Œ�P ������Œ�Q
017110         END-IF
      */�T���ʖڂ̋��z������ꍇ�͈󎚍s�����炷
               IF ���ʂT�O NOT = SPACE
                   IF ���ʂT�W NOT = SPACE
                       IF �������R���U NOT = SPACE
                           MOVE SPACE    TO �������R���U
                           MOVE ���ʂT�W TO ���ʂT�W�Q
                           MOVE ���ʂT�O TO ���ʂT�W
                           MOVE SPACE    TO ���ʂT�O
                       ELSE
                           MOVE ���ʂT�W TO ���ʂT�W�Q
                           MOVE ���ʂT�O TO ���ʂT�W
                           MOVE SPACE    TO ���ʂT�O
                       END-IF
                   ELSE
                       MOVE ���ʂT�O TO ���ʂT�W
                       MOVE SPACE    TO ���ʂT�O
                   END-IF
               END-IF
      */���m��/181204
           WHEN �s�����ԍ��v(3:2) = "23"
      */������
017080         MOVE ������v                         TO ������P ������Q
017090         IF ������v NOT = SPACE
017100            MOVE NC"��"                        TO ������Œ�P ������Œ�Q
017110         END-IF
           WHEN OTHER
�@             MOVE "X" TO EDIT-MODE OF �󋋎ҕ��S�z�Q EDIT-MODE OF ���������z�Q
           END-EVALUATE.
      *
      */���{���̏����͍��v�A�ꕔ���S���A�������z�̂R���L�ڂ���B
      */�{�̂̐����z�̗��ɏ������S�z��]�L���S�s�ڂ��󔒂ɂ���B
           IF �s�����ԍ��v(3:2) = "27"
               MOVE NC"�ꕔ���S��"     TO ���p�ꕔ���S���b�l
               MOVE NC"�������z"       TO �󋋎ҕ��S���b�l
               MOVE SPACE              TO ���t���� �������z�b�l �������z�b�l
               MOVE ZERO               TO ���������z
               MOVE ���Z�|�󋋎ҕ��S�z TO �������z
               MOVE ���Z�|�����������z TO �󋋎ҕ��S�z
           END-IF.
021560*
021570**********************
021580* �{�p���f�[�^�Z�b�g *
021590**********************
021600     MOVE �_���t�ԍ��v           TO �_���t�ԍ�.
021610*     MOVE ��z���󗝔ԍ��v       TO ��z���󗝔ԍ�.
021620     MOVE �{�p���X�֔ԍ��P�v     TO �{�p���X�֔ԍ��P.
021630     MOVE �{�p���X�֔ԍ��Q�v     TO �{�p���X�֔ԍ��Q.
021640*
021650     MOVE �{�p���Z���P�v         TO �{�p���Z���P.
021660     MOVE �{�p���Z���Q�v         TO �{�p���Z���Q.
021670*     MOVE �{�p���Z���R�v         TO �{�p���Z���R.
021680*
021690     MOVE ����ڍ��t�����ԍ��v TO �ڍ��t�����ԍ�.
021700     MOVE ��\�҃J�i�v           TO ��\�҃J�i.
021710     MOVE ��\�Җ��v             TO ��\�Җ�.
021720     MOVE �{�p���d�b�ԍ��v       TO �{�p���d�b�ԍ�.
021730     MOVE �ڍ��@���v             TO �ڍ��@��.
           MOVE �s���{���i�h�r�v       TO �s���{���ԍ�.
021740*
021750*     MOVE ��s���x�X���v         TO ��s���x�X��.
021760*     MOVE �a����ʃR�����g�v     TO �a�����.
021770     MOVE �����ԍ��v             TO �����ԍ�.
021780     MOVE �������`�l�J�i�v       TO �������`�l�J�i.
021790     MOVE �������`�l�v           TO �������`�l.
           MOVE ���Z�@�֖��P�v   TO ���Z�@�֖��P.
           MOVE ���Z�@�֖��Q�v   TO ���Z�@�֖��Q.
           MOVE ���Z�@�֖��R�v   TO ���Z�@�֖��R.
           MOVE ���Z�@�֖��S�v   TO ���Z�@�֖��S.
           MOVE �x�X���P�v       TO �x�X���P.
           MOVE �x�X���Q�v       TO �x�X���Q.
           MOVE �x�X���R�v       TO �x�X���R.
           MOVE �x�X���S�v       TO �x�X���S.
      *     MOVE �U���`�F�b�N�v   TO �U���`�F�b�N.
      *     MOVE ���ʃ`�F�b�N�v   TO ���ʃ`�F�b�N.
      *     MOVE �����`�F�b�N�v   TO �����`�F�b�N.
      *     MOVE ��s�`�F�b�N�v   TO ��s�`�F�b�N.
      *     MOVE ���Ƀ`�F�b�N�v   TO ���Ƀ`�F�b�N.
      *     MOVE �_���`�F�b�N�v   TO �_���`�F�b�N.
      *     MOVE �{�X�`�F�b�N�v   TO �{�X�`�F�b�N.
      *     MOVE �x�X�`�F�b�N�v   TO �x�X�`�F�b�N.
      *     MOVE �{�x���`�F�b�N�v TO �{�x���`�F�b�N.
021800     MOVE ��ϔC���P�v         TO ��ϔC���P.
021810     MOVE ��ϔC���Q�v         TO ��ϔC���Q.
021810     MOVE ��ϔC���R�v         TO ��ϔC���R.
021811*
021812*     MOVE NC"�ĈϔC�͏������܂��B" TO �ϔC���⑫.
021820*
021830****
021840*
021850* / �_���t�E���҈ϔC�� /
      */�����C��/������20190408
037370     IF �{�p�a��v > 4
               MOVE �{�p�a��v         TO ���|�����敪
037380         READ �����}�X�^
037390         NOT INVALID KEY
037400             MOVE ���|��������   TO �󗝘a��
037410         END-READ
               MOVE "===="             TO �󗝘a�����
           END-IF.
      */�����C��/������20190408
021860     MOVE �_���t�N�v             TO �󗝔N.
021870     MOVE �_���t���v             TO �󗝌�.
021880     MOVE �_���t���v             TO �󗝓�.
021890* ( �ϔC�N���� ������邩 )
021900     IF �A���|�ϔC���  = ZERO
      */�����C��/������20190408
037370         IF �{�p�a��v > 4
                   MOVE �{�p�a��v         TO ���|�����敪
037380             READ �����}�X�^
037390             NOT INVALID KEY
037400                 MOVE ���|��������   TO �ϔC�a��
037410             END-READ
                   MOVE "===="             TO �ϔC�a�����
               END-IF
      */�����C��/������20190408
021910        MOVE ���҈ϔC�N�v       TO �ϔC�N
021920        MOVE ���҈ϔC���v       TO �ϔC��
021930        MOVE ���҈ϔC���v       TO �ϔC��
021940     END-IF.
021950*
021960*
021970********************
021980* ���O�f�[�^�Z�b�g *
021990********************
022000     MOVE ���Ҏ����v          TO ��f�Ҏ���.
022010     STRING ���Z�Ǘ�����v       DELIMITED BY SPACE
022020            "-"                  DELIMITED BY SIZE
022030            �{�p���v             DELIMITED BY SPACE
022040            "-"                  DELIMITED BY SIZE
022050            �ڍ��t�����ԍ��v   DELIMITED BY SPACE
022060            "-"                  DELIMITED BY SIZE
022070            ���҃R�[�h�v�q       DELIMITED BY SPACE
022080            INTO ���Z�v�g�Ǘ��ԍ�
022090     END-STRING.
022100     MOVE �����\���Ԃv        TO �����\����.
022670*
      *     IF �������Z�v NOT = 1
      *         MOVE ALL NC"��"      TO ������P ������Q
      *         MOVE "��510-0075 �O�d���l���s�s�����P����4-16 �j�`�m�d�m�h�r���V�e�@TEL  059-359-0333  FAX  059-359-0335"
      *                              TO ��Z��
      *     END-IF.
022110*
022120* �{�pID
022130     MOVE �����όŒ�v        TO �����όŒ�.
022140     MOVE ���{�p�h�c�v        TO ���{�p�h�c.
022150*
022151*
022152* �����s�@�E��Ɂu�O�v�󎚁i����ҁj 14/10�`
022153     MOVE ���ʃ}�[�N�v           TO ���ʃ}�[�N.
022154*
022155* ���m���@���ʃR�����g�i�S�P�V�j14/10�`
022156*     MOVE ���ʃR�����g�v         TO ���ʃR�����g.
022157*
022158*
022160*****     PERFORM �e�X�g�󎚏���.
022170*
022172*-------------------------------------------------------------------------*
022173*--- �� ���Z�E�v�ăZ�b�g�́A���̈���Z�b�gSECTION �̍Ō�ɂ�邱�ƁI -----*
022174     PERFORM ���Z�E�v�ăZ�b�g.
022175*-------------------------------------------------------------------------*
022176*
022180*================================================================*
022190 ���ڏ����� SECTION.
022200*
022210     INITIALIZE �{�p�����v.
022220     INITIALIZE ��f�ҏ��v.
022230     INITIALIZE �������v.
022240     INITIALIZE ���l���v.
022250     INITIALIZE �����P�v�q.
022260     INITIALIZE �����Q�v�q.
022270     INITIALIZE �����R�v�q.
022290     INITIALIZE YHP6425P.
022280     MOVE SPACE TO YHP6425P.
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
022300*================================================================*
022310 �{�p�����擾 SECTION.
022320*
022330**************************************************
022340* �{�@�f�[�^���g�p���A�ȉ��̏����擾           *
022350* �� �_���t�ԍ�.. �_���t�ԍ��v�Ɋi�[             *
022360* �� ����ԍ� ... �ڍ��t�����ԍ��v�Ɋi�[       *
022370* �� ��\�Җ� ... ��\�Җ��v�Ɋi�[               *
022380* �� �Z��1,2   ...�{�p���Z��1,2�v�Ɋi�[          *
022390* �� �d�b�ԍ� ... �{�p���d�b�ԍ��v�Ɋi�[         *
022400**************************************************
022410     MOVE ZERO  TO �{��|�{�p���ԍ�.
022420     READ �{�p�����}�X�^
022430     INVALID KEY
022440         CONTINUE
022450     NOT INVALID KEY
022490         MOVE �{��|�V�_���t�ԍ�   TO �_���t�ԍ��v
022510*
022520*         MOVE   "�����ЁE"             TO �ڍ��t��v
022530         MOVE   �{��|�ڍ��t�����ԍ� TO �ڍ��t�����ԍ��v
022540*
022550         MOVE �{��|�X�֔ԍ��P        TO �{�p���X�֔ԍ��P�v
022560         MOVE �{��|�X�֔ԍ��Q        TO �{�p���X�֔ԍ��Q�v
022570         MOVE �{��|��\�҃J�i        TO ��\�҃J�i�v
022580         MOVE �{��|��\�Җ�          TO ��\�Җ��v
022590         MOVE �{��|�ڍ��@��          TO �ڍ��@���v
               MOVE �{��|�s���{���i�h�r    TO �s���{���i�h�r�v
022600*
022610         MOVE �{��|�Z���P            TO �{�p���Z���P�v
022620         MOVE �{��|�Z���Q            TO �{�p���Z���Q�v
022630*         STRING �{��|�Z���P     DELIMITED BY SPACE
022640*                �{��|�Z���Q     DELIMITED BY SPACE
022650*                INTO �{�p���Z���v
022660*         END-STRING
022670*
022680         MOVE �{��|�d�b�ԍ�          TO �{�p���d�b�ԍ��v
022690** �U������
022700*         MOVE �{��|������s��      TO ������s���v
022710*         MOVE �{��|������s�x�X��  TO ������s�x�X���v
022720*         MOVE �{��|�a�����          TO �a����ʂv
022730*         MOVE �{��|�����ԍ�          TO �����ԍ��v
022740*         MOVE �{��|�������`�l        TO �������`�l�v
022750*         MOVE �{��|�������`�l�J�i    TO �������`�l�J�i�v
022760*         STRING ������s���v     DELIMITED BY SPACE
022770*                " "                DELIMITED BY SIZE
022780*                ������s�x�X���v DELIMITED BY SPACE
022790*                INTO ��s���x�X���v
022800*         END-STRING
022810** �U������  / ����}�X�^���U��������擾 /
023520         MOVE ZERO  TO  ���|�_���I���敪
022820         MOVE 37    TO  ���|����R�[�h
022830         MOVE ZERO  TO  ���|�ی����
023530         MOVE ZERO  TO  ���|�ύX�a��N��
022850         READ ����}�X�^
022860         NOT INVALID KEY
022870             MOVE ���|������s��      TO ������s���v
022880             MOVE ���|������s�x�X��  TO ������s�x�X���v
022890             MOVE ���|�a�����          TO �a����ʂv
022900             MOVE ���|�����ԍ�          TO �����ԍ��v
022910             MOVE ���|�������`�l        TO �������`�l�v
022920             MOVE ���|�������`�l�J�i    TO �������`�l�J�i�v
022930*
022940             STRING ������s���v     DELIMITED BY SPACE
022950                    " "                DELIMITED BY SIZE
022960                    ������s�x�X���v DELIMITED BY SPACE
022970                    INTO ��s���x�X���v
022980             END-STRING
022990             EVALUATE �a����ʂv
023000             WHEN 1
023010                 MOVE NC"�i���ʁj" TO �a����ʃR�����g�v
023020             WHEN 2
023030                 MOVE NC"�i�����j" TO �a����ʃR�����g�v
023040             WHEN OTHER
023050                 MOVE SPACE        TO �a����ʃR�����g�v
023060             END-EVALUATE
023070*
023080*             MOVE ���|�ڍ��t����    TO ����v
023090         END-READ
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
               MOVE ZERO  TO �J�E���^
               MOVE ������s���v TO ���Z�@�֖��v
               INSPECT ������s���v TALLYING �J�E���^ FOR ALL "��s"
               IF ( �J�E���^ >= 1 )
                   MOVE NC"��" TO ��s�`�F�b�N�v
                   MOVE SPACE  TO ���Z�@�֖��v
                   UNSTRING ������s���v DELIMITED BY "��s"
                       INTO ���Z�@�֖��v
                   END-UNSTRING
               END-IF
               MOVE ZERO TO �J�E���^
               INSPECT ������s���v TALLYING �J�E���^ FOR ALL "����"
               IF ( �J�E���^ >= 1 )
                   MOVE NC"��" TO ���Ƀ`�F�b�N�v
                   MOVE SPACE  TO ���Z�@�֖��v
                   UNSTRING ������s���v DELIMITED BY "����"
                       INTO ���Z�@�֖��v
                   END-UNSTRING
               END-IF
               MOVE ZERO TO �J�E���^
               INSPECT ������s���v TALLYING �J�E���^ FOR ALL "�_��"
               IF ( �J�E���^ >= 1 )
                   MOVE NC"��" TO �_���`�F�b�N�v
                   MOVE SPACE  TO ���Z�@�֖��v
                   UNSTRING ������s���v DELIMITED BY "�_��"
                       INTO ���Z�@�֖��v
                   END-UNSTRING
               END-IF
      *
               MOVE ������s�x�X���v TO �x�X���v
               MOVE ZERO TO �J�E���^
               INSPECT ������s�x�X���v TALLYING �J�E���^ FOR ALL "�{�X"
               IF ( �J�E���^ >= 1 )
                   MOVE NC"��" TO �{�X�`�F�b�N�v
                   MOVE SPACE  TO �x�X���v
                   UNSTRING ������s�x�X���v DELIMITED BY "�{�X"
                       INTO �x�X���v
                   END-UNSTRING
               END-IF
               MOVE ZERO TO �J�E���^
               INSPECT ������s�x�X���v TALLYING �J�E���^ FOR ALL "�x�X"
               IF ( �J�E���^ >= 1 )
                   MOVE NC"��" TO �x�X�`�F�b�N�v
                   MOVE SPACE  TO �x�X���v
                   UNSTRING ������s�x�X���v DELIMITED BY "�x�X"
                       INTO �x�X���v
                   END-UNSTRING
               END-IF
               MOVE ZERO TO �J�E���^
               INSPECT ������s�x�X���v TALLYING �J�E���^ FOR ALL "�{��"
               IF ( �J�E���^ >= 1 )
                   MOVE NC"��" TO �{�x���`�F�b�N�v
                   MOVE SPACE  TO �x�X���v
                   UNSTRING ������s�x�X���v DELIMITED BY "�{��"
                       INTO �x�X���v
                   END-UNSTRING
               END-IF
               MOVE ZERO TO �J�E���^
               INSPECT ������s�x�X���v TALLYING �J�E���^ FOR ALL "�x��"
               IF ( �J�E���^ >= 1 )
                   MOVE NC"��" TO �{�x���`�F�b�N�v
                   MOVE SPACE  TO �x�X���v
                   UNSTRING ������s�x�X���v DELIMITED BY "�x��"
                       INTO �x�X���v
                   END-UNSTRING
               END-IF
023100*
023110     END-READ.
023120*
023130* �Œ��
023140*     MOVE NC"�×{��x�����z�̎�̂������Аڍ��t��" TO ��ϔC���P�v.
023150*     MOVE NC"�@��@"           TO ��ϔC���Q�P�v.
023160*     MOVE NC"�@�a�ɈϔC���܂��B" TO ��ϔC���Q�Q�v.
           MOVE "�܂��A�×{��̎�̂��@�z�[�v�ڍ��t��"      TO ��ϔC���P�v
           MOVE "� ���c���Y �a(�O�d���l���s�s����"       TO ��ϔC���Q�v
      */��Z���ύX/20190311
      *     MOVE "1����2-24 T�K����ިݸ�5�K)�ɈϔC���܂��B"  TO ��ϔC���R�v
           MOVE "1����6-14 ���E�e���r�� 1F�j�ɈϔC���܂��B" TO ��ϔC���R�v
      */����31�N4���������ȍ~��Z���ύX/20190311
           MOVE "�O�d���l���s�s����1����6-14 ���E�e���r�� 1F" TO ��Z��
           MOVE ALL "=" TO ���Z��������
023170*
023180*********************************************
023190** �h�c�Ǘ��}�X�^���@���{�p�h�c���擾����B
023200*********************************************
023210** ���{�pID
023220     MOVE 01                   TO �h�c�ǁ|�h�c�敪.
023230     MOVE ZERO                 TO �h�c�ǁ|�{�p���ԍ�.
023240     MOVE ��p���S�Ҕԍ������v�q(3:2)  TO �h�c�ǁ|�ی����.
023250     MOVE SPACE                TO �h�c�ǁ|�ی��Ҕԍ�.
023260     READ �h�c�Ǘ��}�X�^
023270     NOT INVALID KEY
023280*        MOVE NC"���ԍ��@�{�p�@�֔ԍ�"  TO �����όŒ�v
023290        STRING ��p���S�Ҕԍ������v�q(3:2) DELIMITED BY SPACE
023300                     "   "                 DELIMITED BY SIZE
023310                     �h�c�ǁ|�{�p�h�c�ԍ�  DELIMITED BY SPACE
023320                     INTO ���{�p�h�c�v
023330        END-STRING
023340     END-READ.
023350*
023360*================================================================*
023370 ��������擾 SECTION.
023380*
023390****************************************************
023400* �A���f�[�^����s�����}�X�^��萿������擾����B *
023410* ���s�|��������敪=1�̏ꍇ������}�X�^���g�p   *
023420* �� ������...... �����於�̂v�Ɋi�[               *
023421*
023422* 2001/10/26 �C���F����28�̂ݎx��������������
023423*
023430****************************************************
023440     MOVE ������ʂv�q           TO �s�|������.
023450     MOVE ��p���S�Ҕԍ������v�q TO �s�|�s�����ԍ�.
023460****     MOVE ����s�����ԍ��v       TO �s�|�s�����ԍ�.
023470*
023480     READ �s�����}�X�^
023490     INVALID KEY
023500         MOVE SPACE              TO �����於�̂s�a�k
023510         MOVE SPACE              TO �x���������v
023520     NOT INVALID KEY
023530         IF �s�|������敪 = 1
023540             MOVE ������ʂv�q           TO ����|�ی����
023550             MOVE ��p���S�Ҕԍ������v�q TO ����|�ی��Ҕԍ�
023560             READ ������}�X�^
023570             INVALID KEY
023580                 MOVE SPACE        TO �����於�̂s�a�k
023590                 MOVE SPACE        TO �x���������v
023600             NOT INVALID KEY
023610                 MOVE ����|�ی��Җ���  TO �����於�̂s�a�k
023620                 MOVE ����|�x��������  TO �x���������v
023621*
023630                 IF ��p���S�Ҕԍ������v�q(3:2) NOT = "28"
023640                    MOVE SPACE TO �x���������v
023650                 END-IF
023651*
023660             END-READ
023670         ELSE
023680             MOVE �s�|�s��������  TO �����於�̂s�a�k
023690             MOVE SPACE           TO �x���������v
023700         END-IF
023710     END-READ.
023720*
023730     IF �����於�̂s�a�k NOT = SPACE
023740        PERFORM VARYING �J�E���^ FROM 1 BY 1
023750                UNTIL ( �J�E���^ > 20 )  OR
023760                      ( �����於�̂v�s(�J�E���^) = SPACE )
023770           MOVE �����於�̂v�s(�J�E���^) TO �����於�̂v�s�P
023780        END-PERFORM
023790        IF �����於�̂v�s�P = "�s" OR "��" OR "��" OR "��"
023800           STRING �����於�̂s�a�k  DELIMITED BY SPACE
023810                  "��"              DELIMITED BY SIZE
023820                  �x���������v      DELIMITED BY SPACE
023830                  "�a"              DELIMITED BY SIZE
023840                  INTO �����於�̂v
023850           END-STRING
023860        ELSE
023870           STRING �����於�̂s�a�k  DELIMITED BY SPACE
023880                  "�@"              DELIMITED BY SIZE
023890                  �x���������v      DELIMITED BY SPACE
023900                  "�a"              DELIMITED BY SIZE
023910                  INTO �����於�̂v
023920           END-STRING
023930        END-IF
023940     END-IF.
023950*
023960*================================================================*
023970 ��f�ҏ��擾 SECTION.
023980*
023990**************************************************
024000* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
024010* �� �{�p�N ..... �{�p�N�v�Ɋi�[                 *
024020* �� �{�p�� ..... �{�p���v�Ɋi�[                 *
024030* �� ���Ҕԍ�.... ���Ҕԍ��v�Ɋi�[���e�c�A�ԗp   *
024040* �� �L�� ....... �L���v�Ɋi�[                   *
024050* �� �ԍ� ....... �ԍ��v�Ɋi�[                   *
024060* �� �ی��Ҕԍ� . �ی��Ҕԍ��v�Ɋi�[             *
024070* �� �ی���� ... �ی���ʂv�Ɋi�[               *
024080* �� ��ی��҃J�i.��ی��҃J�i�v�Ɋi�[           *
024090* �� ��ی��Ҏ���.��ی��Ҏ����v�Ɋi�[           *
024100* �� �Z���P ......��ی��ҏZ���P�v�Ɋi�[         *
024110* �� �Z���Q ......��ی��ҏZ���Q�v�Ɋi�[         *
024120* �� ���ҏZ���P ..���ҏZ���P�v�Ɋi�[             *
024130* �� ���ҏZ���Q ..���ҏZ���Q�v�Ɋi�[             *
024140* �� ���҃J�i ....���҃J�i�v�Ɋi�[               *
024150* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
024160* �� ���Ґ��� ....�敪�ɂ��`�F�b�N��"��"���i�[ *
024170* �� ���Ҙa�� ....�a��ɂ��`�F�b�N��"��"���i�[ *
024180* �� ���ҔN ......���ҔN�v�Ɋi�[                 *
024190* �� ���Ҍ� ......���Ҍ��v�Ɋi�[                 *
024200* �� ���ғ� ......���ғ��v�Ɋi�[                 *
024210* �� ���� ........���̃}�X�^��葱���v�Ɏ擾     *
024220**************************************************
           IF ��|���R�[�h NOT = SPACE
022660         EVALUATE ��|�ی����
022670         WHEN 01
022690            MOVE NC"��"        TO ���ۃ`�F�b�N�v
022680         WHEN 08
022690            MOVE NC"��"        TO �ސE�`�F�b�N�v
022700         WHEN 02
022710         WHEN 06
022750         WHEN 07
022720            MOVE NC"��"        TO ����`�F�b�N�v
022730         WHEN 03
022740            MOVE NC"��"        TO �g���`�F�b�N�v
               WHEN 04
               WHEN 09
                  MOVE NC"��"        TO ���σ`�F�b�N�v
               WHEN 05
                  MOVE NC"��"        TO ����`�F�b�N�v
022770         END-EVALUATE
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
024320*         EVALUATE ��|�{�p�a��
024330*         WHEN 1
024340*             MOVE NC"����"  TO �{�p�a��v
024350*         WHEN 2
024360*             MOVE NC"�吳"  TO �{�p�a��v
024370*         WHEN 3
024380*             MOVE NC"���a"  TO �{�p�a��v
024390*         WHEN 4
024400*             MOVE NC"����"  TO �{�p�a��v
024410*         END-EVALUATE
      */�����C��/20190408
               MOVE ��|�{�p�a��     TO �{�p�a��v
024420         MOVE ��|�{�p�N       TO �{�p�N�v
024430         MOVE ��|�{�p��       TO �{�p���v
024440         MOVE ��|���Ҕԍ�     TO ���Ҕԍ��v
024450*         MOVE ��|�L��         TO �L���v
024460*         MOVE ��|�ԍ�         TO �ԍ��v
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
024470         MOVE ��|�ی��Ҕԍ�   TO �ی��Ҕԍ��v
024480         MOVE ��|�ی����     TO �ی���ʂv
024490** �S���y�؂̎}�ԍ폜
024500         IF ( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(1:6) = "133033" )
024510            MOVE ��|�ی��Ҕԍ�(1:6)  TO �ی��Ҕԍ��v
024520         END-IF
024530**
024540         MOVE ��|��ی��҃J�i TO ��ی��҃J�i�v
024550         MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ����v
024560*         MOVE ��|�X�֔ԍ��P   TO �X�֔ԍ��P�v
024570*         MOVE ��|�X�֔ԍ��Q   TO �X�֔ԍ��Q�v
024580         MOVE ��|�Z���P       TO ��ی��ҏZ���P�v
024590         MOVE ��|�Z���Q       TO ��ی��ҏZ���Q�v
024560         MOVE ��|���җX�֔ԍ��P   TO �X�֔ԍ��P�v
024570         MOVE ��|���җX�֔ԍ��Q   TO �X�֔ԍ��Q�v
024600         MOVE ��|���ҏZ���P   TO ���ҏZ���P�v
024610         MOVE ��|���ҏZ���Q   TO ���ҏZ���Q�v
      */ �d�b�ԍ��ǉ� /42505
               IF ��|���ғd�b�ԍ� NOT = SPACE
                  STRING "�d�b:"            DELIMITED BY SIZE
                         ��|���ғd�b�ԍ�   DELIMITED BY SPACE
                    INTO �d�b�ԍ��v
                  END-STRING
               END-IF
024620         MOVE ��|���҃J�i     TO ���҃J�i�v
024630         MOVE ��|���Ҏ���     TO ���Ҏ����v
024640* �����p
024650         MOVE ��|��p���S�Ҕԍ����� TO �s�����ԍ��v
024660         MOVE ��|��v�Ҕԍ�����     TO �󋋎Ҕԍ��v
024670*
024680*
024690         EVALUATE ��|��ی��Ґ���
024700         WHEN 1
024710             MOVE NC"�j"  TO ��ی��Ґ��ʂv
024720         WHEN 2
024730             MOVE NC"��"  TO ��ی��Ґ��ʂv
024740         END-EVALUATE
024750*
024760         EVALUATE ��|���Ґ���
024770         WHEN 1
024780             MOVE "(�j)"  TO ���Ґ��ʂv
024790         WHEN 2
024800             MOVE "(��)"  TO ���Ґ��ʂv
024810         END-EVALUATE
024820         EVALUATE ��|���Ґ���
024830         WHEN 1
024840             MOVE NC"��"  TO �j�`�F�b�N�v
024850         WHEN 2
024860             MOVE NC"��"  TO ���`�F�b�N�v
024870         END-EVALUATE
024880
024890         EVALUATE ��|���Ҙa��
024900         WHEN 1
024910             MOVE NC"��"  TO �����`�F�b�N�v
024920         WHEN 2
024930             MOVE NC"��"  TO �吳�`�F�b�N�v
024940         WHEN 3
024950             MOVE NC"��"  TO ���a�`�F�b�N�v
024960         WHEN 4
024970             MOVE NC"��"  TO �����`�F�b�N�v
      */�����C��/20190408
023060         WHEN 5
                   MOVE "5��"   TO �ߘa�b�l�v
023070             MOVE NC"��"  TO �ߘa�`�F�b�N�v
024980         END-EVALUATE
024990         EVALUATE ��|���Ҙa��
025000         WHEN 1
025010             MOVE NC"����"  TO �����v
025020         WHEN 2
025030             MOVE NC"�吳"  TO �����v
025040         WHEN 3
025050             MOVE NC"���a"  TO �����v
025060         WHEN 4
025070             MOVE NC"����"  TO �����v
025080         END-EVALUATE
025090*
      */�����C��/������20190408
029310         IF ��|���Ҙa�� > 4
037370             MOVE ��|���Ҙa��     TO ���|�����敪
037380             READ �����}�X�^
037390             NOT INVALID KEY
037400                 MOVE ���|�������� TO �����v
037410             END-READ
029330         END-IF
      */�����C��/������20190408
025100         MOVE ��|���ҔN  TO ���ҔN�v
025110         MOVE ��|���Ҍ�  TO ���Ҍ��v
025120         MOVE ��|���ғ�  TO ���ғ��v
025130* ����
025140         EVALUATE �ی���ʂv�q 
025150* ���ہE�ސE
025160         WHEN 01
025170         WHEN 08
025180             IF �{�l�Ƒ��敪�v�q = 1
025190                 MOVE NC"�{�l"    TO �����v
025200             ELSE
025210                 MOVE NC"�\"      TO �����v
025220             END-IF
025230         WHEN OTHER
025240             IF �{�l�Ƒ��敪�v�q = 1
025250                  MOVE NC"�{�l"    TO �����v
025260             ELSE
025270                  MOVE 05          TO ���|�敪�R�[�h
025280                  MOVE ��|����    TO ���|���̃R�[�h
025290                  READ ���̃}�X�^
025300                  INVALID KEY
025310                      MOVE SPACE    TO �����v
025320                  NOT INVALID KEY
025330                      MOVE ���|���� TO �����v
025340                  END-READ
025350             END-IF
025360         END-EVALUATE
025370*
025380** �ی���ʃ`�F�b�N��ݒ�
025390         EVALUATE �ی���ʂv�q
025400         WHEN  01
025410             MOVE NC"����"   TO �ی���ʖ��̂v
025420         WHEN  02
025430         WHEN  06
025440         WHEN  07
025450             MOVE NC"����"   TO �ی���ʖ��̂v
025460         WHEN  03
025470             MOVE NC"�g��"   TO �ی���ʖ��̂v
025480         WHEN  04
025490             MOVE NC"����"   TO �ی���ʖ��̂v
025500         WHEN  08
025510             MOVE NC"�ސE"   TO �ی���ʖ��̂v
025520         WHEN  09
025530             MOVE NC"���q��" TO �ی���ʖ��̂v
025540         END-EVALUATE
025550         IF ( �����ʂv�q NOT = ZERO )  AND
025560            ( ������ʂv�q NOT = ZERO )
                   IF ��|�{�p�a��N�� < 42004
025570                 MOVE NC"�V�l" TO �ی���ʖ��̂v
                   ELSE
025570                 MOVE SPACE          TO �ی���ʖ��̂v
025570                 MOVE NC"��������" TO �ی���ʖ��̂Q�v
                   END-IF
025580         END-IF
025590*
025600         MOVE NC"���N����" TO ���N�����Œ�v
025610*
025611*
025612*---  �s�����Ǝ��d�l -----*
025613* 14/10�`�@�����s�̂݁� ���ʋ敪1,2,3(����ҁj�̎��A�u�O�v���E��Ɉ�
025614*                       �e���V�l�̎��A�ی��Ҕԍ����ɂ́A�Q�V�ԍ�����
025615         IF ��|�{�p�a��N�� >= 41410
025616            IF ��|��p���S�Ҕԍ�����(3:2) = "13"
025617               IF ��|������ = ZERO
025618                  IF ��|���ʋ敪 = 1 OR 2 OR 3
025619                     MOVE NC"�O" TO ���ʃ}�[�N�v
025620                  END-IF
025621               ELSE
025622                  MOVE ��|��p���S�Ҕԍ�  TO �ی��Ҕԍ��v
025623               END-IF
025624            END-IF
025625         END-IF
025626*
025627* 14/10�`�@���m���̂݁� 41�V�l�̕��S�����E��Ɉ�
025628         IF ��|�{�p�a��N�� >= 41410
025629            IF ( ��|��p���S�Ҕԍ�����(3:2) = "23" ) AND
025630               ( ��|������� = 51 )
025637               EVALUATE ��|�������S���Ə�
025638               WHEN 2
025639                  MOVE "41�V�l �Q��"   TO ���ʃR�����g�v
025640               WHEN 3
025641                  MOVE "41�V�l �R��"   TO ���ʃR�����g�v
025642               WHEN OTHER
025643                  MOVE "41�V�l �P��"   TO ���ʃR�����g�v
025644               END-EVALUATE
025646            END-IF
025647         END-IF
025648*
025649     END-IF.
025650*================================================================*
025651 �����f�[�^�擾 SECTION.
025652*
025660**************************************************
025670* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾 *
025680* �� ������...���ʁ{������ʂɂĉ��H���Ċi�[     *
025690* �� �����N.......�����N�v                       *
025700* �� ������.......�������v                       *
025710* �� ������.......�������v                       *
025720* �� �J�n�N.......�����N�v                       *
025730* �� �J�n��.......�������v                       *
025740* �� �J�n��.......�������v                       *
025750* �� �I���N.......�I���N�v                       *
025760* �� �I����.......�I�����v                       *
025770* �� �I����.......�I�����v                       *
025780* �� ������.......�������v                       *
025790* �� �]�A�敪 ....�敪�ɂ��`�F�b�N��"��"���i�[ *
025800* �� �������q ....�敪�ɂ��`�F�b�N��"��"���i�[ *
025810* �� �o�߃R�[�h...�o�߃}�X�^���擾             *
025820**************************************************
           IF ���|���R�[�h NOT = SPACE
025920         MOVE ���|���ʐ�                   TO ���ʐ��v
025930         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
025940                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
025950             MOVE ���|�������(���ʂb�m�s) TO ������ʂv(���ʂb�m�s)
025960             MOVE ���|����(���ʂb�m�s)     TO ���ʂv(���ʂb�m�s)
025970             MOVE ���|���E�敪(���ʂb�m�s) TO ���E�敪�v(���ʂb�m�s)
025980             MOVE ���|�����ʒu�ԍ�(���ʂb�m�s)
025990                                           TO �����ʒu�ԍ��v(���ʂb�m�s)
026000********************************************************
026010* ���j�S�_...���ʖ�1+������ʁ{���ʖ�2�ɂĉ��H���Ċi�[ *
026020********************************************************
026030* �������
026040             MOVE SPACE                     TO �������̂v
026050             MOVE 03                        TO ���|�敪�R�[�h
026060             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
026070             READ ���̃}�X�^
026080             INVALID KEY
026090                 MOVE SPACE        TO �������̂v
026100             NOT INVALID KEY
026110                 MOVE ���|�������� TO �������̂v
026120             END-READ
026130* ����
020710             MOVE SPACE                    TO �������v(���ʂb�m�s)
032680*
032690             PERFORM ���ʖ��̖�������
026320*
026330             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
026340             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
026350             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
026360             MOVE ���|�J�n�N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
026370             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
026380             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
026390             IF ���|�]�A�敪(���ʂb�m�s) = 9
026400                 MOVE 99                   TO �I���N�v(���ʂb�m�s)
026410                 MOVE 99                   TO �I�����v(���ʂb�m�s)
026420                 MOVE 99                   TO �I�����v(���ʂb�m�s)
026430             ELSE
026440                 MOVE ���|�I���N(���ʂb�m�s)   TO �I���N�v(���ʂb�m�s)
026450                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
026460                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
026470             END-IF
026480* �o�ߗ��̎擾
026490             MOVE 01                         TO �o�|�敪�R�[�h
026500             MOVE ���|�o�߃R�[�h(���ʂb�m�s) TO �o�|�o�߃R�[�h
026510             READ �o�߃}�X�^
026520             INVALID KEY
026530                 MOVE ZERO            TO ���ʂb�m�s�v(���ʂb�m�s)
026540                 MOVE SPACE           TO ���ʋ�؂v(���ʂb�m�s)
026550                 MOVE SPACE           TO �o�ߗ��̂v(���ʂb�m�s)
026560             NOT INVALID KEY
026570                 EVALUATE ���ʂb�m�s
026580                 WHEN 1
026590                     MOVE NC"�@" TO �o�ߕ��ʂv
026600                 WHEN 2
026610                     MOVE NC"�A" TO �o�ߕ��ʂv
026620                 WHEN 3
026630                     MOVE NC"�B" TO �o�ߕ��ʂv
026640                 WHEN 4
026650                     MOVE NC"�C" TO �o�ߕ��ʂv
026660                 WHEN 5
026670                     MOVE NC"�D" TO �o�ߕ��ʂv
026680                 END-EVALUATE
026690                 STRING  �o�ߕ��ʂv     DELIMITED BY SPACE
026700                         �o�|�o�ߗ���   DELIMITED BY SPACE
026710                        INTO ����o�ߗ��̂v(���ʂb�m�s)
026720                 END-STRING
026730             END-READ
026740*
026750             MOVE ���|�]�A�敪(���ʂb�m�s) TO �]�A�敪�v(���ʂb�m�s)
026760             EVALUATE ���|�]�A�敪(���ʂb�m�s)
026770             WHEN 1
026780             WHEN 2
026790                 MOVE NC"��"               TO �����`�F�b�N�v(���ʂb�m�s)
026800             WHEN 3
026810                 MOVE NC"��"               TO ���~�`�F�b�N�v(���ʂb�m�s)
026820             WHEN 4
026830                 MOVE NC"��"               TO �]��`�F�b�N�v(���ʂb�m�s)
026840             END-EVALUATE
026850*
026860*             EVALUATE ���|�]�A�敪(���ʂb�m�s)
026870*             WHEN 1
026880*             WHEN 2
026890*                 MOVE NC"����"               TO �]�A�v(���ʂb�m�s)
026900*             WHEN 3
026910*                 MOVE NC"���~"               TO �]�A�v(���ʂb�m�s)
026920*             WHEN 4
026930*                 MOVE NC"�]��"               TO �]�A�v(���ʂb�m�s)
026940*             WHEN OTHER
026950*                 MOVE NC"�p��"               TO �]�A�v(���ʂb�m�s)
026960*             END-EVALUATE
026970*
                   MOVE ���Z�|���ʎ�����(���ʂb�m�s) TO �������v(���ʂb�m�s)
026980         END-PERFORM
026990* �V�K/�p�� �`�F�b�N
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
027110* �}�Ԕ���p
027120         MOVE ���|�J�n�f�Ó��蓮�敪 TO  �J�n�f�Ó��蓮�敪�v
027130*
027131* ������������敪
027132         MOVE ���|���Z������������敪 TO ���Z������������敪�v
027880         MOVE ���|���Z�������R����敪 TO ���Z�������R����敪�v
027133*
027140     END-IF.
027150*================================================================*
030910 ���ʖ��̖������� SECTION.
030920*
006490     STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
009980            �������̂v                    DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
006520       INTO �������v(���ʂb�m�s)
006570     END-STRING.
027310*
027320*================================================================*
027330 �������擾 SECTION.
027340*
027350********************
027360* �����f�[�^�Z�b�g *
027370********************
027380*    ****************************************************************
027390*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
027400*    ****************************************************************
027410     MOVE ���Z�|������                 TO �������v�q.
027420     IF ���Z�|���ԊO = 1
027430         MOVE NC"��"                   TO ���ԊO�`�F�b�N�v
027440     END-IF.
027450     IF ���Z�|�x�� = 1
027460         MOVE NC"��"                   TO �x���`�F�b�N�v
027470     END-IF.
027480     IF ���Z�|�[�� = 1
027490         MOVE NC"��"                   TO �[��`�F�b�N�v
027500     END-IF.
027510*
027520*     IF ���Z�|���ԊO = 1
027530*         MOVE NC"���ԊO"               TO ���ԊO�v
027540*     END-IF.
027550*     IF ���Z�|�x�� = 1
027560*         MOVE NC"�x��"                 TO �x���v
027570*     END-IF.
027580*     IF ���Z�|�[�� = 1
027590*         MOVE NC"�[��"                 TO �[��v
027600*     END-IF.
027610*
027620*     STRING ���ԊO�v     DELIMITED BY SPACE
027630*            NC"�@"       DELIMITED BY SIZE
027640*            �x���v       DELIMITED BY SPACE
027650*            NC"�@"       DELIMITED BY SIZE
027660*            �[��v       DELIMITED BY SPACE
027670*            INTO �������Z���e�v
027680*     END-STRING.
027690*
027700     MOVE ���Z�|�������Z��             TO  �������Z���v�q.
           MOVE ���Z�|���������k��           TO  ���������k���v�q.
027710     MOVE ���Z�|�Č���                 TO  �Č����v�q.
027720     MOVE ���Z�|���Ë���               TO  ���Ë����v�q.
027730     MOVE ���Z�|���É�               TO  ���É񐔂v�q.
027740     MOVE ���Z�|���×�                 TO  ���×��v�q.
027750     MOVE ���Z�|���É��Z��             TO  ���É��Z���v�q.
027760*
027770     IF ���Z�|��� = 1
027780         MOVE NC"��"                   TO ��ԃ`�F�b�N�v
027790     END-IF.
027800     IF ���Z�|���ԊO = 1
027810         MOVE NC"��"                   TO ���Ð[��`�F�b�N�v
027820     END-IF.
027830     IF ���Z�|�\���J�� = 1
027840         MOVE NC"��"                   TO �\���J��`�F�b�N�v
027850     END-IF.
027860     IF ���Z�|��H = 1
027870         MOVE NC"��"                   TO ��H�`�F�b�N�v
027880     END-IF.
027890*
027900     MOVE ���Z�|�������q���Z��         TO  �������q���Z���v�q.
027910*
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
028010*
028020*     IF ���Z�|�� = 1
028030*         MOVE NC"��"                   TO ������v
028040*     END-IF.
028050*     IF ���Z�|�� = 1
028060*         MOVE NC"��"                   TO �������v
028070*     END-IF.
028080*     IF ���Z�|�� = 1
028090*         MOVE NC"��"                   TO �������v
028100*     END-IF.
028110*
028120     MOVE ���Z�|�{�p���񋟗�         TO �{�p���񋟗��v�q.
028130* ���v
028140     MOVE ���Z�|���v                   TO ���v�v.
028150********************
028160* ���񏈒u���Z�b�g *
028170********************
028180     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
028190             UNTIL ( ���ʂb�m�s > ���ʐ��v )
028200         MOVE ���Z�|���񏈒u��(���ʂb�m�s) TO ���񏈒u���v�q(���ʂb�m�s)
028210         IF ���Z�|���񏈒u��(���ʂb�m�s) NOT = ZERO
028220            EVALUATE ���|�������(���ʂb�m�s)
028230* �P���E�Ŗo�E����
028240            WHEN 1
028250            WHEN 2
028260            WHEN 3
028270                MOVE NC"��"       TO �{�×��`�F�b�N�v
028280* �E�P�E���܁E���܍S�k
028290            WHEN 4
028300            WHEN 5
028310            WHEN 7
028320                MOVE NC"��"       TO �������`�F�b�N�v
028330* �s�S���܁E�s�S���܍S�k
028340            WHEN 6
028350            WHEN 8
028360                MOVE NC"��"       TO �Œ藿�`�F�b�N�v
028370            END-EVALUATE
028380         END-IF
028390     END-PERFORM.
028400     MOVE ���Z�|���񏈒u�����v    TO ���񏈒u�����v�v.
028410********************
028420* �����������Z�b�g *
028430********************
028440*    **********
028450*    * �P���� *
028460*    **********
028470     MOVE ���Z�|��ÒP���P             TO ��ÒP���P�v�q.
028480     MOVE ���Z�|��É񐔂P             TO ��É񐔂P�v�q.
028490     MOVE ���Z�|��×��P               TO ��×��P�v�q.
028500     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
028510     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
028520     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
028530     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
028540     MOVE ���Z�|�d�É񐔂P             TO �d�É񐔂P�v�q.
028550     MOVE ���Z�|�d�×��P               TO �d�×��P�v�q.
028560     MOVE ���Z�|���v�P                 TO ���v�P�v�q.
028570     MOVE ���Z�|�����������P           TO �����������P�v�q.
028580     MOVE ���Z�|���������v�P           TO ���������v�P�v�q.
028590*    **********
028600*    * �Q���� *
028610*    **********
028620     MOVE ���Z�|��ÒP���Q             TO ��ÒP���Q�v�q.
028630     MOVE ���Z�|��É񐔂Q             TO ��É񐔂Q�v�q.
028640     MOVE ���Z�|��×��Q               TO ��×��Q�v�q.
028650     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
028660     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
028670     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
028680     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
028690     MOVE ���Z�|�d�É񐔂Q             TO �d�É񐔂Q�v�q.
028700     MOVE ���Z�|�d�×��Q               TO �d�×��Q�v�q.
028710     MOVE ���Z�|���v�Q                 TO ���v�Q�v�q.
028720     MOVE ���Z�|�����������Q           TO �����������Q�v�q.
028730     MOVE ���Z�|���������v�Q           TO ���������v�Q�v�q.
028740*    ****************
028750*    * �R���ʁ^�W�� *
028760*    ****************
028770     MOVE ���Z�|��ÒP���R�W             TO ��ÒP���R�W�v�q.
028780     MOVE ���Z�|��É񐔂R�W             TO ��É񐔂R�W�v�q.
028790     MOVE ���Z�|��×��R�W               TO ��×��R�W�v�q.
028800     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
028810     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
028820     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
028830     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
028840     MOVE ���Z�|�d�É񐔂R�W             TO �d�É񐔂R�W�v�q.
028850     MOVE ���Z�|�d�×��R�W               TO �d�×��R�W�v�q.
028860     MOVE ���Z�|���v�R�W                 TO ���v�R�W�v�q.
028870     MOVE ���Z�|�����ʍ����v�R�W         TO �����ʍ����v�R�W�v�q.
028880     MOVE ���Z�|�����������R�W           TO �����������R�W�v�q.
028890     MOVE ���Z�|���������v�R�W           TO ���������v�R�W�v�q.
028900*    ****************
028910*    * �R���ʁ^10�� *
028920*    ****************
028930     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
028940     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
028950     MOVE ���Z�|��ÒP���R�O             TO ��ÒP���R�O�v�q.
028960     MOVE ���Z�|��É񐔂R�O             TO ��É񐔂R�O�v�q.
028970     MOVE ���Z�|��×��R�O               TO ��×��R�O�v�q.
028980     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
028990     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
029000     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
029010     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
029020     MOVE ���Z�|�d�É񐔂R�O             TO �d�É񐔂R�O�v�q.
029030     MOVE ���Z�|�d�×��R�O               TO �d�×��R�O�v�q.
029040     MOVE ���Z�|���v�R�O                 TO ���v�R�O�v�q.
029050     MOVE ���Z�|�����������R�O           TO �����������R�O�v�q.
029060     MOVE ���Z�|���������v�R�O           TO ���������v�R�O�v�q.
029070*    ****************
029080*    * �S���ʁ^�T�� *
029090*    ****************
029100     MOVE ���Z�|��ÒP���S�T             TO ��ÒP���S�T�v�q.
029110     MOVE ���Z�|��É񐔂S�T             TO ��É񐔂S�T�v�q.
029120     MOVE ���Z�|��×��S�T               TO ��×��S�T�v�q.
029130     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
029140     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
029150     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
029160     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
029170     MOVE ���Z�|�d�É񐔂S�T             TO �d�É񐔂S�T�v�q.
029180     MOVE ���Z�|�d�×��S�T               TO �d�×��S�T�v�q.
029190     MOVE ���Z�|���v�S�T                 TO ���v�S�T�v�q.
029200     MOVE ���Z�|�����ʍ����v�S�T         TO �����ʍ����v�S�T�v�q.
029210     MOVE ���Z�|�����������S�T           TO �����������S�T�v�q.
029220     MOVE ���Z�|���������v�S�T           TO ���������v�S�T�v�q.
029230*    ****************
029240*    * �S���ʁ^�W�� *
029250*    ****************
029260     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
029270     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
029280     MOVE ���Z�|��ÒP���S�W             TO ��ÒP���S�W�v�q.
029290     MOVE ���Z�|��É񐔂S�W             TO ��É񐔂S�W�v�q.
029300     MOVE ���Z�|��×��S�W               TO ��×��S�W�v�q.
029310     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
029320     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
029330     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
029340     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
029350     MOVE ���Z�|�d�É񐔂S�W             TO �d�É񐔂S�W�v�q.
029360     MOVE ���Z�|�d�×��S�W               TO �d�×��S�W�v�q.
029370     MOVE ���Z�|���v�S�W                 TO ���v�S�W�v�q.
029380     MOVE ���Z�|�����ʍ����v�S�W         TO �����ʍ����v�S�W�v�q.
029390     MOVE ���Z�|�����������S�W           TO �����������S�W�v�q.
029400     MOVE ���Z�|���������v�S�W           TO ���������v�S�W�v�q.
029410*    ****************
029420*    * �S���ʁ^10�� *
029430*    ****************
029440     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
029450     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
029460     MOVE ���Z�|��ÒP���S�O             TO ��ÒP���S�O�v�q.
029470     MOVE ���Z�|��É񐔂S�O             TO ��É񐔂S�O�v�q.
029480     MOVE ���Z�|��×��S�O               TO ��×��S�O�v�q.
029490     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
029500     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
029510     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
029520     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
029530     MOVE ���Z�|�d�É񐔂S�O             TO �d�É񐔂S�O�v�q.
029540     MOVE ���Z�|�d�×��S�O               TO �d�×��S�O�v�q.
029550     MOVE ���Z�|���v�S�O                 TO ���v�S�O�v�q.
029560     MOVE ���Z�|�����������S�O           TO �����������S�O�v�q.
029570     MOVE ���Z�|���������v�S�O           TO ���������v�S�O�v�q.
029580*    *****************
029590*    * �T���ʁ^2.5�� *
029600*    *****************
029610     MOVE ���Z�|��ÒP���T�Q             TO ��ÒP���T�Q�v�q.
029620     MOVE ���Z�|��É񐔂T�Q             TO ��É񐔂T�Q�v�q.
029630     MOVE ���Z�|��×��T�Q               TO ��×��T�Q�v�q.
029640     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
029650     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
029660     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
029670     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
029680     MOVE ���Z�|�d�É񐔂T�Q             TO �d�É񐔂T�Q�v�q.
029690     MOVE ���Z�|�d�×��T�Q               TO �d�×��T�Q�v�q.
029700     MOVE ���Z�|���v�T�Q                 TO ���v�T�Q�v�q.
029710     MOVE ���Z�|�����ʍ����v�T�Q         TO �����ʍ����v�T�Q�v�q.
029720     MOVE ���Z�|�����������T�Q           TO �����������T�Q�v�q.
029730     MOVE ���Z�|���������v�T�Q           TO ���������v�T�Q�v�q.
029740*    ****************
029750*    * �T���ʁ^�T�� *
029760*    ****************
029770     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
029780     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
029790     MOVE ���Z�|��ÒP���T�T             TO ��ÒP���T�T�v�q.
029800     MOVE ���Z�|��É񐔂T�T             TO ��É񐔂T�T�v�q.
029810     MOVE ���Z�|��×��T�T               TO ��×��T�T�v�q.
029820     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
029830     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
029840     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
029850     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
029860     MOVE ���Z�|�d�É񐔂T�T             TO �d�É񐔂T�T�v�q.
029870     MOVE ���Z�|�d�×��T�T               TO �d�×��T�T�v�q.
029880     MOVE ���Z�|���v�T�T                 TO ���v�T�T�v�q.
029890     MOVE ���Z�|�����ʍ����v�T�T         TO �����ʍ����v�T�T�v�q.
029900     MOVE ���Z�|�����������T�T           TO �����������T�T�v�q.
029910     MOVE ���Z�|���������v�T�T           TO ���������v�T�T�v�q.
029920*    ****************
029930*    * �T���ʁ^�W�� *
029940*    ****************
029950     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
029960     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
029970     MOVE ���Z�|��ÒP���T�W             TO ��ÒP���T�W�v�q.
029980     MOVE ���Z�|��É񐔂T�W             TO ��É񐔂T�W�v�q.
029990     MOVE ���Z�|��×��T�W               TO ��×��T�W�v�q.
030000     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
030010     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
030020     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
030030     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
030040     MOVE ���Z�|�d�É񐔂T�W             TO �d�É񐔂T�W�v�q.
030050     MOVE ���Z�|�d�×��T�W               TO �d�×��T�W�v�q.
030060     MOVE ���Z�|���v�T�W                 TO ���v�T�W�v�q.
030070     MOVE ���Z�|�����ʍ����v�T�W         TO �����ʍ����v�T�W�v�q.
030080     MOVE ���Z�|�����������T�W           TO �����������T�W�v�q.
030090     MOVE ���Z�|���������v�T�W           TO ���������v�T�W�v�q.
030100*    ****************
030110*    * �T���ʁ^10�� *
030120*    ****************
030130     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
030140     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
030150     MOVE ���Z�|��ÒP���T�O             TO ��ÒP���T�O�v�q.
030160     MOVE ���Z�|��É񐔂T�O             TO ��É񐔂T�O�v�q.
030170     MOVE ���Z�|��×��T�O               TO ��×��T�O�v�q.
030180     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
030190     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
030200     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
030210     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
030220     MOVE ���Z�|�d�É񐔂T�O             TO �d�É񐔂T�O�v�q.
030230     MOVE ���Z�|�d�×��T�O               TO �d�×��T�O�v�q.
030240     MOVE ���Z�|���v�T�O                 TO ���v�T�O�v�q.
030250     MOVE ���Z�|�����������T�O           TO �����������T�O�v�q.
030260     MOVE ���Z�|���������v�T�O           TO ���������v�T�O�v�q.
030270*
030280*================================================================*
030290 �{�p�L�^�擾 SECTION.
030300*
030310************************************************************
030320* ��P�f�[�^���畉���f�[�^�e���ȉ��̏����擾           *
030330* �� �������Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
030340* �� ���É��Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
030350************************************************************
030360     MOVE  SPACE  TO  �����Č��t���O.
030370     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���ʐ��v
030380         IF ( �{�p�N�v = �����N�v(���ʂb�m�s) ) AND
030390            ( �{�p���v = �������v(���ʂb�m�s) )
030400             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
030410             MOVE �}�Ԃv�q              TO �{�L�|�}��
030420             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
030430             MOVE �����N�v(���ʂb�m�s)  TO �J�n�N�v(���ʂb�m�s) �{�L�|�{�p�N
030440             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
030450             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
030460         ELSE
030470             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
030480             MOVE �}�Ԃv�q              TO �{�L�|�}��
030490             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
030500             MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
030510             MOVE �{�p���v�q            TO �{�L�|�{�p��
030520             MOVE ZERO                  TO �{�L�|�{�p��
030530         END-IF
030540         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
030550                                      �{�L�|�{�p�a��N����
030560         END-START
030570         IF ��ԃL�[ = "00"
030590             MOVE ZERO  TO �I���N�v�s
030600             MOVE ZERO  TO �I�����v�s
030610             MOVE ZERO  TO �I�����v�s
030620             MOVE SPACE TO �I���t���O�Q
030630             PERFORM �{�p�L�^�e�Ǎ�
030640             IF  ( �I���t���O�Q      = SPACE   ) AND
030650                 ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
030660                 ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
030670                 ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
030680                 ( �{�L�|�{�p��      = �{�p���v�q     ) 
030690*
030700*        *****************************************************************
030710*        * �J�n�N���� ( ���̕��ʂ����������łȂ����A
030720*                       ���������ł��}�Ԃ����鎞�́A�ŏ��̎{�p�����J�n��)*
030730*        *****************************************************************
030740                 IF ( �{�p�N�v NOT = �����N�v(���ʂb�m�s) ) OR
030750                    ( �{�p���v NOT = �������v(���ʂb�m�s) ) OR
030760                    ( �J�n�f�Ó��蓮�敪�v = 1 )
030770                     MOVE �{�L�|�{�p�N   TO �J�n�N�v(���ʂb�m�s)
030780                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
030790                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
030800                 END-IF
030810             END-IF
030820             PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
030830                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
030840                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
030850                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
030860                           ( �{�L�|�{�p��     NOT = �{�p���v�q       ) OR
030870                           ( �{�L�|�{�p��         > �I�����v(���ʂb�m�s))
030920                MOVE �{�L�|�{�p�N               TO �I���N�v�s
030930                MOVE �{�L�|�{�p��               TO �I�����v�s
030940                MOVE �{�L�|�{�p��               TO �I�����v�s
030950*
030960                PERFORM �{�p�L�^�e�Ǎ�
030970            END-PERFORM
030980        END-IF
030990*       **************************
031000*       * �p���F�I���N�����Z�b�g *
031010*       **************************
031020        IF �]�A�敪�v(���ʂb�m�s) = 9
031030            MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
031040            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
031050            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
031060        END-IF
031070        IF �I���N�����v(���ʂb�m�s) > �󗝔N�����v
031080            MOVE �I���N�v(���ʂb�m�s) TO �󗝔N�v
031090            MOVE �I�����v(���ʂb�m�s) TO �󗝌��v
031100            MOVE �I�����v(���ʂb�m�s) TO �󗝓��v
031110        END-IF
031120     END-PERFORM.
031130*
031140** ----- �O�������݂̂��𔻒� -----------*
031150*
031160*     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
031170*     MOVE �}�Ԃv�q              TO �{�L�|�}��.
031180*     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
031190*     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
031200*     MOVE �{�p���v�q            TO �{�L�|�{�p��.
031210*     MOVE ZERO                  TO �{�L�|�{�p��.
031220*     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
031230*                                  �{�L�|�{�p�a��N����
031240*     END-START.
031250*     IF ��ԃL�[ = "00"
031260*             MOVE SPACE TO �I���t���O�Q
031270*             PERFORM �{�p�L�^�e�Ǎ�
031280*             IF  ( �I���t���O�Q      = SPACE   ) AND
031290*                 ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
031300*                 ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
031310*                 ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
031320*                 ( �{�L�|�{�p��      = �{�p���v�q     ) 
031330** �����{�p�J�n�����Č����ǂ�������
031340*                 IF   �{�L�|�Č������� = 1
031350*                      MOVE "YES"  TO  �����Č��t���O
031360*                 END-IF
031370**
031380*             END-IF
031390*     END-IF.
031400*     IF �����Č��t���O = "YES"
031410*        PERFORM �O�������̂ݔ���
031420*     END-IF.
031430*
031440*================================================================*
031450 �O�������̂ݔ��� SECTION.
031460*
031470*** �O���̒ʉ@�������������� 
031480     MOVE  SPACE            TO �O���t���O.
031490     MOVE ��|���҃R�[�h    TO �{�L�|���҃R�[�h.
031500     MOVE ��|�{�p�a��      TO �{�L�|�{�p�a��.
031510     MOVE ��|�{�p�N        TO �{�L�|�{�p�N.
031520     MOVE ��|�{�p��        TO �{�L�|�{�p��.
031530     MOVE 1                 TO �{�L�|�{�p��.
031540     START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
031550                                  �{�L�|�{�p�a��N����
031560                                  REVERSED
031570     END-START.
031580     IF ��ԃL�[ = "00"
031590         MOVE SPACE  TO �I���t���O�Q
031600         PERFORM �{�p�L�^�e�Ǎ�
031610         IF ( �I���t���O�Q      = SPACE  ) AND
031620            ( �{�L�|���҃R�[�h  = ��|���҃R�[�h ) AND
031630            ( �{�L�|�f�Ë敪    = 2 ) 
031640*
031650            PERFORM �O������
031660**** �K�p�P���g�p
031670            IF �O���t���O = "YES"
031680               MOVE NC"���O�������̂�"    TO  �K�p�P�v
031690            END-IF
031700**
031710         END-IF
031720     END-IF.
031730*
031740*================================================================*
031750 �O������  SECTION.
031760* 
031770*** �ǂݍ��񂾎{�p�L�^�̔N�����A�O�����ǂ������� (�N���̍��� 1 ��?)
031780      MOVE  SPACE  TO  �O���t���O.
031790      INITIALIZE  �v�Z�N�����v �J�n�N�����Q�v �I���N�����Q�v.
031800**
031810      MOVE ��|�{�p�a��    TO �I���a��Q�v.
031820      MOVE ��|�{�p�N      TO �I���N�Q�v.
031830      MOVE ��|�{�p��      TO �I�����Q�v.
031840      MOVE �{�L�|�{�p�a��  TO �J�n�a��Q�v.
031850      MOVE �{�L�|�{�p�N    TO �J�n�N�Q�v.
031860      MOVE �{�L�|�{�p��    TO �J�n���Q�v.
031870*
031880      EVALUATE TRUE
031890       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v = �I���N�Q�v)
031900            PERFORM  �O����r��
031910       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v NOT = �I���N�Q�v)
031920            PERFORM  �O����r�N
031930       WHEN  �J�n�a��Q�v NOT = �I���a��Q�v 
031940            PERFORM  �O����r����
031950      END-EVALUATE.
031960*
031970      IF �v�Z���v = 1
031980         MOVE  "YES"  TO  �O���t���O
031990      END-IF.
032000*
032010*================================================================*
032020 ���Z�v�g���я��擾 SECTION.
032030*
032040     MOVE �{�p�a��v�q       TO ��Q�|�{�p�a��.
032050     MOVE �{�p�N�v�q         TO ��Q�|�{�p�N.
032060     MOVE �{�p���v�q         TO ��Q�|�{�p��.
032070     MOVE ���҃R�[�h�v�q     TO ��Q�|���҃R�[�h.
032080** �����́A������ʂ��Z�b�g
032090     MOVE ������ʂv�q       TO ��Q�|�ی����.
032100*
032110     READ ��ƃt�@�C���Q
032120     NOT INVALID KEY
032130          MOVE ��Q�|����    TO �����\���Ԃv
032140     END-READ.
032150*
032160*================================================================*
032170 ��������擾 SECTION.
032180*
032190* �R�J���ȏ�̒�������� "CHOUKI" ���Ă�. 
032200     MOVE  SPACE TO  �A���ԁ|�L�[.
032210     INITIALIZE      �A���ԁ|�L�[.
032220     MOVE �{�p�a��v�q  TO  �A���ԁ|�{�p�a��.
032230     MOVE �{�p�N�v�q    TO  �A���ԁ|�{�p�N.
032240     MOVE �{�p���v�q    TO  �A���ԁ|�{�p��.
032250     MOVE ���Ҕԍ��v�q  TO  �A���ԁ|���Ҕԍ�.
032260     MOVE �}�Ԃv�q      TO  �A���ԁ|�}��.
032270*
032280     CALL   "CHOUKI".
032290     CANCEL "CHOUKI".
032300*
032310**** �K�p�P���g�p (�u�O�������̂݁v�����鎞�́A��������)
032320     IF �A���ԁ|�Ώۃt���O  = "YES"
032330        IF �K�p�P�v  = SPACE
032340           MOVE NC"�������{�p�p�����R���ʂɋL��"  TO �K�p�P�v
032350        ELSE
032360           STRING �K�p�P�v           DELIMITED BY SPACE
032370                  NC"�C"             DELIMITED BY SIZE
032380                  NC"�������{�p�p�����R���ʂɋL��"   DELIMITED BY SIZE
032390                  INTO �K�p�P�v
032400           END-STRING
032410        END-IF
032420     END-IF.
032430*
032440*================================================================*
032450 �������ȑO�̃f�[�^���� SECTION.
032460*
032470*********************************************************************************
032480*  �ŏ��̏������ȑO�̓������Ɏ{�p�L�^���R�[�h����������(�����A���~)�́A�����敪��
032490*  �p���ɂ��`�F�b�N����B(�V�K�ƌp���̗���)
032500*********************************************************************************
032510** �ŏ��̏��������擾
032520     MOVE SPACE                 TO �����t���O.
032530     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
032540     MOVE �}�Ԃv�q              TO �{�L�|�}��.
032550     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
032560     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
032570     MOVE �{�p���v�q            TO �{�L�|�{�p��.
032580     MOVE ZERO                  TO �{�L�|�{�p��.
032590     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
032600                                  �{�L�|�{�p�a��N����
032610     END-START.
032620     IF ��ԃL�[ = "00"
032630         MOVE ZERO  TO �����a��v�s
032640         MOVE ZERO  TO �����N�v�s
032650         MOVE ZERO  TO �������v�s
032660         MOVE ZERO  TO �������v�s
032670         MOVE SPACE TO �I���t���O�Q
032680         PERFORM �{�p�L�^�e�Ǎ�
032690         PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
032700                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
032710                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
032720                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
032730                       ( �{�L�|�{�p��     NOT = �{�p���v�q      ) OR
032740                       ( �����t���O           = "YES"           ) 
032750               IF  �{�L�|�f�Ë敪 = 2
032760                   MOVE �{�L�|�{�p�a��           TO �����a��v�s
032770                   MOVE �{�L�|�{�p�N             TO �����N�v�s
032780                   MOVE �{�L�|�{�p��             TO �������v�s
032790                   MOVE �{�L�|�{�p��             TO �������v�s
032800                   MOVE "YES"                    TO �����t���O
032810               END-IF
032820               PERFORM �{�p�L�^�e�Ǎ�
032830         END-PERFORM
032840     END-IF.
032850*
032860* �������ȑO�̃f�[�^����
032870     IF �����t���O = "YES"
032880        MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
032890        MOVE �}�Ԃv�q              TO �{�L�|�}��
032900        MOVE �����a��v�s          TO �{�L�|�{�p�a��
032910        MOVE �����N�v�s            TO �{�L�|�{�p�N
032920        MOVE �������v�s            TO �{�L�|�{�p��
032930        MOVE �������v�s            TO �{�L�|�{�p��
032940        START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
032950                                     �{�L�|�{�p�a��N����
032960                                     REVERSED
032970        END-START
032980        IF ��ԃL�[ = "00"
032990           MOVE SPACE  TO �I���t���O�Q
033000           PERFORM �{�p�L�^�e�Ǎ�
033010           IF ( �I���t���O�Q    = SPACE        ) AND
033020              ( �{�L�|���Ҕԍ�  = ���Ҕԍ��v�q ) AND
033030              ( �{�L�|�}��      = �}�Ԃv�q     ) AND
033040              ( �{�L�|�{�p�a��  = �����a��v�s ) AND
033050              ( �{�L�|�{�p�N    = �����N�v�s   ) AND
033060              ( �{�L�|�{�p��    = �������v�s   )
033070*  �������ȑO�̓������Ɏ{�p�L�^���R�[�h����������
033080                IF �p���`�F�b�N�v = SPACE
033090                   MOVE NC"��"    TO �p���`�F�b�N�v
033100                END-IF
033110           END-IF
033120         END-IF
033130     END-IF.
033140*
033150*================================================================*
033160 �������Z�����擾 SECTION.
033170*****************************************************************
033180** �������Z�����ԊO�Ɛ[��̎��A�K�p�Ɂu��t���ԁv���󎚂���B
033190**   �����̈󎚂͌�3��܂ŉ\
033200*****************************************************************
033210     IF ( ���Z�|���ԊO = 1 ) OR ( ���Z�|�[�� = 1 ) OR ( ���Z�|�x�� = 1 )
033220*
033230         MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
033240         MOVE �}�Ԃv�q              TO �{�L�|�}��
033250         MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
033260         MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
033270         MOVE �{�p���v�q            TO �{�L�|�{�p��
033280         MOVE ZERO                  TO �{�L�|�{�p��
033290         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
033300                                      �{�L�|�{�p�a��N����
033310         END-START
033320         IF ��ԃL�[ = "00"
033330             MOVE ZERO  TO �������Z�J�E���g
033340             MOVE SPACE TO �I���t���O�Q
033350             PERFORM �{�p�L�^�e�Ǎ�
033360             PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
033370                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
033380                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
033390                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
033400                           ( �{�L�|�{�p��     NOT = �{�p���v�q      ) 
033410                   IF  ( �{�L�|�������Z = 1 OR 2 OR 3 ) AND ( �{�L�|�f�Ë敪 = 2 )
033420                       COMPUTE �������Z�J�E���g = �������Z�J�E���g  + 1
033430                       IF  �������Z�J�E���g <= 3
033440                           MOVE �{�L�|�������Z TO �������Z�敪�v�s(�������Z�J�E���g)
033450                           MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
033460                           MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
033470                       END-IF
033480                   END-IF
033490                   PERFORM �{�p�L�^�e�Ǎ�
033500             END-PERFORM
033510** �������Z�̎�����K�p�ɃZ�b�g
033380            IF ( �������Z���v�s(1) NOT = ZERO ) OR ( �������Z���v�s(1) NOT = ZERO )
                     MOVE �������Z���v�s(1) TO �������Z���v
                     MOVE ":"               TO �������Z��؂v
                     MOVE �������Z���v�s(1) TO �������Z���v
                  END-IF
033380            IF ( �������Z���v�s(2) NOT = ZERO ) OR ( �������Z���v�s(2) NOT = ZERO ) 
031910               PERFORM �������Z�K�p�Z�b�g
                  END-IF
033530         END-IF
033540*
033550     END-IF.
033560*
033570*================================================================*
033580 �������Z�K�p�Z�b�g SECTION.
033590*
033600     PERFORM VARYING �ԍ��J�E���^ FROM 1 BY 1
033610              UNTIL  �ԍ��J�E���^ > 3
033620         IF ( �������Z���v�s(�ԍ��J�E���^)  = ZERO )  AND 
033630            ( �������Z���v�s(�ԍ��J�E���^)  = ZERO ) 
033640             CONTINUE
033650         ELSE
033660* �Œ荀��
033670             EVALUATE �������Z�敪�v�s(�ԍ��J�E���^) 
033680             WHEN 1
033690                MOVE NC"���ԊO"   TO ���Z���e�v(�ԍ��J�E���^)
033320             WHEN 2
033330                MOVE NC"�x�@��"   TO ���Z���e�v(�ԍ��J�E���^)
033700             WHEN 3
033710                MOVE NC"�[�@��"   TO ���Z���e�v(�ԍ��J�E���^)
033720             END-EVALUATE
033730*
033740             MOVE NC"�F"          TO ���Z��؂v(�ԍ��J�E���^)
033750             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
033760             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
033770*
033780**** ���������{��ϊ�
033790* ����
033800             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
033810             IF �����v >= 10
033820                 MOVE �����v�P    TO �����ԍ��v�P
033830                 PERFORM ���{��ϊ�
033840                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
033850                 MOVE �����v�Q    TO �����ԍ��v�P
033860                 PERFORM ���{��ϊ�
033870                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
033880             ELSE
033890                 MOVE �����v�Q    TO �����ԍ��v�P
033900                 PERFORM ���{��ϊ�
033910                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
033920             END-IF
033930* ��
033940             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
033950             MOVE �����v�P    TO �����ԍ��v�P
033960             PERFORM ���{��ϊ�
033970             MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
033980             MOVE �����v�Q    TO �����ԍ��v�P
033990             PERFORM ���{��ϊ�
034000             MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
034010** 
034020        END-IF
034030     END-PERFORM.
034040*
034050     MOVE  �������Z�W�c�m�v(1)   TO �������Z�����P�v. 
034060     MOVE  �������Z�W�c�m�v(2)   TO �������Z�����Q�v. 
034070     MOVE  �������Z�W�c�m�v(3)   TO �������Z�����R�v. 
034080*
034090**** �K�p�P���Q���g�p�i�������R�L�ڂœK�p�P���g���Ă��鎞�́A�K�p�Q�j
034100     IF ( �������Z���v�s(2)  = ZERO ) AND ( �������Z���v�s(2)  = ZERO ) 
034110         CONTINUE
034120     ELSE
034130         IF �K�p�P�v  = SPACE
034140               STRING NC"�������Z"       DELIMITED BY SIZE
034150                      �������Z�����P�v   DELIMITED BY SIZE
034160                      �������Z�����Q�v   DELIMITED BY SIZE
034170                      �������Z�����R�v   DELIMITED BY SIZE
034180                      INTO �K�p�P�v
034190               END-STRING
034200         ELSE
034210               STRING NC"�������Z"       DELIMITED BY SIZE
034220                      �������Z�����P�v   DELIMITED BY SIZE
034230                      �������Z�����Q�v   DELIMITED BY SIZE
034240                      �������Z�����R�v   DELIMITED BY SIZE
034250                      INTO �K�p�Q�v
034260               END-STRING
034270         END-IF
034280     END-IF.
034290*
034300*================================================================*
034310 ���{��ϊ� SECTION.
034320*
034330     MOVE NC"�O"     TO �S�p�����ԍ��v.
034340     CALL "htoz" WITH C LINKAGE
034350                        USING �����ԍ��v�P �S�p�����ԍ��v�P.
034360*
034370*================================================================*
034380 ���t�����擾 SECTION.
034390*
034400* �� �{�l���S�����ł͂Ȃ��A�ی��҂̕��S����
034410*
034420*** �Q�V�g��A�픚�̎��́A�u�V�v�ƈ�
      */�㍂�����̏ꍇ�́u��v�ƈ�/100413
034430     IF ( �����ʂv�q NOT = ZERO )  AND
034440        ( ������ʂv�q NOT = ZERO )
034450         MOVE SPACE     TO  ���t�����v�o
034460*         MOVE NC"�V"    TO  �����Œ�v
034460         MOVE NC"��"    TO  ��󎚂v
034470     ELSE
034500         MOVE ���Z�|���t����   TO  ���t�����v�o
034510*         MOVE NC"��"          TO  �����Œ�v
034520     END-IF.
034530*
034540*================================================================*
034550 ���S�����擾 SECTION.
034560*
034570* �� �{�l���S�����ł͂Ȃ��A�ی��҂̕��S��
034580*
      */�㍂�����̎������S�����󎚂���B(���S���擾PG���g�p)/100413
           IF ��|�{�p�a��N�� >= 41410
               MOVE ���Z�|���S���� TO ���S�����v
040726         MOVE NC"��" TO �����Œ�v
           ELSE
034590         IF ( �����ʂv�q NOT = ZERO )  AND
034600            ( ������ʂv�q NOT = ZERO )
034610             MOVE SPACE     TO  �����Œ�v
034620             MOVE ZERO      TO  ���S�����v
034630         ELSE
                  MOVE ���Z�|���S���� TO ���S�����v
040726            MOVE NC"��"        TO  �����Œ�v
034670         END-IF
034670     END-IF.
034680*
034690*================================================================*
034700 ������擾 SECTION.
034710*
034720*****************************************
034730*  ���������鎞�A������ʂ��󎚂���B
034740*****************************************
034750*
034760     EVALUATE ������ʂv�q 
034770*** ���� (���ۂ͂��̑������ŁA�Y���Ȃ�)
034780     WHEN  50
034790         CONTINUE
034800*** 41�V�l
034810     WHEN  51
034820*********** ��4���� "4113"���� "4108"��� "4132"���� �̎��́A�u���v�B����ȊO�́u�V�v
034830        IF  ( ����s�����ԍ��v(1:4) = "4113" )  OR
034840            ( ����s�����ԍ��v(1:4) = "4108" )  OR
034850            ( ����s�����ԍ��v(1:4) = "4132" )  
034860            MOVE NC"��"    TO ������v
034870        ELSE
034880            MOVE NC"�V"    TO ������v
034890        END-IF
034900*** ��q
034910     WHEN  52
034920            MOVE NC"��"    TO ������v
034930***            MOVE NC"�e"    TO ������v
034940*** �g��
034950     WHEN  53
034960            MOVE NC"��"    TO ������v
034970*** �픚
034980     WHEN  54
034990            MOVE NC"��"    TO ������v
035000*** ���c�� 
035010     WHEN  55
035020            MOVE NC"��"    TO ������v
035030*** ���̑�
035040     WHEN  60
035050            CONTINUE
035060     WHEN  OTHER
035070            CONTINUE
035080     END-EVALUATE.
      *
      */�����󂪋󔒂̏ꍇ��JOSEIMEI�ŃZ�b�g����/181204
           IF ������v = SPACE
033913         MOVE SPACE TO  �A�������́|�L�[
033914         INITIALIZE     �A�������́|�L�[
033915         MOVE ������ʂv�q     TO �A�������́|�������
033916         MOVE ����s�����ԍ��v TO �A�������́|��p���S�Ҕԍ�����
033917*
033918         CALL   "JOSEIMEI"
033919         CANCEL "JOSEIMEI"
033920*
033921         MOVE �A�������́|�P���� TO ������v
           END-IF.
035090*
035100*================================================================*
035110 ��{���擾 SECTION.
035120*
035130     MOVE 01                TO ���`�|�敪�R�[�h.
035140     MOVE ZERO              TO ���`�|�������.
035150     MOVE ZERO              TO ���`�|����.
035160     MOVE ZERO              TO ���`�|���E�敪.
035170     MOVE ZERO              TO ���`�|�����ʒu�ԍ�.
035180     MOVE �{�p�a��v�q      TO ���`�|�J�n�a��.
035190     MOVE �{�p�N�v�q        TO ���`�|�J�n�N.
035200     MOVE �{�p���v�q        TO ���`�|�J�n��.
035210     START �����}�X�^ KEY IS <= ���|�敪�R�[�h
035220                                ���|���ʃR�[�h
035230                                ���|�J�n�a��N��
035240                                REVERSED
035250     END-START.
035260     READ �����}�X�^ NEXT
035270     NOT AT END
035280         MOVE ���`�|��㪖@��          TO ��㪖@�P���v
035290         MOVE ���`�|��㪖@��          TO ��㪖@�P���v
035300         MOVE ���`�|�d�×�            TO �d�ÒP���v
035310     END-READ.
035320*
035330*================================================================*
035340 �{�p����N�擾 SECTION.
035350*
035360     MOVE �{�p�a��v�q TO ���|�����敪.
035370     READ �����}�X�^
035380     NOT INVALID KEY
035390         MOVE ���|�J�n����N TO �{�p����N�v
035400     END-READ.
035410     IF �{�p����N�v NOT = ZERO
035420        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
035430     END-IF.
035440     MOVE �{�p����N�v  TO ���Z�v�g�Ǘ��N�v.
035450*
035460*================================================================*
035470 �ϔC�N�����擾 SECTION.
035480*
035490** ---// �����̎󗝔N�ɂ́A�ŏI�ʉ@���������Ă���ׁA�ޔ����� //----
035500     MOVE �󗝔N�v   TO �ŏI�ʉ@�N�v.
035510     MOVE �󗝌��v   TO �ŏI�ʉ@���v.
035520     MOVE �󗝓��v   TO �ŏI�ʉ@���v.
035530***
035540* (�_���t��)
035550     EVALUATE ���Z�v�g���t�敪�v 
035560*    /  �ŏI�ʉ@�� /
035570     WHEN ZERO
035580         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
035590         MOVE �ŏI�ʉ@���v TO �_���t���v
035600         MOVE �ŏI�ʉ@���v TO �_���t���v
035610*    /  ������ /
035620     WHEN 1 
035630         PERFORM �������擾
035640         MOVE �󗝔N�v     TO �_���t�N�v
035650         MOVE �󗝌��v     TO �_���t���v
035660         MOVE �󗝓��v     TO �_���t���v
035670*    /  �󎚂Ȃ� /
035680     WHEN 9
035690         MOVE ZERO         TO �_���t�N�v
035700         MOVE ZERO         TO �_���t���v
035710         MOVE ZERO         TO �_���t���v
035720*    /  ���̑��́A�ŏI�ʉ@�� /
035730     WHEN OTHER
035740         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
035750         MOVE �ŏI�ʉ@���v TO �_���t���v
035760         MOVE �ŏI�ʉ@���v TO �_���t���v
035770     END-EVALUATE.
035780**
035790* (���ґ�)
035800     EVALUATE ���Z�v�g���ғ��t�敪�v 
035810*    /  �ŏI�ʉ@�� /
035820     WHEN ZERO
035830         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
035840         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
035850         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
035860*    /  ������ /
035870     WHEN 1 
035880         PERFORM �������擾
035890         MOVE �󗝔N�v     TO ���҈ϔC�N�v
035900         MOVE �󗝌��v     TO ���҈ϔC���v
035910         MOVE �󗝓��v     TO ���҈ϔC���v
035920*    /  �󎚂Ȃ� /
035930     WHEN 9
035940         MOVE ZERO         TO ���҈ϔC�N�v
035950         MOVE ZERO         TO ���҈ϔC���v
035960         MOVE ZERO         TO ���҈ϔC���v
035970*    /  ���̑��́A�ŏI�ʉ@�� /
035980     WHEN OTHER
035990         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
036000         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
036010         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
036020     END-EVALUATE.
036030*
036040*================================================================*
036050 �������擾 SECTION.
036060*
036070     MOVE �{�p�N�v�q   TO �󗝔N�v.
036080     MOVE �{�p���v�q   TO �󗝌��v.
036090     MOVE �{�p�a��v�q TO ���|�����敪.
036100     READ �����}�X�^
036110     NOT INVALID KEY
036120         MOVE ���|�J�n����N TO �{�p����N�v
036130     END-READ.
036140     IF �{�p����N�v NOT = ZERO
036150        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
036160     END-IF.
036170*
036180     EVALUATE �{�p���v�q
036190     WHEN 4
036200     WHEN 6
036210     WHEN 9
036220     WHEN 11
036230         MOVE 30 TO �󗝓��v
036240     WHEN 2
036250         DIVIDE 4 INTO �{�p����N�v GIVING    ���v
036260                                    REMAINDER �]�v
036270         END-DIVIDE
036280         IF �]�v = ZERO
036290             MOVE 29 TO �󗝓��v
036300         ELSE
036310             MOVE 28 TO �󗝓��v
036320         END-IF
036330     WHEN 1
036340     WHEN 3
036350     WHEN 5
036360     WHEN 7
036370     WHEN 8
036380     WHEN 10
036390     WHEN 12
036400         MOVE 31 TO �󗝓��v
036410     WHEN OTHER
036420          CONTINUE
036430     END-EVALUATE.
036440*
036450*================================================================*
036460 ���������擾 SECTION.
036470*
036480********************************************************************
036490*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
036500*  ��: �@�A �Ƃœ]��.
036510*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
036520*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
036530********************************************************************
036540     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
036550     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
036560             UNTIL ( ���ʂb�m�s > ���ʐ��v )
036570*
036580****        IF ( ���|�������Ҕԍ�(���ʂb�m�s)  NOT = ZERO )  AND
036590        IF ( ���|�����A��(���ʂb�m�s)      NOT = ZERO )
036600*
036610           IF �J�E���^ = ZERO
036620               MOVE 1   TO  �J�E���^ �J�E���^�Q
036630               MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
036640               MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
036650               MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
036660           ELSE
036670              IF ( ���|�������Ҕԍ�(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
036680                 ( ���|�����A��(���ʂb�m�s)      = �����A�Ԃb�v     )
036690                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
036700                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
036710              ELSE
036720                 COMPUTE �J�E���^ = �J�E���^  +  1
036730                 MOVE 1   TO  �J�E���^�Q
036740                 MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
036750                 MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
036760                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
036770              END-IF
036780           END-IF
036790        END-IF
036800     END-PERFORM.
036810**************************************************************************
036820*  ���������}�X�^��蕶�͎擾
036830**************************************************************************
036840     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
036850     PERFORM VARYING �J�E���^ FROM 1 BY 1
036860             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
036870** ���ۂ� �敪 01
036880         MOVE 01                        TO �����|�敪�R�[�h
036890         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
036900         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
036910         READ ���������e
036920         NOT INVALID KEY
036930             INITIALIZE ���������v�s
036940             MOVE �����|���������b�l(1) TO  ���������P�v�s
036950             MOVE �����|���������b�l(2) TO  ���������Q�v�s
036960             MOVE �����|���������b�l(3) TO  ���������R�v�s
036970             MOVE �����|���������b�l(4) TO  ���������S�v�s
036980             MOVE �����|���������b�l(5) TO  ���������T�v�s
036990             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
037000                     UNTIL ( �J�E���^�Q > 9 )  OR 
037010                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
037020                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
037030                WHEN 1
037040                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037050                WHEN 2
037060                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037070                WHEN 3
037080                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037090                WHEN 4
037100                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037110                WHEN 5
037120                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037090                WHEN 6
037100                   MOVE "�E"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037110                WHEN 7
037120                   MOVE "�F"  TO  ���������i���o�[�v�P(�J�E���^�Q)
037130                WHEN OTHER
037140                   CONTINUE
037150                END-EVALUATE
037160             END-PERFORM
037161*
037162             IF �����|�����������͋敪 = 1
037163                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
037164                        ���������P�v�s  DELIMITED BY SIZE
037165                        ���������Q�v�s  DELIMITED BY SIZE
037166                        ���������R�v�s  DELIMITED BY SIZE
037167                        ���������S�v�s  DELIMITED BY SIZE
037168                        ���������T�v�s  DELIMITED BY SIZE
037169                        INTO �����������e�����v(�J�E���^)
037170                 END-STRING
037171             ELSE
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
037180             END-IF
037181*
037260         END-READ
037270     END-PERFORM.
037280*
037290     PERFORM ���������Z�b�g.
037300*
037310*================================================================*
037320 ���������Z�b�g SECTION.
037330*
037340**************************************************************************
037350*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
037360**************************************************************************
037370     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
037380     PERFORM VARYING �J�E���^ FROM 1 BY 1
037390             UNTIL ( �J�E���^ > 9 )  OR ( �����������e�����v(�J�E���^) = SPACE )
037400*
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
037550*
037560     END-PERFORM.
037570*================================================================*
037580 �������R���擾 SECTION.
037590*
037600* �������R���擾�� "CHOUBUN" ���Ă�. 
037610     MOVE  SPACE TO  �A�����|�L�[.
037620     INITIALIZE      �A�����|�L�[.
037630     MOVE �{�p�a��v�q  TO  �A�����|�{�p�a��.
037640     MOVE �{�p�N�v�q    TO  �A�����|�{�p�N.
037650     MOVE �{�p���v�q    TO  �A�����|�{�p��.
037660     MOVE ���Ҕԍ��v�q  TO  �A�����|���Ҕԍ�.
037670     MOVE �}�Ԃv�q      TO  �A�����|�}��.
037680** ���ڗp��56��
037690     MOVE 56            TO  �A�����|������.
037700*
037710     CALL   "CHOUBUN".
037720     CANCEL "CHOUBUN".
037730*
037740*================================================================*
037750*================================================================*
037760 �{�p�L�^�e�Ǎ� SECTION.
037770*
037780     READ �{�p�L�^�e NEXT
037790     AT END
037800         MOVE "YES" TO �I���t���O�Q
037810     END-READ.
037820*================================================================*
037830 �O����r��  SECTION.
037840*
037850     IF  �I�����Q�v >  �J�n���Q�v
037860         COMPUTE �v�Z���v = �I�����Q�v - �J�n���Q�v
037870     ELSE
037880        MOVE ZERO TO �v�Z���v
037890     END-IF.
037900*
037910*================================================================*
037920 �O����r�N  SECTION.
037930*
037940     IF  �I���N�Q�v >  �J�n�N�Q�v
037950         COMPUTE �v�Z�N�v = �I���N�Q�v - �J�n�N�Q�v
037960         COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
037970     ELSE
037980        MOVE ZERO TO �v�Z���v
037990     END-IF.
038000*
038010*================================================================*
038020 �O����r����  SECTION.
038030*
038040     MOVE �J�n�a��Q�v TO ���|�����敪.
038050     READ �����}�X�^
038060     NOT INVALID KEY
038070         MOVE ���|�J�n����N TO �J�n����N�v
038080     END-READ.
038090     MOVE �I���a��Q�v TO ���|�����敪.
038100     READ �����}�X�^
038110     NOT INVALID KEY
038120         MOVE ���|�J�n����N TO �I������N�v
038130     END-READ.
038140**
038150     IF (�J�n����N�v NOT = ZERO) AND (�I������N�v NOT = ZERO)
038160        COMPUTE �J�n����N�v = �J�n����N�v + �J�n�N�Q�v - 1
038170        COMPUTE �I������N�v = �I������N�v + �I���N�Q�v - 1
038180*
038190        IF �I������N�v =  �J�n����N�v
038200           PERFORM  �O����r��
038210        ELSE
038220           IF  �I������N�v >  �J�n����N�v
038230               COMPUTE �v�Z�N�v = �I������N�v - �J�n����N�v
038240               COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
038250           ELSE
038260               MOVE ZERO TO �v�Z���v
038270           END-IF
038280        END-IF
038290     ELSE
038300        MOVE ZERO TO �v�Z���v
038310     END-IF.
038320*
038330*================================================================*
038340 ������� SECTION.
038350*
      */����ԍ������/110720
041530        MOVE "YHP6425P"  TO  ��`�̖��o
041540        MOVE "GRP002"   TO  ���ڌQ���o
041550        WRITE YHP6425P
041570        PERFORM �G���[�����o
      *
038360     MOVE "YHP6425P"  TO  ��`�̖��o.
038370     MOVE "SCREEN"   TO  ���ڌQ���o.
038380     WRITE YHP6425P.
038390****     WRITE ������R�[�h.
038400     PERFORM �G���[�����o.
038410*================================================================*
038420 �G���[�����o SECTION.
038430*
038440     IF �ʒm���o NOT = "00"
038450         DISPLAY NC"���[�G���["              UPON CONS
038460         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
038470         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
038480         DISPLAY NC"�g������o�F" �g������o UPON CONS
038490         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
038500                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
038510         ACCEPT  �L�[���� FROM CONS
038520         PERFORM �t�@�C����
038530         MOVE 99  TO PROGRAM-STATUS
038540         EXIT PROGRAM
038550     END-IF.
038560*================================================================*
038570 ��f�҈���敪�X�V SECTION.
038580*
038590** //  ��f�ҏ��e�̈���敪�ɂP���Z�b�g���A�X�V����B//  
038600*
038610     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
038620     MOVE �{�p�N�v�q         TO ��|�{�p�N.
038630     MOVE �{�p���v�q         TO ��|�{�p��.
038640     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
038650     READ ��f�ҏ��e
038660     NOT INVALID KEY
038670         MOVE  1  TO  ��|���Z����敪����
038680         REWRITE  ��|���R�[�h
038690         END-REWRITE
038700         IF ��ԃL�[ NOT = "00"
038710            MOVE NC"��f��" TO �t�@�C����
038720            PERFORM �G���[�\��
038730         END-IF
038740     END-READ.
038750*
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
038751*================================================================*
038752 ���Z�E�v�ăZ�b�g SECTION.
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
038770*
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
038771*================================================================*
038772*================================================================*
038773 �G���[�\�� SECTION.
038780*
038790     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
038800     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
038810     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
038820     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
038830     ACCEPT  �L�[���� FROM CONS
038840     PERFORM �t�@�C����.
038850     EXIT PROGRAM.
038860*================================================================*
038870 �I������ SECTION.
038880*
038890     PERFORM �t�@�C����.
038900*================================================================*
038910 �t�@�C���� SECTION.
038920*
038930     CLOSE ����t�@�C��.
038940     CLOSE �����}�X�^       ���̃}�X�^
038950           ���Z�v�g�e       ������}�X�^      �{�p�����}�X�^
038960           �o�߃}�X�^       ��f�ҏ��e        �s�����}�X�^
038970           �{�p�L�^�e       �����f�[�^�e        ���������e
038980           �����}�X�^       ������}�X�^        �h�c�Ǘ��}�X�^
038990           ����}�X�^     ��ƃt�@�C���Q      ��f�ҏ��Q�e.
039000*
039010*================================================================*
039020*================================================================*
039030 �e�X�g�󎚏��� SECTION.
039040*
039050     MOVE ALL "X"    TO ���{�p�h�c.
039060     MOVE ALL NC"�m" TO �����όŒ�.
039070*     MOVE ALL NC"�m" TO ������.
039080     MOVE 99         TO �{�p�N �{�p��.
039090*     MOVE ALL NC"�m" TO �L��.
039100*     MOVE ALL "X"    TO �ԍ�.
039110     MOVE ALL "X"    TO �ی��Ҕԍ� ����S�Ҕԍ� �󋋎Ҕԍ�.
039120*     MOVE ALL NC"�m" TO �ی���� �����Œ�.
039130*     MOVE 99         TO ���S����.
039140     MOVE ALL "X"    TO �Z���P �Z���Q.
039150*     MOVE ALL "X"    TO �󋋎҃J�i.
039160     MOVE ALL "�m" TO ���Ҏ���.
039170*     MOVE "(�j)"     TO ���Ґ���.
039180*     MOVE ALL NC"�m" TO ���N�����Œ�  ����.
      *�󋋎Ҍ���
039190*     MOVE 99         TO �󋋎ҔN �󋋎Ҍ� �󋋎ғ� ���ҔN ���Ҍ� ���ғ�.
039200     MOVE ALL "M"    TO ���������P ���������Q ���������R ���������S
                              ���������T ���������U.
039220     MOVE ALL NC"�m" TO �������P.
039230     MOVE 99 TO �����N�P �������P �������P �����N�P �������P �������P
039240                �J�n�N�P �J�n���P �J�n���P �I���N�P �I�����P �I�����P
039250                �������P.
039260     MOVE NC"��" TO �����`�F�b�N�P ���~�`�F�b�N�P �]��`�F�b�N�P.
039270     MOVE ALL NC"�m" TO �������Q.
039280     MOVE 99 TO �����N�Q �������Q �������Q �����N�Q �������Q �������Q
039290                �J�n�N�Q �J�n���Q �J�n���Q �I���N�Q �I�����Q �I�����Q
039300                �������Q.
039310     MOVE NC"��" TO �����`�F�b�N�Q ���~�`�F�b�N�Q �]��`�F�b�N�Q.
039320     MOVE ALL NC"�m" TO �������R.
039330     MOVE 99 TO �����N�R �������R �������R �����N�R �������R �������R
039340                �J�n�N�R �J�n���R �J�n���R �I���N�R �I�����R �I�����R
039350                �������R.
039360     MOVE NC"��" TO �����`�F�b�N�R ���~�`�F�b�N�R �]��`�F�b�N�R.
039370     MOVE ALL NC"�m" TO �������S.
039380     MOVE 99 TO �����N�S �������S �������S �����N�S �������S �������S
039390                �J�n�N�S �J�n���S �J�n���S �I���N�S �I�����S �I�����S
039400                �������S.
039410     MOVE NC"��" TO �����`�F�b�N�S ���~�`�F�b�N�S �]��`�F�b�N�S.
039420     MOVE ALL NC"�m" TO �������T.
039430     MOVE 99 TO �����N�T �������T �������T �����N�T �������T �������T
039440                �J�n�N�T �J�n���T �J�n���T �I���N�T �I�����T �I�����T
039450                �������T.
039460     MOVE NC"��" TO �����`�F�b�N�T ���~�`�F�b�N�T �]��`�F�b�N�T.
039470     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
039480             UNTIL ( ���ʂb�m�s > 5 )
039490         MOVE ALL NC"�m" TO �o�ߗ���(���ʂb�m�s)
039500     END-PERFORM.
039510     MOVE NC"��" TO �V�K�`�F�b�N �p���`�F�b�N.
039520     MOVE 99999 TO  ������.
039530     MOVE 99999 TO  �Č���.
039540     MOVE 99.9 TO  ���Ë���.
039550     MOVE 99 TO  ���É�.
039560     MOVE 99999 TO  ���×�.
039570     MOVE NC"��" TO  ��`�F�b�N ���`�F�b�N ���`�F�b�N.
039580     MOVE 99999 TO  �������q���Z��.
039590     MOVE 999999 TO  ���v.
039600     MOVE NC"��" TO  ���ԊO�`�F�b�N �x���`�F�b�N �[��`�F�b�N.
039610     MOVE 99999 TO  �������Z��.
039620     MOVE NC"��" TO  ��ԃ`�F�b�N ��H�`�F�b�N �\���J��`�F�b�N.
039630     MOVE 99999 TO  ���É��Z��.
039640     MOVE 99999 TO  �{�p���񋟗�.
039650     MOVE NC"��" TO �������`�F�b�N �Œ藿�`�F�b�N �{�×��`�F�b�N.
039660     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
039670             UNTIL ( ���ʂb�m�s > 5 )
039680         MOVE 99999 TO ���񏈒u��(���ʂb�m�s)
039690     END-PERFORM.
039700     MOVE 999999 TO ���񏈒u�����v.
039710     MOVE 99    TO ��É񐔂P ��㪖@�񐔂P ��㪖@�񐔂P �d�É񐔂P.
039720     MOVE 9999  TO ��ÒP���P ��㪖@���P   ��㪖@���P   �d�×��P.
039730     MOVE 99999 TO ��×��P   ���v�P       ���������v�P.
039740     MOVE 9.9 TO �����������P.
039750     MOVE 99 TO ��É񐔂Q ��㪖@�񐔂Q ��㪖@�񐔂Q �d�É񐔂Q.
039760     MOVE 9999  TO ��ÒP���Q ��㪖@���Q   ��㪖@���Q   �d�×��Q.
039770     MOVE 99999 TO ��×��Q   ���v�Q       ���������v�Q.
039780     MOVE 9.9 TO �����������Q.
039790     MOVE 99 TO ��É񐔂R�W ��㪖@�񐔂R�W ��㪖@�񐔂R�W �d�É񐔂R�W.
039800     MOVE 9999  TO ��ÒP���R�W ��㪖@���R�W   ��㪖@���R�W   �d�×��R�W.
039810     MOVE 99999 TO ��×��R�W ���v�R�W ���������v�R�W �����ʍ����v�R�W.
039820     MOVE 9.9 TO �����������R�W.
039830     MOVE 99 TO �����J�n���R�O �����J�n���R�O.
039840     MOVE 99 TO ��É񐔂R�O ��㪖@�񐔂R�O ��㪖@�񐔂R�O �d�É񐔂R�O.
039850     MOVE 9999  TO ��ÒP���R�O ��㪖@���R�O   ��㪖@���R�O   �d�×��R�O.
039860     MOVE 99999 TO ��×��R�O ���v�R�O ���������v�R�O.
039870     MOVE 9.9 TO �����������R�O.
039880*     MOVE 99 TO ��É񐔂S�T ��㪖@�񐔂S�T ��㪖@�񐔂S�T �d�É񐔂S�T.
039890*     MOVE 9999  TO ��ÒP���S�T ��㪖@���S�T   ��㪖@���S�T   �d�×��S�T.
039900*     MOVE 99999 TO ��×��S�T ���v�S�T ���������v�S�T �����ʍ����v�S�T.
039910*     MOVE 9.9 TO �����������S�T.
039920     MOVE 99 TO �����J�n���S�W �����J�n���S�W.
039930     MOVE 99 TO ��É񐔂S�W ��㪖@�񐔂S�W ��㪖@�񐔂S�W �d�É񐔂S�W.
039940     MOVE 9999  TO ��ÒP���S�W ��㪖@���S�W   ��㪖@���S�W   �d�×��S�W.
039950     MOVE 99999 TO ��×��S�W ���v�S�W ���������v�S�W �����ʍ����v�S�W.
039960     MOVE 9.9 TO �����������S�W.
039970     MOVE 99 TO �����J�n���S�O �����J�n���S�O.
039980     MOVE 99 TO ��É񐔂S�O ��㪖@�񐔂S�O ��㪖@�񐔂S�O �d�É񐔂S�O.
039990     MOVE 9999  TO ��ÒP���S�O ��㪖@���S�O   ��㪖@���S�O   �d�×��S�O.
040000     MOVE 99999 TO ��×��S�O ���v�S�O ���������v�S�O.
040010     MOVE 9.9 TO �����������S�O.
040090     MOVE ALL "X" TO ���ʂT�O ���ʂT�W.
040020*     MOVE NC"���T���ʖڐ�������" TO ���ʂT�K�p.
040030     MOVE ALL NC"�m" TO �K�p�P �K�p�Q.
040040     MOVE ALL "�m" TO �������R���P �������R���Q �������R���R
040050                        �������R���S �������R���T �������R���U.
040060     MOVE 999999 TO ���v �������z.
040070     MOVE 999999 TO �󋋎ҕ��S�z ���������z.
040080*     MOVE ALL "X"    TO �ی��Җ��̂P �ی��Җ��̂Q.
040090     MOVE ALL "X" TO �_���t�ԍ�.
040100     MOVE 99 TO �󗝔N �󗝌� �󗝓�.
040110     MOVE 99 TO �ϔC�N �ϔC�� �ϔC��.
040120     MOVE 999  TO �{�p���X�֔ԍ��P.
040130     MOVE 9999 TO �{�p���X�֔ԍ��Q.
040140     MOVE ALL "X" TO �{�p���Z���P.
040150     MOVE ALL "�m" TO �ڍ��@��.
040160     MOVE ALL "X" TO ��\�҃J�i.
040170     MOVE ALL "�m" TO ��\�Җ�.
040180     MOVE ALL "X" TO �{�p���d�b�ԍ�.
040190*
040200*================================================================*
       �{�p���擾 SECTION.
      *
      *     MOVE SPACE TO �{�p���v.
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
040210******************************************************************
040220 END PROGRAM YHP6425.
040230******************************************************************
