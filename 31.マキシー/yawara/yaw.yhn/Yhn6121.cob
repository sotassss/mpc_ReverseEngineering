000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN6121.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*        ����  ���Z�v�g����i�_+����޳�ޔŁj
000100*         MED = YAW610 YHN6121P
      *
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2020-08-31
000130 DATE-COMPILED.          2020-08-31
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
           SELECT  ���Z�v�g�e      ASSIGN      TO        RECEPTL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  ���Z�|�{�p�a��N��
                                                                ���Z�|���҃R�[�h
                                                                ���Z�|���Z���
                                   ALTERNATE RECORD KEY     IS  ���Z�|���҃R�[�h
                                                                ���Z�|�{�p�a��N��
                                                                ���Z�|���Z���
                                   ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
                                                                ���Z�|�{�p�a��N��
                                                                ���Z�|���҃R�[�h
                                                                ���Z�|���Z���
                                   ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
                                                                ���Z�|���Z���
                                                                ���Z�|�����ی��Ҕԍ�
                                                                ���Z�|���҃R�[�h
                                                                ���Z�|�{�p�a��N��
                                   ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
                                                                ���Z�|�����ی��Ҕԍ�
                                                                ���Z�|���҃R�[�h
                                                                ���Z�|���Z���
                                                                ���Z�|�{�p�a��N��
                                   FILE STATUS              IS  ��ԃL�[
                                   LOCK        MODE         IS  AUTOMATIC.
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
000680     SELECT  �h�c�Ǘ��}�X�^    ASSIGN      TO        IDKANRL
000690                             ORGANIZATION             IS  INDEXED
000700                             ACCESS MODE              IS  DYNAMIC
000710                             RECORD KEY               IS  �h�c�ǁ|�h�c�敪
000720                                                          �h�c�ǁ|�{�p���ԍ�
000730                                                          �h�c�ǁ|�ی����
000740                                                          �h�c�ǁ|�ی��Ҕԍ�
000750                             ALTERNATE RECORD KEY     IS  �h�c�ǁ|�{�p�h�c�ԍ�
000760                                                          �h�c�ǁ|�h�c�敪
000770                                                          �h�c�ǁ|�{�p���ԍ�
000780                                                          �h�c�ǁ|�ی����
000790                                                          �h�c�ǁ|�ی��Ҕԍ�
000800                             FILE STATUS              IS  ��ԃL�[
000810                             LOCK        MODE         IS  AUTOMATIC.
000820     SELECT  �o�߃}�X�^      ASSIGN      TO        KEIKAL
000830                             ORGANIZATION             IS  INDEXED
000840                             ACCESS MODE              IS  DYNAMIC
000850                             RECORD KEY               IS  �o�|�敪�R�[�h
000860                                                          �o�|�o�߃R�[�h
000870                             FILE STATUS              IS  ��ԃL�[
000880                             LOCK        MODE         IS  AUTOMATIC.
000890     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000900                             ORGANIZATION             IS  INDEXED
000910                             ACCESS MODE              IS  DYNAMIC
000920                             RECORD KEY               IS  ��|�{�p�a��N��
000930                                                          ��|���҃R�[�h
000940                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000950                                                          ��|���҃J�i
000960                                                          ��|���҃R�[�h
000970                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000980                                                          ��|�{�p�a��N��
000990                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001000                                                          ��|�ی����
001010                                                          ��|�ی��Ҕԍ�
001020                                                          ��|���҃R�[�h
001030                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001040                                                          ��|������
001050                                                          ��|��p���S�Ҕԍ�
001060                                                          ��|���҃R�[�h
001070                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001080                                                          ��|�������
001090                                                          ��|��p���S�Ҕԍ�����
001100                                                          ��|���҃R�[�h
001110                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
001120                                                          ��|�{�p�a��N��
001130                                                          ��|���҃R�[�h
001140                             FILE STATUS              IS  ��ԃL�[
001150                             LOCK        MODE         IS  AUTOMATIC.
001160     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
001170                             ORGANIZATION             IS  INDEXED
001180                             ACCESS MODE              IS  DYNAMIC
001190                             RECORD KEY               IS  �{�L�|�{�p�a��N����
001200                                                          �{�L�|���҃R�[�h
001210                             ALTERNATE RECORD KEY     IS  �{�L�|���҃R�[�h
001220                                                          �{�L�|�{�p�a��N����
001230                             FILE STATUS              IS  ��ԃL�[
001240                             LOCK        MODE         IS  AUTOMATIC.
001250     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
001260                             ORGANIZATION             IS  INDEXED
001270                             ACCESS MODE              IS  DYNAMIC
001280                             RECORD KEY               IS  ���|�{�p�a��N��
001290                                                          ���|���҃R�[�h
001300                             ALTERNATE RECORD KEY     IS  ���|���҃R�[�h
001310                                                          ���|�{�p�a��N��
001320                             FILE STATUS              IS  ��ԃL�[
001330                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  ���������e      ASSIGN      TO        HUGEINL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  �����|�敪�R�[�h
001380                                                          �����|���������R�[�h
001390                             FILE STATUS              IS  ��ԃL�[
001400                             LOCK        MODE         IS  AUTOMATIC.
001500     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
001510                             ORGANIZATION             IS  INDEXED
001520                             ACCESS MODE              IS  DYNAMIC
001530                             RECORD KEY               IS  �s�|������
001540                                                          �s�|�s�����ԍ�
001550                             ALTERNATE RECORD KEY     IS  �s�|������
001560                                                          �s�|�s��������
001570                                                          �s�|�s�����ԍ�
001580                             FILE STATUS              IS  ��ԃL�[
001590                             LOCK        MODE         IS  AUTOMATIC.
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
001080* ���Z���я��p
001081     SELECT  ��ƃt�@�C���R  ASSIGN      TO  "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001100                             ORGANIZATION             IS  INDEXED
001110                             ACCESS                   IS  DYNAMIC
001120                             RECORD      KEY          IS  ��R�|�{�p�a��N��
001130                                                          ��R�|���҃R�[�h
001140                                                          ��R�|�ی����
001150                             FILE        STATUS       IS  ��ԃL�[
001160                             LOCK        MODE         IS  AUTOMATIC.
      */���я��p�@���Z
000108     SELECT  ��ƃt�@�C���T  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4315L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  ��T�|���҃R�[�h
000912                                                          ��T�|�{�p�a��N��
000912                                                          ��T�|�ی����
000134                             ALTERNATE RECORD KEY     IS  ��T�|�U������
                                                                ��T�|�V������
                                                                ��T�|���ҏ���
000980                             FILE        STATUS       IS  ��ԃL�[
000990                             LOCK        MODE         IS  AUTOMATIC.
001830     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
001840                             SYMBOLIC    DESTINATION  IS "PRT"
001850                             FORMAT                   IS  ��`�̖��o
001860                             GROUP                    IS  ���ڌQ���o
001870                             PROCESSING  MODE         IS  ������ʂo
001880                             UNIT        CONTROL      IS  �g������o
001890                             FILE        STATUS       IS  �ʒm���o.
001900******************************************************************
001910*                      DATA DIVISION                             *
001920******************************************************************
001930 DATA                    DIVISION.
001940 FILE                    SECTION.
001950*                           �m�q�k��  �R�Q�O�n
001960 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001970     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001980*                           �m�q�k��  �P�Q�W�n
001990 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002000     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002010*                           �m�q�k��  �P�Q�W�n
002020 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
002030     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
002070*                           �m�q�k��  �Q�T�U�n
002080 FD  ������}�X�^      BLOCK   CONTAINS   1   RECORDS.
002090     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002100*                           �m�q�k��  �P�Q�W�n
002110 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
002120     COPY SEJOHO          OF  XFDLIB  JOINING   �{��   AS  PREFIX.
002130*                           �m�q�k��  �P�Q�W�n
002140 FD  �h�c�Ǘ��}�X�^      BLOCK   CONTAINS   1   RECORDS.
002150     COPY IDKANR          OF  XFDLIB  JOINING   �h�c��   AS  PREFIX.
002160*                           �m�q�k��  �P�Q�W�n
002170 FD  �o�߃}�X�^          BLOCK   CONTAINS   1   RECORDS.
002180     COPY KEIKA           OF  XFDLIB  JOINING   �o   AS  PREFIX.
002190*                           �m�q�k��  �R�Q�O�n
002200 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
002210     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002220*                           �m�q�k��  �Q�T�U�n
002230 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
002240     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
002250*                           �m�q�k��  �P�Q�W�n
002260 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
002270     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002280*                           �m�q�k��  �P�Q�W�n
002290 FD  ���������e          BLOCK   CONTAINS   1   RECORDS.
002300     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
002250*                           �m�q�k��  �Q�T�U�n
002260 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002270     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
002470*                           �m�q�k��  �U�S�O�n
002480 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
002490     COPY KAIJOHO         OF  XFDLIB  JOINING   ���   AS  PREFIX.
002400**
001740 FD  ��ƃt�@�C���R RECORD  CONTAINS 32 CHARACTERS.
001750 01  ��R�|���R�[�h.
001760     03  ��R�|���R�[�h�L�[.
001770         05  ��R�|�{�p�a��N��.
001780             07  ��R�|�{�p�a��            PIC 9.
001790             07  ��R�|�{�p�N              PIC 9(2).
001800             07  ��R�|�{�p��              PIC 9(2).
001810         05  ��R�|���҃R�[�h.
001820             07 ��R�|���Ҕԍ�             PIC 9(6).
001830             07 ��R�|�}��                 PIC X(1).
001840         05  ��R�|�ی����                PIC 9(2).
001850     03  ��R�|���R�[�h�f�[�^.
001860         05  ��R�|����                    PIC 9(4).
001870         05  FILLER                        PIC X(14).
000174*                           �m�q�k��  �R�Q�n
000175 FD  ��ƃt�@�C���T RECORD  CONTAINS 32 CHARACTERS.
000176 01  ��T�|���R�[�h.
000177     03  ��T�|���R�[�h�L�[.
001310         05  ��T�|���҃R�[�h.
001320             07 ��T�|���Ҕԍ�               PIC 9(6).
001330             07 ��T�|�}��                   PIC X(1).
001340         05  ��T�|�{�p�a��N��.
001350             07  ��T�|�{�p�a��              PIC 9.
001360             07  ��T�|�{�p�N                PIC 9(2).
001370             07  ��T�|�{�p��                PIC 9(2).
001400         05  ��T�|�ی����                  PIC 9(2).
000188     03  ��T�|���R�[�h�f�[�^.
001261         05  ��T�|�U������                  PIC 9(3).
001261         05  ��T�|�V������                  PIC 9(3).
001261         05  ��T�|���ҏ���                  PIC 9(3).
001261         05  ��T�|���ރR�[�h                PIC 9(1).
000201         05  FILLER                          PIC X(8).
002550*
002560 FD  ����t�@�C��.
002570     COPY YHN6121P        OF  XMDLIB.
002580*----------------------------------------------------------------*
002590******************************************************************
002600*                WORKING-STORAGE SECTION                         *
002610******************************************************************
002620 WORKING-STORAGE         SECTION.
002630 01 �L�[����                           PIC X     VALUE SPACE.
002640 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
002650 01 �I���t���O                         PIC X(3)  VALUE SPACE.
002660 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
004581 01 �I���t���O�S                       PIC X(3) VALUE SPACE.
002670 01 �t�@�C����                         PIC N(6)  VALUE SPACE.
002680 01 ���Z�v�g�o�f�v                     PIC X(8)  VALUE SPACE.
002690 01 �O�a��v                           PIC 9     VALUE ZERO.
001363 01 �S�p��                           PIC X(2)  VALUE X"8140".
001364 01 ���p��                           PIC X(2)  VALUE X"2020".
002710 01 ���Z�v�g��ނv                     PIC X(4)  VALUE SPACE.
002640 01 �E�o�t���O                         PIC X(3)  VALUE SPACE.
005190 01 �p�������ڂQ�v                     PIC X(22) VALUE SPACE.
005150 01 �p�������ڂv.
005160   03 �p�������ڂw�v                   PIC X(22) VALUE SPACE.
003630 01 �����b�m�s                         PIC 9(2) VALUE ZERO.
       01 �����ϊ��v.
          03 �����ϊ��v�q                    PIC 9(8) VALUE ZERO.
002700*
002740*--- ����}�X�^�ޔ� ---*
002750 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
002760 01 �ő�o�^���v                       PIC 9(1)  VALUE ZERO.
002770 01 �����A���o�^�v                     PIC 9(1)  VALUE ZERO.
002780 01 �x���t���O                         PIC X(3)  VALUE SPACE.
002790 01 �x���񐔂v                         PIC 9(4)  VALUE ZERO.
002800 01 �x���b�m�s                         PIC 9(5)  VALUE ZERO.
002810*
002820** ���������E�������R����敪�p
002830 01 ������������敪�v                 PIC 9     VALUE ZERO.
002840 01 �������R����敪�v                 PIC 9     VALUE ZERO.
002140*
002860** ���Z���i�̓��t�敪�p (0:�ŏI�ʉ@���A1:�������A9:�󎚂Ȃ�)
002870 01 ���Z�v�g���t�敪�v                 PIC 9     VALUE ZERO.
002880 01 ���Z�v�g���ғ��t�敪�v             PIC 9     VALUE ZERO.
002890*
002900*--- �J�E���^ ---*
002910 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
002920 01 �J�E���^�Q                         PIC 9(2)  VALUE ZERO.
002930 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002940*
002950*--- �X�֔ԍ��ҏW�p ---*
002960 01 �X�֔ԍ��ҏW�v.
002970    03 FILLER                          PIC X(2)  VALUE "��".
002980    03 �X�֔ԍ��ҏW�P�v                PIC X(3)  VALUE SPACE.
002990    03 FILLER                          PIC X(1)  VALUE "-".
003000    03 �X�֔ԍ��ҏW�Q�v                PIC X(4)  VALUE SPACE.
003010*
003020*--- �����f�[�^�擾�p ---*
003030 01 �������̂v                         PIC N(6)  VALUE SPACE.
003040 01 ���ʖ��̂v                         PIC N(12) VALUE SPACE.
003050 01 ���ʒ��v                           PIC 9(2)  VALUE 1.
003060 01 �o�ߕ��ʂv                         PIC N(1)  VALUE SPACE.
003070*
003080** �}�Ԕ���p
003090 01 �J�n�f�Ó��蓮�敪�v               PIC 9     VALUE ZERO.
003100*
003110* ������������敪
003120 01 ���Z������������敪�v             PIC 9     VALUE ZERO.
004440 01 ���Z�������R����敪�v             PIC 9    VALUE ZERO.
003130*
003140*--- �o�ߗ��̕ҏW�p ---*
003150 01 �o�ߗ��̕ҏW�s�a�k.
003160    03 �o�ߗ��̕ҏW�s                  PIC N(10) VALUE SPACE OCCURS 5.
003170 01 �o�ߗ��̕ҏW�v                     PIC N(10) VALUE SPACE.
003180*
003190*--- �{�p�L�^�擾�p ---*
003200 01 �����Č��t���O                     PIC X(3)  VALUE SPACE.
003210 01 �O���t���O                         PIC X(3)  VALUE SPACE.
003220*
003230 01 �I���N�����v�s.
002980    03 �I���a��v�s                    PIC 9     VALUE ZERO.
003240    03 �I���N�v�s                      PIC 9(2)  VALUE ZERO.
003250    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
003260    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
003270** �O������p
003280 01 �J�n�N�����Q�v.
003290    03 �J�n�a��Q�v                    PIC 9(1)  VALUE ZERO.
003300    03 �J�n�N�Q�v                      PIC 9(2)  VALUE ZERO.
003310    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
003320    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
003330    03 �J�n����N�v                    PIC S9(4) VALUE ZERO.
003340 01 �I���N�����Q�v.
003350    03 �I���a��Q�v                    PIC 9(1)  VALUE ZERO.
003360    03 �I���N�Q�v                      PIC 9(2)  VALUE ZERO.
003370    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
003380    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
003390    03 �I������N�v                    PIC S9(4) VALUE ZERO.
003400 01 �v�Z�N�����v.
003410    03 �v�Z�a��v                      PIC 9(1)  VALUE ZERO.
003420    03 �v�Z�N�v                        PIC S9(2) VALUE ZERO.
003430    03 �v�Z���v                        PIC S9(2) VALUE ZERO.
003440    03 �v�Z���v                        PIC S9(2) VALUE ZERO.
003450*
003460*--- �������ޔ�p ---*
003470 01 �����t���O                         PIC X(3)  VALUE SPACE.
003480*
003490 01 �����N�����v�s.
003500    03 �����a��v�s                    PIC 9     VALUE ZERO.
003510    03 �����N�v�s                      PIC 9(2)  VALUE ZERO.
003520    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003530    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003540*
003550*--- �������Z�����p ---*
003560 01 �������Z�v�s.
003570    03 �������Z�J�E���g                PIC 9     VALUE ZERO.
003580    03 �ԍ��J�E���^                    PIC 9     VALUE ZERO.
003590    03 �������Z�W�c�v�s  OCCURS 3.
003600       05 �������Z�敪�v�s             PIC 9     VALUE ZERO.
003610       05 �������Z���v�s               PIC 9(2)  VALUE ZERO.
003620       05 �������Z���v�s               PIC 9(2)  VALUE ZERO.
003630    03 �������Z�W�c�m�v  OCCURS 3.
003640       05 ���Z��؂v                   PIC N(1)  VALUE SPACE.
003650       05 ���Z���e�v                   PIC N(3)  VALUE SPACE.
003660       05 �������Z���m�v�P             PIC N(1)  VALUE SPACE.
003670       05 �������Z���m�v�Q             PIC N(1)  VALUE SPACE.
003680       05 ���Œ�v                     PIC N(1)  VALUE SPACE.
003690       05 �������Z���m�v�P             PIC N(1)  VALUE SPACE.
003700       05 �������Z���m�v�Q             PIC N(1)  VALUE SPACE.
003710       05 ���Œ�v                     PIC N(1)  VALUE SPACE.
003720    03 �������Z�����P�v                PIC N(10) VALUE SPACE.
003730    03 �������Z�����Q�v                PIC N(10) VALUE SPACE.
003740    03 �������Z�����R�v                PIC N(10) VALUE SPACE.
003070    03 �������Z��؂v                  PIC X     VALUE SPACE.
003080    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003090    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003630*
003640* ���ϔԍ��p
003650 01 ���ϘA�ԍ��W�c�v.
003660    03 ���ϘA�ԍ����v                  PIC X(14)  VALUE SPACE.
003670    03 ���ϘA�ԍ����m�v REDEFINES  ���ϘA�ԍ����v  PIC N(7).
003680    03 ���ϘA�ԍ��v                    PIC X(6)  VALUE SPACE.
003690    03 ���ϘA�ԍ��P�ʂv                PIC X(2)  VALUE SPACE.
003700    03 ���ϘA�ԍ��P�ʂm�v REDEFINES  ���ϘA�ԍ��P�ʂv  PIC N.
003710*
003720* ���q���ԍ��p
003730 01 ���q���ԍ��W�c�v.
003740    03 ���q���ԍ����v                  PIC X(8)  VALUE SPACE.
003750    03 ���q���ԍ����m�v REDEFINES  ���q���ԍ����v  PIC N(4).
003760    03 ���q���ԍ��v                    PIC X(6)  VALUE SPACE.
003770    03 ���q���ԍ��P�ʂv                PIC X(2)  VALUE SPACE.
003780    03 ���q���ԍ��P�ʂm�v REDEFINES  ���q���ԍ��P�ʂv  PIC N.
003750*
003760** ���������{��ϊ�
003770 01 �����v                             PIC 9(2).
003780 01 �����q REDEFINES �����v.
003790    03 �����v�P                        PIC X(1).
003800    03 �����v�Q                        PIC X(1).
003810*
003820 01 �����ԍ��v                         PIC 9.
003830 01 �����ԍ��q REDEFINES �����ԍ��v.
003840    03 �����ԍ��v�P                    PIC X.
003850*
003860 01 �S�p�����ԍ��v                     PIC N.
003870 01 �S�p�����ԍ��q REDEFINES �S�p�����ԍ��v.
003880    03 �S�p�����ԍ��v�P                PIC X(2).
003890*
003900*--- ���������p ---*
003910 01 ���������Œ�v                     PIC X(50)
003920     VALUE "�Ɩ��ЊQ�A�ʋ΍ЊQ���͑�O�ҍs�׈ȊO�̌����ɂ��B".
003930*
003940 01 ���������v�s.
003450    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
003460    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
003470    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
003480    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
003490    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
004000    03 ���������i���o�[�v�s.
004010       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
004020    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
004030 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
004040 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
004050 01 ���������s�a�k.
004060    03 ���������R�[�h�s�a�k            OCCURS 9.
004070       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
004080       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
004090       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
004100 01 �����������e�v.
004110    03 �����������e�����v              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 �����������e�����w�v.
003630       05 �����������e�P�w�v           PIC X(80)  VALUE SPACE.
003640       05 �����������e�Q�w�v           PIC X(80)  VALUE SPACE.
003640       05 �����������e�R�w�v           PIC X(80)  VALUE SPACE.
003650       05 �����������e�S�w�v           PIC X(78)  VALUE SPACE.
       01 ���������P���v.
          03 ���������P���v�q                OCCURS 7.
             05 ���������P���v�o             PIC X(100) VALUE SPACE.
004170*
004180*--- �ϔC�N�����p ---*
004190 01 �󗝔N�����v.
007350    03 �󗝘a��v                      PIC 9     VALUE ZERO.
004200    03 �󗝔N�v                        PIC 9(2)  VALUE ZERO.
004210    03 �󗝌��v                        PIC 9(2)  VALUE ZERO.
004220    03 �󗝓��v                        PIC 9(2)  VALUE ZERO.
004230 01 �ŏI�ʉ@�N�����v.
007390    03 �ŏI�ʉ@�a��v                  PIC 9     VALUE ZERO.
004240    03 �ŏI�ʉ@�N�v                    PIC 9(2)  VALUE ZERO.
004250    03 �ŏI�ʉ@���v                    PIC 9(2)  VALUE ZERO.
004260    03 �ŏI�ʉ@���v                    PIC 9(2)  VALUE ZERO.
004270** �������p
004280 01 �{�p����N�v                       PIC 9(4)  VALUE ZERO.
004290 01 ���v                               PIC 9(3)  VALUE ZERO.
004300 01 �]�v                               PIC 9(3)  VALUE ZERO.
004310*
004320*--- ������s�p ---*
004330 01 ��s���x�X���v.
004340    03 ��s���x�X���P�v                PIC X(26) VALUE SPACE.
004350    03 ��s���x�X���Q�v                PIC X(34) VALUE SPACE.
004360 01 �a����ʃR�����g�v                 PIC X(4)  VALUE SPACE.
       01 �x���@�ւv.
          03 ���Z�@�֖��v.
             05 ���Z�@�֖��P�v            PIC X(8)  VALUE SPACE.
             05 ���Z�@�֖��Q�v            PIC X(8)  VALUE SPACE.
             05 ���Z�@�֖��R�v            PIC X(8)  VALUE SPACE.
             05 ���Z�@�֖��S�v            PIC X(8)  VALUE SPACE.
             05 ���Z�@�֖��T�v            PIC X(8)  VALUE SPACE.
          03 �x�X���v.
             05 �x�X���P�v                PIC X(12) VALUE SPACE.
             05 �x�X���Q�v                PIC X(12) VALUE SPACE.
             05 �x�X���R�v                PIC X(12) VALUE SPACE.
             05 �x�X���S�v                PIC X(12) VALUE SPACE.
          03 �U���`�F�b�N�v               PIC N(1)  VALUE SPACE.
          03 ���ʃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
          03 �����`�F�b�N�v               PIC N(1)  VALUE SPACE.
          03 ��s�`�F�b�N�v               PIC N(1)  VALUE SPACE.
          03 ���Ƀ`�F�b�N�v               PIC N(1)  VALUE SPACE.
          03 �_���`�F�b�N�v               PIC N(1)  VALUE SPACE.
          03 �{�X�`�F�b�N�v               PIC N(1)  VALUE SPACE.
          03 �x�X�`�F�b�N�v               PIC N(1)  VALUE SPACE.
          03 �{�x���`�F�b�N�v             PIC N(1)  VALUE SPACE.
004370*
004380*-- ���Z�E�v�p( N(38)�Œ�j--*
004390 01 �����̌o�߂v.
004400    03 �����̌o�ߍs�v                  PIC X(76) OCCURS 2 VALUE SPACE.
004410 01 �����̌o�߂m�v REDEFINES �����̌o�߂v.
004420    03 �����̌o�ߍs�m�v                PIC N(38) OCCURS 2.
004430*
004440*--- ���t�����p ---*
004450 01 ���S�����v                         PIC 9(2)  VALUE ZERO.
004460 01 ���t�����v                         PIC 9(2)  VALUE ZERO.
004470 01 ���S���v                           PIC 9(3)  VALUE ZERO.
004450 01 �����v                             PIC X(2)  VALUE SPACE.
004480*
004490*--- �����於�̕ҏW�p ---*
004500 01 �����於�̂s�a�k.
004510    03 �����於�̂s                    PIC X(1)  OCCURS 40.
004520 01 ���ʒu�v                           PIC S9(2) VALUE ZERO.
003750*
003751** �������Z�܂Ƃߗp
003752 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
003753*
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
       01 �������q�b�l                       PIC X(200) VALUE SPACE.
       01 �^����Âb�l                       PIC X(68)  VALUE SPACE.
004530*
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
005210*
004540****************
004550* �A�����ڑҔ� *
004560****************
004570*    ************
004580*    * ����L�[ *
004590*    ************
004600 01 �Ώۃf�[�^�v�q.
004610    03 �{�p�a��N���v�q.
004620       05 �{�p�a��v�q                 PIC 9(1)  VALUE ZERO.
004630       05 �{�p�N�v�q                   PIC 9(2)  VALUE ZERO.
004640       05 �{�p���v�q                   PIC 9(2)  VALUE ZERO.
004650    03 �ی���ʂv�q                    PIC 9(2)  VALUE ZERO.
004660    03 �ی��Ҕԍ��v�q                  PIC X(10) VALUE SPACE.
004670    03 �����ʂv�q                    PIC 9(2)  VALUE ZERO.
004680    03 ��p���S�Ҕԍ��v�q              PIC X(10) VALUE SPACE.
004690    03 ������ʂv�q                    PIC 9(2)  VALUE ZERO.
004700    03 ��p���S�Ҕԍ������v�q          PIC X(10) VALUE SPACE.
004710    03 �{�l�Ƒ��敪�v�q                PIC 9(1)  VALUE ZERO.
004720    03 ���҃J�i�v�q                    PIC X(20) VALUE SPACE.
004730    03 ���҃R�[�h�v�q.
004740       05 ���Ҕԍ��v�q                 PIC 9(6)  VALUE ZERO.
004750       05 �}�Ԃv�q                     PIC X(1)  VALUE SPACE.
004760*    ************
004770*    * ������� *
004780*    ************
004790*--- �����̗��� ---*
004800 01 �����P�v�q.
004810    03 �����v�q.
004820       05 ���S�����v�q                 PIC 9(3)  VALUE ZERO.
004830       05 �������v�q                   PIC 9(5)  VALUE ZERO.
004840       05 �������Z���v�q               PIC 9(5)  VALUE ZERO.
          03 ���k���v�q                      PIC 9(4)  VALUE ZERO.
004850    03 �Č����v�q                      PIC 9(5)  VALUE ZERO.
004860    03 ���Âv�q.
004870       05 ���Ë����v�q                 PIC 9(2)V9 VALUE ZERO.
004880       05 ���É񐔂v�q                 PIC 9(2)  VALUE ZERO.
004890       05 ���×��v�q                   PIC 9(5)  VALUE ZERO.
004900       05 ���É��Z���v�q               PIC 9(5)  VALUE ZERO.
004910    03 �������q���Z���v�q              PIC 9(5)  VALUE ZERO.
004920    03 �{�p���񋟗��v�q              PIC 9(5)  VALUE ZERO.
004930    03 ���v�v�q                        PIC 9(6)  VALUE ZERO.
004940    03 �ꕔ���S���v�q                  PIC 9(6)  VALUE ZERO.
004950    03 �������z�v�q                    PIC 9(6)  VALUE ZERO.
004960    03 ���t�����v�q                    PIC 9(1)  VALUE ZERO.
004970    03 �󋋎ҕ��S�z�v�q                PIC 9(6)  VALUE ZERO.
004980    03 �����������z�v�q                PIC 9(6)  VALUE ZERO.
004990*
005000*--- �������ʖ��̗��� ---*
005010 01 �����Q�v�q.
005020   03 ���񏈒u�v�q    OCCURS   9.
005030      05 ���񏈒u���v�q                PIC 9(5)  VALUE ZERO.
005040*
005050*--- �������̗��� ---*
005060 01 �����R�v�q.
005070**********
005080* �P���� *
005090**********
005100   03 ���ʂP�v�q.
005110      05 ��ÂP�v�q.
005120         07 ��ÒP���P�v�q             PIC 9(4)  VALUE ZERO.
005130         07 ��É񐔂P�v�q             PIC 9(2)  VALUE ZERO.
005140         07 ��×��P�v�q               PIC 9(5)  VALUE ZERO.
005150      05 ��㪖@�P�v�q.
005160         07 ��㪖@�񐔂P�v�q           PIC 9(2)  VALUE ZERO.
005170         07 ��㪖@���P�v�q             PIC 9(4)  VALUE ZERO.
005180      05 ��㪖@�P�v�q.
005190         07 ��㪖@�񐔂P�v�q           PIC 9(2)  VALUE ZERO.
005200         07 ��㪖@���P�v�q             PIC 9(4)  VALUE ZERO.
005210      05 �d�ÂP�v�q.
005220         07 �d�É񐔂P�v�q             PIC 9(2)  VALUE ZERO.
005230         07 �d�×��P�v�q               PIC 9(4)  VALUE ZERO.
005240      05 ���v�P�v�q                    PIC 9(6)  VALUE ZERO.
005250      05 �����������P�v�q              PIC 9(3)  VALUE ZERO.
005260      05 ���������v�P�v�q              PIC 9(6)  VALUE ZERO.
005270**********
005280* �Q���� *
005290**********
005300   03 ���ʂQ�v�q.
005310      05 ��ÂQ�v�q.
005320         07 ��ÒP���Q�v�q             PIC 9(4)  VALUE ZERO.
005330         07 ��É񐔂Q�v�q             PIC 9(2)  VALUE ZERO.
005340         07 ��×��Q�v�q               PIC 9(5)  VALUE ZERO.
005350      05 ��㪖@�Q�v�q.
005360         07 ��㪖@�񐔂Q�v�q           PIC 9(2)  VALUE ZERO.
005370         07 ��㪖@���Q�v�q             PIC 9(4)  VALUE ZERO.
005380      05 ��㪖@�Q�v�q.
005390         07 ��㪖@�񐔂Q�v�q           PIC 9(2)  VALUE ZERO.
005400         07 ��㪖@���Q�v�q             PIC 9(4)  VALUE ZERO.
005410      05 �d�ÂQ�v�q.
005420         07 �d�É񐔂Q�v�q             PIC 9(2)  VALUE ZERO.
005430         07 �d�×��Q�v�q               PIC 9(4)  VALUE ZERO.
005440      05 ���v�Q�v�q                    PIC 9(6)  VALUE ZERO.
005450      05 �����������Q�v�q              PIC 9(3)  VALUE ZERO.
005460      05 ���������v�Q�v�q              PIC 9(6)  VALUE ZERO.
005470******************
005480* �R���ʁ^�W�� *
005490******************
005500   03 ���ʂR�W�v�q.
005510      05 ��ÂR�W�v�q.
005520         07 ��ÒP���R�W�v�q           PIC 9(4)  VALUE ZERO.
005530         07 ��É񐔂R�W�v�q           PIC 9(2)  VALUE ZERO.
005540         07 ��×��R�W�v�q             PIC 9(5)  VALUE ZERO.
005550      05 ��㪖@�R�W�v�q.
005560         07 ��㪖@�񐔂R�W�v�q         PIC 9(2)  VALUE ZERO.
005570         07 ��㪖@���R�W�v�q           PIC 9(4)  VALUE ZERO.
005580      05 ��㪖@�R�W�v�q.
005590         07 ��㪖@�񐔂R�W�v�q         PIC 9(2)  VALUE ZERO.
005600         07 ��㪖@���R�W�v�q           PIC 9(4)  VALUE ZERO.
005610      05 �d�ÂR�W�v�q.
005620         07 �d�É񐔂R�W�v�q           PIC 9(2)  VALUE ZERO.
005630         07 �d�×��R�W�v�q             PIC 9(4)  VALUE ZERO.
005640      05 ���v�R�W�v�q                  PIC 9(6)  VALUE ZERO.
005650      05 �����ʍ����v�R�W�v�q          PIC 9(6)  VALUE ZERO.
005660      05 �����������R�W�v�q            PIC 9(3)  VALUE ZERO.
005670      05 ���������v�R�W�v�q            PIC 9(6)  VALUE ZERO.
005680******************
005690* �R���ʁ^�P�O�� *
005700******************
005710   03 ���ʂR�O�v�q.
005720      05 �����J�n�����R�O�v�q.
005730         07 �����J�n���R�O�v�q         PIC 9(2)  VALUE ZERO.
005740         07 �����J�n���R�O�v�q         PIC 9(2)  VALUE ZERO.
005750      05 ��ÂR�O�v�q.
005760         07 ��ÒP���R�O�v�q           PIC 9(4)  VALUE ZERO.
005770         07 ��É񐔂R�O�v�q           PIC 9(2)  VALUE ZERO.
005780         07 ��×��R�O�v�q             PIC 9(5)  VALUE ZERO.
005790      05 ��㪖@�R�O�v�q.
005800         07 ��㪖@�񐔂R�O�v�q         PIC 9(2)  VALUE ZERO.
005810         07 ��㪖@���R�O�v�q           PIC 9(4)  VALUE ZERO.
005820      05 ��㪖@�R�O�v�q.
005830         07 ��㪖@�񐔂R�O�v�q         PIC 9(2)  VALUE ZERO.
005840         07 ��㪖@���R�O�v�q           PIC 9(4)  VALUE ZERO.
005850      05 �d�ÂR�O�v�q.
005860         07 �d�É񐔂R�O�v�q           PIC 9(2)  VALUE ZERO.
005870         07 �d�×��R�O�v�q             PIC 9(4)  VALUE ZERO.
005880      05 ���v�R�O�v�q                  PIC 9(6)  VALUE ZERO.
005890      05 �����������R�O�v�q            PIC 9(3)  VALUE ZERO.
005900      05 ���������v�R�O�v�q            PIC 9(6)  VALUE ZERO.
005910****************
005920* �S���ʁ^�T�� *
005930****************
005940   03 ���ʂS�T�v�q.
005950      05 ��ÂS�T�v�q.
005960         07 ��ÒP���S�T�v�q           PIC 9(4)  VALUE ZERO.
005970         07 ��É񐔂S�T�v�q           PIC 9(2)  VALUE ZERO.
005980         07 ��×��S�T�v�q             PIC 9(5)  VALUE ZERO.
005990      05 ��㪖@�S�T�v�q.
006000         07 ��㪖@�񐔂S�T�v�q         PIC 9(2)  VALUE ZERO.
006010         07 ��㪖@���S�T�v�q           PIC 9(4)  VALUE ZERO.
006020      05 ��㪖@�S�T�v�q.
006030         07 ��㪖@�񐔂S�T�v�q         PIC 9(2)  VALUE ZERO.
006040         07 ��㪖@���S�T�v�q           PIC 9(4)  VALUE ZERO.
006050      05 �d�ÂS�T�v�q.
006060         07 �d�É񐔂S�T�v�q           PIC 9(2)  VALUE ZERO.
006070         07 �d�×��S�T�v�q             PIC 9(4)  VALUE ZERO.
006080      05 ���v�S�T�v�q                  PIC 9(6)  VALUE ZERO.
006090      05 �����ʍ����v�S�T�v�q          PIC 9(6)  VALUE ZERO.
006100      05 �����������S�T�v�q            PIC 9(3)  VALUE ZERO.
006110      05 ���������v�S�T�v�q            PIC 9(6)  VALUE ZERO.
006120****************
006130* �S���ʁ^�W�� *
006140****************
006150   03 ���ʂS�W�v�q.
006160      05 �����J�n�����S�W�v�q.
006170         07 �����J�n���S�W�v�q         PIC 9(2)  VALUE ZERO.
006180         07 �����J�n���S�W�v�q         PIC 9(2)  VALUE ZERO.
006190      05 ��ÂS�W�v�q.
006200         07 ��ÒP���S�W�v�q           PIC 9(4)  VALUE ZERO.
006210         07 ��É񐔂S�W�v�q           PIC 9(2)  VALUE ZERO.
006220         07 ��×��S�W�v�q             PIC 9(5)  VALUE ZERO.
006230      05 ��㪖@�S�W�v�q.
006240         07 ��㪖@�񐔂S�W�v�q         PIC 9(2)  VALUE ZERO.
006250         07 ��㪖@���S�W�v�q           PIC 9(4)  VALUE ZERO.
006260      05 ��㪖@�S�W�v�q.
006270         07 ��㪖@�񐔂S�W�v�q         PIC 9(2)  VALUE ZERO.
006280         07 ��㪖@���S�W�v�q           PIC 9(4)  VALUE ZERO.
006290      05 �d�ÂS�W�v�q.
006300         07 �d�É񐔂S�W�v�q           PIC 9(2)  VALUE ZERO.
006310         07 �d�×��S�W�v�q             PIC 9(4)  VALUE ZERO.
006320      05 ���v�S�W�v�q                  PIC 9(6)  VALUE ZERO.
006330      05 �����ʍ����v�S�W�v�q          PIC 9(6)  VALUE ZERO.
006340      05 �����������S�W�v�q            PIC 9(3)  VALUE ZERO.
006350      05 ���������v�S�W�v�q            PIC 9(6)  VALUE ZERO.
006360******************
006370* �S���ʁ^�P�O�� *
006380******************
006390   03 ���ʂS�O�v�q.
006400      05 �����J�n�����S�O�v�q.
006410         07 �����J�n���S�O�v�q         PIC 9(2)  VALUE ZERO.
006420         07 �����J�n���S�O�v�q         PIC 9(2)  VALUE ZERO.
006430      05 ��ÂS�O�v�q.
006440         07 ��ÒP���S�O�v�q           PIC 9(4)  VALUE ZERO.
006450         07 ��É񐔂S�O�v�q           PIC 9(2)  VALUE ZERO.
006460         07 ��×��S�O�v�q             PIC 9(5)  VALUE ZERO.
006470      05 ��㪖@�S�O�v�q.
006480         07 ��㪖@�񐔂S�O�v�q         PIC 9(2)  VALUE ZERO.
006490         07 ��㪖@���S�O�v�q           PIC 9(4)  VALUE ZERO.
006500      05 ��㪖@�S�O�v�q.
006510         07 ��㪖@�񐔂S�O�v�q         PIC 9(2)  VALUE ZERO.
006520         07 ��㪖@���S�O�v�q           PIC 9(4)  VALUE ZERO.
006530      05 �d�ÂS�O�v�q.
006540         07 �d�É񐔂S�O�v�q           PIC 9(2)  VALUE ZERO.
006550         07 �d�×��S�O�v�q             PIC 9(4)  VALUE ZERO.
006560      05 ���v�S�O�v�q                  PIC 9(6)  VALUE ZERO.
006570      05 �����������S�O�v�q            PIC 9(3)  VALUE ZERO.
006580      05 ���������v�S�O�v�q            PIC 9(6)  VALUE ZERO.
006590********************
006600* �T���ʁ^�Q�D�T�� *
006610********************
006620   03 ���ʂT�Q�v�q.
006630      05 ��ÂT�Q�v�q.
006640         07 ��ÒP���T�Q�v�q           PIC 9(4)  VALUE ZERO.
006650         07 ��É񐔂T�Q�v�q           PIC 9(2)  VALUE ZERO.
006660         07 ��×��T�Q�v�q             PIC 9(5)  VALUE ZERO.
006670      05 ��㪖@�T�Q�v�q.
006680         07 ��㪖@�񐔂T�Q�v�q         PIC 9(2)  VALUE ZERO.
006690         07 ��㪖@���T�Q�v�q           PIC 9(4)  VALUE ZERO.
006700      05 ��㪖@�T�Q�v�q.
006710         07 ��㪖@�񐔂T�Q�v�q         PIC 9(2)  VALUE ZERO.
006720         07 ��㪖@���T�Q�v�q           PIC 9(4)  VALUE ZERO.
006730      05 �d�ÂT�Q�v�q.
006740         07 �d�É񐔂T�Q�v�q           PIC 9(2)  VALUE ZERO.
006750         07 �d�×��T�Q�v�q             PIC 9(4)  VALUE ZERO.
006760      05 ���v�T�Q�v�q                  PIC 9(6)  VALUE ZERO.
006770      05 �����ʍ����v�T�Q�v�q          PIC 9(6)  VALUE ZERO.
006780      05 �����������T�Q�v�q            PIC 9(3)  VALUE ZERO.
006790      05 ���������v�T�Q�v�q            PIC 9(6)  VALUE ZERO.
006800****************
006810* �T���ʁ^�T�� *
006820****************
006830   03 ���ʂT�T�v�q.
006840      05 �����J�n�����T�T�v�q.
006850         07 �����J�n���T�T�v�q         PIC 9(2)  VALUE ZERO.
006860         07 �����J�n���T�T�v�q         PIC 9(2)  VALUE ZERO.
006870      05 ��ÂT�T�v�q.
006880         07 ��ÒP���T�T�v�q           PIC 9(4)  VALUE ZERO.
006890         07 ��É񐔂T�T�v�q           PIC 9(2)  VALUE ZERO.
006900         07 ��×��T�T�v�q             PIC 9(5)  VALUE ZERO.
006910      05 ��㪖@�T�T�v�q.
006920         07 ��㪖@�񐔂T�T�v�q         PIC 9(2)  VALUE ZERO.
006930         07 ��㪖@���T�T�v�q           PIC 9(4)  VALUE ZERO.
006940      05 ��㪖@�T�T�v�q.
006950         07 ��㪖@�񐔂T�T�v�q         PIC 9(2)  VALUE ZERO.
006960         07 ��㪖@���T�T�v�q           PIC 9(4)  VALUE ZERO.
006970      05 �d�ÂT�T�v�q.
006980         07 �d�É񐔂T�T�v�q           PIC 9(2)  VALUE ZERO.
006990         07 �d�×��T�T�v�q             PIC 9(4)  VALUE ZERO.
007000      05 ���v�T�T�v�q                  PIC 9(6)  VALUE ZERO.
007010      05 �����ʍ����v�T�T�v�q          PIC 9(6)  VALUE ZERO.
007020      05 �����������T�T�v�q            PIC 9(3)  VALUE ZERO.
007030      05 ���������v�T�T�v�q            PIC 9(6)  VALUE ZERO.
007040****************
007050* �T���ʁ^�W�� *
007060****************
007070   03 ���ʂT�W�v�q.
007080      05 �����J�n�����T�W�v�q.
007090         07 �����J�n���T�W�v�q         PIC 9(2)  VALUE ZERO.
007100         07 �����J�n���T�W�v�q         PIC 9(2)  VALUE ZERO.
007110      05 ��ÂT�W�v�q.
007120         07 ��ÒP���T�W�v�q           PIC 9(4)  VALUE ZERO.
007130         07 ��É񐔂T�W�v�q           PIC 9(2)  VALUE ZERO.
007140         07 ��×��T�W�v�q             PIC 9(5)  VALUE ZERO.
007150      05 ��㪖@�T�W�v�q.
007160         07 ��㪖@�񐔂T�W�v�q         PIC 9(2)  VALUE ZERO.
007170         07 ��㪖@���T�W�v�q           PIC 9(4)  VALUE ZERO.
007180      05 ��㪖@�T�W�v�q.
007190         07 ��㪖@�񐔂T�W�v�q         PIC 9(2)  VALUE ZERO.
007200         07 ��㪖@���T�W�v�q           PIC 9(4)  VALUE ZERO.
007210      05 �d�ÂT�W�v�q.
007220         07 �d�É񐔂T�W�v�q           PIC 9(2)  VALUE ZERO.
007230         07 �d�×��T�W�v�q             PIC 9(4)  VALUE ZERO.
007240      05 ���v�T�W�v�q                  PIC 9(6)  VALUE ZERO.
007250      05 �����ʍ����v�T�W�v�q          PIC 9(6)  VALUE ZERO.
007260      05 �����������T�W�v�q            PIC 9(3)  VALUE ZERO.
007270      05 ���������v�T�W�v�q            PIC 9(6)  VALUE ZERO.
007280******************
007290* �T���ʁ^�P�O�� *
007300******************
007310   03 ���ʂT�O�v�q.
007320      05 �����J�n�����T�O�v�q.
007330         07 �����J�n���T�O�v�q         PIC 9(2)  VALUE ZERO.
007340         07 �����J�n���T�O�v�q         PIC 9(2)  VALUE ZERO.
007350      05 ��ÂT�O�v�q.
007360         07 ��ÒP���T�O�v�q           PIC 9(4)  VALUE ZERO.
007370         07 ��É񐔂T�O�v�q           PIC 9(2)  VALUE ZERO.
007380         07 ��×��T�O�v�q             PIC 9(5)  VALUE ZERO.
007390      05 ��㪖@�T�O�v�q.
007400         07 ��㪖@�񐔂T�O�v�q         PIC 9(2)  VALUE ZERO.
007410         07 ��㪖@���T�O�v�q           PIC 9(4)  VALUE ZERO.
007420      05 ��㪖@�T�O�v�q.
007430         07 ��㪖@�񐔂T�O�v�q         PIC 9(2)  VALUE ZERO.
007440         07 ��㪖@���T�O�v�q           PIC 9(4)  VALUE ZERO.
007450      05 �d�ÂT�O�v�q.
007460         07 �d�É񐔂T�O�v�q           PIC 9(2)  VALUE ZERO.
007470         07 �d�×��T�O�v�q             PIC 9(4)  VALUE ZERO.
007480      05 ���v�T�O�v�q                  PIC 9(6)  VALUE ZERO.
007490      05 �����������T�O�v�q            PIC 9(3)  VALUE ZERO.
007500      05 ���������v�T�O�v�q            PIC 9(6)  VALUE ZERO.
008000*******************
008010*  ���׏����s���Z */202206
008020*******************
008030   03 ���׏����s���Z���v�q                PIC ZZZ   VALUE ZERO.
008030   03 ���׏����s���Z���v�q                PIC ZZ    VALUE ZERO.
007510*
007520**************
007530* �{�p����� *
007540**************
007550 01 �{�p�����v.
007560    03 �_���t�ԍ��v                    PIC X(16) VALUE SPACE.
007570    03 �ڍ��t�����ԍ��v              PIC X(16) VALUE SPACE.
007580    03 ��\�҃J�i�v                    PIC X(50) VALUE SPACE.
007590    03 ��\�Җ��v                      PIC X(50) VALUE SPACE.
007600    03 �ڍ��@���v                      PIC X(50) VALUE SPACE.
          03 �s���{���i�h�r�v                PIC X(2)   VALUE SPACE.
007610    03 �{�p���X�֔ԍ��v.
007620       05 �{�p���X�֔ԍ��P�v           PIC X(3)  VALUE SPACE.
007630       05 �{�p���X�֔ԍ��Q�v           PIC X(4)  VALUE SPACE.
007640    03 �{�p���Z���v.
007650       05 �{�p���Z���P�v               PIC X(50) VALUE SPACE.
007660       05 �{�p���Z���Q�v               PIC X(50) VALUE SPACE.
007670    03 �{�p���d�b�ԍ��v                PIC X(15) VALUE SPACE.
007680    03 �ڍ��t�����v                PIC N(10) VALUE SPACE.
007690    03 �������v.
007700        05 ������s���v              PIC X(40) VALUE SPACE.
007710        05 ������s�x�X���v          PIC X(40) VALUE SPACE.
007720        05 �a����ʂv                  PIC 9(1)  VALUE ZERO.
007730        05 �����ԍ��v                  PIC X(10) VALUE SPACE.
007740        05 �������`�l�v                PIC X(40) VALUE SPACE.
007750        05 �������`�l�J�i�v            PIC X(40) VALUE SPACE.
007760    03 ��z���󗝔ԍ��v                PIC X(15) VALUE SPACE.
007770    03 �_���t�N�����v.
007350       05 �_���t�a��v                 PIC 9      VALUE ZERO.
007780       05 �_���t�N�v                   PIC 9(2)  VALUE ZERO.
007790       05 �_���t���v                   PIC 9(2)  VALUE ZERO.
007800       05 �_���t���v                   PIC 9(2)  VALUE ZERO.
007810    03 ���҈ϔC�N�����v.
007350       05 ���҈ϔC�a��v               PIC 9      VALUE ZERO.
007820       05 ���҈ϔC�N�v                 PIC 9(2)  VALUE ZERO.
007830       05 ���҈ϔC���v                 PIC 9(2)  VALUE ZERO.
007840       05 ���҈ϔC���v                 PIC 9(2)  VALUE ZERO.
007850    03 ���{�p�h�c�v                    PIC X(15) VALUE SPACE.
007860    03 �s�����{�p�h�c�v                PIC X(15) VALUE SPACE.
007330    03 ���ϔԍ��v                      PIC X(28)  VALUE SPACE.
007860    03 ��ϔC���P�v                  PIC X(50) VALUE SPACE.
007860    03 ��ϔC���Q�v                  PIC X(50) VALUE SPACE.
007860    03 ��ϔC���R�v                  PIC X(50) VALUE SPACE.
007860    03 ��ϔC���S�v                  PIC X(50) VALUE SPACE.
007850    03 ���ԍ��v                        PIC X(2) VALUE SPACE.
002600** ����ԍ��E�l�ߗp
002610 01 ����ԍ��v�s.
002620    03 ����ԍ����l�߂v.
002630      05 ����ԍ����l�߂v�P            PIC X OCCURS 8 VALUE SPACE.
002640    03 ����ԍ��E�l�߂v.
002650      05 ����ԍ��E�l�߂v�P            PIC X OCCURS 8 VALUE SPACE.
007870**************
007880* ��f�ҏ�� *
007890**************
007900 01 ��f�ҏ��v.
      */�����C��/20190426
          03 �{�p�a��v                      PIC 9(1)   VALUE ZERO.
007910    03 �{�p�N���v.
007920       05 �{�p�N�v                     PIC 9(2)  VALUE ZERO.
007930       05 �{�p���v                     PIC 9(2)  VALUE ZERO.
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
          03 ���t�����`�F�b�N�v.
             05 �V���`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �W���`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �X���`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �P�O���`�F�b�N�v             PIC N(1)  VALUE SPACE.
007940*    03 �L���v                          PIC N(12) VALUE SPACE.
007570    03 �L���v.
007580       05 ����L���v                   PIC N(12)  VALUE SPACE.
007950    03 �ԍ��v.
007960       05 ����ԍ��v                   PIC X(15) VALUE SPACE.
007970       05 FILLER                       PIC X(15) VALUE SPACE.
          03 �L���ԍ��v.
             05 �L���ԍ��w�v                 PIC X(40) VALUE SPACE.
007980    03 �ی��Ҕԍ��v                    PIC X(10) VALUE SPACE.
007990    03 �ی��Җ��̂v.
008000       05 �ی��Җ��̂P�v               PIC X(30) VALUE SPACE.
008010       05 �ی��Җ��̂Q�v               PIC X(30) VALUE SPACE.
008020       05 FILLER                       PIC X(20) VALUE SPACE.
007340    03 �����於�̂v                    PIC X(56) VALUE SPACE.
008030*    03 �����於�̂v.
008040*       05 �����於�̂P�v               PIC X(30) VALUE SPACE.
008050*       05 �����於�̂Q�v               PIC X(30) VALUE SPACE.
008060*       05 FILLER                       PIC X(20) VALUE SPACE.
          03 ����S�Ҕԍ��v                PIC X(8)   VALUE SPACE.
          03 �󋋎Ҕԍ��v.
             05 ����󋋎Ҕԍ��v             PIC X(7)  VALUE SPACE.
             05 ����󋋎Ҕԍ��Q�v           PIC X(8)  VALUE SPACE.
008070    03 �����於�̃`�F�b�N.
008080       05 �s�`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008090       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008100       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
008110       05 ���g�`�F�b�N�v               PIC N(1)  VALUE SPACE.
008120    03 ��ی��ҏ��v.
008130       05 ��ی��҃J�i�v               PIC X(50) VALUE SPACE.
008140       05 ��ی��Ҏ����v               PIC X(50) VALUE SPACE.
008150       05 �X�֔ԍ��v.
008160          07 �X�֔ԍ��P�v              PIC X(3)  VALUE SPACE.
008170          07 �X�֔ԍ��Q�v              PIC X(4)  VALUE SPACE.
008180       05 ��ی��ҏZ���P�v             PIC X(50) VALUE SPACE.
008190       05 ��ی��ҏZ���Q�v             PIC X(50) VALUE SPACE.
008990       05 �d�b�ԍ��v                   PIC X(35)  VALUE SPACE.
008200    03 ���ҏ��v.
008210       05 ���҃J�i�v                   PIC X(50) VALUE SPACE.
008220       05 ���Ҏ����v                   PIC X(50) VALUE SPACE.
008230       05 ���Ґ��ʂv.
008240          07 ���ʂv                    PIC N(1)  VALUE SPACE.
008250       05 ���ʃ`�F�b�N�v.
008260          07 �j�`�F�b�N�v              PIC N(1)  VALUE SPACE.
008270          07 ���`�F�b�N�v              PIC N(1)  VALUE SPACE.
008280       05 ���Ҙa��v                   PIC 9(1)  VALUE ZERO.
008290       05 ���Ҙa��̂v               PIC N(2)  VALUE SPACE.
008300       05 �a��`�F�b�N�v.
008310          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008320          07 �吳�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008330          07 ���a�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008340          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
      */�����C��/������20190426
008210          07 �ߘa�`�F�b�N�v            PIC N(1)  VALUE SPACE.
                07 �ߘa�b�l�v                PIC X(4)  VALUE SPACE.
009110          07 �����v                    PIC N(2)  VALUE SPACE.
      */�����C��/������20190426
008350       05 ���ҔN�v                     PIC 9(2)  VALUE ZERO.
008360       05 ���Ҍ��v                     PIC 9(2)  VALUE ZERO.
008370       05 ���ғ��v                     PIC 9(2)  VALUE ZERO.
008380       05 ���җX�֔ԍ��v.
008390          07 ���җX�֔ԍ��P�v          PIC X(3)  VALUE SPACE.
008400          07 ���җX�֔ԍ��Q�v          PIC X(4)  VALUE SPACE.
008410       05 ���ҏZ���P�v                 PIC X(50) VALUE SPACE.
008420       05 ���ҏZ���Q�v                 PIC X(50) VALUE SPACE.
008430       05 �����v.
008440          07 ��������v                PIC N(4)  VALUE SPACE.
008450          07 FILLER                    PIC X(4)  VALUE SPACE.
008430*       05 �����`�F�b�N�v.
008260*          07 �{�l�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008260*          07 �Ƒ��`�F�b�N�v            PIC N(1)  VALUE SPACE.
008460*
008470*       05 ���������v                   PIC N(40) OCCURS 34 VALUE SPACE.
      */���p�Ή�/110421
             05 ���������v OCCURS 29.
                07 ���������w�v              PIC X(100)  VALUE SPACE.
008480*
008490    03 ������v                        PIC N(1)  VALUE SPACE.
008500    03 ���ʃR�����g�v                  PIC X(16) VALUE SPACE.
008490    03 �ی���ʂv                      PIC X(4)  VALUE SPACE.
008490    03 ������ʂv                      PIC X(10) VALUE SPACE.
008490    03 �ی���ʂb�l�v                  PIC X(16) VALUE SPACE.
008490    03 ���b�l�v.
             05 ���b�l�v�o                   PIC X(8) VALUE SPACE.
          03 �^�C�g���v                      PIC X(20) VALUE SPACE.
          03 ������v                        PIC X(60) VALUE SPACE.
008510*
008520****************
008530* �����f�[�^�e *
008540****************
008550 01 �������v.
008560    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
008570    03 ���ʏ��v  OCCURS   9.
008580       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
008590       05 ���ʃR�[�h�v.
008600          07 ������ʂv                PIC 9(2)  VALUE ZERO.
008610          07 ���ʂv                    PIC 9(2)  VALUE ZERO.
008620          07 ���E�敪�v                PIC 9(1)  VALUE ZERO.
008630          07 �����ʒu�ԍ��v            PIC 9(2)  VALUE ZERO.
008640       05 �������v                     PIC N(18) VALUE SPACE.
008650       05 �����N�����v.
008660          07 �����N�v                  PIC 9(2)  VALUE ZERO.
008670          07 �������v                  PIC 9(2)  VALUE ZERO.
008680          07 �������v                  PIC 9(2)  VALUE ZERO.
008690       05 �����N�����v.
008700          07 �����N�v                  PIC 9(2)  VALUE ZERO.
008710          07 �������v                  PIC 9(2)  VALUE ZERO.
008720          07 �������v                  PIC 9(2)  VALUE ZERO.
008730       05 �J�n�N�����v.
008740          07 �J�n�N�v                  PIC 9(2)  VALUE ZERO.
008750          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
008760          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
008770       05 �I���N�����v.
002980          07 �I���a��v                PIC 9     VALUE ZERO.
008780          07 �I���N�v                  PIC 9(2)  VALUE ZERO.
008790          07 �I�����v                  PIC 9(2)  VALUE ZERO.
008800          07 �I�����v                  PIC 9(2)  VALUE ZERO.
008810       05 �������v                     PIC 9(2)  VALUE ZERO.
008820       05 �]�A�敪�v                   PIC 9(1)  VALUE ZERO.
008830       05 �]�A�敪�`�F�b�N�v.
008840          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
008850          07 ���~�`�F�b�N�v            PIC N(1)  VALUE SPACE.
008860          07 �]��`�F�b�N�v            PIC N(1)  VALUE SPACE.
008870       05 �J�n�N�����擾�t���O         PIC X(3)  VALUE SPACE.
008880       05 ���ʋ�؂v                   PIC X(1)  VALUE SPACE.
008890       05 �o�ߗ��̂v.
008900          07 ����o�ߗ��̂v            PIC N(10) VALUE SPACE.
008910          07 FILLER                    PIC X(2)  VALUE SPACE.
008920    03 �V�K�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
008930    03 �p���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
          03 �{�p���v.
             05 �{�p���`�F�b�N�v   OCCURS 31 PIC N(1)  VALUE SPACE.
008940*
008950************
008960* ������� *
008970************
008980 01 �������v.
008990    03 �������Z�v.
009000       05 ���ԊO�`�F�b�N�v             PIC N(1)  VALUE SPACE.
009010       05 �x���`�F�b�N�v               PIC N(1)  VALUE SPACE.
009020       05 �[��`�F�b�N�v               PIC N(1)  VALUE SPACE.
009030    03 ���É��Z�v.
009040       05 ��ԃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
009050       05 �\���J��`�F�b�N�v           PIC N(1)  VALUE SPACE.
009060       05 ��H�`�F�b�N�v               PIC N(1)  VALUE SPACE.
009070       05 ���É��Z�񐔂v               PIC 9(2)  VALUE ZERO.
009080    03 �������q�`�F�b�N�v.
009090       05 ��`�F�b�N�v                 PIC N(1)  VALUE SPACE.
009100       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
009110       05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
009120    03 ���v�v                          PIC 9(7)  VALUE ZERO.
009130    03 ���񏈒u�����v�v                PIC 9(6)  VALUE ZERO.
009140    03 ���񏈒u���`�F�b�N�v.
009150       05 �������`�F�b�N�v             PIC N(1)  VALUE SPACE.
009160       05 �Œ藿�`�F�b�N�v             PIC N(1)  VALUE SPACE.
009170       05 �{�×��`�F�b�N�v             PIC N(1)  VALUE SPACE.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
          03 �����񐔂v                         PIC 9(2)  VALUE ZERO.
          03 �^���񐔂v                         PIC 9(1)  VALUE ZERO.
          03 �^�����v                           PIC 9(4)  VALUE ZERO.
009180*
009280************
009290* ���l��� *
009300************
009310 01 ���l���v.
010010    03 �K�p�P�v                        PIC N(48) VALUE SPACE.
010020    03 �K�p�Q�v                        PIC X(40) VALUE SPACE.
009340*    03 �K�p�R�v                        PIC N(38) VALUE SPACE.
009350*    03 �K�p�S�v                        PIC N(38) VALUE SPACE.
009360    03 �o�߃R�����g�v                  PIC N(60) VALUE SPACE.
009370*****************
009380* ���Z�v�g���я� *
009390*****************
009400 01 ���Ԃv                             PIC 9(4) VALUE ZERO.
009410*
       01 �E�v�{�p���v                       PIC X(100) VALUE SPACE.
       01 �{�p���v.
          03 �{�p���Q�v                      PIC X(1)  VALUE SPACE.
          03 �{�p���P�v                      PIC X(1)  VALUE SPACE.
       01 ���C�A�E�g�v.
004750    03 ��������N���v�p.
004720       05 ��������N�v�p               PIC 9(4) VALUE ZERO.
004770       05 �������v�p                   PIC 9(2) VALUE ZERO.
004770    03 ����ԍ��v�p                    PIC 9(8) VALUE ZERO.
004770    03 �ی��ԍ��v�p                    PIC X(8) VALUE ZERO.
004770    03 ����S�Ҕԍ��v�p              PIC X(8) VALUE ZERO.
004770    03 ��Ï����敪�v�p                PIC 9(1) VALUE ZERO.
004770    03 �{�l�Ƒ��v�p                    PIC 9(1) VALUE ZERO.
004750    03 �{�p����N���v�p.
004720       05 �{�p����N�v�p               PIC 9(4) VALUE ZERO.
004770       05 �{�p���v�p                   PIC 9(2) VALUE ZERO.
004770    03 ��p�z�v�p                      PIC 9(6) VALUE ZERO.
004770    03 ���S�z�v�p                      PIC 9(6) VALUE ZERO.
004770    03 �����z�v�p                      PIC 9(6) VALUE ZERO.
004770    03 �������v�p                      PIC 9(2) VALUE ZERO.
004770    03 ���ʐ��v�p                      PIC 9(1) VALUE ZERO.
004730    03 ���҃R�[�h�v�p.
004740       05 ���Ҕԍ��v�p                 PIC 9(6)  VALUE ZERO.
004750       05 �}�Ԃv�p                     PIC X(1)  VALUE SPACE.
             05 FILLER                       PIC X(1)  VALUE SPACE.
004770 01 �J���}�v�p                         PIC X(1) VALUE ",".
008140 01 ��ی��Җ��v�p                     PIC X(20) VALUE SPACE.
008140 01 ��f�Җ��v�p                       PIC X(20) VALUE SPACE.
       01 �p�q�f�[�^�v                       PIC X(109) VALUE SPACE.
009420*-----------------------------------------------------------------------*
009430 01 �������.
009440     03 ��`�̖��o                     PIC X(8)  VALUE SPACE.
009450     03 ���ڌQ���o                     PIC X(8)  VALUE SPACE.
009460     03 ������ʂo                     PIC X(2)  VALUE SPACE.
009470     03 �g������o.
009480         05 �[������o.
009490             07 �ړ������o             PIC X(1)  VALUE SPACE.
009500             07 �ړ��s���o             PIC 9(3)  VALUE ZERO.
009510         05 �ڍא���o                 PIC X(2)  VALUE SPACE.
009520     03 �ʒm���o                     PIC X(2)  VALUE SPACE.
009530     03 ���j�b�g���o                   PIC X(8)  VALUE SPACE.
009540*-----------------------------------------------------------------------*
009700*
      * C �A�g�p
       01  �����P�v        PIC X(4096).
       01  �����Q�v        PIC X(512).
       01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
      *
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
009710******************************************************************
009720*                          �A������                              *
009730******************************************************************
009740******************
009750* ��ʓ��̓f�[�^ *
009760******************
       01 �A���|�v���r���[ IS EXTERNAL.
          03 �A���|�v���r���[�敪          PIC 9.
       01 �A���|���̓f�[�^�d�b��� IS EXTERNAL.
          03 �A���|�d�b���                     PIC 9.
010440*
009770 01 �A���|���̓f�[�^�ϔC��� IS EXTERNAL.
009780    03 �A���|�ϔC���                  PIC 9.
009790*
009800************
009810* ����L�[ *
009820************
009830 01 �A����|�Ώۃf�[�^ IS EXTERNAL.
009840    03 �A����|�{�p�N����.
009850       05 �A����|�{�p�a��             PIC 9(1).
009860       05 �A����|�{�p�N               PIC 9(2).
009870       05 �A����|�{�p��               PIC 9(2).
009880    03 �A����|���҃R�[�h.
009890       05 �A����|���Ҕԍ�             PIC 9(6).
009900       05 �A����|�}��                 PIC X(1).
009910    03 �A����|�ی����                PIC 9(2).
009920    03 �A����|�ی��Ҕԍ�              PIC X(10).
009930    03 �A����|������                PIC 9(2).
009940    03 �A����|��p���S�Ҕԍ�          PIC X(10).
009950    03 �A����|�������                PIC 9(2).
009960    03 �A����|��p���S�Ҕԍ�����      PIC X(10).
009970    03 �A����|���҃J�i                PIC X(20).
009980    03 �A����|�{�l�Ƒ��敪            PIC 9(1).
009990*
013420 01 �A���|�L�[ IS EXTERNAL.
013430    03 �A���|�ی����                  PIC 9(2).
013440*
013450******************
013460* �R�J���������� *
013470******************
013480 01 �A���ԁ|�L�[ IS EXTERNAL.
013490    03 �A���ԁ|�{�p�N��.
013500       05 �A���ԁ|�{�p�a��             PIC 9.
013510       05 �A���ԁ|�{�p�N               PIC 9(2).
013520       05 �A���ԁ|�{�p��               PIC 9(2).
013530    03  �A���ԁ|���҃R�[�h.
013540       05 �A���ԁ|���Ҕԍ�             PIC 9(6).
013550       05 �A���ԁ|�}��                 PIC X.
013560    03 �A���ԁ|�Ώۃt���O              PIC X(3).
013570    03 �A���ԁ|���Ԍ��v.
013580       05 �A���ԁ|���Ԃv               PIC 9(2) OCCURS 9.
013590*
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
013600************************
013610* �������R���Z�b�g     *
013620************************
013630 01 �A�����|�L�[ IS EXTERNAL.
013640    03 �A�����|�{�p�N��.
013650       05 �A�����|�{�p�a��             PIC 9.
013660       05 �A�����|�{�p�N               PIC 9(2).
013670       05 �A�����|�{�p��               PIC 9(2).
013680    03  �A�����|���҃R�[�h.
013690       05 �A�����|���Ҕԍ�             PIC 9(6).
013700       05 �A�����|�}��                 PIC X.
013710    03 �A�����|������                  PIC 9(2).
013720    03 �A�����|���R��                  PIC N(63) OCCURS 15.
013730*
013740* ���S���擾�p14/10�`
013750 01 �A���|���S���擾�L�[ IS EXTERNAL.
013760    03 �A���|�{�p�a��N��.
013770       05 �A���|�{�p�a��               PIC 9.
013780       05 �A���|�{�p�N��.
013790          07 �A���|�{�p�N              PIC 9(2).
013800          07 �A���|�{�p��              PIC 9(2).
013810    03 �A���|���҃R�[�h.
013820       05 �A���|���Ҕԍ�               PIC 9(6).
013830       05 �A���|�}��                   PIC X.
013840    03 �A���|���ە��S��                PIC 9(3).
013850    03 �A���|���ۖ{�̕��S��            PIC 9(3).
013860    03 �A���|���ە��S��                PIC 9(3).
013870    03 �A���|�Q�V�V���S��              PIC 9(3).
013880    03 �A���|�������S��                PIC 9(3).
013890    03 �A���|���ʗp���S��              PIC 9(3).
013900*
013163*************
013164* ��������
013165*************
013166 01 �A�������́|�L�[ IS EXTERNAL.
013167    03 �A�������́|�������             PIC 9(2).
013168    03 �A�������́|��p���S�Ҕԍ�����   PIC X(10).
013169*   / OUT /
013170    03 �A�������́|���̏W�c.
013171       05 �A�������́|�P����            PIC N.
013172       05 �A�������́|����              PIC N(4).
013173       05 �A�������́|��������          PIC N(10).
013180**
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
013910******************************************************************
013920*                      PROCEDURE  DIVISION                       *
013930******************************************************************
013940 PROCEDURE               DIVISION.
013950************
013960*           *
013970* ��������   *
013980*           *
013990************
002570     PERFORM �v�����^�t�@�C���쐬.
014000     PERFORM ������.
014020     PERFORM �A�����ڑҔ�.
014030************
014040*           *
014050* �又��     *
014060*           *
014070************
014080* ���
014090     PERFORM ����Z�b�g.
014100     PERFORM �������.
014110************
014120*           *
014130* �I������   *
014140*           *
014150************
014160     PERFORM ��f�҈���敪�X�V.
014170     PERFORM �I������.
014190     MOVE ZERO  TO PROGRAM-STATUS.
014200     EXIT PROGRAM.
014210*
014220*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YHN6121"             TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪  TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014230************
014240* ��������  *
014250************
014260*================================================================*
014270 ������ SECTION.
014280*================================================================*
014290     PERFORM �t�@�C���I�[�v��.
014380     PERFORM ������擾.
014410*
014420*================================================================*
014430 ������擾 SECTION.
014440*
014450     MOVE ZEROS TO ���|����敪.
014460     READ ������}�X�^
014470     NOT INVALID KEY
014490         MOVE ���|���Z������������敪 TO ������������敪�v
014500         MOVE ���|���Z�������R����敪 TO �������R����敪�v
014510         MOVE ���|���Z�v�g���t�敪     TO ���Z�v�g���t�敪�v
014520         MOVE ���|���Z�v�g���ғ��t�敪 TO ���Z�v�g���ғ��t�敪�v
014530     END-READ.
014540*
014830*================================================================*
014840 �t�@�C���I�[�v�� SECTION.
014850*
014860     OPEN INPUT   �ی��҃}�X�^
014870         MOVE NC"�ی���" TO �t�@�C����.
014880         PERFORM �I�[�v���`�F�b�N.
014890     OPEN INPUT   �����}�X�^
014900         MOVE NC"����" TO �t�@�C����.
014910         PERFORM �I�[�v���`�F�b�N.
014920     OPEN INPUT   ���̃}�X�^
014930         MOVE NC"����" TO �t�@�C����.
014940         PERFORM �I�[�v���`�F�b�N.
007560     OPEN INPUT   ���Z�v�g�e
007570         MOVE NC"���Z" TO �t�@�C����.
007580         PERFORM �I�[�v���`�F�b�N.
014980     OPEN INPUT   ������}�X�^
014990         MOVE NC"������" TO �t�@�C����.
015000         PERFORM �I�[�v���`�F�b�N.
015010     OPEN INPUT   �{�p�����}�X�^
015020         MOVE NC"�{��" TO �t�@�C����.
015030         PERFORM �I�[�v���`�F�b�N.
015040     OPEN INPUT   �h�c�Ǘ��}�X�^
015050         MOVE NC"�h�c" TO �t�@�C����.
015060         PERFORM �I�[�v���`�F�b�N.
015070     OPEN INPUT   �o�߃}�X�^
015080         MOVE NC"�o��" TO �t�@�C����.
015090         PERFORM �I�[�v���`�F�b�N.
015100     OPEN INPUT   �{�p�L�^�e.
015110         MOVE NC"�{�L�e" TO �t�@�C����.
015120         PERFORM �I�[�v���`�F�b�N.
015130     OPEN INPUT   �����f�[�^�e.
015140         MOVE NC"����" TO �t�@�C����.
015150         PERFORM �I�[�v���`�F�b�N.
015160     OPEN INPUT   ���������e.
015170         MOVE NC"��������" TO �t�@�C����.
015180         PERFORM �I�[�v���`�F�b�N.
015250     OPEN INPUT   ��ƃt�@�C���R.
015260         MOVE NC"��R" TO �t�@�C����.
015270         PERFORM �I�[�v���`�F�b�N.
015250     OPEN INPUT   ��ƃt�@�C���T.
015260         MOVE NC"��T" TO �t�@�C����.
015270         PERFORM �I�[�v���`�F�b�N.
014840     OPEN INPUT �s�����}�X�^.
014850         MOVE NC"�s����" TO �t�@�C����.
014860         PERFORM �I�[�v���`�F�b�N.
015160     OPEN INPUT   ����}�X�^.
015170         MOVE NC"���" TO �t�@�C����.
015180         PERFORM �I�[�v���`�F�b�N.
015310*
015320     OPEN I-O   ��f�ҏ��e.
015330         MOVE NC"���" TO �t�@�C����.
015340         PERFORM �I�[�v���`�F�b�N.
015350*
015360     OPEN I-O   ����t�@�C��
015370         PERFORM �G���[�����o.
015380*
015390*================================================================*
015400 �I�[�v���`�F�b�N SECTION.
015410*
015420     IF ( ��ԃL�[  NOT =  "00" )
015430         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
015440         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
015450         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015460                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015470         ACCEPT  �L�[���� FROM CONS
015480         PERFORM �t�@�C����
015490         EXIT PROGRAM.
015500*
015620*================================================================*
015630 �A�����ڑҔ� SECTION.
015640*================================================================*
015650     MOVE �A����|�{�p�a��           TO �{�p�a��v�q.
015660     MOVE �A����|�{�p�N             TO �{�p�N�v�q.
015670     MOVE �A����|�{�p��             TO �{�p���v�q.
015680     MOVE �A����|�ی����           TO �ی���ʂv�q.
015690     MOVE �A����|�ی��Ҕԍ�         TO �ی��Ҕԍ��v�q.
015700     MOVE �A����|������           TO �����ʂv�q.
015710     MOVE �A����|��p���S�Ҕԍ�     TO ��p���S�Ҕԍ��v�q.
015720     MOVE �A����|�������           TO ������ʂv�q.
015730     MOVE �A����|��p���S�Ҕԍ����� TO ��p���S�Ҕԍ������v�q.
015740     MOVE �A����|�{�l�Ƒ��敪       TO �{�l�Ƒ��敪�v�q.
015750     MOVE �A����|���҃J�i           TO ���҃J�i�v�q.
015760     MOVE �A����|���Ҕԍ�           TO ���Ҕԍ��v�q.
015770     MOVE �A����|�}��               TO �}�Ԃv�q.
015850*
015860     EVALUATE �A���|�ی����
015870     WHEN 05
015880        MOVE "ROUJ" TO ���Z�v�g��ނv
015890     WHEN 01
015900        MOVE "KOKU" TO ���Z�v�g��ނv
015910     WHEN 02
015920     WHEN 06
015930     WHEN 07
015940        MOVE "SYAH" TO ���Z�v�g��ނv
015950     WHEN 03
015960        MOVE "KUMI" TO ���Z�v�g��ނv
015970     WHEN 04
015980        MOVE "KYOS" TO ���Z�v�g��ނv
015990     WHEN 08
016000        MOVE "TAIS" TO ���Z�v�g��ނv
016010     WHEN 09
016020        MOVE "JIEI" TO ���Z�v�g��ނv
016030     WHEN 50 THRU 60
016040        MOVE "JYOS" TO ���Z�v�g��ނv
016050     WHEN OTHER
016060        MOVE SPACE  TO ���Z�v�g��ނv
016070     END-EVALUATE.
015780*
015790************
015800* �又��    *
015810************
015820*================================================================*
015830 ����Z�b�g SECTION.
015840*================================================================*
015850     PERFORM ���ڏ�����.
           PERFORM ��{���擾.
015860     PERFORM �{�p�����擾.
015870     PERFORM ��f�ҏ��擾.
015880     PERFORM ��������擾.
015890     PERFORM �����f�[�^�擾.
015910     PERFORM �������擾.
015920     PERFORM �{�p�L�^�擾.
015930     PERFORM ���Z�v�g���я��擾.
015940***     PERFORM ��������擾.
015960     PERFORM �������Z�����擾.
           PERFORM ���{�p�h�c�擾.
           PERFORM �p�q�f�[�^�Z�b�g.
      *
           MOVE ��|���҃R�[�h     TO ���҃R�[�h.
015970*
016791*-----------------------------------------------*
016800     IF ( ������������敪�v  NOT = 1 ) AND ( ���Z������������敪�v NOT = 1 )
016813        IF ( ������������敪�v = 3 OR 4 )
016815           PERFORM ������������Ώ۔��菈��
016817        ELSE
016820           PERFORM ���������擾
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
016020*
016030     IF ( �������R����敪�v  NOT = 1 )
               MOVE �������R����敪�v TO �A�E���|�����敪
016080     END-IF.
016090*
016100     PERFORM �ϔC�N�����擾.
           PERFORM �{�p���擾.
016110*
016120     PERFORM ���É��Z�񐔎擾.
016130*     PERFORM ���S���擾.
           IF ��|������� NOT = ZERO
016140        PERFORM ������擾
              IF ������v NOT = SPACE
                 MOVE ������v         TO ������
                 MOVE NC"��"           TO �����p�}��
              END-IF
           END-IF.
016150*
016420******************
016430* �^�C�g���Z�b�g *
016440******************
           IF �A���|�ی���� > 50
              MOVE �^�C�g���v TO �^�C�g��
              MOVE ������v   TO �����
           END-IF.
016160********************
016170* ��f�ҏ��Z�b�g *
016180********************
016230*
           MOVE �{�p�a��v         TO ���|�����敪.
037380     READ �����}�X�^
037390     NOT INVALID KEY
037400         MOVE ���|��������   TO �{�p�a��
037410     END-READ.
016240     MOVE �{�p�N�v           TO �{�p�N.
016250     MOVE �{�p���v           TO �{�p��.
      *
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
      *
           IF �A���|�ی���� > 50
               IF ����S�Ҕԍ��v(1:2) = "99"
                   MOVE SPACE            TO ����S�Ҕԍ�
               ELSE
                   MOVE ����S�Ҕԍ��v TO ����S�Ҕԍ�
               END-IF
      */�󋋎Ҕԍ����W�����ȏ�̏ꍇ�g�𖳎����Ĉ������/110425
               IF ����󋋎Ҕԍ��Q�v = SPACE
                   MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
               ELSE
                   MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
               END-IF
               MOVE "�i�����j"           TO �����b�l
               EVALUATE ��|�������
               WHEN 51
                   MOVE "/�S�P�V"        TO ������ʂv
               WHEN 52
                   MOVE "/�ЂƂ�e"      TO ������ʂv
               WHEN 53
                   MOVE "/�g��"          TO ������ʂv
               WHEN 54
                   MOVE "/�픚"          TO ������ʂv
               WHEN 55
                   MOVE "/���c"          TO ������ʂv
               WHEN 60
                   MOVE "/������"        TO ������ʂv
               END-EVALUATE
           END-IF.
017070*
016410     MOVE �ی��Ҕԍ��v      TO �ی��Ҕԍ� �ی��Ҕԍ��P.
           MOVE �����於�̂v      TO �ی��Җ�.
016420*
016430*     IF ( �ی��Җ��̂Q�v = SPACE )
016440*        MOVE SPACE          TO �ی��Җ��̂P �ی��Җ��̂Q
016450*        MOVE �ی��Җ��̂P�v TO �ی��Җ���
016460*     ELSE
016470*        MOVE SPACE          TO �ی��Җ���
016480*        MOVE �ی��Җ��̂P�v TO �ی��Җ��̂P
016490*        MOVE �ی��Җ��̂Q�v TO �ی��Җ��̂Q
016500*     END-IF.
016510*
016520*     IF ( �����於�̂Q�v = SPACE )
016530*        MOVE SPACE          TO �����於�̂P �����於�̂Q
016540*        MOVE �����於�̂P�v TO �����於��
016550*     ELSE
016560*        MOVE SPACE          TO �����於��
016570*        MOVE �����於�̂P�v TO �����於�̂P
016580*        MOVE �����於�̂Q�v TO �����於�̂Q
016590*     END-IF.
016640*
016650     MOVE ��ی��҃J�i�v    TO ��ی��҃J�i.
016660     MOVE ��ی��Ҏ����v    TO ��ی��Ҏ���.
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
016700     MOVE ��ی��ҏZ���P�v  TO �Z���P.
016710     MOVE ��ی��ҏZ���Q�v  TO �Z���Q.
016720*
016730***     MOVE ���җX�֔ԍ��P�v  TO �X�֔ԍ��P.
016740***     MOVE ���җX�֔ԍ��Q�v  TO �X�֔ԍ��Q.
016750***     MOVE "-"               TO �X�֋��.
016760***     MOVE ���ҏZ���P�v      TO �Z���P.
016770***     MOVE ���ҏZ���Q�v      TO �Z���Q.
016780*
016790     MOVE ���҃J�i�v        TO ���҃J�i.
016800     MOVE ���Ҏ����v        TO ���Ҏ���.
016810     MOVE �j�`�F�b�N�v      TO �j�`�F�b�N.
016820     MOVE ���`�F�b�N�v      TO ���`�F�b�N.
016830***     MOVE ���ʂv            TO ����.
016840     MOVE �����`�F�b�N�v    TO �����`�F�b�N.
016850     MOVE �吳�`�F�b�N�v    TO �吳�`�F�b�N.
016860     MOVE ���a�`�F�b�N�v    TO ���a�`�F�b�N.
016870     MOVE �����`�F�b�N�v    TO �����`�F�b�N.
023070     MOVE �ߘa�`�F�b�N�v    TO �ߘa�`�F�b�N.
           MOVE "1�� 2�� 3�� 4�� 5��"   TO �����b�l.
016880***     MOVE ���Ҙa��̂v    TO ���Ҙa��.
016890     MOVE ���ҔN�v          TO ���ҔN.
016900     MOVE ���Ҍ��v          TO ���Ҍ�.
016910     MOVE ���ғ��v          TO ���ғ�.
016920***     MOVE ��������v        TO ����.
016930***     MOVE ���ʃR�����g�v    TO �����R�����g.
016940*
016960        MOVE ���������Œ�v TO ���������b�l.
016980        MOVE ���������v(1)  TO ���������P.
016990        MOVE ���������v(2)  TO ���������Q.
017000        MOVE ���������v(3)  TO ���������R.
017010        MOVE ���������v(4)  TO ���������S.
017010        MOVE ���������v(5)  TO ���������T.
017010        MOVE ���������v(6)  TO ���������U.
017010        MOVE ���������v(7)  TO ���������V.
017030*
017040********************
017050* �����f�[�^�Z�b�g *
017060********************
017070* �P���� *
017080**********
017090     MOVE �������v(1)       TO �������P.
017100     MOVE �����N�v(1)       TO �����N�P.
017110     MOVE �������v(1)       TO �������P.
017120     MOVE �������v(1)       TO �������P.
017130     MOVE �����N�v(1)       TO �����N�P.
017140     MOVE �������v(1)       TO �������P.
017150     MOVE �������v(1)       TO �������P.
017160     MOVE �J�n�N�v(1)       TO �J�n�N�P.
017170     MOVE �J�n���v(1)       TO �J�n���P.
017180     MOVE �J�n���v(1)       TO �J�n���P.
017190     MOVE �I���N�v(1)       TO �I���N�P.
017200     MOVE �I�����v(1)       TO �I�����P.
017210     MOVE �I�����v(1)       TO �I�����P.
017220     MOVE �������v(1)       TO �������P.
017230     MOVE �����`�F�b�N�v(1) TO �����`�F�b�N�P.
017240     MOVE ���~�`�F�b�N�v(1) TO ���~�`�F�b�N�P.
017250     MOVE �]��`�F�b�N�v(1) TO �]��`�F�b�N�P.
017260**********
017270* �Q���� *
017280**********
017290     MOVE �������v(2)       TO �������Q.
017300     MOVE �����N�v(2)       TO �����N�Q.
017310     MOVE �������v(2)       TO �������Q.
017320     MOVE �������v(2)       TO �������Q.
017330     MOVE �����N�v(2)       TO �����N�Q.
017340     MOVE �������v(2)       TO �������Q.
017350     MOVE �������v(2)       TO �������Q.
017360     MOVE �J�n�N�v(2)       TO �J�n�N�Q.
017370     MOVE �J�n���v(2)       TO �J�n���Q.
017380     MOVE �J�n���v(2)       TO �J�n���Q.
017390     MOVE �I���N�v(2)       TO �I���N�Q.
017400     MOVE �I�����v(2)       TO �I�����Q.
017410     MOVE �I�����v(2)       TO �I�����Q.
017420     MOVE �������v(2)       TO �������Q.
017230     MOVE �����`�F�b�N�v(2) TO �����`�F�b�N�Q.
017440     MOVE ���~�`�F�b�N�v(2) TO ���~�`�F�b�N�Q.
017450     MOVE �]��`�F�b�N�v(2) TO �]��`�F�b�N�Q.
017460**********
017470* �R���� *
017480**********
017490     MOVE �������v(3)       TO �������R.
017500     MOVE �����N�v(3)       TO �����N�R.
017510     MOVE �������v(3)       TO �������R.
017520     MOVE �������v(3)       TO �������R.
017530     MOVE �����N�v(3)       TO �����N�R.
017540     MOVE �������v(3)       TO �������R.
017550     MOVE �������v(3)       TO �������R.
017560     MOVE �J�n�N�v(3)       TO �J�n�N�R.
017570     MOVE �J�n���v(3)       TO �J�n���R.
017580     MOVE �J�n���v(3)       TO �J�n���R.
017590     MOVE �I���N�v(3)       TO �I���N�R.
017600     MOVE �I�����v(3)       TO �I�����R.
017610     MOVE �I�����v(3)       TO �I�����R.
017620     MOVE �������v(3)       TO �������R.
017230     MOVE �����`�F�b�N�v(3) TO �����`�F�b�N�R.
017640     MOVE ���~�`�F�b�N�v(3) TO ���~�`�F�b�N�R.
017650     MOVE �]��`�F�b�N�v(3) TO �]��`�F�b�N�R.
017660**********
017670* �S���� *
017680**********
017690     MOVE �������v(4)       TO �������S.
017700     MOVE �����N�v(4)       TO �����N�S.
017710     MOVE �������v(4)       TO �������S.
017720     MOVE �������v(4)       TO �������S.
017730     MOVE �����N�v(4)       TO �����N�S.
017740     MOVE �������v(4)       TO �������S.
017750     MOVE �������v(4)       TO �������S.
017760     MOVE �J�n�N�v(4)       TO �J�n�N�S.
017770     MOVE �J�n���v(4)       TO �J�n���S.
017780     MOVE �J�n���v(4)       TO �J�n���S.
017790     MOVE �I���N�v(4)       TO �I���N�S.
017800     MOVE �I�����v(4)       TO �I�����S.
017810     MOVE �I�����v(4)       TO �I�����S.
017820     MOVE �������v(4)       TO �������S.
017230     MOVE �����`�F�b�N�v(4) TO �����`�F�b�N�S.
017840     MOVE ���~�`�F�b�N�v(4) TO ���~�`�F�b�N�S.
017850     MOVE �]��`�F�b�N�v(4) TO �]��`�F�b�N�S.
017860**********
017870* �T���� *
017880**********
017890     MOVE �������v(5)       TO �������T.
017900     MOVE �����N�v(5)       TO �����N�T.
017910     MOVE �������v(5)       TO �������T.
017920     MOVE �������v(5)       TO �������T.
017930     MOVE �����N�v(5)       TO �����N�T.
017940     MOVE �������v(5)       TO �������T.
017950     MOVE �������v(5)       TO �������T.
017960     MOVE �J�n�N�v(5)       TO �J�n�N�T.
017970     MOVE �J�n���v(5)       TO �J�n���T.
017980     MOVE �J�n���v(5)       TO �J�n���T.
017990     MOVE �I���N�v(5)       TO �I���N�T.
018000     MOVE �I�����v(5)       TO �I�����T.
018010     MOVE �I�����v(5)       TO �I�����T.
018020     MOVE �������v(5)       TO �������T.
017230     MOVE �����`�F�b�N�v(5) TO �����`�F�b�N�T.
018040     MOVE ���~�`�F�b�N�v(5) TO ���~�`�F�b�N�T.
018050     MOVE �]��`�F�b�N�v(5) TO �]��`�F�b�N�T.
018060**************
018070* �o�߃Z�b�g *
018080**************
018090*/ �ҏW��̌o�ߗ��̂𒠕[�ɃZ�b�g���� /*
018100     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ( ���ʂb�m�s > 5 )
018110         MOVE �o�ߗ��̕ҏW�s(���ʂb�m�s) TO �o�ߗ���(���ʂb�m�s)
018120     END-PERFORM.
018130*
018140*****     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018150********             UNTIL ( ���ʂb�m�s > ���ʐ��v )
018160*****             UNTIL ( ���ʂb�m�s > 5 )
018170******         MOVE ���ʂb�m�s�v(���ʂb�m�s)   TO �o�ߕ��ʂb�m�s(���ʂb�m�s)
018180******         MOVE ���ʋ�؂v(���ʂb�m�s)     TO ���ʋ��(���ʂb�m�s)
018190*****         MOVE ����o�ߗ��̂v(���ʂb�m�s) TO �o�ߗ���(���ʂb�m�s)
018200*****     END-PERFORM.
018210*****************************************
018220*     �V�K�E�p���`�F�b�N�ɂ���        *
018230*   ���V�K...�����L�� ���p��...�����Ȃ� *
018240*****************************************
018250     MOVE �V�K�`�F�b�N�v    TO �V�K�`�F�b�N.
018260     MOVE �p���`�F�b�N�v    TO �p���`�F�b�N.
018270********************
018280* �����f�[�^�Z�b�g *
018290********************
018300*    ****************************************************************
018310*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
018320*    ****************************************************************
018330     MOVE �������v�q                   TO  ������.
           MOVE ���k���v�q                   TO  ���������k��.
018340     MOVE �x���`�F�b�N�v               TO  �x���`�F�b�N.
018350     MOVE �[��`�F�b�N�v               TO  �[��`�F�b�N.
018360     MOVE ���ԊO�`�F�b�N�v             TO  ���ԊO�`�F�b�N.
018370     MOVE �������Z���v�q               TO  �������Z��.
019110     IF ( �������Z���v�s(1) NOT = ZERO ) OR
019120        ( �������Z���v�s(1) NOT = ZERO )
019130        MOVE �������Z���v�s(1)         TO  �������Z��
019140        MOVE �������Z���v�s(1)         TO  �������Z��
      *        MOVE "�{�p����"                TO �������Z�b�l
              MOVE ":"                       TO �������Z���
019150     END-IF.
018380     MOVE �Č����v�q                   TO  �Č���.
018390*
018400     MOVE ���Ë����v�q                 TO  ���Ë���.
018410     MOVE ���É񐔂v�q                 TO  ���É�.
018420     MOVE ���×��v�q                   TO  ���×�.
018430     MOVE ��ԃ`�F�b�N�v               TO  ��ԃ`�F�b�N.
018440     MOVE ��H�`�F�b�N�v               TO  ��H�`�F�b�N.
018450     MOVE �\���J��`�F�b�N�v           TO  �\���J��`�F�b�N.
018460*     MOVE ���É��Z�񐔂v               TO  ���É��Z��.
018470     MOVE ���É��Z���v�q               TO  ���É��Z��.
018480*
           MOVE �����񐔂v                   TO  ������.
018520     MOVE �������q���Z���v�q           TO  �������q���Z��.
           MOVE �^���񐔂v                   TO  �^����.
           MOVE �^�����v                     TO  �^����×�.
018530     MOVE �{�p���񋟗��v�q           TO  �{�p���񋟗�.
018540*
018550     MOVE ���v�v                       TO ���v.
018560********************
018570* ���񏈒u���Z�b�g *
018580********************
018590     MOVE �������`�F�b�N�v            TO �������`�F�b�N.
018600     MOVE �Œ藿�`�F�b�N�v            TO �Œ藿�`�F�b�N.
018610     MOVE �{�×��`�F�b�N�v            TO �{�×��`�F�b�N.
018620*
018630     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018640             UNTIL ( ���ʂb�m�s > ���ʐ��v )
018650         MOVE ���񏈒u���v�q(���ʂb�m�s) TO ���񏈒u��(���ʂb�m�s)
018660     END-PERFORM.
018670     MOVE ���񏈒u�����v�v            TO ���񏈒u�����v.
018680*
018690********************
018700* �����������Z�b�g *
018710********************
018720*    **********
018730*    * �P���� *
018740*    **********
018750     MOVE ��ÒP���P�v�q             TO ��ÒP���P.
018760     MOVE ��É񐔂P�v�q             TO ��É񐔂P.
018770     MOVE ��×��P�v�q               TO ��×��P.
018780     MOVE ��㪖@�񐔂P�v�q           TO ��㪖@�񐔂P.
018790     MOVE ��㪖@���P�v�q             TO ��㪖@���P.
018800     MOVE ��㪖@�񐔂P�v�q           TO ��㪖@�񐔂P.
018810     MOVE ��㪖@���P�v�q             TO ��㪖@���P.
018820     MOVE �d�É񐔂P�v�q             TO �d�É񐔂P.
018830     MOVE �d�×��P�v�q               TO �d�×��P.
018840     MOVE ���v�P�v�q                 TO ���v�P.
018850     IF ( �����������P�v�q NOT = ZERO )
018860         COMPUTE �����������P = �����������P�v�q / 100
018870     END-IF.
018880     MOVE ���������v�P�v�q           TO ���������v�P.
018890*    **********
018900*    * �Q���� *
018910*    **********
018920     MOVE ��ÒP���Q�v�q             TO ��ÒP���Q.
018930     MOVE ��É񐔂Q�v�q             TO ��É񐔂Q.
018940     MOVE ��×��Q�v�q               TO ��×��Q.
018950     MOVE ��㪖@�񐔂Q�v�q           TO ��㪖@�񐔂Q.
018960     MOVE ��㪖@���Q�v�q             TO ��㪖@���Q.
018970     MOVE ��㪖@�񐔂Q�v�q           TO ��㪖@�񐔂Q.
018980     MOVE ��㪖@���Q�v�q             TO ��㪖@���Q.
018990     MOVE �d�É񐔂Q�v�q             TO �d�É񐔂Q.
019000     MOVE �d�×��Q�v�q               TO �d�×��Q.
019010     MOVE ���v�Q�v�q                 TO ���v�Q.
019020     IF ( �����������Q�v�q NOT = ZERO )
019030         COMPUTE �����������Q = �����������Q�v�q / 100
019040     END-IF.
019050     MOVE ���������v�Q�v�q           TO ���������v�Q.
019060*    ****************
019070*    * �R���ʁ^�W�� *
019080*    ****************
019090     MOVE ��ÒP���R�W�v�q             TO ��ÒP���R�W.
019100     MOVE ��É񐔂R�W�v�q             TO ��É񐔂R�W.
019110     MOVE ��×��R�W�v�q               TO ��×��R�W.
019120     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
019130     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
019140     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
019150     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
019160     MOVE �d�É񐔂R�W�v�q             TO �d�É񐔂R�W.
019170     MOVE �d�×��R�W�v�q               TO �d�×��R�W.
019180     MOVE ���v�R�W�v�q                 TO ���v�R�W.
019190     MOVE �����ʍ����v�R�W�v�q         TO �����ʍ����v�R�W.
019200     IF ( �����������R�W�v�q NOT = ZERO )
019210         COMPUTE �����������R�W = �����������R�W�v�q / 100
019220     END-IF.
019230     MOVE ���������v�R�W�v�q           TO ���������v�R�W.
      */ ������ 0.7��0.6 /42505
      *     IF (�{�p�a��N���v�q >= 42505)
      *        MOVE "60"                      TO �����R�W
      *        MOVE "0.6"                     TO �����ʂR�W
      *        MOVE "==="                     TO ���������R�W �����ʒ����R�W
      *     END-IF.
019240*    ****************
019250*    * �R���ʁ^10�� *
019260*    ****************
019270     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
019280     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
019320     MOVE ��ÒP���R�O�v�q             TO ��ÒP���R�O.
019330     MOVE ��É񐔂R�O�v�q             TO ��É񐔂R�O.
019340     MOVE ��×��R�O�v�q               TO ��×��R�O.
019350     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
019360     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
019370     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
019380     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
019390     MOVE �d�É񐔂R�O�v�q             TO �d�É񐔂R�O.
019400     MOVE �d�×��R�O�v�q               TO �d�×��R�O.
019410     MOVE ���v�R�O�v�q                 TO ���v�R�O.
019420     IF ( �����������R�O�v�q NOT = ZERO )
019430         COMPUTE �����������R�O = �����������R�O�v�q / 100
019440     END-IF.
019450     MOVE ���������v�R�O�v�q           TO ���������v�R�O.
019460**    ****************
019470**    * �S���ʁ^�T�� *
019480**    ****************
019490*     MOVE ��ÒP���S�T�v�q             TO ��ÒP���S�T.
019500*     MOVE ��É񐔂S�T�v�q             TO ��É񐔂S�T.
019510*     MOVE ��×��S�T�v�q               TO ��×��S�T.
019520*     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
019530*     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
019540*     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
019550*     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
019560*     MOVE �d�É񐔂S�T�v�q             TO �d�É񐔂S�T.
019570*     MOVE �d�×��S�T�v�q               TO �d�×��S�T.
019580*     MOVE ���v�S�T�v�q                 TO ���v�S�T.
019590*     MOVE �����ʍ����v�S�T�v�q         TO �����ʍ����v�S�T.
019600*     IF ( �����������S�T�v�q NOT = ZERO )
019610*         COMPUTE �����������S�T = �����������S�T�v�q / 100
019620*     END-IF.
019630*     MOVE ���������v�S�T�v�q           TO ���������v�S�T.
019640*    ****************
019650*    * �S���ʁ^�W�� *
019660*    ****************
019670     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
019680     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
019720     MOVE ��ÒP���S�W�v�q             TO ��ÒP���S�W.
019730     MOVE ��É񐔂S�W�v�q             TO ��É񐔂S�W.
019740     MOVE ��×��S�W�v�q               TO ��×��S�W.
019750     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
019760     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
019770     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
019780     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
019790     MOVE �d�É񐔂S�W�v�q             TO �d�É񐔂S�W.
019800     MOVE �d�×��S�W�v�q               TO �d�×��S�W.
019810     MOVE ���v�S�W�v�q                 TO ���v�S�W.
019820     MOVE �����ʍ����v�S�W�v�q         TO �����ʍ����v�S�W.
019830     IF ( �����������S�W�v�q NOT = ZERO )
019840         COMPUTE �����������S�W = �����������S�W�v�q / 100
019850     END-IF.
019860     MOVE ���������v�S�W�v�q           TO ���������v�S�W.
      */ ������ 0.7��0.6 /42505
      *     IF (�{�p�a��N���v�q >= 42505)
      *        MOVE "60"                      TO �����S�W
      *        MOVE "0.6"                     TO �����ʂS�W
      *        MOVE "==="                     TO ���������S�W �����ʒ����S�W
      *     END-IF.
019870*    ****************
019880*    * �S���ʁ^10�� *
019890*    ****************
019900     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
019910     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
019950     MOVE ��ÒP���S�O�v�q             TO ��ÒP���S�O.
019960     MOVE ��É񐔂S�O�v�q             TO ��É񐔂S�O.
019970     MOVE ��×��S�O�v�q               TO ��×��S�O.
019980     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
019990     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
020000     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
020010     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
020020     MOVE �d�É񐔂S�O�v�q             TO �d�É񐔂S�O.
020030     MOVE �d�×��S�O�v�q               TO �d�×��S�O.
020040     MOVE ���v�S�O�v�q                 TO ���v�S�O.
020050     IF ( �����������S�O�v�q NOT = ZERO )
020060         COMPUTE �����������S�O = �����������S�O�v�q / 100
020070     END-IF.
020080     MOVE ���������v�S�O�v�q           TO ���������v�S�O.
020090*
020100*��***********************************************************************
020110* �T���ʁ^2.5���̈󎚂͕K�v�Ȃ��B
020120*------------------------------------------------------------------------*
020130*    *****************
020140*    * �T���ʁ^2.5�� *
020150*    *****************
020160*     MOVE ��ÒP���T�Q�v�q             TO ��ÒP���T�Q.
020170*     MOVE ��É񐔂T�Q�v�q             TO ��É񐔂T�Q.
020180*     MOVE ��×��T�Q�v�q               TO ��×��T�Q.
020190*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
020200*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
020210*     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
020220*     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
020230*     MOVE �d�É񐔂T�Q�v�q             TO �d�É񐔂T�Q.
020240*     MOVE �d�×��T�Q�v�q               TO �d�×��T�Q.
020250*     MOVE ���v�T�Q�v�q                 TO ���v�T�Q.
020260*     MOVE �����ʍ����v�T�Q�v�q         TO �����ʍ����v�T�Q.
020270*     IF ( �����������T�Q�v�q NOT = ZERO )
020280*         COMPUTE �����������T�Q = �����������T�Q�v�q / 100
020290*     END-IF.
020300*     MOVE ���������v�T�Q�v�q           TO ���������v�T�Q.
020310*��***********************************************************************
020320*
020330*    ****************
020340*    * �T���ʁ^�T�� *
020350*    ****************
020360*     MOVE �����J�n���T�T�v�q           TO �����J�n���T�T.
020370*     MOVE �����J�n���T�T�v�q           TO �����J�n���T�T.
020410*     MOVE ��ÒP���T�T�v�q             TO ��ÒP���T�T.
020420*     MOVE ��É񐔂T�T�v�q             TO ��É񐔂T�T.
020430*     MOVE ��×��T�T�v�q               TO ��×��T�T.
020440*     MOVE ��㪖@�񐔂T�T�v�q           TO ��㪖@�񐔂T�T.
020450*     MOVE ��㪖@���T�T�v�q             TO ��㪖@���T�T.
020460*     MOVE ��㪖@�񐔂T�T�v�q           TO ��㪖@�񐔂T�T.
020470*     MOVE ��㪖@���T�T�v�q             TO ��㪖@���T�T.
020480*     MOVE �d�É񐔂T�T�v�q             TO �d�É񐔂T�T.
020490*     MOVE �d�×��T�T�v�q               TO �d�×��T�T.
020500*     MOVE ���v�T�T�v�q                 TO ���v�T�T.
020510*     MOVE �����ʍ����v�T�T�v�q         TO �����ʍ����v�T�T.
020520*     IF ( �����������T�T�v�q NOT = ZERO )
020530*         COMPUTE �����������T�T = �����������T�T�v�q / 100
020540*     END-IF.
020550*     MOVE ���������v�T�T�v�q           TO ���������v�T�T.
020560*    ****************
020570*    * �T���ʁ^�W�� *
020580*    ****************
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
020590*     MOVE �����J�n���T�W�v�q           TO �����J�n���T�W.
020600*     MOVE �����J�n���T�W�v�q           TO �����J�n���T�W.
020640*     MOVE ��ÒP���T�W�v�q             TO ��ÒP���T�W.
020650*     MOVE ��É񐔂T�W�v�q             TO ��É񐔂T�W.
020660*     MOVE ��×��T�W�v�q               TO ��×��T�W.
020670*     MOVE ��㪖@�񐔂T�W�v�q           TO ��㪖@�񐔂T�W.
020680*     MOVE ��㪖@���T�W�v�q             TO ��㪖@���T�W.
020690*     MOVE ��㪖@�񐔂T�W�v�q           TO ��㪖@�񐔂T�W.
020700*     MOVE ��㪖@���T�W�v�q             TO ��㪖@���T�W.
020710*     MOVE �d�É񐔂T�W�v�q             TO �d�É񐔂T�W.
020720*     MOVE �d�×��T�W�v�q               TO �d�×��T�W.
020730*     MOVE ���v�T�W�v�q                 TO ���v�T�W.
020740*     MOVE �����ʍ����v�T�W�v�q         TO �����ʍ����v�T�W.
020750*     IF ( �����������T�W�v�q NOT = ZERO )
020760*         COMPUTE �����������T�W = �����������T�W�v�q / 100
020770*     END-IF.
020780*     MOVE ���������v�T�W�v�q           TO ���������v�T�W.
020790*    ****************
020800*    * �T���ʁ^10�� *
020810*    ****************
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
020820*     MOVE �����J�n���T�O�v�q           TO �����J�n���T�O.
020830*     MOVE �����J�n���T�O�v�q           TO �����J�n���T�O.
020870*     MOVE ��ÒP���T�O�v�q             TO ��ÒP���T�O.
020880*     MOVE ��É񐔂T�O�v�q             TO ��É񐔂T�O.
020890*     MOVE ��×��T�O�v�q               TO ��×��T�O.
020900*     MOVE ��㪖@�񐔂T�O�v�q           TO ��㪖@�񐔂T�O.
020910*     MOVE ��㪖@���T�O�v�q             TO ��㪖@���T�O.
020920*     MOVE ��㪖@�񐔂T�O�v�q           TO ��㪖@�񐔂T�O.
020930*     MOVE ��㪖@���T�O�v�q             TO ��㪖@���T�O.
020940*     MOVE �d�É񐔂T�O�v�q             TO �d�É񐔂T�O.
020950*     MOVE �d�×��T�O�v�q               TO �d�×��T�O.
020960*     MOVE ���v�T�O�v�q                 TO ���v�T�O.
020970*     MOVE �����������T�O�v�q           TO �����������T�O.
020980*     IF ( �����������T�O�v�q NOT = ZERO )
020990*         COMPUTE �����������T�O = �����������T�O�v�q / 100
021000*     END-IF.
021010*     MOVE ���������v�T�O�v�q           TO ���������v�T�O.
021020*
021327*------------------------------------------------------------------------*
      *
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
           IF ( �{�p�a��N���v�q >= 43006 )
              INITIALIZE �A���^�|�L�[
019550        MOVE �{�p�a��v�q TO �A���^�|�{�p�a��
019560        MOVE �{�p�N�v�q   TO �A���^�|�{�p�N
019570        MOVE �{�p���v�q   TO �A���^�|�{�p��
019580        MOVE ���Ҕԍ��v�q TO �A���^�|���Ҕԍ�
019590        MOVE �}�Ԃv�q     TO �A���^�|�}��
              MOVE �A���|�ی���� TO �A���^�|�ی����
              MOVE 44           TO �A���^�|��R�[�h
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
021330*    ************
021340*    * ���v���z *
021350*    ************
021360     MOVE ���Z�|���v             TO ���v.
021380     MOVE ���Z�|�ꕔ���S��       TO �ꕔ���S��.
021440     MOVE ���Z�|�������z         TO �������z.
           IF �A���|�ی���� > 50
              EVALUATE TRUE
              WHEN ����S�Ҕԍ��v(3:2) = 27
021440            MOVE ���Z�|�󋋎ҕ��S�z TO �ꕔ���S��
021440            MOVE ���Z�|�����������z TO �������z
              WHEN OTHER
021370            MOVE ���Z�|�󋋎ҕ��S�z       TO �󋋎ҕ��S�z
021380            MOVE ���Z�|�����������z       TO ���������z
                  MOVE "�ꕔ���S�������z�i��Ï�����j" TO �ꕔ���S���b�l
                  MOVE "�������z�i��Ï�����j"         TO �������z�b�l
                  MOVE "�~" TO �ꕔ���S���~�b�l �������z�~�b�l
              END-EVALUATE
           END-IF.
021450*
021460**************
021470* �K�p�Z�b�g *
021480**************
021490     MOVE �K�p�P�v               TO �K�p�P.
021500     MOVE �K�p�Q�v               TO �K�p�Q.
021510***     MOVE �K�p�R�v               TO �K�p�R.
021520***     MOVE �K�p�S�v               TO �K�p�S.
021530*
      *
      */���A�L�� �{�̃��Z�ɕ��S�Ҕԍ��A�󋋎Ҕԍ�����
      */�������Z�ɏ����}�[�N
           IF ����S�Ҕԍ��v(3:2) = "27" OR "34"
               IF ����S�Ҕԍ��v(1:2) = "99"
                   MOVE SPACE            TO ����S�Ҕԍ�
               ELSE
                   MOVE ����S�Ҕԍ��v TO ����S�Ҕԍ�
               END-IF
      */�󋋎Ҕԍ����W�����ȏ�̏ꍇ�g�𖳎����Ĉ������/110425
               IF ����󋋎Ҕԍ��Q�v = SPACE
                   MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
               ELSE
                   MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
               END-IF
               IF �A���|�ی���� > 50
                   MOVE ������v         TO ������
                   MOVE NC"��"           TO �����p�}��
               END-IF
           ELSE
      */���A�L���ȊO �{�̃��Z�ɏ����}�[�N
               IF (��|������� NOT = ZERO) AND
                  (�A���|�ی����   < 50  ) AND
                  (������v NOT = SPACE)
                   MOVE ������v         TO ������
                   MOVE NC"��"           TO �����p�}��
               END-IF
           END-IF.
      */�ޗǌ�����(���ۑސE�͕��S�Ҕԍ��A�󋋎Ҕԍ����L�ځA����ȊO�́u�ޗǌ�������Áv�ƋL��)
           IF ����S�Ҕԍ��v(3:2) = "29"
               IF ��|�ی���� = 01 OR 08 OR 05
                   IF ����S�Ҕԍ��v(1:2) = "99"
                       MOVE SPACE        TO ����S�Ҕԍ�
                   ELSE
                       IF (��|������� = 52  AND ����S�Ҕԍ��v(1:2) = "91") OR
                          (��|������� = 53  AND ����S�Ҕԍ��v(1:2) = "81") OR
                          (��|������� = 55  AND ����S�Ҕԍ��v(1:2) = "71")
                           MOVE ����S�Ҕԍ��v TO ����S�Ҕԍ�
                       END-IF
                   END-IF
      *
                   IF ( ����󋋎Ҕԍ��v(1:1) = "*"  ) OR
                      ( ����󋋎Ҕԍ��v(1:2) = "��" )
                      MOVE  SPACE                TO �󋋎Ҕԍ�
                   ELSE
      *    /�󋋎Ҕԍ����W�����ȏ�̏ꍇ�g�𖳎����Ĉ������/110425
                       IF ����󋋎Ҕԍ��Q�v = SPACE
                           MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
                       ELSE
                           MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
                       END-IF
                   END-IF
               ELSE
                   IF (��|������� = 52  AND ����S�Ҕԍ��v(1:2) = "91") OR
                      (��|������� = 53  AND ����S�Ҕԍ��v(1:2) = "81") OR
                      (��|������� = 55  AND ����S�Ҕԍ��v(1:2) = "71")
033830                 STRING �K�p�P             DELIMITED BY SPACE
036850                        NC"�C"             DELIMITED BY SIZE
036860                      NC"�ޗǌ��������"   DELIMITED BY SIZE
033870                      INTO �K�p�P
034720                 END-STRING
                   END-IF
               END-IF
           END-IF.
      */32140410�_�ސ쌧�s�����E���{�픚�̏����̏ꍇ�A�{�̃��Z�ɂ����S�Ҕԍ��Ǝ󋋎Ҕԍ����������/111213
           IF (��|�ی��Ҕԍ� = 32140410) AND (��|������� = 54)
               IF ����S�Ҕԍ��v(1:2) = "99"
                   MOVE SPACE            TO ����S�Ҕԍ�
               ELSE
                   MOVE ����S�Ҕԍ��v TO ����S�Ҕԍ�
               END-IF
               IF ����󋋎Ҕԍ��Q�v = SPACE
                   MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
               ELSE
                   MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
               END-IF
           END-IF.
      */�����a�̏ꍇ�A�{�̃��Z�ɂ����S�Ҕԍ��Ǝ󋋎Ҕԍ����������/150610
      *     IF (��|�ی���� NOT = 05) AND (��|���ʋ敪 = 2) AND
      *        (��|������� = 60) 
               EVALUATE ����S�Ҕԍ��v
               WHEN "51433019"
               WHEN "51433027"
               WHEN "51433035"
               WHEN "51433043"
               WHEN "51153013"
               WHEN "51153021"
               WHEN "51463016"
               WHEN "51463024"
                   MOVE ����S�Ҕԍ��v TO ����S�Ҕԍ�
                   IF ����󋋎Ҕԍ��Q�v = SPACE
                       MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
                   ELSE
                       MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
                   END-IF
               END-EVALUATE.
      *     END-IF.
019850*
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
021680**********************
021690* �{�p���f�[�^�Z�b�g *
021700**********************
           MOVE �s���{���i�h�r�v       TO �s���{���ԍ�.
021710     MOVE �_���t�ԍ��v           TO �_���t�ԍ�.
           MOVE ���ϔԍ��v             TO ���ϔԍ�.
           PERFORM ����ԍ��E�l��.
           MOVE ����ԍ��E�l�߂v     TO ����ԍ�.
021730***     MOVE ��z���󗝔ԍ��v       TO ��z���󗝔ԍ�.
021740*
021760     MOVE �{�p���X�֔ԍ��P�v     TO �{�p���X�֔ԍ��P.
022750     MOVE "-"                    TO �{�p���X�֔ԍ����.
021770     MOVE �{�p���X�֔ԍ��Q�v     TO �{�p���X�֔ԍ��Q.
021800     MOVE �{�p���Z���P�v         TO �{�p���Z���P.
021810     MOVE �{�p���Z���Q�v         TO �{�p���Z���Q.
021820     MOVE �ڍ��@���v             TO �ڍ��@��.
021830     MOVE ��\�҃J�i�v           TO ��\�҃J�i.
021840     MOVE ��\�Җ��v             TO ��\�Җ�.
021850     MOVE �{�p���d�b�ԍ��v       TO �{�p���d�b�ԍ�.
022250     MOVE ��ϔC���P�v         TO ��ϔC�R�����g�Q.
022260     MOVE ��ϔC���Q�v         TO ��ϔC�R�����g�R.
022260     MOVE ��ϔC���R�v         TO ��ϔC�R�����g�S.
022260     MOVE ��ϔC���S�v         TO ��ϔC�R�����g�T.
      *
021940     MOVE �����ԍ��v             TO �����ԍ�.
021950     MOVE �������`�l�J�i�v       TO �������`�l�J�i.
021960     MOVE �������`�l�v           TO �������`�l.
           MOVE ���Z�@�֖��P�v         TO ���Z�@�֖��P.
           MOVE ���Z�@�֖��Q�v         TO ���Z�@�֖��Q.
           MOVE ���Z�@�֖��R�v         TO ���Z�@�֖��R.
           MOVE ���Z�@�֖��S�v         TO ���Z�@�֖��S.
           MOVE �x�X���P�v             TO �x�X���P.
           MOVE �x�X���Q�v             TO �x�X���Q.
           MOVE �x�X���R�v             TO �x�X���R.
           MOVE �x�X���S�v             TO �x�X���S.
           MOVE �U���`�F�b�N�v         TO �U���`�F�b�N.
           MOVE ���ʃ`�F�b�N�v         TO ���ʃ`�F�b�N.
           MOVE �����`�F�b�N�v         TO �����`�F�b�N.
           MOVE ��s�`�F�b�N�v         TO ��s�`�F�b�N.
           MOVE ���Ƀ`�F�b�N�v         TO ���Ƀ`�F�b�N.
           MOVE �_���`�F�b�N�v         TO �_���`�F�b�N.
           MOVE �{�X�`�F�b�N�v         TO �{�X�`�F�b�N.
           MOVE �x�X�`�F�b�N�v         TO �x�X�`�F�b�N.
           MOVE �{�x���`�F�b�N�v       TO �{�x���`�F�b�N.
021970*
021980* / �_���t�E���҈ϔC�� /
           MOVE �_���t�a��v           TO ���|�����敪.
037380     READ �����}�X�^
037390     NOT INVALID KEY
037400         MOVE ���|��������       TO �󗝘a��
037410     END-READ.
021990     MOVE �_���t�N�v             TO �󗝔N.
022000     MOVE �_���t���v             TO �󗝌�.
022010     MOVE �_���t���v             TO �󗝓�.
022020* ( �ϔC�N���� ������邩 )
022030     IF ( �A���|�ϔC���  = ZERO )
               MOVE ���҈ϔC�a��v     TO ���|�����敪
037380         READ �����}�X�^
037390         NOT INVALID KEY
037400             MOVE ���|��������   TO �ϔC�a��
037410         END-READ
022040         MOVE ���҈ϔC�N�v       TO �ϔC�N
022050         MOVE ���҈ϔC���v       TO �ϔC��
022060         MOVE ���҈ϔC���v       TO �ϔC��
022070     END-IF.
022080*
022090* �{�pID
022100     MOVE ���{�p�h�c�v           TO ���{�p�h�c.
      */�����̎{�p���h�c�����͂���Ă���ꍇ�͗D�悷��/120711
           IF �s�����{�p�h�c�v NOT = SPACE
      */���s�s�̌���{��Q/120606
               IF (��|�ی���� = 05 AND ��|������� = 53) AND
                  (��|��p���S�Ҕԍ�����(1:5) = "39261" OR "43264")
022020             MOVE �s�����{�p�h�c�v TO ���{�p�h�c
               END-IF
           END-IF.
022110*
022120* ���ʃR�����g
022130*     MOVE ���ʃR�����g�v         TO ���ʃR�����g.
022140*
      */ �ی���ʁA�����A��ی��Җ�������ԂɈ�� /150219
           IF ��|�ی���� = 01
               MOVE ��|�ی��Ҕԍ�(1:2)  TO ���ԍ��v
           ELSE
               MOVE ��|�ی��Ҕԍ�(3:2)  TO ���ԍ��v
           END-IF.
           IF �A���|�ی���� > 50
               MOVE ��|��p���S�Ҕԍ�����(3:2)  TO ���ԍ��v
           END-IF.
025960     MOVE 13                     TO ���|�敪�R�[�h.
025970     MOVE ���ԍ��v               TO ���|���̃R�[�h.
025980     READ ���̃}�X�^
025990     INVALID KEY
026000         MOVE SPACE              TO ���b�l�v
026010     NOT INVALID KEY
026020         MOVE ���|����           TO ���b�l�v
026030     END-READ.
           STRING "["                  DELIMITED BY SIZE
                  ���b�l�v�o           DELIMITED BY "�@"
                  "]"                  DELIMITED BY SIZE
             INTO ���b�l
           END-STRING.
           STRING "<"                  DELIMITED BY SIZE
                  �ی���ʂv           DELIMITED BY SPACE
                  ������ʂv           DELIMITED BY SPACE
                  ">"                  DELIMITED BY SIZE
             INTO �ی���ʂb�l
           END-STRING.
           IF �A���|�ی���� > 50
016660        MOVE ���Ҏ����v          TO ��ی��Җ�
           ELSE
016660        MOVE ��ی��Ҏ����v      TO ��ی��Җ�
           END-IF.
      *
           MOVE �p�q�f�[�^�v                 TO �p�q�R�[�h.
011710*      MOVE "X" TO EDIT-MODE OF �p�q�R�[�h.
      *
           MOVE ��|���҃R�[�h   TO ��T�|���҃R�[�h.
           MOVE ��|�{�p�a��N�� TO ��T�|�{�p�a��N��.
           IF �A���|�ی���� > 50
              MOVE ��|�������  TO ��T�|�ی����
           ELSE
              MOVE ��|�ی����  TO ��T�|�ی����
           END-IF.
           READ ��ƃt�@�C���T
           NOT INVALID KEY
              MOVE ��T�|�U������   TO �敪�P
              MOVE ��T�|�V������   TO �敪�Q
              MOVE ��T�|���ҏ���   TO �敪�R
              MOVE "-"              TO ��؂P ��؂Q
              IF ��T�|���ރR�[�h = 3 OR 4 OR 6
                 MOVE "�~"          TO ��
              END-IF
              START ��ƃt�@�C���T KEY IS >= ��T�|�U������
                                             ��T�|�V������
                                             ��T�|���ҏ���
              END-START
              READ ��ƃt�@�C���T NEXT
              NOT AT END
                 READ ��ƃt�@�C���T NEXT
                 AT END
                    MOVE "�Y"             TO ����
                 NOT AT END
                    IF (��T�|�U������ NOT = �敪�P) OR
                       (��T�|�V������ NOT = �敪�Q)
                       MOVE "�Y"          TO ����
                    END-IF
                 END-READ
              END-READ
           END-READ.
022150************************
022160* ���Z�v�g���я��Z�b�g *
022170************************
022180     MOVE ���Ԃv                 TO ����.
022190*
022200*-------------------------------------------------------------------------*
022210*--- �� ���Z�E�v�ăZ�b�g�́A���̈���Z�b�gSECTION �̍Ō�ɂ�邱�ƁI -----*
022220     PERFORM ���Z�E�v�ăZ�b�g.
022230*-------------------------------------------------------------------------*
022240*
022250*--- TEST ---*
022260*******     PERFORM �e�X�g�󎚏���.
022270*
022280*================================================================*
022290 ���ڏ����� SECTION.
022300*================================================================*
022310     INITIALIZE �{�p�����v.
022320     INITIALIZE ��f�ҏ��v.
022330     INITIALIZE �������v.
022340     INITIALIZE �������v.
022350*     INITIALIZE ���S���`�F�b�N�v.
022360     INITIALIZE ���l���v.
022370*
022380     INITIALIZE �����P�v�q.
022390     INITIALIZE �����Q�v�q.
022400     INITIALIZE �����R�v�q.
022410*
022420     MOVE SPACE TO YHN6121P.
022430*****     INITIALIZE YHN6121P.
022440*
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
022450*================================================================*
022460 �{�p�����擾 SECTION.
022470*================================================================*
022480**************************************************
022490* �{�@�f�[�^���g�p���A�ȉ��̏����擾           *
022500* �� �_���t�ԍ�.. �_���t�ԍ��v�Ɋi�[             *
022510* �� ����ԍ� ... �ڍ��t�����ԍ��v�Ɋi�[       *
022520* �� ��\�Җ� ... ��\�Җ��v�Ɋi�[               *
022530* �� �Z��1,2   ...�{�p���Z��1,2�v�Ɋi�[          *
022540* �� �d�b�ԍ� ... �{�p���d�b�ԍ��v�Ɋi�[         *
022550**************************************************
022560     MOVE ZERO  TO �{��|�{�p���ԍ�.
022570     READ �{�p�����}�X�^
022580     INVALID KEY
022590         CONTINUE
022600     NOT INVALID KEY
022610*
022650         MOVE �{��|�V�_���t�ԍ�  TO �_���t�ԍ��v
023280* ���ρE���q���̎��̂݁A�_���t�ԍ��̕ҏW������B
023290         IF ( ���Z�v�g��ނv = "ROUJ"  OR "JYOS" )
023300            CONTINUE
023310         ELSE
023320            EVALUATE �ی���ʂv�q 
023330            WHEN  04
023340                PERFORM ���ϔԍ��Z�b�g
023350            WHEN  09
023360                PERFORM ���q���ԍ��Z�b�g
023370            END-EVALUATE
023380         END-IF
022670*
               MOVE �{��|�s���{���i�h�r    TO �s���{���i�h�r�v
022680         MOVE �{��|�ڍ��t�����ԍ�  TO �ڍ��t�����ԍ��v
022690*
022700         MOVE �{��|��\�҃J�i        TO ��\�҃J�i�v
022710         MOVE �{��|��\�Җ�          TO ��\�Җ��v
022720         MOVE �{��|�ڍ��@��          TO �ڍ��@���v
022730*
022740         MOVE �{��|�X�֔ԍ��P        TO �{�p���X�֔ԍ��P�v
022750         MOVE �{��|�X�֔ԍ��Q        TO �{�p���X�֔ԍ��Q�v
022760         MOVE �{��|�Z���P            TO �{�p���Z���P�v
022770         MOVE �{��|�Z���Q            TO �{�p���Z���Q�v
022780         MOVE �{��|�d�b�ԍ�          TO �{�p���d�b�ԍ��v
022790*
022800         MOVE �{��|������s��      TO ������s���v
022810         MOVE �{��|������s�x�X��  TO ������s�x�X���v
022820         MOVE �{��|�a�����          TO �a����ʂv
022830         MOVE �{��|�����ԍ�          TO �����ԍ��v
022840         MOVE �{��|�������`�l        TO �������`�l�v
022850         MOVE �{��|�������`�l�J�i    TO �������`�l�J�i�v
023490*
023500** �U������  / ����}�X�^���U��������擾 /
023520         MOVE ZERO  TO  ���|�_���I���敪
023510         MOVE 44    TO  ���|����R�[�h
023520         MOVE ZERO  TO  ���|�ی����
023530         MOVE ZERO  TO  ���|�ύX�a��N��
023540         READ ����}�X�^
023550         NOT INVALID KEY
023560             MOVE ���|������s��      TO ������s���v
023570             MOVE ���|������s�x�X��  TO ������s�x�X���v
023580             MOVE ���|�a�����          TO �a����ʂv
023590             MOVE ���|�����ԍ�          TO �����ԍ��v
023600             MOVE ���|�������`�l        TO �������`�l�v
023610             MOVE ���|�������`�l�J�i    TO �������`�l�J�i�v
               END-READ.
022860*
               MOVE  ������s���v     TO ���Z�@�֖��v
               MOVE  ������s�x�X���v TO �x�X���v
022920*
022930         EVALUATE �a����ʂv
022940         WHEN 1
022950             MOVE "(��)" TO �a����ʃR�����g�v
022960         WHEN 2
022970             MOVE "(��)" TO �a����ʃR�����g�v
022980         WHEN OTHER
022990             MOVE SPACE  TO �a����ʃR�����g�v
023000         END-EVALUATE
023010*
023020*********************************************
023030** �h�c�Ǘ��}�X�^���@���{�p�h�c���擾����B
023040*   (���ۑg���́A�ΏۊO)
023050*********************************************
023060**   / ���{�pID /
023070*         IF ( �ی��Ҕԍ��v�q(3:1) NOT = "3" )
023080*            MOVE 01                  TO �h�c�ǁ|�h�c�敪
023090*            MOVE ZERO                TO �h�c�ǁ|�{�p���ԍ�
023100*            MOVE �ی��Ҕԍ��v�q(1:2) TO �h�c�ǁ|�ی����
023110*            MOVE SPACE               TO �h�c�ǁ|�ی��Ҕԍ�
023120*            READ �h�c�Ǘ��}�X�^
023130*            NOT INVALID KEY
023140*                MOVE �h�c�ǁ|�{�p�h�c�ԍ� TO ���{�p�h�c�v
023150*            END-READ
023160*         END-IF
023170*     END-READ.
      */����͐U���̂ݑΉ�
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
009765                 END-IF
009766              END-IF
009767           END-IF
009768        ELSE
009769           MOVE  ������s���v  TO ���Z�@�֖��v
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
009801                     END-IF
009804                 END-IF
009805              END-IF
009806           END-IF
009807        ELSE
009808           MOVE  ������s�x�X���v  TO �x�X���v
009809        END-IF
009810     END-IF.
      *
023820* �Œ��
      */��̕ύX/20210824
      *     MOVE "�܂��A�×{��̎�̂�(��)���[���hSPS�ی����� ��"       TO ��ϔC���P�v.
      *     MOVE "�\����� ���� �z��(���s�Z�g�撷����4-12-5�T��"      TO ��ϔC���Q�v.
           MOVE "�܂��A�×{��̎�̂�(��)���[���h�ی����� ��\"       TO ��ϔC���P�v.
           MOVE "����� ���� �z��(���s�Z�g�撷����4-12-5�T��"      TO ��ϔC���Q�v.
           MOVE "�V���C���r��1F)�ɈϔC���܂��B"                        TO ��ϔC���R�v.
      *
      *     MOVE "                    ���[���hSPS�ی�����"              TO �ϔC���P.
      *     MOVE "                      ������ �I�R ���i "              TO �ϔC���Q.
      */��̕ύX/20210824
      *     MOVE "                    ���[���hSPS�ی�����"              TO �ϔC���P.
           MOVE "                       ���[���h�ی�����"              TO �ϔC���P.
           MOVE "    ��558-0004 ���s�Z�g�撷����4-12-5"              TO �ϔC���Q.
           MOVE "                           �T���V���C���r��1F"        TO �ϔC���R.
           MOVE "                           (�d�b)06-6586-9155"        TO �ϔC���S.
023180*
023970*================================================================*
023980 ���ϔԍ��Z�b�g SECTION.
023990*
024000**************************************************************
024010* �ی��Ҕԍ��ɂ��A���ς̔ԍ����󎚂��邩�A�_���t�ԍ�������
024020**************************************************************
024030** 1.���ϑg���A��
024040     MOVE SPACE  TO  �E�o�t���O.
024050     IF ( �{��|���ϘA�ԍ� NOT = ZERO )
024060** ����(�ی��Ҕԍ�)
024070        IF ( �ی��Ҕԍ��v�q(1:2) = "31" )  OR
024080           ( �ی��Ҕԍ��v�q = "34130021" )
024090*
024100           MOVE  NC"���ϑg���A����"   TO ���ϘA�ԍ����m�v 
024110           MOVE  NC"��"               TO ���ϘA�ԍ��P�ʂm�v 
024120           MOVE  �{��|���ϘA�ԍ�     TO ���ϘA�ԍ��v
024130           IF    (���ϘA�ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
024140                 MOVE SPACE TO  ���ϘA�ԍ��v(1:1)
024150           ELSE
024160                 MOVE "YES" TO  �E�o�t���O
024170           END-IF
024180           IF    (���ϘA�ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
024190                 MOVE SPACE TO  ���ϘA�ԍ��v(2:1)
024200           ELSE
024210                 MOVE "YES" TO  �E�o�t���O
024220           END-IF
024230           IF    (���ϘA�ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
024240                 MOVE SPACE TO  ���ϘA�ԍ��v(3:1)
024250           ELSE
024260                 MOVE "YES" TO  �E�o�t���O
024270           END-IF
024280           IF    (���ϘA�ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
024290                 MOVE SPACE TO  ���ϘA�ԍ��v(4:1)
024300           ELSE
024310                 MOVE "YES" TO  �E�o�t���O
024320           END-IF
024330           IF    (���ϘA�ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
024340                 MOVE SPACE TO  ���ϘA�ԍ��v(5:1)
024350           ELSE
024360                 MOVE "YES" TO  �E�o�t���O
024370           END-IF
024380           IF    (���ϘA�ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
024390                 MOVE SPACE TO  ���ϘA�ԍ��v(6:1)
024400           ELSE
024410                 MOVE "YES" TO  �E�o�t���O
024420           END-IF
027560**/���ώ������󎚂���/090608
027570*           MOVE  �_���t�ԍ��v         TO �_���t�ԍ��Q�v
027580*           MOVE  ���ϘA�ԍ��W�c�v     TO �_���t�ԍ��v
024110            MOVE  ���ϘA�ԍ��W�c�v     TO ���ϔԍ��v
024440        END-IF
024450     END-IF.
024460*
027620** 2. �n���ϋ��c��
027630     MOVE SPACE  TO  �E�o�t���O.
027640     IF �{��|�n���ϘA�ԍ� NOT = ZERO
027650** ����(�ی��Ҕԍ�)
027660        IF ( �ی��Ҕԍ��v�q(1:2) = "32" OR "33" OR "34" )  AND
027670           ( �ی��Ҕԍ��v�q NOT = "34130021" )
027680*
027690           MOVE  NC"�n���ϋ��c��"     TO ���ϘA�ԍ����m�v 
027700           MOVE  NC"��"               TO ���ϘA�ԍ��P�ʂm�v 
027710           MOVE  �{��|�n���ϘA�ԍ�   TO ���ϘA�ԍ��v
027720           IF    (���ϘA�ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
027730                 MOVE SPACE TO  ���ϘA�ԍ��v(1:1)
027740           ELSE
027750                 MOVE "YES" TO  �E�o�t���O
027760           END-IF
027770           IF    (���ϘA�ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
027780                 MOVE SPACE TO  ���ϘA�ԍ��v(2:1)
027790           ELSE
027800                 MOVE "YES" TO  �E�o�t���O
027810           END-IF
027820           IF    (���ϘA�ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
027830                 MOVE SPACE TO  ���ϘA�ԍ��v(3:1)
027840           ELSE
027850                 MOVE "YES" TO  �E�o�t���O
027860           END-IF
027870           IF    (���ϘA�ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
027880                 MOVE SPACE TO  ���ϘA�ԍ��v(4:1)
027890           ELSE
027900                 MOVE "YES" TO  �E�o�t���O
027910           END-IF
027920           IF    (���ϘA�ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
027930                 MOVE SPACE TO  ���ϘA�ԍ��v(5:1)
027940           ELSE
027950                 MOVE "YES" TO  �E�o�t���O
027960           END-IF
027970           IF    (���ϘA�ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
027980                 MOVE SPACE TO  ���ϘA�ԍ��v(6:1)
027990           ELSE
028000                 MOVE "YES" TO  �E�o�t���O
028010           END-IF
028020**/���ώ������󎚂���/090608
028030*           MOVE  �_���t�ԍ��v         TO �_���t�ԍ��Q�v
028040*           MOVE  ���ϘA�ԍ��W�c�v     TO �_���t�ԍ��v
024110            MOVE  ���ϘA�ԍ��W�c�v     TO ���ϔԍ��v
                  MOVE "�n��"                TO �ی���ʂv
028050        END-IF
028060     END-IF.
024900*
024910*================================================================*
024920 ���q���ԍ��Z�b�g SECTION.
024930*
028110     MOVE SPACE  TO  �E�o�t���O.
028120     IF �{��|���q���ԍ� NOT = ZERO
028130           IF �{��|�h�q�ȋ敪 = 1
028140              MOVE  NC"�h�q�ȑ�"      TO ���q���ԍ����m�v 
028150           ELSE
028160              MOVE  NC"�h�q����"      TO ���q���ԍ����m�v 
028170           END-IF
028180           MOVE  NC"��"               TO ���q���ԍ��P�ʂm�v 
028190           MOVE  �{��|���q���ԍ�     TO ���q���ԍ��v
028200           IF    (���q���ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
028210                 MOVE SPACE TO  ���q���ԍ��v(1:1)
028220           ELSE
028230                 MOVE "YES" TO  �E�o�t���O
028240           END-IF
028250           IF    (���q���ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
028260                 MOVE SPACE TO  ���q���ԍ��v(2:1)
028270           ELSE
028280                 MOVE "YES" TO  �E�o�t���O
028290           END-IF
028300           IF    (���q���ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
028310                 MOVE SPACE TO  ���q���ԍ��v(3:1)
028320           ELSE
028330                 MOVE "YES" TO  �E�o�t���O
028340           END-IF
028350           IF    (���q���ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
028360                 MOVE SPACE TO  ���q���ԍ��v(4:1)
028370           ELSE
028380                 MOVE "YES" TO  �E�o�t���O
028390           END-IF
028400           IF    (���q���ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
028410                 MOVE SPACE TO  ���q���ԍ��v(5:1)
028420           ELSE
028430                 MOVE "YES" TO  �E�o�t���O
028440           END-IF
028450           IF    (���q���ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
028460                 MOVE SPACE TO  ���q���ԍ��v(6:1)
028470           ELSE
028480                 MOVE "YES" TO  �E�o�t���O
028490           END-IF
028500*           MOVE  ���q���ԍ��W�c�v     TO �_���t�ԍ��v
028500         MOVE  ���q���ԍ��W�c�v     TO ���ϔԍ��v
028510     END-IF.
025310*
023190*================================================================*
023200 ��f�ҏ��擾 SECTION.
023210*================================================================*
023220**************************************************
023230* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
023240* �� �{�p�N ..... �{�p�N�v�Ɋi�[                 *
023250* �� �{�p�� ..... �{�p���v�Ɋi�[                 *
023260* �� �L�� ....... �L���v�Ɋi�[                   *
023270* �� �ԍ� ....... �ԍ��v�Ɋi�[                   *
023280* �� �ی��Ҕԍ� . �ی��Ҕԍ��v�Ɋi�[             *
023290* �� �ی���� ... �ی���ʂv�Ɋi�[               *
023300* �� ��ی��҃J�i.��ی��҃J�i�v�Ɋi�[           *
023310* �� ��ی��Ҏ���.��ی��Ҏ����v�Ɋi�[           *
023320* �� �Z���P ......��ی��ҏZ���P�v�Ɋi�[         *
023330* �� �Z���Q ......��ی��ҏZ���Q�v�Ɋi�[         *
023340* �� ���҃J�i ....���҃J�i�v�Ɋi�[               *
023350* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
023360* �� ���Ґ��� ....�敪�ɂ��`�F�b�N��"��"���i�[ *
023370* �� ���Ҙa�� ....�a��ɂ��`�F�b�N��"��"���i�[ *
023380* �� ���ҔN ......���ҔN�v�Ɋi�[                 *
023390* �� ���Ҍ� ......���Ҍ��v�Ɋi�[                 *
023400* �� ���ғ� ......���ғ��v�Ɋi�[                 *
023410* �� ���� ........���̃}�X�^��葱���v�Ɏ擾     *
023420**************************************************
           IF ��|���R�[�h NOT = SPACE
               IF (��|������� = 53 ) AND (��|��p���S�Ҕԍ�����(1:5) = "39261")
                   EVALUATE ���Z�|���S����
                   WHEN 0
                       MOVE "�O"    TO �����v
                   WHEN 1
                       MOVE "�P"    TO �����v
                   WHEN 2
                       MOVE "�Q"    TO �����v
                   WHEN 3
                       MOVE "�R"    TO �����v
                   END-EVALUATE
                   STRING "���N�Ǘ���@�@"   DELIMITED BY SIZE
                          �����v             DELIMITED BY SIZE
                          "��"               DELIMITED BY SIZE
                     INTO �^�C�g���v
                   END-STRING
                   MOVE ALL "="      TO ������v
               END-IF
      */�����C��/20190426
               MOVE ��|�{�p�a��     TO �{�p�a��v
023520         MOVE ��|�{�p�N       TO �{�p�N�v
023530         MOVE ��|�{�p��       TO �{�p���v
023540*         MOVE ��|�L��         TO �L���v
023550*         MOVE ��|�ԍ�         TO �ԍ��v
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
               MOVE ��|��p���S�Ҕԍ����� TO ����S�Ҕԍ��v
               MOVE ��|��v�Ҕԍ�����     TO �󋋎Ҕԍ��v
023560** �S���y�؂̎}�ԍ폜
023570         IF ( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(1:6) = "133033" )
023580            MOVE ��|�ی��Ҕԍ�(1:6) TO �ی��Ҕԍ��v
023590         ELSE
023600            MOVE ��|�ی��Ҕԍ�      TO �ی��Ҕԍ��v
023610         END-IF
022660         EVALUATE ��|�ی����
022670         WHEN 01
022690            MOVE NC"��"        TO ���ۃ`�F�b�N�v
                  MOVE "����"        TO �ی���ʂv
022700         WHEN 02
022710         WHEN 06
022720            MOVE NC"��"        TO �Еۃ`�F�b�N�v
                  MOVE "����"        TO �ی���ʂv
022750         WHEN 07
022720            MOVE NC"��"        TO �Еۃ`�F�b�N�v
                  MOVE "�D��"        TO �ی���ʂv
022730         WHEN 03
022740            MOVE NC"��"        TO �g���`�F�b�N�v
                  MOVE "�g��"        TO �ی���ʂv
               WHEN 04
                  MOVE NC"��"        TO ���σ`�F�b�N�v
                  MOVE "����"        TO �ی���ʂv
               WHEN 09
                  MOVE NC"��"        TO ���`�F�b�N�v
                  MOVE "�h�q"        TO �ی���ʂv
               WHEN 08
                  MOVE NC"��"        TO �ސE�`�F�b�N�v
                  MOVE "�ސE"        TO �ی���ʂv
               WHEN 05
                  MOVE NC"��"        TO ����`�F�b�N�v
                  MOVE "���"        TO �ی���ʂv
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
      */���t�����͑S�ā��t������/110408
               EVALUATE ���Z�|���t����
               WHEN 10
                   MOVE NC"��" TO �P�O���`�F�b�N�v
               WHEN 9
                   MOVE NC"��" TO �X���`�F�b�N�v
      */�_�ސ쌧�̏ꍇ�A�O������҂P���́A���t�������W���ɂ���B(�����P�����S���邽�߁A���҂P���A�ی��҂W���A���P���ƂȂ�)
                   IF (��|�ی����     = 01 AND ��|�ی��Ҕԍ�(1:2) = "14") OR
                      (��|�ی���� NOT = 01 AND ��|�ی��Ҕԍ�(3:2) = "14")
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
023620**
023630         MOVE ��|��ی��҃J�i       TO ��ی��҃J�i�v
023640         MOVE ��|��ی��Ҏ���       TO ��ی��Ҏ����v
023650         MOVE ��|�X�֔ԍ��P         TO �X�֔ԍ��P�v
023660         MOVE ��|�X�֔ԍ��Q         TO �X�֔ԍ��Q�v
023670         MOVE ��|�Z���P             TO ��ی��ҏZ���P�v
023680         MOVE ��|�Z���Q             TO ��ی��ҏZ���Q�v
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
023690         MOVE ��|���҃J�i           TO ���҃J�i�v
023700         MOVE ��|���Ҏ���           TO ���Ҏ����v
023710         MOVE ��|���җX�֔ԍ��P     TO ���җX�֔ԍ��P�v
023720         MOVE ��|���җX�֔ԍ��Q     TO ���җX�֔ԍ��Q�v
023730         MOVE ��|���ҏZ���P         TO ���ҏZ���P�v
023740         MOVE ��|���ҏZ���Q         TO ���ҏZ���Q�v
023750*
023760         EVALUATE ��|���Ґ���
023770         WHEN 1
023780             MOVE NC"�j"  TO ���ʂv
023790             MOVE NC"��"  TO �j�`�F�b�N�v
023800         WHEN 2
023810             MOVE NC"��"  TO ���ʂv
023820             MOVE NC"��"  TO ���`�F�b�N�v
023830         END-EVALUATE
023840*
023850         MOVE ��|���Ҙa��  TO ���Ҙa��v
023860         EVALUATE ��|���Ҙa��
023870         WHEN 1
023880             MOVE NC"����"  TO ���Ҙa��̂v
023890             MOVE NC"��"    TO �����`�F�b�N�v
023900         WHEN 2
023910             MOVE NC"�吳"  TO ���Ҙa��̂v
023920             MOVE NC"��"    TO �吳�`�F�b�N�v
023930         WHEN 3
023940             MOVE NC"���a"  TO ���Ҙa��̂v
023950             MOVE NC"��"    TO ���a�`�F�b�N�v
023960         WHEN 4
023970             MOVE NC"����"  TO ���Ҙa��̂v
023980             MOVE NC"��"    TO �����`�F�b�N�v
      */�����C��/20190426
023060         WHEN 5
                   MOVE "5��"   TO �ߘa�b�l�v
023070             MOVE NC"��"  TO �ߘa�`�F�b�N�v
023990         END-EVALUATE
024000*
      */�����C��/������20190426
029310         IF ��|���Ҙa�� > 4
037370             MOVE ��|���Ҙa��     TO ���|�����敪
037380             READ �����}�X�^
037390             NOT INVALID KEY
037400                 MOVE ���|�������� TO �����v
037410             END-READ
029330         END-IF
      */�����C��/������20190426
024010         MOVE ��|���ҔN  TO ���ҔN�v
024020         MOVE ��|���Ҍ�  TO ���Ҍ��v
024030         MOVE ��|���ғ�  TO ���ғ��v
024040*
      */���Z�܂Ƃ߂ɑΉ�/101108
030020         IF ��|������� NOT = ZERO
030030            PERFORM �������Z�܂Ƃߔ���
030040         ELSE
030050            MOVE SPACE TO �������Z�܂Ƃ߃t���O
030060         END-IF
029002*
024260     END-IF.
024270*
025540*================================================================*
025550 �����f�[�^�擾 SECTION.
025560*================================================================*
025570**************************************************
025580* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾 *
025590* �� ������...���ʁ{������ʂɂĉ��H���Ċi�[     *
025600* �� �����N.......�����N�v                       *
025610* �� ������.......�������v                       *
025620* �� ������.......�������v                       *
025630* �� �J�n�N.......�����N�v                       *
025640* �� �J�n��.......�������v                       *
025650* �� �J�n��.......�������v                       *
025660* �� �I���N.......�I���N�v                       *
025670* �� �I����.......�I�����v                       *
025680* �� �I����.......�I�����v                       *
025690* �� ������.......�������v                       *
025700* �� �]�A�敪 ....�敪�ɂ��`�F�b�N��"��"���i�[ *
025710* �� �������q ....�敪�ɂ��`�F�b�N��"��"���i�[ *
025720* �� �o�߃R�[�h...�o�߃}�X�^���擾             *
025730**************************************************
           IF ���|���R�[�h NOT = SPACE
025830         MOVE ���|���ʐ�                   TO ���ʐ��v
025840         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
025850                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
025860             MOVE ���|�������(���ʂb�m�s) TO ������ʂv(���ʂb�m�s)
025870             MOVE ���|����(���ʂb�m�s)     TO ���ʂv(���ʂb�m�s)
025880             MOVE ���|���E�敪(���ʂb�m�s) TO ���E�敪�v(���ʂb�m�s)
025890             MOVE ���|�����ʒu�ԍ�(���ʂb�m�s)
025900                                           TO �����ʒu�ԍ��v(���ʂb�m�s)
025910*********************************************
025920* ���j�S�_...������ʁ{���ʂɂĉ��H���Ċi�[ *
025930*********************************************
025940* �������
025950             MOVE SPACE                     TO �������̂v
025960             MOVE 03                        TO ���|�敪�R�[�h
025970             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
025980             READ ���̃}�X�^
025990             INVALID KEY
026000                 MOVE SPACE        TO �������̂v
026010             NOT INVALID KEY
026020                 MOVE ���|�������� TO �������̂v
026030             END-READ
026040* ����
020710             MOVE SPACE                    TO �������v(���ʂb�m�s)
032680*
032690             PERFORM ���ʖ��̖�������
026230*
026240             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
026250             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
026260             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
026270             MOVE ���|�J�n�N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
026280             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
026290             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
026300             IF ( ���|�]�A�敪(���ʂb�m�s) = 9 )
032900                 MOVE 9                    TO �I���a��v(���ʂb�m�s)
026310                 MOVE 99                   TO �I���N�v(���ʂb�m�s)
026320                 MOVE 99                   TO �I�����v(���ʂb�m�s)
026330                 MOVE 99                   TO �I�����v(���ʂb�m�s)
026340             ELSE
032940                 MOVE ���|�I���a��(���ʂb�m�s) TO �I���a��v(���ʂb�m�s)
026350                 MOVE ���|�I���N(���ʂb�m�s)   TO �I���N�v(���ʂb�m�s)
026360                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
026370                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
026380             END-IF
026390* �o�ߗ��̎擾
026400             MOVE 01                         TO �o�|�敪�R�[�h
026410             MOVE ���|�o�߃R�[�h(���ʂb�m�s) TO �o�|�o�߃R�[�h
026420             READ �o�߃}�X�^
026430             INVALID KEY
026440                 MOVE ZERO       TO ���ʂb�m�s�v(���ʂb�m�s)
026450                 MOVE SPACE      TO ���ʋ�؂v(���ʂb�m�s)
026460                 MOVE SPACE      TO �o�ߗ��̂v(���ʂb�m�s)
026470             NOT INVALID KEY
026480*
026490                 EVALUATE ���ʂb�m�s
026500                 WHEN 1
026510                     MOVE NC"�@" TO �o�ߕ��ʂv
026520                 WHEN 2
026530                     MOVE NC"�A" TO �o�ߕ��ʂv
026540                 WHEN 3
026550                     MOVE NC"�B" TO �o�ߕ��ʂv
026560                 WHEN 4
026570                     MOVE NC"�C" TO �o�ߕ��ʂv
026580                 WHEN 5
026590                     MOVE NC"�D" TO �o�ߕ��ʂv
026600                 END-EVALUATE
026610                 STRING  �o�ߕ��ʂv     DELIMITED BY SPACE
026620                         �o�|�o�ߗ���   DELIMITED BY SPACE
026630                        INTO ����o�ߗ��̂v(���ʂb�m�s)
026640                 END-STRING
026650*
026660             END-READ
026670*
026680             MOVE ���|�]�A�敪(���ʂb�m�s) TO �]�A�敪�v(���ʂb�m�s)
026690             EVALUATE ���|�]�A�敪(���ʂb�m�s)
026700             WHEN 1
026710             WHEN 2
026720                 MOVE NC"��"               TO �����`�F�b�N�v(���ʂb�m�s)
026730             WHEN 3
026740                 MOVE NC"��"               TO ���~�`�F�b�N�v(���ʂb�m�s)
026750             WHEN 4
026760                 MOVE NC"��"               TO �]��`�F�b�N�v(���ʂb�m�s)
026770             END-EVALUATE
026780*
026790         END-PERFORM
026800* �V�K/�p�� �`�F�b�N
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
026860*
026870* �}�Ԕ���p
026880         MOVE ���|�J�n�f�Ó��蓮�敪   TO �J�n�f�Ó��蓮�敪�v
026890* ������������敪
026900         MOVE ���|���Z������������敪 TO ���Z������������敪�v
027880         MOVE ���|���Z�������R����敪 TO ���Z�������R����敪�v
026910*
026920     END-IF.
026930*
026940*================================================================*
026950 ���ʖ��̖������� SECTION.
026960*
006490     STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
009980            �������̂v                    DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
006520       INTO �������v(���ʂb�m�s)
006570     END-STRING.
027130*
027560*================================================================*
027570 �������擾 SECTION.
027580*================================================================*
027590********************
027600* �����f�[�^�Z�b�g *
027610********************
027620*    ****************************************************************
027630*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
027640*    ****************************************************************
027650     MOVE ���Z�|������                 TO �������v�q.
027660     IF ( ���Z�|���ԊO = 1 )
027670         MOVE NC"��"                   TO ���ԊO�`�F�b�N�v
027680     END-IF.
027690     IF ( ���Z�|�x�� = 1 )
027700         MOVE NC"��"                   TO �x���`�F�b�N�v
027710     END-IF.
027720     IF ( ���Z�|�[�� = 1 )
027730         MOVE NC"��"                   TO �[��`�F�b�N�v
027740     END-IF.
           MOVE ���Z�|���������k��           TO ���k���v�q.
027750*
027760     MOVE ���Z�|�������Z��             TO  �������Z���v�q.
027770     MOVE ���Z�|�Č���                 TO  �Č����v�q.
027780     MOVE ���Z�|���Ë���               TO  ���Ë����v�q.
027790     MOVE ���Z�|���É�               TO  ���É񐔂v�q.
027800     MOVE ���Z�|���×�                 TO  ���×��v�q.
027810     MOVE ���Z�|���É��Z��             TO  ���É��Z���v�q.
027820*
027830     IF ( ���Z�|��� = 1 )
027840         MOVE NC"��"                   TO ��ԃ`�F�b�N�v
027850     END-IF.
027860     IF ( ���Z�|�\���J�� = 1 )
027870         MOVE NC"��"                   TO �\���J��`�F�b�N�v
027880     END-IF.
027890     IF ( ���Z�|��H = 1 )
027900        MOVE NC"��"                    TO ��H�`�F�b�N�v
027910     END-IF.
027920*
027930     MOVE ���Z�|�������q���Z��         TO  �������q���Z���v�q.
           MOVE ���Z�|�������q��            TO �����񐔂v.
           MOVE ���Z�|�^����É�            TO �^���񐔂v.
           MOVE ���Z�|�^����×�              TO �^�����v.
028040*
028050     MOVE ���Z�|�{�p���񋟗�         TO  �{�p���񋟗��v�q.
028060* ���v
022420     COMPUTE ���v�v = ���Z�|���v + ���Z�|�^����×�.
028080********************
028090* ���񏈒u���Z�b�g *
028100********************
028110     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
028120             UNTIL ( ���ʂb�m�s > ���ʐ��v )
028130         MOVE ���Z�|���񏈒u��(���ʂb�m�s) TO ���񏈒u���v�q(���ʂb�m�s)
028140         IF ( ���Z�|���񏈒u��(���ʂb�m�s) NOT = ZERO )
028150            EVALUATE ���|�������(���ʂb�m�s)
028160* �P���E�Ŗo�E����
028170            WHEN 1
028180            WHEN 2
028190            WHEN 3
028200                MOVE NC"��"            TO �{�×��`�F�b�N�v
028210* �E�P�E���܁E���܍S�k
028220            WHEN 4
028230            WHEN 5
028240            WHEN 7
028250                MOVE NC"��"            TO �������`�F�b�N�v
028260* �s�S���܁E�s�S���܍S�k
028270            WHEN 6
028280            WHEN 8
028290                MOVE NC"��"            TO �Œ藿�`�F�b�N�v
028300            END-EVALUATE
028310         END-IF
028320     END-PERFORM.
028330*
028340     MOVE ���Z�|���񏈒u�����v         TO ���񏈒u�����v�v.
028350********************
028360* �����������Z�b�g *
028370********************
028380*    **********
028390*    * �P���� *
028400*    **********
028410     MOVE ���Z�|��ÒP���P             TO ��ÒP���P�v�q.
028420     MOVE ���Z�|��É񐔂P             TO ��É񐔂P�v�q.
028430     MOVE ���Z�|��×��P               TO ��×��P�v�q.
028440     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
028450     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
028460     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
028470     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
028480     MOVE ���Z�|�d�É񐔂P             TO �d�É񐔂P�v�q.
028490     MOVE ���Z�|�d�×��P               TO �d�×��P�v�q.
028500     MOVE ���Z�|���v�P                 TO ���v�P�v�q.
           IF ���Z�|�����p��������P NOT = ZERO
023850         MOVE ���Z�|�����p��������P   TO �����������P�v�q
           ELSE
024000         MOVE ���Z�|�����������P       TO �����������P�v�q
           END-IF.
028520     MOVE ���Z�|���������v�P           TO ���������v�P�v�q.
028530*    **********
028540*    * �Q���� *
028550*    **********
028560     MOVE ���Z�|��ÒP���Q             TO ��ÒP���Q�v�q.
028570     MOVE ���Z�|��É񐔂Q             TO ��É񐔂Q�v�q.
028580     MOVE ���Z�|��×��Q               TO ��×��Q�v�q.
028590     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
028600     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
028610     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
028620     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
028630     MOVE ���Z�|�d�É񐔂Q             TO �d�É񐔂Q�v�q.
028640     MOVE ���Z�|�d�×��Q               TO �d�×��Q�v�q.
028650     MOVE ���Z�|���v�Q                 TO ���v�Q�v�q.
           IF ���Z�|�����p��������Q NOT = ZERO
023850         MOVE ���Z�|�����p��������Q   TO �����������Q�v�q
           ELSE
024000         MOVE ���Z�|�����������Q       TO �����������Q�v�q
           END-IF.
028670     MOVE ���Z�|���������v�Q           TO ���������v�Q�v�q.
028680*    ****************
028690*    * �R���ʁ^�W�� *
028700*    ****************
028710     MOVE ���Z�|��ÒP���R�W           TO ��ÒP���R�W�v�q.
028720     MOVE ���Z�|��É񐔂R�W           TO ��É񐔂R�W�v�q.
028730     MOVE ���Z�|��×��R�W             TO ��×��R�W�v�q.
028740     MOVE ���Z�|��㪖@�񐔂R�W         TO ��㪖@�񐔂R�W�v�q.
028750     MOVE ���Z�|��㪖@���R�W           TO ��㪖@���R�W�v�q.
028760     MOVE ���Z�|��㪖@�񐔂R�W         TO ��㪖@�񐔂R�W�v�q.
028770     MOVE ���Z�|��㪖@���R�W           TO ��㪖@���R�W�v�q.
028780     MOVE ���Z�|�d�É񐔂R�W           TO �d�É񐔂R�W�v�q.
028790     MOVE ���Z�|�d�×��R�W             TO �d�×��R�W�v�q.
028800     MOVE ���Z�|���v�R�W               TO ���v�R�W�v�q.
028810     MOVE ���Z�|�����ʍ����v�R�W       TO �����ʍ����v�R�W�v�q.
           IF ���Z�|�����p��������R�W NOT = ZERO
023850         MOVE ���Z�|�����p��������R�W   TO �����������R�W�v�q
           ELSE
024160         MOVE ���Z�|�����������R�W       TO �����������R�W�v�q
           END-IF.
028830     MOVE ���Z�|���������v�R�W         TO ���������v�R�W�v�q.
028840*    ****************
028850*    * �R���ʁ^10�� *
028860*    ****************
028870     MOVE ���Z�|�����J�n���R�O         TO �����J�n���R�O�v�q.
028880     MOVE ���Z�|�����J�n���R�O         TO �����J�n���R�O�v�q.
028890     MOVE ���Z�|��ÒP���R�O           TO ��ÒP���R�O�v�q.
028900     MOVE ���Z�|��É񐔂R�O           TO ��É񐔂R�O�v�q.
028910     MOVE ���Z�|��×��R�O             TO ��×��R�O�v�q.
028920     MOVE ���Z�|��㪖@�񐔂R�O         TO ��㪖@�񐔂R�O�v�q.
028930     MOVE ���Z�|��㪖@���R�O           TO ��㪖@���R�O�v�q.
028940     MOVE ���Z�|��㪖@�񐔂R�O         TO ��㪖@�񐔂R�O�v�q.
028950     MOVE ���Z�|��㪖@���R�O           TO ��㪖@���R�O�v�q.
028960     MOVE ���Z�|�d�É񐔂R�O           TO �d�É񐔂R�O�v�q.
028970     MOVE ���Z�|�d�×��R�O             TO �d�×��R�O�v�q.
028980     MOVE ���Z�|���v�R�O               TO ���v�R�O�v�q.
           IF ���Z�|�����p��������R�O NOT = ZERO
023850         MOVE ���Z�|�����p��������R�O   TO �����������R�O�v�q
           ELSE
024330         MOVE ���Z�|�����������R�O       TO �����������R�O�v�q
           END-IF.
029000     MOVE ���Z�|���������v�R�O         TO ���������v�R�O�v�q.
029010*    ****************
029020*    * �S���ʁ^�T�� *
029030*    ****************
029040     MOVE ���Z�|��ÒP���S�T           TO ��ÒP���S�T�v�q.
029050     MOVE ���Z�|��É񐔂S�T           TO ��É񐔂S�T�v�q.
029060     MOVE ���Z�|��×��S�T             TO ��×��S�T�v�q.
029070     MOVE ���Z�|��㪖@�񐔂S�T         TO ��㪖@�񐔂S�T�v�q.
029080     MOVE ���Z�|��㪖@���S�T           TO ��㪖@���S�T�v�q.
029090     MOVE ���Z�|��㪖@�񐔂S�T         TO ��㪖@�񐔂S�T�v�q.
029100     MOVE ���Z�|��㪖@���S�T           TO ��㪖@���S�T�v�q.
029110     MOVE ���Z�|�d�É񐔂S�T           TO �d�É񐔂S�T�v�q.
029120     MOVE ���Z�|�d�×��S�T             TO �d�×��S�T�v�q.
029130     MOVE ���Z�|���v�S�T               TO ���v�S�T�v�q.
029140     MOVE ���Z�|�����ʍ����v�S�T       TO �����ʍ����v�S�T�v�q.
029150     MOVE ���Z�|�����������S�T         TO �����������S�T�v�q.
029160     MOVE ���Z�|���������v�S�T         TO ���������v�S�T�v�q.
029170*    ****************
029180*    * �S���ʁ^�W�� *
029190*    ****************
029200     MOVE ���Z�|�����J�n���S�W         TO �����J�n���S�W�v�q.
029210     MOVE ���Z�|�����J�n���S�W         TO �����J�n���S�W�v�q.
029220     MOVE ���Z�|��ÒP���S�W           TO ��ÒP���S�W�v�q.
029230     MOVE ���Z�|��É񐔂S�W           TO ��É񐔂S�W�v�q.
029240     MOVE ���Z�|��×��S�W             TO ��×��S�W�v�q.
029250     MOVE ���Z�|��㪖@�񐔂S�W         TO ��㪖@�񐔂S�W�v�q.
029260     MOVE ���Z�|��㪖@���S�W           TO ��㪖@���S�W�v�q.
029270     MOVE ���Z�|��㪖@�񐔂S�W         TO ��㪖@�񐔂S�W�v�q.
029280     MOVE ���Z�|��㪖@���S�W           TO ��㪖@���S�W�v�q.
029290     MOVE ���Z�|�d�É񐔂S�W           TO �d�É񐔂S�W�v�q.
029300     MOVE ���Z�|�d�×��S�W             TO �d�×��S�W�v�q.
029310     MOVE ���Z�|���v�S�W               TO ���v�S�W�v�q.
029320     MOVE ���Z�|�����ʍ����v�S�W       TO �����ʍ����v�S�W�v�q.
           IF ���Z�|�����p��������S�W NOT = ZERO
023850         MOVE ���Z�|�����p��������S�W   TO �����������S�W�v�q
           ELSE
024670         MOVE ���Z�|�����������S�W       TO �����������S�W�v�q
           END-IF.
029340     MOVE ���Z�|���������v�S�W         TO ���������v�S�W�v�q.
029350*    ****************
029360*    * �S���ʁ^10�� *
029370*    ****************
029380     MOVE ���Z�|�����J�n���S�O         TO �����J�n���S�O�v�q.
029390     MOVE ���Z�|�����J�n���S�O         TO �����J�n���S�O�v�q.
029400     MOVE ���Z�|��ÒP���S�O           TO ��ÒP���S�O�v�q.
029410     MOVE ���Z�|��É񐔂S�O           TO ��É񐔂S�O�v�q.
029420     MOVE ���Z�|��×��S�O             TO ��×��S�O�v�q.
029430     MOVE ���Z�|��㪖@�񐔂S�O         TO ��㪖@�񐔂S�O�v�q.
029440     MOVE ���Z�|��㪖@���S�O           TO ��㪖@���S�O�v�q.
029450     MOVE ���Z�|��㪖@�񐔂S�O         TO ��㪖@�񐔂S�O�v�q.
029460     MOVE ���Z�|��㪖@���S�O           TO ��㪖@���S�O�v�q.
029470     MOVE ���Z�|�d�É񐔂S�O           TO �d�É񐔂S�O�v�q.
029480     MOVE ���Z�|�d�×��S�O             TO �d�×��S�O�v�q.
029490     MOVE ���Z�|���v�S�O               TO ���v�S�O�v�q.
           IF ���Z�|�����p��������S�O NOT = ZERO
023850         MOVE ���Z�|�����p��������S�O   TO �����������S�O�v�q
           ELSE
024840         MOVE ���Z�|�����������S�O       TO �����������S�O�v�q
           END-IF.
029510     MOVE ���Z�|���������v�S�O         TO ���������v�S�O�v�q.
029520*    *****************
029530*    * �T���ʁ^2.5�� *
029540*    *****************
029550*     MOVE ���Z�|��ÒP���T�Q           TO ��ÒP���T�Q�v�q.
029560*     MOVE ���Z�|��É񐔂T�Q           TO ��É񐔂T�Q�v�q.
029570*     MOVE ���Z�|��×��T�Q             TO ��×��T�Q�v�q.
029580*     MOVE ���Z�|��㪖@�񐔂T�Q         TO ��㪖@�񐔂T�Q�v�q.
029590*     MOVE ���Z�|��㪖@���T�Q           TO ��㪖@���T�Q�v�q.
029600*     MOVE ���Z�|��㪖@�񐔂T�Q         TO ��㪖@�񐔂T�Q�v�q.
029610*     MOVE ���Z�|��㪖@���T�Q           TO ��㪖@���T�Q�v�q.
029620*     MOVE ���Z�|�d�É񐔂T�Q           TO �d�É񐔂T�Q�v�q.
029630*     MOVE ���Z�|�d�×��T�Q             TO �d�×��T�Q�v�q.
029640*     MOVE ���Z�|���v�T�Q               TO ���v�T�Q�v�q.
029650*     MOVE ���Z�|�����ʍ����v�T�Q       TO �����ʍ����v�T�Q�v�q.
029660*     MOVE ���Z�|�����������T�Q         TO �����������T�Q�v�q.
029670*     MOVE ���Z�|���������v�T�Q         TO ���������v�T�Q�v�q.
029680*    ****************
029690*    * �T���ʁ^�T�� *
029700*    ****************
029710     MOVE ���Z�|�����J�n���T�T         TO �����J�n���T�T�v�q.
029720     MOVE ���Z�|�����J�n���T�T         TO �����J�n���T�T�v�q.
029730     MOVE ���Z�|��ÒP���T�T           TO ��ÒP���T�T�v�q.
029740     MOVE ���Z�|��É񐔂T�T           TO ��É񐔂T�T�v�q.
029750     MOVE ���Z�|��×��T�T             TO ��×��T�T�v�q.
029760     MOVE ���Z�|��㪖@�񐔂T�T         TO ��㪖@�񐔂T�T�v�q.
029770     MOVE ���Z�|��㪖@���T�T           TO ��㪖@���T�T�v�q.
029780     MOVE ���Z�|��㪖@�񐔂T�T         TO ��㪖@�񐔂T�T�v�q.
029790     MOVE ���Z�|��㪖@���T�T           TO ��㪖@���T�T�v�q.
029800     MOVE ���Z�|�d�É񐔂T�T           TO �d�É񐔂T�T�v�q.
029810     MOVE ���Z�|�d�×��T�T             TO �d�×��T�T�v�q.
029820     MOVE ���Z�|���v�T�T               TO ���v�T�T�v�q.
029830     MOVE ���Z�|�����ʍ����v�T�T       TO �����ʍ����v�T�T�v�q.
029840     MOVE ���Z�|�����������T�T         TO �����������T�T�v�q.
029850     MOVE ���Z�|���������v�T�T         TO ���������v�T�T�v�q.
029860*    ****************
029870*    * �T���ʁ^�W�� *
029880*    ****************
029890     MOVE ���Z�|�����J�n���T�W         TO �����J�n���T�W�v�q.
029900     MOVE ���Z�|�����J�n���T�W         TO �����J�n���T�W�v�q.
029910     MOVE ���Z�|��ÒP���T�W           TO ��ÒP���T�W�v�q.
029920     MOVE ���Z�|��É񐔂T�W           TO ��É񐔂T�W�v�q.
029930     MOVE ���Z�|��×��T�W             TO ��×��T�W�v�q.
029940     MOVE ���Z�|��㪖@�񐔂T�W         TO ��㪖@�񐔂T�W�v�q.
029950     MOVE ���Z�|��㪖@���T�W           TO ��㪖@���T�W�v�q.
029960     MOVE ���Z�|��㪖@�񐔂T�W         TO ��㪖@�񐔂T�W�v�q.
029970     MOVE ���Z�|��㪖@���T�W           TO ��㪖@���T�W�v�q.
029980     MOVE ���Z�|�d�É񐔂T�W           TO �d�É񐔂T�W�v�q.
029990     MOVE ���Z�|�d�×��T�W             TO �d�×��T�W�v�q.
030000     MOVE ���Z�|���v�T�W               TO ���v�T�W�v�q.
030010     MOVE ���Z�|�����ʍ����v�T�W       TO �����ʍ����v�T�W�v�q.
           IF ���Z�|�����p��������T�W NOT = ZERO
023850         MOVE ���Z�|�����p��������T�W   TO �����������T�W�v�q
           ELSE
025360         MOVE ���Z�|�����������T�W       TO �����������T�W�v�q
           END-IF.
030030     MOVE ���Z�|���������v�T�W         TO ���������v�T�W�v�q.
030040*    ****************
030050*    * �T���ʁ^10�� *
030060*    ****************
030070     MOVE ���Z�|�����J�n���T�O         TO �����J�n���T�O�v�q.
030080     MOVE ���Z�|�����J�n���T�O         TO �����J�n���T�O�v�q.
030090     MOVE ���Z�|��ÒP���T�O           TO ��ÒP���T�O�v�q.
030100     MOVE ���Z�|��É񐔂T�O           TO ��É񐔂T�O�v�q.
030110     MOVE ���Z�|��×��T�O             TO ��×��T�O�v�q.
030120     MOVE ���Z�|��㪖@�񐔂T�O         TO ��㪖@�񐔂T�O�v�q.
030130     MOVE ���Z�|��㪖@���T�O           TO ��㪖@���T�O�v�q.
030140     MOVE ���Z�|��㪖@�񐔂T�O         TO ��㪖@�񐔂T�O�v�q.
030150     MOVE ���Z�|��㪖@���T�O           TO ��㪖@���T�O�v�q.
030160     MOVE ���Z�|�d�É񐔂T�O           TO �d�É񐔂T�O�v�q.
030170     MOVE ���Z�|�d�×��T�O             TO �d�×��T�O�v�q.
030180     MOVE ���Z�|���v�T�O               TO ���v�T�O�v�q.
           IF ���Z�|�����p��������T�O NOT = ZERO
023850         MOVE ���Z�|�����p��������T�O   TO �����������T�O�v�q
           ELSE
025530         MOVE ���Z�|�����������T�O       TO �����������T�O�v�q
           END-IF.
030200     MOVE ���Z�|���������v�T�O         TO ���������v�T�O�v�q.
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
030210*
030220*================================================================*
030230 �{�p�L�^�擾 SECTION.
030240*================================================================*
030250************************************************************
030260* ��P�f�[�^���畉���f�[�^�e���ȉ��̏����擾           *
030270* �� �������Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
030280* �� ���É��Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
030290************************************************************
030300     MOVE  SPACE  TO  �����Č��t���O.
030310     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ( ���ʂb�m�s > ���ʐ��v )
030320         IF ( �{�p�N�v = �����N�v(���ʂb�m�s) ) AND
030330            ( �{�p���v = �������v(���ʂb�m�s) )
030340             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
030350             MOVE �}�Ԃv�q              TO �{�L�|�}��
030360             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
030370             MOVE �����N�v(���ʂb�m�s)  TO �J�n�N�v(���ʂb�m�s) �{�L�|�{�p�N
030380             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
030390             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
030400         ELSE
030410             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
030420             MOVE �}�Ԃv�q              TO �{�L�|�}��
030430             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
030440             MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
030450             MOVE �{�p���v�q            TO �{�L�|�{�p��
030460             MOVE ZERO                  TO �{�L�|�{�p��
030470         END-IF
030480         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
030490                                      �{�L�|�{�p�a��N����
030500         END-START
030510         IF ( ��ԃL�[ = "00" )
030520             MOVE ZERO  TO �������v(���ʂb�m�s)
030830             MOVE ZERO  TO �I���a��v�s
030530             MOVE ZERO  TO �I���N�v�s
030540             MOVE ZERO  TO �I�����v�s
030550             MOVE ZERO  TO �I�����v�s
030560             MOVE SPACE TO �I���t���O�Q
030570             PERFORM �{�p�L�^�e�Ǎ�
030580             IF ( �I���t���O�Q      = SPACE   ) AND
030590                ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
030600                ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
030610                ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
030620                ( �{�L�|�{�p��      = �{�p���v�q     ) 
030630*
030640*        *****************************************************************
030650*        * �J�n�N���� ( ���̕��ʂ����������łȂ����A
030660*                       ���������ł��}�Ԃ����鎞�́A�ŏ��̎{�p�����J�n��)*
030670*        *****************************************************************
030680                 IF ( �{�p�N�v NOT = �����N�v(���ʂb�m�s) ) OR
030690                    ( �{�p���v NOT = �������v(���ʂb�m�s) ) OR
030700                    ( �J�n�f�Ó��蓮�敪�v = 1 )
030710                     MOVE �{�L�|�{�p�N   TO �J�n�N�v(���ʂb�m�s)
030720                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
030730                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
030740                 END-IF
030750             END-IF
030760             PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
030770                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
030780                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
030790                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
030800                           ( �{�L�|�{�p��     NOT = �{�p���v�q       ) OR
030810                           ( �{�L�|�{�p��         > �I�����v(���ʂb�m�s))
030820*               **********
030830*               * ������ *
030840*               **********
030850                COMPUTE �������v(���ʂb�m�s) = �������v(���ʂb�m�s) + 1
031240                MOVE �{�L�|�{�p�a��             TO �I���a��v�s
030860                MOVE �{�L�|�{�p�N               TO �I���N�v�s
030870                MOVE �{�L�|�{�p��               TO �I�����v�s
030880                MOVE �{�L�|�{�p��               TO �I�����v�s
030890*
030900                PERFORM �{�p�L�^�e�Ǎ�
030910            END-PERFORM
030920        END-IF
030930*       **************************
030940*       * �p���F�I���N�����Z�b�g *
030950*       **************************
030960        IF ( �]�A�敪�v(���ʂb�m�s) = 9 )
032090            MOVE �I���a��v�s  TO �I���a��v(���ʂb�m�s)
030970            MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
030980            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
030990            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
031000        END-IF
031010        IF ( �I���N�����v(���ʂb�m�s) > �󗝔N�����v )
032140            MOVE �I���a��v(���ʂb�m�s) TO �󗝘a��v
031020            MOVE �I���N�v(���ʂb�m�s) TO �󗝔N�v
031030            MOVE �I�����v(���ʂb�m�s) TO �󗝌��v
031040            MOVE �I�����v(���ʂb�m�s) TO �󗝓��v
031050        END-IF
031060     END-PERFORM.
031070*
031080** ----- �O�������݂̂��𔻒� -----------*
031090*
031100*     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
031110*     MOVE �}�Ԃv�q              TO �{�L�|�}��.
031120*     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
031130*     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
031140*     MOVE �{�p���v�q            TO �{�L�|�{�p��.
031150*     MOVE ZERO                  TO �{�L�|�{�p��.
031160*     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
031170*                                  �{�L�|�{�p�a��N����
031180*     END-START.
031190*     IF ( ��ԃL�[ = "00" )
031200*             MOVE SPACE TO �I���t���O�Q
031210*             PERFORM �{�p�L�^�e�Ǎ�
031220*             IF ( �I���t���O�Q      = SPACE   ) AND
031230*                ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
031240*                ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
031250*                ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
031260*                ( �{�L�|�{�p��      = �{�p���v�q     ) 
031270** �����{�p�J�n�����Č����ǂ�������
031280*                 IF ( �{�L�|�Č������� = 1 )
031290*                      MOVE "YES"  TO  �����Č��t���O
031300*                 END-IF
031310**
031320*             END-IF
031330*     END-IF.
031340*     IF ( �����Č��t���O = "YES" )
031350*        PERFORM �O�������̂ݔ���
031360*     END-IF.
031370*
031380*================================================================*
031390 �O�������̂ݔ��� SECTION.
031400*
031410*** �O���̒ʉ@�������������� 
031420     MOVE  SPACE            TO �O���t���O.
031430     MOVE ��|���҃R�[�h    TO �{�L�|���҃R�[�h.
031440     MOVE ��|�{�p�a��      TO �{�L�|�{�p�a��.
031450     MOVE ��|�{�p�N        TO �{�L�|�{�p�N.
031460     MOVE ��|�{�p��        TO �{�L�|�{�p��.
031470     MOVE 1                 TO �{�L�|�{�p��.
031480     START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
031490                                  �{�L�|�{�p�a��N����
031500                                  REVERSED
031510     END-START.
031520     IF ( ��ԃL�[ = "00" )
031530         MOVE SPACE  TO �I���t���O�Q
031540         PERFORM �{�p�L�^�e�Ǎ�
031550         IF ( �I���t���O�Q      = SPACE  ) AND
031560            ( �{�L�|���҃R�[�h  = ��|���҃R�[�h ) AND
031570            ( �{�L�|�f�Ë敪    = 2 ) 
031580*
031590            PERFORM �O������
031600**** �K�p�P���g�p
031610            IF ( �O���t���O = "YES" )
031620               MOVE NC"���O�������̂�"    TO  �K�p�P�v
031630            END-IF
031640**
031650         END-IF
031660     END-IF.
031670*
031680*================================================================*
031690 �O������  SECTION.
031700* 
031710*** �ǂݍ��񂾎{�p�L�^�̔N�����A�O�����ǂ������� (�N���̍��� 1 ��?)
031720      MOVE  SPACE  TO  �O���t���O.
031730      INITIALIZE  �v�Z�N�����v �J�n�N�����Q�v �I���N�����Q�v.
031740**
031750      MOVE ��|�{�p�a��    TO �I���a��Q�v.
031760      MOVE ��|�{�p�N      TO �I���N�Q�v.
031770      MOVE ��|�{�p��      TO �I�����Q�v.
031780      MOVE �{�L�|�{�p�a��  TO �J�n�a��Q�v.
031790      MOVE �{�L�|�{�p�N    TO �J�n�N�Q�v.
031800      MOVE �{�L�|�{�p��    TO �J�n���Q�v.
031810*
031820      EVALUATE TRUE
031830       WHEN ( �J�n�a��Q�v = �I���a��Q�v ) AND ( �J�n�N�Q�v = �I���N�Q�v )
031840            PERFORM  �O����r��
031850       WHEN ( �J�n�a��Q�v = �I���a��Q�v ) AND ( �J�n�N�Q�v NOT = �I���N�Q�v )
031860            PERFORM  �O����r�N
031870       WHEN ( �J�n�a��Q�v NOT = �I���a��Q�v )
031880            PERFORM  �O����r����
031890      END-EVALUATE.
031900*
031910      IF ( �v�Z���v = 1 )
031920         MOVE  "YES"  TO  �O���t���O
031930      END-IF.
031940*
031950*================================================================*
031960 �O����r����  SECTION.
031970*
031980     MOVE �J�n�a��Q�v TO ���|�����敪.
031990     READ �����}�X�^
032000     NOT INVALID KEY
032010         MOVE ���|�J�n����N TO �J�n����N�v
032020     END-READ.
032030     MOVE �I���a��Q�v TO ���|�����敪.
032040     READ �����}�X�^
032050     NOT INVALID KEY
032060         MOVE ���|�J�n����N TO �I������N�v
032070     END-READ.
032080**
032090     IF ( �J�n����N�v NOT = ZERO ) AND ( �I������N�v NOT = ZERO )
032100        COMPUTE �J�n����N�v = �J�n����N�v + �J�n�N�Q�v - 1
032110        COMPUTE �I������N�v = �I������N�v + �I���N�Q�v - 1
032120*
032130        IF ( �I������N�v =  �J�n����N�v )
032140           PERFORM  �O����r��
032150        ELSE
032160           IF ( �I������N�v >  �J�n����N�v )
032170               COMPUTE �v�Z�N�v = �I������N�v - �J�n����N�v
032180               COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
032190           ELSE
032200               MOVE ZERO TO �v�Z���v
032210           END-IF
032220        END-IF
032230     ELSE
032240        MOVE ZERO TO �v�Z���v
032250     END-IF.
032260*
032270*================================================================*
032280 �O����r�N  SECTION.
032290*
032300     IF ( �I���N�Q�v >  �J�n�N�Q�v )
032310         COMPUTE �v�Z�N�v = �I���N�Q�v - �J�n�N�Q�v
032320         COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
032330     ELSE
032340        MOVE ZERO TO �v�Z���v
032350     END-IF.
032360*
032370*================================================================*
032380 �O����r��  SECTION.
032390*
032400     IF ( �I�����Q�v >  �J�n���Q�v )
032410         COMPUTE �v�Z���v = �I�����Q�v - �J�n���Q�v
032420     ELSE
032430        MOVE ZERO TO �v�Z���v
032440     END-IF.
032450*
042180*================================================================*
042190 �������Z�܂Ƃߔ��� SECTION.
042200*---------------------------------------------------------------------------*
042210* �{�̂܂Ƃߋ敪���P
042220* �̎��́A�t���OYES (���z���������݂ň󎚁j
042230*�i��F���l�s�̏�Q�́A�{�̕ی��i���یn�j�̃��Z�v�g�P���Ő����A�������Z�͂Ȃ��j
042240*---------------------------------------------------------------------------*
042250*
042260     MOVE SPACE TO �������Z�܂Ƃ߃t���O.
009201     IF ���Z�|�{�̂܂Ƃߋ敪 = 1 
009202        MOVE "YES" TO �������Z�܂Ƃ߃t���O
009203     END-IF.
042650*
042660*----------------------------------------------------------------------*
042670** / �_�ސ쌧�ŗL�F�E�v�ɕ��S�Ҕԍ��Ǝ󋋎Ҕԍ� /
042680     IF ( �������Z�܂Ƃ߃t���O = "YES" ) AND
042690        ( ��|��p���S�Ҕԍ�����(3:2) = "14" )
042700        IF ��|��p���S�Ҕԍ�����(1:2) NOT = "99"
                  MOVE ����S�Ҕԍ��v     TO ����S�Ҕԍ�
      */�󋋎Ҕԍ����W�����ȏ�̏ꍇ�g�𖳎����Ĉ������/110425
                  MOVE ��|��v�Ҕԍ�����   TO �󋋎Ҕԍ��v
                  IF ����󋋎Ҕԍ��Q�v = SPACE
016830                MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
                  ELSE
                      MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
                  END-IF
042790        END-IF
042800     END-IF.
042810**/�a�̎R����Q���c���ЂƂ�e/100518
042820     IF ( �������Z�܂Ƃ߃t���O = "YES" ) AND
042830        ( ��|��p���S�Ҕԍ�����(3:2) = "30" )
042840        IF ��|��p���S�Ҕԍ�����(1:2) NOT = "99"
                  MOVE ����S�Ҕԍ��v     TO ����S�Ҕԍ�
      */�󋋎Ҕԍ����W�����ȏ�̏ꍇ�g�𖳎����Ĉ������/110425
                  MOVE ��|��v�Ҕԍ�����   TO �󋋎Ҕԍ��v
                  IF ����󋋎Ҕԍ��Q�v = SPACE
016830                MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
                  ELSE
                      MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
                  END-IF
042790        END-IF
042930     END-IF.
042940*
032460*================================================================*
032470 ���Z�v�g���я��擾 SECTION.
032480*================================================================*
032490     MOVE �{�p�a��v�q       TO ��R�|�{�p�a��.
032500     MOVE �{�p�N�v�q         TO ��R�|�{�p�N.
032510     MOVE �{�p���v�q         TO ��R�|�{�p��.
032520     MOVE ���҃R�[�h�v�q     TO ��R�|���҃R�[�h.
032530     MOVE �A���|�ی����     TO ��R�|�ی����.
032540     READ ��ƃt�@�C���R
032550     NOT INVALID KEY
032560          MOVE ��R�|����    TO ���Ԃv
032570     END-READ.
032580*
032590*================================================================*
032600 ��������擾 SECTION.
032610*================================================================*
032620* �R�J���ȏ�̒�������� "CHOUKI" ���Ă�. 
032630     MOVE  SPACE TO  �A���ԁ|�L�[.
032640     INITIALIZE      �A���ԁ|�L�[.
032650     MOVE �{�p�a��v�q  TO  �A���ԁ|�{�p�a��.
032660     MOVE �{�p�N�v�q    TO  �A���ԁ|�{�p�N.
032670     MOVE �{�p���v�q    TO  �A���ԁ|�{�p��.
032680     MOVE ���Ҕԍ��v�q  TO  �A���ԁ|���Ҕԍ�.
032690     MOVE �}�Ԃv�q      TO  �A���ԁ|�}��.
032700*
032710     CALL   "CHOUKI".
032720     CANCEL "CHOUKI".
032730*
032740**** �K�p�P���g�p (�u�O�������̂݁v�����鎞�́A��������)
032750*****     IF ( �A���ԁ|�Ώۃt���O  = "YES" )
032760*****        IF ( �K�p�P�v  = SPACE )
032770*****           MOVE NC"�������{�p�p�����R���ʂɋL��"  TO �K�p�P�v
032780*****        ELSE
032790*****           STRING �K�p�P�v           DELIMITED BY SPACE
032800*****                  NC"�C"             DELIMITED BY SIZE
032810*****                  NC"�������{�p�p�����R���ʂɋL��"   DELIMITED BY SIZE
032820*****                  INTO �K�p�P�v
032830*****           END-STRING
032840*****        END-IF
032850*****     END-IF.
032860*
033580*================================================================*
033590 �������Z�����擾 SECTION.
033600*================================================================*
033610*****************************************************************
033620** �������Z�����ԊO�Ɛ[��̎��A�K�p�Ɂu��t���ԁv���󎚂���B
033630**   �����̈󎚂͌�3��܂ŉ\
033640*****************************************************************
033650     INITIALIZE �������Z�v�s.
033660*
033670     IF ( ���Z�|���ԊO = 1 ) OR ( ���Z�|�[�� = 1 ) OR ( ���Z�|�x�� = 1 )
033680         MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
033690         MOVE �}�Ԃv�q              TO �{�L�|�}��
033700         MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
033710         MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
033720         MOVE �{�p���v�q            TO �{�L�|�{�p��
033730         MOVE ZERO                  TO �{�L�|�{�p��
033740         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
033750                                      �{�L�|�{�p�a��N����
033760         END-START
033770         IF ( ��ԃL�[ = "00" )
033780             MOVE ZERO  TO �������Z�J�E���g
033790             MOVE SPACE TO �I���t���O�Q
033800             PERFORM �{�p�L�^�e�Ǎ�
033810             PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
033820                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
033830                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
033840                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
033850                           ( �{�L�|�{�p��     NOT = �{�p���v�q      ) 
033860               IF ( �{�L�|�������Z = 1 OR 2 OR 3 ) AND
033870                  ( �{�L�|�f�Ë敪 = 2 )
033880                  COMPUTE �������Z�J�E���g = �������Z�J�E���g  + 1
033890                  IF ( �������Z�J�E���g <= 3 )
033900                     MOVE �{�L�|�������Z TO �������Z�敪�v�s(�������Z�J�E���g)
033910                     MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
033920                     MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
033930                  END-IF
033940               END-IF
033950               PERFORM �{�p�L�^�e�Ǎ�
033960            END-PERFORM
033970** �������Z�̎�����K�p�ɃZ�b�g
033380            IF ( �������Z���v�s(1) NOT = ZERO ) OR ( �������Z���v�s(1) NOT = ZERO ) 
                      MOVE �������Z���v�s(1) TO �������Z���v
                      MOVE ":"               TO �������Z��؂v
                      MOVE �������Z���v�s(1) TO �������Z���v
                  END-IF
033380            IF ( �������Z���v�s(2) NOT = ZERO ) OR ( �������Z���v�s(2) NOT = ZERO ) 
031910                PERFORM �������Z�K�p�Z�b�g
                  END-IF
033990         END-IF
034000*
034010     END-IF.
034020*
034030*================================================================*
034040 �������Z�K�p�Z�b�g SECTION.
034050*
034060     PERFORM VARYING �ԍ��J�E���^ FROM 1 BY 1
034070             UNTIL ( �ԍ��J�E���^ > 3 )
034080         IF ( �������Z���v�s(�ԍ��J�E���^)  = ZERO )  AND 
034090            ( �������Z���v�s(�ԍ��J�E���^)  = ZERO ) 
034100             CONTINUE
034110         ELSE
034120* �Œ荀��
034130             EVALUATE �������Z�敪�v�s(�ԍ��J�E���^) 
034140             WHEN 1
034150                MOVE NC"���ԊO"   TO ���Z���e�v(�ԍ��J�E���^)
034430             WHEN 2
034440                MOVE NC"�x�@��"   TO ���Z���e�v(�ԍ��J�E���^)
034160             WHEN 3
034170                MOVE NC"�[�@��"   TO ���Z���e�v(�ԍ��J�E���^)
034180             END-EVALUATE
034190*
034200             MOVE NC"�F"          TO ���Z��؂v(�ԍ��J�E���^)
034210             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
034220             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
034230*
034240**** ���������{��ϊ�
034250* ����
034260             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
034270             IF ( �����v >= 10 )
034280                 MOVE �����v�P    TO �����ԍ��v�P
034290                 PERFORM ���{��ϊ�
034300                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
034310                 MOVE �����v�Q    TO �����ԍ��v�P
034320                 PERFORM ���{��ϊ�
034330                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
034340             ELSE
034350                 MOVE �����v�Q    TO �����ԍ��v�P
034360                 PERFORM ���{��ϊ�
034370                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
034380             END-IF
034390* ��
034400             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
034410             MOVE �����v�P    TO �����ԍ��v�P
034420             PERFORM ���{��ϊ�
034430             MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
034440             MOVE �����v�Q    TO �����ԍ��v�P
034450             PERFORM ���{��ϊ�
034460             MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
034470** 
034480        END-IF
034490     END-PERFORM.
034500*
034510     MOVE  �������Z�W�c�m�v(1)   TO �������Z�����P�v. 
034520     MOVE  �������Z�W�c�m�v(2)   TO �������Z�����Q�v. 
034530     MOVE  �������Z�W�c�m�v(3)   TO �������Z�����R�v. 
034540*
034550**** �K�p�P���Q���g�p�i�������R�L�ڂœK�p�P���g���Ă��鎞�́A�K�p�Q�j
034560     IF ( �������Z���v�s(1)  = ZERO ) AND ( �������Z���v�s(1)  = ZERO ) 
034570         CONTINUE
034580     ELSE
034590         IF ( �K�p�P�v  = SPACE )
034600               STRING NC"�������Z"       DELIMITED BY SIZE
034610                      �������Z�����P�v   DELIMITED BY SIZE
034620                      �������Z�����Q�v   DELIMITED BY SIZE
034630                      �������Z�����R�v   DELIMITED BY SIZE
034640                      INTO �K�p�P�v
034650               END-STRING
034660         ELSE
033830               STRING �K�p�P�v           DELIMITED BY SPACE
036850                      NC"�C"             DELIMITED BY SIZE
036860                      NC"�������Z"       DELIMITED BY SIZE
033840                      �������Z�����P�v   DELIMITED BY SIZE
033850                      �������Z�����Q�v   DELIMITED BY SIZE
033860                      �������Z�����R�v   DELIMITED BY SIZE
033870                      INTO �K�p�P�v
034720               END-STRING
034730         END-IF
034740     END-IF.
034750*
034760*================================================================*
034770 ���{��ϊ� SECTION.
034780*
034790     MOVE NC"�O"     TO �S�p�����ԍ��v.
034800     CALL "htoz" WITH C LINKAGE
034810                        USING �����ԍ��v�P �S�p�����ԍ��v�P.
034820*
034830*================================================================*
034840 ���������擾 SECTION.
034850*================================================================*
034860********************************************************************
034870*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
034880*  ��: �@�A �Ƃœ]��.
034890*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
034900*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
034910********************************************************************
034920     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
034930     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
034940             UNTIL ( ���ʂb�m�s > ���ʐ��v )
034950*
034960***        IF ( ���|�������Ҕԍ�(���ʂb�m�s)  NOT = ZERO )  AND
034970        IF ( ���|�����A��(���ʂb�m�s)      NOT = ZERO )
034980*
034990           IF ( �J�E���^ = ZERO )
035000              MOVE 1   TO  �J�E���^ �J�E���^�Q
035010              MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
035020              MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
035030              MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
035040           ELSE
035050              IF ( ���|�������Ҕԍ�(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
035060                 ( ���|�����A��(���ʂb�m�s)      = �����A�Ԃb�v     )
035070                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
035080                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
035090              ELSE
035100                 COMPUTE �J�E���^ = �J�E���^  +  1
035110                 MOVE 1   TO  �J�E���^�Q
035120                 MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)
035130                                                      �������Ҕԍ��b�v
035140                 MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
035150                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
035160              END-IF
035170           END-IF
035180        END-IF
035190     END-PERFORM.
035200**************************************************************************
035210*  ���������}�X�^��蕶�͎擾
035220**************************************************************************
035230     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
035240     PERFORM VARYING �J�E���^ FROM 1 BY 1
035250             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
035260** ���ۂ� �敪 01
035270         MOVE 01                        TO �����|�敪�R�[�h
035280         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
035290         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
035300         READ ���������e
035310         NOT INVALID KEY
035320             INITIALIZE ���������v�s
035330             MOVE �����|���������b�l(1) TO  ���������P�v�s
035340             MOVE �����|���������b�l(2) TO  ���������Q�v�s
035350             MOVE �����|���������b�l(3) TO  ���������R�v�s
035360             MOVE �����|���������b�l(4) TO  ���������S�v�s
035370             MOVE �����|���������b�l(5) TO  ���������T�v�s
035380             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
035390                     UNTIL ( �J�E���^�Q > 9 )  OR 
035400                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
035410                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
035420                WHEN 1
035430                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035440                WHEN 2
035450                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035460                WHEN 3
035470                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035480                WHEN 4
035490                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035500                WHEN 5
035510                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035480                WHEN 6
035490                   MOVE "�E"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035500                WHEN 7
035510                   MOVE "�F"  TO  ���������i���o�[�v�P(�J�E���^�Q)
035520                WHEN OTHER
035530                   CONTINUE
035540                END-EVALUATE
035550             END-PERFORM
035560*
035642             IF �����|�����������͋敪 = 1
035643                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
035644                        ���������P�v�s  DELIMITED BY SIZE
035645                        ���������Q�v�s  DELIMITED BY SIZE
035646                        ���������R�v�s  DELIMITED BY SIZE
035647                        ���������S�v�s  DELIMITED BY SIZE
035648                        ���������T�v�s  DELIMITED BY SIZE
035649                        INTO �����������e�����v(�J�E���^)
035650                 END-STRING
035651             ELSE
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
035660             END-IF
035661*
035662         END-READ
035663     END-PERFORM.
035670*
035680*     PERFORM ���������Z�b�g.
035680     PERFORM �S�����������̃Z�b�g.
035690*
035700*================================================================*
035710 ���������Z�b�g SECTION.
035720*
035730**************************************************************************
035740*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
035750**************************************************************************
035760     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
035770     PERFORM VARYING �J�E���^ FROM 1 BY 1
035780             UNTIL ( �J�E���^ > 9 )  OR ( �����������e�����v(�J�E���^) = SPACE )
035790*
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
035980*
035990     END-PERFORM.
036000*
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
036010*================================================================*
036020 �������R���擾 SECTION.
036030*================================================================*
036040* �������R���擾�� "CHOUBUN" ���Ă�. 
036050     MOVE  SPACE TO  �A�����|�L�[.
036060     INITIALIZE      �A�����|�L�[.
036070     MOVE �{�p�a��v�q  TO  �A�����|�{�p�a��.
036080     MOVE �{�p�N�v�q    TO  �A�����|�{�p�N.
036090     MOVE �{�p���v�q    TO  �A�����|�{�p��.
036100     MOVE ���Ҕԍ��v�q  TO  �A�����|���Ҕԍ�.
036110     MOVE �}�Ԃv�q      TO  �A�����|�}��.
036130     MOVE 56            TO  �A�����|������.
036140*
036150     CALL   "CHOUBUN".
036160     CANCEL "CHOUBUN".
036170*
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
036180*================================================================*
036190 �ϔC�N�����擾 SECTION.
036200*================================================================*
036210** ---// �����̎󗝔N�ɂ́A�ŏI�ʉ@���������Ă���ׁA�ޔ����� //----
036770     MOVE �󗝘a��v TO �ŏI�ʉ@�a��v.
036220     MOVE �󗝔N�v   TO �ŏI�ʉ@�N�v.
036230     MOVE �󗝌��v   TO �ŏI�ʉ@���v.
036240     MOVE �󗝓��v   TO �ŏI�ʉ@���v.
036250***
036260* (�_���t��)
036270     EVALUATE ���Z�v�g���t�敪�v 
036280*    /  �ŏI�ʉ@�� /
036290     WHEN ZERO
036850         MOVE �ŏI�ʉ@�a��v TO �_���t�a��v
036300         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
036310         MOVE �ŏI�ʉ@���v TO �_���t���v
036320         MOVE �ŏI�ʉ@���v TO �_���t���v
036330*    /  ������ /
036340     WHEN 1 
036350         PERFORM �������擾
036910         MOVE �󗝘a��v   TO �_���t�a��v
036360         MOVE �󗝔N�v     TO �_���t�N�v
036370         MOVE �󗝌��v     TO �_���t���v
036380         MOVE �󗝓��v     TO �_���t���v
036390*    /  �󎚂Ȃ� /
036400     WHEN 9
036960         MOVE ZERO         TO �_���t�a��v
036410         MOVE ZERO         TO �_���t�N�v
036420         MOVE ZERO         TO �_���t���v
036430         MOVE ZERO         TO �_���t���v
036440*    /  ���̑��́A�ŏI�ʉ@�� /
036450     WHEN OTHER
037010         MOVE �ŏI�ʉ@�a��v TO �_���t�a��v
036460         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
036470         MOVE �ŏI�ʉ@���v TO �_���t���v
036480         MOVE �ŏI�ʉ@���v TO �_���t���v
036490     END-EVALUATE.
036500**
036510* (���ґ�)
036520     EVALUATE ���Z�v�g���ғ��t�敪�v 
036530*    /  �ŏI�ʉ@�� /
036540     WHEN ZERO
037100         MOVE �ŏI�ʉ@�a��v TO ���҈ϔC�a��v
036550         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
036560         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
036570         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
036580*    /  ������ /
036590     WHEN 1 
036600         PERFORM �������擾
037160         MOVE �󗝘a��v   TO ���҈ϔC�a��v
036610         MOVE �󗝔N�v     TO ���҈ϔC�N�v
036620         MOVE �󗝌��v     TO ���҈ϔC���v
036630         MOVE �󗝓��v     TO ���҈ϔC���v
036640*    /  �󎚂Ȃ� /
036650     WHEN 9
037210         MOVE ZERO         TO ���҈ϔC�a��v
036660         MOVE ZERO         TO ���҈ϔC�N�v
036670         MOVE ZERO         TO ���҈ϔC���v
036680         MOVE ZERO         TO ���҈ϔC���v
036690*    /  ���̑��́A�ŏI�ʉ@�� /
036700     WHEN OTHER
037260         MOVE �ŏI�ʉ@�a��v TO ���҈ϔC�a��v
036710         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
036720         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
036730         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
036740     END-EVALUATE.
036750*
036760*================================================================*
036770 �������擾 SECTION.
036780*
037350     MOVE �{�p�a��v�q TO �󗝘a��v.
036790     MOVE �{�p�N�v�q   TO �󗝔N�v.
036800     MOVE �{�p���v�q   TO �󗝌��v.
036810     MOVE �{�p�a��v�q TO ���|�����敪.
036820     READ �����}�X�^
036830     NOT INVALID KEY
036840         MOVE ���|�J�n����N TO �{�p����N�v
036850     END-READ.
036860     IF ( �{�p����N�v NOT = ZERO )
036870        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
036880     END-IF.
036890*
036900     EVALUATE �{�p���v�q
036910     WHEN 4
036920     WHEN 6
036930     WHEN 9
036940     WHEN 11
036950         MOVE 30 TO �󗝓��v
036960     WHEN 2
036970         DIVIDE 4 INTO �{�p����N�v GIVING    ���v
036980                                    REMAINDER �]�v
036990         END-DIVIDE
037000         IF ( �]�v = ZERO )
037010             MOVE 29 TO �󗝓��v
037020         ELSE
037030             MOVE 28 TO �󗝓��v
037040         END-IF
037050     WHEN 1
037060     WHEN 3
037070     WHEN 5
037080     WHEN 7
037090     WHEN 8
037100     WHEN 10
037110     WHEN 12
037120         MOVE 31 TO �󗝓��v
037130     WHEN OTHER
037140          CONTINUE
037150     END-EVALUATE.
037160*
037170*================================================================*
037180 ���É��Z�񐔎擾 SECTION.
037190*================================================================*
037200     MOVE ���҃R�[�h�v�q TO �{�L�|���҃R�[�h.
037210     MOVE �{�p�a��v�q   TO �{�L�|�{�p�a��.
037220     MOVE �{�p�N�v�q     TO �{�L�|�{�p�N.
037230     MOVE �{�p���v�q     TO �{�L�|�{�p��.
037240     MOVE ZERO           TO �{�L�|�{�p��.
037250*
037260     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
037270                                  �{�L�|�{�p�a��N����
037280     END-START.
037290     MOVE SPACE TO �I���t���O�Q.
037300     PERFORM �{�p�L�^�e�Ǎ�.
037310     PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
037320                   ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
037330                   ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
037340                   ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
037350                   ( �{�L�|�{�p��     NOT = �{�p���v�q      ) 
037360*
037370*       ****************
037380*       * ���É��Z�� *
037390*       ****************
037400        IF ( �{�L�|���É��Z NOT = ZERO )
037410            COMPUTE ���É��Z�񐔂v = ���É��Z�񐔂v + 1
037420        END-IF
037430*
037440        PERFORM �{�p�L�^�e�Ǎ�
037450     END-PERFORM.
037460*
038000*================================================================*
038010 ������擾 SECTION.
038020*================================================================*
036740* 2006/04 �ύX
036750* ������� "JOSEIMEI" ���Ă�. 
036760     MOVE SPACE TO  �A�������́|�L�[.
036770     INITIALIZE     �A�������́|�L�[.
036780     MOVE ������ʂv�q           TO �A�������́|�������.
036790     MOVE ��p���S�Ҕԍ������v�q TO �A�������́|��p���S�Ҕԍ�����.
036800*
036810     CALL   "JOSEIMEI".
036820     CANCEL "JOSEIMEI".
036830*
036840     MOVE �A�������́|�P���� TO ������v.
038400*
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
038610*================================================================*
038620 ������� SECTION.
038630*================================================================*
038640     MOVE "YHN6121P"  TO  ��`�̖��o.
038650     MOVE "SCREEN"   TO  ���ڌQ���o.
038660     WRITE YHN6121P.
038670     PERFORM �G���[�����o.
038680     PERFORM ���ŏ���.
038690*
038700*================================================================*
038710 ���ŏ���  SECTION.
038720*
038730     MOVE "YHN6121P"  TO  ��`�̖��o.
038740     MOVE "CT"       TO  ������ʂo.
038750     MOVE "PAGE"     TO  �g������o.
038760     MOVE SPACE      TO  ���ڌQ���o.
038770     WRITE YHN6121P.
038780     PERFORM �G���[�����o.
038790     MOVE SPACE     TO  �g������o.
038800*
038810************
038820* �I������  *
038830************
038840*================================================================*
038850 ��f�҈���敪�X�V SECTION.
038860*================================================================*
038870** //  ��f�ҏ��e�̈���敪�ɂP���Z�b�g���A�X�V����B//  
038880*
038890     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
038900     MOVE �{�p�N�v�q         TO ��|�{�p�N.
038910     MOVE �{�p���v�q         TO ��|�{�p��.
038920     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
038930     READ ��f�ҏ��e
038940     NOT INVALID KEY
               IF �A���|�ی���� > 50
036620             MOVE  1  TO  ��|���Z����敪����
               ELSE
036620             MOVE  1  TO  ��|���Z����敪
               END-IF
038960         REWRITE  ��|���R�[�h
038970         END-REWRITE
038980         IF ( ��ԃL�[ NOT = "00" )
038990            MOVE NC"��f��" TO �t�@�C����
039000            PERFORM �G���[�\��
039010         END-IF
039020     END-READ.
039030*
039040*================================================================*
039050 �I������ SECTION.
039060*================================================================*
039070     PERFORM �t�@�C����.
039080*
039090*================================================================*
039100 �t�@�C���� SECTION.
039110*
039120     CLOSE ����t�@�C��.
039130     CLOSE �ی��҃}�X�^     �����}�X�^       ���̃}�X�^
039140           ���Z�v�g�e       ������}�X�^   �{�p�����}�X�^
039150           �h�c�Ǘ��}�X�^   �o�߃}�X�^       ��f�ҏ��e
039160           �{�p�L�^�e       �����f�[�^�e     ���������e
039170           �s�����}�X�^     ����}�X�^     ��ƃt�@�C���R     ��ƃt�@�C���T.
039190*
039280*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
039290*================================================================*
039300 �{�p�L�^�e�Ǎ� SECTION.
039310*================================================================*
039320     READ �{�p�L�^�e NEXT
039330     AT END
039340         MOVE "YES" TO �I���t���O�Q
039350     END-READ.
039360*
039370*================================================================*
039380 �G���[�����o SECTION.
039390*================================================================*
039400     IF ( �ʒm���o NOT = "00" )
039410         DISPLAY NC"���[�G���["              UPON CONS
039420         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
039430         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
039440         DISPLAY NC"�g������o�F" �g������o UPON CONS
039450         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
039460                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
039470         ACCEPT  �L�[���� FROM CONS
039480         PERFORM �t�@�C����
039490         MOVE 99 TO PROGRAM-STATUS
039500         EXIT PROGRAM
039510     END-IF.
039520*
039530*================================================================*
039540 �G���[�\�� SECTION.
039550*================================================================*
039560     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
039570     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
039580     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
039590     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
039600                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
039610     ACCEPT  �L�[���� FROM CONS
039620     PERFORM �t�@�C����.
039630     EXIT PROGRAM.
039640*
039650*----------------------------------------------------------------
039660*================================================================*
039670 �e�X�g�󎚏��� SECTION.
039680*
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
           ������(1) ������(2) ������(3) ��(1) ��(2) ��(3) ������(1) ������(2) ������(3)
           �^����×� ������ �^���� �^����(1) �^����(2) �^����(3) �^����(4) �^����(5)
           .
           MOVE ALL "X" TO
           ���{�p�h�c �ی��Ҕԍ� �L���ԍ� ����S�Ҕԍ� �󋋎Ҕԍ� �Z���P �Z���Q 
           �������`�l �_���t�ԍ� �����ԍ� 
           �{�p���X�֔ԍ��P �{�p���X�֔ԍ��Q 
           �{�p���Z���P �{�p���Z���Q �{�p���d�b�ԍ� ��\�҃J�i �ی��Җ�
           ���������P ���������Q ���������R ���������S ���������T ���������U ���ʂT�W ���ʂT�O
           �������R���P �������R���Q �������R���R �������R���S �������R���T �K�p�Q �����p�� 
           �������R���U �������R���V �ڍ��@�� ��\�Җ� ��ی��Ҏ��� ���Ҏ��� �������q
           .
           MOVE ALL NC"�m" TO
           �������P �������Q �������R �������S �������T �o�ߗ���(1) �o�ߗ���(2) �o�ߗ���(3) 
           �o�ߗ���(4) �o�ߗ���(5) �K�p�P
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
           �{�X�`�F�b�N �x�X�`�F�b�N �{�x���`�F�b�N
           .
041110*================================================================*
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
021810 ��������擾 SECTION.
021820*
           IF (�ی���ʂv�q = 05) OR (�ی���ʂv�q >= 50)
030800         MOVE �ی���ʂv�q   TO �s�|������
030810         MOVE �ی��Ҕԍ��v�q TO �s�|�s�����ԍ�
030820         READ �s�����}�X�^
030830         INVALID KEY
030840             MOVE SPACE      TO �����於�̂v
030850         NOT INVALID KEY
021950             IF �ی���ʂv�q = 05
022160                 STRING �s�|�s�������� DELIMITED BY SPACE
022110                        �s�|�x�������� DELIMITED BY SPACE
022200                        INTO �����於�̂v
022210                 END-STRING
                   ELSE
022160                 STRING �s�|�s�������� DELIMITED BY SPACE
002220                        INTO �����於�̂v
022210                 END-STRING
                   END-IF
               END-READ
           ELSE
021880         MOVE �ی���ʂv�q   TO �ہ|�ی����
021890         MOVE �ی��Ҕԍ��v�q TO �ہ|�ی��Ҕԍ�
021900         READ �ی��҃}�X�^
021910         INVALID KEY
021920             MOVE SPACE      TO �����於�̂v
021930         NOT INVALID KEY
021940* �ЕہA���ق́u�Љ�ی��������v������
021950             EVALUATE �ی���ʂv�q 
021960             WHEN 02
021970             WHEN 06
021980                  IF ( �ہ|�ڔ���敪 = 1 )
021990*                      MOVE �ہ|�ی��Җ���    TO �����於�̂v
022160                      STRING �ہ|�ی��Җ��� DELIMITED BY SPACE
022200                             INTO �����於�̂v
022210                      END-STRING
022000                  ELSE
022010                     STRING �ہ|�ی��Җ���  DELIMITED BY SPACE
022020                           "�Љ�ی�������" DELIMITED BY SIZE
022030                           INTO �����於�̂v
022040                     END-STRING
022050                  END-IF
022060* �g���͎x�����܂ň�
022070             WHEN 03
                        IF �ہ|�x�������� = SPACE
022080                      STRING �ہ|�ی��Җ���     DELIMITED BY SPACE
022090                             "���N�ی��g��"     DELIMITED BY SIZE
022110                             �ہ|�x��������     DELIMITED BY SPACE
022120                             INTO �����於�̂v
022130                      END-STRING
                        ELSE
022080                      STRING �ہ|�ی��Җ���     DELIMITED BY SPACE
022090                             "���N�ی��g��"     DELIMITED BY SIZE
022110                             �ہ|�x��������     DELIMITED BY SPACE
022120                             INTO �����於�̂v
022130                      END-STRING
                        END-IF
022140* ���ς͎x�����܂ň�
022150             WHEN 04
024700*/               ���{�����w�Z�U���E���ώ��ƒc(34130021)�̏ꍇ�A"���ϑg��"��t���Ȃ��B
024710                 IF ( �ہ|�ی��Ҕԍ� = "34130021" )
022160                     STRING �ہ|�ی��Җ��� DELIMITED BY SPACE
022200                            INTO �����於�̂v
022210                     END-STRING
024730                 ELSE
                           IF �ہ|�x�������� = SPACE
022160                         STRING �ہ|�ی��Җ���     DELIMITED BY SPACE
022170                                "���ϑg��"         DELIMITED BY SIZE
022190                                �ہ|�x��������     DELIMITED BY SPACE
022200                                INTO �����於�̂v
022210                         END-STRING
                           ELSE
022160                         STRING �ہ|�ی��Җ���     DELIMITED BY SPACE
022170                                "���ϑg��"         DELIMITED BY SIZE
022190                                �ہ|�x��������     DELIMITED BY SPACE
022200                                INTO �����於�̂v
022210                         END-STRING
                           END-IF
                       END-IF
022220             WHEN OTHER
022230*                 MOVE �ہ|�ی��Җ���   TO �����於�̂v
022160                 STRING �ہ|�ی��Җ��� DELIMITED BY SPACE
022200                        INTO �����於�̂v
022210                 END-STRING
022240             END-EVALUATE
022250         END-READ
           END-IF.
022260*
022270*================================================================*
       ���{�p�h�c�擾 SECTION.
      *
026770*********************************************
026780** �h�c�Ǘ��}�X�^���@���{�p�h�c���擾����B
026790*********************************************
      */�{�p�@�փR�[�h ���ۑސE�݈̂󎚂���/130129
           IF �ی���ʂv�q = 01 OR 08 OR 05
026800         EVALUATE �ی���ʂv�q 
026810* ����
026820             WHEN 01
026830                MOVE �ی��Ҕԍ��v�q(1:2)  TO �h�c�ǁ|�ی����
026840* �ސE
026850             WHEN 08
026860** �������
026870             WHEN 05
026880                MOVE �ی��Ҕԍ��v�q(3:2)  TO �h�c�ǁ|�ی����
026890         END-EVALUATE
026900** / ���{�pID /
026910         MOVE 01                     TO �h�c�ǁ|�h�c�敪
026920         MOVE ZERO                   TO �h�c�ǁ|�{�p���ԍ�
026940         MOVE SPACE                  TO �h�c�ǁ|�ی��Ҕԍ�
026950         READ �h�c�Ǘ��}�X�^
026960         NOT INVALID KEY
026970             MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO ���{�p�h�c�v
026980         END-READ
           END-IF.
           IF �A���|�ی���� > 50
025890        MOVE 01                   TO �h�c�ǁ|�h�c�敪
025900        MOVE ZERO                 TO �h�c�ǁ|�{�p���ԍ�
025910        MOVE ��p���S�Ҕԍ������v�q(3:2)  TO �h�c�ǁ|�ی����
025920        MOVE SPACE                TO �h�c�ǁ|�ی��Ҕԍ�
025930        READ �h�c�Ǘ��}�X�^
025940        NOT INVALID KEY
025950             MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO ���{�p�h�c�v
025960        END-READ
025970*
025980** �s����ID
025990        MOVE 02                     TO �h�c�ǁ|�h�c�敪
026000        MOVE ZERO                   TO �h�c�ǁ|�{�p���ԍ�
026010        MOVE ������ʂv�q           TO �h�c�ǁ|�ی����
026020        MOVE ��p���S�Ҕԍ������v�q TO �h�c�ǁ|�ی��Ҕԍ�
      */���s�s�̏d�x��Q/120711
              IF ��p���S�Ҕԍ������v�q(1:5) = "39261"
026020            MOVE "264"              TO �h�c�ǁ|�ی��Ҕԍ�
              END-IF
      *
026030        READ �h�c�Ǘ��}�X�^
              INVALID KEY
                 IF ��p���S�Ҕԍ������v�q(1:5) = "39261"
025890              MOVE 01                   TO �h�c�ǁ|�h�c�敪
025900              MOVE ZERO                 TO �h�c�ǁ|�{�p���ԍ�
025910              MOVE 50                   TO �h�c�ǁ|�ی����
025920              MOVE SPACE                TO �h�c�ǁ|�ی��Ҕԍ�
025930              READ �h�c�Ǘ��}�X�^
025940              NOT INVALID KEY
026050                 MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO �s�����{�p�h�c�v
                    END-READ
                 END-IF
026040        NOT INVALID KEY
026050           MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO �s�����{�p�h�c�v
026060        END-READ
           END-IF.
022270*================================================================*
       �p�q�f�[�^�Z�b�g SECTION.
      *
009900     MOVE ZERO   TO ��������N�v�p.
009910     MOVE ���Z�|�����a�� TO ���|�����敪.
009920     READ �����}�X�^
009930     NOT INVALID KEY
009940         COMPUTE ��������N�v�p = ���|�J�n����N + ���Z�|�����N - 1
009950     END-READ.
           MOVE ���Z�|������           TO �������v�p.
           PERFORM ����ԍ��E�l��.
           MOVE ����ԍ��E�l�߂v       TO ����ԍ��v�p.
           MOVE �ی��Ҕԍ��v�q         TO �ی��ԍ��v�p.
           MOVE ����S�Ҕԍ��v       TO ����S�Ҕԍ��v�p.
           IF �A���|�ی���� > 50
               MOVE 3                  TO ��Ï����敪�v�p
               MOVE ���Z�|�󋋎ҕ��S�z TO ���S�z�v�p
               MOVE ���Z�|�����������z TO �����z�v�p
           ELSE
               MOVE 1                  TO ��Ï����敪�v�p
               MOVE ���Z�|�ꕔ���S��   TO ���S�z�v�p
               MOVE ���Z�|�������z     TO �����z�v�p
           END-IF.
           MOVE �{�l�Ƒ��敪�v�q       TO �{�l�Ƒ��v�p.
009900     MOVE ZERO   TO �{�p����N�v�p.
009910     MOVE ���Z�|�{�p�a�� TO ���|�����敪.
009920     READ �����}�X�^
009930     NOT INVALID KEY
009940         COMPUTE �{�p����N�v�p = ���|�J�n����N + ���Z�|�{�p�N - 1
009950     END-READ.
           MOVE ���Z�|�{�p��           TO �{�p���v�p.
           MOVE ���Z�|���v             TO ��p�z�v�p.
           MOVE ���Z�|���Z������       TO �������v�p.
           MOVE ���ʐ��v               TO ���ʐ��v�p.
           MOVE ��|���Ҕԍ�           TO ���Ҕԍ��v�p.
           MOVE ��|�}��               TO �}�Ԃv�p.
008870     MOVE SPACE TO �I���t���O�S.
008880     MOVE �J���}�v�p             TO �p�������ڂQ�v.
008890     MOVE ��ی��Ҏ����v         TO �p�������ڂQ�v(2:20)
008900     PERFORM VARYING �����b�m�s FROM 22 BY -1
008910             UNTIL (�����b�m�s  <= ZERO) OR
008920                   (�I���t���O�S = "YES")
008930         IF �p�������ڂQ�v(�����b�m�s:1) NOT = SPACE
008940            COMPUTE �����b�m�s = �����b�m�s + 1
008950            MOVE �J���}�v�p TO �p�������ڂQ�v(�����b�m�s:1)
008960            MOVE "YES" TO �I���t���O�S
008970         END-IF
008980     END-PERFORM.
           STRING ���C�A�E�g�v                         DELIMITED BY SIZE
                  �p�������ڂQ�v(1:�����b�m�s + 1)     DELIMITED BY SIZE
                  ���Ҏ����v                           DELIMITED BY SIZE
             INTO �p�q�f�[�^�v
           END-STRING.
019160*================================================================*
019170 ����ԍ��E�l�� SECTION.
019180*
019190     MOVE �ڍ��t�����ԍ��v(1:8)  TO  ����ԍ����l�߂v.
019200     MOVE SPACE         TO  ����ԍ��E�l�߂v.
019210*
019220     MOVE  9  TO  �J�E���^.
019230*
019240     IF  ����ԍ����l�߂v�P(8) NOT = SPACE
019250         COMPUTE �J�E���^ = �J�E���^  -  1
019260         MOVE ����ԍ����l�߂v�P(8)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019270     END-IF.
019280     IF  ����ԍ����l�߂v�P(7) NOT = SPACE
019290         COMPUTE �J�E���^ = �J�E���^  -  1
019300         MOVE ����ԍ����l�߂v�P(7)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019310     END-IF.
019320     IF  ����ԍ����l�߂v�P(6) NOT = SPACE
019330         COMPUTE �J�E���^ = �J�E���^  -  1
019340         MOVE ����ԍ����l�߂v�P(6)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019350     END-IF.
019360     IF  ����ԍ����l�߂v�P(5) NOT = SPACE
019370         COMPUTE �J�E���^ = �J�E���^  -  1
019380         MOVE ����ԍ����l�߂v�P(5)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019390     END-IF.
019400     IF  ����ԍ����l�߂v�P(4) NOT = SPACE
019410         COMPUTE �J�E���^ = �J�E���^  -  1
019420         MOVE ����ԍ����l�߂v�P(4)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019430     END-IF.
019440     IF  ����ԍ����l�߂v�P(3) NOT = SPACE
019450         COMPUTE �J�E���^ = �J�E���^  -  1
019460         MOVE ����ԍ����l�߂v�P(3)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019470     END-IF.
019480     IF  ����ԍ����l�߂v�P(2) NOT = SPACE
019490         COMPUTE �J�E���^ = �J�E���^  -  1
019500         MOVE ����ԍ����l�߂v�P(2)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019510     END-IF.
019520     IF  ����ԍ����l�߂v�P(1) NOT = SPACE
019530         COMPUTE �J�E���^ = �J�E���^  -  1
019540         MOVE ����ԍ����l�߂v�P(1)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019550     END-IF.
           INSPECT ����ԍ��E�l�߂v REPLACING ALL SPACE BY ZERO.
019560*
022270*================================================================*
041120******************************************************************
041130 END PROGRAM YHN6121.
041140******************************************************************
