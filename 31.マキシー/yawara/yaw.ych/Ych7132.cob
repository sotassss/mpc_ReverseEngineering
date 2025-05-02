000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH7132.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*         �J���e�����(������)�@�Đ��� 
000100*         MED = YCH7130 YCH715P
      */�������ł̒������R�̈���͂��Ȃ�
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2018-02-13
000130 DATE-COMPILED.          2018-02-13
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
000320     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  ���|����敪
000360                             FILE STATUS              IS  ��ԃL�[
000370                             LOCK        MODE         IS  AUTOMATIC.
000380     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS  ��|�{�p�a��N��
000420                                                          ��|���҃R�[�h
000430                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000440                                                          ��|���҃J�i
000450                                                          ��|���҃R�[�h
000460                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000470                                                          ��|�{�p�a��N��
000480                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000490                                                          ��|�ی����
000500                                                          ��|�ی��Ҕԍ�
000510                                                          ��|���҃R�[�h
000520                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000530                                                          ��|������
000540                                                          ��|��p���S�Ҕԍ�
000550                                                          ��|���҃R�[�h
000560                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000570                                                          ��|�������
000580                                                          ��|��p���S�Ҕԍ�����
000590                                                          ��|���҃R�[�h
000600                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
000610                                                          ��|�{�p�a��N��
000620                                                          ��|���҃R�[�h
000630                             FILE STATUS              IS  ��ԃL�[
000640                             LOCK        MODE         IS  AUTOMATIC.
000293     SELECT  �����p���҂e    ASSIGN      TO        CHOKEIL
000294                             ORGANIZATION             IS INDEXED
000295                             ACCESS MODE              IS DYNAMIC
000296                             RECORD KEY               IS  ���p�|�{�p�a��N��
000297                                                          ���p�|���҃R�[�h
000298                             ALTERNATE RECORD KEY     IS  ���p�|���҃R�[�h
000299                                                          ���p�|�{�p�a��N��
000300                             FILE STATUS              IS  ��ԃL�[
000301                             LOCK      MODE           IS  AUTOMATIC.
000530     SELECT  ��ƃt�@�C���P  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7131L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  ��P�|�{�p�a��N����
000610                                                          ��P�|���҃R�[�h
000910                             ALTERNATE RECORD KEY     IS  ��P�|�{�p�a��N����
                                                                ��P�|���҃J�i
                                                                ��P�|���҃R�[�h
000910                             ALTERNATE RECORD KEY     IS  ��P�|���҃R�[�h
                                                                ��P�|�{�p�a��N����
000910                             ALTERNATE RECORD KEY     IS  ��P�|���҃J�i
                                                                ��P�|���҃R�[�h
                                                                ��P�|�{�p�a��N����
000620                             FILE        STATUS       IS  ��ԃL�[
000630                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  ��ƃt�@�C���Q  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7132L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  ��Q�|�{�p�a��N��
000610                                                          ��Q�|���҃R�[�h
000910                             ALTERNATE RECORD KEY     IS  ��Q�|���҃R�[�h
                                                                ��Q�|�{�p�a��N��
000620                             FILE        STATUS       IS  ��ԃL�[
000630                             LOCK        MODE         IS  AUTOMATIC.
      */�}�ԗL��`�F�b�N�p
000970     SELECT  ��ƃt�@�C���R  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W7133L.DAT"
000980                             ORGANIZATION             IS  INDEXED
000990                             ACCESS                   IS  DYNAMIC
001000                             RECORD      KEY    IS        ��R�|���Ҕԍ�
                                                                ��R�|�{�p�a��N��
001010                             FILE        STATUS       IS  ��ԃL�[
001020                             LOCK        MODE         IS  AUTOMATIC.
000920     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
000930                             SYMBOLIC    DESTINATION  IS "PRT"
000940                             FORMAT                   IS  ��`�̖��o
000950                             GROUP                    IS  ���ڌQ���o
000960                             PROCESSING  MODE         IS  ������ʂo
000970                             UNIT        CONTROL      IS  �g������o
000980                             FILE        STATUS       IS  �ʒm���o.
000990******************************************************************
001000*                      DATA DIVISION                             *
001010******************************************************************
001020 DATA                    DIVISION.
001030 FILE                    SECTION.
001070*                           �m�q�k��  �Q�T�U�n
001080 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001090     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001090     COPY SEIGYO02        OF  XFDLIB  JOINING   ���O�Q   AS  PREFIX.
001100*                           �m�q�k��  �R�Q�O�n
001110 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001120     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000686*                           �m�q�k��  �P�Q�W�n
000687 FD  �����p���҂e          BLOCK   CONTAINS   1   RECORDS.
000688     COPY CHOKEI          OF  XFDLIB  JOINING   ���p   AS  PREFIX.
001340*                         �m�q�k��  �R�W�S�n
001350 FD  ��ƃt�@�C���P RECORD  CONTAINS 384 CHARACTERS.
001020 01 ��P�|���R�[�h.
001030    03 ��P�|���R�[�h�L�[.
001040       05  ��P�|�{�p�a��N����.
001050          07 ��P�|�{�p�a��               PIC 9.
001060          07 ��P�|�{�p�N��.
001070             09 ��P�|�{�p�N              PIC 9(2).
001080             09 ��P�|�{�p��              PIC 9(2).
001090          07 ��P�|�{�p��                 PIC 9(2).
001110       05 ��P�|���҃R�[�h.
001120          07 ��P�|���Ҕԍ�               PIC 9(6).
001130          07 ��P�|�}��                   PIC X.
001100    03 ��P�|���R�[�h�f�[�^.
             05 ��P�|���҃J�i                  PIC X(50).
001140* �������@�����{���ʖ��f�[�^�@������
001150       05  ��P�|���`�F�b�N               PIC 9.
001160       05  ��P�|���ʃf�[�^               OCCURS 5.
001170           07  ��P�|���ʌv               PIC 9(5).
001180       05  ��P�|�d㪗�.
001190           07  ��P�|㪖@���z             PIC 9(4).
001200           07  ��P�|�d�Ë��z             PIC 9(4).
001210       05  ��P�|�ꕔ���S��               PIC 9(5).
001210       05  ��P�|������                   PIC 9(4).
001210       05  ��P�|�^����                   PIC 9(3).
001551       05  ��P�|�R�����g                 PIC X(100).
             05 ��P�|����                      PIC 9.
003280       05 ��P�|���f��.
003290          07 ��P�|������                 PIC 9(4).
003300          07 ��P�|�������Z��             PIC 9(4).
003310          07 ��P�|�x��                   PIC 9.
003320          07 ��P�|�[��                   PIC 9.
003330          07 ��P�|���ԊO                 PIC 9.
             05 ��P�|���k��                    PIC 9(4).
003340       05 ��P�|�Č���                    PIC 9(4).
003350       05 ��P�|���񏈒u                  OCCURS 5.
003360          07 ��P�|���񏈒u��             PIC 9(5).
003370       05 ��P�|�{�×��`�F�b�N            PIC N.
003380       05 ��P�|�������`�F�b�N            PIC N.
003390       05 ��P�|�Œ藿�`�F�b�N            PIC N.
003400       05 ��P�|���É��Z.
003410          07 ��P�|���                   PIC 9.
003420          07 ��P�|�\��                   PIC 9.
003430          07 ��P�|���É��Z��             PIC 9(5).
003440          07 ��P�|����                   PIC 9(3)V9.
003450       05 ��P�|���×�                    OCCURS 3.
003460          07 ��P�|��                     PIC 9(2).
003470          07 ��P�|��                   PIC 9(2).
003480          07 ��P�|���×����z             PIC 9(6).
003490       05 ��P�|������                    PIC 9.
003500       05 ��P�|������                    PIC 9.
003510       05 ��P�|������                    PIC 9.
003520       05 ��P�|���z                      PIC 9(6).
003530       05 ��P�|���񋟗�                PIC 9(6).
001220       05  FILLER                         PIC X(43).
001230*
001340*                         �m�q�k��  �U�S�n
001350 FD  ��ƃt�@�C���Q RECORD  CONTAINS 64 CHARACTERS.
001360 01 ��Q�|���R�[�h.
001370    03 ��Q�|���R�[�h�L�[.
001535       05 ��Q�|�{�p�a��N��.
001536          07 ��Q�|�{�p�a��                PIC 9.
001537          07 ��Q�|�{�p�N��.
001538             09 ��Q�|�{�p�N               PIC 9(2).
001539             09 ��Q�|�{�p��               PIC 9(2).
001460       05 ��Q�|���҃R�[�h.
001470          07 ��Q�|���Ҕԍ�                PIC 9(6).
001480          07 ��Q�|�}��                    PIC X(1).
001490    03 ��Q�|���R�[�h�f�[�^.
             05 ��Q�|�����v.
001551          07 ��Q�|������                  PIC 9(2).
001551          07 ��Q�|��p�z                  PIC 9(7).
001551          07 ��Q�|���S�z                  PIC 9(6).
001551          07 ��Q�|�����z                  PIC 9(7).
001500       05 FILLER                           PIC X(30).
      *
001580*                           �m�q�k��  �P�U�n
001590 FD  ��ƃt�@�C���R RECORD  CONTAINS 16 CHARACTERS.
001600 01 ��R�|���R�[�h.
001610    03 ��R�|���R�[�h�L�[.
001700       05 ��R�|���Ҕԍ�                  PIC 9(6).
001620       05 ��R�|�{�p�a��N��.
001630          07 ��R�|�{�p�a��               PIC 9.
001640          07 ��R�|�{�p�N��.
001650             09 ��R�|�{�p�N              PIC 9(2).
001660             09 ��R�|�{�p��              PIC 9(2).
001680    03 ��R�|���R�[�h�f�[�^.
001720       05 ��R�|�}�Ԃe                    PIC 9.
             05 ��R�|�ی��ύX�e                PIC 9.
001520       05 FILLE                           PIC X(3).
001530*
001540 FD  ����t�@�C��.
001550     COPY YCH715P        OF  XMDLIB.
001560*----------------------------------------------------------------*
001570******************************************************************
001580*                WORKING-STORAGE SECTION                         *
001590******************************************************************
001600 WORKING-STORAGE         SECTION.
001610 01 �L�[����                           PIC X     VALUE SPACE.
001620 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
001630 01 �I���t���O                         PIC X(3)  VALUE SPACE.
001640 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
001640 01 �I���t���O�R                       PIC X(3)  VALUE SPACE.
001650 01 �����t���O                         PIC 9(1)  VALUE ZERO.
001650 01 �E�o�t���O                         PIC X(3)  VALUE SPACE.
001660 01 �t�@�C����                         PIC N(6)  VALUE SPACE.
001670 01 ���Z�v�g�o�f�v                     PIC X(8)  VALUE SPACE.
001680 01 �O�a��v                           PIC 9     VALUE ZERO.
001690 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
001700 01 ����ʒu�b�m�s                     PIC 9(2)  VALUE ZERO.
001710 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
001720 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
001730 01 ���Ҕԍ��v                         PIC 9(6)  VALUE ZERO.
001210 01 �I�[�v���t���O                     PIC X(3)   VALUE SPACE.
001210 01 ���b�Z�[�W�v                       PIC X(240) VALUE SPACE.
002420 01 ���Ŋm�F�v                         PIC 9(1) VALUE ZERO.
       01 ���s�v                             PIC X(2)  VALUE X"0D0A".
001740**
001750 01 �x���t���O                         PIC X(3) VALUE SPACE.
001760 01 �x���񐔂v                         PIC 9(4) VALUE ZERO.
001770 01 �x���b�m�s                         PIC 9(5) VALUE ZERO.
001740**
001060 01 �{�p�a��N���v.
001070    03 �{�p�a��v                    PIC 9(1) VALUE ZERO.
001080    03 �{�p�N�v                      PIC 9(2) VALUE ZERO.
001090    03 �{�p���v                      PIC 9(2) VALUE ZERO.
001870 01 �I���a��N���v.
001880    03 �I���a��v                    PIC 9(1)  VALUE ZERO.
001890    03 �I���N�v                      PIC 9(2)  VALUE ZERO.
001900    03 �I�����v                      PIC 9(2)  VALUE ZERO.
001780**
      */�������R���p���[�N������0601
       01 �������R���Q�v.
          03 �������R���v PIC N(40) OCCURS 16 VALUE SPACE.
001201 01 �ԍ��J�E���^                       PIC 9(2) VALUE ZERO.
001329****
005080 01 �������擾�p�v.
005090    03 �����I�����v                    PIC 9(2) VALUE ZERO.
005100    03 �Ώې���v                      PIC 9(4) VALUE ZERO.
005110    03 ���v                            PIC 9(3) VALUE ZERO.
005120    03 �]�v                            PIC 9(3) VALUE ZERO.
001329****
001330 01 ����������ʂv                     PIC 9(2) VALUE ZERO.
001331 01 �������ʂv                         PIC 9(2) VALUE ZERO.
001332 01 ���R�R�[�h�v                       PIC 9(3) VALUE ZERO.
001340 01 �Ǐ�R�[�h�v                       PIC 9(2) VALUE ZERO.
001350 01 �Ǐ󖼂v                           PIC N(4) VALUE SPACE.
001361 01 �������R���v.
001369    03 �����ԍ��v                      PIC X(2)   VALUE SPACE.
001360    03 ���R���v                        PIC X(320) VALUE SPACE.
001362 01 �����s���v                         PIC 9(2)  VALUE ZERO.
001363 01 �S�p��                           PIC X(2)  VALUE X"8140".
001364 01 ���p��                           PIC X(2)  VALUE X"2020".
001365***
001366*** �Ǐ�p
001367 01 �Ǐ�v.
001368    03 �Ǐ���e�v                      PIC N(29) VALUE SPACE.
001401** �K�p�Q�T�����p
001402 01 ���R�Q�T���v.
001403    03 ���R�Q�T���P�v                  PIC X(50) VALUE SPACE.
001404    03 ���R�Q�T���Q�v                  PIC X(50) VALUE SPACE.
001405    03 ���R�Q�T���R�v                  PIC X(50) VALUE SPACE.
001405    03 ���R�Q�T���S�v                  PIC X(50) VALUE SPACE.
001405    03 ���R�Q�T���T�v                  PIC X(50) VALUE SPACE.
001405    03 ���R�Q�T���U�v                  PIC X(50) VALUE SPACE.
001405    03 ���R�Q�T���V�v                  PIC X(50) VALUE SPACE.
001405    03 ���R�Q�T���W�v                  PIC X(50) VALUE SPACE.
001401** �K�p�R�T�����p
001402 01 ���R�R�T���v.
001403    03 ���R�R�T���P�v                  PIC N(35) VALUE SPACE.
001404    03 ���R�R�T���Q�v                  PIC N(35) VALUE SPACE.
001405    03 ���R�R�T���R�v                  PIC N(35) VALUE SPACE.
001405    03 ���R�R�T���S�v                  PIC N(35) VALUE SPACE.
001405    03 ���R�R�T���T�v                  PIC N(35) VALUE SPACE.
001405    03 ���R�R�T���U�v                  PIC N(35) VALUE SPACE.
001401** �K�p�S�O�������Z�v�g�p
001402 01 ���R�S�O���v.
001403    03 ���R�S�O���P�v                  PIC N(40) VALUE SPACE.
001404    03 ���R�S�O���Q�v                  PIC N(40) VALUE SPACE.
001405    03 ���R�S�O���R�v                  PIC N(40) VALUE SPACE.
001405    03 ���R�S�O���S�v                  PIC N(40) VALUE SPACE.
001405    03 ���R�S�O���T�v                  PIC N(40) VALUE SPACE.
      *
      */�������R���p���[�N������0601
001790*
001800****************
001810* �A�����ڑҔ� *
001820****************
001830*    ************
001840*    * ����L�[ *
001850*    ************
001860 01 �Ώۃf�[�^�v�q.
001870    03 �J�n�a��N���v�q.
001880       05 �J�n�a��v�q                  PIC 9(1)  VALUE ZERO.
001890       05 �J�n�N�v�q                    PIC 9(2)  VALUE ZERO.
001900       05 �J�n���v�q                    PIC 9(2)  VALUE ZERO.
001910    03 �J�n���v�q                       PIC 9(2)  VALUE ZERO.
001870    03 �I���a��N�����v�q.
001870       05 �I���a��N���v�q.
001880          07 �I���a��v�q               PIC 9(1)  VALUE ZERO.
001890          07 �I���N�v�q                 PIC 9(2)  VALUE ZERO.
001900          07 �I�����v�q                 PIC 9(2)  VALUE ZERO.
001920       05 �I�����v�q                    PIC 9(2)  VALUE ZERO.
001990    03 ���҃J�i�v�q                     PIC X(50) VALUE SPACE.
002000    03 ���҃R�[�h�v�q.
002010       05 ���Ҕԍ��v�q                  PIC 9(6)  VALUE ZERO.
002020       05 �}�Ԃv�q                      PIC X(1)  VALUE SPACE.
002030    03 �������[�h�v�q                   PIC 9(1)  VALUE ZERO.
002040    03 �����ƃ��[�h�v�q                 PIC 9(1)  VALUE ZERO.
002090    03 �R�����g���[�h�v�q               PIC 9(1)  VALUE ZERO.
002050    03 �������׃��[�h�v�q               PIC 9(1)  VALUE ZERO.
002060    03 �����v���[�h�v�q                 PIC 9(1)  VALUE ZERO.
002120    03 �󎚈ʒu�b�m�s                   PIC 9(2)  VALUE ZERO.
002130    03 �i���v�q                         PIC 9(1)  VALUE ZERO.
002160*
002190**************
002200* ��f�ҏ�� *
002210*************
002220 01 ��f�ҏ��v.
002230    03 �{�p�a��N���v�q.
001070       05 �{�p�a��v�q                 PIC 9(1) VALUE ZERO.
002240       05 �{�p�N�v�q                   PIC 9(2)   VALUE ZERO.
002250       05 �{�p���v�q                   PIC 9(2)   VALUE ZERO.
002260    03 �ی��Ҕԍ��v                    PIC X(8)   VALUE SPACE.
002330    03 ��p���S�Ҕԍ������v            PIC X(8)   VALUE SPACE.
002360    03 �ی���ʂv                      PIC 9(2)   VALUE ZERO.
002360    03 ������ʂv                      PIC 9(2)   VALUE ZERO.
002370    03 ���ҏ��v.
002380       05 ���҃J�i�v                   PIC X(50)  VALUE SPACE.
002390       05 ���Ҏ����v                   PIC X(50)  VALUE SPACE.
002400************
002410* ������� *
002420************
001870 01 �������v.
001880    03 �������Z�v.
001890       05 ���ԊO�`�F�b�N�v                PIC N(1) VALUE SPACE.
001900       05 �x���`�F�b�N�v                  PIC N(1) VALUE SPACE.
001910       05 �[��`�F�b�N�v                  PIC N(1) VALUE SPACE.
001920    03 ���É��Z�v.
001930       05 ��ԃ`�F�b�N�v                  PIC N(1) VALUE SPACE.
001940       05 �\���`�F�b�N�v                  PIC N(1) VALUE SPACE.
001950    03 ���Ë����v�q                       PIC 9(3)V9 VALUE ZERO.
001960    03 ���×��v�q                         OCCURS 3.
001970       05 ���Ì��v�q                      PIC 9(2)  VALUE ZERO.
001980       05 ���É񐔂v�q                    PIC 9(2)  VALUE ZERO.
001990       05 ���×����v�q                    PIC 9(6)  VALUE ZERO.
002000    03 ���É��Z���v�q                     PIC 9(5)  VALUE ZERO.
002010    03 �������q�`�F�b�N�v.
002020       05 ��`�F�b�N�v                    PIC N(1) VALUE SPACE.
002030       05 ���`�F�b�N�v                    PIC N(1) VALUE SPACE.
002040       05 ���`�F�b�N�v                    PIC N(1) VALUE SPACE.
002050    03 �������q���v�q                     PIC 9(5) VALUE ZERO.
002060    03 �������v�q                         PIC 9(4) VALUE ZERO.
002060    03 ���k���v�q                         PIC 9(4) VALUE ZERO.
002070    03 �Č����v�q                         PIC 9(4) VALUE ZERO.
002080    03 �������Z���v�q                     PIC 9(4) VALUE ZERO.
002090    03 ���񏈒u���v�q                     PIC 9(5) OCCURS 5 VALUE ZERO.
002100    03 ���񏈒u���`�F�b�N�v.
002110       05 �������`�F�b�N�v                PIC N(1) VALUE SPACE.
002120       05 �Œ藿�`�F�b�N�v                PIC N(1) VALUE SPACE.
002130       05 �{�×��`�F�b�N�v                PIC N(1) VALUE SPACE.
002140    03 ���񋟗��v�q                     PIC 9(6) VALUE ZERO.
002150 01 ����.
002160    03 㪖@���v                           PIC 9(4) VALUE ZERO.
002170    03 �d�×��v                           PIC 9(4) VALUE ZERO.
002180    03 �ꕔ���S���v                       PIC 9(5) VALUE ZERO.
002190    03 ���ʂv                             OCCURS 5.
002200       05 ���ʌv�v                        PIC 9(5) VALUE ZERO.
002210       05 �����v                          PIC 9(2) VALUE ZERO.
002520    03 �R�����g�v.
002530       05 �R�����g�P�v                    PIC X(50) VALUE SPACE.
002540       05 �R�����g�Q�v                    PIC X(50) VALUE SPACE.
002550*******************************************************************
002560 01 �������.
002570     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
002580     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
002590     03 ������ʂo                     PIC X(2) VALUE SPACE.
002600     03 �g������o.
002610         05 �[������o.
002620             07 �ړ������o             PIC X(1) VALUE SPACE.
002630             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
002640         05 �ڍא���o                 PIC X(2) VALUE SPACE.
002650     03 �ʒm���o                     PIC X(2) VALUE SPACE.
002660     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
002670*
002680 01 �v�Z�@����N�v                     PIC 9(2) VALUE ZERO.
002690* ���t�v�n�q�j
002700 01 �a��I���N�v                       PIC 9(4) VALUE ZERO.
002710 01 �v�Z�@����.
002720    03 �v�Z�@����N                    PIC 9(4) VALUE ZERO.
002730    03 �v�Z�@�����                  PIC 9(4) VALUE ZERO.
002740 01 �v�Z�@����q REDEFINES �v�Z�@����.
002750    03 �v�Z�@���I                      PIC 9(2).
002760    03 �v�Z�@���t                      PIC 9(6).
002770    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
002780       05 �v�Z�@�N��                   PIC 9(4).
002790       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
002800         07 �v�Z�@�N                   PIC 9(2).
002810         07 �v�Z�@��                   PIC 9(2).
002820       05 �v�Z�@��                     PIC 9(2).
002280*
001831* POWER COBOL�p
001832 01 dll-name  PIC X(260)  VALUE SPACE.
001833 01 form-name PIC X(14)   VALUE SPACE.
002830*
002840******************************************************************
002850*                          �A������                              *
002860******************************************************************
002870************
002880* ����L�[ *
002890************
       01 �A��|�Ώۃf�[�^�x�b�g�V�P�R�O IS EXTERNAL GLOBAL.
          03 �A��|�{�p�a��N��.
             05 �A��|�{�p�a��                  PIC 9(1).
             05 �A��|�{�p�N                    PIC 9(2).
             05 �A��|�{�p��                    PIC 9(2).
          03 �A��|���҃R�[�h.
             05 �A��|���Ҕԍ�                  PIC 9(6).
             05 �A��|�}��                      PIC X(1).
          03 �A��|�������[�h                   PIC 9(1).
          03 �A��|���׃��[�h                   PIC 9(1).
          03 �A��|�R�����g���[�h               PIC 9(1).
          03 �A��|�������׃��[�h               PIC 9(1).
          03 �A��|�����v���[�h                 PIC 9(1).
          03 �A��|�����v�i��                   PIC 9(1).
          03 �A��|�ǂݏ�                       PIC X(4).
          03 �A��|�i��                         PIC 9(2).
          03 �A��|�����L�[                     PIC X(4).
003620*
003140****************
003150* ��ʓ��͏�� *
003160****************
       01 �A���|���̓f�[�^�x�b�g�V�P�R�O IS EXTERNAL GLOBAL.
          03 �A���|�J�n�a��N����.
             05 �A���|�J�n�a��                  PIC 9(1).
             05 �A���|�J�n�N                    PIC 9(2).
             05 �A���|�J�n��                    PIC 9(2).
             05 �A���|�J�n��                    PIC 9(2).
          03 �A���|�I���a��N����.
             05 �A���|�I���a��                  PIC 9(1).
             05 �A���|�I���N                    PIC 9(2).
             05 �A���|�I����                    PIC 9(2).
             05 �A���|�I����                    PIC 9(2).
          03 �A���|�ی����                     PIC 9(2).
          03 �A���|�{�l�Ƒ��敪                 PIC 9(1).
          03 �A���|���҃R�[�h.
             05 �A���|���Ҕԍ�                  PIC 9(6).
             05 �A���|�}��                      PIC X(1).
          03 �A���|�������                     PIC 9(2).
      *
       01 �A���|�\���t���O�x�b�g�V�P�R�O IS EXTERNAL.
          03 �A���|�v���r���[�敪              PIC 9(1).
003480*
003490**********************
003500* ���b�Z�[�W�\���L�[ *
003510**********************
003520 01 �A���|�L�[ IS EXTERNAL.
003530    03  �A���|���b�Z�[�W                 PIC N(20).
003540*
       01 �A���b�Z�[�W�o���O�O�T�P IS EXTERNAL.
          03 �A���o�|���b�Z�[�W�ԍ�       PIC 9(2).
          03 �A���o�|���b�Z�[�W.
             05 �A���o�|���b�Z�[�W���e    PIC X(40) OCCURS 6.
          03 �A���o�|���b�Z�[�W�P         PIC X(20).
          03 �A���o�|���b�Z�[�W�Q         PIC X(12).
          03 �A���o�|�Ԃ�l               PIC X.
003540*
       01 �A�o�b���b�Z�[�W��� IS EXTERNAL.
          03 �A�o�b���|���b�Z�[�W���e      PIC X(240).
003554*
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
003555*
003560******************************************************************
003570*                      PROCEDURE  DIVISION                       *
003580******************************************************************
003590 PROCEDURE               DIVISION.
003600************
003610*           *
003620* ��������   *
003630*           *
003640************
002570     PERFORM �v�����^�t�@�C���쐬.
003650     PERFORM ������.
003660     PERFORM ������擾.
003670************
003680*           *
003690* �又��     *
003700*           *
003710************
003720* ���
003730     PERFORM �A�����ڑҔ�.
           MOVE "YES"             TO �I���t���O.
003770     PERFORM ����Z�b�g.
003790     PERFORM �������.
003800************
003810*           *
003820* �I������   *
003830*           *
003840************
003850     PERFORM �I������.
003860     PERFORM �x������.
003870     EXIT PROGRAM.
003880*
003890*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCH715"              TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
003900*================================================================*
003910 ������ SECTION.
003920*
003930     PERFORM �t�@�C���I�[�v��.
003940*    /* ���ݓ��t�擾 */
003950     ACCEPT �v�Z�@���t FROM DATE.
003960*    /* 1980�`2079�N�̊ԂŐݒ� */
003970     IF �v�Z�@�N > 80
003980         MOVE 19 TO �v�Z�@���I
003990     ELSE
004000         MOVE 20 TO �v�Z�@���I
004010     END-IF.
004040     COMPUTE �v�Z�@����N�v = �v�Z�@����N - 1988.
004450*================================================================*
004460 �t�@�C���I�[�v�� SECTION.
004470*
004510     OPEN INPUT   ������}�X�^
004520         MOVE NC"������" TO �t�@�C����.
004530         PERFORM �I�[�v���`�F�b�N.
004540     OPEN INPUT   ��f�ҏ��e.
004550         MOVE NC"���" TO �t�@�C����.
004560         PERFORM �I�[�v���`�F�b�N.
002650     OPEN INPUT   �����p���҂e.
002651         MOVE NC"�����p���҂e" TO �t�@�C����.
002652         PERFORM �I�[�v���`�F�b�N.
004630*     OPEN I-O   ����t�@�C��
004640*         PERFORM �G���[�����o.
004650*================================================================*
004660 �I�[�v���`�F�b�N SECTION.
004670*
004680     IF ��ԃL�[  NOT =  "00"
004690         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
004700         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
004710         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004720                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
004730         ACCEPT  �L�[���� FROM CONS
004740         PERFORM �t�@�C����
004750         EXIT PROGRAM.
004760*================================================================*
004770 ������擾 SECTION.
004780*
004790     MOVE ZERO TO ���|����敪
004800     READ ������}�X�^
004810     NOT INVALID KEY
004820         MOVE ���|�x����       TO �x���񐔂v
004830     END-READ.
004840*
004850*================================================================*
004860 �x������ SECTION.
004870*
004880     PERFORM VARYING �x���b�m�s FROM 1 BY 1
004890                                UNTIL �x���b�m�s > �x���񐔂v
004900         MOVE SPACE TO �x���t���O
004910     END-PERFORM.
004920*
004930*================================================================*
004940 �A�����ڑҔ� SECTION.
004950*
004960     MOVE �A���|�J�n�a��           TO �J�n�a��v�q �{�p�a��v�q �{�p�a��v.
004970     MOVE �A���|�J�n�N             TO �J�n�N�v�q �{�p�N�v�q �{�p�N�v.
004980     MOVE �A���|�J�n��             TO �J�n���v�q �{�p���v�q �{�p���v.
004990     MOVE �A���|�J�n��             TO �J�n���v�q.
004960     MOVE �A���|�I���a��           TO �I���a��v�q �I���a��v.
004970     MOVE �A���|�I���N             TO �I���N�v�q �I���N�v.
004980     MOVE �A���|�I����             TO �I�����v�q �I�����v.
005000     MOVE �A���|�I����             TO �I�����v�q.
005010     MOVE �A���|���Ҕԍ�           TO ���Ҕԍ��v�q.
005020     MOVE �A���|�}��               TO �}�Ԃv�q.
005150     MOVE �A��|�������[�h         TO �������[�h�v�q.
005040     MOVE �A��|���׃��[�h         TO �����ƃ��[�h�v�q.
005090     MOVE �A��|�R�����g���[�h     TO �R�����g���[�h�v�q.
005050     MOVE �A��|�������׃��[�h     TO �������׃��[�h�v�q.
005060     MOVE �A��|�����v���[�h       TO �����v���[�h�v�q.
005120     MOVE �A��|�i��               TO ����ʒu�b�m�s.
005130     MOVE �A��|�����v�i��         TO �i���v�q.
      *
005160*================================================================*
005170 ����Z�b�g SECTION.
005180*
004600     OPEN INPUT ��ƃt�@�C���P
004610         MOVE NC"��ƂP" TO �t�@�C����.
004620         PERFORM �I�[�v���`�F�b�N.
004600     OPEN INPUT ��ƃt�@�C���Q
004610         MOVE NC"��ƂQ" TO �t�@�C����.
004620         PERFORM �I�[�v���`�F�b�N.
004600     OPEN INPUT ��ƃt�@�C���R
004610         MOVE NC"��ƂR" TO �t�@�C����.
004620         PERFORM �I�[�v���`�F�b�N.
      *
005190     PERFORM ���ڏ�����.
005200     PERFORM ��f�ҏ��擾.
005370     IF ( �����ƃ��[�h�v�q = 1 ) OR
005390        ( �R�����g���[�h�v�q = 1 ) OR ( �������׃��[�h�v�q = 1 ) 
005400         PERFORM ����f�[�^�Z�b�g
005410         PERFORM ����s���ޔ�
           ELSE
005220         IF �������[�h�v�q = 1
                   PERFORM �w�b�_�Z�b�g
005360         END-IF
           END-IF.
005220     IF �����v���[�h�v�q = 1
               MOVE �I���a��N���v�q  TO �I���a��N���v
               PERFORM �����v�Z�b�g�Q
005360     END-IF.
008670     CLOSE  ��ƃt�@�C���P ��ƃt�@�C���Q ��ƃt�@�C���R.
005460*================================================================*
005470 ���ڏ����� SECTION.
005480*
005500     INITIALIZE �������v.
005510     MOVE SPACE TO YCH715P.
008200*================================================================*
008210 ��f�ҏ��擾 SECTION.
008220*
008230**************************************************
008240* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
008250* �� �{�p�N ..... �{�p�N�v�Ɋi�[                 *
008260* �� �{�p�� ..... �{�p���v�Ɋi�[                 *
008270* �� ���Ҕԍ�.... ���Ҕԍ��v�Ɋi�[���e�c�A�ԗp   *
008280* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
008290**************************************************
008300     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
008310     MOVE �{�p�N�v�q         TO ��|�{�p�N.
008320     MOVE �{�p���v�q         TO ��|�{�p��.
008330     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
008340     READ ��f�ҏ��e
008350     INVALID KEY
008360         CONTINUE
008370*            /* ���肦�Ȃ� */
008380     NOT INVALID KEY
008420         MOVE ��|���Ҏ���     TO ���Ҏ����v
008430     END-READ.
005530*================================================================*
005540 ����f�[�^�Z�b�g SECTION.
005550*
           MOVE 1                  TO �����t���O.
           MOVE SPACE              TO �I���t���O.
005560     MOVE �J�n�a��v�q       TO ��P�|�{�p�a��.
005570     MOVE �J�n�N�v�q         TO ��P�|�{�p�N.
005580     MOVE �J�n���v�q         TO ��P�|�{�p��.
005590     MOVE �J�n���v�q         TO ��P�|�{�p��.
005590     MOVE ���҃R�[�h�v�q     TO ��P�|���҃R�[�h.
005600     START ��ƃt�@�C���P KEY IS >= ��P�|���҃R�[�h
                                          ��P�|�{�p�a��N����
005610     END-START.
005620     IF ��ԃL�[ = "00"
005630         PERFORM ��ƃt�@�C���P�Ǎ�
               MOVE ���Ҕԍ��v�q     TO ��R�|���Ҕԍ�
               MOVE �J�n�a��N���v�q TO ��R�|�{�p�a��N��
008420         READ ��ƃt�@�C���R
               NOT INVALID KEY
                  IF (�}�Ԃv�q = SPACE) AND (��R�|�}�Ԃe = 1)
                     MOVE SPACE  TO  �A���b�Z�[�W�o���O�O�T�P
                     MOVE 10     TO  �A���o�|���b�Z�[�W�ԍ�
                     MOVE "�}�Ԃ�����܂��B"               TO �A���o�|���b�Z�[�W���e(1)
                     MOVE "������Ă�낵����΁m�͂��n��" TO �A���o�|���b�Z�[�W���e(2)
                     MOVE "�����Ă��������B"               TO �A���o�|���b�Z�[�W���e(3)
                     MOVE "PMSG0051.DLL"     TO dll-name
                     MOVE "PMSG0051"         TO form-name
                     CALL "POWEROPENSHEET" USING dll-name form-name
                     EVALUATE  �A���o�|�Ԃ�l
                     WHEN "Y"
                         CONTINUE
                     WHEN OTHER
                         PERFORM �I������
005933                   PERFORM �x������
005934                   EXIT PROGRAM
                     END-EVALUATE
                  END-IF
               END-READ
005220         IF �������[�h�v�q = 1
                   PERFORM �w�b�_�Z�b�g
005360         END-IF
005640         PERFORM UNTIL (��P�|�{�p�a��N���� > �I���a��N�����v�q) OR
                             (�I���t���O = "YES") OR
                             (��P�|���҃R�[�h NOT = ���҃R�[�h�v�q)
005640            PERFORM UNTIL (��P�|�{�p�a��     NOT = �{�p�a��v�q) OR
                                (��P�|�{�p�N       NOT = �{�p�N�v�q) OR
                                (��P�|�{�p��       NOT = �{�p���v�q) OR
                                (�I���t���O = "YES") OR
                                (��P�|���҃R�[�h NOT = ���҃R�[�h�v�q)
                     IF ( ��P�|���� = 1 ) AND (�����t���O = ZERO)
                        STRING "����������܂��B"                  DELIMITED BY SIZE
                               ���s�v                              DELIMITED BY SIZE
                               "�����̑O�܂ň�����Ē��f���܂��B"  DELIMITED BY SIZE
                          INTO ���b�Z�[�W�v
                        END-STRING
005830                  MOVE SPACE  TO  �A�o�b���b�Z�[�W���
                        MOVE ���b�Z�[�W�v     TO �A�o�b���|���b�Z�[�W���e
004793                  MOVE "Pmsg001.DLL"    TO dll-name
004794                  MOVE "Pmsg001"        TO form-name
004795                  CALL "POWEROPENSHEET" USING dll-name form-name
005830                  MOVE SPACE            TO  ���b�Z�[�W�v
005430                  IF �����v���[�h�v�q = 1
                           MOVE �{�p�a��v�q  TO �I���a��v
                           MOVE �{�p�N�v�q    TO �I���N�v
                           MOVE �{�p���v�q    TO �I�����v
                           PERFORM �����v�Z�b�g�Q
005450                  END-IF
005810                  PERFORM �������
005820                  PERFORM ���ŏ���
                        PERFORM ����s���ޔ�
005932                  PERFORM �I������
005933                  PERFORM �x������
005934                  EXIT PROGRAM
                     END-IF
005470               IF ����ʒu�b�m�s > 31
005430                  IF �����v���[�h�v�q = 1
                           MOVE �{�p�a��v�q  TO �I���a��v
                           MOVE �{�p�N�v�q    TO �I���N�v
                           MOVE �{�p���v�q    TO �I�����v
                           PERFORM �����v�Z�b�g
005450                  END-IF
                        PERFORM �������
005820                  PERFORM ���ŏ���
005510                  MOVE 1      TO ����ʒu�b�m�s
005510                  MOVE 1      TO �i���v�q
005520                  MOVE SPACE  TO  �A���|�L�[
005530                  MOVE NC"�V�����p�����Z�b�g���Ă�������" TO �A���|���b�Z�[�W
005540                  CALL   "MSG001"
005550                  CANCEL "MSG001"
005900                  IF �������[�h�v�q = 1
                            PERFORM �w�b�_�Z�b�g
005930                  END-IF
005940               END-IF
005340               IF (�����ƃ��[�h�v�q = 1) AND (��P�|���`�F�b�N = 1)
005350                  MOVE ��P�|�{�p��       TO ��(����ʒu�b�m�s)
005360                  MOVE "/"                TO �X���b�V��(����ʒu�b�m�s)
005370                  MOVE ��P�|�{�p��       TO ��(����ʒu�b�m�s)
005380                  MOVE ��P�|���ʌv(1)    TO ��ÂP(����ʒu�b�m�s)
005390                  MOVE ��P�|���ʌv(2)    TO ��ÂQ(����ʒu�b�m�s)
005400                  MOVE ��P�|���ʌv(3)    TO ��ÂR(����ʒu�b�m�s)
005410                  MOVE ��P�|���ʌv(4)    TO ��ÂS(����ʒu�b�m�s)
005420                  MOVE ��P�|���ʌv(5)    TO ��ÂT(����ʒu�b�m�s)
005430                  MOVE ��P�|㪖@���z     TO ��㪉��(����ʒu�b�m�s)
005440                  MOVE ��P�|�d�Ë��z     TO �d��(����ʒu�b�m�s)
005450                  MOVE ��P�|�ꕔ���S��   TO �ꕔ���S��(����ʒu�b�m�s)
005440                  MOVE ��P�|������       TO �������q(����ʒu�b�m�s)
                        IF ��P�|������ NOT = ZERO
                           MOVE NC"��"          TO ��(����ʒu�b�m�s)
                        END-IF
005440                  MOVE ��P�|�^����       TO �^�����(����ʒu�b�m�s)
                        IF ��P�|�^���� NOT = ZERO
                           MOVE NC"�^"          TO �^(����ʒu�b�m�s)
                        END-IF
005450               END-IF
005340               IF �R�����g���[�h�v�q = 1
005450                  MOVE ��P�|�R�����g     TO �R�����g�v
                        MOVE �R�����g�P�v       TO �R�����g�P(����ʒu�b�m�s)
                        MOVE �R�����g�Q�v       TO �R�����g�Q(����ʒu�b�m�s)
005450               END-IF
005460               COMPUTE ����ʒu�b�m�s = ����ʒu�b�m�s + 1
005340               IF (�������׃��[�h�v�q = 1) AND (��P�|���� = 1)
                        PERFORM �������׃Z�b�g
005450               END-IF
                     MOVE 0             TO �����t���O
007950               PERFORM ��ƃt�@�C���P�Ǎ�
                  END-PERFORM
      */���r���ύX���������ꍇ�I��
                  IF (��P�|���҃R�[�h = ���҃R�[�h�v�q) AND
                     ((��P�|�{�p�a�� NOT = �{�p�a��v�q) OR
                      (��P�|�{�p�N   NOT = �{�p�N�v�q) OR
                      (��P�|�{�p��   NOT = �{�p���v�q))
                     MOVE ���Ҕԍ��v�q       TO ��R�|���Ҕԍ�
                     MOVE ��P�|�{�p�a��     TO ��R�|�{�p�a��
                     MOVE ��P�|�{�p�N       TO ��R�|�{�p�N
                     MOVE ��P�|�{�p��       TO ��R�|�{�p��
008420               READ ��ƃt�@�C���R
                     NOT INVALID KEY
                        IF (��P�|�}�� = SPACE) AND
                           ((��R�|�}�Ԃe = 1) OR (��R�|�ی��ύX�e = 1))
                           STRING "�ی��ؓ��e���ύX����Ă��܂��B"   DELIMITED BY SIZE
                                  ���s�v                             DELIMITED BY SIZE
                                  "�m�F���Ă��������B"               DELIMITED BY SIZE
                                  ���s�v                             DELIMITED BY SIZE
                                  "�ύX�̑O���܂ň�����Ē��f���܂��B"  DELIMITED BY SIZE
                             INTO ���b�Z�[�W�v
                           END-STRING
005830                     MOVE SPACE  TO  �A�o�b���b�Z�[�W���
                           MOVE ���b�Z�[�W�v     TO �A�o�b���|���b�Z�[�W���e
004793                     MOVE "Pmsg001.DLL"    TO dll-name
004794                     MOVE "Pmsg001"        TO form-name
004795                     CALL "POWEROPENSHEET" USING dll-name form-name
005830                     MOVE SPACE            TO  ���b�Z�[�W�v
                           MOVE "YES"            TO �I���t���O
005430                     IF �����v���[�h�v�q = 1
                              MOVE �{�p�a��v�q  TO �I���a��v
                              MOVE �{�p�N�v�q    TO �I���N�v
                              MOVE �{�p���v�q    TO �I�����v
                              PERFORM �����v�Z�b�g�Q
005450                     END-IF
005810                     PERFORM �������
005820                     PERFORM ���ŏ���
                           PERFORM ����s���ޔ�
005932                     PERFORM �I������
005933                     PERFORM �x������
005934                     EXIT PROGRAM
                        END-IF
                     END-READ
                     MOVE ��P�|�{�p�a��      TO �{�p�a��v�q
                     MOVE ��P�|�{�p�N        TO �{�p�N�v�q
                     MOVE ��P�|�{�p��        TO �{�p���v�q
                  END-IF
007960         END-PERFORM
007970     END-IF.
           IF �}�Ԃv�q NOT = SPACE
003970         MOVE  NC"����̑������Ȃ����m�F���ĉ������B" TO �A���|���b�Z�[�W
013150         CALL   "MSG001"
013160         CANCEL "MSG001"
           END-IF.
007980*================================================================*
007990 �w�b�_�Z�b�g SECTION.
008000*
005230     MOVE ��|���Ҏ���         TO ���Ҏ���.
004900*================================================================*
004910 �������׃Z�b�g SECTION.
004920*
004860     MOVE ��P�|������        TO  ������.
           MOVE ��P�|���k��        TO  ���k��.
           IF ��P�|���k�� NOT = ZERO
               MOVE "���������k�x����" TO  ���k���s
               MOVE "�~"               TO  ���k���~
           END-IF.
           IF ��P�|���ԊO = 1
004870         MOVE NC"��"          TO  ���ԊO�`�F�b�N
           END-IF.
           IF ��P�|�x�� = 1
004880         MOVE NC"��"          TO  �x���`�F�b�N
           END-IF.
           IF ��P�|�[�� = 1
004890         MOVE NC"��"          TO  �[��`�F�b�N
           END-IF.
004900     MOVE ��P�|�������Z��    TO  �������Z.
004910     MOVE ��P�|�Č���        TO  �Č���.
           MOVE ��P�|�{�×��`�F�b�N    TO  �{�×��`�F�b�N.
004890     MOVE ��P�|�������`�F�b�N    TO  �������`�F�b�N.
004890     MOVE ��P�|�Œ藿�`�F�b�N    TO  �Œ藿�`�F�b�N.
004950     MOVE ��P�|���񏈒u��(1) TO  ���񏈒u��(1).
004960     MOVE ��P�|���񏈒u��(2) TO  ���񏈒u��(2).
004970     MOVE ��P�|���񏈒u��(3) TO  ���񏈒u��(3).
004980     MOVE ��P�|���񏈒u��(4) TO  ���񏈒u��(4).
004990     MOVE ��P�|���񏈒u��(5) TO  ���񏈒u��(5).
005000     MOVE ��P�|����          TO  ���Ë���.
005010     MOVE ��P�|��(1)         TO  ���Ì��P.
005020     MOVE ��P�|��(1)       TO  ���É񐔂P.
005030     MOVE ��P�|���×����z(1) TO  ���×��P.
           IF ��P�|��� = 1
004890         MOVE NC"��"          TO  ��ԃ`�F�b�N
           END-IF.
           IF ��P�|�\�� = 1
004890         MOVE NC"��"          TO  �\���`�F�b�N
           END-IF.
005060     MOVE ��P�|���É��Z��    TO  ���É��Z.
005070     MOVE ��P�|���񋟗�    TO  ���񋟗�.
           IF �{�p�a��N���v�q < 43006
               IF ��P�|������ = 1
004890             MOVE NC"��"          TO  ��`�F�b�N
               END-IF
               IF ��P�|������ = 1
004890             MOVE NC"��"          TO  ���`�F�b�N
               END-IF
               IF ��P�|������ = 1
004890             MOVE NC"��"          TO  ���`�F�b�N
               END-IF
005110         MOVE ��P�|���z          TO  ������
           END-IF.
007980*================================================================*
007990 �����v�Z�b�g SECTION.
008000*
           MOVE �{�p�a��N���v     TO ��Q�|�{�p�a��N��.
           MOVE ���҃R�[�h�v�q     TO ��Q�|���҃R�[�h.
           START ��ƃt�@�C���Q KEY IS >= ��Q�|���҃R�[�h
                                          ��Q�|�{�p�a��N��
           END-START.
005620     IF ��ԃL�[ = "00"
               MOVE SPACE  TO �I���t���O�Q
               MOVE SPACE  TO �E�o�t���O
005630         PERFORM ��ƃt�@�C���Q�Ǎ�
005640         PERFORM UNTIL (�I���t���O�Q = "YES") OR (�E�o�t���O = "YES") OR
                             (��Q�|���҃R�[�h NOT = ���҃R�[�h�v�q) OR
                             ((��Q�|�{�p�a��N�� = �I���a��N���v))
                   IF �i���v�q < 4
                       PERFORM �������Ԏ擾
                       PERFORM �����v�f�[�^�Z�b�g
                   ELSE
                       MOVE ��Q�|�{�p�a��N�� TO �{�p�a��N���v
                       MOVE "YES"  TO �E�o�t���O
                   END-IF
005630             PERFORM ��ƃt�@�C���Q�Ǎ�
               END-PERFORM
               IF �E�o�t���O = SPACE
                   MOVE ��Q�|�{�p�a��N�� TO �{�p�a��N���v
               END-IF
005420     END-IF.
007980*================================================================*
007990 �����v�Z�b�g�Q SECTION.
008000*
           MOVE �{�p�a��N���v     TO ��Q�|�{�p�a��N��.
           MOVE ���҃R�[�h�v�q     TO ��Q�|���҃R�[�h.
           START ��ƃt�@�C���Q KEY IS >= ��Q�|���҃R�[�h
                                          ��Q�|�{�p�a��N��
           END-START.
005620     IF ��ԃL�[ = "00"
               MOVE SPACE  TO �I���t���O�Q
005630         PERFORM ��ƃt�@�C���Q�Ǎ�
005640         PERFORM UNTIL (�I���t���O�Q = "YES") OR
                             (��Q�|���҃R�[�h NOT = ���҃R�[�h�v�q) OR
                             ((�I���t���O = SPACE) AND (��Q�|�{�p�a��N�� = �I���a��N���v)) OR
                             ((�I���t���O = "YES") AND (��Q�|�{�p�a��N�� > �I���a��N���v))
                   IF �i���v�q > 3
                       PERFORM �������
005820                 PERFORM ���ŏ���
005510                 MOVE 1      TO �i���v�q
005520                 MOVE SPACE  TO  �A���|�L�[
005530                 MOVE NC"�V�����p�����Z�b�g���Ă�������" TO �A���|���b�Z�[�W
005540                 CALL   "MSG001"
005550                 CANCEL "MSG001"
005900                 IF �������[�h�v�q = 1
                           PERFORM �w�b�_�Z�b�g
005930                 END-IF
                   END-IF
                   PERFORM �������Ԏ擾
                   PERFORM �����v�f�[�^�Z�b�g
005630             PERFORM ��ƃt�@�C���Q�Ǎ�
               END-PERFORM
005420     END-IF.
007980*================================================================*
007990 �����v�f�[�^�Z�b�g SECTION.
008000*
021840     MOVE ��Q�|�{�p�N TO ���z�s�N       (�i���v�q).
021850     MOVE ��Q�|�{�p�� TO ���z�s��       (�i���v�q).
021860     MOVE NC"�N"       TO ���z�s�N�\��   (�i���v�q).
021880     MOVE ��Q�|�{�p�N TO ��o�N         (�i���v�q).
021890     MOVE ��Q�|�{�p�� TO ��o��         (�i���v�q).
021900     MOVE �����I�����v TO ��o��         (�i���v�q).
021910     MOVE ��Q�|�{�p�N TO �����J�n�N     (�i���v�q)
021920                          �����I���N     (�i���v�q).
021930     MOVE ��Q�|�{�p�� TO �����J�n��     (�i���v�q)
021940                          �����I����     (�i���v�q).
021950     MOVE 1            TO �����J�n��     (�i���v�q).
021960     MOVE �����I�����v TO �����I����     (�i���v�q).
021870     MOVE ��Q�|������ TO ������         (�i���v�q).
021970     MOVE ��Q�|��p�z TO ��p�z         (�i���v�q).
021970     MOVE ��Q�|���S�z TO ���S�z         (�i���v�q).
021970     MOVE ��Q�|�����z TO �����z         (�i���v�q).
           COMPUTE �i���v�q = �i���v�q + 1.
008140*
008390*================================================================*
008400 ��ƃt�@�C���P�Ǎ� SECTION.
008410*
008420     READ ��ƃt�@�C���P NEXT
008430     AT END
008440         MOVE "YES" TO �I���t���O
008450     END-READ.
008460*
008390*================================================================*
008400 ��ƃt�@�C���Q�Ǎ� SECTION.
008410*
008420     READ ��ƃt�@�C���Q NEXT
008430     AT END
008440         MOVE "YES" TO �I���t���O�Q
008450     END-READ.
008460*
008470*================================================================*
008480 ������� SECTION.
008490*
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
007220*
006420     MOVE "YCH715P"  TO  ��`�̖��o.
006430     MOVE "GRP001"   TO  ���ڌQ���o.
006440     MOVE SPACE      TO  ������ʂo.
006450     MOVE SPACE      TO  �g������o.
006460     WRITE YCH715P.
006470     PERFORM �G���[�����o.
006500*
006510     MOVE "YCH715P"  TO  ��`�̖��o.
006520     MOVE "GRP002"   TO  ���ڌQ���o.
006530     MOVE SPACE      TO  ������ʂo.
006540     MOVE SPACE      TO  �g������o.
006550     WRITE YCH715P.
006560     PERFORM �G���[�����o.
      *
008560*================================================================*
008570 ���ŏ���  SECTION.
008580
006600     MOVE "YCH715P" TO  ��`�̖��o.
006610     MOVE "CT"      TO  ������ʂo.
006620     MOVE "PAGE"    TO  �g������o.
006630     MOVE SPACE     TO  ���ڌQ���o.
006640     WRITE YCH715P.
006650     PERFORM �G���[�����o.
006660     MOVE SPACE     TO  �g������o.
005190     PERFORM ���ڏ�����.
008660*
008670     CLOSE  ����t�@�C��.
004310     IF ( �I�[�v���t���O = "YES" )
004320        MOVE SPACE TO �I�[�v���t���O
004350     END-IF.
008680*     OPEN I-O   ����t�@�C��.
008690*     PERFORM �G���[�����o.
008700*
008940*================================================================*
008950 �G���[�����o SECTION.
008960*
008970     IF �ʒm���o NOT = "00"
008980         DISPLAY NC"���[�G���["              UPON CONS
008990         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
009000         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
009010         DISPLAY NC"�g������o�F" �g������o UPON CONS
009020         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
009030                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
009040         ACCEPT  �L�[���� FROM CONS
009050         PERFORM �t�@�C����
009060         MOVE 99 TO PROGRAM-STATUS
009070         EXIT PROGRAM
009080     END-IF.
009090*================================================================*
009100 �t�@�C���� SECTION.
009110*
002990     IF ( �I�[�v���t���O = "YES" )
002991         CLOSE ����t�@�C��
003041     END-IF.
009130     CLOSE ������}�X�^   ��f�ҏ��e     �����p���҂e.
009150*================================================================*
009160 �I������ SECTION.
009170*
009180     PERFORM �t�@�C����.
009190*================================================================*
009200 ����s���ޔ� SECTION.
009210*
009220     CLOSE ��f�ҏ��e.
009230     PERFORM �x������.
009240     OPEN I-O ��f�ҏ��e.
009250         MOVE NC"��f" TO �t�@�C����.
009260         PERFORM �I�[�v���`�F�b�N.
009270     PERFORM �x������.
009280*
009290     MOVE ZERO            TO ��|�{�p�a��.
009300     MOVE ZERO            TO ��|�{�p�N.
009310     MOVE ZERO            TO ��|�{�p��.
009320     MOVE ���Ҕԍ��v�q    TO ��|���Ҕԍ�.
009330     MOVE �}�Ԃv�q        TO ��|�}��.
009340*
009350     READ ��f�ҏ��e
009360     NOT INVALID KEY
009370         MOVE ����ʒu�b�m�s TO ��|�J���e������s��
009380         REWRITE ��|���R�[�h
009390         INVALID KEY
009400             MOVE NC"��e" TO �t�@�C����
009410             PERFORM �G���[�\��
009420         END-REWRITE
009430     END-READ.
009440     PERFORM �x������.
009450     CLOSE ��f�ҏ��e.
009460     PERFORM �x������.
009470     OPEN INPUT ��f�ҏ��e.
009480         MOVE NC"��f" TO �t�@�C����.
009490         PERFORM �I�[�v���`�F�b�N.
009500*
007980*================================================================*
007990 �������R���Z�b�g SECTION.
008000*
           PERFORM �������R���擾.
           PERFORM VARYING �J�E���^ FROM 1 BY 1
                   UNTIL   �J�E���^ > 11
               MOVE �������R���v(�J�E���^) TO �������R��(�J�E���^)
           END-PERFORM.
007290*================================================================*
035270 �������R���擾 SECTION.
003034     MOVE  ZERO   TO  �J�E���^.
003034     MOVE  ZERO   TO  �����s���v.
035280*
004502     MOVE �{�p�a��v�q   TO ���p�|�{�p�a��.
004503     MOVE �{�p�N�v�q     TO ���p�|�{�p�N.
004504     MOVE �{�p���v�q     TO ���p�|�{�p��.
004505     MOVE ���Ҕԍ��v�q   TO ���p�|���Ҕԍ�.
004506     MOVE �}�Ԃv�q       TO ���p�|�}��.
004507*
004508     READ �����p���҂e 
004509     INVALID KEY
                CONTINUE
004522     NOT INVALID KEY
004523         IF  (���p�|���R��(1)  = SPACE )  AND
004524             (���p�|���R��(2)  = SPACE )  AND
004525             (���p�|���R��(3)  = SPACE )  AND
004526             (���p�|���R��(4)  = SPACE )  AND
004527             (���p�|���R��(5)  = SPACE )
                   CONTINUE
004535         ELSE
                   PERFORM ���R���Z�b�g
004537         END-IF
004538     END-READ.
035430*================================================================*
004541 ���R���Z�b�g SECTION.
004542*
004543     PERFORM VARYING �ԍ��J�E���^ FROM 1 BY 1
004544             UNTIL   �ԍ��J�E���^  > 5 
004548*
004560         IF  ���p�|���R��(�ԍ��J�E���^) NOT = SPACE 
004561             PERFORM �ԍ��]�L
004595* ���R��
004598             MOVE ���p�|���R��(�ԍ��J�E���^)  TO  ���R���v
                   PERFORM ���R�������S�O��
004631         END-IF
004632**
004637     END-PERFORM.
004647*
005590*================================================================*
005600 �ԍ��]�L SECTION.
005601*
005605      EVALUATE �ԍ��J�E���^
005606      WHEN  1
005607          MOVE "�@"  TO �����ԍ��v
005608      WHEN  2
005609          MOVE "�A"  TO �����ԍ��v
005610      WHEN  3
005611          MOVE "�B"  TO �����ԍ��v
005612      WHEN  4
005613          MOVE "�C"  TO �����ԍ��v
005614      WHEN  5
005615          MOVE "�D"  TO �����ԍ��v
005616      WHEN  OTHER
005617          CONTINUE
005618      END-EVALUATE.
005619*
005705*================================================================*
005706 ���R�������S�O�� SECTION.
005707**********
005708*** �S�O��
005709**********
005716* �����������R���𕪊�����B
005717     INITIALIZE ���R�S�O���v.
005718     MOVE  �������R���v  TO  ���R�S�O���v.
005945*���p�X�y�[�X��S�p�ɂ�����(189�޲Ă�蒷���ر�ɔ��p�󔒂������Ă���̂�)
005946     INSPECT ���R�S�O���v REPLACING ALL ���p�� BY �S�p��.
005719*
005720     IF  ���R�S�O���P�v  NOT = SPACE
005721         COMPUTE �J�E���^ = �J�E���^  +  1
005722         MOVE ���R�S�O���P�v  TO �������R���v(�J�E���^)
005724     END-IF.
005725     IF  ���R�S�O���Q�v  NOT = SPACE
005726         COMPUTE �J�E���^ = �J�E���^  +  1
005727         MOVE ���R�S�O���Q�v  TO �������R���v(�J�E���^)
005729     END-IF.
005730     IF  ���R�S�O���R�v  NOT = SPACE
005731         COMPUTE �J�E���^ = �J�E���^  +  1
005732         MOVE ���R�S�O���R�v  TO �������R���v(�J�E���^)
005733     END-IF.
           IF  ���R�S�O���S�v  NOT = SPACE
               COMPUTE �J�E���^ = �J�E���^  +  1
               MOVE ���R�S�O���S�v  TO �������R���v(�J�E���^)
           END-IF.
           IF  ���R�S�O���T�v  NOT = SPACE
               COMPUTE �J�E���^ = �J�E���^  +  1
               MOVE ���R�S�O���T�v  TO �������R���v(�J�E���^)
           END-IF.
005734*
021810*================================================================*
022380  �������Ԏ擾 SECTION.
022390*
022400     EVALUATE ��Q�|�{�p��
022410     WHEN 4
022420     WHEN 6
022430     WHEN 9
022440     WHEN 11
022450         MOVE 30 TO �����I�����v
022460     WHEN 2
022470         COMPUTE �Ώې���v = �a��I���N�v + ��Q�|�{�p�N
022480         DIVIDE 4 INTO �Ώې���v GIVING    ���v
022490                                  REMAINDER �]�v
022500         END-DIVIDE
022510         IF �]�v = ZERO
022520             MOVE 29 TO �����I�����v
022530         ELSE
022540             MOVE 28 TO �����I�����v
022550         END-IF
022560     WHEN 1
022570     WHEN 3
022580     WHEN 5
022590     WHEN 7
022600     WHEN 8
022610     WHEN 10
022620     WHEN 12
022630         MOVE 31 TO �����I�����v
022640     WHEN OTHER
022650         MOVE  NC"���m�̃G���[����" TO �A���|���b�Z�[�W
022660         CALL   "MSG001"
022670         CANCEL "MSG001"
022680     END-EVALUATE.
009850*================================================================*
009860 �G���[�\�� SECTION.
009870*
009880     DISPLAY �t�@�C���� NC"�t�@�C�������G���["     UPON CONS.
009890     DISPLAY NC"��ԃL�[�F" ��ԃL�[               UPON CONS.
009900     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
009910     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
009920     ACCEPT  �L�[���� FROM CONS.
009930     PERFORM �t�@�C����.
009940     EXIT PROGRAM.
009950*================================================================*
009960******************************************************************
009970 END PROGRAM YCH7132.
009980******************************************************************
