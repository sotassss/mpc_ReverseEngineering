000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJB101.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*      ��oFPD�쐬�y�ް��쐬�z�_����޳��95��
000100*
000110* �� �����N��Ver�̂�. (���я��́A�����\581�Ɠ����j
000120*�@�@��ʂ𑍊��\�ƈꏏ�ɂ���
      */��˗����Z�܂Ƃ߂̏ꍇ�͔�p�P�O���E���S�O�~�E�����O�~�B�Ƃ���B*/20170603
      */�������q�A�^����ÁA���׏����s��ύX�ǉ�/220810
      *
000130*      MED = YJB100G 
000140*----------------------------------------------------------------*
000150 DATE-WRITTEN.           2010-04-08
000160 DATE-COMPILED.          2010-04-08
000170*----------------------------------------------------------------*
000180******************************************************************
000190*            ENVIRONMENT         DIVISION                        *
000200******************************************************************
000210 ENVIRONMENT             DIVISION.
000220 CONFIGURATION           SECTION.
000230 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000240 OBJECT-COMPUTER.        FMV-DESKPOWER.
000250 SPECIAL-NAMES.          CONSOLE  IS  CONS
000260                         SYSERR   IS  MSGBOX.
000270 INPUT-OUTPUT            SECTION.
000280 FILE-CONTROL.
000281     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000282                             ORGANIZATION             IS  INDEXED
000283                             ACCESS MODE              IS  DYNAMIC
000284                             RECORD KEY               IS  ���|����敪
000285                             FILE STATUS              IS  ��ԃL�[
000286                             LOCK        MODE         IS  AUTOMATIC.
000290     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000300                             ORGANIZATION             IS  INDEXED
000310                             ACCESS MODE              IS  DYNAMIC
000320                             RECORD KEY               IS  ���|�����敪
000330                             FILE STATUS              IS  ��ԃL�[
000340                             LOCK        MODE         IS  AUTOMATIC.
000350     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000360                             ORGANIZATION             IS  INDEXED
000370                             ACCESS MODE              IS  DYNAMIC
000380                             RECORD KEY               IS  ���|�敪�R�[�h
000390                                                          ���|���̃R�[�h
000400                             FILE STATUS              IS  ��ԃL�[
000410                             LOCK        MODE         IS  AUTOMATIC.
000420     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS �{��|�{�p���ԍ�
000460                             FILE STATUS              IS  ��ԃL�[
000470                             LOCK        MODE         IS  AUTOMATIC.
000480     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
000490                             ORGANIZATION             IS  INDEXED
000500                             ACCESS MODE              IS  DYNAMIC
000510                             RECORD KEY           IS �{�L�|�{�p�a��N����
000520                                                     �{�L�|���҃R�[�h
000530                             ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
000540                                                     �{�L�|�{�p�a��N����
000550                             FILE STATUS              IS  ��ԃL�[
000560                             LOCK        MODE         IS  AUTOMATIC.
000260     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  ��|�{�p�a��N��
000300                                                          ��|���҃R�[�h
000310                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000320                                                          ��|���҃J�i
000330                                                          ��|���҃R�[�h
000340                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000350                                                          ��|�{�p�a��N��
000360                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000370                                                          ��|�ی����
000380                                                          ��|�ی��Ҕԍ�
000390                                                          ��|���҃R�[�h
000400                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000410                                                          ��|������
000420                                                          ��|��p���S�Ҕԍ�
000430                                                          ��|���҃R�[�h
000440                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000450                                                          ��|�������
000460                                                          ��|��p���S�Ҕԍ�����
000470                                                          ��|���҃R�[�h
000480                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
000490                                                          ��|�{�p�a��N��
000500                                                          ��|���҃R�[�h
000510                             FILE STATUS              IS  ��ԃL�[
000520                             LOCK        MODE         IS  AUTOMATIC.
000840     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS ���|�{�p�a��N��
000880                                                         ���|���҃R�[�h
000890                             ALTERNATE RECORD KEY     IS ���|���҃R�[�h
000900                                                         ���|�{�p�a��N��
000910                             FILE STATUS              IS  ��ԃL�[
000920                             LOCK        MODE         IS  AUTOMATIC.
000921     SELECT  ���������e      ASSIGN      TO        HUGEINL
000922                             ORGANIZATION             IS  INDEXED
000923                             ACCESS MODE              IS  DYNAMIC
000924                             RECORD KEY               IS  �����|�敪�R�[�h
000925                                                          �����|���������R�[�h
000926                             FILE STATUS              IS  ��ԃL�[
000927                             LOCK        MODE         IS  AUTOMATIC.
000930     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000940                             ORGANIZATION             IS  INDEXED
000950                             ACCESS MODE              IS  DYNAMIC
000960                             RECORD KEY               IS  �s�|������
000970                                                          �s�|�s�����ԍ�
000980                             ALTERNATE RECORD KEY     IS  �s�|������
000990                                                          �s�|�s��������
001000                                                          �s�|�s�����ԍ�
001010                             FILE STATUS              IS  ��ԃL�[
001020                             LOCK        MODE         IS  AUTOMATIC.
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
000611     SELECT  ��v�f�[�^�e    ASSIGN      TO        KAIKEIL
000612                             ORGANIZATION             IS  INDEXED
000613                             ACCESS MODE              IS  DYNAMIC
000089                             RECORD KEY               IS  ��|�{�p�a��N����
000090                                                          ��|���҃R�[�h
000092                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000093                                                          ��|�{�p�a��N����
000621                             FILE STATUS              IS  ��ԃL�[
000622                             LOCK        MODE         IS  AUTOMATIC.
001090     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001100                             ORGANIZATION             IS  SEQUENTIAL
001110                             ACCESS                   IS  SEQUENTIAL
001120                             FILE        STATUS       IS  ��ԃL�[
001130                             LOCK        MODE         IS  AUTOMATIC.
001140*
001210*  �����\�Ɠ����ی��Ҕԍ���̧��
001211     SELECT  ��ƃt�@�C���R  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5803L.DAT"
001212                             ORGANIZATION             IS  INDEXED
001213                             ACCESS                   IS  DYNAMIC
001214                             RECORD      KEY          IS  ��R�|�����a��N��
001215                                                          ��R�|�����敪
001216                                                          ��R�|�ی��Ҕԍ�
001217                                                          ��R�|�{�l�Ƒ��敪
001218                                                          ��R�|�{�p�a��N��
001219                                                          ��R�|��ی��҃J�i
001220                                                          ��R�|���҃R�[�h
                                                                ��R�|�e�q�敪
001221                             FILE        STATUS       IS  ��ԃL�[
001222                             LOCK        MODE         IS  AUTOMATIC.
001223*
001224******************************************************************
001230*                      DATA DIVISION                             *
001240******************************************************************
001250 DATA                    DIVISION.
001260 FILE                    SECTION.
001261*                           �m�q�k��  �Q�T�U�n
001262 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001263     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001270*                           �m�q�k��  �P�Q�W�n
001280 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001290     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001300*                           �m�q�k��  �P�Q�W�n
001310 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001320     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001330*
001340 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
001350     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001360*                           �m�q�k��  �Q�T�U�n
001370 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
001380     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
001390*                           �m�q�k��  �R�Q�O�n
001400 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001410     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001420*                           �m�q�k��  �P�Q�W�n
001430 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001440     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001441*                           �m�q�k��  �P�Q�W�n
001442 FD  ���������e         BLOCK   CONTAINS   1   RECORDS.
001443     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
001450*                           �m�q�k��  �Q�T�U�n
001460 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001470     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
000981*                           �m�q�k��  �T�P�Q�n
000982 FD  ��v�f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
000983     COPY KAIKEI          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001510**
001520 FD  ��ƃt�@�C���P RECORD  CONTAINS 896 CHARACTERS.
001530 01  ��P�|���R�[�h.
001531*   / �w�b�_���͎g�p���Ȃ� /
001540     03  ��P�|���R�[�h�w�b�_.
001550         05  ��P�|�����a��N���L�[.
001560             07  ��P�|�����a��            PIC 9.
001570             07  ��P�|�����N              PIC 9(2).
001580             07  ��P�|������              PIC 9(2).
001590         05  ��P�|�{�p�a��N���L�[.
001600             07  ��P�|�{�p�a��            PIC 9.
001610             07  ��P�|�{�p�N              PIC 9(2).
001620             07  ��P�|�{�p��              PIC 9(2).
001630         05  ��P�|�ی��敪�L�[            PIC 9.
001640         05  ��P�|�ی��Ҕԍ��L�[          PIC 9(8).
001650         05  ��P�|�{�l�Ƒ��敪�L�[        PIC 9.
001660         05  ��P�|��ی��҃J�i�L�[        PIC X(20).
001670         05  ��P�|���҃R�[�h�L�[.
001680             07 ��P�|���Ҕԍ�             PIC 9(6).
001690             07 ��P�|�}��                 PIC X(1).
001700     03  ��P�|���R�[�h�f�[�^.
001710         05  ��P�|����ԍ�                PIC 9(4).
001720         05  ��P�|��z���󗝔ԍ��敪      PIC 9.
001730         05  ��P�|�����N��                PIC 9(6).
001740         05  ��P�|���Z�v�g�ԍ�            PIC 9(4).
001750         05  ��P�|�{�p�N��                PIC 9(6).
001760         05  ��P�|�Y���ی����            PIC 9.
001770         05  ��P�|�ی��Ҕԍ�              PIC 9(8).
001780         05  ��P�|�L��                    PIC X(20).
001790         05  ��P�|�ԍ�                    PIC X(16).
001800         05  ��P�|�V�l�s�����ԍ�          PIC 9(8).
001810         05  ��P�|�V�l�󋋎Ҕԍ�          PIC 9(8).
001820         05  ��P�|�������S�Ҕԍ�          PIC 9(8).
001830         05  ��P�|�����󋋎Ҕԍ�          PIC X(16).
001840         05  ��P�|��ی��Ҏ���            PIC N(10).
001850         05  ��P�|��ی��҃J�i            PIC X(16).
001860         05  ��P�|���Ҏ���                PIC N(10).
001870         05  ��P�|���҃J�i                PIC X(16).
001880         05  ��P�|�{�l�Ƒ��敪            PIC 9.
001890         05  ��P�|���Ґ���                PIC 9.
001900         05  ��P�|���Ґ��N����            PIC 9(8).
001910         05  ��P�|��ی��ҏZ��            PIC X(28).
001920         05  ��P�|���v���z                PIC 9(6).
001930         05  ��P�|�����p�ی����t����      PIC 9(2).
001940         05  ��P�|�ꕔ���S��              PIC 9(6).
001950         05  ��P�|�������z                PIC 9(6).
001960         05  ��P�|���ʐ�                  PIC 9.
001970         05  ��P�|��������                PIC N(10).
001980         05  ��P�|�������R                PIC N(16).
001990         05  ��P�|�V�K�敪                PIC 9.
002000         05  ��P�|�p���敪                PIC 9.
002010         05  ��P�|������                PIC 9.
002020         05  ��P�|�������ԊO���Z��      PIC 9.
002030         05  ��P�|�����x�����Z��        PIC 9.
002040         05  ��P�|�����[����Z��        PIC 9.
002050         05  ��P�|�Č���                PIC 9.
002060         05  ��P�|���Ë���                PIC 9(3).
002070         05  ��P�|���É�                PIC 9(2).
002080         05  ��P�|��ԉ��Z���É�        PIC 9.
002090         05  ��P�|��H���Z���É�        PIC 9(2).
002100         05  ��P�|�\���J����Z���É�    PIC 9(2).
002110         05  ��P�|�������q��            PIC 9.
002120         05  ��P�|�_�~�[                  PIC X.
002130         05  ��P�|�^����É�            PIC 9.
002140         05  ��P�|���񋟗���          PIC 9.
002150*
002160         05  ��P�|�������ʃf�[�^  OCCURS 6.
002170             07  ��P�|�����敪            PIC 9.
002180             07  ��P�|������              PIC N(16).
002190             07  ��P�|���񏈒u��        PIC 9.
002200             07  ��P�|�����N����          PIC 9(8).
002210             07  ��P�|�����N����          PIC 9(8).
002220             07  ��P�|�{�p�J�n�N����      PIC 9(8).
002230             07  ��P�|�{�p�I���N����      PIC 9(8).
002240             07  ��P�|������              PIC 9(2).
002250             07  ��P�|�]�A�敪            PIC 9.
002260             07  ��P�|��É�            PIC 9(2).
002270             07  ��P�|��㪖@��          PIC 9.
002280             07  ��P�|��㪖@��          PIC 9(2).
002290             07  ��P�|�d�É�            PIC 9(2).
002300             07  ��P�|�����ʒ����敪      PIC 9.
002310             07  ��P�|���������敪        PIC 9.
      */�ǉ�110125
               05  ��P�|�Ǝҋ敪                PIC 9(2).
               05  ��P�|���k�x����            PIC 9.
               05  ��P�|�{�p��                  PIC X(31).
      */�ǉ�220609
               05  ��P�|���ה��s���Z��        PIC 9(1).
               05  ��P�|���ה��s�̐���          PIC 9(3).
               05  ��P�|���׏����s����          PIC 9(4).
002320         05  FILLER                        PIC X(29).
002330***
002331* �ی��Ҕԍ����t�@�C��
002332 FD  ��ƃt�@�C���R RECORD  CONTAINS 64 CHARACTERS.
002333 01  ��R�|���R�[�h.
002334     03  ��R�|���R�[�h�L�[.
002335         05  ��R�|�����a��N��.
002336             07  ��R�|�����a��            PIC 9.
002337             07  ��R�|�����N              PIC 9(2).
002338             07  ��R�|������              PIC 9(2).
002339         05  ��R�|�����敪                PIC 9.
002340         05  ��R�|�ی��Ҕԍ�              PIC 9(8).
002341         05  ��R�|�{�l�Ƒ��敪            PIC 9.
002342         05  ��R�|�{�p�a��N��.
002343             07  ��R�|�{�p�a��            PIC 9.
002344             07  ��R�|�{�p�N              PIC 9(2).
002345             07  ��R�|�{�p��              PIC 9(2).
002346         05  ��R�|��ی��҃J�i            PIC X(20).
002347         05  ��R�|���҃R�[�h.
002348             07 ��R�|���Ҕԍ�             PIC 9(6).
002349             07 ��R�|�}��                 PIC X(1).
002339         05  ��R�|�e�q�敪                PIC 9.
002350     03  ��R�|���R�[�h�f�[�^.
002351         05  FILLER                        PIC X(16).
002352*
002353*----------------------------------------------------------------*
002354******************************************************************
002360*                WORKING-STORAGE SECTION                         *
002370******************************************************************
002380 WORKING-STORAGE         SECTION.
002390 01 �L�[����                           PIC X    VALUE SPACE.
002400 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
002410 01 �����t���O                         PIC X(3) VALUE SPACE.
002420 01 �I���t���O                         PIC X(3) VALUE SPACE.
002430 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
001270 01 �I���t���O�S                       PIC X(3) VALUE SPACE.
002440 01 ���s�L�[�v                         PIC X(3) VALUE SPACE.
002450 01 �{�p�L�^�L�v                       PIC X(3) VALUE SPACE.
002460 01 �t�@�C����                         PIC N(8) VALUE SPACE.
002470*
002480 01 �ی���ʂv�q                       PIC 9(2) VALUE ZERO.
002490 01 ���҃R�[�h�v�q.
002500    03 ���Ҕԍ��v�q                    PIC 9(6) VALUE ZERO.
002510    03 �}�Ԃv�q                        PIC X    VALUE SPACE.
002520*
002530 01 ����`���v�q                       PIC 9    VALUE ZERO.
002540 01 �ی��Ҕԍ��v�q                     PIC X(10) VALUE SPACE.
002550 01 ���Z�v�g��ނv�q                   PIC X(4) VALUE SPACE.
002560 01 �{�l�Ƒ��敪�v�q                   PIC 9    VALUE ZERO.
002570 01 �����v                             PIC N(2) VALUE SPACE.
002580 01 �{�p�a��N���v�q.
002590    03 �{�p�a��v�q                    PIC 9    VALUE ZERO.
002600    03 �{�p�N�v�q                      PIC 9(2) VALUE ZERO.
002610    03 �{�p���v�q                      PIC 9(2) VALUE ZERO.
002620 01 �����a��N���v�q.
002630    03 �����a��v�q                    PIC 9    VALUE ZERO.
002640    03 �����N�v�q                      PIC 9(2) VALUE ZERO.
002650    03 �������v�q                      PIC 9(2) VALUE ZERO.
002660**
002670 01 �A�Ԃv                             PIC 9(4) VALUE ZERO.
002680 01 �����t���O                         PIC X(3) VALUE SPACE.
002690 01 �������̂v                         PIC N(6)  VALUE SPACE.
002700 01 ������ʕϊ��O�v                   PIC 9(2)  VALUE ZERO.
002710 01 ������ʕϊ���v                   PIC 9     VALUE ZERO.
002720 01 �]�A�ϊ��O�v                       PIC 9     VALUE ZERO.
002730 01 �]�A�ϊ���v                       PIC 9     VALUE ZERO.
002740**
002750 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002760 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
002770 01 �J�E���^�Q                         PIC 9(3)  VALUE ZERO.
002780 01 �J�E���^�R                         PIC 9(2)  VALUE ZERO.
002790 01 �S�p��                           PIC X(2)  VALUE X"8140".
002800 01 ���p��                           PIC X(2)  VALUE X"2020".
002810**
       01 �����v.
          03 �S�p�����v                      PIC N(10) VALUE SPACE.
002820** �G���[���b�Z�[�W�p
002830 01 �G���[���b�Z�[�W�v.
002840    03 �G���[���҃R�[�h�v              PIC X(7) VALUE SPACE.
002850    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
002860    03 �G���[�ی���ʂv                PIC X(2) VALUE SPACE.
002870    03 FILLER                          PIC X(10) VALUE SPACE.
002880** �ی��Ҕԍ��E�l�ߗp
002890 01 �ی��Ҕԍ��v�s.
002900    03 �ی��Ҕԍ����l�߂v.
002910      05 �ی��Ҕԍ����l�߂v�P          PIC X OCCURS 8 VALUE SPACE.
002920    03 �ی��Ҕԍ��E�l�߂v.
002930      05 �ی��Ҕԍ��E�l�߂v�P          PIC X OCCURS 8 VALUE ZERO.
002940    03 �ی��Ҕԍ������v                PIC 9(8)  VALUE ZERO.
002950    03 �ی��Ҕԍ��v                    PIC X(8)  VALUE SPACE.
002960** ����ԍ��E�l�ߗp
002970 01 ����ԍ��v�s.
002980    03 ����ԍ����l�߂v.
002990      05 ����ԍ����l�߂v�P            PIC X OCCURS 7 VALUE SPACE.
003000    03 ����ԍ��E�l�߂v.
003010      05 ����ԍ��E�l�߂v�P            PIC X OCCURS 7 VALUE ZERO.
003020    03 ����ԍ������v                  PIC 9(7)  VALUE ZERO.
003030    03 ����ԍ��v                      PIC X(7)  VALUE SPACE.
003040** ������t���[�N�p
003050 01 ����N���v.
003060    03 ����N�v                        PIC 9(4) VALUE ZERO.
003070    03 ����v                        PIC 9(2) VALUE ZERO.
003080** ������N���p
003090 01 ������N���v.
003100    03 ������N�v                    PIC 9(4) VALUE ZERO.
003110    03 ��������v                    PIC 9(2) VALUE ZERO.
003120** ����{�p�N���p
003130 01 ����{�p�N���v.
003140    03 ����{�p�N�v                    PIC 9(4) VALUE ZERO.
003150    03 ����{�p���v                    PIC 9(2) VALUE ZERO.
003160** �L�����l�ߗp
003170 01 �L���v�s.
003180    03 �L�����v.
003190      05 �L�����v�P                    PIC N OCCURS 12 VALUE SPACE.
003200    03 �L�����l�߂v.
003210      05 �L�����l�߂v�P                PIC N OCCURS 12 VALUE SPACE.
003180    03 �L�����w�v.
003190      05 �L�����w�v�P                  PIC X OCCURS 24 VALUE SPACE.
003200    03 �L�����l�߂w�v.
003210      05 �L�����l�߂w�v�P              PIC X OCCURS 24 VALUE SPACE.
003220    03 �L���v.
003230      05 �L���m�v                      PIC N(12) VALUE SPACE.
003240    03 �L���o�v.
003250      05 �L���o�m�v                    PIC X(24) VALUE SPACE.
003260** �������S�Ҕԍ����l�ߗp
003270 01 �����ԍ��v�s.
003280    03 �����ԍ����v.
003290      05 �����ԍ����v�P                PIC X OCCURS 10 VALUE SPACE.
003300    03 �����ԍ����l�߂v.
003310      05 �����ԍ����l�߂v�P            PIC X OCCURS 10 VALUE SPACE.
003320    03 �����ԍ��v                      PIC X(10) VALUE SPACE.
003330*
003340** ����N�������[�N�p
003350 01 �v�Z����N�����v.
003360    03 �v�Z����N�v                    PIC 9(4) VALUE ZERO.
003370    03 �v�Z����v                    PIC 9(2) VALUE ZERO.
003380    03 �v�Z������v                    PIC 9(2) VALUE ZERO.
003390 01 �v�Z�a��N�����v.
003400    03 �v�Z�a��v                      PIC 9 VALUE ZERO.
003410    03 �v�Z�N�v                        PIC 9(2) VALUE ZERO.
003420    03 �v�Z���v                        PIC 9(2) VALUE ZERO.
003430    03 �v�Z���v                        PIC 9(2) VALUE ZERO.
003440** �}�Ԕ���p
003450 01 �J�n�f�Ó��蓮�敪�v               PIC 9    VALUE ZERO.
003460*
003470* �I�����ޔ�p
003480 01 �I���N�����v�s.
003490    03 �I���a��v�s                    PIC 9     VALUE ZERO.
003500    03 �I���N�v�s                      PIC 9(2)  VALUE ZERO.
003510    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
003520    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
003530* �������ޔ�p
003540 01 �����N�����v�s.
003550    03 �����a��v�s                    PIC 9     VALUE ZERO.
003560    03 �����N�v�s                      PIC 9(2)  VALUE ZERO.
003570    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003580    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003590*
003591* �A�v�̋��z�ޔ�p
003592 01 �A�v���z�v.
003593    03  ��p�z�v                   PIC 9(6) VALUE ZERO.
003594    03  ���S�z�v                   PIC 9(6) VALUE ZERO.
003595    03  �����z�v                   PIC 9(6) VALUE ZERO.
003596    03  ��p�z�V�l�v               PIC 9(6) VALUE ZERO.
003597    03  ���S�z�V�l�v               PIC 9(6) VALUE ZERO.
003598    03  �����z�V�l�v               PIC 9(6) VALUE ZERO.
003599    03  ��p�z�����v               PIC 9(6) VALUE ZERO.
003600    03  ���S�z�����v               PIC 9(5) VALUE ZERO.
003601    03  �����z�����v               PIC 9(5) VALUE ZERO.
003602    03  ���S���v                   PIC 9(3) VALUE ZERO.
003603*
003604* ���������p
003605 01 ���������v�s.
003606    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
003607    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
003608    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
003609    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
003610    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
003611    03 ���������i���o�[�v�s.
003612       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
003613    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
003614 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
003615 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
003616 01 ���������s�a�k.
003617    03 ���������R�[�h�s�a�k            OCCURS 9.
003618       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
003619       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
003620       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
003621 01 �����������e�v.
003622    03 �����������e�����v              PIC X(318) OCCURS 9 VALUE SPACE.
003623    03 �����������e�����w�v.
003624       05 �����������e�P�w�v           PIC X(74)  VALUE SPACE.
003625       05 �����������e�Q�w�v           PIC X(74)  VALUE SPACE.
003626       05 �����������e�R�w�v           PIC X(74)  VALUE SPACE.
003650       05 �����������e�S�w�v           PIC X(96)  VALUE SPACE.
003627*
003628** ���������E�������R����敪�p
003629 01 ������������敪�v                 PIC 9 VALUE ZERO.
003630 01 �������R����敪�v                 PIC 9 VALUE ZERO.
003631*
003632** �������Z�܂Ƃߗp
003633 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
003634*
003635**********************************************************************************
003636*
003637 01 �ޔ����ڂf�v.
003638   03 ���Z�v�g��ނv                   PIC X(4).
003640   03 ���Z�v�g��ނf�v                 PIC X(4).
003650   03 ���Z�v�g��ʂf�v                 PIC 9(2).
003660*
003670****************
003680* �����f�[�^�e *
003690****************
003700 01 �������v.
003710    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
003720    03 ���ʏ��v  OCCURS   9.
003730       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
003740       05 ���ʃR�[�h�v.
003750          07 ������ʂv                PIC 9(2)  VALUE ZERO.
003760          07 ���ʂv                    PIC 9(2)  VALUE ZERO.
003770          07 ���E�敪�v                PIC 9(1)  VALUE ZERO.
003780          07 �����ʒu�ԍ��v            PIC 9(2)  VALUE ZERO.
003790       05 �������v                     PIC N(18) VALUE SPACE.
003800       05 �����N�����v.
003810          07 �����a��v                PIC 9     VALUE ZERO.
003820          07 �����N�v                  PIC 9(2)  VALUE ZERO.
003830          07 �������v                  PIC 9(2)  VALUE ZERO.
003840          07 �������v                  PIC 9(2)  VALUE ZERO.
003850       05 �����N�����v.
003860          07 �����a��v                PIC 9     VALUE ZERO.
003870          07 �����N�v                  PIC 9(2)  VALUE ZERO.
003880          07 �������v                  PIC 9(2)  VALUE ZERO.
003890          07 �������v                  PIC 9(2)  VALUE ZERO.
003900       05 �J�n�N�����v.
003910          07 �J�n�a��v                PIC 9     VALUE ZERO.
003920          07 �J�n�N�v                  PIC 9(2)  VALUE ZERO.
003930          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
003940          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
003950       05 �I���N�����v.
003960          07 �I���a��v                PIC 9     VALUE ZERO.
003970          07 �I���N�v                  PIC 9(2)  VALUE ZERO.
003980          07 �I�����v                  PIC 9(2)  VALUE ZERO.
003990          07 �I�����v                  PIC 9(2)  VALUE ZERO.
004000       05 �������v                     PIC 9(2)  VALUE ZERO.
004010       05 ���񏈒u�񐔂v               PIC 9     VALUE ZERO.
004020       05 �]�A�敪�v                   PIC 9(1)  VALUE ZERO.
004030    03 �V�K�敪�v                      PIC 9(1)  VALUE ZERO.
004040    03 �p���敪�v                      PIC 9(1)  VALUE ZERO.
          03 ���������v OCCURS 27.
004041       05 ���������v�o                 PIC X(74) VALUE SPACE.
004050*
004060*********************************************************************
004070*    ************
004080*    * ������� *
004090*    ************
004100*    �����̗���
004110***********************
004120 01 �����P�v�q.
004130   03 �����v�q.
004140      05 �����񐔂v                 PIC 9(1)    VALUE ZERO.
004150      05 �������ԊO�񐔂v           PIC 9(1)    VALUE ZERO.
004160      05 �����x���񐔂v             PIC 9(1)    VALUE ZERO.
004170      05 �����[��񐔂v             PIC 9(1)    VALUE ZERO.
004180   03 �Č��񐔂v                    PIC 9(1)    VALUE ZERO.
004190   03 ���Âv�q.
004200      05 ���É񐔂v                 PIC 9(2)    VALUE ZERO.
004210      05 ���Ë����v                 PIC 9(3)V9  VALUE ZERO.
004211      05 ���Ë����Q�v               PIC 9(3)    VALUE ZERO.
004220      05 ���Ö�Ԃv                 PIC 9(1)    VALUE ZERO.
004230      05 ���Ó�H�v                 PIC 9(2)    VALUE ZERO.
004240      05 ���Ö\���v                 PIC 9(2)    VALUE ZERO.
004250   03 �������q�񐔂v                PIC 9(1)    VALUE ZERO.
004260   03 �^����É񐔂v                PIC 9(1)    VALUE ZERO.
004280   03 ���񋟗��񐔂v              PIC 9(1)    VALUE ZERO.
004270   03 ���ה��s���Z�񐔂v            PIC 9(1)    VALUE ZERO.
004270   03 ���ה��s�̐����v              PIC 9(3)    VALUE ZERO.
004270   03 ���׏����s�����v              PIC 9(4)    VALUE ZERO.
004270   03 ���׌����v.
            05 ���׌��v                   PIC 9(2)    VALUE ZERO.
            05 ���ד��v                   PIC 9(2)    VALUE ZERO.
004290   03 �ꕔ���S���v�q                PIC 9(6)    VALUE ZERO.
004300   03 �������z�v�q                  PIC 9(6)    VALUE ZERO.
004310   03 ���t�����v�q                  PIC 9(1)    VALUE ZERO.
004320   03 �󋋎ҕ��S�z�v�q              PIC 9(6)    VALUE ZERO.
004330   03 �����������z�v�q              PIC 9(6)    VALUE ZERO.
      */
         03 ���k�x���񐔂v                PIC 9(1)    VALUE ZERO.
         03 �{�p���s�v.
            05 �{�p���v                   PIC 9(1) OCCURS 31 VALUE ZERO.
004340*
004350* �������ʖ��̗���
004360***********************
004370 01 �����Q�v�q.
004380   03 ���񏈒u�v�q    OCCURS   9.
004390      05 ���񏈒u���v�q             PIC 9(5)    VALUE ZERO.
004400   03 �����ʋ敪�v�q  OCCURS   9.
004410      05 �����ʋ敪�v               PIC 9(1)    VALUE ZERO.
004420   03 �����敪�v�q  OCCURS   9.
004430      05 �����敪�v                 PIC 9(1)    VALUE ZERO.
004440*
004450* �������̗���
004460***********************
004470 01 �����R�v�q.
004480**********
004490* �P���� *
004500**********
004510   03 ���ʂP�v�q.
004520      05 ��ÂP�v�q.
004530         07 ��É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
004540      05 ��㪖@�P�v�q.
004550         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
004560      05 ��㪖@�P�v�q.
004570         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
004580      05 �d�ÂP�v�q.
004590         07 �d�É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
004600**********
004610* �Q���� *
004620**********
004630   03 ���ʂQ�v�q.
004640      05 ��ÂQ�v�q.
004650         07 ��É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
004660      05 ��㪖@�Q�v�q.
004670         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
004680      05 ��㪖@�Q�v�q.
004690         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
004700      05 �d�ÂQ�v�q.
004710         07 �d�É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
004720******************
004730* �R���ʁ^�W�� *
004740******************
004750   03 ���ʂR�W�v�q.
004760      05 ��ÂR�W�v�q.
004770         07 ��É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
004780      05 ��㪖@�R�W�v�q.
004790         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
004800      05 ��㪖@�R�W�v�q.
004810         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
004820      05 �d�ÂR�W�v�q.
004830         07 �d�É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
004840******************
004850* �R���ʁ^�P�O�� *
004860******************
004870   03 ���ʂR�O�v�q.
004880      05 ��ÂR�O�v�q.
004890         07 ��É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
004900      05 ��㪖@�R�O�v�q.
004910         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
004920      05 ��㪖@�R�O�v�q.
004930         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
004940      05 �d�ÂR�O�v�q.
004950         07 �d�É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
004960******************
004970* �R���ʁ^���v�@ *
004980******************
004990   03 ���ʂR�v�q.
005000      05 ��ÂR�v�q.
005010         07 ��É񐔂R�v�q                PIC 9(2)  VALUE ZERO.
005020      05 ��㪖@�R�v�q.
005030         07 ��㪖@�񐔂R�v�q              PIC 9(2)  VALUE ZERO.
005040      05 ��㪖@�R�v�q.
005050         07 ��㪖@�񐔂R�v�q              PIC 9(2)  VALUE ZERO.
005060      05 �d�ÂR�v�q.
005070         07 �d�É񐔂R�v�q                PIC 9(2)  VALUE ZERO.
005080****************
005090* �S���ʁ^�T�� *
005100****************
005110   03 ���ʂS�T�v�q.
005120      05 ��ÂS�T�v�q.
005130         07 ��É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
005140      05 ��㪖@�S�T�v�q.
005150         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
005160      05 ��㪖@�S�T�v�q.
005170         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
005180      05 �d�ÂS�T�v�q.
005190         07 �d�É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
005200****************
005210* �S���ʁ^�W�� *
005220****************
005230   03 ���ʂS�W�v�q.
005240      05 ��ÂS�W�v�q.
005250         07 ��É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
005260      05 ��㪖@�S�W�v�q.
005270         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
005280      05 ��㪖@�S�W�v�q.
005290         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
005300      05 �d�ÂS�W�v�q.
005310         07 �d�É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
005320******************
005330* �S���ʁ^�P�O�� *
005340******************
005350   03 ���ʂS�O�v�q.
005360      05 ��ÂS�O�v�q.
005370         07 ��É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
005380      05 ��㪖@�S�O�v�q.
005390         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
005400      05 ��㪖@�S�O�v�q.
005410         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
005420      05 �d�ÂS�O�v�q.
005430         07 �d�É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
005440******************
005450* �S���ʁ^���v�@ *
005460******************
005470   03 ���ʂS�v�q.
005480      05 ��ÂS�v�q.
005490         07 ��É񐔂S�v�q                PIC 9(2)  VALUE ZERO.
005500      05 ��㪖@�S�v�q.
005510         07 ��㪖@�񐔂S�v�q              PIC 9(2)  VALUE ZERO.
005520      05 ��㪖@�S�v�q.
005530         07 ��㪖@�񐔂S�v�q              PIC 9(2)  VALUE ZERO.
005540      05 �d�ÂS�v�q.
005550         07 �d�É񐔂S�v�q                PIC 9(2)  VALUE ZERO.
005560********************
005570* �T���ʁ^�Q�D�T�� *
005580********************
005590   03 ���ʂT�Q�v�q.
005600      05 ��ÂT�Q�v�q.
005610         07 ��É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
005620      05 ��㪖@�T�Q�v�q.
005630         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
005640      05 ��㪖@�T�Q�v�q.
005650         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
005660      05 �d�ÂT�Q�v�q.
005670         07 �d�É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
005680****************
005690* �T���ʁ^�T�� *
005700****************
005710   03 ���ʂT�T�v�q.
005720      05 ��ÂT�T�v�q.
005730         07 ��É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
005740      05 ��㪖@�T�T�v�q.
005750         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
005760      05 ��㪖@�T�T�v�q.
005770         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
005780      05 �d�ÂT�T�v�q.
005790         07 �d�É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
005800****************
005810* �T���ʁ^�W�� *
005820****************
005830   03 ���ʂT�W�v�q.
005840      05 ��ÂT�W�v�q.
005850         07 ��É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
005860      05 ��㪖@�T�W�v�q.
005870         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
005880      05 ��㪖@�T�W�v�q.
005890         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
005900      05 �d�ÂT�W�v�q.
005910         07 �d�É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
005920******************
005930* �T���ʁ^�P�O�� *
005940******************
005950   03 ���ʂT�O�v�q.
005960      05 ��ÂT�O�v�q.
005970         07 ��É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
005980      05 ��㪖@�T�O�v�q.
005990         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
006000      05 ��㪖@�T�O�v�q.
006010         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
006020      05 �d�ÂT�O�v�q.
006030         07 �d�É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
006040******************
006050* �T���ʁ^���v�@ *
006060******************
006070   03 ���ʂT�v�q.
006080      05 ��ÂT�v�q.
006090         07 ��É񐔂T�v�q                PIC 9(2)  VALUE ZERO.
006100      05 ��㪖@�T�v�q.
006110         07 ��㪖@�񐔂T�v�q              PIC 9(2)  VALUE ZERO.
006120      05 ��㪖@�T�v�q.
006130         07 ��㪖@�񐔂T�v�q              PIC 9(2)  VALUE ZERO.
006140      05 �d�ÂT�v�q.
006150         07 �d�É񐔂T�v�q                PIC 9(2)  VALUE ZERO.
006160*
006170*****************************************************************
006180 01 �v�Z�@����N�v                     PIC 9(2).
006190* ���t�v�n�q�j
006200 01 �v�Z�@����.
006210    03 �v�Z�@����N                    PIC 9(4).
006220    03 �v�Z�@�����                  PIC 9(4).
006230 01 �v�Z�@����q REDEFINES �v�Z�@����.
006240    03 �v�Z�@���I                      PIC 9(2).
006250    03 �v�Z�@���t                      PIC 9(6).
006260    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
006270       05 �v�Z�@�N��                   PIC 9(4).
006280       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
006290         07 �v�Z�@�N                   PIC 9(2).
006300         07 �v�Z�@��                   PIC 9(2).
006310       05 �v�Z�@��                     PIC 9(2).
006320*
      * C �A�g�p
       01  �����P�v        PIC X(4096).
       01  �����Q�v        PIC X(512).
       01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
      *
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
006330******************************************************************
006340*                          �A������                              *
006350******************************************************************
006360*
006370********************
006380* ���b�Z�[�W�\���L�[ *
006390********************
006400 01 �A���|�L�[ IS EXTERNAL.
006410    03  �A���|���b�Z�[�W               PIC N(20).
006420*
006430 01 �A���R�|�L�[ IS EXTERNAL.
006440    03  �A���R�|���b�Z�[�W             PIC N(20).
006450    03  �A���R�|���b�Z�[�W�P           PIC X(20).
006460*
006470****************
006480* ��ʓ��͏�� *
006490****************
002278 01 �A���|��ʏ��x�i�a�T�W�O IS EXTERNAL.
002279    03 �A���|�����a��N��.
002280       05 �A���|�����a��               PIC 9.
002281       05 �A���|�����N��.
002282         07 �A���|�����N               PIC 9(2).
002283         07 �A���|������               PIC 9(2).
006560*
009851************************
009852* �������R���Z�b�g     *
009853************************
009854 01 �A�����|�L�[ IS EXTERNAL.
009855    03 �A�����|�{�p�N��.
009856       05 �A�����|�{�p�a��               PIC 9.
009857       05 �A�����|�{�p�N                 PIC 9(2).
009858       05 �A�����|�{�p��                 PIC 9(2).
009859    03  �A�����|���҃R�[�h.
009860       05 �A�����|���Ҕԍ�               PIC 9(6).
009861       05 �A�����|�}��                   PIC X.
009862    03 �A�����|������                    PIC 9(2).
009863    03 �A�����|���R��                    PIC N(63) OCCURS 15.
009864*
009865************************
009866* �������Z�܂Ƃ�
009867************************
009868 01 �A���Z�܂Ƃ߁|�L�[ IS EXTERNAL.
009869    03 �A���Z�܂Ƃ߁|�{�p�a��N��.
009870       05 �A���Z�܂Ƃ߁|�{�p�a��               PIC 9.
009871       05 �A���Z�܂Ƃ߁|�{�p�N��.
009872          07 �A���Z�܂Ƃ߁|�{�p�N              PIC 9(2).
009873          07 �A���Z�܂Ƃ߁|�{�p��              PIC 9(2).
009874    03 �A���Z�܂Ƃ߁|���҃R�[�h.
009875       05 �A���Z�܂Ƃ߁|���Ҕԍ�               PIC 9(6).
009876       05 �A���Z�܂Ƃ߁|�}��                   PIC X(1).
009877**-------------------------------------------------------**
009878*   1:�������Z�v�g�Ȃ��̖{�̂܂Ƃ߂̔���
009879*   2:���l�E���p�̎Еۏ������Z���̔���
009880    03 �A���Z�܂Ƃ߁|����敪                  PIC 9.
009881**-------------------------------------------------------**
009882*  / OUT /�@ 0:�ΏۊO�A1:�Ώ�
009883    03 �A���Z�܂Ƃ߁|���茋��                  PIC 9.
009884**
009885*
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
009886******************************************************************
009887*                      PROCEDURE  DIVISION                       *
009888******************************************************************
009890 PROCEDURE               DIVISION.
009900************
009910*           *
009920* ��������   *
009930*           *
009940************
009950     PERFORM ������.
009960     PERFORM ������擾.
009961     PERFORM �{�p�����擾.
009970************
009980*           *
009990* �又��     *
010000*           *
010010************
010020     PERFORM ��ƃt�@�C���쐬.
010030************
010040*           *
010050* �I������   *
010060*           *
010070************
010080     PERFORM �I������.
010090     MOVE ZERO TO PROGRAM-STATUS.
010100     EXIT PROGRAM.
010110*
010120*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
010130*================================================================*
010140 ������ SECTION.
010150*
010160     PERFORM �t�@�C���I�[�v��.
010170* �A�����ڂ̑Ҕ�
010180     MOVE �A���|�����a��  TO �����a��v�q.
010190     MOVE �A���|�����N    TO �����N�v�q.
010200     MOVE �A���|������    TO �������v�q.
010210*
010220     MOVE ZERO            TO �A�Ԃv.
010230*
010240* ������N���̎擾
010250     MOVE ZERO          TO ����N���v  ������N���v.
010260     MOVE �����a��v�q  TO ���|�����敪.
010270     READ �����}�X�^
010280     NOT INVALID KEY
010290         MOVE ���|�J�n����N TO ����N�v
010300     END-READ.
010310*
010320     IF ����N�v = ZERO
010330          MOVE  NC"�����}�X�^�ɊJ�n����N��o�^���ĉ�����" TO �A���|���b�Z�[�W
010340          CALL   "MSG001"
010350          CANCEL "MSG001"
010360          PERFORM �t�@�C����
010370          MOVE 99 TO PROGRAM-STATUS
010380          EXIT PROGRAM
010390     ELSE
010400          COMPUTE ����N�v = ����N�v + �����N�v�q - 1
010410          MOVE �������v�q TO ����v
010420     END-IF.
010430*
010440     MOVE ����N���v   TO  ������N���v.
010450*
010460*================================================================*
010470 �t�@�C���I�[�v�� SECTION.
010480*
010481     OPEN INPUT ������}�X�^.
010482         MOVE NC"������" TO �t�@�C����.
010483         PERFORM �I�[�v���`�F�b�N.
010490     OPEN INPUT �����}�X�^.
010500         MOVE NC"�����}�X�^" TO �t�@�C����.
010510         PERFORM �I�[�v���`�F�b�N.
010520     OPEN INPUT ���̃}�X�^.
010530         MOVE NC"���̃}�X�^" TO �t�@�C����.
010540         PERFORM �I�[�v���`�F�b�N.
010550     OPEN INPUT �{�p�����}�X�^
010560         MOVE NC"�{��" TO �t�@�C����.
010570         PERFORM �I�[�v���`�F�b�N.
010580     OPEN INPUT �{�p�L�^�e.
010590         MOVE NC"�{�p�L�^�e" TO �t�@�C����.
010600         PERFORM �I�[�v���`�F�b�N.
010610     OPEN INPUT ��f�ҏ��e.
010620         MOVE NC"��f�ҏ��e" TO �t�@�C����.
010630         PERFORM �I�[�v���`�F�b�N.
010640     OPEN INPUT �����f�[�^�e.
010650         MOVE NC"�����f�[�^�e" TO �t�@�C����.
010660         PERFORM �I�[�v���`�F�b�N.
010661     OPEN INPUT ���������e.
010662         MOVE NC"��������" TO �t�@�C����.
010663         PERFORM �I�[�v���`�F�b�N.
010670     OPEN INPUT �s�����}�X�^
010680         MOVE NC"�s����" TO �t�@�C����.
010690         PERFORM �I�[�v���`�F�b�N.
006630     OPEN INPUT ���Z�v�g�e.
006640         MOVE NC"���Z" TO �t�@�C����.
006650         PERFORM �I�[�v���`�F�b�N.
003001     OPEN INPUT ��v�f�[�^�e.
003002         MOVE NC"��v" TO �t�@�C����.
003003         PERFORM �I�[�v���`�F�b�N.
011344     OPEN OUTPUT ��ƃt�@�C���P.
011345         MOVE NC"��P" TO �t�@�C����.
011346         PERFORM �I�[�v���`�F�b�N.
010730*
010740*================================================================*
010750 �I�[�v���`�F�b�N SECTION.
010760*
010770     IF ��ԃL�[  NOT =  "00"
010780         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
010790         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
010800         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
010810                                                 UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
010820         ACCEPT  �L�[���� FROM CONS
010830         PERFORM �t�@�C����
010840         MOVE 99 TO PROGRAM-STATUS
010850         EXIT PROGRAM.
010860*================================================================*
010870 �t�@�C���� SECTION.
010880*
010890     CLOSE ������}�X�^ �����}�X�^ ���̃}�X�^ ��f�ҏ��e
010891           �����f�[�^�e   ���������e �{�p�L�^�e �{�p�����}�X�^
010901           �s�����}�X�^   ��v�f�[�^�e ���Z�v�g�e ��ƃt�@�C���P.
010910*================================================================*
010920 �I������ SECTION.
010930*
010940     PERFORM �t�@�C����.
010941*================================================================*
010942 �G���[�\���q SECTION.
010943*
010944     DISPLAY NC"�t�@�C���Ǎ��G���[" �t�@�C����     UPON CONS.
010945     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
010946     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
010947     ACCEPT  �L�[���� FROM CONS.
010948     PERFORM �t�@�C����.
010949     MOVE 99 TO PROGRAM-STATUS.
010950     EXIT PROGRAM.
010951*================================================================*
010960 �G���[�\�� SECTION.
010970*
010980     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
010990     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
011000     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
011010     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
011020     ACCEPT  �L�[���� FROM CONS.
011030     PERFORM �t�@�C����.
011040     MOVE 99 TO PROGRAM-STATUS.
011050     EXIT PROGRAM.
011051*================================================================*
011052 ������擾 SECTION.
011053*
011054     MOVE ZEROS TO ���|����敪.
011055     READ ������}�X�^
011056     NOT INVALID KEY
011058         MOVE ���|���Z������������敪 TO ������������敪�v
011059         MOVE ���|���Z�������R����敪 TO �������R����敪�v
011062     END-READ.
011063*
011064*================================================================*
011070 �{�p�����擾 SECTION.
011080*
011090     MOVE ZERO  TO �{��|�{�p���ԍ�.
011100     READ �{�p�����}�X�^
011110     INVALID KEY
011120          MOVE  NC"�{�p�����}�X�^�ɓo�^��A���s���ĉ�����" TO �A���|���b�Z�[�W
011130          CALL   "MSG001"
011140          CANCEL "MSG001"
011150          PERFORM �t�@�C����
011160          MOVE 99 TO PROGRAM-STATUS
011170          EXIT PROGRAM
011180     NOT INVALID KEY
011190          IF  �{��|�ڍ��t�����ԍ� = SPACE
011200              MOVE  NC"�{�p���}�X�^�ɉ���ԍ���o�^���ĉ�����" TO �A���|���b�Z�[�W
011210              CALL   "MSG001"
011220              CANCEL "MSG001"
011230              PERFORM �t�@�C����
011240              MOVE 99 TO PROGRAM-STATUS
011250              EXIT PROGRAM
011260          ELSE
011270              MOVE �{��|�ڍ��t�����ԍ�  TO ����ԍ��v
011280              PERFORM ����ԍ��E�l��
011290          END-IF
011300     END-READ.
011310*
011320*================================================================*
011330 ��ƃt�@�C���쐬 SECTION.
011340*
011342     PERFORM �ی��Ҕԍ����t�@�C���쐬.
011343*
011347     OPEN INPUT  ��ƃt�@�C���R.
011348         MOVE NC"��R" TO �t�@�C����.
011349         PERFORM �I�[�v���`�F�b�N.
011350*
011351     PERFORM ��ƃt�@�C���P�쐬.
011352*
011353     CLOSE ��ƃt�@�C���R.
011420*
011421*================================================================*
011422 �ی��Ҕԍ����t�@�C���쐬 SECTION.
011423**********************************************************************
011424**   ���Z�v�g�e����A�Y�������N���̃f�[�^�𒊏o���A
011425**   ��ƃt�@�C���R(�ی��Ҕԍ���)�ɏ����o��.
011426**********************************************************************
011427*
011428     OPEN OUTPUT ��ƃt�@�C���R.
011429         MOVE NC"��R" TO �t�@�C����.
011430         PERFORM �I�[�v���`�F�b�N.
011431*
011432     MOVE �����a��v�q  TO ���Z�|�����a��.
011433     MOVE �����N�v�q    TO ���Z�|�����N.
011434     MOVE �������v�q    TO ���Z�|������.
013730     MOVE ZERO          TO ���Z�|���Z���.
013720     MOVE ZERO          TO ���Z�|�{�p�a��.
013730     MOVE ZERO          TO ���Z�|�{�p�N.
013740     MOVE ZERO          TO ���Z�|�{�p��.
013750     MOVE ZERO          TO ���Z�|���Ҕԍ�.
013760     MOVE SPACE         TO ���Z�|�}��.
013770     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
000230                                  ���Z�|�{�p�a��N��
000240                                  ���Z�|���҃R�[�h
000250                                  ���Z�|���Z���
           END-START.
011444     IF ��ԃL�[ = "00"
011445         MOVE SPACE  TO �I���t���O
013820         PERFORM ���Z�v�g�e�Ǎ�
011447         PERFORM UNTIL ( �I���t���O = "YES" ) OR
013840                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
013850                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
013860                       ( ���Z�|������   NOT = �������v�q   )
005430            PERFORM �f�[�^�`�F�b�N
012327**
012328            IF  ���s�L�[�v = "YES"
      */���쌧����52��q�A53��Q�A55���c���A60���̑��͏��ҕ����̈׃f�[�^�ɍڂ��Ȃ�/110922
      */���쌧����60�q�ǂ���Â��Ō�ɍڂ���/211102
      */�e�ی����Z�b�g�ōŌ�ɂ���/230302                                                       
      *                 IF ((��|������� = 52 OR 53 OR 55) OR
      *                     ((���Z�|���Z��� NOT = 3) AND ( ��|������� = 60))) AND
                       IF (��|������� = 52 OR 53 OR 55) AND
                          (��|��p���S�Ҕԍ�����(3:2) = "20" )
                           MOVE ZERO  TO ��|�������
                           MOVE SPACE TO ��|��p���S�Ҕԍ�����
                       END-IF
      *
011452                 MOVE SPACE TO ��R�|���R�[�h
011453                 INITIALIZE    ��R�|���R�[�h
011454                 MOVE ���Z�|�����a��   TO  ��R�|�����a��
011455                 MOVE ���Z�|�����N     TO  ��R�|�����N
011456                 MOVE ���Z�|������     TO  ��R�|������
011457                 MOVE ��|�{�p�a��     TO  ��R�|�{�p�a��
011458                 MOVE ��|�{�p�N       TO  ��R�|�{�p�N
011459                 MOVE ��|�{�p��       TO  ��R�|�{�p��
011460                 IF  ��|�������   = ZERO  OR 50
011461                     IF ( ��|�ی����   NOT = ZERO ) AND
011462                        ( ��|�ی��Ҕԍ� NOT = SPACE )
011463** �����Ȃ�(���ېe����͏����Ȃ�����)
011464                        MOVE ZERO  TO   ��R�|�����敪
011468                     END-IF
011469                 ELSE
011470** ��������
011471                     MOVE 1        TO   ��R�|�����敪
011472*                   /���ʁF ��q�܂��͏�Q�ł��É��� �́A�����Ȃ��ɂ���/
011473                     IF (( ��|������� = "52" ) OR ( ��|������� = "53" )) AND
011474                        ( ��|��p���S�Ҕԍ�����(3:2) = "22" ) 
011475                         MOVE ZERO  TO  ��R�|�����敪
011476                     END-IF
011481                 END-IF
      */���쌧����60�Еۂ̎q�ǂ���Â��Ō�ɍڂ���/211102
      */�e�ی����Z�b�g�ōŌ�ɂ���/230302
      *                 IF (���Z�|���Z��� = 3) AND
                       IF ( ��|������� = 60) AND
                          (��|��p���S�Ҕԍ�����(3:2) = "20" )
011475                         MOVE 2  TO  ��R�|�����敪
                       END-IF
011482*
011483                 IF ( ��|������       = ZERO  ) AND
011484                    ( ��|��p���S�Ҕԍ� = SPACE )
011485                     MOVE ��|�ی��Ҕԍ�     TO �ی��Ҕԍ��v
011486                 ELSE
011487* �V�l�́A�s�����ԍ�
011488                     MOVE ��|��p���S�Ҕԍ� TO �ی��Ҕԍ��v
011489                 END-IF
011490                 PERFORM �ی��Ҕԍ��E�l��
011491                 MOVE �ی��Ҕԍ������v   TO ��R�|�ی��Ҕԍ�
011492*
011493                 MOVE ��|�{�l�Ƒ��敪   TO ��R�|�{�l�Ƒ��敪
011494                 MOVE ��|��ی��҃J�i   TO ��R�|��ی��҃J�i
011495                 MOVE ��|���҃R�[�h     TO ��R�|���҃R�[�h
011496*
                       EVALUATE ���Z�|���Z���
                       WHEN 1
                       WHEN 2
                           MOVE ZERO           TO ��R�|�e�q�敪
                       WHEN 3
                           MOVE 1              TO ��R�|�e�q�敪
                       END-EVALUATE
      *
                       IF (���Z�|���Z��� = 3) AND (��R�|�����敪 = ZERO)
                          CONTINUE
                       ELSE
011497                    WRITE ��R�|���R�[�h
011498                    INVALID KEY
011499                        MOVE NC"��R"  TO �t�@�C����
011500                    PERFORM �G���[�\��
011501                    END-WRITE
                       END-IF
                   END-IF
011503             PERFORM ���Z�v�g�e�Ǎ�
012418         END-PERFORM
           END-IF.
011506*
011507     CLOSE ��ƃt�@�C���R.
011502*
011509*================================================================*
011510 ��ƃt�@�C���P�쐬 SECTION.
011511*
012302     MOVE SPACE  TO �I���t���O.
012303     PERFORM ��ƃt�@�C���R�Ǎ�.
012304     PERFORM UNTIL  �I���t���O = "YES" 
012301*
012305         MOVE SPACE TO �����t���O
012306         MOVE SPACE TO �������Z�܂Ƃ߃t���O
               MOVE "YES" TO ���s�L�[�v
012308*
012309** �J�ЁE�����ӁE���R�E ���ےP�Ƃ͑ΏۊO
012310            IF  ��|�ی���� = 70 OR 80 OR 85 OR 90
012311                MOVE SPACE  TO ���s�L�[�v
012312            END-IF
012318** ���i�ؖ��͑ΏۊO
012319         IF  ( ��|�ی���� = 01 OR 08 ) AND
012320             ( ��|������ = ZERO     ) AND
012321             ( ��|���i�ؖ��敪 = 1 )
012322            MOVE SPACE  TO ���s�L�[�v
012323         END-IF
012327**
012328         IF  ���s�L�[�v = "YES"
012332*
012333*��* ���ʏ����i�������Z�܂Ƃ߁j
012334             IF ��|������� NOT = ZERO
012335                 PERFORM �������Z�܂Ƃߔ���
012336             ELSE
012337                 MOVE SPACE TO �������Z�܂Ƃ߃t���O
012338             END-IF
   339*��*
012341*            ********
012342*            * ���� *
012343*            ********
012372             IF ��R�|�e�q�敪 = ZERO
012344                IF ( ��|�ی����   NOT = ZERO ) AND
012345                   ( ��|�ی��Ҕԍ� NOT = SPACE )
012346*                **********************
012347*                * ��ƃt�@�C���쐬 *
012348*                **********************
012349                    IF ( ��|������       = ZERO  ) AND
012350                       ( ��|��p���S�Ҕԍ� = SPACE )
012351                       IF ��R�|�����敪  = ZERO
012352*   / �����Ȃ� /
012353                          MOVE SPACE TO �����t���O
012356                       ELSE
012357*   / �������� /
012358                          MOVE "YES" TO �����t���O
012366                       END-IF
012354                       PERFORM ��P���R�[�h�Z�b�g����
012355                       PERFORM ��P�t�@�C������
012367                    END-IF
012367                END-IF
                   END-IF
012369*            ********
012370*            * �V�l *
012371*            ********
012372             IF ��R�|�e�q�敪 = ZERO
012372                IF ( ��|������       NOT = ZERO ) AND
012373                   ( ��|��p���S�Ҕԍ� NOT = SPACE )
012374*                **********************
012375*                * ��ƃt�@�C���쐬 *
012376*                **********************
012377                   IF ��R�|�����敪  = ZERO
012378*   / �����Ȃ� /
012379                      MOVE SPACE TO �����t���O
012389                   ELSE
012390*   / �������� /
012391                      MOVE "YES" TO �����t���O
                         END-IF
012380                   PERFORM ��P���R�[�h�Z�b�g�V�l
012381                   PERFORM ��P�t�@�C������
                      END-IF
                   END-IF
012369*            ********
012370*            * ���� *
012371*            ********
012372             IF (��R�|�e�q�敪 = 1)
      */ ��t���̏����ŏ����������z���O�~�̏ꍇ�͏���
005930*/ ���l�E���̏�����Â̐����z�O�͑ΏۊO�ɂ��� /170411
                      IF ((��|��p���S�Ҕԍ�����(3:2) = "12") OR
                          ((��|������� = "55") AND (��|��p���S�Ҕԍ�����(3:3) = "144" OR "145"))) AND
                         (���Z�|�����������z = 0) AND
                         (�������Z�܂Ƃ߃t���O = SPACE)
                         CONTINUE
                      ELSE
012353                   MOVE "YES" TO �����t���O
                         IF ��|������ = ZERO
012394                      PERFORM ��P���R�[�h�Z�b�g���ۏ���
                         ELSE
012394                      PERFORM ��P���R�[�h�Z�b�g�V�l����
                         END-IF
012395                   PERFORM ��P�t�@�C������
012415                END-IF
                   END-IF
               END-IF
012417         PERFORM ��ƃt�@�C���R�Ǎ�
012418     END-PERFORM.
012419*
012439*================================================================*
012440 ��ƃt�@�C���R�Ǎ� SECTION.
012441*
012442     READ ��ƃt�@�C���R NEXT
012443     AT END
012444         MOVE "YES" TO �I���t���O
012445     NOT AT END
006665         MOVE ��R�|�{�p�a��    TO ��|�{�p�a�� ���Z�|�{�p�a��
006666         MOVE ��R�|�{�p�N      TO ��|�{�p�N   ���Z�|�{�p�N  
006667         MOVE ��R�|�{�p��      TO ��|�{�p��   ���Z�|�{�p��  
006668         MOVE ��R�|���Ҕԍ�    TO ��|���Ҕԍ� ���Z�|���Ҕԍ�
006669         MOVE ��R�|�}��        TO ��|�}��     ���Z�|�}��    
006670         READ ��f�ҏ��e
006671         INVALID KEY
006672              MOVE NC"��f��"   TO �t�@�C����
006673              PERFORM �G���[�\���q
006674         END-READ
               IF ��R�|�e�q�敪 = 1
                   MOVE 3          TO ���Z�|���Z���
               ELSE
                  IF ��|������ = 5
                      MOVE 2          TO ���Z�|���Z���
                  ELSE
                      MOVE 1          TO ���Z�|���Z���
                  END-IF
               END-IF
006670         READ ���Z�v�g�e
006671         INVALID KEY
006672              MOVE NC"���Z�v�g"   TO �t�@�C����
006673              PERFORM �G���[�\���q
006674         END-READ
012456     END-READ.
012457*
012420*================================================================*
008560 �f�[�^�`�F�b�N SECTION.
008570*
008580     MOVE SPACE          TO ���s�L�[�v.
019520* *****************************************************************
019530* * ���Z�v�g�e�̐����Ώۋ敪 = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
019540* *****************************************************************
019640     IF ( ���Z�|�����Ώۋ敪 NOT = ZERO ) AND
005778        ( ���Z�|���ҕ����敪 NOT = 1 )
              IF(���Z�|���Z��� = 3) AND ( ���Z�|����\����Ώۋ敪 = 1 )
                 CONTINUE
              ELSE
004090           MOVE ���Z�|�{�p�a��  TO ��|�{�p�a��
004100           MOVE ���Z�|�{�p�N    TO ��|�{�p�N
004110           MOVE ���Z�|�{�p��    TO ��|�{�p��
004120           MOVE ���Z�|���Ҕԍ�  TO ��|���Ҕԍ�
004130           MOVE ���Z�|�}��      TO ��|�}��
                 READ ��f�ҏ��e
                 NOT INVALID KEY
005786**      ���ۂ̂�
005787              IF ��|�ی����� = 1
019880                 MOVE "YES"  TO ���s�L�[�v
005789              END-IF
019950           END-READ
              END-IF
019960     END-IF.
009040*
012900*================================================================*
012910 ���Z�v�g�e�Ǎ� SECTION.
012920*
012930     READ ���Z�v�g�e NEXT
012940     AT END
012950         MOVE "YES" TO �I���t���O
012960     END-READ.
012970*
012980*================================================================*
012990 �{�p�L�^�e�Ǎ� SECTION.
013000*
013010     READ �{�p�L�^�e NEXT
013020     AT END
013030         MOVE "YES"  TO �I���t���O�Q
013040     END-READ.
013050*================================================================*
013060*================================================================*
013070 ��P���R�[�h�Z�b�g���� SECTION.
013080*
013090**********/  ���ۏ����Ȃ��̎�  /**********
013100*
013110     MOVE SPACE TO ��P�|���R�[�h.
013120     INITIALIZE ��P�|���R�[�h.
013130*
013163*��* ���ʏ����i�������Z�܂Ƃ߁j
013164*     IF �������Z�܂Ƃ߃t���O = "YES"
013165*        MOVE ���Z�|���v         TO ��P�|���v���z
013166*        MOVE ���Z�|�󋋎ҕ��S�z TO ��P�|�ꕔ���S��
013167*        COMPUTE ��P�|�������z = ���Z�|���v - ���Z�|�󋋎ҕ��S�z
013169*     ELSE
013173        MOVE ���Z�|���v       TO ��P�|���v���z.
013174        MOVE ���Z�|�ꕔ���S�� TO ��P�|�ꕔ���S��.
013175        MOVE ���Z�|�������z   TO ��P�|�������z.
013176*     END-IF.
013177*��*
013182*
013183     MOVE 1          TO ��P�|�Y���ی����  ��P�|�ی��敪�L�[.
013190*
013200* ���������鎞�́A���S�Ҕԍ��E�󋋎Ҕԍ����Z�b�g
      */���쌧����60�q�ǂ���Â��Ō�ɍڂ���/211102
013210     IF (�����t���O = "YES") OR
              (((���Z�|���Z��� NOT = 3) AND ( ��|������� = 60)) AND
               (��|��p���S�Ҕԍ�����(3:2) = "20" ))
013220         PERFORM �������S�Ҕԍ��擾
013230*
013240         IF ( ��|��v�Ҕԍ�����(1:1) = "*"  ) OR
013250            ( ��|��v�Ҕԍ�����(1:2) = "��" )
013260            MOVE SPACE                TO ��P�|�����󋋎Ҕԍ�
013270         ELSE
013280            MOVE ��|��v�Ҕԍ�����   TO ��P�|�����󋋎Ҕԍ�
013290         END-IF
013300     END-IF.
013310*
013320     MOVE ��|�{�l�Ƒ��敪   TO ��P�|�{�l�Ƒ��敪.
013330*
013340* �Z��(��ی���)
013350     STRING ��|�Z���P    DELIMITED BY SPACE
013360            ��|�Z���Q    DELIMITED BY SPACE
013370            INTO ��P�|��ی��ҏZ��
013380     END-STRING.
013390*
013410     PERFORM ���ʃ��R�[�h�Z�b�g.
013420*
013430*================================================================*
013440 ��P���R�[�h�Z�b�g�V�l SECTION.
013450*
013460**********/ 27�V�l�̎�  /**********
013470*
013480     MOVE SPACE TO ��P�|���R�[�h.
013490     INITIALIZE ��P�|���R�[�h.
013500*
           IF ��|�{�p�a��N�� < 42004
013510         MOVE ��|��p���S�Ҕԍ� TO �ی��Ҕԍ��v
013520         PERFORM �ی��Ҕԍ��E�l��
013530         MOVE �ی��Ҕԍ������v   TO ��P�|�V�l�s�����ԍ�
013540*
013550         MOVE ��|��v�Ҕԍ��V�l TO �ی��Ҕԍ��v
013560         PERFORM �ی��Ҕԍ��E�l��
013570         MOVE �ی��Ҕԍ������v   TO ��P�|�V�l�󋋎Ҕԍ�
           END-IF.
013611*
013612*��* ���ʏ����i�������Z�܂Ƃ߁j
013613*     IF �������Z�܂Ƃ߃t���O = "YES"
013614*        MOVE ���Z�|���v         TO ��P�|���v���z
013615*        MOVE ���Z�|�󋋎ҕ��S�z TO ��P�|�ꕔ���S��
013616*        COMPUTE ��P�|�������z = ���Z�|���v - ���Z�|�󋋎ҕ��S�z
013617*     ELSE
013618        MOVE ���Z�|���v       TO ��P�|���v���z.
013619        MOVE ���Z�|�ꕔ���S�� TO ��P�|�ꕔ���S��.
013620        MOVE ���Z�|�������z   TO ��P�|�������z.
013621*     END-IF.
013622*��*
013623*
           IF ��|�{�p�a��N�� < 42004
013630         MOVE 2              TO ��P�|�Y���ی����  ��P�|�ی��敪�L�[
           ELSE
013630         MOVE 1              TO ��P�|�Y���ی����  ��P�|�ی��敪�L�[
           END-IF.
013640*
013650* ���������鎞�́A���S�Ҕԍ��E�󋋎Ҕԍ����Z�b�g
      */���쌧����60�q�ǂ���Â��Ō�ɍڂ���/211102
013210     IF (�����t���O = "YES") OR
              (((���Z�|���Z��� NOT = 3) AND ( ��|������� = 60)) AND
               (��|��p���S�Ҕԍ�����(3:2) = "20" ))
013670         PERFORM �������S�Ҕԍ��擾
013680*
013690         IF ( ��|��v�Ҕԍ�����(1:1) = "*"  ) OR
013700            ( ��|��v�Ҕԍ�����(1:2) = "��" )
013710            MOVE SPACE                TO ��P�|�����󋋎Ҕԍ�
013720         ELSE
013730            MOVE ��|��v�Ҕԍ�����   TO ��P�|�����󋋎Ҕԍ�
013740         END-IF
013750     END-IF.
013760*
013770* �{�l�̂�
013780     MOVE 1   TO ��P�|�{�l�Ƒ��敪.
013790*
013800* �Z��(����)
013810     STRING ��|���ҏZ���P    DELIMITED BY SPACE
013820            ��|���ҏZ���Q    DELIMITED BY SPACE
013830            INTO ��P�|��ی��ҏZ��
013840     END-STRING.
013850*
013870     PERFORM ���ʃ��R�[�h�Z�b�g.
013880*
013890*================================================================*
013900 ��P���R�[�h�Z�b�g���ۏ��� SECTION.
013910*
013920**********/  ���ۏ�������̎�  /**********
013930*
013940     MOVE SPACE TO ��P�|���R�[�h.
013950     INITIALIZE ��P�|���R�[�h.
013960*
013970     PERFORM �������S�Ҕԍ��擾.
013980*
013990     IF ( ��|��v�Ҕԍ�����(1:1) = "*"  ) OR
014000        ( ��|��v�Ҕԍ�����(1:2) = "��" )
014010        MOVE SPACE                TO ��P�|�����󋋎Ҕԍ�
014020     ELSE
014030        MOVE ��|��v�Ҕԍ�����   TO ��P�|�����󋋎Ҕԍ�
014040     END-IF.
014050*
014113*��* ���ʏ����i�������Z�܂Ƃ߁j
014114     IF �������Z�܂Ƃ߃t���O = "YES"
014115        MOVE ���Z�|���v    TO ��P�|���v���z
014116        MOVE ZERO          TO ��P�|�ꕔ���S��
014117        MOVE ZERO          TO ��P�|�������z
014125        MOVE ���Z�|���t����     TO ��P�|�����p�ی����t����
014118*        MOVE 10            TO ��P�|�����p�ی����t����
014119     ELSE
014122        MOVE ���Z�|���v         TO ��P�|���v���z
014123        MOVE ���Z�|�󋋎ҕ��S�z TO ��P�|�ꕔ���S��
      */�É����̏����͑�����(��v�f�[�^�e)�̍��v��]�L����/130619
              IF (��|������� = "52" OR "53" OR "55" OR "60") AND
                 (��|��p���S�Ҕԍ�����(3:2) = "22" )
                  PERFORM ��v�f�[�^�e�W�v
              END-IF
014124*    �������z
014125        MOVE ���Z�|�����������z TO ��P�|�������z
014125        MOVE ���Z�|���t����     TO ��P�|�����p�ی����t����
014127*        COMPUTE ��P�|�����p�ی����t����  = 10 - ( ���S���v / 10 )
014128     END-IF.
014129*��*
      */���Z�܂Ƃߎ����{�̂̋��t������]�L����/130619
      **/���Z�܂Ƃߎ��̋��t������10��/230828
      *     IF ��|�������Z����Ώۋ敪 = 1 OR 2 OR 3
014118*         MOVE 10            TO ��P�|�����p�ی����t����
      *     END-IF.
014133*
014134     MOVE 3              TO ��P�|�Y���ی����  ��P�|�ی��敪�L�[.
014140*
014150* �{�l�̂�
014160     MOVE 1   TO ��P�|�{�l�Ƒ��敪.
014170*
014180* �Z��(����)
014190     STRING ��|���ҏZ���P    DELIMITED BY SPACE
014200            ��|���ҏZ���Q    DELIMITED BY SPACE
014210            INTO ��P�|��ی��ҏZ��
014220     END-STRING.
014230*
014250     PERFORM ���ʃ��R�[�h�Z�b�g.
014260*
014270*================================================================*
014280 ��P���R�[�h�Z�b�g�V�l���� SECTION.
014290*
014300**********/  �V�l��������̎�  /**********
014310*
014320     MOVE SPACE TO ��P�|���R�[�h.
014330     INITIALIZE ��P�|���R�[�h.
014340*
           IF ��|�{�p�a��N�� < 42004
014350         MOVE ��|��p���S�Ҕԍ� TO �ی��Ҕԍ��v
014360         PERFORM �ی��Ҕԍ��E�l��
014370         MOVE �ی��Ҕԍ������v   TO ��P�|�V�l�s�����ԍ�
014380*
014390         MOVE ��|��v�Ҕԍ��V�l TO �ی��Ҕԍ��v
014400         PERFORM �ی��Ҕԍ��E�l��
014410         MOVE �ی��Ҕԍ������v   TO ��P�|�V�l�󋋎Ҕԍ�
           END-IF.
014420*
014430     PERFORM �������S�Ҕԍ��擾.
014440*
014450     IF ( ��|��v�Ҕԍ�����(1:1) = "*"  ) OR
014460        ( ��|��v�Ҕԍ�����(1:2) = "��" )
014470        MOVE SPACE                TO ��P�|�����󋋎Ҕԍ�
014480     ELSE
014490        MOVE ��|��v�Ҕԍ�����   TO ��P�|�����󋋎Ҕԍ�
014500     END-IF.
014510*
014560*
014561
014562*��* ���ʏ����i�������Z�܂Ƃ߁j
014563*     IF �������Z�܂Ƃ߃t���O = "YES"
014564*        MOVE ���Z�|���v    TO ��P�|���v���z
014565*        MOVE ZERO          TO ��P�|�ꕔ���S��
014566*        MOVE ZERO          TO ��P�|�������z
014567*
014568*        IF ( ��|��p���S�Ҕԍ�����(3:3) = "144" ) OR ( ��|��p���S�Ҕԍ�����(3:3) = "145" )
014576*           EVALUATE ��|�V�l���S���Ə�
014577*           WHEN 2
014579*              MOVE 8   TO ��P�|�����p�ی����t����
014580*           WHEN 3
014582*              MOVE 7   TO ��P�|�����p�ی����t����
014583*           WHEN OTHER
014584*              MOVE 9   TO ��P�|�����p�ی����t����
014585*           END-EVALUATE
014588*        ELSE
014589*           MOVE 11     TO ��P�|�����p�ی����t����
014590*        END-IF
014591*
014592*    ELSE
      */��˗����Z�܂Ƃ߂̏ꍇ�͔�p�P�O���E���S�O�~�E�����O�~�B�Ƃ���B������*/20170603
014113*��* ���ʏ����i�������Z�܂Ƃ߁j
014114     IF �������Z�܂Ƃ߃t���O = "YES"
014115         MOVE ���Z�|���v     TO ��P�|���v���z
014116         MOVE ZERO           TO ��P�|�ꕔ���S��
014117         MOVE ZERO           TO ��P�|�������z
014125         MOVE ���Z�|���t���� TO ��P�|�����p�ی����t����
014119     ELSE
      */��˗����Z�܂Ƃ߂̏ꍇ�͔�p�P�O���E���S�O�~�E�����O�~�B�Ƃ���B������*/20170603
014593        MOVE ���Z�|���v         TO ��P�|���v���z
014594        MOVE ���Z�|�󋋎ҕ��S�z TO ��P�|�ꕔ���S��
014595*    �������z
014596        MOVE ���Z�|�����������z TO ��P�|�������z
014597        MOVE 11                 TO ��P�|�����p�ی����t����
      */��˗����Z�܂Ƃ߂̏ꍇ�͔�p�P�O���E���S�O�~�E�����O�~�B�Ƃ���B*/20170603
014598     END-IF. 
014599*��*
      */�����p�ی����t�����̐ݒ�R��H�C��������/121026
           IF ���Z�|�{�̂܂Ƃߋ敪 = 1
      */����25�N4�����_�ސ쌧���̑S�Ă̎s�����ł܂Ƃ߂�/130406
014568*         IF ( ��|��p���S�Ҕԍ�����(3:3) = "144" ) OR ( ��|��p���S�Ҕԍ�����(3:3) = "145" )
004050         IF (��|��p���S�Ҕԍ�����(3:3) = "144" OR "145") AND (��|�{�p�a��N�� <  42504) OR
                  (��|��p���S�Ҕԍ�����(3:2) = "14")           AND (��|�{�p�a��N�� >= 42504)
014576            EVALUATE ��|�V�l���S���Ə�
014577            WHEN 2
014579               MOVE 8   TO ��P�|�����p�ی����t����
014580            WHEN 3
014582               MOVE 7   TO ��P�|�����p�ی����t����
014583            WHEN OTHER
014584               MOVE 9   TO ��P�|�����p�ی����t����
014585            END-EVALUATE
014588         ELSE
014589            MOVE 11     TO ��P�|�����p�ی����t����
014590         END-IF
014590     END-IF
      */�����p�ی����t�����̐ݒ�R��H�C��������/121026
014602*
014603     MOVE 3              TO ��P�|�Y���ی����  ��P�|�ی��敪�L�[.
014604*
014610* �{�l�̂�
014620     MOVE 1   TO ��P�|�{�l�Ƒ��敪.
014630*
014640* �Z��(����)
014650     STRING ��|���ҏZ���P    DELIMITED BY SPACE
014660            ��|���ҏZ���Q    DELIMITED BY SPACE
014670            INTO ��P�|��ی��ҏZ��
014680     END-STRING.
014690*
014710     PERFORM ���ʃ��R�[�h�Z�b�g.
014720*
014730*================================================================*
014740*================================================================*
014750 ���ʃ��R�[�h�Z�b�g SECTION.
014760*
014770     MOVE ���Z�|�����a��     TO ��P�|�����a��.
014780     MOVE ���Z�|�����N       TO ��P�|�����N.
014790     MOVE ���Z�|������       TO ��P�|������.
014800     MOVE ��|�{�p�a��       TO ��P�|�{�p�a�� �{�p�a��v�q.
014810     MOVE ��|�{�p�N         TO ��P�|�{�p�N �{�p�N�v�q.
014820     MOVE ��|�{�p��         TO ��P�|�{�p�� �{�p���v�q.
014830     MOVE ��|���҃R�[�h     TO ��P�|���҃R�[�h�L�[  ���҃R�[�h�v�q.
014840     MOVE ����ԍ������v     TO ��P�|����ԍ�.
014850* ��z���Ȃ�
014860     MOVE ZERO               TO ��P�|��z���󗝔ԍ��敪.
014870* �N��
014880     MOVE ������N���v     TO ��P�|�����N��.
014890*
014900     PERFORM ����{�p�N���擾.
014910     MOVE ����{�p�N���v     TO ��P�|�{�p�N��.
014920*
014930* ���Z�v�g�ԍ�(�A��) �� YJB102�ł��
014940*     COMPUTE �A�Ԃv =  �A�Ԃv  + 1.
014950*     MOVE �A�Ԃv             TO ��P�|���Z�v�g�ԍ�.
014960*
014970* �ی��Ҕԍ��L�[(����)
014980     IF ( ��|������       = ZERO  ) AND
014990        ( ��|��p���S�Ҕԍ� = SPACE )
015000          MOVE ��|�ی��Ҕԍ�     TO �ی��Ҕԍ��v
015010     ELSE
015020* / �V�l�́A�s�����ԍ����L�[�� /
               IF ��|�{�p�a��N�� < 42004
015030             MOVE ��|��p���S�Ҕԍ� TO �ی��Ҕԍ��v
               ELSE
015000             MOVE ��|�ی��Ҕԍ�     TO �ی��Ҕԍ��v
               END-IF
015040     END-IF.
015050     PERFORM �ی��Ҕԍ��E�l��.
015060     MOVE �ی��Ҕԍ������v   TO ��P�|�ی��Ҕԍ��L�[.
015070*
015080* �ی��Ҕԍ�(����)
015090     MOVE ��|�ی��Ҕԍ�     TO �ی��Ҕԍ��v
015100     PERFORM �ی��Ҕԍ��E�l��.
015110     MOVE �ی��Ҕԍ������v   TO ��P�|�ی��Ҕԍ�.
015120*
015130** �S���y�� (133033) �̎}�ԍ폜���āA�ی��Ҕԍ��ɃZ�b�g
015140     IF ( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(1:6) = "133033" )
015150         MOVE 133033         TO ��P�|�ی��Ҕԍ�  ��P�|�ی��Ҕԍ��L�[
015160     END-IF.
015170*
      *-----------------------------------------------------------------*
           MOVE SPACE TO �A�Í������|�Í����.
      *
      *    / �A�Í������|���͏��Z�b�g /
           MOVE ��|�L��       TO �A�Í������|�L��.
           MOVE ��|�ԍ�       TO �A�Í������|�ԍ�.
           MOVE ��|�Í������� TO �A�Í������|�Í�������.
      *
           CALL   �����v���O�������v.
           CANCEL �����v���O�������v.
      *
      *-----------------------------------------------------------------*
015180* �L��
015190*     IF ��|�L��(1:2)  = "��" 
015190     IF �A�Í������|���������L��(1:2)  = "��" 
015200        MOVE SPACE               TO �L���o�m�v
015210*        INSPECT �L���o�m�v  REPLACING ALL ���p�� BY �S�p��
015220        MOVE �L���o�v            TO ��P�|�L��
015230     ELSE
015240        PERFORM �L�����l��
015250        MOVE �L���o�v            TO ��P�|�L��
015260     END-IF.
015270* �ԍ�
015280*     IF ( ��|�ԍ�(1:1) = "*"  ) OR
015290*        ( ��|�ԍ�(1:2) = "��" )
015280     IF ( �A�Í������|���������ԍ�(1:1) = "*"  ) OR
015290        ( �A�Í������|���������ԍ�(1:2) = "��" )
015300        MOVE SPACE           TO ��P�|�ԍ�
015310     ELSE
015320*        MOVE ��|�ԍ�        TO ��P�|�ԍ�
015320        MOVE �A�Í������|���������ԍ� TO ��P�|�ԍ�
015330     END-IF.
015340* ����
015350     MOVE ��|��ی��Ҏ���   TO �����v.
           MOVE �S�p�����v         TO ��P�|��ی��Ҏ���.
015360     MOVE ��|��ی��҃J�i   TO ��P�|��ی��҃J�i ��P�|��ی��҃J�i�L�[.
015370     MOVE ��|���Ҏ���       TO �����v.
           MOVE �S�p�����v         TO ��P�|���Ҏ���.
015380     MOVE ��|���҃J�i       TO ��P�|���҃J�i.
015390*
015400     MOVE ��|���Ґ���       TO ��P�|���Ґ���.
015410* ���N����
015420     MOVE ZERO               TO �v�Z�a��N�����v.
015430     MOVE ��|���Ґ��N����   TO �v�Z�a��N�����v.
015440     PERFORM ����N�����擾.
015450     MOVE �v�Z����N�����v   TO ��P�|���Ґ��N����.
015460*
015470****�@/ ���Z�v�g�f�[�^�̎擾 /
015480*
015500     PERFORM �����f�[�^�擾.
015510     PERFORM �������擾.
015520     PERFORM �{�p�L�^�擾.
015530*****
015540*
015550* �P���ʖ�
015560     MOVE ������ʂv(1)          TO ������ʕϊ��O�v.
015570     PERFORM ������ʕϊ�.
015580     MOVE ������ʕϊ���v       TO ��P�|�����敪(1).
015590     MOVE �������v(1)            TO ��P�|������(1).
015600     MOVE ���񏈒u�񐔂v(1)      TO ��P�|���񏈒u��(1).
015610*
015620     MOVE ZERO                   TO �v�Z�a��N�����v.
015630     MOVE �����N�����v(1)        TO �v�Z�a��N�����v.
015640     PERFORM ����N�����擾.
015650     MOVE �v�Z����N�����v       TO ��P�|�����N����(1).
015660*
015670     MOVE ZERO                   TO �v�Z�a��N�����v.
015680     MOVE �����N�����v(1)        TO �v�Z�a��N�����v.
015690     PERFORM ����N�����擾.
015700     MOVE �v�Z����N�����v       TO ��P�|�����N����(1).
015710*
015720     MOVE ZERO                   TO �v�Z�a��N�����v.
015730     MOVE �J�n�N�����v(1)        TO �v�Z�a��N�����v.
015740     PERFORM ����N�����擾.
015750     MOVE �v�Z����N�����v       TO ��P�|�{�p�J�n�N����(1).
015760*
015770     MOVE ZERO                   TO �v�Z�a��N�����v.
015780     MOVE �I���N�����v(1)        TO �v�Z�a��N�����v.
015790     PERFORM ����N�����擾.
015800     MOVE �v�Z����N�����v       TO ��P�|�{�p�I���N����(1).
015810*
015820     MOVE �������v(1)            TO ��P�|������(1).
015830*
015840     MOVE �]�A�敪�v(1)          TO �]�A�ϊ��O�v.
015850     PERFORM �]�A�敪�ϊ�.
015860     MOVE �]�A�ϊ���v           TO ��P�|�]�A�敪(1).
015870*
015880     MOVE ��É񐔂P�v�q         TO ��P�|��É�(1).
015890*
015900     MOVE ��㪖@�񐔂P�v�q       TO ��P�|��㪖@��(1).
015910*
015920     MOVE ��㪖@�񐔂P�v�q       TO ��P�|��㪖@��(1).
015930*
015940     MOVE �d�É񐔂P�v�q         TO ��P�|�d�É�(1).
015950*
015960     MOVE �����ʋ敪�v(1)        TO ��P�|�����ʒ����敪(1).
015970*
015980     MOVE �����敪�v(1)          TO ��P�|���������敪(1).
015990*
016000* �Q���ʖ�
016010     MOVE ������ʂv(2)          TO ������ʕϊ��O�v.
016020     PERFORM ������ʕϊ�.
016030     MOVE ������ʕϊ���v       TO ��P�|�����敪(2).
016040     MOVE �������v(2)            TO ��P�|������(2).
016050     MOVE ���񏈒u�񐔂v(2)      TO ��P�|���񏈒u��(2).
016060*
016070     MOVE ZERO                   TO �v�Z�a��N�����v.
016080     MOVE �����N�����v(2)        TO �v�Z�a��N�����v.
016090     PERFORM ����N�����擾.
016100     MOVE �v�Z����N�����v       TO ��P�|�����N����(2).
016110*
016120     MOVE ZERO                   TO �v�Z�a��N�����v.
016130     MOVE �����N�����v(2)        TO �v�Z�a��N�����v.
016140     PERFORM ����N�����擾.
016150     MOVE �v�Z����N�����v       TO ��P�|�����N����(2).
016160*
016170     MOVE ZERO                   TO �v�Z�a��N�����v.
016180     MOVE �J�n�N�����v(2)        TO �v�Z�a��N�����v.
016190     PERFORM ����N�����擾.
016200     MOVE �v�Z����N�����v       TO ��P�|�{�p�J�n�N����(2).
016210*
016220     MOVE ZERO                   TO �v�Z�a��N�����v.
016230     MOVE �I���N�����v(2)        TO �v�Z�a��N�����v.
016240     PERFORM ����N�����擾.
016250     MOVE �v�Z����N�����v       TO ��P�|�{�p�I���N����(2).
016260*
016270     MOVE �������v(2)            TO ��P�|������(2).
016280*
016290     MOVE �]�A�敪�v(2)          TO �]�A�ϊ��O�v.
016300     PERFORM �]�A�敪�ϊ�.
016310     MOVE �]�A�ϊ���v           TO ��P�|�]�A�敪(2).
016320*
016330     MOVE ��É񐔂Q�v�q         TO ��P�|��É�(2).
016340*
016350     MOVE ��㪖@�񐔂Q�v�q       TO ��P�|��㪖@��(2).
016360*
016370     MOVE ��㪖@�񐔂Q�v�q       TO ��P�|��㪖@��(2).
016380*
016390     MOVE �d�É񐔂Q�v�q         TO ��P�|�d�É�(2).
016400*
016410     MOVE �����ʋ敪�v(2)        TO ��P�|�����ʒ����敪(2).
016420*
016430     MOVE �����敪�v(2)          TO ��P�|���������敪(2).
016440*
016450* �R���ʖ�
016460     MOVE ������ʂv(3)          TO ������ʕϊ��O�v.
016470     PERFORM ������ʕϊ�.
016480     MOVE ������ʕϊ���v       TO ��P�|�����敪(3).
016490     MOVE �������v(3)            TO ��P�|������(3).
016500     MOVE ���񏈒u�񐔂v(3)      TO ��P�|���񏈒u��(3).
016510*
016520     MOVE ZERO                   TO �v�Z�a��N�����v.
016530     MOVE �����N�����v(3)        TO �v�Z�a��N�����v.
016540     PERFORM ����N�����擾.
016550     MOVE �v�Z����N�����v       TO ��P�|�����N����(3).
016560*
016570     MOVE ZERO                   TO �v�Z�a��N�����v.
016580     MOVE �����N�����v(3)        TO �v�Z�a��N�����v.
016590     PERFORM ����N�����擾.
016600     MOVE �v�Z����N�����v       TO ��P�|�����N����(3).
016610*
016620     MOVE ZERO                   TO �v�Z�a��N�����v.
016630     MOVE �J�n�N�����v(3)        TO �v�Z�a��N�����v.
016640     PERFORM ����N�����擾.
016650     MOVE �v�Z����N�����v       TO ��P�|�{�p�J�n�N����(3).
016660*
016670     MOVE ZERO                   TO �v�Z�a��N�����v.
016680     MOVE �I���N�����v(3)        TO �v�Z�a��N�����v.
016690     PERFORM ����N�����擾.
016700     MOVE �v�Z����N�����v       TO ��P�|�{�p�I���N����(3).
016710*
016720     MOVE �������v(3)            TO ��P�|������(3).
016730*
016740     MOVE �]�A�敪�v(3)          TO �]�A�ϊ��O�v.
016750     PERFORM �]�A�敪�ϊ�.
016760     MOVE �]�A�ϊ���v           TO ��P�|�]�A�敪(3).
016770*
016780     MOVE ��É񐔂R�v�q         TO ��P�|��É�(3).
016790*
016800     MOVE ��㪖@�񐔂R�v�q       TO ��P�|��㪖@��(3).
016810*
016820     MOVE ��㪖@�񐔂R�v�q       TO ��P�|��㪖@��(3).
016830*
016840     MOVE �d�É񐔂R�v�q         TO ��P�|�d�É�(3).
016850*
016860     MOVE �����ʋ敪�v(3)        TO ��P�|�����ʒ����敪(3).
016870*
016880     MOVE �����敪�v(3)          TO ��P�|���������敪(3).
016890*
016900* �S���ʖ�
016910     MOVE ������ʂv(4)          TO ������ʕϊ��O�v.
016920     PERFORM ������ʕϊ�.
016930     MOVE ������ʕϊ���v       TO ��P�|�����敪(4).
016940     MOVE �������v(4)            TO ��P�|������(4).
016950     MOVE ���񏈒u�񐔂v(4)      TO ��P�|���񏈒u��(4).
016960*
016970     MOVE ZERO                   TO �v�Z�a��N�����v.
016980     MOVE �����N�����v(4)        TO �v�Z�a��N�����v.
016990     PERFORM ����N�����擾.
017000     MOVE �v�Z����N�����v       TO ��P�|�����N����(4).
017010*
017020     MOVE ZERO                   TO �v�Z�a��N�����v.
017030     MOVE �����N�����v(4)        TO �v�Z�a��N�����v.
017040     PERFORM ����N�����擾.
017050     MOVE �v�Z����N�����v       TO ��P�|�����N����(4).
017060*
017070     MOVE ZERO                   TO �v�Z�a��N�����v.
017080     MOVE �J�n�N�����v(4)        TO �v�Z�a��N�����v.
017090     PERFORM ����N�����擾.
017100     MOVE �v�Z����N�����v       TO ��P�|�{�p�J�n�N����(4).
017110*
017120     MOVE ZERO                   TO �v�Z�a��N�����v.
017130     MOVE �I���N�����v(4)        TO �v�Z�a��N�����v.
017140     PERFORM ����N�����擾.
017150     MOVE �v�Z����N�����v       TO ��P�|�{�p�I���N����(4).
017160*
017170     MOVE �������v(4)            TO ��P�|������(4).
017180*
017190     MOVE �]�A�敪�v(4)          TO �]�A�ϊ��O�v.
017200     PERFORM �]�A�敪�ϊ�.
017210     MOVE �]�A�ϊ���v           TO ��P�|�]�A�敪(4).
017220*
017230     MOVE ��É񐔂S�v�q         TO ��P�|��É�(4).
017240*
017250     MOVE ��㪖@�񐔂S�v�q       TO ��P�|��㪖@��(4).
017260*
017270     MOVE ��㪖@�񐔂S�v�q       TO ��P�|��㪖@��(4).
017280*
017290     MOVE �d�É񐔂S�v�q         TO ��P�|�d�É�(4).
017300*
017310     MOVE �����ʋ敪�v(4)        TO ��P�|�����ʒ����敪(4).
017320*
017330     MOVE �����敪�v(4)          TO ��P�|���������敪(4).
017340*
017350* �T���ʖ�
017360     MOVE ������ʂv(5)          TO ������ʕϊ��O�v.
017370     PERFORM ������ʕϊ�.
017380     MOVE ������ʕϊ���v       TO ��P�|�����敪(5).
017390     MOVE �������v(5)            TO ��P�|������(5).
017400     MOVE ���񏈒u�񐔂v(5)      TO ��P�|���񏈒u��(5).
017410*
017420     MOVE ZERO                   TO �v�Z�a��N�����v.
017430     MOVE �����N�����v(5)        TO �v�Z�a��N�����v.
017440     PERFORM ����N�����擾.
017450     MOVE �v�Z����N�����v       TO ��P�|�����N����(5).
017460*
017470     MOVE ZERO                   TO �v�Z�a��N�����v.
017480     MOVE �����N�����v(5)        TO �v�Z�a��N�����v.
017490     PERFORM ����N�����擾.
017500     MOVE �v�Z����N�����v       TO ��P�|�����N����(5).
017510*
017520     MOVE ZERO                   TO �v�Z�a��N�����v.
017530     MOVE �J�n�N�����v(5)        TO �v�Z�a��N�����v.
017540     PERFORM ����N�����擾.
017550     MOVE �v�Z����N�����v       TO ��P�|�{�p�J�n�N����(5).
017560*
017570     MOVE ZERO                   TO �v�Z�a��N�����v.
017580     MOVE �I���N�����v(5)        TO �v�Z�a��N�����v.
017590     PERFORM ����N�����擾.
017600     MOVE �v�Z����N�����v       TO ��P�|�{�p�I���N����(5).
017610*
017620     MOVE �������v(5)            TO ��P�|������(5).
017630*
017640     MOVE �]�A�敪�v(5)          TO �]�A�ϊ��O�v.
017650     PERFORM �]�A�敪�ϊ�.
017660     MOVE �]�A�ϊ���v           TO ��P�|�]�A�敪(5).
017670*
017680     MOVE ��É񐔂T�v�q         TO ��P�|��É�(5).
017690*
017700     MOVE ��㪖@�񐔂T�v�q       TO ��P�|��㪖@��(5).
017710*
017720     MOVE ��㪖@�񐔂T�v�q       TO ��P�|��㪖@��(5).
017730*
017740     MOVE �d�É񐔂T�v�q         TO ��P�|�d�É�(5).
017750*
017760     MOVE �����ʋ敪�v(5)        TO ��P�|�����ʒ����敪(5).
017770*
017780     MOVE �����敪�v(5)          TO ��P�|���������敪(5).
017790*
017800* �U���ʖ�
017810     MOVE ������ʂv(6)          TO ������ʕϊ��O�v.
017820     PERFORM ������ʕϊ�.
017830     MOVE ������ʕϊ���v       TO ��P�|�����敪(6).
017840     MOVE �������v(6)            TO ��P�|������(6).
017850     MOVE ���񏈒u�񐔂v(6)      TO ��P�|���񏈒u��(6).
017860*
017870     MOVE ZERO                   TO �v�Z�a��N�����v.
017880     MOVE �����N�����v(6)        TO �v�Z�a��N�����v.
017890     PERFORM ����N�����擾.
017900     MOVE �v�Z����N�����v       TO ��P�|�����N����(6).
017910*
017920     MOVE ZERO                   TO �v�Z�a��N�����v.
017930     MOVE �����N�����v(6)        TO �v�Z�a��N�����v.
017940     PERFORM ����N�����擾.
017950     MOVE �v�Z����N�����v       TO ��P�|�����N����(6).
017960*
017970     MOVE ZERO                   TO �v�Z�a��N�����v.
017980     MOVE �J�n�N�����v(6)        TO �v�Z�a��N�����v.
017990     PERFORM ����N�����擾.
018000     MOVE �v�Z����N�����v       TO ��P�|�{�p�J�n�N����(6).
018010*
018020     MOVE ZERO                   TO �v�Z�a��N�����v.
018030     MOVE �I���N�����v(6)        TO �v�Z�a��N�����v.
018040     PERFORM ����N�����擾.
018050     MOVE �v�Z����N�����v       TO ��P�|�{�p�I���N����(6).
018060*
018070     MOVE �������v(6)            TO ��P�|������(6).
018080*
018090     MOVE �]�A�敪�v(6)          TO �]�A�ϊ��O�v.
018100     PERFORM �]�A�敪�ϊ�.
018110     MOVE �]�A�ϊ���v           TO ��P�|�]�A�敪(6).
018120*
018130     MOVE ZERO                   TO ��P�|��É�(6).
018140*
018150     MOVE ZERO                   TO ��P�|��㪖@��(6).
018160*
018170     MOVE ZERO                   TO ��P�|��㪖@��(6).
018180*
018190     MOVE ZERO                   TO ��P�|�d�É�(6).
018200*
018210     MOVE �����ʋ敪�v(6)        TO ��P�|�����ʒ����敪(6).
018220*
018230     MOVE �����敪�v(6)          TO ��P�|���������敪(6).
018240*
018250**************************************************************
018260*
018270     MOVE ���ʐ��v               TO ��P�|���ʐ�.
018280*
018320*
018330     MOVE �V�K�敪�v             TO ��P�|�V�K�敪.
018340     MOVE �p���敪�v             TO ��P�|�p���敪.
018350*
018360     MOVE �����񐔂v             TO ��P�|������.
018370     MOVE �������ԊO�񐔂v       TO ��P�|�������ԊO���Z��.
018380     MOVE �����x���񐔂v         TO ��P�|�����x�����Z��.
018390     MOVE �����[��񐔂v         TO ��P�|�����[����Z��.
018400     MOVE �Č��񐔂v             TO ��P�|�Č���.
018410     MOVE ���Ë����Q�v           TO ��P�|���Ë���.
018420     MOVE ���É񐔂v             TO ��P�|���É�.
018430*
018440     MOVE ���Ö�Ԃv             TO ��P�|��ԉ��Z���É�.
018450     MOVE ���Ö\���v             TO ��P�|�\���J����Z���É�.
018460     MOVE ���Ó�H�v             TO ��P�|��H���Z���É�.
018470*
018480     MOVE �������q�񐔂v         TO ��P�|�������q��.
018490     MOVE �^����É񐔂v         TO ��P�|�^����É�.
018510*
018520     MOVE ���񋟗��񐔂v       TO ��P�|���񋟗���.
018531**
018540** / ���������E�������R / **
018541*
018542     IF ������������敪�v  NOT = 1 
018543*      / ���������pWORK�N���A�[ /
018544         INITIALIZE ���������v�s
018545         INITIALIZE �������Ҕԍ��b�v
018546         INITIALIZE �����A�Ԃb�v
018547         INITIALIZE ���������s�a�k
018548         INITIALIZE �����������e�v
018549         PERFORM ���������擾
018550     END-IF.
018551*
018552     IF �������R����敪�v  NOT = 1 
018553        PERFORM �������R���擾
018554     ELSE
018555        MOVE  SPACE TO  �A�����|�L�[
018556        INITIALIZE      �A�����|�L�[
018557     END-IF.
018558*
018559     MOVE ���������v(1)       TO ��P�|��������.
018560     MOVE �A�����|���R��(1)   TO ��P�|�������R.
018561*
           MOVE 07                  TO ��P�|�Ǝҋ敪.
           MOVE ���k�x���񐔂v      TO ��P�|���k�x����.
           MOVE �{�p���s�v          TO ��P�|�{�p��.
018530*
018500     MOVE ���ה��s���Z�񐔂v  TO ��P�|���ה��s���Z��.
018500     MOVE ���ה��s�̐����v    TO ��P�|���ה��s�̐���.
018500     MOVE ���׏����s�����v    TO ��P�|���׏����s����.
018562*================================================================*
019320 ��P�t�@�C������ SECTION.
019330*
019340     WRITE ��P�|���R�[�h
019350     INVALID KEY
019360         MOVE NC"��P"  TO �t�@�C����
019370         PERFORM �G���[�\��
019380     END-WRITE.
019390*================================================================*
019400*================================================================*
019410 ����ԍ��E�l�� SECTION.
019420*
019430     MOVE ����ԍ��v      TO  ����ԍ����l�߂v.
019440     MOVE ZERO            TO  ����ԍ��E�l�߂v.
019450     MOVE ZERO            TO  ����ԍ������v.
019460*
019470     MOVE  8  TO  �J�E���^.
019480*
019490     IF  ����ԍ����l�߂v�P(7) NOT = SPACE
019500         COMPUTE �J�E���^ = �J�E���^  -  1
019510         MOVE ����ԍ����l�߂v�P(7)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019520     END-IF.
019530     IF  ����ԍ����l�߂v�P(6) NOT = SPACE
019540         COMPUTE �J�E���^ = �J�E���^  -  1
019550         MOVE ����ԍ����l�߂v�P(6)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019560     END-IF.
019570     IF  ����ԍ����l�߂v�P(5) NOT = SPACE
019580         COMPUTE �J�E���^ = �J�E���^  -  1
019590         MOVE ����ԍ����l�߂v�P(5)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019600     END-IF.
019610     IF  ����ԍ����l�߂v�P(4) NOT = SPACE
019620         COMPUTE �J�E���^ = �J�E���^  -  1
019630         MOVE ����ԍ����l�߂v�P(4)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019640     END-IF.
019650     IF  ����ԍ����l�߂v�P(3) NOT = SPACE
019660         COMPUTE �J�E���^ = �J�E���^  -  1
019670         MOVE ����ԍ����l�߂v�P(3)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019680     END-IF.
019690     IF  ����ԍ����l�߂v�P(2) NOT = SPACE
019700         COMPUTE �J�E���^ = �J�E���^  -  1
019710         MOVE ����ԍ����l�߂v�P(2)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019720     END-IF.
019730     IF  ����ԍ����l�߂v�P(1) NOT = SPACE
019740         COMPUTE �J�E���^ = �J�E���^  -  1
019750         MOVE ����ԍ����l�߂v�P(1)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019760     END-IF.
019770*
019780     MOVE ����ԍ��E�l�߂v TO ����ԍ������v.
019790*
019800*================================================================*
019810 �ی��Ҕԍ��E�l�� SECTION.
019820*
019830     MOVE �ی��Ҕԍ��v    TO  �ی��Ҕԍ����l�߂v.
019840     MOVE ZERO            TO  �ی��Ҕԍ��E�l�߂v.
019850     MOVE ZERO            TO  �ی��Ҕԍ������v.
019860*
019870     MOVE  9  TO  �J�E���^.
019880*
019890     IF  �ی��Ҕԍ����l�߂v�P(8) NOT = SPACE
019900         COMPUTE �J�E���^ = �J�E���^  -  1
019910         MOVE �ی��Ҕԍ����l�߂v�P(8)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
019920     END-IF.
019930     IF  �ی��Ҕԍ����l�߂v�P(7) NOT = SPACE
019940         COMPUTE �J�E���^ = �J�E���^  -  1
019950         MOVE �ی��Ҕԍ����l�߂v�P(7)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
019960     END-IF.
019970     IF  �ی��Ҕԍ����l�߂v�P(6) NOT = SPACE
019980         COMPUTE �J�E���^ = �J�E���^  -  1
019990         MOVE �ی��Ҕԍ����l�߂v�P(6)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020000     END-IF.
020010     IF  �ی��Ҕԍ����l�߂v�P(5) NOT = SPACE
020020         COMPUTE �J�E���^ = �J�E���^  -  1
020030         MOVE �ی��Ҕԍ����l�߂v�P(5)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020040     END-IF.
020050     IF  �ی��Ҕԍ����l�߂v�P(4) NOT = SPACE
020060         COMPUTE �J�E���^ = �J�E���^  -  1
020070         MOVE �ی��Ҕԍ����l�߂v�P(4)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020080     END-IF.
020090     IF  �ی��Ҕԍ����l�߂v�P(3) NOT = SPACE
020100         COMPUTE �J�E���^ = �J�E���^  -  1
020110         MOVE �ی��Ҕԍ����l�߂v�P(3)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020120     END-IF.
020130     IF  �ی��Ҕԍ����l�߂v�P(2) NOT = SPACE
020140         COMPUTE �J�E���^ = �J�E���^  -  1
020150         MOVE �ی��Ҕԍ����l�߂v�P(2)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020160     END-IF.
020170     IF  �ی��Ҕԍ����l�߂v�P(1) NOT = SPACE
020180         COMPUTE �J�E���^ = �J�E���^  -  1
020190         MOVE �ی��Ҕԍ����l�߂v�P(1)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020200     END-IF.
020210*
020220     MOVE �ی��Ҕԍ��E�l�߂v TO �ی��Ҕԍ������v.
020230*
020240*================================================================*
020250*================================================================*
020260 �L�����l�� SECTION.
020270*
020280***** �L���̖��ʂ�SPACE����菜���āA���l�߂ɂ���B
020290     MOVE SPACE           TO  �L���m�v.
020300     MOVE SPACE           TO  �L�����v.
020310     MOVE SPACE           TO  �L�����l�߂v.
020320*     MOVE ��|�L��        TO  �L�����v.
      *-----------------------------------------------------------------*
           MOVE SPACE TO �A�Í������|�Í����.
      *
      *    / �A�Í������|���͏��Z�b�g /
           MOVE ��|�L��       TO �A�Í������|�L��.
           MOVE ��|�ԍ�       TO �A�Í������|�ԍ�.
           MOVE ��|�Í������� TO �A�Í������|�Í�������.
      *
           CALL   �����v���O�������v.
           CANCEL �����v���O�������v.
      *
           MOVE �A�Í������|���������L�� TO �L�����v.
      *
      *-----------------------------------------------------------------*
020330*
020340     MOVE  ZERO  TO  �J�E���^�Q.
020350     PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 12
020360          IF  �L�����v�P(�J�E���^) NOT = SPACE
020370              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
020380              MOVE �L�����v�P(�J�E���^)  TO  �L�����l�߂v�P(�J�E���^�Q)
020390          END-IF
020400     END-PERFORM.
020410*
020290     MOVE SPACE           TO  �L���o�m�v.
020300     MOVE SPACE           TO  �L�����w�v.
020310     MOVE SPACE           TO  �L�����l�߂w�v.
020320     MOVE �L�����l�߂v    TO  �L�����w�v.
020330*
020340     MOVE  ZERO  TO  �J�E���^�Q.
020350     PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 24
020360          IF  �L�����w�v�P(�J�E���^) NOT = SPACE
020370              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
020380              MOVE �L�����w�v�P(�J�E���^)  TO  �L�����l�߂w�v�P(�J�E���^�Q)
020390          END-IF
020400     END-PERFORM.
020410*
020420     MOVE �L�����l�߂w�v    TO �L���o�m�v.
020430*
020440*���p�X�y�[�X��S�p�ɂ�����
020450*    INSPECT �L���v REPLACING ALL ���p�� BY �S�p��.
020460*
020470*================================================================*
020480*================================================================*
020490 �������擾 SECTION.
020500*
020510***********************************************
020520* �����f�[�^�Z�b�g                            *
020530***********************************************
020540*    ****************************************************************
020550*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
020560*    ****************************************************************
020570     INITIALIZE �����P�v�q.
020580     INITIALIZE �����Q�v�q.
020590     INITIALIZE �����R�v�q.
020600*
020620     MOVE ���Z�|���É�               TO  ���É񐔂v.
020621     MOVE ���Z�|���Ë���               TO  ���Ë����v.
020622* �P��100m
020623     COMPUTE  ���Ë����Q�v  =  ���Ë����v * 10.
020630*
           MOVE ���Z�|�������q��            TO �������q�񐔂v.
           MOVE ���Z�|�^����É�            TO �^����É񐔂v.
      *
           MOVE ���Z�|���׏����s���Z��         TO ���ה��s�̐����v.
           IF ���Z�|���׏����s���Z�� NOT = ZERO
               MOVE ���Z�|������               TO ���׌��v
           END-IF.
           MOVE ���Z�|���׏����s���Z��         TO ���ד��v.
           MOVE ���׌����v                     TO ���׏����s�����v.
      *
020640********************
020650* �����������Z�b�g *
020660********************
020670*    **********
020680*    * �P���� *
020690*    **********
020700     MOVE ���Z�|��É񐔂P             TO ��É񐔂P�v�q.
020710     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
020720     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
020730     MOVE ���Z�|�d�É񐔂P             TO �d�É񐔂P�v�q.
020740     MOVE 0          TO �����ʋ敪�v(1)
020750     IF ���Z�|�����������P NOT = ZERO
020760         MOVE 1      TO �����敪�v(1)
020770     ELSE
020780         MOVE 0      TO �����敪�v(1)
020790     END-IF
020800*    **********
020810*    * �Q���� *
020820*    **********
020830     MOVE ���Z�|��É񐔂Q             TO ��É񐔂Q�v�q.
020840     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
020850     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
020860     MOVE ���Z�|�d�É񐔂Q             TO �d�É񐔂Q�v�q.
020870     MOVE 0          TO �����ʋ敪�v(2)
020880     IF ���Z�|�����������Q NOT = ZERO
020890         MOVE 1      TO �����敪�v(2)
020900     ELSE
020910         MOVE 0      TO �����敪�v(2)
020920     END-IF
020930*    ****************
020940*    * �R���ʁ^�W�� *
020950*    ****************
020960     MOVE 0                              TO �����敪�v(3)
020970     MOVE 0                              TO �����敪�v(3)
020980     MOVE ���Z�|��É񐔂R�W             TO ��É񐔂R�W�v�q.
020990     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
021000     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
021010     MOVE ���Z�|�d�É񐔂R�W             TO �d�É񐔂R�W�v�q.
021020     IF ���Z�|���v�R�W NOT = ���Z�|�����ʍ����v�R�W
021030         MOVE 1        TO �����ʋ敪�v(3)
021040     END-IF
021050     IF ���Z�|�����������R�W NOT = ZERO
021060         MOVE 1        TO �����敪�v(3)
021070     END-IF
021080*    ****************
021090*    * �R���ʁ^10�� *
021100*    ****************
021110     MOVE ���Z�|��É񐔂R�O             TO ��É񐔂R�O�v�q.
021120     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
021130     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
021140     MOVE ���Z�|�d�É񐔂R�O             TO �d�É񐔂R�O�v�q.
021150     IF ���Z�|�����������R�O NOT = ZERO
021160         MOVE 1        TO �����敪�v(3)
021170     END-IF
021180*    ****************
021190*    * �R���ʁ^���v *
021200*    ****************
021210     COMPUTE ��É񐔂R�v�q      = ��É񐔂R�W�v�q   + ��É񐔂R�O�v�q.
021220     COMPUTE ��㪖@�񐔂R�v�q    = ��㪖@�񐔂R�W�v�q + ��㪖@�񐔂R�O�v�q.
021230     COMPUTE ��㪖@�񐔂R�v�q    = ��㪖@�񐔂R�W�v�q + ��㪖@�񐔂R�O�v�q.
021240     COMPUTE �d�É񐔂R�v�q      = �d�É񐔂R�W�v�q   + �d�É񐔂R�O�v�q.
021250*    ****************
021260*    * �S���ʁ^�T�� *
021270*    ****************
021280     MOVE 0                              TO �����敪�v(4)
021290     MOVE 0                              TO �����敪�v(4)
021300     MOVE ���Z�|��É񐔂S�T             TO ��É񐔂S�T�v�q.
021310     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
021320     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
021330     MOVE ���Z�|�d�É񐔂S�T             TO �d�É񐔂S�T�v�q.
021340     IF ���Z�|���v�S�T NOT = ���Z�|�����ʍ����v�S�T
021350         MOVE 1        TO �����ʋ敪�v(4)
021360     END-IF
021370     IF ���Z�|�����������S�T NOT = ZERO
021380         MOVE 1        TO �����敪�v(4)
021390     END-IF
021400*    ****************
021410*    * �S���ʁ^�W�� *
021420*    ****************
021430     MOVE ���Z�|��É񐔂S�W             TO ��É񐔂S�W�v�q.
021440     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
021450     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
021460     MOVE ���Z�|�d�É񐔂S�W             TO �d�É񐔂S�W�v�q.
021470     IF ���Z�|���v�S�W NOT = ���Z�|�����ʍ����v�S�W
021480         MOVE 1        TO �����ʋ敪�v(4)
021490     END-IF
021500     IF ���Z�|�����������S�W NOT = ZERO
021510         MOVE 1        TO �����敪�v(4)
021520     END-IF
021530*    ****************
021540*    * �S���ʁ^10�� *
021550*    ****************
021560     MOVE ���Z�|��É񐔂S�O             TO ��É񐔂S�O�v�q.
021570     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
021580     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
021590     MOVE ���Z�|�d�É񐔂S�O             TO �d�É񐔂S�O�v�q.
021600     IF ���Z�|�����������S�O NOT = ZERO
021610         MOVE 1        TO �����敪�v(4)
021620     END-IF
021630*    ****************
021640*    * �S���ʁ^���v *
021650*    ****************
021660     COMPUTE ��É񐔂S�v�q      = ��É񐔂S�T�v�q   + ��É񐔂S�W�v�q   + ��É񐔂S�O�v�q.
021670     COMPUTE ��㪖@�񐔂S�v�q    = ��㪖@�񐔂S�T�v�q + ��㪖@�񐔂S�W�v�q + ��㪖@�񐔂S�O�v�q.
021680     COMPUTE ��㪖@�񐔂S�v�q    = ��㪖@�񐔂S�T�v�q + ��㪖@�񐔂S�W�v�q + ��㪖@�񐔂S�O�v�q.
021690     COMPUTE �d�É񐔂S�v�q      = �d�É񐔂S�T�v�q   + �d�É񐔂S�W�v�q   + �d�É񐔂S�O�v�q.
021700*    *****************
021710*    * �T���ʁ^2.5�� *
021720*    *****************
021730     MOVE 0                              TO �����敪�v(5)
021740     MOVE 0                              TO �����敪�v(5)
021750     MOVE ���Z�|��É񐔂T�Q             TO ��É񐔂T�Q�v�q.
021760     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
021770     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
021780     MOVE ���Z�|�d�É񐔂T�Q             TO �d�É񐔂T�Q�v�q.
021790     IF ���Z�|���v�T�Q NOT = ���Z�|�����ʍ����v�T�Q
021800         MOVE 1        TO �����ʋ敪�v(5)
021810     END-IF
021820     IF ���Z�|�����������T�Q NOT = ZERO
021830         MOVE 1        TO �����敪�v(5)
021840     END-IF
021850*    ****************
021860*    * �T���ʁ^�T�� *
021870*    ****************
021880     MOVE ���Z�|��É񐔂T�T             TO ��É񐔂T�T�v�q.
021890     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
021900     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
021910     MOVE ���Z�|�d�É񐔂T�T             TO �d�É񐔂T�T�v�q.
021920     IF ���Z�|���v�T�T NOT = ���Z�|�����ʍ����v�T�T
021930         MOVE 1        TO �����ʋ敪�v(5)
021940     END-IF
021950     IF ���Z�|�����������T�T NOT = ZERO
021960         MOVE 1        TO �����敪�v(5)
021970     END-IF
021980*    ****************
021990*    * �T���ʁ^�W�� *
022000*    ****************
022010     MOVE ���Z�|��É񐔂T�W             TO ��É񐔂T�W�v�q.
022020     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
022030     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
022040     MOVE ���Z�|�d�É񐔂T�W             TO �d�É񐔂T�W�v�q.
022050     IF ���Z�|���v�T�W NOT = ���Z�|�����ʍ����v�T�W
022060         MOVE 1        TO �����ʋ敪�v(5)
022070     END-IF
022080     IF ���Z�|�����������T�W NOT = ZERO
022090         MOVE 1        TO �����敪�v(5)
022100     END-IF
022110*    ****************
022120*    * �T���ʁ^10�� *
022130*    ****************
022140     MOVE ���Z�|��É񐔂T�O             TO ��É񐔂T�O�v�q.
022150     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
022160     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
022170     MOVE ���Z�|�d�É񐔂T�O             TO �d�É񐔂T�O�v�q.
022180     IF ���Z�|�����������T�O NOT = ZERO
022190         MOVE 1        TO �����敪�v(5)
022200     END-IF
022210*    ****************
022220*    * �T���ʁ^���v *
022230*    ****************
022240     COMPUTE ��É񐔂T�v�q   = ��É񐔂T�Q�v�q   + ��É񐔂T�T�v�q   +
022250                                ��É񐔂T�W�v�q   + ��É񐔂T�O�v�q.
022260     COMPUTE ��㪖@�񐔂T�v�q = ��㪖@�񐔂T�Q�v�q + ��㪖@�񐔂T�T�v�q +
022270                                ��㪖@�񐔂T�W�v�q + ��㪖@�񐔂T�O�v�q.
022280     COMPUTE ��㪖@�񐔂T�v�q = ��㪖@�񐔂T�Q�v�q + ��㪖@�񐔂T�T�v�q +
022290                                ��㪖@�񐔂T�W�v�q + ��㪖@�񐔂T�O�v�q.
022300     COMPUTE �d�É񐔂T�v�q   = �d�É񐔂T�Q�v�q   + �d�É񐔂T�T�v�q   +
022310                                �d�É񐔂T�W�v�q   + �d�É񐔂T�O�v�q.
022320*
022330*================================================================*
022340 �����f�[�^�擾 SECTION.
022350*
022360     INITIALIZE �������v.
022361*
022370     MOVE �{�p�a��v�q       TO ���|�{�p�a��.
022380     MOVE �{�p�N�v�q         TO ���|�{�p�N.
022390     MOVE �{�p���v�q         TO ���|�{�p��.
022400     MOVE ���҃R�[�h�v�q     TO ���|���҃R�[�h.
022410     READ �����f�[�^�e
022420     INVALID KEY
022430         CONTINUE
022440     NOT INVALID KEY
022450         MOVE ���|���ʐ�                   TO ���ʐ��v
022460         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
022470                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
022480             MOVE ���|�������(���ʂb�m�s) TO ������ʂv(���ʂb�m�s)
022490             MOVE ���|����(���ʂb�m�s)     TO ���ʂv(���ʂb�m�s)
022500             MOVE ���|���E�敪(���ʂb�m�s) TO ���E�敪�v(���ʂb�m�s)
022510             MOVE ���|�����ʒu�ԍ�(���ʂb�m�s)
022520                                           TO �����ʒu�ԍ��v(���ʂb�m�s)
022530* �������
022540             MOVE SPACE                     TO �������̂v
022550             MOVE 03                        TO ���|�敪�R�[�h
022560             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
022570             READ ���̃}�X�^
022580             INVALID KEY
022590                 MOVE SPACE        TO �������̂v
022600             NOT INVALID KEY
022610                 MOVE ���|�������� TO �������̂v
022620             END-READ
022630* ����
022720             STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
022730                    �������̂v                    DELIMITED BY SPACE
022740                    ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
022750                    INTO �������v(���ʂb�m�s)
022760             END-STRING
022780*
022790             MOVE ���|�����a��(���ʂb�m�s)   TO �����a��v(���ʂb�m�s)
022800             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
022810             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
022820             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
022830             MOVE ���|�J�n�a��(���ʂb�m�s)   TO �����a��v(���ʂb�m�s)
022840             MOVE ���|�J�n�N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
022850             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
022860             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
022870             IF ���|�]�A�敪(���ʂb�m�s) = 9
022880                 MOVE 99                   TO �I���N�v(���ʂb�m�s)
022890                 MOVE 99                   TO �I�����v(���ʂb�m�s)
022900                 MOVE 99                   TO �I�����v(���ʂb�m�s)
022910             ELSE
022920                 MOVE ���|�I���a��(���ʂb�m�s)   TO �I���a��v(���ʂb�m�s)
022930                 MOVE ���|�I���N(���ʂb�m�s)   TO �I���N�v(���ʂb�m�s)
022940                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
022950                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
022960             END-IF
022970*
022980             MOVE ���|�]�A�敪(���ʂb�m�s) TO �]�A�敪�v(���ʂb�m�s)
022990*
023000         END-PERFORM
023010* �V�K/�p�� �`�F�b�N
023020         IF ���Z�|������  NOT = ZERO
023030             MOVE 1                   TO �V�K�敪�v
023040         ELSE
023050             MOVE 1                   TO �p���敪�v
023060         END-IF
023070         PERFORM �������ȑO�̃f�[�^����
023080* �}�Ԕ���p
023090         MOVE ���|�J�n�f�Ó��蓮�敪 TO  �J�n�f�Ó��蓮�敪�v
023100*
023110     END-READ.
023120*
023130*================================================================*
023140 �������ȑO�̃f�[�^���� SECTION.
023150*
023160*********************************************************************************
023170*  �ŏ��̏������ȑO�̓������Ɏ{�p�L�^���R�[�h����������(�����A���~)�́A�����敪��
023180*  �p���ɂ��`�F�b�N����B(�V�K�ƌp���̗���)
023190*********************************************************************************
023200** �ŏ��̏��������擾
023210     MOVE SPACE                 TO �����t���O.
023220     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
023230     MOVE �}�Ԃv�q              TO �{�L�|�}��.
023240     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
023250     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
023260     MOVE �{�p���v�q            TO �{�L�|�{�p��.
023270     MOVE ZERO                  TO �{�L�|�{�p��.
023280     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
023290                                  �{�L�|�{�p�a��N����
023300     END-START.
023310     IF ��ԃL�[ = "00"
023320         MOVE ZERO  TO �����a��v�s
023330         MOVE ZERO  TO �����N�v�s
023340         MOVE ZERO  TO �������v�s
023350         MOVE ZERO  TO �������v�s
023360         MOVE SPACE TO �I���t���O�Q
023370         PERFORM �{�p�L�^�e�Ǎ�
023380         PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
023390                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
023400                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
023410                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
023420                       ( �{�L�|�{�p��     NOT = �{�p���v�q      ) OR
023430                       ( �����t���O           = "YES"           ) 
023440               IF  �{�L�|�f�Ë敪 = 2
023450                   MOVE �{�L�|�{�p�a��           TO �����a��v�s
023460                   MOVE �{�L�|�{�p�N             TO �����N�v�s
023470                   MOVE �{�L�|�{�p��             TO �������v�s
023480                   MOVE �{�L�|�{�p��             TO �������v�s
023490                   MOVE "YES"                    TO �����t���O
023500               END-IF
023510               PERFORM �{�p�L�^�e�Ǎ�
023520         END-PERFORM
023530     END-IF.
023540*
023550* �������ȑO�̃f�[�^����
023560     IF �����t���O = "YES"
023570        MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
023580        MOVE �}�Ԃv�q              TO �{�L�|�}��
023590        MOVE �����a��v�s          TO �{�L�|�{�p�a��
023600        MOVE �����N�v�s            TO �{�L�|�{�p�N
023610        MOVE �������v�s            TO �{�L�|�{�p��
023620        MOVE �������v�s            TO �{�L�|�{�p��
023630        START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
023640                                     �{�L�|�{�p�a��N����
023650                                     REVERSED
023660        END-START
023670        IF ��ԃL�[ = "00"
023680           MOVE SPACE  TO �I���t���O�Q
023690           PERFORM �{�p�L�^�e�Ǎ�
023700           IF ( �I���t���O�Q    = SPACE        ) AND
023710              ( �{�L�|���Ҕԍ�  = ���Ҕԍ��v�q ) AND
023720              ( �{�L�|�}��      = �}�Ԃv�q     ) AND
023730              ( �{�L�|�{�p�a��  = �����a��v�s ) AND
023740              ( �{�L�|�{�p�N    = �����N�v�s   ) AND
023750              ( �{�L�|�{�p��    = �������v�s   )
023760*  �������ȑO�̓������Ɏ{�p�L�^���R�[�h����������
023770                IF �p���敪�v = ZERO
023780                   MOVE 1    TO �p���敪�v
023790                END-IF
023800           END-IF
023810         END-IF
023820     END-IF.
023830*
023840*================================================================*
023850 �{�p�L�^�擾 SECTION.
023860*
023870     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���ʐ��v
023880         IF ( �{�p�N�v�q = �����N�v(���ʂb�m�s) ) AND
023890            ( �{�p���v�q = �������v(���ʂb�m�s) )
023900             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
023910             MOVE �}�Ԃv�q              TO �{�L�|�}��
023920             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
023930             MOVE �����a��v(���ʂb�m�s)  TO �J�n�a��v(���ʂb�m�s)
023940             MOVE �����N�v(���ʂb�m�s)  TO �J�n�N�v(���ʂb�m�s) �{�L�|�{�p�N
023950             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
023960             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
023970         ELSE
023980             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
023990             MOVE �}�Ԃv�q              TO �{�L�|�}��
024000             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
024010             MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
024020             MOVE �{�p���v�q            TO �{�L�|�{�p��
024030             MOVE ZERO                  TO �{�L�|�{�p��
024040         END-IF
024050         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
024060                                      �{�L�|�{�p�a��N����
024070         END-START
024080         IF ��ԃL�[ = "00"
024090             MOVE ZERO  TO �������v(���ʂb�m�s)
024100             MOVE ZERO  TO ���񏈒u�񐔂v(���ʂb�m�s)
024110             MOVE ZERO  TO �I���a��v�s
024120             MOVE ZERO  TO �I���N�v�s
024130             MOVE ZERO  TO �I�����v�s
024140             MOVE ZERO  TO �I�����v�s
024150             MOVE SPACE TO �I���t���O�Q
024160             PERFORM �{�p�L�^�e�Ǎ�
024170             IF  ( �I���t���O�Q      = SPACE   ) AND
024180                 ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
024190                 ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
024200                 ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
024210                 ( �{�L�|�{�p��      = �{�p���v�q     ) 
024220*
024340*        ****************
024350*        * ���񋟉� *
024360*        ****************
024370             IF �{�L�|���񋟋敪(���ʂb�m�s) = 1
024380                 COMPUTE ���񋟗��񐔂v = ���񋟗��񐔂v + 1
024390             END-IF
024400*        *****************************************************************
024410*        * �J�n�N���� ( ���̕��ʂ����������łȂ����A
024420*                       ���������ł��}�Ԃ����鎞�́A�ŏ��̎{�p�����J�n��)*
024430*        *****************************************************************
024440                 IF ( �{�p�N�v�q NOT = �����N�v(���ʂb�m�s) ) OR
024450                    ( �{�p���v�q NOT = �������v(���ʂb�m�s) ) OR
024460                    ( �J�n�f�Ó��蓮�敪�v = 1 )
024470                     MOVE �{�L�|�{�p�a�� TO �J�n�a��v(���ʂb�m�s)
024480                     MOVE �{�L�|�{�p�N   TO �J�n�N�v(���ʂb�m�s)
024490                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
024500                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
024510                 END-IF
024520             END-IF
024530             PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
024540                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
024550                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
024560                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
024570                           ( �{�L�|�{�p��     NOT = �{�p���v�q       ) OR
024580                           ( �{�L�|�{�p��         > �I�����v(���ʂb�m�s))
024590*               **********
024600*               * ������ *
024610*               **********
024620                COMPUTE �������v(���ʂb�m�s) = �������v(���ʂb�m�s) + 1
024630                MOVE �{�L�|�{�p�a��             TO �I���a��v�s
024640                MOVE �{�L�|�{�p�N               TO �I���N�v�s
024650                MOVE �{�L�|�{�p��               TO �I�����v�s
024660                MOVE �{�L�|�{�p��               TO �I�����v�s
024670*            /�@���񏈒u�̃J�E���g�@/
024680                IF �{�L�|�����{�Ë敪(���ʂb�m�s) = 1
024690                    COMPUTE ���񏈒u�񐔂v(���ʂb�m�s) = ���񏈒u�񐔂v(���ʂb�m�s) + 1
024700                END-IF
024710*
024720                PERFORM �{�p�L�^�e�Ǎ�
024730            END-PERFORM
024740        END-IF
024750*       **************************
024760*       * �p���F�I���N�����Z�b�g *
024770*       **************************
024780        IF �]�A�敪�v(���ʂb�m�s) = 9
024790            MOVE �I���a��v�s  TO �I���a��v(���ʂb�m�s)
024800            MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
024810            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
024820            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
024830        END-IF
024840     END-PERFORM.
024841***
024850     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
024860     MOVE �}�Ԃv�q              TO �{�L�|�}��.
024870     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
024880     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
024890     MOVE �{�p���v�q            TO �{�L�|�{�p��.
024900     MOVE ZERO                  TO �{�L�|�{�p��.
024910     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
024920                                  �{�L�|�{�p�a��N����
024930     END-START.
024940     IF ��ԃL�[ = "00"
024150         MOVE SPACE TO �I���t���O�Q
024950         PERFORM �{�p�L�^�e�Ǎ�
024960         PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
024970                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
024980                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
024990                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
025000                       ( �{�L�|�{�p��     NOT = �{�p���v�q       )
025010*        ************
025020*        * ������ *
025030*        ************
025040             IF �{�L�|�����������敪 = 1
025050                 COMPUTE �����񐔂v = �����񐔂v + 1
025060             END-IF
025070*        ************
025080*        * �������Z *
025090*        ************
025100             EVALUATE �{�L�|�������Z
025110             WHEN 1
025120                 COMPUTE �������ԊO�񐔂v = �������ԊO�񐔂v + 1
025130             WHEN 2
025140                 COMPUTE �����x���񐔂v   = �����x���񐔂v + 1
025150             WHEN 3
025160                 COMPUTE �����[��񐔂v   = �����[��񐔂v + 1
025170             END-EVALUATE
025180*        ************
025190*        * �Č��� *
025200*        ************
025210             IF �{�L�|�Č������� = 1
025220                 COMPUTE �Č��񐔂v = �Č��񐔂v + 1
025230             END-IF
025240*        ************
025250*        * ���É��Z *
025260*        ************
025270             EVALUATE �{�L�|���É��Z
025280             WHEN 1
025290                 COMPUTE ���Ö�Ԃv = ���Ö�Ԃv + 1
025300             WHEN 2
025310                 COMPUTE ���Ó�H�v = ���Ó�H�v + 1
025320             WHEN 3
025330                 COMPUTE ���Ö\���v = ���Ö\���v + 1
025340             END-EVALUATE
025240*        ****************
025250*        * ���������k�� *
025260*        ****************
                   IF (�{�L�|�f�Ë敪 = 2 ) AND (�{�L�|���������k���敪 NOT = 1)
                       COMPUTE ���k�x���񐔂v = ���k�x���񐔂v + 1
                   END-IF
025180*        **********************
025190*        * ���׏����s���Z�� *
025200*        **********************
025210             IF �{�L�|���׏����s���Z�敪 = 1
025220                 COMPUTE ���ה��s���Z�񐔂v = ���ה��s���Z�񐔂v + 1
025230             END-IF
025240*        **********
025250*        * �{�p�� *
025260*        **********
                   MOVE 1 TO �{�p���v(�{�L�|�{�p��)
      *
025350             PERFORM �{�p�L�^�e�Ǎ�
025360         END-PERFORM
025370     END-IF.
025380*
025390*================================================================*
025820 ����{�p�N���擾 SECTION.
025830* 
025840     MOVE ZERO          TO ����N���v  ����{�p�N���v.
025850     MOVE ��|�{�p�a��  TO ���|�����敪.
025860     READ �����}�X�^
025870     NOT INVALID KEY
025880         MOVE ���|�J�n����N TO ����N�v
025890     END-READ.
025900**
025910     IF ����N�v = ZERO
025920          MOVE  NC"�����}�X�^�ɊJ�n����N��o�^���ĉ�����" TO �A���|���b�Z�[�W
025930          CALL   "MSG001"
025940          CANCEL "MSG001"
025950          PERFORM �t�@�C����
025960          MOVE 99 TO PROGRAM-STATUS
025970          EXIT PROGRAM
025980     ELSE
025990          COMPUTE ����N�v = ����N�v + ��|�{�p�N - 1
026000          MOVE ��|�{�p�� TO ����v
026010     END-IF.
026020*
026030     MOVE ����N���v   TO  ����{�p�N���v.
026040*
026050*================================================================*
026060 ����N�����擾 SECTION.
026070*
026080     MOVE ZERO  TO �v�Z����N�����v.
026090*
026100     IF �v�Z�a��v  NOT = ZERO
026110         MOVE �v�Z�a��v    TO ���|�����敪
026120         READ �����}�X�^
026130         NOT INVALID KEY
026140             MOVE ���|�J�n����N TO �v�Z����N�v
026150         END-READ
026160**
026170         IF �v�Z����N�v = ZERO
026180              MOVE  NC"�����}�X�^�ɊJ�n����N��o�^���ĉ�����" TO �A���|���b�Z�[�W
026190              CALL   "MSG001"
026200              CANCEL "MSG001"
026210              PERFORM �t�@�C����
026220              MOVE 99 TO PROGRAM-STATUS
026230              EXIT PROGRAM
026240         ELSE
026250              COMPUTE �v�Z����N�v = �v�Z����N�v + �v�Z�N�v - 1
026260              MOVE �v�Z���v TO �v�Z����v
026270              MOVE �v�Z���v TO �v�Z������v
026280         END-IF
026290     END-IF.
026300*
026310*================================================================*
026320 �������S�Ҕԍ��擾 SECTION.
026330*
026340*--------------------------------------------------------------------------
026350* ���S�Ҕԍ��� ��а 99XXXXXX�� 26XXXXXX �ɂ���B
026360* ���S�Ҕԍ��������ȊO�Ŏn�܂�(��-)���́A26XXXXXX �ɂ���BXXXXXX�́A���۔ԍ�
026370*   XXXXXX�́A�s�����}�X�^�̒��� �ی��Ҕԍ� ���g�p����B(Ͻ��ɓ��͂��Ă���)
026380*--------------------------------------------------------------------------
026390*
026400     PERFORM �����ԍ����l��.
026410*
026420     IF �����ԍ��v(1:2)  = "99"
026430*  / ��а �ԍ� /
026440         MOVE �����ԍ��v              TO �ی��Ҕԍ��v
026450         MOVE "26"                    TO �ی��Ҕԍ��v(1:2)
026460         PERFORM �ی��Ҕԍ��E�l��
026470         MOVE �ی��Ҕԍ������v        TO ��P�|�������S�Ҕԍ�
026480     ELSE
026490*  / ���� /
026500         IF �����ԍ��v(1:1)  = "0" OR "1" OR "2" OR "3" OR "4" OR
026510                               "5" OR "6" OR "7" OR "8" OR "9" OR SPACE
026520             MOVE �����ԍ��v         TO �ی��Ҕԍ��v
026530             PERFORM �ی��Ҕԍ��E�l��
026540             MOVE �ی��Ҕԍ������v   TO ��P�|�������S�Ҕԍ�
026550         ELSE
026560*  / �����ȊO /
026570             MOVE ��|�������       TO �s�|������
026580             MOVE �����ԍ��v         TO �s�|�s�����ԍ�
026590             READ �s�����}�X�^
026600             INVALID KEY
026610                 MOVE ZERO           TO ��P�|�������S�Ҕԍ�
026620             NOT INVALID KEY
026630                 MOVE SPACE          TO �ی��Ҕԍ��v
026640                 MOVE "26"           TO �ی��Ҕԍ��v(1:2)
026650                 MOVE �s�|�ی��Ҕԍ� TO �ی��Ҕԍ��v(3:6)
026660                 PERFORM �ی��Ҕԍ��E�l��
026670                 MOVE �ی��Ҕԍ������v   TO ��P�|�������S�Ҕԍ�
026680             END-READ
026690         END-IF
026700     END-IF.
026710*
026720*================================================================*
026730*================================================================*
026740 �����ԍ����l�� SECTION.
026750*
026760***** �����̕��S�Ҕԍ��̖��ʂ�SPACE����菜���āA���l�߂ɂ���B
026770     MOVE SPACE           TO  �����ԍ��v.
026780     MOVE SPACE           TO  �����ԍ����v.
026790     MOVE SPACE           TO  �����ԍ����l�߂v.
026800     MOVE ��|��p���S�Ҕԍ�����   TO  �����ԍ����v.
026810*
026820     MOVE  ZERO  TO  �J�E���^�Q.
026830     PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 10
026840          IF  �����ԍ����v�P(�J�E���^) NOT = SPACE
026850              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
026860              MOVE �����ԍ����v�P(�J�E���^)  TO  �����ԍ����l�߂v�P(�J�E���^�Q)
026870          END-IF
026880     END-PERFORM.
026890*
026900     MOVE �����ԍ����l�߂v    TO �����ԍ��v.
026910*
026920*================================================================*
026930 ������ʕϊ� SECTION.
026940*
026950     MOVE ZERO  TO ������ʕϊ���v.
026960*
026970     EVALUATE ������ʕϊ��O�v
026980     WHEN  ZERO
026990        MOVE ZERO TO ������ʕϊ���v
027000* �P��
027010     WHEN  01
027020        MOVE  4   TO ������ʕϊ���v
027030* �Ŗo
027040     WHEN  02
027050        MOVE  5   TO ������ʕϊ���v
027060* ����
027070     WHEN  03
027080        MOVE  6   TO ������ʕϊ���v
027090* �E�P
027100     WHEN  04
027110        MOVE  3   TO ������ʕϊ���v
027120* ����
027130     WHEN  05
027140        MOVE  1   TO ������ʕϊ���v
027150* �s�S����
027160     WHEN  06
027170        MOVE  2   TO ������ʕϊ���v
027180* ���܁E�s�S���܍S�k
027190     WHEN  07
027200     WHEN  08
027210        MOVE  7   TO ������ʕϊ���v
027220* �������Ȃ��i���a�j
027230     WHEN  09
027240        MOVE  9   TO ������ʕϊ���v
027250     WHEN OTHER
027260        CONTINUE
027270     END-EVALUATE.
027280*
027290*================================================================*
027300 �]�A�敪�ϊ� SECTION.
027310*
027320     MOVE ZERO  TO �]�A�ϊ���v.
027330*
027340     EVALUATE �]�A�ϊ��O�v
027350     WHEN  ZERO
027360        MOVE ZERO TO �]�A�ϊ���v
027370* ����
027380     WHEN  1
027390     WHEN  2
027400        MOVE  1   TO �]�A�ϊ���v
027410* ���~
027420     WHEN  3
027430        MOVE  2   TO �]�A�ϊ���v
027440* �]��
027450     WHEN  4
027460        MOVE  4   TO �]�A�ϊ���v
027470* �p���E���R����
027480     WHEN  5
027490     WHEN  9
027500        MOVE  3   TO �]�A�ϊ���v
027510     WHEN OTHER
027520        CONTINUE
027530     END-EVALUATE.
027540*
027550*================================================================*
027551*================================================================*
027552 ���������擾 SECTION.
027553*
027554********************************************************************
027555*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
027556*  ��: �@�A �Ƃœ]��.
027557*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
027558*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
027559********************************************************************
027560     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
027561     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
027562             UNTIL ( ���ʂb�m�s > ���ʐ��v )
027563*
027564****        IF ( ���|�������Ҕԍ�(���ʂb�m�s)  NOT = ZERO )  AND
027565        IF ( ���|�����A��(���ʂb�m�s)      NOT = ZERO )
027566*
027567           IF �J�E���^ = ZERO
027568               MOVE 1   TO  �J�E���^ �J�E���^�Q
027569               MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
027570               MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
027571               MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
027572           ELSE
027573              IF ( ���|�������Ҕԍ�(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
027574                 ( ���|�����A��(���ʂb�m�s)      = �����A�Ԃb�v     )
027575                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
027576                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
027577              ELSE
027578                 COMPUTE �J�E���^ = �J�E���^  +  1
027579                 MOVE 1   TO  �J�E���^�Q
027580                 MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
027581                 MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
027582                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
027583              END-IF
027584           END-IF
027585        END-IF
027586     END-PERFORM.
027587**************************************************************************
027588*  ���������}�X�^��蕶�͎擾
027589**************************************************************************
027590     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
027591     PERFORM VARYING �J�E���^ FROM 1 BY 1
027592             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
027593** ���ۂ� �敪 01
027594         MOVE 01                        TO �����|�敪�R�[�h
027595         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
027596         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
027597         READ ���������e
027598         NOT INVALID KEY
027599             INITIALIZE ���������v�s
027600             MOVE �����|���������b�l(1) TO  ���������P�v�s
027601             MOVE �����|���������b�l(2) TO  ���������Q�v�s
027602             MOVE �����|���������b�l(3) TO  ���������R�v�s
027603             MOVE �����|���������b�l(4) TO  ���������S�v�s
027604             MOVE �����|���������b�l(5) TO  ���������T�v�s
027605             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
027606                     UNTIL ( �J�E���^�Q > 9 )  OR 
027607                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
027608                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
027609                WHEN 1
027610                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
027611                WHEN 2
027612                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
027613                WHEN 3
027614                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
027615                WHEN 4
027616                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
027617                WHEN 5
027618                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
027619                WHEN OTHER
027620                   CONTINUE
027621                END-EVALUATE
027622             END-PERFORM
027623*
027633             IF �����|�����������͋敪 = 1
027634                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
027635                        ���������P�v�s  DELIMITED BY SIZE
027636                        ���������Q�v�s  DELIMITED BY SIZE
027637                        ���������R�v�s  DELIMITED BY SIZE
027638                        ���������S�v�s  DELIMITED BY SIZE
027639                        ���������T�v�s  DELIMITED BY SIZE
027640                        INTO �����������e�����v(�J�E���^)
027641                 END-STRING
027642             ELSE
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
027651             END-IF
027652*
027653         END-READ
027654     END-PERFORM.
027655*
027656     PERFORM ���������Z�b�g.
027657*
027658*================================================================*
027659 ���������Z�b�g SECTION.
027660*
027661**************************************************************************
027662*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
027663**************************************************************************
027664     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
027665     PERFORM VARYING �J�E���^ FROM 1 BY 1
027666             UNTIL ( �J�E���^ > 9 )  OR ( �����������e�����v(�J�E���^) = SPACE )
027667*
027668          INITIALIZE �����������e�����w�v
027669          MOVE �����������e�����v(�J�E���^)   TO �����������e�����w�v
027670          IF  �����������e�P�w�v  NOT = SPACE
027671              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
027672              MOVE �����������e�P�w�v  TO ���������v(�J�E���^�Q)
027673          END-IF
027674          IF  �����������e�Q�w�v  NOT = SPACE
027675              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
027676              MOVE �����������e�Q�w�v  TO ���������v(�J�E���^�Q)
027677          END-IF
027678          IF  �����������e�R�w�v  NOT = SPACE
027679              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
027680              MOVE �����������e�R�w�v  TO ���������v(�J�E���^�Q)
027681          END-IF
034690          IF  �����������e�S�w�v  NOT = SPACE
034700              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
034710              MOVE �����������e�S�w�v  TO ���������v(�J�E���^�Q)
034720          END-IF
027682*
027683     END-PERFORM.
027684*
027685*================================================================*
027686 �������R���擾 SECTION.
027687*
027688* �������R���擾�� "CHOUBUN" ���Ă�. 
027689     MOVE  SPACE TO  �A�����|�L�[.
027690     INITIALIZE      �A�����|�L�[.
027691     MOVE �{�p�a��v�q  TO  �A�����|�{�p�a��.
027692     MOVE �{�p�N�v�q    TO  �A�����|�{�p�N.
027693     MOVE �{�p���v�q    TO  �A�����|�{�p��.
027694     MOVE ���Ҕԍ��v�q  TO  �A�����|���Ҕԍ�.
027695     MOVE �}�Ԃv�q      TO  �A�����|�}��.
027696** ���ڗp��56��
027697     MOVE 56            TO  �A�����|������.
027698*
027699     CALL   "CHOUBUN".
027700     CANCEL "CHOUBUN".
027701*
027702*================================================================*
027703 �������Z�܂Ƃߔ��� SECTION.
027704*---------------------------------------------------------------------------*
027705* �s�����}�X�^��ǂ݁A���Z�܂Ƃߋ敪���P�ł��A�{�̕ی������ہE�ސE
027706* �̎��́A�t���OYES (���z���������݂ň󎚁j
027707*�i��F���l�s�̏�Q�́A�{�̕ی��i���یn�j�̃��Z�v�g�P���Ő����A�������Z�͂Ȃ��j
027708*---------------------------------------------------------------------------*
027709*
027710     MOVE SPACE TO �������Z�܂Ƃ߃t���O.
027711*
027712*     MOVE ��|�������           TO �s�|������.
027713*     MOVE ��|��p���S�Ҕԍ����� TO �s�|�s�����ԍ�.
027714*     READ �s�����}�X�^
027715*     NOT INVALID KEY
027716*         IF �s�|���Z�܂Ƃߋ敪 = 1 
027717*            IF (( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(3:1) NOT = "3" )) OR
027718*               ( ��|�ی���� = 08 ) 
027719*                MOVE "YES" TO �������Z�܂Ƃ߃t���O
027720*            END-IF
027721*         END-IF
027722*     END-READ.
027723*
027724** / CALL JRECEOFF /
027725*     IF �������Z�܂Ƃ߃t���O = SPACE
027726*        INITIALIZE �A���Z�܂Ƃ߁|�L�[
027727*        MOVE ��|�{�p�a�� TO �A���Z�܂Ƃ߁|�{�p�a��
027728*        MOVE ��|�{�p�N   TO �A���Z�܂Ƃ߁|�{�p�N
027729*        MOVE ��|�{�p��   TO �A���Z�܂Ƃ߁|�{�p��
027730*        MOVE ��|���Ҕԍ� TO �A���Z�܂Ƃ߁|���Ҕԍ�
027731*        MOVE ��|�}��     TO �A���Z�܂Ƃ߁|�}��
027732*       1:�������Z�v�g�Ȃ��̖{�̂܂Ƃ߂̔���
027733*        MOVE 1            TO �A���Z�܂Ƃ߁|����敪
027734*        CALL   "JRECEOFF"
027735*        CANCEL "JRECEOFF"
027736*
027737*        IF �A���Z�܂Ƃ߁|���茋�� = 1
027738*           MOVE "YES" TO �������Z�܂Ƃ߃t���O
027739*        END-IF
027740*     END-IF.
           IF ( ���Z�|�{�̂܂Ƃߋ敪 = 1 )
037165           MOVE "YES" TO �������Z�܂Ƃ߃t���O
037167     END-IF.
027741*
027742*================================================================*
       ��v�f�[�^�e�W�v SECTION.
      *
           MOVE ZERO         TO ��P�|�ꕔ���S��.
007680     MOVE ��|�{�p�a�� TO ��|�{�p�a��.
007690     MOVE ��|�{�p�N   TO ��|�{�p�N.
007700     MOVE ��|�{�p��   TO ��|�{�p��.
007710     MOVE ZERO         TO ��|�{�p��.
007720     MOVE ��|���Ҕԍ� TO ��|���Ҕԍ�.
007730     MOVE ��|�}��     TO ��|�}��.
004460     START ��v�f�[�^�e     KEY IS >= ��|���҃R�[�h
004470                                      ��|�{�p�a��N����
004480     END-START.
004490     IF ��ԃL�[ = "00"
004491         MOVE SPACE TO �I���t���O�S
004500         PERFORM ��v�f�[�^�e�Ǎ�
004510         PERFORM VARYING �J�E���^ FROM 1 BY 1
004520                 UNTIL ( ��|���Ҕԍ�     NOT = ��|���Ҕԍ�    ) OR
004530                       ( ��|�}��         NOT = ��|�}��        ) OR
004540                       ( ��|�{�p�a��N�� NOT = ��|�{�p�a��N��) OR
004550                       ( �I���t���O�S         = "YES" )
                   COMPUTE ��P�|�ꕔ���S�� = ��|�ꕔ���S�� + ��P�|�ꕔ���S��
004500             PERFORM ��v�f�[�^�e�Ǎ�
               END-PERFORM
007930*
007770     END-IF.
      *
027743*================================================================*
003830 ��v�f�[�^�e�Ǎ� SECTION.
003840*
003850     READ ��v�f�[�^�e NEXT
003860     AT END
003870         MOVE "YES"  TO �I���t���O�S
003880     END-READ.
003890*================================================================*
027744******************************************************************
027745 END PROGRAM YJB101.
027746******************************************************************
