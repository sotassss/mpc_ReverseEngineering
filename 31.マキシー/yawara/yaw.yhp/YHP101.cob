000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHP101.
000060 AUTHOR.                 ���c�@���a
000070*
000080*----------------------------------------------------------------*
000090*      ��oFPD�쐬�y�ް��쐬�z�_����޳��95��
000100*
000110* �� �����N��Ver�̂�. (���я��́A�����\581�Ɠ����j
000120*�@�@��ʂ𑍊��\�ƈꏏ�ɂ���
000130*
000140*      MED = YHP100G 
000150*----------------------------------------------------------------*
000160 DATE-WRITTEN.           2012-09-20
000170 DATE-COMPILED.          2012-09-20
000180*----------------------------------------------------------------*
000190******************************************************************
000200*            ENVIRONMENT         DIVISION                        *
000210******************************************************************
000220 ENVIRONMENT             DIVISION.
000230 CONFIGURATION           SECTION.
000240 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000250 OBJECT-COMPUTER.        FMV-DESKPOWER.
000260 SPECIAL-NAMES.          CONSOLE  IS  CONS
000270                         SYSERR   IS  MSGBOX.
000280 INPUT-OUTPUT            SECTION.
000290 FILE-CONTROL.
000300     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000310                             ORGANIZATION             IS  INDEXED
000320                             ACCESS MODE              IS  DYNAMIC
000330                             RECORD KEY               IS  ���|����敪
000340                             FILE STATUS              IS  ��ԃL�[
000350                             LOCK        MODE         IS  AUTOMATIC.
000360     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000370                             ORGANIZATION             IS  INDEXED
000380                             ACCESS MODE              IS  DYNAMIC
000390                             RECORD KEY               IS  ���|�����敪
000400                             FILE STATUS              IS  ��ԃL�[
000410                             LOCK        MODE         IS  AUTOMATIC.
000420     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS  ���|�敪�R�[�h
000460                                                          ���|���̃R�[�h
000470                             FILE STATUS              IS  ��ԃL�[
000480                             LOCK        MODE         IS  AUTOMATIC.
000490     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000500                             ORGANIZATION             IS  INDEXED
000510                             ACCESS MODE              IS  DYNAMIC
000520                             RECORD KEY               IS �{��|�{�p���ԍ�
000530                             FILE STATUS              IS  ��ԃL�[
000540                             LOCK        MODE         IS  AUTOMATIC.
000550     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
000560                             ORGANIZATION             IS  INDEXED
000570                             ACCESS MODE              IS  DYNAMIC
000580                             RECORD KEY           IS �{�L�|�{�p�a��N����
000590                                                     �{�L�|���҃R�[�h
000600                             ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
000610                                                     �{�L�|�{�p�a��N����
000620                             FILE STATUS              IS  ��ԃL�[
000630                             LOCK        MODE         IS  AUTOMATIC.
000640     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000650                             ORGANIZATION             IS  INDEXED
000660                             ACCESS MODE              IS  DYNAMIC
000670                             RECORD KEY               IS  ��|�{�p�a��N��
000680                                                          ��|���҃R�[�h
000690                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000700                                                          ��|���҃J�i
000710                                                          ��|���҃R�[�h
000720                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000730                                                          ��|�{�p�a��N��
000740                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000750                                                          ��|�ی����
000760                                                          ��|�ی��Ҕԍ�
000770                                                          ��|���҃R�[�h
000780                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000790                                                          ��|������
000800                                                          ��|��p���S�Ҕԍ�
000810                                                          ��|���҃R�[�h
000820                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000830                                                          ��|�������
000840                                                          ��|��p���S�Ҕԍ�����
000850                                                          ��|���҃R�[�h
000860                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
000870                                                          ��|�{�p�a��N��
000880                                                          ��|���҃R�[�h
000890                             FILE STATUS              IS  ��ԃL�[
000900                             LOCK        MODE         IS  AUTOMATIC.
000910     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000920                             ORGANIZATION             IS  INDEXED
000930                             ACCESS MODE              IS  DYNAMIC
000940                             RECORD KEY               IS ���|�{�p�a��N��
000950                                                         ���|���҃R�[�h
000960                             ALTERNATE RECORD KEY     IS ���|���҃R�[�h
000970                                                         ���|�{�p�a��N��
000980                             FILE STATUS              IS  ��ԃL�[
000990                             LOCK        MODE         IS  AUTOMATIC.
001000     SELECT  �o�߃}�X�^      ASSIGN      TO        KEIKAL
001001                             ORGANIZATION             IS  INDEXED
001002                             ACCESS MODE              IS  DYNAMIC
001003                             RECORD KEY               IS  �o�|�敪�R�[�h
001004                                                          �o�|�o�߃R�[�h
001005                             FILE STATUS              IS  ��ԃL�[
001006                             LOCK        MODE         IS  AUTOMATIC.
001007     SELECT  ���������e      ASSIGN      TO        HUGEINL
001010                             ORGANIZATION             IS  INDEXED
001020                             ACCESS MODE              IS  DYNAMIC
001030                             RECORD KEY               IS  �����|�敪�R�[�h
001040                                                          �����|���������R�[�h
001050                             FILE STATUS              IS  ��ԃL�[
001060                             LOCK        MODE         IS  AUTOMATIC.
001070     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
001080                             ORGANIZATION             IS  INDEXED
001090                             ACCESS MODE              IS  DYNAMIC
001100                             RECORD KEY               IS  �s�|������
001110                                                          �s�|�s�����ԍ�
001120                             ALTERNATE RECORD KEY     IS  �s�|������
001130                                                          �s�|�s��������
001140                                                          �s�|�s�����ԍ�
001150                             FILE STATUS              IS  ��ԃL�[
001160                             LOCK        MODE         IS  AUTOMATIC.
001170     SELECT  ���Z�v�g�e      ASSIGN      TO        RECEPTL
001180                             ORGANIZATION             IS  INDEXED
001190                             ACCESS MODE              IS  DYNAMIC
001200                             RECORD KEY               IS  ���Z�|�{�p�a��N��
001210                                                          ���Z�|���҃R�[�h
001220                                                          ���Z�|���Z���
001230                             ALTERNATE RECORD KEY     IS  ���Z�|���҃R�[�h
001240                                                          ���Z�|�{�p�a��N��
001250                                                          ���Z�|���Z���
001260                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
001270                                                          ���Z�|�{�p�a��N��
001280                                                          ���Z�|���҃R�[�h
001290                                                          ���Z�|���Z���
001300                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
001310                                                          ���Z�|���Z���
001320                                                          ���Z�|�����ی��Ҕԍ�
001330                                                          ���Z�|���҃R�[�h
001340                                                          ���Z�|�{�p�a��N��
001350                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
001360                                                          ���Z�|�����ی��Ҕԍ�
001370                                                          ���Z�|���҃R�[�h
001380                                                          ���Z�|���Z���
001390                                                          ���Z�|�{�p�a��N��
001400                             FILE STATUS              IS  ��ԃL�[
001410                             LOCK        MODE         IS  AUTOMATIC.
001420     SELECT  �v�Z�}�X�^      ASSIGN      TO        KEISANL
001421                             ORGANIZATION             IS  INDEXED
001422                             ACCESS MODE              IS  DYNAMIC
001423                             RECORD KEY               IS  �v�|����敪
001424                                                          �v�|�J�n�a��N��
001425                             FILE STATUS              IS  ��ԃL�[.
001428     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001430                             ORGANIZATION             IS  SEQUENTIAL
001440                             ACCESS                   IS  SEQUENTIAL
001450                             FILE        STATUS       IS  ��ԃL�[
001460                             LOCK        MODE         IS  AUTOMATIC.
001478*
001480*  �����\�Ɠ����ی��Ҕԍ���̧��
001490     SELECT  ��ƃt�@�C���R  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5803L.DAT"
001500                             ORGANIZATION             IS  INDEXED
001510                             ACCESS                   IS  DYNAMIC
001520                             RECORD      KEY          IS  ��R�|�����a��N��
001530                                                          ��R�|�����敪
001540                                                          ��R�|�ی��Ҕԍ�
001550                                                          ��R�|�{�l�Ƒ��敪
001560                                                          ��R�|�{�p�a��N��
001570                                                          ��R�|��ی��҃J�i
001580                                                          ��R�|���҃R�[�h
001590                                                          ��R�|�e�q�敪
001600                             FILE        STATUS       IS  ��ԃL�[
001610                             LOCK        MODE         IS  AUTOMATIC.
001620*
001630******************************************************************
001640*                      DATA DIVISION                             *
001650******************************************************************
001660 DATA                    DIVISION.
001670 FILE                    SECTION.
001680*                           �m�q�k��  �Q�T�U�n
001690 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001700     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001710*                           �m�q�k��  �P�Q�W�n
001720 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001730     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001740*                           �m�q�k��  �P�Q�W�n
001750 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001760     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001770*
001780 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
001790     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001800*                           �m�q�k��  �Q�T�U�n
001810 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
001820     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
001830*                           �m�q�k��  �R�Q�O�n
001840 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001850     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001860*                           �m�q�k��  �P�Q�W�n
001870 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001880     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001890*                           �m�q�k��  �P�Q�W�n
001891 FD  �o�߃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001892     COPY KEIKA          OF  XFDLIB  JOINING   �o   AS  PREFIX.
001893*                           �m�q�k��  �P�Q�W�n
001900 FD  ���������e         BLOCK   CONTAINS   1   RECORDS.
001910     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
001920*                           �m�q�k��  �Q�T�U�n
001930 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001940     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
001950*                          �m�q�k��  �P�T�R�U�n
001960 FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
001970     COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001980*                           �m�q�k��  �Q�T�U�n
001981 FD  �v�Z�}�X�^          BLOCK   CONTAINS   1   RECORDS.
001982     COPY KEISAN          OF  XFDLIB  JOINING   �v   AS  PREFIX.
001983     COPY KEISANA         OF  XFDLIB  JOINING   �v�` AS  PREFIX.
001984**
002867 FD  ��ƃt�@�C���P RECORD  CONTAINS 1920 CHARACTERS.
002868 01  ��P�|���R�[�h.
002869*   / �w�b�_���͎g�p���Ȃ� /
002870     03  ��P�|���R�[�h�w�b�_.
002871         05  ��P�|�����a��N���L�[.
002872             07  ��P�|�����a��            PIC 9.
002873             07  ��P�|�����N              PIC 9(2).
002874             07  ��P�|������              PIC 9(2).
002875         05  ��P�|�{�p�a��N���L�[.
002876             07  ��P�|�{�p�a��            PIC 9.
002877             07  ��P�|�{�p�N              PIC 9(2).
002878             07  ��P�|�{�p��              PIC 9(2).
002879         05  ��P�|�ی��敪�L�[            PIC 9.
002880         05  ��P�|�ی��Ҕԍ��L�[          PIC 9(8).
002881         05  ��P�|�{�l�Ƒ��敪�L�[        PIC 9.
002882         05  ��P�|��ی��҃J�i�L�[        PIC X(20).
002883         05  ��P�|���҃R�[�h�L�[.
002884             07 ��P�|���Ҕԍ��L�[         PIC 9(6).
002885             07 ��P�|�}��                 PIC X(1).
002886     03  ��P�|���R�[�h�f�[�^.
002887         05  ��P�|�����N��                PIC 9(6).
002888         05  ��P�|�{�p�N��                PIC 9(6).
002889         05  ��P�|����ԍ�                PIC 9(7).
002890         05  ��P�|�o�^�L���ԍ�            PIC X(11).
002891         05  ��P�|�ی��Ҕԍ�              PIC X(8).
002892         05  ��P�|�L��                    PIC X(30).
002893         05  ��P�|�ԍ�                    PIC X(16).
002894         05  ��P�|��Ï����敪            PIC 9.
002895         05  ��P�|�������S�Ҕԍ�          PIC X(8).
002896         05  ��P�|�����󋋎Ҕԍ�          PIC X(16).
002897         05  ��P�|�������S�Ҕԍ��Q        PIC X(8).
002898         05  ��P�|�����󋋎Ҕԍ��Q        PIC X(16).
002899         05  ��P�|�ی���ʋ敪            PIC 9.
002900         05  ��P�|�P���敪                PIC 9.
002901         05  ��P�|�{�Ƌ敪                PIC 9.
002902         05  ��P�|���t����                PIC 9(2).
002903         05  ��P�|�{�l�Ƒ��敪            PIC 9.
002904         05  ��P�|��ی��҃J�i            PIC X(25).
002905         05  ��P�|��ی��Ҏ���            PIC X(30).
002906         05  ��P�|���҃J�i                PIC X(25).
002907         05  ��P�|���Ҏ���                PIC X(30).
002908         05  ��P�|���Ґ���                PIC 9.
002909         05  ��P�|���Ґ��N����            PIC 9(8).
002910         05  ��P�|���v���z                PIC 9(6).
002911         05  ��P�|�ꕔ���S��              PIC 9(6).
002912         05  ��P�|�������z                PIC 9(6).
002913         05  ��P�|����S���z            PIC 9(6).
002914         05  ��P�|��������z            PIC 9(6).
002915         05  ��P�|�S�̎�����              PIC 9(2).
002916         05  ��P�|���ʐ�                  PIC 9.
002917         05  ��P�|�Đ����敪              PIC 9.
002918         05  ��P�|�Ǝҋ敪                PIC 9(2).
002919         05  ��P�|����ҋ敪              PIC 9.
002920         05  ��P�|���Ҕԍ�                PIC 9(5).
      */�^����Òǉ�������/20180607
               05 ��P�|�^����×���           PIC 9(1).
               05 ��P�|�^����×�               PIC 9(5).
      */���׏����s�̐����Z�ǉ�������/20221020
             05 ��P�|���׏����s��          PIC 9(1).
             05 ��P�|���׏����s              PIC 9(3).
             05 ��P�|���׏����s����          PIC 9(4).
001598*       05 ��P�|�\��                    PIC X(57).
001598*       05 ��P�|�\��                    PIC X(48).
001598       05 ��P�|�\��                    PIC X(40).
      */���׏����s�̐����Z�ǉ�������/20221020
      */�^����Òǉ�������/20180607
002922*
002923         05  ��P�|�����f�[�^  OCCURS 5.
002924             07  ��P�|�����敪            PIC 9.
002925             07  ��P�|������              PIC X(32).
002926             07  ��P�|�����N����          PIC 9(8).
002927             07  ��P�|�����N����          PIC 9(8).
002928             07  ��P�|�{�p�J�n�N����      PIC 9(8).
002929             07  ��P�|�{�p�I���N����      PIC 9(8).
002930             07  ��P�|������              PIC 9(2).
002931             07  ��P�|�]�A�敪            PIC 9.
002932             07  ��P�|�����Œ�{�É�    PIC 9.
002933             07  ��P�|�����Œ�{�×�      PIC 9(5).
002934*
002935         05  ��P�|�V�K�敪                PIC 9.
002936         05  ��P�|�p���敪                PIC 9.
002937         05  ��P�|�{�p��                  PIC X(31).
002938         05  ��P�|������                PIC 9.
002939         05  ��P�|������                  PIC 9(5).
002940         05  ��P�|�����x�����Z��        PIC 9.
002941         05  ��P�|�����[����Z��        PIC 9.
002942         05  ��P�|�������ԊO���Z��      PIC 9.
002943         05  ��P�|�������Z                PIC 9(5).
002944         05  ��P�|���k�x����            PIC 9.
002945         05  ��P�|���k�x����              PIC 9(5).
002946
002947         05  ��P�|�Č���                PIC 9.
002948         05  ��P�|�Č���                  PIC 9(5).
002949         05  ��P�|���Ë���                PIC 9(3).
002950         05  ��P�|���É�                PIC 9(2).
002951         05  ��P�|���×�                  PIC 9(5).
002952         05  ��P�|��ԉ��Z���É�        PIC 9.
002953         05  ��P�|��H���Z���É�        PIC 9.
002954         05  ��P�|�\���J����Z���É�    PIC 9.
002955         05  ��P�|���É��Z                PIC 9(5).
      */�������q�ύX������/20180611
000561         05 ��P�|�������q��             PIC 9.
000561         05 ��P�|�_�~�[                   PIC X(2).
002956*         05  ��P�|�������q���          PIC 9.
002957*         05  ��P�|�������q����          PIC 9.
002958*         05  ��P�|�������q����          PIC 9.
      */�������q�ύX������/20180611
002959         05  ��P�|�������q���Z            PIC 9(5).
002960         05  ��P�|���񋟗���          PIC 9.
002961         05  ��P�|���񋟗�              PIC 9(5).
002962*
002963         05  ��P�|�������ʃf�[�^  OCCURS 6.
002964             07  ��P�|�����J�n����        PIC 9(4).
002965             07  ��P�|��É�            PIC 9(2).
002966             07  ��P�|��×�              PIC 9(5).
002967             07  ��P�|��㪖@��          PIC 9.
002968             07  ��P�|��㪖@��            PIC 9(5).
002969             07  ��P�|��㪖@��          PIC 9(2).
002970             07  ��P�|��㪖@��            PIC 9(5).
002971             07  ��P�|�d�É�            PIC 9(2).
002972             07  ��P�|�d�×�              PIC 9(5).
002973             07  ��P�|�����ʒ�����        PIC 9(2).
002974             07  ��P�|�����ʒ����z        PIC 9(5).
002975             07  ��P�|����������          PIC 9(2).
002976             07  ��P�|�����v              PIC 9(5).
002977*
002978         05  ��P�|��ی��ҏZ��            PIC X(60).
002979         05  ��P�|��������                PIC X(200).
002980         05  ��P�|�o��                    PIC X(50).
002981         05  ��P�|�������R                PIC X(400).
002982         05  ��P�|���s����                PIC X(2).
002983         05  FILLER                        PIC X(73).
002984*
002985***
002986* �ی��Ҕԍ����t�@�C��
002987 FD  ��ƃt�@�C���R RECORD  CONTAINS 64 CHARACTERS.
002988 01  ��R�|���R�[�h.
002989     03  ��R�|���R�[�h�L�[.
002990         05  ��R�|�����a��N��.
002991             07  ��R�|�����a��            PIC 9.
002992             07  ��R�|�����N              PIC 9(2).
002993             07  ��R�|������              PIC 9(2).
002994         05  ��R�|�����敪                PIC 9.
002995         05  ��R�|�ی��Ҕԍ�              PIC 9(8).
002996         05  ��R�|�{�l�Ƒ��敪            PIC 9.
002997         05  ��R�|�{�p�a��N��.
002998             07  ��R�|�{�p�a��            PIC 9.
002999             07  ��R�|�{�p�N              PIC 9(2).
003000             07  ��R�|�{�p��              PIC 9(2).
003010         05  ��R�|��ی��҃J�i            PIC X(20).
003020         05  ��R�|���҃R�[�h.
003030             07 ��R�|���Ҕԍ�             PIC 9(6).
003040             07 ��R�|�}��                 PIC X(1).
003050         05  ��R�|�e�q�敪                PIC 9.
003060     03  ��R�|���R�[�h�f�[�^.
003070         05  FILLER                        PIC X(16).
003080*
003090*----------------------------------------------------------------*
003100******************************************************************
003110*                WORKING-STORAGE SECTION                         *
003120******************************************************************
003130 WORKING-STORAGE         SECTION.
003140 01 �L�[����                           PIC X    VALUE SPACE.
003150 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
003160 01 �����t���O                         PIC X(3) VALUE SPACE.
003170 01 �I���t���O                         PIC X(3) VALUE SPACE.
003180 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
003190 01 ���s�L�[�v                         PIC X(3)  VALUE SPACE.
003200 01 �{�p�L�^�L�v                       PIC X(3) VALUE SPACE.
003210 01 �t�@�C����                         PIC N(8) VALUE SPACE.
003220*
003230 01 �ی���ʂv�q                       PIC 9(2) VALUE ZERO.
003240 01 ���҃R�[�h�v�q.
003250    03 ���Ҕԍ��v�q                    PIC 9(6) VALUE ZERO.
003260    03 �}�Ԃv�q                        PIC X    VALUE SPACE.
003270*
003280 01 �_���t�ԍ��v                       PIC X(11)  VALUE SPACE.
003281 01 ����`���v�q                       PIC 9    VALUE ZERO.
003290 01 �ی��Ҕԍ��v�q                     PIC X(10) VALUE SPACE.
003302 01 �s�����ԍ��v                     PIC X(10) VALUE SPACE.
003303 01 �󋋎Ҕԍ��v                     PIC X(10) VALUE SPACE.
003304 01 ���Z�v�g��ނv�q                   PIC X(4) VALUE SPACE.
003310 01 �{�l�Ƒ��敪�v�q                   PIC 9    VALUE ZERO.
003320 01 �����v                             PIC N(2) VALUE SPACE.
003330 01 �{�p�a��N���v�q.
003340    03 �{�p�a��v�q                    PIC 9    VALUE ZERO.
003350    03 �{�p�N�v�q                      PIC 9(2) VALUE ZERO.
003360    03 �{�p���v�q                      PIC 9(2) VALUE ZERO.
003370 01 �����a��N���v�q.
003380    03 �����a��v�q                    PIC 9    VALUE ZERO.
003390    03 �����N�v�q                      PIC 9(2) VALUE ZERO.
003400    03 �������v�q                      PIC 9(2) VALUE ZERO.
003410
003411 01 ��Ï����敪�v                     PIC 9    VALUE ZERO.
003422 01 �ی���ʋ敪�v                     PIC 9    VALUE ZERO.
003423 01 �P���敪�v                         PIC 9    VALUE ZERO.
003424 01 �{�Ƌ敪�v                         PIC 9    VALUE ZERO.
003425 01 ���t�����v                         PIC 9(2) VALUE ZERO.
003426 01 �{�l�Ƒ��敪�v                     PIC 9    VALUE ZERO.
003427 01 �S�̎������v                       PIC 9(2) VALUE ZERO.
003428 01 �Đ����敪�v                       PIC 9    VALUE ZERO.
003430 01 �Ǝҋ敪�v                         PIC 9(2) VALUE ZERO.
003431 01 �Q���ʖڒ������v                   PIC 9(3) VALUE ZERO.
003432 01 �R���ʖڒ������v                   PIC 9(3) VALUE ZERO.
003434**
003435 01 �A�Ԃv                             PIC 9(4) VALUE ZERO.
003436 01 �����t���O                         PIC X(3) VALUE SPACE.
003440 01 �������̂v                         PIC N(16) VALUE SPACE.
003451 01 ������ʕϊ��O�v                   PIC 9(2)  VALUE ZERO.
003460 01 ������ʕϊ���v                   PIC 9     VALUE ZERO.
003470 01 �]�A�ϊ��O�v                       PIC 9     VALUE ZERO.
003480 01 �]�A�ϊ���v                       PIC 9     VALUE ZERO.
003492 01 �ی���ʕϊ��O�v                   PIC 9     VALUE ZERO.
003493 01 �ی���ʕϊ���v                   PIC 9     VALUE ZERO.
003494
003495**
003500 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
003510 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
003520 01 �J�E���^�Q                         PIC 9(3)  VALUE ZERO.
003530 01 �J�E���^�R                         PIC 9(2)  VALUE ZERO.
003540 01 ���s                               PIC X(2)  VALUE X"0D0A" GLOBAL.
003543 01 �S�p��                           PIC X(2)  VALUE X"8140".
003550 01 ���p��                           PIC X(2)  VALUE X"2020".
003560
003563 01 ���{��ϊ��v�w.
003564    03 ���{��ϊ��v�m                  PIC N(50) VALUE SPACE. 
003565**
003570 01 �����v.
003580    03 �S�p�����v                      PIC X(30) VALUE SPACE.
003590** �G���[���b�Z�[�W�p
003600 01 �G���[���b�Z�[�W�v.
003610    03 �G���[���҃R�[�h�v              PIC X(7) VALUE SPACE.
003620    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
003630    03 �G���[�ی���ʂv                PIC X(2) VALUE SPACE.
003640    03 FILLER                          PIC X(10) VALUE SPACE.
003650** �ی��Ҕԍ��E�l�ߗp
003660 01 �ی��Ҕԍ��v�s.
003670    03 �ی��Ҕԍ����l�߂v.
003680      05 �ی��Ҕԍ����l�߂v�P          PIC X OCCURS 8 VALUE SPACE.
003690    03 �ی��Ҕԍ��E�l�߂v.
003700      05 �ی��Ҕԍ��E�l�߂v�P          PIC X OCCURS 8 VALUE ZERO.
003710    03 �ی��Ҕԍ������v                PIC 9(8)  VALUE ZERO.
003720    03 �ی��Ҕԍ��v                    PIC X(8)  VALUE SPACE.
003730** ����ԍ��E�l�ߗp
003740 01 ����ԍ��v�s.
003750    03 ����ԍ����l�߂v.
003760      05 ����ԍ����l�߂v�P            PIC X OCCURS 7 VALUE SPACE.
003770    03 ����ԍ��E�l�߂v.
003780      05 ����ԍ��E�l�߂v�P            PIC X OCCURS 7 VALUE ZERO.
003790    03 ����ԍ������v                  PIC 9(7)  VALUE ZERO.
003800    03 ����ԍ��v                      PIC X(7)  VALUE SPACE.
003810** ������t���[�N�p
003820 01 ����N���v.
003830    03 ����N�v                        PIC 9(4) VALUE ZERO.
003840    03 ����v                        PIC 9(2) VALUE ZERO.
003850** ������N���p
003860 01 ������N���v.
003870    03 ������N�v                    PIC 9(4) VALUE ZERO.
003880    03 ��������v                    PIC 9(2) VALUE ZERO.
003890** ����{�p�N���p
003900 01 ����{�p�N���v.
003910    03 ����{�p�N�v                    PIC 9(4) VALUE ZERO.
003920    03 ����{�p���v                    PIC 9(2) VALUE ZERO.
003930** �L�����l�ߗp
003940 01 �L���v�s.
003950    03 �L�����v.
003960      05 �L�����v�P                    PIC N OCCURS 12 VALUE SPACE.
003970    03 �L�����l�߂v.
003980      05 �L�����l�߂v�P                PIC N OCCURS 12 VALUE SPACE.
003990    03 �L�����w�v.
004000      05 �L�����w�v�P                  PIC X OCCURS 24 VALUE SPACE.
004010    03 �L�����l�߂w�v.
004020      05 �L�����l�߂w�v�P              PIC X OCCURS 24 VALUE SPACE.
004030    03 �L���v.
004040      05 �L���m�v                      PIC N(12) VALUE SPACE.
004050    03 �L���o�v.
004060      05 �L���o�m�v                    PIC X(24) VALUE SPACE.
004070** �������S�Ҕԍ����l�ߗp
004080 01 �����ԍ��v�s.
004090    03 �����ԍ����v.
004100      05 �����ԍ����v�P                PIC X OCCURS 10 VALUE SPACE.
004110    03 �����ԍ����l�߂v.
004120      05 �����ԍ����l�߂v�P            PIC X OCCURS 10 VALUE SPACE.
004130    03 �����ԍ��v                      PIC X(10) VALUE SPACE.
004140*
004150** ����N�������[�N�p
004160 01 �v�Z����N�����v.
004170    03 �v�Z����N�v                    PIC 9(4) VALUE ZERO.
004180    03 �v�Z����v                    PIC 9(2) VALUE ZERO.
004190    03 �v�Z������v                    PIC 9(2) VALUE ZERO.
004200 01 �v�Z�a��N�����v.
004210    03 �v�Z�a��v                      PIC 9 VALUE ZERO.
004220    03 �v�Z�N�v                        PIC 9(2) VALUE ZERO.
004230    03 �v�Z���v                        PIC 9(2) VALUE ZERO.
004240    03 �v�Z���v                        PIC 9(2) VALUE ZERO.
004250** �}�Ԕ���p
004260 01 �J�n�f�Ó��蓮�敪�v               PIC 9    VALUE ZERO.
004270*
004280* �I�����ޔ�p
004290 01 �I���N�����v�s.
004300    03 �I���a��v�s                    PIC 9     VALUE ZERO.
004310    03 �I���N�v�s                      PIC 9(2)  VALUE ZERO.
004320    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
004330    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
004340* �������ޔ�p
004350 01 �����N�����v�s.
004360    03 �����a��v�s                    PIC 9     VALUE ZERO.
004370    03 �����N�v�s                      PIC 9(2)  VALUE ZERO.
004380    03 �������v�s                      PIC 9(2)  VALUE ZERO.
004390    03 �������v�s                      PIC 9(2)  VALUE ZERO.
004400*
004410* �A�v�̋��z�ޔ�p
004420 01 �A�v���z�v.
004430    03  ��p�z�v                   PIC 9(6) VALUE ZERO.
004440    03  ���S�z�v                   PIC 9(6) VALUE ZERO.
004450    03  �����z�v                   PIC 9(6) VALUE ZERO.
004460    03  ��p�z�V�l�v               PIC 9(6) VALUE ZERO.
004470    03  ���S�z�V�l�v               PIC 9(6) VALUE ZERO.
004480    03  �����z�V�l�v               PIC 9(6) VALUE ZERO.
004490    03  ��p�z�����v               PIC 9(6) VALUE ZERO.
004500    03  ���S�z�����v               PIC 9(5) VALUE ZERO.
004510    03  �����z�����v               PIC 9(5) VALUE ZERO.
004520    03  ���S���v                   PIC 9(3) VALUE ZERO.
004530*
004540* ���������p
004550 01 ���������v�s.
004560    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
004570    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
004580    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
004590    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
004600    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
004610    03 ���������i���o�[�v�s.
004620       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
004630    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
004640 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
004650 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
004660 01 ���������s�a�k.
004670    03 ���������R�[�h�s�a�k            OCCURS 9.
004680       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
004690       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
004700       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
004710 01 �����������e�v.
004720    03 �����������e�����v              PIC X(318) OCCURS 9 VALUE SPACE.
004730    03 �����������e�����w�v.
004740       05 �����������e�P�w�v           PIC X(74)  VALUE SPACE.
004750       05 �����������e�Q�w�v           PIC X(74)  VALUE SPACE.
004760       05 �����������e�R�w�v           PIC X(74)  VALUE SPACE.
004770       05 �����������e�S�w�v           PIC X(96)  VALUE SPACE.
004780*
004790** ���������E�������R����敪�p
004800 01 ������������敪�v                 PIC 9 VALUE ZERO.
004810 01 �������R����敪�v                 PIC 9 VALUE ZERO.
004820*
004830* ������������敪
004831 01 ���Z������������敪�v             PIC 9    VALUE ZERO.
004832 01 ���Z�������R����敪�v             PIC 9    VALUE ZERO.
004833*
004834** �������Z�܂Ƃߗp
004840 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
004850*
004862 01 �o�ߕ��ʂv                         PIC N(1)  VALUE SPACE.
004863 01 �����o�߂v.
004864    03 �����o�ߕ��ʂv                  PIC X(10) OCCURS 5 VALUE SPACE.
004868*
004877**********************************************************************************
004878*
004880 01 �ޔ����ڂf�v.
004890   03 ���Z�v�g��ނv                   PIC X(4).
004900   03 ���Z�v�g��ނf�v                 PIC X(4).
004910   03 ���Z�v�g��ʂf�v                 PIC 9(2).
004920*
004930****************
004940* �����f�[�^�e *
004950****************
004960 01 �������v.
004970    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
004980    03 ���ʏ��v  OCCURS   9.
004990       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
005000       05 ���ʃR�[�h�v.
005010          07 ������ʂv                PIC 9(2)  VALUE ZERO.
005020          07 ���ʂv                    PIC 9(2)  VALUE ZERO.
005030          07 ���E�敪�v                PIC 9(1)  VALUE ZERO.
005040          07 �����ʒu�ԍ��v            PIC 9(2)  VALUE ZERO.
005050       05 �������v                     PIC N(16) VALUE SPACE.
005060       05 �����N�����v.
005070          07 �����a��v                PIC 9     VALUE ZERO.
005080          07 �����N�v                  PIC 9(2)  VALUE ZERO.
005090          07 �������v                  PIC 9(2)  VALUE ZERO.
005100          07 �������v                  PIC 9(2)  VALUE ZERO.
005110       05 �����N�����v.
005120          07 �����a��v                PIC 9     VALUE ZERO.
005130          07 �����N�v                  PIC 9(2)  VALUE ZERO.
005140          07 �������v                  PIC 9(2)  VALUE ZERO.
005150          07 �������v                  PIC 9(2)  VALUE ZERO.
005160       05 �J�n�N�����v.
005170          07 �J�n�a��v                PIC 9     VALUE ZERO.
005180          07 �J�n�N�v                  PIC 9(2)  VALUE ZERO.
005190          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
005200          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
005210       05 �I���N�����v.
005220          07 �I���a��v                PIC 9     VALUE ZERO.
005230          07 �I���N�v                  PIC 9(2)  VALUE ZERO.
005240          07 �I�����v                  PIC 9(2)  VALUE ZERO.
005250          07 �I�����v                  PIC 9(2)  VALUE ZERO.
005260       05 �������v                     PIC 9(2)  VALUE ZERO.
005270       05 ���񏈒u�񐔂v               PIC 9     VALUE ZERO.
005280       05 �]�A�敪�v                   PIC 9(1)  VALUE ZERO.
005290    03 �V�K�敪�v                      PIC 9(1)  VALUE ZERO.
005300    03 �p���敪�v                      PIC 9(1)  VALUE ZERO.
005310    03 ���������v OCCURS 27.
005320       05 ���������v�o                 PIC X(74) VALUE SPACE.
005330*
005340*********************************************************************
005350*    ************
005360*    * ������� *
005370*    ************
005380*    �����̗���
005390***********************
005400 01 �����P�v�q.
005410   03 �����v�q.
005420      05 �����񐔂v                 PIC 9(1)    VALUE ZERO.
005430      05 �������ԊO�񐔂v           PIC 9(1)    VALUE ZERO.
005440      05 �����x���񐔂v             PIC 9(1)    VALUE ZERO.
005450      05 �����[��񐔂v             PIC 9(1)    VALUE ZERO.
005461      05 ���S�����v                 PIC 9(3)    VALUE ZERO.
005462      05 �������v                   PIC 9(5)    VALUE ZERO.
005463      05 �������Z���v               PIC 9(5)    VALUE ZERO.
005464   03 ���������k���v                PIC 9(4)    VALUE ZERO.
005466   03 �Č��񐔂v                    PIC 9(1)    VALUE ZERO.
005471   03 �Č����v�q                    PIC 9(5)    VALUE ZERO.
005472   03 ���Âv�q.
005480      05 ���É񐔂v                 PIC 9(2)    VALUE ZERO.
005490      05 ���Ë����v                 PIC 9(3)V9  VALUE ZERO.
005500      05 ���Ë����Q�v               PIC 9(3)    VALUE ZERO.
005510      05 ���Ö�Ԃv                 PIC 9(1)    VALUE ZERO.
005520      05 ���Ó�H�v                 PIC 9(2)    VALUE ZERO.
005530      05 ���Ö\���v                 PIC 9(2)    VALUE ZERO.
005542      05 ���×��v                   PIC 9(5)    VALUE ZERO.
005543      05 ���É��Z���v               PIC 9(5)    VALUE ZERO.
005544   03 �������q�v�q.
      */�������q�ύX/20180611
000561      05 �������q�񐔂v             PIC 9(2)    VALUE ZERO.
005546*      05 ��񐔂v                   PIC 9(1)    VALUE ZERO.
005550*      05 ���񐔂v                   PIC 9(1)    VALUE ZERO.
005560*      05 ���񐔂v                   PIC 9(1)    VALUE ZERO.
005573      05 �������q���Z���v           PIC 9(5)    VALUE ZERO.
      */�^����Òǉ�/20180607
         03 �^����×��v�q.
            05 �^����×��񐔂v           PIC 9(1)    VALUE ZERO.
            05 �^����×��v               PIC 9(5)    VALUE ZERO.
005574   03 ���񋟂v�q.
005575      05 ���񋟗��񐔂v           PIC 9(1)    VALUE ZERO.
005580      05 ���񋟗��v               PIC 9(5)    VALUE ZERO.
005581   03 �ꕔ���S���v�q                PIC 9(6)    VALUE ZERO.
005590   03 �������z�v�q                  PIC 9(6)    VALUE ZERO.
005600   03 ���t�����v�q                  PIC 9(1)    VALUE ZERO.
005610   03 �󋋎ҕ��S�z�v�q              PIC 9(6)    VALUE ZERO.
005620   03 �����������z�v�q              PIC 9(6)    VALUE ZERO.
005630*/
005640   03 ���k�x���񐔂v                PIC 9(1)    VALUE ZERO.
005650   03 �{�p���s�v.
005660      05 �{�p���v                   PIC 9(1) OCCURS 31 VALUE ZERO.
      */���׏����s�̐����Z�ǉ�/20221020
         03 ���׏����s�񐔂v              PIC 9(1)    VALUE ZERO.
         03 ���׏����s�v                  PIC 9(3)    VALUE ZERO.
         03 ���׏����s�����v.
            05 ���׏����s���v             PIC 9(2)    VALUE ZERO.
            05 ���׏����s���v             PIC 9(2)    VALUE ZERO.
005670*
005680* �������ʖ��̗���
005690***********************
005700 01 �����Q�v�q.
005710   03 ���񏈒u�v�q    OCCURS   9.
005720      05 ���񏈒u���v�q             PIC 9(5)    VALUE ZERO.
005770*
005780* �������̗���
005790***********************
005800 01 �����R�v�q.
007792**********
007793* �P���� *
007794**********
007795   03 ���ʂP�v�q.
007796      05 ��ÂP�v�q.
007797         07 ��ÒP���P�v�q              PIC 9(4)    VALUE ZERO.
007798         07 ��É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
007799         07 ��×��P�v�q                PIC 9(5)    VALUE ZERO.
007800      05 ��㪖@�P�v�q.
007801         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
007802         07 ��㪖@���P�v�q              PIC 9(4)    VALUE ZERO.
007803      05 ��㪖@�P�v�q.
007804         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
007805         07 ��㪖@���P�v�q              PIC 9(4)    VALUE ZERO.
007806      05 �d�ÂP�v�q.
007807         07 �d�É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
007808         07 �d�×��P�v�q                PIC 9(4)    VALUE ZERO.
007809      05 ���v�P�v�q                     PIC 9(6)    VALUE ZERO.
007811      05 �����������P�v�q               PIC 9(3)    VALUE ZERO.
007812      05 ���������v�P�v�q               PIC 9(6)    VALUE ZERO.
007813**********
007814* �Q���� *
007815**********
007816   03 ���ʂQ�v�q.
007817      05 ��ÂQ�v�q.
007818         07 ��ÒP���Q�v�q              PIC 9(4)    VALUE ZERO.
007819         07 ��É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
007820         07 ��×��Q�v�q                PIC 9(5)    VALUE ZERO.
007821      05 ��㪖@�Q�v�q.
007822         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
007823         07 ��㪖@���Q�v�q              PIC 9(4)    VALUE ZERO.
007824      05 ��㪖@�Q�v�q.
007825         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
007826         07 ��㪖@���Q�v�q              PIC 9(4)    VALUE ZERO.
007827      05 �d�ÂQ�v�q.
007828         07 �d�É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
007829         07 �d�×��Q�v�q                PIC 9(4)    VALUE ZERO.
007830      05 ���v�Q�v�q                     PIC 9(6)    VALUE ZERO.
007831      05 �����������Q�v�q               PIC 9(3)    VALUE ZERO.
007832      05 ���������v�Q�v�q               PIC 9(6)    VALUE ZERO.
007833******************
007834* �R���ʁ^�W�� *
007835******************
007836   03 ���ʂR�W�v�q.
007837      05 ��ÂR�W�v�q.
007838         07 ��ÒP���R�W�v�q              PIC 9(4)  VALUE ZERO.
007839         07 ��É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
007840         07 ��×��R�W�v�q                PIC 9(5)  VALUE ZERO.
007841      05 ��㪖@�R�W�v�q.
007842         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
007843         07 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
007844      05 ��㪖@�R�W�v�q.
007845         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
007846         07 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
007847      05 �d�ÂR�W�v�q.
007848         07 �d�É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
007849         07 �d�×��R�W�v�q                PIC 9(4)  VALUE ZERO.
007850      05 ���v�R�W�v�q                     PIC 9(6)  VALUE ZERO.
007851      05 �����ʍ����v�R�W�v�q             PIC 9(6)  VALUE ZERO.
007852      05 �����������R�W�v�q               PIC 9(3)  VALUE ZERO.
007853      05 ���������v�R�W�v�q               PIC 9(6)  VALUE ZERO.
007854******************
007855* �R���ʁ^�P�O�� *
007856******************
007857   03 ���ʂR�O�v�q.
007858      05 �����J�n�����R�O�v�q.
007859         07 �����J�n���R�O�v�q            PIC 9(2)  VALUE ZERO.
007860         07 �����J�n���R�O�v�q            PIC 9(2)  VALUE ZERO.
007861      05 ��ÂR�O�v�q.
007862         07 ��ÒP���R�O�v�q              PIC 9(4)  VALUE ZERO.
007863         07 ��É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
007864         07 ��×��R�O�v�q                PIC 9(5)  VALUE ZERO.
007865      05 ��㪖@�R�O�v�q.
007866         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
007867         07 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
007868      05 ��㪖@�R�O�v�q.
007869         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
007870         07 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
007871      05 �d�ÂR�O�v�q.
007872         07 �d�É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
007873         07 �d�×��R�O�v�q                PIC 9(4)  VALUE ZERO.
007874      05 ���v�R�O�v�q                     PIC 9(6)  VALUE ZERO.
007876      05 �����������R�O�v�q               PIC 9(3)  VALUE ZERO.
007877      05 ���������v�R�O�v�q               PIC 9(6)  VALUE ZERO.
007878****************
007879* �S���ʁ^�T�� *
007880****************
007881   03 ���ʂS�T�v�q.
007882      05 ��ÂS�T�v�q.
007883         07 ��ÒP���S�T�v�q              PIC 9(4)  VALUE ZERO.
007884         07 ��É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
007885         07 ��×��S�T�v�q                PIC 9(5)  VALUE ZERO.
007886      05 ��㪖@�S�T�v�q.
007887         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
007888         07 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
007889      05 ��㪖@�S�T�v�q.
007890         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
007891         07 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
007892      05 �d�ÂS�T�v�q.
007893         07 �d�É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
007894         07 �d�×��S�T�v�q                PIC 9(4)  VALUE ZERO.
007895      05 ���v�S�T�v�q                     PIC 9(6)  VALUE ZERO.
007896      05 �����ʍ����v�S�T�v�q             PIC 9(6)  VALUE ZERO.
007897      05 �����������S�T�v�q               PIC 9(3)  VALUE ZERO.
007898      05 ���������v�S�T�v�q               PIC 9(6)  VALUE ZERO.
007899****************
007900* �S���ʁ^�W�� *
007901****************
007902   03 ���ʂS�W�v�q.
007903      05 �����J�n�����S�W�v�q.
007904         07 �����J�n���S�W�v�q            PIC 9(2)  VALUE ZERO.
007905         07 �����J�n���S�W�v�q            PIC 9(2)  VALUE ZERO.
007906      05 ��ÂS�W�v�q.
007907         07 ��ÒP���S�W�v�q              PIC 9(4)  VALUE ZERO.
007908         07 ��É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
007909         07 ��×��S�W�v�q                PIC 9(5)  VALUE ZERO.
007910      05 ��㪖@�S�W�v�q.
007911         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
007912         07 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
007913      05 ��㪖@�S�W�v�q.
007914         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
007915         07 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
007916      05 �d�ÂS�W�v�q.
007917         07 �d�É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
007918         07 �d�×��S�W�v�q                PIC 9(4)  VALUE ZERO.
007919      05 ���v�S�W�v�q                     PIC 9(6)  VALUE ZERO.
007920      05 �����ʍ����v�S�W�v�q             PIC 9(6)  VALUE ZERO.
007921      05 �����������S�W�v�q               PIC 9(3)  VALUE ZERO.
007922      05 ���������v�S�W�v�q               PIC 9(6)  VALUE ZERO.
007923******************
007924* �S���ʁ^�P�O�� *
007925******************
007926   03 ���ʂS�O�v�q.
007927      05 �����J�n�����S�O�v�q.
007928         07 �����J�n���S�O�v�q            PIC 9(2)  VALUE ZERO.
007929         07 �����J�n���S�O�v�q            PIC 9(2)  VALUE ZERO.
007930      05 ��ÂS�O�v�q.
007931         07 ��ÒP���S�O�v�q              PIC 9(4)  VALUE ZERO.
007932         07 ��É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
007933         07 ��×��S�O�v�q                PIC 9(5)  VALUE ZERO.
007934      05 ��㪖@�S�O�v�q.
007935         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
007936         07 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
007937      05 ��㪖@�S�O�v�q.
007938         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
007939         07 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
007940      05 �d�ÂS�O�v�q.
007941         07 �d�É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
007942         07 �d�×��S�O�v�q                PIC 9(4)  VALUE ZERO.
007943      05 ���v�S�O�v�q                     PIC 9(6)  VALUE ZERO.
007944      05 �����������S�O�v�q               PIC 9(3)  VALUE ZERO.
007945      05 ���������v�S�O�v�q               PIC 9(6)  VALUE ZERO.
007946********************
007947* �T���ʁ^�Q�D�T�� *
007948********************
007949   03 ���ʂT�Q�v�q.
007950      05 ��ÂT�Q�v�q.
007951         07 ��ÒP���T�Q�v�q              PIC 9(4)  VALUE ZERO.
007952         07 ��É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
007953         07 ��×��T�Q�v�q                PIC 9(5)  VALUE ZERO.
007954      05 ��㪖@�T�Q�v�q.
007955         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
007956         07 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
007957      05 ��㪖@�T�Q�v�q.
007958         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
007959         07 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
007960      05 �d�ÂT�Q�v�q.
007961         07 �d�É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
007962         07 �d�×��T�Q�v�q                PIC 9(4)  VALUE ZERO.
007963      05 ���v�T�Q�v�q                     PIC 9(6)  VALUE ZERO.
007964      05 �����ʍ����v�T�Q�v�q             PIC 9(6)  VALUE ZERO.
007965      05 �����������T�Q�v�q               PIC 9(3)  VALUE ZERO.
007966      05 ���������v�T�Q�v�q               PIC 9(6)  VALUE ZERO.
007967****************
007968* �T���ʁ^�T�� *
007969****************
007970   03 ���ʂT�T�v�q.
007971      05 �����J�n�����T�T�v�q.
007972         07 �����J�n���T�T�v�q            PIC 9(2)  VALUE ZERO.
007973         07 �����J�n���T�T�v�q            PIC 9(2)  VALUE ZERO.
007974      05 ��ÂT�T�v�q.
007975         07 ��ÒP���T�T�v�q              PIC 9(4)  VALUE ZERO.
007976         07 ��É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
007977         07 ��×��T�T�v�q                PIC 9(5)  VALUE ZERO.
007978      05 ��㪖@�T�T�v�q.
007979         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
007980         07 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
007981      05 ��㪖@�T�T�v�q.
007982         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
007983         07 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
007984      05 �d�ÂT�T�v�q.
007985         07 �d�É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
007986         07 �d�×��T�T�v�q                PIC 9(4)  VALUE ZERO.
007987      05 ���v�T�T�v�q                     PIC 9(6)  VALUE ZERO.
007988      05 �����ʍ����v�T�T�v�q             PIC 9(6)  VALUE ZERO.
007989      05 �����������T�T�v�q               PIC 9(3)  VALUE ZERO.
007990      05 ���������v�T�T�v�q               PIC 9(6)  VALUE ZERO.
007991****************
007992* �T���ʁ^�W�� *
007993****************
007994   03 ���ʂT�W�v�q.
007995      05 �����J�n�����T�W�v�q.
007996         07 �����J�n���T�W�v�q            PIC 9(2)  VALUE ZERO.
007997         07 �����J�n���T�W�v�q            PIC 9(2)  VALUE ZERO.
007998      05 ��ÂT�W�v�q.
007999         07 ��ÒP���T�W�v�q              PIC 9(4)  VALUE ZERO.
008000         07 ��É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
008001         07 ��×��T�W�v�q                PIC 9(5)  VALUE ZERO.
008002      05 ��㪖@�T�W�v�q.
008003         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
008004         07 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
008005      05 ��㪖@�T�W�v�q.
008006         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
008007         07 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
008008      05 �d�ÂT�W�v�q.
008009         07 �d�É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
008010         07 �d�×��T�W�v�q                PIC 9(4)  VALUE ZERO.
008011      05 ���v�T�W�v�q                     PIC 9(6)  VALUE ZERO.
008012      05 �����ʍ����v�T�W�v�q             PIC 9(6)  VALUE ZERO.
008013      05 �����������T�W�v�q               PIC 9(3)  VALUE ZERO.
008014      05 ���������v�T�W�v�q               PIC 9(6)  VALUE ZERO.
008015******************
008016* �T���ʁ^�P�O�� *
008017******************
008018   03 ���ʂT�O�v�q.
008019      05 �����J�n�����T�O�v�q.
008020         07 �����J�n���T�O�v�q            PIC 9(2)  VALUE ZERO.
008021         07 �����J�n���T�O�v�q            PIC 9(2)  VALUE ZERO.
008022      05 ��ÂT�O�v�q.
008023         07 ��ÒP���T�O�v�q              PIC 9(4)  VALUE ZERO.
008024         07 ��É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
008025         07 ��×��T�O�v�q                PIC 9(5)  VALUE ZERO.
008026      05 ��㪖@�T�O�v�q.
008027         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
008028         07 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
008029      05 ��㪖@�T�O�v�q.
008030         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
008031         07 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
008032      05 �d�ÂT�O�v�q.
008033         07 �d�É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
008034         07 �d�×��T�O�v�q                PIC 9(4)  VALUE ZERO.
008035      05 ���v�T�O�v�q                     PIC 9(6)  VALUE ZERO.
008036      05 �����������T�O�v�q               PIC 9(3)  VALUE ZERO.
008037      05 ���������v�T�O�v�q               PIC 9(6)  VALUE ZERO.
008038*
008039******************
008040* �R���ʁ^���v�@ *
008041******************
008042   03 ���ʂR�v�q.
008043      05 ��ÂR�v�q.
008044         07 ��É񐔂R�v�q                PIC 9(2)  VALUE ZERO.
008045         07 ��×��R�v�q                  PIC 9(6)  VALUE ZERO.
008046      05 ��㪖@�R�v�q.
008047         07 ��㪖@�񐔂R�v�q              PIC 9(2)  VALUE ZERO.
008048         07 ��㪖@���R�v�q                PIC 9(6)  VALUE ZERO.
008049      05 ��㪖@�R�v�q.
008050         07 ��㪖@�񐔂R�v�q              PIC 9(2)  VALUE ZERO.
008051         07 ��㪖@���R�v�q                PIC 9(6)  VALUE ZERO.
008052      05 �d�ÂR�v�q.
008053         07 �d�É񐔂R�v�q                PIC 9(2)  VALUE ZERO.
008054         07 �d�×��R�v�q                  PIC 9(6)  VALUE ZERO.
008055******************
008056* �S���ʁ^���v�@ *
008057******************
008058   03 ���ʂS�v�q.
008059      05 ��ÂS�v�q.
008060         07 ��É񐔂S�v�q                PIC 9(2)  VALUE ZERO.
008061         07 ��×��S�v�q                  PIC 9(6)  VALUE ZERO.
008062      05 ��㪖@�S�v�q.
008063         07 ��㪖@�񐔂S�v�q              PIC 9(2)  VALUE ZERO.
008064         07 ��㪖@���S�v�q                PIC 9(6)  VALUE ZERO.
008065      05 ��㪖@�S�v�q.
008066         07 ��㪖@�񐔂S�v�q              PIC 9(2)  VALUE ZERO.
008067         07 ��㪖@���S�v�q                PIC 9(6)  VALUE ZERO.
008068      05 �d�ÂS�v�q.
008069         07 �d�É񐔂S�v�q                PIC 9(2)  VALUE ZERO.
008070         07 �d�×��S�v�q                  PIC 9(6)  VALUE ZERO.
008071******************
008072* �T���ʁ^���v�@ *
008073******************
008074   03 ���ʂT�v�q.
008075      05 ��ÂT�v�q.
008076         07 ��É񐔂T�v�q                PIC 9(2)  VALUE ZERO.
008077         07 ��×��T�v�q                  PIC 9(6)  VALUE ZERO.
008078      05 ��㪖@�T�v�q.
008079         07 ��㪖@�񐔂T�v�q              PIC 9(2)  VALUE ZERO.
008080         07 ��㪖@���T�v�q                PIC 9(6)  VALUE ZERO.
008081      05 ��㪖@�T�v�q.
008082         07 ��㪖@�񐔂T�v�q              PIC 9(2)  VALUE ZERO.
008083         07 ��㪖@���T�v�q                PIC 9(6)  VALUE ZERO.
008084      05 �d�ÂT�v�q.
008085         07 �d�É񐔂T�v�q                PIC 9(2)  VALUE ZERO.
008086         07 �d�×��T�v�q                  PIC 9(6)  VALUE ZERO.
008087*
008088*
008089*****************************************************************
008090 01 �v�Z�@����N�v                     PIC 9(2).
008091* ���t�v�n�q�j
008092 01 �v�Z�@����.
008093    03 �v�Z�@����N                    PIC 9(4).
008094    03 �v�Z�@�����                  PIC 9(4).
008095 01 �v�Z�@����q REDEFINES �v�Z�@����.
008096    03 �v�Z�@���I                      PIC 9(2).
008097    03 �v�Z�@���t                      PIC 9(6).
008098    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
008099       05 �v�Z�@�N��                   PIC 9(4).
008100       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
008101         07 �v�Z�@�N                   PIC 9(2).
008102         07 �v�Z�@��                   PIC 9(2).
008103       05 �v�Z�@��                     PIC 9(2).
008104*
008105 01 �{�p�a��N�����b�v.
008106   03 �{�p�a��N���b�v.
008107     05 �{�p�a��b�v                   PIC 9.
008108     05 �{�p�N���b�v.
008109        07 �{�p�N�b�v                  PIC 9(2).
008110        07 �{�p���b�v                  PIC 9(2).
008111   03 �{�p���b�v                       PIC 9(2).
008112*
008113* C �A�g�p
008114 01  �����P�v        PIC X(4096).
008115 01  �����Q�v        PIC X(512).
008116 01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
008117*
008118 01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
008119*
008120******************************************************************
008121*                          �A������                              *
008122******************************************************************
008123*
008124********************
008125* ���b�Z�[�W�\���L�[ *
008126********************
008127 01 �A���|�L�[ IS EXTERNAL.
008128    03  �A���|���b�Z�[�W               PIC N(20).
008129*
008130 01 �A���R�|�L�[ IS EXTERNAL.
008131    03  �A���R�|���b�Z�[�W             PIC N(20).
008132    03  �A���R�|���b�Z�[�W�P           PIC X(20).
008133*
008134****************
008135* ��ʓ��͏�� *
008136****************
008137 01 �A���|��ʏ��x�g�o�T�W�O IS EXTERNAL.
008138    03 �A���|�����a��N��.
008139       05 �A���|�����a��               PIC 9.
008140       05 �A���|�����N��.
008141         07 �A���|�����N               PIC 9(2).
008142         07 �A���|������               PIC 9(2).
008143*
008144************************
008145* �������R���Z�b�g     *
008146************************
008147 01 �A�����|�L�[ IS EXTERNAL.
008148    03 �A�����|�{�p�N��.
008149       05 �A�����|�{�p�a��               PIC 9.
008150       05 �A�����|�{�p�N                 PIC 9(2).
008151       05 �A�����|�{�p��                 PIC 9(2).
008152    03  �A�����|���҃R�[�h.
008153       05 �A�����|���Ҕԍ�               PIC 9(6).
008154       05 �A�����|�}��                   PIC X.
008155    03 �A�����|������                    PIC 9(2).
008156    03 �A�����|���R��                    PIC N(63) OCCURS 15.
008157*
008158************************
008159* ���Z���������������
008160************************
008161 01 �A���Z������|�L�[ IS EXTERNAL.
008162    03 �A���Z������|�{�p�N��.
008163       05 �A���Z������|�{�p�a��               PIC 9.
008164       05 �A���Z������|�{�p�N                 PIC 9(2).
008165       05 �A���Z������|�{�p��                 PIC 9(2).
008166    03  �A���Z������|���҃R�[�h.
008167       05 �A���Z������|���Ҕԍ�               PIC 9(6).
008168       05 �A���Z������|�}��                   PIC X.
008169    03 �A���Z������|�Ώۃt���O                PIC X(3).
008170*
008171************************
008172* �������Z�܂Ƃ�
008173************************
008174 01 �A���Z�܂Ƃ߁|�L�[ IS EXTERNAL.
008175    03 �A���Z�܂Ƃ߁|�{�p�a��N��.
008176       05 �A���Z�܂Ƃ߁|�{�p�a��               PIC 9.
008177       05 �A���Z�܂Ƃ߁|�{�p�N��.
008180          07 �A���Z�܂Ƃ߁|�{�p�N              PIC 9(2).
008190          07 �A���Z�܂Ƃ߁|�{�p��              PIC 9(2).
008200    03 �A���Z�܂Ƃ߁|���҃R�[�h.
008210       05 �A���Z�܂Ƃ߁|���Ҕԍ�               PIC 9(6).
008220       05 �A���Z�܂Ƃ߁|�}��                   PIC X(1).
008230**-------------------------------------------------------**
008240*   1:�������Z�v�g�Ȃ��̖{�̂܂Ƃ߂̔���
008250*   2:���l�E���p�̎Еۏ������Z���̔���
008260    03 �A���Z�܂Ƃ߁|����敪                  PIC 9.
008270**-------------------------------------------------------**
008280*  / OUT /�@ 0:�ΏۊO�A1:�Ώ�
008290    03 �A���Z�܂Ƃ߁|���茋��                  PIC 9.
008300**
008310*
008320* �Í������p
008321 01 �A�Í������|�Í���� IS EXTERNAL.
008322    03 �A�Í������|���͏��.
008323       05 �A�Í������|�L��               PIC X(24).
008324       05 �A�Í������|�ԍ�               PIC X(30).
008325       05 �A�Í������|�Í�������.
008326         07 �A�Í������|�Í����Ҕԍ�     PIC X(6).
008327         07 �A�Í������|�Í�����L��     PIC X.
008328         07 �A�Í������|�Í�����ԍ�     PIC X.
008329         07 �A�Í������|�Í��L��         PIC X(24).
008330         07 �A�Í������|�Í��ԍ�         PIC X(30).
008331    03 �A�Í������|�o�͏��.
008332       05 �A�Í������|���������L��       PIC X(24).
008333       05 �A�Í������|���������ԍ�       PIC X(30).
008334* 
008335******************************************************************
008336*                      PROCEDURE  DIVISION                       *
008340******************************************************************
008500 PROCEDURE               DIVISION.
008510************
008520*           *
008530* ��������   *
008540*           *
008550************
008560     PERFORM ������.
008570     PERFORM ������擾.
008580     PERFORM �{�p�����擾.
008590************
008600*           *
008610* �又��     *
008620*           *
008630************
008640     PERFORM ��ƃt�@�C���쐬.
008650************
008660*           *
008670* �I������   *
008680*           *
008690************
008700     PERFORM �I������.
008710     MOVE ZERO TO PROGRAM-STATUS.
008720     EXIT PROGRAM.
008730*
008740*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
008750*================================================================*
008760 ������ SECTION.
008770*
008780     PERFORM �t�@�C���I�[�v��.
008790* �A�����ڂ̑Ҕ�
008800     MOVE �A���|�����a��  TO �����a��v�q.
008810     MOVE �A���|�����N    TO �����N�v�q.
008820     MOVE �A���|������    TO �������v�q.
008830*
008840     MOVE ZERO            TO �A�Ԃv.
008850*
008860* ������N���̎擾
008870     MOVE ZERO          TO ����N���v  ������N���v.
008880     MOVE �����a��v�q  TO ���|�����敪.
008890     READ �����}�X�^
008900     NOT INVALID KEY
008910         MOVE ���|�J�n����N TO ����N�v
008920     END-READ.
008930*
008940     IF ����N�v = ZERO
008950          MOVE  NC"�����}�X�^�ɊJ�n����N��o�^���ĉ�����" TO �A���|���b�Z�[�W
008960          CALL   "MSG001"
008970          CANCEL "MSG001"
008980          PERFORM �t�@�C����
008990          MOVE 99 TO PROGRAM-STATUS
009000          EXIT PROGRAM
009010     ELSE
009020          COMPUTE ����N�v = ����N�v + �����N�v�q - 1
009030          MOVE �������v�q TO ����v
009040     END-IF.
009050*
009060     MOVE ����N���v   TO  ������N���v.
009070*
009080*================================================================*
009090 �t�@�C���I�[�v�� SECTION.
009100*
009110     OPEN INPUT ������}�X�^.
009120         MOVE NC"������" TO �t�@�C����.
009130         PERFORM �I�[�v���`�F�b�N.
009140     OPEN INPUT �����}�X�^.
009150         MOVE NC"�����}�X�^" TO �t�@�C����.
009160         PERFORM �I�[�v���`�F�b�N.
009170     OPEN INPUT ���̃}�X�^.
009180         MOVE NC"���̃}�X�^" TO �t�@�C����.
009190         PERFORM �I�[�v���`�F�b�N.
009200     OPEN INPUT �{�p�����}�X�^
009210         MOVE NC"�{��" TO �t�@�C����.
009220         PERFORM �I�[�v���`�F�b�N.
009230     OPEN INPUT �{�p�L�^�e.
009240         MOVE NC"�{�p�L�^�e" TO �t�@�C����.
009250         PERFORM �I�[�v���`�F�b�N.
009260     OPEN INPUT ��f�ҏ��e.
009270         MOVE NC"��f�ҏ��e" TO �t�@�C����.
009280         PERFORM �I�[�v���`�F�b�N.
009290     OPEN INPUT �o�߃}�X�^.
009300         MOVE NC"�o�߃}�X�^" TO �t�@�C����.
009310         PERFORM �I�[�v���`�F�b�N.
009320     OPEN INPUT �����f�[�^�e.
009321         MOVE NC"�����f�[�^�e" TO �t�@�C����.
009322         PERFORM �I�[�v���`�F�b�N.
009323     OPEN INPUT ���������e.
009330         MOVE NC"��������" TO �t�@�C����.
009340         PERFORM �I�[�v���`�F�b�N.
009350     OPEN INPUT �s�����}�X�^
009360         MOVE NC"�s����" TO �t�@�C����.
009370         PERFORM �I�[�v���`�F�b�N.
009380     OPEN INPUT ���Z�v�g�e.
009390         MOVE NC"���Z" TO �t�@�C����.
009400         PERFORM �I�[�v���`�F�b�N.
009410     OPEN INPUT �v�Z�}�X�^.
009411         MOVE NC"�v�Z�}�X�^" TO �t�@�C����.
009412         PERFORM �I�[�v���`�F�b�N.
009413     OPEN OUTPUT ��ƃt�@�C���P.
009420         MOVE NC"��P" TO �t�@�C����.
009430         PERFORM �I�[�v���`�F�b�N.
009440*
009450*================================================================*
009460 �I�[�v���`�F�b�N SECTION.
009470*
009480     IF ��ԃL�[  NOT =  "00"
009490         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
009500         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
009510         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
009520                                                 UPON CONS
009530*-----------------------------------------*
009540         CALL "actcshm"  WITH C LINKAGE
009550*-----------------------------------------*
009560         ACCEPT  �L�[���� FROM CONS
009570         PERFORM �t�@�C����
009580         MOVE 99 TO PROGRAM-STATUS
009590         EXIT PROGRAM.
009600*================================================================*
009610 �t�@�C���� SECTION.
009620*
009630     CLOSE ������}�X�^ �����}�X�^ ���̃}�X�^ ��f�ҏ��e
009640           �����f�[�^�e   �o�߃}�X�^ ���������e �{�p�L�^�e �{�p�����}�X�^
009650           �s�����}�X�^   ���Z�v�g�e �v�Z�}�X�^ ��ƃt�@�C���P.
009660*================================================================*
009670 �I������ SECTION.
009680*
009690     PERFORM �t�@�C����.
009700*================================================================*
009710 �G���[�\���q SECTION.
009720*
009730     DISPLAY NC"�t�@�C���Ǎ��G���[" �t�@�C����     UPON CONS.
009740     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
009750     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
009760*-----------------------------------------*
009770     CALL "actcshm"  WITH C LINKAGE.
009780*-----------------------------------------*
009790     ACCEPT  �L�[���� FROM CONS.
009800     PERFORM �t�@�C����.
009810     MOVE 99 TO PROGRAM-STATUS.
009820     EXIT PROGRAM.
009830*================================================================*
009840 �G���[�\�� SECTION.
009850*
009860     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
009870     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
009880     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
009890     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
009900*-----------------------------------------*
009910     CALL "actcshm"  WITH C LINKAGE.
009920*-----------------------------------------*
009930     ACCEPT  �L�[���� FROM CONS.
009940     PERFORM �t�@�C����.
009950     MOVE 99 TO PROGRAM-STATUS.
009960     EXIT PROGRAM.
009970*================================================================*
009980 ������擾 SECTION.
009990*
010000     MOVE ZEROS TO ���|����敪.
010010     READ ������}�X�^
010020     NOT INVALID KEY
010030         MOVE ���|���Z������������敪 TO ������������敪�v
010040         MOVE ���|���Z�������R����敪 TO �������R����敪�v
010050     END-READ.
010060*
010070*================================================================*
010080 �{�p�����擾 SECTION.
010090*
010100     MOVE ZERO  TO �{��|�{�p���ԍ�.
010110     READ �{�p�����}�X�^
010120     INVALID KEY
010130          MOVE  NC"�{�p�����}�X�^�ɓo�^��A���s���ĉ�����" TO �A���|���b�Z�[�W
010140          CALL   "MSG001"
010150          CANCEL "MSG001"
010160          PERFORM �t�@�C����
010170          MOVE 99 TO PROGRAM-STATUS
010180          EXIT PROGRAM
010190     NOT INVALID KEY
010200          IF �{��|�V�_���t�ԍ�(1:2) = "�_"
010201              MOVE �{��|�V�_���t�ԍ�(3:11)   TO �_���t�ԍ��v
010202          ELSE
010203              MOVE �{��|�V�_���t�ԍ�         TO �_���t�ԍ��v
010204          END-IF
010205          IF  �{��|�ڍ��t�����ԍ� = SPACE
010210              MOVE  NC"�{�p���}�X�^�ɉ���ԍ���o�^���ĉ�����" TO �A���|���b�Z�[�W
010220              CALL   "MSG001"
010230              CANCEL "MSG001"
010240              PERFORM �t�@�C����
010250              MOVE 99 TO PROGRAM-STATUS
010260              EXIT PROGRAM
010270          ELSE
010280              MOVE �{��|�ڍ��t�����ԍ�  TO ����ԍ��v
010290              PERFORM ����ԍ��E�l��
010300          END-IF
010310     END-READ.
010320*
010330*================================================================*
010340 ��ƃt�@�C���쐬 SECTION.
010350*
010360     PERFORM �ی��Ҕԍ����t�@�C���쐬.
010370*
010380     OPEN INPUT  ��ƃt�@�C���R.
010390         MOVE NC"��R" TO �t�@�C����.
010400         PERFORM �I�[�v���`�F�b�N.
010410*
010420     PERFORM ��ƃt�@�C���P�쐬.
010430*
010440     CLOSE ��ƃt�@�C���R.
010450*
010460*================================================================*
010470 �ی��Ҕԍ����t�@�C���쐬 SECTION.
010480**********************************************************************
010490**   ���Z�v�g�e����A�Y�������N���̃f�[�^�𒊏o���A
010500**   ��ƃt�@�C���R(�ی��Ҕԍ���)�ɏ����o��.
010510**********************************************************************
010520*
010530     OPEN OUTPUT ��ƃt�@�C���R.
010540         MOVE NC"��R" TO �t�@�C����.
010550         PERFORM �I�[�v���`�F�b�N.
010560*
010570     MOVE �����a��v�q  TO ���Z�|�����a��.
010580     MOVE �����N�v�q    TO ���Z�|�����N.
010590     MOVE �������v�q    TO ���Z�|������.
010600     MOVE ZERO          TO ���Z�|���Z���.
010610     MOVE ZERO          TO ���Z�|�{�p�a��.
010620     MOVE ZERO          TO ���Z�|�{�p�N.
010630     MOVE ZERO          TO ���Z�|�{�p��.
010640     MOVE ZERO          TO ���Z�|���Ҕԍ�.
010650     MOVE SPACE         TO ���Z�|�}��.
010660     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
010670                                  ���Z�|�{�p�a��N��
010680                                  ���Z�|���҃R�[�h
010690                                  ���Z�|���Z���
010700     END-START.
010710     IF ��ԃL�[ = "00"
010720         MOVE SPACE  TO �I���t���O
010730         PERFORM ���Z�v�g�e�Ǎ�
010740         PERFORM UNTIL ( �I���t���O = "YES" ) OR
010750                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
010760                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
010770                       ( ���Z�|������   NOT = �������v�q   )
010780            PERFORM �f�[�^�`�F�b�N
010790**
010800            IF  ���s�L�[�v = "YES"
010810*/���쌧����52��q�A53��Q�A55���c���A60���̑��͏��ҕ����̈׃f�[�^�ɍڂ��Ȃ�/110922
010820                 IF (��|������� = 52 OR 53 OR 55 OR 60) AND
010830                    (��|��p���S�Ҕԍ�����(3:2) = "20" )
010840                     MOVE ZERO  TO ��|�������
010850                     MOVE SPACE TO ��|��p���S�Ҕԍ�����
010860                 END-IF
010870*
010880                 MOVE SPACE TO ��R�|���R�[�h
010890                 INITIALIZE    ��R�|���R�[�h
010900                 MOVE ��|�����a��   TO  ��R�|�����a��
010910                 MOVE ��|�����N     TO  ��R�|�����N
010920                 MOVE ��|������     TO  ��R�|������
010930                 MOVE ��|�{�p�a��   TO  ��R�|�{�p�a��
010940                 MOVE ��|�{�p�N     TO  ��R�|�{�p�N
010950                 MOVE ��|�{�p��     TO  ��R�|�{�p��
010960                 IF  ��|�������   = ZERO  OR 50
010970                     IF ( ��|�ی����   NOT = ZERO ) AND
010980                        ( ��|�ی��Ҕԍ� NOT = SPACE )
010990** �����Ȃ�(���ېe����͏����Ȃ�����)
011000                        MOVE ZERO  TO   ��R�|�����敪
011010                     END-IF
011020                 ELSE
011030** ��������
011040                     MOVE 1        TO   ��R�|�����敪
011050*                   /���ʁF ��q�܂��͏�Q�ł��É��� �́A�����Ȃ��ɂ���/
011060                     IF (( ��|������� = "52" ) OR ( ��|������� = "53" )) AND
011070                        ( ��|��p���S�Ҕԍ�����(3:2) = "22" ) 
011080                         MOVE ZERO  TO  ��R�|�����敪
011090                     END-IF
011100                 END-IF
011110*
011210                 IF ( ��|������       = ZERO  ) AND
011211                    ( ��|��p���S�Ҕԍ� = SPACE )
011212                     MOVE ��|�ی��Ҕԍ�     TO �ی��Ҕԍ��v
011213                 ELSE
011214* �V�l�́A�s�����ԍ�
011215                     MOVE ��|��p���S�Ҕԍ� TO �ی��Ҕԍ��v
011216                 END-IF
011217                 PERFORM �ی��Ҕԍ��E�l��
011218                 MOVE �ی��Ҕԍ������v   TO ��R�|�ی��Ҕԍ�
011219*
011220                 MOVE ��|�{�l�Ƒ��敪   TO ��R�|�{�l�Ƒ��敪
011230                 MOVE ��|��ی��҃J�i   TO ��R�|��ی��҃J�i
011240                 MOVE ��|���҃R�[�h     TO ��R�|���҃R�[�h
011250*
011260                 EVALUATE ���Z�|���Z���
011270                 WHEN 1
011280                 WHEN 2
011290                     MOVE ZERO           TO ��R�|�e�q�敪
011300                 WHEN 3
011310                     MOVE 1              TO ��R�|�e�q�敪
011320                 END-EVALUATE
011330*
011340                 IF (���Z�|���Z��� = 3) AND (��R�|�����敪 = ZERO)
011350                    CONTINUE
011360                 ELSE
011370                    WRITE ��R�|���R�[�h
011380                    INVALID KEY
011390                        MOVE NC"��R"  TO �t�@�C����
011400                    PERFORM �G���[�\��
011410                    END-WRITE
011420                 END-IF
011430             END-IF
011440             PERFORM ���Z�v�g�e�Ǎ�
011450         END-PERFORM
011460     END-IF.
011470*
011480     CLOSE ��ƃt�@�C���R.
011490*
011500*================================================================*
011510 ��ƃt�@�C���P�쐬 SECTION.
011520*
011530     MOVE SPACE  TO �I���t���O.
011540     PERFORM ��ƃt�@�C���R�Ǎ�.
011550     PERFORM UNTIL  �I���t���O = "YES" 
011560*
011570         MOVE SPACE TO �����t���O
011580         MOVE SPACE TO �������Z�܂Ƃ߃t���O
011590         MOVE "YES" TO ���s�L�[�v
011600*
011610** �J�ЁE�����ӁE���R�E ���ےP�Ƃ͑ΏۊO
011620            IF  ��|�ی���� = 70 OR 80 OR 85 OR 90
011630                MOVE SPACE  TO ���s�L�[�v
011640            END-IF
011650** ���i�ؖ��͑ΏۊO
011660         IF  ( ��|�ی���� = 01 OR 08 ) AND
011670             ( ��|������ = ZERO     ) AND
011680             ( ��|���i�ؖ��敪 = 1 )
011690            MOVE SPACE  TO ���s�L�[�v
011700         END-IF
011710**
011720         IF  ���s�L�[�v = "YES"
011730*
011741**��* ���ʏ����i�������Z�܂Ƃ߁j
011750             IF ��|������� NOT = ZERO
011760                 PERFORM �������Z�܂Ƃߔ���
011770             ELSE
011780                 MOVE SPACE TO �������Z�܂Ƃ߃t���O
011790             END-IF
011800**��*
011810*            ********
011820*            * ���� *
011830*            ********
011840             IF ��R�|�e�q�敪 = ZERO
011850                IF ( ��|�ی����   NOT = ZERO ) AND
011860                   ( ��|�ی��Ҕԍ� NOT = SPACE )
011870*                **********************
011880*                * ��ƃt�@�C���쐬 *
011890*                **********************
011900                    IF ( ��|������       = ZERO  ) AND
011910                       ( ��|��p���S�Ҕԍ� = SPACE )
011920                       MOVE 1        TO  ��Ï����敪�v
011922                       IF ��R�|�����敪  = ZERO
011930*   / �����Ȃ� /
011940                          MOVE SPACE TO �����t���O
011950                       ELSE
011960*   / �������� /
011970                          MOVE "YES" TO �����t���O
011980                       END-IF
011990                       PERFORM ��P���R�[�h�Z�b�g����
012000                       PERFORM ��P�t�@�C������
012010                    END-IF
012020                END-IF
012030             END-IF
012040*            ********
012050*            * �V�l *
012060*            ********
012070             IF ��R�|�e�q�敪 = ZERO
012080                IF ( ��|������       NOT = ZERO ) AND
012090                   ( ��|��p���S�Ҕԍ� NOT = SPACE )
012100*                **********************
012110*                * ��ƃt�@�C���쐬 *
012120*                **********************
012130                   MOVE 1        TO ��Ï����敪�v
012131                   IF ��R�|�����敪  = ZERO
012140*   / �����Ȃ� /
012150                      MOVE SPACE TO �����t���O
012160                   ELSE
012170*   / �������� /
012180                      MOVE "YES" TO �����t���O
012190                   END-IF
012200                   PERFORM ��P���R�[�h�Z�b�g�V�l
012210                   PERFORM ��P�t�@�C������
012220                END-IF
012230             END-IF
012240*            ********
012250*            * ���� *
012260*            ********
012270             IF ��R�|�e�q�敪 = 1
005930*         / �����̐����z�O�͑ΏۊO�ɂ��� /170621
005930*         / ���̏����̐����z�O�͑Ώۂɂ��� /170621
006880                IF (���Z�|�����������z NOT = ZERO) OR
                         (��|��p���S�Ҕԍ�����(3:2) = "27")
012280                   MOVE "YES" TO �����t���O
012281                   MOVE 3     TO ��Ï����敪�v
012290                   IF ��|������ = ZERO
012300                       PERFORM ��P���R�[�h�Z�b�g���ۏ���
012310                   ELSE
012320                       PERFORM ��P���R�[�h�Z�b�g�V�l����
012330                   END-IF
012340                   PERFORM ��P�t�@�C������
012350                END-IF
                   END-IF
012360         END-IF
012370         PERFORM ��ƃt�@�C���R�Ǎ�
012380     END-PERFORM.
012390*
012400*================================================================*
012410 ��ƃt�@�C���R�Ǎ� SECTION.
012420*
012430     READ ��ƃt�@�C���R NEXT
012440     AT END
012450         MOVE "YES" TO �I���t���O
012460     NOT AT END
012470         MOVE ��R�|�{�p�a��    TO ��|�{�p�a�� ���Z�|�{�p�a��
012480         MOVE ��R�|�{�p�N      TO ��|�{�p�N   ���Z�|�{�p�N  
012490         MOVE ��R�|�{�p��      TO ��|�{�p��   ���Z�|�{�p��  
012500         MOVE ��R�|���Ҕԍ�    TO ��|���Ҕԍ� ���Z�|���Ҕԍ�
012510         MOVE ��R�|�}��        TO ��|�}��     ���Z�|�}��    
012520         READ ��f�ҏ��e
012530         INVALID KEY
012540              MOVE NC"��f��"   TO �t�@�C����
012550              PERFORM �G���[�\���q
012560         END-READ
012570         IF ��R�|�e�q�敪 = 1
012580             MOVE 3          TO ���Z�|���Z���
012590         ELSE
012600            IF ��|������ = 5
012610                MOVE 2          TO ���Z�|���Z���
012620            ELSE
012630                MOVE 1          TO ���Z�|���Z���
012640            END-IF
012650         END-IF
012660         READ ���Z�v�g�e
012670         INVALID KEY
012680              MOVE NC"���Z�v�g"   TO �t�@�C����
012690              PERFORM �G���[�\���q
012700         END-READ
012710     END-READ.
012720*
012730*================================================================*
012740 �f�[�^�`�F�b�N SECTION.
012750*
012760     MOVE SPACE          TO ���s�L�[�v.
012770* *****************************************************************
012780* * ���Z�v�g�e�̐����Ώۋ敪 = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
012790* *****************************************************************
012800     IF ( ���Z�|�����Ώۋ敪 NOT = ZERO ) AND
012810        ( ���Z�|���ҕ����敪 NOT = 1 )
012820        IF(���Z�|���Z��� = 3) AND ( ���Z�|����\����Ώۋ敪 = 1 )
012830           CONTINUE
012840        ELSE
012850           MOVE ���Z�|�{�p�a��  TO ��|�{�p�a��
012860           MOVE ���Z�|�{�p�N    TO ��|�{�p�N
012870           MOVE ���Z�|�{�p��    TO ��|�{�p��
012880           MOVE ���Z�|���Ҕԍ�  TO ��|���Ҕԍ�
012890           MOVE ���Z�|�}��      TO ��|�}��
012900           READ ��f�ҏ��e
012910           NOT INVALID KEY
012920**      ���ۂ̂�
012930              IF ��|�ی����� = 1
012940                 MOVE "YES"  TO ���s�L�[�v
012950              END-IF
012960           END-READ
012970        END-IF
012980     END-IF.
012990*
013000*================================================================*
013010 ���Z�v�g�e�Ǎ� SECTION.
013020*
013030     READ ���Z�v�g�e NEXT
013040     AT END
013050         MOVE "YES" TO �I���t���O
013060     END-READ.
013070*
013080*================================================================*
013090 �{�p�L�^�e�Ǎ� SECTION.
013100*
013110     READ �{�p�L�^�e NEXT
013120     AT END
013130         MOVE "YES"  TO �I���t���O�Q
013140     END-READ.
013150*================================================================*
013160*================================================================*
013170 ��P���R�[�h�Z�b�g���� SECTION.
013180*
013190**********/  ���ۏ����Ȃ��̎�  /**********
013200*
013210     MOVE SPACE TO ��P�|���R�[�h.
013220     INITIALIZE ��P�|���R�[�h.
013230*
013241*��* ���ʏ����i�������Z�܂Ƃ߁j
013250     MOVE ���Z�|���v               TO ��P�|���v���z.
013260     MOVE ���Z�|�ꕔ���S��         TO ��P�|�ꕔ���S��.
013270     MOVE ���Z�|�������z           TO ��P�|�������z.
013280     IF �������Z�܂Ƃ߃t���O = "YES"
013290         MOVE ���Z�|�󋋎ҕ��S�z   TO ��P�|����S���z
013300         MOVE ���Z�|�����������z   TO ��P�|��������z
013335     ELSE
013337         MOVE ZERO                 TO ��P�|����S���z
013338         MOVE ZERO                 TO ��P�|��������z
013339     END-IF.
013340*��*
013350*
013360     MOVE 1          TO  ��P�|�ی��敪�L�[.
013370*
013390* �������Z�܂Ƃߎ��́A���S�Ҕԍ��E�󋋎Ҕԍ����Z�b�g
013391     IF �����t���O = "YES" AND �������Z�܂Ƃ߃t���O = "YES"
013400*
013403         PERFORM �������S�Ҕԍ��擾
013411*
013420         IF ( ��|��v�Ҕԍ�����(1:1) = "*"  ) OR
013430            ( ��|��v�Ҕԍ�����(1:2) = "��" )
013440            MOVE SPACE                TO ��P�|�����󋋎Ҕԍ� 
013450         ELSE
013460            MOVE ��|��v�Ҕԍ�����   TO ��P�|�����󋋎Ҕԍ�
013470         END-IF
013480     ELSE
013481         MOVE SPACE                   TO ��P�|�������S�Ҕԍ�
013482         MOVE SPACE                   TO ��P�|�����󋋎Ҕԍ�
013483     END-IF.
013490*
013500     MOVE ��|�{�l�Ƒ��敪   TO ��P�|�{�l�Ƒ��敪.
013510*
013520* �Z��(��ی���)
013530     STRING ��|�Z���P    DELIMITED BY SPACE
013540            ��|�Z���Q    DELIMITED BY SPACE
013550            INTO ��P�|��ی��ҏZ��
013560     END-STRING.
013570*
013580     PERFORM ���ʃ��R�[�h�Z�b�g.
013590*
013600*================================================================*
013610 ��P���R�[�h�Z�b�g�V�l SECTION.
013620*
013630**********/ 27�V�l�̎�  /**********
013640*
013890*��* ���ʏ����i�������Z�܂Ƃ߁j
013891     MOVE ���Z�|���v               TO ��P�|���v���z.
013892     MOVE ���Z�|�ꕔ���S��         TO ��P�|�ꕔ���S��.
013893     MOVE ���Z�|�������z           TO ��P�|�������z.
013894     IF �������Z�܂Ƃ߃t���O = "YES"
013895         MOVE ���Z�|�󋋎ҕ��S�z   TO ��P�|����S���z
013896         MOVE ���Z�|�����������z   TO ��P�|��������z
013897     ELSE
013898         MOVE ZERO                 TO ��P�|����S���z
013899         MOVE ZERO                 TO ��P�|��������z
013900     END-IF.
013901*��*
013902**
013903     IF ��|�{�p�a��N�� < 42004
013910         MOVE 2              TO  ��P�|�ی��敪�L�[
013920     ELSE
013930         MOVE 1              TO  ��P�|�ی��敪�L�[
013940     END-IF.
013950*
014080* �������Z�܂Ƃߎ��́A���S�Ҕԍ��E�󋋎Ҕԍ����Z�b�g
014081     IF �����t���O = "YES" AND �������Z�܂Ƃ߃t���O = "YES"
014082*
014083         PERFORM �������S�Ҕԍ��擾
014084*
014085         IF ( ��|��v�Ҕԍ�����(1:1) = "*"  ) OR
014086            ( ��|��v�Ҕԍ�����(1:2) = "��" )
014087            MOVE SPACE                TO ��P�|�����󋋎Ҕԍ� 
014088         ELSE
014089            MOVE ��|��v�Ҕԍ�����   TO ��P�|�����󋋎Ҕԍ�
014090         END-IF
014091     ELSE
014092         MOVE SPACE                   TO ��P�|�������S�Ҕԍ�
014093         MOVE SPACE                   TO ��P�|�����󋋎Ҕԍ�
014094     END-IF.
014095*
014096* �{�l�̂�
014097     MOVE 1   TO ��P�|�{�l�Ƒ��敪.
014100*
014110* �Z��(����)
014120     STRING ��|���ҏZ���P    DELIMITED BY SPACE
014130            ��|���ҏZ���Q    DELIMITED BY SPACE
014140            INTO ��P�|��ی��ҏZ��
014150     END-STRING.
014160*
014170     PERFORM ���ʃ��R�[�h�Z�b�g.
014180*
014190*================================================================*
014200 ��P���R�[�h�Z�b�g���ۏ��� SECTION.
014210*
014220**********/  ���ۏ�������̎�  /**********
014230*
014240     MOVE SPACE TO ��P�|���R�[�h.
014250     INITIALIZE ��P�|���R�[�h.
014260*
014271     PERFORM �������S�Ҕԍ��擾.
014280*
014290     IF ( ��|��v�Ҕԍ�����(1:1) = "*"  ) OR
014300        ( ��|��v�Ҕԍ�����(1:2) = "��" )
014310        MOVE SPACE                TO ��P�|�����󋋎Ҕԍ�
014320     ELSE
014330        MOVE ��|��v�Ҕԍ�����   TO ��P�|�����󋋎Ҕԍ�
014340     END-IF.
014350*
014511     MOVE ���Z�|���v              TO ��P�|���v���z.
014512     MOVE ���Z�|�ꕔ���S��        TO ��P�|�ꕔ���S��.
014513     MOVE ���Z�|�������z          TO ��P�|�������z.
014515     MOVE ���Z�|�󋋎ҕ��S�z      TO ��P�|����S���z.
014516     MOVE ���Z�|�����������z      TO ��P�|��������z.
014519*
014526     MOVE 3                       TO  ��P�|�ی��敪�L�[.
014530*
014540* �{�l�̂�
014550     MOVE 1   TO ��P�|�{�l�Ƒ��敪.
014560*
014570* �Z��(����)
014580     STRING ��|���ҏZ���P    DELIMITED BY SPACE
014590            ��|���ҏZ���Q    DELIMITED BY SPACE
014600            INTO ��P�|��ی��ҏZ��
014610     END-STRING.
014620*
014630     PERFORM ���ʃ��R�[�h�Z�b�g.
014640*
014650*================================================================*
014660 ��P���R�[�h�Z�b�g�V�l���� SECTION.
014670*
014680**********/  �V�l��������̎�  /**********
014690*
014700     MOVE SPACE TO ��P�|���R�[�h.
014710     INITIALIZE ��P�|���R�[�h.
014820*
014830     PERFORM �������S�Ҕԍ��擾.
014840*
014850	    IF ( ��|��v�Ҕԍ�����(1:1) = "*"  ) OR
014860        ( ��|��v�Ҕԍ�����(1:2) = "��" )
014870        MOVE SPACE                TO ��P�|�����󋋎Ҕԍ�
014880     ELSE
014890        MOVE ��|��v�Ҕԍ�����   TO ��P�|�����󋋎Ҕԍ�
014900     END-IF.
014910*
015211     MOVE ���Z�|���v              TO ��P�|���v���z.
015212     MOVE ���Z�|�ꕔ���S��        TO ��P�|�ꕔ���S��.
015213     MOVE ���Z�|�������z          TO ��P�|�������z.
015215     MOVE ���Z�|�󋋎ҕ��S�z      TO ��P�|����S���z.
015216     MOVE ���Z�|�����������z      TO ��P�|��������z.
015219*
015220     MOVE 3                       TO  ��P�|�ی��敪�L�[.
015230*
015240* �{�l�̂�
015250     MOVE 1   TO ��P�|�{�l�Ƒ��敪.
015260*
015270* �Z��(����)
015280     STRING ��|���ҏZ���P    DELIMITED BY SPACE
015290            ��|���ҏZ���Q    DELIMITED BY SPACE
015300            INTO ��P�|��ی��ҏZ��
015310     END-STRING.
015320*
015330     PERFORM ���ʃ��R�[�h�Z�b�g.
015340*
015350*================================================================*
015360*================================================================*
015370 ���ʃ��R�[�h�Z�b�g SECTION.
015380*
015390     MOVE ��|�����a��       TO ��P�|�����a��.
015400     MOVE ��|�����N         TO ��P�|�����N.
015410     MOVE ��|������         TO ��P�|������.
015420     MOVE ��|�{�p�a��       TO ��P�|�{�p�a�� �{�p�a��v�q.
015430     MOVE ��|�{�p�N         TO ��P�|�{�p�N �{�p�N�v�q.
015440     MOVE ��|�{�p��         TO ��P�|�{�p�� �{�p���v�q.
015450     MOVE ��|���҃R�[�h     TO ��P�|���҃R�[�h�L�[  ���҃R�[�h�v�q.
015470
015493* �N��
015500     MOVE ������N���v     TO ��P�|�����N��.
015512*
015520     PERFORM ����{�p�N���擾.
015530     MOVE ����{�p�N���v     TO ��P�|�{�p�N��.
015541*
015542     MOVE ����ԍ������v     TO ��P�|����ԍ�.
015580*
015590     MOVE �_���t�ԍ��v       TO ��P�|�o�^�L���ԍ�.
015593
015594* �ی��Ҕԍ��L�[(����)
015600     IF ( ��|������       = ZERO  ) AND
015610        ( ��|��p���S�Ҕԍ� = SPACE )
015620          MOVE ��|�ی��Ҕԍ�     TO �ی��Ҕԍ��v
015630     ELSE
015640* / �V�l�́A�s�����ԍ����L�[�� /
015650         IF ��|�{�p�a��N�� < 42004
015660             MOVE ��|��p���S�Ҕԍ� TO �ی��Ҕԍ��v
015670         ELSE
015680             MOVE ��|�ی��Ҕԍ�     TO �ی��Ҕԍ��v
015690         END-IF
015700     END-IF.
015710     PERFORM �ی��Ҕԍ��E�l��.
015720     MOVE �ی��Ҕԍ������v   TO ��P�|�ی��Ҕԍ��L�[.
015730*
015740* �ی��Ҕԍ�(����)
015750     MOVE ��|�ی��Ҕԍ�     TO ��P�|�ی��Ҕԍ�.
015780*
015790** �S���y�� (133033) �̎}�ԍ폜���āA�ی��Ҕԍ��ɃZ�b�g
015800     IF ( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(1:6) = "133033" )
015810         MOVE 133033         TO ��P�|�ی��Ҕԍ�  ��P�|�ی��Ҕԍ��L�[
015820     END-IF.
015830*
015840*-----------------------------------------------------------------*
015850     MOVE SPACE TO �A�Í������|�Í����.
015860*
015870*    / �A�Í������|���͏��Z�b�g /
015880     MOVE ��|�L��       TO �A�Í������|�L��.
015890     MOVE ��|�ԍ�       TO �A�Í������|�ԍ�.
015900     MOVE ��|�Í������� TO �A�Í������|�Í�������.
015910*
015920     CALL   �����v���O�������v.
015930     CANCEL �����v���O�������v.
015940*
015950*-----------------------------------------------------------------*
015960* �L��
015980     IF �A�Í������|���������L��(1:2)  = "��" 
015990        MOVE SPACE               TO �L���o�m�v
016010        MOVE �L���o�v            TO ��P�|�L��
016020     ELSE
016030        PERFORM �L�����l��
016040        MOVE �L���o�v            TO ��P�|�L��
016050     END-IF.
016060* �ԍ�
016090     IF ( �A�Í������|���������ԍ�(1:1) = "*"  ) OR
016100        ( �A�Í������|���������ԍ�(1:2) = "��" )
016110        MOVE SPACE           TO ��P�|�ԍ�
016120     ELSE
016140        MOVE �A�Í������|���������ԍ� TO ��P�|�ԍ�
016150     END-IF.
016192
016193*/���{���̏����͖{�̂ɕ��S�Ҕԍ��A�󋋎Ҕԍ����L�ڂ���
016194     MOVE ��|��p���S�Ҕԍ����� TO �s�����ԍ��v
016195     MOVE ��|��v�Ҕԍ�����     TO �󋋎Ҕԍ��v
016196     IF �s�����ԍ��v(3:2) = "27"
016197         IF �s�����ԍ��v(1:2) NOT = "99"
016198             MOVE �s�����ԍ��v TO ��P�|�������S�Ҕԍ�
016199         END-IF
016200         MOVE �󋋎Ҕԍ��v     TO ��P�|�����󋋎Ҕԍ�
016201     END-IF.
016202* ��Ï����敪
016203     MOVE ��Ï����敪�v       TO ��P�|��Ï����敪.
016211* �ی����
016212     MOVE ��|�ی����         TO �ی���ʕϊ��O�v.
016213     PERFORM �ی���ʕϊ�.
016214     MOVE �ی���ʕϊ���v     TO ��P�|�ی���ʋ敪.
016215*
016216* �P���敪
016217     IF ��|������� = ZERO
016218*        �P��
016219         MOVE 1 TO ��P�|�P���敪
016220     ELSE
016221*        �Q��
016222         MOVE 2 TO ��P�|�P���敪
016223     END-IF
016224* �{�Ƌ敪
016225     IF ��|�ی���� = 05
016226         EVALUATE ��|���ʋ敪
016227         WHEN 1
016228*            ����
016229             MOVE 8      TO ��P�|�{�Ƌ敪
016230         WHEN 3
016231*            ���V
016232             MOVE ZERO   TO ��P�|�{�Ƌ敪
016233         END-EVALUATE
016234     ELSE
016235         EVALUATE ��|���ʋ敪
016236         WHEN 1
016237         WHEN 2
016238*            ����
016239             MOVE 8      TO ��P�|�{�Ƌ敪
016240         WHEN 3
016241*            ���V
016242             MOVE ZERO   TO ��P�|�{�Ƌ敪
016243         WHEN 6
016244*            �U��
016245             MOVE 4      TO ��P�|�{�Ƌ敪
016246         WHEN OTHER
016247             IF ��|�{�l�Ƒ��敪 = 1
016248*                �{�l
016249                 MOVE 2  TO ��P�|�{�Ƌ敪
016250             ELSE
016251*                �Ƒ�
016252                 MOVE 6  TO ��P�|�{�Ƌ敪
016253             END-IF
016254         END-EVALUATE
016255     END-IF
016256* ���t����
016257     MOVE ���Z�|���t���� TO ��P�|���t����.
016258*
016259     IF ��|�{�l�Ƒ��敪 = 1
016260*        �{�l
016261         MOVE 1  TO ��P�|�{�l�Ƒ��敪
016262     ELSE
016263*        �Ƒ�
016264         MOVE 2  TO ��P�|�{�l�Ƒ��敪
016265     END-IF
016266
016267* ����
016268     MOVE ��|��ی��Ҏ���   TO �����v.
016269     MOVE �S�p�����v         TO ��P�|��ی��Ҏ���.
016270     MOVE ��|��ی��҃J�i   TO ��P�|��ی��҃J�i ��P�|��ی��҃J�i�L�[.
016271     MOVE ��|���Ҏ���       TO �����v.
016272     MOVE �S�p�����v         TO ��P�|���Ҏ���.
016273     MOVE ��|���҃J�i       TO ��P�|���҃J�i.
016274*
016275     MOVE ��|���Ґ���       TO ��P�|���Ґ���.
016276* ���N����
016277     MOVE ZERO               TO �v�Z�a��N�����v.
016278     MOVE ��|���Ґ��N����   TO �v�Z�a��N�����v.
016280     PERFORM ����N�����擾.
016290     MOVE �v�Z����N�����v   TO ��P�|���Ґ��N����.
016300*
016310****�@/ ���Z�v�g�f�[�^�̎擾 /
016320*
016330     PERFORM �����f�[�^�擾.
016340     PERFORM �������擾.
016350     PERFORM �{�p�L�^�擾.
016351*
016352     MOVE �S�̎������v           TO ��P�|�S�̎�����.
016353*
016354*���Z�R���Ǝ�
016361     MOVE 8                      TO ��P�|�Ǝҋ敪.
016362*����ҋ敪
016363     MOVE ZERO                   TO ��P�|����ҋ敪
016364     IF ��|�ی���� NOT = 05 AND ��|���ʋ敪 NOT = ZERO
016365         EVALUATE ��|���ʋ敪
016366         WHEN 1
016367         WHEN 2
016368         WHEN 3
016369             MOVE ��|���ʋ敪   TO ��P�|����ҋ敪
016370         WHEN 4
016371             MOVE 6              TO ��P�|����ҋ敪
016373         END-EVALUATE
016374     END-IF
016375*���҇��i���g�p���ځj
      */20180611
016376     MOVE ZERO                   TO ��P�|���Ҕԍ�.
016376     MOVE ��|���Ҕԍ�           TO ��P�|���Ҕԍ�.
016377*****
016378*
016380* �P���ʖ�
016390     MOVE ������ʂv(1)          TO ������ʕϊ��O�v.
016400     PERFORM ������ʕϊ�.
016410     MOVE ������ʕϊ���v       TO ��P�|�����敪(1).
016421     IF ������ʕϊ���v = 9
016422         MOVE "����"             TO ���{��ϊ��v�w
016423     ELSE
016424         MOVE �������v(1)        TO ���{��ϊ��v�m
016425     END-IF.
016426     MOVE ���{��ϊ��v�w         TO ��P�|������(1).
016440*
016450     MOVE ZERO                   TO �v�Z�a��N�����v.
016460     MOVE �����N�����v(1)        TO �v�Z�a��N�����v.
016470     PERFORM ����N�����擾.
016480     MOVE �v�Z����N�����v       TO ��P�|�����N����(1).
016490*
016500     MOVE ZERO                   TO �v�Z�a��N�����v.
016510     MOVE �����N�����v(1)        TO �v�Z�a��N�����v.
016520     PERFORM ����N�����擾.
016530     MOVE �v�Z����N�����v       TO ��P�|�����N����(1).
016540*
016550     MOVE ZERO                   TO �v�Z�a��N�����v.
016560     MOVE �J�n�N�����v(1)        TO �v�Z�a��N�����v.
016570     PERFORM ����N�����擾.
016580     MOVE �v�Z����N�����v       TO ��P�|�{�p�J�n�N����(1).
016590*
016600     MOVE ZERO                   TO �v�Z�a��N�����v.
016610     MOVE �I���N�����v(1)        TO �v�Z�a��N�����v.
016620     PERFORM ����N�����擾.
016630     MOVE �v�Z����N�����v       TO ��P�|�{�p�I���N����(1).
016640*
016650     MOVE �������v(1)            TO ��P�|������(1).
016660*
016670     MOVE �]�A�敪�v(1)          TO �]�A�ϊ��O�v.
016680     PERFORM �]�A�敪�ϊ�.
016690     MOVE �]�A�ϊ���v           TO ��P�|�]�A�敪(1).
016700*
016701     MOVE ���񏈒u�񐔂v(1)      TO ��P�|�����Œ�{�É�(1).
016828     MOVE ���񏈒u���v�q(1)      TO ��P�|�����Œ�{�×�(1).
016829*
016830* �Q���ʖ�
016840     MOVE ������ʂv(2)          TO ������ʕϊ��O�v.
016850     PERFORM ������ʕϊ�.
016860     MOVE ������ʕϊ���v       TO ��P�|�����敪(2).
016871     IF ������ʕϊ���v = 9
016872         MOVE "����"             TO ���{��ϊ��v�w
016873     ELSE
016874         MOVE �������v(2)        TO ���{��ϊ��v�m
016875     END-IF.
016876     MOVE ���{��ϊ��v�w         TO ��P�|������(2).
016890*
016900     MOVE ZERO                   TO �v�Z�a��N�����v.
016910     MOVE �����N�����v(2)        TO �v�Z�a��N�����v.
016920     PERFORM ����N�����擾.
016930     MOVE �v�Z����N�����v       TO ��P�|�����N����(2).
016940*
016950     MOVE ZERO                   TO �v�Z�a��N�����v.
016960     MOVE �����N�����v(2)        TO �v�Z�a��N�����v.
016970     PERFORM ����N�����擾.
016980     MOVE �v�Z����N�����v       TO ��P�|�����N����(2).
016990*
017000     MOVE ZERO                   TO �v�Z�a��N�����v.
017010     MOVE �J�n�N�����v(2)        TO �v�Z�a��N�����v.
017020     PERFORM ����N�����擾.
017030     MOVE �v�Z����N�����v       TO ��P�|�{�p�J�n�N����(2).
017040*
017050     MOVE ZERO                   TO �v�Z�a��N�����v.
017060     MOVE �I���N�����v(2)        TO �v�Z�a��N�����v.
017070     PERFORM ����N�����擾.
017080     MOVE �v�Z����N�����v       TO ��P�|�{�p�I���N����(2).
017090*
017100     MOVE �������v(2)            TO ��P�|������(2).
017110*
017120     MOVE �]�A�敪�v(2)          TO �]�A�ϊ��O�v.
017130     PERFORM �]�A�敪�ϊ�.
017140     MOVE �]�A�ϊ���v           TO ��P�|�]�A�敪(2).
017150*
017270     MOVE ���񏈒u�񐔂v(2)      TO ��P�|�����Œ�{�É�(2).
017271     MOVE ���񏈒u���v�q(2)      TO ��P�|�����Œ�{�×�(2).
017272*
017280* �R���ʖ�
017290     MOVE ������ʂv(3)          TO ������ʕϊ��O�v.
017300     PERFORM ������ʕϊ�.
017310     MOVE ������ʕϊ���v       TO ��P�|�����敪(3).
017321     IF ������ʕϊ���v = 9
017322         MOVE "����"             TO ���{��ϊ��v�w
017323     ELSE
017324         MOVE �������v(3)        TO ���{��ϊ��v�m
017325     END-IF.
017326     MOVE ���{��ϊ��v�w         TO ��P�|������(3).
017340*
017350     MOVE ZERO                   TO �v�Z�a��N�����v.
017360     MOVE �����N�����v(3)        TO �v�Z�a��N�����v.
017370     PERFORM ����N�����擾.
017380     MOVE �v�Z����N�����v       TO ��P�|�����N����(3).
017390*
017400     MOVE ZERO                   TO �v�Z�a��N�����v.
017410     MOVE �����N�����v(3)        TO �v�Z�a��N�����v.
017420     PERFORM ����N�����擾.
017430     MOVE �v�Z����N�����v       TO ��P�|�����N����(3).
017440*
017450     MOVE ZERO                   TO �v�Z�a��N�����v.
017460     MOVE �J�n�N�����v(3)        TO �v�Z�a��N�����v.
017470     PERFORM ����N�����擾.
017480     MOVE �v�Z����N�����v       TO ��P�|�{�p�J�n�N����(3).
017490*
017500     MOVE ZERO                   TO �v�Z�a��N�����v.
017510     MOVE �I���N�����v(3)        TO �v�Z�a��N�����v.
017520     PERFORM ����N�����擾.
017530     MOVE �v�Z����N�����v       TO ��P�|�{�p�I���N����(3).
017540*
017550     MOVE �������v(3)            TO ��P�|������(3).
017560*
017570     MOVE �]�A�敪�v(3)          TO �]�A�ϊ��O�v.
017580     PERFORM �]�A�敪�ϊ�.
017590     MOVE �]�A�ϊ���v           TO ��P�|�]�A�敪(3).
017600*
017720     MOVE ���񏈒u�񐔂v(3)      TO ��P�|�����Œ�{�É�(3).
017721     MOVE ���񏈒u���v�q(3)      TO ��P�|�����Œ�{�×�(3).
017722*
017730* �S���ʖ�
017740     MOVE ������ʂv(4)          TO ������ʕϊ��O�v.
017750     PERFORM ������ʕϊ�.
017760     MOVE ������ʕϊ���v       TO ��P�|�����敪(4).
017771     IF ������ʕϊ���v = 9
017772         MOVE "����"             TO ���{��ϊ��v�w
017773     ELSE
017774         MOVE �������v(4)        TO ���{��ϊ��v�m
017775     END-IF.
017776     MOVE ���{��ϊ��v�w         TO ��P�|������(4).
017790*
017800     MOVE ZERO                   TO �v�Z�a��N�����v.
017810     MOVE �����N�����v(4)        TO �v�Z�a��N�����v.
017820     PERFORM ����N�����擾.
017830     MOVE �v�Z����N�����v       TO ��P�|�����N����(4).
017840*
017850     MOVE ZERO                   TO �v�Z�a��N�����v.
017860     MOVE �����N�����v(4)        TO �v�Z�a��N�����v.
017870     PERFORM ����N�����擾.
017880     MOVE �v�Z����N�����v       TO ��P�|�����N����(4).
017890*
017900     MOVE ZERO                   TO �v�Z�a��N�����v.
017910     MOVE �J�n�N�����v(4)        TO �v�Z�a��N�����v.
017920     PERFORM ����N�����擾.
017930     MOVE �v�Z����N�����v       TO ��P�|�{�p�J�n�N����(4).
017940*
017950     MOVE ZERO                   TO �v�Z�a��N�����v.
017960     MOVE �I���N�����v(4)        TO �v�Z�a��N�����v.
017970     PERFORM ����N�����擾.
017980     MOVE �v�Z����N�����v       TO ��P�|�{�p�I���N����(4).
017990*
018000     MOVE �������v(4)            TO ��P�|������(4).
018010*
018020     MOVE �]�A�敪�v(4)          TO �]�A�ϊ��O�v.
018030     PERFORM �]�A�敪�ϊ�.
018040     MOVE �]�A�ϊ���v           TO ��P�|�]�A�敪(4).
018050*
018170     MOVE ���񏈒u�񐔂v(4)      TO ��P�|�����Œ�{�É�(4).
018171     MOVE ���񏈒u���v�q(4)      TO ��P�|�����Œ�{�×�(4).
018172*
018180* �T���ʖ�
018190     MOVE ������ʂv(5)          TO ������ʕϊ��O�v.
018200     PERFORM ������ʕϊ�.
018210     MOVE ������ʕϊ���v       TO ��P�|�����敪(5).
018221     IF ������ʕϊ���v = 9
018222         MOVE "����"             TO ���{��ϊ��v�w
018223     ELSE
018224         MOVE �������v(5)        TO ���{��ϊ��v�m
018225     END-IF.
018226     MOVE ���{��ϊ��v�w         TO ��P�|������(5).
018240*
018250     MOVE ZERO                   TO �v�Z�a��N�����v.
018260     MOVE �����N�����v(5)        TO �v�Z�a��N�����v.
018270     PERFORM ����N�����擾.
018280     MOVE �v�Z����N�����v       TO ��P�|�����N����(5).
018290*
018300     MOVE ZERO                   TO �v�Z�a��N�����v.
018310     MOVE �����N�����v(5)        TO �v�Z�a��N�����v.
018320     PERFORM ����N�����擾.
018330     MOVE �v�Z����N�����v       TO ��P�|�����N����(5).
018340*
018350     MOVE ZERO                   TO �v�Z�a��N�����v.
018360     MOVE �J�n�N�����v(5)        TO �v�Z�a��N�����v.
018370     PERFORM ����N�����擾.
018380     MOVE �v�Z����N�����v       TO ��P�|�{�p�J�n�N����(5).
018390*
018400     MOVE ZERO                   TO �v�Z�a��N�����v.
018410     MOVE �I���N�����v(5)        TO �v�Z�a��N�����v.
018420     PERFORM ����N�����擾.
018430     MOVE �v�Z����N�����v       TO ��P�|�{�p�I���N����(5).
018440*
018450     MOVE �������v(5)            TO ��P�|������(5).
018460*
018470     MOVE �]�A�敪�v(5)          TO �]�A�ϊ��O�v.
018480     PERFORM �]�A�敪�ϊ�.
018490     MOVE �]�A�ϊ���v           TO ��P�|�]�A�敪(5).
018620*
019080     MOVE ���񏈒u�񐔂v(5)      TO ��P�|�����Œ�{�É�(5).
019081     MOVE ���񏈒u���v�q(5)      TO ��P�|�����Œ�{�×�(5).
019082**************************************************************
019090*
019100     MOVE ���ʐ��v               TO ��P�|���ʐ�.
019110*
019130     MOVE �V�K�敪�v             TO ��P�|�V�K�敪.
019140     MOVE �p���敪�v             TO ��P�|�p���敪.
019150*
019160     MOVE �����񐔂v             TO ��P�|������.
019170     MOVE �������v               TO ��P�|������.
019171     MOVE �������ԊO�񐔂v       TO ��P�|�������ԊO���Z��.
019180     MOVE �����x���񐔂v         TO ��P�|�����x�����Z��.
019190     MOVE �����[��񐔂v         TO ��P�|�����[����Z��.
019200     MOVE �������Z���v           TO ��P�|�������Z.
019202     MOVE ���k�x���񐔂v         TO ��P�|���k�x����.
019203     MOVE ���������k���v         TO ��P�|���k�x����.
019204
019205     MOVE �Č��񐔂v             TO ��P�|�Č���.
019210     MOVE �Č����v�q             TO ��P�|�Č���.
019211     MOVE ���Ë����Q�v           TO ��P�|���Ë���.
019220     MOVE ���É񐔂v             TO ��P�|���É�.
019230     MOVE ���×��v               TO ��P�|���×�.
019231*
019240     MOVE ���Ö�Ԃv             TO ��P�|��ԉ��Z���É�.
019250     MOVE ���Ö\���v             TO ��P�|�\���J����Z���É�.
019260     MOVE ���Ó�H�v             TO ��P�|��H���Z���É�.
019270     MOVE ���É��Z���v           TO ��P�|���É��Z.
019271*
      */�������q�ύX������/20180611
           MOVE �������q�񐔂v         TO ��P�|�������q��.
019280*     MOVE ��񐔂v               TO ��P�|�������q���.
019290*     MOVE ���񐔂v               TO ��P�|�������q����.
019300*     MOVE ���񐔂v               TO ��P�|�������q����.
      */�������q�ύX������/20180611
      */�^����Òǉ�/20180611
           MOVE �^����×��񐔂v    TO ��P�|�^����×���.
           MOVE �^����×��v        TO ��P�|�^����×�    .
      *
019310     MOVE �������q���Z���v       TO ��P�|�������q���Z.
019311*
019320     MOVE ���񋟗��񐔂v       TO ��P�|���񋟗���.
019330     MOVE ���񋟗��v           TO ��P�|���񋟗�.
019331*
      */���׏����s�̐����Z�ǉ�/20221012
           MOVE ���׏����s�����v       TO ��P�|���׏����s����.
           MOVE ���׏����s�񐔂v       TO ��P�|���׏����s��.
           MOVE ���׏����s�v           TO ��P�|���׏����s.
      *
019361** / ���ʕʒ������ʃf�[�^ / **
019362*    ���ʂP
019363     MOVE ZERO                   TO ��P�|�����J�n����(1).
019364     MOVE ��É񐔂P�v�q         TO ��P�|��É�(1).
019365     MOVE ��×��P�v�q           TO ��P�|��×�(1).
019366     MOVE ��㪖@�񐔂P�v�q       TO ��P�|��㪖@��(1).
019367     MOVE ��㪖@���P�v�q         TO ��P�|��㪖@��(1).
019368     MOVE ��㪖@�񐔂P�v�q       TO ��P�|��㪖@��(1).
019369     MOVE ��㪖@���P�v�q         TO ��P�|��㪖@��(1).
019370     MOVE �d�É񐔂P�v�q         TO ��P�|�d�É�(1).
019371     MOVE �d�×��P�v�q           TO ��P�|�d�×�(1).
019372     MOVE ZERO                   TO ��P�|�����ʒ�����(1).
019373     MOVE ZERO                   TO ��P�|�����ʒ����z(1).
019374     MOVE �����������P�v�q       TO ��P�|����������(1).
019375     MOVE ���������v�P�v�q       TO ��P�|�����v(1).
019376*    ���ʂQ
019377     MOVE ZERO                   TO ��P�|�����J�n����(2).
019378     MOVE ��É񐔂Q�v�q         TO ��P�|��É�(2).
019379     MOVE ��×��Q�v�q           TO ��P�|��×�(2).
019380     MOVE ��㪖@�񐔂Q�v�q       TO ��P�|��㪖@��(2).
019381     MOVE ��㪖@���Q�v�q         TO ��P�|��㪖@��(2).
019382     MOVE ��㪖@�񐔂Q�v�q       TO ��P�|��㪖@��(2).
019383     MOVE ��㪖@���Q�v�q         TO ��P�|��㪖@��(2).
019384     MOVE �d�É񐔂Q�v�q         TO ��P�|�d�É�(2).
019385     MOVE �d�×��Q�v�q           TO ��P�|�d�×�(2).
019386     MOVE ZERO                   TO ��P�|�����ʒ�����(2).
019387     MOVE ZERO                   TO ��P�|�����ʒ����z(2).
019388     MOVE �����������Q�v�q       TO ��P�|����������(2).
019389     MOVE ���������v�Q�v�q       TO ��P�|�����v(2).
019390*    ���ʂR�̏�i�i�����V�O�j
019391     MOVE ZERO                   TO ��P�|�����J�n����(3).
019392     MOVE ��É񐔂R�W�v�q       TO ��P�|��É�(3).
019393     MOVE ��×��R�W�v�q         TO ��P�|��×�(3).
019394     MOVE ��㪖@�񐔂R�W�v�q     TO ��P�|��㪖@��(3).
019395     MOVE ��㪖@���R�W�v�q       TO ��P�|��㪖@��(3).
019396     MOVE ��㪖@�񐔂R�W�v�q     TO ��P�|��㪖@��(3).
019397     MOVE ��㪖@���R�W�v�q       TO ��P�|��㪖@��(3).
019398     MOVE �d�É񐔂R�W�v�q       TO ��P�|�d�É�(3).
019399     MOVE �d�×��R�W�v�q         TO ��P�|�d�×�(3).
019400     MOVE �R���ʖڒ������v       TO ��P�|�����ʒ�����(3).
019401     MOVE �����ʍ����v�R�W�v�q   TO ��P�|�����ʒ����z(3).
019402     MOVE �����������R�W�v�q     TO ��P�|����������(3).
019403     MOVE ���������v�R�W�v�q     TO ��P�|�����v(3).
019404*    ���ʂR�̉��i�i�����P�O�O�j
019405     MOVE �����J�n�����R�O�v�q   TO ��P�|�����J�n����(4).
019406     MOVE ��É񐔂R�O�v�q       TO ��P�|��É�(4).
019407     MOVE ��×��R�O�v�q         TO ��P�|��×�(4).
019408     MOVE ��㪖@�񐔂R�O�v�q     TO ��P�|��㪖@��(4).
019409     MOVE ��㪖@���R�O�v�q       TO ��P�|��㪖@��(4).
019410     MOVE ��㪖@�񐔂R�O�v�q     TO ��P�|��㪖@��(4).
019411     MOVE ��㪖@���R�O�v�q       TO ��P�|��㪖@��(4).
019412     MOVE �d�É񐔂R�O�v�q       TO ��P�|�d�É�(4).
019413     MOVE �d�×��R�O�v�q         TO ��P�|�d�×�(4).
019414     MOVE �Q���ʖڒ������v       TO ��P�|�����ʒ�����(4).
019415     MOVE ���v�R�O�v�q           TO ��P�|�����ʒ����z(4).
019416     MOVE �����������R�O�v�q     TO ��P�|����������(4).
019417     MOVE ���������v�R�O�v�q     TO ��P�|�����v(4).
019418*    ���ʂS�̏�i�i�����V�O�j
019419     MOVE �����J�n�����S�W�v�q   TO ��P�|�����J�n����(5).
019420     MOVE ��É񐔂S�W�v�q       TO ��P�|��É�(5).
019421     MOVE ��×��S�W�v�q         TO ��P�|��×�(5).
019422     MOVE ��㪖@�񐔂S�W�v�q     TO ��P�|��㪖@��(5).
019423     MOVE ��㪖@���S�W�v�q       TO ��P�|��㪖@��(5).
019424     MOVE ��㪖@�񐔂S�W�v�q     TO ��P�|��㪖@��(5).
019425     MOVE ��㪖@���S�W�v�q       TO ��P�|��㪖@��(5).
019426     MOVE �d�É񐔂S�W�v�q       TO ��P�|�d�É�(5).
019427     MOVE �d�×��S�W�v�q         TO ��P�|�d�×�(5).
019428     MOVE �R���ʖڒ������v       TO ��P�|�����ʒ�����(5).
019429     MOVE �����ʍ����v�S�W�v�q   TO ��P�|�����ʒ����z(5).
019430     MOVE �����������S�W�v�q     TO ��P�|����������(5).
019431     MOVE ���������v�S�W�v�q     TO ��P�|�����v(5).
019432*    ���ʂS�̉��i�i�����P�O�O�j
019433     MOVE �����J�n�����S�O�v�q   TO ��P�|�����J�n����(6).
019434     MOVE ��É񐔂S�O�v�q       TO ��P�|��É�(6).
019435     MOVE ��×��S�O�v�q         TO ��P�|��×�(6).
019436     MOVE ��㪖@�񐔂S�O�v�q     TO ��P�|��㪖@��(6).
019437     MOVE ��㪖@���S�O�v�q       TO ��P�|��㪖@��(6).
019438     MOVE ��㪖@�񐔂S�O�v�q     TO ��P�|��㪖@��(6).
019439     MOVE ��㪖@���S�O�v�q       TO ��P�|��㪖@��(6).
019440     MOVE �d�É񐔂S�O�v�q       TO ��P�|�d�É�(6).
019441     MOVE �d�×��S�O�v�q         TO ��P�|�d�×�(6).
019442     MOVE �Q���ʖڒ������v       TO ��P�|�����ʒ�����(6).
019443     MOVE ���v�S�O�v�q           TO ��P�|�����ʒ����z(6).
019444     MOVE �����������S�O�v�q     TO ��P�|����������(6).
019445     MOVE ���������v�S�O�v�q     TO ��P�|�����v(6).
019446**
019447** / ���������E�������R / **
019448*
019449     IF ������������敪�v  NOT = 1 
019450*      / ���������pWORK�N���A�[ /
019451         INITIALIZE ���������v�s
019452         INITIALIZE �������Ҕԍ��b�v
019453         INITIALIZE �����A�Ԃb�v
019454         INITIALIZE ���������s�a�k
019455         INITIALIZE �����������e�v
019456     END-IF.
019460*-----------------------------------------------*
019463     IF ( ������������敪�v  NOT = 1 ) AND ( ���Z������������敪�v NOT = 1 )
019464        IF ( ������������敪�v = 3 OR 4)
019465           PERFORM ������������Ώ۔��菈��
019466        ELSE
019467           PERFORM ���������擾
019468        END-IF
019469     END-IF.
019472           PERFORM ���������擾
019473*-----------------------------------------------*
019474*
019475     IF �������R����敪�v  NOT = 1 
019480        PERFORM �������R���擾
019490     ELSE
019500        MOVE  SPACE TO  �A�����|�L�[
019510        INITIALIZE      �A�����|�L�[
019520     END-IF.
019530*
019541     INSPECT ���������v�s REPLACING ALL �S�p�� BY ���p��.
019542     INSPECT ���������v�s REPLACING ALL ���s     BY ���p��.
019543     MOVE SPACE TO �����P�v �����Q�v.
019544     MOVE �����������e�����v(1) TO �����P�v.
019545     MOVE �����������e�����v(2) TO �����Q�v.
019546     CALL �v���O�������v WITH C LINKAGE
019547          USING BY REFERENCE �����P�v
019548          BY REFERENCE �����Q�v.
019549     MOVE �����������e�����v(3) TO �����Q�v.
019550     CALL �v���O�������v WITH C LINKAGE
019551          USING BY REFERENCE �����P�v
019552          BY REFERENCE �����Q�v.
019553     MOVE �����������e�����v(4) TO �����Q�v.
019554     CALL �v���O�������v WITH C LINKAGE
019555          USING BY REFERENCE �����P�v
019556          BY REFERENCE �����Q�v.
019557     MOVE �����������e�����v(5) TO �����Q�v.
019558     CALL �v���O�������v WITH C LINKAGE
019559          USING BY REFERENCE �����P�v
019560          BY REFERENCE �����Q�v.
019564     MOVE �����P�v TO ��P�|��������.
019565*
019566     INSPECT ���������v�s REPLACING ALL �S�p�� BY ���p��.
019567     INSPECT ���������v�s REPLACING ALL ���s     BY ���p��.
019568     MOVE SPACE TO �����P�v �����Q�v.
019569     MOVE �����������e�����v(1) TO �����P�v.
019570     MOVE �����������e�����v(2) TO �����Q�v.
019571     CALL �v���O�������v WITH C LINKAGE
019572          USING BY REFERENCE �����P�v
019573          BY REFERENCE �����Q�v.
019574     MOVE �����������e�����v(3) TO �����Q�v.
019575     CALL �v���O�������v WITH C LINKAGE
019576          USING BY REFERENCE �����P�v
019577          BY REFERENCE �����Q�v.
019578     MOVE �����������e�����v(4) TO �����Q�v.
019579     CALL �v���O�������v WITH C LINKAGE
019580          USING BY REFERENCE �����P�v
019581          BY REFERENCE �����Q�v.
019582     MOVE �����������e�����v(5) TO �����Q�v.
019583     CALL �v���O�������v WITH C LINKAGE
019584          USING BY REFERENCE �����P�v
019585          BY REFERENCE �����Q�v.
019586     MOVE �����P�v TO ��P�|��������.
019587
019590     MOVE SPACE TO �����P�v �����Q�v.
019591     MOVE �A�����|���R��(1) TO ���{��ϊ��v�m.
019592     MOVE ���{��ϊ��v�w    TO �����P�v.
019593     INSPECT �����P�v REPLACING ALL �S�p�� BY ���p��.
019594     INSPECT �����P�v REPLACING ALL ���s     BY ���p��.
019595     PERFORM VARYING �J�E���^ FROM 2 BY 1 UNTIL �J�E���^ > 15
019596         MOVE �A�����|���R��(�J�E���^) TO ���{��ϊ��v�m
019597         MOVE ���{��ϊ��v�w           TO �����Q�v
019598         INSPECT �����Q�v REPLACING ALL �S�p�� BY "  "
019599         INSPECT �����Q�v REPLACING ALL ���s     BY "  "
019600         CALL �v���O�������v WITH C LINKAGE
019601              USING BY REFERENCE �����P�v
019602              BY REFERENCE �����Q�v
019611     END-PERFORM.
019612     MOVE �����P�v TO ��P�|�������R.
019614*
      */20180611
019615*     MOVE 05                  TO ��P�|�Ǝҋ敪.
019615*     MOVE 12                  TO ��P�|�Ǝҋ敪.
      */20211027
019615     MOVE 08                  TO ��P�|�Ǝҋ敪.
019617     MOVE �{�p���s�v          TO ��P�|�{�p��.
019618     INSPECT �����o�߂v REPLACING ALL �S�p�� BY ���p��.
019619     MOVE �����o�߂v          TO ��P�|�o��.
019620     MOVE ���s                TO ��P�|���s����.
019621*================================================================*
019622 ��P�t�@�C������ SECTION.
019623*
019630     WRITE ��P�|���R�[�h
019640     INVALID KEY
019650         MOVE NC"��P"  TO �t�@�C����
019660         PERFORM �G���[�\��
019670     END-WRITE.
019680*================================================================*
019690*================================================================*
019700 ����ԍ��E�l�� SECTION.
019710*
019720     MOVE ����ԍ��v      TO  ����ԍ����l�߂v.
019730     MOVE ZERO            TO  ����ԍ��E�l�߂v.
019740     MOVE ZERO            TO  ����ԍ������v.
019750*
019760     MOVE  8  TO  �J�E���^.
019770*
019780     IF  ����ԍ����l�߂v�P(7) NOT = SPACE
019790         COMPUTE �J�E���^ = �J�E���^  -  1
019800         MOVE ����ԍ����l�߂v�P(7)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019810     END-IF.
019820     IF  ����ԍ����l�߂v�P(6) NOT = SPACE
019830         COMPUTE �J�E���^ = �J�E���^  -  1
019840         MOVE ����ԍ����l�߂v�P(6)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019850     END-IF.
019860     IF  ����ԍ����l�߂v�P(5) NOT = SPACE
019870         COMPUTE �J�E���^ = �J�E���^  -  1
019880         MOVE ����ԍ����l�߂v�P(5)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019890     END-IF.
019900     IF  ����ԍ����l�߂v�P(4) NOT = SPACE
019910         COMPUTE �J�E���^ = �J�E���^  -  1
019920         MOVE ����ԍ����l�߂v�P(4)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019930     END-IF.
019940     IF  ����ԍ����l�߂v�P(3) NOT = SPACE
019950         COMPUTE �J�E���^ = �J�E���^  -  1
019960         MOVE ����ԍ����l�߂v�P(3)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
019970     END-IF.
019980     IF  ����ԍ����l�߂v�P(2) NOT = SPACE
019990         COMPUTE �J�E���^ = �J�E���^  -  1
020000         MOVE ����ԍ����l�߂v�P(2)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
020010     END-IF.
020020     IF  ����ԍ����l�߂v�P(1) NOT = SPACE
020030         COMPUTE �J�E���^ = �J�E���^  -  1
020040         MOVE ����ԍ����l�߂v�P(1)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
020050     END-IF.
020060*
020070     MOVE ����ԍ��E�l�߂v TO ����ԍ������v.
020080*
020090*================================================================*
020100 �ی��Ҕԍ��E�l�� SECTION.
020110*
020121     MOVE �ی��Ҕԍ��v    TO  �ی��Ҕԍ����l�߂v.
020130     MOVE ZERO            TO  �ی��Ҕԍ��E�l�߂v.
020140     MOVE ZERO            TO  �ی��Ҕԍ������v.
020150*
020160     MOVE  9  TO  �J�E���^.
020170*
020180     IF  �ی��Ҕԍ����l�߂v�P(8) NOT = SPACE
020190         COMPUTE �J�E���^ = �J�E���^  -  1
020200         MOVE �ی��Ҕԍ����l�߂v�P(8)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020210     END-IF.
020220     IF  �ی��Ҕԍ����l�߂v�P(7) NOT = SPACE
020230         COMPUTE �J�E���^ = �J�E���^  -  1
020240         MOVE �ی��Ҕԍ����l�߂v�P(7)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020250     END-IF.
020260     IF  �ی��Ҕԍ����l�߂v�P(6) NOT = SPACE
020270         COMPUTE �J�E���^ = �J�E���^  -  1
020280         MOVE �ی��Ҕԍ����l�߂v�P(6)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020290     END-IF.
020300     IF  �ی��Ҕԍ����l�߂v�P(5) NOT = SPACE
020310         COMPUTE �J�E���^ = �J�E���^  -  1
020320         MOVE �ی��Ҕԍ����l�߂v�P(5)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020330     END-IF.
020340     IF  �ی��Ҕԍ����l�߂v�P(4) NOT = SPACE
020350         COMPUTE �J�E���^ = �J�E���^  -  1
020360         MOVE �ی��Ҕԍ����l�߂v�P(4)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020370     END-IF.
020380     IF  �ی��Ҕԍ����l�߂v�P(3) NOT = SPACE
020390         COMPUTE �J�E���^ = �J�E���^  -  1
020400         MOVE �ی��Ҕԍ����l�߂v�P(3)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020410     END-IF.
020420     IF  �ی��Ҕԍ����l�߂v�P(2) NOT = SPACE
020430         COMPUTE �J�E���^ = �J�E���^  -  1
020440         MOVE �ی��Ҕԍ����l�߂v�P(2)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020450     END-IF.
020460     IF  �ی��Ҕԍ����l�߂v�P(1) NOT = SPACE
020470         COMPUTE �J�E���^ = �J�E���^  -  1
020480         MOVE �ی��Ҕԍ����l�߂v�P(1)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
020490     END-IF.
020500*
020510     MOVE �ی��Ҕԍ��E�l�߂v TO �ی��Ҕԍ������v.
020521*
020530*================================================================*
020540*================================================================*
020550 �L�����l�� SECTION.
020560*
020570***** �L���̖��ʂ�SPACE����菜���āA���l�߂ɂ���B
020580     MOVE SPACE           TO  �L���m�v.
020590     MOVE SPACE           TO  �L�����v.
020600     MOVE SPACE           TO  �L�����l�߂v.
020610*     MOVE ��|�L��        TO  �L�����v.
020620*-----------------------------------------------------------------*
020630     MOVE SPACE TO �A�Í������|�Í����.
020640*
020650*    / �A�Í������|���͏��Z�b�g /
020660     MOVE ��|�L��       TO �A�Í������|�L��.
020670     MOVE ��|�ԍ�       TO �A�Í������|�ԍ�.
020680     MOVE ��|�Í������� TO �A�Í������|�Í�������.
020690*
020700     CALL   �����v���O�������v.
020710     CANCEL �����v���O�������v.
020720*
020730     MOVE �A�Í������|���������L�� TO �L�����v.
020740*
020750*-----------------------------------------------------------------*
020760*
020770     MOVE  ZERO  TO  �J�E���^�Q.
020780     PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 12
020790          IF  �L�����v�P(�J�E���^) NOT = SPACE
020800              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
020810              MOVE �L�����v�P(�J�E���^)  TO  �L�����l�߂v�P(�J�E���^�Q)
020820          END-IF
020830     END-PERFORM.
020840*
020850     MOVE SPACE           TO  �L���o�m�v.
020860     MOVE SPACE           TO  �L�����w�v.
020870     MOVE SPACE           TO  �L�����l�߂w�v.
020880     MOVE �L�����l�߂v    TO  �L�����w�v.
020890*
020900     MOVE  ZERO  TO  �J�E���^�Q.
020910     PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 24
020920          IF  �L�����w�v�P(�J�E���^) NOT = SPACE
020930              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
020940              MOVE �L�����w�v�P(�J�E���^)  TO  �L�����l�߂w�v�P(�J�E���^�Q)
020950          END-IF
020960     END-PERFORM.
020970*
020980     MOVE �L�����l�߂w�v    TO �L���o�m�v.
020990*
021000*���p�X�y�[�X��S�p�ɂ�����
021010*    INSPECT �L���v REPLACING ALL ���p�� BY �S�p��.
021020*
021030*================================================================*
021040*================================================================*
021050 �������擾 SECTION.
021060*
021070***********************************************
021080* �����f�[�^�Z�b�g                            *
021090***********************************************
021100*    ****************************************************************
021110*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
021120*    ****************************************************************
021130     INITIALIZE �����P�v�q.
021140     INITIALIZE �����Q�v�q.
021150     INITIALIZE �����R�v�q.
021160*
021170     PERFORM �����ʒ������擾.
021171*
021172     IF ���Z�|���Z��� = 1 OR 2
021173         MOVE 1                        TO ��Ï����敪�v
021174     ELSE
021175         MOVE 3                        TO ��Ï����敪�v
021176     END-IF.
021177*
021185     MOVE ���Z�|���Z������             TO �S�̎������v.
021186     IF ���Z�|�����敪 = ZERO OR 1
021187         MOVE ZERO                     TO �Đ����敪�v
021188     ELSE
021189         MOVE 1                        TO �Đ����敪�v
021190     END-IF.
021191*
021192     MOVE ���Z�|���É�               TO  ���É񐔂v.
021193     MOVE ���Z�|���Ë���               TO  ���Ë����v.
021194* �P��100m
021200     COMPUTE  ���Ë����Q�v  =  ���Ë����v * 10.
021210*
021211     MOVE ���Z�|���񏈒u��(1)          TO ���񏈒u���v�q(1).
021212     MOVE ���Z�|���񏈒u��(2)          TO ���񏈒u���v�q(2).
021213     MOVE ���Z�|���񏈒u��(3)          TO ���񏈒u���v�q(3).
021214     MOVE ���Z�|���񏈒u��(4)          TO ���񏈒u���v�q(4).
021215     MOVE ���Z�|���񏈒u��(5)          TO ���񏈒u���v�q(5).
021216*
021217     MOVE ���Z�|������                 TO �������v  .
021218     MOVE ���Z�|�������Z��             TO �������Z���v  .
021219     MOVE ���Z�|���������k��           TO ���������k���v  .
021220     MOVE ���Z�|�Č���                 TO �Č����v�q.
021221     MOVE ���Z�|���×�                 TO ���×��v  .
021222     MOVE ���Z�|���É��Z��             TO ���É��Z���v  .
021223     MOVE ���Z�|�������q���Z��         TO �������q���Z���v  .
021224     MOVE ���Z�|�{�p���񋟗�         TO ���񋟗��v      .
      */���׏����s�̐����Z�ǉ�/20221012
           MOVE ���Z�|���׏����s���Z��       TO ���׏����s�v.
           MOVE ���Z�|���׏����s���Z��       TO ���׏����s���v.
           IF ���Z�|���׏����s���Z�� NOT = ZERO
               MOVE 1                        TO ���׏����s�񐔂v
               MOVE ���Z�|�{�p��             TO ���׏����s���v
           END-IF.
021227*
021228********************
021230* �����������Z�b�g *
021240********************
021250*    **********
021260*    * �P���� *
021270*    **********
021280     MOVE ���Z�|��É񐔂P             TO ��É񐔂P�v�q.
021290     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
021300     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
021310     MOVE ���Z�|�d�É񐔂P             TO �d�É񐔂P�v�q.
021384     MOVE ���Z�|��×��P               TO ��×��P�v�q.
021385     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
021386     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
021387     MOVE ���Z�|�d�×��P               TO �d�×��P�v�q.
021389     MOVE ���Z�|���v�P                 TO ���v�P�v�q.
021390     MOVE ���Z�|�����������P           TO �����������P�v�q.
021392     MOVE ���Z�|���������v�P           TO ���������v�P�v�q.
021393*    **********
021394*    * �Q���� *
021400*    **********
021410     MOVE ���Z�|��É񐔂Q             TO ��É񐔂Q�v�q.
021420     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
021430     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
021440     MOVE ���Z�|�d�É񐔂Q             TO �d�É񐔂Q�v�q.
021511     MOVE ���Z�|��×��Q               TO ��×��Q�v�q.
021512     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
021513     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
021514     MOVE ���Z�|�d�×��Q               TO �d�×��Q�v�q.
021516     MOVE ���Z�|���v�Q                 TO ���v�Q�v�q.
021517     MOVE ���Z�|�����������Q           TO �����������Q�v�q.
021519     MOVE ���Z�|���������v�Q           TO ���������v�Q�v�q.
021520*    ****************
021521*    * �R���ʁ^�W�� *
021530*    ****************
021560     MOVE ���Z�|��É񐔂R�W             TO ��É񐔂R�W�v�q.
021570     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
021580     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
021590     MOVE ���Z�|�d�É񐔂R�W             TO �d�É񐔂R�W�v�q.
021661     MOVE ���Z�|��×��R�W               TO ��×��R�W�v�q.
021662     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
021663     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
021664     MOVE ���Z�|�d�×��R�W               TO �d�×��R�W�v�q.
021666     MOVE ���Z�|���v�R�W                 TO ���v�R�W�v�q.
021667     MOVE ���Z�|�����ʍ����v�R�W         TO �����ʍ����v�R�W�v�q.
021668     MOVE ���Z�|�����������R�W           TO �����������R�W�v�q.
021669     MOVE ���Z�|���������v�R�W           TO ���������v�R�W�v�q.
021670*    ****************
021671*    * �R���ʁ^10�� *
021680*    ****************
021690     MOVE ���Z�|�����J�n�����R�O         TO �����J�n�����R�O�v�q
021691     MOVE ���Z�|��É񐔂R�O             TO ��É񐔂R�O�v�q.
021700     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
021710     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
021720     MOVE ���Z�|�d�É񐔂R�O             TO �d�É񐔂R�O�v�q.
021761     MOVE ���Z�|��×��R�O               TO ��×��R�O�v�q.
021762     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
021763     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
021764     MOVE ���Z�|�d�×��R�O               TO �d�×��R�O�v�q.
021766     MOVE ���Z�|���v�R�O                 TO ���v�R�O�v�q.
021768     MOVE ���Z�|�����������R�O           TO �����������R�O�v�q.
021769     MOVE ���Z�|���������v�R�O           TO ���������v�R�O�v�q.
021834*    ****************
021840*    * �S���ʁ^�T�� *
021850*    ****************
021880     MOVE ���Z�|��É񐔂S�T             TO ��É񐔂S�T�v�q.
021890     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
021900     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
021910     MOVE ���Z�|�d�É񐔂S�T             TO �d�É񐔂S�T�v�q.
021981     MOVE ���Z�|��×��S�T               TO ��×��S�T�v�q.
021982     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
021983     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
021984     MOVE ���Z�|�d�×��S�T               TO �d�×��S�T�v�q.
021986     MOVE ���Z�|���v�S�T                 TO ���v�S�T�v�q.
021987     MOVE ���Z�|�����ʍ����v�S�T         TO �����ʍ����v�S�T�v�q.
021988     MOVE ���Z�|�����������S�T           TO �����������S�T�v�q.
021989     MOVE ���Z�|���������v�S�T           TO ���������v�S�T�v�q.
021990*    ****************
021991*    * �S���ʁ^�W�� *
022000*    ****************
022010     MOVE ���Z�|�����J�n�����S�W         TO �����J�n�����S�W�v�q
022011     MOVE ���Z�|��É񐔂S�W             TO ��É񐔂S�W�v�q.
022020     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
022030     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
022040     MOVE ���Z�|�d�É񐔂S�W             TO �d�É񐔂S�W�v�q.
022111     MOVE ���Z�|��×��S�W               TO ��×��S�W�v�q.
022112     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
022113     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
022114     MOVE ���Z�|�d�×��S�W               TO �d�×��S�W�v�q.
022116     MOVE ���Z�|���v�S�W                 TO ���v�S�W�v�q.
022118     MOVE ���Z�|�����ʍ����v�S�W         TO �����ʍ����v�S�W�v�q.
022119     MOVE ���Z�|�����������S�W           TO �����������S�W�v�q.
022120     MOVE ���Z�|���������v�S�W           TO ���������v�S�W�v�q.
022121*    ****************
022122*    * �S���ʁ^10�� *
022130*    ****************
022140     MOVE ���Z�|�����J�n�����S�O         TO �����J�n�����S�O�v�q
022141     MOVE ���Z�|��É񐔂S�O             TO ��É񐔂S�O�v�q.
022150     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
022160     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
022170     MOVE ���Z�|�d�É񐔂S�O             TO �d�É񐔂S�O�v�q.
022211     MOVE ���Z�|��×��S�O               TO ��×��S�O�v�q.
022212     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
022213     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
022214     MOVE ���Z�|�d�×��S�O               TO �d�×��S�O�v�q.
022216     MOVE ���Z�|���v�S�O                 TO ���v�S�O�v�q.
022218     MOVE ���Z�|�����������S�O           TO �����������S�O�v�q.
022219     MOVE ���Z�|���������v�S�O           TO ���������v�S�O�v�q.
022294*    *****************
022295*    * �T���ʁ^2.5�� *
022300*    *****************
022330     MOVE ���Z�|��É񐔂T�Q             TO ��É񐔂T�Q�v�q.
022340     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
022350     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
022360     MOVE ���Z�|�d�É񐔂T�Q             TO �d�É񐔂T�Q�v�q.
022441     MOVE ���Z�|��×��T�Q               TO ��×��T�Q�v�q.
022442     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
022443     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
022444     MOVE ���Z�|�d�×��T�Q               TO �d�×��T�Q�v�q.
022446     MOVE ���Z�|���v�T�Q                 TO ���v�T�Q�v�q.
022448     MOVE ���Z�|�����ʍ����v�T�Q         TO �����ʍ����v�T�Q�v�q.
022449     MOVE ���Z�|�����������T�Q           TO �����������T�Q�v�q.
022450     MOVE ���Z�|���������v�T�Q           TO ���������v�T�Q�v�q.
022451*    ****************
022452*    * �T���ʁ^�T�� *
022453*    ****************
022460     MOVE ���Z�|�����J�n�����T�T         TO �����J�n�����T�T�v�q
022461     MOVE ���Z�|��É񐔂T�T             TO ��É񐔂T�T�v�q.
022470     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
022480     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
022490     MOVE ���Z�|�d�É񐔂T�T             TO �d�É񐔂T�T�v�q.
022561     MOVE ���Z�|��×��T�T               TO ��×��T�T�v�q.
022562     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
022563     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
022564     MOVE ���Z�|�d�×��T�T               TO �d�×��T�T�v�q.
022566     MOVE ���Z�|���v�T�T                 TO ���v�T�T�v�q.
022568     MOVE ���Z�|�����ʍ����v�T�T         TO �����ʍ����v�T�T�v�q.
022569     MOVE ���Z�|�����������T�T           TO �����������T�T�v�q.
022570     MOVE ���Z�|���������v�T�T           TO ���������v�T�T�v�q.
022571*    ****************
022572*    * �T���ʁ^�W�� *
022580*    ****************
022590     MOVE ���Z�|�����J�n�����T�W         TO �����J�n�����T�W�v�q
022591     MOVE ���Z�|��É񐔂T�W             TO ��É񐔂T�W�v�q.
022600     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
022610     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
022620     MOVE ���Z�|�d�É񐔂T�W             TO �d�É񐔂T�W�v�q.
022691     MOVE ���Z�|��×��T�W               TO ��×��T�W�v�q.
022692     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
022693     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
022694     MOVE ���Z�|�d�×��T�W               TO �d�×��T�W�v�q.
022696     MOVE ���Z�|���v�T�W                 TO ���v�T�W�v�q.
022697     MOVE ���Z�|�����ʍ����v�T�W         TO �����ʍ����v�T�W�v�q.
022698     MOVE ���Z�|�����������T�W           TO �����������T�W�v�q.
022699     MOVE ���Z�|���������v�T�W           TO ���������v�T�W�v�q.
022700*    ****************
022701*    * �T���ʁ^10�� *
022710*    ****************
022720     MOVE ���Z�|��É񐔂T�O             TO ��É񐔂T�O�v�q.
022730     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
022740     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
022750     MOVE ���Z�|�d�É񐔂T�O             TO �d�É񐔂T�O�v�q.
022791     MOVE ���Z�|��×��T�O               TO ��×��T�O�v�q.
022792     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
022793     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
022794     MOVE ���Z�|�d�×��T�O               TO �d�×��T�O�v�q.
022796     MOVE ���Z�|���v�T�O                 TO ���v�T�O�v�q.
022798     MOVE ���Z�|�����������T�O           TO �����������T�O�v�q.
022799     MOVE ���Z�|���������v�T�O           TO ���������v�T�O�v�q.
022908*
      */�������q�ύX/20180611
           COMPUTE �������q�񐔂v = ���Z�|�� + ���Z�|�� + ���Z�|��.
           IF �������q�񐔂v > 9
               MOVE 9 TO �������q�񐔂v
           END-IF.
      */�^����Òǉ�/20180611
           MOVE ���Z�|�^����É�           TO �^����×��񐔂v.
           MOVE ���Z�|�^����×�             TO �^����×��v.
022910*================================================================*
022920 �����f�[�^�擾 SECTION.
022930*
022940     INITIALIZE �������v.
022950*
022960     MOVE �{�p�a��v�q       TO ���|�{�p�a��.
022970     MOVE �{�p�N�v�q         TO ���|�{�p�N.
022980     MOVE �{�p���v�q         TO ���|�{�p��.
022990     MOVE ���҃R�[�h�v�q     TO ���|���҃R�[�h.
023000     READ �����f�[�^�e
023010     INVALID KEY
023020         CONTINUE
023030     NOT INVALID KEY
023040         MOVE ���|���ʐ�                   TO ���ʐ��v
023050         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
023060                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
023070             MOVE ���|�������(���ʂb�m�s) TO ������ʂv(���ʂb�m�s)
023080             MOVE ���|����(���ʂb�m�s)     TO ���ʂv(���ʂb�m�s)
023090             MOVE ���|���E�敪(���ʂb�m�s) TO ���E�敪�v(���ʂb�m�s)
023100             MOVE ���|�����ʒu�ԍ�(���ʂb�m�s)
023110                                           TO �����ʒu�ԍ��v(���ʂb�m�s)
023120* �������
023130             MOVE SPACE                     TO �������̂v
023140             MOVE 03                        TO ���|�敪�R�[�h
023150             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
023160             READ ���̃}�X�^
023170             INVALID KEY
023180                 MOVE SPACE        TO ���{��ϊ��v�m
023190             NOT INVALID KEY
023200                 MOVE ���|�������� TO ���{��ϊ��v�m
023210             END-READ
023211             MOVE ���{��ϊ��v�w   TO �������̂v
023220* ����
023230             STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
023240                    �������̂v                    DELIMITED BY SPACE
023250                    ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
023260                    INTO �������v(���ʂb�m�s)
023270             END-STRING
023280*
023290             MOVE ���|�����a��(���ʂb�m�s)   TO �����a��v(���ʂb�m�s)
023300             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
023310             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
023320             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
023330             MOVE ���|�J�n�a��(���ʂb�m�s)   TO �����a��v(���ʂb�m�s)
023340             MOVE ���|�J�n�N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
023350             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
023360             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
023370             IF ���|�]�A�敪(���ʂb�m�s) = 9
023380                 MOVE 99                   TO �I���N�v(���ʂb�m�s)
023390                 MOVE 99                   TO �I�����v(���ʂb�m�s)
023400                 MOVE 99                   TO �I�����v(���ʂb�m�s)
023410             ELSE
023420                 MOVE ���|�I���a��(���ʂb�m�s)   TO �I���a��v(���ʂb�m�s)
023430                 MOVE ���|�I���N(���ʂb�m�s)   TO �I���N�v(���ʂb�m�s)
023440                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
023450                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
023460             END-IF
023470*
023480             MOVE ���|�]�A�敪(���ʂb�m�s) TO �]�A�敪�v(���ʂb�m�s)
023490*
023500* �o�ߗ��̎擾
023501             MOVE 01                         TO �o�|�敪�R�[�h
023502             MOVE ���|�o�߃R�[�h(���ʂb�m�s) TO �o�|�o�߃R�[�h
023503             READ �o�߃}�X�^
023504             INVALID KEY
023507                 MOVE SPACE           TO �����o�ߕ��ʂv(���ʂb�m�s)
023508             NOT INVALID KEY
023509                 EVALUATE ���ʂb�m�s
023510                 WHEN 1
023511                     MOVE NC"�@" TO �o�ߕ��ʂv
023512                 WHEN 2
023513                     MOVE NC"�A" TO �o�ߕ��ʂv
023514                 WHEN 3
023515                     MOVE NC"�B" TO �o�ߕ��ʂv
023516                 WHEN 4
023517                     MOVE NC"�C" TO �o�ߕ��ʂv
023518                 WHEN 5
023519                     MOVE NC"�D" TO �o�ߕ��ʂv
023520                 END-EVALUATE
023521                 STRING  �o�ߕ��ʂv     DELIMITED BY SPACE
023522                         �o�|�o�ߗ���   DELIMITED BY SPACE
023523                        INTO ���{��ϊ��v�m
023524                 END-STRING
023525                 MOVE ���{��ϊ��v�w TO �����o�ߕ��ʂv(���ʂb�m�s)
023532             END-READ
023533         END-PERFORM
023534
023535* �V�K/�p�� �`�F�b�N
023536         EVALUATE ���Z�|���Z�����敪
023537         WHEN 1
023538             MOVE 1                   TO �V�K�敪�v
023539         WHEN 2
023540             MOVE 1                   TO �p���敪�v
023541         WHEN 3
023542             MOVE 1                   TO �V�K�敪�v
023543             MOVE 1                   TO �p���敪�v
023544         WHEN OTHER
023545             MOVE 1                   TO �p���敪�v
023546         END-EVALUATE
023570
023571         PERFORM �������ȑO�̃f�[�^����
023580* �}�Ԕ���p
023590         MOVE ���|�J�n�f�Ó��蓮�敪 TO  �J�n�f�Ó��蓮�敪�v
023600*
023601* ������������敪
023602         MOVE ���|���Z������������敪 TO ���Z������������敪�v
023603         MOVE ���|���Z�������R����敪 TO ���Z�������R����敪�v
023604*
023610     END-READ.
023620*
023630*================================================================*
023640 �������ȑO�̃f�[�^���� SECTION.
023650*
023660*********************************************************************************
023670*  �ŏ��̏������ȑO�̓������Ɏ{�p�L�^���R�[�h����������(�����A���~)�́A�����敪��
023680*  �p���ɂ��`�F�b�N����B(�V�K�ƌp���̗���)
023690*********************************************************************************
023700** �ŏ��̏��������擾
023710     MOVE SPACE                 TO �����t���O.
023720     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
023730     MOVE �}�Ԃv�q              TO �{�L�|�}��.
023740     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
023750     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
023760     MOVE �{�p���v�q            TO �{�L�|�{�p��.
023770     MOVE ZERO                  TO �{�L�|�{�p��.
023780     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
023790                                  �{�L�|�{�p�a��N����
023800     END-START.
023810     IF ��ԃL�[ = "00"
023820         MOVE ZERO  TO �����a��v�s
023830         MOVE ZERO  TO �����N�v�s
023840         MOVE ZERO  TO �������v�s
023850         MOVE ZERO  TO �������v�s
023860         MOVE SPACE TO �I���t���O�Q
023870         PERFORM �{�p�L�^�e�Ǎ�
023880         PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
023890                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
023900                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
023910                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
023920                       ( �{�L�|�{�p��     NOT = �{�p���v�q      ) OR
023930                       ( �����t���O           = "YES"           ) 
023940               IF  �{�L�|�f�Ë敪 = 2
023950                   MOVE �{�L�|�{�p�a��           TO �����a��v�s
023960                   MOVE �{�L�|�{�p�N             TO �����N�v�s
023970                   MOVE �{�L�|�{�p��             TO �������v�s
023980                   MOVE �{�L�|�{�p��             TO �������v�s
023990                   MOVE "YES"                    TO �����t���O
024000               END-IF
024010               PERFORM �{�p�L�^�e�Ǎ�
024020         END-PERFORM
024030     END-IF.
024040*
024050* �������ȑO�̃f�[�^����
024060     IF �����t���O = "YES"
024070        MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
024080        MOVE �}�Ԃv�q              TO �{�L�|�}��
024090        MOVE �����a��v�s          TO �{�L�|�{�p�a��
024100        MOVE �����N�v�s            TO �{�L�|�{�p�N
024110        MOVE �������v�s            TO �{�L�|�{�p��
024120        MOVE �������v�s            TO �{�L�|�{�p��
024130        START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
024140                                     �{�L�|�{�p�a��N����
024150                                     REVERSED
024160        END-START
024170        IF ��ԃL�[ = "00"
024180           MOVE SPACE  TO �I���t���O�Q
024190           PERFORM �{�p�L�^�e�Ǎ�
024200           IF ( �I���t���O�Q    = SPACE        ) AND
024210              ( �{�L�|���Ҕԍ�  = ���Ҕԍ��v�q ) AND
024220              ( �{�L�|�}��      = �}�Ԃv�q     ) AND
024230              ( �{�L�|�{�p�a��  = �����a��v�s ) AND
024240              ( �{�L�|�{�p�N    = �����N�v�s   ) AND
024250              ( �{�L�|�{�p��    = �������v�s   )
024260*  �������ȑO�̓������Ɏ{�p�L�^���R�[�h����������
024270                IF �p���敪�v = ZERO
024280                   MOVE 1    TO �p���敪�v
024290                END-IF
024300           END-IF
024310         END-IF
024320     END-IF.
024330*
024340*================================================================*
024350 �{�p�L�^�擾 SECTION.
024360*
024370     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���ʐ��v
024380         IF ( �{�p�N�v�q = �����N�v(���ʂb�m�s) ) AND
024390            ( �{�p���v�q = �������v(���ʂb�m�s) )
024400             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
024410             MOVE �}�Ԃv�q              TO �{�L�|�}��
024420             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
024430             MOVE �����a��v(���ʂb�m�s)  TO �J�n�a��v(���ʂb�m�s)
024440             MOVE �����N�v(���ʂb�m�s)  TO �J�n�N�v(���ʂb�m�s) �{�L�|�{�p�N
024450             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
024460             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
024470         ELSE
024480             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
024490             MOVE �}�Ԃv�q              TO �{�L�|�}��
024500             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
024510             MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
024520             MOVE �{�p���v�q            TO �{�L�|�{�p��
024530             MOVE ZERO                  TO �{�L�|�{�p��
024540         END-IF
024550         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
024560                                      �{�L�|�{�p�a��N����
024570         END-START
024580         IF ��ԃL�[ = "00"
024590             MOVE ZERO  TO �������v(���ʂb�m�s)
024600             MOVE ZERO  TO ���񏈒u�񐔂v(���ʂb�m�s)
024610             MOVE ZERO  TO �I���a��v�s
024620             MOVE ZERO  TO �I���N�v�s
024630             MOVE ZERO  TO �I�����v�s
024640             MOVE ZERO  TO �I�����v�s
024650             MOVE SPACE TO �I���t���O�Q
024660             PERFORM �{�p�L�^�e�Ǎ�
024670             IF  ( �I���t���O�Q      = SPACE   ) AND
024680                 ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
024690                 ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
024700                 ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
024710                 ( �{�L�|�{�p��      = �{�p���v�q     ) 
024720*
024730*        ************
024740*        * �������q *
024750*        ************
      */�������q�ύX������/20180611
024760*             EVALUATE �{�L�|�������q�敪(���ʂb�m�s)
024770*             WHEN 1
024780*                 COMPUTE ��񐔂v = ��񐔂v + 1
024790*             WHEN 2
024800*                 COMPUTE ���񐔂v = ���񐔂v + 1
024810*             WHEN 3
024820*                 COMPUTE ���񐔂v = ���񐔂v + 1
024830*             END-EVALUATE
      */�������q�ύX������/20180611
024840*        ****************
024850*        * ���񋟉� *
024860*        ****************
024870             IF �{�L�|���񋟋敪(���ʂb�m�s) = 1
024880                 COMPUTE ���񋟗��񐔂v = ���񋟗��񐔂v + 1
024890             END-IF
024900*        *****************************************************************
024910*        * �J�n�N���� ( ���̕��ʂ����������łȂ����A
024920*                       ���������ł��}�Ԃ����鎞�́A�ŏ��̎{�p�����J�n��)*
024930*        *****************************************************************
024940                 IF ( �{�p�N�v�q NOT = �����N�v(���ʂb�m�s) ) OR
024950                    ( �{�p���v�q NOT = �������v(���ʂb�m�s) ) OR
024960                    ( �J�n�f�Ó��蓮�敪�v = 1 )
024970                     MOVE �{�L�|�{�p�a�� TO �J�n�a��v(���ʂb�m�s)
024980                     MOVE �{�L�|�{�p�N   TO �J�n�N�v(���ʂb�m�s)
024990                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
025000                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
025010                 END-IF
025020             END-IF
025030             PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
025040                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
025050                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
025060                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
025070                           ( �{�L�|�{�p��     NOT = �{�p���v�q       ) OR
025080                           ( �{�L�|�{�p��         > �I�����v(���ʂb�m�s))
025090*               **********
025100*               * ������ *
025110*               **********
025120                COMPUTE �������v(���ʂb�m�s) = �������v(���ʂb�m�s) + 1
025130                MOVE �{�L�|�{�p�a��             TO �I���a��v�s
025140                MOVE �{�L�|�{�p�N               TO �I���N�v�s
025150                MOVE �{�L�|�{�p��               TO �I�����v�s
025160                MOVE �{�L�|�{�p��               TO �I�����v�s
025170*            /�@���񏈒u�̃J�E���g�@/
025180                IF �{�L�|�����{�Ë敪(���ʂb�m�s) = 1
025190                    COMPUTE ���񏈒u�񐔂v(���ʂb�m�s) = ���񏈒u�񐔂v(���ʂb�m�s) + 1
025200                END-IF
025210*
025220                PERFORM �{�p�L�^�e�Ǎ�
025230            END-PERFORM
025240        END-IF
025250*       **************************
025260*       * �p���F�I���N�����Z�b�g *
025270*       **************************
025280        IF �]�A�敪�v(���ʂb�m�s) = 9
025290            MOVE �I���a��v�s  TO �I���a��v(���ʂb�m�s)
025300            MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
025310            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
025320            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
025330        END-IF
025340     END-PERFORM.
025350***
025360     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
025370     MOVE �}�Ԃv�q              TO �{�L�|�}��.
025380     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
025390     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
025400     MOVE �{�p���v�q            TO �{�L�|�{�p��.
025410     MOVE ZERO                  TO �{�L�|�{�p��.
025420     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
025430                                  �{�L�|�{�p�a��N����
025440     END-START.
025450     IF ��ԃL�[ = "00"
025460         MOVE SPACE TO �I���t���O�Q
025470         PERFORM �{�p�L�^�e�Ǎ�
025480         PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
025490                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
025500                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
025510                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
025520                       ( �{�L�|�{�p��     NOT = �{�p���v�q       )
025530*        ************
025540*        * ������ *
025550*        ************
025560             IF �{�L�|�����������敪 = 1
025570                 COMPUTE �����񐔂v = �����񐔂v + 1
025580             END-IF
025590*        ************
025600*        * �������Z *
025610*        ************
025620             EVALUATE �{�L�|�������Z
025630             WHEN 1
025640                 COMPUTE �������ԊO�񐔂v = �������ԊO�񐔂v + 1
025650             WHEN 2
025660                 COMPUTE �����x���񐔂v   = �����x���񐔂v + 1
025670             WHEN 3
025680                 COMPUTE �����[��񐔂v   = �����[��񐔂v + 1
025690             END-EVALUATE
025700*        ************
025710*        * �Č��� *
025720*        ************
025730             IF �{�L�|�Č������� = 1
025740                 COMPUTE �Č��񐔂v = �Č��񐔂v + 1
025750             END-IF
025760*        ************
025770*        * ���É��Z *
025780*        ************
025790             EVALUATE �{�L�|���É��Z
025800             WHEN 1
025810                 COMPUTE ���Ö�Ԃv = ���Ö�Ԃv + 1
025820             WHEN 2
025830                 COMPUTE ���Ó�H�v = ���Ó�H�v + 1
025840             WHEN 3
025850                 COMPUTE ���Ö\���v = ���Ö\���v + 1
025860             END-EVALUATE
025870*        ****************
025880*        * ���������k�� *
025890*        ****************
025900             IF (�{�L�|�f�Ë敪 = 2 ) AND (�{�L�|���������k���敪 NOT = 1)
025910                 COMPUTE ���k�x���񐔂v = ���k�x���񐔂v + 1
025920             END-IF
025930*        **********
025940*        * �{�p�� *
025950*        **********
025960             MOVE 1 TO �{�p���v(�{�L�|�{�p��)
025970*
025980             PERFORM �{�p�L�^�e�Ǎ�
025990         END-PERFORM
026000     END-IF.
026010*
026020*================================================================*
026030 ����{�p�N���擾 SECTION.
026040* 
026050     MOVE ZERO          TO ����N���v  ����{�p�N���v.
026060     MOVE ��|�{�p�a��  TO ���|�����敪.
026070     READ �����}�X�^
026080     NOT INVALID KEY
026090         MOVE ���|�J�n����N TO ����N�v
026100     END-READ.
026110**
026120     IF ����N�v = ZERO
026130          MOVE  NC"�����}�X�^�ɊJ�n����N��o�^���ĉ�����" TO �A���|���b�Z�[�W
026140          CALL   "MSG001"
026150          CANCEL "MSG001"
026160          PERFORM �t�@�C����
026170          MOVE 99 TO PROGRAM-STATUS
026180          EXIT PROGRAM
026190     ELSE
026200          COMPUTE ����N�v = ����N�v + ��|�{�p�N - 1
026210          MOVE ��|�{�p�� TO ����v
026220     END-IF.
026230*
026240     MOVE ����N���v   TO  ����{�p�N���v.
026250*
026260*================================================================*
026270 ����N�����擾 SECTION.
026280*
026290     MOVE ZERO  TO �v�Z����N�����v.
026300*
026310     IF �v�Z�a��v  NOT = ZERO
026320         MOVE �v�Z�a��v    TO ���|�����敪
026330         READ �����}�X�^
026340         NOT INVALID KEY
026350             MOVE ���|�J�n����N TO �v�Z����N�v
026360         END-READ
026370**
026380         IF �v�Z����N�v = ZERO
026390              MOVE  NC"�����}�X�^�ɊJ�n����N��o�^���ĉ�����" TO �A���|���b�Z�[�W
026400              CALL   "MSG001"
026410              CANCEL "MSG001"
026420              PERFORM �t�@�C����
026430              MOVE 99 TO PROGRAM-STATUS
026440              EXIT PROGRAM
026450         ELSE
026460              COMPUTE �v�Z����N�v = �v�Z����N�v + �v�Z�N�v - 1
026470              MOVE �v�Z���v TO �v�Z����v
026480              MOVE �v�Z���v TO �v�Z������v
026490         END-IF
026500     END-IF.
026510*
026520*================================================================*
026530 �������S�Ҕԍ��擾 SECTION.
026540*
026550*--------------------------------------------------------------------------
026560* ���S�Ҕԍ��� ��а 99XXXXXX�� 26XXXXXX �ɂ���B
026570* ���S�Ҕԍ��������ȊO�Ŏn�܂�(��-)���́A26XXXXXX �ɂ���BXXXXXX�́A���۔ԍ�
026580*   XXXXXX�́A�s�����}�X�^�̒��� �ی��Ҕԍ� ���g�p����B(Ͻ��ɓ��͂��Ă���)
026590*--------------------------------------------------------------------------
026600*
026613     PERFORM �����ԍ����l��.
026620*
026630     IF �����ԍ��v(1:2)  = "99"
026640*  / ��а �ԍ� /
026650         MOVE �����ԍ��v              TO �ی��Ҕԍ��v
026660         MOVE "26"                    TO �ی��Ҕԍ��v(1:2)
026670         PERFORM �ی��Ҕԍ��E�l��
026681         MOVE �ی��Ҕԍ������v        TO ��P�|�������S�Ҕԍ�
026690     ELSE
026700*  / ���� /
026710         IF �����ԍ��v(1:1)  = "0" OR "1" OR "2" OR "3" OR "4" OR
026720                               "5" OR "6" OR "7" OR "8" OR "9" OR SPACE
026731             MOVE �����ԍ��v         TO �ی��Ҕԍ��v
026740             PERFORM �ی��Ҕԍ��E�l��
026751             MOVE �ی��Ҕԍ������v   TO ��P�|�������S�Ҕԍ�
026760         ELSE
026770*  / �����ȊO /
026780             MOVE ��|�������       TO �s�|������
026790             MOVE �����ԍ��v         TO �s�|�s�����ԍ�
026800             READ �s�����}�X�^
026810             INVALID KEY
026821                 MOVE SPACE          TO ��P�|�������S�Ҕԍ�
026830             NOT INVALID KEY
026840                 MOVE SPACE          TO �ی��Ҕԍ��v
026850                 MOVE "26"           TO �ی��Ҕԍ��v(1:2)
026860                 MOVE �s�|�ی��Ҕԍ� TO �ی��Ҕԍ��v(3:6)
026870                 PERFORM �ی��Ҕԍ��E�l��
026881                 MOVE �ی��Ҕԍ������v   TO ��P�|�������S�Ҕԍ�
026890             END-READ
026900         END-IF
026910     END-IF.
026920*
026930*================================================================*
026940*================================================================*
026950 �����ԍ����l�� SECTION.
026960*
026970***** �����̕��S�Ҕԍ��̖��ʂ�SPACE����菜���āA���l�߂ɂ���B
026980     MOVE SPACE           TO  �����ԍ��v.
026990     MOVE SPACE           TO  �����ԍ����v.
027000     MOVE SPACE           TO  �����ԍ����l�߂v.
027010     MOVE ��|��p���S�Ҕԍ�����   TO  �����ԍ����v.
027020*
027030     MOVE  ZERO  TO  �J�E���^�Q.
027040     PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 10
027050          IF  �����ԍ����v�P(�J�E���^) NOT = SPACE
027060              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
027070              MOVE �����ԍ����v�P(�J�E���^)  TO  �����ԍ����l�߂v�P(�J�E���^�Q)
027080          END-IF
027090     END-PERFORM.
027100*
027110     MOVE �����ԍ����l�߂v    TO �����ԍ��v.
027120*
027130*================================================================*
027140 ������ʕϊ� SECTION.
027150*
027160     MOVE ZERO  TO ������ʕϊ���v.
027170*
027180     EVALUATE ������ʕϊ��O�v
027190     WHEN  ZERO
027200        MOVE ZERO TO ������ʕϊ���v
027210* �P��
027220     WHEN  01
027230        MOVE  4   TO ������ʕϊ���v
027240* �Ŗo
027250     WHEN  02
027260        MOVE  5   TO ������ʕϊ���v
027270* ����
027280     WHEN  03
027290        MOVE  6   TO ������ʕϊ���v
027300* �E�P
027310     WHEN  04
027320        MOVE  3   TO ������ʕϊ���v
027330* ����
027340     WHEN  05
027350        MOVE  1   TO ������ʕϊ���v
027360* �s�S����
027370     WHEN  06
027380        MOVE  2   TO ������ʕϊ���v
027390* ���܁E�s�S���܍S�k
027400     WHEN  07
027410     WHEN  08
027420        MOVE  7   TO ������ʕϊ���v
027430* �������Ȃ��i���a�j
027440     WHEN  09
027450        MOVE  9   TO ������ʕϊ���v
027460     WHEN OTHER
027470        CONTINUE
027480     END-EVALUATE.
027490*
027502*================================================================*
027510 �ی���ʕϊ� SECTION.
027520*
027530     MOVE ZERO  TO �ی���ʕϊ���v.
027540*
027550     EVALUATE �ی���ʕϊ��O�v
027560     WHEN  ZERO
027570        MOVE ZERO TO �ی���ʕϊ���v
027580* ����
027590     WHEN  1
027600        MOVE  4   TO �ی���ʕϊ���v
027601* ������
027602     WHEN  2
027603        MOVE  1   TO �ی���ʕϊ���v
027604* ���ۑg��
027605     WHEN  3
027606        MOVE  2   TO �ی���ʕϊ���v
027607* ���ϑg��
027608     WHEN  4
027609        MOVE  3   TO �ی���ʕϊ���v
027610* �������
027611     WHEN  5
027612        MOVE  6   TO �ی���ʕϊ���v
027613* �ސE
027614     WHEN  8
027615        MOVE  5   TO �ی���ʕϊ���v
027619* ���̑�
027720     WHEN OTHER
027740        MOVE 9    TO �ی���ʕϊ���v
027741     END-EVALUATE.
027750*
027760*================================================================*
027761*================================================================*
027762 �]�A�敪�ϊ� SECTION.
027763*
027764     MOVE ZERO  TO �]�A�ϊ���v.
027765*
027766     EVALUATE �]�A�ϊ��O�v
027767     WHEN  ZERO
027768        MOVE ZERO TO �]�A�ϊ���v
027769* ����
027770     WHEN  1
027771     WHEN  2
027772     WHEN  5
027773        MOVE  1   TO �]�A�ϊ���v
027774* ���~
027775     WHEN  3
027776        MOVE  2   TO �]�A�ϊ���v
027777* �]��
027778     WHEN  4
027779        MOVE  3   TO �]�A�ϊ���v
027780* �p��
027781     WHEN  9
027782        MOVE  ZERO TO �]�A�ϊ���v
027783     WHEN OTHER
027784        CONTINUE
027785     END-EVALUATE.
027786*
027787*================================================================*
027788 ������������Ώ۔��菈�� SECTION.
027789*------------------------------------------------------------------------------------*
027790* ����}�X�^�́u������������敪�v�� 3 �i�R���ʈȏ����j�̎��A�R���ʈȏォ���肵�āA
027791* ���̎��̂݁A�����������������B
027792*------------------------------------------------------------------------------------*
027793*
027794     MOVE  SPACE TO  �A���Z������|�L�[.
027795     INITIALIZE      �A���Z������|�L�[.
027796     MOVE �{�p�a��v�q  TO  �A���Z������|�{�p�a��.
027797     MOVE �{�p�N�v�q    TO  �A���Z������|�{�p�N.
027798     MOVE �{�p���v�q    TO  �A���Z������|�{�p��.
027799     MOVE ���Ҕԍ��v�q  TO  �A���Z������|���Ҕԍ�.
027800     MOVE �}�Ԃv�q      TO  �A���Z������|�}��.
027801     CALL   "RECEHUGE".
027802     CANCEL "RECEHUGE".
027803*
027804     IF �A���Z������|�Ώۃt���O = "YES"
027805        PERFORM ���������擾
027806     END-IF.
027807*
027808*================================================================*
027809 ���������擾 SECTION.
027810*
027811********************************************************************
027812*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
027820*  ��: �@�A �Ƃœ]��.
027830*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
027840*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
027850********************************************************************
027860     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
027870     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
027880             UNTIL ( ���ʂb�m�s > ���ʐ��v )
027890*
027900****        IF ( ���|�������Ҕԍ�(���ʂb�m�s)  NOT = ZERO )  AND
027910        IF ( ���|�����A��(���ʂb�m�s)      NOT = ZERO )
027920*
027930           IF �J�E���^ = ZERO
027940               MOVE 1   TO  �J�E���^ �J�E���^�Q
027950               MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
027960               MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
027970               MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
027980           ELSE
027990              IF ( ���|�������Ҕԍ�(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
028000                 ( ���|�����A��(���ʂb�m�s)      = �����A�Ԃb�v     )
028010                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
028020                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
028030              ELSE
028040                 COMPUTE �J�E���^ = �J�E���^  +  1
028050                 MOVE 1   TO  �J�E���^�Q
028060                 MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
028070                 MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
028080                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
028090              END-IF
028100           END-IF
028110        END-IF
028120     END-PERFORM.
028130**************************************************************************
028140*  ���������}�X�^��蕶�͎擾
028150**************************************************************************
028161     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
028170     PERFORM VARYING �J�E���^ FROM 1 BY 1
028180             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
028190** ���ۂ� �敪 01
028200         MOVE 01                        TO �����|�敪�R�[�h
028210         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
028220         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
028230         READ ���������e
028240         NOT INVALID KEY
028250             INITIALIZE ���������v�s
028260             MOVE �����|���������b�l(1) TO  ���������P�v�s
028270             MOVE �����|���������b�l(2) TO  ���������Q�v�s
028280             MOVE �����|���������b�l(3) TO  ���������R�v�s
028290             MOVE �����|���������b�l(4) TO  ���������S�v�s
028300             MOVE �����|���������b�l(5) TO  ���������T�v�s
028310             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
028320                     UNTIL ( �J�E���^�Q > 9 )  OR 
028330                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
028340                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
028350                WHEN 1
028360                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
028370                WHEN 2
028380                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
028390                WHEN 3
028400                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
028410                WHEN 4
028420                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
028430                WHEN 5
028440                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
028410                WHEN 6
028420                   MOVE "�E"  TO  ���������i���o�[�v�P(�J�E���^�Q)
028430                WHEN 7
028440                   MOVE "�F"  TO  ���������i���o�[�v�P(�J�E���^�Q)
028450                WHEN OTHER
028460                   CONTINUE
028470                END-EVALUATE
028480             END-PERFORM
028490*
028500             IF �����|�����������͋敪 = 1
028510                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
028520                        ���������P�v�s  DELIMITED BY SIZE
028530                        ���������Q�v�s  DELIMITED BY SIZE
028540                        ���������R�v�s  DELIMITED BY SIZE
028550                        ���������S�v�s  DELIMITED BY SIZE
028560                        ���������T�v�s  DELIMITED BY SIZE
028570                        INTO �����������e�����v(�J�E���^)
028580                 END-STRING
028590             ELSE
028600                 INSPECT ���������v�s REPLACING ALL �S�p�� BY ���p��
028610                 MOVE SPACE TO �����P�v �����Q�v
028620                 MOVE ���������i���o�[�m�v TO �����P�v
028630                 MOVE ���������P�v�s       TO �����Q�v
028640                 CALL �v���O�������v WITH C LINKAGE
028650                      USING BY REFERENCE �����P�v
028660                            BY REFERENCE �����Q�v
028670                 MOVE ���������Q�v�s       TO �����Q�v
028680                 CALL �v���O�������v WITH C LINKAGE
028690                      USING BY REFERENCE �����P�v
028700                            BY REFERENCE �����Q�v
028710                 MOVE ���������R�v�s       TO �����Q�v
028720                 CALL �v���O�������v WITH C LINKAGE
028730                      USING BY REFERENCE �����P�v
028740                            BY REFERENCE �����Q�v
028750                 MOVE ���������S�v�s       TO �����Q�v
028760                 CALL �v���O�������v WITH C LINKAGE
028770                      USING BY REFERENCE �����P�v
028780                            BY REFERENCE �����Q�v
028790                 MOVE ���������T�v�s       TO �����Q�v
028800                 CALL �v���O�������v WITH C LINKAGE
028810                      USING BY REFERENCE �����P�v
028820                            BY REFERENCE �����Q�v
028830                  MOVE �����P�v            TO �����������e�����v(�J�E���^)
028841             END-IF
028850*
028860         END-READ
028870     END-PERFORM.
028880*
029220*================================================================*
029230 �������R���擾 SECTION.
029240*
029250* �������R���擾�� "CHOUBUN" ���Ă�. 
029260     MOVE  SPACE TO  �A�����|�L�[.
029270     INITIALIZE      �A�����|�L�[.
029280     MOVE �{�p�a��v�q  TO  �A�����|�{�p�a��.
029290     MOVE �{�p�N�v�q    TO  �A�����|�{�p�N.
029300     MOVE �{�p���v�q    TO  �A�����|�{�p��.
029310     MOVE ���Ҕԍ��v�q  TO  �A�����|���Ҕԍ�.
029320     MOVE �}�Ԃv�q      TO  �A�����|�}��.
029330** ���ڗp��56��
029340     MOVE 56            TO  �A�����|������.
029350*
029360     CALL   "CHOUBUN".
029370     CANCEL "CHOUBUN".
029380*
029790*================================================================*
029791 �������Z�܂Ƃߔ��� SECTION.
029792*---------------------------------------------------------------------------*
029793* �{�̂܂Ƃߋ敪���P
029794* �̎��́A�t���OYES (���z���������݂ň󎚁j
029795*�i��F���l�s�̏�Q�́A�{�̕ی��i���یn�j�̃��Z�v�g�P���Ő����A�������Z�͂Ȃ��j
029796*---------------------------------------------------------------------------*
029797*
029798     MOVE SPACE TO �������Z�܂Ƃ߃t���O.
029799*
029800     IF ���Z�|�{�̂܂Ƃߋ敪 = 1 
029801        MOVE "YES" TO �������Z�܂Ƃ߃t���O
029802     END-IF.
029803*
029851*================================================================*
029852 �����ʒ������擾 SECTION.
029853*
029854     MOVE 01             TO �v�|����敪.
029855     MOVE ���Z�|�{�p�a�� TO �v�|�J�n�a�� �{�p�a��b�v.
029856     MOVE ���Z�|�{�p�N   TO �v�|�J�n�N   �{�p�N�b�v.
029857     MOVE ���Z�|�{�p��   TO �v�|�J�n��   �{�p���b�v.
029858*
029859     START �v�Z�}�X�^ KEY IS <= �v�|����敪 �v�|�J�n�a��N�� REVERSED
029860     END-START.
029861*
029862     IF ��ԃL�[ = "00"
029863         READ �v�Z�}�X�^ NEXT
029864         AT END
029865*/�G���[�\���̏C��
029866             DISPLAY "�{�p�N���ɑΉ������������݂���܂���"
029867                     " ��f�҇�=" ���Z�|���҃R�[�h
029868                     " �{�p�N��=" ���Z�|�{�p�N ���Z�|�{�p��   UPON CONS
029869*-----------------------------------------*
029870             CALL "actcshm"  WITH C LINKAGE
029871*-----------------------------------------*
029872             ACCEPT  �L�[���� FROM CONS
029873             PERFORM �t�@�C����
029874             MOVE ZERO TO PROGRAM-STATUS
029875             EXIT PROGRAM
029876         NOT AT END
029877             IF ( �{�p�a��N���b�v >= �v�`�|�J�n�a��N�� ) AND
029878                ( �{�p�a��N���b�v <= �v�`�|�I���a��N�� )
029879                 MOVE �v�`�|�����ʒ�����(2) TO �Q���ʖڒ������v
029880                 MOVE �v�`�|�����ʒ�����(3) TO �R���ʖڒ������v
029881             ELSE
029882*/�G���[�\���̏C��
029883                 DISPLAY "�{�p�N���ɑΉ������������݂���܂���"
029884                         " ��f�҇�=" ���Z�|���҃R�[�h
029885                         " �{�p�N��=" ���Z�|�{�p�N ���Z�|�{�p��   UPON CONS
029886*-----------------------------------------*
029887                 CALL "actcshm"  WITH C LINKAGE
029888*-----------------------------------------*
029889                 ACCEPT  �L�[���� FROM CONS
029890                 PERFORM �t�@�C����
029891                 MOVE ZERO TO PROGRAM-STATUS
029892                 EXIT PROGRAM
029893             END-IF
029894         END-READ
029895     END-IF.
029896*
029897*================================================================*
029898*================================================================*
029899******************************************************************
029900 END PROGRAM YHP101.
029901******************************************************************
