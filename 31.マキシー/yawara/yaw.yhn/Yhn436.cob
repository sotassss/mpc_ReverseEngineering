000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN436.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*  �U���p�����      �y�_+����޳�ޔŁz
000100*         MED = YHN436P
000101*  �����N���o�[�W����
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2015-02-24
000130 DATE-COMPILED.          2015-02-24
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
000260     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  ���|�����敪
000300                             FILE STATUS              IS  ��ԃL�[
000310                             LOCK        MODE         IS  AUTOMATIC.
000320     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  ���|����敪
000360                             FILE STATUS              IS  ��ԃL�[
000370                             LOCK        MODE         IS  AUTOMATIC.
000380     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS  ���|�敪�R�[�h
000420                                                          ���|���̃R�[�h
000430                             FILE STATUS              IS  ��ԃL�[
000440                             LOCK        MODE         IS  AUTOMATIC.
000450     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS MODE              IS  DYNAMIC
000480                             RECORD KEY               IS �{��|�{�p���ԍ�
000490                             FILE STATUS              IS  ��ԃL�[
000500                             LOCK        MODE         IS  AUTOMATIC.
000510     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000520                             ORGANIZATION             IS  INDEXED
000530                             ACCESS MODE              IS  DYNAMIC
000540                             RECORD KEY               IS  �ہ|�ی����
000550                                                          �ہ|�ی��Ҕԍ�
000560                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000570                                                          �ہ|�ی��Җ���
000580                                                          �ہ|�ی��Ҕԍ�
000590                             FILE STATUS              IS  ��ԃL�[
000600                             LOCK        MODE         IS  AUTOMATIC.
000610     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000620                             ORGANIZATION             IS  INDEXED
000630                             ACCESS MODE              IS  DYNAMIC
000640                             RECORD KEY               IS  �s�|������
000650                                                          �s�|�s�����ԍ�
000660                             ALTERNATE RECORD KEY     IS  �s�|������
000670                                                          �s�|�s��������
000680                                                          �s�|�s�����ԍ�
000690                             FILE STATUS              IS  ��ԃL�[
000700                             LOCK        MODE         IS  AUTOMATIC.
000701     SELECT  ������}�X�^    ASSIGN      TO        SEIKYUSL
000702                             ORGANIZATION             IS  INDEXED
000703                             ACCESS MODE              IS  DYNAMIC
000704                             RECORD KEY               IS  ����|�ی����
000705                                                          ����|�ی��Ҕԍ�
000706                             FILE STATUS              IS  ��ԃL�[
000707                             LOCK    MODE             IS  AUTOMATIC.
000708     SELECT  ����}�X�^    ASSIGN      TO        KAIJOHOL
000709                             ORGANIZATION             IS  INDEXED
000710                             ACCESS MODE              IS  DYNAMIC
00130                             RECORD KEY               IS  ���|�_���I���敪
000131                                                          ���|����R�[�h
000132                                                          ���|�ی����
000133                                                          ���|�ύX�a��N��
000134                             ALTERNATE RECORD KEY     IS  ���|�_���I���敪
000135                                                          ���|�ڍ��t��J�i
000136                                                          ���|����R�[�h
000137                                                          ���|�ی����
000138                                                          ���|�ύX�a��N��
000718                             FILE STATUS              IS  ��ԃL�[
000719                             LOCK        MODE         IS  AUTOMATIC.
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
000033     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000034                             ORGANIZATION             IS  INDEXED
000035                             ACCESS MODE              IS  DYNAMIC
000036                             RECORD KEY               IS ��|�{�p�a��N��
000037                                                          ��|���҃R�[�h
000038                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000039                                                          ��|���҃J�i
000040                                                          ��|���҃R�[�h
000041                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000042                                                         ��|�{�p�a��N��
000043                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000044                                                          ��|�ی����
000045                                                          ��|�ی��Ҕԍ�
000046                                                          ��|���҃R�[�h
000047                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000048                                                          ��|������
000049                                                     ��|��p���S�Ҕԍ�
000050                                                          ��|���҃R�[�h
000051                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000052                                                          ��|�������
000053                                                  ��|��p���S�Ҕԍ�����
000054                                                          ��|���҃R�[�h
000055                             ALTERNATE RECORD KEY  IS ��|�����a��N��
000056                                                      ��|�{�p�a��N��
000057                                                      ��|���҃R�[�h
000058                             FILE STATUS              IS  ��ԃL�[
000059                             LOCK        MODE         IS  AUTOMATIC.
000060     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000061                             ORGANIZATION             IS  INDEXED
000062                             ACCESS MODE              IS  DYNAMIC
000063                             RECORD KEY               IS  ���|�{�p�a��N��
000064                                                          ���|���҃R�[�h
000065                             ALTERNATE RECORD KEY     IS  ���|���҃R�[�h
000066                                                          ���|�{�p�a��N��
000067                             FILE STATUS              IS  ��ԃL�[
000068                             LOCK        MODE         IS  AUTOMATIC.
000069     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
000070                             ORGANIZATION             IS  INDEXED
000071                             ACCESS MODE              IS  DYNAMIC
000072                             RECORD KEY               IS  �{�L�|�{�p�a��N����
000073                                                          �{�L�|���҃R�[�h
000074                             ALTERNATE RECORD KEY     IS  �{�L�|���҃R�[�h
000075                                                          �{�L�|�{�p�a��N����
000076                             FILE STATUS              IS  ��ԃL�[
000077                             LOCK        MODE         IS  AUTOMATIC.
000108     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4312L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  ��Q�|�����a��N��
000112                                                          ��Q�|���ރR�[�h
000112                                                          ��Q�|���R�[�h
000112                                                          ��Q�|�ی���
000113                                                          ��Q�|�ی��Ҕԍ�
000119                             FILE        STATUS       IS  ��ԃL�[
000120                             LOCK        MODE         IS  AUTOMATIC.
      */���я��p�@�U��
000108     SELECT  ��ƃt�@�C���R  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4313L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  ��R�|���ރR�[�h
000112                                                          ��R�|���R�[�h
000112                                                          ��R�|�ی���
000113                                                          ��R�|�ی��Ҕԍ�
000980                             FILE        STATUS       IS  ��ԃL�[
000990                             LOCK        MODE         IS  AUTOMATIC.
000810     SELECT  ����t�@�C��    ASSIGN      TO         GS-PRTF001
000820                             SYMBOLIC    DESTINATION  IS "PRT"
000830                             FORMAT                   IS  ��`�̖��o
000840                             GROUP                    IS  ���ڌQ���o
000850                             PROCESSING  MODE         IS  ������ʂo
000860                             UNIT        CONTROL      IS  �g������o
000870                             FILE        STATUS       IS  �ʒm���o.
000880*
000890******************************************************************
000900*                      DATA DIVISION                             *
000910******************************************************************
000920 DATA                    DIVISION.
000930 FILE                    SECTION.
000940*                           �m�q�k��  �P�Q�W�n
000950 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000960     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000970*                           �m�q�k��  �Q�T�U�n
000980 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
000990     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000991     COPY SEIGYO01        OF  XFDLIB  JOINING   ���O�P   AS  PREFIX.
000991     COPY SEIGYO02        OF  XFDLIB  JOINING   ���O�Q   AS  PREFIX.
001000*                           �m�q�k��  �P�Q�W�n
001010 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001020     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001030*                           �m�q�k��  �P�Q�W�n
001040 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001050     COPY SEJOHO          OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001060*                           �m�q�k��  �R�Q�O�n
001070 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001080     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001090*                           �m�q�k��  �Q�T�U�n
001100 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001110     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
001111*                           �m�q�k��  �P�Q�W�n
001112 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001113     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
001114*                           �m�q�k��  �U�S�O�n
001115 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
001116     COPY KAIJOHO         OF  XFDLIB  JOINING   ��� AS  PREFIX.
      *
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
000129*                           �m�q�k��  �R�Q�O�n
000130 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
000131     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000132*                           �m�q�k��  �Q�T�U�n
000133 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
000134     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
000135*                           �m�q�k��  �P�Q�W�n
000136 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
000137     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001120*
000174*                           �m�q�k��  �P�Q�W�n
000175 FD  ��ƃt�@�C���Q RECORD  CONTAINS 128 CHARACTERS.
000176 01  ��Q�|���R�[�h.
000177     03  ��Q�|���R�[�h�L�[.
000178         05  ��Q�|�����a��N��.
000179             07  ��Q�|�����a��              PIC 9.
000180             07  ��Q�|�����N                PIC 9(2).
000181             07  ��Q�|������                PIC 9(2).
001261         05  ��Q�|���ރR�[�h                PIC 9(1).
001261         05  ��Q�|���R�[�h                  PIC X(2).
001400         05  ��Q�|�ی���                    PIC 9(2).
000183         05  ��Q�|�ی��Ҕԍ�                PIC X(10).
000188     03  ��Q�|���R�[�h�f�[�^.
001400         05  ��Q�|�ی����                  PIC 9(2).
000189         05  ��Q�|����                      PIC 9(4).
000190         05  ��Q�|��p�z                    PIC 9(9).
000191         05  ��Q�|���S�z                    PIC 9(9).
000192         05  ��Q�|�����z                    PIC 9(9).
000193         05  ��Q�|�{�l����                  PIC 9(3).
000194         05  ��Q�|�{�l��p�z                PIC 9(7).
000195         05  ��Q�|�{�l���S�z                PIC 9(7).
000196         05  ��Q�|�{�l�����z                PIC 9(7).
000193         05  ��Q�|�{�l������                PIC 9(4).
000197         05  ��Q�|�Ƒ�����                  PIC 9(3).
000198         05  ��Q�|�Ƒ���p�z                PIC 9(7).
000199         05  ��Q�|�Ƒ����S�z                PIC 9(7).
000200         05  ��Q�|�Ƒ������z                PIC 9(7).
000197         05  ��Q�|�Ƒ�������                PIC 9(4).
000201         05  FILLER                          PIC X(19).
000173*
000174*                           �m�q�k��  �R�Q�n
000175 FD  ��ƃt�@�C���R RECORD  CONTAINS 32 CHARACTERS.
000176 01  ��R�|���R�[�h.
000177     03  ��R�|���R�[�h�L�[.
001261         05  ��R�|���ރR�[�h                PIC 9(1).
001261         05  ��R�|���R�[�h                  PIC X(2).
001400         05  ��R�|�ی���                    PIC 9(2).
000183         05  ��R�|�ی��Ҕԍ�                PIC X(10).
000188     03  ��R�|���R�[�h�f�[�^.
001261         05  ��R�|�U������                  PIC 9(3).
000201         05  FILLER                          PIC X(13).
001350* 
001360 FD  ����t�@�C��.
001370     COPY YHN436P         OF  XMDLIB.
001371*
001380******************************************************************
001390*                WORKING-STORAGE SECTION                         *
001400******************************************************************
001410 WORKING-STORAGE         SECTION.
001420 01 �L�[����                           PIC X    VALUE SPACE.
001430 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
001440 01 �I���t���O                         PIC X(3) VALUE SPACE.
001450 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
001450 01 �I���t���O�R                       PIC X(3) VALUE SPACE.
001460 01 �m�F���͂v                         PIC X(1) VALUE SPACE.
001470 01 �t�@�C�����v                       PIC N(6) VALUE SPACE.
001480 01 �J�����g�����v                     PIC 9(1) VALUE ZERO.
001490 01 ���s�L�[�v                         PIC X(4) VALUE SPACE.
001304 01 �{�p�L�^�L�v                       PIC X(3)  VALUE SPACE.
001500 01 �n��v                             PIC X(2) VALUE SPACE.
001510 01 ����ԍ��v                         PIC X(8) VALUE SPACE.
001560 01 �O�a��v                           PIC 9    VALUE ZERO.
001570 01 �s���t���O                         PIC X(3) VALUE SPACE.
001580 01 �����Ώۃt���O                     PIC X(4) VALUE SPACE.
001590 01 �t�@�C����                         PIC N(4) VALUE SPACE.
001600 01 �����ړ��L�[                       PIC X(4) VALUE SPACE.
001610 01 �����m�F�敪�v                     PIC 9    VALUE ZERO.
001620 01 �����ʉ߃L�[�v                     PIC X(3) VALUE SPACE.
001630 01 �s�J�E���^                         PIC 9(2) VALUE ZERO.
001640 01 ���v�s�J�E���^                     PIC 9(3) VALUE ZERO.
001650 01 �y�[�W�J�E���^                     PIC 9(3) VALUE ZERO.
001651 01 ����t���O                         PIC X(3)  VALUE SPACE.
001652 01 �X�L�b�v�t���O                     PIC X(3)  VALUE SPACE.
001210 01 �I�[�v���t���O                     PIC X(3)   VALUE SPACE.
001660*
001661 01 ���v�p�v.
001662    03 �{�l�����J�E���g                PIC 9(9) VALUE ZERO.
001670    03 �Ƒ������J�E���g                PIC 9(9) VALUE ZERO.
001680    03 �v�����J�E���g                  PIC 9(9) VALUE ZERO.
001690    03 �{�l��p�z�J�E���g              PIC 9(9) VALUE ZERO.
001700    03 �Ƒ���p�z�J�E���g              PIC 9(9) VALUE ZERO.
001710    03 �v��p�z�J�E���g                PIC 9(9) VALUE ZERO.
001720    03 �{�l�����z�J�E���g              PIC 9(9) VALUE ZERO.
001730    03 �Ƒ������z�J�E���g              PIC 9(9) VALUE ZERO.
001740    03 �v�����z�J�E���g                PIC 9(9) VALUE ZERO.
001770 01 ���v                               PIC X(2) VALUE SPACE.
001771 01 ��������v                         PIC 9(2) VALUE ZERO.
001772*
001773 01 �ی��Җ��̂v.
001774    03 �ی��Җ��̂P�v                  PIC X(20) VALUE SPACE.
001775    03 �ی��Җ��̂Q�v                  PIC X(20) VALUE SPACE.
001776    03 �ی��Җ��̂R�v                  PIC X(20) VALUE SPACE.
001777*
001780 01 �����v�j                           PIC 9(9) VALUE ZERO.
001790 01 ��p�v�j                           PIC 9(9) VALUE ZERO.
001800 01 �����v�j                           PIC 9(9) VALUE ZERO.
001801 01 �S�p��                           PIC X(2)  VALUE X"8140".
001802 01 ���p��                           PIC X(2)  VALUE X"2020".
001810*
001820 01 �{�p�a��N���v.
001830     03 �{�p�a��v                     PIC 9    VALUE ZERO.
001840     03 �{�p�N���v.
001850        05 �{�p�N�v                    PIC 9(2) VALUE ZERO.
001860        05 �{�p���v                    PIC 9(2) VALUE ZERO.
001870        05 �{�p���v                    PIC 9(2) VALUE ZERO.
001880     03 �󗝔N���v.
001890        05 �󗝔N�v                    PIC 9(2) VALUE ZERO.
001900        05 �󗝌��v                    PIC 9(2) VALUE ZERO.
001910        05 �󗝓��v                    PIC 9(2) VALUE ZERO.
001920***
001921***
001922 01 ��ʏ��S�R�O�v.
001923    03 �����N���v.
001924       05 �����a��v                   PIC 9     VALUE ZERO.
001925       05 �����N�v                     PIC 9(2)  VALUE ZERO.
001926       05 �������v                     PIC 9(2)  VALUE ZERO.
001927    03 ��o�N�����v.
001928       05 ��o�a��v                   PIC 9     VALUE ZERO.
001929       05 ��o�N�v                     PIC 9(2)  VALUE ZERO.
001930       05 ��o���v                     PIC 9(2)  VALUE ZERO.
001931       05 ��o���v                     PIC 9(2)  VALUE ZERO.
001932    03 �����ނv                      PIC 9     VALUE ZERO.
001933*
001934**************
001935* �{�p����� *
001936**************
001937 01 �{�p�����v.
001938    03 ��\�҃J�i�v                    PIC X(50)  VALUE SPACE.
001938    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
001939    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
001940    03 �_���t�ԍ��v                    PIC X(20)  VALUE SPACE.
001941    03 �{�p���Z���v.
001942       05 �{�p���Z���P�v               PIC X(40)  VALUE SPACE.
001943       05 �{�p���Z���Q�v               PIC X(40)  VALUE SPACE.
001944    03 �{�p���X�֔ԍ��v.
001945       05 �{�p���X�֔ԍ��L���v         PIC X(2)   VALUE SPACE.
001945       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
001946       05 �{�p���X�֔ԍ���؂v         PIC X(1)   VALUE SPACE.
001947       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
001948    03 �{�p���d�b�ԍ��v                PIC X(15)  VALUE SPACE.
001949    03 �������v.
001950        05 ������s���v              PIC X(40)  VALUE SPACE.
001951        05 ������s�x�X���v          PIC X(40)  VALUE SPACE.
001952        05 �a����ʂv                  PIC 9(1)   VALUE ZERO.
001953        05 ��s�ԍ��v                  PIC X(4)   VALUE SPACE.
001954        05 �X�ԍ��v                    PIC X(3)   VALUE SPACE.
001955        05 �����ԍ��v                  PIC X(10)  VALUE SPACE.
001956        05 �������`�l�J�i�v            PIC X(50)  VALUE SPACE.
001957        05 �������`�l�v                PIC X(50)  VALUE SPACE.
001958*
001959 01 �A�Ԃv                             PIC 9(3)   VALUE ZERO.
001960 01 ��s���x�X���v                     PIC X(60)  VALUE SPACE.
001961 01 �a����ʃR�����g�v                 PIC X(4)   VALUE SPACE.
001962 01 �T���v                             PIC N(4)   VALUE SPACE.
001963*
001964 01 �ی���ʂv                         PIC 9(2)  VALUE ZERO.
001965 01 �ی��Ҕԍ��v                       PIC X(10) VALUE SPACE.
001966 01 �����於�̂v                       PIC X(40) VALUE SPACE.
001967 01 �x���������v                       PIC X(40) VALUE SPACE.
001968 01 �����v                             PIC X(24) VALUE SPACE.
001969 01 �ی��҈����v.
001970     03 �ی��҈����P�v                 PIC X(40) VALUE SPACE.
001971     03 �ی��҈����Q�v                 PIC X(40) VALUE SPACE.
      *
001220 01 ���ރR�[�h�v�q                     PIC 9(1) VALUE ZERO.
001220 01 ���R�[�h�v�q                       PIC X(2) VALUE SPACE.
001220 01 �ی����v�q                         PIC 9(2) VALUE ZERO.
001220 01 �{���v                             PIC X(2) VALUE SPACE.
001220 01 �����v.
          03 �����v�o                        PIC X(8) VALUE SPACE.
001972*
001973* �Еۗp
001974 01 �ڔ���敪�v                       PIC 9     VALUE ZERO.
001975*
001976 01 ����R�[�h�v                       PIC 9(2)  VALUE ZERO.
001977***
001978* ����������p�����^�p
001979*  �U�E�V���������i����敪 0:��� 1:������Ȃ��A�U���� 0:���� 1:�� 9:������Ȃ��j
001980*  ���Зp�������l1:������Ȃ�
001981***
001982 01 �������֘A�v.
001983        07 ���ۂU������敪�v          PIC 9 VALUE ZERO.
001984        07 ���ۂV������敪�v          PIC 9 VALUE ZERO.
001985        07 ���ۓ��Ј���敪�v          PIC 9 VALUE 1.
001986        07 ���ېU����敪�v            PIC 9 VALUE ZERO.
001987        07 �ЕۂU������敪�v          PIC 9 VALUE ZERO.
001988        07 �ЕۂV������敪�v          PIC 9 VALUE ZERO.
001989        07 �Еۓ��Ј���敪�v          PIC 9 VALUE 1.
001990        07 �ЕېU����敪�v            PIC 9 VALUE ZERO.
001991        07 �g���U������敪�v          PIC 9 VALUE ZERO.
001992        07 �g���V������敪�v          PIC 9 VALUE ZERO.
001993        07 �g�����Ј���敪�v          PIC 9 VALUE 1.
001994        07 �g���U����敪�v            PIC 9 VALUE ZERO.
001995        07 ���ςU������敪�v          PIC 9 VALUE ZERO.
001996        07 ���ςV������敪�v          PIC 9 VALUE ZERO.
001997        07 ���ϓ��Ј���敪�v          PIC 9 VALUE 1.
001998        07 ���ϐU����敪�v            PIC 9 VALUE ZERO.
001999        07 �V�l�U������敪�v          PIC 9 VALUE ZERO.
002000        07 �V�l�V������敪�v          PIC 9 VALUE ZERO.
002001        07 �V�l���Ј���敪�v          PIC 9 VALUE 1.
002002        07 �V�l�U����敪�v            PIC 9 VALUE ZERO.
002003        07 �����U������敪�v          PIC 9 VALUE ZERO.
002004        07 �����V������敪�v          PIC 9 VALUE ZERO.
002005        07 �������Ј���敪�v          PIC 9 VALUE 1.
002006        07 �����U����敪�v            PIC 9 VALUE ZERO.
002862        07 �U�V���я��敪�v            PIC 9 VALUE ZERO.
002877*
002862        07 �k�C���敪�v                PIC 9 VALUE ZERO.
002862        07 �X�敪�v                  PIC 9 VALUE ZERO.
002862        07 ���敪�v                  PIC 9 VALUE ZERO.
002862        07 �{��敪�v                  PIC 9 VALUE ZERO.
002862        07 �H�c�敪�v                  PIC 9 VALUE ZERO.
002862        07 �R�`�敪�v                  PIC 9 VALUE ZERO.
002862        07 �����敪�v                  PIC 9 VALUE ZERO.
002862        07 ���敪�v                  PIC 9 VALUE ZERO.
002862        07 �Ȗ؋敪�v                  PIC 9 VALUE ZERO.
002862        07 �Q�n�敪�v                  PIC 9 VALUE ZERO.
002862        07 ��ʋ敪�v                  PIC 9 VALUE ZERO.
002862        07 ��t�敪�v                  PIC 9 VALUE ZERO.
002862        07 �����敪�v                  PIC 9 VALUE ZERO.
002862        07 �_�ސ�敪�v                PIC 9 VALUE ZERO.
002862        07 �V���敪�v                  PIC 9 VALUE ZERO.
002862        07 �x�R�敪�v                  PIC 9 VALUE ZERO.
002862        07 �ΐ�敪�v                  PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 �R���敪�v                  PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 �򕌋敪�v                  PIC 9 VALUE ZERO.
002862        07 �É��敪�v                  PIC 9 VALUE ZERO.
002862        07 ���m�敪�v                  PIC 9 VALUE ZERO.
002862        07 �O�d�敪�v                  PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 ���s�敪�v                  PIC 9 VALUE ZERO.
002862        07 ���敪�v                  PIC 9 VALUE ZERO.
002862        07 ���ɋ敪�v                  PIC 9 VALUE ZERO.
002862        07 �ޗǋ敪�v                  PIC 9 VALUE ZERO.
002862        07 �a�̎R�敪�v                PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 �����敪�v                  PIC 9 VALUE ZERO.
002862        07 ���R�敪�v                  PIC 9 VALUE ZERO.
002862        07 �L���敪�v                  PIC 9 VALUE ZERO.
002862        07 �R���敪�v                  PIC 9 VALUE ZERO.
002862        07 �����敪�v                  PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 ���Q�敪�v                  PIC 9 VALUE ZERO.
002862        07 ���m�敪�v                  PIC 9 VALUE ZERO.
002862        07 �����敪�v                  PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 �F�{�敪�v                  PIC 9 VALUE ZERO.
002862        07 �啪�敪�v                  PIC 9 VALUE ZERO.
002862        07 �{��敪�v                  PIC 9 VALUE ZERO.
002862        07 �������敪�v                PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002007*
002008*********************************************************************
002009 01 �������.
002010     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
002011     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
002012     03 ������ʂo                     PIC X(2) VALUE SPACE.
002013     03 �g������o.
002014         05 �[������o.
002015             07 �ړ������o             PIC X(1) VALUE SPACE.
002016             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
002020         05 �ڍא���o                 PIC X(2) VALUE SPACE.
002030     03 �ʒm���o                     PIC X(2) VALUE SPACE.
002040     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
002050*********************************************************************
002060*
002070 01 �v�Z�@����N�v                     PIC 9(2).
002080* ���t�v�n�q�j
002090 01 �a��I���N�v                       PIC 9(4).
002100 01 �v�Z�@�a��N�v                     PIC 9(2).
002110 01 �v�Z�@����.
002120    03 �v�Z�@����N                    PIC 9(4).
002130    03 �v�Z�@�����                  PIC 9(4).
002140 01 �v�Z�@����q REDEFINES �v�Z�@����.
002150    03 �v�Z�@���I                      PIC 9(2).
002160    03 �v�Z�@���t                      PIC 9(6).
002170    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
002180       05 �v�Z�@�N��                   PIC 9(4).
002190       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
002200         07 �v�Z�@�N                   PIC 9(2).
002210         07 �v�Z�@��                   PIC 9(2).
002220       05 �v�Z�@��                     PIC 9(2).
002230*
       01 �x���t���O                         PIC X(3) VALUE SPACE.
       01 �x���b�m�s                         PIC 9(5) VALUE ZERO.
       01 �x���J�E���^                       PIC 9(4) VALUE ZERO.
       01 �x���񐔂v                         PIC 9(4) VALUE ZERO.
002240******************************************************************
002250*                          �A������                              *
002260******************************************************************
002270*
002280********************
002290* ���b�Z�[�W�\���L�[ *
002300********************
002310 01 �A���|�L�[ IS EXTERNAL.
002320    03  �A���|���b�Z�[�W                 PIC N(20).
002330*
001591 01 �A���|��ʏ��x�g�m�S�R�O   IS EXTERNAL.
002333    03 �A���|�����N��.
002334       05 �A���|�����a��               PIC 9.
002335       05 �A���|�����N                 PIC 9(2).
002336       05 �A���|������                 PIC 9(2).
002337    03 �A���|��o�N����.
002338       05 �A���|��o�a��               PIC 9.
002339       05 �A���|��o�N                 PIC 9(2).
002340       05 �A���|��o��                 PIC 9(2).
002341       05 �A���|��o��                 PIC 9(2).
002342    03 �A���|���Z�v�g���              PIC X(4).
002343    03 �A���|�ی����                  PIC 9(2).
002344    03 �A���|������                  PIC 9.
002345    03 �A���|�{�l�Ƒ�                  PIC 9.
002346    03 �A���|�p�����                  PIC 9.
002347    03 �A���|�������O                  PIC 9.
002348    03 �A���|���i�h�r                  PIC X(2).
002349    03 �A���|���ǂi�h�r                PIC X(2).
002351*
002352 01 �A���|��ʏ��x�g�m�S�R�O�ǉ�   IS EXTERNAL.
002353    03 �A���|�ꊇ�敪    PIC 9.
001933    03 �A���|�쐬�����  PIC 9.
          03 �A���|�v���r���[�敪            PIC 9.
          03 �A���|����                      PIC 9(5).
          03 �A���|�������[�h                PIC X(2).
      *
       01 �A��|������x�g�m�S�R�O   IS EXTERNAL.
          03 �A��|���ރR�[�h      PIC 9(1).
          03 �A��|���R�[�h        PIC X(2).
          03 �A��|�ی���          PIC 9(2).
          03 �A��|�ی��Ҕԍ�      PIC X(10).
002354*
000540************************************
000550* �v�����^�t�@�C���쐬�p           *
000560************************************
000570 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000580     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
000590     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
000600     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
000610     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
001772*
002355******************************************************************
002356*                      PROCEDURE  DIVISION                       *
002360******************************************************************
002370 PROCEDURE               DIVISION.
002380************
002390*           *
002400* ��������   *
002410*           *
002420************
002570     PERFORM �v�����^�t�@�C���쐬.
002430     PERFORM ������.
002431     PERFORM ������擾�Q.
002440************
002450*           *
002460* �又��     *
002470*           *
002480************
           IF �A��|���ރR�[�h = ZERO
002484        PERFORM �������
           ELSE
002484        PERFORM ��������P
           END-IF.
002500************
002510*           *
002520* �I������   *
002530*           *
002540************
002550     PERFORM �I������.
002560     EXIT PROGRAM.
002570*
002580*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002860*================================================================*
002870 �v�����^�t�@�C���쐬 SECTION.
002880*================================================================*
002890*   / ������ /
002900     MOVE SPACE TO �g�A�o�q�s�e�|�쐬�f�[�^.
002910     INITIALIZE �g�A�o�q�s�e�|�쐬�f�[�^.
002920*
002930*
002940*--���� �ύX�ӏ� ����--------------------------------------*
002970*   �g�p����v�����^�t�@�C�����Z�b�g
002231     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
002972*
002973*   �g�p���钠�[�v���O�������Z�b�g
002974     MOVE "YHN436"              TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
002590*================================================================*
002600 ������ SECTION.
002610*
002620     OPEN INPUT �����}�X�^.
002630             MOVE NC"����" TO �t�@�C�����v.
002640             PERFORM �I�[�v���`�F�b�N.
002650     OPEN INPUT ������}�X�^.
002660             MOVE NC"����" TO �t�@�C�����v.
002670             PERFORM �I�[�v���`�F�b�N.
002680     OPEN INPUT ���̃}�X�^.
002690             MOVE NC"����" TO �t�@�C�����v.
002700             PERFORM �I�[�v���`�F�b�N.
002710     OPEN INPUT �{�p�����}�X�^
002720             MOVE NC"�{��" TO �t�@�C�����v.
002730             PERFORM �I�[�v���`�F�b�N.
002740     OPEN INPUT �ی��҃}�X�^
002750             MOVE NC"�ی���" TO �t�@�C����.
002760             PERFORM �I�[�v���`�F�b�N.
002770     OPEN INPUT �s�����}�X�^
002780             MOVE NC"�s����" TO �t�@�C�����v.
002790             PERFORM �I�[�v���`�F�b�N.
002791     OPEN INPUT ������}�X�^
002792             MOVE NC"������" TO �t�@�C�����v.
002793             PERFORM �I�[�v���`�F�b�N.
002794     OPEN INPUT   ����}�X�^
002795             MOVE NC"����" TO �t�@�C�����v.
002796             PERFORM �I�[�v���`�F�b�N.
002800     OPEN INPUT ��ƃt�@�C���Q.
002810             MOVE NC"��Q" TO �t�@�C�����v.
002820             PERFORM �I�[�v���`�F�b�N.
002800     OPEN INPUT ��ƃt�@�C���R.
002810             MOVE NC"��R" TO �t�@�C�����v.
002820             PERFORM �I�[�v���`�F�b�N.
003250     OPEN INPUT ���Z�v�g�e.
003260         MOVE NC"���Z�v�g�e" TO �t�@�C����.
003270         PERFORM �I�[�v���`�F�b�N.
002590     OPEN INPUT ��f�ҏ��e.
002600         MOVE NC"��f�ҏ��e" TO �t�@�C�����v.
002610         PERFORM �I�[�v���`�F�b�N.
002611     OPEN INPUT �����f�[�^�e.
002612         MOVE NC"�����f�[�^�e" TO �t�@�C����.
002613         PERFORM �I�[�v���`�F�b�N.
002614     OPEN INPUT �{�p�L�^�e.
002615         MOVE NC"�{�p�L�^�e" TO �t�@�C����.
002616         PERFORM �I�[�v���`�F�b�N.
002830*
002840*
002850*    /* ���ݓ��t�擾 */
002860     ACCEPT �v�Z�@���t FROM DATE.
002870*    /* 1980�`2079�N�̊ԂŐݒ� */
002880     IF �v�Z�@�N > 80
002890         MOVE 19 TO �v�Z�@���I
002900     ELSE
002910         MOVE 20 TO �v�Z�@���I
002920     END-IF.
002930*
002940     PERFORM �J�����g�����擾.
002950     PERFORM �a��I���N�擾.
002960     COMPUTE �v�Z�@�a��N�v = �v�Z�@����N - �a��I���N�v.
002970     MOVE �v�Z�@�a��N�v TO �{�p�N�v.
002980     MOVE �v�Z�@��       TO �{�p���v.
002990     MOVE �v�Z�@��       TO �{�p���v.
003000*
003001     PERFORM �A�����ڑޔ�.
003002     PERFORM �{�p�����擾.
003003*
003010*================================================================*
003020 �I�[�v���`�F�b�N SECTION.
003030*
003040     IF ��ԃL�[  NOT =  "00"
003050         DISPLAY �t�@�C�����v NC"�e�I�[�v���G���[" UPON CONS
003060         DISPLAY NC"��ԃL�[�F" ��ԃL�[           UPON CONS
003070         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003080                                                   UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
003090         ACCEPT  �L�[���� FROM CONS
003100         PERFORM �t�@�C����
003110         EXIT PROGRAM.
003120*================================================================*
003130 �J�����g�����擾 SECTION.
003140*
003150     MOVE ZEROS TO ���|����敪.
003160     READ ������}�X�^
003170     NOT INVALID KEY
003180         MOVE ���|�J�����g���� TO �J�����g�����v
003181         MOVE ���|����R�[�h   TO ����R�[�h�v
               MOVE ���|�x����     TO �x���񐔂v
003190     END-READ.
003200*
003210*================================================================*
003220 �a��I���N�擾 SECTION.
003230*
003240*     DISPLAY NC"�J�����g�����v"  �J�����g�����v UPON MSGBOX.
003250     MOVE �J�����g�����v TO ���|�����敪.
003260     READ �����}�X�^
003270     INVALID KEY
003280         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
003290         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003300                                                  UPON CONS
003310         ACCEPT  �L�[���� FROM CONS
003320         PERFORM �I������
003330         EXIT PROGRAM
003340     NOT INVALID KEY
003350         COMPUTE �O�a��v = �J�����g�����v - 1
003360         MOVE �O�a��v TO ���|�����敪
003370         READ �����}�X�^
003380         INVALID KEY
003390             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
003400             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003410                                                      UPON CONS
003420             ACCEPT  �L�[���� FROM CONS
003430             PERFORM �I������
003440             EXIT PROGRAM
003450         NOT INVALID KEY
003460             MOVE ���|�I������N TO �a��I���N�v
003470         END-READ
003480     END-READ.
003490*
003491*================================================================*
003492 �A�����ڑޔ� SECTION.
003493*
003494     MOVE �A���|�����a��  TO �����a��v.
003495     MOVE �A���|�����N    TO �����N�v.
003496     MOVE �A���|������    TO �������v.
003497     MOVE �A���|��o�a��  TO ��o�a��v.
003498     MOVE �A���|��o�N    TO ��o�N�v.
003499     MOVE �A���|��o��    TO ��o���v.
003500     MOVE �A���|��o��    TO ��o���v.
003501     MOVE �A���|������  TO �����ނv.
003502*
003503*================================================================*
003504 �{�p�����擾 SECTION.
003505*
003506     MOVE ZERO  TO �{��|�{�p���ԍ�.
003507     READ �{�p�����}�X�^
003508     INVALID KEY
003509         CONTINUE
003510     NOT INVALID KEY
003511*
003513         MOVE "��"                   TO �{�p���X�֔ԍ��L���v
003512         MOVE �{��|�X�֔ԍ��P       TO �{�p���X�֔ԍ��P�v
003513         MOVE "-"                    TO �{�p���X�֔ԍ���؂v
003514         MOVE �{��|�X�֔ԍ��Q       TO �{�p���X�֔ԍ��Q�v
003515         MOVE �{��|��\�҃J�i       TO ��\�҃J�i�v
003515         MOVE �{��|��\�Җ�         TO ��\�Җ��v
003516         MOVE �{��|�ڍ��@��         TO �ڍ��@���v
003516*         MOVE �{��|�Z���P           TO �{�p���Z���P�v
003516*         MOVE �{��|�Z���Q           TO �{�p���Z���Q�v
003517         STRING �{��|�Z���P  DELIMITED BY SPACE
003518                �{��|�Z���Q  DELIMITED BY SPACE
003519           INTO �{�p���Z���v
003520         END-STRING
003521         MOVE �{��|�d�b�ԍ�         TO �{�p���d�b�ԍ��v
003522         MOVE �{��|�V�_���t�ԍ�     TO �_���t�ԍ��v
003523*
003524         MOVE �{��|������s��     TO ������s���v
003525         MOVE �{��|������s�x�X�� TO ������s�x�X���v
003526         MOVE �{��|�a�����         TO �a����ʂv
003527         MOVE �{��|��s�ԍ�         TO ��s�ԍ��v
003528         MOVE �{��|�X�ԍ�           TO �X�ԍ��v
003529         MOVE �{��|�����ԍ�         TO �����ԍ��v
003530         MOVE �{��|�������`�l�J�i   TO �������`�l�J�i�v
003531         MOVE �{��|�������`�l       TO �������`�l�v
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
               END-READ
003537         EVALUATE �a����ʂv
003538         WHEN 1
003539             MOVE "����"   TO �a����ʃR�����g�v
003540         WHEN 2
003541             MOVE "����"   TO �a����ʃR�����g�v
003542         WHEN OTHER
003543             MOVE SPACE    TO �a����ʃR�����g�v
003544         END-EVALUATE
003532         STRING ������s���v     DELIMITED BY SPACE
003533                " "                DELIMITED BY SIZE
003534                ������s�x�X���v DELIMITED BY SPACE
003533                " "                DELIMITED BY SIZE
003534                �a����ʃR�����g�v DELIMITED BY SPACE
003533                " "                DELIMITED BY SIZE
003534                �����ԍ��v         DELIMITED BY SPACE
003535                INTO ��s���x�X���v
003536         END-STRING
003545*
003546     END-READ.
003547*================================================================*
003548 �t�@�C���� SECTION.
004610*
002990     IF ( �I�[�v���t���O = "YES" )
002991         CLOSE ����t�@�C��
003041     END-IF.
003549*
003550     CLOSE �����}�X�^   ������}�X�^   ��ƃt�@�C���Q
003551           ���̃}�X�^   �{�p�����}�X�^ �s�����}�X�^ 
003552           �ی��҃}�X�^ ������}�X�^     ����}�X�^
                 ��f�ҏ��e �����f�[�^�e     �{�p�L�^�e  ���Z�v�g�e
                 ��ƃt�@�C���R.
003553*================================================================*
003560 �I������ SECTION.
003570*
003580     PERFORM �t�@�C����.
003590*================================================================*
003600*================================================================*
003610 �G���[�\�� SECTION.
003620*
003630     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
003640     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
003650     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
003660     ACCEPT  �L�[���� FROM CONS.
003670*================================================================*
003680 �G���[�\���q SECTION.
003690*
003700     DISPLAY NC"�t�@�C���Ǎ��G���[" �t�@�C����     UPON CONS.
003710     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
003720     ACCEPT  �L�[���� FROM CONS.
003730*================================================================*
003740 ��ƃt�@�C���Q�Ǎ� SECTION.
003750*
003760     READ ��ƃt�@�C���Q NEXT
003770     AT END
003780         MOVE "YES" TO �I���t���O
003790     END-READ.
003800*================================================================*
003810 ������� SECTION.
003820*
           MOVE 1         TO �y�[�W�J�E���^.
      * / ���я��ύX
003853     MOVE ZERO      TO  ��Q�|�����a��.
003854     MOVE ZERO      TO  ��Q�|�����N.
003855     MOVE ZERO      TO  ��Q�|������.
003857     MOVE ZERO      TO  ��Q�|���ރR�[�h.
003856     MOVE LOW-VALUE TO  ��Q�|���R�[�h.
003858     MOVE ZERO      TO  ��Q�|�ی���.
003859     MOVE LOW-VALUE TO  ��Q�|�ی��Ҕԍ�.
003860     START ��ƃt�@�C���Q   KEY IS >=  ��Q�|�����a��N��
003861                                       ��Q�|���ރR�[�h
003862                                       ��Q�|���R�[�h
003863                                       ��Q�|�ی���
003864                                       ��Q�|�ی��Ҕԍ�
003865     END-START.
003866     IF ��ԃL�[ = "00"
003867         MOVE SPACE TO �I���t���O
003868         PERFORM ��ƃt�@�C���Q�Ǎ�
003869         IF  �I���t���O = "YES"
003870             MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
003871             CALL   "MSG001"
003872             CANCEL "MSG001"
003873             PERFORM �t�@�C����
003874             MOVE 99 TO PROGRAM-STATUS
003875             EXIT PROGRAM
003876         END-IF
003877*
003878         PERFORM UNTIL �I���t���O = "YES"
003879           MOVE SPACE TO �X�L�b�v�t���O
003880           PERFORM ����Ώۃ`�F�b�N
003881           IF ����t���O = "YES"
003882*
003883             MOVE SPACE TO YHN436P
003884             INITIALIZE    YHN436P
003885             MOVE ��Q�|���ރR�[�h  TO ���ރR�[�h�v�q
003886             MOVE ��Q�|���R�[�h    TO ���R�[�h�v�q
003886             MOVE ��Q�|�ی���      TO �ی����v�q
003887             PERFORM  �\�Z�b�g�P
003888* 
003889             PERFORM VARYING �s�J�E���^ FROM 1 BY 1
003890                     UNTIL ( �s�J�E���^ > 8       ) OR
003891                           ( ���ރR�[�h�v�q   NOT = ��Q�|���ރR�[�h ) OR
003892                           ( ���R�[�h�v�q     NOT = ��Q�|���R�[�h ) OR
                                 ((���ރR�[�h�v�q = 5) AND (��Q�|�ی��� NOT = �ی����v�q)) OR
003893                           ( �I���t���O = "YES" )
003894                  PERFORM ����Ώۃ`�F�b�N
003895                  IF ����t���O = "YES"
003897                     PERFORM �\�Z�b�g�Q
                           PERFORM �t�b�^�Z�b�g
      */�������Ō�A���R�[�h������A����Ώۂ��Ȃ��ꍇ�A���v���������Ȃ�/0510
003898*                  ELSE
003899*                     COMPUTE �s�J�E���^ =  �s�J�E���^ - 1
003900                  END-IF
003901                  PERFORM ��ƃt�@�C���Q�Ǎ�
                        PERFORM ����Ώۃ`�F�b�N
                        IF ����t���O = SPACE
                           COMPUTE �s�J�E���^ =  �s�J�E���^ - 1
                        END-IF
      */�������Ō�A���R�[�h������A����Ώۂ��Ȃ��ꍇ�A���v���������Ȃ�/0510
003902                  MOVE "YES" TO �X�L�b�v�t���O
003903             END-PERFORM
003904*
003905             IF ( �I���t���O =  "YES" ) OR 
003906                ( ���ރR�[�h�v�q NOT = ��Q�|���ރR�[�h ) OR
003907                ( ���R�[�h�v�q   NOT = ��Q�|���R�[�h ) OR
                      ((��Q�|���ރR�[�h = 5) AND (��Q�|�ی��� NOT = �ی����v�q))
003908                 MOVE �{�l�����J�E���g   TO     �{�l�������v
003909                 MOVE �Ƒ������J�E���g   TO     �Ƒ��������v
003910                 MOVE �v�����J�E���g     TO     �v�������v
003911                 MOVE �{�l��p�z�J�E���g TO     �{�l��p�z���v
003912                 MOVE �Ƒ���p�z�J�E���g TO     �Ƒ���p�z���v
003913                 MOVE �v��p�z�J�E���g   TO     �v��p�z���v
003914                 MOVE �{�l�����z�J�E���g TO     �{�l�����z���v
003915                 MOVE �Ƒ������z�J�E���g TO     �Ƒ������z���v
003916                 MOVE �v�����z�J�E���g   TO     �v�����z���v
003917                 IF �{�l�����z�J�E���g NOT = ZERO
003918                    MOVE "(" TO �{�l�����ʍ��v
003919                    MOVE ")" TO �{�l�E���ʍ��v
003920                 ELSE
003921                    MOVE SPACE TO �{�l�����ʍ��v
003922                    MOVE SPACE TO �{�l�E���ʍ��v
003923                 END-IF
003924                 IF �Ƒ������z�J�E���g NOT = ZERO
003925                    MOVE "(" TO �Ƒ������ʍ��v
003926                    MOVE ")" TO �Ƒ��E���ʍ��v
003927                 ELSE
003928                    MOVE SPACE TO �Ƒ������ʍ��v
003929                    MOVE SPACE TO �Ƒ��E���ʍ��v
003930                 END-IF
003931                 IF �v�����z�J�E���g NOT = ZERO
003932                    MOVE "(" TO �v�����ʍ��v
003933                    MOVE ")" TO �v�E���ʍ��v
003934                 ELSE
003935                    MOVE SPACE TO �v�����ʍ��v
003936                    MOVE SPACE TO �v�E���ʍ��v
003937                 END-IF
003938*
003939                 IF �y�[�W�J�E���^ > 1
                           MOVE �y�[�W�J�E���^ TO ��
                       END-IF
                       MOVE 1                  TO �y�[�W�J�E���^
                       INITIALIZE ���v�p�v
003940             ELSE
003941                 MOVE ZERO     TO     �{�l�������v
003942                 MOVE ZERO     TO     �Ƒ��������v
003943                 MOVE ZERO     TO     �v�������v
003944                 MOVE ZERO     TO     �{�l��p�z���v
003945                 MOVE ZERO     TO     �Ƒ���p�z���v
003946                 MOVE ZERO     TO     �v��p�z���v
003947                 MOVE ZERO     TO     �{�l�����z���v
003948                 MOVE ZERO     TO     �Ƒ������z���v
003949                 MOVE ZERO     TO     �v�����z���v
                       MOVE �y�[�W�J�E���^ TO ��
                       COMPUTE �y�[�W�J�E���^ = �y�[�W�J�E���^ + 1
003950             END-IF
003953*
003954             PERFORM �󎚏���
003955             PERFORM ���ŏ���
003956*
003960           END-IF
003961*
003963           IF �X�L�b�v�t���O NOT = "YES"
003964              PERFORM ��ƃt�@�C���Q�Ǎ�
003965           END-IF
003966         END-PERFORM
003967*
003968     ELSE
003969         MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
003970         CALL   "MSG001"
003971         CANCEL "MSG001"
003972         PERFORM �t�@�C����
003973         MOVE 99 TO PROGRAM-STATUS
003974         EXIT PROGRAM
003975     END-IF.
004621*
003800*================================================================*
003810 ��������P SECTION.
003820*
           MOVE 1         TO �y�[�W�J�E���^.
      * / ���я��ύX
003853     MOVE �A���|�����a��    TO  ��Q�|�����a��.
003854     MOVE �A���|�����N      TO  ��Q�|�����N.
003855     MOVE �A���|������      TO  ��Q�|������.
003857     MOVE �A��|���ރR�[�h  TO  ��Q�|���ރR�[�h.
003856     MOVE �A��|���R�[�h    TO  ��Q�|���R�[�h.
003858     MOVE �A��|�ی���      TO  ��Q�|�ی���.
003859     MOVE SPACE             TO  ��Q�|�ی��Ҕԍ�.
003860     START ��ƃt�@�C���Q   KEY IS >=  ��Q�|�����a��N��
003861                                       ��Q�|���ރR�[�h
003862                                       ��Q�|���R�[�h
003863                                       ��Q�|�ی���
003864                                       ��Q�|�ی��Ҕԍ�
003865     END-START.
003866     IF ��ԃL�[ = "00"
003867         MOVE SPACE TO �I���t���O
003868         PERFORM ��ƃt�@�C���Q�Ǎ�
003869         IF  �I���t���O = "YES"
003870             MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
003871             CALL   "MSG001"
003872             CANCEL "MSG001"
003873             PERFORM �t�@�C����
003874             MOVE 99 TO PROGRAM-STATUS
003875             EXIT PROGRAM
003876         END-IF
003877*
003878         PERFORM UNTIL (�I���t���O = "YES") OR
                             (��Q�|���ރR�[�h NOT = �A��|���ރR�[�h) OR
                             (��Q�|�ی���     NOT = �A��|�ی���) OR
                             (��Q�|���R�[�h   NOT = �A��|���R�[�h)
003879           MOVE SPACE TO �X�L�b�v�t���O
003880           PERFORM ����Ώۃ`�F�b�N
003881           IF ����t���O = "YES"
003882*
003883             MOVE SPACE TO YHN436P
003884             INITIALIZE    YHN436P
003887             PERFORM  �\�Z�b�g�P
003888* 
003889             PERFORM VARYING �s�J�E���^ FROM 1 BY 1
003890                     UNTIL ( �s�J�E���^ > 8       ) OR
003891                           ( �A��|���ރR�[�h   NOT = ��Q�|���ރR�[�h ) OR
003892                           ( �A��|���R�[�h     NOT = ��Q�|���R�[�h ) OR
003892                           ( �A��|�ی���       NOT = ��Q�|�ی��� ) OR
003893                           ( �I���t���O = "YES" )
003894                  PERFORM ����Ώۃ`�F�b�N
003895                  IF ����t���O = "YES"
003897                     PERFORM �\�Z�b�g�Q
                           PERFORM �t�b�^�Z�b�g
      */�������Ō�A���R�[�h������A����Ώۂ��Ȃ��ꍇ�A���v���������Ȃ�/0510
003898*                  ELSE
003899*                     COMPUTE �s�J�E���^ =  �s�J�E���^ - 1
003900                  END-IF
003901                  PERFORM ��ƃt�@�C���Q�Ǎ�
                        PERFORM ����Ώۃ`�F�b�N
                        IF ����t���O = SPACE
                           COMPUTE �s�J�E���^ =  �s�J�E���^ - 1
                        END-IF
      */�������Ō�A���R�[�h������A����Ώۂ��Ȃ��ꍇ�A���v���������Ȃ�/0510
003902                  MOVE "YES" TO �X�L�b�v�t���O
003903             END-PERFORM
003904*
003905             IF ( �I���t���O =  "YES" ) OR 
003906                ( �A��|���ރR�[�h NOT = ��Q�|���ރR�[�h ) OR
003892                ( �A��|�ی���     NOT = ��Q�|�ی��� ) OR
003907                ( �A��|���R�[�h   NOT = ��Q�|���R�[�h )
003908                 MOVE �{�l�����J�E���g   TO     �{�l�������v
003909                 MOVE �Ƒ������J�E���g   TO     �Ƒ��������v
003910                 MOVE �v�����J�E���g     TO     �v�������v
003911                 MOVE �{�l��p�z�J�E���g TO     �{�l��p�z���v
003912                 MOVE �Ƒ���p�z�J�E���g TO     �Ƒ���p�z���v
003913                 MOVE �v��p�z�J�E���g   TO     �v��p�z���v
003914                 MOVE �{�l�����z�J�E���g TO     �{�l�����z���v
003915                 MOVE �Ƒ������z�J�E���g TO     �Ƒ������z���v
003916                 MOVE �v�����z�J�E���g   TO     �v�����z���v
003917                 IF �{�l�����z�J�E���g NOT = ZERO
003918                    MOVE "(" TO �{�l�����ʍ��v
003919                    MOVE ")" TO �{�l�E���ʍ��v
003920                 ELSE
003921                    MOVE SPACE TO �{�l�����ʍ��v
003922                    MOVE SPACE TO �{�l�E���ʍ��v
003923                 END-IF
003924                 IF �Ƒ������z�J�E���g NOT = ZERO
003925                    MOVE "(" TO �Ƒ������ʍ��v
003926                    MOVE ")" TO �Ƒ��E���ʍ��v
003927                 ELSE
003928                    MOVE SPACE TO �Ƒ������ʍ��v
003929                    MOVE SPACE TO �Ƒ��E���ʍ��v
003930                 END-IF
003931                 IF �v�����z�J�E���g NOT = ZERO
003932                    MOVE "(" TO �v�����ʍ��v
003933                    MOVE ")" TO �v�E���ʍ��v
003934                 ELSE
003935                    MOVE SPACE TO �v�����ʍ��v
003936                    MOVE SPACE TO �v�E���ʍ��v
003937                 END-IF
003938*
003939                 IF �y�[�W�J�E���^ > 1
                           MOVE �y�[�W�J�E���^ TO ��
                       END-IF
                       MOVE 1                  TO �y�[�W�J�E���^
003939                 INITIALIZE ���v�p�v
003940             ELSE
003941                 MOVE ZERO     TO     �{�l�������v
003942                 MOVE ZERO     TO     �Ƒ��������v
003943                 MOVE ZERO     TO     �v�������v
003944                 MOVE ZERO     TO     �{�l��p�z���v
003945                 MOVE ZERO     TO     �Ƒ���p�z���v
003946                 MOVE ZERO     TO     �v��p�z���v
003947                 MOVE ZERO     TO     �{�l�����z���v
003948                 MOVE ZERO     TO     �Ƒ������z���v
003949                 MOVE ZERO     TO     �v�����z���v
                       MOVE �y�[�W�J�E���^ TO ��
                       COMPUTE �y�[�W�J�E���^ = �y�[�W�J�E���^ + 1
003950             END-IF
003953*
003954             PERFORM �󎚏���
003955             PERFORM ���ŏ���
003956*
003960           END-IF
003961*
003963           IF �X�L�b�v�t���O NOT = "YES"
003964              PERFORM ��ƃt�@�C���Q�Ǎ�
003965           END-IF
003966         END-PERFORM
003967*
003968     ELSE
003969         MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
003970         CALL   "MSG001"
003971         CANCEL "MSG001"
003972         PERFORM �t�@�C����
003973         MOVE 99 TO PROGRAM-STATUS
003974         EXIT PROGRAM
003975     END-IF.
004621*
004630*================================================================*
004640 �\�Z�b�g�P SECTION.
004650*
004944* �����̘a����擾
004945     MOVE �����a��v         TO ���|�����敪.
004946     READ �����}�X�^
004947     INVALID KEY
004948         MOVE SPACE          TO �����a���
004949     NOT INVALID KEY
004950         MOVE ���|��������   TO �����a���
004951     END-READ.
004952*
004953     MOVE �����N�v           TO �����N.
004954     MOVE �������v           TO ������.
004955*
007100* �쐬�����擾
           IF �A���|�쐬����� = 1
007110        MOVE ��o�a��v         TO ���|�����敪
007120        READ �����}�X�^
007130        INVALID KEY
007140           MOVE SPACE          TO �쐬�a��
007150        NOT INVALID KEY
007160           MOVE ���|��������   TO �쐬�a��
007170        END-READ
007180        MOVE ��o�N�v          TO �쐬�N
007190        MOVE ��o���v          TO �쐬��
007200        MOVE ��o���v          TO �쐬��
      */�R�����g���o���ɕύX/101012
      *        MOVE NC"�쐬��"        TO �쐬�b�l
              MOVE NC"��o��"        TO �쐬�b�l
              MOVE NC"�N"            TO �쐬�N�b�l
              MOVE NC"��"            TO �쐬���b�l
              MOVE NC"��"            TO �쐬���b�l
           END-IF.
004967*
004968     MOVE �{�p���Z���v       TO �Z��.
004968*     MOVE �{�p���Z���P�v     TO �Z���P.
004968*     MOVE �{�p���Z���Q�v     TO �Z���Q.
004969*     MOVE �{�p���X�֔ԍ��v   TO �X�֔ԍ�.
004970*     MOVE ��\�҃J�i�v       TO ��\�҃J�i.
004970     MOVE ��\�Җ��v         TO ��\�Җ�.
004971     MOVE �ڍ��@���v         TO �ڍ��@��.
004972     MOVE �_���t�ԍ��v       TO �_���t�ԍ�.
004973     MOVE �{�p���d�b�ԍ��v   TO �d�b�ԍ�.
004974*     MOVE ��s���x�X���v     TO ��s���x�X��.
004975*     MOVE �a����ʃR�����g�v TO �a�����.
004976*     MOVE �����ԍ��v         TO �����ԍ�.
004977*     MOVE �������`�l�J�i�v   TO �������`�l�J�i.
004978*     MOVE �������`�l�v       TO �������`�l.
004979*
004980*================================================================*
004981 �\�Z�b�g�Q SECTION.
004982*
004983     MOVE ��Q�|�{�l����     TO �{�l����(�s�J�E���^).
004990     MOVE ��Q�|�{�l��p�z   TO �{�l��p�z(�s�J�E���^).
005000     MOVE ��Q�|�{�l�����z   TO �{�l�����z(�s�J�E���^).
005010     IF ��Q�|�{�l�����z NOT = ZERO
005020        MOVE "(" TO �{�l������(�s�J�E���^)
005030        MOVE ")" TO �{�l�E����(�s�J�E���^)
005040     ELSE
005050        MOVE SPACE TO �{�l������(�s�J�E���^)
005060        MOVE SPACE TO �{�l�E����(�s�J�E���^)
005070     END-IF.
005080*
005090     MOVE ��Q�|�Ƒ�����     TO �Ƒ�����(�s�J�E���^).
005100     MOVE ��Q�|�Ƒ���p�z   TO �Ƒ���p�z(�s�J�E���^).
005110     MOVE ��Q�|�Ƒ������z   TO �Ƒ������z(�s�J�E���^).
005120     IF ��Q�|�Ƒ������z NOT = ZERO
005130        MOVE "(" TO �Ƒ�������(�s�J�E���^)
005140        MOVE ")" TO �Ƒ��E����(�s�J�E���^)
005150     ELSE
005160        MOVE SPACE TO �Ƒ�������(�s�J�E���^)
005170        MOVE SPACE TO �Ƒ��E����(�s�J�E���^)
005180     END-IF.
005190*
005230     MOVE ��Q�|����    TO  �v����(�s�J�E���^).
005240     MOVE ��Q�|��p�z  TO  �v��p�z(�s�J�E���^).
005250     MOVE ��Q�|�����z  TO  �v�����z(�s�J�E���^).
005260     IF ��Q�|�����z NOT = ZERO
005270        MOVE "(" TO �v������(�s�J�E���^)
005280        MOVE ")" TO �v�E����(�s�J�E���^)
005290     ELSE
005300        MOVE SPACE TO �v������(�s�J�E���^)
005310        MOVE SPACE TO �v�E����(�s�J�E���^)
005320     END-IF.
005330*
005340     ADD ��Q�|�{�l����   TO  �{�l�����J�E���g.
005350     ADD ��Q�|�Ƒ�����   TO  �Ƒ������J�E���g.
005360     ADD ��Q�|����       TO  �v�����J�E���g.
005370     ADD ��Q�|�{�l��p�z TO  �{�l��p�z�J�E���g.
005380     ADD ��Q�|�Ƒ���p�z TO  �Ƒ���p�z�J�E���g.
005390     ADD ��Q�|��p�z     TO  �v��p�z�J�E���g.
005400     ADD ��Q�|�{�l�����z TO  �{�l�����z�J�E���g.
005410     ADD ��Q�|�Ƒ������z TO  �Ƒ������z�J�E���g.
005420     ADD ��Q�|�����z     TO  �v�����z�J�E���g.
005430**
005440* �ی���
005441     MOVE ��Q�|�ی����     TO �ی���ʂv.
005442     MOVE ��Q�|�ی��Ҕԍ�   TO �ی��Ҕԍ��v.
005443     EVALUATE �ی���ʂv
005444     WHEN 1 THRU 4
005445     WHEN 6 THRU 9
005446         PERFORM �ی��ҏ��擾
005447     WHEN 5
005448     WHEN 50 THRU 60
005449         PERFORM �s�������擾
005450     END-EVALUATE.
005451     PERFORM ���������擾.
005452*
005453     MOVE �ی��҈����v  TO �ی��Җ��̂v.
005970*
006000*
006010     COMPUTE ���v�s�J�E���^ = �s�J�E���^ * 3.
006020     MOVE �ی��Җ��̂P�v TO �ی��Җ�(���v�s�J�E���^ - 1).
006030     IF ��Q�|�ی���� = "50" OR "51" OR "52" OR "53" OR
006040                         "54" OR "55" OR "60" OR "05" OR "08"
006050        IF �ی��Җ��̂Q�v NOT = SPACE AND �ی��Җ��̂R�v = SPACE
006060           MOVE �ی��Җ��̂P�v TO �ی��Җ�(���v�s�J�E���^ - 2)
006070           MOVE �ی��Җ��̂Q�v TO �ی��Җ�(���v�s�J�E���^ - 1)
006080        END-IF
006090     ELSE
006100        IF �ی��Җ��̂Q�v NOT = SPACE AND �ی��Җ��̂R�v = SPACE
006110           MOVE �ی��Җ��̂P�v TO �ی��Җ�(���v�s�J�E���^ - 1)
006120           MOVE �ی��Җ��̂Q�v TO �ی��Җ�(���v�s�J�E���^)
006130        END-IF
006140     END-IF.
006150     IF ��Q�|�ی���� = "50" OR "51" OR "52" OR "53" OR
006160                         "54" OR "55" OR "60" OR "05" OR "08"
006170        IF �ی��Җ��̂Q�v NOT = SPACE AND �ی��Җ��̂R�v NOT = SPACE
006180           MOVE �ی��Җ��̂P�v TO �ی��Җ�(���v�s�J�E���^ - 2)
006190           MOVE �ی��Җ��̂Q�v TO �ی��Җ�(���v�s�J�E���^ - 1)
006200        END-IF
006210     ELSE
006220        IF �ی��Җ��̂Q�v NOT = SPACE AND �ی��Җ��̂R�v NOT = SPACE
006230           MOVE �ی��Җ��̂P�v TO �ی��Җ�(���v�s�J�E���^ - 2)
006240           MOVE �ی��Җ��̂Q�v TO �ی��Җ�(���v�s�J�E���^ - 1)
006250           MOVE �ی��Җ��̂R�v TO �ی��Җ�(���v�s�J�E���^)
006260        END-IF
006270     END-IF.
006280*     IF ��Q�|�ی���� = "50" OR "51" OR "52" OR "53" OR
006290*                         "54" OR "55" OR "60" 
006300*        MOVE "�i�����j" TO �ی��Җ�(���v�s�J�E���^)
006310*     END-IF.
006320*     IF ��Q�|�ی���� = "05"
006330*        MOVE "�i�V�l�j" TO �ی��Җ�(���v�s�J�E���^)
006340*     END-IF.
006350*     IF ��Q�|�ی���� = "08"
006360*        MOVE "�i�ސE�j" TO �ی��Җ�(���v�s�J�E���^)
006370*     END-IF.
006380*
006390*================================================================*
006400 �t�b�^�Z�b�g SECTION.
006410*
      */ �ی���ʁA�����A��ی��Җ�������ԂɈ�� /150219
           IF ��Q�|�ی���� = 01
               MOVE ��Q�|�ی��Ҕԍ�(1:2)  TO ���|���̃R�[�h
           ELSE
               MOVE ��Q�|�ی��Ҕԍ�(3:2)  TO ���|���̃R�[�h
           END-IF.
025960     MOVE 13                     TO ���|�敪�R�[�h.
025980     READ ���̃}�X�^
025990     INVALID KEY
026000         MOVE SPACE              TO �����v
026010     NOT INVALID KEY
026020         MOVE ���|����           TO �����v
026030     END-READ.
           STRING "["                  DELIMITED BY SIZE
                  �����v�o             DELIMITED BY "�@"
                  "]"                  DELIMITED BY SIZE
             INTO ���b�l
           END-STRING.
           EVALUATE ��Q�|�ی����
           WHEN 1
           WHEN 8
               MOVE "���ۘA����i���ہj"    TO �ی��Җ���
           WHEN 5
               MOVE "���ۘA����i����j"    TO �ی��Җ���
           WHEN 2
           WHEN 6
               MOVE "�S�����N�ی�����x��"    TO �ی��Җ���
           WHEN 7
               MOVE "�D���ی�"      TO �ی��Җ���
           WHEN 3
               MOVE "���ۑg��"      TO �ی��Җ���
           WHEN 4
           WHEN 9
               MOVE "���ϑg��"      TO �ی��Җ���
           WHEN 51
           WHEN 52
           WHEN 53
           WHEN 54
           WHEN 55
           WHEN 60
               MOVE "���ۘA����i�����j"    TO �ی��Җ���
           END-EVALUATE.
      *
           MOVE ��Q�|���ރR�[�h    TO ��R�|���ރR�[�h.
           MOVE ��Q�|���R�[�h      TO ��R�|���R�[�h.
           MOVE ��Q�|�ی���        TO ��R�|�ی���.
           IF ��Q�|���ރR�[�h = 3 OR 4
              MOVE ��Q�|�ی��Ҕԍ� TO ��R�|�ی��Ҕԍ�
           ELSE
              MOVE SPACE            TO ��R�|�ی��Ҕԍ�
           END-IF.
           READ ��ƃt�@�C���R
           NOT INVALID KEY
              MOVE ��R�|�U������   TO �敪�P
              MOVE "*"              TO �敪�Q �敪�R
              MOVE "-"              TO ��؂P ��؂Q
           END-READ.
      *
006390*================================================================*
006400 �G���[�����o SECTION.
006410*
006420     IF �ʒm���o NOT = "00"
006430         DISPLAY NC"���[�G���["              UPON CONS
006440         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
006450         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
006460         DISPLAY NC"�g������o�F" �g������o UPON CONS
006470         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
006480                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
006490         ACCEPT  �L�[���� FROM CONS
006500         PERFORM �t�@�C����
006510         MOVE 99  TO PROGRAM-STATUS
006520         EXIT PROGRAM
006530     END-IF.
006540*================================================================*
006550 �󎚏���  SECTION.
006560*
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
013440*
006570     MOVE "YHN436P" TO  ��`�̖��o.
006580     MOVE SPACE     TO  ������ʂo.
006590     MOVE "SCREEN"  TO  ���ڌQ���o.
006610     WRITE YHN436P.
006620     PERFORM �G���[�����o.
006650*================================================================*
006660 ���ŏ���  SECTION.
006670
006680     MOVE "YHN436P" TO  ��`�̖��o.
006690     MOVE "CT"      TO  ������ʂo.
006700     MOVE "PAGE"    TO  �g������o.
006710     MOVE SPACE     TO  ���ڌQ���o.
006730     WRITE YHN436P.
006740     PERFORM �G���[�����o.
006750     MOVE SPACE     TO  �g������o.
006760*
006770*     CLOSE  ����t�@�C��.
006780*     OPEN I-O   ����t�@�C��.
006790*     PERFORM �G���[�����o.
006800*
006810*================================================================*
006811*================================================================*
006812 �ی��ҏ��擾 SECTION.
006813*
006814     MOVE  SPACE         TO �����於�̂v.
006815     MOVE  SPACE         TO �x���������v.
006816     MOVE  ZERO          TO �ڔ���敪�v.
006817*
006818     MOVE �ی���ʂv     TO �ہ|�ی����.
006819     MOVE �ی��Ҕԍ��v   TO �ہ|�ی��Ҕԍ�.
006820     READ �ی��҃}�X�^
006821     INVALID KEY
006822         MOVE SPACE      TO �����於�̂v
006823         MOVE SPACE      TO �x���������v
006824     NOT INVALID KEY
006825         IF �ہ|��������敪 = 1
006826             MOVE �ہ|�ی����   TO ����|�ی����
006827             MOVE �ہ|�ی��Ҕԍ� TO ����|�ی��Ҕԍ�
006828             READ ������}�X�^
006829             INVALID KEY
006830                 MOVE SPACE             TO �����於�̂v
006831                 MOVE SPACE             TO �x���������v
006832             NOT INVALID KEY
006833                 MOVE ����|�ی��Җ���  TO �����於�̂v
006834                 MOVE ����|�x��������  TO �x���������v
006835             END-READ
006836         ELSE
006837             MOVE �ہ|�ی��Җ���        TO �����於�̂v
006838             MOVE �ہ|�x��������        TO �x���������v
006839             MOVE �ہ|�ڔ���敪        TO �ڔ���敪�v
006840         END-IF
006841     END-READ.
006842*================================================================*
006843 �s�������擾 SECTION.
006844*
006845     MOVE  SPACE         TO �����於�̂v.
006846     MOVE  SPACE         TO �x���������v.
006847*
006848     MOVE �ی���ʂv               TO �s�|������.
006849     MOVE �ی��Ҕԍ��v             TO �s�|�s�����ԍ�.
006850     READ �s�����}�X�^
006851     INVALID KEY
006852         MOVE SPACE                TO �����於�̂v
006853         MOVE SPACE                TO �x���������v
006854     NOT INVALID KEY
006855         IF �s�|������敪 = 1
006856             MOVE �ی���ʂv       TO ����|�ی����
006857             MOVE �ی��Ҕԍ��v     TO ����|�ی��Ҕԍ�
006858             READ ������}�X�^
006859             INVALID KEY
006860                 MOVE SPACE        TO �����於�̂v
006861                 MOVE SPACE        TO �x���������v
006862             NOT INVALID KEY
006863                 MOVE ����|�ی��Җ���   TO �����於�̂v
006864                 MOVE ����|�x��������   TO �x���������v
006865             END-READ
006866          ELSE
006867             MOVE �s�|�s��������   TO �����於�̂v
006868             MOVE �s�|�x��������   TO �x���������v
006869          END-IF
006870      END-READ.
006871*================================================================*
006872 ���������擾 SECTION.
006873*
006874     MOVE SPACE TO �ی��҈����v.
006875     IF �����於�̂v NOT = SPACE
006876         EVALUATE �ی���ʂv
006877         WHEN 2
006878             IF �ڔ���敪�v = 1
006879                MOVE SPACE            TO �����v
006880             ELSE
006881                MOVE "�Љ�ی�������" TO �����v
006882             END-IF
006883         WHEN 6
006884             IF �ڔ���敪�v = 1
006885                MOVE "�i���فj"               TO �����v
006887             ELSE
006888                MOVE "�Љ�ی��������i���فj" TO �����v
006889             END-IF
006890         WHEN 7
006891             MOVE "�i�D���j"       TO �����v
006892         WHEN 3
006893             MOVE "���N�ی��g��"   TO �����v
006894         WHEN 4
006895             MOVE "���ϑg��"       TO �����v
006896         WHEN OTHER
006897             MOVE SPACE            TO �����v
006898         END-EVALUATE
006899*
006900         IF �x���������v = SPACE
006901             STRING  �����於�̂v  DELIMITED BY SPACE
006902                     �����v        DELIMITED BY SPACE
006903                    INTO �ی��҈����v
006904             END-STRING
006905         ELSE
006906             STRING  �����於�̂v  DELIMITED BY SPACE
006907                     �����v        DELIMITED BY SPACE
006908                     �x���������v  DELIMITED BY SPACE
006909                    INTO �ی��҈����v
006910             END-STRING
006911         END-IF
006912     END-IF.
006913*
006914*================================================================*
006915*================================================================*
006916 ������擾�Q SECTION.
006917* ����敪�A�U����̏����擾
006918* ����敪01
006919*     MOVE 01 TO ���|����敪.
006920*     READ ������}�X�^
006921*     NOT INVALID KEY
006922*        IF ���O�P�|�������X�V�t���O = 1
006923*           MOVE ���O�P�|���ۂU������敪  TO ���ۂU������敪�v 
006924*           MOVE ���O�P�|���ۂV������敪  TO ���ۂV������敪�v 
006925*           MOVE ���O�P�|���ۓ��Ј���敪  TO ���ۓ��Ј���敪�v 
006926*           MOVE ���O�P�|���ېU����敪    TO ���ېU����敪�v     
006927*           MOVE ���O�P�|�ЕۂU������敪  TO �ЕۂU������敪�v 
006928*           MOVE ���O�P�|�ЕۂV������敪  TO �ЕۂV������敪�v 
006929*           MOVE ���O�P�|�Еۓ��Ј���敪  TO �Еۓ��Ј���敪�v 
006930*           MOVE ���O�P�|�ЕېU����敪    TO �ЕېU����敪�v     
006931*           MOVE ���O�P�|�g���U������敪  TO �g���U������敪�v 
006932*           MOVE ���O�P�|�g���V������敪  TO �g���V������敪�v 
006933*           MOVE ���O�P�|�g�����Ј���敪  TO �g�����Ј���敪�v 
006934*           MOVE ���O�P�|�g���U����敪    TO �g���U����敪�v     
006935*           MOVE ���O�P�|���ςU������敪  TO ���ςU������敪�v 
006936*           MOVE ���O�P�|���ςV������敪  TO ���ςV������敪�v 
006937*           MOVE ���O�P�|���ϓ��Ј���敪  TO ���ϓ��Ј���敪�v 
006938*           MOVE ���O�P�|���ϐU����敪    TO ���ϐU����敪�v     
006939*           MOVE ���O�P�|�V�l�U������敪  TO �V�l�U������敪�v 
006940*           MOVE ���O�P�|�V�l�V������敪  TO �V�l�V������敪�v 
006941*           MOVE ���O�P�|�V�l���Ј���敪  TO �V�l���Ј���敪�v 
006942*           MOVE ���O�P�|�V�l�U����敪    TO �V�l�U����敪�v     
006943*           MOVE ���O�P�|�����U������敪  TO �����U������敪�v 
006944*           MOVE ���O�P�|�����V������敪  TO �����V������敪�v 
006945*           MOVE ���O�P�|�������Ј���敪  TO �������Ј���敪�v 
006946*           MOVE ���O�P�|�����U����敪    TO �����U����敪�v     
009271*           MOVE ���O�P�|�U�V���я��敪    TO �U�V���я��敪�v     
006947*        END-IF
006948*     END-READ.
      *
009438* ����敪02
009439     MOVE 02 TO ���|����敪.
009440     READ ������}�X�^
009441     NOT INVALID KEY
009443        MOVE ���O�Q�|�k�C�������\�敪  TO �k�C���敪�v 
009443        MOVE ���O�Q�|�X�����\�敪    TO �X�敪�v 
009443        MOVE ���O�Q�|��葍���\�敪    TO ���敪�v 
009443        MOVE ���O�Q�|�{�鑍���\�敪    TO �{��敪�v 
009443        MOVE ���O�Q�|�H�c�����\�敪    TO �H�c�敪�v 
009443        MOVE ���O�Q�|�R�`�����\�敪    TO �R�`�敪�v 
009443        MOVE ���O�Q�|���������\�敪    TO �����敪�v 
009443        MOVE ���O�Q�|��鑍���\�敪    TO ���敪�v 
009443        MOVE ���O�Q�|�Ȗؑ����\�敪    TO �Ȗ؋敪�v 
009443        MOVE ���O�Q�|�Q�n�����\�敪    TO �Q�n�敪�v 
009443        MOVE ���O�Q�|��ʑ����\�敪    TO ��ʋ敪�v 
009443        MOVE ���O�Q�|��t�����\�敪    TO ��t�敪�v 
009443        MOVE ���O�Q�|���������\�敪    TO �����敪�v 
009443        MOVE ���O�Q�|�_�ސ쑍���\�敪  TO �_�ސ�敪�v 
009443        MOVE ���O�Q�|�V�������\�敪    TO �V���敪�v 
009443        MOVE ���O�Q�|�x�R�����\�敪    TO �x�R�敪�v 
009443        MOVE ���O�Q�|�ΐ쑍���\�敪    TO �ΐ�敪�v 
009443        MOVE ���O�Q�|���䑍���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|�R�������\�敪    TO �R���敪�v 
009443        MOVE ���O�Q�|���쑍���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|�򕌑����\�敪    TO �򕌋敪�v 
009443        MOVE ���O�Q�|�É������\�敪    TO �É��敪�v 
009443        MOVE ���O�Q�|���m�����\�敪    TO ���m�敪�v 
009443        MOVE ���O�Q�|�O�d�����\�敪    TO �O�d�敪�v 
009443        MOVE ���O�Q�|���ꑍ���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|���s�����\�敪    TO ���s�敪�v 
009443        MOVE ���O�Q�|��㑍���\�敪    TO ���敪�v 
009443        MOVE ���O�Q�|���ɑ����\�敪    TO ���ɋ敪�v 
009443        MOVE ���O�Q�|�ޗǑ����\�敪    TO �ޗǋ敪�v 
009443        MOVE ���O�Q�|�a�̎R�����\�敪  TO �a�̎R�敪�v 
009443        MOVE ���O�Q�|���摍���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|���������\�敪    TO �����敪�v 
009443        MOVE ���O�Q�|���R�����\�敪    TO ���R�敪�v 
009443        MOVE ���O�Q�|�L�������\�敪    TO �L���敪�v 
009443        MOVE ���O�Q�|�R�������\�敪    TO �R���敪�v 
009443        MOVE ���O�Q�|���������\�敪    TO �����敪�v 
009443        MOVE ���O�Q�|���쑍���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|���Q�����\�敪    TO ���Q�敪�v 
009443        MOVE ���O�Q�|���m�����\�敪    TO ���m�敪�v 
009443        MOVE ���O�Q�|���������\�敪    TO �����敪�v 
009443        MOVE ���O�Q�|���ꑍ���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|���葍���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|�F�{�����\�敪    TO �F�{�敪�v 
009443        MOVE ���O�Q�|�啪�����\�敪    TO �啪�敪�v 
009443        MOVE ���O�Q�|�{�葍���\�敪    TO �{��敪�v 
009443        MOVE ���O�Q�|�����������\�敪  TO �������敪�v 
009443        MOVE ���O�Q�|���ꑍ���\�敪    TO ����敪�v 
009468     END-READ.
006949*
006950*================================================================*
006951*================================================================*
006952 ����Ώۃ`�F�b�N  SECTION.
006953*
006954*  ����敪�ɂ��U�蕪�� �� �U�E�V���������i����敪 0:��� 1:������Ȃ��j
006955* �i�ꊇ����̂݁j
006956*
006957     MOVE SPACE TO ����t���O.
006958*
006959*     IF �A���|�ꊇ�敪 NOT = 1
006960*        MOVE "YES" TO ����t���O
006961*     ELSE
006963        EVALUATE ��Q�|�ی����
006964        WHEN 01
006965        WHEN 08
006966*           IF ���ۂU������敪�v NOT = 1
006967              MOVE "YES" TO ����t���O
006968*           END-IF
006969        WHEN 02
006970        WHEN 06
006971        WHEN 07
006972*           IF �ЕۂU������敪�v NOT = 1
006973              MOVE "YES" TO ����t���O
006974*           END-IF
006975*        WHEN 03
006976*           IF �g���U������敪�v NOT = 1
006977*              MOVE "YES" TO ����t���O
006978*           END-IF
006979*        WHEN 04
006980*        WHEN 09
006981*           IF ���ςU������敪�v NOT = 1
006982*              MOVE "YES" TO ����t���O
006983*           END-IF
006984        WHEN 05
006985*           IF �V�l�U������敪�v NOT = 1
006986              MOVE "YES" TO ����t���O
006987*           END-IF
006988        WHEN 50 THRU 60
006989*           IF �����U������敪�v NOT = 1
                 IF ��Q�|���ރR�[�h NOT = 6
006990              MOVE "YES" TO ����t���O
006991           END-IF
006992*        WHEN OTHER
006993*           MOVE "YES" TO ����t���O
006994        END-EVALUATE
      *
              IF ����t���O = "YES"
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 01)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 01))
                    IF (�k�C���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 02)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 02))
                    IF (�X�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 03)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 03))
                    IF (���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 04)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 04))
                    IF (�{��敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 05)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 05))
                    IF (�H�c�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 06)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 06))
                    IF (�R�`�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 07)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 07))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 08)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 08))
                    IF (���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 09)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 09))
                    IF (�Ȗ؋敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 10)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 10))
                    IF (�Q�n�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 11)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 11))
                    IF (��ʋ敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
      */��t�͌��ŗL�I���ɂȂ��Ă��Ă��U�V�����o��*/150409
      *           IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 12)) OR
      *              ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 12))
      *              IF (��t�敪�v = ZERO)
009576*                 MOVE "YES" TO ����t���O
      *              ELSE
009576*                 MOVE SPACE TO ����t���O
009577*              END-IF
009577*           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 13)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 13))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 14)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 14))
                    IF (�_�ސ�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 15)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 15))
                    IF (�V���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 16)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 16))
                    IF (�x�R�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 17)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 17))
                    IF (�ΐ�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 18)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 18))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 19)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 19))
                    IF (�R���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 20)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 20)) OR
                    (((��Q�|�ی���� >= 50) AND (��Q�|�ی���� <= 60)) AND (��Q�|�ی��Ҕԍ�(3:2) = 20))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 21)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 21))
                    IF (�򕌋敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 22)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 22))
                    IF (�É��敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 23)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 23))
                    IF (���m�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 24)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 24))
                    IF (�O�d�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 25)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 25))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 26)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 26)) OR
                    (((��Q�|�ی���� >= 50) AND (��Q�|�ی���� <= 60)) AND (��Q�|�ی��Ҕԍ�(3:2) = 26))
                    IF (���s�敪�v = ZERO)
009576                  MOVE "YES" TO ����t���O
                    ELSE
                       IF ((��Q�|�ی���� = 53) AND (��Q�|�ی��Ҕԍ�(1:4) = 3926))
009576                    MOVE "YES" TO ����t���O
                       ELSE
009576                    MOVE SPACE TO ����t���O
                       END-IF
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(3:2) = 27)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 27)) OR
                    (((��Q�|�ی���� >= 50) AND (��Q�|�ی���� <= 60)) AND (��Q�|�ی��Ҕԍ�(3:2) = 27))
                    IF (���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 28)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 28))
                    IF (���ɋ敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 29)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 29))
                    IF (�ޗǋ敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 30)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 30))
                    IF (�a�̎R�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 31)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 31))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 32)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 32))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 33)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 33))
                    IF (���R�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 34)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 34))
                    IF (�L���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 35)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 35))
                    IF (�R���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 36)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 36))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 37)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 37))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 38)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 38))
                    IF (���Q�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 39)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 39))
                    IF (���m�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 40)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 40))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 41)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 41))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 42)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 42))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 43)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 43))
                    IF (�F�{�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 44)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 44))
                    IF (�啪�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 45)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 45))
                    IF (�{��敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 46)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 46))
                    IF (�������敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 47)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 47))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
009577*        END-IF
009581     END-IF.
      */�_�ސ쌧�Ζ���06141261 ���U���̑ΏۊO�ɂ���/120427
           IF ��Q�|�ی��Ҕԍ� = "06141261"
006957         MOVE SPACE TO ����t���O
           END-IF.
006996*
007011*================================================================*
       �x������ SECTION.
      *
           PERFORM VARYING �x���J�E���^ FROM 1 BY 1
                   UNTIL �x���J�E���^ > �x���񐔂v
               MOVE "YES" TO �x���t���O
           END-PERFORM.
      *
005110*================================================================*
007012******************************************************************
007013 END PROGRAM YHN436.
007014******************************************************************
