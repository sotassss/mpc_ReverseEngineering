000001******************************************************************
000002*            IDENTIFICATION      DIVISION                        *
000003******************************************************************
000004 IDENTIFICATION          DIVISION.
000005 PROGRAM-ID.             YHN431.
000006 AUTHOR.                 �r�c�@�K�q
000007*
000008*----------------------------------------------------------------*
000009*         �������ꊇ�y�ް��쐬�z�_�{����޳��
000010*  �����N���o�[�W���� 
000011*         MED = YHN430 
000012*----------------------------------------------------------------*
000013 DATE-WRITTEN.           2015-02-24
000014 DATE-COMPILED.          2015-02-24
000015*----------------------------------------------------------------*
000016******************************************************************
000017*            ENVIRONMENT         DIVISION                        *
000018******************************************************************
000019 ENVIRONMENT             DIVISION.
000020 CONFIGURATION           SECTION.
000021 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000022 OBJECT-COMPUTER.        FMV-DESKPOWER.
000023 SPECIAL-NAMES.          CONSOLE  IS  CONS
000024                         SYSERR   IS  MSGBOX.
000025 INPUT-OUTPUT            SECTION.
000026 FILE-CONTROL.
000027     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000028                             ORGANIZATION             IS  INDEXED
000029                             ACCESS MODE              IS  DYNAMIC
000030                             RECORD KEY               IS  ���|����敪
000031                             FILE STATUS              IS  ��ԃL�[
000032                             LOCK        MODE         IS  AUTOMATIC.
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
000078     SELECT  ������}�X�^    ASSIGN      TO         SEIKYUSL
000079                             ORGANIZATION             IS  INDEXED
000080                             ACCESS MODE              IS  DYNAMIC
000081                             RECORD KEY               IS  ����|�ی����
000082                                                          ����|�ی��Ҕԍ�
000083                             FILE STATUS              IS  ��ԃL�[
000084                             LOCK    MODE             IS  AUTOMATIC.
000085     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000086                             ORGANIZATION             IS  INDEXED
000087                             ACCESS MODE              IS  DYNAMIC
000088                             RECORD KEY               IS  �s�|������
000089                                                          �s�|�s�����ԍ�
000090                             ALTERNATE RECORD KEY     IS  �s�|������
000091                                                          �s�|�s��������
000092                                                          �s�|�s�����ԍ�
000093                             FILE STATUS              IS  ��ԃL�[
000094                             LOCK        MODE         IS  AUTOMATIC.
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
000095     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4311L.DAT"
000096                             ORGANIZATION             IS  INDEXED
000097                             ACCESS                   IS  DYNAMIC
000905                             RECORD      KEY          IS  ��P�|�����a��N��
000907                                                          ��P�|���ރR�[�h
                                                                ��P�|���R�[�h
                                                                ��P�|�ی���
000908                                                          ��P�|�ی��Ҕԍ�
000909                                                          ��P�|�{�l�Ƒ��敪
                                                                ��P�|���S����
000910                                                          ��P�|���҃J�i
000911                                                          ��P�|���҃R�[�h
000912                                                          ��P�|�{�p�a��N��
000106                             FILE        STATUS       IS  ��ԃL�[
000107                             LOCK        MODE         IS  AUTOMATIC.
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
000108     SELECT  ��ƃt�@�C����  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W43121L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  �실�|�����a��N��
000112                                                          �실�|���ރR�[�h
000112                                                          �실�|���R�[�h
000112                                                          �실�|�ی���
000113                                                          �실�|�ی��Ҕԍ�
000911                                                          �실�|���҃R�[�h
000912                                                          �실�|�{�p�a��N��
000119                             FILE        STATUS       IS  ��ԃL�[
000120                             LOCK        MODE         IS  AUTOMATIC.
000121******************************************************************
000122*                      DATA DIVISION                             *
000123******************************************************************
000124 DATA                    DIVISION.
000125 FILE                    SECTION.
000126*                           �m�q�k��  �Q�T�U�n
000127 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
000128     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000991     COPY SEIGYO02        OF  XFDLIB  JOINING   ���O�Q   AS  PREFIX.
000129*                           �m�q�k��  �R�Q�O�n
000130 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
000131     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000138*                           �m�q�k��  �P�Q�W�n
000139 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
000140     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
000141*                           �m�q�k��  �Q�T�U�n
000142 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000143     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
      *
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
000144* 
001190 FD  ��ƃt�@�C���P RECORD  CONTAINS 256 CHARACTERS.
001200 01  ��P�|���R�[�h.
001210     03  ��P�|���R�[�h�L�[.
001220         05  ��P�|�����a��N��.
001230             07  ��P�|�����a��            PIC 9.
001240             07  ��P�|�����N              PIC 9(2).
001250             07  ��P�|������              PIC 9(2).
001261         05  ��P�|���ރR�[�h              PIC 9(1).
001261         05  ��P�|���R�[�h                PIC X(2).
001400         05  ��P�|�ی���                  PIC 9(2).
001271         05  ��P�|�ی��Ҕԍ�.
001530             07 ��P�|�@�ʔԍ�             PIC X(2).
001540             07 ��P�|�۔�.
001550                09 ��P�|�s���{���ԍ�      PIC X(2).
001560                09 ��P�|�ی��ԍ�          PIC X(3).
001570                09 ��P�|���ؔԍ��v�q      PIC X.
001580                09 FILLER                  PIC X(2).
001280         05  ��P�|�{�l�Ƒ��敪            PIC 9.
               05  ��P�|���S����                PIC 9(2).
001300         05  ��P�|���҃J�i                PIC X(50).
001310         05  ��P�|���҃R�[�h.
001320             07 ��P�|���Ҕԍ�             PIC 9(6).
001330             07 ��P�|�}��                 PIC X(1).
001340         05  ��P�|�{�p�a��N��.
001350             07  ��P�|�{�p�a��            PIC 9.
001360             07  ��P�|�{�p�N              PIC 9(2).
001370             07  ��P�|�{�p��              PIC 9(2).
000164     03  ��P�|���R�[�h�f�[�^.
000165         05  ��P�|��ی��Ҏ���            PIC X(50).
000166         05  ��P�|���Ҏ���                PIC X(50).
001400         05  ��P�|�ی����                PIC 9(2).
001271         05  ��P�|�ی��Ҕԍ����K          PIC X(8).
000167         05  ��P�|��p�z                  PIC 9(6).
000168         05  ��P�|���S�z                  PIC 9(6).
000169         05  ��P�|�����z                  PIC 9(6).
000169         05  ��P�|������                  PIC 9(2).
001400         05  ��P�|�e�ی����              PIC 9(2).
000172         05  FILLER                        PIC X(37).
000173*
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
000174*                           �m�q�k��  �P�Q�W�n
000175 FD  ��ƃt�@�C���� RECORD  CONTAINS 128 CHARACTERS.
000176 01  �실�|���R�[�h.
000177     03  �실�|���R�[�h�L�[.
000178         05  �실�|�����a��N��.
000179             07  �실�|�����a��              PIC 9.
000180             07  �실�|�����N                PIC 9(2).
000181             07  �실�|������                PIC 9(2).
001261         05  �실�|���ރR�[�h                PIC 9(1).
001261         05  �실�|���R�[�h                  PIC X(2).
001400         05  �실�|�ی���                    PIC 9(2).
000183         05  �실�|�ی��Ҕԍ�                PIC X(10).
001310         05  �실�|���҃R�[�h.
001320             07 �실�|���Ҕԍ�               PIC 9(6).
001330             07 �실�|�}��                   PIC X(1).
001340         05  �실�|�{�p�a��N��.
001350             07  �실�|�{�p�a��              PIC 9.
001360             07  �실�|�{�p�N                PIC 9(2).
001370             07  �실�|�{�p��                PIC 9(2).
000188     03  �실�|���R�[�h�f�[�^.
001400         05  �실�|�ی����                  PIC 9(2).
001280         05  �실�|�{�l�Ƒ��敪              PIC 9.
000166         05  �실�|���Ҏ���                  PIC X(50).
000192         05  �실�|�����z                    PIC 9(9).
000197         05  �실�|������                    PIC 9(4).
000201         05  FILLER                          PIC X(30).
000173*
000202*----------------------------------------------------------------*
001240******************************************************************
001250*                WORKING-STORAGE SECTION                         *
001260******************************************************************
001270 WORKING-STORAGE         SECTION.
001280 01 �L�[����                           PIC X     VALUE SPACE.
001290 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
001300 01 �I���t���O                         PIC X(3)  VALUE SPACE.
001301 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
001302 01 �쐬�t���O                         PIC X(3)  VALUE SPACE.
001303 01 ���s�L�[�v                         PIC X(3)  VALUE SPACE.
001304 01 �{�p�L�^�L�v                       PIC X(3)  VALUE SPACE.
001305*
001310 01 �ی���ʂv�q                       PIC 9(2)  VALUE ZERO.
001311 01 ���Z�v�g��ނv�q                   PIC X(4)  VALUE SPACE.
001320 01 �ی��Ҕԍ��v�q.
001530    03 �@�ʔԍ��v�q                    PIC X(2)  VALUE SPACE.
001540    03 �۔Ԃv�q.
001550       05 �s���{���ԍ��v�q             PIC X(2)  VALUE SPACE.
001560       05 �ی��ԍ��v�q                 PIC X(3)  VALUE SPACE.
001570       05 ���ؔԍ��v�q                 PIC X     VALUE SPACE.
001580       05 FILLER                       PIC X(2)  VALUE SPACE.
001321 01 �{�l�Ƒ��敪�v�q                   PIC 9     VALUE ZERO.
001330 01 �����a��N���v�q.
001340    03 �����a��v�q                    PIC 9     VALUE ZERO.
001350    03 �����N�v�q                      PIC 9(2)  VALUE ZERO.
001360    03 �������v�q                      PIC 9(2)  VALUE ZERO.
001370*
001410 01 �����N���v.
001420    03 �����a��v                      PIC 9(1)  VALUE ZERO.
001430    03 �����N�v                        PIC 9(2)  VALUE ZERO.
001440    03 �������v                        PIC 9(2)  VALUE ZERO.
001441 01 �{�p�N���v.
001442    03 �{�p�a��v                      PIC 9(1)  VALUE ZERO.
001443    03 �{�p�N�v                        PIC 9(2)  VALUE ZERO.
001444    03 �{�p���v                        PIC 9(2)  VALUE ZERO.
001450 01 �W�v��Ɨp�v.
001460    03 �����v                          PIC 9(4)  VALUE ZERO.
001461    03 ���ʐ��v                        PIC 9(5)  VALUE ZERO.
001470    03 �ʉ@���v                        PIC 9(4)  VALUE ZERO.
001480    03 ��p�z�v                        PIC 9(9)  VALUE ZERO.
001490    03 ���S�z�v                        PIC 9(9)  VALUE ZERO.
001500    03 �����z�v                        PIC 9(9)  VALUE ZERO.
001545    03 �{�l�����v                      PIC 9(3)  VALUE ZERO.
001545    03 �{�l�����v                      PIC 9(4)  VALUE ZERO.
001548    03 �{�l��p�z�v                    PIC 9(7)  VALUE ZERO.
001549    03 �{�l���S�z�v                    PIC 9(7)  VALUE ZERO.
001550    03 �{�l�����z�v                    PIC 9(7)  VALUE ZERO.
001551    03 �Ƒ������v                      PIC 9(3)  VALUE ZERO.
001551    03 �Ƒ������v                      PIC 9(4)  VALUE ZERO.
001552    03 �Ƒ���p�z�v                    PIC 9(7)  VALUE ZERO.
001553    03 �Ƒ����S�z�v                    PIC 9(7)  VALUE ZERO.
001554    03 �Ƒ������z�v                    PIC 9(7)  VALUE ZERO.
001510 01 �{�l�Ƒ��敪�v                     PIC 9(1)  VALUE ZERO.
001520 01 �t�@�C����                         PIC N(2)  VALUE SPACE.
001530 01 �ی���ʂv                         PIC 9(2)  VALUE ZERO.
001540 01 �ی��Ҕԍ��v                       PIC X(10) VALUE SPACE.
001541 01 �e�ی���ʂv                       PIC 9(2)  VALUE ZERO.
001542*
001556* �������O�p
001557 01 �������O�v�q                       PIC 9     VALUE ZERO.
001558 01 ���i�h�r�v�j                       PIC X(2)  VALUE SPACE.
001559 01 ���ǂi�h�r�v�j                     PIC X(2)  VALUE SPACE.
001560 01 �������O�`�F�b�N                   PIC 9     VALUE ZERO.
001561 01 ���v�j                             PIC X(2)  VALUE SPACE.
001562 01 ���v�j�Q                           PIC X(2)  VALUE SPACE.
001563 01 ������v�j                         PIC X(2)  VALUE SPACE.
001564 01 �ی���ʌ��p�v�j                   PIC 9(2)  VALUE ZERO.
001565*
001566* �G���[���b�Z�[�W�p
001567 01 �G���[���b�Z�[�W�v.
001568    03 �G���[���҃R�[�h�v              PIC X(7) VALUE SPACE.
001569    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
001570    03 �G���[�ی���ʂv                PIC X(2) VALUE SPACE.
001571    03 FILLER                          PIC X(10) VALUE SPACE.
001577** �������Z�܂Ƃߗp
001578 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
001579*
001580* ��O�p
001581 01 ����R�[�h�v                       PIC 9(2)  VALUE ZERO.
002877*
001982 01 �������֘A�v.
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
001582*
      *
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
001583******************************************************************
001584*                          �A������                              *
001585******************************************************************
001586*
001587****************
001588* ��ʓ��͏�� *
001589****************
001590**
001591 01 �A���|��ʏ��x�g�m�S�R�O   IS EXTERNAL.
001592    03 �A���|�����N��.
001593       05 �A���|�����a��               PIC 9.
001594       05 �A���|�����N                 PIC 9(2).
001595       05 �A���|������                 PIC 9(2).
001596    03 �A���|��o�N����.
001597       05 �A���|��o�a��               PIC 9.
001598       05 �A���|��o�N                 PIC 9(2).
001599       05 �A���|��o��                 PIC 9(2).
001600       05 �A���|��o��                 PIC 9(2).
001601    03 �A���|���Z�v�g���              PIC X(4).
001602    03 �A���|�ی����                  PIC 9(2).
001603    03 �A���|������                  PIC 9.
001604    03 �A���|�{�l�Ƒ�                  PIC 9.
001605    03 �A���|�p�����                  PIC 9.
001606    03 �A���|�������O                  PIC 9.
001607    03 �A���|���i�h�r                  PIC X(2).
001608    03 �A���|���ǂi�h�r                PIC X(2).
001609*
001782********************
001792* ���b�Z�[�W�\���L�[ *
001802********************
001812 01 �A���R�|�L�[ IS EXTERNAL.
001822    03  �A���R�|���b�Z�[�W             PIC N(20).
001832    03  �A���R�|���b�Z�[�W�P           PIC X(20).
001842*
001852************************
001862* �������Z�܂Ƃ�
001872************************
001882 01 �A���Z�܂Ƃ߁|�L�[ IS EXTERNAL.
001892    03 �A���Z�܂Ƃ߁|�{�p�a��N��.
001902       05 �A���Z�܂Ƃ߁|�{�p�a��               PIC 9.
001912       05 �A���Z�܂Ƃ߁|�{�p�N��.
001922          07 �A���Z�܂Ƃ߁|�{�p�N              PIC 9(2).
001932          07 �A���Z�܂Ƃ߁|�{�p��              PIC 9(2).
001942    03 �A���Z�܂Ƃ߁|���҃R�[�h.
001952       05 �A���Z�܂Ƃ߁|���Ҕԍ�               PIC 9(6).
001962       05 �A���Z�܂Ƃ߁|�}��                   PIC X(1).
001972**-------------------------------------------------------**
001982*   1:�������Z�v�g�Ȃ��̖{�̂܂Ƃ߂̔���
001992*   2:���l�E���p�̎Еۏ������Z���̔���
002002    03 �A���Z�܂Ƃ߁|����敪                  PIC 9.
002012**-------------------------------------------------------**
002022*  / OUT /�@ 0:�ΏۊO�A1:�Ώ�
002032    03 �A���Z�܂Ƃ߁|���茋��                  PIC 9.
002042**
      * 
002160******************************************************************
002170*                      PROCEDURE  DIVISION                       *
002180******************************************************************
002190 PROCEDURE               DIVISION.
002200************
002210*           *
002220* ��������   *
002230*           *
002240************
002250     PERFORM �t�@�C���I�[�v��.
002251     PERFORM �A�����ڑҔ�.
002260     PERFORM ������擾.
002270************
002280*           *
002290* �又��     *
002300*           *
002310************
002320     PERFORM ��ƃt�@�C���P�쐬.
002330     PERFORM ��ƃt�@�C���Q�쐬.
002340************
002350*           *
002360* �I������   *
002370*           *
002380************
002390     PERFORM �I������.
002400     MOVE ZERO TO PROGRAM-STATUS.
002410     EXIT PROGRAM.
002420*
002430*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002440*================================================================*
002450 �A�����ڑҔ� SECTION.
002460*
002470* �A�����ڂ̑Ҕ�
002500     MOVE �A���|�����a��      TO �����a��v�q.
002510     MOVE �A���|�����N        TO �����N�v�q.
002520     MOVE �A���|������        TO �������v�q.
002530     MOVE �A���|���Z�v�g���  TO ���Z�v�g��ނv�q.
002540     MOVE �A���|�ی����      TO �ی���ʂv�q.
002550     MOVE �A���|�{�l�Ƒ�      TO �{�l�Ƒ��敪�v�q.
002551     MOVE �A���|�������O      TO �������O�v�q.
002552     MOVE �A���|���i�h�r      TO ���i�h�r�v�j.
002553     MOVE �A���|���ǂi�h�r    TO ���ǂi�h�r�v�j.
002554*
002560*================================================================*
002570 �t�@�C���I�[�v�� SECTION.
002580*
002581     OPEN INPUT ������}�X�^.
002582         MOVE NC"������" TO �t�@�C����.
002583         PERFORM �I�[�v���`�F�b�N.
002590     OPEN INPUT ��f�ҏ��e.
002600         MOVE NC"��f�ҏ��e" TO �t�@�C����.
002610         PERFORM �I�[�v���`�F�b�N.
002617     OPEN INPUT ������}�X�^.
002618         MOVE NC"������" TO �t�@�C����.
002619         PERFORM �I�[�v���`�F�b�N.
002620     OPEN INPUT �s�����}�X�^.
002621         MOVE NC"�s����" TO �t�@�C����.
002622         PERFORM �I�[�v���`�F�b�N.
003250     OPEN INPUT ���Z�v�g�e.
003260         MOVE NC"���Z�v�g�e" TO �t�@�C����.
003270         PERFORM �I�[�v���`�F�b�N.
002623*================================================================*
002630 �I�[�v���`�F�b�N SECTION.
002640*
002650     IF ��ԃL�[  NOT =  "00"
002660         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
002670         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
002680         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
002690                                                 UPON CONS
003131*-----------------------------------------*
003132         CALL "actcshm"  WITH C LINKAGE
003133*-----------------------------------------*
002700         ACCEPT  �L�[���� FROM CONS
002710         PERFORM �t�@�C����
002720         MOVE 99 TO PROGRAM-STATUS
002730         EXIT PROGRAM.
002731*================================================================*
002732 ������擾 SECTION.
002733*
002734     MOVE ZEROS TO ���|����敪.
002735     READ ������}�X�^
002736     NOT INVALID KEY
002738         MOVE ���|����R�[�h   TO ����R�[�h�v
002739     END-READ.
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
002740*
002741*================================================================*
002750 �G���[�\�� SECTION.
002760*
002770     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
002780     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
002790     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
002800     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
002810     ACCEPT  �L�[���� FROM CONS.
002820     PERFORM �t�@�C����.
002830     MOVE 99 TO PROGRAM-STATUS.
002840     EXIT PROGRAM.
002850*================================================================*
002860 ��ƃt�@�C���P�쐬 SECTION.
002870*
002880     OPEN OUTPUT ��ƃt�@�C���P.
002890         MOVE NC"���" TO �t�@�C����.
002900         PERFORM �I�[�v���`�F�b�N.
002901*
002902     EVALUATE ���Z�v�g��ނv�q
002903*        ***************************************
002904*        * ���ہE�ЕہE�ސE�E�g���E���ρE���q��
002905*        ***************************************
002906         WHEN "KOKU"
002918         WHEN "SYAH"
002907         WHEN "KUMI"
002909         WHEN "KYOU"
002911         WHEN "TAIS"
002912         WHEN "JIEI"
002914             PERFORM ��ƃt�@�C���P�쐬�P
002920*        *************
002921*        * �V�l *
002922*        *************
002923         WHEN "ROUJ"
002924             PERFORM ��ƃt�@�C���P�쐬�R
002925*        *************
002926*        * ���� *
002927*        *************
002928         WHEN "JYOS"
002929             PERFORM ��ƃt�@�C���P�쐬�S
002930*        *************
002931*        * �S���    *
002932*        *************
002933         WHEN "ALL "
002934             PERFORM ��ƃt�@�C���P�쐬�T
002940     END-EVALUATE.
002945*
003140     CLOSE ��ƃt�@�C���P.
003141*
003690*================================================================*
003700 ��ƃt�@�C���P�쐬�P SECTION.
003710*
003720***************************************
003740* ���ہE�ЕہE�ސE�E�g���E���ρE���q��
003750***************************************
005170     MOVE �����a��v�q  TO ���Z�|�����a��.
005180     MOVE �����N�v�q    TO ���Z�|�����N.  
005190     MOVE �������v�q    TO ���Z�|������.  
013730     MOVE 1             TO ���Z�|���Z���.
005200     MOVE SPACE         TO ���Z�|�����ی��Ҕԍ�.
005230     MOVE ZERO          TO ���Z�|���Ҕԍ�.
005240     MOVE SPACE         TO ���Z�|�}��.    
013770     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
000250                                  ���Z�|���Z���
000230                                  ���Z�|�����ی��Ҕԍ�
000240                                  ���Z�|���҃R�[�h
003762     END-START.
003763     IF ��ԃL�[ = "00"
003764         MOVE SPACE  TO �I���t���O
003765         PERFORM ���Z�v�g�e�Ǎ�
003766         PERFORM UNTIL ( �I���t���O = "YES" ) OR
005330                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
005340                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
005350                       ( ���Z�|������   NOT = �������v�q   ) OR
                             ( ���Z�|���Z��� NOT = 1 )
003770             PERFORM �f�[�^�`�F�b�N
003830             IF ���s�L�[�v = "YES"
003920                IF (��|�ی����  = �ی���ʂv�q) OR
                         ((�ی���ʂv�q = 02) AND (��|�ی���� = 06 OR 07))
003923** �{�l�Ƒ��敪���肩
003924                   MOVE SPACE  TO  �쐬�t���O
003926                   IF �{�l�Ƒ��敪�v�q NOT = ZERO
003927                      IF �{�l�Ƒ��敪�v�q = ��|�{�l�Ƒ��敪
003929                         MOVE "YES"  TO  �쐬�t���O
003930                      END-IF
003931                   ELSE
003932                      MOVE "YES"  TO  �쐬�t���O
003933                   END-IF 
003934** �������O��
003935                   IF �쐬�t���O  = "YES"
003936                      MOVE SPACE  TO  �쐬�t���O
003937                      MOVE ��|�ی���� TO �ی���ʌ��p�v�j
003938                      PERFORM �������O�擾
003939*                   (����1�A���O2�A����0)
003940                      IF �������O�v�q NOT = ZERO
003941                         IF �������O�v�q = 1
003942                            IF �������O�`�F�b�N = 1
003943                               MOVE "YES"  TO  �쐬�t���O
003944                            END-IF
003945                         ELSE
003946                            IF �������O�`�F�b�N = ZERO
003947                               MOVE "YES"  TO  �쐬�t���O
003948                            END-IF
003949                         END-IF
003950                      ELSE
003951                         MOVE "YES"  TO  �쐬�t���O
003952                      END-IF 
003953                   END-IF 
003954*
003958                   IF �쐬�t���O  = "YES"
003963                      MOVE SPACE TO ��P�|���R�[�h
003964                      INITIALIZE ��P�|���R�[�h
003937                      MOVE ��|�ی����    TO ��P�|�ی����
003966                      MOVE ��|�ی��Ҕԍ�  TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
003967                      PERFORM ���R�[�h�Z�b�g
003968                      PERFORM ���ۗ����v�Z
003970                      PERFORM �t�@�C������
003971                   END-IF 
003981                END-IF 
004161             END-IF
004170             PERFORM ���Z�v�g�e�Ǎ�
004180         END-PERFORM
004190     END-IF.
004290*================================================================*
004291 ��ƃt�@�C���P�쐬�R SECTION.
004292*
004293**********************************
004294*  �V�l
004295**********************************
005170     MOVE �����a��v�q  TO ���Z�|�����a��.
005180     MOVE �����N�v�q    TO ���Z�|�����N.  
005190     MOVE �������v�q    TO ���Z�|������.  
013730     MOVE 2             TO ���Z�|���Z���.
005200     MOVE SPACE         TO ���Z�|�����ی��Ҕԍ�.
005230     MOVE ZERO          TO ���Z�|���Ҕԍ�.
005240     MOVE SPACE         TO ���Z�|�}��.    
013770     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
000250                                  ���Z�|���Z���
000230                                  ���Z�|�����ی��Ҕԍ�
000240                                  ���Z�|���҃R�[�h
003762     END-START.
003763     IF ��ԃL�[ = "00"
003764         MOVE SPACE  TO �I���t���O
003765         PERFORM ���Z�v�g�e�Ǎ�
003766         PERFORM UNTIL ( �I���t���O = "YES" ) OR
005330                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
005340                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
005350                       ( ���Z�|������   NOT = �������v�q   ) OR
                             ( ���Z�|���Z��� NOT = 2 )
004315*
003770             PERFORM �f�[�^�`�F�b�N
003830             IF ���s�L�[�v = "YES"
004318** �{�l�Ƒ��敪���肩
004319                MOVE SPACE  TO  �쐬�t���O
004320                IF �{�l�Ƒ��敪�v�q NOT = ZERO
004321                   IF �{�l�Ƒ��敪�v�q = ��|�{�l�Ƒ��敪
004322                      MOVE "YES"  TO  �쐬�t���O
004323                   END-IF
004324                ELSE
004325                   MOVE "YES"  TO  �쐬�t���O
004326                END-IF 
004327** �������O��
004328                IF �쐬�t���O  = "YES"
004329                   MOVE SPACE  TO  �쐬�t���O
004330                   MOVE ��|������ TO �ی���ʌ��p�v�j
004331                   PERFORM �������O�擾
004332*                 (����1�A���O2�A����0)
004333                   IF �������O�v�q NOT = ZERO
004334                      IF �������O�v�q = 1
004335                         IF �������O�`�F�b�N = 1
004336                            MOVE "YES"  TO  �쐬�t���O
004337                         END-IF
004338                      ELSE
004339                         IF �������O�`�F�b�N = ZERO
004340                            MOVE "YES"  TO  �쐬�t���O
004341                         END-IF
004342                      END-IF
004343                   ELSE
004344                      MOVE "YES"  TO  �쐬�t���O
004345                   END-IF 
004346                END-IF 
004347*
004348                IF �쐬�t���O  = "YES"
004351                   MOVE SPACE TO ��P�|���R�[�h
004352                   INITIALIZE ��P�|���R�[�h
003937                   MOVE ��|������           TO ��P�|�ی����
004354                   MOVE ��|��p���S�Ҕԍ�     TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
004355                   PERFORM ���R�[�h�Z�b�g
004356                   PERFORM �V�l�����v�Z
004357* �����z�� ZERO �̎��́A�������܂Ȃ��B
004358                   IF  ��P�|�����z NOT = ZERO
004359                       PERFORM �t�@�C������
004360                   END-IF
004361                END-IF 
004362             END-IF 
004364             PERFORM ���Z�v�g�e�Ǎ�
004365         END-PERFORM
004366     END-IF.
004367*================================================================*
004368 ��ƃt�@�C���P�쐬�S SECTION.
004369*
004370**********************************
004371*  ����
004372**********************************
005170     MOVE �����a��v�q  TO ���Z�|�����a��.
005180     MOVE �����N�v�q    TO ���Z�|�����N.  
005190     MOVE �������v�q    TO ���Z�|������.  
013730     MOVE 3             TO ���Z�|���Z���.
005200     MOVE SPACE         TO ���Z�|�����ی��Ҕԍ�.
005230     MOVE ZERO          TO ���Z�|���Ҕԍ�.
005240     MOVE SPACE         TO ���Z�|�}��.    
013770     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
000250                                  ���Z�|���Z���
000230                                  ���Z�|�����ی��Ҕԍ�
000240                                  ���Z�|���҃R�[�h
003762     END-START.
003763     IF ��ԃL�[ = "00"
003764         MOVE SPACE  TO �I���t���O
003765         PERFORM ���Z�v�g�e�Ǎ�
003766         PERFORM UNTIL ( �I���t���O = "YES" ) OR
005330                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
005340                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
005350                       ( ���Z�|������   NOT = �������v�q   ) OR
                             ( ���Z�|���Z��� NOT = 3 )
004315*
003770             PERFORM �f�[�^�`�F�b�N
003830             IF ���s�L�[�v = "YES"
004392*
004395** �{�l�Ƒ��敪���肩
004396                MOVE SPACE  TO  �쐬�t���O
004397                IF �{�l�Ƒ��敪�v�q NOT = ZERO
004398                   IF �{�l�Ƒ��敪�v�q = ��|�{�l�Ƒ��敪
004399                      MOVE "YES"  TO  �쐬�t���O
004400                   END-IF
004401                ELSE
004402                   MOVE "YES"  TO  �쐬�t���O
004403                END-IF 
004404** �������O��
004405                IF �쐬�t���O  = "YES"
004406                   MOVE SPACE  TO  �쐬�t���O
004407                   MOVE ��|������� TO �ی���ʌ��p�v�j
004408                   PERFORM �������O�擾
004409*                 (����1�A���O2�A����0)
004410                   IF �������O�v�q NOT = ZERO
004411                      IF �������O�v�q = 1
004412                         IF �������O�`�F�b�N = 1
004413                            MOVE "YES"  TO  �쐬�t���O
004414                         END-IF
004415                      ELSE
004416                         IF �������O�`�F�b�N = ZERO
004417                            MOVE "YES"  TO  �쐬�t���O
004418                         END-IF
004419                      END-IF
004420                   ELSE
004421                      MOVE "YES"  TO  �쐬�t���O
004422                   END-IF 
004423                END-IF 
004424*
004425                IF �쐬�t���O  = "YES"
004433                   MOVE SPACE TO ��P�|���R�[�h
004434                   INITIALIZE ��P�|���R�[�h
003937                   MOVE ��|�������           TO ��P�|�ی����
004436                   MOVE ��|��p���S�Ҕԍ����� TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
004437                   PERFORM ���R�[�h�Z�b�g
004438                   PERFORM ���������v�Z
004445* �����z�� ZERO �̎��́A�������܂Ȃ��B
004446                   IF  ��P�|�����z NOT = ZERO
004447                       PERFORM �t�@�C������
004448                   END-IF
004449                END-IF
004450             END-IF
004452             PERFORM ���Z�v�g�e�Ǎ�
004453         END-PERFORM
004454     END-IF.
004455*================================================================*
004456 ��ƃt�@�C���P�쐬�T SECTION.
004457*
004458*********************
004459* �S�o�� *
004460*********************
005170     MOVE �����a��v�q  TO ���Z�|�����a��.
005180     MOVE �����N�v�q    TO ���Z�|�����N.  
005190     MOVE �������v�q    TO ���Z�|������.  
013730     MOVE ZERO          TO ���Z�|���Z���.
005200     MOVE ZERO          TO ���Z�|�{�p�a��.
005210     MOVE ZERO          TO ���Z�|�{�p�N.  
005220     MOVE ZERO          TO ���Z�|�{�p��.  
005230     MOVE ZERO          TO ���Z�|���Ҕԍ�.
005240     MOVE SPACE         TO ���Z�|�}��.    
013770     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
000230                                  ���Z�|�{�p�a��N��
000240                                  ���Z�|���҃R�[�h
000250                                  ���Z�|���Z���
004472     END-START.
004473     IF ��ԃL�[ = "00"
004474         MOVE SPACE  TO �I���t���O
003720         PERFORM ���Z�v�g�e�Ǎ�
004476         PERFORM UNTIL ( �I���t���O = "YES" ) OR
005330                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
005340                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
005350                       ( ���Z�|������   NOT = �������v�q   )
003770             PERFORM �f�[�^�`�F�b�N
004492** �J�ЁE�����ӁE����E���ےP�Ɓ@����
004493             IF  ���Z�|���Z��� = 4 OR 5 OR 6 OR 7
004494                 MOVE SPACE  TO ���s�L�[�v
004495             END-IF
004496***
004497             IF ���s�L�[�v = "YES"
004480** �{�l�Ƒ��敪���肩
004481                MOVE SPACE  TO  �쐬�t���O
004482                IF �{�l�Ƒ��敪�v�q NOT = ZERO
004483                   IF �{�l�Ƒ��敪�v�q = ��|�{�l�Ƒ��敪
004484                        MOVE "YES"  TO  �쐬�t���O
004485                   END-IF
004486                ELSE
004487                   MOVE "YES"  TO  �쐬�t���O
004488                END-IF 
004489                IF �쐬�t���O  = "YES"
004498*
005610                   EVALUATE TRUE
004499*                ******************
004500*                * ���� �V�l�܂܂�*
004501*                ******************
013940                   WHEN ���Z�|���Z���   = 1
004506**                 / �������O�� /
004507                      MOVE SPACE  TO  �쐬�t���O
004508                      MOVE ��|�ی���� TO �ی���ʌ��p�v�j
004509                      PERFORM �������O�擾
004510*                  (����1�A���O2�A����0)
004511                      IF �������O�v�q NOT = ZERO
004512                         IF �������O�v�q = 1
004513                            IF �������O�`�F�b�N = 1
004514                               MOVE "YES"  TO  �쐬�t���O
004515                            END-IF
004516                         ELSE
004517                            IF �������O�`�F�b�N = ZERO
004518                               MOVE "YES"  TO  �쐬�t���O
004519                            END-IF
004520                         END-IF
004521                      ELSE
004522                         MOVE "YES"  TO  �쐬�t���O
004523                      END-IF 
004525*
004529                      IF �쐬�t���O  = "YES"
004530                         MOVE SPACE TO ��P�|���R�[�h
004531                         INITIALIZE ��P�|���R�[�h
003937                         MOVE ��|�ی����    TO ��P�|�ی����
004533                         MOVE ��|�ی��Ҕԍ�  TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
004534                         PERFORM ���R�[�h�Z�b�g
004535                         PERFORM ���ۗ����v�Z
004536                         PERFORM �t�@�C������
004537                      END-IF
004540*                ********
004541*                * �V�l *
004542*                ********
                         WHEN ���Z�|���Z���   = 2 
004545**                 / �������O�� /
004546                      MOVE SPACE  TO  �쐬�t���O
004547                      MOVE ��|������ TO �ی���ʌ��p�v�j
004548                      PERFORM �������O�擾
004549*                 (����1�A���O2�A����0)
004550                      IF �������O�v�q NOT = ZERO
004551                         IF �������O�v�q = 1
004552                            IF �������O�`�F�b�N = 1
004553                               MOVE "YES"  TO  �쐬�t���O
004554                            END-IF
004555                         ELSE
004556                            IF �������O�`�F�b�N = ZERO
004557                               MOVE "YES"  TO  �쐬�t���O
004558                            END-IF
004559                         END-IF
004560                      ELSE
004561                         MOVE "YES"  TO  �쐬�t���O
004562                      END-IF 
004563**
004564                      IF �쐬�t���O  = "YES"
004565                         MOVE SPACE TO ��P�|���R�[�h
004566                         INITIALIZE ��P�|���R�[�h
003937                         MOVE ��|������           TO ��P�|�ی����
004568                         MOVE ��|��p���S�Ҕԍ�     TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
004570                         PERFORM ���R�[�h�Z�b�g
004580                         PERFORM �V�l�����v�Z
004591* �����z�� ZERO �̎��́A�������܂Ȃ��B
004592                         IF  ��P�|�����z NOT = ZERO
004593                             PERFORM �t�@�C������
004594                         END-IF
004595                      END-IF
004610*                ********
004620*                * ���� *
004630*                ********
                         WHEN ���Z�|���Z���   = 3
004651**                 / �������O�� /
004652                      MOVE SPACE  TO  �쐬�t���O
004653                      MOVE ��|������� TO �ی���ʌ��p�v�j
004654                      PERFORM �������O�擾
004655*                  (����1�A���O2�A����0)
004656                      IF �������O�v�q NOT = ZERO
004657                         IF �������O�v�q = 1
004658                            IF �������O�`�F�b�N = 1
004659                               MOVE "YES"  TO  �쐬�t���O
004660                            END-IF
004661                         ELSE
004662                            IF �������O�`�F�b�N = ZERO
004663                               MOVE "YES"  TO  �쐬�t���O
004664                            END-IF
004665                         END-IF
004666                      ELSE
004667                         MOVE "YES"  TO  �쐬�t���O
004668                      END-IF 
004669**
004670                      IF �쐬�t���O  = "YES"
004671                         MOVE SPACE TO ��P�|���R�[�h
004672                         INITIALIZE ��P�|���R�[�h
003937                         MOVE ��|�������           TO ��P�|�ی����
004674                         MOVE ��|��p���S�Ҕԍ����� TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
004680                         PERFORM ���R�[�h�Z�b�g
004690                         PERFORM ���������v�Z
004697* �����z�� ZERO �̎��́A�������܂Ȃ��B
004698                         IF  ��P�|�����z NOT = ZERO
004699                             PERFORM �t�@�C������
004700                         END-IF
004710                      END-IF
004711                   END-EVALUATE
                      END-IF
004712             END-IF
004720             PERFORM ���Z�v�g�e�Ǎ�
004730         END-PERFORM
004740     END-IF.
004450*================================================================*
006930 ���Z�v�g�e�Ǎ� SECTION.
006940*
006950     READ ���Z�v�g�e NEXT
004490     AT END
004500         MOVE "YES" TO �I���t���O
004510     END-READ.
004811*================================================================*
004812 �f�[�^�`�F�b�N SECTION.
004813*
004814     MOVE SPACE          TO ���s�L�[�v.
005300     IF ( ���Z�|���Z����Ώۋ敪 NOT = 1 )
005310        MOVE "YES"  TO ���s�L�[�v
005320     END-IF.
019520* *****************************************************************
019530* * ���Z�v�g�e�̐����Ώۋ敪 = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
019540* *****************************************************************
005360     IF ���s�L�[�v  = "YES"
005370*      (�ēx�A���s�L�[�v SPACE)
005380        MOVE SPACE  TO ���s�L�[�v
019640        IF ���Z�|�����Ώۋ敪 NOT = ZERO
004090           MOVE ���Z�|�{�p�a��  TO ��|�{�p�a��
004100           MOVE ���Z�|�{�p�N    TO ��|�{�p�N
004110           MOVE ���Z�|�{�p��    TO ��|�{�p��
004120           MOVE ���Z�|���Ҕԍ�  TO ��|���Ҕԍ�
004130           MOVE ���Z�|�}��      TO ��|�}��
                 READ ��f�ҏ��e
                 NOT INVALID KEY
019880              MOVE "YES"  TO ���s�L�[�v
      *
                 END-READ
019900        ELSE
019910           MOVE SPACE  TO ���s�L�[�v
              END-IF
019950     END-IF.
004860*
004868*================================================================*
004869 ���R�[�h�Z�b�g SECTION.
004870*
004871     MOVE ���Z�|�����a��    TO ��P�|�����a��.
004872     MOVE ���Z�|�����N      TO ��P�|�����N.
004873     MOVE ���Z�|������      TO ��P�|������.
005480     MOVE ���Z�|���S����    TO ��P�|���S����.
005480     MOVE ���Z�|���Z������  TO ��P�|������.
004874     MOVE ��|�{�p�a��      TO ��P�|�{�p�a��.
004875     MOVE ��|�{�p�N        TO ��P�|�{�p�N.
004876     MOVE ��|�{�p��        TO ��P�|�{�p��.
004878     MOVE ��|���҃R�[�h    TO ��P�|���҃R�[�h.
004910     MOVE ��|���҃J�i      TO ��P�|���҃J�i.
004910     MOVE ��|���Ҏ���      TO ��P�|���Ҏ���.
004920     MOVE ��|��ی��Ҏ���  TO ��P�|��ی��Ҏ���.
004939     MOVE ��|�{�l�Ƒ��敪  TO ��P�|�{�l�Ƒ��敪.
005526     IF ��|������ NOT = ZERO
005534         MOVE 1             TO ��P�|�{�l�Ƒ��敪
           END-IF.
           MOVE ��|�ی����      TO ��P�|�e�ی����.
005542     PERFORM ���ގ擾.
004941*
005110*================================================================*
005120 ���ۗ����v�Z SECTION.
005130*
005210     MOVE ���Z�|���v       TO ��P�|��p�z.
005220     MOVE ���Z�|�ꕔ���S�� TO ��P�|���S�z.
005230     MOVE ���Z�|�������z   TO ��P�|�����z.
005231*
005240*================================================================*
005250 �V�l�����v�Z SECTION.
005260*
005210     MOVE ���Z�|���v       TO ��P�|��p�z.
005220     MOVE ���Z�|�ꕔ���S�� TO ��P�|���S�z.
005230     MOVE ���Z�|�������z   TO ��P�|�����z.
005361*
005370*================================================================*
005380 ���������v�Z SECTION.
005390*
005210     MOVE ���Z�|���v         TO ��P�|��p�z.
005220     MOVE ���Z�|�󋋎ҕ��S�z TO ��P�|���S�z.
005230     MOVE ���Z�|�����������z TO ��P�|�����z.
05491*
005705*================================================================*
005706 ���ގ擾 SECTION.
005707***************************************************
005708* ���ރR�[�h
005709*  1:�ЕہE����
      *  2:�D��
005710*  3:�g��
      *  4:���ρE���q��
005711*  5:1:���� 1:�ސE 2:�㍂ 3:�����i�����E���m�E���m�ȊO�j
005712*  6:�����i�����E���m�E���m�j
005716*     /15.6.18 ���s�̌������̏�Q��"39261"����������
005716*
      *  ���ŗL
      *  ���
      *  5:1:�㍂ 1:�����i�e���㍂�j 2:���� 2:�ސE 2:�����i�e�ی����㍂�ȊO�E�����E���m�E���m�ȊO�j
005718***************************************************
005719*
005720     IF ���Z�|���Z���  = 3
               IF (��|��p���S�Ҕԍ�����(3:2) = "13" OR "23" OR "39") OR
                  ((��|��p���S�Ҕԍ�����(1:5) = "39261") AND (��|������ = 5))
005726            MOVE  6  TO  ��P�|���ރR�[�h
               ELSE
005770             MOVE 5  TO  ��P�|���ރR�[�h
005743             MOVE 30 TO  ��P�|�ی���
               END-IF
005742         MOVE ��|��p���S�Ҕԍ�����(3:2) TO ��P�|���R�[�h
005728     ELSE
005729         IF ��|������ = ZERO
005730*       / ���� /
005731             EVALUATE ��|�ی����
005741             WHEN 01
005743                 MOVE 5  TO  ��P�|���ރR�[�h
005743                 MOVE 10 TO  ��P�|�ی���
005742                 MOVE ��|�ی��Ҕԍ�(1:2) TO ��P�|���R�[�h
005732             WHEN 02
005733             WHEN 06
005736                 MOVE 1  TO  ��P�|���ރR�[�h
005742                 MOVE ���v�j TO ��P�|���R�[�h
005734             WHEN 07
005736                 MOVE 2  TO  ��P�|���ރR�[�h
005742                 MOVE ���v�j TO ��P�|���R�[�h
005737             WHEN 03
005740                 MOVE 3  TO  ��P�|���ރR�[�h
005738             WHEN 04
005739             WHEN 09
005740                 MOVE 4  TO  ��P�|���ރR�[�h
005753             WHEN 08
005755                 MOVE 5  TO  ��P�|���ރR�[�h
005743                 MOVE 10 TO  ��P�|�ی���
005742                 MOVE ��|�ی��Ҕԍ�(3:2) TO ��P�|���R�[�h
005765             END-EVALUATE
005766*
005767         ELSE
005768*       / �V�l /
005770             MOVE 5  TO  ��P�|���ރR�[�h
005743             MOVE 20 TO  ��P�|�ی���
005742             MOVE ��|��p���S�Ҕԍ�(3:2) TO ��P�|���R�[�h
005778         END-IF
005779     END-IF.
           IF (���敪�v = 1) AND (��P�|���ރR�[�h = 5)
005720         IF ���Z�|���Z���  = 3
                  IF ��|��p���S�Ҕԍ�����(3:2) = "27"
005729               IF ��|������ = ZERO
005743                  MOVE 21 TO  ��P�|�ی���
                     ELSE
005743                  MOVE 11 TO  ��P�|�ی���
                     END-IF
010202               IF ��|���ی�����(1:3) = "274"
010203                   EVALUATE ��|�@�ʔԍ�����
010204                   WHEN 41
010205                       MOVE 41274002           TO ��P�|�ی��Ҕԍ�
010206                   WHEN 80
010207                       MOVE 80274004           TO ��P�|�ی��Ҕԍ�
010208                   WHEN 82
010209                       MOVE 82274002           TO ��P�|�ی��Ҕԍ�
010210                   WHEN 83
010211                       MOVE 83274001           TO ��P�|�ی��Ҕԍ�
010212                   WHEN 86
010213                       MOVE 86274008           TO ��P�|�ی��Ҕԍ�
                         WHEN 87
009990                       MOVE "87274007"         TO ��P�|�ی��Ҕԍ�
010214                   WHEN OTHER
010215                       MOVE ��|��p���S�Ҕԍ����� TO ��P�|�ی��Ҕԍ�
010216                   END-EVALUATE
010217               ELSE
010218                   IF ( ��|��p���S�Ҕԍ�����(3:3) = "275" ) OR ( ��|��p���S�Ҕԍ�����(3:5) = "27002" )
010219                       EVALUATE ��|�@�ʔԍ�����
010220                       WHEN 41
010221                           MOVE 41275009           TO ��P�|�ی��Ҕԍ�
010222                       WHEN 80
010223                           MOVE 80275001           TO ��P�|�ی��Ҕԍ�
010224                       WHEN 82
010225                           MOVE 82275009           TO ��P�|�ی��Ҕԍ�
010226                       WHEN 86
010227                           MOVE 86275005           TO ��P�|�ی��Ҕԍ�
010228                       WHEN OTHER
010229                           MOVE ��|��p���S�Ҕԍ����� TO ��P�|�ی��Ҕԍ�
010230                       END-EVALUATE
010231                   ELSE
010232                       MOVE ��|��p���S�Ҕԍ�����  TO ��P�|�ی��Ҕԍ�
010233                   END-IF
                     END-IF
010234            END-IF
005728         ELSE
005729            IF ��|������ = ZERO
005730*       / ���� /
005731               EVALUATE ��|�ی����
005741               WHEN 01
                         IF ��|�ی��Ҕԍ�(1:2) = "27"
005743                      MOVE 20 TO  ��P�|�ی���
009114                      IF ��|�ی��Ҕԍ�(1:3) = "274"
009115                          MOVE 00274001       TO ��P�|�ی��Ҕԍ�
009116                      ELSE
009117                          IF ( ��|�ی��Ҕԍ�(1:3) = "275" ) OR ( ��|�ی��Ҕԍ�(1:5) = "27002" )
009118                             MOVE 00275008       TO ��P�|�ی��Ҕԍ�
009119                          ELSE
009120                             MOVE 00             TO �@�ʔԍ��v�q
009121                             MOVE ��|�ی��Ҕԍ� TO �۔Ԃv�q
009121                             MOVE �ی��Ҕԍ��v�q TO ��P�|�ی��Ҕԍ�
009122                          END-IF
                            END-IF
009123                   END-IF
005753               WHEN 08
                         IF ��|�ی��Ҕԍ�(3:2) = "27"
005743                      MOVE 20 TO  ��P�|�ی���
009125                      IF ��|�۔�(1:3) = "274"
009126                         MOVE 67274001       TO ��P�|�ی��Ҕԍ�
009127                      ELSE
009128                         IF ( ��|�ی��Ҕԍ�(3:3) = "275" ) OR ( ��|�ی��Ҕԍ�(3:5) = "27002" )
009129                            MOVE 67275008       TO ��P�|�ی��Ҕԍ�
009130                         ELSE
009131                            MOVE ��|�ی��Ҕԍ� TO ��P�|�ی��Ҕԍ�
009132                         END-IF
009133                      END-IF
                         END-IF
005765               END-EVALUATE
005767            ELSE
005768*       / �V�l /
                     IF ��|��p���S�Ҕԍ�(3:2) = "27"
005743                  MOVE 10 TO  ��P�|�ی���
009672                  IF ��|���ی��V�l(1:3) = "274"
009673                     MOVE 27274000          TO ��P�|�ی��Ҕԍ�
009674                  ELSE
009675                     IF ( ��|��p���S�Ҕԍ�(3:3) = "275" ) OR ( ��|��p���S�Ҕԍ�(3:5) = "27002" )
009676                        MOVE 27275007           TO ��P�|�ی��Ҕԍ�
009677                     ELSE
009678                        MOVE ��|��p���S�Ҕԍ� TO ��P�|�ی��Ҕԍ�
009679                     END-IF
009680                  END-IF
      */�������͑�\�ԍ��ɂ܂Ƃ߂�/080419
                        IF ��|�ی��Ҕԍ�(1:4) = "3927"
                           MOVE "39270004" TO ��P�|�ی��Ҕԍ�
                        END-IF
005778               END-IF
                  END-IF
005778         END-IF
005779     END-IF.
005780*
005500*================================================================*
005510 �t�@�C������ SECTION.
005520*
005530     WRITE ��P�|���R�[�h
005540     INVALID KEY
005550         MOVE NC"���"  TO �t�@�C����
005560         PERFORM �G���[�\��
005570     END-WRITE.
005580*================================================================*
005590 ��ƃt�@�C���Q�쐬 SECTION.
005600*
005610     OPEN INPUT  ��ƃt�@�C���P.
005620     OPEN OUTPUT ��ƃt�@�C���Q.
005620     OPEN OUTPUT ��ƃt�@�C����.
005630*
005640     PERFORM ��ƃt�@�C���Q�쐬����.
005650*
005660     CLOSE ��ƃt�@�C����.
005660     CLOSE ��ƃt�@�C���Q.
005670     CLOSE ��ƃt�@�C���P.
005680*================================================================*
005690 ��ƃt�@�C���Q�쐬���� SECTION.
005700*
005813     MOVE SPACE TO �I���t���O�Q.
005940*
005950     PERFORM ��ƃt�@�C���P�Ǎ�.
006020     PERFORM UNTIL �I���t���O�Q = "YES"
006120         MOVE ��P�|�ی��Ҕԍ�   TO �ی��Ҕԍ��v
006030*
006040         MOVE SPACE TO ��Q�|���R�[�h
006050         INITIALIZE ��Q�|���R�[�h
               INITIALIZE �W�v��Ɨp�v
               PERFORM UNTIL (�I���t���O�Q = "YES") OR
                             (��P�|�ی��Ҕԍ� NOT = �ی��Ҕԍ��v)
      */150620 ���s�̌��ŗL���w�肵�����A�㍂�̏�Q�͂V�������o�����߁A�e�ی����㍂�ł͂Ȃ����̂͏W�v���Ȃ�
                   IF (���s�敪�v = 1) AND
                      ((��P�|�ی���� = 53) AND (��Q�|�ی��Ҕԍ�(1:5) = 39261)) AND
                      (��P�|�e�ی���� NOT = 05)
                      CONTINUE
                   ELSE
006060                PERFORM ��ƃ��R�[�h�Q�݌v
006070*
006120                MOVE ��P�|�����a��N�� TO ��Q�|�����a��N��
006120                MOVE ��P�|���ރR�[�h   TO ��Q�|���ރR�[�h
006120                MOVE ��P�|���R�[�h     TO ��Q�|���R�[�h
006120                MOVE ��P�|�ی���       TO ��Q�|�ی���
006120                MOVE ��P�|�ی����     TO ��Q�|�ی����
006120                MOVE ��P�|�ی��Ҕԍ�   TO ��Q�|�ی��Ҕԍ� �ی��Ҕԍ��v
                      IF ��P�|���ރR�[�h = 4
                         PERFORM ��ƃt�@�C�����쐬
                      END-IF
                   END-IF
006140             PERFORM ��ƃt�@�C���P�Ǎ�
006150         END-PERFORM
006160         PERFORM ��ƃ��R�[�h�Q�Z�b�g
006170         PERFORM ��ƃt�@�C���Q����
006180     END-PERFORM.
006190*================================================================*
006200 ��ƃt�@�C���P�Ǎ� SECTION.
006210*
006220     READ ��ƃt�@�C���P NEXT
006230     AT END
006240         MOVE "YES" TO �I���t���O�Q
006250     END-READ.
006260*================================================================*
006270 ��ƃ��R�[�h�Q�݌v SECTION.
006280*
006490     COMPUTE �����v   = �����v   + 1.
006510     COMPUTE ��p�z�v = ��p�z�v + ��P�|��p�z.
006520     COMPUTE ���S�z�v = ���S�z�v + ��P�|���S�z.
006530     COMPUTE �����z�v = �����z�v + ��P�|�����z.
006531*
006532     IF ��P�|�{�l�Ƒ��敪 = 1
006534        COMPUTE �{�l�����v   = �{�l�����v   + 1
006535        COMPUTE �{�l��p�z�v = �{�l��p�z�v + ��P�|��p�z
006536        COMPUTE �{�l���S�z�v = �{�l���S�z�v + ��P�|���S�z
006537        COMPUTE �{�l�����z�v = �{�l�����z�v + ��P�|�����z
006530        COMPUTE �{�l�����v   = �{�l�����v   + ��P�|������
006538     ELSE
006539        COMPUTE �Ƒ������v   = �Ƒ������v   + 1
006540        COMPUTE �Ƒ���p�z�v = �Ƒ���p�z�v + ��P�|��p�z
006541        COMPUTE �Ƒ����S�z�v = �Ƒ����S�z�v + ��P�|���S�z
006542        COMPUTE �Ƒ������z�v = �Ƒ������z�v + ��P�|�����z
006530        COMPUTE �Ƒ������v   = �Ƒ������v   + ��P�|������
006543     END-IF.
006544*
006547*===============================================================*
006550 ��ƃ��R�[�h�Q�Z�b�g SECTION.
006560*
006630     MOVE �����v         TO ��Q�|����.
006650     MOVE ��p�z�v       TO ��Q�|��p�z.
006660     MOVE ���S�z�v       TO ��Q�|���S�z.
006670     MOVE �����z�v       TO ��Q�|�����z.
006671*
006672     MOVE �{�l�����v     TO ��Q�|�{�l����.
006673     MOVE �{�l��p�z�v   TO ��Q�|�{�l��p�z.
006674     MOVE �{�l���S�z�v   TO ��Q�|�{�l���S�z.
006675     MOVE �{�l�����z�v   TO ��Q�|�{�l�����z.
006675     MOVE �{�l�����v     TO ��Q�|�{�l������.
006676     MOVE �Ƒ������v     TO ��Q�|�Ƒ�����.
006677     MOVE �Ƒ���p�z�v   TO ��Q�|�Ƒ���p�z.
006678     MOVE �Ƒ����S�z�v   TO ��Q�|�Ƒ����S�z.
006679     MOVE �Ƒ������z�v   TO ��Q�|�Ƒ������z.
006679     MOVE �Ƒ������v     TO ��Q�|�Ƒ�������.
006680*
006687*===============================================================*
006690 ��ƃt�@�C���Q���� SECTION.
006700*
006710     WRITE ��Q�|���R�[�h
006720     INVALID KEY
006730**         DISPLAY NC"��ԃL�[" ��ԃL�[  UPON MSGBOX
006740         MOVE NC"��Q" TO �t�@�C����
006750         PERFORM �G���[�\��
006760         PERFORM �t�@�C����
006770         MOVE 99 TO PROGRAM-STATUS
006780         EXIT PROGRAM.
006547*===============================================================*
006550 ��ƃt�@�C�����쐬 SECTION.
006560*
006120     MOVE ��P�|�����a��N�� TO �실�|�����a��N��.
006120     MOVE ��P�|���ރR�[�h   TO �실�|���ރR�[�h.
006120     MOVE ��P�|���R�[�h     TO �실�|���R�[�h.
006120     MOVE ��P�|�ی���       TO �실�|�ی���.
006120     MOVE ��P�|�ی��Ҕԍ�   TO �실�|�ی��Ҕԍ�.
006120     MOVE ��P�|���҃R�[�h   TO �실�|���҃R�[�h.
006120     MOVE ��P�|�{�p�a��N�� TO �실�|�{�p�a��N��.
006120     MOVE ��P�|�ی����     TO �실�|�ی����.
006120     MOVE ��P�|�{�l�Ƒ��敪 TO �실�|�{�l�Ƒ��敪.
006120     MOVE ��P�|���Ҏ���     TO �실�|���Ҏ���.
006670     MOVE ��P�|�����z       TO �실�|�����z.
006679     MOVE ��P�|������       TO �실�|������.
006170     PERFORM ��ƃt�@�C��������.
006680*
006687*===============================================================*
006690 ��ƃt�@�C�������� SECTION.
006700*
006710     WRITE �실�|���R�[�h
006720     INVALID KEY
006730**         DISPLAY NC"��ԃL�[" ��ԃL�[  UPON MSGBOX
006740         MOVE NC"�실" TO �t�@�C����
006750         PERFORM �G���[�\��
006760         PERFORM �t�@�C����
006770         MOVE 99 TO PROGRAM-STATUS
006780         EXIT PROGRAM.
006790*================================================================*
006800 �t�@�C���� SECTION.
006810*
006820     CLOSE ������}�X�^ ��f�ҏ��e
006821           ������}�X�^ �s�����}�X�^ ���Z�v�g�e.
006830*================================================================*
006840 �I������ SECTION.
006850*
006860     PERFORM �t�@�C����.
006861*================================================================*
006862*================================================================*
006863 �������O�擾 SECTION.
006864*
006865      MOVE ZERO  TO  �������O�`�F�b�N ���v�j.
006866      EVALUATE  �ی���ʌ��p�v�j
006867* ����
006868      WHEN  01
006869         IF ��|�ی��Ҕԍ�(1:2) = ���i�h�r�v�j
006870            MOVE  1  TO  �������O�`�F�b�N
006871         END-IF
006872         MOVE ��|�ی��Ҕԍ�(1:2) TO ���v�j
006873* �Е�
006874      WHEN  02
      */����ۂ̑Ή�/081009
               IF ��|�ی��Ҕԍ�(5:5) = SPACE
006875             IF ��|�ی��Ҕԍ�(1:2) = ���ǂi�h�r�v�j
006876                MOVE  1  TO  �������O�`�F�b�N
006877             END-IF
006878             MOVE ��|�ی��Ҕԍ�(1:2) TO ���v�j
006879             PERFORM ������
006880             MOVE ������v�j TO ���v�j
               ELSE
                   IF ��|�ی��Ҕԍ�(3:2) = ���i�h�r�v�j
                       MOVE  1  TO  �������O�`�F�b�N
                   END-IF
                   MOVE ��|�ی��Ҕԍ�(3:2) TO ���v�j
               END-IF
006881* �D��
006882* ����
006883* �g��
006884* ����
006885* ���q��
006886* �ސE����
006887      WHEN  06
006888      WHEN  07
006889      WHEN  08
006890      WHEN  09
006891      WHEN  04
006892      WHEN  03
006893         IF ��|�ی��Ҕԍ�(3:2) = ���i�h�r�v�j
006894            MOVE  1  TO  �������O�`�F�b�N
006895         END-IF
006896         MOVE ��|�ی��Ҕԍ�(3:2) TO ���v�j
006897* ����
006898      WHEN 50 THRU 60
006899         IF ��|��p���S�Ҕԍ�����(3:2) = ���i�h�r�v�j
006900            MOVE  1  TO  �������O�`�F�b�N
006901         END-IF
006902         MOVE ��|��p���S�Ҕԍ�����(3:2) TO ���v�j
006903* �V�l
006904      WHEN  05
006905         IF ��|��p���S�Ҕԍ�(3:2) = ���i�h�r�v�j
006906            MOVE  1  TO  �������O�`�F�b�N
006907         END-IF
006908         MOVE ��|��p���S�Ҕԍ�(3:2) TO ���v�j
006910* �J�ЁA�����ӁE����E���ےP�Ƃ͏��O
006911      WHEN  70
006912      WHEN  80
006912      WHEN  85
006913      WHEN  90
006914      WHEN  ZERO
006915         CONTINUE
006916*
006917     WHEN OTHER
006918         MOVE SPACE                TO �G���[���b�Z�[�W�v
006919         MOVE ��|���҃R�[�h       TO �G���[���҃R�[�h�v
006920         MOVE �ی���ʌ��p�v�j     TO �G���[�ی���ʂv
006921         MOVE "-"                  TO �G���[��؂�v
006922         MOVE  NC"�ی���ʂ��ُ�ł��B���ҁ|�ی����" TO �A���R�|���b�Z�[�W
006923         MOVE  �G���[���b�Z�[�W�v  TO �A���R�|���b�Z�[�W�P
006924         CALL   "MSG003"
006925         CANCEL "MSG003"
006926     END-EVALUATE.
006927*
006928*================================================================*
006929 ������ SECTION.
006930*
006931     MOVE ZERO  TO  ������v�j.
006932     EVALUATE  ���v�j
006933*�k�C��
006934      WHEN  01
006935         MOVE 01 TO ������v�j
006936*�X
006937      WHEN  02
006938         MOVE 02 TO ������v�j
006939*���
006940      WHEN  03
006941         MOVE 03 TO ������v�j
006942*�{��
006943      WHEN  04
006944         MOVE 04 TO ������v�j
006945*�H�c
006946      WHEN  05
006947         MOVE 05 TO ������v�j
006948*�R�`
006949      WHEN  06
006950         MOVE 06 TO ������v�j
006951*����
006952      WHEN  07
006953         MOVE 07 TO ������v�j
006954*���
006955      WHEN  08
006956         MOVE 08 TO ������v�j
006957*�Ȗ�
006958      WHEN  09
006959         MOVE 09 TO ������v�j
006960*�Q�n
006961      WHEN  10
006962         MOVE 10 TO ������v�j
006963*���
006964      WHEN  11
006965         MOVE 11 TO ������v�j
006966*��t
006967      WHEN  12
006968         MOVE 12 TO ������v�j
006969*����
006970      WHEN  21
006971         MOVE 13 TO ������v�j
006972*�_�ސ�
006973      WHEN  31
006974         MOVE 14 TO ������v�j
006975*�V��
006976      WHEN  32
006977         MOVE 15 TO ������v�j
006978*�x�R
006979      WHEN  33
006980         MOVE 16 TO ������v�j
006981*�ΐ�
006982      WHEN  34
006983         MOVE 17 TO ������v�j
006984*����
006985      WHEN  35
006986         MOVE 18 TO ������v�j
006987*�R��
006988      WHEN  36
006989         MOVE 19 TO ������v�j
006990*����
006991      WHEN  37
006992         MOVE 20 TO ������v�j
006993*��
006994      WHEN  38
006995         MOVE 21 TO ������v�j
006996*�É�
006997      WHEN  39
006998         MOVE 22 TO ������v�j
006999*���m
007000      WHEN  51
007001         MOVE 23 TO ������v�j
007002*�O�d
007003      WHEN  52
007004         MOVE 24 TO ������v�j
007005*����
007006      WHEN  53
007007         MOVE 25 TO ������v�j
007008*���s
007009      WHEN  54
007010         MOVE 26 TO ������v�j
007011*���
007012      WHEN  41
007013         MOVE 27 TO ������v�j
007014*����
007015      WHEN  42
007016         MOVE 28 TO ������v�j
007017*�ޗ�
007018      WHEN  55
007019         MOVE 29 TO ������v�j
007020*�a�̎R
007021      WHEN  56
007022         MOVE 30 TO ������v�j
007023*����
007024      WHEN  57
007025         MOVE 31 TO ������v�j
007026*����
007027      WHEN  58
007028         MOVE 32 TO ������v�j
007029*���R
007030      WHEN  59
007031         MOVE 33 TO ������v�j
007032*�L��
007033      WHEN  60
007034         MOVE 34 TO ������v�j
007035*�R��
007036      WHEN  61
007037         MOVE 35 TO ������v�j
007038*����
007039      WHEN  71
007040         MOVE 36 TO ������v�j
007041*����
007042      WHEN  72
007043         MOVE 37 TO ������v�j
007044*���Q
007045      WHEN  73
007046         MOVE 38 TO ������v�j
007047*���m
007048      WHEN  74
007049         MOVE 39 TO ������v�j
007050*����
007051      WHEN  75
007052         MOVE 40 TO ������v�j
007053*����
007054      WHEN  76
007055         MOVE 41 TO ������v�j
007056*����
007057      WHEN  77
007058         MOVE 42 TO ������v�j
007059*�F�{
007060      WHEN  78
007061         MOVE 43 TO ������v�j
007062*�啪
007063      WHEN  79
007064         MOVE 44 TO ������v�j
007065*�{��
007066      WHEN  80
007067         MOVE 45 TO ������v�j
007068*������
007069      WHEN  81
007070         MOVE 46 TO ������v�j
007071*����
007072      WHEN  82
007073         MOVE 47 TO ������v�j
007074*���̑�
007075      WHEN  OTHER
007076         CONTINUE
007077     END-EVALUATE.
007078*
007079*================================================================*
007231******************************************************************
007232 END PROGRAM YHN431.
007233******************************************************************
