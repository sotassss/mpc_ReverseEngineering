000001******************************************************************
000002*            IDENTIFICATION      DIVISION                        *
000003******************************************************************
000004 IDENTIFICATION          DIVISION.
000005 PROGRAM-ID.             YHN432.
000006 AUTHOR.                 �r�c�@�K�q
000007*
000008*----------------------------------------------------------------*
000009*         �������w��y�ް��쐬�z�_�{����޳�ޔ�
000010*  �����N���o�[�W���� 
000011*         MED = YHN430 
000012*----------------------------------------------------------------*
000013 DATE-WRITTEN.           2015-02-25
000014 DATE-COMPILED.          2015-02-25
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
000108     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4322L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  ��Q�|�����a��N��
000112                                                          ��Q�|���ރR�[�h
000112                                                          ��Q�|���R�[�h
000112                                                          ��Q�|�ی���
000113                                                          ��Q�|�ی��Ҕԍ�
000114                             ALTERNATE RECORD KEY     IS  ��Q�|����敪
000115                                                          ��Q�|�����a��N��
000112                                                          ��Q�|���ރR�[�h
000112                                                          ��Q�|���R�[�h
000112                                                          ��Q�|�ی���
000117                                                          ��Q�|�ی��Ҕԍ�
000118                             FILE        STATUS       IS  ��ԃL�[
000119                             LOCK        MODE         IS  AUTOMATIC.
000120******************************************************************
000121*                      DATA DIVISION                             *
000122******************************************************************
000123 DATA                    DIVISION.
000124 FILE                    SECTION.
000125*                           �m�q�k��  �Q�T�U�n
000126 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
000127     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000991     COPY SEIGYO02        OF  XFDLIB  JOINING   ���O�Q   AS  PREFIX.
000128*                           �m�q�k��  �R�Q�O�n
000129 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
000130     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
000143* 
000144 FD  ��ƃt�@�C���P RECORD  CONTAINS 256 CHARACTERS.
000145 01  ��P�|���R�[�h.
000146     03  ��P�|���R�[�h�L�[.
000147         05  ��P�|�����a��N��.
000148             07  ��P�|�����a��              PIC 9.
000149             07  ��P�|�����N                PIC 9(2).
000150             07  ��P�|������                PIC 9(2).
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
000172*
000173*                           �m�q�k��  �U�S�n
000174 FD  ��ƃt�@�C���Q RECORD  CONTAINS 64 CHARACTERS.
000175 01  ��Q�|���R�[�h.
000176     03  ��Q�|���R�[�h�L�[.
000177         05  ��Q�|�����a��N��.
000178             07  ��Q�|�����a��              PIC 9.
000179             07  ��Q�|�����N                PIC 9(2).
000180             07  ��Q�|������                PIC 9(2).
001261         05  ��Q�|���ރR�[�h                PIC 9(1).
001261         05  ��Q�|���R�[�h                  PIC X(2).
001400         05  ��Q�|�ی���                    PIC 9(2).
000182         05  ��Q�|�ی��Ҕԍ�                PIC X(10).
000183         05  ��Q�|����敪                  PIC 9.
000184     03  ��Q�|���R�[�h�f�[�^.
001400         05  ��Q�|�ی����                  PIC 9(2).
000185         05  ��Q�|����                      PIC 9(4).
000186         05  ��Q�|��p�z                    PIC 9(8).
000187         05  ��Q�|���S�z                    PIC 9(8).
000188         05  ��Q�|�����z                    PIC 9(8).
               05  ��Q�|���ی��Ҕԍ�              PIC X(10).
               05  ��Q�|���x���`�F�b�N            PIC X.
               05  FILLER                          PIC X(2).
000173*
000190*----------------------------------------------------------------*
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
001651 01 ����t���O                         PIC X(3)  VALUE SPACE.
001305*
001310 01 �ی���ʂv�q                       PIC 9(2)  VALUE ZERO.
001311 01 ���Z�v�g��ނv�q                   PIC X(4)  VALUE SPACE.
001321 01 �{�l�Ƒ��敪�v�q                   PIC 9     VALUE ZERO.
001330 01 �����a��N���v�q.
001340    03 �����a��v�q                    PIC 9     VALUE ZERO.
001350    03 �����N�v�q                      PIC 9(2)  VALUE ZERO.
001360    03 �������v�q                      PIC 9(2)  VALUE ZERO.
001320 01 �ی��Ҕԍ��v�q.
001530    03 �@�ʔԍ��v�q                    PIC X(2)  VALUE SPACE.
001540    03 �۔Ԃv�q.
001550       05 �s���{���ԍ��v�q             PIC X(2)  VALUE SPACE.
001560       05 �ی��ԍ��v�q                 PIC X(3)  VALUE SPACE.
001570       05 ���ؔԍ��v�q                 PIC X     VALUE SPACE.
001580       05 FILLER                       PIC X(2)  VALUE SPACE.
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
001510 01 �{�l�Ƒ��敪�v                     PIC 9(1)  VALUE ZERO.
001520 01 �t�@�C����                         PIC N(2)  VALUE SPACE.
001530 01 �ی���ʂv                         PIC 9(2)  VALUE ZERO.
001540 01 �ی��Ҕԍ��v                       PIC X(10) VALUE SPACE.
001541 01 �e�ی���ʂv                       PIC 9(2)  VALUE ZERO.
001542*
001543* �������O�p
001544 01 �������O�v�q                       PIC 9     VALUE ZERO.
001545 01 ���i�h�r�v�j                       PIC X(2)  VALUE SPACE.
001546 01 ���ǂi�h�r�v�j                     PIC X(2)  VALUE SPACE.
001547 01 �������O�`�F�b�N                   PIC 9     VALUE ZERO.
001548 01 ���v�j                             PIC X(2)  VALUE SPACE.
001549 01 ���v�j�Q                           PIC X(2)  VALUE SPACE.
001550 01 ������v�j                         PIC X(2)  VALUE SPACE.
001551 01 �ی���ʌ��p�v�j                   PIC 9(2)  VALUE ZERO.
001552*
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
001553* �G���[���b�Z�[�W�p
001554 01 �G���[���b�Z�[�W�v.
001555    03 �G���[���҃R�[�h�v              PIC X(7) VALUE SPACE.
001556    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
001557    03 �G���[�ی���ʂv                PIC X(2) VALUE SPACE.
001558    03 FILLER                          PIC X(10) VALUE SPACE.
001559* �����p
001560 01 ����ی���ʂv                     PIC 9(2)  VALUE ZERO.
001561 01 ��������v                         PIC 9(3)  VALUE ZERO.
001562*
001563** �������Z�܂Ƃߗp
001564 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
001565*
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
001570******************************************************************
001571*                          �A������                              *
001572******************************************************************
001580*
001581****************
001582* ��ʓ��͏�� *
001583****************
001584**
001585 01 �A���|��ʏ��x�g�m�S�R�O   IS EXTERNAL.
001586    03 �A���|�����N��.
001587       05 �A���|�����a��               PIC 9.
001588       05 �A���|�����N                 PIC 9(2).
001589       05 �A���|������                 PIC 9(2).
001590    03 �A���|��o�N����.
001591       05 �A���|��o�a��               PIC 9.
001592       05 �A���|��o�N                 PIC 9(2).
001593       05 �A���|��o��                 PIC 9(2).
001594       05 �A���|��o��                 PIC 9(2).
001595    03 �A���|���Z�v�g���              PIC X(4).
001596    03 �A���|�ی����                  PIC 9(2).
001597    03 �A���|������                  PIC 9.
001598    03 �A���|�{�l�Ƒ�                  PIC 9.
001599    03 �A���|�p�����                  PIC 9.
001600    03 �A���|�������O                  PIC 9.
001601    03 �A���|���i�h�r                  PIC X(2).
001602    03 �A���|���ǂi�h�r                PIC X(2).
001603*
001772*
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
002260     PERFORM �A�����ڑҔ�.
002261     PERFORM ������擾.
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
002739*
002740*================================================================*
002750 �G���[�\�� SECTION.
002760*
002770     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
002780     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
002790     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
002800     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
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
002903*        **********************************
002904*        * ���ہE�ЕہE�ސE�E�g���E���ρE���q��
002905*        **********************************
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
003720**********************************
003740* ���ہE�ސE�E�g���E���ρE���q��
003750**********************************
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
003939*                    (����1�A���O2�A����0)
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
003958*
003959                   IF �쐬�t���O  = "YES"
                            PERFORM ����Ώۃ`�F�b�N
                            IF ����t���O = "YES"
003964                         MOVE SPACE TO ��P�|���R�[�h
003965                         INITIALIZE ��P�|���R�[�h
003937                         MOVE ��|�ی����    TO ��P�|�ی����
003967                         MOVE ��|�ی��Ҕԍ�  TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
003968                         PERFORM ���R�[�h�Z�b�g
003970                         PERFORM �t�@�C������
                            END-IF
003981                   END-IF 
003991                END-IF 
004161             END-IF
004170             PERFORM ���Z�v�g�e�Ǎ�
004180         END-PERFORM
004190     END-IF.
004291*================================================================*
004292 ��ƃt�@�C���P�쐬�R SECTION.
004293*
004294**********************************
004295*  �V�l
004296**********************************
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
004319** �{�l�Ƒ��敪���肩
004320                MOVE SPACE  TO  �쐬�t���O
004321                IF �{�l�Ƒ��敪�v�q NOT = ZERO
004322                   IF �{�l�Ƒ��敪�v�q = ��|�{�l�Ƒ��敪
004323                      MOVE "YES"  TO  �쐬�t���O
004324                   END-IF
004325                ELSE
004326                   MOVE "YES"  TO  �쐬�t���O
004327                END-IF 
004328** �������O��
004329                IF �쐬�t���O  = "YES"
004330                   MOVE SPACE  TO  �쐬�t���O
004331                   MOVE ��|������ TO �ی���ʌ��p�v�j
004332                   PERFORM �������O�擾
004333*                 (����1�A���O2�A����0)
004334                   IF �������O�v�q NOT = ZERO
004335                      IF �������O�v�q = 1
004336                         IF �������O�`�F�b�N = 1
004337                            MOVE "YES"  TO  �쐬�t���O
004338                         END-IF
004339                      ELSE
004340                         IF �������O�`�F�b�N = ZERO
004341                            MOVE "YES"  TO  �쐬�t���O
004342                         END-IF
004343                      END-IF
004344                   ELSE
004345                      MOVE "YES"  TO  �쐬�t���O
004346                   END-IF 
004347                END-IF 
004348*
004349                IF �쐬�t���O  = "YES"
                         PERFORM ����Ώۃ`�F�b�N
                         IF ����t���O = "YES"
004352                      MOVE SPACE TO ��P�|���R�[�h
004353                      INITIALIZE ��P�|���R�[�h
003937                      MOVE ��|������           TO ��P�|�ی����
004355                      MOVE ��|��p���S�Ҕԍ�     TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
004356                      PERFORM ���R�[�h�Z�b�g
004357                      PERFORM �V�l�����v�Z
004358* �����z�� ZERO �̎��́A�������܂Ȃ��B
004359                      IF  ��P�|�����z NOT = ZERO
004360                          PERFORM �t�@�C������
004361                      END-IF
                         END-IF
004363                END-IF 
004364             END-IF
004365             PERFORM ���Z�v�g�e�Ǎ�
004366         END-PERFORM
004367     END-IF.
004368*================================================================*
004369 ��ƃt�@�C���P�쐬�S SECTION.
004370*
004371**********************************
004372*  ����
004373**********************************
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
004396** �{�l�Ƒ��敪���肩
004397                MOVE SPACE  TO  �쐬�t���O
004398                IF �{�l�Ƒ��敪�v�q NOT = ZERO
004399                   IF �{�l�Ƒ��敪�v�q = ��|�{�l�Ƒ��敪
004400                      MOVE "YES"  TO  �쐬�t���O
004401                   END-IF
004402                ELSE
004403                   MOVE "YES"  TO  �쐬�t���O
004404                END-IF 
004405** �������O��
004406                IF �쐬�t���O  = "YES"
004407                   MOVE SPACE  TO  �쐬�t���O
004408                   MOVE ��|������� TO �ی���ʌ��p�v�j
004409                   PERFORM �������O�擾
004410*                 (����1�A���O2�A����0)
004411                   IF �������O�v�q NOT = ZERO
004412                      IF �������O�v�q = 1
004413                         IF �������O�`�F�b�N = 1
004414                            MOVE "YES"  TO  �쐬�t���O
004415                         END-IF
004416                      ELSE
004417                         IF �������O�`�F�b�N = ZERO
004418                            MOVE "YES"  TO  �쐬�t���O
004419                         END-IF
004420                      END-IF
004421                   ELSE
004422                      MOVE "YES"  TO  �쐬�t���O
004423                   END-IF 
004424                END-IF 
004425*
004426                IF �쐬�t���O  = "YES"
                         PERFORM ����Ώۃ`�F�b�N
                         IF ����t���O = "YES"
004434                      MOVE SPACE TO ��P�|���R�[�h
004435                      INITIALIZE ��P�|���R�[�h
003937                      MOVE ��|�������           TO ��P�|�ی����
004437                      MOVE ��|��p���S�Ҕԍ����� TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
004438                      PERFORM ���R�[�h�Z�b�g
004439                      PERFORM ���������v�Z
004446* �����z�� ZERO �̎��́A�������܂Ȃ��B
004447                      IF  ��P�|�����z NOT = ZERO
004448                         PERFORM �t�@�C������
                            END-IF
004449                   END-IF
004451                END-IF
004452             END-IF
004453             PERFORM ���Z�v�g�e�Ǎ�
004454         END-PERFORM
004455     END-IF.
004456*================================================================*
004457 ��ƃt�@�C���P�쐬�T SECTION.
004458*
004459*********************
004460* �S�o�� *
004461*********************
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
004497             IF ���s�L�[�v = "YES"
004481** �{�l�Ƒ��敪���肩
004482                MOVE SPACE  TO  �쐬�t���O
004483                IF �{�l�Ƒ��敪�v�q NOT = ZERO
004484                   IF �{�l�Ƒ��敪�v�q = ��|�{�l�Ƒ��敪
004485                      MOVE "YES"  TO  �쐬�t���O
004486                   END-IF
004487                ELSE
004488                   MOVE "YES"  TO  �쐬�t���O
004489                END-IF 
004490                IF �쐬�t���O  = "YES"
004500*
005610                   EVALUATE TRUE
004501*                ******************
004502*                * ���� �V�l�܂܂�*
004503*                ******************
013940                   WHEN ���Z�|���Z���   = 1
004508**                 / �������O�� /
004509                       MOVE SPACE  TO  �쐬�t���O
004510                       MOVE ��|�ی���� TO �ی���ʌ��p�v�j
004511                       PERFORM �������O�擾
004512*                     (����1�A���O2�A����0)
004513                       IF �������O�v�q NOT = ZERO
004514                          IF �������O�v�q = 1
004515                             IF �������O�`�F�b�N = 1
004516                                MOVE "YES"  TO  �쐬�t���O
004517                             END-IF
004518                          ELSE
004519                             IF �������O�`�F�b�N = ZERO
004520                                MOVE "YES"  TO  �쐬�t���O
004521                             END-IF
004522                          END-IF
004523                       ELSE
004524                          MOVE "YES"  TO  �쐬�t���O
004525                       END-IF 
004530**
004531                       IF �쐬�t���O  = "YES"
                                PERFORM ����Ώۃ`�F�b�N
                                IF ����t���O = "YES"
004532                             MOVE SPACE TO ��P�|���R�[�h
004533                             INITIALIZE ��P�|���R�[�h
003937                             MOVE ��|�ی����    TO ��P�|�ی����
004535                             MOVE ��|�ی��Ҕԍ�  TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
004536                             PERFORM ���R�[�h�Z�b�g
004538                             PERFORM �t�@�C������
                                END-IF
004539                       END-IF
004542*                ********
004543*                * �V�l *
004544*                ********
                         WHEN ���Z�|���Z���   = 2 
004547**                 / �������O�� /
004548                       MOVE SPACE  TO  �쐬�t���O
004549                       MOVE ��|������ TO �ی���ʌ��p�v�j
004550                       PERFORM �������O�擾
004551*                     (����1�A���O2�A����0)
004552                       IF �������O�v�q NOT = ZERO
004553                          IF �������O�v�q = 1
004554                             IF �������O�`�F�b�N = 1
004555                                MOVE "YES"  TO  �쐬�t���O
004556                             END-IF
004557                          ELSE
004558                             IF �������O�`�F�b�N = ZERO
004559                                MOVE "YES"  TO  �쐬�t���O
004560                             END-IF
004561                          END-IF
004562                       ELSE
004563                          MOVE "YES"  TO  �쐬�t���O
004564                       END-IF 
004565**
004566                       IF �쐬�t���O  = "YES"
                                PERFORM ����Ώۃ`�F�b�N
                                IF ����t���O = "YES"
004567                             MOVE SPACE TO ��P�|���R�[�h
004568                             INITIALIZE ��P�|���R�[�h
003937                             MOVE ��|������           TO ��P�|�ی����
004570                             MOVE ��|��p���S�Ҕԍ�     TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
004571                             PERFORM ���R�[�h�Z�b�g
004580                             PERFORM �V�l�����v�Z
004581* �����z�� ZERO �̎��́A�������܂Ȃ��B
004582                             IF  ��P�|�����z NOT = ZERO
004583                                 PERFORM �t�@�C������
004584                             END-IF
                                END-IF
004594                       END-IF
004610*                ********
004620*                * ���� *
004630*                ********
                         WHEN ���Z�|���Z���   = 3
004651**                 / �������O�� /
004652                       MOVE SPACE  TO  �쐬�t���O
004653                       MOVE ��|������� TO �ی���ʌ��p�v�j
004654                       PERFORM �������O�擾
004655*                     (����1�A���O2�A����0)
004656                       IF �������O�v�q NOT = ZERO
004657                          IF �������O�v�q = 1
004658                             IF �������O�`�F�b�N = 1
004659                                MOVE "YES"  TO  �쐬�t���O
004660                             END-IF
004661                          ELSE
004662                             IF �������O�`�F�b�N = ZERO
004663                                MOVE "YES"  TO  �쐬�t���O
004664                             END-IF
004665                          END-IF
004666                       ELSE
004667                          MOVE "YES"  TO  �쐬�t���O
004668                       END-IF 
004669**
004670                       IF �쐬�t���O  = "YES"
                                PERFORM ����Ώۃ`�F�b�N
                                IF ����t���O = "YES"
004671                             MOVE SPACE TO ��P�|���R�[�h
004672                             INITIALIZE ��P�|���R�[�h
003937                             MOVE ��|�������           TO ��P�|�ی����
004674                             MOVE ��|��p���S�Ҕԍ����� TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
004680                             PERFORM ���R�[�h�Z�b�g
004690                             PERFORM ���������v�Z
004697* �����z�� ZERO �̎��́A�������܂Ȃ��B
004698                             IF  ��P�|�����z NOT = ZERO
004699                                 PERFORM �t�@�C������
                                   END-IF
004700                          END-IF
004701                       END-IF
004710                   END-EVALUATE
004711                END-IF
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
004954*
005240*================================================================*
005250 �V�l�����v�Z SECTION.
005260*
005340     MOVE ���Z�|���v         TO ��P�|��p�z.
005350     MOVE ���Z�|�ꕔ���S��   TO ��P�|���S�z.
005360     MOVE ���Z�|�������z     TO ��P�|�����z.
005361*
005370*================================================================*
005380 ���������v�Z SECTION.
005390*
005210     MOVE ���Z�|���v         TO ��P�|��p�z.
005220     MOVE ���Z�|�󋋎ҕ��S�z TO ��P�|���S�z.
005230     MOVE ���Z�|�����������z TO ��P�|�����z.
005491*
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
006951*================================================================*
006952 ����Ώۃ`�F�b�N  SECTION.
006953*
      */�U�����݂̂̂Ƃ��͑g���A���ρA�������͏o���Ȃ�
      *
006957     MOVE "YES" TO ����t���O.
           IF �A���|�p����� = 1
006963        IF (���Z�|���Z��� = 1) AND (��|�ی���� = 03 OR 04 OR 09)
006986            MOVE SPACE TO ����t���O
              END-IF
006988        IF (���Z�|���Z��� = 3) AND
                 ((��|������� >= 50) AND (��|������� <= 60)) AND
                 (��|��p���S�Ҕԍ�����(3:2) = "13" OR "23" OR "39" OR "26")
006990              MOVE SPACE TO ����t���O
006991        END-IF
            END-IF.
      *
            IF ����t���O = "YES"
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 01)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 01))
                    IF (�k�C���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 02)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 02))
                    IF (�X�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 03)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 03))
                    IF (���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 04)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 04))
                    IF (�{��敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 05)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 05))
                    IF (�H�c�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 06)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 06))
                    IF (�R�`�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 07)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 07))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 08)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 08))
                    IF (���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 09)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 09))
                    IF (�Ȗ؋敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 10)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 10))
                    IF (�Q�n�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 11)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 11))
                    IF (��ʋ敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
      */��t�͌��ŗL�I���ɂȂ��Ă��Ă��U�V�����o��*/150409
      *           IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 12)) OR
      *              ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 12))
      *              IF (��t�敪�v = ZERO)
009576*                 MOVE "YES" TO ����t���O
      *              ELSE
009576*                 MOVE SPACE TO ����t���O
009577*              END-IF
009577*           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 13)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 13))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 14)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 14))
                    IF (�_�ސ�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 15)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 15))
                    IF (�V���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 16)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 16))
                    IF (�x�R�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 17)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 17))
                    IF (�ΐ�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 18)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 18))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 19)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 19))
                    IF (�R���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 20)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 20)) OR
                    (((��|������� >= 50) AND (��|������� <= 60)) AND (��|��p���S�Ҕԍ�����(3:2) = 20))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 21)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 21))
                    IF (�򕌋敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 22)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 22))
                    IF (�É��敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 23)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 23))
                    IF (���m�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 24)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 24))
                    IF (�O�d�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 25)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 25))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 26)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 26)) OR
                    ((��|������� >= 50) AND (��|������� <= 60))
                    IF (���s�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
                       IF (���Z�|���Z��� = 3) AND (��|�ی���� = 05) AND
                          (��|������� = 53) AND (��|��p���S�Ҕԍ�����(1:4) = 3926)
009576                    MOVE "YES" TO ����t���O
                       ELSE
009576                    MOVE SPACE TO ����t���O
                       END-IF
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 27)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 27)) OR
                    (((��|������� >= 50) AND (��|������� <= 60)) AND (��|��p���S�Ҕԍ�����(3:2) = 27))
                    IF (���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 28)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 28))
                    IF (���ɋ敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 29)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 29))
                    IF (�ޗǋ敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 30)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 30))
                    IF (�a�̎R�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 31)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 31))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 32)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 32))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 33)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 33))
                    IF (���R�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 34)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 34))
                    IF (�L���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 35)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 35))
                    IF (�R���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 36)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 36))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 37)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 37))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 38)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 38))
                    IF (���Q�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 39)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 39))
                    IF (���m�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 40)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 40))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 41)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 41))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 42)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 42))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 43)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 43))
                    IF (�F�{�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 44)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 44))
                    IF (�啪�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 45)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 45))
                    IF (�{��敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 46)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 46))
                    IF (�������敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��|�ی���� = 01) AND (��|�ی��Ҕԍ�(1:2) = 47)) OR
                    ((��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2) = 47))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
                 END-IF
009581     END-IF.
      */�_�ސ쌧�Ζ���06141261 ���U���̑ΏۊO�ɂ���/120427
           IF ��|�ی��Ҕԍ� = "06141261"
006957         MOVE SPACE TO ����t���O
           END-IF.
006996*
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
005630*
005640     PERFORM ��ƃt�@�C���Q�쐬����.
005650*
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
006540*===============================================================*
006550 ��ƃ��R�[�h�Q�Z�b�g SECTION.
006560*
006630     MOVE �����v         TO ��Q�|����.
006680*===============================================================*
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
006790*================================================================*
006800 �t�@�C���� SECTION.
006810*
006820     CLOSE ������}�X�^ ��f�ҏ��e ���Z�v�g�e.
006830*================================================================*
006840 �I������ SECTION.
006850*
006860     PERFORM �t�@�C����.
006870*================================================================*
006871*================================================================*
006872 �������O�擾 SECTION.
006873*
006874      MOVE ZERO  TO  �������O�`�F�b�N ���v�j.
006875      EVALUATE  �ی���ʌ��p�v�j
006876* ����
006877      WHEN  01
006878         IF ��|�ی��Ҕԍ�(1:2) = ���i�h�r�v�j
006879            MOVE  1  TO  �������O�`�F�b�N
006880         END-IF
006881         MOVE ��|�ی��Ҕԍ�(1:2) TO ���v�j
006882* �Е�
006883      WHEN  02
      */����ۂ̑Ή�/081009
               IF ��|�ی��Ҕԍ�(5:5) = SPACE
006884             IF ��|�ی��Ҕԍ�(1:2) = ���ǂi�h�r�v�j
006885                MOVE  1  TO  �������O�`�F�b�N
006886             END-IF
006887             MOVE ��|�ی��Ҕԍ�(1:2) TO ���v�j
006888             PERFORM ������
006889             MOVE ������v�j TO ���v�j
               ELSE
                   IF ��|�ی��Ҕԍ�(3:2) = ���i�h�r�v�j
                       MOVE  1  TO  �������O�`�F�b�N
                   END-IF
                   MOVE ��|�ی��Ҕԍ�(3:2) TO ���v�j
               END-IF
006890* �D��
006891* ����
006892* �g��
006893* ����
006894* ���q��
006895* �ސE����
006896      WHEN  06
006897      WHEN  07
006898      WHEN  08
006899      WHEN  09
006900      WHEN  04
006901      WHEN  03
006902         IF ��|�ی��Ҕԍ�(3:2) = ���i�h�r�v�j
006903            MOVE  1  TO  �������O�`�F�b�N
006904         END-IF
006905         MOVE ��|�ی��Ҕԍ�(3:2) TO ���v�j
006906* ����
006907      WHEN 50 THRU 60
006915         IF ��|��p���S�Ҕԍ�����(3:2) = ���i�h�r�v�j
006916            MOVE  1  TO  �������O�`�F�b�N
006917         END-IF
006918         MOVE ��|��p���S�Ҕԍ�����(3:2) TO ���v�j
006919* �V�l
006920      WHEN  05
006921         IF ��|��p���S�Ҕԍ�(3:2) = ���i�h�r�v�j
006922            MOVE  1  TO  �������O�`�F�b�N
006923         END-IF
006924         MOVE ��|��p���S�Ҕԍ�(3:2) TO ���v�j
006926* �J�ЁA�����ӁE����E���ےP�Ƃ͏��O
006927      WHEN  70
006928      WHEN  80
006928      WHEN  85
006929      WHEN  90
006930      WHEN  ZERO
006931         CONTINUE
006932*
006933     WHEN OTHER
006934         MOVE SPACE                TO �G���[���b�Z�[�W�v
006935         MOVE ��|���҃R�[�h       TO �G���[���҃R�[�h�v
006936         MOVE �ی���ʌ��p�v�j     TO �G���[�ی���ʂv
006937         MOVE "-"                  TO �G���[��؂�v
006938         MOVE  NC"�ی���ʂ��ُ�ł��B���ҁ|�ی����" TO �A���R�|���b�Z�[�W
006939         MOVE  �G���[���b�Z�[�W�v  TO �A���R�|���b�Z�[�W�P
006940         CALL   "MSG003"
006941         CANCEL "MSG003"
006942     END-EVALUATE.
006943*
006944*================================================================*
006945 ������ SECTION.
006946*
006947     MOVE ZERO  TO  ������v�j.
006948     EVALUATE  ���v�j
006949*�k�C��
006950      WHEN  01
006951         MOVE 01 TO ������v�j
006952*�X
006953      WHEN  02
006954         MOVE 02 TO ������v�j
006955*���
006956      WHEN  03
006957         MOVE 03 TO ������v�j
006958*�{��
006959      WHEN  04
006960         MOVE 04 TO ������v�j
006961*�H�c
006962      WHEN  05
006963         MOVE 05 TO ������v�j
006964*�R�`
006965      WHEN  06
006966         MOVE 06 TO ������v�j
006967*����
006968      WHEN  07
006969         MOVE 07 TO ������v�j
006970*���
006971      WHEN  08
006972         MOVE 08 TO ������v�j
006973*�Ȗ�
006974      WHEN  09
006975         MOVE 09 TO ������v�j
006976*�Q�n
006977      WHEN  10
006978         MOVE 10 TO ������v�j
006979*���
006980      WHEN  11
006981         MOVE 11 TO ������v�j
006982*��t
006983      WHEN  12
006984         MOVE 12 TO ������v�j
006985*����
006986      WHEN  21
006987         MOVE 13 TO ������v�j
006988*�_�ސ�
006989      WHEN  31
006990         MOVE 14 TO ������v�j
006991*�V��
006992      WHEN  32
006993         MOVE 15 TO ������v�j
006994*�x�R
006995      WHEN  33
006996         MOVE 16 TO ������v�j
006997*�ΐ�
006998      WHEN  34
006999         MOVE 17 TO ������v�j
007000*����
007001      WHEN  35
007002         MOVE 18 TO ������v�j
007003*�R��
007004      WHEN  36
007005         MOVE 19 TO ������v�j
007006*����
007007      WHEN  37
007008         MOVE 20 TO ������v�j
007009*��
007010      WHEN  38
007011         MOVE 21 TO ������v�j
007012*�É�
007013      WHEN  39
007014         MOVE 22 TO ������v�j
007015*���m
007016      WHEN  51
007017         MOVE 23 TO ������v�j
007018*�O�d
007019      WHEN  52
007020         MOVE 24 TO ������v�j
007021*����
007022      WHEN  53
007023         MOVE 25 TO ������v�j
007024*���s
007025      WHEN  54
007026         MOVE 26 TO ������v�j
007027*���
007028      WHEN  41
007029         MOVE 27 TO ������v�j
007030*����
007031      WHEN  42
007032         MOVE 28 TO ������v�j
007033*�ޗ�
007034      WHEN  55
007035         MOVE 29 TO ������v�j
007036*�a�̎R
007037      WHEN  56
007038         MOVE 30 TO ������v�j
007039*����
007040      WHEN  57
007041         MOVE 31 TO ������v�j
007042*����
007043      WHEN  58
007044         MOVE 32 TO ������v�j
007045*���R
007046      WHEN  59
007047         MOVE 33 TO ������v�j
007048*�L��
007049      WHEN  60
007050         MOVE 34 TO ������v�j
007051*�R��
007052      WHEN  61
007053         MOVE 35 TO ������v�j
007054*����
007055      WHEN  71
007056         MOVE 36 TO ������v�j
007057*����
007058      WHEN  72
007059         MOVE 37 TO ������v�j
007060*���Q
007061      WHEN  73
007062         MOVE 38 TO ������v�j
007063*���m
007064      WHEN  74
007065         MOVE 39 TO ������v�j
007066*����
007067      WHEN  75
007068         MOVE 40 TO ������v�j
007069*����
007070      WHEN  76
007071         MOVE 41 TO ������v�j
007072*����
007073      WHEN  77
007074         MOVE 42 TO ������v�j
007075*�F�{
007076      WHEN  78
007077         MOVE 43 TO ������v�j
007078*�啪
007079      WHEN  79
007080         MOVE 44 TO ������v�j
007081*�{��
007082      WHEN  80
007083         MOVE 45 TO ������v�j
007084*������
007085      WHEN  81
007086         MOVE 46 TO ������v�j
007087*����
007088      WHEN  82
007089         MOVE 47 TO ������v�j
007090*���̑�
007091      WHEN  OTHER
007092         CONTINUE
007093     END-EVALUATE.
007094*
007167*================================================================*
007168*================================================================*
007169******************************************************************
007170 END PROGRAM YHN432.
007171******************************************************************
