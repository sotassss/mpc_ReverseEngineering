000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN6061.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*         ���Z�v�g���X�g����y�ް��쐬�z�V�_����޳�ޔ�
000100*  �����N���o�[�W���� 
000110*         MED = YAW610 YHN612 
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2014-09-03
000140 DATE-COMPILED.          2014-09-03
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
000270     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000280                             ORGANIZATION             IS  INDEXED
000290                             ACCESS MODE              IS  DYNAMIC
000300                             RECORD KEY               IS ��|�{�p�a��N��
000310                                                          ��|���҃R�[�h
000320                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000330                                                          ��|���҃J�i
000340                                                          ��|���҃R�[�h
000350                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000360                                                         ��|�{�p�a��N��
000370                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000380                                                          ��|�ی����
000390                                                          ��|�ی��Ҕԍ�
000400                                                          ��|���҃R�[�h
000410                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000420                                                          ��|������
000430                                                     ��|��p���S�Ҕԍ�
000440                                                          ��|���҃R�[�h
000450                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000460                                                          ��|�������
000470                                                  ��|��p���S�Ҕԍ�����
000480                                                          ��|���҃R�[�h
000490                             ALTERNATE RECORD KEY  IS ��|�����a��N��
000500                                                      ��|�{�p�a��N��
000510                                                      ��|���҃R�[�h
000520                             FILE STATUS              IS  ��ԃL�[
000530                             LOCK        MODE         IS  AUTOMATIC.
000630     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000640                             ORGANIZATION             IS  INDEXED
000650                             ACCESS MODE              IS  DYNAMIC
000660                             RECORD KEY               IS  ���|����敪
000670                             FILE STATUS              IS  ��ԃL�[
000680                             LOCK        MODE         IS  AUTOMATIC.
000780     SELECT  ������}�X�^    ASSIGN      TO         SEIKYUSL
000790                             ORGANIZATION             IS  INDEXED
000800                             ACCESS MODE              IS  DYNAMIC
000810                             RECORD KEY               IS  ����|�ی����
000820                                                          ����|�ی��Ҕԍ�
000830                             FILE STATUS              IS  ��ԃL�[
000840                             LOCK    MODE             IS  AUTOMATIC.
000850     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000860                             ORGANIZATION             IS  INDEXED
000870                             ACCESS MODE              IS  DYNAMIC
000880                             RECORD KEY               IS  �ہ|�ی����
000890                                                          �ہ|�ی��Ҕԍ�
000900* �����́A�L�[���ڂ̕ی��Җ��̂�ی��҃J�i�ɂ���
000910                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000920                                                          �ہ|�ی��Җ���
000930                                                          �ہ|�ی��Ҕԍ�
000940                             FILE STATUS              IS  ��ԃL�[
000950                             LOCK        MODE         IS  AUTOMATIC.
000960     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000970                             ORGANIZATION             IS  INDEXED
000980                             ACCESS MODE              IS  DYNAMIC
000990                             RECORD KEY               IS  �s�|������
001000                                                          �s�|�s�����ԍ�
001010                             ALTERNATE RECORD KEY     IS  �s�|������
001020                                                          �s�|�s��������
001030                                                          �s�|�s�����ԍ�
001040                             FILE STATUS              IS  ��ԃL�[
001050                             LOCK        MODE         IS  AUTOMATIC.
000241     SELECT  ���ۏ��e      ASSIGN      TO        SEIHOJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS ���ہ|�{�p�a��N��
000245                                                         ���ہ|���҃R�[�h
000255                             ALTERNATE RECORD KEY     IS ���ہ|���҃R�[�h
000265                                                         ���ہ|�{�p�a��N��
000277                             FILE STATUS              IS ��ԃL�[
000278                             LOCK        MODE         IS AUTOMATIC.
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
001060     SELECT  ��ƃt�@�C���P  ASSIGN      TO   "C:\MAKISHISYS\YAWOBJ\TEMP\W6101L.DAT"
001070                             ORGANIZATION             IS  INDEXED
001080                             ACCESS                   IS  DYNAMIC
001090                             RECORD      KEY          IS  ��P�|�����a��N��
000907                                                          ��P�|���ރR�[�h
                                                                ��P�|���R�[�h
                                                                ��P�|�ی���
001130                                                          ��P�|�ی��Ҕԍ�
001140                                                          ��P�|�{�l�Ƒ��敪
                                                                ��P�|���S����
001150                                                          ��P�|���҃J�i
001160                                                          ��P�|���҃R�[�h
001170                                                          ��P�|�{�p�a��N��
001180                             FILE        STATUS       IS  ��ԃL�[
001190                             LOCK        MODE         IS  AUTOMATIC.
001200     SELECT  ��ƃt�@�C���Q  ASSIGN      TO  "C:\MAKISHISYS\YAWOBJ\TEMP\W6102L.DAT"
001210                             ORGANIZATION             IS  INDEXED
001220                             ACCESS                   IS  DYNAMIC
001230                             RECORD      KEY          IS  ��Q�|�ی��敪
                                                                ��Q�|����
                                   ALTERNATE RECORD KEY     IS  ��Q�|�{�p�a��N��
                                                                ��Q�|���҃R�[�h
                                                                ��Q�|�ی��敪
                                                                ��Q�|����
001240                             FILE        STATUS       IS  ��ԃL�[
001250                             LOCK        MODE         IS  AUTOMATIC.
001080* ���Z���я��p
001081     SELECT  ��ƃt�@�C���R  ASSIGN      TO  "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001100                             ORGANIZATION             IS  INDEXED
001110                             ACCESS                   IS  DYNAMIC
001120                             RECORD      KEY          IS  ��R�|�{�p�a��N��
001130                                                          ��R�|���҃R�[�h
001140                                                          ��R�|�ی����
001150                             FILE        STATUS       IS  ��ԃL�[
001160                             LOCK        MODE         IS  AUTOMATIC.
001260******************************************************************
001270*                      DATA DIVISION                             *
001280******************************************************************
001290 DATA                    DIVISION.
001300 FILE                    SECTION.
001310*                           �m�q�k��  �R�Q�O�n
001320 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001330     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001340*                           �m�q�k��  �Q�T�U�n
001350 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001360     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000991     COPY SEIGYO02        OF  XFDLIB  JOINING   ���O�Q   AS  PREFIX.
001430*                           �m�q�k��  �P�Q�W�n
001440 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001450     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
001460*                           �m�q�k��  �R�Q�O�n
001470 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001480     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001490*                           �m�q�k��  �Q�T�U�n
001500 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001510     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
001080* 
000280 FD  ���ۏ��e          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   ����   AS  PREFIX.
      *
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001520* 
001530 FD  ��ƃt�@�C���P RECORD  CONTAINS 256 CHARACTERS.
001540 01  ��P�|���R�[�h.
001550     03  ��P�|���R�[�h�L�[.
001560         05  ��P�|�����a��N��.
001570             07  ��P�|�����a��              PIC 9.
001580             07  ��P�|�����N                PIC 9(2).
001590             07  ��P�|������                PIC 9(2).
001261         05  ��P�|���ރR�[�h                PIC 9(1).
001261         05  ��P�|���R�[�h                  PIC X(2).
001400         05  ��P�|�ی���                    PIC 9(2).
001271         05  ��P�|�ی��Ҕԍ�.
001530             07 ��P�|�@�ʔԍ�             PIC X(2).
001540             07 ��P�|�۔�.
001550                09 ��P�|�s���{���ԍ�      PIC X(2).
001560                09 ��P�|�ی��ԍ�          PIC X(3).
001570                09 ��P�|���ؔԍ��v�q      PIC X.
001580                09 FILLER                  PIC X(2).
001640         05  ��P�|�{�l�Ƒ��敪              PIC 9.
               05  ��P�|���S����                  PIC 9(2).
001650         05  ��P�|���҃J�i                  PIC X(50).
001660         05  ��P�|���҃R�[�h.
001670             07 ��P�|���Ҕԍ�               PIC 9(6).
001680             07 ��P�|�}��                   PIC X(1).
001690         05  ��P�|�{�p�a��N��.
001700             07  ��P�|�{�p�a��              PIC 9.
001710             07  ��P�|�{�p�N                PIC 9(2).
001720             07  ��P�|�{�p��                PIC 9(2).
001730     03  ��P�|���R�[�h�f�[�^.
001221         05  ��P�|���Z���                  PIC 9(2).
001740         05  ��P�|���Ҏ���                  PIC X(50).
001760         05  ��P�|��ی��Ҏ���              PIC X(50).
001750         05  ��P�|�ی����                  PIC 9(2).
001271         05  ��P�|�ی��Ҕԍ����K            PIC X(8).
001760         05  ��P�|���x���`�F�b�N            PIC X.
001366         05  ��P�|��p�z                    PIC 9(6).
001367         05  ��P�|���S�z                    PIC 9(6).
001368         05  ��P�|�����z                    PIC 9(6).
001369         05  ��P�|���Z����敪              PIC 9.
001770         05  FILLER                          PIC X(37).
001780*
001790 FD  ��ƃt�@�C���Q RECORD  CONTAINS 155 CHARACTERS.
001800 01  ��Q�|���R�[�h.
001810     03  ��Q�|���R�[�h�L�[.
001820         05  ��Q�|�ی��敪                PIC 9(1).
001820         05  ��Q�|����                    PIC 9(4).
001830     03  ��Q�|���R�[�h�f�[�^.
               05  ��Q�|���Z���                PIC 9(2).
001840         05  ��Q�|���҃R�[�h.
001850             07 ��Q�|���Ҕԍ�             PIC 9(6).
001860             07 ��Q�|�}��                 PIC X(1).
001870         05  ��Q�|���Ҏ���                PIC X(50).
001740         05  ��Q�|��ی��Ҏ���            PIC X(50).
001880         05  ��Q�|�ی����                PIC 9(2).
               05  ��Q�|�����ی��Ҕԍ�          PIC X(10).
001890         05  ��Q�|�{�l�Ƒ�                PIC X(4).
001900         05  ��Q�|�{�p�a��N��.
001910             07  ��Q�|�{�p�a��            PIC 9.
001920             07  ��Q�|�{�p�N              PIC 9(2).
001930             07  ��Q�|�{�p��              PIC 9(2).
001940         05  ��Q�|����w��                PIC 9(1).
               05  ��Q�|��p�z                  PIC 9(6).
               05  ��Q�|���S�z                  PIC 9(6).
               05  ��Q�|�����z                  PIC 9(6).
               05  ��Q�|���Z����敪            PIC 9.
001730*
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
001960*----------------------------------------------------------------*
001970******************************************************************
001980*                WORKING-STORAGE SECTION                         *
001990******************************************************************
002000 WORKING-STORAGE         SECTION.
002010 01 �L�[����                           PIC X     VALUE SPACE.
002020 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
002030 01 �I���t���O                         PIC X(3)  VALUE SPACE.
002040 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
002050 01 �쐬�t���O                         PIC X(3)  VALUE SPACE.
002060 01 ���s�L�[�v                         PIC X(3)  VALUE SPACE.
002070 01 �{�p�L�^�L�v                       PIC X(3)  VALUE SPACE.
002080*
002090 01 ���Ԃv                             PIC 9(4)  VALUE ZERO.
002090 01 ���ۏ��Ԃv                         PIC 9(4)  VALUE ZERO.
002100 01 �ی���ʂv�q                       PIC 9(2)  VALUE ZERO.
002110 01 ���Z�v�g��ނv�q                   PIC X(4)  VALUE SPACE.
002120 01 ���Z���я����~�v                   PIC 9(1) VALUE ZERO.
001320 01 �ی��Ҕԍ��v�q.
001530    03 �@�ʔԍ��v�q                    PIC X(2)  VALUE SPACE.
001540    03 �۔Ԃv�q.
001550       05 �s���{���ԍ��v�q             PIC X(2)  VALUE SPACE.
001560       05 �ی��ԍ��v�q                 PIC X(3)  VALUE SPACE.
001570       05 ���ؔԍ��v�q                 PIC X     VALUE SPACE.
001580       05 FILLER                       PIC X(2)  VALUE SPACE.
002140 01 �{�l�Ƒ��敪�v�q                   PIC 9     VALUE ZERO.
001770 01 ���S�����v                         PIC 9    VALUE ZERO.
002150 01 �����a��N���v�q.
002160    03 �����a��v�q                    PIC 9     VALUE ZERO.
002170    03 �����N�v�q                      PIC 9(2)  VALUE ZERO.
002180    03 �������v�q                      PIC 9(2)  VALUE ZERO.
002190 01 ���x���ԍ��`�F�b�N                 PIC X(3)  VALUE SPACE.
002200 01 �ی��Ҕԍ����x���v�j               PIC X(10)  VALUE SPACE.
002210 01 ��ʔԍ����x���v�j                 PIC X(2)  VALUE SPACE.
002220*
002230 01 �����N���v.
002240    03 �����a��v                      PIC 9(1)  VALUE ZERO.
002250    03 �����N�v                        PIC 9(2)  VALUE ZERO.
002260    03 �������v                        PIC 9(2)  VALUE ZERO.
002270 01 �{�p�N���v.
002280    03 �{�p�a��v                      PIC 9(1)  VALUE ZERO.
002290    03 �{�p�N�v                        PIC 9(2)  VALUE ZERO.
002300    03 �{�p���v                        PIC 9(2)  VALUE ZERO.
002310 01 �{�l�Ƒ��敪�v                     PIC 9(1)  VALUE ZERO.
002320 01 �t�@�C����                         PIC N(2)  VALUE SPACE.
002330 01 �ی���ʂv                         PIC 9(2)  VALUE ZERO.
002340 01 ���ی��Ҕԍ��v                     PIC X(10) VALUE SPACE.
002350 01 �ی��Ҕԍ��v                       PIC X(10) VALUE SPACE.
002360 01 �e�ی���ʂv                       PIC 9(2)  VALUE ZERO.
002370 01 ���x���`�F�b�N�v                   PIC X     VALUE SPACE.
002380 01 �Ώی����v                         PIC 9(5)  VALUE ZERO.
002390*
       01 ���z�v.
          03 ��p�z�v                        PIC 9(6)  VALUE ZERO.
          03 ���S�z�v                        PIC 9(6)  VALUE ZERO.
          03 �����z�v                        PIC 9(6)  VALUE ZERO.
001576*
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
001559 01 ���ǂi�h�r�v�j                     PIC X(2)  VALUE SPACE.
001561 01 ���v�j                             PIC X(2)  VALUE SPACE.
001563 01 ������v�j                         PIC X(2)  VALUE SPACE.
002391*
002392** �������Z�܂Ƃߗp
002393 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
002394*
002400******************************************************************
002410*                          �A������                              *
002420******************************************************************
002430*
002440****************
002450* ��ʓ��͏�� *
002460****************
002470**
002480 01 �A���|���̓f�[�^�U�P�O IS EXTERNAL.
002490    03 �A���|�����N��.
002500       05 �A���|�����a��                  PIC 9.
002510       05 �A���|�����N                    PIC 9(2).
002520       05 �A���|������                    PIC 9(2).
002540    03 �A���|�ی����                     PIC 9(2).
002600*
002610*****
002611************************
002612* �������Z�܂Ƃ�
002613************************
002614 01 �A���Z�܂Ƃ߁|�L�[ IS EXTERNAL.
002615    03 �A���Z�܂Ƃ߁|�{�p�a��N��.
002616       05 �A���Z�܂Ƃ߁|�{�p�a��               PIC 9.
002617       05 �A���Z�܂Ƃ߁|�{�p�N��.
002618          07 �A���Z�܂Ƃ߁|�{�p�N              PIC 9(2).
002619          07 �A���Z�܂Ƃ߁|�{�p��              PIC 9(2).
002620    03 �A���Z�܂Ƃ߁|���҃R�[�h.
002621       05 �A���Z�܂Ƃ߁|���Ҕԍ�               PIC 9(6).
002622       05 �A���Z�܂Ƃ߁|�}��                   PIC X(1).
002623**-------------------------------------------------------**
002624*   1:�������Z�v�g�Ȃ��̖{�̂܂Ƃ߂̔���
002625*   2:���l�E���p�̎Еۏ������Z���̔���
002626    03 �A���Z�܂Ƃ߁|����敪                  PIC 9.
002627**-------------------------------------------------------**
002628*  / OUT /�@ 0:�ΏۊO�A1:�Ώ�
002629    03 �A���Z�܂Ƃ߁|���茋��                  PIC 9.
002630**
002631******************************************************************
002632*                      PROCEDURE  DIVISION                       *
002640******************************************************************
002650 PROCEDURE               DIVISION.
002660************
002670*           *
002680* ��������   *
002690*           *
002700************
002710     PERFORM �t�@�C���I�[�v��.
002720     PERFORM �A�����ڑҔ�.
002730************
002740*           *
002750* �又��     *
002760*           *
002770************
002780     PERFORM ��ƃt�@�C���P�쐬.
002790     PERFORM ��ƃt�@�C���Q�쐬.
002810     PERFORM ���я��t�@�C���쐬.
002800************
002810*           *
002820* �I������   *
002830*           *
002840************
002850     PERFORM �I������.
002860     MOVE ZERO TO PROGRAM-STATUS.
002870     EXIT PROGRAM.
002880*
002890*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002900*================================================================*
002910 �A�����ڑҔ� SECTION.
002920*
002930* �A�����ڂ̑Ҕ�
002940     MOVE �A���|�����a��      TO �����a��v�q.
002950     MOVE �A���|�����N        TO �����N�v�q.
002960     MOVE �A���|������        TO �������v�q.
002960     MOVE �A���|�ی����      TO �ی���ʂv�q.
002970* ���Z���я������E�~���Z�b�g
002980     MOVE ZEROS TO ���|����敪.
002990     READ ������}�X�^
003000     NOT INVALID KEY
003010         MOVE ���|���Z���я����~ TO ���Z���я����~�v
003020     END-READ.
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
003030*
003040*================================================================*
003050 �t�@�C���I�[�v�� SECTION.
003060*
003070     OPEN INPUT ��f�ҏ��e.
003080         MOVE NC"��f�ҏ��e" TO �t�@�C����.
003090         PERFORM �I�[�v���`�F�b�N.
003130     OPEN INPUT ������}�X�^
003140         MOVE NC"������" TO �t�@�C����.
003150         PERFORM �I�[�v���`�F�b�N.
003190     OPEN INPUT ������}�X�^.
003200         MOVE NC"������" TO �t�@�C����.
003210         PERFORM �I�[�v���`�F�b�N.
003220     OPEN INPUT �ی��҃}�X�^.
003230         MOVE NC"�ی�" TO �t�@�C����.
003240         PERFORM �I�[�v���`�F�b�N.
003250     OPEN INPUT �s�����}�X�^.
003260         MOVE NC"�s����" TO �t�@�C����.
003270         PERFORM �I�[�v���`�F�b�N.
006630     OPEN INPUT ���ۏ��e.
006640         MOVE NC"����" TO �t�@�C����.
006650         PERFORM �I�[�v���`�F�b�N.
003250     OPEN INPUT ���Z�v�g�e.
003260         MOVE NC"���Z�v�g�e" TO �t�@�C����.
003270         PERFORM �I�[�v���`�F�b�N.
003280*================================================================*
003290 �I�[�v���`�F�b�N SECTION.
003300*
003310     IF ��ԃL�[  NOT =  "00"
003320         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
003330         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
003340         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003350                                                 UPON CONS
003131*-----------------------------------------*
003132         CALL "actcshm"  WITH C LINKAGE
003133*-----------------------------------------*
003360         ACCEPT  �L�[���� FROM CONS
003370         PERFORM �t�@�C����
003380         MOVE 99 TO PROGRAM-STATUS
003390         EXIT PROGRAM.
003400*================================================================*
003410 �G���[�\�� SECTION.
003420*
003430     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
003440     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
003450     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
003460     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
003470     ACCEPT  �L�[���� FROM CONS.
003480     PERFORM �t�@�C����.
003490     MOVE 99 TO PROGRAM-STATUS.
003500     EXIT PROGRAM.
003510*================================================================*
003520 ��ƃt�@�C���P�쐬 SECTION.
003530*
003540     OPEN OUTPUT ��ƃt�@�C���P.
003550         MOVE NC"���" TO �t�@�C����.
003560         PERFORM �I�[�v���`�F�b�N.
003570*
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
003690     END-START.
003700     IF ��ԃL�[ = "00"
003710         MOVE SPACE  TO �I���t���O
003750         PERFORM ���Z�v�g�e�Ǎ�
003730         PERFORM UNTIL ( �I���t���O = "YES" ) OR
005330                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
005340                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
005350                       ( ���Z�|������   NOT = �������v�q   )
003770             PERFORM �f�[�^�`�F�b�N
003780** �J�ЁE�����ӁE�����
003790             IF  ���Z�|���Z��� = 4 OR 5 OR 6 OR 8
003800                 MOVE SPACE  TO ���s�L�[�v
003810             END-IF
003820***
003830             IF ���s�L�[�v = "YES"
003840*
003842*��* ���ʏ����i�������Z�܂Ƃ߁j
003843*                IF ��|������� NOT = ZERO
003844*                   PERFORM �������Z�܂Ƃߔ���
003845*                ELSE
003846*                   MOVE SPACE TO �������Z�܂Ƃ߃t���O
003847*                END-IF
003848*��*
005610                EVALUATE TRUE
003850*                ******************
003860*                * ���� �V�l�܂܂�*
003870*                ******************
013940                WHEN ���Z�|���Z���   = 1
                          IF (�ی���ʂv�q = ZERO) OR
                              ((��|�ی���� = �ی���ʂv�q) AND ( ��|������ = ZERO ))
003920                        MOVE SPACE TO ��P�|���R�[�h
003930                        INITIALIZE ��P�|���R�[�h
003940*                        PERFORM ������ԃZ�b�g
003950                        MOVE ��|�ی��Ҕԍ�  TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
003960                        MOVE ��|�ی����      TO ��P�|�ی����
003970*** ���x���ԍ��`�F�b�N
003980*                        MOVE ��|�ی����   TO ��ʔԍ����x���v�j
003990*                        MOVE ��|�ی��Ҕԍ� TO �ی��Ҕԍ����x���v�j
004000*                        PERFORM ���x���ی��҃`�F�b�N
004010                        PERFORM �����Z�b�g
004010                        PERFORM ���R�[�h�Z�b�g
004020                        PERFORM �t�@�C������
004030                    END-IF
004050*                ********
004060*                * �V�l *
004070*                ********
                      WHEN ���Z�|���Z���   = 2 
                          IF (�ی���ʂv�q = ZERO) OR
                             (��|������ = �ی���ʂv�q)
004100                        MOVE SPACE TO ��P�|���R�[�h
004110                        INITIALIZE ��P�|���R�[�h
004120*                        PERFORM ������ԃZ�b�g
004130                        MOVE ��|��p���S�Ҕԍ�     TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
004140                        MOVE ��|������      TO ��P�|�ی����
004150*** ���x���ԍ��`�F�b�N
004160*                        MOVE ��|������       TO ��ʔԍ����x���v�j
004170*                        MOVE ��|��p���S�Ҕԍ� TO �ی��Ҕԍ����x���v�j
004180*                        PERFORM ���x���s�����`�F�b�N
004010                        PERFORM �����Z�b�g
004190                        PERFORM ���R�[�h�Z�b�g
004200                        PERFORM �t�@�C������
004210                    END-IF
004220*                ********
004230*                * ���� *
004240*                ********
004241*��* ���ʏ����i�������Z�܂Ƃ߁j**
                      WHEN ���Z�|���Z���   = 3
                          IF (�ی���ʂv�q = ZERO) OR
                             (��|������� = �ی���ʂv�q)
004242                      IF ( �������Z�܂Ƃ߃t���O = SPACE ) 
004270                        MOVE SPACE TO ��P�|���R�[�h
004280                        INITIALIZE ��P�|���R�[�h
004290                        MOVE ��|�������           TO ��P�|�ی����
004300                        MOVE ��|��p���S�Ҕԍ����� TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
004310*                        PERFORM ������ԃZ�b�g
004320*** ���x���ԍ��`�F�b�N
004330*                        MOVE ��|�������           TO ��ʔԍ����x���v�j
004340*                        MOVE ��|��p���S�Ҕԍ����� TO �ی��Ҕԍ����x���v�j
004350*                        PERFORM ���x���s�����`�F�b�N
004010                        PERFORM ���������Z�b�g
004360                        PERFORM ���R�[�h�Z�b�g
004445* �����z�� ZERO �̎��́A�������܂Ȃ��B
004446                        IF  ��P�|�����z NOT = ZERO
004370                            PERFORM �t�@�C������
004448                        END-IF
004380                      END-IF
004381                    END-IF
004220*                ********
004230*                * ���� *
004240*                ********
                      WHEN ���Z�|���Z���   = 7
                          IF (�ی���ʂv�q = ZERO) OR
                             (��|�ی���� = �ی���ʂv�q)
003920                        MOVE SPACE TO ��P�|���R�[�h
003930                        INITIALIZE ��P�|���R�[�h
003940*                        PERFORM ������ԃZ�b�g
                              MOVE ��|�{�p�a��N�� TO ���ہ|�{�p�a��N��
                              MOVE ��|���҃R�[�h   TO ���ہ|���҃R�[�h
                              READ ���ۏ��e
                              NOT INVALID KEY
003950                           MOVE ���ہ|���S�Ҕԍ�   TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
                              END-READ
003960                        MOVE ��|�ی����     TO ��P�|�ی����
003970*** ���x���ԍ��`�F�b�N
003980*                        MOVE ��|�ی����     TO ��ʔԍ����x���v�j
003990*                        MOVE ��P�|�ی��Ҕԍ� TO �ی��Ҕԍ����x���v�j
004000*                        PERFORM ���x���ی��҃`�F�b�N
004010                        PERFORM �����Z�b�g
004370                        PERFORM ���R�[�h�Z�b�g
004380                        PERFORM �t�@�C������
004391                    END-IF
                      END-EVALUATE
004390             END-IF
004400             PERFORM ���Z�v�g�e�Ǎ�
004410         END-PERFORM
004420     END-IF.
004430     CLOSE ��ƃt�@�C���P.
004440*================================================================*
004450 ���Z�v�g�e�Ǎ� SECTION.
004460*
004470     READ ���Z�v�g�e NEXT
004480     AT END
004490         MOVE "YES" TO �I���t���O
004500     END-READ.
004510*================================================================*
004520 �f�[�^�`�F�b�N SECTION.
004530*
004540     MOVE SPACE          TO ���s�L�[�v.
005350     MOVE SPACE  TO ���s�L�[�v.
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
                 END-READ
019900        ELSE
019910           MOVE SPACE  TO ���s�L�[�v
              END-IF
019950     END-IF.
005000*
005080*================================================================*
005090 ���R�[�h�Z�b�g SECTION.
005100*
005110     MOVE ���Z�|�����a��    TO ��P�|�����a��.
005120     MOVE ���Z�|�����N      TO ��P�|�����N.
005130     MOVE ���Z�|������      TO ��P�|������.
005140     MOVE ���Z�|�{�p�a��    TO ��P�|�{�p�a��.
005150     MOVE ���Z�|�{�p�N      TO ��P�|�{�p�N.
005160     MOVE ���Z�|�{�p��      TO ��P�|�{�p��.
005480     MOVE ���Z�|���S����    TO ��P�|���S����.
005170     MOVE ��|�{�l�Ƒ��敪  TO ��P�|�{�l�Ƒ��敪.
005526     IF ��|������ NOT = ZERO
005534         MOVE 1             TO ��P�|�{�l�Ƒ��敪
           END-IF.
005180     MOVE ��|���҃J�i      TO ��P�|���҃J�i.
005190     MOVE ��|���҃R�[�h    TO ��P�|���҃R�[�h.
005200     MOVE ��|���Ҏ���      TO ��P�|���Ҏ���.
005210     MOVE ��|��ی��Ҏ���  TO ��P�|��ی��Ҏ���.
005210*     IF ���x���ԍ��`�F�b�N = "YES"
005220*        MOVE  "1"           TO ��P�|���x���`�F�b�N
005230*     END-IF.
005240*
005250*************************************************************************************
005260*  ������}�X�^��ǂ�ŁA�f�[�^�����鎞�́A������s�����R�[�h��ی��Ҕԍ��ɓ]�L����B
005270*************************************************************************************
005280*** ���x���ԍ��]�A�̎��͔�΂�
005290*     IF ���x���ԍ��`�F�b�N NOT = "YES"
005300*        MOVE ��P�|�ی����    TO ����|�ی����
005310*        MOVE ��P�|�ی��Ҕԍ�  TO ����|�ی��Ҕԍ�
005320*        READ ������}�X�^
005330*        NOT INVALID KEY
005340*            MOVE ����|������s����   TO ��P�|�ی��Ҕԍ�
005350*        END-READ
005360*     END-IF.
           MOVE ���Z�|���Z���    TO ��P�|���Z���.
           MOVE ��p�z�v          TO ��P�|��p�z.
           MOVE �����z�v          TO ��P�|�����z.
           MOVE ���S�z�v          TO ��P�|���S�z.
005541*
005542     PERFORM ���ގ擾.
005370*
005390*================================================================*
       �����Z�b�g SECTION.
           MOVE ���Z�|���v            TO ��p�z�v.
           MOVE ���Z�|�������z        TO �����z�v.
           MOVE ���Z�|�ꕔ���S��      TO ���S�z�v.
005210     MOVE ��|���Z����敪      TO ��P�|���Z����敪.
005380*
005390*================================================================*
       ���������Z�b�g SECTION.
           MOVE ���Z�|���v            TO ��p�z�v.
           MOVE ���Z�|�����������z    TO �����z�v.
           MOVE ���Z�|�󋋎ҕ��S�z    TO ���S�z�v.
005210     MOVE ��|���Z����敪����  TO ��P�|���Z����敪.
005510*
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
                       IF ��|�ی��Ҕԍ�(5:5) = SPACE
006878                    MOVE ��|�ی��Ҕԍ�(1:2) TO ���v�j
006879                    PERFORM ������
006880                    MOVE ������v�j TO ���v�j
                       ELSE
006878                    MOVE ��|�ی��Ҕԍ�(3:2) TO ���v�j
                       END-IF
005736                 MOVE 1  TO  ��P�|���ރR�[�h
005742                 MOVE ���v�j TO ��P�|���R�[�h
005733             WHEN 06
005736                 MOVE 1  TO  ��P�|���ރR�[�h
005742                 MOVE ��|�ی��Ҕԍ�(3:2) TO ��P�|���R�[�h
005734             WHEN 07
005736                 MOVE 2  TO  ��P�|���ރR�[�h
005742                 MOVE ��|�ی��Ҕԍ�(3:2) TO ��P�|���R�[�h
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
005780*
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
005380*================================================================*
005390 �t�@�C������ SECTION.
005400*
005410     WRITE ��P�|���R�[�h
005420     INVALID KEY
005430         MOVE NC"���"  TO �t�@�C����
005440         PERFORM �G���[�\��
005450     END-WRITE.
005460*================================================================*
005470 ��ƃt�@�C���Q�쐬 SECTION.
005480*
005490     OPEN INPUT  ��ƃt�@�C���P.
005500     OPEN OUTPUT ��ƃt�@�C���Q.
005510*
005520     IF ���Z���я����~�v = 1
005530         PERFORM ���Z���я��~���t�@�C���쐬
005540     ELSE
005550         PERFORM ��ƃt�@�C���Q�쐬����
005560     END-IF.
005570*
005580     CLOSE ��ƃt�@�C���Q.
005590     CLOSE ��ƃt�@�C���P.
005600*================================================================*
005610 ���Z���я��~���t�@�C���쐬 SECTION.
005620*
005630     MOVE ZERO       TO ���Ԃv.      
005640     MOVE 9          TO ��P�|�����a��.
005650     MOVE 99         TO ��P�|�����N.
005660     MOVE 99         TO ��P�|������.
005670     MOVE 9          TO ��P�|���ރR�[�h.
005680     MOVE HIGH-VALUE TO ��P�|���R�[�h.
005690     MOVE 99         TO ��P�|�ی���.
005700     MOVE HIGH-VALUE TO ��P�|�ی��Ҕԍ�.
005710     MOVE 9          TO ��P�|�{�l�Ƒ��敪.
005670     MOVE 99         TO ��P�|���S����.
005720     MOVE HIGH-VALUE TO ��P�|���҃J�i.
005730     MOVE 999999     TO ��P�|���Ҕԍ�.
005740     MOVE HIGH-VALUE TO ��P�|�}��.
005750     MOVE 9          TO ��P�|�{�p�a��.
005760     MOVE 99         TO ��P�|�{�p�N.
005770     MOVE 99         TO ��P�|�{�p��.
005780     START ��ƃt�@�C���P KEY IS <=  ��P�|�����a��N��
005790                                     ��P�|���ރR�[�h
005800                                     ��P�|���R�[�h
005810                                     ��P�|�ی���
005820                                     ��P�|�ی��Ҕԍ�
005830                                     ��P�|�{�l�Ƒ��敪
                                           ��P�|���S����
005840                                     ��P�|���҃J�i
005850                                     ��P�|���҃R�[�h
005860                                     ��P�|�{�p�a��N��
005870                                     REVERSED
005880     END-START.
005890     IF ��ԃL�[ = "00"
005900*
005910         MOVE SPACE TO �I���t���O
005920         PERFORM ��ƃt�@�C���P�Ǎ�
005930         PERFORM UNTIL �I���t���O = "YES"
005940             MOVE SPACE TO ��Q�|���R�[�h
005950             INITIALIZE ��Q�|���R�[�h
005960*
005970                 PERFORM ��ƃ��R�[�h�Q�Z�b�g
005980                 PERFORM ��ƃt�@�C���Q����
005990*
006000             PERFORM ��ƃt�@�C���P�Ǎ�
006010         END-PERFORM
006020     END-IF.
006030*================================================================*
006040 ��ƃt�@�C���Q�쐬���� SECTION.
006050*
006060     MOVE ZERO       TO ���Ԃv.      
006070     MOVE ZERO       TO ��P�|�����a��.
006080     MOVE ZERO       TO ��P�|�����N.
006090     MOVE ZERO       TO ��P�|������.
006100     MOVE ZERO       TO ��P�|���ރR�[�h.
006110     MOVE SPACE      TO ��P�|���R�[�h.
006120     MOVE ZERO       TO ��P�|�ی���.
006130     MOVE LOW-VALUE  TO ��P�|�ی��Ҕԍ�.
006140     MOVE ZERO       TO ��P�|�{�l�Ƒ��敪.
006120     MOVE ZERO       TO ��P�|���S����.
006150     MOVE SPACE      TO ��P�|���҃J�i.
006160     MOVE ZERO       TO ��P�|���Ҕԍ�.
006170     MOVE LOW-VALUE  TO ��P�|�}��.
006180     MOVE ZERO       TO ��P�|�{�p�a��.
006190     MOVE ZERO       TO ��P�|�{�p�N.
006200     MOVE ZERO       TO ��P�|�{�p��.
006210     START ��ƃt�@�C���P KEY IS >=  ��P�|�����a��N��
005790                                     ��P�|���ރR�[�h
005800                                     ��P�|���R�[�h
005810                                     ��P�|�ی���
006250                                     ��P�|�ی��Ҕԍ�
006260                                     ��P�|�{�l�Ƒ��敪
                                           ��P�|���S����
006270                                     ��P�|���҃J�i
006280                                     ��P�|���҃R�[�h
006290                                     ��P�|�{�p�a��N��
006300     END-START.
006310     IF ��ԃL�[ = "00"
006320*
006330         MOVE SPACE TO �I���t���O
006340         PERFORM ��ƃt�@�C���P�Ǎ�
006350         PERFORM UNTIL �I���t���O = "YES"
006360             MOVE SPACE TO ��Q�|���R�[�h
006370             INITIALIZE ��Q�|���R�[�h
006380*
006390                 PERFORM ��ƃ��R�[�h�Q�Z�b�g
006400                 PERFORM ��ƃt�@�C���Q����
006410*
006420             PERFORM ��ƃt�@�C���P�Ǎ�
006430         END-PERFORM
006440     END-IF.
006450*================================================================*
006460 ��ƃt�@�C���P�Ǎ� SECTION.
006470*
006480     READ ��ƃt�@�C���P NEXT
006490     AT END
006500         MOVE "YES" TO �I���t���O
006510     END-READ.
006520*===============================================================*
006530 ��ƃ��R�[�h�Q�Z�b�g SECTION.
006540*
           MOVE ��P�|���Z���       TO ��Q�|���Z���.
006550     MOVE ��P�|�{�p�a��       TO ��Q�|�{�p�a��.
006560     MOVE ��P�|�{�p�N         TO ��Q�|�{�p�N.
006570     MOVE ��P�|�{�p��         TO ��Q�|�{�p��.
006580     MOVE ��P�|���҃R�[�h     TO ��Q�|���҃R�[�h.
006590     MOVE ��P�|�ی����       TO ��Q�|�ی����
006600     MOVE ��P�|���Ҏ���       TO ��Q�|���Ҏ���.
006610     MOVE ��P�|��ی��Ҏ���   TO ��Q�|��ی��Ҏ���.
006610     IF ��P�|�{�l�Ƒ��敪 = 1
006620         MOVE "�{�l"           TO ��Q�|�{�l�Ƒ�
006630     ELSE
006640         MOVE "�Ƒ�"           TO ��Q�|�{�l�Ƒ�
006650     END-IF.
007592     MOVE ��P�|�ی��Ҕԍ����K TO ��Q�|�����ی��Ҕԍ�.
           MOVE ��P�|��p�z         TO ��Q�|��p�z.
           MOVE ��P�|�����z         TO ��Q�|�����z.
           MOVE ��P�|���S�z         TO ��Q�|���S�z.
005210     MOVE ��P�|���Z����敪   TO ��Q�|���Z����敪.
           IF ��P�|���Z��� = 7
              MOVE 1                 TO ��Q�|�ی��敪
006670        COMPUTE ���ۏ��Ԃv  = ���ۏ��Ԃv + 1
006680        MOVE ���ۏ��Ԃv            TO ��Q�|����
           ELSE
              MOVE ZERO              TO ��Q�|�ی��敪
006670        COMPUTE ���Ԃv  = ���Ԃv + 1
006680        MOVE ���Ԃv            TO ��Q�|����
           END-IF.
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
007370*================================================================*
007380 ���я��t�@�C���쐬 SECTION.
007390*
007400     CLOSE ��ƃt�@�C���P.
007410     OPEN INPUT ��ƃt�@�C���P.
007420         MOVE NC"��P" TO �t�@�C����.
007430         PERFORM �I�[�v���`�F�b�N.
007440     OPEN OUTPUT ��ƃt�@�C���R.
007450         MOVE NC"��R" TO �t�@�C����.
007460         PERFORM �I�[�v���`�F�b�N.
007470*
007480     MOVE SPACE TO �I���t���O.
007490     MOVE ZERO  TO ���Ԃv.      
007500     PERFORM ��ƃt�@�C���P�Ǎ�.
007510     PERFORM UNTIL �I���t���O = "YES"
007520             PERFORM ��R���R�[�h�Z�b�g
007530             PERFORM ��R�t�@�C������
007540             PERFORM ��ƃt�@�C���P�Ǎ�
007550     END-PERFORM.
007560
007570*================================================================*
007580 ��R���R�[�h�Z�b�g SECTION.
007590*
007600     MOVE ��P�|�{�p�a��       TO ��R�|�{�p�a��.
007610     MOVE ��P�|�{�p�N         TO ��R�|�{�p�N.
007620     MOVE ��P�|�{�p��         TO ��R�|�{�p��.
007630     MOVE ��P�|���҃R�[�h     TO ��R�|���҃R�[�h.
007640     MOVE ��P�|�ی����       TO ��R�|�ی����.
           IF ��P�|���Z��� = 7
007650        COMPUTE ���ۏ��Ԃv  = ���ۏ��Ԃv + 1
007660        MOVE ���ۏ��Ԃv        TO ��R�|����
           ELSE
007650        COMPUTE ���Ԃv  = ���Ԃv + 1
007660        MOVE ���Ԃv            TO ��R�|����
           END-IF.
007670*
007680*================================================================*
007690 ��R�t�@�C������ SECTION.
007700*
007710     WRITE ��R�|���R�[�h
007720     INVALID KEY
007730         MOVE NC"��R"  TO �t�@�C����
007740         PERFORM �G���[�\��
007750     END-WRITE.
006790*================================================================*
006800 �t�@�C���� SECTION.
006810*
006820     CLOSE ��f�ҏ��e ���Z�v�g�e ������}�X�^
006830           ������}�X�^ �s�����}�X�^ �ی��҃}�X�^
                 ���ۏ��e.
006840*================================================================*
006850 �I������ SECTION.
006860*
006870     PERFORM �t�@�C����.
007670*================================================================*
007671*================================================================*
007672******************************************************************
007673 END PROGRAM YHN6061.
007674******************************************************************
