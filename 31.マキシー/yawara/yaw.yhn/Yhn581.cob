000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN581.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*      �����䒠�y�ް��쐬�z�_+����޳�ޔ�
000100* �����N��Ver.�̂�
000110*      MED = YHN580  YHN582P
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2014-09-05
000140 DATE-COMPILED.          2014-09-05
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
000027     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000028                             ORGANIZATION             IS  INDEXED
000029                             ACCESS MODE              IS  DYNAMIC
000030                             RECORD KEY               IS  ���|����敪
000031                             FILE STATUS              IS  ��ԃL�[
000032                             LOCK        MODE         IS  AUTOMATIC.
000420     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS ��|�{�p�a��N��
000460                                                          ��|���҃R�[�h
000470                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000480                                                          ��|���҃J�i
000490                                                          ��|���҃R�[�h
000500                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000510                                                         ��|�{�p�a��N��
000520                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000530                                                          ��|�ی����
000540                                                          ��|�ی��Ҕԍ�
000550                                                          ��|���҃R�[�h
000560                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000570                                                          ��|������
000580                                                     ��|��p���S�Ҕԍ�
000590                                                          ��|���҃R�[�h
000600                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000610                                                          ��|�������
000620                                                  ��|��p���S�Ҕԍ�����
000630                                                          ��|���҃R�[�h
000640                             ALTERNATE RECORD KEY  IS ��|�����a��N��
000650                                                      ��|�{�p�a��N��
000660                                                      ��|���҃R�[�h
000670                             FILE STATUS              IS  ��ԃL�[
000680                             LOCK        MODE         IS  AUTOMATIC.
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
000771*
000902     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5801L.DAT"
000903                             ORGANIZATION             IS  INDEXED
000904                             ACCESS                   IS  DYNAMIC
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
000913                             FILE        STATUS       IS  ��ԃL�[
000914                             LOCK        MODE         IS  AUTOMATIC.
000930* ���я��p
000931     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5802L.DAT"
000932                             ORGANIZATION             IS  INDEXED
000940                             ACCESS                   IS  DYNAMIC
000950                             RECORD      KEY          IS  ��Q�|�{�p�a��N��
000960                                                          ��Q�|���҃R�[�h
000970                                                          ��Q�|�ی����
000971                             ALTERNATE RECORD KEY     IS  ��Q�|����
000980                             FILE        STATUS       IS  ��ԃL�[
000990                             LOCK        MODE         IS  AUTOMATIC.
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
      */���я��p�@�V��
000108     SELECT  ��ƃt�@�C���S  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4314L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  ��S�|���ރR�[�h
000112                                                          ��S�|���R�[�h
000112                                                          ��S�|�ی���
000113                                                          ��S�|�ی��Ҕԍ�
000980                             FILE        STATUS       IS  ��ԃL�[
000990                             LOCK        MODE         IS  AUTOMATIC.
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
001000*
001010******************************************************************
001020*                      DATA DIVISION                             *
001030******************************************************************
001040 DATA                    DIVISION.
001050 FILE                    SECTION.
000126*                           �m�q�k��  �Q�T�U�n
000127 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
000128     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000991     COPY SEIGYO02        OF  XFDLIB  JOINING   ���O�Q   AS  PREFIX.
001120*                           �m�q�k��  �R�Q�O�n
001130 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001140     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001180*
001181****
001190 FD  ��ƃt�@�C���P RECORD  CONTAINS 114 CHARACTERS.
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
001380     03  ��P�|���R�[�h�f�[�^.
001400         05  ��P�|�ی����                PIC 9(2).
001271         05  ��P�|�ی��Ҕԍ����K          PIC X(8).
               05  ��P�|��p�z                  PIC 9(6).
               05  ��P�|���S�z                  PIC 9(6).
               05  ��P�|�����z                  PIC 9(6).
001260         05  ��P�|�Ԗߋ敪                PIC 9.
001459*
001581 FD  ��ƃt�@�C���Q RECORD  CONTAINS 64 CHARACTERS.
001582 01  ��Q�|���R�[�h.
001583     03  ��Q�|���R�[�h�L�[.
001584         05  ��Q�|�{�p�a��N��.
001585             07  ��Q�|�{�p�a��            PIC 9.
001586             07  ��Q�|�{�p�N              PIC 9(2).
001587             07  ��Q�|�{�p��              PIC 9(2).
001588         05  ��Q�|���҃R�[�h.
001589             07 ��Q�|���Ҕԍ�             PIC 9(6).
001590             07 ��Q�|�}��                 PIC X(1).
001591         05  ��Q�|�ی����                PIC 9(2).
001592     03  ��Q�|���R�[�h�f�[�^.
001593         05  ��Q�|�Ԗߋ敪                PIC 9.
001594         05  ��Q�|���ރR�[�h              PIC 9(1).
001261         05  ��Q�|���R�[�h                PIC X(2).
001400         05  ��Q�|�ی���                  PIC 9(2).
001595         05  ��Q�|����                    PIC 9(4).
001596         05  ��Q�|�{�l�Ƒ��敪            PIC 9.
001597         05  ��Q�|�ی��Ҕԍ�              PIC X(8).
001591         05  ��Q�|���S����                PIC 9(2).
               05  ��Q�|��p�z                  PIC 9(6).
               05  ��Q�|���S�z                  PIC 9(6).
               05  ��Q�|�����z                  PIC 9(6).
001598         05  FILLER                        PIC X(11).
001599*
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
000173*
000174*                           �m�q�k��  �R�Q�n
000175 FD  ��ƃt�@�C���S RECORD  CONTAINS 32 CHARACTERS.
000176 01  ��S�|���R�[�h.
000177     03  ��S�|���R�[�h�L�[.
001261         05  ��S�|���ރR�[�h                PIC 9(1).
001261         05  ��S�|���R�[�h                  PIC X(2).
001400         05  ��S�|�ی���                    PIC 9(2).
000183         05  ��S�|�ی��Ҕԍ�                PIC X(10).
000188     03  ��S�|���R�[�h�f�[�^.
001261         05  ��S�|�U������                  PIC 9(3).
001261         05  ��S�|�V������                  PIC 9(3).
000201         05  FILLER                          PIC X(11).
000173*
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
001600*----------------------------------------------------------------*
001601******************************************************************
001610*                WORKING-STORAGE SECTION                         *
001620******************************************************************
001630 WORKING-STORAGE         SECTION.
001640 01 �L�[����                           PIC X    VALUE SPACE.
001650 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
001660 01 �I���t���O                         PIC X(3) VALUE SPACE.
001670 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
001680 01 ���s�L�[�v                         PIC X(3) VALUE SPACE.
001690 01 �{�p�L�^�L�v                       PIC X(3) VALUE SPACE.
001700 01 �t�@�C����                         PIC N(8) VALUE SPACE.
001710 01 �J�E���^                           PIC 9(2) VALUE ZERO.
001720 01 �J�E���^�Q                         PIC 9(2) VALUE ZERO.
001730 01 �ΏۊO�t���O                       PIC X(3) VALUE SPACE.
001740 01 ���ރt���O                         PIC X(3) VALUE SPACE.
001750 01 �쐬�t���O                         PIC X(3) VALUE SPACE.
001760 01 �E�o�t���O                         PIC X(3) VALUE SPACE.
001770 01 ���S�����v                         PIC 9    VALUE ZERO.
001780 01 ���Ԃv                             PIC 9(4) VALUE ZERO.
001781 01 �������R�[�h�t���O                 PIC X(3) VALUE SPACE.
001782 01 �ی���ʂv                         PIC 9(2) VALUE ZERO.
001790*
001800** �ޔ�p
001801 01 �Ԗߋ敪�v�q                       PIC 9 VALUE ZERO.
001802 01 ���ރR�[�h�v�q                     PIC 9 VALUE ZERO.
001830 01 �����a��N���v�q.
001840    03 �����a��v�q                    PIC 9    VALUE ZERO.
001850    03 �����N�v�q                      PIC 9(2) VALUE ZERO.
001860    03 �������v�q                      PIC 9(2) VALUE ZERO.
001790*
001800** ���v�p
       01 �W�v�v.
          03 ���������v                      PIC 9(4) VALUE ZERO.
          03 �������v�z�v                    PIC 9(10) VALUE ZERO.
          03 �������S���z�v                  PIC 9(10) VALUE ZERO.
          03 �����������z�v                  PIC 9(10) VALUE ZERO.
          03 �Ԗߌ����v                      PIC 9(4) VALUE ZERO.
          03 �Ԗߍ��v�z�v                    PIC 9(10) VALUE ZERO.
          03 �Ԗߕ��S���z�v                  PIC 9(10) VALUE ZERO.
          03 �Ԗߐ������z�v                  PIC 9(10) VALUE ZERO.
          03 �������v                        PIC 9(4) VALUE ZERO.
          03 �����������v                    PIC 9(4) VALUE ZERO.
          03 �����v�z�v                      PIC 9(10) VALUE ZERO.
          03 �����S���z�v                    PIC 9(10) VALUE ZERO.
          03 ���������z�v                    PIC 9(10) VALUE ZERO.
          03 ���������z�v                    PIC 9(10) VALUE ZERO.
001861*
001572* �����p
001261 01 ���ރR�[�h�v                       PIC 9(1) VALUE ZERO.
001261 01 ���R�[�h�v                         PIC X(2) VALUE SPACE.
001400 01 �ی����v                           PIC 9(2) VALUE ZERO.
001400 01 �U�����Ԃv                         PIC 9(3) VALUE ZERO.
001400 01 �V�����Ԃv                         PIC 9(3) VALUE ZERO.
001400 01 ���ҏ��Ԃv                         PIC 9(3) VALUE ZERO.
001320 01 �ی��Ҕԍ��v�q.
001530    03 �@�ʔԍ��v�q                    PIC X(2)  VALUE SPACE.
001540    03 �۔Ԃv�q.
001550       05 �s���{���ԍ��v�q             PIC X(2)  VALUE SPACE.
001560       05 �ی��ԍ��v�q                 PIC X(3)  VALUE SPACE.
001570       05 ���ؔԍ��v�q                 PIC X     VALUE SPACE.
001580       05 FILLER                       PIC X(2)  VALUE SPACE.
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
001582*
001870** �G���[���b�Z�[�W�p
001880 01 �G���[���b�Z�[�W�v.
001890    03 �G���[���҃R�[�h�v              PIC X(7) VALUE SPACE.
001900    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
001910    03 �G���[�ی���ʂv                PIC X(2) VALUE SPACE.
001920    03 FILLER                          PIC X(10) VALUE SPACE.
001930*
001940*****************************************************************
001950 01 �v�Z�@����N�v                     PIC 9(2).
001960* ���t�v�n�q�j
001970 01 �v�Z�@����.
001980    03 �v�Z�@����N                    PIC 9(4).
001990    03 �v�Z�@�����                  PIC 9(4).
002000 01 �v�Z�@����q REDEFINES �v�Z�@����.
002010    03 �v�Z�@���I                      PIC 9(2).
002020    03 �v�Z�@���t                      PIC 9(6).
002030    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
002040       05 �v�Z�@�N��                   PIC 9(4).
002050       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
002060         07 �v�Z�@�N                   PIC 9(2).
002070         07 �v�Z�@��                   PIC 9(2).
002080       05 �v�Z�@��                     PIC 9(2).
002090*
002100******************************************************************
002110*                          �A������                              *
002120******************************************************************
002130*
002140********************
002150* ���b�Z�[�W�\���L�[ *
002160********************
002170 01 �A���|�L�[ IS EXTERNAL.
002180    03  �A���|���b�Z�[�W               PIC N(20).
002190*
002200 01 �A���R�|�L�[ IS EXTERNAL.
002210    03  �A���R�|���b�Z�[�W             PIC N(20).
002220    03  �A���R�|���b�Z�[�W�P           PIC X(20).
002230****************
002240* ��ʓ��͏�� *
002250****************
002260 01 �A���|��ʏ��x�g�m�T�W�O IS EXTERNAL.
002270    03 �A���|�����a��N��.
002280       05 �A���|�����a��               PIC 9.
002290       05 �A���|�����N��.
002300         07 �A���|�����N               PIC 9(2).
002310         07 �A���|������               PIC 9(2).
          03 �A���|�v���r���[�敪             PIC 9.
002320*
002260 01 �A��|������x�g�m�T�W�P IS EXTERNAL.
           03 �A��|��������                 PIC 9(4).
           03 �A��|�������v�z               PIC 9(8).
           03 �A��|�������S���z             PIC 9(8).
           03 �A��|�����������z             PIC 9(8).
           03 �A��|�Ԗߌ���                 PIC 9(4).
           03 �A��|�Ԗߍ��v�z               PIC 9(8).
           03 �A��|�Ԗߕ��S���z             PIC 9(8).
           03 �A��|�Ԗߐ������z             PIC 9(8).
           03 �A��|������                   PIC 9(4).
           03 �A��|�����v�z                 PIC 9(8).
           03 �A��|�����S���z               PIC 9(8).
           03 �A��|���������z               PIC 9(8).
002363*
002660******************************************************************
002670*                      PROCEDURE  DIVISION                       *
002680******************************************************************
002690 PROCEDURE               DIVISION.
002700************
002710*           *
002720* ��������   *
002730*           *
002740************
002750     PERFORM ������.
002260     PERFORM ������擾.
002760************
002770*           *
002780* �又��     *
002790*           *
002800************
002810     PERFORM ��ƃt�@�C���쐬.
002330     PERFORM ���я��t�@�C���쐬.
002820************
002830*           *
002840* �I������   *
002850*           *
002860************
002870     PERFORM �I������.
002880     MOVE ZERO TO PROGRAM-STATUS.
002890     EXIT PROGRAM.
002900*
002910*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002920*================================================================*
002930 ������ SECTION.
002940*
002950     PERFORM �t�@�C���I�[�v��.
002960* �A�����ڂ̑Ҕ�
002970     MOVE �A���|�����a��      TO �����a��v�q.
002980     MOVE �A���|�����N        TO �����N�v�q.
002990     MOVE �A���|������        TO �������v�q.
003000*
003010*================================================================*
003020 �t�@�C���I�[�v�� SECTION.
003030*
002581     OPEN INPUT ������}�X�^.
002582         MOVE NC"������" TO �t�@�C����.
002583         PERFORM �I�[�v���`�F�b�N.
003070     OPEN INPUT ��f�ҏ��e.
003080         MOVE NC"��f�ҏ��e" TO �t�@�C����.
003090         PERFORM �I�[�v���`�F�b�N.
003250     OPEN INPUT ���Z�v�g�e.
003260         MOVE NC"���Z�v�g�e" TO �t�@�C����.
003270         PERFORM �I�[�v���`�F�b�N.
003121*
003130*================================================================*
003140 �I�[�v���`�F�b�N SECTION.
003150*
003160     IF ��ԃL�[  NOT =  "00"
003170         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
003180         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
003190         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003200                                                 UPON CONS
003131*-----------------------------------------*
003132         CALL "actcshm"  WITH C LINKAGE
003133*-----------------------------------------*
003210         ACCEPT  �L�[���� FROM CONS
003220         PERFORM �t�@�C����
003230         MOVE 99 TO PROGRAM-STATUS
003240         EXIT PROGRAM.
003250*================================================================*
003260 �t�@�C���� SECTION.
003270*
003280     CLOSE ������}�X�^ ��f�ҏ��e ���Z�v�g�e.
003290*================================================================*
003300 �I������ SECTION.
003310*
003320     PERFORM �t�@�C����.
003330*================================================================*
003340 �G���[�\�� SECTION.
003350*
003360     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
003370     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
003380     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
003390     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
003400     ACCEPT  �L�[���� FROM CONS.
003410     PERFORM �t�@�C����.
003420     MOVE 99 TO PROGRAM-STATUS.
003430     EXIT PROGRAM.
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
002740*
003440*================================================================*
003450 ��ƃt�@�C���쐬 SECTION.
003460*
003470     OPEN OUTPUT ��ƃt�@�C���P.
003480         MOVE NC"��P" TO �t�@�C����.
003490         PERFORM �I�[�v���`�F�b�N.
003500*
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
003620     END-START.
003630     IF ��ԃL�[ = "00"
003640         MOVE SPACE  TO �I���t���O
003650         PERFORM ���Z�v�g�e�Ǎ�
003660         PERFORM UNTIL ( �I���t���O = "YES" ) OR
005330                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
005340                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
005350                       ( ���Z�|������   NOT = �������v�q   )
003730*
004010             PERFORM �f�[�^�`�F�b�N
004470** �J�ЁE�����ӁE���R�E���ےP�Ƃ͑ΏۊO
003790             IF  ���Z�|���Z��� = 4 OR 5 OR 6 OR 7 OR 8
004040                MOVE SPACE  TO ���s�L�[�v
004050             END-IF
003737             IF  ���s�L�[�v = "YES"
003738                 PERFORM ���R�[�h�Z�b�g
003739             END-IF
003850             PERFORM ���Z�v�g�e�Ǎ�
003860         END-PERFORM
003870     END-IF.
           PERFORM ������Z�b�g.
003880*
003890     CLOSE ��ƃt�@�C���P.
003900*
003910******
003920     OPEN OUTPUT ��ƃt�@�C���Q.
003930         MOVE NC"��Q" TO �t�@�C����.
003940         PERFORM �I�[�v���`�F�b�N.
003950     OPEN INPUT ��ƃt�@�C���P.
003960         MOVE NC"��P" TO �t�@�C����.
003970         PERFORM �I�[�v���`�F�b�N.
003980*
003990* ���я��p�t�@�C���쐬
004000     PERFORM ��ƃt�@�C���쐬�Q.
004010*
004020     CLOSE ��ƃt�@�C���P ��ƃt�@�C���Q.
004030*
004031*****
004032*  �O�����`�F�b�N
004033     OPEN INPUT ��ƃt�@�C���Q.
004034         MOVE NC"��Q" TO �t�@�C����.
004035         PERFORM �I�[�v���`�F�b�N.
004036     READ ��ƃt�@�C���Q NEXT
004037     AT END
004039         MOVE  NC"�@�f�[�^������܂���B�m�F���ĉ������B" TO �A���|���b�Z�[�W
004040         CALL   "MSG001"
004041         CANCEL "MSG001"
004042     END-READ.
004043     CLOSE ��ƃt�@�C���Q.
004044*
004046*================================================================*
004050 �f�[�^�`�F�b�N SECTION.
004060*
004070     MOVE SPACE  TO ���s�L�[�v.
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
004530*
004540*================================================================*
004630*================================================================*
004640 ���Z�v�g�e�Ǎ� SECTION.
004650*
004660     READ ���Z�v�g�e NEXT
004670     AT END
004680         MOVE "YES" TO �I���t���O
004690     END-READ.
004700*
004780*================================================================*
004790 ���R�[�h�Z�b�g SECTION.
004800*
004801     MOVE SPACE TO �������R�[�h�t���O.
004803*
           IF ���Z�|���Z��� = 3
004844         MOVE "YES" TO �������R�[�h�t���O
           END-IF
004811     PERFORM ���R�[�h���׃Z�b�g.
004827*
005442*================================================================*
005443 ���R�[�h���׃Z�b�g SECTION.
005456*
005473     MOVE SPACE TO ��P�|���R�[�h.
005474     INITIALIZE ��P�|���R�[�h.
005475     MOVE ���Z�|�����a��     TO ��P�|�����a��.
005476     MOVE ���Z�|�����N       TO ��P�|�����N.
005477     MOVE ���Z�|������       TO ��P�|������.
005478     MOVE ���Z�|�{�p�a��     TO ��P�|�{�p�a��.
005479     MOVE ���Z�|�{�p�N       TO ��P�|�{�p�N.
005480     MOVE ���Z�|�{�p��       TO ��P�|�{�p��.
005480     MOVE ���Z�|���S����     TO ��P�|���S����.
005650     MOVE ���Z�|�����敪     TO ��P�|�Ԗߋ敪.
005481     MOVE ��|���҃R�[�h     TO ��P�|���҃R�[�h.
005485     MOVE ��|���҃J�i       TO ��P�|���҃J�i.
005527     MOVE ��|�{�l�Ƒ��敪   TO ��P�|�{�l�Ƒ��敪.
      *
           IF �������R�[�h�t���O  = "YES"
005517         MOVE ��|��p���S�Ҕԍ�����  TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
005518         MOVE ��|�������            TO ��P�|�ی����
005526         IF ��|������ = ZERO
                   MOVE ���Z�|�ꕔ���S��    TO ��P�|��p�z
               ELSE
                   MOVE ���Z�|�ꕔ���S��    TO ��P�|��p�z
               END-IF
               MOVE ���Z�|�󋋎ҕ��S�z      TO ��P�|���S�z
               MOVE ���Z�|�����������z      TO ��P�|�����z
004698         IF  ��P�|�����z NOT = ZERO
                  PERFORM �����W�v
               END-IF
005542         PERFORM ���ގ擾
005524     ELSE
005526         IF ��|������ = ZERO
005531             MOVE ��|�ی��Ҕԍ�      TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
005532             MOVE ��|�ی����        TO ��P�|�ی����
                   MOVE ���Z�|���v          TO ��P�|��p�z
                   MOVE ���Z�|�ꕔ���S��    TO ��P�|���S�z
                   MOVE ���Z�|�������z      TO ��P�|�����z
004698             IF  ��P�|�����z NOT = ZERO
                      PERFORM ���ۏW�v
                  END-IF
005533         ELSE
005534             MOVE 1                   TO ��P�|�{�l�Ƒ��敪
005538             MOVE ��|��p���S�Ҕԍ�  TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ����K
005539             MOVE ��|������        TO ��P�|�ی����
                   MOVE ���Z�|���v          TO ��P�|��p�z
                   MOVE ���Z�|�ꕔ���S��    TO ��P�|���S�z
                   MOVE ���Z�|�������z      TO ��P�|�����z
004698             IF  ��P�|�����z NOT = ZERO
                      PERFORM ���ۏW�v
                  END-IF
005540         END-IF
005541*
005542         PERFORM ���ގ擾
005543*
005545     END-IF.
005564*
004697* �����z�� ZERO �̎��́A�������܂Ȃ��B
004698     IF  ��P�|�����z NOT = ZERO
005656         WRITE ��P�|���R�[�h
005657         IF ��ԃL�[  NOT =  "00"
005658             MOVE NC"��P"  TO �t�@�C����
005659             PERFORM �G���[�\��
               END-IF
005660     END-IF.
005661*
005704*================================================================*
005706 ���ۏW�v SECTION.
      *
           IF ���Z�|�����敪 NOT = 2
              COMPUTE ���������v     = ���������v     + 1
              COMPUTE �������v�z�v   = �������v�z�v   + ���Z�|���v      
              COMPUTE �������S���z�v = �������S���z�v + ���Z�|�ꕔ���S��
              COMPUTE �����������z�v = �����������z�v + ���Z�|�������z  
           ELSE
              COMPUTE �Ԗߌ����v     = �Ԗߌ����v     + 1
              COMPUTE �Ԗߍ��v�z�v   = �Ԗߍ��v�z�v   + ���Z�|���v      
              COMPUTE �Ԗߕ��S���z�v = �Ԗߕ��S���z�v + ���Z�|�ꕔ���S��
              COMPUTE �Ԗߐ������z�v = �Ԗߐ������z�v + ���Z�|�������z  
           END-IF.
      *
           COMPUTE �������v     = �������v     + 1.
           COMPUTE �����v�z�v   = �����v�z�v   + ���Z�|���v      .
           COMPUTE �����S���z�v = �����S���z�v + ���Z�|�ꕔ���S��.
           COMPUTE ���������z�v = ���������z�v + ���Z�|�������z  .
      *
005704*================================================================*
005706 �����W�v SECTION.
      *
           IF ���Z�|�����敪 NOT = 2
              COMPUTE ���������v     = ���������v     + 1
              COMPUTE �����������z�v = �����������z�v + ���Z�|�����������z  
           ELSE
              COMPUTE �Ԗߌ����v     = �Ԗߌ����v     + 1
              COMPUTE �Ԗߐ������z�v = �Ԗߐ������z�v + ���Z�|�����������z  
           END-IF.
      *
           COMPUTE �������v     = �������v     + 1.
           COMPUTE ���������z�v = ���������z�v + ���Z�|�����������z  .
      *
005704*================================================================*
005706 ������Z�b�g SECTION.
      *
           MOVE ���������v     TO �A��|��������.
           MOVE �������v�z�v   TO �A��|�������v�z.
           MOVE �������S���z�v TO �A��|�������S���z.
           MOVE �����������z�v TO �A��|�����������z.
           MOVE �Ԗߌ����v     TO �A��|�Ԗߌ���.
           MOVE �Ԗߍ��v�z�v   TO �A��|�Ԗߍ��v�z.
           MOVE �Ԗߕ��S���z�v TO �A��|�Ԗߕ��S���z.
           MOVE �Ԗߐ������z�v TO �A��|�Ԗߐ������z.
           MOVE �������v       TO �A��|������.
           MOVE �����v�z�v     TO �A��|�����v�z.
           MOVE �����S���z�v   TO �A��|�����S���z.
           MOVE ���������z�v   TO �A��|���������z.
      *     MOVE �����������v   TO �A��|����������.
      *     COMPUTE �A��|�����ܐ������z = ���������z�v + ���������z�v.
      *
005705*================================================================*
005706 ���ގ擾 SECTION.
005707***************************************************
005708* ���ރR�[�h
005709*  1:�ЕہE���فE�D��
      *  2:�D��
005710*  3:�g��
      *  4:���ρE���q��
005711*  5:1:���� 1:�ސE 2:�㍂ 3:�����i�����E���m�E���m�ȊO�j
005712*  6:�����i�����E���m�E���m�j
005716*     /15.6.18 ���s�̌������̏�Q��"39261"����������
005716*
      *  ���ŗL
      *  ���
      *  5:10:�㍂ 11:�����i�e���㍂�j 20:���� 20:�ސE 21:�����i�e�ی����㍂�ȊO�E�����E���m�E���m�ȊO�j
      *       
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
005740                 MOVE 3 TO  ��P�|���ރR�[�h
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
005809*================================================================*
005810 ��ƃt�@�C���쐬�Q SECTION.
005811*
005813     MOVE SPACE TO �I���t���O.
005814     MOVE ZERO  TO ���Ԃv.      
005815     PERFORM ��ƃt�@�C���P�Ǎ�.
005818     PERFORM UNTIL �I���t���O = "YES"
005819         PERFORM ��Q���R�[�h�Z�b�g
005820         PERFORM ��Q�t�@�C������
005821*
005824         PERFORM ��ƃt�@�C���P�Ǎ�
005825     END-PERFORM.
005826*
005843*================================================================*
005844 ��Q���R�[�h�Z�b�g SECTION.
005845*
005851     COMPUTE ���Ԃv  = ���Ԃv + 1.
005852     MOVE ���Ԃv  TO ��Q�|����.
005853*
005854     MOVE ��P�|�{�p�a��       TO ��Q�|�{�p�a��.
005855     MOVE ��P�|�{�p�N         TO ��Q�|�{�p�N.
005856     MOVE ��P�|�{�p��         TO ��Q�|�{�p��.
005857     MOVE ��P�|���҃R�[�h     TO ��Q�|���҃R�[�h.
005858     MOVE ��P�|�ی����       TO ��Q�|�ی����.
005859     MOVE ��P�|�Ԗߋ敪       TO ��Q�|�Ԗߋ敪.
005860     MOVE ��P�|���ރR�[�h     TO ��Q�|���ރR�[�h.
005860     MOVE ��P�|���R�[�h       TO ��Q�|���R�[�h.
005860     MOVE ��P�|�ی���         TO ��Q�|�ی���.
005861     MOVE ��P�|�{�l�Ƒ��敪   TO ��Q�|�{�l�Ƒ��敪.
005862     MOVE ��P�|�ی��Ҕԍ����K TO ��Q�|�ی��Ҕԍ�.
005859     MOVE ��P�|���S����       TO ��Q�|���S����.
005859     MOVE ��P�|��p�z         TO ��Q�|��p�z.
005859     MOVE ��P�|���S�z         TO ��Q�|���S�z.
005859     MOVE ��P�|�����z         TO ��Q�|�����z.
005863*
005886*================================================================*
005887 ��Q�t�@�C������ SECTION.
005888*
005889     WRITE ��Q�|���R�[�h
005890     INVALID KEY
005891         MOVE NC"��Q"  TO �t�@�C����
005892         PERFORM �G���[�\��
005893     END-WRITE.
005894*================================================================*
005895 ��ƃt�@�C���P�Ǎ� SECTION.
005896*
005897     READ ��ƃt�@�C���P NEXT
005898     AT END
005899         MOVE "YES" TO �I���t���O
005900     END-READ.
005901*
002850*================================================================*
002860 ���я��t�@�C���쐬 SECTION.
002870*
005610     OPEN INPUT  ��ƃt�@�C���P.
005620     OPEN OUTPUT ��ƃt�@�C���R.
005620     OPEN OUTPUT ��ƃt�@�C���S.
005620     OPEN OUTPUT ��ƃt�@�C���T.
005630*
005813     MOVE SPACE TO �I���t���O.
           MOVE ZERO  TO �U�����Ԃv.
005940*
005950     PERFORM ��ƃt�@�C���P�Ǎ�.
006020     PERFORM UNTIL �I���t���O = "YES"
               MOVE ZERO  TO �V�����Ԃv
006120         MOVE ��P�|���ރR�[�h   TO ��R�|���ރR�[�h ��S�|���ރR�[�h ��T�|���ރR�[�h ���ރR�[�h�v
006120         MOVE ��P�|���R�[�h     TO ��R�|���R�[�h ��S�|���R�[�h ���R�[�h�v
006120         MOVE ��P�|�ی��Ҕԍ�   TO ��S�|�ی��Ҕԍ� �ی��Ҕԍ��v�q
006120         MOVE ��P�|�ی���       TO ��R�|�ی��� �ی����v
               IF ��P�|���ރR�[�h = 3 OR 4
006120            MOVE ��P�|�ی��Ҕԍ�   TO ��R�|�ی��Ҕԍ�
               ELSE
006120            MOVE SPACE              TO ��R�|�ی��Ҕԍ�
               END-IF
006120         COMPUTE �U�����Ԃv = �U�����Ԃv + 1
               MOVE �U�����Ԃv         TO ��R�|�U������ ��S�|�U������ ��T�|�U������
006170         PERFORM ��ƃt�@�C���R����
               PERFORM UNTIL (�I���t���O         = "YES") OR
                             (��P�|���ރR�[�h NOT = ���ރR�[�h�v) OR
                             (((��P�|���R�[�h = SPACE) AND (��P�|�ی��Ҕԍ� NOT = �ی��Ҕԍ��v�q)) OR
                             ((��P�|���R�[�h NOT = SPACE) AND (��P�|���R�[�h   NOT = ���R�[�h�v))) OR
                             ((��P�|���ރR�[�h = 5) AND (��P�|�ی���(1:1) NOT = �ی����v(1:1)))
                   MOVE ZERO  TO ���ҏ��Ԃv
006120             MOVE ��P�|�ی���         TO ��S�|�ی��� �ی����v
006120             MOVE ��P�|�ی��Ҕԍ�     TO ��S�|�ی��Ҕԍ� �ی��Ҕԍ��v�q
006120             COMPUTE �V�����Ԃv = �V�����Ԃv + 1
                   MOVE �V�����Ԃv           TO ��S�|�V������ ��T�|�V������
006170             PERFORM ��ƃt�@�C���S����
                   PERFORM UNTIL (�I���t���O         = "YES") OR
                                 (��P�|�ی���(1:1)  NOT = �ی����v(1:1)) OR
                                 (��P�|�ی��Ҕԍ�   NOT = �ی��Ҕԍ��v�q)
006120                 MOVE ��P�|���҃R�[�h       TO ��T�|���҃R�[�h
006120                 MOVE ��P�|�{�p�a��N��     TO ��T�|�{�p�a��N��
006120                 MOVE ��P�|�ی����         TO ��T�|�ی����
006120                 COMPUTE ���ҏ��Ԃv = ���ҏ��Ԃv + 1
                       MOVE ���ҏ��Ԃv             TO ��T�|���ҏ���
006170                 PERFORM ��ƃt�@�C���T����
006140                 PERFORM ��ƃt�@�C���P�Ǎ�
006150             END-PERFORM
006150         END-PERFORM
006070*
006180     END-PERFORM.
005650*
005660     CLOSE ��ƃt�@�C���T.
005660     CLOSE ��ƃt�@�C���S.
005660     CLOSE ��ƃt�@�C���R.
005670     CLOSE ��ƃt�@�C���P.
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
006687*===============================================================*
006690 ��ƃt�@�C���R���� SECTION.
006700*
006710     WRITE ��R�|���R�[�h
006720     INVALID KEY
006730**         DISPLAY NC"��ԃL�[" ��ԃL�[  UPON MSGBOX
006740         MOVE NC"��R" TO �t�@�C����
006750         PERFORM �G���[�\��
006760         PERFORM �t�@�C����
006770         MOVE 99 TO PROGRAM-STATUS
006780         EXIT PROGRAM.
006687*===============================================================*
006690 ��ƃt�@�C���S���� SECTION.
006700*
006710     WRITE ��S�|���R�[�h
006720     INVALID KEY
006730**         DISPLAY NC"��ԃL�[" ��ԃL�[  UPON MSGBOX
006740         MOVE NC"��S" TO �t�@�C����
006750         PERFORM �G���[�\��
006760         PERFORM �t�@�C����
006770         MOVE 99 TO PROGRAM-STATUS
006780         EXIT PROGRAM.
006687*===============================================================*
006690 ��ƃt�@�C���T���� SECTION.
006700*
006710     WRITE ��T�|���R�[�h
006720     INVALID KEY
006730**         DISPLAY NC"��ԃL�[" ��ԃL�[  UPON MSGBOX
006740         MOVE NC"��T" TO �t�@�C����
006750         PERFORM �G���[�\��
006760         PERFORM �t�@�C����
006770         MOVE 99 TO PROGRAM-STATUS
006780         EXIT PROGRAM.
005910*================================================================*
005911******************************************************************
005912 END PROGRAM YHN581.
005913******************************************************************
