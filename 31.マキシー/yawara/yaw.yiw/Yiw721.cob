000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YIW721.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*         �{�p���ׁy�ް��쐬�z�_+����޳�ޔ�
000100*         MED = YIW720 
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2015-09-16
000130 DATE-COMPILED.          2015-09-16
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
000370     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000380                             ORGANIZATION             IS  INDEXED
000390                             ACCESS MODE              IS  DYNAMIC
000400                             RECORD KEY               IS  ���|�����敪
000410                             FILE STATUS              IS  ��ԃL�[
000420                             LOCK        MODE         IS  AUTOMATIC.
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
           SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  ���|�{�p�a��N��
                                                                ���|���҃R�[�h
                                   ALTERNATE RECORD KEY     IS  ���|���҃R�[�h
                                                                ���|�{�p�a��N��
                                   FILE STATUS              IS  ��ԃL�[
                                   LOCK        MODE         IS  AUTOMATIC.
           SELECT  �{�p�L�^�e      ASSIGN      TO      SEKIROKL
                                   ORGANIZATION        IS  INDEXED
                                   ACCESS MODE         IS  DYNAMIC
                                   RECORD KEY          IS  �{�L�|�{�p�a��N����
                                                           �{�L�|���҃R�[�h
                                   ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
                                                           �{�L�|�{�p�a��N����
                                   FILE STATUS              IS  ��ԃL�[
                                   LOCK        MODE         IS  AUTOMATIC.
000400     SELECT  �����t�@�C��    ASSIGN      TO        MEMOL
000410                             ORGANIZATION             IS  INDEXED
000420                             ACCESS MODE              IS  DYNAMIC
000430                             RECORD KEY               IS  �����|����敪
                                                                �����|���҃R�[�h
                                                                �����|�{�p�a��N����
000360                             ALTERNATE RECORD KEY     IS  �����|����敪
                                                                �����|�{�p�a��N����
                                                                �����|���҃R�[�h
000360                             ALTERNATE RECORD KEY     IS  �����|���҃R�[�h
                                                                �����|�{�p�a��N����
                                                                �����|����敪
000440                             FILE STATUS              IS  ��ԃL�[
000450                             LOCK        MODE         IS  AUTOMATIC.
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
000690     SELECT  ��v�f�[�^�e    ASSIGN      TO        KAIKEIL
000700                             ORGANIZATION             IS  INDEXED
000710                             ACCESS MODE              IS  DYNAMIC
000089                             RECORD KEY               IS  ��|�{�p�a��N����
000090                                                          ��|���҃R�[�h
000092                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000093                                                          ��|�{�p�a��N����
000790                             FILE STATUS              IS  ��ԃL�[
000800                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  ��ƃt�@�C���P  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7211L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
                                   RECORD KEY               IS  ��P�|�{�p�a��N����
000620                             FILE        STATUS       IS  ��ԃL�[
000630                             LOCK        MODE         IS  AUTOMATIC.
000640******************************************************************
000650*                      DATA DIVISION                             *
000660******************************************************************
000670 DATA                    DIVISION.
000680 FILE                    SECTION.
001500*                           �m�q�k��  �P�Q�W�n
001510 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001520     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000690*                           �m�q�k��  �R�Q�O�n
000700 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
000710     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *                           �m�q�k��  �Q�T�U�n
       FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
          COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
      *                           �m�q�k��  �P�Q�W�n
       FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
           COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000600*                           �m�q�k��  �W�R�Q�n
000610 FD  �����t�@�C��        BLOCK CONTAINS 1     RECORDS.
000620     COPY MEMO           OF    XFDLIB JOINING ���� AS PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001060*                           �m�q�k��  �T�P�Q�n
001070 FD  ��v�f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001080     COPY KAIKEI     OF  XFDLIB  JOINING   ��   AS  PREFIX.
001310*****************
001320* ��ƃt�@�C���P *
001330*****************
001340*                         �m�q�k��  �P�U�O�n
001350 FD  ��ƃt�@�C���P RECORD  CONTAINS 160 CHARACTERS.
001360 01 ��P�|���R�[�h.
001370    03 ��P�|���R�[�h�L�[.
001535       05 ��P�|�{�p�a��N����.
001536          07 ��P�|�{�p�a��                PIC 9.
001537          07 ��P�|�{�p�N��.
001538             09 ��P�|�{�p�N               PIC 9(2).
001539             09 ��P�|�{�p��               PIC 9(2).
001540          07 ��P�|�{�p��                  PIC 9(2).
001490    03 ��P�|���R�[�h�f�[�^.
             05 ��P�|����.
001550          07 ��P�|��������.
                   09 ��P�|������               PIC 9(2).
                   09 ��P�|������               PIC 9(2).
001550          07 ��P�|������                  PIC 9(5).
001550          07 ��P�|�{�×�                  PIC 9(5).
001550          07 ��P�|�Č���                  PIC 9(5).
001550          07 ��P�|��×�                  PIC 9(5).
001550          07 ��P�|���×�                  PIC 9(5).
001550          07 ��P�|㪖@��                  PIC 9(5).
001550          07 ��P�|�d�×�                  PIC 9(5).
001551          07 ��P�|��p�z                  PIC 9(5).
001551          07 ��P�|�ꕔ���S��              PIC 9(5).
001551       05 ��P�|�R�����g                   PIC X(100).
001470       05 ��P�|����                       PIC 9(1).
001500       05 FILLER                           PIC X(3).
      *
000930*----------------------------------------------------------------*
000940******************************************************************
000950*                WORKING-STORAGE SECTION                         *
000960******************************************************************
000970 WORKING-STORAGE         SECTION.
000980 01 �L�[����                           PIC X    VALUE SPACE.
000990 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
001000 01 �I���t���O                         PIC X(3) VALUE SPACE.
002120 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
002120 01 �I���t���O�R                       PIC X(3) VALUE SPACE.
001010 01 �t�@�C����                         PIC N(2) VALUE SPACE.
001180 01 ���s�L�[�v                         PIC X(4) VALUE SPACE.
       01 �{�p�L�^�L�v                       PIC X(3) VALUE SPACE.
001540 01 �p���t���O                         PIC X(3) VALUE SPACE.
001540 01 �Ώۃt���O                         PIC X(3) VALUE SPACE.
001891 01 ���ʂb�m�s                         PIC 9    VALUE ZERO.
001891 01 �J�E���^                           PIC 9    VALUE ZERO.
001020* **************
001030* * ���ڑҔ�p 
001040* **************
001050 01 �Ҕ����ڂv�q.
001060    03 �{�p�a��N���v�q.
001070       05 �{�p�a��v�q                 PIC 9(1) VALUE ZERO.
001080       05 �{�p�N�v�q                   PIC 9(2) VALUE ZERO.
001090       05 �{�p���v�q                   PIC 9(2) VALUE ZERO.
001090    03 �{�p���v�q                      PIC 9(2) VALUE ZERO.
          03 �ŏI�ʉ@���v                    PIC 9(2) VALUE ZERO.
001090    03 �ŏI�ʉ@���v                    PIC 9(2) VALUE ZERO.
          03 �󗝌��v                        PIC 9(2) VALUE ZERO.
001090    03 �󗝓��v                        PIC 9(2) VALUE ZERO.
001090    03 �J�n���v�q                      PIC 9(2) VALUE ZERO.
001090    03 �I�����v�q                      PIC 9(2) VALUE ZERO.
001120    03 ���҃R�[�h�v�q.
001130       05 ���Ҕԍ��v�q                 PIC 9(6) VALUE ZERO.
001140       05 �}�Ԃv�q                     PIC X(1) VALUE SPACE.
001160*
001170 01 ����S�Ҕԍ��v.
001180    03 �@�ʔԍ��v                      PIC X(2) VALUE SPACE.
001190    03 FILLER                          PIC X(8) VALUE SPACE.
001530 01 �ޔ����ڂf�v.
001540   03 ���Z�v�g��ނv                 PIC X(4).
001550   03 ���Z�v�g��ނf�v               PIC X(4).
001560   03 ���Z�v�g��ʂf�v               PIC 9(2).
001580*
001590 01 ����.
001600    03 �������v�q                    PIC 9(6)  VALUE ZERO.
001610    03 �������Z���v�q                PIC 9(5)  VALUE ZERO.
001620    03 �x���v�q                      PIC 9     VALUE ZERO.
001630    03 �[��v�q                      PIC 9     VALUE ZERO.
001640    03 ���ԊO�v�q                    PIC 9     VALUE ZERO.
001650    03 �f�Î��v�q                    PIC 9(2)  VALUE ZERO.
001660    03 �f�Õ��v�q                    PIC 9(2)  VALUE ZERO.
          03 ���������k���v�q              PIC 9(4)  VALUE ZERO.
001670    03 �Č����v�q                    PIC 9(4)  VALUE ZERO.
001680    03 ���Ö�Ԃv�q                  PIC 9     VALUE ZERO.
001690    03 ���Ó�H�v�q                  PIC 9     VALUE ZERO.
001700    03 ���Ö\���v�q                  PIC 9     VALUE ZERO.
001710    03 ���É񐔂v�q                  PIC 9(2)  VALUE ZERO.
001720    03 ���Ë����v�q                  PIC 9(3)V9 VALUE ZERO.
001730    03 ���×��v�q                    PIC 9(6)  VALUE ZERO.
001740    03 ���É��Z���v�q                PIC 9(5)  VALUE ZERO.
001750    03 �d㪗��v�q                    PIC 9(4)  VALUE ZERO.
001760    03 ��p�z�v�q                    PIC 9(5)  VALUE ZERO.
001760    03 �ꕔ���S���v�q                PIC 9(5)  VALUE ZERO.
          03 ���׏����s���v�q              PIC 9(2)  VALUE ZERO.
          03 ���׏����s���v�q              PIC 9(3)  VALUE ZERO.
          03 �������v�q                    PIC 9(2)  VALUE ZERO OCCURS 3.
          03 �^�����v�q                    PIC 9(2)  VALUE ZERO OCCURS 5.
004330    03 �^���Ö@���v�q                PIC 9(4)  VALUE ZERO.
003530    03 �������q���Z���v�q            PIC 9(5)  VALUE ZERO.
          03 ��v                          PIC N(1)  VALUE SPACE.
          03 ���v                          PIC N(1)  VALUE SPACE.
          03 ���v                          PIC N(1)  VALUE SPACE.
001770    03 �{�p���񋟗��v�q            PIC 9(6)  VALUE ZERO.
001780    03 ���ʂv                        OCCURS 5.
001790       05 ���񏈒u���v�q             PIC 9(4)  VALUE ZERO.
001800       05 ��×��v�q                 PIC 9(4)  VALUE ZERO.
001810       05 ��㪗��v�q                 PIC 9(4)  VALUE ZERO.
001820       05 ��㪗��v�q                 PIC 9(4)  VALUE ZERO.
001830       05 �d�×��v�q                 PIC 9(4)  VALUE ZERO.
001840       05 ���ʌv�v�q                 PIC 9(4)  VALUE ZERO.
001850       05 �����v�q                   PIC 9     VALUE ZERO.
001860    03 �����v�q                      PIC 9(2)  OCCURS 5 VALUE ZERO.
001870    03 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
001880    03 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
001890    03 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
001900    03 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
001910    03 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
001890    03 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
001900    03 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
001900    03 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
001910    03 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
001920    03 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
001930    03 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
001940    03 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
001950    03 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
001960    03 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
001940    03 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
001950    03 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
001950    03 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
001960    03 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
001970    03 �d�×��R�O�v�q                PIC 9(4)  VALUE ZERO.
001980    03 �d�×��R�W�v�q                PIC 9(4)  VALUE ZERO.
001990    03 �d�×��S�O�v�q                PIC 9(4)  VALUE ZERO.
002000    03 �d�×��S�T�v�q                PIC 9(4)  VALUE ZERO.
002010    03 �d�×��S�W�v�q                PIC 9(4)  VALUE ZERO.
001990    03 �d�×��T�O�v�q                PIC 9(4)  VALUE ZERO.
002000    03 �d�×��T�Q�v�q                PIC 9(4)  VALUE ZERO.
002000    03 �d�×��T�T�v�q                PIC 9(4)  VALUE ZERO.
002010    03 �d�×��T�W�v�q                PIC 9(4)  VALUE ZERO.
002020    03 ��×��R�O�v�q                PIC 9(4)  VALUE ZERO.
002030    03 ��×��R�W�v�q                PIC 9(4)  VALUE ZERO.
002040    03 ��×��S�O�v�q                PIC 9(4)  VALUE ZERO.
002050    03 ��×��S�T�v�q                PIC 9(4)  VALUE ZERO.
002060    03 ��×��S�W�v�q                PIC 9(4)  VALUE ZERO.
002040    03 ��×��T�O�v�q                PIC 9(4)  VALUE ZERO.
002050    03 ��×��T�Q�v�q                PIC 9(4)  VALUE ZERO.
002050    03 ��×��T�T�v�q                PIC 9(4)  VALUE ZERO.
002060    03 ��×��T�W�v�q                PIC 9(4)  VALUE ZERO.
002070    03 ��㪗��v�v�q                  PIC 9(6)  VALUE ZERO.
002080    03 ��㪗��v�v�q                  PIC 9(6)  VALUE ZERO.
002070    03 㪖@���v�v�q                  PIC 9(6)  VALUE ZERO.
002080    03 ��×��v�v�q                  PIC 9(6)  VALUE ZERO.
002090    03 �d�×��v�v�q                  PIC 9(6)  VALUE ZERO.
002100*
002149** ���Z���i�̓��t�敪�p (0:�ŏI�ʉ@���A1:�������A9:�󎚂Ȃ�)
002150 01 ���Z�v�g���t�敪�v                 PIC 9 VALUE ZERO.
002151 01 ���Z�v�g���ғ��t�敪�v             PIC 9 VALUE ZERO.
002617** �������p
002618 01 �{�p����N�v                       PIC 9(4)  VALUE ZERO.
002619 01 ���v                               PIC 9(3)  VALUE ZERO.
002620 01 �]�v                               PIC 9(3)  VALUE ZERO.
002621*
001200******************************************************************
001210*                          �A������                              *
001220******************************************************************
001230*
003080****************
003090* ��ʓ��͏�� *
003100****************
003110 01 �A���|���̓f�[�^�x�h�v�V�Q�O IS EXTERNAL.
          03 �A���|�{�p�a��N��.
             05 �A���|�{�p�a��                  PIC 9(1).
             05 �A���|�{�p�N                    PIC 9(2).
             05 �A���|�{�p��                    PIC 9(2).
          03 �A���|�J�n���t.
             05 �A���|�J�n��                    PIC 9(2).
          03 �A���|�I�����t.
             05 �A���|�I����                    PIC 9(2).
          03 �A���|���҃R�[�h.
             05 �A���|���Ҕԍ�                  PIC 9(6).
             05 �A���|�}��                      PIC X(1).
          03 �A���|������[�h�e                 PIC 9(1).
          03 �A���|�������[�h                   PIC 9(1).
          03 �A���|�N�����[�h                   PIC 9(1).
          03 �A���|�����ƃ��[�h                 PIC 9(1).
          03 �A���|�R�����g���[�h               PIC 9(1).
          03 �A���|���v���[�h                   PIC 9(1).
          03 �A���|����i��                     PIC 9(2).
      */�A�����̎�������̗L��0302
          03 �A���|�������[�h                   PIC X(4).
      *
002830 01 �A��|���v�f�[�^�x�h�v�V�Q�O IS EXTERNAL.
          03 �A��|���Ҏ���                     PIC X(50).
          03 �A��|������                       PIC 9(6).
          03 �A��|��×�                       PIC 9(6).
          03 �A��|���×�                       PIC 9(6).
          03 �A��|㪖@��                       PIC 9(6).
          03 �A��|�d�×�                       PIC 9(6).
          03 �A��|��p�z                       PIC 9(7).
          03 �A��|���S�z                       PIC 9(7).
          03 �A��|���񋟗�                   PIC 9(6).
          03 �A��|��                           PIC N(1).
          03 �A��|��                           PIC N(1).
          03 �A��|��                           PIC N(1).
          03 �A��|�������q��                   PIC 9(6).
          03 �A��|���̑�                       PIC 9(6).
          03 �A��|���k��                       PIC 9(4).
          03 �A��|���ח�                       PIC 9(3).
          03 �A��|�R�����g.
             05 �A��|�R�����g�P                PIC X(100).
             05 �A��|�R�����g�Q                PIC X(100).
007810*
001390******************************************************************
001400*                      PROCEDURE  DIVISION                       *
001410******************************************************************
001420 PROCEDURE               DIVISION.
001430************
001440*           *
001450* ��������   *
001460*           *
001470************
001480     PERFORM ������.
           INITIALIZE �A��|���v�f�[�^�x�h�v�V�Q�O.
001490************
001500*           *
001510* �又��     *
001520*           *
001530************
      *
001540     PERFORM ��ƃt�@�C���쐬.
001550************
001560*           *
001570* �I������   *
001580*           *
001590************
001600     PERFORM �I������.
001610     MOVE ZERO TO PROGRAM-STATUS.
001620     EXIT PROGRAM.
001630*
001640*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
001650*================================================================*
001660 ������ SECTION.
001670*
001680     PERFORM �t�@�C���I�[�v��.
001690* �A�����ڂ̑Ҕ�
001700     INITIALIZE �Ҕ����ڂv�q.
001730     MOVE �A���|���Ҕԍ�      TO ���Ҕԍ��v�q.
001740     MOVE �A���|�}��          TO �}�Ԃv�q.
001750     MOVE �A���|�{�p�a��      TO �{�p�a��v�q.
001760     MOVE �A���|�{�p�N        TO �{�p�N�v�q.
001770     MOVE �A���|�{�p��        TO �{�p���v�q.
001770     MOVE �A���|�J�n��        TO �J�n���v�q.
001770     MOVE �A���|�I����        TO �I�����v�q.
      *
001790*================================================================*
001800 �t�@�C���I�[�v�� SECTION.
001810*
012140     OPEN INPUT   �����}�X�^
012150         MOVE NC"����" TO �t�@�C����.
012160         PERFORM �I�[�v���`�F�b�N.
001820     OPEN INPUT ��f�ҏ��e.
001830         MOVE NC"��f�ҏ��e" TO �t�@�C����.
001840         PERFORM �I�[�v���`�F�b�N.
           OPEN INPUT �����f�[�^�e.
               MOVE NC"�����f�[�^�e" TO �t�@�C����.
               PERFORM �I�[�v���`�F�b�N.
           OPEN INPUT �{�p�L�^�e.
               MOVE NC"�{�p�L�^�e"   TO �t�@�C����.
               PERFORM �I�[�v���`�F�b�N.
002780     OPEN INPUT �����t�@�C��.
002790         MOVE NC"����"         TO �t�@�C����.
002800         PERFORM �I�[�v���`�F�b�N.
007870     OPEN INPUT ���Z�v�g�e.
007880         MOVE NC"���Z"         TO �t�@�C����.
007890         PERFORM �I�[�v���`�F�b�N.
003060     OPEN INPUT ��v�f�[�^�e.
003070         MOVE NC"��v" TO �t�@�C����.
003080         PERFORM �I�[�v���`�F�b�N.
001850*================================================================*
001860 �I�[�v���`�F�b�N SECTION.
001870*
001880     IF ��ԃL�[  NOT =  "00"
001890         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
001900         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
001910         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
001920                                                 UPON CONS
003131*-----------------------------------------*
003132         CALL "actcshm"  WITH C LINKAGE
003133*-----------------------------------------*
001930         ACCEPT  �L�[���� FROM CONS
001940         PERFORM �t�@�C����
001950         MOVE 99 TO PROGRAM-STATUS
001960         EXIT PROGRAM.
001970*================================================================*
001980 �t�@�C���� SECTION.
001990*
002000     CLOSE �����}�X�^  ��f�ҏ��e �����f�[�^�e �{�p�L�^�e
                 �����t�@�C�� ���Z�v�g�e ��v�f�[�^�e.
002010*================================================================*
002020 �I������ SECTION.
002030*
002040     PERFORM �t�@�C����.
002050*================================================================*
002060 �G���[�\�� SECTION.
002070*
002080     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
002090     DISPLAY NC"�T�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
002100     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
002110     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
003131*-----------------------------------------*
003132     CALL "actcshm"  WITH C LINKAGE.
003133*-----------------------------------------*
002120     ACCEPT  �L�[���� FROM CONS.
002130     PERFORM �t�@�C����.
002140     MOVE 99 TO PROGRAM-STATUS.
002150     EXIT PROGRAM.
003960*================================================================*
003970 ��f�ҏ��e�Ǎ� SECTION.
003980*
003990     READ ��f�ҏ��e NEXT
004000     AT END
004010         MOVE "YES" TO �I���t���O
004020     END-READ.
002160*================================================================*
002170 ��ƃt�@�C���쐬 SECTION.
002180*
002210     OPEN OUTPUT ��ƃt�@�C���P
002220          MOVE NC"��P" TO �t�@�C����
002230          PERFORM �I�[�v���`�F�b�N
      *
003720     MOVE �{�p�a��v�q      TO ��|�{�p�a��.
003730     MOVE �{�p�N�v�q        TO ��|�{�p�N.
003740     MOVE �{�p���v�q        TO ��|�{�p��.
           MOVE ���Ҕԍ��v�q      TO ��|���Ҕԍ�.
           MOVE �}�Ԃv�q          TO ��|�}��.
003790     START ��f�ҏ��e   KEY IS >= ��|�{�p�a��N��
                                          ��|���҃R�[�h
003830     IF ��ԃL�[ = "00"
003850         PERFORM ��f�ҏ��e�Ǎ�
009060         PERFORM �f�[�^�`�F�b�N
009070         IF ���s�L�[�v = "YES"
009090             PERFORM VARYING �{�p���v�q FROM 1 BY 1 UNTIL �{�p���v�q > 31
009100                 INITIALIZE ����
                       MOVE ��|���Ҕԍ�  TO �{�L�|���Ҕԍ�
                       MOVE ��|�}��      TO �{�L�|�}��
                       MOVE ��|�{�p�a��  TO �{�L�|�{�p�a��
                       MOVE ��|�{�p�N    TO �{�L�|�{�p�N
                       MOVE ��|�{�p��    TO �{�L�|�{�p��
                       MOVE �{�p���v�q    TO �{�L�|�{�p��
                       READ �{�p�L�^�e
                       NOT INVALID KEY
008270                     INITIALIZE ��P�|���R�[�h
                           MOVE �{�L�|��t��       TO ��P�|������
                           MOVE �{�L�|��t��       TO ��P�|������
                           IF �{�L�|�f�Ë敪 = 2
                               MOVE 1              TO ��P�|����
                           ELSE
                               MOVE 0              TO ��P�|����
                           END-IF
009110                     PERFORM ���Z�v�g�ďo���Q
009130                     PERFORM ���ڂ��ƌv�Z
009140                     PERFORM ��ƃt�@�C���Z�b�g
009150                     PERFORM ��P���R�[�h����
                       END-READ
009170             END-PERFORM
009160             PERFORM ���v�v�Z
009190         END-IF
003950     END-IF.
002810     CLOSE ��ƃt�@�C���P.
012730*================================================================*
012740 ���Z�v�g�ďo���P SECTION.
012750*
0           IF ��|������� NOT = ZERO
              MOVE  3   TO ���Z�|���Z���
           ELSE
              IF ��|������ NOT = ZERO
                 MOVE  2   TO ���Z�|���Z���
              ELSE
                 IF ��|�ی���� = 85
                    MOVE  7   TO ���Z�|���Z���
                 ELSE
                    MOVE  1   TO ���Z�|���Z���
                 END-IF
              END-IF
           END-IF.
005200     MOVE ��|�{�p�a��  TO ���Z�|�{�p�a��.
005210     MOVE ��|�{�p�N    TO ���Z�|�{�p�N.  
005220     MOVE ��|�{�p��    TO ���Z�|�{�p��.  
005230     MOVE ��|���Ҕԍ�  TO ���Z�|���Ҕԍ�.
005240     MOVE ��|�}��      TO ���Z�|�}��.    
           READ ���Z�v�g�e
           INVALID KEY
               MOVE SPACE     TO ���Z�|���R�[�h
           END-READ.
013080*
013090*================================================================*
013100 ���Z�v�g�ďo���Q SECTION.
013110*
009180     MOVE ��|���Ҕԍ�  TO ��|���Ҕԍ�.
009190     MOVE ��|�}��      TO ��|�}��.
009200     MOVE ��|�{�p�a��  TO ��|�{�p�a��.
009210     MOVE ��|�{�p�N    TO ��|�{�p�N.
009220     MOVE ��|�{�p��    TO ��|�{�p��.
009230     MOVE �{�p���v�q    TO ��|�{�p��.
009240     READ ��v�f�[�^�e
           INVALID KEY
               MOVE SPACE     TO ��|���R�[�h
           END-READ.
009710*
009750     MOVE ��|�ꕔ���S��   TO �ꕔ���S���v�q.
013480     MOVE ��|��p�z       TO ��p�z�v�q.
013490*
004030*================================================================*
004040 ��ƃt�@�C���Z�b�g SECTION.
004050*
008280     MOVE �{�p�a��v�q       TO ��P�|�{�p�a�� �����|�{�p�a��.
008290     MOVE �{�p�N�v�q         TO ��P�|�{�p�N �����|�{�p�N.
008300     MOVE �{�p���v�q         TO ��P�|�{�p�� �����|�{�p��.
008310     MOVE �{�p���v�q         TO ��P�|�{�p�� �����|�{�p��.
009320     MOVE �ꕔ���S���v�q     TO ��P�|�ꕔ���S��.
009330     MOVE ��p�z�v�q         TO ��P�|��p�z.
           MOVE 1                  TO �����|����敪.
004130     MOVE ��|���҃R�[�h     TO �����|���҃R�[�h.
           READ �����t�@�C��
           NOT INVALID KEY
               MOVE �����|�{�p�R�����g TO ��P�|�R�����g
           END-READ.
011370*================================================================*
011380 ���ڂ��ƌv�Z SECTION.
011390***********************************************
011400* �����f�[�^�Z�b�g                            *
011410***********************************************
011130     COMPUTE ��P�|������ = ��|������ + ��|�������Z��.
      */�{�×����ɉ^����×����o�� /180519
      *     MOVE ��|���񏈒u�����v TO ��P�|�{�×�.
           COMPUTE ��P�|�{�×� = ��|���񏈒u�����v + ��|�^����×� + ��|�������q���Z��.
015090     COMPUTE ��P�|���×� = ��|���×� + ��|���É��Z��.
011130     MOVE ��|���Ë���     TO ���Ë����v�q.
           MOVE ��|�Č���       TO ��P�|�Č���.
011450     MOVE ��|��×��P     TO ��×��v�q(1).
011460     MOVE ��|��×��Q     TO ��×��v�q(2).
011470     MOVE ��|��×��R�W   TO ��×��R�W�v�q.
011480     MOVE ��|��×��R�O   TO ��×��R�O�v�q.
011490     COMPUTE ��×��v�q(3)   = ��×��R�W�v�q   + ��×��R�O�v�q.
011500     MOVE ��|��×��S�T   TO ��×��S�T�v�q.
011510     MOVE ��|��×��S�W   TO ��×��S�W�v�q.
011520     MOVE ��|��×��S�O   TO ��×��S�O�v�q.
011530     COMPUTE ��×��v�q(4)   = ��×��S�T�v�q   + ��×��S�W�v�q   + ��×��S�O�v�q.
011500     MOVE ��|��×��T�Q   TO ��×��T�Q�v�q.
011500     MOVE ��|��×��T�T   TO ��×��T�T�v�q.
011510     MOVE ��|��×��T�W   TO ��×��T�W�v�q.
011520     MOVE ��|��×��T�O   TO ��×��T�O�v�q.
011530     COMPUTE ��×��v�q(5)   = ��×��T�Q�v�q + ��×��T�T�v�q + ��×��T�W�v�q + ��×��T�O�v�q.
011530     COMPUTE ��P�|��×� = ��×��v�q(1) + ��×��v�q(2) + ��×��v�q(3) + ��×��v�q(4) + ��×��v�q(5).
011570********************
011580* �����������Z�b�g *
011590********************
011600     MOVE ��|��㪖@���P             TO ��㪗��v�q(1).
011610     MOVE ��|��㪖@���Q             TO ��㪗��v�q(2).
011620     MOVE ��|��㪖@���R�W           TO ��㪖@���R�W�v�q.
011630     MOVE ��|��㪖@���R�O           TO ��㪖@���R�O�v�q.
011640     COMPUTE ��㪗��v�q(3)   = ��㪖@���R�W�v�q  + ��㪖@���R�O�v�q.
011650     MOVE ��|��㪖@���S�T           TO ��㪖@���S�T�v�q.
011660     MOVE ��|��㪖@���S�W           TO ��㪖@���S�W�v�q.
011670     MOVE ��|��㪖@���S�O           TO ��㪖@���S�O�v�q.
011680     COMPUTE ��㪗��v�q(4)   = ��㪖@���S�T�v�q  + ��㪖@���S�W�v�q  + ��㪖@���S�O�v�q.
011650     MOVE ��|��㪖@���T�Q           TO ��㪖@���T�Q�v�q.
011650     MOVE ��|��㪖@���T�T           TO ��㪖@���T�T�v�q.
011660     MOVE ��|��㪖@���T�W           TO ��㪖@���T�W�v�q.
011670     MOVE ��|��㪖@���T�O           TO ��㪖@���T�O�v�q.
011680     COMPUTE ��㪗��v�q(5)   = ��㪖@���T�Q�v�q + ��㪖@���T�T�v�q + ��㪖@���T�W�v�q  + ��㪖@���T�O�v�q.
011690     COMPUTE ��㪗��v�v�q = ��㪗��v�q(1) + ��㪗��v�q(2) + ��㪗��v�q(3) + ��㪗��v�q(4) + ��㪗��v�q(5).
011700*
011710     MOVE ��|��㪖@���P             TO ��㪗��v�q(1).
011720     MOVE ��|��㪖@���Q             TO ��㪗��v�q(2).
011730     MOVE ��|��㪖@���R�W           TO ��㪖@���R�W�v�q.
011740     MOVE ��|��㪖@���R�O           TO ��㪖@���R�O�v�q.
011750     COMPUTE ��㪗��v�q(3)   = ��㪖@���R�W�v�q  + ��㪖@���R�O�v�q.
011760     MOVE ��|��㪖@���S�T           TO ��㪖@���S�T�v�q.
011770     MOVE ��|��㪖@���S�W           TO ��㪖@���S�W�v�q.
011780     MOVE ��|��㪖@���S�O           TO ��㪖@���S�O�v�q.
011790     COMPUTE ��㪗��v�q(4)   = ��㪖@���S�T�v�q  + ��㪖@���S�W�v�q  + ��㪖@���S�O�v�q.
011760     MOVE ��|��㪖@���T�Q           TO ��㪖@���T�Q�v�q.
011760     MOVE ��|��㪖@���T�T           TO ��㪖@���T�T�v�q.
011770     MOVE ��|��㪖@���T�W           TO ��㪖@���T�W�v�q.
011780     MOVE ��|��㪖@���T�O           TO ��㪖@���T�O�v�q.
011790     COMPUTE ��㪗��v�q(5)   = ��㪖@���T�Q�v�q + ��㪖@���T�T�v�q + ��㪖@���T�W�v�q + ��㪖@���T�O�v�q.
011800     COMPUTE ��㪗��v�v�q = ��㪗��v�q(1) + ��㪗��v�q(2) + ��㪗��v�q(3) + ��㪗��v�q(4) + ��㪗��v�q(5).
           COMPUTE ��P�|㪖@�� = ��㪗��v�v�q + ��㪗��v�v�q.
011810*
011820     MOVE ��|�d�×��P             TO �d�×��v�q(1).
011830     MOVE ��|�d�×��Q             TO �d�×��v�q(2).
011840     MOVE ��|�d�×��R�W           TO �d�×��R�W�v�q.
011850     MOVE ��|�d�×��R�O           TO �d�×��R�O�v�q.
011860     COMPUTE �d�×��v�q(3)  = �d�×��R�W�v�q  + �d�×��R�O�v�q.
011870     MOVE ��|�d�×��S�T           TO �d�×��S�T�v�q.
011880     MOVE ��|�d�×��S�W           TO �d�×��S�W�v�q.
011890     MOVE ��|�d�×��S�O           TO �d�×��S�O�v�q.
011900     COMPUTE �d�×��v�q(4)  = �d�×��S�T�v�q  + �d�×��S�W�v�q  + �d�×��S�O�v�q.
011870     MOVE ��|�d�×��T�Q           TO �d�×��T�Q�v�q.
011870     MOVE ��|�d�×��T�T           TO �d�×��T�T�v�q.
011880     MOVE ��|�d�×��T�W           TO �d�×��T�W�v�q.
011890     MOVE ��|�d�×��T�O           TO �d�×��T�O�v�q.
011900     COMPUTE �d�×��v�q(5)  = �d�×��T�Q�v�q + �d�×��T�T�v�q + �d�×��T�W�v�q + �d�×��T�O�v�q.
           COMPUTE ��P�|�d�×� = �d�×��v�q(1) + �d�×��v�q(2) + �d�×��v�q(3) + �d�×��v�q(4) + �d�×��v�q(5).
011920*
004140*================================================================*
004150 ��P���R�[�h���� SECTION.
004160*
004170     WRITE ��P�|���R�[�h
004180     INVALID KEY
004190         MOVE NC"��P"  TO �t�@�C����
004200         PERFORM �G���[�\��
004210     END-WRITE.
011020*================================================================*
011030 ���v�v�Z SECTION.
011040*
011050     PERFORM ���Z�v�g�ďo���P.
007720     PERFORM �������擾.
007730     MOVE �������v�q              TO �A��|������.
007840     MOVE ��×��v�v�q            TO �A��|��×�.
007840     MOVE ���×��v�q              TO �A��|���×�.
007840     MOVE 㪖@���v�v�q            TO �A��|㪖@��.
007840     MOVE �d�×��v�v�q            TO �A��|�d�×�.
011060     MOVE ���Z�|�ꕔ���S��        TO �A��|���S�z.
011110     MOVE ���Z�|���v              TO �A��|��p�z.
011110     MOVE �{�p���񋟗��v�q      TO �A��|���񋟗�.
011110     MOVE ��v                    TO �A��|��.
011110     MOVE ���v                    TO �A��|��.
011110     MOVE ���v                    TO �A��|��.
011110     MOVE �������q���Z���v�q      TO �A��|�������q��.
011110     COMPUTE �A��|���̑� = ���������k���v�q + ���׏����s���v�q.
011110     MOVE ���������k���v�q        TO �A��|���k��.
011110     MOVE ���׏����s���v�q        TO �A��|���ח�.
      *
008280     MOVE �{�p�a��v�q            TO �����|�{�p�a��.
008290     MOVE �{�p�N�v�q              TO �����|�{�p�N.
008300     MOVE �{�p���v�q              TO �����|�{�p��.
008310     MOVE 99                      TO �����|�{�p��.
           MOVE 1                       TO �����|����敪.
004130     MOVE ��|���҃R�[�h          TO �����|���҃R�[�h.
           READ �����t�@�C��
           NOT INVALID KEY
               MOVE �����|�{�p�R�����g  TO �A��|�R�����g
           END-READ.
      *
           MOVE ��|���Ҏ���            TO �A��|���Ҏ���.
      *
010960*================================================================*
010970 �������擾 SECTION.
010980*
011050     COMPUTE �������v�q = ���Z�|������ + ���Z�|�������Z�� +
                                ���Z�|���񏈒u�����v + ���Z�|�Č��� +
                                ���Z�|�^����×� + ���Z�|�������q���Z��.
           MOVE ���Z�|���������k��           TO  ���������k���v�q.
           MOVE ���Z�|���׏����s���Z��       TO  ���׏����s���v�q.
011060*
011140     COMPUTE ���×��v�q = ���Z�|���×� + ���Z�|���É��Z��.
011190*
           IF ��|�{�p�a��N�� < 43006
              IF ���Z�|�� >= 1
                 MOVE NC"��"                    TO ��v
              END-IF
              IF ���Z�|�� >= 1
                 MOVE NC"��"                    TO ���v
              END-IF
              IF ���Z�|�� >= 1
                 MOVE NC"��"                    TO ���v
              END-IF
           END-IF.
011200     MOVE ���Z�|�������q���Z��         TO  �������q���Z���v�q.
011210*
011220     MOVE ���Z�|�{�p���񋟗�         TO  �{�p���񋟗��v�q.
      *
011450     MOVE ���Z�|��×��P     TO ��×��v�q(1).
011460     MOVE ���Z�|��×��Q     TO ��×��v�q(2).
011470     MOVE ���Z�|��×��R�W   TO ��×��R�W�v�q.
011480     MOVE ���Z�|��×��R�O   TO ��×��R�O�v�q.
011490     COMPUTE ��×��v�q(3)   = ��×��R�W�v�q   + ��×��R�O�v�q.
011500     MOVE ���Z�|��×��S�T   TO ��×��S�T�v�q.
011510     MOVE ���Z�|��×��S�W   TO ��×��S�W�v�q.
011520     MOVE ���Z�|��×��S�O   TO ��×��S�O�v�q.
011530     COMPUTE ��×��v�q(4)   = ��×��S�T�v�q   + ��×��S�W�v�q   + ��×��S�O�v�q.
011500     MOVE ���Z�|��×��T�Q   TO ��×��T�Q�v�q.
011500     MOVE ���Z�|��×��T�T   TO ��×��T�T�v�q.
011510     MOVE ���Z�|��×��T�W   TO ��×��T�W�v�q.
011520     MOVE ���Z�|��×��T�O   TO ��×��T�O�v�q.
011530     COMPUTE ��×��v�q(5)   = ��×��T�Q�v�q + ��×��T�T�v�q + ��×��T�W�v�q + ��×��T�O�v�q.
011530     COMPUTE ��×��v�v�q = ��×��v�q(1) + ��×��v�q(2) + ��×��v�q(3) + ��×��v�q(4) + ��×��v�q(5).
      *
011330     MOVE ���Z�|��㪖@���P             TO ��㪗��v�q(1).
011340     MOVE ���Z�|��㪖@���Q             TO ��㪗��v�q(2).
011350     MOVE ���Z�|��㪖@���R�W           TO ��㪖@���R�W�v�q.
011360     MOVE ���Z�|��㪖@���R�O           TO ��㪖@���R�O�v�q.
011370     COMPUTE ��㪗��v�q(3)   = ��㪖@���R�W�v�q  + ��㪖@���R�O�v�q.
011380     MOVE ���Z�|��㪖@���S�T           TO ��㪖@���S�T�v�q.
011390     MOVE ���Z�|��㪖@���S�W           TO ��㪖@���S�W�v�q.
011400     MOVE ���Z�|��㪖@���S�O           TO ��㪖@���S�O�v�q.
011410     COMPUTE ��㪗��v�q(4)   = ��㪖@���S�T�v�q  + ��㪖@���S�W�v�q  + ��㪖@���S�O�v�q.
011650     MOVE ���Z�|��㪖@���T�Q           TO ��㪖@���T�Q�v�q.
011650     MOVE ���Z�|��㪖@���T�T           TO ��㪖@���T�T�v�q.
011660     MOVE ���Z�|��㪖@���T�W           TO ��㪖@���T�W�v�q.
011670     MOVE ���Z�|��㪖@���T�O           TO ��㪖@���T�O�v�q.
011680     COMPUTE ��㪗��v�q(5)   = ��㪖@���T�Q�v�q + ��㪖@���T�T�v�q + ��㪖@���T�W�v�q  + ��㪖@���T�O�v�q.
011690     COMPUTE ��㪗��v�v�q = ��㪗��v�q(1) + ��㪗��v�q(2) + ��㪗��v�q(3) + ��㪗��v�q(4) + ��㪗��v�q(5).
011700*
011710     MOVE ���Z�|��㪖@���P             TO ��㪗��v�q(1).
011720     MOVE ���Z�|��㪖@���Q             TO ��㪗��v�q(2).
011730     MOVE ���Z�|��㪖@���R�W           TO ��㪖@���R�W�v�q.
011740     MOVE ���Z�|��㪖@���R�O           TO ��㪖@���R�O�v�q.
011750     COMPUTE ��㪗��v�q(3)   = ��㪖@���R�W�v�q  + ��㪖@���R�O�v�q.
011760     MOVE ���Z�|��㪖@���S�T           TO ��㪖@���S�T�v�q.
011770     MOVE ���Z�|��㪖@���S�W           TO ��㪖@���S�W�v�q.
011780     MOVE ���Z�|��㪖@���S�O           TO ��㪖@���S�O�v�q.
011790     COMPUTE ��㪗��v�q(4)   = ��㪖@���S�T�v�q  + ��㪖@���S�W�v�q  + ��㪖@���S�O�v�q.
011760     MOVE ���Z�|��㪖@���T�Q           TO ��㪖@���T�Q�v�q.
011760     MOVE ���Z�|��㪖@���T�T           TO ��㪖@���T�T�v�q.
011770     MOVE ���Z�|��㪖@���T�W           TO ��㪖@���T�W�v�q.
011780     MOVE ���Z�|��㪖@���T�O           TO ��㪖@���T�O�v�q.
011790     COMPUTE ��㪗��v�q(5)   = ��㪖@���T�Q�v�q + ��㪖@���T�T�v�q + ��㪖@���T�W�v�q + ��㪖@���T�O�v�q.
011800     COMPUTE ��㪗��v�v�q = ��㪗��v�q(1) + ��㪗��v�q(2) + ��㪗��v�q(3) + ��㪗��v�q(4) + ��㪗��v�q(5).
           COMPUTE 㪖@���v�v�q = ��㪗��v�v�q + ��㪗��v�v�q.
011810*
011820     MOVE ���Z�|�d�×��P             TO �d�×��v�q(1).
011830     MOVE ���Z�|�d�×��Q             TO �d�×��v�q(2).
011840     MOVE ���Z�|�d�×��R�W           TO �d�×��R�W�v�q.
011850     MOVE ���Z�|�d�×��R�O           TO �d�×��R�O�v�q.
011860     COMPUTE �d�×��v�q(3)  = �d�×��R�W�v�q  + �d�×��R�O�v�q.
011870     MOVE ���Z�|�d�×��S�T           TO �d�×��S�T�v�q.
011880     MOVE ���Z�|�d�×��S�W           TO �d�×��S�W�v�q.
011890     MOVE ���Z�|�d�×��S�O           TO �d�×��S�O�v�q.
011900     COMPUTE �d�×��v�q(4)  = �d�×��S�T�v�q  + �d�×��S�W�v�q  + �d�×��S�O�v�q.
011870     MOVE ���Z�|�d�×��T�Q           TO �d�×��T�Q�v�q.
011870     MOVE ���Z�|�d�×��T�T           TO �d�×��T�T�v�q.
011880     MOVE ���Z�|�d�×��T�W           TO �d�×��T�W�v�q.
011890     MOVE ���Z�|�d�×��T�O           TO �d�×��T�O�v�q.
011900     COMPUTE �d�×��v�q(5)  = �d�×��T�Q�v�q + �d�×��T�T�v�q + �d�×��T�W�v�q + �d�×��T�O�v�q.
           COMPUTE �d�×��v�v�q = �d�×��v�q(1) + �d�×��v�q(2) + �d�×��v�q(3) + �d�×��v�q(4) + �d�×��v�q(5).
011920*
      *================================================================*
       �f�[�^�`�F�b�N SECTION.
      *
           MOVE SPACE          TO ���s�L�[�v.
      * *****************************************************************
      * * �������ʗL���`�F�b�N�F���ʐ� = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
      * *****************************************************************
           MOVE ��|�{�p�a��   TO ���|�{�p�a��.
           MOVE ��|�{�p�N     TO ���|�{�p�N.
           MOVE ��|�{�p��     TO ���|�{�p��.
           MOVE ��|���Ҕԍ�   TO ���|���Ҕԍ�.
           MOVE ��|�}��       TO ���|�}��.
           READ �����f�[�^�e
           INVALID KEY
               MOVE SPACE  TO ���s�L�[�v
           NOT INVALID KEY
               IF ���|���ʐ� NOT = ZERO
      *        *************************************************************
      *        * �{�p�L�^�`�F�b�N�F�ʉ@�� = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
      *        *************************************************************
                   MOVE ���|���Ҕԍ�  TO �{�L�|���Ҕԍ�
                   MOVE ���|�}��      TO �{�L�|�}��
                   MOVE ���|�{�p�a��  TO �{�L�|�{�p�a��
                   MOVE ���|�{�p�N    TO �{�L�|�{�p�N
                   MOVE ���|�{�p��    TO �{�L�|�{�p��
                   MOVE ZERO          TO �{�L�|�{�p��
                   START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
                                                �{�L�|�{�p�a��N����
                   END-START
                   IF ��ԃL�[ = "00"
                       MOVE SPACE TO �I���t���O�Q
                       MOVE SPACE TO �{�p�L�^�L�v
                       PERFORM �{�p�L�^�e�Ǎ�
                       PERFORM UNTIL (�I���t���O�Q         = "YES"         ) OR
                                     (�{�L�|���҃R�[�h NOT = ���|���҃R�[�h) OR
                                     (�{�L�|�{�p�a��   NOT = ���|�{�p�a��  ) OR
                                     (�{�L�|�{�p�N     NOT = ���|�{�p�N    ) OR
                                     (�{�L�|�{�p��     NOT = ���|�{�p��    ) OR
                                     (�{�p�L�^�L�v         = "YES"         )
                           MOVE "YES"  TO �{�p�L�^�L�v
                           MOVE "YES"  TO ���s�L�[�v
                       END-PERFORM
                   ELSE
                       MOVE SPACE  TO ���s�L�[�v
                   END-IF
               ELSE
                   MOVE SPACE  TO ���s�L�[�v
               END-IF
           END-READ.
      *
      *================================================================*
       �{�p�L�^�e�Ǎ� SECTION.
      *
           READ �{�p�L�^�e NEXT
           AT END
               MOVE "YES"  TO �I���t���O�Q
           END-READ.
027377*================================================================*
027378 �������擾 SECTION.
027379*
027382     MOVE �{�p�a��v�q TO ���|�����敪.
027383     READ �����}�X�^
027384     NOT INVALID KEY
027385         MOVE ���|�J�n����N TO �{�p����N�v
027386     END-READ.
027387     IF �{�p����N�v NOT = ZERO
027388        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
027389     END-IF.
027390*
           MOVE �{�p���v�q   TO �󗝌��v.
027391     EVALUATE �{�p���v�q
027392     WHEN 4
027393     WHEN 6
027394     WHEN 9
027395     WHEN 11
027396         MOVE 30 TO �󗝓��v
027397     WHEN 2
027398         DIVIDE 4 INTO �{�p����N�v GIVING    ���v
027399                                    REMAINDER �]�v
027400         END-DIVIDE
027401         IF �]�v = ZERO
027402             MOVE 29 TO �󗝓��v
027403         ELSE
027404             MOVE 28 TO �󗝓��v
027405         END-IF
027406     WHEN 1
027407     WHEN 3
027408     WHEN 5
027409     WHEN 7
027410     WHEN 8
027411     WHEN 10
027412     WHEN 12
027413         MOVE 31 TO �󗝓��v
027414     WHEN OTHER
027415          CONTINUE
027416     END-EVALUATE.
027417*
004230******************************************************************
004240 END PROGRAM YIW721.
004250******************************************************************
