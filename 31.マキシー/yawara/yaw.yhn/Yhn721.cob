000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN721.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*    ���{�ق˂��E�I���E����܎t����@�{�p�̎����@�f�[�^�쐬
000100*         MED = YHN720 
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2014-09-01
000130 DATE-COMPILED.          2014-09-01
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
000320     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  ��|�{�p�a��N��
000360                                                          ��|���҃R�[�h
000370                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000380                                                          ��|���҃J�i
000390                                                          ��|���҃R�[�h
000400                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000410                                                          ��|�{�p�a��N��
000420                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000430                                                          ��|�ی����
000440                                                          ��|�ی��Ҕԍ�
000450                                                          ��|���҃R�[�h
000460                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000470                                                          ��|������
000480                                                          ��|��p���S�Ҕԍ�
000490                                                          ��|���҃R�[�h
000500                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000510                                                          ��|�������
000520                                                          ��|��p���S�Ҕԍ�����
000530                                                          ��|���҃R�[�h
000540                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
000550                                                          ��|�{�p�a��N��
000560                                                          ��|���҃R�[�h
000570                             FILE STATUS              IS  ��ԃL�[
000580                             LOCK        MODE         IS  AUTOMATIC.
000590     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000600                             ORGANIZATION             IS  INDEXED
000610                             ACCESS MODE              IS  DYNAMIC
000620                             RECORD KEY               IS  ���|�{�p�a��N��
000630                                                          ���|���҃R�[�h
000640                             ALTERNATE RECORD KEY     IS  ���|���҃R�[�h
000650                                                          ���|�{�p�a��N��
000660                             FILE STATUS              IS  ��ԃL�[
000670                             LOCK        MODE         IS  AUTOMATIC.
000680     SELECT  �{�p�L�^�e      ASSIGN      TO      SEKIROKL
000690                             ORGANIZATION        IS  INDEXED
000700                             ACCESS MODE         IS  DYNAMIC
000710                             RECORD KEY          IS  �{�L�|�{�p�a��N����
000720                                                     �{�L�|���҃R�[�h
000730                             ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
000740                                                     �{�L�|�{�p�a��N����
000750                             FILE STATUS              IS  ��ԃL�[
000760                             LOCK        MODE         IS  AUTOMATIC.
000770     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000780                             ORGANIZATION             IS  INDEXED
000790                             ACCESS MODE              IS  DYNAMIC
000800                             RECORD KEY               IS  ���|����敪
000810                             FILE STATUS              IS  ��ԃL�[
000820                             LOCK        MODE         IS  AUTOMATIC.
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
000980     SELECT  ��ƃt�@�C���Q  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7212L.DAT"
000990                             ORGANIZATION             IS  INDEXED
001000                             ACCESS                   IS  DYNAMIC
001010                             RECORD      KEY          IS  ��Q�|�{�p�a��N��
001020                                                          ��Q�|���҃R�[�h
001030                             FILE        STATUS       IS  ��ԃL�[
001040                             LOCK        MODE         IS  AUTOMATIC.
001050******************************************************************
001060*                      DATA DIVISION                             *
001070******************************************************************
001080 DATA                    DIVISION.
001090 FILE                    SECTION.
001100*                           �m�q�k��  �P�Q�W�n
001110 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001120     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001130*                           �m�q�k��  �R�Q�O�n
001140 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001150     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001160*                           �m�q�k��  �Q�T�U�n
001170 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
001180    COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
001190*                           �m�q�k��  �P�Q�W�n
001200 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001210     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001220*                           �m�q�k��  �Q�T�U�n
001230 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001240     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001060*                           �m�q�k��  �T�P�Q�n
001070 FD  ��v�f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001080     COPY KAIKEI     OF  XFDLIB  JOINING   ��   AS  PREFIX.
001250*****************
001260* ��ƃt�@�C���P *
001270*****************
001280*                         �m�q�k��  �P�U�O�n
001290 FD  ��ƃt�@�C���P RECORD  CONTAINS 160 CHARACTERS.
001300 01 ��P�|���R�[�h.
001310    03 ��P�|���R�[�h�L�[.
001320       05 ��P�|�{�p�a��N����.
001330          07 ��P�|�{�p�a��               PIC 9.
001340          07 ��P�|�{�p�N��.
001350             09 ��P�|�{�p�N              PIC 9(2).
001360             09 ��P�|�{�p��              PIC 9(2).
001370          07 ��P�|�{�p��                 PIC 9(2).
001380       05 ��P�|���҃R�[�h.
001390          07 ��P�|���Ҕԍ�                PIC 9(6).
001400          07 ��P�|�}��                    PIC X(1).
001410    03 ��P�|���R�[�h�f�[�^.
001420       05 ��P�|���҃J�i                   PIC X(50).
001430       05 ��P�|���Ҏ���                   PIC X(50).
001440       05 ��P�|����.
001450          07 ��P�|�������z                PIC 9(5).
001460          07 ��P�|�������z                PIC 9(5).
001470          07 ��P�|��Ë��z                PIC 9(5).
001480          07 ��P�|㪖@���z                PIC 9(5).
001490          07 ��P�|�d�Ë��z                PIC 9(5).
001500          07 ��P�|��p�z                  PIC 9(5).
001510          07 ��P�|�ꕔ���S��              PIC 9(5).
001520       05 FILLER                           PIC X(11).
001530*
001540*                         �m�q�k��  �P�Q�W�n
001550 FD  ��ƃt�@�C���Q RECORD  CONTAINS 128 CHARACTERS.
001560 01 ��Q�|���R�[�h.
001570    03 ��Q�|���R�[�h�L�[.
001580       05 ��Q�|�{�p�a��N��.
001590          07 ��Q�|�{�p�a��                PIC 9.
001600          07 ��Q�|�{�p�N��.
001610             09 ��Q�|�{�p�N               PIC 9(2).
001620             09 ��Q�|�{�p��               PIC 9(2).
001630       05 ��Q�|���҃R�[�h.
001640          07 ��Q�|���Ҕԍ�                PIC 9(6).
001650          07 ��Q�|�}��                    PIC X(1).
001660    03 ��Q�|���R�[�h�f�[�^.
001670       05 ��Q�|�ŏI�ʉ@��.
001680          07 ��Q�|�ʉ@��                  PIC 9(2).
001690          07 ��Q�|�ʉ@��                  PIC 9(2).
001700       05 ��Q�|�{�p��                   PIC 9(2).
001710       05 ��Q�|��p�z                     PIC 9(6).
001720       05 ��Q�|�����z                     PIC 9(6).
001730       05 ��Q�|���S��                     PIC 9(5).
001740       05 ��Q�|�]�A                       OCCURS 5.
001750          07 ��Q�|�]�A�敪                PIC N(1).
001760       05 ��Q�|�����v                     PIC 9(5).
001770       05 ��Q�|�����v                     PIC 9(5).
001780       05 ��Q�|��Ìv                     PIC 9(5).
001790       05 ��Q�|㪖@�v                     PIC 9(5).
001800       05 ��Q�|�d�Ìv                     PIC 9(5).
001810       05 ��Q�|��p�v                     PIC 9(5).
001820       05 ��Q�|���S�v                     PIC 9(5).
001830       05 FILLER                           PIC X(48).
001840*
001850*----------------------------------------------------------------*
001860******************************************************************
001870*                WORKING-STORAGE SECTION                         *
001880******************************************************************
001890 WORKING-STORAGE         SECTION.
001900 01 �L�[����                           PIC X    VALUE SPACE.
001910 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
001920 01 �I���t���O                         PIC X(3) VALUE SPACE.
001930 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
001940 01 �I���t���O�R                       PIC X(3) VALUE SPACE.
001950 01 �t�@�C����                         PIC N(2) VALUE SPACE.
001960 01 ���s�L�[�v                         PIC X(4) VALUE SPACE.
001970 01 �{�p�L�^�L�v                       PIC X(3) VALUE SPACE.
001980 01 �p���t���O                         PIC X(3) VALUE SPACE.
001990 01 �Ώۃt���O                         PIC X(3) VALUE SPACE.
002000 01 ���ʂb�m�s                         PIC 9    VALUE ZERO.
002010 01 �J�E���^                           PIC 9    VALUE ZERO.
002020 01 ����Ώۂ���e                     PIC 9    VALUE ZERO.
002030 01 �Ώۊ��҂e                         PIC 9    VALUE ZERO.
002040* **************
002050* * ���ڑҔ�p 
002060* **************
002070 01 �Ҕ����ڂv�q.
002080    03 �{�p�a��N���v�q.
002090       05 �{�p�a��v�q                 PIC 9(1) VALUE ZERO.
002100       05 �{�p�N�v�q                   PIC 9(2) VALUE ZERO.
002110       05 �{�p���v�q                   PIC 9(2) VALUE ZERO.
002120    03 �{�p���v�q                      PIC 9(2) VALUE ZERO.
002130    03 �ŏI�ʉ@���v                    PIC 9(2) VALUE ZERO.
002140    03 �ŏI�ʉ@���v                    PIC 9(2) VALUE ZERO.
002150    03 �󗝌��v                        PIC 9(2) VALUE ZERO.
002160    03 �󗝓��v                        PIC 9(2) VALUE ZERO.
002170    03 �J�n���v�o                      PIC 9(2) VALUE ZERO.
002180    03 �I�����v�o                      PIC 9(2) VALUE ZERO.
002190    03 �J�n�a��N�����v�q.
002200       05 �J�n�a��N���v�q.
002210          07 �J�n�a��v�q              PIC 9(1) VALUE ZERO.
002220          07 �J�n�N�v�q                PIC 9(2) VALUE ZERO.
002230          07 �J�n���v�q                PIC 9(2) VALUE ZERO.
002240       05 �J�n���v�q                   PIC 9(2) VALUE ZERO.
002250    03 �I���a��N�����v�q.
002260       05 �I���a��N���v�q.
002270          07 �I���a��v�q              PIC 9(1) VALUE ZERO.
002280          07 �I���N�v�q                PIC 9(2) VALUE ZERO.
002290          07 �I�����v�q                PIC 9(2) VALUE ZERO.
002300       05 �I�����v�q                   PIC 9(2) VALUE ZERO.
002310    03 ��ʋ敪�v�q                    PIC X(4) VALUE SPACE.
002320    03 �ی���ʂv�q                    PIC 9(2) VALUE ZERO.
002330    03 ���҃R�[�h�v�q.
002340       05 ���Ҕԍ��v�q                 PIC 9(6) VALUE ZERO.
002350       05 �}�Ԃv�q                     PIC X(1) VALUE SPACE.
002360    03 �{�l�Ƒ��敪�v�q                PIC 9(1) VALUE ZERO.
002370*
002380    03 ��������v�q                    PIC 9(2) VALUE ZERO.
002390*
002400 01 ����S�Ҕԍ��v.
002410    03 �@�ʔԍ��v                      PIC X(2) VALUE SPACE.
002420    03 FILLER                          PIC X(8) VALUE SPACE.
002430 01 �ޔ����ڂf�v.
002440   03 ���Z�v�g��ނv                 PIC X(4).
002450   03 ���Z�v�g��ނf�v               PIC X(4).
002460   03 ���Z�v�g��ʂf�v               PIC 9(2).
002470*
002480 01 ����.
002490    03 �������v�q                    PIC 9(4)  VALUE ZERO.
002500    03 �������Z���v�q                PIC 9(4)  VALUE ZERO.
002510    03 �x���v�q                      PIC 9     VALUE ZERO.
002520    03 �[��v�q                      PIC 9     VALUE ZERO.
002530    03 ���ԊO�v�q                    PIC 9     VALUE ZERO.
002540    03 �f�Î��v�q                    PIC 9(2)  VALUE ZERO.
002550    03 �f�Õ��v�q                    PIC 9(2)  VALUE ZERO.
          03 ���k���v�q                    PIC 9(4)  VALUE ZERO.
002560    03 �Č����v�q                    PIC 9(4)  VALUE ZERO.
002570    03 ���Ö�Ԃv�q                  PIC 9     VALUE ZERO.
002580    03 ���Ó�H�v�q                  PIC 9     VALUE ZERO.
002590    03 ���Ö\���v�q                  PIC 9     VALUE ZERO.
002600    03 ���É񐔂v�q                  PIC 9(2)  VALUE ZERO.
002610    03 ���Ë����v�q                  PIC 9(3)V9 VALUE ZERO.
002620    03 ���×��v�q                    PIC 9(6)  VALUE ZERO.
002630    03 ���É��Z���v�q                PIC 9(5)  VALUE ZERO.
002640    03 �d㪗��v�q                    PIC 9(4)  VALUE ZERO.
002650    03 �ꕔ���S���v�q                PIC 9(5)  VALUE ZERO.
002660    03 �������q���Z���v�q            PIC 9(5)  VALUE ZERO.
004330    03 �^���Ö@���v�q                PIC 9(4)  VALUE ZERO.
002670    03 �{�p���񋟗��v�q            PIC 9(6)  VALUE ZERO.
002680    03 ���ʂv                        OCCURS 4.
002690       05 ���񏈒u���v�q             PIC 9(4)  VALUE ZERO.
002700       05 ��×��v�q                 PIC 9(4)  VALUE ZERO.
002710       05 ��㪗��v�q                 PIC 9(4)  VALUE ZERO.
002720       05 ��㪗��v�q                 PIC 9(4)  VALUE ZERO.
002730       05 �d�×��v�q                 PIC 9(4)  VALUE ZERO.
002740       05 ���ʌv�v�q                 PIC 9(4)  VALUE ZERO.
002750       05 �����v�q                   PIC 9     VALUE ZERO.
002760    03 �����v�q                      PIC 9(2)  OCCURS 5 VALUE ZERO.
002770    03 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
002780    03 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
002790    03 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
002800    03 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
002810    03 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
002820    03 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
002830    03 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
002840    03 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
002850    03 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
002860    03 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
002870    03 �d�×��R�O�v�q                PIC 9(4)  VALUE ZERO.
002880    03 �d�×��R�W�v�q                PIC 9(4)  VALUE ZERO.
002890    03 �d�×��S�O�v�q                PIC 9(4)  VALUE ZERO.
002900    03 �d�×��S�T�v�q                PIC 9(4)  VALUE ZERO.
002910    03 �d�×��S�W�v�q                PIC 9(4)  VALUE ZERO.
002920    03 ��×��R�O�v�q                PIC 9(4)  VALUE ZERO.
002930    03 ��×��R�W�v�q                PIC 9(4)  VALUE ZERO.
002940    03 ��×��S�O�v�q                PIC 9(4)  VALUE ZERO.
002950    03 ��×��S�T�v�q                PIC 9(4)  VALUE ZERO.
002960    03 ��×��S�W�v�q                PIC 9(4)  VALUE ZERO.
002970    03 ��㪗��v�v�q                  PIC 9(4)  VALUE ZERO.
002980    03 ��㪗��v�v�q                  PIC 9(4)  VALUE ZERO.
002990    03 �d�×��v�v�q                  PIC 9(4)  VALUE ZERO.
003000*
003010 01 �W�v�v.
003020    03 �����v�v                      PIC 9(5)  VALUE ZERO.
003030    03 �����v�v                      PIC 9(5)  VALUE ZERO.
003040    03 ��Ìv�v                      PIC 9(5)  VALUE ZERO.
003050    03 㪖@�v�v                      PIC 9(5)  VALUE ZERO.
003060    03 �d�Ìv�v                      PIC 9(5)  VALUE ZERO.
003070    03 ��p�v�v                      PIC 9(5)  VALUE ZERO.
003080    03 �ꕔ���S���v�v                PIC 9(5)  VALUE ZERO.
003090** ���Z���i�̓��t�敪�p (0:�ŏI�ʉ@���A1:�������A9:�󎚂Ȃ�)
003100 01 ���Z�v�g���t�敪�v                 PIC 9 VALUE ZERO.
003110 01 ���Z�v�g���ғ��t�敪�v             PIC 9 VALUE ZERO.
003120** �������p
003130 01 �{�p����N�v                       PIC 9(4)  VALUE ZERO.
003140 01 ���v                               PIC 9(3)  VALUE ZERO.
003150 01 �]�v                               PIC 9(3)  VALUE ZERO.
003160*
003170******************************************************************
003180*                          �A������                              *
003190******************************************************************
003200*
003210************
003220* ���̓L�[ *
003230************
003240*
003250**********************
003260* ���b�Z�[�W�\���L�[ *
003270**********************
003280 01 �A���|�L�[ IS EXTERNAL.
003290    03  �A���|���b�Z�[�W                 PIC N(20).
003300*
003310*
003320****************
003330* ��ʓ��͏�� *
003340****************
003350 01 �A���|���̓f�[�^�x�g�m�V�Q�O IS EXTERNAL.
003360    03 �A���|�J�n�a��N����.
003370       05 �A���|�J�n�a��                  PIC 9(1).
003380       05 �A���|�J�n�N                    PIC 9(2).
003390       05 �A���|�J�n��                    PIC 9(2).
003400       05 �A���|�J�n��                    PIC 9(2).
003410    03 �A���|�I���a��N����.
003420       05 �A���|�I���a��                  PIC 9(1).
003430       05 �A���|�I���N                    PIC 9(2).
003440       05 �A���|�I����                    PIC 9(2).
003450       05 �A���|�I����                    PIC 9(2).
          03 �A���|����Ώۂe                   PIC 9.
       01 �A���|���̓f�[�^�U�U�O IS EXTERNAL.
          03 �A���|�{�p�a��N��.
             05 �A���|�{�p�a��                  PIC 9(1).
             05 �A���|�{�p�N                    PIC 9(2).
             05 �A���|�{�p��                    PIC 9(2).
          03 �A���|�ی����                     PIC 9(2).
          03 �A���|�{�l�Ƒ��敪                 PIC 9(1).
          03 �A���|���҃R�[�h.
             05 �A���|���Ҕԍ�                  PIC 9(6).
             05 �A���|�}��                      PIC X(1).
      *
          03 �A���|�������                     PIC 9(2).
          03 �A���|�ǂݏ�                       PIC X(4).
006810*
006820******************************************************************
006830*                      PROCEDURE  DIVISION                       *
006840******************************************************************
006850 PROCEDURE               DIVISION.
006870************
006880*           *
006890* ��������   *
006900*           *
006910************
006920     PERFORM ������.
006930************
006940*           *
006950* �又��     *
006960*           *
006970************
006980     PERFORM ��ƃt�@�C���쐬.
006990************
007000*           *
007010* �I������   *
007020*           *
007030************
007040*/����f�[�^�̂Ȃ��ꍇ�̓��b�Z�[�W���o��20131
007050*     IF ����Ώۂ���e = ZERO
007060*         MOVE  NC"�@�@�@����Ώۃf�[�^������܂���" TO �A���|���b�Z�[�W
007070*         CALL   "MSG001"
007080*         CANCEL "MSG001"
007090*     END-IF.
           MOVE ����Ώۂ���e TO �A���|����Ώۂe.
007100     PERFORM �I������.
007110     MOVE ZERO TO PROGRAM-STATUS.
007120     EXIT PROGRAM.
007130*
007140*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
007150*================================================================*
007160 ������ SECTION.
007170*
007180     PERFORM �t�@�C���I�[�v��.
007190* �A�����ڂ̑Ҕ�
007200     INITIALIZE �Ҕ����ڂv�q.
007220     MOVE �A���|�ی����      TO �ی���ʂv�q.
007230     MOVE �A���|���Ҕԍ�      TO ���Ҕԍ��v�q.
007240     MOVE �A���|�}��          TO �}�Ԃv�q.
007250     MOVE �A���|�J�n�a��      TO �J�n�a��v�q.
007260     MOVE �A���|�J�n�N        TO �J�n�N�v�q.
007270     MOVE �A���|�J�n��        TO �J�n���v�q.
007280     MOVE �A���|�J�n��        TO �J�n���v�q.
007290     MOVE �A���|�I���a��      TO �I���a��v�q.
007300     MOVE �A���|�I���N        TO �I���N�v�q.
007310     MOVE �A���|�I����        TO �I�����v�q.
007320     MOVE �A���|�I����        TO �I�����v�q.
007330     MOVE �A���|�{�l�Ƒ��敪  TO �{�l�Ƒ��敪�v�q.
007340*
007350     MOVE �A���|�������      TO ��������v�q.
007360*================================================================*
007370 �t�@�C���I�[�v�� SECTION.
007380*
007390     OPEN INPUT   �����}�X�^
007400         MOVE NC"����" TO �t�@�C����.
007410         PERFORM �I�[�v���`�F�b�N.
007420     OPEN INPUT ��f�ҏ��e.
007430         MOVE NC"��f�ҏ��e" TO �t�@�C����.
007440         PERFORM �I�[�v���`�F�b�N.
003250     OPEN INPUT ���Z�v�g�e.
003260         MOVE NC"���Z�v�g�e" TO �t�@�C����.
003270         PERFORM �I�[�v���`�F�b�N.
007450     OPEN INPUT �����f�[�^�e.
007460         MOVE NC"�����f�[�^�e" TO �t�@�C����.
007470         PERFORM �I�[�v���`�F�b�N.
007480     OPEN INPUT �{�p�L�^�e.
007490         MOVE NC"�{�p�L�^�e"   TO �t�@�C����.
007500         PERFORM �I�[�v���`�F�b�N.
007510     OPEN INPUT   ������}�X�^
007520         MOVE NC"������" TO �t�@�C����.
007530         PERFORM �I�[�v���`�F�b�N.
003060     OPEN INPUT ��v�f�[�^�e.
003070         MOVE NC"��v" TO �t�@�C����.
003080         PERFORM �I�[�v���`�F�b�N.
007540*================================================================*
007550 �I�[�v���`�F�b�N SECTION.
007560*
007570     IF ��ԃL�[  NOT =  "00"
007580         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
007590         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
007600         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
007610                                                 UPON CONS
003131*-----------------------------------------*
003132         CALL "actcshm"  WITH C LINKAGE
003133*-----------------------------------------*
007620         ACCEPT  �L�[���� FROM CONS
007630         PERFORM �t�@�C����
007640         MOVE 99 TO PROGRAM-STATUS
007650         EXIT PROGRAM.
007660*================================================================*
007670 �t�@�C���� SECTION.
007680*
007690     CLOSE �����}�X�^  ��f�ҏ��e ���Z�v�g�e �����f�[�^�e
                 �{�p�L�^�e  ��v�f�[�^�e.
007700*================================================================*
007710 �I������ SECTION.
007720*
007730     PERFORM �t�@�C����.
007740*================================================================*
007750 �G���[�\�� SECTION.
007760*
007770     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
007780     DISPLAY NC"�T�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
007790     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
007800     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
007810     ACCEPT  �L�[���� FROM CONS.
007820     PERFORM �t�@�C����.
007830     MOVE 99 TO PROGRAM-STATUS.
007840     EXIT PROGRAM.
007850*================================================================*
007860 ��f�ҏ��e�Ǎ� SECTION.
007870*
007880     READ ��f�ҏ��e NEXT
007890     AT END
007900         MOVE "YES" TO �I���t���O
007910     END-READ.
007920*================================================================*
007930 ��ƃt�@�C���쐬 SECTION.
007940*
007950     MOVE ZERO TO ����Ώۂ���e.
007960*
007970     OPEN OUTPUT ��ƃt�@�C���P
007980          MOVE NC"��P" TO �t�@�C����
007990          PERFORM �I�[�v���`�F�b�N
008000     OPEN OUTPUT ��ƃt�@�C���Q
008010          MOVE NC"��Q" TO �t�@�C����
008020          PERFORM �I�[�v���`�F�b�N
008030*
008040     MOVE �J�n�a��v�q      TO ��|�{�p�a��.
008050     MOVE �J�n�N�v�q        TO ��|�{�p�N.
008060     MOVE �J�n���v�q        TO ��|�{�p��.
008070     MOVE SPACE             TO ��|���҃J�i.
008080     MOVE SPACE             TO ��|���҃R�[�h.
008090     START ��f�ҏ��e   KEY IS >= ��|�{�p�a��N��
008100                                    ��|���҃J�i
008110                                    ��|���҃R�[�h
008120     IF ��ԃL�[ = "00"
008130         MOVE SPACE  TO �I���t���O
008140         PERFORM ��f�ҏ��e�Ǎ�
008150         PERFORM UNTIL ( �I���t���O = "YES" ) OR
008160                       ( ��|�{�p�a��N�� > �I���a��N���v�q )
008170*/����Ώۏ����̔���
008180                 IF ((�ی���ʂv�q     NOT = ZERO)              AND
008190                     (�ی���ʂv�q     NOT = ��|�ی����))     OR
008200*/���Ҏw�莞�A�}�Ԃ��܂܂�Ă��܂��וύX20205
008210*                    ((���Ҕԍ��v�q     NOT = ZERO)              AND
008220*                     (���Ҕԍ��v�q     NOT = ��|���Ҕԍ�))     OR
008230*                    ((�}�Ԃv�q         NOT = SPACE)             AND
008240*                     (�}�Ԃv�q         NOT = ��|�}��))         OR
008250                    ((���Ҕԍ��v�q    NOT = ZERO)               AND
008260                     (���҃R�[�h�v�q   NOT = ��|���҃R�[�h))   OR
008270                    ((�{�l�Ƒ��敪�v�q NOT = ZERO)              AND
008280                     (�{�l�Ƒ��敪�v�q NOT = ��|�{�l�Ƒ��敪)) OR
008290                     (��|�ی����         = 70 OR 80 OR 90)
008300*/�ΏۊO
008310                     CONTINUE
008320                 ELSE
008330*/��������v�q���ۗ�
008340                     MOVE "YES" TO �Ώۃt���O
008350                     IF ��������v�q NOT = ZERO
008360                         PERFORM �����������
008370                     END-IF
008380                     IF �Ώۃt���O = "YES"
008390                         PERFORM �f�[�^�`�F�b�N
008400                         IF ���s�L�[�v = "YES"
008410                             MOVE ��|�{�p�a��N�� TO �{�p�a��N���v�q
008420                             IF ��|�{�p�a��N�� = �J�n�a��N���v�q
008430                                 MOVE �J�n���v�q TO �J�n���v�o
008440                             ELSE
008450                                 MOVE 1          TO �J�n���v�o
008460                             END-IF
008470                             IF ��|�{�p�a��N�� = �I���a��N���v�q
008480                                 MOVE �I�����v�q TO �I�����v�o
008490                             ELSE
008500                                 PERFORM �������擾
008510                                 MOVE �󗝓��v   TO �I�����v�o
008520                             END-IF
008530                             PERFORM ��P���R�[�h�Z�b�g
008540*                           */���X�|���X�̌���̈�201312
008550                             IF �Ώۊ��҂e = 1
008560                                 PERFORM ��Q���R�[�h�Z�b�g
008570                             END-IF
008580                         END-IF
008590                     END-IF
008600                 END-IF
008610                 PERFORM ��f�ҏ��e�Ǎ�
008620         END-PERFORM
008630     END-IF.
008640     CLOSE ��ƃt�@�C���P ��ƃt�@�C���Q.
008650*================================================================*
008660 ��P���R�[�h�Z�b�g SECTION.
008670*
008680*/���X�|���X�̌���̈�201312
008690     MOVE ZERO TO �Ώۊ��҂e.
008700     INITIALIZE ��P�|���R�[�h.
008710     MOVE ��|���҃R�[�h   TO ��P�|���҃R�[�h.
008720     MOVE ��|���҃J�i     TO ��P�|���҃J�i.
008730     MOVE ��|���Ҏ���     TO ��P�|���Ҏ���.
008740     PERFORM VARYING �{�p���v�q FROM �J�n���v�o BY 1
008750             UNTIL ( �{�p���v�q > �I�����v�o )
008760         INITIALIZE ��P�|����
008770         MOVE ��|���Ҕԍ�  TO �{�L�|���Ҕԍ�
008780         MOVE ��|�}��      TO �{�L�|�}��
008790         MOVE ��|�{�p�a��  TO �{�L�|�{�p�a��
008800         MOVE ��|�{�p�N    TO �{�L�|�{�p�N
008810         MOVE ��|�{�p��    TO �{�L�|�{�p��
008820         MOVE �{�p���v�q    TO �{�L�|�{�p��
008830         READ �{�p�L�^�e
008840         NOT INVALID KEY
008850*/����f�[�^�̂Ȃ��ꍇ�̓��b�Z�[�W���o��20131
008860             MOVE 1 TO ����Ώۂ���e
008870             PERFORM ���Z�v�g�ďo���P
008880             PERFORM �������擾
008890             PERFORM ���ڂ��ƌv�Z
008900             PERFORM ��ƃt�@�C���Z�b�g
008910             PERFORM ��P���R�[�h����
008920*/���X�|���X�̌���̈�201312
008930             MOVE 1 TO �Ώۊ��҂e
008940         END-READ
008950     END-PERFORM.
008960*================================================================*
008970 ��ƃt�@�C���Z�b�g SECTION.
008980*
008990     MOVE ��|�{�p�a��       TO ��P�|�{�p�a��.
009000     MOVE ��|�{�p�N         TO ��P�|�{�p�N.
009010     MOVE ��|�{�p��         TO ��P�|�{�p��.
009020     MOVE �{�p���v�q         TO ��P�|�{�p��.
009030     MOVE �ꕔ���S���v�q     TO ��P�|�ꕔ���S��.
009040*================================================================*
009050 ��ƂQ�t�@�C���Z�b�g SECTION.
009060*
009070     MOVE ���Z�|�{�p�a��     TO ��Q�|�{�p�a��.
009080     MOVE ���Z�|�{�p�N       TO ��Q�|�{�p�N.
009090     MOVE ���Z�|�{�p��       TO ��Q�|�{�p��.
009100     MOVE �����v�v           TO ��Q�|�����v.
009110     MOVE �����v�v           TO ��Q�|�����v.
009120     MOVE ��Ìv�v           TO ��Q�|��Ìv.
009130     MOVE 㪖@�v�v           TO ��Q�|㪖@�v.
009140     MOVE �d�Ìv�v           TO ��Q�|�d�Ìv.
009150     MOVE ��p�v�v           TO ��Q�|��p�v.
009160     MOVE �ꕔ���S���v�v     TO ��Q�|���S�v.
009170*================================================================*
009180 ��Q���R�[�h�Z�b�g SECTION.
009190*
009200     INITIALIZE ��Q�|���R�[�h.
009210     MOVE SPACE TO �W�v�v.
009220     MOVE ��|���҃R�[�h   TO ��Q�|���҃R�[�h.
009230     PERFORM ���Z�v�g�ďo���Q.
009240     PERFORM �������擾�Q.
009250     PERFORM VARYING �{�p���v�q FROM 1 BY 1
009260             UNTIL ( �{�p���v�q > 31 )
009270         INITIALIZE ��P�|����
009280         MOVE ��|���Ҕԍ�  TO �{�L�|���Ҕԍ�
009290         MOVE ��|�}��      TO �{�L�|�}��
009300         MOVE ��|�{�p�a��  TO �{�L�|�{�p�a��
009310         MOVE ��|�{�p�N    TO �{�L�|�{�p�N
009320         MOVE ��|�{�p��    TO �{�L�|�{�p��
009330         MOVE �{�p���v�q    TO �{�L�|�{�p��
009340         READ �{�p�L�^�e
009350         NOT INVALID KEY
009360             PERFORM ���Z�v�g�ďo���P
009370             PERFORM �W�v
009380         END-READ
009390     END-PERFORM.
009400     PERFORM ��ƂQ�t�@�C���Z�b�g.
009410     PERFORM ��Q���R�[�h����.
009420*================================================================*
009430 ���Z�v�g�ďo���P SECTION.
009440*
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
009750     MOVE ��|�ꕔ���S��     TO �ꕔ���S���v�q.
009810*
009820*================================================================*
009830 ���Z�v�g�ďo���Q SECTION.
009840*
           IF ��|������� NOT = ZERO
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
010170*
011040*================================================================*
011050 �������擾 SECTION.
011060*
011070***********************************************
011080* �����f�[�^�Z�b�g                            *
011090***********************************************
011100*    ****************************************************************
011110*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
011120*    ****************************************************************
011130     MOVE ��|������             TO  �������v�q.
011140*
011150     MOVE ��|�������Z��         TO  �������Z���v�q.
           MOVE ��|���������k��       TO  ���k���v�q.
011160     MOVE ��|�Č���             TO  �Č����v�q.
011170     MOVE ��|���×�             TO  ���×��v�q.
011180     MOVE ��|���É��Z��         TO  ���É��Z���v�q.
011190*
011220     MOVE ��|�{�p���񋟗�     TO  �{�p���񋟗��v�q.
011230*
011240     COMPUTE ��P�|�������z = �������v�q + �������Z���v�q + �Č����v�q +
011250                              ���×��v�q + ���É��Z���v�q + �{�p���񋟗��v�q + ���k���v�q.
011260     MOVE ��|��p�z             TO ��P�|��p�z.
011270*
011280*================================================================*
011290 �������擾�Q SECTION.
011300*
011310     IF ���Z�v�g��ނv = "JYOS"
011320         MOVE ���Z�|�󋋎ҕ��S�z TO ��Q�|���S��
011330         MOVE ���Z�|���v         TO ��Q�|��p�z
011340         MOVE ���Z�|�������z     TO ��Q�|�����z
011350     ELSE
011360         MOVE ���Z�|�ꕔ���S��   TO ��Q�|���S��
011410         MOVE ���Z�|���v         TO ��Q�|��p�z
011420         MOVE ���Z�|�������z     TO ��Q�|�����z
011440     END-IF.
011450     MOVE SPACE  TO �p���t���O.
011460     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
011470               UNTIL (���ʂb�m�s > 5)
011480         EVALUATE ���|�]�A�敪(���ʂb�m�s)
011490         WHEN 1
011500             MOVE NC"��"  TO ��Q�|�]�A�敪(���ʂb�m�s)
011510         WHEN 2
011520             MOVE NC"�S"  TO ��Q�|�]�A�敪(���ʂb�m�s)
011530         WHEN 3
011540             MOVE NC"��"  TO ��Q�|�]�A�敪(���ʂb�m�s)
011550         WHEN 4
011560             MOVE NC"�]"  TO ��Q�|�]�A�敪(���ʂb�m�s)
011570         WHEN 5
011580             MOVE NC"��"  TO ��Q�|�]�A�敪(���ʂb�m�s)
011590         WHEN 9
011600             MOVE NC"�p"  TO ��Q�|�]�A�敪(���ʂb�m�s)
011610             MOVE "YES"   TO �p���t���O
011620         END-EVALUATE
011630     END-PERFORM.
011640     MOVE ���Z�|���Z������   TO ��Q�|�{�p��.
011650     MOVE ZEROS TO ���|����敪.
011660     READ ������}�X�^
011670     NOT INVALID KEY
011680         MOVE ���|���Z�v�g���t�敪     TO ���Z�v�g���t�敪�v
011690         MOVE ���|���Z�v�g���ғ��t�敪 TO ���Z�v�g���ғ��t�敪�v
011700     END-READ.
011710     PERFORM �ŏI�ʉ@���擾.
011720     EVALUATE ���Z�v�g���ғ��t�敪�v 
011730*    /  �ŏI�ʉ@�� /
011740     WHEN ZERO
011750*         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
011760         MOVE �ŏI�ʉ@���v TO ��Q�|�ʉ@��
011770         MOVE �ŏI�ʉ@���v TO ��Q�|�ʉ@��
011780*    /  ������ /
011790     WHEN 1 
011800         PERFORM �������擾
011810*         MOVE �󗝔N�v     TO ���҈ϔC�N�v
011820         MOVE �󗝌��v     TO ��Q�|�ʉ@��
011830         MOVE �󗝓��v     TO ��Q�|�ʉ@��
011840*    /  �󎚂Ȃ� /
011850     WHEN 9
011860*         MOVE ZERO         TO ���҈ϔC�N�v
011870         MOVE ZERO         TO ��Q�|�ʉ@��
011880         MOVE ZERO         TO ��Q�|�ʉ@��
011890*    /  ���̑��́A�ŏI�ʉ@�� /
011900     WHEN OTHER
011910*         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
011920         MOVE �ŏI�ʉ@���v TO ��Q�|�ʉ@��
011930         MOVE �ŏI�ʉ@���v TO ��Q�|�ʉ@��
011940     END-EVALUATE.
011950*================================================================*
011960 ���ڂ��ƌv�Z SECTION.
011970***********************************************
011980* �����f�[�^�Z�b�g                            *
011990***********************************************
011200     MOVE ��|�������q���Z��     TO  �������q���Z���v�q.
010840     MOVE ��|�^����×�         TO  �^���Ö@���v�q.
011210*
012000     COMPUTE ��P�|�������z = ��|���񏈒u�����v + �������q���Z���v�q + �^���Ö@���v�q.
012010*
012020     MOVE ��|��×��P     TO ��×��v�q(1).
012030     MOVE ��|��×��Q     TO ��×��v�q(2).
012040     MOVE ��|��×��R�W   TO ��×��R�W�v�q.
012050     MOVE ��|��×��R�O   TO ��×��R�O�v�q.
012060     COMPUTE ��×��v�q(3)   = ��×��R�W�v�q   + ��×��R�O�v�q.
012070     MOVE ��|��×��S�T   TO ��×��S�T�v�q.
012080     MOVE ��|��×��S�W   TO ��×��S�W�v�q.
012090     MOVE ��|��×��S�O   TO ��×��S�O�v�q.
012100     COMPUTE ��×��v�q(4)   = ��×��S�T�v�q   + ��×��S�W�v�q   + ��×��S�O�v�q.
012110     COMPUTE ��P�|��Ë��z = ��×��v�q(1) + ��×��v�q(2) + ��×��v�q(3) + ��×��v�q(4).
012120********************
012130* �����������Z�b�g *
012140********************
012150     MOVE ��|��㪖@���P   TO ��㪗��v�q(1).
012160     MOVE ��|��㪖@���Q   TO ��㪗��v�q(2).
012170     MOVE ��|��㪖@���R�W TO ��㪖@���R�W�v�q.
012180     MOVE ��|��㪖@���R�O TO ��㪖@���R�O�v�q.
012190     COMPUTE ��㪗��v�q(3)   = ��㪖@���R�W�v�q  + ��㪖@���R�O�v�q.
012200     MOVE ��|��㪖@���S�T TO ��㪖@���S�T�v�q.
012210     MOVE ��|��㪖@���S�W TO ��㪖@���S�W�v�q.
012220     MOVE ��|��㪖@���S�O TO ��㪖@���S�O�v�q.
012230     COMPUTE ��㪗��v�q(4)   = ��㪖@���S�T�v�q  + ��㪖@���S�W�v�q  + ��㪖@���S�O�v�q.
012240     COMPUTE ��㪗��v�v�q = ��㪗��v�q(1) + ��㪗��v�q(2) + ��㪗��v�q(3) + ��㪗��v�q(4).
012250*
012260     MOVE ��|��㪖@���P   TO ��㪗��v�q(1).
012270     MOVE ��|��㪖@���Q   TO ��㪗��v�q(2).
012280     MOVE ��|��㪖@���R�W TO ��㪖@���R�W�v�q.
012290     MOVE ��|��㪖@���R�O TO ��㪖@���R�O�v�q.
012300     COMPUTE ��㪗��v�q(3)   = ��㪖@���R�W�v�q  + ��㪖@���R�O�v�q.
012310     MOVE ��|��㪖@���S�T TO ��㪖@���S�T�v�q.
012320     MOVE ��|��㪖@���S�W TO ��㪖@���S�W�v�q.
012330     MOVE ��|��㪖@���S�O TO ��㪖@���S�O�v�q.
012340     COMPUTE ��㪗��v�q(4)   = ��㪖@���S�T�v�q  + ��㪖@���S�W�v�q  + ��㪖@���S�O�v�q.
012350     COMPUTE ��㪗��v�v�q = ��㪗��v�q(1) + ��㪗��v�q(2) + ��㪗��v�q(3) + ��㪗��v�q(4).
012360     COMPUTE ��P�|㪖@���z = ��㪗��v�v�q + ��㪗��v�v�q.
012370*
012380     MOVE ��|�d�×��P     TO �d�×��v�q(1).
012390     MOVE ��|�d�×��Q     TO �d�×��v�q(2).
012400     MOVE ��|�d�×��R�W   TO �d�×��R�W�v�q.
012410     MOVE ��|�d�×��R�O   TO �d�×��R�O�v�q.
012420     COMPUTE �d�×��v�q(3)  = �d�×��R�W�v�q  + �d�×��R�O�v�q.
012430     MOVE ��|�d�×��S�T   TO �d�×��S�T�v�q.
012440     MOVE ��|�d�×��S�W   TO �d�×��S�W�v�q.
012450     MOVE ��|�d�×��S�O   TO �d�×��S�O�v�q.
012460     COMPUTE �d�×��v�q(4)  = �d�×��S�T�v�q  + �d�×��S�W�v�q  + �d�×��S�O�v�q.
012470     COMPUTE ��P�|�d�Ë��z = �d�×��v�q(1) + �d�×��v�q(2) + �d�×��v�q(3) + �d�×��v�q(4).
012480*
012490*================================================================*
012500 �W�v SECTION.
012510***********************************************
012520* �����f�[�^�Z�b�g                            *
012530***********************************************
012540     MOVE ��|������             TO  �������v�q.
012550*
012560     MOVE ��|�������Z��         TO  �������Z���v�q.
           MOVE ��|���������k��       TO  ���k���v�q.
012570     MOVE ��|�Č���             TO  �Č����v�q.
012580     MOVE ��|���×�             TO  ���×��v�q.
012590     MOVE ��|���É��Z��         TO  ���É��Z���v�q.
012600     MOVE ��|�������q���Z��     TO  �������q���Z���v�q.
010840     MOVE ��|�^����×�         TO  �^���Ö@���v�q.
012610     MOVE ��|�{�p���񋟗�     TO  �{�p���񋟗��v�q.
012620     COMPUTE �����v�v = �����v�v + �������v�q + �������Z���v�q + �Č����v�q +
012630                        ���×��v�q + ���É��Z���v�q + �{�p���񋟗��v�q + ���k���v�q.
012640*
012650     COMPUTE �����v�v = �����v�v + ��|���񏈒u�����v + �������q���Z���v�q + �^���Ö@���v�q.
012660*
012670     MOVE ��|��×��P     TO ��×��v�q(1).
012680     MOVE ��|��×��Q     TO ��×��v�q(2).
012690     MOVE ��|��×��R�W   TO ��×��R�W�v�q.
012700     MOVE ��|��×��R�O   TO ��×��R�O�v�q.
012710     COMPUTE ��×��v�q(3)   = ��×��R�W�v�q   + ��×��R�O�v�q.
012720     MOVE ��|��×��S�T   TO ��×��S�T�v�q.
012730     MOVE ��|��×��S�W   TO ��×��S�W�v�q.
012740     MOVE ��|��×��S�O   TO ��×��S�O�v�q.
012750     COMPUTE ��×��v�q(4)   = ��×��S�T�v�q   + ��×��S�W�v�q   + ��×��S�O�v�q.
012760     COMPUTE ��Ìv�v = ��Ìv�v + ��×��v�q(1) + ��×��v�q(2) + ��×��v�q(3) + ��×��v�q(4).
012770********************
012780* �����������Z�b�g *
012790********************
012800     MOVE ��|��㪖@���P             TO ��㪗��v�q(1).
012810     MOVE ��|��㪖@���Q             TO ��㪗��v�q(2).
012820     MOVE ��|��㪖@���R�W           TO ��㪖@���R�W�v�q.
012830     MOVE ��|��㪖@���R�O           TO ��㪖@���R�O�v�q.
012840     COMPUTE ��㪗��v�q(3)   = ��㪖@���R�W�v�q  + ��㪖@���R�O�v�q.
012850     MOVE ��|��㪖@���S�T           TO ��㪖@���S�T�v�q.
012860     MOVE ��|��㪖@���S�W           TO ��㪖@���S�W�v�q.
012870     MOVE ��|��㪖@���S�O           TO ��㪖@���S�O�v�q.
012880     COMPUTE ��㪗��v�q(4)   = ��㪖@���S�T�v�q  + ��㪖@���S�W�v�q  + ��㪖@���S�O�v�q.
012890     COMPUTE ��㪗��v�v�q = ��㪗��v�q(1) + ��㪗��v�q(2) + ��㪗��v�q(3) + ��㪗��v�q(4).
012900*
012910     MOVE ��|��㪖@���P             TO ��㪗��v�q(1).
012920     MOVE ��|��㪖@���Q             TO ��㪗��v�q(2).
012930     MOVE ��|��㪖@���R�W           TO ��㪖@���R�W�v�q.
012940     MOVE ��|��㪖@���R�O           TO ��㪖@���R�O�v�q.
012950     COMPUTE ��㪗��v�q(3)   = ��㪖@���R�W�v�q  + ��㪖@���R�O�v�q.
012960     MOVE ��|��㪖@���S�T           TO ��㪖@���S�T�v�q.
012970     MOVE ��|��㪖@���S�W           TO ��㪖@���S�W�v�q.
012980     MOVE ��|��㪖@���S�O           TO ��㪖@���S�O�v�q.
012990     COMPUTE ��㪗��v�q(4)   = ��㪖@���S�T�v�q  + ��㪖@���S�W�v�q  + ��㪖@���S�O�v�q.
013000     COMPUTE ��㪗��v�v�q = ��㪗��v�q(1) + ��㪗��v�q(2) + ��㪗��v�q(3) + ��㪗��v�q(4).
013010     COMPUTE 㪖@�v�v = 㪖@�v�v + ��㪗��v�v�q + ��㪗��v�v�q.
013020*
013030     MOVE ��|�d�×��P             TO �d�×��v�q(1).
013040     MOVE ��|�d�×��Q             TO �d�×��v�q(2).
013050     MOVE ��|�d�×��R�W           TO �d�×��R�W�v�q.
013060     MOVE ��|�d�×��R�O           TO �d�×��R�O�v�q.
013070     COMPUTE �d�×��v�q(3)  = �d�×��R�W�v�q  + �d�×��R�O�v�q.
013080     MOVE ��|�d�×��S�T           TO �d�×��S�T�v�q.
013090     MOVE ��|�d�×��S�W           TO �d�×��S�W�v�q.
013100     MOVE ��|�d�×��S�O           TO �d�×��S�O�v�q.
013110     COMPUTE �d�×��v�q(4)  = �d�×��S�T�v�q  + �d�×��S�W�v�q  + �d�×��S�O�v�q.
013120     COMPUTE �d�Ìv�v = �d�Ìv�v + �d�×��v�q(1) + �d�×��v�q(2) + �d�×��v�q(3) + �d�×��v�q(4).
013130*
013140     COMPUTE ��p�v�v = ��p�v�v + ��|��p�z.
013150     COMPUTE �ꕔ���S���v�v = �ꕔ���S���v�v + �ꕔ���S���v�q.
013160*
013170*================================================================*
013180 ��P���R�[�h���� SECTION.
013190*
013200     WRITE ��P�|���R�[�h
013210     INVALID KEY
013220         MOVE NC"��P"  TO �t�@�C����
013230         PERFORM �G���[�\��
013240     END-WRITE.
013250*================================================================*
013260 ��Q���R�[�h���� SECTION.
013270*
013280     WRITE ��Q�|���R�[�h
013290     INVALID KEY
013300         MOVE NC"��Q"  TO �t�@�C����
013310         PERFORM �G���[�\��
013320     END-WRITE.
013330*================================================================*
013340 �f�[�^�`�F�b�N SECTION.
013350*
013360     MOVE SPACE          TO ���s�L�[�v.
013370* *****************************************************************
013380* * �������ʗL���`�F�b�N�F���ʐ� = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
013390* *****************************************************************
013400     MOVE ��|�{�p�a��   TO ���|�{�p�a��.
013410     MOVE ��|�{�p�N     TO ���|�{�p�N.
013420     MOVE ��|�{�p��     TO ���|�{�p��.
013430     MOVE ��|���Ҕԍ�   TO ���|���Ҕԍ�.
013440     MOVE ��|�}��       TO ���|�}��.
013450     READ �����f�[�^�e
013460     INVALID KEY
013470         MOVE SPACE  TO ���s�L�[�v
013480     NOT INVALID KEY
013490         IF ���|���ʐ� NOT = ZERO
013500*        *************************************************************
013510*        * �{�p�L�^�`�F�b�N�F�ʉ@�� = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
013520*        *************************************************************
013530             MOVE ���|���Ҕԍ�  TO �{�L�|���Ҕԍ�
013540             MOVE ���|�}��      TO �{�L�|�}��
013550             MOVE ���|�{�p�a��  TO �{�L�|�{�p�a��
013560             MOVE ���|�{�p�N    TO �{�L�|�{�p�N
013570             MOVE ���|�{�p��    TO �{�L�|�{�p��
013580             MOVE ZERO          TO �{�L�|�{�p��
013590             START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
013600                                          �{�L�|�{�p�a��N����
013610             END-START
013620             IF ��ԃL�[ = "00"
013630                 MOVE SPACE TO �I���t���O�Q
013640                 MOVE SPACE TO �{�p�L�^�L�v
013650                 PERFORM �{�p�L�^�e�Ǎ�
013660                 PERFORM UNTIL (�I���t���O�Q         = "YES"         ) OR
013670                               (�{�L�|���҃R�[�h NOT = ���|���҃R�[�h) OR
013680                               (�{�L�|�{�p�a��   NOT = ���|�{�p�a��  ) OR
013690                               (�{�L�|�{�p�N     NOT = ���|�{�p�N    ) OR
013700                               (�{�L�|�{�p��     NOT = ���|�{�p��    ) OR
013710                               (�{�p�L�^�L�v         = "YES"         )
013720                     MOVE "YES"  TO �{�p�L�^�L�v
013730                     MOVE "YES"  TO ���s�L�[�v
013740                 END-PERFORM
013750             ELSE
013760                 MOVE SPACE  TO ���s�L�[�v
013770             END-IF
013780         ELSE
013790             MOVE SPACE  TO ���s�L�[�v
013800         END-IF
013810     END-READ.
013820*
013830*================================================================*
013840 �{�p�L�^�e�Ǎ� SECTION.
013850*
013860     READ �{�p�L�^�e NEXT
013870     AT END
013880         MOVE "YES"  TO �I���t���O�Q
013890     END-READ.
013900*================================================================*
013910 ����������� SECTION.
013920*
013930     MOVE SPACE TO �Ώۃt���O
013940*/�J�Ў����ӂ̃f�[�^�͑ΏۊO
013950     IF ��|�ی���� NOT = 70 AND 80
013960         EVALUATE ��������v�q
013970         WHEN 01
013980             PERFORM ��������
013990         WHEN 02
014000             PERFORM ��Â̂ݔ���
014010         WHEN 03
014020             PERFORM �I������
014030         WHEN 04
014040             PERFORM ��������
014050             IF �Ώۃt���O = "YES"
014060                 PERFORM �I������
014070             END-IF
014080         WHEN OTHER
014090             CONTINUE
014100         END-EVALUATE
014110     END-IF.
014120*================================================================*
014130 �������� SECTION.
014140*
014150     MOVE ��|���Ҕԍ�  TO �{�L�|���Ҕԍ�
014160     MOVE ��|�}��      TO �{�L�|�}��
014170     MOVE ��|�{�p�a��  TO �{�L�|�{�p�a��
014180     MOVE ��|�{�p�N    TO �{�L�|�{�p�N
014190     MOVE ��|�{�p��    TO �{�L�|�{�p��
014200     MOVE ZERO          TO �{�L�|�{�p��
014210     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
014220                                  �{�L�|�{�p�a��N����
014230     END-START
014240     IF ��ԃL�[ = "00"
014250         MOVE SPACE TO �I���t���O�Q
014260         PERFORM �{�p�L�^�e�Ǎ�
014270         PERFORM UNTIL (�I���t���O�Q         = "YES"         ) OR
014280                       (�{�L�|���҃R�[�h NOT = ��|���҃R�[�h) OR
014290                       (�{�L�|�{�p�a��   NOT = ��|�{�p�a��  ) OR
014300                       (�{�L�|�{�p�N     NOT = ��|�{�p�N    ) OR
014310                       (�{�L�|�{�p��     NOT = ��|�{�p��    )
014320*
014330             IF �{�L�|�f�Ë敪 = 2
014340                 MOVE "YES" TO �Ώۃt���O
014350                 MOVE "YES" TO �I���t���O�Q
014360             END-IF
014370             PERFORM �{�p�L�^�e�Ǎ�
014380         END-PERFORM
014390     END-IF.
014400*================================================================*
014410 ��Â̂ݔ��� SECTION.
014420*
014430     MOVE ��|���Ҕԍ�  TO �{�L�|���Ҕԍ�
014440     MOVE ��|�}��      TO �{�L�|�}��
014450     MOVE ��|�{�p�a��  TO �{�L�|�{�p�a��
014460     MOVE ��|�{�p�N    TO �{�L�|�{�p�N
014470     MOVE ��|�{�p��    TO �{�L�|�{�p��
014480     MOVE ZERO          TO �{�L�|�{�p��
014490     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
014500                                  �{�L�|�{�p�a��N����
014510     END-START
014520     IF ��ԃL�[ = "00"
014530         MOVE SPACE TO �I���t���O�Q
014540         PERFORM �{�p�L�^�e�Ǎ�
014550         PERFORM UNTIL (�I���t���O�Q         = "YES"         ) OR
014560                       (�{�L�|���҃R�[�h NOT = ��|���҃R�[�h) OR
014570                       (�{�L�|�{�p�a��   NOT = ��|�{�p�a��  ) OR
014580                       (�{�L�|�{�p�N     NOT = ��|�{�p�N    ) OR
014590                       (�{�L�|�{�p��     NOT = ��|�{�p��    )
014600*
014610             IF �{�L�|�f�Ë敪 = 1
014620                 MOVE "YES" TO �Ώۃt���O
014630             ELSE
014640                 MOVE SPACE TO �Ώۃt���O
014650                 MOVE "YES" TO �I���t���O�Q
014660             END-IF
014670             PERFORM �{�p�L�^�e�Ǎ�
014680         END-PERFORM
014690     END-IF.
014700*================================================================*
014710 �I������ SECTION.
014720*
014730     MOVE "YES" TO �Ώۃt���O.
014740*
014750     MOVE ��|�{�p�a��   TO ���|�{�p�a��.
014760     MOVE ��|�{�p�N     TO ���|�{�p�N.
014770     MOVE ��|�{�p��     TO ���|�{�p��.
014780     MOVE ��|���Ҕԍ�   TO ���|���Ҕԍ�.
014790     MOVE ��|�}��       TO ���|�}��.
014800     READ �����f�[�^�e
014810     INVALID KEY
014820         MOVE SPACE  TO �Ώۃt���O
014830     NOT INVALID KEY
014840         IF ���|���ʐ� = ZERO
014850             MOVE SPACE TO �Ώۃt���O
014860         ELSE
014870             PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
014880                      UNTIL (���ʂb�m�s > ���|���ʐ�)
014890                 IF ���|�]�A�敪(���ʂb�m�s) = 9 OR 5
014900                     MOVE SPACE TO �Ώۃt���O
014910                 END-IF
014920             END-PERFORM
014930         END-IF
014940     END-READ.
014950*================================================================*
014960 �������擾 SECTION.
014970*
014980     MOVE �{�p�a��v�q TO ���|�����敪.
014990     READ �����}�X�^
015000     NOT INVALID KEY
015010         MOVE ���|�J�n����N TO �{�p����N�v
015020     END-READ.
015030     IF �{�p����N�v NOT = ZERO
015040        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
015050     END-IF.
015060*
015070     MOVE �{�p���v�q   TO �󗝌��v.
015080     EVALUATE �{�p���v�q
015090     WHEN 4
015100     WHEN 6
015110     WHEN 9
015120     WHEN 11
015130         MOVE 30 TO �󗝓��v
015140     WHEN 2
015150         DIVIDE 4 INTO �{�p����N�v GIVING    ���v
015160                                    REMAINDER �]�v
015170         END-DIVIDE
015180         IF �]�v = ZERO
015190             MOVE 29 TO �󗝓��v
015200         ELSE
015210             MOVE 28 TO �󗝓��v
015220         END-IF
015230     WHEN 1
015240     WHEN 3
015250     WHEN 5
015260     WHEN 7
015270     WHEN 8
015280     WHEN 10
015290     WHEN 12
015300         MOVE 31 TO �󗝓��v
015310     WHEN OTHER
015320          CONTINUE
015330     END-EVALUATE.
015340*
015350*================================================================*
015360 �ŏI�ʉ@���擾 SECTION.
015370     MOVE ZERO          TO �ŏI�ʉ@���v.
015380     MOVE ZERO          TO �ŏI�ʉ@���v.
015390     MOVE ��|���Ҕԍ�  TO �{�L�|���Ҕԍ�.
015400     MOVE ��|�}��      TO �{�L�|�}��.
015410     MOVE �{�p�a��v�q  TO �{�L�|�{�p�a��.
015420     MOVE �{�p�N�v�q    TO �{�L�|�{�p�N.
015430     MOVE �{�p���v�q    TO �{�L�|�{�p��.
015440     MOVE 31            TO �{�L�|�{�p��.
015450*
015460     START �{�p�L�^�e   KEY IS <= �{�L�|���҃R�[�h
015470                                  �{�L�|�{�p�a��N����
015480                                  REVERSED
015490     END-START.
015500*
015510     IF ��ԃL�[ = "00"
015520         MOVE SPACE  TO �I���t���O�Q
015530         PERFORM �{�p�L�^�e�Ǎ�
015540         IF ( �{�L�|���Ҕԍ� = ��|���Ҕԍ� ) AND ( �{�L�|�}�� = ��|�}�� ) AND 
015550            ( �{�L�|�{�p�a�� = �{�p�a��v�q ) AND ( �{�L�|�{�p�N = �{�p�N�v�q ) AND
015560            ( �{�L�|�{�p�� = �{�p���v�q )     AND ( �I���t���O�Q = SPACE )
015570             MOVE  �{�L�|�{�p��  TO  �ŏI�ʉ@���v
015580             MOVE  �{�L�|�{�p��  TO  �ŏI�ʉ@���v
015590         END-IF
015600     END-IF.
015610******************************************************************
015620 END PROGRAM YHN721.
015630******************************************************************
