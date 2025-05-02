000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCB7211.
000060 AUTHOR.                 �r�c �K�q
000070*
000080*----------------------------------------------------------------*
000090*         �����_���t���� �J���e���y�f�[�^�쐬�z�m��f�ҁE�N���ʁn
000100*         MED = YCB720 YCB7221P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2012-11-12
000130 DATE-COMPILED.          2012-11-12
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
000260     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY           IS �{�L�|�{�p�a��N����
000300                                                     �{�L�|���҃R�[�h
000310                             ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
000320                                                     �{�L�|�{�p�a��N����
000330                             FILE STATUS              IS  ��ԃL�[
000340                             LOCK        MODE         IS  AUTOMATIC.
000350     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000360                             ORGANIZATION             IS  INDEXED
000370                             ACCESS MODE              IS  DYNAMIC
000380                             RECORD KEY               IS  ��|�{�p�a��N��
000390                                                          ��|���҃R�[�h
000400                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000410                                                          ��|���҃J�i
000420                                                          ��|���҃R�[�h
000430                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000440                                                          ��|�{�p�a��N��
000450                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000460                                                          ��|�ی����
000470                                                          ��|�ی��Ҕԍ�
000480                                                          ��|���҃R�[�h
000490                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000500                                                          ��|������
000510                                                          ��|��p���S�Ҕԍ�
000520                                                          ��|���҃R�[�h
000530                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000540                                                          ��|�������
000550                                                          ��|��p���S�Ҕԍ�����
000560                                                          ��|���҃R�[�h
000570                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
000580                                                          ��|�{�p�a��N��
000590                                                          ��|���҃R�[�h
000600                             FILE STATUS              IS  ��ԃL�[
000610                             LOCK        MODE         IS  AUTOMATIC.
000620     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000630                             ORGANIZATION             IS  INDEXED
000640                             ACCESS MODE              IS  DYNAMIC
000650                             RECORD KEY               IS ���|�{�p�a��N��
000660                                                         ���|���҃R�[�h
000670                             ALTERNATE RECORD KEY     IS ���|���҃R�[�h
000680                                                         ���|�{�p�a��N��
000690                             FILE STATUS              IS  ��ԃL�[
000700                             LOCK        MODE         IS  AUTOMATIC.
000710     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000720                             ORGANIZATION             IS  INDEXED
000730                             ACCESS MODE              IS  DYNAMIC
000740                             RECORD KEY               IS  ���|�����敪
000750                             FILE STATUS              IS  ��ԃL�[
000760                             LOCK        MODE         IS  AUTOMATIC.
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
000690     SELECT  ��v�f�[�^�e    ASSIGN      TO        KAIKEIL
000700                             ORGANIZATION             IS  INDEXED
000710                             ACCESS MODE              IS  DYNAMIC
000089                             RECORD KEY               IS  ��|�{�p�a��N����
000090                                                          ��|���҃R�[�h
000092                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000093                                                          ��|�{�p�a��N����
000790                             FILE STATUS              IS  ��ԃL�[
000800                             LOCK        MODE         IS  AUTOMATIC.
000770     SELECT  ��ƃt�@�C���P    ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W72111L.DAT"
000780                             ORGANIZATION             IS  INDEXED
000790                             ACCESS                   IS  DYNAMIC
000800                             RECORD      KEY    IS  ��P�|�{�p�a��N����
000810                             FILE        STATUS       IS  ��ԃL�[
000820                             LOCK        MODE         IS  AUTOMATIC.
000830******************************************************************
000840*                      DATA DIVISION                             *
000850******************************************************************
000860 DATA                    DIVISION.
000870 FILE                    SECTION.
000880*                           �m�q�k��  �Q�T�U�n
000890 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
000900     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
000910*                           �m�q�k��  �R�Q�O�n
000920 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
000930     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000940*                           �m�q�k��  �U�S�O�n
000950 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
000960     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000970*                           �m�q�k��  �P�Q�W�n
000980 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000990     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000600*                           �m�q�k��  �W�R�Q�n
000610 FD  �����t�@�C��        BLOCK CONTAINS 1     RECORDS.
000620     COPY MEMO           OF    XFDLIB JOINING ���� AS PREFIX.
001060*                           �m�q�k��  �T�P�Q�n
001070 FD  ��v�f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001080     COPY KAIKEI     OF  XFDLIB  JOINING   ��   AS  PREFIX.
001000*                           �m�q�k��  �Q�T�U�n
001010 FD  ��ƃt�@�C���P RECORD  CONTAINS 256 CHARACTERS.
001020 01 ��P�|���R�[�h.
001030    03 ��P�|���R�[�h�L�[.
001040       05  ��P�|�{�p�a��N����.
001050          07 ��P�|�{�p�a��               PIC 9.
001060          07 ��P�|�{�p�N��.
001070             09 ��P�|�{�p�N              PIC 9(2).
001080             09 ��P�|�{�p��              PIC 9(2).
001090          07 ��P�|�{�p��                 PIC 9(2).
001100    03 ��P�|���R�[�h�f�[�^.
001110       05 ��P�|���҃R�[�h.
001120          07 ��P�|���Ҕԍ�               PIC 9(6).
001130          07 ��P�|�}��                   PIC X.
001080       05  ��P�|���`�F�b�N               PIC 9.
             05 ��P�|��p�z                    PIC 9(6).
001200       05 ��P�|�ꕔ���S��                PIC 9(5).
001551       05 ��P�|�R�����g                  PIC X(100).
      *
             05 ��P�|������                    PIC 9(5).
             05 ��P�|�������Z.
                07 ��P�|�������ԊO             PIC 9.
                07 ��P�|�����x��               PIC 9.
                07 ��P�|�����[��               PIC 9.
             05 ��P�|�������Z��                PIC 9(5).
             05 ��P�|�Č���                    PIC 9(5).
             05 ��P�|����.
                07 ��P�|���Ë���               PIC 9(2)V9.
                07 ��P�|���É�               PIC 9(2).
                07 ��P�|���×�                 PIC 9(5).
                07 ��P�|���É��Z��             PIC 9(5).
                07 ��P�|���É��Z.
                   09 ��P�|���Ö��            PIC 9.
                   09 ��P�|���Ó�H            PIC 9.
                   09 ��P�|���Ö\���J          PIC 9.
                   09 ��P�|���Î��ԊO          PIC 9.
             05 ��P�|���񏈒u���v              PIC 9(6).
             05 ��P�|��×�                    PIC 9(6).
             05 ��P�|��㪖@��                  PIC 9(5).
             05 ��P�|��㪖@��                  PIC 9(5).
             05 ��P�|�d�×�                    PIC 9(5).
             05 ��P�|��                        PIC 9(1).
             05 ��P�|��                        PIC 9(1).
             05 ��P�|��                        PIC 9(1).
             05 ��P�|�������q���Z��            PIC 9(5).
             05 ��P�|�^����×�                PIC 9(4).
001210       05 FILLER                          PIC X(54).
001220*
001230*----------------------------------------------------------------*
001240******************************************************************
001250*                WORKING-STORAGE SECTION                         *
001260******************************************************************
001270 WORKING-STORAGE         SECTION.
001280 01 �L�[����                         PIC X     VALUE SPACE.
001290 01 ��ԃL�[                         PIC X(2)  VALUE SPACE.
001300 01 ���s�L�[�v                       PIC X(3)  VALUE SPACE.
001310 01 �I���t���O                       PIC X(3)  VALUE SPACE.
001320 01 �I���t���O�Q                     PIC X(3) VALUE SPACE.
001330 01 �{�p�L�^�L�v                     PIC X(3) VALUE SPACE.
001340 01 �{�p�a��N���v�q.
001350    03 �{�p�a��v�q                  PIC 9(1)  VALUE ZERO.
001360    03 �{�p�N���v�q.
001370       05 �{�p�N�v�q                 PIC 9(2)  VALUE ZERO.
001380       05 �{�p���v�q                 PIC 9(2)  VALUE ZERO.
001390       05 �{�p���v�q                 PIC 9(2)  VALUE ZERO.
001400 01 ���҃R�[�h�v�q.
001410    03 ���Ҕԍ��v�q                  PIC 9(6)  VALUE ZERO.
001420    03 �}�Ԃv�q                      PIC X     VALUE SPACE.
001430 01 ������[�h�e�v�q                 PIC 9(1)  VALUE ZERO.
001440 01 �����ƃ��[�h�v�q                 PIC 9(1)  VALUE ZERO.
001450 01 �������[�h�v�q                   PIC 9(1)  VALUE ZERO.
001460 01 �N�����[�h�v�q                   PIC 9(1)  VALUE ZERO.
001480 01 ���v���[�h�v�q                   PIC 9(1)  VALUE ZERO.
001630 01 �R�����g���[�h�v�q               PIC 9(1)  VALUE ZERO.
       01 �J�n���v�q                       PIC 9(2)  VALUE ZERO.
       01 �I�����v�q                       PIC 9(2)  VALUE ZERO.
001490 01 �{�p�����v                       PIC 9(2)  VALUE ZERO.
001500 01 �t�@�C����                       PIC N(10) VALUE SPACE.
001510 01 ���ʂb�m�s                       PIC 9(2)  VALUE ZERO.
001520 01 �J�E���^                         PIC 9(2)  VALUE ZERO.
001530*
001540 01 �ޔ����ڂf�v.
001550   03 ���Z�v�g��ނv                 PIC X(4).
001560   03 ���Z�v�g��ނf�v               PIC X(4).
001570   03 ���Z�v�g��ʂf�v               PIC 9(2).
001580*
001590*
001600 01 ����.
001610    03 �������v�q                    PIC 9(4)  VALUE ZERO.
001620    03 �������Z���v�q                PIC 9(4)  VALUE ZERO.
001630    03 �x���v�q                      PIC 9     VALUE ZERO.
001640    03 �[��v�q                      PIC 9     VALUE ZERO.
001650    03 ���ԊO�v�q                    PIC 9     VALUE ZERO.
001660    03 �f�Î��v�q                    PIC 9(2)  VALUE ZERO.
001670    03 �f�Õ��v�q                    PIC 9(2)  VALUE ZERO.
001680    03 �Č����v�q                    PIC 9(4)  VALUE ZERO.
001690    03 ���Ö�Ԃv�q                  PIC 9     VALUE ZERO.
001700    03 ���Ó�H�v�q                  PIC 9     VALUE ZERO.
001710    03 ���Ö\���v�q                  PIC 9     VALUE ZERO.
001720    03 ���É񐔂v�q                  PIC 9(2)  VALUE ZERO.
001730    03 ���Ë����v�q                  PIC 9(3)V9 VALUE ZERO.
001740    03 ���×��v�q                    PIC 9(6)  VALUE ZERO.
001750    03 ���É��Z���v�q                PIC 9(5)  VALUE ZERO.
001760    03 �d㪗��v�q                    PIC 9(4)  VALUE ZERO.
001770    03 �ꕔ���S���v�q                PIC 9(5)  VALUE ZERO.
003140    03 �^����×��v�q                PIC 9(4)  VALUE ZERO.
001780    03 �{�p���񋟗��v�q            PIC 9(6)  VALUE ZERO.
001790    03 ���ʂv                        OCCURS 9.
001800       05 ���񏈒u���v�q             PIC 9(4)  VALUE ZERO.
001810       05 ��×��v�q                 PIC 9(4)  VALUE ZERO.
001820       05 ��㪗��v�q                 PIC 9(4)  VALUE ZERO.
001830       05 ��㪗��v�q                 PIC 9(4)  VALUE ZERO.
001840       05 �d�×��v�q                 PIC 9(4)  VALUE ZERO.
001850       05 ���ʌv�v�q                 PIC 9(4)  VALUE ZERO.
001860       05 �����v�q                   PIC 9     VALUE ZERO.
001870    03 �����v�q                      PIC 9(2)  OCCURS 5 VALUE ZERO.
001880    03 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
001890    03 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
001900    03 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
001910    03 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
001920    03 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
001930    03 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
001940    03 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
001950    03 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
001960    03 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
001970    03 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
001980    03 �d�×��R�O�v�q                PIC 9(4)  VALUE ZERO.
001990    03 �d�×��R�W�v�q                PIC 9(4)  VALUE ZERO.
002000    03 �d�×��S�O�v�q                PIC 9(4)  VALUE ZERO.
002010    03 �d�×��S�T�v�q                PIC 9(4)  VALUE ZERO.
002020    03 �d�×��S�W�v�q                PIC 9(4)  VALUE ZERO.
002030    03 ��×��R�O�v�q                PIC 9(4)  VALUE ZERO.
002040    03 ��×��R�W�v�q                PIC 9(4)  VALUE ZERO.
002050    03 ��×��S�O�v�q                PIC 9(4)  VALUE ZERO.
002060    03 ��×��S�T�v�q                PIC 9(4)  VALUE ZERO.
002070    03 ��×��S�W�v�q                PIC 9(4)  VALUE ZERO.
002080    03 ��㪗��v�v�q                  PIC 9(4)  VALUE ZERO.
002090    03 ��㪗��v�v�q                  PIC 9(4)  VALUE ZERO.
002100    03 �d�×��v�v�q                  PIC 9(4)  VALUE ZERO.
002110*
002120 01 �ی���ʂv                       PIC 9(2)  VALUE ZERO.
002130 01 �����ʂv                       PIC 9(2)  VALUE ZERO.
002140 01 ������ʂv                       PIC 9(2)  VALUE ZERO.
002150 01 ���S�z�v�Q                       PIC 9(5)  VALUE ZERO.
002160 01 ��p�z�v�Q                       PIC 9(5)  VALUE ZERO.
002170 01 �{�p�݌v�񐔂v                   PIC 9(3)  VALUE ZERO.
002180*
002190 01 �����a��N�����v.
002200     03 �����a��N���v.
002210         05 �����a��v               PIC 9.
002220         05 �����N���v.
002230            07 �����N�v              PIC 9(2).
002240            07 �������v              PIC 9(2).
002250     03 �������v                     PIC 9(2).
002260*
002270 01 �J�n�a��N�����v.
002280     03 �J�n�a��N���v.
002290         05 �J�n�a��v               PIC 9.
002300         05 �J�n�N���v.
002310            07 �J�n�N�v              PIC 9(2).
002320            07 �J�n���v              PIC 9(2).
002330     03 �J�n���v                     PIC 9(2).
002340*
002350 01 �I���a��N�����v.
002360     03 �I���a��N���v.
002370         05 �I���a��v               PIC 9.
002380         05 �I���N���v.
002390            07 �I���N�v              PIC 9(2).
002400            07 �I�����v              PIC 9(2).
002410     03 �I�����v                     PIC 9(2).
002420*
002440 01 �v�Z�a��N���v.
002450     03 �v�Z�a��v                     PIC 9.
002460     03 �v�Z�N���v.
002470        05 �v�Z�N�v                    PIC 9(2).
002480        05 �v�Z���v                    PIC 9(2).
002490     03 �v�Z���v                       PIC 9(2).
002500*
002510 01 �������v                           PIC 9(2) VALUE ZERO.
002520 01 ���ʐ��v                           PIC 9 VALUE ZERO.
002530 01 �ő�o�^���v                       PIC 9 VALUE ZERO.
002540 01 ���ʒ��v                           PIC 9(2) VALUE 1.
002550 01 ���v                               PIC 9(4) VALUE ZERO.
002560 01 ��]�v                             PIC 9(4) VALUE ZERO.
002570 01 �������v                           PIC N(6) VALUE SPACE.
002580*
002590 01 ��Βl�v                           PIC 9(6).
002600 01 ��Βl���v                         PIC 9(3).
002610 01 ��Βl���v�Q                       PIC 9(3).
002620 01 �����o�ߓ��v                       PIC 9(3).
002630 01 ����N�v                           PIC 9(4).
002640 01 ����N���v                         PIC 9(2).
002650 01 ����N���Q�v                       PIC 9(2).
002660 01 �[�N�񐔂v                         PIC 9(2).
002670 01 �ʏ�N�񐔂v                       PIC 9(2).
002680 01 �ʏ�N��Βl�v                     PIC 9(6).
002690 01 �[�N��Βl�v                       PIC 9(6).
002700 01 �N��Βl�v                         PIC 9(6).
002710 01 ������Βl�v                       PIC 9(6).
002720 01 �{�p��Βl�v                       PIC 9(6).
       01 ���Z�N�v                           PIC 9(4).
002730*
      */��㪖@�̍��v�𑽕��ʒ�����̍��v�ɕύX(���������͂��肦�Ȃ��B�[���͎l�̌ܓ��j
      *
       01 �����ʂb�m�s                       PIC 9(2) VALUE ZERO.
      *
       01 �{�p�a��N�����b�v.
         03 �{�p�a��N���b�v.
           05 �{�p�a��b�v                   PIC 9.
           05 �{�p�N���b�v.
              07 �{�p�N�b�v                  PIC 9(2).
              07 �{�p���b�v                  PIC 9(2).
         03 �{�p���b�v                       PIC 9(2).
      *
       01 �������r�j�v.
         03 �������j�v  OCCURS 9.
           05 �����ʒ������j�v               PIC 9(3) VALUE ZERO.
      *
      */�G���[�\���̏C��
       01 �G���[�\���e                       PIC 9(1)  VALUE ZERO.
      *
002740******************************************************************
002750*                          �A������                              *
002760******************************************************************
002770*
002780****************
002790* ��ʓ��͏�� *
002800****************
002810 01 �A���|���̓f�[�^�x�b�a�V�Q�O IS EXTERNAL.
002820    03 �A���|�{�p�a��N��.
002830       05 �A���|�{�p�a��                  PIC 9(1).
002840       05 �A���|�{�p�N                    PIC 9(2).
002850       05 �A���|�{�p��                    PIC 9(2).
002860    03 �A���|�J�n���t.
002870       05 �A���|�J�n��                    PIC 9(2).
002880    03 �A���|�I�����t.
002890       05 �A���|�I����                    PIC 9(2).
002900    03 �A���|���҃R�[�h.
002910       05 �A���|���Ҕԍ�                  PIC 9(6).
002920       05 �A���|�}��                      PIC X(1).
002930    03 �A���|������[�h�e                 PIC 9(1).
002940    03 �A���|�����ƃ��[�h                 PIC 9(1).
002950    03 �A���|�������[�h                   PIC 9(1).
          03 �A���|�R�����g���[�h               PIC 9(1).
          03 �A���|����i��                     PIC 9(2).
      */�A�����̎�������̗L��0302
          03 �A���|�������[�h                   PIC X(4).
006570*
006580 01 �A���|�L�[ IS EXTERNAL.
006590    03 �A���|�ی����                  PIC 9(2).
006600*
006610 01 �A���|�L�[ IS EXTERNAL.
006620    03  �A���|���b�Z�[�W                 PIC N(20).
      *
009182* �m�F���b�Z�[�W�p (�Q�s)
009183 01 �A���V�|�L�[ IS EXTERNAL.
009184    03  �A���V�|���b�Z�[�W�P             PIC X(40).
009185    03  �A���V�|���b�Z�[�W�Q             PIC X(40).
      *
006700******************************************************************
006710*                      PROCEDURE  DIVISION                       *
006720******************************************************************
006730 PROCEDURE               DIVISION.
006740************
006750*           *
006760* ��������   *
006770*           *
006780************
006790     PERFORM �t�@�C���I�[�v��.
006800     PERFORM �A�����ڑҔ�.
006810************
006820*           *
006830* �又��     *
006840*           *
006850************
006890     IF (�����ƃ��[�h�v�q = 1) OR (�R�����g���[�h�v�q = 1)
006900         PERFORM ��ƃt�@�C���쐬
006910     END-IF.
006920************
006930*           *
006940* �I������   *
006950*           *
006960************
006970     PERFORM �I������.
006980     MOVE ZERO TO PROGRAM-STATUS.
006990     EXIT PROGRAM.
007000*
007010*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
007020*================================================================*
007030 �A�����ڑҔ� SECTION.
007040*
007050     MOVE �A���|���Ҕԍ�       TO ���Ҕԍ��v�q.
007060     MOVE �A���|�}��           TO �}�Ԃv�q.
007070     MOVE �A���|�{�p�a��       TO �{�p�a��v�q.
007080     MOVE �A���|�{�p�N         TO �{�p�N�v�q.
007090     MOVE �A���|�{�p��         TO �{�p���v�q.
007100     MOVE �A���|������[�h�e   TO ������[�h�e�v�q.
007110     MOVE �A���|�����ƃ��[�h   TO �����ƃ��[�h�v�q.
007120     MOVE �A���|�������[�h     TO �������[�h�v�q.
004540     MOVE �A���|�R�����g���[�h TO �R�����g���[�h�v�q.
      *
           MOVE �A���|�J�n��         TO �J�n���v�q.
           MOVE �A���|�I����         TO �I�����v�q.
007160*================================================================*
007170 �t�@�C���I�[�v�� SECTION.
007180*
007190     OPEN INPUT �{�p�L�^�e.
007200         MOVE NC"�{�L" TO �t�@�C����.
007210         PERFORM �I�[�v���`�F�b�N.
007220     OPEN INPUT ��f�ҏ��e.
007230         MOVE NC"��f" TO �t�@�C����.
007240         PERFORM �I�[�v���`�F�b�N.
007250     OPEN INPUT �����f�[�^�e.
007260         MOVE NC"����" TO �t�@�C����.
007270         PERFORM �I�[�v���`�F�b�N.
007280     OPEN INPUT �����}�X�^.
007290         MOVE NC"����" TO �t�@�C����.
007300         PERFORM �I�[�v���`�F�b�N.
002780     OPEN INPUT �����t�@�C��.
002790         MOVE NC"����"         TO �t�@�C����.
002800         PERFORM �I�[�v���`�F�b�N.
003060     OPEN INPUT ��v�f�[�^�e.
003070         MOVE NC"��v" TO �t�@�C����.
003080         PERFORM �I�[�v���`�F�b�N.
007310*================================================================*
007320 �I�[�v���`�F�b�N SECTION.
007330*
007340     IF ��ԃL�[  NOT =  "00"
007350         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
007360         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
007370         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
007380                                                 UPON CONS
003131*-----------------------------------------*
003132         CALL "actcshm"  WITH C LINKAGE
003133*-----------------------------------------*
007390         ACCEPT  �L�[���� FROM CONS
007400         PERFORM �t�@�C����
007410         MOVE 99 TO PROGRAM-STATUS
007420         EXIT PROGRAM.
007430*================================================================*
007440 �G���[�\�� SECTION.
007450*
007460     DISPLAY NC"��ԃL�[" ��ԃL�[                              UPON CONS.
007470     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����                UPON CONS.
007480     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"               UPON CONS.
007490     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
003131*-----------------------------------------*
003132     CALL "actcshm"  WITH C LINKAGE.
003133*-----------------------------------------*
007500     ACCEPT  �L�[���� FROM CONS.
007510     PERFORM �t�@�C����.
007520     MOVE 99 TO PROGRAM-STATUS.
007530     EXIT PROGRAM.
007540*================================================================*
008520 �f�[�^�`�F�b�N SECTION.
008530*
008540     MOVE SPACE  TO ���s�L�[�v.
008550* *****************************************************************
008560* * �������ʗL���`�F�b�N�F���ʐ� = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
008570* *****************************************************************
008580     MOVE ��|�{�p�a��   TO ���|�{�p�a��.
008590     MOVE ��|�{�p�N     TO ���|�{�p�N.
008600     MOVE ��|�{�p��     TO ���|�{�p��.
008610     MOVE ��|���Ҕԍ�   TO ���|���Ҕԍ�.
008620     MOVE ��|�}��       TO ���|�}��.
008630     READ �����f�[�^�e
008640     INVALID KEY
008650         MOVE SPACE  TO ���s�L�[�v
008660     NOT INVALID KEY
008670         IF ���|���ʐ� NOT = ZERO
008680*        *************************************************************
008690*        * �{�p�L�^�`�F�b�N�F�ʉ@�� = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
008700*        *************************************************************
008710             MOVE ���|���Ҕԍ�  TO �{�L�|���Ҕԍ�
008720             MOVE ���|�}��      TO �{�L�|�}��
008730             MOVE ���|�{�p�a��  TO �{�L�|�{�p�a��
008740             MOVE ���|�{�p�N    TO �{�L�|�{�p�N
008750             MOVE ���|�{�p��    TO �{�L�|�{�p��
008760             MOVE ZERO          TO �{�L�|�{�p��
008770             START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
008780                                          �{�L�|�{�p�a��N����
008790             END-START
008800             IF ��ԃL�[ = "00"
008810                 MOVE SPACE TO �I���t���O�Q
008820                 MOVE SPACE TO �{�p�L�^�L�v
008830                 PERFORM �{�p�L�^�e�Ǎ�
008840                 PERFORM UNTIL ( �I���t���O�Q         = "YES"          ) OR
008850                               ( �{�L�|���҃R�[�h NOT = ���|���҃R�[�h ) OR
008860                               ( �{�L�|�{�p�a��   NOT = ���|�{�p�a��   ) OR
008870                               ( �{�L�|�{�p�N     NOT = ���|�{�p�N     ) OR
008880                               ( �{�L�|�{�p��     NOT = ���|�{�p��     ) OR
008890                               ( �{�p�L�^�L�v         = "YES"          )
008900                     MOVE "YES"  TO �{�p�L�^�L�v
008910                     MOVE "YES"  TO ���s�L�[�v
008920                 END-PERFORM
008930             ELSE
008940                 MOVE SPACE  TO ���s�L�[�v
008950             END-IF
008960         ELSE
008970             MOVE  NC"�f�[�^������܂���B" TO �A���|���b�Z�[�W
008980             CALL   "MSG001"
008990             CANCEL "MSG001"
009000             MOVE SPACE  TO ���s�L�[�v
009010         END-IF
009020     END-READ.
009030*
009380*================================================================*
009390 ���Z�v�g�ďo���Q SECTION.
009400*
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
009750     MOVE ��|�ꕔ���S�� TO �ꕔ���S���v�q.
009770*
011430*================================================================*
011440 ���ڂ��ƌv�Z SECTION.
011450***********************************************
011460* �����f�[�^�Z�b�g                            *
011470***********************************************
011480     PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 9
011490         MOVE ��|���񏈒u��(�J�E���^) TO ���񏈒u���v�q(�J�E���^)
011500     END-PERFORM.
011510     MOVE ��|��×��P     TO ��×��v�q(1).
011520     MOVE ��|��×��Q     TO ��×��v�q(2).
011530     MOVE ��|��×��R�W   TO ��×��R�W�v�q.
011540     MOVE ��|��×��R�O   TO ��×��R�O�v�q.
011550     COMPUTE ��×��v�q(3)  = ��×��R�W�v�q   + ��×��R�O�v�q.
011560     MOVE ��|��×��S�T   TO ��×��S�T�v�q.
011570     MOVE ��|��×��S�W   TO ��×��S�W�v�q.
011580     MOVE ��|��×��S�O   TO ��×��S�O�v�q.
011590     COMPUTE ��×��v�q(4)  = ��×��S�T�v�q   + ��×��S�W�v�q   + ��×��S�O�v�q.
011600     PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 9
011610         COMPUTE ���ʌv�v�q(�J�E���^) = ��×��v�q(�J�E���^) + ���񏈒u���v�q(�J�E���^)
011620     END-PERFORM.
011630********************
011640* �����������Z�b�g *
011650********************
011660     MOVE ��|��㪖@���P             TO ��㪗��v�q(1).
011670     MOVE ��|��㪖@���Q             TO ��㪗��v�q(2).
011680     MOVE ��|��㪖@���R�W           TO ��㪖@���R�W�v�q.
011690     MOVE ��|��㪖@���R�O           TO ��㪖@���R�O�v�q.
011700     COMPUTE ��㪗��v�q(3)   = ��㪖@���R�W�v�q  + ��㪖@���R�O�v�q.
011710     MOVE ��|��㪖@���S�T           TO ��㪖@���S�T�v�q.
011720     MOVE ��|��㪖@���S�W           TO ��㪖@���S�W�v�q.
011730     MOVE ��|��㪖@���S�O           TO ��㪖@���S�O�v�q.
011740     COMPUTE ��㪗��v�q(4)   = ��㪖@���S�T�v�q  + ��㪖@���S�W�v�q  + ��㪖@���S�O�v�q.
011750     COMPUTE ��㪗��v�v�q = ��㪗��v�q(1) + ��㪗��v�q(2) + ��㪗��v�q(3) + ��㪗��v�q(4).
011760*
011770     MOVE ��|��㪖@���P             TO ��㪗��v�q(1).
011780     MOVE ��|��㪖@���Q             TO ��㪗��v�q(2).
011790     MOVE ��|��㪖@���R�W           TO ��㪖@���R�W�v�q.
011800     MOVE ��|��㪖@���R�O           TO ��㪖@���R�O�v�q.
011810     COMPUTE ��㪗��v�q(3)   = ��㪖@���R�W�v�q  + ��㪖@���R�O�v�q.
011820     MOVE ��|��㪖@���S�T           TO ��㪖@���S�T�v�q.
011830     MOVE ��|��㪖@���S�W           TO ��㪖@���S�W�v�q.
011840     MOVE ��|��㪖@���S�O           TO ��㪖@���S�O�v�q.
011850     COMPUTE ��㪗��v�q(4)   = ��㪖@���S�T�v�q  + ��㪖@���S�W�v�q  + ��㪖@���S�O�v�q.
011860     COMPUTE ��㪗��v�v�q = ��㪗��v�q(1) + ��㪗��v�q(2) + ��㪗��v�q(3) + ��㪗��v�q(4).
011870*
011880     MOVE ��|�d�×��P             TO �d�×��v�q(1).
011890     MOVE ��|�d�×��Q             TO �d�×��v�q(2).
011900     MOVE ��|�d�×��R�W           TO �d�×��R�W�v�q.
011910     MOVE ��|�d�×��R�O           TO �d�×��R�O�v�q.
011920     COMPUTE �d�×��v�q(3)  = �d�×��R�W�v�q  + �d�×��R�O�v�q.
011930     MOVE ��|�d�×��S�T           TO �d�×��S�T�v�q.
011940     MOVE ��|�d�×��S�W           TO �d�×��S�W�v�q.
011950     MOVE ��|�d�×��S�O           TO �d�×��S�O�v�q.
011960     COMPUTE �d�×��v�q(4)  = �d�×��S�T�v�q  + �d�×��S�W�v�q  + �d�×��S�O�v�q.
011970     COMPUTE �d�×��v�v�q = �d�×��v�q(1) + �d�×��v�q(2) + �d�×��v�q(3) + �d�×��v�q(4).
011980*
011990     COMPUTE �d㪗��v�q = ��㪗��v�v�q + ��㪗��v�v�q + �d�×��v�v�q.
012000*
012080*================================================================*
012090 �{�p�L�^�e�Ǎ� SECTION.
012100*
012110     READ �{�p�L�^�e NEXT
012120     AT END
012130         MOVE "YES" TO �I���t���O�Q
012140     END-READ.
012150*================================================================*
012160 ��f�ҏ��e�Ǎ� SECTION.
012170*
012180     READ ��f�ҏ��e NEXT
012190     AT END
012200         MOVE "YES" TO �I���t���O
012210     END-READ.
012220*================================================================*
012230 �t�@�C������ SECTION.
012240*
012250     WRITE ��P�|���R�[�h
012260     INVALID KEY
012270         MOVE NC"���"  TO �t�@�C����
012280         PERFORM �G���[�\��
012290     END-WRITE.
012300*================================================================*
012310 �t�@�C���� SECTION.
012320*
012330     CLOSE �{�p�L�^�e ��f�ҏ��e �����f�[�^�e 
                 �����}�X�^ �����t�@�C�� ��v�f�[�^�e.
012340*================================================================*
012350 �I������ SECTION.
012360*
012370     PERFORM �t�@�C����.
012380*================================================================*
012390 ����N�擾 SECTION.
012400*
012410     MOVE �v�Z�a��v TO ���|�����敪.
012420     READ �����}�X�^
012430     INVALID KEY
012440         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
012450         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
012460                                                  UPON CONS
003131*-----------------------------------------*
003132         CALL "actcshm"  WITH C LINKAGE
003133*-----------------------------------------*
012470         ACCEPT  �L�[���� FROM CONS
012480         PERFORM �I������
012490         EXIT PROGRAM
012500     NOT INVALID KEY
012510         COMPUTE ����N�v = ���|�J�n����N + �v�Z�N�v - 1
012520     END-READ.
012530*
012540*================================================================*
012550 ���t��Βl�v�Z SECTION.
012560*
012570** 1989�N����Ƃ��A���N�܂ŉ[�N�����񂠂邩���v�Z
012580** �����}�X�^�̐���J�n�N�m�F�B
012590*     COMPUTE ����N���v   = ����N�v - 1989 - 1.
012600*     DIVIDE 4 INTO ����N���v GIVING �[�N�񐔂v
012610*                            REMAINDER ��]�v
012620*     END-DIVIDE.
      */��N1989�N���瓖�N�܂ł̉[�N�̉񐔂��v�Z(���N�͊܂܂Ȃ�)
      */�n�߂̂��邤�N��1992�N���琔����
007500     COMPUTE ����N���v   = ����N�v - 1989 - 1.
           MOVE 1992 TO ���Z�N�v.
           MOVE ZERO TO �[�N�񐔂v.
           PERFORM UNTIL ����N�v <= ���Z�N�v
               COMPUTE ���Z�N�v   = ���Z�N�v   + 4
               COMPUTE �[�N�񐔂v = �[�N�񐔂v + 1
           END-PERFORM.
012630*
012640     COMPUTE �ʏ�N�񐔂v     = ����N���v     - �[�N�񐔂v.
012650     COMPUTE �ʏ�N��Βl�v   = �ʏ�N�񐔂v   * 365.
012660     COMPUTE �[�N��Βl�v     = �[�N�񐔂v     * 366.
012670     COMPUTE �N��Βl�v       = �ʏ�N��Βl�v + �[�N��Βl�v.
012680* ���N�����邤�N���ǂ����𒲂ׂ�i�Q���̖����v�Z�j
012690*     COMPUTE ����N���Q�v     = ����N�v + 1.
012700*     DIVIDE 4 INTO ����N���Q�v GIVING �[�N�񐔂v
012710*                                REMAINDER ��]�v
012720*     END-DIVIDE.
012730     DIVIDE 4 INTO ����N�v GIVING �[�N�񐔂v
012740                            REMAINDER ��]�v
012750     END-DIVIDE.
012760*
012770     MOVE ZERO TO ��Βl���v.
012780*
012790* �O���܂ł̐�Βl��݌v
012800     COMPUTE ��Βl���v�Q = �v�Z���v * 30.
012810*
012820     EVALUATE �v�Z���v
012830     WHEN 3
012840         COMPUTE ��Βl���v   = ��Βl���v�Q - 1
012850     WHEN 2
012860     WHEN 6
012870     WHEN 7
012880         COMPUTE ��Βl���v   = ��Βl���v�Q + 1
012890     WHEN 8
012900         COMPUTE ��Βl���v   = ��Βl���v�Q + 2
012910     WHEN 9
012920     WHEN 10
012930         COMPUTE ��Βl���v   = ��Βl���v�Q + 3
012940     WHEN 11
012950     WHEN 12
012960         COMPUTE ��Βl���v   = ��Βl���v�Q + 4
012970     WHEN OTHER
012980         MOVE ��Βl���v�Q TO ��Βl���v
012990     END-EVALUATE.
013000*
013010     IF ( ��]�v = ZERO ) AND
013020        ( �v�Z���v > 2  )
013030         COMPUTE ��Βl���v = ��Βl���v + 1
013040     END-IF.
013050*
013060     COMPUTE ��Βl�v   = �N��Βl�v + ��Βl���v + �v�Z���v.
013070*
      *================================================================*
007990 ��ƃt�@�C���쐬 SECTION.
008000*
008010     OPEN OUTPUT ��ƃt�@�C���P.
008020         MOVE NC"���" TO �t�@�C����.
008030         PERFORM �I�[�v���`�F�b�N.
008040*
008050     MOVE �{�p�a��v�q  TO ��|�{�p�a��.
008060     MOVE �{�p�N�v�q    TO ��|�{�p�N.
008070     MOVE �{�p���v�q    TO ��|�{�p��.
008080     MOVE ���Ҕԍ��v�q  TO ��|���Ҕԍ�.
008090     MOVE �}�Ԃv�q      TO ��|�}��.
008100     START ��f�ҏ��e   KEY IS >= ��|�{�p�a��N��
008110                                    ��|���҃R�[�h
008120     END-START.
008130     IF ��ԃL�[ = "00"
008140         PERFORM ��f�ҏ��e�Ǎ�
008150         PERFORM �f�[�^�`�F�b�N
008160         IF ���s�L�[�v = "YES"
                   MOVE ��|���Ҕԍ�  TO �{�L�|���Ҕԍ�
                   MOVE ��|�}��      TO �{�L�|�}��
                   MOVE ��|�{�p�a��  TO �{�L�|�{�p�a��
                   MOVE ��|�{�p�N    TO �{�L�|�{�p�N
                   MOVE ��|�{�p��    TO �{�L�|�{�p��
                   MOVE ZERO          TO �{�L�|�{�p��
                   START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
                                                �{�L�|�{�p�a��N����
                   END-START
                   IF ��ԃL�[ = "00"
                       MOVE SPACE TO �I���t���O�Q
                       PERFORM �{�p�L�^�e�Ǎ�
                       PERFORM UNTIL ( �I���t���O�Q         = "YES"          ) OR
                                     ( �{�L�|���҃R�[�h NOT = ��|���҃R�[�h ) OR
                                     ( �{�L�|�{�p�a��   NOT = ��|�{�p�a��   ) OR
                                     ( �{�L�|�{�p�N     NOT = ��|�{�p�N     ) OR
                                     ( �{�L�|�{�p��     NOT = ��|�{�p��     ) OR
                                     ( �{�L�|�{�p��         > �I�����v�q     )
      *
                           MOVE �{�L�|�{�p�� TO �{�p���v�q
008180                     PERFORM ���Z�v�g�ďo���Q
008190                     PERFORM ���ڂ��ƌv�Z
008200                     PERFORM ��ƃt�@�C���Z�b�g
008210                     PERFORM �t�@�C������
      *
                           PERFORM �{�p�L�^�e�Ǎ�
008220                 END-PERFORM
                   END-IF
008260         END-IF
008270     END-IF.
008280*
008290     CLOSE ��ƃt�@�C���P.
008300*================================================================*
008310 ��ƃt�@�C���Z�b�g SECTION.
008320*
008330     INITIALIZE ��P�|���R�[�h.
008050     MOVE �{�L�|�{�p�a��     TO ��P�|�{�p�a��.
008060     MOVE �{�L�|�{�p�N       TO ��P�|�{�p�N.
008070     MOVE �{�L�|�{�p��       TO ��P�|�{�p��.
008370     MOVE �{�L�|�{�p��       TO ��P�|�{�p��.
008380     MOVE �{�L�|���҃R�[�h   TO ��P�|���҃R�[�h.
008390     MOVE �ꕔ���S���v�q     TO ��P�|�ꕔ���S��.
           MOVE ��|��p�z         TO ��P�|��p�z.
      *
           MOVE ��|������       TO ��P�|������      .
           MOVE ��|���ԊO       TO ��P�|�������ԊO  .
           MOVE ��|�x��         TO ��P�|�����x��    .
           MOVE ��|�[��         TO ��P�|�����[��    .
           MOVE ��|�������Z��   TO ��P�|�������Z��  .
           MOVE ��|�Č���       TO ��P�|�Č���      .
           MOVE ��|���Ë���     TO ��P�|���Ë���    .
           MOVE ��|���É�     TO ��P�|���É�    .
           MOVE ��|���×�       TO ��P�|���×�      .
           MOVE ��|���É��Z��   TO ��P�|���É��Z��  .
           MOVE ��|���         TO ��P�|���Ö��    .
           MOVE ��|��H         TO ��P�|���Ó�H    .
           MOVE ��|�\���J��     TO ��P�|���Ö\���J  .
           MOVE ��|���ԊO       TO ��P�|���Î��ԊO  .
      */�^����×�/180518
           MOVE ��|�������q���Z�� TO ��P�|�������q���Z��.
           MOVE ��|�^����×�   TO ��P�|�^����×�  .
           MOVE ��|���񏈒u�����v TO ��P�|���񏈒u���v.
      */��×�
           COMPUTE ��P�|��×� = 
                   ��|��×��P   + ��|��×��Q   +
                   ��|��×��R�W + ��|��×��R�O +
                   ��|��×��S�T + ��|��×��S�W + ��|��×��S�O.
      */��㪖@
           COMPUTE ��P�|��㪖@�� =
                   ��|��㪖@���P   + ��|��㪖@���Q   +
                   ��|��㪖@���R�W + ��|��㪖@���R�O +
                   ��|��㪖@���S�T + ��|��㪖@���S�W + ��|��㪖@���S�O.
      */��㪖@
           COMPUTE ��P�|��㪖@�� =
                   ��|��㪖@���P   + ��|��㪖@���Q   +
                   ��|��㪖@���R�W + ��|��㪖@���R�O +
                   ��|��㪖@���S�T + ��|��㪖@���S�W + ��|��㪖@���S�O.
      */�d�×�
           COMPUTE ��P�|�d�×� =
                   ��|�d�×��P   + ��|�d�×��Q   +
�@                 ��|�d�×��R�W + ��|�d�×��R�O + 
�@                 ��|�d�×��S�T + ��|�d�×��S�W + ��|�d�×��S�O.
      */�������q
           MOVE ��|�� TO ��P�|��.
           MOVE ��|�� TO ��P�|��.
           MOVE ��|�� TO ��P�|��.
      *
008280     MOVE �{�L�|�{�p�a��   TO �����|�{�p�a��.
008290     MOVE �{�L�|�{�p�N     TO �����|�{�p�N.
008300     MOVE �{�L�|�{�p��     TO �����|�{�p��.
008310     MOVE �{�L�|�{�p��     TO �����|�{�p��.
           MOVE 1                TO �����|����敪.
004130     MOVE �{�L�|���҃R�[�h TO �����|���҃R�[�h.
           READ �����t�@�C��
           NOT INVALID KEY
               MOVE �����|�{�p�R�����g TO ��P�|�R�����g
           END-READ.
010250*================================================================*
013080******************************************************************
013090 END PROGRAM YCB7211.
013100******************************************************************
