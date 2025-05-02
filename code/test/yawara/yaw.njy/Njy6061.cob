000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             NJY6061.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*      ���Z�v�g����\���y�ް��쐬�z�_+����޳�ޔ�
000100* 
000110*      MED = YAW610
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2019-08-06
000140 DATE-COMPILED.          2019-08-06
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
000270     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000280                             ORGANIZATION             IS  INDEXED
000290                             ACCESS MODE              IS  DYNAMIC
000300                             RECORD KEY               IS  �s�|������
000310                                                          �s�|�s�����ԍ�
000320                             ALTERNATE RECORD KEY     IS  �s�|������
000330                                                          �s�|�s��������
000340                                                          �s�|�s�����ԍ�
000350                             FILE STATUS              IS  ��ԃL�[
000360                             LOCK        MODE         IS  AUTOMATIC.
000370     SELECT  ���ۏ��e      ASSIGN      TO        SEIHOJL
000380                             ORGANIZATION             IS INDEXED
000390                             ACCESS MODE              IS DYNAMIC
000400                             RECORD KEY               IS ���ہ|�{�p�a��N��
000410                                                         ���ہ|���҃R�[�h
000420                             ALTERNATE RECORD KEY     IS ���ہ|���҃R�[�h
000430                                                         ���ہ|�{�p�a��N��
000440                             FILE STATUS              IS ��ԃL�[
000450                             LOCK        MODE         IS AUTOMATIC.
000460     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
000470                             ORGANIZATION             IS  INDEXED
000480                             ACCESS MODE              IS  DYNAMIC
000490                             RECORD KEY           IS �{�L�|�{�p�a��N����
000500                                                     �{�L�|���҃R�[�h
000510                             ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
000520                                                     �{�L�|�{�p�a��N����
000530                             FILE STATUS              IS  ��ԃL�[
000540                             LOCK        MODE         IS  AUTOMATIC.
000550     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000560                             ORGANIZATION             IS  INDEXED
000570                             ACCESS MODE              IS  DYNAMIC
000580                             RECORD KEY               IS ���|�{�p�a��N��
000590                                                         ���|���҃R�[�h
000600                             ALTERNATE RECORD KEY     IS ���|���҃R�[�h
000610                                                         ���|�{�p�a��N��
000620                             FILE STATUS              IS  ��ԃL�[
000630                             LOCK        MODE         IS  AUTOMATIC.
000640     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000650                             ORGANIZATION             IS  INDEXED
000660                             ACCESS MODE              IS  DYNAMIC
000670                             RECORD KEY               IS  ���|����敪
000680                             FILE STATUS              IS  ��ԃL�[
000690                             LOCK        MODE         IS  AUTOMATIC.
000700     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000710                             ORGANIZATION             IS  INDEXED
000720                             ACCESS MODE              IS  DYNAMIC
000730                             RECORD KEY               IS  ��|�{�p�a��N��
000740                                                          ��|���҃R�[�h
000750                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000760                                                          ��|���҃J�i
000770                                                          ��|���҃R�[�h
000780                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000790                                                          ��|�{�p�a��N��
000800                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000810                                                          ��|�ی����
000820                                                          ��|�ی��Ҕԍ�
000830                                                          ��|���҃R�[�h
000840                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000850                                                          ��|������
000860                                                          ��|��p���S�Ҕԍ�
000870                                                          ��|���҃R�[�h
000880                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000890                                                          ��|�������
000900                                                          ��|��p���S�Ҕԍ�����
000910                                                          ��|���҃R�[�h
000920                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
000930                                                          ��|�{�p�a��N��
000940                                                          ��|���҃R�[�h
000950                             FILE STATUS              IS  ��ԃL�[
000960                             LOCK        MODE         IS  AUTOMATIC.
000970     SELECT  ���Z�v�g�e      ASSIGN      TO        RECEPTL
000980                             ORGANIZATION             IS  INDEXED
000990                             ACCESS MODE              IS  DYNAMIC
001000                             RECORD KEY               IS  ���Z�|�{�p�a��N��
001010                                                          ���Z�|���҃R�[�h
001020                                                          ���Z�|���Z���
001030                             ALTERNATE RECORD KEY     IS  ���Z�|���҃R�[�h
001040                                                          ���Z�|�{�p�a��N��
001050                                                          ���Z�|���Z���
001060                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
001070                                                          ���Z�|�{�p�a��N��
001080                                                          ���Z�|���҃R�[�h
001090                                                          ���Z�|���Z���
001100                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
001110                                                          ���Z�|���Z���
001120                                                          ���Z�|�����ی��Ҕԍ�
001130                                                          ���Z�|���҃R�[�h
001140                                                          ���Z�|�{�p�a��N��
001150                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
001160                                                          ���Z�|�����ی��Ҕԍ�
001170                                                          ���Z�|���҃R�[�h
001180                                                          ���Z�|���Z���
001190                                                          ���Z�|�{�p�a��N��
001200                             FILE STATUS              IS  ��ԃL�[
001210                             LOCK        MODE         IS  AUTOMATIC.
001220     SELECT  ��ƃt�@�C���P  ASSIGN      TO    "C:\MAKISHISYS\YAWOBJ\TEMP\W60610L.DAT"
001230                             ORGANIZATION             IS  INDEXED
001240                             ACCESS                   IS  DYNAMIC
001250                             RECORD      KEY          IS  ��P�|��R�[�h
000908                                                          ��P�|����敪�P
000908                                                          ��P�|�ی��Ҕԍ���
000908                                                          ��P�|����敪�Q
001280                                                          ��P�|���҃J�i
001290                                                          ��P�|���҃R�[�h
001300                                                          ��P�|�{�p�a��N��
001310                             FILE        STATUS       IS  ��ԃL�[
001320                             LOCK        MODE         IS  AUTOMATIC.
001330* ���Z��ʕ��я��p
001340     SELECT  ��ƃt�@�C���Q  ASSIGN      TO   "C:\MAKISHISYS\YAWOBJ\TEMP\W6102L.DAT"
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS                   IS  DYNAMIC
001370                             RECORD      KEY          IS  ��Q�|�ی��敪
001380                                                          ��Q�|����
001390                             ALTERNATE RECORD KEY     IS  ��Q�|�{�p�a��N��
001400                                                          ��Q�|���҃R�[�h
001410                                                          ��Q�|�ی��敪
001420                                                          ��Q�|����
001430                             FILE        STATUS       IS  ��ԃL�[
001440                             LOCK        MODE         IS  AUTOMATIC.
001450* ���Z���я��p
001460     SELECT  ��ƃt�@�C���R  ASSIGN      TO  "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001470                             ORGANIZATION             IS  INDEXED
001480                             ACCESS                   IS  DYNAMIC
001490                             RECORD      KEY          IS  ��R�|�{�p�a��N��
001500                                                          ��R�|���҃R�[�h
001510                                                          ��R�|�ی����
001520                             FILE        STATUS       IS  ��ԃL�[
001530                             LOCK        MODE         IS  AUTOMATIC.
001540******************************************************************
001550*                      DATA DIVISION                             *
001560******************************************************************
001570 DATA                    DIVISION.
001580 FILE                    SECTION.
001590*                           �m�q�k��  �Q�T�U�n
001600 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001610     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
001620* 
001630 FD  ���ۏ��e          BLOCK   CONTAINS   1   RECORDS.
001640     COPY SEIHOJ          OF  XFDLIB  JOINING   ����   AS  PREFIX.
001650*                           �m�q�k��  �Q�T�U�n
001660 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
001670     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
001680*                           �m�q�k��  �P�Q�W�n
001690 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001700     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001710*                           �m�q�k��  �Q�T�U�n
001720 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001730     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001740*
001750 FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
001760     COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001770*                           �m�q�k��  �R�Q�O�n
001780 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001790     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001800* �ی��Ҕԍ����t�@�C��
001810 FD  ��ƃt�@�C���P RECORD  CONTAINS 256 CHARACTERS.
001820 01  ��P�|���R�[�h.
001830     03  ��P�|���R�[�h�L�[.
               05  ��P�|��R�[�h                PIC 9(2).
               05  ��P�|����敪�P.
                   07  ��P�|����敪�P�P        PIC 9(1).
                   07  ��P�|����敪�P�Q        PIC 9(2).
001840         05  ��P�|�ی��Ҕԍ���            PIC 9(8).
               05  ��P�|����敪�Q.
                   07  ��P�|����敪�Q�P        PIC 9(1).
                   07  ��P�|����敪�Q�Q        PIC 9(1).
                   07  ��P�|����敪�Q�R        PIC 9(1).
001870         05  ��P�|���҃J�i                PIC X(50).
001880         05  ��P�|���҃R�[�h.
001890             07 ��P�|���Ҕԍ�             PIC 9(6).
001900             07 ��P�|�}��                 PIC X(1).
001910         05  ��P�|�{�p�a��N��.
001920             07  ��P�|�{�p�a��            PIC 9.
001930             07  ��P�|�{�p�N              PIC 9(2).
001940             07  ��P�|�{�p��              PIC 9(2).
001950     03  ��P�|���R�[�h�f�[�^.
001960         05  ��P�|���Z���                PIC 9(2).
001840         05  ��P�|�ی��Ҕԍ�              PIC 9(8).
001970         05  ��P�|�ی��Ҕԍ��e            PIC 9(8).
002190         05  ��P�|�ی����                PIC 9(2).
001860         05  ��P�|�{�l�Ƒ��敪            PIC 9.
001980         05  ��P�|���Ҏ���                PIC X(50).
001990         05  ��P�|��ی��Ҏ���            PIC X(50).
002000         05  ��P�|��p�z                  PIC 9(6).
002010         05  ��P�|���S�z                  PIC 9(6).
002020         05  ��P�|�����z                  PIC 9(6).
002030         05  ��P�|���Z����敪            PIC 9.
002040         05  ��P�|�������                PIC 9(2).
002050         05  FILLER                        PIC X(36).
002060*
002070 FD  ��ƃt�@�C���Q RECORD  CONTAINS 155 CHARACTERS.
002080 01  ��Q�|���R�[�h.
002090     03  ��Q�|���R�[�h�L�[.
002100         05  ��Q�|�ی��敪                PIC 9(1).
002110         05  ��Q�|����                    PIC 9(4).
002120     03  ��Q�|���R�[�h�f�[�^.
002130         05  ��Q�|���Z���                PIC 9(2).
002140         05  ��Q�|���҃R�[�h.
002150             07 ��Q�|���Ҕԍ�             PIC 9(6).
002160             07 ��Q�|�}��                 PIC X(1).
002170         05  ��Q�|���Ҏ���                PIC X(50).
002180         05  ��Q�|��ی��Ҏ���            PIC X(50).
002190         05  ��Q�|�ی����                PIC 9(2).
002200         05  ��Q�|�����ی��Ҕԍ�          PIC X(10).
002210         05  ��Q�|�{�l�Ƒ�                PIC X(4).
002220         05  ��Q�|�{�p�a��N��.
002230             07  ��Q�|�{�p�a��            PIC 9.
002240             07  ��Q�|�{�p�N              PIC 9(2).
002250             07  ��Q�|�{�p��              PIC 9(2).
002260         05  ��Q�|����w��                PIC 9(1).
002270         05  ��Q�|��p�z                  PIC 9(6).
002280         05  ��Q�|���S�z                  PIC 9(6).
002290         05  ��Q�|�����z                  PIC 9(6).
002300         05  ��Q�|���Z����敪            PIC 9.
002310*
002320 FD  ��ƃt�@�C���R RECORD  CONTAINS 32 CHARACTERS.
002330 01  ��R�|���R�[�h.
002340     03  ��R�|���R�[�h�L�[.
002350         05  ��R�|�{�p�a��N��.
002360             07  ��R�|�{�p�a��            PIC 9.
002370             07  ��R�|�{�p�N              PIC 9(2).
002380             07  ��R�|�{�p��              PIC 9(2).
002390         05  ��R�|���҃R�[�h.
002400             07 ��R�|���Ҕԍ�             PIC 9(6).
002410             07 ��R�|�}��                 PIC X(1).
002420         05  ��R�|�ی����                PIC 9(2).
002430     03  ��R�|���R�[�h�f�[�^.
002440         05  ��R�|����                    PIC 9(4).
002450         05  FILLER                        PIC X(14).
002460*
002470*----------------------------------------------------------------*
002480******************************************************************
002490*                WORKING-STORAGE SECTION                         *
002500******************************************************************
002510 WORKING-STORAGE         SECTION.
002520 01 �L�[����                           PIC X    VALUE SPACE.
002530 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
002540 01 �I���t���O                         PIC X(3) VALUE SPACE.
002550 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
002560 01 ���s�L�[�v                         PIC X(3)  VALUE SPACE.
002570 01 �{�p�L�^�L�v                       PIC X(3) VALUE SPACE.
002580 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
002590**
       01 ���я��v.
002600    03 ��������v                      PIC 9(2) VALUE ZERO.
002610    03 ����敪�P�P�v                  PIC 9    VALUE ZERO.
002610    03 ����敪�P�Q�v                  PIC 9(2) VALUE ZERO.
002610    03 ����敪�Q�P�v                  PIC 9    VALUE ZERO.
002610    03 ����敪�Q�Q�v                  PIC 9    VALUE ZERO.
002610    03 ����敪�Q�R�v                  PIC 9    VALUE ZERO.
002620 01 �ی���ʂv�q                       PIC 9(2) VALUE ZERO.
002630 01 ����`���v�q                       PIC 9    VALUE ZERO.
002640 01 �ی��Ҕԍ��v�q                     PIC X(10) VALUE SPACE.
002650 01 ���Z�v�g��ނv�q                   PIC X(4) VALUE SPACE.
002660 01 �{�l�Ƒ��敪�v�q                   PIC 9    VALUE ZERO.
002670 01 �{�p�a��N���v�q.
002680    03 �{�p�a��v�q                    PIC 9    VALUE ZERO.
002690    03 �{�p�N�v�q                      PIC 9(2) VALUE ZERO.
002700    03 �{�p���v�q                      PIC 9(2) VALUE ZERO.
002710 01 �����a��N���v�q.
002720    03 �����a��v�q                    PIC 9    VALUE ZERO.
002730    03 �����N�v�q                      PIC 9(2) VALUE ZERO.
002740    03 �������v�q                      PIC 9(2) VALUE ZERO.
002750**
002760 01 �t�@�C����                         PIC N(8) VALUE SPACE.
002770 01 �ی���ʂv                         PIC 9(2) VALUE ZERO.
002780 01 ���Ԃv                             PIC 9(4) VALUE ZERO.
002790 01 ���ۏ��Ԃv                         PIC 9(4)  VALUE ZERO.
002800 01 ���Z���я����~�v                   PIC 9(1) VALUE ZERO.
002810*
002820*
002830** �������Z�܂Ƃߗp
002840 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
002850*
002860** ���v�W�v�p
002870 01 ���v�v.
002880    03 �������v�v                      PIC 9(4) VALUE ZERO.
002890    03 ��p�z���v�v                    PIC 9(8) VALUE ZERO.
002900    03 �����z���v�v                    PIC 9(8) VALUE ZERO.
002910** �G���[���b�Z�[�W�p
002920 01 �G���[���b�Z�[�W�v.
002930    03 �G���[���҃R�[�h�v              PIC X(7) VALUE SPACE.
002940    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
002950    03 �G���[�ی���ʂv                PIC X(2) VALUE SPACE.
002960    03 FILLER                          PIC X(10) VALUE SPACE.
002970*
002980** �ی��Ҕԍ��E�l�ߗp
002990 01 �ی��Ҕԍ��v�s.
003000    03 �ی��Ҕԍ����l�߂v.
003010      05 �ی��Ҕԍ����l�߂v�P          PIC X OCCURS 8 VALUE SPACE.
003020    03 �ی��Ҕԍ��E�l�߂v.
003030      05 �ی��Ҕԍ��E�l�߂v�P          PIC X OCCURS 8 VALUE ZERO.
003040    03 �ی��Ҕԍ������v                PIC 9(8)  VALUE ZERO.
003050    03 �ی��Ҕԍ��v                    PIC X(8)  VALUE SPACE.
003060 01 ���z�v.
003070    03 ��p�z�v                        PIC 9(6)  VALUE ZERO.
003080    03 ���S�z�v                        PIC 9(6)  VALUE ZERO.
003090    03 �����z�v                        PIC 9(6)  VALUE ZERO.
003250*
003260******************************************************************
003270*                          �A������                              *
003280******************************************************************
003290*
003300********************
003310* ���b�Z�[�W�\���L�[ *
003320********************
003330 01 �A���|�L�[ IS EXTERNAL.
003340    03  �A���|���b�Z�[�W               PIC N(20).
003350*
003360 01 �A���R�|�L�[ IS EXTERNAL.
003370    03  �A���R�|���b�Z�[�W             PIC N(20).
003380    03  �A���R�|���b�Z�[�W�P           PIC X(20).
003390***
003400 01 �A���|���̓f�[�^�U�P�O IS EXTERNAL.
003410    03 �A���|�����N��.
003420       05 �A���|�����a��                  PIC 9.
003430       05 �A���|�����N                    PIC 9(2).
003440       05 �A���|������                    PIC 9(2).
003450    03 �A���|�ی����                     PIC 9(2).
003460*
003470************************
003480* �������Z�܂Ƃ�
003490************************
003500 01 �A���Z�܂Ƃ߁|�L�[ IS EXTERNAL.
003510    03 �A���Z�܂Ƃ߁|�{�p�a��N��.
003520       05 �A���Z�܂Ƃ߁|�{�p�a��               PIC 9.
003530       05 �A���Z�܂Ƃ߁|�{�p�N��.
003540          07 �A���Z�܂Ƃ߁|�{�p�N              PIC 9(2).
003550          07 �A���Z�܂Ƃ߁|�{�p��              PIC 9(2).
003560    03 �A���Z�܂Ƃ߁|���҃R�[�h.
003570       05 �A���Z�܂Ƃ߁|���Ҕԍ�               PIC 9(6).
003580       05 �A���Z�܂Ƃ߁|�}��                   PIC X(1).
003590**-------------------------------------------------------**
003600*   1:�������Z�v�g�Ȃ��̖{�̂܂Ƃ߂̔���
003610*   2:���l�E���p�̎Еۏ������Z���̔���
003620    03 �A���Z�܂Ƃ߁|����敪                  PIC 9.
003630**-------------------------------------------------------**
003640*  / OUT /�@ 0:�ΏۊO�A1:�Ώ�
003650    03 �A���Z�܂Ƃ߁|���茋��                  PIC 9.
003660**
003670******************************************************************
003680*                      PROCEDURE  DIVISION                       *
003690******************************************************************
003700 PROCEDURE               DIVISION.
003710************
003720*           *
003730* ��������   *
003740*           *
003750************
003760     PERFORM ������.
003770************
003780*           *
003790* �又��     *
003800*           *
003810************
003820     PERFORM ��ƃt�@�C���쐬.
003830     PERFORM ���я��t�@�C���쐬.
003840************
003850*           *
003860* �I������   *
003870*           *
003880************
003890     PERFORM �I������.
003900     MOVE ZERO TO PROGRAM-STATUS.
003910     EXIT PROGRAM.
003920*
003930*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
003940*================================================================*
003950 ������ SECTION.
003960*
003970     PERFORM �t�@�C���I�[�v��.
003980* �A�����ڂ̑Ҕ�
003990     MOVE �A���|�����a��      TO �����a��v�q.
004000     MOVE �A���|�����N        TO �����N�v�q.
004010     MOVE �A���|������        TO �������v�q.
004020     MOVE �A���|�ی����      TO �ی���ʂv�q.
004030* ���Z���я������E�~���Z�b�g
004040     MOVE ZEROS TO ���|����敪.
004050     READ ������}�X�^
004060     NOT INVALID KEY
004070         MOVE ���|���Z���я����~ TO ���Z���я����~�v
004080     END-READ.
004090*
004100*================================================================*
004110 �t�@�C���I�[�v�� SECTION.
004120*
004130     OPEN INPUT �{�p�L�^�e.
004140         MOVE NC"�{�p�L�^�e" TO �t�@�C����.
004150         PERFORM �I�[�v���`�F�b�N.
004160     OPEN INPUT �����f�[�^�e.
004170         MOVE NC"�����f�[�^�e" TO �t�@�C����.
004180         PERFORM �I�[�v���`�F�b�N.
004190     OPEN INPUT ������}�X�^
004200         MOVE NC"������" TO �t�@�C����.
004210         PERFORM �I�[�v���`�F�b�N.
004220     OPEN INPUT ��f�ҏ��e.
004230         MOVE NC"��f�ҏ��e" TO �t�@�C����.
004240         PERFORM �I�[�v���`�F�b�N.
004250     OPEN INPUT �s�����}�X�^.
004260         MOVE NC"�s����" TO �t�@�C����.
004270         PERFORM �I�[�v���`�F�b�N.
004280     OPEN INPUT ���ۏ��e.
004290         MOVE NC"����" TO �t�@�C����.
004300         PERFORM �I�[�v���`�F�b�N.
004310     OPEN INPUT ���Z�v�g�e.
004320         MOVE NC"���Z�v�g�e" TO �t�@�C����.
004330         PERFORM �I�[�v���`�F�b�N.
004340*================================================================*
004350 �I�[�v���`�F�b�N SECTION.
004360*
004370     IF ��ԃL�[  NOT =  "00"
004380         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
004390         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
004400         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004410                                                 UPON CONS
004420*-----------------------------------------*
004430         CALL "actcshm"  WITH C LINKAGE
004440*-----------------------------------------*
004450         ACCEPT  �L�[���� FROM CONS
004460         PERFORM �t�@�C����
004470         MOVE 99 TO PROGRAM-STATUS
004480         EXIT PROGRAM.
004490*================================================================*
004500 �t�@�C���� SECTION.
004510*
004520     CLOSE �{�p�L�^�e �����f�[�^�e ������}�X�^
004530           ���ۏ��e ��f�ҏ��e �s�����}�X�^   ���Z�v�g�e.
004540*================================================================*
004550 �I������ SECTION.
004560*
004570     PERFORM �t�@�C����.
004580*================================================================*
004590 �G���[�\�� SECTION.
004600*
004610     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
004620     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
004630     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
004640     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
004650*-----------------------------------------*
004660     CALL "actcshm"  WITH C LINKAGE.
004670*-----------------------------------------*
004680     ACCEPT  �L�[���� FROM CONS.
004690     PERFORM �t�@�C����.
004700     MOVE 99 TO PROGRAM-STATUS.
004710     EXIT PROGRAM.
004720*================================================================*
004730 �G���[�\���q SECTION.
004740*
004750     DISPLAY NC"�t�@�C���Ǎ��G���[" �t�@�C����     UPON CONS.
004760     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
004770     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
004780*-----------------------------------------*
004790     CALL "actcshm"  WITH C LINKAGE.
004800*-----------------------------------------*
004810     ACCEPT  �L�[���� FROM CONS.
004820     PERFORM �t�@�C����.
004830     MOVE 99 TO PROGRAM-STATUS.
004840     EXIT PROGRAM.
004850*================================================================*
004860 ��ƃt�@�C���쐬 SECTION.
004870*
004880     PERFORM �ی��Ҕԍ����t�@�C���쐬.
004890*
004900* ���Z���я��t�@�C���쐬
004910     OPEN OUTPUT ��ƃt�@�C���Q.
004920         MOVE NC"��Q" TO �t�@�C����.
004930         PERFORM �I�[�v���`�F�b�N.
004940*
004950     IF ���Z���я����~�v = 1
004960         PERFORM ���Z���я��~���t�@�C���쐬
004970     ELSE
004980         PERFORM ���Z���я��t�@�C���쐬
004990     END-IF.
005000*
005010     CLOSE ��ƃt�@�C���Q.
005020*
005030*================================================================*
005040 �ی��Ҕԍ����t�@�C���쐬 SECTION.
005050**********************************************************************
005060**   ��f�ҏ��e����A�Y�������N���̃f�[�^�𒊏o���A
005070**   ��ƃt�@�C���P(�ی��Ҕԍ���)�ɏ����o��.
005080**********************************************************************
005090*
005100     OPEN OUTPUT ��ƃt�@�C���P.
005110         MOVE NC"��P" TO �t�@�C����.
005120         PERFORM �I�[�v���`�F�b�N.
005130*
005140     MOVE "YES"         TO ���s�L�[�v.
005150     MOVE �����a��v�q  TO ���Z�|�����a��.
005160     MOVE �����N�v�q    TO ���Z�|�����N.  
005170     MOVE �������v�q    TO ���Z�|������.  
005180     MOVE ZERO          TO ���Z�|���Z���.
005190     MOVE ZERO          TO ���Z�|�{�p�a��.
005200     MOVE ZERO          TO ���Z�|�{�p�N.  
005210     MOVE ZERO          TO ���Z�|�{�p��.  
005220     MOVE ZERO          TO ���Z�|���Ҕԍ�.
005230     MOVE SPACE         TO ���Z�|�}��.    
005240     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
005250                                  ���Z�|�{�p�a��N��
005260                                  ���Z�|���҃R�[�h
005270                                  ���Z�|���Z���
005280     END-START.
005290*     PERFORM �f�[�^�m�F.
005300     IF ��ԃL�[ = "00"
005310         MOVE SPACE  TO �I���t���O
005320         PERFORM ���Z�v�g�e�Ǎ�
005330         PERFORM UNTIL ( �I���t���O = "YES" ) OR
005340                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
005350                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
005360                       ( ���Z�|������   NOT = �������v�q   )
005370*
005380            PERFORM �f�[�^�`�F�b�N
005390** �J�ЁE�����ӁE���R�͑ΏۊO
005400            IF  ���Z�|���Z��� = 4 OR 5 OR 6 OR 8
005410               MOVE SPACE  TO ���s�L�[�v
005420            END-IF
005430            IF ���s�L�[�v = "YES"
005440*
005450               EVALUATE TRUE
005460*            ********
005470*            * ���� *
005480*            ********
005490                WHEN ���Z�|���Z���   = 1
005500                    IF (�ی���ʂv�q = ZERO) OR
005510                       ((��|�ی���� = �ی���ʂv�q) AND ( ��|������ = ZERO ))
005520*                **********************
005530*                * ��ƃt�@�C���쐬 *
005540*                **********************
005550                       MOVE SPACE TO ��P�|���R�[�h
005560                       INITIALIZE ��P�|���R�[�h
005570                       MOVE ��|�ی����       TO ��P�|�ی����
005580                       MOVE ��|�ی��Ҕԍ�     TO �ی��Ҕԍ��v
005596                       PERFORM �ی��Ҕԍ��E�l��
005600                       MOVE �ی��Ҕԍ������v   TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ���
005610                       PERFORM �����Z�b�g
005620                       PERFORM ��P���R�[�h�Z�b�g
005630                       PERFORM ��P�t�@�C������
005640*
005650                    END-IF
005660*            ********
005670*            * �V�l *
005680*            ********
005690                WHEN ���Z�|���Z���   = 2 
005700                    IF (�ی���ʂv�q = ZERO) OR
005710                       (��|������ = �ی���ʂv�q)
005720*                    **********************
005730*                    * ��ƃt�@�C���쐬 *
005740*                    **********************
005750                       MOVE SPACE TO ��P�|���R�[�h
005760                       INITIALIZE ��P�|���R�[�h
005770                       MOVE ��|������       TO ��P�|�ی����
005780                       MOVE ��|��p���S�Ҕԍ�  TO �ی��Ҕԍ��v
005790                       PERFORM �ی��Ҕԍ��E�l��
005800                       MOVE �ی��Ҕԍ������v   TO ��P�|�ی��Ҕԍ� ��P�|�ی��Ҕԍ���
005810                       PERFORM �����Z�b�g
005820                       PERFORM ��P���R�[�h�Z�b�g
005830                       PERFORM ��P�t�@�C������
005840                    END-IF
005850*            ********
005860*            * ���� *
005870*            ********
005880*��* ���ʏ����i�������Z�܂Ƃ߁j**
005890                WHEN ���Z�|���Z���   = 3
005900                    IF (�ی���ʂv�q = ZERO) OR
005910                       (��|������� = �ی���ʂv�q)
005920                       IF ���Z�|�{�̂܂Ƃߋ敪 NOT = 1
005930                          MOVE SPACE TO ��P�|���R�[�h
005940                          INITIALIZE ��P�|���R�[�h
005950                          MOVE ��|�������       TO ��P�|�ی����
005960                          MOVE ��|�ی��Ҕԍ�     TO �ی��Ҕԍ��v
005970                          PERFORM �ی��Ҕԍ��E�l��
005980                          MOVE �ی��Ҕԍ������v   TO ��P�|�ی��Ҕԍ��e
023280                          MOVE ��|�������       TO �s�|������
023290                          MOVE ��|��p���S�Ҕԍ����� TO �s�|�s�����ԍ�  ��P�|�ی��Ҕԍ�
009792                          IF (��|������� = 54) OR
                                   ((��|������� = 60) AND (��|��p���S�Ҕԍ�����(1:2) = 51))
023290                             MOVE ��|��p���S�Ҕԍ�����   TO ��P�|�ی��Ҕԍ���
                                ELSE
023300                             READ �s�����}�X�^
023310                             INVALID KEY
023320                                 MOVE ZERO                 TO  ��P�|�ی��Ҕԍ���
023330                             NOT INVALID KEY
005990                                 MOVE �s�|�ی��Ҕԍ�(1:8)  TO  ��P�|�ی��Ҕԍ���
023380                             END-READ
                                END-IF
006000                          PERFORM ���������Z�b�g
006010                          PERFORM ��P���R�[�h�Z�b�g
006020                          PERFORM ��P�t�@�C������
006030                       END-IF
006040                    END-IF
006050*                ********
006060*                * ���� *
006070*                ********
006080                WHEN ���Z�|���Z���   = 7
006090                    IF (�ی���ʂv�q = ZERO) OR
006100                       (��|�ی���� = �ی���ʂv�q)
006110                        MOVE SPACE TO ��P�|���R�[�h
006120                        INITIALIZE ��P�|���R�[�h
006130                        MOVE ��|�ی����     TO ��P�|�ی����
006140                        MOVE ��|�{�p�a��N�� TO ���ہ|�{�p�a��N��
006150                        MOVE ��|���҃R�[�h   TO ���ہ|���҃R�[�h
006160                        READ ���ۏ��e
006170                        NOT INVALID KEY
006180                           MOVE ���ہ|���S�Ҕԍ�   TO �ی��Ҕԍ��v
006190                        END-READ
006200                        PERFORM �ی��Ҕԍ��E�l��
006210                        MOVE �ی��Ҕԍ������v   TO ��P�|�ی��Ҕԍ�  ��P�|�ی��Ҕԍ���
006220                        PERFORM �����Z�b�g
006230                        PERFORM ��P���R�[�h�Z�b�g
006240                        PERFORM ��P�t�@�C������
006250                    END-IF
006260                END-EVALUATE
006270             END-IF
006280*
006290             PERFORM ���Z�v�g�e�Ǎ�
006300         END-PERFORM
006310     END-IF.
006320*
006330     CLOSE ��ƃt�@�C���P.
006340*
006350*================================================================*
006360*================================================================*
006370 �f�[�^�m�F SECTION.
006380*
006390     IF ��ԃL�[  NOT =  "00"
006400         MOVE  NC"�@�@�@�Y���N���̃f�[�^������܂���B" TO �A���|���b�Z�[�W
006410         CALL   "MSG001"
006420         CANCEL "MSG001"
006430         PERFORM �t�@�C����
006440         MOVE 99 TO PROGRAM-STATUS
006450         EXIT PROGRAM
006460     END-IF.
006470*
006480*================================================================*
006490 �f�[�^�`�F�b�N SECTION.
006500*
006510     MOVE SPACE  TO ���s�L�[�v.
006520     IF ( ���Z�|���Z����Ώۋ敪 NOT = 1 )
006530        MOVE "YES"  TO ���s�L�[�v
006540     END-IF.
006550* *****************************************************************
006560* * ���Z�v�g�e�̐����Ώۋ敪 = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
006570* *****************************************************************
006580     IF ���s�L�[�v  = "YES"
006590*      (�ēx�A���s�L�[�v SPACE)
006600        MOVE SPACE  TO ���s�L�[�v
006610        IF ���Z�|�����Ώۋ敪 NOT = ZERO
006620           MOVE ���Z�|�{�p�a��  TO ��|�{�p�a��
006630           MOVE ���Z�|�{�p�N    TO ��|�{�p�N
006640           MOVE ���Z�|�{�p��    TO ��|�{�p��
006650           MOVE ���Z�|���Ҕԍ�  TO ��|���Ҕԍ�
006660           MOVE ���Z�|�}��      TO ��|�}��
006670           READ ��f�ҏ��e
006680           NOT INVALID KEY
006690              MOVE "YES"  TO ���s�L�[�v
006700           END-READ
006710        ELSE
006720           MOVE SPACE  TO ���s�L�[�v
006730        END-IF
006740     END-IF.
006750*
006760*================================================================*
006770 ��P���R�[�h�Z�b�g SECTION.
006780*
006790     MOVE ���Z�|�{�p�a��    TO ��P�|�{�p�a��.
006800     MOVE ���Z�|�{�p�N      TO ��P�|�{�p�N.
006810     MOVE ���Z�|�{�p��      TO ��P�|�{�p��.
006820     PERFORM ��������Z�b�g.
006830     MOVE ��������v        TO ��P�|��R�[�h.
006830     MOVE ����敪�P�P�v    TO ��P�|����敪�P�P.
006830     MOVE ����敪�P�Q�v    TO ��P�|����敪�P�Q.
006830     MOVE ����敪�Q�P�v    TO ��P�|����敪�Q�P.
006830     MOVE ����敪�Q�Q�v    TO ��P�|����敪�Q�Q.
006830     MOVE ����敪�Q�R�v    TO ��P�|����敪�Q�R.
006840     MOVE ��|�{�l�Ƒ��敪  TO ��P�|�{�l�Ƒ��敪.
006850     MOVE ��|���҃J�i      TO ��P�|���҃J�i.
006860     MOVE ��|���҃R�[�h    TO ��P�|���҃R�[�h.
006870     MOVE ��|���Ҏ���      TO ��P�|���Ҏ���.
006880     MOVE ��|��ی��Ҏ���  TO ��P�|��ی��Ҏ���.
006890     MOVE ���Z�|���Z���    TO ��P�|���Z���.
006900     MOVE ��p�z�v          TO ��P�|��p�z.
006910     MOVE �����z�v          TO ��P�|�����z.
006920     MOVE ���S�z�v          TO ��P�|���S�z.
006930*
006940*================================================================*
006950 �����Z�b�g SECTION.
006960     MOVE ���Z�|���v        TO ��p�z�v.
006970     MOVE ���Z�|�������z    TO �����z�v.
006980     MOVE ���Z�|�ꕔ���S��  TO ���S�z�v.
006990     MOVE ��|���Z����敪  TO ��P�|���Z����敪.
007000*
007010*================================================================*
007020 ���������Z�b�g SECTION.
007030     MOVE ���Z�|���v           TO ��p�z�v.
007040     MOVE ���Z�|�����������z   TO �����z�v.
007050     MOVE ���Z�|�󋋎ҕ��S�z   TO ���S�z�v.
007060     MOVE ��|���Z����敪���� TO ��P�|���Z����敪.
007070*
007080*================================================================*
007090 ��P�t�@�C������ SECTION.
007100*
007110     WRITE ��P�|���R�[�h
007120     INVALID KEY
007130         MOVE NC"��P"  TO �t�@�C����
007140         PERFORM �G���[�\��
007150     END-WRITE.
007160*
007170*================================================================*
007180 �ی��Ҕԍ��E�l�� SECTION.
007190*
007200     MOVE �ی��Ҕԍ��v    TO  �ی��Ҕԍ����l�߂v.
007210     MOVE ZERO            TO  �ی��Ҕԍ��E�l�߂v.
007220     MOVE ZERO            TO  �ی��Ҕԍ������v.
007230*
007240     MOVE  9  TO  �J�E���^.
007250*
007260     IF  �ی��Ҕԍ����l�߂v�P(8) NOT = SPACE
007270         COMPUTE �J�E���^ = �J�E���^  -  1
007280         MOVE �ی��Ҕԍ����l�߂v�P(8)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
007290     END-IF.
007300     IF  �ی��Ҕԍ����l�߂v�P(7) NOT = SPACE
007310         COMPUTE �J�E���^ = �J�E���^  -  1
007320         MOVE �ی��Ҕԍ����l�߂v�P(7)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
007330     END-IF.
007340     IF  �ی��Ҕԍ����l�߂v�P(6) NOT = SPACE
007350         COMPUTE �J�E���^ = �J�E���^  -  1
007360         MOVE �ی��Ҕԍ����l�߂v�P(6)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
007370     END-IF.
007380     IF  �ی��Ҕԍ����l�߂v�P(5) NOT = SPACE
007390         COMPUTE �J�E���^ = �J�E���^  -  1
007400         MOVE �ی��Ҕԍ����l�߂v�P(5)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
007410     END-IF.
007420     IF  �ی��Ҕԍ����l�߂v�P(4) NOT = SPACE
007430         COMPUTE �J�E���^ = �J�E���^  -  1
007440         MOVE �ی��Ҕԍ����l�߂v�P(4)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
007450     END-IF.
007460     IF  �ی��Ҕԍ����l�߂v�P(3) NOT = SPACE
007470         COMPUTE �J�E���^ = �J�E���^  -  1
007480         MOVE �ی��Ҕԍ����l�߂v�P(3)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
007490     END-IF.
007500     IF  �ی��Ҕԍ����l�߂v�P(2) NOT = SPACE
007510         COMPUTE �J�E���^ = �J�E���^  -  1
007520         MOVE �ی��Ҕԍ����l�߂v�P(2)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
007530     END-IF.
007540     IF  �ی��Ҕԍ����l�߂v�P(1) NOT = SPACE
007550         COMPUTE �J�E���^ = �J�E���^  -  1
007560         MOVE �ی��Ҕԍ����l�߂v�P(1)  TO  �ی��Ҕԍ��E�l�߂v�P(�J�E���^)
007570     END-IF.
007580*
007590     MOVE �ی��Ҕԍ��E�l�߂v TO �ی��Ҕԍ������v.
007600*
007610*================================================================*
007620 ��ƃt�@�C���P�Ǎ� SECTION.
007630*
007640     READ ��ƃt�@�C���P NEXT
007650     AT END
007660         MOVE "YES" TO �I���t���O
007670     NOT AT END
007680         MOVE ��P�|�{�p�a��    TO ��|�{�p�a��
007690         MOVE ��P�|�{�p�N      TO ��|�{�p�N
007700         MOVE ��P�|�{�p��      TO ��|�{�p��
007710         MOVE ��P�|���Ҕԍ�    TO ��|���Ҕԍ�
007720         MOVE ��P�|�}��        TO ��|�}��
007730         READ ��f�ҏ��e
007740         INVALID KEY
007750              MOVE NC"��f��"   TO �t�@�C����
007760              PERFORM �G���[�\���q
007770         END-READ
007780     END-READ.
007790*
007800*================================================================*
007810 ���Z�v�g�e�Ǎ� SECTION.
007820*
007830     READ ���Z�v�g�e NEXT
007840     AT END
007850         MOVE "YES" TO �I���t���O
007860     END-READ.
007870*================================================================*
007880 ��������Z�b�g SECTION.
007890*
007900**///  ��������̕ύX�͂���SECTION��   ////**
007920*
007930***************************************************
007940* �ی���ʏ���
007990*     1:�Е�     �{�Ƌ敪 ���ʋ敪                      *
007960*     2:����     �{�Ƌ敪 ���ʋ敪                      *
008040*     3:�D��     �{�Ƌ敪 ���ʋ敪                      *
008010*     4:�g��     �ʏ�E�ސE �� / �{�Ƌ敪 ���ʋ敪      *
008030*     5:���q��   �{�Ƌ敪 ���ʋ敪                      *
008030*     6:����     �{�Ƌ敪 ���ʋ敪                      *
007950*     7:����     ���E�� ���ʋ敪 �{�Ƌ敪               *
007960*     8:������� �{�Ƌ敪 ����                          *
008060*                                                       *
008090*     9:�����E���� �{�Ƌ敪                             *
008070*    10:����       ��Q�E���c���E��q�E���̑� �{�Ƌ敪  *
008100*
008110***************************************************
008120*
            MOVE ZERO                    TO ���я��v.
008130      EVALUATE ���Z�|���Z���
008140      WHEN 1
008150         EVALUATE  ��|�ی����
008160* ����
008170         WHEN  01
009793            MOVE 7                 TO ��������v
009793            MOVE 1                 TO ����敪�Q�P�v
                  MOVE ��|�{�l�Ƒ��敪  TO ����敪�Q�R�v
008230* �Е�
008260         WHEN  02
009793            MOVE 1                 TO ��������v
                  MOVE ��|�{�l�Ƒ��敪  TO ����敪�Q�P�v
008340* �g��
008350         WHEN  03
009793            MOVE 4                 TO ��������v
                  MOVE ��|�{�l�Ƒ��敪  TO ����敪�Q�P�v
                  IF ��|�ی��Ҕԍ�(1:2) = "06"
009793                MOVE 1             TO ����敪�P�P�v
                  ELSE
009793                MOVE 2             TO ����敪�P�P�v
                  END-IF
                  IF ��|�ی��Ҕԍ�(1:4) = "0663"
                      MOVE "13"                TO ����敪�P�Q�v
                  ELSE
                      MOVE ��|�ی��Ҕԍ�(3:2) TO ����敪�P�Q�v
                  END-IF
008410* ���ρE���q��
008420         WHEN  04
009793            MOVE 6                 TO ��������v
                  MOVE ��|�{�l�Ƒ��敪  TO ����敪�Q�P�v
008430         WHEN  09
009793            MOVE 5                 TO ��������v
                  MOVE ��|�{�l�Ƒ��敪  TO ����敪�Q�P�v
008240* ����
008270         WHEN  06
009793            MOVE 2                 TO ��������v
                  MOVE ��|�{�l�Ƒ��敪  TO ����敪�Q�P�v
008250* �D��
008280         WHEN  07
009793            MOVE 3                 TO ��������v
                  MOVE ��|�{�l�Ƒ��敪  TO ����敪�Q�P�v
008490* �ސE����
008500         WHEN  08
009793            MOVE 7                 TO ��������v
009793            MOVE 2                 TO ����敪�Q�P�v
                  MOVE ��|�{�l�Ƒ��敪  TO ����敪�Q�R�v
008560         END-EVALUATE
008570*
008580      WHEN 2
009793         MOVE 8                    TO ��������v
               MOVE ��|�{�l�Ƒ��敪     TO ����敪�Q�P�v
008600      WHEN 3
009792         IF (��|������� = 54) OR
                  ((��|������� = 60) AND (��|��p���S�Ҕԍ�����(1:2) = 51))
009793            MOVE 9                 TO ��������v
                  MOVE ��|�{�l�Ƒ��敪  TO ����敪�Q�P�v
               ELSE
009793            MOVE 10                TO ��������v
                  MOVE ��|�{�l�Ƒ��敪  TO ����敪�Q�R�v
009791            EVALUATE ��|�������
009792            WHEN 52
008610               MOVE 3              TO  ����敪�Q�P�v
009792            WHEN 53
008610               MOVE 1              TO  ����敪�Q�P�v
009792            WHEN 55
009792            WHEN 60
008610               MOVE 2              TO  ����敪�Q�P�v
009792            WHEN OTHER
008610               MOVE 4              TO  ����敪�Q�P�v
                  END-EVALUATE
               END-IF
008620      WHEN 7
008630         MOVE  99  TO  ��������v
008640     END-EVALUATE.
      *
009791     EVALUATE ��|���ʋ敪
009792     WHEN 1
009793         MOVE 2    TO ����敪�Q�Q�v
009794     WHEN 2
009793         MOVE 3    TO ����敪�Q�Q�v
009795     WHEN 3
009796         MOVE 4    TO ����敪�Q�Q�v
009797     WHEN 6
009798         MOVE 5    TO ����敪�Q�Q�v
009799     WHEN OTHER
009801         MOVE 1    TO ����敪�Q�Q�v
009803     END-EVALUATE.
008650*
008660*================================================================*
008670 ���Z���я��~���t�@�C���쐬 SECTION.
008680******************************************************************************
008690*  �S����̏ꍇ�A
008700*  ���Z�v�g�𑍊��\�̈󎚏��ԂƋt�ɕ��ׂ邽�߁A���Ԃ��o�͂���B
008710******************************************************************************
008720*
008730     OPEN INPUT ��ƃt�@�C���P.
008740         MOVE NC"��P" TO �t�@�C����.
008750         PERFORM �I�[�v���`�F�b�N.
008760*
008770     MOVE SPACE TO �I���t���O.
008780     MOVE ZERO  TO ���Ԃv.      
008790     MOVE 99         TO ��P�|��R�[�h.
008790     MOVE 999        TO ��P�|����敪�P.
008800     MOVE 99999999   TO  ��P�|�ی��Ҕԍ���.
008790     MOVE 999        TO ��P�|����敪�Q
008810     MOVE HIGH-VALUE TO ��P�|���҃J�i.
008820     MOVE 999999     TO ��P�|���Ҕԍ�.
008830     MOVE HIGH-VALUE TO ��P�|�}��.
008850     MOVE 9          TO ��P�|�{�p�a��.
008860     MOVE 99         TO ��P�|�{�p�N.
008870     MOVE 99         TO ��P�|�{�p��.
008880     START ��ƃt�@�C���P KEY IS <=  ��P�|��R�[�h    
                                           ��P�|����敪�P  
                                            ��P�|�ی��Ҕԍ���
008890                                     ��P�|����敪�Q  
008930                                     ��P�|���҃J�i    
008910                                     ��P�|���҃R�[�h  
008920                                     ��P�|�{�p�a��N��
008940                                     REVERSED
008950     END-START.
008960     IF ��ԃL�[ = "00"
008970*
008980         MOVE SPACE TO �I���t���O
008990         PERFORM ��ƃt�@�C���P�Ǎ�
009000         PERFORM UNTIL �I���t���O = "YES"
009010             PERFORM ��Q���R�[�h�Z�b�g
009020             PERFORM ��Q�t�@�C������
009030             PERFORM ��ƃt�@�C���P�Ǎ�
009040         END-PERFORM
009050     END-IF.
009060*
009070     CLOSE ��ƃt�@�C���P.
009080*
009090*================================================================*
009100 ���Z���я��t�@�C���쐬 SECTION.
009110******************************************************************************
009120*  �S����̏ꍇ�A
009130*  ���Z�v�g�𑍊��\�̈󎚏��ԂƓ����l�ɕ��ׂ邽�߁A���Ԃ��o�͂���B
009140******************************************************************************
009150*
009160     OPEN INPUT ��ƃt�@�C���P.
009170         MOVE NC"��P" TO �t�@�C����.
009180         PERFORM �I�[�v���`�F�b�N.
009190*
009200     MOVE SPACE TO �I���t���O.
009210     MOVE ZERO  TO ���Ԃv.      
009220     PERFORM ��ƃt�@�C���P�Ǎ�.
009230     PERFORM UNTIL �I���t���O = "YES"
009240             PERFORM ��Q���R�[�h�Z�b�g
009250             PERFORM ��Q�t�@�C������
009260             PERFORM ��ƃt�@�C���P�Ǎ�
009270     END-PERFORM.
009280*
009290     CLOSE ��ƃt�@�C���P.
009300*
009310*================================================================*
009320 ��Q���R�[�h�Z�b�g SECTION.
009330*
009340     MOVE ��P�|���Z���       TO ��Q�|���Z���.
009350     MOVE ��P�|�{�p�a��       TO ��Q�|�{�p�a��.
009360     MOVE ��P�|�{�p�N         TO ��Q�|�{�p�N.
009370     MOVE ��P�|�{�p��         TO ��Q�|�{�p��.
009380     MOVE ��P�|���҃R�[�h     TO ��Q�|���҃R�[�h.
009390     MOVE ��P�|�ی����       TO ��Q�|�ی����
009400     MOVE ��P�|���Ҏ���       TO ��Q�|���Ҏ���.
009410     MOVE ��P�|��ی��Ҏ���   TO ��Q�|��ی��Ҏ���.
009420     IF ��P�|�{�l�Ƒ��敪 = 1
009430         MOVE "�{�l"           TO ��Q�|�{�l�Ƒ�
009440     ELSE
009450         MOVE "�Ƒ�"           TO ��Q�|�{�l�Ƒ�
009460     END-IF.
009480     MOVE ��P�|�ی��Ҕԍ�     TO �ی��Ҕԍ��v�q.
009520     IF �ی��Ҕԍ��v�q(1:4) = "0000"
009530        MOVE �ی��Ҕԍ��v�q(5:6)  TO ��Q�|�����ی��Ҕԍ�
009540     ELSE
009550        IF �ی��Ҕԍ��v�q(1:2) = "00"
009560           MOVE �ی��Ҕԍ��v�q(3:8)  TO ��Q�|�����ی��Ҕԍ�
009570        ELSE
009580           MOVE �ی��Ҕԍ��v�q    TO ��Q�|�����ی��Ҕԍ�
009590        END-IF
009600     END-IF.
009610     MOVE ��P�|��p�z         TO ��Q�|��p�z.
009620     MOVE ��P�|�����z         TO ��Q�|�����z.
009630     MOVE ��P�|���S�z         TO ��Q�|���S�z.
009640     MOVE ��P�|���Z����敪   TO ��Q�|���Z����敪.
009650     IF ��P�|���Z��� = 7
009660        MOVE 1                 TO ��Q�|�ی��敪
009670        COMPUTE ���ۏ��Ԃv  = ���ۏ��Ԃv + 1
009680        MOVE ���ۏ��Ԃv            TO ��Q�|����
009690     ELSE
009700        MOVE ZERO              TO ��Q�|�ی��敪
009710        COMPUTE ���Ԃv  = ���Ԃv + 1
009720        MOVE ���Ԃv            TO ��Q�|����
009730     END-IF.
009740*
009750*================================================================*
009760 ��Q�t�@�C������ SECTION.
009770*
009780     WRITE ��Q�|���R�[�h
009790     INVALID KEY
009800         MOVE NC"��Q"  TO �t�@�C����
009810         PERFORM �G���[�\��
009820     END-WRITE.
009830*================================================================*
009840 ���я��t�@�C���쐬 SECTION.
009850*
009860     CLOSE ��ƃt�@�C���P.
009870     OPEN INPUT ��ƃt�@�C���P.
009880         MOVE NC"��P" TO �t�@�C����.
009890         PERFORM �I�[�v���`�F�b�N.
009900     OPEN OUTPUT ��ƃt�@�C���R.
009910     CLOSE ��ƃt�@�C���R.
009920     OPEN I-O ��ƃt�@�C���R.
009930         MOVE NC"��R" TO �t�@�C����.
009940         PERFORM �I�[�v���`�F�b�N.
009950*
009960     MOVE SPACE TO �I���t���O.
009970     MOVE ZERO  TO ���Ԃv.      
009980     MOVE ZERO  TO ���ۏ��Ԃv.      
009990     PERFORM ��ƃt�@�C���P�Ǎ�.
010010     PERFORM UNTIL �I���t���O = "YES"
010020         PERFORM ��R���R�[�h�Z�b�g
010030         PERFORM ��ƃt�@�C���P�Ǎ�
010040     END-PERFORM.
010050
010060*================================================================*
010070 ��R���R�[�h�Z�b�g SECTION.
010080*
010100     MOVE ��P�|�{�p�a��       TO ��R�|�{�p�a��.
010110     MOVE ��P�|�{�p�N         TO ��R�|�{�p�N.
010120     MOVE ��P�|�{�p��         TO ��R�|�{�p��.
010130     MOVE ��P�|���҃R�[�h     TO ��R�|���҃R�[�h.
010140     MOVE ��P�|�ی����       TO ��R�|�ی����.
010150     IF ��P�|���Z��� = 7
010160        COMPUTE ���ۏ��Ԃv  = ���ۏ��Ԃv + 1
010170        MOVE ���ۏ��Ԃv        TO ��R�|����
010180     ELSE
010190        COMPUTE ���Ԃv  = ���Ԃv + 1
010200        MOVE ���Ԃv            TO ��R�|����
010210     END-IF.
010220*
010230     WRITE ��R�|���R�[�h
010240     INVALID KEY
010250         MOVE NC"��R"  TO �t�@�C����
010260         PERFORM �G���[�\��
010270     END-WRITE.
010280*
010290*================================================================*
010300*================================================================*
010310******************************************************************
010320 END PROGRAM NJY6061.
010330******************************************************************
