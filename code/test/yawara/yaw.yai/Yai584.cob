000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAI584.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*  �U���p�����      �y�_+����޳�ޔŁz
000100*         MED = YAI584P
000101*  �����N���o�[�W����
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2013-06-27
000130 DATE-COMPILED.          2013-06-27
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
000108     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5831L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  ��Q�|�ی����
000113                                                          ��Q�|�ی��Ҕԍ�
000114                             ALTERNATE RECORD KEY     IS  ��Q�|��������P
000115                                                          ��Q�|��
000116                                                          ��Q�|�������
000117                                                          ��Q�|�ی����
000118                                                          ��Q�|�ی��Ҕԍ�
000119                             FILE        STATUS       IS  ��ԃL�[
000120                             LOCK        MODE         IS  AUTOMATIC.
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
      *
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
000129*                           �m�q�k��  �R�Q�O�n
000130 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
000131     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001123*                           �m�q�k��  �P�Q�W�n
001124 FD  ��ƃt�@�C���Q RECORD  CONTAINS 128 CHARACTERS.
001125 01  ��Q�|���R�[�h.
001126     03  ��Q�|���R�[�h�L�[.
001131         05  ��Q�|�ی����                  PIC 9(2).
001132         05  ��Q�|�ی��Ҕԍ�                PIC X(10).
001137     03  ��Q�|���R�[�h�f�[�^.
001133         05  ��Q�|��                        PIC X(2).
001134         05  ��Q�|�������.
001135             07  ��Q�|��������P            PIC 9(2).
001136             07  ��Q�|��������Q            PIC 9.
001138         05  ��Q�|����                      PIC 9(4).
001139         05  ��Q�|��p�z                    PIC 9(9).
001140         05  ��Q�|���S�z                    PIC 9(9).
001141         05  ��Q�|�����z                    PIC 9(9).
001142         05  ��Q�|�{�l����                  PIC 9(3).
001143         05  ��Q�|�{�l��p�z                PIC 9(7).
001144         05  ��Q�|�{�l���S�z                PIC 9(7).
001145         05  ��Q�|�{�l�����z                PIC 9(7).
001146         05  ��Q�|�Ƒ�����                  PIC 9(3).
001147         05  ��Q�|�Ƒ���p�z                PIC 9(7).
001148         05  ��Q�|�Ƒ����S�z                PIC 9(7).
001149         05  ��Q�|�Ƒ������z                PIC 9(7).
001150         05  FILLER                          PIC X(32).
001350* 
001360 FD  ����t�@�C��.
001370     COPY YAI584P         OF  XMDLIB.
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
001470 01 �t�@�C�����v                       PIC N(6) VALUE SPACE.
001490 01 ���s�L�[�v                         PIC X(4) VALUE SPACE.
001590 01 �t�@�C����                         PIC N(4) VALUE SPACE.
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
001801 01 �S�p��                           PIC X(2)  VALUE X"8140".
001802 01 ���p��                           PIC X(2)  VALUE X"2020".
001920***
001921***
001922 01 ��ʏ��v.
001923    03 �����N���v.
001924       05 �����a��v                   PIC 9     VALUE ZERO.
001925       05 �����N�v                     PIC 9(2)  VALUE ZERO.
001926       05 �������v                     PIC 9(2)  VALUE ZERO.
001927    03 ��o�N�����v.
001928       05 ��o�a��v                   PIC 9     VALUE ZERO.
001929       05 ��o�N�v                     PIC 9(2)  VALUE ZERO.
001930       05 ��o���v                     PIC 9(2)  VALUE ZERO.
001931       05 ��o���v                     PIC 9(2)  VALUE ZERO.
001933*
001934**************
001935* �{�p����� *
001936**************
001937 01 �{�p�����v.
001938    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
001939    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
001940    03 �_���t�ԍ��v.
             05 �_���t�L���v                 PIC X(2)   VALUE SPACE.
             05 �_���t�ԍ��P�v               PIC X(7)   VALUE SPACE.
             05 �_���t��؂P�v               PIC X(1)   VALUE SPACE.
             05 �_���t�ԍ��Q�v               PIC X(1)   VALUE SPACE.
             05 �_���t��؂Q�v               PIC X(1)   VALUE SPACE.
             05 �_���t�ԍ��R�v               PIC X(1)   VALUE SPACE.
001941    03 �{�p���Z���v.
001942       05 �{�p���Z���P�v               PIC X(40)  VALUE SPACE.
001943       05 �{�p���Z���Q�v               PIC X(40)  VALUE SPACE.
001944    03 �{�p���X�֔ԍ��v.
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
001956        05 �������`�l�J�i�v            PIC X(40)  VALUE SPACE.
001957        05 �������`�l�v                PIC X(40)  VALUE SPACE.
001958*
001959 01 �A�Ԃv                             PIC 9(3)   VALUE ZERO.
001960 01 ��s���x�X���v                     PIC X(40)  VALUE SPACE.
001961 01 �a����ʃR�����g�v                 PIC N(2)   VALUE SPACE.
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
001972*
001973* �Еۗp
001974 01 �ڔ���敪�v                       PIC 9     VALUE ZERO.
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
       01 �A���|��ʏ��x�`�h�T�W�O IS EXTERNAL.
          03 �A���|�����a��N��.
             05 �A���|�����a��               PIC 9(1).
             05 �A���|�����N��.
                07 �A���|�����N              PIC 9(2).
                07 �A���|������              PIC 9(2).
          03 �A���|�쐬�a��N����.
             05 �A���|�쐬�a��N��.
                07 �A���|�쐬�a��            PIC 9(1).
                07 �A���|�쐬�N              PIC 9(2).
                07 �A���|�쐬��              PIC 9(2).
             05 �A���|�쐬��                 PIC 9(2).
002351*
       01 �A��|�v���r���[ IS EXTERNAL.
          03 �A��|�v���r���[�敪            PIC 9.
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
002440************
002450*           *
002460* �又��     *
002470*           *
002480************
002484     PERFORM �������.
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
002974     MOVE "YAI584"              TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A��|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
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
002800     OPEN INPUT ��ƃt�@�C���Q.
002810             MOVE NC"��Q" TO �t�@�C�����v.
002820             PERFORM �I�[�v���`�F�b�N.
003250     OPEN INPUT ���Z�v�g�e.
003260         MOVE NC"���Z�v�g�e" TO �t�@�C����.
003270         PERFORM �I�[�v���`�F�b�N.
002590     OPEN INPUT ��f�ҏ��e.
002600         MOVE NC"��f�ҏ��e" TO �t�@�C�����v.
002610         PERFORM �I�[�v���`�F�b�N.
002830*
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
003491*================================================================*
003492 �A�����ڑޔ� SECTION.
003493*
003494     MOVE �A���|�����a��  TO �����a��v.
003495     MOVE �A���|�����N    TO �����N�v.
003496     MOVE �A���|������    TO �������v.
003497     MOVE �A���|�쐬�a��  TO ��o�a��v.
003498     MOVE �A���|�쐬�N    TO ��o�N�v.
003499     MOVE �A���|�쐬��    TO ��o���v.
003500     MOVE �A���|�쐬��    TO ��o���v.
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
003512         MOVE �{��|�X�֔ԍ��P       TO �{�p���X�֔ԍ��P�v
003513         MOVE "-"                    TO �{�p���X�֔ԍ���؂v
003514         MOVE �{��|�X�֔ԍ��Q       TO �{�p���X�֔ԍ��Q�v
003515         MOVE �{��|��\�Җ�         TO ��\�Җ��v
003516         MOVE �{��|�ڍ��@��         TO �ڍ��@���v
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
003532         STRING ������s���v     DELIMITED BY SPACE
003533                " "                DELIMITED BY SIZE
003534                ������s�x�X���v DELIMITED BY SPACE
003535                INTO ��s���x�X���v
003536         END-STRING
003537         EVALUATE �a����ʂv
003538         WHEN 1
003539             MOVE NC"����" TO �a����ʃR�����g�v
003540         WHEN 2
003541             MOVE NC"����" TO �a����ʃR�����g�v
003542         WHEN OTHER
003543             MOVE SPACE    TO �a����ʃR�����g�v
003544         END-EVALUATE
003545*
003546     END-READ.
003547*================================================================*
003548 �t�@�C���� SECTION.
003549*
003550     CLOSE �����}�X�^   ��ƃt�@�C���Q  �{�p�����}�X�^ �s�����}�X�^ 
003552           �ی��҃}�X�^ ������}�X�^    ��f�ҏ��e     ���Z�v�g�e.
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
003856     MOVE LOW-VALUE TO  ��Q�|��.
003857     MOVE ZERO      TO  ��Q�|�������.
003858     MOVE ZERO      TO  ��Q�|�ی����.
003859     MOVE LOW-VALUE TO  ��Q�|�ی��Ҕԍ�.
005617     START ��ƃt�@�C���Q   KEY IS >=  ��Q�|��������P
005621                                       ��Q�|��
005622                                       ��Q�|�������
005623                                       ��Q�|�ی����
005624                                       ��Q�|�ی��Ҕԍ�
005625     END-START.
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
003882*
003883             MOVE SPACE TO YAI584P
003884             INITIALIZE    YAI584P
003885             MOVE ��Q�|��         TO ���v
003886             MOVE ��Q�|��������P TO ��������v
003887             PERFORM  �\�Z�b�g�P
003888* 
003889             PERFORM VARYING �s�J�E���^ FROM 1 BY 1
003890                     UNTIL ( �s�J�E���^ > 8       ) OR
003891                           ( ���v       NOT = ��Q�|��       ) OR
003892                           ( ��������v NOT = ��Q�|��������P ) OR
003893                           ( �I���t���O = "YES" )
003897                  PERFORM �\�Z�b�g�Q
003901                  PERFORM ��ƃt�@�C���Q�Ǎ�
003903             END-PERFORM
003904*
003905             IF ( �I���t���O =  "YES" ) OR 
003906                ( ��������v NOT = ��Q�|��������P ) OR
003907                ( ���v       NOT = ��Q�|�� )
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
003950             END-IF
003953*
003954             PERFORM �󎚏���
003955             PERFORM ���ŏ���
                   INITIALIZE ���v�p�v
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
004610*
002990     IF ( �I�[�v���t���O = "YES" )
002991         CLOSE ����t�@�C��
003041     END-IF.
004621*
004630*================================================================*
004640 �\�Z�b�g�P SECTION.
004650*
004944* �����̘a����擾
004945*     MOVE �����a��v         TO ���|�����敪.
004946*     READ �����}�X�^
004947*     INVALID KEY
004948*         MOVE SPACE          TO �����a���
004949*     NOT INVALID KEY
004950*         MOVE ���|��������   TO �����a���
004951*     END-READ.
004952*
      */�����C��/������20190514
037370     IF �����a��v > 4
              MOVE �����a��v         TO ���|�����敪
037380        READ �����}�X�^
037390        NOT INVALID KEY
037400            MOVE ���|��������   TO �{�p�a��
037410        END-READ
              MOVE "===="             TO �{�p�a�����
           END-IF.
      */�����C��/������20190514
004953     MOVE �����N�v           TO �����N.
004954     MOVE �������v           TO ������.
004955*
004968*     MOVE �{�p���Z���v       TO �Z��.
004969*     MOVE �{�p���X�֔ԍ��v   TO �X�֔ԍ�.
004970     MOVE ��\�Җ��v         TO ��\�Җ�.
004971     MOVE �ڍ��@���v         TO �ڍ��@��.
004972     MOVE �_���t�ԍ��P�v     TO �_���t�ԍ��P.
004972     MOVE �_���t�ԍ��Q�v     TO �_���t�ԍ��Q.
004972     MOVE �_���t�ԍ��R�v     TO �_���t�ԍ��R.
004973*     MOVE �{�p���d�b�ԍ��v   TO �d�b�ԍ�.
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
006280     IF ��Q�|�ی���� = "50" OR "51" OR "52" OR "53" OR
006290                         "54" OR "55" OR "60" 
006300        MOVE "�i�����j" TO �ی��Җ�(���v�s�J�E���^)
006310     END-IF.
006320     IF ��Q�|�ی���� = "05"
006330        MOVE "�i�V�l�j" TO �ی��Җ�(���v�s�J�E���^)
006340     END-IF.
006350     IF ��Q�|�ی���� = "08"
006360        MOVE "�i�ސE�j" TO �ی��Җ�(���v�s�J�E���^)
006370     END-IF.
      */�ی��Җ��̈����ی��Ҕԍ�����ɕύX/1311
           MOVE SPACE            TO �ی��Җ�(���v�s�J�E���^ - 2).
           MOVE SPACE            TO �ی��Җ�(���v�s�J�E���^ - 1).
           MOVE SPACE            TO �ی��Җ�(���v�s�J�E���^).
           MOVE ��Q�|�ی��Ҕԍ� TO �ی��Ҕԍ�(�s�J�E���^).
006380*
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
006570     MOVE "YAI584P" TO  ��`�̖��o.
006580     MOVE SPACE     TO  ������ʂo.
006590     MOVE "SCREEN"  TO  ���ڌQ���o.
006610     WRITE YAI584P.
006620     PERFORM �G���[�����o.
006650*================================================================*
006660 ���ŏ���  SECTION.
006670
006680     MOVE "YAI584P" TO  ��`�̖��o.
006690     MOVE "CT"      TO  ������ʂo.
006700     MOVE "PAGE"    TO  �g������o.
006710     MOVE SPACE     TO  ���ڌQ���o.
006730     WRITE YAI584P.
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
007011*================================================================*
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
                 END-READ
019900        ELSE
019910           MOVE SPACE  TO ���s�L�[�v
              END-IF
019950     END-IF.
004860*
004868*================================================================*
006930 ���Z�v�g�e�Ǎ� SECTION.
006940*
006950     READ ���Z�v�g�e NEXT
004790     AT END
004800         MOVE "YES" TO �I���t���O�R
004810     END-READ.
004868*================================================================*
007012******************************************************************
007013 END PROGRAM YAI584.
007014******************************************************************
