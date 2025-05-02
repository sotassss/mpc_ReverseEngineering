000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             NJY582.
000060 AUTHOR.                 �r�c  �K�q
000070*
000080*----------------------------------------------------------------*
000090*        �y�×{��������[ ����z
000110*         MED = NJY580 NJY582P 
      *    �o�͓��e�ύX�@071016
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2019-10-24
000140 DATE-COMPILED.          2019-10-24
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
000270     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000280                             ORGANIZATION             IS  INDEXED
000290                             ACCESS MODE              IS  DYNAMIC
000300                             RECORD KEY               IS  ���|�����敪
000310                             FILE STATUS              IS  ��ԃL�[
000320                             LOCK        MODE         IS  AUTOMATIC.
000330     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000340                             ORGANIZATION             IS  INDEXED
000350                             ACCESS MODE              IS  DYNAMIC
000360                             RECORD KEY               IS  ���|����敪
000370                             FILE STATUS              IS  ��ԃL�[
000380                             LOCK        MODE         IS  AUTOMATIC.
000390     SELECT  �{�p�����}�X�^ ASSIGN     TO        SEJOHOL
000400                             ORGANIZATION             IS  INDEXED
000410                             ACCESS MODE              IS  DYNAMIC
000420                             RECORD KEY               IS  �{��|�{�p���ԍ�
000430                             FILE STATUS              IS  ��ԃL�[
000440                             LOCK        MODE         IS  AUTOMATIC.
000450     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W58021L.DAT"
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS                   IS  DYNAMIC
000480                             RECORD      KEY          IS  ��P�|�ی��敪
000490                             FILE        STATUS       IS  ��ԃL�[
000500                             LOCK        MODE         IS  AUTOMATIC.
000510     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF001
000520                             SYMBOLIC    DESTINATION  IS "PRT"
000530                             FORMAT                   IS  ��`�̖��o
000540                             GROUP                    IS  ���ڌQ���o
000550                             PROCESSING  MODE         IS  ������ʂo
000560                             UNIT        CONTROL      IS  �g������o
000570                             FILE        STATUS       IS  �ʒm���o.
000580******************************************************************
000590*                      DATA DIVISION                             *
000600******************************************************************
000610 DATA                    DIVISION.
000620 FILE                    SECTION.
000630*                           �m�q�k��  �P�Q�W�n
000640 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000650     COPY GENGOU         OF  XFDLIB  JOINING    ��   AS  PREFIX.
000660*                           �m�q�k��  �Q�T�U�n
000670 FD  ������}�X�^      BLOCK   CONTAINS   1   RECORDS.
000680     COPY SEIGYO         OF  XFDLIB  JOINING    ��   AS  PREFIX.
000690*                           �m�q�k��  �U�S�O�n
000700 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
000710     COPY SEJOHO         OF  XFDLIB  JOINING    �{�� AS  PREFIX.
000720*
000721**
001320 FD  ��ƃt�@�C���P RECORD  CONTAINS 32 CHARACTERS.
001330 01  ��P�|���R�[�h.
001340   03  ��P�|���R�[�h�L�[.
001350     05  ��P�|�ی��敪                PIC 9(2).
001360   03  ��P�|���R�[�h�f�[�^.
001370     05  ��P�|����                    PIC 9(4).
001380     05  ��P�|��p�z                  PIC 9(8).
001390     05  ��P�|�����z                  PIC 9(8).
001400     05  FILLER                        PIC X(10).
000820*
000830 FD  ����t�@�C��.
000840     COPY NJY582P        OF  XMDLIB.
000850*
000860******************************************************************
000870*                WORKING-STORAGE SECTION                         *
000880******************************************************************
000890 WORKING-STORAGE         SECTION.
000900 01 �L�[����                           PIC X    VALUE SPACE.
000910 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
000920 01 �I���t���O                         PIC X(3) VALUE SPACE.
000930 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
000940 01 �I���t���O�R                       PIC X(3) VALUE SPACE.
000950 01 �m�F���͂v                         PIC X(1) VALUE SPACE.
000960 01 �t�@�C�����v                       PIC N(2) VALUE SPACE.
000970 01 �J�����g�����v                     PIC 9(1) VALUE ZERO.
000980 01 ���s�L�[�v                         PIC X(4) VALUE SPACE.
000990 01 �O�a��v                           PIC 9 VALUE ZERO.
001000 01 �s���t���O                         PIC X(3) VALUE SPACE.
001210 01 �I�[�v���t���O                     PIC X(3)   VALUE SPACE.
001010 01 �����ړ��L�[                       PIC X(4) VALUE SPACE.
001020 01 �{�p�a��N���v.
001030     03 �{�p�a��v                     PIC 9.
001040     03 �{�p�N���v.
001050        05 �{�p�N�v                    PIC 9(2).
001060        05 �{�p���v                    PIC 9(2).
001070 01 �{�p���v                           PIC 9(2).
001080 01 �����a��N���v.
001090    03 �����a��v                      PIC 9     VALUE ZERO.
001100    03 �����N�v                        PIC 9(2)  VALUE ZERO.
001110    03 �������v                        PIC 9(2)  VALUE ZERO.
001120 01 �������v                           PIC 9(2).
001130 01 �����a��̂v                     PIC N(2) VALUE SPACE.
001140 01 �������̂v                         PIC N(2) VALUE SPACE.
001150 01 �J�E���^                           PIC 9(2) VALUE ZERO.
001160 01 ��������b�m�s                     PIC 9(2) VALUE ZERO.
001170 01 �����\�v.
001180    03 ����ԍ��v                      PIC X(10) VALUE SPACE.
001180    03 �{�p���Z���v                    PIC X(50) VALUE SPACE.
001190    03 �{�p���Z���P�v                  PIC X(50) VALUE SPACE.
001200    03 �{�p���Z���Q�v                  PIC X(50) VALUE SPACE.
001210    03 �ڍ��@���v                      PIC X(50) VALUE SPACE.
001220    03 ��\�Җ��v                      PIC X(50) VALUE SPACE.
001230    03 �{�p���d�b�ԍ��v                PIC X(15) VALUE SPACE.
001240    03 �{�p���X�֔ԍ��v.
001250       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
001260       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
001270    03 ���׍s.
001280       05 ���ی����v                   PIC 9(4)  VALUE ZERO.
001290       05 ���۔�p�z�v                 PIC 9(8)  VALUE ZERO.
001300       05 ���ې����z�v                 PIC 9(8)  VALUE ZERO.
001300       05 ���ە��ϐ����z�v             PIC 9(8)  VALUE ZERO.
001310       05 �Еی����v                   PIC 9(4)  VALUE ZERO.
001320       05 �Е۔�p�z�v                 PIC 9(8)  VALUE ZERO.
001330       05 �Еې����z�v                 PIC 9(8)  VALUE ZERO.
001300       05 �Еە��ϐ����z�v             PIC 9(8)  VALUE ZERO.
001310       05 ���ٌ����v                   PIC 9(4)  VALUE ZERO.
001320       05 ���ٔ�p�z�v                 PIC 9(8)  VALUE ZERO.
001330       05 ���ِ����z�v                 PIC 9(8)  VALUE ZERO.
001300       05 ���ٕ��ϐ����z�v             PIC 9(8)  VALUE ZERO.
001310       05 �D�������v                   PIC 9(4)  VALUE ZERO.
001320       05 �D����p�z�v                 PIC 9(8)  VALUE ZERO.
001330       05 �D�������z�v                 PIC 9(8)  VALUE ZERO.
001300       05 �D�����ϐ����z�v             PIC 9(8)  VALUE ZERO.
001370       05 �g�������v                   PIC 9(4)  VALUE ZERO.
001380       05 �g����p�z�v                 PIC 9(8)  VALUE ZERO.
001390       05 �g�������z�v                 PIC 9(8)  VALUE ZERO.
001300       05 �g�����ϐ����z�v             PIC 9(8)  VALUE ZERO.
001340       05 ���ό����v                   PIC 9(4)  VALUE ZERO.
001350       05 ���ϔ�p�z�v                 PIC 9(8)  VALUE ZERO.
001360       05 ���ϐ����z�v                 PIC 9(8)  VALUE ZERO.
001300       05 ���ϕ��ϐ����z�v             PIC 9(8)  VALUE ZERO.
001280       05 �ސE�����v                   PIC 9(4)  VALUE ZERO.
001290       05 �ސE��p�z�v                 PIC 9(8)  VALUE ZERO.
001300       05 �ސE�����z�v                 PIC 9(8)  VALUE ZERO.
001300       05 �ސE���ϐ����z�v             PIC 9(8)  VALUE ZERO.
001430       05 �㍂�����v                   PIC 9(4)  VALUE ZERO.
001440       05 �㍂��p�z�v                 PIC 9(8)  VALUE ZERO.
001450       05 �㍂�����z�v                 PIC 9(8)  VALUE ZERO.
001300       05 �㍂���ϐ����z�v             PIC 9(8)  VALUE ZERO.
001430       05 ���������v                   PIC 9(4)  VALUE ZERO.
001440       05 ������p�z�v                 PIC 9(8)  VALUE ZERO.
001450       05 ���������z�v                 PIC 9(8)  VALUE ZERO.
001300       05 �������ϐ����z�v             PIC 9(8)  VALUE ZERO.
001430       05 ��Q�����v                   PIC 9(4)  VALUE ZERO.
001440       05 ��Q��p�z�v                 PIC 9(8)  VALUE ZERO.
001450       05 ��Q�����z�v                 PIC 9(8)  VALUE ZERO.
001300       05 ��Q���ϐ����z�v             PIC 9(8)  VALUE ZERO.
001430       05 ��q�����v                   PIC 9(4)  VALUE ZERO.
001440       05 ��q��p�z�v                 PIC 9(8)  VALUE ZERO.
001450       05 ��q�����z�v                 PIC 9(8)  VALUE ZERO.
001300       05 ��q���ϐ����z�v             PIC 9(8)  VALUE ZERO.
001430       05 ���������v                   PIC 9(4)  VALUE ZERO.
001440       05 ������p�z�v                 PIC 9(8)  VALUE ZERO.
001450       05 ���������z�v                 PIC 9(8)  VALUE ZERO.
001300       05 �������ϐ����z�v             PIC 9(8)  VALUE ZERO.
001430       05 �픚�����v                   PIC 9(4)  VALUE ZERO.
001440       05 �픚��p�z�v                 PIC 9(8)  VALUE ZERO.
001450       05 �픚�����z�v                 PIC 9(8)  VALUE ZERO.
001300       05 �픚���ϐ����z�v             PIC 9(8)  VALUE ZERO.
001430       05 ���̑������v                 PIC 9(4)  VALUE ZERO.
001440       05 ���̑���p�z�v               PIC 9(8)  VALUE ZERO.
001450       05 ���̑������z�v               PIC 9(8)  VALUE ZERO.
001300       05 ���̑����ϐ����z�v           PIC 9(8)  VALUE ZERO.
001460       05 �J�Ќ����v                   PIC 9(4)  VALUE ZERO.
001470       05 �J�Д�p�z�v                 PIC 9(8)  VALUE ZERO.
001480       05 �J�А����z�v                 PIC 9(8)  VALUE ZERO.
001300       05 �J�Е��ϐ����z�v             PIC 9(8)  VALUE ZERO.
001460       05 ���������v                   PIC 9(4)  VALUE ZERO.
001470       05 ������p�z�v                 PIC 9(8)  VALUE ZERO.
001480       05 ���������z�v                 PIC 9(8)  VALUE ZERO.
001300       05 �������ϐ����z�v             PIC 9(8)  VALUE ZERO.
001280       05 ���ی����v                   PIC 9(4)  VALUE ZERO.
001290       05 ���۔�p�z�v                 PIC 9(8)  VALUE ZERO.
001300       05 ���ې����z�v                 PIC 9(8)  VALUE ZERO.
001300       05 ���ە��ϐ����z�v             PIC 9(8)  VALUE ZERO.
001490    03 ����f�[�^.
001500       05 ���v�����v                   PIC 9(4)  VALUE ZERO.
001500       05 ���v�����v                   PIC 9(5)  VALUE ZERO.
001510       05 ���v��p�z�v                 PIC 9(9)  VALUE ZERO.
001520       05 ���v�����z�v                 PIC 9(9)  VALUE ZERO.
001500       05 �����v�����v                 PIC 9(5)  VALUE ZERO.
001520       05 �����v��p�z�v               PIC 9(9)  VALUE ZERO.
001520       05 �����v�����z�v               PIC 9(9)  VALUE ZERO.
001500       05 �����v�����v                 PIC 9(5)  VALUE ZERO.
001520       05 �����v��p�z�v               PIC 9(9)  VALUE ZERO.
001520       05 �����v�����z�v               PIC 9(9)  VALUE ZERO.
001530 01 �Ő��v                             PIC 9(1)  VALUE ZERO.
001540 01 �S�łv                             PIC 9(1)  VALUE ZERO.
001550 01 �ی���ʂv                         PIC 9(2)  VALUE ZERO.
001560 01 �����ʂv                         PIC 9(2)  VALUE ZERO.
001570 01 ���t�����v                         PIC 9(2)  VALUE ZERO.
001580 01 �{�l�Ƒ��敪�v                     PIC 9(1)  VALUE ZERO.
001590 01 �{�p���R�[�h�v                     PIC X(7)  VALUE SPACE.
001790*
001800 01 �������.
001810     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
001820     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
001830     03 ������ʂo                     PIC X(2) VALUE SPACE.
001840     03 �g������o.
001850         05 �[������o.
001860             07 �ړ������o             PIC X(1).
001870             07 �ړ��s���o             PIC 9(3).
001880         05 �ڍא���o                 PIC X(2).
001890     03 �ʒm���o                     PIC X(2) VALUE SPACE.
001900     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
001910 01 �v�Z�@����N�v                     PIC 9(2).
001920* ���t�v�n�q�j
001930 01 �a��I���N�v                       PIC 9(4).
001940 01 �v�Z�@�a��N�v                     PIC 9(2).
001950 01 �v�Z�@����.
001960    03 �v�Z�@����N                    PIC 9(4).
001970    03 �v�Z�@�����                  PIC 9(4).
001980 01 �v�Z�@����q REDEFINES �v�Z�@����.
001990    03 �v�Z�@���I                      PIC 9(2).
002000    03 �v�Z�@���t                      PIC 9(6).
002010    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
002020       05 �v�Z�@�N��                   PIC 9(4).
002030       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
002040         07 �v�Z�@�N                   PIC 9(2).
002050         07 �v�Z�@��                   PIC 9(2).
002060       05 �v�Z�@��                     PIC 9(2).
002070 01 �{�p�a��N���v�q.
002080    03 �{�p�a��v�q                  PIC 9     VALUE ZERO.
002090    03 �{�p�N�v�q                    PIC 9(2)  VALUE ZERO.
002100    03 �{�p���v�q                    PIC 9(2)  VALUE ZERO.
002110*
002120******************************************************************
002130*                          �A������                              *
002140******************************************************************
002260*
002494 01 �A���|�L�[ IS EXTERNAL.
002495    03  �A���|���b�Z�[�W                 PIC N(20).
002150*
       01 �A��|��ʏ��m�i�x�T�W�O IS EXTERNAL GLOBAL.
          03 �A��|�����a��N��.
             05 �A��|�����a��               PIC 9.
             05 �A��|�����N��.
                07 �A��|�����N              PIC 9(2).
                07 �A��|������              PIC 9(2).
          03 �A��|�v���r���[�敪            PIC 9.
002220*
003090 01 �A��|����f�[�^�m�i�x�T�W�O IS EXTERNAL.
003100   03 �A��|���v.
003110     05 �A��|���v����                 PIC 9(4).
003110     05 �A��|���v����                 PIC 9(5).
003120     05 �A��|���v��p�z               PIC 9(9).
003130     05 �A��|���v�����z               PIC 9(9).
003110     05 �A��|�����v����               PIC 9(5).
003130     05 �A��|�����v��p�z             PIC 9(9).
003130     05 �A��|�����v�����z             PIC 9(9).
003110     05 �A��|�����v����               PIC 9(5).
003120     05 �A��|�����v��p�z             PIC 9(9).
003130     05 �A��|�����v�����z             PIC 9(9).
002560*
000540************************************
000550* �v�����^�t�@�C���쐬�p           *
000560************************************
000570 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000580     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
000590     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
000600     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
000610     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
002271*
002280******************************************************************
002290*                      PROCEDURE  DIVISION                       *
002300******************************************************************
002310 PROCEDURE               DIVISION.
002320************
002330*           *
002340* ��������   *
002350*           *
002360************
002560     MOVE SPACE TO �I�[�v���t���O.
002570     PERFORM �v�����^�t�@�C���쐬.
002370     PERFORM ������.
002380************
002390*           *
002400* �又��     *
002410*           *
002420************
002430     PERFORM �A�����ڑҔ�.
002440     PERFORM �{�p�����}�X�^�Ǎ�.
002450     PERFORM ���z�W�v.
002460     PERFORM ������R�[�h�Z�b�g.
002470     PERFORM �������.
002480************
002490*           *
002500* �I������   *
002510*           *
002520************
002530     PERFORM �I������.
002540     EXIT PROGRAM.
002550*
002560*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
      *   �󎚒����L��
002971     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
002972*
002973*   �g�p���钠�[�v���O�������Z�b�g
002974     MOVE "NJY582"             TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A��|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
002570*================================================================*
002580 ������ SECTION.
002590*
002600     OPEN INPUT �����}�X�^.
002610             MOVE NC"����" TO �t�@�C�����v.
002620             PERFORM �I�[�v���`�F�b�N.
002630     OPEN INPUT ������}�X�^.
002640             MOVE NC"����" TO �t�@�C�����v.
002650             PERFORM �I�[�v���`�F�b�N.
002660     OPEN INPUT �{�p�����}�X�^.
002670             MOVE NC"�{��" TO �t�@�C�����v.
002680             PERFORM �I�[�v���`�F�b�N.
002710*
002870* �A�����ڂ̑Ҕ�
002880     MOVE �A��|�����a�� TO �����a��v.
002890     MOVE �A��|�����N   TO �����N�v.
002900     MOVE �A��|������   TO �������v.
002910     MOVE �����a��v  TO ���|�����敪.
002920     READ �����}�X�^
002930     INVALID KEY
002940         MOVE SPACE         TO �����a��̂v
002950     NOT INVALID KEY
002960         MOVE ���|��������  TO �����a��̂v
002970     END-READ.
002980*
002990*================================================================*
003000 �I�[�v���`�F�b�N SECTION.
003010*
003020     IF ��ԃL�[  NOT =  "00"
003030         DISPLAY �t�@�C�����v NC"�e�I�[�v���G���[" UPON CONS
003040         DISPLAY NC"��ԃL�[�F" ��ԃL�[           UPON CONS
003050         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003060                                                   UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
003070         ACCEPT  �L�[���� FROM CONS
003080         PERFORM �t�@�C����
003090         EXIT PROGRAM.
003530*================================================================*
003540 �{�p�����}�X�^�Ǎ� SECTION.
003550*
003560     MOVE ZERO TO �{��|�{�p���ԍ�.
003570     READ �{�p�����}�X�^
003580     INVALID KEY
003590         MOVE NC"�{��" TO �t�@�C�����v
003600         PERFORM �G���[�\���q
003610         PERFORM �t�@�C����
003620         MOVE 99 TO PROGRAM-STATUS
003630         EXIT PROGRAM
003640     NOT INVALID KEY
003650         MOVE �{��|�ڍ��t�����ԍ�  TO ����ԍ��v
003720         MOVE �{��|�ڍ��@��         TO �ڍ��@���v
003730         MOVE �{��|��\�Җ�         TO ��\�Җ��v
003750     END-READ.
003760*
003770*================================================================*
003810*================================================================*
003820 �t�@�C���� SECTION.
003830*
003570     IF ( �I�[�v���t���O = "YES" )
003580         CLOSE ����t�@�C��
003590     ELSE
003600         MOVE  NC"�@�@�f�[�^���O���ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
003610         CALL   "MSG001"
003620         CANCEL "MSG001"
003630     END-IF.
003640*
003840     CLOSE  �����}�X�^   ������}�X�^ �{�p�����}�X�^.
003860*================================================================*
003870 �I������ SECTION.
003880*
003890     PERFORM �t�@�C����.
003900*================================================================*
003910 �A�����ڑҔ� SECTION.
003920*
003930     MOVE SPACE TO �����\�v.
003940     INITIALIZE    �����\�v.
003950     MOVE  �A��|���v����         TO ���v�����v.
003950     MOVE  �A��|���v����         TO ���v�����v.
003960     MOVE  �A��|���v��p�z       TO ���v��p�z�v.
003970     MOVE  �A��|���v�����z       TO ���v�����z�v.
003950     MOVE  �A��|�����v����       TO �����v�����v.
003970     MOVE  �A��|�����v��p�z     TO �����v��p�z�v.
003970     MOVE  �A��|�����v�����z     TO �����v�����z�v.
003970     MOVE  �A��|�����v����       TO �����v�����v.
003970     MOVE  �A��|�����v��p�z     TO �����v��p�z�v.
003970     MOVE  �A��|�����v�����z     TO �����v�����z�v.
003980*================================================================*
003990 ���z�W�v SECTION.
004000*
004010     OPEN INPUT ��ƃt�@�C���P.
004020         MOVE NC"��P" TO �t�@�C�����v.
004030         PERFORM �I�[�v���`�F�b�N.
004040     MOVE SPACE TO �I���t���O�Q.
004050     MOVE SPACE TO �I���t���O�R.
004060     PERFORM ��ƃt�@�C���P�Ǎ�.
004070     PERFORM UNTIL ( �I���t���O�Q NOT = SPACE )
004080         PERFORM �W�v�����P
004090         PERFORM ��ƃt�@�C���P�Ǎ�
004100     END-PERFORM.
004110     CLOSE ��ƃt�@�C���P.
004120*================================================================*
004130 ��ƃt�@�C���P�Ǎ� SECTION.
004140*
004150     READ ��ƃt�@�C���P NEXT
004160     AT END
004170         MOVE "YES" TO �I���t���O�Q
004180         INITIALIZE ��P�|���R�[�h
004190     END-READ.
004200*================================================================*
004210 �W�v�����P SECTION.
004220*
004230     EVALUATE ��P�|�ی��敪
004360     WHEN 1
004290         MOVE ��P�|����       TO �Еی����v  
004300         MOVE ��P�|��p�z     TO �Е۔�p�z�v
004310         MOVE ��P�|�����z     TO �Еې����z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE �Еە��ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004280     WHEN 3
004290         MOVE ��P�|����       TO ���ٌ����v  
004300         MOVE ��P�|��p�z     TO ���ٔ�p�z�v
004310         MOVE ��P�|�����z     TO ���ِ����z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE ���ٕ��ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004240     WHEN 2
004290         MOVE ��P�|����       TO �D�������v  
004300         MOVE ��P�|��p�z     TO �D����p�z�v
004310         MOVE ��P�|�����z     TO �D�������z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE �D�����ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004240     WHEN 4
004370         MOVE ��P�|����       TO �g�������v  
004380         MOVE ��P�|��p�z     TO �g����p�z�v
004390         MOVE ��P�|�����z     TO �g�������z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE �g�����ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004240     WHEN 5
004330         MOVE ��P�|����       TO ���ό����v  
004340         MOVE ��P�|��p�z     TO ���ϔ�p�z�v
004350         MOVE ��P�|�����z     TO ���ϐ����z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE ���ϕ��ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004320     WHEN 6
004250         MOVE ��P�|����       TO ���ی����v  
004260         MOVE ��P�|��p�z     TO ���۔�p�z�v
004270         MOVE ��P�|�����z     TO ���ې����z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE ���ە��ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004440     WHEN 7
004410         MOVE ��P�|����       TO �ސE�����v  
004420         MOVE ��P�|��p�z     TO �ސE��p�z�v
004430         MOVE ��P�|�����z     TO �ސE�����z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE �ސE���ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004400     WHEN 8
004410         MOVE ��P�|����       TO �㍂�����v  
004420         MOVE ��P�|��p�z     TO �㍂��p�z�v
004430         MOVE ��P�|�����z     TO �㍂�����z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE �㍂���ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004400     WHEN 9
004410         MOVE ��P�|����       TO �픚�����v  
004420         MOVE ��P�|��p�z     TO �픚��p�z�v
004430         MOVE ��P�|�����z     TO �픚�����z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE �픚���ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004400     WHEN 10
004410         MOVE ��P�|����       TO ��Q�����v  
004420         MOVE ��P�|��p�z     TO ��Q��p�z�v
004430         MOVE ��P�|�����z     TO ��Q�����z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE ��Q���ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004400     WHEN 11
004410         MOVE ��P�|����       TO ���������v  
004420         MOVE ��P�|��p�z     TO ������p�z�v
004430         MOVE ��P�|�����z     TO ���������z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE �������ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004400     WHEN 12
004410         MOVE ��P�|����       TO ��q�����v  
004420         MOVE ��P�|��p�z     TO ��q��p�z�v
004430         MOVE ��P�|�����z     TO ��q�����z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE ��q���ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004400     WHEN 13
004410         MOVE ��P�|����       TO ���������v
004420         MOVE ��P�|��p�z     TO ������p�z�v
004430         MOVE ��P�|�����z     TO ���������z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE �������ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004400     WHEN 14
004410         MOVE ��P�|����       TO ���̑������v  
004420         MOVE ��P�|��p�z     TO ���̑���p�z�v
004430         MOVE ��P�|�����z     TO ���̑������z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE ���̑����ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004480     WHEN 21
004490         MOVE ��P�|����       TO �J�Ќ����v  
004500         MOVE ��P�|��p�z     TO �J�Д�p�z�v
004510         MOVE ��P�|�����z     TO �J�А����z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE �J�Е��ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004480     WHEN 22
004490         MOVE ��P�|����       TO ���������v  
004500         MOVE ��P�|��p�z     TO ������p�z�v
004510         MOVE ��P�|�����z     TO ���������z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE �������ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004480     WHEN 23
004490         MOVE ��P�|����       TO ���ی����v  
004500         MOVE ��P�|��p�z     TO ���۔�p�z�v
004510         MOVE ��P�|�����z     TO ���ې����z�v
               IF ��P�|���� NOT = ZERO
                   COMPUTE ���ە��ϐ����z�v = ��P�|�����z / ��P�|����
               END-IF
004520     END-EVALUATE.
004530*================================================================*
004540 ������R�[�h�Z�b�g SECTION.
004550*
004560     MOVE SPACE TO NJY582P.
004570*     INITIALIZE NJY582P.
004580     MOVE �����a��̂v       TO �����a��.
004590     MOVE �����N�v             TO �����N.
004600     MOVE �������v             TO ������.
004610*     MOVE �{�p���R�[�h�v       TO �{�p�@�֔ԍ�.
004620*     MOVE �{�p���X�֔ԍ��P�v   TO �X�֔ԍ��P.
004630*     MOVE �{�p���X�֔ԍ��Q�v   TO �X�֔ԍ��Q.
004640*     MOVE �{�p���Z���v         TO �Z��.
004870     MOVE ���v�����v           TO ����.
004650     MOVE ����ԍ��v           TO ����ԍ�.
004650     MOVE �ڍ��@���v           TO �{�p����.
004650     MOVE ��\�Җ��v           TO ����.
004660     MOVE ���ی����v           TO ���ی���  .
004670     MOVE ���۔�p�z�v         TO ���۔�p�z.
004680     MOVE ���ې����z�v         TO ���ې����z.
004680     MOVE ���ە��ϐ����z�v     TO ���ە��ϐ����z.
004690     MOVE �Еی����v           TO �Еی���  .
004700     MOVE �Е۔�p�z�v         TO �Е۔�p�z.
004710     MOVE �Еې����z�v         TO �Еې����z.
004680     MOVE �Еە��ϐ����z�v     TO �Еە��ϐ����z.
004690     MOVE ���ٌ����v           TO ���ٌ���  .
004700     MOVE ���ٔ�p�z�v         TO ���ٔ�p�z.
004710     MOVE ���ِ����z�v         TO ���ِ����z.
004680     MOVE ���ٕ��ϐ����z�v     TO ���ٕ��ϐ����z.
004690     MOVE �D�������v           TO �D������  .
004700     MOVE �D����p�z�v         TO �D����p�z.
004710     MOVE �D�������z�v         TO �D�������z.
004680     MOVE �D�����ϐ����z�v     TO �D�����ϐ����z.
004750     MOVE �g�������v           TO �g������  .
004760     MOVE �g����p�z�v         TO �g����p�z.
004770     MOVE �g�������z�v         TO �g�������z.
004680     MOVE �g�����ϐ����z�v     TO �g�����ϐ����z.
004720     MOVE ���ό����v           TO ���ό���  .
004730     MOVE ���ϔ�p�z�v         TO ���ϔ�p�z.
004740     MOVE ���ϐ����z�v         TO ���ϐ����z.
004680     MOVE ���ϕ��ϐ����z�v     TO ���ϕ��ϐ����z.
004660     MOVE �ސE�����v           TO �ސE����  .
004670     MOVE �ސE��p�z�v         TO �ސE��p�z.
004680     MOVE �ސE�����z�v         TO �ސE�����z.
004680     MOVE �ސE���ϐ����z�v     TO �ސE���ϐ����z.
004810     MOVE �㍂�����v           TO �㍂����  .
004820     MOVE �㍂��p�z�v         TO �㍂��p�z.
004830     MOVE �㍂�����z�v         TO �㍂�����z.
004680     MOVE �㍂���ϐ����z�v     TO �㍂���ϐ����z.
004810     MOVE ���������v           TO ��������  .
004820     MOVE ������p�z�v         TO ������p�z.
004830     MOVE ���������z�v         TO ���������z.
004680     MOVE �������ϐ����z�v     TO �������ϐ����z.
004810     MOVE ��Q�����v           TO ��Q����  .
004820     MOVE ��Q��p�z�v         TO ��Q��p�z.
004830     MOVE ��Q�����z�v         TO ��Q�����z.
004680     MOVE ��Q���ϐ����z�v     TO ��Q���ϐ����z.
004810     MOVE ��q�����v           TO ��q����  .
004820     MOVE ��q��p�z�v         TO ��q��p�z.
004830     MOVE ��q�����z�v         TO ��q�����z.
004680     MOVE ��q���ϐ����z�v     TO ��q���ϐ����z.
004810     MOVE ���������v           TO ��������  .
004820     MOVE ������p�z�v         TO ������p�z.
004830     MOVE ���������z�v         TO ���������z.
004680     MOVE �������ϐ����z�v     TO �������ϐ����z.
004810     MOVE �픚�����v           TO �픚����  .
004820     MOVE �픚��p�z�v         TO �픚��p�z.
004830     MOVE �픚�����z�v         TO �픚�����z.
004680     MOVE �픚���ϐ����z�v     TO �픚���ϐ����z.
004810     MOVE ���̑������v         TO ���̑�����  .
004820     MOVE ���̑���p�z�v       TO ���̑���p�z.
004830     MOVE ���̑������z�v       TO ���̑������z.
004680     MOVE ���̑����ϐ����z�v   TO ���̑����ϐ����z.
004840     MOVE �J�Ќ����v           TO �J�Ќ���  .
004850     MOVE �J�Д�p�z�v         TO �J�Д�p�z.
004850     MOVE �J�А����z�v         TO �J�А����z.
004680     MOVE �J�Е��ϐ����z�v     TO �J�Е��ϐ����z.
004840*     MOVE ���������v           TO ��������  .
004850*     MOVE ������p�z�v         TO ������p�z.
004850*     MOVE ���������z�v         TO ���������z.
004680*     MOVE �������ϐ����z�v     TO �������ϐ����z.
004840*     MOVE ���ی����v           TO ���ی���  .
004850*     MOVE ���۔�p�z�v         TO ���۔�p�z.
004850*     MOVE ���ې����z�v         TO ���ې����z.
004680*     MOVE ���ە��ϐ����z�v     TO ���ە��ϐ����z.
004870     MOVE ���v�����v           TO ���v����.
004880     MOVE ���v��p�z�v         TO ���v��p�z.
004890     MOVE ���v�����z�v         TO ���v�����z.
           IF ���v�����v NOT = ZERO
               COMPUTE ���v���ϐ����z = ���v�����z�v / ���v�����v
           ELSE
               MOVE ZERO             TO ���v���ϐ����z
           END-IF.
004660     MOVE �����v�����v         TO �����v����  .
004880     MOVE �����v��p�z�v       TO �����v��p�z.
004680     MOVE �����v�����z�v       TO �����v�����z.
           IF �����v�����v NOT = ZERO
               COMPUTE �����v���ϐ����z = �����v�����z�v / �����v�����v
           ELSE
               MOVE ZERO             TO �����v���ϐ����z
           END-IF.
004880     MOVE �����v�����v         TO �����v����.
004880     MOVE �����v��p�z�v       TO �����v��p�z.
004890     MOVE �����v�����z�v       TO �����v�����z.
           IF �����v�����v NOT = ZERO
               COMPUTE �����v���ϐ����z = �����v�����z�v / �����v�����v
           ELSE
               MOVE ZERO             TO �����v���ϐ����z
           END-IF.
004900*================================================================*
004910 ������� SECTION.
004920*
006050     IF ( �I�[�v���t���O NOT = "YES" )
006060        MOVE "YES" TO �I�[�v���t���O
006070        OPEN I-O  ����t�@�C��
006080        PERFORM �G���[�����o
006090     END-IF.
004930     MOVE "NJY582P"  TO  ��`�̖��o.
004940     MOVE "SCREEN"   TO  ���ڌQ���o.
004950     MOVE SPACE      TO  ������ʂo.
004960     MOVE SPACE      TO  �g������o.
004970     WRITE NJY582P.
004980     PERFORM �G���[�����o.
004990*================================================================*
005000 �G���[�\�� SECTION.
005010*
005020     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C�����v UPON CONS.
005030     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
005040     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
005050     ACCEPT  �L�[���� FROM CONS.
005060*================================================================*
005070 �G���[�����o SECTION.
005080*
005090     IF �ʒm���o NOT = "00"
005100         DISPLAY NC"���[�G���["              UPON CONS
005110         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
005120         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
005130         DISPLAY NC"�g������o�F" �g������o UPON CONS
005140         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005150                                             UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
005160         ACCEPT  �L�[���� FROM CONS
005170         PERFORM �t�@�C����
005180         MOVE 99 TO PROGRAM-STATUS
005190         EXIT PROGRAM
005200     END-IF.
005210*================================================================*
005220 �G���[�\���q SECTION.
005230*
005240     DISPLAY NC"�t�@�C���Ǎ��G���[" �t�@�C�����v UPON CONS.
005250     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
005260     ACCEPT  �L�[���� FROM CONS.
005270     PERFORM �t�@�C����.
005280     EXIT PROGRAM.
005290*================================================================*
005300 �G���[�\�����̑� SECTION.
005310*
005320     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
005330     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005340                                                   UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
005350     ACCEPT  �L�[���� FROM CONS.
005360     PERFORM �t�@�C����.
005370     EXIT PROGRAM.
005380*================================================================*
005390******************************************************************
005400 END PROGRAM NJY582.
005410******************************************************************
