000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCK582.
000060 AUTHOR.                 �r�c �K�q
000070*
000080*----------------------------------------------------------------*
000090*         �����\����i�_+����޳�ޔŁj
000091* �����Ɠ��� (����- �ɂ���)
000092* �����N��Ver.
000100*         MED = YCK580 YCK582P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2012-11-29
000130 DATE-COMPILED.          2012-11-29
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
000320     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  ���|����敪
000360                             FILE STATUS              IS  ��ԃL�[
000370                             LOCK        MODE         IS  AUTOMATIC.
000450     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS MODE              IS  DYNAMIC
000480                             RECORD KEY               IS �{��|�{�p���ԍ�
000490                             FILE STATUS              IS  ��ԃL�[
000500                             LOCK        MODE         IS  AUTOMATIC.
000510     SELECT  ��ƃt�@�C���P  ASSIGN      TO           "C:\MAKISHISYS\YAWOBJ\TEMP\W5801L.DAT"
000520                             ORGANIZATION             IS  SEQUENTIAL
000530                             ACCESS                   IS  SEQUENTIAL
000540                             FILE        STATUS       IS  ��ԃL�[
000550                             LOCK        MODE         IS  AUTOMATIC.
000560     SELECT  ��ƃt�@�C���Q  ASSIGN      TO           "C:\MAKISHISYS\YAWOBJ\TEMP\W5802L.DAT"
000570                             ORGANIZATION             IS  SEQUENTIAL
000580                             ACCESS                   IS  SEQUENTIAL
000590                             FILE        STATUS       IS  ��ԃL�[
000600                             LOCK        MODE         IS  AUTOMATIC.
000640     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF001
000650                             SYMBOLIC    DESTINATION  IS "PRT"
000660                             FORMAT                   IS  ��`�̖��o
000670                             GROUP                    IS  ���ڌQ���o
000680                             PROCESSING  MODE         IS  ������ʂo
000690                             UNIT        CONTROL      IS  �g������o
000700                             FILE        STATUS       IS  �ʒm���o.
000710******************************************************************
000720*                      DATA DIVISION                             *
000730******************************************************************
000740 DATA                    DIVISION.
000750 FILE                    SECTION.
000970*                           �m�q�k��  �P�Q�W�n
000980 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000990     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001000*                           �m�q�k��  �Q�T�U�n
001010 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001020     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000850*                           �m�q�k��  �U�S�O�n
000860 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000870     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
000871*
000880*
000881 FD  ��ƃt�@�C���P RECORD  CONTAINS 250 CHARACTERS.
000882 01  ��P�|���R�[�h.
000883     03  ��P�|���R�[�h�f�[�^.
000884         05  ��P�|�����a��N��.
000885             07  ��P�|�����a��            PIC 9.
000886             07  ��P�|�����N              PIC 9(2).
000887             07  ��P�|������              PIC 9(2).
000888         05  ��P�|�{�p�a��N��.
000889             07  ��P�|�{�p�a��            PIC 9.
000890             07  ��P�|�{�p�N              PIC 9(2).
000891             07  ��P�|�{�p��              PIC 9(2).
000892         05  ��P�|�����敪                PIC 9.
000893         05  ��P�|�ی��Ҕԍ�              PIC X(10).
000894         05  ��P�|�L��                    PIC X(24).
000895         05  ��P�|�ԍ�                    PIC X(30).
000896         05  ��P�|��ی��҃J�i            PIC X(50).
000897         05  ��P�|���҃J�i                PIC X(50).
000898         05  ��P�|�{�l�Ƒ��敪            PIC 9.
000899         05  ��P�|��p�z                  PIC 9(7).
000900         05  ��P�|���S�z                  PIC 9(7).
000901         05  ��P�|�����z                  PIC 9(7).
000902         05  ��P�|���҃R�[�h.
000903             07 ��P�|���Ҕԍ�             PIC 9(6).
000904             07 ��P�|�}��                 PIC X(1).
000905         05  ��P�|�ی����                PIC 9(2).
000906         05  ��P�|�e�ی����              PIC 9(2).
000907         05  ��P�|�V�l���R�[�h�敪        PIC 9.
000908         05  ��P�|�������R�[�h�敪        PIC 9.
000909         05  FILLER                        PIC X(40).
000910* ���v�t�@�C��
000911 FD  ��ƃt�@�C���Q RECORD  CONTAINS 32 CHARACTERS.
000912 01  ��Q�|���R�[�h.
000913     03  ��Q�|���R�[�h�f�[�^.
000914         05  ��Q�|�������v                PIC 9(4).
000915         05  ��Q�|��p�z���v              PIC 9(8).
000916         05  ��Q�|�����z���v              PIC 9(8).
000917         05  FILLER                        PIC X(12).
001160*
001170 FD  ����t�@�C��.
001180     COPY YCK582P        OF  XMDLIB.
001190*----------------------------------------------------------------*
001200******************************************************************
001210*                WORKING-STORAGE SECTION                         *
001220******************************************************************
001230 WORKING-STORAGE         SECTION.
001240 01 �L�[����                           PIC X    VALUE SPACE.
001250 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
001260 01 �I���t���O                         PIC X(3) VALUE SPACE.
001270 01 �I���s�t���O                       PIC X(3) VALUE SPACE.
001280 01 �t�@�C����                         PIC N(10).
001281 01 �ΏۂȂ��t���O                     PIC X(3) VALUE SPACE.
001210 01 �I�[�v���t���O                     PIC X(3)   VALUE SPACE.
001282*
001331 01 �����敪�o�v                       PIC 9     VALUE ZERO.
001340 01 �ی���ʂo�v                       PIC 9(2)  VALUE ZERO.
001350 01 �����a��N���o�v.
001360    03 �����a��o�v                    PIC 9    VALUE ZERO.
001370    03 �����N�o�v                      PIC 9(2) VALUE ZERO.
001380    03 �������o�v                      PIC 9(2) VALUE ZERO.
001381*
001390 01 �ی����̂v                         PIC N(3) VALUE SPACE.
001460 01 ���l�v                             PIC X(20).
001470 01 �O�a��v                           PIC 9    VALUE ZERO.
001480 01 �s�J�E���^                         PIC 9(2) VALUE 0.
001490 01 �ŃJ�E���^                         PIC 9(4) VALUE 0.
001500 01 �ő�s��                           PIC 9(2) VALUE 0.
001510 01 �w�b�_�s��                         PIC 9(2) VALUE 0.
001520 01 �ړ��s���v                         PIC 9(2) VALUE 0.
001530 01 �����ړ��L�[                       PIC X(4) VALUE SPACE.
001540 01 �J�����g�����v                     PIC 9(1) VALUE ZERO.
001541*
001542 01 ����ԍ��v.
001543    03 ����                            PIC N(2) VALUE SPACE.
001544    03 �n�C�t��                        PIC X(1) VALUE "-".
001545    03 ����ԍ��v�o                    PIC X(10) VALUE SPACE.
001546*
001570 01 �ی���ʖ��̂v.
001580    03 �ی���ʖ��̂v�o                PIC X(6) VALUE SPACE.
001590 01 �e�ی���ʖ��̂v.
001600    03 �e�ی���ʖ��̂v�o              PIC X(4) VALUE SPACE.
001610    03 ���v����                        PIC X(4).
001620 01 �����v�v.
001630    03 �����v�v�o                      PIC X(6).
001631*
001632** ���������p
001640 01 ��ی��҃J�i�v.
001650    03 ��ی��҃J�i�v�o                PIC X    OCCURS 50 VALUE SPACE.
001660 01 �����J�i�v.
001670    03 �����J�i�v�o                    PIC X    OCCURS 50 VALUE SPACE.
001680 01 ���O�J�i�v.
001690    03 ���O�J�i�v�o                    PIC X    OCCURS 50 VALUE SPACE.
001700 01 �J�E���^                           PIC 9(2) VALUE ZERO.
001710 01 �l�J�E���^                         PIC 9(2) VALUE ZERO.
001720 01 �m�J�E���^                         PIC 9(2) VALUE ZERO.
001730 01 �����t���O                         PIC X(3) VALUE SPACE.
001731**
001740 01 ���v�v.
001750    03 ���v�����v                      PIC 9(4) VALUE ZERO.
001760    03 ��p�z���v�v                    PIC 9(7) VALUE ZERO.
001770    03 ���S�z���v�v                    PIC 9(7) VALUE ZERO.
001780    03 �����z���v�v                    PIC 9(7) VALUE ZERO.
001790 01 ���v�v.
001800    03 ���v�����v                      PIC 9(4) VALUE ZERO.
001810    03 ��p�z���v�v                    PIC 9(8) VALUE ZERO.
001820    03 ���S�z���v�v                    PIC 9(8) VALUE ZERO.
001830    03 �����z���v�v                    PIC 9(8) VALUE ZERO.
001831*
001840 01 �{�p�a��N���v.
001850     03 �{�p�a��v                   PIC 9.
001860     03 �{�p�N���v.
001870        05 �{�p�N�v                  PIC 9(2).
001880        05 �N�v                      PIC N(1).
001890        05 �{�p���v                  PIC 9(2).
001900        05 ���v                      PIC N(1).
001910 01 ���ݘa��N���v.
001920    03 ���ݘa��v                    PIC 9.
001930    03 ���ݔN�v                      PIC 9(2).
001940    03 ���݌��v                      PIC 9(2).
001941*
001942 01 �ی���ʂv�q                     PIC 9(2) VALUE ZERO.
001943 01 ����`���v�q                     PIC 9    VALUE ZERO.
001950*******************************************************************
001960 01 �������.
001970     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
001980     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
001990     03 ������ʂo                     PIC X(2) VALUE SPACE.
002000     03 �g������o.
002010         05 �[������o.
002020             07 �ړ������o             PIC X(1).
002030             07 �ړ��s���o             PIC 9(3).
002040         05 �ڍא���o                 PIC X(2).
002050     03 �ʒm���o                     PIC X(2) VALUE SPACE.
002060     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
002070*
002080 01 �v�Z�@����N�v                     PIC 9(2).
002090* ���t�v�n�q�j
002100 01 �a��I���N�v                       PIC 9(4).
001500 01 �v�Z�@�a��N�v                     PIC 9(2).
002110 01 �v�Z�@����.
002120    03 �v�Z�@����N                    PIC 9(4).
002130    03 �v�Z�@�����                  PIC 9(4).
002140 01 �v�Z�@����q REDEFINES �v�Z�@����.
002150    03 �v�Z�@���I                      PIC 9(2).
002160    03 �v�Z�@���t                      PIC 9(6).
002170    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
002180       05 �v�Z�@�N��                   PIC 9(4).
002190       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
002200         07 �v�Z�@�N                   PIC 9(2).
002210         07 �v�Z�@��                   PIC 9(2).
002220       05 �v�Z�@��                     PIC 9(2).
002230******************************************************************
002240*                          �A������                              *
002250******************************************************************
002260*
      ****************************
      * �������t�`�F�b�N�p���[�N *
      *****************************
       01 �A���b�g�j�|�L�[ IS EXTERNAL.
      */ in
          03 �A���b�g�j�|�a��N����.
             05 �A���b�g�j�|�a��N��.
               07 �A���b�g�j�|�a��           PIC 9.
               07 �A���b�g�j�|�N             PIC 9(2).
               07 �A���b�g�j�|��             PIC 9(2).
             05 �A���b�g�j�|��               PIC 9(2).
      *   / �m�F���b�Z�[�W���g�p����ꍇ�F"YES" /
          03 �A���b�g�j�|�m�F�t���O          PIC X(3).
      */ out
      *   / �N���P�ʂ̔N�������쐬 /
          03 �A���b�g�j�|�����a��N����.
             05 �A���b�g�j�|�����a��N��.
               07 �A���b�g�j�|�����a��       PIC 9.
               07 �A���b�g�j�|�����N         PIC 9(2).
               07 �A���b�g�j�|������         PIC 9(2).
            05 �A���b�g�j�|������            PIC 9(2).
      *   / ���݂��Ȃ��ꍇ�G���[�F"YES" /
         03 �A���b�g�j�|�G���[�t���O         PIC X(3).
      *
      *   / �������t�̔N�������쐬�i�������ύX����Ă��Ȃ��ꍇ�͏�L�N�����Ɠ����j /
         03 �A���b�g�j�|�����a��N�����Q.
            05 �A���b�g�j�|�����a��N���Q.
              07 �A���b�g�j�|�����a��Q     PIC 9.
              07 �A���b�g�j�|�����N�Q       PIC 9(2).
              07 �A���b�g�j�|�������Q       PIC 9(2).
            05 �A���b�g�j�|�������Q          PIC 9(2).
      *   / ���݂��Ȃ��ꍇ�G���[�F"YES" /
         03 �A���b�g�j�|�G���[�t���O�Q       PIC X(3).
      *
002494 01 �A���|�L�[ IS EXTERNAL.
002495    03  �A���|���b�Z�[�W                 PIC N(20).
002260*
002287 01 �A���|��ʏ��x�b�j�T�W�O IS EXTERNAL.
002297    03 �A���|����`��                  PIC 9.
002307    03 �A���|�����a��N��.
002317       05 �A���|�����a��               PIC 9.
002327       05 �A���|�����N��.
002337         07 �A���|�����N               PIC 9(2).
002347         07 �A���|������               PIC 9(2).
          03 �A���|�v���r���[�敪            PIC 9.
002560*
000540************************************
000550* �v�����^�t�@�C���쐬�p           *
000560************************************
000570 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000580     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
000590     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
000600     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
000610     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
002269*
002270******************************************************************
002280*                      PROCEDURE  DIVISION                       *
002290******************************************************************
002300 PROCEDURE               DIVISION.
002310************
002320*           *
002330* ��������   *
002340*           *
002350************
002560     MOVE SPACE TO �I�[�v���t���O.
002570     PERFORM �v�����^�t�@�C���쐬.
002360     PERFORM ������.
002361     MOVE SPACE  TO YCK582P.
002362***     INITIALIZE YCK582P.
002370************
002380*           *
002390* �又��     *
002400*           *
002410************
002411*
002412     EVALUATE TRUE
002414     WHEN  ����`���v�q = ZERO 
002415          PERFORM �������
002425     WHEN  ����`���v�q = 1 OR 2 
002427          PERFORM �ꕔ�������
002429     WHEN OTHER
002430          CONTINUE
002431     END-EVALUATE.
002432*
002433************
002440*           *
002450* �I������   *
002460*           *
002470************
002480     PERFORM �I������.
002490     EXIT PROGRAM.
002500*
002510*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCK582"              TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
002520*================================================================*
002530 ������ SECTION.
002540*
002550     PERFORM �t�@�C���I�[�v��.
002560*    /* ���ݓ��t�擾 */
002570     ACCEPT �v�Z�@���t FROM DATE.
002580*    /* 1980�`2079�N�̊ԂŐݒ� */
002590     IF �v�Z�@�N > 80
002600         MOVE 19 TO �v�Z�@���I
002610     ELSE
002620         MOVE 20 TO �v�Z�@���I
002630     END-IF.
002970     PERFORM �J�����g�����擾.
002980     PERFORM �a��I���N�擾.
003610     COMPUTE �v�Z�@�a��N�v = �v�Z�@����N - �a��I���N�v + 1.
           INITIALIZE �A���b�g�j�|�L�[.
           MOVE �J�����g�����v   TO �A���b�g�j�|�a��.
           MOVE �v�Z�@�a��N�v   TO �A���b�g�j�|�N.
           MOVE �v�Z�@��         TO �A���b�g�j�|��.
           MOVE ZERO             TO �A���b�g�j�|��.
           CALL "GENGOCHK".
           CANCEL "GENGOCHK".
002701*
002702* �A�����ڂ̑Ҕ�
002704     MOVE �A���|����`��    TO ����`���v�q.
003070*
003000*================================================================*
003010 �J�����g�����擾 SECTION.
003020*
003030     MOVE ZEROS TO ���|����敪.
003040     READ ������}�X�^
003050     NOT INVALID KEY
003060         MOVE ���|�J�����g���� TO �J�����g�����v
003070     END-READ.
003080*
003090*================================================================*
003100 �a��I���N�擾 SECTION.
003110*
003120*     DISPLAY NC"�J�����g�����v"  �J�����g�����v UPON MSGBOX.
003130     MOVE �J�����g�����v TO ���|�����敪.
003140     READ �����}�X�^
003150     INVALID KEY
003160         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
003170         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003180                                                  UPON CONS
002441*-----------------------------------------*
002442         CALL "actcshm"  WITH C LINKAGE
002443*-----------------------------------------*
003190         ACCEPT  �L�[���� FROM CONS
003200         PERFORM �I������
003210         EXIT PROGRAM
003220     NOT INVALID KEY
               IF ( ���|�����J�n����N = ZERO OR SPACE )
005500             COMPUTE �O�a��v = �J�����g�����v - 1
005510             MOVE �O�a��v TO ���|�����敪
005520             READ �����}�X�^
005530             INVALID KEY
005540                 DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
005550                 DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005560                                                          UPON CONS
005570                 ACCEPT  �L�[���� FROM CONS
005580                 PERFORM �I������
005590                 EXIT PROGRAM
005600             NOT INVALID KEY
                       IF ( ���|�I������N = ZERO OR SPACE )
                           CONTINUE
                       ELSE
                           COMPUTE �a��I���N�v = ���|�I������N + 1
                       END-IF
                   END-READ
               ELSE
                   MOVE ���|�����J�n����N TO �a��I���N�v
               END-IF
003360     END-READ.
003370*
003080*================================================================*
003090 �t�@�C���I�[�v�� SECTION.
003100*
003410     OPEN INPUT   �����}�X�^
003420         MOVE NC"����" TO �t�@�C����.
003430         PERFORM �I�[�v���`�F�b�N.
003440     OPEN INPUT   ������}�X�^
003450         MOVE NC"������" TO �t�@�C����.
003460         PERFORM �I�[�v���`�F�b�N.
003200     OPEN INPUT �{�p�����}�X�^
003210         MOVE NC"�{�p�����" TO �t�@�C����.
003220         PERFORM �I�[�v���`�F�b�N.
003230     OPEN INPUT ��ƃt�@�C���P
003240         MOVE NC"��ƂP" TO �t�@�C����.
003250         PERFORM �I�[�v���`�F�b�N.
003251     OPEN INPUT ��ƃt�@�C���Q
003252         MOVE NC"��ƂQ" TO �t�@�C����.
003253         PERFORM �I�[�v���`�F�b�N.
003280*================================================================*
003290 �I�[�v���`�F�b�N SECTION.
003300*
003310     IF ��ԃL�[  NOT =  "00"
003320         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
003330         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
003340         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003350                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
003360         ACCEPT  �L�[���� FROM CONS
003370         PERFORM �t�@�C����
003380         EXIT PROGRAM.
003390*================================================================*
003400 �t�@�C���� SECTION.
003410*
003570     IF ( �I�[�v���t���O = "YES" )
003580         CLOSE ����t�@�C��
003590     ELSE
003600         MOVE  NC"�@�@�f�[�^���O���ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
003610         CALL   "MSG001"
003620         CANCEL "MSG001"
003630     END-IF.
003640*
003420     CLOSE �����}�X�^ ������}�X�^ �{�p�����}�X�^ ��ƃt�@�C���P  ��ƃt�@�C���Q.
003440*================================================================*
003450 �I������ SECTION.
003460*
003470     PERFORM �t�@�C����.
003480*================================================================*
003490 �G���[�\�� SECTION.
003500*
003510     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
003520     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
003530     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
003540     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
003550     ACCEPT  �L�[���� FROM CONS.
003560     PERFORM �t�@�C����.
003570     EXIT PROGRAM.
003580*================================================================*
003590 ������� SECTION.
003600*
003602     MOVE SPACE TO �I���t���O.
003603     INITIALIZE ���v�v.
003604     INITIALIZE ���v�v.
003605     MOVE 20    TO �ő�s��.
003606     MOVE 1     TO �ŃJ�E���^.
003607     PERFORM ��ƃt�@�C���P�Ǎ�.
003608     PERFORM UNTIL �I���t���O = "YES"
003609*
003610         MOVE ��P�|�����a��     TO �����a��o�v
003611         MOVE ��P�|�����N       TO �����N�o�v
003612         MOVE ��P�|������       TO �������o�v
003613         MOVE ��P�|�����敪     TO �����敪�o�v
003620         PERFORM �w�b�_�Z�b�g
003621*
003622         PERFORM UNTIL ( �I���t���O = "YES" ) OR ( �s�J�E���^ > �ő�s�� )  OR
003623                       ( ��P�|�����a��   NOT = �����a��o�v ) OR
003624                       ( ��P�|�����N     NOT = �����N�o�v   ) OR
003625                       ( ��P�|������     NOT = �������o�v   ) OR
003626                       ( ��P�|�����敪   NOT = �����敪�o�v ) 
003627             PERFORM ���ו��Z�b�g
003628             PERFORM ���v�z�݌v
003629             PERFORM ��ƃt�@�C���P�Ǎ�
003631         END-PERFORM
003632*
003633         IF ( �s�J�E���^ > �ő�s�� )
003634             IF  ( �I���t���O = "YES" ) OR 
003635                 ( ��P�|�����a��   NOT = �����a��o�v ) OR
003636                 ( ��P�|�����N     NOT = �����N�o�v   ) OR
003637                 ( ��P�|������     NOT = �������o�v   ) OR
003638                 ( ��P�|�����敪   NOT = �����敪�o�v ) 
003639                 PERFORM �󎚏���
003640                 PERFORM ���ŏ���
003645                 IF  �I���t���O = "YES" 
003646                     PERFORM �w�b�_�Z�b�g
003647                     PERFORM �����v�Z�b�g
003648                     PERFORM �󎚏���
003649                     PERFORM ���ŏ���
003650                 END-IF
003651             ELSE
003652                 PERFORM �󎚏���
003653                 PERFORM ���ŏ���
003654             END-IF
003655         ELSE
003658             IF  �I���t���O = "YES" 
003660                 IF �s�J�E���^ > �ő�s��
003661                    PERFORM �󎚏���
003662                    PERFORM ���ŏ���
003663                    PERFORM �w�b�_�Z�b�g
003664                 END-IF
003665                 PERFORM �����v�Z�b�g
003666             END-IF
003667             PERFORM �󎚏���
003668             PERFORM ���ŏ���
003669         END-IF
003670*
003671     END-PERFORM.
003672*
003673*================================================================*
003674 �ꕔ������� SECTION.
003675*
003676     MOVE SPACE TO �I���t���O.
003677     INITIALIZE ���v�v.
003678     INITIALIZE ���v�v.
003679     MOVE 20    TO �ő�s��.
003680     MOVE 1     TO �ŃJ�E���^.
003681     PERFORM ��ƃt�@�C���P�Ǎ�.
003682     IF  �I���t���O = "YES"
003683         MOVE "YES"  TO �ΏۂȂ��t���O
003684     END-IF.
003685     PERFORM UNTIL �I���t���O = "YES"
003686*
003687         MOVE ��P�|�����a��     TO �����a��o�v
003688         MOVE ��P�|�����N       TO �����N�o�v
003689         MOVE ��P�|������       TO �������o�v
003690         MOVE ��P�|�����敪     TO �����敪�o�v
003691         PERFORM �w�b�_�Z�b�g
003692*
003693         PERFORM UNTIL ( �I���t���O = "YES" ) OR ( �s�J�E���^ > �ő�s�� )  OR
003694                       ( ��P�|�����a��   NOT = �����a��o�v ) OR
003695                       ( ��P�|�����N     NOT = �����N�o�v   ) OR
003696                       ( ��P�|������     NOT = �������o�v   ) OR
003697                       ( ��P�|�����敪   NOT = �����敪�o�v ) 
003698             PERFORM ���ו��Z�b�g
003699             PERFORM ���v�z�݌v
003700             PERFORM ��ƃt�@�C���P�Ǎ�
003701         END-PERFORM
003702*
003734         PERFORM �󎚏���
003735         PERFORM ���ŏ���
003738*
003739     END-PERFORM.
003740*
003741* ���v�y�[�W�݈̂��
003742     IF �ΏۂȂ��t���O NOT = "YES"
003743         PERFORM �w�b�_�Z�b�g
003744         MOVE ZERO  TO ��
003750         PERFORM ���v�y�[�W�̂݃Z�b�g
003751         PERFORM �󎚏���
003752         PERFORM ���ŏ���
003753     END-IF.
003760*
004330*================================================================*
004340 ��ƃt�@�C���P�Ǎ� SECTION.
004350*
004360     READ ��ƃt�@�C���P NEXT
004370     AT END
004380         MOVE "YES" TO �I���t���O
004390     END-READ.
004400*
004401*================================================================*
004402 �󎚏���  SECTION.
004403*
006050     IF ( �I�[�v���t���O NOT = "YES" )
006060        MOVE "YES" TO �I�[�v���t���O
006070        OPEN I-O  ����t�@�C��
006080        PERFORM �G���[�����o
006090     END-IF.
004404     MOVE "YCK582P" TO  ��`�̖��o.
004405     MOVE SPACE     TO  ������ʂo.
004406     MOVE "SCREEN"  TO  ���ڌQ���o.
004408     WRITE YCK582P.
004409     PERFORM �G���[�����o.
004410     MOVE SPACE TO YCK582P.
004413*================================================================*
004414 ���ŏ���  SECTION.
004415
004416     MOVE "YCK582P" TO  ��`�̖��o.
004417     MOVE "CT"      TO  ������ʂo.
004418     MOVE "PAGE"    TO  �g������o.
004419     MOVE SPACE     TO  ���ڌQ���o.
004421     WRITE YCK582P.
004422     PERFORM �G���[�����o.
004423     MOVE SPACE     TO  �g������o.
004424*
004425     COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1.
004426*
004650*================================================================*
004710 �w�b�_�Z�b�g SECTION.
004720*
      */�����C��/������20190514
037370     IF �A���b�g�j�|�����a�� > 4
              MOVE �A���b�g�j�|�����a�� TO ���|�����敪
037380        READ �����}�X�^
037390        NOT INVALID KEY
037400            MOVE ���|��������   TO �쐬�a��
037410        END-READ
              MOVE "===="             TO �쐬�a�����
           END-IF.
      */�����C��/������20190514
004760     MOVE �A���b�g�j�|�����N TO �쐬�N.
004770     MOVE �A���b�g�j�|������ TO �쐬��.
004780     MOVE �v�Z�@��           TO �쐬��.
004790     MOVE �ŃJ�E���^         TO ��.
004800*
004810* ����ԍ����擾
004820     MOVE 00         TO �{��|�{�p���ԍ�.
004830     READ �{�p�����}�X�^
004840     INVALID KEY
004850         MOVE SPACE       TO ����ԍ�
004860         MOVE SPACE       TO ��\�Җ�
004870     NOT INVALID KEY
004881         MOVE NC"����"                TO ����
004890         MOVE �{��|�ڍ��t�����ԍ�  TO ����ԍ��v�o
004900         MOVE ����ԍ��v              TO ����ԍ�
004910         MOVE �{��|��\�Җ�          TO ��\�Җ�
004920     END-READ.
004930*
004940     MOVE 1    TO �s�J�E���^.
004950*
005080*================================================================*
005090 ���ו��Z�b�g SECTION.
005100*
005140     MOVE ��P�|�{�p�N        TO �{�p�N�v.
005150     MOVE NC"�N"              TO �N�v.
005160     MOVE ��P�|�{�p��        TO �{�p���v.
005170     MOVE NC"��"              TO ���v.
005180     MOVE �{�p�N���v          TO �{�p�N��(�s�J�E���^).
005181*
005182     MOVE ��P�|�ی��Ҕԍ�    TO �ی��Ҕԍ�(�s�J�E���^).
005190     IF   ��P�|�������R�[�h�敪 = 1 
005200          IF ��P�|�ی��Ҕԍ�(1:2) = "99"
005201             MOVE SPACE       TO �ی��Ҕԍ�(�s�J�E���^)
005202          END-IF
005203     END-IF.
005231*
005232     IF ��P�|�L��(1:2) = "��" 
005233        MOVE  SPACE        TO  �L��(�s�J�E���^)
005234     ELSE
005235        MOVE ��P�|�L��    TO  �L��(�s�J�E���^)
005236     END-IF.
005237     IF ( ��P�|�ԍ�(1:1) = "*"  ) OR
005238        ( ��P�|�ԍ�(1:2) = "��" )
005239        MOVE  SPACE        TO  �ԍ�(�s�J�E���^)
005240     ELSE
005241        MOVE ��P�|�ԍ�    TO  �ԍ�(�s�J�E���^)
005242     END-IF.
005243*
005244*** �V�l�Ə������R�[�h�́A���҃J�i�ŁA�{�l�Ƒ��敪 = 1 �ƂȂ�. ***
005245     IF ( ��P�|�V�l���R�[�h�敪 = 1 )  OR
005246        ( ��P�|�������R�[�h�敪 = 1 ) 
005247        MOVE ��P�|���҃J�i      TO ��ی��҃J�i�v
005248        MOVE 1                   TO �{�l�Ƒ�(�s�J�E���^)
005249     ELSE
005250        MOVE ��P�|��ی��҃J�i  TO ��ی��҃J�i�v
005251        MOVE ��P�|�{�l�Ƒ��敪  TO �{�l�Ƒ�(�s�J�E���^)
005252     END-IF.
005253**
005254     PERFORM ��������.
005260     MOVE �����J�i�v         TO ��ی��Ҏ��J�i(�s�J�E���^).
005270     MOVE ���O�J�i�v         TO ��ی��Җ��J�i(�s�J�E���^).
005290     MOVE ��P�|��p�z       TO ���v�z(�s�J�E���^).
005300     MOVE ��P�|�����z       TO �����z(�s�J�E���^).
005310*
005320     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
005330*
005430*================================================================*
005610 ��������  SECTION.
005611*
005612**** �����Ɩ��O�𕪗�����B
005613*
005620     MOVE 1          TO �J�E���^ �l�J�E���^ �m�J�E���^.
005630     MOVE SPACE      TO �����J�i�v ���O�J�i�v �����t���O.
005876*
005878     PERFORM VARYING �J�E���^ FROM 1 BY 1 
005879                     UNTIL ( �J�E���^ > 50 ) OR ( �����t���O = "YES" )
005880          IF ��ی��҃J�i�v�o(�J�E���^)  = SPACE
005884             MOVE "YES" TO  �����t���O
005885          ELSE
005886             MOVE ��ی��҃J�i�v�o(�J�E���^) TO �����J�i�v�o(�l�J�E���^)
005887             COMPUTE �l�J�E���^ = �l�J�E���^ + 1
005888          END-IF
005890     END-PERFORM.
005891*
005892     PERFORM  UNTIL �J�E���^ > 20 
005894          IF ��ی��҃J�i�v�o(�J�E���^)  = SPACE
005895             CONTINUE
005896          ELSE
005898             MOVE ��ی��҃J�i�v�o(�J�E���^) TO ���O�J�i�v�o(�m�J�E���^)
005900             COMPUTE �m�J�E���^ = �m�J�E���^ + 1
005901          END-IF
005902          COMPUTE �J�E���^ = �J�E���^ + 1
005903     END-PERFORM.
005904*
005906*================================================================*
005907 ���v�z�݌v SECTION.
005908*
005914     COMPUTE ���v�����v     = ���v�����v      + 1.
005915     COMPUTE ��p�z���v�v   = ��p�z���v�v    + ��P�|��p�z.
005916     COMPUTE ���S�z���v�v   = ���S�z���v�v    + ��P�|���S�z.
005917     COMPUTE �����z���v�v   = �����z���v�v    + ��P�|�����z.
005918*================================================================*
006450*================================================================*
006460 �����v�Z�b�g SECTION.
006470*
006500     MOVE "�����v"     TO  �����v�v�o.
006510     MOVE �����v�v     TO  ��ی��Ҏ��J�i(�s�J�E���^).
006520     MOVE ���v�����v   TO  �{�l�Ƒ�(�s�J�E���^).
006530     MOVE ��p�z���v�v TO  ���v�z(�s�J�E���^).
006540     MOVE �����z���v�v TO  �����z(�s�J�E���^).
006541*================================================================*
006542 ���v�y�[�W�̂݃Z�b�g SECTION.
006543*
006544     READ ��ƃt�@�C���Q NEXT
006545     NOT AT END
006549         MOVE "�����v"         TO  �����v�v�o
006550         MOVE �����v�v         TO  ��ی��Ҏ��J�i(1)
006551         MOVE ��Q�|�������v   TO  �{�l�Ƒ�(1)
006552         MOVE ��Q�|��p�z���v TO  ���v�z(1)
006553         MOVE ��Q�|�����z���v TO  �����z(1)
006557     END-READ.
006558*================================================================*
006660 �G���[�����o SECTION.
006670*
006680     IF �ʒm���o NOT = "00"
006690         DISPLAY NC"���[�G���["              UPON CONS
006700         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
006710         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
006720         DISPLAY NC"�g������o�F" �g������o UPON CONS
006730         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
006740                                             UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
006750         ACCEPT  �L�[���� FROM CONS
006760         PERFORM �t�@�C����
006770         EXIT PROGRAM
006780     END-IF.
006790*================================================================*
006800******************************************************************
006810 END PROGRAM YCK582.
006820******************************************************************
