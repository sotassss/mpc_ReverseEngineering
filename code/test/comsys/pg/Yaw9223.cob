000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAW9223.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*         ��f�҃��x��������X�g
000100*         MED = YAW922 YAW9223P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2009-08-20
000130 DATE-COMPILED.          2009-08-20
      *
      */2014.07.14 ��f�҃J�i������ǉ�
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
000380     SELECT  �������e�e      ASSIGN      TO        KENSAKUL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS  �����|����敪
000420                             FILE STATUS              IS  ��ԃL�[
000430                             LOCK        MODE         IS  AUTOMATIC.
000440     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\HMOBJ\TEMP\H9221L.DAT"
000450                             ORGANIZATION             IS  INDEXED
000460                             ACCESS                   IS  DYNAMIC
000470                             RECORD      KEY          IS  ��P�|�{�p�a��N���m
000480                                                          ��P�|���҃R�[�h
000490                             ALTERNATE RECORD KEY     IS  ��P�|����敪
000500                                                          ��P�|�{�p�a��N���m
000510                                                          ��P�|���҃R�[�h
000520*/                                                       */���я��̕ύX/0404
000530                             ALTERNATE RECORD KEY     IS  ��P�|���җX�֔ԍ�
000540                                                          ��P�|���҃J�i
000550                                                          ��P�|���҃R�[�h
000560                                                          ��P�|�{�p�a��N���m
000570                             ALTERNATE RECORD KEY     IS  ��P�|����敪
000580                                                          ��P�|���җX�֔ԍ�
000590                                                          ��P�|���҃J�i
000600                                                          ��P�|���҃R�[�h
000610                                                          ��P�|�{�p�a��N���m
000930                             ALTERNATE RECORD KEY     IS  ��P�|����敪
000400                                                          ��P�|���҃J�i
000950                                                          ��P�|���҃R�[�h
000960                                                          ��P�|�{�p�a��N���m
000620                             FILE        STATUS       IS  ��ԃL�[
000630                             LOCK        MODE         IS  AUTOMATIC.
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
000760*                           �m�q�k��  �P�Q�W�n
000770 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000780     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000790*                           �m�q�k��  �Q�T�U�n
000800 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
000810     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000820
000830 FD  �������e�e          BLOCK   CONTAINS   1   RECORDS.
000840     COPY KENSAKU         OF  XFDLIB  JOINING   ����   AS  PREFIX.
000850* 
000860 FD  ��ƃt�@�C���P RECORD  CONTAINS 320 CHARACTERS.
000870 01  ��P�|���R�[�h.
000880     03  ��P�|���R�[�h�L�[.
000890       05 ��P�|�{�p�a��N���m.
000900         07 ��P�|�{�p�a��m                 PIC 9.
000910         07 ��P�|�{�p�N���m.
000920           09 ��P�|�{�p�N�m                 PIC 9(2).
000930           09 ��P�|�{�p���m                 PIC 9(2).
000940       05 ��P�|���҃R�[�h.
000950         07 ��P�|���Ҕԍ�                   PIC 9(6).
000960         07 ��P�|�}��                       PIC X.
000970     03  ��P�|���R�[�h�f�[�^.
000980       05 ��P�|���҃J�i                     PIC X(50).
000990       05 ��P�|���Ҏ���                     PIC X(50).
001000       05 ��P�|���җX�֔ԍ�.
001010         07 ��P�|���җX�֔ԍ��P             PIC X(3).
001020         07 ��P�|���җX�֔ԍ��Q             PIC X(4).
001030       05 ��P�|���ҏZ���P                   PIC X(50).
001040       05 ��P�|���ҏZ���Q                   PIC X(50).
001050       05 ��P�|���Ґ��N����.
001060         07 ��P�|���Ҙa��                   PIC 9.
001070         07 ��P�|���ҔN                     PIC 9(2).
001080         07 ��P�|���Ҍ�                     PIC 9(2).
001090         07 ��P�|���ғ�                     PIC 9(2).
001100       05 ��P�|���ғd�b�ԍ�                 PIC X(30).
001110       05 ��P�|���Ґ���                     PIC 9.
001120       05 ��P�|����敪                     PIC 9.
001130       05 ��P�|�C���f�b�N�X                 PIC 9(7).
001140       05 FILLER                             PIC X(55).
001150*
001160 FD  ����t�@�C��.
001170     COPY YAW9223P         OF  XMDLIB.
001180*----------------------------------------------------------------*
001190******************************************************************
001200*                WORKING-STORAGE SECTION                         *
001210******************************************************************
001220 WORKING-STORAGE         SECTION.
001230 01 �L�[����                           PIC X    VALUE SPACE.
001240 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
001250 01 �I���t���O                         PIC X(3) VALUE SPACE.
001260 01 �t�@�C����                         PIC N(10).
001270 01 �I�[�v���t���O                     PIC X(3) VALUE SPACE.
001280*
001290 01 �s�J�E���^                         PIC 9(2) VALUE 0.
001300 01 �ŃJ�E���^                         PIC 9(4) VALUE 0.
001310 01 �ő�s��                           PIC 9(2) VALUE 0.
001320 01 �w�b�_�s��                         PIC 9(2) VALUE 0.
001330*
001340 01 �A�Ԃv                             PIC 9(6) VALUE ZERO.
001350 01 ���v�����v                         PIC 9(4) VALUE ZERO.
001360 01 ���v�����v                         PIC 9(4) VALUE ZERO.
001370*
001380 01 ���ݘa��N���v.
001390    03 ���ݘa��v                      PIC 9(1).
001400    03 ���ݔN�v                        PIC 9(2).
001410    03 ���݌��v                        PIC 9(2).
001420 01 �O�a��v                           PIC 9 VALUE ZERO.
001430 01 �J�����g�����v                     PIC 9(1) VALUE ZERO.
001440*
001450 01 �a��̂v.
001460    03 �a��̂Q�v                    PIC N(2) VALUE SPACE.
001470 01 �{�p�a��N���v.
001480    03 �{�p�a��v                      PIC 9    VALUE ZERO.
001490    03 �{�p�N���v.
001500       05 �{�p�N�v                     PIC 9(2) VALUE ZERO.
001510       05 �{�p���v                     PIC 9(2) VALUE ZERO.
001520 01 �J�n�N��v                         PIC Z(3) VALUE ZERO.
001530 01 �I���N��v                         PIC Z(3) VALUE ZERO.
001540*
001550 01 ���o�����R�����g�v                 PIC X(100) VALUE SPACE.
001560 01 ���o�����R�����g�v�Q               PIC X(100) VALUE SPACE.
001570 01 ���o�����R�����g�v�R               PIC X(100) VALUE SPACE.
001580 01 ���o�����R�����g�v�S               PIC X(100) VALUE SPACE.
001590 01 ���o�����R�����g�v�T               PIC X(100) VALUE SPACE.
001600 01 ���o�����R�����g�v�U               PIC X(100) VALUE SPACE.
001610 01 ���o�����R�����g�v�V               PIC X(100) VALUE SPACE.
001620 01 ���o�����R�����g�v�W               PIC X(100) VALUE SPACE.
001630*
001640 01 �a�����v                           PIC X(20)  VALUE SPACE.
001650 01 �����v                             PIC X(100) VALUE SPACE.
001660 01 �w��N���v                         PIC X(40)  VALUE SPACE.
001670 01 �a��v                             PIC X(4)   VALUE SPACE.
001680 01 �n���v                             PIC X(100) VALUE SPACE.
001690 01 �N��v                             PIC X(10)  VALUE SPACE.
001700*
001710 01 �������.
001720     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
001730     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
001740     03 ������ʂo                     PIC X(2) VALUE SPACE.
001750     03 �g������o.
001760         05 �[������o.
001770             07 �ړ������o             PIC X(1).
001780             07 �ړ��s���o             PIC 9(3).
001790         05 �ڍא���o                 PIC X(2).
001800     03 �ʒm���o                     PIC X(2) VALUE SPACE.
001810     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
001820*
001830 01 �v�Z�@����N�v                     PIC 9(2).
001840* ���t�v�n�q�j
001850 01 �a��I���N�v                       PIC 9(4).
001860 01 �v�Z�@����.
001870    03 �v�Z�@����N                    PIC 9(4).
001880    03 �v�Z�@�����                  PIC 9(4).
001890 01 �v�Z�@����q REDEFINES �v�Z�@����.
001900    03 �v�Z�@���I                      PIC 9(2).
001910    03 �v�Z�@���t                      PIC 9(6).
001920    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
001930       05 �v�Z�@�N��                   PIC 9(4).
001940       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
001950         07 �v�Z�@�N                   PIC 9(2).
001960         07 �v�Z�@��                   PIC 9(2).
001970       05 �v�Z�@��                     PIC 9(2).
001980******************************************************************
001990*                          �A������                              *
002000******************************************************************
002010************************************
002020* �v�����^�t�@�C���쐬�p           *
002030************************************
002040 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
002050     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
002060     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
002070     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
002080     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
002090*
002100*
002110********************
002120* ���b�Z�[�W�\���L�[ *
002130********************
002140 01 �A���|�L�[ IS EXTERNAL.
002150    03  �A���|���b�Z�[�W               PIC N(20).
002160*
002170****************
002180* ��ʓ��͏�� *
002190****************
002200 01 �A���|���͋敪�X�Q�Q IS EXTERNAL GLOBAL.
002210    03 �A���|�\���敪                PIC 9(1).
002220*/0:��f�ҏZ�����o�́A1:��ی��ҏZ�����o�́B
002230    03 �A���|�Z���敪                PIC 9(1).
002240*/0:���x�����o�́A1:���X�g���o�́A3:�b�r�u�B
002250    03 �A���|����`��                PIC 9(1).
002260*/���x���̊J�n�ʒu
002270    03 �A���|���x���J�n�ʒu          PIC 9(2).
      *
      */0:�X�֔ԍ��� 1:��f�҃J�i��
       01 �A���|����敪�X�Q�Q IS EXTERNAL.
          03 �A���|������敪              PIC 9(1).
002280*
002290******************************************************************
002300*                      PROCEDURE  DIVISION                       *
002310******************************************************************
002320 PROCEDURE               DIVISION.
002330************
002340*           *
002350* ��������   *
002360*           *
002370************
002380     PERFORM �v�����^�t�@�C���쐬.
002390     PERFORM ������.
002400*     PERFORM ���o�����Z�b�g.
002410************
002420*           *
002430* �又��     *
002440*           *
002450************
002460     PERFORM �������.
002470************
002480*           *
002490* �I������   *
002500*           *
002510************
002520     PERFORM �I������.
002530     EXIT PROGRAM.
002540*
002550*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002560*================================================================*
002570 �v�����^�t�@�C���쐬 SECTION.
002580*
002590*   / ������ /
002600     MOVE SPACE TO �g�A�o�q�s�e�|�쐬�f�[�^.
002610     INITIALIZE �g�A�o�q�s�e�|�쐬�f�[�^.
002620*
002630*
002640*--���� �ύX�ӏ� ����--------------------------------------*
002650*   �g�p����v�����^�t�@�C�����Z�b�g
002660     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
002670*
002680*   �g�p���钠�[�v���O�������Z�b�g
002690     MOVE "YAW9223P"              TO �g�A�o�q�s�e�|���[�v���O������.
002700*
002710*   / �v���r���[�敪�Z�b�g /
002720     MOVE ZERO TO �g�A�o�q�s�e�|�v���r���[�敪.
002730*****     MOVE �g�A���|�v���r���[�敪  TO �g�A�o�q�s�e�|�v���r���[�敪.
002740*
002750*--����-----------------------------------------------------*
002760*
002770     CALL   "CRTPRTF".
002780     CANCEL "CRTPRTF".
002790*
002800*================================================================*
002810 ������ SECTION.
002820*
002830     PERFORM �t�@�C���I�[�v��.
002840*    /* ���ݓ��t�擾 */
002850     ACCEPT �v�Z�@���t FROM DATE.
002860*    /* 1980�`2079�N�̊ԂŐݒ� */
002870     IF ( �v�Z�@�N > 80 )
002880         MOVE 19 TO �v�Z�@���I
002890     ELSE
002900         MOVE 20 TO �v�Z�@���I
002910     END-IF.
002920     PERFORM �J�����g�����擾.
002930     PERFORM �a��I���N�擾.
002940*     COMPUTE �v�Z�@����N�v = �v�Z�@����N - 1988.
002950*
002960*     MOVE �J�����g�����v TO ���ݘa��v.
002970*     MOVE �v�Z�@����N�v TO ���ݔN�v.
002980*     MOVE �v�Z�@��       TO ���݌��v.
002990*================================================================*
003000 �J�����g�����擾 SECTION.
003010*
003020     MOVE ZEROS TO ���|����敪.
003030     READ ������}�X�^
003040     NOT INVALID KEY
003050         MOVE ���|�J�����g���� TO �J�����g�����v
003060     END-READ.
003070*
003080*================================================================*
003090 �a��I���N�擾 SECTION.
003100*
003110     MOVE �J�����g�����v TO ���|�����敪.
003120     READ �����}�X�^
003130     INVALID KEY
003140         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
003150         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003160                                                  UPON CONS
003170         ACCEPT  �L�[���� FROM CONS
003180         PERFORM �I������
003190         EXIT PROGRAM
003200     NOT INVALID KEY
003210         COMPUTE �O�a��v = �J�����g�����v - 1
003220         MOVE �O�a��v TO ���|�����敪
003230         READ �����}�X�^
003240         INVALID KEY
003250             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
003260             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003270                                                      UPON CONS
003280             ACCEPT  �L�[���� FROM CONS
003290             PERFORM �I������
003300             EXIT PROGRAM
003310         NOT INVALID KEY
003320             MOVE ���|�I������N TO �a��I���N�v
003330         END-READ
003340     END-READ.
003350*
003360*================================================================*
003370 �t�@�C���I�[�v�� SECTION.
003380*
003390     OPEN INPUT �����}�X�^
003400         MOVE NC"����" TO �t�@�C����.
003410         PERFORM �I�[�v���`�F�b�N.
003420     OPEN INPUT ������}�X�^
003430         MOVE NC"������" TO �t�@�C����.
003440         PERFORM �I�[�v���`�F�b�N.
003450     OPEN INPUT �������e�e
003460         MOVE NC"�������e�e" TO �t�@�C����.
003470         PERFORM �I�[�v���`�F�b�N.
003480     OPEN INPUT ��ƃt�@�C���P
003490         MOVE NC"��P" TO �t�@�C����.
003500         PERFORM �I�[�v���`�F�b�N.
003510* �v���r���[�̎��AOPEN����WRITE������CLOSE����ƁA�G���[���ł�̂ŁA�f�[�^����������OPEN����
003520*     OPEN I-O   ����t�@�C��
003530*         PERFORM �G���[�����o.
003540*================================================================*
003550 �I�[�v���`�F�b�N SECTION.
003560*
003570     IF ( ��ԃL�[  NOT =  "00" )
003580         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
003590         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
003600         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003610                                                 UPON CONS
003620         CALL "actcshm"  WITH C LINKAGE
003630         ACCEPT  �L�[���� FROM CONS
003640         PERFORM �t�@�C����
003650         EXIT PROGRAM.
003660*================================================================*
003670 �t�@�C���� SECTION.
003680*
003690     IF ( �I�[�v���t���O = "YES" )
003700         CLOSE ����t�@�C��
003710     ELSE
003720         MOVE  NC"�@�@�f�[�^���O���ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
003730         CALL   "MSG001"
003740         CANCEL "MSG001"
003750     END-IF.
003760*
003770     CLOSE �����}�X�^ ������}�X�^ ��ƃt�@�C���P.
003780*================================================================*
003790 �I������ SECTION.
003800*
003810     PERFORM �t�@�C����.
003820*================================================================*
003830 �G���[�\�� SECTION.
003840*
003850     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
003860     DISPLAY NC"�T�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
003870     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"    UPON CONS.
003880     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003890                                                     UPON CONS.
003900     CALL "actcshm"  WITH C LINKAGE.
003910     ACCEPT  �L�[���� FROM CONS.
003920     PERFORM �t�@�C����.
003930     EXIT PROGRAM.
003940*================================================================*
003950 ������� SECTION.
003960*
003970     MOVE 60    TO �ő�s��.
003980     MOVE 6     TO �w�b�_�s��.
003990     MOVE 1     TO �A�Ԃv.
004000*
004010     MOVE SPACE TO �I���t���O.
004020     MOVE ZERO  TO �s�J�E���^.
004030     MOVE ZERO  TO �ŃJ�E���^.
004040*/���я���X�֔ԍ����҃J�i���ɂ���B/0404
004050     MOVE  1    TO ��P�|����敪.
004060     MOVE SPACE TO ��P�|���җX�֔ԍ�.
004070     MOVE SPACE TO ��P�|���҃J�i.
004080     MOVE ZERO  TO ��P�|�{�p�a��m.
004090     MOVE ZERO  TO ��P�|�{�p�N�m.
004100     MOVE ZERO  TO ��P�|�{�p���m.
004110     MOVE ZERO  TO ��P�|���Ҕԍ�.
004120     MOVE SPACE TO ��P�|�}��.
      */2014.07.14 ��f�҃J�i������ǉ�
           IF �A���|������敪 = ZERO
003170         START ��ƃt�@�C���P  KEY IS >= ��P�|����敪
003180                                         ��P�|���җX�֔ԍ�
003190                                         ��P�|���҃J�i
003200                                         ��P�|���҃R�[�h
003210                                         ��P�|�{�p�a��N���m
003220         END-START
           ELSE
003170         START ��ƃt�@�C���P  KEY IS >= ��P�|����敪
003190                                         ��P�|���҃J�i
003200                                         ��P�|���҃R�[�h
003210                                         ��P�|�{�p�a��N���m
003220         END-START
           END-IF.
004190     IF ( ��ԃL�[ = "00" )
004200         PERFORM ��ƃt�@�C���P�Ǎ�
004210         COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
004220         IF ( �I���t���O NOT = "YES" )
004230             MOVE "YES" TO �I�[�v���t���O
004240             OPEN I-O  ����t�@�C��
004250             PERFORM �G���[�����o
004260*
004270             PERFORM �w�b�_�󎚏���
004280         END-IF
004290         PERFORM UNTIL ( �I���t���O = "YES" ) OR ( ��P�|����敪 NOT = 1 )
004300             IF ( �s�J�E���^ >= �ő�s�� )
004310                 PERFORM ���ŏ���
004320                 COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
004330                 PERFORM �w�b�_�󎚏���
004340             ELSE
004350                 PERFORM ���׈󎚏���
004360                 PERFORM ���v�z�݌v
004370                 PERFORM ��ƃt�@�C���P�Ǎ�
004380                 COMPUTE �A�Ԃv = �A�Ԃv + 1
004390             END-IF
004400         END-PERFORM
004410     END-IF.
004420     IF ( �I�[�v���t���O = "YES" )
004430        PERFORM ���ŏ���
004440     END-IF.
004450*
004460*================================================================*
004470 ��ƃt�@�C���P�Ǎ� SECTION.
004480*
004490     READ ��ƃt�@�C���P NEXT
004500     AT END
004510         MOVE "YES" TO �I���t���O
004520     END-READ.
004530*
004540*================================================================*
004550 ���ŏ���  SECTION.
004560*
004570     MOVE "YAW9223P"  TO  ��`�̖��o.
004580     MOVE "CT"      TO  ������ʂo.
004590     MOVE "PAGE"    TO  �g������o.
004600     MOVE SPACE     TO  ���ڌQ���o.
004610     WRITE YAW9223P.
004620     PERFORM �G���[�����o.
004630     MOVE SPACE     TO  �g������o.
004640*
004650     CLOSE  ����t�@�C��.
004660     OPEN I-O   ����t�@�C��.
004670     PERFORM �G���[�����o.
004680*
004690*================================================================*
004700 ���v�z�݌v SECTION.
004710*
004720     COMPUTE ���v�����v   = ���v�����v   + 1.
004730     COMPUTE ���v�����v   = ���v�����v   + 1.
004740*================================================================*
004750 �w�b�_�󎚏��� SECTION.
004760*
004770     PERFORM �w�b�_�Z�b�g.
004780     PERFORM �w�b�_��.
004790*/0404
004800*     PERFORM ���o������.
004810     PERFORM �w�b�_�R��.
004820*================================================================*
004830 �w�b�_�Z�b�g SECTION.
004840*
004850     MOVE SPACE TO YAW9223P.
004860     INITIALIZE    YAW9223P.
004870     MOVE �ŃJ�E���^ TO �y�[�W.
004880*
004890*================================================================*
004900 ���o������ SECTION.
004910*
004920     IF ( ���o�����R�����g�v NOT = SPACE )
004930        MOVE ���o�����R�����g�v TO ���o�����R�����g
004940        MOVE "YAW9223P"  TO  ��`�̖��o
004950        MOVE SPACE     TO  ������ʂo
004960        MOVE "HEAD02"  TO  ���ڌQ���o
004970        WRITE YAW9223P
004980        PERFORM �G���[�����o
004990        INITIALIZE YAW9223P
005000     END-IF.
005010*
005020     IF ( ���o�����R�����g�v�Q NOT = SPACE )
005030        MOVE ���o�����R�����g�v�Q TO ���o�����R�����g
005040        MOVE "YAW9223P"  TO  ��`�̖��o
005050        MOVE SPACE     TO  ������ʂo
005060        MOVE "HEAD02"  TO  ���ڌQ���o
005070        WRITE YAW9223P
005080        PERFORM �G���[�����o
005090        INITIALIZE YAW9223P
005100     END-IF.
005110*
005120     IF ( ���o�����R�����g�v�R NOT = SPACE )
005130         MOVE ���o�����R�����g�v�R TO ���o�����R�����g
005140         MOVE "YAW9223P"  TO  ��`�̖��o
005150         MOVE SPACE     TO  ������ʂo
005160         MOVE "HEAD02"  TO  ���ڌQ���o
005170         WRITE YAW9223P
005180         PERFORM �G���[�����o
005190         INITIALIZE YAW9223P
005200     END-IF.
005210*
005220     IF ( ���o�����R�����g�v�S NOT = SPACE )
005230        MOVE ���o�����R�����g�v�S TO ���o�����R�����g
005240        MOVE "YAW9223P"  TO  ��`�̖��o
005250        MOVE SPACE     TO  ������ʂo
005260        MOVE "HEAD02"  TO  ���ڌQ���o
005270        WRITE YAW9223P
005280        PERFORM �G���[�����o
005290        INITIALIZE YAW9223P
005300     END-IF.
005310*
005320     IF ( ���o�����R�����g�v�T NOT = SPACE )
005330         MOVE ���o�����R�����g�v�T TO ���o�����R�����g
005340         MOVE "YAW9223P"  TO  ��`�̖��o
005350         MOVE SPACE     TO  ������ʂo
005360         MOVE "HEAD02"  TO  ���ڌQ���o
005370         WRITE YAW9223P
005380         PERFORM �G���[�����o
005390         INITIALIZE YAW9223P
005400     END-IF.
005410*
005420     IF ( ���o�����R�����g�v�U NOT = SPACE )
005430        MOVE ���o�����R�����g�v�U TO ���o�����R�����g
005440        MOVE "YAW9223P"  TO  ��`�̖��o
005450        MOVE SPACE     TO  ������ʂo
005460        MOVE "HEAD02"  TO  ���ڌQ���o
005470        WRITE YAW9223P
005480        PERFORM �G���[�����o
005490        INITIALIZE YAW9223P
005500     END-IF.
005510*
005520     IF ( ���o�����R�����g�v�V NOT = SPACE )
005530         MOVE ���o�����R�����g�v�V TO ���o�����R�����g
005540         MOVE "YAW9223P"  TO  ��`�̖��o
005550         MOVE SPACE     TO  ������ʂo
005560         MOVE "HEAD02"  TO  ���ڌQ���o
005570         WRITE YAW9223P
005580         PERFORM �G���[�����o
005590         INITIALIZE YAW9223P
005600     END-IF.
005610*
005620     IF ( ���o�����R�����g�v�W NOT = SPACE )
005630        MOVE ���o�����R�����g�v�W TO ���o�����R�����g
005640        MOVE "YAW9223P"  TO  ��`�̖��o
005650        MOVE SPACE     TO  ������ʂo
005660        MOVE "HEAD02"  TO  ���ڌQ���o
005670        WRITE YAW9223P
005680        PERFORM �G���[�����o
005690        INITIALIZE YAW9223P
005700     END-IF.
005710*================================================================*
005720 ���o�����Z�b�g SECTION.
005730*
005740     MOVE SPACE TO ���o�����R�����g�v.
005750     MOVE ZERO TO �����|����敪.
005760     READ �������e�e
005770     NOT INVALID KEY
005780        IF �����|�a�����ȓ� NOT = ZERO
005790            STRING ���o�����R�����g�v         DELIMITED BY SPACE
005800                   �����|�a�����ȓ�           DELIMITED BY SPACE
005810                   "���ȓ��ɒa����������l�B" DELIMITED BY SIZE
005820               INTO ���o�����R�����g�v
005830            END-STRING
005840        END-IF
005850        IF �����|�a���a�� NOT = ZERO
005860            EVALUATE �����|�a���a��
005870            WHEN 1
005880                MOVE "����"  TO �a�����v
005890             WHEN 2
005900                MOVE "�吳"  TO �a�����v
005910            WHEN 3
005920                MOVE "���a"  TO �a�����v
005930            WHEN 4
005940                MOVE "����"  TO �a�����v
005950            END-EVALUATE
005960        END-IF
005970        IF �����|�a���N NOT = ZERO
005980            STRING �a�����v      DELIMITED BY SPACE
005990                   �����|�a���N  DELIMITED BY SPACE
006000                   "�N"          DELIMITED BY SIZE
006010               INTO �a�����v
006020            END-STRING
006030        END-IF
006040        IF �����|�a���� NOT = ZERO
006050            STRING �a�����v      DELIMITED BY SPACE
006060                   �����|�a����  DELIMITED BY SPACE
006070                   "��"          DELIMITED BY SIZE
006080               INTO �a�����v
006090            END-STRING
006100        END-IF
006110        IF �����|�a���� NOT = ZERO
006120            STRING �a�����v      DELIMITED BY SPACE
006130                   �����|�a����  DELIMITED BY SPACE
006140                   "��"          DELIMITED BY SIZE
006150               INTO �a�����v
006160            END-STRING
006170        END-IF
006180        IF �a�����v NOT = SPACE
006190            STRING ���o�����R�����g�v  DELIMITED BY SPACE
006200                   �a�����v            DELIMITED BY SPACE
006210                   "���܂�̐l�B"      DELIMITED BY SIZE
006220               INTO ���o�����R�����g�v
006230            END-STRING
006240        END-IF
006250        IF �����|���� NOT = ZERO
006260            STRING ���o�����R�����g�v       DELIMITED BY SPACE
006270                   �����|���Ô͈͓���       DELIMITED BY SPACE
006280                   "���ȓ��ɏ��Â�����l�B" DELIMITED BY SIZE
006290               INTO ���o�����R�����g�v
006300            END-STRING
006310        END-IF
006320        IF �����|�����@ NOT = ZERO
006330            STRING ���o�����R�����g�v           DELIMITED BY SPACE
006340                   �����|�����@�͈͓���         DELIMITED BY SPACE
006350                   "���ȓ��ɏ��߂ė��@�����l�B" DELIMITED BY SIZE
006360               INTO ���o�����R�����g�v
006370            END-STRING
006380        END-IF
006390*
006400        INSPECT ���o�����R�����g�v REPLACING ALL "01" BY " 1"
006410        INSPECT ���o�����R�����g�v REPLACING ALL "02" BY " 2"
006420        INSPECT ���o�����R�����g�v REPLACING ALL "03" BY " 3"
006430        INSPECT ���o�����R�����g�v REPLACING ALL "04" BY " 4"
006440        INSPECT ���o�����R�����g�v REPLACING ALL "05" BY " 5"
006450        INSPECT ���o�����R�����g�v REPLACING ALL "06" BY " 6"
006460        INSPECT ���o�����R�����g�v REPLACING ALL "07" BY " 7"
006470        INSPECT ���o�����R�����g�v REPLACING ALL "08" BY " 8"
006480        INSPECT ���o�����R�����g�v REPLACING ALL "09" BY " 9"
006490*
006500        IF �����|���Ì�o�� NOT = ZERO
006510            STRING ���o�����R�����g�v�Q     DELIMITED BY SPACE
006520                   "���Ì�"                 DELIMITED BY SIZE
006530                   �����|���Ì�o�ߓ���     DELIMITED BY SPACE
006540                   "���o�߂��Ă��邪�A�ė��@���Ȃ��l�B�i�������o�ߓ���"
006550                                            DELIMITED BY SIZE
006560                   �����|���Ì�ő�o�ߓ��� DELIMITED BY SPACE
006570                   "���܂Łj"               DELIMITED BY SIZE
006580               INTO ���o�����R�����g�v�Q
006590            END-STRING
006600        END-IF
006610*
006620        IF �����|�ŏI����o�� NOT = ZERO
006630            STRING ���o�����R�����g�v�R     DELIMITED BY SPACE
006640                   "�p���ōŏI���@�����"   DELIMITED BY SIZE
006650                   �����|�ŏI����o�ߓ���   DELIMITED BY SPACE
006660                   "���o�߂��Ă��邪�A�ė��@���Ȃ��l�B�i�������o�ߓ���"
006670                                            DELIMITED BY SIZE
006680                   �����|�ŏI����ő�o�ߓ��� DELIMITED BY SPACE
006690                   "���܂Łj"               DELIMITED BY SIZE
006700               INTO ���o�����R�����g�v�R
006710            END-STRING
006720        END-IF
006730*
006740        IF �����|���� NOT = ZERO
006750            STRING ���o�����R�����g�v�S DELIMITED BY SPACE
006760                   �����|�����͈͓���   DELIMITED BY SPACE
006770                   "���ȓ��Ɏ����̐l�B" DELIMITED BY SIZE
006780               INTO ���o�����R�����g�v�S
006790            END-STRING
006800        END-IF
006810        IF �����|���~ NOT = ZERO
006820            STRING ���o�����R�����g�v�S DELIMITED BY SPACE
006830                   �����|���~�͈͓���   DELIMITED BY SPACE
006840                   "���ȓ��ɒ��~�̐l�B" DELIMITED BY SIZE
006850               INTO ���o�����R�����g�v�S
006860            END-STRING
006870        END-IF
006880*
006890        IF �����|�͈͓����� NOT = ZERO
006900            STRING "���Â�����l�B" DELIMITED BY SIZE
006910               INTO �����v
006920            END-STRING
006930        END-IF
006940        IF �����|���Z���v�z NOT = ZERO
006950            STRING �����v             DELIMITED BY SPACE
006960                   "���Z���v���z��"   DELIMITED BY SIZE
006970                   �����|���Z���v�z   DELIMITED BY SPACE
006980                   "�~�ȏ�B"         DELIMITED BY SIZE
006990               INTO �����v
007000            END-STRING
007010        END-IF
007020        IF �����|������z NOT = ZERO
007030            STRING �����v           DELIMITED BY SPACE
007040                   "������z��"     DELIMITED BY SIZE
007050                   �����|������z   DELIMITED BY SPACE
007060                   "�~�ȏ�B"       DELIMITED BY SIZE
007070               INTO �����v
007080            END-STRING
007090        END-IF
007100        IF �����|�ʉ@�񐔈ȏ� NOT = ZERO
007110            STRING �����v              DELIMITED BY SPACE
007120                   "���@�񐔂�"        DELIMITED BY SIZE
007130                   �����|�ʉ@�񐔈ȏ�  DELIMITED BY SPACE
007140                   "��ȏ�"            DELIMITED BY SIZE
007150               INTO �����v
007160            END-STRING
007170        END-IF
007180        IF �����|�ʉ@�񐔈ȉ� NOT = ZERO
007190            IF �����|�ʉ@�񐔈ȏ� = ZERO
007200                STRING �����v              DELIMITED BY SPACE
007210                       "���@�񐔂�"        DELIMITED BY SIZE
007220                       �����|�ʉ@�񐔈ȉ�  DELIMITED BY SPACE
007230                       "��ȉ��B"          DELIMITED BY SIZE
007240                   INTO �����v
007250                END-STRING
007260            ELSE
007270                STRING �����v              DELIMITED BY SPACE
007280                       �����|�ʉ@�񐔈ȉ�  DELIMITED BY SPACE
007290                       "��ȉ��B"          DELIMITED BY SIZE
007300                   INTO �����v
007310               END-STRING
007320           END-IF
007330        ELSE
007340           IF �����|�ʉ@�񐔈ȏ� NOT = ZERO
007350               STRING �����v              DELIMITED BY SPACE
007360                      "�B"                DELIMITED BY SIZE
007370                  INTO �����v
007380               END-STRING
007390           END-IF
007400        END-IF
007410        IF �����v NOT = SPACE
007420           IF �����|�J�n�{�p�N NOT = ZERO
007430               EVALUATE �����|�J�n�{�p�a��
007440               WHEN 4
007450                   MOVE "����"  TO �a��v
007460               END-EVALUATE
007470               STRING �a��v           DELIMITED BY SPACE
007480                      �����|�J�n�{�p�N DELIMITED BY SPACE
007490                      "�N"             DELIMITED BY SIZE
007500                      �����|�J�n�{�p�� DELIMITED BY SPACE
007510                      "��"             DELIMITED BY SIZE
007520                   INTO �w��N���v
007530               END-STRING
007540            END-IF
007550            IF �����|�I���{�p�N NOT = ZERO
007560               IF �w��N���v NOT = SPACE
007570                   STRING �w��N���v       DELIMITED BY SPACE
007580                          "�`"             DELIMITED BY SIZE
007590                     INTO �w��N���v
007600                   END-STRING
007610               END-IF
007620               EVALUATE �����|�I���{�p�a��
007630               WHEN 4
007640                   MOVE "����"  TO �a��v
007650               END-EVALUATE
007660               STRING �w��N���v       DELIMITED BY SPACE
007670                      �a��v           DELIMITED BY SPACE
007680                      �����|�I���{�p�N DELIMITED BY SPACE
007690                      "�N"             DELIMITED BY SIZE
007700                      �����|�I���{�p�� DELIMITED BY SPACE
007710                      "��"             DELIMITED BY SIZE
007720                 INTO �w��N���v
007730               END-STRING
007740            END-IF
007750            STRING �w��N���v         DELIMITED BY SPACE
007760                   "��"               DELIMITED BY SIZE
007770              INTO ���o�����R�����g�v�T
007780            END-STRING
007790*
007800            STRING �����v             DELIMITED BY SPACE
007810               INTO ���o�����R�����g�v�U
007820            END-STRING
007830        END-IF
007840*
007850        IF ( �����|�n���P NOT = SPACE )
007860            MOVE �����|�n���P TO �n���v
007870        END-IF
007880        IF ( �����|�n���Q NOT = SPACE )
007890            STRING �n���v         DELIMITED BY SPACE
007900                   "�A"           DELIMITED BY SIZE
007910                   �����|�n���Q   DELIMITED BY SPACE
007920               INTO �n���v
007930            END-STRING
007940        END-IF
007950        IF ( �����|�n���R NOT = SPACE )
007960            STRING �n���v         DELIMITED BY SPACE
007970                   "�A"           DELIMITED BY SIZE
007980                   �����|�n���R   DELIMITED BY SPACE
007990               INTO �n���v
008000            END-STRING
008010        END-IF
008020        IF �n���v NOT = SPACE
008030             STRING "�Z����"      DELIMITED BY SIZE
008040                    �n���v        DELIMITED BY SPACE
008050                    "�̐l�B"      DELIMITED BY SIZE
008060               INTO ���o�����R�����g�v�V
008070            END-STRING
008080        END-IF
008090        IF �����|�J�n�N�� NOT = ZERO
008100            STRING �����|�J�n�N�� DELIMITED BY SPACE
008110                   "��"           DELIMITED BY SIZE
008120               INTO �N��v
008130            END-STRING
008140        END-IF
008150        IF �����|�I���N�� NOT = ZERO
008160            STRING �N��v         DELIMITED BY SPACE
008170                   "�`"           DELIMITED BY SIZE
008180                   �����|�I���N�� DELIMITED BY SPACE
008190                   "��"           DELIMITED BY SIZE
008200               INTO �N��v
008210            END-STRING
008220        END-IF
008230        IF �N��v NOT = SPACE
008240            STRING "�N�"       DELIMITED BY SIZE
008250                   �N��v         DELIMITED BY SPACE
008260                   "�̐l�B"       DELIMITED BY SIZE
008270               INTO ���o�����R�����g�v�W
008280            END-STRING
008290        END-IF
008300        IF �����|�j�� NOT = ZERO
008310            STRING ���o�����R�����g�v�W DELIMITED BY SPACE
008320                   "�j���̐l�B"       DELIMITED BY SIZE
008330               INTO ���o�����R�����g�v�W
008340            END-STRING
008350        END-IF
008360        IF �����|���� NOT = ZERO
008370             STRING ���o�����R�����g�v�W DELIMITED BY SPACE
008380                    "�����̐l�B"       DELIMITED BY SIZE
008390               INTO ���o�����R�����g�v�W
008400            END-STRING
008410        END-IF
008420     END-READ.
008430*================================================================*
008440 �w�b�_��  SECTION.
008450*
008460     MOVE "YAW9223P"  TO  ��`�̖��o.
008470     MOVE SPACE     TO  ������ʂo.
008480     MOVE "HEAD01"  TO  ���ڌQ���o.
008490     WRITE YAW9223P.
008500     PERFORM �G���[�����o.
008510     INITIALIZE YAW9223P.
008520*/0404
008530*     MOVE �w�b�_�s�� TO �s�J�E���^.
008540     MOVE ZERO TO �s�J�E���^.
008550*================================================================*
008560 �w�b�_�R��  SECTION.
008570*
008580     MOVE "YAW9223P"  TO  ��`�̖��o.
008590     MOVE SPACE     TO  ������ʂo.
008600     MOVE "HEAD03"  TO  ���ڌQ���o.
008610     WRITE YAW9223P.
008620     PERFORM �G���[�����o.
008630     INITIALIZE YAW9223P.
008640*================================================================*
008650 ���׈󎚏��� SECTION.
008660*
008670     PERFORM ���ו��Z�b�g.
008680     PERFORM ���׈�.
008690*================================================================*
008700 ���ו��Z�b�g SECTION.
008710*
008720     MOVE SPACE TO YAW9223P.
008730     INITIALIZE    YAW9223P.
008740     MOVE �A�Ԃv               TO �A��.
008750*     MOVE ��P�|���҃R�[�h     TO ���҃R�[�h.
008760     MOVE ��P�|���Ҕԍ�       TO ���Ҕԍ�.
008770     MOVE ��P�|�}��           TO �}��.
008780     MOVE NC"��"               TO �X�փ}�[�N.
008790     MOVE ��P�|���җX�֔ԍ��P TO �X�֔ԍ��P.
008800     MOVE ��P�|���җX�֔ԍ��Q TO �X�֔ԍ��Q.
008810     IF ��P�|���җX�֔ԍ��Q = SPACE
008820         MOVE SPACE            TO ���
008830     ELSE
008840         MOVE "-"              TO ���
008850     END-IF.
008860     MOVE SPACE                TO ��f�ҏZ��.
008870     STRING ��P�|���ҏZ���P DELIMITED BY SPACE
008880            ��P�|���ҏZ���Q DELIMITED BY SPACE
008890            INTO ��f�ҏZ��
008900     END-STRING.
008910     MOVE ��P�|���Ҏ��� TO ���Ҏ���.
008920*
008930*================================================================*
008940 ���׈�  SECTION.
008950*
008960     MOVE "YAW9223P"   TO  ��`�̖��o.
008970     MOVE "GRP001"   TO  ���ڌQ���o.
008980     WRITE YAW9223P.
008990     PERFORM �G���[�����o.
009000     INITIALIZE YAW9223P.
009010     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
009020*================================================================*
009030 �G���[�����o SECTION.
009040*
009050     IF ( �ʒm���o NOT = "00" )
009060         DISPLAY NC"���[�G���["              UPON CONS
009070         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
009080         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
009090         DISPLAY NC"�g������o�F" �g������o UPON CONS
009100         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
009110                                             UPON CONS
009120         CALL "actcshm"  WITH C LINKAGE
009130         ACCEPT  �L�[���� FROM CONS
009140         PERFORM �t�@�C����
009150         EXIT PROGRAM
009160     END-IF.
009170*================================================================*
009180******************************************************************
009190 END PROGRAM YAW9223.
009200******************************************************************
