000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             GETUJIK2.
000060 AUTHOR.                 ����@��
000070*
000080*----------------------------------------------------------------*
000090*  �����X�V ����
000100*  �J�ЁE�����ӁE���ۂ̃t�@�C���������X�V
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2009-09-11
000130 DATE-COMPILED.          2009-09-11
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
000260*
000270     SELECT  �����ӏ��e    ASSIGN      TO        JIBAIJL
000280                             ORGANIZATION             IS INDEXED
000290                             ACCESS MODE              IS DYNAMIC
000300                             RECORD KEY               IS �����|�{�p�a��N��
000310                                                         �����|���҃R�[�h
000320                             ALTERNATE RECORD KEY     IS �����|���҃R�[�h
000330                                                         �����|�{�p�a��N��
000340                             FILE STATUS              IS ��ԃL�[
000350                             LOCK        MODE         IS AUTOMATIC.
000360*
000370     SELECT  �J�Џ��e      ASSIGN      TO        ROUSAIJL
000380                             ORGANIZATION             IS INDEXED
000390                             ACCESS MODE              IS DYNAMIC
000400                             RECORD KEY               IS �J�Ё|�{�p�a��N��
000410                                                         �J�Ё|���҃R�[�h
000420                             ALTERNATE RECORD KEY     IS �J�Ё|���҃R�[�h
000430                                                         �J�Ё|�{�p�a��N��
000440                             FILE STATUS              IS ��ԃL�[
000450                             LOCK        MODE         IS AUTOMATIC.
000460*
000470     SELECT  ���ۏ��e      ASSIGN      TO        SEIHOJL
000480                             ORGANIZATION             IS INDEXED
000490                             ACCESS MODE              IS DYNAMIC
000500                             RECORD KEY               IS ���ہ|�{�p�a��N��
000510                                                         ���ہ|���҃R�[�h
000520                             ALTERNATE RECORD KEY     IS ���ہ|���҃R�[�h
000530                                                         ���ہ|�{�p�a��N��
000540                             FILE STATUS              IS ��ԃL�[
000550                             LOCK        MODE         IS AUTOMATIC.
000560*
000570     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000580                             ORGANIZATION             IS  INDEXED
000590                             ACCESS MODE              IS  DYNAMIC
000600                             RECORD KEY               IS  ���|�{�p�a��N��
000610                                                          ���|���҃R�[�h
000620                             ALTERNATE RECORD KEY     IS  ���|���҃R�[�h
000630                                                          ���|�{�p�a��N��
000640                             FILE STATUS              IS  ��ԃL�[
000650                             LOCK        MODE         IS  AUTOMATIC.
000660     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
000670                             ORGANIZATION             IS  INDEXED
000680                             ACCESS MODE              IS  DYNAMIC
000690                             RECORD KEY           IS �{�L�|�{�p�a��N����
000700                                                     �{�L�|���҃R�[�h
000710                             ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
000720                                                     �{�L�|�{�p�a��N����
000730                             FILE STATUS              IS  ��ԃL�[
000740                             LOCK        MODE         IS  AUTOMATIC.
000750*
000760     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000770                             ORGANIZATION             IS  INDEXED
000780                             ACCESS MODE              IS  DYNAMIC
000790                             RECORD KEY               IS  ���|�����敪
000800                             FILE STATUS              IS  ��ԃL�[
000810                             LOCK        MODE         IS  AUTOMATIC.
000820******************************************************************
000830*                      DATA DIVISION                             *
000840******************************************************************
000850 DATA                    DIVISION.
000860 FILE                    SECTION.
000870*
000880 FD  �����ӏ��e        BLOCK   CONTAINS   1   RECORDS.
000890     COPY JIBAIJ      OF  XFDLIB  JOINING   ����   AS  PREFIX.
000900*
000910 FD  �J�Џ��e        BLOCK   CONTAINS   1   RECORDS.
000920     COPY ROUSAIJ      OF  XFDLIB  JOINING   �J��   AS  PREFIX.
000930*
000940 FD  ���ۏ��e       BLOCK   CONTAINS   1   RECORDS.
000950     COPY SEIHOJ      OF  XFDLIB  JOINING   ����   AS  PREFIX.
000960*                           �m�q�k��  �P�Q�W�n
000970 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
000980     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000990*                           �m�q�k��  �Q�T�U�n
001000 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
001010     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
001020*                           �m�q�k��  �P�Q�W�n
001030 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001040     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001050*
001060*----------------------------------------------------------------*
001070******************************************************************
001080*                WORKING-STORAGE SECTION                         *
001090******************************************************************
001100 WORKING-STORAGE         SECTION.
001110 01 �L�[����                           PIC X    VALUE SPACE.
001120 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
001130 01 �I���t���O                         PIC X(3) VALUE SPACE.
001140 01 �X�V�t���O                         PIC X(3) VALUE SPACE.
001150 01 �t�@�C����                         PIC N(6) VALUE SPACE.
001160 01 �{�p�a��N���m�v.
001170   03 �{�p�a��m�v                     PIC 9.
001180   03 �{�p�N�m�v                       PIC 9(2).
001190   03 �{�p���m�v                       PIC 9(2).
001200*
001210 01 �O���t���O                         PIC X(3) VALUE SPACE.
001220 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
001230*/�O��ʉ@������
001240 01 �v�Z�N�����v.
001250    03 �v�Z�a��v                      PIC 9(1)  VALUE ZERO.
001260    03 �v�Z�N�v                        PIC S9(2)  VALUE ZERO.
001270    03 �v�Z���v                        PIC S9(2)  VALUE ZERO.
001280    03 �v�Z���v                        PIC S9(2)  VALUE ZERO.
001290 01 �J�n�N�����Q�v.
001300    03 �J�n�a��Q�v                    PIC 9(1)  VALUE ZERO.
001310    03 �J�n�N�Q�v                      PIC 9(2)  VALUE ZERO.
001320    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
001330    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
001340    03 �J�n����N�v                    PIC S9(4) VALUE ZERO.
001350 01 �I���N�����Q�v.
001360    03 �I���a��Q�v                    PIC 9(1)  VALUE ZERO.
001370    03 �I���N�Q�v                      PIC 9(2)  VALUE ZERO.
001380    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
001390    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
001400    03 �I������N�v                    PIC S9(4) VALUE ZERO.
001410**�f�[�^�`�F�b�N�p
001420 01 ���s�L�[�v                         PIC X(3)  VALUE SPACE.
001430 01 �{�p�L�^�L�v                       PIC X(3) VALUE SPACE.
001440**
001450 01 �O�l�I���t���O                     PIC X(3) VALUE SPACE.
001460 01 ���ʃJ�E���^                       PIC 9(2) VALUE ZERO.
001470 01 ���ʃJ�E���^�v                     PIC 9(2) VALUE ZERO.
001480 01 ���ʃJ�E���^�Q                     PIC 9(2) VALUE ZERO.
001490 01 ���R�[�h�f�[�^�s�v.
001500    03 ���ʕʗ����s�v�P.
001510       05 ���ʕʗ����s�v OCCURS 7.
001520          07 �������s�v                PIC 9(5) VALUE ZERO.
001530          07 ��×��Q�O�s�v            PIC 9(4) VALUE ZERO.
001540          07 ��×��Q�R�s�v            PIC 9(4) VALUE ZERO.
001550          07 ��×��Q�S�s�v            PIC 9(4) VALUE ZERO.
001560          07 ��×��R�O�s�v            PIC 9(4) VALUE ZERO.
001570          07 ���ʍޗ���s�v            PIC 9(4) VALUE ZERO.
001580          07 ������ї��s�v            PIC 9(4) VALUE ZERO.
001590          07 �S�k��×��Q�O�s�v        PIC 9(4) VALUE ZERO.
001600          07 �S�k��×��R�O�s�v        PIC 9(4) VALUE ZERO.
001610*****************************************************************
001620*                          �A������                             *
001630*****************************************************************
001640*
001650*
001660*-------------------------------------------------------------------*
001670* �J�ЁE�����ӁE���ۃt�@�C���̌����X�V�A�g�L�[
001680 01 �A���J�Г��|�L�[ IS EXTERNAL.
001690    03  �A���J�Г��|�{�p�a��N��.
001700      05  �A���J�Г��|�{�p�a��               PIC 9.
001710      05  �A���J�Г��|�{�p�N��.
001720        07  �A���J�Г��|�{�p�N               PIC 9(2).
001730        07  �A���J�Г��|�{�p��               PIC 9(2).
001740    03  �A���J�Г��|���҃R�[�h.
001750      05  �A���J�Г��|���Ҕԍ�               PIC 9(6).
001760      05  �A���J�Г��|�}��                   PIC X.
001770*   / 1:���N�ی��A2:�J�ЁA3:�����ӁA4:���ہA5:���� /
001780    03  �A���J�Г��|�ی�����                 PIC 9(2).
001790    03  �A���J�Г��|�ŐV�{�p�a��N��.
001800      05  �A���J�Г��|�ŐV�{�p�a��           PIC 9.
001810      05  �A���J�Г��|�ŐV�{�p�N��.
001820        07  �A���J�Г��|�ŐV�{�p�N           PIC 9(2).
001830        07  �A���J�Г��|�ŐV�{�p��           PIC 9(2).
001840*
001850*
001860 01 �A���|�L�[ IS EXTERNAL.
001870    03  �A���|���b�Z�[�W                 PIC N(20).
001880*
001890*
001900******************************************************************
001910*                      PROCEDURE  DIVISION                       *
001920******************************************************************
001930 PROCEDURE               DIVISION.
001940************
001950*           *
001960* ��������   *
001970*           *
001980************
001990     PERFORM ������.
002000************
002010*           *
002020* �又��     *
002030*           *
002040************
002050*
002060     EVALUATE �A���J�Г��|�ی�����
002070     WHEN 2
002080        PERFORM �J�Ќ����X�V
002090     WHEN 3
002100        PERFORM �����ӌ����X�V
002110     WHEN 4
002120        PERFORM ���ی����X�V
002130     END-EVALUATE.
002140*
002150************
002160*           *
002170* �I������   *
002180*           *
002190************
002200     PERFORM �I������.
002210     EXIT PROGRAM.
002220*
002230*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002240*================================================================*
002250 ������ SECTION.
002260*
002270*
002280*================================================================*
002290 �I�[�v���`�F�b�N SECTION.
002300*
002310     IF ��ԃL�[  NOT =  "00"
002320         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
002330         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
002340         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
002350                                                 UPON CONS
002360*-----------------------------------------*
002370         CALL "actcshm"  WITH C LINKAGE
002380*-----------------------------------------*
002390         ACCEPT  �L�[���� FROM CONS
002400         PERFORM �t�@�C����
002410         EXIT PROGRAM.
002420*================================================================*
002430 �t�@�C���� SECTION.
002440*
002450*================================================================*
002460 �I������ SECTION.
002470*
002480     PERFORM �t�@�C����.
002490     MOVE 1 TO PROGRAM-STATUS.
002500*================================================================*
002510 �G���[�\�� SECTION.
002520*
002530     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
002540     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
002550     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
002560     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
002570*-----------------------------------------*
002580     CALL "actcshm"  WITH C LINKAGE.
002590*-----------------------------------------*
002600     ACCEPT  �L�[���� FROM CONS.
002610     PERFORM �I������.
002620     MOVE ZERO TO PROGRAM-STATUS.
002630     EXIT PROGRAM.
002640*
002650*================================================================*
002660 �G���[�\���q SECTION.
002670*
002680     DISPLAY NC"�t�@�C���Ǎ��G���[" �t�@�C����     UPON CONS.
002690     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
002700     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
002710*-----------------------------------------*
002720     CALL "actcshm"  WITH C LINKAGE.
002730*-----------------------------------------*
002740     ACCEPT  �L�[���� FROM CONS.
002750     PERFORM �I������.
002760     MOVE ZERO TO PROGRAM-STATUS.
002770     EXIT PROGRAM.
002780*
002790*================================================================*
002800*================================================================*
002810 �J�Ќ����X�V SECTION.
002820*
002830     OPEN I-O �J�Џ��e.
002840         MOVE NC"�J��" TO �t�@�C����.
002850         PERFORM �I�[�v���`�F�b�N.
002860*
002870     MOVE �A���J�Г��|�ŐV�{�p�a�� TO �J�Ё|�{�p�a��.
002880     MOVE �A���J�Г��|�ŐV�{�p�N   TO �J�Ё|�{�p�N.
002890     MOVE �A���J�Г��|�ŐV�{�p��   TO �J�Ё|�{�p��.
002900     MOVE �A���J�Г��|���Ҕԍ�     TO �J�Ё|���Ҕԍ�.
002910     MOVE �A���J�Г��|�}��         TO �J�Ё|�}��.
002920     READ �J�Џ��e
002930     INVALID KEY
002940          CLOSE �J�Џ��e
002950          DISPLAY NC"�J�Џ��e�̌����X�V���ł��܂���B" UPON CONS
002960          DISPLAY NC"�ŐV�N���̃f�[�^������܂���B" UPON CONS
002970          MOVE NC"�J��" TO �t�@�C����
002980          PERFORM �G���[�\���q
002990     NOT INVALID KEY
003000          MOVE �A���J�Г��|�{�p�a�� TO �J�Ё|�{�p�a��
003010          MOVE �A���J�Г��|�{�p�N   TO �J�Ё|�{�p�N
003020          MOVE �A���J�Г��|�{�p��   TO �J�Ё|�{�p��
003030*
003040          WRITE �J�Ё|���R�[�h
003050          IF ��ԃL�[ NOT = "00"
003060             IF ��ԃL�[ NOT = "22"
003070                MOVE NC"�J��" TO �t�@�C����
003080                PERFORM �G���[�\��
003090             END-IF
003100          END-IF
003110     END-READ.
003120*
003130     CLOSE �J�Џ��e.
003140*
003150*================================================================*
003160*================================================================*
003170 �����ӌ����X�V SECTION.
003180*
003190     OPEN I-O �����ӏ��e.
003200         MOVE NC"������" TO �t�@�C����.
003210         PERFORM �I�[�v���`�F�b�N.
003220     OPEN INPUT �����f�[�^�e.
003230         MOVE NC"����" TO �t�@�C����.
003240         PERFORM �I�[�v���`�F�b�N.
003250     OPEN INPUT �{�p�L�^�e.
003260         MOVE NC"�{�L�e" TO �t�@�C����.
003270         PERFORM �I�[�v���`�F�b�N.
003280     OPEN INPUT   �����}�X�^.
003290         MOVE NC"����" TO �t�@�C����.
003300         PERFORM �I�[�v���`�F�b�N.
003310*
003320     MOVE �A���J�Г��|�ŐV�{�p�a�� TO �����|�{�p�a��.
003330     MOVE �A���J�Г��|�ŐV�{�p�N   TO �����|�{�p�N.
003340     MOVE �A���J�Г��|�ŐV�{�p��   TO �����|�{�p��.
003350     MOVE �A���J�Г��|���Ҕԍ�     TO �����|���Ҕԍ�.
003360     MOVE �A���J�Г��|�}��         TO �����|�}��.
003370     READ �����ӏ��e
003380*/�R���o�[�g����̓��R�[�h���Ȃ��B�Ȃ��Ă����Ȃ�������/130227
003390*     INVALID KEY
003400*          CLOSE �����ӏ��e
003410*          DISPLAY NC"�����ӏ��e�̌����X�V���ł��܂���B" UPON CONS
003420*          DISPLAY NC"�ŐV�N���̃f�[�^������܂���B"       UPON CONS
003430*          MOVE NC"������" TO �t�@�C����
003440*          PERFORM �G���[�\���q
003450*/�R���o�[�g����̓��R�[�h���Ȃ��B�Ȃ��Ă����Ȃ�������/130227
003460     NOT INVALID KEY
003470          MOVE �A���J�Г��|�{�p�a�� TO �����|�{�p�a��
003480          MOVE �A���J�Г��|�{�p�N   TO �����|�{�p�N
003490          MOVE �A���J�Г��|�{�p��   TO �����|�{�p��
003500*/�O���ɕ����ʉ@������ꍇ�f�[�^���J��z��
003510*/�O���ɕ����ʉ@�������Ă��ŗL�P���g�p�敪���[���Ȃ�ŗL�P�����N���A
003520*/�O���ɕ����ʉ@���Ȃ��ꍇ�̓L�[���̂݌J��z��
003530          PERFORM �O��ʉ@������
003540          IF ���s�L�[�v = "YES"
003550              IF �����|�ŗL�P���g�p�敪 = 1
003560                  PERFORM �ŗL�P���O�l����
003570              ELSE
003580                  PERFORM �ŗL�P�������N���A
003590              END-IF
003600          ELSE
003610              MOVE SPACE TO �����|���R�[�h�f�[�^
003620              INITIALIZE    �����|���R�[�h�f�[�^
003630          END-IF
003640*
003650          WRITE �����|���R�[�h
003660          IF ��ԃL�[ NOT = "00"
003670             IF ��ԃL�[ NOT = "22"
003680                MOVE NC"������" TO �t�@�C����
003690                PERFORM �G���[�\��
003700             END-IF
003710          END-IF
003720     END-READ.
003730*
003740     CLOSE �����ӏ��e �����f�[�^�e �{�p�L�^�e �����}�X�^.
003750*
003760*================================================================*
003770*================================================================*
003780 ���ی����X�V SECTION.
003790*
003800     OPEN I-O ���ۏ��e.
003810         MOVE NC"����" TO �t�@�C����.
003820         PERFORM �I�[�v���`�F�b�N.
003830*
003840     MOVE �A���J�Г��|�ŐV�{�p�a�� TO ���ہ|�{�p�a��.
003850     MOVE �A���J�Г��|�ŐV�{�p�N   TO ���ہ|�{�p�N.
003860     MOVE �A���J�Г��|�ŐV�{�p��   TO ���ہ|�{�p��.
003870     MOVE �A���J�Г��|���Ҕԍ�     TO ���ہ|���Ҕԍ�.
003880     MOVE �A���J�Г��|�}��         TO ���ہ|�}��.
003890     READ ���ۏ��e
003900     INVALID KEY
003910          CLOSE ���ۏ��e
003920          DISPLAY NC"���ۏ��e�̌����X�V���ł��܂���B" UPON CONS
003930          DISPLAY NC"�ŐV�N���̃f�[�^������܂���B" UPON CONS
003940          MOVE NC"����" TO �t�@�C����
003950          PERFORM �G���[�\���q
003960     NOT INVALID KEY
003970          MOVE �A���J�Г��|�{�p�a�� TO ���ہ|�{�p�a��
003980          MOVE �A���J�Г��|�{�p�N   TO ���ہ|�{�p�N
003990          MOVE �A���J�Г��|�{�p��   TO ���ہ|�{�p��
004000*
004010          WRITE ���ہ|���R�[�h
004020          IF ��ԃL�[ NOT = "00"
004030             IF ��ԃL�[ NOT = "22"
004040                MOVE NC"����" TO �t�@�C����
004050                PERFORM �G���[�\��
004060             END-IF
004070          END-IF
004080     END-READ.
004090*
004100     CLOSE ���ۏ��e.
004110*
004120*================================================================*
004130*================================================================*
004140*================================================================*
004150 �O��ʉ@������ SECTION.
004160*
004170*** �O��̕����f�[�^���O�������� 
004180     MOVE  SPACE                 TO �O���t���O.
004190     MOVE �A���J�Г��|���҃R�[�h TO ���|���҃R�[�h.
004200     MOVE �A���J�Г��|�{�p�a��   TO ���|�{�p�a��.
004210     MOVE �A���J�Г��|�{�p�N     TO ���|�{�p�N.
004220     MOVE �A���J�Г��|�{�p��     TO ���|�{�p��.
004230     START �����f�[�^�e KEY IS <  ���|���҃R�[�h
004240                                  ���|�{�p�a��N��
004250                                  REVERSED
004260     END-START.
004270     IF ��ԃL�[ = "00"
004280         MOVE SPACE  TO �I���t���O�Q
004290         PERFORM �����f�[�^�e�Ǎ�
004300         IF ( �I���t���O�Q    = SPACE                  ) AND
004310            ( ���|���҃R�[�h  = �A���J�Г��|���҃R�[�h )
004320            PERFORM �O������
004330         END-IF
004340     END-IF.
004350*
004360*================================================================*
004370 �O������ SECTION.
004380* 
004390*** �ǂݍ��񂾕����f�[�^�̔N�����A�O�����ǂ������� (�N���̍��� 1 ��?)
004400      MOVE  SPACE  TO  �O���t���O.
004410      INITIALIZE  �v�Z�N�����v �J�n�N�����Q�v �I���N�����Q�v.
004420**
004430      MOVE �A���J�Г��|�{�p�a��  TO �I���a��Q�v.
004440      MOVE �A���J�Г��|�{�p�N    TO �I���N�Q�v.
004450      MOVE �A���J�Г��|�{�p��    TO �I�����Q�v.
004460      MOVE ���|�{�p�a��          TO �J�n�a��Q�v.
004470      MOVE ���|�{�p�N            TO �J�n�N�Q�v.
004480      MOVE ���|�{�p��            TO �J�n���Q�v.
004490*
004500      EVALUATE TRUE
004510       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v = �I���N�Q�v)
004520            PERFORM  �O����r��
004530       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v NOT = �I���N�Q�v)
004540            PERFORM  �O����r�N
004550       WHEN  �J�n�a��Q�v NOT = �I���a��Q�v 
004560            PERFORM  �O����r����
004570      END-EVALUATE.
004580*
004590*--- �������t�`�F�b�N ---*
004600*****      IF �v�Z���v = 1
004610*****         MOVE  "YES"  TO  �O���t���O
004620*****         PERFORM �f�[�^�`�F�b�N
004630******      END-IF.
004640      IF ( �v�Z���v = 1 )
004650         MOVE  "YES"  TO  �O���t���O
004660         IF ( �J�n�a��Q�v NOT = �I���a��Q�v )
004670            MOVE "YES"  TO  ���s�L�[�v
004680         ELSE
004690            PERFORM �f�[�^�`�F�b�N
004700         END-IF
004710      END-IF.
004720*
004730*================================================================*
004740 �O����r��  SECTION.
004750*
004760     IF  �I�����Q�v >  �J�n���Q�v
004770         COMPUTE �v�Z���v = �I�����Q�v - �J�n���Q�v
004780     ELSE
004790        MOVE ZERO TO �v�Z���v
004800     END-IF.
004810*
004820*================================================================*
004830 �O����r�N  SECTION.
004840*
004850     IF  �I���N�Q�v >  �J�n�N�Q�v
004860         COMPUTE �v�Z�N�v = �I���N�Q�v - �J�n�N�Q�v
004870         COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
004880     ELSE
004890        MOVE ZERO TO �v�Z���v
004900     END-IF.
004910*
004920*================================================================*
004930 �O����r����  SECTION.
004940*
004950     MOVE �J�n�a��Q�v TO ���|�����敪.
004960     READ �����}�X�^
004970     NOT INVALID KEY
004980         MOVE ���|�J�n����N TO �J�n����N�v
004990     END-READ.
005000     MOVE �I���a��Q�v TO ���|�����敪.
005010     READ �����}�X�^
005020     NOT INVALID KEY
005030         MOVE ���|�J�n����N TO �I������N�v
005040     END-READ.
005050**
005060     IF (�J�n����N�v NOT = ZERO) AND (�I������N�v NOT = ZERO)
005070        COMPUTE �J�n����N�v = �J�n����N�v + �J�n�N�Q�v - 1
005080        COMPUTE �I������N�v = �I������N�v + �I���N�Q�v - 1
005090*
005100        IF �I������N�v =  �J�n����N�v
005110           PERFORM  �O����r��
005120*--- �������t�`�F�b�N ---*
005130           IF ( �v�Z���v = ZERO )
005140              MOVE 1 TO �v�Z���v
005150           END-IF
005160*
005170        ELSE
005180           IF  �I������N�v >  �J�n����N�v
005190               COMPUTE �v�Z�N�v = �I������N�v - �J�n����N�v
005200               COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
005210           ELSE
005220               MOVE ZERO TO �v�Z���v
005230           END-IF
005240        END-IF
005250     ELSE
005260        MOVE ZERO TO �v�Z���v
005270     END-IF.
005280*
005290*================================================================*
005300 �����f�[�^�e�Ǎ� SECTION.
005310*
005320     READ �����f�[�^�e NEXT
005330     AT END
005340         MOVE "YES" TO �I���t���O�Q
005350     END-READ.
005360*================================================================*
005370 �f�[�^�`�F�b�N SECTION.
005380*
005390     MOVE SPACE  TO ���s�L�[�v.
005400* *****************************************************************
005410* * �������ʗL���`�F�b�N�F���ʐ� = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
005420* *****************************************************************
005430     IF ���|���ʐ� NOT = ZERO
005440*    *************************************************************
005450*    * �{�p�L�^�`�F�b�N�F�ʉ@�� = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
005460*    *************************************************************
005470         MOVE ���|���Ҕԍ�  TO �{�L�|���Ҕԍ�
005480         MOVE ���|�}��      TO �{�L�|�}��
005490         MOVE ���|�{�p�a��  TO �{�L�|�{�p�a��
005500         MOVE ���|�{�p�N    TO �{�L�|�{�p�N
005510         MOVE ���|�{�p��    TO �{�L�|�{�p��
005520         MOVE ZERO          TO �{�L�|�{�p��
005530         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
005540                                      �{�L�|�{�p�a��N����
005550         END-START
005560         IF ��ԃL�[ = "00"
005570             MOVE SPACE TO �I���t���O�Q
005580             MOVE SPACE TO �{�p�L�^�L�v
005590             PERFORM �{�p�L�^�e�Ǎ�
005600             PERFORM UNTIL ( �I���t���O�Q         = "YES"          ) OR
005610                           ( �{�L�|���҃R�[�h NOT = ���|���҃R�[�h ) OR
005620                           ( �{�L�|�{�p�a��   NOT = ���|�{�p�a��   ) OR
005630                           ( �{�L�|�{�p�N     NOT = ���|�{�p�N     ) OR
005640                           ( �{�L�|�{�p��     NOT = ���|�{�p��     ) OR
005650                           ( �{�p�L�^�L�v         = "YES"          )
005660                 MOVE "YES"  TO �{�p�L�^�L�v
005670                 MOVE "YES"  TO ���s�L�[�v
005680             END-PERFORM
005690         ELSE
005700             MOVE SPACE  TO ���s�L�[�v
005710         END-IF
005720     ELSE
005730         MOVE SPACE  TO ���s�L�[�v
005740     END-IF.
005750*
005760*================================================================*
005770 �{�p�L�^�e�Ǎ� SECTION.
005780*
005790     READ �{�p�L�^�e NEXT
005800     AT END
005810         MOVE "YES" TO �I���t���O�Q
005820     END-READ.
005830*================================================================*
005840*================================================================*
005850 �ŗL�P���O�l���� SECTION.
005860*
005870     MOVE �A���J�Г��|�ŐV�{�p�a�� TO ���|�{�p�a��.
005880     MOVE �A���J�Г��|�ŐV�{�p�N   TO ���|�{�p�N.
005890     MOVE �A���J�Г��|�ŐV�{�p��   TO ���|�{�p��.
005900     MOVE �A���J�Г��|���Ҕԍ�     TO ���|���Ҕԍ�.
005910     MOVE �A���J�Г��|�}��         TO ���|�}��.
005920     READ �����f�[�^�e
005930     INVALID KEY
005940         MOVE NC"����" TO �t�@�C����
005950         PERFORM �G���[�\���q
005960     NOT INVALID KEY
005970         MOVE ZERO  TO ���ʃJ�E���^�Q
005980         MOVE SPACE TO ���ʕʗ����s�v�P
005990         INITIALIZE    ���ʕʗ����s�v�P
006000         PERFORM VARYING ���ʃJ�E���^ FROM 1 BY 1 UNTIL ���ʃJ�E���^ > ���|���ʐ�
006010             IF ���|�]�A�敪(���ʃJ�E���^) = 9
006020                 COMPUTE ���ʃJ�E���^�Q = ���ʃJ�E���^�Q + 1
006030                 MOVE �����|���ʕʗ���(���ʃJ�E���^) TO ���ʕʗ����s�v(���ʃJ�E���^�Q)
006040             END-IF
006050         END-PERFORM
006060         PERFORM VARYING ���ʃJ�E���^ FROM 1 BY 1 UNTIL ���ʃJ�E���^ > ���|���ʐ�
006070             MOVE ���ʕʗ����s�v(���ʃJ�E���^) TO �����|���ʕʗ���(���ʃJ�E���^)
006080         END-PERFORM
006090     END-READ.
006100*================================================================*
006110 �ŗL�P�������N���A SECTION.
006120*
006130     MOVE ZERO TO �����|������.
006140     MOVE ZERO TO �����|�������Z��.
006150     MOVE ZERO TO �����|�Č���.
006160     MOVE ZERO TO �����|�w���Ǘ���.
006170     MOVE ZERO TO �����|�^���Ö@��.
006180     MOVE ZERO TO �����|�{�p���񋟗�.
006190     MOVE ZERO TO �����|�{�p�ؖ�����.
006200     MOVE ZERO TO �����|�{�p���׏���.
006210     MOVE ZERO TO �����|��㪖@��.
006220     MOVE ZERO TO �����|��㪖@���R�O.
006230     MOVE ZERO TO �����|��㪖@���Q�R.
006240     MOVE ZERO TO �����|��㪖@���Q�S����.
006250     MOVE ZERO TO �����|��㪖@���Q�S��.
006260     MOVE ZERO TO �����|��㪖@���R�O.
006270     MOVE ZERO TO �����|�d�×��Q�R.
006280     MOVE ZERO TO �����|�d�×��Q�S����.
006290     MOVE ZERO TO �����|�d�×��Q�S��.
006300     MOVE ZERO TO �����|�d�×��R�O.
006310     MOVE ZERO TO �����|�^���Z��Ԋu.
006320     MOVE ZERO TO �����|�^�������.
006330     MOVE ZERO TO �����|�w���Z��Ԋu.
006340     MOVE ZERO TO �����|�w�������.
006350     MOVE ZERO TO �����|����敪.
006360     MOVE ZERO TO �����|���������k��.
006370     MOVE ZERO TO �����|������������.
006380     MOVE ZERO TO �����|�����ʒ������ʐ�.
006390     PERFORM VARYING ���ʃJ�E���^ FROM 1 BY 1 UNTIL ���ʃJ�E���^ > 7
006400         MOVE ZERO TO �����|���ʕʗ���(���ʃJ�E���^)
006410     END-PERFORM.
006420*================================================================*
006430******************************************************************
006440 END PROGRAM GETUJIK2.
006450******************************************************************
