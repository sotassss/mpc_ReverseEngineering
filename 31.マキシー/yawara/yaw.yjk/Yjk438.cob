000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJK438.
000060 AUTHOR.                 ���c ���a
000070*
000080*----------------------------------------------------------------*
000090*  ���{�_�����ω�p  �����s �����\�T�U
000100*  �����N���o�[�W���� 
000110*         MED = YJK430G 
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2012-09-03
000140 DATE-COMPILED.          2012-09-03
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
000880     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000881                             ORGANIZATION             IS  INDEXED
000882                             ACCESS MODE              IS  DYNAMIC
000883                             RECORD KEY               IS  ���|����敪
000884                             FILE STATUS              IS  ��ԃL�[
000885                             LOCK        MODE         IS  AUTOMATIC.
000886     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000887                             ORGANIZATION             IS  INDEXED
000888                             ACCESS MODE              IS  DYNAMIC
000889                             RECORD KEY               IS  ��|�{�p�a��N��
000890                                                          ��|���҃R�[�h
000891                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000892                                                          ��|���҃J�i
000893                                                          ��|���҃R�[�h
000894                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000895                                                          ��|�{�p�a��N��
000896                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000897                                                          ��|�ی����
000898                                                          ��|�ی��Ҕԍ�
000899                                                          ��|���҃R�[�h
000900                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000901                                                          ��|������
000902                                                          ��|��p���S�Ҕԍ�
000903                                                          ��|���҃R�[�h
000904                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000905                                                          ��|�������
000906                                                          ��|��p���S�Ҕԍ�����
000907                                                          ��|���҃R�[�h
000908                             ALTERNATE RECORD KEY  IS ��|�����a��N��
000909                                                      ��|�{�p�a��N��
000910                                                      ��|���҃R�[�h
000911                             FILE STATUS              IS  ��ԃL�[
000912                             LOCK        MODE         IS  AUTOMATIC.
000920     SELECT  ���Z�v�g�e      ASSIGN      TO        RECEPTL
000921                             ORGANIZATION             IS  INDEXED
000922                             ACCESS MODE              IS  DYNAMIC
000923                             RECORD KEY               IS  ���Z�|�{�p�a��N��
000924                                                          ���Z�|���҃R�[�h
000925                                                          ���Z�|���Z���
000926                             ALTERNATE RECORD KEY     IS  ���Z�|���҃R�[�h
000927                                                          ���Z�|�{�p�a��N��
000928                                                          ���Z�|���Z���
000929                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000930                                                          ���Z�|�{�p�a��N��
000931                                                          ���Z�|���҃R�[�h
000932                                                          ���Z�|���Z���
000933                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000934                                                          ���Z�|���Z���
000935                                                          ���Z�|�����ی��Ҕԍ�
000936                                                          ���Z�|���҃R�[�h
000937                                                          ���Z�|�{�p�a��N��
000938                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000939                                                          ���Z�|�����ی��Ҕԍ�
000940                                                          ���Z�|���҃R�[�h
000941                                                          ���Z�|���Z���
000942                                                          ���Z�|�{�p�a��N��
000943                             FILE STATUS              IS  ��ԃL�[
000944                             LOCK        MODE         IS  AUTOMATIC.
000980     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000981                             ORGANIZATION             IS  INDEXED
000982                             ACCESS MODE              IS  DYNAMIC
000983                             RECORD KEY               IS  �ہ|�ی����
000984                                                          �ہ|�ی��Ҕԍ�
000985                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000986                                                          �ہ|�ی��Җ���
000987                                                          �ہ|�ی��Ҕԍ�
000988                             FILE STATUS              IS  ��ԃL�[
000989                             LOCK        MODE         IS  AUTOMATIC.
000990     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000991                             ORGANIZATION             IS  INDEXED
000992                             ACCESS MODE              IS  DYNAMIC
000993                             RECORD KEY               IS  �s�|������
000994                                                          �s�|�s�����ԍ�
000995                             ALTERNATE RECORD KEY     IS  �s�|������
000996                                                          �s�|�s��������
000997                                                          �s�|�s�����ԍ�
000998                             FILE STATUS              IS  ��ԃL�[
000999                             LOCK        MODE         IS  AUTOMATIC.
001000     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
001001                             ORGANIZATION             IS  INDEXED
001002                             ACCESS MODE              IS  DYNAMIC
001010                             RECORD KEY               IS �{��|�{�p���ԍ�
001020                             FILE STATUS              IS  ��ԃL�[
001030                             LOCK        MODE         IS  AUTOMATIC.
001040     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
001050                             ORGANIZATION             IS  INDEXED
001060                             ACCESS MODE              IS  DYNAMIC
001070                             RECORD KEY               IS  ���|�����敪
001080                             FILE STATUS              IS  ��ԃL�[
001090                             LOCK        MODE         IS  AUTOMATIC.
001100     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W438.DAT"
001110                             ORGANIZATION             IS  INDEXED
001120                             ACCESS                   IS  DYNAMIC
001130                             RECORD      KEY          IS  ��P�|�������
001140                                                          ��P�|�ی��Ҕԍ�
001150                                                          ��P�|�ی����
001160                                                          ��P�|���҃R�[�h
001170                                                          ��P�|�{�p�a��N��
001180                             FILE        STATUS       IS  ��ԃL�[
001190                             LOCK        MODE         IS  AUTOMATIC.
001200     SELECT  ����t�@�C��    ASSIGN      TO         GS-PRTF001
001210                             SYMBOLIC    DESTINATION  IS "PRT"
001220                             FORMAT                   IS  ��`�̖��o
001230                             GROUP                    IS  ���ڌQ���o
001240                             PROCESSING  MODE         IS  ������ʂo
001250                             UNIT        CONTROL      IS  �g������o
001260                             FILE        STATUS       IS  �ʒm���o.
001270     SELECT  ����t�@�C���Q  ASSIGN      TO         GS-PRTF001
001280                             SYMBOLIC    DESTINATION  IS "PRT"
001290                             FORMAT                   IS  ��`�̖��o
001300                             GROUP                    IS  ���ڌQ���o
001310                             PROCESSING  MODE         IS  ������ʂo
001320                             UNIT        CONTROL      IS  �g������o
001330                             FILE        STATUS       IS  �ʒm���o.
001340******************************************************************
001350*                      DATA DIVISION                             *
001360******************************************************************
001370 DATA                    DIVISION.
001380 FILE                    SECTION.
001540*                           �m�q�k��  �Q�T�U�n
001541 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001542     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001543*                           �m�q�k��  �R�Q�O�n
001544 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001545     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001549*
001550 FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
001551     COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001570*                           �m�q�k��  �R�Q�O�n
001571 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001572     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001573*                           �m�q�k��  �Q�T�U�n
001574 FD  �s�����}�X�^        BLOCK   CONTAINS   1   RECORDS.
001575     COPY SITYOSN         OF  XFDLIB  JOINING   �s   AS  PREFIX.
001576*                           �m�q�k��  �P�Q�W�n
001580 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001590     COPY SEJOHO          OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001600*                           �m�q�k��  �P�Q�W�n
001610 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001620     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001630* 
001640 FD  ��ƃt�@�C���P RECORD  CONTAINS 200 CHARACTERS.
001650 01  ��P�|���R�[�h.
001660     03  ��P�|���R�[�h�L�[.
001670         05  ��P�|�������                  PIC 9(2).
001680         05  ��P�|�ی��Ҕԍ�                PIC X(10).
001690         05  ��P�|�ی����                  PIC 9(2).
001700         05  ��P�|���҃R�[�h.
001710             07 ��P�|���Ҕԍ�               PIC 9(6).
001720             07 ��P�|�}��                   PIC X(1).
001730         05  ��P�|�{�p�a��N��.
001740             07  ��P�|�{�p�a��              PIC 9.
001750             07  ��P�|�{�p�N                PIC 9(2).
001760             07  ��P�|�{�p��                PIC 9(2).
001770     03  ��P�|���R�[�h�f�[�^.
001780         05  ��P�|��p�z                    PIC 9(7).
001790         05  ��P�|���S�z                    PIC 9(7).
001800         05  ��P�|�����z                    PIC 9(7).
001810*
001820 FD  ����t�@�C��.
001830     COPY YJK4371P         OF  XMDLIB.
001840*
001850 FD  ����t�@�C���Q.
001860     COPY YJK4362P         OF  XMDLIB JOINING �V�� AS  PREFIX.
001870*----------------------------------------------------------------*
001880******************************************************************
001890*                WORKING-STORAGE SECTION                         *
001900******************************************************************
001910 WORKING-STORAGE         SECTION.
001920 01 �L�[����                           PIC X     VALUE SPACE.
001930 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
001940 01 �I���t���O                         PIC X(3)  VALUE SPACE.
001950 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
001960 01 �쐬�t���O                         PIC X(3)  VALUE SPACE.
001970 01 ���s�L�[�v                         PIC X(3)  VALUE SPACE.
001980 01 �{�p�L�^�L�v                       PIC X(3)  VALUE SPACE.
001990*
002000 01 �ی���ʂv�q                       PIC 9(2)  VALUE ZERO.
002010 01 ���Z�v�g��ނv�q                   PIC X(4)  VALUE SPACE.
002020 01 �ی��Ҕԍ��v�q                     PIC X(10) VALUE SPACE.
002030 01 �{�l�Ƒ��敪�v�q                   PIC 9     VALUE ZERO.
002040 01 �����a��N���v�q.
002050    03 �����a��v�q                    PIC 9     VALUE ZERO.
002060    03 �����N�v�q                      PIC 9(2)  VALUE ZERO.
002070    03 �������v�q                      PIC 9(2)  VALUE ZERO.
002080*
002090 01 �����N���v.
002100    03 �����a��v                      PIC 9(1)  VALUE ZERO.
002110    03 �����N�v                        PIC 9(2)  VALUE ZERO.
002120    03 �������v                        PIC 9(2)  VALUE ZERO.
002130 01 �{�p�N���v.
002140    03 �{�p�a��v                      PIC 9(1)  VALUE ZERO.
002150    03 �{�p�N�v                        PIC 9(2)  VALUE ZERO.
002160    03 �{�p���v                        PIC 9(2)  VALUE ZERO.
002170 01 �W�v��Ɨp�v.
002180    03 �����v                          PIC 9(4)  VALUE ZERO.
002190    03 ���ʐ��v                        PIC 9(5)  VALUE ZERO.
002200    03 �ʉ@���v                        PIC 9(4)  VALUE ZERO.
002210    03 ��p�z�v                        PIC 9(9)  VALUE ZERO.
002220    03 ���S�z�v                        PIC 9(9)  VALUE ZERO.
002230    03 �����z�v                        PIC 9(9)  VALUE ZERO.
002240 01 �{�l�Ƒ��敪�v                     PIC 9(1)  VALUE ZERO.
002250 01 �t�@�C����                         PIC N(2)  VALUE SPACE.
002260 01 �ی���ʂv                         PIC 9(2)  VALUE ZERO.
002270 01 �ی��Ҕԍ��v                       PIC X(10) VALUE SPACE.
002280 01 �ی��Ҕԍ��Q�v                     PIC X(10) VALUE SPACE.
002290 01 �e�ی���ʂv                       PIC 9(2)  VALUE ZERO.
002300*
002310* �{�l/�Ƒ��W�v�p
002320 01 �{�l�Ƒ��W�v�p�v.
002330    03 �{�l�����v                      PIC 9(3)  VALUE ZERO.
002340    03 �{�l��p�z�v                    PIC 9(7)  VALUE ZERO.
002350    03 �{�l���S�z�v                    PIC 9(7)  VALUE ZERO.
002360    03 �{�l�����z�v                    PIC 9(7)  VALUE ZERO.
002370    03 �Ƒ������v                      PIC 9(3)  VALUE ZERO.
002380    03 �Ƒ���p�z�v                    PIC 9(7)  VALUE ZERO.
002390    03 �Ƒ����S�z�v                    PIC 9(7)  VALUE ZERO.
002400    03 �Ƒ������z�v                    PIC 9(7)  VALUE ZERO.
002410*
002420* �������O�p
002430 01 �������O�v�q                       PIC 9     VALUE ZERO.
002440 01 ���i�h�r�v�j                       PIC X(2)  VALUE SPACE.
002450 01 ���ǂi�h�r�v�j                     PIC X(2)  VALUE SPACE.
002460 01 �������O�`�F�b�N                   PIC 9     VALUE ZERO.
002470 01 ���v�j                             PIC X(2)  VALUE SPACE.
002480 01 ���v�j�Q                           PIC X(2)  VALUE SPACE.
002490 01 ������v�j                         PIC X(2)  VALUE SPACE.
002500 01 �ی���ʌ��p�v�j                   PIC 9(2)  VALUE ZERO.
002510*
002520* �G���[���b�Z�[�W�p
002530 01 �G���[���b�Z�[�W�v.
002540    03 �G���[���҃R�[�h�v              PIC X(7) VALUE SPACE.
002550    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
002560    03 �G���[�ی���ʂv                PIC X(2) VALUE SPACE.
002570    03 FILLER                          PIC X(10) VALUE SPACE.
002580* �����p
002590 01 ����ی���ʂv                     PIC 9(2)  VALUE ZERO.
002600 01 ��������v                         PIC 9(2)  VALUE ZERO.
002610*
002620** �������Z�܂Ƃߗp
002630 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
002640*
002650* ��O�p
002660 01 ����R�[�h�v                       PIC 9(2)  VALUE ZERO.
002670*
002680 01 �㍂��\�ԍ��v                     PIC X(10) VALUE SPACE.
002690 01 �����於�̂v.
002700    05 �����於�̂P�v                  PIC X(20) VALUE SPACE.
002710    05 �����於�̂Q�v                  PIC X(20) VALUE SPACE.
002720    05 �����於�̂R�v                  PIC X(20) VALUE SPACE.
002730 01 �łv                               PIC 9(2) VALUE ZERO.
002740 01 �s�J�E���^                         PIC 9(2) VALUE ZERO.
002750
002751 01 �I�[�v���t���O                     PIC X(3)  VALUE SPACE.
002752 01 �I�[�v���t���O�Q                   PIC X(3)  VALUE SPACE.
002753*
002760 01 ���v�v.
002770    03 ���ی������v�v                  PIC 9(4) VALUE ZERO.
002780    03 ���۔�p�z���v�v                PIC 9(8) VALUE ZERO.
002790    03 �ސE�������v�v                  PIC 9(4) VALUE ZERO.
002800    03 �ސE��p�z���v�v                PIC 9(8) VALUE ZERO.
002810    03 �V���������v�v                  PIC 9(4) VALUE ZERO.
002820    03 �V����p�z���v�v                PIC 9(8) VALUE ZERO.
002830    03 ����������v�v                  PIC 9(4) VALUE ZERO.
002840    03 �����p�z���v�v                PIC 9(8) VALUE ZERO.
002850    03 �ی��҈����v.
002860       05 �ی��҈����P�v               PIC X(40) VALUE SPACE.
002870       05 �ی��҈����Q�v               PIC X(40) VALUE SPACE.
002880**************
002890* �{�p����� *
002900**************
002910 01 �{�p�����v.
002920    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
002930    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
002940    03 �_���t�ԍ��v                    PIC X(20)  VALUE SPACE.
002950    03 �{�p���Z���v.
002960       05 �{�p���Z���P�v               PIC X(40)  VALUE SPACE.
002970       05 �{�p���Z���Q�v               PIC X(40)  VALUE SPACE.
002980    03 �{�p���X�֔ԍ��v.
002990       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
003000       05 �{�p���X�֔ԍ���؂v         PIC X(1)   VALUE SPACE.
003010       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
003020    03 �{�p���d�b�ԍ��v                PIC X(15)  VALUE SPACE.
003030    03 �������v.
003040        05 ������s���v              PIC X(40)  VALUE SPACE.
003050        05 ������s�x�X���v          PIC X(40)  VALUE SPACE.
003060        05 �a����ʂv                  PIC 9(1)   VALUE ZERO.
003070        05 ��s�ԍ��v                  PIC X(4)   VALUE SPACE.
003080        05 �X�ԍ��v                    PIC X(3)   VALUE SPACE.
003090        05 �����ԍ��v                  PIC X(10)  VALUE SPACE.
003100        05 �������`�l�J�i�v            PIC X(40)  VALUE SPACE.
003110        05 �������`�l�v                PIC X(40)  VALUE SPACE.
003120 01 ��s���x�X���v                     PIC X(40)  VALUE SPACE.
003130 01 �a����ʃR�����g�v                 PIC N(2)   VALUE SPACE.
003140*
003150 01 �������.
003160     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
003170     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
003180     03 ������ʂo                     PIC X(2) VALUE SPACE.
003190     03 �g������o.
003200         05 �[������o.
003210             07 �ړ������o             PIC X(1) VALUE SPACE.
003220             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
003230         05 �ڍא���o                 PIC X(2) VALUE SPACE.
003240     03 �ʒm���o                     PIC X(2) VALUE SPACE.
003250     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
003260******************************************************************
003270*                          �A������                              *
003280******************************************************************
003290*
003300****************
003310* ��ʓ��͏�� *
003320****************
003330**
003340 01 �A���|��ʏ��x�i�j�S�R�O   IS EXTERNAL.
003350    03 �A���|�����N��.
003360       05 �A���|�����a��               PIC 9.
003370       05 �A���|�����N                 PIC 9(2).
003380       05 �A���|������                 PIC 9(2).
003390    03 �A���|��o�N����.
003400       05 �A���|��o�a��               PIC 9.
003410       05 �A���|��o�N                 PIC 9(2).
003420       05 �A���|��o��                 PIC 9(2).
003430       05 �A���|��o��                 PIC 9(2).
003440    03 �A���|���Z�v�g���              PIC X(4).
003450    03 �A���|�ی����                  PIC 9(2).
003460    03 �A���|������                  PIC 9.
003470    03 �A���|�{�l�Ƒ�                  PIC 9.
003480    03 �A���|�p�����                  PIC 9.
003490    03 �A���|�������O                  PIC 9.
003500    03 �A���|���i�h�r                  PIC X(2).
003510    03 �A���|���ǂi�h�r                PIC X(2).
003520*
003521 01 �A���|��ʏ��x�i�j�S�R�O�ǉ�   IS EXTERNAL.
003522    03 �A���|�ꊇ�敪                  PIC 9.
003523    03 �A���|�v���r���[�敪            PIC 9.
003524*
003525*
003530********************
003540* ���b�Z�[�W�\���L�[ *
003550********************
003560 01 �A���|�L�[ IS EXTERNAL.
003570    03  �A���|���b�Z�[�W                 PIC N(20).
003580*
003920********************
003930* ���b�Z�[�W�\���L�[ *
003940********************
003950 01 �A���R�|�L�[ IS EXTERNAL.
003960    03  �A���R�|���b�Z�[�W             PIC N(20).
003970    03  �A���R�|���b�Z�[�W�P           PIC X(20).
003980*
003990************************
004000* �������Z�܂Ƃ�
004010************************
004020 01 �A���Z�܂Ƃ߁|�L�[ IS EXTERNAL.
004030    03 �A���Z�܂Ƃ߁|�{�p�a��N��.
004040       05 �A���Z�܂Ƃ߁|�{�p�a��               PIC 9.
004050       05 �A���Z�܂Ƃ߁|�{�p�N��.
004060          07 �A���Z�܂Ƃ߁|�{�p�N              PIC 9(2).
004070          07 �A���Z�܂Ƃ߁|�{�p��              PIC 9(2).
004080    03 �A���Z�܂Ƃ߁|���҃R�[�h.
004090       05 �A���Z�܂Ƃ߁|���Ҕԍ�               PIC 9(6).
004100       05 �A���Z�܂Ƃ߁|�}��                   PIC X(1).
004110**-------------------------------------------------------**
004120*   1:�������Z�v�g�Ȃ��̖{�̂܂Ƃ߂̔���
004130*   2:���l�E���p�̎Еۏ������Z���̔���
004140    03 �A���Z�܂Ƃ߁|����敪                  PIC 9.
004150**-------------------------------------------------------**
004160*  / OUT /�@ 0:�ΏۊO�A1:�Ώ�
004170    03 �A���Z�܂Ƃ߁|���茋��                  PIC 9.
004180**
004195************************************
004196* �v�����^�t�@�C���쐬�p           *
004197************************************
004198 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
004199     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
004200     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
004201     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
004202     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
004203*
004205 01 �A�ی��Ҕԍ��ϊ��|�L�[ IS EXTERNAL.
004206    03 �A�ی��Ҕԍ��ϊ��|�{�p�a��N��.
004207       05 �A�ی��Ҕԍ��ϊ��|�{�p�a��               PIC 9.
004208       05 �A�ی��Ҕԍ��ϊ��|�{�p�N��.
004209          07 �A�ی��Ҕԍ��ϊ��|�{�p�N              PIC 9(2).
004210          07 �A�ی��Ҕԍ��ϊ��|�{�p��              PIC 9(2).
004211    03 �A�ی��Ҕԍ��ϊ��|�ی����                  PIC 9(2).
004212    03 �A�ی��Ҕԍ��ϊ��|���ی��Ҕԍ�              PIC X(10).
004213* 0:�ʏ�i�{�p�N���֌W�Ȃ��j
004214    03 �A�ی��Ҕԍ��ϊ��|�ďo�敪                  PIC 9(2).
004215*  / OUT /
004216    03 �A�ی��Ҕԍ��ϊ��|��ی��Ҕԍ�              PIC X(10).
004217*
004218******************************************************************
004219*                      PROCEDURE  DIVISION                       *
004220******************************************************************
004221 PROCEDURE               DIVISION.
004230************
004240*           *
004250* ��������   *
004260*           *
004270************
004280     PERFORM �v�����^�t�@�C���쐬.
004281     PERFORM �v�����^�t�@�C���쐬�Q.
004282     PERFORM �t�@�C���I�[�v��.
004290     PERFORM �A�����ڑҔ�.
004300     PERFORM ������擾.
004310************
004320*           *
004330* �又��     *
004340*           *
004350************
004360     PERFORM ��ƃt�@�C���쐬.
004370     PERFORM �U���������.
004380     PERFORM �V���������.
004390************
004400*           *
004410* �I������   *
004420*           *
004430************
004440     PERFORM �I������.
004450     MOVE ZERO TO PROGRAM-STATUS.
004460     EXIT PROGRAM.
004470*
004480*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
004490*================================================================*
004491 �v�����^�t�@�C���쐬 SECTION.
004492*================================================================*
004493*   / ������ /
004494     MOVE SPACE TO �g�A�o�q�s�e�|�쐬�f�[�^.
004495     INITIALIZE �g�A�o�q�s�e�|�쐬�f�[�^.
004496*
004497*
004498*--���� �ύX�ӏ� ����--------------------------------------*
004499*   �g�p����v�����^�t�@�C�����Z�b�g
004500     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
004501*
004502*   �g�p���钠�[�v���O�������Z�b�g
004503     MOVE "YJK4371P"              TO �g�A�o�q�s�e�|���[�v���O������.
004504*
004505*--����-----------------------------------------------------*
004506*
004507*   / �v���r���[�敪�Z�b�g /
004508     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
004509*
004510     CALL   "CRTPRTF".
004511     CANCEL "CRTPRTF".
004512*
004513*================================================================*
004514 �v�����^�t�@�C���쐬�Q SECTION.
004515*================================================================*
004516*   / ������ /
004517     MOVE SPACE TO �g�A�o�q�s�e�|�쐬�f�[�^.
004518     INITIALIZE �g�A�o�q�s�e�|�쐬�f�[�^.
004519*
004520*
004521*--���� �ύX�ӏ� ����--------------------------------------*
004522*   �g�p����v�����^�t�@�C�����Z�b�g
004523     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
004524*
004525*   �g�p���钠�[�v���O�������Z�b�g
004526     MOVE "YJK4362"             TO �g�A�o�q�s�e�|���[�v���O������.
004527*
004528*--����-----------------------------------------------------*
004529*
004530*   / �v���r���[�敪�Z�b�g /
004531     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
004532*
004533     CALL   "CRTPRTF".
004534     CANCEL "CRTPRTF".
004535*
004536*================================================================*
004537 �A�����ڑҔ� SECTION.
004538*
004539* �A�����ڂ̑Ҕ�
004540     MOVE �A���|�����a��      TO �����a��v�q.
004541     MOVE �A���|�����N        TO �����N�v�q.
004550     MOVE �A���|������        TO �������v�q.
004560     MOVE �A���|���Z�v�g���  TO ���Z�v�g��ނv�q.
004570     MOVE �A���|�ی����      TO �ی���ʂv�q.
004580     MOVE �A���|�{�l�Ƒ�      TO �{�l�Ƒ��敪�v�q.
004590     MOVE �A���|�������O      TO �������O�v�q.
004600     MOVE �A���|���i�h�r      TO ���i�h�r�v�j.
004610     MOVE �A���|���ǂi�h�r    TO ���ǂi�h�r�v�j.
004620*
004630*================================================================*
004640 �t�@�C���I�[�v�� SECTION.
004650*
004660     OPEN INPUT ������}�X�^.
004670         MOVE NC"������" TO �t�@�C����.
004680         PERFORM �I�[�v���`�F�b�N.
004690     OPEN INPUT ��f�ҏ��e.
004700         MOVE NC"��f�ҏ��e" TO �t�@�C����.
004710         PERFORM �I�[�v���`�F�b�N.
004840     OPEN INPUT ���Z�v�g�e.
004841         MOVE NC"���Z�v�g�e" TO �t�@�C����.
004842         PERFORM �I�[�v���`�F�b�N.
004843     OPEN INPUT �ی��҃}�X�^
004844         MOVE NC"�ی�" TO �t�@�C����.
004845         PERFORM �I�[�v���`�F�b�N.
004846     OPEN INPUT �s�����}�X�^
004847         MOVE NC"�s��" TO �t�@�C����.
004848         PERFORM �I�[�v���`�F�b�N.
004849     OPEN INPUT �{�p�����}�X�^
004850         MOVE NC"�{��" TO �t�@�C����.
004860         PERFORM �I�[�v���`�F�b�N.
004870     OPEN INPUT �����}�X�^.
004880         MOVE NC"����" TO �t�@�C����.
004890         PERFORM �I�[�v���`�F�b�N.
004903     PERFORM �{�p�����擾.
004910*================================================================*
004920 �I�[�v���`�F�b�N SECTION.
004930*
004940     IF ��ԃL�[  NOT =  "00"
004950         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
004960         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
004970         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004980                                                 UPON CONS
004990         ACCEPT  �L�[���� FROM CONS
005000         PERFORM �t�@�C����
005010         MOVE 99 TO PROGRAM-STATUS
005020         EXIT PROGRAM.
005030*================================================================*
005040 ������擾 SECTION.
005050*
005060     MOVE ZEROS TO ���|����敪.
005070     READ ������}�X�^
005080     NOT INVALID KEY
005090         MOVE ���|����R�[�h   TO ����R�[�h�v
005100     END-READ.
005110*
005120*================================================================*
005130 �G���[�\�� SECTION.
005140*
005150     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
005160     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
005170     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
005180     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
005190     ACCEPT  �L�[���� FROM CONS.
005200     PERFORM �t�@�C����.
005210     MOVE 99 TO PROGRAM-STATUS.
005220     EXIT PROGRAM.
005230*================================================================*
005240 ��f�ҏ��e�Ǎ� SECTION.
005250*
005260     READ ��f�ҏ��e NEXT
005270     AT END
005280         MOVE "YES" TO �I���t���O
005290     END-READ.
005300*================================================================*
005310 ���Z�v�g�e�Ǎ� SECTION.
005320*
005330     READ ���Z�v�g�e NEXT
005340     AT END
005350         MOVE "YES" TO �I���t���O
005360     END-READ.
005370*
005380*================================================================*
005790 �f�[�^�`�F�b�N SECTION.
005791*
005792     MOVE SPACE          TO ���s�L�[�v.
005793     IF ( ���Z�|���Z����Ώۋ敪 NOT = 1 )
005794        MOVE "YES"  TO ���s�L�[�v
005795     END-IF.
005796* *****************************************************************
005797* * ���Z�v�g�e�̐����Ώۋ敪 = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
005798* *****************************************************************
005799     IF ���s�L�[�v  = "YES"
005800*      (�ēx�A���s�L�[�v SPACE)
005801        MOVE SPACE  TO ���s�L�[�v
005802        IF ���Z�|�����Ώۋ敪 NOT = ZERO
005803           MOVE ���Z�|�{�p�a��  TO ��|�{�p�a��
005804           MOVE ���Z�|�{�p�N    TO ��|�{�p�N
005805           MOVE ���Z�|�{�p��    TO ��|�{�p��
005806           MOVE ���Z�|���Ҕԍ�  TO ��|���Ҕԍ�
005807           MOVE ���Z�|�}��      TO ��|�}��
005808           READ ��f�ҏ��e
005809           NOT INVALID KEY
005810              MOVE "YES"  TO ���s�L�[�v
005811*
005812*/����ۂ̑Ή��A���ی��Ҕԍ��ł��V�ی��Ҕԍ��Ƃ܂Ƃ߂Đ�������/081016
005813              INITIALIZE �A�ی��Ҕԍ��ϊ��|�L�[
005814              MOVE  ��|�{�p�a��N��  TO �A�ی��Ҕԍ��ϊ��|�{�p�a��N��
005815              MOVE  ��|�ی����      TO �A�ی��Ҕԍ��ϊ��|�ی����
005816              MOVE  ��|�ی��Ҕԍ�    TO �A�ی��Ҕԍ��ϊ��|���ی��Ҕԍ�
005817              MOVE  ZERO              TO �A�ی��Ҕԍ��ϊ��|�ďo�敪
005818              CALL   "CHGHOKNO"
005819              CANCEL "CHGHOKNO"
005820*   / �u���� /
005821              MOVE �A�ی��Ҕԍ��ϊ��|��ی��Ҕԍ�  TO  ��|�ی��Ҕԍ�
005822*
005823           END-READ
005824        ELSE
005825           MOVE SPACE  TO ���s�L�[�v
005826        END-IF
005827     END-IF.
005828*
006670*================================================================*
006671 ���ۗ����v�Z SECTION.
006672*
006673     MOVE ���Z�|���v         TO ��P�|��p�z.
006674     MOVE ���Z�|�ꕔ���S��   TO ��P�|���S�z.
006675     MOVE ���Z�|�������z     TO ��P�|�����z.
006676*
006677* ����
006678*     PERFORM �������Z����.
006679*     IF �������Z�܂Ƃ߃t���O = "YES"
006680*         MOVE ���Z�|�󋋎ҕ��S�z  TO ��P�|���S�z
006681*         COMPUTE ��P�|�����z = ��P�|��p�z - ��P�|���S�z
006682*     END-IF.
006683*
006684*================================================================*
006685 �V�l�����v�Z SECTION.
006686*
006687     MOVE ���Z�|���v        TO ��P�|��p�z.
006688     MOVE ���Z�|�ꕔ���S��  TO ��P�|���S�z.
006689     MOVE ���Z�|�������z    TO ��P�|�����z.
006690*
006691* ����
006692*     PERFORM �������Z����.
006693*     IF �������Z�܂Ƃ߃t���O = "YES"
006694*         MOVE ���Z�|�󋋎ҕ��S�z  TO ��P�|���S�z
006695*         COMPUTE ��P�|�����z = ��P�|��p�z - ��P�|���S�z
006696*     END-IF.
006697*
006698*================================================================*
006699 ���������v�Z SECTION.
006700*
006701     MOVE ���Z�|���v         TO ��P�|��p�z.
006702     MOVE ���Z�|�󋋎ҕ��S�z TO ��P�|���S�z.
006703     MOVE ���Z�|�����������z TO ��P�|�����z.
006704*
006705* ����
006706*     PERFORM �������Z����.
006707*     IF �������Z�܂Ƃ߃t���O = "YES"
006708*   / �������܂Ȃ��悤�ɐ����z�[���ɂ��� /
006709*        MOVE ZERO TO ��P�|�����z
006710*     END-IF.
006711*
006712*================================================================*
006713 �t�@�C������ SECTION.
006714*
006715     WRITE ��P�|���R�[�h
006716     INVALID KEY
006720         MOVE NC"���"  TO �t�@�C����
006730         PERFORM �G���[�\��
006740     END-WRITE.
006750*================================================================*
006760 ��ƃt�@�C���P�Ǎ� SECTION.
006770*
006780     READ ��ƃt�@�C���P NEXT
006790     AT END
006800         MOVE "YES" TO �I���t���O
006810     END-READ.
006820*================================================================*
006830 �t�@�C���� SECTION.
006840*
006850     IF ( �I�[�v���t���O = "YES" )
006851         CLOSE ����t�@�C��
006852     END-IF.
006853     IF ( �I�[�v���t���O�Q = "YES" )
006854         CLOSE ����t�@�C���Q
006855     END-IF.
006856     CLOSE ������}�X�^ ��f�ҏ��e �ی��҃}�X�^ �s�����}�X�^
006860           �{�p�����}�X�^ �����}�X�^.
006870*================================================================*
006880 �I������ SECTION.
006890*
006900     PERFORM �t�@�C����.
006910*================================================================*
006920 �������O�擾 SECTION.
006930*
006940      MOVE ZERO  TO  �������O�`�F�b�N ���v�j.
006950      EVALUATE  �ی���ʌ��p�v�j
006960* ����
006970      WHEN  01
006980         IF ��|�ی��Ҕԍ�(1:2) = ���i�h�r�v�j
006990            MOVE  1  TO  �������O�`�F�b�N
007000         END-IF
007010         MOVE ��|�ی��Ҕԍ�(1:2) TO ���v�j
007020* �Е�
007030      WHEN  02
007040         IF ��|�ی��Ҕԍ�(1:2) = ���ǂi�h�r�v�j
007050            MOVE  1  TO  �������O�`�F�b�N
007060         END-IF
007070         MOVE ��|�ی��Ҕԍ�(1:2) TO ���v�j
007080         PERFORM ������
007090         MOVE ������v�j TO ���v�j
007100* �D��
007110* ����
007120* �g��
007130* ����
007140* ���q��
007150* �ސE����
007160      WHEN  06
007170      WHEN  07
007180      WHEN  08
007190      WHEN  09
007200      WHEN  04
007210      WHEN  03
007220         IF ��|�ی��Ҕԍ�(3:2) = ���i�h�r�v�j
007230            MOVE  1  TO  �������O�`�F�b�N
007240         END-IF
007250         MOVE ��|�ی��Ҕԍ�(3:2) TO ���v�j
007260* ����
007270      WHEN 50 THRU 60
007280         IF ��|��p���S�Ҕԍ�����(3:2) = ���i�h�r�v�j
007290            MOVE  1  TO  �������O�`�F�b�N
007300         END-IF
007310         MOVE ��|��p���S�Ҕԍ�����(3:2) TO ���v�j
007320* �V�l
007330      WHEN  05
007340         IF ��|��p���S�Ҕԍ�(3:2) = ���i�h�r�v�j
007350            MOVE  1  TO  �������O�`�F�b�N
007360         END-IF
007370         MOVE ��|��p���S�Ҕԍ�(3:2) TO ���v�j
007380* �J�ЁA�����ӁE����E���ےP�Ƃ͏��O
007390      WHEN  70
007400      WHEN  80
007410      WHEN  90
007420      WHEN  ZERO
007430         CONTINUE
007440*
007450     WHEN OTHER
007460         MOVE SPACE                TO �G���[���b�Z�[�W�v
007470         MOVE ��|���҃R�[�h       TO �G���[���҃R�[�h�v
007480         MOVE �ی���ʌ��p�v�j     TO �G���[�ی���ʂv
007490         MOVE "-"                  TO �G���[��؂�v
007500         MOVE  NC"�������O�擾�G���[�B���Ҕԍ�" TO �A���R�|���b�Z�[�W
007510         MOVE  �G���[���b�Z�[�W�v  TO �A���R�|���b�Z�[�W�P
007520         CALL   "MSG003"
007530         CANCEL "MSG003"
007540     END-EVALUATE.
007550*
007560*================================================================*
007570 ������ SECTION.
007580*
007590     MOVE ZERO  TO  ������v�j.
007600     EVALUATE  ���v�j
007610*�k�C��
007620      WHEN  01
007630         MOVE 01 TO ������v�j
007640*�X
007650      WHEN  02
007660         MOVE 02 TO ������v�j
007670*���
007680      WHEN  03
007690         MOVE 03 TO ������v�j
007700*�{��
007710      WHEN  04
007720         MOVE 04 TO ������v�j
007730*�H�c
007740      WHEN  05
007750         MOVE 05 TO ������v�j
007760*�R�`
007770      WHEN  06
007780         MOVE 06 TO ������v�j
007790*����
007800      WHEN  07
007810         MOVE 07 TO ������v�j
007820*���
007830      WHEN  08
007840         MOVE 08 TO ������v�j
007850*�Ȗ�
007860      WHEN  09
007870         MOVE 09 TO ������v�j
007880*�Q�n
007890      WHEN  10
007900         MOVE 10 TO ������v�j
007910*���
007920      WHEN  11
007930         MOVE 11 TO ������v�j
007940*��t
007950      WHEN  12
007960         MOVE 12 TO ������v�j
007970*����
007980      WHEN  21
007990         MOVE 13 TO ������v�j
008000*�_�ސ�
008010      WHEN  31
008020         MOVE 14 TO ������v�j
008030*�V��
008040      WHEN  32
008050         MOVE 15 TO ������v�j
008060*�x�R
008070      WHEN  33
008080         MOVE 16 TO ������v�j
008090*�ΐ�
008100      WHEN  34
008110         MOVE 17 TO ������v�j
008120*����
008130      WHEN  35
008140         MOVE 18 TO ������v�j
008150*�R��
008160      WHEN  36
008170         MOVE 19 TO ������v�j
008180*����
008190      WHEN  37
008200         MOVE 20 TO ������v�j
008210*��
008220      WHEN  38
008230         MOVE 21 TO ������v�j
008240*�É�
008250      WHEN  39
008260         MOVE 22 TO ������v�j
008270*���m
008280      WHEN  51
008290         MOVE 23 TO ������v�j
008300*�O�d
008310      WHEN  52
008320         MOVE 24 TO ������v�j
008330*����
008340      WHEN  53
008350         MOVE 25 TO ������v�j
008360*���s
008370      WHEN  54
008380         MOVE 26 TO ������v�j
008390*���
008400      WHEN  41
008410         MOVE 27 TO ������v�j
008420*����
008430      WHEN  42
008440         MOVE 28 TO ������v�j
008450*�ޗ�
008460      WHEN  55
008470         MOVE 29 TO ������v�j
008480*�a�̎R
008490      WHEN  56
008500         MOVE 30 TO ������v�j
008510*����
008520      WHEN  57
008530         MOVE 31 TO ������v�j
008540*����
008550      WHEN  58
008560         MOVE 32 TO ������v�j
008570*���R
008580      WHEN  59
008590         MOVE 33 TO ������v�j
008600*�L��
008610      WHEN  60
008620         MOVE 34 TO ������v�j
008630*�R��
008640      WHEN  61
008650         MOVE 35 TO ������v�j
008660*����
008670      WHEN  71
008680         MOVE 36 TO ������v�j
008690*����
008700      WHEN  72
008710         MOVE 37 TO ������v�j
008720*���Q
008730      WHEN  73
008740         MOVE 38 TO ������v�j
008750*���m
008760      WHEN  74
008770         MOVE 39 TO ������v�j
008780*����
008790      WHEN  75
008800         MOVE 40 TO ������v�j
008810*����
008820      WHEN  76
008830         MOVE 41 TO ������v�j
008840*����
008850      WHEN  77
008860         MOVE 42 TO ������v�j
008870*�F�{
008880      WHEN  78
008890         MOVE 43 TO ������v�j
008900*�啪
008910      WHEN  79
008920         MOVE 44 TO ������v�j
008930*�{��
008940      WHEN  80
008950         MOVE 45 TO ������v�j
008960*������
008970      WHEN  81
008980         MOVE 46 TO ������v�j
008990*����
009000      WHEN  82
009010         MOVE 47 TO ������v�j
009020*���̑�
009030      WHEN  OTHER
009040         CONTINUE
009050     END-EVALUATE.
009060*
009580*================================================================*
009581 �������Z���� SECTION.
009582* ���ʏ����i�������Z�܂Ƃ߁j
009583     MOVE SPACE TO �������Z�܂Ƃ߃t���O.
009584*
009585     IF ��|������� NOT = ZERO  
009586*/��������ǉ�/111007
009587            IF ( ��|�ی���� = 01 OR 08 OR 05)
009588                PERFORM �������Z�܂Ƃߔ���
009589            END-IF
009590     END-IF.
009591*
009592*================================================================*
009593*================================================================*
009594 �������Z�܂Ƃߔ��� SECTION.
009595* ���ʏ����i�������Z�܂Ƃ߁j
009596     MOVE SPACE TO �������Z�܂Ƃ߃t���O.
009597*
009603     IF ( ���Z�|�{�̂܂Ƃߋ敪 = 1 )
009604           MOVE "YES" TO �������Z�܂Ƃ߃t���O
009605     END-IF.
009606*
009607*================================================================*
009608*================================================================*
009609 ��O����  SECTION.
009610*
009620* ����Ώۂ̗�O�̒�`
009630*
009640* �@�����_�� 27 �� �Е�(02,06,07)�A�g��(03)�A����(04,09)�A
009650*                �@����(��������133264,�S���y��133033) �͕Ԗߕ�������
009660*
009670*      IF ����R�[�h�v = 27
009680*          IF ( ��|�����敪 = 2 ) AND ( ��|������  = ZERO  )
009690*             IF  ( ��|�ی����  = 02 OR 03 OR 04 OR 06 OR 07 OR 09 ) OR
009700*                 (( ��|�ی����  = 01 ) AND ( ��|�ی��Ҕԍ� = "133264")) OR
009710*                 (( ��|�ی����  = 01 ) AND ( ��|�ی��Ҕԍ� = "133033")) 
009720*                  MOVE SPACE  TO  �쐬�t���O
009730*             END-IF
009740*          END-IF
009750*      END-IF.
009760
009770* �����ی�͏���
009780     IF ��|������� = 50
009790         MOVE SPACE  TO  �쐬�t���O
009800     END-IF.
009810
009820*
009830*================================================================*
009840*================================================================*
009850 �㍂��\�ԍ��Z�b�g SECTION.
009860*
009870     EVALUATE �㍂��\�ԍ��v(3:2)
009880     WHEN "01"
009890         MOVE "39010004" TO �㍂��\�ԍ��v
009900     WHEN "02"
009910         MOVE "39020003" TO �㍂��\�ԍ��v
009920     WHEN "03"
009930         MOVE "39030002" TO �㍂��\�ԍ��v
009940     WHEN "04"
009950         MOVE "39040001" TO �㍂��\�ԍ��v
009960     WHEN "05"
009970         MOVE "39050000" TO �㍂��\�ԍ��v
009980     WHEN "06"
009990         MOVE "39060009" TO �㍂��\�ԍ��v
010000     WHEN "07"
010010         MOVE "39070008" TO �㍂��\�ԍ��v
010020     WHEN "08"
010030         MOVE "39080007" TO �㍂��\�ԍ��v
010040     WHEN "09"
010050         MOVE "39090006" TO �㍂��\�ԍ��v
010060     WHEN "10"
010070         MOVE "39100003" TO �㍂��\�ԍ��v
010080     WHEN "11"
010090         MOVE "39110002" TO �㍂��\�ԍ��v
010100     WHEN "12"
010110         MOVE "39120001" TO �㍂��\�ԍ��v
010120     WHEN "13"
010130         MOVE "39130000" TO �㍂��\�ԍ��v
010140     WHEN "14"
010150         MOVE "39140009" TO �㍂��\�ԍ��v
010160     WHEN "15"
010170         MOVE "39150008" TO �㍂��\�ԍ��v
010180     WHEN "16"
010190         MOVE "39160007" TO �㍂��\�ԍ��v
010200     WHEN "17"
010210         MOVE "39170006" TO �㍂��\�ԍ��v
010220     WHEN "18"
010230         MOVE "39180005" TO �㍂��\�ԍ��v
010240     WHEN "19"
010250         MOVE "39190004" TO �㍂��\�ԍ��v
010260     WHEN "20"
010270         MOVE "39200001" TO �㍂��\�ԍ��v
010280     WHEN "21"
010290         MOVE "39210000" TO �㍂��\�ԍ��v
010300     WHEN "22"
010310         MOVE "39220009" TO �㍂��\�ԍ��v
010320     WHEN "23"
010330         MOVE "39230008" TO �㍂��\�ԍ��v
010340     WHEN "24"
010350         MOVE "39240007" TO �㍂��\�ԍ��v
010360     WHEN "25"
010370         MOVE "39250006" TO �㍂��\�ԍ��v
010380     WHEN "26"
010390         MOVE "39260008" TO �㍂��\�ԍ��v
010400     WHEN "27"
010410         MOVE "39270004" TO �㍂��\�ԍ��v
010420     WHEN "28"
010430         MOVE "39280003" TO �㍂��\�ԍ��v
010440     WHEN "29"
010450         MOVE "39290002" TO �㍂��\�ԍ��v
010460     WHEN "30"
010470         MOVE "39300009" TO �㍂��\�ԍ��v
010480     WHEN "31"
010490         MOVE "39310008" TO �㍂��\�ԍ��v
010500     WHEN "32"
010510         MOVE "39320007" TO �㍂��\�ԍ��v
010520     WHEN "33"
010530         MOVE "39330006" TO �㍂��\�ԍ��v
010540     WHEN "34"
010550         MOVE "39340005" TO �㍂��\�ԍ��v
010560     WHEN "35"
010570         MOVE "39350004" TO �㍂��\�ԍ��v
010580     WHEN "36"
010590         MOVE "39360003" TO �㍂��\�ԍ��v
010600     WHEN "37"
010610         MOVE "39370002" TO �㍂��\�ԍ��v
010620     WHEN "38"
010630         MOVE "39380001" TO �㍂��\�ԍ��v
010640     WHEN "39"
010650         MOVE "39390000" TO �㍂��\�ԍ��v
010660     WHEN "40"
010670         MOVE "39400007" TO �㍂��\�ԍ��v
010680     WHEN "41"
010690         MOVE "39410006" TO �㍂��\�ԍ��v
010700     WHEN "42"
010710         MOVE "39420005" TO �㍂��\�ԍ��v
010720     WHEN "43"
010730         MOVE "39430004" TO �㍂��\�ԍ��v
010740     WHEN "44"
010750         MOVE "39440003" TO �㍂��\�ԍ��v
010760     WHEN "45"
010770         MOVE "39450002" TO �㍂��\�ԍ��v
010780     WHEN "46"
010790         MOVE "39460001" TO �㍂��\�ԍ��v
010800     WHEN "47"
010810         MOVE "39470000" TO �㍂��\�ԍ��v
010820     END-EVALUATE.
010830*================================================================*
010840*================================================================*
010850 ��ƃt�@�C���쐬 SECTION.
010860*
010870     OPEN OUTPUT ��ƃt�@�C���P.
010880         MOVE NC"���" TO �t�@�C����.
010890         PERFORM �I�[�v���`�F�b�N.
010900**********************************
010910* ���ہE�ސE�E�V�l
010920**********************************
010930     MOVE �����a��v�q  TO ���Z�|�����a��.
010931     MOVE �����N�v�q    TO ���Z�|�����N.  
010932     MOVE �������v�q    TO ���Z�|������.  
010933     MOVE 1             TO ���Z�|���Z���.
010934     MOVE SPACE         TO ���Z�|�����ی��Ҕԍ�.
010935     MOVE ZERO          TO ���Z�|���Ҕԍ�.
010936     MOVE SPACE         TO ���Z�|�}��.    
010937     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
010938                                  ���Z�|���Z���
010939                                  ���Z�|�����ی��Ҕԍ�
010940                                  ���Z�|���҃R�[�h
010941     END-START.
010942     IF ��ԃL�[ = "00"
010943         MOVE SPACE  TO �I���t���O
010944         PERFORM ���Z�v�g�e�Ǎ�
010945         PERFORM UNTIL ( �I���t���O = "YES" ) OR
010946                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
010947                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
010948                       ( ���Z�|������   NOT = �������v�q   ) OR
      */����������/140402
                             ( ���Z�|���Z��� NOT = 1 AND 2)
010958             PERFORM �f�[�^�`�F�b�N
010968             IF ���s�L�[�v = "YES"
010978                 IF (��|�ی���� = 01      ) AND (��|�ی��Ҕԍ�(1:2)     = "13") OR
011130                    (��|�ی���� = 08 OR 05) AND (��|�ی��Ҕԍ�(3:2)     = "13") OR
011140                    (��|������ = 05      ) AND (��|��p���S�Ҕԍ�(3:2) = "13")
011170                     PERFORM ���ۗ����v�Z
011180                     PERFORM ���R�[�h�Z�b�g
011190                     PERFORM �t�@�C������
011200                 END-IF
011210             END-IF
011220             PERFORM ���Z�v�g�e�Ǎ�
011230         END-PERFORM
011240     END-IF.
011250     CLOSE ��ƃt�@�C���P.
011260*================================================================*
011270 ���R�[�h�Z�b�g SECTION.
011280*
011290     MOVE SPACE TO ��P�|���R�[�h
011300     INITIALIZE ��P�|���R�[�h
011310     PERFORM ��������Z�b�g.
011320     MOVE ��������v   TO ��P�|�������.
011330     IF ��|������ = 05
011340         MOVE ��|������ TO ��P�|�ی����
011350     ELSE
011360         MOVE ��|�ی���� TO ��P�|�ی����
011370     END-IF
011380     EVALUATE TRUE
011390     WHEN ��|�ی���� = 01
011400         MOVE ��|�ی��Ҕԍ�      TO ��P�|�ی��Ҕԍ�
011410     WHEN ��|�ی���� = 08
011420         MOVE ��|�ی��Ҕԍ�(3:8) TO ��P�|�ی��Ҕԍ�
011430     WHEN ��|�ی���� = 05
011440         MOVE ��|�ی��Ҕԍ�      TO �㍂��\�ԍ��v
011450         PERFORM �㍂��\�ԍ��Z�b�g
011460         MOVE �㍂��\�ԍ��v      TO ��P�|�ی��Ҕԍ�
011470     WHEN ��|������ = 05
011480         PERFORM �s�������擾
011490         MOVE �s�|�ی��Ҕԍ�      TO ��P�|�ی��Ҕԍ�
011500     END-EVALUATE.
011510     MOVE ��|���҃R�[�h          TO ��P�|���҃R�[�h.
011520     MOVE ��|�{�p�a��N��        TO ��P�|�{�p�a��N��.
011530     IF ��|������ = 05
011540*         MOVE �A�v�|��p�z�V�l    TO ��P�|��p�z
011550*         MOVE �A�v�|���S�z�V�l    TO ��P�|���S�z
011560*         MOVE �A�v�|�����z�V�l    TO ��P�|�����z
011570         MOVE ���Z�|���v          TO ��P�|��p�z
011571         MOVE ���Z�|�ꕔ���S��    TO ��P�|���S�z
011572         MOVE ���Z�|�������z      TO ��P�|�����z
011573     ELSE
011580*         MOVE �A�v�|��p�z        TO ��P�|��p�z
011590*         MOVE �A�v�|���S�z        TO ��P�|���S�z
011600*         MOVE �A�v�|�����z        TO ��P�|�����z
011610         MOVE ���Z�|���v          TO ��P�|��p�z
011611         MOVE ���Z�|�ꕔ���S��    TO ��P�|���S�z
011612         MOVE ���Z�|�������z      TO ��P�|�����z
011613     END-IF.
011620*================================================================*
011630 ��������Z�b�g SECTION.
011640*
011650**///  ��������̕ύX�͂���SECTION��   ////**
011660*
011670*************************************************************************
011680*  �������:�ی�����(�ی���ʃR�[�h)                                    *
011690*  10�F�s�������ہA�ސE�A�V�l                                           *
011700*  20�F���ۑg��                                                         *
011710*  30�F�������                                                         *
011720**************************************************************************
011730     EVALUATE TRUE
011740     WHEN (��|�ی���� = 01) AND (��|�ی��Ҕԍ�(3:1) = "3")
011750         MOVE 20 TO ��������v
011760     WHEN (��|�ی���� = 05) AND (��|�ی��Ҕԍ�(1:2) = "39")
011770         MOVE 30 TO ��������v
011780     WHEN (��|�ی���� = 01 OR 08) OR (��|������ = 05)
011790         MOVE 10 TO ��������v
011800     END-EVALUATE.
011810*
011820*================================================================*
011830 �s�������擾 SECTION.
011840*
011850     MOVE ��|������       TO �s�|������.
011860     MOVE ��|��p���S�Ҕԍ� TO �s�|�s�����ԍ�.
011870     READ �s�����}�X�^
011880     INVALID KEY
011890         MOVE SPACE TO �s�|���R�[�h
011900     END-READ.
011910*================================================================*
011920*================================================================*
011930 �U��������� SECTION.
011940*
011950     OPEN INPUT ��ƃt�@�C���P.
011960         MOVE NC"���" TO �t�@�C����.
011970         PERFORM �I�[�v���`�F�b�N.
012000     MOVE SPACE TO YJK4371P.
012010     INITIALIZE    YJK4371P.
012020
012030     MOVE 1     TO �łv.
012040*
012050     MOVE 1     TO �s�J�E���^.
012060     MOVE SPACE TO �ی��Ҕԍ��v.
012070     MOVE SPACE TO �I���t���O.
012080     PERFORM ��ƃt�@�C���Ǎ�
012090     IF  �I���t���O = "YES"
012100         MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
012110         CALL   "MSG001"
012120         CANCEL "MSG001"
012130         PERFORM �t�@�C����
012140         MOVE 99 TO PROGRAM-STATUS
012150         EXIT PROGRAM
012160     END-IF.
012170     MOVE ��P�|�ی��Ҕԍ� TO �ی��Ҕԍ��v.
012180     PERFORM UNTIL �I���t���O = "YES"
012190         PERFORM ���׍s�Z�b�g
012200         PERFORM ��ƃt�@�C���Ǎ�
012210         IF ��P�|�ی��Ҕԍ� NOT = �ی��Ҕԍ��v
012220             MOVE ��P�|�ی��Ҕԍ� TO �ی��Ҕԍ��v
012230             ADD 1 TO �s�J�E���^
012240         END-IF
012250         IF �s�J�E���^ > 12
012260             PERFORM �w�b�_�Z�b�g
012270             PERFORM �󎚏���
012280             PERFORM ���ŏ���
012290             COMPUTE �łv = �łv + 1
012300             MOVE 1 TO �s�J�E���^
012310             MOVE SPACE TO YJK4371P
012320             INITIALIZE    YJK4371P
012330         END-IF
012340     END-PERFORM.
012350     PERFORM �w�b�_�Z�b�g.
012360     MOVE ���ی������v�v   TO ���ی������v.
012370     MOVE ���۔�p�z���v�v TO ���۔�p�z���v.
012380     MOVE �ސE�������v�v   TO �ސE�������v.
012390     MOVE �ސE��p�z���v�v TO �ސE��p�z���v.
012400     MOVE �V���������v�v   TO �V���������v.
012410     MOVE �V����p�z���v�v TO �V����p�z���v.
012420     MOVE ����������v�v   TO ����������v.
012430     MOVE �����p�z���v�v TO �����p�z���v.
012440*
012450     COMPUTE �v�������v   = ���ی������v�v   + �ސE�������v�v   +
012460                            �V���������v�v   + ����������v�v
012470     COMPUTE �v��p�z���v = ���۔�p�z���v�v + �ސE��p�z���v�v +
012480                            �V����p�z���v�v + �����p�z���v�v
012490     PERFORM �󎚏���.
012500     PERFORM ���ŏ���
012510     CLOSE ��ƃt�@�C���P.
012520     CLOSE ����t�@�C��.
012530*================================================================*
012540 ���׍s�Z�b�g SECTION.
012550*
012560     IF �ی��Җ��̂P(�s�J�E���^) = SPACE
012570         IF (��P�|�ی���� = 05) AND (��P�|�ی��Ҕԍ�(1:2) = "39")
012580             PERFORM ���������擾
012590         ELSE
012600             PERFORM �ی��ҏ��擾
012610         END-IF
012620         MOVE �����於�̂P�v TO �ی��Җ��̂P(�s�J�E���^)
012630         MOVE �����於�̂Q�v TO �ی��Җ��̂Q(�s�J�E���^)
012640     END-IF.
012650     EVALUATE ��P�|�ی����
012660     WHEN 01
012670         ADD 1            TO ���ی���  (�s�J�E���^) �v����    (�s�J�E���^)
012680                             ���ی������v�v
012690         ADD ��P�|��p�z TO ���۔�p�z(�s�J�E���^) �v��p�z  (�s�J�E���^)
012700                             ���۔�p�z���v�v
012710     WHEN 08
012720         ADD 1            TO �ސE����  (�s�J�E���^) �v����    (�s�J�E���^)
012730                             �ސE�������v�v
012740         ADD ��P�|��p�z TO �ސE��p�z(�s�J�E���^) �v��p�z  (�s�J�E���^)
012750                             �ސE��p�z���v�v
012760     WHEN 05
012770         IF ��P�|�ی��Ҕԍ�(1:2) = "39"
012780             ADD 1            TO �������  (�s�J�E���^) �v����    (�s�J�E���^)
012790                                 ����������v�v
012800             ADD ��P�|��p�z TO �����p�z(�s�J�E���^) �v��p�z  (�s�J�E���^)
012810                                 �����p�z���v�v
012820         ELSE
012830             ADD 1            TO �V������  (�s�J�E���^) �v����    (�s�J�E���^)
012840                                 �V���������v�v
012850             ADD ��P�|��p�z TO �V����p�z(�s�J�E���^) �v��p�z  (�s�J�E���^)
012860                                 �V����p�z���v�v
012870         END-IF
012880     END-EVALUATE.
012890*================================================================*
012900 �ی��ҏ��擾 SECTION.
012910*
012920     MOVE  SPACE             TO �����於�̂v.
012930*
012940     MOVE 01                 TO �ہ|�ی����.
012950     MOVE ��P�|�ی��Ҕԍ�   TO �ہ|�ی��Ҕԍ�.
012960     READ �ی��҃}�X�^
012970     NOT INVALID KEY
012980         MOVE �ہ|�ی��Җ��� TO �����於�̂v
012990     END-READ.
013000*================================================================*
013010 ���������擾 SECTION.
013020*
013030     MOVE SPACE            TO �����於�̂v.
013040     MOVE 05               TO �s�|������.
013050     MOVE ��P�|�ی��Ҕԍ� TO �s�|�s�����ԍ�.
013060     START �s�����}�X�^ KEY IS >= �s�|������ �s�|�s�����ԍ�
013070     END-START.
013080     IF ��ԃL�[ = "00"
013090         READ �s�����}�X�^ NEXT
013100         AT END
013110             MOVE SPACE              TO �����於�̂v
013120         NOT AT END
013130             IF (�s�|������        = 05 ) AND
013140                (�s�|�s�����ԍ�(1:4) = ��P�|�ی��Ҕԍ�(1:4))
013150                 MOVE �s�|�s�������� TO �����於�̂v
013160             END-IF
013170         END-READ
013180     END-IF.
013190*================================================================*
013200 �w�b�_�Z�b�g SECTION.
013210*
013220     MOVE �łv               TO ��.
013230* �����̘a����擾
013240     MOVE �����a��v�q       TO ���|�����敪.
013250     READ �����}�X�^
013260     INVALID KEY
013270         MOVE SPACE          TO �����a���
013280     NOT INVALID KEY
013290         MOVE ���|��������   TO �����a���
013300     END-READ.
013310*
013320     MOVE �����N�v�q         TO �����N.
013330     MOVE �������v�q         TO ������.
013340*
013350     MOVE ��\�Җ��v         TO ��\�Җ�.
013360     MOVE �ڍ��@���v         TO �ڍ��@��.
013370     MOVE �_���t�ԍ��v(3:7)  TO �_���t�ԍ��P.
013380     MOVE �_���t�ԍ��v(11:1) TO �_���t�ԍ��Q.
013390     MOVE �_���t�ԍ��v(13:1) TO �_���t�ԍ��R.
013400     MOVE NC"�����s"         TO �s���{����.
013410     MOVE NC"�����ی�"       TO �ی���ʖ�.
013420*================================================================*
013430 �󎚏���  SECTION.
013440*
013441     IF ( �I�[�v���t���O NOT = "YES" )
013442        MOVE "YES" TO �I�[�v���t���O
013443        OPEN I-O  ����t�@�C��
013444        PERFORM �G���[�����o
013445     END-IF.
013446*
013450     MOVE "YJK4371P" TO  ��`�̖��o.
013460     MOVE SPACE      TO  ������ʂo.
013470     MOVE "SCREEN"   TO  ���ڌQ���o.
013480     WRITE YJK4371P.
013490     PERFORM �G���[�����o.
013500*================================================================*
013510 ���ŏ���  SECTION.
013520
013530     MOVE "YJK4371P" TO  ��`�̖��o.
013540     MOVE "CT"       TO  ������ʂo.
013550     MOVE "PAGE"     TO  �g������o.
013560     MOVE SPACE      TO  ���ڌQ���o.
013570     WRITE YJK4371P.
013580     PERFORM �G���[�����o.
013590     MOVE SPACE     TO  �g������o.
013600*
013610*================================================================*
013620 ��ƃt�@�C���Ǎ� SECTION.
013630*
013640     READ ��ƃt�@�C���P NEXT
013650     AT END
013660         MOVE "YES" TO �I���t���O
013670     END-READ.
013680*================================================================*
013690 �G���[�����o SECTION.
013700*
013710     IF �ʒm���o NOT = "00"
013720         DISPLAY NC"���[�G���["              UPON CONS
013730         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
013740         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
013750         DISPLAY NC"�g������o�F" �g������o UPON CONS
013760         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS
013770         ACCEPT  �L�[���� FROM CONS
013780         PERFORM �t�@�C����
013790         MOVE 99  TO PROGRAM-STATUS
013800         EXIT PROGRAM
013810     END-IF.
013820*================================================================*
013830 �{�p�����擾 SECTION.
013840*
013850     MOVE ZERO  TO �{��|�{�p���ԍ�.
013860     READ �{�p�����}�X�^
013870     INVALID KEY
013880         CONTINUE
013890     NOT INVALID KEY
013900*
013910         MOVE �{��|�X�֔ԍ��P       TO �{�p���X�֔ԍ��P�v
013920         MOVE "-"                    TO �{�p���X�֔ԍ���؂v
013930         MOVE �{��|�X�֔ԍ��Q       TO �{�p���X�֔ԍ��Q�v
013940         MOVE �{��|��\�Җ�         TO ��\�Җ��v
013950         MOVE �{��|�ڍ��@��         TO �ڍ��@���v
013960         STRING �{��|�Z���P  DELIMITED BY SPACE
013970                �{��|�Z���Q  DELIMITED BY SPACE
013980           INTO �{�p���Z���v
013990         END-STRING
014000         MOVE �{��|�d�b�ԍ�         TO �{�p���d�b�ԍ��v
014010         MOVE �{��|�V�_���t�ԍ�     TO �_���t�ԍ��v
014020*
014030         MOVE �{��|������s��     TO ������s���v
014040         MOVE �{��|������s�x�X�� TO ������s�x�X���v
014050         MOVE �{��|�a�����         TO �a����ʂv
014060         MOVE �{��|��s�ԍ�         TO ��s�ԍ��v
014070         MOVE �{��|�X�ԍ�           TO �X�ԍ��v
014080         MOVE �{��|�����ԍ�         TO �����ԍ��v
014090         MOVE �{��|�������`�l�J�i   TO �������`�l�J�i�v
014100         MOVE �{��|�������`�l       TO �������`�l�v
014110         STRING ������s���v     DELIMITED BY SPACE
014120                " "                DELIMITED BY SIZE
014130                ������s�x�X���v DELIMITED BY SPACE
014140                INTO ��s���x�X���v
014150         END-STRING
014160         EVALUATE �a����ʂv
014170         WHEN 1
014180             MOVE NC"����" TO �a����ʃR�����g�v
014190         WHEN 2
014200             MOVE NC"����" TO �a����ʃR�����g�v
014210         WHEN OTHER
014220             MOVE SPACE    TO �a����ʃR�����g�v
014230         END-EVALUATE
014240
014250     END-READ.
014260*================================================================*
014270 �V��������� SECTION.
014280*
014290*
014300     OPEN INPUT ��ƃt�@�C���P.
014310         MOVE NC"���" TO �t�@�C����.
014320         PERFORM �I�[�v���`�F�b�N.
014350     MOVE SPACE TO YJK4362P.
014360     INITIALIZE    YJK4362P.
014370     MOVE 30    TO ��P�|�������.
014380     MOVE SPACE TO ��P�|�ی��Ҕԍ�.
014390     MOVE ZERO  TO ��P�|�ی����.
014400     MOVE SPACE TO ��P�|���҃R�[�h.
014410     MOVE ZERO  TO ��P�|�{�p�a��N��.
014420     START ��ƃt�@�C���P KEY IS >=  ��P�|�������
014430                                     ��P�|�ی��Ҕԍ�
014440                                     ��P�|�ی����
014450                                     ��P�|���҃R�[�h
014460                                     ��P�|�{�p�a��N��
014470     END-START.
014480     IF ��ԃL�[ = "00"
014490         MOVE SPACE TO �I���t���O
014500         PERFORM ��ƃt�@�C���Ǎ�
014510         IF  �I���t���O = "YES"
014520             MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
014530             CALL   "MSG001"
014540             CANCEL "MSG001"
014550             PERFORM �t�@�C����
014560             MOVE 99 TO PROGRAM-STATUS
014570             EXIT PROGRAM
014580         END-IF
014590         PERFORM UNTIL �I���t���O = "YES"
014600             PERFORM ������R�[�h�Z�b�g
014610             PERFORM ��ƃt�@�C���Ǎ�
014620         END-PERFORM
014630         PERFORM �w�b�_�Z�b�g�Q
014640         PERFORM �󎚏����Q
014650         PERFORM ���ŏ����Q
014660     END-IF.
014670
014700     CLOSE ��ƃt�@�C���P.
014710     CLOSE ����t�@�C���Q.
014720*================================================================*
014730 ������R�[�h�Z�b�g SECTION.
014740*
014750     ADD 1            TO �V���|�{�l����.
014760     ADD ��P�|��p�z TO �V���|�{�l��p�z
014770     ADD ��P�|���S�z TO �V���|�{�l���S�z.
014780     ADD ��P�|�����z TO �V���|�{�l�����z.
014790*================================================================*
014800 �w�b�_�Z�b�g�Q SECTION.
014810*
014820* �����̘a����擾
014830     MOVE �����a��v�q       TO ���|�����敪.
014840     READ �����}�X�^
014850     INVALID KEY
014860         MOVE SPACE          TO �V���|�����a���
014870     NOT INVALID KEY
014880         MOVE ���|��������   TO �V���|�����a���
014890     END-READ.
014900*
014910     MOVE �����N�v�q         TO �V���|�����N.
014920     MOVE �������v�q         TO �V���|������.
014930     MOVE ��\�Җ��v         TO �V���|��\�Җ�.
014940     MOVE �ڍ��@���v         TO �V���|�ڍ��@��.
014950     MOVE �_���t�ԍ��v       TO �V���|�_���t�ԍ�.
014960*
014970* / �ی���/
014980     MOVE ��P�|�ی����     TO �ی���ʂv.
014990     IF (��P�|�ی���� = 05) AND (��P�|�ی��Ҕԍ�(1:2) = "39")
015000         MOVE ��P�|�ی��Ҕԍ�(1:4) TO �V���|�ی��Ҕԍ�
015010     ELSE
015020         MOVE ��P�|�ی��Ҕԍ�      TO �V���|�ی��Ҕԍ�
015030     END-IF.
015040*     PERFORM �����Z�b�g.
015050*     PERFORM �ی���ʖ��Z�b�g.
015060     MOVE NC"�����s" TO �V���|�s���{����.
015070     MOVE NC"���"   TO �V���|�ی���ʖ�.
015080     PERFORM ���������擾.
015090*
015100     STRING  �����於�̂v  DELIMITED BY SPACE
015110             "  �a"        DELIMITED BY SIZE
015120            INTO �ی��҈����v
015130     END-STRING.
015140     MOVE �ی��҈����v  TO �V���|�ی��Җ���.
015150*
015160*================================================================*
015170 �󎚏����Q  SECTION.
015180*
015190     IF ( �I�[�v���t���O�Q NOT = "YES" )
015191        MOVE "YES" TO �I�[�v���t���O�Q
015192        OPEN I-O  ����t�@�C���Q
015193        PERFORM �G���[�����o
015194     END-IF.
015195*
015196     MOVE "YJK4362P" TO  ��`�̖��o.
015200     MOVE SPACE      TO  ������ʂo.
015210     MOVE "SCREEN"   TO  ���ڌQ���o.
015220     WRITE YJK4362P.
015230     PERFORM �G���[�����o.
015240*================================================================*
015250 ���ŏ����Q  SECTION.
015260
015270     MOVE "YJK4362P" TO  ��`�̖��o.
015280     MOVE "CT"       TO  ������ʂo.
015290     MOVE "PAGE"     TO  �g������o.
015300     MOVE SPACE      TO  ���ڌQ���o.
015310     WRITE YJK4362P.
015320     PERFORM �G���[�����o.
015330     MOVE SPACE     TO  �g������o.
015340*
015350*================================================================*
015360******************************************************************
015370 END PROGRAM YJK438.
015380******************************************************************
