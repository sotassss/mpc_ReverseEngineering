000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJK582.
000060 AUTHOR.                 ���c�@���a
000070*
000080*----------------------------------------------------------------*
000090*         �����䒠����i�_����޳��95�Łj
000100* �����N��Ver.
000110*         MED = YJK580G YJK582P
000120* ������ԍ~���Ή� 2007/6/5
000130*----------------------------------------------------------------*
000140 DATE-WRITTEN.           2012-08-08
000150 DATE-COMPILED.          2012-08-08
000160*----------------------------------------------------------------*
000170******************************************************************
000180*            ENVIRONMENT         DIVISION                        *
000190******************************************************************
000200 ENVIRONMENT             DIVISION.
000210 CONFIGURATION           SECTION.
000220 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000230 OBJECT-COMPUTER.        FMV-DESKPOWER.
000240 SPECIAL-NAMES.          CONSOLE  IS  CONS
000250                         SYSERR   IS  MSGBOX.
000260 INPUT-OUTPUT            SECTION.
000270 FILE-CONTROL.
000280     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000290                             ORGANIZATION             IS  INDEXED
000300                             ACCESS MODE              IS  DYNAMIC
000310                             RECORD KEY               IS  ���|�����敪
000320                             FILE STATUS              IS  ��ԃL�[
000330                             LOCK        MODE         IS  AUTOMATIC.
000410     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000420                             ORGANIZATION             IS  INDEXED
000430                             ACCESS MODE              IS  DYNAMIC
000440                             RECORD KEY               IS  ���|����敪
000450                             FILE STATUS              IS  ��ԃL�[
000460                             LOCK        MODE         IS  AUTOMATIC.
000470     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000480                             ORGANIZATION             IS  INDEXED
000490                             ACCESS MODE              IS  DYNAMIC
000500                             RECORD KEY               IS �{��|�{�p���ԍ�
000510                             FILE STATUS              IS  ��ԃL�[
000520                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS MODE              IS  DYNAMIC
000560                             RECORD KEY               IS  �ہ|�ی����
000570                                                          �ہ|�ی��Ҕԍ�
000580                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000590                                                          �ہ|�ی��Җ���
000600                                                          �ہ|�ی��Ҕԍ�
000610                             FILE STATUS              IS  ��ԃL�[
000620                             LOCK        MODE         IS  AUTOMATIC.
000630     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000640                             ORGANIZATION             IS  INDEXED
000650                             ACCESS MODE              IS  DYNAMIC
000660                             RECORD KEY               IS  �s�|������
000670                                                          �s�|�s�����ԍ�
000680                             ALTERNATE RECORD KEY     IS  �s�|������
000690                                                          �s�|�s��������
000700                                                          �s�|�s�����ԍ�
000710                             FILE STATUS              IS  ��ԃL�[
000720                             LOCK        MODE         IS  AUTOMATIC.
000742     SELECT  ��ƃt�@�C���P  ASSIGN      TO           "C:\MAKISHISYS\YAWOBJ\TEMP\W5801L.DAT"
000743                             ORGANIZATION             IS  SEQUENTIAL
000750                             ACCESS                   IS  SEQUENTIAL
000760                             FILE        STATUS       IS  ��ԃL�[
000770                             LOCK        MODE         IS  AUTOMATIC.
000790     SELECT  ��ƃt�@�C���Q  ASSIGN      TO           "C:\MAKISHISYS\YAWOBJ\TEMP\W58211L.DAT"
000791                             ORGANIZATION             IS  INDEXED
000800                             ACCESS                   IS  DYNAMIC
000810                             RECORD      KEY          IS  ��Q�|�����a��N��
000820                                                          ��Q�|����敪
000830                                                          ��Q�|�ی��敪
000840                                                          ��Q�|�ی��Ҕԍ�
000850                                                          ��Q�|�{�l�Ƒ��敪
000860                                                          ��Q�|��ی��҃J�i
000870                                                          ��Q�|���҃J�i
000880                                                          ��Q�|�����敪
000890                                                          ��Q�|�{�p�a��N��
000900                                                          ��Q�|���҃R�[�h
000910                             FILE        STATUS       IS  ��ԃL�[
000920                             LOCK        MODE         IS  AUTOMATIC.
000940     SELECT  ��ƃt�@�C���R  ASSIGN      TO           "C:\MAKISHISYS\YAWOBJ\TEMP\W58212L.DAT"
000941                             ORGANIZATION             IS  INDEXED
000950                             ACCESS                   IS  DYNAMIC
000960                             RECORD      KEY          IS  ��R�|�����a��N��
000970                                                          ��R�|����敪
000980                                                          ��R�|�ی��敪
000990                                                          ��R�|�ی��Ҕԍ�
001000                                                          ��R�|�{�l�Ƒ��敪
001010                                                          ��R�|��ی��҃J�i
001020                                                          ��R�|���҃J�i
001030                                                          ��R�|�����敪
001040                                                          ��R�|�{�p�a��N��
001050                                                          ��R�|���҃R�[�h
001060                             FILE        STATUS       IS  ��ԃL�[
001070                             LOCK        MODE         IS  AUTOMATIC.
001080* ���Z���я��p
001100     SELECT  ��ƃt�@�C���S  ASSIGN      TO           "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001101                             ORGANIZATION             IS  INDEXED
001110                             ACCESS                   IS  DYNAMIC
001120                             RECORD      KEY          IS  ��S�|�{�p�a��N��
001130                                                          ��S�|���҃R�[�h
001140                                                          ��S�|�ی����
001150                             FILE        STATUS       IS  ��ԃL�[
001160                             LOCK        MODE         IS  AUTOMATIC.
001170     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF001
001180                             SYMBOLIC    DESTINATION  IS "PRT"
001190                             FORMAT                   IS  ��`�̖��o
001200                             GROUP                    IS  ���ڌQ���o
001210                             PROCESSING  MODE         IS  ������ʂo
001220                             UNIT        CONTROL      IS  �g������o
001230                             FILE        STATUS       IS  �ʒm���o.
001240******************************************************************
001250*                      DATA DIVISION                             *
001260******************************************************************
001270 DATA                    DIVISION.
001280 FILE                    SECTION.
001290*                           �m�q�k��  �P�Q�W�n
001300 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001310     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001350*                           �m�q�k��  �Q�T�U�n
001360 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001370     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001380*                           �m�q�k��  �U�S�O�n
001390 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001400     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001410*                           �m�q�k��  �R�Q�O�n
001420 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001430     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001440*                           �m�q�k��  �Q�T�U�n
001450 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001460     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
001470*
001480*
001490 FD  ��ƃt�@�C���P RECORD  CONTAINS 288 CHARACTERS.
001500 01  ��P�|���R�[�h.
001510     03  ��P�|���R�[�h�f�[�^.
001520         05  ��P�|�����a��N��.
001530             07  ��P�|�����a��            PIC 9.
001540             07  ��P�|�����N              PIC 9(2).
001550             07  ��P�|������              PIC 9(2).
001560         05  ��P�|�{�p�a��N��.
001570             07  ��P�|�{�p�a��            PIC 9.
001580             07  ��P�|�{�p�N              PIC 9(2).
001590             07  ��P�|�{�p��              PIC 9(2).
001600         05  ��P�|����敪                PIC 9(2).
001610         05  ��P�|�ی��Ҕԍ�              PIC X(10).
001620         05  ��P�|�L��                    PIC X(24).
001630         05  ��P�|�ԍ�                    PIC X(30).
001640         05  ��P�|��ی��҃J�i            PIC X(50).
001650         05  ��P�|��ی��Ҏ���            PIC X(50).
001660         05  ��P�|���҃J�i                PIC X(50).
001670         05  ��P�|�{�l�Ƒ��敪            PIC 9.
001680         05  ��P�|��p�z                  PIC 9(7).
001690         05  ��P�|���S�z                  PIC 9(7).
001700         05  ��P�|�����z                  PIC 9(7).
001710         05  ��P�|���҃R�[�h.
001720             07 ��P�|���Ҕԍ�             PIC 9(6).
001730             07 ��P�|�}��                 PIC X(1).
001740         05  ��P�|�ی����                PIC 9(2).
001750         05  ��P�|�e�ی����              PIC 9(2).
001760         05  ��P�|�V�l���R�[�h�敪        PIC 9.
001770         05  ��P�|�������R�[�h�敪        PIC 9.
001780         05  FILLER                        PIC X(27).
001790
001800 FD  ��ƃt�@�C���Q RECORD  CONTAINS 288 CHARACTERS.
001810 01  ��Q�|���R�[�h.
001820     03  ��Q�|���R�[�h�L�[.
001830         05  ��Q�|�����a��N��.
001840             07  ��Q�|�����a��            PIC 9.
001850             07  ��Q�|�����N              PIC 9(2).
001860             07  ��Q�|������              PIC 9(2).
001870         05  ��Q�|����敪                PIC 9(2).
001880         05  ��Q�|�ی��敪                PIC 9.
001890         05  ��Q�|�{�l�Ƒ��敪            PIC 9.
001900         05  ��Q�|�ی��Ҕԍ�              PIC X(10).
001910         05  ��Q�|��ی��҃J�i            PIC X(50).
001920         05  ��Q�|���҃J�i                PIC X(50).
001930         05  ��Q�|�����敪                PIC 9.
001940         05  ��Q�|�{�p�a��N��.
001950             07  ��Q�|�{�p�a��            PIC 9.
001960             07  ��Q�|�{�p�N              PIC 9(2).
001970             07  ��Q�|�{�p��              PIC 9(2).
001980         05  ��Q�|���҃R�[�h.
001990             07 ��Q�|���Ҕԍ�             PIC 9(6).
002000             07 ��Q�|�}��                 PIC X(1).
002010     03  ��Q�|���R�[�h�f�[�^.
002020         05  ��Q�|�L��                    PIC X(24).
002030         05  ��Q�|�ԍ�                    PIC X(30).
002040         05  ��Q�|��ی��Ҏ���            PIC X(50).
002050         05  ��Q�|��p�z                  PIC 9(7).
002060         05  ��Q�|���S�z                  PIC 9(7).
002070         05  ��Q�|�����z                  PIC 9(7).
002080         05  ��Q�|�ی����                PIC 9(2).
002090         05  ��Q�|�e�ی����              PIC 9(2).
002100         05  ��Q�|�V�l���R�[�h�敪        PIC 9.
002110         05  ��Q�|�������R�[�h�敪        PIC 9.
002120         05  FILLER                        PIC X(25).
002130
002140 FD  ��ƃt�@�C���R RECORD  CONTAINS 288 CHARACTERS.
002150 01  ��R�|���R�[�h.
002160     03  ��R�|���R�[�h�L�[.
002170         05  ��R�|�����a��N��.
002180             07  ��R�|�����a��            PIC 9.
002190             07  ��R�|�����N              PIC 9(2).
002200             07  ��R�|������              PIC 9(2).
002210         05  ��R�|����敪                PIC 9(2).
002220         05  ��R�|�ی��敪                PIC 9.
002230         05  ��R�|�{�l�Ƒ��敪            PIC 9.
002240         05  ��R�|�ی��Ҕԍ�              PIC X(10).
002250         05  ��R�|��ی��҃J�i            PIC X(50).
002260         05  ��R�|���҃J�i                PIC X(50).
002270         05  ��R�|�����敪                PIC 9.
002280         05  ��R�|�{�p�a��N��.
002290             07  ��R�|�{�p�a��            PIC 9.
002300             07  ��R�|�{�p�N              PIC 9(2).
002310             07  ��R�|�{�p��              PIC 9(2).
002320         05  ��R�|���҃R�[�h.
002330             07 ��R�|���Ҕԍ�             PIC 9(6).
002340             07 ��R�|�}��                 PIC X(1).
002350     03  ��R�|���R�[�h�f�[�^.
002360         05  ��R�|�L��                    PIC X(24).
002370         05  ��R�|�ԍ�                    PIC X(30).
002380         05  ��R�|��ی��Ҏ���            PIC X(50).
002390         05  ��R�|��p�z                  PIC 9(7).
002400         05  ��R�|���S�z                  PIC 9(7).
002410         05  ��R�|�����z                  PIC 9(7).
002420         05  ��R�|�ی����                PIC 9(2).
002430         05  ��R�|�e�ی����              PIC 9(2).
002440         05  ��R�|�V�l���R�[�h�敪        PIC 9.
002450         05  ��R�|�������R�[�h�敪        PIC 9.
002460         05  FILLER                        PIC X(25).
002470
002480* ���Z�v�g���я��t�@�C��
002490 FD  ��ƃt�@�C���S RECORD  CONTAINS 32 CHARACTERS.
002500 01  ��S�|���R�[�h.
002510     03  ��S�|���R�[�h�L�[.
002520         05  ��S�|�{�p�a��N��.
002530             07  ��S�|�{�p�a��            PIC 9.
002540             07  ��S�|�{�p�N              PIC 9(2).
002550             07  ��S�|�{�p��              PIC 9(2).
002560         05  ��S�|���҃R�[�h.
002570             07 ��S�|���Ҕԍ�             PIC 9(6).
002580             07 ��S�|�}��                 PIC X(1).
002590         05  ��S�|�ی����                PIC 9(2).
002600     03  ��S�|���R�[�h�f�[�^.
002610         05  ��S�|����                    PIC 9(4).
002620         05  FILLER                        PIC X(14).
002630*
002640 FD  ����t�@�C��.
002650     COPY YJK582P        OF  XMDLIB.
002660*----------------------------------------------------------------*
002670******************************************************************
002680*                WORKING-STORAGE SECTION                         *
002690******************************************************************
002700 WORKING-STORAGE         SECTION.
002710 01 �L�[����                           PIC X    VALUE SPACE.
002720 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
002730 01 �I���t���O                         PIC X(3) VALUE SPACE.
002740 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
002750 01 �I���s�t���O                       PIC X(3) VALUE SPACE.
002760 01 �t�@�C����                         PIC N(10).
002770 01 �ΏۂȂ��t���O                     PIC X(3)  VALUE SPACE.
002780 01 �I�[�v���t���O                     PIC X(3)  VALUE SPACE.
002781*
002790 01 ����敪�v                         PIC 9(2)  VALUE ZERO.
002800 01 ����敪�o�v                       PIC 9(2)  VALUE ZERO.
002810 01 ����敪�o�v�Q                     PIC 9(2)  VALUE ZERO.
002820 01 �ی���ʂv                         PIC 9(2)  VALUE ZERO.
002830 01 �ی��Ҕԍ��v�q                     PIC X(10) VALUE SPACE.
002840 01 �����a��N���o�v.
002850    03 �����a��o�v                    PIC 9    VALUE ZERO.
002860    03 �����N�o�v                      PIC 9(2) VALUE ZERO.
002870    03 �������o�v                      PIC 9(2) VALUE ZERO.
002880 01 �����a��N���o�v�Q.
002890    03 �����a��o�v�Q                  PIC 9    VALUE ZERO.
002900    03 �����N�o�v�Q                    PIC 9(2) VALUE ZERO.
002910    03 �������o�v�Q                    PIC 9(2) VALUE ZERO.
002920*
002930 01 �ی����̂v                         PIC N(3) VALUE SPACE.
002940 01 ���l�v                             PIC X(20).
002950 01 �O�a��v                           PIC 9    VALUE ZERO.
002960 01 �s�J�E���^                         PIC 9(2) VALUE 0.
002970 01 �ŃJ�E���^                         PIC 9(4) VALUE 0.
002980 01 �ő�s��                           PIC 9(2) VALUE 0.
002990 01 �w�b�_�s��                         PIC 9(2) VALUE 0.
003000 01 �ړ��s���v                         PIC 9(2) VALUE 0.
003010 01 �����ړ��L�[                       PIC X(4) VALUE SPACE.
003020 01 �J�����g�����v                     PIC 9(1) VALUE ZERO.
003030*
003040 01 ����ԍ��v.
003050    03 ����ԍ��v�o                    PIC X(10) VALUE SPACE.
003060*
003070 01 �ی���ʖ��̂v.
003080    03 �ی���ʖ��̂v�o                PIC X(6) VALUE SPACE.
003090 01 �e�ی���ʖ��̂v.
003100    03 �e�ی���ʖ��̂v�o              PIC X(4) VALUE SPACE.
003110    03 ���v����                        PIC X(4).
003120 01 �����v�v.
003130    03 �����v�v�o                      PIC X(6).
003140*
003150** ���������p
003160 01 ��ی��҃J�i�v.
003170    03 ��ی��҃J�i�v�o                PIC X    OCCURS 20 VALUE SPACE.
003180 01 �����J�i�v.
003190    03 �����J�i�v�o                    PIC X    OCCURS 20 VALUE SPACE.
003200 01 ���O�J�i�v.
003210    03 ���O�J�i�v�o                    PIC X    OCCURS 20 VALUE SPACE.
003220 01 �J�E���^                           PIC 9(2) VALUE ZERO.
003230 01 �l�J�E���^                         PIC 9(2) VALUE ZERO.
003240 01 �m�J�E���^                         PIC 9(2) VALUE ZERO.
003250 01 �����t���O                         PIC X(3) VALUE SPACE.
003260**
003270 01 ���v�v.
003280    03 ���v�����v                      PIC 9(4) VALUE ZERO.
003290    03 ��p�z���v�v                    PIC 9(7) VALUE ZERO.
003300    03 ���S�z���v�v                    PIC 9(7) VALUE ZERO.
003310    03 �����z���v�v                    PIC 9(7) VALUE ZERO.
003320 01 ���v�v.
003330    03 ���v�����v                      PIC 9(4) VALUE ZERO.
003340    03 ��p�z���v�v                    PIC 9(8) VALUE ZERO.
003350    03 ���S�z���v�v                    PIC 9(8) VALUE ZERO.
003360    03 �����z���v�v                    PIC 9(8) VALUE ZERO.
003370*
003380 01 �{�p�a��N���v.
003390     03 �{�p�a��v                   PIC 9.
003400     03 �{�p�N���v.
003410        05 �{�p�N�v                  PIC 9(2).
003420        05 �N�v                      PIC N(1).
003430        05 �{�p���v                  PIC 9(2).
003440        05 ���v                      PIC N(1).
003450 01 ���ݘa��N���v.
003460    03 ���ݘa��v                    PIC 9.
003470    03 ���ݔN�v                      PIC 9(2).
003480    03 ���݌��v                      PIC 9(2).
003490*
003500 01 �ی���ʂv�q                     PIC 9(2) VALUE ZERO.
003510 01 ����`���v�q                     PIC 9    VALUE ZERO.
003520
003530 01 ���Z���я����~�v                   PIC 9    VALUE ZERO.
003540
003541 01 ���{��ϊ��w.
003542    03 ���{��ϊ��m                    PIC N(50) VALUE SPACE.
003550*******************************************************************
003560 01 �������.
003570     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
003580     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
003590     03 ������ʂo                     PIC X(2) VALUE SPACE.
003600     03 �g������o.
003610         05 �[������o.
003620             07 �ړ������o             PIC X(1).
003630             07 �ړ��s���o             PIC 9(3).
003640         05 �ڍא���o                 PIC X(2).
003650     03 �ʒm���o                     PIC X(2) VALUE SPACE.
003660     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
003670*
003680 01 �v�Z�@����N�v                     PIC 9(2).
003690* ���t�v�n�q�j
003700 01 �a��I���N�v                       PIC 9(4).
003710 01 �v�Z�@����.
003720    03 �v�Z�@����N                    PIC 9(4).
003730    03 �v�Z�@�����                  PIC 9(4).
003740 01 �v�Z�@����q REDEFINES �v�Z�@����.
003750    03 �v�Z�@���I                      PIC 9(2).
003760    03 �v�Z�@���t                      PIC 9(6).
003770    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
003780       05 �v�Z�@�N��                   PIC 9(4).
003790       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
003800         07 �v�Z�@�N                   PIC 9(2).
003810         07 �v�Z�@��                   PIC 9(2).
003820       05 �v�Z�@��                     PIC 9(2).
003830******************************************************************
003840*                          �A������                              *
003850******************************************************************
003950*
003958 01 �A���|��ʏ��x�i�j�T�W�O IS EXTERNAL.
003959    03 �A���|����`��                  PIC 9.
003960    03 �A���|�����a��N��.
003961       05 �A���|�����a��               PIC 9.
003962       05 �A���|�����N��.
003963         07 �A���|�����N               PIC 9(2).
003964         07 �A���|������               PIC 9(2).
003965    03 �A���|�ی����                  PIC 9(2).
003967*
003971 01 �A��|��ʏ��x�i�j�T�W�O IS EXTERNAL GLOBAL.
003972    03 �A��|�J�n��                    PIC 9(3).
003973    03 �A��|�I����                    PIC 9(3).
003974    03 �A��|�v���r���[�敪            PIC 9.
003975*
004000 01 �A���|�L�[ IS EXTERNAL.
004010    03  �A���|���b�Z�[�W               PIC N(20).
004020*
004030************************************
004031* �v�����^�t�@�C���쐬�p           *
004032************************************
004033 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
004034     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
004035     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
004036     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
004037     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
004038*
004039******************************************************************
004040*                      PROCEDURE  DIVISION                       *
004050******************************************************************
004060 PROCEDURE               DIVISION.
004070************
004080*           *
004090* ��������   *
004100*           *
004110************
004123     MOVE SPACE TO �I�[�v���t���O.
004124     PERFORM �v�����^�t�@�C���쐬.
004125     PERFORM ������.
004130     MOVE SPACE  TO YJK582P.
004140
004150***     INITIALIZE YJK582P.
004160************
004170*           *
004180* �又��     *
004190*           *
004200************
004210*
004220*/ �ی���ʎw�����ǉ� /0904
004230*
004240     EVALUATE TRUE
004250*     WHEN  ����`���v�q = ZERO 
004260     WHEN  �ی���ʂv�q = ZERO 
004270* �������ɂ́A�����̏������s��
004280         IF ���Z���я����~�v = ZERO
004290             PERFORM �������
004300         ELSE
004310             PERFORM ��ƃt�@�C���Q�R�쐬
004320             PERFORM ��������Q
004330         END-IF
004340
004350*          PERFORM �������
004360*     WHEN  ����`���v�q = 1 OR 2 
004370*          PERFORM �ꕔ�������
004380     WHEN OTHER
004390         PERFORM ����敪�擾
004401         IF ���Z���я����~�v = ZERO
004412             PERFORM �ی���ʂ��ƈ������
004420             IF �ی���ʂv�q = 01
004430                 MOVE 3     TO ����敪�v
004440                 PERFORM �ی���ʂ��ƈ������
004450             END-IF
004460         ELSE
004471             PERFORM ��ƃt�@�C���Q�R�쐬
004480             PERFORM �ی���ʂ��ƈ�������Q
004490             IF �ی���ʂv�q = 01
004500                 MOVE 3     TO ����敪�v
004510                 PERFORM �ی���ʂ��ƈ�������Q
004520             END-IF
004530         END-IF
004540*          CONTINUE
004550     END-EVALUATE.
004560*
004570************
004580*           *
004590* �I������   *
004600*           *
004610************
004620     PERFORM �I������.
004630     EXIT PROGRAM.
004640*
004650*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
004660*================================================================*
004661 �v�����^�t�@�C���쐬 SECTION.
004662*================================================================*
004663*   / ������ /
004664     MOVE SPACE TO �g�A�o�q�s�e�|�쐬�f�[�^.
004665     INITIALIZE �g�A�o�q�s�e�|�쐬�f�[�^.
004666*
004667*
004668*--���� �ύX�ӏ� ����--------------------------------------*
004669*   �g�p����v�����^�t�@�C�����Z�b�g
004671     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
004672*
004673*   �g�p���钠�[�v���O�������Z�b�g
004674     MOVE "YJK582"              TO �g�A�o�q�s�e�|���[�v���O������.
004675*
004676*--����-----------------------------------------------------*
004677*
004678*   / �v���r���[�敪�Z�b�g /
004679     MOVE �A��|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
004681*
004682     CALL   "CRTPRTF".
004683     CANCEL "CRTPRTF".
004684*
004685*================================================================*
004686 ������ SECTION.
004687*
004690     PERFORM �t�@�C���I�[�v��.
004700*    /* ���ݓ��t�擾 */
004710     ACCEPT �v�Z�@���t FROM DATE.
004720*    /* 1980�`2079�N�̊ԂŐݒ� */
004730     IF �v�Z�@�N > 80
004740         MOVE 19 TO �v�Z�@���I
004750     ELSE
004760         MOVE 20 TO �v�Z�@���I
004770     END-IF.
004780     PERFORM �J�����g�����擾.
004790     PERFORM �a��I���N�擾.
004800     COMPUTE �v�Z�@����N�v = �v�Z�@����N - 1988.
004810*
004820     MOVE �J�����g�����v TO ���ݘa��v.
004830     MOVE �v�Z�@����N�v TO ���ݔN�v.
004840     MOVE �v�Z�@��       TO ���݌��v.
004850*
004860* �A�����ڂ̑Ҕ�
004880     MOVE �A���|�ی����    TO �ی���ʂv�q.
004890*================================================================*
004900 �J�����g�����擾 SECTION.
004910*
004920     MOVE ZEROS TO ���|����敪.
004930     READ ������}�X�^
004940     NOT INVALID KEY
004950         MOVE ���|�J�����g���� TO �J�����g�����v
004960         MOVE ���|���Z���я����~ TO ���Z���я����~�v
004970     END-READ.
004980*
004990*================================================================*
005000 �a��I���N�擾 SECTION.
005010*
005020     MOVE �J�����g�����v TO ���|�����敪.
005030     READ �����}�X�^
005040     INVALID KEY
005050         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
005060         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005070                                                  UPON CONS
005080         ACCEPT  �L�[���� FROM CONS
005090         PERFORM �I������
005100         EXIT PROGRAM
005110     NOT INVALID KEY
005120         COMPUTE �O�a��v = �J�����g�����v - 1
005130         MOVE �O�a��v TO ���|�����敪
005140         READ �����}�X�^
005150         INVALID KEY
005160             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
005170             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005180                                                      UPON CONS
005190             ACCEPT  �L�[���� FROM CONS
005200             PERFORM �I������
005210             EXIT PROGRAM
005220         NOT INVALID KEY
005230             MOVE ���|�I������N TO �a��I���N�v
005240         END-READ
005250     END-READ.
005260*
005270*================================================================*
005280 �t�@�C���I�[�v�� SECTION.
005290*
005300     OPEN INPUT �����}�X�^
005310         MOVE NC"����" TO �t�@�C����.
005320         PERFORM �I�[�v���`�F�b�N.
005360     OPEN INPUT ������}�X�^
005370         MOVE NC"������" TO �t�@�C����.
005380         PERFORM �I�[�v���`�F�b�N.
005390     OPEN INPUT �{�p�����}�X�^
005400         MOVE NC"�{�p�����" TO �t�@�C����.
005410         PERFORM �I�[�v���`�F�b�N.
005420     OPEN INPUT �ی��҃}�X�^.
005430         MOVE NC"�ی��҃}�X�^" TO �t�@�C����.
005440         PERFORM �I�[�v���`�F�b�N.
005450     OPEN INPUT �s�����}�X�^.
005460         MOVE NC"�s�����}�X�^" TO �t�@�C����.
005470         PERFORM �I�[�v���`�F�b�N.
005480     OPEN INPUT ��ƃt�@�C���P
005490         MOVE NC"��ƂP" TO �t�@�C����.
005500         PERFORM �I�[�v���`�F�b�N.
005510     OPEN INPUT   ��ƃt�@�C���S.
005520         MOVE NC"��S" TO �t�@�C����.
005530         PERFORM �I�[�v���`�F�b�N.
005560*================================================================*
005570 �I�[�v���`�F�b�N SECTION.
005580*
005590     IF ��ԃL�[  NOT =  "00"
005600         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
005610         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
005620         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005630                                                 UPON CONS
005640         ACCEPT  �L�[���� FROM CONS
005650         PERFORM �t�@�C����
005660         EXIT PROGRAM.
005670*================================================================*
005680 �t�@�C���� SECTION.
005690*
005700     IF ( �I�[�v���t���O = "YES" )
005701         CLOSE ����t�@�C��
005702     ELSE
005703         MOVE  NC"�@�@�f�[�^���O���ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
005704         CALL   "MSG001"
005705         CANCEL "MSG001"
005706     END-IF.
005707*
005710     CLOSE �����}�X�^ ������}�X�^ �{�p�����}�X�^
005711           �ی��҃}�X�^ �s�����}�X�^ ��ƃt�@�C���P ��ƃt�@�C���Q
005720           ��ƃt�@�C���R ��ƃt�@�C���S.
005730*================================================================*
005740 �I������ SECTION.
005750*
005760     PERFORM �t�@�C����.
005770*================================================================*
005780 �G���[�\�� SECTION.
005790*
005800     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
005810     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
005820     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
005830     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
005840     ACCEPT  �L�[���� FROM CONS.
005850     PERFORM �t�@�C����.
005860     EXIT PROGRAM.
005870
005880
005890*================================================================*
005900 ������� SECTION.
005910*
005920
005930     MOVE SPACE TO �I���t���O.
005940     INITIALIZE ���v�v.
005950     INITIALIZE ���v�v.
005960     MOVE 20    TO �ő�s��.
005970     MOVE 1     TO �ŃJ�E���^.
005980     PERFORM ��ƃt�@�C���P�Ǎ�.
005990     IF  �I���t���O = "YES"
006000         MOVE  NC"�@�@�@�Y������f�[�^������܂���B" TO �A���|���b�Z�[�W
006010         CALL   "MSG001"
006020         CANCEL "MSG001"
006030         PERFORM �t�@�C����
006040         MOVE 99 TO PROGRAM-STATUS
006050         EXIT PROGRAM
006060     END-IF.
006070     PERFORM UNTIL �I���t���O = "YES"
006080*
006090         MOVE ��P�|�����a��     TO �����a��o�v
006100         MOVE ��P�|�����N       TO �����N�o�v
006110         MOVE ��P�|������       TO �������o�v
006120         MOVE ��P�|����敪     TO ����敪�o�v
006130         PERFORM �w�b�_�Z�b�g
006140         PERFORM �w�b�_�󎚏���
006150*
006160         PERFORM UNTIL ( �I���t���O = "YES" ) OR
006170                       ( �s�J�E���^ > �ő�s�� )  OR
006180                       ( ��P�|�����a��   NOT = �����a��o�v ) OR
006190                       ( ��P�|�����N     NOT = �����N�o�v   ) OR
006200                       ( ��P�|������     NOT = �������o�v   ) OR
006210                       ( ��P�|����敪   NOT = ����敪�o�v ) 
006225             PERFORM ���ו��Z�b�g
006230             PERFORM ���׈󎚏���
006240             PERFORM ���v�z�݌v
006250             PERFORM ��ƃt�@�C���P�Ǎ�
006260         END-PERFORM
006270*
006280         IF ( �s�J�E���^ > �ő�s�� )
006290             IF  ( �I���t���O = "YES" ) OR 
006300                 ( ��P�|�����a��   NOT = �����a��o�v ) OR
006310                 ( ��P�|�����N     NOT = �����N�o�v   ) OR
006320                 ( ��P�|������     NOT = �������o�v   ) OR
006330                 ( ��P�|����敪   NOT = ����敪�o�v ) 
006340                 PERFORM ���v�Z�b�g
006350                 PERFORM ���v�󎚏���
006360                 INITIALIZE ���v�v
006370                 PERFORM ���ŏ���
006380                 MOVE 1  TO �ŃJ�E���^
006390             ELSE
006400                 PERFORM ���ŏ���
006410                 COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
006420             END-IF
006430         ELSE
006440             PERFORM ���v�Z�b�g
006450             PERFORM ���v�󎚏���
006460             INITIALIZE ���v�v
006470             PERFORM ���ŏ���
006480             MOVE 1  TO �ŃJ�E���^
006490         END-IF
006500*
006510     END-PERFORM.
006520
006530*
006540*================================================================*
006550 ��������Q SECTION.
006560*
006570     OPEN INPUT ��ƃt�@�C���Q
006580         MOVE NC"��ƂQ" TO �t�@�C����.
006590         PERFORM �I�[�v���`�F�b�N.
006600
006610     OPEN INPUT ��ƃt�@�C���R
006620         MOVE NC"��ƂR" TO �t�@�C����.
006630         PERFORM �I�[�v���`�F�b�N.
006640* �~���Ńt�@�C����ǂ݁A����敪�̐擪�ʒu���擾���A��ʖ��Ɉ�����s��
006650
      */20190603
006660*     MOVE 4          TO  ��R�|�����a��.
006660     MOVE 9          TO  ��R�|�����a��.
006670     MOVE 99         TO  ��R�|�����N.
006680     MOVE 12         TO  ��R�|������.
006690     MOVE 99         TO  ��R�|����敪.
006700
006710     MOVE HIGH-VALUE TO  ��R�|�ی��Ҕԍ�.
006720     MOVE 999999     TO  ��R�|���Ҕԍ�
006730     MOVE HIGH-VALUE TO  ��R�|�}��
006740     MOVE ZERO       TO  ��R�|�{�l�Ƒ��敪.
006750
006760     MOVE 9          TO  ��R�|�ی��敪.
006770     MOVE 9          TO  ��R�|�����敪.
006780     MOVE HIGH-VALUE TO  ��R�|��ی��҃J�i.
006790     MOVE HIGH-VALUE TO  ��R�|���҃J�i.
      */20190603
006800*     MOVE 4          TO  ��R�|�{�p�a��.
006800     MOVE 5          TO  ��R�|�{�p�a��.
006810     MOVE 99         TO  ��R�|�{�p�N.
006820     MOVE 12         TO  ��R�|�{�p��.
006830
006840     START ��ƃt�@�C���R   KEY IS <=  ��R�|�����a��N��
006850                                       ��R�|����敪
006860                                       ��R�|�ی��敪
006870                                       ��R�|�ی��Ҕԍ�
006880                                       ��R�|�{�l�Ƒ��敪
006890                                       ��R�|��ی��҃J�i
006900                                       ��R�|���҃J�i
006910                                       ��R�|�����敪
006920                                       ��R�|�{�p�a��N��
006930                                       ��R�|���҃R�[�h
006940                                       REVERSED
006950     END-START.
006960     IF ��ԃL�[ = "00"
006970
006980         MOVE SPACE TO �I���t���O�Q
006990         PERFORM ��ƃt�@�C���R�Ǎ�
007000         IF  �I���t���O�Q = "YES"
007010             MOVE  NC"�@�@�@�Y������f�[�^������܂���B" TO �A���|���b�Z�[�W
007020             CALL   "MSG001"
007030             CANCEL "MSG001"
007040             PERFORM �t�@�C����
007050             MOVE 99 TO PROGRAM-STATUS
007060             EXIT PROGRAM
007070         END-IF
007080
007090         PERFORM UNTIL �I���t���O�Q = "YES"
007100*
007110             MOVE ��R�|�����a��     TO �����a��o�v�Q
007120             MOVE ��R�|�����N       TO �����N�o�v�Q
007130             MOVE ��R�|������       TO �������o�v�Q
007140             MOVE ��R�|����敪     TO ����敪�o�v�Q
007150
007160             PERFORM UNTIL ( �I���t���O�Q = "YES" ) OR
007170                           ( ��R�|�����a��   NOT = �����a��o�v�Q ) OR
007180                           ( ��R�|�����N     NOT = �����N�o�v�Q   ) OR
007190                           ( ��R�|������     NOT = �������o�v�Q   ) OR
007200                           ( ��R�|����敪   NOT = ����敪�o�v�Q ) 
007210
007220
007230                 MOVE ��R�|�����a��     TO �����a��o�v�Q
007240                 MOVE ��R�|�����N       TO �����N�o�v�Q
007250                 MOVE ��R�|������       TO �������o�v�Q
007260                 MOVE ��R�|����敪     TO ����敪�o�v�Q
007270
007280                 PERFORM ��ƃt�@�C���R�Ǎ�
007290             END-PERFORM
007300             PERFORM ��������R
007310*
007320*
007330         END-PERFORM
007340
007350     END-IF.
007360
007370*
007380*================================================================*
007390 ��������R SECTION.
007400*
007410* ��������Q�Œ��o��������敪�ɑ΂��āA����������s��
007420     MOVE �����a��o�v�Q    TO  ��Q�|�����a��.
007430     MOVE �����N�o�v�Q      TO  ��Q�|�����N.
007440     MOVE �������o�v�Q      TO  ��Q�|������.
007450     MOVE LOW-VALUE         TO  ��Q�|�ی��Ҕԍ�.
007460     MOVE ����敪�o�v�Q    TO  ��Q�|����敪.
007470     MOVE ZERO              TO  ��Q�|�ی��敪.
007480     MOVE ZERO              TO  ��Q�|�����敪.
007490     MOVE ZERO              TO  ��Q�|�{�l�Ƒ��敪.
007500     MOVE LOW-VALUE         TO  ��Q�|��ی��҃J�i.
007510     MOVE LOW-VALUE         TO  ��Q�|���҃J�i.
007520     MOVE ZERO              TO  ��Q�|�{�p�a��.
007530     MOVE ZERO              TO  ��Q�|�{�p�N.
007540     MOVE ZERO              TO  ��Q�|�{�p��.
007550     MOVE ZERO              TO  ��Q�|���Ҕԍ�
007560     MOVE LOW-VALUE         TO  ��Q�|�}��
007570     
007580     START ��ƃt�@�C���Q   KEY IS >=  ��Q�|�����a��N��
007590                                       ��Q�|����敪
007600                                       ��Q�|�ی��敪
007610                                       ��Q�|�ی��Ҕԍ�
007620                                       ��Q�|�{�l�Ƒ��敪
007630                                       ��Q�|��ی��҃J�i
007640                                       ��Q�|���҃J�i
007650                                       ��Q�|�����敪
007660                                       ��Q�|�{�p�a��N��
007670                                       ��Q�|���҃R�[�h
007680     END-START.
007690     IF ��ԃL�[ = "00"
007700
007710         MOVE SPACE TO �I���t���O
007720         INITIALIZE ���v�v
007730         INITIALIZE ���v�v
007740         MOVE 20    TO �ő�s��
007750         MOVE 1     TO �ŃJ�E���^
007760         PERFORM ��ƃt�@�C���Q�Ǎ�
007770         IF  �I���t���O = "YES"
007780             MOVE  NC"�@�@�@�Y������f�[�^������܂���B" TO �A���|���b�Z�[�W
007790             CALL   "MSG001"
007800             CANCEL "MSG001"
007810             PERFORM �t�@�C����
007820             MOVE 99 TO PROGRAM-STATUS
007830             EXIT PROGRAM
007840         END-IF
007850         PERFORM UNTIL �I���t���O = "YES"
007860*
007870             MOVE ��Q�|�����a��     TO �����a��o�v
007880             MOVE ��Q�|�����N       TO �����N�o�v
007890             MOVE ��Q�|������       TO �������o�v
007900             MOVE ��Q�|����敪     TO ����敪�o�v
007910             PERFORM �w�b�_�Z�b�g�Q
007920             PERFORM �w�b�_�󎚏���
007930*
007940             PERFORM UNTIL ( �I���t���O = "YES" ) OR
007950                           ( �s�J�E���^ > �ő�s�� )  OR
007960                           ( ��Q�|�����a��   NOT = �����a��o�v ) OR
007970                           ( ��Q�|�����N     NOT = �����N�o�v   ) OR
007980                           ( ��Q�|������     NOT = �������o�v   ) OR
007990                           ( ��Q�|����敪   NOT = ����敪�o�v ) 
008000                 PERFORM ���ו��Z�b�g�Q
008010                 PERFORM ���׈󎚏���
008020                 PERFORM ���v�z�݌v�Q
008030                 PERFORM ��ƃt�@�C���Q�Ǎ�
008040             END-PERFORM
008050*
008060             IF ( �s�J�E���^ > �ő�s�� )
008070                 IF  ( �I���t���O = "YES" ) OR 
008080                     ( ��Q�|�����a��   NOT = �����a��o�v ) OR
008090                     ( ��Q�|�����N     NOT = �����N�o�v   ) OR
008100                     ( ��Q�|������     NOT = �������o�v   ) OR
008110                     ( ��Q�|����敪   NOT = ����敪�o�v ) 
008120                     PERFORM ���v�Z�b�g
008130                     PERFORM ���v�󎚏���
008140                     INITIALIZE ���v�v
008150                     PERFORM ���ŏ���
008160                     MOVE 1  TO �ŃJ�E���^
008170                     MOVE "YES"  TO �I���t���O
008180                 ELSE
008190                     PERFORM ���ŏ���
008200                     COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
008210                 END-IF
008220             ELSE
008230                 PERFORM ���v�Z�b�g
008240                 PERFORM ���v�󎚏���
008250                 INITIALIZE ���v�v
008260                 PERFORM ���ŏ���
008270                 MOVE 1  TO �ŃJ�E���^
008280                 MOVE "YES"  TO �I���t���O
008290             END-IF
008300*
008310         END-PERFORM
008320
008330     END-IF.
008340*
008350*================================================================*
008360 �ꕔ������� SECTION.
008370*
008380     MOVE SPACE TO �I���t���O.
008390     INITIALIZE ���v�v.
008400     INITIALIZE ���v�v.
008410     MOVE 20    TO �ő�s��.
008420     MOVE 1     TO �ŃJ�E���^.
008430     PERFORM ��ƃt�@�C���P�Ǎ�.
008440     IF  �I���t���O = "YES"
008450         MOVE "YES"  TO �ΏۂȂ��t���O
008460     END-IF.
008470     PERFORM UNTIL �I���t���O = "YES"
008480*
008490         MOVE ��P�|�����a��     TO �����a��o�v
008500         MOVE ��P�|�����N       TO �����N�o�v
008510         MOVE ��P�|������       TO �������o�v
008520         MOVE ��P�|����敪     TO ����敪�o�v
008530         PERFORM �w�b�_�Z�b�g
008540         PERFORM �w�b�_�󎚏���
008550*
008560         PERFORM UNTIL ( �I���t���O = "YES" ) OR ( �s�J�E���^ > �ő�s�� )  OR
008570                       ( ��P�|�����a��   NOT = �����a��o�v ) OR
008580                       ( ��P�|�����N     NOT = �����N�o�v   ) OR
008590                       ( ��P�|������     NOT = �������o�v   ) OR
008600                       ( ��P�|����敪   NOT = ����敪�o�v ) 
008610             PERFORM ���ו��Z�b�g
008620             PERFORM ���׈󎚏���
008630             PERFORM ���v�z�݌v
008640             PERFORM ��ƃt�@�C���P�Ǎ�
008650         END-PERFORM
008660*
008670         PERFORM ���v�Z�b�g
008680         PERFORM ���v�󎚏���
008690         PERFORM ���ŏ���
008700*
008710     END-PERFORM.
008720*
008730*================================================================*
008740 �ی���ʂ��ƈ������ SECTION.
008750*
008760     CLOSE ��ƃt�@�C���P.
008770     OPEN INPUT ��ƃt�@�C���P
008780         MOVE NC"��ƂP" TO �t�@�C����.
008790         PERFORM �I�[�v���`�F�b�N.
008800*
008810     MOVE SPACE TO �I���t���O.
008820     INITIALIZE ���v�v.
008830     INITIALIZE ���v�v.
008840     MOVE 20    TO �ő�s��.
008850     MOVE 1     TO �ŃJ�E���^.
008860     PERFORM ��ƃt�@�C���P�Ǎ�.
008870     IF  �I���t���O = "YES"
008880         MOVE  NC"�@�@�@�Y������f�[�^������܂���B" TO �A���|���b�Z�[�W
008890         CALL   "MSG001"
008900         CANCEL "MSG001"
008910         PERFORM �t�@�C����
008920         MOVE 99 TO PROGRAM-STATUS
008930         EXIT PROGRAM
008940     END-IF.
008950     PERFORM UNTIL ( �I���t���O = "YES" ) OR
008960                   ( ��P�|����敪   = ����敪�v ) 
008970         PERFORM ��ƃt�@�C���P�Ǎ�
008980     END-PERFORM.
008990     PERFORM UNTIL ( �I���t���O = "YES" ) OR
009000                   ( ��P�|����敪   NOT = ����敪�v ) 
009010*
009020         MOVE ��P�|�����a��     TO �����a��o�v
009030         MOVE ��P�|�����N       TO �����N�o�v
009040         MOVE ��P�|������       TO �������o�v
009050         PERFORM �w�b�_�Z�b�g
009060         PERFORM �w�b�_�󎚏���
009070*
009080         PERFORM UNTIL ( �I���t���O = "YES" ) OR
009090                       ( �s�J�E���^ > �ő�s�� )  OR
009100                       ( ��P�|�����a��   NOT = �����a��o�v ) OR
009110                       ( ��P�|�����N     NOT = �����N�o�v   ) OR
009120                       ( ��P�|������     NOT = �������o�v   ) OR
009130                       ( ��P�|����敪   NOT = ����敪�v ) 
009140             PERFORM ���ו��Z�b�g
009150             PERFORM ���׈󎚏���
009160             PERFORM ���v�z�݌v
009170             PERFORM ��ƃt�@�C���P�Ǎ�
009180         END-PERFORM
009190*
009200         IF ( �s�J�E���^ > �ő�s�� )
009210             IF  ( �I���t���O = "YES" ) OR 
009220                 ( ��P�|�����a��   NOT = �����a��o�v ) OR
009230                 ( ��P�|�����N     NOT = �����N�o�v   ) OR
009240                 ( ��P�|������     NOT = �������o�v   ) OR
009250                 ( ��P�|����敪   NOT = ����敪�v ) 
009260                 PERFORM ���v�Z�b�g
009270                 PERFORM ���v�󎚏���
009280                 INITIALIZE ���v�v
009290                 PERFORM ���ŏ���
009300                 MOVE 1  TO �ŃJ�E���^
009310             ELSE
009320                 PERFORM ���ŏ���
009330                 COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
009340             END-IF
009350         ELSE
009360             PERFORM ���v�Z�b�g
009370             PERFORM ���v�󎚏���
009380             INITIALIZE ���v�v
009390             PERFORM ���ŏ���
009400             MOVE 1  TO �ŃJ�E���^
009410         END-IF
009420*
009430     END-PERFORM.
009440*================================================================*
009450 �ی���ʂ��ƈ�������Q SECTION.
009460*
009470     OPEN INPUT ��ƃt�@�C���Q
009480         MOVE NC"��ƂQ" TO �t�@�C����.
009490         PERFORM �I�[�v���`�F�b�N.
009500*
009510     MOVE SPACE TO �I���t���O.
009520     INITIALIZE ���v�v.
009530     INITIALIZE ���v�v.
009540     MOVE 20    TO �ő�s��.
009550     MOVE 1     TO �ŃJ�E���^.
009560*
009570     MOVE �A���|�����a��    TO  ��Q�|�����a��.
009580     MOVE �A���|�����N      TO  ��Q�|�����N.
009590     MOVE �A���|������      TO  ��Q�|������.
009600     MOVE ����敪�v        TO  ��Q�|����敪.
009610     MOVE LOW-VALUE         TO  ��Q�|�ی��Ҕԍ�.
009620     MOVE ZERO              TO  ��Q�|�ی��敪.
009630     MOVE ZERO              TO  ��Q�|�����敪.
009640     MOVE ZERO              TO  ��Q�|�{�l�Ƒ��敪.
009650     MOVE LOW-VALUE         TO  ��Q�|��ی��҃J�i.
009660     MOVE LOW-VALUE         TO  ��Q�|���҃J�i.
009670     MOVE ZERO              TO  ��Q�|�{�p�a��.
009680     MOVE ZERO              TO  ��Q�|�{�p�N.
009690     MOVE ZERO              TO  ��Q�|�{�p��.
009700     MOVE ZERO              TO  ��Q�|���Ҕԍ�
009710     MOVE LOW-VALUE         TO  ��Q�|�}��
009720     
009730     START ��ƃt�@�C���Q   KEY IS >=  ��Q�|�����a��N��
009740                                       ��Q�|����敪
009750                                       ��Q�|�ی��敪
009760                                       ��Q�|�ی��Ҕԍ�
009770                                       ��Q�|�{�l�Ƒ��敪
009780                                       ��Q�|��ی��҃J�i
009790                                       ��Q�|���҃J�i
009800                                       ��Q�|�����敪
009810                                       ��Q�|�{�p�a��N��
009820                                       ��Q�|���҃R�[�h
009830     END-START.
009840     IF ��ԃL�[ = "00"
009850
009860         MOVE SPACE TO �I���t���O
009870         INITIALIZE ���v�v
009880         INITIALIZE ���v�v
009890         MOVE 20    TO �ő�s��
009900         MOVE 1     TO �ŃJ�E���^
009910         PERFORM ��ƃt�@�C���Q�Ǎ�
009920         IF  �I���t���O = "YES"
009930             MOVE  NC"�@�@�@�Y������f�[�^������܂���B" TO �A���|���b�Z�[�W
009940             CALL   "MSG001"
009950             CANCEL "MSG001"
009960             PERFORM �t�@�C����
009970             MOVE 99 TO PROGRAM-STATUS
009980             EXIT PROGRAM
009990         END-IF
010000         PERFORM UNTIL ( �I���t���O = "YES" ) OR
010010                       ( ��Q�|����敪   NOT = ����敪�v ) 
010020*
010030             MOVE ��Q�|�����a��     TO �����a��o�v
010040             MOVE ��Q�|�����N       TO �����N�o�v
010050             MOVE ��Q�|������       TO �������o�v
010060             PERFORM �w�b�_�Z�b�g�Q
010070             PERFORM �w�b�_�󎚏���
010080*
010090             PERFORM UNTIL ( �I���t���O = "YES" ) OR
010100                           ( �s�J�E���^ > �ő�s�� )  OR
010110                           ( ��Q�|�����a��   NOT = �����a��o�v ) OR
010120                           ( ��Q�|�����N     NOT = �����N�o�v   ) OR
010130                           ( ��Q�|������     NOT = �������o�v   ) OR
010140                           ( ��Q�|����敪   NOT = ����敪�v ) 
010150                 PERFORM ���ו��Z�b�g�Q
010160                 PERFORM ���׈󎚏���
010170                 PERFORM ���v�z�݌v�Q
010180                 PERFORM ��ƃt�@�C���Q�Ǎ�
010190             END-PERFORM
010200*
010210             IF ( �s�J�E���^ > �ő�s�� )
010220                 IF  ( �I���t���O = "YES" ) OR 
010230                     ( ��Q�|�����a��   NOT = �����a��o�v ) OR
010240                     ( ��Q�|�����N     NOT = �����N�o�v   ) OR
010250                     ( ��Q�|������     NOT = �������o�v   ) OR
010260                     ( ��Q�|����敪   NOT = ����敪�v ) 
010270                     PERFORM ���v�Z�b�g
010280                     PERFORM ���v�󎚏���
010290                     INITIALIZE ���v�v
010300                     PERFORM ���ŏ���
010310                     MOVE 1  TO �ŃJ�E���^
010320                     MOVE "YES"  TO �I���t���O
010330                 ELSE
010340                     PERFORM ���ŏ���
010350                     COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
010360                 END-IF
010370             ELSE
010380                 PERFORM ���v�Z�b�g
010390                 PERFORM ���v�󎚏���
010400                 INITIALIZE ���v�v
010410                 PERFORM ���ŏ���
010420                 MOVE 1  TO �ŃJ�E���^
010430                 MOVE "YES"  TO �I���t���O
010440             END-IF
010450*
010460         END-PERFORM
010470
010480     END-IF.
010490*
010500     CLOSE ��ƃt�@�C���Q.
010510*
010520*================================================================*
010530 ����敪�擾 SECTION.
010540*
010550     EVALUATE �ی���ʂv�q
010560     WHEN 01
010570         MOVE 2      TO ����敪�v
010580     WHEN 02
010590     WHEN 06
010600     WHEN 07
010610         MOVE 1       TO ����敪�v
010620     WHEN 03
010630         MOVE 12      TO ����敪�v
010640     WHEN 04
010650         MOVE 13      TO ����敪�v
010660     WHEN 09
010670         MOVE 13      TO ����敪�v
010680     WHEN 08
010690         MOVE 4       TO ����敪�v
010700     WHEN 05
010710         MOVE 5       TO ����敪�v
010720     WHEN 51
010730         MOVE 6       TO ����敪�v
010740     WHEN 52
010750         MOVE 7       TO ����敪�v
010760     WHEN 53
010770         MOVE 9       TO ����敪�v
010780     WHEN 54
010790         MOVE 10      TO ����敪�v
010800     WHEN 55
010810         MOVE 8       TO ����敪�v
010820     WHEN 60
010830         MOVE 11      TO ����敪�v
010840     END-EVALUATE.
010850*
010860*================================================================*
010870 ��ƃt�@�C���P�Ǎ� SECTION.
010880*
010890     READ ��ƃt�@�C���P NEXT
010900     AT END
010910         MOVE "YES" TO �I���t���O
010920     END-READ.
010930*
010940*================================================================*
010950 ��ƃt�@�C���Q�Ǎ� SECTION.
010960*
010970     READ ��ƃt�@�C���Q NEXT
010980     AT END
010990         MOVE "YES" TO �I���t���O
011000     END-READ.
011010*
011020*================================================================*
011030 ��ƃt�@�C���R�Ǎ� SECTION.
011040*
011050     READ ��ƃt�@�C���R NEXT
011060     AT END
011070         MOVE "YES" TO �I���t���O�Q
011080     END-READ.
011090*
011100*================================================================*
011110 ��ƃt�@�C���Q���� SECTION.
011120*
011130     WRITE ��Q�|���R�[�h
011140     INVALID KEY
011150         MOVE NC"��Q"  TO �t�@�C����
011160         PERFORM �G���[�\��
011170     END-WRITE.
011180*
011190*================================================================*
011200 ��ƃt�@�C���R���� SECTION.
011210*
011220     WRITE ��R�|���R�[�h
011230     INVALID KEY
011240         MOVE NC"��R"  TO �t�@�C����
011250         PERFORM �G���[�\��
011260     END-WRITE.
011270*
011280*================================================================*
011290 �w�b�_�󎚏���  SECTION.
011300*
011310     IF ( �I�[�v���t���O NOT = "YES" )
011311        MOVE "YES" TO �I�[�v���t���O
011312        OPEN I-O  ����t�@�C��
011313        PERFORM �G���[�����o
011314     END-IF.
011315     MOVE "YJK582P" TO  ��`�̖��o.
011320     MOVE SPACE     TO  ������ʂo.
011330     MOVE "HEAD"    TO  ���ڌQ���o.
011340     WRITE YJK582P.
011350     PERFORM �G���[�����o.
011360     MOVE SPACE TO YJK582P.
011370*================================================================*
011380 ���׈󎚏���  SECTION.
011390*
011400     MOVE "YJK582P" TO  ��`�̖��o.
011410     MOVE SPACE     TO  ������ʂo.
011420     MOVE "GRP001"  TO  ���ڌQ���o.
011430     WRITE YJK582P.
011440     PERFORM �G���[�����o.
011450     MOVE SPACE TO YJK582P.
011460*================================================================*
011470 ���v�󎚏���  SECTION.
011480*
011490     MOVE "YJK582P" TO  ��`�̖��o.
011500     MOVE SPACE     TO  ������ʂo.
011510     MOVE "FOOT"    TO  ���ڌQ���o.
011520     WRITE YJK582P.
011530     PERFORM �G���[�����o.
011540     MOVE SPACE TO YJK582P.
011550*================================================================*
011560 ���ŏ���  SECTION.
011570
011580     MOVE "YJK582P" TO  ��`�̖��o.
011590     MOVE "CT"      TO  ������ʂo.
011600     MOVE "PAGE"    TO  �g������o.
011610     MOVE SPACE     TO  ���ڌQ���o.
011620     WRITE YJK582P.
011630     PERFORM �G���[�����o.
011640     MOVE SPACE     TO  �g������o.
011650*
011660*================================================================*
011670 �w�b�_�Z�b�g SECTION.
011680*
011690     MOVE �A���|�����a�� TO �����a��o�v.
011700     MOVE �����a��o�v     TO ���|�����敪.
011710     READ �����}�X�^
011720     INVALID KEY
011730         MOVE SPACE         TO �����a��
011740     NOT INVALID KEY
011750         MOVE ���|��������  TO ���{��ϊ��m
011751         MOVE ���{��ϊ��w  TO �����a��
011760     END-READ.
011770     MOVE �A���|�����N   TO �����N.
011780     MOVE �A���|������   TO ������.
011790     MOVE �ŃJ�E���^     TO ��.
011800*
011810* ����ԍ����擾
011820     MOVE 00         TO �{��|�{�p���ԍ�.
011830     READ �{�p�����}�X�^
011840     INVALID KEY
011850         MOVE SPACE       TO ����ԍ�
011860         MOVE SPACE       TO ��\�Җ�
011870     NOT INVALID KEY
011880         MOVE �{��|�V�_���t�ԍ�      TO �_���t�ԍ�
011890         MOVE �{��|�ڍ��t�����ԍ�  TO ����ԍ�
011900         MOVE �{��|�ڍ��@��          TO �ڍ��@��
011910         MOVE �{��|��\�Җ�          TO ��\�Җ�
011920     END-READ.
011930     EVALUATE ��P�|����敪
011940     WHEN 1
011950         MOVE "�ЕہE�D��"            TO ������
011960     WHEN 2
011970     WHEN 4
011980         MOVE "���ہE�ލ�"            TO ������
011990     WHEN 3
012000         MOVE "���ۑg��"              TO ������
012010     WHEN 5
012020         MOVE "�V���@�V�l"            TO ������
012030     WHEN 6
012040         MOVE "����V�l"              TO ������
012050     WHEN 7
012060         MOVE "��q�ƒ�"              TO ������
012070     WHEN 8
012080         MOVE "���c��"                TO ������
012090     WHEN 9
012100         MOVE "��Q��"                TO ������
012110     WHEN 10
012120         MOVE "�픚��"                TO ������
012130     WHEN 11
012140         MOVE "���̑�"                TO ������
012150     WHEN 12
012160         MOVE "�g���ی�"              TO ������
012170     WHEN 13
012180         MOVE "���ϑg��"              TO ������
012190     END-EVALUATE.
012200*
012210     MOVE 1    TO �s�J�E���^.
012220*
012230*================================================================*
012240 �w�b�_�Z�b�g�Q SECTION.
012250*
012260     MOVE �A���|�����a�� TO �����a��o�v.
012270     MOVE �����a��o�v     TO ���|�����敪.
012280     READ �����}�X�^
012290     INVALID KEY
012300         MOVE SPACE         TO �����a��
012310     NOT INVALID KEY
012320         MOVE ���|��������  TO ���{��ϊ��m
012321         MOVE ���{��ϊ��w  TO �����a��
012330     END-READ.
012340     MOVE �A���|�����N   TO �����N.
012350     MOVE �A���|������   TO ������.
012360     MOVE �ŃJ�E���^     TO ��.
012370*
012380* ����ԍ����擾
012390     MOVE 00         TO �{��|�{�p���ԍ�.
012400     READ �{�p�����}�X�^
012410     INVALID KEY
012420         MOVE SPACE       TO ����ԍ�
012430         MOVE SPACE       TO ��\�Җ�
012440     NOT INVALID KEY
012450         MOVE �{��|�V�_���t�ԍ�      TO �_���t�ԍ�
012460         MOVE �{��|�ڍ��t�����ԍ�  TO ����ԍ�
012470         MOVE �{��|�ڍ��@��          TO �ڍ��@��
012480         MOVE �{��|��\�Җ�          TO ��\�Җ�
012490     END-READ.
012500     EVALUATE ��Q�|����敪
012510     WHEN 1
012520         MOVE "�ЕہE�D��"          TO ������
012530     WHEN 2
012540     WHEN 4
012550         MOVE "���ہE�ލ�"          TO ������
012560     WHEN 3
012570         MOVE "���ۑg��"            TO ������
012580     WHEN 5
012590         MOVE "�V���@�V�l"          TO ������
012600     WHEN 6
012610         MOVE "����V�l"            TO ������
012620     WHEN 7
012630         MOVE "��q�ƒ�"            TO ������
012640     WHEN 8
012650         MOVE "���c��"              TO ������
012660     WHEN 9
012670         MOVE "��Q��"              TO ������
012680     WHEN 10
012690         MOVE "�픚��"              TO ������
012700     WHEN 11
012710         MOVE "���̑�"              TO ������
012720     WHEN 12
012730         MOVE "�g���ی�"            TO ������
012740     WHEN 13
012750         MOVE "���ϑg��"            TO ������
012760     END-EVALUATE.
012770*
012780     MOVE 1    TO �s�J�E���^.
012790*
012800*================================================================*
012810 ���ו��Z�b�g SECTION.
012820*
012830     IF ( ����`���v�q = ZERO )
012840        PERFORM ���Z�v�g���я��Z�b�g
012850     END-IF.
012860*
012870     MOVE ��P�|�ی��Ҕԍ�   TO �ی��Ҕԍ��v�q
012880     MOVE ��P�|�ی����     TO �ی���ʂv
012890     IF ( ��P�|�V�l���R�[�h�敪 = 1 ) OR
012900        ( ��P�|�������R�[�h�敪 = 1 )
012910         PERFORM �s�������擾
012920     ELSE
012930         PERFORM ��������擾
012940     END-IF.
012950     IF ��P�|�ی���� = 8
012960         MOVE "(�ސE)"        TO �ی����
012970     END-IF.
012980*
012990     MOVE ��P�|��ی��Ҏ���  TO ��ی��Ҏ���.
013000     IF ��P�|�{�l�Ƒ��敪 = 1
013010         MOVE "�{�l"        TO ����
013020     ELSE
013030         MOVE "�Ƒ�"        TO ����
013040     END-IF.
013050**
013060     MOVE ��P�|��p�z       TO ���v�z.
013070     MOVE ��P�|�����z       TO �����z.
013080*
013090     IF ��P�|�����a��N�� NOT = ��P�|�{�p�a��N��
013100         MOVE ��P�|�{�p�N    TO �{�p�N
013110         MOVE ��P�|�{�p��    TO �{�p��
013120         MOVE "/"             TO ��؂�
013130     END-IF.
013140*
013150     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
013160*
013170*================================================================*
013180 ���ו��Z�b�g�Q SECTION.
013190*
013200     IF ( ����`���v�q = ZERO )
013210        PERFORM ���Z�v�g���я��Z�b�g�Q
013220     END-IF.
013230*
013240     MOVE ��Q�|�ی��Ҕԍ�   TO �ی��Ҕԍ��v�q
013250     MOVE ��Q�|�ی����     TO �ی���ʂv
013260     IF ( ��Q�|�V�l���R�[�h�敪 = 1 ) OR
013270        ( ��Q�|�������R�[�h�敪 = 1 )
013280         PERFORM �s�������擾
013290     ELSE
013300         PERFORM ��������擾
013310     END-IF.
013320     IF ��Q�|�ی���� = 8
013330         MOVE "(�ސE)"        TO �ی����
013340     END-IF.
013350*
013360     MOVE ��Q�|��ی��Ҏ���  TO ��ی��Ҏ���.
013370     IF ��Q�|�{�l�Ƒ��敪 = 1
013380         MOVE "�{�l"        TO ����
013390     ELSE
013400         MOVE "�Ƒ�"        TO ����
013410     END-IF.
013420**
013430     MOVE ��Q�|��p�z       TO ���v�z.
013440     MOVE ��Q�|�����z       TO �����z.
013450*
013460     IF ��Q�|�����a��N�� NOT = ��Q�|�{�p�a��N��
013470         MOVE ��Q�|�{�p�N    TO �{�p�N
013480         MOVE ��Q�|�{�p��    TO �{�p��
013490         MOVE "/"             TO ��؂�
013500     END-IF.
013510*
013520     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
013530*
013540*================================================================*
013550 ���Z�v�g���я��Z�b�g SECTION.
013560*
013570     MOVE ��P�|�{�p�a��     TO ��S�|�{�p�a��.
013580     MOVE ��P�|�{�p�N       TO ��S�|�{�p�N.
013590     MOVE ��P�|�{�p��       TO ��S�|�{�p��.
013600     MOVE ��P�|���҃R�[�h   TO ��S�|���҃R�[�h.
013610     MOVE ��P�|�ی����     TO ��S�|�ی����.
013620     READ ��ƃt�@�C���S
013630     NOT INVALID KEY
013640          MOVE ��S�|����    TO �ԍ�
013650     END-READ.
013660*
013670*================================================================*
013680 ���Z�v�g���я��Z�b�g�Q SECTION.
013690*
013700     MOVE ��Q�|�{�p�a��     TO ��S�|�{�p�a��.
013710     MOVE ��Q�|�{�p�N       TO ��S�|�{�p�N.
013720     MOVE ��Q�|�{�p��       TO ��S�|�{�p��.
013730     MOVE ��Q�|���҃R�[�h   TO ��S�|���҃R�[�h.
013740     MOVE ��Q�|�ی����     TO ��S�|�ی����.
013750     READ ��ƃt�@�C���S
013760     NOT INVALID KEY
013770          MOVE ��S�|����    TO �ԍ�
013780     END-READ.
013790*
013800*================================================================*
013810 ���v�z�݌v SECTION.
013820*
013830*     COMPUTE ���v�����v     = ���v�����v      + 1.
013840*     COMPUTE ��p�z���v�v   = ��p�z���v�v    + ��P�|��p�z.
013850*     COMPUTE ���S�z���v�v   = ���S�z���v�v    + ��P�|���S�z.
013860*     COMPUTE �����z���v�v   = �����z���v�v    + ��P�|�����z.
013870*
013880     COMPUTE ���v�����v     = ���v�����v      + 1.
013890     COMPUTE ��p�z���v�v   = ��p�z���v�v    + ��P�|��p�z.
013900     COMPUTE ���S�z���v�v   = ���S�z���v�v    + ��P�|���S�z.
013910     COMPUTE �����z���v�v   = �����z���v�v    + ��P�|�����z.
013920*
013930*================================================================*
013940 ���v�z�݌v�Q SECTION.
013950*
013960*     COMPUTE ���v�����v     = ���v�����v      + 1.
013970*     COMPUTE ��p�z���v�v   = ��p�z���v�v    + ��P�|��p�z.
013980*     COMPUTE ���S�z���v�v   = ���S�z���v�v    + ��P�|���S�z.
013990*     COMPUTE �����z���v�v   = �����z���v�v    + ��P�|�����z.
014000*
014010     COMPUTE ���v�����v     = ���v�����v      + 1.
014020     COMPUTE ��p�z���v�v   = ��p�z���v�v    + ��Q�|��p�z.
014030     COMPUTE ���S�z���v�v   = ���S�z���v�v    + ��Q�|���S�z.
014040     COMPUTE �����z���v�v   = �����z���v�v    + ��Q�|�����z.
014050*================================================================*
014060 ���v�Z�b�g SECTION.
014070*
014080     MOVE ��p�z���v�v TO  ���v���v���z.
014090     MOVE �����z���v�v TO  ���v�����z.
014100*================================================================*
014110 ��������擾 SECTION.
014120*
014130*********************************************************
014140* �A���f�[�^����ی��҃}�X�^��萿������擾����B      *
014150* �� ������...... �����於�̂v�Ɋi�[                    *
014160*********************************************************
014170     MOVE �ی���ʂv     TO �ہ|�ی����.
014180     MOVE �ی��Ҕԍ��v�q TO �ہ|�ی��Ҕԍ�.
014190     READ �ی��҃}�X�^
014200     INVALID KEY
014210         MOVE SPACE           TO �ی��Җ�
014220     NOT INVALID KEY
014230         MOVE �ہ|�ی��Җ���  TO �ی��Җ�
014240     END-READ.
014250*================================================================*
014260 �s�������擾 SECTION.
014270*
014280****************************************************
014290* �A���f�[�^����ی��҃}�X�^��萿������擾����B *
014300* �� ������...... �����於�̂v�Ɋi�[               *
014310****************************************************
014320     MOVE �ی���ʂv          TO �s�|������.
014330     MOVE �ی��Ҕԍ��v�q      TO �s�|�s�����ԍ�.
014340     READ �s�����}�X�^
014350     INVALID KEY
014360         MOVE SPACE           TO �ی��Җ�
014370     NOT INVALID KEY
014380         MOVE �s�|�s��������  TO �ی��Җ�
014390     END-READ.
014400*================================================================*
014410 �G���[�����o SECTION.
014420*
014430     IF �ʒm���o NOT = "00"
014440         DISPLAY NC"���[�G���["              UPON CONS
014450         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
014460         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
014470         DISPLAY NC"�g������o�F" �g������o UPON CONS
014480         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
014490                                             UPON CONS
014500         ACCEPT  �L�[���� FROM CONS
014510         PERFORM �t�@�C����
014520         EXIT PROGRAM
014530     END-IF.
014540
014550*================================================================*
014560 ��ƃt�@�C���Q�R�쐬 SECTION.
014570
014580
014590     OPEN OUTPUT ��ƃt�@�C���Q
014600         MOVE NC"��ƂQ" TO �t�@�C����.
014610         PERFORM �I�[�v���`�F�b�N.
014620
014630     OPEN OUTPUT ��ƃt�@�C���R
014640         MOVE NC"��ƂR" TO �t�@�C����.
014650         PERFORM �I�[�v���`�F�b�N.
014660
014670     MOVE SPACE  TO �I���t���O.
014680     PERFORM ��ƃt�@�C���P�Ǎ�.
014690     PERFORM UNTIL �I���t���O = "YES" 
014700         MOVE SPACE TO ��Q�|���R�[�h
014710         INITIALIZE    ��Q�|���R�[�h
014720         MOVE ��P�|����敪         TO ��Q�|����敪     ��R�|����敪
014730         MOVE ��P�|�����a��         TO ��Q�|�����a��     ��R�|�����a��
014740         MOVE ��P�|�����N           TO ��Q�|�����N       ��R�|�����N
014750         MOVE ��P�|������           TO ��Q�|������       ��R�|������
014760         MOVE ��P�|�{�p�a��         TO ��Q�|�{�p�a��     ��R�|�{�p�a��
014770         MOVE ��P�|�{�p�N           TO ��Q�|�{�p�N       ��R�|�{�p�N
014780         MOVE ��P�|�{�p��           TO ��Q�|�{�p��       ��R�|�{�p��
014790         MOVE ��P�|���҃J�i         TO ��Q�|���҃J�i     ��R�|���҃J�i
014800         MOVE ��P�|���҃R�[�h       TO ��Q�|���҃R�[�h   ��R�|���҃R�[�h
014810         MOVE ��P�|��ی��҃J�i     TO ��Q�|��ی��҃J�i ��R�|��ی��҃J�i
014820         MOVE ��P�|��ی��Ҏ���     TO ��Q�|��ی��Ҏ��� ��R�|��ی��Ҏ���
014830         MOVE ��P�|�{�l�Ƒ��敪     TO ��Q�|�{�l�Ƒ��敪 ��R�|�{�l�Ƒ��敪
014840         MOVE ��P�|�L��             TO ��Q�|�L��         ��R�|�L��
014850         MOVE ��P�|�ԍ�             TO ��Q�|�ԍ�         ��R�|�ԍ�
014860
014870         MOVE ��P�|��p�z           TO ��Q�|��p�z       ��R�|��p�z
014880         MOVE ��P�|���S�z           TO ��Q�|���S�z       ��R�|���S�z
014890         MOVE ��P�|�����z           TO ��Q�|�����z       ��R�|�����z
014900         MOVE ��P�|�ی����         TO ��Q�|�ی����     ��R�|�ی����
014910         MOVE ��P�|�ی��Ҕԍ�       TO ��Q�|�ی��Ҕԍ�   ��R�|�ی��Ҕԍ�
014920         MOVE ��P�|�e�ی����       TO ��Q�|�e�ی����   ��R�|�e�ی����
014930         MOVE ��P�|�V�l���R�[�h�敪 TO ��Q�|�V�l���R�[�h�敪 ��R�|�V�l���R�[�h�敪
014940         MOVE ��P�|�������R�[�h�敪 TO ��Q�|�������R�[�h�敪 ��R�|�������R�[�h�敪
014950         
014960* �ی��敪�Z�b�g
014970         EVALUATE ��P�|�ی����
014980* �Еہ��D�������ق̏�
014990* ����
015000             WHEN 06
015010                 MOVE 2       TO ��Q�|�ی��敪 ��R�|�ی��敪
015020* �D��
015030             WHEN 07
015040                 MOVE 1       TO ��Q�|�ی��敪 ��R�|�ی��敪
015050* ���ρ����q���̏�
015060* ���q��
015070             WHEN 09
015080                 MOVE 1       TO ��Q�|�ی��敪 ��R�|�ی��敪
015090             WHEN OTHER
015100                 MOVE ZERO    TO ��Q�|�ی��敪 ��R�|�ی��敪
015110         END-EVALUATE
015120
015130* �����敪�Z�b�g
015140         IF ��P�|�����a��N�� = ��P�|�{�p�a��N��
015150             MOVE ZERO       TO  ��Q�|�����敪 ��R�|�����敪
015160         ELSE
015170             MOVE 1          TO  ��Q�|�����敪 ��R�|�����敪
015180         END-IF
015190
015200         
015210         PERFORM ��ƃt�@�C���Q����
015220         PERFORM ��ƃt�@�C���R����
015230         
015240         PERFORM ��ƃt�@�C���P�Ǎ�
015250     END-PERFORM.
015260
015270     CLOSE ��ƃt�@�C���Q ��ƃt�@�C���R.
015280*
015290*================================================================*
015300******************************************************************
015310 END PROGRAM YJK582.
015320******************************************************************
