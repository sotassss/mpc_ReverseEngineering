000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJK437.
000060 AUTHOR.                 ���c�@���a
000070*
000080*----------------------------------------------------------------*
000090* ���{�_�����ω�p �U���p�� �������y����z�_����޳��95��
000100*         MED = YJK437P
000110*
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2012-09-06
000140 DATE-COMPILED.          2012-09-06
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
000390     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000400                             ORGANIZATION             IS  INDEXED
000410                             ACCESS MODE              IS  DYNAMIC
000420                             RECORD KEY               IS  ���|�敪�R�[�h
000430                                                          ���|���̃R�[�h
000440                             FILE STATUS              IS  ��ԃL�[
000450                             LOCK        MODE         IS  AUTOMATIC.
000460     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000470                             ORGANIZATION             IS  INDEXED
000480                             ACCESS MODE              IS  DYNAMIC
000490                             RECORD KEY               IS �{��|�{�p���ԍ�
000500                             FILE STATUS              IS  ��ԃL�[
000510                             LOCK        MODE         IS  AUTOMATIC.
000520     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000530                             ORGANIZATION             IS  INDEXED
000540                             ACCESS MODE              IS  DYNAMIC
000550                             RECORD KEY               IS  �ہ|�ی����
000560                                                          �ہ|�ی��Ҕԍ�
000570                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000580                                                          �ہ|�ی��Җ���
000590                                                          �ہ|�ی��Ҕԍ�
000600                             FILE STATUS              IS  ��ԃL�[
000610                             LOCK        MODE         IS  AUTOMATIC.
000620     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000630                             ORGANIZATION             IS  INDEXED
000640                             ACCESS MODE              IS  DYNAMIC
000650                             RECORD KEY               IS  �s�|������
000660                                                          �s�|�s�����ԍ�
000670                             ALTERNATE RECORD KEY     IS  �s�|������
000680                                                          �s�|�s��������
000690                                                          �s�|�s�����ԍ�
000700                             FILE STATUS              IS  ��ԃL�[
000710                             LOCK        MODE         IS  AUTOMATIC.
000720     SELECT  ������}�X�^    ASSIGN      TO        SEIKYUSL
000730                             ORGANIZATION             IS  INDEXED
000740                             ACCESS MODE              IS  DYNAMIC
000750                             RECORD KEY               IS  ����|�ی����
000760                                                          ����|�ی��Ҕԍ�
000770                             FILE STATUS              IS  ��ԃL�[
000780                             LOCK    MODE             IS  AUTOMATIC.
000910     SELECT  ����}�X�^    ASSIGN      TO        KAIJOHOL
000911                             ORGANIZATION             IS  INDEXED
000912                             ACCESS MODE              IS  DYNAMIC
000913                             RECORD KEY               IS  ���|�_���I���敪
000914                                                          ���|����R�[�h
000915                                                          ���|�ی����
000916                                                          ���|�ύX�a��N��
000917                             ALTERNATE RECORD KEY     IS  ���|�_���I���敪
000918                                                          ���|�ڍ��t��J�i
000919                                                          ���|����R�[�h
000920                                                          ���|�ی����
000921                                                          ���|�ύX�a��N��
000922                             FILE STATUS              IS  ��ԃL�[
000923                             LOCK        MODE         IS  AUTOMATIC.
000924     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4312L.DAT"
000925                             ORGANIZATION             IS  INDEXED
000930                             ACCESS                   IS  DYNAMIC
000940                             RECORD      KEY          IS  ��Q�|�����a��N��
000950                                                          ��Q�|�ی����
000960                                                          ��Q�|�ی��Ҕԍ�
000970                             ALTERNATE RECORD KEY     IS  ��Q�|�����a��N��
000980                                                          ��Q�|��
000990                                                          ��Q�|�������
001000                                                          ��Q�|�ی����
001010                                                          ��Q�|�ی��Ҕԍ�
001020                             ALTERNATE RECORD KEY     IS  ��Q�|�����a��N��
001030                                                          ��Q�|�������
001040                                                          ��Q�|�ی����
001050                                                          ��Q�|�ی��Ҕԍ�
001060*                                                          ��Q�|��
001070*/���ʁA�Еۓ��ق����܂Ƃ߁A�D����ʂɏW�v/081021
001080                             ALTERNATE RECORD KEY     IS  ��Q�|�����a��N��
001090                                                          ��Q�|���Q
001100                                                          ��Q�|�������
001110                                                          ��Q�|�ی����
001120                                                          ��Q�|�ی��Ҕԍ�
001130                             FILE        STATUS       IS  ��ԃL�[
001140                             LOCK        MODE         IS  AUTOMATIC.
001150     SELECT  ����t�@�C��    ASSIGN      TO         GS-PRTF001
001160                             SYMBOLIC    DESTINATION  IS "PRT"
001170                             FORMAT                   IS  ��`�̖��o
001180                             GROUP                    IS  ���ڌQ���o
001190                             PROCESSING  MODE         IS  ������ʂo
001200                             UNIT        CONTROL      IS  �g������o
001210                             FILE        STATUS       IS  �ʒm���o.
001220*
001230******************************************************************
001240*                      DATA DIVISION                             *
001250******************************************************************
001260 DATA                    DIVISION.
001270 FILE                    SECTION.
001280*                           �m�q�k��  �P�Q�W�n
001290 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001300     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001310*                           �m�q�k��  �Q�T�U�n
001320 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001330     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001340     COPY SEIGYO01        OF  XFDLIB  JOINING   ���O�P   AS  PREFIX.
001350*                           �m�q�k��  �P�Q�W�n
001360 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001370     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001380*                           �m�q�k��  �P�Q�W�n
001390 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001400     COPY SEJOHO          OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001410*                           �m�q�k��  �R�Q�O�n
001420 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001430     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001440*                           �m�q�k��  �Q�T�U�n
001450 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001460     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
001470*                           �m�q�k��  �P�Q�W�n
001480 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001490     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
001500*                           �m�q�k��  �U�S�O�n
001510 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
001520     COPY KAIJOHO         OF  XFDLIB  JOINING   ��� AS  PREFIX.
001530*
001540*
001550* �{�p/����  (�����a��N���́A�{�p/���� ���p) 
001560*                           �m�q�k��  �P�Q�W�n
001570 FD  ��ƃt�@�C���Q RECORD  CONTAINS 128 CHARACTERS.
001580 01  ��Q�|���R�[�h.
001590     03  ��Q�|���R�[�h�L�[.
001600         05  ��Q�|�����a��N��.
001610             07  ��Q�|�����a��              PIC 9.
001620             07  ��Q�|�����N                PIC 9(2).
001630             07  ��Q�|������                PIC 9(2).
001640         05  ��Q�|�ی����                  PIC 9(2).
001650         05  ��Q�|�ی��Ҕԍ�                PIC X(10).
001660         05  ��Q�|��                        PIC X(2).
001670         05  ��Q�|�������.
001680             07  ��Q�|��������P            PIC 9(2).
001690             07  ��Q�|��������Q            PIC 9.
001700     03  ��Q�|���R�[�h�f�[�^.
001710         05  ��Q�|�ی��Ҕԍ��Q              PIC X(10).
001720         05  ��Q�|����                      PIC 9(4).
001730         05  ��Q�|��p�z                    PIC 9(9).
001740         05  ��Q�|���S�z                    PIC 9(9).
001750         05  ��Q�|�����z                    PIC 9(9).
001760         05  ��Q�|�{�l����                  PIC 9(3).
001770         05  ��Q�|�{�l��p�z                PIC 9(7).
001780         05  ��Q�|�{�l���S�z                PIC 9(7).
001790         05  ��Q�|�{�l�����z                PIC 9(7).
001800         05  ��Q�|�Ƒ�����                  PIC 9(3).
001810         05  ��Q�|�Ƒ���p�z                PIC 9(7).
001820         05  ��Q�|�Ƒ����S�z                PIC 9(7).
001830         05  ��Q�|�Ƒ������z                PIC 9(7).
001840         05  ��Q�|���Q                      PIC X(2).
001850         05  FILLER                          PIC X(15).
001860*         05  FILLER                          PIC X(17).
001870* 
001880 FD  ����t�@�C��.
001890     COPY YJK437P         OF  XMDLIB.
001900*
001910******************************************************************
001920*                WORKING-STORAGE SECTION                         *
001930******************************************************************
001940 WORKING-STORAGE         SECTION.
001950 01 �L�[����                           PIC X    VALUE SPACE.
001960 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
001970 01 �I���t���O                         PIC X(3) VALUE SPACE.
001980 01 �I���t���O�P                       PIC X(3) VALUE SPACE.
001990 01 �m�F���͂v                         PIC X(1) VALUE SPACE.
002000 01 �t�@�C�����v                       PIC N(6) VALUE SPACE.
002010 01 �J�����g�����v                     PIC 9(1) VALUE ZERO.
002020 01 ���s�L�[�v                         PIC X(4) VALUE SPACE.
002030 01 �n��v                             PIC X(2) VALUE SPACE.
002040 01 ����ԍ��v                         PIC X(8) VALUE SPACE.
002050 01 �O�a��v                           PIC 9    VALUE ZERO.
002060 01 �s���t���O                         PIC X(3) VALUE SPACE.
002070 01 �����Ώۃt���O                     PIC X(4) VALUE SPACE.
002080 01 �t�@�C����                         PIC N(4) VALUE SPACE.
002090 01 �����ړ��L�[                       PIC X(4) VALUE SPACE.
002100 01 �����m�F�敪�v                     PIC 9    VALUE ZERO.
002110 01 �����ʉ߃L�[�v                     PIC X(3) VALUE SPACE.
002120 01 �s�J�E���^                         PIC 9(2) VALUE ZERO.
002130 01 ���v�s�J�E���^                     PIC 9(3) VALUE ZERO.
002140 01 �y�[�W�J�E���^                     PIC 9(3) VALUE ZERO.
002150 01 ����t���O                         PIC X(3) VALUE SPACE.
002160 01 �X�L�b�v�t���O                     PIC X(3) VALUE SPACE.
002170 01 �I�[�v���t���O                     PIC X(3) VALUE SPACE.
002171*
002180 01 ���v�p�v.
002190    03 �{�l�����J�E���g                PIC 9(9) VALUE ZERO.
002200    03 �Ƒ������J�E���g                PIC 9(9) VALUE ZERO.
002210    03 �v�����J�E���g                  PIC 9(9) VALUE ZERO.
002220    03 �{�l��p�z�J�E���g              PIC 9(9) VALUE ZERO.
002230    03 �Ƒ���p�z�J�E���g              PIC 9(9) VALUE ZERO.
002240    03 �v��p�z�J�E���g                PIC 9(9) VALUE ZERO.
002250    03 �{�l�����z�J�E���g              PIC 9(9) VALUE ZERO.
002260    03 �Ƒ������z�J�E���g              PIC 9(9) VALUE ZERO.
002270    03 �v�����z�J�E���g                PIC 9(9) VALUE ZERO.
002280 01 ���v                               PIC X(2) VALUE SPACE.
002290 01 ��������v                         PIC 9(3) VALUE ZERO.
002300 01 �łv                               PIC 9(2) VALUE ZERO.
002310*
002320 01 �ی��Җ��̂v.
002330    03 �ی��Җ��̂P�v                  PIC X(20) VALUE SPACE.
002340    03 �ی��Җ��̂Q�v                  PIC X(20) VALUE SPACE.
002350    03 �ی��Җ��̂R�v                  PIC X(20) VALUE SPACE.
002360*
002370 01 �����v�j                           PIC 9(9) VALUE ZERO.
002380 01 ��p�v�j                           PIC 9(9) VALUE ZERO.
002390 01 �����v�j                           PIC 9(9) VALUE ZERO.
002400 01 �S�p��                           PIC X(2)  VALUE X"8140".
002410 01 ���p��                           PIC X(2)  VALUE X"2020".
002420*
002430 01 �{�p�a��N���v.
002440     03 �{�p�a��v                     PIC 9    VALUE ZERO.
002450     03 �{�p�N���v.
002460        05 �{�p�N�v                    PIC 9(2) VALUE ZERO.
002470        05 �{�p���v                    PIC 9(2) VALUE ZERO.
002480        05 �{�p���v                    PIC 9(2) VALUE ZERO.
002490     03 �󗝔N���v.
002500        05 �󗝔N�v                    PIC 9(2) VALUE ZERO.
002510        05 �󗝌��v                    PIC 9(2) VALUE ZERO.
002520        05 �󗝓��v                    PIC 9(2) VALUE ZERO.
002530***
002540***
002550 01 ��ʏ��S�R�O�v.
002560    03 �����N���v.
002570       05 �����a��v                   PIC 9     VALUE ZERO.
002580       05 �����N�v                     PIC 9(2)  VALUE ZERO.
002590       05 �������v                     PIC 9(2)  VALUE ZERO.
002600    03 ��o�N�����v.
002610       05 ��o�a��v                   PIC 9     VALUE ZERO.
002620       05 ��o�N�v                     PIC 9(2)  VALUE ZERO.
002630       05 ��o���v                     PIC 9(2)  VALUE ZERO.
002640       05 ��o���v                     PIC 9(2)  VALUE ZERO.
002650    03 �����ނv                      PIC 9     VALUE ZERO.
002660*
002670**************
002680* �{�p����� *
002690**************
002700 01 �{�p�����v.
002710    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
002720    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
002730    03 �_���t�ԍ��v                    PIC X(20)  VALUE SPACE.
002740    03 �{�p���Z���v.
002750       05 �{�p���Z���P�v               PIC X(40)  VALUE SPACE.
002760       05 �{�p���Z���Q�v               PIC X(40)  VALUE SPACE.
002770    03 �{�p���X�֔ԍ��v.
002780       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
002790       05 �{�p���X�֔ԍ���؂v         PIC X(1)   VALUE SPACE.
002800       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
002810    03 �{�p���d�b�ԍ��v                PIC X(15)  VALUE SPACE.
002820    03 �������v.
002830        05 ������s���v              PIC X(40)  VALUE SPACE.
002840        05 ������s�x�X���v          PIC X(40)  VALUE SPACE.
002850        05 �a����ʂv                  PIC 9(1)   VALUE ZERO.
002860        05 ��s�ԍ��v                  PIC X(4)   VALUE SPACE.
002870        05 �X�ԍ��v                    PIC X(3)   VALUE SPACE.
002880        05 �����ԍ��v                  PIC X(10)  VALUE SPACE.
002890        05 �������`�l�J�i�v            PIC X(40)  VALUE SPACE.
002900        05 �������`�l�v                PIC X(40)  VALUE SPACE.
002910*
002920 01 �A�Ԃv                             PIC 9(3)   VALUE ZERO.
002930 01 ��s���x�X���v                     PIC X(40)  VALUE SPACE.
002940 01 �a����ʃR�����g�v                 PIC N(2)   VALUE SPACE.
002950 01 �T���v                             PIC N(4)   VALUE SPACE.
002960*
002970 01 �ی���ʂv                         PIC 9(2)  VALUE ZERO.
002980 01 �ی��Ҕԍ��v                       PIC X(10) VALUE SPACE.
002990 01 �����於�̂v                       PIC X(40) VALUE SPACE.
003000 01 �x���������v                       PIC X(40) VALUE SPACE.
003010 01 �����v                             PIC X(24) VALUE SPACE.
003020 01 �ی��҈����v.
003030     03 �ی��҈����P�v                 PIC X(40) VALUE SPACE.
003040     03 �ی��҈����Q�v                 PIC X(40) VALUE SPACE.
003050*
003060* �Еۗp
003070 01 �ڔ���敪�v                       PIC 9     VALUE ZERO.
003080*
003090 01 ����R�[�h�v                       PIC 9(2)  VALUE ZERO.
003100***
003110* ����������p�����^�p
003120*  �U�E�V���������i����敪 0:��� 1:������Ȃ��A�U���� 0:���� 1:�� 9:������Ȃ��j
003130*  ���Зp�������l1:������Ȃ�
003140***
003150 01 �������֘A�v.
003160        07 ���ۂU������敪�v          PIC 9 VALUE ZERO.
003170        07 ���ۂV������敪�v          PIC 9 VALUE ZERO.
003180        07 ���ۓ��Ј���敪�v          PIC 9 VALUE 1.
003190        07 ���ېU����敪�v            PIC 9 VALUE ZERO.
003200        07 �ЕۂU������敪�v          PIC 9 VALUE ZERO.
003210        07 �ЕۂV������敪�v          PIC 9 VALUE ZERO.
003220        07 �Еۓ��Ј���敪�v          PIC 9 VALUE 1.
003230        07 �ЕېU����敪�v            PIC 9 VALUE ZERO.
003240        07 �g���U������敪�v          PIC 9 VALUE 1.
003250        07 �g���V������敪�v          PIC 9 VALUE ZERO.
003260        07 �g�����Ј���敪�v          PIC 9 VALUE 1.
003270        07 �g���U����敪�v            PIC 9 VALUE ZERO.
003280        07 ���ςU������敪�v          PIC 9 VALUE 1.
003290        07 ���ςV������敪�v          PIC 9 VALUE ZERO.
003300        07 ���ϓ��Ј���敪�v          PIC 9 VALUE 1.
003310        07 ���ϐU����敪�v            PIC 9 VALUE ZERO.
003320        07 �V�l�U������敪�v          PIC 9 VALUE ZERO.
003330        07 �V�l�V������敪�v          PIC 9 VALUE ZERO.
003340        07 �V�l���Ј���敪�v          PIC 9 VALUE 1.
003350        07 �V�l�U����敪�v            PIC 9 VALUE ZERO.
003360        07 �����U������敪�v          PIC 9 VALUE 1.
003370        07 �����V������敪�v          PIC 9 VALUE 1.
003380        07 �������Ј���敪�v          PIC 9 VALUE 1.
003390        07 �����U����敪�v            PIC 9 VALUE ZERO.
003400*
003410*********************************************************************
003420 01 �������.
003430     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
003440     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
003450     03 ������ʂo                     PIC X(2) VALUE SPACE.
003460     03 �g������o.
003470         05 �[������o.
003480             07 �ړ������o             PIC X(1) VALUE SPACE.
003490             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
003500         05 �ڍא���o                 PIC X(2) VALUE SPACE.
003510     03 �ʒm���o                     PIC X(2) VALUE SPACE.
003520     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
003530*********************************************************************
003540*
003550 01 �v�Z�@����N�v                     PIC 9(2).
003560* ���t�v�n�q�j
003570 01 �a��I���N�v                       PIC 9(4).
003580 01 �v�Z�@�a��N�v                     PIC 9(2).
003590 01 �v�Z�@����.
003600    03 �v�Z�@����N                    PIC 9(4).
003610    03 �v�Z�@�����                  PIC 9(4).
003620 01 �v�Z�@����q REDEFINES �v�Z�@����.
003630    03 �v�Z�@���I                      PIC 9(2).
003640    03 �v�Z�@���t                      PIC 9(6).
003650    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
003660       05 �v�Z�@�N��                   PIC 9(4).
003670       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
003680         07 �v�Z�@�N                   PIC 9(2).
003690         07 �v�Z�@��                   PIC 9(2).
003700       05 �v�Z�@��                     PIC 9(2).
003710*
003720******************************************************************
003730*                          �A������                              *
003740******************************************************************
003750*
003760********************
003770* ���b�Z�[�W�\���L�[ *
003780********************
003790 01 �A���|�L�[ IS EXTERNAL.
003800    03  �A���|���b�Z�[�W                 PIC N(20).
003810*
003820*
003830 01 �A���|��ʏ��x�i�j�S�R�O   IS EXTERNAL.
003840    03 �A���|�����N��.
003850       05 �A���|�����a��               PIC 9.
003860       05 �A���|�����N                 PIC 9(2).
003870       05 �A���|������                 PIC 9(2).
003880    03 �A���|��o�N����.
003890       05 �A���|��o�a��               PIC 9.
003900       05 �A���|��o�N                 PIC 9(2).
003910       05 �A���|��o��                 PIC 9(2).
003920       05 �A���|��o��                 PIC 9(2).
003930    03 �A���|���Z�v�g���              PIC X(4).
003940    03 �A���|�ی����                  PIC 9(2).
003950    03 �A���|������                  PIC 9.
003960    03 �A���|�{�l�Ƒ�                  PIC 9.
003970    03 �A���|�p�����                  PIC 9.
003980    03 �A���|�������O                  PIC 9.
003990    03 �A���|���i�h�r                  PIC X(2).
004000    03 �A���|���ǂi�h�r                PIC X(2).
004010*
004020 01 �A���|��ʏ��x�i�j�S�R�O�ǉ�   IS EXTERNAL.
004030    03 �A���|�ꊇ�敪    PIC 9.
004050    03 �A���|�v���r���[�敪            PIC 9.
004051*
004052************************************
004053* �v�����^�t�@�C���쐬�p           *
004054************************************
004055 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
004056     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
004057     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
004058     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
004059     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
004060*
004061******************************************************************
004062*                      PROCEDURE  DIVISION                       *
004070******************************************************************
004080 PROCEDURE               DIVISION.
004090************
004100*           *
004110* ��������   *
004120*           *
004130************
004140     PERFORM �v�����^�t�@�C���쐬.
004141     PERFORM ������.
004150     PERFORM ������擾�Q.
004160************
004170*           *
004180* �又��     *
004190*           *
004200************
004210     PERFORM �������.
004220************
004230*           *
004240* �I������   *
004250*           *
004260************
004270     PERFORM �I������.
004280     EXIT PROGRAM.
004290*
004300*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
004310*================================================================*
004311 �v�����^�t�@�C���쐬 SECTION.
004312*================================================================*
004313*   / ������ /
004314     MOVE SPACE TO �g�A�o�q�s�e�|�쐬�f�[�^.
004315     INITIALIZE �g�A�o�q�s�e�|�쐬�f�[�^.
004316*
004317*
004318*--���� �ύX�ӏ� ����--------------------------------------*
004319*   �g�p����v�����^�t�@�C�����Z�b�g
004320     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
004321*
004322*   �g�p���钠�[�v���O�������Z�b�g
004323     MOVE "YJK437"              TO �g�A�o�q�s�e�|���[�v���O������.
004324*
004325*--����-----------------------------------------------------*
004326*
004327*   / �v���r���[�敪�Z�b�g /
004328     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
004329*
004330     CALL   "CRTPRTF".
004331     CANCEL "CRTPRTF".
004332*
004333*================================================================*
004334 ������ SECTION.
004335*
004340     OPEN INPUT �����}�X�^.
004350             MOVE NC"����" TO �t�@�C�����v.
004360             PERFORM �I�[�v���`�F�b�N.
004370     OPEN INPUT ������}�X�^.
004380             MOVE NC"����" TO �t�@�C�����v.
004390             PERFORM �I�[�v���`�F�b�N.
004400     OPEN INPUT ���̃}�X�^.
004410             MOVE NC"����" TO �t�@�C�����v.
004420             PERFORM �I�[�v���`�F�b�N.
004430     OPEN INPUT �{�p�����}�X�^
004440             MOVE NC"�{��" TO �t�@�C�����v.
004450             PERFORM �I�[�v���`�F�b�N.
004460     OPEN INPUT �ی��҃}�X�^
004470             MOVE NC"�ی���" TO �t�@�C����.
004480             PERFORM �I�[�v���`�F�b�N.
004490     OPEN INPUT �s�����}�X�^
004500             MOVE NC"�s����" TO �t�@�C�����v.
004510             PERFORM �I�[�v���`�F�b�N.
004520     OPEN INPUT ������}�X�^
004530             MOVE NC"������" TO �t�@�C�����v.
004540             PERFORM �I�[�v���`�F�b�N.
004550     OPEN INPUT   ����}�X�^
004560             MOVE NC"����" TO �t�@�C�����v.
004570             PERFORM �I�[�v���`�F�b�N.
004580     OPEN INPUT ��ƃt�@�C���Q.
004590             MOVE NC"��Q" TO �t�@�C�����v.
004600             PERFORM �I�[�v���`�F�b�N.
004610*
004620*
004630*    /* ���ݓ��t�擾 */
004640     ACCEPT �v�Z�@���t FROM DATE.
004650*    /* 1980�`2079�N�̊ԂŐݒ� */
004660     IF �v�Z�@�N > 80
004670         MOVE 19 TO �v�Z�@���I
004680     ELSE
004690         MOVE 20 TO �v�Z�@���I
004700     END-IF.
004710*
004720     PERFORM �J�����g�����擾.
004730     PERFORM �a��I���N�擾.
004740     COMPUTE �v�Z�@�a��N�v = �v�Z�@����N - �a��I���N�v.
004750     MOVE �v�Z�@�a��N�v TO �{�p�N�v.
004760     MOVE �v�Z�@��       TO �{�p���v.
004770     MOVE �v�Z�@��       TO �{�p���v.
004780*
004790     PERFORM �A�����ڑޔ�.
004800     PERFORM �{�p�����擾.
004810*
004820*================================================================*
004830 �I�[�v���`�F�b�N SECTION.
004840*
004850     IF ��ԃL�[  NOT =  "00"
004860         DISPLAY �t�@�C�����v NC"�e�I�[�v���G���[" UPON CONS
004870         DISPLAY NC"��ԃL�[�F" ��ԃL�[           UPON CONS
004880         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004890                                                   UPON CONS
004900         ACCEPT  �L�[���� FROM CONS
004910         PERFORM �t�@�C����
004920         EXIT PROGRAM.
004930*================================================================*
004940 �J�����g�����擾 SECTION.
004950*
004960     MOVE ZEROS TO ���|����敪.
004970     READ ������}�X�^
004980     NOT INVALID KEY
004990         MOVE ���|�J�����g���� TO �J�����g�����v
005000     END-READ.
005010*
005020*================================================================*
005030 �a��I���N�擾 SECTION.
005040*
005050*     DISPLAY NC"�J�����g�����v"  �J�����g�����v UPON MSGBOX.
005060     MOVE �J�����g�����v TO ���|�����敪.
005070     READ �����}�X�^
005080     INVALID KEY
005090         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
005100         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005110                                                  UPON CONS
005120         ACCEPT  �L�[���� FROM CONS
005130         PERFORM �I������
005140         EXIT PROGRAM
005150     NOT INVALID KEY
005160         COMPUTE �O�a��v = �J�����g�����v - 1
005170         MOVE �O�a��v TO ���|�����敪
005180         READ �����}�X�^
005190         INVALID KEY
005200             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
005210             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005220                                                      UPON CONS
005230             ACCEPT  �L�[���� FROM CONS
005240             PERFORM �I������
005250             EXIT PROGRAM
005260         NOT INVALID KEY
005270             MOVE ���|�I������N TO �a��I���N�v
005280         END-READ
005290     END-READ.
005300*
005310*================================================================*
005320 �A�����ڑޔ� SECTION.
005330*
005340     MOVE �A���|�����a��  TO �����a��v.
005350     MOVE �A���|�����N    TO �����N�v.
005360     MOVE �A���|������    TO �������v.
005370     MOVE �A���|��o�a��  TO ��o�a��v.
005380     MOVE �A���|��o�N    TO ��o�N�v.
005390     MOVE �A���|��o��    TO ��o���v.
005400     MOVE �A���|��o��    TO ��o���v.
005410     MOVE �A���|������  TO �����ނv.
005420*
005430*================================================================*
005440 �{�p�����擾 SECTION.
005450*
005460     MOVE ZERO  TO �{��|�{�p���ԍ�.
005470     READ �{�p�����}�X�^
005480     INVALID KEY
005490         CONTINUE
005500     NOT INVALID KEY
005510*
005520         MOVE �{��|�X�֔ԍ��P       TO �{�p���X�֔ԍ��P�v
005530         MOVE "-"                    TO �{�p���X�֔ԍ���؂v
005540         MOVE �{��|�X�֔ԍ��Q       TO �{�p���X�֔ԍ��Q�v
005550         MOVE �{��|��\�Җ�         TO ��\�Җ��v
005560         MOVE �{��|�ڍ��@��         TO �ڍ��@���v
005570         STRING �{��|�Z���P  DELIMITED BY SPACE
005580                �{��|�Z���Q  DELIMITED BY SPACE
005590           INTO �{�p���Z���v
005600         END-STRING
005610         MOVE �{��|�d�b�ԍ�         TO �{�p���d�b�ԍ��v
005620         MOVE �{��|�V�_���t�ԍ�     TO �_���t�ԍ��v
005630*
005640         MOVE �{��|������s��     TO ������s���v
005650         MOVE �{��|������s�x�X�� TO ������s�x�X���v
005660         MOVE �{��|�a�����         TO �a����ʂv
005670         MOVE �{��|��s�ԍ�         TO ��s�ԍ��v
005680         MOVE �{��|�X�ԍ�           TO �X�ԍ��v
005690         MOVE �{��|�����ԍ�         TO �����ԍ��v
005700         MOVE �{��|�������`�l�J�i   TO �������`�l�J�i�v
005710         MOVE �{��|�������`�l       TO �������`�l�v
005720         STRING ������s���v     DELIMITED BY SPACE
005730                " "                DELIMITED BY SIZE
005740                ������s�x�X���v DELIMITED BY SPACE
005750                INTO ��s���x�X���v
005760         END-STRING
005770         EVALUATE �a����ʂv
005780         WHEN 1
005790             MOVE NC"����" TO �a����ʃR�����g�v
005800         WHEN 2
005810             MOVE NC"����" TO �a����ʃR�����g�v
005820         WHEN OTHER
005830             MOVE SPACE    TO �a����ʃR�����g�v
005840         END-EVALUATE
005850*
005860     END-READ.
005870*================================================================*
005880 �t�@�C���� SECTION.
005890*
005900     CLOSE �����}�X�^ ������}�X�^   ��ƃt�@�C���Q
005910           ���̃}�X�^ �{�p�����}�X�^ �s�����}�X�^ 
005920           �ی��҃}�X�^ ������}�X�^   ����}�X�^.
005930*================================================================*
005940 �I������ SECTION.
005950*
005960     PERFORM �t�@�C����.
005970*
005980*================================================================*
005990 �G���[�\�� SECTION.
006000*
006010     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
006020     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
006030     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
006040     ACCEPT  �L�[���� FROM CONS.
006050*================================================================*
006060 �G���[�\���q SECTION.
006070*
006080     DISPLAY NC"�t�@�C���Ǎ��G���[" �t�@�C����     UPON CONS.
006090     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
006100     ACCEPT  �L�[���� FROM CONS.
006110*================================================================*
006120 ��ƃt�@�C���Q�Ǎ� SECTION.
006130*
006140     READ ��ƃt�@�C���Q NEXT
006150     AT END
006160         MOVE "YES" TO �I���t���O
006170     END-READ.
006180*================================================================*
006190 ������� SECTION.
006200*
006240     MOVE 1       TO �łv.
006250*
006260* / ���L�[(�������+�ی����+�ی��Ҕԍ�)�œǂݍ���./
006270     MOVE ZERO      TO  ��Q�|�����a��.
006280     MOVE ZERO      TO  ��Q�|�����N.
006290     MOVE ZERO      TO  ��Q�|������.
006300*     MOVE LOW-VALUE TO  ��Q�|��.
006310     MOVE ZERO      TO  ��Q�|��.
006320     MOVE ZERO      TO  ��Q�|�������.
006330     MOVE ZERO      TO  ��Q�|�ی����.
006340     MOVE LOW-VALUE TO  ��Q�|�ی��Ҕԍ�.
006350*/���ʁA�Еۓ��ق����܂Ƃ߁A�D����ʂɏW�v����/081021
006360     MOVE ZERO      TO  ��Q�|���Q.
006370*     START ��ƃt�@�C���Q   KEY IS >=  ��Q�|�����a��N��
006380*                                       ��Q�|�������
006390*                                       ��Q�|�ی����
006400*                                       ��Q�|�ی��Ҕԍ�
006410**                                       ��Q�|��
006420     START ��ƃt�@�C���Q   KEY IS >=  ��Q�|�����a��N��
006430                                       ��Q�|���Q
006440                                       ��Q�|�������
006450                                       ��Q�|�ی����
006460                                       ��Q�|�ی��Ҕԍ�
006470*/���ʁA�Еۓ��ق����܂Ƃ߁A�D����ʂɏW�v����/081021
006480     END-START.
006490     IF ��ԃL�[ = "00"
006500         MOVE SPACE TO �I���t���O
006510         PERFORM ��ƃt�@�C���Q�Ǎ�
006520         IF  �I���t���O = "YES"
006530             MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
006540             CALL   "MSG001"
006550             CANCEL "MSG001"
006560             PERFORM �t�@�C����
006570             MOVE 99 TO PROGRAM-STATUS
006580             EXIT PROGRAM
006590         END-IF
006600*
006610         PERFORM UNTIL �I���t���O = "YES"
006620             MOVE SPACE TO �X�L�b�v�t���O
006630             PERFORM ����Ώۃ`�F�b�N
006640             IF ����t���O = "YES"
006650*
006660             MOVE SPACE TO YJK437P
006670             INITIALIZE    YJK437P
006680             MOVE ��Q�|��         TO ���v
006690             MOVE ��Q�|������� TO ��������v
006700             PERFORM  �\�Z�b�g�P
006710
006720* �ЕہA�D���A���قƁA���ρA���q���͂܂Ƃ߂Ĉ󎚂���
006730             IF ��������v(2:1) = 1 OR 9
006740* 
006750                 PERFORM VARYING �s�J�E���^ FROM 1 BY 1
006760                         UNTIL ( �s�J�E���^ > 8       ) OR
006770                               ( ���v       NOT = ��Q�|��       ) OR
006780                               ( ��������v(2:1) NOT = ��Q�|�������(2:1) ) OR
006790                               ( �I���t���O = "YES" )
006800                      PERFORM ����Ώۃ`�F�b�N
006810                      IF ����t���O = "YES"
006820                         PERFORM �\�Z�b�g�Q
006830*/�������Ō�A���R�[�h������A����Ώۂ��Ȃ��ꍇ�A���v���������Ȃ�/0510
006840*                  ELSE
006850*                     COMPUTE �s�J�E���^ =  �s�J�E���^ - 1
006860                      END-IF
006870                      PERFORM ��ƃt�@�C���Q�Ǎ�
006880                      PERFORM ����Ώۃ`�F�b�N
006890                      IF ����t���O = SPACE
006900                         COMPUTE �s�J�E���^ =  �s�J�E���^ - 1
006910                      END-IF
006920*/�������Ō�A���R�[�h������A����Ώۂ��Ȃ��ꍇ�A���v���������Ȃ�/0510
006930                      MOVE "YES" TO �X�L�b�v�t���O
006940                 END-PERFORM
006950*
006960                 IF ( �I���t���O =  "YES" ) OR 
006970                    ( ��������v(2:1) NOT = ��Q�|�������(2:1) ) OR
006980                    ( ���v       NOT = ��Q�|�� )
006990                     MOVE �{�l�����J�E���g   TO     �{�l�������v
007000                     MOVE �Ƒ������J�E���g   TO     �Ƒ��������v
007010                     MOVE �v�����J�E���g     TO     �v�������v
007020                     MOVE �{�l��p�z�J�E���g TO     �{�l��p�z���v
007030                     MOVE �Ƒ���p�z�J�E���g TO     �Ƒ���p�z���v
007040                     MOVE �v��p�z�J�E���g   TO     �v��p�z���v
007050                     MOVE ZERO   TO �łv
007060*
007070                     INITIALIZE ���v�p�v
007080                 ELSE
007090                     MOVE ZERO     TO     �{�l�������v
007100                     MOVE ZERO     TO     �Ƒ��������v
007110                     MOVE ZERO     TO     �v�������v
007120                     MOVE ZERO     TO     �{�l��p�z���v
007130                     MOVE ZERO     TO     �Ƒ���p�z���v
007140                     MOVE ZERO     TO     �v��p�z���v
007150                     MOVE ZERO     TO     �{�l�����z���v
007160                     MOVE ZERO     TO     �Ƒ������z���v
007170                     MOVE ZERO     TO     �v�����z���v
007180                 END-IF
007190
007200             ELSE
007210* �ЕہA�D���A���فA���ρA���q���ȊO
007220* 
007230                 PERFORM VARYING �s�J�E���^ FROM 1 BY 1
007240                         UNTIL ( �s�J�E���^ > 8       ) OR
007250                               ( ���v       NOT = ��Q�|��       ) OR
007260                               ( ��������v NOT = ��Q�|������� ) OR
007270                               ( �I���t���O = "YES" )
007280                      PERFORM ����Ώۃ`�F�b�N
007290                      IF ����t���O = "YES"
007300                          PERFORM �\�Z�b�g�Q
007310*/�������Ō�A���R�[�h������A����Ώۂ��Ȃ��ꍇ�A���v���������Ȃ�/0510
007320*                  ELSE
007330*                     COMPUTE �s�J�E���^ =  �s�J�E���^ - 1
007340                      END-IF
007350                      PERFORM ��ƃt�@�C���Q�Ǎ�
007360                      PERFORM ����Ώۃ`�F�b�N
007370                      IF ����t���O = SPACE
007380                          COMPUTE �s�J�E���^ =  �s�J�E���^ - 1
007390                      END-IF
007400*/�������Ō�A���R�[�h������A����Ώۂ��Ȃ��ꍇ�A���v���������Ȃ�/0510
007410                      MOVE "YES" TO �X�L�b�v�t���O
007420                 END-PERFORM
007430*
007440                 IF ( �I���t���O =  "YES" ) OR 
007450                    ( ��������v NOT = ��Q�|������� ) OR
007460                    ( ���v       NOT = ��Q�|�� )
007470                     MOVE �{�l�����J�E���g   TO     �{�l�������v
007480                     MOVE �Ƒ������J�E���g   TO     �Ƒ��������v
007490                     MOVE �v�����J�E���g     TO     �v�������v
007500                     MOVE �{�l��p�z�J�E���g TO     �{�l��p�z���v
007510                     MOVE �Ƒ���p�z�J�E���g TO     �Ƒ���p�z���v
007520                     MOVE �v��p�z�J�E���g   TO     �v��p�z���v
007530                     MOVE ZERO               TO     �łv
007540*
007550                     INITIALIZE ���v�p�v
007560                 ELSE
007570                     MOVE ZERO     TO     �{�l�������v
007580                     MOVE ZERO     TO     �Ƒ��������v
007590                     MOVE ZERO     TO     �v�������v
007600                     MOVE ZERO     TO     �{�l��p�z���v
007610                     MOVE ZERO     TO     �Ƒ���p�z���v
007620                     MOVE ZERO     TO     �v��p�z���v
007630                     MOVE ZERO     TO     �{�l�����z���v
007640                     MOVE ZERO     TO     �Ƒ������z���v
007650                     MOVE ZERO     TO     �v�����z���v
007660                 END-IF
007670
007680             END-IF
007690*
007700             PERFORM �󎚏���
007710             PERFORM ���ŏ���
007720*
007730             COMPUTE �łv = �łv + 1
007740           END-IF
007750*
007760           IF �X�L�b�v�t���O NOT = "YES"
007770              PERFORM ��ƃt�@�C���Q�Ǎ�
007780           END-IF
007790         END-PERFORM
007800*
007810     ELSE
007820         MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
007830         CALL   "MSG001"
007840         CANCEL "MSG001"
007850         PERFORM �t�@�C����
007860         MOVE 99 TO PROGRAM-STATUS
007870         EXIT PROGRAM
007880     END-IF.
007890*
007900     CLOSE ����t�@�C��.
007910*
007920*================================================================*
007930 �\�Z�b�g�P SECTION.
007940*
007950     MOVE �łv                 TO ��.
007960
007970* �����̘a����擾
007980     MOVE �����a��v         TO ���|�����敪.
007990     READ �����}�X�^
008000     INVALID KEY
008010         MOVE SPACE          TO �����a���
008020     NOT INVALID KEY
008030         MOVE ���|��������   TO �����a���
008040     END-READ.
008050*
008060     MOVE �����N�v           TO �����N.
008070     MOVE �������v           TO ������.
008080*
008090     MOVE ��\�Җ��v         TO ��\�Җ�.
008100     MOVE �ڍ��@���v         TO �ڍ��@��.
008110     MOVE �_���t�ԍ��v(3:7)  TO �_���t�ԍ��P.
008120     MOVE �_���t�ԍ��v(11:1) TO �_���t�ԍ��Q.
008130     MOVE �_���t�ԍ��v(13:1) TO �_���t�ԍ��R.
008140     PERFORM �����Z�b�g.
008150     PERFORM �ی���ʖ��Z�b�g.
008160*
008170*================================================================*
008180 �\�Z�b�g�Q SECTION.
008190*
008200     MOVE ��Q�|�{�l����     TO �{�l����(�s�J�E���^).
008210     MOVE ��Q�|�{�l��p�z   TO �{�l��p�z(�s�J�E���^).
008220*
008230     MOVE ��Q�|�Ƒ�����     TO �Ƒ�����(�s�J�E���^).
008240     MOVE ��Q�|�Ƒ���p�z   TO �Ƒ���p�z(�s�J�E���^).
008250*
008260     MOVE ��Q�|����    TO  �v����(�s�J�E���^).
008270     MOVE ��Q�|��p�z  TO  �v��p�z(�s�J�E���^).
008280*
008290     ADD ��Q�|�{�l����   TO  �{�l�����J�E���g.
008300     ADD ��Q�|�Ƒ�����   TO  �Ƒ������J�E���g.
008310     ADD ��Q�|����       TO  �v�����J�E���g.
008320     ADD ��Q�|�{�l��p�z TO  �{�l��p�z�J�E���g.
008330     ADD ��Q�|�Ƒ���p�z TO  �Ƒ���p�z�J�E���g.
008340     ADD ��Q�|��p�z     TO  �v��p�z�J�E���g.
008350     ADD ��Q�|�{�l�����z TO  �{�l�����z�J�E���g.
008360     ADD ��Q�|�Ƒ������z TO  �Ƒ������z�J�E���g.
008370*     ADD ��Q�|�����z     TO  �v�����z�J�E���g.
008380**
008390* �ی���
008400     MOVE ��Q�|�ی����     TO �ی���ʂv.
008410     MOVE ��Q�|�ی��Ҕԍ�   TO �ی��Ҕԍ��v.
008420     EVALUATE �ی���ʂv
008430     WHEN 1 THRU 4
008440     WHEN 6 THRU 9
008450         PERFORM �ی��ҏ��擾
008460     WHEN 5
008470     WHEN 50 THRU 60
008480         IF (��Q�|�ی���� = 05) AND (��Q�|�ی��Ҕԍ�(1:2) = "39")
008490             PERFORM ���������擾
008500         ELSE
008510             PERFORM �s�������擾
008520         END-IF
008530     END-EVALUATE.
008540     PERFORM ���������擾.
008550*
008560     MOVE �ی��҈����v  TO �ی��Җ��̂v.
008570*
008580*
008590     COMPUTE ���v�s�J�E���^ = �s�J�E���^ * 3.
008600     MOVE �ی��Җ��̂P�v TO �ی��Җ�(���v�s�J�E���^ - 1).
008610     IF ��Q�|�ی���� = "50" OR "51" OR "52" OR "53" OR
008620                         "54" OR "55" OR "60" OR "05" OR "08"
008630        IF �ی��Җ��̂Q�v NOT = SPACE AND �ی��Җ��̂R�v = SPACE
008640           MOVE �ی��Җ��̂P�v TO �ی��Җ�(���v�s�J�E���^ - 2)
008650           MOVE �ی��Җ��̂Q�v TO �ی��Җ�(���v�s�J�E���^ - 1)
008660        END-IF
008670     ELSE
008680        IF �ی��Җ��̂Q�v NOT = SPACE AND �ی��Җ��̂R�v = SPACE
008690           MOVE �ی��Җ��̂P�v TO �ی��Җ�(���v�s�J�E���^ - 1)
008700           MOVE �ی��Җ��̂Q�v TO �ی��Җ�(���v�s�J�E���^)
008710        END-IF
008720     END-IF.
008730     IF ��Q�|�ی���� = "50" OR "51" OR "52" OR "53" OR
008740                         "54" OR "55" OR "60" OR "05" OR "08"
008750        IF �ی��Җ��̂Q�v NOT = SPACE AND �ی��Җ��̂R�v NOT = SPACE
008760           MOVE �ی��Җ��̂P�v TO �ی��Җ�(���v�s�J�E���^ - 2)
008770           MOVE �ی��Җ��̂Q�v TO �ی��Җ�(���v�s�J�E���^ - 1)
008780        END-IF
008790     ELSE
008800        IF �ی��Җ��̂Q�v NOT = SPACE AND �ی��Җ��̂R�v NOT = SPACE
008810           MOVE �ی��Җ��̂P�v TO �ی��Җ�(���v�s�J�E���^ - 2)
008820           MOVE �ی��Җ��̂Q�v TO �ی��Җ�(���v�s�J�E���^ - 1)
008830           MOVE �ی��Җ��̂R�v TO �ی��Җ�(���v�s�J�E���^)
008840        END-IF
008850     END-IF.
008860*
008870*================================================================*
008880 �G���[�����o SECTION.
008890*
008900     IF �ʒm���o NOT = "00"
008910         DISPLAY NC"���[�G���["              UPON CONS
008920         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
008930         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
008940         DISPLAY NC"�g������o�F" �g������o UPON CONS
008950         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
008960                                             UPON CONS
008970         ACCEPT  �L�[���� FROM CONS
008980         PERFORM �t�@�C����
008990         MOVE 99  TO PROGRAM-STATUS
009000         EXIT PROGRAM
009010     END-IF.
009020*================================================================*
009030 �󎚏���  SECTION.
009040*
009051     IF ( �I�[�v���t���O NOT = "YES" )
009052        MOVE "YES" TO �I�[�v���t���O
009053        OPEN I-O  ����t�@�C��
009054        PERFORM �G���[�����o
009056     END-IF.
009057*
009058     MOVE "YJK437P" TO  ��`�̖��o.
009060     MOVE SPACE     TO  ������ʂo.
009070     MOVE "SCREEN"  TO  ���ڌQ���o.
009081     WRITE YJK437P.
009091     PERFORM �G���[�����o.
009100*================================================================*
009110 ���ŏ���  SECTION.
009120
009130     MOVE "YJK437P" TO  ��`�̖��o.
009140     MOVE "CT"      TO  ������ʂo.
009150     MOVE "PAGE"    TO  �g������o.
009160     MOVE SPACE     TO  ���ڌQ���o.
009170     WRITE YJK437P.
009180     PERFORM �G���[�����o.
009190     MOVE SPACE     TO  �g������o.
009200*
009210*================================================================*
009220 �ی��ҏ��擾 SECTION.
009230*
009240     MOVE  SPACE         TO �����於�̂v.
009250     MOVE  SPACE         TO �x���������v.
009260     MOVE  ZERO          TO �ڔ���敪�v.
009270*
009280     MOVE �ی���ʂv     TO �ہ|�ی����.
009290     MOVE �ی��Ҕԍ��v   TO �ہ|�ی��Ҕԍ�.
009300     READ �ی��҃}�X�^
009310     INVALID KEY
009320         MOVE SPACE      TO �����於�̂v
009330         MOVE SPACE      TO �x���������v
009340     NOT INVALID KEY
009350         IF �ہ|��������敪 = 1
009360             MOVE �ہ|�ی����   TO ����|�ی����
009370             MOVE �ہ|�ی��Ҕԍ� TO ����|�ی��Ҕԍ�
009380             READ ������}�X�^
009390             INVALID KEY
009400                 MOVE SPACE             TO �����於�̂v
009410                 MOVE SPACE             TO �x���������v
009420             NOT INVALID KEY
009430                 MOVE ����|�ی��Җ���  TO �����於�̂v
009440                 MOVE ����|�x��������  TO �x���������v
009450             END-READ
009460         ELSE
009470             MOVE �ہ|�ی��Җ���        TO �����於�̂v
009480             MOVE �ہ|�x��������        TO �x���������v
009490             MOVE �ہ|�ڔ���敪        TO �ڔ���敪�v
009500         END-IF
009510     END-READ.
009520*================================================================*
009530 �s�������擾 SECTION.
009540*
009550     MOVE  SPACE         TO �����於�̂v.
009560     MOVE  SPACE         TO �x���������v.
009570*
009580     MOVE �ی���ʂv               TO �s�|������.
009590     MOVE �ی��Ҕԍ��v             TO �s�|�s�����ԍ�.
009600     READ �s�����}�X�^
009610     INVALID KEY
009620         MOVE SPACE                TO �����於�̂v
009630         MOVE SPACE                TO �x���������v
009640     NOT INVALID KEY
009650         IF �s�|������敪 = 1
009660             MOVE �ی���ʂv       TO ����|�ی����
009670             MOVE �ی��Ҕԍ��v     TO ����|�ی��Ҕԍ�
009680             READ ������}�X�^
009690             INVALID KEY
009700                 MOVE SPACE        TO �����於�̂v
009710                 MOVE SPACE        TO �x���������v
009720             NOT INVALID KEY
009730                 MOVE ����|�ی��Җ���   TO �����於�̂v
009740                 MOVE ����|�x��������   TO �x���������v
009750             END-READ
009760          ELSE
009770             MOVE �s�|�s��������   TO �����於�̂v
009780             MOVE �s�|�x��������   TO �x���������v
009790          END-IF
009800      END-READ.
009810*================================================================*
009820 ���������擾 SECTION.
009830*
009840     MOVE SPACE TO �ی��҈����v.
009850     IF �����於�̂v NOT = SPACE
009860         EVALUATE �ی���ʂv
009870         WHEN 2
009880             IF �ڔ���敪�v = 1
009890                MOVE SPACE            TO �����v
009900             ELSE
009910                MOVE "�Љ�ی�������" TO �����v
009920             END-IF
009930         WHEN 6
009940             IF �ڔ���敪�v = 1
009950                MOVE "�i���فj"               TO �����v
009960             ELSE
009970                MOVE "�Љ�ی��������i���فj" TO �����v
009980             END-IF
009990         WHEN 7
010000             MOVE "�i�D���j"       TO �����v
010010         WHEN 3
010020             MOVE "���N�ی��g��"   TO �����v
010030         WHEN 4
010040             MOVE "���ϑg��"       TO �����v
010050         WHEN OTHER
010060             MOVE SPACE            TO �����v
010070         END-EVALUATE
010080*
010090         IF �x���������v = SPACE
010100             STRING  �����於�̂v  DELIMITED BY SPACE
010110                     �����v        DELIMITED BY SPACE
010120                    INTO �ی��҈����v
010130             END-STRING
010140         ELSE
010150             STRING  �����於�̂v  DELIMITED BY SPACE
010160                     �����v        DELIMITED BY SPACE
010170                     �x���������v  DELIMITED BY SPACE
010180                    INTO �ی��҈����v
010190             END-STRING
010200         END-IF
010210     END-IF.
010220*
010230*================================================================*
010240 ������擾�Q SECTION.
010250* �����l�ݒ�
010260     MOVE ZERO  TO ���ۂU������敪�v. 
010270     MOVE ZERO  TO �ЕۂU������敪�v. 
010280     MOVE 1     TO �g���U������敪�v. 
010290     MOVE 1     TO ���ςU������敪�v. 
010300     MOVE ZERO  TO �V�l�U������敪�v. 
010310     MOVE 1     TO �����U������敪�v. 
010320*
010330*================================================================*
010340 ����Ώۃ`�F�b�N  SECTION.
010350*
010360*  ����敪�ɂ��U�蕪�� �� �U�E�V���������i����敪 0:��� 1:������Ȃ��j
010370* �i�ꊇ����̂݁j
010380*
010390     MOVE SPACE TO ����t���O.
010400*
010410     IF �A���|�ꊇ�敪 NOT = 1
010420        MOVE "YES" TO ����t���O
010430     ELSE
010440        EVALUATE ��Q�|�ی����
010450        WHEN 01
010460        WHEN 08
010470           IF ���ۂU������敪�v NOT = 1
010480              MOVE "YES" TO ����t���O
010490           END-IF
010500        WHEN 02
010510        WHEN 06
010520        WHEN 07
010530           IF �ЕۂU������敪�v NOT = 1
010540              MOVE "YES" TO ����t���O
010550           END-IF
010560        WHEN 03
010570           IF �g���U������敪�v NOT = 1
010580              MOVE "YES" TO ����t���O
010590           END-IF
010600        WHEN 04
010610        WHEN 09
010620           IF ���ςU������敪�v NOT = 1
010630              MOVE "YES" TO ����t���O
010640           END-IF
010650        WHEN 05
010660           IF �V�l�U������敪�v NOT = 1
010670              MOVE "YES" TO ����t���O
010680           END-IF
010690        WHEN 50 THRU 60
010700           IF �����U������敪�v NOT = 1
010710              MOVE "YES" TO ����t���O
010720           END-IF
010730        WHEN OTHER
010740           MOVE "YES" TO ����t���O
010750        END-EVALUATE
010760     END-IF.
010770*
010780*/�����s�̍��ۑސE�V�l�㍂�͈�����Ȃ�(�ʗp��)/080516
010790     EVALUATE TRUE
010800     WHEN (��Q�|�ی���� = 01)       AND (��Q�|�ی��Ҕԍ�(1:2) = "13")
010810     WHEN (��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = "13")
010820         MOVE SPACE TO ����t���O
010830     END-EVALUATE.
010840*================================================================*
010850 �����Z�b�g SECTION.
010860*
010870     EVALUATE  ��Q�|��
010880*�k�C��
010890      WHEN  01
010900         MOVE NC"�k�C��" TO �s���{����
010910*�X
010920      WHEN  02
010930         MOVE NC"�X��" TO �s���{����
010940*���
010950      WHEN  03
010960         MOVE NC"��茧" TO �s���{����
010970*�{��
010980      WHEN  04
010990         MOVE NC"�{�錧" TO �s���{����
011000*�H�c
011010      WHEN  05
011020         MOVE NC"�H�c��" TO �s���{����
011030*�R�`
011040      WHEN  06
011050         MOVE NC"�R�`��" TO �s���{����
011060*����
011070      WHEN  07
011080         MOVE NC"������" TO �s���{����
011090*���
011100      WHEN  08
011110         MOVE NC"��錧" TO �s���{����
011120*�Ȗ�
011130      WHEN  09
011140         MOVE NC"�Ȗ،�" TO �s���{����
011150*�Q�n
011160      WHEN  10
011170         MOVE NC"�Q�n��" TO �s���{����
011180*���
011190      WHEN  11
011200         MOVE NC"��ʌ�" TO �s���{����
011210*��t
011220      WHEN  12
011230         MOVE NC"��t��" TO �s���{����
011240*����
011250      WHEN  13
011260         MOVE NC"�����s" TO �s���{����
011270*�_�ސ�
011280      WHEN  14
011290         MOVE NC"�_�ސ쌧" TO �s���{����
011300*�V��
011310      WHEN  15
011320         MOVE NC"�V����" TO �s���{����
011330*�x�R
011340      WHEN  16
011350         MOVE NC"�x�R��" TO �s���{����
011360*�ΐ�
011370      WHEN  17
011380         MOVE NC"�ΐ쌧" TO �s���{����
011390*����
011400      WHEN  18
011410         MOVE NC"���䌧" TO �s���{����
011420*�R��
011430      WHEN  19
011440         MOVE NC"�R����" TO �s���{����
011450*����
011460      WHEN  20
011470         MOVE NC"���쌧" TO �s���{����
011480*��
011490      WHEN  21
011500         MOVE NC"�򕌌�" TO �s���{����
011510*�É�
011520      WHEN  22
011530         MOVE NC"�É���" TO �s���{����
011540*���m
011550      WHEN  23
011560         MOVE NC"���m��" TO �s���{����
011570*�O�d
011580      WHEN  24
011590         MOVE NC"�O�d��" TO �s���{����
011600*����
011610      WHEN  25
011620         MOVE NC"���ꌧ" TO �s���{����
011630*���s
011640      WHEN  26
011650         MOVE NC"���s�{" TO �s���{����
011660*���
011670      WHEN  27
011680         MOVE NC"���{" TO �s���{����
011690*����
011700      WHEN  28
011710         MOVE NC"���Ɍ�" TO �s���{����
011720*�ޗ�
011730      WHEN  29
011740         MOVE NC"�ޗǌ�" TO �s���{����
011750*�a�̎R
011760      WHEN  30
011770         MOVE NC"�a�̎R��" TO �s���{����
011780*����
011790      WHEN  31
011800         MOVE NC"���挧" TO �s���{����
011810*����
011820      WHEN  32
011830         MOVE NC"������" TO �s���{����
011840*���R
011850      WHEN  33
011860         MOVE NC"���R��" TO �s���{����
011870*�L��
011880      WHEN  34
011890         MOVE NC"�L����" TO �s���{����
011900*�R��
011910      WHEN  35
011920         MOVE NC"�R����" TO �s���{����
011930*����
011940      WHEN  36
011950         MOVE NC"������" TO �s���{����
011960*����
011970      WHEN  37
011980         MOVE NC"���쌧" TO �s���{����
011990*���Q
012000      WHEN  38
012010         MOVE NC"���Q��" TO �s���{����
012020*���m
012030      WHEN  39
012040         MOVE NC"���m��" TO �s���{����
012050*����
012060      WHEN  40
012070         MOVE NC"������" TO �s���{����
012080*����
012090      WHEN  41
012100         MOVE NC"���ꌧ" TO �s���{����
012110*����
012120      WHEN  42
012130         MOVE NC"���茧" TO �s���{����
012140*�F�{
012150      WHEN  43
012160         MOVE NC"�F�{��" TO �s���{����
012170*�啪
012180      WHEN  44
012190         MOVE NC"�啪��" TO �s���{����
012200*�{��
012210      WHEN  45
012220         MOVE NC"�{�茧" TO �s���{����
012230*������
012240      WHEN  46
012250         MOVE NC"��������" TO �s���{����
012260*����
012270      WHEN  47
012280         MOVE NC"���ꌧ" TO �s���{����
012290*���̑�
012300      WHEN  OTHER
012310         CONTINUE
012320     END-EVALUATE.
012330*================================================================*
012340 �ی���ʖ��Z�b�g SECTION.
012350*
012360**///  ��������̕ύX�͂���SECTION��   ////**
012370*
012380*************************************************************************
012390*  �������:�ی�����(�ی���ʃR�[�h)                                    *
012400*                                                                       *
012410*    10:�Е�(2)                                                         *
012420*    11:�D��(7)                                                         *
012430*    12:����(6)                                                         *
012440*    50:����(1)                                                         *
012450*    51:���ۑg��(1)                                                     *
012460*    52:�ސE(8)                                                         *
012470*    53:�V�l(5)                                                         *
012480*    60:�S�P�V�l(51)                                                    *
012490*    61:��q(52)                                                        *
012500*    62:���c��(55)                                                      *
012510*    63:��Q(53)                                                        *
012520*    64:�픚(54)                                                        *
012530*    65:���̑�(60)                                                      *
012540*    80:�g��(3)                                                         *
012550*    90:����(4)                                                         *
012560*    91:���q��(9)                                                       *
012570*                                                                       *
012580*   �J�ЁA�����ӁA����͂̂���                                          *
012590*                                                                       *
012600*                                                                       *
012610*************************************************************************
012620*
012630*
012640      EVALUATE  ��Q�|�������
012650* ����
012660      WHEN  050
012670          MOVE NC"�����ی�" TO �ی���ʖ�
012680* ���ۑg������
012690      WHEN  051
012700          MOVE NC"���ۑg��" TO �ی���ʖ�
012710* �Е�
012720      WHEN  010
012730*          MOVE NC"�Љ�ی�" TO �ی���ʖ�
012740          MOVE NC"����" TO �ی���ʖ�
012750* �D���͎Еۂŕ\��
012760*      WHEN  011
012770      WHEN  020
012780          MOVE NC"�Љ�ی�" TO �ی���ʖ�
012790* ���ق͎Еۂŕ\��
012800      WHEN  012
012810*          MOVE NC"�Љ�ی�" TO �ی���ʖ�
012820          MOVE NC"����" TO �ی���ʖ�
012830* �g��
012840      WHEN  080
012850          MOVE NC"���ۑg��" TO �ی���ʖ�
012860* ����
012870      WHEN  090
012880          MOVE NC"���ϑg��" TO �ی���ʖ�
012890* ���q���͋��ϑg���ŕ\��
012900      WHEN  091
012910          MOVE NC"���ϑg��" TO �ی���ʖ�
012920* �Q�V�V�l
012930      WHEN  053
012940          IF ��Q�|�ی��Ҕԍ�(1:2) = "39"
012950              MOVE NC"���"     TO �ی���ʖ�
012960          ELSE
012970              MOVE NC"�V���V�l" TO �ی���ʖ�
012980          END-IF
012990* �ސE����
013000      WHEN  052
013010          MOVE NC"�ސE����" TO �ی���ʖ�
013020* �S�P�V�l
013030      WHEN  060
013040          MOVE NC"����V�l" TO �ی���ʖ�
013050* ��q
013060      WHEN  061
013070          MOVE NC"��q�ƒ�" TO �ی���ʖ�
013080* ���c��
013090      WHEN  062
013100          MOVE NC"���c��" TO �ی���ʖ�
013110* ��Q
013120      WHEN  063
013130          MOVE NC"��Q��" TO �ی���ʖ�
013140* �픚
013150      WHEN  064
013160          MOVE NC"�픚��" TO �ی���ʖ�
013170* ���̑�
013180* �����̂W�W�P�R�̂݁u�q�v���󎚂��A����ȊO�͋󔒂Ƃ���
013190      WHEN  065
013200          EVALUATE ��Q�|�ی��Ҕԍ�(1:4)
013210          WHEN "8813"
013220              MOVE NC"�q" TO �ی���ʖ�
013230          WHEN OTHER
013240              MOVE SPACE TO �ی���ʖ�
013250          END-EVALUATE
013260      WHEN OTHER
013270          MOVE SPACE TO �ی���ʖ�
013280     END-EVALUATE.
013290*
013300*================================================================*
013310 ���������擾 SECTION.
013320*
013330     MOVE SPACE        TO �����於�̂v.
013340     MOVE SPACE        TO �x���������v.
013350     MOVE �ی���ʂv   TO �s�|������.
013360     MOVE �ی��Ҕԍ��v TO �s�|�s�����ԍ�.
013370     START �s�����}�X�^ KEY IS >= �s�|������ �s�|�s�����ԍ�
013380     END-START.
013390     IF ��ԃL�[ = "00"
013400         READ �s�����}�X�^ NEXT
013410         AT END
013420             MOVE SPACE              TO �����於�̂v
013430             MOVE SPACE              TO �x���������v
013440         NOT AT END
013450             IF (�ی���ʂv        = �s�|������       ) AND
013460                (�ی��Ҕԍ��v(1:4) = �s�|�s�����ԍ�(1:4))
013470                 MOVE �s�|�s�������� TO �����於�̂v
013480                 MOVE SPACE          TO �x���������v
013490             END-IF
013500         END-READ
013510     END-IF.
013520*================================================================*
013530******************************************************************
013540 END PROGRAM YJK437.
013550******************************************************************
