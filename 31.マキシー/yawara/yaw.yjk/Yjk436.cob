000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJK436.
000060 AUTHOR.                 ���c ���a
000070*
000080*----------------------------------------------------------------*
000090*  ���{�_�����ω�p �V���p�� �������y����z�_����޳��95��
000100*         MED = YJK436P
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
000390     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000400                             ORGANIZATION             IS  INDEXED
000410                             ACCESS MODE              IS  DYNAMIC
000420                             RECORD KEY               IS  �ہ|�ی����
000430                                                          �ہ|�ی��Ҕԍ�
000440                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000450                                                          �ہ|�ی��Җ���
000460                                                          �ہ|�ی��Ҕԍ�
000470                             FILE STATUS              IS  ��ԃL�[
000480                             LOCK        MODE         IS  AUTOMATIC.
000490     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000500                             ORGANIZATION             IS  INDEXED
000510                             ACCESS MODE              IS  DYNAMIC
000520                             RECORD KEY               IS  �s�|������
000530                                                          �s�|�s�����ԍ�
000540                             ALTERNATE RECORD KEY     IS  �s�|������
000550                                                          �s�|�s��������
000560                                                          �s�|�s�����ԍ�
000570                             FILE STATUS              IS  ��ԃL�[
000580                             LOCK        MODE         IS  AUTOMATIC.
000660     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000670                             ORGANIZATION             IS  INDEXED
000680                             ACCESS MODE              IS  DYNAMIC
000690                             RECORD KEY               IS �{��|�{�p���ԍ�
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
000924*
000925     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4312L.DAT"
000930                             ORGANIZATION             IS  INDEXED
000940                             ACCESS                   IS  DYNAMIC
000950                             RECORD      KEY          IS  ��Q�|�����a��N��
000960                                                          ��Q�|�ی����
000970                                                          ��Q�|�ی��Ҕԍ�
000980                             ALTERNATE RECORD KEY     IS  ��Q�|�����a��N��
000990                                                          ��Q�|��
001000                                                          ��Q�|�������
001010                                                          ��Q�|�ی����
001020                                                          ��Q�|�ی��Ҕԍ�
001030                             ALTERNATE RECORD KEY     IS  ��Q�|�����a��N��
001040                                                          ��Q�|�������
001050                                                          ��Q�|�ی����
001060                                                          ��Q�|�ی��Ҕԍ�
001070*                                                          ��Q�|��
001080*/���ʁA�Еۓ��ق����܂Ƃ߁A�D����ʂɏW�v/081021
001090                             ALTERNATE RECORD KEY     IS  ��Q�|�����a��N��
001100                                                          ��Q�|���Q
001110                                                          ��Q�|�������
001120                                                          ��Q�|�ی����
001130                                                          ��Q�|�ی��Ҕԍ�
001140                             FILE        STATUS       IS  ��ԃL�[
001150                             LOCK        MODE         IS  AUTOMATIC.
001160*
001170     SELECT  �ی��Ҋg���}�X�^ ASSIGN     TO        HOKENEXL
001180                             ORGANIZATION             IS  INDEXED
001190                             ACCESS MODE              IS  DYNAMIC
001200                             RECORD KEY               IS  �ۊg�|�ی����
001210                                                          �ۊg�|�ی��Ҕԍ�
001220                             FILE STATUS              IS  ��ԃL�[
001230                             LOCK        MODE         IS  AUTOMATIC.
001240     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF001
001250                             SYMBOLIC    DESTINATION  IS "PRT"
001260                             FORMAT                   IS  ��`�̖��o
001270                             GROUP                    IS  ���ڌQ���o
001280                             PROCESSING  MODE         IS  ������ʂo
001290                             UNIT        CONTROL      IS  �g������o
001300                             FILE        STATUS       IS  �ʒm���o.
001310******************************************************************
001320*                      DATA DIVISION                             *
001330******************************************************************
001340 DATA                    DIVISION.
001350 FILE                    SECTION.
001360*                           �m�q�k��  �P�Q�W�n
001370 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001380     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001390*                           �m�q�k��  �Q�T�U�n
001400 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001410     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001420     COPY SEIGYO01        OF  XFDLIB  JOINING   ���O�P   AS  PREFIX.
001430*                           �m�q�k��  �R�Q�O�n
001440 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001450     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001460*                           �m�q�k��  �Q�T�U�n
001470 FD  �s�����}�X�^        BLOCK   CONTAINS   1   RECORDS.
001480     COPY SITYOSN         OF  XFDLIB  JOINING   �s   AS  PREFIX.
001520*                           �m�q�k��  �P�Q�W�n
001530 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001540     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001550*                           �m�q�k��  �P�Q�W�n
001560 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001570     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
001580*                          �m�q�k��  �U�S�O�n
001590 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
001600     COPY KAIJOHO         OF  XFDLIB  JOINING   ���   AS  PREFIX.
001610*                           �m�q�k��  �W�O�O�n
001620 FD  �ی��Ҋg���}�X�^        BLOCK   CONTAINS   1   RECORDS.
001630     COPY HOKENEX        OF  XFDLIB  JOINING   �ۊg   AS  PREFIX.
001640*
001650*
001660* �{�p/����  (�����a��N���́A�{�p/���� ���p) 
001670*                           �m�q�k��  �P�Q�W�n
001680 FD  ��ƃt�@�C���Q RECORD  CONTAINS 128 CHARACTERS.
001690 01  ��Q�|���R�[�h.
001700     03  ��Q�|���R�[�h�L�[.
001710         05  ��Q�|�����a��N��.
001720             07  ��Q�|�����a��              PIC 9.
001730             07  ��Q�|�����N                PIC 9(2).
001740             07  ��Q�|������                PIC 9(2).
001750         05  ��Q�|�ی����                  PIC 9(2).
001760         05  ��Q�|�ی��Ҕԍ�                PIC X(10).
001770         05  ��Q�|��                        PIC X(2).
001780         05  ��Q�|�������.
001790             07  ��Q�|��������P            PIC 9(2).
001800             07  ��Q�|��������Q            PIC 9.
001810     03  ��Q�|���R�[�h�f�[�^.
001820         05  ��Q�|�ی��Ҕԍ��Q              PIC X(10).
001830         05  ��Q�|����                      PIC 9(4).
001840         05  ��Q�|��p�z                    PIC 9(9).
001850         05  ��Q�|���S�z                    PIC 9(9).
001860         05  ��Q�|�����z                    PIC 9(9).
001870         05  ��Q�|�{�l����                  PIC 9(3).
001880         05  ��Q�|�{�l��p�z                PIC 9(7).
001890         05  ��Q�|�{�l���S�z                PIC 9(7).
001900         05  ��Q�|�{�l�����z                PIC 9(7).
001910         05  ��Q�|�Ƒ�����                  PIC 9(3).
001920         05  ��Q�|�Ƒ���p�z                PIC 9(7).
001930         05  ��Q�|�Ƒ����S�z                PIC 9(7).
001940         05  ��Q�|�Ƒ������z                PIC 9(7).
001950         05  ��Q�|���Q                      PIC X(2).
001960         05  FILLER                          PIC X(15).
001970*         05  FILLER                          PIC X(17).
001980*
001990 FD  ����t�@�C��.
002000     COPY YJK436P        OF  XMDLIB.
002010*----------------------------------------------------------------*
002020******************************************************************
002030*                WORKING-STORAGE SECTION                         *
002040******************************************************************
002050 WORKING-STORAGE         SECTION.
002060 01 �L�[����                           PIC X     VALUE SPACE.
002070 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
002080 01 �I���t���O                         PIC X(3)  VALUE SPACE.
002090 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
002100 01 �����t���O                         PIC X(4)  VALUE SPACE.
002110 01 ��ƃt���O                         PIC X(3)  VALUE SPACE.
002120 01 ��ƈړ��L�[                       PIC X(4)  VALUE SPACE.
002130 01 �I���s�t���O                       PIC X(3)  VALUE SPACE.
002140 01 �t�@�C����                         PIC N(2)  VALUE SPACE.
002150 01 ���l�v                             PIC X(20) VALUE SPACE.
002160 01 �O�a��v                           PIC 9(1)  VALUE ZERO.
002170 01 �ی���ʂv                         PIC 9(2)  VALUE ZERO.
002180 01 �ی��Ҕԍ��v                       PIC X(10) VALUE SPACE.
002190 01 �ی��Ҕԍ��P�v                     PIC X(10) VALUE SPACE.
002200 01 �ی��Ҕԍ��Q�v                     PIC X(10) VALUE SPACE.
002210 01 ����t���O                         PIC X(3)  VALUE SPACE.
002211 01 �I�[�v���t���O                     PIC X(3)  VALUE SPACE.
002220*
002230 01 �s�J�E���^                         PIC 9(2)  VALUE ZERO.
002240 01 �ŃJ�E���^                         PIC 9(4)  VALUE ZERO.
002250 01 �ő�s��                           PIC 9(2)  VALUE ZERO.
002260 01 �w�b�_�s��                         PIC 9(2)  VALUE ZERO.
002270 01 �ړ��s���v                         PIC 9(2)  VALUE ZERO.
002280 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
002290 01 �ی����̂v                         PIC N(2) VALUE SPACE.
002300*
002310 01 �{�p�a��N���v.
002320     03 �{�p�a��v                     PIC 9(1)  VALUE ZERO.
002330     03 �{�p�N���v.
002340        05 �{�p�N�v                    PIC 9(2)  VALUE ZERO.
002350        05 �{�p���v                    PIC 9(2)  VALUE ZERO.
002360**
002370**************
002380* �{�p����� *
002390**************
002400 01 �{�p�����v.
002410    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
002420    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
002430    03 �_���t�ԍ��v                    PIC X(20)  VALUE SPACE.
002440    03 �{�p���Z���v.
002450       05 �{�p���Z���P�v               PIC X(40)  VALUE SPACE.
002460       05 �{�p���Z���Q�v               PIC X(40)  VALUE SPACE.
002470    03 �{�p���X�֔ԍ��v.
002480       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
002490       05 �{�p���X�֔ԍ���؂v         PIC X(1)   VALUE SPACE.
002500       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
002510    03 �{�p���d�b�ԍ��v                PIC X(15)  VALUE SPACE.
002520    03 �������v.
002530        05 ������s���v              PIC X(40)  VALUE SPACE.
002540        05 ������s�x�X���v          PIC X(40)  VALUE SPACE.
002550        05 �a����ʂv                  PIC 9(1)   VALUE ZERO.
002560        05 ��s�ԍ��v                  PIC X(4)   VALUE SPACE.
002570        05 �X�ԍ��v                    PIC X(3)   VALUE SPACE.
002580        05 �����ԍ��v                  PIC X(10)  VALUE SPACE.
002590        05 �������`�l�J�i�v            PIC X(40)  VALUE SPACE.
002600        05 �������`�l�v                PIC X(40)  VALUE SPACE.
002610*
002620 01 �A�Ԃv                             PIC 9(3)   VALUE ZERO.
002630 01 ��s���x�X���v                     PIC X(40)  VALUE SPACE.
002640 01 �a����ʃR�����g�v                 PIC N(2)   VALUE SPACE.
002650 01 �T���v                             PIC N(4)   VALUE SPACE.
002660**
002670* �Еۗp
002680 01 �ڔ���敪�v                       PIC 9     VALUE ZERO.
002690*
002700********************
002710* �ی��ҕʍ��v���z *
002720********************
002730 01 �ی��ҕʍ��v���z.
002740    03 ���z�v                          PIC N(2)  VALUE SPACE.
002750    03 �������b�Z�[�W�v                PIC N(15) VALUE SPACE.
002760    03 �~�v                            PIC N(1)  VALUE SPACE.
002770    03 �����v                          PIC 9(3)  VALUE ZERO.
002780    03 ��p�z�v                        PIC 9(8)  VALUE ZERO.
002790    03 �����z�v                        PIC 9(7)  VALUE ZERO.
002800    03 �����於�̂v                    PIC X(40) VALUE SPACE.
002810    03 �x���������v                    PIC X(40) VALUE SPACE.
002820    03 �����v                          PIC X(24) VALUE SPACE.
002830    03 �ی��҈����v.
002840       05 �ی��҈����P�v               PIC X(40) VALUE SPACE.
002850       05 �ی��҈����Q�v               PIC X(40) VALUE SPACE.
002860**
002870 01 ��ʏ��S�R�O�v.
002880    03 �����N���v.
002890       05 �����a��v                   PIC 9     VALUE ZERO.
002900       05 �����N�v                     PIC 9(2)  VALUE ZERO.
002910       05 �������v                     PIC 9(2)  VALUE ZERO.
002920    03 ��o�N�����v.
002930       05 ��o�a��v                   PIC 9     VALUE ZERO.
002940       05 ��o�N�v                     PIC 9(2)  VALUE ZERO.
002950       05 ��o���v                     PIC 9(2)  VALUE ZERO.
002960       05 ��o���v                     PIC 9(2)  VALUE ZERO.
002970    03 �����ނv                      PIC 9     VALUE ZERO.
002980***
002990* �G���[���b�Z�[�W�p
003000 01 �G���[���b�Z�[�W�v.
003010    03 �G���[�ی���ʂv                PIC X(2) VALUE SPACE.
003020    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
003030    03 �G���[�ی��Ҕԍ��v              PIC X(10) VALUE SPACE.
003040    03 FILLER                          PIC X(7) VALUE SPACE.
003050*
003060***
003070* ����������p�����^�p
003080*  �U�E�V���������i����敪 0:��� 1:������Ȃ��A�U���� 0:���� 1:�� 9:������Ȃ��j
003090*  ���Зp�������l1:������Ȃ�
003100***
003110 01 �������֘A�v.
003120        07 ���ۂU������敪�v          PIC 9 VALUE ZERO.
003130        07 ���ۂV������敪�v          PIC 9 VALUE ZERO.
003140        07 ���ۓ��Ј���敪�v          PIC 9 VALUE 1.
003150        07 ���ېU����敪�v            PIC 9 VALUE ZERO.
003160        07 �ЕۂU������敪�v          PIC 9 VALUE ZERO.
003170        07 �ЕۂV������敪�v          PIC 9 VALUE ZERO.
003180        07 �Еۓ��Ј���敪�v          PIC 9 VALUE 1.
003190        07 �ЕېU����敪�v            PIC 9 VALUE ZERO.
003200        07 �g���U������敪�v          PIC 9 VALUE ZERO.
003210        07 �g���V������敪�v          PIC 9 VALUE ZERO.
003220        07 �g�����Ј���敪�v          PIC 9 VALUE 1.
003230        07 �g���U����敪�v            PIC 9 VALUE ZERO.
003240        07 ���ςU������敪�v          PIC 9 VALUE ZERO.
003250        07 ���ςV������敪�v          PIC 9 VALUE ZERO.
003260        07 ���ϓ��Ј���敪�v          PIC 9 VALUE 1.
003270        07 ���ϐU����敪�v            PIC 9 VALUE ZERO.
003280        07 �V�l�U������敪�v          PIC 9 VALUE ZERO.
003290        07 �V�l�V������敪�v          PIC 9 VALUE ZERO.
003300        07 �V�l���Ј���敪�v          PIC 9 VALUE 1.
003310        07 �V�l�U����敪�v            PIC 9 VALUE ZERO.
003320        07 �����U������敪�v          PIC 9 VALUE 1.
003330        07 �����V������敪�v          PIC 9 VALUE ZERO.
003340        07 �������Ј���敪�v          PIC 9 VALUE 1.
003350        07 �����U����敪�v            PIC 9 VALUE ZERO.
003360*
003370***********************************************************************
003380 01 �������.
003390     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
003400     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
003410     03 ������ʂo                     PIC X(2) VALUE SPACE.
003420     03 �g������o.
003430         05 �[������o.
003440             07 �ړ������o             PIC X(1).
003450             07 �ړ��s���o             PIC 9(3).
003460         05 �ڍא���o                 PIC X(2).
003470     03 �ʒm���o                     PIC X(2) VALUE SPACE.
003480     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
003490*
003500 01 �v�Z�@����N�v                     PIC 9(2).
003510* ���t�v�n�q�j
003520 01 �a��I���N�v                       PIC 9(4).
003530 01 �v�Z�@����.
003540    03 �v�Z�@����N                    PIC 9(4).
003550    03 �v�Z�@�����                  PIC 9(4).
003560 01 �v�Z�@����q REDEFINES �v�Z�@����.
003570    03 �v�Z�@���I                      PIC 9(2).
003580    03 �v�Z�@���t                      PIC 9(6).
003590    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
003600       05 �v�Z�@�N��                   PIC 9(4).
003610       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
003620         07 �v�Z�@�N                   PIC 9(2).
003630         07 �v�Z�@��                   PIC 9(2).
003640       05 �v�Z�@��                     PIC 9(2).
003650*
003660******************************************************************
003670*                          �A������                              *
003680******************************************************************
003690*
003700********************
003710* ���b�Z�[�W�\���L�[ *
003720********************
003730 01 �A���|�L�[ IS EXTERNAL.
003740    03  �A���|���b�Z�[�W               PIC N(20).
003750*
003760 01 �A���R�|�L�[ IS EXTERNAL.
003770    03  �A���R�|���b�Z�[�W             PIC N(20).
003780    03  �A���R�|���b�Z�[�W�P           PIC X(20).
003790*
003990****************
003991* ��ʓ��͏�� *
003992****************
003993**
003994 01 �A���|��ʏ��x�i�j�S�R�O   IS EXTERNAL.
003995    03 �A���|�����N��.
003996       05 �A���|�����a��               PIC 9.
003997       05 �A���|�����N                 PIC 9(2).
003998       05 �A���|������                 PIC 9(2).
003999    03 �A���|��o�N����.
004000       05 �A���|��o�a��               PIC 9.
004001       05 �A���|��o�N                 PIC 9(2).
004002       05 �A���|��o��                 PIC 9(2).
004003       05 �A���|��o��                 PIC 9(2).
004004    03 �A���|���Z�v�g���              PIC X(4).
004005    03 �A���|�ی����                  PIC 9(2).
004006    03 �A���|������                  PIC 9.
004007    03 �A���|�{�l�Ƒ�                  PIC 9.
004008    03 �A���|�p�����                  PIC 9.
004009    03 �A���|�������O                  PIC 9.
004010    03 �A���|���i�h�r                  PIC X(2).
004011    03 �A���|���ǂi�h�r                PIC X(2).
004012*
004013*
004015 01 �A���|��ʏ��x�i�j�S�R�O�ǉ�   IS EXTERNAL.
004016    03 �A���|�ꊇ�敪    PIC 9.
004020    03 �A���|�v���r���[�敪            PIC 9.
004021*
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
004120     PERFORM �v�����^�t�@�C���쐬.
004121     PERFORM ������.
004130     PERFORM ������擾�Q.
004140************
004150*           *
004160* �又��     *
004170*           *
004180************
004190     PERFORM �������.
004200************
004210*           *
004220* �I������   *
004230*           *
004240************
004250     PERFORM �I������.
004260     EXIT PROGRAM.
004270*
004280*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
004290*================================================================*
004291 �v�����^�t�@�C���쐬 SECTION.
004292*================================================================*
004293*   / ������ /
004294     MOVE SPACE TO �g�A�o�q�s�e�|�쐬�f�[�^.
004295     INITIALIZE �g�A�o�q�s�e�|�쐬�f�[�^.
004296*
004297*
004298*--���� �ύX�ӏ� ����--------------------------------------*
004299*   �g�p����v�����^�t�@�C�����Z�b�g
004300     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
004301*
004302*   �g�p���钠�[�v���O�������Z�b�g
004303     MOVE "YJK436"              TO �g�A�o�q�s�e�|���[�v���O������.
004304*
004305*--����-----------------------------------------------------*
004306*
004307*   / �v���r���[�敪�Z�b�g /
004308     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
004309*
004310     CALL   "CRTPRTF".
004311     CANCEL "CRTPRTF".
004312*
004313*================================================================*
004314 ������ SECTION.
004315*
004320     PERFORM �t�@�C���I�[�v��.
004330*    /* ���ݓ��t�擾 */
004340     ACCEPT �v�Z�@���t FROM DATE.
004350*    /* 1980�`2079�N�̊ԂŐݒ� */
004360     IF �v�Z�@�N > 80
004370         MOVE 19 TO �v�Z�@���I
004380     ELSE
004390         MOVE 20 TO �v�Z�@���I
004400     END-IF.
004410*
004420     PERFORM �A�����ڑޔ�.
004430     PERFORM �{�p�����擾.
004440     PERFORM �J�����g�����擾.
004450     PERFORM �a��I���N�擾.
004460***     COMPUTE �v�Z�@����N�v = �v�Z�@����N - 1988.
004470*================================================================*
004480 �J�����g�����擾 SECTION.
004490*
004500     MOVE ZEROS TO ���|����敪.
004510     READ ������}�X�^
004520     NOT INVALID KEY
004530         MOVE ���|�J�����g���� TO �J�����g�����v
004540     END-READ.
004550*
004560*================================================================*
004570 �a��I���N�擾 SECTION.
004580*
004590*     DISPLAY NC"�J�����g�����v"  �J�����g�����v UPON MSGBOX.
004600     MOVE �J�����g�����v TO ���|�����敪.
004610     READ �����}�X�^
004620     INVALID KEY
004630         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
004640         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004650                                                  UPON CONS
004660         ACCEPT  �L�[���� FROM CONS
004670         PERFORM �I������
004680         EXIT PROGRAM
004690     NOT INVALID KEY
004700         COMPUTE �O�a��v = �J�����g�����v - 1
004710         MOVE �O�a��v TO ���|�����敪
004720         READ �����}�X�^
004730         INVALID KEY
004740             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
004750             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004760                                                      UPON CONS
004770             ACCEPT  �L�[���� FROM CONS
004780             PERFORM �I������
004790             EXIT PROGRAM
004800         NOT INVALID KEY
004810             MOVE ���|�I������N TO �a��I���N�v
004820         END-READ
004830     END-READ.
004840*
004850*================================================================*
004860 �A�����ڑޔ� SECTION.
004870*
004880     MOVE �A���|�����a��  TO �����a��v.
004890     MOVE �A���|�����N    TO �����N�v.
004900     MOVE �A���|������    TO �������v.
004910     MOVE �A���|��o�a��  TO ��o�a��v.
004920     MOVE �A���|��o�N    TO ��o�N�v.
004930     MOVE �A���|��o��    TO ��o���v.
004940     MOVE �A���|��o��    TO ��o���v.
004950     MOVE �A���|������  TO �����ނv.
004960*
004970*================================================================*
004980 �{�p�����擾 SECTION.
004990*
005000     MOVE ZERO  TO �{��|�{�p���ԍ�.
005010     READ �{�p�����}�X�^
005020     INVALID KEY
005030         CONTINUE
005040     NOT INVALID KEY
005050*
005060         MOVE �{��|�X�֔ԍ��P       TO �{�p���X�֔ԍ��P�v
005070         MOVE "-"                    TO �{�p���X�֔ԍ���؂v
005080         MOVE �{��|�X�֔ԍ��Q       TO �{�p���X�֔ԍ��Q�v
005090         MOVE �{��|��\�Җ�         TO ��\�Җ��v
005100         MOVE �{��|�ڍ��@��         TO �ڍ��@���v
005110         STRING �{��|�Z���P  DELIMITED BY SPACE
005120                �{��|�Z���Q  DELIMITED BY SPACE
005130           INTO �{�p���Z���v
005140         END-STRING
005150         MOVE �{��|�d�b�ԍ�         TO �{�p���d�b�ԍ��v
005160         MOVE �{��|�V�_���t�ԍ�     TO �_���t�ԍ��v
005170*
005180         MOVE �{��|������s��     TO ������s���v
005190         MOVE �{��|������s�x�X�� TO ������s�x�X���v
005200         MOVE �{��|�a�����         TO �a����ʂv
005210         MOVE �{��|��s�ԍ�         TO ��s�ԍ��v
005220         MOVE �{��|�X�ԍ�           TO �X�ԍ��v
005230         MOVE �{��|�����ԍ�         TO �����ԍ��v
005240         MOVE �{��|�������`�l�J�i   TO �������`�l�J�i�v
005250         MOVE �{��|�������`�l       TO �������`�l�v
005260         STRING ������s���v     DELIMITED BY SPACE
005270                " "                DELIMITED BY SIZE
005280                ������s�x�X���v DELIMITED BY SPACE
005290                INTO ��s���x�X���v
005300         END-STRING
005310         EVALUATE �a����ʂv
005320         WHEN 1
005330             MOVE NC"����" TO �a����ʃR�����g�v
005340         WHEN 2
005350             MOVE NC"����" TO �a����ʃR�����g�v
005360         WHEN OTHER
005370             MOVE SPACE    TO �a����ʃR�����g�v
005380         END-EVALUATE
005390*
005400     END-READ.
005410*================================================================*
005420 �t�@�C���I�[�v�� SECTION.
005430*
005440     OPEN INPUT   �����}�X�^
005450         MOVE NC"����" TO �t�@�C����.
005460         PERFORM �I�[�v���`�F�b�N.
005470     OPEN INPUT  ������}�X�^.
005480         MOVE NC"������" TO �t�@�C����.
005490         PERFORM �I�[�v���`�F�b�N.
005500     OPEN INPUT   �ی��҃}�X�^
005510         MOVE NC"�ی�" TO �t�@�C����.
005520         PERFORM �I�[�v���`�F�b�N.
005530     OPEN INPUT   �s�����}�X�^
005540         MOVE NC"�s��" TO �t�@�C����.
005550         PERFORM �I�[�v���`�F�b�N.
005590     OPEN INPUT   �{�p�����}�X�^
005600         MOVE NC"�{��" TO �t�@�C����.
005610         PERFORM �I�[�v���`�F�b�N.
005620     OPEN INPUT   ������}�X�^
005630         MOVE NC"����" TO �t�@�C����.
005640         PERFORM �I�[�v���`�F�b�N.
005650     OPEN INPUT   ����}�X�^
005660         MOVE NC"����" TO �t�@�C����.
005670         PERFORM �I�[�v���`�F�b�N.
005680     OPEN INPUT �ی��Ҋg���}�X�^.
005690         MOVE NC"�ۊg" TO �t�@�C����.
005700         PERFORM �I�[�v���`�F�b�N.
005730*================================================================*
005740 �I�[�v���`�F�b�N SECTION.
005750*
005760     IF ��ԃL�[  NOT =  "00"
005770         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
005780         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
005790         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005800                                                 UPON CONS
005810         ACCEPT  �L�[���� FROM CONS
005820         PERFORM �t�@�C����
005830         EXIT PROGRAM.
005840*================================================================*
005850 �t�@�C���� SECTION.
005860*
005910     IF ( �I�[�v���t���O = "YES" )
005911         CLOSE ����t�@�C��
005912     END-IF.
005913     CLOSE �����}�X�^     ������}�X�^   �ی��҃}�X�^
005914           �s�����}�X�^   �{�p�����}�X�^ ������}�X�^  
005915           �ی��Ҋg���}�X�^ ����}�X�^.
005916*================================================================*
005920 �I������ SECTION.
005930*
005940     PERFORM �t�@�C����.
005950*================================================================*
005960 �G���[�\�� SECTION.
005970*
005980     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
005990     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
006000     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
006010     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
006020     ACCEPT  �L�[���� FROM CONS.
006030     PERFORM �t�@�C����.
006040     EXIT PROGRAM.
006050*================================================================*
006060 ������� SECTION.
006070*
006080     OPEN INPUT  ��ƃt�@�C���Q.
006090         MOVE NC"��Q" TO �t�@�C����.
006100         PERFORM �I�[�v���`�F�b�N.
006110* / ���L�[(�������+�ی����+�ی��Ҕԍ�)�œǂݍ���./
006120     MOVE ZERO      TO  ��Q�|�����a��.
006130     MOVE ZERO      TO  ��Q�|�����N.
006140     MOVE ZERO      TO  ��Q�|������.
006150*     MOVE LOW-VALUE TO  ��Q�|��.
006160     MOVE ZERO      TO  ��Q�|�������.
006170     MOVE ZERO      TO  ��Q�|�ی����.
006180     MOVE LOW-VALUE TO  ��Q�|�ی��Ҕԍ�.
006190     START ��ƃt�@�C���Q   KEY IS >=  ��Q�|�����a��N��
006200                                       ��Q�|�������
006210                                       ��Q�|�ی����
006220                                       ��Q�|�ی��Ҕԍ�
006230*                                       ��Q�|��
006240     END-START.
006250     IF ��ԃL�[ = "00"
006260         MOVE SPACE TO �I���t���O
006270         PERFORM ��ƃt�@�C���Q�Ǎ�
006280         IF  �I���t���O = "YES"
006290             MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
006300             CALL   "MSG001"
006310             CANCEL "MSG001"
006320             PERFORM �t�@�C����
006330             MOVE 99 TO PROGRAM-STATUS
006340             EXIT PROGRAM
006350         END-IF
006360*
006370         PERFORM UNTIL  �I���t���O = "YES"
006380                PERFORM ����Ώۃ`�F�b�N
006390                IF ����t���O = "YES"
006400                   MOVE SPACE TO YJK436P
006410****                INITIALIZE YJK436P
006420                   PERFORM �w�b�_�Z�b�g
006430                   PERFORM ���׃Z�b�g
006440                   PERFORM �󎚏���
006450                   PERFORM ���ŏ���
006460                END-IF
006470                PERFORM ��ƃt�@�C���Q�Ǎ�
006480         END-PERFORM
006490*
006500     ELSE
006510         MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
006520         CALL   "MSG001"
006530         CANCEL "MSG001"
006540         PERFORM �t�@�C����
006550         MOVE 99 TO PROGRAM-STATUS
006560         EXIT PROGRAM
006570     END-IF.
006580*
006590     CLOSE ��ƃt�@�C���Q.
006600*
006610*================================================================*
006620 ��ƃt�@�C���Q�Ǎ� SECTION.
006630*
006640     READ ��ƃt�@�C���Q NEXT
006650     AT END
006660         MOVE "YES" TO �I���t���O
006670     END-READ.
006680*================================================================*
006690 �󎚏���  SECTION.
006700*
006701     IF ( �I�[�v���t���O NOT = "YES" )
006702        MOVE "YES" TO �I�[�v���t���O
006703        OPEN I-O  ����t�@�C��
006704        PERFORM �G���[�����o
006705     END-IF.
006706*
006710     MOVE "YJK436P" TO  ��`�̖��o.
006720     MOVE SPACE     TO  ������ʂo.
006730     MOVE "SCREEN"  TO  ���ڌQ���o.
006740     WRITE YJK436P.
006750     PERFORM �G���[�����o.
006760*================================================================*
006770 ���ŏ���  SECTION.
006780*
006790     MOVE "YJK436P" TO  ��`�̖��o.
006800     MOVE "CT"      TO  ������ʂo.
006810     MOVE "PAGE"    TO  �g������o.
006820     MOVE SPACE     TO  ���ڌQ���o.
006830     WRITE YJK436P.
006840     PERFORM �G���[�����o.
006850     MOVE SPACE     TO  �g������o.
006860*
006870     CLOSE  ����t�@�C��.
           MOVE SPACE  TO �I�[�v���t���O.
006880*     OPEN I-O   ����t�@�C��.
006890*     PERFORM �G���[�����o.
006900*
006910*================================================================*
006920 �w�b�_�Z�b�g SECTION.
006930*
006940* �����̘a����擾
006950     MOVE �����a��v         TO ���|�����敪.
006960     READ �����}�X�^
006970     INVALID KEY
006980         MOVE SPACE          TO �����a���
006990     NOT INVALID KEY
007000         MOVE ���|��������   TO �����a���
007010     END-READ.
007020*
007030     MOVE �����N�v           TO �����N.
007040     MOVE �������v           TO ������.
007050     MOVE ��\�Җ��v         TO ��\�Җ�.
007060     MOVE �ڍ��@���v         TO �ڍ��@��.
007070     MOVE �_���t�ԍ��v       TO �_���t�ԍ�.
007080*
007090* / �ی���/
007100     MOVE ��Q�|�ی����     TO �ی���ʂv.
007110     IF (��Q�|�ی���� = 05) AND (��Q�|�ی��Ҕԍ�(1:2) = "39")
007120         MOVE ��Q�|�ی��Ҕԍ�(1:4) TO �ی��Ҕԍ�
007130     ELSE
007140         MOVE ��Q�|�ی��Ҕԍ�      TO �ی��Ҕԍ�
007150     END-IF.
007160     MOVE ��Q�|�ی��Ҕԍ��Q TO �ی��Ҕԍ��v.
007170     PERFORM �����Z�b�g.
007180     PERFORM �ی���ʖ��Z�b�g.
007190     EVALUATE �ی���ʂv
007200     WHEN 1 THRU 4
007210     WHEN 6 THRU 9
007220*     WHEN 70 
007230*     WHEN 80
007240         PERFORM �ی��ҏ��擾
007250     WHEN 5
007260     WHEN 50 THRU 60
007270         PERFORM �s�������擾
007280     END-EVALUATE.
007290     PERFORM ���������擾.
007300*
007310     MOVE �ی��҈����v  TO �ی��Җ���.
007320
007330*
007340*================================================================*
007350 ���׃Z�b�g SECTION.
007360*
007370     IF ��Q�|�{�l���� NOT = ZERO
007380        MOVE ��Q�|�{�l����     TO �{�l����
007390        MOVE ��Q�|�{�l��p�z   TO �{�l��p�z
007400        MOVE ��Q�|�{�l���S�z   TO �{�l���S�z
007410        MOVE ��Q�|�{�l�����z   TO �{�l�����z
007420     END-IF.
007430     IF ��Q�|�Ƒ����� NOT = ZERO
007440        MOVE ��Q�|�Ƒ�����     TO �Ƒ�����
007450        MOVE ��Q�|�Ƒ���p�z   TO �Ƒ���p�z
007460        MOVE ��Q�|�Ƒ����S�z   TO �Ƒ����S�z
007470        MOVE ��Q�|�Ƒ������z   TO �Ƒ������z
007480     END-IF.
007490*
007500*================================================================*
007510 �G���[�����o SECTION.
007520*
007530     IF �ʒm���o NOT = "00"
007540         DISPLAY NC"���[�G���["              UPON CONS
007550         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
007560         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
007570         DISPLAY NC"�g������o�F" �g������o UPON CONS
007580         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS
007590         ACCEPT  �L�[���� FROM CONS
007600         PERFORM �t�@�C����
007610         EXIT PROGRAM
007620     END-IF.
007630*================================================================*
007640 �ی��ҏ��擾 SECTION.
007650*
007660     MOVE  SPACE         TO �����於�̂v.
007670     MOVE  SPACE         TO �x���������v.
007680     MOVE  ZERO          TO �ڔ���敪�v.
007690*
007700     MOVE �ی���ʂv     TO �ہ|�ی����.
007710     MOVE �ی��Ҕԍ��v   TO �ہ|�ی��Ҕԍ�.
007720     READ �ی��҃}�X�^
007730     INVALID KEY
007740         MOVE SPACE      TO �����於�̂v
007750         MOVE SPACE      TO �x���������v
007760     NOT INVALID KEY
007770         IF �ہ|��������敪 = 1
007780             MOVE �ہ|�ی����   TO ����|�ی����
007790             MOVE �ہ|�ی��Ҕԍ� TO ����|�ی��Ҕԍ�
007800             READ ������}�X�^
007810             INVALID KEY
007820                 MOVE SPACE             TO �����於�̂v
007830                 MOVE SPACE             TO �x���������v
007840             NOT INVALID KEY
007850                 MOVE ����|�ی��Җ���  TO �����於�̂v
007860                 MOVE ����|�x��������  TO �x���������v
007870             END-READ
007880         ELSE
007890             MOVE �ہ|�ی��Җ���        TO �����於�̂v
007900             MOVE �ہ|�x��������        TO �x���������v
007910             MOVE �ہ|�ڔ���敪        TO �ڔ���敪�v
007920         END-IF
007930     END-READ.
007940*================================================================*
007950 �s�������擾 SECTION.
007960*
007970     MOVE  SPACE         TO �����於�̂v.
007980     MOVE  SPACE         TO �x���������v.
007990*
008000     MOVE �ی���ʂv               TO �s�|������.
008010     MOVE �ی��Ҕԍ��v             TO �s�|�s�����ԍ�.
008020     READ �s�����}�X�^
008030     INVALID KEY
008040         MOVE SPACE                TO �����於�̂v
008050         MOVE SPACE                TO �x���������v
008060     NOT INVALID KEY
008070         IF �s�|������敪 = 1
008080             MOVE �ی���ʂv       TO ����|�ی����
008090             MOVE �ی��Ҕԍ��v     TO ����|�ی��Ҕԍ�
008100             READ ������}�X�^
008110             INVALID KEY
008120                 MOVE SPACE        TO �����於�̂v
008130                 MOVE SPACE        TO �x���������v
008140             NOT INVALID KEY
008150                 MOVE ����|�ی��Җ���   TO �����於�̂v
008160                 MOVE ����|�x��������   TO �x���������v
008170             END-READ
008180          ELSE
008190             MOVE �s�|�s��������   TO �����於�̂v
008200             MOVE �s�|�x��������   TO �x���������v
008210          END-IF
008220      END-READ.
008230*/���������\�ԍ��ł܂Ƃ߂�/080516
008240      IF ��Q�|�ی��Ҕԍ�(1:2) = "39"
008250          MOVE SPACE TO �x���������v
008260      END-IF.
008270*================================================================*
008280 ���������擾 SECTION.
008290*
008300     MOVE SPACE TO �ی��҈����v.
008310     IF �����於�̂v NOT = SPACE
008320         EVALUATE �ی���ʂv
008330         WHEN 2
008340             IF �ڔ���敪�v = 1
008350                MOVE SPACE            TO �����v
008360             ELSE
008370                MOVE "�Љ�ی�������" TO �����v
008380             END-IF
008390         WHEN 6
008400             IF �ڔ���敪�v = 1
008410                MOVE "�i���فj"               TO �����v
008420             ELSE
008430                MOVE "�Љ�ی��������i���فj" TO �����v
008440             END-IF
008450         WHEN 7
008460             MOVE "�i�D���j"       TO �����v
008470         WHEN 3
008480             MOVE "���N�ی��g��"   TO �����v
008490         WHEN 4
008500             MOVE "���ϑg��"       TO �����v
008510         WHEN 8
008520             MOVE "�i�ސE�j"       TO �����v
008530         WHEN OTHER
008540             MOVE SPACE            TO �����v
008550         END-EVALUATE
008560*
008570         IF �x���������v = SPACE
008580             STRING  �����於�̂v  DELIMITED BY SPACE
008590                     �����v        DELIMITED BY SPACE
008600                     "  �a"        DELIMITED BY SIZE
008610                    INTO �ی��҈����v
008620             END-STRING
008630         ELSE
008640             STRING  �����於�̂v  DELIMITED BY SPACE
008650                     �����v        DELIMITED BY SPACE
008660                     " "           DELIMITED BY SIZE
008670                     �x���������v  DELIMITED BY SPACE
008680                     "  �a"        DELIMITED BY SIZE
008690                    INTO �ی��҈����v
008700             END-STRING
008710         END-IF
008720     END-IF.
008730*
008740*================================================================*
008750 ������擾�Q SECTION.
008760* �����l�ݒ�
008770       MOVE ZERO  TO ���ۂV������敪�v. 
008780       MOVE ZERO  TO �ЕۂV������敪�v. 
008790       MOVE ZERO  TO �g���V������敪�v. 
008800       MOVE ZERO  TO ���ςV������敪�v. 
008810       MOVE ZERO  TO �V�l�V������敪�v. 
008820       MOVE ZERO  TO �����V������敪�v. 
008830*
008840*================================================================*
008850 ����Ώۃ`�F�b�N  SECTION.
008860*
008870*  ����敪�ɂ��U�蕪�� �� �U�E�V���������i����敪 0:��� 1:������Ȃ��j
008880* �i�ꊇ����̂݁j
008890* 7���p���ł́A�ЕہA���ہA�ސE�A�V�l�A�����A�g���A���ς̈�����s��
008900*
008910     MOVE SPACE TO ����t���O.
008920*
008930     IF �A���|�ꊇ�敪 NOT = 1
008940        MOVE "YES" TO ����t���O
008950     ELSE
008960        EVALUATE ��Q�|�ی����
008970        WHEN 01
008980        WHEN 08
008990           IF ���ۂV������敪�v NOT = 1
009000              MOVE "YES" TO ����t���O
009010           END-IF
009020        WHEN 02
009030        WHEN 06
009040        WHEN 07
009050           IF �ЕۂV������敪�v NOT = 1
009060              MOVE "YES" TO ����t���O
009070           END-IF
009080        WHEN 03
009090           IF �g���V������敪�v NOT = 1
009100              MOVE "YES" TO ����t���O
009110           END-IF
009120        WHEN 04
009130        WHEN 09
009140           IF ���ςV������敪�v NOT = 1
009150              MOVE "YES" TO ����t���O
009160           END-IF
009170        WHEN 05
009180           IF �V�l�V������敪�v NOT = 1
009190              MOVE "YES" TO ����t���O
009200           END-IF
009210* �����ی�͏���
009220        WHEN 51 THRU 60
009230           IF �����V������敪�v NOT = 1
009240              MOVE "YES" TO ����t���O
009250           END-IF
009260        WHEN OTHER
009270           MOVE "YES" TO ����t���O
009280        END-EVALUATE
009290     END-IF.
009300*
009310*/�����s�̌㍂�͈�����Ȃ�(�ʗp��)/080516
009320     EVALUATE TRUE
009330     WHEN (��Q�|�ی���� = 05) AND (��Q�|�ی��Ҕԍ�(3:2) = "13")
009340         MOVE SPACE TO ����t���O
009350     END-EVALUATE.
009360*================================================================*
009370 �����Z�b�g SECTION.
009380*
009390     EVALUATE  ��Q�|��
009400*�k�C��
009410      WHEN  01
009420         MOVE NC"�k�C��" TO �s���{����
009430*�X
009440      WHEN  02
009450         MOVE NC"�X��" TO �s���{����
009460*���
009470      WHEN  03
009480         MOVE NC"��茧" TO �s���{����
009490*�{��
009500      WHEN  04
009510         MOVE NC"�{�錧" TO �s���{����
009520*�H�c
009530      WHEN  05
009540         MOVE NC"�H�c��" TO �s���{����
009550*�R�`
009560      WHEN  06
009570         MOVE NC"�R�`��" TO �s���{����
009580*����
009590      WHEN  07
009600         MOVE NC"������" TO �s���{����
009610*���
009620      WHEN  08
009630         MOVE NC"��錧" TO �s���{����
009640*�Ȗ�
009650      WHEN  09
009660         MOVE NC"�Ȗ،�" TO �s���{����
009670*�Q�n
009680      WHEN  10
009690         MOVE NC"�Q�n��" TO �s���{����
009700*���
009710      WHEN  11
009720         MOVE NC"��ʌ�" TO �s���{����
009730*��t
009740      WHEN  12
009750         MOVE NC"��t��" TO �s���{����
009760*����
009770      WHEN  13
009780         MOVE NC"�����s" TO �s���{����
009790*�_�ސ�
009800      WHEN  14
009810         MOVE NC"�_�ސ쌧" TO �s���{����
009820*�V��
009830      WHEN  15
009840         MOVE NC"�V����" TO �s���{����
009850*�x�R
009860      WHEN  16
009870         MOVE NC"�x�R��" TO �s���{����
009880*�ΐ�
009890      WHEN  17
009900         MOVE NC"�ΐ쌧" TO �s���{����
009910*����
009920      WHEN  18
009930         MOVE NC"���䌧" TO �s���{����
009940*�R��
009950      WHEN  19
009960         MOVE NC"�R����" TO �s���{����
009970*����
009980      WHEN  20
009990         MOVE NC"���쌧" TO �s���{����
010000*��
010010      WHEN  21
010020         MOVE NC"�򕌌�" TO �s���{����
010030*�É�
010040      WHEN  22
010050         MOVE NC"�É���" TO �s���{����
010060*���m
010070      WHEN  23
010080         MOVE NC"���m��" TO �s���{����
010090*�O�d
010100      WHEN  24
010110         MOVE NC"�O�d��" TO �s���{����
010120*����
010130      WHEN  25
010140         MOVE NC"���ꌧ" TO �s���{����
010150*���s
010160      WHEN  26
010170         MOVE NC"���s�{" TO �s���{����
010180*���
010190      WHEN  27
010200         MOVE NC"���{" TO �s���{����
010210*����
010220      WHEN  28
010230         MOVE NC"���Ɍ�" TO �s���{����
010240*�ޗ�
010250      WHEN  29
010260         MOVE NC"�ޗǌ�" TO �s���{����
010270*�a�̎R
010280      WHEN  30
010290         MOVE NC"�a�̎R��" TO �s���{����
010300*����
010310      WHEN  31
010320         MOVE NC"���挧" TO �s���{����
010330*����
010340      WHEN  32
010350         MOVE NC"������" TO �s���{����
010360*���R
010370      WHEN  33
010380         MOVE NC"���R��" TO �s���{����
010390*�L��
010400      WHEN  34
010410         MOVE NC"�L����" TO �s���{����
010420*�R��
010430      WHEN  35
010440         MOVE NC"�R����" TO �s���{����
010450*����
010460      WHEN  36
010470         MOVE NC"������" TO �s���{����
010480*����
010490      WHEN  37
010500         MOVE NC"���쌧" TO �s���{����
010510*���Q
010520      WHEN  38
010530         MOVE NC"���Q��" TO �s���{����
010540*���m
010550      WHEN  39
010560         MOVE NC"���m��" TO �s���{����
010570*����
010580      WHEN  40
010590         MOVE NC"������" TO �s���{����
010600*����
010610      WHEN  41
010620         MOVE NC"���ꌧ" TO �s���{����
010630*����
010640      WHEN  42
010650         MOVE NC"���茧" TO �s���{����
010660*�F�{
010670      WHEN  43
010680         MOVE NC"�F�{��" TO �s���{����
010690*�啪
010700      WHEN  44
010710         MOVE NC"�啪��" TO �s���{����
010720*�{��
010730      WHEN  45
010740         MOVE NC"�{�茧" TO �s���{����
010750*������
010760      WHEN  46
010770         MOVE NC"��������" TO �s���{����
010780*����
010790      WHEN  47
010800         MOVE NC"���ꌧ" TO �s���{����
010810*���̑�
010820      WHEN  OTHER
010830         CONTINUE
010840     END-EVALUATE.
010850*================================================================*
010860 �ی���ʖ��Z�b�g SECTION.
010870*
010880**///  ��������̕ύX�͂���SECTION��   ////**
010890*
010900*************************************************************************
010910*  �������:�ی�����(�ی���ʃR�[�h)                                    *
010920*                                                                       *
010930*    10:�Е�(2)                                                         *
010940*    11:�D��(7)                                                         *
010950*    12:����(6)                                                         *
010960*    50:����(1)                                                         *
010970*    51:���ۑg��(1)                                                     *
010980*    52:�ސE(8)                                                         *
010990*    53:�V�l(5)                                                         *
011000*    60:�S�P�V�l(51)                                                    *
011010*    61:��q(52)                                                        *
011020*    62:���c��(55)                                                      *
011030*    63:��Q(53)                                                        *
011040*    64:�픚(54)                                                        *
011050*    65:���̑�(60)                                                      *
011060*    80:�g��(3)                                                         *
011070*    90:����(4)                                                         *
011080*    91:���q��(9)                                                       *
011090*                                                                       *
011100*   �J�ЁA�����ӁA����͂̂���                                          *
011110*                                                                       *
011120*                                                                       *
011130*************************************************************************
011140*
011150*
011160      EVALUATE  ��Q�|�������
011170* ����
011180      WHEN  050
011190          MOVE NC"�����ی�" TO �ی���ʖ�
011200* ���ۑg������
011210      WHEN  051
011220          MOVE NC"���ۑg��" TO �ی���ʖ�
011230* �Е�
011240      WHEN  010
011250*          MOVE NC"�Љ�ی�" TO �ی���ʖ�
011260          MOVE NC"����" TO �ی���ʖ�
011270* �D���͎Еۂŕ\��
011280*      WHEN  011
011290      WHEN  020
011300          MOVE NC"�Љ�ی�" TO �ی���ʖ�
011310* ���ق͎Еۂŕ\��
011320      WHEN  012
011330*          MOVE NC"�Љ�ی�" TO �ی���ʖ�
011340          MOVE NC"����" TO �ی���ʖ�
011350* �g��
011360      WHEN  080
011370          MOVE NC"���ۑg��" TO �ی���ʖ�
011380* ����
011390      WHEN  090
011400          MOVE NC"���ϑg��" TO �ی���ʖ�
011410* ���q���͋��ϑg���ŕ\��
011420      WHEN  091
011430          MOVE NC"���ϑg��" TO �ی���ʖ�
011440* �Q�V�V�l
011450      WHEN  053
011460          IF ��Q�|�ی��Ҕԍ�(1:2) = "39"
011470              MOVE NC"���"     TO �ی���ʖ�
011480          ELSE
011490              MOVE NC"�V���V�l" TO �ی���ʖ�
011500          END-IF
011510* �ސE����
011520      WHEN  052
011530          MOVE NC"�ސE����" TO �ی���ʖ�
011540* �S�P�V�l
011550      WHEN  060
011560          MOVE NC"����V�l" TO �ی���ʖ�
011570* ��q
011580      WHEN  061
011590          MOVE NC"��q�ƒ�" TO �ی���ʖ�
011600* ���c��
011610      WHEN  062
011620          MOVE NC"���c��" TO �ی���ʖ�
011630* ��Q
011640      WHEN  063
011650          MOVE NC"��Q��" TO �ی���ʖ�
011660* �픚
011670      WHEN  064
011680          MOVE NC"�픚��" TO �ی���ʖ�
011690* ���̑�
011700* �����̂W�W�P�R�̂݁u�q�v���󎚂��A����ȊO�͋󔒂Ƃ���
011710      WHEN  065
011720          EVALUATE ��Q�|�ی��Ҕԍ�(1:4)
011730          WHEN "8813"
011740              MOVE NC"�q" TO �ی���ʖ�
011750          WHEN OTHER
011760              MOVE SPACE TO �ی���ʖ�
011770          END-EVALUATE
011780      WHEN OTHER
011790          MOVE SPACE TO �ی���ʖ�
011800     END-EVALUATE.
011810*
011820*================================================================*
011830******************************************************************
011840 END PROGRAM YJK436.
011850******************************************************************
