000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN582.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*       ��o�p���ёւ��ꗗ �y����z
000100*       MED = YHN580 YHN582P 
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2014-09-05
000130 DATE-COMPILED.          2014-09-05
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
000251     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000252                             ORGANIZATION             IS  INDEXED
000253                             ACCESS MODE              IS  DYNAMIC
000254                             RECORD KEY               IS ��|�{�p�a��N��
000255                                                          ��|���҃R�[�h
000256                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000257                                                          ��|���҃J�i
000258                                                          ��|���҃R�[�h
000259                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000260                                                         ��|�{�p�a��N��
000261                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000262                                                          ��|�ی����
000263                                                          ��|�ی��Ҕԍ�
000264                                                          ��|���҃R�[�h
000265                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000266                                                          ��|������
000267                                                     ��|��p���S�Ҕԍ�
000268                                                          ��|���҃R�[�h
000269                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000270                                                          ��|�������
000271                                                  ��|��p���S�Ҕԍ�����
000272                                                          ��|���҃R�[�h
000273                             ALTERNATE RECORD KEY  IS ��|�����a��N��
000274                                                      ��|�{�p�a��N��
000275                                                      ��|���҃R�[�h
000276                             FILE STATUS              IS  ��ԃL�[
000277                             LOCK        MODE         IS  AUTOMATIC.
000278     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000279                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  ���|�����敪
000300                             FILE STATUS              IS  ��ԃL�[
000310                             LOCK        MODE         IS  AUTOMATIC.
000320     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  �ہ|�ی����
000360                                                          �ہ|�ی��Ҕԍ�
000370                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000380                                                          �ہ|�ی��Җ���
000390                                                          �ہ|�ی��Ҕԍ�
000400                             FILE STATUS              IS  ��ԃL�[
000410                             LOCK        MODE         IS  AUTOMATIC.
000420     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS  �s�|������
000460                                                          �s�|�s�����ԍ�
000470                             ALTERNATE RECORD KEY     IS  �s�|������
000480                                                          �s�|�s��������
000490                                                          �s�|�s�����ԍ�
000500                             FILE STATUS              IS  ��ԃL�[
000510                             LOCK        MODE         IS  AUTOMATIC.
000650     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS  �{��|�{�p���ԍ�
000690                             FILE STATUS              IS  ��ԃL�[
000700                             LOCK        MODE         IS  AUTOMATIC.
000650     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS  ���|�敪�R�[�h
000690                                                          ���|���̃R�[�h
000700                             FILE STATUS              IS  ��ԃL�[
000710                             LOCK        MODE         IS  AUTOMATIC.
000541* ���Z���я��p
000931     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5802L.DAT"
000543                             ORGANIZATION             IS  INDEXED
000544                             ACCESS                   IS  DYNAMIC
000545                             RECORD      KEY          IS  ��Q�|�{�p�a��N��
000546                                                          ��Q�|���҃R�[�h
000547                                                          ��Q�|�ی����
000548                             ALTERNATE RECORD KEY     IS  ��Q�|����
000551                             FILE        STATUS       IS  ��ԃL�[
000552                             LOCK        MODE         IS  AUTOMATIC.
000553*
000554     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF001
000560                             SYMBOLIC    DESTINATION  IS "PRT"
000570                             FORMAT                   IS  ��`�̖��o
000580                             GROUP                    IS  ���ڌQ���o
000590                             PROCESSING  MODE         IS  ������ʂo
000600                             UNIT        CONTROL      IS  �g������o
000610                             FILE        STATUS       IS  �ʒm���o.
000620******************************************************************
000630*                      DATA DIVISION                             *
000640******************************************************************
000650 DATA                    DIVISION.
000660 FILE                    SECTION.
000670*                           �m�q�k��  �P�Q�W�n
000680 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000690     COPY GENGOU         OF  XFDLIB  JOINING    ��   AS  PREFIX.
000700*                           �m�q�k��  �R�Q�O�n
000710 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
000720     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000721*                           �m�q�k��  �Q�T�U�n
000722 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000723     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
000724*
000744 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
000754     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002140*                           �m�q�k��  �P�Q�W�n
002150 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
002160     COPY SEJOHO          OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001070*                           �m�q�k��  �P�Q�W�n
001080 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001090     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000971*
001581 FD  ��ƃt�@�C���Q RECORD  CONTAINS 64 CHARACTERS.
001582 01  ��Q�|���R�[�h.
001583     03  ��Q�|���R�[�h�L�[.
001584         05  ��Q�|�{�p�a��N��.
001585             07  ��Q�|�{�p�a��            PIC 9.
001586             07  ��Q�|�{�p�N              PIC 9(2).
001587             07  ��Q�|�{�p��              PIC 9(2).
001588         05  ��Q�|���҃R�[�h.
001589             07 ��Q�|���Ҕԍ�             PIC 9(6).
001590             07 ��Q�|�}��                 PIC X(1).
001591         05  ��Q�|�ی����                PIC 9(2).
001592     03  ��Q�|���R�[�h�f�[�^.
001593         05  ��Q�|�Ԗߋ敪                PIC 9.
001594         05  ��Q�|���ރR�[�h              PIC 9(1).
001261         05  ��Q�|���R�[�h                PIC X(2).
001400         05  ��Q�|�ی���                  PIC 9(2).
001595         05  ��Q�|����                    PIC 9(4).
001596         05  ��Q�|�{�l�Ƒ��敪            PIC 9.
001597         05  ��Q�|�ی��Ҕԍ�              PIC X(8).
001591         05  ��Q�|���S����                PIC 9(2).
               05  ��Q�|��p�z                  PIC 9(6).
               05  ��Q�|���S�z                  PIC 9(6).
               05  ��Q�|�����z                  PIC 9(6).
001598         05  FILLER                        PIC X(11).
000994*
000995 FD  ����t�@�C��.
001000     COPY YHN582P        OF  XMDLIB.
001001*
001010******************************************************************
001020*                WORKING-STORAGE SECTION                         *
001030******************************************************************
001040 WORKING-STORAGE         SECTION.
001050 01 �s�J�E���^                         PIC 9(2) VALUE ZERO.
001060 01 �ŃJ�E���^                         PIC 9(4) VALUE ZERO.
001070 01 �ő�s��                           PIC 9(2) VALUE ZERO.
001080 01 �w�b�_�s��                         PIC 9(2) VALUE ZERO.
001090 01 �L�[����                           PIC X    VALUE SPACE.
001100 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
001110 01 �I���t���O                         PIC X(3) VALUE SPACE.
001120 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
001130 01 �I���t���O�R                       PIC X(3) VALUE SPACE.
001140 01 �m�F���͂v                         PIC X(1) VALUE SPACE.
001150 01 �t�@�C�����v                       PIC N(2) VALUE SPACE.
001160 01 �J�����g�����v                     PIC 9(1) VALUE ZERO.
001210 01 �I�[�v���t���O                     PIC X(3) VALUE SPACE.
001170 01 ���s�L�[�v                         PIC X(4) VALUE SPACE.
001180 01 �O�a��v                           PIC 9 VALUE ZERO.
001190 01 �Ԗ߂e                             PIC 9 VALUE ZERO.
001200 01 �����ړ��L�[                       PIC X(4) VALUE SPACE.
001210 01 �{�l�Ƒ��v                         PIC 9(1) VALUE ZERO.
001220 01 �Ԗ߂v                             PIC 9(1) VALUE ZERO.
001220 01 ���ރR�[�h�v�q                     PIC 9(1) VALUE ZERO.
001220 01 ���R�[�h�v�q                       PIC X(2) VALUE SPACE.
001220 01 �ی����v�q                         PIC 9(2) VALUE ZERO.
001220 01 �{���v                             PIC X(2) VALUE SPACE.
001220 01 �����v.
          03 �����v�o                        PIC X(8) VALUE SPACE.
001220 01 �ی���ʂv                         PIC 9(2) VALUE ZERO.
001597 01 �ی��Ҕԍ��v�q                     PIC X(8) VALUE SPACE.
001200 01 ��ʂv                             PIC X(30) VALUE SPACE.
001230 01 �ԍ��v.
001231    03 ���ރR�[�h�v                    PIC 9 VALUE ZERO.
001232    03 FILLER                          PIC X VALUE "-".
001233    03 ���Ԃv                          PIC 9(3) VALUE ZERO.
       01 �V�X�e�����t�v.
          03 ����v                          PIC X(2) VALUE SPACE.
          03 �N�����v                        PIC X(6) VALUE SPACE.
       01 ���Ԃv�o.
          03 �����b�v                        PIC X(6) VALUE SPACE.
          03 �b�ȉ��v                        PIC X(2) VALUE SPACE.
001200 01 ������t�v                         PIC X(20) VALUE SPACE.
001240*
001250 01 �����於�̂v.
001260    03 ��������於�̂v                PIC X(40) VALUE SPACE.
001270    03 FILLER                          PIC X(30) VALUE SPACE.
001280 01 �{�p�a��N���v�q.
001290    03 �{�p�a��v�q                    PIC 9    VALUE ZERO.
001300    03 �{�p�N���v�q.
001310       05 �{�p�N�v�q                   PIC 9(2) VALUE ZERO.
001320       05 �{�p���v�q                   PIC 9(2) VALUE ZERO.
001330 01 �����a��N�����v.
001340    03 �����a��v                      PIC 9    VALUE ZERO.
001350    03 �����N���v.
001360       05 �����N�v                     PIC 9(2) VALUE ZERO.
001370       05 �������v                     PIC 9(2) VALUE ZERO.
001380    03 �������v                        PIC 9(2) VALUE ZERO.
001390 01 �������̂v                         PIC N(2) VALUE SPACE.
007080**************
007090* �{�p����� *
007100**************
007110 01 �{�p�����v.
007120    03 �_���t�ԍ��v                    PIC X(22)  VALUE SPACE.
007130    03 �ڍ��t�����ԍ��v              PIC X(10)  VALUE SPACE.
007140    03 ��\�҃J�i�v                    PIC X(50)  VALUE SPACE.
007150    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
007160    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
007170    03 �{�p���Z���v.
007180       05 �{�p���Z���P�v               PIC X(50)  VALUE SPACE.
007190       05 �{�p���Z���Q�v               PIC X(50)  VALUE SPACE.
007200    03 �{�p���X�֔ԍ��v.
007210       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
007220       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
007230    03 �{�p���d�b�ԍ��v                PIC X(15)  VALUE SPACE.
001800** ���v�p
       01 ���v�v.
          03 ���v�����v                      PIC 9(4) VALUE ZERO.
          03 ���v��p�z�v                    PIC 9(10) VALUE ZERO.
          03 ���v���S�z�v                    PIC 9(10) VALUE ZERO.
          03 ���v�����z�v                    PIC 9(10) VALUE ZERO.
          03 ���v���������v                  PIC 9(4) VALUE ZERO.
          03 ���v�󋋎ҕ��S���v              PIC 9(10) VALUE ZERO.
       01 ��ʍ��v�v.
          03 ��ʍ��v�����v                  PIC 9(4) VALUE ZERO.
          03 ��ʍ��v��p�z�v                PIC 9(10) VALUE ZERO.
          03 ��ʍ��v���S�z�v                PIC 9(10) VALUE ZERO.
          03 ��ʍ��v�����z�v                PIC 9(10) VALUE ZERO.
          03 ��ʍ��v���������v              PIC 9(4) VALUE ZERO.
          03 ��ʍ��v�󋋎ҕ��S���v          PIC 9(10) VALUE ZERO.
      *
001400* �G���[���b�Z�[�W�p
001410 01 �G���[���b�Z�[�W�v.
001420    03 �G���[���҃R�[�h�v              PIC X(7)  VALUE SPACE.
001430    03 �G���[��؂�v                  PIC X(1)  VALUE SPACE.
001440    03 �G���[�ی���ʂv                PIC X(2)  VALUE SPACE.
001450    03 FILLER                          PIC X(10) VALUE SPACE.
001650*
001651***********************************************************************
001660 01 �������.
001670     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
001680     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
001690     03 ������ʂo                     PIC X(2) VALUE SPACE.
001700     03 �g������o.
001710         05 �[������o.
001720             07 �ړ������o             PIC X(1).
001730             07 �ړ��s���o             PIC 9(3).
001740         05 �ڍא���o                 PIC X(2).
001750     03 �ʒm���o                     PIC X(2) VALUE SPACE.
001760     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
001770 01 �v�Z�@����N�v                     PIC 9(2).
001780* ���t�v�n�q�j
001790 01 �a��I���N�v                       PIC 9(4).
001800 01 �v�Z�@�a��N�v                     PIC 9(2).
001810 01 �v�Z�@����.
001820    03 �v�Z�@����N                    PIC 9(4).
001830    03 �v�Z�@�����                  PIC 9(4).
001840 01 �v�Z�@����q REDEFINES �v�Z�@����.
001850    03 �v�Z�@���I                      PIC 9(2).
001860    03 �v�Z�@���t                      PIC 9(6).
001870    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
001880       05 �v�Z�@�N��                   PIC 9(4).
001890       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
001900         07 �v�Z�@�N                   PIC 9(2).
001910         07 �v�Z�@��                   PIC 9(2).
001920       05 �v�Z�@��                     PIC 9(2).
001930*
001940******************************************************************
001950*                          �A������                              *
001960******************************************************************
001970*
001980**********************
001990* ���b�Z�[�W�\���L�[ *
002000**********************
002010*
002020 01 �A���|�L�[ IS EXTERNAL.
002030    03  �A���|���b�Z�[�W                 PIC N(20).
002040*
002050 01 �A���R�|�L�[ IS EXTERNAL.
002060    03  �A���R�|���b�Z�[�W             PIC N(20).
002070    03  �A���R�|���b�Z�[�W�P           PIC X(20).
002140*
002150****************
002160* ��ʓ��͏�� *
002170****************
002180 01 �A���|��ʏ��x�g�m�T�W�O IS EXTERNAL.
002270    03 �A���|�����a��N��.
002280       05 �A���|�����a��               PIC 9.
002290       05 �A���|�����N��.
002300         07 �A���|�����N               PIC 9(2).
002310         07 �A���|������               PIC 9(2).
          03 �A���|�v���r���[�敪             PIC 9.
002320*
002260 01 �A��|������x�g�m�T�W�P IS EXTERNAL.
           03 �A��|��������                 PIC 9(4).
           03 �A��|�������v�z               PIC 9(8).
           03 �A��|�������S���z             PIC 9(8).
           03 �A��|�����������z             PIC 9(8).
           03 �A��|�Ԗߌ���                 PIC 9(4).
           03 �A��|�Ԗߍ��v�z               PIC 9(8).
           03 �A��|�Ԗߕ��S���z             PIC 9(8).
           03 �A��|�Ԗߐ������z             PIC 9(8).
           03 �A��|������                   PIC 9(4).
           03 �A��|�����v�z                 PIC 9(8).
           03 �A��|�����S���z               PIC 9(8).
           03 �A��|���������z               PIC 9(8).
002231*
000540************************************
000550* �v�����^�t�@�C���쐬�p           *
000560************************************
000570 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000580     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
000590     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
000600     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
000610     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
002240*
002240******************************************************************
002250*                      PROCEDURE  DIVISION                       *
002260******************************************************************
002270 PROCEDURE               DIVISION.
002280************
002290*           *
002300* ��������   *
002310*           *
002320************
002560     MOVE SPACE TO �I�[�v���t���O.
002570     PERFORM �v�����^�t�@�C���쐬.
002330     PERFORM ������.
002340************
002350*           *
002360* �又��     *
002370*           *
002380************
002390     PERFORM �������.
002400************
002410*           *
002420* �I������   *
002430*           *
002440************
002450     PERFORM �I������.
002460     EXIT PROGRAM.
002470*
002480*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
      *   �󎚒����Ȃ�
002971     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
002972*
002973*   �g�p���钠�[�v���O�������Z�b�g
002974     MOVE "YHN582"              TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
002490*================================================================*
002500 ������ SECTION.
002510*
002520     OPEN INPUT �����}�X�^.
002530         MOVE NC"����" TO �t�@�C�����v.
002540          PERFORM �I�[�v���`�F�b�N.
002550     OPEN INPUT �ی��҃}�X�^.
002560         MOVE NC"�ی��҃}�X�^" TO �t�@�C�����v.
002570         PERFORM �I�[�v���`�F�b�N.
002571     OPEN INPUT �s�����}�X�^
002572         MOVE NC"�s����" TO �t�@�C�����v.
002573         PERFORM �I�[�v���`�F�b�N.
002574     OPEN INPUT ��f�ҏ��e.
002575         MOVE NC"��f��" TO �t�@�C�����v.
002576         PERFORM �I�[�v���`�F�b�N.
014470     OPEN INPUT   �{�p�����}�X�^.
014480         MOVE NC"�{��" TO �t�@�C�����v.
014490         PERFORM �I�[�v���`�F�b�N.
003540     OPEN INPUT ���̃}�X�^.
003550         MOVE NC"���̃}�X�^"   TO �t�@�C�����v.
003560         PERFORM �I�[�v���`�F�b�N.
002577*
002580     OPEN INPUT ��ƃt�@�C���Q.
002590         MOVE NC"��Q" TO �t�@�C�����v.
002600         PERFORM �I�[�v���`�F�b�N.
002640*
002650*================================================================*
002660 �I�[�v���`�F�b�N SECTION.
002670*
002680     IF ��ԃL�[  NOT =  "00"
002690         DISPLAY �t�@�C�����v NC"�e�I�[�v���G���[" UPON CONS
002700         DISPLAY NC"��ԃL�[�F" ��ԃL�[           UPON CONS
002710         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
002720                                                   UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
002730         ACCEPT  �L�[���� FROM CONS
002740         PERFORM �t�@�C����
002750         EXIT PROGRAM.
002760*================================================================*
002770 �t�@�C���� SECTION.
002780*
003570     IF ( �I�[�v���t���O = "YES" )
003580         CLOSE ����t�@�C��
003630     END-IF.
003640*
002790     CLOSE  ��f�ҏ��e �����}�X�^  �ی��҃}�X�^   �{�p�����}�X�^
                  ��ƃt�@�C���Q  �s�����}�X�^ ���̃}�X�^.
002800*================================================================*
002810 �I������ SECTION.
002820*
002830     PERFORM �t�@�C����.
002840*================================================================*
002850 ������� SECTION.
002860*
002870     MOVE 40    TO �ő�s��.
002880*
002890     MOVE ZERO  TO ���v�v.
002890     MOVE ZERO  TO ��ʍ��v�v.
002890     MOVE ZERO  TO �s�J�E���^.
002900     MOVE ZERO  TO �ŃJ�E���^.
002910     MOVE SPACE TO �I���t���O.
002930*
           ACCEPT �N�����v FROM DATE.
      *    /* 1980�`2079�N�̊ԂŐݒ� */
           IF �N�����v(1:2) > 80
               MOVE 19 TO ����v
           ELSE
               MOVE 20 TO ����v
           END-IF.
           ACCEPT ���Ԃv�o FROM TIME.
      *
           STRING �V�X�e�����t�v(1:4)    DELIMITED BY SIZE
                  "/"                    DELIMITED BY SIZE
                  �V�X�e�����t�v(5:2)    DELIMITED BY SIZE
                  "/"                    DELIMITED BY SIZE
                  �V�X�e�����t�v(7:2)    DELIMITED BY SIZE
                  " "                    DELIMITED BY SIZE
                  ���Ԃv�o(1:2)          DELIMITED BY SIZE
                  ":"                    DELIMITED BY SIZE
                  ���Ԃv�o(3:2)          DELIMITED BY SIZE
                  ":"                    DELIMITED BY SIZE
                  ���Ԃv�o(5:2)          DELIMITED BY SIZE
             INTO ������t�v
           END-STRING.
      *
002970     MOVE ZERO  TO ��Q�|���ރR�[�h.
002960     MOVE ZERO  TO ��Q�|���R�[�h.
002970     MOVE ZERO  TO ��Q�|�ی��Ҕԍ�.
002980     MOVE ZERO  TO ��Q�|����.
003020*
003030     START ��ƃt�@�C���Q KEY IS > ��Q�|����
003110     END-START.
003120     IF ��ԃL�[  =  "00"
003130         PERFORM ��ƃt�@�C���Q�Ǎ�
003140         MOVE ��Q�|���ރR�[�h  TO ���ރR�[�h�v�q
003140         MOVE ��Q�|�ی��Ҕԍ�  TO �ی��Ҕԍ��v�q
003200         MOVE ��Q�|���R�[�h    TO ���R�[�h�v�q
003200         MOVE ��Q�|�ی���      TO �ی����v�q
003160         COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
003170         PERFORM �w�b�_���R�[�h�Z�b�g
003180         PERFORM �w�b�_�������
               PERFORM ��ʃ��R�[�h�Z�b�g
               PERFORM ��ʈ������
003190         PERFORM UNTIL �I���t���O = "YES"
003200             IF ( �s�J�E���^ >= �ő�s��   )
003210                 PERFORM ���ŏ���
003220                 COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
003230                 PERFORM �w�b�_���R�[�h�Z�b�g
003240                 PERFORM �w�b�_�������
003251                 MOVE ZERO    TO �s�J�E���^
003260             ELSE
                       IF ((��Q�|���R�[�h = SPACE) AND (��Q�|�ی��Ҕԍ� NOT = �ی��Ҕԍ��v�q)) OR
                          ((��Q�|���R�[�h NOT = SPACE) AND (��Q�|���R�[�h NOT = ���R�[�h�v�q )) OR
                          ((���ރR�[�h�v�q = 5) AND (��Q�|�ی���     NOT = �ی����v�q))
                           PERFORM ���v���R�[�h�Z�b�g
                           PERFORM ���v�������
                           MOVE ZERO TO ���v�v
003140                     MOVE ��Q�|�ی��Ҕԍ�  TO �ی��Ҕԍ��v�q
003200                     MOVE ��Q�|���R�[�h    TO ���R�[�h�v�q
003200                     MOVE ��Q�|�ی���      TO �ی����v�q
003200                     IF ( �s�J�E���^ >= �ő�s��   )
003210                        PERFORM ���ŏ���
003220                        COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
003230                        PERFORM �w�b�_���R�[�h�Z�b�g
003240                        PERFORM �w�b�_�������
003251                        MOVE ZERO    TO �s�J�E���^
                           END-IF
                       END-IF
                       IF ( ��Q�|���ރR�[�h NOT = ���ރR�[�h�v�q )
                           PERFORM ��ʍ��v���R�[�h�Z�b�g
                           PERFORM ���v�������
                           MOVE ZERO TO ��ʍ��v�v
003200                     IF ( �s�J�E���^ >= �ő�s��   )
003210                        PERFORM ���ŏ���
003220                        COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
003230                        PERFORM �w�b�_���R�[�h�Z�b�g
003240                        PERFORM �w�b�_�������
003251                        MOVE ZERO    TO �s�J�E���^
                           END-IF
                           PERFORM ��ʃ��R�[�h�Z�b�g
                           PERFORM ��ʈ������
003140                     MOVE ��Q�|���ރR�[�h  TO ���ރR�[�h�v�q
003200                     IF ( �s�J�E���^ >= �ő�s��   )
003210                        PERFORM ���ŏ���
003220                        COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
003230                        PERFORM �w�b�_���R�[�h�Z�b�g
003240                        PERFORM �w�b�_�������
003251                        MOVE ZERO    TO �s�J�E���^
                           END-IF
                       END-IF
003320                 PERFORM ���׃��R�[�h�Z�b�g
003330                 PERFORM ���׈������
003330                 PERFORM �W�v����
003360                 PERFORM ��ƃt�@�C���Q�Ǎ�
003370             END-IF
003380         END-PERFORM
               PERFORM ���v���R�[�h�Z�b�g
               PERFORM ���v�������
003200         IF ( �s�J�E���^ >= �ő�s��   )
003210            PERFORM ���ŏ���
003220            COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
003230            PERFORM �w�b�_���R�[�h�Z�b�g
003240            PERFORM �w�b�_�������
003251            MOVE ZERO    TO �s�J�E���^
               END-IF
               PERFORM ��ʍ��v���R�[�h�Z�b�g
               PERFORM ���v�������
003200         IF ( �s�J�E���^ >= �ő�s��   )
003210            PERFORM ���ŏ���
003220            COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
003230            PERFORM �w�b�_���R�[�h�Z�b�g
003240            PERFORM �w�b�_�������
003251            MOVE ZERO    TO �s�J�E���^
               END-IF
               PERFORM �����v���R�[�h�Z�b�g
               PERFORM �����v�������
003400     END-IF.
003401*
003410*================================================================*
003420 ��ƃt�@�C���Q�Ǎ� SECTION.
003430*
003440     READ ��ƃt�@�C���Q NEXT
003450     AT END
003460         MOVE "YES" TO �I���t���O
003470     END-READ.
003471*
003480*================================================================*
003490 �w�b�_���R�[�h�Z�b�g SECTION.
003500*
003510     MOVE SPACE TO YHN582P.
003520     INITIALIZE YHN582P.
003530     MOVE �ŃJ�E���^        TO ��.
           MOVE ������t�v        TO ������t.
003546*
      */�����C��/������20190514
           MOVE �A���|�����a��     TO ���|�����敪
037380     READ �����}�X�^
037390     NOT INVALID KEY
037400         MOVE ���|��������   TO �����a��
037410     END-READ.
      */�����C��/������20190514
003620     MOVE �A���|�����N      TO �����N.
003630     MOVE �A���|������      TO ������.
           PERFORM �{�p�����擾.
           MOVE �ڍ��t�����ԍ��v TO ����ԍ�.
           MOVE �ڍ��@���v        TO �{�p����.
           MOVE ��\�Җ��v        TO ��\�Җ�.
003631*
003640*================================================================*
003650 ���׃��R�[�h�Z�b�g SECTION.
003660*
003675     MOVE SPACE TO YHN582P.
003680     INITIALIZE YHN582P.
003690*
003700     MOVE ��Q�|����         TO �ԍ�.
003710*
           MOVE 10              TO ���|�敪�R�[�h.
           MOVE ��Q�|�ی����  TO ���|���̃R�[�h.
           READ ���̃}�X�^
           NOT INVALID KEY
               MOVE ���|����       TO ���
           END-READ.
           EVALUATE ��Q�|�ی����
           WHEN 1
               IF ��Q�|�ی��Ҕԍ�(3:1) = 3
                  MOVE NC"���g"    TO ���
               END-IF
           WHEN 8
               MOVE NC"�ލ�"       TO ���
           WHEN 5
               MOVE NC"���"       TO ���
           END-EVALUATE.
003711*
003772     IF ��Q�|�{�l�Ƒ��敪 = 1
003773         MOVE NC"�{"     TO �{�l�Ƒ�
003774     ELSE
003775         MOVE NC"��"     TO �{�l�Ƒ�
003776     END-IF.
003781*
003782     MOVE ��Q�|�ی��Ҕԍ�     TO �ی��Ҕԍ�.
003790     PERFORM ��������擾.
003800     MOVE �����於�̂v         TO �ی��Җ���.
003810     MOVE ��Q�|���Ҕԍ�       TO ���Ҕԍ�.
003810     MOVE ��Q�|�}��           TO �}��.
003811*
003813     MOVE ��Q�|�{�p�a��       TO ��|�{�p�a��.
003814     MOVE ��Q�|�{�p�N         TO ��|�{�p�N.
003815     MOVE ��Q�|�{�p��         TO ��|�{�p��.
003816     MOVE ��Q�|���҃R�[�h     TO ��|���҃R�[�h.
003817     READ ��f�ҏ��e
003821     NOT INVALID KEY
003845         MOVE ��|���Ҏ���     TO ���Ҏ���
003856     END-READ.
003711*
003772     EVALUATE ��Q�|�Ԗߋ敪
           WHEN 1
003773         MOVE NC"��"           TO �Ԗ�
003774     WHEN 2
003775         MOVE NC"��"           TO �Ԗ�
003776     END-EVALUATE.
003893*
003894     IF �A���|�����a��N�� NOT = ��Q�|�{�p�a��N��
003895         MOVE ��Q�|�{�p�N     TO �{�p�N
003896         MOVE ��Q�|�{�p��     TO �{�p��
003897         MOVE "/"              TO ���
003898     END-IF.
003690*
           MOVE ��Q�|���S����       TO ����.
           MOVE NC"��"               TO ��.
003700     MOVE ��Q�|��p�z         TO ��p�z.
003700     MOVE ��Q�|���S�z         TO �ꕔ���S��.
003700     MOVE ��Q�|�����z         TO �����z.
           IF ��Q�|�ی���� > 50
              MOVE "*"               TO ��p�z����
              MOVE "*"               TO �ꕔ���S������
           END-IF.
      *
           IF ��Q�|�Ԗߋ敪 = 2
              MOVE 1                 TO �Ԗ߂e
           END-IF.
003899*
003640*================================================================*
003650 ��ʃ��R�[�h�Z�b�g SECTION.
003857*
003675     MOVE SPACE TO YHN582P.
003680     INITIALIZE YHN582P.
003690*
003858     EVALUATE ��Q�|���ރR�[�h
003862     WHEN 01
003863         MOVE "�S�����N�ی�����"            TO �ی���� ��ʂv
003862     WHEN 02
003863         MOVE "�D���ی�"                    TO �ی���� ��ʂv
           WHEN 03
003865         MOVE "���N�ی��g��"                TO �ی���� ��ʂv
003866     WHEN 04
003867         MOVE "���ϑg���A���q��"            TO �ی���� ��ʂv
           WHEN 05
003861         MOVE "���ۘA����"                  TO �ی���� ��ʂv
003868     WHEN 06
003875         MOVE "��Ï���"                    TO �ی���� ��ʂv
003890     END-EVALUATE.
003899*
003640*================================================================*
003650 ���v���R�[�h�Z�b�g SECTION.
003857*
003675     MOVE SPACE TO YHN582P.
003680     INITIALIZE YHN582P.
003857*
           IF (���R�[�h�v�q = SPACE) OR (���ރR�[�h�v�q = 1 OR 2)
              STRING "�@�@�y���v�z�@"        DELIMITED BY SIZE
                     �����於�̂v            DELIMITED BY SPACE
                INTO ���v�ی����
              END-STRING
           ELSE
              EVALUATE ���R�[�h�v�q
              WHEN 01
                  MOVE SPACE         TO �{���v
              WHEN 13
                  MOVE "�s"          TO �{���v
              WHEN 26
              WHEN 27
                  MOVE "�{"          TO �{���v
              WHEN OTHER
                  MOVE "��"          TO �{���v
              END-EVALUATE
              MOVE 13                TO ���|�敪�R�[�h
              MOVE ���R�[�h�v�q      TO ���|���̃R�[�h
              READ ���̃}�X�^
              NOT INVALID KEY
                 MOVE ���|����       TO �����v
              END-READ
              STRING "�@�@�y���v�z�@"         DELIMITED BY SIZE
                     �����v�o                 DELIMITED BY "�@"
                     �{���v                   DELIMITED BY SPACE
                     "�������N�ی��c�̘A����" DELIMITED BY SIZE
                INTO ���v�ی����
              END-STRING
           END-IF.
      *
           IF �Ԗ߂e = 1
              MOVE "���Ԗߕ��͏W�v����܂���" TO �Ԗ߃R�����g
              MOVE ZERO              TO �Ԗ߂e
           END-IF.
      *
           MOVE ���v�����v           TO ���v����.
           MOVE ���v��p�z�v         TO ���v��p�z.
           MOVE ���v���S�z�v         TO ���v�ꕔ���S��.
           MOVE ���v�����z�v         TO ���v�����z.
           IF ���v���������v NOT = ZERO
              MOVE ���v���������v     TO ���v��������
              MOVE ���v�󋋎ҕ��S���v TO ���v�󋋎ҕ��S��
              MOVE "��"               TO ������
              MOVE "*"             TO �󋋎ҕ��S������
              IF ���v�󋋎ҕ��S���v = ZERO
                 MOVE "0"             TO �󋋎ҕ��S���[��
              END-IF
           END-IF.
003899*
003640*================================================================*
003650 ��ʍ��v���R�[�h�Z�b�g SECTION.
003857*
003675     MOVE SPACE TO YHN582P.
003680     INITIALIZE YHN582P.
003857*
           STRING "�y��ʍ��v�z�@"        DELIMITED BY SIZE
                  ��ʂv                  DELIMITED BY SPACE
             INTO ���v�ی����
           END-STRING.
      *
           MOVE ��ʍ��v�����v           TO ���v����.
           MOVE ��ʍ��v��p�z�v         TO ���v��p�z.
           MOVE ��ʍ��v���S�z�v         TO ���v�ꕔ���S��.
           MOVE ��ʍ��v�����z�v         TO ���v�����z.
           IF ��ʍ��v���������v NOT = ZERO
              MOVE ��ʍ��v���������v     TO ���v��������
              MOVE ��ʍ��v�󋋎ҕ��S���v TO ���v�󋋎ҕ��S��
              MOVE "��"                   TO ������
              MOVE "*"             TO �󋋎ҕ��S������
              IF ��ʍ��v�󋋎ҕ��S���v = ZERO
                 MOVE "0"             TO �󋋎ҕ��S���[��
              END-IF
           END-IF.
003899*
003640*================================================================*
003650 �����v���R�[�h�Z�b�g SECTION.
003857*
003675     MOVE SPACE TO YHN582P.
003680     INITIALIZE YHN582P.
003857*
           MOVE �A��|��������         TO �������v����.
           MOVE �A��|�������v�z       TO �������v��p�z.
           MOVE �A��|�������S���z     TO �������v�ꕔ���S��.
           MOVE �A��|�����������z     TO �������v�����z.
003857*
           IF �A��|�Ԗߌ��� NOT = ZERO
               MOVE "�Ԗߕ��F"         TO �Ԗߕ�
               MOVE "��"               TO �Ԗߌ�
               MOVE �A��|�Ԗߌ���     TO �Ԗߌ���
               MOVE �A��|�Ԗߍ��v�z   TO �Ԗߔ�p�z
               MOVE �A��|�Ԗߕ��S���z TO �Ԗ߈ꕔ���S��
               MOVE �A��|�Ԗߐ������z TO �Ԗߐ����z
           END-IF.
      *
           MOVE �A��|������           TO �����v����.
           MOVE �A��|�����v�z         TO �����v��p�z.
           MOVE �A��|�����S���z       TO �����v�ꕔ���S��.
           MOVE �A��|���������z       TO �����v�����z.
003899*
003640*================================================================*
003650 �W�v���� SECTION.
003857*
           IF ��Q�|�Ԗߋ敪 NOT = 2
              IF ��Q�|�ی���� < 10
                 COMPUTE ���v�����v   = ���v�����v + 1
                 COMPUTE ���v��p�z�v = ���v��p�z�v + ��Q�|��p�z
                 COMPUTE ���v���S�z�v = ���v���S�z�v + ��Q�|���S�z
                 COMPUTE ���v�����z�v = ���v�����z�v + ��Q�|�����z
      *
                 COMPUTE ��ʍ��v�����v   = ��ʍ��v�����v + 1
                 COMPUTE ��ʍ��v��p�z�v = ��ʍ��v��p�z�v + ��Q�|��p�z
                 COMPUTE ��ʍ��v���S�z�v = ��ʍ��v���S�z�v + ��Q�|���S�z
                 COMPUTE ��ʍ��v�����z�v = ��ʍ��v�����z�v + ��Q�|�����z
              ELSE
                 COMPUTE ���v���������v     = ���v���������v + 1
                 COMPUTE ���v�󋋎ҕ��S���v = ���v�󋋎ҕ��S���v + ��Q�|���S�z
                 COMPUTE ���v�����z�v = ���v�����z�v + ��Q�|�����z
      *
                 COMPUTE ��ʍ��v���������v     = ��ʍ��v���������v + 1
                 COMPUTE ��ʍ��v�󋋎ҕ��S���v = ��ʍ��v�󋋎ҕ��S���v + ��Q�|���S�z
                 COMPUTE ��ʍ��v�����z�v = ��ʍ��v�����z�v + ��Q�|�����z
              END-IF
           END-IF.
      *
022390*================================================================*
022400 �{�p�����擾 SECTION.
022410*================================================================*
022420**************************************************
022430* �{�@�f�[�^���g�p���A�ȉ��̏����擾           *
022440* �� �_���t�ԍ�.. �_���t�ԍ��v�Ɋi�[             *
022450* �� ����ԍ� ... �ڍ��t�����ԍ��v�Ɋi�[       *
022460* �� ��\�Җ� ... ��\�Җ��v�Ɋi�[               *
022470* �� �Z��1,2   ...�{�p���Z��1,2�v�Ɋi�[          *
022480* �� �d�b�ԍ� ... �{�p���d�b�ԍ��v�Ɋi�[         *
022490**************************************************
022500     MOVE ZERO  TO �{��|�{�p���ԍ�.
022510     READ �{�p�����}�X�^
022520     INVALID KEY
022530         CONTINUE
022540     NOT INVALID KEY
022550*
022590         MOVE �{��|�V�_���t�ԍ�      TO �_���t�ԍ��v
022610*
022620         MOVE �{��|�ڍ��t�����ԍ�  TO �ڍ��t�����ԍ��v
022630*
022640         MOVE �{��|�X�֔ԍ��P        TO �{�p���X�֔ԍ��P�v
022650         MOVE �{��|�X�֔ԍ��Q        TO �{�p���X�֔ԍ��Q�v
022660         MOVE �{��|��\�҃J�i        TO ��\�҃J�i�v
022670         MOVE �{��|��\�Җ�          TO ��\�Җ��v
022680         MOVE �{��|�ڍ��@��          TO �ڍ��@���v
022690*
022700         MOVE �{��|�Z���P            TO �{�p���Z���P�v
022710         MOVE �{��|�Z���Q            TO �{�p���Z���Q�v
022720*
022730         MOVE �{��|�d�b�ԍ�          TO �{�p���d�b�ԍ��v
022750**
023010     END-READ.
003900*================================================================*
003901 ��������擾 SECTION.
003910*
003920     IF ( ��Q�|�ی���� NOT = 05 ) AND ( ��Q�|�ی���� < 10 )
003930*    / ���� /
003960        MOVE ��Q�|�ی����      TO �ہ|�ی����
003970        MOVE ��Q�|�ی��Ҕԍ�    TO �ہ|�ی��Ҕԍ�
003980        MOVE SPACE               TO �����於�̂v
003990        READ �ی��҃}�X�^
004000        INVALID KEY
004010            MOVE SPACE           TO �����於�̂v
004020        NOT INVALID KEY
004030            MOVE �ہ|�ی��Җ���  TO �����於�̂v
004040        END-READ
004041     ELSE
004042*    / �V�l�E���� /
004045        MOVE ��Q�|�ی����      TO �s�|������
004046        MOVE ��Q�|�ی��Ҕԍ�    TO �s�|�s�����ԍ�
004048        READ �s�����}�X�^
004049        INVALID KEY
004050            MOVE SPACE           TO �����於�̂v
004051        NOT INVALID KEY
                  IF ��Q�|�ی���� = 05
004062                MOVE �s�|�x����  TO �����於�̂v
                  ELSE
004062                MOVE �s�|�s��������  TO �����於�̂v
                  END-IF
004064        END-READ
004065     END-IF.
004066*
004068*================================================================*
004069 �w�b�_������� SECTION.
004070*
006050     IF ( �I�[�v���t���O NOT = "YES" )
006060        MOVE "YES" TO �I�[�v���t���O
006070        OPEN I-O  ����t�@�C��
006080        PERFORM �G���[�����o
006090     END-IF.
004080     MOVE "YHN582P"  TO  ��`�̖��o.
004090     MOVE SPACE      TO  ������ʂo.
004100     MOVE "HEAD"     TO  ���ڌQ���o.
004110     WRITE YHN582P.
004120     PERFORM �G���[�����o.
004130*================================================================*
004140 ��ʈ������ SECTION.
004150*
004160     MOVE "YHN582P"  TO  ��`�̖��o.
004090     MOVE SPACE      TO  ������ʂo.
004170     MOVE "GRP001"   TO  ���ڌQ���o.
004180     WRITE YHN582P.
004190     PERFORM �G���[�����o.
004200     COMPUTE �s�J�E���^ = �s�J�E���^ + 2.
004130*================================================================*
004140 ���׈������ SECTION.
004150*
004160     MOVE "YHN582P"  TO  ��`�̖��o.
004090     MOVE SPACE      TO  ������ʂo.
004170     MOVE "GRP002"   TO  ���ڌQ���o.
004180     WRITE YHN582P.
004190     PERFORM �G���[�����o.
004200     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
004210*================================================================*
004220 ���v������� SECTION.
004230*
004240     MOVE "YHN582P"  TO  ��`�̖��o.
004090     MOVE SPACE      TO  ������ʂo.
004250     MOVE "FOOT1"    TO  ���ڌQ���o.
004260     WRITE YHN582P.
004270     PERFORM �G���[�����o.
004280     COMPUTE �s�J�E���^ = �s�J�E���^ + 2.
004210*================================================================*
004220 �����v������� SECTION.
004230*
004240     MOVE "YHN582P"  TO  ��`�̖��o.
004090     MOVE SPACE      TO  ������ʂo.
004250     MOVE "FOOT2"    TO  ���ڌQ���o.
004260     WRITE YHN582P.
004270     PERFORM �G���[�����o.
004280     COMPUTE �s�J�E���^ = �s�J�E���^ + 2.
004290*================================================================*
004300 ���ŏ���  SECTION.
004310*
004320     MOVE SPACE TO YHN582P.
004330     INITIALIZE YHN582P.
004340     MOVE "YHN582P" TO  ��`�̖��o.
004350     MOVE "CT"      TO  ������ʂo.
004360     MOVE "PAGE"    TO  �g������o.
004370     MOVE SPACE     TO  ���ڌQ���o.
004380     WRITE YHN582P.
004390     PERFORM �G���[�����o.
004400     MOVE SPACE     TO  �g������o.
004410*================================================================*
004420 �G���[�\�� SECTION.
004430*
004440     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C�����v UPON CONS.
004450     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
004460     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
004470     ACCEPT  �L�[���� FROM CONS.
004480*================================================================*
004490 �G���[�����o SECTION.
004500*
004510     IF �ʒm���o NOT = "00"
004520         DISPLAY NC"���[�G���["              UPON CONS
004530         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
004540         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
004550         DISPLAY NC"�g������o�F" �g������o UPON CONS
004560         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004570                                             UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
004580         ACCEPT  �L�[���� FROM CONS
004590         PERFORM �t�@�C����
004600         MOVE 99 TO PROGRAM-STATUS
004610         EXIT PROGRAM
004620     END-IF.
004630*================================================================*
004640 �G���[�\���q SECTION.
004650*
004660     DISPLAY NC"�t�@�C���Ǎ��G���[" �t�@�C�����v UPON CONS.
004670     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
004680     ACCEPT  �L�[���� FROM CONS.
004690     PERFORM �t�@�C����.
004700     EXIT PROGRAM.
004710*================================================================*
004720 �G���[�\�����̑� SECTION.
004730*
004740     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
004750     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004760                                                   UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
004770     ACCEPT  �L�[���� FROM CONS.
004780     PERFORM �t�@�C����.
004790     EXIT PROGRAM.
004791*================================================================*
004800*================================================================*
004810******************************************************************
004820 END PROGRAM YHN582.
004830******************************************************************
