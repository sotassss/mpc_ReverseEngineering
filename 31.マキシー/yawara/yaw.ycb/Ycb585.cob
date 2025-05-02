000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCB585.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*       �ی���Ô���\ �y����z
000100*       MED = YCB580 YCB585P 
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2012-11-14
000130 DATE-COMPILED.          2012-11-14
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
000902     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5841L.DAT"
000903                             ORGANIZATION             IS  INDEXED
000904                             ACCESS                   IS  DYNAMIC
000905                             RECORD      KEY          IS  ��P�|���ރR�[�h
                                                                ��P�|�ی���ʏ���
000908                                                          ��P�|�ی��Ҕԍ�
000909                                                          ��P�|���ʋ敪
000909                                                          ��P�|�{�l�Ƒ��敪
000908                                                          ��P�|��ی��҃J�i
000910                                                          ��P�|���҃J�i
000911                                                          ��P�|���҃R�[�h
000912                                                          ��P�|�{�p�a��N��
000551                             FILE        STATUS       IS  ��ԃL�[
000552                             LOCK        MODE         IS  AUTOMATIC.
000553*
001080* ���Z���я��p
001081     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001100                             ORGANIZATION             IS  INDEXED
001110                             ACCESS                   IS  DYNAMIC
001120                             RECORD      KEY          IS  ��Q�|�{�p�a��N��
001130                                                          ��Q�|���҃R�[�h
001140                                                          ��Q�|�ی����
001150                             FILE        STATUS       IS  ��ԃL�[
001160                             LOCK        MODE         IS  AUTOMATIC.
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
000971*
001190 FD  ��ƃt�@�C���P RECORD  CONTAINS 160 CHARACTERS.
001200 01  ��P�|���R�[�h.
001210     03  ��P�|���R�[�h�L�[.
001261         05  ��P�|���ރR�[�h              PIC 9(2).
               05  ��P�|�ی���ʏ���            PIC 9(2).
001271         05  ��P�|�ی��Ҕԍ�              PIC X(8).
001280         05  ��P�|���ʋ敪                PIC 9.
001280         05  ��P�|�{�l�Ƒ��敪            PIC 9.
001260         05  ��P�|��ی��҃J�i            PIC X(50).
001300         05  ��P�|���҃J�i                PIC X(50).
001310         05  ��P�|���҃R�[�h.
001320             07 ��P�|���Ҕԍ�             PIC 9(6).
001330             07 ��P�|�}��                 PIC X(1).
001340         05  ��P�|�{�p�a��N��.
001350             07  ��P�|�{�p�a��            PIC 9.
001360             07  ��P�|�{�p�N              PIC 9(2).
001370             07  ��P�|�{�p��              PIC 9(2).
001380     03  ��P�|���R�[�h�f�[�^.
001400         05  ��P�|�ی����                PIC 9(2).
               05  ��P�|��p�z                  PIC 9(6).
               05  ��P�|�����z                  PIC 9(6).
001260         05  ��P�|���x��敪              PIC 9.
001598         05  FILLER                        PIC X(19).
001730*
001740 FD  ��ƃt�@�C���Q RECORD  CONTAINS 32 CHARACTERS.
001750 01  ��Q�|���R�[�h.
001760     03  ��Q�|���R�[�h�L�[.
001770         05  ��Q�|�{�p�a��N��.
001780             07  ��Q�|�{�p�a��            PIC 9.
001790             07  ��Q�|�{�p�N              PIC 9(2).
001800             07  ��Q�|�{�p��              PIC 9(2).
001810         05  ��Q�|���҃R�[�h.
001820             07 ��Q�|���Ҕԍ�             PIC 9(6).
001830             07 ��Q�|�}��                 PIC X(1).
001840         05  ��Q�|�ی����                PIC 9(2).
001850     03  ��Q�|���R�[�h�f�[�^.
001860         05  ��Q�|����                    PIC 9(4).
001870         05  FILLER                        PIC X(14).
000994*
000995 FD  ����t�@�C��.
001000     COPY YCB585P        OF  XMDLIB.
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
001170 01 ���s�L�[�v                         PIC X(4) VALUE SPACE.
001210 01 �I�[�v���t���O                     PIC X(3)   VALUE SPACE.
001180 01 �O�a��v                           PIC 9 VALUE ZERO.
001190 01 �s���t���O                         PIC X(3) VALUE SPACE.
001200 01 �����ړ��L�[                       PIC X(4) VALUE SPACE.
001210 01 �{�l�Ƒ��v                         PIC 9(1) VALUE ZERO.
001220 01 �Ԗ߂v                             PIC 9(1) VALUE ZERO.
001220 01 ���ރR�[�h�v�q                     PIC 9(2) VALUE ZERO.
001200 01 �ی��Ҕԍ��v�q                     PIC X(8) VALUE SPACE.
       01 �ی���ʏ����v�q                   PIC 9(2) VALUE ZERO.
       01 �ی���ʂv�q                       PIC 9(2) VALUE ZERO.
001220 01 �ی���ʂv                         PIC 9(2) VALUE ZERO.
001230* 01 �ԍ��v.
001231*    03 ���ރR�[�h�v                    PIC 9 VALUE ZERO.
001232*    03 FILLER                          PIC X VALUE "-".
001233*    03 ���Ԃv                          PIC 9(3) VALUE ZERO.
002230 01 ���Ԃv                             PIC 9(4) VALUE ZERO.
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
      *
       01 ���v�v.
          03 ���v�����v                      PIC 9(4) VALUE ZERO.
          03 ���v��p�z�v                    PIC 9(9) VALUE ZERO.
          03 ���v�����z�v                    PIC 9(9) VALUE ZERO.
      *
       01 �����v�v.
          03 �����v�����v                    PIC 9(4) VALUE ZERO.
          03 �����v��p�z�v                  PIC 9(9) VALUE ZERO.
          03 �����v�����z�v                  PIC 9(9) VALUE ZERO.
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
      *
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
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
002080*
002150****************
002160* ��ʓ��͏�� *
002170****************
002160 01 �A���|��ʏ��x�b�a�T�W�O IS EXTERNAL.
002170    03 �A���|�{�p�a��N��.
002180       05 �A���|�{�p�a��                PIC 9.
002190       05 �A���|�{�p�N��.
002200         07 �A���|�{�p�N                PIC 9(2).
002210         07 �A���|�{�p��                PIC 9(2).
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
002231*
      * �Í������p
       01 �A�Í������|�Í���� IS EXTERNAL.
          03 �A�Í������|���͏��.
             05 �A�Í������|�L��               PIC X(24).
             05 �A�Í������|�ԍ�               PIC X(30).
             05 �A�Í������|�Í�������.
               07 �A�Í������|�Í����Ҕԍ�     PIC X(6).
               07 �A�Í������|�Í�����L��     PIC X.
               07 �A�Í������|�Í�����ԍ�     PIC X.
               07 �A�Í������|�Í��L��         PIC X(24).
               07 �A�Í������|�Í��ԍ�         PIC X(30).
          03 �A�Í������|�o�͏��.
             05 �A�Í������|���������L��       PIC X(24).
             05 �A�Í������|���������ԍ�       PIC X(30).
      * 
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
002810     PERFORM ���я��t�@�C���쐬.
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
      *   �󎚒����L��
002971     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
002972*
002973*   �g�p���钠�[�v���O�������Z�b�g
002974     MOVE "YCB585"              TO �g�A�o�q�s�e�|���[�v���O������.
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
002577*
002580     OPEN INPUT ��ƃt�@�C���P.
002590         MOVE NC"��P" TO �t�@�C�����v.
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
003590     ELSE
003600         MOVE  NC"�@�@�f�[�^���O���ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
003610         CALL   "MSG001"
003620         CANCEL "MSG001"
003630     END-IF.
003640*
002790     CLOSE  ��f�ҏ��e �����}�X�^  �ی��҃}�X�^   �{�p�����}�X�^
                  ��ƃt�@�C���P  �s�����}�X�^.
002800*================================================================*
002810 �I������ SECTION.
002820*
002830     PERFORM �t�@�C����.
002840*================================================================*
002850 ������� SECTION.
002860*
002870     MOVE 55    TO �ő�s��.
002880*
002890     MOVE ZERO  TO �s�J�E���^.
002900     MOVE ZERO  TO �ŃJ�E���^.
002910     MOVE SPACE TO �I���t���O.
002930*
002970     MOVE ZERO  TO ��P�|���ރR�[�h.
      */���یn(���ہA�ސE�A���ۑg��)�A�V�l�n(���ۘV�l�A�ЕۘV�l)�����ł��Ȃ��ő����Ɉ������/0801
           MOVE ZERO  TO ��P�|�ی���ʏ���
002980     MOVE SPACE TO ��P�|�ی��Ҕԍ�.
002980     MOVE SPACE TO ��P�|��ی��҃J�i.
002970     MOVE ZERO  TO ��P�|�{�l�Ƒ��敪.
002970     MOVE ZERO  TO ��P�|���ʋ敪.
002980     MOVE SPACE TO ��P�|���҃J�i.
002970     MOVE ZERO  TO ��P�|���Ҕԍ�.
002980     MOVE SPACE TO ��P�|�}��.
002980     MOVE SPACE TO ��P�|�{�p�a��N��.
003020*
003030     START ��ƃt�@�C���P KEY IS > ��P�|���ރR�[�h
      */���یn(���ہA�ސE�A���ۑg��)�A�V�l�n(���ۘV�l�A�ЕۘV�l)�����ł��Ȃ��ő����Ɉ������/0801
                                         ��P�|�ی���ʏ���
002980                                   ��P�|�ی��Ҕԍ�
002970                                   ��P�|���ʋ敪
002970                                   ��P�|�{�l�Ƒ��敪
002980                                   ��P�|��ی��҃J�i
002980                                   ��P�|���҃J�i
002970                                   ��P�|���҃R�[�h
002980                                   ��P�|�{�p�a��N��
003110     END-START.
003120     IF ��ԃL�[  =  "00"
003130         PERFORM ��ƃt�@�C���P�Ǎ�
003140         MOVE ��P�|���ރR�[�h  TO ���ރR�[�h�v�q
003140         MOVE ��P�|�ی��Ҕԍ�  TO �ی��Ҕԍ��v�q
      */���یn(���ہA�ސE�A���ۑg��)�A�V�l�n(���ۘV�l�A�ЕۘV�l)�����ł��Ȃ��ő����Ɉ������/0801
               MOVE ��P�|�ی���ʏ��� TO �ی���ʏ����v�q
               MOVE ��P�|�ی����     TO �ی���ʂv�q
003160         COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
003170         PERFORM �w�b�_���R�[�h�Z�b�g
003180         PERFORM �w�b�_�������
003190         PERFORM UNTIL �I���t���O = "YES"
003190             PERFORM UNTIL ( �I���t���O = "YES" ) OR
                                 ( ��P�|���ރR�[�h NOT = ���ރR�[�h�v�q )
003190                 PERFORM UNTIL ( �I���t���O = "YES" ) OR
                                     ( ��P�|���ރR�[�h NOT = ���ރR�[�h�v�q ) OR
                                     ( ��P�|�ی��Ҕԍ� NOT = �ی��Ҕԍ��v�q )
      */���یn(���ہA�ސE�A���ۑg��)�A�V�l�n(���ۘV�l�A�ЕۘV�l)�����ł��Ȃ��ő����Ɉ������/0801
                                  OR (��P�|�ی���ʏ��� NOT = �ی���ʏ����v�q)
003200                     IF ( �s�J�E���^ >= �ő�s��   )
003210                         PERFORM ���ŏ���
003220                         COMPUTE �ŃJ�E���^ = �ŃJ�E���^ + 1
003230                         PERFORM �w�b�_���R�[�h�Z�b�g
003240                         PERFORM �w�b�_�������
003251                         MOVE ZERO    TO �s�J�E���^
003260                     ELSE
003320                         PERFORM ���׃��R�[�h�Z�b�g
003330                         PERFORM ���׈������
                               PERFORM ���v�W�v����
                               PERFORM �����v�W�v����
003140                         MOVE ��P�|���ރR�[�h  TO ���ރR�[�h�v�q
003140                         MOVE ��P�|�ی��Ҕԍ�  TO �ی��Ҕԍ��v�q
      */���یn(���ہA�ސE�A���ۑg��)�A�V�l�n(���ۘV�l�A�ЕۘV�l)�����ł��Ȃ��ő����Ɉ������/0801
                               MOVE ��P�|�ی���ʏ��� TO �ی���ʏ����v�q
                               MOVE ��P�|�ی����     TO �ی���ʂv�q
003360                         PERFORM ��ƃt�@�C���P�Ǎ�
003370                     END-IF
                       END-PERFORM
                       IF ( ��P�|�ی��Ҕԍ� NOT = �ی��Ҕԍ��v�q )
      */���یn(���ہA�ސE�A���ۑg��)�A�V�l�n(���ۘV�l�A�ЕۘV�l)�����ł��Ȃ��ő����Ɉ������/0801
                       OR ((�ی���ʂv�q = 05 ) AND (��P�|�ی���ʏ��� NOT = �ی���ʏ����v�q))
                           PERFORM ���v�Z�b�g
                           PERFORM ���v�������
                           MOVE ZERO   TO ���v�v
                           MOVE ��P�|�ی��Ҕԍ�  TO �ی��Ҕԍ��v�q
      */���یn(���ہA�ސE�A���ۑg��)�A�V�l�n(���ۘV�l�A�ЕۘV�l)�����ł��Ȃ��ő����Ɉ������/0801
                           MOVE ��P�|�ی���ʏ��� TO �ی���ʏ����v�q
                           MOVE ��P�|�ی����     TO �ی���ʂv�q
                       END-IF
                   END-PERFORM
                   IF ( ��P�|���ރR�[�h NOT = ���ރR�[�h�v�q )
                       IF ( ���ރR�[�h�v�q   >= 10 ) AND
                          ( ��P�|���ރR�[�h <= 15) 
                           MOVE ��P�|���ރR�[�h  TO ���ރR�[�h�v�q
                       ELSE
                           PERFORM �����v�Z�b�g
                           PERFORM �����v�������
                           MOVE ZERO   TO �����v�v
                           PERFORM ���ŏ���
                           MOVE ��P�|���ރR�[�h  TO ���ރR�[�h�v�q
                           MOVE 1       TO �ŃJ�E���^
                           IF ( �I���t���O = SPACE )
                               PERFORM �w�b�_���R�[�h�Z�b�g
                               PERFORM �w�b�_�������
                               MOVE ZERO    TO �s�J�E���^
                           END-IF
                       END-IF
                   END-IF
003380         END-PERFORM
               PERFORM ���v�Z�b�g
               PERFORM ���v�������
               PERFORM �����v�Z�b�g
               PERFORM �����v�������
003390         PERFORM ���ŏ���
003400     END-IF.
003401*
003410*================================================================*
003420 ��ƃt�@�C���P�Ǎ� SECTION.
003430*
003440     READ ��ƃt�@�C���P NEXT
003450     AT END
003460         MOVE "YES" TO �I���t���O
003470     END-READ.
003471*
003480*================================================================*
003490 �w�b�_���R�[�h�Z�b�g SECTION.
003500*
003510     MOVE SPACE TO YCB585P.
003520     INITIALIZE YCB585P.
003530     MOVE �ŃJ�E���^        TO ��.
003546*
003550     MOVE �A���|�{�p�a��    TO ���|�����敪.
003560     READ �����}�X�^
003570     INVALID KEY
003580         MOVE SPACE         TO �����a��
003590     NOT INVALID KEY
003600         MOVE ���|��������  TO �����a��
003610     END-READ.
003620     MOVE �A���|�{�p�N      TO �����N.
003630     MOVE �A���|�{�p��      TO ������.
           PERFORM �{�p�����擾.
           MOVE �ڍ��t�����ԍ��v  TO ����ԍ�.
           MOVE �ڍ��@���v        TO �ڍ��@��.
           MOVE ��\�Җ��v        TO ��\�Җ�.
003858     EVALUATE ��P�|���ރR�[�h
003862     WHEN 01
003861         MOVE "�y �� �� �� �N �� �� �z"      TO ���
           WHEN 02
003875         MOVE "�y �� �E �� �� �� �z"         TO ���
003866     WHEN 03
003867         MOVE "�y �� �� �g �� �z"            TO ���
           WHEN 04
      */���یn(���ہA�ސE�A���ۑg��)�A�V�l�n(���ۘV�l�A�ЕۘV�l)�����ł��Ȃ��ő����Ɉ������/0801
003869*         MOVE "�y �V �l �� �� ( �� �� ) �z"  TO ���
003869         MOVE "�y �V �l �� ��  �z"           TO ���
003868     WHEN 05
003869         MOVE "�y �V �l �� �� ( �� �� ) �z"  TO ���
003866     WHEN 06
003867         MOVE "�y �� �� �� �� �z"            TO ���
003860     WHEN 07
      */�����ۂɕύX/081021
003863*         MOVE "�y �� �� �� �� �� �� �� �z"   TO ���
003863         MOVE "�y �� �� �� �� �� �z"   TO ���
003878     WHEN 08
003865         MOVE "�y �� �N �� �� �g �� �z"      TO ���
003866     WHEN 09
003867         MOVE "�y �� �� �g �� �z"            TO ���
003878     WHEN 10
003879*         MOVE "�y �V �l �� �� �z"            TO ���
003880     WHEN 11
003881*         MOVE "�y �� �q �� �� �z"            TO ���
003882     WHEN 12
003883*         MOVE "�y �� �Q �� �� �z"            TO ���
003884     WHEN 13
003885*         MOVE "�y �� �� �� �� �� �z"         TO ���
003886     WHEN 14
003886     WHEN 15
003887*         MOVE "�y �� �c �� �� �z"            TO ���
003887         MOVE "�y �� �� �� �� �z"            TO ���
003866     WHEN 20
003867         MOVE "�y �� �� �� �s �� �� �z"       TO ���
003890     END-EVALUATE.
003631*
003640*================================================================*
003650 ���׃��R�[�h�Z�b�g SECTION.
003660*
003675     MOVE SPACE TO YCB585P.
003680     INITIALIZE YCB585P.
003711*
003782     MOVE ��P�|�ی��Ҕԍ�     TO �ی��Ҕԍ�.
003790     PERFORM ��������擾.
003800     MOVE �����於�̂v         TO �ی��Җ���.
003811*
003813     MOVE ��P�|�{�p�a��       TO ��|�{�p�a��.
003814     MOVE ��P�|�{�p�N         TO ��|�{�p�N.
003815     MOVE ��P�|�{�p��         TO ��|�{�p��.
003816     MOVE ��P�|���҃R�[�h     TO ��|���҃R�[�h.
003817     READ ��f�ҏ��e
003821     NOT INVALID KEY
003845         MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ���
003845         MOVE ��|���Ҏ���     TO ���Ҏ���
003845*         MOVE ��|�L��         TO �L��
003845*         MOVE ��|�ԍ�         TO �ԍ�
      *                                          
               MOVE SPACE TO �A�Í������|�Í����
      *
      *    / �A�Í������|���͏��Z�b�g /
               MOVE ��|�L��       TO �A�Í������|�L��
               MOVE ��|�ԍ�       TO �A�Í������|�ԍ�
               MOVE ��|�Í������� TO �A�Í������|�Í�������
      *     
               CALL   �����v���O�������v
               CANCEL �����v���O�������v
      *
               MOVE �A�Í������|���������L�� TO �L��
               MOVE �A�Í������|���������ԍ� TO �ԍ�
      *
003856     END-READ.
003893*
           MOVE ��P�|��p�z         TO ��p�z.
           MOVE ��P�|�����z         TO �������z.
      *
           IF ��P�|���x��敪 = 1
               MOVE NC"���x��"       TO ���x��.
003899*
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
022590         MOVE �{��|�V�_���t�ԍ�  TO �_���t�ԍ��v
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
003920     IF ( ��P�|�ی���� NOT = 05 ) AND ( ��P�|�ی���� < 10 )
003930*    / ���� /
003960        MOVE ��P�|�ی����      TO �ہ|�ی����
003970        MOVE ��P�|�ی��Ҕԍ�    TO �ہ|�ی��Ҕԍ�
003980        MOVE SPACE               TO �����於�̂v
003990        READ �ی��҃}�X�^
004000        INVALID KEY
004010            MOVE SPACE           TO �����於�̂v
004020        NOT INVALID KEY
004030            MOVE �ہ|�ی��Җ���  TO �����於�̂v
004040        END-READ
004041     ELSE
004042*    / �V�l�E���� /
004045        MOVE ��P�|�ی����      TO �s�|������
004046        MOVE ��P�|�ی��Ҕԍ�    TO �s�|�s�����ԍ�
004048        READ �s�����}�X�^
004049        INVALID KEY
004050            MOVE SPACE           TO �����於�̂v
004051        NOT INVALID KEY
004062            MOVE �s�|�s��������  TO �����於�̂v
004064        END-READ
004065     END-IF.
004066*
004130*================================================================*
004140 ���v�W�v���� SECTION.
004150*
           COMPUTE ���v�����v   = ���v�����v + 1.
           COMPUTE ���v��p�z�v = ���v��p�z�v + ��P�|��p�z.
           COMPUTE ���v�����z�v = ���v�����z�v + ��P�|�����z.
      *
004130*================================================================*
004140 �����v�W�v���� SECTION.
004150*
           COMPUTE �����v�����v   = �����v�����v + 1.
           COMPUTE �����v��p�z�v = �����v��p�z�v + ��P�|��p�z.
           COMPUTE �����v�����z�v = �����v�����z�v + ��P�|�����z.
      *
004130*================================================================*
004140 ���v�Z�b�g SECTION.
004150*
           MOVE NC"���v"     TO ���v.
           MOVE ���v�����v   TO ���v����.
           MOVE ���v��p�z�v TO ���v��p�z.
           MOVE ���v�����z�v TO ���v�������z.
      *
004130*================================================================*
004140 �����v�Z�b�g SECTION.
004150*
           MOVE NC"�����v"     TO �����v.
           MOVE �����v�����v   TO �����v����.
           MOVE �����v��p�z�v TO �����v��p�z.
           MOVE �����v�����z�v TO �����v�������z.
      *
004068*================================================================*
004069 �w�b�_������� SECTION.
004070*
006050     IF ( �I�[�v���t���O NOT = "YES" )
006060        MOVE "YES" TO �I�[�v���t���O
006070        OPEN I-O  ����t�@�C��
006080        PERFORM �G���[�����o
006090     END-IF.
004080     MOVE "YCB585P"  TO  ��`�̖��o.
004090     MOVE SPACE      TO  ������ʂo.
004100     MOVE "HEAD01"   TO  ���ڌQ���o.
004110     WRITE YCB585P.
004120     PERFORM �G���[�����o.
004130*================================================================*
004140 ���׈������ SECTION.
004150*
004160     MOVE "YCB585P"  TO  ��`�̖��o.
004170     MOVE "GRP001"   TO  ���ڌQ���o.
004180     WRITE YCB585P.
004190     PERFORM �G���[�����o.
004200     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
004130*================================================================*
004140 ���v������� SECTION.
004150*
004160     MOVE "YCB585P"  TO  ��`�̖��o.
004170     MOVE "GRP002"   TO  ���ڌQ���o.
004180     WRITE YCB585P.
004190     PERFORM �G���[�����o.
004200     COMPUTE �s�J�E���^ = �s�J�E���^ + 1.
004130*================================================================*
004140 �����v������� SECTION.
004150*
004160     MOVE "YCB585P"  TO  ��`�̖��o.
004170     MOVE "GRP003"   TO  ���ڌQ���o.
004180     WRITE YCB585P.
004190     PERFORM �G���[�����o.
004290*================================================================*
004300 ���ŏ���  SECTION.
004310*
004320     MOVE SPACE TO YCB585P.
004330     INITIALIZE YCB585P.
004340     MOVE "YCB585P" TO  ��`�̖��o.
004350     MOVE "CT"      TO  ������ʂo.
004360     MOVE "PAGE"    TO  �g������o.
004370     MOVE SPACE     TO  ���ڌQ���o.
004380     WRITE YCB585P.
004390     PERFORM �G���[�����o.
004400     MOVE SPACE     TO  �g������o.
004410*================================================================*
004420 �G���[�\�� SECTION.
004430*
004440     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C�����v UPON CONS.
004450     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
004460     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
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
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
004680     ACCEPT  �L�[���� FROM CONS.
004690     PERFORM �t�@�C����.
004700     EXIT PROGRAM.
004710*================================================================*
004720 �G���[�\�����̑� SECTION.
004730*
004740     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
004750     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004760                                                   UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
004770     ACCEPT  �L�[���� FROM CONS.
004780     PERFORM �t�@�C����.
004790     EXIT PROGRAM.
004791*================================================================*
007380 ���я��t�@�C���쐬 SECTION.
007390*
007400     CLOSE ��ƃt�@�C���P.
007410     OPEN INPUT ��ƃt�@�C���P.
007420         MOVE NC"��P" TO �t�@�C�����v.
007430         PERFORM �I�[�v���`�F�b�N.
007440     OPEN OUTPUT ��ƃt�@�C���Q.
007450         MOVE NC"��Q" TO �t�@�C�����v.
007460         PERFORM �I�[�v���`�F�b�N.
007470*
007480     MOVE SPACE TO �I���t���O.
007490     MOVE ZERO  TO ���Ԃv.      
007500     PERFORM ��ƃt�@�C���P�Ǎ�.
007510     PERFORM UNTIL �I���t���O = "YES"
007520             PERFORM ��Q���R�[�h�Z�b�g
007530             PERFORM ��Q�t�@�C������
007540             PERFORM ��ƃt�@�C���P�Ǎ�
007550     END-PERFORM.
           CLOSE ��ƃt�@�C���Q.
007560
007570*================================================================*
007580 ��Q���R�[�h�Z�b�g SECTION.
007590*
007600     MOVE ��P�|�{�p�a��       TO ��Q�|�{�p�a��.
007610     MOVE ��P�|�{�p�N         TO ��Q�|�{�p�N.
007620     MOVE ��P�|�{�p��         TO ��Q�|�{�p��.
007630     MOVE ��P�|���҃R�[�h     TO ��Q�|���҃R�[�h.
007640     MOVE ��P�|�ی����       TO ��Q�|�ی����.
007650     COMPUTE ���Ԃv  = ���Ԃv + 1.
007660     MOVE ���Ԃv               TO ��Q�|����.
007670*
007680*================================================================*
007690 ��Q�t�@�C������ SECTION.
007700*
007710     WRITE ��Q�|���R�[�h
007720     INVALID KEY
007730         MOVE NC"��Q"  TO �t�@�C�����v
007740         PERFORM �G���[�\��
007750     END-WRITE.
007760*================================================================*
004810******************************************************************
004820 END PROGRAM YCB585.
004830******************************************************************
