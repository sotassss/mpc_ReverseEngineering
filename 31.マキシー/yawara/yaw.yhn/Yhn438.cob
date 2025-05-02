000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN438.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*      �W���p���i���ϗp�j   �������y����z�_+����޳�ޔ�
000100*  �����N���o�[�W����
000101*         MED = YHN438P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2015-03-04
000130 DATE-COMPILED.          2015-03-04
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
000380     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS  ���|�敪�R�[�h
000420                                                          ���|���̃R�[�h
000430                             FILE STATUS              IS  ��ԃL�[
000440                             LOCK        MODE         IS  AUTOMATIC.
000380     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS  �ہ|�ی����
000420                                                          �ہ|�ی��Ҕԍ�
000430                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000440                                                          �ہ|�ی��Җ���
000450                                                          �ہ|�ی��Ҕԍ�
000460                             FILE STATUS              IS  ��ԃL�[
000470                             LOCK        MODE         IS  AUTOMATIC.
000650     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS �{��|�{�p���ԍ�
000690                             FILE STATUS              IS  ��ԃL�[
000700                             LOCK        MODE         IS  AUTOMATIC.
000710     SELECT  ������}�X�^    ASSIGN      TO        SEIKYUSL
000720                             ORGANIZATION             IS  INDEXED
000730                             ACCESS MODE              IS  DYNAMIC
000740                             RECORD KEY               IS  ����|�ی����
000750                                                          ����|�ی��Ҕԍ�
000760                             FILE STATUS              IS  ��ԃL�[
000770                             LOCK    MODE             IS  AUTOMATIC.
000127     SELECT  ����}�X�^    ASSIGN      TO        KAIJOHOL
000128                             ORGANIZATION             IS  INDEXED
000129                             ACCESS MODE              IS  DYNAMIC
000130                             RECORD KEY               IS  ���|�_���I���敪
000131                                                          ���|����R�[�h
000132                                                          ���|�ی����
000133                                                          ���|�ύX�a��N��
000134                             ALTERNATE RECORD KEY     IS  ���|�_���I���敪
000135                                                          ���|�ڍ��t��J�i
000136                                                          ���|����R�[�h
000137                                                          ���|�ی����
000138                                                          ���|�ύX�a��N��
000139                             FILE STATUS              IS  ��ԃL�[
000140                             LOCK        MODE         IS  AUTOMATIC.
000783*
000108     SELECT  ��ƃt�@�C����  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W43121L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  �실�|�����a��N��
000112                                                          �실�|���ރR�[�h
000112                                                          �실�|���R�[�h
000112                                                          �실�|�ی���
000113                                                          �실�|�ی��Ҕԍ�
000911                                                          �실�|���҃R�[�h
000912                                                          �실�|�{�p�a��N��
000119                             FILE        STATUS       IS  ��ԃL�[
000120                             LOCK        MODE         IS  AUTOMATIC.
      */���я��p�@�V��
000108     SELECT  ��ƃt�@�C���S  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4314L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  ��S�|���ރR�[�h
000112                                                          ��S�|���R�[�h
000112                                                          ��S�|�ی���
000113                                                          ��S�|�ی��Ҕԍ�
000980                             FILE        STATUS       IS  ��ԃL�[
000990                             LOCK        MODE         IS  AUTOMATIC.
000806*
000980     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF001
000990                             SYMBOLIC    DESTINATION  IS "PRT"
001000                             FORMAT                   IS  ��`�̖��o
001010                             GROUP                    IS  ���ڌQ���o
001020                             PROCESSING  MODE         IS  ������ʂo
001030                             UNIT        CONTROL      IS  �g������o
001040                             FILE        STATUS       IS  �ʒm���o.
001050******************************************************************
001060*                      DATA DIVISION                             *
001070******************************************************************
001080 DATA                    DIVISION.
001090 FILE                    SECTION.
001100*                           �m�q�k��  �P�Q�W�n
001110 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001120     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001130*                           �m�q�k��  �Q�T�U�n
001140 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001150     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001151     COPY SEIGYO01        OF  XFDLIB  JOINING   ���O�P   AS  PREFIX.
001151     COPY SEIGYO02        OF  XFDLIB  JOINING   ���O�Q   AS  PREFIX.
001000*                           �m�q�k��  �P�Q�W�n
001010 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001020     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001160*                           �m�q�k��  �R�Q�O�n
001170 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001180     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001250*                           �m�q�k��  �P�Q�W�n
001260 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001270     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001280*                           �m�q�k��  �P�Q�W�n
001290 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001300     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
001302*                          �m�q�k��  �U�S�O�n
001303 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
001304     COPY KAIJOHO         OF  XFDLIB  JOINING   ���   AS  PREFIX.
000174*                           �m�q�k��  �P�Q�W�n
000175 FD  ��ƃt�@�C���� RECORD  CONTAINS 128 CHARACTERS.
000176 01  �실�|���R�[�h.
000177     03  �실�|���R�[�h�L�[.
000178         05  �실�|�����a��N��.
000179             07  �실�|�����a��              PIC 9.
000180             07  �실�|�����N                PIC 9(2).
000181             07  �실�|������                PIC 9(2).
001261         05  �실�|���ރR�[�h                PIC 9(1).
001261         05  �실�|���R�[�h                  PIC X(2).
001400         05  �실�|�ی���                    PIC 9(2).
000183         05  �실�|�ی��Ҕԍ�                PIC X(10).
001310         05  �실�|���҃R�[�h.
001320             07 �실�|���Ҕԍ�               PIC 9(6).
001330             07 �실�|�}��                   PIC X(1).
001340         05  �실�|�{�p�a��N��.
001350             07  �실�|�{�p�a��              PIC 9.
001360             07  �실�|�{�p�N                PIC 9(2).
001370             07  �실�|�{�p��                PIC 9(2).
000188     03  �실�|���R�[�h�f�[�^.
001400         05  �실�|�ی����                  PIC 9(2).
001280         05  �실�|�{�l�Ƒ��敪              PIC 9.
000166         05  �실�|���Ҏ���                  PIC X(50).
000192         05  �실�|�����z                    PIC 9(9).
000197         05  �실�|������                    PIC 9(4).
000201         05  FILLER                          PIC X(30).
000173*
000174*                           �m�q�k��  �R�Q�n
000175 FD  ��ƃt�@�C���S RECORD  CONTAINS 32 CHARACTERS.
000176 01  ��S�|���R�[�h.
000177     03  ��S�|���R�[�h�L�[.
001261         05  ��S�|���ރR�[�h                PIC 9(1).
001261         05  ��S�|���R�[�h                  PIC X(2).
001400         05  ��S�|�ی���                    PIC 9(2).
000183         05  ��S�|�ی��Ҕԍ�                PIC X(10).
000188     03  ��S�|���R�[�h�f�[�^.
001261         05  ��S�|�U������                  PIC 9(3).
001261         05  ��S�|�V������                  PIC 9(3).
000201         05  FILLER                          PIC X(11).
001700*
001710 FD  ����t�@�C��.
001720     COPY YHN438P        OF  XMDLIB.
001721*
001730*----------------------------------------------------------------*
001740******************************************************************
001750*                WORKING-STORAGE SECTION                         *
001760******************************************************************
001770 WORKING-STORAGE         SECTION.
001780 01 �L�[����                           PIC X     VALUE SPACE.
001790 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
001800 01 �I���t���O                         PIC X(3)  VALUE SPACE.
001810 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
001820 01 �����t���O                         PIC X(4)  VALUE SPACE.
001830 01 ��ƃt���O                         PIC X(3)  VALUE SPACE.
001840 01 ��ƈړ��L�[                       PIC X(4)  VALUE SPACE.
001850 01 �I���s�t���O                       PIC X(3)  VALUE SPACE.
001860 01 �t�@�C����                         PIC N(2)  VALUE SPACE.
001870 01 ���l�v                             PIC X(20) VALUE SPACE.
001880 01 �O�a��v                           PIC 9(1)  VALUE ZERO.
001890 01 �ی���ʂv                         PIC 9(2)  VALUE ZERO.
001900 01 �ی��Ҕԍ��v                       PIC X(10) VALUE SPACE.
001910 01 �ی��Ҕԍ��P�v                     PIC X(10) VALUE SPACE.
001920 01 �ی��Ҕԍ��Q�v                     PIC X(10) VALUE SPACE.
001921 01 ����t���O                         PIC X(3)  VALUE SPACE.
002640 01 �E�o�t���O                         PIC X(3)  VALUE SPACE.
001210 01 �I�[�v���t���O                     PIC X(3)   VALUE SPACE.
001930*
001940 01 �s�J�E���^                         PIC 9(2)  VALUE ZERO.
001950 01 �łv                               PIC 9(4)  VALUE ZERO.
001960 01 �ő�s��                           PIC 9(2)  VALUE ZERO.
001970 01 �w�b�_�s��                         PIC 9(2)  VALUE ZERO.
001980 01 �ړ��s���v                         PIC 9(2)  VALUE ZERO.
001990 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
002000 01 �ی����̂v                         PIC N(2) VALUE SPACE.
002010*
002030 01 �{�p�a��N���v.
002040     03 �{�p�a��v                     PIC 9(1)  VALUE ZERO.
002050     03 �{�p�N���v.
002060        05 �{�p�N�v                    PIC 9(2)  VALUE ZERO.
002070        05 �{�p���v                    PIC 9(2)  VALUE ZERO.
002220**
002290**************
002300* �{�p����� *
002310**************
002320 01 �{�p�����v.
002330    03 ��\�҃J�i�v                    PIC X(50)  VALUE SPACE.
002330    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
002340    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
002341    03 �_���t�ԍ��v                    PIC X(20)  VALUE SPACE.
002350    03 �{�p���Z���v.
002360       05 �{�p���Z���P�v               PIC X(50)  VALUE SPACE.
002370       05 �{�p���Z���Q�v               PIC X(50)  VALUE SPACE.
002380    03 �{�p���X�֔ԍ��v.
002400       05 �{�p���X�֔ԍ��L���v         PIC X(2)   VALUE SPACE.
002390       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
002400       05 �{�p���X�֔ԍ���؂v         PIC X(1)   VALUE SPACE.
002410       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
002420    03 �{�p���d�b�ԍ��v                PIC X(15)  VALUE SPACE.
002430    03 �������v.
002440        05 ������s���v              PIC X(40)  VALUE SPACE.
002450        05 ������s�x�X���v          PIC X(40)  VALUE SPACE.
002460        05 �a����ʂv                  PIC 9(1)   VALUE ZERO.
002470        05 ��s�ԍ��v                  PIC X(4)   VALUE SPACE.
002480        05 �X�ԍ��v                    PIC X(3)   VALUE SPACE.
002490        05 �����ԍ��v                  PIC X(10)  VALUE SPACE.
002500        05 �������`�l�J�i�v.
002500           07 �������`�l�J�i�P�v       PIC X(60)  VALUE SPACE.
002500           07 �������`�l�J�i�Q�v       PIC X(60)  VALUE SPACE.
002501        05 �������`�l�v.
002501           07 �������`�l�P�v           PIC X(60)  VALUE SPACE.
002501           07 �������`�l�Q�v           PIC X(60)  VALUE SPACE.
002510*
002520 01 �A�Ԃv                             PIC 9(3)   VALUE ZERO.
002530 01 ��s���x�X���v                     PIC X(82)  VALUE SPACE.
002540 01 �a����ʃR�����g�v                 PIC X(6)   VALUE SPACE.
002551**
002552* �Еۗp
002553 01 �ڔ���敪�v                       PIC 9     VALUE ZERO.
002554*
002560********************
002570* �ی��ҕʍ��v���z *
002580********************
002590 01 ���ׂv.
          03 �\���ڂv                        OCCURS 99.
002600       05 �{�l�ԍ��v                   PIC 9(2)  VALUE ZERO.
002610       05 ��ی��Ҏ����v               PIC X(50) VALUE SPACE.
002650       05 �{�l�����z�v                 PIC 9(7)  VALUE ZERO.
002650       05 �{�l�������v                 PIC 9(2)  VALUE ZERO.
002600       05 �Ƒ��ԍ��v                   PIC 9(2)  VALUE ZERO.
002610       05 ���Ҏ����v                   PIC X(50) VALUE SPACE.
002650       05 �Ƒ������z�v                 PIC 9(7)  VALUE ZERO.
002650       05 �Ƒ��������v                 PIC 9(2)  VALUE ZERO.
          03 �W�v�v.
002650       05 �{�l���v�����v               PIC 9(2)  VALUE ZERO.
002650       05 �{�l���v�����z�v             PIC 9(8)  VALUE ZERO.
002650       05 �{�l���v�������v             PIC 9(3)  VALUE ZERO.
002650       05 �Ƒ����v�����v               PIC 9(2)  VALUE ZERO.
002650       05 �Ƒ����v�����z�v             PIC 9(8)  VALUE ZERO.
002650       05 �Ƒ����v�������v             PIC 9(3)  VALUE ZERO.
       01 ���v�v.
002650    03 ���v�����v                      PIC 9(4)  VALUE ZERO.
002650    03 ���v�����z�v                    PIC 9(9)  VALUE ZERO.
002650    03 ���v�������v                    PIC 9(5)  VALUE ZERO.
003280 01 �{�l�J�E���^                       PIC 9(2)  VALUE ZERO.
003280 01 �Ƒ��J�E���^                       PIC 9(2)  VALUE ZERO.
002590 01 �w�b�_�v.
002660    03 �����於�̂v                    PIC X(40) VALUE SPACE.
002670    03 �x���������v                    PIC X(40) VALUE SPACE.
002680    03 �����v                          PIC X(24) VALUE SPACE.
002690    03 �ی��҈����v.
002700       05 �ی��҈����P�v               PIC X(40) VALUE SPACE.
002710       05 �ی��҈����Q�v               PIC X(40) VALUE SPACE.
002711**
002720 01 ��ʏ��S�R�O�v.
002730    03 �����N���v.
002740       05 �����a��v                   PIC 9     VALUE ZERO.
002750       05 �����N�v                     PIC 9(2)  VALUE ZERO.
002760       05 �������v                     PIC 9(2)  VALUE ZERO.
002770    03 ��o�N�����v.
002780       05 ��o�a��v                   PIC 9     VALUE ZERO.
002790       05 ��o�N�v                     PIC 9(2)  VALUE ZERO.
002800       05 ��o���v                     PIC 9(2)  VALUE ZERO.
002810       05 ��o���v                     PIC 9(2)  VALUE ZERO.
002820    03 �����ނv                      PIC 9     VALUE ZERO.
002821***
002822* �G���[���b�Z�[�W�p
002823 01 �G���[���b�Z�[�W�v.
002824    03 �G���[�ی���ʂv                PIC X(2) VALUE SPACE.
002825    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
002826    03 �G���[�ی��Ҕԍ��v              PIC X(10) VALUE SPACE.
002827    03 FILLER                          PIC X(7) VALUE SPACE.
002828*
002829***
002830 01 ����R�[�h�v                       PIC 9(2)  VALUE ZERO.
002831*
002841*
002842* ���̑���p
002844 01 ���̑��ҏW�v.
002845    03 ���̑��ҏW���e�v                PIC X(42) VALUE SPACE.
      *
001220 01 ���ރR�[�h�v�q                     PIC 9(2) VALUE ZERO.
001220 01 ���R�[�h�v�q                       PIC X(2) VALUE SPACE.
001220 01 �{���v                             PIC X(2) VALUE SPACE.
001220 01 �����v.
          03 �����v�o                        PIC X(8) VALUE SPACE.
007330    03 ���ϔԍ��v                      PIC X(28)  VALUE SPACE.
003630*
003640* ���ϔԍ��p
003650 01 ���ϘA�ԍ��W�c�v.
003660    03 ���ϘA�ԍ����v                  PIC X(14)  VALUE SPACE.
003670    03 ���ϘA�ԍ����m�v REDEFINES  ���ϘA�ԍ����v  PIC N(7).
003680    03 ���ϘA�ԍ��v                    PIC X(6)  VALUE SPACE.
003690    03 ���ϘA�ԍ��P�ʂv                PIC X(2)  VALUE SPACE.
003700    03 ���ϘA�ԍ��P�ʂm�v REDEFINES  ���ϘA�ԍ��P�ʂv  PIC N.
003710*
003720* ���q���ԍ��p
003730 01 ���q���ԍ��W�c�v.
003740    03 ���q���ԍ����v                  PIC X(8)  VALUE SPACE.
003750    03 ���q���ԍ����m�v REDEFINES  ���q���ԍ����v  PIC N(4).
003760    03 ���q���ԍ��v                    PIC X(6)  VALUE SPACE.
003770    03 ���q���ԍ��P�ʂv                PIC X(2)  VALUE SPACE.
003780    03 ���q���ԍ��P�ʂm�v REDEFINES  ���q���ԍ��P�ʂv  PIC N.
002846*
002847***
002848* ����������p�����^�p
002849*  �U�E�V���������i����敪 0:��� 1:������Ȃ��A�U���� 0:���� 1:�� 9:������Ȃ��j
002850*  ���Зp�������l1:������Ȃ�
002851***
002852 01 �������֘A�v.
002853        07 ���ۂU������敪�v          PIC 9 VALUE ZERO.
002854        07 ���ۂV������敪�v          PIC 9 VALUE ZERO.
002855        07 ���ۓ��Ј���敪�v          PIC 9 VALUE 1.
002856        07 ���ېU����敪�v            PIC 9 VALUE ZERO.
002857        07 �ЕۂU������敪�v          PIC 9 VALUE ZERO.
002858        07 �ЕۂV������敪�v          PIC 9 VALUE ZERO.
002859        07 �Еۓ��Ј���敪�v          PIC 9 VALUE 1.
002860        07 �ЕېU����敪�v            PIC 9 VALUE ZERO.
002861        07 �g���U������敪�v          PIC 9 VALUE ZERO.
002862        07 �g���V������敪�v          PIC 9 VALUE ZERO.
002863        07 �g�����Ј���敪�v          PIC 9 VALUE 1.
002864        07 �g���U����敪�v            PIC 9 VALUE ZERO.
002865        07 ���ςU������敪�v          PIC 9 VALUE ZERO.
002866        07 ���ςV������敪�v          PIC 9 VALUE ZERO.
002867        07 ���ϓ��Ј���敪�v          PIC 9 VALUE 1.
002868        07 ���ϐU����敪�v            PIC 9 VALUE ZERO.
002869        07 �V�l�U������敪�v          PIC 9 VALUE ZERO.
002870        07 �V�l�V������敪�v          PIC 9 VALUE ZERO.
002871        07 �V�l���Ј���敪�v          PIC 9 VALUE 1.
002872        07 �V�l�U����敪�v            PIC 9 VALUE ZERO.
002873        07 �����U������敪�v          PIC 9 VALUE ZERO.
002874        07 �����V������敪�v          PIC 9 VALUE ZERO.
002875        07 �������Ј���敪�v          PIC 9 VALUE 1.
002876        07 �����U����敪�v            PIC 9 VALUE ZERO.
002862        07 �U�V���я��敪�v            PIC 9 VALUE ZERO.
002877*
003280 01 �J�E���^                           PIC 9(3)  VALUE ZERO.
003290 01 �J�E���^�Q                         PIC 9(3)  VALUE ZERO.
004200 01 ���v                               PIC 9(3)  VALUE ZERO.
004210 01 �]�v                               PIC 9(3)  VALUE ZERO.
002878***********************************************************************
002879 01 �������.
002880     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
002881     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
002882     03 ������ʂo                     PIC X(2) VALUE SPACE.
002883     03 �g������o.
002890         05 �[������o.
002900             07 �ړ������o             PIC X(1).
002910             07 �ړ��s���o             PIC 9(3).
002920         05 �ڍא���o                 PIC X(2).
002930     03 �ʒm���o                     PIC X(2) VALUE SPACE.
002940     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
002950*
002960 01 �v�Z�@����N�v                     PIC 9(2).
002970* ���t�v�n�q�j
002980 01 �a��I���N�v                       PIC 9(4).
002990 01 �v�Z�@����.
003000    03 �v�Z�@����N                    PIC 9(4).
003010    03 �v�Z�@�����                  PIC 9(4).
003020 01 �v�Z�@����q REDEFINES �v�Z�@����.
003030    03 �v�Z�@���I                      PIC 9(2).
003040    03 �v�Z�@���t                      PIC 9(6).
003050    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
003060       05 �v�Z�@�N��                   PIC 9(4).
003070       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
003080         07 �v�Z�@�N                   PIC 9(2).
003090         07 �v�Z�@��                   PIC 9(2).
003100       05 �v�Z�@��                     PIC 9(2).
003110*
003120******************************************************************
003130*                          �A������                              *
003140******************************************************************
003150*
003160********************
003170* ���b�Z�[�W�\���L�[ *
003180********************
003184 01 �A���|�L�[ IS EXTERNAL.
003185    03  �A���|���b�Z�[�W               PIC N(20).
003186*
003190 01 �A���R�|�L�[ IS EXTERNAL.
003200    03  �A���R�|���b�Z�[�W             PIC N(20).
003210    03  �A���R�|���b�Z�[�W�P           PIC X(20).
003220*
003310*
003311 01 �A���|��ʏ��x�g�m�S�R�O   IS EXTERNAL.
003312    03 �A���|�����N��.
003313       05 �A���|�����a��               PIC 9.
003314       05 �A���|�����N                 PIC 9(2).
003315       05 �A���|������                 PIC 9(2).
003316    03 �A���|��o�N����.
003317       05 �A���|��o�a��               PIC 9.
003318       05 �A���|��o�N                 PIC 9(2).
003319       05 �A���|��o��                 PIC 9(2).
003320       05 �A���|��o��                 PIC 9(2).
003321    03 �A���|���Z�v�g���              PIC X(4).
003322    03 �A���|�ی����                  PIC 9(2).
003323    03 �A���|������                  PIC 9.
003324    03 �A���|�{�l�Ƒ�                  PIC 9.
003325    03 �A���|�p�����                  PIC 9.
003326    03 �A���|�������O                  PIC 9.
003327    03 �A���|���i�h�r                  PIC X(2).
003328    03 �A���|���ǂi�h�r                PIC X(2).
003338*
003348 01 �A���|��ʏ��x�g�m�S�R�O�ǉ�   IS EXTERNAL.
003358    03 �A���|�ꊇ�敪                  PIC 9.
001933    03 �A���|�쐬�����                PIC 9.
          03 �A���|�v���r���[�敪            PIC 9.
          03 �A���|����                      PIC 9(5).
          03 �A���|�������[�h                PIC X(2).
      *
       01 �A��|������x�g�m�S�R�O   IS EXTERNAL.
          03 �A��|���ރR�[�h      PIC 9(1).
          03 �A��|���R�[�h        PIC X(2).
          03 �A��|�ی���          PIC 9(2).
          03 �A��|�ی��Ҕԍ�      PIC X(10).
003020*
000540************************************
000550* �v�����^�t�@�C���쐬�p           *
000560************************************
000570 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000580     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
000590     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
000600     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
000610     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
003470*
003480******************************************************************
003490*                      PROCEDURE  DIVISION                       *
003500******************************************************************
003510 PROCEDURE               DIVISION.
003520************
003530*           *
003540* ��������   *
003550*           *
003560************
002570     PERFORM �v�����^�t�@�C���쐬.
003581     PERFORM ������.
003583************
003590*           *
003600* �又��     *
003610*           *
003620************
002484     PERFORM �������.
003760************
003770*           *
003780* �I������   *
003790*           *
003800************
003810     PERFORM �I������.
003820     EXIT PROGRAM.
003830*
003840*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002231     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
002972*
002973*   �g�p���钠�[�v���O�������Z�b�g
002974     MOVE "YHN438"              TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
003850*================================================================*
003860 ������ SECTION.
003870*
003880     PERFORM �t�@�C���I�[�v��.
003890*    /* ���ݓ��t�擾 */
003900     ACCEPT �v�Z�@���t FROM DATE.
003910*    /* 1980�`2079�N�̊ԂŐݒ� */
003920     IF �v�Z�@�N > 80
003930         MOVE 19 TO �v�Z�@���I
003940     ELSE
003950         MOVE 20 TO �v�Z�@���I
003960     END-IF.
003961*
003970     PERFORM �A�����ڑޔ�.
003990     PERFORM �J�����g�����擾.
004020*================================================================*
004030 �J�����g�����擾 SECTION.
004040*
004050     MOVE ZEROS TO ���|����敪.
004060     READ ������}�X�^
004070     NOT INVALID KEY
004080*         MOVE ���|�J�����g���� TO �J�����g�����v
004081         MOVE ���|����R�[�h   TO ����R�[�h�v
004090     END-READ.
004100*
004400*================================================================*
004410 �A�����ڑޔ� SECTION.
004420*
004430     MOVE �A���|�����a��  TO �����a��v.
004440     MOVE �A���|�����N    TO �����N�v.
004450     MOVE �A���|������    TO �������v.
004460     MOVE �A���|��o�a��  TO ��o�a��v.
004470     MOVE �A���|��o�N    TO ��o�N�v.
004480     MOVE �A���|��o��    TO ��o���v.
004490     MOVE �A���|��o��    TO ��o���v.
004500     MOVE �A���|������  TO �����ނv.
004510*
004520*================================================================*
004530 �{�p�����擾 SECTION.
004540*
004550     MOVE ZERO  TO �{��|�{�p���ԍ�.
004560     READ �{�p�����}�X�^
004570     INVALID KEY
004580         CONTINUE
004590     NOT INVALID KEY
004600*
004620         MOVE "��"                   TO �{�p���X�֔ԍ��L���v
004610         MOVE �{��|�X�֔ԍ��P       TO �{�p���X�֔ԍ��P�v
004620         MOVE "-"                    TO �{�p���X�֔ԍ���؂v
004630         MOVE �{��|�X�֔ԍ��Q       TO �{�p���X�֔ԍ��Q�v
004640         MOVE �{��|��\�҃J�i       TO ��\�҃J�i�v
004640         MOVE �{��|��\�Җ�         TO ��\�Җ��v
004650         MOVE �{��|�ڍ��@��         TO �ڍ��@���v
004660         MOVE �{��|�Z���P           TO �{�p���Z���P�v
004670         MOVE �{��|�Z���Q           TO �{�p���Z���Q�v
004700         MOVE �{��|�d�b�ԍ�         TO �{�p���d�b�ԍ��v
004701         MOVE �{��|�V�_���t�ԍ�     TO �_���t�ԍ��v
004710*
004720         MOVE �{��|������s��     TO ������s���v
004730         MOVE �{��|������s�x�X�� TO ������s�x�X���v
004740         MOVE �{��|�a�����         TO �a����ʂv
004750         MOVE �{��|��s�ԍ�         TO ��s�ԍ��v
004760         MOVE �{��|�X�ԍ�           TO �X�ԍ��v
004770         MOVE �{��|�����ԍ�         TO �����ԍ��v
004780         MOVE �{��|�������`�l�J�i   TO �������`�l�J�i�v
004781         MOVE �{��|�������`�l       TO �������`�l�v
004790         STRING ������s���v     DELIMITED BY SPACE
004800                " "                DELIMITED BY SIZE
004810                ������s�x�X���v DELIMITED BY SPACE
004820                INTO ��s���x�X���v
004830         END-STRING
004840         EVALUATE �a����ʂv
004850         WHEN 1
004860             MOVE "����"   TO �a����ʃR�����g�v
004870         WHEN 2
004880             MOVE "����"   TO �a����ʃR�����g�v
004890         WHEN OTHER
004900             MOVE SPACE    TO �a����ʃR�����g�v
004910         END-EVALUATE
023320         EVALUATE �ی���ʂv 
023330         WHEN  04
023340             PERFORM ���ϔԍ��Z�b�g
023350         WHEN  09
023360             PERFORM ���q���ԍ��Z�b�g
023370         END-EVALUATE
004920*
004930     END-READ.
004940*================================================================*
004950 �t�@�C���I�[�v�� SECTION.
004960*
004970     OPEN INPUT   �����}�X�^
004980         MOVE NC"����" TO �t�@�C����.
004990         PERFORM �I�[�v���`�F�b�N.
002650     OPEN INPUT ������}�X�^.
002660         MOVE NC"����" TO �t�@�C����.
002670         PERFORM �I�[�v���`�F�b�N.
002680     OPEN INPUT ���̃}�X�^.
002690             MOVE NC"����" TO �t�@�C����.
002700             PERFORM �I�[�v���`�F�b�N.
005030     OPEN INPUT   �ی��҃}�X�^
005040         MOVE NC"�ی�" TO �t�@�C����.
005050         PERFORM �I�[�v���`�F�b�N.
005120     OPEN INPUT   �{�p�����}�X�^
005130         MOVE NC"�{��" TO �t�@�C����.
005140         PERFORM �I�[�v���`�F�b�N.
005150     OPEN INPUT   ������}�X�^
005160         MOVE NC"����" TO �t�@�C����.
005170         PERFORM �I�[�v���`�F�b�N.
005171     OPEN INPUT   ����}�X�^
005172         MOVE NC"����" TO �t�@�C����.
005173         PERFORM �I�[�v���`�F�b�N.
005180*     OPEN I-O   ����t�@�C��
005190*         PERFORM �G���[�����o.
005200*================================================================*
005210 �I�[�v���`�F�b�N SECTION.
005220*
005230     IF ��ԃL�[  NOT =  "00"
005240         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
005250         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
005260         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005270                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
005280         ACCEPT  �L�[���� FROM CONS
005290         PERFORM �t�@�C����
005300         EXIT PROGRAM.
005310*================================================================*
005320 �t�@�C���� SECTION.
005330*
002990     IF ( �I�[�v���t���O = "YES" )
002991         CLOSE ����t�@�C��
003041     END-IF.
005340     CLOSE �����}�X�^     ������}�X�^   �ی��҃}�X�^
005350           �{�p�����}�X�^ ������}�X�^  
005360           ����}�X�^   ���̃}�X�^.
005370*================================================================*
005380 �I������ SECTION.
005390*
005400     PERFORM �t�@�C����.
005410*================================================================*
005420 �G���[�\�� SECTION.
005430*
005440     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
005450     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
005460     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
005470     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
005480     ACCEPT  �L�[���� FROM CONS.
005490     PERFORM �t�@�C����.
005500     EXIT PROGRAM.
005510*================================================================*
005520 ������� SECTION.
005530*
005570     OPEN INPUT  ��ƃt�@�C����.
005580         MOVE NC"�실" TO �t�@�C����.
005590         PERFORM �I�[�v���`�F�b�N.
005570     OPEN INPUT  ��ƃt�@�C���S.
005580         MOVE NC"��S" TO �t�@�C����.
005590         PERFORM �I�[�v���`�F�b�N.
005530*
005627     MOVE SPACE TO �I���t���O
005628     PERFORM ��ƃt�@�C�����Ǎ�
005673*
005699     PERFORM UNTIL  �I���t���O = "YES"
007300             MOVE �실�|�ی����     TO �ی���ʂv
007310             MOVE �실�|�ی��Ҕԍ�   TO �ی��Ҕԍ��v
                   INITIALIZE ���ׂv
                   INITIALIZE ���v�v
                   MOVE 1  TO �{�l�J�E���^
                   MOVE 1  TO �Ƒ��J�E���^
003889             PERFORM UNTIL ( �ی��Ҕԍ��v     NOT = �실�|�ی��Ҕԍ� ) OR
003893                           ( �I���t���O = "YES" )
005706                PERFORM ���׃Z�b�g
005710                PERFORM ��ƃt�@�C�����Ǎ�
                   END-PERFORM
                   MOVE 1     TO �J�E���^
                   MOVE 1     TO �łv
003889             PERFORM UNTIL ((��ی��Ҏ����v(�J�E���^) = SPACE) AND
                                  (���Ҏ����v(�J�E���^) = SPACE))
005703                MOVE SPACE TO YHN438P
005705                PERFORM �w�b�_�Z�b�g
                      PERFORM �t�b�^�Z�b�g
003889                PERFORM VARYING �s�J�E���^ FROM 1 BY 1
003890                        UNTIL ( �s�J�E���^ > 10 ) OR
                                    ((��ی��Ҏ����v(�J�E���^) = SPACE) AND
                                     (���Ҏ����v(�J�E���^) = SPACE))
                         MOVE �{�l�ԍ��v(�J�E���^)     TO �{�l�ԍ�(�s�J�E���^)
                         MOVE ��ی��Ҏ����v(�J�E���^) TO ��ی��Ҏ���(�s�J�E���^)
                         MOVE �{�l�����z�v(�J�E���^)   TO �{�l�����z(�s�J�E���^)
                         MOVE �{�l�������v(�J�E���^)   TO �{�l������(�s�J�E���^)
                         MOVE �Ƒ��ԍ��v(�J�E���^)     TO �Ƒ��ԍ�(�s�J�E���^)
                         MOVE ���Ҏ����v(�J�E���^)     TO ���Ҏ���(�s�J�E���^)
                         MOVE �Ƒ������z�v(�J�E���^)   TO �Ƒ������z(�s�J�E���^)
                         MOVE �Ƒ��������v(�J�E���^)   TO �Ƒ�������(�s�J�E���^)
                         COMPUTE �J�E���^ = �J�E���^ + 1
                      END-PERFORM
                      MOVE �{�l���v�����v    TO �{�l�������v
                      MOVE �{�l���v�����z�v  TO �{�l�����z���v
                      MOVE �{�l���v�������v  TO �{�l���������v
                      MOVE �Ƒ����v�����v    TO �Ƒ��������v
                      MOVE �Ƒ����v�����z�v  TO �Ƒ������z���v
                      MOVE �Ƒ����v�������v  TO �Ƒ����������v
                      IF ((��ی��Ҏ����v(�J�E���^) NOT = SPACE) OR
                          (���Ҏ����v(�J�E���^) NOT = SPACE))
                          MOVE �łv   TO ��
                          COMPUTE �łv = �łv + 1
                      ELSE
                          MOVE ���v�����v    TO ���v����
                          MOVE ���v�����z�v  TO ���v�����z
                          MOVE ���v�������v  TO ���v������
                      END-IF
005707                PERFORM �󎚏���
005708                PERFORM ���ŏ���
                      INITIALIZE ���ׂv
                   END-PERFORM
005711     END-PERFORM.
005712*
006580     CLOSE ��ƃt�@�C���� ��ƃt�@�C���S.
006581*
006660*================================================================*
006670 ��ƃt�@�C�����Ǎ� SECTION.
006680*
006690     READ ��ƃt�@�C���� NEXT
006700     AT END
006710         MOVE "YES" TO �I���t���O
006720     END-READ.
006721*================================================================*
006722 �󎚏���  SECTION.
006723*
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
013440*
006724     MOVE "YHN438P" TO  ��`�̖��o.
006725     MOVE SPACE     TO  ������ʂo.
006726     MOVE "SCREEN"  TO  ���ڌQ���o.
006727     WRITE YHN438P.
006728     PERFORM �G���[�����o.
006755*================================================================*
006756 ���ŏ���  SECTION.
006757*
006760     MOVE "YHN438P" TO  ��`�̖��o.
006770     MOVE "CT"      TO  ������ʂo.
006780     MOVE "PAGE"    TO  �g������o.
006790     MOVE SPACE     TO  ���ڌQ���o.
006800     WRITE YHN438P.
006810     PERFORM �G���[�����o.
006820     MOVE SPACE     TO  �g������o.
006830*
006840*     CLOSE  ����t�@�C��.
006850*     OPEN I-O   ����t�@�C��.
006860*     PERFORM �G���[�����o.
006870*
006930*================================================================*
006940 �w�b�_�Z�b�g SECTION.
006950*
003980     PERFORM �{�p�����擾.
006982* �����̘a����擾
006983     MOVE �����a��v         TO ���|�����敪.
006984     READ �����}�X�^
006985     INVALID KEY
006986         MOVE SPACE          TO �����a���
006987     NOT INVALID KEY
006988         MOVE ���|��������   TO �����a���
006989     END-READ.
006990*
006991     MOVE �����N�v           TO �����N.
006992     MOVE �������v           TO ������.
007001*
007100* �쐬�����擾
007110     MOVE ��o�a��v         TO ���|�����敪.
007120     READ �����}�X�^
007130     INVALID KEY
007140        MOVE SPACE          TO �쐬�a��
007150     NOT INVALID KEY
007160        MOVE ���|��������   TO �쐬�a��
007170     END-READ.
007180     MOVE ��o�N�v          TO �쐬�N.
007190     MOVE ��o���v          TO �쐬��.
007200     MOVE ��o���v          TO �쐬��.
007210*
007211     MOVE �{�p���Z���P�v     TO �Z���P.
007211     MOVE �{�p���Z���Q�v     TO �Z���Q.
007220     MOVE �{�p���X�֔ԍ��v   TO �X�֔ԍ�.
007230     MOVE ��\�҃J�i�v       TO ��\�҃J�i.
007230     MOVE ��\�Җ��v         TO ��\�Җ�.
007240     MOVE �ڍ��@���v         TO �ڍ��@��.
007241     MOVE �_���t�ԍ��v       TO �_���t�ԍ�.
           MOVE ���ϔԍ��v         TO ���ϔԍ�.
007250     MOVE �{�p���d�b�ԍ��v   TO �d�b�ԍ�.
007251*
007253     PERFORM ��s�Z�b�g.
007254*
007260     MOVE ��s���x�X���v     TO ��s���x�X��.
007270     MOVE �a����ʃR�����g�v TO �������.
007280     MOVE �����ԍ��v         TO �����ԍ�.
007281     MOVE �������`�l�J�i�v   TO ���`�J�i.
007282     MOVE �������`�l�v       TO �������`.
007285*
007292* / �ی���/
007370     PERFORM �ی��ҏ��擾.
007420     PERFORM ���������擾.
007421*
007430     MOVE �ی��҈����v  TO �ی��Җ�.
007490*
007741*================================================================*
007742 ���׃Z�b�g SECTION.
007743*
007760     IF �실�|�{�l�Ƒ��敪 = 1
007770        MOVE �{�l�J�E���^   TO �{�l�ԍ��v(�{�l�J�E���^)
007770        MOVE �실�|���Ҏ��� TO ��ی��Ҏ����v(�{�l�J�E���^)
007770        MOVE �실�|�����z   TO �{�l�����z�v(�{�l�J�E���^)
007770        MOVE �실�|������   TO �{�l�������v(�{�l�J�E���^)
              COMPUTE �{�l�J�E���^ = �{�l�J�E���^ + 1
              COMPUTE �{�l���v�����v = �{�l���v�����v + 1
              COMPUTE �{�l���v�����z�v = �{�l���v�����z�v + �실�|�����z
              COMPUTE �{�l���v�������v = �{�l���v�������v + �실�|������
007771     ELSE
007770        MOVE �Ƒ��J�E���^   TO �Ƒ��ԍ��v(�Ƒ��J�E���^)
007770        MOVE �실�|���Ҏ��� TO ���Ҏ����v(�Ƒ��J�E���^)
007770        MOVE �실�|�����z   TO �Ƒ������z�v(�Ƒ��J�E���^)
007770        MOVE �실�|������   TO �Ƒ��������v(�Ƒ��J�E���^)
              COMPUTE �Ƒ��J�E���^ = �Ƒ��J�E���^ + 1
              COMPUTE �Ƒ����v�����v = �Ƒ����v�����v + 1
              COMPUTE �Ƒ����v�����z�v = �Ƒ����v�����z�v + �실�|�����z
              COMPUTE �Ƒ����v�������v = �Ƒ����v�������v + �실�|������
007778     END-IF.
           COMPUTE ���v�����v = ���v�����v + 1.
           COMPUTE ���v�����z�v = ���v�����z�v + �실�|�����z.
           COMPUTE ���v�������v = ���v�������v + �실�|������.
007780*
006390*================================================================*
006400 �t�b�^�Z�b�g SECTION.
006410*
      */ �ی���ʁA�����A��ی��Җ�������ԂɈ�� 
           MOVE �ی��Ҕԍ��v(3:2)      TO ���|���̃R�[�h.
025960     MOVE 13                     TO ���|�敪�R�[�h.
025980     READ ���̃}�X�^
025990     INVALID KEY
026000         MOVE SPACE              TO �����v
026010     NOT INVALID KEY
026020         MOVE ���|����           TO �����v
026030     END-READ.
           STRING "["                  DELIMITED BY SIZE
                  �����v�o             DELIMITED BY "�@"
                  "]"                  DELIMITED BY SIZE
             INTO ���b�l
           END-STRING.
           STRING "���ϑg��("          DELIMITED BY SIZE
                  �ی��Ҕԍ��v         DELIMITED BY SPACE
                  ")"                  DELIMITED BY SIZE
             INTO �ی��Җ���
           END-STRING.
      *
           MOVE �실�|���ރR�[�h TO ��S�|���ރR�[�h.
           MOVE �실�|���R�[�h   TO ��S�|���R�[�h.
           MOVE �실�|�ی���     TO ��S�|�ی���.
           MOVE �ی��Ҕԍ��v     TO ��S�|�ی��Ҕԍ�.
           READ ��ƃt�@�C���S
           NOT INVALID KEY
              MOVE ��S�|�U������   TO �敪�P
              MOVE "�~"             TO ��
              MOVE ��S�|�V������   TO �敪�Q
              MOVE "*"              TO �敪�R
              MOVE "-"              TO ��؂P ��؂Q
           END-READ.
      *
008290*================================================================*
008300 �G���[�����o SECTION.
008310*
008320     IF �ʒm���o NOT = "00"
008330         DISPLAY NC"���[�G���["              UPON CONS
008340         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
008350         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
008360         DISPLAY NC"�g������o�F" �g������o UPON CONS
008370         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
008380                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008390         ACCEPT  �L�[���� FROM CONS
008400         PERFORM �t�@�C����
008410         EXIT PROGRAM
008420     END-IF.
008430*================================================================*
008440 �ی��ҏ��擾 SECTION.
008450*
008451     MOVE  SPACE         TO �����於�̂v.
008452     MOVE  SPACE         TO �x���������v.
008453     MOVE  ZERO          TO �ڔ���敪�v.
008454*
008460     MOVE �ی���ʂv     TO �ہ|�ی����.
008470     MOVE �ی��Ҕԍ��v   TO �ہ|�ی��Ҕԍ�.
008480     READ �ی��҃}�X�^
008490     INVALID KEY
008500         MOVE SPACE      TO �����於�̂v
008510         MOVE SPACE      TO �x���������v
008520     NOT INVALID KEY
008530         IF �ہ|��������敪 = 1
008540             MOVE �ہ|�ی����   TO ����|�ی����
008550             MOVE �ہ|�ی��Ҕԍ� TO ����|�ی��Ҕԍ�
008560             READ ������}�X�^
008570             INVALID KEY
008580                 MOVE SPACE             TO �����於�̂v
008590                 MOVE SPACE             TO �x���������v
008600             NOT INVALID KEY
008610                 MOVE ����|�ی��Җ���  TO �����於�̂v
008620                 MOVE ����|�x��������  TO �x���������v
008630             END-READ
008640         ELSE
008650             MOVE �ہ|�ی��Җ���        TO �����於�̂v
008660             MOVE �ہ|�x��������        TO �x���������v
008661             MOVE �ہ|�ڔ���敪        TO �ڔ���敪�v
008670         END-IF
008680     END-READ.
008950*================================================================*
008960 ���������擾 SECTION.
008970*
008971     MOVE SPACE TO �ی��҈����v.
008972     IF �����於�̂v NOT = SPACE
008980         EVALUATE �ی���ʂv
009031         WHEN 4
009032             MOVE "���ϑg��"       TO �����v
009040         WHEN OTHER
009050             MOVE SPACE            TO �����v
009060         END-EVALUATE
009070*
009080         IF �x���������v = SPACE
009090             STRING  �����於�̂v  DELIMITED BY SPACE
009100                     �����v        DELIMITED BY SPACE
009110                     "  �a"        DELIMITED BY SIZE
009120                    INTO �ی��҈����v
009130             END-STRING
009140         ELSE
009150             STRING  �����於�̂v  DELIMITED BY SPACE
009160                     �����v        DELIMITED BY SPACE
009170                     " "           DELIMITED BY SIZE
009180                     �x���������v  DELIMITED BY SPACE
009190                     "  �a"        DELIMITED BY SIZE
009200                    INTO �ی��҈����v
009210             END-STRING
009220         END-IF
009221     END-IF.
009222*
009434*================================================================*
007592 ��s�Z�b�g  SECTION.
007593*
009473* ��̐U����󎚐ݒ�̎�
009474*
009475* ������
009476      PERFORM ��s���N���A�[.
009477*
009479      MOVE ZERO          TO ���|�_���I���敪.
009478      MOVE ����R�[�h�v  TO ���|����R�[�h.
009479      MOVE ZERO          TO ���|�ی����.
009480      MOVE ZERO          TO ���|�ύX�a��N��.
009481      READ ����}�X�^
009482      NOT INVALID KEY
009483            MOVE ���|������s��      TO ������s���v
009484            MOVE ���|������s�x�X��  TO ������s�x�X���v
009485            MOVE ���|�a�����          TO �a����ʂv
009486            MOVE ���|��s�ԍ�          TO ��s�ԍ��v
009487            MOVE ���|�X�ԍ�            TO �X�ԍ��v
009488            MOVE ���|�����ԍ�          TO �����ԍ��v
009489            MOVE ���|�������`�l�J�i    TO �������`�l�J�i�v
009490            MOVE ���|�������`�l        TO �������`�l�v
009491            MOVE SPACE TO ��s���x�X���v
009492            STRING ������s���v     DELIMITED BY SPACE
009493                   " "                DELIMITED BY SIZE
009494                   ������s�x�X���v DELIMITED BY SPACE
009495                   INTO ��s���x�X���v
009496            END-STRING
009497            EVALUATE �a����ʂv
009498            WHEN 1
009499                MOVE "����"   TO �a����ʃR�����g�v
009500            WHEN 2
009501                MOVE "����"   TO �a����ʃR�����g�v
009502            WHEN OTHER
009503                MOVE SPACE    TO �a����ʃR�����g�v
009504            END-EVALUATE
009505      END-READ.
009506*
009507*================================================================*
009508 ��s���N���A�[  SECTION.
009509*
009510* ��̐U����󎚂Ȃ�
009511*
009512      MOVE SPACE TO ������s���v.
009513      MOVE SPACE TO ������s�x�X���v.
009514      MOVE ZERO  TO �a����ʂv.
009515      MOVE SPACE TO ��s�ԍ��v.
009516      MOVE SPACE TO �X�ԍ��v.
009517      MOVE SPACE TO �����ԍ��v.
009518      MOVE SPACE TO �������`�l�J�i�v.
009519      MOVE SPACE TO �������`�l�v.
009520      MOVE SPACE TO ��s���x�X���v.
009521      MOVE SPACE TO �a����ʃR�����g�v.
009522*
023970*================================================================*
023980 ���ϔԍ��Z�b�g SECTION.
023990*
024000**************************************************************
024010* �ی��Ҕԍ��ɂ��A���ς̔ԍ����󎚂��邩�A�_���t�ԍ�������
024020**************************************************************
024030** 1.���ϑg���A��
024040     MOVE SPACE  TO  �E�o�t���O.
024050     IF ( �{��|���ϘA�ԍ� NOT = ZERO )
024060** ����(�ی��Ҕԍ�)
024070        IF ( �ی��Ҕԍ��v(1:2) = "31" )  OR
024080           ( �ی��Ҕԍ��v = "34130021" )
024090*
024100           MOVE  NC"���ϑg���A����"   TO ���ϘA�ԍ����m�v 
024110           MOVE  NC"��"               TO ���ϘA�ԍ��P�ʂm�v 
024120           MOVE  �{��|���ϘA�ԍ�     TO ���ϘA�ԍ��v
024130           IF    (���ϘA�ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
024140                 MOVE SPACE TO  ���ϘA�ԍ��v(1:1)
024150           ELSE
024160                 MOVE "YES" TO  �E�o�t���O
024170           END-IF
024180           IF    (���ϘA�ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
024190                 MOVE SPACE TO  ���ϘA�ԍ��v(2:1)
024200           ELSE
024210                 MOVE "YES" TO  �E�o�t���O
024220           END-IF
024230           IF    (���ϘA�ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
024240                 MOVE SPACE TO  ���ϘA�ԍ��v(3:1)
024250           ELSE
024260                 MOVE "YES" TO  �E�o�t���O
024270           END-IF
024280           IF    (���ϘA�ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
024290                 MOVE SPACE TO  ���ϘA�ԍ��v(4:1)
024300           ELSE
024310                 MOVE "YES" TO  �E�o�t���O
024320           END-IF
024330           IF    (���ϘA�ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
024340                 MOVE SPACE TO  ���ϘA�ԍ��v(5:1)
024350           ELSE
024360                 MOVE "YES" TO  �E�o�t���O
024370           END-IF
024380           IF    (���ϘA�ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
024390                 MOVE SPACE TO  ���ϘA�ԍ��v(6:1)
024400           ELSE
024410                 MOVE "YES" TO  �E�o�t���O
024420           END-IF
024110            MOVE  ���ϘA�ԍ��W�c�v     TO ���ϔԍ��v
024440        END-IF
024450     END-IF.
024460*
027620** 2. �n���ϋ��c��
027630     MOVE SPACE  TO  �E�o�t���O.
027640     IF �{��|�n���ϘA�ԍ� NOT = ZERO
027650** ����(�ی��Ҕԍ�)
027660        IF ( �ی��Ҕԍ��v(1:2) = "32" OR "33" OR "34" )  AND
027670           ( �ی��Ҕԍ��v NOT = "34130021" )
027680*
027690           MOVE  NC"�n���ϋ��c��"     TO ���ϘA�ԍ����m�v 
027700           MOVE  NC"��"               TO ���ϘA�ԍ��P�ʂm�v 
027710           MOVE  �{��|�n���ϘA�ԍ�   TO ���ϘA�ԍ��v
027720           IF    (���ϘA�ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
027730                 MOVE SPACE TO  ���ϘA�ԍ��v(1:1)
027740           ELSE
027750                 MOVE "YES" TO  �E�o�t���O
027760           END-IF
027770           IF    (���ϘA�ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
027780                 MOVE SPACE TO  ���ϘA�ԍ��v(2:1)
027790           ELSE
027800                 MOVE "YES" TO  �E�o�t���O
027810           END-IF
027820           IF    (���ϘA�ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
027830                 MOVE SPACE TO  ���ϘA�ԍ��v(3:1)
027840           ELSE
027850                 MOVE "YES" TO  �E�o�t���O
027860           END-IF
027870           IF    (���ϘA�ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
027880                 MOVE SPACE TO  ���ϘA�ԍ��v(4:1)
027890           ELSE
027900                 MOVE "YES" TO  �E�o�t���O
027910           END-IF
027920           IF    (���ϘA�ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
027930                 MOVE SPACE TO  ���ϘA�ԍ��v(5:1)
027940           ELSE
027950                 MOVE "YES" TO  �E�o�t���O
027960           END-IF
027970           IF    (���ϘA�ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
027980                 MOVE SPACE TO  ���ϘA�ԍ��v(6:1)
027990           ELSE
028000                 MOVE "YES" TO  �E�o�t���O
028010           END-IF
024110            MOVE  ���ϘA�ԍ��W�c�v     TO ���ϔԍ��v
028050        END-IF
028060     END-IF.
024900*
024910*================================================================*
024920 ���q���ԍ��Z�b�g SECTION.
024930*
028110     MOVE SPACE  TO  �E�o�t���O.
028120     IF �{��|���q���ԍ� NOT = ZERO
028130           IF �{��|�h�q�ȋ敪 = 1
028140              MOVE  NC"�h�q�ȑ�"      TO ���q���ԍ����m�v 
028150           ELSE
028160              MOVE  NC"�h�q����"      TO ���q���ԍ����m�v 
028170           END-IF
028180           MOVE  NC"��"               TO ���q���ԍ��P�ʂm�v 
028190           MOVE  �{��|���q���ԍ�     TO ���q���ԍ��v
028200           IF    (���q���ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
028210                 MOVE SPACE TO  ���q���ԍ��v(1:1)
028220           ELSE
028230                 MOVE "YES" TO  �E�o�t���O
028240           END-IF
028250           IF    (���q���ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
028260                 MOVE SPACE TO  ���q���ԍ��v(2:1)
028270           ELSE
028280                 MOVE "YES" TO  �E�o�t���O
028290           END-IF
028300           IF    (���q���ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
028310                 MOVE SPACE TO  ���q���ԍ��v(3:1)
028320           ELSE
028330                 MOVE "YES" TO  �E�o�t���O
028340           END-IF
028350           IF    (���q���ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
028360                 MOVE SPACE TO  ���q���ԍ��v(4:1)
028370           ELSE
028380                 MOVE "YES" TO  �E�o�t���O
028390           END-IF
028400           IF    (���q���ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
028410                 MOVE SPACE TO  ���q���ԍ��v(5:1)
028420           ELSE
028430                 MOVE "YES" TO  �E�o�t���O
028440           END-IF
028450           IF    (���q���ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
028460                 MOVE SPACE TO  ���q���ԍ��v(6:1)
028470           ELSE
028480                 MOVE "YES" TO  �E�o�t���O
028490           END-IF
028500         MOVE  ���q���ԍ��W�c�v     TO ���ϔԍ��v
028510     END-IF.
025310*
009605*================================================================*
009606******************************************************************
009607 END PROGRAM YHN438.
009608******************************************************************
