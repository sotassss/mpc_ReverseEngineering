000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN437.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*      �V���p��   �������y����z�_+����޳�ޔ�
000100*  �����N���o�[�W����
000101*         MED = YHN437P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2015-03-02
000130 DATE-COMPILED.          2015-03-02
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
000480     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000490                             ORGANIZATION             IS  INDEXED
000500                             ACCESS MODE              IS  DYNAMIC
000510                             RECORD KEY               IS  �s�|������
000520                                                          �s�|�s�����ԍ�
000530                             ALTERNATE RECORD KEY     IS  �s�|������
000540                                                          �s�|�s��������
000550                                                          �s�|�s�����ԍ�
000560                             FILE STATUS              IS  ��ԃL�[
000570                             LOCK        MODE         IS  AUTOMATIC.
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
000793     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4312L.DAT"
000794                             ORGANIZATION             IS  INDEXED
000795                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  ��Q�|�����a��N��
000112                                                          ��Q�|���ރR�[�h
000112                                                          ��Q�|���R�[�h
000112                                                          ��Q�|�ی���
000113                                                          ��Q�|�ی��Ҕԍ�
000804                             FILE        STATUS       IS  ��ԃL�[
000805                             LOCK        MODE         IS  AUTOMATIC.
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
000816     SELECT  �ی��Ҋg���}�X�^ ASSIGN     TO        HOKENEXL
000826                             ORGANIZATION             IS  INDEXED
000836                             ACCESS MODE              IS  DYNAMIC
000846                             RECORD KEY               IS  �ۊg�|�ی����
000856                                                          �ۊg�|�ی��Ҕԍ�
000866                             FILE STATUS              IS  ��ԃL�[
000876                             LOCK        MODE         IS  AUTOMATIC.
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
001190*                           �m�q�k��  �Q�T�U�n
001200 FD  �s�����}�X�^        BLOCK   CONTAINS   1   RECORDS.
001210     COPY SITYOSN         OF  XFDLIB  JOINING   �s   AS  PREFIX.
001250*                           �m�q�k��  �P�Q�W�n
001260 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001270     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001280*                           �m�q�k��  �P�Q�W�n
001290 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001300     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
001302*                          �m�q�k��  �U�S�O�n
001303 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
001304     COPY KAIJOHO         OF  XFDLIB  JOINING   ���   AS  PREFIX.
001305*                           �m�q�k��  �W�O�O�n
001306 FD  �ی��Ҋg���}�X�^        BLOCK   CONTAINS   1   RECORDS.
001307     COPY HOKENEX        OF  XFDLIB  JOINING   �ۊg   AS  PREFIX.
001320*                           �m�q�k��  �P�Q�W�n
001330 FD  ��ƃt�@�C���Q RECORD  CONTAINS 128 CHARACTERS.
001340 01  ��Q�|���R�[�h.
001350     03  ��Q�|���R�[�h�L�[.
000178         05  ��Q�|�����a��N��.
000179             07  ��Q�|�����a��              PIC 9.
000180             07  ��Q�|�����N                PIC 9(2).
000181             07  ��Q�|������                PIC 9(2).
001261         05  ��Q�|���ރR�[�h                PIC 9(1).
001261         05  ��Q�|���R�[�h                  PIC X(2).
001400         05  ��Q�|�ی���                    PIC 9(2).
000183         05  ��Q�|�ی��Ҕԍ�                PIC X(10).
000188     03  ��Q�|���R�[�h�f�[�^.
001400         05  ��Q�|�ی����                  PIC 9(2).
000189         05  ��Q�|����                      PIC 9(4).
000190         05  ��Q�|��p�z                    PIC 9(9).
000191         05  ��Q�|���S�z                    PIC 9(9).
000192         05  ��Q�|�����z                    PIC 9(9).
000193         05  ��Q�|�{�l����                  PIC 9(3).
000194         05  ��Q�|�{�l��p�z                PIC 9(7).
000195         05  ��Q�|�{�l���S�z                PIC 9(7).
000196         05  ��Q�|�{�l�����z                PIC 9(7).
000193         05  ��Q�|�{�l������                PIC 9(4).
000197         05  ��Q�|�Ƒ�����                  PIC 9(3).
000198         05  ��Q�|�Ƒ���p�z                PIC 9(7).
000199         05  ��Q�|�Ƒ����S�z                PIC 9(7).
000200         05  ��Q�|�Ƒ������z                PIC 9(7).
000197         05  ��Q�|�Ƒ�������                PIC 9(4).
000201         05  FILLER                          PIC X(19).
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
001720     COPY YHN437P        OF  XMDLIB.
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
001210 01 �I�[�v���t���O                     PIC X(3)   VALUE SPACE.
001930*
001940 01 �s�J�E���^                         PIC 9(2)  VALUE ZERO.
001950 01 �ŃJ�E���^                         PIC 9(4)  VALUE ZERO.
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
002330    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
002340    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
002341    03 �_���t�ԍ��v                    PIC X(20)  VALUE SPACE.
002350    03 �{�p���Z���v.
002360       05 �{�p���Z���P�v               PIC X(50)  VALUE SPACE.
002370       05 �{�p���Z���Q�v               PIC X(50)  VALUE SPACE.
002380    03 �{�p���X�֔ԍ��v.
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
002540 01 �a����ʃR�����g�v                 PIC N(2)   VALUE SPACE.
002551**
002552* �Еۗp
002553 01 �ڔ���敪�v                       PIC 9     VALUE ZERO.
002554*
002560********************
002570* �ی��ҕʍ��v���z *
002580********************
002590 01 �ی��ҕʍ��v���z.
002600    03 ���z�v                          PIC N(2)  VALUE SPACE.
002610    03 �������b�Z�[�W�v                PIC N(15) VALUE SPACE.
002620    03 �~�v                            PIC N(1)  VALUE SPACE.
002630    03 �����v                          PIC 9(3)  VALUE ZERO.
002640    03 ��p�z�v                        PIC 9(8)  VALUE ZERO.
002650    03 �����z�v                        PIC 9(7)  VALUE ZERO.
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
001220 01 �ی���ʖ��v.
          03 �ی���ʖ��v�o                  PIC X(8) VALUE SPACE.
002846*
002847***
002852 01 �������֘A�v.
002862        07 �k�C���敪�v                PIC 9 VALUE ZERO.
002862        07 �X�敪�v                  PIC 9 VALUE ZERO.
002862        07 ���敪�v                  PIC 9 VALUE ZERO.
002862        07 �{��敪�v                  PIC 9 VALUE ZERO.
002862        07 �H�c�敪�v                  PIC 9 VALUE ZERO.
002862        07 �R�`�敪�v                  PIC 9 VALUE ZERO.
002862        07 �����敪�v                  PIC 9 VALUE ZERO.
002862        07 ���敪�v                  PIC 9 VALUE ZERO.
002862        07 �Ȗ؋敪�v                  PIC 9 VALUE ZERO.
002862        07 �Q�n�敪�v                  PIC 9 VALUE ZERO.
002862        07 ��ʋ敪�v                  PIC 9 VALUE ZERO.
002862        07 ��t�敪�v                  PIC 9 VALUE ZERO.
002862        07 �����敪�v                  PIC 9 VALUE ZERO.
002862        07 �_�ސ�敪�v                PIC 9 VALUE ZERO.
002862        07 �V���敪�v                  PIC 9 VALUE ZERO.
002862        07 �x�R�敪�v                  PIC 9 VALUE ZERO.
002862        07 �ΐ�敪�v                  PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 �R���敪�v                  PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 �򕌋敪�v                  PIC 9 VALUE ZERO.
002862        07 �É��敪�v                  PIC 9 VALUE ZERO.
002862        07 ���m�敪�v                  PIC 9 VALUE ZERO.
002862        07 �O�d�敪�v                  PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 ���s�敪�v                  PIC 9 VALUE ZERO.
002862        07 ���敪�v                  PIC 9 VALUE ZERO.
002862        07 ���ɋ敪�v                  PIC 9 VALUE ZERO.
002862        07 �ޗǋ敪�v                  PIC 9 VALUE ZERO.
002862        07 �a�̎R�敪�v                PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 �����敪�v                  PIC 9 VALUE ZERO.
002862        07 ���R�敪�v                  PIC 9 VALUE ZERO.
002862        07 �L���敪�v                  PIC 9 VALUE ZERO.
002862        07 �R���敪�v                  PIC 9 VALUE ZERO.
002862        07 �����敪�v                  PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 ���Q�敪�v                  PIC 9 VALUE ZERO.
002862        07 ���m�敪�v                  PIC 9 VALUE ZERO.
002862        07 �����敪�v                  PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
002862        07 �F�{�敪�v                  PIC 9 VALUE ZERO.
002862        07 �啪�敪�v                  PIC 9 VALUE ZERO.
002862        07 �{��敪�v                  PIC 9 VALUE ZERO.
002862        07 �������敪�v                PIC 9 VALUE ZERO.
002862        07 ����敪�v                  PIC 9 VALUE ZERO.
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
           IF �A��|���ރR�[�h = 4
005715         CALL   "YHN438"
005716         CANCEL "YHN438"
003820         EXIT PROGRAM
           END-IF.
002570     PERFORM �v�����^�t�@�C���쐬.
003581     PERFORM ������.
003582     PERFORM ������擾�Q.
003980     PERFORM �{�p�����擾.
003583************
003590*           *
003600* �又��     *
003610*           *
003620************
           IF �A��|���ރR�[�h = ZERO
002484        PERFORM �������
           ELSE
002484        PERFORM ��������P
           END-IF.
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
002974     MOVE "YHN437"              TO �g�A�o�q�s�e�|���[�v���O������.
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
004610         MOVE �{��|�X�֔ԍ��P       TO �{�p���X�֔ԍ��P�v
004620         MOVE "-"                    TO �{�p���X�֔ԍ���؂v
004630         MOVE �{��|�X�֔ԍ��Q       TO �{�p���X�֔ԍ��Q�v
004640         MOVE �{��|��\�Җ�         TO ��\�Җ��v
004650         MOVE �{��|�ڍ��@��         TO �ڍ��@���v
004660         STRING �{��|�Z���P  DELIMITED BY SPACE
004670                �{��|�Z���Q  DELIMITED BY SPACE
004680           INTO �{�p���Z���v
004690         END-STRING
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
004860             MOVE NC"����" TO �a����ʃR�����g�v
004870         WHEN 2
004880             MOVE NC"����" TO �a����ʃR�����g�v
004890         WHEN OTHER
004900             MOVE SPACE    TO �a����ʃR�����g�v
004910         END-EVALUATE
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
005060     OPEN INPUT   �s�����}�X�^
005070         MOVE NC"�s��" TO �t�@�C����.
005080         PERFORM �I�[�v���`�F�b�N.
005120     OPEN INPUT   �{�p�����}�X�^
005130         MOVE NC"�{��" TO �t�@�C����.
005140         PERFORM �I�[�v���`�F�b�N.
005150     OPEN INPUT   ������}�X�^
005160         MOVE NC"����" TO �t�@�C����.
005170         PERFORM �I�[�v���`�F�b�N.
005171     OPEN INPUT   ����}�X�^
005172         MOVE NC"����" TO �t�@�C����.
005173         PERFORM �I�[�v���`�F�b�N.
005174     OPEN INPUT �ی��Ҋg���}�X�^.
005175         MOVE NC"�ۊg" TO �t�@�C����.
005176         PERFORM �I�[�v���`�F�b�N.
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
005350           �s�����}�X�^   �{�p�����}�X�^ ������}�X�^  
005360           �ی��Ҋg���}�X�^ ����}�X�^   ���̃}�X�^.
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
005570     OPEN INPUT  ��ƃt�@�C���Q.
005580         MOVE NC"��Q" TO �t�@�C����.
005590         PERFORM �I�[�v���`�F�b�N.
005530*
           MOVE SPACE     TO  ��ƃt���O.
      * / ���я��ύX
003853     MOVE ZERO      TO  ��Q�|�����a��.
003854     MOVE ZERO      TO  ��Q�|�����N.
003855     MOVE ZERO      TO  ��Q�|������.
003857     MOVE ZERO      TO  ��Q�|���ރR�[�h.
003856     MOVE LOW-VALUE TO  ��Q�|���R�[�h.
003858     MOVE ZERO      TO  ��Q�|�ی���.
003859     MOVE LOW-VALUE TO  ��Q�|�ی��Ҕԍ�.
003860     START ��ƃt�@�C���Q   KEY IS >=  ��Q�|�����a��N��
003861                                       ��Q�|���ރR�[�h
003862                                       ��Q�|���R�[�h
003863                                       ��Q�|�ی���
003864                                       ��Q�|�ی��Ҕԍ�
003865     END-START.
005626     IF ��ԃL�[ = "00"
005627         MOVE SPACE TO �I���t���O
005628         PERFORM ��ƃt�@�C���Q�Ǎ�
005629         IF  �I���t���O = "YES"
005630             MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
005631             CALL   "MSG001"
005632             CANCEL "MSG001"
005633             PERFORM �t�@�C����
005634             MOVE 99 TO PROGRAM-STATUS
005635             EXIT PROGRAM
005636         END-IF
005673*
005699         PERFORM UNTIL  �I���t���O = "YES"
                  IF (��Q�|���ރR�[�h = 4)
                      IF (��ƃt���O = SPACE)
002990                   IF ( �I�[�v���t���O = "YES" )
002991                      CLOSE ����t�@�C��
004320                      MOVE SPACE TO �I�[�v���t���O
003041                   END-IF
005715                   CALL   "YHN438"
005716                   CANCEL "YHN438"
                         MOVE "YES" TO ��ƃt���O
                      END-IF
003820            ELSE
005700                PERFORM ����Ώۃ`�F�b�N
005701                IF ����t���O = "YES"
005703                   MOVE SPACE TO YHN437P
005704****                INITIALIZE YHN437P
005705                   PERFORM �w�b�_�Z�b�g
005706                   PERFORM ���׃Z�b�g
                         PERFORM �t�b�^�Z�b�g
005707                   PERFORM �󎚏���
005708                   PERFORM ���ŏ���
                      END-IF
005709            END-IF
005710            PERFORM ��ƃt�@�C���Q�Ǎ�
005711         END-PERFORM
005712*
005713     ELSE
005714         MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
005715         CALL   "MSG001"
005716         CANCEL "MSG001"
005717         PERFORM �t�@�C����
005718         MOVE 99 TO PROGRAM-STATUS
005719         EXIT PROGRAM
005720     END-IF.
006570*
006580     CLOSE ��ƃt�@�C���Q.
006581*
005510*================================================================*
005520 ��������P SECTION.
005530*
005570     OPEN INPUT  ��ƃt�@�C���Q.
005580         MOVE NC"��Q" TO �t�@�C����.
005590         PERFORM �I�[�v���`�F�b�N.
005530*
      * / ���я��ύX
003853     MOVE �A���|�����a��    TO  ��Q�|�����a��.
003854     MOVE �A���|�����N      TO  ��Q�|�����N.
003855     MOVE �A���|������      TO  ��Q�|������.
003857     MOVE �A��|���ރR�[�h  TO  ��Q�|���ރR�[�h.
003856     MOVE �A��|���R�[�h    TO  ��Q�|���R�[�h.
003858     MOVE �A��|�ی���      TO  ��Q�|�ی���.
003859     MOVE LOW-VALUE         TO  ��Q�|�ی��Ҕԍ�.
003860     START ��ƃt�@�C���Q   KEY IS >=  ��Q�|�����a��N��
003861                                       ��Q�|���ރR�[�h
003862                                       ��Q�|���R�[�h
003863                                       ��Q�|�ی���
003864                                       ��Q�|�ی��Ҕԍ�
003865     END-START.
005626     IF ��ԃL�[ = "00"
005627         MOVE SPACE TO �I���t���O
005628         PERFORM ��ƃt�@�C���Q�Ǎ�
005629         IF  �I���t���O = "YES"
005630             MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
005631             CALL   "MSG001"
005632             CANCEL "MSG001"
005633             PERFORM �t�@�C����
005634             MOVE 99 TO PROGRAM-STATUS
005635             EXIT PROGRAM
005636         END-IF
005673*
003878         PERFORM UNTIL (�I���t���O = "YES") OR
                             (��Q�|���ރR�[�h NOT = �A��|���ރR�[�h) OR
                             (��Q�|�ی���     NOT = �A��|�ی���) OR
                             (��Q�|���R�[�h   NOT = �A��|���R�[�h)
005700                PERFORM ����Ώۃ`�F�b�N
005701                IF ����t���O = "YES"
005703                   MOVE SPACE TO YHN437P
005704****                INITIALIZE YHN437P
005705                   PERFORM �w�b�_�Z�b�g
005706                   PERFORM ���׃Z�b�g
                         PERFORM �t�b�^�Z�b�g
005707                   PERFORM �󎚏���
005708                   PERFORM ���ŏ���
005709                END-IF
005710                PERFORM ��ƃt�@�C���Q�Ǎ�
005711         END-PERFORM
005712*
005713     ELSE
005714         MOVE  NC"�@�Y���f�[�^�Ȃ��ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
005715         CALL   "MSG001"
005716         CANCEL "MSG001"
005717         PERFORM �t�@�C����
005718         MOVE 99 TO PROGRAM-STATUS
005719         EXIT PROGRAM
005720     END-IF.
006570*
006580     CLOSE ��ƃt�@�C���Q.
006581*
006660*================================================================*
006670 ��ƃt�@�C���Q�Ǎ� SECTION.
006680*
006690     READ ��ƃt�@�C���Q NEXT
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
006724     MOVE "YHN437P" TO  ��`�̖��o.
006725     MOVE SPACE     TO  ������ʂo.
006726     MOVE "SCREEN"  TO  ���ڌQ���o.
006727     WRITE YHN437P.
006728     PERFORM �G���[�����o.
006755*================================================================*
006756 ���ŏ���  SECTION.
006757*
006760     MOVE "YHN437P" TO  ��`�̖��o.
006770     MOVE "CT"      TO  ������ʂo.
006780     MOVE "PAGE"    TO  �g������o.
006790     MOVE SPACE     TO  ���ڌQ���o.
006800     WRITE YHN437P.
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
           IF �A���|�쐬����� = 1
007110        MOVE ��o�a��v         TO ���|�����敪
007120        READ �����}�X�^
007130        INVALID KEY
007140           MOVE SPACE          TO �쐬�a��
007150        NOT INVALID KEY
007160           MOVE ���|��������   TO �쐬�a��
007170        END-READ
007180        MOVE ��o�N�v          TO �쐬�N
007190        MOVE ��o���v          TO �쐬��
007200        MOVE ��o���v          TO �쐬��
      */�R�����g���o���ɕύX/101012
      *        MOVE NC"�쐬��"        TO �쐬�b�l
              MOVE NC"��o��"        TO �쐬�b�l
              MOVE NC"�N"            TO �쐬�N�b�l
              MOVE NC"��"            TO �쐬���b�l
              MOVE NC"��"            TO �쐬���b�l
           END-IF.
007210*
007211     MOVE �{�p���Z���v       TO �Z��.
007220*     MOVE �{�p���X�֔ԍ��v   TO �X�֔ԍ�.
007230     MOVE ��\�Җ��v         TO ��\�Җ�.
007240     MOVE �ڍ��@���v         TO �ڍ��@��.
007241     MOVE �_���t�ԍ��v       TO �_���t�ԍ�.
007250     MOVE �{�p���d�b�ԍ��v   TO �d�b�ԍ�.
007251*
007253     PERFORM ��s�Z�b�g.
007254*
007260     MOVE ��s���x�X���v     TO ��s���x�X��.
007270     MOVE �a����ʃR�����g�v TO �a�����.
007280     MOVE �����ԍ��v         TO �����ԍ�.
      */�ϔC�ҏ��Ή�
007281*     MOVE �������`�l�J�i�v   TO �������`�l�J�i.
007282*     MOVE �������`�l�v       TO �������`�l.
007281     MOVE �������`�l�J�i�P�v   TO �������`�l�J�i.
007281     MOVE �������`�l�J�i�Q�v   TO �������`�l�J�i�Q.
007282     MOVE �������`�l�P�v       TO �������`�l.
007282     MOVE �������`�l�Q�v       TO �������`�l�Q.
007283*
007284     MOVE NC"�U����F"       TO �U����\��.
007285*
007292* / �ی���/
007300     MOVE ��Q�|�ی����     TO �ی���ʂv.
007310     MOVE ��Q�|�ی��Ҕԍ�   TO �ی��Ҕԍ��v.
007320     EVALUATE �ی���ʂv
007330     WHEN 1 THRU 4
007340     WHEN 6 THRU 9
007350*     WHEN 70 
007360*     WHEN 80
007370         PERFORM �ی��ҏ��擾
007380     WHEN 5
007390     WHEN 50 THRU 60
007400         PERFORM �s�������擾
007410     END-EVALUATE.
007420     PERFORM ���������擾.
007421*
007430     MOVE �ی��҈����v  TO �ی��Җ���.
007490*
007500* / �U���ԍ� /
007510     MOVE �ی���ʂv    TO �ۊg�|�ی����.
007520     MOVE �ی��Ҕԍ��v  TO �ۊg�|�ی��Ҕԍ�.
007530     READ �ی��Ҋg���}�X�^
007540     INVALID KEY
007550         MOVE SPACE           TO �U���ԍ�
007560     NOT INVALID KEY
007570         MOVE �ۊg�|�U���ԍ�  TO �U���ԍ�
007580     END-READ.
007590*
      */�ی��Ҕԍ����󎚂���/081016
           IF ��Q�|�ی���� <= 09
               MOVE NC"�ی��Ҕԍ�"   TO �ی��Ҕԍ��\��
               MOVE ��Q�|�ی��Ҕԍ� TO �ی��Ҕԍ�
           END-IF.
007741*================================================================*
007742 ���׃Z�b�g SECTION.
007743*
007760     IF ��Q�|�{�l���� NOT = ZERO
007761        MOVE ��Q�|�{�l����     TO �{�l����
007762        MOVE ��Q�|�{�l��p�z   TO �{�l��p�z
007763        MOVE ��Q�|�{�l���S�z   TO �{�l���S�z
007770        MOVE ��Q�|�{�l�����z   TO �{�l�����z
007771     END-IF.
007772     IF ��Q�|�Ƒ����� NOT = ZERO
007774        MOVE ��Q�|�Ƒ�����     TO �Ƒ�����
007775        MOVE ��Q�|�Ƒ���p�z   TO �Ƒ���p�z
007776        MOVE ��Q�|�Ƒ����S�z   TO �Ƒ����S�z
007777        MOVE ��Q�|�Ƒ������z   TO �Ƒ������z
007778     END-IF.
007780*
006390*================================================================*
006400 �t�b�^�Z�b�g SECTION.
006410*
005570     OPEN INPUT  ��ƃt�@�C���S.
005580         MOVE NC"��S" TO �t�@�C����.
005590         PERFORM �I�[�v���`�F�b�N.
      */ �ی���ʁA�����A��ی��Җ�������ԂɈ�� /150219
           IF ��Q�|�ی���� = 01
               MOVE ��Q�|�ی��Ҕԍ�(1:2)  TO ���|���̃R�[�h
           ELSE
               MOVE ��Q�|�ی��Ҕԍ�(3:2)  TO ���|���̃R�[�h
           END-IF.
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
           EVALUATE ��Q�|���ރR�[�h
           WHEN 5
               EVALUATE ��Q�|�ی���
               WHEN 10
                   MOVE "���ۘA����|����"    TO �ی��Җ��̂b�l
               WHEN 20
                   MOVE "���ۘA����|���"    TO �ی��Җ��̂b�l
               WHEN 30
                   MOVE ��Q�|�ی����        TO ���|���̃R�[�h
025960             MOVE 12                    TO ���|�敪�R�[�h
025980             READ ���̃}�X�^
026010             NOT INVALID KEY
026020                 MOVE ���|����          TO �ی���ʖ��v
026030             END-READ
                   IF ��Q�|�ی���� = 52
                       MOVE "�ЂƂ�e"        TO �ی���ʖ��v�o
                   END-IF
                   STRING "(���ۘA����|"  DELIMITED BY SIZE
                          �ی���ʖ��v�o   DELIMITED BY SPACE
                         ")"              DELIMITED BY SIZE
                     INTO �ی��Җ��̂b�l
                   END-STRING
               END-EVALUATE
           WHEN 1
               MOVE "�S�����N�ی�����x���|�����"    TO �ی��Җ��̂b�l
           WHEN 2
               MOVE "�D���ی�"      TO �ی��Җ��̂b�l
           WHEN 3
               MOVE "���ۑg��"      TO �ی��Җ��̂b�l
           WHEN 4
               STRING "���ϑg��("      DELIMITED BY SIZE
                      ��Q�|�ی��Ҕԍ� DELIMITED BY SPACE
                      ")"              DELIMITED BY SIZE
                 INTO �ی��Җ��̂b�l
               END-STRING
           WHEN 6
               MOVE ��Q�|�ی����     TO ���|���̃R�[�h
025960         MOVE 12                 TO ���|�敪�R�[�h
025980         READ ���̃}�X�^
026010         NOT INVALID KEY
026020             MOVE ���|����       TO �ی���ʖ��v
026030         END-READ
               IF ��Q�|�ی���� = 52
                   MOVE "�ЂƂ�e"     TO �ی���ʖ��v�o
               END-IF
               STRING �ی���ʖ��v�o   DELIMITED BY SPACE
                      "("              DELIMITED BY SIZE
                      ��Q�|�ی��Ҕԍ� DELIMITED BY SPACE
                      ")"              DELIMITED BY SIZE
                 INTO �ی��Җ��̂b�l
               END-STRING
           END-EVALUATE.
      *
           MOVE ��Q�|���ރR�[�h TO ��S�|���ރR�[�h.
           MOVE ��Q�|���R�[�h   TO ��S�|���R�[�h.
           MOVE ��Q�|�ی���     TO ��S�|�ی���.
           MOVE ��Q�|�ی��Ҕԍ� TO ��S�|�ی��Ҕԍ�.
           READ ��ƃt�@�C���S
           NOT INVALID KEY
              MOVE ��S�|�U������   TO �敪�P
              MOVE ��S�|�V������   TO �敪�Q
              MOVE "*"              TO �敪�R
              MOVE "-"              TO ��؂P ��؂Q
              IF ��S�|���ރR�[�h = 3 OR 4 OR 6
                 MOVE "�~"             TO ��
              END-IF
           END-READ.
006580     CLOSE ��ƃt�@�C���S.
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
008690*================================================================*
008700 �s�������擾 SECTION.
008710*
008711     MOVE  SPACE         TO �����於�̂v.
008712     MOVE  SPACE         TO �x���������v.
008713*
008720     MOVE �ی���ʂv               TO �s�|������.
008730     MOVE �ی��Ҕԍ��v             TO �s�|�s�����ԍ�.
008740     READ �s�����}�X�^
008750     INVALID KEY
008760         MOVE SPACE                TO �����於�̂v
008770         MOVE SPACE                TO �x���������v
008780     NOT INVALID KEY
008790         IF �s�|������敪 = 1
008800             MOVE �ی���ʂv       TO ����|�ی����
008810             MOVE �ی��Ҕԍ��v     TO ����|�ی��Ҕԍ�
008820             READ ������}�X�^
008830             INVALID KEY
008840                 MOVE SPACE        TO �����於�̂v
008850                 MOVE SPACE        TO �x���������v
008860             NOT INVALID KEY
008868                 MOVE ����|�ی��Җ���   TO �����於�̂v
008869                 MOVE ����|�x��������   TO �x���������v
008890             END-READ
008900          ELSE
008908             MOVE �s�|�s��������   TO �����於�̂v
008909             MOVE �s�|�x��������   TO �x���������v
008930          END-IF
008940      END-READ.
008950*================================================================*
008960 ���������擾 SECTION.
008970*
008971     MOVE SPACE TO �ی��҈����v.
008972     IF �����於�̂v NOT = SPACE
008980         EVALUATE �ی���ʂv
008981         WHEN 2
008982             IF �ڔ���敪�v = 1
008983                MOVE SPACE            TO �����v
008984             ELSE
008985                MOVE "�Љ�ی�������" TO �����v
008986             END-IF
008987         WHEN 6
008988             IF �ڔ���敪�v = 1
008989*                MOVE "�i���فj"               TO �����v
                      CONTINUE
008991             ELSE
008992*                MOVE "�Љ�ی��������i���فj" TO �����v
008992                MOVE "�Љ�ی�������" TO �����v
008993             END-IF
008994         WHEN 7
008995*             MOVE "�i�D���j"       TO �����v
                   CONTINUE
009020         WHEN 3
009030             MOVE "���N�ی��g��"   TO �����v
009031         WHEN 4
009032             MOVE "���ϑg��"       TO �����v
009033         WHEN 8
009034             MOVE "�i�ސE�j"       TO �����v
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
009435*================================================================*
009436 ������擾�Q SECTION.
      *
009438* ����敪02
009439     MOVE 02 TO ���|����敪.
009440     READ ������}�X�^
009441     NOT INVALID KEY
009443        MOVE ���O�Q�|�k�C�������\�敪  TO �k�C���敪�v 
009443        MOVE ���O�Q�|�X�����\�敪    TO �X�敪�v 
009443        MOVE ���O�Q�|��葍���\�敪    TO ���敪�v 
009443        MOVE ���O�Q�|�{�鑍���\�敪    TO �{��敪�v 
009443        MOVE ���O�Q�|�H�c�����\�敪    TO �H�c�敪�v 
009443        MOVE ���O�Q�|�R�`�����\�敪    TO �R�`�敪�v 
009443        MOVE ���O�Q�|���������\�敪    TO �����敪�v 
009443        MOVE ���O�Q�|��鑍���\�敪    TO ���敪�v 
009443        MOVE ���O�Q�|�Ȗؑ����\�敪    TO �Ȗ؋敪�v 
009443        MOVE ���O�Q�|�Q�n�����\�敪    TO �Q�n�敪�v 
009443        MOVE ���O�Q�|��ʑ����\�敪    TO ��ʋ敪�v 
009443        MOVE ���O�Q�|��t�����\�敪    TO ��t�敪�v 
009443        MOVE ���O�Q�|���������\�敪    TO �����敪�v 
009443        MOVE ���O�Q�|�_�ސ쑍���\�敪  TO �_�ސ�敪�v 
009443        MOVE ���O�Q�|�V�������\�敪    TO �V���敪�v 
009443        MOVE ���O�Q�|�x�R�����\�敪    TO �x�R�敪�v 
009443        MOVE ���O�Q�|�ΐ쑍���\�敪    TO �ΐ�敪�v 
009443        MOVE ���O�Q�|���䑍���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|�R�������\�敪    TO �R���敪�v 
009443        MOVE ���O�Q�|���쑍���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|�򕌑����\�敪    TO �򕌋敪�v 
009443        MOVE ���O�Q�|�É������\�敪    TO �É��敪�v 
009443        MOVE ���O�Q�|���m�����\�敪    TO ���m�敪�v 
009443        MOVE ���O�Q�|�O�d�����\�敪    TO �O�d�敪�v 
009443        MOVE ���O�Q�|���ꑍ���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|���s�����\�敪    TO ���s�敪�v 
009443        MOVE ���O�Q�|��㑍���\�敪    TO ���敪�v 
009443        MOVE ���O�Q�|���ɑ����\�敪    TO ���ɋ敪�v 
009443        MOVE ���O�Q�|�ޗǑ����\�敪    TO �ޗǋ敪�v 
009443        MOVE ���O�Q�|�a�̎R�����\�敪  TO �a�̎R�敪�v 
009443        MOVE ���O�Q�|���摍���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|���������\�敪    TO �����敪�v 
009443        MOVE ���O�Q�|���R�����\�敪    TO ���R�敪�v 
009443        MOVE ���O�Q�|�L�������\�敪    TO �L���敪�v 
009443        MOVE ���O�Q�|�R�������\�敪    TO �R���敪�v 
009443        MOVE ���O�Q�|���������\�敪    TO �����敪�v 
009443        MOVE ���O�Q�|���쑍���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|���Q�����\�敪    TO ���Q�敪�v 
009443        MOVE ���O�Q�|���m�����\�敪    TO ���m�敪�v 
009443        MOVE ���O�Q�|���������\�敪    TO �����敪�v 
009443        MOVE ���O�Q�|���ꑍ���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|���葍���\�敪    TO ����敪�v 
009443        MOVE ���O�Q�|�F�{�����\�敪    TO �F�{�敪�v 
009443        MOVE ���O�Q�|�啪�����\�敪    TO �啪�敪�v 
009443        MOVE ���O�Q�|�{�葍���\�敪    TO �{��敪�v 
009443        MOVE ���O�Q�|�����������\�敪  TO �������敪�v 
009443        MOVE ���O�Q�|���ꑍ���\�敪    TO ����敪�v 
009468     END-READ.
009469*
007591*================================================================*
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
009499                MOVE NC"����" TO �a����ʃR�����g�v
009500            WHEN 2
009501                MOVE NC"����" TO �a����ʃR�����g�v
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
009538*================================================================*
009539 ����Ώۃ`�F�b�N  SECTION.
009540*
009541*  ����敪�ɂ��U�蕪�� �� �U�E�V���������i����敪 0:��� 1:������Ȃ��j
009542* �i�ꊇ����̂݁j
009543*
009544*     MOVE SPACE TO ����t���O.
009545*
009546*     IF �A���|�ꊇ�敪 NOT = 1
009547        MOVE "YES" TO ����t���O
009548*     ELSE
009549*        EVALUATE ��Q�|�ی����
009550*        WHEN 01
009551*        WHEN 08
009552*           IF ���ۂV������敪�v NOT = 1
009553*              MOVE "YES" TO ����t���O
009554*           END-IF
009555*        WHEN 02
009556*        WHEN 06
009557*        WHEN 07
009558*           IF �ЕۂV������敪�v NOT = 1
009559*              MOVE "YES" TO ����t���O
009560*           END-IF
009561*        WHEN 03
009562*           IF �g���V������敪�v NOT = 1
009563*              MOVE "YES" TO ����t���O
009564*           END-IF
009565*        WHEN 04
009566*        WHEN 09
009567*           IF ���ςV������敪�v NOT = 1
009568*              MOVE "YES" TO ����t���O
009569*           END-IF
009570*        WHEN 05
009571*           IF �V�l�V������敪�v NOT = 1
009572*              MOVE "YES" TO ����t���O
009573*           END-IF
009574*        WHEN 50 THRU 60
009575*           IF �����V������敪�v NOT = 1
009576*              MOVE "YES" TO ����t���O
009577*           END-IF
009578*        WHEN OTHER
009579*           MOVE "YES" TO ����t���O
009580*        END-EVALUATE
      *
              IF ����t���O = "YES"
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 01)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 01))
                    IF (�k�C���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 02)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 02))
                    IF (�X�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 03)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 03))
                    IF (���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 04)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 04))
                    IF (�{��敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 05)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 05))
                    IF (�H�c�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 06)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 06))
                    IF (�R�`�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 07)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 07))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 08)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 08))
                    IF (���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 09)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 09))
                    IF (�Ȗ؋敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 10)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 10))
                    IF (�Q�n�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 11)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 11))
                    IF (��ʋ敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
      */��t�͌��ŗL�I���ɂȂ��Ă��Ă��U�V�����o��*/150409
      *           IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 12)) OR
      *              ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 12))
      *              IF (��t�敪�v = ZERO)
009576*                 MOVE "YES" TO ����t���O
      *              ELSE
009576*                 MOVE SPACE TO ����t���O
009577*              END-IF
009577*           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 13)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 13))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 14)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 14))
                    IF (�_�ސ�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 15)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 15))
                    IF (�V���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 16)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 16))
                    IF (�x�R�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 17)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 17))
                    IF (�ΐ�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 18)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 18))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 19)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 19))
                    IF (�R���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 20)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 20)) OR
                    (((��Q�|�ی���� >= 50) AND (��Q�|�ی���� <= 60)) AND (��Q�|�ی��Ҕԍ�(3:2) = 20))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 21)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 21))
                    IF (�򕌋敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 22)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 22))
                    IF (�É��敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 23)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 23))
                    IF (���m�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 24)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 24))
                    IF (�O�d�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 25)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 25))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 26)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 26)) OR
                    ((��Q�|�ی���� >= 50) AND (��Q�|�ی���� <= 60))
                    IF (���s�敪�v = ZERO)
009576                  MOVE "YES" TO ����t���O
                    ELSE
                       IF ((��Q�|�ی���� = 53) AND (��Q�|�ی��Ҕԍ�(1:4) = 3926))
009576                    MOVE "YES" TO ����t���O
                       ELSE
009576                    MOVE SPACE TO ����t���O
                       END-IF
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(3:2) = 27)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 27)) OR
                    (((��Q�|�ی���� >= 50) AND (��Q�|�ی���� <= 60)) AND (��Q�|�ی��Ҕԍ�(3:2) = 27))
                    IF (���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 28)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 28))
                    IF (���ɋ敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 29)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 29))
                    IF (�ޗǋ敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 30)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 30))
                    IF (�a�̎R�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 31)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 31))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 32)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 32))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 33)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 33))
                    IF (���R�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 34)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 34))
                    IF (�L���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 35)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 35))
                    IF (�R���敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 36)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 36))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 37)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 37))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 38)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 38))
                    IF (���Q�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 39)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 39))
                    IF (���m�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 40)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 40))
                    IF (�����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 41)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 41))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 42)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 42))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 43)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 43))
                    IF (�F�{�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 44)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 44))
                    IF (�啪�敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 45)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 45))
                    IF (�{��敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 46)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 46))
                    IF (�������敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
                 IF ((��Q�|�ی���� = 01) AND (��Q�|�ی��Ҕԍ�(1:2) = 47)) OR
                    ((��Q�|�ی���� = 08 OR 05) AND (��Q�|�ی��Ҕԍ�(3:2) = 47))
                    IF (����敪�v = ZERO)
009576                 MOVE "YES" TO ����t���O
                    ELSE
009576                 MOVE SPACE TO ����t���O
009577              END-IF
009577           END-IF
009577*        END-IF
009581     END-IF.
009582*
009605*================================================================*
009606******************************************************************
009607 END PROGRAM YHN437.
009608******************************************************************
