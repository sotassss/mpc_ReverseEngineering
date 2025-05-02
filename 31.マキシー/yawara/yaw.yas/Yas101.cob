000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAS101.
000060 AUTHOR.                 ���c ���a
000070*
000080*----------------------------------------------------------------*
000090*      ��oFPD�쐬�y�ް��쐬�z�_����޳��95��
000100*
000110* �� ���N����(����-���a)�t�A���я��ǉ�
000120*
000130* �����N��Ver.
000140*
000150*----------------------------------------------------------------*
000160 DATE-WRITTEN.           2000-02-01
000170 DATE-COMPILED.          2000-02-01
000180*----------------------------------------------------------------*
000190******************************************************************
000200*            ENVIRONMENT         DIVISION                        *
000210******************************************************************
000220 ENVIRONMENT             DIVISION.
000230 CONFIGURATION           SECTION.
000240 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000250 OBJECT-COMPUTER.        FMV-DESKPOWER.
000260 SPECIAL-NAMES.          CONSOLE  IS  CONS
000270                         SYSERR   IS  MSGBOX.
000280 INPUT-OUTPUT            SECTION.
000290 FILE-CONTROL.
000300     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000310                             ORGANIZATION             IS  INDEXED
000320                             ACCESS MODE              IS  DYNAMIC
000330                             RECORD KEY               IS  ���|�����敪
000340                             FILE STATUS              IS  ��ԃL�[
000350                             LOCK        MODE         IS  AUTOMATIC.
000360     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000370                             ORGANIZATION             IS  INDEXED
000380                             ACCESS MODE              IS  DYNAMIC
000390                             RECORD KEY               IS  ���|�敪�R�[�h
000400                                                          ���|���̃R�[�h
000410                             FILE STATUS              IS  ��ԃL�[
000420                             LOCK        MODE         IS  AUTOMATIC.
000430     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000440                             ORGANIZATION             IS  INDEXED
000450                             ACCESS MODE              IS  DYNAMIC
000460                             RECORD KEY               IS �{��|�{�p���ԍ�
000470                             FILE STATUS              IS  ��ԃL�[
000480                             LOCK        MODE         IS  AUTOMATIC.
000490     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
000500                             ORGANIZATION             IS  INDEXED
000510                             ACCESS MODE              IS  DYNAMIC
000520                             RECORD KEY           IS �{�L�|�{�p�a��N����
000530                                                     �{�L�|���҃R�[�h
000540                             ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
000550                                                     �{�L�|�{�p�a��N����
000560                             FILE STATUS              IS  ��ԃL�[
000570                             LOCK        MODE         IS  AUTOMATIC.
000580     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000590                             ORGANIZATION             IS  INDEXED
000600                             ACCESS MODE              IS  DYNAMIC
000610                             RECORD KEY               IS ��|�{�p�a��N��
000620                                                          ��|���҃R�[�h
000630                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000640                                                          ��|���҃J�i
000650                                                          ��|���҃R�[�h
000660                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000670                                                         ��|�{�p�a��N��
000680                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000690                                                          ��|�ی����
000700                                                          ��|�ی��Ҕԍ�
000710                                                          ��|���҃R�[�h
000720                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000730                                                          ��|������
000740                                                     ��|��p���S�Ҕԍ�
000750                                                          ��|���҃R�[�h
000760                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000770                                                          ��|�������
000780                                                  ��|��p���S�Ҕԍ�����
000790                                                          ��|���҃R�[�h
000800                             ALTERNATE RECORD KEY  IS ��|�����a��N��
000810                                                      ��|�{�p�a��N��
000820                                                      ��|���҃R�[�h
000830                             FILE STATUS              IS  ��ԃL�[
000840                             LOCK        MODE         IS  AUTOMATIC.
000850     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000860                             ORGANIZATION             IS  INDEXED
000870                             ACCESS MODE              IS  DYNAMIC
000880                             RECORD KEY               IS ���|�{�p�a��N��
000890                                                         ���|���҃R�[�h
000900                             ALTERNATE RECORD KEY     IS ���|���҃R�[�h
000910                                                         ���|�{�p�a��N��
000920                             FILE STATUS              IS  ��ԃL�[
000930                             LOCK        MODE         IS  AUTOMATIC.
000940     SELECT  ���Z�v�g�e      ASSIGN      TO        RECEPTL
000950                             ORGANIZATION             IS  INDEXED
000960                             ACCESS MODE              IS  DYNAMIC
000970                             RECORD KEY               IS  ���Z�|�{�p�a��N��
000980                                                          ���Z�|���҃R�[�h
000990                                                          ���Z�|���Z���
001000                             ALTERNATE RECORD KEY     IS  ���Z�|���҃R�[�h
001010                                                          ���Z�|�{�p�a��N��
001020                                                          ���Z�|���Z���
001030                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
001040                                                          ���Z�|�{�p�a��N��
001050                                                          ���Z�|���҃R�[�h
001060                                                          ���Z�|���Z���
001070                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
001080                                                          ���Z�|���Z���
001090                                                          ���Z�|�����ی��Ҕԍ�
001100                                                          ���Z�|���҃R�[�h
001110                                                          ���Z�|�{�p�a��N��
001120                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
001130                                                          ���Z�|�����ی��Ҕԍ�
001140                                                          ���Z�|���҃R�[�h
001150                                                          ���Z�|���Z���
001160                                                          ���Z�|�{�p�a��N��
001170                             FILE STATUS              IS  ��ԃL�[
001180                             LOCK        MODE         IS  AUTOMATIC.
001190     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001200                             ORGANIZATION             IS  INDEXED
001210                             ACCESS                   IS  DYNAMIC
001220                             RECORD      KEY          IS  ��P�|�����a��N��
001230                                                          ��P�|�{�p�a��N��
001240                                                          ��P�|�ی��敪
001250                                                          ��P�|��p���S�Ҕԍ�
001260                                                          ��P�|�ی��Ҕԍ�
001270                                                          ��P�|�{�l�Ƒ��敪
001280                                                          ��P�|���҃J�i
001290                                                          ��P�|���҃R�[�h
001300                             FILE        STATUS       IS  ��ԃL�[
001310                             LOCK        MODE         IS  AUTOMATIC.
001320*
001330******************************************************************
001340*                      DATA DIVISION                             *
001350******************************************************************
001360 DATA                    DIVISION.
001370 FILE                    SECTION.
001380*                           �m�q�k��  �P�Q�W�n
001390 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001400     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001410*                           �m�q�k��  �P�Q�W�n
001420 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001430     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001440*
001450 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
001460     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001470*                           �m�q�k��  �Q�T�U�n
001480 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
001490     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
001500*                           �m�q�k��  �R�Q�O�n
001510 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001520     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001530*                           �m�q�k��  �P�Q�W�n
001540 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001550     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001560*
001570*                          �m�q�k��  �P�T�R�U�n
001580 FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
001590     COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001600*
001610 FD  ��ƃt�@�C���P RECORD  CONTAINS 516 CHARACTERS.
001620 01  ��P�|���R�[�h.
001630     03  ��P�|���R�[�h�w�b�_.
001640         05  ��P�|�����a��N��.
001650             07  ��P�|�����a��            PIC 9.
001660             07  ��P�|�����N              PIC 9(2).
001670             07  ��P�|������              PIC 9(2).
001680         05  ��P�|�ی��敪                PIC 99.
001690         05  ��P�|�ی��Ҕԍ�              PIC X(10).
001700         05  ��P�|�{�l�Ƒ��敪            PIC 9.
001710         05  ��P�|�{�p�a��N��.
001720             07  ��P�|�{�p�a��            PIC 9.
001730             07  ��P�|�{�p�N              PIC 9(2).
001740             07  ��P�|�{�p��              PIC 9(2).
001750         05  ��P�|��ی��҃J�i            PIC X(20).
001760         05  ��P�|���҃R�[�h.
001770             07 ��P�|���Ҕԍ�             PIC 9(6).
001780             07 ��P�|�}��                 PIC X(1).
001790         05  ��P�|�ی����                PIC 9(2).
001800         05  ��P�|�������                PIC 9(2).
001810     03  ��P�|���R�[�h�f�[�^.
001820         05  ��P�|����ԍ�                PIC X(5).
001830         05  ��P�|�V�l����                PIC 99.
001840         05  ��P�|����p.
001850             07  ��P�|��p���S�Ҕԍ�      PIC X(10).
001860             07  ��P�|��v�Ҕԍ�          PIC X(10).
001870         05  ��P�|���Ҏ���.
001880             07  ��P�|���Ҋ���            PIC X(20).
001890             07  ��P�|���҃J�i            PIC X(20).
001900         05  ��P�|���Ґ���                PIC 9.
001910         05  ��P�|���Ґ��N����.
001920             07  ��P�|���Ҙa��            PIC 9(1).
001930             07  ��P�|���ҔN              PIC 9(2).
001940             07  ��P�|���Ҍ�              PIC 9(2).
001950             07  ��P�|���ғ�              PIC 9(2).
001960         05  ��P�|����                    PIC N(2).
001970         05  ��P�|��ی��Ҏ���.
001980             07  ��P�|��ی��Ҋ���        PIC X(20).
001990         05  ��P�|��ی��Ґ���            PIC 9.
002000         05  ��P�|��ی��Ґ��N����.
002010             07  ��P�|��ی��Ҙa��        PIC 9(1).
002020             07  ��P�|��ی��ҔN          PIC 9(2).
002030             07  ��P�|��ی��Ҍ�          PIC 9(2).
002040             07  ��P�|��ی��ғ�          PIC 9(2).
002050         05  ��P�|�L��                    PIC X(24).
002060         05  ��P�|�ԍ�                    PIC X(12).
002070         05  ��P�|���i�擾�N����.
002080             07  ��P�|���i�a��            PIC 9(1).
002090             07  ��P�|���i�N              PIC 9(2).
002100             07  ��P�|���i��              PIC 9(2).
002110             07  ��P�|���i��              PIC 9(2).
002120         05  ��P�|�L���N����.
002130             07  ��P�|�L���a��            PIC 9(1).
002140             07  ��P�|�L���N              PIC 9(2).
002150             07  ��P�|�L����              PIC 9(2).
002160             07  ��P�|�L����              PIC 9(2).
002170         05  ��P�|�������ȕ��S������      PIC 9.
002180         05  ��P�|�]�A�敪����            PIC 9.
002190         05  FILLER                        PIC 9(2).
002200         05  ��P�|�ʉ@��                PIC 9(2).
002210         05  ��P�|���f��.
002220             07  ��P�|���f��{��          PIC 9(4).
002230             07  ��P�|���f���Z��          PIC 9(4).
002240         05  ��P�|���×�.
002250             07  ��P�|���Z                PIC 9.
002260             07  ��P�|��                PIC 9(2).
002270             07  ��P�|���×����z          PIC 9(6).
002280         05  ��P�|�������q���Z.
002290             07  ��P�|�g�p����.
002300                 09  ��P�|��              PIC 9.
002310                 09  ��P�|��              PIC 9.
002320                 09  ��P�|��              PIC 9.
002330             07  ��P�|���z                PIC 9(6).
002340         05  ��P�|���񋟗�              PIC 9(6).
002350         05  ��P�|������                  PIC 9(6).
002360         05  ��P�|��ʔ�p���z.
002370             07  ��P�|��p���v���z        PIC 9(6).
002380             07  ��P�|���S��              PIC 9.
002390             07  ��P�|���S���z            PIC 9(6).
002400             07  ��P�|�������z            PIC 9(6).
002410         05  ��P�|�V�l�������z.
002420             07  ��P�|���ȕ��S���z        PIC 9(6).
002430             07  ��P�|�����������z        PIC 9(6).
002440             07  ��P�|���v�������z        PIC 9(6).
002450         05  ��P�|���ʃf�[�^              OCCURS 5.
002460             07  ��P�|�����R�[�h.
002470                 09  ��P�|���            PIC 9.
002480                 09  ��P�|���ʃR�[�h      PIC 99.
002490             07  ��P�|��㪗Ö@.
002500                 09  ��P�|��㪉�        PIC 99.
002510                 09  ��P�|��㪋��z        PIC 9(6).
002520             07  ��P�|�{�p��              PIC 9(6).
002530             07  ��P�|㪖@��.
002540                 09  ��P�|㪖@��        PIC 99.
002550                 09  ��P�|㪖@���z        PIC 9(6).
002560             07  ��P�|�d�×�.
002570                 09  ��P�|�d�É�        PIC 99.
002580                 09  ��P�|�d�Ë��z        PIC 9(6).
002590             07  ��P�|��×�.
002600                 09  ��P�|��É�        PIC 99.
002610                 09  ��P�|��Ë��z        PIC 9(6).
002620             07  ��P�|�]�A�敪            PIC 9.
002630         05  FILLER                        PIC X(13).
002640*----------------------------------------------------------------*
002650******************************************************************
002660*                WORKING-STORAGE SECTION                         *
002670******************************************************************
002680 WORKING-STORAGE         SECTION.
002690 01 �L�[����                           PIC X    VALUE SPACE.
002700 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
002710 01 �I���t���O                         PIC X(3) VALUE SPACE.
002720 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
002730 01 ���s�L�[�v                         PIC X(3)  VALUE SPACE.
002740 01 �{�p�L�^�L�v                       PIC X(3) VALUE SPACE.
002750 01 �t�@�C����                         PIC N(8) VALUE SPACE.
002760 01 �p���t���O                         PIC X(3) VALUE SPACE.
002770**
002780 01 �ی���ʂv�q                       PIC 9(2) VALUE ZERO.
002790 01 ���҃R�[�h�v�q.
002800    03 ���Ҕԍ��v�q                    PIC 9(6) VALUE ZERO.
002810    03 �}�Ԃv�q                        PIC X    VALUE SPACE.
002820*
002830 01 ����`���v�q                       PIC 9    VALUE ZERO.
002840 01 �ی��Ҕԍ��v�q                     PIC X(10) VALUE SPACE.
002850 01 ���Z�v�g��ނv�q                   PIC X(4) VALUE SPACE.
002860 01 �{�l�Ƒ��敪�v�q                   PIC 9    VALUE ZERO.
002870 01 �����v                             PIC N(2) VALUE SPACE.
002880 01 �{�p�a��N���v�q.
002890    03 �{�p�a��v�q                    PIC 9    VALUE ZERO.
002900    03 �{�p�N�v�q                      PIC 9(2) VALUE ZERO.
002910    03 �{�p���v�q                      PIC 9(2) VALUE ZERO.
002920 01 �����a��N���v�q.
002930    03 �����a��v�q                    PIC 9    VALUE ZERO.
002940    03 �����N�v�q                      PIC 9(2) VALUE ZERO.
002950    03 �������v�q                      PIC 9(2) VALUE ZERO.
002960**
002970 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002980 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
002990 01 �J�E���^�Q                         PIC 9(3)  VALUE ZERO.
003000 01 �J�E���^�R                         PIC 9(2)  VALUE ZERO.
003010 01 �S�p��                           PIC X(2)  VALUE X"8140".
003020 01 ���p��                           PIC X(2)  VALUE X"2020".
003030**
003040 01 ���Ԃv                             PIC 9(4) VALUE ZERO.
003050** �G���[���b�Z�[�W�p
003060 01 �G���[���b�Z�[�W�v.
003070    03 �G���[���҃R�[�h�v              PIC X(7) VALUE SPACE.
003080    03 �G���[��؂�v                  PIC X(1) VALUE SPACE.
003090    03 �G���[�ی���ʂv                PIC X(2) VALUE SPACE.
003100    03 FILLER                          PIC X(10) VALUE SPACE.
003110** �ی��Ҕԍ��E�l�ߗp
003120 01 �ی��Ҕԍ��v�s.
003130    03 �ی��Ҕԍ����l�߂v.
003140      05 �ی��Ҕԍ����l�߂v�P          PIC X OCCURS 8 VALUE SPACE.
003150    03 �ی��Ҕԍ��E�l�߂v.
003160      05 �ی��Ҕԍ��E�l�߂v�P          PIC X OCCURS 8 VALUE ZERO.
003170    03 �ی��Ҕԍ������v                PIC 9(8)  VALUE ZERO.
003180    03 �ی��Ҕԍ��v                    PIC X(8)  VALUE SPACE.
003190** ����ԍ��E�l�ߗp
003200 01 ����ԍ��v�s.
003210    03 ����ԍ����l�߂v.
003220      05 ����ԍ����l�߂v�P            PIC X OCCURS 7 VALUE SPACE.
003230    03 ����ԍ��E�l�߂v.
003240      05 ����ԍ��E�l�߂v�P            PIC X OCCURS 7 VALUE ZERO.
003250    03 ����ԍ������v                  PIC 9(7)  VALUE ZERO.
003260    03 ����ԍ��v                      PIC X(7)  VALUE SPACE.
003270** ������t���[�N�p
003280 01 ����N���v.
003290    03 ����N�v                        PIC 9(4) VALUE ZERO.
003300    03 ����v                        PIC 9(2) VALUE ZERO.
003310** ������N���p
003320 01 ������N���v.
003330    03 ������N�v                    PIC 9(4) VALUE ZERO.
003340    03 ��������v                    PIC 9(2) VALUE ZERO.
003350** ����{�p�N���p
003360 01 ����{�p�N���v.
003370    03 ����{�p�N�v                    PIC 9(4) VALUE ZERO.
003380    03 ����{�p���v                    PIC 9(2) VALUE ZERO.
003390** �L�����l�ߗp
003400 01 �L���v�s.
003410    03 �L�����v.
003420      05 �L�����v�P                    PIC N OCCURS 12 VALUE SPACE.
003430    03 �L�����l�߂v.
003440      05 �L�����l�߂v�P                PIC N OCCURS 12 VALUE SPACE.
003450    03 �L���v.
003460      05 �L���m�v                      PIC N(12) VALUE SPACE.
003470    03 �L���o�v.
003480      05 �L���o�m�v                    PIC X(24) VALUE SPACE.
003490*
003500**********************************************************************************
003510*
003520 01 �ޔ����ڂf�v.
003530   03 ���Z�v�g��ނv                   PIC X(4).
003540   03 ���Z�v�g��ނf�v                 PIC X(4).
003550   03 ���Z�v�g��ʂf�v                 PIC 9(2).
003560*
003570****************
003580* �����f�[�^�e *
003590****************
003600 01 �������v.
003610    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
003620    03 ���ʏ��v  OCCURS   9.
003630       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
003640       05 ���ʃR�[�h�v.
003650          07 ������ʂv                PIC 9(2)  VALUE ZERO.
003660          07 ���ʂv                    PIC 9(2)  VALUE ZERO.
003670       05 �]�A�敪�v                   PIC 9(1)  VALUE ZERO.
003680*    ************
003690*    * ������� *
003700*    ************
003710*    �����̗���
003720***********************
003730 01 �����P�v�q.
003740   03 �����v�q.
003750      05 ���S�����v�q               PIC 9(3)    VALUE ZERO.
003760      05 �������v�q                 PIC 9(5)    VALUE ZERO.
003770      05 �������Z���v�q             PIC 9(5)    VALUE ZERO.
003780   03 �Č����v�q                    PIC 9(5)    VALUE ZERO.
003790   03 ���Âv�q.
003800      05 ���É񐔂v�q               PIC 9(2)    VALUE ZERO.
003810      05 ���×��v�q                 PIC 9(5)    VALUE ZERO.
003820      05 ���É��Z���v�q             PIC 9(5)    VALUE ZERO.
003830   03 �������q���Z���v�q            PIC 9(5)    VALUE ZERO.
003840   03 ��v                          PIC 9(1)    VALUE ZERO.
003850   03 ���v                          PIC 9(1)    VALUE ZERO.
003860   03 ���v                          PIC 9(1)    VALUE ZERO.
003870   03 �{�p���񋟗��v�q            PIC 9(5)    VALUE ZERO.
003880   03 �ꕔ���S���v�q                PIC 9(6)    VALUE ZERO.
003890   03 �������z�v�q                  PIC 9(6)    VALUE ZERO.
003900   03 ���t�����v�q                  PIC 9(1)    VALUE ZERO.
003910   03 �󋋎ҕ��S�z�v�q              PIC 9(6)    VALUE ZERO.
003920   03 �����������z�v�q              PIC 9(6)    VALUE ZERO.
003930*
003940* �������ʖ��̗���
003950***********************
003960 01 �����Q�v�q.
003970   03 ���񏈒u�v�q    OCCURS   9.
003980      05 ���񏈒u���v�q             PIC 9(5)    VALUE ZERO.
003990*
004000* �������̗���
004010***********************
004020 01 �����R�v�q.
004030**********
004040* �P���� *
004050**********
004060   03 ���ʂP�v�q.
004070      05 ��ÂP�v�q.
004080         07 ��É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
004090         07 ��×��P�v�q                PIC 9(5)    VALUE ZERO.
004100      05 ��㪖@�P�v�q.
004110         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
004120         07 ��㪖@���P�v�q              PIC 9(4)    VALUE ZERO.
004130      05 ��㪖@�P�v�q.
004140         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
004150         07 ��㪖@���P�v�q              PIC 9(4)    VALUE ZERO.
004160      05 �d�ÂP�v�q.
004170         07 �d�É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
004180         07 �d�×��P�v�q                PIC 9(4)    VALUE ZERO.
004190**********
004200* �Q���� *
004210**********
004220   03 ���ʂQ�v�q.
004230      05 ��ÂQ�v�q.
004240         07 ��É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
004250         07 ��×��Q�v�q                PIC 9(5)    VALUE ZERO.
004260      05 ��㪖@�Q�v�q.
004270         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
004280         07 ��㪖@���Q�v�q              PIC 9(4)    VALUE ZERO.
004290      05 ��㪖@�Q�v�q.
004300         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
004310         07 ��㪖@���Q�v�q              PIC 9(4)    VALUE ZERO.
004320      05 �d�ÂQ�v�q.
004330         07 �d�É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
004340         07 �d�×��Q�v�q                PIC 9(4)    VALUE ZERO.
004350******************
004360* �R���ʁ^�W�� *
004370******************
004380   03 ���ʂR�W�v�q.
004390      05 ��ÂR�W�v�q.
004400         07 ��É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
004410         07 ��×��R�W�v�q                PIC 9(5)  VALUE ZERO.
004420      05 ��㪖@�R�W�v�q.
004430         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
004440         07 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
004450      05 ��㪖@�R�W�v�q.
004460         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
004470         07 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
004480      05 �d�ÂR�W�v�q.
004490         07 �d�É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
004500         07 �d�×��R�W�v�q                PIC 9(4)  VALUE ZERO.
004510******************
004520* �R���ʁ^�P�O�� *
004530******************
004540   03 ���ʂR�O�v�q.
004550      05 ��ÂR�O�v�q.
004560         07 ��É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
004570         07 ��×��R�O�v�q                PIC 9(5)  VALUE ZERO.
004580      05 ��㪖@�R�O�v�q.
004590         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
004600         07 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
004610      05 ��㪖@�R�O�v�q.
004620         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
004630         07 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
004640      05 �d�ÂR�O�v�q.
004650         07 �d�É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
004660         07 �d�×��R�O�v�q                PIC 9(4)  VALUE ZERO.
004670******************
004680* �R���ʁ^���v�@ *
004690******************
004700   03 ���ʂR�v�q.
004710      05 ��ÂR�v�q.
004720         07 ��É񐔂R�v�q                PIC 9(2)  VALUE ZERO.
004730         07 ��×��R�v�q                  PIC 9(6)  VALUE ZERO.
004740      05 ��㪖@�R�v�q.
004750         07 ��㪖@�񐔂R�v�q              PIC 9(2)  VALUE ZERO.
004760         07 ��㪖@���R�v�q                PIC 9(6)  VALUE ZERO.
004770      05 ��㪖@�R�v�q.
004780         07 ��㪖@�񐔂R�v�q              PIC 9(2)  VALUE ZERO.
004790         07 ��㪖@���R�v�q                PIC 9(6)  VALUE ZERO.
004800      05 �d�ÂR�v�q.
004810         07 �d�É񐔂R�v�q                PIC 9(2)  VALUE ZERO.
004820         07 �d�×��R�v�q                  PIC 9(6)  VALUE ZERO.
004830****************
004840* �S���ʁ^�T�� *
004850****************
004860   03 ���ʂS�T�v�q.
004870      05 ��ÂS�T�v�q.
004880         07 ��É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
004890         07 ��×��S�T�v�q                PIC 9(5)  VALUE ZERO.
004900      05 ��㪖@�S�T�v�q.
004910         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
004920         07 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
004930      05 ��㪖@�S�T�v�q.
004940         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
004950         07 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
004960      05 �d�ÂS�T�v�q.
004970         07 �d�É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
004980         07 �d�×��S�T�v�q                PIC 9(4)  VALUE ZERO.
004990****************
005000* �S���ʁ^�W�� *
005010****************
005020   03 ���ʂS�W�v�q.
005030      05 ��ÂS�W�v�q.
005040         07 ��É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
005050         07 ��×��S�W�v�q                PIC 9(5)  VALUE ZERO.
005060      05 ��㪖@�S�W�v�q.
005070         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
005080         07 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
005090      05 ��㪖@�S�W�v�q.
005100         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
005110         07 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
005120      05 �d�ÂS�W�v�q.
005130         07 �d�É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
005140         07 �d�×��S�W�v�q                PIC 9(4)  VALUE ZERO.
005150******************
005160* �S���ʁ^�P�O�� *
005170******************
005180   03 ���ʂS�O�v�q.
005190      05 ��ÂS�O�v�q.
005200         07 ��É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
005210         07 ��×��S�O�v�q                PIC 9(5)  VALUE ZERO.
005220      05 ��㪖@�S�O�v�q.
005230         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
005240         07 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
005250      05 ��㪖@�S�O�v�q.
005260         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
005270         07 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
005280      05 �d�ÂS�O�v�q.
005290         07 �d�É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
005300         07 �d�×��S�O�v�q                PIC 9(4)  VALUE ZERO.
005310******************
005320* �S���ʁ^���v�@ *
005330******************
005340   03 ���ʂS�v�q.
005350      05 ��ÂS�v�q.
005360         07 ��É񐔂S�v�q                PIC 9(2)  VALUE ZERO.
005370         07 ��×��S�v�q                  PIC 9(6)  VALUE ZERO.
005380      05 ��㪖@�S�v�q.
005390         07 ��㪖@�񐔂S�v�q              PIC 9(2)  VALUE ZERO.
005400         07 ��㪖@���S�v�q                PIC 9(6)  VALUE ZERO.
005410      05 ��㪖@�S�v�q.
005420         07 ��㪖@�񐔂S�v�q              PIC 9(2)  VALUE ZERO.
005430         07 ��㪖@���S�v�q                PIC 9(6)  VALUE ZERO.
005440      05 �d�ÂS�v�q.
005450         07 �d�É񐔂S�v�q                PIC 9(2)  VALUE ZERO.
005460         07 �d�×��S�v�q                  PIC 9(6)  VALUE ZERO.
005470********************
005480* �T���ʁ^�Q�D�T�� *
005490********************
005500   03 ���ʂT�Q�v�q.
005510      05 ��ÂT�Q�v�q.
005520         07 ��É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
005530         07 ��×��T�Q�v�q                PIC 9(5)  VALUE ZERO.
005540      05 ��㪖@�T�Q�v�q.
005550         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
005560         07 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
005570      05 ��㪖@�T�Q�v�q.
005580         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
005590         07 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
005600      05 �d�ÂT�Q�v�q.
005610         07 �d�É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
005620         07 �d�×��T�Q�v�q                PIC 9(4)  VALUE ZERO.
005630****************
005640* �T���ʁ^�T�� *
005650****************
005660   03 ���ʂT�T�v�q.
005670      05 ��ÂT�T�v�q.
005680         07 ��É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
005690         07 ��×��T�T�v�q                PIC 9(5)  VALUE ZERO.
005700      05 ��㪖@�T�T�v�q.
005710         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
005720         07 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
005730      05 ��㪖@�T�T�v�q.
005740         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
005750         07 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
005760      05 �d�ÂT�T�v�q.
005770         07 �d�É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
005780         07 �d�×��T�T�v�q                PIC 9(4)  VALUE ZERO.
005790****************
005800* �T���ʁ^�W�� *
005810****************
005820   03 ���ʂT�W�v�q.
005830      05 ��ÂT�W�v�q.
005840         07 ��É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
005850         07 ��×��T�W�v�q                PIC 9(5)  VALUE ZERO.
005860      05 ��㪖@�T�W�v�q.
005870         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
005880         07 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
005890      05 ��㪖@�T�W�v�q.
005900         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
005910         07 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
005920      05 �d�ÂT�W�v�q.
005930         07 �d�É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
005940         07 �d�×��T�W�v�q                PIC 9(4)  VALUE ZERO.
005950******************
005960* �T���ʁ^�P�O�� *
005970******************
005980   03 ���ʂT�O�v�q.
005990      05 ��ÂT�O�v�q.
006000         07 ��É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
006010         07 ��×��T�O�v�q                PIC 9(5)  VALUE ZERO.
006020      05 ��㪖@�T�O�v�q.
006030         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
006040         07 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
006050      05 ��㪖@�T�O�v�q.
006060         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
006070         07 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
006080      05 �d�ÂT�O�v�q.
006090         07 �d�É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
006100         07 �d�×��T�O�v�q                PIC 9(4)  VALUE ZERO.
006110******************
006120* �T���ʁ^���v�@ *
006130******************
006140   03 ���ʂT�v�q.
006150      05 ��ÂT�v�q.
006160         07 ��É񐔂T�v�q                PIC 9(2)  VALUE ZERO.
006170         07 ��×��T�v�q                  PIC 9(6)  VALUE ZERO.
006180      05 ��㪖@�T�v�q.
006190         07 ��㪖@�񐔂T�v�q              PIC 9(2)  VALUE ZERO.
006200         07 ��㪖@���T�v�q                PIC 9(6)  VALUE ZERO.
006210      05 ��㪖@�T�v�q.
006220         07 ��㪖@�񐔂T�v�q              PIC 9(2)  VALUE ZERO.
006230         07 ��㪖@���T�v�q                PIC 9(6)  VALUE ZERO.
006240      05 �d�ÂT�v�q.
006250         07 �d�É񐔂T�v�q                PIC 9(2)  VALUE ZERO.
006260         07 �d�×��T�v�q                  PIC 9(6)  VALUE ZERO.
006270*
006280*****************************************************************
006290 01 �v�Z�@����N�v                     PIC 9(2).
006300* ���t�v�n�q�j
006310 01 �v�Z�@����.
006320    03 �v�Z�@����N                    PIC 9(4).
006330    03 �v�Z�@�����                  PIC 9(4).
006340 01 �v�Z�@����q REDEFINES �v�Z�@����.
006350    03 �v�Z�@���I                      PIC 9(2).
006360    03 �v�Z�@���t                      PIC 9(6).
006370    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
006380       05 �v�Z�@�N��                   PIC 9(4).
006390       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
006400         07 �v�Z�@�N                   PIC 9(2).
006410         07 �v�Z�@��                   PIC 9(2).
006420       05 �v�Z�@��                     PIC 9(2).
006430*
006440*
006450 01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
006460 01 �v���O�������v         PIC X(8) VALUE SPACE.
006470*
006480******************************************************************
006490*                          �A������                              *
006500******************************************************************
006510*
006520********************
006530* ���b�Z�[�W�\���L�[ *
006540********************
006550 01 �A���|�L�[ IS EXTERNAL.
006560    03  �A���|���b�Z�[�W               PIC N(20).
006570*
006580 01 �A���R�|�L�[ IS EXTERNAL.
006590    03  �A���R�|���b�Z�[�W             PIC N(20).
006600    03  �A���R�|���b�Z�[�W�P           PIC X(20).
006610****************
006620* ��ʓ��͏�� *
006630****************
006640 01 �A���|��ʏ��x�`�r�T�W�O IS EXTERNAL.
006650    03 �A���|�����a��N��.
006660       05 �A���|�����a��               PIC 9.
006670       05 �A���|�����N��.
006680         07 �A���|�����N               PIC 9(2).
006690         07 �A���|������               PIC 9(2).
006700*
006710 01 �A���|�L�[ IS EXTERNAL.
006720    03 �A���|�ی����                  PIC 9(2).
006730*
006740*******************************
006750* ���S���擾�p �o�f�̑Ή�1410 *
006760*******************************
006770 01 �A���|���S���擾�L�[ IS EXTERNAL.
006780    03 �A���|�{�p�a��N��.
006790       05 �A���|�{�p�a��             PIC 9.
006800       05 �A���|�{�p�N��.
006810          07 �A���|�{�p�N            PIC 9(2).
006820          07 �A���|�{�p��            PIC 9(2).
006830    03 �A���|���҃R�[�h.
006840       05 �A���|���Ҕԍ�             PIC 9(6).
006850       05 �A���|�}��                 PIC X.
006860    03 �A���|���ە��S��              PIC 9(3).
006870    03 �A���|���ۖ{�̕��S��          PIC 9(3).
006880    03 �A���|���ە��S��              PIC 9(3).
006890    03 �A���|�Q�V�V���S��            PIC 9(3).
006900    03 �A���|�������S��              PIC 9(3).
006910    03 �A���|���ʗp���S��            PIC 9(3).
006920*
006930** �Í������p
006940 01 �A�Í������|�Í���� IS EXTERNAL.
006950    03 �A�Í������|���͏��.
006960       05 �A�Í������|�L��               PIC X(24).
006970       05 �A�Í������|�ԍ�               PIC X(30).
006980       05 �A�Í������|�Í�������.
006990         07 �A�Í������|�Í����Ҕԍ�     PIC X(6).
007000         07 �A�Í������|�Í�����L��     PIC X.
007010         07 �A�Í������|�Í�����ԍ�     PIC X.
007020         07 �A�Í������|�Í��L��         PIC X(24).
007030         07 �A�Í������|�Í��ԍ�         PIC X(30).
007040    03 �A�Í������|�o�͏��.
007050       05 �A�Í������|���������L��       PIC X(24).
007060       05 �A�Í������|���������ԍ�       PIC X(30).
007070*
007080******************************************************************
007090*                      PROCEDURE  DIVISION                       *
007100******************************************************************
007110 PROCEDURE               DIVISION.
007120************
007130*           *
007140* ��������   *
007150*           *
007160************
007170     PERFORM ������.
007180     PERFORM �{�p�����擾.
007190************
007200*           *
007210* �又��     *
007220*           *
007230************
007240     PERFORM ��ƃt�@�C���쐬.
007250************
007260*           *
007270* �I������   *
007280*           *
007290************
007300     PERFORM �I������.
007310     MOVE ZERO TO PROGRAM-STATUS.
007320     EXIT PROGRAM.
007330*
007340*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
007350*================================================================*
007360 ������ SECTION.
007370*
007380     PERFORM �t�@�C���I�[�v��.
007390* �A�����ڂ̑Ҕ�
007400     MOVE �A���|�����a��      TO �����a��v�q.
007410     MOVE �A���|�����N        TO �����N�v�q.
007420     MOVE �A���|������        TO �������v�q.
007430*
007440*================================================================*
007450 �t�@�C���I�[�v�� SECTION.
007460*
007470     OPEN INPUT �����}�X�^.
007480         MOVE NC"�����}�X�^" TO �t�@�C����.
007490         PERFORM �I�[�v���`�F�b�N.
007500     OPEN INPUT ���̃}�X�^.
007510         MOVE NC"���̃}�X�^" TO �t�@�C����.
007520         PERFORM �I�[�v���`�F�b�N.
007530     OPEN INPUT �{�p�����}�X�^
007540         MOVE NC"�{��" TO �t�@�C����.
007550         PERFORM �I�[�v���`�F�b�N.
007560     OPEN INPUT �{�p�L�^�e.
007570         MOVE NC"�{�p�L�^�e" TO �t�@�C����.
007580         PERFORM �I�[�v���`�F�b�N.
007590     OPEN INPUT ��f�ҏ��e.
007600         MOVE NC"��f�ҏ��e" TO �t�@�C����.
007610         PERFORM �I�[�v���`�F�b�N.
007620     OPEN INPUT �����f�[�^�e.
007630         MOVE NC"�����f�[�^�e" TO �t�@�C����.
007640         PERFORM �I�[�v���`�F�b�N.
007650     OPEN INPUT ���Z�v�g�e.
007660         MOVE NC"���Z" TO �t�@�C����.
007670         PERFORM �I�[�v���`�F�b�N.
007680*================================================================*
007690 �I�[�v���`�F�b�N SECTION.
007700*
007710     IF ��ԃL�[  NOT =  "00"
007720         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
007730         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
007740         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
007750                                                 UPON CONS
007760         ACCEPT  �L�[���� FROM CONS
007770         PERFORM �t�@�C����
007780         MOVE 99 TO PROGRAM-STATUS
007790         EXIT PROGRAM.
007800*================================================================*
007810 �t�@�C���� SECTION.
007820*
007830     CLOSE �����}�X�^ ���̃}�X�^ ��f�ҏ��e �����f�[�^�e
007840           �{�p�L�^�e �{�p�����}�X�^ ���Z�v�g�e.
007850*================================================================*
007860 �I������ SECTION.
007870*
007880     PERFORM �t�@�C����.
007890*================================================================*
007900 �G���[�\�� SECTION.
007910*
007920     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
007930     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
007940     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
007950     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
007960     ACCEPT  �L�[���� FROM CONS.
007970     PERFORM �t�@�C����.
007980     MOVE 99 TO PROGRAM-STATUS.
007990     EXIT PROGRAM.
008000*================================================================*
008010 �{�p�����擾 SECTION.
008020*
008030     MOVE ZERO  TO �{��|�{�p���ԍ�.
008040     READ �{�p�����}�X�^
008050     INVALID KEY
008060          MOVE  NC"�{�p�����}�X�^�ɓo�^��A���s���ĉ�����" TO �A���|���b�Z�[�W
008070          CALL   "MSG001"
008080          CANCEL "MSG001"
008090          PERFORM �t�@�C����
008100          MOVE 99 TO PROGRAM-STATUS
008110          EXIT PROGRAM
008120     NOT INVALID KEY
008130          IF  �{��|�ڍ��t�����ԍ� = SPACE
008140              MOVE  NC"�{�p���}�X�^�ɉ���ԍ���o�^���ĉ�����" TO �A���|���b�Z�[�W
008150              CALL   "MSG001"
008160              CANCEL "MSG001"
008170              PERFORM �t�@�C����
008180              MOVE 99 TO PROGRAM-STATUS
008190              EXIT PROGRAM
008200          ELSE
008210              MOVE �{��|�ڍ��t�����ԍ�  TO ����ԍ��v
008220****              PERFORM ����ԍ��E�l��
008230          END-IF
008240     END-READ.
008250*
008260*================================================================*
008270 ��ƃt�@�C���쐬 SECTION.
008280*
008290     OPEN OUTPUT ��ƃt�@�C���P.
008300         MOVE NC"��P" TO �t�@�C����.
008310         PERFORM �I�[�v���`�F�b�N.
008320*
008330     PERFORM ��ƃt�@�C���P�쐬�P
008340*
008350     CLOSE ��ƃt�@�C���P.
008360*
008370*================================================================*
008380 ��ƃt�@�C���P�쐬�P SECTION.
008390*
008400*********************
008410*       �S�o��      *
008420*********************
008430*
008440     MOVE �����a��v�q  TO ���Z�|�����a��.
008450     MOVE �����N�v�q    TO ���Z�|�����N.
008460     MOVE �������v�q    TO ���Z�|������.
008470     MOVE ZERO          TO ���Z�|���Z���.
008480     MOVE ZERO          TO ���Z�|�{�p�a��.
008490     MOVE ZERO          TO ���Z�|�{�p�N.
008500     MOVE ZERO          TO ���Z�|�{�p��.
008510     MOVE ZERO          TO ���Z�|���Ҕԍ�.
008520     MOVE SPACE         TO ���Z�|�}��.
008530     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
008540                                  ���Z�|�{�p�a��N��
008550                                  ���Z�|���҃R�[�h
008560                                  ���Z�|���Z���
008570     END-START.
008580     IF ��ԃL�[ = "00"
008590         MOVE SPACE  TO �I���t���O
008600         PERFORM ���Z�v�g�e�Ǎ�
008610         PERFORM UNTIL ( �I���t���O = "YES" ) OR
008620                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
008630                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
008640                       ( ���Z�|������   NOT = �������v�q   )
008650            PERFORM �f�[�^�`�F�b�N
008660**
008670** ���R�͑ΏۊO
008680            IF  ��|�ی���� = 90
008690                MOVE SPACE  TO ���s�L�[�v
008700            END-IF
008710** ///
008720** �Ƃ肠���� �J�ЁE�����ӂ͑ΏۊO (���Z�v�g���e���Ⴄ��)
008730            IF  ��|�ی���� = 70 OR 80
008740                MOVE SPACE  TO ���s�L�[�v
008750            END-IF
008760** ///
008770            IF  ���s�L�[�v = "YES"
008780**
008790*            ********
008800*            * ���� *
008810*            ********
008820                 IF ( ��|�ی����   NOT = ZERO ) AND
008830                    ( ��|�ی��Ҕԍ� NOT = SPACE )
008840*                **********************
008850*                * ��ƃt�@�C���쐬 *
008860*                **********************
008870                     IF ( ��|������       = ZERO  ) AND
008880                        ( ��|��p���S�Ҕԍ� = SPACE )
008890** ���ےP�Ƃ͑ΏۊO (���ېe����Ə������̑��́A�e�̂ݏ����Ȃ��ŏo��)
008900*/60���̑����������R�[�h�̑ΏۂɕύX/101109
008910*                         IF  ��|�������   = ZERO  OR 50 OR 60
008920                         IF  ��|�������   = ZERO  OR 50
008930                             PERFORM ��P���R�[�h�Z�b�g����
008940                         ELSE
008950                             PERFORM ��P���R�[�h�Z�b�g���ۏ���
008960                         END-IF
008970* ���������鎞�o��
008980                         IF ( ��P�|�������z     = ZERO ) AND
008990                            ( ��P�|�����������z = ZERO )
009000                             CONTINUE
009010                         ELSE
009020                             PERFORM ��P�t�@�C������
009030                         END-IF
009040                     END-IF
009050                 END-IF
009060*            ********
009070*            * �V�l *
009080*            ********
009090                 IF ( ��|������       NOT = ZERO ) AND
009100                    ( ��|��p���S�Ҕԍ� NOT = SPACE )
009110*                    **********************
009120*                    * ��ƃt�@�C���쐬 *
009130*                    **********************
009140**
009150                         IF ��|�{�p�a��N�� < 42004
009160                             PERFORM ��P���R�[�h�Z�b�g�V�l
009170                         ELSE
009180*/60���̑����������R�[�h�̑ΏۂɕύX/101109
009190*                             IF  ��|�������   = ZERO  OR 50 OR 60
009200                             IF  ��|�������   = ZERO  OR 50
009210                                 PERFORM ��P���R�[�h�Z�b�g�㍂
009220                             ELSE
009230                                 PERFORM ��P���R�[�h�Z�b�g�㍂����
009240                             END-IF
009250                         END-IF
009260* ���������鎞�o��
009270                         IF ( ��P�|�������z     = ZERO ) AND
009280                            ( ��P�|�����������z = ZERO )
009290                             CONTINUE
009300                         ELSE
009310                             PERFORM ��P�t�@�C������
009320                         END-IF
009330                 END-IF
009340             END-IF
009350             PERFORM ���Z�v�g�e�Ǎ�
009360         END-PERFORM
009370     END-IF.
009380*
009390*================================================================*
009400 �f�[�^�`�F�b�N SECTION.
009410*
009420     MOVE SPACE          TO ���s�L�[�v.
009430* *****************************************************************
009440* * ���Z�v�g�e�̐����Ώۋ敪 = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
009450* *****************************************************************
009460
009470     IF ( ���Z�|�����Ώۋ敪 NOT = ZERO ) AND
009480        ( ���Z�|���ҕ����敪 NOT = 2 OR 3 )
009490*        IF(���Z�|���Z��� = 3) AND ( ���Z�|����\����Ώۋ敪 = 1 )
009500        IF(���Z�|���Z��� = 3)
009510           CONTINUE
009520        ELSE
009530           MOVE ���Z�|�{�p�a��  TO ��|�{�p�a��
009540           MOVE ���Z�|�{�p�N    TO ��|�{�p�N
009550           MOVE ���Z�|�{�p��    TO ��|�{�p��
009560           MOVE ���Z�|���Ҕԍ�  TO ��|���Ҕԍ�
009570           MOVE ���Z�|�}��      TO ��|�}��
009580           READ ��f�ҏ��e
009590           NOT INVALID KEY
009600**      ���ۂ̂�
009610              IF ��|�ی����� = 1
009620                 MOVE "YES"  TO ���s�L�[�v
009630                 MOVE ��|�{�p�a��       TO ���|�{�p�a��
009640                 MOVE ��|�{�p�N         TO ���|�{�p�N
009650                 MOVE ��|�{�p��         TO ���|�{�p��
009660                 MOVE ��|���҃R�[�h     TO ���|���҃R�[�h
009670                 READ �����f�[�^�e
009680                 END-READ
009690              END-IF
009700           END-READ
009710        END-IF
009720     END-IF.
009730*
009740*================================================================*
009750 ���Z�v�g�e�Ǎ� SECTION.
009760*
009770     READ ���Z�v�g�e NEXT
009780     AT END
009790         MOVE "YES" TO �I���t���O
009800     END-READ.
009810*
009820*================================================================*
009830 ��f�ҏ��e�Ǎ� SECTION.
009840*
009850     READ ��f�ҏ��e NEXT
009860     AT END
009870         MOVE "YES" TO �I���t���O
009880     END-READ.
009890*
009900*================================================================*
009910 �{�p�L�^�e�Ǎ� SECTION.
009920*
009930     READ �{�p�L�^�e NEXT
009940     AT END
009950         MOVE "YES"  TO �I���t���O�Q
009960     END-READ.
009970*================================================================*
009980*================================================================*
009990 ��P���R�[�h�Z�b�g���� SECTION.
010000*
010010**********/  ���ۏ����Ȃ��̎�  /**********
010020*
010030     MOVE SPACE TO ��P�|���R�[�h.
010040     INITIALIZE ��P�|���R�[�h.
010050*
010060     MOVE ��|�ی����       TO ��P�|�ی����.
010070     MOVE ZERO               TO ��P�|�������.
010080*
010090     MOVE ���Z�|���Z������   TO ��P�|�ʉ@��.
010100     MOVE ���Z�|���v         TO ��P�|��p���v���z.
010110     MOVE ���Z�|�ꕔ���S��   TO ��P�|���S���z.
010120     MOVE ���Z�|�������z     TO ��P�|�������z.
010130     MOVE ZERO               TO ��P�|�V�l�������z.
010140*
010150*    ���S���̐ݒ�
010160     PERFORM ���S�����擾.
010170     COMPUTE ��P�|���S��  = �A���|���ۖ{�̕��S�� / 10.
010180*
010190* �A�v �]�L�̌�
010200     PERFORM ���ʃ��R�[�h�Z�b�g.
010210*
010220*================================================================*
010230 ��P���R�[�h�Z�b�g�V�l SECTION.
010240*
010250**********/ 27�V�l�̎�  /**********
010260*
010270     MOVE SPACE TO ��P�|���R�[�h.
010280     INITIALIZE ��P�|���R�[�h.
010290*
010300     MOVE ��|������       TO ��P�|�ی����.
010310     MOVE ��|�������       TO ��P�|�������.
010320*
010330     MOVE ��|��p���S�Ҕԍ� TO ��P�|��p���S�Ҕԍ�.
010340     MOVE ��|��v�Ҕԍ��V�l TO ��P�|��v�Ҕԍ�.
010350*
010360     MOVE ��|�V�l���S���Ə� TO ��P�|�������ȕ��S������.
010370*
010380     MOVE ���Z�|���Z������   TO ��P�|�ʉ@��.
010390     MOVE ���Z�|���v         TO ��P�|��p���v���z.
010400     MOVE ���Z�|�ꕔ���S��   TO ��P�|���S���z.
010410     MOVE ���Z�|�������z     TO ��P�|�������z.
010420     MOVE ZERO               TO ��P�|�V�l�������z.
010430*
010440     MOVE ZERO               TO ��P�|���S��.
010450*
010460* �A�v �]�L�̌�
010470     PERFORM ���ʃ��R�[�h�Z�b�g.
010480*
010490** 15/01 27�����Ή��i��L�]�L��A�u�����j
010500     IF ��|������� = 53 OR 54
010510         MOVE ��|��p���S�Ҕԍ�����  TO ��P�|�ی��Ҕԍ�
010520         EVALUATE ��|�������
010530* �g��
010540         WHEN 53
010550             MOVE 14   TO ��P�|�V�l����
010560* �픚
010570         WHEN 54
010580             MOVE 15   TO ��P�|�V�l����
010590         END-EVALUATE
010600* �������z
010610         MOVE ���Z�|�󋋎ҕ��S�z   TO ��P�|���ȕ��S���z
010620         MOVE ���Z�|�����������z   TO ��P�|�����������z
010630         COMPUTE ��P�|���v�������z = ��P�|���ȕ��S���z + ��P�|�����������z
010640*
010650     END-IF.
010660**
010670*================================================================*
010680 ��P���R�[�h�Z�b�g�㍂ SECTION.
010690*
010700**********/  �㍂�����Ȃ��̎�  /**********
010710*
010720     MOVE SPACE TO ��P�|���R�[�h.
010730     INITIALIZE ��P�|���R�[�h.
010740*
010750     MOVE ��|�ی����       TO ��P�|�ی����.
010760     MOVE ZERO               TO ��P�|�������.
010770*
010780     MOVE ���Z�|���Z������   TO ��P�|�ʉ@��.
010790     MOVE ���Z�|���v         TO ��P�|��p���v���z.
010800     MOVE ���Z�|�ꕔ���S��   TO ��P�|���S���z.
010810     MOVE ���Z�|�������z     TO ��P�|�������z.
010820     MOVE ZERO               TO ��P�|�V�l�������z.
010830*
010840*    ���_�Ɠ����d�l�ɂ��邽�߁A�����ă[�����Z�b�g�B�{���͂P���R���Ȃǂ��݂��ׂ�
010850     MOVE ZERO               TO  ��P�|���S��.
010860*
010870* �A�v �]�L�̌�
010880     PERFORM ���ʃ��R�[�h�Z�b�g.
010890*
010900*================================================================*
010910 ��P���R�[�h�Z�b�g���ۏ��� SECTION.
010920*
010930**********/  ���ۏ�������̎�  /**********
010940*
010950     MOVE SPACE TO ��P�|���R�[�h.
010960     INITIALIZE ��P�|���R�[�h.
010970*
010980     MOVE ��|�ی����       TO ��P�|�ی����.
010990     MOVE ��|�������       TO ��P�|�������.
011000*
011010* -- 99XXXXXX �ł��]�L���� ---
011020     MOVE ��|��p���S�Ҕԍ�����  TO ��P�|��p���S�Ҕԍ�.
011030*
011040     IF ( ��|��v�Ҕԍ�����(1:1) = "*"  ) OR
011050        ( ��|��v�Ҕԍ�����(1:2) = "��" )
011060        MOVE SPACE                TO ��P�|��v�Ҕԍ�
011070     ELSE
011080        MOVE ��|��v�Ҕԍ�����   TO ��P�|��v�Ҕԍ�
011090     END-IF.
011100*
011110**     MOVE ��|�������S���Ə� TO ��P�|�������ȕ��S������.
011120     MOVE ��|�V�l���S���Ə� TO ��P�|�������ȕ��S������.
011130*
011140*
011150     MOVE ���Z�|���Z������   TO ��P�|�ʉ@��.
011160     MOVE ���Z�|���v         TO ��P�|��p���v���z.
011170     MOVE ���Z�|�ꕔ���S��   TO ��P�|���S���z.
011180     MOVE ���Z�|�������z     TO ��P�|�������z.
011190*
011200*    ���S���̐ݒ�
011210     PERFORM ���S�����擾.
011220     COMPUTE ��P�|���S��  = �A���|���ۖ{�̕��S�� / 10.
011230*
011240* �������z
011250     MOVE ���Z�|�󋋎ҕ��S�z   TO ��P�|���ȕ��S���z.
011260     MOVE ���Z�|�����������z   TO ��P�|�����������z.
011270     COMPUTE ��P�|���v�������z = ��P�|���ȕ��S���z + ��P�|�����������z.
011280*
011290* �A�v �]�L�̌�
011300     PERFORM ���ʃ��R�[�h�Z�b�g.
011310*
011320*================================================================*
011330 ��P���R�[�h�Z�b�g�㍂���� SECTION.
011340*
011350**********/  �㍂��������̎�  /**********
011360*
011370     MOVE SPACE TO ��P�|���R�[�h.
011380     INITIALIZE ��P�|���R�[�h.
011390*
011400     MOVE ��|�ی����       TO ��P�|�ی����.
011410     MOVE ��|�������       TO ��P�|�������.
011420*
011430* -- 99XXXXXX �ł��]�L���� ---
011440     MOVE ��|��p���S�Ҕԍ�����  TO ��P�|��p���S�Ҕԍ�.
011450*
011460     IF ( ��|��v�Ҕԍ�����(1:1) = "*"  ) OR
011470        ( ��|��v�Ҕԍ�����(1:2) = "��" )
011480        MOVE SPACE                TO ��P�|��v�Ҕԍ�
011490     ELSE
011500        MOVE ��|��v�Ҕԍ�����   TO ��P�|��v�Ҕԍ�
011510     END-IF.
011520*
011530**     MOVE ��|�������S���Ə� TO ��P�|�������ȕ��S������.
011540*     MOVE ��|�V�l���S���Ə� TO ��P�|�������ȕ��S������.
011550*
011560*
011570     MOVE ���Z�|���Z������   TO ��P�|�ʉ@��.
011580     MOVE ���Z�|���v         TO ��P�|��p���v���z.
011590     MOVE ���Z�|�ꕔ���S��   TO ��P�|���S���z.
011600     MOVE ���Z�|�������z     TO ��P�|�������z.
011610*
011620*    ���_�Ɠ����d�l�ɂ��邽�߁A�����ă[�����Z�b�g�B�{���͂P���R���Ȃǂ��݂��ׂ�
011630     MOVE ZERO               TO ��P�|���S��.
011640*
011650* �������z
011660     MOVE ���Z�|�󋋎ҕ��S�z   TO ��P�|���ȕ��S���z.
011670     MOVE ���Z�|�����������z   TO ��P�|�����������z.
011680     COMPUTE ��P�|���v�������z = ��P�|���ȕ��S���z + ��P�|�����������z.
011690*
011700* �A�v �]�L�̌�
011710     PERFORM ���ʃ��R�[�h�Z�b�g.
011720*================================================================*
011730 ���ʃ��R�[�h�Z�b�g SECTION.
011740*
011750     MOVE ���Z�|�����a��     TO ��P�|�����a��.
011760     MOVE ���Z�|�����N       TO ��P�|�����N.
011770     MOVE ���Z�|������       TO ��P�|������.
011780     MOVE ��|�{�p�a��       TO ��P�|�{�p�a�� �{�p�a��v�q.
011790     MOVE ��|�{�p�N         TO ��P�|�{�p�N �{�p�N�v�q.
011800     MOVE ��|�{�p��         TO ��P�|�{�p�� �{�p���v�q.
011810     MOVE ��|���҃R�[�h     TO ��P�|���҃R�[�h ���҃R�[�h�v�q.
011820****     MOVE ����ԍ������v     TO ��P�|����ԍ�.
011830     MOVE ����ԍ��v         TO ��P�|����ԍ�.
011840*
011850     PERFORM �ی��敪�擾.
011860*
011870     MOVE ��|�ی��Ҕԍ�     TO ��P�|�ی��Ҕԍ�.
011880     MOVE ��|���Ҏ���       TO ��P�|���Ҏ���.
011890     MOVE ��|���҃J�i       TO ��P�|���҃J�i.
011900     MOVE ��|���Ґ���       TO ��P�|���Ґ���.
011910     MOVE ��|���Ґ��N����   TO ��P�|���Ґ��N����.
011920** �a�� �����Ə��a���t�ɂ���B
011930     EVALUATE ��P�|���Ҙa��
011940     WHEN 1
011950        MOVE  3  TO ��P�|���Ҙa��
011960     WHEN 3
011970        MOVE  1  TO ��P�|���Ҙa��
011980     END-EVALUATE.
011990**
012000     MOVE ��|���i�擾�N���� TO ��P�|���i�擾�N����.
012010     MOVE ��|�L���N����     TO ��P�|�L���N����.
012020     MOVE ��|�{�l�Ƒ��敪   TO ��P�|�{�l�Ƒ��敪.
012030     IF ��|�{�l�Ƒ��敪 = 1
012040         MOVE NC"�{�l"    TO �����v
012050     ELSE
012060         MOVE 05          TO ���|�敪�R�[�h
012070         MOVE ��|����    TO ���|���̃R�[�h
012080         READ ���̃}�X�^
012090         INVALID KEY
012100             MOVE SPACE     TO �����v
012110         NOT INVALID KEY
012120             MOVE ���|����  TO �����v
012130         END-READ
012140     END-IF.
012150     MOVE �����v             TO ��P�|����.
012160*
012170     MOVE ��|��ی��Ҏ���   TO ��P�|��ی��Ҏ���.
012180     MOVE ��|��ی��҃J�i   TO ��P�|��ی��҃J�i.
012190     MOVE ��|��ی��Ґ���   TO ��P�|��ی��Ґ���.
012200     MOVE ��|��ی��Ґ��N����   TO ��P�|��ی��Ґ��N����.
012210** �a�� �����Ə��a���t�ɂ���B
012220     EVALUATE ��P�|��ی��Ҙa��
012230     WHEN 1
012240        MOVE  3  TO ��P�|��ی��Ҙa��
012250     WHEN 3
012260        MOVE  1  TO ��P�|��ی��Ҙa��
012270     END-EVALUATE.
012280**
012290*-----------------------------------------------------------------*
012300     MOVE SPACE TO �A�Í������|�Í����.
012310*
012320*    / �A�Í������|���͏��Z�b�g /
012330     MOVE ��|�L��       TO �A�Í������|�L��.
012340     MOVE ��|�ԍ�       TO �A�Í������|�ԍ�.
012350     MOVE ��|�Í������� TO �A�Í������|�Í�������.
012360*
012370     CALL   �����v���O�������v.
012380     CANCEL �����v���O�������v.
012390*
012400*-----------------------------------------------------------------*
012410*
012420     IF ( �A�Í������|���������ԍ�(1:1) = "*"  ) OR
012430        ( �A�Í������|���������ԍ�(1:2) = "��" )
012440        MOVE SPACE           TO ��P�|�ԍ�
012450     ELSE
012460        MOVE �A�Í������|���������ԍ�        TO ��P�|�ԍ�
012470     END-IF.
012480*
012490     IF ( �A�Í������|���������L��(1:1) = "*"  ) OR
012500        ( �A�Í������|���������L��(1:2) = "��" )
012510        MOVE SPACE               TO �L���o�m�v
012520        INSPECT �L���o�m�v  REPLACING ALL ���p�� BY �S�p��
012530        MOVE �L���o�v            TO ��P�|�L��
012540     ELSE
012550        PERFORM �L�����l��
012560        MOVE �L���v              TO ��P�|�L��
012570     END-IF.
012580*
012590*
012600*****     PERFORM ���Z�v�g�ďo��.
012610     PERFORM �����f�[�^�擾.
012620     PERFORM �������擾.
012630     MOVE �������v�q             TO  ��P�|���f��{��.
012640*
012650     MOVE �������Z���v�q         TO  ��P�|���f���Z��.
012660     MOVE ���É񐔂v�q           TO  ��P�|��.
012670     MOVE ���×��v�q             TO  ��P�|���×����z.
012680*
012690     IF ( ���Z�|���     = 1 ) OR
012700        ( ���Z�|�\���J�� = 1 ) OR
012710        ( ���Z�|��H     = 1 )
012720         MOVE 1  TO  ��P�|���Z
012730     END-IF.
012740*
012750     MOVE �������q���Z���v�q     TO ��P�|���z.
012760*
012770     MOVE ��v                   TO ��P�|��.
012780     MOVE ���v                   TO ��P�|��.
012790     MOVE ���v                   TO ��P�|��.
012800*
012810     MOVE �{�p���񋟗��v�q     TO ��P�|���񋟗�.
012820*
012830     MOVE ������ʂv(1)          TO ��P�|���(1).
012840     MOVE ���ʂv(1)              TO ��P�|���ʃR�[�h(1).
012850     MOVE ��É񐔂P�v�q         TO ��P�|��É�(1).
012860     MOVE ��×��P�v�q           TO ��P�|��Ë��z(1).
012870     MOVE ��㪖@�񐔂P�v�q       TO ��P�|��㪉�(1).
012880     MOVE ��㪖@���P�v�q         TO ��P�|��㪋��z(1).
012890     MOVE ���񏈒u���v�q(1)      TO ��P�|�{�p��(1).
012900     MOVE ��㪖@�񐔂P�v�q       TO ��P�|㪖@��(1).
012910     MOVE ��㪖@���P�v�q         TO ��P�|㪖@���z(1).
012920     MOVE �d�É񐔂P�v�q         TO ��P�|�d�É�(1).
012930     MOVE �d�×��P�v�q           TO ��P�|�d�Ë��z(1).
012940     MOVE �]�A�敪�v(1)          TO ��P�|�]�A�敪(1).
012950     MOVE ������ʂv(2)          TO ��P�|���(2).
012960     MOVE ���ʂv(2)              TO ��P�|���ʃR�[�h(2).
012970     MOVE ��É񐔂Q�v�q         TO ��P�|��É�(2).
012980     MOVE ��×��Q�v�q           TO ��P�|��Ë��z(2).
012990     MOVE ��㪖@�񐔂Q�v�q       TO ��P�|��㪉�(2).
013000     MOVE ��㪖@���Q�v�q         TO ��P�|��㪋��z(2).
013010     MOVE ���񏈒u���v�q(2)      TO ��P�|�{�p��(2).
013020     MOVE ��㪖@�񐔂Q�v�q       TO ��P�|㪖@��(2).
013030     MOVE ��㪖@���Q�v�q         TO ��P�|㪖@���z(2).
013040     MOVE �d�É񐔂Q�v�q         TO ��P�|�d�É�(2).
013050     MOVE �d�×��Q�v�q           TO ��P�|�d�Ë��z(2).
013060     MOVE �]�A�敪�v(2)          TO ��P�|�]�A�敪(2).
013070     MOVE ������ʂv(3)          TO ��P�|���(3).
013080     MOVE ���ʂv(3)              TO ��P�|���ʃR�[�h(3).
013090     MOVE ��É񐔂R�v�q         TO ��P�|��É�(3).
013100     MOVE ��×��R�v�q           TO ��P�|��Ë��z(3).
013110     MOVE ��㪖@�񐔂R�v�q       TO ��P�|��㪉�(3).
013120     MOVE ��㪖@���R�v�q         TO ��P�|��㪋��z(3).
013130     MOVE ���񏈒u���v�q(3)      TO ��P�|�{�p��(3).
013140     MOVE ��㪖@�񐔂R�v�q       TO ��P�|㪖@��(3).
013150     MOVE ��㪖@���R�v�q         TO ��P�|㪖@���z(3).
013160     MOVE �d�É񐔂R�v�q         TO ��P�|�d�É�(3).
013170     MOVE �d�×��R�v�q           TO ��P�|�d�Ë��z(3).
013180     MOVE �]�A�敪�v(3)          TO ��P�|�]�A�敪(3).
013190     MOVE ������ʂv(4)          TO ��P�|���(4).
013200     MOVE ���ʂv(4)              TO ��P�|���ʃR�[�h(4).
013210     MOVE ��É񐔂S�v�q         TO ��P�|��É�(4).
013220     MOVE ��×��S�v�q           TO ��P�|��Ë��z(4).
013230     MOVE ��㪖@�񐔂S�v�q       TO ��P�|��㪉�(4).
013240     MOVE ��㪖@���S�v�q         TO ��P�|��㪋��z(4).
013250     MOVE ���񏈒u���v�q(4)      TO ��P�|�{�p��(4).
013260     MOVE ��㪖@�񐔂S�v�q       TO ��P�|㪖@��(4).
013270     MOVE ��㪖@���S�v�q         TO ��P�|㪖@���z(4).
013280     MOVE �d�É񐔂S�v�q         TO ��P�|�d�É�(4).
013290     MOVE �d�×��S�v�q           TO ��P�|�d�Ë��z(4).
013300     MOVE �]�A�敪�v(4)          TO ��P�|�]�A�敪(4).
013310     MOVE ������ʂv(5)          TO ��P�|���(5).
013320     MOVE ���ʂv(5)              TO ��P�|���ʃR�[�h(5).
013330     MOVE ��É񐔂T�v�q         TO ��P�|��É�(5).
013340     MOVE ��×��T�v�q           TO ��P�|��Ë��z(5).
013350     MOVE ��㪖@�񐔂T�v�q       TO ��P�|��㪉�(5).
013360     MOVE ��㪖@���T�v�q         TO ��P�|��㪋��z(5).
013370     MOVE ���񏈒u���v�q(5)      TO ��P�|�{�p��(5).
013380     MOVE ��㪖@�񐔂T�v�q       TO ��P�|㪖@��(5).
013390     MOVE ��㪖@���T�v�q         TO ��P�|㪖@���z(5).
013400     MOVE �d�É񐔂T�v�q         TO ��P�|�d�É�(5).
013410     MOVE �d�×��T�v�q           TO ��P�|�d�Ë��z(5).
013420     MOVE �]�A�敪�v(5)          TO ��P�|�]�A�敪(5).
013430*
013440* 1�ł��p��������� 9 ��]�L
013450     IF �p���t���O = "YES"
013460         MOVE  9                 TO ��P�|�]�A�敪����
013470     END-IF.
013480*
013490*
013500*================================================================*
013510*================================================================*
013520 ���S�����擾 SECTION.
013530*/���S����
013540     INITIALIZE �A���|���S���擾�L�[.
013550     MOVE ��|�{�p�a��N�� TO �A���|�{�p�a��N��.
013560     MOVE ��|���҃R�[�h   TO �A���|���҃R�[�h.
013570     MOVE "HUTANRIT" TO �v���O�������v.
013580     CALL �v���O�������v.
013590     CANCEL �v���O�������v.
013600*
013610*================================================================*
013620*================================================================*
013630 ��P�t�@�C������ SECTION.
013640*
013650     WRITE ��P�|���R�[�h
013660     INVALID KEY
013670         MOVE NC"��P"  TO �t�@�C����
013680         PERFORM �G���[�\��
013690     END-WRITE.
013700*================================================================*
013710*================================================================*
013720 ����ԍ��E�l�� SECTION.
013730*
013740     MOVE ����ԍ��v      TO  ����ԍ����l�߂v.
013750     MOVE ZERO            TO  ����ԍ��E�l�߂v.
013760     MOVE ZERO            TO  ����ԍ������v.
013770*
013780     MOVE  8  TO  �J�E���^.
013790*
013800     IF  ����ԍ����l�߂v�P(7) NOT = SPACE
013810         COMPUTE �J�E���^ = �J�E���^  -  1
013820         MOVE ����ԍ����l�߂v�P(7)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
013830     END-IF.
013840     IF  ����ԍ����l�߂v�P(6) NOT = SPACE
013850         COMPUTE �J�E���^ = �J�E���^  -  1
013860         MOVE ����ԍ����l�߂v�P(6)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
013870     END-IF.
013880     IF  ����ԍ����l�߂v�P(5) NOT = SPACE
013890         COMPUTE �J�E���^ = �J�E���^  -  1
013900         MOVE ����ԍ����l�߂v�P(5)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
013910     END-IF.
013920     IF  ����ԍ����l�߂v�P(4) NOT = SPACE
013930         COMPUTE �J�E���^ = �J�E���^  -  1
013940         MOVE ����ԍ����l�߂v�P(4)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
013950     END-IF.
013960     IF  ����ԍ����l�߂v�P(3) NOT = SPACE
013970         COMPUTE �J�E���^ = �J�E���^  -  1
013980         MOVE ����ԍ����l�߂v�P(3)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
013990     END-IF.
014000     IF  ����ԍ����l�߂v�P(2) NOT = SPACE
014010         COMPUTE �J�E���^ = �J�E���^  -  1
014020         MOVE ����ԍ����l�߂v�P(2)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
014030     END-IF.
014040     IF  ����ԍ����l�߂v�P(1) NOT = SPACE
014050         COMPUTE �J�E���^ = �J�E���^  -  1
014060         MOVE ����ԍ����l�߂v�P(1)  TO  ����ԍ��E�l�߂v�P(�J�E���^)
014070     END-IF.
014080*
014090     MOVE ����ԍ��E�l�߂v TO ����ԍ������v.
014100*
014110*================================================================*
014120 �L�����l�� SECTION.
014130*
014140***** �L���̖��ʂ�SPACE����菜���āA���l�߂ɂ���B
014150     MOVE SPACE           TO  �L���m�v.
014160     MOVE SPACE           TO  �L�����v.
014170     MOVE SPACE           TO  �L�����l�߂v.
014180     MOVE �A�Í������|���������L�� TO  �L�����v.
014190*
014200     MOVE  ZERO  TO  �J�E���^�Q.
014210     PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 12
014220          IF  �L�����v�P(�J�E���^) NOT = SPACE
014230              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
014240              MOVE �L�����v�P(�J�E���^)  TO  �L�����l�߂v�P(�J�E���^�Q)
014250          END-IF
014260     END-PERFORM.
014270*
014280     MOVE �L�����l�߂v    TO �L���m�v.
014290*
014300*���p�X�y�[�X��S�p�ɂ�����
014310     INSPECT �L���v REPLACING ALL ���p�� BY �S�p��.
014320*
014330*================================================================*
014340*================================================================*
014350 �ی��敪�擾 SECTION.
014360*
014370*
014380     IF ( ��|������� NOT = ZERO ) OR
014390        (( ��|������ = 05 ) AND ( ��|�{�p�a��N�� < 42004 ))
014400**-- �����E�V�l --
014410         IF ( ��|������ = 05 ) AND ( ��|�{�p�a��N�� < 42004 )
014420             MOVE 05     TO ��P�|�ی��敪
014430         ELSE
014440             EVALUATE ��|�������
014450* ����
014460             WHEN 50
014470                 MOVE 10     TO ��P�|�ی��敪
014480* �S�P�V�l
014490             WHEN 51
014500                 MOVE 05     TO ��P�|�ی��敪
014510* �ЂƂ�e
014520             WHEN 52
014530                 MOVE 13     TO ��P�|�ی��敪
014540* �g��
014550             WHEN 53
014560                 MOVE 14     TO ��P�|�ی��敪
014570* �픚
014580             WHEN 54
014590                 MOVE 15     TO ��P�|�ی��敪
014600* ���c��
014610             WHEN 55
014620                 MOVE 16     TO ��P�|�ی��敪
014630             WHEN 60
014640*�l���s�ΐ��s�̏����w���͓��c�������ɂ���/100927
014650                 IF ��|��p���S�Ҕԍ�����(1:4) = "8322"
014660                     MOVE 16     TO ��P�|�ی��敪
014670                 END-IF
014680             END-EVALUATE
014690         END-IF
014700         EVALUATE ��|�ی����
014710* ����
014720         WHEN 01
014730* �Е�
014740         WHEN 02
014750* �g��
014760         WHEN 03
014770* ����
014780         WHEN 04
014790* ����
014800         WHEN 06
014810* �D��
014820         WHEN 07
014830* �ސE
014840         WHEN 08
014850             MOVE ��|�ی���� TO ��P�|�V�l����
014860* ���q��
014870         WHEN 09
014880             MOVE 4 TO ��P�|�V�l����
014890* �������
014900         WHEN 05
014910             IF ��|�{�p�a��N�� >= 42004
014920                 MOVE 30 TO ��P�|�V�l����
014930             END-IF
014940*
014950         WHEN OTHER
014960             MOVE SPACE                TO �G���[���b�Z�[�W�v
014970             MOVE ��|���҃R�[�h       TO �G���[���҃R�[�h�v
014980             MOVE ��|�ی����         TO �G���[�ی���ʂv
014990             MOVE "-"                  TO �G���[��؂�v
015000             MOVE  NC"�ی���ʂ��ُ�ł��B���ҁ|�ی����" TO �A���R�|���b�Z�[�W
015010             MOVE  �G���[���b�Z�[�W�v  TO �A���R�|���b�Z�[�W�P
015020             CALL   "MSG003"
015030             CANCEL "MSG003"
015040         END-EVALUATE
015050     ELSE
015060**-- ���� --
015070         EVALUATE ��|�ی����
015080* ����
015090         WHEN 01
015100* �Е�
015110         WHEN 02
015120* �g��
015130         WHEN 03
015140* ����
015150         WHEN 04
015160* ����
015170         WHEN 06
015180* �D��
015190         WHEN 07
015200* �ސE
015210         WHEN 08
015220             MOVE ��|�ی���� TO ��P�|�ی��敪
015230* ���q��
015240         WHEN 09
015250             MOVE 4 TO ��P�|�ی��敪
015260* �J��
015270         WHEN 70
015280             MOVE 9 TO ��P�|�ی��敪
015290* ������
015300         WHEN 80
015310             MOVE 12 TO ��P�|�ی��敪
015320* �������
015330         WHEN 05
015340             IF ��|�{�p�a��N�� >= 42004
015350                 MOVE 30 TO ��P�|�ی��敪
015360             END-IF
015370*
015380         WHEN OTHER
015390             MOVE SPACE                TO �G���[���b�Z�[�W�v
015400             MOVE ��|���҃R�[�h       TO �G���[���҃R�[�h�v
015410             MOVE ��|�ی����         TO �G���[�ی���ʂv
015420             MOVE "-"                  TO �G���[��؂�v
015430             MOVE  NC"�ی���ʂ��ُ�ł��B���ҁ|�ی����" TO �A���R�|���b�Z�[�W
015440             MOVE  �G���[���b�Z�[�W�v  TO �A���R�|���b�Z�[�W�P
015450             CALL   "MSG003"
015460             CANCEL "MSG003"
015470         END-EVALUATE
015480     END-IF.
015490*
015500*================================================================*
015510*================================================================*
015520 �������擾 SECTION.
015530*
015540***********************************************
015550* �����f�[�^�Z�b�g                            *
015560***********************************************
015570*    ****************************************************************
015580*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
015590*    ****************************************************************
015600     INITIALIZE �����P�v�q.
015610     INITIALIZE �����Q�v�q.
015620     INITIALIZE �����R�v�q.
015630     MOVE ���Z�|������                 TO  �������v�q.
015640*
015650     MOVE ���Z�|�������Z��             TO  �������Z���v�q.
015660     MOVE ���Z�|�Č���                 TO  �Č����v�q.
015670     MOVE ���Z�|���É�               TO  ���É񐔂v�q.
015680     MOVE ���Z�|���×�                 TO  ���×��v�q.
015690     MOVE ���Z�|���É��Z��             TO  ���É��Z���v�q.
015700*
015710*
015720     MOVE ���Z�|�������q���Z��         TO  �������q���Z���v�q.
015730*
015740     MOVE ���Z�|��                     TO ��v.
015750     MOVE ���Z�|��                     TO ���v.
015760     MOVE ���Z�|��                     TO ���v.
015770*
015780     MOVE ���Z�|�{�p���񋟗�         TO  �{�p���񋟗��v�q.
015790********************
015800* ���񏈒u���Z�b�g *
015810********************
015820     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
015830             UNTIL ( ���ʂb�m�s > ���ʐ��v )
015840         MOVE ���Z�|���񏈒u��(���ʂb�m�s) TO ���񏈒u���v�q(���ʂb�m�s)
015850     END-PERFORM.
015860********************
015870* �����������Z�b�g *
015880********************
015890*    **********
015900*    * �P���� *
015910*    **********
015920     MOVE ���Z�|��É񐔂P             TO ��É񐔂P�v�q.
015930     MOVE ���Z�|��×��P               TO ��×��P�v�q.
015940     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
015950     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
015960     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
015970     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
015980     MOVE ���Z�|�d�É񐔂P             TO �d�É񐔂P�v�q.
015990     MOVE ���Z�|�d�×��P               TO �d�×��P�v�q.
016000*    **********
016010*    * �Q���� *
016020*    **********
016030     MOVE ���Z�|��É񐔂Q             TO ��É񐔂Q�v�q.
016040     MOVE ���Z�|��×��Q               TO ��×��Q�v�q.
016050     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
016060     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
016070     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
016080     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
016090     MOVE ���Z�|�d�É񐔂Q             TO �d�É񐔂Q�v�q.
016100     MOVE ���Z�|�d�×��Q               TO �d�×��Q�v�q.
016110*    ****************
016120*    * �R���ʁ^�W�� *
016130*    ****************
016140     MOVE ���Z�|��É񐔂R�W             TO ��É񐔂R�W�v�q.
016150     MOVE ���Z�|��×��R�W               TO ��×��R�W�v�q.
016160     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
016170     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
016180     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
016190     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
016200     MOVE ���Z�|�d�É񐔂R�W             TO �d�É񐔂R�W�v�q.
016210     MOVE ���Z�|�d�×��R�W               TO �d�×��R�W�v�q.
016220*    ****************
016230*    * �R���ʁ^10�� *
016240*    ****************
016250     MOVE ���Z�|��É񐔂R�O             TO ��É񐔂R�O�v�q.
016260     MOVE ���Z�|��×��R�O               TO ��×��R�O�v�q.
016270     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
016280     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
016290     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
016300     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
016310     MOVE ���Z�|�d�É񐔂R�O             TO �d�É񐔂R�O�v�q.
016320     MOVE ���Z�|�d�×��R�O               TO �d�×��R�O�v�q.
016330*    ****************
016340*    * �R���ʁ^���v *
016350*    ****************
016360     COMPUTE ��É񐔂R�v�q      = ��É񐔂R�W�v�q   + ��É񐔂R�O�v�q.
016370     COMPUTE ��×��R�v�q        = ��×��R�W�v�q     + ��×��R�O�v�q.
016380     COMPUTE ��㪖@�񐔂R�v�q    = ��㪖@�񐔂R�W�v�q + ��㪖@�񐔂R�O�v�q.
016390     COMPUTE ��㪖@���R�v�q      = ��㪖@���R�W�v�q   + ��㪖@���R�O�v�q.
016400     COMPUTE ��㪖@�񐔂R�v�q    = ��㪖@�񐔂R�W�v�q + ��㪖@�񐔂R�O�v�q.
016410     COMPUTE ��㪖@���R�v�q      = ��㪖@���R�W�v�q   + ��㪖@���R�O�v�q.
016420     COMPUTE �d�É񐔂R�v�q      = �d�É񐔂R�W�v�q   + �d�É񐔂R�O�v�q.
016430     COMPUTE �d�×��R�v�q        = �d�×��R�W�v�q     + �d�×��R�O�v�q.
016440*    ****************
016450*    * �S���ʁ^�T�� *
016460*    ****************
016470     MOVE ���Z�|��É񐔂S�T             TO ��É񐔂S�T�v�q.
016480     MOVE ���Z�|��×��S�T               TO ��×��S�T�v�q.
016490     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
016500     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
016510     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
016520     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
016530     MOVE ���Z�|�d�É񐔂S�T             TO �d�É񐔂S�T�v�q.
016540     MOVE ���Z�|�d�×��S�T               TO �d�×��S�T�v�q.
016550*    ****************
016560*    * �S���ʁ^�W�� *
016570*    ****************
016580     MOVE ���Z�|��É񐔂S�W             TO ��É񐔂S�W�v�q.
016590     MOVE ���Z�|��×��S�W               TO ��×��S�W�v�q.
016600     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
016610     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
016620     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
016630     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
016640     MOVE ���Z�|�d�É񐔂S�W             TO �d�É񐔂S�W�v�q.
016650     MOVE ���Z�|�d�×��S�W               TO �d�×��S�W�v�q.
016660*    ****************
016670*    * �S���ʁ^10�� *
016680*    ****************
016690     MOVE ���Z�|��É񐔂S�O             TO ��É񐔂S�O�v�q.
016700     MOVE ���Z�|��×��S�O               TO ��×��S�O�v�q.
016710     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
016720     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
016730     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
016740     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
016750     MOVE ���Z�|�d�É񐔂S�O             TO �d�É񐔂S�O�v�q.
016760     MOVE ���Z�|�d�×��S�O               TO �d�×��S�O�v�q.
016770*    ****************
016780*    * �S���ʁ^���v *
016790*    ****************
016800     COMPUTE ��É񐔂S�v�q      = ��É񐔂S�T�v�q   + ��É񐔂S�W�v�q   + ��É񐔂S�O�v�q.
016810     COMPUTE ��×��S�v�q        = ��×��S�T�v�q     + ��×��S�W�v�q     + ��×��S�O�v�q.
016820     COMPUTE ��㪖@�񐔂S�v�q    = ��㪖@�񐔂S�T�v�q + ��㪖@�񐔂S�W�v�q + ��㪖@�񐔂S�O�v�q.
016830     COMPUTE ��㪖@���S�v�q      = ��㪖@���S�T�v�q   + ��㪖@���S�W�v�q   + ��㪖@���S�O�v�q.
016840     COMPUTE ��㪖@�񐔂S�v�q    = ��㪖@�񐔂S�T�v�q + ��㪖@�񐔂S�W�v�q + ��㪖@�񐔂S�O�v�q.
016850     COMPUTE ��㪖@���S�v�q      = ��㪖@���S�T�v�q   + ��㪖@���S�W�v�q   + ��㪖@���S�O�v�q.
016860     COMPUTE �d�É񐔂S�v�q      = �d�É񐔂S�T�v�q   + �d�É񐔂S�W�v�q   + �d�É񐔂S�O�v�q.
016870     COMPUTE �d�×��S�v�q        = �d�×��S�T�v�q     + �d�×��S�W�v�q     + �d�×��S�O�v�q.
016880*    *****************
016890*    * �T���ʁ^2.5�� *
016900*    *****************
016910     MOVE ���Z�|��É񐔂T�Q             TO ��É񐔂T�Q�v�q.
016920     MOVE ���Z�|��×��T�Q               TO ��×��T�Q�v�q.
016930     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
016940     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
016950     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
016960     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
016970     MOVE ���Z�|�d�É񐔂T�Q             TO �d�É񐔂T�Q�v�q.
016980     MOVE ���Z�|�d�×��T�Q               TO �d�×��T�Q�v�q.
016990*    ****************
017000*    * �T���ʁ^�T�� *
017010*    ****************
017020     MOVE ���Z�|��É񐔂T�T             TO ��É񐔂T�T�v�q.
017030     MOVE ���Z�|��×��T�T               TO ��×��T�T�v�q.
017040     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
017050     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
017060     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
017070     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
017080     MOVE ���Z�|�d�É񐔂T�T             TO �d�É񐔂T�T�v�q.
017090     MOVE ���Z�|�d�×��T�T               TO �d�×��T�T�v�q.
017100*    ****************
017110*    * �T���ʁ^�W�� *
017120*    ****************
017130     MOVE ���Z�|��É񐔂T�W             TO ��É񐔂T�W�v�q.
017140     MOVE ���Z�|��×��T�W               TO ��×��T�W�v�q.
017150     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
017160     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
017170     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
017180     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
017190     MOVE ���Z�|�d�É񐔂T�W             TO �d�É񐔂T�W�v�q.
017200     MOVE ���Z�|�d�×��T�W               TO �d�×��T�W�v�q.
017210*    ****************
017220*    * �T���ʁ^10�� *
017230*    ****************
017240     MOVE ���Z�|��É񐔂T�O             TO ��É񐔂T�O�v�q.
017250     MOVE ���Z�|��×��T�O               TO ��×��T�O�v�q.
017260     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
017270     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
017280     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
017290     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
017300     MOVE ���Z�|�d�É񐔂T�O             TO �d�É񐔂T�O�v�q.
017310     MOVE ���Z�|�d�×��T�O               TO �d�×��T�O�v�q.
017320*    ****************
017330*    * �T���ʁ^���v *
017340*    ****************
017350     COMPUTE ��É񐔂T�v�q   = ��É񐔂T�Q�v�q   + ��É񐔂T�T�v�q   +
017360                                ��É񐔂T�W�v�q   + ��É񐔂T�O�v�q.
017370     COMPUTE ��×��T�v�q     = ��×��T�Q�v�q     + ��×��T�T�v�q     +
017380                                ��×��T�W�v�q     + ��×��T�O�v�q.
017390     COMPUTE ��㪖@�񐔂T�v�q = ��㪖@�񐔂T�Q�v�q + ��㪖@�񐔂T�T�v�q +
017400                                ��㪖@�񐔂T�W�v�q + ��㪖@�񐔂T�O�v�q.
017410     COMPUTE ��㪖@���T�v�q   = ��㪖@���T�Q�v�q   + ��㪖@���T�T�v�q   +
017420                                ��㪖@���T�W�v�q   + ��㪖@���T�O�v�q.
017430     COMPUTE ��㪖@�񐔂T�v�q = ��㪖@�񐔂T�Q�v�q + ��㪖@�񐔂T�T�v�q +
017440                                ��㪖@�񐔂T�W�v�q + ��㪖@�񐔂T�O�v�q.
017450     COMPUTE ��㪖@���T�v�q   = ��㪖@���T�Q�v�q   + ��㪖@���T�T�v�q   +
017460                                ��㪖@���T�W�v�q   + ��㪖@���T�O�v�q.
017470     COMPUTE �d�É񐔂T�v�q   = �d�É񐔂T�Q�v�q   + �d�É񐔂T�T�v�q   +
017480                                �d�É񐔂T�W�v�q   + �d�É񐔂T�O�v�q.
017490     COMPUTE �d�×��T�v�q     = �d�×��T�Q�v�q     + �d�×��T�T�v�q     +
017500                                �d�×��T�W�v�q     + �d�×��T�O�v�q.
017510*
017520*================================================================*
017530 �����f�[�^�擾 SECTION.
017540*
017550**************************************************
017560* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾 *
017570* �� ���ʁ{�������                              *
017580* �� �]�A�敪 ....�敪�ɂ��`�F�b�N��"��"���i�[ *
017590* �� �������q ....�敪�ɂ��`�F�b�N��"��"���i�[ *
017600* �� �o�߃R�[�h...�o�߃}�X�^���擾             *
017610**************************************************
017620     INITIALIZE �������v.
017630     MOVE SPACE TO �p���t���O.
017640     MOVE �{�p�a��v�q       TO ���|�{�p�a��.
017650     MOVE �{�p�N�v�q         TO ���|�{�p�N.
017660     MOVE �{�p���v�q         TO ���|�{�p��.
017670     MOVE ���҃R�[�h�v�q     TO ���|���҃R�[�h.
017680     READ �����f�[�^�e
017690     INVALID KEY
017700         CONTINUE
017710*            /* ���肦�Ȃ� */
017720     NOT INVALID KEY
017730         MOVE ���|���ʐ�                   TO ���ʐ��v
017740         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
017750                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
017760             MOVE ���|�������(���ʂb�m�s) TO ������ʂv(���ʂb�m�s)
017770             MOVE ���|����(���ʂb�m�s)     TO ���ʂv(���ʂb�m�s)
017780*
017790             MOVE ���|�]�A�敪(���ʂb�m�s) TO �]�A�敪�v(���ʂb�m�s)
017800*   --- �p���̕��ʂ����邩���� ---
017810             IF ���|�]�A�敪(���ʂb�m�s) = 9
017820                  MOVE "YES" TO �p���t���O
017830             END-IF
017840*
017850         END-PERFORM
017860     END-READ.
017870*
017880*================================================================*
017890*================================================================*
017900 ���Z�v�g��ʎ擾 SECTION.
017910*
017920     IF ��|�������  = ZERO
017930         EVALUATE TRUE
017940         WHEN (��|�ی���� = 01) AND (��|������ = ZERO)
017950            MOVE "KOKU"    TO ���Z�v�g��ނv
017960         WHEN (��|�ی���� = 02 OR 06 OR 07) AND (��|�{�l�Ƒ��敪 = 1) AND (��|������ = ZERO)
017970            MOVE "SYAH"    TO ���Z�v�g��ނv
017980         WHEN (��|�ی���� = 02 OR 06 OR 07) AND (��|�{�l�Ƒ��敪 = 2) AND (��|������ = ZERO)
017990            MOVE "SYAK"    TO ���Z�v�g��ނv
018000         WHEN (��|�ی���� = 03) AND (��|�{�l�Ƒ��敪 = 1) AND (��|������ = ZERO)
018010            MOVE "KUMH"    TO ���Z�v�g��ނv
018020         WHEN (��|�ی���� = 03) AND (��|�{�l�Ƒ��敪 = 2) AND (��|������ = ZERO)
018030            MOVE "KUMK"    TO ���Z�v�g��ނv
018040         WHEN (��|�ی���� = 04) AND (��|�{�l�Ƒ��敪 = 1) AND (��|������ = ZERO)
018050            MOVE "KYOH"    TO ���Z�v�g��ނv
018060         WHEN (��|�ی���� = 04) AND (��|�{�l�Ƒ��敪 = 2) AND (��|������ = ZERO)
018070            MOVE "KYOK"    TO ���Z�v�g��ނv
018080         WHEN ��|������ = 05
018090            MOVE "ROUJ"    TO ���Z�v�g��ނv
018100         WHEN (��|�ی���� = 08) AND (��|������ = ZERO)
018110            MOVE "TAIS"    TO ���Z�v�g��ނv
018120         WHEN (��|�ی���� = 09) AND (��|������ = ZERO)
018130            MOVE "JIEI"    TO ���Z�v�g��ނv
018140         WHEN OTHER
018150            CONTINUE
018160*** �� �J�ЁE������ �ǉ��K�v
018170*            MOVE  NC"�ی���ʂ��s���ł��B" TO �A���|���b�Z�[�W
018180*            CALL   "MSG001"
018190*            CANCEL "MSG001"
018200*            PERFORM �t�@�C����
018210*            EXIT PROGRAM
018220         END-EVALUATE
018230     ELSE
018240** ���������鎞
018250         MOVE "JYOS"    TO ���Z�v�g��ނv
018260     END-IF.
018270*
018280*================================================================*
018290******************************************************************
018300 END PROGRAM YAS101.
018310******************************************************************
