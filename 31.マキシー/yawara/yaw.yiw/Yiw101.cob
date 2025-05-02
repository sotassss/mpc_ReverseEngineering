000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YIW101.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*      �A�C�� ���o�t���b�s�[�쐬�yFPD�����z
000100* �����N��Ver.
000110*      MED = YIW580
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2015-09-09
000140 DATE-COMPILED.          2015-09-09
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
000270     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000280                             ORGANIZATION             IS  INDEXED
000290                             ACCESS MODE              IS  DYNAMIC
000300                             RECORD KEY               IS  ��|�{�p�a��N��
000310                                                          ��|���҃R�[�h
000320                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000330                                                          ��|���҃J�i
000340                                                          ��|���҃R�[�h
000350                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000360                                                          ��|�{�p�a��N��
000370                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000380                                                          ��|�ی����
000390                                                          ��|�ی��Ҕԍ�
000400                                                          ��|���҃R�[�h
000410                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000420                                                          ��|������
000430                                                          ��|��p���S�Ҕԍ�
000440                                                          ��|���҃R�[�h
000450                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000460                                                          ��|�������
000470                                                          ��|��p���S�Ҕԍ�����
000480                                                          ��|���҃R�[�h
000490                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
000500                                                          ��|�{�p�a��N��
000510                                                          ��|���҃R�[�h
000520                             FILE STATUS              IS  ��ԃL�[
000530                             LOCK        MODE         IS  AUTOMATIC.
000330     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000340                             ORGANIZATION             IS  INDEXED
000350                             ACCESS MODE              IS  DYNAMIC
000360                             RECORD KEY               IS  ���|�敪�R�[�h
000370                                                          ���|���̃R�[�h
000380                             FILE STATUS              IS  ��ԃL�[
000390                             LOCK        MODE         IS  AUTOMATIC.
000130     SELECT  ���Z�v�g�e      ASSIGN      TO        RECEPTL
000140                             ORGANIZATION             IS  INDEXED
000150                             ACCESS MODE              IS  DYNAMIC
000160                             RECORD KEY               IS  ���Z�|�{�p�a��N��
000170                                                          ���Z�|���҃R�[�h
000180                                                          ���Z�|���Z���
000190                             ALTERNATE RECORD KEY     IS  ���Z�|���҃R�[�h
000200                                                          ���Z�|�{�p�a��N��
000210                                                          ���Z�|���Z���
000220                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000230                                                          ���Z�|�{�p�a��N��
000240                                                          ���Z�|���҃R�[�h
000250                                                          ���Z�|���Z���
000260                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000270                                                          ���Z�|���Z���
000280                                                          ���Z�|�����ی��Ҕԍ�
000290                                                          ���Z�|���҃R�[�h
000300                                                          ���Z�|�{�p�a��N��
000310                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000320                                                          ���Z�|�����ی��Ҕԍ�
000330                                                          ���Z�|���҃R�[�h
000340                                                          ���Z�|���Z���
000350                                                          ���Z�|�{�p�a��N��
000360                             FILE STATUS              IS  ��ԃL�[
000370                             LOCK        MODE         IS  AUTOMATIC.
000540     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000550                             ORGANIZATION             IS  INDEXED
000560                             ACCESS MODE              IS  DYNAMIC
000570                             RECORD KEY               IS  ���|�{�p�a��N��
000580                                                          ���|���҃R�[�h
000590                             ALTERNATE RECORD KEY     IS  ���|���҃R�[�h
000600                                                          ���|�{�p�a��N��
000610                             FILE STATUS              IS  ��ԃL�[
000620                             LOCK        MODE         IS  AUTOMATIC.
001270     SELECT  ���������e      ASSIGN      TO        HUGEINL
001280                             ORGANIZATION             IS  INDEXED
001290                             ACCESS MODE              IS  DYNAMIC
001300                             RECORD KEY               IS  �����|�敪�R�[�h
001310                                                          �����|���������R�[�h
001320                             FILE STATUS              IS  ��ԃL�[
001330                             LOCK        MODE         IS  AUTOMATIC.
000293     SELECT  �����p���҂e    ASSIGN      TO        CHOKEIL
000294                             ORGANIZATION             IS INDEXED
000295                             ACCESS MODE              IS DYNAMIC
000296                             RECORD KEY               IS ���p�|�{�p�a��N��
000297                                                         ���p�|���҃R�[�h
000298                             ALTERNATE RECORD KEY     IS ���p�|���҃R�[�h
000299                                                         ���p�|�{�p�a��N��
000300                             FILE STATUS              IS ��ԃL�[
000301                             LOCK      MODE           IS AUTOMATIC.
000630     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
000640                             ORGANIZATION             IS  INDEXED
000650                             ACCESS MODE              IS  DYNAMIC
000660                             RECORD KEY               IS  �{�L�|�{�p�a��N����
000670                                                          �{�L�|���҃R�[�h
000680                             ALTERNATE RECORD KEY     IS  �{�L�|���҃R�[�h
000690                                                          �{�L�|�{�p�a��N����
000700                             FILE STATUS              IS  ��ԃL�[
000710                             LOCK        MODE         IS  AUTOMATIC.
000720     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000730                             ORGANIZATION             IS  INDEXED
000740                             ACCESS MODE              IS  DYNAMIC
000750                             RECORD KEY               IS �{��|�{�p���ԍ�
000760                             FILE STATUS              IS  ��ԃL�[
000770                             LOCK        MODE         IS  AUTOMATIC.
000780     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000790                             ORGANIZATION             IS  INDEXED
000800                             ACCESS MODE              IS  DYNAMIC
000810                             RECORD KEY               IS  ���|�����敪
000820                             FILE STATUS              IS  ��ԃL�[
000830                             LOCK        MODE         IS  AUTOMATIC.
000840     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS  �ہ|�ی����
000880                                                          �ہ|�ی��Ҕԍ�
000890                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000900                                                          �ہ|�ی��Җ���
000910                                                          �ہ|�ی��Ҕԍ�
000920                             FILE STATUS              IS  ��ԃL�[
000930                             LOCK        MODE         IS  AUTOMATIC.
000940     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000950                             ORGANIZATION             IS  INDEXED
000960                             ACCESS MODE              IS  DYNAMIC
000970                             RECORD KEY               IS  ���|����敪
000980                             FILE STATUS              IS  ��ԃL�[
000990                             LOCK        MODE         IS  AUTOMATIC.
001000     SELECT  ���S���}�X�^    ASSIGN      TO        HUTANRIL
001010                             ORGANIZATION             IS  INDEXED
001020                             ACCESS MODE              IS  DYNAMIC
001030                             RECORD KEY               IS  �����|�ی����
001040                                                          �����|�J�n�a��N��
001050                             FILE STATUS              IS  ��ԃL�[
001060                             LOCK        MODE         IS  AUTOMATIC.
000098     SELECT  �ی��ғ��ʕ��S�}�X�^   ASSIGN      TO        HOKENTKL
000099                             ORGANIZATION             IS  INDEXED
000100                             ACCESS MODE              IS  DYNAMIC
000101                             RECORD KEY               IS  �ۓ��|�ی����
000102                                                          �ۓ��|�ی��Ҕԍ�
000103                                                          �ۓ��|�J�n�a��N��
000105                             FILE STATUS              IS  ��ԃL�[
000106                             LOCK        MODE         IS  AUTOMATIC.
001070     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
001080                             ORGANIZATION             IS  INDEXED
001090                             ACCESS MODE              IS  DYNAMIC
001100                             RECORD KEY               IS  �s�|������
001110                                                          �s�|�s�����ԍ�
001120                             ALTERNATE RECORD KEY     IS  �s�|������
001130                                                          �s�|�s��������
001140                                                          �s�|�s�����ԍ�
001150                             FILE STATUS              IS  ��ԃL�[
001160                             LOCK        MODE         IS  AUTOMATIC.
000241     SELECT  ���ۏ��e      ASSIGN      TO        SEIHOJL
000242                             ORGANIZATION           IS INDEXED
000243                             ACCESS MODE            IS DYNAMIC
000244                             RECORD KEY             IS ���ہ|�{�p�a��N��
000245                                                       ���ہ|���҃R�[�h
000255                             ALTERNATE RECORD KEY   IS ���ہ|���҃R�[�h
000265                                                       ���ہ|�{�p�a��N��
000277                             FILE STATUS            IS ��ԃL�[
000278                             LOCK        MODE       IS AUTOMATIC.
000241     SELECT  �J�Џ��e      ASSIGN      TO        ROUSAIJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS �J�Ё|�{�p�a��N��
000245                                                         �J�Ё|���҃R�[�h
000255                             ALTERNATE RECORD KEY     IS �J�Ё|���҃R�[�h
000265                                                         �J�Ё|�{�p�a��N��
000277                             FILE STATUS              IS ��ԃL�[
000278                             LOCK        MODE         IS AUTOMATIC.
001170     SELECT  ����}�X�^    ASSIGN      TO        KAIJOHOL
001180                             ORGANIZATION             IS  INDEXED
001190                             ACCESS MODE              IS  DYNAMIC
000130                             RECORD KEY               IS  ���|�_���I���敪
000131                                                          ���|����R�[�h
000132                                                          ���|�ی����
000133                                                          ���|�ύX�a��N��
000134                             ALTERNATE RECORD KEY     IS  ���|�_���I���敪
000135                                                          ���|�ڍ��t��J�i
000136                                                          ���|����R�[�h
000137                                                          ���|�ی����
000138                                                          ���|�ύX�a��N��
001270                             FILE STATUS              IS  ��ԃL�[
001280                             LOCK        MODE         IS  AUTOMATIC.
001290*
001291* 2006.12.21 �쐬����t�@�C���̕ۑ����TEMP�ɕύX
001300     SELECT ���[�U�[�t�@�C��   ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\USER.TXT"
001310                             ORGANIZATION             IS  LINE SEQUENTIAL
001320                             ACCESS MODE              IS  SEQUENTIAL
001330                             FILE STATUS              IS  ��ԃL�[
001340                             LOCK        MODE         IS  AUTOMATIC.
001350     SELECT ���ҏ��t�@�C�� ASSIGN  TO "C:\MAKISHISYS\YAWOBJ\TEMP\SAKURA.DAT"
001360*                             ORGANIZATION             IS  LINE SEQUENTIAL
001360                             ORGANIZATION             IS  SEQUENTIAL
001370                             ACCESS MODE              IS  SEQUENTIAL
001380                             FILE STATUS              IS  ��ԃL�[
001390                             LOCK        MODE         IS  AUTOMATIC.
001400     SELECT �ی��҃t�@�C��   ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\HOKEN.CSV"
001410                             ORGANIZATION             IS  LINE SEQUENTIAL
001420                             ACCESS MODE              IS  SEQUENTIAL
001430                             FILE STATUS              IS  ��ԃL�[
001440                             LOCK        MODE         IS  AUTOMATIC.
001450     SELECT ��ƕی��҃t�@�C�� ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001460                             ORGANIZATION             IS  INDEXED
001470                             ACCESS MODE              IS  DYNAMIC
001480                             RECORD KEY               IS  ��ی��|�ی���ʃL�[
001490                                                          ��ی��|�ی��Ҕԍ��L�[
001491                             ALTERNATE RECORD KEY     IS  ��ی��|�ی����
001492                                                          ��ی��|�ی��Ҕԍ��L�[
001500                             FILE STATUS              IS  ��ԃL�[
001510                             LOCK        MODE         IS  AUTOMATIC.
001130     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5811L.DAT"
001140                             ORGANIZATION             IS  INDEXED
001150                             ACCESS                   IS  DYNAMIC
001160                             RECORD      KEY          IS  ��P�|�������
001180                                                          ��P�|�۔�
001170                                                          ��P�|���
001190                                                          ��P�|�ی��Ҕԍ�
001170                                                          ��P�|�{�l�Ƒ��敪
001200                                                          ��P�|���҃R�[�h
001210                                                          ��P�|�{�p�a��N��
001590                             FILE STATUS              IS  ��ԃL�[
001600                             LOCK        MODE         IS  AUTOMATIC.
001601*
001602     SELECT ���o�t�@�C��   ASSIGN      TO     FD-NAME
001603                             ORGANIZATION             IS  LINE SEQUENTIAL
001604                             ACCESS MODE              IS  SEQUENTIAL
001605                             FILE STATUS              IS  ��ԃL�[
001606                             LOCK      MODE           IS  AUTOMATIC.
001610******************************************************************
001620*                      DATA DIVISION                             *
001630******************************************************************
001640 DATA                    DIVISION.
001650 FILE                    SECTION.
001660*                           �m�q�k��  �R�Q�O�n
001670 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001680     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000780*                           �m�q�k��  �P�Q�W�n
000790 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
000800     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001690*                           �m�q�k��  �P�Q�W�n
001700 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001710     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000107*
000108 FD  �ی��ғ��ʕ��S�}�X�^  BLOCK   CONTAINS   1   RECORDS.
000109     COPY HOKENTK         OF  XFDLIB  JOINING   �ۓ� AS PREFIX.
002390*                           �m�q�k��  �P�Q�W�n
002400 FD  ���������e          BLOCK   CONTAINS   1   RECORDS.
002410     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
000686*                           �m�q�k��  �P�Q�W�n
000687 FD  �����p���҂e          BLOCK   CONTAINS   1   RECORDS.
000688     COPY CHOKEI          OF  XFDLIB  JOINING   ���p   AS  PREFIX.
001720*                           �m�q�k��  �Q�T�U�n
001730 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
001740     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
001750*
001760 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
001770     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001780*                           �m�q�k��  �P�Q�W�n
001790 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001800     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001810*                           �m�q�k��  �R�Q�O�n
001820 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001830     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001840*                           �m�q�k��  �Q�T�U�n
001850 FD  ������}�X�^      BLOCK   CONTAINS   1   RECORDS.
001860     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001870*                           �m�q�k��  �Q�T�U�n
001880 FD  ���S���}�X�^        BLOCK   CONTAINS   1   RECORDS.
001890     COPY HUTANRI    OF  XFDLIB  JOINING   ���� AS  PREFIX.
001900*                           �m�q�k��  �Q�T�U�n
001910 FD  �s�����}�X�^        BLOCK   CONTAINS   1   RECORDS.
001920     COPY SITYOSN         OF  XFDLIB  JOINING   �s   AS  PREFIX.
001080* 
000280 FD  ���ۏ��e          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   ����   AS  PREFIX.
001510*
000280 FD  �J�Џ��e          BLOCK   CONTAINS   1   RECORDS.
000281     COPY ROUSAIJ         OF  XFDLIB  JOINING   �J��   AS  PREFIX.
001930*                           �m�q�k��  �U�S�O�n
001940 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
001950     COPY KAIJOHO         OF  XFDLIB  JOINING   ���   AS  PREFIX.
001960 FD  ���[�U�[�t�@�C�� RECORD IS VARYING IN SIZE
001970               FROM 1 TO 50 DEPENDING ON �����J�E���^.
001980* FD  ���[�U�[�t�@�C�� RECORD  CONTAINS 718 CHARACTERS.
001990 01  ���|���R�[�h.
002000     03  ���|���R�[�h�f�[�^                PIC X(50).
002010*
002020* FD  ���ҏ��t�@�C�� RECORD IS VARYING IN SIZE
002030*               FROM 1 TO 1272 DEPENDING ON �����J�E���^.
       FD  ���ҏ��t�@�C�� RECORD  CONTAINS 956 CHARACTERS.
002040 01 ���|���R�[�h.
002050   03 ���|���R�[�h�f�[�^.
002060     05 ���|����ԍ�                      PIC X(4).
002080     05 ���|��z���󗝋敪                PIC 9(1).
002500     05 ���|�����N��                      PIC 9(6).
002080     05 ���|���Z�v�g�ԍ�                  PIC 9(4).
002520     05 ���|�{�p�N��                      PIC 9(6).
002080     05 ���|���Z���                      PIC 9(1).
002080     05 ���|�ی��Ҕԍ�                    PIC X(8).
002340     05 ���|�ی��؋L��                    PIC X(20).
002360     05 ���|�ی��ؔԍ�                    PIC X(16).
002260     05 ���|�V���s�����ԍ�                PIC X(8).
002380     05 ���|�V���󋋎Ҕԍ�                PIC X(8).
002300     05 ���|�������S�Ҕԍ�                PIC X(8).
002400     05 ���|�����󋋎Ҕԍ�                PIC X(16).
002200     05 ���|��ی��Ҏ�������              PIC X(20).
002180     05 ���|��ی��Ҏ����J�i              PIC X(16).
002160     05 ���|���Ҏ�������                  PIC X(20).
002140     05 ���|���Ҏ����J�i                  PIC X(16).
002120     05 ���|�{�l�Ƒ��敪                  PIC 9(1).
002440     05 ���|����                          PIC 9(1).
002420     05 ���|���N����                      PIC X(8).
002080     05 ���|��ی��ҏZ��                  PIC X(28).
002640     05 ���|��p�z                        PIC 9(6).
002080     05 ���|���t����                      PIC 9(2).
002660     05 ���|�ꕔ���S��                    PIC 9(6).
002680     05 ���|�������z                      PIC 9(6).
002740     05 ���|���ʐ�                        PIC 9(1).
003280     05 ���|��������                      PIC X(20).
003400     05 ���|�������R                      PIC X(32).
002100     05 ���|�V�K                          PIC 9(1).
002100     05 ���|�p��                          PIC 9(1).
002100     05 ���|������                      PIC 9(1).
002100     05 ���|�������ԊO��                PIC 9(1).
002100     05 ���|�����x����                  PIC 9(1).
002100     05 ���|�����[���                  PIC 9(1).
002100     05 ���|�Č���                      PIC 9(1).
002220     05 ���|���Ë���                      PIC 9(3).
002220     05 ���|���É�                      PIC 9(2).
002100     05 ���|���Ö�ԉ�                  PIC 9(1).
002100     05 ���|���Ó�H��                  PIC 9(2).
002100     05 ���|���Ö\���J���              PIC 9(2).
002240     05 ���|�������q���                PIC 9(1).
002240     05 ���|�������q����                PIC 9(1).
002240     05 ���|�������q����                PIC 9(1).
002280     05 ���|���񋟉�                  PIC 9(1).
002320     05 ���|�����敪�P                    PIC 9(1).
002460     05 ���|�������P                      PIC N(16).
002960     05 ���|���񏈒u�񐔂P                PIC 9(1).
002980     05 ���|�����N�����P                  PIC 9(8).
003000     05 ���|�����N�����P                  PIC 9(8).
002540     05 ���|�{�p�J�n���P                  PIC 9(8).
002560     05 ���|�{�p�I�����P                  PIC 9(8).
002580     05 ���|�������P                      PIC 9(2).
003080     05 ���|�]�A�敪�P                    PIC 9(1).
003100     05 ���|��É񐔂P                    PIC 9(2).
003120     05 ���|��㪖@�񐔂P                  PIC 9(1).
003140     05 ���|��㪖@�񐔂P                  PIC 9(2).
003180     05 ���|�d�É񐔂P                    PIC 9(2).
003200     05 ���|�����ʒ����敪�P              PIC 9(1).
003220     05 ���|���������敪�P                PIC 9(1).
002320     05 ���|�����敪�Q                    PIC 9(1).
002460     05 ���|�������Q                      PIC N(16).
002960     05 ���|���񏈒u�񐔂Q                PIC 9(1).
002980     05 ���|�����N�����Q                  PIC 9(8).
003000     05 ���|�����N�����Q                  PIC 9(8).
002540     05 ���|�{�p�J�n���Q                  PIC 9(8).
002560     05 ���|�{�p�I�����Q                  PIC 9(8).
002580     05 ���|�������Q                      PIC 9(2).
003080     05 ���|�]�A�敪�Q                    PIC 9(1).
003100     05 ���|��É񐔂Q                    PIC 9(2).
003120     05 ���|��㪖@�񐔂Q                  PIC 9(1).
003140     05 ���|��㪖@�񐔂Q                  PIC 9(2).
003180     05 ���|�d�É񐔂Q                    PIC 9(2).
003200     05 ���|�����ʒ����敪�Q              PIC 9(1).
003220     05 ���|���������敪�Q                PIC 9(1).
002320     05 ���|�����敪�R                    PIC 9(1).
002460     05 ���|�������R                      PIC N(16).
002960     05 ���|���񏈒u�񐔂R                PIC 9(1).
002980     05 ���|�����N�����R                  PIC 9(8).
003000     05 ���|�����N�����R                  PIC 9(8).
002540     05 ���|�{�p�J�n���R                  PIC 9(8).
002560     05 ���|�{�p�I�����R                  PIC 9(8).
002580     05 ���|�������R                      PIC 9(2).
003080     05 ���|�]�A�敪�R                    PIC 9(1).
003100     05 ���|��É񐔂R                    PIC 9(2).
003120     05 ���|��㪖@�񐔂R                  PIC 9(1).
003140     05 ���|��㪖@�񐔂R                  PIC 9(2).
003180     05 ���|�d�É񐔂R                    PIC 9(2).
003200     05 ���|�����ʒ����敪�R              PIC 9(1).
003220     05 ���|���������敪�R                PIC 9(1).
002320     05 ���|�����敪�S                    PIC 9(1).
002460     05 ���|�������S                      PIC N(16).
002960     05 ���|���񏈒u�񐔂S                PIC 9(1).
002980     05 ���|�����N�����S                  PIC 9(8).
003000     05 ���|�����N�����S                  PIC 9(8).
002540     05 ���|�{�p�J�n���S                  PIC 9(8).
002560     05 ���|�{�p�I�����S                  PIC 9(8).
002580     05 ���|�������S                      PIC 9(2).
003080     05 ���|�]�A�敪�S                    PIC 9(1).
003100     05 ���|��É񐔂S                    PIC 9(2).
003120     05 ���|��㪖@�񐔂S                  PIC 9(1).
003140     05 ���|��㪖@�񐔂S                  PIC 9(2).
003180     05 ���|�d�É񐔂S                    PIC 9(2).
003200     05 ���|�����ʒ����敪�S              PIC 9(1).
003220     05 ���|���������敪�S                PIC 9(1).
002320     05 ���|�����敪�T                    PIC 9(1).
002460     05 ���|�������T                      PIC N(16).
002960     05 ���|���񏈒u�񐔂T                PIC 9(1).
002980     05 ���|�����N�����T                  PIC 9(8).
003000     05 ���|�����N�����T                  PIC 9(8).
002540     05 ���|�{�p�J�n���T                  PIC 9(8).
002560     05 ���|�{�p�I�����T                  PIC 9(8).
002580     05 ���|�������T                      PIC 9(2).
003080     05 ���|�]�A�敪�T                    PIC 9(1).
003100     05 ���|��É񐔂T                    PIC 9(2).
003120     05 ���|��㪖@�񐔂T                  PIC 9(1).
003140     05 ���|��㪖@�񐔂T                  PIC 9(2).
003180     05 ���|�d�É񐔂T                    PIC 9(2).
003200     05 ���|�����ʒ����敪�T              PIC 9(1).
003220     05 ���|���������敪�T                PIC 9(1).
002320     05 ���|�����敪�U                    PIC 9(1).
002460     05 ���|�������U                      PIC N(16).
002960     05 ���|���񏈒u�񐔂U                PIC 9(1).
002980     05 ���|�����N�����U                  PIC 9(8).
003000     05 ���|�����N�����U                  PIC 9(8).
002540     05 ���|�{�p�J�n���U                  PIC 9(8).
002560     05 ���|�{�p�I�����U                  PIC 9(8).
002580     05 ���|�������U                      PIC 9(2).
003080     05 ���|�]�A�敪�U                    PIC 9(1).
003100     05 ���|��É񐔂U                    PIC 9(2).
003120     05 ���|��㪖@�񐔂U                  PIC 9(1).
003140     05 ���|��㪖@�񐔂U                  PIC 9(2).
003180     05 ���|�d�É񐔂U                    PIC 9(2).
003200     05 ���|�����ʒ����敪�U              PIC 9(1).
003220     05 ���|���������敪�U                PIC 9(1).
002320     05 ���|�����敪�V                    PIC 9(1).
002460     05 ���|�������V                      PIC N(16).
002960     05 ���|���񏈒u�񐔂V                PIC 9(1).
002980     05 ���|�����N�����V                  PIC 9(8).
003000     05 ���|�����N�����V                  PIC 9(8).
002540     05 ���|�{�p�J�n���V                  PIC 9(8).
002560     05 ���|�{�p�I�����V                  PIC 9(8).
002580     05 ���|�������V                      PIC 9(2).
003080     05 ���|�]�A�敪�V                    PIC 9(1).
003100     05 ���|��É񐔂V                    PIC 9(2).
003120     05 ���|��㪖@�񐔂V                  PIC 9(1).
003140     05 ���|��㪖@�񐔂V                  PIC 9(2).
003180     05 ���|�d�É񐔂V                    PIC 9(2).
003200     05 ���|�����ʒ����敪�V              PIC 9(1).
003220     05 ���|���������敪�V                PIC 9(1).
002320     05 ���|�����敪�W                    PIC 9(1).
002460     05 ���|�������W                      PIC N(16).
002960     05 ���|���񏈒u�񐔂W                PIC 9(1).
002980     05 ���|�����N�����W                  PIC 9(8).
003000     05 ���|�����N�����W                  PIC 9(8).
002540     05 ���|�{�p�J�n���W                  PIC 9(8).
002560     05 ���|�{�p�I�����W                  PIC 9(8).
002580     05 ���|�������W                      PIC 9(2).
003080     05 ���|�]�A�敪�W                    PIC 9(1).
003100     05 ���|��É񐔂W                    PIC 9(2).
003120     05 ���|��㪖@�񐔂W                  PIC 9(1).
003140     05 ���|��㪖@�񐔂W                  PIC 9(2).
003180     05 ���|�d�É񐔂W                    PIC 9(2).
003200     05 ���|�����ʒ����敪�W              PIC 9(1).
003220     05 ���|���������敪�W                PIC 9(1).
002080     05 ���|�O�����S����                  PIC 9(2).
002080     05 ���|��o                          PIC 9(1).
002080     05 ���|���Ҕԍ�                      PIC 9(6).
002080     05 ���|�}��                          PIC X(2).
002080     05 ���|��ԍ�                        PIC X(10).
002080     05 ���|�ی����                      PIC X(1).
003600*     05 �w�b�_�p�X�y�[�X                  PIC X(316).
003603*
003604 FD  �ی��҃t�@�C�� RECORD IS VARYING IN SIZE
003605               FROM 1 TO 334 DEPENDING ON �����J�E���^.
003606 01  �ی��|���R�[�h.
003607   03 �ی��|���R�[�h�f�[�^.
003608     05 �ی��|�ڍ��@�h�c                    PIC X(10).
003609     05 �ی��|��ؕ����P                    PIC X.
003610     05 �ی��|�ی����                      PIC 9(2).
003611     05 �ی��|��ؕ����Q                    PIC X.
003612     05 �ی��|�ی��Ҕԍ�                    PIC X(10).
003613     05 �ی��|��ؕ����R                    PIC X.
003614     05 �ی��|�ی��Ҏ��ʂh�c                PIC 9.
003615     05 �ی��|��ؕ����S                    PIC X.
003616     05 �ی��|�ی��Җ���                    PIC X(60).
003617     05 �ی��|��ؕ����T                    PIC X.
003618     05 �ی��|�X���於��                    PIC X(60).
003619     05 �ی��|��ؕ����U                    PIC X.
003620     05 �ی��|�󎚗p����                    PIC X(60).
003621     05 �ی��|��ؕ����V                    PIC X.
003622     05 �ی��|���R�[�h                      PIC X(2).
003623     05 �ی��|��ؕ����W                    PIC X.
003624     05 �ی��|�X�֔ԍ�                      PIC X(7).
003625     05 �ی��|��ؕ����X                    PIC X.
003626     05 �ی��|�Z���P                        PIC X(40).
003627     05 �ی��|��ؕ����P�O                  PIC X.
003628     05 �ی��|�Z���Q                        PIC X(40).
003629     05 �ی��|��ؕ����P�P                  PIC X.
003630     05 �ی��|�s�d�k                        PIC X(15).
003631     05 �ی��|��ؕ����P�Q                  PIC X.
003632     05 �ی��|�{�l���S�敪                  PIC 9(1).
003633     05 �ی��|��ؕ����P�R                  PIC X.
003634     05 �ی��|�{�l���S��                    PIC 9(1).
003635     05 �ی��|��ؕ����P�S                  PIC X.
003636     05 �ی��|�Ƒ����S�敪                  PIC 9(1).
003637     05 �ی��|��ؕ����P�T                  PIC X.
003638     05 �ی��|�Ƒ����S��                    PIC 9(1).
003639     05 �ی��|��ؕ����P�U                  PIC X.
003640     05 �ی��|�����\���                    PIC X(1).
003641     05 �ی��|��ؕ����P�V                  PIC X.
003642     05 �ی��|��������                      PIC X(1).
003643     05 �ی��|��ؕ����P�W                  PIC X.
003644     05 �ی��|���ϋ敪                      PIC 9(1).
003645     05 �ی��|��ؕ����P�X                  PIC X.
003646     05 �ی��|�X�V���t                      PIC X(1).
003647
004060*                           �m�q�k��  256�n
004070 FD  ��ƕی��҃t�@�C�� BLOCK   CONTAINS   328   RECORDS.
004080 01 ��ی��|���R�[�h.
004090   03 ��ی��|���R�[�h�L�[.
004100     05 ��ی��|�ی���ʃL�[                PIC 9(2).
004110     05 ��ی��|�ی��Ҕԍ��L�[              PIC X(10).
004120   03 ��ی��|���R�[�h�f�[�^.
004130     05 ��ی��|�ڍ��@�h�c                  PIC X(10).
004140     05 ��ی��|�ی����                    PIC 9(2).
004141     05 ��ی��|�ی���ʂQ                  PIC X(2).
004150     05 ��ی��|�ی��Ҕԍ�                  PIC X(10).
004160     05 ��ی��|�ی��Ҏ��ʂh�c              PIC 9.
004170     05 ��ی��|�ی��Җ���                  PIC X(60).
004180     05 ��ی��|�X���於��                  PIC X(60).
004190     05 ��ی��|�󎚗p����                  PIC X(60).
004200     05 ��ی��|���R�[�h                    PIC X(2).
004210     05 ��ی��|�X�֔ԍ�                    PIC X(7).
004220     05 ��ی��|�Z���P                      PIC X(40).
004230     05 ��ی��|�Z���Q                      PIC X(40).
004240     05 ��ی��|�s�d�k                      PIC X(15).
004250     05 ��ی��|�{�l���S�敪                PIC 9(1).
004260     05 ��ی��|�{�l���S��                  PIC 9(1).
004270     05 ��ی��|�Ƒ����S�敪                PIC 9(1).
004280     05 ��ی��|�Ƒ����S��                  PIC 9(1).
004290     05 ��ی��|�����\���                  PIC X(1).
004300     05 ��ی��|��������                    PIC X(1).
004310     05 ��ی��|���ϋ敪                    PIC 9(1).
004320     05 ��ی��|�X�V���t                    PIC X(10).
001510*
001520 FD  ��ƃt�@�C���P RECORD  CONTAINS 176 CHARACTERS.
001530 01  ��P�|���R�[�h.
001540     03  ��P�|���R�[�h�L�[.
001590         05  ��P�|�������.
                   07  ��P�|����                PIC 9(2).
001570             07  ��P�|��                  PIC 9(2).
001580             07  ��P�|�ێ�                PIC 9(1).
001720         05  ��P�|�۔�                    PIC X(6).
001600         05  ��P�|���                    PIC X(2).
001710         05  ��P�|�ی��Ҕԍ�              PIC X(10).
001700         05  ��P�|�{�l�Ƒ��敪            PIC 9(1).
001620         05  ��P�|���҃R�[�h.
001630             07  ��P�|���Ҕԍ�            PIC 9(6).
001640             07  ��P�|�}��                PIC X(1).
001650         05  ��P�|�{�p�a��N��.
001660             07  ��P�|�{�p�a��            PIC 9(1).
001670             07  ��P�|�{�p�N              PIC 9(2).
001680             07  ��P�|�{�p��              PIC 9(2).
001690     03  ��Q�|���R�[�h�f�[�^.
001600         05  ��P�|�ی����                PIC 9(2).
001600         05  ��P�|�������                PIC 9(2).
001740         05  ��P�|���Ҏ���                PIC X(50).
001750         05  ��P�|��ی��Ҏ���            PIC X(50).
001880         05  ��P�|������                  PIC 9(2).
001790         05  ��P�|��p�z                  PIC 9(7).
001800         05  ��P�|���S�z                  PIC 9(7).
001810         05  ��P�|�����z                  PIC 9(7).
002360         05  ��P�|�O���敪                PIC 9(1).
002370         05  FILLER                        PIC X(12).
004471*
004472 FD  ���o�t�@�C�� RECORD  CONTAINS 8 CHARACTERS.
004473 01  ���|���R�[�h.
004474     03  ���|���R�[�h�f�[�^.
004475         05  ���|����ԍ�              PIC 9(8).
004480*
004490*----------------------------------------------------------------*
004500******************************************************************
004510*                WORKING-STORAGE SECTION                         *
004520******************************************************************
004530 WORKING-STORAGE         SECTION.
004540 01 �L�[����                           PIC X    VALUE SPACE.
004550 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
004560 01 �I���t���O                         PIC X(3) VALUE SPACE.
004570 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
004580 01 �I���t���O�R                       PIC X(3) VALUE SPACE.
002090 01 �G���[�t���O                       PIC X(3) VALUE SPACE.
004590 01 ���s�L�[�v                         PIC X(3)  VALUE SPACE.
004600 01 �{�p�L�^�L�v                       PIC X(3) VALUE SPACE.
004610 01 �t�@�C����                         PIC N(8) VALUE SPACE.
004620 01 �J�E���^                           PIC 9(1) VALUE ZERO.
004630 01 �����J�E���^                       PIC 9(4) VALUE ZERO.
004631 01 �����J�E���^�Q                     PIC 9(4) VALUE ZERO.
004632 01 �����J�E���^�R                     PIC 9(4) VALUE ZERO.
004633 01 �����J�E���^�S                     PIC 9(4) VALUE ZERO.
004634 01 �����J�E���^�T                     PIC 9(4) VALUE ZERO.
004635 01 �����J�E���^�U                     PIC 9(4) VALUE ZERO.
004640 01 �A�Ԃv                             PIC 9(4) VALUE ZERO.
004650 01 �a����ʂv                         PIC X(1) VALUE SPACE.
004660 01 �_���t�R�[�h�v.
004670    03 ���v                            PIC N(1) VALUE SPACE.
004680    03 �_���t�R�[�h�����v.
004690       05 �_���t�R�[�h�����P�v         PIC X(4) VALUE SPACE.
004700       05 ����ԍ��R�[�h�v             PIC X(3) VALUE SPACE.
004710*
004720 01 ��������N�v                       PIC 9(4) VALUE ZERO.
004730 01 �����a��N���v�q.
004740    03 �����a��v�q                    PIC 9(1) VALUE ZERO.
004750    03 �����N���v�q.
004760       05 �����N�v�q                   PIC 9(2) VALUE ZERO.
004770       05 �������v�q                   PIC 9(2) VALUE ZERO.
004780*
004790 01 ����N�v                           PIC 9(4) VALUE ZERO.
004800* 01 �a��v                             PIC 9(1) VALUE ZERO.
004810* 01 �N�v                               PIC 9(2) VALUE ZERO.
004820 01 �a��N�����v.
004830   03 �a��N���v.
004840      05 �a��v                        PIC 9(1) VALUE ZERO.
004850      05 �N�v                          PIC 9(2) VALUE ZERO.
004860      05 ���v                          PIC 9(2) VALUE ZERO.
004870   03 ���v                             PIC 9(2) VALUE ZERO.
004880 01 ���t�W���v                         PIC 9(8) VALUE ZERO.
004890 01 �J�n�a��N�����v.
004900   03 �J�n�a��v                       PIC 9(1) VALUE ZERO.
004910   03 �J�n�N�v                         PIC 9(2) VALUE ZERO.
004920   03 �J�n���v                         PIC 9(2) VALUE ZERO.
004930   03 �J�n���v                         PIC 9(2) VALUE ZERO.
004940 01 �I���a��N�����v.
004950   03 �I���a��v                       PIC 9(1) VALUE ZERO.
004960   03 �I���N�v                         PIC 9(2) VALUE ZERO.
004970   03 �I�����v                         PIC 9(2) VALUE ZERO.
004980   03 �I�����v                         PIC 9(2) VALUE ZERO.
004990 01 �������v                           PIC 9(2) VALUE ZERO.
005000 01 ���ʂb�m�s                         PIC 9(2) VALUE ZERO.
005010 01 ���ʂb�m�s�Q                       PIC 9(2) VALUE ZERO.
005020 01 �����������v                       PIC 9(2) VALUE ZERO.
005040 01 �p�����ʐ��v                       PIC 9    VALUE ZERO.
005050 01 �ő�p�����ʐ��v                   PIC 9    VALUE ZERO.
005060 01 �����I���a��N�����v               PIC 9(7) VALUE ZERO.
005070 01 ������ʂv                         PIC 9(2) VALUE ZERO.
005070 01 ���Z�v�g�ԍ��v                     PIC 9(4) VALUE ZERO.
005080 01 ���ʃ^�C�v�v                       PIC 9    VALUE ZERO.
005100 01 �������̂v                         PIC N(10) VALUE SPACE.
       01 �L���ԍ��v.
007570    03 �L���v.
007580       05 ����L���v                   PIC N(12)  VALUE SPACE.
007670    03 �ԍ��v.
007680       05 ����ԍ��v                   PIC X(30)  VALUE SPACE.
005090*
005100 01 �x���t���O                         PIC X(3) VALUE SPACE.
005110 01 �x���b�m�s                         PIC 9(5) VALUE ZERO.
005120 01 �x���J�E���^                       PIC 9(4) VALUE ZERO.
005130 01 �x���񐔂v                         PIC 9(4) VALUE ZERO.
005140*
005150 01 �p�������ڂv.
005160   03 �p�������ڂw�v                   PIC X(70) VALUE SPACE.
005170 01 ���{�ꍀ�ڂv.
005180   03 ���{�ꍀ�ڂm�v                   PIC N(35) VALUE SPACE.
005190 01 �p�������ڂQ�v                     PIC X(70) VALUE SPACE.
005200*
005210*�����擾�p���[�N
005220 01 ���a��J�n�N���v                   PIC 9(6) VALUE ZERO.
005230 01 �����a��N���v.
005240    03 �����a��v                      PIC 9    VALUE ZERO.
005250    03 �����N�v                        PIC 9(2) VALUE ZERO.
005260    03 �������v                        PIC 9(2) VALUE ZERO.
005270 01 ��������N���v.
005280    03 ��������N�v                    PIC 9(4) VALUE ZERO.
005290    03 ��������v                    PIC 9(2) VALUE ZERO.
005300 01 �O���a��N���擾�p�v.
005310    03 �����a��v                      PIC 9(1) VALUE ZERO.
005320    03 �����N�v                        PIC 9(2) VALUE ZERO.
005330    03 �������v                        PIC 9(2) VALUE ZERO.
005340*    03 �O���a��v                      PIC 9(1) VALUE ZERO.
005350*    03 �O���N�v                        PIC 9(2) VALUE ZERO.
005360*    03 �O�����v                        PIC 9(2) VALUE ZERO.
005370*--- ��ϔC�p ---*
005380 01 ��ϔC�t���O                     PIC X(3)  VALUE SPACE.
005381 01 �ی��҃f�[�^�v                     PIC X(340)  VALUE SPACE.
005382 01 �ی��Ҕԍ��v                       PIC X(10) VALUE SPACE.
005383 01 �ی��Җ��̂v                       PIC X(60) VALUE SPACE.
005384 01 �X���於�̂v                       PIC X(60) VALUE SPACE.
005385 01 �󎚗p���̂v                       PIC X(60) VALUE SPACE.
005386 01 �Z���P�v                           PIC X(40) VALUE SPACE.
005387 01 �Z���Q�v                           PIC X(40) VALUE SPACE.
      *
       01 �񐔃J�E���^�v.
          03 �����񐔂v                      PIC 9(2)  VALUE ZERO.
          03 ���ԊO�񐔂v                    PIC 9(2)  VALUE ZERO.
          03 �x���񐔂v                      PIC 9(2)  VALUE ZERO.
          03 �[��񐔂v                      PIC 9(2)  VALUE ZERO.
          03 �Č��񐔂v                      PIC 9(2)  VALUE ZERO.
          03 ���É񐔂v                      PIC 9(2)  VALUE ZERO.
          03 ��ԉ񐔂v                      PIC 9(2)  VALUE ZERO.
          03 ��H�񐔂v                      PIC 9(2)  VALUE ZERO.
          03 �\���J��񐔂v                  PIC 9(2)  VALUE ZERO.
          03 ��񐔂v                        PIC 9(2)  VALUE ZERO.
          03 ���񐔂v                        PIC 9(2)  VALUE ZERO.
          03 ���񐔂v                        PIC 9(2)  VALUE ZERO.
          03 ���񋟉񐔂v                  PIC 9(2)  VALUE ZERO.
005630* �ޔ�p
005640 01 �I���a��N�����v�s.
005650    03 �I���a��v�s                    PIC 9(1)  VALUE ZERO.
005660    03 �I���N�v�s                      PIC 9(2)  VALUE ZERO.
005670    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
005680    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
005690*
005700 01 �����Č��t���O                     PIC X(3)  VALUE SPACE.
005710*
005720 01 �����於�̂v                       PIC X(60) VALUE SPACE.
005722
002251 01 �h���C�u�v�q                       PIC X VALUE SPACE.
005723 01 FD-NAME                            PIC X(30) VALUE SPACE.
005724*
005725 01 �ی���ʂv                         PIC X(2)  VALUE SPACE.
014774*
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
005730******************************************************************
005740*                          �A������                              *
005750******************************************************************
005760*
005770********************
005780* ���b�Z�[�W�\���L�[ *
005790********************
005800 01 �A���|�L�[ IS EXTERNAL.
005810    03  �A���|���b�Z�[�W               PIC N(20).
005820*
005830 01 �A���R�|�L�[ IS EXTERNAL.
005840    03  �A���R�|���b�Z�[�W             PIC N(20).
005850    03  �A���R�|���b�Z�[�W�P           PIC X(20).
005860*
005870****************
005880* ��ʓ��͏�� *
005890****************
       01 �A���|��ʏ��x�h�v�T�W�O IS EXTERNAL.
          03 �A���|�����a��N��.
             05 �A���|�����a��               PIC 9.
             05 �A���|�����N                 PIC 9(2).
             05 �A���|������                 PIC 9(2).
          03 �A���|�v���r���[�敪            PIC 9(1).
      *
       01 �A���|��ʏ��x�h�v�P�O�O IS EXTERNAL.
          03 �A���|�h���C�u                  PIC X(1).
006280*
      * �Í������p
       01 �A�Í������|�Í���� IS EXTERNAL.
          03 �A�Í������|���͏��.
             05 �A�Í������|�L��               PIC X(24).
             05 �A�Í������|�ԍ�               PIC X(30).
             05 �A�Í������|�Í�������.
                07 �A�Í������|�Í����Ҕԍ�    PIC X(6).
                07 �A�Í������|�Í�����L��    PIC X.
                07 �A�Í������|�Í�����ԍ�    PIC X.
                07 �A�Í������|�Í��L��        PIC X(24).
                07 �A�Í������|�Í��ԍ�        PIC X(30).
          03 �A�Í������|�o�͏��.
             05 �A�Í������|���������L��       PIC X(24).
             05 �A�Í������|���������ԍ�       PIC X(30).
006290******************************************************************
006300*                      PROCEDURE  DIVISION                       *
006310******************************************************************
006320 PROCEDURE               DIVISION.
006330************
006340*           *
006350* ��������   *
006360*           *
006370************
006380     PERFORM ������.
006390************
006400*           *
006410* �又��     *
006420*           *
006430************
006440     PERFORM ������擾.
006450     PERFORM �A�����ڑޔ�.
006460     PERFORM ���[�U�[�t�@�C���쐬.
006480     PERFORM ���ҏ��t�@�C���쐬.
006490     PERFORM �ی��҃t�@�C���쐬.
006500************
006510*           *
006520* �I������   *
006530*           *
006540************
006550     PERFORM �I������.
006560     MOVE ZERO TO PROGRAM-STATUS.
006570     EXIT PROGRAM.
006580*
006590*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
006600*================================================================*
006610 ������ SECTION.
006620*
006630     PERFORM �t�@�C���I�[�v��.
006640*
006650*================================================================*
006660 �t�@�C���I�[�v�� SECTION.
006670*
006680     OPEN INPUT ��f�ҏ��e
006690         MOVE NC"��" TO �t�@�C����.
006700         PERFORM �I�[�v���`�F�b�N.
004140     OPEN INPUT  ���̃}�X�^
004150         MOVE NC"����" TO �t�@�C����.
004160         PERFORM �I�[�v���`�F�b�N.
006630     OPEN INPUT ���Z�v�g�e.
006640         MOVE NC"���Z" TO �t�@�C����.
006650         PERFORM �I�[�v���`�F�b�N.
006710     OPEN INPUT �����f�[�^�e.
006720             MOVE NC"����" TO �t�@�C����.
006730             PERFORM �I�[�v���`�F�b�N.
014910     OPEN INPUT   ���������e.
014920         MOVE NC"��������" TO �t�@�C����.
014930         PERFORM �I�[�v���`�F�b�N.
002650     OPEN INPUT   �����p���҂e.
002651         MOVE NC"�����p���҂e" TO �t�@�C����.
002652         PERFORM �I�[�v���`�F�b�N.
006740     OPEN INPUT �{�p�L�^�e.
006750             MOVE NC"�{�L" TO �t�@�C����.
006760             PERFORM �I�[�v���`�F�b�N.
006770     OPEN INPUT �{�p�����}�X�^
006780         MOVE NC"�{��" TO �t�@�C����.
006790         PERFORM �I�[�v���`�F�b�N.
006800     OPEN INPUT �����}�X�^.
006810             MOVE NC"����" TO �t�@�C����.
006820             PERFORM �I�[�v���`�F�b�N.
006830     OPEN INPUT �ی��҃}�X�^.
006840             MOVE NC"�ی��҃}�X�^" TO �t�@�C����.
006850             PERFORM �I�[�v���`�F�b�N.
006860     OPEN INPUT ������}�X�^.
006870             MOVE NC"����" TO �t�@�C����.
006880             PERFORM �I�[�v���`�F�b�N.
006890     OPEN INPUT ���S���}�X�^.
006900             MOVE NC"����" TO �t�@�C����.
006910             PERFORM �I�[�v���`�F�b�N.
004870     OPEN INPUT �ی��ғ��ʕ��S�}�X�^.
004880         MOVE NC"�ی��ғ��ʕ��S�}�X�^" TO �t�@�C����.
004890         PERFORM �I�[�v���`�F�b�N.
006920     OPEN INPUT �s�����}�X�^.
006930             MOVE NC"�s��" TO �t�@�C����.
006940             PERFORM �I�[�v���`�F�b�N.
006630     OPEN INPUT ���ۏ��e.
006640         MOVE NC"����" TO �t�@�C����.
006650         PERFORM �I�[�v���`�F�b�N.
006630     OPEN INPUT �J�Џ��e.
006640         MOVE NC"�J��" TO �t�@�C����.
006650         PERFORM �I�[�v���`�F�b�N.
006950     OPEN INPUT   ����}�X�^.
006960         MOVE NC"����}�X�^" TO �t�@�C����.
006970         PERFORM �I�[�v���`�F�b�N.
006980*
006990     OPEN OUTPUT ���[�U�[�t�@�C��.
007000     IF ��ԃL�[  =  "30"
007010         MOVE  NC"�@�@�@�h���C�u������܂���B" TO �A���|���b�Z�[�W
007020         CALL   "MSG001"
007030         CANCEL "MSG001"
007040         MOVE 99 TO PROGRAM-STATUS
007050         EXIT PROGRAM
007060     ELSE
007070         MOVE NC"���[�U�[" TO �t�@�C����
007080         PERFORM �I�[�v���`�F�b�N
007090     END-IF.
007091
007094* �t���b�s�[�}���m�F
007095* �_�~�[�t�@�C�����I�[�v�����A�I�[�v���ł�����t�@�C�����폜
003324     MOVE �A���|�h���C�u TO �h���C�u�v�q.
007096     STRING �h���C�u�v�q       DELIMITED BY SIZE
007097            ":\REZDATA.LZH"    DELIMITED BY SIZE
007098           INTO FD-NAME
007099     END-STRING.
007100
007101     OPEN OUTPUT ���o�t�@�C��.
007102     IF ��ԃL�[  =  "30"
007103         MOVE  NC"�h���C�u������܂���B" TO �A���|���b�Z�[�W
007104         CALL   "MSG001"
007105         CANCEL "MSG001"
007106         MOVE 99 TO PROGRAM-STATUS
007107         EXIT PROGRAM
007108     ELSE
007070         MOVE NC"���o" TO �t�@�C����
007109         PERFORM �I�[�v���`�F�b�N
007110         CLOSE ���o�t�@�C��
007111         CALL "delfile"  WITH C LINKAGE
007112                         USING BY REFERENCE FD-NAME
007113     END-IF.
007130     OPEN OUTPUT �ی��҃t�@�C��.
007140         MOVE NC"�ی���" TO �t�@�C����.
007150         PERFORM �I�[�v���`�F�b�N.
007117     OPEN OUTPUT ���ҏ��t�@�C��.
007118         MOVE NC"���ҏ��" TO �t�@�C����.
007120         PERFORM �I�[�v���`�F�b�N.
007160*================================================================*
007170 �I�[�v���`�F�b�N SECTION.
007180*
007190     IF ��ԃL�[  NOT =  "00"
007200         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
007210         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
007220         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
007230         ACCEPT  �L�[���� FROM CONS
007240         PERFORM �t�@�C����
007250         MOVE 99 TO PROGRAM-STATUS
007260         EXIT PROGRAM.
007270*================================================================*
007280 �t�@�C���� SECTION.
007290*
007300     CLOSE ��f�ҏ��e      �{�p�����}�X�^  �����f�[�^�e    ���̃}�X�^
007310           �����}�X�^        ������}�X�^    �ی��҃}�X�^    �ی��ғ��ʕ��S�}�X�^
007320           ���S���}�X�^      �s�����}�X�^      ����}�X�^    ���ۏ��e
007330           ���[�U�[�t�@�C��  ���ҏ��t�@�C��  �ی��҃t�@�C��  �J�Џ��e
                 ���Z�v�g�e        ���������e        �����p���҂e.
007340*================================================================*
007350 �I������ SECTION.
007360*
007370     PERFORM �t�@�C����.
007380*================================================================*
007390 �G���[�\�� SECTION.
007400*
007410     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
007420     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
007430     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
007440     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������" UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
007450     ACCEPT  �L�[���� FROM CONS.
007460     PERFORM �t�@�C����.
007470     MOVE 99 TO PROGRAM-STATUS.
007480     EXIT PROGRAM.
007490*================================================================*
007500 ���[�U�[�t�@�C���쐬 SECTION.
007510*
007520     PERFORM �{�p�����擾.
007530     PERFORM ����擾.
007540*/�w�b�_
007550     INITIALIZE ���|���R�[�h.
007560     MOVE "[DATA]"           TO ���|���R�[�h�f�[�^.
007570     PERFORM ���[�U�[�t�@�C������.
007580*/���ʁF�O�Œ�
007590     INITIALIZE ���|���R�[�h.
007600     MOVE "����=0"         TO ���|���R�[�h�f�[�^.
007610     PERFORM ���[�U�[�t�@�C������.
007620*/���R�[�h�F�P�R�Œ�
007630     INITIALIZE ���|���R�[�h.
007640*     MOVE "���R�[�h=13"       TO ���|���R�[�h�f�[�^.
            STRING  "���R�[�h="           DELIMITED BY SIZE
                     �{��|�s���{���i�h�r DELIMITED BY SIZE
007700       INTO ���|���R�[�h�f�[�^
007710     END-STRING.
007650     PERFORM ���[�U�[�t�@�C������.
007660*/����J�i
007670     INITIALIZE ���|���R�[�h.
007680     STRING "������i�J�i�j="    DELIMITED BY SIZE
007690            ���|�ڍ��t���J�i DELIMITED BY SIZE
007700       INTO ���|���R�[�h�f�[�^
007710     END-STRING.
007720     PERFORM ���[�U�[�t�@�C������.
007730*/�������
007740     INITIALIZE ���|���R�[�h.
007750     MOVE ���|�ڍ��t���� TO �p�������ڂv.
007760     INSPECT �p�������ڂw�v REPLACING ALL "�@" BY "  ".
007770     STRING "������i�����j=" DELIMITED BY SIZE
007780            �p�������ڂw�v      DELIMITED BY SIZE
007790       INTO ���|���R�[�h�f�[�^
007800     END-STRING.
007810     PERFORM ���[�U�[�t�@�C������.
007820*/�ڍ��@���J�i
007830     INITIALIZE ���|���R�[�h.
007840     STRING "�ڍ��@�i�J�i�j="  DELIMITED BY SIZE
007850            �{��|�ڍ��@���J�i DELIMITED BY SIZE
007860       INTO ���|���R�[�h�f�[�^
007870     END-STRING.
007880     PERFORM ���[�U�[�t�@�C������.
007890*/�ڍ��@��
007900     INITIALIZE ���|���R�[�h.
007910     MOVE �{��|�ڍ��@�� TO �p�������ڂv.
007920     INSPECT �p�������ڂw�v REPLACING ALL "�@" BY "  ".
007930     STRING "�ڍ��@�i�����j=" DELIMITED BY SIZE
007940            �p�������ڂw�v    DELIMITED BY SIZE
007950       INTO ���|���R�[�h�f�[�^
007960     END-STRING.
007970     PERFORM ���[�U�[�t�@�C������.
007980*/��\�҃J�i
007990     INITIALIZE ���|���R�[�h.
008000     STRING "�_���t�����i�J�i�j=" DELIMITED BY SIZE
008010            �{��|��\�҃J�i      DELIMITED BY SIZE
008020       INTO ���|���R�[�h�f�[�^
008030     END-STRING.
008040     PERFORM ���[�U�[�t�@�C������.
008050*/��\�Җ�
008060     INITIALIZE ���|���R�[�h.
008070     MOVE �{��|��\�Җ� TO �p�������ڂv.
008080     INSPECT �p�������ڂw�v REPLACING ALL "�@" BY "  ".
008090     STRING "�_���t�����i�����j=" DELIMITED BY SIZE
008100            �p�������ڂw�v        DELIMITED BY SIZE
008110       INTO ���|���R�[�h�f�[�^
008120     END-STRING.
008130     PERFORM ���[�U�[�t�@�C������.
008140*/�X�֔ԍ�
008150     INITIALIZE ���|���R�[�h.
008160     STRING "�X�֔ԍ�="    DELIMITED BY SIZE
008170            �{��|�X�֔ԍ� DELIMITED BY SIZE
008180       INTO ���|���R�[�h�f�[�^
008190     END-STRING.
008200     PERFORM ���[�U�[�t�@�C������.
008210*/�Z���P
008220     INITIALIZE ���|���R�[�h.
008230     MOVE �{��|�Z���P TO �p�������ڂv.
008240     INSPECT �p�������ڂw�v REPLACING ALL "�@" BY "  ".
008250     STRING "�Z��1="       DELIMITED BY SIZE
008260            �p�������ڂw�v DELIMITED BY SIZE
008270       INTO ���|���R�[�h�f�[�^
008280     END-STRING.
008290     PERFORM ���[�U�[�t�@�C������.
008300*/�Z���Q
008310     INITIALIZE ���|���R�[�h.
008320     MOVE �{��|�Z���Q TO �p�������ڂv.
008330     INSPECT �p�������ڂw�v REPLACING ALL "�@" BY "  ".
008340     STRING "�Z��2="       DELIMITED BY SIZE
008350            �p�������ڂw�v DELIMITED BY SIZE
008360       INTO ���|���R�[�h�f�[�^
008370     END-STRING.
008380     PERFORM ���[�U�[�t�@�C������.
008390*/�d�b�ԍ�
008400     INITIALIZE ���|���R�[�h.
008410     STRING "TEL="         DELIMITED BY SIZE
008420            �{��|�d�b�ԍ� DELIMITED BY SIZE
008430       INTO ���|���R�[�h�f�[�^
008440     END-STRING.
008450     PERFORM ���[�U�[�t�@�C������.
008460*/��s��
008470     INITIALIZE ���|���R�[�h.
008480     STRING "��s��="          DELIMITED BY SIZE
008490            �{��|������s�� DELIMITED BY SIZE
008500       INTO ���|���R�[�h�f�[�^
008510     END-STRING.
008520     PERFORM ���[�U�[�t�@�C������.
008530*/�x�X��
008540     INITIALIZE ���|���R�[�h.
008550     STRING "�x�X��="              DELIMITED BY SIZE
008560            �{��|������s�x�X�� DELIMITED BY SIZE
008570       INTO ���|���R�[�h�f�[�^
008580     END-STRING.
008590     PERFORM ���[�U�[�t�@�C������.
008600*/�����ԍ�
008610     INITIALIZE ���|���R�[�h.
008620     STRING "�����ԍ�="    DELIMITED BY SIZE
008630            �{��|�����ԍ� DELIMITED BY SIZE
008640       INTO ���|���R�[�h�f�[�^
008650     END-STRING.
008660     PERFORM ���[�U�[�t�@�C������.
008670*/�a����ʁA�O�F���ʁA�P�F�����E�_�ł͂P�F���ʁA�Q�F����
008680     INITIALIZE ���|���R�[�h.
008690     EVALUATE �{��|�a�����
008700     WHEN 1
008710         MOVE ZERO  TO �a����ʂv
008720     WHEN 2
008730         MOVE 1     TO �a����ʂv
008740     WHEN OTHER
008750         MOVE SPACE TO �a����ʂv
008760     END-EVALUATE
008770     STRING "�a�����=" DELIMITED BY SIZE
008780            �a����ʂv  DELIMITED BY SIZE
008790       INTO ���|���R�[�h�f�[�^
008800     END-STRING.
008810     PERFORM ���[�U�[�t�@�C������.
008820*/�������`�l�J�i
008830     INITIALIZE ���|���R�[�h.
008840     STRING "�������`�i�J�i�j"   DELIMITED BY SIZE
008850            �{��|�������`�l�J�i DELIMITED BY SIZE
008860       INTO ���|���R�[�h�f�[�^
008870     END-STRING.
008880     PERFORM ���[�U�[�t�@�C������.
008890*/�������`�l��
008900     INITIALIZE ���|���R�[�h.
008910     MOVE �{��|�������`�l TO �p�������ڂv.
008920     INSPECT �p�������ڂw�v REPLACING ALL "�@" BY "  ".
008930     STRING "�������`�i�����j=" DELIMITED BY SIZE
008940            �p�������ڂw�v      DELIMITED BY SIZE
008950       INTO ���|���R�[�h�f�[�^
008960     END-STRING.
008970     PERFORM ���[�U�[�t�@�C������.
008980*/�V�_���t�ԍ�
008990     INITIALIZE ���|���R�[�h.
009000     STRING "�_���t�R�[�h�i�����j=" DELIMITED BY SIZE
009010            �{��|�V�_���t�ԍ� DELIMITED BY SIZE
009020       INTO ���|���R�[�h�f�[�^
009030     END-STRING.
009040     PERFORM ���[�U�[�t�@�C������.
009050*/�_���t�R�[�h
009060     INITIALIZE ���|���R�[�h.
009070     MOVE �{��|�V�_���t�ԍ� TO �_���t�R�[�h�v
009080     STRING "�_���t�R�[�h�i�����j=" DELIMITED BY SIZE
009090            �_���t�R�[�h�����v      DELIMITED BY SIZE
009100       INTO ���|���R�[�h�f�[�^
009110     END-STRING.
009120     PERFORM ���[�U�[�t�@�C������.
009130*/����ԍ�
009140     INITIALIZE ���|���R�[�h.
009150     STRING "����ԍ�="            DELIMITED BY SIZE
009160*            ����ԍ��R�[�h�v       DELIMITED BY SIZE
009160            �{��|�ڍ��t�����ԍ� DELIMITED BY SIZE
009170       INTO ���|���R�[�h�f�[�^
009180     END-STRING.
009190     PERFORM ���[�U�[�t�@�C������.
009200*/�{�p�N��
009210     INITIALIZE ���|���R�[�h.
009220     MOVE �����a��v�q TO �a��v.
009230     MOVE �����N�v�q   TO �N�v.
009240     PERFORM ����N�擾.
009250     STRING "�����N��=" DELIMITED BY SIZE
009260             ����N�v   DELIMITED BY SIZE
009270            "/"         DELIMITED BY SIZE
009280            �������v�q  DELIMITED BY SIZE
009290       INTO ���|���R�[�h�f�[�^
009300     END-STRING.
009310     PERFORM ���[�U�[�t�@�C������.
026650     PERFORM �x������.
009320*================================================================*
009330 ���[�U�[�t�@�C������ SECTION.
009340*
009350     MOVE SPACE TO �I���t���O.
009360     PERFORM VARYING �����J�E���^ FROM 50 BY -1
009370             UNTIL   (�����J�E���^  <= ZERO ) OR
009380                     (�I���t���O NOT = SPACE)
009390         IF ���|���R�[�h�f�[�^(�����J�E���^:1) NOT = SPACE
009400             COMPUTE �����J�E���^ = �����J�E���^ + 1
009410             MOVE "YES" TO �I���t���O
009420         END-IF
009430     END-PERFORM.
009440*
009450     WRITE ���|���R�[�h
009460     IF ��ԃL�[  NOT =  "00"
009470         MOVE NC"���[�U�["  TO �t�@�C����
009480         PERFORM �G���[�\��
009490     END-IF.
009500*================================================================*
009510 �{�p�����擾 SECTION.
009520*
009530     MOVE ZERO  TO �{��|�{�p���ԍ�.
009540     READ �{�p�����}�X�^
009550     INVALID KEY
009560          MOVE  NC"�{�p�����}�X�^�ɓo�^��A���s���ĉ�����" TO �A���|���b�Z�[�W
009570          CALL   "MSG001"
009580          CANCEL "MSG001"
009590          PERFORM �t�@�C����
009600          MOVE 99 TO PROGRAM-STATUS
009610          EXIT PROGRAM
009620     END-READ.
009630*
009640*================================================================*
009650 ����擾 SECTION.
009660*
009680     MOVE ZERO           TO ���|�_���I���敪.
009670     MOVE ���|����R�[�h TO ���|����R�[�h.
009680     MOVE ZERO           TO ���|�ی����.
009690     MOVE ZERO           TO ���|�ύX�a��N��.
009700*
009720     READ ����}�X�^
009730     INVALID KEY
009740         MOVE  NC"�Y������ڍ��t���񂪂���܂���" TO �A���|���b�Z�[�W
009750         CALL   "MSG001"
009760         CANCEL "MSG001"
009770         PERFORM �t�@�C����
009780         MOVE 99 TO PROGRAM-STATUS
009790         EXIT PROGRAM
009800     END-READ.
009810*================================================================*
009820 �A�����ڑޔ� SECTION.
009830*
009840     MOVE �A���|�����a�� TO �����a��v�q.
009850     MOVE �A���|�����N   TO �����N�v�q.
009860     MOVE �A���|������   TO �������v�q.
009860     MOVE �A���|�h���C�u TO �h���C�u�v�q.
009870*================================================================*
009880 ����N�擾 SECTION.
009890*
009900     MOVE ZERO   TO ����N�v.
009910     MOVE �a��v TO ���|�����敪.
009920     READ �����}�X�^
009930     NOT INVALID KEY
009940         COMPUTE ����N�v = ���|�J�n����N + �N�v - 1
009950     END-READ.
010650*================================================================*
010660 ���ҏ��t�@�C������ SECTION.
010670*
010680     WRITE ���|���R�[�h
010690     IF ��ԃL�[  NOT =  "00"
010700         MOVE NC"���ҏ��"  TO �t�@�C����
010710         PERFORM �G���[�\��
010720     END-IF.
026650     PERFORM �x������.
010730*================================================================*
010740 ��f�ҏ��e�Ǎ� SECTION.
010750*
010760     READ ��f�ҏ��e NEXT
010770     AT END
010780         MOVE "YES" TO �I���t���O
010790     END-READ.
010800*================================================================*
010810 ���ۃ��R�[�h�Z�b�g SECTION.
010820*
010830*/����
010840     MOVE ���Z�|���v          TO ���|��p�z.
010850     MOVE ���Z�|�ꕔ���S��    TO ���|�ꕔ���S��.
010860     MOVE ���Z�|�������z      TO ���|�������z.
011000*================================================================*
011130 �������R�[�h�Z�b�g SECTION.
011140*
011150*/����
011160*/��p�z�͌��ۂ̂���
011170     MOVE ���Z�|���v               TO ���|��p�z.
011180     MOVE ���Z�|�󋋎ҕ��S�z       TO ���|�ꕔ���S��.
011190     MOVE ���Z�|�����������z       TO ���|�������z.
011200*** ���ےP�� (�����z�� �����z+�����z����)
011210     IF ( ��|�ی����    = ZERO ) AND
011220        ( ��|�ی��Ҕԍ�  = SPACE )
010860         MOVE ���Z�|�������z       TO ���|�������z
011240     END-IF.
011400*================================================================*
011410 ���ʃ��R�[�h�Z�b�g SECTION.
011420*
011430     MOVE SPACE TO ���|���R�[�h.
011440     INITIALIZE    ���|���R�[�h.
011450*/����ԍ�
011460     PERFORM �{�p�����擾.
           MOVE �{��|�ڍ��t�����ԍ�(1:4) TO ���|����ԍ�.
012410*/��z���󗝋敪�D�O�Œ�
012420     MOVE ZERO                       TO ���|��z���󗝋敪.
012150*/�����N��
012200     MOVE �����a��v�q               TO �a��v.
012210     MOVE �����N�v�q                 TO �N�v.
012220     PERFORM ����N�擾.
012240     COMPUTE ���|�����N�� = (����N�v * 100) + �������v�q.
      */���Z�v�g�ԍ�
           MOVE ���Z�v�g�ԍ��v             TO ���|���Z�v�g�ԍ�.
           COMPUTE ���Z�v�g�ԍ��v = ���Z�v�g�ԍ��v + 1.
012250*/�{�p�N���D2003��103(�}�C�i�X1900)
012260     MOVE ��|�{�p�a��               TO �a��v.
012270     MOVE ��|�{�p�N                 TO �N�v.
012280     PERFORM ����N�擾.
012300     COMPUTE ���|�{�p�N�� = (����N�v * 100) + ��|�{�p��.
      */�ی���� �P�F��ʁE�㍂ �Q�F�V�l �R�F����
           IF ���Z�|���Z��� = 2
              MOVE 1                       TO ���|���Z���
           ELSE
              MOVE ���Z�|���Z���          TO ���|���Z���
           END-IF.
011870*/�ی��؋L���ԍ�
           MOVE SPACE TO �A�Í������|�Í����.
      *
      *    / �A�Í������|���͏��Z�b�g /
           MOVE ��|�L��       TO �A�Í������|�L��.
           MOVE ��|�ԍ�       TO �A�Í������|�ԍ�.
           MOVE ��|�Í������� TO �A�Í������|�Í�������.
      *     
           CALL   �����v���O�������v.
           CANCEL �����v���O�������v.
      *
           MOVE �A�Í������|���������L�� TO �L���v.
           MOVE �A�Í������|���������ԍ� TO �ԍ��v.
011880     IF �L���v(1:2) NOT = "��"
011890         MOVE �L���v                 TO ���|�ی��؋L��
011900     END-IF.
011910     IF (�ԍ��v(1:1) NOT = "*") AND (�ԍ��v(1:2) NOT = "��")
011920         MOVE �ԍ��v                 TO ���|�ی��ؔԍ�
011930     END-IF.
      */�ی��Ҕԍ�
           MOVE ��|�ی��Ҕԍ�             TO ���|�ی��Ҕԍ�.
           IF ��|�ی���� = 70 
              MOVE ��|�{�p�a��N��        TO �J�Ё|�{�p�a��N��
              MOVE ��|���҃R�[�h          TO �J�Ё|���҃R�[�h
              READ �J�Џ��e
              NOT INVALID KEY
                 MOVE �J�Ё|�J���ی��ԍ�   TO ���|�ی��Ҕԍ�
              END-READ
           END-IF.
           IF ��|�ی���� = 85 
              MOVE ��|�{�p�a��N��        TO ���ہ|�{�p�a��N��
              MOVE ��|���҃R�[�h          TO ���ہ|���҃R�[�h
              READ ���ۏ��e
              NOT INVALID KEY
                 MOVE ���ہ|���S�Ҕԍ�     TO ���|�ی��Ҕԍ�
                 MOVE ���ہ|���ۋL���ԍ�   TO ���|�ی��؋L��
              END-READ
           END-IF.
011690*/�V�l
011700     IF (��|������ NOT = ZERO) AND (��|�{�p�a��N�� < 42004)
011710         MOVE ��|��p���S�Ҕԍ�     TO ���|�V���s�����ԍ�
011730         MOVE ��|��v�Ҕԍ��V�l     TO ���|�V���󋋎Ҕԍ�
           ELSE
               MOVE SPACE                  TO ���|�V���s�����ԍ�
                                              ���|�V���󋋎Ҕԍ�
011740     END-IF.
011750*/����
011760     IF (��|�������           NOT = ZERO ) AND
011770        (��|��p���S�Ҕԍ����� NOT = SPACE)
011790         MOVE ��|��p���S�Ҕԍ����� TO ���|�������S�Ҕԍ�
011820         IF (��|��v�Ҕԍ�����(1:1) NOT = "*" ) AND 
011830            (��|��v�Ҕԍ�����(1:2) NOT = "��")
011840             MOVE ��|��v�Ҕԍ����� TO ���|�����󋋎Ҕԍ�
011850         END-IF
           ELSE
               MOVE SPACE                  TO ���|�������S�Ҕԍ�
                                              ���|�����󋋎Ҕԍ�
011860     END-IF.
011580*/���O
011620     MOVE ��|��ی��Ҏ���           TO ���|��ی��Ҏ�������.
011610     MOVE ��|��ی��҃J�i           TO ���|��ی��Ҏ����J�i.
011600     MOVE ��|���Ҏ���               TO ���|���Ҏ�������.
011590     MOVE ��|���҃J�i               TO ���|���Ҏ����J�i.
010960     MOVE ��|�{�l�Ƒ��敪           TO ���|�{�l�Ƒ��敪.
011980*/���ʂP�F�j�A�Q�F��
012000     MOVE ��|���Ґ���               TO ���|����.
011940*/���Ґ��N����
011950     MOVE ��|���Ґ��N����           TO �a��N�����v.
011960     PERFORM ���t�ϊ��W��.
011970     MOVE ���t�W���v                 TO ���|���N����.
011940*/��ی��ҏZ��
011620     MOVE ��|�Z���P                 TO ���|��ی��ҏZ��.
           IF ��|�ی���� > 60
011620        MOVE ��|���Ҏ���            TO ���|��ی��Ҏ�������
011610        MOVE ��|���҃J�i            TO ���|��ی��Ҏ����J�i
011620        MOVE ��|���ҏZ���P          TO ���|��ی��ҏZ��
           END-IF.
011940*/���t����
011620     MOVE ���Z�|���t����             TO ���|���t����.
012430*/���ʐ�
012440     MOVE ���|���ʐ�                 TO ���|���ʐ�.
012430*/��������
032870     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
032880             UNTIL ( ���ʂb�m�s > ���|���ʐ� ) OR
                         ( ���|�����A��(���ʂb�m�s)  NOT = ZERO )
               CONTINUE
           END-PERFORM.
033200     MOVE 01                           TO �����|�敪�R�[�h.
033210     MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �����|���Ҕԍ�.
033220     MOVE ���|�����A��(���ʂb�m�s)     TO �����|���������A��.
033230     READ ���������e
033240     NOT INVALID KEY
012440        MOVE �����|���������b�l(1)     TO ���|��������
           END-READ.
012430*/�������R
004502     MOVE ��|�{�p�a��                 TO ���p�|�{�p�a��
004503     MOVE ��|�{�p�N                   TO ���p�|�{�p�N.
004504     MOVE ��|�{�p��                   TO ���p�|�{�p��.
004505     MOVE ��|���Ҕԍ�                 TO ���p�|���Ҕԍ�.
004506     MOVE ��|�}��                     TO ���p�|�}��.
004507*
004508     READ �����p���҂e 
004522     NOT INVALID KEY
004523         MOVE ���p�|���R��(1)          TO ���|�������R
004538     END-READ.
028280* �V�K/�p��
033380     EVALUATE ���Z�|���Z�����敪
           WHEN 1
033390         MOVE 1                        TO ���|�V�K
033410         MOVE ZERO                     TO ���|�p��
           WHEN 2
033390         MOVE ZERO                     TO ���|�V�K
033410         MOVE 1                        TO ���|�p��
033400     WHEN 3
033390         MOVE 1                        TO ���|�V�K
033410         MOVE 1                        TO ���|�p��
033420     END-EVALUATE.
      */�����A���ÁA���̑�
           PERFORM �{�p���ݒ�.
           MOVE �����񐔂v                   TO ���|������.
           MOVE ���ԊO�񐔂v                 TO ���|�������ԊO��.
           MOVE �x���񐔂v                   TO ���|�����x����.
           MOVE �[��񐔂v                   TO ���|�����[���.
           MOVE �Č��񐔂v                   TO ���|�Č���.
           COMPUTE ���|���Ë��� = ���Z�|���Ë��� * 10.
           MOVE ���É񐔂v                   TO ���|���É�.
           MOVE ��ԉ񐔂v                   TO ���|���Ö�ԉ�.
           MOVE ��H�񐔂v                   TO ���|���Ó�H��.
           MOVE �\���J��񐔂v               TO ���|���Ö\���J���.
           MOVE ��񐔂v                     TO ���|�������q���.
           MOVE ���񐔂v                     TO ���|�������q����.
           MOVE ���񐔂v                     TO ���|�������q����.
           MOVE ���񋟉񐔂v               TO ���|���񋟉�.
012490*/������ʂP�F���܁A�Q�F�s�S���܁A�R�F�E�P�A�S�F�Ŗo�A�T�F�P���A�U�F�����@�V�F�S�k��Â���
012500*/�_�ł͂P�F�P���A�Q�F�Ŗo�A�R�F�����A    �S�F�E�P�A
012510*/      �T�F���܁A�U�F�s���A�V�F���܍S�k�A�W�F�s�S�S�k�A
012520     MOVE ���|�������(1)              TO ������ʂv.
012530     PERFORM ���ʃ^�C�v�擾.
012540     MOVE ���ʃ^�C�v�v                 TO ���|�����敪�P.
      */������
027440     MOVE 03                           TO ���|�敪�R�[�h.
027450     MOVE ���|�������(1)              TO ���|���̃R�[�h.
027460     READ ���̃}�X�^
027470     INVALID KEY
027480         MOVE SPACE                    TO �������̂v
027490     NOT INVALID KEY
027500         MOVE ���|��������             TO �������̂v
027510     END-READ.
006490     STRING ���Z�|���ʖ��̂P(1)  DELIMITED BY SPACE
009980            �������̂v           DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(1)  DELIMITED BY SPACE
006520       INTO ���|�������P
006570     END-STRING.
      */���񏈒u��
           IF ���Z�|���񏈒u��(1) NOT = ZERO
              MOVE 1                         TO ���|���񏈒u�񐔂P
           END-IF.
012310*/�����N����
012320     MOVE ���|�����a��N����(1)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����P.
012310*/�{�p�J�n���F�ŏ��̒ʉ@��
012720     MOVE ���|�J�n�a��N����(1)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����P
                                                ���|�{�p�J�n���P.
012350*/�{�p�I�����F�ŏI�ʉ@���H
012720     MOVE ���|�I���a��N����(1)        TO �a��N�����v.
012370     PERFORM ���t�ϊ��W��.
012380     MOVE ���t�W���v                   TO ���|�{�p�I�����P.
012390*/������
012400     MOVE ���Z�|���ʎ�����(1)          TO ���|�������P.
012390*/�]�A�敪
           EVALUATE ���|�]�A�敪(1)
           WHEN 3
012400         MOVE 2                        TO ���|�]�A�敪�P
           WHEN 4
012400         MOVE 3                        TO ���|�]�A�敪�P
           WHEN 9
012400         MOVE ZERO                     TO ���|�]�A�敪�P
           WHEN OTHER
012400         MOVE 1                        TO ���|�]�A�敪�P
           END-EVALUATE.
012390*/��É�
012400     MOVE ���Z�|��É񐔂P             TO ���|��É񐔂P.
012390*/��㪖@��
012400     MOVE ���Z�|��㪖@�񐔂P           TO ���|��㪖@�񐔂P.
012390*/��㪖@��
012400     MOVE ���Z�|��㪖@�񐔂P           TO ���|��㪖@�񐔂P.
012390*/�d�É�
012400     MOVE ���Z�|�d�É񐔂P             TO ���|�d�É񐔂P.
012390*/�����ʒ����敪
012400     MOVE ZERO                         TO ���|�����ʒ����敪�P.
012390*/���������敪
           IF ���Z�|�����������P NOT = ZERO
012400         MOVE 1                        TO ���|���������敪�P
           END-IF.
012490*/������ʂP�F���܁A�Q�F�s�S���܁A�R�F�E�P�A�S�F�Ŗo�A�T�F�P���A�U�F�����@�V�F�S�k��Â���
012500*/�_�ł͂P�F�P���A�Q�F�Ŗo�A�R�F�����A    �S�F�E�P�A
012510*/      �T�F���܁A�U�F�s���A�V�F���܍S�k�A�W�F�s�S�S�k�A
012520     MOVE ���|�������(2)              TO ������ʂv.
012530     PERFORM ���ʃ^�C�v�擾.
012540     MOVE ���ʃ^�C�v�v                 TO ���|�����敪�Q.
      */������
027440     MOVE 03                           TO ���|�敪�R�[�h.
027450     MOVE ���|�������(2)              TO ���|���̃R�[�h.
027460     READ ���̃}�X�^
027470     INVALID KEY
027480         MOVE SPACE                    TO �������̂v
027490     NOT INVALID KEY
027500         MOVE ���|��������             TO �������̂v
027510     END-READ.
006490     STRING ���Z�|���ʖ��̂P(2)  DELIMITED BY SPACE
009980            �������̂v           DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(2)  DELIMITED BY SPACE
006520       INTO ���|�������Q
006570     END-STRING.
      */���񏈒u��
           IF ���Z�|���񏈒u��(2) NOT = ZERO
              MOVE 1                         TO ���|���񏈒u�񐔂Q
           END-IF.
012310*/�����N����
012320     MOVE ���|�����a��N����(2)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����Q.
012700*/������
012310*/�{�p�J�n���F�ŏ��̒ʉ@��
012720     MOVE ���|�J�n�a��N����(2)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����Q
                                                ���|�{�p�J�n���Q.
012350*/�{�p�I�����F�ŏI�ʉ@���H
012720     MOVE ���|�I���a��N����(2)        TO �a��N�����v.
012370     PERFORM ���t�ϊ��W��.
012380     MOVE ���t�W���v                   TO ���|�{�p�I�����Q.
012390*/������
012400     MOVE ���Z�|���ʎ�����(2)          TO ���|�������Q.
012390*/�]�A�敪
           EVALUATE ���|�]�A�敪(2)
           WHEN 3
012400         MOVE 2                        TO ���|�]�A�敪�Q
           WHEN 4
012400         MOVE 3                        TO ���|�]�A�敪�Q
           WHEN 9
012400         MOVE ZERO                     TO ���|�]�A�敪�Q
           WHEN OTHER
012400         MOVE 1                        TO ���|�]�A�敪�Q
           END-EVALUATE.
012390*/��É�
012400     MOVE ���Z�|��É񐔂Q             TO ���|��É񐔂Q.
012390*/��㪖@��
012400     MOVE ���Z�|��㪖@�񐔂Q           TO ���|��㪖@�񐔂Q.
012390*/��㪖@��
012400     MOVE ���Z�|��㪖@�񐔂Q           TO ���|��㪖@�񐔂Q.
012390*/�d�É�
012400     MOVE ���Z�|�d�É񐔂Q             TO ���|�d�É񐔂Q.
012390*/�����ʒ����敪
012400     MOVE ZERO                         TO ���|�����ʒ����敪�Q.
012390*/���������敪
           IF ���Z�|�����������Q NOT = ZERO
012400         MOVE 1                        TO ���|���������敪�Q
           END-IF.
012490*/������ʂP�F���܁A�Q�F�s�S���܁A�R�F�E�P�A�S�F�Ŗo�A�T�F�P���A�U�F�����@�V�F�S�k��Â���
012500*/�_�ł͂P�F�P���A�Q�F�Ŗo�A�R�F�����A    �S�F�E�P�A
012510*/      �T�F���܁A�U�F�s���A�V�F���܍S�k�A�W�F�s�S�S�k�A
012520     MOVE ���|�������(3)              TO ������ʂv.
012530     PERFORM ���ʃ^�C�v�擾.
012540     MOVE ���ʃ^�C�v�v                 TO ���|�����敪�R.
      */������
027440     MOVE 03                           TO ���|�敪�R�[�h.
027450     MOVE ���|�������(3)              TO ���|���̃R�[�h.
027460     READ ���̃}�X�^
027470     INVALID KEY
027480         MOVE SPACE                    TO �������̂v
027490     NOT INVALID KEY
027500         MOVE ���|��������             TO �������̂v
027510     END-READ.
006490     STRING ���Z�|���ʖ��̂P(3)  DELIMITED BY SPACE
009980            �������̂v           DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(3)  DELIMITED BY SPACE
006520       INTO ���|�������R
006570     END-STRING.
      */���񏈒u��
           IF ���Z�|���񏈒u��(3) NOT = ZERO
              MOVE 1                         TO ���|���񏈒u�񐔂R
           END-IF.
012310*/�����N����
012320     MOVE ���|�����a��N����(3)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����R.
012700*/������
012310*/�{�p�J�n���F�ŏ��̒ʉ@��
012720     MOVE ���|�J�n�a��N����(3)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����R
                                                ���|�{�p�J�n���R.
012350*/�{�p�I�����F�ŏI�ʉ@���H
012720     MOVE ���|�I���a��N����(3)        TO �a��N�����v.
012370     PERFORM ���t�ϊ��W��.
012380     MOVE ���t�W���v                   TO ���|�{�p�I�����R.
012390*/������
012400     MOVE ���Z�|���ʎ�����(3)          TO ���|�������R.
012390*/�]�A�敪
           EVALUATE ���|�]�A�敪(3)
           WHEN 3
012400         MOVE 2                        TO ���|�]�A�敪�R
           WHEN 4
012400         MOVE 3                        TO ���|�]�A�敪�R
           WHEN 9
012400         MOVE ZERO                     TO ���|�]�A�敪�R
           WHEN OTHER
012400         MOVE 1                        TO ���|�]�A�敪�R
           END-EVALUATE.
012390*/��É�
012400     COMPUTE ���|��É񐔂R = ���Z�|��É񐔂R�O + ���Z�|��É񐔂R�W.
012390*/��㪖@��
012400     COMPUTE ���|��㪖@�񐔂R = ���Z�|��㪖@�񐔂R�O + ���Z�|��㪖@�񐔂R�W.
012390*/��㪖@��
012400     COMPUTE ���|��㪖@�񐔂R = ���Z�|��㪖@�񐔂R�O + ���Z�|��㪖@�񐔂R�W.
012390*/�d�É�
012400     COMPUTE ���|�d�É񐔂R = ���Z�|�d�É񐔂R�O + ���Z�|�d�É񐔂R�W.
012390*/�����ʒ����敪
           IF ���Z�|���v�R�W NOT = ZERO
012400        MOVE 1                         TO ���|�����ʒ����敪�R
           END-IF.
012390*/���������敪
           IF (���Z�|�����������R�W NOT = ZERO) OR (���Z�|�����������R�O NOT = ZERO)
012400         MOVE 1                        TO ���|���������敪�R
           END-IF.
012490*/������ʂP�F���܁A�Q�F�s�S���܁A�R�F�E�P�A�S�F�Ŗo�A�T�F�P���A�U�F�����@�V�F�S�k��Â���
012500*/�_�ł͂P�F�P���A�Q�F�Ŗo�A�R�F�����A    �S�F�E�P�A
012510*/      �T�F���܁A�U�F�s���A�V�F���܍S�k�A�W�F�s�S�S�k�A
012520     MOVE ���|�������(4)              TO ������ʂv.
012530     PERFORM ���ʃ^�C�v�擾.
012540     MOVE ���ʃ^�C�v�v                 TO ���|�����敪�S.
      */������
027440     MOVE 03                           TO ���|�敪�R�[�h.
027450     MOVE ���|�������(4)              TO ���|���̃R�[�h.
027460     READ ���̃}�X�^
027470     INVALID KEY
027480         MOVE SPACE                    TO �������̂v
027490     NOT INVALID KEY
027500         MOVE ���|��������             TO �������̂v
027510     END-READ.
006490     STRING ���Z�|���ʖ��̂P(4)  DELIMITED BY SPACE
009980            �������̂v           DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(4)  DELIMITED BY SPACE
006520       INTO ���|�������S
006570     END-STRING.
      */���񏈒u��
           IF ���Z�|���񏈒u��(4) NOT = ZERO
              MOVE 1                         TO ���|���񏈒u�񐔂S
           END-IF.
012310*/�����N����
012320     MOVE ���|�����a��N����(4)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����S.
012700*/������
012310*/�{�p�J�n���F�ŏ��̒ʉ@��
012720     MOVE ���|�J�n�a��N����(4)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����S
                                                ���|�{�p�J�n���S.
012350*/�{�p�I�����F�ŏI�ʉ@���H
012720     MOVE ���|�I���a��N����(4)        TO �a��N�����v.
012370     PERFORM ���t�ϊ��W��.
012380     MOVE ���t�W���v                   TO ���|�{�p�I�����S.
012390*/������
012400     MOVE ���Z�|���ʎ�����(4)          TO ���|�������S.
012390*/�]�A�敪
           EVALUATE ���|�]�A�敪(4)
           WHEN 3
012400         MOVE 2                        TO ���|�]�A�敪�S
           WHEN 4
012400         MOVE 3                        TO ���|�]�A�敪�S
           WHEN 9
012400         MOVE ZERO                     TO ���|�]�A�敪�S
           WHEN OTHER
012400         MOVE 1                        TO ���|�]�A�敪�S
           END-EVALUATE.
012390*/��É�
012400     COMPUTE ���|��É񐔂S = ���Z�|��É񐔂S�O + ���Z�|��É񐔂S�W +
                                    ���Z�|��É񐔂S�T.
012390*/��㪖@��
012400     COMPUTE ���|��㪖@�񐔂S = ���Z�|��㪖@�񐔂S�O + ���Z�|��㪖@�񐔂S�W +
                                      ���Z�|��㪖@�񐔂S�T.
012390*/��㪖@��
012400     COMPUTE ���|��㪖@�񐔂S = ���Z�|��㪖@�񐔂S�O + ���Z�|��㪖@�񐔂S�W +
                                      ���Z�|��㪖@�񐔂S�T.
012390*/�d�É�
012400     COMPUTE ���|�d�É񐔂S = ���Z�|�d�É񐔂S�O + ���Z�|�d�É񐔂S�W +
                                    ���Z�|�d�É񐔂S�T.
012390*/�����ʒ����敪
           IF (���Z�|���v�S�W NOT = ZERO) OR (���Z�|���v�S�T NOT = ZERO)
012400        MOVE 1                         TO ���|�����ʒ����敪�S
           END-IF.
012390*/���������敪
           IF (���Z�|�����������S�W NOT = ZERO) OR
              (���Z�|�����������S�T NOT = ZERO) OR
              (���Z�|�����������S�O NOT = ZERO)
012400         MOVE 1                        TO ���|���������敪�S
           END-IF.
012490*/������ʂP�F���܁A�Q�F�s�S���܁A�R�F�E�P�A�S�F�Ŗo�A�T�F�P���A�U�F�����@�V�F�S�k��Â���
012500*/�_�ł͂P�F�P���A�Q�F�Ŗo�A�R�F�����A    �S�F�E�P�A
012510*/      �T�F���܁A�U�F�s���A�V�F���܍S�k�A�W�F�s�S�S�k�A
012520     MOVE ���|�������(5)              TO ������ʂv.
012530     PERFORM ���ʃ^�C�v�擾.
012540     MOVE ���ʃ^�C�v�v                 TO ���|�����敪�T.
      */������
027440     MOVE 03                           TO ���|�敪�R�[�h.
027450     MOVE ���|�������(5)              TO ���|���̃R�[�h.
027460     READ ���̃}�X�^
027470     INVALID KEY
027480         MOVE SPACE                    TO �������̂v
027490     NOT INVALID KEY
027500         MOVE ���|��������             TO �������̂v
027510     END-READ.
006490     STRING ���Z�|���ʖ��̂P(5)  DELIMITED BY SPACE
009980            �������̂v           DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(5)  DELIMITED BY SPACE
006520       INTO ���|�������T
006570     END-STRING.
      */���񏈒u��
           IF ���Z�|���񏈒u��(5) NOT = ZERO
              MOVE 1                         TO ���|���񏈒u�񐔂T
           END-IF.
012310*/�����N����
012320     MOVE ���|�����a��N����(5)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����T.
012700*/������
012310*/�{�p�J�n���F�ŏ��̒ʉ@��
012720     MOVE ���|�J�n�a��N����(5)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����T
                                                ���|�{�p�J�n���T.
012350*/�{�p�I�����F�ŏI�ʉ@���H
012720     MOVE ���|�I���a��N����(5)        TO �a��N�����v.
012370     PERFORM ���t�ϊ��W��.
012380     MOVE ���t�W���v                   TO ���|�{�p�I�����T.
012390*/������
012400     MOVE ���Z�|���ʎ�����(5)          TO ���|�������T.
012390*/�]�A�敪
           EVALUATE ���|�]�A�敪(5)
           WHEN 3
012400         MOVE 2                        TO ���|�]�A�敪�T
           WHEN 4
012400         MOVE 3                        TO ���|�]�A�敪�T
           WHEN 9
012400         MOVE ZERO                     TO ���|�]�A�敪�T
           WHEN OTHER
012400         MOVE 1                        TO ���|�]�A�敪�T
           END-EVALUATE.
012390*/��É�
012400     COMPUTE ���|��É񐔂T = ���Z�|��É񐔂T�O + ���Z�|��É񐔂T�W +
                                    ���Z�|��É񐔂T�T + ���Z�|��É񐔂T�Q.
012390*/��㪖@��
012400     COMPUTE ���|��㪖@�񐔂T = ���Z�|��㪖@�񐔂T�O + ���Z�|��㪖@�񐔂T�W +
                                      ���Z�|��㪖@�񐔂T�T + ���Z�|��É񐔂T�Q.
012390*/��㪖@��
012400     COMPUTE ���|��㪖@�񐔂T = ���Z�|��㪖@�񐔂T�O + ���Z�|��㪖@�񐔂T�W +
                                      ���Z�|��㪖@�񐔂T�T + ���Z�|��É񐔂T�Q.
012390*/�d�É�
012400     COMPUTE ���|�d�É񐔂T = ���Z�|�d�É񐔂T�O + ���Z�|�d�É񐔂T�W +
                                    ���Z�|�d�É񐔂T�T + ���Z�|��É񐔂T�Q.
012390*/�����ʒ����敪
           IF (���Z�|���v�T�Q NOT = ZERO) OR (���Z�|���v�T�T NOT = ZERO) OR (���Z�|���v�T�W NOT = ZERO)
012400        MOVE 1                         TO ���|�����ʒ����敪�T
           END-IF.
012390*/���������敪
           IF (���Z�|�����������T�Q NOT = ZERO) OR (���Z�|�����������T�T NOT = ZERO) OR
              (���Z�|�����������T�W NOT = ZERO) OR (���Z�|�����������T�O NOT = ZERO)
012400         MOVE 1                        TO ���|���������敪�T
           END-IF.
012490*/������ʂP�F���܁A�Q�F�s�S���܁A�R�F�E�P�A�S�F�Ŗo�A�T�F�P���A�U�F�����@�V�F�S�k��Â���
012500*/�_�ł͂P�F�P���A�Q�F�Ŗo�A�R�F�����A    �S�F�E�P�A
012510*/      �T�F���܁A�U�F�s���A�V�F���܍S�k�A�W�F�s�S�S�k�A
012520     MOVE ���|�������(6)              TO ������ʂv.
012530     PERFORM ���ʃ^�C�v�擾.
012540     MOVE ���ʃ^�C�v�v                 TO ���|�����敪�U.
      */������
027440     MOVE 03                           TO ���|�敪�R�[�h.
027450     MOVE ���|�������(6)              TO ���|���̃R�[�h.
027460     READ ���̃}�X�^
027470     INVALID KEY
027480         MOVE SPACE                    TO �������̂v
027490     NOT INVALID KEY
027500         MOVE ���|��������             TO �������̂v
027510     END-READ.
006490     STRING ���Z�|���ʖ��̂P(6)  DELIMITED BY SPACE
009980            �������̂v           DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(6)  DELIMITED BY SPACE
006520       INTO ���|�������U
006570     END-STRING.
      */���񏈒u��
           IF ���Z�|���񏈒u��(6) NOT = ZERO
              MOVE 1                         TO ���|���񏈒u�񐔂U
           END-IF.
012310*/�����N����
012320     MOVE ���|�����a��N����(6)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����U.
      */������
012310*/�{�p�J�n���F�ŏ��̒ʉ@��
012720     MOVE ���|�J�n�a��N����(6)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����U
                                                ���|�{�p�J�n���U.
012350*/�{�p�I�����F�ŏI�ʉ@���H
012720     MOVE ���|�I���a��N����(6)        TO �a��N�����v.
012370     PERFORM ���t�ϊ��W��.
012380     MOVE ���t�W���v                   TO ���|�{�p�I�����U.
012390*/������
012400     MOVE ���Z�|���ʎ�����(6)          TO ���|�������U.
012390*/�]�A�敪
           EVALUATE ���|�]�A�敪(6)
           WHEN 3
012400         MOVE 2                        TO ���|�]�A�敪�U
           WHEN 4
012400         MOVE 3                        TO ���|�]�A�敪�U
           WHEN 9
012400         MOVE ZERO                     TO ���|�]�A�敪�U
           WHEN OTHER
012400         MOVE 1                        TO ���|�]�A�敪�U
           END-EVALUATE.
012390*/��É�
012400*     MOVE ���Z�|��É񐔂U             TO ���|��É񐔂U.
012390*/��㪖@��
012400*     MOVE ���Z�|��㪖@�񐔂U           TO ���|��㪖@�񐔂U.
012390*/��㪖@��
012400*     MOVE ���Z�|��㪖@�񐔂U           TO ���|��㪖@�񐔂U.
012390*/�d�É�
012400*     MOVE ���Z�|�d�É񐔂U             TO ���|�d�É񐔂U.
012390*/�����ʒ����敪
012400*     MOVE ZERO                         TO ���|�����ʒ����敪�U.
012390*/���������敪
      *     IF ���|�����������U NOT = ZERO
012400*         MOVE 1                        TO ���|���������敪�U
      *     END-IF.
012490*/������ʂP�F���܁A�Q�F�s�S���܁A�R�F�E�P�A�S�F�Ŗo�A�T�F�P���A�U�F�����@�V�F�S�k��Â���
012500*/�_�ł͂P�F�P���A�Q�F�Ŗo�A�R�F�����A    �S�F�E�P�A
012510*/      �T�F���܁A�U�F�s���A�V�F���܍S�k�A�W�F�s�S�S�k�A
012520     MOVE ���|�������(7)              TO ������ʂv.
012530     PERFORM ���ʃ^�C�v�擾.
012540     MOVE ���ʃ^�C�v�v                 TO ���|�����敪�V.
      */������
027440     MOVE 03                           TO ���|�敪�R�[�h.
027450     MOVE ���|�������(7)              TO ���|���̃R�[�h.
027460     READ ���̃}�X�^
027470     INVALID KEY
027480         MOVE SPACE                    TO �������̂v
027490     NOT INVALID KEY
027500         MOVE ���|��������             TO �������̂v
027510     END-READ.
006490     STRING ���Z�|���ʖ��̂P(7)  DELIMITED BY SPACE
009980            �������̂v           DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(7)  DELIMITED BY SPACE
006520       INTO ���|�������V
006570     END-STRING.
      */���񏈒u��
           IF ���Z�|���񏈒u��(7) NOT = ZERO
              MOVE 1                         TO ���|���񏈒u�񐔂V
           END-IF.
012310*/�����N����
012320     MOVE ���|�����a��N����(7)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����V.
012700*/������
012310*/�{�p�J�n���F�ŏ��̒ʉ@��
012720     MOVE ���|�J�n�a��N����(7)        TO �a��N�����v.
012330     PERFORM ���t�ϊ��W��.
012340     MOVE ���t�W���v                   TO ���|�����N�����V
                                                ���|�{�p�J�n���V.
012350*/�{�p�I�����F�ŏI�ʉ@���H
012720     MOVE ���|�I���a��N����(7)        TO �a��N�����v.
012370     PERFORM ���t�ϊ��W��.
012380     MOVE ���t�W���v                   TO ���|�{�p�I�����V.
012390*/������
012400     MOVE ���Z�|���ʎ�����(7)          TO ���|�������V.
012390*/�]�A�敪
           EVALUATE ���|�]�A�敪(7)
           WHEN 3
012400         MOVE 2                        TO ���|�]�A�敪�V
           WHEN 4
012400         MOVE 3                        TO ���|�]�A�敪�V
           WHEN 9
012400         MOVE ZERO                     TO ���|�]�A�敪�V
           WHEN OTHER
012400         MOVE 1                        TO ���|�]�A�敪�V
           END-EVALUATE.
012390*/��É�
012400*     MOVE ���Z�|��É񐔂V             TO ���|��É񐔂V.
012390*/��㪖@��
012400*     MOVE ���Z�|��㪖@�񐔂V           TO ���|��㪖@�񐔂V.
012390*/��㪖@��
012400*     MOVE ���Z�|��㪖@�񐔂V           TO ���|��㪖@�񐔂V.
012390*/�d�É�
012400*     MOVE ���Z�|�d�É񐔂V             TO ���|�d�É񐔂V.
012390*/�����ʒ����敪
012400*     MOVE ZERO                         TO ���|�����ʒ����敪�V.
012390*/���������敪
      *     IF ���|�����������V NOT = ZERO
012400*         MOVE 1                        TO ���|���������敪�V
      *     END-IF.
      */�O������S����
           IF ��|������ NOT = 5
              IF (��|���ʋ敪 NOT = ZERO) AND (��|���ʋ敪 < 7)
                 MOVE ���Z�|���S����          TO ���|�O�����S����
              ELSE
                 MOVE ZERO                    TO ���|�O�����S����
              END-IF
           ELSE
              MOVE ZERO                       TO ���|�O�����S����
           END-IF.
011480*/��o
011490     EVALUATE ���Z�|�����敪
           WHEN 1
               MOVE 2                         TO ���|��o
           WHEN 2
               MOVE 1                         TO ���|��o
           WHEN OTHER
               MOVE ZERO                      TO ���|��o
           END-EVALUATE.
011480*/���҃R�[�h
011490     MOVE ��|���Ҕԍ�                  TO ���|���Ҕԍ�.
011500*     MOVE ��|�}��                   TO ���|�}��.
011510     PERFORM �}�ԕϊ�.
      */��ԍ�
           MOVE �{��|�ڍ��t�����ԍ�(1:6)   TO ���|��ԍ�.
      */�ی����
011510     PERFORM �ی���ʕϊ�.
014400*================================================================*
014410 �}�ԕϊ� SECTION.
014420*
014430     EVALUATE ��|�}��
014440     WHEN SPACE
014450         MOVE 1  TO ���|�}��
014460     WHEN "A"
014470         MOVE 2  TO ���|�}��
014480     WHEN "B"
014490         MOVE 3  TO ���|�}��
014500     WHEN "C"
014510         MOVE 4  TO ���|�}��
014520     WHEN "D"
014530         MOVE 5  TO ���|�}��
014540     WHEN "E"
014550         MOVE 6  TO ���|�}��
014560     WHEN "F"
014570         MOVE 7  TO ���|�}��
014580     WHEN "G"
014590         MOVE 8  TO ���|�}��
014600     WHEN "H"
014610         MOVE 9  TO ���|�}��
014620*     WHEN "I"
014630*         MOVE 10 TO ���|�}��
014640*     WHEN "J"
014650*         MOVE 11 TO ���|�}��
014660*     WHEN "K"
014670*         MOVE 12 TO ���|�}��
014680*     WHEN "L"
014690*         MOVE 13 TO ���|�}��
014700*     WHEN "M"
014710*         MOVE 14 TO ���|�}��
014720*     WHEN "N"
014730*         MOVE 15 TO ���|�}��
014740*     WHEN "O"
014750*         MOVE 16 TO ���|�}��
014760*     WHEN "P"
014770*         MOVE 17 TO ���|�}��
014780*     WHEN "Q"
014790*         MOVE 18 TO ���|�}��
014800*     WHEN "R"
014810*         MOVE 19 TO ���|�}��
014820*     WHEN "S"
014830*         MOVE 20 TO ���|�}��
014840*     WHEN "T"
014850*         MOVE 21 TO ���|�}��
014860*     WHEN "U"
014870*         MOVE 22 TO ���|�}��
014880*     WHEN "V"
014890*         MOVE 23 TO ���|�}��
014900*     WHEN "W"
014910*         MOVE 24 TO ���|�}��
014920*     WHEN "X"
014930*         MOVE 25 TO ���|�}��
014940*     WHEN "Y"
014950*         MOVE 26 TO ���|�}��
014960*     WHEN "Z"
014970*         MOVE 27 TO ���|�}��
014980     WHEN OTHER
014990         MOVE 1  TO ���|�}��
015000     END-EVALUATE.
015010*================================================================*
015020 ���ʃ^�C�v�擾 SECTION.
015030*
015040     EVALUATE ������ʂv
015050     WHEN 01
015060         MOVE 5    TO ���ʃ^�C�v�v
015070     WHEN 02
015080         MOVE 4    TO ���ʃ^�C�v�v
015090     WHEN 03
015100         MOVE 6    TO ���ʃ^�C�v�v
015110     WHEN 04
015120         MOVE 3    TO ���ʃ^�C�v�v
015130     WHEN 05
015150         MOVE 1    TO ���ʃ^�C�v�v
015160     WHEN 06
015180         MOVE 2    TO ���ʃ^�C�v�v
015140     WHEN 07
015140     WHEN 08
015200         MOVE 7    TO ���ʃ^�C�v�v
015160     WHEN 09
015180         MOVE ZERO TO ���ʃ^�C�v�v
015210     END-EVALUATE.
014400*================================================================*
014410 �ی���ʕϊ� SECTION.
014420*
014430     EVALUATE ��P�|�ی����
014440     WHEN 1
               IF ��|�ی��Ҕԍ�(1:3) = "3"
014450            MOVE "1"    TO ���|�ی����
               ELSE
014450            MOVE "0"    TO ���|�ی����
               END-IF
014440     WHEN 2
014450         MOVE "3"    TO ���|�ی����
014440     WHEN 3
014450         MOVE "6"    TO ���|�ی����
014440     WHEN 4
014450         MOVE "7"    TO ���|�ی����
014440     WHEN 5
               IF ��|�ی��Ҕԍ�(1:2) = "27"
014450            MOVE "B" TO ���|�ی����
               ELSE
014450            MOVE "L" TO ���|�ی����
               END-IF
014440     WHEN 6
014450         MOVE "4"    TO ���|�ی����
014440     WHEN 7
014450         MOVE "5"    TO ���|�ی����
014440     WHEN 8
014450         MOVE "2"    TO ���|�ی����
014440     WHEN 9
014450         MOVE "8"    TO ���|�ی����
014440     WHEN 51
014450         MOVE "C"    TO ���|�ی����
014440     WHEN 52
014450         MOVE "E"    TO ���|�ی����
014440     WHEN 53
014450         MOVE "D"    TO ���|�ی����
014440     WHEN 54
014450         MOVE "H"    TO ���|�ی����
014440     WHEN 55
014450         MOVE "F"    TO ���|�ی����
014440     WHEN 60
014450         MOVE "H"    TO ���|�ی����
014440     WHEN 70
014450         MOVE "9"    TO ���|�ی����
014440     WHEN 80
014450         MOVE "I"    TO ���|�ی����
014440     WHEN 85
014440     WHEN 50
014450         MOVE "A"    TO ���|�ی����
           END-EVALUATE.
015220*================================================================*
015230 ���t�ϊ��W�� SECTION.
015240******************************************************************
015250* �a��N�����v��"yyyymmdd"�`���ɕϊ�����Z�N�V����
015260******************************************************************
015270     PERFORM ����N�擾.
015280     STRING ����N�v DELIMITED BY SIZE
015300            ���v     DELIMITED BY SIZE
015320            ���v     DELIMITED BY SIZE
015330       INTO ���t�W���v
015340     END-STRING.
015350*================================================================*
015360 �{�p���ݒ� SECTION.
015370******************************************************************
015380* �������̉񐔂��J�E���g����
015390******************************************************************
015400     MOVE ��|���Ҕԍ�  TO �{�L�|���Ҕԍ�
015410     MOVE ��|�}��      TO �{�L�|�}��
015420     MOVE ��|�{�p�a��  TO �{�L�|�{�p�a��
015430     MOVE ��|�{�p�N    TO �{�L�|�{�p�N
015440     MOVE ��|�{�p��    TO �{�L�|�{�p��
015450     MOVE 1             TO �{�L�|�{�p��
015460     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
015470                                  �{�L�|�{�p�a��N����
015480     END-START
015490     IF ��ԃL�[ = "00"
015500         MOVE SPACE TO �I���t���O�Q
015510         MOVE ZERO  TO �񐔃J�E���^�v
015540         PERFORM �{�p�L�^�e�Ǎ�
015550         PERFORM UNTIL ( �I���t���O�Q     NOT = SPACE          ) OR
015560                       ( �{�L�|���҃R�[�h NOT = ��|���҃R�[�h ) OR
015570                       ( �{�L�|�{�p�a��   NOT = ��|�{�p�a��   ) OR
015580                       ( �{�L�|�{�p�N     NOT = ��|�{�p�N     ) OR
015590                       ( �{�L�|�{�p��     NOT = ��|�{�p��     )
015610             IF �{�L�|�f�Ë敪 = 2
015620                 COMPUTE �����񐔂v = �����񐔂v + 1
015630             END-IF
                   EVALUATE �{�L�|�������Z
                   WHEN 1
015620                 COMPUTE ���ԊO�񐔂v = ���ԊO�񐔂v + 1
                   WHEN 2
015620                 COMPUTE �x���񐔂v   = �x���񐔂v + 1
                   WHEN 3
015620                 COMPUTE �[��񐔂v   = �[��񐔂v + 1
                   END-EVALUATE
015610             IF �{�L�|�Č������� = 1
015620                 COMPUTE �Č��񐔂v = �Č��񐔂v + 1
015630             END-IF
015610             IF �{�L�|���Ë��� NOT = ZERO
015620                 COMPUTE ���É񐔂v = ���É񐔂v + 1
015630             END-IF
                   EVALUATE �{�L�|���É��Z
                   WHEN 1
015620                 COMPUTE ��ԉ񐔂v       = ��ԉ񐔂v + 1
                   WHEN 2
015620                 COMPUTE ��H�񐔂v       = ��H�񐔂v + 1
                   WHEN 3
015620                 COMPUTE �\���J��񐔂v   = �\���J��񐔂v + 1
                   END-EVALUATE
015670             PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
015680                     UNTIL   ���ʂb�m�s > ���|���ʐ�
                       EVALUATE �{�L�|�������q�敪(���ʂb�m�s)
                       WHEN 1
015620                     COMPUTE ��񐔂v   = ��񐔂v + 1
                       WHEN 2
015620                     COMPUTE ���񐔂v   = ���񐔂v + 1
                       WHEN 3
015620                     COMPUTE ���񐔂v   = ���񐔂v + 1
                       END-EVALUATE
015610                 IF �{�L�|���񋟋敪(���ʂb�m�s) = 1
015620                     COMPUTE ���񋟉񐔂v = ���񋟉񐔂v + 1
015630                 END-IF
015780             END-PERFORM
015820             PERFORM �{�p�L�^�e�Ǎ�
015830         END-PERFORM
015840     END-IF.
015850*================================================================*
015860 �����f�[�^�e�Z�b�g SECTION.
      *
015940     MOVE SPACE          TO ���|���R�[�h.
015900     MOVE ��|�{�p�a��   TO ���|�{�p�a��.
015910     MOVE ��|�{�p�N     TO ���|�{�p�N.
015920     MOVE ��|�{�p��     TO ���|�{�p��.
015930     MOVE ��|���Ҕԍ�   TO ���|���Ҕԍ�.
015940     MOVE ��|�}��       TO ���|�}��.
015950     READ �����f�[�^�e
015960     INVALID KEY
015940         MOVE SPACE      TO ���|���R�[�h
016140     END-READ.
015850*================================================================*
015860 ���Z�v�g�e�Z�b�g SECTION.
      *
015940     MOVE SPACE          TO ���Z�|���R�[�h.
015900     MOVE ��|�{�p�a��   TO ���Z�|�{�p�a��.
015910     MOVE ��|�{�p�N     TO ���Z�|�{�p�N.
015920     MOVE ��|�{�p��     TO ���Z�|�{�p��.
015930     MOVE ��|���Ҕԍ�   TO ���Z�|���Ҕԍ�.
015940     MOVE ��|�}��       TO ���Z�|�}��.
           EVALUATE TRUE
           WHEN (��P�|�ی���� < 10) AND (��P�|�ی���� NOT = 5)
015940         MOVE 1          TO ���Z�|���Z���
           WHEN (��P�|�ی���� = 5)
015940         MOVE 2          TO ���Z�|���Z���
           WHEN (��P�|�ی���� > 50) AND (��P�|�ی���� <= 60)
015940         MOVE 3          TO ���Z�|���Z���
           WHEN (��P�|�ی���� = 70)
015940         MOVE 4          TO ���Z�|���Z���
           WHEN (��P�|�ی���� = 80)
015940         MOVE 5          TO ���Z�|���Z���
           WHEN (��P�|�ی���� = 50) OR (��P�|�ی���� = 85)
015940         MOVE 7          TO ���Z�|���Z���
           END-EVALUATE.
015950     READ ���Z�v�g�e
015960     INVALID KEY
015940         MOVE SPACE      TO ���Z�|���R�[�h
016140     END-READ.
016650*================================================================*
016660 �{�p�L�^�e�Ǎ� SECTION.
016670*
016680     READ �{�p�L�^�e NEXT
016690     AT END
016700         MOVE "YES" TO �I���t���O�Q
016710     END-READ.
017110*================================================================*
017120 ��ƕی��҃t�@�C���쐬 SECTION.
017130*
017140     INITIALIZE    ��ی��|���R�[�h.
017150     MOVE SPACE TO ��ی��|���R�[�h.
017160*/���ۂ̕ی���(�Q�V�V�l����)
017170     IF (��|�ی����   NOT = ZERO ) AND
017190        (��|������       = ZERO)
017191** ���R�͑ΏۊO
017192         IF  ��|�ی���� NOT = 90
017195
017200             MOVE ��|�ی����   TO �ہ|�ی����
017210             MOVE ��|�ی��Ҕԍ� TO �ہ|�ی��Ҕԍ�
017220             READ �ی��҃}�X�^
017230             INVALID KEY
017240                 INITIALIZE    ��ی��|���R�[�h
017250                 MOVE SPACE TO ��ی��|���R�[�h
017260                 MOVE ��|�ی����   TO ��ی��|�ی���ʃL�[
017270                 MOVE ��|�ی��Ҕԍ� TO ��ی��|�ی��Ҕԍ��L�[
017280                 READ ��ƕی��҃t�@�C��
017290                 INVALID KEY
017300                     PERFORM ��ƕی��҃��R�[�h�Z�b�g����
017310                     WRITE   ��ی��|���R�[�h
017320                     INVALID KEY
017330                         MOVE NC"��ی�" TO �t�@�C����
017340                         PERFORM �G���[�\��
017350                     END-WRITE
017360                 END-READ
017380             NOT INVALID KEY
017390                 INITIALIZE    ��ی��|���R�[�h
017400                 MOVE SPACE TO ��ی��|���R�[�h
017410                 MOVE �ہ|�ی����   TO ��ی��|�ی���ʃL�[
017420                 MOVE �ہ|�ی��Ҕԍ� TO ��ی��|�ی��Ҕԍ��L�[
017430                 READ ��ƕی��҃t�@�C��
017440                 INVALID KEY
017450                     PERFORM ��ƕی��҃��R�[�h�Z�b�g����
017460                     WRITE   ��ی��|���R�[�h
017470                     INVALID KEY
017480                          MOVE NC"��ی�" TO �t�@�C����
017490                          PERFORM �G���[�\��
017500                     END-WRITE
017510                 END-READ
017520             END-READ
017521         END-IF
017530     ELSE
017540         INITIALIZE    ��ی��|���R�[�h
017550         MOVE SPACE TO ��ی��|���R�[�h
017590     END-IF.
017600*�V�l
017610     IF (��|������           NOT = ZERO ) AND
017620        (��|��p���S�Ҕԍ�     NOT = SPACE)
017630         MOVE ��|������       TO �s�|������  
017640         MOVE ��|��p���S�Ҕԍ� TO �s�|�s�����ԍ�
017650         READ �s�����}�X�^
017660         INVALID KEY
017670             CONTINUE
017680         NOT INVALID KEY
017690             INITIALIZE    ��ی��|���R�[�h
017700             MOVE SPACE TO ��ی��|���R�[�h
017710             MOVE �s�|������   TO ��ی��|�ی���ʃL�[
017720             MOVE �s�|�s�����ԍ� TO ��ی��|�ی��Ҕԍ��L�[
017730             READ ��ƕی��҃t�@�C��
017740             INVALID KEY
017750                 PERFORM ��ƕی��҃��R�[�h�Z�b�g�V�l
017760                 WRITE   ��ی��|���R�[�h
017770                 INVALID KEY
017780                      MOVE NC"��ی�" TO �t�@�C����
017790                      PERFORM �G���[�\��
017800                 END-WRITE
017810             END-READ
017820         END-READ
017830     END-IF.
017840*����
017850     IF (��|�������           NOT = ZERO ) AND
017860        (��|��p���S�Ҕԍ����� NOT = SPACE)
017870          MOVE ��|�������           TO �s�|������  
017880          MOVE ��|��p���S�Ҕԍ����� TO �s�|�s�����ԍ�
017890          READ �s�����}�X�^
017900          INVALID KEY
017910              CONTINUE
017920          NOT INVALID KEY
017930              INITIALIZE    ��ی��|���R�[�h
017940              MOVE SPACE TO ��ی��|���R�[�h
017950              MOVE �s�|������   TO ��ی��|�ی���ʃL�[
017960              MOVE �s�|�s�����ԍ� TO ��ی��|�ی��Ҕԍ��L�[
017970              READ ��ƕی��҃t�@�C��
017980              INVALID KEY
017990                  PERFORM ��ƕی��҃��R�[�h�Z�b�g����
018000                  WRITE   ��ی��|���R�[�h
018010                  INVALID KEY
018020                      MOVE NC"��ی�" TO �t�@�C����
018030                      PERFORM �G���[�\��
018040                  END-WRITE
018050              END-READ
018060          END-READ
018070     END-IF.
018080*================================================================*
018090 ��ƕی��҃��R�[�h�Z�b�g���� SECTION.
018100*
           MOVE �{��|�ڍ��t�����ԍ� TO ��ی��|�ڍ��@�h�c.
018120*/�ی����
018130     EVALUATE �ہ|�ی����
018140     WHEN 01
018150         IF �ہ|�ی��Ҕԍ�(3:1) = "3"
018160*/�Ǝ�ʍ��ہF�P
018170             MOVE 1               TO ��ی��|�ی���� ��ی��|�ی���ʂQ
018180         ELSE
018190*/�s�������ہF�O
018200             MOVE ZERO            TO ��ی��|�ی���� ��ی��|�ی���ʂQ(1:1)
018210         END-IF
018220         MOVE �ہ|�ی��Ҕԍ�(1:2) TO ��ی��|���R�[�h
018230*/�ЕہF�R
018240     WHEN 02
018250         MOVE  3 TO ��ی��|�ی���� ��ی��|�ی���ʂQ
018260         PERFORM �Еی��R�[�h����
018270*/�g���F�U
018280     WHEN 03
018290         MOVE  6                  TO ��ی��|�ی���� ��ی��|�ی���ʂQ
018300         MOVE �ہ|�ی��Ҕԍ�(3:2) TO ��ی��|���R�[�h
018310*/���ρF�V
018320     WHEN 04
018330         MOVE  7                  TO ��ی��|�ی���� ��ی��|�ی���ʂQ
018340         MOVE �ہ|�ی��Ҕԍ�(3:2) TO ��ی��|���R�[�h
018350*/���فF�S
018360     WHEN 06
018370         MOVE  4                  TO ��ی��|�ی���� ��ی��|�ی���ʂQ
018380         MOVE �ہ|�ی��Ҕԍ�(3:2) TO ��ی��|���R�[�h
018390*/�D���F�T
018400     WHEN 07
018410         MOVE  5                  TO ��ی��|�ی���� ��ی��|�ی���ʂQ
018420         MOVE �ہ|�ی��Ҕԍ�(3:2) TO ��ی��|���R�[�h
018430*/�ސE�F�Q
018440     WHEN 08
018450         MOVE  2                  TO ��ی��|�ی���� ��ی��|�ی���ʂQ
018460         MOVE �ہ|�ی��Ҕԍ�(3:2) TO ��ی��|���R�[�h
018470*/���q���F�W
018480     WHEN 09
018490         MOVE  8                  TO ��ی��|�ی���� ��ی��|�ی���ʂQ
018500         MOVE �ہ|�ی��Ҕԍ�(3:2) TO ��ی��|���R�[�h
018550*/�����ی�F10
018560     WHEN 85
018570         MOVE 10                  TO ��ی��|�ی���� ��ی��|�ی���ʂQ
018580         MOVE SPACE               TO ��ی��|���R�[�h
018510*/�����ӁF18
018520     WHEN 80
018530         MOVE 18                  TO ��ی��|�ی���� ��ی��|�ی���ʂQ
018540         MOVE SPACE               TO ��ی��|���R�[�h
018550*/�J�ЁF�X
018560     WHEN 70
018570         MOVE  9                  TO ��ی��|�ی���� ��ی��|�ی���ʂQ
018580         MOVE SPACE               TO ��ی��|���R�[�h
018590     END-EVALUATE.
018600*
018610     MOVE �ہ|�ی��Ҕԍ�     TO ��ی��|�ی��Ҕԍ�.
018620     MOVE ZERO               TO ��ی��|�ی��Ҏ��ʂh�c.
018660     MOVE SPACE              TO �����於�̂v.
018670     EVALUATE �ہ|�ی����
018680** �ЕہE����
018690     WHEN 2
018700     WHEN 6
018720         MOVE �ہ|�ی��Җ���      TO �����於�̂v
018790** �g���E���ς͎x�����܂ň�
018800     WHEN 3
018930         IF �ہ|�x�������� = SPACE
018931             MOVE �ہ|�ی��Җ���      TO �����於�̂v
018936         ELSE
018937             STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
018939                    "�@"              DELIMITED BY SIZE
018940                    �ہ|�x��������    DELIMITED BY SPACE
018941                    INTO �����於�̂v
018942             END-STRING
018943         END-IF
018944     WHEN 4
019060         IF �ہ|�x�������� = SPACE
019061             MOVE �ہ|�ی��Җ���      TO �����於�̂v
019066         ELSE
019067             STRING �ہ|�ی��Җ���    DELIMITED BY SPACE
019069                    "�@"              DELIMITED BY SIZE
019070                    �ہ|�x��������    DELIMITED BY SPACE
019071                    INTO �����於�̂v
019072             END-STRING
019073         END-IF
           WHEN 70 
              MOVE ��|�{�p�a��N�� TO �J�Ё|�{�p�a��N��
              MOVE ��|���҃R�[�h   TO �J�Ё|���҃R�[�h
              READ �J�Џ��e
              NOT INVALID KEY
                 MOVE �J�Ё|�J�Ў��Ə�����  TO �����於�̂v
                 MOVE �J�Ё|�J���ی��ԍ�    TO �ہ|�ی��Ҕԍ� ��ی��|�ی��Ҕԍ� ��ی��|�ی��Ҕԍ��L�[
019140           MOVE �J�Ё|�J�Ў��Ə��X�֔ԍ�  TO �ہ|�X�֔ԍ�
019150           MOVE �J�Ё|�J�Ў��Ə��Z���P    TO �ہ|�Z���P
019160           MOVE �J�Ё|�J�Ў��Ə��Z���Q    TO �ہ|�Z���Q
019170           MOVE �J�Ё|�J�Ў��Ə��d�b�ԍ�  TO �ہ|�d�b�ԍ�
              END-READ
           WHEN 85 
              MOVE ��|�{�p�a��N�� TO ���ہ|�{�p�a��N��
              MOVE ��|���҃R�[�h   TO ���ہ|���҃R�[�h
              READ ���ۏ��e
              NOT INVALID KEY
                 MOVE ���ہ|���ێs������        TO �����於�̂v
                 MOVE ���ہ|���S�Ҕԍ�          TO �ہ|�ی��Ҕԍ� ��ی��|�ی��Ҕԍ� ��ی��|�ی��Ҕԍ��L�[
019140           MOVE ���ہ|���ۑ��t��X�֔ԍ�  TO �ہ|�X�֔ԍ�
019150           MOVE ���ہ|���ۑ��t��Z���P    TO �ہ|�Z���P
019160           MOVE ���ہ|���ۑ��t��Z���Q    TO �ہ|�Z���Q
019170           MOVE ���ہ|���ۓd�b�ԍ�        TO �ہ|�d�b�ԍ�
              END-READ
019074     WHEN OTHER
019075         MOVE �ہ|�ی��Җ���      TO �����於�̂v
019080     END-EVALUATE.
019090     MOVE �����於�̂v            TO ��ی��|�ی��Җ���
019100                                     ��ی��|�X���於��
019110                                     ��ی��|�󎚗p����.
019120*
019130*     MOVE �ہ|�s���{���i�h�r TO ��ی��|���R�[�h.
019140     MOVE �ہ|�X�֔ԍ�       TO ��ی��|�X�֔ԍ�.
019150     MOVE �ہ|�Z���P         TO ��ی��|�Z���P.
019160     MOVE �ہ|�Z���Q         TO ��ی��|�Z���Q.
019170     MOVE �ہ|�d�b�ԍ�       TO ��ی��|�s�d�k.
019180*
019190     MOVE ZERO               TO ��ی��|�{�l���S�敪.
019200     MOVE ZERO               TO ��ی��|�Ƒ����S�敪.
019210     MOVE SPACE              TO ��ی��|�����\���.
019220     MOVE SPACE              TO ��ی��|��������.
019230     MOVE ZERO               TO ��ی��|���ϋ敪.
019240*
           MOVE "YES" TO �G���[�t���O.
010460     MOVE �ہ|�ی����   TO �ۓ��|�ی����.
010470     MOVE �ہ|�ی��Ҕԍ� TO �ۓ��|�ی��Ҕԍ�.
010460     MOVE 99999          TO �ۓ��|�J�n�a��N��.
007550     START �ی��ғ��ʕ��S�}�X�^   KEY IS < �ۓ��|�ی����
                                                 �ۓ��|�ی��Ҕԍ�
                                                 �ۓ��|�J�n�a��N��
                                                 REVERSED
           END-START.
006220     IF ��ԃL�[ = "00"
010480        READ �ی��ғ��ʕ��S�}�X�^ NEXT
010510        NOT AT END
                 IF (�ہ|�ی����   = �ۓ��|�ی����) AND
                    (�ہ|�ی��Ҕԍ� = �ۓ��|�ی��Ҕԍ�)
010520              IF ( �ۓ��|���S���敪 NOT = ZERO )
019540                 COMPUTE ��ی��|�{�l���S�� = �ۓ��|�{�l���S�� / 10
019550                 COMPUTE ��ی��|�Ƒ����S�� = �ۓ��|�Ƒ����S�� / 10
                       MOVE SPACE TO �G���[�t���O
                    END-IF
                 END-IF
              END-READ
           END-IF.
           IF �G���[�t���O NOT = SPACE
019390        MOVE �ہ|�ی���� TO �����|�ی����
019400        MOVE �����a��v�q TO �����|�J�n�a��
019410        MOVE �����N�v�q   TO �����|�J�n�N
019420        MOVE �������v�q   TO �����|�J�n��
019430        START ���S���}�X�^ KEY IS <= �����|�ی����
019440                                     �����|�J�n�a��N��
019450                                     REVERSED
019460        END-START
019470        IF ��ԃL�[ = "00" 
019480            READ ���S���}�X�^ NEXT
019490            AT END
019500                CONTINUE
019510            NOT AT END
019520                IF ( �����a��N���v�q >= �����|�J�n�a��N�� ) AND
019530                   ( �����a��N���v�q <= �����|�I���a��N�� )
019540                    COMPUTE ��ی��|�{�l���S�� = �����|�{�l���S�� / 10
019550                    COMPUTE ��ی��|�Ƒ����S�� = �����|�Ƒ����S�� / 10
019560                END-IF
019570            END-READ
019580        END-IF
010830     END-IF.
022190     MOVE "2001/01/01"        TO ��ی��|�X�V���t.
019600*
019830*================================================================*
019840 ��ƕی��҃��R�[�h�Z�b�g�V�l SECTION.
019850*
019860     MOVE �s�|������        TO ��ی��|�ی���ʃL�[  .
019870     MOVE �s�|�s�����ԍ�      TO ��ی��|�ی��Ҕԍ��L�[.
           MOVE �{��|�ڍ��t�����ԍ� TO ��ی��|�ڍ��@�h�c.
019890     IF �s�|������ = 05
019900         MOVE 21              TO ��ی��|�ی���� ��ی��|�ی���ʂQ
019910     END-IF.
019920     MOVE �s�|�s�����ԍ�      TO ��ی��|�ی��Ҕԍ�.
019930     MOVE �s�|�s�����ԍ�(3:2) TO ��ی��|���R�[�h.
019940     MOVE ZERO                TO ��ی��|�ی��Ҏ��ʂh�c
019950     MOVE �s�|�s��������      TO ��ی��|�ی��Җ���
019960                                 ��ی��|�X���於��
019970                                 ��ی��|�󎚗p����.
019990     MOVE �s�|�X�֔ԍ�        TO ��ی��|�X�֔ԍ�.
020000     MOVE �s�|�Z���P          TO ��ی��|�Z���P.
020010     MOVE �s�|�Z���Q          TO ��ی��|�Z���Q.
020020     MOVE �s�|�d�b�ԍ�        TO ��ی��|�s�d�k.
020030     MOVE 1                   TO ��ی��|�{�l���S�敪.
020040*
           MOVE "YES" TO �G���[�t���O.
010460     MOVE �s�|������   TO �ۓ��|�ی����.
010470     MOVE �s�|�ی��Ҕԍ� TO �ۓ��|�ی��Ҕԍ�.
010460     MOVE 99999          TO �ۓ��|�J�n�a��N��.
007550     START �ی��ғ��ʕ��S�}�X�^   KEY IS < �ۓ��|�ی����
                                                 �ۓ��|�ی��Ҕԍ�
                                                 �ۓ��|�J�n�a��N��
                                                 REVERSED
           END-START.
006220     IF ��ԃL�[ = "00"
010480        READ �ی��ғ��ʕ��S�}�X�^ NEXT
010510        NOT AT END
                 IF (�s�|������   = �ۓ��|�ی����) AND
                    (�s�|�ی��Ҕԍ� = �ۓ��|�ی��Ҕԍ�)
010520              IF ( �ۓ��|���S���敪 NOT = ZERO ) OR
010530                 ( ��|�b���敪   NOT = ZERO )
010540                 IF ��|�b���敪 NOT = 2
010550                     MOVE �ۓ��|�{�l���S��   TO ��ی��|�{�l���S��
010560                     MOVE �ۓ��|�Ƒ����S��   TO ��ی��|�Ƒ����S��
010570                 ELSE
010580                     MOVE �ۓ��|�{�l���S���� TO ��ی��|�{�l���S��
010590                     MOVE �ۓ��|�Ƒ����S���� TO ��ی��|�Ƒ����S��
010600                 END-IF
                       MOVE SPACE TO �G���[�t���O
                    END-IF
                 END-IF
              END-READ
           END-IF.
           IF �G���[�t���O NOT = SPACE
019390        MOVE �s�|������ TO �����|�ی����
019400        MOVE �����a��v�q TO �����|�J�n�a��
019410        MOVE �����N�v�q   TO �����|�J�n�N
019420        MOVE �������v�q   TO �����|�J�n��
019430        START ���S���}�X�^ KEY IS <= �����|�ی����
019440                                     �����|�J�n�a��N��
019450                                     REVERSED
019460        END-START
019470        IF ��ԃL�[ = "00" 
019480            READ ���S���}�X�^ NEXT
019490            AT END
019500                CONTINUE
019510            NOT AT END
019520                IF ( �����a��N���v�q >= �����|�J�n�a��N�� ) AND
019530                   ( �����a��N���v�q <= �����|�I���a��N�� )
019540                    COMPUTE ��ی��|�{�l���S�� = �����|�{�l���S�� / 10
019550                    COMPUTE ��ی��|�Ƒ����S�� = �����|�Ƒ����S�� / 10
019560                END-IF
019570            END-READ
019580        END-IF
010830     END-IF.
020350*
020360     MOVE ZERO                TO ��ی��|�Ƒ����S�敪.
020370     MOVE SPACE               TO ��ی��|�����\���.
020380     MOVE SPACE               TO ��ی��|��������.
020390     MOVE ZERO                TO ��ی��|���ϋ敪.
022190     MOVE "2001/01/01"        TO ��ی��|�X�V���t.
020400*
020630*================================================================*
020640 ��ƕی��҃��R�[�h�Z�b�g���� SECTION.
020650*
020660     MOVE �s�|������        TO ��ی��|�ی���ʃL�[  .
020670     MOVE �s�|�s�����ԍ�      TO ��ی��|�ی��Ҕԍ��L�[.
020680*     MOVE ����ԍ��R�[�h�v    TO ��ی��|�ڍ��@�h�c.
           MOVE �{��|�ڍ��t�����ԍ� TO ��ی��|�ڍ��@�h�c.
020681     MOVE ZERO                TO ��ی��|�{�l���S�敪.
020690     EVALUATE �s�|������
020700*/�����ی�F10
020710     WHEN 50
020720         MOVE 10              TO ��ی��|�ی���� ��ی��|�ی���ʂQ
020730*/�S�P�V�l�F12
020740     WHEN 51
020750         MOVE 12              TO ��ی��|�ی���� ��ی��|�ی���ʂQ
020751         MOVE 2               TO ��ی��|�{�l���S�敪
020760*/��q�F16
020770     WHEN 52
020780         MOVE 16              TO ��ی��|�ی���� ��ی��|�ی���ʂQ
020790*/�g��F13
020800     WHEN 53
020810         MOVE 13              TO ��ی��|�ی���� ��ی��|�ی���ʂQ
020820*/�픚�F�Ȃ��A���̑��F17
020830     WHEN 54
020840         MOVE 17              TO ��ی��|�ی���� ��ی��|�ی���ʂQ
020850*/���c���F15
020860     WHEN 55
020870         MOVE 15              TO ��ی��|�ی���� ��ی��|�ی���ʂQ
020880*/���̑��F17
020890     WHEN 60
020900         MOVE 17              TO ��ی��|�ی���� ��ی��|�ی���ʂQ
020910     END-EVALUATE.
020920     MOVE �s�|�s�����ԍ�      TO ��ی��|�ی��Ҕԍ�.
020930     MOVE �s�|�s�����ԍ�(3:2) TO ��ی��|���R�[�h.
020940     MOVE ZERO                TO ��ی��|�ی��Ҏ��ʂh�c
020950     MOVE �s�|�s��������      TO ��ی��|�ی��Җ���
020960                                 ��ی��|�X���於��
020970                                 ��ی��|�󎚗p����.
020980*     MOVE �s�|�s���{���i�h�r TO ��ی��|���R�[�h.
020990     MOVE �s�|�X�֔ԍ�        TO ��ی��|�X�֔ԍ�.
021000     MOVE �s�|�Z���P          TO ��ی��|�Z���P.
021010     MOVE �s�|�Z���Q          TO ��ی��|�Z���Q.
021020     MOVE �s�|�d�b�ԍ�        TO ��ی��|�s�d�k.
021040*
           MOVE "YES" TO �G���[�t���O.
010460     MOVE �s�|������   TO �ۓ��|�ی����.
010470     MOVE �s�|�ی��Ҕԍ� TO �ۓ��|�ی��Ҕԍ�.
010460     MOVE 99999          TO �ۓ��|�J�n�a��N��.
007550     START �ی��ғ��ʕ��S�}�X�^   KEY IS < �ۓ��|�ی����
                                                 �ۓ��|�ی��Ҕԍ�
                                                 �ۓ��|�J�n�a��N��
                                                 REVERSED
           END-START.
006220     IF ��ԃL�[ = "00"
010480        READ �ی��ғ��ʕ��S�}�X�^ NEXT
010510        NOT AT END
                 IF (�s�|������   = �ۓ��|�ی����) AND
                    (�s�|�ی��Ҕԍ� = �ۓ��|�ی��Ҕԍ�)
010520              IF ( �ۓ��|���S���敪 NOT = ZERO ) OR
010530                 ( ��|�b���敪   NOT = ZERO )
010540                 IF ��|�b���敪 NOT = 2
010550                     MOVE �ۓ��|�{�l���S��   TO ��ی��|�{�l���S��
010560                     MOVE �ۓ��|�Ƒ����S��   TO ��ی��|�Ƒ����S��
010570                 ELSE
010580                     MOVE �ۓ��|�{�l���S���� TO ��ی��|�{�l���S��
010590                     MOVE �ۓ��|�Ƒ����S���� TO ��ی��|�Ƒ����S��
010600                 END-IF
                       MOVE SPACE TO �G���[�t���O
                    END-IF
                 END-IF
              END-READ
           END-IF.
           IF �G���[�t���O NOT = SPACE
019390        MOVE �s�|������ TO �����|�ی����
019400        MOVE �����a��v�q TO �����|�J�n�a��
019410        MOVE �����N�v�q   TO �����|�J�n�N
019420        MOVE �������v�q   TO �����|�J�n��
019430        START ���S���}�X�^ KEY IS <= �����|�ی����
019440                                     �����|�J�n�a��N��
019450                                     REVERSED
019460        END-START
019470        IF ��ԃL�[ = "00" 
019480            READ ���S���}�X�^ NEXT
019490            AT END
019500                CONTINUE
019510            NOT AT END
019520                IF ( �����a��N���v�q >= �����|�J�n�a��N�� ) AND
019530                   ( �����a��N���v�q <= �����|�I���a��N�� )
019540                    COMPUTE ��ی��|�{�l���S�� = �����|�{�l���S�� / 10
019550                    COMPUTE ��ی��|�Ƒ����S�� = �����|�Ƒ����S�� / 10
019560                END-IF
019570            END-READ
019580        END-IF
010830     END-IF.
022150*
022160     MOVE ZERO                TO ��ی��|�Ƒ����S�敪.
022170     MOVE SPACE               TO ��ی��|�����\���.
022180     MOVE SPACE               TO ��ی��|��������.
022190     MOVE ZERO                TO ��ی��|���ϋ敪.
022190     MOVE "2001/01/01"        TO ��ی��|�X�V���t.
022200*
022430*================================================================*
022440 �x������ SECTION.
022450*
022460     PERFORM VARYING �x���J�E���^ FROM 1 BY 1
022470             UNTIL �x���J�E���^ > �x���񐔂v
022480         MOVE "YES" TO �x���t���O
022490     END-PERFORM.
022500*
022510*================================================================*
022520 ������擾 SECTION.
022530*
022540     MOVE ZERO TO ���|����敪
022550     READ ������}�X�^
022560     NOT INVALID KEY
022570         MOVE ���|�x���� TO �x���񐔂v
022580     END-READ.
022841*
022842*================================================================*
022843 �ی��҃t�@�C���쐬 SECTION.
022844* ���R�[�h�ϒ��o�[�W����
022845     OPEN INPUT ��ƕی��҃t�@�C��.
022846         MOVE NC"��ی�" TO �t�@�C����.
022847         PERFORM �I�[�v���`�F�b�N.
022848*
022849     MOVE ZERO TO ��ی��|�ی����.
022850     MOVE ZERO TO ��ی��|�ی��Ҕԍ��L�[.
022851     START ��ƕی��҃t�@�C��  KEY IS >= ��ی��|�ی����
022852                                         ��ی��|�ی��Ҕԍ��L�[
022853     END-START
022854     IF ��ԃL�[ = "00"
022855         MOVE 196 TO �����J�E���^
022856         PERFORM �ی��|�w�b�_����
022857*         MOVE 327 TO �����J�E���^
022857         MOVE 334 TO �����J�E���^
022858         MOVE SPACE TO �I���t���O
022859         PERFORM ��ƕی��҃t�@�C���Ǎ�
022860         PERFORM UNTIL �I���t���O NOT = SPACE
022861* �����ӁA����͏��O
022862*             IF ��ی��|�ی���� NOT = 18
022863                 PERFORM �ی��҃��R�[�h�Z�b�g
022864                 PERFORM �ی��҃t�@�C������
022865*             END-IF
022866             PERFORM ��ƕی��҃t�@�C���Ǎ�
022867         END-PERFORM
022868     END-IF.
022869     CLOSE ��ƕی��҃t�@�C��.
022870*================================================================*
022871 ��ƕی��҃t�@�C���Ǎ� SECTION.
022872*
022880     READ ��ƕی��҃t�@�C�� NEXT
022890     AT END
022900         MOVE "YES" TO �I���t���O
022910     END-READ.
022920*================================================================*
022930 �ی��҃t�@�C������ SECTION.
022940*
023050     WRITE �ی��|���R�[�h
023060     IF ��ԃL�[  NOT =  "00"
023070         MOVE NC"�ی�"  TO �t�@�C����
023080         PERFORM �G���[�\��
023090     END-IF.
026650     PERFORM �x������.
023471*
023472*================================================================*
023473 �ی��҃��R�[�h�Z�b�g SECTION.
023474*
023475     INITIALIZE    �ی��|���R�[�h.
023476     MOVE SPACE TO �ی��|���R�[�h �ی��҃f�[�^�v.
023477
023479* �ی����
023480     IF ��ی��|�ی����(1:1) = ZERO
023481         MOVE ��ی��|�ی����(2:1) TO �ی���ʂv(1:1)
023482     ELSE
023483         MOVE ��ی��|�ی����      TO �ی���ʂv
023484     END-IF.
023486
023487* �ی��Җ��̃f�[�^���擾
023488     MOVE SPACE TO �I���t���O�Q.
023489     PERFORM VARYING �����J�E���^�Q FROM 60 BY -1
023490             UNTIL   (�����J�E���^�Q    <= ZERO ) OR
023491                     (�I���t���O�Q NOT = SPACE)
023492         IF ��ی��|�ی��Җ���(�����J�E���^�Q:1) NOT = SPACE
023493             COMPUTE �����J�E���^�Q = �����J�E���^�Q + 1
023494             MOVE "YES" TO �I���t���O�Q
023495         END-IF
023496     END-PERFORM.
023497     IF �����J�E���^�Q = ZERO
023498         MOVE " " TO �ی��Җ��̂v
023499         MOVE 1   TO �����J�E���^�Q
023500     ELSE
023501         MOVE ��ی��|�ی��Җ���(1:�����J�E���^�Q) TO �ی��Җ��̂v
023502     END-IF.
023503
023504* �X���於�̃f�[�^���擾
023505     MOVE SPACE TO �I���t���O�Q.
023506     PERFORM VARYING �����J�E���^�R FROM 60 BY -1
023507             UNTIL   (�����J�E���^�R    <= ZERO ) OR
023508                     (�I���t���O�Q NOT = SPACE)
023509         IF ��ی��|�X���於��(�����J�E���^�R:1) NOT = SPACE
023510             COMPUTE �����J�E���^�R = �����J�E���^�R + 1
023511             MOVE "YES" TO �I���t���O�Q
023512         END-IF
023513     END-PERFORM.
023514     IF �����J�E���^�R = ZERO
023515         MOVE " " TO �X���於�̂v
023516         MOVE 1   TO �����J�E���^�R
023517     ELSE
023518         MOVE ��ی��|�X���於��(1:�����J�E���^�R) TO �X���於�̂v
023519     END-IF.
023520
023521* �󎚗p���̃f�[�^���擾
023522     MOVE SPACE TO �I���t���O�Q.
023523     PERFORM VARYING �����J�E���^�S FROM 60 BY -1
023524             UNTIL   (�����J�E���^�S    <= ZERO ) OR
023525                     (�I���t���O�Q NOT = SPACE)
023526         IF ��ی��|�󎚗p����(�����J�E���^�S:1) NOT = SPACE
023527             COMPUTE �����J�E���^�S = �����J�E���^�S + 1
023528             MOVE "YES" TO �I���t���O�Q
023529         END-IF
023530     END-PERFORM.
023531     IF �����J�E���^�S = ZERO
023532         MOVE " " TO �󎚗p���̂v
023533         MOVE 1   TO �����J�E���^�S
023534     ELSE
023535         MOVE ��ی��|�󎚗p����(1:�����J�E���^�S) TO �󎚗p���̂v
023536     END-IF.
023537
023538* �Z���P�f�[�^���擾
023539     MOVE SPACE TO �I���t���O�Q.
023540     PERFORM VARYING �����J�E���^�T FROM 40 BY -1
023541             UNTIL   (�����J�E���^�T    <= ZERO ) OR
023542                     (�I���t���O�Q NOT = SPACE)
023543         IF ��ی��|�Z���P(�����J�E���^�T:1) NOT = SPACE
023544             COMPUTE �����J�E���^�T = �����J�E���^�T + 1
023545             MOVE "YES" TO �I���t���O�Q
023546         END-IF
023547     END-PERFORM.
023548     IF �����J�E���^�T = ZERO
023549         MOVE " " TO �Z���P�v
023550         MOVE 1   TO �����J�E���^�T
023551     ELSE
023552         MOVE ��ی��|�Z���P(1:�����J�E���^�T) TO �Z���P�v
023553     END-IF.
023554
023555* �Z���Q�f�[�^���擾
023556     MOVE SPACE TO �I���t���O�Q.
023557     PERFORM VARYING �����J�E���^�U FROM 40 BY -1
023558             UNTIL   (�����J�E���^�U    <= ZERO ) OR
023559                     (�I���t���O�Q NOT = SPACE)
023560         IF ��ی��|�Z���Q(�����J�E���^�U:1) NOT = SPACE
023561             COMPUTE �����J�E���^�U = �����J�E���^�U + 1
023562             MOVE "YES" TO �I���t���O�Q
023563         END-IF
023564     END-PERFORM.
023565     IF �����J�E���^�U = ZERO
023566         MOVE " " TO �Z���Q�v
023567         MOVE 1   TO �����J�E���^�U
023568     ELSE
023569         MOVE ��ی��|�Z���Q(1:�����J�E���^�U) TO �Z���Q�v
023570     END-IF.
023571     
023572* �ی��Ҕԍ��擾
023573     IF (��ی��|�ی��Ҕԍ�(1:2) NOT = "99") AND
023574        (��ی��|�ی���ʃL�[    NOT =  70 ) AND
023575        (��ی��|�ی���ʃL�[    NOT =  80 )
023576         MOVE ��ی��|�ی��Ҕԍ�        TO �ی��Ҕԍ��v
023577     ELSE
023578         MOVE " " TO �ی��Ҕԍ��v
023579     END-IF.
023580
023581     STRING ��ی��|�ڍ��@�h�c      DELIMITED BY SPACE
023582            ","                     DELIMITED BY SIZE
023583*	          ��ی��|�ی����        DELIMITED BY SPACE
023584	          �ی���ʂv              DELIMITED BY SPACE
023585            ","                     DELIMITED BY SIZE
023586            �ی��Ҕԍ��v            DELIMITED BY SPACE
023587            ","                     DELIMITED BY SIZE
023588            ��ی��|�ی��Ҏ��ʂh�c  DELIMITED BY SPACE
023589            ","                     DELIMITED BY SIZE
023590*            ��ی��|�ی��Җ���(1:�����J�E���^�Q) 
023591            �ی��Җ��̂v(1:�����J�E���^�Q) 
023592            ","                     DELIMITED BY SIZE
023593            �X���於�̂v(1:�����J�E���^�R)
023594            ","                     DELIMITED BY SIZE
023595            �󎚗p���̂v(1:�����J�E���^�S)
023596            ","                     DELIMITED BY SIZE
023597            ��ی��|���R�[�h        DELIMITED BY SPACE
023598            ","                     DELIMITED BY SIZE
023599            ��ی��|�X�֔ԍ�        DELIMITED BY SPACE
023600            ","                     DELIMITED BY SIZE
023601            �Z���P�v(1:�����J�E���^�T)
023602            ","                     DELIMITED BY SIZE
023603            �Z���Q�v(1:�����J�E���^�U)
023604            ","                     DELIMITED BY SIZE
023605            ��ی��|�s�d�k          DELIMITED BY SPACE
023606            ","                     DELIMITED BY SIZE
023607            ��ی��|�{�l���S�敪    DELIMITED BY SPACE
023608            ","                     DELIMITED BY SIZE
023609            ��ی��|�{�l���S��      DELIMITED BY SPACE
023610            ","                     DELIMITED BY SIZE
023611            ��ی��|�Ƒ����S�敪    DELIMITED BY SPACE
023612            ","                     DELIMITED BY SIZE
023613            ��ی��|�Ƒ����S��      DELIMITED BY SPACE
023614            ","                     DELIMITED BY SIZE
023615            ��ی��|�����\���      DELIMITED BY SIZE
023616            ","                     DELIMITED BY SIZE
023617            ��ی��|��������        DELIMITED BY SIZE
023618            ","                     DELIMITED BY SIZE
023619            ��ی��|���ϋ敪        DELIMITED BY SPACE
023620            ","                     DELIMITED BY SIZE
023621            ��ی��|�X�V���t        DELIMITED BY SIZE
023622            
023623            INTO �ی��҃f�[�^�v
023624     END-STRING.
023625
023626* �P���R�[�h�f�[�^���擾
023627     MOVE SPACE TO �I���t���O�Q.
023628     PERFORM VARYING �����J�E���^ FROM 334 BY -1
023629             UNTIL   (�����J�E���^    <= ZERO ) OR
023630                     (�I���t���O�Q NOT = SPACE)
023631         IF �ی��҃f�[�^�v(�����J�E���^:1) NOT = SPACE
023632             COMPUTE �����J�E���^ = �����J�E���^ + 1
023633             MOVE "YES" TO �I���t���O�Q
023634         END-IF
023635     END-PERFORM.
023636
023637     MOVE �ی��҃f�[�^�v(1:�����J�E���^) TO �ی��|���R�[�h�f�[�^.
023638
023639*================================================================*
023640 �ی��|�w�b�_���� SECTION.
023641*
023642     INITIALIZE    �ی��|���R�[�h.
023643     MOVE SPACE TO �ی��|���R�[�h.
023644     MOVE "�����@ID,�ی����,�ی��Ҕԍ�,�ی��Ҏ��ʂh�c,�ی��Җ���,�X���於��,"    TO �ی��|���R�[�h(1:66)
023645     MOVE "�󎚗p����,���R�[�h,�X�֔ԍ�,�Z��1,�Z��2,TEL,�{�l���S�敪,�{�l���S��," TO �ی��|���R�[�h(67:69)
023646     MOVE "�Ƒ����S�敪,�Ƒ����S��,�����\���,��������,���ϋ敪,�X�V���t"         TO �ی��|���R�[�h(136:61)
023647     PERFORM �ی��҃t�@�C������.
023960*================================================================*
023970 �Еی��R�[�h���� SECTION.
023980*
023990     EVALUATE �ہ|�ی��Ҕԍ�(1:2)
024000     WHEN 01
024010         MOVE 01 TO ��ی��|���R�[�h
024020     WHEN 02
024030         MOVE 02 TO ��ی��|���R�[�h
024040     WHEN 03
024050         MOVE 03 TO ��ی��|���R�[�h
024060     WHEN 04
024070         MOVE 04 TO ��ی��|���R�[�h
024080     WHEN 05
024090         MOVE 05 TO ��ی��|���R�[�h
024100     WHEN 06
024110         MOVE 06 TO ��ی��|���R�[�h
024120     WHEN 07
024130         MOVE 07 TO ��ی��|���R�[�h
024140     WHEN 08
024150         MOVE 08 TO ��ی��|���R�[�h
024160     WHEN 09
024170         MOVE 09 TO ��ی��|���R�[�h
024180     WHEN 10
024190         MOVE 10 TO ��ی��|���R�[�h
024200     WHEN 11
024210         MOVE 11 TO ��ی��|���R�[�h
024220     WHEN 12
024230         MOVE 12 TO ��ی��|���R�[�h
024240*
024250     WHEN 21
024260         MOVE 13 TO ��ی��|���R�[�h
024270*
024280     WHEN 31
024290         MOVE 14 TO ��ی��|���R�[�h
024300     WHEN 32
024310         MOVE 15 TO ��ی��|���R�[�h
024320     WHEN 33
024330         MOVE 16 TO ��ی��|���R�[�h
024340     WHEN 34
024350         MOVE 17 TO ��ی��|���R�[�h
024360     WHEN 35
024370         MOVE 18 TO ��ی��|���R�[�h
024380     WHEN 36
024390         MOVE 19 TO ��ی��|���R�[�h
024400     WHEN 37
024410         MOVE 20 TO ��ی��|���R�[�h
024420     WHEN 38
024430         MOVE 21 TO ��ی��|���R�[�h
024440     WHEN 39
024450         MOVE 22 TO ��ی��|���R�[�h
024460*
024470     WHEN 41
024480         MOVE 23 TO ��ی��|���R�[�h
024490     WHEN 42
024500         MOVE 24 TO ��ی��|���R�[�h
024510*
024520     WHEN 51
024530         MOVE 25 TO ��ی��|���R�[�h
024540     WHEN 52
024550         MOVE 26 TO ��ی��|���R�[�h
024560     WHEN 53
024570         MOVE 27 TO ��ی��|���R�[�h
024580     WHEN 54
024590         MOVE 28 TO ��ی��|���R�[�h
024600     WHEN 55
024610         MOVE 29 TO ��ی��|���R�[�h
024620     WHEN 56
024630         MOVE 30 TO ��ی��|���R�[�h
024640     WHEN 57
024650         MOVE 31 TO ��ی��|���R�[�h
024660     WHEN 58
024670         MOVE 32 TO ��ی��|���R�[�h
024680     WHEN 59
024690         MOVE 33 TO ��ی��|���R�[�h
024700     WHEN 60
024710         MOVE 34 TO ��ی��|���R�[�h
024720     WHEN 61
024730         MOVE 35 TO ��ی��|���R�[�h
024740*
024750     WHEN 71
024760         MOVE 36 TO ��ی��|���R�[�h
024770     WHEN 72
024780         MOVE 37 TO ��ی��|���R�[�h
024790     WHEN 73
024800         MOVE 38 TO ��ی��|���R�[�h
024810     WHEN 74
024820         MOVE 39 TO ��ی��|���R�[�h
024830     WHEN 75
024840         MOVE 40 TO ��ی��|���R�[�h
024850     WHEN 76
024860         MOVE 41 TO ��ی��|���R�[�h
024870     WHEN 77
024880         MOVE 42 TO ��ی��|���R�[�h
024890     WHEN 78
024900         MOVE 43 TO ��ی��|���R�[�h
024910     WHEN 79
024920         MOVE 44 TO ��ی��|���R�[�h
024930     WHEN 80
024940         MOVE 45 TO ��ی��|���R�[�h
024950     WHEN 81
024960         MOVE 46 TO ��ی��|���R�[�h
024970     WHEN 82
024980         MOVE 47 TO ��ی��|���R�[�h
024990     END-EVALUATE.
027970*================================================================*
027980 ���ҏ��t�@�C���쐬 SECTION.
027990*
028000     OPEN INPUT ��ƃt�@�C���P.
028010         MOVE NC"��P" TO �t�@�C����.
028020         PERFORM �I�[�v���`�F�b�N.
026610*
026620     OPEN OUTPUT ��ƕی��҃t�@�C��.
026630         MOVE NC"��ی�" TO �t�@�C����.
026640         PERFORM �I�[�v���`�F�b�N.
026650     PERFORM �x������.
026660     CLOSE ��ƕی��҃t�@�C��.
026670     PERFORM �x������.
026680     OPEN I-O ��ƕی��҃t�@�C��.
026690         MOVE NC"��ی�" TO �t�@�C����.
026700         PERFORM �I�[�v���`�F�b�N.
028070*
010090     MOVE 956   TO �����J�E���^.
           MOVE 1     TO ���Z�v�g�ԍ��v.
028080*/���я����A�ی���ʁA��o�ی��Ҕԍ��A���҃R�[�h�A�{�p�N�����ɂ���B
028090     MOVE ZERO  TO ��P�|�������
028100     MOVE SPACE TO ��P�|�۔�
028100     MOVE SPACE TO ��P�|���
028100     MOVE SPACE TO ��P�|�ی��Ҕԍ�
028090     MOVE ZERO  TO ��P�|�{�l�Ƒ��敪
028110     MOVE SPACE TO ��P�|���҃R�[�h
028120     MOVE ZERO  TO ��P�|�{�p�a��N��
028130     START ��ƃt�@�C���P KEY IS >= ��P�|�������
028140                                    ��P�|�۔�
028140                                    ��P�|���
028140                                    ��P�|�ی��Ҕԍ�
028140                                    ��P�|�{�l�Ƒ��敪
028150                                    ��P�|���҃R�[�h
028160                                    ��P�|�{�p�a��N��
028170     END-START.
028180     IF ��ԃL�[ = "00"
028190         MOVE SPACE  TO �I���t���O
028200         PERFORM ��ƃt�@�C���P�Ǎ�
028210         PERFORM UNTIL ( �I���t���O = "YES" )
028220*
028230            MOVE ��P�|�{�p�a��N�� TO ��|�{�p�a��N��
028240            MOVE ��P�|���҃R�[�h   TO ��|���҃R�[�h
028250            READ ��f�ҏ��e
028260            INVALID KEY
028270                CONTINUE
028280            NOT INVALID KEY
028310                PERFORM �����f�[�^�e�Z�b�g
028310                PERFORM ���Z�v�g�e�Z�b�g
028340*
028350                PERFORM ���ʃ��R�[�h�Z�b�g
                      IF ���Z�|���Z��� = 3
028460                    PERFORM �������R�[�h�Z�b�g
028360                ELSE
028390                    PERFORM ���ۃ��R�[�h�Z�b�g
                      END-IF
028400                PERFORM ���ҏ��t�@�C������
026670                PERFORM �x������
027824                PERFORM ��ƕی��҃t�@�C���쐬
028510*
028540             END-READ
028530             PERFORM ��ƃt�@�C���P�Ǎ�
028550         END-PERFORM
028560     END-IF.
028570*
028580     CLOSE ��ƃt�@�C���P ��ƕی��҃t�@�C��.
028590*================================================================*
028600 ��ƃt�@�C���P�Ǎ� SECTION.
028610*
028620     READ ��ƃt�@�C���P NEXT
028630     AT END
028640         MOVE "YES" TO �I���t���O
028650     END-READ.
028660*================================================================*
028670******************************************************************
028680 END PROGRAM YIW101.
028690******************************************************************
