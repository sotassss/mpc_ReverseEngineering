000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YDT6421.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*         ��� ���ۋ��� ���Z�v�g����i�_+����޳�ޔŁj
000100*  (�����������S�Ă̕ی���ʗp)
000110*         MED = YDT6421P
000120*
000120*2014/12/15 �T���ʖڂ̖��ׂ��󎚂��Ȃ�
000130*----------------------------------------------------------------*
000140 DATE-WRITTEN.           2019-05-16
000150 DATE-COMPILED.          2019-05-16
      */�������̓��Z�|���ʎ�������]�L����/160816
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
      */���������ɂ��V�p��/1905
      */���׏����s���Z��K�p�Q�ɒǉ�/2022
      */2024.10  �����p���K�p�ɒǉ�/2407
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
000280     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000290                             ORGANIZATION             IS  INDEXED
000300                             ACCESS MODE              IS  DYNAMIC
000310                             RECORD KEY               IS  �ہ|�ی����
000320                                                          �ہ|�ی��Ҕԍ�
000330* �����́A�L�[���ڂ̕ی��Җ��̂�ی��҃J�i�ɂ���
000340                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000350                                                          �ہ|�ی��Җ���
000360                                                         �ہ|�ی��Ҕԍ�
000370                             FILE STATUS              IS  ��ԃL�[
000380                             LOCK        MODE         IS  AUTOMATIC.
000390     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000400                             ORGANIZATION             IS  INDEXED
000410                             ACCESS MODE              IS  DYNAMIC
000420                             RECORD KEY               IS  ���|�����敪
000430                             FILE STATUS              IS  ��ԃL�[
000440                             LOCK        MODE         IS  AUTOMATIC.
000450     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS MODE              IS  DYNAMIC
000480                             RECORD KEY               IS  ���|�敪�R�[�h
000490                                                          ���|���̃R�[�h
000500                             FILE STATUS              IS  ��ԃL�[
000510                             LOCK        MODE         IS  AUTOMATIC.
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
000580     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000590                             ORGANIZATION             IS  INDEXED
000600                             ACCESS MODE              IS  DYNAMIC
000610                             RECORD KEY               IS  ���|����敪
000620                             FILE STATUS              IS  ��ԃL�[
000630                             LOCK        MODE         IS  AUTOMATIC.
000640     SELECT  �{�p�����}�X�^ ASSIGN      TO        SEJOHOL
000650                             ORGANIZATION             IS  INDEXED
000660                             ACCESS MODE              IS  DYNAMIC
000670                             RECORD KEY               IS �{��|�{�p���ԍ�
000680                             FILE STATUS              IS  ��ԃL�[
000690                             LOCK        MODE         IS  AUTOMATIC.
001410     SELECT  ����}�X�^    ASSIGN      TO        KAIJOHOL
001420                             ORGANIZATION             IS  INDEXED
001430                             ACCESS MODE              IS  DYNAMIC
000130                             RECORD KEY               IS  ���|�_���I���敪
000131                                                          ���|����R�[�h
000132                                                          ���|�ی����
000133                                                          ���|�ύX�a��N��
000134                             ALTERNATE RECORD KEY     IS  ���|�_���I���敪
000135                                                          ���|�ڍ��t��J�i
000136                                                          ���|����R�[�h
000137                                                          ���|�ی����
000138                                                          ���|�ύX�a��N��
000151                             FILE STATUS              IS  ��ԃL�[
001520                             LOCK        MODE         IS  AUTOMATIC.
000700     SELECT  ������}�X�^    ASSIGN      TO        SEIKYUSL
000710                             ORGANIZATION           IS  INDEXED
000720                             ACCESS MODE            IS  DYNAMIC
000730                             RECORD KEY             IS ����|�ی����
000740                                                       ����|�ی��Ҕԍ�
000750                             FILE STATUS            IS  ��ԃL�[
000760                             LOCK    MODE           IS  AUTOMATIC.
000770     SELECT  �o�߃}�X�^      ASSIGN      TO        KEIKAL
000780                             ORGANIZATION             IS  INDEXED
000790                             ACCESS MODE              IS  DYNAMIC
000800                             RECORD KEY               IS  �o�|�敪�R�[�h
000810                                                          �o�|�o�߃R�[�h
000820                             FILE STATUS              IS  ��ԃL�[
000830                             LOCK        MODE         IS  AUTOMATIC.
000840     SELECT  ���������e      ASSIGN      TO        HUGEINL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS  �����|�敪�R�[�h
000880                                                          �����|���������R�[�h
000890                             FILE STATUS              IS  ��ԃL�[
000900                             LOCK        MODE         IS  AUTOMATIC.
000910     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000920                             ORGANIZATION             IS  INDEXED
000930                             ACCESS MODE              IS  DYNAMIC
000940                             RECORD KEY               IS ��|�{�p�a��N��
000950                                                          ��|���҃R�[�h
000960                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000970                                                          ��|���҃J�i
000980                                                          ��|���҃R�[�h
000990                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
001000                                                         ��|�{�p�a��N��
001010                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
001020                                                          ��|�ی����
001030                                                          ��|�ی��Ҕԍ�
001040                                                          ��|���҃R�[�h
001050                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
001060                                                          ��|������
001070                                                     ��|��p���S�Ҕԍ�
001080                                                          ��|���҃R�[�h
001090                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
001100                                                          ��|�������
001110                                                  ��|��p���S�Ҕԍ�����
001120                                                          ��|���҃R�[�h
001130                             ALTERNATE RECORD KEY  IS ��|�����a��N��
001140                                                      ��|�{�p�a��N��
001150                                                      ��|���҃R�[�h
001160                             FILE STATUS              IS  ��ԃL�[
001170                             LOCK        MODE         IS  AUTOMATIC.
000180     SELECT  ��f�ҏ��Q�e  ASSIGN      TO        JUSINJ2L
000190                             ORGANIZATION             IS INDEXED
000200                             ACCESS MODE              IS DYNAMIC
000210                             RECORD KEY               IS ��Q�|�{�p�a��N��
000220                                                         ��Q�|���҃R�[�h
000230                             ALTERNATE RECORD KEY     IS ��Q�|�����Ώۋ敪
000240                                                         ��Q�|�����a��N��
000250                                                         ��Q�|�{�p�a��N��
000260                                                         ��Q�|���҃R�[�h
000270                             ALTERNATE RECORD KEY     IS ��Q�|���������Ώۋ敪
000280                                                         ��Q�|���������a��N��
000290                                                         ��Q�|�{�p�a��N��
000300                                                         ��Q�|���҃R�[�h
000310                             FILE STATUS              IS  ��ԃL�[
000320                             LOCK        MODE         IS  AUTOMATIC.
001180     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
001190                             ORGANIZATION             IS  INDEXED
001200                             ACCESS MODE              IS  DYNAMIC
001210                             RECORD KEY           IS �{�L�|�{�p�a��N����
001220                                                     �{�L�|���҃R�[�h
001230                             ALTERNATE RECORD KEY IS �{�L�|���҃R�[�h
001240                                                     �{�L�|�{�p�a��N����
001250                             FILE STATUS              IS  ��ԃL�[
001260                             LOCK        MODE         IS  AUTOMATIC.
001270     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
001280                             ORGANIZATION             IS  INDEXED
001290                             ACCESS MODE              IS  DYNAMIC
001300                             RECORD KEY               IS ���|�{�p�a��N��
001310                                                         ���|���҃R�[�h
001320                             ALTERNATE RECORD KEY     IS ���|���҃R�[�h
001330                                                         ���|�{�p�a��N��
001340                             FILE STATUS              IS  ��ԃL�[
001350                             LOCK        MODE         IS  AUTOMATIC.
001360     SELECT  �h�c�Ǘ��}�X�^    ASSIGN      TO        IDKANRL
001370                             ORGANIZATION             IS  INDEXED
001380                             ACCESS MODE              IS  DYNAMIC
001390                             RECORD KEY               IS  �h�c�ǁ|�h�c�敪
001400                                                          �h�c�ǁ|�{�p���ԍ�
001410                                                          �h�c�ǁ|�ی����
001420                                                          �h�c�ǁ|�ی��Ҕԍ�
001430                             ALTERNATE RECORD KEY     IS  �h�c�ǁ|�{�p�h�c�ԍ�
001440                                                          �h�c�ǁ|�h�c�敪
001450                                                          �h�c�ǁ|�{�p���ԍ�
001460                                                          �h�c�ǁ|�ی����
001470                                                          �h�c�ǁ|�ی��Ҕԍ�
001480                             FILE STATUS              IS  ��ԃL�[
001490                             LOCK        MODE         IS  AUTOMATIC.
001500     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
001510                             ORGANIZATION             IS  INDEXED
001520                             ACCESS MODE              IS  DYNAMIC
001530                             RECORD KEY               IS  �s�|������
001540                                                          �s�|�s�����ԍ�
001550                             ALTERNATE RECORD KEY     IS  �s�|������
001560                                                          �s�|�s��������
001570                                                          �s�|�s�����ԍ�
001580                             FILE STATUS              IS  ��ԃL�[
001590                             LOCK        MODE         IS  AUTOMATIC.
001600     SELECT  �����t�@�C��    ASSIGN      TO        MEMOL
001610                             ORGANIZATION             IS  INDEXED
001620                             ACCESS MODE              IS  DYNAMIC
001630                             RECORD KEY               IS  �����|����敪
001640                                                          �����|���҃R�[�h
001650                                                          �����|�{�p�a��N����
001660                             ALTERNATE RECORD KEY     IS  �����|����敪
001670                                                          �����|�{�p�a��N����
001680                                                          �����|���҃R�[�h
001690                             ALTERNATE RECORD KEY     IS  �����|���҃R�[�h
001700                                                          �����|�{�p�a��N����
001710                                                          �����|����敪
001720                             FILE STATUS              IS  ��ԃL�[
001730                             LOCK        MODE         IS  AUTOMATIC.
000340     SELECT  �ϔC�ҏ��}�X�^    ASSIGN      TO ININSHAL
000350                             ORGANIZATION             IS  INDEXED
000360                             ACCESS MODE              IS  DYNAMIC
000370                             RECORD KEY               IS  �ϔC�|�ی����
000440                             FILE STATUS              IS  ��ԃL�[
000450                             LOCK        MODE         IS  AUTOMATIC.
001860* ���я��󎚗p
001870     SELECT  ��ƃt�@�C���Q  ASSIGN      TO     "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001880                             ORGANIZATION             IS  INDEXED
001890                             ACCESS                   IS  DYNAMIC
001900                             RECORD      KEY          IS  ��Q�|�{�p�a��N��
001910                                                          ��Q�|���҃R�[�h
001920                                                          ��Q�|�ی����
001930                             FILE        STATUS       IS  ��ԃL�[
001940                             LOCK        MODE         IS  AUTOMATIC.
001950     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
001960                             SYMBOLIC    DESTINATION  IS "PRT"
001970                             FORMAT                   IS  ��`�̖��o
001980                             GROUP                    IS  ���ڌQ���o
001990                             PROCESSING  MODE         IS  ������ʂo
002000                             UNIT        CONTROL      IS  �g������o
002010                             FILE        STATUS       IS  �ʒm���o.
002020******************************************************************
002030*                      DATA DIVISION                             *
002040******************************************************************
002050 DATA                    DIVISION.
002060 FILE                    SECTION.
002070*                           �m�q�k��  �R�Q�O�n
002080 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
002090     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002100*                           �m�q�k��  �P�Q�W�n
002110 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002120     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002130*                           �m�q�k��  �P�Q�W�n
002140 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
002150     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002470*                           �m�q�k��  �U�S�O�n
002480 FD  ����}�X�^        BLOCK   CONTAINS   1   RECORDS.
002490     COPY KAIJOHO         OF  XFDLIB  JOINING   ���   AS  PREFIX.
      *                          �m�q�k��  �P�T�R�U�n
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
002190*                           �m�q�k��  �Q�T�U�n
002200 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
002210     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002220     COPY SEIGYO01        OF  XFDLIB  JOINING   ���O�P   AS  PREFIX.
002230*                           �m�q�k��  �P�Q�W�n
002240 FD  �{�p�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002250     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
002260*                           �m�q�k��  �P�Q�W�n
002270 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
002280     COPY SEIKYUS         OF  XFDLIB  JOINING   ����   AS  PREFIX.
002290*                           �m�q�k��  �P�Q�W�n
002300 FD  �o�߃}�X�^          BLOCK   CONTAINS   1   RECORDS.
002310     COPY KEIKA          OF  XFDLIB  JOINING   �o   AS  PREFIX.
002320*                           �m�q�k��  �R�Q�O�n
002330 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
002340     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002560*                          �m�q�k��  1024�n
000340 FD  ��f�ҏ��Q�e        BLOCK   CONTAINS   1   RECORDS.
000350     COPY JUSINJ2          OF  XFDLIB  JOINING   ��Q   AS  PREFIX.
002350*                           �m�q�k��  �Q�T�U�n
002360 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
002370     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
002380*                           �m�q�k��  �P�Q�W�n
002390 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
002400     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002410*                           �m�q�k��  �P�Q�W�n
002420 FD  ���������e         BLOCK   CONTAINS   1   RECORDS.
002430     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
002440*                           �m�q�k��  �P�Q�W�n
002450 FD  �h�c�Ǘ��}�X�^          BLOCK   CONTAINS   1   RECORDS.
002460     COPY IDKANR    OF  XFDLIB  JOINING   �h�c��   AS  PREFIX.
002470*                           �m�q�k��  �Q�T�U�n
002480 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002490     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
002500*                           �m�q�k��  �W�R�Q�n
002510 FD  �����t�@�C��        BLOCK CONTAINS 1     RECORDS.
002520     COPY MEMO           OF    XFDLIB JOINING ���� AS PREFIX.
002560*                          �m�q�k��  1024�n
000820 FD  �ϔC�ҏ��}�X�^    BLOCK   CONTAINS   1   RECORDS.
000830     COPY ININSHA         OF  XFDLIB  JOINING   �ϔC   AS  PREFIX.
002570*
002580 FD  ��ƃt�@�C���Q RECORD  CONTAINS 32 CHARACTERS.
002590 01  ��Q�|���R�[�h.
002600     03  ��Q�|���R�[�h�L�[.
002610         05  ��Q�|�{�p�a��N��.
002620             07  ��Q�|�{�p�a��            PIC 9.
002630             07  ��Q�|�{�p�N              PIC 9(2).
002640             07  ��Q�|�{�p��              PIC 9(2).
002650         05  ��Q�|���҃R�[�h.
002660             07 ��Q�|���Ҕԍ�             PIC 9(6).
002670             07 ��Q�|�}��                 PIC X(1).
002680         05  ��Q�|�ی����                PIC 9(2).
002690     03  ��Q�|���R�[�h�f�[�^.
002700         05  ��Q�|����                    PIC 9(4).
002710         05  FILLER                        PIC X(14).
002720*
002730 FD  ����t�@�C��.
002740     COPY YDT6421P       OF  XMDLIB.
002750*----------------------------------------------------------------*
002760******************************************************************
002770*                WORKING-STORAGE SECTION                         *
002780******************************************************************
002790 WORKING-STORAGE         SECTION.
002800 01 �L�[����                           PIC X     VALUE SPACE.
002810 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
002820 01 �I���t���O                         PIC X(3)  VALUE SPACE.
002830 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
002840 01 �����t���O                         PIC X(3)  VALUE SPACE.
002850 01 �p���t���O                         PIC X(3)  VALUE SPACE.
002860 01 �t�@�C����                         PIC N(6)  VALUE SPACE.
002870 01 ���Z�v�g�o�f�v                     PIC X(8)  VALUE SPACE.
002880 01 �O�a��v                           PIC 9     VALUE ZERO.
002890 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
002900 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002910 01 ���Ҕԍ��v                         PIC 9(6)  VALUE ZERO.
002920 01 �������̂v                         PIC N(6)  VALUE SPACE.
002930 01 ���ʖ��̂v                         PIC N(12) VALUE SPACE.
002940 01 ���ʒ��v                           PIC 9(2) VALUE 1.
002950 01 �E�o�t���O                         PIC X(3)  VALUE SPACE.
002960 01 �󔒂v                             PIC X(2)  VALUE SPACE.
001363 01 �S�p��                           PIC X(2)  VALUE X"8140".
001364 01 ���p��                           PIC X(2)  VALUE X"2020".
002910 01 �������Z�v                         PIC 9(1)  VALUE ZERO.
002970*
002980** ���������{��ϊ�
002990 01 �����v                             PIC 9(2).
003000 01 �����q REDEFINES �����v.
003010    03 �����v�P                        PIC X(1).
003020    03 �����v�Q                        PIC X(1).
003030*
003040 01 �����ԍ��v                         PIC 9.
003050 01 �����ԍ��q REDEFINES �����ԍ��v.
003060    03 �����ԍ��v�P                    PIC X.
003070*
003080 01 �S�p�����ԍ��v                     PIC N.
003090 01 �S�p�����ԍ��q REDEFINES �S�p�����ԍ��v.
003100    03 �S�p�����ԍ��v�P                PIC X(2).
003110*************
003120* ���ϔԍ��p
003130 01 ���ϘA�ԍ��W�c�v.
003140    03 ���ϘA�ԍ����v                  PIC X(14)  VALUE SPACE.
003150    03 ���ϘA�ԍ����m�v REDEFINES  ���ϘA�ԍ����v  PIC N(7).
003160    03 ���ϘA�ԍ��v                    PIC X(6)  VALUE SPACE.
003170    03 ���ϘA�ԍ��P�ʂv                PIC X(2)  VALUE SPACE.
003180    03 ���ϘA�ԍ��P�ʂm�v REDEFINES  ���ϘA�ԍ��P�ʂv  PIC N.
003190* ���q���ԍ��p
003200 01 ���q���ԍ��W�c�v.
003210    03 ���q���ԍ����v                  PIC X(8)  VALUE SPACE.
003220    03 ���q���ԍ����m�v REDEFINES  ���q���ԍ����v  PIC N(4).
003230    03 ���q���ԍ��v                    PIC X(6)  VALUE SPACE.
003240    03 ���q���ԍ��P�ʂv                PIC X(2)  VALUE SPACE.
003250    03 ���q���ԍ��P�ʂm�v REDEFINES  ���q���ԍ��P�ʂv  PIC N.
003260*******
003270*
003280 01 �J�E���^                           PIC 9(3)  VALUE ZERO.
003290 01 �J�E���^�Q                         PIC 9(3)  VALUE ZERO.
003300 01 �ی����̂v                         PIC N(12) VALUE SPACE.
003310*
003320* �ޔ�p
003330 01 �I���N�����v�s.
003340    03 �I���N�v�s                      PIC 9(2)  VALUE ZERO.
003350    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
003360    03 �I�����v�s                      PIC 9(2)  VALUE ZERO.
003370* �������ޔ�p
003380 01 �����N�����v�s.
003390    03 �����a��v�s                    PIC 9     VALUE ZERO.
003400    03 �����N�v�s                      PIC 9(2)  VALUE ZERO.
003410    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003420    03 �������v�s                      PIC 9(2)  VALUE ZERO.
003430* ���������p
003440 01 ���������v�s.
003450    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
003460    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
003470    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
003480    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
003490    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
003500    03 ���������i���o�[�v�s.
003510       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
003520    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
003530 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
003540 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
003550 01 ���������s�a�k.
003560    03 ���������R�[�h�s�a�k            OCCURS 9.
003570       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
003580       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
003590       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
003600 01 �����������e�v.
003610    03 �����������e�����v              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 �����������e�����w�v.
003630       05 �����������e�P�w�v           PIC X(80)  VALUE SPACE.
003640       05 �����������e�Q�w�v           PIC X(80)  VALUE SPACE.
003640       05 �����������e�R�w�v           PIC X(80)  VALUE SPACE.
003650       05 �����������e�S�w�v           PIC X(78)  VALUE SPACE.
009270*
009280 01 �������R�v.
          03 �E�v���v�s�a�k.
             05 �E�v���v�s                   PIC X(112) OCCURS 15 VALUE SPACE.
          03 �������R�v�s�a�k.
             05 �������R�v�s                 PIC X(112) OCCURS 15 VALUE SPACE.
009280    03 �������R�����v                  PIC N(846) VALUE SPACE.
          03 �������R�����v.
             05 �������R���e�v               PIC X(112) OCCURS 30 VALUE SPACE.
003660*
003670* �������Z�����p
003680 01 �������Z�v�s.
003690    03 �������Z�J�E���g                PIC 9    VALUE ZERO.
003700    03 �ԍ��J�E���^                    PIC 9    VALUE ZERO.
003710    03 �������Z�W�c�v�s  OCCURS 3.
003720       05 �������Z�敪�v�s             PIC 9    VALUE ZERO.
003730       05 �������Z���v�s               PIC 9(2) VALUE ZERO.
003740       05 �������Z���v�s               PIC 9(2) VALUE ZERO.
003750    03 �������Z�W�c�m�v  OCCURS 3.
003760       05 ���Z��؂v                   PIC N(1) VALUE SPACE.
003770       05 ���Z���e�v                   PIC N(3) VALUE SPACE.
003780       05 �������Z���m�v�P             PIC N(1) VALUE SPACE.
003790       05 �������Z���m�v�Q             PIC N(1) VALUE SPACE.
003800       05 ���Œ�v                     PIC N(1) VALUE SPACE.
003810       05 �������Z���m�v�P             PIC N(1) VALUE SPACE.
003820       05 �������Z���m�v�Q             PIC N(1) VALUE SPACE.
003830       05 ���Œ�v                     PIC N(1) VALUE SPACE.
003840    03 �������Z�����P�v                PIC N(10) VALUE SPACE.
003850    03 �������Z�����Q�v                PIC N(10) VALUE SPACE.
003860    03 �������Z�����R�v                PIC N(10) VALUE SPACE.
003070    03 �������Z��؂v                  PIC X     VALUE SPACE.
003080    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003090    03 �������Z���v                    PIC 9(2)  VALUE ZERO.
003870*
003880** �O�������̂ݗp
003890 01 �����Č��t���O                     PIC X(3)  VALUE SPACE.
003900 01 �O���t���O                         PIC X(3)  VALUE SPACE.
003910*
003920 01 �v�Z�N�����v.
003930    03 �v�Z�a��v                      PIC 9(1)  VALUE ZERO.
003940    03 �v�Z�N�v                        PIC S9(2)  VALUE ZERO.
003950    03 �v�Z���v                        PIC S9(2)  VALUE ZERO.
003960    03 �v�Z���v                        PIC S9(2)  VALUE ZERO.
003970 01 �J�n�N�����Q�v.
003980    03 �J�n�a��Q�v                    PIC 9(1)  VALUE ZERO.
003990    03 �J�n�N�Q�v                      PIC 9(2)  VALUE ZERO.
004000    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
004010    03 �J�n���Q�v                      PIC 9(2)  VALUE ZERO.
004020    03 �J�n����N�v                    PIC S9(4) VALUE ZERO.
004030 01 �I���N�����Q�v.
004040    03 �I���a��Q�v                    PIC 9(1)  VALUE ZERO.
004050    03 �I���N�Q�v                      PIC 9(2)  VALUE ZERO.
004060    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
004070    03 �I�����Q�v                      PIC 9(2)  VALUE ZERO.
004080    03 �I������N�v                    PIC S9(4) VALUE ZERO.
004090***
004100** ���������E�������R����敪�p
004110 01 ������������敪�v                 PIC 9 VALUE ZERO.
004120 01 �������R����敪�v                 PIC 9 VALUE ZERO.
004130*
004140** ���Z���i�̓��t�敪�p (0:�ŏI�ʉ@���A1:�������A9:�󎚂Ȃ�)
004150 01 ���Z�v�g���t�敪�v                 PIC 9 VALUE ZERO.
004160 01 ���Z�v�g���ғ��t�敪�v             PIC 9 VALUE ZERO.
004170*
004180** �������p
004190 01 �{�p����N�v                       PIC 9(4)  VALUE ZERO.
004200 01 ���v                               PIC 9(3)  VALUE ZERO.
004210 01 �]�v                               PIC 9(3)  VALUE ZERO.
004220*
004230*
004240** �}�Ԕ���p
004250 01 �J�n�f�Ó��蓮�敪�v               PIC 9    VALUE ZERO.
004260*
004270* �ی��Ҕԍ�
004280 01 �ی��Ҕԍ���r�v                   PIC X(6)   VALUE SPACE.
004290*
004300*
004310** �������Z�܂Ƃߗp
004320 01 �������Z�܂Ƃ߃t���O               PIC X(3)  VALUE SPACE.
004330 01 ������ʗ��̂v                     PIC N(4)  VALUE SPACE.
004340 01 ������ʗ��̂v�Q                   PIC N(4)  VALUE SPACE.
004350*
004360** ���Z�E�v�p( N(38)�Œ�j /
004370 01 �����̌o�߂v.
004380    03 �����̌o�ߍs�v                  PIC X(76) OCCURS 2 VALUE SPACE.
004390 01 �����̌o�߂m�v REDEFINES �����̌o�߂v.
004400    03 �����̌o�ߍs�m�v                PIC N(38) OCCURS 2.
004410*
004420*
004430* ������������敪
004440 01 ���Z������������敪�v             PIC 9    VALUE ZERO.
004440 01 ���Z�������R����敪�v             PIC 9    VALUE ZERO.
004450*
004460* ���Z�v�g���я� *
004470 01 ���Ԃv                             PIC 9(4) VALUE ZERO.
004480*
004490* ���쌧�p
004500 01 �󋋎Ҕԍ��ҏW�v.
004510    03 �󋋎Ҕԍ��ҏW�v�P              PIC X(3)  VALUE SPACE.
004520    03 �󋋎ҋ�؂P                    PIC X     VALUE SPACE.
004530    03 �󋋎Ҕԍ��ҏW�v�Q              PIC X(2)  VALUE SPACE.
004540    03 �󋋎ҋ�؂Q                    PIC X     VALUE SPACE.
004550    03 �󋋎Ҕԍ��ҏW�v�R              PIC X(10) VALUE SPACE.
004560*
004570** H18/08 ���Z�v�g�̉�㪖@���̒��������邩���Ȃ����̐ݒ�B�i�O�F�������� �P�F�����Ȃ��j
004580 01 ���Z��㪖@�����v.
004590    03 ���ۃ��Z��㪒����v              PIC 9 VALUE ZERO.
004600    03 �V�l���Z��㪒����v              PIC 9 VALUE ZERO.
004610    03 �������Z��㪒����v              PIC 9 VALUE ZERO.
004620*
004630***
004640 01 ����R�[�h�v                       PIC 9(2)  VALUE ZERO.
004650* ��ϔC���p�i�_�������j
004660 01 ���̑��ҏW�v.
004670    03 ���̑��ҏW���e�v                PIC N(10) VALUE SPACE.
      * �ϔC�ҏ��}�X�^�g�p�敪(�O�F�g�p���Ȃ��A�P�F�g�p����)
       01 �ϔC�ҏ��敪�v                   PIC 9     VALUE ZERO.
004680***
004690*
004700* �����̌o�ߌŒ�󎚗p�Ɏg�p
004710 01 �S�_�e�o�c�敪�v                   PIC 9     VALUE ZERO.
004720 01 �o�ߕ��ʐ����v                     PIC N(1)  VALUE SPACE.
      *
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
       01 �������q�b�l                       PIC X(200) VALUE SPACE.
       01 �^����Âb�l                       PIC X(68)  VALUE SPACE.
004730*
      */�����p��̒ǉ�/2407
       01 �����p��v.
          03 �����p��b�l                    PIC X(280) VALUE SPACE.
          03 �����p��P�v�s                  PIC X(54)  VALUE SPACE.
          03 �����p��Q�v�s                  PIC X(54)  VALUE SPACE.
          03 �����p��R�v�s                  PIC X(54)  VALUE SPACE.
          03 �����p��S�v�s                  PIC X(54)  VALUE SPACE.
          03 �����p��T�v�s                  PIC X(54)  VALUE SPACE.
          03 �������v�q                      OCCURS 5.
             05 �������v�o                   PIC X(36)  VALUE SPACE.
          03 �����p��b�l�Q                  PIC X(280) VALUE SPACE.
       01 �����v                             PIC Z9     VALUE ZERO.
004730*
004770******************************
004780* �T����  �E�v����  �ҏW�p *
004790******************************
004800* 01 ���ʂT�v.
004810*   03 FILLER                           PIC X(1).
004820*   03 �����Œ�T�v                     PIC X(5).
004830*   03 FILLER                           PIC X(1).
004840*   03 �����J�n�����T�v.
004850*      05 �����J�n���T�v                PIC ZZ.
004860*      05 FILLER                        PIC X(2).
004870*      05 �����J�n���T�v                PIC ZZ.
004880*   03 FILLER                           PIC X(2).
004890*   03 ��ÂT�v.
004900*      05 ��ÒP���T�v                  PIC ZZZZ.
004910*      05 FILLER                        PIC X(2).
004920*      05 ��É񐔂T�v                  PIC ZZ.
004930*      05 FILLER                        PIC X(2).
004940*      05 ��×��T�v                    PIC ZZ,ZZZ.
004950*   03 FILLER                           PIC X(3).
004960*   03 ��㪖@�T�v.
004970*      05 ��㪖@�񐔂T�v                PIC ZZ.
004980*      05 FILLER                        PIC X(2).
004990*      05 ��㪖@���T�v                  PIC ZZZZ.
005000*   03 FILLER                           PIC X(3).
005010*   03 ��㪖@�T�v.
005020*      05 ��㪖@�񐔂T�v                PIC ZZ.
005030*      05 FILLER                        PIC X(2).
005040*      05 ��㪖@���T�v                  PIC ZZZZ.
005050*   03 FILLER                           PIC X(3).
005060*   03 �d�ÂT�v.
005070*      05 �d�É񐔂T�v                  PIC ZZ.
005080*      05 FILLER                        PIC X(2).
005090*      05 �d�×��T�v                    PIC ZZZZ.
005100*   03 FILLER                           PIC X(4).
005110*   03 ���v�T�v                         PIC ZZ,ZZZ.
005120*   03 FILLER                           PIC X(2).
005130*   03 �����ʗ��T�v                     PIC X(4).
005140*   03 FILLER                           PIC X(1).
005150*   03 �����ʍ����v�T�v                 PIC ZZ,ZZZ.
005160*   03 FILLER                           PIC X(3).
005170*   03 �����������T�v                   PIC 9.9.
005180*   03 FILLER                           PIC X(5).
005190*   03 ���������v�T�v                   PIC ZZ,ZZZ.
005200*   03 FILLER                           PIC X(1).
004800 01 ���ʂT�v.
004840   03 �����J�n�����T�v.
004850      05 �����J�n���T�v                PIC ZZ.
            05 ���b�l                        PIC X(2).
004870      05 �����J�n���T�v                PIC ZZ.
            05 ���b�l                        PIC X(2).
         03 ���ʂP�v                         PIC X(1).
004890   03 ��ÂT�v.
            05 ���ʂQ�v                      PIC X(1).
004900      05 ��ÒP���T�v                  PIC ZZZZ.
            05 ��Z�L���P�v                  PIC X(1).
004920      05 ��É񐔂T�v                  PIC ZZ.
            05 �C�R�[���P�v                  PIC X(1).
004940      05 ��×��T�v                    PIC ZZ,ZZZ.
         03 ���ʂR�v                         PIC X(1).
         03 ���Z�L���P�v                     PIC X(1).
         03 ���ʂS�v                         PIC X(1).
004960   03 ��㪖@�T�v.
            05 ��㪖@�P���T�v                PIC Z(2).
            05 ��Z�L���Q�v                  PIC X(1).
004970      05 ��㪖@�񐔂T�v                PIC ZZ.
            05 �C�R�[���Q�v                  PIC X(1).
004990      05 ��㪖@���T�v                  PIC ZZZZ.
         03 ���ʂT�v                         PIC X(1).
         03 ���Z�L���Q�v                     PIC X(1).
         03 ���ʂU�v                         PIC X(1).
005010   03 ��㪖@�T�v.
            05 ��㪖@�P���T�v                PIC Z(2).
            05 ��Z�L���R�v                  PIC X(1).
005020      05 ��㪖@�񐔂T�v                PIC ZZ.
            05 �C�R�[���R�v                  PIC X(1).
005040      05 ��㪖@���T�v                  PIC ZZZZ.
         03 ���ʂV�v                         PIC X(1).
         03 ���Z�L���R�v                     PIC X(1).
         03 ���ʂW�v                         PIC X(1).
005060   03 �d�ÂT�v.
            05 �d�ÒP���T�v                  PIC Z(2).
            05 ��Z�L���S�v                  PIC X(1).
005070      05 �d�É񐔂T�v                  PIC ZZ.
            05 �C�R�[���S�v                  PIC X(1).
005090      05 �d�×��T�v                    PIC ZZZZ.
            05 ���ʂX�v                      PIC X(1).
         03 ���ʂP�O�v                       PIC X(1).
         03 ��Z�L���T�v                     PIC X(1).
005130   03 �����ʗ��T�v                     PIC X(3).
         03 ��Z�L���U�v                     PIC X(1).
005170   03 �����������T�v                   PIC 9.9.
         03 �C�R�[���T�v                     PIC X(1).
005190   03 ���������v�T�v                   PIC ZZ,ZZZ.
005210*
005220****************
005230* �A�����ڑҔ� *
005240****************
005250*    ************
005260*    * ����L�[ *
005270*    ************
005280 01 �Ώۃf�[�^�v�q.
005290    03 �{�p�a��N���v�q.
005300       05 �{�p�a��v�q                  PIC 9(1)  VALUE ZERO.
005310       05 �{�p�N�v�q                    PIC 9(2)  VALUE ZERO.
005320       05 �{�p���v�q                    PIC 9(2)  VALUE ZERO.
005330    03 �ی���ʂv�q                     PIC 9(2)  VALUE ZERO.
005340    03 �ی��Ҕԍ��v�q                   PIC X(10) VALUE SPACE.
005350    03 �����ʂv�q                     PIC 9(2)  VALUE ZERO.
005360    03 ��p���S�Ҕԍ��v�q               PIC X(10) VALUE SPACE.
005370    03 ������ʂv�q                     PIC 9(2)  VALUE ZERO.
005380    03 ��p���S�Ҕԍ������v�q           PIC X(10) VALUE SPACE.
005390    03 �{�l�Ƒ��敪�v�q                 PIC 9(1)  VALUE ZERO.
005400    03 ���҃J�i�v�q                     PIC X(20) VALUE SPACE.
005410    03 ���҃R�[�h�v�q.
005420       05 ���Ҕԍ��v�q                  PIC 9(6)  VALUE ZERO.
005430       05 �}�Ԃv�q                      PIC X(1)  VALUE SPACE.
005440*    ************
005450*    * ������� *
005460*    ************
005470*    �����̗���
005480***********************
005490 01 �����P�v�q.
005500   03 �����v�q.
005510      05 ���S�����v�q               PIC 9(3)    VALUE ZERO.
005520      05 �������v�q                 PIC 9(5)    VALUE ZERO.
005530      05 �������Z���v�q             PIC 9(5)    VALUE ZERO.
005540   03 ���k���v�q                    PIC 9(4)    VALUE ZERO.
005550   03 �Č����v�q                    PIC 9(5)    VALUE ZERO.
005560   03 ���Âv�q.
005570      05 ���Ë����v�q               PIC 9(2)V9  VALUE ZERO.
005580      05 ���É񐔂v�q               PIC 9(2)    VALUE ZERO.
005590      05 ���×��v�q                 PIC 9(6)    VALUE ZERO.
005600      05 ���É��Z���v�q             PIC 9(5)    VALUE ZERO.
005610   03 �������q���Z���v�q            PIC 9(5)    VALUE ZERO.
005620   03 �{�p���񋟗��v�q            PIC 9(5)    VALUE ZERO.
005630   03 ���v�v�q                      PIC 9(6)    VALUE ZERO.
005640   03 �ꕔ���S���v�q                PIC 9(6)    VALUE ZERO.
005650   03 �������z�v�q                  PIC 9(6)    VALUE ZERO.
005660   03 ���t�����v�q                  PIC 9(1)    VALUE ZERO.
005670   03 �󋋎ҕ��S�z�v�q              PIC 9(6)    VALUE ZERO.
005680   03 �����������z�v�q              PIC 9(6)    VALUE ZERO.
005690*
005700* �������ʖ��̗���
005710***********************
005720 01 �����Q�v�q.
005730   03 ���񏈒u�v�q    OCCURS   9.
005740      05 ���񏈒u���v�q             PIC 9(5)    VALUE ZERO.
005750*
005760* �������̗���
005770***********************
005780 01 �����R�v�q.
005790**********
005800* �P���� *
005810**********
005820   03 ���ʂP�v�q.
005830      05 ��ÂP�v�q.
005840         07 ��ÒP���P�v�q              PIC 9(4)    VALUE ZERO.
005850         07 ��É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
005860         07 ��×��P�v�q                PIC 9(5)    VALUE ZERO.
005870      05 ��㪖@�P�v�q.
005880         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
005890         07 ��㪖@���P�v�q              PIC 9(4)    VALUE ZERO.
005900      05 ��㪖@�P�v�q.
005910         07 ��㪖@�񐔂P�v�q            PIC 9(2)    VALUE ZERO.
005920         07 ��㪖@���P�v�q              PIC 9(4)    VALUE ZERO.
005930      05 �d�ÂP�v�q.
005940         07 �d�É񐔂P�v�q              PIC 9(2)    VALUE ZERO.
005950         07 �d�×��P�v�q                PIC 9(4)    VALUE ZERO.
005960      05 ���v�P�v�q                     PIC 9(6)    VALUE ZERO.
005970      05 �����������P�v�q               PIC 9(3)    VALUE ZERO.
005980      05 ���������v�P�v�q               PIC 9(6)    VALUE ZERO.
005990**********
006000* �Q���� *
006010**********
006020   03 ���ʂQ�v�q.
006030      05 ��ÂQ�v�q.
006040         07 ��ÒP���Q�v�q              PIC 9(4)    VALUE ZERO.
006050         07 ��É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
006060         07 ��×��Q�v�q                PIC 9(5)    VALUE ZERO.
006070      05 ��㪖@�Q�v�q.
006080         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
006090         07 ��㪖@���Q�v�q              PIC 9(4)    VALUE ZERO.
006100      05 ��㪖@�Q�v�q.
006110         07 ��㪖@�񐔂Q�v�q            PIC 9(2)    VALUE ZERO.
006120         07 ��㪖@���Q�v�q              PIC 9(4)    VALUE ZERO.
006130      05 �d�ÂQ�v�q.
006140         07 �d�É񐔂Q�v�q              PIC 9(2)    VALUE ZERO.
006150         07 �d�×��Q�v�q                PIC 9(4)    VALUE ZERO.
006160      05 ���v�Q�v�q                     PIC 9(6)    VALUE ZERO.
006170      05 �����������Q�v�q               PIC 9(3)    VALUE ZERO.
006180      05 ���������v�Q�v�q               PIC 9(6)    VALUE ZERO.
006190******************
006200* �R���ʁ^�W�� *
006210******************
006220   03 ���ʂR�W�v�q.
006230      05 ��ÂR�W�v�q.
006240         07 ��ÒP���R�W�v�q              PIC 9(4)  VALUE ZERO.
006250         07 ��É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
006260         07 ��×��R�W�v�q                PIC 9(5)  VALUE ZERO.
006270      05 ��㪖@�R�W�v�q.
006280         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
006290         07 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
006300      05 ��㪖@�R�W�v�q.
006310         07 ��㪖@�񐔂R�W�v�q            PIC 9(2)  VALUE ZERO.
006320         07 ��㪖@���R�W�v�q              PIC 9(4)  VALUE ZERO.
006330      05 �d�ÂR�W�v�q.
006340         07 �d�É񐔂R�W�v�q              PIC 9(2)  VALUE ZERO.
006350         07 �d�×��R�W�v�q                PIC 9(4)  VALUE ZERO.
006360      05 ���v�R�W�v�q                     PIC 9(6)  VALUE ZERO.
006370      05 �����ʍ����v�R�W�v�q             PIC 9(6)  VALUE ZERO.
006380      05 �����������R�W�v�q               PIC 9(3)  VALUE ZERO.
006390      05 ���������v�R�W�v�q               PIC 9(6)  VALUE ZERO.
006400******************
006410* �R���ʁ^�P�O�� *
006420******************
006430   03 ���ʂR�O�v�q.
006440      05 �����J�n�����R�O�v�q.
006450         07 �����J�n���R�O�v�q            PIC 9(2)  VALUE ZERO.
006460         07 �����J�n���R�O�v�q            PIC 9(2)  VALUE ZERO.
006470      05 ��ÂR�O�v�q.
006480         07 ��ÒP���R�O�v�q              PIC 9(4)  VALUE ZERO.
006490         07 ��É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
006500         07 ��×��R�O�v�q                PIC 9(5)  VALUE ZERO.
006510      05 ��㪖@�R�O�v�q.
006520         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
006530         07 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
006540      05 ��㪖@�R�O�v�q.
006550         07 ��㪖@�񐔂R�O�v�q            PIC 9(2)  VALUE ZERO.
006560         07 ��㪖@���R�O�v�q              PIC 9(4)  VALUE ZERO.
006570      05 �d�ÂR�O�v�q.
006580         07 �d�É񐔂R�O�v�q              PIC 9(2)  VALUE ZERO.
006590         07 �d�×��R�O�v�q                PIC 9(4)  VALUE ZERO.
006600      05 ���v�R�O�v�q                     PIC 9(6)  VALUE ZERO.
006610      05 �����������R�O�v�q               PIC 9(3)  VALUE ZERO.
006620      05 ���������v�R�O�v�q               PIC 9(6)  VALUE ZERO.
006630****************
006640* �S���ʁ^�T�� *
006650****************
006660   03 ���ʂS�T�v�q.
006670      05 ��ÂS�T�v�q.
006680         07 ��ÒP���S�T�v�q              PIC 9(4)  VALUE ZERO.
006690         07 ��É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
006700         07 ��×��S�T�v�q                PIC 9(5)  VALUE ZERO.
006710      05 ��㪖@�S�T�v�q.
006720         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
006730         07 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
006740      05 ��㪖@�S�T�v�q.
006750         07 ��㪖@�񐔂S�T�v�q            PIC 9(2)  VALUE ZERO.
006760         07 ��㪖@���S�T�v�q              PIC 9(4)  VALUE ZERO.
006770      05 �d�ÂS�T�v�q.
006780         07 �d�É񐔂S�T�v�q              PIC 9(2)  VALUE ZERO.
006790         07 �d�×��S�T�v�q                PIC 9(4)  VALUE ZERO.
006800      05 ���v�S�T�v�q                     PIC 9(6)  VALUE ZERO.
006810      05 �����ʍ����v�S�T�v�q             PIC 9(6)  VALUE ZERO.
006820      05 �����������S�T�v�q               PIC 9(3)  VALUE ZERO.
006830      05 ���������v�S�T�v�q               PIC 9(6)  VALUE ZERO.
006840****************
006850* �S���ʁ^�W�� *
006860****************
006870   03 ���ʂS�W�v�q.
006880      05 �����J�n�����S�W�v�q.
006890         07 �����J�n���S�W�v�q            PIC 9(2)  VALUE ZERO.
006900         07 �����J�n���S�W�v�q            PIC 9(2)  VALUE ZERO.
006910      05 ��ÂS�W�v�q.
006920         07 ��ÒP���S�W�v�q              PIC 9(4)  VALUE ZERO.
006930         07 ��É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
006940         07 ��×��S�W�v�q                PIC 9(5)  VALUE ZERO.
006950      05 ��㪖@�S�W�v�q.
006960         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
006970         07 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
006980      05 ��㪖@�S�W�v�q.
006990         07 ��㪖@�񐔂S�W�v�q            PIC 9(2)  VALUE ZERO.
007000         07 ��㪖@���S�W�v�q              PIC 9(4)  VALUE ZERO.
007010      05 �d�ÂS�W�v�q.
007020         07 �d�É񐔂S�W�v�q              PIC 9(2)  VALUE ZERO.
007030         07 �d�×��S�W�v�q                PIC 9(4)  VALUE ZERO.
007040      05 ���v�S�W�v�q                     PIC 9(6)  VALUE ZERO.
007050      05 �����ʍ����v�S�W�v�q             PIC 9(6)  VALUE ZERO.
007060      05 �����������S�W�v�q               PIC 9(3)  VALUE ZERO.
007070      05 ���������v�S�W�v�q               PIC 9(6)  VALUE ZERO.
007080******************
007090* �S���ʁ^�P�O�� *
007100******************
007110   03 ���ʂS�O�v�q.
007120      05 �����J�n�����S�O�v�q.
007130         07 �����J�n���S�O�v�q            PIC 9(2)  VALUE ZERO.
007140         07 �����J�n���S�O�v�q            PIC 9(2)  VALUE ZERO.
007150      05 ��ÂS�O�v�q.
007160         07 ��ÒP���S�O�v�q              PIC 9(4)  VALUE ZERO.
007170         07 ��É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
007180         07 ��×��S�O�v�q                PIC 9(5)  VALUE ZERO.
007190      05 ��㪖@�S�O�v�q.
007200         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
007210         07 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
007220      05 ��㪖@�S�O�v�q.
007230         07 ��㪖@�񐔂S�O�v�q            PIC 9(2)  VALUE ZERO.
007240         07 ��㪖@���S�O�v�q              PIC 9(4)  VALUE ZERO.
007250      05 �d�ÂS�O�v�q.
007260         07 �d�É񐔂S�O�v�q              PIC 9(2)  VALUE ZERO.
007270         07 �d�×��S�O�v�q                PIC 9(4)  VALUE ZERO.
007280      05 ���v�S�O�v�q                     PIC 9(6)  VALUE ZERO.
007290      05 �����������S�O�v�q               PIC 9(3)  VALUE ZERO.
007300      05 ���������v�S�O�v�q               PIC 9(6)  VALUE ZERO.
007310********************
007320* �T���ʁ^�Q�D�T�� *
007330********************
007340   03 ���ʂT�Q�v�q.
007350      05 ��ÂT�Q�v�q.
007360         07 ��ÒP���T�Q�v�q              PIC 9(4)  VALUE ZERO.
007370         07 ��É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
007380         07 ��×��T�Q�v�q                PIC 9(5)  VALUE ZERO.
007390      05 ��㪖@�T�Q�v�q.
007400         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
007410         07 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
007420      05 ��㪖@�T�Q�v�q.
007430         07 ��㪖@�񐔂T�Q�v�q            PIC 9(2)  VALUE ZERO.
007440         07 ��㪖@���T�Q�v�q              PIC 9(4)  VALUE ZERO.
007450      05 �d�ÂT�Q�v�q.
007460         07 �d�É񐔂T�Q�v�q              PIC 9(2)  VALUE ZERO.
007470         07 �d�×��T�Q�v�q                PIC 9(4)  VALUE ZERO.
007480      05 ���v�T�Q�v�q                     PIC 9(6)  VALUE ZERO.
007490      05 �����ʍ����v�T�Q�v�q             PIC 9(6)  VALUE ZERO.
007500      05 �����������T�Q�v�q               PIC 9(3)  VALUE ZERO.
007510      05 ���������v�T�Q�v�q               PIC 9(6)  VALUE ZERO.
007520****************
007530* �T���ʁ^�T�� *
007540****************
007550   03 ���ʂT�T�v�q.
007560      05 �����J�n�����T�T�v�q.
007570         07 �����J�n���T�T�v�q            PIC 9(2)  VALUE ZERO.
007580         07 �����J�n���T�T�v�q            PIC 9(2)  VALUE ZERO.
007590      05 ��ÂT�T�v�q.
007600         07 ��ÒP���T�T�v�q              PIC 9(4)  VALUE ZERO.
007610         07 ��É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
007620         07 ��×��T�T�v�q                PIC 9(5)  VALUE ZERO.
007630      05 ��㪖@�T�T�v�q.
007640         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
007650         07 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
007660      05 ��㪖@�T�T�v�q.
007670         07 ��㪖@�񐔂T�T�v�q            PIC 9(2)  VALUE ZERO.
007680         07 ��㪖@���T�T�v�q              PIC 9(4)  VALUE ZERO.
007690      05 �d�ÂT�T�v�q.
007700         07 �d�É񐔂T�T�v�q              PIC 9(2)  VALUE ZERO.
007710         07 �d�×��T�T�v�q                PIC 9(4)  VALUE ZERO.
007720      05 ���v�T�T�v�q                     PIC 9(6)  VALUE ZERO.
007730      05 �����ʍ����v�T�T�v�q             PIC 9(6)  VALUE ZERO.
007740      05 �����������T�T�v�q               PIC 9(3)  VALUE ZERO.
007750      05 ���������v�T�T�v�q               PIC 9(6)  VALUE ZERO.
007760****************
007770* �T���ʁ^�W�� *
007780****************
007790   03 ���ʂT�W�v�q.
007800      05 �����J�n�����T�W�v�q.
007810         07 �����J�n���T�W�v�q            PIC 9(2)  VALUE ZERO.
007820         07 �����J�n���T�W�v�q            PIC 9(2)  VALUE ZERO.
007830      05 ��ÂT�W�v�q.
007840         07 ��ÒP���T�W�v�q              PIC 9(4)  VALUE ZERO.
007850         07 ��É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
007860         07 ��×��T�W�v�q                PIC 9(5)  VALUE ZERO.
007870      05 ��㪖@�T�W�v�q.
007880         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
007890         07 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
007900      05 ��㪖@�T�W�v�q.
007910         07 ��㪖@�񐔂T�W�v�q            PIC 9(2)  VALUE ZERO.
007920         07 ��㪖@���T�W�v�q              PIC 9(4)  VALUE ZERO.
007930      05 �d�ÂT�W�v�q.
007940         07 �d�É񐔂T�W�v�q              PIC 9(2)  VALUE ZERO.
007950         07 �d�×��T�W�v�q                PIC 9(4)  VALUE ZERO.
007960      05 ���v�T�W�v�q                     PIC 9(6)  VALUE ZERO.
007970      05 �����ʍ����v�T�W�v�q             PIC 9(6)  VALUE ZERO.
007980      05 �����������T�W�v�q               PIC 9(3)  VALUE ZERO.
007990      05 ���������v�T�W�v�q               PIC 9(6)  VALUE ZERO.
008000******************
008010* �T���ʁ^�P�O�� *
008020******************
008030   03 ���ʂT�O�v�q.
008040      05 �����J�n�����T�O�v�q.
008050         07 �����J�n���T�O�v�q            PIC 9(2)  VALUE ZERO.
008060         07 �����J�n���T�O�v�q            PIC 9(2)  VALUE ZERO.
008070      05 ��ÂT�O�v�q.
008080         07 ��ÒP���T�O�v�q              PIC 9(4)  VALUE ZERO.
008090         07 ��É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
008100         07 ��×��T�O�v�q                PIC 9(5)  VALUE ZERO.
008110      05 ��㪖@�T�O�v�q.
008120         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
008130         07 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
008140      05 ��㪖@�T�O�v�q.
008150         07 ��㪖@�񐔂T�O�v�q            PIC 9(2)  VALUE ZERO.
008160         07 ��㪖@���T�O�v�q              PIC 9(4)  VALUE ZERO.
008170      05 �d�ÂT�O�v�q.
008180         07 �d�É񐔂T�O�v�q              PIC 9(2)  VALUE ZERO.
008190         07 �d�×��T�O�v�q                PIC 9(4)  VALUE ZERO.
008200      05 ���v�T�O�v�q                     PIC 9(6)  VALUE ZERO.
008210      05 �����������T�O�v�q               PIC 9(3)  VALUE ZERO.
008220      05 ���������v�T�O�v�q               PIC 9(6)  VALUE ZERO.
008000*******************
008010*  ���׏����s���Z */202206
008020*******************
008030   03 ���׏����s���Z���v�q                PIC ZZZ   VALUE ZERO.
008030   03 ���׏����s���Z���v�q                PIC ZZ    VALUE ZERO.
008230*
008240**************
008250* �{�p����� *
008260**************
008270 01 �{�p�����v.
008280    03 �_���t�ԍ��v                    PIC X(22)  VALUE SPACE.
008290    03 �ڍ��t�����ԍ��v              PIC X(10)  VALUE SPACE.
008300    03 ��\�҃J�i�v                    PIC X(50)  VALUE SPACE.
008310    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
008320    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
          03 �s���{���i�h�r�v                PIC X(2)   VALUE SPACE.
008330    03 �{�p���Z���v.
008340       05 �{�p���Z���P�v               PIC X(50)  VALUE SPACE.
008350       05 �{�p���Z���Q�v               PIC X(50)  VALUE SPACE.
008360    03 �{�p���X�֔ԍ��v.
008370       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
008380       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
008390    03 �{�p���d�b�ԍ��v                PIC X(15)  VALUE SPACE.
008400    03 ��z���󗝔ԍ��v                PIC X(15)  VALUE SPACE.
008410    03 �󗝔N�����v.
007350       05 �󗝘a��v                   PIC 9      VALUE ZERO.
008420       05 �󗝔N�v                     PIC 9(2)   VALUE ZERO.
008430       05 �󗝌��v                     PIC 9(2)   VALUE ZERO.
008440       05 �󗝓��v                     PIC 9(2)   VALUE ZERO.
008450    03 �ŏI�ʉ@�N�����v.
007390       05 �ŏI�ʉ@�a��v               PIC 9      VALUE ZERO.
008460       05 �ŏI�ʉ@�N�v                 PIC 9(2)   VALUE ZERO.
008470       05 �ŏI�ʉ@���v                 PIC 9(2)   VALUE ZERO.
008480       05 �ŏI�ʉ@���v                 PIC 9(2)   VALUE ZERO.
008490    03 �_���t�N�����v.
007430       05 �_���t�a��v                 PIC 9      VALUE ZERO.
008500       05 �_���t�N�v                   PIC 9(2)   VALUE ZERO.
008510       05 �_���t���v                   PIC 9(2)   VALUE ZERO.
008520       05 �_���t���v                   PIC 9(2)   VALUE ZERO.
008530    03 ���҈ϔC�N�����v.
007470       05 ���҈ϔC�a��v               PIC 9      VALUE ZERO.
008540       05 ���҈ϔC�N�v                 PIC 9(2)   VALUE ZERO.
008550       05 ���҈ϔC���v                 PIC 9(2)   VALUE ZERO.
008560       05 ���҈ϔC���v                 PIC 9(2)   VALUE ZERO.
008570    03 �������v.
008580        05 ������s���v              PIC X(40)  VALUE SPACE.
008590        05 ������s�x�X���v          PIC X(40)  VALUE SPACE.
008600        05 �a����ʂv                  PIC 9(1)   VALUE ZERO.
008610        05 �����ԍ��v                  PIC X(10)  VALUE SPACE.
008620        05 �������`�l�v.
008620           07 �������`�l�P�v           PIC X(40)  VALUE SPACE.
008620           07 �������`�l�Q�v           PIC X(40)  VALUE SPACE.
008630        05 �������`�l�J�i�v.
008630           07 �������`�l�J�i�P�v       PIC X(60)  VALUE SPACE.
008630           07 �������`�l�J�i�Q�v       PIC X(50)  VALUE SPACE.
008630           07 �������`�l�J�i�R�v       PIC X(40)  VALUE SPACE.
008640        05 ��s���x�X���v              PIC X(60)  VALUE SPACE.
008650        05 �a����ʃR�����g�v          PIC N(2)   VALUE SPACE.
          03 �x���@��.
             05 ���Z�@�֖��v.
                07 ���Z�@�֖��P�v            PIC X(12) VALUE SPACE.
                07 ���Z�@�֖��Q�v            PIC X(12) VALUE SPACE.
                07 ���Z�@�֖��R�v            PIC X(12) VALUE SPACE.
                07 ���Z�@�֖��S�v            PIC X(12) VALUE SPACE.
                07 ���Z�@�֖��T�v            PIC X(8)  VALUE SPACE.
             05 �x�X���v.
                07 �x�X���P�v                PIC X(12) VALUE SPACE.
                07 �x�X���Q�v                PIC X(12) VALUE SPACE.
                07 �x�X���R�v                PIC X(12) VALUE SPACE.
                07 �x�X���S�v                PIC X(12) VALUE SPACE.
             05 �U���`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ���ʃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �����`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ��s�`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ���Ƀ`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �_���`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �{�X�`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �x�X�`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 �{�x���`�F�b�N�v             PIC N(1)  VALUE SPACE.
008660    03 ���{�p�h�c�v                    PIC X(15)  VALUE SPACE.
008670    03 �s�����{�p�h�c�v                PIC X(15)  VALUE SPACE.
008680    03 �_���t�ԍ��Q�v                  PIC X(22)  VALUE SPACE.
007330    03 ���ϔԍ��v                      PIC X(28)  VALUE SPACE.
008690**************
008700* ��f�ҏ�� *
008710**************
008720 01 ��f�ҏ��v.
          03 �{�p�a��v                      PIC 9(1)   VALUE ZERO.
008730    03 �{�p�N���v.
008740       05 �{�p�N�v                     PIC 9(2)   VALUE ZERO.
008750       05 �{�p���v                     PIC 9(2)   VALUE ZERO.
008760*    03 �L���v                          PIC N(12)  VALUE SPACE.
007570    03 �L���v.
007580       05 ����L���v                   PIC N(12)  VALUE SPACE.
          03 �L���ԍ��v.
             05 �L���ԍ��w�v                 PIC X(40) VALUE SPACE.
008770    03 �ԍ��v.
008780       05 ����ԍ��v                   PIC X(15)  VALUE SPACE.
008790       05 FILLER                       PIC X(15)  VALUE SPACE.
008800    03 �ی��Ҕԍ��v.
008810       05 ����ی��Ҕԍ��v             PIC X(8)   VALUE SPACE.
008820       05 FILLER                       PIC X(2)   VALUE SPACE.
008830    03 �s�����ԍ��v.
008840       05 ����s�����ԍ��v             PIC X(8)   VALUE SPACE.
008850       05 FILLER                       PIC X(2).
008860*
008870    03 �����於�̂v.
008880       05 ��������於�̂P�v           PIC X(54)  VALUE SPACE.
008890       05 ��������於�̂Q�v           PIC X(32)  VALUE SPACE.
008870    03 �����於�̂v�q                  PIC X(80)  VALUE SPACE.
008900*
008910    03 �ی���ʂv                      PIC 9(2)   VALUE ZERO.
008870    03 �ی���ʖ��̂v.
008880       05 �ی���ʖ��̂v�o             PIC N(3)  VALUE SPACE.
007390    03 �ی���ʃ`�F�b�N�v.
007400       05 �Еۃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
007410       05 �D���`�F�b�N�v               PIC N(1)  VALUE SPACE.
007420       05 �g���`�F�b�N�v               PIC N(1)  VALUE SPACE.
007430       05 ���ۃ`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ���σ`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ���`�F�b�N�v                 PIC N(1)  VALUE SPACE.
             05 �ސE�`�F�b�N�v               PIC N(1)  VALUE SPACE.
             05 ����`�F�b�N�v               PIC N(1)  VALUE SPACE.
          03 �{�l�`�F�b�N�v                  PIC N(1)   VALUE SPACE.
          03 �Ƒ��`�F�b�N�v                  PIC N(1)   VALUE SPACE.
          03 �P�ƃ`�F�b�N�v                  PIC N(1)   VALUE SPACE.
          03 �Q���`�F�b�N�v                  PIC N(1)   VALUE SPACE.
          03 ����`�F�b�N�v                  PIC N(1)   VALUE SPACE.
          03 ���V�`�F�b�N�v                  PIC N(1)   VALUE SPACE.
          03 �U�΃`�F�b�N�v                  PIC N(1)   VALUE SPACE.
007750    03 ���t�����`�F�b�N�v.
007760       05 �V���`�F�b�N�v               PIC N(1)  VALUE SPACE.
007770       05 �W���`�F�b�N�v               PIC N(1)  VALUE SPACE.
007780       05 �X���`�F�b�N�v               PIC N(1)  VALUE SPACE.
007790       05 �P�O���`�F�b�N�v             PIC N(1)  VALUE SPACE.
008920    03 ��ی��ҏ��v.
008930       05 ��ی��҃J�i�v               PIC X(50)  VALUE SPACE.
008940       05 ��ی��Ҏ����v               PIC X(50)  VALUE SPACE.
008950       05 �X�֔ԍ��v.
008960          07 �X�֔ԍ��P�v              PIC X(3)   VALUE SPACE.
008970          07 �X�֔ԍ��Q�v              PIC X(4)   VALUE SPACE.
008980       05 ��ی��ҏZ���P�v             PIC X(50)  VALUE SPACE.
008990       05 ��ی��ҏZ���Q�v             PIC X(50)  VALUE SPACE.
008990       05 �d�b�ԍ��v                   PIC X(35)  VALUE SPACE.
           03 �󋋎Ҕԍ��v.
              05 ����󋋎Ҕԍ��v            PIC X(7)  VALUE SPACE.
              05 ����󋋎Ҕԍ��Q�v          PIC X(8)  VALUE SPACE.
009000    03 ���ҏ��v.
009010       05 ���҃J�i�v                   PIC X(50)  VALUE SPACE.
009020       05 ���Ҏ����v                   PIC X(50)  VALUE SPACE.
008980       05 ���ҏZ���P�v                 PIC X(50)  VALUE SPACE.
008990       05 ���ҏZ���Q�v                 PIC X(50)  VALUE SPACE.
009030       05 ���ʃ`�F�b�N�v.
009040          07 �j�`�F�b�N�v              PIC N(1)  VALUE SPACE.
009050          07 ���`�F�b�N�v              PIC N(1)  VALUE SPACE.
009060       05 �a��`�F�b�N�v.
009070          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
009080          07 �吳�`�F�b�N�v            PIC N(1)  VALUE SPACE.
009090          07 ���a�`�F�b�N�v            PIC N(1)  VALUE SPACE.
009100          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
      */�����C��/������20190405
008210          07 �ߘa�`�F�b�N�v            PIC N(1)  VALUE SPACE.
                07 �ߘa�b�l�v                PIC X(4)  VALUE SPACE.
009110          07 �����v                    PIC N(2)  VALUE SPACE.
009120       05 ���ҔN�v                     PIC 9(2)  VALUE ZERO.
009130       05 ���Ҍ��v                     PIC 9(2)  VALUE ZERO.
009140       05 ���ғ��v                     PIC 9(2)  VALUE ZERO.
009150       05 �����v.
009160          07 ��������v                PIC N(4)  VALUE SPACE.
009170          07 FILLER                    PIC X(4)  VALUE SPACE.
009180       05 �����`�F�b�N�v.
009190          07 �����{�l�`�F�b�N�v        PIC N(1)  VALUE SPACE.
009200          07 �����Ƒ��`�F�b�N�v        PIC N(1)  VALUE SPACE.
009210*
009220       05 ���������P�v                 PIC X(72) VALUE SPACE.
009230       05 ���������Q�v                 PIC X(72) VALUE SPACE.
009240       05 ���������R�v                 PIC X(72) VALUE SPACE.
009250       05 ���������S�v                 PIC X(72) VALUE SPACE.
009260       05 ���������T�v                 PIC X(72) VALUE SPACE.
009270*
009280       05 ���������v                   PIC X(80) OCCURS 36 VALUE SPACE.
009290*
009300    03 ������v                        PIC N(1)  VALUE SPACE.
009310    03 �����ԍ��v                      PIC X(2)  VALUE SPACE.
009320    03 ���ʃR�����g�v                  PIC X(16) VALUE SPACE.
009330    03 ���ۘA�p�L���v                  PIC N(1)  VALUE SPACE.
009340    03 ���ۘA�p�}���v                  PIC N(1)  VALUE SPACE.
009350*
009360****************
009370* �����f�[�^�e *
009380****************
009390 01 �������v.
009400    03 ���ʐ��v                        PIC 9(1)  VALUE ZERO.
009410    03 ���ʏ��v  OCCURS   9.
009420       05 ���ʂb�m�s�v                 PIC 9(1)  VALUE ZERO.
009430       05 ���ʃR�[�h�v.
009440          07 ������ʂv                PIC 9(2)  VALUE ZERO.
009450          07 ���ʂv                    PIC 9(2)  VALUE ZERO.
009460          07 ���E�敪�v                PIC 9(1)  VALUE ZERO.
009470          07 �����ʒu�ԍ��v            PIC 9(2)  VALUE ZERO.
009480       05 �������v                     PIC N(18) VALUE SPACE.
009490       05 �����N�����v.
009500          07 �����N�v                  PIC 9(2)  VALUE ZERO.
009510          07 �������v                  PIC 9(2)  VALUE ZERO.
009520          07 �������v                  PIC 9(2)  VALUE ZERO.
009530       05 �����N�����v.
009540          07 �����N�v                  PIC 9(2)  VALUE ZERO.
009550          07 �������v                  PIC 9(2)  VALUE ZERO.
009560          07 �������v                  PIC 9(2)  VALUE ZERO.
009570       05 �J�n�N�����v.
009580          07 �J�n�N�v                  PIC 9(2)  VALUE ZERO.
009590          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
009600          07 �J�n���v                  PIC 9(2)  VALUE ZERO.
009610       05 �I���N�����v.
009620          07 �I���N�v                  PIC 9(2)  VALUE ZERO.
009630          07 �I�����v                  PIC 9(2)  VALUE ZERO.
009640          07 �I�����v                  PIC 9(2)  VALUE ZERO.
009650       05 �������v                     PIC 9(2)  VALUE ZERO.
009660       05 �]�A�敪�v                   PIC 9(1)  VALUE ZERO.
009670       05 �]�A�敪�`�F�b�N�v.
009680          07 �����`�F�b�N�v            PIC N(1)  VALUE SPACE.
009690          07 ���~�`�F�b�N�v            PIC N(1)  VALUE SPACE.
009700          07 �]��`�F�b�N�v            PIC N(1)  VALUE SPACE.
009710       05 �J�n�N�����擾�t���O         PIC X(3)  VALUE SPACE.
009720       05 ���ʋ�؂v                   PIC X(1)  VALUE SPACE.
009730       05 �o�ߗ��̂v.
009740          07 ����o�ߗ��̂v            PIC N(5)  VALUE SPACE.
009750          07 FILLER                    PIC X(2)  VALUE SPACE.
009760    03 �o�ߕ��ʂv                      PIC N(1)  VALUE SPACE.
009770    03 �V�K�`�F�b�N�v                  PIC N(1)  VALUE SPACE.
009780    03 �p���`�F�b�N�v                  PIC N(1)  VALUE SPACE.
          03 �{�p���v.
             05 �{�p���`�F�b�N�v   OCCURS 31 PIC N(1)  VALUE SPACE.
009790*
009800************
009810* ������� *
009820************
009830 01 �������v.
009840    03 �������Z�v.
009850       05 ���ԊO�`�F�b�N�v                PIC N(1) VALUE SPACE.
009860       05 �x���`�F�b�N�v                  PIC N(1) VALUE SPACE.
009870       05 �[��`�F�b�N�v                  PIC N(1) VALUE SPACE.
009880    03 ���É��Z�v.
009890       05 ��ԃ`�F�b�N�v                  PIC N(1) VALUE SPACE.
009900       05 �\���J��`�F�b�N�v              PIC N(1) VALUE SPACE.
009910    03 �������q�`�F�b�N�v.
009920       05 ��`�F�b�N�v                    PIC N(1) VALUE SPACE.
009930       05 ���`�F�b�N�v                    PIC N(1) VALUE SPACE.
009940       05 ���`�F�b�N�v                    PIC N(1) VALUE SPACE.
009950    03 ���v�v                             PIC 9(7) VALUE ZERO.
009960    03 ���񏈒u�����v�v                   PIC 9(6) VALUE ZERO.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
          03 �����񐔂v                         PIC 9(2)  VALUE ZERO.
          03 �^���񐔂v                         PIC 9(1)  VALUE ZERO.
          03 �^�����v                           PIC 9(5)  VALUE ZERO.
009970************
009980* ���l��� *
009990************
010000 01 ���l���v.
010010    03 �K�p�P�v                        PIC N(38) VALUE SPACE.
010020    03 �K�p�Q�v                        PIC N(38) VALUE SPACE.
010030    03 �K�p�R�v                        PIC X(40) VALUE SPACE.
010040*    03 �K�p�S�v                        PIC N(38) VALUE SPACE.
010050    03 �o�߃R�����g�v                  PIC N(60) VALUE SPACE.
010060**
003720*--- ���S���t�����p ---*
003730 01 ���S�����v                         PIC 9(2)  VALUE ZERO.
003740 01 ���t�����v                         PIC 9(2)  VALUE ZERO.
010070*
       01 �E�v�{�p���v                       PIC X(100) VALUE SPACE.
       01 �{�p���v.
          03 �{�p���Q�v                      PIC X(1)  VALUE SPACE.
          03 �{�p���P�v                      PIC X(1)  VALUE SPACE.
      */�ϔC�ҏ��
       01 �ϔC�ҏ��v.
          03 �ڍ��t��v.
            05 �ڍ��t��m�v                PIC X(50) VALUE SPACE.
          03 �ڍ��t�����v.
            05 �ڍ��t�����m�v            PIC X(50) VALUE SPACE.
          03 ��Z���v                        PIC X(80) VALUE SPACE.
          03  �ϔC�c�̖��v                   PIC X(60) VALUE SPACE.
          03  �ϔC�Җ��v                     PIC X(60) VALUE SPACE.
          03  �㗝�l�X�֔ԍ��v               PIC X(10) VALUE SPACE.
          03  �㗝�l�Z���v.
            05  �㗝�l�Z���P�v               PIC X(50) VALUE SPACE.
            05  �㗝�l�Z���Q�v               PIC X(50) VALUE SPACE.
          03  �ϔC�d�b�ԍ��P�v               PIC X(20) VALUE SPACE.
          03  �ϔC�d�b�ԍ��Q�v               PIC X(20) VALUE SPACE.
          03 ���㗝�l�b�l�v                PIC X(10) VALUE SPACE.
       01 �ϔC�R�����g�v                     PIC X(200) VALUE SPACE.
       01 �ϔC�R�����g�v�Q                   PIC X(200) VALUE SPACE.
       01 �ϔC�R�����g�v�T.
          03 �ϔC�R�����g�P�v                PIC X(84) VALUE SPACE.
          03 �ϔC�R�����g�Q�v                PIC X(84) VALUE SPACE.
          03 �ϔC�R�����g�R�v                PIC X(40) VALUE SPACE.
          03 �ϔC�R�����g�S�v                PIC X(40) VALUE SPACE.
          03 �ϔC�R�����g�T�v                PIC X(34) VALUE SPACE.
          03 FILLER                          PIC X(50).
010080*-------------------------------------------------------------------*
010090 01 �������.
010100     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
010110     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
010120     03 ������ʂo                     PIC X(2) VALUE SPACE.
010130     03 �g������o.
010140         05 �[������o.
010150             07 �ړ������o             PIC X(1) VALUE SPACE.
010160             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
010170         05 �ڍא���o                 PIC X(2) VALUE SPACE.
010180     03 �ʒm���o                     PIC X(2) VALUE SPACE.
010190     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
010200*
010210 01 �v�Z�@����N�v                     PIC 9(2) VALUE ZERO.
010220* ���t�v�n�q�j
010230 01 �a��I���N�v                       PIC 9(4) VALUE ZERO.
010240 01 �v�Z�@����.
010250    03 �v�Z�@����N                    PIC 9(4) VALUE ZERO.
010260    03 �v�Z�@�����                  PIC 9(4) VALUE ZERO.
010270 01 �v�Z�@����q REDEFINES �v�Z�@����.
010280    03 �v�Z�@���I                      PIC 9(2).
010290    03 �v�Z�@���t                      PIC 9(6).
010300    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
010310       05 �v�Z�@�N��                   PIC 9(4).
010320       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
010330         07 �v�Z�@�N                   PIC 9(2).
010340         07 �v�Z�@��                   PIC 9(2).
010350       05 �v�Z�@��                     PIC 9(2).
010360*
      * C �A�g�p
       01  �����P�v        PIC X(4096).
       01  �����Q�v        PIC X(512).
       01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
      *
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      *
010370******************************************************************
010380*                          �A������                              *
010390******************************************************************
010400************
010410* ����L�[ *
010420************
010430*
       01 �A���|�v���r���[ IS EXTERNAL.
          03 �A���|�v���r���[�敪          PIC 9.
010440*
010450 01 �A����|�Ώۃf�[�^ IS EXTERNAL.
010460    03 �A����|�{�p�N����.
010470       05 �A����|�{�p�a��                  PIC 9(1).
010480       05 �A����|�{�p�N                    PIC 9(2).
010490       05 �A����|�{�p��                    PIC 9(2).
010500    03 �A����|���҃R�[�h.
010510       05 �A����|���Ҕԍ�                  PIC 9(6).
010520       05 �A����|�}��                      PIC X(1).
010530    03 �A����|�ی����                     PIC 9(2).
010540    03 �A����|�ی��Ҕԍ�                   PIC X(10).
010550    03 �A����|������                     PIC 9(2).
010560    03 �A����|��p���S�Ҕԍ�               PIC X(10).
010570    03 �A����|�������                     PIC 9(2).
010580    03 �A����|��p���S�Ҕԍ�����           PIC X(10).
010590    03 �A����|���҃J�i                     PIC X(20).
010600    03 �A����|�{�l�Ƒ��敪                 PIC 9(1).
013600*
001408************************
014090** �R�J����������
014100************************
014110 01 �A���ԁ|�L�[ IS EXTERNAL.
014120    03 �A���ԁ|�{�p�N��.
014130       05 �A���ԁ|�{�p�a��               PIC 9.
014140       05 �A���ԁ|�{�p�N                 PIC 9(2).
014150       05 �A���ԁ|�{�p��                 PIC 9(2).
014160    03  �A���ԁ|���҃R�[�h.
014170       05 �A���ԁ|���Ҕԍ�               PIC 9(6).
014180       05 �A���ԁ|�}��                   PIC X.
014190    03 �A���ԁ|�Ώۃt���O                PIC X(3).
014200    03 �A���ԁ|���Ԍ��v.
014210       05 �A���ԁ|���Ԃv                 PIC 9(2) OCCURS 9.
014220*
014230************************
014240* �������R���Z�b�g     *
014250************************
014260 01 �A�����|�L�[ IS EXTERNAL.
014270    03 �A�����|�{�p�N��.
014280       05 �A�����|�{�p�a��               PIC 9.
014290       05 �A�����|�{�p�N                 PIC 9(2).
014300       05 �A�����|�{�p��                 PIC 9(2).
014310    03  �A�����|���҃R�[�h.
014320       05 �A�����|���Ҕԍ�               PIC 9(6).
014330       05 �A�����|�}��                   PIC X.
014340    03 �A�����|������                    PIC 9(2).
014350    03 �A�����|���R��                    PIC N(63) OCCURS 15.
014360*
014230************************
014240* �E�v���Z�b�g     *
014250************************
014260 01 �A�E���|�L�[ IS EXTERNAL.
014270    03 �A�E���|�{�p�N��.
014280       05 �A�E���|�{�p�a��               PIC 9.
014290       05 �A�E���|�{�p�N                 PIC 9(2).
014300       05 �A�E���|�{�p��                 PIC 9(2).
014310    03  �A�E���|���҃R�[�h.
014320       05 �A�E���|���Ҕԍ�               PIC 9(6).
014330       05 �A�E���|�}��                   PIC X.
014340    03 �A�E���|������                    PIC 9(2).
014350    03 �A�E���|�E�v��                    PIC X(126) OCCURS 30.
014340    03 �A�E���|�����敪                  PIC 9(1).
014370*
014380************************
014390* �������Z�܂Ƃ�
014400************************
014410 01 �A���Z�܂Ƃ߁|�L�[ IS EXTERNAL.
014420    03 �A���Z�܂Ƃ߁|�{�p�a��N��.
014430       05 �A���Z�܂Ƃ߁|�{�p�a��               PIC 9.
014440       05 �A���Z�܂Ƃ߁|�{�p�N��.
014450          07 �A���Z�܂Ƃ߁|�{�p�N              PIC 9(2).
014460          07 �A���Z�܂Ƃ߁|�{�p��              PIC 9(2).
014470    03 �A���Z�܂Ƃ߁|���҃R�[�h.
014480       05 �A���Z�܂Ƃ߁|���Ҕԍ�               PIC 9(6).
014490       05 �A���Z�܂Ƃ߁|�}��                   PIC X(1).
014500**-------------------------------------------------------**
014510*   1:�������Z�v�g�Ȃ��̖{�̂܂Ƃ߂̔���
014520*   2:���l�E���p�̎Еۏ������Z���̔���
014530    03 �A���Z�܂Ƃ߁|����敪                  PIC 9.
014540**-------------------------------------------------------**
014550*  / OUT /�@ 0:�ΏۊO�A1:�Ώ�
014560    03 �A���Z�܂Ƃ߁|���茋��                  PIC 9.
014570**
014580*
014590**  ��ʓ��̓f�[�^
014600 01 �A���|���̓f�[�^�ϔC��� IS EXTERNAL.
014610    03 �A���|�ϔC���                     PIC 9.
014620*
       01 �A���|���̓f�[�^�d�b��� IS EXTERNAL.
          03 �A���|�d�b���                     PIC 9.
014630*
014640*************
014650* ��������
014660*************
014670 01 �A�������́|�L�[ IS EXTERNAL.
014680    03 �A�������́|�������             PIC 9(2).
014690    03 �A�������́|��p���S�Ҕԍ�����   PIC X(10).
014700*   / OUT /
014710    03 �A�������́|���̏W�c.
014720       05 �A�������́|�P����            PIC N.
014730       05 �A�������́|����              PIC N(4).
014740       05 �A�������́|��������          PIC N(10).
014070 01 �A�������́|��L�[ IS EXTERNAL.
014080    03 �A�������́|����R�[�h           PIC 9(2).
014750*
014762************************
014763* ���Z���������������
014764************************
014765 01 �A���Z������|�L�[ IS EXTERNAL.
014766    03 �A���Z������|�{�p�N��.
014767       05 �A���Z������|�{�p�a��               PIC 9.
014768       05 �A���Z������|�{�p�N                 PIC 9(2).
014769       05 �A���Z������|�{�p��                 PIC 9(2).
014770    03  �A���Z������|���҃R�[�h.
014771       05 �A���Z������|���Ҕԍ�               PIC 9(6).
014772       05 �A���Z������|�}��                   PIC X.
014773    03 �A���Z������|�Ώۃt���O                PIC X(3).
014774*
000540************************************
000550* �v�����^�t�@�C���쐬�p           *
000560************************************
000570 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000580     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
000590     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
000600     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
000610     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
000993************************************
000994* �v�����^�t�@�C���쐬����p       *
000995************************************
000996 01 �g�A����o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000997     03 �g�A����o�q�s�e�|�p�����         PIC X(8).
006490*
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
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
       01 �A���^�|�L�[ IS EXTERNAL.
          03 �A���^�|�{�p�a��N��.
             05 �A���^�|�{�p�a��                  PIC 9(1).
             05 �A���^�|�{�p�N��.
                07 �A���^�|�{�p�N                 PIC 9(2).
                07 �A���^�|�{�p��                 PIC 9(2).
          03 �A���^�|���҃R�[�h.
             05 �A���^�|���Ҕԍ�                  PIC 9(6).
             05 �A���^�|�}��                      PIC X(1).
          03 �A���^�|�ی����                     PIC 9(2).
          03 �A���^�|��R�[�h                     PIC 9(2).
          03 �A���^�|�p�����                     PIC 9(1).
          03 �A���^�|�������q.
             05 �A���^�|�������q�b�l              PIC X(200).
             05 �A���^�|�������q����              OCCURS 5.
                07 �A���^�|�������q�a��N����     OCCURS 3.
                   09 �A���^�|�������q�a��N��.
                      11 �A���^�|�������q�a��     PIC 9(1).
                      11 �A���^�|�������q�N��.
                         13 �A���^�|�������q�N    PIC 9(2).
                         13 �A���^�|�������q��    PIC 9(2).
                   09 �A���^�|�������q��          PIC 9(2).
          03 �A���^�|�^�����.
             05 �A���^�|�^����Âb�l              PIC X(100).
             05 �A���^�|�^����                    PIC 9(2)    OCCURS 5.
      * 
014775******************************************************************
014780*                      PROCEDURE  DIVISION                       *
014790******************************************************************
014800 PROCEDURE               DIVISION.
014810************
014820*           *
014830* ��������   *
014840*           *
014850************
002570     PERFORM �v�����^�t�@�C���쐬.
014860     PERFORM ������.
014870************
014880*           *
014890* �又��     *
014900*           *
014910************
014920* ���
014930     PERFORM �A�����ڑҔ�.
014940     PERFORM ����Z�b�g.
014950     PERFORM �������.
014960************
014970*           *
014980* �I������   *
014990*           *
015000************
015010     PERFORM ��f�҈���敪�X�V.
015020     PERFORM �I������.
015030     MOVE ZERO  TO PROGRAM-STATUS.
015040     EXIT PROGRAM.
015050*
015060*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002860*================================================================*
002870 �v�����^�t�@�C���쐬 SECTION.
002880*================================================================*
002890*   / ������ /
002900     MOVE SPACE TO �g�A�o�q�s�e�|�쐬�f�[�^.
002910     INITIALIZE �g�A�o�q�s�e�|�쐬�f�[�^.
002225     MOVE SPACE TO �g�A����o�q�s�e�|�쐬�f�[�^.
002226     INITIALIZE �g�A����o�q�s�e�|�쐬�f�[�^.
002920*
002930*
002940*--���� �ύX�ӏ� ����--------------------------------------*
002230*   �g�p����p����ʃZ�b�g
           MOVE "RECE"                TO �g�A����o�q�s�e�|�p�����.
002970*   �g�p����v�����^�t�@�C�����Z�b�g
002971     MOVE "PRTF002"             TO �g�A�o�q�s�e�|�t�@�C����.
002972*
002973*   �g�p���钠�[�v���O�������Z�b�g
002974     MOVE "YDT6421"             TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪  TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
015070*================================================================*
015080 ������ SECTION.
015090*
015100     PERFORM �t�@�C���I�[�v��.
015110*    /* ���ݓ��t�擾 */
015120     ACCEPT �v�Z�@���t FROM DATE.
015130*    /* 1980�`2079�N�̊ԂŐݒ� */
015140     IF �v�Z�@�N > 80
015150         MOVE 19 TO �v�Z�@���I
015160     ELSE
015170         MOVE 20 TO �v�Z�@���I
015180     END-IF.
015190     PERFORM �J�����g�����擾.
015200     PERFORM �a��I���N�擾.
015210     COMPUTE �v�Z�@����N�v = �v�Z�@����N - �a��I���N�v.
015220*================================================================*
015230 �J�����g�����擾 SECTION.
015240*
015250     MOVE ZEROS TO ���|����敪.
015260     READ ������}�X�^
015270     NOT INVALID KEY
015280         MOVE ���|�J�����g����         TO �J�����g�����v
015290         MOVE ���|���Z������������敪 TO ������������敪�v
015300         MOVE ���|���Z�������R����敪 TO �������R����敪�v
015310         MOVE ���|���Z�v�g���t�敪     TO ���Z�v�g���t�敪�v
015320         MOVE ���|���Z�v�g���ғ��t�敪 TO ���Z�v�g���ғ��t�敪�v
015330         MOVE ���|����R�[�h           TO ����R�[�h�v
015340         MOVE ���|�S�_�e�o�c�敪       TO �S�_�e�o�c�敪�v
015330         MOVE ���|�������Z             TO �������Z�v
015350     END-READ.
015360*
015370*** ����敪01
015380     MOVE 01 TO ���|����敪.
015390     READ ������}�X�^
015400     NOT INVALID KEY
               MOVE ���O�P�|�ϔC�ҏ��敪    TO �ϔC�ҏ��敪�v
015440     END-READ.
015450***
015460*
015470*================================================================*
015480 �a��I���N�擾 SECTION.
015490*
015500*     DISPLAY NC"�J�����g�����v"  �J�����g�����v UPON MSGBOX.
015510     MOVE �J�����g�����v TO ���|�����敪.
015520     READ �����}�X�^
015530     INVALID KEY
015540         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
015550         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015560                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015570         ACCEPT  �L�[���� FROM CONS
015580         PERFORM �I������
015590         EXIT PROGRAM
015600     NOT INVALID KEY
015610         COMPUTE �O�a��v = �J�����g�����v - 1
015620         MOVE �O�a��v TO ���|�����敪
015630         READ �����}�X�^
015640         INVALID KEY
015650             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
015660             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
015670                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015680             ACCEPT  �L�[���� FROM CONS
015690             PERFORM �I������
015700             EXIT PROGRAM
015710         NOT INVALID KEY
015720             MOVE ���|�I������N TO �a��I���N�v
015730         END-READ
015740     END-READ.
015750*
015760*================================================================*
015770 �t�@�C���I�[�v�� SECTION.
015780*
015790     OPEN INPUT   �ی��҃}�X�^
015800         MOVE NC"�ی���" TO �t�@�C����.
015810         PERFORM �I�[�v���`�F�b�N.
015820     OPEN INPUT   �����}�X�^
015830         MOVE NC"����" TO �t�@�C����.
015840         PERFORM �I�[�v���`�F�b�N.
015850     OPEN INPUT   ���̃}�X�^
015860         MOVE NC"����" TO �t�@�C����.
015870         PERFORM �I�[�v���`�F�b�N.
007560     OPEN INPUT   ���Z�v�g�e
007570         MOVE NC"���Z" TO �t�@�C����.
007580         PERFORM �I�[�v���`�F�b�N.
015910     OPEN INPUT   ������}�X�^
015920         MOVE NC"������" TO �t�@�C����.
015930         PERFORM �I�[�v���`�F�b�N.
015940     OPEN INPUT   �{�p�����}�X�^
015950         MOVE NC"�{��" TO �t�@�C����.
015960         PERFORM �I�[�v���`�F�b�N.
015160     OPEN INPUT   ����}�X�^.
015170         MOVE NC"���" TO �t�@�C����.
015180         PERFORM �I�[�v���`�F�b�N.
015970     OPEN INPUT   ������}�X�^
015980         MOVE NC"����" TO �t�@�C����.
015990         PERFORM �I�[�v���`�F�b�N.
016000     OPEN INPUT   �o�߃}�X�^
016010         MOVE NC"�o��" TO �t�@�C����.
016020         PERFORM �I�[�v���`�F�b�N.
016030     OPEN INPUT   �{�p�L�^�e.
016040         MOVE NC"�{�L�e" TO �t�@�C����.
016050         PERFORM �I�[�v���`�F�b�N.
016060     OPEN INPUT   �����f�[�^�e.
016070         MOVE NC"����" TO �t�@�C����.
016080         PERFORM �I�[�v���`�F�b�N.
016090     OPEN INPUT   ���������e.
016100         MOVE NC"��������" TO �t�@�C����.
016110         PERFORM �I�[�v���`�F�b�N.
016120     OPEN INPUT   �h�c�Ǘ��}�X�^
016130         MOVE NC"�h�c" TO �t�@�C����.
016140         PERFORM �I�[�v���`�F�b�N.
016150     OPEN INPUT �s�����}�X�^.
016160         MOVE NC"�s����" TO �t�@�C����.
016170         PERFORM �I�[�v���`�F�b�N.
016180     OPEN INPUT �����t�@�C��.
016190         MOVE NC"����" TO �t�@�C����.
016200         PERFORM �I�[�v���`�F�b�N.
005550     OPEN INPUT �ϔC�ҏ��}�X�^.
005560         MOVE NC"�ϔC" TO �t�@�C����.
005570         PERFORM �I�[�v���`�F�b�N.
016210     OPEN INPUT ��ƃt�@�C���Q.
015170         IF ( ��ԃL�[  NOT =  "00" )
015060            OPEN OUTPUT  ��ƃt�@�C���Q
                  CLOSE ��ƃt�@�C���Q
015060            OPEN INPUT  ��ƃt�@�C���Q
               END-IF.
015560     OPEN INPUT   ��f�ҏ��Q�e.
015570         MOVE NC"��f�ҏ��Q�e" TO �t�@�C����.
015580         PERFORM �I�[�v���`�F�b�N.
016240     OPEN I-O   ��f�ҏ��e.
016250         MOVE NC"���" TO �t�@�C����.
016260         PERFORM �I�[�v���`�F�b�N.
016270     OPEN I-O   ����t�@�C��
016280         PERFORM �G���[�����o.
016290*
016300*================================================================*
016310 �I�[�v���`�F�b�N SECTION.
016320*
016330     IF ��ԃL�[  NOT =  "00"
016340         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
016350         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
016360         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
016370                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
016380         ACCEPT  �L�[���� FROM CONS
016390         PERFORM �t�@�C����
016400         EXIT PROGRAM.
016410*================================================================*
016420 �A�����ڑҔ� SECTION.
016430*
016440     MOVE �A����|�{�p�a��           TO �{�p�a��v�q.
016450     MOVE �A����|�{�p�N             TO �{�p�N�v�q.
016460     MOVE �A����|�{�p��             TO �{�p���v�q.
016470     MOVE �A����|�ی����           TO �ی���ʂv�q.
016480     MOVE �A����|�ی��Ҕԍ�         TO �ی��Ҕԍ��v�q.
016490     MOVE �A����|������           TO �����ʂv�q.
016500     MOVE �A����|��p���S�Ҕԍ�     TO ��p���S�Ҕԍ��v�q.
016510     MOVE �A����|�������           TO ������ʂv�q.
016520     MOVE �A����|��p���S�Ҕԍ����� TO ��p���S�Ҕԍ������v�q.
016530     MOVE �A����|�{�l�Ƒ��敪       TO �{�l�Ƒ��敪�v�q.
016540     MOVE �A����|���҃J�i           TO ���҃J�i�v�q.
016550     MOVE �A����|���Ҕԍ�           TO ���Ҕԍ��v�q.
016560     MOVE �A����|�}��               TO �}�Ԃv�q.
016570*================================================================*
016580 ����Z�b�g SECTION.
016590*
016600     PERFORM ���ڏ�����.
014800     PERFORM �����Ǎ�.
016650     PERFORM �������擾.
016610     PERFORM �{�p�����擾.
016620     PERFORM ��������擾.
016630     PERFORM ��f�ҏ��擾.
016640     PERFORM �����f�[�^�擾.
016660     PERFORM �{�p�L�^�擾.
016670*******     PERFORM ��������擾.
016680*******     PERFORM �������ȑO�̃f�[�^����.
016690     PERFORM �������Z�����擾.
016700*     PERFORM ������擾.
016710*     PERFORM �ی����̎擾.
016720     PERFORM �ϔC�N�����擾.
           PERFORM �{�p���擾.
016730     PERFORM ���Z�v�g���я��擾.
030010**
030020     IF ��|������� NOT = ZERO
030030        PERFORM �������Z�܂Ƃߔ���
030040     ELSE
030050        MOVE SPACE TO �������Z�܂Ƃ߃t���O
030060     END-IF.
016740*-----------------------------------------------*
016800     IF ( ������������敪�v  NOT = 1 ) AND ( ���Z������������敪�v NOT = 1 )
016813        IF ( ������������敪�v = 3 OR 4 )
016815           PERFORM ������������Ώ۔��菈��
016817        ELSE
016820           PERFORM ���������擾
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
016832*
016850     IF �������R����敪�v  NOT = 1 
               MOVE �������R����敪�v TO �A�E���|�����敪
016900     END-IF.
016910*
      */���s�s�̌���{��Q/120606
           IF (��|�ی���� = 05 AND ��|������� = 53)
              IF (��|��p���S�Ҕԍ�����(1:5) = "39261" OR "43264")
022020           MOVE ALL NC"��"      TO �����
                 MOVE NC"���N�Ǘ���"  TO �^�C�g��
              END-IF
           END-IF.
016910*
016940********************
016950* ��f�ҏ��Z�b�g *
016960********************
015190     MOVE �Еۃ`�F�b�N�v     TO �Еۃ`�F�b�N.
015210     MOVE �g���`�F�b�N�v     TO �g���`�F�b�N.
015220     MOVE ���ۃ`�F�b�N�v     TO ���ۃ`�F�b�N.
           MOVE ���σ`�F�b�N�v     TO ���σ`�F�b�N.
           MOVE ���`�F�b�N�v       TO ���`�F�b�N.
           MOVE �ސE�`�F�b�N�v     TO �ސE�`�F�b�N.
           MOVE ����`�F�b�N�v     TO ����`�F�b�N.
015230     MOVE �V���`�F�b�N�v     TO �V���`�F�b�N.
015240     MOVE �W���`�F�b�N�v     TO �W���`�F�b�N.
015250     MOVE �X���`�F�b�N�v     TO �X���`�F�b�N.
015260     MOVE �P�O���`�F�b�N�v   TO �P�O���`�F�b�N.
      *
           MOVE �{�l�`�F�b�N�v     TO �{�l�`�F�b�N.
           MOVE �Ƒ��`�F�b�N�v     TO �Ƒ��`�F�b�N.
           MOVE �P�ƃ`�F�b�N�v     TO �P�ƃ`�F�b�N.
           MOVE �Q���`�F�b�N�v     TO �Q���`�F�b�N.
           MOVE ����`�F�b�N�v     TO ����`�F�b�N.
           MOVE ���V�`�F�b�N�v     TO ���V�`�F�b�N.
           MOVE �U�΃`�F�b�N�v     TO �U�΃`�F�b�N.
      *��������󎚂���/111208
017000     MOVE ������v            TO ������.
      */���s�s�d�x��Q�̏ꍇ���S�����������/120711
           IF (��|�ی���� = 05 AND ��|������� = 53) AND
              (��|��p���S�Ҕԍ�����(1:5) = "39261" OR "43264")
               MOVE ���Z�|���S���� TO ���S����
               MOVE NC"��"         TO ���S�����Œ�
017220         MOVE ��������於�̂P�v  TO �ی��Җ��̂P
017230         MOVE ��������於�̂Q�v  TO �ی��Җ��̂Q
           END-IF.
017030*
           MOVE �{�p�a��v         TO ���|�����敪
037380     READ �����}�X�^
037390     NOT INVALID KEY
037400         MOVE ���|��������   TO �{�p�a��
037410     END-READ.
017040     MOVE �{�p�N�v            TO �{�p�N.
017050     MOVE �{�p���v            TO �{�p��.
017060*
           IF ( ����L���v(1:1) = NC"��" )
              MOVE  SPACE          TO  �L���v
           END-IF.
           IF ( ����ԍ��v(1:1) = "*"  ) OR
              ( ����ԍ��v(1:2) = "��" )
              MOVE  SPACE          TO  �ԍ��v
           END-IF.
      *
           INSPECT �L���v  REPLACING ALL "�@" BY "  ".
           EVALUATE TRUE
           WHEN (�L���v NOT = SPACE) AND (�ԍ��v NOT = SPACE)
               MOVE SPACE TO �I���t���O�Q
               PERFORM VARYING �J�E���^ FROM 24 BY -1
                 UNTIL (�J�E���^ <= ZERO) OR (�I���t���O�Q NOT = SPACE)
                   IF �L���v(�J�E���^:1) NOT = SPACE
                       MOVE �L���v TO �L���ԍ��v
                       MOVE "�E"   TO �L���ԍ��v(�J�E���^ + 1:2)
                       MOVE �ԍ��v TO �L���ԍ��v(�J�E���^ + 3:40 - �J�E���^ - 2)
                       MOVE "YES"  TO �I���t���O�Q
                   END-IF
               END-PERFORM
               MOVE �L���ԍ��v TO �L���ԍ�
           WHEN �L���v NOT = SPACE
               MOVE �L���v TO �L���ԍ�
           WHEN �ԍ��v NOT = SPACE
               MOVE �ԍ��v TO �L���ԍ�
           END-EVALUATE.
017200*
017210     MOVE ����ی��Ҕԍ��v    TO �ی��Ҕԍ�.
017240*     MOVE ��ی��҃J�i�v      TO ��ی��҃J�i.
017250*     MOVE ��ی��Ҏ����v      TO ��ی��Ҏ���.
      */ �X�֔ԍ��E�d�b�ԍ��ǉ� /42505
           IF (�{�p�a��N���v�q >= 42505) AND (�A���|�d�b��� = 1)
              IF (��|�_���X�֓d�b�ԍ���� = 0 OR 2) AND
                 ((�X�֔ԍ��P�v NOT = SPACE) OR (�X�֔ԍ��Q�v NOT = SPACE))
017280           MOVE "��"          TO �X��
017260           MOVE �X�֔ԍ��P�v  TO �X�֔ԍ��P
017270           MOVE �X�֔ԍ��Q�v  TO �X�֔ԍ��Q
017280           MOVE "-"           TO �X�֋��
              END-IF
              IF ��|�_���X�֓d�b�ԍ���� = 0 OR 3
017260           MOVE �d�b�ԍ��v    TO �d�b�ԍ�
              END-IF
           END-IF.
017290*     MOVE ��ی��ҏZ���P�v    TO �Z���P.
017300*     MOVE ��ی��ҏZ���Q�v    TO �Z���Q.
017290     MOVE ���ҏZ���P�v        TO �Z���P.
017300     MOVE ���ҏZ���Q�v        TO �Z���Q.
017310*     MOVE ���҃J�i�v          TO ���҃J�i.
017320     MOVE ���Ҏ����v          TO ���Ҏ��� ��ی��Ҏ���.
017330     MOVE �j�`�F�b�N�v        TO �j�`�F�b�N.
017340     MOVE ���`�F�b�N�v        TO ���`�F�b�N.
017350     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
017360     MOVE �吳�`�F�b�N�v      TO �吳�`�F�b�N.
017370     MOVE ���a�`�F�b�N�v      TO ���a�`�F�b�N.
017380     MOVE �����`�F�b�N�v      TO �����`�F�b�N.
017390*     MOVE �����v              TO ����.
      */�����C��������/20190405
      */�ߘa���̂�5�߂��������/20190519
           MOVE "1�� 2��"      TO �����b�l�P.
           IF �ߘa�`�F�b�N�v NOT = SPACE
               MOVE "1�� 2�� 5��"  TO �����b�l�P
           END-IF.
           MOVE "3�� 4��"          TO �����b�l�Q.
023070     MOVE �ߘa�`�F�b�N�v      TO �ߘa�`�F�b�N.
017390*     MOVE �����v              TO ���Ҙa��.
      */�����C��������/20190405
017400     MOVE ���ҔN�v            TO ���ҔN.
017410     MOVE ���Ҍ��v            TO ���Ҍ�.
017420     MOVE ���ғ��v            TO ���ғ�.
017430*     MOVE ��������v          TO ����.
017440*     MOVE �����{�l�`�F�b�N�v  TO �����{�l�`�F�b�N.
017450*     MOVE �����Ƒ��`�F�b�N�v  TO �����Ƒ��`�F�b�N.
      *
           IF (��Q�|������ی��Ҏ��� = SPACE)
              CONTINUE
           ELSE
016940        MOVE ��Q�|������ی��Ҏ��� TO ��ی��Ҏ���
           END-IF.
017460* 
017470     MOVE ���������v(1)       TO ���������P.
017480     MOVE ���������v(2)       TO ���������Q.
017490     MOVE ���������v(3)       TO ���������R.
017500     MOVE ���������v(4)       TO ���������S.
017510     MOVE ���������v(5)       TO ���������T.
017510     MOVE ���������v(6)       TO ���������U.
017510     MOVE ���������v(7)       TO ���������V.
017510     MOVE ���������v(8)       TO ���������W.
017520*
      */���{���̏����͖{�̂ɕ��S�Ҕԍ��A�󋋎Ҕԍ����L�ڂ���
      *     IF �s�����ԍ��v(3:2) = "27"
      *         IF �s�����ԍ��v(1:2) NOT = "99"
      *             MOVE �s�����ԍ��v TO ����S�Ҕԍ�
      *         END-IF
      */�󋋎Ҕԍ����W�����ȏ�̏ꍇ�g�𖳎����Ĉ������/110425
      *         IF ����󋋎Ҕԍ��Q�v = SPACE
      *             MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
      *         ELSE
      *             MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
      *         END-IF
      *     END-IF.
            MOVE �s�����ԍ��v TO ����S�Ҕԍ�.
      */�󋋎Ҕԍ����W�����ȏ�̏ꍇ�g�𖳎����Ĉ������/110425
            IF ����󋋎Ҕԍ��Q�v = SPACE
                MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
            ELSE
                MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
            END-IF.
017720********************
017730* �����f�[�^�Z�b�g *
017740********************
017750* �P���� *
017760**********
017770     MOVE �������v(1)       TO �������P.
017780     MOVE �����N�v(1)       TO �����N�P.
017790     MOVE �������v(1)       TO �������P.
017800     MOVE �������v(1)       TO �������P.
017810     MOVE �����N�v(1)       TO �����N�P.
017820     MOVE �������v(1)       TO �������P.
017830     MOVE �������v(1)       TO �������P.
017840     MOVE �J�n�N�v(1)       TO �J�n�N�P.
017850     MOVE �J�n���v(1)       TO �J�n���P.
017860     MOVE �J�n���v(1)       TO �J�n���P.
017870     MOVE �I���N�v(1)       TO �I���N�P.
017880     MOVE �I�����v(1)       TO �I�����P.
017890     MOVE �I�����v(1)       TO �I�����P.
           IF �����N�v(1) NOT = ZERO
              MOVE "�"            TO �����P��؂P �����P��؂Q
           END-IF.
           IF �����N�v(1) NOT = ZERO
              MOVE "�"            TO �����P��؂R �����P��؂S
           END-IF.
           IF �J�n�N�v(1) NOT = ZERO
              MOVE "�"            TO �����P��؂T �����P��؂U
           END-IF.
           IF �I���N�v(1) NOT = ZERO
              MOVE "�"            TO �����P��؂V �����P��؂W
           END-IF.
017900     MOVE �������v(1)       TO �������P.
017910     MOVE �����`�F�b�N�v(1) TO �����`�F�b�N�P.
017920     MOVE ���~�`�F�b�N�v(1) TO ���~�`�F�b�N�P.
017930     MOVE �]��`�F�b�N�v(1) TO �]��`�F�b�N�P.
017940**********
017950* �Q���� *
017960**********
017970     MOVE �������v(2)       TO �������Q.
017980     MOVE �����N�v(2)       TO �����N�Q.
017990     MOVE �������v(2)       TO �������Q.
018000     MOVE �������v(2)       TO �������Q.
018010     MOVE �����N�v(2)       TO �����N�Q.
018020     MOVE �������v(2)       TO �������Q.
018030     MOVE �������v(2)       TO �������Q.
018040     MOVE �J�n�N�v(2)       TO �J�n�N�Q.
018050     MOVE �J�n���v(2)       TO �J�n���Q.
018060     MOVE �J�n���v(2)       TO �J�n���Q.
018070     MOVE �I���N�v(2)       TO �I���N�Q.
018080     MOVE �I�����v(2)       TO �I�����Q.
018090     MOVE �I�����v(2)       TO �I�����Q.
           IF �����N�v(2) NOT = ZERO
              MOVE "�"            TO �����Q��؂P �����Q��؂Q
           END-IF.
           IF �����N�v(2) NOT = ZERO
              MOVE "�"            TO �����Q��؂R �����Q��؂S
           END-IF.
           IF �J�n�N�v(2) NOT = ZERO
              MOVE "�"            TO �����Q��؂T �����Q��؂U
           END-IF.
           IF �I���N�v(2) NOT = ZERO
              MOVE "�"            TO �����Q��؂V �����Q��؂W
           END-IF.
018100     MOVE �������v(2)       TO �������Q.
018110     MOVE �����`�F�b�N�v(2) TO �����`�F�b�N�Q.
018120     MOVE ���~�`�F�b�N�v(2) TO ���~�`�F�b�N�Q.
018130     MOVE �]��`�F�b�N�v(2) TO �]��`�F�b�N�Q.
018140**********
018150* �R���� *
018160**********
018170     MOVE �������v(3)       TO �������R.
018180     MOVE �����N�v(3)       TO �����N�R.
018190     MOVE �������v(3)       TO �������R.
018200     MOVE �������v(3)       TO �������R.
018210     MOVE �����N�v(3)       TO �����N�R.
018220     MOVE �������v(3)       TO �������R.
018230     MOVE �������v(3)       TO �������R.
018240     MOVE �J�n�N�v(3)       TO �J�n�N�R.
018250     MOVE �J�n���v(3)       TO �J�n���R.
018260     MOVE �J�n���v(3)       TO �J�n���R.
018270     MOVE �I���N�v(3)       TO �I���N�R.
018280     MOVE �I�����v(3)       TO �I�����R.
018290     MOVE �I�����v(3)       TO �I�����R.
           IF �����N�v(3) NOT = ZERO
              MOVE "�"            TO �����R��؂P �����R��؂Q
           END-IF.
           IF �����N�v(3) NOT = ZERO
              MOVE "�"            TO �����R��؂R �����R��؂S
           END-IF.
           IF �J�n�N�v(3) NOT = ZERO
              MOVE "�"            TO �����R��؂T �����R��؂U
           END-IF.
           IF �I���N�v(3) NOT = ZERO
              MOVE "�"            TO �����R��؂V �����R��؂W
           END-IF.
018300     MOVE �������v(3)       TO �������R.
018310     MOVE �����`�F�b�N�v(3) TO �����`�F�b�N�R.
018320     MOVE ���~�`�F�b�N�v(3) TO ���~�`�F�b�N�R.
018330     MOVE �]��`�F�b�N�v(3) TO �]��`�F�b�N�R.
018340**********
018350* �S���� *
018360**********
018370     MOVE �������v(4)       TO �������S.
018380     MOVE �����N�v(4)       TO �����N�S.
018390     MOVE �������v(4)       TO �������S.
018400     MOVE �������v(4)       TO �������S.
018410     MOVE �����N�v(4)       TO �����N�S.
018420     MOVE �������v(4)       TO �������S.
018430     MOVE �������v(4)       TO �������S.
018440     MOVE �J�n�N�v(4)       TO �J�n�N�S.
018450     MOVE �J�n���v(4)       TO �J�n���S.
018460     MOVE �J�n���v(4)       TO �J�n���S.
018470     MOVE �I���N�v(4)       TO �I���N�S.
018480     MOVE �I�����v(4)       TO �I�����S.
018490     MOVE �I�����v(4)       TO �I�����S.
           IF �����N�v(4) NOT = ZERO
              MOVE "�"            TO �����S��؂P �����S��؂Q
           END-IF.
           IF �����N�v(4) NOT = ZERO
              MOVE "�"            TO �����S��؂R �����S��؂S
           END-IF.
           IF �J�n�N�v(4) NOT = ZERO
              MOVE "�"            TO �����S��؂T �����S��؂U
           END-IF.
           IF �I���N�v(4) NOT = ZERO
              MOVE "�"            TO �����S��؂V �����S��؂W
           END-IF.
018500     MOVE �������v(4)       TO �������S.
018510     MOVE �����`�F�b�N�v(4) TO �����`�F�b�N�S.
018520     MOVE ���~�`�F�b�N�v(4) TO ���~�`�F�b�N�S.
018530     MOVE �]��`�F�b�N�v(4) TO �]��`�F�b�N�S.
018540**********
018550* �T���� *
018560**********
018570     MOVE �������v(5)       TO �������T.
018580     MOVE �����N�v(5)       TO �����N�T.
018590     MOVE �������v(5)       TO �������T.
018600     MOVE �������v(5)       TO �������T.
018610     MOVE �����N�v(5)       TO �����N�T.
018620     MOVE �������v(5)       TO �������T.
018630     MOVE �������v(5)       TO �������T.
018640     MOVE �J�n�N�v(5)       TO �J�n�N�T.
018650     MOVE �J�n���v(5)       TO �J�n���T.
018660     MOVE �J�n���v(5)       TO �J�n���T.
018670     MOVE �I���N�v(5)       TO �I���N�T.
018680     MOVE �I�����v(5)       TO �I�����T.
018690     MOVE �I�����v(5)       TO �I�����T.
           IF �����N�v(5) NOT = ZERO
              MOVE "�"            TO �����T��؂P �����T��؂Q
           END-IF.
           IF �����N�v(5) NOT = ZERO
              MOVE "�"            TO �����T��؂R �����T��؂S
           END-IF.
           IF �J�n�N�v(5) NOT = ZERO
              MOVE "�"            TO �����T��؂T �����T��؂U
           END-IF.
           IF �I���N�v(5) NOT = ZERO
              MOVE "�"            TO �����T��؂V �����T��؂W
           END-IF.
018700     MOVE �������v(5)       TO �������T.
018710     MOVE �����`�F�b�N�v(5) TO �����`�F�b�N�T.
018720     MOVE ���~�`�F�b�N�v(5) TO ���~�`�F�b�N�T.
018730     MOVE �]��`�F�b�N�v(5) TO �]��`�F�b�N�T.
018740**************
018750* �o�߃Z�b�g *
018760**************
018770     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018780***             UNTIL ( ���ʂb�m�s > ���ʐ��v )
018790             UNTIL ( ���ʂb�m�s > 5 )
018800**         MOVE ���ʂb�m�s�v(���ʂb�m�s)   TO �o�ߕ��ʂb�m�s(���ʂb�m�s)
018810**         MOVE ���ʋ�؂v(���ʂb�m�s)     TO ���ʋ��(���ʂb�m�s)
018820         MOVE ����o�ߗ��̂v(���ʂb�m�s) TO �o�ߗ���(���ʂb�m�s)
018830     END-PERFORM.
018840*****************************************
018850*     �V�K�E�p���`�F�b�N�ɂ���        *
018860*   ���V�K...�����L�� ���p��...�����Ȃ� *
018870*****************************************
018880     MOVE �V�K�`�F�b�N�v    TO �V�K�`�F�b�N.
018890     MOVE �p���`�F�b�N�v    TO �p���`�F�b�N.
018900********************
018910* �����f�[�^�Z�b�g *
018920********************
018930*    ****************************************************************
018940*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
018950*    ****************************************************************
018960     MOVE �������v�q                   TO  ������.
018970     MOVE ���k���v�q                   TO  ���������k��.
019020     MOVE ���ԊO�`�F�b�N�v             TO  ���ԊO�`�F�b�N.
019030     MOVE �x���`�F�b�N�v               TO  �x���`�F�b�N.
019040     MOVE �[��`�F�b�N�v               TO  �[��`�F�b�N.
019050     MOVE �������Z���v�q               TO  �������Z��.
      *
           IF (���ԊO�`�F�b�N�v NOT = SPACE) OR (�[��`�F�b�N�v NOT = SPACE) OR
              (�x���`�F�b�N�v NOT = SPACE)
              MOVE �������Z���v                 TO  �������Z��
              MOVE �������Z��؂v               TO  �������Z���
              MOVE �������Z���v                 TO  �������Z��
           END-IF.
      *
019060     MOVE �Č����v�q                   TO  �Č���.
019070     MOVE ���Ë����v�q                 TO  ���Ë���.
019080     MOVE ���É񐔂v�q                 TO  ���É�.
019090     MOVE ���×��v�q                   TO  ���×�.
019100     MOVE ��ԃ`�F�b�N�v               TO  ��ԃ`�F�b�N.
019110     MOVE �\���J��`�F�b�N�v           TO  �\���J��`�F�b�N.
019120     MOVE ���É��Z���v�q               TO  ���É��Z��.
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
           MOVE �����񐔂v                   TO  ������.
019380     MOVE �������q���Z���v�q           TO  �������q���Z��.
           MOVE �^���񐔂v                   TO  �^����.
           MOVE �^�����v                     TO  �^����×�.
019160     MOVE �������q���Z���v�q           TO  �������q���Z��.
019170     MOVE �{�p���񋟗��v�q           TO  �{�p���񋟗�.
019180     MOVE ���v�v                       TO ���v.
019190********************
019200* ���񏈒u���Z�b�g *
019210********************
019220     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
019230***             UNTIL ( ���ʂb�m�s > ���ʐ��v )
019240             UNTIL ( ���ʂb�m�s > 5 )
019250         MOVE ���񏈒u���v�q(���ʂb�m�s) TO ���񏈒u��(���ʂb�m�s)
019260     END-PERFORM.
019270     MOVE ���񏈒u�����v�v         TO ���񏈒u�����v
019280********************
019290* �����������Z�b�g *
019300********************
019310*    **********
019320*    * �P���� *
019330*    **********
019340     MOVE ��ÒP���P�v�q             TO ��ÒP���P.
019350     MOVE ��É񐔂P�v�q             TO ��É񐔂P.
019360     MOVE ��×��P�v�q               TO ��×��P.
019370     MOVE ��㪖@�񐔂P�v�q           TO ��㪖@�񐔂P.
019380     MOVE ��㪖@���P�v�q             TO ��㪖@���P.
019390     MOVE ��㪖@�񐔂P�v�q           TO ��㪖@�񐔂P.
019400     MOVE ��㪖@���P�v�q             TO ��㪖@���P.
019410     MOVE �d�É񐔂P�v�q             TO �d�É񐔂P.
019420     MOVE �d�×��P�v�q               TO �d�×��P.
019430     MOVE ���v�P�v�q                 TO ���v�P.
019440     IF �����������P�v�q NOT = ZERO
019450         COMPUTE �����������P = �����������P�v�q / 100
019460     END-IF.
019470     MOVE ���������v�P�v�q           TO ���������v�P.
019480*    **********
019490*    * �Q���� *
019500*    **********
019510     MOVE ��ÒP���Q�v�q             TO ��ÒP���Q.
019520     MOVE ��É񐔂Q�v�q             TO ��É񐔂Q.
019530     MOVE ��×��Q�v�q               TO ��×��Q.
019540     MOVE ��㪖@�񐔂Q�v�q           TO ��㪖@�񐔂Q.
019550     MOVE ��㪖@���Q�v�q             TO ��㪖@���Q.
019560     MOVE ��㪖@�񐔂Q�v�q           TO ��㪖@�񐔂Q.
019570     MOVE ��㪖@���Q�v�q             TO ��㪖@���Q.
019580     MOVE �d�É񐔂Q�v�q             TO �d�É񐔂Q.
019590     MOVE �d�×��Q�v�q               TO �d�×��Q.
019600     MOVE ���v�Q�v�q                 TO ���v�Q.
019610     IF �����������Q�v�q NOT = ZERO
019620         COMPUTE �����������Q = �����������Q�v�q / 100
019630     END-IF.
019640     MOVE ���������v�Q�v�q           TO ���������v�Q.
019650*    ****************
019660*    * �R���ʁ^�W�� *
019670*    ****************
019680     MOVE ��ÒP���R�W�v�q             TO ��ÒP���R�W.
019690     MOVE ��É񐔂R�W�v�q             TO ��É񐔂R�W.
019700     MOVE ��×��R�W�v�q               TO ��×��R�W.
019710     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
019720     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
019730     MOVE ��㪖@�񐔂R�W�v�q           TO ��㪖@�񐔂R�W.
019740     MOVE ��㪖@���R�W�v�q             TO ��㪖@���R�W.
019750     MOVE �d�É񐔂R�W�v�q             TO �d�É񐔂R�W.
019760     MOVE �d�×��R�W�v�q               TO �d�×��R�W.
019770     MOVE ���v�R�W�v�q                 TO ���v�R�W.
019780     MOVE �����ʍ����v�R�W�v�q         TO �����ʍ����v�R�W.
019790     IF �����������R�W�v�q NOT = ZERO
019800         COMPUTE �����������R�W = �����������R�W�v�q / 100
019810     END-IF.
019820     MOVE ���������v�R�W�v�q           TO ���������v�R�W.
      */ ������ 0.7��0.6 /42505
           IF (�{�p�a��N���v�q >= 42505) AND
              (�������Z�v = ZERO)
              MOVE "60"                      TO �����R�W
              MOVE "0.6"                     TO �����ʂR�W
              MOVE "==="                     TO ���������R�W �����ʒ����R�W
           END-IF.
019830*    ****************
019840*    * �R���ʁ^10�� *
019850*    ****************
019860     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
019870     MOVE �����J�n���R�O�v�q           TO �����J�n���R�O.
019880     MOVE ��ÒP���R�O�v�q             TO ��ÒP���R�O.
019890     MOVE ��É񐔂R�O�v�q             TO ��É񐔂R�O.
019900     MOVE ��×��R�O�v�q               TO ��×��R�O.
019910     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
019920     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
019930     MOVE ��㪖@�񐔂R�O�v�q           TO ��㪖@�񐔂R�O.
019940     MOVE ��㪖@���R�O�v�q             TO ��㪖@���R�O.
019950     MOVE �d�É񐔂R�O�v�q             TO �d�É񐔂R�O.
019960     MOVE �d�×��R�O�v�q               TO �d�×��R�O.
019970     MOVE ���v�R�O�v�q                 TO ���v�R�O.
019980     IF �����������R�O�v�q NOT = ZERO
019990         COMPUTE �����������R�O = �����������R�O�v�q / 100
020000     END-IF.
020010     MOVE ���������v�R�O�v�q           TO ���������v�R�O.
020020*    ****************
020030*    * �S���ʁ^�T�� *
020040*    ****************
020050*     MOVE ��ÒP���S�T�v�q             TO ��ÒP���S�T.
020060*     MOVE ��É񐔂S�T�v�q             TO ��É񐔂S�T.
020070*     MOVE ��×��S�T�v�q               TO ��×��S�T.
020080*     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
020090*     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
020100*     MOVE ��㪖@�񐔂S�T�v�q           TO ��㪖@�񐔂S�T.
020110*     MOVE ��㪖@���S�T�v�q             TO ��㪖@���S�T.
020120*     MOVE �d�É񐔂S�T�v�q             TO �d�É񐔂S�T.
020130*     MOVE �d�×��S�T�v�q               TO �d�×��S�T.
020140*     MOVE ���v�S�T�v�q                 TO ���v�S�T.
020150*     MOVE �����ʍ����v�S�T�v�q         TO �����ʍ����v�S�T.
020160*     IF �����������S�T�v�q NOT = ZERO
020170*         COMPUTE �����������S�T = �����������S�T�v�q / 100
020180*     END-IF.
020190*     MOVE ���������v�S�T�v�q           TO ���������v�S�T.
020200*    ****************
020210*    * �S���ʁ^�W�� *
020220*    ****************
020230     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
020240     MOVE �����J�n���S�W�v�q           TO �����J�n���S�W.
020250     MOVE ��ÒP���S�W�v�q             TO ��ÒP���S�W.
020260     MOVE ��É񐔂S�W�v�q             TO ��É񐔂S�W.
020270     MOVE ��×��S�W�v�q               TO ��×��S�W.
020280     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
020290     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
020300     MOVE ��㪖@�񐔂S�W�v�q           TO ��㪖@�񐔂S�W.
020310     MOVE ��㪖@���S�W�v�q             TO ��㪖@���S�W.
020320     MOVE �d�É񐔂S�W�v�q             TO �d�É񐔂S�W.
020330     MOVE �d�×��S�W�v�q               TO �d�×��S�W.
020340     MOVE ���v�S�W�v�q                 TO ���v�S�W.
020350     MOVE �����ʍ����v�S�W�v�q         TO �����ʍ����v�S�W.
020360     IF �����������S�W�v�q NOT = ZERO
020370         COMPUTE �����������S�W = �����������S�W�v�q / 100
020380     END-IF.
020390     MOVE ���������v�S�W�v�q           TO ���������v�S�W.
      */ ������ 0.7��0.6 /42505
           IF (�{�p�a��N���v�q >= 42505) AND
              (�������Z�v = ZERO)
              MOVE "60"                      TO �����S�W
              MOVE "0.6"                     TO �����ʂS�W
              MOVE "==="                     TO ���������S�W �����ʒ����S�W
           END-IF.
020400*    ****************
020410*    * �S���ʁ^10�� *
020420*    ****************
020430     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
020440     MOVE �����J�n���S�O�v�q           TO �����J�n���S�O.
020450     MOVE ��ÒP���S�O�v�q             TO ��ÒP���S�O.
020460     MOVE ��É񐔂S�O�v�q             TO ��É񐔂S�O.
020470     MOVE ��×��S�O�v�q               TO ��×��S�O.
020480     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
020490     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
020500     MOVE ��㪖@�񐔂S�O�v�q           TO ��㪖@�񐔂S�O.
020510     MOVE ��㪖@���S�O�v�q             TO ��㪖@���S�O.
020520     MOVE �d�É񐔂S�O�v�q             TO �d�É񐔂S�O.
020530     MOVE �d�×��S�O�v�q               TO �d�×��S�O.
020540     MOVE ���v�S�O�v�q                 TO ���v�S�O.
020550     IF �����������S�O�v�q NOT = ZERO
020560         COMPUTE �����������S�O = �����������S�O�v�q / 100
020570     END-IF.
020580     MOVE ���������v�S�O�v�q           TO ���������v�S�O.
020590*
020600**��***********************************************************************
020610** �T���ʁ^2.5���̈󎚂͕K�v�Ȃ��B
020620**------------------------------------------------------------------------*
020630**    *****************
020640**    * �T���ʁ^2.5�� *
020650**    *****************
020660**     MOVE ��ÒP���T�Q�v�q             TO ��ÒP���T�Q.
020670**     MOVE ��É񐔂T�Q�v�q             TO ��É񐔂T�Q.
020680**     MOVE ��×��T�Q�v�q               TO ��×��T�Q.
020690**     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
020700**     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
020710**     MOVE ��㪖@�񐔂T�Q�v�q           TO ��㪖@�񐔂T�Q.
020720**     MOVE ��㪖@���T�Q�v�q             TO ��㪖@���T�Q.
020730**     MOVE �d�É񐔂T�Q�v�q             TO �d�É񐔂T�Q.
020740**     MOVE �d�×��T�Q�v�q               TO �d�×��T�Q.
020750**     MOVE ���v�T�Q�v�q                 TO ���v�T�Q.
020760**     MOVE �����ʍ����v�T�Q�v�q         TO �����ʍ����v�T�Q.
020770**     IF �����������T�Q�v�q NOT = ZERO
020780**         COMPUTE �����������T�Q = �����������T�Q�v�q / 100
020790**     END-IF.
020800**     MOVE ���������v�T�Q�v�q           TO ���������v�T�Q.
020810**��***********************************************************************
020820**
020830**    ****************
020840**    * �T���ʁ^�T�� *
020850**    ****************
020860**     MOVE SPACE TO ���ʂT�v.
020870**     IF ���v�T�T�v�q NOT = ZERO
020880**        MOVE "5)33 "                      TO �����Œ�T�v
020890**        MOVE "0.33"                       TO �����ʗ��T�v
020900**        MOVE �����J�n���T�T�v�q           TO �����J�n���T�v
020910**        MOVE �����J�n���T�T�v�q           TO �����J�n���T�v
020920**        MOVE ��ÒP���T�T�v�q             TO ��ÒP���T�v
020930**        MOVE ��É񐔂T�T�v�q             TO ��É񐔂T�v
020940**        MOVE ��×��T�T�v�q               TO ��×��T�v
020950**        MOVE ��㪖@�񐔂T�T�v�q           TO ��㪖@�񐔂T�v
020960**        MOVE ��㪖@���T�T�v�q             TO ��㪖@���T�v
020970**        MOVE ��㪖@�񐔂T�T�v�q           TO ��㪖@�񐔂T�v
020980**        MOVE ��㪖@���T�T�v�q             TO ��㪖@���T�v
020990**        MOVE �d�É񐔂T�T�v�q             TO �d�É񐔂T�v
021000**        MOVE �d�×��T�T�v�q               TO �d�×��T�v
021010**        MOVE ���v�T�T�v�q                 TO ���v�T�v
021020**        MOVE �����ʍ����v�T�T�v�q         TO �����ʍ����v�T�v
021030**        IF �����������T�T�v�q NOT = ZERO
021040**           COMPUTE �����������T�v = �����������T�T�v�q / 100
021050**        END-IF
021060**        MOVE ���������v�T�T�v�q           TO ���������v�T�v
021070***------------------------------------------------------------------------------------*
021080*** ����14�N6������4���ʖځE5���ʖڂ̒�������45��33�ɕύX�B
021090*** ����ɂ��A5���ʖځi���O�j�󎚂ɂ��āA����14�N6�����O�̏ꍇ�A45��ݒ肷��B
021100***
021110**        IF ( �{�p�a��N���v�q < 41406 )
021120**           MOVE "5)45 "                   TO �����Œ�T�v
021130**           MOVE "0.45"                    TO �����ʗ��T�v
021140**        END-IF
021150***------------------------------------------------------------------------------------*
021160***
021170***        MOVE ���ʂT�v                     TO ���ʂT�T
021180**     END-IF.
021190**    ****************
021200**    * �T���ʁ^�W�� *
021210**    ****************
021220*     MOVE SPACE TO ���ʂT�v.
021230*     IF ���v�T�W�v�q NOT = ZERO
021240**        MOVE "5)80 "                      TO �����Œ�T�v
021250**        MOVE "0.8 "                       TO �����ʗ��T�v
021260***/����22�N6�����A������������/100602
021270**        IF ( �{�p�a��N���v�q >= 42206 )
021280**            MOVE "5)70 "                  TO �����Œ�T�v
021290**            MOVE "0.7 "                   TO �����ʗ��T�v
021300**        END-IF
021310**        MOVE �����J�n���T�W�v�q           TO �����J�n���T�v
021320**        MOVE �����J�n���T�W�v�q           TO �����J�n���T�v
021330**        MOVE ��ÒP���T�W�v�q             TO ��ÒP���T�v
021340**        MOVE ��É񐔂T�W�v�q             TO ��É񐔂T�v
021350**        MOVE ��×��T�W�v�q               TO ��×��T�v
021360**        MOVE ��㪖@�񐔂T�W�v�q           TO ��㪖@�񐔂T�v
021370**        MOVE ��㪖@���T�W�v�q             TO ��㪖@���T�v
021380**        MOVE ��㪖@�񐔂T�W�v�q           TO ��㪖@�񐔂T�v
021390**        MOVE ��㪖@���T�W�v�q             TO ��㪖@���T�v
021400**        MOVE �d�É񐔂T�W�v�q             TO �d�É񐔂T�v
021410**        MOVE �d�×��T�W�v�q               TO �d�×��T�v
021420**        MOVE ���v�T�W�v�q                 TO ���v�T�v
021430**        MOVE �����ʍ����v�T�W�v�q         TO �����ʍ����v�T�v
021440**        IF �����������T�W�v�q NOT = ZERO
021450**           COMPUTE �����������T�v = �����������T�W�v�q / 100
021460**        END-IF
021470**        MOVE ���������v�T�W�v�q           TO ���������v�T�v
021480**        MOVE ���ʂT�v                     TO ���ʂT�W
      **/���t
021560*        MOVE �����J�n���T�W�v�q           TO �����J�n���T�v
      *        MOVE "��"                         TO ���b�l
021570*        MOVE �����J�n���T�W�v�q           TO �����J�n���T�v
      *        MOVE "��"                         TO ���b�l
      *        MOVE "("                          TO ���ʂP�v
      **/��×�
      *        IF ��×��T�W�v�q NOT = ZERO
      *            MOVE "("                      TO ���ʂQ�v
021580*            MOVE ��ÒP���T�W�v�q         TO ��ÒP���T�v
      *            MOVE "x"                      TO ��Z�L���P�v
021590*            MOVE ��É񐔂T�W�v�q         TO ��É񐔂T�v
      *            MOVE "="                      TO �C�R�[���P�v
021600*            MOVE ��×��T�W�v�q           TO ��×��T�v
      *            MOVE ")"                      TO ���ʂR�v
      *        END-IF
      **/��㪖@
      *        IF ��㪖@���T�W�v�q NOT = ZERO
      *            MOVE "+"                      TO ���Z�L���P�v
      *            MOVE "("                      TO ���ʂS�v
      *            COMPUTE ��㪖@�P���T�v        =  ��㪖@���T�W�v�q / ��㪖@�񐔂T�W�v�q
      *            MOVE "x"                      TO ��Z�L���Q�v
021610*            MOVE ��㪖@�񐔂T�W�v�q       TO ��㪖@�񐔂T�v
      *            MOVE "="                      TO �C�R�[���Q�v
021620*            MOVE ��㪖@���T�W�v�q         TO ��㪖@���T�v
      *            MOVE ")"                      TO ���ʂT�v
      *        END-IF
      **/��㪖@
      *        IF ��㪖@���T�W�v�q NOT = ZERO
      *            MOVE "+"                      TO ���Z�L���Q�v
      *            MOVE "("                      TO ���ʂU�v
      *            COMPUTE ��㪖@�P���T�v        =  ��㪖@���T�W�v�q / ��㪖@�񐔂T�W�v�q
      *            MOVE "x"                      TO ��Z�L���R�v
021630*            MOVE ��㪖@�񐔂T�W�v�q       TO ��㪖@�񐔂T�v
      *            MOVE "="                      TO �C�R�[���R�v
021640*            MOVE ��㪖@���T�W�v�q         TO ��㪖@���T�v
      *            MOVE ")"                      TO ���ʂV�v
      *        END-IF
      **/�d�×�
      *        IF �d�×��T�W�v�q NOT = ZERO
      *            MOVE "+"                      TO ���Z�L���R�v
      *            MOVE "("                      TO ���ʂW�v
      *            COMPUTE �d�ÒP���T�v          =  �d�×��T�W�v�q / �d�É񐔂T�W�v�q
      *            MOVE "x"                      TO ��Z�L���S�v
021650*            MOVE �d�É񐔂T�W�v�q         TO �d�É񐔂T�v
      *            MOVE "="                      TO �C�R�[���S�v
021660*            MOVE �d�×��T�W�v�q           TO �d�×��T�v
      *            MOVE ")"                      TO ���ʂX�v
      *        END-IF
      **
      *        MOVE ")"                          TO ���ʂP�O�v
      **/������
      *        MOVE "x"                          TO ��Z�L���T�v
      **/ ������ 0.7��0.6 /42505
      *        IF (�{�p�a��N���v�q >= 42505)
021290*           MOVE "0.6 "                    TO �����ʗ��T�v
      *        ELSE
021290*           MOVE "0.7 "                    TO �����ʗ��T�v
      *        END-IF
      **/����
021680*        IF �����������T�W�v�q NOT = ZERO
      *           MOVE "x"                       TO ��Z�L���U�v
021690*           COMPUTE �����������T�v = �����������T�W�v�q / 100
021700*        END-IF
      **/���v
      *        MOVE "="                          TO �C�R�[���T�v
021710*        MOVE ���������v�T�W�v�q           TO ���������v�T�v
021720*        MOVE ���ʂT�v                     TO ���ʂT�W
021490*     END-IF.
021500**    ****************
021510**    * �T���ʁ^10�� *
021520**    ****************
021530*     MOVE SPACE TO ���ʂT�v.
021540*     IF ���v�T�O�v�q NOT = ZERO
021550**        MOVE "5)100"                      TO �����Œ�T�v
021560**        MOVE �����J�n���T�O�v�q           TO �����J�n���T�v
021570**        MOVE �����J�n���T�O�v�q           TO �����J�n���T�v
021580**        MOVE ��ÒP���T�O�v�q             TO ��ÒP���T�v
021590**        MOVE ��É񐔂T�O�v�q             TO ��É񐔂T�v
021600**        MOVE ��×��T�O�v�q               TO ��×��T�v
021610**        MOVE ��㪖@�񐔂T�O�v�q           TO ��㪖@�񐔂T�v
021620**        MOVE ��㪖@���T�O�v�q             TO ��㪖@���T�v
021630**        MOVE ��㪖@�񐔂T�O�v�q           TO ��㪖@�񐔂T�v
021640**        MOVE ��㪖@���T�O�v�q             TO ��㪖@���T�v
021650**        MOVE �d�É񐔂T�O�v�q             TO �d�É񐔂T�v
021660**        MOVE �d�×��T�O�v�q               TO �d�×��T�v
021670**        MOVE ���v�T�O�v�q                 TO ���v�T�v
021680**        IF �����������T�O�v�q NOT = ZERO
021690**           COMPUTE �����������T�v = �����������T�O�v�q / 100
021700**        END-IF
021710**        MOVE ���������v�T�O�v�q           TO ���������v�T�v
      **/���t
021560*        MOVE �����J�n���T�O�v�q           TO �����J�n���T�v
      *        MOVE "��"                         TO ���b�l
021570*        MOVE �����J�n���T�O�v�q           TO �����J�n���T�v
      *        MOVE "��"                         TO ���b�l
      *        MOVE "("                          TO ���ʂP�v
      **/��×�
      *        IF ��×��T�O�v�q NOT = ZERO
      *            MOVE "("                      TO ���ʂQ�v
021580*            MOVE ��ÒP���T�O�v�q         TO ��ÒP���T�v
      *            MOVE "x"                      TO ��Z�L���P�v
021590*            MOVE ��É񐔂T�O�v�q         TO ��É񐔂T�v
      *            MOVE "="                      TO �C�R�[���P�v
021600*            MOVE ��×��T�O�v�q           TO ��×��T�v
      *            MOVE ")"                      TO ���ʂR�v
      *        END-IF
      **/��㪖@
      *        IF ��㪖@���T�O�v�q NOT = ZERO
      *            MOVE "+"                      TO ���Z�L���P�v
      *            MOVE "("                      TO ���ʂS�v
      *            COMPUTE ��㪖@�P���T�v        =  ��㪖@���T�O�v�q / ��㪖@�񐔂T�O�v�q
      *            MOVE "x"                      TO ��Z�L���Q�v
021610*            MOVE ��㪖@�񐔂T�O�v�q       TO ��㪖@�񐔂T�v
      *            MOVE "="                      TO �C�R�[���Q�v
021620*            MOVE ��㪖@���T�O�v�q         TO ��㪖@���T�v
      *            MOVE ")"                      TO ���ʂT�v
      *        END-IF
      **/��㪖@
      *        IF ��㪖@���T�O�v�q NOT = ZERO
      *            MOVE "+"                      TO ���Z�L���Q�v
      *            MOVE "("                      TO ���ʂU�v
      *            COMPUTE ��㪖@�P���T�v        =  ��㪖@���T�O�v�q / ��㪖@�񐔂T�O�v�q
      *            MOVE "x"                      TO ��Z�L���R�v
021630*            MOVE ��㪖@�񐔂T�O�v�q       TO ��㪖@�񐔂T�v
      *            MOVE "="                      TO �C�R�[���R�v
021640*            MOVE ��㪖@���T�O�v�q         TO ��㪖@���T�v
      *            MOVE ")"                      TO ���ʂV�v
      *        END-IF
      **/�d�×�
      *        IF �d�×��T�O�v�q NOT = ZERO
      *            MOVE "+"                      TO ���Z�L���R�v
      *            MOVE "("                      TO ���ʂW�v
      *            COMPUTE �d�ÒP���T�v          =  �d�×��T�O�v�q / �d�É񐔂T�O�v�q
      *            MOVE "x"                      TO ��Z�L���S�v
021650*            MOVE �d�É񐔂T�O�v�q         TO �d�É񐔂T�v
      *            MOVE "="                      TO �C�R�[���S�v
021660*            MOVE �d�×��T�O�v�q           TO �d�×��T�v
      *            MOVE ")"                      TO ���ʂX�v
      *        END-IF
      **
      *        MOVE ")"                          TO ���ʂP�O�v
      **/������
      **        ��Z�L���T�v �����ʗ��T�v
      **/����
021680*        IF �����������T�O�v�q NOT = ZERO
      *           MOVE "x"                       TO ��Z�L���U�v
021690*           COMPUTE �����������T�v = �����������T�O�v�q / 100
021700*        END-IF
      **/���v
      *        MOVE "="                          TO �C�R�[���T�v
021710*        MOVE ���������v�T�O�v�q           TO ���������v�T�v
021720*        MOVE ���ʂT�v                     TO ���ʂT�O
021730*     END-IF.
021740**
021750     MOVE �K�p�P�v                       TO �K�p�P.
021760     MOVE �K�p�Q�v                       TO �K�p�Q.
021760     MOVE �K�p�R�v                       TO �K�p�R.
      *
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
           IF ( �{�p�a��N���v�q >= 43006 )
              INITIALIZE �A���^�|�L�[
019550        MOVE �{�p�a��v�q TO �A���^�|�{�p�a��
019560        MOVE �{�p�N�v�q   TO �A���^�|�{�p�N
019570        MOVE �{�p���v�q   TO �A���^�|�{�p��
019580        MOVE ���Ҕԍ��v�q TO �A���^�|���Ҕԍ�
019590        MOVE �}�Ԃv�q     TO �A���^�|�}��
              MOVE ������ʂv�q TO �A���^�|�ی����
              MOVE 39           TO �A���^�|��R�[�h
              MOVE 1            TO �A���^�|�p�����
              CALL "KINUNRYO"
              CANCEL "KINUNRYO"
              MOVE �A���^�|�������q�b�l           TO �������q�b�l
              IF ( �������q���Z���v�q NOT = ZERO )
                 MOVE �������q�b�l                TO �������q
              END-IF
              PERFORM VARYING �J�E���^ FROM 1 BY 1
                        UNTIL �J�E���^ > 3
                 MOVE �A���^�|�������q��(1 �J�E���^) TO ������(�J�E���^)
                 MOVE �A���^�|�������q��(1 �J�E���^) TO ������(�J�E���^)
                 IF �A���^�|�������q��(1 �J�E���^) NOT = ZERO
                    MOVE "��"                        TO ��(�J�E���^)
                 END-IF
              END-PERFORM
              PERFORM VARYING �J�E���^ FROM 1 BY 1
                        UNTIL �J�E���^ > 5
                 MOVE �A���^�|�^����(�J�E���^)     TO �^����(�J�E���^)
              END-PERFORM
           END-IF.
      *
021770     MOVE ���Z�|���v                     TO ���v.
021370     MOVE ���Z�|�󋋎ҕ��S�z             TO �ꕔ���S��.
021380     MOVE ���Z�|�����������z             TO �������z.
           MOVE "�ꕔ���S��"                   TO �ꕔ���S���b�l.
           MOVE "�ی����t���z"                 TO �������z�b�l.
021780     MOVE ���Z�|�ꕔ���S��               TO �󋋎ҕ��S�z.
021790     MOVE ���Z�|�������z                 TO ���������z.
           MOVE "�~"          TO �ꕔ���S���~�b�l �������z�~�b�l.
021800*
022410*------------------------------------------------------------------------*
022420* �����p��̎��A�E�v���ɓ��e���L��
      *
           MOVE SPACE                     TO �����p��v.
      *     IF (���Z�|���ʌp������(1) > 5) OR (���Z�|���ʌp������(2) > 5) OR
      *        (���Z�|���ʌp������(3) > 5) OR (���Z�|���ʌp������(4) > 5) OR
      *        (���Z�|���ʌp������(5) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l
      *     END-IF.
           IF (���Z�|���ʌp������(1) >= 1) OR (���Z�|���ʌp������(2) >= 1) OR
              (���Z�|���ʌp������(3) >= 1) OR (���Z�|���ʌp������(4) >= 1) OR
              (���Z�|���ʌp������(5) >= 1)
              MOVE "�����p��Y���F"       TO �����p��b�l
           END-IF.
           MOVE SPACE                     TO �����p��b�l�Q.
      *     IF (���Z�|���ʌp������(1) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l�Q
      *     END-IF.
           IF (���Z�|���ʌp������(1) > 0)
              MOVE ���Z�|���ʌp������(1)  TO �����v
              MOVE �������v(1)            TO �������v�q(1)
              STRING �����p��b�l�Q       DELIMITED BY SPACE
                     "(1)"                DELIMITED BY SIZE
                     �������v�o(1)        DELIMITED BY "�@"
                     "�A�p������"         DELIMITED BY SIZE
                     �����v               DELIMITED BY SIZE
                     "��"                 DELIMITED BY SIZE
                INTO �����p��P�v�s
              END-STRING
           END-IF.
           MOVE SPACE                     TO �����p��b�l�Q.
      *     IF (���Z�|���ʌp������(2) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l�Q
      *     END-IF.
           IF (���Z�|���ʌp������(2) > 0)
              MOVE ���Z�|���ʌp������(2)  TO �����v
              MOVE �������v(2)            TO �������v�q(2)
              STRING �����p��b�l�Q       DELIMITED BY SPACE
                     "(2)"                DELIMITED BY SIZE
                     �������v�o(2)        DELIMITED BY "�@"
                     "�A�p������"         DELIMITED BY SIZE
                     �����v               DELIMITED BY SIZE
                     "��"                 DELIMITED BY SIZE
                INTO �����p��Q�v�s
              END-STRING
           END-IF.
           MOVE SPACE                     TO �����p��b�l�Q.
      *     IF (���Z�|���ʌp������(3) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l�Q
      *     END-IF.
           IF (���Z�|���ʌp������(3) > 0)
              MOVE ���Z�|���ʌp������(3)  TO �����v
              MOVE �������v(3)            TO �������v�q(3)
              STRING �����p��b�l�Q       DELIMITED BY SPACE
                     "(3)"                DELIMITED BY SIZE
                     �������v�o(3)        DELIMITED BY "�@"
                     "�A�p������"         DELIMITED BY SIZE
                     �����v               DELIMITED BY SIZE
                     "��"                 DELIMITED BY SIZE
                INTO �����p��R�v�s
              END-STRING
           END-IF.
           MOVE SPACE                     TO �����p��b�l�Q.
      *     IF (���Z�|���ʌp������(4) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l�Q
      *     END-IF.
           IF (���Z�|���ʌp������(4) > 0)
              MOVE ���Z�|���ʌp������(4)  TO �����v
              MOVE �������v(4)            TO �������v�q(4)
              STRING �����p��b�l�Q       DELIMITED BY SPACE
                     "(4)"                DELIMITED BY SIZE
                     �������v�o(4)        DELIMITED BY "�@"
                     "�A�p������"         DELIMITED BY SIZE
                     �����v               DELIMITED BY SIZE
                     "��"                 DELIMITED BY SIZE
                INTO �����p��S�v�s
              END-STRING
           END-IF.
           MOVE SPACE                     TO �����p��b�l�Q.
      *     IF (���Z�|���ʌp������(5) > 5)
      *        MOVE "�����p��Y���F"       TO �����p��b�l�Q
      *     END-IF.
           IF (���Z�|���ʌp������(5) > 0)
              MOVE ���Z�|���ʌp������(5)  TO �����v
              MOVE �������v(5)            TO �������v�q(5)
              STRING �����p��b�l�Q       DELIMITED BY SPACE
                     "(5)"                DELIMITED BY SIZE
                     �������v�o(5)        DELIMITED BY "�@"
                     "�A�p������"         DELIMITED BY SIZE
                     �����v               DELIMITED BY SIZE
                     "��"                 DELIMITED BY SIZE
                INTO �����p��T�v�s
              END-STRING
           END-IF.
           MOVE �����p��b�l   TO �����P�v.
           MOVE �����p��P�v�s TO �����Q�v.
           CALL �v���O�������v WITH C LINKAGE
                         USING BY REFERENCE �����P�v
                               BY REFERENCE �����Q�v.
           MOVE �����p��Q�v�s TO �����Q�v.
           CALL �v���O�������v WITH C LINKAGE
                         USING BY REFERENCE �����P�v
                               BY REFERENCE �����Q�v.
           MOVE �����p��R�v�s TO �����Q�v.
           CALL �v���O�������v WITH C LINKAGE
                         USING BY REFERENCE �����P�v
                               BY REFERENCE �����Q�v.
           MOVE �����p��S�v�s TO �����Q�v.
           CALL �v���O�������v WITH C LINKAGE
                         USING BY REFERENCE �����P�v
                               BY REFERENCE �����Q�v.
           MOVE �����p��T�v�s TO �����Q�v.
           CALL �v���O�������v WITH C LINKAGE
                         USING BY REFERENCE �����P�v
                               BY REFERENCE �����Q�v.
           MOVE �����P�v       TO �����p��.
      *
022100*------------------------------------------------------------------------------------*
022230**********************
022240* �{�p���f�[�^�Z�b�g *
022250**********************
           MOVE �s���{���i�h�r�v       TO �s���{���ԍ�.
022260*/���ώ������󎚂���/090608
022270*     IF �_���t�ԍ��Q�v NOT = SPACE
022280*         MOVE �_���t�ԍ��Q�v TO �_���t�ԍ��Q
022290*     END-IF.
022300     MOVE �_���t�ԍ��v           TO �_���t�ԍ�.
           MOVE ���ϔԍ��v             TO ���ϔԍ�.
022310*     MOVE ��z���󗝔ԍ��v       TO ��z���󗝔ԍ�.
022320     MOVE �{�p���X�֔ԍ��P�v     TO �{�p���X�֔ԍ��P.
022330     MOVE �{�p���X�֔ԍ��Q�v     TO �{�p���X�֔ԍ��Q.
022340*     MOVE �{�p���Z���v           TO �{�p���Z���P.
022350     MOVE �{�p���Z���P�v         TO �{�p���Z���P.
022360     MOVE �{�p���Z���Q�v         TO �{�p���Z���Q.
022370*     MOVE �ڍ��t�����ԍ��v     TO �ڍ��t�����ԍ�.
022380     MOVE ��\�҃J�i�v           TO ��\�҃J�i.
022390     MOVE ��\�Җ��v             TO ��\�Җ�.
022400     MOVE �{�p���d�b�ԍ��v       TO �{�p���d�b�ԍ�.
022410*
022420*     MOVE ��s���x�X���v         TO ��s���x�X��.
022430*     MOVE �a����ʃR�����g�v     TO �a�����.
022440     MOVE �����ԍ��v             TO �����ԍ�.
022450     MOVE �������`�l�J�i�P�v     TO �������`�l�J�i�P.
022450     MOVE �������`�l�J�i�Q�v     TO �������`�l�J�i�Q.
           MOVE �������`�l�J�i�R�v     TO �������`�l�J�i�R.
           IF (�������`�l�J�i�R�v = SPACE) AND (�������`�l�Q�v = SPACE)
022460         MOVE �������`�l�v       TO �������`�l
           END-IF.
           MOVE ���Z�@�֖��P�v   TO ���Z�@�֖��P.
           MOVE ���Z�@�֖��Q�v   TO ���Z�@�֖��Q.
           MOVE ���Z�@�֖��R�v   TO ���Z�@�֖��R.
           MOVE ���Z�@�֖��S�v   TO ���Z�@�֖��S.
           MOVE �x�X���P�v       TO �x�X���P.
           MOVE �x�X���Q�v       TO �x�X���Q.
           MOVE �x�X���R�v       TO �x�X���R.
           MOVE �x�X���S�v       TO �x�X���S.
      *     MOVE �U���`�F�b�N�v   TO �U���`�F�b�N.
      *     MOVE ���ʃ`�F�b�N�v   TO ���ʃ`�F�b�N.
      *     MOVE �����`�F�b�N�v   TO �����`�F�b�N.
           MOVE ��s�`�F�b�N�v   TO ��s�`�F�b�N.
           MOVE ���Ƀ`�F�b�N�v   TO ���Ƀ`�F�b�N.
           MOVE �_���`�F�b�N�v   TO �_���`�F�b�N.
           MOVE �{�X�`�F�b�N�v   TO �{�X�`�F�b�N.
           MOVE �x�X�`�F�b�N�v   TO �x�X�`�F�b�N.
           MOVE �{�x���`�F�b�N�v TO �{�x���`�F�b�N.
      */�ϔC�ҏ��
           MOVE "�܂��A�×{��̎�̂��ֈ�Ë��� �����ǒ� ��ؖΗY�i�Z���͗��O�ɋL���j�ɈϔC���܂��B"
                                 TO �ϔC�R�����g�v�T.
           MOVE �ϔC�R�����g�P�v TO ��ϔC�R�����g�P.
           MOVE �ϔC�R�����g�Q�v TO ��ϔC�R�����g�Q.
           MOVE �ϔC�R�����g�R�v TO ��ϔC�R�����g�R.
           MOVE �ϔC�R�����g�S�v TO ��ϔC�R�����g�S.
           MOVE �ϔC�R�����g�T�v TO ��ϔC�R�����g�T.
022470*
022480     MOVE �ڍ��@���v             TO �ڍ��@��.
022490*
022500* / �_���t�E���҈ϔC�� /
      */�����C��/������20190405
           MOVE �{�p�a��v         TO ���|�����敪
037380     READ �����}�X�^
037390      NOT INVALID KEY
037400         MOVE ���|��������   TO �󗝘a��
037410     END-READ.
      */�����C��/������20190405
022510     MOVE �_���t�N�v             TO �󗝔N.
022520     MOVE �_���t���v             TO �󗝌�.
022530     MOVE �_���t���v             TO �󗝓�.
022540* ( �ϔC�N���� ������邩 )
022550     IF �A���|�ϔC���  = ZERO
      */�����C��/������20190405
              MOVE �{�p�a��v         TO ���|�����敪
037380        READ �����}�X�^
037390        NOT INVALID KEY
037400            MOVE ���|��������   TO �ϔC�a��
037410        END-READ
      */�����C��/������20190405
022560         MOVE ���҈ϔC�N�v       TO �ϔC�N
022570         MOVE ���҈ϔC���v       TO �ϔC��
022580         MOVE ���҈ϔC���v       TO �ϔC��
022590     END-IF.
022600*
           PERFORM �t�b�^�Z�b�g.
022610* �{�pID
022620     MOVE ���{�p�h�c�v           TO ���{�p�h�c.
      */�����̎{�p���h�c�����͂���Ă���ꍇ�͗D�悷��/120711
           IF �s�����{�p�h�c�v NOT = SPACE
      */���s�s�̌���{��Q/120606
               IF (��|�ی���� = 05 AND ��|������� = 53) AND
                  (��|��p���S�Ҕԍ�����(1:5) = "39261" OR "43264")
022020             MOVE �s�����{�p�h�c�v TO ���{�p�h�c
                   MOVE "99"             TO ��p���S�Ҕԍ������v�q(1:2)
                   MOVE ��p���S�Ҕԍ������v�q TO ����S�Ҕԍ�
                   STRING "["                    DELIMITED BY SIZE
                          ��p���S�Ҕԍ������v�q DELIMITED BY SIZE
                          "]"                    DELIMITED BY SIZE
                     INTO �ی��Ҕԍ��Q
                   END-STRING
                   MOVE "���s�s�i�d�x��Q�V�l�j"  TO �ی��Җ���
               END-IF
           END-IF.
022630*     MOVE �s�����{�p�h�c�v       TO �s�����{�p�h�c.
022680*
022740*
022890*-------------------------------------------------------------------------*
022900*--- �� ���Z�E�v�ăZ�b�g�́A���̈���Z�b�gSECTION �̍Ō�ɂ�邱�ƁI -----*
022910     PERFORM ���Z�E�v�ăZ�b�g.
022920*-------------------------------------------------------------------------*
022770*
022780* ���ŗL�̔��l
022790*     MOVE �󋋎Ҕԍ��ҏW�v       TO ���ŗL���l.
      */�X�y�[�X�������̂łT���ʖڂ̋��z�̗��ŊJ���Ă���s���g�p�B�J���ĂȂ���Β����̂V�s��/110323
           IF �󋋎Ҕԍ��ҏW�v NOT = SPACE
               EVALUATE TRUE
               WHEN ���ʂT�O = SPACE
                   MOVE �󋋎Ҕԍ��ҏW�v TO ���ʂT�O
               WHEN ���ʂT�W = SPACE
                   MOVE �󋋎Ҕԍ��ҏW�v TO ���ʂT�W
               WHEN OTHER
                   MOVE SPACE            TO �������R���V
                   MOVE �󋋎Ҕԍ��ҏW�v TO �������R���V
               END-EVALUATE
           END-IF.
022860*
022870*****     PERFORM �e�X�g�󎚏���.
022930*
022970*-------------------------------------------------------------------------*
022980*
022990*================================================================*
023000 ���ڏ����� SECTION.
023010*
023020     INITIALIZE �{�p�����v.
023030     INITIALIZE ��f�ҏ��v.
023040     INITIALIZE �������v.
023050     INITIALIZE ���l���v.
023060     INITIALIZE �����P�v�q.
023070     INITIALIZE �����Q�v�q.
023080     INITIALIZE �����R�v�q.
023100     INITIALIZE YDT6421P.
023090     MOVE SPACE TO YDT6421P.
023110*================================================================*
023120 �������擾 SECTION.
023130*
           EVALUATE �����ʂv�q
           WHEN 05
               MOVE 2          TO ���Z�|���Z���
           WHEN OTHER
               MOVE 1          TO ���Z�|���Z���
           END-EVALUATE.
019550     MOVE �{�p�a��v�q   TO ���Z�|�{�p�a��.
019560     MOVE �{�p�N�v�q     TO ���Z�|�{�p�N.
019570     MOVE �{�p���v�q     TO ���Z�|�{�p��.
019580     MOVE ���Ҕԍ��v�q   TO ���Z�|���Ҕԍ�.
019590     MOVE �}�Ԃv�q       TO ���Z�|�}��.
019600     READ ���Z�v�g�e
019630     INVALID KEY
              MOVE SPACE     TO ���Z�|���R�[�h
              INITIALIZE        ���Z�|���R�[�h
           END-READ.
023140********************
023150* �����f�[�^�Z�b�g *
023160********************
023170*    ****************************************************************
023180*    * �����i�����j�i�������j�i�������j�ɂ��Ă͘A�����ڂ��Z�b�g *
023190*    ****************************************************************
023200     MOVE ���Z�|������                 TO �������v�q.
023210     IF ���Z�|���ԊO = 1
023220         MOVE NC"��"                   TO ���ԊO�`�F�b�N�v
023230     END-IF.
023240     IF ���Z�|�x�� = 1
023250         MOVE NC"��"                   TO �x���`�F�b�N�v
023260     END-IF.
023270     IF ���Z�|�[�� = 1
023280         MOVE NC"��"                   TO �[��`�F�b�N�v
023290     END-IF.
023300     MOVE ���Z�|���������k��           TO ���k���v�q.
023310*
023320     MOVE ���Z�|�������Z��             TO  �������Z���v�q.
023330     MOVE ���Z�|�Č���                 TO  �Č����v�q.
023340     MOVE ���Z�|���Ë���               TO  ���Ë����v�q.
023350     MOVE ���Z�|���É�               TO  ���É񐔂v�q.
023360     MOVE ���Z�|���×�                 TO  ���×��v�q.
023370     MOVE ���Z�|���É��Z��             TO  ���É��Z���v�q.
023380*
023390     IF ���Z�|��� = 1
023400         MOVE NC"��"                   TO ��ԃ`�F�b�N�v
023410     END-IF.
023420     IF ���Z�|�\���J�� = 1
023430         MOVE NC"��"                   TO �\���J��`�F�b�N�v
023440     END-IF.
023450*
023460     MOVE ���Z�|�������q���Z��         TO  �������q���Z���v�q.
023470*
      */�������q�E�^����Â̕ύX�E�ǉ�/1805
           MOVE ���Z�|�������q��            TO �����񐔂v.
           MOVE ���Z�|�^����É�            TO �^���񐔂v.
           MOVE ���Z�|�^����×�              TO �^�����v.
023570*
023580     MOVE ���Z�|�{�p���񋟗�         TO  �{�p���񋟗��v�q.
023590* ���v
022420     COMPUTE ���v�v = ���Z�|���v + ���Z�|�^����×�.
023610********************
023620* ���񏈒u���Z�b�g *
023630********************
023640     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
023650             UNTIL ( ���ʂb�m�s > ���ʐ��v )
023660         MOVE ���Z�|���񏈒u��(���ʂb�m�s) TO ���񏈒u���v�q(���ʂb�m�s)
023670     END-PERFORM.
023680     MOVE ���Z�|���񏈒u�����v         TO ���񏈒u�����v�v.
023690********************
023700* �����������Z�b�g *
023710********************
023720*    **********
023730*    * �P���� *
023740*    **********
023750     MOVE ���Z�|��ÒP���P             TO ��ÒP���P�v�q.
023760     MOVE ���Z�|��É񐔂P             TO ��É񐔂P�v�q.
023770     MOVE ���Z�|��×��P               TO ��×��P�v�q.
023780     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
023790     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
023800     MOVE ���Z�|��㪖@�񐔂P           TO ��㪖@�񐔂P�v�q.
023810     MOVE ���Z�|��㪖@���P             TO ��㪖@���P�v�q.
023820     MOVE ���Z�|�d�É񐔂P             TO �d�É񐔂P�v�q.
023830     MOVE ���Z�|�d�×��P               TO �d�×��P�v�q.
023840     MOVE ���Z�|���v�P                 TO ���v�P�v�q.
           IF ���Z�|�����p��������P NOT = ZERO
023850         MOVE ���Z�|�����p��������P   TO �����������P�v�q
           ELSE
024000         MOVE ���Z�|�����������P       TO �����������P�v�q
           END-IF.
023860     MOVE ���Z�|���������v�P           TO ���������v�P�v�q.
023870*    **********
023880*    * �Q���� *
023890*    **********
023900     MOVE ���Z�|��ÒP���Q             TO ��ÒP���Q�v�q.
023910     MOVE ���Z�|��É񐔂Q             TO ��É񐔂Q�v�q.
023920     MOVE ���Z�|��×��Q               TO ��×��Q�v�q.
023930     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
023940     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
023950     MOVE ���Z�|��㪖@�񐔂Q           TO ��㪖@�񐔂Q�v�q.
023960     MOVE ���Z�|��㪖@���Q             TO ��㪖@���Q�v�q.
023970     MOVE ���Z�|�d�É񐔂Q             TO �d�É񐔂Q�v�q.
023980     MOVE ���Z�|�d�×��Q               TO �d�×��Q�v�q.
023990     MOVE ���Z�|���v�Q                 TO ���v�Q�v�q.
           IF ���Z�|�����p��������Q NOT = ZERO
023850         MOVE ���Z�|�����p��������Q   TO �����������Q�v�q
           ELSE
024000         MOVE ���Z�|�����������Q       TO �����������Q�v�q
           END-IF.
024010     MOVE ���Z�|���������v�Q           TO ���������v�Q�v�q.
024020*    ****************
024030*    * �R���ʁ^�W�� *
024040*    ****************
024050     MOVE ���Z�|��ÒP���R�W             TO ��ÒP���R�W�v�q.
024060     MOVE ���Z�|��É񐔂R�W             TO ��É񐔂R�W�v�q.
024070     MOVE ���Z�|��×��R�W               TO ��×��R�W�v�q.
024080     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
024090     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
024100     MOVE ���Z�|��㪖@�񐔂R�W           TO ��㪖@�񐔂R�W�v�q.
024110     MOVE ���Z�|��㪖@���R�W             TO ��㪖@���R�W�v�q.
024120     MOVE ���Z�|�d�É񐔂R�W             TO �d�É񐔂R�W�v�q.
024130     MOVE ���Z�|�d�×��R�W               TO �d�×��R�W�v�q.
024140     MOVE ���Z�|���v�R�W                 TO ���v�R�W�v�q.
024150     MOVE ���Z�|�����ʍ����v�R�W         TO �����ʍ����v�R�W�v�q.
           IF ���Z�|�����p��������R�W NOT = ZERO
023850         MOVE ���Z�|�����p��������R�W   TO �����������R�W�v�q
           ELSE
024160         MOVE ���Z�|�����������R�W       TO �����������R�W�v�q
           END-IF.
024170     MOVE ���Z�|���������v�R�W           TO ���������v�R�W�v�q.
024180*    ****************
024190*    * �R���ʁ^10�� *
024200*    ****************
024210     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
024220     MOVE ���Z�|�����J�n���R�O           TO �����J�n���R�O�v�q.
024230     MOVE ���Z�|��ÒP���R�O             TO ��ÒP���R�O�v�q.
024240     MOVE ���Z�|��É񐔂R�O             TO ��É񐔂R�O�v�q.
024250     MOVE ���Z�|��×��R�O               TO ��×��R�O�v�q.
024260     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
024270     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
024280     MOVE ���Z�|��㪖@�񐔂R�O           TO ��㪖@�񐔂R�O�v�q.
024290     MOVE ���Z�|��㪖@���R�O             TO ��㪖@���R�O�v�q.
024300     MOVE ���Z�|�d�É񐔂R�O             TO �d�É񐔂R�O�v�q.
024310     MOVE ���Z�|�d�×��R�O               TO �d�×��R�O�v�q.
024320     MOVE ���Z�|���v�R�O                 TO ���v�R�O�v�q.
           IF ���Z�|�����p��������R�O NOT = ZERO
023850         MOVE ���Z�|�����p��������R�O   TO �����������R�O�v�q
           ELSE
024330         MOVE ���Z�|�����������R�O       TO �����������R�O�v�q
           END-IF.
024340     MOVE ���Z�|���������v�R�O           TO ���������v�R�O�v�q.
024350*    ****************
024360*    * �S���ʁ^�T�� *
024370*    ****************
024380     MOVE ���Z�|��ÒP���S�T             TO ��ÒP���S�T�v�q.
024390     MOVE ���Z�|��É񐔂S�T             TO ��É񐔂S�T�v�q.
024400     MOVE ���Z�|��×��S�T               TO ��×��S�T�v�q.
024410     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
024420     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
024430     MOVE ���Z�|��㪖@�񐔂S�T           TO ��㪖@�񐔂S�T�v�q.
024440     MOVE ���Z�|��㪖@���S�T             TO ��㪖@���S�T�v�q.
024450     MOVE ���Z�|�d�É񐔂S�T             TO �d�É񐔂S�T�v�q.
024460     MOVE ���Z�|�d�×��S�T               TO �d�×��S�T�v�q.
024470     MOVE ���Z�|���v�S�T                 TO ���v�S�T�v�q.
024480     MOVE ���Z�|�����ʍ����v�S�T         TO �����ʍ����v�S�T�v�q.
024490     MOVE ���Z�|�����������S�T           TO �����������S�T�v�q.
024500     MOVE ���Z�|���������v�S�T           TO ���������v�S�T�v�q.
024510*    ****************
024520*    * �S���ʁ^�W�� *
024530*    ****************
024540     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
024550     MOVE ���Z�|�����J�n���S�W           TO �����J�n���S�W�v�q.
024560     MOVE ���Z�|��ÒP���S�W             TO ��ÒP���S�W�v�q.
024570     MOVE ���Z�|��É񐔂S�W             TO ��É񐔂S�W�v�q.
024580     MOVE ���Z�|��×��S�W               TO ��×��S�W�v�q.
024590     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
024600     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
024610     MOVE ���Z�|��㪖@�񐔂S�W           TO ��㪖@�񐔂S�W�v�q.
024620     MOVE ���Z�|��㪖@���S�W             TO ��㪖@���S�W�v�q.
024630     MOVE ���Z�|�d�É񐔂S�W             TO �d�É񐔂S�W�v�q.
024640     MOVE ���Z�|�d�×��S�W               TO �d�×��S�W�v�q.
024650     MOVE ���Z�|���v�S�W                 TO ���v�S�W�v�q.
024660     MOVE ���Z�|�����ʍ����v�S�W         TO �����ʍ����v�S�W�v�q.
           IF ���Z�|�����p��������S�W NOT = ZERO
023850         MOVE ���Z�|�����p��������S�W   TO �����������S�W�v�q
           ELSE
024670         MOVE ���Z�|�����������S�W       TO �����������S�W�v�q
           END-IF.
024680     MOVE ���Z�|���������v�S�W           TO ���������v�S�W�v�q.
024690*    ****************
024700*    * �S���ʁ^10�� *
024710*    ****************
024720     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
024730     MOVE ���Z�|�����J�n���S�O           TO �����J�n���S�O�v�q.
024740     MOVE ���Z�|��ÒP���S�O             TO ��ÒP���S�O�v�q.
024750     MOVE ���Z�|��É񐔂S�O             TO ��É񐔂S�O�v�q.
024760     MOVE ���Z�|��×��S�O               TO ��×��S�O�v�q.
024770     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
024780     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
024790     MOVE ���Z�|��㪖@�񐔂S�O           TO ��㪖@�񐔂S�O�v�q.
024800     MOVE ���Z�|��㪖@���S�O             TO ��㪖@���S�O�v�q.
024810     MOVE ���Z�|�d�É񐔂S�O             TO �d�É񐔂S�O�v�q.
024820     MOVE ���Z�|�d�×��S�O               TO �d�×��S�O�v�q.
024830     MOVE ���Z�|���v�S�O                 TO ���v�S�O�v�q.
           IF ���Z�|�����p��������S�O NOT = ZERO
023850         MOVE ���Z�|�����p��������S�O   TO �����������S�O�v�q
           ELSE
024840         MOVE ���Z�|�����������S�O       TO �����������S�O�v�q
           END-IF.
024850     MOVE ���Z�|���������v�S�O           TO ���������v�S�O�v�q.
024860*    *****************
024870*    * �T���ʁ^2.5�� *
024880*    *****************
024890     MOVE ���Z�|��ÒP���T�Q             TO ��ÒP���T�Q�v�q.
024900     MOVE ���Z�|��É񐔂T�Q             TO ��É񐔂T�Q�v�q.
024910     MOVE ���Z�|��×��T�Q               TO ��×��T�Q�v�q.
024920     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
024930     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
024940     MOVE ���Z�|��㪖@�񐔂T�Q           TO ��㪖@�񐔂T�Q�v�q.
024950     MOVE ���Z�|��㪖@���T�Q             TO ��㪖@���T�Q�v�q.
024960     MOVE ���Z�|�d�É񐔂T�Q             TO �d�É񐔂T�Q�v�q.
024970     MOVE ���Z�|�d�×��T�Q               TO �d�×��T�Q�v�q.
024980     MOVE ���Z�|���v�T�Q                 TO ���v�T�Q�v�q.
024990     MOVE ���Z�|�����ʍ����v�T�Q         TO �����ʍ����v�T�Q�v�q.
025000     MOVE ���Z�|�����������T�Q           TO �����������T�Q�v�q.
025010     MOVE ���Z�|���������v�T�Q           TO ���������v�T�Q�v�q.
025020*    ****************
025030*    * �T���ʁ^�T�� *
025040*    ****************
025050     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
025060     MOVE ���Z�|�����J�n���T�T           TO �����J�n���T�T�v�q.
025070     MOVE ���Z�|��ÒP���T�T             TO ��ÒP���T�T�v�q.
025080     MOVE ���Z�|��É񐔂T�T             TO ��É񐔂T�T�v�q.
025090     MOVE ���Z�|��×��T�T               TO ��×��T�T�v�q.
025100     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
025110     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
025120     MOVE ���Z�|��㪖@�񐔂T�T           TO ��㪖@�񐔂T�T�v�q.
025130     MOVE ���Z�|��㪖@���T�T             TO ��㪖@���T�T�v�q.
025140     MOVE ���Z�|�d�É񐔂T�T             TO �d�É񐔂T�T�v�q.
025150     MOVE ���Z�|�d�×��T�T               TO �d�×��T�T�v�q.
025160     MOVE ���Z�|���v�T�T                 TO ���v�T�T�v�q.
025170     MOVE ���Z�|�����ʍ����v�T�T         TO �����ʍ����v�T�T�v�q.
025180     MOVE ���Z�|�����������T�T           TO �����������T�T�v�q.
025190     MOVE ���Z�|���������v�T�T           TO ���������v�T�T�v�q.
025200*    ****************
025210*    * �T���ʁ^�W�� *
025220*    ****************
025230     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
025240     MOVE ���Z�|�����J�n���T�W           TO �����J�n���T�W�v�q.
025250     MOVE ���Z�|��ÒP���T�W             TO ��ÒP���T�W�v�q.
025260     MOVE ���Z�|��É񐔂T�W             TO ��É񐔂T�W�v�q.
025270     MOVE ���Z�|��×��T�W               TO ��×��T�W�v�q.
025280     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
025290     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
025300     MOVE ���Z�|��㪖@�񐔂T�W           TO ��㪖@�񐔂T�W�v�q.
025310     MOVE ���Z�|��㪖@���T�W             TO ��㪖@���T�W�v�q.
025320     MOVE ���Z�|�d�É񐔂T�W             TO �d�É񐔂T�W�v�q.
025330     MOVE ���Z�|�d�×��T�W               TO �d�×��T�W�v�q.
025340     MOVE ���Z�|���v�T�W                 TO ���v�T�W�v�q.
025350     MOVE ���Z�|�����ʍ����v�T�W         TO �����ʍ����v�T�W�v�q.
           IF ���Z�|�����p��������T�W NOT = ZERO
023850         MOVE ���Z�|�����p��������T�W   TO �����������T�W�v�q
           ELSE
025360         MOVE ���Z�|�����������T�W       TO �����������T�W�v�q
           END-IF.
025370     MOVE ���Z�|���������v�T�W           TO ���������v�T�W�v�q.
025380*    ****************
025390*    * �T���ʁ^10�� *
025400*    ****************
025410     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
025420     MOVE ���Z�|�����J�n���T�O           TO �����J�n���T�O�v�q.
025430     MOVE ���Z�|��ÒP���T�O             TO ��ÒP���T�O�v�q.
025440     MOVE ���Z�|��É񐔂T�O             TO ��É񐔂T�O�v�q.
025450     MOVE ���Z�|��×��T�O               TO ��×��T�O�v�q.
025460     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
025470     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
025480     MOVE ���Z�|��㪖@�񐔂T�O           TO ��㪖@�񐔂T�O�v�q.
025490     MOVE ���Z�|��㪖@���T�O             TO ��㪖@���T�O�v�q.
025500     MOVE ���Z�|�d�É񐔂T�O             TO �d�É񐔂T�O�v�q.
025510     MOVE ���Z�|�d�×��T�O               TO �d�×��T�O�v�q.
025520     MOVE ���Z�|���v�T�O                 TO ���v�T�O�v�q.
           IF ���Z�|�����p��������T�O NOT = ZERO
023850         MOVE ���Z�|�����p��������T�O   TO �����������T�O�v�q
           ELSE
025530         MOVE ���Z�|�����������T�O       TO �����������T�O�v�q
           END-IF.
025540     MOVE ���Z�|���������v�T�O           TO ���������v�T�O�v�q.
      */2022
           MOVE ���Z�|���׏����s���Z��         TO ���׏����s���Z���v�q.
           MOVE ���Z�|���׏����s���Z��         TO ���׏����s���Z���v�q.
           IF ���Z�|���׏����s���Z�� NOT = ZERO
               STRING "���׏����s�̐����Z"     DELIMITED BY SIZE
                      ���׏����s���Z���v�q     DELIMITED BY SIZE
                      "�~ ���Z��"              DELIMITED BY SIZE
                      ���׏����s���Z���v�q     DELIMITED BY SIZE
                      "��"                     DELIMITED BY SIZE
                 INTO �K�p�R�v
               END-STRING
           END-IF.
025550*
025560*================================================================*
025570 �{�p�����擾 SECTION.
025580*
025590**************************************************
025600* �{�@�f�[�^���g�p���A�ȉ��̏����擾           *
025610* �� �_���t�ԍ�.. �_���t�ԍ��v�Ɋi�[             *
025620* �� ����ԍ� ... �ڍ��t�����ԍ��v�Ɋi�[       *
025630* �� ��\�Җ� ... ��\�Җ��v�Ɋi�[               *
025640* �� �Z��1,2   ...�{�p���Z��1,2�v�Ɋi�[          *
025650* �� �d�b�ԍ� ... �{�p���d�b�ԍ��v�Ɋi�[         *
025660**************************************************
025670     MOVE ZERO  TO �{��|�{�p���ԍ�.
025680     READ �{�p�����}�X�^
025690     INVALID KEY
025700         CONTINUE
025710     NOT INVALID KEY
025720*
               MOVE �{��|�s���{���i�h�r TO �s���{���i�h�r�v
025760         MOVE �{��|�V�_���t�ԍ�   TO �_���t�ԍ��v
025780*
025790*** ���ρE���q���̎��̂݁A�_���t�ԍ��̕ҏW������B
025800         EVALUATE �ی���ʂv�q
025810         WHEN 04
025820             PERFORM ���ϔԍ��Z�b�g
025830         WHEN 09
025840             PERFORM ���q���ԍ��Z�b�g
025850         END-EVALUATE
025860***
025940         MOVE �{��|�X�֔ԍ��P        TO �{�p���X�֔ԍ��P�v
025950         MOVE �{��|�X�֔ԍ��Q        TO �{�p���X�֔ԍ��Q�v
025960         MOVE �{��|��\�҃J�i        TO ��\�҃J�i�v
025970         MOVE �{��|��\�Җ�          TO ��\�Җ��v
025980*
025990         MOVE �{��|�ڍ��@��          TO �ڍ��@���v
026000*
026050         MOVE �{��|�Z���P            TO �{�p���Z���P�v
026060         MOVE �{��|�Z���Q            TO �{�p���Z���Q�v
026070*
026080         MOVE �{��|�d�b�ԍ�          TO �{�p���d�b�ԍ��v
026090** �U������
026100         MOVE �{��|������s��      TO ������s���v
026110         MOVE �{��|������s�x�X��  TO ������s�x�X���v
026120         MOVE �{��|�a�����          TO �a����ʂv
026130         MOVE �{��|�����ԍ�          TO �����ԍ��v
026140         MOVE �{��|�������`�l        TO �������`�l�v
026150         MOVE �{��|�������`�l�J�i    TO �������`�l�J�i�v
026160         STRING ������s���v     DELIMITED BY SPACE
026170                " "                DELIMITED BY SIZE
026180                ������s�x�X���v DELIMITED BY SPACE
026190                INTO ��s���x�X���v
026200         END-STRING
026210         EVALUATE �a����ʂv
026220         WHEN 1
026230             MOVE NC"����" TO �a����ʃR�����g�v
026240         WHEN 2
026250             MOVE NC"����" TO �a����ʃR�����g�v
026260         WHEN OTHER
026270             MOVE SPACE    TO �a����ʃR�����g�v
026280         END-EVALUATE
026290*
026300     END-READ.
026310*
026320*-------------------------------------------------------------------------*
026330*  �g����03, ���ρE���q����04 �͉�̌������g�p
      */ �S�ĉ�̌������g�p /140728
026350*-------------------------------------------------------------------------*
026360*     IF �ی���ʂv�q = 03 OR 04 OR 09
              MOVE ZERO  TO ���|�_���I���敪
              MOVE 39    TO ���|����R�[�h
              MOVE ZERO  TO ���|�ی����
              MOVE ZERO  TO ���|�ύX�a��N��
026480        READ ����}�X�^
026490        NOT INVALID KEY
026500            IF ( ���|������s�� NOT = SPACE ) AND
026510               ( ���|�����ԍ�     NOT = SPACE )
026520*           / �U������̍ăZ�b�g /
026530                MOVE ���|������s��      TO ������s���v
026540                MOVE ���|������s�x�X��  TO ������s�x�X���v
026550                MOVE ���|�a�����          TO �a����ʂv
026560                MOVE ���|�����ԍ�          TO �����ԍ��v
026570                MOVE ���|�������`�l        TO �������`�l�v
026580                MOVE ���|�������`�l�J�i    TO �������`�l�J�i�v
026590                MOVE SPACE TO ��s���x�X���v
026600                STRING ������s���v     DELIMITED BY SPACE
026610                       " "                DELIMITED BY SIZE
026620                       ������s�x�X���v DELIMITED BY SPACE
026630                       INTO ��s���x�X���v
026640                END-STRING
026650                EVALUATE �a����ʂv
026660                WHEN 1
026670                    MOVE NC"����" TO �a����ʃR�����g�v
026680                WHEN 2
026690                    MOVE NC"����" TO �a����ʃR�����g�v
026700                WHEN OTHER
026710                    MOVE SPACE    TO �a����ʃR�����g�v
026720                END-EVALUATE
026730            END-IF
026740        END-READ.
026750*     END-IF.
      */����͐U���̂ݑΉ�
           MOVE NC"��" TO �U���`�F�b�N�v.
      *
           EVALUATE �a����ʂv
           WHEN 1
               MOVE NC"��" TO ���ʃ`�F�b�N�v
           WHEN 2
               MOVE NC"��" TO �����`�F�b�N�v
           END-EVALUATE.
      *
009745     IF ������s���v NOT = SPACE
009746        PERFORM VARYING �J�E���^ FROM 40 BY -1
009747                  UNTIL (������s���v(�J�E���^:1) NOT = SPACE) OR
009748                        (�J�E���^ <= ZERO)
009749            CONTINUE
009750        END-PERFORM
009751        IF �J�E���^ > 4
009752           IF ������s���v(�J�E���^ - 3 : 4)  = "��s"
009753              MOVE  ������s���v(1:�J�E���^ - 4)   TO ���Z�@�֖��v
009754              MOVE NC"��" TO ��s�`�F�b�N�v
009755           ELSE
009756              IF ������s���v(�J�E���^ - 3 : 4)  = "����"
009757                 MOVE  ������s���v(1:�J�E���^ - 4)   TO ���Z�@�֖��v
009758                 MOVE NC"��" TO ���Ƀ`�F�b�N�v
009759              ELSE
009760                 IF ������s���v(�J�E���^ - 3 : 4)  = "�_��"
009761                    MOVE  ������s���v(1:�J�E���^ - 4)   TO ���Z�@�֖��v
009762                    MOVE NC"��" TO �_���`�F�b�N�v
009763                 ELSE
009764                    MOVE  ������s���v  TO ���Z�@�֖��v
009765                 END-IF
009766              END-IF
009767           END-IF
009768        ELSE
009769           MOVE  ������s���v  TO ���Z�@�֖��v
009770        END-IF
009771     END-IF.
009779*
009780     IF ������s�x�X���v NOT = SPACE
009781        PERFORM VARYING �J�E���^ FROM 40 BY -1
009782                  UNTIL (������s�x�X���v(�J�E���^:1) NOT = SPACE) OR
009783                        (�J�E���^ <= ZERO)
009784            CONTINUE
009785        END-PERFORM
009786        IF �J�E���^ >= 4
009787           IF ������s�x�X���v(�J�E���^ - 3 : 4)  = "�{�X"
009788              MOVE  ������s�x�X���v(1:�J�E���^ - 4)   TO �x�X���v
009789              MOVE NC"��" TO �{�X�`�F�b�N�v
009790           ELSE
009791              IF ������s�x�X���v(�J�E���^ - 3 : 4)  = "�x�X"
009792                 MOVE  ������s�x�X���v(1:�J�E���^ - 4)   TO �x�X���v
009793                 MOVE NC"��" TO �x�X�`�F�b�N�v
009794              ELSE
009791                 IF ������s�x�X���v(�J�E���^ - 3 : 4)  = "�x��"
009792                    MOVE  ������s�x�X���v(1:�J�E���^ - 4)   TO �x�X���v
009793                    MOVE NC"��" TO �{�x���`�F�b�N�v
009794                 ELSE
009791                     IF ������s�x�X���v(�J�E���^ - 3 : 4)  = "�{��"
009792                        MOVE  ������s�x�X���v(1:�J�E���^ - 4)   TO �x�X���v
009793                        MOVE NC"��" TO �{�x���`�F�b�N�v
009794                     ELSE
009800                         MOVE  ������s�x�X���v  TO �x�X���v
009801                     END-IF
009804                 END-IF
009805              END-IF
009806           END-IF
009807        ELSE
009808           MOVE  ������s�x�X���v  TO �x�X���v
009809        END-IF
009809     END-IF.
      *
026760*
026770*********************************************
026780** �h�c�Ǘ��}�X�^���@���{�p�h�c���擾����B
026790*********************************************
026800     EVALUATE �ی���ʂv�q 
026810* ����
026820         WHEN 01
026830            MOVE �ی��Ҕԍ��v�q       TO �ی��Ҕԍ���r�v
026840* �ސE
026850         WHEN 08
026860* �������
026870         WHEN 05
026880            MOVE �ی��Ҕԍ��v�q(3:6)  TO �ی��Ҕԍ���r�v
026890     END-EVALUATE.
026900**   / ���{�pID /
026910     MOVE 01                     TO �h�c�ǁ|�h�c�敪.
026920     MOVE ZERO                   TO �h�c�ǁ|�{�p���ԍ�.
026930     MOVE �ی��Ҕԍ���r�v(1:2)  TO �h�c�ǁ|�ی����.
026940     MOVE SPACE                  TO �h�c�ǁ|�ی��Ҕԍ�.
026950     READ �h�c�Ǘ��}�X�^
026960     NOT INVALID KEY
026970         MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO ���{�p�h�c�v
026980     END-READ.
026990*
027000**   / �s�����{�pID /
027010     MOVE 02                     TO �h�c�ǁ|�h�c�敪.
027020     MOVE ZERO                   TO �h�c�ǁ|�{�p���ԍ�.
027030     MOVE �ی���ʂv�q           TO �h�c�ǁ|�ی����.
027040     MOVE �ی��Ҕԍ��v�q         TO �h�c�ǁ|�ی��Ҕԍ�.
      */���s�s�̏d�x��Q/120711
           IF ��p���S�Ҕԍ������v�q(1:5) = "39261" OR "43264"
026910        MOVE 01                     TO �h�c�ǁ|�h�c�敪
026920        MOVE ZERO                   TO �h�c�ǁ|�{�p���ԍ�
026930        MOVE 50                     TO �h�c�ǁ|�ی����
026940        MOVE SPACE                  TO �h�c�ǁ|�ی��Ҕԍ�
           END-IF.
      *
027050     READ �h�c�Ǘ��}�X�^
027060     NOT INVALID KEY
027070          MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO �s�����{�p�h�c�v
027080     END-READ.
027090*
027100*================================================================*
027110 ���ϔԍ��Z�b�g SECTION.
027120*
027130**************************************************************
027140* �ی��Ҕԍ��ɂ��A���ς̔ԍ����󎚂��邩�A�_���t�ԍ�������
027150**************************************************************
027160** 1.���ϑg���A��
027170     MOVE SPACE  TO  �E�o�t���O.
027180     IF �{��|���ϘA�ԍ� NOT = ZERO
027190** ����(�ی��Ҕԍ�)
027200        IF ( �ی��Ҕԍ��v�q(1:2) = "31" )  OR
027210           ( �ی��Ҕԍ��v�q = "34130021" )
027220*
027230           MOVE  NC"���ϑg���A����"   TO ���ϘA�ԍ����m�v 
027240           MOVE  NC"��"               TO ���ϘA�ԍ��P�ʂm�v 
027250           MOVE  �{��|���ϘA�ԍ�     TO ���ϘA�ԍ��v
027260           IF    (���ϘA�ԍ��v(1:1) = "0")  AND (�E�o�t���O  = SPACE )
027270                 MOVE SPACE TO  ���ϘA�ԍ��v(1:1)
027280           ELSE
027290                 MOVE "YES" TO  �E�o�t���O
027300           END-IF
027310           IF    (���ϘA�ԍ��v(2:1) = "0")  AND (�E�o�t���O  = SPACE )
027320                 MOVE SPACE TO  ���ϘA�ԍ��v(2:1)
027330           ELSE
027340                 MOVE "YES" TO  �E�o�t���O
027350           END-IF
027360           IF    (���ϘA�ԍ��v(3:1) = "0")  AND (�E�o�t���O  = SPACE )
027370                 MOVE SPACE TO  ���ϘA�ԍ��v(3:1)
027380           ELSE
027390                 MOVE "YES" TO  �E�o�t���O
027400           END-IF
027410           IF    (���ϘA�ԍ��v(4:1) = "0")  AND (�E�o�t���O  = SPACE )
027420                 MOVE SPACE TO  ���ϘA�ԍ��v(4:1)
027430           ELSE
027440                 MOVE "YES" TO  �E�o�t���O
027450           END-IF
027460           IF    (���ϘA�ԍ��v(5:1) = "0")  AND (�E�o�t���O  = SPACE )
027470                 MOVE SPACE TO  ���ϘA�ԍ��v(5:1)
027480           ELSE
027490                 MOVE "YES" TO  �E�o�t���O
027500           END-IF
027510           IF    (���ϘA�ԍ��v(6:1) = "0")  AND (�E�o�t���O  = SPACE )
027520                 MOVE SPACE TO  ���ϘA�ԍ��v(6:1)
027530           ELSE
027540                 MOVE "YES" TO  �E�o�t���O
027550           END-IF
027560**/���ώ������󎚂���/090608
027570*           MOVE  �_���t�ԍ��v         TO �_���t�ԍ��Q�v
027580*           MOVE  ���ϘA�ԍ��W�c�v     TO �_���t�ԍ��v
024110            MOVE  ���ϘA�ԍ��W�c�v     TO ���ϔԍ��v
027590        END-IF
027600     END-IF.
027610*
027620** 2. �n���ϋ��c��
027630     MOVE SPACE  TO  �E�o�t���O.
027640     IF �{��|�n���ϘA�ԍ� NOT = ZERO
027650** ����(�ی��Ҕԍ�)
027660        IF ( �ی��Ҕԍ��v�q(1:2) = "32" OR "33" OR "34" )  AND
027670           ( �ی��Ҕԍ��v�q NOT = "34130021" )
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
028020**/���ώ������󎚂���/090608
028030*           MOVE  �_���t�ԍ��v         TO �_���t�ԍ��Q�v
028040*           MOVE  ���ϘA�ԍ��W�c�v     TO �_���t�ԍ��v
024110            MOVE  ���ϘA�ԍ��W�c�v     TO ���ϔԍ��v
028050        END-IF
028060     END-IF.
028070*
028080*================================================================*
028090 ���q���ԍ��Z�b�g SECTION.
028100*
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
028500*           MOVE  ���q���ԍ��W�c�v     TO �_���t�ԍ��v
028500         MOVE  ���q���ԍ��W�c�v     TO ���ϔԍ��v
028510     END-IF.
028520*
028530*================================================================*
028540 ��f�ҏ��擾 SECTION.
028550*
028560**************************************************
028570* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
028580* �� �{�p�N ..... �{�p�N�v�Ɋi�[                 *
028590* �� �{�p�� ..... �{�p���v�Ɋi�[                 *
028600* �� ���Ҕԍ�.... ���Ҕԍ��v�Ɋi�[���e�c�A�ԗp   *
028610* �� �L�� ....... �L���v�Ɋi�[                   *
028620* �� �ԍ� ....... �ԍ��v�Ɋi�[                   *
028630* �� �ی��Ҕԍ� . �ی��Ҕԍ��v�Ɋi�[             *
028640* �� �ی���� ... �ی���ʂv�Ɋi�[               *
028650* �� ��ی��҃J�i.��ی��҃J�i�v�Ɋi�[           *
028660* �� ��ی��Ҏ���.��ی��Ҏ����v�Ɋi�[           *
028670* �� �Z���P ......��ی��ҏZ���P�v�Ɋi�[         *
028680* �� �Z���Q ......��ی��ҏZ���Q�v�Ɋi�[         *
028690* �� ���҃J�i ....���҃J�i�v�Ɋi�[               *
028700* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
028710* �� ���Ґ��� ....�敪�ɂ��`�F�b�N��"��"���i�[ *
028720* �� ���Ҙa�� ....�a��ɂ��`�F�b�N��"��"���i�[ *
028730* �� ���ҔN ......���ҔN�v�Ɋi�[                 *
028740* �� ���Ҍ� ......���Ҍ��v�Ɋi�[                 *
028750* �� ���ғ� ......���ғ��v�Ɋi�[                 *
028760* �� ���� ........���̃}�X�^��葱���v�Ɏ擾     *
028770**************************************************
028780     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
028790     MOVE �{�p�N�v�q         TO ��|�{�p�N.
028800     MOVE �{�p���v�q         TO ��|�{�p��.
028810     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
028820     READ ��f�ҏ��e
028830     INVALID KEY
028840         CONTINUE
028850*            /* ���肦�Ȃ� */
028860     NOT INVALID KEY
022660         EVALUATE ��|�ی����
022670         WHEN 01
022690            MOVE NC"��"        TO ���ۃ`�F�b�N�v
022700         WHEN 02
022710         WHEN 06
022750         WHEN 07
022720            MOVE NC"��"        TO �Еۃ`�F�b�N�v
022730         WHEN 03
022740            MOVE NC"��"        TO �g���`�F�b�N�v
022750*         WHEN 07
022760*            MOVE NC"��"        TO �D���`�F�b�N�v
               WHEN 04
                  MOVE NC"��"        TO ���σ`�F�b�N�v
               WHEN 09
                  MOVE NC"��"        TO ���`�F�b�N�v
               WHEN 08
                  MOVE NC"��"        TO �ސE�`�F�b�N�v
               WHEN 05
                  MOVE NC"��"        TO ����`�F�b�N�v
022770         END-EVALUATE
      *
               IF ��|������� = ZERO
                   MOVE NC"��" TO �P�ƃ`�F�b�N�v
               ELSE
                   MOVE NC"��" TO �Q���`�F�b�N�v
               END-IF
      */�{�Ƌ敪�͂ǂꂩ�P�Ɂ�������B
               IF ��|�ی���� = 05
                   EVALUATE ��|���ʋ敪
                   WHEN 1
                   WHEN 2
                       MOVE NC"��" TO ����`�F�b�N�v
                   WHEN 3
                       MOVE NC"��" TO ���V�`�F�b�N�v
                   END-EVALUATE
               ELSE
028984             EVALUATE ��|���ʋ敪
                   WHEN 1
                   WHEN 2
                       MOVE NC"��" TO ����`�F�b�N�v
                   WHEN 3
                       MOVE NC"��" TO ���V�`�F�b�N�v
028991             WHEN 6
                       MOVE NC"��" TO �U�΃`�F�b�N�v
                   WHEN OTHER
                       IF ��|�{�l�Ƒ��敪 = 1
                           MOVE NC"��" TO �{�l�`�F�b�N�v
                       ELSE
                           MOVE NC"��" TO �Ƒ��`�F�b�N�v
                       END-IF
028999             END-EVALUATE
               END-IF
               EVALUATE ���Z�|���t����
               WHEN 10
                   MOVE NC"��" TO �P�O���`�F�b�N�v
               WHEN 9
                   MOVE NC"��" TO �X���`�F�b�N�v
      **/�_�ސ�14�A�{��04�̏ꍇ�A�O������҂P���́A���t�������W���ɂ���B(�����P�����S���邽�߁A���҂P���A�ی��҂W���A���P���ƂȂ�)
      *             IF (��|�ی����     = 01 AND ��|�ی��Ҕԍ�(1:2) = "14" OR "04") OR
      *                (��|�ی���� NOT = 01 AND ��|�ی��Ҕԍ�(3:2) = "14" OR "04")
      */�_�ސ�14�A�{��04�A���Q38�A�R��35�A����07�A����18�̏ꍇ�A�O������҂P���́A���t�������W���ɂ���B(�����P�����S���邽�߁A���҂P���A�ی��҂W���A���P���ƂȂ�)/130109�R���ǉ�/130319�����ǉ�/130401����ǉ�
                   IF ((��|�ی����     = 01) AND (��|�ی��Ҕԍ�(1:2) = "14" OR "04" OR "38" OR "35" OR "07" OR "18")) OR
                      ((��|�ی���� NOT = 01) AND (��|�ی��Ҕԍ�(3:2) = "14" OR "04" OR "38" OR "35" OR "07" OR "18"))
                       IF (��|�ی���� NOT = 05 ) AND (��|���ʋ敪 = 1)
                           MOVE SPACE  TO �X���`�F�b�N�v
                           MOVE NC"��" TO �W���`�F�b�N�v
                       END-IF
                   END-IF
               WHEN 8
                   MOVE NC"��" TO �W���`�F�b�N�v
               WHEN 7
                   MOVE NC"��" TO �V���`�F�b�N�v
               END-EVALUATE
028870         MOVE ��|�{�p�a��     TO �{�p�a��v
028870         MOVE ��|�{�p�N       TO �{�p�N�v
028880         MOVE ��|�{�p��       TO �{�p���v
028890         MOVE ��|���Ҕԍ�     TO ���Ҕԍ��v
028900*         MOVE ��|�L��         TO �L���v
028910*         MOVE ��|�ԍ�         TO �ԍ��v
      *-----------------------------------------------------------------*
               MOVE SPACE TO �A�Í������|�Í����
      *
      *        / �A�Í������|���͏��Z�b�g /
               MOVE ��|�L��       TO �A�Í������|�L��
               MOVE ��|�ԍ�       TO �A�Í������|�ԍ�
               MOVE ��|�Í������� TO �A�Í������|�Í�������
      *
               CALL   �����v���O�������v
               CANCEL �����v���O�������v
      *
               MOVE �A�Í������|���������L�� TO �L���v
               MOVE �A�Í������|���������ԍ� TO �ԍ��v
      *
      *-----------------------------------------------------------------*
028920         MOVE ��|�ی��Ҕԍ�   TO �ی��Ҕԍ��v
028930         MOVE ��|�ی����     TO �ی���ʂv
028940** �S���y�؂̎}�ԍ폜
028950         IF ( ��|�ی���� = 01 ) AND ( ��|�ی��Ҕԍ�(1:6) = "133033" )
028960            MOVE ��|�ی��Ҕԍ�(1:6)  TO �ی��Ҕԍ��v
028970         END-IF
028980**
028990         MOVE ��|��p���S�Ҕԍ����� TO �s�����ԍ��v
027240         MOVE ��|��v�Ҕԍ�����     TO �󋋎Ҕԍ��v
029000         MOVE ��|��ی��҃J�i TO ��ی��҃J�i�v
029010         MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ����v
029020*         MOVE ��|�X�֔ԍ��P   TO �X�֔ԍ��P�v
029030*         MOVE ��|�X�֔ԍ��Q   TO �X�֔ԍ��Q�v
029040         MOVE ��|�Z���P       TO ��ی��ҏZ���P�v
029050         MOVE ��|�Z���Q       TO ��ی��ҏZ���Q�v
029020         MOVE ��|���җX�֔ԍ��P   TO �X�֔ԍ��P�v
029030         MOVE ��|���җX�֔ԍ��Q   TO �X�֔ԍ��Q�v
029040         MOVE ��|���ҏZ���P       TO ���ҏZ���P�v
029050         MOVE ��|���ҏZ���Q       TO ���ҏZ���Q�v
      */ �d�b�ԍ��ǉ� /42505
      *         IF ��|�d�b�ԍ� NOT = SPACE
      *            STRING "�d�b:"        DELIMITED BY SIZE
      *                   ��|�d�b�ԍ�   DELIMITED BY SPACE
      *              INTO �d�b�ԍ��v
      *            END-STRING
      *         ELSE
                  IF ��|���ғd�b�ԍ� NOT = SPACE
                     STRING "�d�b:"            DELIMITED BY SIZE
                            ��|���ғd�b�ԍ�   DELIMITED BY SPACE
                       INTO �d�b�ԍ��v
                     END-STRING
                  END-IF
      *         END-IF
029060         MOVE ��|���҃J�i     TO ���҃J�i�v
029070         MOVE ��|���Ҏ���     TO ���Ҏ����v
029080         EVALUATE ��|���Ґ���
029090         WHEN 1
029100             MOVE NC"��"  TO �j�`�F�b�N�v
029110         WHEN 2
029120             MOVE NC"��"  TO ���`�F�b�N�v
029130         END-EVALUATE
029140         EVALUATE ��|���Ҙa��
029150         WHEN 1
029160             MOVE NC"��"  TO �����`�F�b�N�v
029170         WHEN 2
029180             MOVE NC"��"  TO �吳�`�F�b�N�v
029190         WHEN 3
029200             MOVE NC"��"  TO ���a�`�F�b�N�v
029210         WHEN 4
029220             MOVE NC"��"  TO �����`�F�b�N�v
      */�����C��/20190405
023060         WHEN 5
                   MOVE "5��"   TO �ߘa�b�l�v
023070             MOVE NC"��"  TO �ߘa�`�F�b�N�v
029230         END-EVALUATE
029240         EVALUATE ��|���Ҙa��
029250         WHEN 1
029260             MOVE NC"����"  TO �����v
029270         WHEN 2
029280             MOVE NC"�吳"  TO �����v
029290         WHEN 3
029300             MOVE NC"���a"  TO �����v
029310         WHEN 4
029320             MOVE NC"����"  TO �����v
029330         END-EVALUATE
029340*
029350         MOVE ��|���ҔN  TO ���ҔN�v
029360         MOVE ��|���Ҍ�  TO ���Ҍ��v
029370         MOVE ��|���ғ�  TO ���ғ��v
029380* ����
029390         EVALUATE �ی���ʂv�q 
029400* ���q���͖�������"�{�l"
029410         WHEN  09
029420              MOVE NC"�{�l"    TO �����v
029430              MOVE NC"��"      TO �����{�l�`�F�b�N�v
029440* �ސE
029450         WHEN  08
029460             IF (�{�l�Ƒ��敪�v�q = 1 ) AND (��|���ю呱�� = 1)
029470                 MOVE NC"���ю�"  TO �����v
029480                 MOVE NC"��"      TO �����{�l�`�F�b�N�v
029490             ELSE
029500*                / ���������� /
029510                 IF ��|�ی��Ҕԍ�(3:2) = "36"
029520                    IF �{�l�Ƒ��敪�v�q = 1
029530                       MOVE NC"��"   TO �����{�l�`�F�b�N�v
029540                    ELSE
029550                       MOVE NC"��"   TO �����Ƒ��`�F�b�N�v
029560                    END-IF
029570                 ELSE
029580                    MOVE 05          TO ���|�敪�R�[�h
029590                    MOVE ��|����    TO ���|���̃R�[�h
029600                    READ ���̃}�X�^
029610                    INVALID KEY
029620                        MOVE SPACE    TO �����v
029630                    NOT INVALID KEY
029640                        MOVE ���|���� TO �����v
029650                    END-READ
029660                    MOVE NC"��"       TO �����Ƒ��`�F�b�N�v
029670                END-IF
029680             END-IF
029690* ����
029700         WHEN 01
029710             IF �{�l�Ƒ��敪�v�q = 1
029720                 MOVE NC"���ю�"  TO �����v
029730                 MOVE NC"��"      TO �����{�l�`�F�b�N�v
029740             ELSE
029750                 MOVE 05          TO ���|�敪�R�[�h
029760                 MOVE ��|����    TO ���|���̃R�[�h
029770                 READ ���̃}�X�^
029780                 INVALID KEY
029790                     MOVE SPACE    TO �����v
029800                 NOT INVALID KEY
029810                     MOVE ���|���� TO �����v
029820                 END-READ
029830                 MOVE NC"��"       TO �����Ƒ��`�F�b�N�v
029840             END-IF
029850         WHEN OTHER
029860             IF �{�l�Ƒ��敪�v�q = 1
029870                 MOVE NC"�{�l"    TO �����v
029880                 MOVE NC"��"      TO �����{�l�`�F�b�N�v
029890             ELSE
029900                 MOVE 05          TO ���|�敪�R�[�h
029910                 MOVE ��|����    TO ���|���̃R�[�h
029920                 READ ���̃}�X�^
029930                 INVALID KEY
029940                     MOVE SPACE    TO �����v
029950                 NOT INVALID KEY
029960                     MOVE ���|���� TO �����v
029970                 END-READ
029980                 MOVE NC"��"       TO �����Ƒ��`�F�b�N�v
029990             END-IF
030000         END-EVALUATE
030070**
030080* 14/10�`�@���ʋ敪�R�����g��
030090         IF ��|�{�p�a��N�� >= 41410
030100             IF ��|������ = ZERO
030110                EVALUATE ��|���ʋ敪
030120                WHEN 1
030130                   MOVE "70�Έȏ� 1��"  TO ���ʃR�����g�v
030140                WHEN 2
030150                   MOVE "70�Έȏ� 2��"  TO ���ʃR�����g�v
030160                WHEN 3
030170                   MOVE "70�Έȏ� 3��"  TO ���ʃR�����g�v
030180                WHEN 6
030190                   IF ��|�{�p�a��N�� < 42004
030200                      MOVE "3�Ζ���"       TO ���ʃR�����g�v
030210                   ELSE
030220                      MOVE "�`������A�w�O"  TO ���ʃR�����g�v
030230                   END-IF
030240                END-EVALUATE
030250             END-IF
030260         END-IF
030270**
030280*---  �s�����Ǝ��d�l -----*
030290* 14/10�`�@�V���@���ۑސE�̂ݕ\�����Ⴄ
030300         IF ��|�{�p�a��N�� >= 41410
030310             IF ��|������ = ZERO
030320                EVALUATE �ی���ʂv�q 
030330                WHEN 01
030340                   IF ��|�ی��Ҕԍ�(1:2) = "15"
030350                      EVALUATE ��|���ʋ敪
030360                      WHEN 1
030370                         MOVE "����҂X��"    TO ���ʃR�����g�v
030380                      WHEN 2
030390                         MOVE "����҂W��"    TO ���ʃR�����g�v
030400                      WHEN 3
030410                         MOVE "����҂V��"    TO ���ʃR�����g�v
030420                      WHEN 6
030430                         IF ��|�{�p�a��N�� < 42004
030440                            MOVE "3�Ζ��� 8��"   TO ���ʃR�����g�v
030450                         ELSE
030460                            MOVE "���A�w��8��"   TO ���ʃR�����g�v
030470                         END-IF
030480                      WHEN OTHER
030490                         MOVE SPACE           TO ���ʃR�����g�v
030500                      END-EVALUATE
030510                   END-IF
030520                WHEN 08
030530                   IF ��|�ی��Ҕԍ�(3:2) = "15"
030540                      EVALUATE ��|���ʋ敪
030550                      WHEN 1
030560                         MOVE "����҂X��"    TO ���ʃR�����g�v
030570                      WHEN 2
030580                         MOVE "����҂W��"    TO ���ʃR�����g�v
030590                      WHEN 3
030600                         MOVE "����҂V��"    TO ���ʃR�����g�v
030610                      WHEN 6
030620                         IF ��|�{�p�a��N�� < 42004
030630                            MOVE "3�Ζ��� 8��"   TO ���ʃR�����g�v
030640                         ELSE
030650                            MOVE "���A�w��8��"   TO ���ʃR�����g�v
030660                         END-IF
030670                      WHEN OTHER
030680                         MOVE SPACE           TO ���ʃR�����g�v
030690                      END-EVALUATE
030700                   END-IF
030710                END-EVALUATE
030720             END-IF
030730         END-IF
030740**
030750* 20/04�`�@���������ʋ敪�R�����g��
030760         IF ��|�{�p�a��N�� >= 42004
030770             IF ��|�ی���� = 05
030780                EVALUATE ��|���ʋ敪
030790                WHEN 1
030800                   MOVE "����҂P��"  TO ���ʃR�����g�v
030810                WHEN 2
030820                   MOVE "����҂Q��"  TO ���ʃR�����g�v
030830                WHEN 3
030840                   MOVE "����҂R��"  TO ���ʃR�����g�v
030850                END-EVALUATE
030860             END-IF
030870         END-IF
031450* 15/7�`�@����@����
031460         IF ��|�{�p�a��N�� >= 41507
031470            IF ( ��|������� = 52 OR 53 OR 55 ) AND
031480               ( ��|�{������ = 20 ) AND
031490               ( ��|���i�ؖ��敪 NOT = 1 ) AND
031500               ( ���Z�|�ꕔ���S�� NOT = ZERO )
031510*
031520               MOVE ��|��v�Ҕԍ�����(1:3)   TO �󋋎Ҕԍ��ҏW�v�P
031530               MOVE ��|��v�Ҕԍ�����(4:2)   TO �󋋎Ҕԍ��ҏW�v�Q
031540               MOVE ��|��v�Ҕԍ�����(6:10)  TO �󋋎Ҕԍ��ҏW�v�R
031550               MOVE "-"  TO �󋋎ҋ�؂P �󋋎ҋ�؂Q
031560            END-IF
031570         END-IF
031580*
031590     END-READ.
      *
028780     MOVE �{�p�a��v�q       TO ��Q�|�{�p�a��.
028790     MOVE �{�p�N�v�q         TO ��Q�|�{�p�N.
028800     MOVE �{�p���v�q         TO ��Q�|�{�p��.
028810     MOVE ���҃R�[�h�v�q     TO ��Q�|���҃R�[�h.
028820     READ ��f�ҏ��Q�e
019630     INVALID KEY
              MOVE SPACE     TO ��Q�|���R�[�h
              INITIALIZE        ��Q�|���R�[�h
           END-READ.
031600*================================================================*
031610 ��������擾 SECTION.
031620*
031630****************************************************
031640* �A���f�[�^����ی��҃}�X�^��萿������擾����B *
031650* ���ہ|��������敪=1�̏ꍇ������}�X�^���g�p   *
031660* �� ������...... �����於�̂v�Ɋi�[               *
031670****************************************************
031730     MOVE ������ʂv�q           TO �s�|������.
031740     MOVE ��p���S�Ҕԍ������v�q TO �s�|�s�����ԍ�.
031750     READ �s�����}�X�^
031760     INVALID KEY
031770         MOVE SPACE      TO �����於�̂v
031780     NOT INVALID KEY
028520         IF �s�|������敪 = 1
028530             MOVE ������ʂv�q           TO ����|�ی����
028540             MOVE ��p���S�Ҕԍ������v�q TO ����|�ی��Ҕԍ�
028550             READ ������}�X�^
028560             INVALID KEY
028570                 MOVE SPACE        TO �����於�̂v
028580             NOT INVALID KEY
028590                 MOVE ����|�ی��Җ���  TO �����於�̂v
028600             END-READ
028610         ELSE
028620             MOVE �s�|�s��������  TO �����於�̂v
028630         END-IF
028640     END-READ.
028650*
           MOVE �����於�̂v   TO �����於�̂v�q.
           STRING �����於�̂v DELIMITED BY SPACE
                  "���@�a"     DELIMITED BY SIZE
                  INTO �����於�̂v
           END-STRING.
027590*================================================================*
027600 �����Ǎ� SECTION.
027610*
027790     MOVE �{�p�a��v�q       TO ���|�{�p�a��.
027800     MOVE �{�p�N�v�q         TO ���|�{�p�N.
027810     MOVE �{�p���v�q         TO ���|�{�p��.
027820     MOVE ���҃R�[�h�v�q     TO ���|���҃R�[�h.
027830     READ �����f�[�^�e
027870     NOT INVALID KEY
027900         MOVE ���|���ʐ�                   TO ���ʐ��v
           END-READ.
032130*================================================================*
032140 �����f�[�^�擾 SECTION.
032150*
032160**************************************************
032170* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾 *
032180* �� ������...���ʁ{������ʂɂĉ��H���Ċi�[     *
032190* �� �����N.......�����N�v                       *
032200* �� ������.......�������v                       *
032210* �� ������.......�������v                       *
032220* �� �J�n�N.......�����N�v                       *
032230* �� �J�n��.......�������v                       *
032240* �� �J�n��.......�������v                       *
032250* �� �I���N.......�I���N�v                       *
032260* �� �I����.......�I�����v                       *
032270* �� �I����.......�I�����v                       *
032280* �� ������.......�������v                       *
032290* �� �]�A�敪 ....�敪�ɂ��`�F�b�N��"��"���i�[ *
032300* �� �������q ....�敪�ɂ��`�F�b�N��"��"���i�[ *
032310* �� �o�߃R�[�h...�o�߃}�X�^���擾             *
032320**************************************************
032330*     MOVE �{�p�a��v�q       TO ���|�{�p�a��.
032340*     MOVE �{�p�N�v�q         TO ���|�{�p�N.
032350*     MOVE �{�p���v�q         TO ���|�{�p��.
032360*     MOVE ���҃R�[�h�v�q     TO ���|���҃R�[�h.
032370*     READ �����f�[�^�e
032380*     INVALID KEY
032390*         CONTINUE
032400**            /* ���肦�Ȃ� */
032410*     NOT INVALID KEY
032420*         MOVE ���|���ʐ�                   TO ���ʐ��v
032430         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
032440                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
032450             MOVE ���|�������(���ʂb�m�s) TO ������ʂv(���ʂb�m�s)
032460             MOVE ���|����(���ʂb�m�s)     TO ���ʂv(���ʂb�m�s)
032470             MOVE ���|���E�敪(���ʂb�m�s) TO ���E�敪�v(���ʂb�m�s)
032480             MOVE ���|�����ʒu�ԍ�(���ʂb�m�s)
032490                                           TO �����ʒu�ԍ��v(���ʂb�m�s)
032500********************************************************
032510* ���j�S�_...���ʖ�1+������ʁ{���ʖ�2�ɂĉ��H���Ċi�[ *
032520********************************************************
032530* �������
032540             MOVE SPACE                     TO �������̂v
032550             MOVE 03                        TO ���|�敪�R�[�h
032560             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
032570             READ ���̃}�X�^
032580             INVALID KEY
032590                 MOVE SPACE        TO �������̂v
032600             NOT INVALID KEY
032610                 MOVE ���|�������� TO �������̂v
032620             END-READ
032630* ����
020710             MOVE SPACE                    TO �������v(���ʂb�m�s)
032680*
032690             PERFORM ���ʖ��̖�������
032700*
032830             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
032840             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
032850             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
032860             MOVE ���|�J�n�N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
032870             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
032880             MOVE ���|�J�n��(���ʂb�m�s)   TO �������v(���ʂb�m�s)
032890             IF ���|�]�A�敪(���ʂb�m�s) = 9
032900                 MOVE 99                   TO �I���N�v(���ʂb�m�s)
032910                 MOVE 99                   TO �I�����v(���ʂb�m�s)
032920                 MOVE 99                   TO �I�����v(���ʂb�m�s)
032930             ELSE
032940                 MOVE ���|�I���N(���ʂb�m�s)   TO �I���N�v(���ʂb�m�s)
032950                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
032960                 MOVE ���|�I����(���ʂb�m�s)   TO �I�����v(���ʂb�m�s)
032970             END-IF
032980* �o�ߗ��̎擾
032990             MOVE 01                         TO �o�|�敪�R�[�h
033000             MOVE ���|�o�߃R�[�h(���ʂb�m�s) TO �o�|�o�߃R�[�h
033010             READ �o�߃}�X�^
033020             INVALID KEY
033030                 MOVE ZERO            TO ���ʂb�m�s�v(���ʂb�m�s)
033040                 MOVE SPACE           TO ���ʋ�؂v(���ʂb�m�s)
033050                 MOVE SPACE           TO �o�ߗ��̂v(���ʂb�m�s)
033060             NOT INVALID KEY
033070                 EVALUATE ���ʂb�m�s
033080                 WHEN 1
033090                     MOVE NC"�@" TO �o�ߕ��ʂv
033100                 WHEN 2
033110                     MOVE NC"�A" TO �o�ߕ��ʂv
033120                 WHEN 3
033130                     MOVE NC"�B" TO �o�ߕ��ʂv
033140                 WHEN 4
033150                     MOVE NC"�C" TO �o�ߕ��ʂv
033160                 WHEN 5
033170                     MOVE NC"�D" TO �o�ߕ��ʂv
033180                 END-EVALUATE
033190                 STRING  �o�ߕ��ʂv     DELIMITED BY SPACE
033200                         �o�|�o�ߗ���   DELIMITED BY SPACE
033210                        INTO ����o�ߗ��̂v(���ʂb�m�s)
033220                 END-STRING
033230             END-READ
033240*
033250             MOVE ���|�]�A�敪(���ʂb�m�s) TO �]�A�敪�v(���ʂb�m�s)
033260             EVALUATE ���|�]�A�敪(���ʂb�m�s)
033270             WHEN 1
033280             WHEN 2
033290                 MOVE NC"��"               TO �����`�F�b�N�v(���ʂb�m�s)
033300             WHEN 3
033310                 MOVE NC"��"               TO ���~�`�F�b�N�v(���ʂb�m�s)
033320             WHEN 4
033330                 MOVE NC"��"               TO �]��`�F�b�N�v(���ʂb�m�s)
033340             END-EVALUATE
033350*
      */�������̓��Z�|���ʎ�������]�L����/160816
031230             MOVE ���Z�|���ʎ�����(���ʂb�m�s) TO �������v(���ʂb�m�s)
033360         END-PERFORM.
033370* �V�K/�p�� �`�F�b�N
033380         EVALUATE ���Z�|���Z�����敪
               WHEN 1
033390             MOVE NC"��"                   TO �V�K�`�F�b�N�v
               WHEN 2
033410             MOVE NC"��"                   TO �p���`�F�b�N�v
033400         WHEN 3
033390             MOVE NC"��"                   TO �V�K�`�F�b�N�v
033410             MOVE NC"��"                   TO �p���`�F�b�N�v
               WHEN OTHER
033410             MOVE NC"��"                   TO �p���`�F�b�N�v
033420         END-EVALUATE.
033430* �}�Ԕ���p
033440         MOVE ���|�J�n�f�Ó��蓮�敪 TO  �J�n�f�Ó��蓮�敪�v.
033450*
033460* ������������敪
033470         MOVE ���|���Z������������敪 TO ���Z������������敪�v.
027880         MOVE ���|���Z�������R����敪 TO ���Z�������R����敪�v.
033480*
033490*     END-READ.
033500*================================================================*
033510*================================================================*
033520 ���������擾 SECTION.
033530*
033540********************************************************************
033550*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
033560*  ��: �@�A �Ƃœ]��.
033570*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
033580*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
033590********************************************************************
033600     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
033610     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
033620             UNTIL ( ���ʂb�m�s > ���ʐ��v )
033630*
033640****        IF ( ���|�������Ҕԍ�(���ʂb�m�s)  NOT = ZERO )  AND
033650        IF ( ���|�����A��(���ʂb�m�s)      NOT = ZERO )
033660*
033670           IF �J�E���^ = ZERO
033680               MOVE 1   TO  �J�E���^ �J�E���^�Q
033690               MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
033700               MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
033710               MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
033720           ELSE
033730              IF ( ���|�������Ҕԍ�(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
033740                 ( ���|�����A��(���ʂb�m�s)      = �����A�Ԃb�v     )
033750                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
033760                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
033770              ELSE
033780                 COMPUTE �J�E���^ = �J�E���^  +  1
033790                 MOVE 1   TO  �J�E���^�Q
033800                 MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
033810                 MOVE ���|�����A��(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
033820                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
033830              END-IF
033840           END-IF
033850        END-IF
033860     END-PERFORM.
033870**************************************************************************
033880*  ���������}�X�^��蕶�͎擾
033890**************************************************************************
033900     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
033910     PERFORM VARYING �J�E���^ FROM 1 BY 1
033920             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
033930** ���ۂ� �敪 01
033940         MOVE 01                        TO �����|�敪�R�[�h
033950         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
033960         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
033970         READ ���������e
033980         NOT INVALID KEY
033990             INITIALIZE ���������v�s
034000             MOVE �����|���������b�l(1) TO  ���������P�v�s
034010             MOVE �����|���������b�l(2) TO  ���������Q�v�s
034020             MOVE �����|���������b�l(3) TO  ���������R�v�s
034030             MOVE �����|���������b�l(4) TO  ���������S�v�s
034040             MOVE �����|���������b�l(5) TO  ���������T�v�s
034050             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
034060                     UNTIL ( �J�E���^�Q > 9 )  OR 
034070                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
034080                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
034090                WHEN 1
034100                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034110                WHEN 2
034120                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034130                WHEN 3
034140                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034150                WHEN 4
034160                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034170                WHEN 5
034180                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034170                WHEN 6
034180                   MOVE "�E"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034170                WHEN 7
034180                   MOVE "�F"  TO  ���������i���o�[�v�P(�J�E���^�Q)
034190                WHEN OTHER
034200                   CONTINUE
034210                END-EVALUATE
034220             END-PERFORM
034230*
034240             IF �����|�����������͋敪 = 1
034250                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
034260                        ���������P�v�s  DELIMITED BY SIZE
034270                        ���������Q�v�s  DELIMITED BY SIZE
034280                        ���������R�v�s  DELIMITED BY SIZE
034290                        ���������S�v�s  DELIMITED BY SIZE
034300                        ���������T�v�s  DELIMITED BY SIZE
034310                        INTO �����������e�����v(�J�E���^)
034320                 END-STRING
034330             ELSE
005946                 INSPECT ���������v�s REPLACING ALL �S�p�� BY ���p��
                       MOVE SPACE TO �����P�v �����Q�v
                       MOVE ���������i���o�[�m�v TO �����P�v
                       MOVE ���������P�v�s       TO �����Q�v
                       CALL �v���O�������v WITH C LINKAGE
                            USING BY REFERENCE �����P�v
                                  BY REFERENCE �����Q�v
                       MOVE ���������Q�v�s       TO �����Q�v
                       CALL �v���O�������v WITH C LINKAGE
                            USING BY REFERENCE �����P�v
                                  BY REFERENCE �����Q�v
                       MOVE ���������R�v�s       TO �����Q�v
                       CALL �v���O�������v WITH C LINKAGE
                            USING BY REFERENCE �����P�v
                                  BY REFERENCE �����Q�v
                       MOVE ���������S�v�s       TO �����Q�v
                       CALL �v���O�������v WITH C LINKAGE
                            USING BY REFERENCE �����P�v
                                  BY REFERENCE �����Q�v
                       MOVE ���������T�v�s       TO �����Q�v
                       CALL �v���O�������v WITH C LINKAGE
                            USING BY REFERENCE �����P�v
                                  BY REFERENCE �����Q�v
                        MOVE �����P�v TO �����������e�����v(�J�E���^)
034420             END-IF
034430*
034440         END-READ
034450     END-PERFORM.
034460*
034470     PERFORM ���������Z�b�g.
034480*
034490*================================================================*
034500 ���������Z�b�g SECTION.
034510*
034520**************************************************************************
034530*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
034540**************************************************************************
034550     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
034560     PERFORM VARYING �J�E���^ FROM 1 BY 1
034570             UNTIL ( �J�E���^ > 9 )  OR ( �����������e�����v(�J�E���^) = SPACE )
034580*
034590          INITIALIZE �����������e�����w�v
034600          MOVE �����������e�����v(�J�E���^)   TO  �����������e�����w�v
034610          IF  �����������e�P�w�v  NOT = SPACE
034620              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
034630              MOVE �����������e�P�w�v  TO ���������v(�J�E���^�Q)
034640          END-IF
034650          IF  �����������e�Q�w�v  NOT = SPACE
034660              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
034670              MOVE �����������e�Q�w�v  TO ���������v(�J�E���^�Q)
034680          END-IF
034690          IF  �����������e�R�w�v  NOT = SPACE
034700              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
034710              MOVE �����������e�R�w�v  TO ���������v(�J�E���^�Q)
034720          END-IF
034690          IF  �����������e�S�w�v  NOT = SPACE
034700              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
034710              MOVE �����������e�S�w�v  TO ���������v(�J�E���^�Q)
034720          END-IF
034730*
034740     END-PERFORM.
034750*================================================================*
034760*================================================================*
034770 �{�p�L�^�擾 SECTION.
034780*
034790************************************************************
034800* ��P�f�[�^���畉���f�[�^�e���ȉ��̏����擾           *
034810* �� �������Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
034820* �� ���É��Z .....�敪�ɂ��`�F�b�N��"��"���i�[...������ *
034830************************************************************
034840     MOVE  SPACE  TO  �����Č��t���O.
034850     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���ʐ��v
034860         IF ( �{�p�N�v = �����N�v(���ʂb�m�s) ) AND
034870            ( �{�p���v = �������v(���ʂb�m�s) )
034880             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
034890             MOVE �}�Ԃv�q              TO �{�L�|�}��
034900             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
034910             MOVE �����N�v(���ʂb�m�s)  TO �J�n�N�v(���ʂb�m�s) �{�L�|�{�p�N
034920             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
034930             MOVE �������v(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
034940         ELSE
034950             MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
034960             MOVE �}�Ԃv�q              TO �{�L�|�}��
034970             MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
034980             MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
034990             MOVE �{�p���v�q            TO �{�L�|�{�p��
035000             MOVE ZERO                  TO �{�L�|�{�p��
035010         END-IF
035020         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
035030                                      �{�L�|�{�p�a��N����
035040         END-START
035050         IF ��ԃL�[ = "00"
      */�������̓��Z�|���ʎ�������]�L����/160816
035060*             MOVE ZERO  TO �������v(���ʂb�m�s)
035070             MOVE ZERO  TO �I���N�v�s
035080             MOVE ZERO  TO �I�����v�s
035090             MOVE ZERO  TO �I�����v�s
035100             MOVE SPACE TO �I���t���O�Q
035110             PERFORM �{�p�L�^�e�Ǎ�
035120             IF  ( �I���t���O�Q      = SPACE   ) AND
035130                 ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
035140                 ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
035150                 ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
035160                 ( �{�L�|�{�p��      = �{�p���v�q     ) 
035170*
035180*        *****************************************************************
035190*        * �J�n�N���� ( ���̕��ʂ����������łȂ����A
035200*                       ���������ł��}�Ԃ����鎞�́A�ŏ��̎{�p�����J�n��)*
035210*        *****************************************************************
035220                 IF ( �{�p�N�v NOT = �����N�v(���ʂb�m�s) ) OR
035230                    ( �{�p���v NOT = �������v(���ʂb�m�s) ) OR
035240                    ( �J�n�f�Ó��蓮�敪�v = 1 )
035250                     MOVE �{�L�|�{�p�N   TO �J�n�N�v(���ʂb�m�s)
035260                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
035270                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
035280                 END-IF
035290             END-IF
035300             PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
035310                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q   ) OR
035320                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q     ) OR
035330                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q       ) OR
035340                           ( �{�L�|�{�p��     NOT = �{�p���v�q       ) OR
035350                           ( �{�L�|�{�p��         > �I�����v(���ʂb�m�s))
035360*               **********
035370*               * ������ *
035380*               **********
      */�������̓��Z�|���ʎ�������]�L����/160816
035390*                COMPUTE �������v(���ʂb�m�s) = �������v(���ʂb�m�s) + 1
035400                MOVE �{�L�|�{�p�N               TO �I���N�v�s
035410                MOVE �{�L�|�{�p��               TO �I�����v�s
035420                MOVE �{�L�|�{�p��               TO �I�����v�s
035430*
035440                PERFORM �{�p�L�^�e�Ǎ�
035450            END-PERFORM
035460        END-IF
035470*       **************************
035480*       * �p���F�I���N�����Z�b�g *
035490*       **************************
035500        IF �]�A�敪�v(���ʂb�m�s) = 9
035510            MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
035520            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
035530            MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
035540        END-IF
035550        IF �I���N�����v(���ʂb�m�s) > �󗝔N�����v
035560            MOVE �I���N�v(���ʂb�m�s) TO �󗝔N�v
035570            MOVE �I�����v(���ʂb�m�s) TO �󗝌��v
035580            MOVE �I�����v(���ʂb�m�s) TO �󗝓��v
035590        END-IF
035600     END-PERFORM.
035610*
035620** ----- �O�������݂̂��𔻒� -----------*
035630*
035640*     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
035650*     MOVE �}�Ԃv�q              TO �{�L�|�}��.
035660*     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
035670*     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
035680*     MOVE �{�p���v�q            TO �{�L�|�{�p��.
035690*     MOVE ZERO                  TO �{�L�|�{�p��.
035700*     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
035710*                                  �{�L�|�{�p�a��N����
035720*     END-START.
035730*     IF ��ԃL�[ = "00"
035740*             MOVE SPACE TO �I���t���O�Q
035750*             PERFORM �{�p�L�^�e�Ǎ�
035760*             IF  ( �I���t���O�Q      = SPACE   ) AND
035770*                 ( �{�L�|���҃R�[�h  = ���҃R�[�h�v�q ) AND
035780*                 ( �{�L�|�{�p�a��    = �{�p�a��v�q   ) AND
035790*                 ( �{�L�|�{�p�N      = �{�p�N�v�q     ) AND
035800*                 ( �{�L�|�{�p��      = �{�p���v�q     ) 
035810** �����{�p�J�n�����Č����ǂ�������
035820*                 IF   �{�L�|�Č������� = 1
035830*                      MOVE "YES"  TO  �����Č��t���O
035840*                 END-IF
035850**
035860*             END-IF
035870*     END-IF.
035880*     IF �����Č��t���O = "YES"
035890*        PERFORM �O�������̂ݔ���
035900*     END-IF.
035910*
035920*================================================================*
035930*================================================================*
035940 �������ȑO�̃f�[�^���� SECTION.
035950*
035960*********************************************************************************
035970*  �ŏ��̏������ȑO�̓������Ɏ{�p�L�^���R�[�h����������(�����A���~)�́A�����敪��
035980*  �p���ɂ��`�F�b�N����B(�V�K�ƌp���̗���)
035990*********************************************************************************
036000** �ŏ��̏��������擾
036010     MOVE SPACE                 TO �����t���O.
036020     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�.
036030     MOVE �}�Ԃv�q              TO �{�L�|�}��.
036040     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��.
036050     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N.
036060     MOVE �{�p���v�q            TO �{�L�|�{�p��.
036070     MOVE ZERO                  TO �{�L�|�{�p��.
036080     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
036090                                  �{�L�|�{�p�a��N����
036100     END-START.
036110     IF ��ԃL�[ = "00"
036120         MOVE ZERO  TO �����a��v�s
036130         MOVE ZERO  TO �����N�v�s
036140         MOVE ZERO  TO �������v�s
036150         MOVE ZERO  TO �������v�s
036160         MOVE SPACE TO �I���t���O�Q
036170         PERFORM �{�p�L�^�e�Ǎ�
036180         PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
036190                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
036200                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
036210                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
036220                       ( �{�L�|�{�p��     NOT = �{�p���v�q      ) OR
036230                       ( �����t���O           = "YES"           ) 
036240               IF  �{�L�|�f�Ë敪 = 2
036250                   MOVE �{�L�|�{�p�a��           TO �����a��v�s
036260                   MOVE �{�L�|�{�p�N             TO �����N�v�s
036270                   MOVE �{�L�|�{�p��             TO �������v�s
036280                   MOVE �{�L�|�{�p��             TO �������v�s
036290                   MOVE "YES"                    TO �����t���O
036300               END-IF
036310               PERFORM �{�p�L�^�e�Ǎ�
036320         END-PERFORM
036330     END-IF.
036340*
036350* �������ȑO�̃f�[�^����
036360     IF �����t���O = "YES"
036370        MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
036380        MOVE �}�Ԃv�q              TO �{�L�|�}��
036390        MOVE �����a��v�s          TO �{�L�|�{�p�a��
036400        MOVE �����N�v�s            TO �{�L�|�{�p�N
036410        MOVE �������v�s            TO �{�L�|�{�p��
036420        MOVE �������v�s            TO �{�L�|�{�p��
036430        START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
036440                                     �{�L�|�{�p�a��N����
036450                                     REVERSED
036460        END-START
036470        IF ��ԃL�[ = "00"
036480           MOVE SPACE  TO �I���t���O�Q
036490           PERFORM �{�p�L�^�e�Ǎ�
036500           IF ( �I���t���O�Q    = SPACE        ) AND
036510              ( �{�L�|���Ҕԍ�  = ���Ҕԍ��v�q ) AND
036520              ( �{�L�|�}��      = �}�Ԃv�q     ) AND
036530              ( �{�L�|�{�p�a��  = �����a��v�s ) AND
036540              ( �{�L�|�{�p�N    = �����N�v�s   ) AND
036550              ( �{�L�|�{�p��    = �������v�s   )
036560*  �������ȑO�̓������Ɏ{�p�L�^���R�[�h����������
036570                IF �p���`�F�b�N�v = SPACE
036580                   MOVE NC"��"    TO �p���`�F�b�N�v
036590                END-IF
036600           END-IF
036610         END-IF
036620     END-IF.
036630*
036640*================================================================*
036650 ��������擾 SECTION.
036660*
036670* �R�J���ȏ�̒�������� "CHOUKI" ���Ă�. 
036680     MOVE  SPACE TO  �A���ԁ|�L�[.
036690     INITIALIZE      �A���ԁ|�L�[.
036700     MOVE �{�p�a��v�q  TO  �A���ԁ|�{�p�a��.
036710     MOVE �{�p�N�v�q    TO  �A���ԁ|�{�p�N.
036720     MOVE �{�p���v�q    TO  �A���ԁ|�{�p��.
036730     MOVE ���Ҕԍ��v�q  TO  �A���ԁ|���Ҕԍ�.
036740     MOVE �}�Ԃv�q      TO  �A���ԁ|�}��.
036750*
036760     CALL   "CHOUKI".
036770     CANCEL "CHOUKI".
036780*
036790**** �K�p�P���g�p (�u�O�������̂݁v�����鎞�́A��������)
036800     IF �A���ԁ|�Ώۃt���O  = "YES"
036810        IF �K�p�P�v  = SPACE
036820           MOVE NC"�������{�p�p�����R���ʂɋL��"  TO �K�p�P�v
036830        ELSE
036840           STRING �K�p�P�v           DELIMITED BY SPACE
036850                  NC"�C"             DELIMITED BY SIZE
036860                  NC"�������{�p�p�����R���ʂɋL��"   DELIMITED BY SIZE
036870                  INTO �K�p�P�v
036880           END-STRING
036890        END-IF
036900     END-IF.
036910*
036920*================================================================*
036930 �������Z�����擾 SECTION.
036940*****************************************************************
036950** �������Z�����ԊO�Ɛ[��̎��A�u��t���ԁv���󎚂���B
036970*****************************************************************
036980     IF ( ���Z�|���ԊO = 1 ) OR ( ���Z�|�[�� = 1 ) OR ( ���Z�|�x�� = 1 )
036990*
037000         MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
037010         MOVE �}�Ԃv�q              TO �{�L�|�}��
037020         MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
037030         MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
037040         MOVE �{�p���v�q            TO �{�L�|�{�p��
037050         MOVE ZERO                  TO �{�L�|�{�p��
037060         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
037070                                      �{�L�|�{�p�a��N����
037080         END-START
037090         IF ��ԃL�[ = "00"
037100             MOVE ZERO  TO �������Z�J�E���g
037110             MOVE SPACE TO �I���t���O�Q
037120             PERFORM �{�p�L�^�e�Ǎ�
037130             PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
037140                           ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
037150                           ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
037160                           ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
037170                           ( �{�L�|�{�p��     NOT = �{�p���v�q      ) 
037180                   IF  ( �{�L�|�������Z = 1 OR 2 OR 3 ) AND ( �{�L�|�f�Ë敪 = 2 )
035640                       COMPUTE �������Z�J�E���g = �������Z�J�E���g  + 1
037200                       IF  �������Z�J�E���g <= 3
037210                           MOVE �{�L�|�������Z TO �������Z�敪�v�s(�������Z�J�E���g)
037220                           MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
037230                           MOVE �{�L�|��t��   TO �������Z���v�s(�������Z�J�E���g)
037240                       END-IF
037250                   END-IF
037260                   PERFORM �{�p�L�^�e�Ǎ�
037270             END-PERFORM
037280** �������Z�̎������Z�b�g
033380             IF ( �������Z���v�s(1) NOT = ZERO ) OR ( �������Z���v�s(1) NOT = ZERO ) 
                       MOVE �������Z���v�s(1) TO �������Z���v
                       MOVE ":"               TO �������Z��؂v
                       MOVE �������Z���v�s(1) TO �������Z���v
                   END-IF
033380             IF ( �������Z���v�s(2) NOT = ZERO ) OR ( �������Z���v�s(2) NOT = ZERO ) 
031910                 PERFORM �������Z�K�p�Z�b�g
                   END-IF
037300         END-IF
037310*
037320     END-IF.
037330*
037340*================================================================*
037350 �������Z�K�p�Z�b�g SECTION.
037360*
037370     PERFORM VARYING �ԍ��J�E���^ FROM 1 BY 1
037380              UNTIL  �ԍ��J�E���^ > 3
037390         IF ( �������Z���v�s(�ԍ��J�E���^)  = ZERO )  AND 
037400            ( �������Z���v�s(�ԍ��J�E���^)  = ZERO ) 
037410             CONTINUE
037420         ELSE
037430* �Œ荀��
037440             EVALUATE �������Z�敪�v�s(�ԍ��J�E���^) 
037450             WHEN 1
037460                MOVE NC"���ԊO"   TO ���Z���e�v(�ԍ��J�E���^)
033320             WHEN 2
033330                MOVE NC"�x�@��"   TO ���Z���e�v(�ԍ��J�E���^)
037470             WHEN 3
037480                MOVE NC"�[�@��"   TO ���Z���e�v(�ԍ��J�E���^)
037490             END-EVALUATE
037500*
037510             MOVE NC"�F"          TO ���Z��؂v(�ԍ��J�E���^)
037520             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
037530             MOVE NC"��"          TO ���Œ�v(�ԍ��J�E���^)
037540*
037550**** ���������{��ϊ�
037560* ����
037570             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
037580             IF �����v >= 10
037590                 MOVE �����v�P    TO �����ԍ��v�P
037600                 PERFORM ���{��ϊ�
037610                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
037620                 MOVE �����v�Q    TO �����ԍ��v�P
037630                 PERFORM ���{��ϊ�
037640                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
037650             ELSE
037660                 MOVE �����v�Q    TO �����ԍ��v�P
037670                 PERFORM ���{��ϊ�
037680                 MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
037690             END-IF
037700* ��
037710             MOVE �������Z���v�s(�ԍ��J�E���^)  TO  �����v
037720             MOVE �����v�P    TO �����ԍ��v�P
037730             PERFORM ���{��ϊ�
037740             MOVE �S�p�����ԍ��v  TO �������Z���m�v�P(�ԍ��J�E���^)
037750             MOVE �����v�Q    TO �����ԍ��v�P
037760             PERFORM ���{��ϊ�
037770             MOVE �S�p�����ԍ��v  TO �������Z���m�v�Q(�ԍ��J�E���^)
037780** 
037790        END-IF
037800     END-PERFORM.
037810*
037820     MOVE  �������Z�W�c�m�v(1)   TO �������Z�����P�v. 
037830     MOVE  �������Z�W�c�m�v(2)   TO �������Z�����Q�v. 
037840     MOVE  �������Z�W�c�m�v(3)   TO �������Z�����R�v. 
037850*
037860**** �K�p�P���Q���g�p�i�������R�L�ڂœK�p�P���g���Ă��鎞�́A�K�p�Q�j
037870     IF ( �������Z���v�s(2)  = ZERO ) AND ( �������Z���v�s(2)  = ZERO ) 
037880         CONTINUE
037890     ELSE
037900         IF �K�p�P�v  = SPACE
037910               STRING NC"�������Z"       DELIMITED BY SIZE
037920                      �������Z�����P�v   DELIMITED BY SIZE
037930                      �������Z�����Q�v   DELIMITED BY SIZE
037940                      �������Z�����R�v   DELIMITED BY SIZE
037950                      INTO �K�p�P�v
037960               END-STRING
037970         ELSE
037980               STRING NC"�������Z"       DELIMITED BY SIZE
037990                      �������Z�����P�v   DELIMITED BY SIZE
038000                      �������Z�����Q�v   DELIMITED BY SIZE
038010                      �������Z�����R�v   DELIMITED BY SIZE
038020                      INTO �K�p�Q�v
038030               END-STRING
038040         END-IF
038050     END-IF.
038060*
038070*================================================================*
038080 ���{��ϊ� SECTION.
038090*
038100     MOVE NC"�O"     TO �S�p�����ԍ��v.
038110     CALL "htoz" WITH C LINKAGE
038120                        USING �����ԍ��v�P �S�p�����ԍ��v�P.
038130*
038140*================================================================*
038150 �{�p�L�^�e�Ǎ� SECTION.
038160*
038170     READ �{�p�L�^�e NEXT
038180     AT END
038190         MOVE "YES" TO �I���t���O�Q
038200     END-READ.
038210*
038220*================================================================*
038230 ������擾 SECTION.
038240*
038250* 2006/04 �ύX
038260* ������� "JOSEIMEI" ���Ă�. 
038270     MOVE SPACE TO  �A�������́|�L�[.
038280     INITIALIZE     �A�������́|�L�[.
038290     MOVE ������ʂv�q           TO �A�������́|�������.
038300     MOVE ��p���S�Ҕԍ������v�q TO �A�������́|��p���S�Ҕԍ�����.
           MOVE 39                     TO �A�������́|����R�[�h.
038310*
038320     CALL   "JOSEIMEI".
038330     CANCEL "JOSEIMEI".
038340*
038350     MOVE �A�������́|�P���� TO ������v.
038740*------------------------------------------------------------------------*
038750*/ ���("27")�̍��ہE�ސE�ŏ���������ꍇ�A�����ԍ����󎚂���
038760*/ ���̌����������l��/
038770*
038780     MOVE SPACE TO �����ԍ��v.
038790     IF ( ������ʂv�q NOT = ZERO ) AND
038800        ( ��p���S�Ҕԍ������v�q(1:2) NOT = "99" ) AND
038810        ( ��p���S�Ҕԍ������v�q(3:2)     = "27" )
038820        IF ( �����ʂv�q NOT = 05 ) AND
038830           (( �ی���ʂv�q = 01 AND �ی��Ҕԍ��v�q(1:2) = "27" ) OR
038840            ( �ی���ʂv�q = 08 AND �ی��Ҕԍ��v�q(3:2) = "27" ))
038850           MOVE ��p���S�Ҕԍ������v�q(1:2) TO �����ԍ��v
038860        END-IF
038870*
038880        IF (�ی���ʂv�q = 05) AND (�ی��Ҕԍ��v�q(3:2) = "27" )
038890           MOVE ��p���S�Ҕԍ������v�q(1:2) TO �����ԍ��v
038900        END-IF
038910     END-IF.
038920*
039500*================================================================*
039510 �O�������̂ݔ��� SECTION.
039520*
039530*** �O���̒ʉ@�������������� 
039540     MOVE  SPACE            TO �O���t���O.
039550     MOVE ��|���҃R�[�h    TO �{�L�|���҃R�[�h.
039560     MOVE ��|�{�p�a��      TO �{�L�|�{�p�a��.
039570     MOVE ��|�{�p�N        TO �{�L�|�{�p�N.
039580     MOVE ��|�{�p��        TO �{�L�|�{�p��.
039590     MOVE 1                 TO �{�L�|�{�p��.
039600     START �{�p�L�^�e   KEY IS <  �{�L�|���҃R�[�h
039610                                  �{�L�|�{�p�a��N����
039620                                  REVERSED
039630     END-START.
039640     IF ��ԃL�[ = "00"
039650         MOVE SPACE  TO �I���t���O�Q
039660         PERFORM �{�p�L�^�e�Ǎ�
039670         IF ( �I���t���O�Q      = SPACE  ) AND
039680            ( �{�L�|���҃R�[�h  = ��|���҃R�[�h ) AND
039690            ( �{�L�|�f�Ë敪    = 2 ) 
039700*
039710            PERFORM �O������
039720**** �K�p�P���g�p
039730            IF �O���t���O = "YES"
039740               MOVE NC"���O�������̂�"    TO  �K�p�P�v
039750            END-IF
039760**
039770         END-IF
039780     END-IF.
039790*
039800*================================================================*
039810 �O������  SECTION.
039820* 
039830*** �ǂݍ��񂾎{�p�L�^�̔N�����A�O�����ǂ������� (�N���̍��� 1 ��?)
039840      MOVE  SPACE  TO  �O���t���O.
039850      INITIALIZE  �v�Z�N�����v �J�n�N�����Q�v �I���N�����Q�v.
039860**
039870      MOVE ��|�{�p�a��    TO �I���a��Q�v.
039880      MOVE ��|�{�p�N      TO �I���N�Q�v.
039890      MOVE ��|�{�p��      TO �I�����Q�v.
039900      MOVE �{�L�|�{�p�a��  TO �J�n�a��Q�v.
039910      MOVE �{�L�|�{�p�N    TO �J�n�N�Q�v.
039920      MOVE �{�L�|�{�p��    TO �J�n���Q�v.
039930*
039940      EVALUATE TRUE
039950       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v = �I���N�Q�v)
039960            PERFORM  �O����r��
039970       WHEN (�J�n�a��Q�v = �I���a��Q�v) AND (�J�n�N�Q�v NOT = �I���N�Q�v)
039980            PERFORM  �O����r�N
039990       WHEN  �J�n�a��Q�v NOT = �I���a��Q�v 
040000            PERFORM  �O����r����
040010      END-EVALUATE.
040020*
040030      IF �v�Z���v = 1
040040         MOVE  "YES"  TO  �O���t���O
040050      END-IF.
040060*
040070*================================================================*
040080 �O����r��  SECTION.
040090*
040100     IF  �I�����Q�v >  �J�n���Q�v
040110         COMPUTE �v�Z���v = �I�����Q�v - �J�n���Q�v
040120     ELSE
040130        MOVE ZERO TO �v�Z���v
040140     END-IF.
040150*
040160*================================================================*
040170 �O����r�N  SECTION.
040180*
040190     IF  �I���N�Q�v >  �J�n�N�Q�v
040200         COMPUTE �v�Z�N�v = �I���N�Q�v - �J�n�N�Q�v
040210         COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
040220     ELSE
040230        MOVE ZERO TO �v�Z���v
040240     END-IF.
040250*
040260*================================================================*
040270 �O����r����  SECTION.
040280*
040290     MOVE �J�n�a��Q�v TO ���|�����敪.
040300     READ �����}�X�^
040310     NOT INVALID KEY
040320         MOVE ���|�J�n����N TO �J�n����N�v
040330     END-READ.
040340     MOVE �I���a��Q�v TO ���|�����敪.
040350     READ �����}�X�^
040360     NOT INVALID KEY
040370         MOVE ���|�J�n����N TO �I������N�v
040380     END-READ.
040390**
040400     IF (�J�n����N�v NOT = ZERO) AND (�I������N�v NOT = ZERO)
040410        COMPUTE �J�n����N�v = �J�n����N�v + �J�n�N�Q�v - 1
040420        COMPUTE �I������N�v = �I������N�v + �I���N�Q�v - 1
040430*
040440        IF �I������N�v =  �J�n����N�v
040450           PERFORM  �O����r��
040460        ELSE
040470           IF  �I������N�v >  �J�n����N�v
040480               COMPUTE �v�Z�N�v = �I������N�v - �J�n����N�v
040490               COMPUTE �v�Z���v = (�v�Z�N�v * 12 + �I�����Q�v) - �J�n���Q�v
040500           ELSE
040510               MOVE ZERO TO �v�Z���v
040520           END-IF
040530        END-IF
040540     ELSE
040550        MOVE ZERO TO �v�Z���v
040560     END-IF.
040570*
040580*================================================================*
040590 �E�v���擾 SECTION.
040600*
040610* �E�v���擾�� "TEKIYBUN" ���Ă�. 
040620     MOVE  SPACE TO  �A�E���|�L�[.
040630     INITIALIZE      �A�E���|�L�[.
040640     MOVE �{�p�a��v�q  TO  �A�E���|�{�p�a��.
040650     MOVE �{�p�N�v�q    TO  �A�E���|�{�p�N.
040660     MOVE �{�p���v�q    TO  �A�E���|�{�p��.
040670     MOVE ���Ҕԍ��v�q  TO  �A�E���|���Ҕԍ�.
040680     MOVE �}�Ԃv�q      TO  �A�E���|�}��.
040700*     MOVE 63            TO  �A�E���|������.
039370     MOVE 56            TO  �A�E���|������.
015000     IF (���Z�������R����敪�v NOT = 1 )
               MOVE �������R����敪�v TO �A�E���|�����敪
           ELSE
               MOVE 1                  TO �A�E���|�����敪
015050     END-IF.
040710*
040720     CALL   "TEKIYBUN".
040730     CANCEL "TEKIYBUN".
040740*
040750*================================================================*
040760 ������� SECTION.
040770*
040780     MOVE "YDT6421P" TO  ��`�̖��o.
040790     MOVE "SCREEN"   TO  ���ڌQ���o.
040800     WRITE YDT6421P.
040810***     WRITE ������R�[�h.
040820     PERFORM �G���[�����o.
040830*================================================================*
040840 �G���[�����o SECTION.
040850*
040860     IF �ʒm���o NOT = "00"
040870         DISPLAY NC"���[�G���["              UPON CONS
040880         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
040890         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
040900         DISPLAY NC"�g������o�F" �g������o UPON CONS
040910         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
040920                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
040930         ACCEPT  �L�[���� FROM CONS
040940         PERFORM �t�@�C����
040950         MOVE 99  TO PROGRAM-STATUS
040960         EXIT PROGRAM
040970     END-IF.
040980*================================================================*
040990 ��f�҈���敪�X�V SECTION.
041000*
041010** //  ��f�ҏ��e�̈���敪�ɂP���Z�b�g���A�X�V����B//  
041020*
041030     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
041040     MOVE �{�p�N�v�q         TO ��|�{�p�N.
041050     MOVE �{�p���v�q         TO ��|�{�p��.
041060     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
041070     READ ��f�ҏ��e
041080     NOT INVALID KEY
041090         MOVE  1  TO  ��|���Z����敪����
041100         REWRITE  ��|���R�[�h
041110         END-REWRITE
041120         IF ��ԃL�[ NOT = "00"
041130            MOVE NC"��f��" TO �t�@�C����
041140            PERFORM �G���[�\��
041150         END-IF
041160     END-READ.
041170*
041180*================================================================*
041190 �ϔC�N�����擾 SECTION.
041200*
041210** ---// �����̎󗝔N�ɂ́A�ŏI�ʉ@���������Ă���ׁA�ޔ����� //----
041220     MOVE �󗝔N�v   TO �ŏI�ʉ@�N�v.
041230     MOVE �󗝌��v   TO �ŏI�ʉ@���v.
041240     MOVE �󗝓��v   TO �ŏI�ʉ@���v.
041250***/�������Œ�
041260* (�_���t��)
041270*     EVALUATE ���Z�v�g���t�敪�v 
041280*    /  �ŏI�ʉ@�� /
041290*     WHEN ZERO
041300*         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
041310*         MOVE �ŏI�ʉ@���v TO �_���t���v
041320*         MOVE �ŏI�ʉ@���v TO �_���t���v
041330*    /  ������ /
041340*     WHEN 1 
041350         PERFORM �������擾
041360         MOVE �󗝔N�v     TO �_���t�N�v.
041370         MOVE �󗝌��v     TO �_���t���v.
041380         MOVE �󗝓��v     TO �_���t���v.
041390*    /  �󎚂Ȃ� /
041400*     WHEN 9
041410*         MOVE ZERO         TO �_���t�N�v
041420*         MOVE ZERO         TO �_���t���v
041430*         MOVE ZERO         TO �_���t���v
041440*    /  ���̑��́A�ŏI�ʉ@�� /
041450*     WHEN OTHER
041460*         MOVE �ŏI�ʉ@�N�v TO �_���t�N�v
041470*         MOVE �ŏI�ʉ@���v TO �_���t���v
041480*         MOVE �ŏI�ʉ@���v TO �_���t���v
041490*     END-EVALUATE.
041500**
041510* (���ґ�)
041520*     EVALUATE ���Z�v�g���ғ��t�敪�v 
041530*    /  �ŏI�ʉ@�� /
041540*     WHEN ZERO
041550*         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
041560*         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
041570*         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
041580*    /  ������ /
041590*     WHEN 1 
041600*         PERFORM �������擾
041610         MOVE �󗝔N�v     TO ���҈ϔC�N�v.
041620         MOVE �󗝌��v     TO ���҈ϔC���v.
041630         MOVE �󗝓��v     TO ���҈ϔC���v.
041640*    /  �󎚂Ȃ� /
041650*     WHEN 9
041660*         MOVE ZERO         TO ���҈ϔC�N�v
041670*         MOVE ZERO         TO ���҈ϔC���v
041680*         MOVE ZERO         TO ���҈ϔC���v
041690*    /  ���̑��́A�ŏI�ʉ@�� /
041700*     WHEN OTHER
041710*         MOVE �ŏI�ʉ@�N�v TO ���҈ϔC�N�v
041720*         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
041730*         MOVE �ŏI�ʉ@���v TO ���҈ϔC���v
041740*     END-EVALUATE.
041750*
041760*================================================================*
041770*================================================================*
041780 �������擾 SECTION.
041790*
041800     MOVE �{�p�N�v�q   TO �󗝔N�v.
041810     MOVE �{�p���v�q   TO �󗝌��v.
041820     MOVE �{�p�a��v�q TO ���|�����敪.
041830     READ �����}�X�^
041840     NOT INVALID KEY
041850         MOVE ���|�J�n����N TO �{�p����N�v
041860     END-READ.
041870     IF �{�p����N�v NOT = ZERO
041880        COMPUTE �{�p����N�v = �{�p����N�v + �{�p�N�v�q - 1
041890     END-IF.
041900*
041910     EVALUATE �{�p���v�q
041920     WHEN 4
041930     WHEN 6
041940     WHEN 9
041950     WHEN 11
041960         MOVE 30 TO �󗝓��v
041970     WHEN 2
041980         DIVIDE 4 INTO �{�p����N�v GIVING    ���v
041990                                    REMAINDER �]�v
042000         END-DIVIDE
042010         IF �]�v = ZERO
042020             MOVE 29 TO �󗝓��v
042030         ELSE
042040             MOVE 28 TO �󗝓��v
042050         END-IF
042060     WHEN 1
042070     WHEN 3
042080     WHEN 5
042090     WHEN 7
042100     WHEN 8
042110     WHEN 10
042120     WHEN 12
042130         MOVE 31 TO �󗝓��v
042140     WHEN OTHER
042150          CONTINUE
042160     END-EVALUATE.
042170*
042180*================================================================*
042190 �������Z�܂Ƃߔ��� SECTION.
042200*---------------------------------------------------------------------------*
042210* �s�����}�X�^��ǂ݁A���Z�܂Ƃߋ敪���P�ł��A�{�̕ی������ہE�ސE
042220* �̎��́A�t���OYES (���z���������݂ň󎚁j
042230*�i��F���l�s�̏�Q�́A�{�̕ی��i���یn�j�̃��Z�v�g�P���Ő����A�������Z�͂Ȃ��j
042240*---------------------------------------------------------------------------*
042250*
042260     MOVE SPACE TO �������Z�܂Ƃ߃t���O.
           IF ( ���Z�|�{�̂܂Ƃߋ敪 = 1 )
042620           MOVE "YES" TO �������Z�܂Ƃ߃t���O
042630*        END-IF
042640     END-IF.
042650*
042660*----------------------------------------------------------------------*
042670** / �_�ސ쌧�ŗL�F�E�v�ɕ��S�Ҕԍ��Ǝ󋋎Ҕԍ� /
042680     IF ( �������Z�܂Ƃ߃t���O = "YES" ) AND
042690        ( ��|��p���S�Ҕԍ�����(3:2) = "14" )
042700        IF ��|��p���S�Ҕԍ�����(1:2) NOT = "99"
042770            MOVE ��|��p���S�Ҕԍ�����    TO ����S�Ҕԍ�
042780*            MOVE ��|��v�Ҕԍ�����        TO �󋋎Ҕԍ�
      */�󋋎Ҕԍ����W�����ȏ�̏ꍇ�g�𖳎����Ĉ������/110425
                  MOVE ��|��v�Ҕԍ�����   TO �󋋎Ҕԍ��v
                  IF ����󋋎Ҕԍ��Q�v = SPACE
016830                MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
                  ELSE
                      MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
                  END-IF
042790        END-IF
042800     END-IF.
042810*/�a�̎R����Q���c���ЂƂ�e/100518
042820     IF ( �������Z�܂Ƃ߃t���O = "YES" ) AND
042830        ( ��|��p���S�Ҕԍ�����(3:2) = "30" )
042840        IF ��|��p���S�Ҕԍ�����(1:2) NOT = "99"
042910            MOVE ��|��p���S�Ҕԍ�����    TO ����S�Ҕԍ� 
042920*            MOVE ��|��v�Ҕԍ�����        TO �󋋎Ҕԍ�
      */�󋋎Ҕԍ����W�����ȏ�̏ꍇ�g�𖳎����Ĉ������/110425
                  MOVE ��|��v�Ҕԍ�����   TO �󋋎Ҕԍ��v
                  IF ����󋋎Ҕԍ��Q�v = SPACE
016830                MOVE ����󋋎Ҕԍ��v TO �󋋎Ҕԍ�
                  ELSE
                      MOVE �󋋎Ҕԍ��v     TO �󋋎Ҕԍ��Q
                  END-IF
              END-IF
042930     END-IF.
      */���������̃��Z�܂Ƃߎ��͋��t������10���ɂ���/121108
042820     IF ( �������Z�܂Ƃ߃t���O        = "YES") AND
042830        ( ��|��p���S�Ҕԍ�����(3:2) = "07" )
               MOVE NC"��" TO �P�O���`�F�b�N�v
               MOVE SPACE  TO �X���`�F�b�N�v �W���`�F�b�N�v �V���`�F�b�N�v
           END-IF.
042940*
042950*================================================================*
042960 ���������v�Z SECTION.
           EVALUATE ��|�ی����
           WHEN 05
               MOVE 2          TO ���Z�|���Z���
           WHEN OTHER
               MOVE 1          TO ���Z�|���Z���
           END-EVALUATE.
019550     MOVE ��|�{�p�a�� TO ���Z�|�{�p�a��.
019560     MOVE ��|�{�p�N   TO ���Z�|�{�p�N.
019570     MOVE ��|�{�p��   TO ���Z�|�{�p��.
019580     MOVE ��|���Ҕԍ� TO ���Z�|���Ҕԍ�.
019590     MOVE ��|�}��     TO ���Z�|�}��.
019600     READ ���Z�v�g�e
019630     INVALID KEY
              MOVE SPACE     TO ���Z�|���R�[�h
              INITIALIZE        ���Z�|���R�[�h
           END-READ.
043210*================================================================*
043220 ���Z�E�v�ăZ�b�g SECTION.
043230*---------------------------------------------------------------*
043240* �E�v�t�@�C��������Β������R�̑O�ɍăZ�b�g����B
043250* �i������Ή������Ȃ��A�܂蒷�����R�͂��̂܂܁j
043260*---------------------------------------------------------------*
           PERFORM �E�v���擾.
           MOVE �A�E���|�E�v��(1)    TO �������R���P.
           MOVE �A�E���|�E�v��(2)    TO �������R���Q.
           MOVE �A�E���|�E�v��(3)    TO �������R���R.
           MOVE �A�E���|�E�v��(4)    TO �������R���S.
           MOVE �A�E���|�E�v��(5)    TO �������R���T.
           MOVE �A�E���|�E�v��(6)    TO �������R���U.
           MOVE �A�E���|�E�v��(7)    TO �������R���V.
           MOVE �A�E���|�E�v��(8)    TO �������R���W.
043400*
043410*================================================================*
043420 ���Z�v�g���я��擾 SECTION.
043430*
043440     MOVE �{�p�a��v�q       TO ��Q�|�{�p�a��.
043450     MOVE �{�p�N�v�q         TO ��Q�|�{�p�N.
043460     MOVE �{�p���v�q         TO ��Q�|�{�p��.
043470     MOVE ���҃R�[�h�v�q     TO ��Q�|���҃R�[�h.
043480     MOVE ������ʂv�q       TO ��Q�|�ی����.
043490     READ ��ƃt�@�C���Q
043500     NOT INVALID KEY
043510          MOVE ��Q�|����    TO ���Ԃv
043520     END-READ.
043530*
043540*================================================================*
043550*================================================================*
043560 �G���[�\�� SECTION.
043570*
043580     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
043590     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
043600     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
043610     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
043620     ACCEPT  �L�[���� FROM CONS
043630     PERFORM �t�@�C����.
043640     EXIT PROGRAM.
044950*
044951*================================================================*
044960*================================================================*
044961 ������������Ώ۔��菈�� SECTION.
044963*------------------------------------------------------------------------------------*
044964* ����}�X�^�́u������������敪�v�� 3 �i�R���ʈȏ����j�̎��A�R���ʈȏォ���肵�āA
044965* ���̎��̂݁A�����������������B
044966*------------------------------------------------------------------------------------*
044967*
044979     MOVE  SPACE TO  �A���Z������|�L�[.
044980     INITIALIZE      �A���Z������|�L�[.
044981     MOVE �{�p�a��v�q  TO  �A���Z������|�{�p�a��.
044982     MOVE �{�p�N�v�q    TO  �A���Z������|�{�p�N.
044983     MOVE �{�p���v�q    TO  �A���Z������|�{�p��.
044984     MOVE ���Ҕԍ��v�q  TO  �A���Z������|���Ҕԍ�.
044985     MOVE �}�Ԃv�q      TO  �A���Z������|�}��.
044986     CALL   "RECEHUGE".
044987     CANCEL "RECEHUGE".
044989*
044990     IF �A���Z������|�Ώۃt���O = "YES"
044991        PERFORM ���������擾
044992     END-IF.
044993*
045001*================================================================*
045002*================================================================*
045003 ���ʖ��̖������� SECTION.
045004*
006490     STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
009980            �������̂v                    DELIMITED BY SPACE
006500            ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
006520       INTO �������v(���ʂb�m�s)
006570     END-STRING.
045140*
045150*================================================================*
045160 �t�@�C���� SECTION.
045170*
045180     CLOSE ����t�@�C��     �ی��҃}�X�^     �����}�X�^
045190           ���̃}�X�^       ���Z�v�g�e       ������}�X�^
045200           �{�p�����}�X�^ ������}�X�^     �o�߃}�X�^
045210           ��f�ҏ��e     �{�p�L�^�e       �����f�[�^�e
045220           ���������e      �h�c�Ǘ��}�X�^    �s�����}�X�^
045230           �����t�@�C��     ��ƃt�@�C���Q
                 �ϔC�ҏ��}�X�^ ����}�X�^     ��f�ҏ��Q�e.
045240*================================================================*
045250 �I������ SECTION.
045260*
045270     PERFORM �t�@�C����.
045280*================================================================*
045290*================================================================*
045300 �e�X�g�󎚏��� SECTION.
045310*
           MOVE ALL "9" TO
           �s���{���ԍ� �{�p�� �{�p�N ���ҔN ���Ҍ� ���ғ� �J�n�N�P �J�n���P �J�n���P �I���N�P 
           �I�����P �I�����P �����N�P �������P �������P �����N�P �������P �������P �������P 
           �J�n�N�Q �J�n���Q �J�n���Q �I���N�Q �I�����Q �I�����Q �����N�Q �������Q �������Q 
           �����N�Q �������Q �������Q �������Q �J�n�N�R �J�n���R �J�n���R �I���N�R �I�����R 
           �I�����R �����N�R �������R �������R �����N�R �������R �������R �������R �J�n�N�S 
           �J�n���S �J�n���S �I���N�S �I�����S �I�����S �����N�S �������S �������S �����N�S 
           �������S �������S �������S �J�n�N�T �J�n���T �J�n���T �I���N�T �I�����T �I�����T 
           �����N�T �������T �������T �����N�T �������T �������T �������T ������ ���������k�� 
           ���Ë��� �Č��� �������q���Z�� ���É� ���×� ���v �������Z�� �{�p���񋟗� 
           ���É��Z�� �������Z�� �������Z�� �������Z��� ���񏈒u��(1) ���񏈒u��(2) 
           ���񏈒u��(3) ���񏈒u��(4) ���񏈒u��(5) ���񏈒u�����v ��ÒP���P ��É񐔂P 
           ��×��P ��㪖@�񐔂P ��㪖@���P ��㪖@�񐔂P ��㪖@���P �d�É񐔂P �d�×��P ���v�P 
           �����������P ���������v�P ��ÒP���Q ��É񐔂Q ��×��Q ��㪖@�񐔂Q ��㪖@���Q 
           ��㪖@�񐔂Q ��㪖@���Q �d�É񐔂Q �d�×��Q ���v�Q �����������Q ���������v�Q 
           ��ÒP���R�W ��É񐔂R�W ��×��R�W ��㪖@�񐔂R�W ��㪖@���R�W ��㪖@�񐔂R�W 
           ��㪖@���R�W �d�É񐔂R�W �d�×��R�W ���v�R�W �����ʍ����v�R�W �����������R�W 
           ���������v�R�W �����J�n���R�O �����J�n���R�O ��ÒP���R�O ��É񐔂R�O ��×��R�O 
           ��㪖@�񐔂R�O ��㪖@���R�O ��㪖@�񐔂R�O ��㪖@���R�O �d�É񐔂R�O �d�×��R�O ���v�R�O
           �����������R�O ���������v�R�O �����J�n���S�W �����J�n���S�W ��ÒP���S�W ��É񐔂S�W 
           ��×��S�W ��㪖@�񐔂S�W ��㪖@���S�W ��㪖@�񐔂S�W ��㪖@���S�W �d�É񐔂S�W �d�×��S�W 
           ���v�S�W �����ʍ����v�S�W �����������S�W ���������v�S�W �����J�n���S�O �����J�n���S�O 
           ��ÒP���S�O ��É񐔂S�O ��×��S�O ��㪖@�񐔂S�O ��㪖@���S�O ��㪖@�񐔂S�O ��㪖@���S�O 
           �d�É񐔂S�O �d�×��S�O ���v�S�O �����������S�O ���������v�S�O ���v �ꕔ���S�� �������z 
           �󗝔N �󗝌� �󗝓� �ϔC�N �ϔC�� �ϔC�� ������ �^���� �^����×� ���������z �󋋎ҕ��S�z
           ������(1) ������(2) ������(3) ��(1) ��(2) ��(3) ������(1) ������(2) ������(3)
           �^����(1) �^����(2) �^����(3) �^����(4) �^����(5)
           .
           MOVE ALL "X" TO
           ���ϔԍ� ���{�p�h�c �ی��Ҕԍ� �L���ԍ� ����S�Ҕԍ� �󋋎Ҕԍ� �Z���P �Z���Q 
           �������`�l�J�i�P �������`�l �_���t�ԍ� �����ԍ� ���Z�@�֖��P ���Z�@�֖��Q ���Z�@�֖��R 
           ���Z�@�֖��S �x�X���P �x�X���Q �x�X���R �x�X���S �{�p���X�֔ԍ��P �{�p���X�֔ԍ��Q 
           �{�p���Z���P �{�p���Z���Q �{�p���d�b�ԍ� ��\�҃J�i �ی��Җ��� �������q
           .
           MOVE ALL "�m" TO
           ��ی��Ҏ��� ���Ҏ��� �ڍ��@�� ��\�Җ�
           ���������P ���������Q ���������R ���������S ���������T ���������U ���������V ���������W
           �������R���P  �������R���Q �������R���R �������R���S �������R���T
           �������R���U �������R���V �K�p�R �����p��
           .
           MOVE ALL NC"�m" TO
           �������P �������Q �������R �������S �������T �o�ߗ���(1) �o�ߗ���(2) �o�ߗ���(3) 
           �o�ߗ���(4) �o�ߗ���(5) �K�p�P �K�p�Q
           .
           MOVE NC"��" TO
           �P�ƃ`�F�b�N �{�l�`�F�b�N ����`�F�b�N ���σ`�F�b�N ���`�F�b�N �Еۃ`�F�b�N 
           �g���`�F�b�N �P�O���`�F�b�N �X���`�F�b�N �Q���`�F�b�N �U�΃`�F�b�N �W���`�F�b�N 
           �V���`�F�b�N ����`�F�b�N �ސE�`�F�b�N ���ۃ`�F�b�N �Ƒ��`�F�b�N ���V�`�F�b�N 
           �j�`�F�b�N �����`�F�b�N �吳�`�F�b�N ���`�F�b�N ���a�`�F�b�N �����`�F�b�N 
           �����`�F�b�N�P ���~�`�F�b�N�P �]��`�F�b�N�P �����`�F�b�N�Q ���~�`�F�b�N�Q 
           �]��`�F�b�N�Q �����`�F�b�N�R ���~�`�F�b�N�R �]��`�F�b�N�R �����`�F�b�N�S 
           ���~�`�F�b�N�S �]��`�F�b�N�S �����`�F�b�N�T ���~�`�F�b�N�T �]��`�F�b�N�T �V�K�`�F�b�N 
           �p���`�F�b�N �[��`�F�b�N ���ԊO�`�F�b�N �x���`�F�b�N �Œ藿�`�F�b�N �������`�F�b�N 
           �{�×��`�F�b�N ��ԃ`�F�b�N �\���J��`�F�b�N ��H�`�F�b�N
           ���ʃ`�F�b�N �U���`�F�b�N �����`�F�b�N ��s�`�F�b�N ���Ƀ`�F�b�N �_���`�F�b�N 
           �{�X�`�F�b�N �x�X�`�F�b�N �{�x���`�F�b�N
           .
046640*
      *================================================================*
       �{�p���擾 SECTION.
      *
028350     MOVE ���Ҕԍ��v�q          TO �{�L�|���Ҕԍ�
028360     MOVE �}�Ԃv�q              TO �{�L�|�}��
028370     MOVE �{�p�a��v�q          TO �{�L�|�{�p�a��
028380     MOVE �{�p�N�v�q            TO �{�L�|�{�p�N
028390     MOVE �{�p���v�q            TO �{�L�|�{�p��
028400     MOVE ZERO                  TO �{�L�|�{�p��
028420     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
028430                                  �{�L�|�{�p�a��N����
028440     END-START
028450     IF ��ԃL�[ = "00"
030910         MOVE SPACE TO �I���t���O�Q
030920         PERFORM �{�p�L�^�e�Ǎ�
030930         PERFORM UNTIL ( �I���t���O�Q         = "YES"           ) OR
030940                       ( �{�L�|���҃R�[�h NOT = ���҃R�[�h�v�q  ) OR
030950                       ( �{�L�|�{�p�a��   NOT = �{�p�a��v�q    ) OR
030960                       ( �{�L�|�{�p�N     NOT = �{�p�N�v�q      ) OR
030970                       ( �{�L�|�{�p��     NOT = �{�p���v�q      )
                   MOVE NC"��" TO �{�p���`�F�b�N(�{�L�|�{�p��)
                   PERFORM �{�p�L�^�e�Ǎ�
               END-PERFORM
           END-IF.
           IF �������Z�v NOT = ZERO
               PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 31
                   MOVE �J�E���^ TO �{�p��(�J�E���^)
               END-PERFORM
           END-IF.
037520*================================================================*
       �t�b�^�Z�b�g SECTION.
      *
           MOVE ZERO  TO ���|�_���I���敪.
           MOVE 39    TO ���|����R�[�h.
           MOVE ZERO  TO ���|�ی����.
           MOVE ZERO  TO ���|�ύX�a��N��.
026480     READ ����}�X�^.
           MOVE "�y���肢�z"          TO ���肢.
           MOVE "�{�\�������e�ɂ��Ė⍇�킹�A�x���s�x���̒ʒm���A���͖{�\�����Ԗߓ��̑��t��͉��L�Z���A����ɂ��肢���܂��B"
                                      TO �ϔC���P.
           MOVE ���|�ڍ��t�      TO �ϔC�c�̖�.
           STRING "��"                DELIMITED BY SIZE
                  ���|��X�֔ԍ��P  DELIMITED BY SIZE
                  "-"                 DELIMITED BY SIZE
                  ���|��X�֔ԍ��Q  DELIMITED BY SIZE
             INTO �㗝�l�X�֔ԍ�
           END-STRING.
           MOVE ���|��Z���P        TO �㗝�l�Z���P.
           MOVE "�d�b"                TO �d�b�P�b�l.
           MOVE ���|��d�b�ԍ�      TO �ϔC�d�b�ԍ��P.
      *
           STRING "["                    DELIMITED BY SIZE
                  ��p���S�Ҕԍ������v�q DELIMITED BY SIZE
                  "]"                    DELIMITED BY SIZE
             INTO �ی��Ҕԍ��Q
           END-STRING.
           MOVE �����於�̂v�q        TO �ی��Җ���.
           STRING "("                 DELIMITED BY SIZE
                  ���҃R�[�h�v�q      DELIMITED BY SIZE
                  ")"                 DELIMITED BY SIZE
             INTO ���҃R�[�h
           END-STRING.
      *     MOVE ��ی��Ҏ����v        TO ��ی��Ҏ����Q.
           MOVE ���Ҏ����v            TO ��ی��Ҏ����Q.
           MOVE 02                    TO ���|�敪�R�[�h.
           MOVE ������ʂv�q          TO ���|���̃R�[�h.
           READ ���̃}�X�^.
           MOVE ���|����              TO �ی���ʖ��̂v�o.
           STRING �ی���ʖ��̂v      DELIMITED BY "�@"
                  ":DNo.="            DELIMITED BY SIZE
             INTO �ی����
           END-STRING.
022750* ���Z�v�g���я��Z�b�g *
022760     MOVE ���Ԃv                 TO ����.
037520*================================================================*
046660******************************************************************
046670 END PROGRAM YDT6421.
046680******************************************************************
