000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YIW1011.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*      �A�C�� ���o�t���b�s�[�쐬�yFPD�����z
000100* �����N��Ver.
000110*      MED = YIW580
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2022-11-11
000140 DATE-COMPILED.          2022-11-11
000150*----------------------------------------------------------------*
      */������Â��R�s�[
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
           SELECT  �v�Z�}�X�^      ASSIGN      TO        KEISANL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  �v�|����敪
                                                                �v�|�J�n�a��N��
                                   FILE STATUS              IS  ��ԃL�[
                                   LOCK        MODE         IS  AUTOMATIC.
           SELECT  �E�v�t�@�C��    ASSIGN      TO        TEKIYOL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  �E�v�|����敪
                                                                �E�v�|���҃R�[�h
                                                                �E�v�|�{�p�a��N��
                                   ALTERNATE RECORD KEY     IS  �E�v�|����敪
                                                                �E�v�|�{�p�a��N��
                                                                �E�v�|���҃R�[�h
                                   FILE STATUS              IS  ��ԃL�[
                                   LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  �h�c�Ǘ��}�X�^    ASSIGN      TO        IDKANRL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  �h�c�ǁ|�h�c�敪
001380                                                          �h�c�ǁ|�{�p���ԍ�
001390                                                          �h�c�ǁ|�ی����
001400                                                          �h�c�ǁ|�ی��Ҕԍ�
001410                             ALTERNATE RECORD KEY     IS  �h�c�ǁ|�{�p�h�c�ԍ�
001420                                                          �h�c�ǁ|�h�c�敪
001430                                                          �h�c�ǁ|�{�p���ԍ�
001440                                                          �h�c�ǁ|�ی����
001450                                                          �h�c�ǁ|�ی��Ҕԍ�
001460                             FILE STATUS              IS  ��ԃL�[
001470                             LOCK        MODE         IS  AUTOMATIC.
001739*  �U������
001740     SELECT �U�������e       ASSIGN      TO     "C:\MAKISHISYS\YAWOBJ\IWKOUZA.DAT"
001741                             ORGANIZATION             IS  LINE SEQUENTIAL
001742                             ACCESS MODE              IS  SEQUENTIAL
001743                             FILE STATUS              IS  ��ԃL�[
001744                             LOCK        MODE         IS  AUTOMATIC.
001720* ���я��󎚗p
001730     SELECT  ��ƃt�@�C���S  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001740                             ORGANIZATION             IS  INDEXED
001750                             ACCESS                   IS  DYNAMIC
001760                             RECORD      KEY          IS  ��S�|�{�p�a��N��
001770                                                          ��S�|���҃R�[�h
001780                                                          ��S�|�ی����
001790                             FILE        STATUS       IS  ��ԃL�[
001800                             LOCK        MODE         IS  AUTOMATIC.
001206     SELECT ��ƃt�@�C���P ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001208                             ORGANIZATION             IS  INDEXED
001210                             ACCESS MODE              IS  DYNAMIC
001220                             RECORD      KEY          IS  ��P�|���Z�v�g�ԍ�
000520                                                          ��P�|�{�p�a��N��
000570                                                          ��P�|���҃R�[�h
000510                                                          ��P�|���Z���
001330                             FILE STATUS              IS  ��ԃL�[
001340                             LOCK        MODE         IS  AUTOMATIC.
001206     SELECT ��ƃt�@�C���Q ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1012L.DAT"
001208                             ORGANIZATION             IS  INDEXED
001210                             ACCESS MODE              IS  DYNAMIC
001220                             RECORD      KEY          IS  ��Q�|�ی���ʂP
000520                                                          ��Q�|�ی��Ҕԍ�
001330                             FILE STATUS              IS  ��ԃL�[
001340                             LOCK        MODE         IS  AUTOMATIC.
001602*     SELECT ���o�t�@�C��   ASSIGN      TO     FD-NAME
001602     SELECT ���o�t�@�C��   ASSIGN      TO  "C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT1.json"
001603                             ORGANIZATION             IS  LINE SEQUENTIAL
001604                             ACCESS MODE              IS  SEQUENTIAL
001605                             FILE STATUS              IS  ��ԃL�[
001606                             LOCK      MODE           IS  AUTOMATIC.
001602     SELECT ���o�t�@�C���O   ASSIGN      TO  "C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT3.json"
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
001410     COPY SEIGYO01        OF  XFDLIB  JOINING   ���O�P AS  PREFIX.
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
      *                           �m�q�k��  �Q�T�U�n
       FD  �v�Z�}�X�^          BLOCK   CONTAINS   1   RECORDS.
           COPY KEISAN          OF  XFDLIB  JOINING   �v   AS  PREFIX.
           COPY KEISANA         OF  XFDLIB  JOINING   �v�` AS  PREFIX.
      *
       FD  �E�v�t�@�C��        BLOCK CONTAINS 1     RECORDS GLOBAL.
           COPY TEKIYO          OF    XFDLIB JOINING �E�v AS PREFIX.
002300*                           �m�q�k��  �P�Q�W�n
002310 FD  �h�c�Ǘ��}�X�^          BLOCK   CONTAINS   1   RECORDS.
002320     COPY IDKANR    OF  XFDLIB  JOINING   �h�c��   AS  PREFIX.
002390**
002294 FD  �U�������e      BLOCK   CONTAINS   1   RECORDS.
002295 01  �����|���R�[�h.
002296     03  �����|���R�[�h�f�[�^               PIC X(128).
002400**
002410 FD  ��ƃt�@�C���S RECORD  CONTAINS 32 CHARACTERS.
002420 01  ��S�|���R�[�h.
002430     03  ��S�|���R�[�h�L�[.
002440         05  ��S�|�{�p�a��N��.
002450             07  ��S�|�{�p�a��            PIC 9.
002460             07  ��S�|�{�p�N              PIC 9(2).
002470             07  ��S�|�{�p��              PIC 9(2).
002480         05  ��S�|���҃R�[�h.
002490             07 ��S�|���Ҕԍ�             PIC 9(6).
002500             07 ��S�|�}��                 PIC X(1).
002510         05  ��S�|�ی����                PIC 9(2).
002520     03  ��S�|���R�[�h�f�[�^.
002530         05  ��S�|����                    PIC 9(4).
002540         05  FILLER                        PIC X(14).
002550*
001520* FD  ��ƃt�@�C���P RECORD  CONTAINS 7257 CHARACTERS.
001520* FD  ��ƃt�@�C���P RECORD  CONTAINS 7261 CHARACTERS.
001520 FD  ��ƃt�@�C���P RECORD  CONTAINS 7321 CHARACTERS.
001530 01 ��P�|���R�[�h.
001540     03 ��P�|���R�[�h�L�[.
               05 ��P�|���Z�v�g�ԍ�           PIC 9(4).
000520         05 ��P�|�{�p�a��N��.
000530           07 ��P�|�{�p�a��             PIC 9.
000540           07 ��P�|�{�p�N��.
000550             09 ��P�|�{�p�N             PIC 9(2).
000560             09 ��P�|�{�p��             PIC 9(2).
000570         05 ��P�|���҃R�[�h.
000580           07 ��P�|���Ҕԍ�             PIC 9(6).
000590           07 ��P�|�}��                 PIC X.
000510         05 ��P�|���Z���               PIC 9(2).
           03 ��P�|���R�[�h�f�[�^.
               05 ��P�|�{�p�Ҕԍ�             PIC X(11).
               05 ��P�|�{�p����N��           PIC X(6).
               05 ��P�|��o��敪             PIC X(1).
               05 ��P�|�ی��Ҕԍ�             PIC X(8).
               05 ��P�|�ی��؋L��             PIC X(16).
               05 ��P�|�ی��ؔԍ�             PIC X(16).
               05 ��P�|����S�Ҕԍ��P       PIC X(8).
               05 ��P�|����󋋎Ҕԍ��P       PIC X(16).
               05 ��P�|����S�Ҕԍ��Q       PIC X(8).
               05 ��P�|����󋋎Ҕԍ��Q       PIC X(16).
               05 ��P�|�ی���ʑ�敪         PIC X(1).
               05 ��P�|�P���敪               PIC X(1).
               05 ��P�|�{�Ƌ敪               PIC X(1).
               05 ��P�|���t����               PIC 9(2).
               05 ��P�|����                   PIC X(1).
               05 ��P�|��ی��҃J�i           PIC X(25).
               05 ��P�|��ی��Җ�             PIC X(30).
               05 ��P�|��ÎҖ��J�i           PIC X(25).
               05 ��P�|��ÎҖ�               PIC X(30).
               05 ��P�|��ÎҐ���             PIC X(1).
               05 ��P�|��ÎҐ��N����         PIC X(8).
               05 ��P�|��ی��җX�֔ԍ�       PIC X(7).
      */�S�p���p��킸�ő�60����20230413
      *         05 ��P�|��ی��ҏZ��           PIC X(60).
               05 ��P�|��ی��ҏZ��           PIC X(120).
               05 ��P�|���v���z               PIC 9(6).
               05 ��P�|�ꕔ���S��             PIC 9(6).
               05 ��P�|�������z               PIC 9(6).
               05 ��P�|����ꕔ���S�������z   PIC 9(6).
               05 ��P�|��������z           PIC 9(6).
               05 ��P�|��������               PIC 9(2).
               05 ��P�|���ʐ�                 PIC 9(1).
               05 ��P�|�Đ����敪             PIC X(1).
               05 ��P�|���S�敪               PIC X(1).
               05 ��P�|���S����               PIC 9(1).
               05 ��P�|���Ҕԍ��Q             PIC 9(6).
               05 ��P�|���Ҕԍ��}��           PIC 9(1).
               05 ��P�|�����f�[�^ OCCURS 6.
                   07 ��P�|�����i���o�[       PIC 9(1).
                   07 ��P�|�����敪           PIC X(1).
                   07 ��P�|�����R�[�h         PIC X(8).
                   07 ��P�|������             PIC X(32).
                   07 ��P�|�����N����         PIC X(8).
                   07 ��P�|�����N����         PIC X(8).
                   07 ��P�|�{�p�J�n��         PIC X(8).
                   07 ��P�|�{�p�I����         PIC X(8).
                   07 ��P�|������             PIC 9(2).
                   07 ��P�|���ʎ{�p��.
                      09 ��P�|�{�p��          PIC 9(2) OCCURS 31.
                   07 ��P�|�]�A�敪           PIC X(1).
                   07 ��P�|���Ŏ{�敪         PIC X(1).
                   07 ��P�|���Ŏ{��         PIC 9(1).
                   07 ��P�|���Ŏ{�×�         PIC 9(5).
                   07 ��P�|��������           PIC X(200).
                   07 ��P�|�������R           PIC X(200).
                   07 ��P�|�����p�񗝗R       PIC X(200).
                   07 ��P�|�o��               PIC X(200).
                   07 ��P�|�R���敪           PIC X(1).
               05 ��P�|�����敪               PIC X(1).
               05 ��P�|������               PIC 9(1).
               05 ��P�|������                 PIC 9(5).
               05 ��P�|�����x�����Z��       PIC 9(1).
               05 ��P�|�����[����Z��       PIC 9(1).
               05 ��P�|�������ԊO���Z��     PIC 9(1).
               05 ��P�|�������Z               PIC 9(5).
               05 ��P�|���������k�x������   PIC 9(1).
               05 ��P�|���������k�x����       PIC 9(5).
               05 ��P�|�Č���               PIC 9(1).
               05 ��P�|�Č���                 PIC 9(5).
               05 ��P�|���Ë���               PIC 9(3).
               05 ��P�|���É�               PIC 9(2).
               05 ��P�|���×�                 PIC 9(5).
               05 ��P�|���×��R               PIC X(200).
               05 ��P�|��ԉ��Z�̉��É�     PIC 9(2).
               05 ��P�|��H���Z�̉��É�     PIC 9(2).
               05 ��P�|�\���J����Z�̉��É� PIC 9(2).
               05 ��P�|���É��Z               PIC 9(5).
               05 ��P�|�������q���         PIC 9(2).
               05 ��P�|�������q����         PIC 9(1).
               05 ��P�|�������q����         PIC 9(1).
               05 ��P�|�������q���Z           PIC 9(5).
               05 ��P�|�������q���Z��         PIC 9(2) OCCURS 3.
               05 ��P�|�{�p���񋟗��̉�   PIC 9(1).
               05 ��P�|�{�p���񋟗�         PIC 9(5).
               05 ��P�|�^����Î��{��       PIC 9(1).
               05 ��P�|�^����×�             PIC 9(5).
               05 ��P�|�^����Î��{��         PIC 9(2) OCCURS 5.
               05 ��P�|���ʕʗ����f�[�^ OCCURS 10.
                   07 ��P�|�s�ԍ�             PIC 9(1).
                   07 ��P�|�����J�n����       PIC X(4).
                   07 ��P�|��É�           PIC 9(2).
                   07 ��P�|��×�             PIC 9(5).
                   07 ��P�|��㪖@��         PIC 9(1).
                   07 ��P�|��㪖@��           PIC 9(5).
                   07 ��P�|��㪖@��         PIC 9(2).
                   07 ��P�|��㪖@��           PIC 9(5).
                   07 ��P�|�d�É�           PIC 9(2).
                   07 ��P�|�d�×�             PIC 9(5).
                   07 ��P�|�����ʒ�����       PIC 9(2).
                   07 ��P�|�����ʒ����z       PIC 9(5).
                   07 ��P�|����������         PIC 9(2).
                   07 ��P�|���ʒ������ʗ����v PIC 9(5).
               05 ��P�|�E�v                   PIC X(400).
               05 ��P�|���ӔN����             PIC X(8).
               05 ��P�|���ӈ�@               PIC X(40).
               05 ��P�|���ӈ�t��             PIC X(20).
               05 ��P�|�ی���ʏڍ�           PIC X(2).
      */���ڒǉ�������/20221028
               05 ��P�|���׏����s���Z��       PIC 9(2).
               05 ��P�|���׏����s���Z��       PIC 9(2).
      */���ڒǉ�������/20221028
001520 FD  ��ƃt�@�C���Q RECORD  CONTAINS 241 CHARACTERS.
001530 01 ��Q�|���R�[�h.
001540     03 ��Q�|���R�[�h�L�[.
               05 ��Q�|�ی���ʂP         PIC 9(2).
               05 ��Q�|�ی��Ҕԍ�         PIC X(8).
           03 ��Q�|���R�[�h�f�[�^.
               05 ��Q�|�ی��Җ�           PIC X(40).
               05 ��Q�|�ی����           PIC X(2).
               05 ��Q�|���R�[�h           PIC X(2).
               05 ��Q�|�X�֔ԍ�           PIC X(7).
               05 ��Q�|�Z���P             PIC X(80).
               05 ��Q�|�Z���Q             PIC X(80).
               05 ��Q�|�d�b�ԍ�           PIC X(13).
               05 ��Q�|�����ԍ�           PIC X(7).
004472* FD  ���o�t�@�C�� RECORD  CONTAINS 200 CHARACTERS.
004472* FD  ���o�t�@�C�� RECORD IS VARYING IN SIZE FROM 1 TO 200 DEPENDING ON �����b�m�s.
004472 FD  ���o�t�@�C�� RECORD IS VARYING IN SIZE FROM 1 TO 500 DEPENDING ON �����b�m�s.
004473 01  ���|���R�[�h.
004474*     03  ���|���R�[�h�f�[�^        PIC X(200).
004474     03  ���|���R�[�h�f�[�^        PIC X(500).
004472 FD  ���o�t�@�C���O RECORD IS VARYING IN SIZE FROM 1 TO 200 DEPENDING ON �����b�m�s.
004473 01  ���O�|���R�[�h.
004474     03  ���O�|���R�[�h�f�[�^        PIC X(200).
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
004580 01 �I���t���O�S                       PIC X(3) VALUE SPACE.
002090 01 �G���[�t���O                       PIC X(3) VALUE SPACE.
004590 01 ���s�L�[�v                         PIC X(3)  VALUE SPACE.
004600 01 �{�p�L�^�L�v                       PIC X(3) VALUE SPACE.
004610 01 �t�@�C����                         PIC N(8) VALUE SPACE.
004620 01 �J�E���^                           PIC 9(2) VALUE ZERO.
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
004790* 01 ����N�v                           PIC 9(4) VALUE ZERO.
004800* 01 �a��v                             PIC 9(1) VALUE ZERO.
004810* 01 �N�v                               PIC 9(2) VALUE ZERO.
004820 01 �a��N�����v.
004830   03 �a��N���v.
004840      05 �a��v                        PIC 9(1) VALUE ZERO.
004850      05 �N�v                          PIC 9(2) VALUE ZERO.
004860      05 ���v                          PIC 9(2) VALUE ZERO.
004870   03 ���v                             PIC 9(2) VALUE ZERO.
004880 01 ���t�W���v                         PIC 9(8) VALUE ZERO.
       01 �J�n�v.
004890   03 �J�n�a��N�����v                 OCCURS 7.
004900     05 �J�n�a��v                     PIC 9(1) VALUE ZERO.
004910     05 �J�n�N�v                       PIC 9(2) VALUE ZERO.
004920     05 �J�n���v                       PIC 9(2) VALUE ZERO.
004930     05 �J�n���v                       PIC 9(2) VALUE ZERO.
       01 �I���v.
004940   03 �I���a��N�����v                 OCCURS 7.
004950     05 �I���a��v                     PIC 9(1) VALUE ZERO.
004960     05 �I���N�v                       PIC 9(2) VALUE ZERO.
004970     05 �I�����v                       PIC 9(2) VALUE ZERO.
004980     05 �I�����v                       PIC 9(2) VALUE ZERO.
004980 01 �I�����v�q                         PIC 9(2) VALUE ZERO.
004990 01 �������v                           PIC 9(2) VALUE ZERO.
005000 01 ���ʂb�m�s                         PIC 9(2) VALUE ZERO.
005010 01 ���ʂb�m�s�Q                       PIC 9(2) VALUE ZERO.
005000 01 ���J�E���^                         PIC 9(2) VALUE ZERO.
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
005120 01 �x���J�E���^                       PIC 9(6) VALUE ZERO.
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
          03 ���k�x���񐔂v                  PIC 9(1)  VALUE ZERO.
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
      *
003040** ������t���[�N�p
003050 01 ����N���v.
003060    03 ����N�v                        PIC 9(4) VALUE ZERO.
003070    03 ����v                        PIC 9(2) VALUE ZERO.
003080** ������N���p
003090 01 ������N���v.
003100    03 ������N�v                    PIC 9(4) VALUE ZERO.
003110    03 ��������v                    PIC 9(2) VALUE ZERO.
004982* �Í������p
004983*
000140 01 �a��v�Z�N�����v.
000150   03 �a��v�Z�N���v.
000160     05 �a��v�Z�a��v                 PIC 9    VALUE ZERO.
000170     05 �a��v�Z�N���v.
000180       07 �a��v�Z�N�v                 PIC 9(2) VALUE ZERO.
000190       07 �a��v�Z���v                 PIC 9(2) VALUE ZERO.
000200   03 �a��v�Z���v                     PIC 9(2) VALUE ZERO.
000140 01 ����v�Z�N�����v.
000170   03 ����v�Z�N���v.
000180       05 ����v�Z�N�v                 PIC 9(4) VALUE ZERO.
000190       05 ����v�Z���v                 PIC 9(2) VALUE ZERO.
000200   03 ����v�Z���v                     PIC 9(2) VALUE ZERO.
003670****************
003680* �����f�[�^�e *
003690****************
003700 01 �������v.
003720    03 ���ʏ��v  OCCURS   9.
             05 �������v�q.
003790          07 �������v                  PIC N(16) VALUE SPACE.
004010       05 ���񏈒u�񐔂v               PIC 9     VALUE ZERO.
      *
005000 01 ���ʖ����v.
005000    03 ���ʓ��b�m�s OCCURS 9           PIC 9(2) VALUE ZERO.
005000    03 �����b�m�s                      PIC 9(2) VALUE ZERO.
004590    03 �����ʗ��R�v�q                  PIC 9(2) VALUE ZERO.
      * ���Z�E�v�p
001362 01 �����J�E���^                       PIC 9(3)  VALUE ZERO.
       01 �����ʒu�v                         PIC 9(4)  VALUE ZERO.
001363 01 ���s                               PIC X(2)  VALUE X"0D0A".
001367 01 �E�v�v.
001370    03 �E�v���v                        PIC X(1800)  VALUE SPACE.
001367 01 �E�v�������v.
          03 ���𕶂v                        OCCURS 20.
001370       05 �E�v���𕶂v                 PIC X(1800)  VALUE SPACE.
001370 01 ���͂v                             PIC X(1800)  VALUE SPACE.
       01 �K���S�T���v.
001403    03 �K���S�T���P�v                  PIC X(90) VALUE SPACE.
001403    03 �K���S�T���Q�v                  PIC X(90) VALUE SPACE.
001403    03 �K���S�T���R�v                  PIC X(90) VALUE SPACE.
001403    03 �K���S�T���S�v                  PIC X(90) VALUE SPACE.
001403    03 �K���S�T���T�v                  PIC X(90) VALUE SPACE.
005260 01 �U�������v.
005261    03 �����ی��Ҕԍ��v                PIC X(10)  VALUE SPACE.
005262    03 �����ی��Җ��v                  PIC X(100) VALUE SPACE.
005263    03 ���������ԍ��v                  PIC X(10)  VALUE SPACE.
005263    03 ���Z�@�փR�[�h�v                PIC X(8)  VALUE SPACE.
007370**************
007380* �{�p����� *
007390**************
007400 01 �{�p�����v.
007410    03 �_���t�ԍ��v                    PIC X(16)  VALUE SPACE.
007420    03 �ڍ��t�����ԍ��v              PIC X(16)  VALUE SPACE.
007430    03 ��\�҃J�i�v                    PIC X(50)  VALUE SPACE.
007440    03 ��\�Җ��v                      PIC X(50)  VALUE SPACE.
007450    03 �ڍ��@���v                      PIC X(50)  VALUE SPACE.
          03 �s���{���i�h�r�v                PIC X(2)   VALUE SPACE.
007460    03 �{�p���Z���v.
007470       05 �{�p���Z���P�v               PIC X(50)  VALUE SPACE.
007480       05 �{�p���Z���Q�v               PIC X(50)  VALUE SPACE.
007490    03 �{�p���X�֔ԍ��v.
007500       05 �{�p���X�֔ԍ��P�v           PIC X(3)   VALUE SPACE.
007510       05 �{�p���X�֔ԍ��Q�v           PIC X(4)   VALUE SPACE.
007520    03 �{�p���d�b�ԍ��v                PIC X(15)  VALUE SPACE.
007530    03 ��z���󗝔ԍ��v                PIC X(15)  VALUE SPACE.
007540    03 �󗝔N�����v.
007350       05 �󗝘a��v                   PIC 9     VALUE ZERO.
007550       05 �󗝔N�v                     PIC 9(2)   VALUE ZERO.
007560       05 �󗝌��v                     PIC 9(2)   VALUE ZERO.
007570       05 �󗝓��v                     PIC 9(2)   VALUE ZERO.
007580    03 �ŏI�ʉ@�N�����v.
007390       05 �ŏI�ʉ@�a��v               PIC 9     VALUE ZERO.
007590       05 �ŏI�ʉ@�N�v                 PIC 9(2)   VALUE ZERO.
007600       05 �ŏI�ʉ@���v                 PIC 9(2)   VALUE ZERO.
007610       05 �ŏI�ʉ@���v                 PIC 9(2)   VALUE ZERO.
007620    03 �_���t�N�����v.
007350       05 �_���t�a��v                 PIC 9      VALUE ZERO.
007630       05 �_���t�N�v                   PIC 9(2)   VALUE ZERO.
007640       05 �_���t���v                   PIC 9(2)   VALUE ZERO.
007650       05 �_���t���v                   PIC 9(2)   VALUE ZERO.
007660    03 ���҈ϔC�N�����v.
007350       05 ���҈ϔC�a��v               PIC 9      VALUE ZERO.
007670       05 ���҈ϔC�N�v                 PIC 9(2)   VALUE ZERO.
007680       05 ���҈ϔC���v                 PIC 9(2)   VALUE ZERO.
007690       05 ���҈ϔC���v                 PIC 9(2)   VALUE ZERO.
007700    03 �������v.
007710        05 ������s���v.
007720           07 ������s���P�v         PIC X(10)  VALUE SPACE.
007730           07 ������s���Q�v         PIC X(10)  VALUE SPACE.
007740           07 FILLER                   PIC X(20)  VALUE SPACE.
007750        05 ������s�x�X���v.
007760           07 ������s�x�X���P�v     PIC X(10)  VALUE SPACE.
007770           07 ������s�x�X���Q�v     PIC X(10)  VALUE SPACE.
007780           07 FILLER                   PIC X(20)  VALUE SPACE.
007790        05 �a����ʂv                  PIC 9(1)   VALUE ZERO.
007800        05 �����ԍ��v                  PIC X(10)  VALUE SPACE.
007810        05 �������`�l�v                PIC X(40)  VALUE SPACE.
007820        05 �������`�l�J�i�v            PIC X(40)  VALUE SPACE.
007830        05 �a����ʃR�����g�v          PIC N(3)   VALUE SPACE.
007840        05 �a����ʃR�����g�w�v        PIC X(4)   VALUE SPACE.
007850*
007860    03 ���{�p�h�c�v                    PIC X(15)  VALUE SPACE.
007870    03 �s�����{�p�h�c�v                PIC X(15)  VALUE SPACE.
007880    03 ���ϔԍ��v                      PIC X(28)  VALUE SPACE.
007880    03 �n���ϔԍ��v                    PIC X(28)  VALUE SPACE.
      *
008070 01 �����ʂv                         PIC 9(2)   VALUE ZERO.
008010 01 �s�����ԍ��v                       PIC X(10)  VALUE SPACE.
004510 01 ������ʂv�q                       PIC 9(2)  VALUE ZERO.
       01 �����b�m�s                         PIC 9(4)  VALUE ZERO.
003520 01 ��ؕ����q                         PIC X(2) VALUE X"0022".
003530 01 ��ؕ����v REDEFINES ��ؕ����q.
003540   03 ��ؕ����v�P                     PIC X(1).
003550   03 ��ؕ����v�Q                     PIC X(1).
       01 �{�p�J�n�N�����v                   PIC 9(7) VALUE ZERO.
       01 ����������v                       PIC X(100) VALUE SPACE.
       01 �A�b�g�}�[�N�v                     PIC X(1) VALUE X"40".
      */ZIP���k
       01  �p���P�v PIC X(2001) VALUE SPACE.
       01  �v���O�������v PIC X(8)  VALUE "zip".
005723 01 ���k�t�@�C�����v                   PIC X(60) VALUE SPACE.
005723 01 ���t�@�C�����v                     PIC X(60) VALUE SPACE.
      * C �A�g�p
       01 �t���p�X���v     PIC X(151) VALUE SPACE.
       01 �󔒋l�v                           PIC X(200) VALUE SPACE.
      */��ƃt�@�C���P���[�N
001530 01 ��P���R�[�h�v.
001540     03 ��P���R�[�h�L�[�v.
               05 ��P���Z�v�g�ԍ��v           PIC Z(4).
000520         05 ��P�{�p�a��N���v.
000530           07 ��P�{�p�a��v             PIC 9.
000540           07 ��P�{�p�N���v.
000550             09 ��P�{�p�N�v             PIC 9(2).
000560             09 ��P�{�p���v             PIC 9(2).
000570         05 ��P���҃R�[�h�v.
000580           07 ��P���Ҕԍ��v             PIC 9(6).
000590           07 ��P�}�Ԃv                 PIC X.
000510         05 ��P���Z��ʂv               PIC 9(2).
           03 ��P���R�[�h�f�[�^�v.
               05 ��P�{�p�Ҕԍ��v             PIC X(11).
               05 ��P�{�p����N���v           PIC X(6).
               05 ��P��o��敪�v             PIC X(1).
               05 ��P�ی��Ҕԍ��v             PIC X(8).
               05 ��P�ی��؋L���v             PIC X(16).
               05 ��P�ی��ؔԍ��v             PIC X(16).
               05 ��P����S�Ҕԍ��P�v       PIC X(8).
               05 ��P����󋋎Ҕԍ��P�v       PIC X(16).
               05 ��P����S�Ҕԍ��Q�v       PIC X(8).
               05 ��P����󋋎Ҕԍ��Q�v       PIC X(16).
               05 ��P�ی���ʑ�敪�v         PIC X(1).
               05 ��P�P���敪�v               PIC X(1).
               05 ��P�{�Ƌ敪�v               PIC X(1).
               05 ��P���t�����v               PIC 9(2).
               05 ��P�����v                   PIC X(1).
               05 ��P��ی��҃J�i�v           PIC X(25).
               05 ��P��ی��Җ��v             PIC X(30).
               05 ��P��ÎҖ��J�i�v           PIC X(25).
               05 ��P��ÎҖ��v               PIC X(30).
               05 ��P��ÎҐ��ʂv             PIC X(1).
               05 ��P��ÎҐ��N�����v         PIC X(8).
               05 ��P��ی��җX�֔ԍ��v       PIC X(7).
               05 ��P��ی��ҏZ���v           PIC X(60).
               05 ��P���v���z�v               PIC 9(6).
               05 ��P�ꕔ���S���v             PIC 9(6).
               05 ��P�������z�v               PIC 9(6).
               05 ��P����ꕔ���S�������z�v   PIC 9(6).
               05 ��P��������z�v           PIC 9(6).
               05 ��P���������v               PIC 9(2).
               05 ��P���ʐ��v                 PIC 9(1).
               05 ��P�Đ����敪�v             PIC X(1).
               05 ��P���S�敪�v               PIC X(1).
               05 ��P���S�����v               PIC 9(1).
               05 ��P���Ҕԍ��Q�v             PIC 9(6).
               05 ��P���Ҕԍ��}�Ԃv           PIC 9(1).
               05 ��P�����f�[�^�v OCCURS 6.
                   07 ��P�����i���o�[�v       PIC 9(1).
                   07 ��P�����敪�v           PIC X(1).
                   07 ��P�����R�[�h�v         PIC X(8).
                   07 ��P�������v             PIC X(32).
                   07 ��P�����N�����v         PIC X(8).
                   07 ��P�����N�����v         PIC X(8).
                   07 ��P�{�p�J�n���v         PIC X(8).
                   07 ��P�{�p�I�����v         PIC X(8).
                   07 ��P�������v             PIC X(2).
                   07 ��P���ʎ{�p���v.
                      09 ��P�{�p���v          PIC 9(2) OCCURS 31.
                   07 ��P�]�A�敪�v           PIC X(1).
                   07 ��P���Ŏ{�敪�v         PIC X(1).
                   07 ��P���Ŏ{�񐔂v         PIC 9(1).
                   07 ��P���Ŏ{�×��v         PIC 9(5).
                   07 ��P���������v           PIC X(200).
                   07 ��P�������R�v           PIC X(200).
                   07 ��P�����p�񗝗R�v       PIC X(200).
                   07 ��P�o�߂v               PIC X(200).
                   07 ��P�R���敪�v           PIC X(1).
               05 ��P�����敪�v               PIC X(1).
               05 ��P�����񐔂v               PIC 9(1).
               05 ��P�������v                 PIC 9(5).
               05 ��P�����x�����Z�񐔂v       PIC 9(1).
               05 ��P�����[����Z�񐔂v       PIC 9(1).
               05 ��P�������ԊO���Z�񐔂v     PIC 9(1).
               05 ��P�������Z�v               PIC 9(5).
               05 ��P���������k�x�����񐔂v   PIC 9(1).
               05 ��P���������k�x�����v       PIC 9(5).
               05 ��P�Č��񐔂v               PIC 9(1).
               05 ��P�Č����v                 PIC 9(5).
               05 ��P���Ë����v               PIC 9(3).
               05 ��P���É񐔂v               PIC 9(2).
               05 ��P���×��v                 PIC 9(5).
               05 ��P���×��R�v               PIC X(200).
               05 ��P��ԉ��Z�̉��É񐔂v     PIC 9(2).
               05 ��P��H���Z�̉��É񐔂v     PIC 9(2).
               05 ��P�\���J����Z�̉��É񐔂v PIC 9(2).
               05 ��P���É��Z�v               PIC 9(5).
               05 ��P�������q��񐔂v         PIC 9(2).
               05 ��P�������q���񐔂v         PIC 9(1).
               05 ��P�������q���񐔂v         PIC 9(1).
               05 ��P�������q���Z�v           PIC 9(5).
               05 ��P�������q���Z���v         PIC 9(2) OCCURS 3.
               05 ��P�{�p���񋟗��̉񐔂v   PIC 9(1).
               05 ��P�{�p���񋟗��v         PIC 9(5).
               05 ��P�^����Î��{�񐔂v       PIC 9(1).
               05 ��P�^����×��v             PIC 9(5).
               05 ��P�^����Î��{���v         PIC 9(2) OCCURS 5.
               05 ��P���ʕʗ����f�[�^�v OCCURS 10.
                   07 ��P�s�ԍ��v             PIC 9(1).
                   07 ��P�����J�n�����v       PIC X(4).
                   07 ��P��É񐔂v           PIC 9(2).
                   07 ��P��×��v             PIC 9(5).
                   07 ��P��㪖@�񐔂v         PIC 9(1).
                   07 ��P��㪖@���v           PIC 9(5).
                   07 ��P��㪖@�񐔂v         PIC 9(2).
                   07 ��P��㪖@���v           PIC 9(5).
                   07 ��P�d�É񐔂v           PIC 9(2).
                   07 ��P�d�×��v             PIC 9(5).
                   07 ��P�����ʒ������v       PIC 9(2).
                   07 ��P�����ʒ����z�v       PIC 9(5).
                   07 ��P�����������v         PIC 9(2).
                   07 ��P���ʒ������ʗ����v�v PIC 9(5).
               05 ��P�E�v�v                   PIC X(400).
               05 ��P���ӔN�����v             PIC X(8).
               05 ��P���ӈ�@�v               PIC X(40).
               05 ��P���ӈ�t���v             PIC X(20).
               05 ��P�ی���ʏڍׂv           PIC X(2).
               05 ��P���׏����s���Z���v       PIC 9(2).
               05 ��P���׏����s���Z���v       PIC 9(2).
      *
       01 �������l�w�v.
           03 �������l�v                       PIC 9(6).
       01 �������l�Q�v                         PIC X(6).
      *
       01 ���׏��e                             PIC 9(1) VALUE ZERO.
       01 �s�ԍ����݂e                         PIC 9(1) VALUE ZERO.
       01 ���ה��s���Z�敪�v                   PIC 9(1) VALUE ZERO.
       01 ���׏����s�񐔂v                     PIC 9(1) VALUE ZERO.
004010*�������J�E���g�p/20230413
004020 01 �����`�F�b�N�s�a�k.
004030    03 �����`�F�b�N�v                  PIC X(1)  OCCURS 120.
004040    03 ���v                            PIC 9(3) VALUE ZERO.
004050    03 �]�v                            PIC 9(3) VALUE ZERO.
004060*
004230 01 �J�E���^�P                         PIC 9(3)  VALUE ZERO.
004240 01 �J�E���^�Q                         PIC 9(3)  VALUE ZERO.
004250 01 �J�E���^�R                         PIC 9(3)  VALUE ZERO.
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
005860*
004091 01 �A���V�|�L�[ IS EXTERNAL.
004092    03  �A���V�|���b�Z�[�W�P           PIC X(40).
004102    03  �A���V�|���b�Z�[�W�Q           PIC X(40).
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
      *
      *0:�ʏ� 1:�Œ�p�X
       01 �A���|�Œ�t���O�R�Q�P�P IS EXTERNAL.
          03 �A���|�Œ�t���O                PIC 9(1).
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
           PERFORM ���я��ݒ�.
006380     PERFORM ������.
006390************
006400*           *
006410* �又��     *
006420*           *
006430************
006440     PERFORM ������擾.
006450     PERFORM �A�����ڑޔ�.
010020     PERFORM ��ƃt�@�C���P�쐬.
           PERFORM ���o�t�@�C���쐬.
006500************
006510*           *
006520* �I������   *
006530*           *
006540************
006550     PERFORM �I������.
      *
           PERFORM ���k�t�@�C���쐬.
      *
006560     MOVE ZERO TO PROGRAM-STATUS.
006570     EXIT PROGRAM.
006580*
006590*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
006600*================================================================*
006610 ������ SECTION.
006620*
006630     PERFORM �t�@�C���I�[�v��.
006640*
006650**================================================================*
006660 �t�@�C���I�[�v�� SECTION.
006670*
            MOVE "C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT.json" TO ���t�@�C�����v
            MOVE "delfile" TO �v���O�������v.
            CALL �v���O�������v WITH C LINKAGE
                    USING BY REFERENCE ���t�@�C�����v.
            MOVE "C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT1.json" TO ���t�@�C�����v
            MOVE "delfile" TO �v���O�������v.
            CALL �v���O�������v WITH C LINKAGE
                    USING BY REFERENCE ���t�@�C�����v.
            MOVE "C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT3.json" TO ���t�@�C�����v
            MOVE "delfile" TO �v���O�������v.
            CALL �v���O�������v WITH C LINKAGE
                    USING BY REFERENCE ���t�@�C�����v.
006680     OPEN INPUT ��f�ҏ��e
006690         MOVE NC"��" TO �t�@�C����.
006700         PERFORM �I�[�v���`�F�b�N.
006630     OPEN INPUT ���Z�v�g�e.
006640         MOVE NC"���Z" TO �t�@�C����.
006650         PERFORM �I�[�v���`�F�b�N.
006770     OPEN INPUT �{�p�����}�X�^
006780         MOVE NC"�{��" TO �t�@�C����.
006790         PERFORM �I�[�v���`�F�b�N.
006800     OPEN INPUT �����}�X�^.
006810             MOVE NC"����" TO �t�@�C����.
006820             PERFORM �I�[�v���`�F�b�N.
006710     OPEN INPUT �����f�[�^�e.
006720             MOVE NC"����" TO �t�@�C����.
006730             PERFORM �I�[�v���`�F�b�N.
006740     OPEN INPUT �{�p�L�^�e.
006750             MOVE NC"�{�L" TO �t�@�C����.
006760             PERFORM �I�[�v���`�F�b�N.
004140     OPEN INPUT  ���̃}�X�^
004150         MOVE NC"����" TO �t�@�C����.
004160         PERFORM �I�[�v���`�F�b�N.
014910     OPEN INPUT   ���������e.
014920         MOVE NC"��������" TO �t�@�C����.
014930         PERFORM �I�[�v���`�F�b�N.
002650     OPEN INPUT   �����p���҂e.
002651         MOVE NC"�����p���҂e" TO �t�@�C����.
002652         PERFORM �I�[�v���`�F�b�N.
           OPEN INPUT �v�Z�}�X�^.
               MOVE NC"�v�Z" TO �t�@�C����.
               PERFORM �I�[�v���`�F�b�N.
014970     OPEN INPUT �E�v�t�@�C��.
014980         MOVE NC"�E�v" TO �t�@�C����.
014990         PERFORM �I�[�v���`�F�b�N.
006830     OPEN INPUT �ی��҃}�X�^.
006840             MOVE NC"�ی��҃}�X�^" TO �t�@�C����.
006850             PERFORM �I�[�v���`�F�b�N.
006920     OPEN INPUT �s�����}�X�^.
006930             MOVE NC"�s��" TO �t�@�C����.
006940             PERFORM �I�[�v���`�F�b�N.
015080     OPEN INPUT   �h�c�Ǘ��}�X�^
015090         MOVE NC"�h�c" TO �t�@�C����.
015100         PERFORM �I�[�v���`�F�b�N.
015170     OPEN INPUT  ��ƃt�@�C���S.
015170         IF ( ��ԃL�[  NOT =  "00" )
015060            OPEN OUTPUT  ��ƃt�@�C���S
                  CLOSE ��ƃt�@�C���S
015060            OPEN INPUT  ��ƃt�@�C���S
               END-IF.
006860     OPEN INPUT ������}�X�^.
006870             MOVE NC"����" TO �t�@�C����.
006880             PERFORM �I�[�v���`�F�b�N.
007091*
007094* �t���b�s�[�}���m�F
007095* �_�~�[�t�@�C�����I�[�v�����A�I�[�v���ł�����t�@�C�����폜
           IF �A���|�Œ�t���O = 1
002890        MOVE "C:\makishisys\REZEPT1.json" TO FD-NAME
           ELSE
003324        MOVE �A���|�h���C�u TO �h���C�u�v�q
007096        STRING �h���C�u�v�q       DELIMITED BY SIZE
007097               ":\REZEPT1.json"    DELIMITED BY SIZE
007098               INTO FD-NAME
007099        END-STRING
           END-IF.
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
007160**================================================================*
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
007270**================================================================*
007280 �t�@�C���� SECTION.
007290*
           CLOSE ��f�ҏ��e �{�p�����}�X�^ ���Z�v�g�e �����}�X�^ �����f�[�^�e 
           �{�p�L�^�e ���������e ���̃}�X�^ �����p���҂e �v�Z�}�X�^ �E�v�t�@�C�� 
           �ی��҃}�X�^ �s�����}�X�^ �h�c�Ǘ��}�X�^ ������}�X�^ ��ƃt�@�C���S.
007340**================================================================*
007350 �I������ SECTION.
007360*
007370     PERFORM �t�@�C����.
007380**================================================================*
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
009500**================================================================*
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
009810**================================================================*
009820 �A�����ڑޔ� SECTION.
009830*
009840     MOVE �A���|�����a�� TO �����a��v�q.
009850     MOVE �A���|�����N   TO �����N�v�q.
009860     MOVE �A���|������   TO �������v�q.
009860     MOVE �A���|�h���C�u TO �h���C�u�v�q.
030740**================================================================*
030750 �J�n���擾 SECTION.
030760*
030830     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > ���|���ʐ�
030840         IF ( ��|�{�p�N = ���|�J�n�N(���ʂb�m�s) ) AND
030850            ( ��|�{�p�� = ���|�J�n��(���ʂb�m�s) )
030860             MOVE ��|���Ҕԍ�              TO �{�L�|���Ҕԍ�
030870             MOVE ��|�}��                  TO �{�L�|�}��
030890             MOVE ���|�J�n�a��(���ʂb�m�s)  TO �J�n�a��v(���ʂb�m�s) �{�L�|�{�p�a��
030890             MOVE ���|�J�n�N(���ʂb�m�s)  TO �J�n�N�v(���ʂb�m�s) �{�L�|�{�p�N
030900             MOVE ���|�J�n��(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
030910             MOVE ���|�J�n��(���ʂb�m�s)  TO �J�n���v(���ʂb�m�s) �{�L�|�{�p��
030920         ELSE
030930             MOVE ��|���Ҕԍ�          TO �{�L�|���Ҕԍ�
030940             MOVE ��|�}��              TO �{�L�|�}��
030950             MOVE ��|�{�p�a��          TO �{�L�|�{�p�a��
030960             MOVE ��|�{�p�N            TO �{�L�|�{�p�N
030970             MOVE ��|�{�p��            TO �{�L�|�{�p��
030980             MOVE ZERO                  TO �{�L�|�{�p��
030990         END-IF
031000         START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
031010                                      �{�L�|�{�p�a��N����
031020         END-START
031030         IF ( ��ԃL�[ = "00" )
031490             MOVE ZERO  TO �I���a��v�s
031050             MOVE ZERO  TO �I���N�v�s
031060             MOVE ZERO  TO �I�����v�s
031070             MOVE ZERO  TO �I�����v�s

      */�I�����ɒʉ@���ݒ肳��Ė����ꍇ�ɓ]�L����Ȃ��א�ɓ]�L����B
                   IF ���|�]�A�敪(���ʂb�m�s) NOT = ZERO
031490                 MOVE ���|�I���a��(���ʂb�m�s) TO �I���a��v�s
031050                 MOVE ���|�I���N  (���ʂb�m�s) TO �I���N�v�s
031060                 MOVE ���|�I����  (���ʂb�m�s) TO �I�����v�s
031070                 MOVE ���|�I����  (���ʂb�m�s) TO �I�����v�s
                   END-IF


031080             MOVE SPACE TO �I���t���O�Q
031090             PERFORM �{�p�L�^�e�Ǎ�
031100             IF ( �I���t���O�Q      = SPACE   ) AND
031110                ( �{�L�|���҃R�[�h  = ��|���҃R�[�h ) AND
031120                ( �{�L�|�{�p�a��    = ��|�{�p�a��   ) AND
031130                ( �{�L�|�{�p�N      = ��|�{�p�N     ) AND
031140                ( �{�L�|�{�p��      = ��|�{�p��     ) 
031150*
031160*        *****************************************************************
031170*        * �J�n�N���� ( ���̕��ʂ����������łȂ����A
031180*                       ���������ł��}�Ԃ����鎞�́A�ŏ��̎{�p�����J�n��)*
031190*        *****************************************************************
031200                 IF ( ��|�{�p�N NOT = ���|�J�n�N(���ʂb�m�s) ) OR
031210                    ( ��|�{�p�� NOT = ���|�J�n��(���ʂb�m�s) )
      */20240322
031220                 OR ( ���|�J�n�f�Ó��蓮�敪 = 1 )
031230                     MOVE �{�L�|�{�p�a�� TO �J�n�a��v(���ʂb�m�s)
031230                     MOVE �{�L�|�{�p�N   TO �J�n�N�v(���ʂb�m�s)
031240                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
031250                     MOVE �{�L�|�{�p��   TO �J�n���v(���ʂb�m�s)
031260                 END-IF
031270             END-IF
                   IF ���|�I����(���ʂb�m�s) = ZERO
                       MOVE 99                     TO �I�����v�q
                   ELSE
                       MOVE ���|�I����(���ʂb�m�s) TO �I�����v�q
                   END-IF
031280             PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
031290                           ( �{�L�|���҃R�[�h NOT = ��|���҃R�[�h   ) OR
031300                           ( �{�L�|�{�p�a��   NOT = ��|�{�p�a��     ) OR
031310                           ( �{�L�|�{�p�N     NOT = ��|�{�p�N       ) OR
031320                           ( �{�L�|�{�p��     NOT = ��|�{�p��       ) OR
031330                           ( �{�L�|�{�p��         > �I�����v�q)
031380                 MOVE �{�L�|�{�p�a��             TO �I���a��v�s
031380                 MOVE �{�L�|�{�p�N               TO �I���N�v�s
031390                 MOVE �{�L�|�{�p��               TO �I�����v�s
031400                 MOVE �{�L�|�{�p��               TO �I�����v�s
031410*
031420                 PERFORM �{�p�L�^�e�Ǎ�
031430             END-PERFORM
031440         END-IF
031450*       **************************
031460*       * �p���F�I���N�����Z�b�g *
031470*       **************************
031490         MOVE �I���a��v�s  TO �I���a��v(���ʂb�m�s)
031490         MOVE �I���N�v�s    TO �I���N�v(���ʂb�m�s)
031500         MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
031510         MOVE �I�����v�s    TO �I�����v(���ʂb�m�s)
031580     END-PERFORM.
016650**================================================================*
016660 �{�p�L�^�e�Ǎ� SECTION.
016670*
016680     READ �{�p�L�^�e NEXT
016690     AT END
016700         MOVE "YES" TO �I���t���O�Q
016710     END-READ.
022430**================================================================*
022440 �x������ SECTION.
022450*
022460     PERFORM VARYING �x���J�E���^ FROM 1 BY 1
022470             UNTIL �x���J�E���^ > �x���񐔂v
022480         MOVE "YES" TO �x���t���O
022490     END-PERFORM.
022500*
022510**================================================================*
022520 ������擾 SECTION.
022530*
022540     MOVE ZERO TO ���|����敪
022550     READ ������}�X�^
022560     NOT INVALID KEY
022570         MOVE ���|�x���� TO �x���񐔂v
022580     END-READ.
022841*
           MOVE 01   TO ���O�P�|����敪.
           READ ������}�X�^
           NOT INVALID KEY
               MOVE ���O�P�|���ה��s���Z�敪 TO ���ה��s���Z�敪�v
           END-READ.
      *
012420*================================================================*
011330 ��ƃt�@�C���P�쐬 SECTION.
011340*
011344     OPEN OUTPUT ��ƃt�@�C���P.
011345         MOVE NC"��P" TO �t�@�C����.
011346         PERFORM �I�[�v���`�F�b�N.
011344     OPEN OUTPUT ��ƃt�@�C���Q.
011345         MOVE NC"��Q" TO �t�@�C����.
011346         PERFORM �I�[�v���`�F�b�N.
           CLOSE ��ƃt�@�C���P.
           CLOSE ��ƃt�@�C���Q.
026580     OPEN I-O ��ƃt�@�C���P.
026590         MOVE NC"��P" TO �t�@�C����.
026600         PERFORM �I�[�v���`�F�b�N.
026580     OPEN I-O ��ƃt�@�C���Q.
026590         MOVE NC"��Q" TO �t�@�C����.
026600         PERFORM �I�[�v���`�F�b�N.
026710*
           MOVE ZERO TO �{�p�J�n�N�����v.
      *
005420     MOVE �����a��v�q  TO ���Z�|�����a��.
005430     MOVE �����N�v�q    TO ���Z�|�����N.
005440     MOVE �������v�q    TO ���Z�|������.
005450     MOVE 1             TO ���Z�|�{�p�a��.
005460     MOVE ZERO          TO ���Z�|�{�p�N.
005470     MOVE ZERO          TO ���Z�|�{�p��.
005480     MOVE ZERO          TO ���Z�|���Ҕԍ�.
005490     MOVE LOW-VALUE     TO ���Z�|�}��.
005500     MOVE ZERO          TO ���Z�|���Z���.
005510     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
005520                                  ���Z�|�{�p�a��N��
005530                                  ���Z�|���҃R�[�h
005540                                  ���Z�|���Z���
026830     END-START.
026840     IF ��ԃL�[ = "00"
026850         MOVE SPACE  TO �I���t���O
005580         PERFORM ���Z�v�g�e�Ǎ�
005590         PERFORM UNTIL ( �I���t���O = "YES" ) OR
005600                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
005610                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
005620                       ( ���Z�|������   NOT = �������v�q   )
026910*
                   IF ���Z�|���Z��� = 1 OR 2 OR 3 OR 7
026920                 PERFORM �f�[�^�`�F�b�N
026930                 IF ���s�L�[�v = "YES"
005930*/ �����Ő����z�O�~�͑ΏۊO�ɂ��� /170411 �Ȃ�
006880*                    IF (���Z�|���Z��� = 3) AND (���Z�|�����������z = ZERO)
      *                       CONTINUE
      *                    ELSE
                             PERFORM ��P���R�[�h�Z�b�g
                             PERFORM ��P�t�@�C������
      *                    END-IF
                       END-IF
005200             END-IF
027850             PERFORM ���Z�v�g�e�Ǎ�
027860         END-PERFORM
027870     END-IF.
027880     CLOSE ��ƃt�@�C���P ��ƃt�@�C���Q.
012420*================================================================*
012910 ���Z�v�g�e�Ǎ� SECTION.
012920*
012930     READ ���Z�v�g�e NEXT
012940     AT END
012950         MOVE "YES" TO �I���t���O
012960     END-READ.
012970*
012980*================================================================*
       ��P���R�[�h�Z�b�g SECTION.
      *
           INITIALIZE ��P�|���R�[�h.
           PERFORM ���Z�v�g���я��擾.
007520     PERFORM �{�p�����擾.
022340     PERFORM �����f�[�^�擾.
           PERFORM �����p���҂e�Ǎ�.
      */�J�n���I�����擾
           PERFORM �J�n���擾.
           PERFORM �{�p�L�^����.
           PERFORM �v�Z���擾.
      *
032020     MOVE ��S�|����                     TO ��P�|���Z�v�g�ԍ�.
000520     MOVE ��|�{�p�a��N��               TO ��P�|�{�p�a��N��.
000570     MOVE ��|���҃R�[�h                 TO ��P�|���҃R�[�h.
000510     MOVE ���Z�|���Z���                 TO ��P�|���Z���.
           MOVE �{��|�V�_���t�ԍ�(3:11)       TO ��P�|�{�p�Ҕԍ�.
010240* ������N���̎擾
010250     MOVE ZERO          TO ����N���v  ������N���v.
010260     MOVE �����a��v�q  TO ���|�����敪.
010270     READ �����}�X�^
           INVALID KEY
010330          MOVE  NC"�����}�X�^�̓ǂݍ��݂Ɏ��s���܂����B" TO �A���|���b�Z�[�W
010340          CALL   "MSG001"
010350          CANCEL "MSG001"
                DISPLAY ��|���҃R�[�h " " ��|�{�p�a��N�� " �����敪:" ���|�����敪
010360          PERFORM �t�@�C����
010370          MOVE 99 TO PROGRAM-STATUS
010380          EXIT PROGRAM
010280     NOT INVALID KEY
010290         MOVE ���|�J�n����N TO ����N�v
010300     END-READ.
010310*
010320     IF ����N�v = ZERO
010330          MOVE  NC"�����}�X�^�ɊJ�n����N��o�^���ĉ�����" TO �A���|���b�Z�[�W
010340          CALL   "MSG001"
010350          CANCEL "MSG001"
010360          PERFORM �t�@�C����
010370          MOVE 99 TO PROGRAM-STATUS
010380          EXIT PROGRAM
010390     ELSE
010400          COMPUTE ����N�v = ����N�v + �����N�v�q - 1
010410          MOVE �������v�q TO ����v
010420     END-IF.
010430*
010440*     MOVE ����N���v   TO  ������N���v.
010450*
      */�{�p�N����]�L���遫����/20190817
010440*     MOVE ����N���v    TO  ��P�|�{�p����N��.
           MOVE ��|�{�p�a��N�� TO �a��v�Z�N�����v
           PERFORM ����N�����擾
           MOVE ����v�Z�N���v   TO  ��P�|�{�p����N��.
      */�{�p�N����]�L���遪����/20190817
           IF ���Z�|���Z��� = 3
               MOVE 3                         TO ��P�|��o��敪
           ELSE
               MOVE 1                         TO ��P�|��o��敪
           END-IF.
           MOVE ��|�ی��Ҕԍ�                TO ��P�|�ی��Ҕԍ�.
013031*-----------------------------------------------------------------*
013032     MOVE SPACE TO �A�Í������|�Í����.
013033*
013034*    / �A�Í������|���͏��Z�b�g /
013035     MOVE ��|�L��       TO �A�Í������|�L��.
013036     MOVE ��|�ԍ�       TO �A�Í������|�ԍ�.
013037     MOVE ��|�Í������� TO �A�Í������|�Í�������.
013038*
013039     CALL   �����v���O�������v.
013040     CANCEL �����v���O�������v.
013041*
013042*-----------------------------------------------------------------*
013296*/�ی��؋L���ԍ�
013297     IF �A�Í������|���������L��(1:2) NOT = "��"
013300         MOVE �A�Í������|���������L��               TO ��P�|�ی��؋L��
013310     END-IF.
013320     IF (�A�Í������|���������ԍ�(1:1) NOT = "*") AND (�A�Í������|���������ԍ�(1:2) NOT = "��")
013330         MOVE �A�Í������|���������ԍ�               TO ��P�|�ی��ؔԍ�
013340     END-IF.
001110     MOVE ��|��p���S�Ҕԍ�����        TO ��P�|����S�Ҕԍ��P
001180     MOVE ��|��v�Ҕԍ�����            TO ��P�|����󋋎Ҕԍ��P
      *          ��P�|����S�Ҕԍ��Q       
      *          ��P�|����󋋎Ҕԍ��Q       
           EVALUATE ��|�ی����
           WHEN 01
               MOVE "4"                       TO ��P�|�ی���ʑ�敪
           WHEN 02
           WHEN 06
           WHEN 07
               MOVE "1"                       TO ��P�|�ی���ʑ�敪
           WHEN 03
               MOVE "2"                       TO ��P�|�ی���ʑ�敪
           WHEN 04
           WHEN 09
               MOVE "3"                       TO ��P�|�ی���ʑ�敪
           WHEN 05
               MOVE "6"                       TO ��P�|�ی���ʑ�敪
           WHEN 08
               MOVE "5"                       TO ��P�|�ی���ʑ�敪
           WHEN OTHER
               MOVE "9"                       TO ��P�|�ی���ʑ�敪
           END-EVALUATE.
           IF ��|������� = ZERO
               MOVE "1"                       TO ��P�|�P���敪
           ELSE
               MOVE "2"                       TO ��P�|�P���敪
           END-IF
      */�{�Ƌ敪�͂ǂꂩ�P�Ɂ�������B
           IF ��|�ی���� = 05
               EVALUATE ��|���ʋ敪
               WHEN 1
               WHEN 2
      *             MOVE NC"��" TO ����`�F�b�N�v
                   MOVE "8"      TO ��P�|�{�Ƌ敪 
               WHEN 3
      *             MOVE NC"��" TO ���V�`�F�b�N�v
                   MOVE "0"      TO ��P�|�{�Ƌ敪 
               END-EVALUATE
           ELSE
028984         EVALUATE ��|���ʋ敪
               WHEN 1
               WHEN 2
      *             MOVE NC"��" TO ����`�F�b�N�v
                   MOVE "8"      TO ��P�|�{�Ƌ敪 
               WHEN 3
      *             MOVE NC"��" TO ���V�`�F�b�N�v
                   MOVE "0"      TO ��P�|�{�Ƌ敪 
028991         WHEN 6
      *             MOVE NC"��" TO �U�΃`�F�b�N�v
                   MOVE "4"      TO ��P�|�{�Ƌ敪 
               WHEN OTHER
                   IF ��|�{�l�Ƒ��敪 = 1
      *                 MOVE NC"��" TO �{�l�`�F�b�N�v
                       MOVE "2"      TO ��P�|�{�Ƌ敪 
                   ELSE
      *                 MOVE NC"��" TO �Ƒ��`�F�b�N�v
                       MOVE "6"      TO ��P�|�{�Ƌ敪 
                   END-IF
028999         END-EVALUATE
           END-IF.
           MOVE ���Z�|���t���� TO ��P�|���t����.
           IF ��|�{�l�Ƒ��敪 = 1
               MOVE "1" TO ��P�|����
           ELSE
               MOVE "2" TO ��P�|����
           END-IF.
001680     MOVE ��|��ی��҃J�i              TO ��P�|��ی��҃J�i.
001670     MOVE ��|��ی��Ҏ���              TO ��P�|��ی��Җ�  .
000870     MOVE ��|���҃J�i                  TO ��P�|��ÎҖ��J�i.
000860     MOVE ��|���Ҏ���                  TO ��P�|��ÎҖ�    .
000880     IF ��|���Ґ��� = 1
               MOVE "1"                       TO ��P�|��ÎҐ���
           ELSE
               MOVE "2"                       TO ��P�|��ÎҐ���
           END-IF.
010240* ����N���̎擾
           MOVE ��|���Ґ��N����              TO �a��v�Z�N�����v.
           PERFORM ����N�����擾.
           MOVE ����v�Z�N�����v              TO ��P�|��ÎҐ��N����.
001750     MOVE ��|�X�֔ԍ�                  TO ��P�|��ی��җX�֔ԍ�.
022720     STRING ��|�Z���P DELIMITED BY SPACE
022730            ��|�Z���Q DELIMITED BY SPACE
022750            INTO ��P�|��ی��ҏZ��
022760     END-STRING.
013173     MOVE ���Z�|���v             TO ��P�|���v���z. 
013174     MOVE ���Z�|�ꕔ���S��       TO ��P�|�ꕔ���S��.
013175     MOVE ���Z�|�������z         TO ��P�|�������z  .
013174     MOVE ���Z�|�󋋎ҕ��S�z     TO ��P�|����ꕔ���S�������z.
013175     MOVE ���Z�|�����������z     TO ��P�|��������z.
000730     MOVE ���Z�|���Z������       TO ��P�|��������.
000310     MOVE ���|���ʐ�             TO ��P�|���ʐ�.
000550     IF ���Z�|�����敪 = ZERO
              MOVE "0"                 TO ��P�|�Đ����敪
           ELSE
              MOVE "1"                 TO ��P�|�Đ����敪
           END-IF.
           IF ��|�ی���� = 05
               MOVE "0" TO ��P�|���S�敪
           ELSE
               EVALUATE ��|���ʋ敪
               WHEN 1
               WHEN 2
               WHEN 3
                   MOVE "1" TO ��P�|���S�敪
               WHEN 6
                   MOVE "6" TO ��P�|���S�敪
               END-EVALUATE
           END-IF.
000790     MOVE ���Z�|���S����        TO ��P�|���S����.
000480     MOVE ���Z�|���Ҕԍ�        TO ��P�|���Ҕԍ��Q.
           MOVE "0"                   TO ��P�|���Ҕԍ��}��.
           PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL
              (���ʂb�m�s > ���|���ʐ�) OR (���ʂb�m�s > 6)
               MOVE ���ʂb�m�s TO ��P�|�����i���o�[(���ʂb�m�s)
000340         EVALUATE ���|�������(���ʂb�m�s)
               WHEN 01
                   MOVE "5" TO ��P�|�����敪(���ʂb�m�s)
               WHEN 02
                   MOVE "4" TO ��P�|�����敪(���ʂb�m�s)
               WHEN 03
                   MOVE "6" TO ��P�|�����敪(���ʂb�m�s)
               WHEN 04
                   MOVE "3" TO ��P�|�����敪(���ʂb�m�s)
               WHEN 05
                   MOVE "1" TO ��P�|�����敪(���ʂb�m�s)
               WHEN 06
                   MOVE "2" TO ��P�|�����敪(���ʂb�m�s)
               WHEN 07
               WHEN 08
                   MOVE "7" TO ��P�|�����敪(���ʂb�m�s)
               WHEN 09
                   MOVE "0" TO ��P�|�����敪(���ʂb�m�s)
               END-EVALUATE
               MOVE SPACE TO ��P�|�����R�[�h(���ʂb�m�s)
022530* �������
022540         MOVE SPACE                     TO �������̂v
022550         MOVE 03                        TO ���|�敪�R�[�h
022560         MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
022570         READ ���̃}�X�^
022580         INVALID KEY
022590             MOVE SPACE        TO �������̂v
022600         NOT INVALID KEY
022610             MOVE ���|�������� TO �������̂v
022620         END-READ
022630* ����
022720         STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
022730                �������̂v                    DELIMITED BY SPACE
022740                ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
022750                INTO �������v(���ʂb�m�s)
022760         END-STRING
022780*
022750         MOVE �������v�q(���ʂb�m�s) TO ��P�|������(���ʂb�m�s)
               INSPECT ��P�|������(���ʂb�m�s)   REPLACING ALL "�@" BY "  "
HILO  *         DISPLAY ��|���R�[�h�L�[ " ��P�|������(" ���ʂb�m�s ") " ��P�|������(���ʂb�m�s)
      */������
               MOVE ���|�����a��N����(���ʂb�m�s)   TO �a��v�Z�N�����v
               PERFORM ����N�����擾
               MOVE ����v�Z�N�����v                 TO ��P�|�����N����(���ʂb�m�s)
      */�_�̊J�n��
               MOVE ���|�J�n�a��N����(���ʂb�m�s)   TO �a��v�Z�N�����v
               PERFORM ����N�����擾
               MOVE ����v�Z�N�����v                 TO ��P�|�����N����(���ʂb�m�s)
      */�����̊J�n��
               MOVE �J�n�a��N�����v(���ʂb�m�s)     TO �a��v�Z�N�����v
               PERFORM ����N�����擾
               MOVE ����v�Z�N�����v                 TO ��P�|�{�p�J�n��(���ʂb�m�s)
      */�I�����܂��͍ŏI�ʉ@��
                  MOVE �I���a��N�����v(���ʂb�m�s)  TO �a��v�Z�N�����v
                  PERFORM ����N�����擾
                  MOVE ����v�Z�N�����v              TO ��P�|�{�p�I����(���ʂb�m�s)
HILO  *         DISPLAY "(" ���ʂb�m�s ") ����" ��P�|�����N����(���ʂb�m�s) " ����" ��P�|�����N����(���ʂb�m�s) 
HILO  *                                 " �J�n" ��P�|�{�p�J�n��(���ʂb�m�s) " �I��" ��P�|�{�p�I����(���ʂb�m�s)
000690          MOVE ���Z�|���ʎ�����(���ʂb�m�s)    TO ��P�|������(���ʂb�m�s)
               EVALUATE ���|�]�A�敪(���ʂb�m�s)
               WHEN 1
               WHEN 2
               WHEN 5
                   MOVE "1" TO ��P�|�]�A�敪(���ʂb�m�s)
               WHEN 3
                   MOVE "2" TO ��P�|�]�A�敪(���ʂb�m�s)
               WHEN 4
                   MOVE "3" TO ��P�|�]�A�敪(���ʂb�m�s)
               WHEN 9
                   MOVE "0" TO ��P�|�]�A�敪(���ʂb�m�s)
               END-EVALUATE
      */�������i���܁E�E�P�j�Œ藿�i�s�S���܁j�{�×��i�Ŗo�A�P���E�����j
               IF ���|�Z��敪(���ʂb�m�s) = ZERO
                   EVALUATE ���|�������(���ʂb�m�s)
                   WHEN 01
                   WHEN 02
                   WHEN 03
                       MOVE "3" TO ��P�|���Ŏ{�敪(���ʂb�m�s)
                   WHEN 04
                   WHEN 05
                       MOVE "1" TO ��P�|���Ŏ{�敪(���ʂb�m�s)
                   WHEN 06
                       MOVE "2" TO ��P�|���Ŏ{�敪(���ʂb�m�s)
                   WHEN 07
                   WHEN 08
                   WHEN 09
                       MOVE "0" TO ��P�|���Ŏ{�敪(���ʂb�m�s)
                   END-EVALUATE
               ELSE
                   MOVE "0" TO ��P�|���Ŏ{�敪(���ʂb�m�s)
               END-IF
               MOVE ���񏈒u�񐔂v(���ʂb�m�s) TO ��P�|���Ŏ{��(���ʂb�m�s)
HILO  *         DISPLAY "��P�|���Ŏ{��(" ���ʂb�m�s ")" ��P�|���Ŏ{��(���ʂb�m�s)
               MOVE ���Z�|���񏈒u��(���ʂb�m�s) TO ��P�|���Ŏ{�×�(���ʂb�m�s)
      */��������
               MOVE 01                             TO �����|�敪�R�[�h
               MOVE ���|���������R�[�h(���ʂb�m�s) TO �����|���������R�[�h
               READ ���������e
               NOT INVALID KEY
                   MOVE SPACE TO �󔒋l�v
                   STRING �����|���������b�l(1) DELIMITED BY SIZE
                          �����|���������b�l(2) DELIMITED BY SIZE
                          �����|���������b�l(3) DELIMITED BY SIZE
                          �����|���������b�l(4) DELIMITED BY SIZE
                          �����|���������b�l(5) DELIMITED BY SIZE
                     INTO �󔒋l�v
                   END-STRING
                   PERFORM �󔒋l����
                   MOVE �󔒋l�v TO ��P�|��������(���ʂb�m�s)
               END-READ
      */�������R
               MOVE ���p�|���R��(���ʂb�m�s)       TO  ��P�|�������R(���ʂb�m�s)
               MOVE "0" TO ��P�|�R���敪(���ʂb�m�s)
           END-PERFORM.
000720     EVALUATE ���Z�|���Z�����敪
           WHEN 1
               MOVE "1" TO ��P�|�����敪
           WHEN 2
               MOVE "2" TO ��P�|�����敪
           WHEN 3
               MOVE "3" TO ��P�|�����敪
           END-EVALUATE.
           MOVE �����񐔂v                   TO ��P�|������.
           MOVE ���Z�|������                 TO ��P�|������.
           MOVE �x���񐔂v                   TO ��P�|�����x�����Z��  
           MOVE �[��񐔂v                   TO ��P�|�����[����Z��  
           MOVE ���ԊO�񐔂v                 TO ��P�|�������ԊO���Z��
           MOVE ���Z�|�������Z��             TO ��P�|�������Z
           MOVE ���k�x���񐔂v               TO ��P�|���������k�x������
           MOVE ���Z�|���������k��           TO ��P�|���������k�x����
           MOVE �Č��񐔂v                   TO ��P�|�Č���.
           MOVE ���Z�|�Č���                 TO ��P�|�Č���
           COMPUTE ��P�|���Ë��� = ���Z�|���Ë��� * 10
           MOVE ���É񐔂v                   TO ��P�|���É�.
           MOVE ���Z�|���×�                 TO ��P�|���×�
           MOVE ��ԉ񐔂v                   TO ��P�|��ԉ��Z�̉��É�    
           MOVE ��H�񐔂v                   TO ��P�|��H���Z�̉��É�    
           MOVE �\���J��񐔂v               TO ��P�|�\���J����Z�̉��É�
           MOVE ���Z�|���É��Z��             TO ��P�|���É��Z
           MOVE ��񐔂v                     TO ��P�|�������q���
           MOVE ���񐔂v                     TO ��P�|�������q����
           MOVE ���񐔂v                     TO ��P�|�������q����
           MOVE ���Z�|�������q���Z��         TO ��P�|�������q���Z
           MOVE ���񋟉񐔂v               TO ��P�|�{�p���񋟗��̉�
           MOVE ���Z�|�{�p���񋟗�         TO ��P�|�{�p���񋟗�      
           MOVE ���Z�|�^����É�           TO ��P�|�^����Î��{��    
           MOVE ���Z�|�^����×�             TO ��P�|�^����×�          
           PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL ���ʂb�m�s > 5
               MOVE ���Z�|�^���Z���(���ʂb�m�s) TO ��P�|�^����Î��{��(���ʂb�m�s)
           END-PERFORM
      *
           IF ���Z�|���������v�P NOT = ZERO
           OR ���Z�|���ʖ��̂P(1) NOT = SPACE
               MOVE 1                       TO ��P�|�s�ԍ�            (1)
               MOVE SPACE                   TO ��P�|�����J�n����      (1)
               MOVE ���Z�|��É񐔂P        TO ��P�|��É�          (1)
               MOVE ���Z�|��×��P          TO ��P�|��×�            (1)
               MOVE ���Z�|��㪖@�񐔂P      TO ��P�|��㪖@��        (1)
               MOVE ���Z�|��㪖@���P        TO ��P�|��㪖@��          (1)
               MOVE ���Z�|��㪖@�񐔂P      TO ��P�|��㪖@��        (1)
               MOVE ���Z�|��㪖@���P        TO ��P�|��㪖@��          (1)
               MOVE ���Z�|�d�É񐔂P        TO ��P�|�d�É�          (1)
               MOVE ���Z�|�d�×��P          TO ��P�|�d�×�            (1)
               MOVE ZERO                    TO ��P�|�����ʒ�����      (1)
               COMPUTE ��P�|�����ʒ����z(1) = ���Z�|��×��P   + ���Z�|��㪖@���P
                                             + ���Z�|��㪖@���P + ���Z�|�d�×��P
               MOVE ���Z�|�����������P      TO ��P�|����������        (1)
               MOVE ���Z�|���������v�P      TO ��P�|���ʒ������ʗ����v(1)
           END-IF.
      *
           IF ���Z�|���������v�Q NOT = ZERO
           OR ���Z�|���ʖ��̂P(2) NOT = SPACE
               MOVE 2                       TO ��P�|�s�ԍ�            (2)
               MOVE SPACE                   TO ��P�|�����J�n����      (2)
               MOVE ���Z�|��É񐔂Q        TO ��P�|��É�          (2)
               MOVE ���Z�|��×��Q          TO ��P�|��×�            (2)
               MOVE ���Z�|��㪖@�񐔂Q      TO ��P�|��㪖@��        (2)
               MOVE ���Z�|��㪖@���Q        TO ��P�|��㪖@��          (2)
               MOVE ���Z�|��㪖@�񐔂Q      TO ��P�|��㪖@��        (2)
               MOVE ���Z�|��㪖@���Q        TO ��P�|��㪖@��          (2)
               MOVE ���Z�|�d�É񐔂Q        TO ��P�|�d�É�          (2)
               MOVE ���Z�|�d�×��Q          TO ��P�|�d�×�            (2)
               MOVE ZERO                    TO ��P�|�����ʒ�����      (2)
               COMPUTE ��P�|�����ʒ����z(2) = ���Z�|��×��Q   + ���Z�|��㪖@���Q
                                             + ���Z�|��㪖@���Q + ���Z�|�d�×��Q
               MOVE ���Z�|�����������Q      TO ��P�|����������        (2)
               MOVE ���Z�|���������v�Q      TO ��P�|���ʒ������ʗ����v(2)
           END-IF.
      *
           IF ���Z�|���������v�R�W NOT = ZERO
               MOVE 3                       TO ��P�|�s�ԍ�            (3)
               MOVE SPACE                   TO ��P�|�����J�n����      (3)
               MOVE ���Z�|��É񐔂R�W      TO ��P�|��É�          (3)
               MOVE ���Z�|��×��R�W        TO ��P�|��×�            (3)
               MOVE ���Z�|��㪖@�񐔂R�W    TO ��P�|��㪖@��        (3)
               MOVE ���Z�|��㪖@���R�W      TO ��P�|��㪖@��          (3)
               MOVE ���Z�|��㪖@�񐔂R�W    TO ��P�|��㪖@��        (3)
               MOVE ���Z�|��㪖@���R�W      TO ��P�|��㪖@��          (3)
               MOVE ���Z�|�d�É񐔂R�W      TO ��P�|�d�É�          (3)
               MOVE ���Z�|�d�×��R�W        TO ��P�|�d�×�            (3)
               MOVE �����ʗ��R�v�q          TO ��P�|�����ʒ�����      (3)
               COMPUTE ��P�|�����ʒ����z(3) = ���Z�|��×��R�W   + ���Z�|��㪖@���R�W
                                             + ���Z�|��㪖@���R�W + ���Z�|�d�×��R�W
               MOVE ���Z�|�����������R�W    TO ��P�|����������        (3)
               MOVE ���Z�|���������v�R�W    TO ��P�|���ʒ������ʗ����v(3)
           END-IF.
      *
           IF ���Z�|���������v�R�O NOT = ZERO
               MOVE 4                       TO ��P�|�s�ԍ�            (4)
               MOVE ���Z�|�����J�n�����R�O  TO ��P�|�����J�n����      (4)
               MOVE ���Z�|��É񐔂R�O      TO ��P�|��É�          (4)
               MOVE ���Z�|��×��R�O        TO ��P�|��×�            (4)
               MOVE ���Z�|��㪖@�񐔂R�O    TO ��P�|��㪖@��        (4)
               MOVE ���Z�|��㪖@���R�O      TO ��P�|��㪖@��          (4)
               MOVE ���Z�|��㪖@�񐔂R�O    TO ��P�|��㪖@��        (4)
               MOVE ���Z�|��㪖@���R�O      TO ��P�|��㪖@��          (4)
               MOVE ���Z�|�d�É񐔂R�O      TO ��P�|�d�É�          (4)
               MOVE ���Z�|�d�×��R�O        TO ��P�|�d�×�            (4)
               MOVE ZERO                    TO ��P�|�����ʒ�����      (4)
               COMPUTE ��P�|�����ʒ����z(4) = ���Z�|��×��R�O   + ���Z�|��㪖@���R�O
                                             + ���Z�|��㪖@���R�O + ���Z�|�d�×��R�O
               MOVE ���Z�|�����������R�O    TO ��P�|����������        (4)
               MOVE ���Z�|���������v�R�O    TO ��P�|���ʒ������ʗ����v(4)
           END-IF.
      *
           IF ���Z�|���������v�S�W NOT = ZERO
               MOVE 5                       TO ��P�|�s�ԍ�            (5)
               MOVE ���Z�|�����J�n�����S�W  TO ��P�|�����J�n����      (5)
               MOVE ���Z�|��É񐔂S�W      TO ��P�|��É�          (5)
               MOVE ���Z�|��×��S�W        TO ��P�|��×�            (5)
               MOVE ���Z�|��㪖@�񐔂S�W    TO ��P�|��㪖@��        (5)
               MOVE ���Z�|��㪖@���S�W      TO ��P�|��㪖@��          (5)
               MOVE ���Z�|��㪖@�񐔂S�W    TO ��P�|��㪖@��        (5)
               MOVE ���Z�|��㪖@���S�W      TO ��P�|��㪖@��          (5)
               MOVE ���Z�|�d�É񐔂S�W      TO ��P�|�d�É�          (5)
               MOVE ���Z�|�d�×��S�W        TO ��P�|�d�×�            (5)
               MOVE �����ʗ��R�v�q          TO ��P�|�����ʒ�����      (5)
               COMPUTE ��P�|�����ʒ����z(5) = ���Z�|��×��S�W   + ���Z�|��㪖@���S�W
                                             + ���Z�|��㪖@���S�W + ���Z�|�d�×��S�W
               MOVE ���Z�|�����������S�W    TO ��P�|����������        (5)
               MOVE ���Z�|���������v�S�W    TO ��P�|���ʒ������ʗ����v(5)
           END-IF.
      *
           IF ���Z�|���������v�S�O NOT = ZERO
               MOVE 6                       TO ��P�|�s�ԍ�            (6)
               MOVE ���Z�|�����J�n�����S�O  TO ��P�|�����J�n����      (6)
               MOVE ���Z�|��É񐔂S�O      TO ��P�|��É�          (6)
               MOVE ���Z�|��×��S�O        TO ��P�|��×�            (6)
               MOVE ���Z�|��㪖@�񐔂S�O    TO ��P�|��㪖@��        (6)
               MOVE ���Z�|��㪖@���S�O      TO ��P�|��㪖@��          (6)
               MOVE ���Z�|��㪖@�񐔂S�O    TO ��P�|��㪖@��        (6)
               MOVE ���Z�|��㪖@���S�O      TO ��P�|��㪖@��          (6)
               MOVE ���Z�|�d�É񐔂S�O      TO ��P�|�d�É�          (6)
               MOVE ���Z�|�d�×��S�O        TO ��P�|�d�×�            (6)
               MOVE ZERO                    TO ��P�|�����ʒ�����      (6)
               COMPUTE ��P�|�����ʒ����z(6) = ���Z�|��×��S�O   + ���Z�|��㪖@���S�O
                                             + ���Z�|��㪖@���S�O + ���Z�|�d�×��S�O
               MOVE ���Z�|�����������S�O    TO ��P�|����������        (6)
               MOVE ���Z�|���������v�S�O    TO ��P�|���ʒ������ʗ����v(6)
           END-IF.
      *
           IF ���Z�|���������v�T�W NOT = ZERO
               MOVE 7                       TO ��P�|�s�ԍ�            (7)
               MOVE ���Z�|�����J�n�����T�W  TO ��P�|�����J�n����      (7)
               MOVE ���Z�|��É񐔂T�W      TO ��P�|��É�          (7)
               MOVE ���Z�|��×��T�W        TO ��P�|��×�            (7)
               MOVE ���Z�|��㪖@�񐔂T�W    TO ��P�|��㪖@��        (7)
               MOVE ���Z�|��㪖@���T�W      TO ��P�|��㪖@��          (7)
               MOVE ���Z�|��㪖@�񐔂T�W    TO ��P�|��㪖@��        (7)
               MOVE ���Z�|��㪖@���T�W      TO ��P�|��㪖@��          (7)
               MOVE ���Z�|�d�É񐔂T�W      TO ��P�|�d�É�          (7)
               MOVE ���Z�|�d�×��T�W        TO ��P�|�d�×�            (7)
               MOVE �����ʗ��R�v�q          TO ��P�|�����ʒ�����      (7)
               COMPUTE ��P�|�����ʒ����z(7) = ���Z�|��×��T�W   + ���Z�|��㪖@���T�W
                                             + ���Z�|��㪖@���T�W + ���Z�|�d�×��T�W
               MOVE ���Z�|�����������T�W    TO ��P�|����������        (7)
               MOVE ���Z�|���������v�T�W    TO ��P�|���ʒ������ʗ����v(7)
           END-IF.
      *
           IF ���Z�|���������v�T�O NOT = ZERO
               MOVE 8                       TO ��P�|�s�ԍ�            (8)
               MOVE ���Z�|�����J�n�����T�O  TO ��P�|�����J�n����      (8)
               MOVE ���Z�|��É񐔂T�O      TO ��P�|��É�          (8)
               MOVE ���Z�|��×��T�O        TO ��P�|��×�            (8)
               MOVE ���Z�|��㪖@�񐔂T�O    TO ��P�|��㪖@��        (8)
               MOVE ���Z�|��㪖@���T�O      TO ��P�|��㪖@��          (8)
               MOVE ���Z�|��㪖@�񐔂T�O    TO ��P�|��㪖@��        (8)
               MOVE ���Z�|��㪖@���T�O      TO ��P�|��㪖@��          (8)
               MOVE ���Z�|�d�É񐔂T�O      TO ��P�|�d�É�          (8)
               MOVE ���Z�|�d�×��T�O        TO ��P�|�d�×�            (8)
               MOVE ZERO                    TO ��P�|�����ʒ�����      (8)
               COMPUTE ��P�|�����ʒ����z(8) = ���Z�|��×��T�O   + ���Z�|��㪖@���T�O
                                             + ���Z�|��㪖@���T�O + ���Z�|�d�×��T�O
               MOVE ���Z�|�����������T�O    TO ��P�|����������        (8)
               MOVE ���Z�|���������v�T�O    TO ��P�|���ʒ������ʗ����v(8)
           END-IF.
      *
           IF ���Z�|���������v�U�W NOT = ZERO
               MOVE 9                       TO ��P�|�s�ԍ�            (9)
               MOVE ���Z�|�����J�n�����U�W  TO ��P�|�����J�n����      (9)
               MOVE ���Z�|��É񐔂U�W      TO ��P�|��É�          (9)
               MOVE ���Z�|��×��U�W        TO ��P�|��×�            (9)
               MOVE ���Z�|��㪖@�񐔂U�W    TO ��P�|��㪖@��        (9)
               MOVE ���Z�|��㪖@���U�W      TO ��P�|��㪖@��          (9)
               MOVE ���Z�|��㪖@�񐔂U�W    TO ��P�|��㪖@��        (9)
               MOVE ���Z�|��㪖@���U�W      TO ��P�|��㪖@��          (9)
               MOVE ���Z�|�d�É񐔂U�W      TO ��P�|�d�É�          (9)
               MOVE ���Z�|�d�×��U�W        TO ��P�|�d�×�            (9)
               MOVE �����ʗ��R�v�q          TO ��P�|�����ʒ�����      (9)
               COMPUTE ��P�|�����ʒ����z(9) = ���Z�|��×��U�W   + ���Z�|��㪖@���U�W
                                             + ���Z�|��㪖@���U�W + ���Z�|�d�×��U�W
               MOVE ���Z�|�����������U�W    TO ��P�|����������        (9)
               MOVE ���Z�|���������v�U�W    TO ��P�|���ʒ������ʗ����v(9)
           END-IF.
      *
           IF ���Z�|���������v�U�O NOT = ZERO
      *         MOVE 10                      TO ��P�|�s�ԍ�            (10)
               MOVE ���Z�|�����J�n�����U�O  TO ��P�|�����J�n����      (10)
               MOVE ���Z�|��É񐔂U�O      TO ��P�|��É�          (10)
               MOVE ���Z�|��×��U�O        TO ��P�|��×�            (10)
               MOVE ���Z�|��㪖@�񐔂U�O    TO ��P�|��㪖@��        (10)
               MOVE ���Z�|��㪖@���U�O      TO ��P�|��㪖@��          (10)
               MOVE ���Z�|��㪖@�񐔂U�O    TO ��P�|��㪖@��        (10)
               MOVE ���Z�|��㪖@���U�O      TO ��P�|��㪖@��          (10)
               MOVE ���Z�|�d�É񐔂U�O      TO ��P�|�d�É�          (10)
               MOVE ���Z�|�d�×��U�O        TO ��P�|�d�×�            (10)
               MOVE ZERO                    TO ��P�|�����ʒ�����      (10)
               COMPUTE ��P�|�����ʒ����z(10) = ���Z�|��×��U�O   + ���Z�|��㪖@���U�O
                                              + ���Z�|��㪖@���U�O + ���Z�|�d�×��U�O
               MOVE ���Z�|�����������U�O    TO ��P�|����������        (10)
               MOVE ���Z�|���������v�U�O    TO ��P�|���ʒ������ʗ����v(10)
           END-IF.
      *
004549     MOVE SPACE TO �E�v���v. 
           MOVE 1              TO �E�v�|����敪.
           MOVE ��|�{�p�a��   TO �E�v�|�{�p�a��.
           MOVE ��|�{�p�N     TO �E�v�|�{�p�N.
           MOVE ��|�{�p��     TO �E�v�|�{�p��.
           MOVE ��|���Ҕԍ�   TO �E�v�|���Ҕԍ�.
           MOVE ��|�}��       TO �E�v�|�}��.
           READ �E�v�t�@�C��
           NOT INVALID KEY
               IF �E�v�|�E�v NOT = SPACE
004598*             MOVE �E�v�|�E�v  TO  �E�v���v
      *             PERFORM �E�v��������
004598             MOVE �E�v�|�E�v  TO  ��P�|�E�v
               END-IF
           END-READ.
           IF ���Z�|���Z��� NOT = 3
               EVALUATE ��|�ی����
               WHEN 01
                   IF ��|�ی��Ҕԍ�(3:1) = "3"
                       MOVE "01" TO ��P�|�ی���ʏڍ�
                   ELSE
                       MOVE "00" TO ��P�|�ی���ʏڍ�
                   END-IF
               WHEN 02
                   MOVE "03" TO ��P�|�ی���ʏڍ�
               WHEN 03
                   MOVE "06" TO ��P�|�ی���ʏڍ�
               WHEN 04
                   MOVE "07" TO ��P�|�ی���ʏڍ�
               WHEN 05
                   MOVE "21" TO ��P�|�ی���ʏڍ�
               WHEN 06
                   MOVE "04" TO ��P�|�ی���ʏڍ�
               WHEN 07
                   MOVE "05" TO ��P�|�ی���ʏڍ�
               WHEN 08
                   MOVE "02" TO ��P�|�ی���ʏڍ�
               WHEN 09
                   MOVE "08" TO ��P�|�ی���ʏڍ�
               END-EVALUATE
           ELSE
               EVALUATE ��|�������
               WHEN 50
                   MOVE "10" TO ��P�|�ی���ʏڍ�
               WHEN 51
                   MOVE "12" TO ��P�|�ی���ʏڍ�
               WHEN 52
                   MOVE "14" TO ��P�|�ی���ʏڍ�
               WHEN 53
                   MOVE "13" TO ��P�|�ی���ʏڍ�
               WHEN 54
                   MOVE "17" TO ��P�|�ی���ʏڍ�
               WHEN 55
                   MOVE "15" TO ��P�|�ی���ʏڍ�
               WHEN 60
                   MOVE "17" TO ��P�|�ی���ʏڍ�
               END-EVALUATE
           END-IF.
      */���ڒǉ�������/20221028
           MOVE ���Z�|���׏����s���Z�� TO ��P�|���׏����s���Z��.
           MOVE ���Z�|���׏����s���Z�� TO ��P�|���׏����s���Z��.
      */���ڒǉ�������/20221028
027870     MOVE ��|�ی��Ҕԍ�         TO �ی��Ҕԍ��v
027880     MOVE ��|�ی����           TO �ی���ʂv
027880     MOVE ��|������           TO �����ʂv
004510     MOVE ��|�������           TO ������ʂv�q
008010     MOVE ��|��p���S�Ҕԍ����� TO �s�����ԍ��v
           EVALUATE ���Z�|���Z���
           WHEN 2
               MOVE ��|������           TO ��Q�|�ی���ʂP
               MOVE ��|��p���S�Ҕԍ�     TO ��Q�|�ی��Ҕԍ�
017220         READ ��ƃt�@�C���Q
017230         INVALID KEY
017870             MOVE ��|������       TO �s�|������  
017880             MOVE ��|��p���S�Ҕԍ� TO �s�|�s�����ԍ�
017890             READ �s�����}�X�^
017900             INVALID KEY
017910                 CONTINUE
017920             NOT INVALID KEY
017910                 CONTINUE
                       MOVE �s�|�s��������      TO ��Q�|�ی��Җ�  
                       MOVE "21"                TO ��Q�|�ی����
                       MOVE �s�|�s�����ԍ�(3:2) TO ��Q�|���R�[�h
                       MOVE �s�|�X�֔ԍ�        TO ��Q�|�X�֔ԍ�
                       MOVE �s�|�Z���P          TO ��Q�|�Z���P  
                       MOVE �s�|�Z���Q          TO ��Q�|�Z���Q  
                       MOVE �s�|�d�b�ԍ�        TO ��Q�|�d�b�ԍ�
023691                 PERFORM �U�������Z�b�g
                       MOVE �����ԍ��v          TO ��Q�|�����ԍ�
019340                 WRITE ��Q�|���R�[�h
019350                 INVALID KEY
019360                     MOVE NC"��Q"  TO �t�@�C����
019370                     PERFORM �G���[�\��
019380                 END-WRITE
                   END-READ
               END-READ
           WHEN 3
               MOVE ��|�������           TO ��Q�|�ی���ʂP
               MOVE ��|��p���S�Ҕԍ����� TO ��Q�|�ی��Ҕԍ�
017220         READ ��ƃt�@�C���Q
017230         INVALID KEY
017870             MOVE ��|�������           TO �s�|������  
017880             MOVE ��|��p���S�Ҕԍ����� TO �s�|�s�����ԍ�
017890             READ �s�����}�X�^
017900             INVALID KEY
017910                 CONTINUE
017920             NOT INVALID KEY
017910                 CONTINUE
                       MOVE �s�|�s�������� TO ��Q�|�ی��Җ�  
                       EVALUATE ��|�������
                       WHEN 50
                           MOVE "10" TO ��Q�|�ی����
                       WHEN 51
                           MOVE "12" TO ��Q�|�ی����
                       WHEN 52
                           MOVE "14" TO ��Q�|�ی����
                       WHEN 53
                           MOVE "13" TO ��Q�|�ی����
                       WHEN 54
                           MOVE "17" TO ��Q�|�ی����
                       WHEN 55
                           MOVE "15" TO ��Q�|�ی����
                       WHEN 60
                           MOVE "17" TO ��Q�|�ی����
                       END-EVALUATE
                       MOVE �s�|�s�����ԍ�(3:2) TO ��Q�|���R�[�h
                       MOVE �s�|�X�֔ԍ�        TO ��Q�|�X�֔ԍ�
                       MOVE �s�|�Z���P          TO ��Q�|�Z���P  
                       MOVE �s�|�Z���Q          TO ��Q�|�Z���Q  
                       MOVE �s�|�d�b�ԍ�        TO ��Q�|�d�b�ԍ�
023691                 PERFORM �U�������Z�b�g����
                       MOVE �����ԍ��v              TO ��Q�|�����ԍ�
019340                 WRITE ��Q�|���R�[�h
019350                 INVALID KEY
019360                     MOVE NC"��Q"  TO �t�@�C����
019370                     PERFORM �G���[�\��
019380                 END-WRITE
                   END-READ
               END-READ
           WHEN OTHER
               MOVE ��|�ی����           TO ��Q�|�ی���ʂP
               MOVE ��|�ی��Ҕԍ�         TO ��Q�|�ی��Ҕԍ�
017220         READ ��ƃt�@�C���Q
017230         INVALID KEY
017200             MOVE ��|�ی����           TO �ہ|�ی����
017210             MOVE ��|�ی��Ҕԍ�         TO �ہ|�ی��Ҕԍ�
017220             READ �ی��҃}�X�^
017900             INVALID KEY
017910                 CONTINUE
017230             NOT INVALID KEY
                       MOVE �ہ|�ی��Җ���     TO ��Q�|�ی��Җ�
                       EVALUATE �ہ|�ی����
                       WHEN 01
                           IF �ہ|�ی��Ҕԍ�(3:1) = "3"
                               MOVE "01" TO ��Q�|�ی����
                           ELSE
                               MOVE "00" TO ��Q�|�ی����
                           END-IF
                       WHEN 02
                           MOVE "03" TO ��Q�|�ی����
                       WHEN 03
                           MOVE "06" TO ��Q�|�ی����
                       WHEN 04
                           MOVE "07" TO ��Q�|�ی����
                       WHEN 05
                           MOVE "21" TO ��Q�|�ی����
                       WHEN 06
                           MOVE "04" TO ��Q�|�ی����
                       WHEN 07
                           MOVE "05" TO ��Q�|�ی����
                       WHEN 08
                           MOVE "02" TO ��Q�|�ی����
                       WHEN 09
                           MOVE "08" TO ��Q�|�ی����
                       END-EVALUATE
                       IF �ہ|�ی���� = 01
                           MOVE �ہ|�ی��Ҕԍ�(1:2) TO ��Q�|���R�[�h
                       ELSE
                           MOVE �ہ|�ی��Ҕԍ�(3:2) TO ��Q�|���R�[�h
                       END-IF
                       MOVE �ہ|�X�֔ԍ�            TO ��Q�|�X�֔ԍ�
                       MOVE �ہ|�Z���P              TO ��Q�|�Z���P
                       MOVE �ہ|�Z���Q              TO ��Q�|�Z���Q
                       MOVE �ہ|�d�b�ԍ�            TO ��Q�|�d�b�ԍ�
023691                 PERFORM �U�������Z�b�g
                       MOVE �����ԍ��v              TO ��Q�|�����ԍ�
019340                 WRITE ��Q�|���R�[�h
019350                 INVALID KEY
019360                     MOVE NC"��Q"  TO �t�@�C����
019370                     PERFORM �G���[�\��
019380                 END-WRITE
                   END-READ
               END-READ
           END-EVALUATE.
012900*================================================================*
       �{�p�L�^���� SECTION.
      *
           INITIALIZE ���ʖ����v �񐔃J�E���^�v.
      *
030930     MOVE ��|���Ҕԍ�          TO �{�L�|���Ҕԍ�
030940     MOVE ��|�}��              TO �{�L�|�}��
030950     MOVE ��|�{�p�a��          TO �{�L�|�{�p�a��
030960     MOVE ��|�{�p�N            TO �{�L�|�{�p�N
030970     MOVE ��|�{�p��            TO �{�L�|�{�p��
030980     MOVE ZERO                  TO �{�L�|�{�p��
031000     START �{�p�L�^�e   KEY IS >= �{�L�|���҃R�[�h
031010                                  �{�L�|�{�p�a��N����
031020     END-START
031030     IF ��ԃL�[ = "00"
               MOVE SPACE TO �I���t���O�Q
031090         PERFORM �{�p�L�^�e�Ǎ�
031280         PERFORM UNTIL ( �I���t���O�Q         = "YES"            ) OR
031290                       ( �{�L�|���҃R�[�h NOT = ��|���҃R�[�h   ) OR
031300                       ( �{�L�|�{�p�a��   NOT = ��|�{�p�a��     ) OR
031310                       ( �{�L�|�{�p�N     NOT = ��|�{�p�N       ) OR
031320                       ( �{�L�|�{�p��     NOT = ��|�{�p��       )
      */��������
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
                   IF (�{�L�|�f�Ë敪 = 2 ) AND (�{�L�|���������k���敪 NOT = 1)
                       COMPUTE ���k�x���񐔂v = ���k�x���񐔂v + 1
                   END-IF
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
      */�������ʖ�����
                   PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL
                      (���ʂb�m�s > ���|���ʐ�) OR (���ʂb�m�s > 6)
      */���ʖ��̎{�p��
                      IF (�{�L�|�{�p�a��N���� >= ���|�J�n�a��N����(���ʂb�m�s)) AND
                        ((�{�L�|�{�p�a��N���� <= ���|�I���a��N����(���ʂb�m�s)) OR 
                         (���|�I���a��N����(���ʂb�m�s) = ZERO))
                          COMPUTE ���ʓ��b�m�s(���ʂb�m�s) = ���ʓ��b�m�s(���ʂb�m�s) + 1
                          MOVE �{�L�|�{�p�� TO ��P�|�{�p��((���ʂb�m�s)(���ʓ��b�m�s(���ʂb�m�s)))
                      END-IF
024670*            /�@���񏈒u�̃J�E���g�@/
024680                IF �{�L�|�����{�Ë敪(���ʂb�m�s) = 1
024690                    COMPUTE ���񏈒u�񐔂v(���ʂb�m�s) = ���񏈒u�񐔂v(���ʂb�m�s) + 1
024700                END-IF
031410*
                       EVALUATE �{�L�|�������q�敪(���ʂb�m�s)
                       WHEN 1
015620                     COMPUTE ��񐔂v   = ��񐔂v + 1
                       WHEN 2
015620                     COMPUTE ���񐔂v   = ���񐔂v + 1
                       WHEN 3
015620                     COMPUTE ���񐔂v   = ���񐔂v + 1
                       END-EVALUATE
                       IF �{�L�|�������q�敪(���ʂb�m�s) NOT = ZERO
                          COMPUTE �����b�m�s = �����b�m�s + 1
                          IF �����b�m�s <= 3
                              MOVE �{�L�|�{�p�� TO ��P�|�������q���Z��(�����b�m�s)
                          END-IF
                       END-IF
015610                 IF �{�L�|���񋟋敪(���ʂb�m�s) = 1
015620                     COMPUTE ���񋟉񐔂v = ���񋟉񐔂v + 1
015630                 END-IF
      *               */�{�p�J�n�N����
                       IF (�{�p�J�n�N�����v > �{�L�|�{�p�a��N����) OR
                          (�{�p�J�n�N�����v = ZERO)
                           MOVE �{�L�|�{�p�a��N���� TO �{�p�J�n�N�����v
                       END-IF
031430             END-PERFORM
031420             PERFORM �{�p�L�^�e�Ǎ�
031430         END-PERFORM
           END-IF.
012900*================================================================*
022340 �����f�[�^�擾 SECTION.
022350*
022360     INITIALIZE �������v.
022361*
022370     MOVE ���Z�|�{�p�a��   TO ���|�{�p�a��.
022380     MOVE ���Z�|�{�p�N     TO ���|�{�p�N.
022390     MOVE ���Z�|�{�p��     TO ���|�{�p��.
022400     MOVE ���Z�|���҃R�[�h TO ���|���҃R�[�h.
022410     READ �����f�[�^�e
022420     INVALID KEY
022430         MOVE SPACE TO ���|���R�[�h
022440     NOT INVALID KEY
022430         CONTINUE
           END-READ.
012980*================================================================*
       ����N�����擾 SECTION.
      *
010250     MOVE ZERO               TO ����N���v  ������N���v.
010260     MOVE �a��v�Z�a��v     TO ���|�����敪.
010270     READ �����}�X�^
           INVALID KEY
010330          MOVE  NC"�����}�X�^�̓ǂݍ��݂Ɏ��s���܂����B" TO �A���|���b�Z�[�W
010340          CALL   "MSG001"
010350          CANCEL "MSG001"
                DISPLAY ��|���҃R�[�h " " ��|�{�p�a��N�� " �����敪:" ���|�����敪
010360          PERFORM �t�@�C����
010370          MOVE 99 TO PROGRAM-STATUS
010380          EXIT PROGRAM
010280     NOT INVALID KEY
010290         MOVE ���|�J�n����N TO ����N�v
010300     END-READ.
010310*
010320     IF ����N�v = ZERO
010330          MOVE  NC"�����}�X�^�ɊJ�n����N��o�^���ĉ�����" TO �A���|���b�Z�[�W
010340          CALL   "MSG001"
010350          CANCEL "MSG001"
010360          PERFORM �t�@�C����
010370          MOVE 99 TO PROGRAM-STATUS
010380          EXIT PROGRAM
010390     ELSE
010400          COMPUTE ����N�v = ����N�v + �a��v�Z�N�v - 1
010410          MOVE �a��v�Z���v  TO ����v
010420     END-IF.
010430*
010440     MOVE ����N���v         TO ����v�Z�N���v.
           MOVE �a��v�Z���v       TO ����v�Z���v.
010450*
012980*================================================================*
019320 ��P�t�@�C������ SECTION.
019330*
019340     WRITE ��P�|���R�[�h
019350     INVALID KEY
019360         MOVE NC"��P"  TO �t�@�C����
019370         PERFORM �G���[�\��
019380     END-WRITE.
036110*================================================================*
008560 �f�[�^�`�F�b�N SECTION.
008570*
008580     MOVE SPACE          TO ���s�L�[�v.
019520* *****************************************************************
019530* * ���Z�v�g�e�̐����Ώۋ敪 = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
019540* *****************************************************************
019640     IF ( ���Z�|�����Ώۋ敪 NOT = ZERO ) AND
005778        ( ���Z�|���ҕ����敪 NOT = 1 )
              IF(���Z�|���Z��� = 3) AND ( ���Z�|����\����Ώۋ敪 = 1 )
                 CONTINUE
              ELSE
004090           MOVE ���Z�|�{�p�a��  TO ��|�{�p�a��
004100           MOVE ���Z�|�{�p�N    TO ��|�{�p�N
004110           MOVE ���Z�|�{�p��    TO ��|�{�p��
004120           MOVE ���Z�|���Ҕԍ�  TO ��|���Ҕԍ�
004130           MOVE ���Z�|�}��      TO ��|�}��
                 READ ��f�ҏ��e
                 NOT INVALID KEY
019880               MOVE "YES"  TO ���s�L�[�v
019950           END-READ
              END-IF
019960     END-IF.
009040*
020480*================================================================*
031920 ���Z�v�g���я��擾 SECTION.
031930*
031940     MOVE ���Z�|�{�p�a��   TO ��S�|�{�p�a��.
031950     MOVE ���Z�|�{�p�N     TO ��S�|�{�p�N.
031960     MOVE ���Z�|�{�p��     TO ��S�|�{�p��.
031970     MOVE ���Z�|���҃R�[�h TO ��S�|���҃R�[�h.
           IF ���Z�|���Z��� = 3
031980         MOVE ��|������� TO ��S�|�ی����
           ELSE
031980         MOVE ��|�ی���� TO ��S�|�ی����
           END-IF.
031990     READ ��ƃt�@�C���S
032000     INVALID KEY
032020         MOVE SPACE TO ��S�|���R�[�h
032000     NOT INVALID KEY
032020         CONTINUE
032030     END-READ.
032040*
032050*================================================================*
       �����p���҂e�Ǎ� SECTION.
      *
           MOVE ���Z�|�{�p�a��N�� TO ���p�|�{�p�a��N��.
000170     MOVE ���Z�|���҃R�[�h   TO ���p�|���҃R�[�h.
           READ �����p���҂e
           INVALID KEY
               MOVE SPACE TO ���p�|���R�[�h
           END-READ.
012420*================================================================*
       �v�Z���擾 SECTION.
      *
      * ����敪�F�O�P  ���N�ی�
           MOVE 01             TO �v�|����敪.
           MOVE ��|�{�p�a��   TO �v�|�J�n�a��.
           MOVE ��|�{�p�N     TO �v�|�J�n�N.
           MOVE ��|�{�p��     TO �v�|�J�n��.
      *
           START �v�Z�}�X�^ KEY IS <= �v�|����敪 �v�|�J�n�a��N��
                                                           REVERSED
           END-START.
      *
           IF ��ԃL�[ = "00"
               READ �v�Z�}�X�^ NEXT
               NOT AT END
                   IF ( ��|�{�p�a��N�� >= �v�`�|�J�n�a��N�� ) AND
                      ( ��|�{�p�a��N�� <= �v�`�|�I���a��N�� )
                       MOVE �v�`�|�����ʒ�����(3) TO �����ʗ��R�v�q
                   END-IF
               END-READ
           END-IF.
012900*================================================================*
023691 �U�������Z�b�g SECTION.
023692*
023693*****************************************
023694*  �ی��ҕʂɐU��������ݒ肷��
023695*****************************************
023696*
023702     MOVE SPACE    TO �I���t���O�R.
023702     MOVE SPACE    TO �����ԍ��v.
023703
023716     OPEN INPUT �U�������e.
023717             MOVE NC"�U��" TO �t�@�C����.
023718             PERFORM �I�[�v���`�F�b�N.
      *
023722     PERFORM �U�������e�Ǎ�.
023723     PERFORM UNTIL �I���t���O�R NOT = SPACE
023724*        ����������񕪉�
023725         UNSTRING �����|���R�[�h�f�[�^  DELIMITED BY ","
023726             INTO �����ی��Ҕԍ��v �����ی��Җ��v ���������ԍ��v ���Z�@�փR�[�h�v
023728         END-UNSTRING
      *        ���Z�@�փR�[�h(2004-135)�̎��͌����ԍ�(1029444)�Œ�
023731*        ��������ی��Ҕԍ��ƃ}�b�`���邩�i�擪�̕ی��Ҕԍ�0�͖��o�^���p�Ȃ̂Ŗ������Z�b�g�j
               IF �����ی��Ҕԍ��v = �ی��Ҕԍ��v
                   IF ���Z�@�փR�[�h�v = "2004-135"
023746                 MOVE "1029444"        TO �����ԍ��v
                   ELSE
023746                 MOVE ���������ԍ��v   TO �����ԍ��v
023747             END-IF
                   MOVE "YES"                TO �I���t���O�R
               ELSE
                   MOVE "3620000"        TO �����ԍ��v
023747         END-IF
023748         PERFORM �U�������e�Ǎ�
023749     END-PERFORM.
023719*
023752     CLOSE �U�������e.
023719*
023703*/�ی��Ҕԍ�����v���Ȃ��������A���܂ł̑O����v�`�F�b�N/
023719*
           IF �����ԍ��v = "3620000"
023702         MOVE SPACE    TO �I���t���O�R
023716         OPEN INPUT �U�������e
023717             MOVE NC"�U��" TO �t�@�C����
023718             PERFORM �I�[�v���`�F�b�N
023719*
023722         PERFORM �U�������e�Ǎ�
023723         PERFORM UNTIL �I���t���O�R NOT = SPACE
023724*        ����������񕪉�
023725             UNSTRING �����|���R�[�h�f�[�^  DELIMITED BY ","
023726                INTO �����ی��Ҕԍ��v �����ی��Җ��v ���������ԍ��v ���Z�@�փR�[�h�v
023728             END-UNSTRING
      *
                   PERFORM VARYING �J�E���^ FROM 1 BY 1
                           UNTIL (�����ی��Ҕԍ��v(�J�E���^:1) = "@") OR
                                 (�J�E���^ > 10)
                       CONTINUE
                   END-PERFORM
                   IF (�����ی��Ҕԍ��v(1:1) NOT = "@") AND
                      (�����ی��Ҕԍ��v(�J�E���^:1) = "@") AND
                      (�����ی��Ҕԍ��v(1:�J�E���^ - 1) = �ی��Ҕԍ��v(1:�J�E���^ - 1))
023746                 MOVE ���������ԍ��v   TO �����ԍ��v
                       MOVE "YES"            TO �I���t���O�R
                   ELSE
                       MOVE "3620000"        TO �����ԍ��v
                   END-IF
023748             PERFORM �U�������e�Ǎ�
               END-PERFORM
023719*
023752         CLOSE �U�������e
           END-IF.
023719*
023703*/�ی��Ҕԍ�����v���Ȃ��������A���I������̌����v�`�F�b�N/
023719*
           IF �����ԍ��v = "3620000"
023702         MOVE SPACE    TO �I���t���O�R
023716         OPEN INPUT �U�������e
023717             MOVE NC"�U��" TO �t�@�C����
023718             PERFORM �I�[�v���`�F�b�N
023719*
023722         PERFORM �U�������e�Ǎ�
023723         PERFORM UNTIL �I���t���O�R NOT = SPACE
023724*        ����������񕪉�
023725             UNSTRING �����|���R�[�h�f�[�^  DELIMITED BY ","
023726                INTO �����ی��Ҕԍ��v �����ی��Җ��v ���������ԍ��v ���Z�@�փR�[�h�v
023728             END-UNSTRING
      *
                   PERFORM VARYING �J�E���^ FROM 1 BY 1
                           UNTIL (�����ی��Ҕԍ��v(�J�E���^:1) NOT = "@") OR
                                 (�J�E���^ > 10)
                       CONTINUE
                   END-PERFORM
                   IF (�����ی��Ҕԍ��v(1:1) = "@") AND
                      (�����ی��Ҕԍ��v(�J�E���^:10 - �J�E���^) = �ی��Ҕԍ��v(�J�E���^:10 - �J�E���^))
023746                 MOVE ���������ԍ��v   TO �����ԍ��v
                       MOVE "YES"            TO �I���t���O�R
                   ELSE
                       MOVE "3620000"        TO �����ԍ��v
                   END-IF
023748             PERFORM �U�������e�Ǎ�
               END-PERFORM
023719*
023752         CLOSE �U�������e
           END-IF.
023754*
023755*================================================================*
023756 �U�������e�Ǎ� SECTION.
023757*
023761     READ �U�������e
023762     AT END
023763         MOVE "YES"  TO �I���t���O�R
023764     END-READ.
023767*
037520*================================================================*
023691 �U�������Z�b�g���� SECTION.
023692*
023693*****************************************
023694*  �ی��ҕʂɐU��������ݒ肷��
023695*****************************************
023696*
023702     MOVE SPACE    TO �I���t���O�R.
023702     MOVE SPACE    TO �����ԍ��v.
023703
023716     OPEN INPUT �U�������e.
023717             MOVE NC"�U��" TO �t�@�C����.
023718             PERFORM �I�[�v���`�F�b�N.
023719
023722     PERFORM �U�������e�Ǎ�.
023723     PERFORM UNTIL �I���t���O�R NOT = SPACE
023724*        ����������񕪉�
023725         UNSTRING �����|���R�[�h�f�[�^  DELIMITED BY ","
023726             INTO �����ی��Ҕԍ��v �����ی��Җ��v ���������ԍ��v ���Z�@�փR�[�h�v
023728         END-UNSTRING
023731*        ��������ی��Ҕԍ��ƃ}�b�`���邩�i�擪�̕ی��Ҕԍ�0�͖��o�^���p�Ȃ̂Ŗ������Z�b�g�j
023735         IF �����ی��Ҕԍ��v = �s�����ԍ��v
023746             MOVE ���������ԍ��v   TO �����ԍ��v
                   MOVE "YES"                TO �I���t���O�R
               ELSE
                   MOVE "3620000"        TO �����ԍ��v
023747         END-IF
023748         PERFORM �U�������e�Ǎ�
023749     END-PERFORM.
023751*
023752     CLOSE �U�������e.
023719*
023703*/�ی��Ҕԍ�����v���Ȃ��������A���܂ł̑O����v�`�F�b�N/
023719*
           IF �����ԍ��v = "3620000"
023702         MOVE SPACE    TO �I���t���O�R
023716         OPEN INPUT �U�������e
023717             MOVE NC"�U��" TO �t�@�C����
023718             PERFORM �I�[�v���`�F�b�N
023719*
023722         PERFORM �U�������e�Ǎ�
023723         PERFORM UNTIL �I���t���O�R NOT = SPACE
023724*        ����������񕪉�
023725             UNSTRING �����|���R�[�h�f�[�^  DELIMITED BY ","
023726                INTO �����ی��Ҕԍ��v �����ی��Җ��v ���������ԍ��v ���Z�@�փR�[�h�v
023728             END-UNSTRING
      *
                   PERFORM VARYING �J�E���^ FROM 1 BY 1
                           UNTIL (�����ی��Ҕԍ��v(�J�E���^:1) = "@") OR
                                 (�J�E���^ > 10)
                       CONTINUE
                   END-PERFORM
                   IF (�����ی��Ҕԍ��v(1:1) NOT = "@") AND
                      (�����ی��Ҕԍ��v(�J�E���^:1) = "@") AND
                      (�����ی��Ҕԍ��v(1:�J�E���^ - 1) = �s�����ԍ��v(1:�J�E���^ - 1))
023746                 MOVE ���������ԍ��v   TO �����ԍ��v
                       MOVE "YES"            TO �I���t���O�R
                   ELSE
                       MOVE "3620000"        TO �����ԍ��v
                   END-IF
023748             PERFORM �U�������e�Ǎ�
               END-PERFORM
023719*
023752         CLOSE �U�������e
           END-IF.
023754*
023703*/�ی��Ҕԍ�����v���Ȃ��������A���I����̌����v�`�F�b�N/
023719*
           IF �����ԍ��v = "3620000"
023702         MOVE SPACE    TO �I���t���O�R
023716         OPEN INPUT �U�������e
023717             MOVE NC"�U��" TO �t�@�C����
023718             PERFORM �I�[�v���`�F�b�N
023719*
023722         PERFORM �U�������e�Ǎ�
023723         PERFORM UNTIL �I���t���O�R NOT = SPACE
023724*        ����������񕪉�
023725             UNSTRING �����|���R�[�h�f�[�^  DELIMITED BY ","
023726                INTO �����ی��Ҕԍ��v �����ی��Җ��v ���������ԍ��v ���Z�@�փR�[�h�v
023728             END-UNSTRING
      *
                   PERFORM VARYING �J�E���^ FROM 1 BY 1
                           UNTIL (�����ی��Ҕԍ��v(�J�E���^:1) NOT = "@") OR
                                 (�J�E���^ > 10)
                       CONTINUE
                   END-PERFORM
                   IF (�����ی��Ҕԍ��v(1:1) = "@") AND
                      (�����ی��Ҕԍ��v(�J�E���^:10 - �J�E���^) = �s�����ԍ��v(�J�E���^:10 - �J�E���^))
023746                 MOVE ���������ԍ��v   TO �����ԍ��v
                       MOVE "YES"            TO �I���t���O�R
                   ELSE
                       MOVE "3620000"        TO �����ԍ��v
                   END-IF
023748             PERFORM �U�������e�Ǎ�
               END-PERFORM
023719*
023752         CLOSE �U�������e
           END-IF.
023719*
023755*================================================================*
       ���o�t�@�C���쐬 SECTION.
      *
007101     OPEN OUTPUT ���o�t�@�C��.
002680     IF ��ԃL�[  NOT =  "00"
007103         MOVE  NC"�w�肳�ꂽ�h���C�u���Ȃ����������߂܂���" TO �A���|���b�Z�[�W
007104         CALL   "MSG001"
007105         CANCEL "MSG001"
002720         MOVE 99 TO PROGRAM-STATUS
002730         EXIT PROGRAM
002770     END-IF.
      *
           MOVE "{" TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
           MOVE "  |header|: {"               TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */�f�[�^���
           MOVE "    |data_type|: |1|,"       TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */�����N��
           MOVE �����a��N���v�q  TO �a��v�Z�N�����v
           PERFORM ����N�����擾
           STRING "    |seikyu_ym|: |" DELIMITED BY SIZE
                  ����v�Z�N���v       DELIMITED BY SIZE
                  "|,"                 DELIMITED BY SIZE
             INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */���Z�R���Ǝ�
            MOVE "    |vendor|: |MKS|,"       TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */�o�[�W����
           MOVE "    |version|: |5.3|"       TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
           MOVE "  },"                       TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */�{�p�ҏ��
           MOVE "  |sejutsusha_arr|: ["      TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
           MOVE "    {"                      TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */����ԍ�
           STRING "      |kaiin_bango|: |" DELIMITED BY SIZE
                   �{��|�ڍ��t�����ԍ�  DELIMITED BY SPACE
                  "|,"                     DELIMITED BY SIZE
             INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p�Ҕԍ�
           STRING "      |sejutsusha_bango|: |" DELIMITED BY SIZE
                  �{��|�V�_���t�ԍ�(3:11)      DELIMITED BY SIZE
                  "|,"                          DELIMITED BY SIZE
             INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p�J�n�N����
           MOVE �{�p�J�n�N�����v  TO �a��v�Z�N�����v
           PERFORM ����N�����擾
           STRING "      |sejutsu_kaishi_ymd|: |" DELIMITED BY SIZE
                  ����v�Z�N�����v                DELIMITED BY SIZE
                  "|,"                            DELIMITED BY SIZE
             INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p����
           MOVE �{��|�ڍ��@��(1:40) TO ����������v
           PERFORM ����������.
           STRING "      |sejutsusho_mei|: |" DELIMITED BY SIZE
                   ����������v(1:�����b�m�s)  DELIMITED BY SIZE
                   "|,"                        DELIMITED BY SIZE
             INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p�����J�i
           MOVE �{��|�ڍ��@���J�i(1:40) TO ����������v
           PERFORM ����������.
           STRING "      |sejutsusho_kana|: |" DELIMITED BY SIZE
                   ����������v(1:�����b�m�s)   DELIMITED BY SIZE
                   "|,"                         DELIMITED BY SIZE
             INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p�Җ�
           MOVE �{��|��\�Җ�(1:30) TO ����������v
           PERFORM ����������.
           STRING "      |sejutsusha_mei|: |" DELIMITED BY SIZE
                   ����������v(1:�����b�m�s)  DELIMITED BY SIZE
                   "|,"                        DELIMITED BY SIZE
             INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p�Җ��J�i
           MOVE �{��|��\�҃J�i(1:30) TO ����������v
           PERFORM ����������.
           STRING "      |sejutsusha_kana|: |" DELIMITED BY SIZE
                   ����������v(1:�����b�m�s)  DELIMITED BY SIZE
                   "|,"                          DELIMITED BY SIZE
             INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p��_�X�֔ԍ�
           STRING "      |yubin|: |" DELIMITED BY SIZE
                   �{��|�X�֔ԍ�     DELIMITED BY SIZE
                   "|,"               DELIMITED BY SIZE
             INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p��_�Z���P
      */�|���E�ɂȂ�΍�]�ɕϊ�/20230413
           INSPECT �{��|�Z���P   REPLACING ALL "�|" BY "�]"
           MOVE �{��|�Z���P(1:40) TO ����������v
           PERFORM ����������.
           STRING "      |jusho1|: |"        DELIMITED BY SIZE
                   ����������v(1:�����b�m�s) DELIMITED BY SIZE
                   "|,"                       DELIMITED BY SIZE
             INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p��_�Z���Q
           MOVE �{��|�Z���Q(1:40) TO ����������v
           PERFORM ����������.
           STRING "      |jusho2|: |"        DELIMITED BY SIZE
                   ����������v(1:�����b�m�s) DELIMITED BY SIZE
                   "|,"                       DELIMITED BY SIZE
             INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p��_TEL
            STRING "      |tel|: |"     DELIMITED BY SIZE
                   �{��|�d�b�ԍ�(1:13) DELIMITED BY SPACE
                   "|,"                 DELIMITED BY SIZE
             INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p��_FAX
           MOVE "      |fax|: ||,"       TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */�{�p��_EMAIL
           MOVE "      |email|: ||,"       TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */�U����_��s��
            MOVE �{��|������s��(1:30) TO ����������v
            PERFORM ����������.
            STRING "      |ginko_mei|: |"     DELIMITED BY SIZE
                   ����������v(1:�����b�m�s) DELIMITED BY SIZE
                   "|,"                       DELIMITED BY SIZE
               INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�U����_�x�X��
           MOVE �{��|������s�x�X��(1:30) TO ����������v
           PERFORM ����������.
           STRING "      |shiten_mei|: |"     DELIMITED BY SIZE
                  ����������v(1:�����b�m�s) DELIMITED BY SIZE
                  "|,"                       DELIMITED BY SIZE
               INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�U����_��s���J�i
           MOVE "      |ginko_kana|: ||,"    TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */�U����_�x�X���J�i
           MOVE "      |shiten_kana|: ||,"   TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */�U����_�������
           STRING "      |koza_shu|: |" DELIMITED BY SIZE
                  �{��|�a�����        DELIMITED BY SIZE
                  "|,"                  DELIMITED BY SIZE
               INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�U����_�����ԍ�
           STRING "      |koza_bango|: |" DELIMITED BY SIZE
                  �{��|�����ԍ�(1:7)     DELIMITED BY SIZE
                  "|,"                    DELIMITED BY SIZE
               INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�U����_���`
           MOVE �{��|�������`�l(1:60) TO ����������v
           PERFORM ����������.
           STRING "      |koza_meigi|: |"    DELIMITED BY SIZE
                  ����������v(1:�����b�m�s) DELIMITED BY SIZE
                  "|,"                       DELIMITED BY SIZE
               INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�U����_���`�J�i
           MOVE �{��|�������`�l�J�i(1:60) TO ����������v
           PERFORM ����������.
           STRING "      |koza_meigi_kana|: |" DELIMITED BY SIZE
                  ����������v(1:�����b�m�s)   DELIMITED BY SIZE
                  "|,"                         DELIMITED BY SIZE
               INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */���ϔԍ�
           STRING "      |kyosai_bango|: |" DELIMITED BY SIZE
                  �{��|���ϘA�ԍ�          DELIMITED BY SIZE
                  "|,"                      DELIMITED BY SIZE
               INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�n���ϔԍ�
           STRING "      |chikyosai_bango|: |" DELIMITED BY SIZE
                  �{��|�n���ϘA�ԍ�           DELIMITED BY SIZE
                  "|,"                         DELIMITED BY SIZE
               INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�h�q�Ȕԍ�
           STRING "      |boeisho_bango|: |" DELIMITED BY SIZE
                  �{��|���q���ԍ�           DELIMITED BY SIZE
                  "|,"                       DELIMITED BY SIZE
               INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p�@�֔ԍ�1
025580**   / ���{�pID /
025600     MOVE 01                     TO �h�c�ǁ|�h�c�敪.
025610     MOVE ZERO                   TO �h�c�ǁ|�{�p���ԍ�.
025620     MOVE �{��|�s���{���i�h�r   TO �h�c�ǁ|�ی����.
025630     MOVE SPACE                  TO �h�c�ǁ|�ی��Ҕԍ�.
025640     READ �h�c�Ǘ��}�X�^
025650     NOT INVALID KEY
025660         MOVE �h�c�ǁ|�{�p�h�c�ԍ�   TO ���{�p�h�c�v
025670     END-READ.
           STRING "      |sejutsu_kikan_bango1|: |" DELIMITED BY SIZE
                  ���{�p�h�c�v                      DELIMITED BY SPACE
                  "|,"                              DELIMITED BY SIZE
               INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */�{�p�@�֔ԍ�2
           MOVE "      |sejutsu_kikan_bango2|: ||,"    TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */���ڒǉ�������/20221028
      */�J�Ўw��E�w���ԍ�
           MOVE "      |rosai_shitei_shimei_bango|: ||,"    TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */���׏����s�͏o�L��
           STRING "      |meisaisho_hakko_umu|: |" DELIMITED BY SIZE
                  ���ה��s���Z�敪�v               DELIMITED BY SPACE
                  "|"                             DELIMITED BY SIZE
               INTO ���|���R�[�h�f�[�^
           END-STRING
           PERFORM ���o�e����.
      */���ڒǉ�������/20221028
           MOVE "    }"   TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
           MOVE "  ],"   TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */���Z�v�g���***************************
           MOVE "  |rezept_arr|: ["   TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
026580     OPEN INPUT ��ƃt�@�C���P.
026590         MOVE NC"��P" TO �t�@�C����.
026600         PERFORM �I�[�v���`�F�b�N.
           MOVE SPACE TO �I���t���O�R.
005580     PERFORM ��ƃt�@�C���P�Ǎ�
005590     PERFORM UNTIL �I���t���O�R = "YES"
               MOVE ��P�|���R�[�h TO ��P���R�[�h�v
      */���Z�v�g�ԍ�
               MOVE "      {"   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
               MOVE ��P�|���Z�v�g�ԍ� TO �������l�v
               PERFORM �������l����
               STRING "      |rezept_no|: " DELIMITED BY SIZE
      *                ��P�|���Z�v�g�ԍ�    DELIMITED BY SIZE
                      �������l�Q�v          DELIMITED BY SPACE
                      ","                   DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�{�p�Ҕԍ�
               STRING "      |sejutsusha_bango|: |" DELIMITED BY SIZE
                      ��P�|�{�p�Ҕԍ�              DELIMITED BY SIZE
                      "|,"                          DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�{�p����N��
               STRING "      |sejutsu_ym|: |" DELIMITED BY SIZE
                      ��P�|�{�p����N��      DELIMITED BY SIZE
                      "|,"                    DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��o��敪
               STRING "      |teishutsusaki_kbn|: |" DELIMITED BY SIZE
                      ��P�|��o��敪               DELIMITED BY SIZE
                      "|,"                           DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�ی��Ҕԍ�
               STRING "      |hokensha_bango|: |" DELIMITED BY SIZE
                      ��P�|�ی��Ҕԍ�            DELIMITED BY SIZE
                      "|,"                        DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�ی��؋L��
               STRING "      |hokensho_kigo|: |" DELIMITED BY SIZE
                      ��P�|�ی��؋L��           DELIMITED BY SPACE
                      "|,"                       DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�ی��ؔԍ�
               STRING "      |hokensho_bango|: |" DELIMITED BY SIZE
                      ��P�|�ی��ؔԍ�            DELIMITED BY SPACE
                      "|,"                        DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */����S�Ҕԍ��P
               STRING "      |kohi_futansha_bango1|: |" DELIMITED BY SIZE
                      ��P�|����S�Ҕԍ��P            DELIMITED BY SPACE
                      "|,"                              DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */����󋋎Ҕԍ��P
               STRING "      |kohi_jukyusha_bango1|: |" DELIMITED BY SIZE
                      ��P�|����󋋎Ҕԍ��P            DELIMITED BY SPACE
                      "|,"                              DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */����S�Ҕԍ��Q
               STRING "      |kohi_futansha_bango2|: |" DELIMITED BY SIZE
                      ��P�|����S�Ҕԍ��Q            DELIMITED BY SPACE
                      "|,"                              DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */����󋋎Ҕԍ��Q
               STRING "      |kohi_jukyusha_bango2|: |" DELIMITED BY SIZE
                      ��P�|����󋋎Ҕԍ��Q            DELIMITED BY SPACE
                      "|,"                              DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�ی���ʑ�敪
               STRING "      |hoken_shubetsu_kbn|: |" DELIMITED BY SIZE
                      ��P�|�ی���ʑ�敪            DELIMITED BY SIZE
                      "|,"                            DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�P���敪
               STRING "      |tanhei_kbn|: |" DELIMITED BY SIZE
                      ��P�|�P���敪          DELIMITED BY SIZE
                      "|,"                    DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�{�Ƌ敪
               STRING "      |honka_kbn|: |" DELIMITED BY SIZE
                      ��P�|�{�Ƌ敪         DELIMITED BY SIZE
                      "|,"                   DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���t����
               MOVE ��P�|���t���� TO �������l�v
               PERFORM �������l����
               STRING "      |kyufu_wari|: " DELIMITED BY SIZE
      *                ��P�|���t����         DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                    DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */����
               STRING "      |zokugara|: |" DELIMITED BY SIZE
                      ��P�|����            DELIMITED BY SIZE
                      "|,"                  DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��ی��҃J�i
               MOVE ��P�|��ی��҃J�i TO ����������v
               PERFORM ����������
               STRING "      |hihokensha_kana|: |" DELIMITED BY SIZE
                      ����������v(1:�����b�m�s)   DELIMITED BY SIZE
                      "|,"                         DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��ی��Җ�
               MOVE ��P�|��ی��Җ� TO ����������v
               PERFORM ����������
               STRING "      |hihokensha_mei|: |" DELIMITED BY SIZE
                      ����������v(1:�����b�m�s)  DELIMITED BY SIZE
                      "|,"                        DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��ÎҖ��J�i
               MOVE ��P�|��ÎҖ��J�i TO ����������v
               PERFORM ����������
               STRING "      |juryosha_kana|: |" DELIMITED BY SIZE
                      ����������v(1:�����b�m�s) DELIMITED BY SIZE
                      "|,"                       DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��ÎҖ�
               MOVE ��P�|��ÎҖ� TO ����������v
               PERFORM ����������
               STRING "      |juryosha_mei|: |"  DELIMITED BY SIZE
                      ����������v(1:�����b�m�s) DELIMITED BY SIZE
                      "|,"                       DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��ÎҐ���
               STRING "      |juryosha_seibetsu|: |" DELIMITED BY SIZE
                      ��P�|��ÎҐ���               DELIMITED BY SIZE
                      "|,"                           DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��ÎҐ��N����
               STRING "      |juryosha_seinengappi|: |" DELIMITED BY SIZE
                      ��P�|��ÎҐ��N����              DELIMITED BY SIZE
                      "|,"                              DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��ی��җX�֔ԍ�
               STRING "      |hihokensha_yubin|: |" DELIMITED BY SIZE
                   ��P�|��ی��җX�֔ԍ�           DELIMITED BY SIZE
                      "|,"                          DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��ی��ҏZ��
      */�|���E�ɂȂ�΍�]�ɕϊ�/20230413
               INSPECT ��P�|��ی��ҏZ��   REPLACING ALL "�|" BY "�]"
      */�S�p�A���p�ɂ�炸�ő�60����/20230413
               MOVE ��P�|��ی��ҏZ�� TO �����`�F�b�N�s�a�k
               PERFORM �S�p���p�`�F�b�N
               MOVE �����`�F�b�N�s�a�k(1:�J�E���^�R) TO ��P�|��ی��ҏZ��
      *
               MOVE ��P�|��ی��ҏZ�� TO ����������v
               PERFORM ����������
               STRING "      |hihokensha_jusho|: |" DELIMITED BY SIZE
                      ����������v(1:�����b�m�s)    DELIMITED BY SIZE
                      "|,"                          DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���v���z
               MOVE ��P�|���v���z TO �������l�v
               PERFORM �������l����
               STRING "      |hiyo_gaku|: " DELIMITED BY SIZE
      *                ��P�|���v���z        DELIMITED BY SIZE
                      �������l�Q�v          DELIMITED BY SPACE
                      ","                   DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�ꕔ���S��
               MOVE ��P�|�ꕔ���S�� TO �������l�v
               PERFORM �������l����
               STRING "      |ichibu_futankin|: " DELIMITED BY SIZE
      *                ��P�|�ꕔ���S��            DELIMITED BY SIZE
                      �������l�Q�v                DELIMITED BY SPACE
                      ","                         DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�������z
               MOVE ��P�|�������z TO �������l�v
               PERFORM �������l����
               STRING "      |seikyu_gaku|: " DELIMITED BY SIZE
      *                ��P�|�������z          DELIMITED BY SIZE
                      �������l�Q�v            DELIMITED BY SPACE
                      ","                     DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */����ꕔ���S�������z
               MOVE ��P�|����ꕔ���S�������z TO �������l�v
               PERFORM �������l����
               STRING "      |kohi_ichibu_futankin|: " DELIMITED BY SIZE
      *                ��P�|����ꕔ���S�������z       DELIMITED BY SIZE
                      �������l�Q�v                     DELIMITED BY SPACE
                      ","                              DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��������z
               MOVE ��P�|��������z TO �������l�v
               PERFORM �������l����
               STRING "      |kohi_seikyu_gaku|: " DELIMITED BY SIZE
      *               ��P�|��������z            DELIMITED BY SIZE
                      �������l�Q�v                 DELIMITED BY SPACE
                      ","                          DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��������
               MOVE ��P�|�������� TO �������l�v
               PERFORM �������l����
               STRING "      |sojitsu_nissu|: " DELIMITED BY SIZE
      *                ��P�|��������            DELIMITED BY SIZE
                      �������l�Q�v              DELIMITED BY SPACE
                      ","                       DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���ʐ�
               MOVE ��P�|���ʐ� TO �������l�v
               PERFORM �������l����
               STRING "      |buisu|: " DELIMITED BY SIZE
      *                ��P�|���ʐ�      DELIMITED BY SIZE
                      �������l�Q�v      DELIMITED BY SPACE
                      ","               DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�Đ����敪
               STRING "      |saiseikyu_kbn|: |" DELIMITED BY SIZE
                      ��P�|�Đ����敪           DELIMITED BY SIZE
                      "|,"                       DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���S�敪
               STRING "      |futan_kbn|: |" DELIMITED BY SIZE
                      ��P�|���S�敪         DELIMITED BY SIZE
                      "|,"                   DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���S����
               STRING "      |futan_wari|: " DELIMITED BY SIZE
                      ��P�|���S����         DELIMITED BY SIZE
                      ","                    DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���Ҕԍ��Q
               MOVE ��P�|���Ҕԍ��Q TO �������l�v
               PERFORM �������l����
               STRING "      |kanja_no|: " DELIMITED BY SIZE
      *                ��P�|���Ҕԍ��Q     DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                  DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���Ҕԍ��}��
               STRING "      |kanja_subno|: " DELIMITED BY SIZE
                      ��P�|���Ҕԍ��}��      DELIMITED BY SIZE
                      ","                     DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�����f�[�^
               MOVE "      |fusho_arr|: ["   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
               PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL
      *                (���ʂb�m�s > ���|���ʐ�) OR (���ʂb�m�s > 6) OR (��P�|�����i���o�[(���ʂb�m�s) = ZERO)
                       (���ʂb�m�s > 6) OR (��P�|�����i���o�[(���ʂb�m�s) = ZERO)
                   IF ���ʂb�m�s > 1
                       MOVE "        },"   TO ���|���R�[�h�f�[�^
                       PERFORM ���o�e����
                   END-IF
                   MOVE "        {"   TO ���|���R�[�h�f�[�^
                   PERFORM ���o�e����
      */�����i���o�[
                   STRING "          |fusho_no|: "       DELIMITED BY SIZE
                          ��P�|�����i���o�[(���ʂb�m�s) DELIMITED BY SIZE
                          ","                            DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */�����敪
                   STRING "          |fusho_kbn|: |" DELIMITED BY SIZE
                   ��P�|�����敪    (���ʂb�m�s)    DELIMITED BY SIZE
                          "|,"                       DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */�����R�[�h          
                   MOVE "          |fusho_cd|: ||,"   TO ���|���R�[�h�f�[�^
                   PERFORM ���o�e����
      */������              
                   STRING "          |fusho_mei|: |"     DELIMITED BY SIZE
                          ��P�|������      (���ʂb�m�s) DELIMITED BY SPACE
                          "|,"                           DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */�����N����          
                   STRING "          |fusho_ymd|: |"     DELIMITED BY SIZE
                          ��P�|�����N����  (���ʂb�m�s) DELIMITED BY SIZE
                          "|,"                           DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */�����N����          
                   STRING "          |shoken_ymd|: |"    DELIMITED BY SIZE
                          ��P�|�����N����  (���ʂb�m�s) DELIMITED BY SIZE
                          "|,"                           DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */�{�p�J�n��          
                   STRING "          |sejutsu_kaishi_ymd|: |" DELIMITED BY SIZE
                          ��P�|�{�p�J�n��  (���ʂb�m�s)      DELIMITED BY SIZE
                          "|,"                                DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */�{�p�I����          
                   STRING "          |sejutsu_shuryo_ymd|: |" DELIMITED BY SIZE
                          ��P�|�{�p�I����  (���ʂb�m�s)      DELIMITED BY SIZE
                          "|,"                                DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */������              
                   MOVE ��P�|������(���ʂb�m�s) TO �������l�v
                   PERFORM �������l����
                   STRING "          |jitsu_nissu|: "    DELIMITED BY SIZE
      *                    ��P�|������      (���ʂb�m�s) DELIMITED BY SIZE
                          �������l�Q�v           DELIMITED BY SPACE
                          ","                            DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */�{�p��
                   MOVE "          |sejutsubi_arr|: ["   TO ���|���R�[�h�f�[�^
                   PERFORM ���o�e����
                   PERFORM VARYING ���J�E���^ FROM 1 BY 1 UNTIL
                           (���J�E���^ > 31) OR (��P�|�{�p��(���ʂb�m�s ���J�E���^) = ZERO)
                       MOVE ��P�|�{�p��(���ʂb�m�s ���J�E���^) TO �������l�v
                       PERFORM �������l����
                       IF ��P�|�{�p��(���ʂb�m�s ���J�E���^ + 1) NOT = ZERO
                           STRING "            "                       DELIMITED BY SIZE
      *                            ��P�|�{�p�� (���ʂb�m�s ���J�E���^) DELIMITED BY SIZE
                                   �������l�Q�v                        DELIMITED BY SPACE
                                  ","                                  DELIMITED BY SIZE
                               INTO ���|���R�[�h�f�[�^
                           END-STRING
                       ELSE
                           STRING "            "                       DELIMITED BY SIZE
      *                            ��P�|�{�p�� (���ʂb�m�s ���J�E���^) DELIMITED BY SIZE
                                   �������l�Q�v                        DELIMITED BY SPACE
                               INTO ���|���R�[�h�f�[�^
                           END-STRING
                       END-IF
                       PERFORM ���o�e����
                   END-PERFORM
                   MOVE "          ],"   TO ���|���R�[�h�f�[�^
                   PERFORM ���o�e����
      */�]�A�敪            
                   STRING "          |tenki_kbn|: |"     DELIMITED BY SIZE
                          ��P�|�]�A�敪    (���ʂb�m�s) DELIMITED BY SIZE
                          "|,"                           DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */���Ŏ{�敪          
                   STRING "          |seikose_kbn|: |"   DELIMITED BY SIZE
                          ��P�|���Ŏ{�敪  (���ʂb�m�s) DELIMITED BY SIZE
                          "|,"                           DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */���Ŏ{��          
                   STRING "          |seikose_kaisu|: "  DELIMITED BY SIZE
                          ��P�|���Ŏ{��  (���ʂb�m�s) DELIMITED BY SIZE
                          ","                            DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */���Ŏ{�×�          
                   MOVE ��P�|���Ŏ{�×�(���ʂb�m�s) TO �������l�v
                   PERFORM �������l����
                   STRING "          |seikose_ryo|: "    DELIMITED BY SIZE
      *                    ��P�|���Ŏ{�×�  (���ʂb�m�s) DELIMITED BY SIZE
                          �������l�Q�v           DELIMITED BY SPACE
                          ","                            DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */��������            
                   STRING "          |fusho_genin|: |"   DELIMITED BY SIZE
                          ��P�|��������    (���ʂb�m�s) DELIMITED BY SPACE
                          "|,"                           DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */�������R            
                   STRING "          |choki_riyu|: |"    DELIMITED BY SIZE
                          ��P�|�������R    (���ʂb�m�s) DELIMITED BY SPACE
                          "|,"                           DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
                   PERFORM ���o�e����
      */�����p�񗝗R
                   MOVE "          |choki_hinkai_riyu|: ||,"   TO ���|���R�[�h�f�[�^
                   PERFORM ���o�e����
      */�o��                
                   MOVE "          |keika|: ||,"   TO ���|���R�[�h�f�[�^
                   PERFORM ���o�e����
      */�R���敪            
                   MOVE "          |shinsa_kbn|: |0|"   TO ���|���R�[�h�f�[�^
                   PERFORM ���o�e����
               END-PERFORM
               MOVE "        }"   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
               MOVE "      ],"   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
      */�����敪
               STRING "      |seikyu_kbn|: |" DELIMITED BY SIZE
                     ��P�|�����敪           DELIMITED BY SIZE
                      "|,"                    DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */������               
               STRING "      |shoken_kaisu|: " DELIMITED BY SIZE
                     ��P�|������            DELIMITED BY SIZE
                      ","                      DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */������                 
               MOVE ��P�|������ TO �������l�v
               PERFORM �������l����
               STRING "      |shoken_ryo|: " DELIMITED BY SIZE
      *               ��P�|������            DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                    DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�����x�����Z��       
               STRING "      |shoken_kyujitsu_kasan_kaisu|: " DELIMITED BY SIZE
                     ��P�|�����x�����Z��                   DELIMITED BY SIZE
                      ","                                     DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�����[����Z��       
               STRING "      |shoken_shinya_kasan_kaisu|: " DELIMITED BY SIZE
                     ��P�|�����[����Z��                 DELIMITED BY SIZE
                      ","                                   DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�������ԊO���Z��     
               STRING "      |shoken_jikangai_kasan_kaisu|: " DELIMITED BY SIZE
                     ��P�|�������ԊO���Z��                 DELIMITED BY SIZE
                      ","                                     DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�������Z               
               MOVE ��P�|�������Z TO �������l�v
               PERFORM �������l����
               STRING "      |shoken_kasan|: " DELIMITED BY SIZE
      *               ��P�|�������Z            DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                      DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���������k�x������   
               MOVE ��P�|���������k�x���� TO �������l�v
               PERFORM �������l����
               STRING "      |sodan_shien_kaisu|: " DELIMITED BY SIZE
      *               ��P�|���������k�x������     DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                           DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���������k�x����       
               MOVE ��P�|���������k�x���� TO �������l�v
               PERFORM �������l����
               STRING "      |sodan_shien_ryo|: " DELIMITED BY SIZE
      *                ��P�|���������k�x����       DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                         DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�Č���               
               STRING "      |saiken_kaisu|: " DELIMITED BY SIZE
                     ��P�|�Č���            DELIMITED BY SIZE
                      ","                      DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�Č���                 
               MOVE ��P�|�Č��� TO �������l�v
               PERFORM �������l����
               STRING "      |saiken_ryo|: " DELIMITED BY SIZE
      *               ��P�|�Č���            DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                    DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���Ë���               
               MOVE ��P�|���Ë��� TO �������l�v
               PERFORM �������l����
               STRING "      |oryo_kyori|: " DELIMITED BY SIZE
      *               ��P�|���Ë���          DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                    DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���É�               
               MOVE ��P�|���É� TO �������l�v
               PERFORM �������l����
               STRING "      |oryo_kaisu|: " DELIMITED BY SIZE
      *               ��P�|���É�          DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                    DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���×�                 
               MOVE ��P�|���×� TO �������l�v
               PERFORM �������l����
               STRING "      |oryo_ryo|: " DELIMITED BY SIZE
      *               ��P�|���×�          DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                  DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���×��R               
               STRING "      |oryo_riyu|: |" DELIMITED BY SIZE
                     ��P�|���×��R          DELIMITED BY SPACE
                      "|,"                   DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��ԉ��Z�̉��É�     
               MOVE ��P�|��ԉ��Z�̉��É� TO �������l�v
               PERFORM �������l����
               STRING "      |yakan_kasan_oryo_kaisu|: " DELIMITED BY SIZE
      *               ��P�|��ԉ��Z�̉��É�            DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                                DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */��H���Z�̉��É�     
               MOVE ��P�|��H���Z�̉��É� TO �������l�v
               PERFORM �������l����
               STRING "      |nanro_kasan_oryo_kaisu|: " DELIMITED BY SIZE
      *               ��P�|��H���Z�̉��É�            DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                                DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�\���J����Z�̉��É� 
               MOVE ��P�|�\���J����Z�̉��É� TO �������l�v
               PERFORM �������l����
               STRING "      |bofu_kasan_oryo_kaisu|: " DELIMITED BY SIZE
      *               ��P�|�\���J����Z�̉��É�       DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                               DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���É��Z               
               MOVE ��P�|���É��Z TO �������l�v
               PERFORM �������l����
               STRING "      |oryo_kasan|: " DELIMITED BY SIZE
      *               ��P�|���É��Z          DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                    DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�������q���         
               MOVE ��P�|�������q��� TO �������l�v
               PERFORM �������l����
               STRING "      |kinzoku_fukushi_dai_kaisu|: " DELIMITED BY SIZE
      *               ��P�|�������q���                   DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                                   DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�������q����         
               STRING "      |kinzoku_fukushi_chu_kaisu|: " DELIMITED BY SIZE
                     ��P�|�������q����                   DELIMITED BY SIZE
                      ","                                   DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�������q����         
               STRING "      |kinzoku_fukushi_sho_kaisu|: " DELIMITED BY SIZE
                     ��P�|�������q����                   DELIMITED BY SIZE
                      ","                                   DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�������q���Z           
               MOVE ��P�|�������q���Z TO �������l�v
               PERFORM �������l����
               STRING "      |kinzoku_fukushi_kasan|: " DELIMITED BY SIZE
      *               ��P�|�������q���Z                 DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                               DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�������q���Z��
               MOVE ��P�|�������q���Z��(���J�E���^) TO �������l�v
               PERFORM �������l����
               MOVE "      |kinzoku_fukushi_kasanbi_arr|: ["   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
               PERFORM VARYING ���J�E���^ FROM 1 BY 1 UNTIL ���J�E���^ > 3
                   IF ��P�|�������q���Z��(���J�E���^) NOT = ZERO
                       IF (��P�|�������q���Z��(���J�E���^ + 1) NOT = ZERO) AND (���J�E���^ < 3)
                           STRING "       "                        DELIMITED BY SIZE
      *                            ��P�|�������q���Z��(���J�E���^) DELIMITED BY SIZE
                                  �������l�Q�v           DELIMITED BY SPACE
                                  ","                              DELIMITED BY SIZE
                               INTO ���|���R�[�h�f�[�^
                           END-STRING
                       ELSE
                           STRING "       "                        DELIMITED BY SIZE
      *                            ��P�|�������q���Z��(���J�E���^) DELIMITED BY SIZE
                                  �������l�Q�v           DELIMITED BY SPACE
                               INTO ���|���R�[�h�f�[�^
                           END-STRING
                       END-IF
                       PERFORM ���o�e����
                   END-IF
               END-PERFORM
               MOVE "      ],"   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
     */�{�p���񋟗��̉� 
               STRING "      |joho_teikyo_ryo_kaisu|: " DELIMITED BY SIZE
                     ��P�|�{�p���񋟗��̉�         DELIMITED BY SIZE
                      ","                               DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
     */�{�p���񋟗�       
               MOVE ��P�|�{�p���񋟗� TO �������l�v
               PERFORM �������l����
               STRING "      |joho_teikyo_ryo|: " DELIMITED BY SIZE
      *               ��P�|�{�p���񋟗�         DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                         DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
     */�^����Î��{��     
               STRING "      |undo_koryo_kaisu|: " DELIMITED BY SIZE
                     ��P�|�^����Î��{��        DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
     */�^����×�           
               MOVE ��P�|�^����×� TO �������l�v
               PERFORM �������l����
               STRING "      |undo_koryo_ryo|: " DELIMITED BY SIZE
      *               ��P�|�^����×�            DELIMITED BY SIZE
                      �������l�Q�v           DELIMITED BY SPACE
                      ","                        DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�^����Î��{��
               MOVE ��P�|�^����Î��{��(���J�E���^) TO �������l�v
               PERFORM �������l����
               MOVE "      |undo_koryobi_arr|: ["   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
               PERFORM VARYING ���J�E���^ FROM 1 BY 1 UNTIL ���J�E���^ > 5
                   IF ��P�|�^����Î��{��(���J�E���^) NOT = ZERO
                       IF (��P�|�^����Î��{��(���J�E���^ + 1) NOT = ZERO) AND (���J�E���^ < 5)
                           STRING "       "                        DELIMITED BY SIZE
      *                            ��P�|�^����Î��{��(���J�E���^) DELIMITED BY SIZE
                                  �������l�Q�v           DELIMITED BY SPACE
                                  ","                              DELIMITED BY SIZE
                               INTO ���|���R�[�h�f�[�^
                           END-STRING
                       ELSE
                           STRING "       "                        DELIMITED BY SIZE
      *                            ��P�|�^����Î��{��(���J�E���^) DELIMITED BY SIZE
                                  �������l�Q�v           DELIMITED BY SPACE
                               INTO ���|���R�[�h�f�[�^
                           END-STRING
                       END-IF
                       PERFORM ���o�e����
                   END-IF
               END-PERFORM
               MOVE "      ],"   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
      */���ڒǉ�������/20221028
      */���׏����s�̐����Z�̉�
               MOVE ZERO TO ���׏����s�񐔂v
               IF ��P�|���׏����s���Z�� NOT = ZERO
                   MOVE 1 TO ���׏����s�񐔂v
               END-IF
               STRING "      |meisaisho_hakko_kaisu|: " DELIMITED BY SIZE
                      ���׏����s�񐔂v                  DELIMITED BY SIZE
                      ","                               DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���׏����s�̐����Z
               STRING "      |meisaisho_hakko_ryo|: " DELIMITED BY SIZE
                      ��P�|���׏����s���Z��          DELIMITED BY SIZE
                      ","                             DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���׏����s��
               IF ��P�|���׏����s���Z�� NOT = ZERO
                   STRING "      |meisaisho_hakko_md|: |" DELIMITED BY SIZE
                          ��P�|�{�p��                    DELIMITED BY SIZE
                          ��P�|���׏����s���Z��          DELIMITED BY SIZE
                          "|,"                            DELIMITED BY SIZE
                       INTO ���|���R�[�h�f�[�^
                   END-STRING
               ELSE
                   MOVE "      |meisaisho_hakko_md|: ||,"    TO ���|���R�[�h�f�[�^
               END-IF
               PERFORM ���o�e����
      */���ڒǉ�������/20221028
      */���ʕʗ����f�[�^
               MOVE "      |bui_ryokin_arr|: ["   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
               MOVE ZERO TO �s�ԍ����݂e
               PERFORM VARYING ���ʂb�m�s FROM 1 BY 1 UNTIL
      *                (���ʂb�m�s > ���|���ʐ�) OR (���ʂb�m�s > 10)
                      (���ʂb�m�s > 10)
                   IF ��P�|�s�ԍ�(���ʂb�m�s) NOT = ZERO
                       MOVE 1 TO �s�ԍ����݂e
                       IF ���ʂb�m�s > 1
                           MOVE "        },"   TO ���|���R�[�h�f�[�^
                           PERFORM ���o�e����
                       END-IF
                       MOVE "        {"   TO ���|���R�[�h�f�[�^
                       PERFORM ���o�e����
      */�s�ԍ�
                       STRING "          |line_no|: "    DELIMITED BY SIZE
                                ��P�|�s�ԍ�(���ʂb�m�s) DELIMITED BY SIZE
                              ","                        DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */�����J�n����        
                       STRING "          |teigen_kaishi_md|: |"     DELIMITED BY SIZE
                              ��P�|�����J�n����      (���ʂb�m�s) DELIMITED BY SPACE
                              "|,"                                 DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */��É�            
                       MOVE ��P�|��É�(���ʂb�m�s) TO �������l�v
                       PERFORM �������l����
                       STRING "          |koryo_kaisu|: "          DELIMITED BY SIZE
      *                        ��P�|��É�          (���ʂb�m�s) DELIMITED BY SIZE
                              �������l�Q�v           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */��×�              
                       MOVE ��P�|��×�(���ʂb�m�s) TO �������l�v
                       PERFORM �������l����
                       STRING "          |koryo_ryo|: "            DELIMITED BY SIZE
      *                        ��P�|��×�            (���ʂb�m�s) DELIMITED BY SIZE
                              �������l�Q�v           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */��㪖@��          
                       STRING "          |reianpo_kaisu|: "        DELIMITED BY SIZE
                              ��P�|��㪖@��        (���ʂb�m�s) DELIMITED BY SIZE
                              ","                                  DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */��㪖@��            
                       MOVE ��P�|��㪖@��(���ʂb�m�s) TO �������l�v
                       PERFORM �������l����
                       STRING "          |reianpo_ryo|: "          DELIMITED BY SIZE
      *                        ��P�|��㪖@��          (���ʂb�m�s) DELIMITED BY SIZE
                              �������l�Q�v           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */��㪖@��          
                       MOVE ��P�|��㪖@��(���ʂb�m�s) TO �������l�v
                       PERFORM �������l����
                       STRING "          |onanpo_kaisu|: "         DELIMITED BY SIZE
      *                        ��P�|��㪖@��        (���ʂb�m�s) DELIMITED BY SIZE
                              �������l�Q�v           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */��㪖@��            
                       MOVE ��P�|��㪖@��(���ʂb�m�s) TO �������l�v
                       PERFORM �������l����
                       STRING "          |onanpo_ryo|: "           DELIMITED BY SIZE
      *                        ��P�|��㪖@��          (���ʂb�m�s) DELIMITED BY SIZE
                              �������l�Q�v           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */�d�É�            
                       MOVE ��P�|�d�É�(���ʂb�m�s) TO �������l�v
                       PERFORM �������l����
                       STRING "          |denryo_kaisu|: "         DELIMITED BY SIZE
      *                        ��P�|�d�É�          (���ʂb�m�s) DELIMITED BY SIZE
                              �������l�Q�v           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */�d�×�              
                       MOVE ��P�|�d�×�(���ʂb�m�s) TO �������l�v
                       PERFORM �������l����
                       STRING "          |denryo_ryo|: "           DELIMITED BY SIZE
      *                        ��P�|�d�×�            (���ʂb�m�s) DELIMITED BY SIZE
                              �������l�Q�v           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */�����ʒ�����        
                       MOVE ��P�|�����ʒ�����(���ʂb�m�s) TO �������l�v
                       PERFORM �������l����
                       STRING "          |tabui_teigen_ritsu|: "   DELIMITED BY SIZE
      *                        ��P�|�����ʒ�����      (���ʂb�m�s) DELIMITED BY SIZE
                              �������l�Q�v           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */�����ʒ����z        
                       MOVE ��P�|�����ʒ����z(���ʂb�m�s) TO �������l�v
                       PERFORM �������l����
                       STRING "          |tabui_teigen_gaku|: "    DELIMITED BY SIZE
      *                        ��P�|�����ʒ����z      (���ʂb�m�s) DELIMITED BY SIZE
                              �������l�Q�v           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */����������          
                       MOVE ��P�|����������(���ʂb�m�s) TO �������l�v
                       PERFORM �������l����
                       STRING "          |choki_teigen_ritsu|: "   DELIMITED BY SIZE
      *                        ��P�|����������        (���ʂb�m�s) DELIMITED BY SIZE
                              �������l�Q�v           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
      */���ʒ������ʗ����v  
                       MOVE ��P�|���ʒ������ʗ����v(���ʂb�m�s) TO �������l�v
                       PERFORM �������l����
                       STRING "          |bui_teigen_ryokin_kei|: " DELIMITED BY SIZE
      *                        ��P�|���ʒ������ʗ����v(���ʂb�m�s)  DELIMITED BY SIZE
                              �������l�Q�v           DELIMITED BY SPACE
      *                        ","                    DELIMITED BY SIZE
                           INTO ���|���R�[�h�f�[�^
                       END-STRING
                       PERFORM ���o�e����
                   END-IF
               END-PERFORM
               IF �s�ԍ����݂e NOT = ZERO
                   MOVE "        }"   TO ���|���R�[�h�f�[�^
                   PERFORM ���o�e����
               END-IF
               MOVE "      ],"   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
      */�E�v
               STRING "      |tekiyo|: |" DELIMITED BY SIZE
                      ��P�|�E�v          DELIMITED BY SPACE
                      "|,"                DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���ӔN����              ��P�|���ӔN����     
               MOVE "      |doi_ymd|: ||,"   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
      */���ӈ�@                ��P�|���ӈ�@       
               MOVE "      |doi_byoin|: ||,"   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
      */���ӈ�t��              ��P�|���ӈ�t��     
               MOVE "      |doi_ishi|: ||,"   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
      */�ی���ʏڍ�
               STRING "      |hoken_shubetsu|: |" DELIMITED BY SIZE
                  ��P�|�ی���ʏڍ�              DELIMITED BY SIZE
                      "|"                         DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
005580         PERFORM ��ƃt�@�C���P�Ǎ�
               IF �I���t���O�R = SPACE
                   MOVE "    },"   TO ���|���R�[�h�f�[�^
               ELSE
                   MOVE "    }"    TO ���|���R�[�h�f�[�^
               END-IF
               PERFORM ���o�e����
           END-PERFORM.
           MOVE "  ],"    TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
      */�ی��ҏ��
           MOVE "  |hokensha_arr|: ["   TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
026580     OPEN INPUT ��ƃt�@�C���Q.
026590         MOVE NC"��Q" TO �t�@�C����.
026600         PERFORM �I�[�v���`�F�b�N.
           MOVE SPACE TO �I���t���O�R.
005580     PERFORM ��ƃt�@�C���Q�Ǎ�
005590     PERFORM UNTIL �I���t���O�R = "YES"
               MOVE "    {"   TO ���|���R�[�h�f�[�^
               PERFORM ���o�e����
      */�ی��Ҕԍ�
               STRING "      |hokensha_bango|: |" DELIMITED BY SIZE
                      ��Q�|�ی��Ҕԍ�            DELIMITED BY SIZE
                      "|,"                        DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�ی��Җ�
               STRING "      |hokensha_mei|: |" DELIMITED BY SIZE
                      ��Q�|�ی��Җ�            DELIMITED BY SPACE
                      "|,"                      DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�ی����
               STRING "      |hoken_shubetsu|: |" DELIMITED BY SIZE
                      ��Q�|�ی����              DELIMITED BY SIZE
                      "|,"                        DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */���R�[�h
               STRING "      |ken_cd|: |" DELIMITED BY SIZE
                      ��Q�|���R�[�h      DELIMITED BY SIZE
                      "|,"                DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�X�֔ԍ�
               STRING "      |yubin|: |" DELIMITED BY SIZE
                      ��Q�|�X�֔ԍ�     DELIMITED BY SIZE
                      "|,"               DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�Z���P  
      */�|���E�ɂȂ�΍�]�ɕϊ�/20230413
               INSPECT ��Q�|�Z���P   REPLACING ALL "�|" BY "�]"
               STRING "      |jusho1|: |" DELIMITED BY SIZE
                      ��Q�|�Z���P        DELIMITED BY SPACE
                      "|,"                DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�Z���Q  
               STRING "      |jusho2|: |" DELIMITED BY SIZE
                      ��Q�|�Z���Q        DELIMITED BY SPACE
                      "|,"                DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�d�b�ԍ�
               STRING "      |tel|: |" DELIMITED BY SIZE
                      ��Q�|�d�b�ԍ�   DELIMITED BY SIZE
                      "|,"             DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
      */�����ԍ�
               STRING "      |koza_bango|: |" DELIMITED BY SIZE
                      ��Q�|�����ԍ�          DELIMITED BY SIZE
                      "|"                     DELIMITED BY SIZE
                   INTO ���|���R�[�h�f�[�^
               END-STRING
               PERFORM ���o�e����
005580         PERFORM ��ƃt�@�C���Q�Ǎ�
               IF �I���t���O�R = SPACE
                   MOVE "    },"   TO ���|���R�[�h�f�[�^
               ELSE
                   MOVE "    }"    TO ���|���R�[�h�f�[�^
               END-IF
               PERFORM ���o�e����
           END-PERFORM.
           MOVE "  ]"   TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
           MOVE "}"   TO ���|���R�[�h�f�[�^.
           PERFORM ���o�e����.
           CLOSE ���o�t�@�C�� ��ƃt�@�C���P ��ƃt�@�C���Q.
           PERFORM �����ϊ�����.
023755*================================================================*
005580 ��ƃt�@�C���P�Ǎ� SECTION.
012920*
012930     READ ��ƃt�@�C���P NEXT
012940     AT END
012950         MOVE "YES" TO �I���t���O�R
012960     END-READ.
012970*
023755*================================================================*
       ���o�e���� SECTION.
      *
           MOVE SPACE TO �I���t���O�Q.
      *     PERFORM VARYING �����b�m�s FROM 200 BY -1
           PERFORM VARYING �����b�m�s FROM 500 BY -1
                   UNTIL   (�����b�m�s      <= ZERO ) OR
                           (�I���t���O�Q NOT = SPACE)
               IF ���|���R�[�h�f�[�^(�����b�m�s:1) NOT = SPACE
                   COMPUTE �����b�m�s = �����b�m�s + 1
                   MOVE "YES" TO �I���t���O�Q
               END-IF
           END-PERFORM.
      *
           INSPECT ���|���R�[�h�f�[�^   REPLACING ALL "|" BY ��ؕ����v�Q.
           INSPECT ���|���R�[�h�f�[�^   REPLACING ALL ���s BY "\r".
           WRITE ���|���R�[�h
           IF ��ԃL�[  NOT =  "00"
               MOVE NC"���"  TO �t�@�C����
               PERFORM �G���[�\��
           END-IF.
           INITIALIZE    ���|���R�[�h�f�[�^.
           MOVE SPACE TO ���|���R�[�h�f�[�^.
014200*================================================================*
       ���������� SECTION.
      *
           MOVE SPACE TO �I���t���O�Q.
           PERFORM VARYING �����b�m�s FROM 100 BY -1
                   UNTIL   (�����b�m�s      <= ZERO ) OR
                           (�I���t���O�Q NOT = SPACE)
               IF ����������v(�����b�m�s:1) NOT = SPACE
                   COMPUTE �����b�m�s = �����b�m�s + 1
                   MOVE "YES" TO �I���t���O�Q
               END-IF
           END-PERFORM.
014200*================================================================*
005580 ��ƃt�@�C���Q�Ǎ� SECTION.
012920*
012930     READ ��ƃt�@�C���Q NEXT
012940     AT END
012950         MOVE "YES" TO �I���t���O�R
012960     END-READ.
012970*
023755*================================================================*
       ���k�t�@�C���쐬 SECTION.
      *
           IF �A���|�Œ�t���O = 1
002890        MOVE "C:\makishisys\REZEPT.zip" TO ���k�t�@�C�����v
           ELSE
003324        MOVE �A���|�h���C�u TO �h���C�u�v�q
007096        STRING �h���C�u�v�q       DELIMITED BY SIZE
007097               ":\REZEPT.zip"    DELIMITED BY SIZE
007098               INTO ���k�t�@�C�����v
007099        END-STRING
           END-IF.
002890     MOVE "C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT.json" TO ���t�@�C�����v
      *
            MOVE SPACE TO �p���P�v.
      *      
      * ���k�R�}���h:"a -tzip"
            STRING "a -tzip"  DELIMITED BY SIZE
      *�@�@�@�@�@  /���k�t�@�C����
                   " "              DELIMITED BY SIZE
                   ���k�t�@�C�����v DELIMITED BY SPACE
      /
      *�@�@�@�@�@  /���̃t�@�C����
                   " "              DELIMITED BY SIZE
                   ���t�@�C�����v   DELIMITED BY SPACE
      *
      *            /�����_�C�A���O�o���Ȃ����w��
                   " -hide"  DELIMITED BY SIZE
      *
      *            /�p�X���[�h�́A -p�ɑ����Ŏw��
                   " -pIwa#Re0pt"  DELIMITED BY SIZE
                   INTO �p���P�v
            END-STRING.
            MOVE "zip" TO �v���O�������v.
            CALL �v���O�������v WITH C LINKAGE
                      USING BY REFERENCE �p���P�v.
      *
            IF PROGRAM-STATUS NOT = ZERO
      *         INVOKE POW-SELF "DisplayMessage" USING "���k���s�I�I" "���b�Z�[�W"
002470         MOVE  NC"�@�@�@�@�@�@���k���s�I�I" TO �A���|���b�Z�[�W
002480         CALL   "MSG001"
002490         CANCEL "MSG001"
            END-IF.     
      *
      *      MOVE "delfile" TO �v���O�������v.
      *      CALL �v���O�������v WITH C LINKAGE
      *              USING BY REFERENCE ���t�@�C�����v.
028706*================================================================*
       �����ϊ����� SECTION.
      *
      */SJIS����UTF8�ɕϊ�
001987     MOVE "C:\makishisys\YAWOBJ\YIW1011.bat"  TO �t���p�X���v.
001988*
001989     CALL "gofile2"  WITH C LINKAGE
001990                     USING BY REFERENCE �t���p�X���v.
001991*
001992      IF PROGRAM-STATUS NOT = ZERO 
001993          DISPLAY "�t�@�C�����J���܂���ł����B" UPON CONS
001996      END-IF.
      *
HILO  *     ACCEPT  �L�[���� FROM CONS.
      */�t�@�C�������݂���܂ŌJ��Ԃ�
           MOVE ZERO TO �x���J�E���^
           OPEN INPUT ���o�t�@�C���O
           PERFORM UNTIL ��ԃL�[ NOT = "35" OR �x���J�E���^ > 999990
               COMPUTE �x���J�E���^ = �x���J�E���^ + 1
               OPEN INPUT ���o�t�@�C���O
           END-PERFORM
           CLOSE ���o�t�@�C���O
           MOVE �x���J�E���^ TO �x���񐔂v.
026650     PERFORM �x������.
028706*================================================================*
       �󔒋l���� SECTION.
      *
HILO  *     DISPLAY "�󔒂P" �󔒋l�v
           PERFORM VARYING �����b�m�s FROM 1 BY 1
                   UNTIL   �����b�m�s > 200
               IF �󔒋l�v(�����b�m�s:1) = SPACE
                   MOVE �󔒋l�v(�����b�m�s + 1:200 - �����b�m�s) TO �󔒋l�v(�����b�m�s:200 - �����b�m�s + 1)
               END-IF
           END-PERFORM.
HILO  *     DISPLAY "�󔒂Q" �󔒋l�v.
028706*================================================================*
       �������l���� SECTION.
      *
           MOVE SPACE TO �I���t���O�S.
           PERFORM VARYING �����b�m�s FROM 1 BY 1 UNTIL �����b�m�s > 5 OR �I���t���O�S NOT = SPACE
               IF �������l�v(�����b�m�s:1) = "0"
                   MOVE SPACE TO �������l�v(�����b�m�s:1)
               ELSE
                   MOVE "YES" TO �I���t���O�S
               END-IF
           END-PERFORM.
           MOVE SPACE TO �I���t���O�S �������l�Q�v.
           PERFORM VARYING �����b�m�s FROM 1 BY 1 UNTIL �����b�m�s > 6 OR �I���t���O�S NOT = SPACE
               IF �������l�v(�����b�m�s:1) NOT = SPACE
                   MOVE �������l�v(�����b�m�s:7 - �����b�m�s) TO �������l�Q�v
                   MOVE "YES" TO �I���t���O�S
               END-IF
           END-PERFORM.
028706*================================================================*
       ���я��ݒ� SECTION.
      *
      *      MOVE ���́|�v���r���[�敪 TO �A���|�v���r���[�敪
            MOVE ZERO                 TO �A���|�v���r���[�敪
            CALL   "YIW581".
            CANCEL "YIW581".
028706*================================================================*
       �S�p���p�`�F�b�N SECTION.
      *
028110     MOVE ZERO TO �J�E���^�P �J�E���^�Q.
028120     PERFORM VARYING �J�E���^�P FROM 1 BY 1
028130             UNTIL   (�J�E���^�P > 120) OR (�J�E���^�Q >= 60)
028140         IF ((�����`�F�b�N�v(�J�E���^�P) >= X"81") AND (�����`�F�b�N�v(�J�E���^�P) <= X"9F" )) OR
028150            ((�����`�F�b�N�v(�J�E���^�P) >= X"E0") AND (�����`�F�b�N�v(�J�E���^�P) <= X"FC" ))
                   MOVE �J�E���^�P TO �J�E���^�R
028160             COMPUTE �J�E���^�P = �J�E���^�P + 1
028180             COMPUTE �J�E���^�Q = �J�E���^�Q + 1
028170         ELSE
                   MOVE �J�E���^�P TO �J�E���^�R
028180             COMPUTE �J�E���^�Q = �J�E���^�Q + 1
028190         END-IF
028200     END-PERFORM.
028706*================================================================*
028670*******************************************************************
028680* END PROGRAM YIW1011.
028690*******************************************************************
