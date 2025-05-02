000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAS1022.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090* �h�e�����T�|�[�g�Z���^�[ ���o�f�[�^�쐬�yFPD�����z
000100* �����N��Ver.
000110*      MED = YAS580 
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2021-09-09
000140 DATE-COMPILED.          2021-09-09
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
           SELECT  ���Z�v�g�e      ASSIGN      TO        RECEPTL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  ���Z�|�{�p�a��N��
                                                                ���Z�|���҃R�[�h
                                                                ���Z�|���Z���
                                   ALTERNATE RECORD KEY     IS  ���Z�|���҃R�[�h
                                                                ���Z�|�{�p�a��N��
                                                                ���Z�|���Z���
                                   ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
                                                                ���Z�|�{�p�a��N��
                                                                ���Z�|���҃R�[�h
                                                                ���Z�|���Z���
                                   ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
                                                                ���Z�|���Z���
                                                                ���Z�|�����ی��Ҕԍ�
                                                                ���Z�|���҃R�[�h
                                                                ���Z�|�{�p�a��N��
                                   ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
                                                                ���Z�|�����ی��Ҕԍ�
                                                                ���Z�|���҃R�[�h
                                                                ���Z�|���Z���
                                                                ���Z�|�{�p�a��N��
                                   FILE STATUS              IS  ��ԃL�[
                                   LOCK        MODE         IS  AUTOMATIC.
000540     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000550                             ORGANIZATION             IS  INDEXED
000560                             ACCESS MODE              IS  DYNAMIC
000570                             RECORD KEY               IS  ���|�{�p�a��N��
000580                                                          ���|���҃R�[�h
000590                             ALTERNATE RECORD KEY     IS  ���|���҃R�[�h
000600                                                          ���|�{�p�a��N��
000610                             FILE STATUS              IS  ��ԃL�[
000620                             LOCK        MODE         IS  AUTOMATIC.
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
000940     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000950                             ORGANIZATION             IS  INDEXED
000960                             ACCESS MODE              IS  DYNAMIC
000970                             RECORD KEY               IS  ���|����敪
000980                             FILE STATUS              IS  ��ԃL�[
000990                             LOCK        MODE         IS  AUTOMATIC.
000293     SELECT  �����p���҂e    ASSIGN      TO        CHOKEIL
000294                             ORGANIZATION             IS INDEXED
000295                             ACCESS MODE              IS DYNAMIC
000296                             RECORD KEY               IS ���p�|�{�p�a��N��
000297                                                         ���p�|���҃R�[�h
000298                             ALTERNATE RECORD KEY     IS ���p�|���҃R�[�h
000299                                                         ���p�|�{�p�a��N��
000300                             FILE STATUS              IS ��ԃL�[
000301                             LOCK      MODE           IS AUTOMATIC.
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
000650     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS  ���|�敪�R�[�h
000690                                                          ���|���̃R�[�h
000700                             FILE STATUS              IS  ��ԃL�[
000710                             LOCK        MODE         IS  AUTOMATIC.
001290*
000931     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W1022L.DAT"
000932                             ORGANIZATION             IS  INDEXED
000940                             ACCESS                   IS  DYNAMIC
000950                             RECORD      KEY          IS  ��Q�|�{�p�a��N��
000960                                                          ��Q�|���҃R�[�h
000970                                                          ��Q�|�ی����
000971                             ALTERNATE RECORD KEY     IS  ��Q�|����
000980                             FILE        STATUS       IS  ��ԃL�[
000990                             LOCK        MODE         IS  AUTOMATIC.
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
      *
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001690*                           �m�q�k��  �P�Q�W�n
001700 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
001710     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001750*
001760 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
001770     COPY SEJOHO         OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001780*                           �m�q�k��  �P�Q�W�n
001790 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001800     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001840*                           �m�q�k��  �Q�T�U�n
001850 FD  ������}�X�^      BLOCK   CONTAINS   1   RECORDS.
001860     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000686*                           �m�q�k��  �P�Q�W�n
000687 FD  �����p���҂e          BLOCK   CONTAINS   1   RECORDS.
000688     COPY CHOKEI          OF  XFDLIB  JOINING   ���p   AS  PREFIX.
000700*                           �m�q�k��  �R�Q�O�n
000710 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
000720     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000721*                           �m�q�k��  �Q�T�U�n
000722 FD  �s�����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000723     COPY SITYOSN        OF  XFDLIB  JOINING   �s   AS  PREFIX.
001070*                           �m�q�k��  �P�Q�W�n
001080 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001090     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
004330*                           �m�q�k��  32�n
001581 FD  ��ƃt�@�C���Q RECORD  CONTAINS 80 CHARACTERS.
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
001594         05  ��Q�|���ރR�[�h              PIC 9(2).
001261         05  ��Q�|���R�[�h                PIC X(2).
001595         05  ��Q�|����                    PIC 9(4).
001595         05  ��Q�|�ی���ʏ���            PIC 9(1).
001595         05  ��Q�|�\�[�g�ԍ�              PIC 9(4).
001595         05  ��Q�|�f�[�^�\����            PIC 9(4).
001596         05  ��Q�|�{�l�Ƒ��敪            PIC 9.
001597         05  ��Q�|�ی��Ҕԍ�              PIC X(8).
001591         05  ��Q�|���S����                PIC 9(2).
               05  ��Q�|��p�z                  PIC 9(6).
               05  ��Q�|���S�z                  PIC 9(6).
               05  ��Q�|�����z                  PIC 9(6).
001591         05  ��Q�|���ޕی����            PIC 9(2).
001597         05  ��Q�|���ޕی��Ҕԍ�          PIC X(8).
001260         05  ��Q�|���敪                  PIC 9.
001595         05  ��Q�|�\���t���O              PIC 9(1).
001598         05  FILLER                        PIC X(7).
004471*
004477 FD  ���o�t�@�C�� RECORD IS VARYING IN SIZE
004478               FROM 1 TO 780 DEPENDING ON �����J�E���^.
004479 01 ���|���R�[�h.
004480   03 ���|���R�[�h�f�[�^.
            05 ���|�f�[�^ PIC X OCCURS 1 TO 780 TIMES DEPENDING ON �����J�E���^.
004508*
004509*----------------------------------------------------------------*
004510******************************************************************
004511*                WORKING-STORAGE SECTION                         *
004520******************************************************************
004530 WORKING-STORAGE         SECTION.
004540 01 �L�[����                           PIC X    VALUE SPACE.
004550 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
004560 01 �I���t���O                         PIC X(3) VALUE SPACE.
004570 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
004580 01 �I���t���O�R                       PIC X(3) VALUE SPACE.
004581 01 �I���t���O�S                       PIC X(3) VALUE SPACE.
004590 01 ���s�L�[�v                         PIC X(3)  VALUE SPACE.
004600 01 �{�p�L�^�L�v                       PIC X(3) VALUE SPACE.
004610 01 �t�@�C����                         PIC N(8) VALUE SPACE.
004620 01 �J�E���^                           PIC 9(2) VALUE ZERO.
004621 01 �J�E���^�Q                         PIC 9(2) VALUE ZERO.
004630 01 �����J�E���^                       PIC 9(4) VALUE ZERO.
003630 01 �����b�m�s                         PIC 9(2) VALUE ZERO.
004630 01 ���ʒu�v                           PIC 9(4) VALUE ZERO.
       01 �O���t���O                         PIC 9(1) VALUE ZERO.
       01 �\���t���O�v                       PIC 9(1) VALUE ZERO.
004640 01 �ʔԂv.
          03 �ʔԂv�o                        PIC 9(5) VALUE ZERO.
001220 01 �{���v                             PIC X(2) VALUE SPACE.
001220 01 �����v.
          03 �����v�o                        PIC X(8) VALUE SPACE.
004660 01 �_���t�R�[�h�v.
004670    03 ���v                            PIC N(1) VALUE SPACE.
004680    03 �_���t�R�[�h�����v.
004690       05 �_���t�R�[�h�����P�v         PIC X(4) VALUE SPACE.
004700       05 ����ԍ��R�[�h�v             PIC X(3) VALUE SPACE.
       01 ����ԍ��v                         PIC X(8) VALUE SPACE.
       01 �V�X�e�����t�v.
          03 ����v                          PIC X(2) VALUE SPACE.
          03 �N�����v                        PIC X(6) VALUE SPACE.
          03 ���Ԃv                          PIC X(6) VALUE SPACE.
       01 ���Ԃv�o.
          03 �����b�v                        PIC X(6) VALUE SPACE.
          03 �b�ȉ��v                        PIC X(2) VALUE SPACE.
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
004880 01 ���t�P�O���v                       PIC X(10) VALUE SPACE.
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
005070 01 ���S�����v                         PIC 9(3) VALUE ZERO.
005000 01 ���ʂb�m�s                         PIC 9(2) VALUE ZERO.
005010 01 ���ʂb�m�s�Q                       PIC 9(2) VALUE ZERO.
005020 01 �����������v                       PIC 9(2) VALUE ZERO.
005030 01 �ő哯���������v                   PIC 9(2) VALUE ZERO.
005040 01 �p�����ʐ��v                       PIC 9    VALUE ZERO.
005050 01 �ő�p�����ʐ��v                   PIC 9    VALUE ZERO.
005060 01 �����I���a��N�����v               PIC 9(7) VALUE ZERO.
005070 01 ������ʂv                         PIC 9(2) VALUE ZERO.
005080 01 ���ʃ^�C�v�v                       PIC 9    VALUE ZERO.
003520 01 ��ؕ����q                         PIC X(1) VALUE X"22".
003520 01 ��ؕ����q�v                       PIC X(2) VALUE X"2222".
       01 ��ؕ����q�Q.
          03 ��ؕ����P�o                    PIC X(1) VALUE X"22".
          03 ��ؕ����Q�o                    PIC X(1) VALUE ",".
          03 ��ؕ����R�o                    PIC X(1) VALUE X"22".
       01 ��ؕ����q�R.
          03 ��ؕ����P�o                    PIC X(1) VALUE X"22".
          03 ��ؕ����Q�o                    PIC X(1) VALUE ",".
005090*
005100 01 �x���t���O                         PIC X(3) VALUE SPACE.
005110 01 �x���b�m�s                         PIC 9(5) VALUE ZERO.
005120 01 �x���J�E���^                       PIC 9(4) VALUE ZERO.
005130 01 �x���񐔂v                         PIC 9(4) VALUE ZERO.
005140*
005440** ���������E�������R����敪�p
005450 01 ������������敪�v                 PIC 9 VALUE ZERO.
005460 01 �������R����敪�v                 PIC 9 VALUE ZERO.
005470*
005150 01 �p�������ڂv.
005160   03 �p�������ڂw�v                   PIC X(70) VALUE SPACE.
005170 01 ���{�ꍀ�ڂv.
005180   03 ���{�ꍀ�ڂm�v                   PIC N(35) VALUE SPACE.
005190 01 �p�������ڂQ�v                     PIC X(70) VALUE SPACE.
005200*
005388** �����z�E�l�ߗp
005389 01 �����z�v�s.
005390    03 �����z���l�߂v.
005391      05 �����z���l�߂v�P          PIC X OCCURS 6 VALUE SPACE.
005392    03 �����z�E�l�߂v.
005393      05 �����z�E�l�߂v�P          PIC X OCCURS 6 VALUE ZERO.
005394    03 �����z�����v                PIC 9(6)  VALUE ZERO.
005395    03 �����z�v                    PIC X(6)  VALUE SPACE.
005630*
005640 01 �{�p����N�����v.
          03 �{�p����N���v.
005660       05 �{�p����N�v                 PIC 9(4)  VALUE ZERO.
005670       05 �{�p���v                     PIC 9(2)  VALUE ZERO.
005680    03 �{�p���v                        PIC 9(2)  VALUE ZERO.
001240*
001220 01 �ی���ʂv                         PIC 9(2) VALUE ZERO.
001597 01 �ی��Ҕԍ��v                       PIC X(8) VALUE SPACE.
001250 01 �����於�̂v.
001260    03 ��������於�̂v                PIC X(70) VALUE SPACE.
001270    03 FILLER                          PIC X(10) VALUE SPACE.
005690*
002251 01 ����C���e�c�v                     PIC X VALUE SPACE.
005723 01 FD-NAME                            PIC X(80) VALUE SPACE.
005724*
005725 01 �����v                             PIC X(30)  VALUE SPACE.
005725 01 �Z���v                             PIC X(60)  VALUE SPACE.
004780*
007570 01 �L���v.
007580    03 ����L���v                      PIC N(10)  VALUE SPACE.
007660*
007670 01 �ԍ��v.
007680    03 ����ԍ��v                      PIC X(20)  VALUE SPACE.
005690*
005100 01 ���������t���O                     PIC 9(1) VALUE ZERO.
005100 01 �������R�t���O                     PIC 9(1) VALUE ZERO.
005100 01 �o�߃t���O                         PIC 9(1) VALUE ZERO.
      *
002251 01 ���R�[�h�f�[�^�v                   PIC X(780) VALUE SPACE.
014774*
       01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
      */ZIP���k
       01  �p���P�v PIC X(2001) VALUE SPACE.
       01  �v���O�������v PIC X(8)  VALUE "zip".
005723 01 ���k�t�@�C�����v                   PIC X(80) VALUE SPACE.
005723 01 ���t�@�C�����v                     PIC X(80) VALUE SPACE.
000491* C �A�g�p
000492 01 �t�H���_���v        PIC X(81).
       01 ���Z�܂ƂߑΏۂe                   PIC 9 VALUE ZERO.
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
004091 01 �A���V�|�L�[ IS EXTERNAL.
004092    03  �A���V�|���b�Z�[�W�P           PIC X(40).
004102    03  �A���V�|���b�Z�[�W�Q           PIC X(40).
005860*
005870****************
005880* ��ʓ��͏�� *
005890****************
       01 �A���|��ʏ��x�`�r�P�O�O IS EXTERNAL.
          03 �A���|�h���C�u                  PIC X(1).
      *
      *0:�ʏ� 1:�Œ�p�X
       01 �A���|�Œ�t���O�R�Q�P�P IS EXTERNAL.
          03 �A���|�Œ�t���O                  PIC 9(1).
006830*
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
006280*
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
           PERFORM �{�p�����擾.
006480     PERFORM �N���C�A���g�t�@�C���쐬.
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
000501     MOVE SPACE TO �t�H���_���v.
000502     MOVE "C:\makishisys\mcb_rese\01_jyu\" TO �t�H���_���v.
000503     CALL "delfile2" WITH C LINKAGE USING BY REFERENCE �t�H���_���v.
           CANCEL "delfile2".
006650*================================================================*
006660 �t�@�C���I�[�v�� SECTION.
006670*
006680     OPEN INPUT ��f�ҏ��e
006690         MOVE NC"��" TO �t�@�C����.
006700         PERFORM �I�[�v���`�F�b�N.
004570     OPEN INPUT ���Z�v�g�e.
004580         MOVE NC"���Z" TO �t�@�C����.
004590         PERFORM �I�[�v���`�F�b�N.
006710     OPEN INPUT �����f�[�^�e.
006720             MOVE NC"����" TO �t�@�C����.
006730             PERFORM �I�[�v���`�F�b�N.
006770     OPEN INPUT �{�p�����}�X�^
006780         MOVE NC"�{��" TO �t�@�C����.
006790         PERFORM �I�[�v���`�F�b�N.
006800     OPEN INPUT �����}�X�^.
006810             MOVE NC"����" TO �t�@�C����.
006820             PERFORM �I�[�v���`�F�b�N.
006860     OPEN INPUT ������}�X�^.
006870             MOVE NC"����" TO �t�@�C����.
006880             PERFORM �I�[�v���`�F�b�N.
002647     OPEN INPUT   �����p���҂e.
002648         MOVE NC"�����p���҂e" TO �t�@�C����.
002649         PERFORM �I�[�v���`�F�b�N.
002550     OPEN INPUT �ی��҃}�X�^.
002560         MOVE NC"�ی��҃}�X�^" TO �t�@�C����.
002570         PERFORM �I�[�v���`�F�b�N.
002571     OPEN INPUT �s�����}�X�^
002572         MOVE NC"�s����" TO �t�@�C����.
002573         PERFORM �I�[�v���`�F�b�N.
003540     OPEN INPUT ���̃}�X�^.
003550         MOVE NC"���̃}�X�^"   TO �t�@�C����.
003560         PERFORM �I�[�v���`�F�b�N.
006980*
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
007300     CLOSE ��f�ҏ��e        �{�p�����}�X�^       �����f�[�^�e
007310           �����}�X�^          ������}�X�^         ���Z�v�g�e
007330           �����p���҂e        �ی��҃}�X�^           �s�����}�X�^
                 ���̃}�X�^          ���o�t�@�C��.
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
004910*-----------------------------------------*
004920     CALL "actcshm"  WITH C LINKAGE.
004930*-----------------------------------------*
007450     ACCEPT  �L�[���� FROM CONS.
007460     PERFORM �t�@�C����.
007470     MOVE 99 TO PROGRAM-STATUS.
007480     EXIT PROGRAM.
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
009620*     
009621     NOT INVALID KEY
009622          MOVE �{��|�ڍ��t�����ԍ�     TO ����ԍ��v
009623
009624     END-READ.
      *
           ACCEPT �N�����v FROM DATE.
      *    /* 1980�`2079�N�̊ԂŐݒ� */
           IF �N�����v(1:2) > 80
               MOVE 19 TO ����v
           ELSE
               MOVE 20 TO ����v
           END-IF.
           ACCEPT ���Ԃv�o FROM TIME.
           MOVE �����b�v   TO ���Ԃv.
009630*
007094* �t���b�s�[�}���m�F
           IF �A���|�Œ�t���O = 1
002890        STRING "C:\makishisys\mcb_rese\01_jyu\"      DELIMITED BY SIZE
                     ����ԍ��v            DELIMITED BY SPACE
                     "recedata_"           DELIMITED BY SIZE
                     �V�X�e�����t�v        DELIMITED BY SIZE
                     ".csv"                DELIMITED BY SIZE
                INTO FD-NAME
           ELSE
007096        STRING �A���|�h���C�u        DELIMITED BY SIZE
007097               ":\"                  DELIMITED BY SIZE
                     ����ԍ��v            DELIMITED BY SPACE
                     "recedata_"           DELIMITED BY SIZE
                     �V�X�e�����t�v        DELIMITED BY SIZE
                     ".csv"                DELIMITED BY SIZE
007098              INTO FD-NAME
007099        END-STRING
           END-IF.
007100
007101     OPEN OUTPUT ���o�t�@�C��.
003770     IF ��ԃL�[  NOT =  "00"
002690         MOVE  "�w�肳�ꂽ�h���C�u���Ȃ����A" TO �A���V�|���b�Z�[�W�P
               MOVE  "�������ނ��Ƃ��o���܂���B" TO �A���V�|���b�Z�[�W�Q
002700         CALL   "MSG007"
002710         CANCEL "MSG007"
003820         MOVE 99 TO PROGRAM-STATUS
003830         EXIT PROGRAM
007108     ELSE
007109         PERFORM �I�[�v���`�F�b�N
007110         CLOSE ���o�t�@�C��
007111         CALL "delfile"  WITH C LINKAGE
007112                         USING BY REFERENCE FD-NAME
007113     END-IF.
027970*================================================================*
027980 �N���C�A���g�t�@�C���쐬 SECTION.
027990*
028000     OPEN INPUT ��ƃt�@�C���Q.
028010         MOVE NC"��Q" TO �t�@�C����.
028020         PERFORM �I�[�v���`�F�b�N.
004260     OPEN OUTPUT ���o�t�@�C��.
004270         MOVE NC"���o�t�@�C��" TO �t�@�C����.
004280         PERFORM �I�[�v���`�F�b�N.
028030*
002970     MOVE ZERO  TO ��Q�|���ރR�[�h.
002960     MOVE ZERO  TO ��Q�|���R�[�h.
002970     MOVE ZERO  TO ��Q�|�ی��Ҕԍ�.
002980     MOVE ZERO  TO ��Q�|����.
003020*
003030     START ��ƃt�@�C���Q KEY IS > ��Q�|����
003110     END-START.
003120     IF ��ԃL�[  =  "00"
028190         MOVE SPACE  TO �I���t���O
028200         PERFORM ��ƃt�@�C���Q�Ǎ�
002460         IF  �I���t���O = "YES"
002470             MOVE  NC"�@�f�[�^���O���ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
002480             CALL   "MSG001"
002490             CANCEL "MSG001"
002500             PERFORM �t�@�C����
002510             MOVE 99 TO PROGRAM-STATUS
002520             EXIT PROGRAM
               ELSE
028052             MOVE 780  TO �����J�E���^
005090             PERFORM �^�C�g���Z�b�g
028400             PERFORM ���o�t�@�C������
002530         END-IF
028214         PERFORM UNTIL ( �I���t���O = "YES" )
028220*
028230            MOVE ��Q�|�{�p�a��N�� TO ��|�{�p�a��N��
028240            MOVE ��Q�|���҃R�[�h   TO ��|���҃R�[�h
028250            READ ��f�ҏ��e
028260            INVALID KEY
028270                CONTINUE
028280            NOT INVALID KEY
028297                MOVE "YES"      TO ���s�L�[�v
028299*                
028305                IF ���s�L�[�v = "YES"
028052                    MOVE 780   TO �����J�E���^
011430                    MOVE SPACE TO ���|���R�[�h
028320                    PERFORM ���Z�v�g�e�Z�b�g
028340*
028350                    PERFORM ���R�[�h�Z�b�g
                          MOVE ���R�[�h�f�[�^�v TO ���|���R�[�h
028052*                    MOVE ���ʒu�v         TO �����J�E���^
                          COMPUTE �����J�E���^ = ���ʒu�v - 1
028470                    PERFORM ���o�t�@�C������
028510*
028520                END-IF
028530                PERFORM ��ƃt�@�C���Q�Ǎ�
028540             END-READ
028550         END-PERFORM
028570     ELSE
002470         MOVE  NC"�@�f�[�^���O���ł��B�m�F���ĉ������B" TO �A���|���b�Z�[�W
002480         CALL   "MSG001"
002490         CANCEL "MSG001"
002500         PERFORM �t�@�C����
002510         MOVE 99 TO PROGRAM-STATUS
002520         EXIT PROGRAM
028571     END-IF.
028573*
028580     CLOSE ��ƃt�@�C���Q.
004093*
005820*================================================================*
005830 �^�C�g���Z�b�g SECTION.
005840*
004340     MOVE SPACE  TO ���|���R�[�h.
005842*
005850     STRING ��ؕ����q                   DELIMITED BY SIZE
                  "����h�c"                   DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�ʔ�"                       DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��ی��t���O"               DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�ڍs�f�[�^�t���O"           DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�\���t���O�P"               DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�{�p�N��"                   DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�ԖߍĐ����A������"         DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�ی��Ҕԍ�"                 DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�ی��؋L��"                 DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�ی��ؔԍ�"                 DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�V�l�ی��s�����ԍ�"         DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�V�l�ی��󋋎Ҕԍ�"         DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��Ï�������S�Ҕԍ�"     DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��Ï����󋋎Ҕԍ�"         DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�{�l�E�Ƒ��敪"             DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "����A�c�N�����敪"         DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��ی��Җ��i�J�i�j"         DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��ی��Җ��i�����j"         DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��ی��Җ��O���g�p�t���O"   DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��ی��ҏZ��"               DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��ÎҖ��i�J�i�j"           DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��ÎҖ��i�����j"           DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��ÎҖ��O���g�p�t���O"     DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��p�z"                     DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�ꕔ���S��"                 DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "���S����"                   DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�������z"                   DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�󋋎ҕ��S�z"               DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�����������z"               DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�ʉ@����"                   DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��ÎҖ��@���N����"         DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��ÎҖ��@����"             DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "���ʐ�"                     DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�����̌����L���t���O"       DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�������R�L���t���O"         DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�o��"                       DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��{�p�N��"               DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�ی���ʃO���[�v"           DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "���܂Ƃߗp�\�[�g�ԍ�"     DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "���܂Ƃߗp�̕�����"       DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�f�[�^�\����"               DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "���p�\�L"                   DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�\���t���O"                 DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�\���p����S�Ҕԍ�"       DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "�\���p����S�Җ���"       DELIMITED BY SIZE
                  ��ؕ����q�Q                 DELIMITED BY SIZE
                  "��f��No"                   DELIMITED BY SIZE
                  ��ؕ����q                   DELIMITED BY SIZE
             INTO ���|���R�[�h
           END-STRING.
006150*
009870*================================================================*
009880 ����N�擾 SECTION.
009890*
009900     MOVE ZERO   TO ����N�v.
009910     MOVE �a��v TO ���|�����敪.
009920     READ �����}�X�^
009930     NOT INVALID KEY
009940         COMPUTE �{�p����N�v = ���|�J�n����N + �N�v - 1
009950     END-READ.
010650*================================================================*
010740 ���Z�v�g�e�Ǎ� SECTION.
010750*
010760     READ ���Z�v�g�e NEXT
010770     AT END
010780         MOVE "YES" TO �I���t���O
010790     END-READ.
011406*================================================================*
011410 ���R�[�h�Z�b�g SECTION.
011420*
           MOVE SPACE               TO ���R�[�h�f�[�^�v.
           MOVE 1                   TO ���ʒu�v.
011450*/�ڍ��@�h�c
005420     MOVE ����ԍ��v          TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v    DELIMITED BY SPACE
                  ","               DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011450*/�ʔ�
           COMPUTE �ʔԂv�o = �ʔԂv�o + 1.
005420     MOVE �ʔԂv          TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011450*/�ی��t���O
009622     EVALUATE ���Z�|���Z���
           WHEN 3
               STRING ��ؕ����q    DELIMITED BY SIZE
                      "2"           DELIMITED BY SIZE
                      ��ؕ����q    DELIMITED BY SIZE
                 INTO �p�������ڂQ�v
               END-STRING
           WHEN OTHER
               STRING ��ؕ����q    DELIMITED BY SIZE
                      "0"           DELIMITED BY SIZE
                      ��ؕ����q    DELIMITED BY SIZE
                 INTO �p�������ڂQ�v
               END-STRING
           END-EVALUATE.
005440     STRING �p�������ڂQ�v(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011450*/�ڍs�t���O�A�\���t���O
           STRING ��ؕ����q    DELIMITED BY SIZE
                  "0"           DELIMITED BY SIZE
                  ��ؕ����q    DELIMITED BY SIZE
             INTO �p�������ڂQ�v
           END-STRING.
005440     STRING �p�������ڂQ�v(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
           STRING ��ؕ����q    DELIMITED BY SIZE
                  "0"           DELIMITED BY SIZE
                  ��ؕ����q    DELIMITED BY SIZE
             INTO �p�������ڂQ�v
           END-STRING.
005440     STRING �p�������ڂQ�v(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011450*/�{�p�N��/����
           MOVE ��|�{�p�a��N��    TO �a��N���v.
           PERFORM ����N�擾.
           MOVE ���v   TO �{�p���v.
005420     MOVE �{�p����N���v      TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011450*/�Ԗ߁A���x��
           EVALUATE ���Z�|�����敪
           WHEN 1
               STRING ��ؕ����q    DELIMITED BY SIZE
                      "2"           DELIMITED BY SIZE
                      ��ؕ����q    DELIMITED BY SIZE
                 INTO �p�������ڂQ�v
               END-STRING
           WHEN 2
               STRING ��ؕ����q    DELIMITED BY SIZE
                      "1"           DELIMITED BY SIZE
                      ��ؕ����q    DELIMITED BY SIZE
                 INTO �p�������ڂQ�v
               END-STRING
           WHEN OTHER
               STRING ��ؕ����q    DELIMITED BY SIZE
                      "0"           DELIMITED BY SIZE
                      ��ؕ����q    DELIMITED BY SIZE
                 INTO �p�������ڂQ�v
               END-STRING
           END-EVALUATE.
005440     STRING �p�������ڂQ�v(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011450*/�ی��Ҕԍ�
011690*/�㍂
011700     IF ��|������ NOT = ZERO
005420         MOVE ��|��p���S�Ҕԍ�  TO �p�������ڂv
           ELSE
005420         MOVE ��|�ی��Ҕԍ�      TO �p�������ڂv
           END-IF.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
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
011880     IF (����L���v(1:1) NOT = NC"��")
005420         MOVE �L���v               TO �p�������ڂv
005430         PERFORM �ϊ������p����
005440         STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
           ELSE
               MOVE ��ؕ����q�v         TO �p�������ڂQ�v
005440         STRING �p�������ڂQ�v(1:2)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
011900     END-IF.
011910     IF (�ԍ��v(1:1) NOT = "*") AND (�ԍ��v(1:2) NOT = "��")
005420         MOVE �ԍ��v               TO �p�������ڂv
005430         PERFORM �ϊ������p����
005440         STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
           ELSE
               MOVE ��ؕ����q�v         TO �p�������ڂQ�v
005440         STRING �p�������ڂQ�v(1:2)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
011930     END-IF.
011750*/�V�l
           MOVE ��ؕ����q�v             TO �p�������ڂQ�v.
005440     STRING �p�������ڂQ�v(1:2)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
005440     STRING �p�������ڂQ�v(1:2)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011750*/����
011760     IF ���Z�|���Z��� = 3
005420         MOVE ��|��p���S�Ҕԍ����� TO �p�������ڂv
005430         PERFORM �ϊ������p����
005440         STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
005420         MOVE ��|��v�Ҕԍ�����     TO �p�������ڂv
005430         PERFORM �ϊ������p����
005440         STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
           ELSE
               MOVE ��ؕ����q�v             TO �p�������ڂQ�v
005440         STRING �p�������ڂQ�v(1:2)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
005440         STRING �p�������ڂQ�v(1:2)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
011860     END-IF.
011520*/����
011580     IF ��|�{�l�Ƒ��敪 = 1
               STRING ��ؕ����q    DELIMITED BY SIZE
                      "0"           DELIMITED BY SIZE
                      ��ؕ����q    DELIMITED BY SIZE
                 INTO �p�������ڂQ�v
               END-STRING
011582     ELSE
               STRING ��ؕ����q    DELIMITED BY SIZE
                      "1"           DELIMITED BY SIZE
                      ��ؕ����q    DELIMITED BY SIZE
                 INTO �p�������ڂQ�v
               END-STRING
011584     END-IF.
005440     STRING �p�������ڂQ�v(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011520*/���ʋ敪
011580     IF ��|�ی���� NOT = 5
               EVALUATE ��|���ʋ敪
               WHEN 1
               WHEN 2
               WHEN 3
                   STRING ��ؕ����q    DELIMITED BY SIZE
                          "2"           DELIMITED BY SIZE
                          ��ؕ����q    DELIMITED BY SIZE
                     INTO �p�������ڂQ�v
                   END-STRING
               WHEN 6
                   STRING ��ؕ����q    DELIMITED BY SIZE
                          "1"           DELIMITED BY SIZE
                          ��ؕ����q    DELIMITED BY SIZE
                     INTO �p�������ڂQ�v
                   END-STRING
               WHEN OTHER
                   STRING ��ؕ����q    DELIMITED BY SIZE
                          "0"           DELIMITED BY SIZE
                          ��ؕ����q    DELIMITED BY SIZE
                     INTO �p�������ڂQ�v
                   END-STRING
               END-EVALUATE
           ELSE
               STRING ��ؕ����q    DELIMITED BY SIZE
                      "0"           DELIMITED BY SIZE
                      ��ؕ����q    DELIMITED BY SIZE
                 INTO �p�������ڂQ�v
               END-STRING
           END-IF.
005440     STRING �p�������ڂQ�v(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011585*/���O
005420     MOVE ��|��ی��҃J�i           TO �����v.
           MOVE �����v                     TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
005420     MOVE ��|��ی��Ҏ���           TO �����v.
           MOVE �����v                     TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011600     MOVE ��|��ی��Ҏ���           TO �����v.
           PERFORM �O������.
005420     MOVE �O���t���O                 TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
      *
           MOVE SPACE   TO �Z���v.
005420     STRING ��|�Z���P                       DELIMITED BY SPACE
                  ��|�Z���Q                       DELIMITED BY SPACE
             INTO �Z���v
           END-STRING.
           INSPECT �Z���v REPLACING ALL "," BY ".".
           INSPECT �Z���v REPLACING ALL "�C" BY "�A".
           MOVE �Z���v                     TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
005420     MOVE ��|���҃J�i               TO �����v.
           MOVE �����v                     TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
005420     MOVE ��|���Ҏ���               TO �����v.
           MOVE �����v                     TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011600     MOVE ��|���Ҏ���               TO �����v.
           PERFORM �O������.
005420     MOVE �O���t���O                 TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
010842*
           MOVE ���Z�|���v                 TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
           MOVE ���Z�|�ꕔ���S��     TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
           COMPUTE ���S�����v = ���Z�|���t���� * 10.
005420     MOVE ���S�����v                 TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
           MOVE ���Z�|�������z     TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011760     IF ���Z�|���Z��� = 3
               MOVE ���Z�|�󋋎ҕ��S�z     TO �p�������ڂv
005430         PERFORM �ϊ������p����
005440         STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
               MOVE ���Z�|�����������z     TO �p�������ڂv
005430         PERFORM �ϊ������p����
005440         STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
           ELSE
               STRING ��ؕ����q    DELIMITED BY SIZE
                      "0"           DELIMITED BY SIZE
                      ��ؕ����q    DELIMITED BY SIZE
                 INTO �p�������ڂQ�v
               END-STRING
005440         STRING �p�������ڂQ�v(1:3)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
005440         STRING �p�������ڂQ�v(1:3)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
           END-IF.
012150*/�ʉ@��
005420     MOVE ���Z�|���Z������           TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
012150*/���N����/����
012180     MOVE ��|���Ґ��N����           TO �a��N�����v.
           PERFORM ����N�擾.
           MOVE ���v                       TO �{�p���v.
           MOVE ���v                       TO �{�p���v.
005420     MOVE �{�p����N�����v           TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
012150*/����
012170     IF ��|���Ґ��� = 1
               STRING ��ؕ����q    DELIMITED BY SIZE
                      "0"           DELIMITED BY SIZE
                      ��ؕ����q    DELIMITED BY SIZE
                 INTO �p�������ڂQ�v
               END-STRING
           ELSE
               STRING ��ؕ����q    DELIMITED BY SIZE
                      "1"           DELIMITED BY SIZE
                      ��ؕ����q    DELIMITED BY SIZE
                 INTO �p�������ڂQ�v
               END-STRING
           END-IF.
005440     STRING �p�������ڂQ�v(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
      */�����Z�b�g
028310     PERFORM �����f�[�^�e�Z�b�g
011450*/�����N��/����
           MOVE ���Z�|�����a��N��         TO �a��N���v.
           PERFORM ����N�擾.
           MOVE ���v   TO �{�p���v.
005420     MOVE �{�p����N���v             TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
012150*/�ی���ʃO���[�v
005420     MOVE ��Q�|�ی���ʏ���         TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
012150*/���܂Ƃߗp�\�[�g�ԍ�
005420     MOVE ��Q�|�\�[�g�ԍ�           TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
012150*/���܂Ƃߗp�̕�����
003782     MOVE ��Q�|���ޕی����   TO �ی���ʂv.
003782     MOVE ��Q�|���ޕی��Ҕԍ� TO �ی��Ҕԍ��v.
           IF (��Q�|���R�[�h = SPACE) OR (��Q�|���ރR�[�h = 10 OR 20)
003790         PERFORM ��������擾
           ELSE
              EVALUATE ��Q�|���R�[�h
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
              MOVE ��Q�|���R�[�h    TO ���|���̃R�[�h
              READ ���̃}�X�^
              NOT INVALID KEY
                 MOVE ���|����       TO �����v
              END-READ
              STRING �����v�o                 DELIMITED BY "�@"
                     �{���v                   DELIMITED BY SPACE
                     "�������N�ی��c�̘A����" DELIMITED BY SIZE
                INTO �����於�̂v
              END-STRING
           END-IF.
005420     MOVE �����於�̂v               TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
012150*/�f�[�^�\����
005420     MOVE ��Q�|�f�[�^�\����         TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
012150*/���p�\�L
005420     MOVE ��Q�|���敪               TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
012150*/�\���t���O
005420     MOVE ��Q�|�\���t���O           TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
011750*/����
011760     IF ���Z�|���Z��� = 3
005420         MOVE ��|��p���S�Ҕԍ����� TO �p�������ڂv
005430         PERFORM �ϊ������p����
005440         STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
003782         MOVE ��Q�|�ی����   TO �ی���ʂv
003782         MOVE ��Q�|�ی��Ҕԍ� TO �ی��Ҕԍ��v
003790         PERFORM ��������擾
005420         MOVE �����於�̂v         TO �p�������ڂv
005430         PERFORM �ϊ������p����
005440         STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
           ELSE
               MOVE ��ؕ����q�v             TO �p�������ڂQ�v
005440         STRING �p�������ڂQ�v(1:2)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
005440         STRING �p�������ڂQ�v(1:2)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
011860     END-IF.
011450*/�ڍs�t���O�A�\���t���O
           STRING ��ؕ����q         DELIMITED BY SIZE
                  ��Q�|���҃R�[�h   DELIMITED BY SIZE
                  ��ؕ����q         DELIMITED BY SIZE
             INTO �p�������ڂQ�v
           END-STRING.
005440     STRING �p�������ڂQ�v(1:9)          DELIMITED BY SIZE
             INTO ���R�[�h�f�[�^�v
             WITH POINTER ���ʒu�v
           END-STRING.
      *
003900*================================================================*
003901 ��������擾 SECTION.
003910*
003920     IF ( �ی���ʂv NOT = 05 ) AND ( �ی���ʂv < 10 )
003930*    / ���� /
003960        MOVE �ی���ʂv          TO �ہ|�ی����
003970        MOVE �ی��Ҕԍ��v        TO �ہ|�ی��Ҕԍ�
003980        MOVE SPACE               TO �����於�̂v
003990        READ �ی��҃}�X�^
004000        INVALID KEY
004010            MOVE SPACE           TO �����於�̂v
004020        NOT INVALID KEY
004030            MOVE �ہ|�ی��Җ���  TO �����於�̂v
004040        END-READ
004041     ELSE
004042*    / �V�l�E���� /
004045        MOVE �ی���ʂv          TO �s�|������
004046        MOVE �ی��Ҕԍ��v        TO �s�|�s�����ԍ�
004048        READ �s�����}�X�^
004049        INVALID KEY
004050            MOVE SPACE           TO �����於�̂v
004051        NOT INVALID KEY
                  IF �ی���ʂv = 05
004062                MOVE �s�|�x����  TO �����於�̂v
                  ELSE
004062                MOVE �s�|�s��������  TO �����於�̂v
                  END-IF
004064        END-READ
004065     END-IF.
004066*
015850*================================================================*
015860 �����f�[�^�e�Z�b�g SECTION.
015870******************************************************************
015880* �ő哯�������������߂�B
015890******************************************************************
015900     MOVE ��|�{�p�a��   TO ���|�{�p�a��.
015910     MOVE ��|�{�p�N     TO ���|�{�p�N.
015920     MOVE ��|�{�p��     TO ���|�{�p��.
015930     MOVE ��|���Ҕԍ�   TO ���|���Ҕԍ�.
015940     MOVE ��|�}��       TO ���|�}��.
015950     READ �����f�[�^�e
015960     INVALID KEY
015970         CONTINUE
015980     NOT INVALID KEY
005420         MOVE ���|���ʐ�            TO �p�������ڂv
005430         PERFORM �ϊ������p����
005440         STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO ���R�[�h�f�[�^�v
                 WITH POINTER ���ʒu�v
               END-STRING
               PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
                         UNTIL ( ���ʂb�m�s > ���|���ʐ� )
                  IF (������������敪�v  NOT = 1) AND
                     (���|���������R�[�h(���ʂb�m�s) NOT = ZERO)
                      MOVE 1              TO ���������t���O
                  END-IF
                  IF ���|�o�߃R�[�h(���ʂb�m�s) NOT = ZERO
                      MOVE 1              TO �o�߃t���O
                  END-IF
               END-PERFORM
016140     END-READ.
018470     IF �������R����敪�v  NOT = 1 
028310         PERFORM �����p���҂e�Z�b�g
           END-IF.
005420     MOVE ���������t���O     TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
              INTO ���R�[�h�f�[�^�v
              WITH POINTER ���ʒu�v
           END-STRING.
005420     MOVE �������R�t���O     TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
              INTO ���R�[�h�f�[�^�v
              WITH POINTER ���ʒu�v
           END-STRING.
005420     MOVE �o�߃t���O     TO �p�������ڂv.
005430     PERFORM �ϊ������p����.
005440     STRING �p�������ڂQ�v(1:�����b�m�s + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
              INTO ���R�[�h�f�[�^�v
              WITH POINTER ���ʒu�v
           END-STRING.
      *
015850*================================================================*
015860 �����p���҂e�Z�b�g SECTION.
      *
004502     MOVE ��|�{�p�a��   TO ���p�|�{�p�a��
004503     MOVE ��|�{�p�N     TO ���p�|�{�p�N.
004504     MOVE ��|�{�p��     TO ���p�|�{�p��.
004505     MOVE ��|���Ҕԍ�   TO ���p�|���Ҕԍ�.
004506     MOVE ��|�}��       TO ���p�|�}��.
004507*
004508     READ �����p���҂e
004522     NOT INVALID KEY
004523         IF  (���p�|���R��(1)  = SPACE )  AND
004524             (���p�|���R��(2)  = SPACE )  AND
004525             (���p�|���R��(3)  = SPACE )  AND
004526             (���p�|���R��(4)  = SPACE )  AND
004527             (���p�|���R��(5)  = SPACE )
                   MOVE 0              TO �o�߃t���O
004535         ELSE
                   MOVE 1              TO �o�߃t���O
004537         END-IF
004538     END-READ.
004539*
015350*================================================================*
015360 ���Z�v�g�e�Z�b�g SECTION.
      *
005520     MOVE ��Q�|�{�p�a��N�� TO ���Z�|�{�p�a��N��.
005530     MOVE ��Q�|���҃R�[�h   TO ���Z�|���҃R�[�h.
005540     EVALUATE ��Q�|�ی����
           WHEN 5
               MOVE 2      TO ���Z�|���Z���
           WHEN 51
           WHEN 52
           WHEN 53
           WHEN 54
           WHEN 55
           WHEN 60
               MOVE 3      TO ���Z�|���Z���
           WHEN OTHER
               MOVE 1      TO ���Z�|���Z���
           END-EVALUATE.
           READ ���Z�v�g�e
           INVALID KEY
               MOVE SPACE  TO ���Z�|���R�[�h
           END-READ.
022510*================================================================*
022520 ������擾 SECTION.
022530*
022540     MOVE ZERO TO ���|����敪
022550     READ ������}�X�^
022560     NOT INVALID KEY
008810         MOVE ���|���Z������������敪 TO ������������敪�v
008820         MOVE ���|���Z�������R����敪 TO �������R����敪�v
022580     END-READ.
022841*
022870*================================================================*
023649 �O������ SECTION.
023650*
            MOVE ZERO TO �O���t���O.
      */�������J�E���g
            PERFORM VARYING �J�E���^�Q FROM 50 BY -1
              UNTIL (�J�E���^�Q <= ZERO) OR (�����v(�J�E���^�Q:1) NOT = SPACE)
                CONTINUE
            END-PERFORM.
            MOVE 1 TO �J�E���^.
            PERFORM UNTIL ( �J�E���^ > �J�E���^�Q ) OR ( �O���t���O = 1 )
               IF (( �����v(�J�E���^:1) >= X"20" ) AND ( �����v(�J�E���^:1) <= X"80" )) OR
                  (( �����v(�J�E���^:1) >= X"A0" ) AND ( �����v(�J�E���^:1) <= X"DF" ))
                  COMPUTE �J�E���^ = �J�E���^ + 1
               ELSE
                  IF (( �����v(�J�E���^:2) >= X"F040" ) AND ( �����v(�J�E���^:2) <= X"F9FC" ))
                      MOVE 1 TO �O���t���O
                  END-IF
                  COMPUTE �J�E���^ = �J�E���^ + 2
                END-IF
            END-PERFORM.
023960*
008830*================================================================*
008840 �ϊ������p���� SECTION.
008850*
008860*/�p�������ڂ�"�ł����鏈���B
008870     MOVE SPACE TO �I���t���O�S.
008880     MOVE ��ؕ����q   TO �p�������ڂQ�v.
008890     MOVE �p�������ڂv TO �p�������ڂQ�v(2:69)
008900     PERFORM VARYING �����b�m�s FROM 70 BY -1
008910             UNTIL (�����b�m�s  <= ZERO) OR
008920                   (�I���t���O�S = "YES")
008930         IF �p�������ڂQ�v(�����b�m�s:1) NOT = SPACE
008940            COMPUTE �����b�m�s = �����b�m�s + 1
008950            MOVE ��ؕ����q TO �p�������ڂQ�v(�����b�m�s:1)
008960            MOVE "YES" TO �I���t���O�S
008970         END-IF
008980     END-PERFORM.
028590*================================================================*
028600 ��ƃt�@�C���Q�Ǎ� SECTION.
028610*
028620     READ ��ƃt�@�C���Q NEXT
028630     AT END
028640         MOVE "YES" TO �I���t���O
028650     END-READ.
028695*
028696*================================================================*
028697 ���o�t�@�C������ SECTION.
028698*
028699     WRITE ���|���R�[�h
028700     END-WRITE.
028702     IF ��ԃL�[  NOT =  "00"
028703         MOVE NC"���" TO �t�@�C����
028704         PERFORM �G���[�\��
028705     END-IF.
028706*================================================================*
       ���k�t�@�C���쐬 SECTION.
      *
           IF �A���|�Œ�t���O = 1
002890        STRING "C:\makishisys\mcb_rese\01_jyu\"      DELIMITED BY SIZE
                     ����ԍ��v            DELIMITED BY SPACE
                     "recedata_"           DELIMITED BY SIZE
                     �V�X�e�����t�v        DELIMITED BY SIZE
                     ".ZIP"                DELIMITED BY SIZE
                INTO ���k�t�@�C�����v
           ELSE
007096        STRING �A���|�h���C�u        DELIMITED BY SIZE
007097               ":\"                  DELIMITED BY SIZE
                     ����ԍ��v            DELIMITED BY SPACE
                     "recedata_"           DELIMITED BY SIZE
                     �V�X�e�����t�v        DELIMITED BY SIZE
                     ".ZIP"                DELIMITED BY SIZE
007098              INTO ���k�t�@�C�����v
007099        END-STRING
           END-IF.
           IF �A���|�Œ�t���O = 1
002890        STRING "C:\makishisys\JTAB\" DELIMITED BY SIZE
                     ����ԍ��v            DELIMITED BY SPACE
                     "recedata_"           DELIMITED BY SIZE
                     �V�X�e�����t�v        DELIMITED BY SIZE
                     ".csv"                DELIMITED BY SIZE
                INTO ���t�@�C�����v
           ELSE
007096        STRING �A���|�h���C�u        DELIMITED BY SIZE
007097               ":\"                  DELIMITED BY SIZE
                     ����ԍ��v            DELIMITED BY SPACE
                     "recedata_"           DELIMITED BY SIZE
                     �V�X�e�����t�v        DELIMITED BY SIZE
                     ".csv"                DELIMITED BY SIZE
007098              INTO ���t�@�C�����v
007099        END-STRING
           END-IF.
      *
           MOVE SPACE TO �p���P�v.
      *      
      ** ���k�R�}���h:"a -tzip"
           STRING "a -tzip"  DELIMITED BY SIZE
      *�@�@�@  /���k�t�@�C����
                  " "              DELIMITED BY SIZE
                  ���k�t�@�C�����v DELIMITED BY SPACE
      */
      *�@�@�@  /���̃t�@�C����
                  " "              DELIMITED BY SIZE
                  ���t�@�C�����v   DELIMITED BY SPACE
      *
      *        /�����_�C�A���O�o���Ȃ����w��
                         " -hide"  DELIMITED BY SIZE
      *
      *        /�p�X���[�h�́A -p�ɑ����Ŏw��
                        " -pjksn"  DELIMITED BY SIZE
                INTO �p���P�v
            END-STRING.
            MOVE "zip" TO �v���O�������v.
            CALL �v���O�������v WITH C LINKAGE
                      USING BY REFERENCE �p���P�v.
      *
            IF PROGRAM-STATUS NOT = ZERO
002470         MOVE  NC"�@�@�@�@�@�@���k���s�I�I" TO �A���|���b�Z�[�W
002480         CALL   "MSG001"
002490         CANCEL "MSG001"
            END-IF.     
      *
            MOVE "delfile" TO �v���O�������v.
            CALL �v���O�������v WITH C LINKAGE
                    USING BY REFERENCE ���t�@�C�����v.
028706*================================================================*
028707******************************************************************
028708 END PROGRAM YAS1022.
028709******************************************************************
