000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN662.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*         ���{�_���A���p    �J���e����@�ʏ�p���i�_+����޳�ޔŁj
000100*         MED = YAW660 YHN662P YHN6622P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2012-10-29
000130 DATE-COMPILED.          2012-10-29
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
000260     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  �ہ|�ی����
000300                                                          �ہ|�ی��Ҕԍ�
000310* �����́A�L�[���ڂ̕ی��Җ��̂�ی��҃J�i�ɂ���
000320                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000330                                                          �ہ|�ی��Җ���
000340                                                          �ہ|�ی��Ҕԍ�
000350                             FILE STATUS              IS  ��ԃL�[
000360                             LOCK        MODE         IS  AUTOMATIC.
000370     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000380                             ORGANIZATION             IS  INDEXED
000390                             ACCESS MODE              IS  DYNAMIC
000400                             RECORD KEY               IS  ���|�����敪
000410                             FILE STATUS              IS  ��ԃL�[
000420                             LOCK        MODE         IS  AUTOMATIC.
000430     SELECT  ���̃}�X�^      ASSIGN      TO        MEISYOL
000440                             ORGANIZATION             IS  INDEXED
000450                             ACCESS MODE              IS  DYNAMIC
000460                             RECORD KEY               IS  ���|�敪�R�[�h
000470                                                          ���|���̃R�[�h
000480                             FILE STATUS              IS  ��ԃL�[
000490                             LOCK        MODE         IS  AUTOMATIC.
000500     SELECT  ���Z�v�g�e      ASSIGN      TO        RECEPTL
000510                             ORGANIZATION             IS  INDEXED
000520                             ACCESS MODE              IS  DYNAMIC
000530                             RECORD KEY               IS  ���Z�|�{�p�a��N��
000540                                                          ���Z�|���҃R�[�h
000550                                                          ���Z�|���Z���
000560                             ALTERNATE RECORD KEY     IS  ���Z�|���҃R�[�h
000570                                                          ���Z�|�{�p�a��N��
000580                                                          ���Z�|���Z���
000590                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000600                                                          ���Z�|�{�p�a��N��
000610                                                          ���Z�|���҃R�[�h
000620                                                          ���Z�|���Z���
000630                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000640                                                          ���Z�|���Z���
000650                                                          ���Z�|�����ی��Ҕԍ�
000660                                                          ���Z�|���҃R�[�h
000670                                                          ���Z�|�{�p�a��N��
000680                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
000690                                                          ���Z�|�����ی��Ҕԍ�
000700                                                          ���Z�|���҃R�[�h
000710                                                          ���Z�|���Z���
000720                                                          ���Z�|�{�p�a��N��
000730                             FILE STATUS              IS  ��ԃL�[
000740                             LOCK        MODE         IS  AUTOMATIC.
000750     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000760                             ORGANIZATION             IS  INDEXED
000770                             ACCESS MODE              IS  DYNAMIC
000780                             RECORD KEY               IS  ���|����敪
000790                             FILE STATUS              IS  ��ԃL�[
000800                             LOCK        MODE         IS  AUTOMATIC.
000810     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000820                             ORGANIZATION             IS  INDEXED
000830                             ACCESS MODE              IS  DYNAMIC
000840                             RECORD KEY               IS  ��|�{�p�a��N��
000850                                                          ��|���҃R�[�h
000860                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000870                                                          ��|���҃J�i
000880                                                          ��|���҃R�[�h
000890                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000900                                                          ��|�{�p�a��N��
000910                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000920                                                          ��|�ی����
000930                                                          ��|�ی��Ҕԍ�
000940                                                          ��|���҃R�[�h
000950                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000960                                                          ��|������
000970                                                          ��|��p���S�Ҕԍ�
000980                                                          ��|���҃R�[�h
000990                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
001000                                                          ��|�������
001010                                                          ��|��p���S�Ҕԍ�����
001020                                                          ��|���҃R�[�h
001030                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
001040                                                          ��|�{�p�a��N��
001050                                                          ��|���҃R�[�h
001060                             FILE STATUS              IS  ��ԃL�[
001070                             LOCK        MODE         IS  AUTOMATIC.
001080     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
001090                             ORGANIZATION             IS  INDEXED
001100                             ACCESS MODE              IS  DYNAMIC
001110                             RECORD KEY               IS  �{�L�|�{�p�a��N����
001120                                                          �{�L�|���҃R�[�h
001130                             ALTERNATE RECORD KEY     IS  �{�L�|���҃R�[�h
001140                                                          �{�L�|�{�p�a��N����
001150                             FILE STATUS              IS  ��ԃL�[
001160                             LOCK        MODE         IS  AUTOMATIC.
001170     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
001180                             ORGANIZATION             IS  INDEXED
001190                             ACCESS MODE              IS  DYNAMIC
001200                             RECORD KEY               IS ���|�{�p�a��N��
001210                                                         ���|���҃R�[�h
001220                             ALTERNATE RECORD KEY     IS ���|���҃R�[�h
001230                                                         ���|�{�p�a��N��
001240                             FILE STATUS              IS  ��ԃL�[
001250                             LOCK        MODE         IS  AUTOMATIC.
001260     SELECT  ���������e      ASSIGN      TO        HUGEINL
001270                             ORGANIZATION             IS  INDEXED
001280                             ACCESS MODE              IS  DYNAMIC
001290                             RECORD KEY               IS  �����|�敪�R�[�h
001300                                                          �����|���������R�[�h
001310                             FILE STATUS              IS  ��ԃL�[
001320                             LOCK        MODE         IS  AUTOMATIC.
001330     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
001340                             ORGANIZATION             IS  INDEXED
001350                             ACCESS MODE              IS  DYNAMIC
001360                             RECORD KEY               IS  �s�|������
001370                                                          �s�|�s�����ԍ�
001380                             ALTERNATE RECORD KEY     IS  �s�|������
001390                                                          �s�|�s��������
001400                                                          �s�|�s�����ԍ�
001410                             FILE STATUS              IS  ��ԃL�[
001420                             LOCK        MODE         IS  AUTOMATIC.
001430     SELECT  ���Ə��}�X�^    ASSIGN      TO        JIGYOSL
001440                             ORGANIZATION             IS  INDEXED
001450                             ACCESS MODE              IS  DYNAMIC
001460                             RECORD KEY               IS  ���|�ی����
001470                                                          ���|�ی��Ҕԍ�
001480                                                          ���|�L��
001490                             FILE STATUS              IS  ��ԃL�[
001500                             LOCK        MODE         IS  AUTOMATIC.
000241     SELECT  ���ۏ��e      ASSIGN      TO        SEIHOJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS ���ہ|�{�p�a��N��
000245                                                         ���ہ|���҃R�[�h
000255                             ALTERNATE RECORD KEY     IS ���ہ|���҃R�[�h
000265                                                         ���ہ|�{�p�a��N��
000277                             FILE STATUS              IS ��ԃL�[
000278                             LOCK        MODE         IS AUTOMATIC.
001510     SELECT  �J�Џ��e      ASSIGN      TO        ROUSAIJL
001520                             ORGANIZATION             IS INDEXED
001530                             ACCESS MODE              IS DYNAMIC
001540                             RECORD KEY               IS �J�Ё|�{�p�a��N��
001550                                                         �J�Ё|���҃R�[�h
001560                             ALTERNATE RECORD KEY     IS �J�Ё|���҃R�[�h
001570                                                         �J�Ё|�{�p�a��N��
001580                             FILE STATUS              IS ��ԃL�[
001590                             LOCK        MODE         IS AUTOMATIC.
000241     SELECT  �����ӏ��e    ASSIGN      TO        JIBAIJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS �����|�{�p�a��N��
000245                                                         �����|���҃R�[�h
000277                             ALTERNATE RECORD KEY     IS �����|���҃R�[�h
000278                                                         �����|�{�p�a��N��
000279                             FILE STATUS              IS ��ԃL�[
000280                             LOCK        MODE         IS AUTOMATIC.
000102     SELECT  �ی���Ѓ}�X�^  ASSIGN      TO           HOKCOML
000103                             ORGANIZATION             IS  INDEXED
000104                             ACCESS MODE              IS  DYNAMIC
000105                             RECORD KEY               IS  �ی���|�ی���Дԍ�
000108                             ALTERNATE RECORD KEY     IS  �ی���|�ی���ЃJ�i
000110                                                          �ی���|�ی���Дԍ�
000112                             FILE STATUS              IS  ��ԃL�[
000113                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  ��ƃt�@�C���P  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7211L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  ��P�|�{�p�a��N����
000610                                                          ��P�|���҃R�[�h
000910                             ALTERNATE RECORD KEY     IS  ��P�|�{�p�a��N����
                                                                ��P�|���҃J�i
                                                                ��P�|���҃R�[�h
000910                             ALTERNATE RECORD KEY     IS  ��P�|���҃R�[�h
                                                                ��P�|�{�p�a��N����
000910                             ALTERNATE RECORD KEY     IS  ��P�|���҃J�i
                                                                ��P�|���҃R�[�h
                                                                ��P�|�{�p�a��N����
000620                             FILE        STATUS       IS  ��ԃL�[
000630                             LOCK        MODE         IS  AUTOMATIC.
000980     SELECT  ��ƃt�@�C���Q  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7212L.DAT"
000990                             ORGANIZATION             IS  INDEXED
001000                             ACCESS                   IS  DYNAMIC
001010                             RECORD      KEY          IS  ��Q�|�{�p�a��N��
001020                                                          ��Q�|���҃R�[�h
001030                             FILE        STATUS       IS  ��ԃL�[
001040                             LOCK        MODE         IS  AUTOMATIC.
001600     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
001610                             SYMBOLIC    DESTINATION  IS "PRT"
001620                             FORMAT                   IS  ��`�̖��o
001630                             GROUP                    IS  ���ڌQ���o
001640                             PROCESSING  MODE         IS  ������ʂo
001650                             UNIT        CONTROL      IS  �g������o
001660                             FILE        STATUS       IS  �ʒm���o.
001740******************************************************************
001750*                      DATA DIVISION                             *
001760******************************************************************
001770 DATA                    DIVISION.
001780 FILE                    SECTION.
001790*                           �m�q�k��  �R�Q�O�n
001800 FD  �ی��҃}�X�^        BLOCK   CONTAINS   1   RECORDS.
001810     COPY HOKENS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001820*                           �m�q�k��  �P�Q�W�n
001830 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
001840     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001850*                           �m�q�k��  �P�Q�W�n
001860 FD  ���̃}�X�^          BLOCK   CONTAINS   1   RECORDS.
001870     COPY MEISYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001880*                          �m�q�k��  �P�T�R�U�n
001890 FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
001900     COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001910*                           �m�q�k��  �Q�T�U�n
001920 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
001930     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001940*                           �m�q�k��  �R�Q�O�n
001950 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001960     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001970*                           �m�q�k��  �Q�T�U�n
001980 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
001990     COPY SEKIROK         OF  XFDLIB  JOINING   �{�L AS  PREFIX.
002000*                           �m�q�k��  �P�Q�W�n
002010 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
002020     COPY HUSYOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
002030*                           �m�q�k��  �P�Q�W�n
002040 FD  ���������e          BLOCK   CONTAINS   1   RECORDS.
002050     COPY HUGEIN          OF  XFDLIB  JOINING   ����   AS  PREFIX.
002060*                           �m�q�k��  �Q�T�U�n
002070 FD  �s�����}�X�^        BLOCK   CONTAINS   1   RECORDS.
002080     COPY SITYOSN         OF  XFDLIB  JOINING   �s   AS  PREFIX.
002090*
002100 FD  ���Ə��}�X�^        BLOCK   CONTAINS   1   RECORDS GLOBAL.
002110     COPY JIGYOS          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001080* 
000280 FD  ���ۏ��e          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   ����   AS  PREFIX.
002120*
002130 FD  �J�Џ��e          BLOCK   CONTAINS   1   RECORDS.
002140     COPY ROUSAIJ         OF  XFDLIB  JOINING   �J��   AS  PREFIX.
000284*
000282 FD  �����ӏ��e        BLOCK   CONTAINS   1   RECORDS.
000283     COPY JIBAIJ      OF  XFDLIB  JOINING   ����   AS  PREFIX.
000114*
000115 FD  �ی���Ѓ}�X�^   BLOCK   CONTAINS   1   RECORDS.
000116     COPY HOKENCOM    OF  XFDLIB  JOINING   �ی���   AS  PREFIX.
002150*
001250*****************
001260* ��ƃt�@�C���P *
001270*****************
001280*                         �m�q�k��  �P�U�O�n
001290 FD  ��ƃt�@�C���P RECORD  CONTAINS 160 CHARACTERS.
001300 01 ��P�|���R�[�h.
001310    03 ��P�|���R�[�h�L�[.
001320       05 ��P�|�{�p�a��N����.
001330          07 ��P�|�{�p�a��               PIC 9.
001340          07 ��P�|�{�p�N��.
001350             09 ��P�|�{�p�N              PIC 9(2).
001360             09 ��P�|�{�p��              PIC 9(2).
001370          07 ��P�|�{�p��                 PIC 9(2).
001380       05 ��P�|���҃R�[�h.
001390          07 ��P�|���Ҕԍ�                PIC 9(6).
001400          07 ��P�|�}��                    PIC X(1).
001410    03 ��P�|���R�[�h�f�[�^.
001420       05 ��P�|���҃J�i                   PIC X(50).
001430       05 ��P�|���Ҏ���                   PIC X(50).
001440       05 ��P�|����.
001450          07 ��P�|�������z                PIC 9(5).
001460          07 ��P�|�������z                PIC 9(5).
001470          07 ��P�|��Ë��z                PIC 9(5).
001480          07 ��P�|㪖@���z                PIC 9(5).
001490          07 ��P�|�d�Ë��z                PIC 9(5).
001500          07 ��P�|��p�z                  PIC 9(5).
001510          07 ��P�|�ꕔ���S��              PIC 9(5).
001520       05 FILLER                           PIC X(11).
001530*
001540*                         �m�q�k��  �P�Q�W�n
001550 FD  ��ƃt�@�C���Q RECORD  CONTAINS 128 CHARACTERS.
001560 01 ��Q�|���R�[�h.
001570    03 ��Q�|���R�[�h�L�[.
001580       05 ��Q�|�{�p�a��N��.
001590          07 ��Q�|�{�p�a��                PIC 9.
001600          07 ��Q�|�{�p�N��.
001610             09 ��Q�|�{�p�N               PIC 9(2).
001620             09 ��Q�|�{�p��               PIC 9(2).
001630       05 ��Q�|���҃R�[�h.
001640          07 ��Q�|���Ҕԍ�                PIC 9(6).
001650          07 ��Q�|�}��                    PIC X(1).
001660    03 ��Q�|���R�[�h�f�[�^.
001670       05 ��Q�|�ŏI�ʉ@��.
001680          07 ��Q�|�ʉ@��                  PIC 9(2).
001690          07 ��Q�|�ʉ@��                  PIC 9(2).
001700       05 ��Q�|�{�p��                   PIC 9(2).
001710       05 ��Q�|��p�z                     PIC 9(6).
001720       05 ��Q�|�����z                     PIC 9(6).
001730       05 ��Q�|���S��                     PIC 9(5).
001740       05 ��Q�|�]�A                       OCCURS 5.
001750          07 ��Q�|�]�A�敪                PIC N(1).
001760       05 ��Q�|�����v                     PIC 9(5).
001770       05 ��Q�|�����v                     PIC 9(5).
001780       05 ��Q�|��Ìv                     PIC 9(5).
001790       05 ��Q�|㪖@�v                     PIC 9(5).
001800       05 ��Q�|�d�Ìv                     PIC 9(5).
001810       05 ��Q�|��p�v                     PIC 9(5).
001820       05 ��Q�|���S�v                     PIC 9(5).
001830       05 FILLER                           PIC X(48).
001840*
002160 FD  ����t�@�C��.
002170     COPY YHN662P        OF  XMDLIB.
002210*----------------------------------------------------------------*
002220******************************************************************
002230*                WORKING-STORAGE SECTION                         *
002240******************************************************************
002250 WORKING-STORAGE         SECTION.
002260 01 �L�[����                           PIC X     VALUE SPACE.
002270 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
002280 01 �I���t���O                         PIC X(3)  VALUE SPACE.
002290 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
002300 01 �ڍׂb�m�s                         PIC 9(2)  VALUE ZERO.
002310 01 ���ʂb�m�s                         PIC 9     VALUE ZERO.
002320 01 �����b�m�s                         PIC 9(2)  VALUE ZERO.
002330 01 �����t���O                         PIC X(3)  VALUE SPACE.
002340 01 �t�@�C����                         PIC N(2)  VALUE SPACE.
002350 01 �O�a��v                           PIC 9     VALUE ZERO.
002360 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
002370 01 �S�p��                           PIC X(2)  VALUE X"8140".
002380 01 ���p��                           PIC X(2)  VALUE X"2020".
001210 01 �I�[�v���t���O                     PIC X(3)   VALUE SPACE.
002390 01 �������̂v                         PIC N(6)  VALUE SPACE.
002400 01 ���ʖ��̂v                         PIC N(10) VALUE SPACE.
002410 01 �����ԍ��v                         PIC 9.
002420 01 �����ԍ��q REDEFINES �����ԍ��v.
002430    03 �����ԍ��v�P                    PIC X.
002440*
002450 01 �S�p�����ԍ��v                     PIC N.
002460 01 �S�p�����ԍ��q REDEFINES �S�p�����ԍ��v.
002470    03 �S�p�����ԍ��v�P                PIC X(2).
002480*
002490 01 ���S���v                           PIC 9(3) VALUE ZERO.
002500 01 �b���敪�v                         PIC 9    VALUE ZERO.
002510 01 �{�l���S���v                       PIC 9(3) VALUE ZERO.
002520 01 �Ƒ����S���v                       PIC 9(3) VALUE ZERO.
002530*
002540****************
002550* �ڍחp���[�N *
002560****************
002570* �A��|��������s�p���[�N
002580 01 ����s�r�v                           PIC 9(2) VALUE ZERO.
002590 01 ����s�r�v�o                         PIC 9(2) VALUE ZERO.
002600 01 �������r�v.
002610    03 �����f�[�^�r�v  OCCURS   9.
002620       05 ��ʃR�[�h�r�v               PIC X(4)  VALUE SPACE.
002630       05 ���ʂb�m�s�r�v                 PIC 9(1)  VALUE ZERO.
002640       05 ������ʂr�v                   PIC 9(2)  VALUE ZERO.
002650       05 ���ʂr�v                       PIC 9(2)  VALUE ZERO.
002660       05 ���E�敪�r�v                   PIC 9(1)  VALUE ZERO.
002670       05 �����ʒu�ԍ��r�v               PIC 9(2)  VALUE ZERO.
002680       05 �������r�v                     PIC N(18) VALUE SPACE.
002690       05 �����N�����r�v.
002700          07 �����a��r�v                PIC 9(1)  VALUE ZERO.
002710          07 �����N�r�v                  PIC 9(2)  VALUE ZERO.
002720          07 �������r�v                  PIC 9(2)  VALUE ZERO.
002730          07 �������r�v                  PIC 9(2)  VALUE ZERO.
002740          07 �����N������؂r�v          PIC X(1)  VALUE SPACE.
002750       05 �{�p�J�n�N�����r�v.
002760          07 �{�p�J�n�a��r�v            PIC 9(1)  VALUE ZERO.
002770          07 �{�p�J�n�N�r�v              PIC 9(2)  VALUE ZERO.
002780          07 �{�p�J�n���r�v              PIC 9(2)  VALUE ZERO.
002790          07 �{�p�J�n���r�v              PIC 9(2)  VALUE ZERO.
002800          07 �{�J�N������؂r�v          PIC X(1)  VALUE SPACE.
002810       05 �{�p�I���N�����r�v.
002820          07 �{�p�I���a��r�v            PIC 9(1)  VALUE ZERO.
002830          07 �{�p�I���N�r�v              PIC 9(2)  VALUE ZERO.
002840          07 �{�p�I�����r�v              PIC 9(2)  VALUE ZERO.
002850          07 �{�p�I�����r�v              PIC 9(2)  VALUE ZERO.
002860          07 �{�I�N������؂r�v          PIC X(1)  VALUE SPACE.
002870       05 �J�n�N�����擾�t���O�r         PIC X(3)  VALUE SPACE.
002880       05 �]�A�r�v                       PIC N(4)  VALUE SPACE.
002890       05 �]�A�`�F�b�N�r�v.
002900          07 �����`�F�b�N�r�v            PIC N(1)  VALUE SPACE.
002910          07 ���~�`�F�b�N�r�v            PIC N(1)  VALUE SPACE.
002920          07 �]��`�F�b�N�r�v            PIC N(1)  VALUE SPACE.
002930       05 �{�p�݌v�񐔂r�v               PIC 9(4)  VALUE ZERO.
002940 01 �����������r�v.
002950    03 ���������R�[�h�r�v  OCCURS   9.
002960       09 �������Ҕԍ��r�v               PIC 9(6) VALUE ZERO.
002970       09 �����A�Ԃr�v                   PIC 9(4) VALUE ZERO.
002980*
002990 01 �{�p�a��N�����b�v.
003000   03 �{�p�a��N���b�v.
003010     05 �{�p�a��b�v                   PIC 9 VALUE ZERO.
003020     05 �{�p�N���b�v.
003030        07 �{�p�N�b�v                  PIC 9(2) VALUE ZERO.
003040        07 �{�p���b�v                  PIC 9(2) VALUE ZERO.
003050   03 �{�p���b�v                       PIC 9(2) VALUE ZERO.
003060 01 �{�p����N�v                       PIC 9(4) VALUE ZERO.
003070 01 ���Ґ���N�v                       PIC 9(4) VALUE ZERO.
003080*
003090** ���������p
003100*
003110 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
003120 01 �J�E���^�Q                         PIC 9(2)  VALUE ZERO.
      */�Ǐ󏈒u�o�ߗ��p(�P��)
       01 �J�E���^�R                         PIC 9(1)  VALUE ZERO.
003130*
003140 01 ���������v�s.
003150    03 ���������P�v�s                  PIC X(60) VALUE SPACE.
003160    03 ���������Q�v�s                  PIC X(60) VALUE SPACE.
003170    03 ���������R�v�s                  PIC X(60) VALUE SPACE.
003180    03 ���������S�v�s                  PIC X(60) VALUE SPACE.
003190    03 ���������T�v�s                  PIC X(60) VALUE SPACE.
003200    03 ���������i���o�[�v�s.
003210       05 ���������i���o�[�v�P         PIC X(2)  OCCURS 9 VALUE SPACE.
003220    03 ���������i���o�[�m�v  REDEFINES ���������i���o�[�v�s PIC X(18).
003230 01 �������Ҕԍ��b�v                   PIC 9(6)  VALUE ZERO.
003240 01 �����A�Ԃb�v                       PIC 9(4)  VALUE ZERO.
003250 01 ���������s�a�k.
003260    03 ���������R�[�h�s�a�k            OCCURS 9.
003270       05 �������Ҕԍ��v               PIC 9(6)  VALUE ZERO.
003280       05 �����A�Ԃv                   PIC 9(4)  VALUE ZERO.
003290       05 �����������ʂv               PIC 9  OCCURS 9 VALUE ZERO.
003300 01 �����������e�v.
003310    03 �����������e�����v              PIC X(316) OCCURS 9 VALUE SPACE.
003320    03 �����������e�����w�v.
003330       05 �����������e�P�w�v           PIC X(72)  VALUE SPACE.
003340       05 �����������e�Q�w�v           PIC X(72)  VALUE SPACE.
003350       05 �����������e�R�w�v           PIC X(72)  VALUE SPACE.
003360       05 �����������e�S�w�v           PIC X(72)  VALUE SPACE.
003370       05 �����������e�T�w�v           PIC X(28)  VALUE SPACE.
003380*
003390*/���������̏ڍ׈��/0610
003400 01 ���������v�e�[�u���Q.
003410    03 ���������v�Q                    PIC X(72) OCCURS 45 VALUE SPACE.
003420** ������������敪�p
003430 01 ������������敪�v                 PIC 9 VALUE ZERO.
003440*
003450 01 �����������v.
003460    03 ���������R�[�h�v  OCCURS   9.
003470       09 �����������Ҕԍ��v           PIC 9(6) VALUE ZERO.
003480       09 ���������A�Ԃv               PIC 9(4) VALUE ZERO.
003490****************
003500* �A�����ڑҔ� *
003510****************
003520*    ************
003530*    * ����L�[ *
003540*    ************
003550 01 �Ώۃf�[�^�v�q.
003560    03 �{�p�a��N���v�q.
003570       05 �{�p�a��v�q                  PIC 9(1)  VALUE ZERO.
003580       05 �{�p�N�v�q                    PIC 9(2)  VALUE ZERO.
003590       05 �{�p���v�q                    PIC 9(2)  VALUE ZERO.
003600    03 �ی���ʂv�q                     PIC 9(2)  VALUE ZERO.
003610    03 �ی��Ҕԍ��v�q                   PIC X(10) VALUE SPACE.
003620    03 �{�l�Ƒ��敪�v�q                 PIC 9(1)  VALUE ZERO.
003630    03 ��ی��҃J�i�v�q                 PIC X(20) VALUE SPACE.
003640    03 ���҃R�[�h�v�q.
003650       05 ���Ҕԍ��v�q                  PIC 9(6)  VALUE ZERO.
003660       05 �}�Ԃv�q                      PIC X(1)  VALUE SPACE.
003670    03 ������[�h�e�v�q                 PIC 9(1)  VALUE ZERO.
003680    03 ��o�N�����v�q.
003690       05 ��o�N�v�q                    PIC 9(2)  VALUE ZERO.
003700       05 ��o���v�q                    PIC 9(2)  VALUE ZERO.
003710       05 ��o���v�q                    PIC 9(2)  VALUE ZERO.
003720    03 �i���v�q                         PIC 9(1)  VALUE ZERO.
          03 ���i���v�q                       PIC 9(2) VALUE ZERO.
001630    03 �󎚈ʒu�b�m�s                   PIC 9(2)  VALUE ZERO.
003730************
003740* ������� *
003750************
003760*
003770 01 ������S���v                        PIC 9(1) VALUE ZERO.
003780**************
003790* ��f�ҏ�� *
003800**************
003810 01 ��f�ҏ��v.
003820    03 ���҃R�[�h�v.
003830       05 ���Ҕԍ��v                   PIC 9(6) VALUE ZERO.
003840       05 �}�Ԃv                       PIC X(1) VALUE SPACE.
003850    03 �s�����ԍ��v.
003860*       05 ����s�����ԍ��v             PIC X(8) VALUE SPACE.
003870       05 ����s�����ԍ��v.
003880         07 �s�����ԍ��P�v             PIC X(1) VALUE SPACE.
003890         07 �s�����ԍ��Q�v             PIC X(1) VALUE SPACE.
003900         07 �s�����ԍ��R�v             PIC X(1) VALUE SPACE.
003910         07 �s�����ԍ��S�v             PIC X(1) VALUE SPACE.
003920         07 �s�����ԍ��T�v             PIC X(1) VALUE SPACE.
003930         07 �s�����ԍ��U�v             PIC X(1) VALUE SPACE.
003940         07 �s�����ԍ��V�v             PIC X(1) VALUE SPACE.
003950         07 �s�����ԍ��W�v             PIC X(1) VALUE SPACE.
003960       05 FILLER                       PIC X(2) VALUE SPACE.
003970    03 ��v�Ҕԍ��v.
003980*       05 �����v�Ҕԍ��v             PIC X(7) VALUE SPACE.
003990       05 �����v�Ҕԍ��v.
004000         07 ��v�Ҕԍ��P�v             PIC X(1) VALUE SPACE.
004010         07 ��v�Ҕԍ��Q�v             PIC X(1) VALUE SPACE.
004020         07 ��v�Ҕԍ��R�v             PIC X(1) VALUE SPACE.
004030         07 ��v�Ҕԍ��S�v             PIC X(1) VALUE SPACE.
004040         07 ��v�Ҕԍ��T�v             PIC X(1) VALUE SPACE.
004050         07 ��v�Ҕԍ��U�v             PIC X(1) VALUE SPACE.
004060         07 ��v�Ҕԍ��V�v             PIC X(1) VALUE SPACE.
004070       03 FILLER                       PIC X(13) VALUE SPACE.
004080    03 �L���v                          PIC X(24) VALUE SPACE.
004090    03 �ԍ��v.
004100       05 ����ԍ��v                   PIC X(12) VALUE SPACE.
004110       05 FILLER                       PIC X(18) VALUE SPACE.
004120    03 ��ی��ҏ��v.
004130       05 ��ی��҃J�i�v               PIC X(50) VALUE SPACE.
004140       05 ��ی��Ҏ����v               PIC X(50) VALUE SPACE.
004150*
004160       05 ��ی��Ҙa��`�F�b�N�v.
004170          07 ��ی��Җ����v            PIC N(1) VALUE SPACE.
004180          07 ��ی��ґ吳�v            PIC N(1) VALUE SPACE.
004190          07 ��ی��ҏ��a�v            PIC N(1) VALUE SPACE.
004200          07 ��ی��ҕ����v            PIC N(1) VALUE SPACE.
003940          07 ��ی��җߘa�v            PIC N(1) VALUE SPACE.
004210       05 ���ʃ`�F�b�N�v.
004220          07 �j�`�F�b�N�v              PIC N(1) VALUE SPACE.
004230          07 ���`�F�b�N�v              PIC N(1) VALUE SPACE.
004240       05 ��ی��Ґ��ʂv               PIC 9.
004250       05 ��ی��Ґ��N�����v.
004260          07 ��ی��Ҙa��v            PIC 9 VALUE ZERO.
004270          07 ��ی��ҔN�v              PIC 9(2) VALUE ZERO.
004280          07 ��ی��Ҍ��v              PIC 9(2) VALUE ZERO.
004290          07 ��ی��ғ��v              PIC 9(2) VALUE ZERO.
004300*
004310       05 �X�֔ԍ��v.
004320          07 �X�֔ԍ��P�v              PIC X(3) VALUE SPACE.
004330          07 �X�֔ԍ��Q�v              PIC X(4) VALUE SPACE.
004340       05 ��ی��ҏZ���v.
004350          07 ����Z���P�v               PIC X(50) VALUE SPACE.
004360          07 ����Z���Q�v               PIC X(50) VALUE SPACE.
004370          07 FILLER                     PIC X(18).
004380       05 �d�b�ԍ��v                   PIC X(15) VALUE SPACE.
004390       05 �����d�b�ԍ��v.
004400          07 �����d�b�ԍ��P�v           PIC X(5) VALUE SPACE.
004410          07 �����d�b�ԍ��Q�v           PIC X(5) VALUE SPACE.
004420          07 �����d�b�ԍ��R�v           PIC X(5) VALUE SPACE.
004430    03 ���i�擾�N�����v.
004440       05 ���i�擾�����v               PIC N(2) VALUE SPACE.
004450       05 ���i�擾�a��v               PIC 9(1) VALUE ZERO.
004460       05 ���i�擾�N�v                 PIC 9(2) VALUE ZERO.
004470       05 ���i�擾���v                 PIC 9(2) VALUE ZERO.
004480       05 ���i�擾���v                 PIC 9(2) VALUE ZERO.
004490       05 ���i�a��`�F�b�N�v.
004500          07 ���i���a�`�F�b�N�v        PIC N(1) VALUE SPACE.
004510          07 ���i�����`�F�b�N�v        PIC N(1) VALUE SPACE.
004510          07 ���i�ߘa�`�F�b�N�v        PIC N(1) VALUE SPACE.
004520    03 �L���N�����v.
004530       05 �L���a��v                   PIC 9(1) VALUE ZERO.
004540       05 �L���N�v                     PIC 9(2) VALUE ZERO.
004550       05 �L�����v                     PIC 9(2) VALUE ZERO.
004560       05 �L�����v                     PIC 9(2) VALUE ZERO.
004570    03 ���ҏ��v.
004580       05 ���҃J�i�v                   PIC X(50) VALUE SPACE.
004590       05 ���Ҏ����v                   PIC X(50) VALUE SPACE.
004600       05 �����v.
004610          07 ��������v                PIC N(4) VALUE SPACE.
004620          07 FILLER                    PIC X(4) VALUE SPACE.
004630       05 �{�l�`�F�b�N�v               PIC N(1) VALUE SPACE.
004640       05 �Ƒ��`�F�b�N�v               PIC N(1) VALUE SPACE.
004650       05 ���Ҍ����v                   PIC N(2) VALUE SPACE.
004660       05 �a��`�F�b�N�v.
004670          07 �����`�F�b�N�v            PIC N(1) VALUE SPACE.
004680          07 �吳�`�F�b�N�v            PIC N(1) VALUE SPACE.
004690          07 ���a�`�F�b�N�v            PIC N(1) VALUE SPACE.
004700          07 �����`�F�b�N�v            PIC N(1) VALUE SPACE.
004280          07 �ߘa�`�F�b�N�v            PIC N(1)  VALUE SPACE.
004710       05 ���ҔN�v                     PIC 9(2) VALUE ZERO.
004720       05 ���Ҍ��v                     PIC 9(2) VALUE ZERO.
004730       05 ���ғ��v                     PIC 9(2) VALUE ZERO.
004740       05 ���ҔN��v                   PIC 9(3) VALUE ZERO.
004750       05 ���Ґ��ʂv                   PIC N(4) VALUE SPACE.
004760       05 ���ʃ`�F�b�N�v.
004770          07 ���Ғj�`�F�b�N�v          PIC N(1) VALUE SPACE.
004780          07 ���ҏ��`�F�b�N�v          PIC N(1) VALUE SPACE.
004790       05 ���җX�֔ԍ��v.
004800          07 ���җX�֔ԍ��P�v          PIC X(3) VALUE SPACE.
004810          07 ���җX�֔ԍ��Q�v          PIC X(4) VALUE SPACE.
004820       05 ���ҏZ���v.
004830          07 ���ҏZ���P�v              PIC X(50) VALUE SPACE.
004840          07 ���ҏZ���Q�v              PIC X(50) VALUE SPACE.
004850          07 FILLER                    PIC X(12).
004860       05 ���ғd�b�ԍ��v               PIC X(19) VALUE SPACE.
004870    03 �ی���ʃ`�F�b�N�v.
004880       05 �Еۃ`�F�b�N�v               PIC N(1) VALUE SPACE.
004890       05 �g���`�F�b�N�v               PIC N(1) VALUE SPACE.
004900       05 ���ك`�F�b�N�v               PIC N(1) VALUE SPACE.
004910       05 �D���`�F�b�N�v               PIC N(1) VALUE SPACE.
004920       05 ���σ`�F�b�N�v               PIC N(1) VALUE SPACE.
004930       05 ���ۃ`�F�b�N�v               PIC N(1) VALUE SPACE.
004940       05 �ޖ{�`�F�b�N�v               PIC N(1) VALUE SPACE.
004950       05 �މƃ`�F�b�N�v               PIC N(1) VALUE SPACE.
004960       05 �V�`�F�b�N�v                 PIC N(1) VALUE SPACE.
004970       05 ��`�F�b�N�v                 PIC N(1) VALUE SPACE.
004980       05 ��`�F�b�N�v                 PIC N(1) VALUE SPACE.
004990       05 ���`�F�b�N�v                 PIC N(1) VALUE SPACE.
005000       05 ���`�F�b�N�v                 PIC N(1) VALUE SPACE.
005010       05 �O���`�F�b�N�v               PIC N(1) VALUE SPACE.
005020       05 �P���`�F�b�N�v               PIC N(1) VALUE SPACE.
005030       05 �Q���`�F�b�N�v               PIC N(1) VALUE SPACE.
005040       05 �R���`�F�b�N�v               PIC N(1) VALUE SPACE.
005050       05 �؃`�F�b�N�v                 PIC N(1) VALUE SPACE.
005060    03 ���������v                      PIC X(72) OCCURS 45 VALUE SPACE.
005070**************
005080* ���Ə���� *
005090**************
005100 01 ���Ə����v.
005110*    03 ���Ə����̂v                    PIC X(35) VALUE SPACE.
005120    03 ���Ə����̂v.
005130       05 ������Ə����̂v             PIC X(60) VALUE SPACE.
005140       05 FILLER                       PIC X(5) VALUE SPACE.
005150    03 ���Ə��X�֔ԍ��v.
005160       05 ���Ə��X�֔ԍ��P�v           PIC X(3) VALUE SPACE.
005170       05 ���Ə��X�֔ԍ��Q�v           PIC X(4) VALUE SPACE.
005180    03 ���Ə��Z���v                    PIC X(50) VALUE SPACE.
005180    03 ���Ə��Z���Q�v                  PIC X(50) VALUE SPACE.
005190**************
005200* �������� *
005210**************
005220 01 ��������v.
005230    03 �ی��Ҕԍ��v.
005240       05 ����ی��Ҕԍ��v             PIC X(8) VALUE SPACE.
005250       05 FILLER                       PIC X(2) VALUE SPACE.
005260    03 �����於�̂v.
005270       05 �����於�̂P�v               PIC X(40) VALUE SPACE.
005280       05 �����於�̂Q�v               PIC X(40) VALUE SPACE.
005290    03 �x�����v                        PIC X(40) VALUE SPACE.
005300    03 ����x�����v                    PIC X(10) VALUE SPACE.
005310    03 ������X�֔ԍ��v.
005320       05 ������X�֔ԍ��P�v           PIC X(3) VALUE SPACE.
005330       05 ������X�֔ԍ��Q�v           PIC X(4) VALUE SPACE.
005340    03 ������Z���v.
005350       05 ������Z���P�v               PIC X(40) VALUE SPACE.
005360       05 ������Z���Q�v               PIC X(40) VALUE SPACE.
005370****************
005380* �����f�[�^�e *
005390****************
005400 01 �������v.
005410    03 ���ʐ��v                        PIC 9(1) VALUE ZERO.
005420    03 �����f�[�^���v  OCCURS   9.
005430       05 ��ʃR�[�h�v               PIC X(4) VALUE SPACE.
005440       05 ���ʂb�m�s�v                 PIC 9(1) VALUE ZERO.
005450       05 ������ʂv                   PIC 9(2) VALUE ZERO.
005460       05 ���ʂv                       PIC 9(2) VALUE ZERO.
005470       05 ���E�敪�v                   PIC 9(1) VALUE ZERO.
005480       05 �����ʒu�ԍ��v               PIC 9(2) VALUE ZERO.
005490       05 �������v                     PIC N(18) VALUE SPACE.
005500       05 �����N�����v.
005510          07 �����a��v                PIC 9(1) VALUE ZERO.
005520          07 �����N�v                  PIC 9(2) VALUE ZERO.
005530          07 �������v                  PIC 9(2) VALUE ZERO.
005540          07 �������v                  PIC 9(2) VALUE ZERO.
005550          07 �����N������؂v          PIC X(1) VALUE SPACE.
005560       05 �{�p�J�n�N�����v.
005570          07 �{�p�J�n�a��v            PIC 9(1) VALUE ZERO.
005580          07 �{�p�J�n�N�v              PIC 9(2) VALUE ZERO.
005590          07 �{�p�J�n���v              PIC 9(2) VALUE ZERO.
005600          07 �{�p�J�n���v              PIC 9(2) VALUE ZERO.
005610          07 �{�J�N������؂v          PIC X(1) VALUE SPACE.
005620       05 �{�p�I���N�����v.
005630          07 �{�p�I���a��v            PIC 9(1) VALUE ZERO.
005640          07 �{�p�I���N�v              PIC 9(2) VALUE ZERO.
005650          07 �{�p�I�����v              PIC 9(2) VALUE ZERO.
005660          07 �{�p�I�����v              PIC 9(2) VALUE ZERO.
005670          07 �{�I�N������؂v          PIC X(1) VALUE SPACE.
005680       05 �J�n�N�����擾�t���O         PIC X(3) VALUE SPACE.
005690       05 �]�A�`�F�b�N�v.
005700          07 �����`�F�b�N�v            PIC N(1) VALUE SPACE.
005710          07 ���~�`�F�b�N�v            PIC N(1) VALUE SPACE.
005720          07 �]��`�F�b�N�v            PIC N(1) VALUE SPACE.
005730**********
005740* ���v�s *
005750**********
005760 01 ���v�s�v.
005770    03 �����I�����v                    PIC 9(2) VALUE ZERO.
005780    03 �Ώې���v                      PIC 9(4) VALUE ZERO.
005790    03 ���v                            PIC 9(3) VALUE ZERO.
005800    03 �]�v                            PIC 9(3) VALUE ZERO.
005810*
005820*
005830*/�N�������ی��҃f�[�^�̃��[�N
005840 01 �ە��S���敪�v                     PIC 9    VALUE ZERO.
005850 01 �ۖ{�l���S���v                     PIC 9(3) VALUE ZERO.
005860 01 �ۉƑ����S���v                     PIC 9(3) VALUE ZERO.
005870 01 �ۖ{�l���S�����v                   PIC 9(3) VALUE ZERO.
005880 01 �ۉƑ����S�����v                   PIC 9(3) VALUE ZERO.
005890*
005900 01 ��r�a��N���v.
005910    03 ��r�a��v                      PIC 9    VALUE ZERO.
005920    03 ��r�N�v                        PIC 9(2) VALUE ZERO.
005930    03 ��r���v                        PIC 9(2) VALUE ZERO.
005940*
005950*/�V�l�ی������̑Ή�0101
005960 01 �V�l���S�v�Z�敪�v                 PIC 9    VALUE ZERO.
005970 01 ���S���p��ʂv                     PIC 9(2) VALUE ZERO.
005980 01 �������S�敪�v                     PIC 9(2) VALUE ZERO.
005990*
006000*
006010** �������S���p(14/10�`)
006020 01 �������S���v�v.
006030    03 �������S���v                    PIC 9(3) VALUE ZERO.
006040    03 �������S���v�P                  PIC 9    VALUE ZERO.
006050    03 �������S�������v�P              PIC X    VALUE SPACE.
006060    03 �������S���\���v                PIC X(6) VALUE SPACE.
006070*
006080** �o�[�R�[�h�敪�p
006090 01 �o�[�R�[�h�g�p�敪�v               PIC 9 VALUE ZERO.
       01 �R�����g���v.
          03 �Ǐ�v OCCURS 8 PIC X(48) VALUE SPACE.
          03 ���u�v OCCURS 8 PIC X(67) VALUE SPACE.
          03 �o�߂v OCCURS 8 PIC X(48) VALUE SPACE.
001390**
001680 01 �{�p�N���v.
001690    03 �{�p�a��v                      PIC 9(1) VALUE ZERO.
001690    03 �{�p�N�v                        PIC 9(2) VALUE ZERO.
001700    03 �{�p���v                        PIC 9(2) VALUE ZERO.
001360 01 ���v�v.
001380    03 �������v�v                      PIC 9(5) VALUE ZERO.
001390    03 �������v�v                      PIC 9(5) VALUE ZERO.
001400    03 ��×��v�v                      PIC 9(5) VALUE ZERO.
001400    03 㪖@���v�v                      PIC 9(5) VALUE ZERO.
001400    03 �d�×��v�v                      PIC 9(5) VALUE ZERO.
001400    03 ���S���v�v                      PIC 9(5) VALUE ZERO.
001400*
001360 01 �����v.
001380    03 �������v                        PIC 9(5) VALUE ZERO.
001390    03 �������v                        PIC 9(5) VALUE ZERO.
001400    03 ��×��v                        PIC 9(5) VALUE ZERO.
001400    03 㪖@���v                        PIC 9(5) VALUE ZERO.
001400    03 �d�×��v                        PIC 9(5) VALUE ZERO.
001400    03 ���S���v                        PIC 9(5) VALUE ZERO.
001831* POWER COBOL�p
001832 01 dll-name  PIC X(260)  VALUE SPACE.
001833 01 form-name PIC X(14)   VALUE SPACE.
006100******************************************************************
006110 01 �������.
006120     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
006130     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
006140     03 ������ʂo                     PIC X(2) VALUE SPACE.
006150     03 �g������o.
006160         05 �[������o.
006170             07 �ړ������o             PIC X(1).
006180             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
006190         05 �ڍא���o                 PIC X(2).
006200     03 �ʒm���o                     PIC X(2) VALUE SPACE.
006210     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
006220*
006230 01 �v�Z�@����N�v                     PIC 9(2) VALUE ZERO.
006240* ���t�v�n�q�j
006250 01 �a��I���N�v                       PIC 9(4) VALUE ZERO.
006260 01 �v�Z�@����.
006270    03 �v�Z�@����N                    PIC 9(4) VALUE ZERO.
006280    03 �v�Z�@�����                  PIC 9(4) VALUE ZERO.
006290 01 �v�Z�@����q REDEFINES �v�Z�@����.
006300    03 �v�Z�@���I                      PIC 9(2).
006310    03 �v�Z�@���t                      PIC 9(6).
006320    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
006330       05 �v�Z�@�N��                   PIC 9(4).
006340       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
006350         07 �v�Z�@�N                   PIC 9(2).
006360         07 �v�Z�@��                   PIC 9(2).
006370       05 �v�Z�@��                     PIC 9(2).
006380*
006390* C �A�g�p
006400 01  �����P�v        PIC X(4096).
006410 01  �����Q�v        PIC X(512).
006420 01  �v���O�������v  PIC X(8)  VALUE "strmoji2".
006421*
006422 01 �����v���O�������v     PIC X(8) VALUE "MOJI2".
006423*
006430******************************************************************
006440*                          �A������                              *
006450******************************************************************
006460*
006470**********************
006480* ���b�Z�[�W�\���L�[ *
006490**********************
006500 01 �A���|�L�[ IS EXTERNAL.
006510    03  �A���|���b�Z�[�W                 PIC N(20).
003540*
       01 �A���b�Z�[�W�o���O�O�T�P IS EXTERNAL.
          03 �A���o�|���b�Z�[�W�ԍ�                PIC 9(2).
          03 �A���o�|���b�Z�[�W.
             05 �A���o�|���b�Z�[�W���e             PIC X(40) OCCURS 6.
          03 �A���o�|���b�Z�[�W�P                  PIC X(20).
          03 �A���o�|���b�Z�[�W�Q                  PIC X(12).
          03 �A���o�|�Ԃ�l                        PIC X.
006520*
006530************
006540* ����L�[ *
006550************
006560 01 �A��|�Ώۃf�[�^ IS EXTERNAL.
006570    03 �A��|�{�p�a��N��.
006580       05 �A��|�{�p�a��                  PIC 9(1).
006590       05 �A��|�{�p�N                    PIC 9(2).
006600       05 �A��|�{�p��                    PIC 9(2).
006610    03 �A��|�ی����                     PIC 9(2).
006620    03 �A��|�ی��Ҕԍ�                   PIC X(10).
006630    03 �A��|�{�l�Ƒ��敪                 PIC 9(1).
006640    03 �A��|��ی��҃J�i                 PIC X(20).
006650    03 �A��|���҃R�[�h.
006660       05 �A��|���Ҕԍ�                  PIC 9(6).
006670       05 �A��|�}��                      PIC X(1).
006680    03 �A��|������[�h�e                 PIC 9(1).
006690*/�ڍ�
006700    03 �A��|�ی��؈���敪               PIC 9(1).
006710    03 �A��|�����ڍ� OCCURS 7.
006720       05 �A��|��������s                PIC 9(1).
006730       05 �A��|���ʈ���敪              PIC 9(1).
006740       05 �A��|�]�A����敪              PIC 9(1).
006750       05 �A��|��������敪              PIC 9(1).
006760*/
006770 01 �A��|�Ώۃf�[�^�ǉ� IS EXTERNAL.
006780    03 �A��|���ʊەt��                   PIC 9(1).
006790*/�N����0304
006800*/���͉��(660,6601)�ƐV�p��(6621)�Ƃ̘A�����ځB(���p���A���ʗp�����̂o�f��ς����������ׁA�V���ɍ��܂�)
006810    03 �A��|�N�����敪                 PIC 9(1).
006820*/���������̏ڍ׃��[�h�Ή�/0610
006830    03 �A��|�����J�n�s                   PIC 9(2).
006840*/���k�x���L�ڒǉ�/080812
006850    03 �A��|���k�x���L��                 PIC 9(1).
006860*/���N���ƈ�� �O�F�g�Ȃ� �P�F���N���� �Q�F���N�̂� /0402
006870    03 �A��|�g����e                     PIC 9(1).
006880*/�������R0311
006890    03 �A��|��������e                   PIC 9(1).
      *
       01 �A��|�Ώۃf�[�^�x�g�m�V�Q�O IS EXTERNAL.
          03 �A��|���v���[�h                   PIC 9(1).
          03 �A��|���׃��[�h                   PIC 9(1).
          03 �A��|���i��                       PIC 9(2).
      */�ڍ�
          03 �A��|��������敪                 PIC 9(1).
          03 �A��|�{�p����敪                 PIC 9(1).
006900*
006910 01 �A���|�\���t���O�U�U�O IS EXTERNAL.
006920    03 �A���|�v���r���[�敪               PIC 9(1).
006930* ���S���擾�p14/10�`
006940 01 �A���|���S���擾�L�[ IS EXTERNAL.
006950    03 �A���|�{�p�a��N��.
006960       05 �A���|�{�p�a��               PIC 9.
006970       05 �A���|�{�p�N��.
006980          07 �A���|�{�p�N              PIC 9(2).
006990          07 �A���|�{�p��              PIC 9(2).
007000    03 �A���|���҃R�[�h.
007010       05 �A���|���Ҕԍ�               PIC 9(6).
007020       05 �A���|�}��                   PIC X.
007030    03 �A���|���ە��S��                PIC 9(3).
007040    03 �A���|���ۖ{�̕��S��            PIC 9(3).
007050    03 �A���|���ە��S��                PIC 9(3).
007060    03 �A���|�Q�V�V���S��              PIC 9(3).
007070    03 �A���|�������S��                PIC 9(3).
007080    03 �A���|���ʗp���S��              PIC 9(3).
007090*
007100************************************
007110* �v�����^�t�@�C���쐬�p           *
007120************************************
007130 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
007140     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
007150     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
007160     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
007170     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
007180************************************
007190* �v�����^�t�@�C���쐬����p       *
007200************************************
007210 01 �g�A����o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
007220     03 �g�A����o�q�s�e�|�p�����         PIC X(8).
007230*
007231*--------------------------------------------------------*
007232* �Í������p
007233 01 �A�Í������|�Í���� IS EXTERNAL.
007234    03 �A�Í������|���͏��.
007235       05 �A�Í������|�L��               PIC X(24).
007236       05 �A�Í������|�ԍ�               PIC X(30).
007237       05 �A�Í������|�Í�������.
007238         07 �A�Í������|�Í����Ҕԍ�     PIC X(6).
007239         07 �A�Í������|�Í�����L��     PIC X.
007240         07 �A�Í������|�Í�����ԍ�     PIC X.
007241         07 �A�Í������|�Í��L��         PIC X(24).
007242         07 �A�Í������|�Í��ԍ�         PIC X(30).
007243    03 �A�Í������|�o�͏��.
007244       05 �A�Í������|���������L��       PIC X(24).
007245       05 �A�Í������|���������ԍ�       PIC X(30).
007246*--------------------------------------------------------*
007247******************************************************************
007250*                      PROCEDURE  DIVISION                       *
007260******************************************************************
007270 PROCEDURE               DIVISION.
007280************
007290*           *
007300* ��������   *
007310*           *
007320************
007330     PERFORM �v�����^�t�@�C���쐬.
007340     PERFORM ������.
007350************
007360*           *
007370* �又��     *
007380*           *
007390************
007400* ���
007430     PERFORM �A�����ڑҔ�.
007440     PERFORM ����Z�b�g.
007520************
007530*           *
007540* �I������   *
007550*           *
007560************
007570     PERFORM �I������.
007580     EXIT PROGRAM.
007590*
007600*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
007610*================================================================*
007620 �v�����^�t�@�C���쐬 SECTION.
007630*================================================================*
007640*   / ������ /
007650     MOVE SPACE TO �g�A�o�q�s�e�|�쐬�f�[�^.
007660     INITIALIZE �g�A�o�q�s�e�|�쐬�f�[�^.
007670     MOVE SPACE TO �g�A����o�q�s�e�|�쐬�f�[�^.
007680     INITIALIZE �g�A����o�q�s�e�|�쐬�f�[�^.
007690*
007700*
007710*--���� �ύX�ӏ� ����--------------------------------------*
007720*   �g�p����p����ʃZ�b�g
007730     MOVE "KARUTE"              TO �g�A����o�q�s�e�|�p�����.
007740*   �g�p����v�����^�t�@�C�����Z�b�g
007750     MOVE "PRTF002"             TO �g�A�o�q�s�e�|�t�@�C����.
007760*
007770*   �g�p���钠�[�v���O�������Z�b�g
007790     MOVE "YHN662"              TO �g�A�o�q�s�e�|���[�v���O������.
007830*
007840*--����-----------------------------------------------------*
007850*
007860*   / �v���r���[�敪�Z�b�g /
007870     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
007880*     MOVE 1 TO �g�A�o�q�s�e�|�v���r���[�敪.
007890*
007900     CALL   "CRTPRTF".
007910     CANCEL "CRTPRTF".
007920*
007930*================================================================*
007940 ������ SECTION.
007950*
007960     PERFORM �t�@�C���I�[�v��.
007970*    /* ���ݓ��t�擾 */
007980     ACCEPT �v�Z�@���t FROM DATE.
007990*    /* 1980�`2079�N�̊ԂŐݒ� */
008000     IF �v�Z�@�N > 80
008010         MOVE 19 TO �v�Z�@���I
008020     ELSE
008030         MOVE 20 TO �v�Z�@���I
008040     END-IF.
008050     PERFORM �J�����g�����擾.
008060     PERFORM �a��I���N�擾.
008070     COMPUTE �v�Z�@����N�v = �v�Z�@����N - 1988.
008080*================================================================*
008090 �J�����g�����擾 SECTION.
008100*
008110     MOVE ZEROS TO ���|����敪.
008120     READ ������}�X�^
008130     NOT INVALID KEY
008140         MOVE ���|�J�����g���� TO �J�����g�����v
008150         MOVE ���|�J���e������������敪 TO ������������敪�v
008160         MOVE ���|�o�[�R�[�h�Q�R�Q�b�g�p�敪 TO �o�[�R�[�h�g�p�敪�v
008170     END-READ.
008180*
008190*================================================================*
008200 �a��I���N�擾 SECTION.
008210*
008220     MOVE �J�����g�����v TO ���|�����敪.
008230     READ �����}�X�^
008240     INVALID KEY
008250         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
008260         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
008270                                                  UPON CONS
008280*-----------------------------------------*
008290         CALL "actcshm"  WITH C LINKAGE
008300*-----------------------------------------*
008310         ACCEPT  �L�[���� FROM CONS
008320         PERFORM �I������
008330         EXIT PROGRAM
008340     NOT INVALID KEY
008350         COMPUTE �O�a��v = �J�����g�����v - 1
008360         MOVE �O�a��v TO ���|�����敪
008370         READ �����}�X�^
008380         INVALID KEY
008390             DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
008400             DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
008410                                                      UPON CONS
008420*-----------------------------------------*
008430             CALL "actcshm"  WITH C LINKAGE
008440*-----------------------------------------*
008450             ACCEPT  �L�[���� FROM CONS
008460             PERFORM �I������
008470             EXIT PROGRAM
008480         NOT INVALID KEY
008490             MOVE ���|�I������N TO �a��I���N�v
008500         END-READ
008510     END-READ.
008520*
008530*================================================================*
008540 �A�����ڑҔ� SECTION.
008550*
008560     INITIALIZE �Ώۃf�[�^�v�q.
008570     MOVE �A��|�{�p�a��      TO �{�p�a��v�q.
008580     MOVE �A��|�{�p�N        TO �{�p�N�v�q.
008590     MOVE �A��|�{�p��        TO �{�p���v�q.
008600     MOVE �A��|�ی����      TO �ی���ʂv�q.
008610     MOVE �A��|�ی��Ҕԍ�    TO �ی��Ҕԍ��v�q.
008620     MOVE �A��|�{�l�Ƒ��敪  TO �{�l�Ƒ��敪�v�q.
008630     MOVE �A��|��ی��҃J�i  TO ��ی��҃J�i�v�q.
008640     MOVE �A��|���Ҕԍ�      TO ���Ҕԍ��v�q.
008650     MOVE �A��|�}��          TO �}�Ԃv�q.
008660     MOVE �A��|������[�h�e  TO ������[�h�e�v�q.
008590     MOVE �A��|���i��        TO ���i���v�q.
008670*================================================================*
008680 �t�@�C���I�[�v�� SECTION.
008690*
008700     OPEN INPUT   �ی��҃}�X�^
008710         MOVE NC"�ی���" TO �t�@�C����.
008720         PERFORM �I�[�v���`�F�b�N.
008730     OPEN INPUT   �����}�X�^
008740         MOVE NC"����" TO �t�@�C����.
008750         PERFORM �I�[�v���`�F�b�N.
008760     OPEN INPUT   ���̃}�X�^
008770         MOVE NC"����" TO �t�@�C����.
008780         PERFORM �I�[�v���`�F�b�N.
008790     OPEN INPUT   ���Z�v�g�e
008800         MOVE NC"���Z" TO �t�@�C����.
008810         PERFORM �I�[�v���`�F�b�N.
008820     OPEN INPUT   ������}�X�^
008830         MOVE NC"������" TO �t�@�C����.
008840         PERFORM �I�[�v���`�F�b�N.
008850     OPEN INPUT   ��f�ҏ��e.
008860         MOVE NC"���" TO �t�@�C����.
008870         PERFORM �I�[�v���`�F�b�N.
008880     OPEN INPUT   �{�p�L�^�e.
008890         MOVE NC"�{�L�e" TO �t�@�C����.
008900         PERFORM �I�[�v���`�F�b�N.
008910     OPEN INPUT   �����f�[�^�e.
008920         MOVE NC"����" TO �t�@�C����.
008930         PERFORM �I�[�v���`�F�b�N.
008940     OPEN INPUT   ���������e.
008950         MOVE NC"��������" TO �t�@�C����.
008960         PERFORM �I�[�v���`�F�b�N.
008970     OPEN INPUT �s�����}�X�^.
008980         MOVE NC"�s����" TO �t�@�C����.
008990         PERFORM �I�[�v���`�F�b�N.
009000     OPEN INPUT ���Ə��}�X�^.
009010         MOVE NC"���Ə�" TO �t�@�C����.
009020         PERFORM �I�[�v���`�F�b�N.
009030     OPEN INPUT �J�Џ��e.
009040         MOVE NC"�J�Џ��e" TO �t�@�C����.
009050         PERFORM �I�[�v���`�F�b�N.
006630     OPEN INPUT ���ۏ��e.
006640         MOVE NC"����" TO �t�@�C����.
006650         PERFORM �I�[�v���`�F�b�N.
009290     OPEN INPUT �����ӏ��e.
009300         MOVE NC"����" TO �t�@�C����.
009310         PERFORM �I�[�v���`�F�b�N.
009290     OPEN INPUT �ی���Ѓ}�X�^.
009300         MOVE NC"�ۉ�" TO �t�@�C����.
009310         PERFORM �I�[�v���`�F�b�N.
009290     OPEN INPUT ��ƃt�@�C���P.
009300         MOVE NC"��P" TO �t�@�C����.
009310         PERFORM �I�[�v���`�F�b�N.
009290     OPEN INPUT ��ƃt�@�C���Q.
009300         MOVE NC"��Q" TO �t�@�C����.
009310         PERFORM �I�[�v���`�F�b�N.
009080*     OPEN I-O   ����t�@�C��.
009090*         MOVE NC"���" TO �t�@�C����.
009100*         PERFORM �I�[�v���`�F�b�N.
009160*================================================================*
009170 �I�[�v���`�F�b�N SECTION.
009180*
009190     IF ��ԃL�[  NOT =  "00"
009200         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
009210         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
009220         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
009230                                                 UPON CONS
009240*-----------------------------------------*
009250         CALL "actcshm"  WITH C LINKAGE
009260*-----------------------------------------*
009270         ACCEPT  �L�[���� FROM CONS
009280         PERFORM �t�@�C����
009290         EXIT PROGRAM.
009300*================================================================*
009310 �t�@�C���� SECTION.
009320*
009330     CLOSE �ی��҃}�X�^   �����}�X�^    ���̃}�X�^ 
009340           ������}�X�^ ���Z�v�g�e    ���Ə��}�X�^
009350           ��f�ҏ��e   �{�p�L�^�e    �s�����}�X�^
009360           �����f�[�^�e   ���������e    �J�Џ��e
                 ���ۏ��e     �����ӏ��e  �ی���Ѓ}�X�^
                 ��ƃt�@�C���P ��ƃt�@�C���Q.
002990     IF ( �I�[�v���t���O = "YES" )
007470        CLOSE ����t�@�C��
           END-IF.
009430*================================================================*
009440 �I������ SECTION.
009450*
009460     PERFORM �t�@�C����.
009470*================================================================*
009480 ����Z�b�g SECTION.
009490*
009500     EVALUATE ������[�h�e�v�q
009510     WHEN 0
009520         PERFORM ����Z�b�g�P
021230         PERFORM ��������P
009530     WHEN 1
009540         PERFORM ����Z�b�g�Q
021230         PERFORM ��������Q
009550     WHEN 2
009560         PERFORM ����Z�b�g�R
021250         PERFORM ��������R
009570*/�ڍ׃��[�h�p
009580     WHEN 4
009590         PERFORM ����Z�b�g�T
021270         IF �A��|�ی��؈���敪 = 1
021280             PERFORM ��������Q
021290         END-IF
021270         IF (�A��|��������敪 = 1) OR (�A��|�{�p����敪 = 1)
021300             PERFORM ��������R
               END-IF
009570*/�{�p�̎����p
009580     WHEN 5
009590         PERFORM ����Z�b�g�U
021250         PERFORM ��������R
009600     WHEN OTHER
009610         CONTINUE
009620     END-EVALUATE.
009630*
009640*================================================================*
009650 ����Z�b�g�P SECTION.
009660*
009670     INITIALIZE YHN662P.
009670     MOVE SPACE TO YHN662P.
009680     MOVE "X" TO EDIT-MODE OF ���҃R�[�h�a.
009690     INITIALIZE ��f�ҏ��v.
009700     INITIALIZE ���Ə����v.
009710     INITIALIZE ��������v.
009720     INITIALIZE �������v.
009730     PERFORM ���S���擾.
009740     PERFORM ��������擾.
009750     PERFORM ��f�ҏ��擾.
009760     PERFORM �����f�[�^�擾.
           PERFORM �Ǐ󏈒u�o�߃Z�b�g.
009770*
009780     IF ������������敪�v  NOT = 1 
009790        PERFORM ���������擾
009800     END-IF.
009810*
009840     PERFORM ����㕔�Z�b�g.
009850     PERFORM ��������Z�b�g.
009850     PERFORM ����{�p�Z�b�g.
009900*// TEST
009910******     PERFORM �󎚃e�X�g.
009920*================================================================*
009930 ����Z�b�g�Q SECTION.
009940*
009950     INITIALIZE YHN662P.
009960     MOVE "X" TO EDIT-MODE OF ���҃R�[�h�a.
009970     INITIALIZE ��f�ҏ��v.
009980     INITIALIZE ���Ə����v.
009990     INITIALIZE ��������v.
010000     INITIALIZE �������v.
010010     PERFORM ���S���擾.
010020     PERFORM ��������擾.
010030     PERFORM ��f�ҏ��擾.
010060     PERFORM ����㕔�Z�b�g.
010100*================================================================*
010110 ����Z�b�g�R SECTION.
010120*
010130     INITIALIZE YHN662P.
009670     MOVE SPACE TO YHN662P.
010140     MOVE "X" TO EDIT-MODE OF ���҃R�[�h�a.
010150     INITIALIZE ��f�ҏ��v.
010160     INITIALIZE ���Ə����v.
010170     INITIALIZE ��������v.
010180     INITIALIZE �������v.
010190     PERFORM ��f�ҏ��擾.
010200     PERFORM �����f�[�^�擾.
           PERFORM �Ǐ󏈒u�o�߃Z�b�g.
010210*
010220     IF ������������敪�v  NOT = 1 
010230        PERFORM ���������擾
010240     END-IF.
010250*
010280     PERFORM ��������Z�b�g.
010320*================================================================*
010330 ����Z�b�g�T SECTION.
010340*
010350*/�ڍ׃��[�h�p
010360     INITIALIZE YHN662P.
009670     MOVE SPACE TO YHN662P.
010370     MOVE "X" TO EDIT-MODE OF ���҃R�[�h�a.
010380     INITIALIZE ��f�ҏ��v.
010390     INITIALIZE ���Ə����v.
010400     INITIALIZE ��������v.
010410     INITIALIZE �������v.
010420     PERFORM ���S���擾.
010430     PERFORM ��������擾.
010440     PERFORM ��f�ҏ��擾.
010450     PERFORM �����f�[�^�擾.
010460*
010470     IF ������������敪�v  NOT = 1
010480        PERFORM �ڍ׃��[�h������������
010490        PERFORM ���������擾
010500     END-IF.
010510*
010520*/���������̏ڍ׈��/0610
010530     PERFORM �����s�Z�b�g.
010540*
010550     IF �A��|�ی��؈���敪 = 1
010580        PERFORM ����㕔�Z�b�g
010620     END-IF.
010630*
010550     IF �A��|��������敪 = 1
              IF (�A��|�����ڍ�(1) NOT = ZERO) OR
                 (�A��|�����ڍ�(2) NOT = ZERO) OR
                 (�A��|�����ڍ�(3) NOT = ZERO) OR
                 (�A��|�����ڍ�(4) NOT = ZERO) OR
                 (�A��|�����ڍ�(5) NOT = ZERO) OR
                 (�A��|�����ڍ�(6) NOT = ZERO) OR
                 (�A��|�����ڍ�(7) NOT = ZERO)
010640           PERFORM �ڍ׃��[�h��������
              END-IF
010670        PERFORM ��������Z�b�g
           END-IF.
010540*
010550     IF �A��|�{�p����敪 = 1
010580        PERFORM ����{�p�Z�b�g
           END-IF.
010710*
010100*================================================================*
010110 ����Z�b�g�U SECTION.
010120*
010130     INITIALIZE YHN662P.
009670     MOVE SPACE TO YHN662P.
010140     MOVE "X" TO EDIT-MODE OF ���҃R�[�h�a.
010150     INITIALIZE ��f�ҏ��v.
010160     INITIALIZE ���Ə����v.
010170     INITIALIZE ��������v.
010180     INITIALIZE �������v.
010250*
010280     PERFORM ����{�p�Z�b�g.
010720*================================================================*
010730 ����㕔�Z�b�g SECTION.
010740*
010750     MOVE ���Ҕԍ��v                 TO ���Ҕԍ�.
010760     MOVE �}�Ԃv                     TO �}��.
010770     IF �o�[�R�[�h�g�p�敪�v NOT = ZERO
010780         MOVE ���҃R�[�h�v    TO ���҃R�[�h�a
010790         MOVE SPACE           TO EDIT-MODE OF ���҃R�[�h�a
010800     ELSE
010810         MOVE "X" TO EDIT-MODE OF ���҃R�[�h�a
010820     END-IF.
010830**************************
010840* ��ی��ҏ؏��Z�b�g   *
010850**************************
010860     IF �L���v(1:2) = "��" 
010870        MOVE  SPACE    TO  �L��
010880     ELSE
010890        MOVE �L���v    TO  �L��
010900     END-IF.
010910     IF ( ����ԍ��v(1:1) = "*"  ) OR
010920        ( ����ԍ��v(1:2) = "��" )
010930        MOVE  SPACE      TO  �ԍ�
010940     ELSE
010950        MOVE ����ԍ��v  TO  �ԍ�
010960     END-IF.
010970************************
010980* ��ی��ҏ��Z�b�g   *
010990************************
011000     MOVE ��ی��Ҏ����v             TO ��ی��Ҏ���.
011010     MOVE ��ی��҃J�i�v             TO ��ی��҃J�i.
011020     MOVE �X�֔ԍ��P�v               TO �X�֔ԍ��P.
011030     MOVE �X�֔ԍ��Q�v               TO �X�֔ԍ��Q.
011040     MOVE "-"                        TO ��ی��җX�֋��.
011050     MOVE ����Z���P�v               TO �Z���P.
011060     MOVE ����Z���Q�v               TO �Z���Q.
011070*     MOVE ���i�擾�����v             TO ���i�擾����.
011080     MOVE ���i���a�`�F�b�N�v         TO ���i���a�`�F�b�N.
011090     MOVE ���i�����`�F�b�N�v         TO ���i�����`�F�b�N.
           IF ���i�ߘa�`�F�b�N�v NOT = SPACE
              MOVE NC"�E��"             TO ���i�ߘa�b�l
              MOVE ���i�ߘa�`�F�b�N�v   TO ���i�ߘa�`�F�b�N
           END-IF.
011100     MOVE ���i�擾�N�v               TO ���i�擾�N.
011110     MOVE ���i�擾���v               TO ���i�擾��.
011120     MOVE ���i�擾���v               TO ���i�擾��.
010970     IF (( �L���N�v NOT = ZERO ) OR
010980         ( �L�����v NOT = ZERO ) OR
010990         ( �L�����v NOT = ZERO )) AND
              ( �L���a��v = 5)
011090         MOVE NC"�ߘa"               TO �L���a��
011100         MOVE NC"����"               TO �L���a�����
011110     END-IF.
011130     MOVE �L���N�v                   TO �L���N.
011140     MOVE �L�����v                   TO �L����.
011150     MOVE �L�����v                   TO �L����.
011160     MOVE �j�`�F�b�N�v               TO �j�`�F�b�N.
011170     MOVE ���`�F�b�N�v               TO ���`�F�b�N.
011180     MOVE ��ی��Җ����v             TO ��ی��Җ���.
011190     MOVE ��ی��ґ吳�v             TO ��ی��ґ吳.
011200     MOVE ��ی��ҏ��a�v             TO ��ی��ҏ��a.
011210     MOVE ��ی��ҕ����v             TO ��ی��ҕ���.
           IF ��ی��җߘa�v NOT = SPACE
              MOVE NC"�E��"                TO ��ی��җߘa�b�l
              MOVE ��ی��җߘa�v          TO ��ی��җߘa
           END-IF.
011220     MOVE ��ی��ҔN�v               TO ��ی��ҔN.
011230     MOVE ��ی��Ҍ��v               TO ��ی��Ҍ�.
011240     MOVE ��ی��ғ��v               TO ��ی��ғ�.
011250********************
011260* ���ҏ��Z�b�g   *
011270********************
011280     MOVE ���҃J�i�v                 TO ���҃J�i.
011290     MOVE ���Ҏ����v                 TO ���Ҏ���.
011300     MOVE ���Ғj�`�F�b�N�v           TO ���Ғj�`�F�b�N.
011310     MOVE ���ҏ��`�F�b�N�v           TO ���ҏ��`�F�b�N.
011320*     MOVE ���Ґ��ʂv                 TO ���Ґ���.
011330*     MOVE ���Ҍ����v                 TO ���Ҍ���.
011340     MOVE �����`�F�b�N�v             TO �����`�F�b�N.
011350     MOVE �吳�`�F�b�N�v             TO �吳�`�F�b�N.
011360     MOVE ���a�`�F�b�N�v             TO ���a�`�F�b�N.
011370     MOVE �����`�F�b�N�v             TO �����`�F�b�N.
           IF �ߘa�`�F�b�N�v NOT = SPACE
              MOVE NC"�ߘa"                TO �ߘa�b�l
              MOVE �ߘa�`�F�b�N�v          TO �ߘa�`�F�b�N
           END-IF.
011380     MOVE ���ҔN�v                   TO ���ҔN.
011390     MOVE ���Ҍ��v                   TO ���Ҍ�.
011400     MOVE ���ғ��v                   TO ���ғ�.
011410     MOVE ��������v                 TO ����.
011420     MOVE �{�l�`�F�b�N�v             TO �{�l�`�F�b�N.
011430     MOVE �Ƒ��`�F�b�N�v             TO �Ƒ��`�F�b�N.
011440     IF  ����s�����ԍ��v(1:2) = "99"
011450         MOVE SPACE                  TO �s�����ԍ�
011460     ELSE
011470         MOVE �s�����ԍ��v           TO �s�����ԍ�
011480     END-IF.
011490     IF (�����v�Ҕԍ��v(1:1) = "*") OR
011500        (�����v�Ҕԍ��v(1:2) = "��") 
011510         MOVE SPACE                  TO ��v�Ҕԍ�
011520     ELSE
011530         MOVE ��v�Ҕԍ��v           TO ��v�Ҕԍ�
011540     END-IF.
011550     MOVE ���җX�֔ԍ��P�v           TO ���җX�֔ԍ��P.
011560     MOVE ���җX�֔ԍ��Q�v           TO ���җX�֔ԍ��Q.
011570     MOVE "-"                        TO ���җX�֋��.
011580     MOVE ���ҏZ���P�v               TO ���ҏZ���P.
011590     MOVE ���ҏZ���Q�v               TO ���ҏZ���Q.
011600*     MOVE ���ғd�b�ԍ��v             TO ���ғd�b�ԍ�.
011610     MOVE �Еۃ`�F�b�N�v             TO �Еۃ`�F�b�N.
011620     MOVE �g���`�F�b�N�v             TO �g���`�F�b�N.
011630     MOVE ���ك`�F�b�N�v             TO ���ك`�F�b�N.
011640     MOVE �D���`�F�b�N�v             TO �D���`�F�b�N.
011650     MOVE ���σ`�F�b�N�v             TO ���σ`�F�b�N.
011660     MOVE ���ۃ`�F�b�N�v             TO ���ۃ`�F�b�N.
011670     MOVE �ޖ{�`�F�b�N�v             TO �ޖ{�`�F�b�N.
011680     MOVE �މƃ`�F�b�N�v             TO �މƃ`�F�b�N.
011690     MOVE �V�`�F�b�N�v               TO �V�`�F�b�N.
011700     MOVE ��`�F�b�N�v               TO ��`�F�b�N.
011710     MOVE ��`�F�b�N�v               TO ��`�F�b�N.
011720     MOVE ���`�F�b�N�v               TO ���`�F�b�N.
011730     MOVE ���`�F�b�N�v               TO ���`�F�b�N.
011740     MOVE �O���`�F�b�N�v             TO �O���`�F�b�N.
011750     MOVE �P���`�F�b�N�v             TO �P���`�F�b�N.
011760     MOVE �Q���`�F�b�N�v             TO �Q���`�F�b�N.
011770     MOVE �R���`�F�b�N�v             TO �R���`�F�b�N.
011780     MOVE �؃`�F�b�N�v               TO �؃`�F�b�N.
011790********************
011800* ���Ə����Z�b�g *
011810********************
011820     MOVE ���Ə����̂v               TO ���Ə�����.
011830     MOVE ���Ə��X�֔ԍ��P�v         TO ���Ə��X�֔ԍ��P.
011840     MOVE ���Ə��X�֔ԍ��Q�v         TO ���Ə��X�֔ԍ��Q.
011850     MOVE ���Ə��Z���v               TO ���Ə��Z��.
011850     MOVE ���Ə��Z���Q�v             TO ���Ə��Z���Q.
011860************************************
011870* �s���{���E���N�ی��g�����Z�b�g *
011880************************************
011890*�J�ЁA����̎��͕ی��ҏ��͈󎚂��Ȃ�
011900     IF �ی���ʂv�q = 70 OR 90
011910         MOVE SPACE              TO �ی��Ҕԍ�
011920         MOVE SPACE              TO �����於�̂P
011930         MOVE SPACE              TO �����於�̂Q
011940         MOVE SPACE              TO ������X�֔ԍ��P
011950         MOVE SPACE              TO �ی��җX�֋��
011960         MOVE SPACE              TO ������X�֔ԍ��Q
011970         MOVE SPACE              TO ������Z���P
011980         MOVE SPACE              TO ������Z���Q
011990     ELSE
012000         MOVE ����ی��Ҕԍ��v   TO �ی��Ҕԍ�
012010         MOVE �����於�̂P�v     TO �����於�̂P
012020         MOVE �����於�̂Q�v     TO �����於�̂Q
012030         MOVE ������X�֔ԍ��P�v TO ������X�֔ԍ��P
012040         MOVE "-"                TO �ی��җX�֋��
012050         MOVE ������X�֔ԍ��Q�v TO ������X�֔ԍ��Q
012060         MOVE ������Z���P�v     TO ������Z���P
012070         MOVE ������Z���Q�v     TO ������Z���Q
012080     END-IF.
012090*================================================================*
012100 ��������Z�b�g SECTION.
012110*
012120********************
012130* �����f�[�^�Z�b�g *
012140********************
012150* �P���� *
012160**********
012170     MOVE �������v(1)         TO �������P.
           IF �����a��v(1) = 5
              MOVE NC"����"         TO �����a������P
              MOVE NC"�ߘa"         TO �����a��P
           END-IF.
012180     MOVE �����N�v(1)         TO �����N�P.
012190     MOVE �������v(1)         TO �������P.
012200     MOVE �������v(1)         TO �������P.
012210     MOVE �{�p�J�n�N�v(1)     TO �{�p�J�n�N�P.
012220     MOVE �{�p�J�n���v(1)     TO �{�p�J�n���P.
012230     MOVE �{�p�J�n���v(1)     TO �{�p�J�n���P.
012240     MOVE �{�p�I���N�v(1)     TO �{�p�I���N�P.
012250     MOVE �{�p�I�����v(1)     TO �{�p�I�����P.
012260     MOVE �{�p�I�����v(1)     TO �{�p�I�����P.
012270     MOVE �����N������؂v(1) TO �����N����؂P ����������؂P.
012280     MOVE �{�J�N������؂v(1) TO �{�J�N����؂P �{�J������؂P.
012290     MOVE �{�I�N������؂v(1) TO �{�I�N����؂P �{�I������؂P.
012300     MOVE �����`�F�b�N�v(1)   TO �����`�F�b�N�P.
012310     MOVE ���~�`�F�b�N�v(1)   TO ���~�`�F�b�N�P.
012320     MOVE �]��`�F�b�N�v(1)   TO �]��`�F�b�N�P.
012330*     ******************
012340*     * �����͈ꎞ�ۗ� *
012350*     ******************
012360*     **********************
012370*     * �{�p�񐔂͈ꎞ�ۗ� *
012380*     **********************
012390**********
012400* �Q���� *
012410**********
012420     MOVE �������v(2)         TO �������Q.
           IF �����a��v(2) = 5
              MOVE NC"����"         TO �����a������Q
              MOVE NC"�ߘa"         TO �����a��Q
           END-IF.
012430     MOVE �����N�v(2)         TO �����N�Q.
012440     MOVE �������v(2)         TO �������Q.
012450     MOVE �������v(2)         TO �������Q.
012460     MOVE �{�p�J�n�N�v(2)     TO �{�p�J�n�N�Q.
012470     MOVE �{�p�J�n���v(2)     TO �{�p�J�n���Q.
012480     MOVE �{�p�J�n���v(2)     TO �{�p�J�n���Q.
012490     MOVE �{�p�I���N�v(2)     TO �{�p�I���N�Q.
012500     MOVE �{�p�I�����v(2)     TO �{�p�I�����Q.
012510     MOVE �{�p�I�����v(2)     TO �{�p�I�����Q.
012520     MOVE �����N������؂v(2) TO �����N����؂Q ����������؂Q.
012530     MOVE �{�J�N������؂v(2) TO �{�J�N����؂Q �{�J������؂Q.
012540     MOVE �{�I�N������؂v(2) TO �{�I�N����؂Q �{�I������؂Q.
012550     MOVE �����`�F�b�N�v(2)   TO �����`�F�b�N�Q.
012560     MOVE ���~�`�F�b�N�v(2)   TO ���~�`�F�b�N�Q.
012570     MOVE �]��`�F�b�N�v(2)   TO �]��`�F�b�N�Q.
012580*     ******************
012590*     * �����͈ꎞ�ۗ� *
012600*     ******************
012610*     **********************
012620*     * �{�p�񐔂͈ꎞ�ۗ� *
012630*     **********************
012640**********
012650* �R���� *
012660**********
012670     MOVE �������v(3)         TO �������R.
           IF �����a��v(3) = 5
              MOVE NC"����"         TO �����a������R
              MOVE NC"�ߘa"         TO �����a��R
           END-IF.
012680     MOVE �����N�v(3)         TO �����N�R.
012690     MOVE �������v(3)         TO �������R.
012700     MOVE �������v(3)         TO �������R.
012710     MOVE �{�p�J�n�N�v(3)     TO �{�p�J�n�N�R.
012720     MOVE �{�p�J�n���v(3)     TO �{�p�J�n���R.
012730     MOVE �{�p�J�n���v(3)     TO �{�p�J�n���R.
012740     MOVE �{�p�I���N�v(3)     TO �{�p�I���N�R.
012750     MOVE �{�p�I�����v(3)     TO �{�p�I�����R.
012760     MOVE �{�p�I�����v(3)     TO �{�p�I�����R.
012770     MOVE �����N������؂v(3) TO �����N����؂R ����������؂R.
012780     MOVE �{�J�N������؂v(3) TO �{�J�N����؂R �{�J������؂R.
012790     MOVE �{�I�N������؂v(3) TO �{�I�N����؂R �{�I������؂R.
012800     MOVE �����`�F�b�N�v(3)   TO �����`�F�b�N�R.
012810     MOVE ���~�`�F�b�N�v(3)   TO ���~�`�F�b�N�R.
012820     MOVE �]��`�F�b�N�v(3)   TO �]��`�F�b�N�R.
012830*     ******************
012840*     * �����͈ꎞ�ۗ� *
012850*     ******************
012860*     **********************
012870*     * �{�p�񐔂͈ꎞ�ۗ� *
012880*     **********************
012890**********
012900* �S���� *
012910**********
012920     MOVE �������v(4)         TO �������S.
           IF �����a��v(4) = 5
              MOVE NC"����"         TO �����a������S
              MOVE NC"�ߘa"         TO �����a��S
           END-IF.
012930     MOVE �����N�v(4)         TO �����N�S.
012940     MOVE �������v(4)         TO �������S.
012950     MOVE �������v(4)         TO �������S.
012960     MOVE �{�p�J�n�N�v(4)     TO �{�p�J�n�N�S.
012970     MOVE �{�p�J�n���v(4)     TO �{�p�J�n���S.
012980     MOVE �{�p�J�n���v(4)     TO �{�p�J�n���S.
012990     MOVE �{�p�I���N�v(4)     TO �{�p�I���N�S.
013000     MOVE �{�p�I�����v(4)     TO �{�p�I�����S.
013010     MOVE �{�p�I�����v(4)     TO �{�p�I�����S.
013020     MOVE �����N������؂v(4) TO �����N����؂S ����������؂S.
013030     MOVE �{�J�N������؂v(4) TO �{�J�N����؂S �{�J������؂S.
013040     MOVE �{�I�N������؂v(4) TO �{�I�N����؂S �{�I������؂S.
013050     MOVE �����`�F�b�N�v(4)   TO �����`�F�b�N�S.
013060     MOVE ���~�`�F�b�N�v(4)   TO ���~�`�F�b�N�S.
013070     MOVE �]��`�F�b�N�v(4)   TO �]��`�F�b�N�S.
013080*     ******************
013090*     * �����͈ꎞ�ۗ� *
013100*     ******************
013110*     **********************
013120*     * �{�p�񐔂͈ꎞ�ۗ� *
013130*     **********************
013140**********
013150* �T���� *
013160**********
013170     MOVE �������v(5)         TO �������T.
           IF �����a��v(5) = 5
              MOVE NC"����"         TO �����a������T
              MOVE NC"�ߘa"         TO �����a��T
           END-IF.
013180     MOVE �����N�v(5)         TO �����N�T.
013190     MOVE �������v(5)         TO �������T.
013200     MOVE �������v(5)         TO �������T.
013210     MOVE �{�p�J�n�N�v(5)     TO �{�p�J�n�N�T.
013220     MOVE �{�p�J�n���v(5)     TO �{�p�J�n���T.
013230     MOVE �{�p�J�n���v(5)     TO �{�p�J�n���T.
013240     MOVE �{�p�I���N�v(5)     TO �{�p�I���N�T.
013250     MOVE �{�p�I�����v(5)     TO �{�p�I�����T.
013260     MOVE �{�p�I�����v(5)     TO �{�p�I�����T.
013270     MOVE �����N������؂v(5) TO �����N����؂T ����������؂T.
013280     MOVE �{�J�N������؂v(5) TO �{�J�N����؂T �{�J������؂T.
013290     MOVE �{�I�N������؂v(5) TO �{�I�N����؂T �{�I������؂T.
013300     MOVE �����`�F�b�N�v(5)   TO �����`�F�b�N�T.
013310     MOVE ���~�`�F�b�N�v(5)   TO ���~�`�F�b�N�T.
013320     MOVE �]��`�F�b�N�v(5)   TO �]��`�F�b�N�T.
013330*     ******************
013340*     * �����͈ꎞ�ۗ� *
013350*     ******************
013360*     **********************
013370*     * �{�p�񐔂͈ꎞ�ۗ� *
013380*     **********************
013390************
013400* �������� *
013410************
013420     MOVE ���������v(1)       TO ���������P.
013430     MOVE ���������v(2)       TO ���������Q.
013440     MOVE ���������v(3)       TO ���������R.
013450     MOVE ���������v(4)       TO ���������S.
013460     MOVE ���������v(5)       TO ���������T.
013470     MOVE ���������v(6)       TO ���������U.
013480     MOVE ���������v(7)       TO ���������V.
013490     MOVE ���������v(8)       TO ���������W.
013500     MOVE ���������v(9)       TO ���������X.
013510     MOVE ���������v(10)      TO ���������P�O.
012290****************
012300* �Ǐ󏈒u�o�� *
012310****************
      *     PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 8
      *         MOVE �Ǐ�v(�J�E���^) TO �Ǐ�(�J�E���^)
      *         MOVE ���u�v(�J�E���^) TO ���u(�J�E���^)
      *         MOVE �o�߂v(�J�E���^) TO �o��(�J�E���^)
      *     END-PERFORM.
013520*
010720*================================================================*
010730 ����{�p�Z�b�g SECTION.
010740*
002980     PERFORM ���v�l������.
           MOVE 1               TO �󎚈ʒu�b�m�s.
           MOVE ���҃R�[�h�v�q  TO ��P�|���҃R�[�h.
           MOVE �{�p�a��v�q    TO ��P�|�{�p�a��.
           MOVE �{�p�N�v�q      TO ��P�|�{�p�N.
           MOVE �{�p���v�q      TO ��P�|�{�p��.
           MOVE ZERO            TO ��P�|�{�p��.
           START ��ƃt�@�C���P KEY IS >= ��P�|���҃R�[�h
                                          ��P�|�{�p�a��N����
           END-START.
013500     IF ��ԃL�[  =  "00"
013510         MOVE SPACE  TO �I���t���O
013520         PERFORM ��ƃt�@�C���P�Ǎ�
003000         MOVE ��P�|���҃R�[�h   TO ���҃R�[�h�v
               MOVE ��P�|�{�p�a��     TO �{�p�a��v
               MOVE ��P�|�{�p�N       TO �{�p�N�v
               MOVE ��P�|�{�p��       TO �{�p���v
               IF ���i���v�q = ZERO
                   MOVE 1              TO �J�E���^
               ELSE
                   MOVE ���i���v�q   TO �J�E���^
               END-IF
      */      */���҃R�[�h���ς��܂ō�ƃt�@�C����ǂ�
003070         PERFORM UNTIL ( �I���t���O = "YES" ) OR
003090                       ( ��P�|���҃R�[�h NOT = ���҃R�[�h�v )
      *           */�Q�X�s�𒴂�������y�[�W
004380             PERFORM UNTIL ( �J�E���^ > 29 ) OR
003090                           ( ��P�|���҃R�[�h NOT = ���҃R�[�h�v ) OR
                                 ( �I���t���O = "YES" )
004640                 IF �A��|���׃��[�h = 1
      */                  */���׍s�̓]�L
                           PERFORM ���׈������
                       END-IF
003590                 MOVE ��P�|���҃R�[�h   TO ���҃R�[�h�v
003590                 MOVE ��P�|�{�p�a��     TO �{�p�a��v
003590                 MOVE ��P�|�{�p�N       TO �{�p�N�v
003590                 MOVE ��P�|�{�p��       TO �{�p���v
003600                 PERFORM ��ƃt�@�C���P�Ǎ�
003610             END-PERFORM
                   IF ((�I���t���O           = "YES") OR 
                       (��P�|���҃R�[�h NOT = ���҃R�[�h�v))
      */           */���łȂ�
                       CONTINUE
                   ELSE
      */              */���������K�v
                       MOVE 1 TO �J�E���^
      */              */���׍s��������ĉ��ł���
                       PERFORM �������
003650                 PERFORM ���ŏ���
                       MOVE 1                    TO �A���o�|���b�Z�[�W�ԍ�
                       MOVE "���̗p�����Z�b�g���Ă�������" TO �A���o�|���b�Z�[�W
                       MOVE "PMSG0051.DLL" TO dll-name
                       MOVE "PMSG0051"     TO form-name
                       CALL "POWEROPENSHEET" USING dll-name form-name
                       EVALUATE  �A���o�|�Ԃ�l
005871                 WHEN "Y"
021210                     EVALUATE ������[�h�e�v�q
021310                     WHEN 0
009840                         PERFORM ����㕔�Z�b�g
009850                         PERFORM ��������Z�b�g
021260                     WHEN 4
021270                        IF �A��|�ی��؈���敪 = 1
009840                           PERFORM ����㕔�Z�b�g
021290                        END-IF
021270                        IF (�A��|��������敪 = 1) OR (�A��|�{�p����敪 = 1)
009850                           PERFORM ��������Z�b�g
                              END-IF
021330                     END-EVALUATE
                       WHEN OTHER
      */                  */�m�@���́@�L�����Z��
                           PERFORM ����s���ޔ�
                           PERFORM �I������
                           EXIT PROGRAM
                       END-EVALUATE
                   END-IF
               END-PERFORM
      */���v�s�̈�����K�v�ȏꍇ������20130
               IF �A��|���v���[�h = 1
                   IF �J�E���^ > 28
      */         */����������20130
                      MOVE 1    TO �J�E���^
                      PERFORM �������
003650                PERFORM ���ŏ���
                      MOVE 1                    TO �A���o�|���b�Z�[�W�ԍ�
                      MOVE "���̗p�����Z�b�g���Ă�������" TO �A���o�|���b�Z�[�W
                      MOVE "PMSG0051.DLL" TO dll-name
                      MOVE "PMSG0051"     TO form-name
                      CALL "POWEROPENSHEET" USING dll-name form-name
                      EVALUATE  �A���o�|�Ԃ�l
005871                WHEN "Y"
021210                     EVALUATE ������[�h�e�v�q
021310                     WHEN 0
009840                         PERFORM ����㕔�Z�b�g
009850                         PERFORM ��������Z�b�g
021260                     WHEN 4
021270                        IF �A��|�ی��؈���敪 = 1
009840                           PERFORM ����㕔�Z�b�g
021290                        END-IF
021270                        IF (�A��|��������敪 = 1) OR (�A��|�{�p����敪 = 1)
009850                           PERFORM ��������Z�b�g
                              END-IF
021330                     END-EVALUATE
                      WHEN OTHER
      */         */�m�@���́@�L�����Z��
                         PERFORM ����s���ޔ�
                         PERFORM �I������
                         EXIT PROGRAM
                      END-EVALUATE
                   END-IF
                   PERFORM ���v�������
               END-IF
               MOVE �J�E���^ TO �󎚈ʒu�b�m�s
               PERFORM ����s���ޔ�
           END-IF.
004900*================================================================*
004910 ���׈������ SECTION.
004920*
           PERFORM �{�p�L�^�e�Ǎ�
      *     IF (�J�E���^ = 1) OR (��P�|�{�p�� = �{�L�|�{�p��)
      *         MOVE ��P�|�{�p��   TO ��  (�J�E���^)
      *         MOVE "/"            TO ���(�J�E���^)
      *     END-IF
005510     MOVE ��P�|�{�p��       TO ��(�J�E���^).
           MOVE "/"                TO ���(�J�E���^).
005520     MOVE ��P�|�{�p��       TO ��(�J�E���^).
004940********************
004950* �����f�[�^�Z�b�g *
004960********************
005010     MOVE ��P�|�������z     TO  ��������(�J�E���^).
005010     MOVE ��P�|�������z     TO  ������(�J�E���^).
005010     MOVE ��P�|��Ë��z     TO  ��×�(�J�E���^).
005010     MOVE ��P�|㪖@���z     TO  㪖@��(�J�E���^).
005010     MOVE ��P�|�d�Ë��z     TO  �d�×�(�J�E���^).
           MOVE ��P�|��p�z       TO  ��p�z(�J�E���^).
           MOVE ��P�|�ꕔ���S��   TO  ���S��(�J�E���^).
           COMPUTE �J�E���^ = �J�E���^ + 1.
005730*================================================================*
005740 ���v�l������ SECTION.
005750*
005760     MOVE ZERO  TO ���v�v.
004900*================================================================*
004910 ���v������� SECTION.
004920*
           MOVE �{�p�a��v   TO ��Q�|�{�p�a��.
           MOVE �{�p�N�v     TO ��Q�|�{�p�N.
           MOVE �{�p���v     TO ��Q�|�{�p��.
           MOVE ���҃R�[�h�v TO ��Q�|���҃R�[�h.
           READ ��ƃt�@�C���Q
           NOT INVALID KEY
               MOVE NC"���v"     TO ���v(�J�E���^)
               MOVE ��Q�|�����v TO ��������(�J�E���^)
               MOVE ��Q�|�����v TO ������(�J�E���^)
               MOVE ��Q�|��Ìv TO ��×�(�J�E���^)
               MOVE ��Q�|㪖@�v TO 㪖@��(�J�E���^)
               MOVE ��Q�|�d�Ìv TO �d�×�(�J�E���^)
               MOVE ��Q�|��p�v TO ��p�z(�J�E���^)
               MOVE ��Q�|���S�v TO ���S��        (�J�E���^)
      *
               COMPUTE �J�E���^ = �J�E���^ + 1
               MOVE NC"��p�z"            TO ��p(�J�E���^)
               MOVE ��Q�|��p�z          TO ���v��p�z(�J�E���^)
               MOVE NC"�~"                TO �~�P(�J�E���^)
               MOVE NC"�����z"            TO ����(�J�E���^)
               MOVE ��Q�|�����z          TO ���v�����z(�J�E���^)
               MOVE NC"�~"                TO �~�Q(�J�E���^)
               MOVE ��Q�|�ʉ@��          TO �ʉ@��(�J�E���^)
               MOVE NC"��"                TO ���P(�J�E���^)
               MOVE ��Q�|�ʉ@��          TO �ʉ@��(�J�E���^)
               MOVE NC"��"                TO ���P(�J�E���^)
               MOVE NC"��"              TO �{�p��(�J�E���^)
               MOVE ��Q�|�{�p��        TO ��(�J�E���^)
               MOVE NC"��"                TO ��(�J�E���^)
               MOVE ��Q�|�]�A�敪(1)     TO �]�A�P(�J�E���^)
               MOVE ��Q�|�]�A�敪(2)     TO �]�A�Q(�J�E���^)
               MOVE ��Q�|�]�A�敪(3)     TO �]�A�R(�J�E���^)
               MOVE ��Q�|�]�A�敪(4)     TO �]�A�S(�J�E���^)
               MOVE ��Q�|�]�A�敪(5)     TO �]�A�T(�J�E���^)
           END-READ.
007070*================================================================*
007080 ��ƃt�@�C���P�Ǎ� SECTION.
007090*
007100     READ ��ƃt�@�C���P NEXT
007110     AT END
007120         MOVE "YES" TO �I���t���O
007130     END-READ.
007140*
013530*================================================================*
013540 ���S���擾 SECTION.
013550*
013560     MOVE ZERO TO �{�l���S���v.
013570     MOVE ZERO TO �Ƒ����S���v.
013580*
013590     MOVE �{�p�a��v�q   TO ��|�{�p�a��.
013600     MOVE �{�p�N�v�q     TO ��|�{�p�N.
013610     MOVE �{�p���v�q     TO ��|�{�p��.
013620     MOVE ���Ҕԍ��v�q   TO ��|���Ҕԍ�.
013630     MOVE �}�Ԃv�q       TO ��|�}��.
013640     READ ��f�ҏ��e
013650     INVALID KEY
013660         MOVE  NC"�@�@�{�p���̎�f�ҏ�񂪂���܂���" TO �A���|���b�Z�[�W
013670         CALL   "MSG001"
013680         CANCEL "MSG001"
013690         PERFORM �t�@�C����
013700         MOVE ZERO TO PROGRAM-STATUS
013710         EXIT PROGRAM
013720     NOT INVALID KEY
013730* 14/10�`�@�T�u���[�`������
013740         IF ��|�{�p�a��N�� >= 41410
013750            PERFORM ���S���擾�P�S�P�O
013760         END-IF
013770*
013780     END-READ.
013790*
013800     EVALUATE ���S���v
013810         WHEN ZERO
013820             MOVE NC"��" TO �O���`�F�b�N�v
013830         WHEN 10
013840             MOVE NC"��" TO �P���`�F�b�N�v
013850         WHEN 20
013860             MOVE NC"��" TO �Q���`�F�b�N�v
013870         WHEN 30
013880             MOVE NC"��" TO �R���`�F�b�N�v
013890         WHEN OTHER
013900             CONTINUE
013910     END-EVALUATE.
013920*
013930*================================================================*
013940 ���S���擾�P�S�P�O SECTION.
013950*
013960* ��f�҂e READ��
013970* ����14/10�`
013980     MOVE ZERO  TO ���S���v.
013990     INITIALIZE �������S���v�v.
014000*
014010     MOVE SPACE TO �A���|���S���擾�L�[.
014020     INITIALIZE �A���|���S���擾�L�[.
014030     MOVE ��|�{�p�a��N�� TO �A���|�{�p�a��N��.
014040     MOVE ��|���҃R�[�h   TO �A���|���҃R�[�h.
014050*
014060     CALL   "HUTANRIT".
014070     CANCEL "HUTANRIT".
014080*
014090* / �{�� /
014100*(���ۂɂ���j
014110     MOVE �A���|���ە��S�� TO ���S���v.
014120*
014130*
014140* / ���� /
014150*     IF ��|������� NOT = ZERO
014160*        MOVE �A���|�������S��  TO �������S���v
014170*
014180*        COMPUTE �������S���v�P = �������S���v / 10
014190*        MOVE �������S���v�P TO  �������S�������v�P
014200*
014210*        STRING "("                 DELIMITED BY SIZE
014220*               �������S�������v�P  DELIMITED BY SIZE
014230*               "��)"               DELIMITED BY SIZE
014240*               INTO �������S���\���v
014250*        END-STRING
014260*
014270*     END-IF.
014280*
014290*================================================================*
014300*================================================================*
014310 ��f�ҏ��擾 SECTION.
014320*
014330**************************************************
014340* �A���f�[�^�����f�ҏ��e���ȉ��̏����擾 *
014350* �� ���Ҕԍ�.... ���Ҕԍ��v                     *
014360* �� �}��........ �}�Ԃv                         *
014370* �� �L�� ....... �L���v�Ɋi�[                   *
014380* �� �ԍ� ....... �ԍ��v�Ɋi�[                   *
014390* �� ��ی��҃J�i.��ی��҃J�i�v�Ɋi�[           *
014400* �� ��ی��Ҏ���.��ی��Ҏ����v�Ɋi�[           *
014410* �� �X�֔ԍ��P ..�X�֔ԍ��P�v�Ɋi�[             *
014420* �� �X�֔ԍ��Q ..�X�֔ԍ��Q�v�Ɋi�[             *
014430* �� �Z���P+�Z���Q ..��ی��ҏZ���v�Ɋi�[        *
014440* �� �d�b�ԍ�.....�d�b�ԍ��v�Ɋi�[               *
014450* �� ���҃J�i ....���҃J�i�v�Ɋi�[               *
014460* �� ���Ҏ��� ....���Ҏ����v�Ɋi�[               *
014470* �� ���� ........���̃}�X�^��葱���v�Ɏ擾     *
014480* �� ���Ҙa�� ....�a��ɂ��`�F�b�N��"��"���i�[ *
014490* �� ���ҔN ......���ҔN�v�Ɋi�[                 *
014500* �� ���Ҍ� ......���Ҍ��v�Ɋi�[                 *
014510* �� ���ғ� ......���ғ��v�Ɋi�[                 *
014520**************************************************
014530     MOVE �{�p�a��v�q       TO ��|�{�p�a��.
014540     MOVE �{�p�N�v�q         TO ��|�{�p�N.
014550     MOVE �{�p���v�q         TO ��|�{�p��.
014560     MOVE ���҃R�[�h�v�q     TO ��|���҃R�[�h.
014570     READ ��f�ҏ��e
014580     INVALID KEY
014590         CONTINUE
014600*            /* ���肦�Ȃ� */
014610     NOT INVALID KEY
014620         MOVE ��|���Ҕԍ�     TO ���Ҕԍ��v
014630         MOVE ��|�}��         TO �}�Ԃv
014651*
014652*-----------------------------------------------------------------*
014653         MOVE SPACE TO �A�Í������|�Í����
014654*
014655*        / �A�Í������|���͏��Z�b�g /
014656         MOVE ��|�L��       TO �A�Í������|�L��
014657         MOVE ��|�ԍ�       TO �A�Í������|�ԍ�
014658         MOVE ��|�Í������� TO �A�Í������|�Í�������
014659*
014660         CALL   �����v���O�������v
014661         CANCEL �����v���O�������v
014662*
014663         MOVE �A�Í������|���������L�� TO �L���v
014664         MOVE �A�Í������|���������ԍ� TO �ԍ��v
014665*-----------------------------------------------------------------*
014666*
014668         MOVE ��|��ی��҃J�i TO ��ی��҃J�i�v
014670         MOVE ��|��ی��Ҏ��� TO ��ی��Ҏ����v
014680*
014690         EVALUATE ��|��ی��Ґ���
014700         WHEN 1
014710             MOVE NC"��"  TO �j�`�F�b�N�v
014720         WHEN 2
014730             MOVE NC"��"  TO ���`�F�b�N�v
014740         END-EVALUATE
014750*
014760         EVALUATE ��|��ی��Ҙa��
014770         WHEN 1
014780             MOVE NC"��"  TO ��ی��Җ����v
014790         WHEN 2
014800             MOVE NC"��"  TO ��ی��ґ吳�v
014810         WHEN 3
014820             MOVE NC"��"  TO ��ی��ҏ��a�v
014830         WHEN 4
014840             MOVE NC"��"  TO ��ی��ҕ����v
014880         WHEN 5
014890             MOVE NC"��"  TO ��ی��җߘa�v
014850         END-EVALUATE
014860         MOVE ��|��ی��ҔN   TO ��ی��ҔN�v
014870         MOVE ��|��ی��Ҍ�   TO ��ی��Ҍ��v
014880         MOVE ��|��ی��ғ�   TO ��ی��ғ��v
014890         MOVE ��|�X�֔ԍ��P   TO �X�֔ԍ��P�v
014900         MOVE ��|�X�֔ԍ��Q   TO �X�֔ԍ��Q�v
014910*         STRING ��|�Z���P DELIMITED BY SPACE
014920*                ��|�Z���Q DELIMITED BY SPACE
014930*                INTO ��ی��ҏZ���v
014940*         END-STRING
014950         MOVE ��|�Z���P         TO ����Z���P�v
014960         MOVE ��|�Z���Q         TO ����Z���Q�v
014970         MOVE ��|���җX�֔ԍ��P TO ���җX�֔ԍ��P�v
014980         MOVE ��|���җX�֔ԍ��Q TO ���җX�֔ԍ��Q�v
014990*         STRING ��|���ҏZ���P DELIMITED BY SPACE
015000*                ��|���ҏZ���Q DELIMITED BY SPACE
015010*                INTO ���ҏZ���v
015020*         END-STRING
015030         MOVE ��|�d�b�ԍ�     TO �d�b�ԍ��v
               IF ��|�ی���� = 70 OR 80 OR 85 OR 90
018370             MOVE ��|���҃J�i       TO ��ی��҃J�i�v
018380             MOVE ��|���Ҏ���       TO ��ی��Ҏ����v
018000             EVALUATE ��|���Ґ���
018010             WHEN 1
018020                 MOVE NC"��"  TO �j�`�F�b�N�v
018030             WHEN 2
018040                 MOVE NC"��"  TO ���`�F�b�N�v
018050             END-EVALUATE
018070             EVALUATE ��|���Ҙa��
018080             WHEN 1
018090                 MOVE NC"��"  TO ��ی��Җ����v
018100             WHEN 2
018110                 MOVE NC"��"  TO ��ی��ґ吳�v
018120             WHEN 3
018130                 MOVE NC"��"  TO ��ی��ҏ��a�v
                   WHEN 4
                       MOVE NC"��"  TO ��ی��ҕ����v
                   WHEN 5
                       MOVE NC"��"  TO ��ی��җߘa�v
018140             END-EVALUATE
018150             MOVE ��|���ҔN   TO ��ی��ҔN�v
018160             MOVE ��|���Ҍ�   TO ��ی��Ҍ��v
018170             MOVE ��|���ғ�   TO ��ی��ғ��v
014970             MOVE ��|���җX�֔ԍ��P TO �X�֔ԍ��P�v
014980             MOVE ��|���җX�֔ԍ��Q TO �X�֔ԍ��Q�v
016980             MOVE ��|���ҏZ���P     TO ����Z���P�v
016990             MOVE ��|���ҏZ���Q     TO ����Z���Q�v
018220             MOVE ��|���ғd�b�ԍ�   TO �d�b�ԍ��v
               END-IF
015040         MOVE ��|���ҏZ���P   TO ���ҏZ���P�v
015050         MOVE ��|���ҏZ���Q   TO ���ҏZ���Q�v
015060*         MOVE ��|���ғd�b�ԍ� TO ���ғd�b�ԍ��v
015070         STRING "TEL:"           DELIMITED BY SPACE
015080                ��|���ғd�b�ԍ� DELIMITED BY SPACE
015090                INTO ���ғd�b�ԍ��v
015100         END-STRING
015110*
015120         MOVE ��|���҃J�i     TO ���҃J�i�v
015130         MOVE ��|���Ҏ���     TO ���Ҏ����v
015140*
               MOVE ��|�ی����     TO ���|�ی����
               MOVE ��|�ی��Ҕԍ�   TO ���|�ی��Ҕԍ�
      *         MOVE ��|�L��         TO ���|�L��
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
               MOVE �A�Í������|���������L�� TO ���|�L��
015150         READ ���Ə��}�X�^
015160         INVALID KEY
015170            MOVE SPACE TO ���|���R�[�h
015180            INITIALIZE    ���|���R�[�h
015190         END-READ
015200         MOVE ���|���Ə�����   TO ���Ə����̂v
015210         STRING ���|���Ə��Z���P  DELIMITED BY SPACE
015220                ���|���Ə��Z���Q  DELIMITED BY SPACE
015230           INTO ���Ə��Z���v
015240         END-STRING
015200         MOVE ���|���Ə��Z���P   TO ���Ə��Z���v
015200         MOVE ���|���Ə��Z���Q   TO ���Ə��Z���Q�v
015250* ����
015260         IF ��|�{�l�Ƒ��敪 = 1
015270             MOVE NC"�{�l"    TO �����v
015280             MOVE NC"��"      TO �{�l�`�F�b�N�v
015290         ELSE
015300             MOVE NC"��"      TO �Ƒ��`�F�b�N�v
015310             MOVE 05          TO ���|�敪�R�[�h
015320             MOVE ��|����    TO ���|���̃R�[�h
015330             READ ���̃}�X�^
015340             INVALID KEY
015350                 MOVE SPACE   TO �����v
015360             NOT INVALID KEY
015370                 MOVE ���|����  TO �����v
015380             END-READ
015390         END-IF
015400* ���Ҙa��N����
015410         MOVE ��|���Ҙa�� TO ���|�����敪
015420         READ �����}�X�^
015430         INVALID KEY
015440             MOVE SPACE        TO ���Ҍ����v
015450         NOT INVALID KEY
015460             MOVE ���|�������� TO ���Ҍ����v
015470         END-READ
015480         EVALUATE ��|���Ҙa��
015490         WHEN 1
015500             MOVE NC"��"  TO �����`�F�b�N�v
015510         WHEN 2
015520             MOVE NC"��"  TO �吳�`�F�b�N�v
015530         WHEN 3
015540             MOVE NC"��"  TO ���a�`�F�b�N�v
015550         WHEN 4
015560             MOVE NC"��"  TO �����`�F�b�N�v
015550         WHEN 5
015560             MOVE NC"��"  TO �ߘa�`�F�b�N�v
015570         END-EVALUATE
015580         MOVE ��|���ҔN  TO ���ҔN�v
015590         MOVE ��|���Ҍ�  TO ���Ҍ��v
015600         MOVE ��|���ғ�  TO ���ғ��v
015610*���ҔN��
015620     MOVE �A��|�{�p�a�� TO ���|�����敪
015630     READ �����}�X�^
015640     NOT INVALID KEY
015650         COMPUTE �{�p����N�v = ���|�J�n����N + ( �A��|�{�p�N - 1 )
015660     END-READ
015670     IF ( ��|���Ҙa�� NOT = ZERO ) AND
015680        ( ��|���ҔN   NOT = ZERO ) AND
015690        ( ��|���Ҍ�   NOT = ZERO ) AND
015700        ( ��|���ғ�   NOT = ZERO )
015710*
015720         MOVE ZERO TO ���ҔN��v
015730*
015740         MOVE ��|���Ҙa�� TO ���|�����敪
015750         READ �����}�X�^
015760         NOT INVALID KEY
015770             COMPUTE ���Ґ���N�v = ���|�J�n����N + ( ��|���ҔN - 1 )
015780         END-READ
015790*
015800         COMPUTE ���ҔN��v   = �{�p����N�v - ���Ґ���N�v
015810*
015820         IF �A��|�{�p�� < ��|���Ҍ�
015830             COMPUTE ���ҔN��v = ���ҔN��v - 1
015840         END-IF
015850*
015860*         MOVE ���ҔN��v     TO ���ҔN��
015870     END-IF
015880* ���Ґ���
015890         EVALUATE ��|���Ґ���
015900         WHEN 1
015910             MOVE NC"��" TO ���Ғj�`�F�b�N�v
015920         WHEN 2
015930             MOVE NC"��" TO ���ҏ��`�F�b�N�v
015940         END-EVALUATE
015950         MOVE ��|�L���a�� TO �L���a��v
015960         MOVE ��|�L���N   TO �L���N�v
015970         MOVE ��|�L����   TO �L�����v
015980         MOVE ��|�L����   TO �L�����v
015990*
016000         MOVE ��|���i�a�� TO ���i�擾�a��v
016010         MOVE ��|���i�N   TO ���i�擾�N�v
016020         MOVE ��|���i��   TO ���i�擾���v
016030         MOVE ��|���i��   TO ���i�擾���v
016040         IF (���i�擾�a��v NOT = ZERO) AND
016050            (���i�擾�N�v   NOT = ZERO) AND
016060            (���i�擾���v   NOT = ZERO) AND
016070            (���i�擾���v   NOT = ZERO)
016080             EVALUATE ���i�擾�a��v
016090             WHEN 3
016100                 MOVE NC"��"  TO ���i���a�`�F�b�N�v
016110             WHEN 4
016120                 MOVE NC"��"  TO ���i�����`�F�b�N�v
016110             WHEN 5
016120                 MOVE NC"��"  TO ���i�ߘa�`�F�b�N�v
016130             END-EVALUATE
016140         END-IF
016150*         IF (���i�擾�a��v NOT = ZERO) AND
016160*            (���i�擾�N�v   NOT = ZERO) AND
016170*            (���i�擾���v   NOT = ZERO) AND
016180*            (���i�擾���v   NOT = ZERO)
016190*             MOVE ���i�擾�a��v  TO ���|�����敪
016200*             READ �����}�X�^
016210*             INVALID KEY
016220*                 MOVE SPACE        TO ���i�擾�����v
016230*             NOT INVALID KEY
016240*                 MOVE ���|�������� TO ���i�擾�����v
016250*             END-READ
016260*         END-IF
016270*
016280* �s�����ԍ� �󋋎Ҕԍ�
016290* �V�l ���� ���� �̏ꍇ�͘V�l�̔ԍ����󎚂���(����ǂ��Ȃ邩�͉���Ȃ�)
016300         IF ��|������� NOT = ZERO
016310             MOVE ��|��p���S�Ҕԍ����� TO �s�����ԍ��v
016320             MOVE ��|��v�Ҕԍ�����     TO ��v�Ҕԍ��v
016330         END-IF
016340         IF ( ��|������ = 05 ) AND ( ��|�{�p�a��N�� < 42004 )
016350             MOVE ��|��p���S�Ҕԍ�     TO �s�����ԍ��v
016360             MOVE ��|��v�Ҕԍ��V�l     TO ��v�Ҕԍ��v
016370         END-IF
016380*�ی����
016390         EVALUATE ��|�ی����
016400         WHEN 02
016410             MOVE NC"��"  TO �Еۃ`�F�b�N�v
016420         WHEN 03
016430             MOVE NC"��"  TO �g���`�F�b�N�v
016440         WHEN 04
016450         WHEN 09
016460             MOVE NC"��"  TO ���σ`�F�b�N�v
016470         WHEN 06
016480             MOVE NC"��"  TO ���ك`�F�b�N�v
016490         WHEN 07
016500             MOVE NC"��"  TO �D���`�F�b�N�v
016510         WHEN 01
016520             MOVE NC"��"  TO ���ۃ`�F�b�N�v
016530         WHEN 08
016540             IF ��|�{�l�Ƒ��敪 = 1
016550                MOVE NC"��"  TO �ޖ{�`�F�b�N�v
016560             ELSE
016570                MOVE NC"��"  TO �މƃ`�F�b�N�v
016580             END-IF
016590         WHEN OTHER
016600             CONTINUE
016610         END-EVALUATE
016620*�������
016630         EVALUATE ��|�������
016640         WHEN 51
016650             MOVE NC"��"  TO �V�`�F�b�N�v
016660         WHEN 53
016670             MOVE NC"��"  TO ��`�F�b�N�v
016680         WHEN 52
016690             MOVE NC"��"  TO ��`�F�b�N�v
016700         WHEN 55
016710             MOVE NC"��"  TO ���`�F�b�N�v
016720         WHEN 54
016730             MOVE NC"��"  TO ���`�F�b�N�v
016740         WHEN OTHER
016750             CONTINUE
016760         END-EVALUATE
016770*
016780* �V�l�ꕔ���S���Ə�
016790         IF (��|������ = 05  )    AND
016800            (��|�V�l���S���Ə� = 1) AND (��|��p���S�Ҕԍ�(3:2) = "27")
016810            MOVE NC"��" TO �؃`�F�b�N�v
016820         END-IF
016830         IF (��|������ = ZERO)    AND (��|������� = 51) AND
016840            (��|�V�l���S���Ə� = 1) AND (��|��p���S�Ҕԍ�����(3:2) = "27")
016850            MOVE NC"��" TO �؃`�F�b�N�v
016860         END-IF
016870*
016880         IF ��|�ی���� = 70
016890            MOVE ��|�{�p�a��N�� TO �J�Ё|�{�p�a��N��
016900            MOVE ��|���҃R�[�h   TO �J�Ё|���҃R�[�h
016910            READ �J�Џ��e
016920            INVALID KEY
016930               MOVE SPACE TO �J�Ё|���R�[�h
016940               INITIALIZE    �J�Ё|���R�[�h
016950            END-READ
016960            MOVE �J�Ё|�J�Ў��Ə�����       TO ���Ə����̂v
016970            MOVE �J�Ё|�J�Ў��Ə��X�֔ԍ��P TO ���Ə��X�֔ԍ��P�v
016980            MOVE �J�Ё|�J�Ў��Ə��X�֔ԍ��Q TO ���Ə��X�֔ԍ��Q�v
016990            STRING �J�Ё|�J�Ў��Ə��Z���P DELIMITED BY SPACE
017000                   �J�Ё|�J�Ў��Ə��Z���Q DELIMITED BY SPACE
017010               INTO ���Ə��Z���v
017020            END-STRING
015200            MOVE �J�Ё|�J�Ў��Ə��Z���P     TO ���Ə��Z���v
015200            MOVE �J�Ё|�J�Ў��Ə��Z���Q     TO ���Ə��Z���Q�v
017030            MOVE �J�Ё|�J���ی��ԍ�         TO �ԍ��v
017040         END-IF
017050*
017060     END-READ.
017070*================================================================*
017080 ��������擾 SECTION.
017090*
017100****************************************************
017110* �A���f�[�^����ی��҃}�X�^��萿������擾����B *
017120* �� �ی��Ҕԍ�...�ی��Ҕԍ��v�Ɋi�[               *
017130* �� ����........ �����於�̂v�Ɋi�[               *
017140* �� �X�֔ԍ��P.. ������X�֔ԍ��P�v�Ɋi�[         *
017150* �� �X�֔ԍ��Q.. ������X�֔ԍ��Q�v�Ɋi�[         *
017160* �� �Z���P.......������Z���P�v�Ɋi�[             *
017170* �� �Z���Q.......������Z���Q�v�Ɋi�[             *
017180****************************************************
017190     MOVE �ی���ʂv�q   TO �ہ|�ی����.
017200     MOVE �ی��Ҕԍ��v�q TO �ہ|�ی��Ҕԍ�.
017210     READ �ی��҃}�X�^
017220     INVALID KEY
017230         IF ( ��|�{�p�a��N�� >= 42004 ) AND ( �ی���ʂv�q = 05 )
017240             MOVE �ی���ʂv�q   TO �s�|������
017250             MOVE �ی��Ҕԍ��v�q TO �s�|�s�����ԍ�
017260             READ �s�����}�X�^
017270             INVALID KEY
017280                 MOVE SPACE      TO �����於�̂v
017290             NOT INVALID KEY
017300                 MOVE �s�|�s�����ԍ�  TO �ی��Ҕԍ��v
017310                 MOVE �s�|�s��������  TO �����於�̂v
017320                 MOVE �s�|�x��������  TO �x�����v
017330                 MOVE �s�|�X�֔ԍ��P  TO ������X�֔ԍ��P�v
017340                 MOVE �s�|�X�֔ԍ��Q  TO ������X�֔ԍ��Q�v
017350                 MOVE �s�|�Z���P      TO ������Z���P�v
017360                 MOVE �s�|�Z���Q      TO ������Z���Q�v
017370             END-READ
017380         ELSE
017390             MOVE SPACE      TO �����於�̂v
017400         END-IF
017410     NOT INVALID KEY
017420         MOVE �ہ|�ی��Ҕԍ�  TO �ی��Ҕԍ��v
017430         EVALUATE �ی���ʂv�q
017440         WHEN 2
017450         WHEN 6
017460            IF �ہ|�ڔ���敪 = 1
017470               MOVE �ہ|�ی��Җ��� TO �����於�̂v
017480            ELSE
017490               STRING �ہ|�ی��Җ���   DELIMITED BY SPACE
017500                      "�Љ�ی�������" DELIMITED BY SIZE
017510                      INTO �����於�̂v
017520               END-STRING
017530            END-IF
017540         WHEN 3
017550             STRING �ہ|�ی��Җ���   DELIMITED BY SPACE
017560                    "���N�ی��g��  " DELIMITED BY SIZE
017570                    �ہ|�x��������   DELIMITED BY SPACE
017580               INTO �����於�̂v
017590             END-STRING
017600         WHEN 4
017610             STRING �ہ|�ی��Җ���   DELIMITED BY SPACE
017620                    "���ϑg��  "     DELIMITED BY SIZE
017630                    �ہ|�x��������   DELIMITED BY SPACE
017640               INTO �����於�̂v
017650             END-STRING
017660         WHEN OTHER
017670             MOVE �ہ|�ی��Җ��� TO �����於�̂v
017680         END-EVALUATE
017690         MOVE �ہ|�x��������     TO �x�����v 
017700         MOVE �ہ|�X�֔ԍ��P     TO ������X�֔ԍ��P�v
017710         MOVE �ہ|�X�֔ԍ��Q     TO ������X�֔ԍ��Q�v
017720         MOVE �ہ|�Z���P         TO ������Z���P�v
017730         MOVE �ہ|�Z���Q         TO ������Z���Q�v
017740*         STRING �ہ|�Z���P DELIMITED BY SPACE
017750*                �ہ|�Z���Q DELIMITED BY SPACE
017760*           INTO ������Z���v
017770*         END-STRING
017780     END-READ.
           IF �ی���ʂv�q = 85
              MOVE �{�p�a��N���v�q TO ���ہ|�{�p�a��N��
              MOVE ���҃R�[�h�v�q   TO ���ہ|���҃R�[�h
              READ ���ۏ��e
              NOT INVALID KEY
019480           MOVE ���ہ|���S�Ҕԍ�    TO �ی��Ҕԍ��v
019490           MOVE ���ہ|���ێs������  TO �����於�̂v
019500           MOVE SPACE               TO �x�����v
019510           MOVE ���ہ|���ۑ��t��X�֔ԍ��P  TO ������X�֔ԍ��P�v
019520           MOVE ���ہ|���ۑ��t��X�֔ԍ��Q  TO ������X�֔ԍ��Q�v
020220           MOVE ���ہ|���ۑ��t��Z���P      TO ������Z���P�v
020230           MOVE ���ہ|���ۑ��t��Z���Q      TO ������Z���Q�v
                 MOVE SPACE              TO �L���v
                 MOVE ���ہ|���ۋL���ԍ� TO �ԍ��v
              END-READ
           END-IF.
      *
022000* �����ӏ��
022010     IF �ی���ʂv�q = 80
022020        MOVE �{�p�a��N���v�q TO �����|�{�p�a��N��
022030        MOVE ���҃R�[�h�v�q   TO �����|���҃R�[�h
022040        READ �����ӏ��e
022050        INVALID KEY
022060           MOVE SPACE TO �����|���R�[�h
022070           INITIALIZE    �����|���R�[�h
022080        END-READ
022090        MOVE �����|�ی���Дԍ�    TO �ی���|�ی���Дԍ�
022040        READ �ی���Ѓ}�X�^
022050        NOT INVALID KEY
022060           MOVE �ی���|�ی���Ж� TO �����於�̂v
019500           MOVE SPACE              TO �x�����v
                 IF �����|�ی���Б��t��Z���P = SPACE
022450               MOVE �ی���|�X�֔ԍ��P TO ������X�֔ԍ��P�v
022460               MOVE �ی���|�X�֔ԍ��Q TO ������X�֔ԍ��Q�v
022470               MOVE �ی���|�Z���P     TO ������Z���P�v
022480               MOVE �ی���|�Z���Q     TO ������Z���Q�v
                  ELSE
022450               MOVE �����|�ی���Б��t��X�֔ԍ��P TO ������X�֔ԍ��P�v
022460               MOVE �����|�ی���Б��t��X�֔ԍ��Q TO ������X�֔ԍ��Q�v
019580               MOVE �����|�ی���Б��t��Z���P     TO ������Z���P�v
019590               MOVE �����|�ی���Б��t��Z���Q     TO ������Z���Q�v
                  END-IF
022080        END-READ
022170     END-IF.
017790*
017800*================================================================*
017810 �����f�[�^�擾 SECTION.
017820*
017830**************************************************
017840* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾 *
017850* �� ������...���ʁ{������ʂɂĉ��H���Ċi�[     *
017860* �� �����N.......�����N�v                       *
017870* �� ������.......�������v                       *
017880* �� ������.......�������v                       *
017890* �� �{�p�I���N...�I���N�v                       *
017900* �� �{�p�I����...�I�����v                       *
017910* �� �{�p�I����...�I�����v                       *
017920**************************************************
017930     MOVE �{�p�a��v�q       TO ���|�{�p�a��.
017940     MOVE �{�p�N�v�q         TO ���|�{�p�N.
017950     MOVE �{�p���v�q         TO ���|�{�p��.
017960     MOVE ���҃R�[�h�v�q     TO ���|���҃R�[�h.
017970     READ �����f�[�^�e
017980     INVALID KEY
017990         CONTINUE
018000*            /* ���肦�Ȃ� */
018010     NOT INVALID KEY
018020         MOVE ZERO                         TO ���ʐ��v
018030         MOVE ���|���ʐ�                   TO ���ʐ��v
018040         PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018050                 UNTIL ( ���ʂb�m�s > ���ʐ��v )
018060*********************************************
018070* ���j�S�_...������ʁ{���ʂɂĉ��H���Ċi�[ *
018080*********************************************
018090* �������
018100             MOVE SPACE                     TO �������̂v
018110             MOVE 03                        TO ���|�敪�R�[�h
018120             MOVE ���|�������(���ʂb�m�s)  TO ���|���̃R�[�h
018130             READ ���̃}�X�^
018140             INVALID KEY
018150                 MOVE SPACE    TO �������̂v
018160             NOT INVALID KEY
018170*                 MOVE ���|���� TO �������̂v
018180                 MOVE ���|�������� TO �������̂v
018190             END-READ
018200* ����
018210             MOVE SPACE                    TO �������v(���ʂb�m�s)
018220             PERFORM ���̖�������
021050             MOVE ���|�����a��(���ʂb�m�s) TO �����a��v(���ʂb�m�s)
018230             MOVE ���|�����N(���ʂb�m�s)   TO �����N�v(���ʂb�m�s)
018240             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
018250             MOVE ���|������(���ʂb�m�s)   TO �������v(���ʂb�m�s)
018260*
018270             MOVE ���|�J�n�N(���ʂb�m�s)   TO �{�p�J�n�N�v(���ʂb�m�s)
018280             MOVE ���|�J�n��(���ʂb�m�s)   TO �{�p�J�n���v(���ʂb�m�s)
018290             MOVE ���|�J�n��(���ʂb�m�s)   TO �{�p�J�n���v(���ʂb�m�s)
018300*
018310             MOVE ���|�I���N(���ʂb�m�s)   TO �{�p�I���N�v(���ʂb�m�s)
018320             MOVE ���|�I����(���ʂb�m�s)   TO �{�p�I�����v(���ʂb�m�s)
018330             MOVE ���|�I����(���ʂb�m�s)   TO �{�p�I�����v(���ʂb�m�s)
018340*�]�A
018350             EVALUATE ���|�]�A�敪(���ʂb�m�s)
018360             WHEN 1
018370             WHEN 2
018380                 MOVE NC"��"               TO �����`�F�b�N�v(���ʂb�m�s)
018390             WHEN 3
018400                 MOVE NC"��"               TO ���~�`�F�b�N�v(���ʂb�m�s)
018410             WHEN 4
018420                 MOVE NC"��"               TO �]��`�F�b�N�v(���ʂb�m�s)
018430             END-EVALUATE
018440             PERFORM �����f�[�^�ޔ�
018450         END-PERFORM
018460     END-READ.
018470*================================================================*
018480 �{�p�L�^�擾 SECTION.
018490*
018500************************************************************
018510* �A���f�[�^���畉���f�[�^�e���ȉ��̏����擾           *
018520* �� �{�p�J�n�N����...�Y�����镔�ʂɑ΂��ē����ŏ��̎{�p�� *
018530************************************************************
018540     MOVE �{�p�a��v�q  TO �{�L�|�{�p�a��.
018550     MOVE �{�p�N�v�q    TO �{�L�|�{�p�N.
018560     MOVE �{�p���v�q    TO �{�L�|�{�p��.
018570     MOVE ZERO            TO �{�L�|�{�p��.
018580     MOVE ZERO            TO �{�L�|���Ҕԍ�.
018590     MOVE SPACE           TO �{�L�|�}��.
018600     START �{�p�L�^�e   KEY IS >= �{�L�|�{�p�a��N����
018610                                  �{�L�|���҃R�[�h.
018620     IF ��ԃL�[ = "00"
018630         MOVE SPACE  TO �I���t���O�Q
018640         PERFORM �{�p�L�^�e�Ǎ�
018650         PERFORM UNTIL ( �I���t���O�Q       = "YES" ) OR
018660                       ( �{�L�|�{�p�a�� NOT = �{�p�a��v�q ) OR
018670                       ( �{�L�|�{�p�N   NOT = �{�p�N�v�q   ) OR
018680                       ( �{�L�|�{�p��   NOT = �{�p���v�q   )
018690*            **************
018700*            * �J�n�N���� *
018710*            **************
018720             PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
018730                     UNTIL ( ���ʂb�m�s > ���ʐ��v )
018740            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
018750            WHEN �{�L�|�������(���ʂb�m�s) = ������ʂv(���ʂb�m�s) ALSO
018760                 �{�L�|����(���ʂb�m�s)     = ���ʂv(���ʂb�m�s)     ALSO
018770                 �{�L�|���E�敪(���ʂb�m�s) = ���E�敪�v(���ʂb�m�s) ALSO
018780                 �{�L�|�����ʒu�ԍ�(���ʂb�m�s)
018790                                       = �����ʒu�ԍ��v(���ʂb�m�s)  ALSO
018800                 �J�n�N�����擾�t���O(���ʂb�m�s) = SPACE
018810                   MOVE �{�L�|�{�p�N     TO �{�p�J�n�N�v(���ʂb�m�s)
018820                   MOVE �{�L�|�{�p��     TO �{�p�J�n���v(���ʂb�m�s)
018830                   MOVE �{�L�|�{�p��     TO �{�p�J�n���v(���ʂb�m�s)
018840                   MOVE "YES"       TO �J�n�N�����擾�t���O(���ʂb�m�s)
018850*
018860            WHEN OTHER
018870                CONTINUE
018880            END-EVALUATE
018890             END-PERFORM
018900             PERFORM �{�p�L�^�e�Ǎ�
018910         END-PERFORM
018920     END-IF.
018930*================================================================*
018940 �{�p�L�^�e�Ǎ� SECTION.
018950*
018960     READ �{�p�L�^�e NEXT
018970     AT END
018980         MOVE "YES" TO �I���t���O�Q
018990     END-READ.
019000*================================================================*
019010 �����f�[�^�ޔ� SECTION.
019020*
019030*********************************************************************
019040* �@ ���������擾�̂��߁A�����f�[�^�e���畉�������v�ɑޔ�����B
019050* �A �ڍ׃��[�h�p  �A��|�����s����ɂ��A����ʒu����ёւ��邽�߁A
019060*                  �������v  ���畉�����r�v�ɁA
019070*                  ���������v  ���畉�������r�v�ɑޔ�����B
019080*********************************************************************
019090*
019100     MOVE ���|�������Ҕԍ�(���ʂb�m�s) TO �����������Ҕԍ��v(���ʂb�m�s).
019110     MOVE ���|�����A��(���ʂb�m�s)     TO ���������A�Ԃv(���ʂb�m�s).
019120*--------------------------------------------------------------------*
019130     MOVE �������v(���ʂb�m�s) TO �������r�v(���ʂb�m�s).
019140*
019150     MOVE �����a��v(���ʂb�m�s) TO �����a��r�v(���ʂb�m�s).
019150     MOVE �����N�v(���ʂb�m�s) TO �����N�r�v(���ʂb�m�s).
019160     MOVE �������v(���ʂb�m�s) TO �������r�v(���ʂb�m�s).
019170     MOVE �������v(���ʂb�m�s) TO �������r�v(���ʂb�m�s).
019180*
019190     MOVE �{�p�J�n�N�v(���ʂb�m�s) TO �{�p�J�n�N�r�v(���ʂb�m�s).
019200     MOVE �{�p�J�n���v(���ʂb�m�s) TO �{�p�J�n���r�v(���ʂb�m�s).
019210     MOVE �{�p�J�n���v(���ʂb�m�s) TO �{�p�J�n���r�v(���ʂb�m�s).
019220*
019230     MOVE �{�p�I���N�v(���ʂb�m�s) TO �{�p�I���N�r�v(���ʂb�m�s).
019240     MOVE �{�p�I�����v(���ʂb�m�s) TO �{�p�I�����r�v(���ʂb�m�s).
019250     MOVE �{�p�I�����v(���ʂb�m�s) TO �{�p�I�����r�v(���ʂb�m�s).
019260*
019270     MOVE �����`�F�b�N�v(���ʂb�m�s) TO �����`�F�b�N�r�v(���ʂb�m�s).
019280     MOVE ���~�`�F�b�N�v(���ʂb�m�s) TO ���~�`�F�b�N�r�v(���ʂb�m�s).
019290     MOVE �]��`�F�b�N�v(���ʂb�m�s) TO �]��`�F�b�N�r�v(���ʂb�m�s).
019300*     MOVE �{�p�݌v�񐔂v(���ʂb�m�s) TO �{�p�݌v�񐔂r�v(���ʂb�m�s).
019310*
019320     MOVE �����������Ҕԍ��v(���ʂb�m�s) TO �������Ҕԍ��r�v(���ʂb�m�s).
019330     MOVE ���������A�Ԃv(���ʂb�m�s)     TO �����A�Ԃr�v(���ʂb�m�s).
019340*
019350*================================================================*
019360 ���������擾 SECTION.
019370*
019380********************************************************************
019390*  ���������R�[�h���������̂́A1�s�ɂ܂Ƃ߂Ĉ󎚂���B
019400*  ��: �@�A �Ƃœ]��.
019410*     ���������R�[�h���������̂��܂Ƃ߁A�e�[�u���ɃZ�b�g
019420*     (�������A���ʂ���œ������̂́A2�s�ɂȂ�)
019430********************************************************************
019440     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
019450     PERFORM VARYING ���ʂb�m�s FROM 1 BY 1
019460*/�o�^���ʐ��ȏ�̍s�Ɉ���o���Ȃ��s��̏C��/090311
019470             UNTIL ( ���ʂb�m�s > 7 )
019480*             UNTIL ( ���ʂb�m�s > ���ʐ��v )
019490*
019500****        IF ( ���|�������Ҕԍ�(���ʂb�m�s)  NOT = ZERO )  AND
019510        IF ( ���������A�Ԃv(���ʂb�m�s)      NOT = ZERO )
019520*
019530           IF �J�E���^ = ZERO
019540               MOVE 1   TO  �J�E���^ �J�E���^�Q
019550               MOVE �����������Ҕԍ��v(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
019560               MOVE ���������A�Ԃv(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)   �����A�Ԃb�v
019570               MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
019580           ELSE
019590              IF ( �����������Ҕԍ��v(���ʂb�m�s)  = �������Ҕԍ��b�v )  AND
019600                 ( ���������A�Ԃv(���ʂb�m�s)      = �����A�Ԃb�v     )
019610                 COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
019620                 MOVE ���ʂb�m�s                  TO �����������ʂv(�J�E���^ �J�E���^�Q)
019630              ELSE
019640                 COMPUTE �J�E���^ = �J�E���^  +  1
019650                 MOVE 1   TO  �J�E���^�Q
019660                 MOVE �����������Ҕԍ��v(���ʂb�m�s) TO �������Ҕԍ��v(�J�E���^)  �������Ҕԍ��b�v
019670                 MOVE ���������A�Ԃv(���ʂb�m�s)     TO �����A�Ԃv(�J�E���^)  �����A�Ԃb�v
019680                 MOVE ���ʂb�m�s                   TO �����������ʂv(�J�E���^ �J�E���^�Q)
019690              END-IF
019700           END-IF
019710        END-IF
019720     END-PERFORM.
019730**************************************************************************
019740*  ���������}�X�^��蕶�͎擾
019750**************************************************************************
019760     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
019770     PERFORM VARYING �J�E���^ FROM 1 BY 1
019780             UNTIL ( �J�E���^ > 9 )  OR ( �����A�Ԃv(�J�E���^) = ZERO )
019790** ���ۂ� �敪 01
019800*         MOVE 01                        TO �����|�敪�R�[�h
019810*/���ہF�O�P�A�J�ЁF�O�Q�A�����ӁF�O�R
019820         EVALUATE ��|�ی����
019830         WHEN 70
019840             MOVE 2 TO �����|�敪�R�[�h
019850         WHEN 80
019860             MOVE 3 TO �����|�敪�R�[�h
019870         WHEN OTHER
019880             MOVE 1 TO �����|�敪�R�[�h
019890         END-EVALUATE
019900         MOVE �������Ҕԍ��v(�J�E���^)  TO �����|���Ҕԍ�
019910         MOVE �����A�Ԃv(�J�E���^)      TO �����|���������A��
019920         READ ���������e
019930         NOT INVALID KEY
019940             INITIALIZE ���������v�s
019950             MOVE �����|���������b�l(1) TO  ���������P�v�s
019960             MOVE �����|���������b�l(2) TO  ���������Q�v�s
019970             MOVE �����|���������b�l(3) TO  ���������R�v�s
019980             MOVE �����|���������b�l(4) TO  ���������S�v�s
019990             MOVE �����|���������b�l(5) TO  ���������T�v�s
020000             PERFORM VARYING �J�E���^�Q FROM 1 BY 1
020010                     UNTIL ( �J�E���^�Q > 9 )  OR 
020020                           ( �����������ʂv(�J�E���^ �J�E���^�Q) = ZERO )
020030                EVALUATE �����������ʂv(�J�E���^ �J�E���^�Q)
020040                WHEN 1
020050                   MOVE "�@"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020060                WHEN 2
020070                   MOVE "�A"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020080                WHEN 3
020090                   MOVE "�B"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020100                WHEN 4
020110                   MOVE "�C"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020120                WHEN 5
020130                   MOVE "�D"  TO  ���������i���o�[�v�P(�J�E���^�Q)
020140                WHEN OTHER
020150                   CONTINUE
020160                END-EVALUATE
020170             END-PERFORM
020180*
020190             IF �����|�����������͋敪 = 1
020200                 STRING ���������i���o�[�m�v  DELIMITED BY SPACE
020210                        ���������P�v�s  DELIMITED BY SIZE
020220                        ���������Q�v�s  DELIMITED BY SIZE
020230                        ���������R�v�s  DELIMITED BY SIZE
020240                        ���������S�v�s  DELIMITED BY SIZE
020250                        ���������T�v�s  DELIMITED BY SIZE
020260                        INTO �����������e�����v(�J�E���^)
020270                 END-STRING
020280             ELSE
020290                 INSPECT ���������v�s REPLACING ALL �S�p�� BY ���p��
020300                 MOVE SPACE TO �����P�v �����Q�v
020310                 MOVE ���������i���o�[�m�v TO �����P�v
020320                 MOVE ���������P�v�s       TO �����Q�v
020330                 CALL �v���O�������v WITH C LINKAGE
020340                      USING BY REFERENCE �����P�v
020350                            BY REFERENCE �����Q�v
020360                 MOVE ���������Q�v�s       TO �����Q�v
020370                 CALL �v���O�������v WITH C LINKAGE
020380                      USING BY REFERENCE �����P�v
020390                            BY REFERENCE �����Q�v
020400                 MOVE ���������R�v�s       TO �����Q�v
020410                 CALL �v���O�������v WITH C LINKAGE
020420                      USING BY REFERENCE �����P�v
020430                            BY REFERENCE �����Q�v
020440                 MOVE ���������S�v�s       TO �����Q�v
020450                 CALL �v���O�������v WITH C LINKAGE
020460                      USING BY REFERENCE �����P�v
020470                            BY REFERENCE �����Q�v
020480                 MOVE ���������T�v�s       TO �����Q�v
020490                 CALL �v���O�������v WITH C LINKAGE
020500                      USING BY REFERENCE �����P�v
020510                            BY REFERENCE �����Q�v
020520                  MOVE �����P�v TO �����������e�����v(�J�E���^)
020530             END-IF
020540*
020550         END-READ
020560     END-PERFORM.
020570*
020580     PERFORM ���������Z�b�g.
020590*
020600*================================================================*
020610 ���������Z�b�g SECTION.
020620*
020630**************************************************************************
020640*  ���͂�1�s�𒴂��鎞�́A�����s�ɕ�������B
020650**************************************************************************
020660     MOVE  ZERO   TO  �J�E���^ �J�E���^�Q.
020670     PERFORM VARYING �J�E���^ FROM 1 BY 1
020680             UNTIL ( �J�E���^ > 9 )  OR ( �����������e�����v(�J�E���^) = SPACE )
020690*
020700          INITIALIZE �����������e�����w�v
020710          MOVE �����������e�����v(�J�E���^)   TO �����������e�����w�v
020720          IF  �����������e�P�w�v  NOT = SPACE
020730              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
020740              MOVE �����������e�P�w�v  TO ���������v(�J�E���^�Q)
020750          END-IF
020760          IF  �����������e�Q�w�v  NOT = SPACE
020770              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
020780              MOVE �����������e�Q�w�v  TO ���������v(�J�E���^�Q)
020790          END-IF
020800          IF  �����������e�R�w�v  NOT = SPACE
020810              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
020820              MOVE �����������e�R�w�v  TO ���������v(�J�E���^�Q)
020830          END-IF
020840          IF  �����������e�S�w�v  NOT = SPACE
020850              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
020860              MOVE �����������e�S�w�v  TO ���������v(�J�E���^�Q)
020870          END-IF
020880          IF  �����������e�T�w�v  NOT = SPACE
020890              COMPUTE �J�E���^�Q = �J�E���^�Q  +  1
020900              MOVE �����������e�T�w�v  TO ���������v(�J�E���^�Q)
020910          END-IF
020920*
020930     END-PERFORM.
020940*
020950*================================================================*
020960 �����s�Z�b�g SECTION.
020970*/0610
020980* �w�肳�ꂽ�J�n�s����������悤�ɃZ�b�g�������B
020990* �P�O�s.
021000*
021010     MOVE SPACE                  TO ���������v�e�[�u���Q.
021020     MOVE ZERO                   TO �J�E���^.
021030     IF �A��|�����J�n�s = ZERO
021040         MOVE 1                  TO �J�E���^�Q
021050     ELSE
021060         MOVE �A��|�����J�n�s   TO �J�E���^�Q
021070     END-IF.
021080     PERFORM VARYING �J�E���^ FROM 1 BY 1
021090             UNTIL ( �J�E���^ > 10 ) OR (�J�E���^�Q > 10)
021100         MOVE ���������v(�J�E���^) TO ���������v�Q(�J�E���^�Q)
021110         COMPUTE �J�E���^�Q = �J�E���^�Q + 1
021120     END-PERFORM.
021130*
021140     PERFORM VARYING �J�E���^ FROM 1 BY 1
021150               UNTIL �J�E���^ > 10
021160         MOVE ���������v�Q(�J�E���^) TO ���������v(�J�E���^)
021170     END-PERFORM.
021180*================================================================*
021190 ������� SECTION.
021200*
021210     EVALUATE ������[�h�e�v�q
021310     WHEN 0
021320         PERFORM ��������P
021260     WHEN 4
021270         IF �A��|�ی��؈���敪 = 1
021280             PERFORM ��������Q
021290         END-IF
021270         IF (�A��|��������敪 = 1) OR (�A��|�{�p����敪 = 1)
021300             PERFORM ��������R
               END-IF
021310     WHEN 5
021320         PERFORM ��������R
021330     END-EVALUATE.
021340*================================================================*
021350 ��������P SECTION.
021360*
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
013440*
021390     MOVE "YHN662P"  TO  ��`�̖��o.
021400     MOVE SPACE       TO  ������ʂo.
021410     MOVE "GRP001"    TO  ���ڌQ���o.
021420     WRITE YHN662P.
021430     PERFORM �G���[�����o.
021440*
021450     MOVE "YHN662P"  TO  ��`�̖��o.
021460     MOVE SPACE       TO  ������ʂo.
021470     MOVE "GRP002"    TO  ���ڌQ���o.
021480     WRITE YHN662P.
021490     PERFORM �G���[�����o.
021630*================================================================*
021640 ��������Q SECTION.
021650*
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
013440*
021680     MOVE "YHN662P"  TO  ��`�̖��o.
021690     MOVE SPACE       TO  ������ʂo.
021700     MOVE "GRP001"    TO  ���ڌQ���o.
021710     WRITE YHN662P.
021720     PERFORM �G���[�����o.
021800*================================================================*
021810 ��������R SECTION.
021820*
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
013440*
021850     MOVE "YHN662P"  TO  ��`�̖��o.
021860     MOVE SPACE       TO  ������ʂo.
021870     MOVE "GRP002"    TO  ���ڌQ���o.
021880     WRITE YHN662P.
021890     PERFORM �G���[�����o.
003730*================================================================*
003740 ���ŏ���  SECTION.
003750*
003760     MOVE "YHN662P" TO  ��`�̖��o.
003770     MOVE "CT"      TO  ������ʂo.
003780     MOVE "PAGE"    TO  �g������o.
003790     MOVE SPACE     TO  ���ڌQ���o.
003800     WRITE YHN662P.
003810     PERFORM �G���[�����o.
003820     MOVE SPACE     TO  �g������o.
003821*
003822     CLOSE  ����t�@�C��.
004320     MOVE SPACE TO �I�[�v���t���O.
           MOVE SPACE TO YHN662P.
003825*
021970*================================================================*
021980 �G���[�����o SECTION.
021990*
022000     IF �ʒm���o NOT = "00"
022010         DISPLAY NC"���[�G���["              UPON CONS
022020         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
022030         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
022040         DISPLAY NC"�g������o�F" �g������o UPON CONS
022050         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
022060                                             UPON CONS
022070*-----------------------------------------*
022080         CALL "actcshm"  WITH C LINKAGE
022090*-----------------------------------------*
022100         ACCEPT  �L�[���� FROM CONS
022110         PERFORM �t�@�C����
022120         EXIT PROGRAM
022130     END-IF.
022140*================================================================*
022150 ���̖������� SECTION.
022160*
022170     EVALUATE ��|�ی����
022180     WHEN 05
022190         MOVE 2          TO ���Z�|���Z���
022200     WHEN 70
022210         MOVE 4          TO ���Z�|���Z���
022220     WHEN 80
022230         MOVE 5          TO ���Z�|���Z���
022240     WHEN 85
022250         MOVE 7          TO ���Z�|���Z���
022260     WHEN 90
022270         MOVE 6          TO ���Z�|���Z���
022280     WHEN 91
022290         MOVE 8          TO ���Z�|���Z���
022300     WHEN OTHER
022310         MOVE 1          TO ���Z�|���Z���
022320     END-EVALUATE.
022330     MOVE ��|�{�p�a�� TO ���Z�|�{�p�a��.
022340     MOVE ��|�{�p�N   TO ���Z�|�{�p�N.
022350     MOVE ��|�{�p��   TO ���Z�|�{�p��.
022360     MOVE ��|���Ҕԍ� TO ���Z�|���Ҕԍ�.
022370     MOVE ��|�}��     TO ���Z�|�}��.
022380     READ ���Z�v�g�e
022390     NOT INVALID KEY
022400*
022410         STRING ���Z�|���ʖ��̂P(���ʂb�m�s)  DELIMITED BY SPACE
022420                ���Z�|���ʖ��̂Q(���ʂb�m�s)  DELIMITED BY SPACE
022430                �������̂v                    DELIMITED BY SPACE
022440           INTO �������v(���ʂb�m�s)
022450         END-STRING
022460     END-READ.
022470*
022480*================================================================*
022490 �ڍ׃��[�h������������ SECTION.
022500*
022510*******************************************************************
022520* �A��|��������s�ɂ��A���������f�[�^�̕��ёւ����s���B
022530* ( �����������v�����������A�ޔ��ς݂̕����������r�v����
022540*         ���ёւ����s���A�����������v���Đݒ肷��B)
022550*******************************************************************
022560*
022570     INITIALIZE �����������v.
022580*
022590     PERFORM VARYING �ڍׂb�m�s FROM 1 BY 1
022600                                UNTIL ( �ڍׂb�m�s > 5)
022610*
022620        IF ( �A��|��������s(�ڍׂb�m�s) NOT = ZERO) AND
022630           ( �A��|��������敪(�ڍׂb�m�s)   = 1)
022640*
022650           MOVE �A��|��������s(�ڍׂb�m�s) TO ����s�r�v
022660*
022670           MOVE �������Ҕԍ��r�v(�ڍׂb�m�s) TO �����������Ҕԍ��v(����s�r�v)
022680           MOVE �����A�Ԃr�v(�ڍׂb�m�s) TO ���������A�Ԃv(����s�r�v)
022690        END-IF
022700*
022710     END-PERFORM.
022720*
022730*================================================================*
022740 �ڍ׃��[�h�������� SECTION.
022750*
022760*******************************************************************
022770* �A��|��������s�ɂ��A�����f�[�^�̕��ёւ����s���B
022780* ( �������v�����������A�ޔ��ς݂̕������r�v����
022790*         ���ёւ����s���A�������v���Đݒ肷��B)
022800*******************************************************************
022810*
022820     INITIALIZE �������v.
022830*
022840     PERFORM VARYING �ڍׂb�m�s FROM 1 BY 1
022850                                UNTIL ( �ڍׂb�m�s > 5)
022860*
022870        IF �A��|��������s(�ڍׂb�m�s) NOT = ZERO
022880*/�]�A�݈̂󎚂��o���Ȃ��s��̏C���B
022890           MOVE �A��|��������s(�ڍׂb�m�s) TO ����s�r�v
022900           IF �A��|���ʈ���敪(�ڍׂb�m�s) = 1
022910*
022920*              MOVE �A��|��������s(�ڍׂb�m�s) TO ����s�r�v
022930*
022940               MOVE �������r�v(�ڍׂb�m�s) TO �������v(����s�r�v)
022950               MOVE �����a��r�v(�ڍׂb�m�s) TO �����a��v(����s�r�v)
022950               MOVE �����N�r�v(�ڍׂb�m�s) TO �����N�v(����s�r�v)
022960               MOVE �������r�v(�ڍׂb�m�s) TO �������v(����s�r�v)
022970               MOVE �������r�v(�ڍׂb�m�s) TO �������v(����s�r�v)
022980               MOVE �{�p�J�n�N�r�v(�ڍׂb�m�s) TO �{�p�J�n�N�v(����s�r�v)
022990               MOVE �{�p�J�n���r�v(�ڍׂb�m�s) TO �{�p�J�n���v(����s�r�v)
023000               MOVE �{�p�J�n���r�v(�ڍׂb�m�s) TO �{�p�J�n���v(����s�r�v)
023010           END-IF
023020*
023030           IF �A��|�]�A����敪(�ڍׂb�m�s) = 1
023040               MOVE �{�p�I���N�r�v(�ڍׂb�m�s) TO �{�p�I���N�v(����s�r�v)
023050               MOVE �{�p�I�����r�v(�ڍׂb�m�s) TO �{�p�I�����v(����s�r�v)
023060               MOVE �{�p�I�����r�v(�ڍׂb�m�s) TO �{�p�I�����v(����s�r�v)
023070               MOVE �����`�F�b�N�r�v(�ڍׂb�m�s)   TO �����`�F�b�N�v(����s�r�v)
023080               MOVE ���~�`�F�b�N�r�v(�ڍׂb�m�s)   TO ���~�`�F�b�N�v(����s�r�v)
023090               MOVE �]��`�F�b�N�r�v(�ڍׂb�m�s)   TO �]��`�F�b�N�v(����s�r�v)
023100           END-IF
023110        END-IF
023120*
023130     END-PERFORM.
023140*
025960*================================================================*
025970 �󎚃e�X�g SECTION.
025980     MOVE NC"��" TO ���i���a�`�F�b�N ���i�����`�F�b�N.
025990     MOVE NC"��" TO �؃`�F�b�N.
026000     MOVE NC"��" TO �O���`�F�b�N �P���`�F�b�N �Q���`�F�b�N �R���`�F�b�N.
026010     MOVE NC"��" TO �Еۃ`�F�b�N �g���`�F�b�N ���σ`�F�b�N ���ك`�F�b�N
026020                    �D���`�F�b�N ���ۃ`�F�b�N �ޖ{�`�F�b�N �މƃ`�F�b�N.
026030     MOVE NC"��" TO �V�`�F�b�N ��`�F�b�N ��`�F�b�N ���`�F�b�N
026040                    ���`�F�b�N �{�l�`�F�b�N �Ƒ��`�F�b�N.
026050     MOVE NC"��" TO �����`�F�b�N �吳�`�F�b�N ���a�`�F�b�N �����`�F�b�N.
026060     MOVE NC"��" TO ��ی��Җ��� ��ی��ґ吳 ��ی��ҏ��a ��ی��ҕ���.
026070     MOVE NC"��" TO �����`�F�b�N�P �����`�F�b�N�Q �����`�F�b�N�R �����`�F�b�N�S �����`�F�b�N�T.
026080     MOVE NC"��" TO ���~�`�F�b�N�P ���~�`�F�b�N�Q ���~�`�F�b�N�R ���~�`�F�b�N�S ���~�`�F�b�N�T.
026090     MOVE NC"��" TO �]��`�F�b�N�P �]��`�F�b�N�Q �]��`�F�b�N�R �]��`�F�b�N�S �]��`�F�b�N�T.
026100     MOVE NC"��" TO �j�`�F�b�N ���`�F�b�N ���Ғj�`�F�b�N ���ҏ��`�F�b�N.
026110     MOVE ALL "X"    TO �s�����ԍ� ��v�Ҕԍ� �ی��Ҕԍ� �ԍ�.
026120     MOVE ALL "X"    TO ��ی��҃J�i ���҃J�i �X�֔ԍ��P �X�֔ԍ��Q ���җX�֔ԍ��P ���җX�֔ԍ��Q.
026130     MOVE ALL "�_"   TO �L�� ��ی��Ҏ��� ���Ҏ���.
026140     MOVE ALL "�_"   TO �Z���P �Z���Q ���ҏZ���P ���ҏZ���Q.
026150     MOVE ALL "9"    TO ��ی��҃J�i ���҃J�i
026160                        ���i�擾�N ���i�擾�� ���i�擾�� �L���N �L���� �L����
026170                        ��ی��ҔN ��ی��Ҍ� ��ی��ғ� ���ҔN ���Ҍ� ���ғ�.
026180     MOVE ALL "X"    TO ���Ə��X�֔ԍ��P ���Ə��X�֔ԍ��Q ������X�֔ԍ��P ������X�֔ԍ��Q.
026190     MOVE ALL "�_"   TO ���Ə����� ���Ə��Z�� �����於�̂P �����於�̂Q ������Z���P ������Z���Q.
026200     MOVE ALL NC"�_" TO ���� �������P �������Q �������R �������S �������T.
026210     MOVE ALL "�_"   TO ���������P ���������Q ���������R ���������S ���������T
026220                        ���������U ���������V ���������W ���������X ���������P�O.
026230     MOVE ALL "9"    TO �����N�P �����N�Q �����N�R �����N�S �����N�T
026240                        �������P �������Q �������R �������S �������T
026250                        �������P �������Q �������R �������S �������T
026260                        �{�p�J�n�N�P �{�p�J�n�N�Q �{�p�J�n�N�R �{�p�J�n�N�S �{�p�J�n�N�T
026270                        �{�p�J�n���P �{�p�J�n���Q �{�p�J�n���R �{�p�J�n���S �{�p�J�n���T
026280                        �{�p�J�n���P �{�p�J�n���Q �{�p�J�n���R �{�p�J�n���S �{�p�J�n���T
026290                        �{�p�I���N�P �{�p�I���N�Q �{�p�I���N�R �{�p�I���N�S �{�p�I���N�T
026300                        �{�p�I�����P �{�p�I�����Q �{�p�I�����R �{�p�I�����S �{�p�I�����T
026310                        �{�p�I�����P �{�p�I�����Q �{�p�I�����R �{�p�I�����S �{�p�I�����T.
023910*================================================================*
       �Ǐ󏈒u�o�߃Z�b�g SECTION.
      *
      */�Ǐ�A�o��
           MOVE 1 TO �J�E���^�Q
           PERFORM 2 TIMES
               PERFORM VARYING �J�E���^�R FROM 1 BY 1 UNTIL (�J�E���^�R > ���ʐ��v) OR (�J�E���^�R > 4)
                   STRING "("        DELIMITED BY SIZE
                          �J�E���^�R DELIMITED BY SIZE
                          ")��E�u�ɁE���ɁE�^���Ɏ����ɒ����@�c��" DELIMITED BY SIZE
                     INTO �Ǐ�v(�J�E���^�Q)
                   END-STRING
                   MOVE �Ǐ�v(�J�E���^�Q) TO �o�߂v(�J�E���^�Q)
                   COMPUTE �J�E���^�Q = �J�E���^�Q + 1
               END-PERFORM
           END-PERFORM.
      */���u
           MOVE 1 TO �J�E���^�Q
           PERFORM 2 TIMES
               PERFORM VARYING �J�E���^�R FROM 1 BY 1 UNTIL (�J�E���^�R > ���ʐ��v) OR (�J�E���^�R > 4)
      *             STRING "("        DELIMITED BY SIZE
      *                    �J�E���^�R DELIMITED BY SIZE
      *                    ")���񏈒u�A�{�ÁA㪖@�A���z(�N,�e,�L,��)�Œ�" DELIMITED BY SIZE
      *               INTO ���u�v(�J�E���^�Q)
      *             END-STRING
      *             COMPUTE �J�E���^�Q = �J�E���^�Q + 1
      *             MOVE "�@���,�d��,��Z,㪖@" TO ���u(�J�E���^�Q)
      *             COMPUTE �J�E���^�Q = �J�E���^�Q + 1
                   STRING "("        DELIMITED BY SIZE
                          �J�E���^�R DELIMITED BY SIZE
                          ")���񏈒u�A�{�ÁA㪖@�A���z(�N,�e,�L,��)�Œ�" DELIMITED BY SIZE
                          " ���,�d��,��Z" DELIMITED BY SIZE
                     INTO ���u�v(�J�E���^�Q)
                   END-STRING
                   COMPUTE �J�E���^�Q = �J�E���^�Q + 1
               END-PERFORM
           END-PERFORM.
006850*================================================================*
006860 ����s���ޔ� SECTION.
006870     CLOSE ��f�ҏ��e.
006890     OPEN I-O ��f�ҏ��e.
006900         MOVE NC"��f" TO �t�@�C����.
006910         PERFORM �I�[�v���`�F�b�N.
006930*
006940     MOVE ZERO            TO ��|�{�p�a��.
006950     MOVE ZERO            TO ��|�{�p�N.
006960     MOVE ZERO            TO ��|�{�p��.
006970     MOVE ���҃R�[�h�v    TO ��|���҃R�[�h.
006990*
007000     READ ��f�ҏ��e
007010     NOT INVALID KEY
007020         MOVE �󎚈ʒu�b�m�s TO ��|�J���e������s��
007030         REWRITE ��|���R�[�h
007040         INVALID KEY
007050             MOVE NC"��e" TO �t�@�C����
007060             PERFORM �G���[�\��
007070         END-REWRITE
007080     END-READ.
007100     CLOSE ��f�ҏ��e.
007120     OPEN INPUT ��f�ҏ��e.
007130         MOVE NC"��f" TO �t�@�C����.
007140         PERFORM �I�[�v���`�F�b�N.
007610*================================================================*
007620 �G���[�\�� SECTION.
007630*
007640     DISPLAY �t�@�C����   NC"�t�@�C�������G���["   UPON CONS.
007650     DISPLAY NC"��ԃL�[�F" ��ԃL�[               UPON CONS.
007660     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
007670     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"  UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
007680     ACCEPT  �L�[���� FROM CONS.
007690     PERFORM �t�@�C����.
007700     EXIT PROGRAM.
026320*================================================================*
026330******************************************************************
026340 END PROGRAM YHN662.
026350******************************************************************
