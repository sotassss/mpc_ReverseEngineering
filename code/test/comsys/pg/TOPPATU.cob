000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             TOPPATU.
000060 AUTHOR.                 �R�c �_�V
000070*
000080*------------------------------------------------------------------*
000090* �˔����Â̐���
000100* �����ɓ˔��������Z��\�����ׂ�
000110* 
000170*------------------------------------------------------------------*
000180 DATE-WRITTEN.           2004-08-26
000190 DATE-COMPILED.          2004-08-26
000200*----------------------------------------------------------------*
000372******************************************************************
000380*            ENVIRONMENT         DIVISION                        *
000390******************************************************************
000400 ENVIRONMENT             DIVISION.
000410 CONFIGURATION           SECTION.
000420 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000430 OBJECT-COMPUTER.        FMV-DESKPOWER.
000440 SPECIAL-NAMES.          CONSOLE  IS  CONS
000450                         SYSERR   IS  MSGBOX.
000460 INPUT-OUTPUT            SECTION.
000470 FILE-CONTROL.
000480*
001710     SELECT  �Q�Ƃg���v�f�[�^�e  ASSIGN      TO        HNIKEIL
001720                             ORGANIZATION             IS  INDEXED
001730                             ACCESS MODE              IS  DYNAMIC
001740                             RECORD KEY               IS  �Q�Ƃg���|�{�p�敪
001750                                                          �Q�Ƃg���|�{�p�a��N����
001760                                                          �Q�Ƃg���|���҃R�[�h
001770                             ALTERNATE RECORD KEY     IS  �Q�Ƃg���|�{�p�敪
001780                                                          �Q�Ƃg���|���҃R�[�h
001790                                                          �Q�Ƃg���|�{�p�a��N����
001800                             ALTERNATE RECORD KEY     IS  �Q�Ƃg���|�{�p�a��N����
001810                                                          �Q�Ƃg���|�o�^��
001820                             FILE STATUS              IS  ��ԃL�[
001830                             LOCK        MODE         IS  AUTOMATIC.
001840*
001160     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
001170                             ORGANIZATION             IS  INDEXED
001180                             ACCESS MODE              IS  DYNAMIC
001190                             RECORD KEY               IS  ���|�����敪
001200                             FILE STATUS              IS  ��ԃL�[
001210                             LOCK        MODE         IS  AUTOMATIC.
002340     SELECT  �g���Î��тe    ASSIGN      TO        HNOURYOL
002350                             ORGANIZATION             IS  INDEXED
002360                             ACCESS MODE              IS  DYNAMIC
002370                             RECORD KEY               IS  �g�����|�{�p�敪
002380                                                          �g�����|�{�p�a��N����
002390                                                          �g�����|���҃R�[�h
002400                             ALTERNATE RECORD KEY     IS  �g�����|�{�p�敪
002410                                                          �g�����|���҃R�[�h
002420                                                          �g�����|�{�p�a��N����
002430                             ALTERNATE RECORD KEY     IS  �g�����|�{�p�a��N����
002440                                                          �g�����|�{�p�Ҕԍ�
002450                                                          �g�����|�o�^��
002460                             ALTERNATE RECORD KEY     IS  �g�����|�{�p�a��N����
002470                                                          �g�����|�{�p�J�n����
002480                                                          �g�����|�{�p�Ҕԍ�
002490                                                          �g�����|�o�^��
002500                             FILE STATUS              IS  ��ԃL�[
002510                             LOCK        MODE         IS  AUTOMATIC.
002770******************************************************************
002780*                      DATA DIVISION                             *
002790******************************************************************
002800 DATA                    DIVISION.
002810 FILE                    SECTION.
002820**
003090*
003100 FD  �Q�Ƃg���v�f�[�^�e  BLOCK   CONTAINS   1   RECORDS.
003110     COPY H_NIKEI    OF  XFDLIB  JOINING   �Q�Ƃg��   AS  PREFIX.
002200*                           �m�q�k��  �P�Q�W�n
002210 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002220     COPY GENGOU     OF  XFDLIB  JOINING   ��   AS  PREFIX.
003210*
003220 FD  �g���Î��тe        BLOCK   CONTAINS   1   RECORDS.
003230     COPY H_NOURYO   OF  XFDLIB  JOINING   �g����   AS  PREFIX.
003280******************************************************************
003290*                WORKING-STORAGE SECTION                         *
003300******************************************************************
003310 WORKING-STORAGE         SECTION.
003320 01 �L�[����                           PIC X    VALUE SPACE.
003330 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
003340 01 �I���t���O                         PIC X(3) VALUE SPACE.
003350 01 �I���t���O�Q                       PIC X(3) VALUE SPACE.
003360 01 �I���t���O�R                       PIC X(3) VALUE SPACE.
003360 01 �I���t���O�S                       PIC X(3) VALUE SPACE.
003370 01 �t�@�C����                         PIC N(10) VALUE SPACE.
002490 01 �v�Z�a��N�����v.
002500     03 �v�Z�a��v                     PIC 9    VALUE ZERO.
002510     03 �v�Z�N���v.
002520        05 �v�Z�N�v                    PIC 9(2) VALUE ZERO.
002530        05 �v�Z���v                    PIC 9(2) VALUE ZERO.
002540     03 �v�Z���v                       PIC 9(2) VALUE ZERO.
002550*
002560 01 ��Βl�v                           PIC 9(6) VALUE ZERO.
002570 01 ��Βl���v                         PIC 9(3) VALUE ZERO.
002580 01 ��Βl���v�Q                       PIC 9(3) VALUE ZERO.
002590 01 �����o�ߓ��v                       PIC 9(4) VALUE ZERO.
002600 01 ����N�v                           PIC 9(4) VALUE ZERO.
002610 01 ����N���v                         PIC 9(2) VALUE ZERO.
002620 01 ����N���Q�v                       PIC 9(2) VALUE ZERO.
002630 01 �[�N�񐔂v                         PIC 9(2) VALUE ZERO.
002640 01 �ʏ�N�񐔂v                       PIC 9(2) VALUE ZERO.
002650 01 �ʏ�N��Βl�v                     PIC 9(6) VALUE ZERO.
002660 01 �[�N��Βl�v                       PIC 9(6) VALUE ZERO.
002670 01 �N��Βl�v                         PIC 9(6) VALUE ZERO.
002680 01 ������Βl�v                       PIC 9(6) VALUE ZERO.
002690 01 �{�p��Βl�v                       PIC 9(6) VALUE ZERO.
002700 01 ���Z�N�v                           PIC 9(4) VALUE ZERO.
002560 01 ������Βl�v                       PIC 9(6) VALUE ZERO.
001540 01 ��]�v                             PIC 9(4) VALUE ZERO.
006256******************************************************************
006257*                          �A������                              *
006258******************************************************************
006259*
       01 �A���Q�O�P�|��ʏ�� IS EXTERNAL GLOBAL.
          03 �A���Q�O�P�|�{�p�敪            PIC 9(1).
          03 �A���Q�O�P�|�{�p�a��N����.
             05 �A���Q�O�P�|�{�p�a��N��.
                07 �A���Q�O�P�|�{�p�a��         PIC 9(1).
                07 �A���Q�O�P�|�{�p�N           PIC 9(2).
                07 �A���Q�O�P�|�{�p��           PIC 9(2).
             05 �A���Q�O�P�|�{�p��              PIC 9(2).
          03 �A���Q�O�P�|���҃R�[�h.
             05 �A���Q�O�P�|���Ҕԍ�         PIC 9(6).
             05 �A���Q�O�P�|�}��             PIC X(1).
          03 �A���Q�O�P�|�G���[�t���O        PIC X(3).

007476******************************************************************
007477*                      PROCEDURE  DIVISION                       *
007478******************************************************************
007479 PROCEDURE               DIVISION.
HILO  *     DISPLAY "Top:St----" �A���Q�O�P�|�{�p�a��N���� " " �A���Q�O�P�|���҃R�[�h
007482************
007483*           *
007484* ��������   *
007485*           *
007486************
007720     PERFORM �t�@�C���I�[�v��.
007492************
007493*           *
007494* �又��     *
007495*           *
007496************
           MOVE SPACE TO �A���Q�O�P�|�G���[�t���O.
           MOVE SPACE TO �I���t���O.
027940     MOVE �A���Q�O�P�|�{�p�a��N���� TO �v�Z�a��N�����v.
014470     PERFORM ���t��Βl�v�Z.
014400     MOVE ��Βl�v                   TO ������Βl�v.
HILO***       DISPLAY "TOP-1 " �A���Q�O�P�|�{�p�a��N���� " ������Βl�v=" ������Βl�v.
027910     MOVE �A���Q�O�P�|�{�p�敪       TO �Q�Ƃg���|�{�p�敪
027920     MOVE �A���Q�O�P�|���҃R�[�h     TO �Q�Ƃg���|���҃R�[�h
027940     MOVE �A���Q�O�P�|�{�p�a��N���� TO �Q�Ƃg���|�{�p�a��N����
027970*
028050     START �Q�Ƃg���v�f�[�^�e KEY IS < �Q�Ƃg���|�{�p�敪
028060                                       �Q�Ƃg���|���҃R�[�h
028070                                       �Q�Ƃg���|�{�p�a��N����
                                             REVERSED
028080     END-START
HILO***       DISPLAY "TOP-2 " ��ԃL�[
028090     IF ��ԃL�[ = "00"
               MOVE SPACE TO �I���t���O
               PERFORM �Q�Ɠ��v�f�[�^�e�Ǎ�
021400         PERFORM UNTIL (�A���Q�O�P�|���҃R�[�h   NOT = �Q�Ƃg���|���҃R�[�h  ) OR
027940*                       (�A���Q�O�P�|�{�p�a��N�� NOT = �Q�Ƃg���|�{�p�a��N��) OR
                             (�A���Q�O�P�|�{�p�敪     NOT = �Q�Ƃg���|�{�p�敪    ) OR
                             (�I���t���O�S             NOT = SPACE)
HILO  *             DISPLAY �Q�Ƃg���|�{�p�a��N���� " " �Q�Ƃg���|���҃R�[�h
001107             IF �Q�Ƃg���|�˔����Ë敪 = 1 
                       MOVE �Q�Ƃg���|�{�p�敪       TO �g�����|�{�p�敪
002380                 MOVE �Q�Ƃg���|�{�p�a��N���� TO �g�����|�{�p�a��N����
002390                 MOVE �Q�Ƃg���|���҃R�[�h     TO �g�����|���҃R�[�h
                       READ �g���Î��тe
                       NOT INVALID KEY
                           IF �g�����|���Ë��z NOT = ZERO
                               MOVE �Q�Ƃg���|�{�p�a��N���� TO �v�Z�a��N�����v
014470                         PERFORM ���t��Βl�v�Z
HILO***                           DISPLAY "TOP-3 ��Βl�v " ��Βl�v 
                               COMPUTE ��Βl�v = ������Βl�v - ��Βl�v
HILO***                        DISPLAY "TOP-3-1 �� " �A���Q�O�P�|�{�p�a��N���� " " �Q�Ƃg���|�{�p�a��N���� " " ��Βl�v 
                               IF ��Βl�v <= 14
HILO***                               DISPLAY "TOP-7 �G���[�Ώ�"
                                   MOVE "YES" TO �A���Q�O�P�|�G���[�t���O �I���t���O�S
                               END-IF
                               MOVE "YES" TO �I���t���O�S
                           END-IF
                       END-READ
                   END-IF
                   PERFORM �Q�Ɠ��v�f�[�^�e�Ǎ�
               END-PERFORM
           END-IF.
      *     IF �A���Q�O�P�|�G���[�t���O NOT = SPACE
      *         DISPLAY "�˔��I�ȉ��Â��s�������̗�������N�Z����14���ȓ��͉��×����Z��ł��܂���B"
      *     END-IF.
007520************
007530*           *
007540* �I������  *
007550*           *
007560************
007570     PERFORM �I������.
007580     MOVE ZERO TO PROGRAM-STATUS.
007590     EXIT PROGRAM.
007600*
007610*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
007620*================================================================*
007630* ������ SECTION.
007640**
007840*================================================================*
007850 �t�@�C���I�[�v�� SECTION.
007860*
008070     OPEN INPUT �Q�Ƃg���v�f�[�^�e.
008080         MOVE NC"�Q�Ƃg���v�f�[�^�e" TO �t�@�C����.
008090         PERFORM �I�[�v���`�F�b�N.
008310*
018000     OPEN INPUT �����}�X�^.
018010         MOVE NC"����" TO �t�@�C����.
018020         PERFORM �I�[�v���`�F�b�N.
008310*
008280     OPEN INPUT �g���Î��тe.
008290         MOVE NC"�g���Î��тe" TO �t�@�C����.
008300         PERFORM �I�[�v���`�F�b�N.
008350*================================================================*
008360 �I�[�v���`�F�b�N SECTION.
008370*
008380     IF ��ԃL�[  NOT =  "00"
008390         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
008400         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
008410         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
008420                                                 UPON CONS
008430*-----------------------------------------*
008440         CALL "actcshm"  WITH C LINKAGE
008450*-----------------------------------------*
008460         ACCEPT  �L�[���� FROM CONS
008470         PERFORM �t�@�C����
008480         MOVE 99 TO PROGRAM-STATUS
008490         EXIT PROGRAM.
008500*
008510*================================================================*
008520 �t�@�C���� SECTION.
008530*
           CLOSE �Q�Ƃg���v�f�[�^�e �����}�X�^ �g���Î��тe.
008580*================================================================*
008590 �I������ SECTION.
008600*
008610     PERFORM �t�@�C����.
008620*================================================================*
028370 �G���[�\�� SECTION.
028380*
028390     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
028400     DISPLAY NC"��ԃL�[" ��ԃL�[                 UPON CONS.
028410     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
028420     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"  UPON CONS.
028430*-----------------------------------------*
028440     CALL "actcshm"  WITH C LINKAGE.
028450*-----------------------------------------*
028460     ACCEPT  �L�[���� FROM CONS.
028470*
028480*================================================================*
007550 ����N�擾 SECTION.
007560*
007570     MOVE �v�Z�a��v TO ���|�����敪.
007580     READ �����}�X�^
007590     INVALID KEY
007600         DISPLAY NC"�w��a��o�^����Ă��܂���" UPON CONS
007610         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
007620                                                  UPON CONS
007630         ACCEPT  �L�[���� FROM CONS
007640         PERFORM �I������
007650         EXIT PROGRAM
007660     NOT INVALID KEY
007670         COMPUTE ����N�v = ���|�J�n����N + �v�Z�N�v - 1
007680     END-READ.
007690*
007700*================================================================*
015550 ���t��Βl�v�Z SECTION.
015560*
015570** 1989�N����Ƃ��A���N�܂ŉ[�N�����񂠂邩���v�Z
015580** �����}�X�^�̐���J�n�N�m�F�B
015590*     COMPUTE ����N���v   = ����N�v - 1989 - 1.
015600*     DIVIDE 4 INTO ����N���v GIVING �[�N�񐔂v
015610*                            REMAINDER ��]�v
015620*     END-DIVIDE.
014460     PERFORM ����N�擾.
015630*/��N1989�N���瓖�N�܂ł̉[�N�̉񐔂��v�Z(���N�͊܂܂Ȃ�)
015640*/�n�߂̂��邤�N��1992�N���琔����
015650     COMPUTE ����N���v   = ����N�v - 1989 - 1.
015660     MOVE 1992 TO ���Z�N�v.
015670     MOVE ZERO TO �[�N�񐔂v.
015680     PERFORM UNTIL ����N�v <= ���Z�N�v
015690         COMPUTE ���Z�N�v   = ���Z�N�v   + 4
015700         COMPUTE �[�N�񐔂v = �[�N�񐔂v + 1
015710     END-PERFORM.
015720*
015730     COMPUTE �ʏ�N�񐔂v     = ����N���v     - �[�N�񐔂v.
015740     COMPUTE �ʏ�N��Βl�v   = �ʏ�N�񐔂v   * 365.
015750     COMPUTE �[�N��Βl�v     = �[�N�񐔂v     * 366.
015760     COMPUTE �N��Βl�v       = �ʏ�N��Βl�v + �[�N��Βl�v.
015770* ���N�����邤�N���ǂ����𒲂ׂ�i�Q���̖����v�Z�j
015780*     COMPUTE ����N���Q�v     = ����N�v + 1.
015790*     DIVIDE 4 INTO ����N���Q�v GIVING �[�N�񐔂v
015800*                                REMAINDER ��]�v
015810*     END-DIVIDE.
015820     DIVIDE 4 INTO ����N�v GIVING �[�N�񐔂v
015830                            REMAINDER ��]�v
015840     END-DIVIDE.
015850*
015860     MOVE ZERO TO ��Βl���v.
015870*
015880* �O���܂ł̐�Βl��݌v
015890     COMPUTE ��Βl���v�Q = �v�Z���v * 30.
015900*
015910     EVALUATE �v�Z���v
015920     WHEN 3
015930         COMPUTE ��Βl���v   = ��Βl���v�Q - 1
015940     WHEN 2
015950     WHEN 6
015960     WHEN 7
015970         COMPUTE ��Βl���v   = ��Βl���v�Q + 1
015980     WHEN 8
015990         COMPUTE ��Βl���v   = ��Βl���v�Q + 2
016000     WHEN 9
016010     WHEN 10
016020         COMPUTE ��Βl���v   = ��Βl���v�Q + 3
016030     WHEN 11
016040     WHEN 12
016050         COMPUTE ��Βl���v   = ��Βl���v�Q + 4
016060     WHEN OTHER
016070         MOVE ��Βl���v�Q TO ��Βl���v
016080     END-EVALUATE.
016090*
016100     IF ( ��]�v = ZERO ) AND
016110        ( �v�Z���v > 2  )
016120         COMPUTE ��Βl���v = ��Βl���v + 1
016130     END-IF.
016140*
016150     COMPUTE ��Βl�v   = �N��Βl�v + ��Βl���v + �v�Z���v.
016160*
016170*================================================================*
013670 �Q�Ɠ��v�f�[�^�e�Ǎ� SECTION.
013680*
013710     READ �Q�Ƃg���v�f�[�^�e NEXT
013720     AT END
013730         MOVE "YES" TO �I���t���O�S
013740     END-READ.
013750*
013760*================================================================*
028510******************************************************************
028520 END PROGRAM TOPPATU.
028530******************************************************************
