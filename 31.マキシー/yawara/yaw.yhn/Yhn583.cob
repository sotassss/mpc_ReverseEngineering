000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN583.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*       �����p       �\�����f�[�^���t�� �y����z
000100*       MED = YHN580 YHN583P 
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2014-09-04
000130 DATE-COMPILED.          2014-09-04
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
000610     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
000620                             ORGANIZATION             IS  INDEXED
000630                             ACCESS MODE              IS  DYNAMIC
000640                             RECORD KEY               IS  ���|�����敪
000650                             FILE STATUS              IS  ��ԃL�[
000660                             LOCK        MODE         IS  AUTOMATIC.
000730     SELECT  �{�p�����}�X�^ ASSIGN     TO        SEJOHOL
000740                             ORGANIZATION             IS  INDEXED
000750                             ACCESS MODE              IS  DYNAMIC
000760                             RECORD KEY               IS  �{��|�{�p���ԍ�
000760                             FILE STATUS              IS  ��ԃL�[
000770                             LOCK        MODE         IS  AUTOMATIC.
000420     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS ��|�{�p�a��N��
000460                                                          ��|���҃R�[�h
000470                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000480                                                          ��|���҃J�i
000490                                                          ��|���҃R�[�h
000500                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000510                                                         ��|�{�p�a��N��
000520                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000530                                                          ��|�ی����
000540                                                          ��|�ی��Ҕԍ�
000550                                                          ��|���҃R�[�h
000560                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000570                                                          ��|������
000580                                                     ��|��p���S�Ҕԍ�
000590                                                          ��|���҃R�[�h
000600                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000610                                                          ��|�������
000620                                                  ��|��p���S�Ҕԍ�����
000630                                                          ��|���҃R�[�h
000640                             ALTERNATE RECORD KEY  IS ��|�����a��N��
000650                                                      ��|�{�p�a��N��
000660                                                      ��|���҃R�[�h
000670                             FILE STATUS              IS  ��ԃL�[
000680                             LOCK        MODE         IS  AUTOMATIC.
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
000780     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF001
000790                             SYMBOLIC    DESTINATION  IS "PRT"
000800                             FORMAT                   IS  ��`�̖��o
000810                             GROUP                    IS  ���ڌQ���o
000820                             PROCESSING  MODE         IS  ������ʂo
000830                             UNIT        CONTROL      IS  �g������o
000840                             FILE        STATUS       IS  �ʒm���o.
000850******************************************************************
000860*                      DATA DIVISION                             *
000870******************************************************************
000880 DATA                    DIVISION.
000890 FILE                    SECTION.
000950*                           �m�q�k��  �P�Q�W�n
000960 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000970     COPY GENGOU         OF  XFDLIB  JOINING    ��   AS  PREFIX.
001010*                           �m�q�k��  �U�S�O�n
001020 FD  �{�p�����}�X�^    BLOCK   CONTAINS   1   RECORDS.
001030     COPY SEJOHO         OF  XFDLIB  JOINING    �{�� AS  PREFIX.
001120*                           �m�q�k��  �R�Q�O�n
001130 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
001140     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
      *
       FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   ���Z  AS  PREFIX.
001500*
001040 FD  ����t�@�C��.
001050     COPY YHN583P        OF  XMDLIB.
001060******************************************************************
001070*                WORKING-STORAGE SECTION                         *
001080******************************************************************
001090 WORKING-STORAGE         SECTION.
001640 01 �L�[����                           PIC X    VALUE SPACE.
001650 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
001160 01 �t�@�C�����v                       PIC N(2) VALUE SPACE.
001180 01 ���s�L�[�v                         PIC X(4) VALUE SPACE.
001660 01 �I���t���O                         PIC X(3) VALUE SPACE.
001210 01 �I�[�v���t���O                     PIC X(3) VALUE SPACE.
001560*
001830 01 �����a��N���v�q.
001840    03 �����a��v�q                    PIC 9    VALUE ZERO.
001850    03 �����N�v�q                      PIC 9(2) VALUE ZERO.
001860    03 �������v�q                      PIC 9(2) VALUE ZERO.
001320 01 �������̂v                         PIC N(2) VALUE SPACE.
       01 �V�X�e�����t�v.
          03 ����v                          PIC X(2) VALUE SPACE.
          03 �N�����v                        PIC X(6) VALUE SPACE.
       01 ���Ԃv�o.
          03 �����b�v                        PIC X(6) VALUE SPACE.
          03 �b�ȉ��v                        PIC X(2) VALUE SPACE.
001800** ���v�p
       01 �W�v�v.
          03 �����v                          PIC 9(4) VALUE ZERO.
          03 �������z�v                      PIC 9(9) VALUE ZERO.
002433* �G���[���b�Z�[�W�p
002434 01 �G���[���b�Z�[�W�v.
002435    03 �G���[���҃R�[�h�v              PIC X(7)  VALUE SPACE.
002436    03 �G���[��؂�v                  PIC X(1)  VALUE SPACE.
002437    03 �G���[�ی���ʂv                PIC X(2)  VALUE SPACE.
002438    03 FILLER                          PIC X(10) VALUE SPACE.
001530*
001540 01 �������.
001550     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
001560     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
001570     03 ������ʂo                     PIC X(2) VALUE SPACE.
001580     03 �g������o.
001590         05 �[������o.
001600             07 �ړ������o             PIC X(1).
001610             07 �ړ��s���o             PIC 9(3).
001620         05 �ڍא���o                 PIC X(2).
001630     03 �ʒm���o                     PIC X(2) VALUE SPACE.
001640     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
001650 01 �v�Z�@����N�v                     PIC 9(2).
001660* ���t�v�n�q�j
001670 01 �a��I���N�v                       PIC 9(4).
001680 01 �v�Z�@�a��N�v                     PIC 9(2).
001690 01 �v�Z�@����.
001700    03 �v�Z�@����N                    PIC 9(4).
001710    03 �v�Z�@�����                  PIC 9(4).
001720 01 �v�Z�@����q REDEFINES �v�Z�@����.
001730    03 �v�Z�@���I                      PIC 9(2).
001740    03 �v�Z�@���t                      PIC 9(6).
001750    03 �v�Z�@���t�q REDEFINES �v�Z�@���t.
001760       05 �v�Z�@�N��                   PIC 9(4).
001770       05 �v�Z�@�N���q REDEFINES �v�Z�@�N��.
001780         07 �v�Z�@�N                   PIC 9(2).
001790         07 �v�Z�@��                   PIC 9(2).
001800       05 �v�Z�@��                     PIC 9(2).
001810*
003340******************************************************************
003350*                          �A������                              *
003360******************************************************************
003370*
003370**********************
003370* ���b�Z�[�W�\���L�[ *
003370**********************
003370*
003370 01 �A���|�L�[ IS EXTERNAL.
003370    03  �A���|���b�Z�[�W                 PIC N(20).
003370*
002634 01 �A���R�|�L�[ IS EXTERNAL.
002635    03  �A���R�|���b�Z�[�W             PIC N(20).
002636    03  �A���R�|���b�Z�[�W�P           PIC X(20).
002636*
002230****************
002240* ��ʓ��͏�� *
002250****************
002260 01 �A���|��ʏ��x�g�m�T�W�O IS EXTERNAL.
002270    03 �A���|�����a��N��.
002280       05 �A���|�����a��               PIC 9.
002290       05 �A���|�����N��.
002300         07 �A���|�����N               PIC 9(2).
002310         07 �A���|������               PIC 9(2).
          03 �A���|�v���r���[�敪             PIC 9.
002231*
000540************************************
000550* �v�����^�t�@�C���쐬�p           *
000560************************************
000570 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
000580     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
000590     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
000600     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
000610     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
003890******************************************************************
003900*                      PROCEDURE  DIVISION                       *
003910******************************************************************
003920 PROCEDURE               DIVISION.
003930************
003940*           *
003950* ��������   *
003960*           *
003970************
002560     MOVE SPACE TO �I�[�v���t���O.
002570     PERFORM �v�����^�t�@�C���쐬.
003980     PERFORM ������.
001790************
001800*           *
001810* �又��     *
001820*           *
001830************
004550     PERFORM �W�v����.
004550     PERFORM �������.
004620************
004630*           *
004640* �I������   *
004650*           *
004660************
004670     PERFORM �I������.
004680     EXIT PROGRAM.
004690*
004700*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
      *   �󎚒����Ȃ�
002971     MOVE "PRTF001"             TO �g�A�o�q�s�e�|�t�@�C����.
002972*
002973*   �g�p���钠�[�v���O�������Z�b�g
002974     MOVE "YHN583"              TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
004710*================================================================*
004720 ������ SECTION.
004730*
004770     OPEN INPUT �����}�X�^.
004780             MOVE NC"����" TO �t�@�C�����v.
004790             PERFORM �I�[�v���`�F�b�N.
004830     OPEN INPUT �{�p�����}�X�^.
004840             MOVE NC"�{��" TO �t�@�C�����v.
004850             PERFORM �I�[�v���`�F�b�N.
003070     OPEN INPUT ��f�ҏ��e.
003080         MOVE NC"��f�ҏ��e" TO �t�@�C�����v.
003090         PERFORM �I�[�v���`�F�b�N.
003250     OPEN INPUT ���Z�v�g�e.
003260         MOVE NC"���Z�v�g�e" TO �t�@�C�����v.
003270         PERFORM �I�[�v���`�F�b�N.
002960* �A�����ڂ̑Ҕ�
002970     MOVE �A���|�����a��      TO �����a��v�q.
002980     MOVE �A���|�����N        TO �����N�v�q.
002990     MOVE �A���|������        TO �������v�q.
004900*
005080*================================================================*
005090 �I�[�v���`�F�b�N SECTION.
005100*
005110     IF ��ԃL�[  NOT =  "00"
005120         DISPLAY �t�@�C�����v NC"�e�I�[�v���G���[" UPON CONS
005130         DISPLAY NC"��ԃL�[�F" ��ԃL�[           UPON CONS
005140         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005150                                                   UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
005160         ACCEPT  �L�[���� FROM CONS
005170         PERFORM �t�@�C����
005180         EXIT PROGRAM.
006150*================================================================*
006770 �t�@�C���� SECTION.
006780*
003570     IF ( �I�[�v���t���O = "YES" )
003580         CLOSE ����t�@�C��
003630     END-IF.
003640*
006790     CLOSE  �����}�X�^   �{�p�����}�X�^.
006810*================================================================*
006820 �I������ SECTION.
006830*
006840     PERFORM �t�@�C����.
003440*================================================================*
003450 �W�v���� SECTION.
003460*
005170     MOVE �����a��v�q  TO ���Z�|�����a��.
005180     MOVE �����N�v�q    TO ���Z�|�����N.  
005190     MOVE �������v�q    TO ���Z�|������.  
013730     MOVE ZERO          TO ���Z�|���Z���.
005200     MOVE ZERO          TO ���Z�|�{�p�a��.
005210     MOVE ZERO          TO ���Z�|�{�p�N.  
005220     MOVE ZERO          TO ���Z�|�{�p��.  
005230     MOVE ZERO          TO ���Z�|���Ҕԍ�.
005240     MOVE SPACE         TO ���Z�|�}��.    
013770     START ���Z�v�g�e   KEY IS >= ���Z�|�����a��N��
000230                                  ���Z�|�{�p�a��N��
000240                                  ���Z�|���҃R�[�h
000250                                  ���Z�|���Z���
003620     END-START.
003630     IF ��ԃL�[ = "00"
003640         MOVE SPACE  TO �I���t���O
003650         PERFORM ���Z�v�g�e�Ǎ�
003660         PERFORM UNTIL ( �I���t���O = "YES" ) OR
005330                       ( ���Z�|�����a�� NOT = �����a��v�q ) OR
005340                       ( ���Z�|�����N   NOT = �����N�v�q   ) OR
005350                       ( ���Z�|������   NOT = �������v�q   )
003736             PERFORM �f�[�^�`�F�b�N
003730*
003731** �J�ЁE�����ӁE���R�E���ےP�Ƃ͑ΏۊO
003790             IF  ���Z�|���Z��� = 4 OR 5 OR 6 OR 7 OR 8
003733                CONTINUE
003734             ELSE
003737                IF  ���s�L�[�v = "YES"
003738                    PERFORM ���v����
003739                END-IF
003741             END-IF
003850             PERFORM ���Z�v�g�e�Ǎ�
003860         END-PERFORM
003870     END-IF.
003980*
004046*================================================================*
004050 �f�[�^�`�F�b�N SECTION.
004060*
004070     MOVE SPACE  TO ���s�L�[�v.
005300     IF ( ���Z�|���Z����Ώۋ敪 NOT = 1 )
005310        MOVE "YES"  TO ���s�L�[�v
005320     END-IF.
019520* *****************************************************************
019530* * ���Z�v�g�e�̐����Ώۋ敪 = 0 �̏ꍇ�f�[�^�쐬�ΏۂƂ��Ȃ� *
019540* *****************************************************************
005360     IF ���s�L�[�v  = "YES"
005370*      (�ēx�A���s�L�[�v SPACE)
005380        MOVE SPACE  TO ���s�L�[�v
019640        IF ���Z�|�����Ώۋ敪 NOT = ZERO
004090           MOVE ���Z�|�{�p�a��  TO ��|�{�p�a��
004100           MOVE ���Z�|�{�p�N    TO ��|�{�p�N
004110           MOVE ���Z�|�{�p��    TO ��|�{�p��
004120           MOVE ���Z�|���Ҕԍ�  TO ��|���Ҕԍ�
004130           MOVE ���Z�|�}��      TO ��|�}��
                 READ ��f�ҏ��e
                 NOT INVALID KEY
019880              MOVE "YES"  TO ���s�L�[�v
                 END-READ
019900        ELSE
019910           MOVE SPACE  TO ���s�L�[�v
              END-IF
019950     END-IF.
004530*
004630*================================================================*
004640 ���Z�v�g�e�Ǎ� SECTION.
004650*
004660     READ ���Z�v�g�e NEXT
004670     AT END
004680         MOVE "YES" TO �I���t���O
004690     END-READ.
004700*
004780*================================================================*
004790 ���v���� SECTION.
004800*
           IF ���Z�|���Z��� = 3
               IF  ���Z�|�����������z NOT = ZERO
                   COMPUTE �������z�v = �������z�v + ���Z�|�����������z
                   COMPUTE �����v     = �����v     + 1
               END-IF
005524     ELSE
               COMPUTE �������z�v = �������z�v + ���Z�|�������z
               COMPUTE �����v     = �����v     + 1
005541*
005545     END-IF.
004854*
006840*================================================================*
010770 ������� SECTION.
010780*
010780     MOVE SPACE TO YHN583P.
010780     INITIALIZE YHN583P.
      *
           IF �����v = ZERO
               MOVE NC"�@�@�f�[�^������܂���B" TO �A���|���b�Z�[�W
006020         CALL   "MSG001"
006030         CANCEL "MSG001"
           ELSE
               ACCEPT �N�����v FROM DATE
      *    /* 1980�`2079�N�̊ԂŐݒ� */
               IF �N�����v(1:2) > 80
                   MOVE 19 TO ����v
               ELSE
                   MOVE 20 TO ����v
               END-IF
               ACCEPT ���Ԃv�o FROM TIME
      *
               STRING �V�X�e�����t�v(1:4)    DELIMITED BY SIZE
                      "/"                    DELIMITED BY SIZE
                      �V�X�e�����t�v(5:2)    DELIMITED BY SIZE
                      "/"                    DELIMITED BY SIZE
                      �V�X�e�����t�v(7:2)    DELIMITED BY SIZE
                      " "                    DELIMITED BY SIZE
                      ���Ԃv�o(1:2)          DELIMITED BY SIZE
                      ":"                    DELIMITED BY SIZE
                      ���Ԃv�o(3:2)          DELIMITED BY SIZE
                      ":"                    DELIMITED BY SIZE
                      ���Ԃv�o(5:2)          DELIMITED BY SIZE
                 INTO ������t
               END-STRING
      *
027710         MOVE ZERO  TO �{��|�{�p���ԍ�
027720         READ �{�p�����}�X�^
027730         INVALID KEY
027740             CONTINUE
027750         NOT INVALID KEY
022640             MOVE �{��|�X�֔ԍ��P        TO �X�֔ԍ��P
022650             MOVE �{��|�X�֔ԍ��Q        TO �X�֔ԍ��Q
022700             MOVE �{��|�Z���P            TO �Z���P
022710             MOVE �{��|�Z���Q            TO �Z���Q
022730             MOVE �{��|�d�b�ԍ�          TO �d�b�ԍ�
027680             MOVE �{��|�ڍ��t�����ԍ�  TO ����ԍ�
027940             MOVE �{��|��\�Җ�          TO ��\�Җ�
022680             MOVE �{��|�ڍ��@��          TO �{�p���� �{�p�����Q
                   STRING �{��|��\�Җ�    DELIMITED BY SPACE
                          "�@�l"            DELIMITED BY SIZE
                     INTO ��\�Җ��Q
                   END-STRING
               END-READ
      */�����C��/������20190514
               MOVE �A���|�����a��     TO ���|�����敪
037380         READ �����}�X�^
037390         NOT INVALID KEY
037400             MOVE ���|��������   TO ��o�a�� ��o�a��Q ��o�a��R
037410         END-READ
      */�����C��/������20190514
      *         MOVE �A���|�����N       TO ��o�N
      *         MOVE �A���|������       TO ��o��
               MOVE �����v             TO ��o����
               MOVE �������z�v         TO ���v���z
      *
               PERFORM ���׈������
           END-IF.
010780*
011820*================================================================*
011820 ���׈������ SECTION.
011790*
006050     IF ( �I�[�v���t���O NOT = "YES" )
006060        MOVE "YES" TO �I�[�v���t���O
006070        OPEN I-O  ����t�@�C��
006080        PERFORM �G���[�����o
006090     END-IF.
011800     MOVE "YHN583P"  TO  ��`�̖��o.
011810     MOVE "GRP001"   TO  ���ڌQ���o.
011820     WRITE YHN583P.
011820     PERFORM �G���[�����o.
014110*================================================================*
014110 �G���[�����o SECTION.
014110*
014110     IF �ʒm���o NOT = "00"
014110         DISPLAY NC"���[�G���["              UPON CONS
014110         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
014110         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
014110         DISPLAY NC"�g������o�F" �g������o UPON CONS
014110         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
014110                                             UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
014110         ACCEPT  �L�[���� FROM CONS
014110         PERFORM �t�@�C����
014110         MOVE 99 TO PROGRAM-STATUS
014110         EXIT PROGRAM
014110     END-IF.
001370*================================================================*
014300******************************************************************
014310 END PROGRAM YHN583.
014320******************************************************************
