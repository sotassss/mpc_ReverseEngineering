000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YIW722.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*         �{�p���ׁi�_+����޳�ޔŁj
000100*         MED = YIW720 YIW722P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2015-09-16
000130 DATE-COMPILED.          2015-09-16
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
000380     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS ��|�{�p�a��N��
000420                                                          ��|���҃R�[�h
000430                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000440                                                          ��|���҃J�i
000450                                                          ��|���҃R�[�h
000460                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000470                                                         ��|�{�p�a��N��
000480                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000490                                                          ��|�ی����
000500                                                          ��|�ی��Ҕԍ�
000510                                                          ��|���҃R�[�h
000520                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000530                                                          ��|������
000540                                                     ��|��p���S�Ҕԍ�
000550                                                          ��|���҃R�[�h
000560                             ALTERNATE RECORD KEY     IS ��|�{�p�a��N��
000570                                                          ��|�������
000580                                                  ��|��p���S�Ҕԍ�����
000590                                                          ��|���҃R�[�h
000600                             ALTERNATE RECORD KEY  IS ��|�����a��N��
000610                                                      ��|�{�p�a��N��
000620                                                      ��|���҃R�[�h
000630                             FILE STATUS              IS  ��ԃL�[
000640                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  ��ƃt�@�C���P  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7211L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
                                   RECORD KEY               IS  ��P�|�{�p�a��N����
000620                             FILE        STATUS       IS  ��ԃL�[
000630                             LOCK        MODE         IS  AUTOMATIC.
000720     SELECT  ����t�@�C��    ASSIGN      TO     GS-PRTF002
000730                             SYMBOLIC    DESTINATION  IS "PRT"
000740                             FORMAT                   IS  ��`�̖��o
000750                             GROUP                    IS  ���ڌQ���o
000760                             PROCESSING  MODE         IS  ������ʂo
000770                             UNIT        CONTROL      IS  �g������o
000780                             FILE        STATUS       IS  �ʒm���o.
000790******************************************************************
000800*                      DATA DIVISION                             *
000810******************************************************************
000820 DATA                    DIVISION.
000830 FILE                    SECTION.
000840*                           �m�q�k��  �P�Q�W�n
000850 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
000860     COPY GENGOU          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000870*                           �m�q�k��  �Q�T�U�n
000880 FD  ������}�X�^          BLOCK   CONTAINS   1   RECORDS.
000890     COPY SEIGYO          OF  XFDLIB  JOINING   ��   AS  PREFIX.
000910*                           �m�q�k��  �R�Q�O�n
000920 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
000930     COPY JUSINJ          OF  XFDLIB  JOINING   ��   AS  PREFIX.
001310*****************
001320* ��ƃt�@�C���P *
001330*****************
001340*                         �m�q�k��  �P�U�O�n
001350 FD  ��ƃt�@�C���P RECORD  CONTAINS 160 CHARACTERS.
001360 01 ��P�|���R�[�h.
001370    03 ��P�|���R�[�h�L�[.
001535       05 ��P�|�{�p�a��N����.
001536          07 ��P�|�{�p�a��                PIC 9.
001537          07 ��P�|�{�p�N��.
001538             09 ��P�|�{�p�N               PIC 9(2).
001539             09 ��P�|�{�p��               PIC 9(2).
001540          07 ��P�|�{�p��                  PIC 9(2).
001490    03 ��P�|���R�[�h�f�[�^.
             05 ��P�|����.
001550          07 ��P�|��������.
                   09 ��P�|������               PIC 9(2).
                   09 ��P�|������               PIC 9(2).
001550          07 ��P�|������                  PIC 9(5).
001550          07 ��P�|�{�×�                  PIC 9(5).
001550          07 ��P�|�Č���                  PIC 9(5).
001550          07 ��P�|��×�                  PIC 9(5).
001550          07 ��P�|���×�                  PIC 9(5).
001550          07 ��P�|㪖@��                  PIC 9(5).
001550          07 ��P�|�d�×�                  PIC 9(5).
001551          07 ��P�|��p�z                  PIC 9(5).
001551          07 ��P�|�ꕔ���S��              PIC 9(5).
001551       05 ��P�|�R�����g                   PIC X(100).
001470       05 ��P�|����                       PIC 9(1).
001500       05 FILLER                           PIC X(3).
000920*
001160 FD  ����t�@�C��.
001170     COPY YIW722P        OF  XMDLIB.
001180*----------------------------------------------------------------*
001190******************************************************************
001200*                WORKING-STORAGE SECTION                         *
001210******************************************************************
001220 WORKING-STORAGE         SECTION.
001230 01 �L�[����                           PIC X     VALUE SPACE.
001240 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
001250 01 �I���t���O                         PIC X(3)  VALUE SPACE.
001260 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
001270 01 �����t���O                         PIC X(3)  VALUE SPACE.
001280 01 �t�@�C����                         PIC N(6)  VALUE SPACE.
001290 01 ���Z�v�g�o�f�v                     PIC X(8)  VALUE SPACE.
001300 01 �O�a��v                           PIC 9     VALUE ZERO.
001310 01 �J�����g�����v                     PIC 9(1)  VALUE ZERO.
001460 01 ����ʒu�b�m�s                     PIC 9(2)  VALUE ZERO.
001330 01 �J�E���^                           PIC 9(2)  VALUE ZERO.
001330 01 �J�E���^�P                         PIC 9(2)  VALUE ZERO.
001340 01 ���Ҕԍ��v                         PIC 9(6)  VALUE ZERO.
001240 01 �s�J�E���^                         PIC 9(2) VALUE 0.
001580 01 �I�[�v���t���O                     PIC X(3) VALUE SPACE.
001350**
001360 01 �x���t���O                         PIC X(3) VALUE SPACE.
001370 01 �x���񐔂v                         PIC 9(4) VALUE ZERO.
001380 01 �x���b�m�s                         PIC 9(5) VALUE ZERO.
001390**
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
      *
       01 ���̑��v.
001380    03 ���k���v                        PIC ZZZZ.
001390    03 ���ח��v                        PIC ZZZ.
001410****************
001420* �A�����ڑҔ� *
001430****************
001440*    ************
001450*    * ����L�[ *
001460*    ************
001470 01 �Ώۃf�[�^�v�q.
001480    03 �{�p�a��N���v�q.
001490       05 �{�p�a��v�q                  PIC 9(1)  VALUE ZERO.
001500       05 �{�p�N�v�q                    PIC 9(2)  VALUE ZERO.
001510       05 �{�p���v�q                    PIC 9(2)  VALUE ZERO.
001520    03 �J�n���v�q                       PIC 9(2)  VALUE ZERO.
001530    03 �I�����v�q                       PIC 9(2)  VALUE ZERO.
001550    03 ���҃R�[�h�v.
001560       05 ���Ҕԍ��v                    PIC 9(6)  VALUE ZERO.
001570       05 �}�Ԃv                        PIC X(1)  VALUE SPACE.
001580    03 ������[�h�e�v�q                 PIC 9(1)  VALUE ZERO.
       01 �R�����g�v.
          03 �R�����g�P�v                       PIC X(50) VALUE SPACE.
          03 �R�����g�Q�v                       PIC X(50) VALUE SPACE.
001831* POWER COBOL�p
001832 01 dll-name  PIC X(260)  VALUE SPACE.
001833 01 form-name PIC X(14)   VALUE SPACE.
002210*******************************************************************
002220 01 �������.
002230     03 ��`�̖��o                     PIC X(8) VALUE SPACE.
002240     03 ���ڌQ���o                     PIC X(8) VALUE SPACE.
002250     03 ������ʂo                     PIC X(2) VALUE SPACE.
002260     03 �g������o.
002270         05 �[������o.
002280             07 �ړ������o             PIC X(1) VALUE SPACE.
002290             07 �ړ��s���o             PIC 9(3) VALUE ZERO.
002300         05 �ڍא���o                 PIC X(2) VALUE SPACE.
002310     03 �ʒm���o                     PIC X(2) VALUE SPACE.
002320     03 ���j�b�g���o                   PIC X(8) VALUE SPACE.
002330*
002500******************************************************************
002510*                          �A������                              *
002520******************************************************************
002510**********************
002520* ���b�Z�[�W�\���L�[ *
002530**********************
002540 01 �A���|�L�[ IS EXTERNAL.
002550    03  �A���|���b�Z�[�W                 PIC N(20).
002560*
002870************
002880* ����L�[ *
002890************
003110 01 �A���|���̓f�[�^�x�h�v�V�Q�O IS EXTERNAL.
          03 �A���|�{�p�a��N��.
             05 �A���|�{�p�a��                  PIC 9(1).
             05 �A���|�{�p�N                    PIC 9(2).
             05 �A���|�{�p��                    PIC 9(2).
          03 �A���|�J�n���t.
             05 �A���|�J�n��                    PIC 9(2).
          03 �A���|�I�����t.
             05 �A���|�I����                    PIC 9(2).
          03 �A���|���҃R�[�h.
             05 �A���|���Ҕԍ�                  PIC 9(6).
             05 �A���|�}��                      PIC X(1).
          03 �A���|������[�h�e                 PIC 9(1).
          03 �A���|�������[�h                   PIC 9(1).
          03 �A���|�N�����[�h                   PIC 9(1).
          03 �A���|�����ƃ��[�h                 PIC 9(1).
          03 �A���|�R�����g���[�h               PIC 9(1).
          03 �A���|���v���[�h                   PIC 9(1).
          03 �A���|����i��                     PIC 9(2).
      */�A�����̎�������̗L��0302
          03 �A���|�������[�h                   PIC X(4).
      *
002830 01 �A��|���v�f�[�^�x�h�v�V�Q�O IS EXTERNAL.
          03 �A��|���Ҏ���                     PIC X(50).
          03 �A��|������                       PIC 9(6).
          03 �A��|��×�                       PIC 9(6).
          03 �A��|���×�                       PIC 9(6).
          03 �A��|㪖@��                       PIC 9(6).
          03 �A��|�d�×�                       PIC 9(6).
          03 �A��|��p�z                       PIC 9(7).
          03 �A��|���S�z                       PIC 9(7).
          03 �A��|���񋟗�                   PIC 9(6).
          03 �A��|��                           PIC N(1).
          03 �A��|��                           PIC N(1).
          03 �A��|��                           PIC N(1).
          03 �A��|�������q��                   PIC 9(6).
          03 �A��|���̑�                       PIC 9(6).
          03 �A��|���k��                       PIC 9(4).
          03 �A��|���ח�                       PIC 9(3).
          03 �A��|�R�����g.
             05 �A��|�R�����g�P                PIC X(100).
             05 �A��|�R�����g�Q                PIC X(100).
003020*
       01 �A���|�\���t���O�x�h�v�V�Q�O IS EXTERNAL GLOBAL.
          03 �A���|�v���r���[�敪               PIC 9(1).
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
007308*
       01 �A���b�Z�[�W�o���O�O�T�Q IS EXTERNAL.
          03 �A���o�|���b�Z�[�W�ԍ�                PIC 9(2).
          03 �A���o�|���b�Z�[�W.
             05 �A���o�|���b�Z�[�W���e             PIC X(40) OCCURS 6.
          03 �A���o�|���b�Z�[�W�P                  PIC X(20).
          03 �A���o�|���b�Z�[�W�Q                  PIC X(12).
          03 �A���o�|�Ԃ�l                        PIC X.
003090******************************************************************
003100*                      PROCEDURE  DIVISION                       *
003110******************************************************************
003120 PROCEDURE               DIVISION.
003130************
003140*           *
003150* ��������   *
003160*           *
003170************
002570     PERFORM �v�����^�t�@�C���쐬.
003180     PERFORM ������.
003190     PERFORM ������擾.
003200************
003210*           *
003220* �又��     *
003230*           *
003240************
003250* ���
003290     PERFORM ����Z�b�g�P.
003350************
003360*           *
003370* �I������   *
003380*           *
003390************
003400     PERFORM �I������.
003410     PERFORM �x������.
003420     EXIT PROGRAM.
003430*
003440*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
           MOVE "KARUTE"              TO �g�A����o�q�s�e�|�p�����.
002970*   �g�p����v�����^�t�@�C�����Z�b�g
002231     MOVE "PRTF002"             TO �g�A�o�q�s�e�|�t�@�C����.
002972*
002973*   �g�p���钠�[�v���O�������Z�b�g
002974     MOVE "YIW722"             TO �g�A�o�q�s�e�|���[�v���O������.
002975*
002976*--����-----------------------------------------------------*
002980*
002990*   / �v���r���[�敪�Z�b�g /
003000     MOVE �A���|�v���r���[�敪 TO �g�A�o�q�s�e�|�v���r���[�敪.
003000*     MOVE 1 TO �g�A�o�q�s�e�|�v���r���[�敪.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
003450*================================================================*
003460 ������ SECTION.
003470*
003480     PERFORM �t�@�C���I�[�v��.
003970*
003980*================================================================*
003990 �t�@�C���I�[�v�� SECTION.
004000*
004010     OPEN INPUT   �����}�X�^
004020         MOVE NC"����" TO �t�@�C����.
004030         PERFORM �I�[�v���`�F�b�N.
004040     OPEN INPUT   ������}�X�^
004050         MOVE NC"������" TO �t�@�C����.
004060         PERFORM �I�[�v���`�F�b�N.
003940     OPEN INPUT   ��f�ҏ��e.
003950         MOVE NC"��e" TO �t�@�C����.
003960         PERFORM �I�[�v���`�F�b�N.
004100     OPEN INPUT ��ƃt�@�C���P
004110         MOVE NC"��ƂP" TO �t�@�C����.
004120         PERFORM �I�[�v���`�F�b�N.
004130*     OPEN I-O   ����t�@�C��
004140*         PERFORM �G���[�����o.
004150*================================================================*
004160 �I�[�v���`�F�b�N SECTION.
004170*
004180     IF ��ԃL�[  NOT =  "00"
004190         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
004200         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
004210         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
004220                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
004230         ACCEPT  �L�[���� FROM CONS
004240         PERFORM �t�@�C����
004250         EXIT PROGRAM.
004260*================================================================*
004270 ������擾 SECTION.
004280*
004290     MOVE ZERO TO ���|����敪
004300     READ ������}�X�^
004310     NOT INVALID KEY
004320         MOVE ���|�x����       TO �x���񐔂v
004330     END-READ.
004340*
004350*================================================================*
004360 �x������ SECTION.
004370*
004380     PERFORM VARYING �x���b�m�s FROM 1 BY 1
004390                                UNTIL �x���b�m�s > �x���񐔂v
004400         MOVE SPACE TO �x���t���O
004410     END-PERFORM.
004420*
004590*================================================================*
004600 ����Z�b�g�P SECTION.
004610*
005220     IF �A���|�������[�h = 1
005230         MOVE �A��|���Ҏ���    TO ���Ҏ���
               MOVE �A���|���Ҕԍ�    TO ���Ҕԍ�
               MOVE �A���|�}��        TO �}��
           END-IF.
           MOVE 1                  TO �����t���O.
002940     MOVE SPACE TO �I���t���O.
005630     MOVE �A���|����i�� TO ����ʒu�b�m�s.
           MOVE �A���|�{�p�a��    TO ��P�|�{�p�a��.
           MOVE �A���|�{�p�N      TO ��P�|�{�p�N.
           MOVE �A���|�{�p��      TO ��P�|�{�p��.
           MOVE �A���|�J�n��      TO ��P�|�{�p��.
           START ��ƃt�@�C���P KEY IS >= ��P�|�{�p�a��N����
           END-START.
013500     IF ��ԃL�[  =  "00"
013520         PERFORM ��ƃt�@�C���P�Ǎ�
      *
003070         PERFORM UNTIL ( �I���t���O = "YES" ) OR
003090                       ( ��P�|�{�p�� > �A���|�I���� ) OR
                             ( ����ʒu�b�m�s > 31 )
                   IF ( ��P�|���� = 1 )
                       MOVE NC"�������ł��B�V�����J���e�ɕύX���܂����H" TO �A���o�|���b�Z�[�W
                       MOVE 1                  TO �A���o�|���b�Z�[�W�ԍ�
                       MOVE "PMSG0052.DLL"     TO dll-name
                       MOVE "PMSG0052"         TO form-name
                       CALL "POWEROPENSHEET" USING dll-name form-name
005870                 EVALUATE  �A���o�|�Ԃ�l
005871                 WHEN "Y"
                           IF ( �����t���O = 0 )
005810                        PERFORM ���׈��
005820                        PERFORM ���ŏ���
                              MOVE SPACE TO YIW722P
004640                        IF ( �A���|�������[�h = 1 )
      */             */�w�b�_���Z�b�g���Ĉ������B
005230                           MOVE �A��|���Ҏ���    TO ���Ҏ���
                                 MOVE �A���|���Ҕԍ�    TO ���Ҕԍ�
                                 MOVE �A���|�}��        TO �}��
004660                        END-IF
                           END-IF
005850                     MOVE 1             TO ����ʒu�b�m�s
005860                     MOVE ��P�|�{�p��  TO �A���|�J�n��
005871                 WHEN "N"
                           CONTINUE
005931                 WHEN OTHER
                           IF ( �����t���O = 0 )
005810                        PERFORM ���׈��
005820                        PERFORM ���ŏ���
                           END-IF
                           PERFORM ����s���ޔ�
005932                     PERFORM �I������
005933                     PERFORM �x������
005934                     EXIT PROGRAM
005935                 END-EVALUATE
                   END-IF
004640             IF (�A���|�����ƃ��[�h = 1) OR (�A���|�R�����g���[�h = 1) OR
                      (�A���|�N�����[�h = 1)
                       PERFORM ���׈������
                       COMPUTE ����ʒu�b�m�s = ����ʒu�b�m�s + 1
                   END-IF
      *
                   MOVE 0             TO �����t���O
003600             PERFORM ��ƃt�@�C���P�Ǎ�
003610         END-PERFORM
           END-IF.
           IF �A���|���v���[�h = 1
               PERFORM �����v�������
           END-IF.
      *
      *****     PERFORM �e�X�g���.
      *
           IF (����ʒu�b�m�s > 31) AND ( �I���t���O NOT = "YES" )
              MOVE NC"������y�[�W�𒴂��܂��B������܂����H" TO �A���o�|���b�Z�[�W
              MOVE 1                  TO �A���o�|���b�Z�[�W�ԍ�
              MOVE "PMSG0052.DLL"     TO dll-name
              MOVE "PMSG0052"         TO form-name
              CALL "POWEROPENSHEET" USING dll-name form-name
005870        EVALUATE  �A���o�|�Ԃ�l
005871        WHEN "Y"
                 PERFORM ���׈��
                 PERFORM ����s���ޔ�
              WHEN "N"
                 PERFORM �I������
                 PERFORM �x������
                 EXIT PROGRAM
              END-EVALUATE
           ELSE
              PERFORM ���׈��
              PERFORM ����s���ޔ�
           END-IF.
004900*================================================================*
004910 ���׈������ SECTION.
004920*
           IF �A���|�N�����[�h = 1
005510        MOVE ��P�|�{�p��     TO ��(����ʒu�b�m�s)
005520        MOVE ��P�|�{�p��     TO ��(����ʒu�b�m�s)
           END-IF.
004940********************
004950* �����f�[�^�Z�b�g *
004960********************
           IF �A���|�����ƃ��[�h = 1
              IF ����ʒu�b�m�s = 1
005010           MOVE ��P�|������  TO  ������
005010           MOVE ��P�|������  TO  ������
005010           MOVE ��P�|������  TO  ������
              END-IF
              IF ����ʒu�b�m�s = 2
005010           MOVE ��P�|�Č���  TO  �Č���
              END-IF
005010        MOVE ��P�|�{�×�     TO  �{�×�(����ʒu�b�m�s)
005010        MOVE ��P�|��×�     TO  ��×�(����ʒu�b�m�s)
005010        MOVE ��P�|���×�     TO  ���×�(����ʒu�b�m�s)
005010        MOVE ��P�|㪖@��     TO  㪖@��(����ʒu�b�m�s)
005010        MOVE ��P�|�d�×�     TO  �d�×�(����ʒu�b�m�s)
005010        MOVE ��P�|��p�z     TO  ��p�z(����ʒu�b�m�s)
005010        MOVE ��P�|�ꕔ���S�� TO  �ꕔ���S��(����ʒu�b�m�s)
           END-IF.
005550*
           IF �A���|�R�����g���[�h = 1
              MOVE ��P�|�R�����g   TO  �R�����g�v
              MOVE �R�����g�P�v     TO  �R�����g�P(����ʒu�b�m�s)
              MOVE �R�����g�Q�v     TO  �R�����g�Q(����ʒu�b�m�s)
              MOVE �A��|�R�����g�P TO  �R�����g�v
              MOVE �R�����g�P�v     TO  �����R�����g�P
              MOVE �R�����g�Q�v     TO  �����R�����g�Q
              MOVE �A��|�R�����g�Q TO  �R�����g�v
              MOVE �R�����g�P�v     TO  �����R�����g�R
              MOVE �R�����g�Q�v     TO  �����R�����g�S
           END-IF.
005300*
004900*================================================================*
004910 �����v������� SECTION.
004920*
           MOVE �A���|�{�p��        TO ���v��.
           MOVE �A��|������        TO ���������v.
           MOVE �A��|��×�        TO ��×����v.
           MOVE �A��|���×�        TO ���×����v.
           MOVE �A��|㪖@��        TO 㪖@�����v.
           MOVE �A��|�d�×�        TO �d�×����v.
           MOVE �A��|��p�z        TO ���v���z.
           MOVE �A��|���S�z        TO ���S���v.
           MOVE �A��|���񋟗�    TO ���񋟗�.
           MOVE �A��|��            TO ��`�F�b�N.
           MOVE �A��|��            TO ���`�F�b�N.
           MOVE �A��|��            TO ���`�F�b�N.
           MOVE �A��|�������q��    TO �������q��.
      *     MOVE �A��|���̑�        TO ���̑�.
           MOVE �A��|���k��        TO ���k���v.
           MOVE �A��|���ח�        TO ���ח��v.
           IF �A��|���k�� NOT = ZERO
              IF �A��|���ח� NOT = ZERO
                 STRING "���k "        DELIMITED BY SIZE
                        ���k���v       DELIMITED BY SIZE
                        "�~�@�A"       DELIMITED BY SIZE
                        "���� "        DELIMITED BY SIZE
                        ���ח��v       DELIMITED BY SIZE
                   INTO ���̑�����
                 END-STRING
              ELSE
                 STRING "���k "        DELIMITED BY SIZE
                        ���k���v       DELIMITED BY SIZE
                   INTO ���̑�����
                 END-STRING
              END-IF
           ELSE
              IF �A��|���ח� NOT = ZERO
                 MOVE �A��|���ח�     TO ���ח��v
                 STRING "���� "        DELIMITED BY SIZE
                        ���ח��v       DELIMITED BY SIZE
                   INTO ���̑�����
                 END-STRING
              END-IF
           END-IF.
007070*================================================================*
007080 ��ƃt�@�C���P�Ǎ� SECTION.
007090*
007100     READ ��ƃt�@�C���P NEXT
007110     AT END
007120         MOVE "YES" TO �I���t���O
007130     END-READ.
007140*
007150*================================================================*
007160 �w�b�_��� SECTION.
007170*
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
013440*
007180     MOVE "YIW722P"  TO  ��`�̖��o.
007190     MOVE "HEDDA"   TO  ���ڌQ���o.
003770     MOVE SPACE      TO  ������ʂo.
007200     WRITE YIW722P.
007210     PERFORM �G���[�����o.
007150*================================================================*
007160 ���׈�� SECTION.
007170*
004310     IF ( �I�[�v���t���O NOT = "YES" )
004320        MOVE "YES" TO �I�[�v���t���O
004330        OPEN I-O  ����t�@�C��
004340        PERFORM �G���[�����o
004350     END-IF.
013440*
007180     MOVE "YIW722P"  TO  ��`�̖��o.
007190     MOVE "HEDDA"   TO  ���ڌQ���o.
003770     MOVE SPACE      TO  ������ʂo.
007200     WRITE YIW722P.
007210     PERFORM �G���[�����o.
013440*
007180     MOVE "YIW722P"  TO  ��`�̖��o.
007190     MOVE "MEISAI"   TO  ���ڌQ���o.
003770     MOVE SPACE      TO  ������ʂo.
007200     WRITE YIW722P.
007210     PERFORM �G���[�����o.
003730*================================================================*
003740 ���ŏ���  SECTION.
003750*
003760     MOVE "YIW722P" TO  ��`�̖��o.
003770     MOVE "CT"      TO  ������ʂo.
003780     MOVE "PAGE"    TO  �g������o.
003790     MOVE SPACE     TO  ���ڌQ���o.
003800     WRITE YIW722P.
003810     PERFORM �G���[�����o.
003820     MOVE SPACE     TO  �g������o.
003821*
003822     CLOSE  ����t�@�C��.
004320     MOVE SPACE TO �I�[�v���t���O.
003823*     OPEN I-O   ����t�@�C��.
003824*     PERFORM �G���[�����o.
003825*
007610*================================================================*
007620 �G���[�\�� SECTION.
007630*
007640     DISPLAY �t�@�C����   NC"�t�@�C�������G���["   UPON CONS.
007650     DISPLAY NC"��ԃL�[�F" ��ԃL�[               UPON CONS.
007660     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
007670     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
007680     ACCEPT  �L�[���� FROM CONS.
007690     PERFORM �t�@�C����.
007700     EXIT PROGRAM.
007290*================================================================*
007300 �G���[�����o SECTION.
007310*
007320     IF �ʒm���o NOT = "00"
007330         DISPLAY NC"���[�G���["              UPON CONS
007340         DISPLAY NC"���ڌQ���o�F" ���ڌQ���o UPON CONS
007350         DISPLAY NC"�ʒm���o�F" �ʒm���o UPON CONS
007360         DISPLAY NC"�g������o�F" �g������o UPON CONS
007370         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
007380                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007390         ACCEPT  �L�[���� FROM CONS
007400         PERFORM �t�@�C����
007410         MOVE 99 TO PROGRAM-STATUS
007420         EXIT PROGRAM
007430     END-IF.
007440*================================================================*
007450 �t�@�C���� SECTION.
007460*
002990     IF ( �I�[�v���t���O = "YES" )
002991         CLOSE ����t�@�C��
003041     END-IF.
007480     CLOSE �����}�X�^   ������}�X�^
                 ��f�ҏ��e ��ƃt�@�C���P.
007490*================================================================*
007500 �I������ SECTION.
007510*
007520     PERFORM �t�@�C����.
006850*================================================================*
006860 ����s���ޔ� SECTION.
      *
006870     CLOSE ��f�ҏ��e.
006880     PERFORM �x������.
006890     OPEN I-O ��f�ҏ��e.
006900         MOVE NC"��f" TO �t�@�C����.
006910         PERFORM �I�[�v���`�F�b�N.
006920     PERFORM �x������.
006930*
006940     MOVE ZERO                TO ��|�{�p�a��.
006950     MOVE ZERO                TO ��|�{�p�N.
006960     MOVE ZERO                TO ��|�{�p��.
006970     MOVE �A���|���҃R�[�h    TO ��|���҃R�[�h.
006990*
007000     READ ��f�ҏ��e
007010     NOT INVALID KEY
007020         IF ����ʒu�b�m�s > 31
                  COMPUTE ����ʒu�b�m�s = ����ʒu�b�m�s - 31
               END-IF
007020         MOVE ����ʒu�b�m�s TO ��|�J���e������s��
007030         REWRITE ��|���R�[�h
007040         INVALID KEY
007050             MOVE NC"��e" TO �t�@�C����
007060             PERFORM �G���[�\��
007070         END-REWRITE
007080     END-READ.
007090     PERFORM �x������.
007100     CLOSE ��f�ҏ��e.
007110     PERFORM �x������.
007120     OPEN INPUT ��f�ҏ��e.
007130         MOVE NC"��f" TO �t�@�C����.
007140         PERFORM �I�[�v���`�F�b�N.
006850*================================================================*
006860 �e�X�g��� SECTION.
      *
           MOVE ALL "��"  TO ���Ҏ���.
           MOVE 99999     TO ���Ҕԍ�.
           MOVE "X"       TO �}��.
           PERFORM VARYING �J�E���^ FROM 1 BY 1 UNTIL �J�E���^ > 31
              MOVE 99     TO ��(�J�E���^) ��(�J�E���^)
              MOVE 99999  TO �{�×�(�J�E���^) ��×�(�J�E���^) ���×�(�J�E���^)
                             㪖@��(�J�E���^) �d�×�(�J�E���^) ��p�z(�J�E���^) �ꕔ���S��(�J�E���^)
              MOVE ALL "��"  TO �R�����g�P(�J�E���^) �R�����g�Q(�J�E���^)
           END-PERFORM.
           MOVE 99        TO ���v�� ������ ������
           MOVE 99999     TO ������ �Č��� ���������v ��×����v ���×����v 㪖@�����v
                             �d�×����v ���v���z ���S���v ���񋟗� �������q�� ���̑�
           MOVE ALL "��"  TO ���Ҏ��� �����R�����g�P �����R�����g�Q �����R�����g�R �����R�����g�S.
           MOVE NC"��"    TO ��`�F�b�N ���`�F�b�N ���`�F�b�N.
      *
004900*================================================================*
007540******************************************************************
007550 END PROGRAM YIW722.
007560******************************************************************
