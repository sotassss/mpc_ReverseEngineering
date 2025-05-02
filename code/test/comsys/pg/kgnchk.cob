000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             KGNCHK.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*         �g�p���Ǝg�p�����̍X�V�`�F�b�N
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2016-03-01
000130 DATE-COMPILED.          2016-03-01
000140*----------------------------------------------------------------*
000150******************************************************************
000160*            ENVIRONMENT         DIVISION                        *
000170******************************************************************
000180 ENVIRONMENT             DIVISION.
000190 CONFIGURATION           SECTION.
000200 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000210 OBJECT-COMPUTER.        FMV-DESKPOWER.
000230 SPECIAL-NAMES.          CONSOLE  IS  CONS
000240                         SYSERR   IS  MSGBOX.
000250 INPUT-OUTPUT            SECTION.
000260 FILE-CONTROL.
           SELECT  �ڋq���}�X�^  ASSIGN      TO        KOKYAKUL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  �ڋq�|�敪
                                   FILE STATUS              IS  ��ԃL�[
                                   LOCK        MODE         IS  AUTOMATIC.
           SELECT  �ڋq�������X�g  ASSIGN      TO        "C:\MAKISHISYS\UPDOBJ\kklst.dat"
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  �����|�敪
                                                                �����|�ڋq�ԍ�
                                   FILE STATUS              IS  ��ԃL�[
                                   LOCK        MODE         IS  AUTOMATIC.
      *
000400******************************************************************
000410*                      DATA DIVISION                             *
000420******************************************************************
000430 DATA                    DIVISION.
000810 FILE                    SECTION.
       FD  �ڋq���}�X�^      BLOCK   CONTAINS   1   RECORDS.
           COPY KOKYAKU         OF  XFDLIB  JOINING   �ڋq   AS  PREFIX.
      *
001190 FD  �ڋq�������X�g RECORD  CONTAINS 64 CHARACTERS.
001200 01 �����|���R�[�h.
001210    03 �����|���R�[�h�L�[.
             05 �����|�敪                   PIC 9(1).
             05 �����|�ڋq�ԍ�               PIC 9(5).
001280    03 �����|���R�[�h�f�[�^.
             05 �����|�g�p���               PIC 9(1).
004780       05 �����|�g�p�����N����.
004800          07 �����|�g�p�����N          PIC 9(4).
004810          07 �����|�g�p������          PIC 9(2).
004810          07 �����|�g�p������          PIC 9(2).
             05 FILLER                       PIC X(49).
000520******************************************************************
000530*                WORKING-STORAGE SECTION                         *
000540******************************************************************
000550 WORKING-STORAGE         SECTION.
001610 01 �L�[����                           PIC X     VALUE SPACE.
001620 01 ��ԃL�[                           PIC X(2)  VALUE SPACE.
001630 01 �I���t���O                         PIC X(3)  VALUE SPACE.
001280 01 �t�@�C�����v                       PIC N(5) VALUE SPACE.
001750 01 ���b�Z�[�W�v                       PIC X(40) VALUE SPACE.
001830*
004710****************
004720* �A�����ڑҔ� *
004730****************
004770 01 �ޔ��f�[�^�v.
          03 �敪�v                          PIC 9(1)  VALUE ZERO.
          03 �ڋq�ԍ��v                      PIC 9(5)  VALUE ZERO.
          03 �g�p��Ԃv                      PIC 9(1)  VALUE ZERO.
          03 �g�p�����g�p�t���O�v            PIC 9(1)  VALUE ZERO.
          03 �I�����C���c�k�v                PIC 9(1)  VALUE ZERO.
004780    03 �_��N�����v.
004800       05 �_��N�v                     PIC 9(4)  VALUE ZERO.
004810       05 �_�񌎂v                     PIC 9(2)  VALUE ZERO.
004810       05 �_����v                     PIC 9(2)  VALUE ZERO.
004780    03 �g�p�����N�����v.
004800       05 �g�p�����N�v                 PIC 9(4)  VALUE ZERO.
004810       05 �g�p�������v                 PIC 9(2)  VALUE ZERO.
004810       05 �g�p�������v                 PIC 9(2)  VALUE ZERO.
004780    03 �ŏI�N���N�����v.
004800       05 �ŏI�N���N�v                 PIC 9(4)  VALUE ZERO.
004810       05 �ŏI�N�����v                 PIC 9(2)  VALUE ZERO.
004810       05 �ŏI�N�����v                 PIC 9(2)  VALUE ZERO.
001830*
001660* ���t�v�n�q�j
001650 01 �v�Z�@����N�v                     PIC 9(2).
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
003330*
      * C �A�g�p
       01  �p�����^�v      PIC X(80) VALUE SPACE.
       01  �v���O�������v  PIC X(8)  VALUE "filexist".
      *
001831* POWER COBOL�p
001832 01 dll-name  PIC X(260)  VALUE SPACE.
001833 01 form-name PIC X(14)   VALUE SPACE.
001834*
001600******************************************************************
001610*                          �A������                              *
001620******************************************************************
      *
002491********************
002492* ���b�Z�[�W�\���L�[ *
002493********************
002494 01 �A���|�L�[ IS EXTERNAL.
002495    03  �A���|���b�Z�[�W                 PIC N(20).
      *
       01 �A���b�Z�[�W�o��� IS EXTERNAL.
          03 �A���o�|���b�Z�[�W�ԍ�            PIC 9(2).
          03 �A���o�|���b�Z�[�W.
             05 �A���o�|���b�Z�[�W���e         PIC X(40) OCCURS 6.
          03 �A���o�|���b�Z�[�W�P              PIC X(20).
          03 �A���o�|���b�Z�[�W�Q              PIC X(12).
002497*
       01 �g�A���C���|�{�p�敪��� IS EXTERNAL.
          03 �g�A���C���|�{�p�敪              PIC 9.
001630********************************
001640* �g�p�����̃p�����[�^ *
001650********************************
       01 �A�g�p������� IS EXTERNAL.
          03 �A�g�|���e                  PIC 9(1).
      *
004130******************************************************************
004140*                      PROCEDURE  DIVISION                       *
004150******************************************************************
004160 PROCEDURE               DIVISION.
003290************
003300*           *
003310* ��������   *
003320*           *
003330************
      *
003340     PERFORM ������.
004792*
003350************
003360*           *
003370* �又��     *
003380*           *
003390************
           PERFORM �ڋq���擾.
      *     IF �g�p�����g�p�t���O�v = 1
      *         IF �I�����C���c�k�v = 1
      *             PERFORM �_�E�����[�h����
      *         END-IF
               PERFORM �X�V����.
               PERFORM ���t�`�F�b�N.
      *     ELSE
      *         MOVE ZERO    TO �A�g�|���e
      *     END-IF.
      *     IF �A�g�|���e = ZERO
      *         MOVE �v�Z�@����           TO �ڋq�|�ŏI�N���N����
      *         REWRITE �ڋq�|���R�[�h
      *         END-REWRITE
      *     END-IF.
      *
003410************
003420*           *
003430* �I������   *
003440*           *
003450************
003460     PERFORM �I������.
004410     EXIT PROGRAM.
003500*================================================================*
003510 ������ SECTION.
003520*
018000     OPEN I-O �ڋq���}�X�^.
018010         MOVE NC"�ڋq" TO �t�@�C�����v.
018020         PERFORM �I�[�v���`�F�b�N.
      *
003940*    /* ���ݓ��t�擾 */
003950     ACCEPT �v�Z�@���t FROM DATE.
003960*    /* 1980�`2079�N�̊ԂŐݒ� */
003970     IF �v�Z�@�N > 80
003980         MOVE 19 TO �v�Z�@���I
003990     ELSE
004000         MOVE 20 TO �v�Z�@���I
004010     END-IF.
004040     COMPUTE �v�Z�@����N�v = �v�Z�@����N - 1988.
003860*================================================================*
003870 �I�[�v���`�F�b�N SECTION.
003880*
003890     IF ��ԃL�[  NOT =  "00"
003900         DISPLAY �t�@�C�����v NC"�e�I�[�v���G���[" UPON CONS
003910         DISPLAY NC"��ԃL�[�F" ��ԃL�[           UPON CONS
003920         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003930                                                   UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
003940         ACCEPT  �L�[���� FROM CONS
               MOVE 1    TO �A�g�|���e
003950         PERFORM �t�@�C����
003960         EXIT PROGRAM.
003500*================================================================*
003510 �ڋq���擾 SECTION.
003520*
           IF �g�A���C���|�{�p�敪 = ZERO
              MOVE ZERO                   TO �ڋq�|�敪
           ELSE
              MOVE 1                      TO �ڋq�|�敪
           END-IF.
           READ �ڋq���}�X�^
           INVALID KEY
               MOVE SPACE                 TO �ڋq�|���R�[�h
           END-READ.
           MOVE �ڋq�|�敪                TO �敪�v.
           MOVE �ڋq�|�ڋq�ԍ�            TO �ڋq�ԍ��v.
           MOVE �ڋq�|�g�p���            TO �g�p��Ԃv.
           MOVE �ڋq�|�g�p�����g�p�t���O  TO �g�p�����g�p�t���O�v.
           MOVE �ڋq�|�I�����C���c�k      TO �I�����C���c�k�v.
           MOVE �ڋq�|�_��N              TO �_��N�v.
           MOVE �ڋq�|�_��              TO �_�񌎂v.
           MOVE �ڋq�|�_���              TO �_����v.
           MOVE �ڋq�|�����N              TO �g�p�����N�v.
           MOVE �ڋq�|������              TO �g�p�������v.
           MOVE �ڋq�|������              TO �g�p�������v.
           MOVE �ڋq�|�ŏI�N���N          TO �ŏI�N���N�v.
           MOVE �ڋq�|�ŏI�N����          TO �ŏI�N�����v.
           MOVE �ڋq�|�ŏI�N����          TO �ŏI�N�����v.
003500*================================================================*
003510 �_�E�����[�h���� SECTION.
003520*
           MOVE "C:\MAKISHISYS\YAWOBJ\getcollect.exe" TO  �p�����^�v.
           CALL �v���O�������v WITH C LINKAGE  USING BY REFERENCE �p�����^�v.
      *
      *     IF PROGRAM-STATUS = ZERO
              CALL   �p�����^�v
              CANCEL �p�����^�v
              IF (PROGRAM-STATUS NOT = ZERO) AND (�g�p��Ԃv = ZERO)
005830           MOVE SPACE  TO  �A���b�Z�[�W�o���
                 MOVE 7      TO  �A���o�|���b�Z�[�W�ԍ�
005840           MOVE "�ڋq�R�[�h���擾�ł��܂���B"       TO �A���o�|���b�Z�[�W���e(1)
005840           MOVE "�V�X�e���Ǘ��҂ɘA�����Ă��������B" TO �A���o�|���b�Z�[�W���e(2)
004793           MOVE "PMSG001.DLL" TO dll-name
004794           MOVE "PMSG001"     TO form-name
004795           CALL "POWEROPENSHEET" USING dll-name form-name
              END-IF.
      *     ELSE
      *         MOVE "35"     TO ��ԃL�[
018010*         MOVE NC"�c�k" TO �t�@�C�����v
018020*         PERFORM �I�[�v���`�F�b�N
      *     END-IF.
003500*================================================================*
003510 �X�V���� SECTION.
003520*
018000     OPEN INPUT �ڋq�������X�g.
018010         MOVE NC"���X�g" TO �t�@�C�����v.
               IF ��ԃL�[  NOT =  "00" AND "35"
018020             PERFORM �I�[�v���`�F�b�N
               END-IF.
      *
           MOVE �敪�v                    TO �����|�敪.
           MOVE �ڋq�ԍ��v                TO �����|�ڋq�ԍ�.
           READ �ڋq�������X�g
           INVALID KEY
005830         MOVE SPACE  TO  �A���b�Z�[�W�o���
               MOVE 7      TO  �A���o�|���b�Z�[�W�ԍ�
005840         MOVE "�ڋq�R�[�h���擾�ł��܂���B"       TO �A���o�|���b�Z�[�W���e(1)
005840         MOVE "�V�X�e���Ǘ��҂ɘA�����Ă��������B" TO �A���o�|���b�Z�[�W���e(2)
004793         MOVE "PMSG001.DLL" TO dll-name
004794         MOVE "PMSG001"     TO form-name
004795         CALL "POWEROPENSHEET" USING dll-name form-name
           NOT INVALID KEY
               MOVE �����|�g�p���       TO �g�p��Ԃv
               MOVE �����|�g�p�����N���� TO �g�p�����N�����v
               MOVE �����|�g�p���       TO �ڋq�|�g�p���
               MOVE �����|�g�p�����N���� TO �ڋq�|�����N����
               REWRITE �ڋq�|���R�[�h
               END-REWRITE
           END-READ.
009220     CLOSE �ڋq�������X�g.
003520*
003500*================================================================*
003510 ���t�`�F�b�N SECTION.
003520*
           EVALUATE TRUE
           WHEN �v�Z�@���� < �ŏI�N���N�����v
005830         MOVE SPACE  TO  �A���b�Z�[�W�o���
               MOVE 1      TO  �A���o�|���b�Z�[�W�ԍ�
005840         MOVE "�V�X�e�����t���s���ł��B"       TO �A���o�|���b�Z�[�W���e(1)
004793         MOVE "PMSG001.DLL" TO dll-name
004794         MOVE "PMSG001"     TO form-name
004795         CALL "POWEROPENSHEET" USING dll-name form-name
               MOVE 1    TO �A�g�|���e
           WHEN �g�p��Ԃv = 2
005830         MOVE SPACE  TO  �A���b�Z�[�W�o���
               MOVE 10     TO  �A���o�|���b�Z�[�W�ԍ�
005840         MOVE "�_����Ԃ��I�����܂����B"               TO �A���o�|���b�Z�[�W���e(1)
005840         MOVE "���������w�s�g�d �_�x�������p��������"  TO �A���o�|���b�Z�[�W���e(2)
005840         MOVE "�ɂ́A�_�񉄒��̂��\�����݂����肢��"   TO �A���o�|���b�Z�[�W���e(3)
005840         MOVE "�����܂��B"                             TO �A���o�|���b�Z�[�W���e(4)
004793         MOVE "PMSG001.DLL" TO dll-name
004794         MOVE "PMSG001"     TO form-name
004795         CALL "POWEROPENSHEET" USING dll-name form-name
               MOVE 1    TO �A�g�|���e
           WHEN (�g�p��Ԃv = 1) AND (�v�Z�@���� > �g�p�����N�����v)
005830         MOVE SPACE  TO  �A���b�Z�[�W�o���
               MOVE 10     TO  �A���o�|���b�Z�[�W�ԍ�
005840         MOVE "�U�荞�݂��m�F�o���Ȃ��ׁA"             TO �A���o�|���b�Z�[�W���e(1)
005840         MOVE "�T�[�r�X�̂����p���~�����Ă�������"   TO �A���o�|���b�Z�[�W���e(2)
005840         MOVE "�܂����B�����p���p������ꍇ�͎g�p��"   TO �A���o�|���b�Z�[�W���e(3)
005840         MOVE "�̐U�荞�݂����肢�������܂��B"         TO �A���o�|���b�Z�[�W���e(4)
005840         MOVE "�f�[�^�̔��f�ɍő�P�O�c�Ɠ�������܂��B" TO �A���o�|���b�Z�[�W���e(5)
005840         MOVE "�U�荞�݌�A�A�������肢�������܂��B"   TO �A���o�|���b�Z�[�W���e(6)
004793         MOVE "PMSG001.DLL" TO dll-name
004794         MOVE "PMSG001"     TO form-name
004795         CALL "POWEROPENSHEET" USING dll-name form-name
               MOVE 1    TO �A�g�|���e
           WHEN (�g�p��Ԃv = 1) AND (�v�Z�@���� <= �g�p�����N�����v)
               STRING �g�p�����N�v               DELIMITED BY SIZE
                      "�N"                       DELIMITED BY SIZE
                      �g�p�������v               DELIMITED BY SIZE
                      "��"                       DELIMITED BY SIZE
                      �g�p�������v               DELIMITED BY SIZE
                      "���܂łɐU�荞�݂��m�F"   DELIMITED BY SIZE
                 INTO ���b�Z�[�W�v
               END-STRING
005830         MOVE SPACE  TO  �A���b�Z�[�W�o���
               MOVE 10     TO  �A���o�|���b�Z�[�W�ԍ�
005840         MOVE "�U�荞�݂��m�F�o���܂���B"             TO �A���o�|���b�Z�[�W���e(1)
005840         MOVE ���b�Z�[�W�v                             TO �A���o�|���b�Z�[�W���e(2)
005840         MOVE "�o���Ȃ��ꍇ�́A�����p���~������"     TO �A���o�|���b�Z�[�W���e(3)
005840         MOVE "���������܂��B"                         TO �A���o�|���b�Z�[�W���e(4)
005840         MOVE "�f�[�^�̔��f�ɍő�P�O�c�Ɠ�������܂��B" TO �A���o�|���b�Z�[�W���e(5)
005840         MOVE "�U�荞�݌�A�A�������肢�������܂��B"   TO �A���o�|���b�Z�[�W���e(6)
004793         MOVE "PMSG001.DLL" TO dll-name
004794         MOVE "PMSG001"     TO form-name
004795         CALL "POWEROPENSHEET" USING dll-name form-name
               MOVE 0    TO �A�g�|���e
           WHEN (�g�p��Ԃv = ZERO) AND (�v�Z�@���� > �g�p�����N�����v)
005830         MOVE SPACE  TO  �A���b�Z�[�W�o���
               MOVE 7      TO  �A���o�|���b�Z�[�W�ԍ�
005840         MOVE "�ڋq�R�[�h���擾�ł��܂���B"       TO �A���o�|���b�Z�[�W���e(1)
005840         MOVE "�V�X�e���Ǘ��҂ɘA�����Ă��������B" TO �A���o�|���b�Z�[�W���e(2)
004793         MOVE "PMSG001.DLL" TO dll-name
004794         MOVE "PMSG001"     TO form-name
004795         CALL "POWEROPENSHEET" USING dll-name form-name
               MOVE 1    TO �A�g�|���e
           WHEN OTHER
               MOVE 0    TO �A�g�|���e
           END-EVALUATE.
009150*================================================================*
009160 �I������ SECTION.
009170*
009180     PERFORM �t�@�C����.
009090*================================================================*
009100 �t�@�C���� SECTION.
009110*
009130     CLOSE �ڋq���}�X�^.
005570*================================================================*
005580******************************************************************
005590 END PROGRAM KGNCHK.
005600******************************************************************
