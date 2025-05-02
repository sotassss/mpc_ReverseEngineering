000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             K50610.
000060 AUTHOR.                 �R�c �_�V
000070*
000080*------------------------------------------------------------------*
000090* �v�Z�v���O�����F�ߘa6�N10���`�̑Ή��o�f
000100*
000110* �����̓��ꕉ�S�͈ȉ��̂ݑΉ��B����ȊO�́A���S�z�����O�A�S�z�����B
000120* ���S�敪:6,8,14 (���������z�A���񐔂𕉒S)
000130*          13  (���������z�A���񐔂𕉒S�i�P�O�~�P�ʁj)
000140*
000150* ���ۏ؂��Ή�
000160* ���ҕ������Ή�200907
000170*------------------------------------------------------------------*
000180 DATE-WRITTEN.           2004-08-26
000190 DATE-COMPILED.          2004-08-26
000200*----------------------------------------------------------------*
000210* 2014/05/08 ���c���a
000220* �@�g���|�̎������S���v�Z�敪��3:1�~�P�ʂŌ��̎����Z���S�z�i�������׊܁j��ǉ������̂őΉ�
000230* 2015/08/20 ���c���a
000240* �@���×��O�~�Ή��ׁ̈A�g�����|���Ȃ�����敪����������ɒǉ��i�t�@�C���A�T�C�����ƒǉ��j
000250* 2016/09/01 ���c���a
000260* �@���×���������Z����C���i����28�N10��01����������Ή��j
000270* 2017/02/27 ���c���a
000280* �@��Ï������{�̂܂Ƃ߂̏ꍇ�ł��A�������v�Z�����悤�ɏC���i�ȑO�͋���0�~�������j
000290* 2017/04/03 ���c���a
000300* �@�_�ސ쌧�̈�Ï����ňꕔ���S�������ꍇ�͖{�̂܂Ƃ߂��ȑO�d�l�ɖ߂�
000310* 2017/07/05 ���c���a
000320* �}�b�T�[�W�ƕό`�k�苸���p�̏d���{�p����Ή�
000330* 2017/08/31 ���c���a
000340* ���s�̏�Q�҈�Ï����ɂ��Ă͂��ꂼ��T�O�O�~�Q�񒥎�����l�Ɏd�l�ύX
000350* 2018/01/18 ���c���a
000360* ��Ï����̌v�Z�ŕό`�k��̒P�������������Ȃ�s����C��
000370* 2018/08/20 ���c���a
000371* ����30�N10��1������̎{�p�񍐏���t���̌v�Z��ǉ� (10/3 �ό`�Q��v�Z�C��)
000370* 2019/01/10 �r�c�K�q
000371* ����31�N1��1������̎�̈ϔC�֘A�̏C��
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
000490* KHT41410�Ƃ�EXTERNAL�p
000500     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000510                             ORGANIZATION             IS  INDEXED
000520                             ACCESS MODE              IS  DYNAMIC
000530                             RECORD KEY               IS  ��|�{�p�a��N��
000540                                                          ��|���҃R�[�h
000550                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000560                                                          ��|���҃J�i
000570                                                          ��|���҃R�[�h
000580                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000590                                                          ��|�{�p�a��N��
000600                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000610                                                          ��|�ی����
000620                                                          ��|�ی��Ҕԍ�
000630                                                          ��|���҃R�[�h
000640                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000650                                                          ��|������
000660                                                          ��|��p���S�Ҕԍ�
000670                                                          ��|���҃R�[�h
000680                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000690                                                          ��|�������
000700                                                          ��|��p���S�Ҕԍ�����
000710                                                          ��|���҃R�[�h
000720                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
000730                                                          ��|�{�p�a��N��
000740                                                          ��|���҃R�[�h
000750                             FILE STATUS              IS  ��ԃL�[
000760                             LOCK        MODE         IS  AUTOMATIC.
000770     SELECT  �ی��҃}�X�^    ASSIGN      TO        HOKENSL
000780                             ORGANIZATION             IS  INDEXED
000790                             ACCESS MODE              IS  DYNAMIC
000800                             RECORD KEY               IS  �ہ|�ی����
000810                                                          �ہ|�ی��Ҕԍ�
000820                             ALTERNATE RECORD KEY     IS  �ہ|�ی����
000830                                                          �ہ|�ی��Җ���
000840                                                          �ہ|�ی��Ҕԍ�
000850                             FILE STATUS              IS  ��ԃL�[
000860                             LOCK        MODE         IS  AUTOMATIC.
000870     SELECT  ���S���}�X�^    ASSIGN      TO        HUTANRIL
000880                             ORGANIZATION             IS  INDEXED
000890                             ACCESS MODE              IS  DYNAMIC
000900                             RECORD KEY               IS  �����|�ی����
000910                                                          �����|�J�n�a��N��
000920                             FILE STATUS              IS  ��ԃL�[
000930                             LOCK        MODE         IS  AUTOMATIC.
000940     SELECT  �s�����}�X�^    ASSIGN      TO        SITYOSNL
000950                             ORGANIZATION             IS  INDEXED
000960                             ACCESS MODE              IS  DYNAMIC
000970                             RECORD KEY               IS  �s�|������
000980                                                          �s�|�s�����ԍ�
000990                             ALTERNATE RECORD KEY     IS  �s�|������
001000                                                          �s�|�s��������
001010                                                          �s�|�s�����ԍ�
001020                             FILE STATUS              IS  ��ԃL�[
001030                             LOCK        MODE         IS  AUTOMATIC.
001040*
001050     SELECT  �ی��ғ��ʕ��S�}�X�^   ASSIGN      TO     HOKENTKL
001060                             ORGANIZATION          IS  INDEXED
001070                             ACCESS MODE           IS  DYNAMIC
001080                             RECORD KEY            IS  �ۓ��|�ی����
001090                                                       �ۓ��|�ی��Ҕԍ�
001100                                                       �ۓ��|�J�n�a��N��
001110                             FILE STATUS           IS  ��ԃL�[
001120                             LOCK        MODE      IS  AUTOMATIC.
001130*
001140*-----------------------------------------------------------------------------*
001150     SELECT  �g�����}�X�^    ASSIGN      TO        HRYOKINL
001160                             ORGANIZATION             IS  INDEXED
001170                             ACCESS MODE              IS  DYNAMIC
001180                             RECORD KEY               IS  �g���P�|�敪�R�[�h
001190                                                          �g���P�|�J�n�a��N��
001200                             FILE STATUS              IS  ��ԃL�[
001210                             LOCK        MODE         IS  AUTOMATIC.
001220*
001230     SELECT  �g���Z�v�g�e    ASSIGN      TO        HRECEL
001240                             ORGANIZATION             IS  INDEXED
001250                             ACCESS MODE              IS  DYNAMIC
001260                             RECORD KEY               IS  �g���Z�|�{�p�敪
001270                                                          �g���Z�|�{�p�a��N��
001280                                                          �g���Z�|���҃R�[�h
001290                                                          �g���Z�|���Z���
001300                             ALTERNATE RECORD KEY     IS  �g���Z�|�{�p�敪
001310                                                          �g���Z�|���҃R�[�h
001320                                                          �g���Z�|�{�p�a��N��
001330                                                          �g���Z�|���Z���
001340                             ALTERNATE RECORD KEY     IS  �g���Z�|�{�p�a��N��
001350                                                          �g���Z�|���҃R�[�h
001360                                                          �g���Z�|���Z���
001370                                                          �g���Z�|�{�p�敪
001380                             ALTERNATE RECORD KEY     IS  �g���Z�|�����Ώۋ敪
001390                                                          �g���Z�|�����a��N��
001400                                                          �g���Z�|�{�p�敪
001410                                                          �g���Z�|�{�p�a��N��
001420                                                          �g���Z�|���҃R�[�h
001430                                                          �g���Z�|���Z���
001440                             FILE STATUS              IS  ��ԃL�[
001450                             LOCK        MODE         IS  AUTOMATIC.
001460*
001470*     SELECT  �g�����f�[�^�e  ASSIGN      TO        HHUSYOUL
001480*                             ORGANIZATION             IS  INDEXED
001490*                             ACCESS MODE              IS  DYNAMIC
001500*                             RECORD KEY               IS  �g���|��L�[
001510*                             ALTERNATE RECORD KEY     IS  �g���|�{�p�敪
001520*                                                          �g���|���҃R�[�h
001530*                                                          �g���|��L�[
001540*                             FILE STATUS              IS  ��ԃL�[
001550*                             LOCK        MODE         IS  AUTOMATIC.
001560*
001570     SELECT  �g���v�f�[�^�e  ASSIGN      TO        HNIKEIL
001580                             ORGANIZATION             IS  INDEXED
001590                             ACCESS MODE              IS  DYNAMIC
001600                             RECORD KEY               IS  �g���|�{�p�敪
001610                                                          �g���|�{�p�a��N����
001620                                                          �g���|���҃R�[�h
001630                             ALTERNATE RECORD KEY     IS  �g���|�{�p�敪
001640                                                          �g���|���҃R�[�h
001650                                                          �g���|�{�p�a��N����
001660                             ALTERNATE RECORD KEY     IS  �g���|�{�p�a��N����
001670                                                          �g���|�o�^��
001680                             FILE STATUS              IS  ��ԃL�[
001690                             LOCK        MODE         IS  AUTOMATIC.
001700*
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
001850     SELECT  ��v�̎��e      ASSIGN      TO        RYOSYUL
001860                             ORGANIZATION             IS  INDEXED
001870                             ACCESS MODE              IS  DYNAMIC
001880                             RECORD KEY               IS  �́|�{�p�敪
001890                                                          �́|��v�̎��敪
001900                                                          �́|�{�p�a��N����
001910                                                          �́|���҃R�[�h
001920                             ALTERNATE RECORD KEY     IS  �́|�{�p�敪
001930                                                          �́|���҃R�[�h
001940                                                          �́|��v�̎��敪
001950                                                          �́|�{�p�a��N����
001960                             FILE STATUS              IS  ��ԃL�[
001970                             LOCK        MODE         IS  AUTOMATIC.
001980*
001990     SELECT  �g������}�X�^  ASSIGN      TO      HSEIGYOL
002000                               ORGANIZATION             IS  INDEXED
002010                               ACCESS MODE              IS  DYNAMIC
002020                               RECORD KEY               IS  �g���|����敪
002030                               FILE STATUS              IS  ��ԃL�[
002040                               LOCK        MODE         IS  AUTOMATIC.
002050*
002060     SELECT  �Q�Ǝ�f�ҏ��e    ASSIGN      TO        JUSINJL
002070                             ORGANIZATION             IS  INDEXED
002080                             ACCESS MODE              IS  DYNAMIC
002090                             RECORD KEY               IS  �Q�Ǝ�|�{�p�a��N��
002100                                                          �Q�Ǝ�|���҃R�[�h
002110                             ALTERNATE RECORD KEY     IS  �Q�Ǝ�|�{�p�a��N��
002120                                                          �Q�Ǝ�|���҃J�i
002130                                                          �Q�Ǝ�|���҃R�[�h
002140                             ALTERNATE RECORD KEY     IS  �Q�Ǝ�|���҃R�[�h
002150                                                          �Q�Ǝ�|�{�p�a��N��
002160                             ALTERNATE RECORD KEY     IS  �Q�Ǝ�|�{�p�a��N��
002170                                                          �Q�Ǝ�|�ی����
002180                                                          �Q�Ǝ�|�ی��Ҕԍ�
002190                                                          �Q�Ǝ�|���҃R�[�h
002200                             ALTERNATE RECORD KEY     IS  �Q�Ǝ�|�{�p�a��N��
002210                                                          �Q�Ǝ�|������
002220                                                          �Q�Ǝ�|��p���S�Ҕԍ�
002230                                                          �Q�Ǝ�|���҃R�[�h
002240                             ALTERNATE RECORD KEY     IS  �Q�Ǝ�|�{�p�a��N��
002250                                                          �Q�Ǝ�|�������
002260                                                          �Q�Ǝ�|��p���S�Ҕԍ�����
002270                                                          �Q�Ǝ�|���҃R�[�h
002280                             ALTERNATE RECORD KEY     IS  �Q�Ǝ�|�����a��N��
002290                                                          �Q�Ǝ�|�{�p�a��N��
002300                                                          �Q�Ǝ�|���҃R�[�h
002310                             FILE STATUS              IS  ��ԃL�[
002320                             LOCK        MODE         IS  AUTOMATIC.
002330**
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
000620     SELECT  �g�{�p�����}�X�^ ASSIGN      TO     HSEJOHOL
000630                                ORGANIZATION             IS  INDEXED
000640                                ACCESS MODE              IS  DYNAMIC
000650                                RECORD KEY               IS  �g�{��|�{�p���ԍ�
000660                                FILE STATUS              IS  ��ԃL�[
000670                                LOCK        MODE         IS  AUTOMATIC.
002520*
002530     SELECT  �X�V�g���Z�v�g�e    ASSIGN      TO        HRECEL
002540                             ORGANIZATION             IS  INDEXED
002550                             ACCESS MODE              IS  DYNAMIC
002560                             RECORD KEY               IS  �X�V�g���Z�|�{�p�敪
002570                                                          �X�V�g���Z�|�{�p�a��N��
002580                                                          �X�V�g���Z�|���҃R�[�h
002590                                                          �X�V�g���Z�|���Z���
002600                             ALTERNATE RECORD KEY     IS  �X�V�g���Z�|�{�p�敪
002610                                                          �X�V�g���Z�|���҃R�[�h
002620                                                          �X�V�g���Z�|�{�p�a��N��
002630                                                          �X�V�g���Z�|���Z���
002640                             ALTERNATE RECORD KEY     IS  �X�V�g���Z�|�{�p�a��N��
002650                                                          �X�V�g���Z�|���҃R�[�h
002660                                                          �X�V�g���Z�|���Z���
002670                                                          �X�V�g���Z�|�{�p�敪
002680                             ALTERNATE RECORD KEY     IS  �X�V�g���Z�|�����Ώۋ敪
002690                                                          �X�V�g���Z�|�����a��N��
002700                                                          �X�V�g���Z�|�{�p�敪
002710                                                          �X�V�g���Z�|�{�p�a��N��
002720                                                          �X�V�g���Z�|���҃R�[�h
002730                                                          �X�V�g���Z�|���Z���
002740                             FILE STATUS              IS  ��ԃL�[
002750                             LOCK        MODE         IS  AUTOMATIC.
002760*
000140     SELECT  �g���Z�v�g�ڍׂe    ASSIGN      TO       HRECEHKL
000150                             ORGANIZATION             IS  INDEXED
000160                             ACCESS MODE              IS  DYNAMIC
000170                             RECORD KEY               IS  �g���Z�ڍׁ|�{�p�敪
000180                                                          �g���Z�ڍׁ|�{�p�a��N��
000190                                                          �g���Z�ڍׁ|���҃R�[�h
000200                                                          �g���Z�ڍׁ|���Z���
000350                             FILE STATUS              IS  ��ԃL�[
000360                             LOCK        MODE         IS  AUTOMATIC.
000101*
000102     SELECT  �{�݃}�X�^         ASSIGN      TO       SISETUL
000103                                ORGANIZATION             IS  INDEXED
000104                                ACCESS MODE              IS  DYNAMIC
000105                                RECORD KEY               IS  �{�݁|�{�݃R�[�h
000111                                FILE STATUS              IS  ��ԃL�[
000112                                LOCK        MODE         IS  AUTOMATIC.
000113
000370*
001360     SELECT  ��ƃt�@�C���P  ASSIGN      TO        "C:\MAKISHISYS\hmobj\TEMP\W50610L.DAT"
001370                             ORGANIZATION             IS  INDEXED
001380                             ACCESS                   IS  DYNAMIC
001390                             RECORD      KEY          IS  ��P�|�{�p�敪
000180                                                          ��P�|�{�p�a��N����
000190                                                          ��P�|���҃R�[�h
001490                             FILE        STATUS       IS  ��ԃL�[
001500                             LOCK        MODE         IS  AUTOMATIC.
000370*
001360     SELECT  ��ƃt�@�C���Q  ASSIGN      TO        "C:\MAKISHISYS\hmobj\TEMP\W506102L.DAT"
001370                             ORGANIZATION             IS  INDEXED
001380                             ACCESS                   IS  DYNAMIC
001390                             RECORD      KEY          IS  ��Q�|�{�p�a��N����
000190                                                          ��Q�|���҃R�[�h
001490                             FILE        STATUS       IS  ��ԃL�[
001500                             LOCK        MODE         IS  AUTOMATIC.
002340     SELECT  �X�V�p�g���Î��тe    ASSIGN      TO        HNOURYOL
002350                             ORGANIZATION             IS  INDEXED
002360                             ACCESS MODE              IS  DYNAMIC
002370                             RECORD KEY               IS  �X�g�����|�{�p�敪
002380                                                          �X�g�����|�{�p�a��N����
002390                                                          �X�g�����|���҃R�[�h
002400                             ALTERNATE RECORD KEY     IS  �X�g�����|�{�p�敪
002410                                                          �X�g�����|���҃R�[�h
002420                                                          �X�g�����|�{�p�a��N����
002430                             ALTERNATE RECORD KEY     IS  �X�g�����|�{�p�a��N����
002440                                                          �X�g�����|�{�p�Ҕԍ�
002450                                                          �X�g�����|�o�^��
002460                             ALTERNATE RECORD KEY     IS  �X�g�����|�{�p�a��N����
002470                                                          �X�g�����|�{�p�J�n����
002480                                                          �X�g�����|�{�p�Ҕԍ�
002490                                                          �X�g�����|�o�^��
002500                             FILE STATUS              IS  ��ԃL�[
002510                             LOCK        MODE         IS  AUTOMATIC.
002760*
000140     SELECT  �Q�Ƃg���Z�v�g�ڍׂe    ASSIGN      TO       HRECEHKL
000150                             ORGANIZATION             IS  INDEXED
000160                             ACCESS MODE              IS  DYNAMIC
000170                             RECORD KEY               IS  �Q�g���Z�ڍׁ|�{�p�敪
000180                                                          �Q�g���Z�ڍׁ|�{�p�a��N��
000190                                                          �Q�g���Z�ڍׁ|���҃R�[�h
000200                                                          �Q�g���Z�ڍׁ|���Z���
000350                             FILE STATUS              IS  ��ԃL�[
000360                             LOCK        MODE         IS  AUTOMATIC.
000101*
001160     SELECT  �����}�X�^      ASSIGN      TO        GENGOUL
001170                             ORGANIZATION             IS  INDEXED
001180                             ACCESS MODE              IS  DYNAMIC
001190                             RECORD KEY               IS  ���|�����敪
001200                             FILE STATUS              IS  ��ԃL�[
001210                             LOCK        MODE         IS  AUTOMATIC.
002770******************************************************************
002780*                      DATA DIVISION                             *
002790******************************************************************
002800 DATA                    DIVISION.
002810 FILE                    SECTION.
002820*
002830*-----------------------------------------------------------------------*
002840** EXTERNAL�p
002850 FD  ��f�ҏ��e IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
002860     COPY JUSINJ     OF  XFDLIB  JOINING   ��   AS  PREFIX.
002870 FD  �ی��҃}�X�^ IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
002880     COPY HOKENS     OF  XFDLIB  JOINING   ��   AS  PREFIX.
002890 FD  ���S���}�X�^ IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
002900     COPY HUTANRI    OF  XFDLIB  JOINING   ���� AS  PREFIX.
002910 FD  �s�����}�X�^ IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
002920     COPY SITYOSN    OF  XFDLIB  JOINING   �s   AS  PREFIX.
002930 FD  �ی��ғ��ʕ��S�}�X�^  IS EXTERNAL BLOCK   CONTAINS   1   RECORDS.
002940     COPY HOKENTK    OF  XFDLIB  JOINING   �ۓ� AS PREFIX.
002950*
002960*-----------------------------------------------------------------------//
002970*                           �m�q�k��  �R�W�S�n
002980 FD  �g�����}�X�^        BLOCK   CONTAINS   1   RECORDS.
002990     COPY H_RYOKIN1  OF  XFDLIB  JOINING   �g���P   AS  PREFIX.
003000*                           �m�q�k��  �V�U�W�n
003010 FD  �g���Z�v�g�e        BLOCK   CONTAINS   1   RECORDS.
003020     COPY H_RECE     OF  XFDLIB  JOINING   �g���Z  AS  PREFIX.
003030*                           �m�q�k��  �U�S�O�n
003040* FD  �g�����f�[�^�e     BLOCK   CONTAINS   1   RECORDS.
003050*     COPY H_HUSYOU  OF  XFDLIB  JOINING   �g�� AS  PREFIX.
003060*                           �m�q�k��  �T�P�Q�n
003070 FD  �g���v�f�[�^�e      BLOCK   CONTAINS   1   RECORDS.
003080     COPY H_NIKEI    OF  XFDLIB  JOINING   �g��   AS  PREFIX.
003090*
003100 FD  �Q�Ƃg���v�f�[�^�e  BLOCK   CONTAINS   1   RECORDS.
003110     COPY H_NIKEI    OF  XFDLIB  JOINING   �Q�Ƃg��   AS  PREFIX.
003120*
003130 FD  ��v�̎��e          BLOCK   CONTAINS   1   RECORDS.
003140     COPY RYOSYU     OF  XFDLIB  JOINING   ��  AS  PREFIX.
003150*
003160 FD  �g������}�X�^    BLOCK   CONTAINS   1   RECORDS.
003170     COPY H_SEIGYO   OF  XFDLIB  JOINING   �g��   AS  PREFIX.
003180*
003190 FD  �Q�Ǝ�f�ҏ��e    BLOCK   CONTAINS   1   RECORDS.
003200     COPY JUSINJ     OF  XFDLIB  JOINING   �Q�Ǝ�   AS  PREFIX.
003210*
003220 FD  �g���Î��тe        BLOCK   CONTAINS   1   RECORDS.
003230     COPY H_NOURYO   OF  XFDLIB  JOINING   �g����   AS  PREFIX.
001780*                           �m�q�k��  1280�n
001790 FD  �g�{�p�����}�X�^  BLOCK   CONTAINS   1   RECORDS.
001800     COPY H_SEJOHO   OF  XFDLIB  JOINING   �g�{��   AS  PREFIX.
           COPY H_SEJOHO41 OF  XFDLIB  JOINING   �g�{��S�P   AS  PREFIX.
003240*                           �m�q�k��  �V�U�W�n
003250 FD  �X�V�g���Z�v�g�e    BLOCK   CONTAINS   1   RECORDS.
003260     COPY H_RECE     OF  XFDLIB  JOINING   �X�V�g���Z  AS  PREFIX.
      *
000380 FD  �g���Z�v�g�ڍׂe        BLOCK   CONTAINS   1   RECORDS.
000390     COPY H_RECEHK    OF  XFDLIB  JOINING   �g���Z�ڍ�  AS  PREFIX.
000113*
000114 FD  �{�݃}�X�^  BLOCK   CONTAINS   1   RECORDS.
000115     COPY SISETU    OF  XFDLIB  JOINING   �{��   AS  PREFIX.
001350*
001870 FD  ��ƃt�@�C���P RECORD  CONTAINS 32 CHARACTERS.
001880 01  ��P�|���R�[�h.
001890   03  ��P�|���R�[�h�L�[.
000450* 1:�I���A2:����܁E�}�b�T�[�W
000460     05 ��P�|�{�p�敪                       PIC 9.
000470*-----------------------------------------------*
000390     05 ��P�|�{�p�a��N����.
000480        07 ��P�|�{�p�a��N��.
000490           09 ��P�|�{�p�a��                 PIC 9.
000500           09 ��P�|�{�p�N��.
000510              11 ��P�|�{�p�N                PIC 9(2).
000520              11 ��P�|�{�p��                PIC 9(2).
000450        07 ��P�|�{�p��                      PIC 9(2).
000530     05 ��P�|���҃R�[�h.
000540        07 ��P�|���Ҕԍ�                    PIC 9(6).
000550        07 ��P�|�}��                        PIC X.
003270*---------------------------------------------------------------*
001350*
001870 FD  ��ƃt�@�C���Q RECORD  CONTAINS 32 CHARACTERS.
001880 01  ��Q�|���R�[�h.
001890   03  ��Q�|���R�[�h�L�[.
000470*-----------------------------------------------*
000390     05 ��Q�|�{�p�a��N����.
000480        07 ��Q�|�{�p�a��N��.
000490           09 ��Q�|�{�p�a��                 PIC 9.
000500           09 ��Q�|�{�p�N��.
000510              11 ��Q�|�{�p�N                PIC 9(2).
000520              11 ��Q�|�{�p��                PIC 9(2).
000450        07 ��Q�|�{�p��                      PIC 9(2).
000530     05 ��Q�|���҃R�[�h.
000540        07 ��Q�|���Ҕԍ�                    PIC 9(6).
000550        07 ��Q�|�}��                        PIC X.
003270*---------------------------------------------------------------*
003210*
003220 FD  �X�V�p�g���Î��тe        BLOCK   CONTAINS   1   RECORDS.
003230     COPY H_NOURYO   OF  XFDLIB  JOINING   �X�g����   AS  PREFIX.
      *
000380 FD  �Q�Ƃg���Z�v�g�ڍׂe        BLOCK   CONTAINS   1   RECORDS.
000390     COPY H_RECEHK    OF  XFDLIB  JOINING   �Q�g���Z�ڍ�  AS  PREFIX.
000113*
002200*                           �m�q�k��  �P�Q�W�n
002210 FD  �����}�X�^          BLOCK   CONTAINS   1   RECORDS.
002220     COPY GENGOU     OF  XFDLIB  JOINING   ��   AS  PREFIX.
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
003360 01 �I���t���O�T                       PIC X(3) VALUE SPACE.
003370 01 �t�@�C����                         PIC N(10) VALUE SPACE.
003380 01 ��v�̎������t���O                 PIC X(3) VALUE SPACE.
       01 ��̈ϔC�e                         PIC 9(1) VALUE ZERO.
       01 �{�p�Ҏ�̈ϔC�e                   PIC 9(1) VALUE ZERO.
003390*
003400 01 ���Ê�{����                       PIC 9(2)V9 VALUE ZERO.
003410*
003420* ���v�̌v�Z�̒P�ʁi0:10�~�P�ʁA1:1�~�P�ʁA2:1�~�P�ʂŌ��̘A���̎����̂݃��Z�v�g���S�z�Ɠ��z�j
003430 01 �̎������S���v�Z�敪�v             PIC 9 VALUE ZERO.
003440*
003450*--------------------------------------------------------------*
003460**
003470 01 �������v.
003480     03 �͂肫�イ�f�[�^�v.
003490       05 �g�P�p����v                        PIC 9(5).
003500       05 �g�Q�p����v                        PIC 9(5).
003510       05 �g�P�p�v                            PIC 9(5).
003520       05 �g�Q�p�v                            PIC 9(5).
003530       05 �g�d�×��v                          PIC 9(4).
003540       05 �g���×��v.
003550         07 �g���Ê�{�����v                  PIC 9(4).
003560         07 �g���Òǉ������v                  PIC 9(4).
      */�ߘa6�N10���ȍ~������/*20240618-----------------------------------
000350*�ǉ�
000331       05 �g�P�p�K��{�p���P�v                PIC 9(5) VALUE ZERO.
000332       05 �g�Q�p�K��{�p���P�v                PIC 9(5) VALUE ZERO.
000331       05 �g�P�p�K��{�p���Q�v                PIC 9(5) VALUE ZERO.
000332       05 �g�Q�p�K��{�p���Q�v                PIC 9(5) VALUE ZERO.
000331       05 �g�P�p�K��{�p���R�v                PIC 9(5) VALUE ZERO.
000332       05 �g�Q�p�K��{�p���R�v                PIC 9(5) VALUE ZERO.
000331       05 �g�P�p�K��{�p���S�v                PIC 9(5) VALUE ZERO.
000332       05 �g�Q�p�K��{�p���S�v                PIC 9(5) VALUE ZERO.
             05 �P�p�������v                        PIC 9(5) VALUE ZERO.
             05 �Q�p�������v                        PIC 9(5) VALUE ZERO.
      */�ߘa6�N10���ȍ~������/*20240618-----------------------------------
      *
003570     03 �}�b�T�[�W�f�[�^�v.
003580       05 �l�P�Ǐ��v                          PIC 9(5).
003590       05 �l�ό`�k�苸���p�v                  PIC 9(5).
003600       05 �l��㪖@���v                        PIC 9(4).
003610       05 �l�d�×��v                          PIC 9(4).
003620       05 �l���×��v.
003630         07 �l���Ê�{�����v                  PIC 9(4).
003640         07 �l���Òǉ������v                  PIC 9(4).
      */�ߘa6�N10���ȍ~������/*20240621-----------------------------------
000350*�ǉ�
000339         05  �l�P�Ǐ��{�p���v                 PIC 9(4) VALUE ZERO.
000339         05  �l�Q�Ǐ��{�p���v                 PIC 9(4) VALUE ZERO.
000339         05  �l�R�Ǐ��{�p���v                 PIC 9(4) VALUE ZERO.
000339         05  �l�S�Ǐ��{�p���v                 PIC 9(4) VALUE ZERO.
000339         05  �l�T�Ǐ��{�p���v                 PIC 9(4) VALUE ZERO.
000339         05  �l�P�Ǐ��K��{�p���P�v           PIC 9(4) VALUE ZERO.
000339         05  �l�Q�Ǐ��K��{�p���P�v           PIC 9(4) VALUE ZERO.
000339         05  �l�R�Ǐ��K��{�p���P�v           PIC 9(4) VALUE ZERO.
000339         05  �l�S�Ǐ��K��{�p���P�v           PIC 9(4) VALUE ZERO.
000339         05  �l�T�Ǐ��K��{�p���P�v           PIC 9(4) VALUE ZERO.
000339         05  �l�P�Ǐ��K��{�p���Q�v           PIC 9(4) VALUE ZERO.
000339         05  �l�Q�Ǐ��K��{�p���Q�v           PIC 9(4) VALUE ZERO.
000339         05  �l�R�Ǐ��K��{�p���Q�v           PIC 9(4) VALUE ZERO.
000339         05  �l�S�Ǐ��K��{�p���Q�v           PIC 9(4) VALUE ZERO.
000339         05  �l�T�Ǐ��K��{�p���Q�v           PIC 9(4) VALUE ZERO.
000339         05  �l�P�Ǐ��K��{�p���R�v           PIC 9(4) VALUE ZERO.
000339         05  �l�Q�Ǐ��K��{�p���R�v           PIC 9(4) VALUE ZERO.
000339         05  �l�R�Ǐ��K��{�p���R�v           PIC 9(4) VALUE ZERO.
000339         05  �l�S�Ǐ��K��{�p���R�v           PIC 9(4) VALUE ZERO.
000339         05  �l�T�Ǐ��K��{�p���R�v           PIC 9(4) VALUE ZERO.
000339         05  �l�P�Ǐ��K��{�p���S�v           PIC 9(4) VALUE ZERO.
000339         05  �l�Q�Ǐ��K��{�p���S�v           PIC 9(4) VALUE ZERO.
000339         05  �l�R�Ǐ��K��{�p���S�v           PIC 9(4) VALUE ZERO.
000339         05  �l�S�Ǐ��K��{�p���S�v           PIC 9(4) VALUE ZERO.
000339         05  �l�T�Ǐ��K��{�p���S�v           PIC 9(4) VALUE ZERO.
      */�ߘa6�N10���ȍ~������/*20240621-----------------------------------
003651     03 ���ʃf�[�^�v.
003652       05 �{�p�񍐏���t���P���v              PIC 9(4).
002831       05 �I�����ʒn��敪�v                  PIC X VALUE SPACE.
000382       03 ���l���ʒn����Z���v                PIC 9(4) VALUE ZERO.
003653*
003660*---------------------------------------------------------------------*
003670 01 �v�Z���v.
003680     03 �͂荇�v�z�v          PIC 9(6).
003690     03 �͂�p�z�v            PIC 9(5).
003700     03 �͂�d�×��v          PIC 9(5).
003710     03 �͂艝�×��v          PIC 9(5).
003720*
003730     03 �}�b�T�[�W���v�z�v          PIC 9(6).
003740     03 �}�b�T�[�W���v              PIC 9(5).
003750     03 �}�b�T�[�W�ό`�k�苸���p�v  PIC 9(5).
003760     03 �}�b�T�[�W��㪖@���v        PIC 9(5).
003770     03 �}�b�T�[�W��㪖@�d�×��v    PIC 9(5).
003780     03 �}�b�T�[�W���×��v          PIC 9(5).
003790*
003800     03 �i�}�b�T�[�W���v�z�v          PIC 9(6).
003810     03 �i�}�b�T�[�W���v              PIC 9(6).
003820     03 �i�}�b�T�[�W�ό`�k�苸���p�v  PIC 9(6).
003830     03 �i�}�b�T�[�W��㪖@���v        PIC 9(6).
003840     03 �i�}�b�T�[�W��㪖@�d�×��v    PIC 9(6).
003850     03 �i�}�b�T�[�W���×��v          PIC 9(5).
003860*
003870*    ���Ê֘A
003880     03 ���Ô폜���v          PIC 9(3).
003890     03 ���Ï����v            PIC 9(3).
003900     03 ���Ï��v              PIC 9(3).
003910     03 ���Ï�]�v            PIC 9(3).
003920*
003930*---------------------------------------------------------------------*
003940*
003950 01 �݌v���v.
003960     03 �͂菉��{�p���e�敪�v PIC 9(2).
003970     03 �͂菉�񗿂v           PIC 9(6).
003980     03 �͂�񐔂v             PIC 9(3).
003990     03 �͂�d�C�񐔂v         PIC 9(3).
004000     03 ���イ�񐔂v           PIC 9(3).
004010     03 ���イ�d�C�񐔂v       PIC 9(3).
004020     03 �͂肫�イ�񐔂v       PIC 9(3).
004030     03 �͂肫�イ�d�C�񐔂v   PIC 9(3).
004040     03 �͂艝�É񐔂v         PIC 9(3).
004050     03 �͂艝�É��Z�񐔂v     PIC 9(3).
004060     03 �͂艝�É��Z�P���v     PIC 9(5).
004070     03 �͂艝�É��Z���v       PIC 9(6).
004080     03 �͂�݌v���×��v       PIC 9(6).
004090     03 �͂�݌v�z�v           PIC 9(6).
004100*
004110     03 �}�b�T�[�W�񐔂v                 PIC 9(3).
004120     03 �}�b�T�[�W�ό`�k�苸���p�񐔂v   PIC 9(3).
004130     03 �}�b�T�[�W��㪖@�񐔂v           PIC 9(3).
004140     03 �}�b�T�[�W��㪖@�d�C�񐔂v       PIC 9(3).
004150     03 �}�b�T�[�W���É񐔂v             PIC 9(3).
004160     03 �}�b�T�[�W���É��Z�񐔂v         PIC 9(3).
004170     03 �}�b�T�[�W���É��Z�P���v         PIC 9(5).
004180     03 �}�b�T�[�W���É��Z���v           PIC 9(6).
004190     03 �}�b�T�[�W�݌v���×��v           PIC 9(6).
004200     03 �}�b�T�[�W�݌v�z�v               PIC 9(6).
004210*
004220* ���É��Z�����p
004230     03 �ޔ����Ë����v                   PIC 9(2)V9 VALUE ZERO.
004240     03 ���É��Z��������v               PIC 9 VALUE ZERO.
004250*   ����{�p�̗L��
004260     03 ����{�p�t���O�v                 PIC 9 VALUE ZERO.
004270*
004280*    �{�p�񍐏���t��.
004281     03 �͂�񍐏��v.
004282        05 �͂�񍐏���t�񐔂v          PIC 9(2) VALUE ZERO.
004283        05 �͂�񍐏���t���v            PIC 9(6) VALUE ZERO.
004284     03 �}�b�T�[�W�񍐏��v.
004285        05 �}�b�T�[�W�񍐏���t�񐔂v    PIC 9(2) VALUE ZERO.
004286        05 �}�b�T�[�W�񍐏���t���v      PIC 9(6) VALUE ZERO.
      *
004287***
004288***��L�̗݌v���v�ƁA�������[�N���`�B
004290 01 �i�݌v���v.
004300     03 �i�͂菉��{�p���e�敪�v PIC 9(2).
004310     03 �i�͂菉�񗿂v           PIC 9(6).
004320     03 �i�͂�񐔂v             PIC 9(3).
004330     03 �i�͂�d�C�񐔂v         PIC 9(3).
004340     03 �i���イ�񐔂v           PIC 9(3).
004350     03 �i���イ�d�C�񐔂v       PIC 9(3).
004360     03 �i�͂肫�イ�񐔂v       PIC 9(3).
004370     03 �i�͂肫�イ�d�C�񐔂v   PIC 9(3).
004380     03 �i�͂艝�É񐔂v         PIC 9(3).
004390     03 �i�͂艝�É��Z�񐔂v     PIC 9(3).
004400     03 �i�͂艝�É��Z�P���v     PIC 9(5).
004410     03 �i�͂艝�É��Z���v       PIC 9(6).
004420     03 �i�͂�݌v���×��v       PIC 9(6).
004430     03 �i�͂�݌v�z�v           PIC 9(6).
004440*
004450     03 �i�}�b�T�[�W�񐔂v                 PIC 9(3).
004460     03 �i�}�b�T�[�W�ό`�k�苸���p�񐔂v   PIC 9(3).
004470     03 �i�}�b�T�[�W��㪖@�񐔂v           PIC 9(3).
004480     03 �i�}�b�T�[�W��㪖@�d�C�񐔂v       PIC 9(3).
004490     03 �i�}�b�T�[�W���É񐔂v             PIC 9(3).
004500     03 �i�}�b�T�[�W���É��Z�񐔂v         PIC 9(3).
004510     03 �i�}�b�T�[�W���É��Z�P���v         PIC 9(5).
004520     03 �i�}�b�T�[�W���É��Z���v           PIC 9(6).
004530     03 �i�}�b�T�[�W�݌v���×��v           PIC 9(6).
004540     03 �i�}�b�T�[�W�݌v�z�v               PIC 9(6).
004550*
004560* ���É��Z�����p
004570     03 �i�ޔ����Ë����v                   PIC 9(2)V9 VALUE ZERO.
004580     03 �i���É��Z��������v               PIC 9 VALUE ZERO.
004590*   ����{�p�̗L��
004600     03 �i����{�p�t���O�v                 PIC 9 VALUE ZERO.
004610*
004632*    �{�p�񍐏���t��.
004633     03 �i�͂�񍐏��v.
004634        05 �i�͂�񍐏���t�񐔂v          PIC 9(2) VALUE ZERO.
004635        05 �i�͂�񍐏���t���v            PIC 9(6) VALUE ZERO.
004636     03 �i�}�b�T�[�W�񍐏��v.
004637        05 �i�}�b�T�[�W�񍐏���t�񐔂v    PIC 9(2) VALUE ZERO.
004638        05 �i�}�b�T�[�W�񍐏���t���v      PIC 9(6) VALUE ZERO.
004639*---------------------------------------------------------------------*
004640*---------------------------------------------------------------------*
004641
004650* ��ʃ��[�N
004660 01 �}�b�T�[�W�ŗL�v.
004670     03 �}�b�T�[�W�Ǐ����v          PIC 9.
004680     03 �ό`�k�苸���p���v          PIC 9.
004690     03 �}�b�T�[�W�Ǐ����o�v        PIC 9.
004700     03 �ό`�k�苸���p���o�v        PIC 9.
004710     03 �}�b�T�[�W�Ǐ����Z�b�g�v    PIC 9.
004720     03 �ό`�k�苸���p���Z�b�g�v    PIC 9.
004730     03 �}�b�T�[�W�Ǐ������v�v      PIC 9.
004740     03 �ό`�k�苸���p�����v�v      PIC 9.
004750     03 �}�b�T�[�W�Ǐ��������v      PIC 9.
004760     03 �ό`�k�苸���p�������v      PIC 9.
004770     03 �}�b�T�[�W�����v�v          PIC 9(7).
004780     03 �ό`�k�藿���v�v            PIC 9(7).
004790*
004800 01 �i�}�b�T�[�W�ŗL�v.
004810     03 �i�}�b�T�[�W�Ǐ����v        PIC 9.
004820     03 �i�ό`�k�苸���p���v        PIC 9.
004830     03 �i�}�b�T�[�W�Ǐ����o�v      PIC 9.
004840     03 �i�ό`�k�苸���p���o�v      PIC 9.
004850     03 �i�}�b�T�[�W�Ǐ����Z�b�g�v  PIC 9.
004860     03 �i�ό`�k�苸���p���Z�b�g�v  PIC 9.
004870     03 �i�}�b�T�[�W�Ǐ������v�v    PIC 9.
004880     03 �i�ό`�k�苸���p�����v�v    PIC 9.
004890     03 �i�}�b�T�[�W�Ǐ��������v    PIC 9.
004900     03 �i�ό`�k�苸���p�������v    PIC 9.
004910     03 �i�}�b�T�[�W�����v�v        PIC 9(7).
004920     03 �i�ό`�k�藿���v�v          PIC 9(7).
004930*
004940 01 �}�b�T�[�W�{�p���Ӊӏ�.
004950     03 �}�b�T�[�W�̊����ӂv        PIC 9.
004960     03 �}�b�T�[�W�E�㎈���ӂv      PIC 9.
004970     03 �}�b�T�[�W���㎈���ӂv      PIC 9.
004980     03 �}�b�T�[�W�E�������ӂv      PIC 9.
004990     03 �}�b�T�[�W���������ӂv      PIC 9.
005000     03 �ό`�k�苸���p�E�㎈���ӂv  PIC 9.
005010     03 �ό`�k�苸���p���㎈���ӂv  PIC 9.
005020     03 �ό`�k�苸���p�E�������ӂv  PIC 9.
005030     03 �ό`�k�苸���p���������ӂv  PIC 9.
005040*
005050*   �{�p���ʂ��Ƃ̉񐔂Ɨ���
005060 01 �}�b�T�[�W���ʕʋ��z���v.
005070*      �}�b�T�[�W�֌W
005080   03 �}�b�T�[�W����{�p�v.
005090* / �}�b�T�[�W����{�p  0:�Ȃ��@1:����/
005100     05 �}�b�T�[�W����{�p�t���O�v            PIC 9.
005110     05 �}�b�T�[�W�̊��{�p�v.
005120       07 �}�b�T�[�W�̊��񐔂v                PIC 9(3).
005130       07 �}�b�T�[�W�̊����z�v                PIC 9(6).
005140     05 �}�b�T�[�W�E�㎈�{�p�v.
005150       07 �}�b�T�[�W�E�㎈�񐔂v              PIC 9(3).
005160       07 �}�b�T�[�W�E�㎈���z�v              PIC 9(6).
005170     05 �}�b�T�[�W���㎈�{�p�v.
005180       07 �}�b�T�[�W���㎈�񐔂v              PIC 9(3).
005190       07 �}�b�T�[�W���㎈���z�v              PIC 9(6).
005200     05 �}�b�T�[�W�E�����{�p�v.
005210       07 �}�b�T�[�W�E�����񐔂v              PIC 9(3).
005220       07 �}�b�T�[�W�E�������z�v              PIC 9(6).
005230     05 �}�b�T�[�W�������{�p�v.
005240       07 �}�b�T�[�W�������񐔂v              PIC 9(3).
005250       07 �}�b�T�[�W���������z�v              PIC 9(6).
005260*  �ό`�k�苸���p�֌W
005270   03 �ό`�k�苸���p����{�p�v.
005280*  / �ό`�k�苸���p����{�p�@1:����/
005290     05 �ό`�k�苸���p����{�p�t���O�v        PIC 9.
005300     05 �ό`�k�苸���p�E�㎈�{�p�v.
005310       07 �ό`�k�苸���p�E�㎈�񐔂v          PIC 9(3).
005320       07 �ό`�k�苸���p�E�㎈���z�v          PIC 9(6).
005330     05 �ό`�k�苸���p���㎈�{�p�v.
005340       07 �ό`�k�苸���p���㎈�񐔂v          PIC 9(3).
005350       07 �ό`�k�苸���p���㎈���z�v          PIC 9(6).
005360     05 �ό`�k�苸���p�E�����{�p�v.
005370       07 �ό`�k�苸���p�E�����񐔂v          PIC 9(3).
005380       07 �ό`�k�苸���p�E�������z�v          PIC 9(6).
005390     05 �ό`�k�苸���p�������{�p�v.
005400       07 �ό`�k�苸���p�������񐔂v          PIC 9(3).
005410       07 �ό`�k�苸���p���������z�v          PIC 9(6).
005420*
005430*   �{�p���ʂ��Ƃ̉񐔂Ɨ����i�����p�j
005440 01 �i�}�b�T�[�W���ʕʋ��z���v.
005450*      �i�}�b�T�[�W�֌W
005460   03 �i�}�b�T�[�W����{�p�v.
005470* / �i�}�b�T�[�W����{�p  0:�Ȃ��@1:����/
005480     05 �i�}�b�T�[�W����{�p�t���O�v          PIC 9.
005490     05 �i�}�b�T�[�W�̊��{�p�v.
005500       07 �i�}�b�T�[�W�̊��񐔂v              PIC 9(3).
005510       07 �i�}�b�T�[�W�̊����z�v              PIC 9(6).
005520     05 �i�}�b�T�[�W�E�㎈�{�p�v.
005530       07 �i�}�b�T�[�W�E�㎈�񐔂v            PIC 9(3).
005540       07 �i�}�b�T�[�W�E�㎈���z�v            PIC 9(6).
005550     05 �i�}�b�T�[�W���㎈�{�p�v.
005560       07 �i�}�b�T�[�W���㎈�񐔂v            PIC 9(3).
005570       07 �i�}�b�T�[�W���㎈���z�v            PIC 9(6).
005580     05 �i�}�b�T�[�W�E�����{�p�v.
005590       07 �i�}�b�T�[�W�E�����񐔂v            PIC 9(3).
005600       07 �i�}�b�T�[�W�E�������z�v            PIC 9(6).
005610     05 �i�}�b�T�[�W�������{�p�v.
005620       07 �i�}�b�T�[�W�������񐔂v            PIC 9(3).
005630       07 �i�}�b�T�[�W���������z�v            PIC 9(6).
005640*  �i�ό`�k�苸���p�֌W
005650   03 �i�ό`�k�苸���p����{�p�v.
005660*  / �i�ό`�k�苸���p����{�p  0:�Ȃ��@1:����/
005670     05 �i�ό`�k�苸���p����{�p�t���O�v      PIC 9.
005680     05 �i�ό`�k�苸���p�E�㎈�{�p�v.
005690       07 �i�ό`�k�苸���p�E�㎈�񐔂v        PIC 9(3).
005700       07 �i�ό`�k�苸���p�E�㎈���z�v        PIC 9(6).
005710     05 �i�ό`�k�苸���p���㎈�{�p�v.
005720       07 �i�ό`�k�苸���p���㎈�񐔂v        PIC 9(3).
005730       07 �i�ό`�k�苸���p���㎈���z�v        PIC 9(6).
005740     05 �i�ό`�k�苸���p�E�����{�p�v.
005750       07 �i�ό`�k�苸���p�E�����񐔂v        PIC 9(3).
005760       07 �i�ό`�k�苸���p�E�������z�v        PIC 9(6).
005770     05 �i�ό`�k�苸���p�������{�p�v.
005780       07 �i�ό`�k�苸���p�������񐔂v        PIC 9(3).
005790       07 �i�ό`�k�苸���p���������z�v        PIC 9(6).
005800*
005810 01 ���ۏ؃t���O                  PIC X(3) VALUE SPACE.
005820 01 ����t���O                      PIC X(3) VALUE SPACE.
005830 01 �����ʂv                      PIC 9(2) VALUE ZERO.
005840 01 ������ʂv                      PIC 9(2) VALUE ZERO.
005850 01 �ی���ʂv                      PIC 9(2) VALUE ZERO.
005860 01 ��p���S�Ҕԍ������v            PIC X(10) VALUE SPACE.
005860 01 ��p���S�Ҕԍ��v                PIC X(10) VALUE SPACE.
005860 01 �ی��Ҕԍ��v                    PIC X(10) VALUE SPACE.
005870* �ʉ@�e�[�u���p
005880 01 �ʉ@���b�v.
005890     03 �ʉ@���v                    PIC 9 OCCURS 31.
005900 01 �ʉ@���Q�b�v.
005910     03 �ʉ@���Q�v                  PIC 9 OCCURS 31.
005920*
005930 01 �J�E���^                        PIC 9(2) VALUE ZERO.
005940 01 �񐔃J�E���^                    PIC 9(2) VALUE ZERO.
005950 01 ���ÂO�~�J�E���^                PIC 9(2) VALUE ZERO.
005960*
005970**
005980*
005990 01 �v�Z�g�����v.
006000   03 �{�̏��ҕ����敪�v            PIC 9 VALUE ZERO.
006010   03 �������ҕ����敪�v            PIC 9 VALUE ZERO.
006020   03 �{�̂܂Ƃߋ敪�v              PIC 9 VALUE ZERO.
006030*
006040**
006050* �������r���J�n��
006060 01 �������r���J�n���v              PIC 9(2) VALUE ZERO.
006070*
006080 01 ���S�敪�v                      PIC 9(2) VALUE ZERO.
006090 01 �}�ԕ��S�݌v�z�v                PIC 9(6) VALUE ZERO.
006100***
006110***
006120* ���Z�e�ޔ�p
006130     COPY H_RECE   OF  XFDLIB  JOINING   �ޔ����Z  AS  PREFIX.
006140*
006150* �_�ސ��Ï����ňꕔ���S������t���O 2017/04/03
006160 01 �������S����t���O              PIC X(3) VALUE ZERO.
006170*
006238*   �O��{�p�񍐏���t���擾�ۑ�
006239 01 �O���t���v.
006240   03 �O���t�a��N���v.
006241     05 �O���t�a��v                          PIC 9 VALUE ZERO.
006242     05 �O���t�N�v                            PIC 9(2) VALUE ZERO.
006243     05 �O���t���v                            PIC 9(2) VALUE ZERO.
006249   03 ��t�\�a��N���v.
006250     05 ��t�\�a��v                          PIC 9 VALUE ZERO.
006251     05 ��t�\�N�v                            PIC 9(2) VALUE ZERO.
006252     05 ��t�\���v                            PIC 9(2) VALUE ZERO.
006253   03 �ό`�k��t���O�v                          PIC 9(1) VALUE ZERO.
006254   03 �v�Z�p�񍐏���t���v                      PIC 9(6) VALUE ZERO.
006255 01 �v���O�����v                                PIC X(8) VALUE "HMZKOFU".
HILO*********************************************************************�ǉ����[�N
000590*    / 1:���� 2:�{�� 3:���� /
000600 01 ���Ïꏊ�敪�v                              PIC 9(1) VALUE ZERO.
000610 01 ���Ïꏊ�v.
000620   03 ���Ïꏊ�{�݃R�[�h�v                      PIC 9(3) VALUE ZERO.
000630   03 ���Ïꏊ���҃R�[�h�v.
000640     05 ���Ïꏊ���Ҕԍ��v                      PIC 9(6) VALUE ZERO.
000650     05 ���Ïꏊ�}�Ԃv                          PIC X    VALUE SPACE.
000370   03 �{�p�敪�v                                PIC 9    VALUE ZERO.
000510   03 �{�p�Ҕԍ��v                              PIC 9(4) VALUE ZERO.
       01 ���ꌚ�����Ґ��v                            PIC 9(3) VALUE ZERO.
       01 ���ꌚ�����Ґ��v�Q                          PIC 9(3) VALUE ZERO.
       01 �����Ǐ����v                                PIC 9(1) VALUE ZERO.
      */20241112
000430 01 �g���Z�v�g�ڍ׃��R�[�h�L�[�v.
000460     05 �g���Z�v�g�ڍ׎{�p�敪�v                PIC 9.
000470*-----------------------------------------------*
000480     05 �g���Z�v�g�ڍ׎{�p�a��N���v.
000490        07 �g���Z�v�g�ڍ׎{�p�a��v             PIC 9.
000500        07 �g���Z�v�g�ڍ׎{�p�N���v.
000510          09 �g���Z�v�g�ڍ׎{�p�N�v             PIC 9(2).
000520          09 �g���Z�v�g�ڍ׎{�p���v             PIC 9(2).
000530     05 �g���Z�v�g�ڍ׊��҃R�[�h�v.
000540        07 �g���Z�v�g�ڍ׊��Ҕԍ��v             PIC 9(6).
000550        07 �g���Z�v�g�ڍ׎}�Ԃv                 PIC X.
000570     05 �g���Z�v�g�ڍ׃��Z��ʂv                PIC 9(2).
      *20240627
       01 �g���Z�v�g�ڍׂe�v.
000590   03 �g���Z�v�g�ڍ׃��R�[�h�f�[�^�v.
001400     05 ���ʗ�.
001410        07 ���ʒn����Z���P���v                 PIC 9(5) VALUE ZERO.
001420        07 ���ʒn����Z�񐔂v                   PIC 9(2) VALUE ZERO.
001420        07 ���ʒn����Z���v                     PIC 9(6) VALUE ZERO.
001420        07 �݌v���ʒn����Z���v                 PIC 9(6) VALUE ZERO.
              07 �K��d���e                           PIC 9(1) VALUE ZERO.
              07 �˔��s�e                           PIC 9(1) VALUE ZERO.
001490     05 �͂���z��.
001410        07 �͂肫�イ�ʏ��{�p���P���P�v         PIC 9(5) VALUE ZERO.
001420        07 �͂肫�イ�ʏ��{�p���񐔂P�v         PIC 9(2) VALUE ZERO.
001420        07 �͂肫�イ�ʏ��{�p���{�p���P�v       PIC 9(6) VALUE ZERO.
001410        07 �͂肫�イ�ʏ��{�p���P���Q�v         PIC 9(5) VALUE ZERO.
001420        07 �͂肫�イ�ʏ��{�p���񐔂Q�v         PIC 9(2) VALUE ZERO.
001420        07 �͂肫�イ�ʏ��{�p���{�p���Q�v       PIC 9(6) VALUE ZERO.
001410        07 �͂肫�イ�K��{�p���P�P���P�v       PIC 9(5) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���P�񐔂P�v       PIC 9(2) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���P�{�p���P�v     PIC 9(6) VALUE ZERO.
001410        07 �͂肫�イ�K��{�p���P�P���Q�v       PIC 9(5) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���P�񐔂Q�v       PIC 9(2) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���P�{�p���Q�v     PIC 9(6) VALUE ZERO.
001410        07 �͂肫�イ�K��{�p���Q�P���P�v       PIC 9(5) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���Q�񐔂P�v       PIC 9(2) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���Q�{�p���P�v     PIC 9(6) VALUE ZERO.
001410        07 �͂肫�イ�K��{�p���Q�P���Q�v       PIC 9(5) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���Q�񐔂Q�v       PIC 9(2) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���Q�{�p���Q�v     PIC 9(6) VALUE ZERO.
001410        07 �͂肫�イ�K��{�p���R�P���P�v       PIC 9(5) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���R�񐔂P�v       PIC 9(2) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���R�{�p���P�v     PIC 9(6) VALUE ZERO.
001410        07 �͂肫�イ�K��{�p���R�P���Q�v       PIC 9(5) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���R�񐔂Q�v       PIC 9(2) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���R�{�p���Q�v     PIC 9(6) VALUE ZERO.
001410        07 �͂肫�イ�K��{�p���S�P���P�v       PIC 9(5) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���S�񐔂P�v       PIC 9(2) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���S�{�p���P�v     PIC 9(6) VALUE ZERO.
001410        07 �͂肫�イ�K��{�p���S�P���Q�v       PIC 9(5) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���S�񐔂Q�v       PIC 9(2) VALUE ZERO.
001420        07 �͂肫�イ�K��{�p���S�{�p���Q�v     PIC 9(6) VALUE ZERO.
001420        07 �͂�P�p�񐔂v                       PIC 9(2) VALUE ZERO.
001420        07 ���イ�P�p�񐔂v                     PIC 9(2) VALUE ZERO.
001420        07 �͂肫�イ�Q�p�񐔂v                 PIC 9(2) VALUE ZERO.
001880     05 �}�b�T�[�W���z���v.
001410        07 �}�b�T�[�W�ʏ��{�p���P���P�v         PIC 9(5) VALUE ZERO.
001420        07 �}�b�T�[�W�ʏ��{�p���񐔂P�v         PIC 9(2) VALUE ZERO.
001420        07 �}�b�T�[�W�ʏ��{�p���{�p���P�v       PIC 9(6) VALUE ZERO.
001410        07 �}�b�T�[�W�ʏ��{�p���P���Q�v         PIC 9(5) VALUE ZERO.
001420        07 �}�b�T�[�W�ʏ��{�p���񐔂Q�v         PIC 9(2) VALUE ZERO.
001420        07 �}�b�T�[�W�ʏ��{�p���{�p���Q�v       PIC 9(6) VALUE ZERO.
001410        07 �}�b�T�[�W�K��{�p���P�P���P�v       PIC 9(5) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���P�񐔂P�v       PIC 9(2) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���P�{�p���P�v     PIC 9(6) VALUE ZERO.
001410        07 �}�b�T�[�W�K��{�p���P�P���Q�v       PIC 9(5) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���P�񐔂Q�v       PIC 9(2) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���P�{�p���Q�v     PIC 9(6) VALUE ZERO.
001410        07 �}�b�T�[�W�K��{�p���Q�P���P�v       PIC 9(5) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���Q�񐔂P�v       PIC 9(2) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���Q�{�p���P�v     PIC 9(6) VALUE ZERO.
001410        07 �}�b�T�[�W�K��{�p���Q�P���Q�v       PIC 9(5) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���Q�񐔂Q�v       PIC 9(2) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���Q�{�p���Q�v     PIC 9(6) VALUE ZERO.
001410        07 �}�b�T�[�W�K��{�p���R�P���P�v       PIC 9(5) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���R�񐔂P�v       PIC 9(2) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���R�{�p���P�v     PIC 9(6) VALUE ZERO.
001410        07 �}�b�T�[�W�K��{�p���R�P���Q�v       PIC 9(5) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���R�񐔂Q�v       PIC 9(2) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���R�{�p���Q�v     PIC 9(6) VALUE ZERO.
001410        07 �}�b�T�[�W�K��{�p���S�P���P�v       PIC 9(5) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���S�񐔂P�v       PIC 9(2) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���S�{�p���P�v     PIC 9(6) VALUE ZERO.
001410        07 �}�b�T�[�W�K��{�p���S�P���Q�v       PIC 9(5) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���S�񐔂Q�v       PIC 9(2) VALUE ZERO.
001420        07 �}�b�T�[�W�K��{�p���S�{�p���Q�v     PIC 9(6) VALUE ZERO.
      */20220828
001940        07 �ό`�k�苸���p�v.
001950           09 �ό`�k�苸���p�P���v              PIC 9(5) VALUE ZERO.
001960           09 �ό`�k�苸���p�񐔂v              PIC 9(3) VALUE ZERO.
000910     05 �Ǐ�v                                  PIC X(200) VALUE SPACE.

HILO*********************************************************************�ǉ����[�N


006256******************************************************************
006257*                          �A������                              *
006258******************************************************************
006259*
006260* ���S���擾�p14/10�`
006261 01 �A���|���S���擾�L�[ IS EXTERNAL.
006262    03 �A���|�{�p�a��N��.
006263       05 �A���|�{�p�a��               PIC 9.
006264       05 �A���|�{�p�N��.
006270          07 �A���|�{�p�N              PIC 9(2).
006280          07 �A���|�{�p��              PIC 9(2).
006290    03 �A���|���҃R�[�h.
006300       05 �A���|���Ҕԍ�               PIC 9(6).
006310       05 �A���|�}��                   PIC X.
006320    03 �A���|���ە��S��                PIC 9(3).
006330    03 �A���|���ۖ{�̕��S��            PIC 9(3).
006340    03 �A���|���ە��S��                PIC 9(3).
006350    03 �A���|�Q�V�V���S��              PIC 9(3).
006360    03 �A���|�������S��                PIC 9(3).
006370    03 �A���|���ʗp���S��              PIC 9(3).
006380*
006390 01 �A���|�L�[ IS EXTERNAL.
006400    03 �A���|���b�Z�[�W                PIC N(20).
006410*
006420*--------------------------------------------------------
006430*
006440*
006450* �v�Z�A�g�L�[
006460 01 �g�A�v�Z�|�L�[ IS EXTERNAL.
006470* / in�p�����^ /
006480   03 �g�A�v�Z�|�{�p�敪                   PIC 9.
006490   03 �g�A�v�Z�|�{�p�a��N����.
006500      05 �g�A�v�Z�|�{�p�a��N��.
006510        07 �g�A�v�Z�|�{�p�a��              PIC 9.
006520        07 �g�A�v�Z�|�{�p�N��.
006530          09 �g�A�v�Z�|�{�p�N              PIC 9(2).
006540          09 �g�A�v�Z�|�{�p��              PIC 9(2).
006550      05 �g�A�v�Z�|�{�p��                  PIC 9(2).
006560   03 �g�A�v�Z�|���҃R�[�h.
006570      05 �g�A�v�Z�|���Ҕԍ�                PIC 9(6).
006580      05 �g�A�v�Z�|�}��                    PIC X.
006590   03 �g�A�v�Z�|�ی����                   PIC 9(2).
006600* / �����p�����^/
006610   03 �g�A�v�Z�|�����p�����^.
006620      05 �g�A�v�Z�|���S���v�Z�敪          PIC 9.
006630      05 �g�A�v�Z�|��p�z                  PIC 9(6).
006640*  ��������"YES"(���S�敪1,2,3,4)
006650      05 �g�A�v�Z�|�������t���O            PIC X(3).
006660*  �����̏���Ȃ�"YES"(���S�敪5)
006670      05 �g�A�v�Z�|���f�Ãt���O            PIC X(3).
006680*  �������̋��z��]�L(���S�敪1)
006690      05 �g�A�v�Z�|������                  PIC 9(4).
006700*  �����̓����܂ł̎�����(�}�Ԃ��܂�)(���S�敪6,8,13)
006710      05 �g�A�v�Z�|������                  PIC 9(2).
006720*  �����É����c���p�B�����̕��S�̂���Ȃ�  �O�F�Ȃ��@�P�F���� (���S�敪7)
006730      05 �g�A�v�Z�|���̑��v�Z�敪�P        PIC 9.
006740*  out
006750      05 �g�A�v�Z�|���S�z                  PIC 9(6).
006760      05 �g�A�v�Z�|�����z                  PIC 9(6).
006770      05 �g�A�v�Z�|���S�z����              PIC 9(5).
006780      05 �g�A�v�Z�|�����z����              PIC 9(5).
006790      05 �g�A�v�Z�|���S��                  PIC 9(3).
006800*
      */�����z�����̂U��ver/20231117
001980  01 �g�A�v�Z�|�����z�����Q                PIC 9(6) IS EXTERNAL.
      *
      */���v���S�z�͏����ȉ���ʂ��l�̌ܓ�/20220601
006460 01 �g�A�v�Z�Q�|�L�[ IS EXTERNAL.
006620      05 �g�A�v�Z�Q�|���v�敪              PIC 9.
002010*
      */���S�z�؂�グ�p/20221125
001660 01 �g�A�v�Z�R�|�L�[ IS EXTERNAL.
001950    03 �g�A�v�Z�R�|���S�z                  PIC 9(6).
001970    03 �g�A�v�Z�R�|���S�z����              PIC 9(5).
006810**
006820*
006830*---------------------------------------------------------*
006840* ���ۏ؃t���O�p
006850 01 �g�A�v�Z���|�L�[ IS EXTERNAL.
006860   03 �g�A�v�Z���|���ۏ؃t���O         PIC X(3).
006870*
006880*---------------------------------------------------------*
006890* �������r���p
006900 01 �g�A�v�Z�������r���|�L�[ IS EXTERNAL.
006910   03 �g�A�v�Z�������r���|�������r���J�n��   PIC 9(2).
006920*---------------------------------------------------------*
006930*
006940*
006950*---------------------------------------------------------*
006960* �����݌v�p
006970 01 �g�A�v�Z�����݌v�z�|���S�z����       PIC 9(6) IS EXTERNAL.
006980 01 �g�A�v�Z�����݌v�z�|�������S���Ə�   PIC 9 IS EXTERNAL.
006990*---------------------------------------------------------*
007000*
007010*---------------------------------------------------------*
007020* ���×���������Z��p
007030*---------------------------------------------------------*
007040 01 �g�A�ŉ��|�L�[ IS EXTERNAL.
007050* 1:�I���A2:����܁E�}�b�T�[�W�A3:�����i�J�Ёj
007060    03 �g�A�ŉ��|�{�p�敪                      PIC 9(1).
007070    03 �g�A�ŉ��|�{�p�a��N��.
007080       05 �g�A�ŉ��|�{�p�a��                   PIC 9.
007090       05 �g�A�ŉ��|�{�p�N��.
007100          07 �g�A�ŉ��|�{�p�N                  PIC 9(2).
007110          07 �g�A�ŉ��|�{�p��                  PIC 9(2).
007120* ���Ë���
007130    03 �g�A�ŉ��|���Ë���                      PIC 9(2)V9.
007140* ���Ë��z�̏���ɊY���������H�@0:�Y������ 1:�Y������
007150    03 �g�A�ŉ��|�Y���敪                      PIC 9(1).
007160* ����̉��Ë��z�i���Ê�{��+���É��Z���̍��Z�j
007170    03 �g�A�ŉ��|���É��Z���z                  PIC 9(6).
007180* �����\������
007190    03 �g�A�ŉ��|���̑��P                      PIC 9(6).
007200    03 �g�A�ŉ��|���̑��Q                      PIC 9(6).
007210    03 �g�A�ŉ��|���̑��R                      PIC 9(6).
007220*
007230*/���S�敪�Q�Q(���{����30�N4���`)/20180307
007240 01 �A�|���S�z�����P�f�v               PIC 9(6) IS EXTERNAL.
007250 01 �A�|���S�z�����P�O�f�v             PIC 9(6) IS EXTERNAL.
007260 01 �A�|���S�z�����P�f�v�Q             PIC 9(6) IS EXTERNAL.
007270 01 �A�|���v���Z�e                     PIC 9    IS EXTERNAL.
007280*
007447******************************
007448* �O��{�p�񍐏���t���擾 *
007449******************************
007450 01 �A�O���|�L�[ IS EXTERNAL.
007451*/ in
007452*   ���͂͂g���Z�v�g�e�̎�L�[
007453    03 �A�O���|�{�p�敪                PIC 9.
007454    03 �A�O���|�{�p�a��N��.
007455       05 �A�O���|�{�p�a��             PIC 9.
007456       05 �A�O���|�{�p�N��.
007457         07 �A�O���|�{�p�N             PIC 9(2).
007458         07 �A�O���|�{�p��             PIC 9(2).
007459    03 �A�O���|���҃R�[�h.
007460       05 �A�O���|���Ҕԍ�             PIC 9(6).
007461       05 �A�O���|�}��                 PIC X.
007462    03 �A�O���|���Z���                PIC 9(2).
007463*
007464*/ out
007465    03 �A�O���|�O��x���a��N��.
007466       05 �A�O���|�O��x���a��         PIC 9.
007467       05 �A�O���|�O��x���N           PIC 9(2).
007468       05 �A�O���|�O��x����           PIC 9(2).
007469    03 �A�O���|��t�\�a��N��.
007470       05 �A�O���|��t�\�a��         PIC 9.
007471       05 �A�O���|��t�\�N           PIC 9(2).
007472       05 �A�O���|��t�\��           PIC 9(2).
007473*/  ( 1:�ό`�k�肠�� )
007474    03 �A�O���|�ό`�k��t���O          PIC 9(1).
007475*
      */��㏕���F���̏����1�x���z���Ȃ��ꍇ�A�{�̂̕��S�z�ƍ��������Ă��������Ȃ�/20220830
       01 �S�z���ҕ��S�e                     PIC X(3) IS EXTERNAL.
002664 01 �g�A�v�Z�S�|�L�[ IS EXTERNAL.
002666   03 �g�A�v�Z�S�|���ꌚ�����Ґ��擾�e     PIC 9.
      *
006259*
       01 �A���Q�O�P�|��ʏ�� IS EXTERNAL GLOBAL.
          03 �A���Q�O�P�|�{�p�敪               PIC 9(1).
          03 �A���Q�O�P�|�{�p�a��N����.
             05 �A���Q�O�P�|�{�p�a��N��.
                07 �A���Q�O�P�|�{�p�a��         PIC 9(1).
                07 �A���Q�O�P�|�{�p�N           PIC 9(2).
                07 �A���Q�O�P�|�{�p��           PIC 9(2).
             05 �A���Q�O�P�|�{�p��              PIC 9(2).
          03 �A���Q�O�P�|���҃R�[�h.
             05 �A���Q�O�P�|���Ҕԍ�            PIC 9(6).
             05 �A���Q�O�P�|�}��                PIC X(1).
          03 �A���Q�O�P�|�G���[�t���O           PIC X(3).
007476******************************************************************
007477*                      PROCEDURE  DIVISION                       *
007478******************************************************************
007479 PROCEDURE               DIVISION.
HILO***       DISPLAY "K0610:St--- ���҇�" �g�A�v�Z�|���҃R�[�h " �{�p��" �g�A�v�Z�|�{�p�a��N�� "---" �g�A�v�Z�|�{�p��
007482************
007483*           *
007484* ��������   *
007485*           *
007486************
007487     PERFORM ������.
007488     PERFORM �����f�[�^�擾.
007489     PERFORM ���S���擾.
007490     PERFORM �����f�[�^����.
           PERFORM ��̈ϔC���擾.
007491*
007492************
007493*           *
007494* �又��     *
007495*           *
007496************
007497     PERFORM ���v�f�[�^����.
007498*   ��L�̌�
007500     PERFORM ���Z�v�g����.
007500     PERFORM ���Z�v�g�ڍ׏���.
007510
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
007630 ������ SECTION.
007640*
007650*   / �����p�����^�̏����� /
007660     INITIALIZE �g�A�v�Z�|�����p�����^.
007670*
007680*   / ���ۏ؃t���O�p������ /
007690     INITIALIZE �g�A�v�Z���|�L�[.
007700*
007720     PERFORM �t�@�C���I�[�v��.
007730*
007740*------------------------------------------------------*
007750*   / �g�������� /
007760     MOVE ZERO TO �g���|����敪.
007770     READ �g������}�X�^
007780     NOT INVALID KEY
007790         MOVE �g���|�̎������S���v�Z�敪 TO �̎������S���v�Z�敪�v
007800     END-READ.
007810*------------------------------------------------------*
007820*    �t���O���N���A 2017/04/03
007830     MOVE SPACE TO �������S����t���O.
007840*================================================================*
007850 �t�@�C���I�[�v�� SECTION.
007860*
007870* EXTERNAL�p
007880     OPEN INPUT ��f�ҏ��e.
007890         MOVE NC"��f��" TO �t�@�C����.
007900         PERFORM �I�[�v���`�F�b�N.
007910     OPEN INPUT �ی��҃}�X�^.
007920         MOVE NC"�ی���" TO �t�@�C����.
007930         PERFORM �I�[�v���`�F�b�N.
007940     OPEN INPUT ���S���}�X�^.
007950         MOVE NC"���S��" TO �t�@�C����.
007960         PERFORM �I�[�v���`�F�b�N.
007970     OPEN INPUT �s�����}�X�^.
007980         MOVE NC"�s����" TO �t�@�C����.
007990         PERFORM �I�[�v���`�F�b�N.
008000     OPEN INPUT �ی��ғ��ʕ��S�}�X�^.
008010         MOVE NC"�ی��ғ��ʕ��S" TO �t�@�C����.
008020         PERFORM �I�[�v���`�F�b�N.
008030*
008040     OPEN INPUT �g�����}�X�^.
008050         MOVE NC"�g�����}�X�^" TO �t�@�C����.
008060         PERFORM �I�[�v���`�F�b�N.
008070     OPEN INPUT �Q�Ƃg���v�f�[�^�e.
008080         MOVE NC"�Q�Ƃg���v�f�[�^�e" TO �t�@�C����.
008090         PERFORM �I�[�v���`�F�b�N.
008100*     OPEN INPUT �g�����f�[�^�e.
008110*         MOVE NC"�g�����f�[�^�e" TO �t�@�C����.
008120*         PERFORM �I�[�v���`�F�b�N.
008130     OPEN I-O �g���v�f�[�^�e.
008140         MOVE NC"�g���v�f�[�^�e" TO �t�@�C����.
008150         PERFORM �I�[�v���`�F�b�N.
008160     OPEN I-O �g���Z�v�g�e.
008170         MOVE NC"�g���Z�v�g�e" TO �t�@�C����.
008180         PERFORM �I�[�v���`�F�b�N.
008190*
008200     OPEN INPUT ��v�̎��e.
008210         MOVE NC"��v�̎��e" TO �t�@�C����.
008220         PERFORM �I�[�v���`�F�b�N.
008230*
008240     OPEN INPUT �g������}�X�^.
008250         MOVE NC"�g����" TO �t�@�C����.
008260         PERFORM �I�[�v���`�F�b�N.
008270*
008280     OPEN INPUT �g���Î��тe.
008290         MOVE NC"�g���Î��тe" TO �t�@�C����.
008300         PERFORM �I�[�v���`�F�b�N.
008310*
004840     OPEN INPUT �g�{�p�����}�X�^.
004850         MOVE NC"�g�{�p��" TO �t�@�C����.
004860         PERFORM �I�[�v���`�F�b�N.
008310*
008320     OPEN I-O �X�V�g���Z�v�g�e.
008330         MOVE NC"�X�V�g���Z�v�g�e" TO �t�@�C����.
008340         PERFORM �I�[�v���`�F�b�N.
      *
008320     OPEN I-O �g���Z�v�g�ڍׂe.
           IF ��ԃL�[ = 35
008320         OPEN OUTPUT �g���Z�v�g�ڍׂe
           END-IF.
008330     MOVE NC"���Z�v�g�ڍׂe" TO �t�@�C����.
008340     PERFORM �I�[�v���`�F�b�N.
           IF �g�A�v�Z�S�|���ꌚ�����Ґ��擾�e = ZERO
008320         OPEN OUTPUT ��ƃt�@�C���P
008330             MOVE NC"��P" TO �t�@�C����
008340             PERFORM �I�[�v���`�F�b�N
           END-IF.
      *
008320     OPEN OUTPUT ��ƃt�@�C���Q
008330         MOVE NC"��Q" TO �t�@�C����
008340         PERFORM �I�[�v���`�F�b�N
           CLOSE ��ƃt�@�C���Q
008320     OPEN I-O ��ƃt�@�C���Q
008330         MOVE NC"��Q" TO �t�@�C����
008340         PERFORM �I�[�v���`�F�b�N
008310*
004840     OPEN INPUT �{�݃}�X�^.
004850         MOVE NC"�{��" TO �t�@�C����.
004860         PERFORM �I�[�v���`�F�b�N.
008310*
008320     OPEN INPUT �Q�Ƃg���Z�v�g�ڍׂe.
008330         MOVE NC"�Q�ƃ��Z�v�g�ڍׂe" TO �t�@�C����.
008340         PERFORM �I�[�v���`�F�b�N.
008310*
018000     OPEN INPUT �����}�X�^.
018010         MOVE NC"����" TO �t�@�C����.
018020         PERFORM �I�[�v���`�F�b�N.
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
008540     CLOSE ��f�ҏ��e �ی��҃}�X�^ ���S���}�X�^ �s�����}�X�^ �ی��ғ��ʕ��S�}�X�^.
008550     CLOSE �g���Z�v�g�e �g���v�f�[�^�e ��v�̎��e �Q�Ƃg���v�f�[�^�e �g�����}�X�^ �g������}�X�^.
008560     CLOSE �g���Î��тe �g�{�p�����}�X�^.
008570     CLOSE �X�V�g���Z�v�g�e.
           CLOSE �g���Z�v�g�ڍׂe �{�݃}�X�^.
           IF �g�A�v�Z�S�|���ꌚ�����Ґ��擾�e = ZERO
               CLOSE ��ƃt�@�C���P 
           END-IF.
           CLOSE  ��ƃt�@�C���Q �g���Z�v�g�ڍׂe �����}�X�^.
008580*================================================================*
008590 �I������ SECTION.
008600*
008610     PERFORM �t�@�C����.
008620*================================================================*
008640 ��̈ϔC���擾 SECTION.
      *
      */�ی��҂���̈ϔC�̎��A�{�p�҂���̈ϔC�o�^�����Ă��Ȃ��ꍇ���ҕ����ɂ���
      *
           MOVE ZERO  TO �{�p�Ҏ�̈ϔC�e.
           MOVE 41    TO �g�{��|�{�p���ԍ�.
           READ �g�{�p�����}�X�^
           NOT INVALID KEY
               EVALUATE �g�A�v�Z�|�{�p�敪
               WHEN 1
                   IF �g�{��S�P�|�o�^�L���ԍ��͂肫�イ NOT = SPACE
                       MOVE 1     TO �{�p�Ҏ�̈ϔC�e
                   END-IF
               WHEN 2
                   IF �g�{��S�P�|�o�^�L���ԍ��}�b�T�[�W NOT = SPACE
                       MOVE 1     TO �{�p�Ҏ�̈ϔC�e
                   END-IF
               END-EVALUATE
           END-READ.
           IF �{�p�Ҏ�̈ϔC�e = ZERO
               MOVE ZERO                 TO ��̈ϔC�e
011650         IF �����ʂv NOT = ZERO
                   MOVE �����ʂv       TO �s�|������
                   MOVE ��p���S�Ҕԍ��v TO �s�|�s�����ԍ�
                   READ �s�����}�X�^
                   NOT INVALID KEY
                       IF (�s�|��̈ϔC�J�n�a��N�� =  ZERO OR SPACE)
                           IF (�s�|��̈ϔC�敪   = 1)
                               MOVE 1      TO ��̈ϔC�e
                           END-IF
                       ELSE
                           IF (�s�|��̈ϔC�J�n�a��N�� <= �g�A�v�Z�|�{�p�a��N��)
                               IF (�s�|��̈ϔC�敪��   = 1)
                                   MOVE 1      TO ��̈ϔC�e
                               END-IF
                           ELSE
                               IF (�s�|��̈ϔC�敪   = 1)
                                   MOVE 1      TO ��̈ϔC�e
                               ELSE
                                   MOVE ZERO   TO ��̈ϔC�e
                               END-IF
                           END-IF
                       END-IF
                   END-READ
011670         ELSE
                   IF �ی���ʂv < 10
                       MOVE �ی���ʂv     TO �ہ|�ی����
                       MOVE �ی��Ҕԍ��v   TO �ہ|�ی��Ҕԍ�
                       READ �ی��҃}�X�^
                       NOT INVALID KEY
                           IF (�ہ|��̈ϔC�J�n�a��N�� =  ZERO OR SPACE)
                               IF (�ہ|��̈ϔC�敪   = 1)
                                   MOVE 1      TO ��̈ϔC�e
                               END-IF
                           ELSE
                               IF (�ہ|��̈ϔC�J�n�a��N�� <= �g�A�v�Z�|�{�p�a��N��)
                                   IF (�ہ|��̈ϔC�敪��   = 1)
                                       MOVE 1      TO ��̈ϔC�e
                                   END-IF
                               ELSE
                                   IF (�ہ|��̈ϔC�敪   = 1)
                                       MOVE 1      TO ��̈ϔC�e
                                   ELSE
                                       MOVE ZERO   TO ��̈ϔC�e
                                   END-IF
                               END-IF
                           END-IF
                       END-READ
                   END-IF
               END-IF
               IF ��̈ϔC�e = 1
                   INITIALIZE �X�V�g���Z�|���R�[�h
011500             MOVE �g�A�v�Z�|�{�p�敪  TO  �X�V�g���Z�|�{�p�敪
011510             MOVE �g�A�v�Z�|�{�p�a��  TO  �X�V�g���Z�|�{�p�a��
011520             MOVE �g�A�v�Z�|�{�p�N    TO  �X�V�g���Z�|�{�p�N
011530             MOVE �g�A�v�Z�|�{�p��    TO  �X�V�g���Z�|�{�p��
011540             MOVE �g�A�v�Z�|���Ҕԍ�  TO  �X�V�g���Z�|���Ҕԍ�
011550             MOVE �g�A�v�Z�|�}��      TO  �X�V�g���Z�|�}��
011560*
011650             IF �����ʂv NOT = ZERO
011660                 MOVE 2  TO �X�V�g���Z�|���Z���
011670             ELSE
                       IF �ی���ʂv < 10
011680                     MOVE 1  TO �X�V�g���Z�|���Z���
                       END-IF
011690             END-IF
                   READ �X�V�g���Z�v�g�e
                   NOT INVALID KEY
011740                 MOVE 1      TO �X�V�g���Z�|���ҕ����敪
                       REWRITE �X�V�g���Z�|���R�[�h
                       IF ��ԃL�[ NOT = "00"
                           MOVE NC"�X�V�g���Z" TO �t�@�C����
                           PERFORM �G���[�\��
                       END-IF
                   END-READ
               END-IF
011650         IF ������ʂv NOT = ZERO
                   MOVE ZERO                 TO ��̈ϔC�e
                   MOVE ������ʂv           TO �s�|������
                   MOVE ��p���S�Ҕԍ������v TO �s�|�s�����ԍ�
                   READ �s�����}�X�^
                   NOT INVALID KEY
                       IF (�s�|��̈ϔC�J�n�a��N�� =  ZERO OR SPACE)
                           IF (�s�|��̈ϔC�敪   = 1)
                               MOVE 1      TO ��̈ϔC�e
                           END-IF
                       ELSE
                           IF (�s�|��̈ϔC�J�n�a��N�� <= �g�A�v�Z�|�{�p�a��N��)
                               IF (�s�|��̈ϔC�敪��   = 1)
                                   MOVE 1      TO ��̈ϔC�e
                               END-IF
                           ELSE
                               IF (�s�|��̈ϔC�敪   = 1)
                                   MOVE 1      TO ��̈ϔC�e
                               ELSE
                                   MOVE ZERO   TO ��̈ϔC�e
                               END-IF
                           END-IF
                       END-IF
                   END-READ
                   IF ��̈ϔC�e = 1
                       INITIALIZE �X�V�g���Z�|���R�[�h
011500                 MOVE �g�A�v�Z�|�{�p�敪  TO  �X�V�g���Z�|�{�p�敪
011510                 MOVE �g�A�v�Z�|�{�p�a��  TO  �X�V�g���Z�|�{�p�a��
011520                 MOVE �g�A�v�Z�|�{�p�N    TO  �X�V�g���Z�|�{�p�N
011530                 MOVE �g�A�v�Z�|�{�p��    TO  �X�V�g���Z�|�{�p��
011540                 MOVE �g�A�v�Z�|���Ҕԍ�  TO  �X�V�g���Z�|���Ҕԍ�
011550                 MOVE �g�A�v�Z�|�}��      TO  �X�V�g���Z�|�}��
011660                 MOVE 3                   TO  �X�V�g���Z�|���Z���
                       READ �X�V�g���Z�v�g�e
                       NOT INVALID KEY
011740                     MOVE 1      TO �X�V�g���Z�|���ҕ����敪
                           REWRITE �X�V�g���Z�|���R�[�h
                           IF ��ԃL�[ NOT = "00"
                               MOVE NC"�X�V�g���Z" TO �t�@�C����
                               PERFORM �G���[�\��
                           END-IF
                       END-READ
                   END-IF
               END-IF
           END-IF.
      *
008630*================================================================*
008640 �����f�[�^�擾 SECTION.
008650*
008660*   / ������ /
008670     INITIALIZE �������v.
008680*/���S�敪�Q�Q(���{����30�N4���`)/20180306
008690     INITIALIZE �A�|���S�z�����P�f�v �A�|���S�z�����P�O�f�v �A�|���S�z�����P�f�v�Q.
008700*
008710*   /  �敪�R�[�h:1�i���ہj
008720     MOVE 1                   TO  �g���P�|�敪�R�[�h.
008730     MOVE �g�A�v�Z�|�{�p�a��  TO  �g���P�|�J�n�a��.
008740     MOVE �g�A�v�Z�|�{�p�N    TO  �g���P�|�J�n�N.
008750     MOVE �g�A�v�Z�|�{�p��    TO  �g���P�|�J�n��.
008760*
008770     START �g�����}�X�^ KEY IS <= �g���P�|�敪�R�[�h �g���P�|�J�n�a��N��
008780                                  REVERSED
008790     END-START.
008800*
008810     IF ��ԃL�[ = "00"
008820         MOVE SPACE TO �I���t���O�Q
008830         PERFORM �g�����}�X�^�Ǎ�
008840         IF ( �g���P�|�敪�R�[�h = 1 ) AND
008850            ( �I���t���O�Q       = SPACE )
008860*         / �ޔ�
008870              MOVE �g���P�|�g�P�p����  TO �g�P�p����v
008880              MOVE �g���P�|�g�Q�p����  TO �g�Q�p����v
008890              MOVE �g���P�|�g�P�p      TO �g�P�p�v
008900              MOVE �g���P�|�g�Q�p      TO �g�Q�p�v
008910              MOVE �g���P�|�g�d�×�    TO �g�d�×��v
008920              MOVE �g���P�|�g���Ê�{����    TO �g���Ê�{�����v
008930              MOVE �g���P�|�g���Òǉ�����    TO �g���Òǉ������v
008940              MOVE �g���P�|�l�P�Ǐ�    TO �l�P�Ǐ��v
008950              MOVE �g���P�|�l�ό`�k�苸���p  TO �l�ό`�k�苸���p�v
008960              MOVE �g���P�|�l��㪖@��  TO �l��㪖@���v
008970              MOVE �g���P�|�l�d�×�    TO �l�d�×��v
008980              MOVE �g���P�|�l���Ê�{����    TO �l���Ê�{�����v
008990              MOVE �g���P�|�l���Òǉ�����    TO �l���Òǉ������v
009000*
009010              MOVE �g���P�|���Ê�{����      TO ���Ê�{����
009020              MOVE �g���P�|�{�p�񍐏���t��  TO �{�p�񍐏���t���P���v
009023*
      */�ߘa6�N10���ȍ~������/*20240618---------------------------------------
000331              MOVE �g���P�|�g�P�p�K��{�p���P TO �g�P�p�K��{�p���P�v
000332              MOVE �g���P�|�g�Q�p�K��{�p���P TO �g�Q�p�K��{�p���P�v
000331              MOVE �g���P�|�g�P�p�K��{�p���Q TO �g�P�p�K��{�p���Q�v
000332              MOVE �g���P�|�g�Q�p�K��{�p���Q TO �g�Q�p�K��{�p���Q�v
000331              MOVE �g���P�|�g�P�p�K��{�p���R TO �g�P�p�K��{�p���R�v
000332              MOVE �g���P�|�g�Q�p�K��{�p���R TO �g�Q�p�K��{�p���R�v
000331              MOVE �g���P�|�g�P�p�K��{�p���S TO �g�P�p�K��{�p���S�v
000332              MOVE �g���P�|�g�Q�p�K��{�p���S TO �g�Q�p�K��{�p���S�v
      *
008870              COMPUTE �P�p�������v = �g���P�|�g�P�p���� - �g���P�|�g�P�p
008880              COMPUTE �Q�p�������v = �g���P�|�g�Q�p���� - �g���P�|�g�Q�p
      */�}�b�T�[�W
000339              MOVE �g���P�|�l�P�Ǐ��{�p��            TO �l�P�Ǐ��{�p���v
000339              MOVE �g���P�|�l�Q�Ǐ��{�p��            TO �l�Q�Ǐ��{�p���v
000339              MOVE �g���P�|�l�R�Ǐ��{�p��            TO �l�R�Ǐ��{�p���v
000339              MOVE �g���P�|�l�S�Ǐ��{�p��            TO �l�S�Ǐ��{�p���v
000339              MOVE �g���P�|�l�T�Ǐ��{�p��            TO �l�T�Ǐ��{�p���v
000339              MOVE �g���P�|�l�P�Ǐ��K��{�p���P      TO �l�P�Ǐ��K��{�p���P�v
000339              MOVE �g���P�|�l�Q�Ǐ��K��{�p���P      TO �l�Q�Ǐ��K��{�p���P�v
000339              MOVE �g���P�|�l�R�Ǐ��K��{�p���P      TO �l�R�Ǐ��K��{�p���P�v
000339              MOVE �g���P�|�l�S�Ǐ��K��{�p���P      TO �l�S�Ǐ��K��{�p���P�v
000339              MOVE �g���P�|�l�T�Ǐ��K��{�p���P      TO �l�T�Ǐ��K��{�p���P�v
000339              MOVE �g���P�|�l�P�Ǐ��K��{�p���Q      TO �l�P�Ǐ��K��{�p���Q�v
000339              MOVE �g���P�|�l�Q�Ǐ��K��{�p���Q      TO �l�Q�Ǐ��K��{�p���Q�v
000339              MOVE �g���P�|�l�R�Ǐ��K��{�p���Q      TO �l�R�Ǐ��K��{�p���Q�v
000339              MOVE �g���P�|�l�S�Ǐ��K��{�p���Q      TO �l�S�Ǐ��K��{�p���Q�v
000339              MOVE �g���P�|�l�T�Ǐ��K��{�p���Q      TO �l�T�Ǐ��K��{�p���Q�v
000339              MOVE �g���P�|�l�P�Ǐ��K��{�p���R      TO �l�P�Ǐ��K��{�p���R�v
000339              MOVE �g���P�|�l�Q�Ǐ��K��{�p���R      TO �l�Q�Ǐ��K��{�p���R�v
000339              MOVE �g���P�|�l�R�Ǐ��K��{�p���R      TO �l�R�Ǐ��K��{�p���R�v
000339              MOVE �g���P�|�l�S�Ǐ��K��{�p���R      TO �l�S�Ǐ��K��{�p���R�v
000339              MOVE �g���P�|�l�T�Ǐ��K��{�p���R      TO �l�T�Ǐ��K��{�p���R�v
000339              MOVE �g���P�|�l�P�Ǐ��K��{�p���S      TO �l�P�Ǐ��K��{�p���S�v
000339              MOVE �g���P�|�l�Q�Ǐ��K��{�p���S      TO �l�Q�Ǐ��K��{�p���S�v
000339              MOVE �g���P�|�l�R�Ǐ��K��{�p���S      TO �l�R�Ǐ��K��{�p���S�v
000339              MOVE �g���P�|�l�S�Ǐ��K��{�p���S      TO �l�S�Ǐ��K��{�p���S�v
000339              MOVE �g���P�|�l�T�Ǐ��K��{�p���S      TO �l�T�Ǐ��K��{�p���S�v
                    MOVE �g���P�|���ʒn����Z��            TO ���l���ʒn����Z���v

HILO  *     DISPLAY "610-51 �g���P�|���ʒn����Z��" �g���P�|���ʒn����Z��

      */�ߘa6�N10���ȍ~������/*20240618---------------------------------------
      *
009030         ELSE
009040             MOVE  NC"�@�Y���̎{�p�N���̗�����������܂���B" TO �A���|���b�Z�[�W
009050             CALL   "MSG001"
009060             CANCEL "MSG001"
009070         END-IF
009080     ELSE
009090         MOVE  NC"�@�Y���̎{�p�N���̗�����������܂���B" TO �A���|���b�Z�[�W
009100         CALL   "MSG001"
009110         CANCEL "MSG001"
009120     END-IF.
009130*
009140*================================================================*
009150 �g�����}�X�^�Ǎ� SECTION.
009160*
009170     READ �g�����}�X�^ NEXT
009180     AT END
009190         MOVE "YES"  TO �I���t���O�Q
009200     END-READ.
009210*
009394*================================================================*
009395 ���S���擾 SECTION.
009396*
009397     MOVE SPACE TO �A���|���S���擾�L�[.
009398     INITIALIZE �A���|���S���擾�L�[.
009399     MOVE �g�A�v�Z�|�{�p�a��N�� TO �A���|�{�p�a��N��.
009400     MOVE �g�A�v�Z�|���҃R�[�h   TO �A���|���҃R�[�h.
009401*
009402     CALL   "HUTANRIT".
009403     CANCEL "HUTANRIT".
009404*
009435*================================================================*
009436 �����f�[�^���� SECTION.
009437*----------------------------------------------------------*
009438* ���S�敪:6,8,14 (���������z�A���񐔂𕉒S)
009439*          13  (���������z�A���񐔂𕉒S�i�P�O�~�P�ʁj)
009440*          ���V�l���S���Ə��Ώێ�
009441*          5,16�i�݌v�j
009442*----------------------------------------------------------*
009443* ���ꕉ�S�F�����z�����񐔂܂ŕ��S�ɑΉ����邽�߂̏���
009444* �}�Ԃ��܂߂��ʉ@�e�[�u�����쐬����B
009445*----------------------------------------------------------*
009446*
009450*    / �ʉ@�e�[�u��ZERO�N���A�[ /
009460     MOVE ZERO  TO �ʉ@���b�v.
009470     MOVE ZERO  TO �ʉ@���Q�b�v.
009480     MOVE SPACE TO ���ۏ؃t���O.
009490*
009500*    / �������r���p�N���A�[ /
009510     MOVE ZERO TO �������r���J�n���v.
009520     INITIALIZE �g�A�v�Z�������r���|�L�[.
009530
009540*   / �����݌v�p������ /
009550     MOVE ZERO TO ���S�敪�v.
009560     MOVE ZERO TO �}�ԕ��S�݌v�z�v.
009570     MOVE ZERO TO �g�A�v�Z�����݌v�z�|���S�z����.
009580     MOVE ZERO TO �g�A�v�Z�����݌v�z�|�������S���Ə�.
009590*
009610     MOVE �g�A�v�Z�|�{�p�a�� TO ��|�{�p�a��.
009620     MOVE �g�A�v�Z�|�{�p�N   TO ��|�{�p�N.
009630     MOVE �g�A�v�Z�|�{�p��   TO ��|�{�p��.
009640     MOVE �g�A�v�Z�|���Ҕԍ� TO ��|���Ҕԍ�.
009650     MOVE �g�A�v�Z�|�}��     TO ��|�}��.
009660     READ ��f�ҏ��e
009670     NOT INVALID KEY
009680*       / �ޔ� /
009690        MOVE ��|������ TO �����ʂv
009700        MOVE ��|������� TO ������ʂv
009710        MOVE ��|�ی���� TO �ی���ʂv
009720        MOVE ��|��p���S�Ҕԍ����� TO ��p���S�Ҕԍ������v
009720        MOVE ��|��p���S�Ҕԍ�     TO ��p���S�Ҕԍ��v
009720        MOVE ��|�ی��Ҕԍ�         TO �ی��Ҕԍ��v
009730*
009740*------------------------------------------------------*
009750* ���V�l���S���Ə��Ώێ҂�
009760**        IF (��|�{�p�a��N�� >= 41611)
009770        IF ( ��|�{�p�a��N�� >= 41611 ) AND ( ��|�{�p�a��N�� <= 42003 )
009780            IF (( ��|������ = ZERO )    AND  ( ��|������� = 51 ) AND
009790                ( ��|�V�l���S���Ə� = 1 ) AND
009800                ( ��|��p���S�Ҕԍ�����(3:2) = "27" ))
009810              OR
009820               (( ��|������ = 05 ) AND ( ��|�V�l���S���Ə� = 1 ) AND
009830                ( ��|��p���S�Ҕԍ�(3:2) = "27" ))
009840              OR
009850               (( ��|������ = 05 ) AND ( ��|�������S���Ə� = 1 ) AND
009860                ( ��|��p���S�Ҕԍ�(3:2) = "27" ))
009870*
009880                MOVE "YES" TO ���ۏ؃t���O
009890            END-IF
009900        END-IF
009910*
009920*------------------------------------------------------*
009930* �����̌��r���J�n
009940        IF ( ��|�ی����� = 1 ) AND ( ��|������� NOT = ZERO ) AND ( ��|��p���S�Ҕԍ����� NOT = SPACE ) AND
009950           ( ��|�������r���J�n�� NOT = ZERO ) AND ( ��|�������r���J�n�� NOT = SPACE )
009960           MOVE ��|�������r���J�n�� TO �������r���J�n���v
009970           MOVE ��|�������r���J�n�� TO �g�A�v�Z�������r���|�������r���J�n��
009980        END-IF
009990*------------------------------------------------------*
010000*
010010*------------------------------------------------------*
010020* ��L��A�����}�Ԃ̗݌v�����i���S�敪5,16�p�j
010030        IF ��|������� NOT = ZERO
010040             MOVE ��|�������S���Ə� TO �g�A�v�Z�����݌v�z�|�������S���Ə�
010050             PERFORM �������S�敪�擾
010060             IF ( ��|�}�ԍ쐬���}�� NOT = SPACE ) AND ( ���S�敪�v = 05 OR 16 )
010070                IF �������r���J�n���v = ZERO
010080                   PERFORM �}�ԕ��S�݌v�z�v�Z
010090                END-IF
010100             END-IF
010110        END-IF
010120*------------------------------------------------------*
010130*
010140     END-READ.
010150**
010160**
010170     IF ( ������ʂv NOT = ZERO ) OR ( ���ۏ؃t���O = "YES" )
010180        MOVE �g�A�v�Z�|�{�p�a�� TO ��|�{�p�a��
010190        MOVE �g�A�v�Z�|�{�p�N   TO ��|�{�p�N
010200        MOVE �g�A�v�Z�|�{�p��   TO ��|�{�p��
010210        MOVE �g�A�v�Z�|���Ҕԍ� TO ��|���Ҕԍ�
010220        MOVE SPACE              TO ��|�}��
010230        START ��f�ҏ��e KEY IS >= ��|�{�p�a��N��
010240                                     ��|���҃R�[�h
010250        END-START
010260        IF ��ԃL�[ = "00"
010270            MOVE SPACE TO �I���t���O�R
010280            PERFORM ��f�ҏ��e�Ǎ�
010290            PERFORM UNTIL ( �I���t���O�R         NOT = SPACE ) OR
010300                          ( �g�A�v�Z�|�{�p�a��   NOT = ��|�{�p�a�� ) OR
010310                          ( �g�A�v�Z�|�{�p�N     NOT = ��|�{�p�N ) OR
010320                          ( �g�A�v�Z�|�{�p��     NOT = ��|�{�p�� ) OR
010330                          ( �g�A�v�Z�|���Ҕԍ�   NOT = ��|���Ҕԍ� )
010340*---*
010350**             ���ۂ̂ݑΏ�
010360               IF ��|�ی����� = 1
010370*
010380*                  / ���ꊳ�Ҕԍ��A�N���̒ʉ@���Z�b�g �͂肫�イ�ƃ}�b�T�[�W���� /
010390*----------------------------------------------------------------------------------*
010400*                  / �@�͂肫�イ /
010410                   MOVE 1             TO �Q�Ƃg���|�{�p�敪
010420                   MOVE ��|���Ҕԍ�  TO �Q�Ƃg���|���Ҕԍ�
010430                   MOVE ��|�}��      TO �Q�Ƃg���|�}��
010440                   MOVE ��|�{�p�a��  TO �Q�Ƃg���|�{�p�a��
010450                   MOVE ��|�{�p�N    TO �Q�Ƃg���|�{�p�N
010460                   MOVE ��|�{�p��    TO �Q�Ƃg���|�{�p��
010470                   MOVE 1             TO �Q�Ƃg���|�{�p��
010480*
010490                   START �Q�Ƃg���v�f�[�^�e KEY IS >= �Q�Ƃg���|�{�p�敪
010500                                                      �Q�Ƃg���|���҃R�[�h
010510                                                      �Q�Ƃg���|�{�p�a��N����
010520                   END-START
010530                   IF ��ԃL�[ = "00"
010540                       MOVE SPACE TO �I���t���O
010550                       PERFORM �Q�Ƃg���v�f�[�^�e�Ǎ�
010560*                      �J��Ԃ��i�����j
010570                       PERFORM UNTIL ( �Q�Ƃg���|�{�p�敪   NOT = 1              ) OR
010580                                     ( �Q�Ƃg���|���҃R�[�h NOT = ��|���҃R�[�h ) OR
010590                                     ( �Q�Ƃg���|�{�p�a��   NOT = ��|�{�p�a��   ) OR
010600                                     ( �Q�Ƃg���|�{�p�N     NOT = ��|�{�p�N     ) OR
010610                                     ( �Q�Ƃg���|�{�p��     NOT = ��|�{�p��     ) OR
010620                                     ( �I���t���O       NOT = SPACE )
010630*
010640                               EVALUATE �ʉ@���v(�Q�Ƃg���|�{�p��)
010650                               WHEN 1
010660                                  MOVE 1 TO �ʉ@���v(�Q�Ƃg���|�{�p��)
010670                               WHEN 2
010680                                  MOVE 3 TO �ʉ@���v(�Q�Ƃg���|�{�p��)
010690                               WHEN 3
010700                                  MOVE 3 TO �ʉ@���v(�Q�Ƃg���|�{�p��)
010710                               WHEN OTHER
010720                                  MOVE 1 TO �ʉ@���v(�Q�Ƃg���|�{�p��)
010730                               END-EVALUATE
010740*
010750*                              / �I���E�}�ʂ̒ʉ@�� /
010760                               IF �g�A�v�Z�|�{�p�敪 = 1
010770                                  MOVE 1 TO �ʉ@���Q�v(�Q�Ƃg���|�{�p��)
010780                               END-IF
010801*
010802                               PERFORM �Q�Ƃg���v�f�[�^�e�Ǎ�
010810                       END-PERFORM
010820                   END-IF
010830*----------------------------------------------------------------------------------*
010840*                  / �A�}�b�T�[�W /
010850                   MOVE 2             TO �Q�Ƃg���|�{�p�敪
010860                   MOVE ��|���Ҕԍ�  TO �Q�Ƃg���|���Ҕԍ�
010870                   MOVE ��|�}��      TO �Q�Ƃg���|�}��
010880                   MOVE ��|�{�p�a��  TO �Q�Ƃg���|�{�p�a��
010890                   MOVE ��|�{�p�N    TO �Q�Ƃg���|�{�p�N
010900                   MOVE ��|�{�p��    TO �Q�Ƃg���|�{�p��
010910                   MOVE 1             TO �Q�Ƃg���|�{�p��
010920*
010930                   START �Q�Ƃg���v�f�[�^�e KEY IS >= �Q�Ƃg���|�{�p�敪
010940                                                      �Q�Ƃg���|���҃R�[�h
010950                                                      �Q�Ƃg���|�{�p�a��N����
010960                   END-START
010970                   IF ��ԃL�[ = "00"
010980                       MOVE SPACE TO �I���t���O
010990                       PERFORM �Q�Ƃg���v�f�[�^�e�Ǎ�
011000*                      �J��Ԃ��i�����j
011010                       PERFORM UNTIL ( �Q�Ƃg���|�{�p�敪   NOT = 2              ) OR
011020                                     ( �Q�Ƃg���|���҃R�[�h NOT = ��|���҃R�[�h ) OR
011030                                     ( �Q�Ƃg���|�{�p�a��   NOT = ��|�{�p�a��   ) OR
011040                                     ( �Q�Ƃg���|�{�p�N     NOT = ��|�{�p�N     ) OR
011050                                     ( �Q�Ƃg���|�{�p��     NOT = ��|�{�p��     ) OR
011060                                     ( �I���t���O       NOT = SPACE )
011070*
011080                               EVALUATE �ʉ@���v(�Q�Ƃg���|�{�p��)
011090                               WHEN 1
011100                                  MOVE 3 TO �ʉ@���v(�Q�Ƃg���|�{�p��)
011110                               WHEN 2
011120                                  MOVE 2 TO �ʉ@���v(�Q�Ƃg���|�{�p��)
011130                               WHEN 3
011140                                  MOVE 3 TO �ʉ@���v(�Q�Ƃg���|�{�p��)
011150                               WHEN OTHER
011160                                  MOVE 2 TO �ʉ@���v(�Q�Ƃg���|�{�p��)
011170                               END-EVALUATE
011180*
011190*                              / �I���E�}�ʂ̒ʉ@�� /
011200                               IF �g�A�v�Z�|�{�p�敪 = 2
011210                                  MOVE 1 TO �ʉ@���Q�v(�Q�Ƃg���|�{�p��)
011220                               END-IF
011235*
011240                               PERFORM �Q�Ƃg���v�f�[�^�e�Ǎ�
011250                       END-PERFORM
011260                   END-IF
011270*----------------------------------------------------------------------------------*
011280               END-IF
011290*---*
011300               PERFORM ��f�ҏ��e�Ǎ�
011310**
011320            END-PERFORM
011330        END-IF
011340*
011350     END-IF.
011360*
011370*================================================================*
011380 ��f�ҏ��e�Ǎ� SECTION.
011390*
011400     READ ��f�ҏ��e NEXT
011410     AT END
011420         MOVE "YES" TO �I���t���O�R
011430     END-READ.
011440*================================================================*
011450*================================================================*
011460 ���Z�e�ŗL���擾 SECTION.
011470*
011480     INITIALIZE �v�Z�g�����v.
011490*
011500     MOVE �g�A�v�Z�|�{�p�敪  TO  �g���Z�|�{�p�敪.
011510     MOVE �g�A�v�Z�|�{�p�a��  TO  �g���Z�|�{�p�a��.
011520     MOVE �g�A�v�Z�|�{�p�N    TO  �g���Z�|�{�p�N.
011530     MOVE �g�A�v�Z�|�{�p��    TO  �g���Z�|�{�p��.
011540     MOVE �g�A�v�Z�|���Ҕԍ�  TO  �g���Z�|���Ҕԍ�.
011550     MOVE �g�A�v�Z�|�}��      TO  �g���Z�|�}��.
011560*
011570* �ی���ʁ@85:���ہA91:�ی��ؖY����v�Z�Ώۂł����ɂ���
011580* ���Z��� 1:���,2:�V�l,3:����,4:�J��,5:������,6:����,7:���ےP��,8�ی��ؖY�� /
011590     EVALUATE �ی���ʂv
011600     WHEN 85
011610        MOVE 7  TO �g���Z�|���Z���
011620     WHEN 91
011630        MOVE 8  TO �g���Z�|���Z���
011640     WHEN OTHER
011650        IF �����ʂv = ZERO
011660           MOVE 1  TO �g���Z�|���Z���
011670        ELSE
011680           MOVE 2  TO �g���Z�|���Z���
011690        END-IF
011700     END-EVALUATE.
011710*
011720     READ �g���Z�v�g�e
011730     NOT INVALID KEY
011740         MOVE �g���Z�|���ҕ����敪        TO �{�̏��ҕ����敪�v
011750         MOVE �g���Z�|�{�̂܂Ƃߋ敪      TO �{�̂܂Ƃߋ敪�v
011760*        / �}�b�T�[�W�ŗL���ڑޔ� /
011770         IF �g�A�v�Z�|�{�p�敪 = 2
011780            MOVE �g���Z�|�}�b�T�[�W��     TO �}�b�T�[�W�Ǐ����v
011790            MOVE �g���Z�|�ό`�k�苸���p�� TO �ό`�k�苸���p���v
011800            MOVE �g���Z�|�}�b�T�[�W��     TO �i�}�b�T�[�W�Ǐ����v
011810            MOVE �g���Z�|�ό`�k�苸���p�� TO �i�ό`�k�苸���p���v
011820
011830*           ���ӏ��̎{�p����
011840            MOVE �g���Z�|�}�b�T�[�W�̊�       TO �}�b�T�[�W�̊����ӂv
011850            MOVE �g���Z�|�}�b�T�[�W�E�㎈     TO �}�b�T�[�W�E�㎈���ӂv
011860            MOVE �g���Z�|�}�b�T�[�W���㎈     TO �}�b�T�[�W���㎈���ӂv
011870            MOVE �g���Z�|�}�b�T�[�W�E����     TO �}�b�T�[�W�E�������ӂv
011880            MOVE �g���Z�|�}�b�T�[�W������     TO �}�b�T�[�W���������ӂv
011890            MOVE �g���Z�|�ό`�k�苸���p�E�㎈ TO �ό`�k�苸���p�E�㎈���ӂv
011900            MOVE �g���Z�|�ό`�k�苸���p���㎈ TO �ό`�k�苸���p���㎈���ӂv
011910            MOVE �g���Z�|�ό`�k�苸���p�E���� TO �ό`�k�苸���p�E�������ӂv
011920            MOVE �g���Z�|�ό`�k�苸���p������ TO �ό`�k�苸���p���������ӂv
011930         END-IF
011940
011941*        �O��{�p�񍐏���t���擾
011942         PERFORM �O���t���擾
011943     END-READ.
011950**
011960**
011970     IF ������ʂv NOT = ZERO
011980        MOVE �g�A�v�Z�|�{�p�敪  TO  �g���Z�|�{�p�敪
011990        MOVE �g�A�v�Z�|�{�p�a��  TO  �g���Z�|�{�p�a��
012000        MOVE �g�A�v�Z�|�{�p�N    TO  �g���Z�|�{�p�N
012010        MOVE �g�A�v�Z�|�{�p��    TO  �g���Z�|�{�p��
012020        MOVE �g�A�v�Z�|���Ҕԍ�  TO  �g���Z�|���Ҕԍ�
012030        MOVE �g�A�v�Z�|�}��      TO  �g���Z�|�}��
012040        MOVE 3                   TO  �g���Z�|���Z���
012050        READ �g���Z�v�g�e
012060        NOT INVALID KEY
012070            MOVE �g���Z�|���ҕ����敪  TO �������ҕ����敪�v
012080        END-READ
012090     END-IF.
012124*
012125*================================================================*
012126 �O���t���擾 SECTION.
012127*
012128     INITIALIZE �A�O���|�L�[ �O���t���v.
012129*
012130     MOVE �g���Z�|�{�p�敪               TO  �A�O���|�{�p�敪.
012131     MOVE �g���Z�|�{�p�a��N��           TO  �A�O���|�{�p�a��N��.
012132     MOVE �g���Z�|���҃R�[�h             TO  �A�O���|���҃R�[�h.
012133     MOVE �g���Z�|���Z���               TO  �A�O���|���Z���.
012135     CALL   �v���O�����v.
012136     CANCEL �v���O�����v.
012137     MOVE �A�O���|�O��x���a��N��       TO �O���t�a��N���v.
012138     MOVE �A�O���|��t�\�a��N��       TO ��t�\�a��N���v.
012139     MOVE �A�O���|�ό`�k��t���O         TO �ό`�k��t���O�v.
012142*
012143*================================================================*
012144*================================================================*
012145 ���v�f�[�^���� SECTION.
012146*
HILO  *     DISPLAY "K5-2 " �g�A�v�Z�|�{�p�敪 " " �g�A�v�Z�|���҃R�[�h  " " 
HILO  *                     �g�A�v�Z�|�{�p�a��N�� " ���v�f�[�^���� " �K��d���e *>HILO

012150*   / ������ /
012160     INITIALIZE �݌v���v.
012170     INITIALIZE �i�݌v���v.
012180     INITIALIZE �}�b�T�[�W�ŗL�v.
012190     INITIALIZE �i�}�b�T�[�W�ŗL�v.
006460     INITIALIZE �g�A�v�Z�Q�|�L�[.
           INITIALIZE �g���Z�v�g�ڍׂe�v.
012200     MOVE ZERO                TO  �}�b�T�[�W�����v�v.
012210     MOVE ZERO                TO  �ό`�k�藿���v�v.
      */��㏕���F���̏����1�x���z���Ȃ��ꍇ�A�{�̂̕��S�z�ƍ��������Ă��������Ȃ�/20220830
           MOVE "YES" TO �S�z���ҕ��S�e. *>HILO
      *
HILO****/���ʒn��̂��߁H
HILO***006500     MOVE �g�A�v�Z�|�{�p�a��N�� TO ��|�{�p�a��N��
HILO***006560     MOVE �g�A�v�Z�|���҃R�[�h   TO ��|���҃R�[�h
HILO***           READ ��f�ҏ��e
HILO***012060     INVALID KEY
HILO***               MOVE SPACE TO ��|���R�[�h
HILO***012060     NOT INVALID KEY
HILO***012070         CONTINUE
HILO***012080     END-READ.
012220*
012230*    �Ђƌ��̊Ԃɓ���{�p���P��ł�����Γ���{�p�Ōv�Z����
012240     PERFORM ����{�p�L�����菈��.
012250*
012260*    �����̎{�p���e�Ɨ������v�Z����
012270     MOVE �g�A�v�Z�|�{�p�敪  TO  �g���|�{�p�敪.
012280     MOVE �g�A�v�Z�|���Ҕԍ�  TO  �g���|���Ҕԍ�.
012290     MOVE �g�A�v�Z�|�}��      TO  �g���|�}��.
012300     MOVE �g�A�v�Z�|�{�p�a��  TO  �g���|�{�p�a��.
012310     MOVE �g�A�v�Z�|�{�p�N    TO  �g���|�{�p�N.
012320     MOVE �g�A�v�Z�|�{�p��    TO  �g���|�{�p��.
012330     MOVE 1                   TO  �g���|�{�p��.
012340*
012350     INITIALIZE �}�b�T�[�W���ʕʋ��z���v.
012360     INITIALIZE �i�}�b�T�[�W���ʕʋ��z���v.
012370*
012380     START �g���v�f�[�^�e KEY IS >= �g���|�{�p�敪
012390                                    �g���|���҃R�[�h
012400                                    �g���|�{�p�a��N����
012410     END-START.
012420*
012430     IF ��ԃL�[ = "00"
012440         MOVE SPACE TO �I���t���O
012450         PERFORM �g���v�f�[�^�e�Ǎ�
012460*
012470         IF ( �g���|�{�p�敪    = �g�A�v�Z�|�{�p�敪   ) AND
012480            ( �g���|���҃R�[�h  = �g�A�v�Z�|���҃R�[�h ) AND
012490            ( �g���|�{�p�a��    = �g�A�v�Z�|�{�p�a��   ) AND
012500            ( �g���|�{�p�N      = �g�A�v�Z�|�{�p�N     ) AND
012510            ( �g���|�{�p��      = �g�A�v�Z�|�{�p��     ) AND
012520            ( �I���t���O        = SPACE )
012530            PERFORM ���Z�e�ŗL���擾
012540         END-IF
012550*
012560*        �J��Ԃ��i�����j
012570         PERFORM UNTIL ( �g���|�{�p�敪   NOT = �g�A�v�Z�|�{�p�敪   ) OR
012580                       ( �g���|���҃R�[�h NOT = �g�A�v�Z�|���҃R�[�h ) OR
012590                       ( �g���|�{�p�a��   NOT = �g�A�v�Z�|�{�p�a��   ) OR
012600                       ( �g���|�{�p�N     NOT = �g�A�v�Z�|�{�p�N     ) OR
012610                       ( �g���|�{�p��     NOT = �g�A�v�Z�|�{�p��     ) OR
012620                       ( �I���t���O       NOT = SPACE )
                   IF �g�A�v�Z�|�{�p�a��N�� >= 50610 
                       PERFORM ���ꌚ�����Ґ��擾
                   END-IF
HILO***       IF �g�A�v�Z�S�|���ꌚ�����Ґ��擾�e = ZERO
HILO***       DISPLAY "610-11 ������������ ���҇�" �g���|���҃R�[�h " �{�p��" �g���|�{�p�a��N���� 
HILO***              " ���ꌚ�����Ґ�= " ���ꌚ�����Ґ��v "�l �������������� ��" ��|���҃R�[�h END-IF
HILO  *    DISPLAY "610-4 " �g���|�{�p�a��N���� " �����̓��ꌚ�����Ґ�=" ���ꌚ�����Ґ��v
012630**          / �v�Z�v������ /
012640             INITIALIZE �v�Z���v ���ʒn����Z���v
012650*
HILO  */�g���|���Ó��敪 �[���F�ʏ��E�P�F�K��   �H�K��H
      */20240627
      *             IF (��|�I�����ʒn��敪 = 1) AND (�g���|���Ó��敪 NOT = ZERO)
      *                 MOVE ���l���ʒn����Z���v TO ���ʒn����Z���P���v
      *                 COMPUTE ���ʒn����Z���v   = ���ʒn����Z���v   + ���ʒn����Z���P���v
      *                 COMPUTE ���ʒn����Z�񐔂v = ���ʒn����Z�񐔂v + 1
      *             END-IF
      *
013550             PERFORM �g���Î��тe�Ǎ�
                   IF �g�����|���Ïꏊ�{�݃R�[�h NOT = ZERO
                       MOVE �g�����|���Ïꏊ�{�݃R�[�h TO �{�݁|�{�݃R�[�h
                       READ �{�݃}�X�^
                       INVALID KEY
                           MOVE SPACE TO �{�݁|���R�[�h
                       END-READ
                   END-IF
                   IF ((�g���|���Ó��敪   NOT = ZERO) AND
000600*                (((�g�����|���Ïꏊ�{�݃R�[�h NOT = ZERO)  AND (�{�݁|�I�����ʒn��敪 = "1")) OR
000600*                 ((�g�����|���Ïꏊ�{�݃R�[�h     = ZERO)  AND (��|�I�����ʒn��敪   = "1")))
                       (�g���|���ʒn����Z�敪 = 1))
                       MOVE ���l���ʒn����Z���v TO ���ʒn����Z���P���v
HILO                   COMPUTE �݌v���ʒn����Z���v  = �݌v���ʒn����Z���v + ���ʒn����Z���P���v
HILO                   MOVE    ���ʒn����Z���P���v TO ���ʒn����Z���v
                       COMPUTE ���ʒn����Z�񐔂v    = ���ʒn����Z�񐔂v   + 1
HILO***                   DISPLAY "810-6-1 ��"
HILO****                ��|" ��|�I�����ʒn��敪 " �{�݁|"     �{�݁|�I�����ʒn��敪
HILO***             "  �� �敪="   �g���|���ʒn����Z�敪
HILO***             "  �� ���Z��=" ���ʒn����Z���v 
HILO***             "  �� �P��="   ���ʒn����Z���P���v
HILO***             "  �� �Z��=" ���ʒn����Z�񐔂v " ��"
                   END-IF
012660*           / �������͕K�{ /
012670             IF �g���|������L�[ NOT = ZERO
012680*
012690                EVALUATE �g���|�{�p�敪
012700                WHEN 1
012710                   PERFORM �͂���v�����v�Z
012720                WHEN 2
012730                   PERFORM �}�b�T�[�W���v�����v�Z
012740*                  �����v���W�v
012750                   COMPUTE �}�b�T�[�W�����v�v = �}�b�T�[�W�����v�v + �}�b�T�[�W���v
012760                   COMPUTE �ό`�k�藿���v�v   = �ό`�k�藿���v�v   + �}�b�T�[�W�ό`�k�苸���p�v
012770                WHEN OTHER
012780                   DISPLAY "���v�{�p�敪�G���[�@���t�F" �g���|�{�p�a��N����   UPON CONS
012790*                 /-- ���zZERO--/
012800                   MOVE ZERO  TO �g���|��p�z
012810                   MOVE ZERO  TO �g���|�ꕔ���S��
012820                   MOVE ZERO  TO �g���|��O���Z�p���z��
012830                   COMPUTE �g���|�����z = �g���|�ꕔ���S�� + �g���|����z
012840                END-EVALUATE
      */20240704������/*
002830*/20240601�i1,2,3,4�j
                      EVALUATE TRUE
                      WHEN ���ꌚ�����Ґ��v = ZERO
001107                    MOVE ZERO TO �g���|�K��{�p���敪
                      WHEN ���ꌚ�����Ґ��v =  1
      *                   /�P�l
001107                    MOVE 1 TO �g���|�K��{�p���敪
                      WHEN ���ꌚ�����Ґ��v =  2
      *                   /�Q�l
001107                    MOVE 2 TO �g���|�K��{�p���敪
                      WHEN ���ꌚ�����Ґ��v <= 9
      *                   /�R�l�`�X�l
001107                    MOVE 3 TO �g���|�K��{�p���敪
                      WHEN ���ꌚ�����Ґ��v >= 10
      *                   /10�l�ȏ�
001107                    MOVE 4 TO �g���|�K��{�p���敪
                      END-EVALUATE
001107                MOVE ���ꌚ�����Ґ��v TO �g���|���ꌚ�����Ґ�

HILO  *                DISPLAY "�g���|�K��{�p���敪 " �g���|�K��{�p���敪
HILO  *                DISPLAY "�g���|���ꌚ�����Ґ� " �g���|���ꌚ�����Ґ�
      */20240704������/*
      */��������I���}�b�T�[�W�����{�p���Ă���ꍇ/20240830������
HILO  *     DISPLAY "k5-3 " �g���|���R�[�h�L�[ " �K��d���e=" �K��d���e
           IF �g���|�{�p�敪 = 1
               MOVE �K��d���e TO �g���|�K��d���e
           END-IF
      */��������I���}�b�T�[�W�����{�p���Ă���ꍇ/20240830������
HILO***                  DISPLAY "k5-4 �g���|���ꌚ�����Ґ� " �g���|���ꌚ�����Ґ�
012850**
012860                REWRITE �g���|���R�[�h
012870                IF ��ԃL�[ NOT = "00"
012880                     MOVE NC"�g���v�f�[�^�e" TO �t�@�C����
012890                     PERFORM �G���[�\��
012900                END-IF
012910             END-IF
012920**
012930             PERFORM �g���v�f�[�^�e�Ǎ�
012940         END-PERFORM
012950     END-IF.
012960*
012970*================================================================*
012980 ����{�p�L�����菈�� SECTION.
012990*
013000*   / ������ /
013010     MOVE ZERO                TO  ����{�p�t���O�v.
013020     MOVE ZERO                TO  �i����{�p�t���O�v.
013030*
013040     MOVE �g�A�v�Z�|�{�p�敪  TO  �g���|�{�p�敪.
013050     MOVE �g�A�v�Z�|���Ҕԍ�  TO  �g���|���Ҕԍ�.
013060     MOVE �g�A�v�Z�|�}��      TO  �g���|�}��.
013070     MOVE �g�A�v�Z�|�{�p�a��  TO  �g���|�{�p�a��.
013080     MOVE �g�A�v�Z�|�{�p�N    TO  �g���|�{�p�N.
013090     MOVE �g�A�v�Z�|�{�p��    TO  �g���|�{�p��.
013100     MOVE 1                   TO  �g���|�{�p��.
013110*
013120     START �g���v�f�[�^�e KEY IS >= �g���|�{�p�敪
013130                                    �g���|���҃R�[�h
013140                                    �g���|�{�p�a��N����
013150     END-START.
013160*
013170     IF ��ԃL�[ = "00"
013180         MOVE SPACE TO �I���t���O
013190         PERFORM �g���v�f�[�^�e�Ǎ�
013200*
013210*        �J��Ԃ��i�����j
013220         PERFORM UNTIL ( �g���|�{�p�敪   NOT = �g�A�v�Z�|�{�p�敪   ) OR
013230                       ( �g���|���҃R�[�h NOT = �g�A�v�Z�|���҃R�[�h ) OR
013240                       ( �g���|�{�p�a��   NOT = �g�A�v�Z�|�{�p�a��   ) OR
013250                       ( �g���|�{�p�N     NOT = �g�A�v�Z�|�{�p�N     ) OR
013260                       ( �g���|�{�p��     NOT = �g�A�v�Z�|�{�p��     ) OR
013270                       ( �I���t���O       NOT = SPACE )
013280*
013290             IF �g���|������L�[ NOT = ZERO
013300*                �P���`�����̊Ԃɓ���{�p�����������H
013310                 IF �g���|����{�p�敪 = 1
013320                     MOVE 1    TO  ����{�p�t���O�v
013330*
013340*                    �����J�n���ȍ~�ɓ���{�p�����������H
013350                     IF ( �������r���J�n���v NOT = ZERO ) AND ( �������r���J�n���v <= �g���|�{�p��)
013360                         MOVE 1    TO  �i����{�p�t���O�v
013370                     END-IF
013380                 END-IF
013401             END-IF
013402**
013410             PERFORM �g���v�f�[�^�e�Ǎ�
013420         END-PERFORM
013430     END-IF.
013440*
013450*================================================================*
013460 �g���v�f�[�^�e�Ǎ� SECTION.
013470*
013480     READ �g���v�f�[�^�e NEXT
013490     AT END
013500         MOVE "YES"  TO �I���t���O
013510     END-READ.
013520
013530**  / �g���Î��тe�Ǎ� /
013540     IF �I���t���O = SPACE
013550         PERFORM �g���Î��тe�Ǎ�
013560     END-IF.
013570*
013580*================================================================*
013590 �Q�Ƃg���v�f�[�^�e�Ǎ� SECTION.
013600*
013610     READ �Q�Ƃg���v�f�[�^�e NEXT
013620     AT END
013630         MOVE "YES"  TO �I���t���O
013640     END-READ.
013650*
013660*================================================================*
013670 �g���Î��тe�Ǎ� SECTION.
013680*
013690     INITIALIZE �g�����|���R�[�h.
013700     MOVE �g���|���R�[�h�L�[ TO �g�����|���R�[�h�L�[.
013710     READ �g���Î��тe
013720     INVALID
013730         INITIALIZE �g�����|���R�[�h
013740     END-READ.
013750*
013760*================================================================*
013770 �͂���v�����v�Z SECTION.
013780*--------------------------------------------*
013790*�i�{�p���e�敪�j
013800*  1:�͂�
013810*  2:�͂�{�d�C
013820*  3:���イ
013830*  4:���イ�{�d�C
013840*  5:�͂�{���イ
013850*  6:�͂�{���イ�{�d�C
013860*--------------------------------------------*
013870*
013880*---- �@�P�p���Q�p�� ------------------------------------------*
013890*     IF �g���|�{�p���e�敪 = 5 OR 6
013900**       /  �f�Ë敪 1:��ÁA2:����
013910*        IF �g���|�f�Ë敪 = 2
013920*           MOVE �g�Q�p����v  TO �͂�p�z�v
013930*        ELSE
013940*           MOVE �g�Q�p�v      TO �͂�p�z�v
013950*        END-IF
013960*     ELSE
013970**       /  �f�Ë敪 1:��ÁA2:����
013980*        IF �g���|�f�Ë敪 = 2
013990*           MOVE �g�P�p����v  TO �͂�p�z�v
014000*        ELSE
014010*           MOVE �g�P�p�v      TO �͂�p�z�v
014020*        END-IF
014030*     END-IF.
014040**
014050*--------------------------------------------------------------*
*****************************************************************
hilo  *     DISPLAY "610-1 �{�p�敪" �g���|�{�p�敪      
      *            " �{�p�a��N����" �g���|�{�p�a��N����
      *            " ���҃R�[�h"     �g���|���҃R�[�h
      *            " �˔����Ë敪"   �g���|�˔����Ë敪

           MOVE ZERO TO �K��d���e.
           IF (�g���|���Ó��敪 NOT = ZERO) AND (�g���|�˔����Ë敪 NOT = 1)
               MOVE 2                    TO �Q�Ƃg���|�{�p�敪      
               MOVE �g���|�{�p�a��N���� TO �Q�Ƃg���|�{�p�a��N����
               MOVE �g���|���҃R�[�h     TO �Q�Ƃg���|���҃R�[�h    
               READ �Q�Ƃg���v�f�[�^�e
               INVALID KEY
                  CONTINUE
               NOT INVALID KEY
                  IF (�Q�Ƃg���|���Ó��敪 NOT = ZERO) AND (�Q�Ƃg���|�˔����Ë敪 NOT = 1)
HILO***                  DISPLAY "k5-1 ��������I�}�L�@ " �g���|���҃R�[�h �g���|�{�p�a��N����
                      MOVE 1 TO �K��d���e
                  END-IF
               END-READ
           END-IF.
      */�˔����Ẫ`�F�b�N������/20240903
001107     IF �g���|�˔����Ë敪 = 1 
               MOVE �g���|�{�p�敪       TO �A���Q�O�P�|�{�p�敪       
               MOVE �g���|�{�p�a��N���� TO �A���Q�O�P�|�{�p�a��N���� 
               MOVE �g���|���҃R�[�h     TO �A���Q�O�P�|���҃R�[�h     
               MOVE SPACE                TO �A���Q�O�P�|�G���[�t���O
               MOVE ZERO TO �˔��s�e
022360         CALL   "TOPPATU"
022370         CANCEL "TOPPATU"
               IF �A���Q�O�P�|�G���[�t���O NOT = SPACE
                   MOVE 1 TO �˔��s�e
               END-IF
               OPEN I-O �X�V�p�g���Î��тe
004850             MOVE NC"�g����" TO �t�@�C����
004860             PERFORM �I�[�v���`�F�b�N
               MOVE �g���|�{�p�敪       TO �X�g�����|�{�p�敪
002380         MOVE �g���|�{�p�a��N���� TO �X�g�����|�{�p�a��N����
002390         MOVE �g���|���҃R�[�h     TO �X�g�����|���҃R�[�h
               READ �X�V�p�g���Î��тe
               NOT INVALID KEY
                   IF �A���Q�O�P�|�G���[�t���O NOT = SPACE
                       MOVE ZERO             TO �X�g�����|���Ë��z
                   ELSE
                       MOVE �g���Ê�{�����v TO �X�g�����|���Ë��z
                   END-IF
HILO***       IF �g�A�v�Z�S�|���ꌚ�����Ґ��擾�e = ZERO DISPLAY "�X�g�����|���Ë��z " �X�g�����|���Ë��z END-IF
                   REWRITE �X�g�����|���R�[�h
                   IF ��ԃL�[ NOT = "00"
                       MOVE NC"�X�g����" TO �t�@�C����
                       PERFORM �G���[�\��
                   END-IF
               END-READ
               CLOSE �X�V�p�g���Î��тe
           END-IF.
HILO***       DISPLAY "K5-A �A���Q�O�P�|�G���[�t���O " �A���Q�O�P�|�G���[�t���O "'" �˔��s�e "'"
HILO  */�˔����Ẫ`�F�b�N������/20240903
*****************************************************************
      **�����ۂɂ� �g���|�K��{�p���敪 �� �g���|�˔����Ë敪 ���g��
      */20240626
HILO***     DISPLAY "610-6 " �g���|���Ó��敪 " " �g���|�˔����Ë敪
      */�g���|���Ó��敪 �[���F�ʏ��E�P�F�K��
      *     IF �g���|���Ó��敪 = ZERO
           IF (�g���|���Ó��敪 = ZERO) OR (�g���|�˔����Ë敪 = 1) OR (�K��d���e = 1)
HILO***           DISPLAY "610-7 �ʏ�" �g���|���R�[�h�L�[
013890         IF �g���|�{�p���e�敪 = 5 OR 6
013900*           /  �f�Ë敪 1:��ÁA2:����
013910            IF �g���|�f�Ë敪 = 2
013920               MOVE �g�Q�p����v  TO �͂�p�z�v
013930            ELSE
013940               MOVE �g�Q�p�v      TO �͂�p�z�v
013950            END-IF
013960         ELSE
013970*           /  �f�Ë敪 1:��ÁA2:����
013980            IF �g���|�f�Ë敪 = 2
013990               MOVE �g�P�p����v  TO �͂�p�z�v
014000            ELSE
014010               MOVE �g�P�p�v      TO �͂�p�z�v
014020            END-IF
014030         END-IF
013890         IF �g���|�{�p���e�敪 = 5 OR 6
013940             MOVE �g�Q�p�v TO �͂肫�イ�ʏ��{�p���P���Q�v   
001420             COMPUTE �͂肫�イ�ʏ��{�p���񐔂Q�v   = �͂肫�イ�ʏ��{�p���񐔂Q�v   + 1
001420             COMPUTE �͂肫�イ�ʏ��{�p���{�p���Q�v = �͂肫�イ�ʏ��{�p���{�p���Q�v + �g�Q�p�v
013960         ELSE
014010             MOVE �g�P�p�v TO �͂肫�イ�ʏ��{�p���P���P�v   
001420             COMPUTE �͂肫�イ�ʏ��{�p���񐔂P�v   = �͂肫�イ�ʏ��{�p���񐔂P�v   + 1
001420             COMPUTE �͂肫�イ�ʏ��{�p���{�p���P�v = �͂肫�イ�ʏ��{�p���{�p���P�v + �g�P�p�v
014030         END-IF
           ELSE
HILO***           DISPLAY "610-8 �K��" �g���|���R�[�h�L�[
               EVALUATE TRUE
               WHEN ���ꌚ�����Ґ��v =   1
      *            /�P�l
013890             IF �g���|�{�p���e�敪 = 5 OR 6
      *                /�Q�p
000332                 MOVE �g�Q�p�K��{�p���P�v TO �͂�p�z�v
001410                 MOVE �g�Q�p�K��{�p���P�v TO �͂肫�イ�K��{�p���P�P���Q�v 
001420                 COMPUTE �͂肫�イ�K��{�p���P�񐔂Q�v   = �͂肫�イ�K��{�p���P�񐔂Q�v   + 1
001420                 COMPUTE �͂肫�イ�K��{�p���P�{�p���Q�v = �͂肫�イ�K��{�p���P�{�p���Q�v + �g�Q�p�K��{�p���P�v
013960             ELSE
      *                /�P�p
000331                 MOVE �g�P�p�K��{�p���P�v TO �͂�p�z�v
001410                 MOVE �g�P�p�K��{�p���P�v TO �͂肫�イ�K��{�p���P�P���P�v
001420                 COMPUTE �͂肫�イ�K��{�p���P�񐔂P�v   = �͂肫�イ�K��{�p���P�񐔂P�v   + 1
001420                 COMPUTE �͂肫�イ�K��{�p���P�{�p���P�v = �͂肫�イ�K��{�p���P�{�p���P�v + �g�P�p�K��{�p���P�v
014030             END-IF
               WHEN ���ꌚ�����Ґ��v =   2
      *            /�Q�l
013890             IF �g���|�{�p���e�敪 = 5 OR 6
      *                /�Q�p
000332                 MOVE �g�Q�p�K��{�p���Q�v TO �͂�p�z�v
001410                 MOVE �g�Q�p�K��{�p���Q�v TO �͂肫�イ�K��{�p���Q�P���Q�v 
001420                 COMPUTE �͂肫�イ�K��{�p���Q�񐔂Q�v   = �͂肫�イ�K��{�p���Q�񐔂Q�v   + 1
001420                 COMPUTE �͂肫�イ�K��{�p���Q�{�p���Q�v = �͂肫�イ�K��{�p���Q�{�p���Q�v + �g�Q�p�K��{�p���Q�v 
013960             ELSE
      *                /�P�p
000331                 MOVE �g�P�p�K��{�p���Q�v TO �͂�p�z�v
001410                 MOVE �g�P�p�K��{�p���Q�v TO �͂肫�イ�K��{�p���Q�P���P�v     
001420                 COMPUTE �͂肫�イ�K��{�p���Q�񐔂P�v   = �͂肫�イ�K��{�p���Q�񐔂P�v   + 1
001420                 COMPUTE �͂肫�イ�K��{�p���Q�{�p���P�v = �͂肫�イ�K��{�p���Q�{�p���P�v +  �g�P�p�K��{�p���Q�v 
014030             END-IF
               WHEN ���ꌚ�����Ґ��v <= 9
      *            /�R�l�`�X�l
013890             IF �g���|�{�p���e�敪 = 5 OR 6
      *                /�Q�p
000332                 MOVE �g�Q�p�K��{�p���R�v TO �͂�p�z�v
001410                 MOVE �g�Q�p�K��{�p���R�v TO �͂肫�イ�K��{�p���R�P���Q�v 
001420                 COMPUTE �͂肫�イ�K��{�p���R�񐔂Q�v   = �͂肫�イ�K��{�p���R�񐔂Q�v   + 1
001420                 COMPUTE �͂肫�イ�K��{�p���R�{�p���Q�v = �͂肫�イ�K��{�p���R�{�p���Q�v + �g�Q�p�K��{�p���R�v  
013960             ELSE
      *                /�P�p
000331                 MOVE �g�P�p�K��{�p���R�v TO �͂�p�z�v
001410                 MOVE �g�P�p�K��{�p���R�v TO �͂肫�イ�K��{�p���R�P���P�v 
001420                 COMPUTE �͂肫�イ�K��{�p���R�񐔂P�v   = �͂肫�イ�K��{�p���R�񐔂P�v   + 1
001420                 COMPUTE �͂肫�イ�K��{�p���R�{�p���P�v = �͂肫�イ�K��{�p���R�{�p���P�v + �g�P�p�K��{�p���R�v  
014030             END-IF
               WHEN ���ꌚ�����Ґ��v >= 10
      *            /�R�l�`�X�l
013890             IF �g���|�{�p���e�敪 = 5 OR 6
      *                /�Q�p
000332                 MOVE �g�Q�p�K��{�p���S�v TO �͂�p�z�v
001410                 MOVE �g�Q�p�K��{�p���S�v TO �͂肫�イ�K��{�p���S�P���Q�v 
001420                 COMPUTE �͂肫�イ�K��{�p���S�񐔂Q�v   = �͂肫�イ�K��{�p���S�񐔂Q�v   + 1
001420                 COMPUTE �͂肫�イ�K��{�p���S�{�p���Q�v = �͂肫�イ�K��{�p���S�{�p���Q�v + �g�Q�p�K��{�p���S�v  
013960             ELSE
      *                /�P�p
000331                 MOVE �g�P�p�K��{�p���S�v TO �͂�p�z�v
001410                 MOVE �g�P�p�K��{�p���S�v TO �͂肫�イ�K��{�p���S�P���P�v 
001420                 COMPUTE �͂肫�イ�K��{�p���S�񐔂P�v   = �͂肫�イ�K��{�p���S�񐔂P�v   + 1
001420                 COMPUTE �͂肫�イ�K��{�p���S�{�p���P�v = �͂肫�イ�K��{�p���S�{�p���P�v + �g�P�p�K��{�p���S�v  
014030             END-IF
               END-EVALUATE
      */20240917������
013910*            DISPLAY "k5-3 " �g���|�f�Ë敪
013900*    /  �f�Ë敪 1:��ÁA2:����
013910         IF �g���|�f�Ë敪 = 2
013890             IF �g���|�{�p���e�敪 = 5 OR 6
013920                 COMPUTE �͂�p�z�v = �͂�p�z�v + �Q�p�������v
013960             ELSE
013920                 COMPUTE �͂�p�z�v = �͂�p�z�v + �P�p�������v
013950             END-IF
HILO***           DISPLAY "610-11-1 �͂�p�z�v�{������=" �͂�p�z�v
013950         END-IF
      */20240917������
           END-IF.
      *
013890     EVALUATE �g���|�{�p���e�敪
           WHEN 1
           WHEN 2
001420         COMPUTE �͂�P�p�񐔂v       = �͂�P�p�񐔂v       + 1
           WHEN 3
           WHEN 4
001420         COMPUTE ���イ�P�p�񐔂v     = ���イ�P�p�񐔂v     + 1
           WHEN 5
           WHEN 6
001420         COMPUTE �͂肫�イ�Q�p�񐔂v = �͂肫�イ�Q�p�񐔂v + 1
           END-EVALUATE.
      *
HILO***       DISPLAY "610-10 �͂�p�z�v=" �͂�p�z�v
014060*
014070*---- �A�d�×�-------------------------------------------------*
014080     IF �g���|�{�p���e�敪 = 2 OR 4 OR 6
014090        MOVE �g�d�×��v  TO �͂�d�×��v
014100     END-IF.
014110*
014120*--------------------------------------------------------------*
014130*
014140*---- �B���×�-------------------------------------------------*
HILO***       DISPLAY "61-53 �g���|�˔����Ë敪" �g���|�˔����Ë敪 " "  �g���|���Ë��� " " 
hilo***                      �g�����|���Ȃ�����敪 " �˔��s�e" �˔��s�e
HILO  */�������˔�����
HILO  */�g���|�˔����Ë敪
*******20240903
      *     IF �g���|�˔����Ë敪 = 1
           IF (�g���|�˔����Ë敪 = 1) AND (�˔��s�e = ZERO)

HILO  *     DISPLAY "61-54 " �g���|���Ë��� " " �g�����|���Ȃ�����敪
HILO  *     MOVE 50 TO �g���|���Ë��� *>t

014150* (�����Ǝ��{)
014160*    / ���Ó��敪 1:����  ���聂�Q�������ǉ�
014170     IF (�g���|���Ó��敪 = 1) AND (�g���|���Ë��� NOT = ZERO) AND (�g�����|���Ȃ�����敪 NOT = 2)

HILO  *     DISPLAY "61-55 " 

014180        IF �g���|���Ë��� <= ���Ê�{����
HILO  *     DISPLAY "61-56 " �g���Ê�{�����v
014190            MOVE �g���Ê�{�����v  TO �͂艝�×��v
014200*           (���Z�v�g�e�p�񐔗݌v)
014210            COMPUTE �͂艝�É񐔂v = �͂艝�É񐔂v + 1
014220        ELSE
HILO  *     DISPLAY "61-57 " 
014230*           �����ɂ��邽�߁A�P�O���|����
014240            COMPUTE ���Ô폜���v = �g���|���Ë��� * 10
014250            COMPUTE ���Ï����v   = ���Ê�{���� * 10
014260            DIVIDE ���Ï����v INTO ���Ô폜���v GIVING ���Ï��v
014270                              REMAINDER ���Ï�]�v
014280            COMPUTE �͂艝�×��v = �g���Ê�{�����v +
014290                             ( �g���Òǉ������v * ( ���Ï��v - 1 ) )
014300            IF ���Ï�]�v > ZERO
014310                COMPUTE �͂艝�×��v = �͂艝�×��v + �g���Òǉ������v
014320            END-IF
014330**------------------------------------------------------------------------------**
014340*           ���×���������Z��@20160901
014350            INITIALIZE �g�A�ŉ��|�L�[
014360            MOVE 1                  TO �g�A�ŉ��|�{�p�敪
014370            MOVE �g���|�{�p�a��N�� TO �g�A�ŉ��|�{�p�a��N��
014380            MOVE �g���|���Ë���     TO �g�A�ŉ��|���Ë���
014390            CALL "MAXOURYO"
014400            IF �g�A�ŉ��|�Y���敪 = 1
014410                COMPUTE �͂艝�×��v = �g���Ê�{�����v + �g�A�ŉ��|���É��Z���z
014420            END-IF
014430            CANCEL "MAXOURYO"
014440**------------------------------------------------------------------------------**
014450**------------------------------------------------------------------------------**
014460*
014470*    (���Z�v�g�e�p�񐔁E���Z���E���É��Z�P���A�P���͂P�̂݁��Ō�̓��̃f�[�^�����A�S�����������Ȃ���Ȃ��I)
014480            COMPUTE �͂艝�É񐔂v = �͂艝�É񐔂v + 1
014490            COMPUTE �͂艝�É��Z�񐔂v = �͂艝�É��Z�񐔂v + 1
014500            COMPUTE �͂艝�É��Z�P���v = �͂艝�×��v -  �g���Ê�{�����v
014510            COMPUTE �͂艝�É��Z���v = �͂艝�É��Z���v + (�͂艝�×��v -  �g���Ê�{�����v)
014520**         /-- ���Z�v�g�e�p�F���Z�̉��Ë������Ⴄ�������邩���� --/
HILO  *     DISPLAY "61-58 " 
014530            IF �ޔ����Ë����v = ZERO
014540               MOVE �g���|���Ë���  TO �ޔ����Ë����v
014550            ELSE
014560               IF �g���|���Ë��� NOT = �ޔ����Ë����v
014570                  MOVE 1 TO ���É��Z��������v
014580               END-IF
014590            END-IF
014600**
014610        END-IF

HILO***       DISPLAY "610-2-4 �͂艝�×��v" �͂艝�×��v " " �g���|���È��敪 "'" �g���|���È����z "'"

      */���͎g��Ȃ�/20240919������
014620**------------------------------------------------------------------------------------*
014630**    �� ��L��A���̏ꍇ�́A���×����㏑��
014640*        IF �g���|���È��敪 = 1
014650*           MOVE �g���|���È����z  TO �͂艝�×��v
014660*        END-IF
      */���͎g��Ȃ�/20240919������
014670*------------------------------------------------------------------------------------*
014680*    �� ��L��A���Z�v�g�e�p�ɉ��×���݌v
014690        COMPUTE �͂�݌v���×��v = �͂�݌v���×��v + �͂艝�×��v
014700*------------------------------------------------------------------------------------*
014710*

HILO***       DISPLAY "610-2-2 �͂艝�×��v" �͂艝�×��v

014720     END-IF
HILO***       DISPLAY "610-2-3 �͂艝�×��v" �͂艝�×��v

014730
014740*    �� ���×��Ȃ��̏ꍇ�̓[���~�㏑��
014750     IF �g�����|���Ȃ�����敪 = 2
014760        ADD 1                    TO ���ÂO�~�J�E���^
014770        ADD 1                    TO �͂艝�É񐔂v
014780        MOVE ZERO                TO �͂艝�×��v
014790     END-IF

014720     END-IF.
HILO  */�������˔�����

HILO***       DISPLAY "610-2-1 �͂艝�×��v" �͂艝�×��v

014800*
014801*    �{�p�񍐏���t��
014806     IF (�g���|�{�p�a��N����     >= "4301001") AND
014807        (�g���|�{�p�񍐏���t�敪  = 1) AND
014808        (�g���|�{�p�a��N��       >= ��t�\�a��N���v)
014811         ADD  1                       TO �͂�񍐏���t�񐔂v
014812         ADD  �{�p�񍐏���t���P���v  TO �͂�񍐏���t���v
014813         MOVE �{�p�񍐏���t���P���v  TO �v�Z�p�񍐏���t���v
014814     ELSE
014815         MOVE ZERO                    TO �v�Z�p�񍐏���t���v
014816     END-IF.
014818*
014819*--------------------------------------------------------------*
014820*
014830* ���v
014840     COMPUTE �͂荇�v�z�v = �͂�p�z�v + �͂�d�×��v + �͂艝�×��v +
014841                            �v�Z�p�񍐏���t���v
      */20240627
                                + ���ʒn����Z���v.

HILO***       DISPLAY "61-52 " ��|���Ҏ���(1:10) "�F ���v�z" �͂荇�v�z�v  
HILO***                  " �͂�p" �͂�p�z�v  
HILO***                  " �d��"   �͂�d�×��v  
HILO***                  " ����"   �͂艝�×��v 
HILO***                  " �񍐏�" �v�Z�p�񍐏���t���v  
HILO***                  " ��"     ���ʒn����Z���v


014850* �A���̔�p�z�Z�b�g
014860     MOVE �͂荇�v�z�v TO �g�A�v�Z�|��p�z.
014870*
014880* ���Z�p�݌v
014890     COMPUTE �͂�݌v�z�v = �͂�݌v�z�v + �͂荇�v�z�v.
014900*
014910*--------------------------------------------------------------*
014920*
014930* / ���Z�v�g�e�p�񐔗݌v /
014940     IF �g���|�f�Ë敪 = 2
014950*   (����j
014960        MOVE �g���|�{�p���e�敪 TO �͂菉��{�p���e�敪�v
014970***        MOVE �͂荇�v�z�v       TO �͂菉�񗿂v
014980***      / �͂艝�×��v �����Ȃ� /
014990        COMPUTE �͂菉�񗿂v = �͂�p�z�v + �͂�d�×��v
015000***
015010     ELSE
015020*   (��Áj
015030        EVALUATE �g���|�{�p���e�敪
015040        WHEN 1
015050           COMPUTE �͂�񐔂v = �͂�񐔂v + 1
015060        WHEN 2
015070           COMPUTE �͂�d�C�񐔂v = �͂�d�C�񐔂v + 1
015080        WHEN 3
015090           COMPUTE ���イ�񐔂v = ���イ�񐔂v + 1
015100        WHEN 4
015110           COMPUTE ���イ�d�C�񐔂v = ���イ�d�C�񐔂v + 1
015120        WHEN 5
015130           COMPUTE �͂肫�イ�񐔂v = �͂肫�イ�񐔂v + 1
015140        WHEN 6
015150           COMPUTE �͂肫�イ�d�C�񐔂v = �͂肫�イ�d�C�񐔂v + 1
015160        END-EVALUATE
015170     END-IF.
015187*
015190*--------------------------------------------------------------*
015200*
015210     PERFORM ���v���S�z�擾.
015220*
015231*--------------------------------------------------------------*
015250*
015260*--------------------------------------------------------------*
015270* ���r�������̃��Z�e�p�̏W�v
015280*
015290     IF ( �������r���J�n���v NOT = ZERO ) AND ( �g���|�{�p�� >= �������r���J�n���v )
015300*
015310        IF �g���|�f�Ë敪 = 2
015320*         (����j
015330           MOVE �g���|�{�p���e�敪 TO �i�͂菉��{�p���e�敪�v
015340           COMPUTE �i�͂菉�񗿂v = �͂�p�z�v + �͂�d�×��v
015350        ELSE
015360*         (��Áj
015370           EVALUATE �g���|�{�p���e�敪
015380           WHEN 1
015390              COMPUTE �i�͂�񐔂v = �i�͂�񐔂v + 1
015400           WHEN 2
015410              COMPUTE �i�͂�d�C�񐔂v = �i�͂�d�C�񐔂v + 1
015420           WHEN 3
015430              COMPUTE �i���イ�񐔂v = �i���イ�񐔂v + 1
015440           WHEN 4
015450              COMPUTE �i���イ�d�C�񐔂v = �i���イ�d�C�񐔂v + 1
015460           WHEN 5
015470              COMPUTE �i�͂肫�イ�񐔂v = �i�͂肫�イ�񐔂v + 1
015480           WHEN 6
015490              COMPUTE �i�͂肫�イ�d�C�񐔂v = �i�͂肫�イ�d�C�񐔂v + 1
015500           END-EVALUATE
015510        END-IF
015520***
015530***     ���聂�Q�������ǉ�
015540        IF (�g���|���Ó��敪 = 1) AND (�g���|���Ë��� NOT = ZERO) AND (�g�����|���Ȃ�����敪 NOT = 2)
015550           IF �g���|���Ë��� <= ���Ê�{����
015560               MOVE �g���Ê�{�����v  TO �͂艝�×��v
015570*              (���Z�v�g�e�p�񐔗݌v)
015580               COMPUTE �i�͂艝�É񐔂v = �i�͂艝�É񐔂v + 1
015590           ELSE
015600*              �����ɂ��邽�߁A�P�O���|����
015610               COMPUTE ���Ô폜���v = �g���|���Ë��� * 10
015620               COMPUTE ���Ï����v   = ���Ê�{���� * 10
015630               DIVIDE ���Ï����v INTO ���Ô폜���v GIVING ���Ï��v
015640                                 REMAINDER ���Ï�]�v
015650               COMPUTE �͂艝�×��v = �g���Ê�{�����v +
015660                                ( �g���Òǉ������v * ( ���Ï��v - 1 ) )
015670               IF ���Ï�]�v > ZERO
015680                   COMPUTE �͂艝�×��v = �͂艝�×��v + �g���Òǉ������v
015690               END-IF
015700**------------------------------------------------------------------------------**
015710*              ���×���������Z��@20160901
015720               INITIALIZE �g�A�ŉ��|�L�[
015730               MOVE 1                  TO �g�A�ŉ��|�{�p�敪
015740               MOVE �g���|�{�p�a��N�� TO �g�A�ŉ��|�{�p�a��N��
015750               MOVE �g���|���Ë���     TO �g�A�ŉ��|���Ë���
015760               CALL "MAXOURYO"
015770               IF �g�A�ŉ��|�Y���敪 = 1
015780                   COMPUTE �͂艝�×��v = �g���Ê�{�����v + �g�A�ŉ��|���É��Z���z
015790               END-IF
015800               CANCEL "MAXOURYO"
015810**------------------------------------------------------------------------------**
015820*
015830*      (���Z�v�g�e�p�񐔁E���Z���E���É��Z�P���A�P���͂P�̂݁��Ō�̓��̃f�[�^�����A�S�����������Ȃ���Ȃ��I)
015840               COMPUTE �i�͂艝�É񐔂v = �i�͂艝�É񐔂v + 1
015850               COMPUTE �i�͂艝�É��Z�񐔂v = �i�͂艝�É��Z�񐔂v + 1
015860               COMPUTE �i�͂艝�É��Z�P���v = �͂艝�×��v -  �g���Ê�{�����v
015870               COMPUTE �i�͂艝�É��Z���v = �i�͂艝�É��Z���v + (�͂艝�×��v -  �g���Ê�{�����v)
015880*           /-- ���Z�v�g�e�p�F���Z�̉��Ë������Ⴄ�������邩���� --/
015890               IF �i�ޔ����Ë����v = ZERO
015900                  MOVE �g���|���Ë���  TO �i�ޔ����Ë����v
015910               ELSE
015920                  IF �g���|���Ë��� NOT = �i�ޔ����Ë����v
015930                     MOVE 1 TO �i���É��Z��������v
015940                  END-IF
015950               END-IF
015960**
015970           END-IF
015980*------------------------------------------------------------------------------------*
      */���͎g��Ȃ�/20240919������
015990**      �� ��L��A���̏ꍇ�́A���×����㏑��
016000*           IF �g���|���È��敪 = 1
016010*              MOVE �g���|���È����z  TO �͂艝�×��v
016020*           END-IF
      */���͎g��Ȃ�/20240919������
016030
016041*------------------------------------------------------------------------------------*
016042*      �� ��L��A���Z�v�g�e�p�ɉ��×���݌v
016050           COMPUTE �i�͂�݌v���×��v = �i�͂�݌v���×��v + �͂艝�×��v
016060*------------------------------------------------------------------------------------*
016070*
016080        END-IF
016101**
016102*       �� ���×��Ȃ��̏ꍇ�̓[���~�㏑��
016110        IF �g�����|���Ȃ�����敪 = 2
016120           ADD 1                    TO ���ÂO�~�J�E���^
016130           ADD 1                    TO �i�͂艝�É񐔂v
016140           MOVE ZERO                TO �͂艝�×��v
016150        END-IF
016172*
016173*       �{�p�񍐏���t��
016174        IF (�g���|�{�p�a��N����     >= "4301001") AND
016175           (�g���|�{�p�񍐏���t�敪  = 1) AND
016176           (�g���|�{�p�a��N��       >= ��t�\�a��N���v)
016177            ADD 1                      TO �i�͂�񍐏���t�񐔂v
016178            ADD �{�p�񍐏���t���P���v TO �i�͂�񍐏���t���v
016179            MOVE �{�p�񍐏���t���P���v  TO �v�Z�p�񍐏���t���v
016182        ELSE
016183            MOVE ZERO                    TO �v�Z�p�񍐏���t���v
016184        END-IF
016185*
016186* ���v
016188        COMPUTE �i�͂�݌v�z�v = �i�͂�݌v�z�v + �͂荇�v�z�v
016189*
016190     END-IF.
016200*--------------------------------------------------------------*
016210*
016220*================================================================*
016230 �}�b�T�[�W���v�����v�Z SECTION.
016240*--------------------------------------------*
016250*�i�{�p���e�敪�j
016260*  1:�}�b�T�[�W
016270*  2:�}�b�T�[�W�{��㪖@
016280*  3:�}�b�T�[�W�{��㪖@�{�d�C
016290*  4:�ό`�k�苸���p
016300*  5:�ό`�k�苸���p�{��㪖@
016310*  6:�ό`�k�苸���p�{��㪖@�{�d�C
016320*  7:�}�b�T�[�W�{�ό`�k�苸���p
016330*  8:�}�b�T�[�W�{�ό`�k�苸���p�{��㪖@
016340*  9:�}�b�T�[�W�{�ό`�k�苸���p�{��㪖@�{�d�C
016350*
016360*--------------------------------------------*
016370*
      */�˔����Ẫ`�F�b�N������/20240903
001107     IF �g���|�˔����Ë敪 = 1 
               MOVE �g���|�{�p�敪       TO �A���Q�O�P�|�{�p�敪       
               MOVE �g���|�{�p�a��N���� TO �A���Q�O�P�|�{�p�a��N���� 
               MOVE �g���|���҃R�[�h     TO �A���Q�O�P�|���҃R�[�h     
               MOVE SPACE                TO �A���Q�O�P�|�G���[�t���O
               MOVE ZERO TO �˔��s�e
022360         CALL   "TOPPATU"
022370         CANCEL "TOPPATU"
               IF �A���Q�O�P�|�G���[�t���O NOT = SPACE
                   MOVE 1 TO �˔��s�e
               END-IF
               OPEN I-O �X�V�p�g���Î��тe
004850             MOVE NC"�g����" TO �t�@�C����
004860             PERFORM �I�[�v���`�F�b�N
               MOVE �g���|�{�p�敪       TO �X�g�����|�{�p�敪
002380         MOVE �g���|�{�p�a��N���� TO �X�g�����|�{�p�a��N����
002390         MOVE �g���|���҃R�[�h     TO �X�g�����|���҃R�[�h
               READ �X�V�p�g���Î��тe
               NOT INVALID KEY
                   IF �A���Q�O�P�|�G���[�t���O NOT = SPACE
                       MOVE ZERO             TO �X�g�����|���Ë��z
                   ELSE
                       MOVE �g���Ê�{�����v TO �X�g�����|���Ë��z
                   END-IF
HILO***       IF �g�A�v�Z�S�|���ꌚ�����Ґ��擾�e = ZERO DISPLAY "�X�g�����|���Ë��z " �X�g�����|���Ë��z END-IF
                   REWRITE �X�g�����|���R�[�h
                   IF ��ԃL�[ NOT = "00"
                       MOVE NC"�X�g����" TO �t�@�C����
                       PERFORM �G���[�\��
                   END-IF
               END-READ
               CLOSE �X�V�p�g���Î��тe
           END-IF.
HILO***       DISPLAY "K5-A �A���Q�O�P�|�G���[�t���O " �A���Q�O�P�|�G���[�t���O "'" �˔��s�e "'"
HILO  */�˔����Ẫ`�F�b�N������/20240903
      **/�˔����Ẫ`�F�b�N������/20240903
001107*     IF �g���|�˔����Ë敪 = 1 
      *     MOVE �g���|�{�p�敪       TO �A���Q�O�P�|�{�p�敪       
      *     MOVE �g���|�{�p�a��N���� TO �A���Q�O�P�|�{�p�a��N���� 
      *     MOVE �g���|���҃R�[�h     TO �A���Q�O�P�|���҃R�[�h     
      *     MOVE SPACE                TO �A���Q�O�P�|�G���[�t���O
      *     MOVE ZERO TO �˔��s�e
022360**     CALL   "TOPPATU"
022370**     CANCEL "TOPPATU"
      *     END-IF
HILO****       DISPLAY "K5-A �A���Q�O�P�|�G���[�t���O " �A���Q�O�P�|�G���[�t���O
      *     IF �A���Q�O�P�|�G���[�t���O NOT = SPACE
      *         MOVE 1 TO �˔��s�e
      *         OPEN I-O �X�V�p�g���Î��тe
004850*         MOVE NC"�{��" TO �t�@�C����
004860*         PERFORM �I�[�v���`�F�b�N
      *         MOVE �g���|�{�p�敪       TO �X�g�����|�{�p�敪
002380*         MOVE �g���|�{�p�a��N���� TO �X�g�����|�{�p�a��N����
002390*         MOVE �g���|���҃R�[�h     TO �X�g�����|���҃R�[�h
      *         READ �X�V�p�g���Î��тe
      *         NOT INVALID KEY
      *             MOVE ZERO TO �X�g�����|���Ë��z
      *             REWRITE �X�g�����|���R�[�h
      *             IF ��ԃL�[ NOT = "00"
      *                 MOVE NC"�X�g����" TO �t�@�C����
      *                 PERFORM �G���[�\��
      *             END-IF
      *         END-READ
      *         CLOSE �X�V�p�g���Î��тe
      *     END-IF.
HILO  */�˔����Ẫ`�F�b�N������/20240903
HILO  *     DISPLAY "610-5 ����{�p�t���O�v=" ����{�p�t���O�v
016380     IF ����{�p�t���O�v NOT = 1
016390*      �� ����{�p���͖����A�������͌Â��f�[�^ ��
016400*------ �@�}�b�T�[�W -------------------------------------------*
016410        IF �g���|�{�p���e�敪 = 1 OR 2 OR 3 OR 7 OR 8 OR 9
016420            COMPUTE �}�b�T�[�W�񐔂v = �}�b�T�[�W�񐔂v + 1
016430            COMPUTE �}�b�T�[�W���v = �l�P�Ǐ��v * �}�b�T�[�W�Ǐ����v
016440        END-IF
HILO  *        DISPLAY "610-3-1 �}�b�T�[�W�Ǐ����v" �}�b�T�[�W�Ǐ����v " �}�b�T�[�W���v" �}�b�T�[�W���v
016450*
016460*------ �A�ό`�k�苸���p-----------------------------------------*
016470        IF �g���|�{�p���e�敪 = 4 OR 5 OR 6 OR 7 OR 8 OR 9
016480            COMPUTE �}�b�T�[�W�ό`�k�苸���p�񐔂v = �}�b�T�[�W�ό`�k�苸���p�񐔂v + 1
016490            COMPUTE �}�b�T�[�W�ό`�k�苸���p�v = �l�ό`�k�苸���p�v * �ό`�k�苸���p���v
016500        END-IF
016510     ELSE
016520*------- ����{�p���v�Z----------*
016530*       �{�p�ӏ����ƂɌv�Z
016540        MOVE ZERO         TO �}�b�T�[�W�Ǐ������v�v
016550        MOVE ZERO         TO �ό`�k�苸���p�����v�v
016560*
016570        IF (�g���|�}�b�T�[�W�̊�   = 1) OR
016580           (�g���|�}�b�T�[�W�E�㎈ = 1) OR
016590           (�g���|�}�b�T�[�W���㎈ = 1) OR
016600           (�g���|�}�b�T�[�W�E���� = 1) OR
016610           (�g���|�}�b�T�[�W������ = 1)
016620           ADD 1          TO �}�b�T�[�W�񐔂v
016630        END-IF
016640
016650        IF �g���|�}�b�T�[�W�̊� = 1
016660           ADD 1          TO �}�b�T�[�W�̊��񐔂v �}�b�T�[�W�Ǐ������v�v
016670           ADD �l�P�Ǐ��v TO �}�b�T�[�W�̊����z�v �}�b�T�[�W���v
016680        END-IF
016690        IF �g���|�}�b�T�[�W�E�㎈ = 1
016700           ADD 1          TO �}�b�T�[�W�E�㎈�񐔂v �}�b�T�[�W�Ǐ������v�v
016710           ADD �l�P�Ǐ��v TO �}�b�T�[�W�E�㎈���z�v �}�b�T�[�W���v
016720        END-IF
016730        IF �g���|�}�b�T�[�W���㎈ = 1
016740           ADD 1          TO �}�b�T�[�W���㎈�񐔂v �}�b�T�[�W�Ǐ������v�v
016750           ADD �l�P�Ǐ��v TO �}�b�T�[�W���㎈���z�v �}�b�T�[�W���v
016760        END-IF
016770        IF �g���|�}�b�T�[�W�E���� = 1
016780           ADD 1          TO �}�b�T�[�W�E�����񐔂v �}�b�T�[�W�Ǐ������v�v
016790           ADD �l�P�Ǐ��v TO �}�b�T�[�W�E�������z�v �}�b�T�[�W���v
016800        END-IF
016810        IF �g���|�}�b�T�[�W������ = 1
016820           ADD 1          TO �}�b�T�[�W�������񐔂v �}�b�T�[�W�Ǐ������v�v
016830           ADD �l�P�Ǐ��v TO �}�b�T�[�W���������z�v �}�b�T�[�W���v
016840        END-IF
016850*
HILO  *        DISPLAY "610-3-2 �}�b�T�[�W�Ǐ������v�v " �}�b�T�[�W�Ǐ������v�v
016860        IF (�g���|�ό`�k�苸���p�E�㎈ = 1) OR
016870           (�g���|�ό`�k�苸���p���㎈ = 1) OR
016880           (�g���|�ό`�k�苸���p�E���� = 1) OR
016890           (�g���|�ό`�k�苸���p������ = 1)
016900           ADD 1          TO �}�b�T�[�W�ό`�k�苸���p�񐔂v
016910        END-IF
016920*
016930        IF �g���|�ό`�k�苸���p�E�㎈ = 1
016940           ADD 1                  TO �ό`�k�苸���p�E�㎈�񐔂v �ό`�k�苸���p�����v�v
016950           ADD �l�ό`�k�苸���p�v TO �ό`�k�苸���p�E�㎈���z�v �}�b�T�[�W�ό`�k�苸���p�v
016960        END-IF
016970        IF �g���|�ό`�k�苸���p���㎈ = 1
016980           ADD 1                  TO �ό`�k�苸���p���㎈�񐔂v �ό`�k�苸���p�����v�v
016990           ADD �l�ό`�k�苸���p�v TO �ό`�k�苸���p���㎈���z�v �}�b�T�[�W�ό`�k�苸���p�v
017000        END-IF
017010        IF �g���|�ό`�k�苸���p�E���� = 1
017020           ADD 1                  TO �ό`�k�苸���p�E�����񐔂v �ό`�k�苸���p�����v�v
017030           ADD �l�ό`�k�苸���p�v TO �ό`�k�苸���p�E�������z�v �}�b�T�[�W�ό`�k�苸���p�v
017040        END-IF
017050        IF �g���|�ό`�k�苸���p������ = 1
017060           ADD 1                  TO �ό`�k�苸���p�������񐔂v �ό`�k�苸���p�����v�v
017070           ADD �l�ό`�k�苸���p�v TO �ό`�k�苸���p���������z�v �}�b�T�[�W�ό`�k�苸���p�v
017080        END-IF
017090*
017100*       �{�p�ӏ������ӏ����ƈقȂ�Γ���{�p���e��\��������
017110        IF (�g���|�{�p���e�敪                = 1 OR 2 OR 3 OR 7 OR 8 OR 9)
017120           IF (�}�b�T�[�W�̊����ӂv       NOT = �g���|�}�b�T�[�W�̊�) OR
017130              (�}�b�T�[�W�E�㎈���ӂv     NOT = �g���|�}�b�T�[�W�E�㎈) OR
017140              (�}�b�T�[�W���㎈���ӂv     NOT = �g���|�}�b�T�[�W���㎈) OR
017150              (�}�b�T�[�W�E�������ӂv     NOT = �g���|�}�b�T�[�W�E����) OR
017160              (�}�b�T�[�W���������ӂv     NOT = �g���|�}�b�T�[�W������)
017170              MOVE 1                  TO �}�b�T�[�W����{�p�t���O�v
017180              MOVE 1                  TO �i�}�b�T�[�W����{�p�t���O�v
017190           END-IF
017200        END-IF
017210        IF (�g���|�{�p���e�敪                 = 4 OR 5 OR 6 OR 7 OR 8 OR 9 )
017220           IF (�ό`�k�苸���p�E�㎈���ӂv  NOT = �g���|�ό`�k�苸���p�E�㎈) OR
017230              (�ό`�k�苸���p���㎈���ӂv  NOT = �g���|�ό`�k�苸���p���㎈) OR
017240              (�ό`�k�苸���p�E�������ӂv  NOT = �g���|�ό`�k�苸���p�E����) OR
017250              (�ό`�k�苸���p���������ӂv  NOT = �g���|�ό`�k�苸���p������)
017260               MOVE 1                 TO �ό`�k�苸���p����{�p�t���O�v
017270               MOVE 1                 TO �i�ό`�k�苸���p����{�p�t���O�v
017280           END-IF
017290        END-IF
017300*
017310     END-IF.
017320*
017330*    �{�p�ӏ����������p�^�[�����`�F�b�N
017340     IF �g���|�{�p���e�敪 = 1 OR 2 OR 3 OR 7 OR 8 OR 9
017350        IF �}�b�T�[�W�Ǐ����Z�b�g�v NOT = ZERO
017360*          �ŏ���1��Ɣ�r
017370           IF �g���|����{�p�敪 = ZERO
017380              IF (�}�b�T�[�W�Ǐ����o�v NOT = �}�b�T�[�W�Ǐ����v) AND
017390                 (�}�b�T�[�W�Ǐ����v   NOT = ZERO)
017400                 MOVE 1                        TO �}�b�T�[�W�Ǐ��������v
017410              END-IF
017420           ELSE
017430              IF (�}�b�T�[�W�Ǐ����o�v   NOT = �}�b�T�[�W�Ǐ������v�v) AND
017440                 (�}�b�T�[�W�Ǐ������v�v NOT = ZERO)
017450                 MOVE 1                        TO �}�b�T�[�W�Ǐ��������v
017460              END-IF
017470           END-IF
017480        ELSE
017490*          �p�^�[������p�ɍŏ���1���ۑ�����B
017500           IF �g���|����{�p�敪 = ZERO
017510              IF �}�b�T�[�W�Ǐ����v NOT = ZERO
017520                 MOVE �}�b�T�[�W�Ǐ����v       TO �}�b�T�[�W�Ǐ����o�v
017530                 MOVE 1                        TO �}�b�T�[�W�Ǐ����Z�b�g�v
017540              END-IF
017550           ELSE
017560              IF �}�b�T�[�W�Ǐ������v�v NOT = ZERO
017570                 MOVE �}�b�T�[�W�Ǐ������v�v   TO �}�b�T�[�W�Ǐ����o�v
017580                 MOVE 1                        TO �}�b�T�[�W�Ǐ����Z�b�g�v
017590              END-IF
017600           END-IF
017610        END-IF
017620     END-IF.
017630*
017640     IF �g���|�{�p���e�敪 = 4 OR 5 OR 6 OR 7 OR 8 OR 9
017650        IF �ό`�k�苸���p���Z�b�g�v NOT = ZERO
017660*          �ŏ���1��Ɣ�r
017670           IF �g���|����{�p�敪 = ZERO
017680              IF (�ό`�k�苸���p���o�v NOT = �ό`�k�苸���p���v) AND
017690                 (�ό`�k�苸���p���v   NOT = ZERO)
017700                 MOVE 1                        TO �ό`�k�苸���p�������v
017710              END-IF
017720           ELSE
017730              IF (�ό`�k�苸���p���o�v   NOT = �ό`�k�苸���p�����v�v) AND
017740                 (�ό`�k�苸���p�����v�v NOT = ZERO)
017750                 MOVE 1                        TO �ό`�k�苸���p�������v
017760              END-IF
017770           END-IF
017780        ELSE
017790*          �p�^�[������p�ɍŏ���1���ۑ�����B
017800           IF �g���|����{�p�敪 = ZERO
017810              IF �ό`�k�苸���p���v NOT = ZERO
017820                 MOVE �ό`�k�苸���p���v       TO �ό`�k�苸���p���o�v
017830                 MOVE 1                        TO �ό`�k�苸���p���Z�b�g�v
017840              END-IF
017850           ELSE
017860              IF �ό`�k�苸���p�����v�v NOT = ZERO
017870                 MOVE �ό`�k�苸���p�����v�v   TO �ό`�k�苸���p���o�v
017880                 MOVE 1                        TO �ό`�k�苸���p���Z�b�g�v
017890              END-IF
017900           END-IF
017910        END-IF
017920     END-IF.
017930*
      */202406������
      *x���ۂɂ� �g���|�K��{�p���敪 �� �g���|�˔����Ë敪 ���g��
HILO  *     DISPLAY "k5-1 " �g���|�˔����Ë敪 " " �˔��s�e 
      */�g���|���Ó��敪 �[���F�ʏ��E�P�F�K��
      *     IF �g���|���Ó��敪 = ZERO
*******20240903
           IF (�g���|���Ó��敪 = ZERO) OR (�g���|�˔����Ë敪 = 1)
      */�ʏ��͂��̂܂܁H
HILO***           DISPLAY "610-4-1 �ʏ�"
HILO  *         DISPLAY "610-3-1 �}�b�T�[�W�Ǐ����v" �}�b�T�[�W�Ǐ����v " �}�b�T�[�W���v" �}�b�T�[�W���v
               CONTINUE
      *
               IF (�}�b�T�[�W�ʏ��{�p���P���P�v = ZERO           ) OR
001410            (�}�b�T�[�W�ʏ��{�p���P���P�v = �}�b�T�[�W���v )
001410             MOVE �}�b�T�[�W���v TO �}�b�T�[�W�ʏ��{�p���P���P�v  
001420             COMPUTE �}�b�T�[�W�ʏ��{�p���񐔂P�v   = �}�b�T�[�W�ʏ��{�p���񐔂P�v + 1
001420             COMPUTE �}�b�T�[�W�ʏ��{�p���{�p���P�v = �}�b�T�[�W�ʏ��{�p���{�p���P�v + �}�b�T�[�W���v
               ELSE
001410             MOVE �}�b�T�[�W���v TO �}�b�T�[�W�ʏ��{�p���P���Q�v  
001420             COMPUTE �}�b�T�[�W�ʏ��{�p���񐔂Q�v   = �}�b�T�[�W�ʏ��{�p���񐔂Q�v   + 1
001420             COMPUTE �}�b�T�[�W�ʏ��{�p���{�p���Q�v = �}�b�T�[�W�ʏ��{�p���{�p���Q�v + �}�b�T�[�W���v
               END-IF
           ELSE
HILO***           DISPLAY "610-4-2 �K��"
HILO  *     DISPLAY "610-13 �}�b�T�[�W�Ǐ������v�v " �}�b�T�[�W�Ǐ������v�v
016380         IF ����{�p�t���O�v NOT = 1
016390*          �� ����{�p���͖����A�������͌Â��f�[�^ ��
                   MOVE �}�b�T�[�W�Ǐ����v     TO �����Ǐ����v
016510         ELSE
016520*----------- ����{�p���v�Z----------*
                   MOVE �}�b�T�[�W�Ǐ������v�v TO �����Ǐ����v
               END-IF
               EVALUATE TRUE
               WHEN ���ꌚ�����Ґ��v = 1
      *            /�P�l
                   EVALUATE �����Ǐ����v
                   WHEN 1
000339                 MOVE �l�P�Ǐ��K��{�p���P�v TO �}�b�T�[�W���v
                   WHEN 2
000339                 MOVE �l�Q�Ǐ��K��{�p���P�v TO �}�b�T�[�W���v
                   WHEN 3
000339                 MOVE �l�R�Ǐ��K��{�p���P�v TO �}�b�T�[�W���v
                   WHEN 4
000339                 MOVE �l�S�Ǐ��K��{�p���P�v TO �}�b�T�[�W���v
                   WHEN 5
000339                 MOVE �l�T�Ǐ��K��{�p���P�v TO �}�b�T�[�W���v
                   END-EVALUATE
001410             IF (�}�b�T�[�W�K��{�p���P�P���P�v = ZERO          ) OR
001410                (�}�b�T�[�W�K��{�p���P�P���P�v = �}�b�T�[�W���v)
001410                 MOVE �}�b�T�[�W���v TO �}�b�T�[�W�K��{�p���P�P���P�v
001420                 COMPUTE �}�b�T�[�W�K��{�p���P�񐔂P�v   = �}�b�T�[�W�K��{�p���P�񐔂P�v   + 1
001420                 COMPUTE �}�b�T�[�W�K��{�p���P�{�p���P�v = �}�b�T�[�W�K��{�p���P�{�p���P�v + �}�b�T�[�W���v
                   ELSE
001410                 MOVE �}�b�T�[�W���v TO �}�b�T�[�W�K��{�p���P�P���Q�v   
001420                 COMPUTE �}�b�T�[�W�K��{�p���P�񐔂Q�v   = �}�b�T�[�W�K��{�p���P�񐔂Q�v   + 1
001420                 COMPUTE �}�b�T�[�W�K��{�p���P�{�p���Q�v = �}�b�T�[�W�K��{�p���P�{�p���Q�v + �}�b�T�[�W���v
                   END-IF
               WHEN ���ꌚ�����Ґ��v = 2
      *            /�Q�l
                   EVALUATE �����Ǐ����v
                   WHEN 1
000339                 MOVE �l�P�Ǐ��K��{�p���Q�v TO �}�b�T�[�W���v
                   WHEN 2
000339                 MOVE �l�Q�Ǐ��K��{�p���Q�v TO �}�b�T�[�W���v
                   WHEN 3
000339                 MOVE �l�R�Ǐ��K��{�p���Q�v TO �}�b�T�[�W���v
                   WHEN 4
000339                 MOVE �l�S�Ǐ��K��{�p���Q�v TO �}�b�T�[�W���v
                   WHEN 5
000339                 MOVE �l�T�Ǐ��K��{�p���Q�v TO �}�b�T�[�W���v
                   END-EVALUATE
001410             IF (�}�b�T�[�W�K��{�p���Q�P���P�v = ZERO          ) OR
001410                (�}�b�T�[�W�K��{�p���Q�P���P�v = �}�b�T�[�W���v)
001410                 MOVE �}�b�T�[�W���v TO �}�b�T�[�W�K��{�p���Q�P���P�v    
001420                 COMPUTE �}�b�T�[�W�K��{�p���Q�񐔂P�v   =  �}�b�T�[�W�K��{�p���Q�񐔂P�v   + 1
001420                 COMPUTE �}�b�T�[�W�K��{�p���Q�{�p���P�v =  �}�b�T�[�W�K��{�p���Q�{�p���P�v + �}�b�T�[�W���v
                   ELSE
001410                 MOVE �}�b�T�[�W���v TO �}�b�T�[�W�K��{�p���Q�P���Q�v    
001420                 COMPUTE �}�b�T�[�W�K��{�p���Q�񐔂Q�v   = �}�b�T�[�W�K��{�p���Q�񐔂Q�v    + 1
001420                 COMPUTE �}�b�T�[�W�K��{�p���Q�{�p���Q�v = �}�b�T�[�W�K��{�p���Q�{�p���Q�v  + �}�b�T�[�W���v
                   END-IF
               WHEN ���ꌚ�����Ґ��v <= 9
      *            /�R�l�`�X�l
                   EVALUATE �����Ǐ����v
                   WHEN 1
000339                 MOVE �l�P�Ǐ��K��{�p���R�v TO �}�b�T�[�W���v
                   WHEN 2
000339                 MOVE �l�Q�Ǐ��K��{�p���R�v TO �}�b�T�[�W���v
                   WHEN 3
000339                 MOVE �l�R�Ǐ��K��{�p���R�v TO �}�b�T�[�W���v
                   WHEN 4
000339                 MOVE �l�S�Ǐ��K��{�p���R�v TO �}�b�T�[�W���v
                   WHEN 5
000339                 MOVE �l�T�Ǐ��K��{�p���R�v TO �}�b�T�[�W���v
                   END-EVALUATE
001410             IF (�}�b�T�[�W�K��{�p���R�P���P�v = ZERO          ) OR
001410                (�}�b�T�[�W�K��{�p���R�P���P�v = �}�b�T�[�W���v)
001410                 MOVE �}�b�T�[�W���v TO �}�b�T�[�W�K��{�p���R�P���P�v    
001420                 COMPUTE �}�b�T�[�W�K��{�p���R�񐔂P�v   = �}�b�T�[�W�K��{�p���R�񐔂P�v    + 1
001420                 COMPUTE �}�b�T�[�W�K��{�p���R�{�p���P�v = �}�b�T�[�W�K��{�p���R�{�p���P�v  + �}�b�T�[�W���v
                   ELSE
001410                 MOVE �}�b�T�[�W���v TO �}�b�T�[�W�K��{�p���R�P���Q�v    
001420                 COMPUTE �}�b�T�[�W�K��{�p���R�񐔂Q�v   = �}�b�T�[�W�K��{�p���R�񐔂Q�v    + 1
001420                 COMPUTE �}�b�T�[�W�K��{�p���R�{�p���Q�v = �}�b�T�[�W�K��{�p���R�{�p���Q�v  + �}�b�T�[�W���v
                   END-IF
               WHEN ���ꌚ�����Ґ��v >= 10
      *            /�P�O�l�ȏ�
                   EVALUATE �����Ǐ����v
                   WHEN 1
000339                 MOVE �l�P�Ǐ��K��{�p���S�v TO �}�b�T�[�W���v
                   WHEN 2
000339                 MOVE �l�Q�Ǐ��K��{�p���S�v TO �}�b�T�[�W���v
                   WHEN 3
000339                 MOVE �l�R�Ǐ��K��{�p���S�v TO �}�b�T�[�W���v
                   WHEN 4
000339                 MOVE �l�S�Ǐ��K��{�p���S�v TO �}�b�T�[�W���v
                   WHEN 5
000339                 MOVE �l�T�Ǐ��K��{�p���S�v TO �}�b�T�[�W���v
                   END-EVALUATE
001410             IF (�}�b�T�[�W�K��{�p���S�P���P�v = ZERO          ) OR
001410                (�}�b�T�[�W�K��{�p���S�P���P�v = �}�b�T�[�W���v)
001410                 MOVE �}�b�T�[�W���v TO �}�b�T�[�W�K��{�p���S�P���P�v
001420                 COMPUTE �}�b�T�[�W�K��{�p���S�񐔂P�v   = �}�b�T�[�W�K��{�p���S�񐔂P�v    + 1
001420                 COMPUTE �}�b�T�[�W�K��{�p���S�{�p���P�v = �}�b�T�[�W�K��{�p���S�{�p���P�v  + �}�b�T�[�W���v
                   ELSE
001410                 MOVE �}�b�T�[�W���v TO �}�b�T�[�W�K��{�p���S�P���Q�v    
001420                 COMPUTE �}�b�T�[�W�K��{�p���S�񐔂Q�v   = �}�b�T�[�W�K��{�p���S�񐔂Q�v    + 1
001420                 COMPUTE �}�b�T�[�W�K��{�p���S�{�p���Q�v = �}�b�T�[�W�K��{�p���S�{�p���Q�v  + �}�b�T�[�W���v
                   END-IF
               END-EVALUATE
           END-IF
HILO  *        DISPLAY "610-3-2 �}�b�T�[�W���v  =" �}�b�T�[�W���v " �Ǐ����v=" �}�b�T�[�W�Ǐ����v 
HILO  *        DISPLAY "610-3-3 �ό`�k�苸���p�v=" �}�b�T�[�W�ό`�k�苸���p�v 
      *        " �P��=" �l�ό`�k�苸���p�v  " ��=" �ό`�k�苸���p���v
017940*--- �B��㪖@��(1��̂�)-------------------------------------------*
017950     IF �g���|�{�p���e�敪 = 2 OR 5 OR 8
017960        COMPUTE �}�b�T�[�W��㪖@�񐔂v = �}�b�T�[�W��㪖@�񐔂v + 1
017970        MOVE �l��㪖@���v TO �}�b�T�[�W��㪖@���v
017980     END-IF.
017990*
018000*--- �C��㪖@+�d�×�(1��̂�)--------------------------------------*
018010     IF �g���|�{�p���e�敪 = 3 OR 6 OR 9
018020        COMPUTE �}�b�T�[�W��㪖@�d�C�񐔂v = �}�b�T�[�W��㪖@�d�C�񐔂v + 1
018030        COMPUTE �}�b�T�[�W��㪖@�d�×��v = �l��㪖@���v + �l�d�×��v
018040     END-IF.
018050*
018060*---- �D���×�-------------------------------------------------*
HILO  */�������˔�����
HILO  */�g���|�˔����Ë敪
      *     IF �g���|�˔����Ë敪 = 1
           IF (�g���|�˔����Ë敪 = 1) AND (�˔��s�e = ZERO)
      *     IF �g���|���Ó��敪 = ZERO
018070* (�����Ǝ��{)
018080*    / ���Ó��敪 1:���� ���聂�Q�������ǉ�
018090     IF (�g���|���Ó��敪 = 1) AND (�g���|���Ë��� NOT = ZERO) AND (�g�����|���Ȃ�����敪 NOT = 2)
018100        IF �g���|���Ë��� <= ���Ê�{����
018110            MOVE �l���Ê�{�����v  TO �}�b�T�[�W���×��v
018120*           (���Z�v�g�e�p�񐔗݌v)
018130            COMPUTE �}�b�T�[�W���É񐔂v = �}�b�T�[�W���É񐔂v + 1
018140        ELSE
018150*           �����ɂ��邽�߁A�P�O���|����
018160            COMPUTE ���Ô폜���v = �g���|���Ë��� * 10
018170            COMPUTE ���Ï����v   = ���Ê�{���� * 10
018180            DIVIDE ���Ï����v INTO ���Ô폜���v GIVING ���Ï��v
018190                              REMAINDER ���Ï�]�v
018200            COMPUTE �}�b�T�[�W���×��v = �l���Ê�{�����v +
018210                             ( �l���Òǉ������v * ( ���Ï��v - 1 ) )
018220            IF ���Ï�]�v > ZERO
018230                COMPUTE �}�b�T�[�W���×��v = �}�b�T�[�W���×��v + �l���Òǉ������v
018240            END-IF
018250**------------------------------------------------------------------------------**
018260*           ���×���������Z��@20160901
018270            INITIALIZE �g�A�ŉ��|�L�[
018280            MOVE 2                  TO �g�A�ŉ��|�{�p�敪
018290            MOVE �g���|�{�p�a��N�� TO �g�A�ŉ��|�{�p�a��N��
018300            MOVE �g���|���Ë���     TO �g�A�ŉ��|���Ë���
018310            CALL "MAXOURYO"
018320            IF �g�A�ŉ��|�Y���敪 = 1
018330                COMPUTE �}�b�T�[�W���×��v = �l���Ê�{�����v + �g�A�ŉ��|���É��Z���z
018340            END-IF
018350            CANCEL "MAXOURYO"
018360**------------------------------------------------------------------------------**
018370*
018380*           (���Z�v�g�e�p�񐔁E���Z���E���É��Z�P���A�P���͂P�̂݁��Ō�̓��̃f�[�^�����A�S�����������Ȃ���Ȃ��I)
018390            COMPUTE �}�b�T�[�W���É񐔂v = �}�b�T�[�W���É񐔂v + 1
018400            COMPUTE �}�b�T�[�W���É��Z�񐔂v = �}�b�T�[�W���É��Z�񐔂v + 1
018410            COMPUTE �}�b�T�[�W���É��Z�P���v = �}�b�T�[�W���×��v -  �l���Ê�{�����v
018420            COMPUTE �}�b�T�[�W���É��Z���v = �}�b�T�[�W���É��Z���v + (�}�b�T�[�W���×��v -  �l���Ê�{�����v)
018430**         /-- ���Z�v�g�e�p�F���Z�̉��Ë������Ⴄ�������邩���� --/
018440            IF �ޔ����Ë����v = ZERO
018450               MOVE �g���|���Ë���  TO �ޔ����Ë����v
018460            ELSE
018470               IF �g���|���Ë��� NOT = �ޔ����Ë����v
018480                  MOVE 1 TO ���É��Z��������v
018490               END-IF
018500            END-IF
018510**
018520        END-IF
018530*------------------------------------------------------------------------------------*
018540*    �� ��L��A���̏ꍇ�́A���×����㏑��
018550        IF �g���|���È��敪 = 1
018560           MOVE �g���|���È����z  TO �}�b�T�[�W���×��v
018570        END-IF
018580*------------------------------------------------------------------------------------*
018590*    �� ��L��A���Z�v�g�e�p�ɉ��×���݌v
018600        COMPUTE �}�b�T�[�W�݌v���×��v = �}�b�T�[�W�݌v���×��v + �}�b�T�[�W���×��v
018610*------------------------------------------------------------------------------------*
018620*
018630     END-IF


018640*
018650*    �� ���×��Ȃ��̏ꍇ�̓[���~�㏑��
018660     IF �g�����|���Ȃ�����敪 = 2
018670        ADD 1                    TO ���ÂO�~�J�E���^
018680        ADD 1                    TO �}�b�T�[�W���É񐔂v
018690        MOVE ZERO                TO �}�b�T�[�W���×��v
018700     END-IF

018630     END-IF.
HILO  */�������˔�����

HILO  *     DISPLAY "610-3-4 �}�b�T�[�W���×��v" �}�b�T�[�W���×��v

018710*
018725*
018726*    �{�p�񍐏���t��
018729     IF (�g���|�{�p�a��N����     >= "4301001") AND
018730        (�g���|�{�p�񍐏���t�敪  = 1) AND
018733        (�g���|�{�p�a��N��       >= ��t�\�a��N���v)
018734         ADD 1                      TO �}�b�T�[�W�񍐏���t�񐔂v
018735         ADD �{�p�񍐏���t���P���v TO �}�b�T�[�W�񍐏���t���v
018736         MOVE �{�p�񍐏���t���P���v  TO �v�Z�p�񍐏���t���v
018737     ELSE
018738         MOVE ZERO                    TO �v�Z�p�񍐏���t���v
018739     END-IF.
018743*
018744*--------------------------------------------------------------*
018745*
018746* ���v
018750     COMPUTE �}�b�T�[�W���v�z�v = �}�b�T�[�W���v + �}�b�T�[�W�ό`�k�苸���p�v +
018760                                  �}�b�T�[�W��㪖@���v + �}�b�T�[�W��㪖@�d�×��v + �}�b�T�[�W���×��v +
018761                                  �v�Z�p�񍐏���t���v
      */20240627
                                      + ���ʒn����Z���v.

HILO***     DISPLAY "61-9 " ��|���Ҏ���(1:10) ": �v"       �}�b�T�[�W���v�z�v
HILO***            " ϯ����" �}�b�T�[�W���v
HILO***            " �ό`"   �}�b�T�[�W�ό`�k�苸���p�v
HILO***            " ��"     �}�b�T�[�W��㪖@���v
HILO***            " ���d"   �}�b�T�[�W��㪖@�d�×��v
HILO***            " ����"   �}�b�T�[�W���×��v
HILO***            " ��"     �v�Z�p�񍐏���t���v
HILO***            " ��"     ���ʒn����Z���v



018770* �A���̔�p�z�Z�b�g
018780     MOVE �}�b�T�[�W���v�z�v TO �g�A�v�Z�|��p�z.
018790*
018800* ���Z�p�݌v
018810     COMPUTE �}�b�T�[�W�݌v�z�v = �}�b�T�[�W�݌v�z�v + �}�b�T�[�W���v�z�v.
018820*
018830*--------------------------------------------------------------*
018840*
018850     PERFORM ���v���S�z�擾.
018860*
018870*--------------------------------------------------------------*
018880*
018890*--------------------------------------------------------------*
018900* ���r�������̃��Z�e�p�̏W�v
018910*
018920     IF ( �������r���J�n���v NOT = ZERO ) AND ( �������r���J�n���v <= �g���|�{�p��)
018930*
018940*       ��㪖@
018950        IF �g���|�{�p���e�敪 = 2 OR 5 OR 8
018960           COMPUTE �i�}�b�T�[�W��㪖@�񐔂v = �i�}�b�T�[�W��㪖@�񐔂v + 1
018970            MOVE �l��㪖@���v TO �i�}�b�T�[�W��㪖@���v
018980        END-IF
018990*
019000*       �d�C
019010        IF �g���|�{�p���e�敪 = 3 OR 6 OR 9
019020           COMPUTE �i�}�b�T�[�W��㪖@�d�C�񐔂v = �i�}�b�T�[�W��㪖@�d�C�񐔂v + 1
019030           COMPUTE �i�}�b�T�[�W��㪖@�d�×��v = �l��㪖@���v + �l�d�×��v
019040        END-IF
019050*
019060*       �}�b�T�[�W�E�ό`�k��͓���p�̗L���ł��ƂȂ�
019070        IF �i����{�p�t���O�v NOT = 1
019080           IF �g���|�{�p���e�敪 = 1 OR 2 OR 3 OR 7 OR 8 OR 9
019090               COMPUTE �i�}�b�T�[�W�񐔂v = �i�}�b�T�[�W�񐔂v + 1
019100               COMPUTE �i�}�b�T�[�W���v = �l�P�Ǐ��v * �i�}�b�T�[�W�Ǐ����v
019110           END-IF
019120*
019130           IF �g���|�{�p���e�敪 = 4 OR 5 OR 6 OR 7 OR 8 OR 9
019140               COMPUTE �i�}�b�T�[�W�ό`�k�苸���p�񐔂v = �i�}�b�T�[�W�ό`�k�苸���p�񐔂v + 1
019150               COMPUTE �i�}�b�T�[�W�ό`�k�苸���p�v =  �l�ό`�k�苸���p�v * �i�ό`�k�苸���p���v
019160           END-IF
019170        ELSE
019180*-------   ����{�p���v�Z----------*
019190*          �{�p�ӏ����ƂɌv�Z�i�������r���ύX������ׁA�ēx�W�v�������K�v������j
019200           MOVE ZERO         TO �i�}�b�T�[�W�Ǐ������v�v
019210           MOVE ZERO         TO �i�ό`�k�苸���p�����v�v
019220*
019230           IF (�g���|�}�b�T�[�W�̊�   = 1) OR
019240              (�g���|�}�b�T�[�W�E�㎈ = 1) OR
019250              (�g���|�}�b�T�[�W���㎈ = 1) OR
019260              (�g���|�}�b�T�[�W�E���� = 1) OR
019270              (�g���|�}�b�T�[�W������ = 1)
019280              ADD 1          TO �i�}�b�T�[�W�񐔂v
019290           END-IF
019300
019310           IF �g���|�}�b�T�[�W�̊� = 1
019320              ADD 1          TO �i�}�b�T�[�W�̊��񐔂v �i�}�b�T�[�W�Ǐ������v�v
019330              ADD �l�P�Ǐ��v TO �i�}�b�T�[�W�̊����z�v �i�}�b�T�[�W���v
019340           END-IF
019350           IF �g���|�}�b�T�[�W�E�㎈ = 1
019360              ADD 1          TO �i�}�b�T�[�W�E�㎈�񐔂v �i�}�b�T�[�W�Ǐ������v�v
019370              ADD �l�P�Ǐ��v TO �i�}�b�T�[�W�E�㎈���z�v �i�}�b�T�[�W���v
019380           END-IF
019390           IF �g���|�}�b�T�[�W���㎈ = 1
019400              ADD 1          TO �i�}�b�T�[�W���㎈�񐔂v �i�}�b�T�[�W�Ǐ������v�v
019410              ADD �l�P�Ǐ��v TO �i�}�b�T�[�W���㎈���z�v �i�}�b�T�[�W���v
019420           END-IF
019430           IF �g���|�}�b�T�[�W�E���� = 1
019440              ADD 1          TO �i�}�b�T�[�W�E�����񐔂v �i�}�b�T�[�W�Ǐ������v�v
019450              ADD �l�P�Ǐ��v TO �i�}�b�T�[�W�E�������z�v �i�}�b�T�[�W���v
019460           END-IF
019470           IF �g���|�}�b�T�[�W������ = 1
019480              ADD 1          TO �i�}�b�T�[�W�������񐔂v �i�}�b�T�[�W�Ǐ������v�v
019490              ADD �l�P�Ǐ��v TO �i�}�b�T�[�W���������z�v �i�}�b�T�[�W���v
019500           END-IF
019510*
019520           IF (�g���|�ό`�k�苸���p�E�㎈ = 1) OR
019530              (�g���|�ό`�k�苸���p���㎈ = 1) OR
019540              (�g���|�ό`�k�苸���p�E���� = 1) OR
019550              (�g���|�ό`�k�苸���p������ = 1)
019560              ADD 1          TO �i�}�b�T�[�W�ό`�k�苸���p�񐔂v
019570           END-IF
019580*
019590           IF �g���|�ό`�k�苸���p�E�㎈ = 1
019600              ADD 1                  TO �i�ό`�k�苸���p�E�㎈�񐔂v �i�ό`�k�苸���p�����v�v
019610              ADD �l�ό`�k�苸���p�v TO �i�ό`�k�苸���p�E�㎈���z�v �i�}�b�T�[�W�ό`�k�苸���p�v
019620           END-IF
019630           IF �g���|�ό`�k�苸���p���㎈ = 1
019640              ADD 1                  TO �i�ό`�k�苸���p���㎈�񐔂v �i�ό`�k�苸���p�����v�v
019650              ADD �l�ό`�k�苸���p�v TO �i�ό`�k�苸���p���㎈���z�v �i�}�b�T�[�W�ό`�k�苸���p�v
019660           END-IF
019670           IF �g���|�ό`�k�苸���p�E���� = 1
019680              ADD 1                  TO �i�ό`�k�苸���p�E�����񐔂v �i�ό`�k�苸���p�����v�v
019690              ADD �l�ό`�k�苸���p�v TO �i�ό`�k�苸���p�E�������z�v �i�}�b�T�[�W�ό`�k�苸���p�v
019700           END-IF
019710           IF �g���|�ό`�k�苸���p������ = 1
019720              ADD 1                  TO �i�ό`�k�苸���p�������񐔂v �i�ό`�k�苸���p�����v�v
019730              ADD �l�ό`�k�苸���p�v TO �i�ό`�k�苸���p���������z�v �i�}�b�T�[�W�ό`�k�苸���p�v
019740           END-IF
019750*
019760*          �{�p�ӏ����������p�^�[�����`�F�b�N
019770*          �{�͕̂����p�^�[���ł��A�������r���ύX�ŏ������Z�����V���O���p�^�[���̉\��������
019780           IF �i�}�b�T�[�W�Ǐ����Z�b�g�v NOT = ZERO
019790*             �ŏ���1��Ɣ�r
019800              IF �g���|����{�p�敪 = ZERO
019810                 IF (�i�}�b�T�[�W�Ǐ����o�v NOT = �i�}�b�T�[�W�Ǐ����v) AND
019820                    (�i�}�b�T�[�W�Ǐ����v   NOT = ZERO)
019830                    MOVE 1                        TO �i�}�b�T�[�W�Ǐ��������v
019840                 END-IF
019850              ELSE
019860                 IF (�i�}�b�T�[�W�Ǐ����o�v   NOT = �i�}�b�T�[�W�Ǐ������v�v) AND
019870                    (�i�}�b�T�[�W�Ǐ������v�v NOT = ZERO)
019880                    MOVE 1                        TO �i�}�b�T�[�W�Ǐ��������v
019890                 END-IF
019900              END-IF
019910           ELSE
019920*          �p�^�[������p�ɍŏ���1���ۑ�����B
019930              IF �g���|����{�p�敪 = ZERO
019940                 IF �i�}�b�T�[�W�Ǐ����v NOT = ZERO
019950                    MOVE �i�}�b�T�[�W�Ǐ����v     TO �i�}�b�T�[�W�Ǐ����o�v
019960                    MOVE 1                        TO �i�}�b�T�[�W�Ǐ����Z�b�g�v
019970                 END-IF
019980              ELSE
019990                 IF �i�}�b�T�[�W�Ǐ������v�v NOT = ZERO
020000                    MOVE �i�}�b�T�[�W�Ǐ������v�v TO �i�}�b�T�[�W�Ǐ����o�v
020010                    MOVE 1                        TO �i�}�b�T�[�W�Ǐ����Z�b�g�v
020020                 END-IF
020030              END-IF
020040           END-IF
020050*
020060           IF �i�ό`�k�苸���p���Z�b�g�v NOT = ZERO
020070*             �ŏ���1��Ɣ�r
020080              IF �g���|����{�p�敪 = ZERO
020090                 IF (�i�ό`�k�苸���p���o�v NOT = �i�ό`�k�苸���p���v) AND
020100                    (�i�ό`�k�苸���p���v   NOT = ZERO)
020110                    MOVE 1                        TO �i�ό`�k�苸���p�������v
020120                 END-IF
020130              ELSE
020140                 IF (�i�ό`�k�苸���p���o�v   NOT = �i�ό`�k�苸���p�����v�v) AND
020150                    (�i�ό`�k�苸���p�����v�v NOT = ZERO)
020160                    MOVE 1                        TO �i�ό`�k�苸���p�������v
020170                 END-IF
020180              END-IF
020190           ELSE
020200*             �p�^�[������p�ɍŏ���1���ۑ�����B
020210              IF �g���|����{�p�敪 = ZERO
020220                 IF �i�ό`�k�苸���p���v NOT = ZERO
020230                    MOVE �i�ό`�k�苸���p���v     TO �i�ό`�k�苸���p���o�v
020240                    MOVE 1                        TO �i�ό`�k�苸���p���Z�b�g�v
020250                 END-IF
020260              ELSE
020270                 IF �i�ό`�k�苸���p�����v�v NOT = ZERO
020280                    MOVE �i�ό`�k�苸���p�����v�v TO �i�ό`�k�苸���p���o�v
020290                    MOVE 1                        TO �i�ό`�k�苸���p���Z�b�g�v
020300                 END-IF
020310              END-IF
020320           END-IF
020330        END-IF
020340*
020350***
020360***     20151008 ���ÂO�~�łȂ�������ǉ�
020370        IF (�g���|���Ó��敪 = 1) AND (�g���|���Ë��� NOT = ZERO) AND (�g�����|���Ȃ�����敪 NOT = 2)
020380
020390           IF �g���|���Ë��� <= ���Ê�{����
020400               MOVE �l���Ê�{�����v  TO �i�}�b�T�[�W���×��v
020410*              (���Z�v�g�e�p�񐔗݌v)
020420               COMPUTE �i�}�b�T�[�W���É񐔂v = �i�}�b�T�[�W���É񐔂v + 1
020430           ELSE
020440*              �����ɂ��邽�߁A�P�O���|����
020450               COMPUTE ���Ô폜���v = �g���|���Ë��� * 10
020460               COMPUTE ���Ï����v   = ���Ê�{���� * 10
020470               DIVIDE ���Ï����v INTO ���Ô폜���v GIVING ���Ï��v
020480                                 REMAINDER ���Ï�]�v
020490               COMPUTE �i�}�b�T�[�W���×��v = �l���Ê�{�����v +
020500                                ( �l���Òǉ������v * ( ���Ï��v - 1 ) )
020510               IF ���Ï�]�v > ZERO
020520                   COMPUTE �i�}�b�T�[�W���×��v = �i�}�b�T�[�W���×��v + �l���Òǉ������v
020530               END-IF
020540**------------------------------------------------------------------------------**
020550*              ���×���������Z��@20160901
020560               INITIALIZE �g�A�ŉ��|�L�[
020570               MOVE 2                  TO �g�A�ŉ��|�{�p�敪
020580               MOVE �g���|�{�p�a��N�� TO �g�A�ŉ��|�{�p�a��N��
020590               MOVE �g���|���Ë���     TO �g�A�ŉ��|���Ë���
020600               CALL "MAXOURYO"
020610               IF �g�A�ŉ��|�Y���敪 = 1
020620                   COMPUTE �i�}�b�T�[�W���×��v = �l���Ê�{�����v + �g�A�ŉ��|���É��Z���z
020630               END-IF
020640               CANCEL "MAXOURYO"
020650**------------------------------------------------------------------------------**
020660*
020670*             (���Z�v�g�e�p�񐔁E���Z���E���É��Z�P���A�P���͂P�̂݁��Ō�̓��̃f�[�^�����A�S�����������Ȃ���Ȃ��I)
020680               COMPUTE �i�}�b�T�[�W���É񐔂v = �i�}�b�T�[�W���É񐔂v + 1
020690               COMPUTE �i�}�b�T�[�W���É��Z�񐔂v = �i�}�b�T�[�W���É��Z�񐔂v + 1
020700               COMPUTE �i�}�b�T�[�W���É��Z�P���v = �i�}�b�T�[�W���×��v -  �l���Ê�{�����v
020710               COMPUTE �i�}�b�T�[�W���É��Z���v = �i�}�b�T�[�W���É��Z���v + (�i�}�b�T�[�W���×��v -  �l���Ê�{�����v)
020720*             /-- ���Z�v�g�e�p�F���Z�̉��Ë������Ⴄ�������邩���� --/
020730               IF �i�ޔ����Ë����v = ZERO
020740                  MOVE �g���|���Ë���  TO �i�ޔ����Ë����v
020750               ELSE
020760                  IF �g���|���Ë��� NOT = �i�ޔ����Ë����v
020770                     MOVE 1 TO �i���É��Z��������v
020780                  END-IF
020790               END-IF
020800*
020810           END-IF
020820*------------------------------------------------------------------------------------*
020830*       �� ��L��A���̏ꍇ�́A���×����㏑��
020840           IF �g���|���È��敪 = 1
020850              MOVE �g���|���È����z  TO �i�}�b�T�[�W���×��v
020860           END-IF
020870
020882*------------------------------------------------------------------------------------*
020890*       �� ��L��A���Z�v�g�e�p�ɉ��×���݌v
020900           COMPUTE �i�}�b�T�[�W�݌v���×��v = �i�}�b�T�[�W�݌v���×��v + �i�}�b�T�[�W���×��v
020910*------------------------------------------------------------------------------------*
020920*
020930        END-IF
020940*
020950*       �� ���×��Ȃ��̏ꍇ�̓[���~�㏑��
020960        IF �g�����|���Ȃ�����敪 = 2
020970           ADD 1                    TO ���ÂO�~�J�E���^
020980           ADD 1                    TO �i�}�b�T�[�W���É񐔂v
020990           MOVE ZERO                TO �i�}�b�T�[�W���×��v
021000        END-IF
021010*
021020*       �{�p�񍐏���t��
021021        IF (�g���|�{�p�a��N����     >= "4301001") AND
021022           (�g���|�{�p�񍐏���t�敪  = 1) AND
021023           (�g���|�{�p�a��N��       >= ��t�\�a��N���v)
021024            ADD 1                        TO �i�}�b�T�[�W�񍐏���t�񐔂v
021025            ADD �{�p�񍐏���t���P���v   TO �i�}�b�T�[�W�񍐏���t���v
021026            MOVE �{�p�񍐏���t���P���v  TO �v�Z�p�񍐏���t���v
021028        ELSE
021029            MOVE ZERO                    TO �v�Z�p�񍐏���t���v
021030        END-IF
021031
021032*       �������ɉ��Z
021035        COMPUTE �i�}�b�T�[�W���v�z�v = �i�}�b�T�[�W���v + �i�}�b�T�[�W�ό`�k�苸���p�v +
021040                                       �i�}�b�T�[�W��㪖@���v + �i�}�b�T�[�W��㪖@�d�×��v + �i�}�b�T�[�W���×��v + 
021041                                       �v�Z�p�񍐏���t���v
021051        COMPUTE �i�}�b�T�[�W�݌v�z�v = �i�}�b�T�[�W�݌v�z�v + �i�}�b�T�[�W���v�z�v
021060        COMPUTE �i�}�b�T�[�W�����v�v = �i�}�b�T�[�W�����v�v + �i�}�b�T�[�W���v
021070        COMPUTE �i�ό`�k�藿���v�v   = �i�ό`�k�藿���v�v   + �i�}�b�T�[�W�ό`�k�苸���p�v
021080*
021090     END-IF.
021100*------------------------------------------------------------------------------------*
021110*------------------------------------------------------------------------------------*
021120*
021130*================================================================*
021140 ���v���S�z�擾 SECTION.
021150*
021160* ���S�z�擾
021170*   / �g�A�v�Z�̏����p�����^�N���A�[ /
021180     MOVE SPACE TO �g�A�v�Z�|�������t���O.
021190     MOVE SPACE TO �g�A�v�Z�|���f�Ãt���O.
021200     MOVE ZERO  TO �g�A�v�Z�|������.
021210     MOVE ZERO  TO �g�A�v�Z�|������.
021220     MOVE ZERO  TO �g�A�v�Z�|���̑��v�Z�敪�P.
006620     MOVE ZERO  TO �g�A�v�Z�Q�|���v�敪.
021230*
021240*    2014/05/08 �̎������S���v�Z�敪��3�ǉ��Ή�
021250     IF �̎������S���v�Z�敪�v = 1 OR 2 OR 3
021260*       / 1�~�P�� /
021270        MOVE ZERO TO �g�A�v�Z�|���S���v�Z�敪
021280     ELSE
021290*       / 10�~�P�� /
021300        MOVE 2    TO �g�A�v�Z�|���S���v�Z�敪
021310     END-IF.
021320*
      */���v���S�z�͏����ȉ���ʂ��l�̌ܓ�������/20220601
           IF �g�A�v�Z�|�{�p�a��N�� >= 50406
021270         MOVE ZERO TO �g�A�v�Z�|���S���v�Z�敪
006620         MOVE 1    TO �g�A�v�Z�Q�|���v�敪
           END-IF.
      */���v���S�z�͏����ȉ���ʂ��l�̌ܓ�������/20220601
021330     MOVE �g���|�{�p�� TO �g�A�v�Z�|�{�p��.
021340*
021350*
021360**---  �����E���ۏ؎�------------------------------------------------------------------*
021370*   �����̊Y���{�p���܂ł̉񐔁i����ڂ��j(�}�Ԃ��܂�)(���S�敪6,8,13�p)���Z�b�g����B
021380     IF ( ������ʂv NOT = ZERO ) OR ( ���ۏ؃t���O = "YES" )
021390         MOVE ZERO TO �񐔃J�E���^
021400         PERFORM VARYING �J�E���^ FROM 1 BY 1
021410                         UNTIL ( �J�E���^ > 31 ) OR ( �J�E���^ > �g���|�{�p�� )
021420            IF �ʉ@���Q�v(�J�E���^) NOT = ZERO
021430*              / �������r���J�n���l�� /
021440               IF ( �������r���J�n���v = ZERO ) OR
021450                  (( �������r���J�n���v NOT = ZERO ) AND ( �J�E���^ >= �������r���J�n���v ))
021460                  COMPUTE �񐔃J�E���^ = �񐔃J�E���^ + 1
021470               END-IF
021480            END-IF
021490         END-PERFORM
021500*
021510         MOVE �񐔃J�E���^  TO �g�A�v�Z�|������
021520*----------------------------------------------*
021530**** 2017/08/31 ���s�̏�Q�҈�Ï����ɂ��Ă͂��ꂼ��T�O�O�~�Q�񒥎�����l�Ɏd�l�ύX�i���ꃍ�W�b�N�R�����g�A�E�g�j
021540**        / ��L�� ���s�̂݁A�͂肫�イ�ƃ}�b�T�[�W�����̓��́A�}�b�T�[�W�̕��́A0��ڂƂ���
021550**         IF ��p���S�Ҕԍ������v(3:3) = "274"
021560**            IF �ʉ@���v(�g���|�{�p��) = 3
021570**               IF �g���|�{�p�敪 = 2
021580**                  MOVE ZERO TO �g�A�v�Z�|������
021590**               END-IF
021600**            END-IF
021610**         END-IF
021620**
021630*----------------------------------------------*
021640*
021650     END-IF.
021660**--------------------------------------------------------------------------------------*
021670*
021680*
021690     MOVE ���ۏ؃t���O TO �g�A�v�Z���|���ۏ؃t���O.
021700*
021710
021720
021730     MOVE ZERO TO �A�|���v���Z�e
021740
021750     CALL   "KHT41410".
021760     CANCEL "KHT41410".
021770*
021780* �t�@�C���փZ�b�g
021790     MOVE �g�A�v�Z�|��p�z  TO  �g���|��p�z.
021800*
021810*    / �����́A���S�z�������Z�b�g /
021820     IF ������ʂv NOT = ZERO
021830        MOVE �g�A�v�Z�|���S�z���� TO �g���|�ꕔ���S��  �g���|��O�ꕔ���S��
      */���S�z�؂�グ�ɑΉ�������/20221125
              IF (��|��p���S�Ҕԍ�����(3:2) = "27") AND ( ���S�敪�v = 06 OR 22 )
021830            MOVE �g�A�v�Z�R�|���S�z���� TO �g���|�ꕔ���S��  �g���|��O�ꕔ���S��
              END-IF
      */���S�z�؂�グ�ɑΉ�������/20221125
021840*
021850*       / �݌v���� /
021860        IF ( ���S�敪�v = 05 OR 16 )
021870*
021880           IF ( �������r���J�n���v = ZERO )
021890               COMPUTE �g�A�v�Z�����݌v�z�|���S�z���� = �g�A�v�Z�����݌v�z�|���S�z���� + �g�A�v�Z�|���S�z����
021900           ELSE
021910              IF ( �g�A�v�Z�|�{�p�� >= �������r���J�n���v )
021920                  COMPUTE �g�A�v�Z�����݌v�z�|���S�z���� = �g�A�v�Z�����݌v�z�|���S�z���� + �g�A�v�Z�|���S�z����
021930              END-IF
021940           END-IF
021950        END-IF
021978     ELSE
021980        MOVE �g�A�v�Z�|���S�z     TO �g���|�ꕔ���S��  �g���|��O�ꕔ���S��
021990     END-IF.
022000*
022010*
022020*---------------------------------------------------------------------------------------*
022030* �� �����z�́A��v�̎�F�ɂȂ����A�ꕔ���S�� + ����ōČv�Z
022040     MOVE SPACE TO ��v�̎������t���O.
022050     MOVE �g���|�{�p�敪     TO �́|�{�p�敪.
022060* 1:���A2:���A3:�N
022070     MOVE 1                  TO �́|��v�̎��敪.
022080     MOVE �g���|�{�p�a��     TO �́|�{�p�a��.
022090     MOVE �g���|�{�p�N       TO �́|�{�p�N.
022100     MOVE �g���|�{�p��       TO �́|�{�p��.
022110     MOVE �g���|�{�p��       TO �́|�{�p��.
022120     MOVE �g���|���Ҕԍ�     TO �́|���Ҕԍ�.
022130     MOVE �g���|�}��         TO �́|�}��.
022140     READ ��v�̎��e
022150     INVALID KEY
022160*
022170        COMPUTE �g���|�����z = �g���|�ꕔ���S�� + �g���|����z
022180        MOVE "YES" TO ��v�̎������t���O
022190*
022200     END-READ.
022210*---------------------------------------------------------------------------------------*
022220*
022230*
022240*
022250*---------------------------------------------------------------------------------------*
022260**��--- ���ҕ���+ �����E���ۏ؎�------------------------------------------------------*
022270* ��L�̒ʏ���z���Z�b�g��A���S�z��1�~�P�ʂŎZ�o(���S�敪6,8+���ۏؗp)
022280     IF ( ������ʂv NOT = ZERO  ) OR ( ���ۏ؃t���O = "YES" ) OR
022290        ( �{�̏��ҕ����敪�v = 1 ) OR ( �������ҕ����敪�v = 1 ) OR ( �{�̂܂Ƃߋ敪�v = 1 )
022300*
022310        MOVE ZERO  TO �g�A�v�Z�|���S���v�Z�敪
022320
022330
022340        MOVE 1     TO �A�|���v���Z�e
022350
022360        CALL   "KHT41410"
022370        CANCEL "KHT41410"
022380*
022390        IF ( ������ʂv NOT = ZERO )
022400           MOVE �g�A�v�Z�|���S�z���� TO �g���|�����v�Z���S�z  �g���|��O�����v�Z���S�z
      */���S�z�؂�グ�ɑΉ�������/20221125
                 IF (��|��p���S�Ҕԍ�����(3:2) = "27") AND ( ���S�敪�v = 06 OR 22 )
022400                MOVE �g�A�v�Z�R�|���S�z���� TO �g���|�����v�Z���S�z  �g���|��O�����v�Z���S�z
                 END-IF
      */���S�z�؂�グ�ɑΉ�������/20221125
022410        ELSE
022420           MOVE �g�A�v�Z�|���S�z     TO �g���|�����v�Z���S�z  �g���|��O�����v�Z���S�z
022430        END-IF
022440*
022450*
022460*---------------------------------------------------------------------------*
022470*** ��L��i�Ō�Ɂj�A���ҕ����E�{�̂܂Ƃ߂̋��z���Z�o���A�Z�b�g�������B
022480* �g���|��O�ꕔ���S���Ƃg���|��O�����v�Z���S�z�́A�ʏ�i���҂łȂ��j�l���Z�b�g����Ă���B
022490*
022500        IF ( �{�̏��ҕ����敪�v = 1 ) OR ( �������ҕ����敪�v = 1 ) OR ( �{�̂܂Ƃߋ敪�v = 1 )
022510           IF ( �{�̏��ҕ����敪�v = 1 ) OR ( �������ҕ����敪�v = 1 )
022520              IF ������ʂv = ZERO
022530*                /�{�̂̂ݏ���/
022540                 IF �{�̏��ҕ����敪�v = 1
022550                    MOVE �g���|��p�z  TO �g���|�ꕔ���S�� �g���|�����v�Z���S�z
022560                 END-IF
022570              ELSE
022580                 EVALUATE TRUE
022590*                /�{�̏��ҁA�����ʏ�/
022600                 WHEN ( �{�̏��ҕ����敪�v = 1 ) AND ( �������ҕ����敪�v = ZERO )
022610                    COMPUTE �g���|�ꕔ���S�� =  ( �g���|��p�z - �g�A�v�Z�|���S�z ) + �g�A�v�Z�|���S�z����
022620                    MOVE �g���|�ꕔ���S��  TO �g���|�����v�Z���S�z
022630*                /�{�̒ʏ�A��������/
022640                 WHEN ( �{�̏��ҕ����敪�v = ZERO ) AND ( �������ҕ����敪�v = 1 )
022650                    MOVE �g�A�v�Z�|���S�z  TO �g���|�ꕔ���S�� �g���|�����v�Z���S�z
022660*                /�{�̏��ҁA��������/
022670                 WHEN ( �{�̏��ҕ����敪�v = 1 ) AND ( �������ҕ����敪�v = 1 )
022680                    MOVE �g���|��p�z  TO �g���|�ꕔ���S�� �g���|�����v�Z���S�z
022690                 END-EVALUATE
022700              END-IF
022710           ELSE
022720*             / �{�̂܂Ƃ߂̂� /
022730*             ����29�N4��1���ȍ~�ł͈ꕔ���S�L��̏ꍇ�̂݁A�{�̂܂Ƃ߂ł����Ă����S�z���o�� 2017/02/27 2017/04/03
022740              IF (������ʂv                 = 55) AND
022750                 (��p���S�Ҕԍ������v(3:2)  = "14") AND
022760                 (���S�敪�v             NOT = ZERO)
022770                 MOVE "YES"  TO �������S����t���O
022780              ELSE
022790                  MOVE ZERO  TO �g���|�ꕔ���S�� �g���|�����v�Z���S�z
022800              END-IF
022810           END-IF
022820**
022830*          �����z�́A��v�̎�F�ɂȂ����A�ꕔ���S�� + ����ōČv�Z
022840           IF ��v�̎������t���O = "YES"
022850              COMPUTE �g���|�����z = �g���|�ꕔ���S�� + �g���|����z
022860           END-IF
022870*
022880        END-IF
022890*---------------------------------------------------------------------------*
022900     END-IF.
022910*---------------------------------------------------------------------------------------*
022920*
022930*================================================================*
022940*================================================================*
022950*================================================================*
022960 ���Z�v�g���� SECTION.
022970*
006460     INITIALIZE �g�A�v�Z�Q�|�L�[.
022980     MOVE �g�A�v�Z�|�{�p�敪  TO  �g���Z�|�{�p�敪.
022990     MOVE �g�A�v�Z�|�{�p�a��  TO  �g���Z�|�{�p�a��.
023000     MOVE �g�A�v�Z�|�{�p�N    TO  �g���Z�|�{�p�N.
023010     MOVE �g�A�v�Z�|�{�p��    TO  �g���Z�|�{�p��.
023020     MOVE �g�A�v�Z�|���Ҕԍ�  TO  �g���Z�|���Ҕԍ�.
023030     MOVE �g�A�v�Z�|�}��      TO  �g���Z�|�}��.
023040     MOVE 1                   TO  �g���Z�|���Z���.
023050*
023060     START �g���Z�v�g�e KEY IS >= �g���Z�|�{�p�敪
023070                                  �g���Z�|�{�p�a��N��
023080                                  �g���Z�|���҃R�[�h
023090                                  �g���Z�|���Z���
023100     END-START.
023110*
023120     IF ��ԃL�[ = "00"
023130         MOVE SPACE TO ����t���O
023140         MOVE SPACE TO �I���t���O
023150         PERFORM �g���Z�v�g�e�Ǎ�
023160*
023170*        �J��Ԃ��i�������邩���j
023180         PERFORM UNTIL ( �g���Z�|�{�p�敪   NOT = �g�A�v�Z�|�{�p�敪   ) OR
023190                       ( �g���Z�|�{�p�a��   NOT = �g�A�v�Z�|�{�p�a��   ) OR
023200                       ( �g���Z�|�{�p�N     NOT = �g�A�v�Z�|�{�p�N     ) OR
023210                       ( �g���Z�|�{�p��     NOT = �g�A�v�Z�|�{�p��     ) OR
023220                       ( �g���Z�|���҃R�[�h NOT = �g�A�v�Z�|���҃R�[�h ) OR
023230                       ( �I���t���O         NOT = SPACE )
023240*
023250**           / �t�@�C���̋��z�������� /
023260             INITIALIZE �g���Z�|���z���v��
023270             INITIALIZE �g���Z�|�͂���z��
023280             INITIALIZE �g���Z�|�}�b�T�[�W���z��
023290             INITIALIZE �g���Z�|��O���Z���z���v��
023300             INITIALIZE �g���Z�|��O�{�̎}�Ԃ܂ƂߒP�Ƌ��z���v��
023310             INITIALIZE �g���Z�|�{�p�񍐏���t����
023311*
023320             INITIALIZE �g���Z�|�}�b�T�[�W���ʕʋ��z��
023330**
023340             IF ����t���O = SPACE
023350                EVALUATE �g���Z�|�{�p�敪
023360                WHEN 1
023370                   IF �͂�݌v�z�v NOT = ZERO
023380                      PERFORM �͂背�Z�����v�Z
023390                   END-IF
023400                WHEN 2
023410                   IF �}�b�T�[�W�݌v�z�v NOT = ZERO
023420                      PERFORM �}�b�T�[�W���Z�����v�Z
023430                   END-IF
023440                WHEN OTHER
023450                   DISPLAY "���Z�{�p�敪�G���[�@�N���F" �g���Z�|�{�p�a��N��   UPON CONS
023460                END-EVALUATE
023470*
023480*              / �������R�[�h�̂��߂ɑޔ� /
023490                INITIALIZE �ޔ����Z�|���R�[�h
023500                MOVE �g���Z�|���R�[�h TO �ޔ����Z�|���R�[�h
023510                MOVE "YES" TO ����t���O
023520             ELSE
023530                IF �g���Z�|���Z��� = 3
023540*
023550                   IF �������r���J�n���v = ZERO
023560*                     / �������R�[�h���́A�ޔ������f�[�^���Z�b�g /
023570                      MOVE �ޔ����Z�|���z���v��         TO �g���Z�|���z���v��
023580                      MOVE �ޔ����Z�|�͂���z��         TO �g���Z�|�͂���z��
023590                      MOVE �ޔ����Z�|�}�b�T�[�W���z��   TO �g���Z�|�}�b�T�[�W���z��
023600                      MOVE �ޔ����Z�|��O���Z���z���v�� TO �g���Z�|��O���Z���z���v��
023610*                     / �������z�Z�b�g /
023620                      MOVE �g�A�v�Z�|���S�z����         TO �g���Z�|�󋋎ҕ��S�z
023630                      MOVE �g�A�v�Z�|�����z����         TO �g���Z�|�����������z
      */�����z�����̂U��ver������/20231213
                            IF (�g�A�v�Z�|�����z�����Q NOT = ZERO) AND (�g�A�v�Z�|�����z�����Q > �g�A�v�Z�|�����z����)
027010                          MOVE �g�A�v�Z�|�����z�����Q TO �g���Z�|�����������z
                            END-IF
      */�����z�����̂U��ver������/20231213
      */���S�z�؂�グ�ɑΉ�������/20221125
                            IF (��|��p���S�Ҕԍ�����(3:2) = "27") AND ( ���S�敪�v = 06 OR 22 )
023620                          MOVE �g�A�v�Z�R�|���S�z����   TO �g���Z�|�󋋎ҕ��S�z
                            END-IF
      */���S�z�؂�グ�ɑΉ�������/20221125
      */��㏕���F���̏����1�x���z���Ȃ��ꍇ�A�{�̂̕��S�z�ƍ��������Ă��������Ȃ�������/20220830
                            IF (���S�敪�v = 22 AND ��|��p���S�Ҕԍ�����(3:2) = "27" AND �S�z���ҕ��S�e = "YES")
                                MOVE �g���Z�|���S�z TO �g���Z�|�󋋎ҕ��S�z
                                MOVE ZERO           TO �g���Z�|�����������z
                            END-IF
      */��㏕���F���̏����1�x���z���Ȃ��ꍇ�A�{�̂̕��S�z�ƍ��������Ă��������Ȃ�������/20220830
023640*
023650*                     �{�̕��v�Z�l�œ���{�p���e���Z�b�g�i20180118 �}�b�T�[�W�ŗL�ǉ��j
023660                      MOVE �ޔ����Z�|�}�b�T�[�W�ŗL     TO �g���Z�|�}�b�T�[�W�ŗL
023670                      MOVE �ޔ����Z�|�}�b�T�[�W���ʕʋ��z�� TO �g���Z�|�}�b�T�[�W���ʕʋ��z��
023680
023681*                     �{�p�񍐏�
023682                      MOVE �ޔ����Z�|�{�p�񍐏���t���� TO �g���Z�|�{�p�񍐏���t����
023683                   ELSE
023690*                     / �������r���́A�i�݌v���v��݌v���v�֓]�L��A���Z�����v�Z /
023700                      MOVE �i�݌v���v                 TO �݌v���v
023710                      MOVE �i�}�b�T�[�W�ŗL�v           TO �}�b�T�[�W�ŗL�v
023720                      MOVE �i�}�b�T�[�W���ʕʋ��z���v   TO �}�b�T�[�W���ʕʋ��z���v
023730                      EVALUATE �g���Z�|�{�p�敪
023740                      WHEN 1
023750                         IF �͂�݌v�z�v NOT = ZERO
023760                            PERFORM �͂背�Z�����v�Z
023770                         END-IF
023780                      WHEN 2
023790                         IF �}�b�T�[�W�݌v�z�v NOT = ZERO
023800                            PERFORM �}�b�T�[�W���Z�����v�Z
023810                         END-IF
023820                      END-EVALUATE
023830*                     / �������z�Z�b�g /
023840                      MOVE �g�A�v�Z�|���S�z����         TO �g���Z�|�󋋎ҕ��S�z
023850                      MOVE �g�A�v�Z�|�����z����         TO �g���Z�|�����������z
      */���S�z�؂�グ�ɑΉ�������/20221125
                            IF (��|��p���S�Ҕԍ�����(3:2) = "27") AND ( ���S�敪�v = 06 OR 22 )
023840                          MOVE �g�A�v�Z�R�|���S�z����   TO �g���Z�|�󋋎ҕ��S�z
                            END-IF
      */���S�z�؂�グ�ɑΉ�������/20221125
023862*
023870                   END-IF
023880*                  / ��L����{ /
023890                   PERFORM ��O�������Z���z�Z�b�g
023900*
023910*                  2017/02/27 ��Ï������{�̂܂Ƃ߂ł����Ă���������v�Z������
023920*                  ����29�N4��1���ȍ~�ł͈ꕔ���S�L��̏ꍇ�̂݁A�{�̂܂Ƃ߂ł����Ă����S�z���o�� 2017/02/27 2017/04/03
023930*                  �{�̂܂Ƃ߂��s���Ă���ꍇ�A�{�̕��ɏ������̈ꕔ���S�z�𔽉f������
023940                   IF ( �{�̂܂Ƃߋ敪�v       = 1 ) AND
023950                      (�������S����t���O  NOT = SPACE)
023960                      INITIALIZE �X�V�g���Z�|���R�[�h
023970                      MOVE �ޔ����Z�|���R�[�h�L�[  TO  �X�V�g���Z�|���R�[�h�L�[
023980                      READ �X�V�g���Z�v�g�e
023990                      NOT INVALID KEY
024000*                         �{�̕��ɂ܂Ƃ߂鏕�����𒲐�����
024010*       �ς��Ȃ� ->     COMPUTE �X�V�g���Z�|��p�z = �X�V�g���Z�|��p�z
024020                          COMPUTE �X�V�g���Z�|���S�z = �X�V�g���Z�|���S�z + �g�A�v�Z�|���S�z����
024030                          COMPUTE �X�V�g���Z�|�����z = �X�V�g���Z�|�����z - �g�A�v�Z�|���S�z����
024040*                         ��������ݒ�
024050                          MOVE �g�A�v�Z�|���S�z����    TO  �X�V�g���Z�|�󋋎ҕ��S�z
024060                          MOVE �g�A�v�Z�|�����z����    TO  �X�V�g���Z�|�����������z
024070                          MOVE �X�V�g���Z�|���S�z      TO  �X�V�g���Z�|��O���S�z
024080                          MOVE �X�V�g���Z�|�����z      TO  �X�V�g���Z�|��O�����z
024090                          MOVE �g�A�v�Z�|���S�z����    TO  �X�V�g���Z�|��O�󋋎ҕ��S�z
024100                          MOVE �g�A�v�Z�|�����z����    TO  �X�V�g���Z�|��O�����������z
024110*                         �{�̃��R�[�h���X�V
024120                          REWRITE �X�V�g���Z�|���R�[�h
024130                          IF ��ԃL�[ NOT = "00"
024140                             MOVE NC"�X�V�g���Z" TO �t�@�C����
024150                             PERFORM �G���[�\��
024160                          END-IF
024170                      END-READ
024180                   END-IF
024190*
024200                ELSE
024210                   DISPLAY "���Z��ʂ����ۈȊO�ł��B�N���|��ʁF" �g���Z�|�{�p�a��N�� "�|"  �g���Z�|���Z��� UPON CONS
024220                END-IF
024230             END-IF
024240**
      *����{�p��/20241203������
                   IF �g���Z�|�ό`�k�苸���p����{�p�t���O = 1
002854                 COMPUTE �ό`�k�苸���p�񐔂v = �g���Z�|�ό`�k�苸���p�E�㎈�� +
002857                                                �g���Z�|�ό`�k�苸���p���㎈�� +
002860                                                �g���Z�|�ό`�k�苸���p�E������ +
002864                                                �g���Z�|�ό`�k�苸���p��������
                       IF ((�g���Z�|�ό`�k�苸���p�E�㎈���z NOT = ZERO)  OR
                           (�g���Z�|�ό`�k�苸���p���㎈���z NOT = ZERO)  OR
                           (�g���Z�|�ό`�k�苸���p�E�������z NOT = ZERO)  OR
                           (�g���Z�|�ό`�k�苸���p���������z NOT = ZERO)) AND
                          ( �ό`�k�苸���p�񐔂v             NOT = ZERO )
002855                 COMPUTE �ό`�k�苸���p�P���v = 
002855                                               (�g���Z�|�ό`�k�苸���p�E�㎈���z +
002858                                                �g���Z�|�ό`�k�苸���p���㎈���z +
002861                                                �g���Z�|�ό`�k�苸���p�E�������z +
002865                                                �g���Z�|�ό`�k�苸���p���������z) / �ό`�k�苸���p�񐔂v
                       END-IF
                   ELSE
      *����{�p��/20241203������
                   IF �g���Z�|�ό`�k�苸���p�� NOT = ZERO
                       COMPUTE �ό`�k�苸���p�P���v = �g���Z�|�ό`�k�苸���p�P�� / 
                              (�g���Z�|�ό`�k�苸���p�E�㎈ + �g���Z�|�ό`�k�苸���p���㎈ +
                               �g���Z�|�ό`�k�苸���p�E���� + �g���Z�|�ό`�k�苸���p������ )
                       COMPUTE �ό`�k�苸���p�񐔂v = �g���Z�|�ό`�k�苸���p�� * 
                              (�g���Z�|�ό`�k�苸���p�E�㎈ + �g���Z�|�ό`�k�苸���p���㎈ +
                               �g���Z�|�ό`�k�苸���p�E���� + �g���Z�|�ό`�k�苸���p������ )
                   ELSE
                       COMPUTE �ό`�k�苸���p�P���v = �g���Z�|�ό`�k�苸���p�P��
                       COMPUTE �ό`�k�苸���p�񐔂v = 
                               �g���Z�|�ό`�k�苸���p�E�㎈�� + �g���Z�|�ό`�k�苸���p���㎈�� +
                               �g���Z�|�ό`�k�苸���p�E������ + �g���Z�|�ό`�k�苸���p��������
                   END-IF
      *����{�p��/20241203��
                   END-IF
      *
024251             REWRITE �g���Z�|���R�[�h
024260             IF ��ԃL�[ NOT = "00"
024270                  MOVE NC"�g���Z�v�g�e" TO �t�@�C����
024280                  PERFORM �G���[�\��
024290             END-IF
024300**
024310             PERFORM �g���Z�v�g�e�Ǎ�
024320         END-PERFORM
024330     END-IF.
024340*
024350*================================================================*
024360 �g���Z�v�g�e�Ǎ� SECTION.
024370*
024380     READ �g���Z�v�g�e NEXT
024390     AT END
024400         MOVE "YES"  TO �I���t���O
024410     END-READ.
024420*
024430*================================================================*
024440 �͂背�Z�����v�Z SECTION.
024450*
024460*--���ׁi���v���̗݌v�����Z�b�g�j-------------*
024470*
024480* �͂菉��
024490     MOVE �͂菉��{�p���e�敪�v TO �g���Z�|�͂菉��{�p���e�敪.
024500     IF �͂菉��{�p���e�敪�v NOT = ZERO
024510        MOVE �͂菉�񗿂v TO �g���Z�|�͂菉��
024520     END-IF.
024530* ��Â͂�
024540     IF �͂�񐔂v NOT = ZERO
024550        MOVE �g�P�p�v   TO �g���Z�|��ÒP���͂�
024560     END-IF.
024570     MOVE �͂�񐔂v    TO �g���Z�|��É񐔂͂�.
024580     COMPUTE �g���Z�|��×��͂� = �g���Z�|��ÒP���͂� * �g���Z�|��É񐔂͂�.
024590* ��Â͂�d�C
024600     IF �͂�d�C�񐔂v NOT = ZERO
024610        COMPUTE �g���Z�|��ÒP���͂�d�C = �g�P�p�v + �g�d�×��v
024620     END-IF.
024630     MOVE �͂�d�C�񐔂v TO �g���Z�|��É񐔂͂�d�C.
024640     COMPUTE �g���Z�|��×��͂�d�C = �g���Z�|��ÒP���͂�d�C * �g���Z�|��É񐔂͂�d�C.
024650* ��Â��イ
024660     IF ���イ�񐔂v NOT = ZERO
024670        MOVE �g�P�p�v   TO �g���Z�|��ÒP�����イ
024680     END-IF.
024690     MOVE ���イ�񐔂v  TO �g���Z�|��É񐔂��イ.
024700     COMPUTE �g���Z�|��×����イ = �g���Z�|��ÒP�����イ * �g���Z�|��É񐔂��イ.
024710* ��Â��イ�d�C
024720     IF ���イ�d�C�񐔂v NOT = ZERO
024730        COMPUTE �g���Z�|��ÒP�����イ�d�C = �g�P�p�v + �g�d�×��v
024740     END-IF.
024750     MOVE ���イ�d�C�񐔂v TO �g���Z�|��É񐔂��イ�d�C.
024760     COMPUTE �g���Z�|��×����イ�d�C = �g���Z�|��ÒP�����イ�d�C * �g���Z�|��É񐔂��イ�d�C.
024770* ��Â͂肫�イ
024780     IF �͂肫�イ�񐔂v NOT = ZERO
024790        MOVE �g�Q�p�v   TO �g���Z�|��ÒP���͂肫�イ
024800     END-IF.
024810     MOVE �͂肫�イ�񐔂v  TO �g���Z�|��É񐔂͂肫�イ.
024820     COMPUTE �g���Z�|��×��͂肫�イ = �g���Z�|��ÒP���͂肫�イ * �g���Z�|��É񐔂͂肫�イ.
024830* ��Â͂肫�イ�d�C
024840     IF �͂肫�イ�d�C�񐔂v NOT = ZERO
024850        COMPUTE �g���Z�|��ÒP���͂肫�イ�d�C = �g�Q�p�v + �g�d�×��v
024860     END-IF.
024870     MOVE �͂肫�イ�d�C�񐔂v TO �g���Z�|��É񐔂͂肫�イ�d�C.
024880     COMPUTE �g���Z�|��×��͂肫�イ�d�C = �g���Z�|��ÒP���͂肫�イ�d�C * �g���Z�|��É񐔂͂肫�イ�d�C.
024890*
024900* ���Ê�{

HILO***       DISPLAY "61-5-1�� �͂艝�É񐔂v " �͂艝�É񐔂v

024910     IF �͂艝�É񐔂v NOT = ZERO
024920        MOVE �g���Ê�{�����v   TO �g���Z�|�͂艝�ÒP��
024930     END-IF.
024940     MOVE �͂艝�É񐔂v  TO �g���Z�|�͂艝�É�.
024950*----------------------------------------------------*
024960*   / ���Ή� /  ���ÂO�~�L������ǉ�
024970     IF (�g���Z�|���Z���È��敪 NOT = 1) AND (���ÂO�~�J�E���^ = ZERO)
024980        COMPUTE �g���Z�|�͂艝�×� = �g���Z�|�͂艝�ÒP�� * �g���Z�|�͂艝�É�
024990     ELSE
025000        MOVE �͂�݌v���×��v TO �g���Z�|�͂艝�×�
025010     END-IF.
025020*----------------------------------------------------*
025030*
025040* ���É��Z
025050     IF ���É��Z��������v NOT = 1
025060        IF �͂艝�É��Z�񐔂v NOT = ZERO
025070           MOVE �͂艝�É��Z�P���v  TO �g���Z�|�͂艝�É��Z�P��
025080        END-IF
025090     ELSE
025100*     / ���Z���������̎� /
025110        MOVE 1 TO �g���Z�|�͂艝�É��Z��������
025120     END-IF.
025130     MOVE �͂艝�É��Z�񐔂v     TO �g���Z�|�͂艝�É��Z��.
025140     MOVE �͂艝�É��Z���v       TO �g���Z�|�͂艝�É��Z��.
025150***     COMPUTE �g���Z�|�͂艝�É��Z�� = �g���Z�|�͂艝�É��Z�P�� * �g���Z�|�͂艝�É��Z��.
025160*
025170*    �{�p�񍐏���t��
025172
025182     IF �͂�񍐏���t���v NOT = ZERO
025187         MOVE �͂�񍐏���t�񐔂v    TO �g���Z�|�{�p�񍐏���t��
025188         MOVE �{�p�񍐏���t���P���v  TO �g���Z�|�{�p�񍐏���t���P��
025190         MOVE �͂�񍐏���t���v      TO �g���Z�|�{�p�񍐏���t��
025192     ELSE
025193         MOVE ZERO                    TO �g���Z�|�{�p�񍐏���t��
025194         MOVE ZERO                    TO �g���Z�|�{�p�񍐏���t���P��
025195         MOVE ZERO                    TO �g���Z�|�{�p�񍐏���t��
025196     END-IF.
025197*
025198*--���v---------------------------------------------------------*
025199*
025200* �A���̔�p�z�Z�b�g
025201     MOVE �͂�݌v�z�v  TO �g�A�v�Z�|��p�z.
025210*
025221     PERFORM ���Z���S�z�擾.
025230*
025240*--------------------------------------------------------------*
025250*
025260*
025270*================================================================*
025280 �}�b�T�[�W���Z�����v�Z SECTION.
025290*
025300*--���ׁi���v���̗݌v�����Z�b�g�j-------------*
025310*
025320*    �}�b�T�[�W
025330     IF �}�b�T�[�W�񐔂v NOT = ZERO
025340        MOVE �l�P�Ǐ��v         TO �g���Z�|�}�b�T�[�W�P��
025350        MOVE �}�b�T�[�W�Ǐ����v TO �g���Z�|�}�b�T�[�W�Ǐ���
025360     END-IF.
025370     MOVE �}�b�T�[�W�񐔂v      TO �g���Z�|�}�b�T�[�W��.
025380     COMPUTE �g���Z�|�}�b�T�[�W�� = �g���Z�|�}�b�T�[�W�P�� * �g���Z�|�}�b�T�[�W�Ǐ��� * �g���Z�|�}�b�T�[�W��.
025390*    �ό`�k�苸���p
025400     IF �}�b�T�[�W�ό`�k�苸���p�񐔂v NOT = ZERO
025410        COMPUTE �g���Z�|�ό`�k�苸���p�P�� = �l�ό`�k�苸���p�v * �ό`�k�苸���p���v
025420     END-IF.
025430     MOVE �}�b�T�[�W�ό`�k�苸���p�񐔂v   TO �g���Z�|�ό`�k�苸���p��.
025440     COMPUTE �g���Z�|�ό`�k�苸���p�� = �g���Z�|�ό`�k�苸���p�P�� * �g���Z�|�ό`�k�苸���p��.
025450*
025460* ��㪖@
025470     IF �}�b�T�[�W��㪖@�񐔂v NOT = ZERO
025480        MOVE �l��㪖@���v   TO �g���Z�|��㪖@�P��
025490     END-IF.
025500     MOVE �}�b�T�[�W��㪖@�񐔂v   TO �g���Z�|��㪖@��.
025510     COMPUTE �g���Z�|��㪖@�� = �g���Z�|��㪖@�P�� * �g���Z�|��㪖@��.
025520* ��㪖@+�d�C
025530     IF �}�b�T�[�W��㪖@�d�C�񐔂v NOT = ZERO
025540        COMPUTE �g���Z�|��㪖@�d�C�P�� = �l��㪖@���v + �l�d�×��v
025550     END-IF.
025560     MOVE �}�b�T�[�W��㪖@�d�C�񐔂v   TO �g���Z�|��㪖@�d�C��.
025570     COMPUTE �g���Z�|��㪖@�d�C�� = �g���Z�|��㪖@�d�C�P�� * �g���Z�|��㪖@�d�C��.
025580*
025590* ���Ê�{
025600     IF �}�b�T�[�W���É񐔂v NOT = ZERO
025610        MOVE �l���Ê�{�����v   TO �g���Z�|�}�b�T�[�W���ÒP��
025620     END-IF.
025630     MOVE �}�b�T�[�W���É񐔂v  TO �g���Z�|�}�b�T�[�W���É�.
025640*----------------------------------------------------*
025650*   / ���Ή� /  ���ÂO�~�L������ǉ�
025660     IF (�g���Z�|���Z���È��敪 NOT = 1) AND (���ÂO�~�J�E���^ = ZERO)
025670        COMPUTE �g���Z�|�}�b�T�[�W���×� = �g���Z�|�}�b�T�[�W���ÒP�� * �g���Z�|�}�b�T�[�W���É�
025680     ELSE
025690        MOVE �}�b�T�[�W�݌v���×��v TO �g���Z�|�}�b�T�[�W���×�
025700     END-IF.
025710*----------------------------------------------------*
025720*
025730* ���É��Z
025740     IF ���É��Z��������v NOT = 1
025750        IF �}�b�T�[�W���É��Z�񐔂v NOT = ZERO
025760           MOVE �}�b�T�[�W���É��Z�P���v  TO �g���Z�|�}�b�T�[�W���É��Z�P��
025770        END-IF
025780     ELSE
025790*     / ���Z���������̎� /
025800        MOVE 1 TO �g���Z�|�}�b�T�[�W���É��Z��������
025810     END-IF.
025820     MOVE �}�b�T�[�W���É��Z�񐔂v        TO �g���Z�|�}�b�T�[�W���É��Z��.
025830     MOVE �}�b�T�[�W���É��Z���v          TO �g���Z�|�}�b�T�[�W���É��Z��.
025840***     COMPUTE �g���Z�|�}�b�T�[�W���É��Z�� = �g���Z�|�}�b�T�[�W���É��Z�P�� * �g���Z�|�}�b�T�[�W���É��Z��.
025850*
025860*    ���ۂ̎{�p���e��ݒ肷��
025870     IF ����{�p�t���O�v = 1
025880*       �{�p���̏��Z�b�g
025890        IF �}�b�T�[�W����{�p�t���O�v = 1
025900*          �����p�^�[���̏ꍇ�͍��v���z�̂݃Z�b�g
025910           MOVE �}�b�T�[�W����{�p�v         TO �g���Z�|�}�b�T�[�W����{�p
025920           MOVE 1                            TO �g���Z�|�}�b�T�[�W����{�p�t���O
025930           IF �}�b�T�[�W�Ǐ��������v NOT = 1
025940              MOVE �}�b�T�[�W�Ǐ����o�v      TO �g���Z�|�}�b�T�[�W�Ǐ���
025950              MOVE �}�b�T�[�W�񐔂v          TO �g���Z�|�}�b�T�[�W��
025960           ELSE
025970              MOVE ZERO                      TO �g���Z�|�}�b�T�[�W�Ǐ���
025980              MOVE ZERO                      TO �g���Z�|�}�b�T�[�W��
025990           END-IF
026000           MOVE �}�b�T�[�W�����v�v           TO �g���Z�|�}�b�T�[�W��
026010        END-IF
026020        IF �ό`�k�苸���p����{�p�t���O�v = 1
026030*          �����p�^�[���̏ꍇ�͍��v���z�̂݃Z�b�g
026040           MOVE �ό`�k�苸���p����{�p�v     TO �g���Z�|�ό`�k�苸���p����{�p
026050           MOVE 1                            TO �g���Z�|�ό`�k�苸���p����{�p�t���O
026060           IF �ό`�k�苸���p�������v NOT = 1
026070              COMPUTE �g���Z�|�ό`�k�苸���p�P�� = �l�ό`�k�苸���p�v * �ό`�k�苸���p���o�v
026080              MOVE �ό`�k�苸���p���o�v           TO �g���Z�|�ό`�k�苸���p��
026090              MOVE �}�b�T�[�W�ό`�k�苸���p�񐔂v TO �g���Z�|�ό`�k�苸���p��
026100           ELSE
026110              COMPUTE �g���Z�|�ό`�k�苸���p�P�� = �l�ό`�k�苸���p�v * 1
026120              MOVE 1                         TO �g���Z�|�ό`�k�苸���p��
026130              MOVE ZERO                      TO �g���Z�|�ό`�k�苸���p��
026140           END-IF
026150           MOVE �ό`�k�藿���v�v             TO �g���Z�|�ό`�k�苸���p��
026160        END-IF
026170     END-IF
026180*
026182*    �{�p�񍐏���t��
026194     IF �}�b�T�[�W�񍐏���t���v NOT = ZERO
026195         MOVE �}�b�T�[�W�񍐏���t�񐔂v     TO �g���Z�|�{�p�񍐏���t��
026196         MOVE �{�p�񍐏���t���P���v         TO �g���Z�|�{�p�񍐏���t���P��
026202         MOVE �}�b�T�[�W�񍐏���t���v       TO �g���Z�|�{�p�񍐏���t��
026203     ELSE
026204         MOVE ZERO                           TO �g���Z�|�{�p�񍐏���t��
026205         MOVE ZERO                           TO �g���Z�|�{�p�񍐏���t���P��
026206         MOVE ZERO                           TO �g���Z�|�{�p�񍐏���t��
026207     END-IF.
026208*
026209*--���v---------------------------------------------------------*
026210*
026211* �A���̔�p�z�Z�b�g
026212     MOVE �}�b�T�[�W�݌v�z�v  TO �g�A�v�Z�|��p�z.
026220*
026231     PERFORM ���Z���S�z�擾.
026240*
026250*--------------------------------------------------------------*
026260*
026270*
026280*================================================================*
026290*================================================================*
026300 ���Z���S�z�擾 SECTION.
026310*
026320* ���S�z�擾
026330*   / �g�A�v�Z�̏����p�����^�N���A�[ /
026340     MOVE SPACE TO �g�A�v�Z�|�������t���O.
026350     MOVE SPACE TO �g�A�v�Z�|���f�Ãt���O.
026360     MOVE ZERO  TO �g�A�v�Z�|������.
026370     MOVE ZERO  TO �g�A�v�Z�|������.
026380     MOVE ZERO  TO �g�A�v�Z�|���̑��v�Z�敪�P.
006620     MOVE ZERO  TO �g�A�v�Z�Q�|���v�敪.
026390*
026400*   / 1�~�P�� /
026410     MOVE ZERO  TO �g�A�v�Z�|���S���v�Z�敪.
026420     MOVE ZERO  TO �g�A�v�Z�|�{�p��.
026430*
026440*   / ���ۏ؊֌W�Ȃ� /
026450     MOVE SPACE TO �g�A�v�Z���|���ۏ؃t���O.
026460*
026470     CALL   "KHT41410".
026480     CANCEL "KHT41410".
026490*
026500* �t�@�C���փZ�b�g
026510*�i�����l���g���Z�|��O���S�z�Ƃg���Z�|��O�����z�ɂ��Z�b�g�j
026521     MOVE �g�A�v�Z�|��p�z  TO  �g���Z�|��p�z.
026530     MOVE �g�A�v�Z�|���S�z  TO  �g���Z�|���S�z �g���Z�|��O���S�z.
026540     MOVE �g�A�v�Z�|�����z  TO  �g���Z�|�����z �g���Z�|��O�����z.
026550*
026560*  �i���S�����F%�������ցj
026570     COMPUTE �g���Z�|���S���� =  �g�A�v�Z�|���S�� / 10.
026580*
026590**
026600*---------------------------------------------------------------------------------------*
026610**��--- ���ҕ����E�{�̂܂Ƃߎ�----------------------------------------------------------*
026620*** ��L��i�Ō�Ɂj�A���ҕ����E�{�̂܂Ƃ߂̋��z���Z�o���A�Z�b�g�������B
026630     IF ( �{�̏��ҕ����敪�v = 1 ) OR ( �������ҕ����敪�v = 1 ) OR ( �{�̂܂Ƃߋ敪�v = 1 )
026640**
026650           IF ( �{�̏��ҕ����敪�v = 1 ) OR ( �������ҕ����敪�v = 1 )
026660              IF ������ʂv = ZERO
026670*                /�{�̂̂ݏ���/
026680                 IF �{�̏��ҕ����敪�v = 1
026690                    MOVE �g���Z�|��p�z  TO �g���Z�|���S�z
026700                    MOVE ZERO            TO �g���Z�|�����z
026710                 END-IF
026720              ELSE
026730                 EVALUATE TRUE
026740*                /�{�̏��ҁA�����ʏ�/
026750                 WHEN ( �{�̏��ҕ����敪�v = 1 ) AND ( �������ҕ����敪�v = ZERO )
026760                    COMPUTE �g���Z�|���S�z =  �g���Z�|��p�z - �g�A�v�Z�|���S�z
026770                    MOVE ZERO  TO �g���Z�|�����z
026780*                /�{�̒ʏ�A��������/
026790                 WHEN ( �{�̏��ҕ����敪�v = ZERO ) AND ( �������ҕ����敪�v = 1 )
026800                    CONTINUE
026810*                /�{�̏��ҁA��������/
026820                 WHEN ( �{�̏��ҕ����敪�v = 1 ) AND ( �������ҕ����敪�v = 1 )
026830                    COMPUTE �g���Z�|���S�z =  �g���Z�|��p�z - �g�A�v�Z�|���S�z
026840                    MOVE ZERO  TO �g���Z�|�����z
026850                 END-EVALUATE
026860              END-IF
026870           ELSE
026880*             / �{�̂܂Ƃ߂̂� /
026890              MOVE ZERO            TO �g���Z�|���S�z
026900              MOVE �g���Z�|��p�z  TO �g���Z�|�����z
026910           END-IF
026920*
026930     END-IF.
026940*---------------------------------------------------------------------------------------*
026950*
026960*================================================================*
026970 ��O�������Z���z�Z�b�g SECTION.
026980*
026990*�i�����l���g���Z�|��O�󋋎ҕ��S�z�Ƃg���Z�|��O�����������z�ɂ��Z�b�g�j
027000     MOVE �g�A�v�Z�|���S�z����       TO  �g���Z�|��O�󋋎ҕ��S�z.
027010     MOVE �g�A�v�Z�|�����z����       TO  �g���Z�|��O�����������z.
      */�����z�����̂U��ver������/20231117
           IF (�g�A�v�Z�|�����z�����Q NOT = ZERO) AND (�g�A�v�Z�|�����z�����Q > �g�A�v�Z�|�����z����)
027010         MOVE �g�A�v�Z�|�����z�����Q TO  �g���Z�|��O�����������z
           END-IF.
      */�����z�����̂U��ver������/20231117
      */���S�z�؂�グ�ɑΉ�������/20221125
           IF (��|��p���S�Ҕԍ�����(3:2) = "27") AND ( ���S�敪�v = 06 OR 22 )
027000         MOVE �g�A�v�Z�R�|���S�z���� TO  �g���Z�|��O�󋋎ҕ��S�z
           END-IF.
      */���S�z�؂�グ�ɑΉ�������/20221125
      */��㏕���F���̏����1�x���z���Ȃ��ꍇ�A�{�̂̕��S�z�ƍ��������Ă��������Ȃ�������/20220830
           IF (���S�敪�v = 22 AND ��|��p���S�Ҕԍ�����(3:2) = "27" AND �S�z���ҕ��S�e = "YES")
               MOVE �g���Z�|���S�z TO �g���Z�|��O�󋋎ҕ��S�z
               MOVE ZERO           TO �g���Z�|��O�����������z
           END-IF
      */��㏕���F���̏����1�x���z���Ȃ��ꍇ�A�{�̂̕��S�z�ƍ��������Ă��������Ȃ�������/20220830
027020*
027030**��--- ���ҕ����E�{�̂܂Ƃߎ�----------------------------------------------------------*
027040*** ��L��i�Ō�Ɂj�A���ҕ����E�{�̂܂Ƃ߂̋��z���Z�o���A�Z�b�g�������B
027050     IF ( �{�̏��ҕ����敪�v = 1 ) OR ( �������ҕ����敪�v = 1 ) OR ( �{�̂܂Ƃߋ敪�v = 1 )
027060**
027070           IF ( �{�̏��ҕ����敪�v = 1 ) OR ( �������ҕ����敪�v = 1 )
027080              IF ������ʂv NOT = ZERO
027090                 EVALUATE TRUE
027100*                /�{�̏��ҁA�����ʏ�/
027110                 WHEN ( �{�̏��ҕ����敪�v = 1 ) AND ( �������ҕ����敪�v = ZERO )
027120                    CONTINUE
027130*                /�{�̒ʏ�A��������/
027140                 WHEN ( �{�̏��ҕ����敪�v = ZERO ) AND ( �������ҕ����敪�v = 1 )
027150                    MOVE ZERO    TO  �g���Z�|�󋋎ҕ��S�z
027160                    MOVE ZERO    TO  �g���Z�|�����������z
027170*                /�{�̏��ҁA��������/
027180                 WHEN ( �{�̏��ҕ����敪�v = 1 ) AND ( �������ҕ����敪�v = 1 )
027190                    MOVE �g���Z�|��O���S�z  TO �g���Z�|�󋋎ҕ��S�z
027200                    MOVE ZERO                TO �g���Z�|�����������z
027210                 END-EVALUATE
027220              END-IF
027230           ELSE
027240*             / �{�̂܂Ƃ߂̂� /
027250              MOVE ZERO    TO  �g���Z�|�󋋎ҕ��S�z
027260              MOVE ZERO    TO  �g���Z�|�����������z
027270              MOVE ZERO    TO  �g���Z�|��O�󋋎ҕ��S�z
027280              MOVE ZERO    TO  �g���Z�|��O�����������z
027290           END-IF
027300*
027310     END-IF.
027320*
027330*================================================================*
027340*================================================================*
027350 �������S�敪�擾 SECTION.
027360*
027370     MOVE ZERO  TO ���S�敪�v.
027380     MOVE ��|�������             TO �s�|������.
027390     MOVE ��|��p���S�Ҕԍ�����   TO �s�|�s�����ԍ�.
027400     READ �s�����}�X�^
027410     NOT INVALID KEY
027420         MOVE ��|�������             TO �ۓ��|�ی����
027430         MOVE ��|��p���S�Ҕԍ�����   TO �ۓ��|�ی��Ҕԍ�
027440         MOVE �g�A�v�Z�|�{�p�a��       TO �ۓ��|�J�n�a��
027450         MOVE �g�A�v�Z�|�{�p�N         TO �ۓ��|�J�n�N
027460         MOVE �g�A�v�Z�|�{�p��         TO �ۓ��|�J�n��
027470         START �ی��ғ��ʕ��S�}�X�^ KEY IS <= �ۓ��|�ی����
027480                                              �ۓ��|�ی��Ҕԍ�
027490                                              �ۓ��|�J�n�a��N��
027500                                              REVERSED
027510         END-START
027520         IF ��ԃL�[ = "00"
027530             READ �ی��ғ��ʕ��S�}�X�^ NEXT
027540             NOT AT END
027550                 IF ( �ۓ��|�ی����   = ��|������� ) AND
027560                    ( �ۓ��|�ی��Ҕԍ� = ��|��p���S�Ҕԍ����� )
027570                    MOVE �ۓ��|���S�敪   TO ���S�敪�v
027580                 END-IF
027590             END-READ
027600         END-IF
027610     END-READ.
027620*
027630*================================================================*
027640*================================================================*
027650 �}�ԕ��S�݌v�z�v�Z SECTION.
027660*
027670*  ��|�}�ԍ쐬���}�ԂŁA�Q�Ǝ�f�ҏ��e��ǂ݁A���������Ȃ�A
027680*  �Q�Ƃg���|�ꕔ���S�����W�v���āA�}�ԕ��S�݌v�z�v�ɃZ�b�g�B
027690*
027700*
027710     MOVE ZERO TO �}�ԕ��S�݌v�z�v.
027720*
027730     OPEN INPUT �Q�Ǝ�f�ҏ��e.
027740         MOVE NC"�Q�Ǝ�f��" TO �t�@�C����.
027750         PERFORM �I�[�v���`�F�b�N.
027760*
027770     MOVE �g�A�v�Z�|�{�p�a��        TO �Q�Ǝ�|�{�p�a��.
027780     MOVE �g�A�v�Z�|�{�p�N          TO �Q�Ǝ�|�{�p�N.
027790     MOVE �g�A�v�Z�|�{�p��          TO �Q�Ǝ�|�{�p��.
027800     MOVE �g�A�v�Z�|���Ҕԍ�        TO �Q�Ǝ�|���Ҕԍ�.
027810*    / ��|�}�ԍ쐬���}�� /
027820     MOVE ��|�}�ԍ쐬���}��        TO �Q�Ǝ�|�}��.
027830*
027840     READ �Q�Ǝ�f�ҏ��e
027850     NOT INVALID KEY
027860         IF ( �Q�Ǝ�|�������           = ��|������� ) AND
027870            ( �Q�Ǝ�|��p���S�Ҕԍ����� = ��|��p���S�Ҕԍ����� ) AND
027880            ( �Q�Ǝ�|��v�Ҕԍ�����     = ��|��v�Ҕԍ����� ) AND
027890            ( �Q�Ǝ�|�������S���Ə�     = ��|�������S���Ə� )
027900*
027910               MOVE �g�A�v�Z�|�{�p�敪  TO �Q�Ƃg���|�{�p�敪
027920               MOVE �Q�Ǝ�|���Ҕԍ�    TO �Q�Ƃg���|���Ҕԍ�
027930               MOVE �Q�Ǝ�|�}��        TO �Q�Ƃg���|�}��
027940               MOVE �Q�Ǝ�|�{�p�a��    TO �Q�Ƃg���|�{�p�a��
027950               MOVE �Q�Ǝ�|�{�p�N      TO �Q�Ƃg���|�{�p�N
027960               MOVE �Q�Ǝ�|�{�p��      TO �Q�Ƃg���|�{�p��
027970*
027980*              / �������r���Ή� /
027990               IF �Q�Ǝ�|�������r���J�n�� NOT = ZERO
028000                  MOVE �Q�Ǝ�|�������r���J�n�� TO �Q�Ƃg���|�{�p��
028010               ELSE
028020                  MOVE ZERO                     TO �Q�Ƃg���|�{�p��
028030               END-IF
028040*
028050               START �Q�Ƃg���v�f�[�^�e KEY IS >= �Q�Ƃg���|�{�p�敪
028060                                                  �Q�Ƃg���|���҃R�[�h
028070                                                  �Q�Ƃg���|�{�p�a��N����
028080               END-START
028090               IF ��ԃL�[ = "00"
028100                       MOVE SPACE TO �I���t���O
028110                       PERFORM �Q�Ƃg���v�f�[�^�e�Ǎ�
028120*                      �J��Ԃ��i�����j
028130                       PERFORM UNTIL ( �Q�Ƃg���|�{�p�敪   NOT = �g�A�v�Z�|�{�p�敪 ) OR
028140                                     ( �Q�Ƃg���|���҃R�[�h NOT = �Q�Ǝ�|���҃R�[�h ) OR
028150                                     ( �Q�Ƃg���|�{�p�a��   NOT = �Q�Ǝ�|�{�p�a��   ) OR
028160                                     ( �Q�Ƃg���|�{�p�N     NOT = �Q�Ǝ�|�{�p�N     ) OR
028170                                     ( �Q�Ƃg���|�{�p��     NOT = �Q�Ǝ�|�{�p��     ) OR
028180                                     ( �I���t���O           NOT = SPACE )
028190*
028200                               COMPUTE �}�ԕ��S�݌v�z�v  = �}�ԕ��S�݌v�z�v + �Q�Ƃg���|�ꕔ���S��
028210*
028220                               PERFORM �Q�Ƃg���v�f�[�^�e�Ǎ�
028230                       END-PERFORM
028240               END-IF
028250*
028260*
028270         END-IF
028280     END-READ.
028290*
028300     CLOSE �Q�Ǝ�f�ҏ��e.
028310*
028320*    / �A���փZ�b�g /
028330     MOVE �}�ԕ��S�݌v�z�v TO �g�A�v�Z�����݌v�z�|���S�z����.
028340*
028350*================================================================*
028360*================================================================*
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
       ���ꌚ�����Ґ��擾 SECTION.
      *
      */�����̓��ꌚ���Ґ����J�E���g
      *
           MOVE ZERO TO ���ꌚ�����Ґ��v ���ꌚ�����Ґ��v�Q.
      *     DISPLAY "41-5 " �g���|���R�[�h�L�[
013690     INITIALIZE �g�����|���R�[�h.
013700     MOVE �g���|���R�[�h�L�[ TO �g�����|���R�[�h�L�[.
013710     READ �g���Î��тe
013720     INVALID
013730         INITIALIZE �g�����|���R�[�h
           NOT INVALID KEY 
      *     DISPLAY "41-2 " �g�����|�{�p�a��N���� " " �g�����|���҃R�[�h " �{�p��=" �g�����|�{�p�Ҕԍ�  
      *     " ��=" �g�����|�o�^�� " ����=" �g�����|���Ïꏊ�敪 " ����{=" �g�����|���Ïꏊ�{�݃R�[�h 
      *     " ���ꊳ=" �g�����|���Ïꏊ���҃R�[�h
               MOVE �g�����|���Ïꏊ�敪       TO ���Ïꏊ�敪�v
               MOVE �g�����|���Ïꏊ�{�݃R�[�h TO ���Ïꏊ�{�݃R�[�h�v
               MOVE �g�����|���Ïꏊ���҃R�[�h TO ���Ïꏊ���҃R�[�h�v
      */�{�p�敪�E�{�p�Ҕԍ�������Ɋ܂߂�
               MOVE �g�����|�{�p�敪           TO �{�p�敪�v
               MOVE �g�����|�{�p�Ҕԍ�         TO �{�p�Ҕԍ��v
HILO  *         DISPLAY "610-30 " �g�����|�{�p�敪  " " �g�����|�{�p�Ҕԍ� "|"
013740     END-READ.
HILO  *     DISPLAY "�����̓��ꌚ���Ґ����J�E���g".
           MOVE �g���|�{�p�a��N���� TO �g�����|�{�p�a��N����.
           MOVE ZERO TO �g�����|�{�p�Ҕԍ� �g�����|�o�^��.
      *
028050     START �g���Î��тe KEY IS >= �g�����|�{�p�a��N����
028060                                  �g�����|�{�p�Ҕԍ�
028070                                  �g�����|�o�^��
028080     END-START.
028090     IF ��ԃL�[ = "00"
               MOVE SPACE TO �I���t���O�S
               PERFORM �g���Î��тe�Ǎ��Q
023180         PERFORM UNTIL (�g���|�{�p�a��N���� NOT = �g�����|�{�p�a��N����) OR 
                             (�I���t���O�S         NOT = SPACE)
      *         DISPLAY "610-3 " �g�����|�{�p�a��N���� " " �g�����|���҃R�[�h
      *       " �{�p��="         �g�����|�{�p�Ҕԍ�  " ��=" �g�����|�o�^�� 
      *       " ���Ïꏊ="       �g�����|���Ïꏊ�敪
      *       " ���Ïꏊ�{��="   �g�����|���Ïꏊ�{�݃R�[�h
      *       " ���Ïꏊ����="   �g�����|���Ïꏊ���҃R�[�h
      *             IF (�g�����|���Ïꏊ�敪 = 2 OR 3) AND (���Ïꏊ�{�݃R�[�h�v = �g�����|���Ïꏊ�{�݃R�[�h)
      *                 COMPUTE ���ꌚ�����Ґ��v = ���ꌚ�����Ґ��v + 1
      *             END-IF
      *             IF (���Ïꏊ�敪�v = �g�����|���Ïꏊ�敪)
      *                 IF (���Ïꏊ�{�݃R�[�h�v = �g�����|���Ïꏊ�{�݃R�[�h) OR
      *                    (���Ïꏊ���҃R�[�h�v = �g�����|���Ïꏊ���҃R�[�h)
      *                     COMPUTE ���ꌚ�����Ґ��v = ���ꌚ�����Ґ��v + 1
      *                 END-IF
      *             END-IF
      *             IF (���Ïꏊ�敪�v       = �g�����|���Ïꏊ�敪) OR 
      *                (���Ïꏊ�敪�v       = 3                   ) OR 
      *                (�g�����|���Ïꏊ�敪 = 3                   )
      *                 IF ���Ïꏊ�{�݃R�[�h�v NOT = ZERO
      *                     IF ���Ïꏊ�{�݃R�[�h�v = �g�����|���Ïꏊ�{�݃R�[�h
      *                         COMPUTE ���ꌚ�����Ґ��v = ���ꌚ�����Ґ��v + 1
      *                     END-IF
      *                 ELSE
      *                     IF ���Ïꏊ���҃R�[�h�v = �g�����|���Ïꏊ���҃R�[�h
      *                         COMPUTE ���ꌚ�����Ґ��v = ���ꌚ�����Ґ��v + 1
      *                     END-IF
      *                 END-IF
      *             END-IF
HILO  *      DISPLAY "610-31 " �g�����|�{�p�敪  " " �g�����|�{�p�Ҕԍ� "|"
000370*      DISPLAY "�g�����|�{�p�敪          "  �g�����|�{�p�敪      
000390*      DISPLAY "�g�����|�{�p�a��N����    "  �g�����|�{�p�a��N����
000460*      DISPLAY "�g�����|���҃R�[�h        "  �g�����|���҃R�[�h    
000510*      DISPLAY "�g�����|�{�p�Ҕԍ�        "  �g�����|�{�p�Ҕԍ�    
000600*      DISPLAY "�g�����|���Ïꏊ�敪      "  �g�����|���Ïꏊ�敪  
000600*      DISPLAY "�g�����|���Ïꏊ���҃R�[�h"  �g�����|���Ïꏊ���҃R�[�h
      */����̏ꍇ���ꌚ�����Ґ��̑ΏۊO�ɂ��遫����/20241206
                   PERFORM �Q�Ǝ�f�҂e�擾
                   IF �Q�Ǝ�|�ی���� = 90
                       CONTINUE
                   ELSE
      */����̏ꍇ���ꌚ�����Ґ��̑ΏۊO�ɂ��遪����/20241206
                   IF ((���Ïꏊ�敪�v = 2 OR 3) AND (�g�����|���Ïꏊ�敪 = 2 OR 3)) OR
                      ((���Ïꏊ�敪�v = 1 OR 3) AND (�g�����|���Ïꏊ�敪 = 1 OR 3))
                       IF ���Ïꏊ�{�݃R�[�h�v NOT = ZERO
                           IF (���Ïꏊ�{�݃R�[�h�v = �g�����|���Ïꏊ�{�݃R�[�h)
      */�{�p�Ǘ��ҒP�ʂœ�������ꌚ���Ŏ{�p���s��������(�{�p�敪�E�{�p�҂��ʂł��P�W�v)/20240902
      *                     AND (�{�p�敪�v           = �g�����|�{�p�敪          )
      *                     AND (�{�p�Ҕԍ��v         = �g�����|�{�p�Ҕԍ�        )
                               COMPUTE ���ꌚ�����Ґ��v = ���ꌚ�����Ґ��v + 1
                               PERFORM ��ƃt�@�C���P�쐬
                           END-IF
                       ELSE
                           IF (���Ïꏊ���҃R�[�h�v = �g�����|���Ïꏊ���҃R�[�h)
      */�{�p�Ǘ��ҒP�ʂœ�������ꌚ���Ŏ{�p���s��������(�{�p�敪�E�{�p�҂��ʂł��P�W�v)/20240902
      *                     AND (�{�p�敪�v           = �g�����|�{�p�敪          )
      *                     AND (�{�p�Ҕԍ��v         = �g�����|�{�p�Ҕԍ�       )
                               COMPUTE ���ꌚ�����Ґ��v = ���ꌚ�����Ґ��v + 1
                               PERFORM ��ƃt�@�C���P�쐬
                           END-IF
                       END-IF
                   END-IF
                   END-IF
                   PERFORM �g���Î��тe�Ǎ��Q
               END-PERFORM
HILO  *         DISPLAY "610-4-1 " �g���|�{�p�a��N���� " �����̓��ꌚ�����Ґ�=" ���ꌚ�����Ґ��v
               PERFORM ��Q�e�J�E���g
HILO  *         DISPLAY "610-4-2 " �g���|�{�p�a��N���� " �����̓��ꌚ�����Ґ�=" ���ꌚ�����Ґ��v�Q
      */������ɓ��ꊳ�҂��I�}�̗������{�p���Ă���ꍇ�͓��ꌚ�����Ґ��͂P�J�E���g/20240902
               MOVE ���ꌚ�����Ґ��v�Q TO ���ꌚ�����Ґ��v
           END-IF.
      */��ƃt�@�C�����N���A����
           CLOSE ��ƃt�@�C���Q.
008320     OPEN OUTPUT ��ƃt�@�C���Q.
008330         MOVE NC"��Q" TO �t�@�C����.
008340         PERFORM �I�[�v���`�F�b�N.
           CLOSE ��ƃt�@�C���Q.
008320     OPEN I-O ��ƃt�@�C���Q.
008330         MOVE NC"��Q" TO �t�@�C����.
008340         PERFORM �I�[�v���`�F�b�N.
013660*================================================================*
013670 �g���Î��тe�Ǎ��Q SECTION.
013680*
013710     READ �g���Î��тe NEXT
013720     AT END
013730         MOVE "YES" TO �I���t���O�S
013740     END-READ.
013750*
013760*================================================================*
022960 ���Z�v�g�ڍ׏��� SECTION.
022970*
HILO***       DISPLAY "610-20 ��f" ��|���R�[�h�L�[.
           MOVE SPACE TO �g���Z�ڍׁ|���R�[�h.
           INITIALIZE    �g���Z�ڍׁ|���R�[�h.
006480     MOVE �g�A�v�Z�|�{�p�敪       TO �g���Z�ڍׁ|�{�p�敪
006490     MOVE �g�A�v�Z�|�{�p�a��N���� TO �g���Z�ڍׁ|�{�p�a��N��
006560     MOVE �g�A�v�Z�|���҃R�[�h     TO �g���Z�ڍׁ|���҃R�[�h
011580* ���Z��� 1:���,2:�V�l,3:����,4:�J��,5:������,6:����,7:���ےP��,8�ی��ؖY�� /
011650     IF ��|������ NOT = ZERO
011660         MOVE 2                    TO �g���Z�ڍׁ|���Z���
011670     ELSE
               IF ��|�ی���� = 85
011610             MOVE 7                TO �g���Z�ڍׁ|���Z���
               ELSE
                   IF ��|�ی���� < 10
011680                 MOVE 1            TO �g���Z�ڍׁ|���Z���
                   END-IF
               END-IF
011690     END-IF.
      *���W�b�N�̌�����/20241202������
      **/�Ǐ�ۑ�/20241112������
      *     MOVE �g���Z�ڍׁ|���R�[�h�L�[ TO �g���Z�v�g�ڍ׃��R�[�h�L�[�v.
      *     READ �g���Z�v�g�ڍׂe
      *     NOT INVALID KEY
      *         MOVE �g���Z�ڍׁ|�Ǐ� TO �Ǐ�v
      *     END-READ.
      *     MOVE SPACE TO �g���Z�ڍׁ|���R�[�h.
      *     INITIALIZE    �g���Z�ڍׁ|���R�[�h.
      *     MOVE �g���Z�v�g�ڍ׃��R�[�h�L�[�v TO �g���Z�ڍׁ|���R�[�h�L�[.
      *     MOVE �Ǐ�v                       TO �g���Z�ڍׁ|�Ǐ�.
      **/�Ǐ�ۑ�/20241112������
      **
      *     PERFORM ���Z�ڍ׃��R�[�h�Z�b�g.
      **
      *     WRITE �g���Z�ڍׁ|���R�[�h
      *     END-WRITE.
      *     EVALUATE ��ԃL�[
      *     WHEN "00"
      *         CONTINUE
      *     WHEN "22"
      *         REWRITE �g���Z�ڍׁ|���R�[�h
      *         IF ��ԃL�[  NOT =  "00"
      *             MOVE NC"�g���Z�ڍ�" TO �t�@�C����
      *             PERFORM �G���[�\��
      *         END-IF
      *     WHEN OTHER
      *         MOVE NC"�g���Z�ڍ�" TO �t�@�C����
      *         PERFORM �G���[�\��
      *     END-EVALUATE.
      */�Ǐ�ۑ�/20241112������
      */�����ŌJ��z���̂���߂�
      *     PERFORM ���Z�ڍבO���Ǎ�.
           MOVE �g���Z�ڍׁ|���R�[�h�L�[ TO �g���Z�v�g�ڍ׃��R�[�h�L�[�v.
           READ �g���Z�v�g�ڍׂe
           INVALID KEY
      *         MOVE �g���Z�ڍׁ|�Ǐ� TO �Ǐ�v
               MOVE SPACE TO �g���Z�ڍׁ|���R�[�h
               INITIALIZE    �g���Z�ڍׁ|���R�[�h
               MOVE �g���Z�v�g�ڍ׃��R�[�h�L�[�v TO �g���Z�ڍׁ|���R�[�h�L�[
      *         MOVE �Ǐ�v                       TO �g���Z�ڍׁ|�Ǐ�
               MOVE �Q�g���Z�ڍׁ|�Ǐ�           TO �g���Z�ڍׁ|�Ǐ�
               PERFORM ���Z�ڍ׃��R�[�h�Z�b�g
HILO***           DISPLAY "610-1 " �g���Z�ڍׁ|�Ǐ�
               WRITE �g���Z�ڍׁ|���R�[�h
               IF ��ԃL�[  NOT =  "00"
                   MOVE NC"�g���Z�ڍ�" TO �t�@�C����
                   PERFORM �G���[�\��
               END-IF
           NOT INVALID KEY
               MOVE �g���Z�ڍׁ|�Ǐ� TO �Ǐ�v
               MOVE SPACE TO �g���Z�ڍׁ|���R�[�h
               INITIALIZE    �g���Z�ڍׁ|���R�[�h
               MOVE �g���Z�v�g�ڍ׃��R�[�h�L�[�v TO �g���Z�ڍׁ|���R�[�h�L�[
               MOVE �Ǐ�v                       TO �g���Z�ڍׁ|�Ǐ�
      *         IF (�g���Z�ڍׁ|�Ǐ� = SPACE) AND (�Q�g���Z�ڍׁ|�Ǐ� NOT = SPACE)
      *             MOVE �Q�g���Z�ڍׁ|�Ǐ�       TO �g���Z�ڍׁ|�Ǐ�
      *         END-IF
               PERFORM ���Z�ڍ׃��R�[�h�Z�b�g
HILO***           DISPLAY "610-2 " �g���Z�ڍׁ|�Ǐ�
               REWRITE �g���Z�ڍׁ|���R�[�h
               IF ��ԃL�[  NOT =  "00"
                   MOVE NC"�g���Z�ڍ�" TO �t�@�C����
                   PERFORM �G���[�\��
               END-IF
           END-READ.
      *���W�b�N�̌�����/20241202������
      *
011650     IF ��|������� NOT = ZERO
011660*         MOVE 3 TO �g���Z�ڍׁ|���Z���
      *
      *         WRITE �g���Z�ڍׁ|���R�[�h
      *         END-WRITE
      *         EVALUATE ��ԃL�[
      *         WHEN "00"
      *             CONTINUE
      *         WHEN "22"
      *             REWRITE �g���Z�ڍׁ|���R�[�h
      *             IF ��ԃL�[  NOT =  "00"
      *                 MOVE NC"�g���Z�ڍ�" TO �t�@�C����
      *                 PERFORM �G���[�\��
      *             END-IF
      *         WHEN OTHER
      *             MOVE NC"�g���Z�ڍ�" TO �t�@�C����
      *             PERFORM �G���[�\��
      *         END-EVALUATE
011660         MOVE 3 TO �g���Z�ڍׁ|���Z���
               MOVE �g���Z�ڍׁ|���R�[�h�L�[ TO �g���Z�v�g�ڍ׃��R�[�h�L�[�v
               READ �g���Z�v�g�ڍׂe
               INVALID KEY
      *             MOVE �g���Z�ڍׁ|�Ǐ� TO �Ǐ�v
                   MOVE SPACE TO �g���Z�ڍׁ|���R�[�h
                   INITIALIZE    �g���Z�ڍׁ|���R�[�h
                   MOVE �g���Z�v�g�ڍ׃��R�[�h�L�[�v TO �g���Z�ڍׁ|���R�[�h�L�[
      *             MOVE �Ǐ�v                       TO �g���Z�ڍׁ|�Ǐ�
                   MOVE �Q�g���Z�ڍׁ|�Ǐ�           TO �g���Z�ڍׁ|�Ǐ�
                   PERFORM ���Z�ڍ׃��R�[�h�Z�b�g
HILO***               DISPLAY "610-3 " �g���Z�ڍׁ|�Ǐ�
                   WRITE �g���Z�ڍׁ|���R�[�h
                   IF ��ԃL�[  NOT =  "00"
                       MOVE NC"�g���Z�ڍ�" TO �t�@�C����
                       PERFORM �G���[�\��
                   END-IF
               NOT INVALID KEY
                   MOVE �g���Z�ڍׁ|�Ǐ� TO �Ǐ�v
                   MOVE SPACE TO �g���Z�ڍׁ|���R�[�h
                   INITIALIZE    �g���Z�ڍׁ|���R�[�h
                   MOVE �g���Z�v�g�ڍ׃��R�[�h�L�[�v TO �g���Z�ڍׁ|���R�[�h�L�[
                   MOVE �Ǐ�v                       TO �g���Z�ڍׁ|�Ǐ�
      *             IF (�g���Z�ڍׁ|�Ǐ� = SPACE) AND (�Q�g���Z�ڍׁ|�Ǐ� NOT = SPACE)
      *                 MOVE �Q�g���Z�ڍׁ|�Ǐ�       TO �g���Z�ڍׁ|�Ǐ�
      *             END-IF
                   PERFORM ���Z�ڍ׃��R�[�h�Z�b�g
HILO***               DISPLAY "610-4 " �g���Z�ڍׁ|�Ǐ�
                   REWRITE �g���Z�ڍׁ|���R�[�h
                   IF ��ԃL�[  NOT =  "00"
                       MOVE NC"�g���Z�ڍ�" TO �t�@�C����
                       PERFORM �G���[�\��
                   END-IF
               END-READ
011690     END-IF.
      *
028490*================================================================*
       ���Z�ڍ׃��R�[�h�Z�b�g SECTION.
      *
001400*     05 ���ʗ�.
           MOVE ���ʒn����Z���P���v             TO �g���Z�ڍׁ|���ʒn����Z���P��.
           MOVE ���ʒn����Z�񐔂v               TO �g���Z�ڍׁ|���ʒn����Z��  .
      *     MOVE ���ʒn����Z���v                 TO �g���Z�ڍׁ|���ʒn����Z��    .
           MOVE �݌v���ʒn����Z���v             TO �g���Z�ڍׁ|���ʒn����Z��    .
001490*     05 �͂���z��.
001410     MOVE �͂肫�イ�ʏ��{�p���P���P�v     TO �g���Z�ڍׁ|�͂肫�イ�ʏ��{�p���P���P    .
001420     MOVE �͂肫�イ�ʏ��{�p���񐔂P�v     TO �g���Z�ڍׁ|�͂肫�イ�ʏ��{�p���񐔂P    .
001420     MOVE �͂肫�イ�ʏ��{�p���{�p���P�v   TO �g���Z�ڍׁ|�͂肫�イ�ʏ��{�p���{�p���P  .
001410     MOVE �͂肫�イ�ʏ��{�p���P���Q�v     TO �g���Z�ڍׁ|�͂肫�イ�ʏ��{�p���P���Q    .
001420     MOVE �͂肫�イ�ʏ��{�p���񐔂Q�v     TO �g���Z�ڍׁ|�͂肫�イ�ʏ��{�p���񐔂Q    .
001420     MOVE �͂肫�イ�ʏ��{�p���{�p���Q�v   TO �g���Z�ڍׁ|�͂肫�イ�ʏ��{�p���{�p���Q  .
001410     MOVE �͂肫�イ�K��{�p���P�P���P�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���P�P���P  .
001420     MOVE �͂肫�イ�K��{�p���P�񐔂P�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���P�񐔂P  .
001420     MOVE �͂肫�イ�K��{�p���P�{�p���P�v TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���P�{�p���P.
001410     MOVE �͂肫�イ�K��{�p���P�P���Q�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���P�P���Q  .
001420     MOVE �͂肫�イ�K��{�p���P�񐔂Q�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���P�񐔂Q  .
001420     MOVE �͂肫�イ�K��{�p���P�{�p���Q�v TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���P�{�p���Q.
001410     MOVE �͂肫�イ�K��{�p���Q�P���P�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���Q�P���P  .
001420     MOVE �͂肫�イ�K��{�p���Q�񐔂P�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���Q�񐔂P  .
001420     MOVE �͂肫�イ�K��{�p���Q�{�p���P�v TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���Q�{�p���P.
001410     MOVE �͂肫�イ�K��{�p���Q�P���Q�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���Q�P���Q  .
001420     MOVE �͂肫�イ�K��{�p���Q�񐔂Q�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���Q�񐔂Q  .
001420     MOVE �͂肫�イ�K��{�p���Q�{�p���Q�v TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���Q�{�p���Q.
001410     MOVE �͂肫�イ�K��{�p���R�P���P�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���R�P���P  .
001420     MOVE �͂肫�イ�K��{�p���R�񐔂P�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���R�񐔂P  .
001420     MOVE �͂肫�イ�K��{�p���R�{�p���P�v TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���R�{�p���P.
001410     MOVE �͂肫�イ�K��{�p���R�P���Q�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���R�P���Q  .
001420     MOVE �͂肫�イ�K��{�p���R�񐔂Q�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���R�񐔂Q  .
001420     MOVE �͂肫�イ�K��{�p���R�{�p���Q�v TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���R�{�p���Q.
001410     MOVE �͂肫�イ�K��{�p���S�P���P�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���S�P���P  .
001420     MOVE �͂肫�イ�K��{�p���S�񐔂P�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���S�񐔂P  .
001420     MOVE �͂肫�イ�K��{�p���S�{�p���P�v TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���S�{�p���P.
001410     MOVE �͂肫�イ�K��{�p���S�P���Q�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���S�P���Q  .
001420     MOVE �͂肫�イ�K��{�p���S�񐔂Q�v   TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���S�񐔂Q  .
001420     MOVE �͂肫�イ�K��{�p���S�{�p���Q�v TO �g���Z�ڍׁ|�͂肫�イ�K��{�p���S�{�p���Q.
001420     MOVE �͂�P�p�񐔂v                   TO �g���Z�ڍׁ|�͂�P�p��                  .
001420     MOVE ���イ�P�p�񐔂v                 TO �g���Z�ڍׁ|���イ�P�p��                .
001420     MOVE �͂肫�イ�Q�p�񐔂v             TO �g���Z�ڍׁ|�͂肫�イ�Q�p��            .
001880*     05 �}�b�T�[�W���z���v.
001410     MOVE �}�b�T�[�W�ʏ��{�p���P���P�v     TO �g���Z�ڍׁ|�}�b�T�[�W�ʏ��{�p���P���P    .
001420     MOVE �}�b�T�[�W�ʏ��{�p���񐔂P�v     TO �g���Z�ڍׁ|�}�b�T�[�W�ʏ��{�p���񐔂P    .
001420     MOVE �}�b�T�[�W�ʏ��{�p���{�p���P�v   TO �g���Z�ڍׁ|�}�b�T�[�W�ʏ��{�p���{�p���P  .
001410     MOVE �}�b�T�[�W�ʏ��{�p���P���Q�v     TO �g���Z�ڍׁ|�}�b�T�[�W�ʏ��{�p���P���Q    .
001420     MOVE �}�b�T�[�W�ʏ��{�p���񐔂Q�v     TO �g���Z�ڍׁ|�}�b�T�[�W�ʏ��{�p���񐔂Q    .
001420     MOVE �}�b�T�[�W�ʏ��{�p���{�p���Q�v   TO �g���Z�ڍׁ|�}�b�T�[�W�ʏ��{�p���{�p���Q  .
001410     MOVE �}�b�T�[�W�K��{�p���P�P���P�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���P�P���P  .
001420     MOVE �}�b�T�[�W�K��{�p���P�񐔂P�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���P�񐔂P  .
001420     MOVE �}�b�T�[�W�K��{�p���P�{�p���P�v TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���P�{�p���P.
001410     MOVE �}�b�T�[�W�K��{�p���P�P���Q�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���P�P���Q  .
001420     MOVE �}�b�T�[�W�K��{�p���P�񐔂Q�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���P�񐔂Q  .
001420     MOVE �}�b�T�[�W�K��{�p���P�{�p���Q�v TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���P�{�p���Q.
001410     MOVE �}�b�T�[�W�K��{�p���Q�P���P�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���Q�P���P  .
001420     MOVE �}�b�T�[�W�K��{�p���Q�񐔂P�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���Q�񐔂P  .
001420     MOVE �}�b�T�[�W�K��{�p���Q�{�p���P�v TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���Q�{�p���P.
001410     MOVE �}�b�T�[�W�K��{�p���Q�P���Q�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���Q�P���Q  .
001420     MOVE �}�b�T�[�W�K��{�p���Q�񐔂Q�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���Q�񐔂Q  .
001420     MOVE �}�b�T�[�W�K��{�p���Q�{�p���Q�v TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���Q�{�p���Q.
001410     MOVE �}�b�T�[�W�K��{�p���R�P���P�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���R�P���P  .
001420     MOVE �}�b�T�[�W�K��{�p���R�񐔂P�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���R�񐔂P  .
001420     MOVE �}�b�T�[�W�K��{�p���R�{�p���P�v TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���R�{�p���P.
001410     MOVE �}�b�T�[�W�K��{�p���R�P���Q�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���R�P���Q  .
001420     MOVE �}�b�T�[�W�K��{�p���R�񐔂Q�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���R�񐔂Q  .
001420     MOVE �}�b�T�[�W�K��{�p���R�{�p���Q�v TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���R�{�p���Q.
001410     MOVE �}�b�T�[�W�K��{�p���S�P���P�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���S�P���P  .
001420     MOVE �}�b�T�[�W�K��{�p���S�񐔂P�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���S�񐔂P  .
001420     MOVE �}�b�T�[�W�K��{�p���S�{�p���P�v TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���S�{�p���P.
001410     MOVE �}�b�T�[�W�K��{�p���S�P���Q�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���S�P���Q  .
001420     MOVE �}�b�T�[�W�K��{�p���S�񐔂Q�v   TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���S�񐔂Q  .
001420     MOVE �}�b�T�[�W�K��{�p���S�{�p���Q�v TO �g���Z�ڍׁ|�}�b�T�[�W�K��{�p���S�{�p���Q.
      */20240828
001950     MOVE �ό`�k�苸���p�P���v             TO �g���Z�ڍׁ|�ό`�k�苸���p�P��.
001960     MOVE �ό`�k�苸���p�񐔂v             TO �g���Z�ڍׁ|�ό`�k�苸���p��.
HILO  *     DISPLAY "610-1 " �g���Z�ڍׁ|�ό`�k�苸���p�P�� " " �g���Z�ڍׁ|�ό`�k�苸���p��.
028500*================================================================*
       ��ƃt�@�C���P�쐬 SECTION.
      *
HILO***     DISPLAY "k5-5 " �g�A�v�Z�S�|���ꌚ�����Ґ��擾�e " " �g�A�v�Z�|���҃R�[�h �g�A�v�Z�|�{�p�敪
HILO***                   " " �g�����|���҃R�[�h �g�����|�{�p�敪 *>HILO
           IF �g�A�v�Z�S�|���ꌚ�����Ґ��擾�e = ZERO
      */240902������
      *         IF �g�A�v�Z�|���҃R�[�h NOT = �g�����|���҃R�[�h
               IF (�g�A�v�Z�|���҃R�[�h = �g�����|���҃R�[�h) AND
                  (�g�A�v�Z�|�{�p�敪   = �g�����|�{�p�敪)
                   CONTINUE
               ELSE
      */240902������
HILO***               DISPLAY "610-4-1 ���ꌚ������ " �g�����|���R�[�h�L�[
                   INITIALIZE    ��P�|���R�[�h
                   MOVE SPACE TO ��P�|���R�[�h
                   MOVE �g�����|���R�[�h�L�[ TO ��P�|���R�[�h�L�[
005656             WRITE ��P�|���R�[�h
005657             IF ��ԃL�[  NOT =  "00"
005658                 MOVE NC"��P"  TO �t�@�C����
005659                 PERFORM �G���[�\��
005660             END-IF
005660         END-IF
005660     END-IF.
005661*
      *
      */��������ꌚ�����Ґ��̃J�E���g�ׂ̈ɏ�����
      *     MOVE �g�����|���R�[�h�L�[ TO ��P�|���R�[�h�L�[.
000390     MOVE �g�����|�{�p�a��N���� TO ��Q�|�{�p�a��N����.
000530     MOVE �g�����|���҃R�[�h     TO ��Q�|���҃R�[�h.
           READ ��ƃt�@�C���Q
           INVALID KEY
HILO  *     DISPLAY "K5-8 ��������������"
               INITIALIZE    ��Q�|���R�[�h
               MOVE SPACE TO ��Q�|���R�[�h
000390*         MOVE ��P�|�{�p�a��N���� TO ��Q�|�{�p�a��N����
000530*         MOVE ��P�|���҃R�[�h     TO ��Q�|���҃R�[�h
000390         MOVE �g�����|�{�p�a��N���� TO ��Q�|�{�p�a��N����
000530         MOVE �g�����|���҃R�[�h     TO ��Q�|���҃R�[�h
               WRITE ��Q�|���R�[�h
005657         IF ��ԃL�[  NOT =  "00"
005658             MOVE NC"��Q"  TO �t�@�C����
005659             PERFORM �G���[�\��
HILO  *         ELSE DISPLAY "K5-7 " ��Q�|���R�[�h�L�[ "������������������"
005660         END-IF
           NOT INVALID KEY
HILO  *     DISPLAY "K5-9 ����������������"
               CONTINUE
           END-READ.
028500*================================================================*
       ��Q�e�J�E���g SECTION.
      *
           MOVE ZERO  TO ���ꌚ�����Ґ��v�Q.
           INITIALIZE    ��Q�|���R�[�h.
           MOVE SPACE TO ��Q�|���R�[�h.
028050     START ��ƃt�@�C���Q KEY IS >= ��Q�|�{�p�a��N����
000190                                    ��Q�|���҃R�[�h
028080     END-START.
028090     IF ��ԃL�[ = "00"
               MOVE SPACE TO �I���t���O�T
               PERFORM ��ƃt�@�C���Q�Ǎ�
               PERFORM UNTIL �I���t���O�T NOT = SPACE
                   COMPUTE ���ꌚ�����Ґ��v�Q = ���ꌚ�����Ґ��v�Q + 1
HILO***            IF �g�A�v�Z�S�|���ꌚ�����Ґ��擾�e = ZERO
HILO***            DISPLAY "k5-1 " ���ꌚ�����Ґ��v�Q " " ��Q�|���҃R�[�h ��Q�|�{�p�a��N���� END-IF
                   PERFORM ��ƃt�@�C���Q�Ǎ�
               END-PERFORM
           END-IF.
028500*================================================================*
       ��ƃt�@�C���Q�Ǎ� SECTION.
024370*
024380     READ ��ƃt�@�C���Q NEXT
024390     AT END
024400         MOVE "YES"  TO �I���t���O�T
024410     END-READ.
024420*
028500*================================================================*
      * ���Z�ڍבO���Ǎ� SECTION.
      **
      *     MOVE �g���Z�ڍׁ|���R�[�h�L�[ TO �Q�g���Z�ڍׁ|���R�[�h�L�[.
      *     IF �g���Z�ڍׁ|�{�p�� = 1
      *         MOVE 12 TO �Q�g���Z�ڍׁ|�{�p��
      *         COMPUTE �Q�g���Z�ڍׁ|�{�p�N = �g���Z�ڍׁ|�{�p�N - 1
      *     ELSE
      *         MOVE �g���Z�ڍׁ|�{�p�N TO �Q�g���Z�ڍׁ|�{�p�N
      *         COMPUTE �Q�g���Z�ڍׁ|�{�p�� = �g���Z�ڍׁ|�{�p�� - 1
      *     END-IF
018340*     MOVE �g���Z�ڍׁ|�{�p�a�� TO ���|�����敪 �Q�g���Z�ڍׁ|�{�p�a��
018350*     READ �����}�X�^
018360*     NOT INVALID KEY
      *         IF ���|�����J�n�a��N�� > �g���Z�ڍׁ|�{�p�a��N��
      *             COMPUTE ���|�����敪 = ���|�����敪 - 1
018350*             READ �����}�X�^
018360*             NOT INVALID KEY
      *                 MOVE ���|�����I���a��N�� TO �Q�g���Z�ڍׁ|�{�p�a��N��
      *             END-READ
      *         END-IF
      *     END-READ
HILO  **     DISPLAY "610-0-1  " �Q�g���Z�ڍׁ|�{�p�a��N��
027530*     READ �Q�Ƃg���Z�v�g�ڍׂe
027540*     INVALID KEY
      *         MOVE SPACE TO �Q�g���Z�ڍׁ|���R�[�h
027590*     END-READ
028500*================================================================*
       �Q�Ǝ�f�҂e�擾 SECTION.
      *
027730     OPEN INPUT �Q�Ǝ�f�ҏ��e.
027740         MOVE NC"�Q�Ǝ�f��" TO �t�@�C����.
027750         PERFORM �I�[�v���`�F�b�N.
000390     MOVE �g�����|�{�p�a��N�� TO �Q�Ǝ�|�{�p�a��N��
000460     MOVE �g�����|���҃R�[�h   TO �Q�Ǝ�|���҃R�[�h    
027840     READ �Q�Ǝ�f�ҏ��e
027850     INVALID KEY
027850         MOVE SPACE TO �Q�Ǝ�|���R�[�h
HILO  *     NOT INVALID KEY DISPLAY "610-2 " �Q�Ǝ�|�ی����
           END-READ.
028300     CLOSE �Q�Ǝ�f�ҏ��e.
028500*================================================================*
028510******************************************************************
028520 END PROGRAM K50610.
028530******************************************************************
