000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             CHGJUNO.
000060 AUTHOR.                 �r�c�@�K�q
000070*
000080*----------------------------------------------------------------*
000090*         ��f��No.�ύX�E�폜
000100*         MED = 
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2009-09-03
000130 DATE-COMPILED.          2009-09-03
000140*----------------------------------------------------------------*
000150* 2012/06/26 ���c ���a
000151* ���Î��т̉��Ïꏊ���҃R�[�h���揇���҃R�[�h���X�V�Ɋ܂߂�i�S�����[�h���Y�������C�g�j
000152* 2018/10/26 ���c ���a �g�񍐏��t�@�C����ǉ�
000154*----------------------------------------------------------------*
000155******************************************************************
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
000260     SELECT  ��f�ҏ��e    ASSIGN      TO        JUSINJL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  ��|�{�p�a��N��
000300                                                          ��|���҃R�[�h
000310                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000320                                                          ��|���҃J�i
000330                                                          ��|���҃R�[�h
000340                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
000350                                                          ��|�{�p�a��N��
000360                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000370                                                          ��|�ی����
000380                                                          ��|�ی��Ҕԍ�
000390                                                          ��|���҃R�[�h
000400                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000410                                                          ��|������
000420                                                          ��|��p���S�Ҕԍ�
000430                                                          ��|���҃R�[�h
000440                             ALTERNATE RECORD KEY     IS  ��|�{�p�a��N��
000450                                                          ��|�������
000460                                                          ��|��p���S�Ҕԍ�����
000470                                                          ��|���҃R�[�h
000480                             ALTERNATE RECORD KEY     IS  ��|�����a��N��
000490                                                          ��|�{�p�a��N��
000500                                                          ��|���҃R�[�h
000510                             FILE STATUS              IS  ��ԃL�[
000520                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  ���������e      ASSIGN      TO        HUGEINL
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS MODE              IS  DYNAMIC
000560                             RECORD KEY               IS  �����|�敪�R�[�h
000570                                                          �����|���������R�[�h
000580                             FILE STATUS              IS  ��ԃL�[
000590                             LOCK        MODE         IS  AUTOMATIC.
000600     SELECT  �����f�[�^�e    ASSIGN      TO        HUSYOUL
000610                             ORGANIZATION             IS  INDEXED
000620                             ACCESS MODE              IS  DYNAMIC
000630                             RECORD KEY               IS  ���|�{�p�a��N��
000640                                                          ���|���҃R�[�h
000650                             ALTERNATE RECORD KEY     IS  ���|���҃R�[�h
000660                                                          ���|�{�p�a��N��
000670                             FILE STATUS              IS  ��ԃL�[
000680                             LOCK        MODE         IS  AUTOMATIC.
000690     SELECT  ��t�f�[�^�e    ASSIGN      TO        UKETUKEL
000700                             ORGANIZATION             IS  INDEXED
000710                             ACCESS MODE              IS  DYNAMIC
000720                             RECORD KEY               IS  ��t�|�{�p�a��N����
000730                                                          ��t�|���҃R�[�h
000740                             ALTERNATE RECORD KEY     IS  ��t�|�{�p�a��N����
000750                                                          ��t�|��t����
000760                                                          ��t�|���҃R�[�h
000770                             ALTERNATE RECORD KEY     IS  ��t�|���҃R�[�h
000780                                                          ��t�|�{�p�a��N����
000790                                                          ��t�|��t����
000800                             ALTERNATE RECORD KEY     IS  ��t�|�{�p�a��N����
000810                                                          ��t�|�A��
000820                             FILE STATUS              IS  ��ԃL�[
000830                             LOCK        MODE         IS  AUTOMATIC.
000840     SELECT  �{�p�L�^�e      ASSIGN      TO        SEKIROKL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS  �{�L�|�{�p�a��N����
000880                                                          �{�L�|���҃R�[�h
000890                             ALTERNATE RECORD KEY     IS  �{�L�|���҃R�[�h
000900                                                          �{�L�|�{�p�a��N����
000910                             FILE STATUS              IS  ��ԃL�[
000920                             LOCK        MODE         IS  AUTOMATIC.
000930     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000940                             ORGANIZATION             IS  INDEXED
000950                             ACCESS MODE              IS  DYNAMIC
000960                             RECORD KEY               IS  ���|����敪
000970                             FILE STATUS              IS  ��ԃL�[
000980                             LOCK        MODE         IS  AUTOMATIC.
001102     SELECT  ��v�f�[�^�e    ASSIGN      TO        KAIKEIL
001103                             ORGANIZATION             IS  INDEXED
001104                             ACCESS MODE              IS  DYNAMIC
001105                             RECORD KEY               IS  ��|�{�p�a��N����
001106                                                          ��|���҃R�[�h
001107                             ALTERNATE RECORD KEY     IS  ��|���҃R�[�h
001108                                                          ��|�{�p�a��N����
001109                             FILE STATUS              IS  ��ԃL�[
001110                             LOCK        MODE         IS  AUTOMATIC.
001111*
001113     SELECT  �����p���҂e    ASSIGN      TO        CHOKEIL
001120                             ORGANIZATION             IS INDEXED
001130                             ACCESS MODE              IS DYNAMIC
001140                             RECORD KEY               IS ���p�|�{�p�a��N��
001150                                                         ���p�|���҃R�[�h
001160                             ALTERNATE RECORD KEY     IS ���p�|���҃R�[�h
001170                                                         ���p�|�{�p�a��N��
001180                             FILE STATUS              IS ��ԃL�[
001190                             LOCK      MODE           IS AUTOMATIC.
001200     SELECT �o�[�R�[�h�Ǘ��e ASSIGN      TO        BARKANRL
001210                             ORGANIZATION             IS  INDEXED
001220                             ACCESS MODE              IS  DYNAMIC
001230                             RECORD KEY               IS  �o�ǁ|�����ԍ�
001240                             ALTERNATE KEY            IS  �o�ǁ|�{�p�a��N��
001250                                                          �o�ǁ|���҃R�[�h
001260                                                          �o�ǁ|�����ԍ�
001270                             ALTERNATE KEY            IS  �o�ǁ|���҃R�[�h
001280                                                          �o�ǁ|�{�p�a��N��
001290                                                          �o�ǁ|�����ԍ�
001300                             ALTERNATE KEY            IS  �o�ǁ|���҃J�i
001310                                                          �o�ǁ|�{�p�a��N��
001320                                                          �o�ǁ|���҃R�[�h
001330                                                          �o�ǁ|�����ԍ�
001340                             ALTERNATE KEY            IS  �o�ǁ|�{�p�a��N��
001350                                                          �o�ǁ|���҃J�i
001360                                                          �o�ǁ|�����ԍ�
001370                             ALTERNATE KEY            IS  �o�ǁ|���Z�v�g���
001380                                                          �o�ǁ|�{�p�a��N��
001390                                                          �o�ǁ|���҃R�[�h
001400                             ALTERNATE KEY            IS  �o�ǁ|���Z�v�g���
001410                                                          �o�ǁ|���҃R�[�h
001420                                                          �o�ǁ|�{�p�a��N��
001430                             ALTERNATE KEY            IS  �o�ǁ|�{�p�a��N��
001440                                                          �o�ǁ|���҃R�[�h
001450                                                          �o�ǁ|���Z�v�g���
001460                             ALTERNATE KEY            IS  �o�ǁ|���҃R�[�h
001470                                                          �o�ǁ|�{�p�a��N��
001480                                                          �o�ǁ|���Z�v�g���
001490                             FILE STATUS              IS  ��ԃL�[
001500                             LOCK        MODE         IS  AUTOMATIC.
001791     SELECT  ���Z�v�g�e      ASSIGN      TO        RECEPTL
001792                             ORGANIZATION             IS  INDEXED
001793                             ACCESS MODE              IS  DYNAMIC
001794                             RECORD KEY               IS  ���Z�|�{�p�a��N��
001795                                                          ���Z�|���҃R�[�h
001796                                                          ���Z�|���Z���
001797                             ALTERNATE RECORD KEY     IS  ���Z�|���҃R�[�h
001798                                                          ���Z�|�{�p�a��N��
001799                                                          ���Z�|���Z���
001800                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
001801                                                          ���Z�|�{�p�a��N��
001802                                                          ���Z�|���҃R�[�h
001803                                                          ���Z�|���Z���
001804                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
001805                                                          ���Z�|���Z���
001806                                                          ���Z�|�����ی��Ҕԍ�
001807                                                          ���Z�|���҃R�[�h
001808                                                          ���Z�|�{�p�a��N��
001809                             ALTERNATE RECORD KEY     IS  ���Z�|�����a��N��
001810                                                          ���Z�|�����ی��Ҕԍ�
001811                                                          ���Z�|���҃R�[�h
001812                                                          ���Z�|���Z���
001813                                                          ���Z�|�{�p�a��N��
001814                             FILE STATUS              IS  ��ԃL�[
001815                             LOCK        MODE         IS  AUTOMATIC.
001816     SELECT �J���e�t�@�C��   ASSIGN      TO        KARUTEL
001817                             ORGANIZATION             IS  INDEXED
001820                             ACCESS                   IS  DYNAMIC
001830                             RECORD      KEY          IS  �J�|��R�[�h
001840                                                          �J�|�敪
001850                                                          �J�|�{�p�a��N��
001860                                                          �J�|���҃R�[�h
001870                             ALTERNATE RECORD KEY     IS  �J�|��R�[�h
001880                                                          �J�|�敪
001890                                                          �J�|���҃R�[�h
001900                                                          �J�|�{�p�a��N��
001910                             ALTERNATE RECORD KEY     IS  �J�|�敪
001920                                                          �J�|�{�p�a��N��
001930                                                          �J�|���҃R�[�h
001940                                                          �J�|��R�[�h
001950                             ALTERNATE RECORD KEY     IS  �J�|�敪
001960                                                          �J�|���҃R�[�h
001970                                                          �J�|�{�p�a��N��
001980                                                          �J�|��R�[�h
001990                             FILE        STATUS       IS  ��ԃL�[
002000                             LOCK        MODE         IS  AUTOMATIC.
002010     SELECT  �����t�@�C��    ASSIGN      TO        MEMOL
002020                             ORGANIZATION             IS  INDEXED
002030                             ACCESS MODE              IS  DYNAMIC
002040                             RECORD KEY               IS  �����|����敪
002050                                                          �����|���҃R�[�h
002060                                                          �����|�{�p�a��N����
002070                             ALTERNATE RECORD KEY     IS  �����|����敪
002080                                                          �����|�{�p�a��N����
002090                                                          �����|���҃R�[�h
002100                             ALTERNATE RECORD KEY     IS  �����|���҃R�[�h
002110                                                          �����|�{�p�a��N����
002120                                                          �����|����敪
002130                             FILE STATUS              IS  ��ԃL�[
002140                             LOCK        MODE         IS  AUTOMATIC.
002141     SELECT  �E�v�t�@�C��    ASSIGN      TO        TEKIYOL
002142                             ORGANIZATION             IS  INDEXED
002143                             ACCESS MODE              IS  DYNAMIC
002144                             RECORD KEY               IS  �E�v�|����敪
002145                                                          �E�v�|���҃R�[�h
002146                                                          �E�v�|�{�p�a��N��
002147                             ALTERNATE RECORD KEY     IS  �E�v�|����敪
002148                                                          �E�v�|�{�p�a��N��
002149                                                          �E�v�|���҃R�[�h
002150                             FILE STATUS              IS  ��ԃL�[
002151                             LOCK        MODE         IS  AUTOMATIC.
002152**
002153     SELECT  �g���Z�v�g�e    ASSIGN      TO        HRECEL
002160                             ORGANIZATION             IS  INDEXED
002170                             ACCESS MODE              IS  DYNAMIC
002180                             RECORD KEY               IS  �g���Z�|�{�p�敪
002190                                                          �g���Z�|�{�p�a��N��
002200                                                          �g���Z�|���҃R�[�h
002210                                                          �g���Z�|���Z���
002220                             ALTERNATE RECORD KEY     IS  �g���Z�|�{�p�敪
002230                                                          �g���Z�|���҃R�[�h
002240                                                          �g���Z�|�{�p�a��N��
002250                                                          �g���Z�|���Z���
002260                             ALTERNATE RECORD KEY     IS  �g���Z�|�{�p�a��N��
002270                                                          �g���Z�|���҃R�[�h
002280                                                          �g���Z�|���Z���
002290                                                          �g���Z�|�{�p�敪
002300                             ALTERNATE RECORD KEY     IS  �g���Z�|�����Ώۋ敪
002310                                                          �g���Z�|�����a��N��
002320                                                          �g���Z�|�{�p�敪
002330                                                          �g���Z�|�{�p�a��N��
002340                                                          �g���Z�|���҃R�[�h
002350                                                          �g���Z�|���Z���
002360                             FILE STATUS              IS  ��ԃL�[
002370                             LOCK        MODE         IS  AUTOMATIC.
002380     SELECT  �g���v�f�[�^�e  ASSIGN      TO        HNIKEIL
002390                             ORGANIZATION             IS  INDEXED
002400                             ACCESS MODE              IS  DYNAMIC
002410                             RECORD KEY               IS  �g���|�{�p�敪
002420                                                          �g���|�{�p�a��N����
002430                                                          �g���|���҃R�[�h
002440                             ALTERNATE RECORD KEY     IS  �g���|�{�p�敪
002450                                                          �g���|���҃R�[�h
002460                                                          �g���|�{�p�a��N����
002470                             ALTERNATE RECORD KEY     IS  �g���|�{�p�a��N����
002480                                                          �g���|�o�^��
002490                             FILE STATUS              IS  ��ԃL�[
002500                             LOCK        MODE         IS  AUTOMATIC.
002510     SELECT  �g�����f�[�^�e  ASSIGN      TO        HHUSYOUL
002520                             ORGANIZATION             IS  INDEXED
002530                             ACCESS MODE              IS  DYNAMIC
002540                             RECORD KEY               IS  �g���|��L�[
002550                             ALTERNATE RECORD KEY     IS  �g���|�{�p�敪
002560                                                          �g���|���҃R�[�h
002570                                                          �g���|��L�[
002580                             FILE STATUS              IS  ��ԃL�[
002590                             LOCK        MODE         IS  AUTOMATIC.
002600     SELECT  ��f�ҏ��Q�e  ASSIGN      TO        JUSINJ2L
002610                             ORGANIZATION             IS INDEXED
002620                             ACCESS MODE              IS DYNAMIC
002630                             RECORD KEY               IS ��Q�|�{�p�a��N��
002640                                                         ��Q�|���҃R�[�h
002650                             ALTERNATE RECORD KEY     IS ��Q�|�����Ώۋ敪
002660                                                         ��Q�|�����a��N��
002670                                                         ��Q�|�{�p�a��N��
002680                                                         ��Q�|���҃R�[�h
002690                             ALTERNATE RECORD KEY     IS ��Q�|���������Ώۋ敪
002700                                                         ��Q�|���������a��N��
002710                                                         ��Q�|�{�p�a��N��
002720                                                         ��Q�|���҃R�[�h
002730                             FILE STATUS              IS  ��ԃL�[
002740                             LOCK        MODE         IS  AUTOMATIC.
002750     SELECT  ���ۏ��e      ASSIGN      TO        SEIHOJL
002760                             ORGANIZATION             IS INDEXED
002770                             ACCESS MODE              IS DYNAMIC
002780                             RECORD KEY               IS ���ہ|�{�p�a��N��
002790                                                         ���ہ|���҃R�[�h
002800                             ALTERNATE RECORD KEY     IS ���ہ|���҃R�[�h
002810                                                         ���ہ|�{�p�a��N��
002820                             FILE STATUS              IS ��ԃL�[
002830                             LOCK        MODE         IS AUTOMATIC.
002840     SELECT  �����ӏ��e    ASSIGN      TO        JIBAIJL
002850                             ORGANIZATION             IS INDEXED
002860                             ACCESS MODE              IS DYNAMIC
002870                             RECORD KEY               IS �����|�{�p�a��N��
002880                                                         �����|���҃R�[�h
002890                             ALTERNATE RECORD KEY     IS �����|���҃R�[�h
002900                                                         �����|�{�p�a��N��
002910                             FILE STATUS              IS ��ԃL�[
002920                             LOCK        MODE         IS AUTOMATIC.
002930     SELECT  �J�Џ��e      ASSIGN      TO        ROUSAIJL
002940                             ORGANIZATION             IS INDEXED
002950                             ACCESS MODE              IS DYNAMIC
002960                             RECORD KEY               IS �J�Ё|�{�p�a��N��
002970                                                         �J�Ё|���҃R�[�h
002980                             ALTERNATE RECORD KEY     IS �J�Ё|���҃R�[�h
002990                                                         �J�Ё|�{�p�a��N��
003000                             FILE STATUS              IS ��ԃL�[
003010                             LOCK        MODE         IS AUTOMATIC.
003020     SELECT  ��v�̎��e      ASSIGN      TO        RYOSYUL
003030                             ORGANIZATION             IS  INDEXED
003040                             ACCESS MODE              IS  DYNAMIC
003050                             RECORD KEY               IS  �́|�{�p�敪
003060                                                          �́|��v�̎��敪
003070                                                          �́|�{�p�a��N����
003080                                                          �́|���҃R�[�h
003090                             ALTERNATE RECORD KEY     IS  �́|�{�p�敪
003100                                                          �́|���҃R�[�h
003110                                                          �́|��v�̎��敪
003120                                                          �́|�{�p�a��N����
003130                             FILE STATUS              IS  ��ԃL�[
003140                             LOCK        MODE         IS  AUTOMATIC.
003141*
003142     SELECT  �c�l�L�^�e          ASSIGN      TO        DMKIROKL
003143                                 ORGANIZATION             IS  INDEXED
003144                                 ACCESS MODE              IS  DYNAMIC
003145                                 RECORD KEY               IS  �c�l�|���s�a��N����
003146                                                              �c�l�|���s�}��
003147                                                              �c�l�|���҃R�[�h
003148                                 ALTERNATE RECORD KEY     IS  �c�l�|���҃R�[�h
003149                                                              �c�l�|���s�a��N����
003150                                                              �c�l�|���s�}��
003151                                 FILE STATUS              IS  ��ԃL�[
003152                                 LOCK        MODE         IS  AUTOMATIC.
003153*
003154     SELECT  �{�ݎ�f�҃}�X�^   ASSIGN      TO       SISETJUL
003155                                ORGANIZATION             IS  INDEXED
003156                                ACCESS MODE              IS  DYNAMIC
003157                                RECORD KEY               IS  �{�ݎ�|�{�݃R�[�h
003158                                                             �{�ݎ�|���҃R�[�h
003159                                ALTERNATE RECORD KEY     IS  �{�ݎ�|�{�݃R�[�h
003160                                                             �{�ݎ�|���݋敪
003161                                                             �{�ݎ�|���҃R�[�h
003162                                ALTERNATE RECORD KEY     IS  �{�ݎ�|���҃R�[�h
003163                                                             �{�ݎ�|�{�݃R�[�h
003164                                FILE STATUS              IS  ��ԃL�[
003165                                LOCK        MODE         IS  AUTOMATIC.
003166     SELECT  �g���Î��тe    ASSIGN      TO        HNOURYOL
003167                             ORGANIZATION             IS  INDEXED
003168                             ACCESS MODE              IS  DYNAMIC
003169                             RECORD KEY               IS  �g�����|�{�p�敪
003170                                                          �g�����|�{�p�a��N����
003171                                                          �g�����|���҃R�[�h
003172                             ALTERNATE RECORD KEY     IS  �g�����|�{�p�敪
003173                                                          �g�����|���҃R�[�h
003174                                                          �g�����|�{�p�a��N����
003175                             ALTERNATE RECORD KEY     IS  �g�����|�{�p�a��N����
003176                                                          �g�����|�{�p�Ҕԍ�
003177                                                          �g�����|�o�^��
003178                             ALTERNATE RECORD KEY     IS  �g�����|�{�p�a��N����
003179                                                          �g�����|�{�p�J�n����
003180                                                          �g�����|�{�p�Ҕԍ�
003181                                                          �g�����|�o�^��
003182                             FILE STATUS              IS  ��ԃL�[
003183                             LOCK        MODE         IS  AUTOMATIC.
003184     SELECT  �g���×\��e    ASSIGN      TO        HNOYOTEL
003185                             ORGANIZATION             IS  INDEXED
003186                             ACCESS MODE              IS  DYNAMIC
003187                             RECORD KEY               IS  �g���\�|�{�p�敪
003188                                                          �g���\�|�{�p�a��N����
003189                                                          �g���\�|���҃R�[�h
003190                             ALTERNATE RECORD KEY     IS  �g���\�|�{�p�a��N����
003191                                                          �g���\�|���҃R�[�h
003192                                                          �g���\�|�{�p�敪
003193                             ALTERNATE RECORD KEY     IS  �g���\�|���҃R�[�h
003194                                                          �g���\�|�{�p�a��N����
003195                                                          �g���\�|�{�p�敪
003196                             ALTERNATE RECORD KEY     IS  �g���\�|�{�p�a��N����
003197                                                          �g���\�|�{�p�Ҕԍ�
003198                                                          �g���\�|�{�p�J�n����
003199                                                          �g���\�|�o�^��
003200                                                          �g���\�|���҃R�[�h
003201                             ALTERNATE RECORD KEY     IS  �g���\�|�{�p�a��N����
003202                                                          �g���\�|�{�p�J�n����
003203                                                          �g���\�|�{�p�Ҕԍ�
003204                                                          �g���\�|�o�^��
003205                                                          �g���\�|���҃R�[�h
003206                             FILE STATUS              IS  ��ԃL�[
003207                             LOCK        MODE         IS  AUTOMATIC.
003216     SELECT  ���Z�v�g�J���e  ASSIGN      TO        RECERJL
003217                             ORGANIZATION             IS  INDEXED
003218                             ACCESS MODE              IS  DYNAMIC
003219                             RECORD KEY               IS  ���Z�J���|�{�p�a��N��
003220                                                          ���Z�J���|���҃R�[�h
003221                                                          ���Z�J���|���Z���
003225                             FILE STATUS              IS  ��ԃL�[
003226                             LOCK        MODE         IS  AUTOMATIC.
003227*
003228     SELECT  �ؖ��������e    ASSIGN      TO        SYOMEIRL
003229                             ORGANIZATION             IS  INDEXED
003230                             ACCESS                   IS  DYNAMIC
003231                             RECORD      KEY          IS  �ؗ��|�{�p�敪
003232                                                          �ؗ��|�p���敪
003233                                                          �ؗ��|�{�p�a��N��
003234                                                          �ؗ��|���҃R�[�h
003235                                                          �ؗ��|�A��
003236                             FILE        STATUS       IS  ��ԃL�[
003237                             LOCK        MODE         IS  AUTOMATIC.
003238*
003239*****
003240*****
003241     SELECT  ����f�ҏ��e    ASSIGN      TO        JUSINJL
003242                             ORGANIZATION             IS  INDEXED
003243                             ACCESS MODE              IS  DYNAMIC
003244                             RECORD KEY               IS  ����|�{�p�a��N��
003245                                                          ����|���҃R�[�h
003246                             ALTERNATE RECORD KEY     IS  ����|�{�p�a��N��
003247                                                          ����|���҃J�i
003248                                                          ����|���҃R�[�h
003249                             ALTERNATE RECORD KEY     IS  ����|���҃R�[�h
003250                                                          ����|�{�p�a��N��
003251                             ALTERNATE RECORD KEY     IS  ����|�{�p�a��N��
003252                                                          ����|�ی����
003253                                                          ����|�ی��Ҕԍ�
003254                                                          ����|���҃R�[�h
003255                             ALTERNATE RECORD KEY     IS  ����|�{�p�a��N��
003256                                                          ����|������
003257                                                          ����|��p���S�Ҕԍ�
003258                                                          ����|���҃R�[�h
003259                             ALTERNATE RECORD KEY     IS  ����|�{�p�a��N��
003260                                                          ����|�������
003261                                                          ����|��p���S�Ҕԍ�����
003262                                                          ����|���҃R�[�h
003263                             ALTERNATE RECORD KEY     IS  ����|�����a��N��
003264                                                          ����|�{�p�a��N��
003265                                                          ����|���҃R�[�h
003266                             FILE STATUS              IS  ��ԃL�[
003267                             LOCK        MODE         IS  AUTOMATIC.
003268*
003269     SELECT  �g�񍐏��e      ASSIGN      TO        HHOKOKL
003270                             ORGANIZATION             IS  INDEXED
003271                             ACCESS                   IS  DYNAMIC
003272                             RECORD      KEY          IS  �g��|�{�p�敪
003273                                                          �g��|�p���敪
003274                                                          �g��|�{�p�a��N��
003275                                                          �g��|���҃R�[�h
003276                                                          �g��|�A��
003277                             ALTERNATE RECORD KEY     IS  �g��|�{�p�敪
003278                                                          �g��|�p���敪
003279                                                          �g��|���҃R�[�h
003280                                                          �g��|�{�p�a��N��
003281                                                          �g��|�{�p��
003282                                                          �g��|�A��
003283                             FILE        STATUS       IS  ��ԃL�[
003284                             LOCK        MODE         IS  AUTOMATIC.
003285*
000140     SELECT  �g���Z�v�g�ڍׂe    ASSIGN      TO       HRECEHKL
000150                             ORGANIZATION             IS  INDEXED
000160                             ACCESS MODE              IS  DYNAMIC
000170                             RECORD KEY               IS  �g���Z�ڍׁ|�{�p�敪
000180                                                          �g���Z�ڍׁ|�{�p�a��N��
000190                                                          �g���Z�ڍׁ|���҃R�[�h
000200                                                          �g���Z�ڍׁ|���Z���
000350                             FILE STATUS              IS  ��ԃL�[
000360                             LOCK        MODE         IS  AUTOMATIC.
003285*
003286******************************************************************
003287*                      DATA DIVISION                             *
003288******************************************************************
003289 DATA                    DIVISION.
003290 FILE                    SECTION.
003291*                           �m�q�k��  �R�Q�O�n
003292 FD  ��f�ҏ��e        BLOCK   CONTAINS   1   RECORDS.
003293     COPY JUSINJ     OF  XFDLIB  JOINING   ��   AS  PREFIX.
003294*                           �m�q�k��  �P�Q�W�n
003295 FD  ���������e          BLOCK   CONTAINS   1   RECORDS.
003296     COPY HUGEIN     OF  XFDLIB  JOINING   ����   AS  PREFIX.
003297*                           �m�q�k��  �P�Q�W�n
003298 FD  �����f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
003299     COPY HUSYOU     OF  XFDLIB  JOINING   ��   AS  PREFIX.
003300*                           �m�q�k��  �P�Q�W�n
003301 FD  ��t�f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
003310     COPY UKETUKE    OF  XFDLIB  JOINING   ��t AS  PREFIX.
003320*                           �m�q�k��  �Q�T�U�n
003330 FD  �{�p�L�^�e          BLOCK   CONTAINS   1   RECORDS.
003340     COPY SEKIROK    OF  XFDLIB  JOINING   �{�L AS  PREFIX.
003350*                           �m�q�k��  �Q�T�U�n
003360 FD  ������}�X�^      BLOCK   CONTAINS   1   RECORDS.
003370     COPY SEIGYO     OF  XFDLIB  JOINING   ��   AS  PREFIX.
003380*                           �m�q�k��  �P�Q�W�n
003390 FD  ��v�f�[�^�e        BLOCK   CONTAINS   1   RECORDS.
003400     COPY KAIKEI     OF  XFDLIB  JOINING   ��   AS  PREFIX.
003410*                           �m�q�k��  �P�Q�W�n
003420 FD  �����p���҂e        BLOCK   CONTAINS   1   RECORDS.
003430     COPY CHOKEI     OF  XFDLIB  JOINING   ���p   AS  PREFIX.
003440*                           �m�q�k��  �U�S�n
003450 FD  �o�[�R�[�h�Ǘ��e    BLOCK   CONTAINS   1   RECORDS.
003460     COPY BARKANR    OF  XFDLIB  JOINING   �o��  AS  PREFIX.
003470*
003480 FD  ���Z�v�g�e          BLOCK   CONTAINS   1   RECORDS.
003490     COPY RECEPT     OF  XFDLIB  JOINING   ���Z   AS  PREFIX.
003500*                           �m�q�k��  �R�W�S�O�n
003510 FD  �J���e�t�@�C��      BLOCK   CONTAINS   1   RECORDS.
003520     COPY KARUTE     OF  XFDLIB  JOINING   �J   AS  PREFIX.
003530     COPY KARUTA     OF  XFDLIB  JOINING   �J�\ AS  PREFIX.
003540     COPY KARUTB     OF  XFDLIB  JOINING   �J�� AS  PREFIX.
003550*                           �m�q�k��  �W�R�Q�n
003560 FD  �����t�@�C��        BLOCK CONTAINS 1     RECORDS.
003570     COPY MEMO       OF  XFDLIB JOINING ���� AS PREFIX.
003571*
003572 FD  �E�v�t�@�C��        BLOCK CONTAINS 1     RECORDS.
003573     COPY TEKIYO     OF    XFDLIB JOINING �E�v AS PREFIX.
003574*
003580*                           �m�q�k��  �V�U�W�n
003590 FD  �g���Z�v�g�e        BLOCK   CONTAINS   1   RECORDS.
003600     COPY H_RECE     OF  XFDLIB  JOINING   �g���Z  AS  PREFIX.
003610*                           �m�q�k��  �T�P�Q�n
003620 FD  �g���v�f�[�^�e      BLOCK   CONTAINS   1   RECORDS.
003630     COPY H_NIKEI    OF  XFDLIB  JOINING   �g��   AS  PREFIX.
003640*                           �m�q�k��  �U�S�O�n
003650 FD  �g�����f�[�^�e      BLOCK   CONTAINS   1   RECORDS.
003660     COPY H_HUSYOU   OF  XFDLIB  JOINING   �g�� AS  PREFIX.
003670*
003680 FD  ��f�ҏ��Q�e      BLOCK   CONTAINS   1   RECORDS.
003690     COPY JUSINJ2    OF  XFDLIB  JOINING   ��Q   AS  PREFIX.
003700*
003710 FD  ���ۏ��e          BLOCK   CONTAINS   1   RECORDS.
003720     COPY SEIHOJ     OF  XFDLIB  JOINING   ����   AS  PREFIX.
003730*
003740 FD  �����ӏ��e        BLOCK   CONTAINS   1   RECORDS.
003750     COPY JIBAIJ     OF  XFDLIB  JOINING   ����   AS  PREFIX.
003760*
003770 FD  �J�Џ��e          BLOCK   CONTAINS   1   RECORDS.
003780     COPY ROUSAIJ    OF  XFDLIB  JOINING   �J��   AS  PREFIX.
003790*
003800 FD  ��v�̎��e          BLOCK   CONTAINS   1   RECORDS.
003810     COPY RYOSYU     OF  XFDLIB  JOINING   ��  AS  PREFIX.
003811*
003812 FD  �c�l�L�^�e          BLOCK   CONTAINS   1   RECORDS.
003813     COPY DMKIROK    OF  XFDLIB  JOINING   �c�l   AS  PREFIX.
003814*
003815 FD  �{�ݎ�f�҃}�X�^  BLOCK   CONTAINS   1   RECORDS.
003816     COPY SISETJU    OF  XFDLIB  JOINING   �{�ݎ�   AS  PREFIX.
003817*
003818 FD  �g���Î��тe   BLOCK   CONTAINS   1   RECORDS.
003819     COPY H_NOURYO   OF  XFDLIB  JOINING   �g����   AS  PREFIX.
003820*
003821 FD  �g���×\��e   BLOCK   CONTAINS   1   RECORDS.
003822     COPY H_NOYOTE   OF  XFDLIB  JOINING   �g���\   AS  PREFIX.
003824*
003825 FD  ���Z�v�g�J���e      BLOCK   CONTAINS   1   RECORDS.
003826     COPY RECERJ          OF  XFDLIB  JOINING   ���Z�J�� AS  PREFIX.
003827*
003828 FD  �ؖ��������e      BLOCK   CONTAINS   1   RECORDS GLOBAL.
003829     COPY SYOMEIRR         OF  XFDLIB  JOINING   �ؗ�     AS  PREFIX.
003830*
003831***
003832*
003833 FD  ����f�ҏ��e      BLOCK   CONTAINS   1   RECORDS.
003834     COPY JUSINJ     OF  XFDLIB  JOINING   ����   AS  PREFIX.
003835*                           �m�q�k��  2048�n
003836 FD  �g�񍐏��e          BLOCK   CONTAINS   1   RECORDS GLOBAL.
003837     COPY H_HOKOK         OF  XFDLIB  JOINING   �g��     AS  PREFIX.
003838*
000380 FD  �g���Z�v�g�ڍׂe        BLOCK   CONTAINS   1   RECORDS.
000390     COPY H_RECEHK    OF  XFDLIB  JOINING   �g���Z�ڍ�  AS  PREFIX.
000113*
003839*---------------------------------------------------------------*
003840******************************************************************
003850*                WORKING-STORAGE SECTION                         *
003860******************************************************************
003870 WORKING-STORAGE         SECTION.
003880 01 �L�[����                           PIC X    VALUE SPACE.
003890 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
003900 01 �I���t���O                         PIC X(3) VALUE SPACE.
003901 01 �I���t���O�Q                       PIC X(3)  VALUE SPACE.
003910 01 �J�E���^                           PIC 9    VALUE ZERO.
003920 01 �t�@�C�����v                       PIC N(5) VALUE SPACE.
003930 01 �x���t���O                         PIC X(3) VALUE SPACE.
003940 01 �x���b�m�s                         PIC 9(5) VALUE ZERO.
003950 01 �x���J�E���^                       PIC 9(4) VALUE ZERO.
003960 01 �x���񐔂v                         PIC 9(4) VALUE ZERO.
003970*
003980 01 �{�p�敪�v                         PIC 9 VALUE ZERO.
003981 01 �ی����ނv                         PIC 9(2) VALUE ZERO.
003982**
003983 01 �J�ԃJ�E���^                       PIC 9(2) VALUE ZERO.
003984*�E�v�t�@�C���̐���敪��MAX�敪
003985 01 �E�v�敪��                         PIC 9(2) VALUE 10.
003986**
003987*
003988 01 ���҃R�[�h�v.
003989    03 ���Ҕԍ��v                      PIC 9(6) VALUE ZERO.
003990    03 �}�Ԃv                          PIC X    VALUE SPACE.
003991 01 �{�p�a��N���v.
003992    03 �{�p�a��v                      PIC 9    VALUE ZERO.
003993    03 �{�p�N�v                        PIC 9(2) VALUE ZERO.
003994    03 �{�p���v                        PIC 9(2) VALUE ZERO.
003995*
003996****
003997 01 ���Z���݃t���O                     PIC X(3)  VALUE SPACE.
003998**
003999 01 ���Î��эX�V�t���O                 PIC X(3)  VALUE SPACE.
004000**
004001**
004002*
004003* �o�[�R�[�h�Ǘ��e�ޔ�p
004010     COPY BARKANR    OF  XFDLIB  JOINING   �ޔ��o AS  PREFIX.
004020*
004030* �g�����f�[�^�e�ޔ�p
004040     COPY H_HUSYOU   OF  XFDLIB  JOINING   �ޔ� AS  PREFIX.
004050*
004060*****************************************************************
004070*                          �A������                             *
004080*****************************************************************
004090*
004100* �폜�A�g�L�[
004110*�����敪 1:�����폜(in �{�p�a��N��,���҃R�[�h)
004120*         2:�S���폜(in ���҃R�[�h)
004130*         3:���Ҕԍ��ύX(in ���҃R�[�h,�ύX���҃R�[�h)
004140*         4:�}�ԍ쐬(in �{�p�a��N��,���҃R�[�h,�ύX���҃R�[�h)
004150 01 �A���ҍ폜�|�L�[ IS EXTERNAL.
004160    03 �A���ҍ폜�|�����敪                PIC 9.
004170    03 �A���ҍ폜�|�{�p�a��N��.
004180       05 �A���ҍ폜�|�{�p�a��             PIC 9.
004190       05 �A���ҍ폜�|�{�p�N��.
004200          07 �A���ҍ폜�|�{�p�N            PIC 9(2).
004210          07 �A���ҍ폜�|�{�p��            PIC 9(2).
004220    03 �A���ҍ폜�|���҃R�[�h.
004230       05 �A���ҍ폜�|���Ҕԍ�             PIC 9(6).
004240       05 �A���ҍ폜�|�}��                 PIC X.
004250    03 �A���ҍ폜�|�ύX���҃R�[�h.
004260       05 �A���ҍ폜�|�ύX���Ҕԍ�         PIC 9(6).
004270       05 �A���ҍ폜�|�ύX�}��             PIC X.
004280*
004290* �}�ԘA�g�L�[
004300*
004310 01 �A���Ҏ}�ԁ|�L�[ IS EXTERNAL.
004320    03 �A���Ҏ}�ԁ|�ύX��                   PIC 9(2).
004330    03 �A���Ҏ}�ԁ|�ی��ύX�敪             PIC 9.
004340*
004350 01 �g�A�}�ԍ쐬�|�L�[ IS EXTERNAL.
004360   03 �g�A�}�ԍ쐬�|�{�p�a��N��.
004370      05 �g�A�}�ԍ쐬�|�{�p�a��              PIC 9.
004380      05 �g�A�}�ԍ쐬�|�{�p�N��.
004390        07 �g�A�}�ԍ쐬�|�{�p�N              PIC 9(2).
004400        07 �g�A�}�ԍ쐬�|�{�p��              PIC 9(2).
004410   03 �g�A�}�ԍ쐬�|���҃R�[�h.
004420     05 �g�A�}�ԍ쐬�|���Ҕԍ�               PIC 9(6).
004430     05 �g�A�}�ԍ쐬�|�}��                   PIC X.
004440   03 �g�A�}�ԍ쐬�|�V�l����                 PIC 9.
004450   03 �g�A�}�ԍ쐬�|�����҃R�[�h.
004451     05 �g�A�}�ԍ쐬�|�����Ҕԍ�             PIC 9(6).
004452     05 �g�A�}�ԍ쐬�|���}��                 PIC X.
004453*
004460* �N���p
004470 01 �A�N��Q�|�L�[ IS EXTERNAL.
004480    03 �A�N��Q�|�{�p�a��N��.
004490       05 �A�N��Q�|�{�p�a��      PIC 9.
004500       05 �A�N��Q�|�{�p�N��.
004510         07 �A�N��Q�|�{�p�N      PIC 9(2).
004520         07 �A�N��Q�|�{�p��      PIC 9(2).
004530    03 �A�N��Q�|�a��N����.
004540       05 �A�N��Q�|�a��          PIC 9.
004550       05 �A�N��Q�|�N            PIC 9(2).
004560       05 �A�N��Q�|��            PIC 9(2).
004570       05 �A�N��Q�|��            PIC 9(2).
004580    03 �A�N��Q�|�ی��N��         PIC 9(3).
004590    03 �A�N��Q�|���c������       PIC 9.
004600    03 �A�N��Q�|�V�l����         PIC 9.
004610*
004611* �V�_�v�Z�A��
004612 01 �A�x�v�Z�|�L�[ IS EXTERNAL.
004613   03 �A�x�v�Z�|�{�p�a��N��.
004614     05 �A�x�v�Z�|�{�p�a��           PIC 9.
004615     05 �A�x�v�Z�|�{�p�N��.
004616       07 �A�x�v�Z�|�{�p�N           PIC 9(2).
004617       07 �A�x�v�Z�|�{�p��           PIC 9(2).
004618   03 �A�x�v�Z�|���҃R�[�h.
004619     05 �A�x�v�Z�|���Ҕԍ�           PIC 9(6).
004620     05 �A�x�v�Z�|�}��               PIC X.
004621*
004622******************************************************************
004630*                      PROCEDURE  DIVISION                       *
004640******************************************************************
004650 PROCEDURE               DIVISION.
004660************
004670*           *
004680* ��������   *
004690*           *
004700************
004710     PERFORM ������.
004720     PERFORM ������擾.
004730************
004740*           *
004750* �又��     *
004760*           *
004770************
004780*--------------------------------------------------------*
004790*  �A���ҍ폜�|�����敪
004800*    1:�����폜(in �{�p�a��N��,���҃R�[�h)
004810*    2:�S���폜(in ���҃R�[�h)
004820*    3:���Ҕԍ��ύX(in ���҃R�[�h,�ύX���҃R�[�h)
004830*    4:�}�ԍ쐬(in �{�p�a��N��,���҃R�[�h,�ύX���҃R�[�h)
004840*--------------------------------------------------------*
004850
004860     EVALUATE �A���ҍ폜�|�����敪
004870     WHEN 1
004880         PERFORM �f�[�^�����폜
004890         PERFORM �͂肫�イ�f�[�^�����폜
004900     WHEN 2
004910         PERFORM �f�[�^�S�폜
004920         PERFORM �͂肫�イ�f�[�^�S�폜
004930     WHEN 3
004940         PERFORM �f�[�^�ύX
004950         PERFORM �͂肫�イ�f�[�^�ύX
004960     WHEN 4
004970         PERFORM �}�ԍ쐬
004980         PERFORM �͂肫�イ�}�ԍ쐬
004981*        / CLOSE�ς� �I������ /
004982         EXIT PROGRAM
004990     WHEN OTHER
005000        DISPLAY "�����敪�G���[�B�����ł��܂���B"  UPON CONS
005010     END-EVALUATE.
005020************
005030*           *
005040* �I������   *
005050*           *
005060************
005070     PERFORM �I������.
005080     EXIT PROGRAM.
005090*
005100*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
005110*================================================================*
005120 ������ SECTION.
005130*
005140     OPEN I-O ��f�ҏ��e.
005150             MOVE NC"��f" TO �t�@�C�����v.
005160             PERFORM �I�[�v���`�F�b�N.
005170     OPEN I-O ���������e.
005180             MOVE NC"��������" TO �t�@�C�����v.
005190             PERFORM �I�[�v���`�F�b�N.
005200     OPEN I-O �����f�[�^�e.
005210             MOVE NC"����" TO �t�@�C�����v.
005220             PERFORM �I�[�v���`�F�b�N.
005230     OPEN I-O ��t�f�[�^�e.
005240             MOVE NC"��t" TO �t�@�C�����v.
005250             PERFORM �I�[�v���`�F�b�N.
005260     OPEN I-O �{�p�L�^�e.
005270             MOVE NC"�{�L" TO �t�@�C�����v.
005280             PERFORM �I�[�v���`�F�b�N.
005290     OPEN I-O ������}�X�^.
005300             MOVE NC"����" TO �t�@�C�����v.
005310             PERFORM �I�[�v���`�F�b�N.
005320     OPEN I-O ��v�f�[�^�e.
005330             MOVE NC"��v" TO �t�@�C�����v.
005340             PERFORM �I�[�v���`�F�b�N.
005350     OPEN I-O �����p���҂e.
005360             MOVE NC"���p" TO �t�@�C�����v.
005370             PERFORM �I�[�v���`�F�b�N.
005380     OPEN I-O �o�[�R�[�h�Ǘ��e.
005390             MOVE NC"�o�[" TO �t�@�C�����v.
005400             PERFORM �I�[�v���`�F�b�N.
005410     OPEN I-O ���Z�v�g�e.
005420             MOVE NC"���Z" TO �t�@�C�����v.
005430             PERFORM �I�[�v���`�F�b�N.
005440     OPEN I-O �J���e�t�@�C��.
005450             MOVE NC"�J���e" TO �t�@�C�����v.
005460             PERFORM �I�[�v���`�F�b�N.
005470     OPEN I-O �����t�@�C��.
005480             MOVE NC"����" TO �t�@�C�����v.
005490             PERFORM �I�[�v���`�F�b�N.
005491     OPEN I-O �E�v�t�@�C��.
005492             MOVE NC"�E�v" TO �t�@�C�����v.
005493             PERFORM �I�[�v���`�F�b�N.
005500     OPEN I-O �g���Z�v�g�e.
005510             MOVE NC"�g���Z�v�g�e" TO �t�@�C�����v.
005520             PERFORM �I�[�v���`�F�b�N.
005530     OPEN I-O �g���v�f�[�^�e.
005540             MOVE NC"�g���v�f�[�^�e" TO �t�@�C�����v.
005550             PERFORM �I�[�v���`�F�b�N.
005560     OPEN I-O �g�����f�[�^�e.
005570             MOVE NC"�g�����f�[�^�e" TO �t�@�C�����v.
005580             PERFORM �I�[�v���`�F�b�N.
005590     OPEN I-O ��f�ҏ��Q�e.
005600             MOVE NC"��f�Q" TO �t�@�C�����v.
005610             PERFORM �I�[�v���`�F�b�N.
005620     OPEN I-O ���ۏ��e.
005630             MOVE NC"����" TO �t�@�C�����v.
005640             PERFORM �I�[�v���`�F�b�N.
005650     OPEN I-O �����ӏ��e.
005660             MOVE NC"����" TO �t�@�C�����v.
005670             PERFORM �I�[�v���`�F�b�N.
005680     OPEN I-O �J�Џ��e.
005690             MOVE NC"�J��" TO �t�@�C�����v.
005700             PERFORM �I�[�v���`�F�b�N.
005710     OPEN I-O ��v�̎��e.
005720             MOVE NC"��v" TO �t�@�C�����v.
005730             PERFORM �I�[�v���`�F�b�N.
005731     OPEN I-O �c�l�L�^�e.
005732             MOVE NC"�c�l�L�^" TO �t�@�C�����v.
005733             PERFORM �I�[�v���`�F�b�N.
005734     OPEN I-O �g���Î��тe.
005735             MOVE NC"�g���Î��тe" TO �t�@�C�����v.
005736             PERFORM �I�[�v���`�F�b�N.
005737     OPEN I-O �{�ݎ�f�҃}�X�^.
005738             MOVE NC"�{�ݎ�f�҃}�X�^" TO �t�@�C�����v.
005739             PERFORM �I�[�v���`�F�b�N.
005740     OPEN I-O �g���×\��e.
005741             MOVE NC"�g���×\��e" TO �t�@�C�����v.
005742             PERFORM �I�[�v���`�F�b�N.
005743     OPEN I-O ���Z�v�g�J���e.
005744             MOVE NC"���Z�v�g�J���e" TO �t�@�C�����v.
005745             PERFORM �I�[�v���`�F�b�N.
005746     OPEN I-O �ؖ��������e.
005747             MOVE NC"�ؖ��������e" TO �t�@�C�����v.
005748             PERFORM �I�[�v���`�F�b�N.
005754     OPEN I-O �g�񍐏��e.
005755             MOVE NC"�g�񍐏��e" TO �t�@�C�����v.
005756             PERFORM �I�[�v���`�F�b�N.
005500     OPEN I-O �g���Z�v�g�ڍׂe.
005900             IF ��ԃL�[  =  "35"
                       OPEN OUTPUT �g���Z�v�g�ڍׂe
                       CLOSE �g���Z�v�g�ڍׂe
005500                 OPEN I-O �g���Z�v�g�ڍׂe
                   END-IF.
005510             MOVE NC"�g���Z�v�g�ڍׂe" TO �t�@�C�����v.
005520             PERFORM �I�[�v���`�F�b�N.
005760*
005761*================================================================*
005762 �t�@�C���� SECTION.
005763*
005770     CLOSE ��f�ҏ��e     ���������e     �����f�[�^�e   ��t�f�[�^�e
005780           ������}�X�^   �{�p�L�^�e     ��v�f�[�^�e   �����p���҂e
005790           �o�[�R�[�h�Ǘ��e ���Z�v�g�e     �J���e�t�@�C�� �����t�@�C�� �E�v�t�@�C��
005800           �g���Z�v�g�e     �g���v�f�[�^�e �g�����f�[�^�e
005810           ��f�ҏ��Q�e   ���ۏ��e     �����ӏ��e   �J�Џ��e
005820           ��v�̎��e       �c�l�L�^�e     �g���Î��тe   �g���×\��e ���Z�v�g�J���e �ؖ��������e
005821           �{�ݎ�f�҃}�X�^ �g�񍐏��e     �g���Z�v�g�ڍׂe.
005830*================================================================*
005840 �I������ SECTION.
005850*
005860     PERFORM �t�@�C����.
005870*================================================================*
005880 �I�[�v���`�F�b�N SECTION.
005890*
005900     IF ��ԃL�[  NOT =  "00"
005910         DISPLAY �t�@�C�����v NC"�e�I�[�v���G���[" UPON CONS
005920         DISPLAY NC"��ԃL�[�F" ��ԃL�[           UPON CONS
005930         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
005940                                                   UPON CONS
005950*-----------------------------------------*
005960         CALL "actcshm"  WITH C LINKAGE
005970*-----------------------------------------*
005980         ACCEPT  �L�[���� FROM CONS
005990         PERFORM �t�@�C����
006000         EXIT PROGRAM.
006010*
006020*================================================================*
006030 �G���[�\���q SECTION.
006040*
006050     DISPLAY �t�@�C�����v NC"�t�@�C���Ǎ��G���["   UPON CONS.
006060     DISPLAY NC"��ԃL�[�F" ��ԃL�[               UPON CONS.
006070     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
006080     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
006090*-----------------------------------------*
006100     CALL "actcshm"  WITH C LINKAGE.
006110*-----------------------------------------*
006120     ACCEPT  �L�[���� FROM CONS.
006130*     PERFORM �t�@�C����.
006140*     EXIT PROGRAM.
006150*================================================================*
006160 �G���[�\�� SECTION.
006170*
006180     DISPLAY �t�@�C�����v NC"�t�@�C�������G���["   UPON CONS.
006190     DISPLAY NC"��ԃL�[�F" ��ԃL�[               UPON CONS.
006200     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
006210     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
006220*-----------------------------------------*
006230     CALL "actcshm"  WITH C LINKAGE.
006240*-----------------------------------------*
006250     ACCEPT  �L�[���� FROM CONS.
006260*     PERFORM �t�@�C����.
006270*     EXIT PROGRAM.
006280*================================================================*
006290 �G���[�\���c SECTION.
006300*
006310     DISPLAY �t�@�C�����v NC"�t�@�C���폜�G���["   UPON CONS.
006320     DISPLAY NC"��ԃL�[�F" ��ԃL�[               UPON CONS.
006330     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
006340     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"                                                                    UPON CONS.
006350*-----------------------------------------*
006360     CALL "actcshm"  WITH C LINKAGE.
006370*-----------------------------------------*
006380     ACCEPT  �L�[���� FROM CONS.
006390*     PERFORM �t�@�C����.
006400*     EXIT PROGRAM.
006410*================================================================*
006420 �f�[�^�����폜 SECTION.
006430*
006440     PERFORM ��f�ҏ��e�����폜.
006441     PERFORM ��f�ҏ��Q�e�����폜.
006442     PERFORM ��f�ҏ��[���č\�z.
006450*
006460     PERFORM �����f�[�^�e�����폜.
006470     PERFORM ��t�f�[�^�e�����폜.
006480     PERFORM �{�p�L�^�����폜.
006490     PERFORM ��v�f�[�^�e�����폜.
006500     PERFORM �����p���҂e�����폜.
006510     PERFORM �o�[�R�[�h�Ǘ��e�����폜.
006511     PERFORM ���Z�v�g�e�����폜.
006520     PERFORM ���Z�v�g�J���e�����폜.
006530     PERFORM �J���e�t�@�C�������폜.
006531     PERFORM �����t�@�C�������폜.
006540     PERFORM �E�v�t�@�C�������폜.
006550*
006570     PERFORM ���ۏ��e�����폜.
006580     PERFORM �����ӏ��e�����폜.
006590     PERFORM �J�Џ��e�����폜.
006600     PERFORM ��v�̎��e�����폜.
006610*
006620*================================================================*
006630 �͂肫�イ�f�[�^�����폜 SECTION.
006640*
006650     MOVE 1 TO �{�p�敪�v.
006660     PERFORM �g���Z�v�g�e�����폜.
006670     PERFORM �g���v�f�[�^�e�����폜.
006680     PERFORM �g�񍐏��e�����폜.
006681*
006690     MOVE 2 TO �{�p�敪�v.
006700     PERFORM �g���Z�v�g�e�����폜.
006710     PERFORM �g���v�f�[�^�e�����폜.
006720     PERFORM �g�񍐏��e�����폜.
006721*
006722*
006730*================================================================*
006740 �f�[�^�S�폜 SECTION.
006750*
006760     PERFORM ��f�ҏ��e�S�폜.
006770     IF �A���ҍ폜�|�}�� = SPACE
006780         PERFORM ���������e�S�폜
006790     END-IF.
006800     PERFORM �����f�[�^�e�S�폜.
006810     PERFORM ��t�f�[�^�e�S�폜.
006820     PERFORM �{�p�L�^�S�폜.
006830     PERFORM ��v�f�[�^�e�S�폜.
006840     PERFORM �����p���҂e�S�폜.
006850     PERFORM �o�[�R�[�h�Ǘ��e�S�폜.
006851     PERFORM ���Z�v�g�e�S�폜.
006860     PERFORM ���Z�v�g�J���e�S�폜.
006870     PERFORM �J���e�t�@�C���S�폜.
006871     PERFORM �����t�@�C���S�폜.
006880     PERFORM �E�v�t�@�C���S�폜.
006890*
006900     PERFORM ��f�ҏ��Q�e�S�폜.
006910     PERFORM ���ۏ��e�S�폜.
006920     PERFORM �����ӏ��e�S�폜.
006930     PERFORM �J�Џ��e�S�폜.
006931     PERFORM ��v�̎��e�S�폜.
006932     PERFORM �c�l�L�^�e�S�폜.
006940     PERFORM �{�ݎ�f�҃}�X�^�S�폜.
006941     PERFORM �ؖ��������e�S�폜.
006950*
006960*================================================================*
006970 �͂肫�イ�f�[�^�S�폜 SECTION.
006980*
006990     MOVE 1 TO �{�p�敪�v.
007000     PERFORM �g���Z�v�g�e�S�폜.
007010     PERFORM �g���v�f�[�^�e�S�폜.
007011     PERFORM �g�����f�[�^�e�S�폜.
007012     PERFORM �g���Î��тe�S�폜.
007030     PERFORM �g�񍐏��e�S�폜.
007031*
007040     MOVE 2 TO �{�p�敪�v.
007050     PERFORM �g���Z�v�g�e�S�폜.
007060     PERFORM �g���v�f�[�^�e�S�폜.
007070     PERFORM �g�����f�[�^�e�S�폜.
007071     PERFORM �g���Î��тe�S�폜.
007073     PERFORM �g�񍐏��e�S�폜.
007074*
007075*    1���OK
007076     PERFORM �g���×\��e�S�폜.
007091*================================================================*
007100 �f�[�^�ύX SECTION.
007110*
007120     PERFORM ��f�ҏ��e�ύX.
007130*
007140     PERFORM �����f�[�^�e�ύX.
007150     PERFORM ��t�f�[�^�e�ύX.
007160     PERFORM �{�p�L�^�ύX.
007170     PERFORM ��v�f�[�^�e�ύX.
007180     PERFORM �����p���҂e�ύX.
007190     PERFORM �o�[�R�[�h�Ǘ��e�ύX.
007191     PERFORM ���Z�v�g�e�ύX.
007200     PERFORM ���Z�v�g�J���e�ύX.
007210     PERFORM �J���e�t�@�C���ύX.
007211     PERFORM �����t�@�C���ύX.
007220     PERFORM �E�v�t�@�C���ύX.
007221     PERFORM ���������R���o�[�g.
007230*
007240     PERFORM ��f�ҏ��Q�e�ύX.
007250     PERFORM ���ۏ��e�ύX.
007260     PERFORM �����ӏ��e�ύX.
007270     PERFORM �J�Џ��e�ύX.
007280     PERFORM ��v�̎��e�ύX.
007281     PERFORM �c�l�L�^�e�ύX.
007282     PERFORM �{�ݎ�f�҃}�X�^�ύX.
007283     PERFORM �ؖ��������e�ύX.
007290*
007300*================================================================*
007310 �͂肫�イ�f�[�^�ύX SECTION.
007320*
007330     MOVE 1 TO �{�p�敪�v.
007340     PERFORM �g���Z�v�g�e�ύX.
007350     PERFORM �g���v�f�[�^�e�ύX.
007360     PERFORM �g�����f�[�^�e�ύX.
007361     PERFORM �g���Î��тe�ύX.
007371     PERFORM �g�񍐏��e�ύX.
007372*
007380     MOVE 2 TO �{�p�敪�v.
007390     PERFORM �g���Z�v�g�e�ύX.
007400     PERFORM �g���v�f�[�^�e�ύX.
007410     PERFORM �g�����f�[�^�e�ύX.
007411     PERFORM �g���Î��тe�ύX.
007414     PERFORM �g�񍐏��e�ύX.
007415*
007416*    1���OK
007417     PERFORM �g���×\��e�ύX.
007421*    ���Ïꏊ���҃R�[�h/�揇���҃R�[�h�X�V
007422     PERFORM �g���Î��тe�ύX�Q.
007424*
007430*================================================================*
007440 �}�ԍ쐬 SECTION.
007450*
007460*/���ԍ����w��}�ԂɃR�s�[
007470     PERFORM ��f�ҏ��e�}�ԍ쐬.
007480     PERFORM �����f�[�^�e�}�ԍ쐬.
007490     PERFORM �����p���҂e�}�ԍ쐬.
007500     PERFORM �J���e�t�@�C���}�ԍ쐬.
007510     PERFORM ��f�ҏ��Q�e�}�ԍ쐬.
007520     PERFORM ���ۏ��e�}�ԍ쐬.
007530     PERFORM �����ӏ��e�}�ԍ쐬.
007540     PERFORM �J�Џ��e�}�ԍ쐬.
007541     PERFORM �E�v�t�@�C���}�ԍ쐬.
007550*
007560*/���ԍ����w��}�ԂɃR�s�[�B���ԍ��̈ꕔ�ʉ@�f�[�^���폜�B
007570     PERFORM ��t�f�[�^�e�}�ԍ쐬.
007580     PERFORM �{�p�L�^�}�ԍ쐬.
007590     PERFORM ��v�f�[�^�e�}�ԍ쐬.
007600     PERFORM �o�[�R�[�h�Ǘ��e�}�ԍ쐬.
007601     PERFORM ���Z�v�g�e�}�ԍ쐬.
007610     PERFORM ���Z�v�g�J���e�}�ԍ쐬.
007620     PERFORM �����t�@�C���}�ԍ쐬.
007630     PERFORM ��v�̎��e�}�ԍ쐬.
007631     PERFORM �c�l�L�^�e�}�ԍ쐬.
007640*
007643*-------------------------------------------------------------------**
007644* ykeisan
007649*
007651*    / CLOSE���Ȃ��ƁAYKEISAN�ł��܂������Ȃ� /
007653     PERFORM �I������.
007654*
007655*
007656*   / ���Z�v�g�e���鎞�̂݌v�Z /
007657     IF ���Z���݃t���O = "YES"
007660* ���ԍ�
007661        MOVE SPACE TO �A�x�v�Z�|�L�[
007662        INITIALIZE �A�x�v�Z�|�L�[
007663        MOVE �A���ҍ폜�|�{�p�a��   TO �A�x�v�Z�|�{�p�a��
007664        MOVE �A���ҍ폜�|�{�p�N     TO �A�x�v�Z�|�{�p�N
007665        MOVE �A���ҍ폜�|�{�p��     TO �A�x�v�Z�|�{�p��
007666        MOVE �A���ҍ폜�|���҃R�[�h TO �A�x�v�Z�|���҃R�[�h
007667        CALL   "YKEISAN"
007668        CANCEL "YKEISAN"
007669***
007670* �}�Ԃ���
007671        MOVE �A���ҍ폜�|�ύX�}��   TO �A�x�v�Z�|�}��
007672        CALL   "YKEISAN"
007673        CANCEL "YKEISAN"
007674     END-IF.
007677*-------------------------------------------------------------------**
007681*
007682*================================================================*
007683 �͂肫�イ�}�ԍ쐬 SECTION.
007684*
007685     MOVE SPACE TO �g�A�}�ԍ쐬�|�L�[.
007690     INITIALIZE    �g�A�}�ԍ쐬�|�L�[.
007700*
007710     MOVE �A���ҍ폜�|�{�p�a��     TO �g�A�}�ԍ쐬�|�{�p�a��.
007720     MOVE �A���ҍ폜�|�{�p�N       TO �g�A�}�ԍ쐬�|�{�p�N.
007730     MOVE �A���ҍ폜�|�{�p��       TO �g�A�}�ԍ쐬�|�{�p��.
007740     MOVE �A���ҍ폜�|�ύX���Ҕԍ� TO �g�A�}�ԍ쐬�|���Ҕԍ�.
007750     MOVE �A���ҍ폜�|�ύX�}��     TO �g�A�}�ԍ쐬�|�}��.
007760     MOVE �A�N��Q�|�V�l����       TO �g�A�}�ԍ쐬�|�V�l����.
007761*
007762     MOVE �A���ҍ폜�|���Ҕԍ�     TO �g�A�}�ԍ쐬�|�����Ҕԍ�.
007763     MOVE �A���ҍ폜�|�}��         TO �g�A�}�ԍ쐬�|���}��.
007764*
007770     CALL   "HM012".
007780     CANCEL "HM012".
007790*
007822*================================================================*
007823 ��f�ҏ��e�����폜 SECTION.
007824* 
007830     MOVE �A���ҍ폜�|�{�p�a��N�� TO ��|�{�p�a��N��.
007840     MOVE �A���ҍ폜�|���҃R�[�h   TO ��|���҃R�[�h.
007850*
007860     READ ��f�ҏ��e
007870     NOT INVALID KEY
007880         DELETE ��f�ҏ��e
007890         INVALID KEY
007900             MOVE NC"��f" TO �t�@�C�����v
007910             PERFORM �G���[�\���c
007920         END-DELETE
007930     END-READ.
007931*
007934*================================================================*
007935 ��f�ҏ��[���č\�z SECTION.
007936* 
007937* JISINJ,JUSINJ2��ZERO���R�[�h�č\�z
007939*
007943     OPEN INPUT ����f�ҏ��e.
007944          MOVE NC"����" TO �t�@�C�����v.
007945          PERFORM �I�[�v���`�F�b�N.
007958*
007971     MOVE �A���ҍ폜�|���҃R�[�h TO ���҃R�[�h�v.
007972     MOVE �A���ҍ폜�|���҃R�[�h TO ����|���҃R�[�h.
007973     MOVE 99999                  TO ����|�{�p�a��N��.
007974     START ����f�ҏ��e KEY IS < ����|���҃R�[�h
007975                                   ����|�{�p�a��N��
007976                                   REVERSED
007977     END-START.
007978     IF ��ԃL�[ = "00"
007979                 MOVE SPACE TO �I���t���O�Q
007980                 PERFORM ����f�ҏ��e�Ǎ�
007981                 IF (����|���҃R�[�h = ���҃R�[�h�v) AND
007982                    (����|�{�p�a��N�� NOT = ZERO)  AND ( �I���t���O�Q = SPACE )
007983                     MOVE ����|���R�[�h TO ��|���R�[�h
007984                     MOVE ����|�{�p�a��N�� TO ��|�{�p�a��N���m
007985                     MOVE ����|�{�p�a��N�� TO �{�p�a��N���v
007986                     MOVE ZERO  TO ��|�{�p�a��
007987                     MOVE ZERO  TO ��|�{�p�N  
007988                     MOVE ZERO  TO ��|�{�p��  
007989                     MOVE ZERO  TO ��|�����a��
007990                     MOVE ZERO  TO ��|�����N  
007991                     MOVE ZERO  TO ��|������  
007992* ����������
007993                     MOVE ZERO           TO ��|�p���敪
007994                     MOVE ZERO           TO ��|�����敪
007995                     MOVE ZERO           TO ��|���Z����敪
007996                     MOVE ZERO           TO ��|���Z����敪����
007997                     MOVE ZERO           TO ��|�V�l���S��
007998                     MOVE ZERO           TO ��|�V�l���S�z�݌v
007999                     MOVE ZERO           TO ��|�V�l�ŏI���S��
008000                     MOVE ZERO           TO ��|�V�l���S�z�[��
008001                     MOVE ZERO           TO ��|�V�l���S����
008002                     MOVE SPACE          TO ��|�}�ԍ쐬���}��
008003                     MOVE ZERO           TO ��|�͂�敪
008004                     MOVE ZERO           TO ��|����܋敪
008005                     MOVE ZERO           TO ��|�_���L�敪
008006                     MOVE ZERO           TO ��|�I���L�敪
008007                     MOVE ZERO           TO ��|�}�b�T�[�W�L�敪
008008                     MOVE ZERO           TO ��|���p�敪
008009                     INITIALIZE             ��|�ی��ύX���
008010                     MOVE ZERO           TO ��|�������r���J�n��
008011*
008012                     REWRITE ��|���R�[�h
008013	                   IF ��ԃL�[ NOT = "00"
008014                        MOVE NC"��f" TO �t�@�C�����v
008015                        PERFORM �G���[�\��
008020                     END-IF
008021*
008022                     PERFORM ��f�ҏ��Q�e�č\�z����
008023*
008024                 ELSE
008025                     IF (����|���҃R�[�h = ���҃R�[�h�v) AND (����|�{�p�a��N�� = ZERO) AND ( �I���t���O�Q = SPACE )
008026                          MOVE ���҃R�[�h�v  TO ��|���҃R�[�h
008027                          MOVE ZERO          TO ��|�{�p�a��N��
008028                          DELETE ��f�ҏ��e
008029                          INVALID KEY
008030                              MOVE NC"��f" TO �t�@�C�����v
008031                              PERFORM �G���[�\���c
008032                          END-DELETE
008033*
008034                          MOVE ���҃R�[�h�v  TO ��Q�|���҃R�[�h
008035                          MOVE ZERO          TO ��Q�|�{�p�a��N��
008036                          DELETE ��f�ҏ��Q�e
008037                          INVALID KEY
008038                              MOVE NC"��f�Q" TO �t�@�C�����v
008039                              PERFORM �G���[�\���c
008040                          END-DELETE
008041                     END-IF
008042                 END-IF
008061     END-IF.
008062*
008063     CLOSE ����f�ҏ��e.
008064*
008065*================================================================*
008066 ��f�ҏ��Q�e�č\�z���� SECTION.
008067*
008068     MOVE �{�p�a��N���v   TO ��Q�|�{�p�a��N��.
008069     MOVE ���҃R�[�h�v     TO ��Q�|���҃R�[�h.
008070     READ ��f�ҏ��Q�e
008071     NOT INVALID KEY
008072* ����������
008073                 MOVE ZERO           TO ��Q�|�{�p�a��
008074                 MOVE ZERO           TO ��Q�|�{�p�N  
008075                 MOVE ZERO           TO ��Q�|�{�p��  
008076                 MOVE ZERO           TO ��Q�|�����a��
008077                 MOVE ZERO           TO ��Q�|�����N  
008078                 MOVE ZERO           TO ��Q�|������  
008079                 MOVE ZERO           TO ��Q�|�����敪
008080                 MOVE ZERO           TO ��Q�|�����Ώۋ敪
008081                 MOVE ZERO           TO ��Q�|���������a��
008082                 MOVE ZERO           TO ��Q�|���������N  
008083                 MOVE ZERO           TO ��Q�|����������  
008084                 MOVE ZERO           TO ��Q�|���������敪
008085                 MOVE ZERO           TO ��Q�|���������Ώۋ敪
008086                 MOVE ZERO           TO ��Q�|���@���@�敪
008087                 MOVE ZERO           TO ��Q�|�Љ�Ҋ��Ҕԍ�
008088                 MOVE ZERO           TO ��Q�|�_���{�̏��ҕ����敪
008089                 MOVE ZERO           TO ��Q�|�_���{�̂܂Ƃߋ敪
008090                 MOVE ZERO           TO ��Q�|�_���������ҕ����敪
008091                 MOVE ZERO           TO ��Q�|�_���������Z����Ώۋ敪
008092                 MOVE ZERO           TO ��Q�|�_���������Z����\����Ώۋ敪
008093                 MOVE ZERO           TO ��Q�|�_���{�̎}�Ԃ܂Ƃߋ敪
008094*
008095         REWRITE ��Q�|���R�[�h
008096         INVALID KEY
008097            MOVE NC"��f�Q" TO �t�@�C�����v
008098            PERFORM �G���[�\��
008099         END-REWRITE
008100     END-READ.
008101*
008102*================================================================*
008103 ����f�ҏ��e�Ǎ� SECTION.
008104*
008105     READ ����f�ҏ��e NEXT
008106     AT END
008107         MOVE "YES" TO �I���t���O�Q
008108     END-READ.
008109*
008110*================================================================*
008111*================================================================*
008112*================================================================*
008113 ��f�ҏ��e�S�폜 SECTION.
008114*
008115     MOVE ZERO                   TO ��|�{�p�a��N��.
008116     MOVE �A���ҍ폜�|���҃R�[�h TO ��|���҃R�[�h.
008117*
008118     START ��f�ҏ��e KEY IS >= ��|���҃R�[�h
008119                                  ��|�{�p�a��N��
008120     END-START.
008121*
008122     IF ��ԃL�[ = "00"
008123         MOVE SPACE  TO �I���t���O
008124         PERFORM ��f�ҏ��e�Ǎ�
008125         PERFORM UNTIL ( �A���ҍ폜�|���҃R�[�h NOT = ��|���҃R�[�h ) OR
008126                       ( �I���t���O = "YES" )
008127             DELETE ��f�ҏ��e
008128             INVALID KEY
008129                 MOVE NC"��f" TO �t�@�C�����v
008130                 PERFORM �G���[�\���c
008131             END-DELETE
008140             PERFORM ��f�ҏ��e�Ǎ�
008150         END-PERFORM
008160     END-IF.
008170*================================================================*
008180 ��f�ҏ��e�ύX SECTION.
008190*
008200     MOVE ZERO                   TO ��|�{�p�a��N��.
008210     MOVE �A���ҍ폜�|���҃R�[�h TO ��|���҃R�[�h.
008220*
008230     START ��f�ҏ��e KEY IS >= ��|���҃R�[�h
008240                                  ��|�{�p�a��N��
008250     END-START.
008260*
008270     IF ��ԃL�[ = "00"
008280         MOVE SPACE  TO �I���t���O
008290         PERFORM ��f�ҏ��e�Ǎ�
008300         PERFORM UNTIL ( �A���ҍ폜�|���҃R�[�h NOT = ��|���҃R�[�h ) OR
008310                       ( �I���t���O = "YES" )
008320             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO  ��|���҃R�[�h
008330             WRITE ��|���R�[�h
008340             INVALID KEY
008350                 MOVE NC"��f" TO �t�@�C�����v
008360                 PERFORM �G���[�\��
008370             END-WRITE
008380             MOVE  �A���ҍ폜�|���҃R�[�h TO  ��|���҃R�[�h
008390             DELETE ��f�ҏ��e
008400             INVALID KEY
008410                 MOVE NC"��f" TO �t�@�C�����v
008420                 PERFORM �G���[�\���c
008430             END-DELETE
008440             PERFORM ��f�ҏ��e�Ǎ�
008450         END-PERFORM
008460     END-IF.
008470*
008480*================================================================*
008490 ��f�ҏ��e�}�ԍ쐬 SECTION.
008500*
008510*/���̎�f�ҏ���ǂ݁A�w��}�ԂɃR�s�[�B�ύX�I�����Z�b�g
008520     MOVE �A���ҍ폜�|�{�p�a��N�� TO ��|�{�p�a��N��.
008530     MOVE �A���ҍ폜�|���҃R�[�h   TO ��|���҃R�[�h.
008540*
008550     READ ��f�ҏ��e
008551     INVALID KEY
008552         MOVE NC"��f" TO �t�@�C�����v
008553         PERFORM �G���[�\���q
008560     NOT INVALID KEY
008561         MOVE ��|�ی����� TO �ی����ނv
008564*
008570         MOVE �A���ҍ폜�|�ύX�}��       TO ��|�}��
008580         MOVE �A���Ҏ}�ԁ|�ύX��         TO ��|�ύX�I����
008590         MOVE �A���Ҏ}�ԁ|�ی��ύX�敪   TO ��|�ی��ύX�敪
008600         WRITE ��|���R�[�h
008610         INVALID KEY
008620             MOVE NC"��f" TO �t�@�C�����v
008630             PERFORM �G���[�\��
008640         END-WRITE
008650     END-READ.
008651*
008660*/�w��}�ԃf�[�^�̂O�O�̃f�[�^�쐬�B�i�����ۂ̂݁j
008662* 2:�J�ЁA3:�����ӁA4:���ہA5:����́A���r���ύX�Ŏ}�ԃ��R�[�h�����Ȃ��B
008663     IF �ی����ނv = 1
008664*
008670       MOVE ZERO     TO ��|�{�p�a��
008680       MOVE ZERO     TO ��|�{�p�N
008690       MOVE ZERO     TO ��|�{�p��
008700       MOVE �A���ҍ폜�|�ύX���҃R�[�h TO ��|���҃R�[�h
008710       READ ��f�ҏ��e
008720       INVALID KEY
008730         MOVE SPACE TO ��|���R�[�h
008740         INITIALIZE    ��|���R�[�h
008750         MOVE �A���ҍ폜�|�ύX���҃R�[�h TO ��|���҃R�[�h
008760         MOVE �A���ҍ폜�|�{�p�a��N��   TO ��|�{�p�a��N��
008770         READ ��f�ҏ��e
008780         NOT INVALID KEY
008790             MOVE ZERO  TO ��|�{�p�a��
008800             MOVE ZERO  TO ��|�{�p�N  
008810             MOVE ZERO  TO ��|�{�p��  
008820             MOVE �A���ҍ폜�|�{�p�a��N�� TO ��|�{�p�a��N���m
008830             MOVE ZERO  TO ��|�����a��
008840             MOVE ZERO  TO ��|�����N  
008850             MOVE ZERO  TO ��|������  
008852* ����������
008879             MOVE ZERO           TO ��|�p���敪
008880             MOVE ZERO           TO ��|�����敪
008881             MOVE ZERO           TO ��|���Z����敪
008882             MOVE ZERO           TO ��|���Z����敪����
008883             MOVE ZERO           TO ��|�V�l���S��
008884             MOVE ZERO           TO ��|�V�l���S�z�݌v
008885             MOVE ZERO           TO ��|�V�l�ŏI���S��
008886             MOVE ZERO           TO ��|�V�l���S�z�[��
008887             MOVE ZERO           TO ��|�V�l���S����
008888             MOVE SPACE          TO ��|�}�ԍ쐬���}��
008889             MOVE ZERO           TO ��|�͂�敪
008890             MOVE ZERO           TO ��|����܋敪
008891             MOVE ZERO           TO ��|�_���L�敪
008892             MOVE ZERO           TO ��|�I���L�敪
008893             MOVE ZERO           TO ��|�}�b�T�[�W�L�敪
008894             MOVE ZERO           TO ��|���p�敪
008895             INITIALIZE             ��|�ی��ύX���
008896             MOVE ZERO           TO ��|�������r���J�n��
008897*
008898             WRITE ��|���R�[�h
008899             INVALID KEY
008900                 MOVE NC"��f" TO �t�@�C�����v
008901                 PERFORM �G���[�\��
008902             END-WRITE
008910         END-READ
008920       END-READ
008921     END-IF.
008922**
008930*/�}�ԂȂ��̕ύX�J�n�����Z�b�g
008940     MOVE �A���ҍ폜�|�{�p�a��N�� TO ��|�{�p�a��N��.
008950     MOVE �A���ҍ폜�|���҃R�[�h   TO ��|���҃R�[�h.
008960*
008970     READ ��f�ҏ��e
008980     NOT INVALID KEY
008990         MOVE �A���Ҏ}�ԁ|�ύX��         TO ��|�ύX�J�n��
009000         MOVE �A���Ҏ}�ԁ|�ی��ύX�敪   TO ��|�ی��ύX�敪
009010         MOVE �A���ҍ폜�|�ύX�}��       TO ��|�}�ԍ쐬���}��
009020         REWRITE ��|���R�[�h
009030         INVALID KEY
009040             MOVE NC"��f" TO �t�@�C�����v
009050             PERFORM �G���[�\��
009060         END-REWRITE
009070     END-READ.
009080*
009090*================================================================*
009100 ��f�ҏ��e�Ǎ� SECTION.
009110*
009120     READ ��f�ҏ��e NEXT
009130     AT END
009140         MOVE "YES" TO �I���t���O
009150     END-READ.
009160*
009170*================================================================*
009180 ���������e�S�폜 SECTION.
009190*
009200     MOVE 01           TO �����|�敪�R�[�h.
009210     MOVE ZERO         TO �����|���Ҕԍ�.
009220     MOVE 01           TO �����|���������A��.
009230*
009240     START ���������e KEY IS >= �����|�敪�R�[�h
009250                                �����|���������R�[�h
009260     END-START.
009270     IF ��ԃL�[ = "00"
009280         MOVE SPACE TO �I���t���O
009290         PERFORM ���������e�Ǎ�
009300         PERFORM UNTIL  �I���t���O NOT = SPACE 
009310             IF �A���ҍ폜�|���Ҕԍ� = �����|���Ҕԍ�
009320                 DELETE ���������e
009330                 INVALID KEY
009340                     MOVE NC"����" TO �t�@�C�����v
009350                     PERFORM �G���[�\���c
009360                 END-DELETE
009370             END-IF
009380             PERFORM ���������e�Ǎ�
009390         END-PERFORM
009400     END-IF.
009410*================================================================*
009420 ���������R���o�[�g SECTION.
009430*
009440     IF ( �A���ҍ폜�|�}�� = SPACE ) AND ( �A���ҍ폜�|�ύX�}�� = SPACE )
009450         MOVE 01           TO �����|�敪�R�[�h
009460         MOVE ZERO         TO �����|���Ҕԍ�
009470         MOVE 01           TO �����|���������A��
009480         MOVE SPACE        TO �I���t���O
009490*
009500         START ���������e KEY IS >= �����|�敪�R�[�h
009510                                    �����|���������R�[�h
009520         END-START
009530         IF ��ԃL�[ = "00"
009540             PERFORM ���������e�Ǎ�
009550             PERFORM UNTIL ( �I���t���O = "YES" )
009560                 IF �A���ҍ폜�|���Ҕԍ� = �����|���Ҕԍ�
009570                     MOVE �A���ҍ폜�|�ύX���Ҕԍ�  TO �����|���Ҕԍ�
009580                     WRITE �����|���R�[�h
009590                     INVALID KEY
009600                         MOVE NC"����" TO �t�@�C�����v
009610                         PERFORM �G���[�\��
009620                     END-WRITE
009630                     MOVE �A���ҍ폜�|���Ҕԍ�  TO �����|���Ҕԍ�
009640                     DELETE ���������e
009650                     INVALID KEY
009660                         MOVE NC"����" TO �t�@�C�����v
009670                         PERFORM �G���[�\���c
009680                     END-DELETE
009690                 END-IF
009700                 PERFORM ���������e�Ǎ�
009710             END-PERFORM
009720         END-IF
009730     END-IF.
009740*
009750*================================================================*
009760 ���������e�Ǎ� SECTION.
009770*
009780     READ ���������e NEXT
009790     AT END
009800         MOVE "YES" TO �I���t���O
009810     END-READ.
009820*================================================================*
009830 �����f�[�^�e�����폜 SECTION.
009840*
009850     MOVE �A���ҍ폜�|�{�p�a��N�� TO ���|�{�p�a��N��.
009860     MOVE �A���ҍ폜�|���҃R�[�h   TO ���|���҃R�[�h.
009870*
009880     READ �����f�[�^�e
009890     NOT INVALID KEY
009900         DELETE �����f�[�^�e
009910         INVALID KEY
009920             MOVE NC"����" TO �t�@�C�����v
009930             PERFORM �G���[�\���c
009940         END-DELETE
009950     END-READ.
009960*================================================================*
009970 �����f�[�^�e�S�폜 SECTION.
009980*
009990     MOVE ZERO                   TO ���|�{�p�a��N��.
010000     MOVE �A���ҍ폜�|���҃R�[�h TO ���|���҃R�[�h.
010010*
010020     START �����f�[�^�e KEY IS >= ���|���҃R�[�h
010030                                  ���|�{�p�a��N��
010040     END-START.
010050     IF ��ԃL�[ = "00"
010060         MOVE SPACE TO �I���t���O
010070         PERFORM �����f�[�^�e�Ǎ�
010080         PERFORM UNTIL ( �A���ҍ폜�|���҃R�[�h NOT = ���|���҃R�[�h ) OR
010090                       ( �I���t���O = "YES" )
010100             DELETE �����f�[�^�e
010110             INVALID KEY
010120                 MOVE NC"����" TO �t�@�C�����v
010130                 PERFORM �G���[�\���c
010140             END-DELETE
010150             PERFORM �����f�[�^�e�Ǎ�
010160         END-PERFORM
010170     END-IF.
010180*================================================================*
010190 �����f�[�^�e�ύX SECTION.
010200*
010210     MOVE ZERO                   TO ���|�{�p�a��N��.
010220     MOVE �A���ҍ폜�|���҃R�[�h TO ���|���҃R�[�h.
010230*
010240     START �����f�[�^�e KEY IS >= ���|���҃R�[�h
010250                                  ���|�{�p�a��N��
010260     END-START.
010270     IF ��ԃL�[ = "00"
010280         MOVE SPACE TO �I���t���O
010290         PERFORM �����f�[�^�e�Ǎ�
010300         PERFORM UNTIL ( �A���ҍ폜�|���҃R�[�h NOT = ���|���҃R�[�h ) OR
010310                       ( �I���t���O = "YES" )
010320             MOVE �A���ҍ폜�|�ύX���҃R�[�h TO ���|���҃R�[�h
010330             PERFORM VARYING �J�E���^ FROM 1 BY 1
010340                           UNTIL ( �J�E���^ > 8 )
010350                 IF ���|�������Ҕԍ�(�J�E���^) = �A���ҍ폜�|���Ҕԍ�
010360                     MOVE �A���ҍ폜�|�ύX���Ҕԍ� TO ���|�������Ҕԍ�(�J�E���^)
010370                 END-IF
010380             END-PERFORM
010390             WRITE ���|���R�[�h
010400             INVALID KEY
010410                 MOVE NC"����" TO �t�@�C�����v
010420                 PERFORM �G���[�\��
010430             END-WRITE
010440             MOVE �A���ҍ폜�|���҃R�[�h TO ���|���҃R�[�h
010450             DELETE �����f�[�^�e
010460             INVALID KEY
010470                 MOVE NC"����" TO �t�@�C�����v
010480                 PERFORM �G���[�\���c
010490             END-DELETE
010500             PERFORM �����f�[�^�e�Ǎ�
010510         END-PERFORM
010520     END-IF.
010530*
010540*================================================================*
010550 �����f�[�^�e�}�ԍ쐬 SECTION.
010560*
010570     MOVE �A���ҍ폜�|�{�p�a��N�� TO ���|�{�p�a��N��.
010580     MOVE �A���ҍ폜�|���҃R�[�h   TO ���|���҃R�[�h.
010590*
010600     READ �����f�[�^�e
010610     INVALID KEY
010620         MOVE NC"����" TO �t�@�C�����v
010630         PERFORM �G���[�\���q
010640     NOT INVALID KEY
010650*      /*�}�ԗL�背�R�[�h�̍쐬
010660         MOVE �A���ҍ폜�|�ύX�}�� TO ���|�}��
010670         WRITE ���|���R�[�h
010680         INVALID KEY
010690             MOVE NC"����" TO �t�@�C�����v
010700             PERFORM �G���[�\��
010710         END-WRITE
010720*      /*�}�ԍ쐬���Ɍ��ԍ��̕������R�[�h�̊J�n�f�Ó��蓮�敪���P�ɂ���B
010730         MOVE �A���ҍ폜�|�}�� TO ���|�}��
010740         MOVE 1     TO ���|�J�n�f�Ó��蓮�敪
010750         REWRITE ���|���R�[�h
010760         INVALID KEY
010770             MOVE NC"����" TO �t�@�C�����v
010780             PERFORM �G���[�\��
010790         END-REWRITE
010800     END-READ.
010810*
010820*================================================================*
010830 �����f�[�^�e�Ǎ� SECTION.
010840*
010850     READ �����f�[�^�e NEXT
010860     AT END
010870         MOVE "YES" TO �I���t���O
010880     END-READ.
010890*================================================================*
010900 ��t�f�[�^�e�����폜 SECTION.
010910*
010920     MOVE �A���ҍ폜�|�{�p�a��N�� TO ��t�|�{�p�a��N��.
010930     MOVE ZERO                     TO ��t�|�{�p��.
010940     MOVE �A���ҍ폜�|���҃R�[�h   TO ��t�|���҃R�[�h.
010950     MOVE ZERO                     TO ��t�|��t����.
010960*
010970     START ��t�f�[�^�e KEY IS >= ��t�|���҃R�[�h
010980                                  ��t�|�{�p�a��N����
010990                                  ��t�|��t����
011000     END-START.
011010*
011020     IF ��ԃL�[ = "00"
011030         MOVE SPACE TO �I���t���O
011040         PERFORM ��t�f�[�^�e�Ǎ�
011050         PERFORM UNTIL ( �I���t���O = "YES" ) OR
011060                       ( �A���ҍ폜�|���҃R�[�h   NOT = ��t�|���҃R�[�h ) OR
011070                       ( �A���ҍ폜�|�{�p�a��N�� NOT = ��t�|�{�p�a��N�� )
011080             DELETE ��t�f�[�^�e
011090             INVALID KEY
011100                 MOVE NC"��t" TO �t�@�C�����v
011110                 PERFORM �G���[�\���c
011120             END-DELETE
011130             PERFORM ��t�f�[�^�e�Ǎ�
011140         END-PERFORM
011150     END-IF.
011160*================================================================*
011170 ��t�f�[�^�e�S�폜 SECTION.
011180*
011190     MOVE ZERO                   TO  ��t�|�{�p�a��N����.
011200     MOVE �A���ҍ폜�|���҃R�[�h TO  ��t�|���҃R�[�h.
011210     MOVE ZERO                   TO  ��t�|��t����.
011220*
011230     START ��t�f�[�^�e KEY IS >= ��t�|���҃R�[�h
011240                                  ��t�|�{�p�a��N����
011250                                  ��t�|��t����
011260     END-START.
011270*
011280     IF ��ԃL�[ = "00"
011290         MOVE SPACE TO �I���t���O
011300         PERFORM ��t�f�[�^�e�Ǎ�
011310         PERFORM UNTIL ( �I���t���O = "YES" ) OR
011320                       ( �A���ҍ폜�|���҃R�[�h   NOT = ��t�|���҃R�[�h )
011330             DELETE ��t�f�[�^�e
011340             INVALID KEY
011350                 MOVE NC"��t" TO �t�@�C�����v
011360                 PERFORM �G���[�\���c
011370             END-DELETE
011380             PERFORM ��t�f�[�^�e�Ǎ�
011390         END-PERFORM
011400     END-IF.
011410*================================================================*
011420 ��t�f�[�^�e�ύX SECTION.
011430*
011440     MOVE ZERO                   TO  ��t�|�{�p�a��N����.
011450     MOVE �A���ҍ폜�|���҃R�[�h TO  ��t�|���҃R�[�h.
011460     MOVE ZERO                   TO  ��t�|��t����.
011470*
011480     START ��t�f�[�^�e KEY IS >= ��t�|���҃R�[�h
011490                                  ��t�|�{�p�a��N����
011500                                  ��t�|��t����
011510     END-START.
011520*
011530     IF ��ԃL�[ = "00"
011540         MOVE SPACE TO �I���t���O
011550         PERFORM ��t�f�[�^�e�Ǎ�
011560         PERFORM UNTIL ( �I���t���O = "YES" ) OR
011570                       ( �A���ҍ폜�|���҃R�[�h   NOT = ��t�|���҃R�[�h )
011580             MOVE �A���ҍ폜�|�ύX���҃R�[�h TO ��t�|���҃R�[�h
011590             COMPUTE ��t�|�A��  = ��t�|�A�� + 0.1
011600             WRITE ��t�|���R�[�h
011610             INVALID KEY
011620                 MOVE NC"��t" TO �t�@�C�����v
011630                 PERFORM �G���[�\��
011640             END-WRITE
011650             MOVE �A���ҍ폜�|���҃R�[�h TO ��t�|���҃R�[�h
011660             DELETE ��t�f�[�^�e
011670             INVALID KEY
011680                 MOVE NC"��t" TO �t�@�C�����v
011690                 PERFORM �G���[�\���c
011700             END-DELETE
011710*
011720             PERFORM ��t�f�[�^�e�Ǎ�
011730         END-PERFORM
011740     END-IF.
011750*
011760*================================================================*
011770 ��t�f�[�^�e�}�ԍ쐬 SECTION.
011780*
011790     MOVE �A���ҍ폜�|�{�p�a��N�� TO  ��t�|�{�p�a��N��.
011800     MOVE 1                        TO  ��t�|�{�p��.
011810     MOVE �A���ҍ폜�|���҃R�[�h   TO  ��t�|���҃R�[�h.
011820     MOVE ZERO                     TO  ��t�|��t����.
011830*
011840     START ��t�f�[�^�e KEY IS >= ��t�|���҃R�[�h
011850                                  ��t�|�{�p�a��N����
011860                                  ��t�|��t����
011870     END-START.
011880*
011890     IF ��ԃL�[ = "00"
011900         MOVE SPACE TO �I���t���O
011910         PERFORM ��t�f�[�^�e�Ǎ�
011920*/�ύX���ȍ~�̓R���o�[�g���Ȃ�
011930         PERFORM UNTIL ( �I���t���O         NOT = SPACE ) OR 
011940                       ( ��t�|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h ) OR
011950                       ( ��t�|�{�p�a��N�� NOT = �A���ҍ폜�|�{�p�a��N�� ) OR
011960                       ( ��t�|�{�p��          >= �A���Ҏ}�ԁ|�ύX��    )
011970             MOVE �A���ҍ폜�|�ύX�}��   TO ��t�|�}��
011980             COMPUTE ��t�|�A�� = ��t�|�A�� + 0.1
011990             WRITE ��t�|���R�[�h
012000             INVALID KEY
012010                 MOVE NC"��t" TO �t�@�C�����v
012020                 PERFORM �G���[�\��
012030             END-WRITE
012040*
012050             MOVE �A���ҍ폜�|�}��  TO ��t�|�}��
012060             DELETE ��t�f�[�^�e
012070             INVALID KEY
012080                 MOVE NC"��t" TO �t�@�C�����v
012090                 PERFORM �G���[�\���c
012100             END-DELETE
012110*
012120             PERFORM ��t�f�[�^�e�Ǎ�
012130         END-PERFORM
012140     END-IF.
012150*
012160*================================================================*
012170 ��t�f�[�^�e�Ǎ� SECTION.
012180*
012190     READ ��t�f�[�^�e NEXT
012200     AT END
012210         MOVE "YES" TO �I���t���O
012220     END-READ.
012230*================================================================*
012240 �{�p�L�^�����폜 SECTION.
012250*
012260     MOVE �A���ҍ폜�|�{�p�a��N�� TO  �{�L�|�{�p�a��N��.
012270     MOVE ZERO                     TO  �{�L�|�{�p��.  
012280     MOVE �A���ҍ폜�|���҃R�[�h   TO  �{�L�|���҃R�[�h.
012290*
012300     START �{�p�L�^�e KEY IS >= �{�L�|���҃R�[�h
012310                                �{�L�|�{�p�a��N����
012320     END-START.
012330*
012340     IF ��ԃL�[ = "00"
012350         MOVE SPACE TO �I���t���O
012360         PERFORM �{�p�L�^�e�Ǎ�
012370         PERFORM UNTIL ( �I���t���O = "YES" ) OR
012380                       ( �A���ҍ폜�|���҃R�[�h   NOT = �{�L�|���҃R�[�h ) OR
012390                       ( �A���ҍ폜�|�{�p�a��N�� NOT = �{�L�|�{�p�a��N�� )
012400             DELETE �{�p�L�^�e
012410             INVALID KEY
012420                 MOVE NC"�{�L" TO �t�@�C�����v
012430                 PERFORM �G���[�\���c
012440             END-DELETE
012450             PERFORM �{�p�L�^�e�Ǎ�
012460         END-PERFORM
012470     END-IF.
012480*================================================================*
012490 �{�p�L�^�S�폜 SECTION.
012500*
012510     MOVE ZERO                   TO  �{�L�|�{�p�a��N����.
012520     MOVE �A���ҍ폜�|���҃R�[�h TO  �{�L�|���҃R�[�h.
012530*
012540     START �{�p�L�^�e KEY IS >= �{�L�|���҃R�[�h
012550                                �{�L�|�{�p�a��N����
012560     END-START.
012570*
012580     IF ��ԃL�[ = "00"
012590         MOVE SPACE TO �I���t���O
012600         PERFORM �{�p�L�^�e�Ǎ�
012610         PERFORM UNTIL ( �I���t���O = "YES" ) OR
012620                       ( �A���ҍ폜�|���҃R�[�h   NOT = �{�L�|���҃R�[�h )
012630             DELETE �{�p�L�^�e
012640             INVALID KEY
012650                 MOVE NC"�{�L" TO �t�@�C�����v
012660                 PERFORM �G���[�\���c
012670             END-DELETE
012680             PERFORM �{�p�L�^�e�Ǎ�
012690         END-PERFORM
012700     END-IF.
012710*================================================================*
012720 �{�p�L�^�ύX SECTION.
012730*
012740     MOVE ZERO                   TO  �{�L�|�{�p�a��N����.
012750     MOVE �A���ҍ폜�|���҃R�[�h TO  �{�L�|���҃R�[�h.
012760*
012770     START �{�p�L�^�e KEY IS >= �{�L�|���҃R�[�h
012780                                �{�L�|�{�p�a��N����
012790     END-START.
012800*
012810     IF ��ԃL�[ = "00"
012820         MOVE SPACE TO �I���t���O
012830         PERFORM �{�p�L�^�e�Ǎ�
012840         PERFORM UNTIL ( �I���t���O = "YES" ) OR
012850                       ( �A���ҍ폜�|���҃R�[�h   NOT = �{�L�|���҃R�[�h )
012860             MOVE �A���ҍ폜�|�ύX���҃R�[�h TO �{�L�|���҃R�[�h
012870             WRITE �{�L�|���R�[�h
012880             INVALID KEY
012890                 MOVE NC"�{�L" TO �t�@�C�����v
012900                 PERFORM �G���[�\��
012910             END-WRITE
012920             MOVE �A���ҍ폜�|���҃R�[�h TO �{�L�|���҃R�[�h
012930             DELETE �{�p�L�^�e
012940             INVALID KEY
012950                 MOVE NC"�{�L" TO �t�@�C�����v
012960                 PERFORM �G���[�\���c
012970             END-DELETE
012980*
012990             PERFORM �{�p�L�^�e�Ǎ�
013000         END-PERFORM
013010     END-IF.
013020*
013030*================================================================*
013040 �{�p�L�^�}�ԍ쐬 SECTION.
013050*
013060     MOVE �A���ҍ폜�|�{�p�a��N�� TO  �{�L�|�{�p�a��N��.
013070     MOVE 1                        TO  �{�L�|�{�p��.
013080     MOVE �A���ҍ폜�|���҃R�[�h   TO  �{�L�|���҃R�[�h.
013090*
013100     START �{�p�L�^�e KEY IS >= �{�L�|���҃R�[�h
013110                                �{�L�|�{�p�a��N����
013120     END-START.
013130*
013140     IF ��ԃL�[ = "00"
013150         MOVE SPACE TO �I���t���O
013160         PERFORM �{�p�L�^�e�Ǎ�
013170*/�ύX���ȍ~�̓R���o�[�g���Ȃ�
013180         PERFORM UNTIL ( �I���t���O         NOT = SPACE ) OR 
013190                       ( �{�L�|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h ) OR
013200                       ( �{�L�|�{�p�a��N�� NOT = �A���ҍ폜�|�{�p�a��N�� ) OR
013210                       ( �{�L�|�{�p��          >= �A���Ҏ}�ԁ|�ύX��    )
013220             MOVE �A���ҍ폜�|�ύX�}��   TO �{�L�|�}��
013230             WRITE �{�L�|���R�[�h
013240             INVALID KEY
013250                 MOVE NC"�{�L" TO �t�@�C�����v
013260                 PERFORM �G���[�\��
013270             END-WRITE
013280*
013290             MOVE �A���ҍ폜�|�}��  TO �{�L�|�}��
013300             DELETE �{�p�L�^�e
013310             INVALID KEY
013320                 MOVE NC"�{�L" TO �t�@�C�����v
013330                 PERFORM �G���[�\���c
013340             END-DELETE
013350*
013360             PERFORM �{�p�L�^�e�Ǎ�
013370*
013380         END-PERFORM
013390     END-IF.
013400*
013410*================================================================*
013420 �{�p�L�^�e�Ǎ� SECTION.
013430*
013440     READ �{�p�L�^�e NEXT
013450     AT END
013460         MOVE "YES" TO �I���t���O
013470     END-READ.
013480*================================================================*
013490 ��v�f�[�^�e�����폜 SECTION.
013500*
013510     MOVE �A���ҍ폜�|�{�p�a��N�� TO  ��|�{�p�a��N��.
013520     MOVE ZERO                     TO  ��|�{�p��.
013530     MOVE �A���ҍ폜�|���҃R�[�h   TO  ��|���҃R�[�h.
013540*
013550     START ��v�f�[�^�e KEY IS >= ��|���҃R�[�h
013560                                  ��|�{�p�a��N����
013570     END-START.
013580*
013590     IF ��ԃL�[ = "00"
013600         MOVE SPACE TO �I���t���O
013610         PERFORM ��v�f�[�^�e�Ǎ�
013620         PERFORM UNTIL ( �I���t���O = "YES" ) OR
013630                       ( �A���ҍ폜�|���҃R�[�h   NOT = ��|���҃R�[�h ) OR
013640                       ( �A���ҍ폜�|�{�p�a��N�� NOT = ��|�{�p�a��N�� )
013650             DELETE ��v�f�[�^�e
013660             INVALID KEY
013670                 MOVE NC"��v" TO �t�@�C�����v
013680                 PERFORM �G���[�\���c
013690             END-DELETE
013700             PERFORM ��v�f�[�^�e�Ǎ�
013710         END-PERFORM
013720     END-IF.
013730*
013740*================================================================*
013750 ��v�f�[�^�e�S�폜 SECTION.
013760*
013770     MOVE ZERO                   TO  ��|�{�p�a��N����.
013780     MOVE �A���ҍ폜�|���҃R�[�h TO  ��|���҃R�[�h.
013790*
013800     START ��v�f�[�^�e KEY IS >= ��|���҃R�[�h
013810                                  ��|�{�p�a��N����
013820     END-START.
013830*
013840     IF ��ԃL�[ = "00"
013850         MOVE SPACE TO �I���t���O
013860         PERFORM ��v�f�[�^�e�Ǎ�
013870         PERFORM UNTIL ( �I���t���O = "YES" ) OR
013880                       ( �A���ҍ폜�|���҃R�[�h   NOT = ��|���҃R�[�h )
013890             DELETE ��v�f�[�^�e
013900             INVALID KEY
013910                 MOVE NC"��v" TO �t�@�C�����v
013920                 PERFORM �G���[�\���c
013930             END-DELETE
013940             PERFORM ��v�f�[�^�e�Ǎ�
013950         END-PERFORM
013960     END-IF.
013970*
013980*================================================================*
013990 ��v�f�[�^�e�ύX SECTION.
014000*
014010     MOVE ZERO                   TO  ��|�{�p�a��N����.
014020     MOVE �A���ҍ폜�|���҃R�[�h TO  ��|���҃R�[�h.
014030*
014040     START ��v�f�[�^�e KEY IS >= ��|���҃R�[�h
014050                                  ��|�{�p�a��N����
014060     END-START.
014070*
014080     IF ��ԃL�[ = "00"
014090         MOVE SPACE TO �I���t���O
014100         PERFORM ��v�f�[�^�e�Ǎ�
014110         PERFORM UNTIL ( �I���t���O = "YES" ) OR
014120                       ( �A���ҍ폜�|���҃R�[�h   NOT = ��|���҃R�[�h )
014130             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO ��|���҃R�[�h 
014140             WRITE ��|���R�[�h
014150             INVALID KEY
014160                 MOVE NC"��v" TO �t�@�C�����v
014170                 PERFORM �G���[�\��
014180             END-WRITE
014190             MOVE  �A���ҍ폜�|���҃R�[�h TO ��|���҃R�[�h 
014200             DELETE ��v�f�[�^�e
014210             INVALID KEY
014220                 MOVE NC"��v" TO �t�@�C�����v
014230                 PERFORM �G���[�\���c
014240             END-DELETE
014250             PERFORM ��v�f�[�^�e�Ǎ�
014260         END-PERFORM
014270     END-IF.
014280*
014290*================================================================*
014300 ��v�f�[�^�e�}�ԍ쐬 SECTION.
014310*
014320     MOVE �A���ҍ폜�|�{�p�a��N�� TO  ��|�{�p�a��N��.
014330     MOVE 1                        TO  ��|�{�p��.
014340     MOVE �A���ҍ폜�|���҃R�[�h   TO  ��|���҃R�[�h.
014350*
014360     START ��v�f�[�^�e KEY IS >= ��|���҃R�[�h
014370                                  ��|�{�p�a��N����
014380     END-START.
014390*
014400     IF ��ԃL�[ = "00"
014410         MOVE SPACE TO �I���t���O
014420         PERFORM ��v�f�[�^�e�Ǎ�
014430*/�ύX���ȍ~�̓R���o�[�g���Ȃ�
014440         PERFORM UNTIL ( �I���t���O       NOT = SPACE ) OR 
014450                       ( ��|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h ) OR
014460                       ( ��|�{�p�a��N�� NOT = �A���ҍ폜�|�{�p�a��N�� ) OR
014470                       ( ��|�{�p��          >= �A���Ҏ}�ԁ|�ύX��    )
014480             MOVE �A���ҍ폜�|�ύX�}��   TO ��|�}��
014490             WRITE ��|���R�[�h
014500             INVALID KEY
014510                 MOVE NC"��" TO �t�@�C�����v
014520                 PERFORM �G���[�\��
014530             END-WRITE
014540*
014550             MOVE �A���ҍ폜�|�}��  TO ��|�}��
014560             DELETE ��v�f�[�^�e
014570             INVALID KEY
014580                 MOVE NC"��" TO �t�@�C�����v
014590                 PERFORM �G���[�\���c
014600             END-DELETE
014610*
014620             PERFORM ��v�f�[�^�e�Ǎ�
014630         END-PERFORM
014640     END-IF.
014650*
014660*================================================================*
014670 ��v�f�[�^�e�Ǎ� SECTION.
014680*
014690     READ ��v�f�[�^�e NEXT
014700     AT END
014710         MOVE "YES" TO �I���t���O
014720     END-READ.
014730*================================================================*
014740 �����p���҂e�����폜 SECTION.
014750*
014760     MOVE �A���ҍ폜�|�{�p�a��N�� TO  ���p�|�{�p�a��N��.
014770     MOVE �A���ҍ폜�|���҃R�[�h   TO  ���p�|���҃R�[�h.
014780*
014790     START �����p���҂e KEY IS >= ���p�|���҃R�[�h
014800                                  ���p�|�{�p�a��N��
014810     END-START.
014820*
014830     IF ��ԃL�[ = "00"
014840         MOVE SPACE TO �I���t���O
014850         PERFORM �����p���҂e�Ǎ�
014860         PERFORM UNTIL ( �I���t���O = "YES" ) OR
014870                       ( �A���ҍ폜�|���҃R�[�h   NOT = ���p�|���҃R�[�h ) OR
014880                       ( �A���ҍ폜�|�{�p�a��N�� NOT = ���p�|�{�p�a��N�� )
014890             DELETE �����p���҂e
014900             INVALID KEY
014910                 MOVE NC"���p" TO �t�@�C�����v
014920                 PERFORM �G���[�\���c
014930             END-DELETE
014940             PERFORM �����p���҂e�Ǎ�
014950         END-PERFORM
014960     END-IF.
014970*================================================================*
014980 �����p���҂e�S�폜 SECTION.
014990*
015000     MOVE ZERO                   TO  ���p�|�{�p�a��N��.
015010     MOVE �A���ҍ폜�|���҃R�[�h TO  ���p�|���҃R�[�h.
015020*
015030     START �����p���҂e KEY IS >= ���p�|���҃R�[�h
015040                                  ���p�|�{�p�a��N��
015050     END-START.
015060*
015070     IF ��ԃL�[ = "00"
015080         MOVE SPACE TO �I���t���O
015090         PERFORM �����p���҂e�Ǎ�
015100         PERFORM UNTIL ( �I���t���O = "YES" ) OR
015110                       ( �A���ҍ폜�|���҃R�[�h   NOT = ���p�|���҃R�[�h )
015120             DELETE �����p���҂e
015130             INVALID KEY
015140                 MOVE NC"���p" TO �t�@�C�����v
015150                 PERFORM �G���[�\���c
015160             END-DELETE
015170             PERFORM �����p���҂e�Ǎ�
015180         END-PERFORM
015190     END-IF.
015200*================================================================*
015210 �����p���҂e�ύX SECTION.
015220*
015230     MOVE ZERO                   TO  ���p�|�{�p�a��N��.
015240     MOVE �A���ҍ폜�|���҃R�[�h TO  ���p�|���҃R�[�h.
015250*
015260     START �����p���҂e KEY IS >= ���p�|���҃R�[�h
015270                                  ���p�|�{�p�a��N��
015280     END-START.
015290*
015300     IF ��ԃL�[ = "00"
015310         MOVE SPACE TO �I���t���O
015320         PERFORM �����p���҂e�Ǎ�
015330         PERFORM UNTIL ( �I���t���O = "YES" ) OR
015340                       ( �A���ҍ폜�|���҃R�[�h   NOT = ���p�|���҃R�[�h )
015350             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO ���p�|���҃R�[�h 
015360             WRITE ���p�|���R�[�h
015370             INVALID KEY
015380                 MOVE NC"���p" TO �t�@�C�����v
015390                 PERFORM �G���[�\��
015400             END-WRITE
015410             MOVE  �A���ҍ폜�|���҃R�[�h TO ���p�|���҃R�[�h 
015420             DELETE �����p���҂e
015430             INVALID KEY
015440                 MOVE NC"���p" TO �t�@�C�����v
015450                 PERFORM �G���[�\���c
015460             END-DELETE
015470             PERFORM �����p���҂e�Ǎ�
015480         END-PERFORM
015490     END-IF.
015500*
015510*================================================================*
015520 �����p���҂e�}�ԍ쐬 SECTION.
015530*
015540     MOVE �A���ҍ폜�|�{�p�a��N�� TO  ���p�|�{�p�a��N��.
015550     MOVE �A���ҍ폜�|���҃R�[�h   TO  ���p�|���҃R�[�h.
015560*
015570     READ �����p���҂e
015580     NOT INVALID KEY
015590*      /*�}�ԗL�背�R�[�h�̍쐬
015600         MOVE �A���ҍ폜�|�ύX�}�� TO ���p�|�}��
015610         WRITE ���p�|���R�[�h
015620         INVALID KEY
015630             MOVE NC"���p" TO �t�@�C�����v
015640             PERFORM �G���[�\��
015650         END-WRITE
015660     END-READ.
015670*
015680*================================================================*
015690 �����p���҂e�Ǎ� SECTION.
015700*
015710     READ �����p���҂e NEXT
015720     AT END
015730         MOVE "YES" TO �I���t���O
015740     END-READ.
015750*================================================================*
015760 �o�[�R�[�h�Ǘ��e�����폜 SECTION.
015770*
015780     MOVE �A���ҍ폜�|���҃R�[�h   TO  �o�ǁ|���҃R�[�h.
015790     MOVE �A���ҍ폜�|�{�p�a��N�� TO  �o�ǁ|�{�p�a��N��.
015800     MOVE ZERO                     TO  �o�ǁ|�����ԍ�.
015810     START �o�[�R�[�h�Ǘ��e KEY IS >= �o�ǁ|���҃R�[�h
015820                                      �o�ǁ|�{�p�a��N��
015830                                      �o�ǁ|�����ԍ�
015840     END-START.
015850     IF ��ԃL�[ = "00"
015860         MOVE SPACE TO �I���t���O
015870         PERFORM �o�[�R�[�h�Ǘ��e�Ǎ�
015880         PERFORM UNTIL ( �I���t���O = "YES" ) OR
015890                       ( �A���ҍ폜�|���҃R�[�h   NOT = �o�ǁ|���҃R�[�h ) OR
015900                       ( �A���ҍ폜�|�{�p�a��N�� NOT = �o�ǁ|�{�p�a��N�� )
015910             DELETE �o�[�R�[�h�Ǘ��e
015920             INVALID KEY
015930                 MOVE NC"�o��" TO �t�@�C�����v
015940                 PERFORM �G���[�\���c
015950             END-DELETE
015960             PERFORM �o�[�R�[�h�Ǘ��e�Ǎ�
015970         END-PERFORM
015980     END-IF.
015990*================================================================*
016000 �o�[�R�[�h�Ǘ��e�S�폜 SECTION.
016010*
016020     MOVE �A���ҍ폜�|���҃R�[�h TO  �o�ǁ|���҃R�[�h.
016030     MOVE ZERO                   TO  �o�ǁ|�{�p�a��N��.
016040     MOVE ZERO                   TO  �o�ǁ|�����ԍ�.
016050     START �o�[�R�[�h�Ǘ��e KEY IS >= �o�ǁ|���҃R�[�h
016060                                      �o�ǁ|�{�p�a��N��
016070                                      �o�ǁ|�����ԍ�
016080     END-START.
016090     IF ��ԃL�[ = "00"
016100         MOVE SPACE TO �I���t���O
016110         PERFORM �o�[�R�[�h�Ǘ��e�Ǎ�
016120         PERFORM UNTIL ( �I���t���O = "YES" ) OR
016130                       ( �A���ҍ폜�|���҃R�[�h   NOT = �o�ǁ|���҃R�[�h )
016140             DELETE �o�[�R�[�h�Ǘ��e
016150             INVALID KEY
016160                 MOVE NC"�o��" TO �t�@�C�����v
016170                 PERFORM �G���[�\���c
016180             END-DELETE
016190             PERFORM �o�[�R�[�h�Ǘ��e�Ǎ�
016200         END-PERFORM
016210     END-IF.
016220*================================================================*
016230 �o�[�R�[�h�Ǘ��e�ύX SECTION.
016240*
016250     MOVE ZERO                   TO  �o�ǁ|�{�p�a��N��
016260     MOVE ZERO                   TO  �o�ǁ|�����ԍ�.
016270     MOVE �A���ҍ폜�|���҃R�[�h TO  �o�ǁ|���҃R�[�h.
016280*
016290     START �o�[�R�[�h�Ǘ��e KEY IS >= �o�ǁ|���҃R�[�h
016300                                      �o�ǁ|�{�p�a��N��
016310                                      �o�ǁ|�����ԍ�
016320     END-START.
016330*
016340     IF ��ԃL�[ = "00"
016350         MOVE SPACE TO �I���t���O
016360         PERFORM �o�[�R�[�h�Ǘ��e�Ǎ�
016370         PERFORM UNTIL ( �I���t���O = "YES" ) OR
016380                       ( �A���ҍ폜�|���҃R�[�h   NOT = �o�ǁ|���҃R�[�h )
016390             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �o�ǁ|���҃R�[�h
016400*        / REWITE /
016410             REWRITE �o�ǁ|���R�[�h
016420             INVALID KEY
016430                 MOVE NC"�o��" TO �t�@�C�����v
016440                 PERFORM �G���[�\��
016450             END-REWRITE
016460             PERFORM �o�[�R�[�h�Ǘ��e�Ǎ�
016470         END-PERFORM
016480     END-IF.
016490*
016500*================================================================*
016510 �o�[�R�[�h�Ǘ��e�}�ԍ쐬 SECTION.
016520*
016530     MOVE �A���ҍ폜�|���҃R�[�h   TO  �o�ǁ|���҃R�[�h.
016540     MOVE �A���ҍ폜�|�{�p�a��N�� TO  �o�ǁ|�{�p�a��N��.
016550     MOVE ZERO                     TO  �o�ǁ|�����ԍ�.
016560     START �o�[�R�[�h�Ǘ��e KEY IS >= �o�ǁ|���҃R�[�h
016570                                      �o�ǁ|�{�p�a��N��
016580                                      �o�ǁ|�����ԍ�
016590     END-START.
016600     IF ��ԃL�[ = "00"
016610         MOVE SPACE TO �I���t���O
016620         PERFORM �o�[�R�[�h�Ǘ��e�Ǎ�
016630         PERFORM UNTIL ( �I���t���O = "YES" ) OR
016640                       ( �A���ҍ폜�|���҃R�[�h   NOT = �o�ǁ|���҃R�[�h ) OR
016650                       ( �A���ҍ폜�|�{�p�a��N�� NOT = �o�ǁ|�{�p�a��N�� )
016660* �����ԍ��̃_�u����h�����߁A��Ƀ��R�[�h�폜���Ă���쐬����B
016670*------------   / ���R�[�h�̑ޔ� / -------------------------------*
016680             INITIALIZE �ޔ��o�|���R�[�h
016690             MOVE �o�ǁ|���R�[�h  TO �ޔ��o�|���R�[�h
016700*------------   / ���R�[�h�̍폜 / -------------------------------*
016710             DELETE �o�[�R�[�h�Ǘ��e
016720             INVALID KEY
016730                 MOVE NC"�o��" TO �t�@�C�����v
016740                 PERFORM �G���[�\���c
016750             END-DELETE
016760*
016770*------------   / �V���R�[�h�̍쐬 / -------------------------------*
016780             MOVE SPACE TO �o�ǁ|���R�[�h
016790             INITIALIZE �o�ǁ|���R�[�h
016800             MOVE �ޔ��o�|���R�[�h  TO �o�ǁ|���R�[�h
016810             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �o�ǁ|���҃R�[�h
016820             WRITE �o�ǁ|���R�[�h
016830             INVALID KEY
016840                 MOVE NC"�o��" TO �t�@�C�����v
016850                 PERFORM �G���[�\��
016860             END-WRITE
016870             MOVE  �A���ҍ폜�|���҃R�[�h TO �o�ǁ|���҃R�[�h
016880             PERFORM �o�[�R�[�h�Ǘ��e�Ǎ�
016890         END-PERFORM
016900     END-IF.
016910*
016920*================================================================*
016930 �o�[�R�[�h�Ǘ��e�Ǎ� SECTION.
016940*
016950     READ �o�[�R�[�h�Ǘ��e NEXT
016960     AT END
016970         MOVE "YES" TO �I���t���O
016980     END-READ.
016990*================================================================*
017000 ���Z�v�g�e�����폜 SECTION.
017010*
017020     MOVE �A���ҍ폜�|���҃R�[�h   TO ���Z�|���҃R�[�h.
017030     MOVE �A���ҍ폜�|�{�p�a��N�� TO ���Z�|�{�p�a��N��.
017040     MOVE ZERO                     TO ���Z�|���Z���.
017050*
017060     START ���Z�v�g�e KEY IS >= ���Z�|���҃R�[�h
017070                                ���Z�|�{�p�a��N��
017080                                ���Z�|���Z���
017090     END-START.
017100*
017110     IF ��ԃL�[ = "00"
017120         MOVE SPACE TO �I���t���O
017130         PERFORM ���Z�v�g�e�Ǎ�
017140         PERFORM UNTIL (���Z�|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
017150                       (���Z�|�{�p�a��N�� NOT = �A���ҍ폜�|�{�p�a��N��) OR
017160                       (�I���t���O             = "YES")
017170             DELETE ���Z�v�g�e
017180             INVALID KEY
017190                 MOVE NC"���Z�v�g" TO �t�@�C�����v
017200                 PERFORM �G���[�\���c
017210             END-DELETE
017220             PERFORM ���Z�v�g�e�Ǎ�
017230         END-PERFORM
017240     END-IF.
017250*
017260*================================================================*
017270 ���Z�v�g�e�S�폜 SECTION.
017280*
017290     MOVE �A���ҍ폜�|���҃R�[�h   TO ���Z�|���҃R�[�h.
017300     MOVE ZERO                     TO ���Z�|�{�p�a��N��.
017310     MOVE ZERO                     TO ���Z�|���Z���.
017320*
017330     START ���Z�v�g�e KEY IS >= ���Z�|���҃R�[�h
017340                                ���Z�|�{�p�a��N��
017350                                ���Z�|���Z���
017360     END-START.
017370*
017380     IF ��ԃL�[ = "00"
017390         MOVE SPACE TO �I���t���O
017400         PERFORM ���Z�v�g�e�Ǎ�
017410         PERFORM UNTIL (���Z�|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h) OR
017420                       (�I���t���O             = "YES")
017430             DELETE ���Z�v�g�e
017440             INVALID KEY
017450                 MOVE NC"���Z�v�g" TO �t�@�C�����v
017460                 PERFORM �G���[�\���c
017470             END-DELETE
017480             PERFORM ���Z�v�g�e�Ǎ�
017490         END-PERFORM
017500     END-IF.
017510*
017520*================================================================*
017530 ���Z�v�g�e�ύX SECTION.
017540*
017550     MOVE �A���ҍ폜�|���҃R�[�h   TO ���Z�|���҃R�[�h.
017560     MOVE ZERO                     TO ���Z�|�{�p�a��N��.
017570     MOVE ZERO                     TO ���Z�|���Z���.
017580*
017590     START ���Z�v�g�e KEY IS >= ���Z�|���҃R�[�h
017600                                ���Z�|�{�p�a��N��
017610                                ���Z�|���Z���
017620     END-START.
017630*
017640     IF ��ԃL�[ = "00"
017650         MOVE SPACE TO �I���t���O
017660         PERFORM ���Z�v�g�e�Ǎ�
017670         PERFORM UNTIL (���Z�|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h) OR
017680                       (�I���t���O             = "YES")
017690             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO ���Z�|���҃R�[�h
017700             WRITE ���Z�|���R�[�h
017710             INVALID KEY
017720                 MOVE NC"���Z" TO �t�@�C�����v
017730                 PERFORM �G���[�\��
017740             END-WRITE
017750             MOVE  �A���ҍ폜�|���҃R�[�h TO ���Z�|���҃R�[�h
017760             DELETE ���Z�v�g�e
017770             INVALID KEY
017780                 MOVE NC"���Z�v�g" TO �t�@�C�����v
017790                 PERFORM �G���[�\���c
017800             END-DELETE
017810             PERFORM ���Z�v�g�e�Ǎ�
017820         END-PERFORM
017830     END-IF.
017840*
017850*================================================================*
017860 ���Z�v�g�e�}�ԍ쐬 SECTION.
017870*
017871     MOVE SPACE TO ���Z���݃t���O.
017872*
017880     MOVE �A���ҍ폜�|���҃R�[�h   TO ���Z�|���҃R�[�h.
017890     MOVE �A���ҍ폜�|�{�p�a��N�� TO ���Z�|�{�p�a��N��.
017900     MOVE ZERO                     TO ���Z�|���Z���.
017910*
017912     START ���Z�v�g�e KEY IS >= ���Z�|���҃R�[�h
017913                                ���Z�|�{�p�a��N��
017914                                ���Z�|���Z���
017915     END-START.
017916*
017917     IF ��ԃL�[ = "00"
017918         MOVE SPACE TO �I���t���O
017919         PERFORM ���Z�v�g�e�Ǎ�
017921         PERFORM UNTIL (���Z�|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
017922                       (���Z�|�{�p�a��N�� NOT = �A���ҍ폜�|�{�p�a��N��) OR
017923                       (�I���t���O             = "YES")
017925                MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO ���Z�|���҃R�[�h
017926                WRITE ���Z�|���R�[�h
017927                INVALID KEY
017928                    MOVE NC"���Z" TO �t�@�C�����v
017929                    PERFORM �G���[�\��
017930                END-WRITE
017940                MOVE "YES" TO ���Z���݃t���O
017941                PERFORM ���Z�v�g�e�Ǎ�
017942         END-PERFORM
017979     END-IF.
018093*
018094*================================================================*
018095 ���Z�v�g�e�Ǎ� SECTION.
018100*
018110     READ ���Z�v�g�e NEXT
018120     AT END
018130         MOVE "YES" TO �I���t���O
018140     END-READ.
018141*================================================================*
018142*================================================================*
018143 ���Z�v�g�J���e�����폜 SECTION.
018144*
018145*
018146     MOVE �A���ҍ폜�|�{�p�a��N�� TO ���Z�J���|�{�p�a��N��.
018147     MOVE �A���ҍ폜�|���҃R�[�h   TO ���Z�J���|���҃R�[�h.
018149*   / 4:�J��,5:������ /
018150     MOVE 4                        TO ���Z�J���|���Z���.
018156*
018157     READ ���Z�v�g�J���e
018158     NOT INVALID KEY
018159         DELETE ���Z�v�g�J���e
018160         INVALID KEY
018161             MOVE NC"���Z�v�g�J���e" TO �t�@�C�����v
018162             PERFORM �G���[�\���c
018163         END-DELETE
018164     END-READ.
018165***
018167     MOVE �A���ҍ폜�|�{�p�a��N�� TO ���Z�J���|�{�p�a��N��.
018168     MOVE �A���ҍ폜�|���҃R�[�h   TO ���Z�J���|���҃R�[�h.
018169*   / 4:�J��,5:������ /
018170     MOVE 5                        TO ���Z�J���|���Z���.
018171*
018172     READ ���Z�v�g�J���e
018173     NOT INVALID KEY
018174         DELETE ���Z�v�g�J���e
018175         INVALID KEY
018176             MOVE NC"���Z�v�g�J���e" TO �t�@�C�����v
018177             PERFORM �G���[�\���c
018178         END-DELETE
018179     END-READ.
018190*
018191*================================================================*
018192 ���Z�v�g�J���e�S�폜 SECTION.
018193*
018194     MOVE ZERO            TO ���Z�J���|�{�p�a��N��.
018195     MOVE ZERO            TO ���Z�J���|���Ҕԍ�.
018196     MOVE SPACE           TO ���Z�J���|�}��.
018197     MOVE ZERO            TO ���Z�J���|���Z���.
018198*
018199     START ���Z�v�g�J���e KEY IS >= ���Z�J���|�{�p�a��N��
018200                                    ���Z�J���|���҃R�[�h
018201                                    ���Z�J���|���Z���
018202     END-START.
018206*
018207     IF ��ԃL�[ = "00"
018208         MOVE SPACE            TO �I���t���O
018209         PERFORM ���Z�v�g�J���e�Ǎ�
018210         PERFORM UNTIL (�I���t���O   = "YES")
018211             IF (���Z�J���|���҃R�[�h    = �A���ҍ폜�|���҃R�[�h)
018212                 DELETE ���Z�v�g�J���e
018213                 INVALID KEY
018214                     MOVE NC"���Z�v�g�J��" TO �t�@�C�����v
018215                     PERFORM �G���[�\���c
018216                 END-DELETE
018217             END-IF
018218             PERFORM ���Z�v�g�J���e�Ǎ�
018219         END-PERFORM
018220     END-IF.
018247*
018248*================================================================*
018249 ���Z�v�g�J���e�ύX SECTION.
018250*
018251     MOVE ZERO            TO ���Z�J���|�{�p�a��N��.
018252     MOVE ZERO            TO ���Z�J���|���Ҕԍ�.
018253     MOVE SPACE           TO ���Z�J���|�}��.
018254     MOVE ZERO            TO ���Z�J���|���Z���.
018255*
018256     START ���Z�v�g�J���e KEY IS >= ���Z�J���|�{�p�a��N��
018257                                    ���Z�J���|���҃R�[�h
018258                                    ���Z�J���|���Z���
018259     END-START.
018260*
018261     IF ��ԃL�[ = "00"
018262         MOVE SPACE            TO �I���t���O
018263         PERFORM ���Z�v�g�J���e�Ǎ�
018264         PERFORM UNTIL (�I���t���O   = "YES")
018265             IF (���Z�J���|���҃R�[�h    = �A���ҍ폜�|���҃R�[�h)
018267                 MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO ���Z�J���|���҃R�[�h
018268                 WRITE ���Z�J���|���R�[�h
018269                 INVALID KEY
018270                     MOVE NC"���Z�J��" TO �t�@�C�����v
018271                     PERFORM �G���[�\��
018272                 END-WRITE
018273                 MOVE  �A���ҍ폜�|���҃R�[�h TO ���Z�J���|���҃R�[�h
018284                 DELETE ���Z�v�g�J���e
018285                 INVALID KEY
018286                     MOVE NC"���Z�v�g�J��" TO �t�@�C�����v
018287                     PERFORM �G���[�\���c
018288                 END-DELETE
018289             END-IF
018290             PERFORM ���Z�v�g�J���e�Ǎ�
018291         END-PERFORM
018292     END-IF.
018308*
018309*================================================================*
018310 ���Z�v�g�J���e�}�ԍ쐬 SECTION.
018311*
018312     MOVE �A���ҍ폜�|�{�p�a��N�� TO ���Z�J���|�{�p�a��N��.
018313     MOVE �A���ҍ폜�|���҃R�[�h   TO ���Z�J���|���҃R�[�h.
018314*   / 4:�J��,5:������ /
018315     MOVE 4                        TO ���Z�J���|���Z���.
018316*
018317     READ ���Z�v�g�J���e
018318     NOT INVALID KEY
018319         MOVE  �A���ҍ폜�|�ύX�}�� TO  ���Z�J���|�}��
018320         WRITE ���Z�J���|���R�[�h
018321         INVALID KEY
018322             MOVE NC"���Z�J��" TO �t�@�C�����v
018323             PERFORM �G���[�\��
018324         END-WRITE
018333     END-READ.
018334***
018348     MOVE �A���ҍ폜�|�{�p�a��N�� TO ���Z�J���|�{�p�a��N��.
018349     MOVE �A���ҍ폜�|���҃R�[�h   TO ���Z�J���|���҃R�[�h.
018350*   / 4:�J��,5:������ /
018351     MOVE 5                        TO ���Z�J���|���Z���.
018352*
018353     READ ���Z�v�g�J���e
018354     NOT INVALID KEY
018355         MOVE  �A���ҍ폜�|�ύX�}�� TO  ���Z�J���|�}��
018356         WRITE ���Z�J���|���R�[�h
018357         INVALID KEY
018358             MOVE NC"���Z�J��" TO �t�@�C�����v
018359             PERFORM �G���[�\��
018360         END-WRITE
018361     END-READ.
018394*
018395*================================================================*
018396 ���Z�v�g�J���e�Ǎ� SECTION.
018397*
018398     READ ���Z�v�g�J���e NEXT
018399     AT END
018400         MOVE "YES" TO �I���t���O
018401     END-READ.
018402*================================================================*
018403*================================================================*
018404
018405*                             RECORD      KEY          IS  �ؗ��|�{�p�敪
018406*                                                          �ؗ��|�p���敪
018407*                                                          �ؗ��|�{�p�a��N��
018408*                                                          �ؗ��|���҃R�[�h
018409*                                                          �ؗ��|�A��
018422
018423*================================================================*
018424 �ؖ��������e�S�폜 SECTION.
018425*
018426     MOVE ZERO            TO �ؗ��|�{�p�敪.
018427     MOVE ZERO            TO �ؗ��|�p���敪.
018428     MOVE ZERO            TO �ؗ��|�{�p�a��N��.
018429     MOVE ZERO            TO �ؗ��|���Ҕԍ�.
018431     MOVE SPACE           TO �ؗ��|�}��.
018432     MOVE ZERO            TO �ؗ��|�A��.
018433*
018434     START �ؖ��������e KEY IS >= �ؗ��|�{�p�敪
018435                                  �ؗ��|�p���敪
018436                                  �ؗ��|�{�p�a��N��
018437                                  �ؗ��|���҃R�[�h
018438                                  �ؗ��|�A��
018441     END-START.
018442*
018443     IF ��ԃL�[ = "00"
018444         MOVE SPACE            TO �I���t���O
018445         PERFORM �ؖ��������e�Ǎ�
018446         PERFORM UNTIL (�I���t���O   = "YES")
018447             IF (�ؗ��|�p���敪  NOT = 999) AND
                      (�ؗ��|���҃R�[�h    = �A���ҍ폜�|���҃R�[�h)
018448                 DELETE �ؖ��������e
018449                 INVALID KEY
018450                     MOVE NC"�ؖ��������e" TO �t�@�C�����v
018451                     PERFORM �G���[�\���c
018452                 END-DELETE
018453             END-IF
018454             PERFORM �ؖ��������e�Ǎ�
018455         END-PERFORM
018456     END-IF.
018457*
018458*================================================================*
018459 �ؖ��������e�ύX SECTION.
018460*
018472     MOVE ZERO            TO �ؗ��|�{�p�敪.
018473     MOVE ZERO            TO �ؗ��|�p���敪.
018474     MOVE ZERO            TO �ؗ��|�{�p�a��N��.
018475     MOVE ZERO            TO �ؗ��|���Ҕԍ�.
018476     MOVE SPACE           TO �ؗ��|�}��.
018477     MOVE ZERO            TO �ؗ��|�A��.
018478*
018479     START �ؖ��������e KEY IS >= �ؗ��|�{�p�敪
018480                                  �ؗ��|�p���敪
018481                                  �ؗ��|�{�p�a��N��
018482                                  �ؗ��|���҃R�[�h
018483                                  �ؗ��|�A��
018484     END-START.
018485*
018486     IF ��ԃL�[ = "00"
018487         MOVE SPACE            TO �I���t���O
018488         PERFORM �ؖ��������e�Ǎ�
018489         PERFORM UNTIL (�I���t���O   = "YES")
018490             IF (�ؗ��|�p���敪  NOT = 999) AND
                      (�ؗ��|���҃R�[�h    = �A���ҍ폜�|���҃R�[�h)
018492                 MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �ؗ��|���҃R�[�h
018493                 WRITE �ؗ��|���R�[�h
018494                 INVALID KEY
018495                     MOVE NC"�ؖ��������e" TO �t�@�C�����v
018496                     PERFORM �G���[�\��
018497                 END-WRITE
018498                 MOVE  �A���ҍ폜�|���҃R�[�h TO �ؗ��|���҃R�[�h
018499                 DELETE �ؖ��������e
018500                 INVALID KEY
018501                     MOVE NC"�ؖ��������e" TO �t�@�C�����v
018502                     PERFORM �G���[�\���c
018503                 END-DELETE
018515             END-IF
018516             PERFORM �ؖ��������e�Ǎ�
018517         END-PERFORM
018518     END-IF.
018524*
018525*================================================================*
018526 �ؖ��������e�Ǎ� SECTION.
018527*
018528     READ �ؖ��������e NEXT
018529     AT END
018530         MOVE "YES" TO �I���t���O
018531     END-READ.
018532*
018533*================================================================*
018534*================================================================*
018535 �J���e�t�@�C�������폜 SECTION.
018536*
018537     MOVE ZERO         TO �J�|��R�[�h.
018538     MOVE ZERO         TO �J�|�敪.
018539     MOVE ZERO         TO �J�|�{�p�a��N��.
018540     MOVE ZERO         TO �J�|���Ҕԍ�.
018541     MOVE SPACE        TO �J�|�}��.
018542*
018543     START �J���e�t�@�C�� KEY IS >= �J�|�敪
018544                                    �J�|���҃R�[�h
018545                                    �J�|�{�p�a��N��
018546                                    �J�|��R�[�h
018547     END-START.
018548*
018549     IF ��ԃL�[ = "00"
018550         MOVE SPACE TO �I���t���O
018551         PERFORM �J���e�t�@�C���Ǎ�
018552         PERFORM UNTIL (�I���t���O = "YES")
018553             IF ( �J�|�{�p�a��N�� = �A���ҍ폜�|�{�p�a��N�� ) AND
018554                ( �J�|���҃R�[�h   = �A���ҍ폜�|���҃R�[�h )
018555                 DELETE �J���e�t�@�C��
018556                 INVALID KEY
018557                     MOVE NC"�J���e" TO �t�@�C�����v
018558                     PERFORM �G���[�\���c
018559                 END-DELETE
018560             END-IF
018561             PERFORM �J���e�t�@�C���Ǎ�
018562         END-PERFORM
018563     END-IF.
018564*
018565*================================================================*
018566 �J���e�t�@�C���S�폜 SECTION.
018567*
018568     MOVE ZERO         TO �J�|��R�[�h.
018569     MOVE ZERO         TO �J�|�敪.
018570     MOVE ZERO         TO �J�|�{�p�a��N��.
018571     MOVE ZERO         TO �J�|���Ҕԍ�.
018572     MOVE SPACE        TO �J�|�}��.
018573*
018574     START �J���e�t�@�C�� KEY IS >= �J�|�敪
018575                                    �J�|���҃R�[�h
018576                                    �J�|�{�p�a��N��
018580                                    �J�|��R�[�h
018590     END-START.
018600*
018610     IF ��ԃL�[ = "00"
018620         MOVE SPACE TO �I���t���O
018630         PERFORM �J���e�t�@�C���Ǎ�
018640         PERFORM UNTIL (�I���t���O = "YES")
018650             IF ( �J�|���҃R�[�h   = �A���ҍ폜�|���҃R�[�h )
018660                 DELETE �J���e�t�@�C��
018670                 INVALID KEY
018680                     MOVE NC"�J���e" TO �t�@�C�����v
018690                     PERFORM �G���[�\���c
018700                 END-DELETE
018710             END-IF
018720             PERFORM �J���e�t�@�C���Ǎ�
018730         END-PERFORM
018740     END-IF.
018750*
018760*================================================================*
018770 �J���e�t�@�C���ύX SECTION.
018780*
018790     MOVE ZERO         TO �J�|��R�[�h.
018800     MOVE ZERO         TO �J�|�敪.
018810     MOVE ZERO         TO �J�|�{�p�a��N��.
018820     MOVE ZERO         TO �J�|���Ҕԍ�.
018830     MOVE SPACE        TO �J�|�}��.
018840*
018850     START �J���e�t�@�C�� KEY IS >= �J�|�敪
018860                                    �J�|���҃R�[�h
018870                                    �J�|�{�p�a��N��
018880                                    �J�|��R�[�h
018890     END-START.
018900*
018910     IF ��ԃL�[ = "00"
018920         MOVE SPACE TO �I���t���O
018930         PERFORM �J���e�t�@�C���Ǎ�
018940         PERFORM UNTIL (�I���t���O = "YES")
018950             IF ( �J�|���҃R�[�h   = �A���ҍ폜�|���҃R�[�h )
018960                 MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �J�|���҃R�[�h
018970                 WRITE �J�|���R�[�h
018980                 INVALID KEY
018990                     MOVE NC"�J���e" TO �t�@�C�����v
019000                     PERFORM �G���[�\��
019010                 END-WRITE
019020                 MOVE  �A���ҍ폜�|���҃R�[�h TO �J�|���҃R�[�h
019030                 DELETE �J���e�t�@�C��
019040                 INVALID KEY
019050                     MOVE NC"�J���e" TO �t�@�C�����v
019060                     PERFORM �G���[�\���c
019070                 END-DELETE
019080             END-IF
019090             PERFORM �J���e�t�@�C���Ǎ�
019100         END-PERFORM
019110     END-IF.
019120*
019130*================================================================*
019140 �J���e�t�@�C���}�ԍ쐬 SECTION.
019150*
019160     MOVE ZERO         TO �J�|��R�[�h.
019170     MOVE ZERO         TO �J�|�敪.
019180     MOVE ZERO         TO �J�|�{�p�a��N��.
019190     MOVE ZERO         TO �J�|���Ҕԍ�.
019200     MOVE SPACE        TO �J�|�}��.
019210*
019220     START �J���e�t�@�C�� KEY IS >= �J�|�敪
019230                                    �J�|���҃R�[�h
019240                                    �J�|�{�p�a��N��
019250                                    �J�|��R�[�h
019260     END-START.
019270*
019280     IF ��ԃL�[ = "00"
019290         MOVE SPACE TO �I���t���O
019300         PERFORM �J���e�t�@�C���Ǎ�
019310         PERFORM UNTIL (�I���t���O = "YES")
019320             IF ( �J�|�{�p�a��N�� = �A���ҍ폜�|�{�p�a��N�� ) AND
019330                ( �J�|���҃R�[�h   = �A���ҍ폜�|���҃R�[�h )
019340                 MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �J�|���҃R�[�h
019350                 WRITE �J�|���R�[�h
019360                 INVALID KEY
019370                     MOVE NC"�J���e" TO �t�@�C�����v
019380                     PERFORM �G���[�\��
019390                 END-WRITE
019400                 MOVE  �A���ҍ폜�|���҃R�[�h TO �J�|���҃R�[�h
019410             END-IF
019420             PERFORM �J���e�t�@�C���Ǎ�
019430         END-PERFORM
019440     END-IF.
019450*
019460*================================================================*
019470 �J���e�t�@�C���Ǎ� SECTION.
019480*
019490     READ �J���e�t�@�C�� NEXT
019500     AT END
019510         MOVE "YES" TO �I���t���O
019520     END-READ.
019530*================================================================*
019540 �����t�@�C�������폜 SECTION.
019550*
019560     MOVE �A���ҍ폜�|���҃R�[�h   TO �����|���҃R�[�h.
019570     MOVE �A���ҍ폜�|�{�p�a��N�� TO �����|�{�p�a��N��.
019580     MOVE ZERO                     TO �����|�{�p��.
019590     MOVE ZERO                     TO �����|����敪.
019600*
019610     START �����t�@�C�� KEY IS >= �����|���҃R�[�h
019620                                  �����|�{�p�a��N����
019630                                  �����|����敪
019640     END-START.
019650*
019660     IF ��ԃL�[ = "00"
019670         MOVE SPACE            TO �I���t���O
019680         PERFORM �����t�@�C���Ǎ�
019690         PERFORM UNTIL (�����|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
019700                       (�����|�{�p�a��N�� NOT = �A���ҍ폜�|�{�p�a��N��) OR
019710                       (�I���t���O             = "YES")
019720             DELETE �����t�@�C��
019730             INVALID KEY
019740                 MOVE NC"����" TO �t�@�C�����v
019750                 PERFORM �G���[�\���c
019760             END-DELETE
019770             PERFORM �����t�@�C���Ǎ�
019780         END-PERFORM
019790     END-IF.
019800*
019810*================================================================*
019820 �����t�@�C���S�폜 SECTION.
019830*
019840     MOVE �A���ҍ폜�|���҃R�[�h   TO �����|���҃R�[�h.
019850     MOVE ZERO                     TO �����|�{�p�a��N����.
019860     MOVE ZERO                     TO �����|����敪.
019870*
019880     START �����t�@�C�� KEY IS >= �����|���҃R�[�h
019890                                  �����|�{�p�a��N����
019900                                  �����|����敪
019910     END-START.
019920*
019930     IF ��ԃL�[ = "00"
019940         MOVE SPACE            TO �I���t���O
019950         PERFORM �����t�@�C���Ǎ�
019960         PERFORM UNTIL (�����|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
019970                       (�I���t���O             = "YES")
019980             DELETE �����t�@�C��
019990             INVALID KEY
020000                 MOVE NC"����" TO �t�@�C�����v
020010                 PERFORM �G���[�\���c
020020             END-DELETE
020030             PERFORM �����t�@�C���Ǎ�
020040         END-PERFORM
020050     END-IF.
020060*
020070*================================================================*
020080 �����t�@�C���ύX SECTION.
020090*
020100     MOVE �A���ҍ폜�|���҃R�[�h   TO �����|���҃R�[�h.
020110     MOVE ZERO                     TO �����|�{�p�a��N����.
020120     MOVE ZERO                     TO �����|����敪.
020130*
020140     START �����t�@�C�� KEY IS >= �����|���҃R�[�h
020150                                  �����|�{�p�a��N����
020160                                  �����|����敪
020170     END-START.
020180*
020190     IF ��ԃL�[ = "00"
020200         MOVE SPACE TO �I���t���O
020210         PERFORM �����t�@�C���Ǎ�
020220         PERFORM UNTIL (�����|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
020230                       (�I���t���O             = "YES")
020240             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �����|���҃R�[�h
020250             WRITE �����|���R�[�h
020260             INVALID KEY
020270                 MOVE NC"����" TO �t�@�C�����v
020280                 PERFORM �G���[�\��
020290             END-WRITE
020300             MOVE  �A���ҍ폜�|���҃R�[�h TO �����|���҃R�[�h
020310             DELETE �����t�@�C��
020320             INVALID KEY
020330                 MOVE NC"����" TO �t�@�C�����v
020340                 PERFORM �G���[�\���c
020350             END-DELETE
020360             PERFORM �����t�@�C���Ǎ�
020370         END-PERFORM
020380     END-IF.
020390*
020400*================================================================*
020410 �����t�@�C���}�ԍ쐬 SECTION.
020420*
020430     MOVE �A���ҍ폜�|���҃R�[�h   TO �����|���҃R�[�h.
020440     MOVE �A���ҍ폜�|�{�p�a��N�� TO �����|�{�p�a��N��.
020450     MOVE 1                        TO �����|�{�p��
020460     MOVE ZERO                     TO �����|����敪.
020470*
020480     START �����t�@�C�� KEY IS >= �����|���҃R�[�h
020490                                  �����|�{�p�a��N����
020500                                  �����|����敪
020510     END-START.
020520*
020530     IF ��ԃL�[ = "00"
020540         MOVE SPACE TO �I���t���O
020550         PERFORM �����t�@�C���Ǎ�
020560*/�ύX���ȍ~�̓R���o�[�g���Ȃ�
020570         PERFORM UNTIL (�����|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
020580                       (�����|�{�p�a��N�� NOT = �A���ҍ폜�|�{�p�a��N��) OR
020590                       (�����|�{�p��          >= �A���Ҏ}�ԁ|�ύX��    ) OR
020600                       (�I���t���O             = "YES")
020610             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �����|���҃R�[�h
020620             WRITE �����|���R�[�h
020630             INVALID KEY
020640                 MOVE NC"����" TO �t�@�C�����v
020650                 PERFORM �G���[�\��
020660             END-WRITE
020670             MOVE  �A���ҍ폜�|���҃R�[�h TO �����|���҃R�[�h
020680             DELETE �����t�@�C��
020690             INVALID KEY
020700                 MOVE NC"����" TO �t�@�C�����v
020710                 PERFORM �G���[�\���c
020720             END-DELETE
020730             PERFORM �����t�@�C���Ǎ�
020740         END-PERFORM
020750     END-IF.
020760*
020770*================================================================*
020780 �����t�@�C���Ǎ� SECTION.
020790*
020800     READ �����t�@�C�� NEXT
020810     AT END
020820         MOVE "YES" TO �I���t���O
020830     END-READ.
020831*================================================================*
020832*================================================================*
020833*================================================================*
020834 �E�v�t�@�C�������폜 SECTION.
020836*
020840* ����敪���Ō�̕��L�[�������̂ŌJ�Ԃ�����
020842*
020843*
020844      PERFORM VARYING �J�ԃJ�E���^ FROM 1 BY 1  UNTIL ( �J�ԃJ�E���^ > �E�v�敪�� ) 
020850*
020851        MOVE �J�ԃJ�E���^             TO �E�v�|����敪
020852        MOVE �A���ҍ폜�|�{�p�a��N�� TO �E�v�|�{�p�a��N��
020853        MOVE �A���ҍ폜�|���҃R�[�h   TO �E�v�|���҃R�[�h
020854        READ �E�v�t�@�C��
020855        NOT INVALID KEY
020856            DELETE �E�v�t�@�C��
020857            INVALID KEY
020858                MOVE NC"�E�v" TO �t�@�C�����v
020859                PERFORM �G���[�\���c
020860            END-DELETE
020861        END-READ
020862*
020866      END-PERFORM.
020875*
020876*================================================================*
020877*================================================================*
020878 �E�v�t�@�C���S�폜 SECTION.
020879*
020880* ����敪���Ō�̕��L�[�������̂ŌJ�Ԃ�����
020881*
020882*
020883      PERFORM VARYING �J�ԃJ�E���^ FROM 1 BY 1  UNTIL ( �J�ԃJ�E���^ > �E�v�敪�� ) 
020884*
020885        MOVE �J�ԃJ�E���^             TO �E�v�|����敪
020886        MOVE �A���ҍ폜�|���҃R�[�h   TO �E�v�|���҃R�[�h
020887        MOVE ZERO                     TO �E�v�|�{�p�a��N��
020889        START �E�v�t�@�C�� KEY IS >= �E�v�|����敪
020890                                     �E�v�|���҃R�[�h
020891                                     �E�v�|�{�p�a��N��
020892        END-START
020893        IF ��ԃL�[ = "00"
020894            MOVE SPACE TO �I���t���O
020895            PERFORM �E�v�t�@�C���Ǎ�
020896            PERFORM UNTIL ( �J�ԃJ�E���^           NOT = �E�v�|����敪 ) OR
020897                          ( �A���ҍ폜�|���҃R�[�h NOT = �E�v�|���҃R�[�h ) OR
020898                          ( �I���t���O = "YES" )
020899                DELETE �E�v�t�@�C��
020900                INVALID KEY
020901                    MOVE NC"�E�v" TO �t�@�C�����v
020902                    PERFORM �G���[�\���c
020903                END-DELETE
020904                PERFORM �E�v�t�@�C���Ǎ�
020905            END-PERFORM
020906        END-IF
020909*
020910      END-PERFORM.
020911*
020912*================================================================*
020913 �E�v�t�@�C���ύX SECTION.
020914*
020915* ����敪���Ō�̕��L�[�������̂ŌJ�Ԃ�����
020916*
020917*
020918      PERFORM VARYING �J�ԃJ�E���^ FROM 1 BY 1  UNTIL ( �J�ԃJ�E���^ > �E�v�敪�� ) 
020919*
020920        MOVE �J�ԃJ�E���^             TO �E�v�|����敪
020921        MOVE �A���ҍ폜�|���҃R�[�h   TO �E�v�|���҃R�[�h
020922        MOVE ZERO                     TO �E�v�|�{�p�a��N��
020923        START �E�v�t�@�C�� KEY IS >= �E�v�|����敪
020924                                     �E�v�|���҃R�[�h
020925                                     �E�v�|�{�p�a��N��
020926        END-START
020927        IF ��ԃL�[ = "00"
020928            MOVE SPACE TO �I���t���O
020929            PERFORM �E�v�t�@�C���Ǎ�
020930            PERFORM UNTIL ( �J�ԃJ�E���^           NOT = �E�v�|����敪 ) OR
020931                          ( �A���ҍ폜�|���҃R�[�h NOT = �E�v�|���҃R�[�h ) OR
020932                          ( �I���t���O = "YES" )
020933
020934                MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �E�v�|���҃R�[�h 
020935                WRITE �E�v�|���R�[�h
020936                INVALID KEY
020937                    MOVE NC"�E�v" TO �t�@�C�����v
020938                    PERFORM �G���[�\��
020939                END-WRITE
020940*
020942                MOVE  �A���ҍ폜�|���҃R�[�h TO �E�v�|���҃R�[�h 
020943                DELETE �E�v�t�@�C��
020944                INVALID KEY
020945                    MOVE NC"�E�v" TO �t�@�C�����v
020946                    PERFORM �G���[�\���c
020947                END-DELETE
020948                PERFORM �E�v�t�@�C���Ǎ�
020949            END-PERFORM
020950        END-IF
020951*
020952      END-PERFORM.
020953*
020954*================================================================*
020955 �E�v�t�@�C���}�ԍ쐬 SECTION.
020956*
020966* ����敪���Ō�̕��L�[�������̂ŌJ�Ԃ�����
020995*
020996      PERFORM VARYING �J�ԃJ�E���^ FROM 1 BY 1  UNTIL ( �J�ԃJ�E���^ > �E�v�敪�� ) 
020997*
020998        MOVE �J�ԃJ�E���^             TO �E�v�|����敪
020999        MOVE �A���ҍ폜�|�{�p�a��N�� TO �E�v�|�{�p�a��N��
021000        MOVE �A���ҍ폜�|���҃R�[�h   TO �E�v�|���҃R�[�h
021001        READ �E�v�t�@�C��
021002        NOT INVALID KEY
021003*          /*�}�ԗL�背�R�[�h�̍쐬
021004            MOVE �A���ҍ폜�|�ύX�}�� TO �E�v�|�}��
021005            WRITE �E�v�|���R�[�h
021006            INVALID KEY
021007                MOVE NC"�E�v" TO �t�@�C�����v
021008                PERFORM �G���[�\��
021009            END-WRITE
021018        END-READ
021019*
021020      END-PERFORM.
021022*
021055*================================================================*
021056 �E�v�t�@�C���Ǎ� SECTION.
021057*
021058     READ �E�v�t�@�C�� NEXT
021059     AT END
021060         MOVE "YES" TO �I���t���O
021061     END-READ.
021062*================================================================*
021063*================================================================*
021064 ��f�ҏ��Q�e�����폜 SECTION.
021065*
021066     MOVE �A���ҍ폜�|���҃R�[�h   TO ��Q�|���҃R�[�h.
021067     MOVE �A���ҍ폜�|�{�p�a��N�� TO ��Q�|�{�p�a��N��.
021068*
021069     READ ��f�ҏ��Q�e
021070     NOT INVALID KEY
021071         DELETE ��f�ҏ��Q�e
021072         INVALID KEY
021073             MOVE NC"��Q" TO �t�@�C�����v
021074             PERFORM �G���[�\���c
021075         END-DELETE
021076     END-READ.
021077*
021078*================================================================*
021079 ��f�ҏ��Q�e�S�폜 SECTION.
021080*
021081     MOVE ZERO            TO ��Q�|���Ҕԍ�.
021082     MOVE SPACE           TO ��Q�|�}��.
021083     MOVE ZERO            TO ��Q�|�{�p�a��N��.
021084*
021085     START ��f�ҏ��Q�e KEY IS >= ��Q�|�{�p�a��N��
021086                                    ��Q�|���҃R�[�h
021087     END-START.
021090*
021100     IF ��ԃL�[ = "00"
021110         MOVE SPACE            TO �I���t���O
021120         PERFORM ��f�ҏ��Q�e�Ǎ�
021130         PERFORM UNTIL (�I���t���O   = "YES")
021140             IF (��Q�|���҃R�[�h    = �A���ҍ폜�|���҃R�[�h)
021150                 DELETE ��f�ҏ��Q�e
021160                 INVALID KEY
021170                     MOVE NC"��Q" TO �t�@�C�����v
021180                     PERFORM �G���[�\���c
021190                 END-DELETE
021200             END-IF
021210             PERFORM ��f�ҏ��Q�e�Ǎ�
021220         END-PERFORM
021230     END-IF.
021240*
021250*================================================================*
021260 ��f�ҏ��Q�e�ύX SECTION.
021270*
021280     MOVE ZERO            TO ��Q�|���Ҕԍ�.
021290     MOVE SPACE           TO ��Q�|�}��.
021300     MOVE ZERO            TO ��Q�|�{�p�a��N��.
021310*
021320     START ��f�ҏ��Q�e KEY IS >= ��Q�|�{�p�a��N��
021330                                    ��Q�|���҃R�[�h
021340     END-START.
021350*
021360     IF ��ԃL�[ = "00"
021370         MOVE SPACE TO �I���t���O
021380         PERFORM ��f�ҏ��Q�e�Ǎ�
021390         PERFORM UNTIL (�I���t���O   = "YES")
021400             IF (��Q�|���҃R�[�h    = �A���ҍ폜�|���҃R�[�h)
021410                 MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO ��Q�|���҃R�[�h
021420                 WRITE ��Q�|���R�[�h
021430                 INVALID KEY
021440                     MOVE NC"��Q" TO �t�@�C�����v
021450                     PERFORM �G���[�\��
021460                 END-WRITE
021470                 MOVE  �A���ҍ폜�|���҃R�[�h TO ��Q�|���҃R�[�h
021480                 DELETE ��f�ҏ��Q�e
021490                 INVALID KEY
021500                     MOVE NC"��Q" TO �t�@�C�����v
021510                     PERFORM �G���[�\���c
021520                 END-DELETE
021530             END-IF
021540             PERFORM ��f�ҏ��Q�e�Ǎ�
021550         END-PERFORM
021560     END-IF.
021570*
021580*================================================================*
021590 ��f�ҏ��Q�e�}�ԍ쐬 SECTION.
021600*
021610     MOVE �A���ҍ폜�|���҃R�[�h   TO ��Q�|���҃R�[�h.
021620     MOVE �A���ҍ폜�|�{�p�a��N�� TO ��Q�|�{�p�a��N��.
021630*
021640     READ ��f�ҏ��Q�e
021650     NOT INVALID KEY
021660         MOVE  �A���ҍ폜�|�ύX�}�� TO  ��Q�|�}��
021670         WRITE ��Q�|���R�[�h
021680         INVALID KEY
021690             MOVE NC"��Q" TO �t�@�C�����v
021700             PERFORM �G���[�\��
021710         END-WRITE
021720     END-READ.
021721*
021722*/�w��}�ԃf�[�^�̂O�O�̃f�[�^�쐬�B�i�����ۂ̂݁j
021723* 2:�J�ЁA3:�����ӁA4:���ہA5:����́A���r���ύX�Ŏ}�ԃ��R�[�h�����Ȃ��B
021724     IF �ی����ނv = 1
021725*
021730       MOVE ZERO     TO ��Q�|�{�p�a��
021740       MOVE ZERO     TO ��Q�|�{�p�N
021750       MOVE ZERO     TO ��Q�|�{�p��
021760       MOVE �A���ҍ폜�|�ύX���҃R�[�h TO ��Q�|���҃R�[�h
021770       READ ��f�ҏ��Q�e
021780       INVALID KEY
021790         MOVE SPACE TO ��Q�|���R�[�h
021800         INITIALIZE    ��Q�|���R�[�h
021810         MOVE �A���ҍ폜�|�ύX���҃R�[�h TO ��Q�|���҃R�[�h
021820         MOVE �A���ҍ폜�|�{�p�a��N��   TO ��Q�|�{�p�a��N��
021830         READ ��f�ҏ��Q�e
021840         NOT INVALID KEY
021871* ����������
021872                 MOVE ZERO           TO ��Q�|�{�p�a��
021873                 MOVE ZERO           TO ��Q�|�{�p�N  
021874                 MOVE ZERO           TO ��Q�|�{�p��  
021875                 MOVE ZERO           TO ��Q�|�����a��
021876                 MOVE ZERO           TO ��Q�|�����N  
021877                 MOVE ZERO           TO ��Q�|������  
021878                 MOVE ZERO           TO ��Q�|�����敪
021879                 MOVE ZERO           TO ��Q�|�����Ώۋ敪
021880                 MOVE ZERO           TO ��Q�|���������a��
021881                 MOVE ZERO           TO ��Q�|���������N  
021882                 MOVE ZERO           TO ��Q�|����������  
021883                 MOVE ZERO           TO ��Q�|���������敪
021884                 MOVE ZERO           TO ��Q�|���������Ώۋ敪
021885                 MOVE ZERO           TO ��Q�|���@���@�敪
021886                 MOVE ZERO           TO ��Q�|�Љ�Ҋ��Ҕԍ�
021887                 MOVE ZERO           TO ��Q�|�_���{�̏��ҕ����敪
021888                 MOVE ZERO           TO ��Q�|�_���{�̂܂Ƃߋ敪
021889                 MOVE ZERO           TO ��Q�|�_���������ҕ����敪
021890                 MOVE ZERO           TO ��Q�|�_���������Z����Ώۋ敪
021891                 MOVE ZERO           TO ��Q�|�_���������Z����\����Ώۋ敪
021892                 MOVE ZERO           TO ��Q�|�_���{�̎}�Ԃ܂Ƃߋ敪
021893*
021896             WRITE ��Q�|���R�[�h
021897             INVALID KEY
021900                 MOVE NC"��Q" TO �t�@�C�����v
021910                 PERFORM �G���[�\��
021920             END-WRITE
021930         END-READ
021940       END-READ
021941     END-IF.
021950*
021960*================================================================*
021970 ��f�ҏ��Q�e�Ǎ� SECTION.
021980*
021990     READ ��f�ҏ��Q�e NEXT
022000     AT END
022010         MOVE "YES" TO �I���t���O
022020     END-READ.
022030*================================================================*
022040 ���ۏ��e�����폜 SECTION.
022050*
022060     MOVE �A���ҍ폜�|���҃R�[�h   TO ���ہ|���҃R�[�h.
022070     MOVE �A���ҍ폜�|�{�p�a��N�� TO ���ہ|�{�p�a��N��.
022080*
022090     READ ���ۏ��e
022100     NOT INVALID KEY
022110         DELETE ���ۏ��e
022120         INVALID KEY
022130             MOVE NC"����" TO �t�@�C�����v
022140             PERFORM �G���[�\���c
022150         END-DELETE
022160     END-READ.
022170*
022180*================================================================*
022190 ���ۏ��e�S�폜 SECTION.
022200*
022210     MOVE �A���ҍ폜�|���҃R�[�h   TO ���ہ|���҃R�[�h.
022220     MOVE ZERO                     TO ���ہ|�{�p�a��N��.
022230*
022240     START ���ۏ��e KEY IS >= ���ہ|���҃R�[�h
022250                                ���ہ|�{�p�a��N��
022260     END-START.
022270*
022280     IF ��ԃL�[ = "00"
022290         MOVE SPACE            TO �I���t���O
022300         PERFORM ���ۏ��e�Ǎ�
022310         PERFORM UNTIL (���ہ|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
022320                       (�I���t���O             = "YES")
022330             DELETE ���ۏ��e
022340             INVALID KEY
022350                 MOVE NC"����" TO �t�@�C�����v
022360                 PERFORM �G���[�\���c
022370             END-DELETE
022380             PERFORM ���ۏ��e�Ǎ�
022390         END-PERFORM
022400     END-IF.
022410*
022420*================================================================*
022430 ���ۏ��e�ύX SECTION.
022440*
022450     MOVE �A���ҍ폜�|���҃R�[�h   TO ���ہ|���҃R�[�h.
022460     MOVE ZERO                     TO ���ہ|�{�p�a��N��.
022470*
022480     START ���ۏ��e KEY IS >= ���ہ|���҃R�[�h
022490                                ���ہ|�{�p�a��N��
022500     END-START.
022510*
022520     IF ��ԃL�[ = "00"
022530         MOVE SPACE TO �I���t���O
022540         PERFORM ���ۏ��e�Ǎ�
022550         PERFORM UNTIL (���ہ|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
022560                       (�I���t���O             = "YES")
022570             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO ���ہ|���҃R�[�h
022580             WRITE ���ہ|���R�[�h
022590             INVALID KEY
022600                 MOVE NC"����" TO �t�@�C�����v
022610                 PERFORM �G���[�\��
022620             END-WRITE
022630             MOVE  �A���ҍ폜�|���҃R�[�h TO ���ہ|���҃R�[�h
022640             DELETE ���ۏ��e
022650             INVALID KEY
022660                 MOVE NC"����" TO �t�@�C�����v
022670                 PERFORM �G���[�\���c
022680             END-DELETE
022690             PERFORM ���ۏ��e�Ǎ�
022700         END-PERFORM
022710     END-IF.
022720*
022730*================================================================*
022740 ���ۏ��e�}�ԍ쐬 SECTION.
022750*
022760     MOVE �A���ҍ폜�|���҃R�[�h   TO ���ہ|���҃R�[�h.
022770     MOVE �A���ҍ폜�|�{�p�a��N�� TO ���ہ|�{�p�a��N��.
022780*
022790     READ ���ۏ��e
022800     NOT INVALID KEY
022810         MOVE  �A���ҍ폜�|�ύX�}�� TO  ���ہ|�}��
022820         WRITE ���ہ|���R�[�h
022830         INVALID KEY
022840             MOVE NC"����" TO �t�@�C�����v
022850             PERFORM �G���[�\��
022860         END-WRITE
022870     END-READ.
022880*
022890*================================================================*
022900 ���ۏ��e�Ǎ� SECTION.
022910*
022920     READ ���ۏ��e NEXT
022930     AT END
022940         MOVE "YES" TO �I���t���O
022950     END-READ.
022960*================================================================*
022970 �����ӏ��e�����폜 SECTION.
022980*
022990     MOVE �A���ҍ폜�|���҃R�[�h   TO �����|���҃R�[�h.
023000     MOVE �A���ҍ폜�|�{�p�a��N�� TO �����|�{�p�a��N��.
023010*
023020     READ �����ӏ��e
023030     NOT INVALID KEY
023040         DELETE �����ӏ��e
023050         INVALID KEY
023060             MOVE NC"����" TO �t�@�C�����v
023070             PERFORM �G���[�\���c
023080         END-DELETE
023090     END-READ.
023100*
023110*================================================================*
023120 �����ӏ��e�S�폜 SECTION.
023130*
023140     MOVE �A���ҍ폜�|���҃R�[�h   TO �����|���҃R�[�h.
023150     MOVE ZERO                     TO �����|�{�p�a��N��.
023160*
023170     START �����ӏ��e KEY IS >= �����|���҃R�[�h
023180                                  �����|�{�p�a��N��
023190     END-START.
023200*
023210     IF ��ԃL�[ = "00"
023220         MOVE SPACE            TO �I���t���O
023230         PERFORM �����ӏ��e�Ǎ�
023240         PERFORM UNTIL (�����|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h) OR
023250                       (�I���t���O           = "YES")
023260             DELETE �����ӏ��e
023270             INVALID KEY
023280                 MOVE NC"����" TO �t�@�C�����v
023290                 PERFORM �G���[�\���c
023300             END-DELETE
023310             PERFORM �����ӏ��e�Ǎ�
023320         END-PERFORM
023330     END-IF.
023340*
023350*================================================================*
023360 �����ӏ��e�ύX SECTION.
023370*
023380     MOVE �A���ҍ폜�|���҃R�[�h   TO �����|���҃R�[�h.
023390     MOVE ZERO                     TO �����|�{�p�a��N��.
023400*
023410     START �����ӏ��e KEY IS >= �����|���҃R�[�h
023420                                  �����|�{�p�a��N��
023430     END-START.
023440*
023450     IF ��ԃL�[ = "00"
023460         MOVE SPACE TO �I���t���O
023470         PERFORM �����ӏ��e�Ǎ�
023480         PERFORM UNTIL (�����|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h) OR
023490                       (�I���t���O           = "YES")
023500             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �����|���҃R�[�h
023510             WRITE �����|���R�[�h
023520             INVALID KEY
023530                 MOVE NC"����" TO �t�@�C�����v
023540                 PERFORM �G���[�\��
023550             END-WRITE
023560             MOVE  �A���ҍ폜�|���҃R�[�h TO �����|���҃R�[�h
023570             DELETE �����ӏ��e
023580             INVALID KEY
023590                 MOVE NC"����" TO �t�@�C�����v
023600                 PERFORM �G���[�\���c
023610             END-DELETE
023620             PERFORM �����ӏ��e�Ǎ�
023630         END-PERFORM
023640     END-IF.
023650*
023660*================================================================*
023670 �����ӏ��e�}�ԍ쐬 SECTION.
023680*
023690     MOVE �A���ҍ폜�|���҃R�[�h   TO �����|���҃R�[�h.
023700     MOVE �A���ҍ폜�|�{�p�a��N�� TO �����|�{�p�a��N��.
023710*
023720     READ �����ӏ��e
023730     NOT INVALID KEY
023740         MOVE  �A���ҍ폜�|�ύX�}�� TO  �����|�}��
023750         WRITE �����|���R�[�h
023760         INVALID KEY
023770             MOVE NC"����" TO �t�@�C�����v
023780             PERFORM �G���[�\��
023790         END-WRITE
023800     END-READ.
023810*
023820*================================================================*
023830 �����ӏ��e�Ǎ� SECTION.
023840*
023850     READ �����ӏ��e NEXT
023860     AT END
023870         MOVE "YES" TO �I���t���O
023880     END-READ.
023890*================================================================*
023900 �J�Џ��e�����폜 SECTION.
023910*
024132     MOVE �A���ҍ폜�|���҃R�[�h   TO �J�Ё|���҃R�[�h.
024133     MOVE �A���ҍ폜�|�{�p�a��N�� TO �J�Ё|�{�p�a��N��.
024134*
024135     READ �J�Џ��e
024136     NOT INVALID KEY
024137         DELETE �J�Џ��e
024138         INVALID KEY
024139             MOVE NC"�J��" TO �t�@�C�����v
024140             PERFORM �G���[�\���c
024141         END-DELETE
024142     END-READ.
024143*
024145*================================================================*
024150 �J�Џ��e�S�폜 SECTION.
024160*
024281     MOVE �A���ҍ폜�|���҃R�[�h   TO �J�Ё|���҃R�[�h.
024282     MOVE ZERO                     TO �J�Ё|�{�p�a��N��.
024283*
024284     START �J�Џ��e KEY IS >= �J�Ё|���҃R�[�h
024285                                �J�Ё|�{�p�a��N��
024286     END-START.
024287*
024288     IF ��ԃL�[ = "00"
024289         MOVE SPACE            TO �I���t���O
024290         PERFORM �J�Џ��e�Ǎ�
024291         PERFORM UNTIL (�J�Ё|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
024292                       (�I���t���O             = "YES")
024293             DELETE �J�Џ��e
024294             INVALID KEY
024295                 MOVE NC"�J��" TO �t�@�C�����v
024296                 PERFORM �G���[�\���c
024297             END-DELETE
024298             PERFORM �J�Џ��e�Ǎ�
024299         END-PERFORM
024300     END-IF.
024301*
024305*================================================================*
024306 �J�Џ��e�ύX SECTION.
024310*
024320     MOVE �A���ҍ폜�|���҃R�[�h   TO �J�Ё|���҃R�[�h.
024330     MOVE ZERO                     TO �J�Ё|�{�p�a��N��.
024340*
024350     START �J�Џ��e KEY IS >= �J�Ё|���҃R�[�h
024360                                �J�Ё|�{�p�a��N��
024370     END-START.
024380*
024390     IF ��ԃL�[ = "00"
024400         MOVE SPACE TO �I���t���O
024410         PERFORM �J�Џ��e�Ǎ�
024420         PERFORM UNTIL (�J�Ё|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h) OR
024430                       (�I���t���O           = "YES")
024440             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �J�Ё|���҃R�[�h
024450             WRITE �J�Ё|���R�[�h
024460             INVALID KEY
024470                 MOVE NC"�J��" TO �t�@�C�����v
024480                 PERFORM �G���[�\��
024490             END-WRITE
024500             MOVE  �A���ҍ폜�|���҃R�[�h TO �J�Ё|���҃R�[�h
024510             DELETE �J�Џ��e
024520             INVALID KEY
024530                 MOVE NC"�J��" TO �t�@�C�����v
024540                 PERFORM �G���[�\���c
024550             END-DELETE
024560             PERFORM �J�Џ��e�Ǎ�
024570         END-PERFORM
024580     END-IF.
024590*
024600*================================================================*
024610 �J�Џ��e�}�ԍ쐬 SECTION.
024620*
024630     MOVE �A���ҍ폜�|���҃R�[�h   TO �J�Ё|���҃R�[�h.
024640     MOVE �A���ҍ폜�|�{�p�a��N�� TO �J�Ё|�{�p�a��N��.
024650*
024660     READ �J�Џ��e
024670     NOT INVALID KEY
024680         MOVE  �A���ҍ폜�|�ύX�}�� TO  �J�Ё|�}��
024690         WRITE �J�Ё|���R�[�h
024700         INVALID KEY
024710             MOVE NC"�J��" TO �t�@�C�����v
024720             PERFORM �G���[�\��
024730         END-WRITE
024740     END-READ.
024750*
024760*================================================================*
024770 �J�Џ��e�Ǎ� SECTION.
024780*
024790     READ �J�Џ��e NEXT
024800     AT END
024810         MOVE "YES" TO �I���t���O
024820     END-READ.
024830*================================================================*
024840 ��v�̎��e�����폜 SECTION.
024850*
024860     MOVE ZERO            TO �́|�{�p�敪.
024870     MOVE ZERO            TO �́|��v�̎��敪.
024880     MOVE ZERO            TO �́|���Ҕԍ�.
024890     MOVE SPACE           TO �́|�}��.
024900     MOVE ZERO            TO �́|�{�p�a��N����.
024910*
024920     START ��v�̎��e KEY IS >= �́|�{�p�敪
024930                                �́|��v�̎��敪
024940                                �́|�{�p�a��N����
024950                                �́|���҃R�[�h
024960     END-START.
024970*
024980     IF ��ԃL�[ = "00"
024990         MOVE SPACE            TO �I���t���O
025000         PERFORM ��v�̎��e�Ǎ�
025010         PERFORM UNTIL (�I���t���O = "YES")
025020             IF (�́|���҃R�[�h    = �A���ҍ폜�|���҃R�[�h) AND
025030                (�́|�{�p�a��N��  = �A���ҍ폜�|�{�p�a��N��)
025040                 DELETE ��v�̎��e
025050                 INVALID KEY
025060                     MOVE NC"�̎�" TO �t�@�C�����v
025070                     PERFORM �G���[�\���c
025080                 END-DELETE
025090             END-IF
025100             PERFORM ��v�̎��e�Ǎ�
025110         END-PERFORM
025120     END-IF.
025130*
025140*================================================================*
025150 ��v�̎��e�S�폜 SECTION.
025160*
025170     MOVE ZERO            TO �́|�{�p�敪.
025180     MOVE ZERO            TO �́|��v�̎��敪.
025190     MOVE ZERO            TO �́|���Ҕԍ�.
025200     MOVE SPACE           TO �́|�}��.
025210     MOVE ZERO            TO �́|�{�p�a��N����.
025220*
025230     START ��v�̎��e KEY IS >= �́|�{�p�敪
025240                                �́|��v�̎��敪
025250                                �́|�{�p�a��N����
025260                                �́|���҃R�[�h
025270     END-START.
025280*
025290     IF ��ԃL�[ = "00"
025300         MOVE SPACE            TO �I���t���O
025310         PERFORM ��v�̎��e�Ǎ�
025320         PERFORM UNTIL (�I���t���O = "YES")
025330             IF (�́|���҃R�[�h    = �A���ҍ폜�|���҃R�[�h)
025340                 DELETE ��v�̎��e
025350                 INVALID KEY
025360                     MOVE NC"�̎�" TO �t�@�C�����v
025370                     PERFORM �G���[�\���c
025380                 END-DELETE
025390             END-IF
025400             PERFORM ��v�̎��e�Ǎ�
025410         END-PERFORM
025420     END-IF.
025430*
025440*================================================================*
025450 ��v�̎��e�ύX SECTION.
025460*
025470     MOVE ZERO            TO �́|�{�p�敪.
025480     MOVE ZERO            TO �́|��v�̎��敪.
025490     MOVE ZERO            TO �́|���Ҕԍ�.
025500     MOVE SPACE           TO �́|�}��.
025510     MOVE ZERO            TO �́|�{�p�a��N����.
025520*
025530     START ��v�̎��e KEY IS >= �́|�{�p�敪
025540                                �́|��v�̎��敪
025550                                �́|�{�p�a��N����
025560                                �́|���҃R�[�h
025570     END-START.
025580*
025590     IF ��ԃL�[ = "00"
025600         MOVE SPACE            TO �I���t���O
025610         PERFORM ��v�̎��e�Ǎ�
025620         PERFORM UNTIL (�I���t���O = "YES")
025630             IF (�́|���҃R�[�h    = �A���ҍ폜�|���҃R�[�h)
025640                 MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �́|���҃R�[�h
025650                 WRITE �́|���R�[�h
025660                 INVALID KEY
025670                     MOVE NC"�̎�" TO �t�@�C�����v
025680                     PERFORM �G���[�\��
025690                 END-WRITE
025700                 MOVE  �A���ҍ폜�|���҃R�[�h TO �́|���҃R�[�h
025710                 DELETE ��v�̎��e
025720                 INVALID KEY
025730                     MOVE NC"�̎�" TO �t�@�C�����v
025740                     PERFORM �G���[�\���c
025750                 END-DELETE
025760             END-IF
025770             PERFORM ��v�̎��e�Ǎ�
025780         END-PERFORM
025790     END-IF.
025800*
025810*================================================================*
025820 ��v�̎��e�}�ԍ쐬 SECTION.
025830*
025840     MOVE ZERO            TO �́|�{�p�敪.
025850     MOVE ZERO            TO �́|��v�̎��敪.
025860     MOVE ZERO            TO �́|���Ҕԍ�.
025870     MOVE SPACE           TO �́|�}��.
025880     MOVE ZERO            TO �́|�{�p�a��N����.
025890*
025900     START ��v�̎��e KEY IS >= �́|�{�p�敪
025910                                �́|��v�̎��敪
025920                                �́|�{�p�a��N����
025930                                �́|���҃R�[�h
025940     END-START.
025950*
025960     IF ��ԃL�[ = "00"
025970         MOVE SPACE            TO �I���t���O
025980         PERFORM ��v�̎��e�Ǎ�
025990         PERFORM UNTIL (�I���t���O = "YES")
026000*/�ύX���ȍ~�̓R���o�[�g���Ȃ�
026010             IF (�́|���҃R�[�h    = �A���ҍ폜�|���҃R�[�h) AND
026020                (�́|�{�p�a��N��  = �A���ҍ폜�|�{�p�a��N��) AND
026030                (�́|�{�p��        < �A���Ҏ}�ԁ|�ύX��)
026040                 MOVE �A���ҍ폜�|�ύX�}�� TO �́|�}��
026050                 WRITE �́|���R�[�h
026060                 INVALID KEY
026070                     MOVE NC"�̎�" TO �t�@�C�����v
026080                     PERFORM �G���[�\��
026090                 END-WRITE
026100                 MOVE �A���ҍ폜�|�}��  TO �́|�}��
026110                 DELETE ��v�̎��e
026120                 INVALID KEY
026130                     MOVE NC"�̎�" TO �t�@�C�����v
026140                     PERFORM �G���[�\���c
026150                 END-DELETE
026160             END-IF
026170             PERFORM ��v�̎��e�Ǎ�
026180         END-PERFORM
026190     END-IF.
026200*
026210*================================================================*
026220 ��v�̎��e�Ǎ� SECTION.
026230*
026240     READ ��v�̎��e NEXT
026250     AT END
026260         MOVE "YES" TO �I���t���O
026270     END-READ.
026280*================================================================*
026282*================================================================*
026283 �c�l�L�^�e�S�폜 SECTION.
026284*
026303     MOVE �A���ҍ폜�|���҃R�[�h   TO �c�l�|���҃R�[�h.
026304     MOVE ZERO                     TO �c�l�|���s�a��N����.
026305     MOVE ZERO                     TO �c�l�|���s�}��.
026306*
026307     START �c�l�L�^�e KEY IS >= �c�l�|���҃R�[�h
026308                                �c�l�|���s�a��N����
026309                                �c�l�|���s�}��
026310     END-START.
026311*
026312     IF ��ԃL�[ = "00"
026313         MOVE SPACE            TO �I���t���O
026314         PERFORM �c�l�L�^�e�Ǎ�
026315         PERFORM UNTIL (�c�l�|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
026316                       (�I���t���O             = "YES")
026317             DELETE �c�l�L�^�e
026318             INVALID KEY
026319                 MOVE NC"�c�l�L�^" TO �t�@�C�����v
026320                 PERFORM �G���[�\���c
026321             END-DELETE
026322             PERFORM �c�l�L�^�e�Ǎ�
026323         END-PERFORM
026324     END-IF.
026325*
026326*================================================================*
026327 �c�l�L�^�e�ύX SECTION.
026328*
026337     MOVE �A���ҍ폜�|���҃R�[�h   TO �c�l�|���҃R�[�h.
026338     MOVE ZERO                     TO �c�l�|���s�a��N����.
026339     MOVE ZERO                     TO �c�l�|���s�}��.
026340*
026341     START �c�l�L�^�e KEY IS >= �c�l�|���҃R�[�h
026342                                �c�l�|���s�a��N����
026343                                �c�l�|���s�}��
026344     END-START.
026345*
026346     IF ��ԃL�[ = "00"
026347         MOVE SPACE TO �I���t���O
026348         PERFORM �c�l�L�^�e�Ǎ�
026349         PERFORM UNTIL (�c�l�|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
026350                       (�I���t���O             = "YES")
026357             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �c�l�|���҃R�[�h
026358             WRITE �c�l�|���R�[�h
026359             INVALID KEY
026360                 MOVE NC"�c�l�L�^" TO �t�@�C�����v
026361                 PERFORM �G���[�\��
026362             END-WRITE
026363             MOVE  �A���ҍ폜�|���҃R�[�h TO �c�l�|���҃R�[�h
026364             DELETE �c�l�L�^�e
026365             INVALID KEY
026366                 MOVE NC"�c�l�L�^" TO �t�@�C�����v
026367                 PERFORM �G���[�\���c
026368             END-DELETE
026369             PERFORM �c�l�L�^�e�Ǎ�
026370         END-PERFORM
026371     END-IF.
026372*
026373*================================================================*
026374 �c�l�L�^�e�}�ԍ쐬 SECTION.
026375*
026376     MOVE �A���ҍ폜�|���҃R�[�h   TO �c�l�|���҃R�[�h.
026377     MOVE �A���ҍ폜�|�{�p�a��N�� TO �c�l�|���s�a��N��.
026378     MOVE 1                        TO �c�l�|���s��.
026379     MOVE ZERO                     TO �c�l�|���s�}��.
026380*
026391     START �c�l�L�^�e KEY IS >= �c�l�|���҃R�[�h
026392                                �c�l�|���s�a��N����
026393                                �c�l�|���s�}��
026394     END-START.
026397*
026398     IF ��ԃL�[ = "00"
026399         MOVE SPACE TO �I���t���O
026400         PERFORM �c�l�L�^�e�Ǎ�
026401*/�ύX���ȍ~�̓R���o�[�g���Ȃ�
026402         PERFORM UNTIL (�c�l�|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
026403                       (�c�l�|���s�a��N�� NOT = �A���ҍ폜�|�{�p�a��N��) OR
026404                       (�c�l�|���s��          >= �A���Ҏ}�ԁ|�ύX��    ) OR
026405                       (�I���t���O             = "YES")
026406             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �c�l�|���҃R�[�h
026407             WRITE �c�l�|���R�[�h
026408             INVALID KEY
026409                 MOVE NC"�c�l�L�^" TO �t�@�C�����v
026410                 PERFORM �G���[�\��
026411             END-WRITE
026412             MOVE  �A���ҍ폜�|���҃R�[�h TO �c�l�|���҃R�[�h
026413             DELETE �c�l�L�^�e
026414             INVALID KEY
026415                 MOVE NC"�c�l�L�^" TO �t�@�C�����v
026416                 PERFORM �G���[�\���c
026417             END-DELETE
026418             PERFORM �c�l�L�^�e�Ǎ�
026419         END-PERFORM
026420     END-IF.
026421*
026422*================================================================*
026423 �c�l�L�^�e�Ǎ� SECTION.
026424*
026425     READ �c�l�L�^�e NEXT
026426     AT END
026427         MOVE "YES" TO �I���t���O
026428     END-READ.
026429*
026430*================================================================*
026431 �{�ݎ�f�҃}�X�^�S�폜 SECTION.
026432*
026433     MOVE �A���ҍ폜�|���҃R�[�h   TO �{�ݎ�|���҃R�[�h.
026434     MOVE ZERO                     TO �{�ݎ�|�{�݃R�[�h.
026437*
026438     START �{�ݎ�f�҃}�X�^ KEY IS >= �{�ݎ�|���҃R�[�h
026439                                      �{�ݎ�|�{�݃R�[�h
026440     END-START.
026441*
026442     IF ��ԃL�[ = "00"
026443         MOVE SPACE            TO �I���t���O
026444         PERFORM �{�ݎ�f�҃}�X�^�Ǎ�
026445         PERFORM UNTIL (�{�ݎ�|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h)   OR
026446                       (�I���t���O             = "YES")
026447             DELETE �{�ݎ�f�҃}�X�^
026448             INVALID KEY
026449                 MOVE NC"�{�ݎ�f�҃}�X�^" TO �t�@�C�����v
026450                 PERFORM �G���[�\���c
026451             END-DELETE
026452             PERFORM �{�ݎ�f�҃}�X�^�Ǎ�
026453         END-PERFORM
026454     END-IF.
026455*
026456*================================================================*
026457 �{�ݎ�f�҃}�X�^�ύX SECTION.
026458*
026461     MOVE �A���ҍ폜�|���҃R�[�h   TO �{�ݎ�|���҃R�[�h.
026462     MOVE ZERO                     TO �{�ݎ�|�{�݃R�[�h.
026463*
026467     START �{�ݎ�f�҃}�X�^ KEY IS >= �{�ݎ�|���҃R�[�h
026468                                      �{�ݎ�|�{�݃R�[�h
026469     END-START.
026470*
026471     IF ��ԃL�[ = "00"
026472         MOVE SPACE TO �I���t���O
026473         PERFORM �{�ݎ�f�҃}�X�^�Ǎ�
026474         PERFORM UNTIL (�{�ݎ�|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h) OR
026475                       (�I���t���O           = "YES")
026476             MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �{�ݎ�|���҃R�[�h
026477             WRITE �{�ݎ�|���R�[�h
026478             INVALID KEY
026479                 MOVE NC"�{�ݎ�f�҃}�X�^" TO �t�@�C�����v
026480                 PERFORM �G���[�\��
026481             END-WRITE
026482             MOVE  �A���ҍ폜�|���҃R�[�h TO �{�ݎ�|���҃R�[�h
026483             DELETE �{�ݎ�f�҃}�X�^
026484             INVALID KEY
026485                 MOVE NC"�{�ݎ�f�҃}�X�^" TO �t�@�C�����v
026486                 PERFORM �G���[�\���c
026487             END-DELETE
026488             PERFORM �{�ݎ�f�҃}�X�^�Ǎ�
026489         END-PERFORM
026490     END-IF.
026491*
026501*================================================================*
026502 �{�ݎ�f�҃}�X�^�Ǎ� SECTION.
026503*
026504     READ �{�ݎ�f�҃}�X�^ NEXT
026505     AT END
026506         MOVE "YES" TO �I���t���O
026507     END-READ.
026508*================================================================*
026509*================================================================*
026510*================================================================*
026511 �g���Z�v�g�e�����폜 SECTION.
026512*
026513     MOVE �{�p�敪�v                TO  �g���Z�|�{�p�敪.
026514     MOVE �A���ҍ폜�|���҃R�[�h    TO  �g���Z�|���҃R�[�h.
026515     MOVE �A���ҍ폜�|�{�p�a��N��  TO  �g���Z�|�{�p�a��N��.
026516     MOVE ZERO                      TO  �g���Z�|���Z���.
026517*
026518     START �g���Z�v�g�e KEY IS >= �g���Z�|�{�p�敪
026519                                  �g���Z�|���҃R�[�h
026520                                  �g���Z�|�{�p�a��N��
026521                                  �g���Z�|���Z���
026522     END-START.
026523*
026524     IF ��ԃL�[ = "00"
026525            MOVE SPACE TO �I���t���O
026526            PERFORM �g���Z�v�g�e�Ǎ�
026527            PERFORM UNTIL ( �g���Z�|�{�p�敪     NOT = �{�p�敪�v               ) OR
026528                          ( �g���Z�|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h ) OR
026529                          ( �g���Z�|�{�p�a��N�� NOT = �A���ҍ폜�|�{�p�a��N��   ) OR
026530                          ( �I���t���O           NOT = SPACE )
026531*
                      PERFORM �g���Z�v�g�ڍׂe�폜
026532                DELETE �g���Z�v�g�e
026533                INVALID KEY
026534                   MOVE NC"�g���Z�v�g�e" TO �t�@�C�����v
026540                   PERFORM �G���[�\���c
026550                END-DELETE
026560*
026570                PERFORM �g���Z�v�g�e�Ǎ�
026580            END-PERFORM
026590     END-IF.
026600*
026610*================================================================*
026620 �g���Z�v�g�e�S�폜 SECTION.
026630*
026640     MOVE �{�p�敪�v             TO  �g���Z�|�{�p�敪.
026650     MOVE �A���ҍ폜�|���҃R�[�h TO  �g���Z�|���҃R�[�h.
026660     MOVE ZERO                   TO  �g���Z�|�{�p�a��N��.
026670     MOVE ZERO                   TO  �g���Z�|���Z���.
026680*
026690     START �g���Z�v�g�e KEY IS >= �g���Z�|�{�p�敪
026700                                  �g���Z�|���҃R�[�h
026710                                  �g���Z�|�{�p�a��N��
026720                                  �g���Z�|���Z���
026730     END-START.
026740*
026750     IF ��ԃL�[ = "00"
026760            MOVE SPACE TO �I���t���O
026770            PERFORM �g���Z�v�g�e�Ǎ�
026780            PERFORM UNTIL ( �g���Z�|�{�p�敪   NOT = �{�p�敪�v               ) OR
026790                          ( �g���Z�|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h ) OR
026800                          ( �I���t���O         NOT = SPACE )
026810*
                      PERFORM �g���Z�v�g�ڍׂe�폜
026820                DELETE �g���Z�v�g�e
026830                INVALID KEY
026840                   MOVE NC"�g���Z�v�g�e" TO �t�@�C�����v
026850                   PERFORM �G���[�\���c
026860                END-DELETE
026870*
026880                PERFORM �g���Z�v�g�e�Ǎ�
026890            END-PERFORM
026900     END-IF.
026910*
026920*================================================================*
026930 �g���Z�v�g�e�ύX SECTION.
026940*
026950     MOVE �{�p�敪�v             TO  �g���Z�|�{�p�敪.
026960     MOVE �A���ҍ폜�|���҃R�[�h TO  �g���Z�|���҃R�[�h.
026970     MOVE ZERO                   TO  �g���Z�|�{�p�a��N��.
026980     MOVE ZERO                   TO  �g���Z�|���Z���.
026990*
027000     START �g���Z�v�g�e KEY IS >= �g���Z�|�{�p�敪
027010                                  �g���Z�|���҃R�[�h
027020                                  �g���Z�|�{�p�a��N��
027030                                  �g���Z�|���Z���
027040     END-START.
027050*
027060     IF ��ԃL�[ = "00"
027070            MOVE SPACE TO �I���t���O
027080            PERFORM �g���Z�v�g�e�Ǎ�
027090            PERFORM UNTIL ( �g���Z�|�{�p�敪   NOT = �{�p�敪�v               ) OR
027100                          ( �g���Z�|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h ) OR
027110                          ( �I���t���O         NOT = SPACE )
027120*
027130*------------    / �V���R�[�h�̍쐬 / -------------------------------*
                      PERFORM �g���Z�v�g�ڍׂe�쐬
027140                MOVE �A���ҍ폜�|�ύX���҃R�[�h  TO �g���Z�|���҃R�[�h
027150                WRITE �g���Z�|���R�[�h
027160                INVALID KEY
027170                   MOVE NC"�g���Z�v�g�e" TO �t�@�C�����v
027180                   PERFORM �G���[�\��
027190                END-WRITE
027200*
027210*------------   / ���R�[�h�̍폜 / -------------------------------*
027220                MOVE �A���ҍ폜�|���҃R�[�h  TO �g���Z�|���҃R�[�h
                      PERFORM �g���Z�v�g�ڍׂe�폜
027230                DELETE �g���Z�v�g�e
027240                INVALID KEY
027250                   MOVE NC"�g���Z�v�g�e" TO �t�@�C�����v
027260                   PERFORM �G���[�\���c
027270                END-DELETE
027280*
027290                PERFORM �g���Z�v�g�e�Ǎ�
027300            END-PERFORM
027310     END-IF.
027320*
027330*================================================================*
027340 �g���Z�v�g�e�Ǎ� SECTION.
027350*
027360     READ �g���Z�v�g�e NEXT
027370     AT END
027380         MOVE "YES"  TO �I���t���O
027390     END-READ.
027400*
027410*================================================================*
027420 �g���v�f�[�^�e�����폜 SECTION.
027430*
027440     MOVE �{�p�敪�v                TO  �g���|�{�p�敪.
027450     MOVE �A���ҍ폜�|���҃R�[�h    TO  �g���|���҃R�[�h.
027460     MOVE �A���ҍ폜�|�{�p�a��N��  TO  �g���|�{�p�a��N��.
027470     MOVE ZERO                      TO  �g���|�{�p��.
027480*
027490     START �g���v�f�[�^�e KEY IS >= �g���|�{�p�敪
027500                                    �g���|���҃R�[�h
027510                                    �g���|�{�p�a��N����
027520     END-START.
027530*
027540     IF ��ԃL�[ = "00"
027550         MOVE SPACE TO �I���t���O
027560         PERFORM �g���v�f�[�^�e�Ǎ�
027570         PERFORM UNTIL ( �g���|�{�p�敪     NOT = �{�p�敪�v               ) OR
027580                       ( �g���|���҃R�[�h   NOT = �A���ҍ폜�|���҃R�[�h ) OR
027590                       ( �g���|�{�p�a��N�� NOT = �A���ҍ폜�|�{�p�a��N��   ) OR
027600                       ( �I���t���O         NOT = SPACE )
027610*
027620                DELETE �g���v�f�[�^�e
027630                IF ��ԃL�[ NOT = "00"
027640                   MOVE NC"�g���v�f�[�^�e" TO �t�@�C�����v
027650                   PERFORM �G���[�\���c
027660                END-IF
027670*
027680                PERFORM �g���v�f�[�^�e�Ǎ�
027690         END-PERFORM
027700     END-IF.
027710*
027720*================================================================*
027730 �g���v�f�[�^�e�S�폜 SECTION.
027740*
027750     MOVE �{�p�敪�v              TO  �g���|�{�p�敪.
027760     MOVE �A���ҍ폜�|���҃R�[�h  TO  �g���|���҃R�[�h.
027770     MOVE ZERO                    TO  �g���|�{�p�a��N����.
027780*
027790     START �g���v�f�[�^�e KEY IS >= �g���|�{�p�敪
027800                                    �g���|���҃R�[�h
027810                                    �g���|�{�p�a��N����
027820     END-START.
027830*
027840     IF ��ԃL�[ = "00"
027850         MOVE SPACE TO �I���t���O
027860         PERFORM �g���v�f�[�^�e�Ǎ�
027870         PERFORM UNTIL ( �g���|�{�p�敪   NOT = �{�p�敪�v               ) OR
027880                       ( �g���|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h ) OR
027890                       ( �I���t���O       NOT = SPACE )
027900*
027910                DELETE �g���v�f�[�^�e
027920                IF ��ԃL�[ NOT = "00"
027930                   MOVE NC"�g���v�f�[�^�e" TO �t�@�C�����v
027940                   PERFORM �G���[�\���c
027950                END-IF
027960*
027970                PERFORM �g���v�f�[�^�e�Ǎ�
027980         END-PERFORM
027990     END-IF.
028000*
028010*================================================================*
028020 �g���v�f�[�^�e�ύX SECTION.
028030*
028040     MOVE �{�p�敪�v              TO  �g���|�{�p�敪.
028050     MOVE �A���ҍ폜�|���҃R�[�h  TO  �g���|���҃R�[�h.
028060     MOVE ZERO                    TO  �g���|�{�p�a��N����.
028070*
028080     START �g���v�f�[�^�e KEY IS >= �g���|�{�p�敪
028090                                    �g���|���҃R�[�h
028100                                    �g���|�{�p�a��N����
028110     END-START.
028120*
028130     IF ��ԃL�[ = "00"
028140         MOVE SPACE TO �I���t���O
028150         PERFORM �g���v�f�[�^�e�Ǎ�
028160         PERFORM UNTIL ( �g���|�{�p�敪   NOT = �{�p�敪�v               ) OR
028170                       ( �g���|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h ) OR
028180                       ( �I���t���O       NOT = SPACE )
028190*
028200*------------    / �V���R�[�h�̍쐬 / -------------------------------*
028210                MOVE �A���ҍ폜�|�ύX���҃R�[�h  TO �g���|���҃R�[�h
028220                COMPUTE �g���|�o�^��  = �g���|�o�^�� + 0.1
028230                WRITE �g���|���R�[�h
028240                IF ��ԃL�[ NOT = "00"
028250                   MOVE NC"�g���v�f�[�^�e" TO �t�@�C�����v
028260                   PERFORM �G���[�\��
028270                END-IF
028280*
028290*------------   / ���R�[�h�̍폜 / -------------------------------*
028300                MOVE �A���ҍ폜�|���҃R�[�h  TO �g���|���҃R�[�h
028310                DELETE �g���v�f�[�^�e
028320                IF ��ԃL�[ NOT = "00"
028330                   MOVE NC"�g���v�f�[�^�e" TO �t�@�C�����v
028340                   PERFORM �G���[�\���c
028350                END-IF
028360*
028370                PERFORM �g���v�f�[�^�e�Ǎ�
028380         END-PERFORM
028390     END-IF.
028400*
028410*================================================================*
028420 �g���v�f�[�^�e�Ǎ� SECTION.
028430*
028440     READ �g���v�f�[�^�e NEXT
028450     AT END
028460         MOVE "YES"  TO �I���t���O
028470     END-READ.
028480*
028490*================================================================*
028500 �g�����f�[�^�e�S�폜 SECTION.
028510*
028511* �}�Ԃ���R�[�h�͍폜����Ȃ��B
028520     MOVE �{�p�敪�v              TO  �g���|�{�p�敪.
028522     MOVE �A���ҍ폜�|���҃R�[�h  TO  �g���|���҃R�[�h.
028540     MOVE ZERO                    TO  �g���|��L�[.
028550*
028560     START �g�����f�[�^�e KEY IS >= �g���|�{�p�敪
028570                                    �g���|���҃R�[�h
028580                                    �g���|��L�[
028590     END-START.
028600*
028610     IF ��ԃL�[ = "00"
028620         MOVE SPACE TO �I���t���O
028630         PERFORM �g�����f�[�^�e�Ǎ�
028640         PERFORM UNTIL ( �g���|�{�p�敪    NOT = �{�p�敪�v               ) OR
028650                       ( �g���|���҃R�[�h  NOT = �A���ҍ폜�|���҃R�[�h   ) OR
028660                       ( �I���t���O        NOT = SPACE )
028670*
028680                DELETE �g�����f�[�^�e
028690                IF ��ԃL�[ NOT = "00"
028700                   MOVE NC"�g�����f�[�^�e" TO �t�@�C�����v
028710                   PERFORM �G���[�\���c
028720                END-IF
028730*
028740                PERFORM �g�����f�[�^�e�Ǎ�
028750         END-PERFORM
028760     END-IF.
028770*
028780*================================================================*
028790 �g�����f�[�^�e�ύX SECTION.
028800*
028810     MOVE �{�p�敪�v              TO  �g���|�{�p�敪.
028820     MOVE �A���ҍ폜�|���҃R�[�h  TO  �g���|���҃R�[�h.
028830     MOVE ZERO                    TO  �g���|��L�[.
028840*
028850     START �g�����f�[�^�e KEY IS >= �g���|�{�p�敪
028860                                    �g���|���҃R�[�h
028870                                    �g���|��L�[
028880     END-START.
028890*
028900     IF ��ԃL�[ = "00"
028910         MOVE SPACE TO �I���t���O
028920         PERFORM �g�����f�[�^�e�Ǎ�
028930         PERFORM UNTIL ( �g���|�{�p�敪   NOT = �{�p�敪�v             ) OR
028940                       ( �g���|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h ) OR
028950                       ( �I���t���O       NOT = SPACE )
028960*
028970* ��L�[�̃_�u����h�����߁A��Ƀ��R�[�h�폜���Ă���쐬����B
028980*------------   / ���R�[�h�̑ޔ� / -------------------------------*
028990                INITIALIZE �ޔ𕉁|���R�[�h
029000                MOVE �g���|���R�[�h  TO �ޔ𕉁|���R�[�h
029010*------------   / ���R�[�h�̍폜 / -------------------------------*
029020                DELETE �g�����f�[�^�e
029030                IF ��ԃL�[ NOT = "00"
029040                   MOVE NC"�g�����f�[�^�e" TO �t�@�C�����v
029050                   PERFORM �G���[�\���c
029060                END-IF
029070*
029080*------------   / �V���R�[�h�̍쐬 / -------------------------------*
029090                MOVE SPACE TO �g���|���R�[�h
029100                INITIALIZE �g���|���R�[�h
029110                MOVE �ޔ𕉁|���R�[�h  TO �g���|���R�[�h
029120                MOVE �A���ҍ폜�|�ύX���҃R�[�h  TO �g���|���҃R�[�h
029130*               / �g�����f�[�^�e�̎}�Ԃ͏��SPACE /
029140                MOVE SPACE                       TO �g���|�}��
029150                WRITE �g���|���R�[�h
029160                IF ��ԃL�[ NOT = "00"
029170                   MOVE NC"�g�����f�[�^�e" TO �t�@�C�����v
029180                   PERFORM �G���[�\��
029190                END-IF
029200*
029210                PERFORM �g�����f�[�^�e�Ǎ�
029220         END-PERFORM
029230     END-IF.
029240*
029250*================================================================*
029260 �g�����f�[�^�e�Ǎ� SECTION.
029270*
029280     READ �g�����f�[�^�e NEXT
029290     AT END
029300         MOVE "YES"  TO �I���t���O
029310     END-READ.
029320*
029321*================================================================*
029322*================================================================*
029323 �g���Î��тe�S�폜 SECTION.
029324*
029325     MOVE �{�p�敪�v              TO  �g�����|�{�p�敪.
029326     MOVE �A���ҍ폜�|���҃R�[�h  TO  �g�����|���҃R�[�h.
029327     MOVE ZERO                    TO  �g�����|�{�p�a��N����.
029332*
029333     START �g���Î��тe KEY IS >= �g�����|�{�p�敪
029334                                  �g�����|���҃R�[�h
029335                                  �g�����|�{�p�a��N����
029336     END-START.
029337*
029338     IF ��ԃL�[ = "00"
029339         MOVE SPACE TO �I���t���O
029340         PERFORM �g���Î��тe�Ǎ�
029341         PERFORM UNTIL ( �g�����|�{�p�敪   NOT = �{�p�敪�v             ) OR
029342                       ( �g�����|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h ) OR
029343                       ( �I���t���O         NOT = SPACE )
029344*
029345                DELETE �g���Î��тe
029346                IF ��ԃL�[ NOT = "00"
029347                   MOVE NC"�g���Î��тe" TO �t�@�C�����v
029348                   PERFORM �G���[�\���c
029349                END-IF
029350*
029351                PERFORM �g���Î��тe�Ǎ�
029352         END-PERFORM
029353     END-IF.
029354*
029355*================================================================*
029356 �g���Î��тe�ύX SECTION.
029357*
029358     MOVE �{�p�敪�v              TO  �g�����|�{�p�敪.
029359     MOVE �A���ҍ폜�|���҃R�[�h  TO  �g�����|���҃R�[�h.
029360     MOVE ZERO                    TO  �g�����|�{�p�a��N����.
029361*
029362     START �g���Î��тe KEY IS >= �g�����|�{�p�敪
029363                                  �g�����|���҃R�[�h
029364                                  �g�����|�{�p�a��N����
029365     END-START.
029366*
029367     IF ��ԃL�[ = "00"
029368         MOVE SPACE TO �I���t���O
029369         PERFORM �g���Î��тe�Ǎ�
029370         PERFORM UNTIL ( �g�����|�{�p�敪   NOT = �{�p�敪�v             ) OR
029371                       ( �g�����|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h ) OR
029372                       ( �I���t���O         NOT = SPACE )
029373*
029374*------------    / �V���R�[�h�̍쐬 / -------------------------------*
029375                MOVE �A���ҍ폜�|�ύX���҃R�[�h  TO �g�����|���҃R�[�h
029376                COMPUTE �g�����|�o�^��  = �g�����|�o�^�� + 0.1
029377                WRITE �g�����|���R�[�h
029378                IF ��ԃL�[ NOT = "00"
029379                   MOVE NC"�g���Î��тe" TO �t�@�C�����v
029380                   PERFORM �G���[�\��
029381                END-IF
029382*
029383*------------   / ���R�[�h�̍폜 / -------------------------------*
029384                MOVE �A���ҍ폜�|���҃R�[�h  TO �g�����|���҃R�[�h
029385                DELETE �g���Î��тe
029386                IF ��ԃL�[ NOT = "00"
029387                   MOVE NC"�g���Î��тe" TO �t�@�C�����v
029388                   PERFORM �G���[�\���c
029389                END-IF
029390*
029391                PERFORM �g���Î��тe�Ǎ�
029392         END-PERFORM
029393     END-IF.
029394*
029395*================================================================*
029396 �g���Î��тe�Ǎ� SECTION.
029397*
029398     READ �g���Î��тe NEXT
029399     AT END
029400         MOVE "YES"  TO �I���t���O
029401     END-READ.
029402*
029403*================================================================*
029404*================================================================*
029405*================================================================*
029406 �g���×\��e�S�폜 SECTION.
029407*
029409     MOVE �A���ҍ폜�|���҃R�[�h  TO  �g���\�|���҃R�[�h.
029410     MOVE ZERO                    TO  �g���\�|�{�p�a��N����.
029411     MOVE ZERO                    TO  �g���\�|�{�p�敪.
029412*
029413     START �g���×\��e KEY IS >= �g���\�|���҃R�[�h
029415                                  �g���\�|�{�p�a��N����
029416                                  �g���\�|�{�p�敪
029420     END-START.
029421*
029422     IF ��ԃL�[ = "00"
029423         MOVE SPACE TO �I���t���O
029424         PERFORM �g���×\��e�Ǎ�
029425         PERFORM UNTIL ( �g���\�|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h ) OR
029427                       ( �I���t���O         NOT = SPACE )
029428*
029429                DELETE �g���×\��e
029430                IF ��ԃL�[ NOT = "00"
029431                   MOVE NC"�g���×\��e" TO �t�@�C�����v
029432                   PERFORM �G���[�\���c
029433                END-IF
029434*
029435                PERFORM �g���×\��e�Ǎ�
029436         END-PERFORM
029437     END-IF.
029438*
029439*================================================================*
029440 �g���×\��e�ύX SECTION.
029441*
029442     MOVE �A���ҍ폜�|���҃R�[�h  TO  �g���\�|���҃R�[�h.
029443     MOVE ZERO                    TO  �g���\�|�{�p�a��N����.
029444     MOVE ZERO                    TO  �g���\�|�{�p�敪.
029445*
029446     START �g���×\��e KEY IS >= �g���\�|���҃R�[�h
029447                                  �g���\�|�{�p�a��N����
029448                                  �g���\�|�{�p�敪
029449     END-START.
029460*
029461     IF ��ԃL�[ = "00"
029462         MOVE SPACE TO �I���t���O
029463         PERFORM �g���×\��e�Ǎ�
029464         PERFORM UNTIL ( �g���\�|���҃R�[�h NOT = �A���ҍ폜�|���҃R�[�h ) OR
029467                       ( �I���t���O         NOT = SPACE )
029468*
029469*------------    / �V���R�[�h�̍쐬 / -------------------------------*
029470                MOVE �A���ҍ폜�|�ύX���҃R�[�h  TO �g���\�|���҃R�[�h
029471                COMPUTE �g���\�|�o�^��  = �g���\�|�o�^�� + 0.1
029472                WRITE �g���\�|���R�[�h
029473                IF ��ԃL�[ NOT = "00"
029474                   MOVE NC"�g���×\��e" TO �t�@�C�����v
029475                   PERFORM �G���[�\��
029476                END-IF
029477*
029478*------------   / ���R�[�h�̍폜 / -------------------------------*
029479                MOVE �A���ҍ폜�|���҃R�[�h  TO �g���\�|���҃R�[�h
029480                DELETE �g���×\��e
029481                IF ��ԃL�[ NOT = "00"
029482                   MOVE NC"�g���×\��e" TO �t�@�C�����v
029483                   PERFORM �G���[�\���c
029484                END-IF
029485*
029486                PERFORM �g���×\��e�Ǎ�
029487         END-PERFORM
029488     END-IF.
029489*
029490*================================================================*
029491 �g���×\��e�Ǎ� SECTION.
029492*
029493     READ �g���×\��e NEXT
029494     AT END
029495         MOVE "YES"  TO �I���t���O
029496     END-READ.
029497*
029498*================================================================*
029499*================================================================*
029500 �g���Î��тe�ύX�Q SECTION.
029502*    ���Î��уf�[�^���̉��Ïꏊ���҃R�[�h�Ɛ揇���҃R�[�h���X�V����
029503*    �S�����r�߂ōX�V
029504*
029505     MOVE SPACE                   TO  �g�����|���R�[�h.
029506     INITIALIZE                       �g�����|���R�[�h.
029508*
029509     START �g���Î��тe KEY IS >= �g�����|�{�p�敪
029510                                  �g�����|���҃R�[�h
029511                                  �g�����|�{�p�a��N����
029512     END-START.
029513*
029514     IF ��ԃL�[ = "00"
029516         MOVE SPACE TO �I���t���O
029517         PERFORM �g���Î��тe�Ǎ�
029518         PERFORM UNTIL �I���t���O         NOT = SPACE 
029520*
029521             MOVE SPACE TO ���Î��эX�V�t���O
029522*
029523             IF �g�����|���Ïꏊ���҃R�[�h = �A���ҍ폜�|���҃R�[�h
029524                 MOVE �A���ҍ폜�|�ύX���҃R�[�h TO �g�����|���Ïꏊ���҃R�[�h
029525                 MOVE "YES"                      TO ���Î��эX�V�t���O
029526             END-IF
029527*
029528             IF �g�����|�揇���҃R�[�h = �A���ҍ폜�|���҃R�[�h
029529                 MOVE �A���ҍ폜�|�ύX���҃R�[�h TO �g�����|�揇���҃R�[�h
029530                 MOVE "YES"                      TO ���Î��эX�V�t���O
029531             END-IF
029532*
029533             IF ���Î��эX�V�t���O NOT = SPACE
029534*------------    / �V���R�[�h�̍X�V / -------------------------------*
029537                REWRITE �g�����|���R�[�h
029538                IF ��ԃL�[ NOT = "00"
029539                   MOVE NC"�g���Î��тe" TO �t�@�C�����v
029540                   PERFORM �G���[�\��
029541                END-IF
029550             END-IF
029551*
029552             PERFORM �g���Î��тe�Ǎ�
029553         END-PERFORM
029554     END-IF.
029555*
029556*================================================================*
029557*================================================================*
029558 �g�񍐏��e�����폜 SECTION.
029559*
029560     MOVE �{�p�敪�v      TO �g��|�{�p�敪.
029561     MOVE ZERO            TO �g��|�p���敪.
029562     MOVE ZERO            TO �g��|�{�p�a��N��.
029563     MOVE ZERO            TO �g��|���Ҕԍ�.
029564     MOVE SPACE           TO �g��|�}��.
029565     MOVE ZERO            TO �g��|�A��.
029566*
029567     START �g�񍐏��e KEY IS >= �g��|�{�p�敪
029568                                �g��|�p���敪
029569                                �g��|�{�p�a��N��
029570                                �g��|���҃R�[�h
029571                                �g��|�A��
029572     END-START.
029573*
029574     IF ��ԃL�[ = "00"
029575         MOVE SPACE            TO �I���t���O
029576         PERFORM �g�񍐏��e�Ǎ�
029577         PERFORM UNTIL (�I���t���O = "YES") OR (�g��|�{�p�敪 NOT = �{�p�敪�v) 
029578             IF (�g��|�{�p�敪      = �{�p�敪�v) AND
029579                (�g��|���҃R�[�h    = �A���ҍ폜�|���҃R�[�h) AND
029580                (�g��|�{�p�a��N��  = �A���ҍ폜�|�{�p�a��N��)
029581                 DELETE �g�񍐏��e
029582                 INVALID KEY
029583                     MOVE NC"�g��" TO �t�@�C�����v
029584                     PERFORM �G���[�\���c
029585                 END-DELETE
029586             END-IF
029587             PERFORM �g�񍐏��e�Ǎ�
029588         END-PERFORM
029589     END-IF.
029590*
029591*================================================================*
029592 �g�񍐏��e�S�폜 SECTION.
029593*
029594     MOVE �{�p�敪�v      TO �g��|�{�p�敪.
029595     MOVE ZERO            TO �g��|�p���敪.
029596     MOVE ZERO            TO �g��|�{�p�a��N��.
029597     MOVE ZERO            TO �g��|���Ҕԍ�.
029598     MOVE SPACE           TO �g��|�}��.
029599     MOVE ZERO            TO �g��|�A��.
029600*
029601     START �g�񍐏��e KEY IS >= �g��|�{�p�敪
029602                                �g��|�p���敪
029603                                �g��|�{�p�a��N��
029604                                �g��|���҃R�[�h
029605                                �g��|�A��
029606     END-START.
029607*
029608     IF ��ԃL�[ = "00"
029609         MOVE SPACE            TO �I���t���O
029610         PERFORM �g�񍐏��e�Ǎ�
029611         PERFORM UNTIL (�I���t���O = "YES") OR (�g��|�{�p�敪 NOT = �{�p�敪�v)
029612             IF (�g��|�{�p�敪      = �{�p�敪�v) AND
029613                (�g��|���҃R�[�h    = �A���ҍ폜�|���҃R�[�h)
029614                 DELETE �g�񍐏��e
029615                 INVALID KEY
029616                     MOVE NC"�g��" TO �t�@�C�����v
029617                     PERFORM �G���[�\���c
029618                 END-DELETE
029619             END-IF
029620             PERFORM �g�񍐏��e�Ǎ�
029621         END-PERFORM
029622     END-IF.
029623*
029624*================================================================*
029625 �g�񍐏��e�ύX SECTION.
029626*
029627     MOVE �{�p�敪�v      TO �g��|�{�p�敪.
029628     MOVE ZERO            TO �g��|�p���敪.
029629     MOVE ZERO            TO �g��|�{�p�a��N��.
029630     MOVE ZERO            TO �g��|���Ҕԍ�.
029631     MOVE SPACE           TO �g��|�}��.
029632     MOVE ZERO            TO �g��|�A��.
029633*
029634     START �g�񍐏��e KEY IS >= �g��|�{�p�敪
029635                                �g��|�p���敪
029636                                �g��|�{�p�a��N��
029637                                �g��|���҃R�[�h
029638                                �g��|�A��
029639     END-START.
029640*
029641     IF ��ԃL�[ = "00"
029642         MOVE SPACE            TO �I���t���O
029643         PERFORM �g�񍐏��e�Ǎ�
029644         PERFORM UNTIL (�I���t���O = "YES") OR (�g��|�{�p�敪 NOT = �{�p�敪�v)
029645             IF (�g��|�{�p�敪    = �{�p�敪�v) AND
029646                (�g��|���҃R�[�h  = �A���ҍ폜�|���҃R�[�h)
029647                 MOVE  �A���ҍ폜�|�ύX���҃R�[�h TO �g��|���҃R�[�h
029648                 WRITE �g��|���R�[�h
029649                 INVALID KEY
029650                     MOVE NC"�g��" TO �t�@�C�����v
029651                     PERFORM �G���[�\��
029652                 END-WRITE
029653                 MOVE  �A���ҍ폜�|���҃R�[�h TO �g��|���҃R�[�h
029654                 DELETE �g�񍐏��e
029655                 INVALID KEY
029656                     MOVE NC"�g��" TO �t�@�C�����v
029657                     PERFORM �G���[�\���c
029658                 END-DELETE
029659             END-IF
029660             PERFORM �g�񍐏��e�Ǎ�
029661         END-PERFORM
029662     END-IF.
029663*
029704*================================================================*
029705 �g�񍐏��e�Ǎ� SECTION.
029706*
029707     READ �g�񍐏��e NEXT
029708     AT END
029709         MOVE "YES" TO �I���t���O
029710     END-READ.
029711*================================================================*
029712*================================================================*
029713 �x������ SECTION.
029714*
029715     PERFORM VARYING �x���J�E���^ FROM 1 BY 1
029716             UNTIL �x���J�E���^ > �x���񐔂v
029717         MOVE "YES" TO �x���t���O
029718     END-PERFORM.
029719*
029720*================================================================*
029721 ������擾 SECTION.
029722*
029723     MOVE ZERO TO ���|����敪
029724     READ ������}�X�^
029725     NOT INVALID KEY
029726         MOVE ���|�x����             TO �x���񐔂v
029727     END-READ.
029728*
029729*================================================================*
       �g���Z�v�g�ڍׂe�폜 SECTION.
      *
           MOVE �g���Z�|���R�[�h�L�[ TO �g���Z�ڍׁ|���R�[�h�L�[.
027360     READ �g���Z�v�g�ڍׂe
027370     INVALID KEY
               CONTINUE
027370     NOT INVALID KEY
026532         DELETE �g���Z�v�g�ڍׂe
026533         INVALID KEY
026534            MOVE NC"�g���Z�v�g�ڍׂe" TO �t�@�C�����v
026540            PERFORM �G���[�\���c
026550         END-DELETE
027390     END-READ.
029730*================================================================*
       �g���Z�v�g�ڍׂe�쐬 SECTION.
      *
           MOVE �g���Z�|���R�[�h�L�[ TO �g���Z�ڍׁ|���R�[�h�L�[.
027360     READ �g���Z�v�g�ڍׂe
027370     INVALID KEY
               CONTINUE
027370     NOT INVALID KEY
027140         MOVE �A���ҍ폜�|�ύX���҃R�[�h TO �g���Z�ڍׁ|���҃R�[�h
027150         WRITE �g���Z�ڍׁ|���R�[�h
027160         INVALID KEY
027170            MOVE NC"�g���Z�v�g�ڍׂe" TO �t�@�C�����v
027180            PERFORM �G���[�\��
027190         END-WRITE
027390     END-READ.
029730*================================================================*
029731*******************************************************************
029732 END PROGRAM CHGJUNO.
029733*******************************************************************
