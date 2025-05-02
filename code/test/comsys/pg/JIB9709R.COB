000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             JIB9709R.
000060 AUTHOR.                 ŽR“c _”V
000070*
000080*----------------------------------------------------------------*
000090*         ”ï—pŠzŒvŽZƒvƒƒOƒ‰ƒ€@ƒŒƒZƒvƒg”­smŽ©”…Ó•ÛŒ¯—pni_j
000100*         MED = 
000110*----------------------------------------------------------------*
000120* Š³ŽÒ‚Pl‚ÌŒŽ‚ÌƒŒƒZƒvƒg€–Ú‚ÌŽZoB¬Œvi’·Šú’üŒ¸ŠÜ‚Þ¬ŒvjB
000130* ”ï—pŠzi¬Œv‚ÌcŒvjB•‰’S‹àB¿‹Šz‚ð‹‚ß‚éB
000140* 
000150* •½¬‚X”N‚XŒŽ•ª¿‹‚©‚çŽg—pŠJŽn
000160*----------------------------------------------------------------*
000170 DATE-WRITTEN.           1997-12-21
000180 DATE-COMPILED.          1997-12-21
000190*----------------------------------------------------------------*
000200* C³—š—ð
000210* 19981210 Ž©”ïf—ÃŒvŽZ‚ª‚Å‚«‚é‚æ‚¤‚ÉC³
      */ƒŒƒZ[ƒŒƒZ¿‹‹æ•ª‚ðŽg‚¤‚o‚f‚ª‚ ‚éˆ×ƒRƒƒ“ƒg‰ðœ«««/170911
000220******************************************************************
000230*            ENVIRONMENT         DIVISION                        *
000240******************************************************************
000250 ENVIRONMENT             DIVISION.
000260 CONFIGURATION           SECTION.
000270 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000280 OBJECT-COMPUTER.        FMV-DESKPOWER.
000290 SPECIAL-NAMES.          CONSOLE  IS  CONS
000300                         SYSERR   IS  MSGBOX.
000310 INPUT-OUTPUT            SECTION.
000320 FILE-CONTROL.
000330     SELECT  Ž{p‹L˜^‚e      ASSIGN      TO        SEKIROKL
000340                             ORGANIZATION             IS  INDEXED
000350                             ACCESS MODE              IS  DYNAMIC
000360                             RECORD KEY               IS  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
000370                                                          Ž{‹L|Š³ŽÒƒR[ƒh
000380                             ALTERNATE RECORD KEY     IS  Ž{‹L|Š³ŽÒƒR[ƒh
000390                                                          Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
000400                             FILE STATUS              IS  ó‘ÔƒL[
000410                             LOCK        MODE         IS  AUTOMATIC.
000420     SELECT  —¿‹àƒ}ƒXƒ^      ASSIGN      TO        JRYOUKL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS  —¿|‹æ•ªƒR[ƒh
000460                                                          —¿|•”ˆÊƒR[ƒh
000470                                                          —¿|ŠJŽn˜a—ï”NŒŽ
000480                             FILE STATUS              IS  ó‘ÔƒL[
000490                             LOCK        MODE         IS  AUTOMATIC.
000500     SELECT  ŒvŽZƒ}ƒXƒ^      ASSIGN      TO        JKEISANL
000510                             ORGANIZATION             IS  INDEXED
000520                             ACCESS MODE              IS  DYNAMIC
000530                             RECORD KEY               IS  Œv|§Œä‹æ•ª
000540                                                          Œv|ŠJŽn˜a—ï”NŒŽ
000550                             FILE STATUS              IS  ó‘ÔƒL[
000560                             LOCK        MODE         IS  AUTOMATIC.
000570*     SELECT  ŒvŽZ‚o‚fƒ}ƒXƒ^  ASSIGN      TO        KEISAPGL
000580*                             ORGANIZATION             IS  INDEXED
000590*                             ACCESS MODE              IS  DYNAMIC
000600*                             RECORD KEY               IS  Œv‚o|‹æ•ªƒR[ƒh
000610*                                                          Œv‚o|ŠJŽn˜a—ï”NŒŽ
000620*                             FILE STATUS              IS  ó‘ÔƒL[
000630*                             LOCK        MODE         IS  AUTOMATIC.
000640     SELECT  ŽófŽÒî•ñ‚e    ASSIGN      TO        JUSINJL
000650                             ORGANIZATION             IS  INDEXED
000660                             ACCESS MODE              IS  DYNAMIC
000670                             RECORD KEY               IS  Žó|Ž{p˜a—ï”NŒŽ
000680                                                          Žó|Š³ŽÒƒR[ƒh
000690                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000700                                                          Žó|Š³ŽÒƒJƒi
000710                                                          Žó|Š³ŽÒƒR[ƒh
000720                             ALTERNATE RECORD KEY     IS  Žó|Š³ŽÒƒR[ƒh
000730                                                          Žó|Ž{p˜a—ï”NŒŽ
000740                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000750                                                          Žó|•ÛŒ¯Ží•Ê
000760                                                          Žó|•ÛŒ¯ŽÒ”Ô†
000770                                                          Žó|Š³ŽÒƒR[ƒh
000780                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000790                                                          Žó|Œö”ïŽí•Ê
000800                                                          Žó|”ï—p•‰’SŽÒ”Ô†
000810                                                          Žó|Š³ŽÒƒR[ƒh
000820                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000830                                                          Žó|•¬Ží•Ê
000840                                                          Žó|”ï—p•‰’SŽÒ”Ô†•¬
000850                                                          Žó|Š³ŽÒƒR[ƒh
000860                             ALTERNATE RECORD KEY     IS  Žó|¿‹˜a—ï”NŒŽ
000870                                                          Žó|Ž{p˜a—ï”NŒŽ
000880                                                          Žó|Š³ŽÒƒR[ƒh
000890                             FILE STATUS              IS  ó‘ÔƒL[
000900                             LOCK        MODE         IS  AUTOMATIC.
000910     SELECT  •‰ƒf[ƒ^‚e    ASSIGN      TO        HUSYOUL
000920                             ORGANIZATION             IS  INDEXED
000930                             ACCESS MODE              IS  DYNAMIC
000940                             RECORD KEY               IS  •‰|Ž{p˜a—ï”NŒŽ
000950                                                          •‰|Š³ŽÒƒR[ƒh
000960                             ALTERNATE RECORD KEY     IS  •‰|Š³ŽÒƒR[ƒh
000970                                                          •‰|Ž{p˜a—ï”NŒŽ
000980                             FILE STATUS              IS  ó‘ÔƒL[
000990                             LOCK        MODE         IS  AUTOMATIC.
001000     SELECT  §Œäî•ñƒ}ƒXƒ^  ASSIGN      TO        SEIGYOL
001010                             ORGANIZATION             IS  INDEXED
001020                             ACCESS MODE              IS  DYNAMIC
001030                             RECORD KEY               IS  §|§Œä‹æ•ª
001040                             FILE STATUS              IS  ó‘ÔƒL[
001050                             LOCK        MODE         IS  AUTOMATIC.
001060     SELECT  –¼Ìƒ}ƒXƒ^      ASSIGN      TO        MEISYOL
001070                             ORGANIZATION             IS  INDEXED
001080                             ACCESS MODE              IS  DYNAMIC
001090                             RECORD KEY               IS  –¼|‹æ•ªƒR[ƒh
001100                                                          –¼|–¼ÌƒR[ƒh
001110                             FILE STATUS              IS  ó‘ÔƒL[
001120                             LOCK        MODE         IS  AUTOMATIC.
001130     SELECT  Œ³†ƒ}ƒXƒ^      ASSIGN      TO        GENGOUL
001140                             ORGANIZATION             IS  INDEXED
001150                             ACCESS MODE              IS  DYNAMIC
001160                             RECORD KEY               IS  Œ³|Œ³†‹æ•ª
001170                             FILE STATUS              IS  ó‘ÔƒL[
001180                             LOCK        MODE         IS  AUTOMATIC.
001190*******
001200     SELECT  ‰ïŒvƒf[ƒ^‚e    ASSIGN      TO        KAIKEIL
001210                             ORGANIZATION             IS  INDEXED
001220                             ACCESS MODE              IS  DYNAMIC
001230                             RECORD KEY               IS  ‰ï|Ž{p˜a—ï”NŒŽ“ú
001240                                                          ‰ï|Š³ŽÒƒR[ƒh
001250                             ALTERNATE RECORD KEY     IS  ‰ï|Š³ŽÒƒR[ƒh
001260                                                          ‰ï|Ž{p˜a—ï”NŒŽ“ú
001270                             FILE STATUS              IS  ó‘ÔƒL[
001280                             LOCK        MODE         IS  AUTOMATIC.
001290     SELECT  ƒŒƒZƒvƒg‚e      ASSIGN      TO        RECEPTL
001300                             ORGANIZATION             IS  INDEXED
001310                             ACCESS MODE              IS  DYNAMIC
001320                             RECORD KEY               IS  ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001330                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
001340                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001350                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|Š³ŽÒƒR[ƒh
001360                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001370                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001380                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|¿‹˜a—ï”NŒŽ
001390                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001400                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
001410                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001420                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|¿‹˜a—ï”NŒŽ
001430                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001440                                                          ƒŒƒZ|¿‹•ÛŒ¯ŽÒ”Ô†
001450                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
001460                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001470                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|¿‹˜a—ï”NŒŽ
001480                                                          ƒŒƒZ|¿‹•ÛŒ¯ŽÒ”Ô†
001490                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
001500                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001510                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001520                             FILE STATUS              IS  ó‘ÔƒL[
001530                             LOCK        MODE         IS  AUTOMATIC.
001540     SELECT  ƒŒƒZƒvƒg˜JŽ©‚e  ASSIGN      TO        RECERJL
001550                             ORGANIZATION             IS  INDEXED
001560                             ACCESS MODE              IS  DYNAMIC
001570                             RECORD KEY               IS  ƒŒƒZ˜JŽ©|Ž{p˜a—ï”NŒŽ
001580                                                          ƒŒƒZ˜JŽ©|Š³ŽÒƒR[ƒh
001590                                                          ƒŒƒZ˜JŽ©|ƒŒƒZŽí•Ê
001600                             FILE STATUS              IS  ó‘ÔƒL[
001610                             LOCK        MODE         IS  AUTOMATIC.
001620**
001630     SELECT  ‰ïŒv—ÌŽû‚e      ASSIGN      TO        RYOSYUL
001640                             ORGANIZATION             IS  INDEXED
001650                             ACCESS MODE              IS  DYNAMIC
001660                             RECORD KEY               IS  —Ì|Ž{p‹æ•ª
001670                                                          —Ì|‰ïŒv—ÌŽû‹æ•ª
001680                                                          —Ì|Ž{p˜a—ï”NŒŽ“ú
001690                                                          —Ì|Š³ŽÒƒR[ƒh
001700                             ALTERNATE RECORD KEY     IS  —Ì|Ž{p‹æ•ª
001710                                                          —Ì|Š³ŽÒƒR[ƒh
001720                                                          —Ì|‰ïŒv—ÌŽû‹æ•ª
001730                                                          —Ì|Ž{p˜a—ï”NŒŽ“ú
001740                             FILE STATUS              IS  ó‘ÔƒL[
001750                             LOCK        MODE         IS  AUTOMATIC.
002840     SELECT  Ž©”…Óî•ñ‚e    ASSIGN      TO        JIBAIJL
002850                             ORGANIZATION             IS INDEXED
002860                             ACCESS MODE              IS DYNAMIC
002870                             RECORD KEY               IS Ž©”…|Ž{p˜a—ï”NŒŽ
002880                                                         Ž©”…|Š³ŽÒƒR[ƒh
002890                             ALTERNATE RECORD KEY     IS Ž©”…|Š³ŽÒƒR[ƒh
002900                                                         Ž©”…|Ž{p˜a—ï”NŒŽ
002910                             FILE STATUS              IS ó‘ÔƒL[
002920                             LOCK        MODE         IS AUTOMATIC.
001760*
      */“Á•ÊÞ—¿‚Ì‘Î‰ž/20180829
000330     SELECT  XVŽ{p‹L˜^‚e  ASSIGN      TO        SEKIROKL
000340                             ORGANIZATION             IS  INDEXED
000350                             ACCESS MODE              IS  DYNAMIC
000360                             RECORD KEY               IS  XVŽ{‹L|Ž{p˜a—ï”NŒŽ“ú
000370                                                          XVŽ{‹L|Š³ŽÒƒR[ƒh
000380                             ALTERNATE RECORD KEY     IS  XVŽ{‹L|Š³ŽÒƒR[ƒh
000390                                                          XVŽ{‹L|Ž{p˜a—ï”NŒŽ“ú
000400                             FILE STATUS              IS  ó‘ÔƒL[
000410                             LOCK        MODE         IS  AUTOMATIC.
001770******************************************************************
001780*                      DATA DIVISION                             *
001790******************************************************************
001800 DATA                    DIVISION.
001810 FILE                    SECTION.
001820*                           m‚q‚k  ‚Q‚T‚Un
001830 FD  Ž{p‹L˜^‚e          BLOCK   CONTAINS   1   RECORDS.
001840     COPY SEKIROK         OF  XFDLIB  JOINING   Ž{‹L AS  PREFIX.
001850*                           m‚q‚k  ‚R‚Q‚On
001860 FD  —¿‹àƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
001870     COPY RYOUKIN         OF  XFDLIB  JOINING   —¿   AS  PREFIX.
001880     COPY RYOUKNA         OF  XFDLIB  JOINING   —¿‚` AS  PREFIX.
001890     COPY RYOUKNB         OF  XFDLIB  JOINING   —¿‚a AS  PREFIX.
001900     COPY RYOUKNC         OF  XFDLIB  JOINING   —¿‚b AS  PREFIX.
001910     COPY RYOUKND         OF  XFDLIB  JOINING   —¿‚c AS  PREFIX.
001920     COPY RYOUKNE         OF  XFDLIB  JOINING   —¿‚d AS  PREFIX.
001930     COPY RYOUKNF         OF  XFDLIB  JOINING   —¿‚e AS  PREFIX.
001940*                           m‚q‚k  ‚Q‚T‚Un
001950 FD  ŒvŽZƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
001960     COPY KEISAN          OF  XFDLIB  JOINING   Œv   AS  PREFIX.
001970     COPY KEISANA         OF  XFDLIB  JOINING   Œv‚` AS  PREFIX.
001980     COPY KEISANB         OF  XFDLIB  JOINING   Œv‚a AS  PREFIX.
001990     COPY KEISANC         OF  XFDLIB  JOINING   Œv‚b AS  PREFIX.
002000**                           m‚q‚k  ‚P‚Q‚Wn
002010* FD  ŒvŽZ‚o‚fƒ}ƒXƒ^       BLOCK   CONTAINS   1   RECORDS.
002020*     COPY KEISAPG    OF  XFDLIB  JOINING   Œv‚o AS  PREFIX.
002030*                           m‚q‚k  ‚R‚Q‚On
002040 FD  ŽófŽÒî•ñ‚e        BLOCK   CONTAINS   1   RECORDS.
002050     COPY JUSINJ     OF  XFDLIB  JOINING   Žó   AS  PREFIX.
002060*                           m‚q‚k  ‚P‚Q‚Wn
002070 FD  •‰ƒf[ƒ^‚e        BLOCK   CONTAINS   1   RECORDS.
002080     COPY HUSYOU          OF  XFDLIB  JOINING   •‰   AS  PREFIX.
002090*                           m‚q‚k  ‚Q‚T‚Un
002100 FD  §Œäî•ñƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
002110     COPY SEIGYO     OF  XFDLIB  JOINING   §   AS  PREFIX.
002120*                           m‚q‚k  ‚P‚Q‚Wn
002130 FD  Œ³†ƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
002140     COPY GENGOU     OF  XFDLIB  JOINING   Œ³   AS  PREFIX.
002150*                           m‚q‚k  ‚P‚Q‚Wn
002160 FD  –¼Ìƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
002170     COPY MEISYO     OF  XFDLIB  JOINING   –¼   AS  PREFIX.
002180*******
002190 FD  ‰ïŒvƒf[ƒ^‚e        BLOCK   CONTAINS   1   RECORDS.
002200     COPY KAIKEI     OF  XFDLIB  JOINING   ‰ï     AS  PREFIX.
002200     COPY KAIKEI02   OF  XFDLIB  JOINING   ‰ï‚O‚Q AS  PREFIX.
002210*
002220 FD  ƒŒƒZƒvƒg‚e          BLOCK   CONTAINS   1   RECORDS.
002230     COPY RECEPT          OF  XFDLIB  JOINING   ƒŒƒZ   AS  PREFIX.
002240*
002250 FD  ƒŒƒZƒvƒg˜JŽ©‚e      BLOCK   CONTAINS   1   RECORDS.
002260     COPY RECERJ          OF  XFDLIB  JOINING   ƒŒƒZ˜JŽ© AS  PREFIX.
002270*
002280 FD  ‰ïŒv—ÌŽû‚e     BLOCK   CONTAINS   1   RECORDS.
002290     COPY RYOSYU    OF  XFDLIB  JOINING   —Ì  AS  PREFIX.
002300*
003740 FD  Ž©”…Óî•ñ‚e        BLOCK   CONTAINS   1   RECORDS.
003750     COPY JIBAIJ     OF  XFDLIB  JOINING   Ž©”…   AS  PREFIX.
002290*
001830 FD  XVŽ{p‹L˜^‚e      BLOCK   CONTAINS   1   RECORDS.
001840     COPY SEKIROK         OF  XFDLIB  JOINING   XVŽ{‹L AS  PREFIX.
002290*
002310*----------------------------------------------------------------*
002320******************************************************************
002330*                WORKING-STORAGE SECTION                         *
002340******************************************************************
002350 WORKING-STORAGE         SECTION.
002360 01 ƒL[“ü—Í                           PIC X    VALUE SPACE.
002370 01 ó‘ÔƒL[                           PIC X(2) VALUE SPACE.
002380 01 I—¹ƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002390 01 I—¹ƒtƒ‰ƒO‚Q                       PIC X(3) VALUE SPACE.
002400 01 ‘žƒtƒ‰ƒO                         PIC X(4) VALUE SPACE.
002410 01 ‘žƒtƒ‰ƒO‚Q                       PIC X(4) VALUE SPACE.
002420 01 ‘ž‹–‰Âƒtƒ‰ƒO                     PIC X(3) VALUE SPACE.
002430 01 ”NŒŽƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002440 01 ’Ç‰Áƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002450 01 sŒ…ƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002460 01 €–Úƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002470 01 ’·Šúƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002480 01 ‰—Ã‰ÁŽZƒtƒ‰ƒO                     PIC X(3) VALUE SPACE.
002490 01 •ÛŒ¯ŽÒ”Ô†‚v                       PIC X(6) VALUE SPACE.
002500 01 ƒtƒ@ƒCƒ‹–¼‚v                       PIC N(2) VALUE SPACE.
002510 01 ƒGƒ‰[“à—e‚v                       PIC N(18) VALUE SPACE.
002520 01 ‰ïˆõ–¼‚v                           PIC N(16) VALUE SPACE.
002530 01 ‰æ–Ê–¼Ì‚v                         PIC N(2) VALUE SPACE.
002540 01 •‰‚b‚m‚s                         PIC 9(2) VALUE ZERO.
002550 01 •”ˆÊƒJƒEƒ“ƒ^                       PIC 9(2) VALUE ZERO.
002560 01 •”ˆÊƒJƒEƒ“ƒ^‚Q                     PIC 9(2) VALUE ZERO.
002570 01 •”ˆÊ‚b‚m‚s                         PIC 9(2) VALUE ZERO.
002580 01 •”ˆÊ‚b‚m‚s‚Q                       PIC 9(2) VALUE ZERO.
002590 01 ‘½•”ˆÊ‚b‚m‚s                       PIC 9(2) VALUE ZERO.
002600 01 ƒJƒEƒ“ƒ^                           PIC 9(2) VALUE ZERO.
002610 01 ƒJƒEƒ“ƒ^‚Q                         PIC 9(2) VALUE ZERO.
002620 01 ãª–@’P‰¿‚v                         PIC 9(5) VALUE ZERO.
002630 01 “d—Ã’P‰¿‚v‚Q                       PIC 9(5) VALUE ZERO.
002640 01 Å‘å“o˜^•”ˆÊ”‚v                   PIC 9(2) VALUE ZERO.
002650 01 ¿‹ˆÚ“®ƒL[                       PIC X(4) VALUE SPACE.
002660 01 ‰æ–ÊˆÚ“®ƒL[                       PIC X(4) VALUE SPACE.
002670 01 ˆ—ˆÚ“®ƒL[                       PIC X(4) VALUE SPACE.
002680 01 Ä“ü—ÍƒL[‚v                       PIC X(4) VALUE SPACE.
002690 01 “ü—Íó‘Ô‚v                         PIC X(4) VALUE SPACE.
002700 01 •”ˆÊ–¼Ì‚v                         PIC N(12) VALUE SPACE.
002710 01 s‚v                               PIC 9(4) VALUE ZERO.
002720 01 Œ…‚v                               PIC 9(4) VALUE ZERO.
002730 01 ‘O˜a—ï‚v                           PIC 9 VALUE ZERO.
002740 01 ƒJƒŒƒ“ƒg˜a—ï‚v                     PIC 9 VALUE ZERO.
002750 01 “ü‹àŠz‚v                           PIC 9(6) VALUE ZERO.
002760 01 ‰ŒŸ¿‹‹æ•ª‚v                     PIC 9 VALUE ZERO.
002770 01 “]‹A‹æ•ª‚v                         PIC 9 VALUE ZERO.
002780 01 •ÛŒ¯Ží•Ê‚v                         PIC 9(2) VALUE ZERO.
002790 01 Œö”ïŽí•Ê‚v                         PIC 9(2) VALUE ZERO.
002800 01 •¬Ží•Ê‚v                         PIC 9(2) VALUE ZERO.
002810 01 •¬Ží•Ê‚P‚v                       PIC 9(2) VALUE ZERO.
002820 01 •¬Ží•Ê‚Q‚v                       PIC 9(2) VALUE ZERO.
002830 01 ˜Vl•‰’S‰ñ”‚v                     PIC 9(2) VALUE ZERO.
002840 01 •¬•‰’SŒvŽZ‹æ•ª‚v                 PIC 9 VALUE ZERO.
002850*
002860 01 ‰ŒŸŽžŠÔŠO‚v                       PIC 9 VALUE ZERO.
002870 01 ‰ŒŸ‹x“ú‚v                         PIC 9 VALUE ZERO.
002880 01 ‰ŒŸ[–é‚v                         PIC 9 VALUE ZERO.
002890*
002900 01 ‰—Ã–éŠÔ‚v                         PIC 9 VALUE ZERO.
002910 01 ‰—Ã“ï˜H‚v                         PIC 9 VALUE ZERO.
002920 01 ‰—Ã–\•—‰J‚v                       PIC 9 VALUE ZERO.
002930 01 ‰—ÃŽžŠÔŠO‚v                       PIC 9 VALUE ZERO.
002940*
002950 01 ‹à‘®•›Žq‘å‚v                       PIC 9 VALUE ZERO.
002960 01 ‹à‘®•›Žq’†‚v                       PIC 9 VALUE ZERO.
002970 01 ‹à‘®•›Žq¬‚v                       PIC 9 VALUE ZERO.
002980*
002990 01 ‰—Ã”íœ”‚v                       PIC 9(3) VALUE ZERO.
003000 01 ‰—Ãœ”‚v                         PIC 9(3) VALUE ZERO.
003010 01 ‰—Ã¤‚v                           PIC 9(3) VALUE ZERO.
003020 01 ‰—Ãè—]‚v                         PIC 9(3) VALUE ZERO.
003030 01 ‰—Ã‹——£‚v                         PIC 9(3)V9 VALUE ZERO.
003040 01 ‰—Ã‰ÁŽZ—¦‚v                       PIC 9(3) VALUE ZERO.
003050*
003060 01 ‰ŒŸ—¿‚v                           PIC 9(4) VALUE ZERO.
003070 01 ‰ŒŸ‰ÁŽZ—¿‚v                       PIC 9(5) VALUE ZERO.
003080 01 ÄŒŸ—¿‚v                           PIC 9(4) VALUE ZERO.
003090 01 ‰—Ã—¿‚v                           PIC 9(5) VALUE ZERO.
003100*/‰—Ã’P‰¿‚Ìˆ×‚Ìƒ[ƒN
003110 01 ‰—Ã—¿‚v‚Q                         PIC 9(5) VALUE ZERO.
003120 01 ‰—Ã‰ÁŽZ—¿‚v                       PIC 9(6) VALUE ZERO.
003130 01 Œã—Ã—¿‚v                           PIC 9(5) VALUE ZERO.
003140 01 —âãª–@—¿‚v                         PIC 9(4) VALUE ZERO.
003150 01 ‰·ãª–@—¿‚v                         PIC 9(4) VALUE ZERO.
003160 01 “d—Ã—¿‚v                           PIC 9(4) VALUE ZERO.
003170*
003180 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚P                 PIC 9(6) VALUE ZERO.
003190 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚Q                 PIC 9(6) VALUE ZERO.
003200 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚R                 PIC 9(6) VALUE ZERO.
003210 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚R‚P               PIC 9(6) VALUE ZERO.
003220 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚S                 PIC 9(6) VALUE ZERO.
003230 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚S‚P               PIC 9(6) VALUE ZERO.
003240 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚T                 PIC 9(6) VALUE ZERO.
003250 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚T‚P               PIC 9(6) VALUE ZERO.
003260 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚T‚Q               PIC 9(6) VALUE ZERO.
003270 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚T‚R               PIC 9(6) VALUE ZERO.
003280*
003290 01 •”ˆÊ“úŒv’·Šú‚v‚P                   PIC 9(6) VALUE ZERO.
003300 01 •”ˆÊ“úŒv’·Šú‚v‚Q                   PIC 9(6) VALUE ZERO.
003310 01 •”ˆÊ“úŒv’·Šú‚v‚R                   PIC 9(6) VALUE ZERO.
003320 01 •”ˆÊ“úŒv’·Šú‚v‚R‚P                 PIC 9(6) VALUE ZERO.
003330 01 •”ˆÊ“úŒv’·Šú‚v‚S                   PIC 9(6) VALUE ZERO.
003340 01 •”ˆÊ“úŒv’·Šú‚v‚S‚P                 PIC 9(6) VALUE ZERO.
003350 01 •”ˆÊ“úŒv’·Šú‚v‚S‚Q                 PIC 9(6) VALUE ZERO.
003360 01 •”ˆÊ“úŒv’·Šú‚v‚T                   PIC 9(6) VALUE ZERO.
003370 01 •”ˆÊ“úŒv’·Šú‚v‚T‚P                 PIC 9(6) VALUE ZERO.
003380 01 •”ˆÊ“úŒv’·Šú‚v‚T‚Q                 PIC 9(6) VALUE ZERO.
003390 01 •”ˆÊ“úŒv’·Šú‚v‚T‚R                 PIC 9(6) VALUE ZERO.
003400*
003410 01 •”ˆÊ“úŒv‚v‚P                       PIC 9(6) VALUE ZERO.
003420 01 •”ˆÊ“úŒv‚v‚Q                       PIC 9(6) VALUE ZERO.
003430 01 •”ˆÊ“úŒv‚v‚R                       PIC 9(6) VALUE ZERO.
003440 01 •”ˆÊ“úŒv‚v‚R‚P                     PIC 9(6) VALUE ZERO.
003450 01 •”ˆÊ“úŒv‚v‚S                       PIC 9(6) VALUE ZERO.
003460 01 •”ˆÊ“úŒv‚v‚S‚P                     PIC 9(6) VALUE ZERO.
003470 01 •”ˆÊ“úŒv‚v‚S‚Q                     PIC 9(6) VALUE ZERO.
003480 01 •”ˆÊ“úŒv‚v‚T                       PIC 9(6) VALUE ZERO.
003490 01 •”ˆÊ“úŒv‚v‚T‚P                     PIC 9(6) VALUE ZERO.
003500 01 •”ˆÊ“úŒv‚v‚T‚Q                     PIC 9(6) VALUE ZERO.
003510 01 •”ˆÊ“úŒv‚v‚T‚R                     PIC 9(6) VALUE ZERO.
003520*
003530 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚P                 PIC 9(6) VALUE ZERO.
003540 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚Q                 PIC 9(6) VALUE ZERO.
003550 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚R                 PIC 9(6) VALUE ZERO.
003560 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚R‚P               PIC 9(6) VALUE ZERO.
003570 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚S                 PIC 9(6) VALUE ZERO.
003580 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚S‚P               PIC 9(6) VALUE ZERO.
003590 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚T                 PIC 9(6) VALUE ZERO.
003600 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚T‚P               PIC 9(6) VALUE ZERO.
003610 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚T‚Q               PIC 9(6) VALUE ZERO.
003620 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚T‚R               PIC 9(6) VALUE ZERO.
003630*
003640 01 •”ˆÊ‡Œv’·Šú‚v‚P                   PIC 9(6) VALUE ZERO.
003650 01 •”ˆÊ‡Œv’·Šú‚v‚Q                   PIC 9(6) VALUE ZERO.
003660 01 •”ˆÊ‡Œv’·Šú‚v‚R                   PIC 9(6) VALUE ZERO.
003670 01 •”ˆÊ‡Œv’·Šú‚v‚S                   PIC 9(6) VALUE ZERO.
003680 01 •”ˆÊ‡Œv’·Šú‚v‚T                   PIC 9(6) VALUE ZERO.
003690 01 •”ˆÊ‡Œv’·Šú‚v‚U                   PIC 9(6) VALUE ZERO.
003700 01 •”ˆÊ‡Œv’·Šú‚v‚V                   PIC 9(6) VALUE ZERO.
003710 01 •”ˆÊ‡Œv’·Šú‚v‚W                   PIC 9(6) VALUE ZERO.
003720 01 •”ˆÊ‡Œv’·Šú‚v‚X                   PIC 9(6) VALUE ZERO.
003730 01 •”ˆÊ‡Œv’·Šú‚v‚R‚P                 PIC 9(6) VALUE ZERO.
003740 01 •”ˆÊ‡Œv’·Šú‚v‚S‚P                 PIC 9(6) VALUE ZERO.
003750 01 •”ˆÊ‡Œv’·Šú‚v‚S‚Q                 PIC 9(6) VALUE ZERO.
003760 01 •”ˆÊ‡Œv’·Šú‚v‚T‚P                 PIC 9(6) VALUE ZERO.
003770 01 •”ˆÊ‡Œv’·Šú‚v‚T‚Q                 PIC 9(6) VALUE ZERO.
003780 01 •”ˆÊ‡Œv’·Šú‚v‚T‚R                 PIC 9(6) VALUE ZERO.
003790*
003800 01 •”ˆÊ‡Œv‚v‚P                       PIC 9(6) VALUE ZERO.
003810 01 •”ˆÊ‡Œv‚v‚Q                       PIC 9(6) VALUE ZERO.
003820 01 •”ˆÊ‡Œv‚v‚R                       PIC 9(6) VALUE ZERO.
003830 01 •”ˆÊ‡Œv‚v‚S                       PIC 9(6) VALUE ZERO.
003840 01 •”ˆÊ‡Œv‚v‚T                       PIC 9(6) VALUE ZERO.
003850 01 •”ˆÊ‡Œv‚v‚U                       PIC 9(6) VALUE ZERO.
003860 01 •”ˆÊ‡Œv‚v‚V                       PIC 9(6) VALUE ZERO.
003870 01 •”ˆÊ‡Œv‚v‚W                       PIC 9(6) VALUE ZERO.
003880 01 •”ˆÊ‡Œv‚v‚X                       PIC 9(6) VALUE ZERO.
003890 01 •”ˆÊ‡Œv‚v‚R‚P                     PIC 9(6) VALUE ZERO.
003900 01 •”ˆÊ‡Œv‚v‚S‚P                     PIC 9(6) VALUE ZERO.
003910 01 •”ˆÊ‡Œv‚v‚S‚Q                     PIC 9(6) VALUE ZERO.
003920 01 •”ˆÊ‡Œv‚v‚T‚P                     PIC 9(6) VALUE ZERO.
003930 01 •”ˆÊ‡Œv‚v‚T‚Q                     PIC 9(6) VALUE ZERO.
003940 01 •”ˆÊ‡Œv‚v‚T‚R                     PIC 9(6) VALUE ZERO.
003950*
003960 01 ‰‰ñˆ’u‚r‚v.
003970   03 ‰‰ñˆ’u‚n‚v OCCURS 9.
003980     05 ‰‰ñˆ’u—¿‚n‚v                 PIC 9(5) VALUE ZERO.
003990*
004000 01 Œã—Ã’P‰¿‚r‚v.
004010   03 Œã—Ã’P‰¿‚n‚v‚P OCCURS 9.
004020     05 Œã—Ã’P‰¿‚n‚v                   PIC 9(4) VALUE ZERO.
004030     05 SkŒã—Ã’P‰¿‚n‚v               PIC 9(4) VALUE ZERO.
004040     05 ‘½•”ˆÊŒã—Ã’P‰¿‚n‚v             PIC 9(4) VALUE ZERO.
004050     05 ’·ŠúŒã—Ã’P‰¿‚n‚v               PIC 9(4) VALUE ZERO.
004060     05 ‘½•”ˆÊSkŒã—Ã’P‰¿‚n‚v         PIC 9(4) VALUE ZERO.
004070*
004080*/˜JÐ‚Í‘½•”ˆÊ’üŒ¸‚ª–³‚¢
004090 01 Œã—Ã—¿‚P‚O‚O‚r‚v.
004100     05 Œã—Ã—¿‚P‚O‚O‚v                 PIC 9(5) OCCURS 9 VALUE ZERO.
004110 01 “d—Ã—¿‚P‚O‚O‚r‚v.
004120     05 “d—Ã—¿‚P‚O‚O‚v                 PIC 9(5) OCCURS 9 VALUE ZERO.
004130 01 —âãª–@—¿‚P‚O‚O‚r‚v.
004140     05 —âãª–@—¿‚P‚O‚O‚v               PIC 9(5) OCCURS 9 VALUE ZERO.
004150 01 ‰·ãª–@—¿‚P‚O‚O‚r‚v.
004160     05 ‰·ãª–@—¿‚P‚O‚O‚v               PIC 9(5) OCCURS 9 VALUE ZERO.
004170*
004180 01 “d—Ã’P‰¿‚r‚v.
004190     05 “d—Ã’P‰¿‚v                     PIC 9(5) OCCURS 9 VALUE ZERO.
004200 01 —âãª–@’P‰¿‚r‚v.
004210     05 —âãª–@’P‰¿‚v                   PIC 9(5) OCCURS 9 VALUE ZERO.
004220 01 ‰·ãª–@’P‰¿‚r‚v.
004230     05 ‰·ãª–@’P‰¿‚v                   PIC 9(5) OCCURS 9 VALUE ZERO.
004240*
004250 01 “d—Ã’P‰¿‚r‚v’üŒ¸.
004260     05 “d—Ã’P‰¿‚v’üŒ¸                 PIC 9(5) OCCURS 9 VALUE ZERO.
004270 01 —âãª–@’P‰¿‚r‚v’üŒ¸.
004280     05 —âãª–@’P‰¿‚v’üŒ¸               PIC 9(5) OCCURS 9 VALUE ZERO.
004290 01 ‰·ãª–@’P‰¿‚r‚v’üŒ¸.
004300     05 ‰·ãª–@’P‰¿‚v’üŒ¸               PIC 9(5) OCCURS 9 VALUE ZERO.
004310*
004320 01 ‰ŒŸ—¿‚f‚v                         PIC 9(5) VALUE ZERO.
004330 01 ‰ŒŸ‰ÁŽZ—¿‚f‚v                     PIC 9(6) VALUE ZERO.
004340 01 ‰‰ñˆ’u—¿‚f‚v                     PIC 9(5) VALUE ZERO.
004350 01 ÄŒŸ—¿‚f‚v                         PIC 9(5) VALUE ZERO.
004360 01 ‰—Ã—¿‚f‚v                         PIC 9(6) VALUE ZERO.
004370 01 ‰—Ã‰ÁŽZ—¿‚f‚v                     PIC 9(6) VALUE ZERO.
004380 01 ‰—Ã‰ñ”‚f‚v                       PIC 9(2) VALUE ZERO.
004390 01 Œã—Ã—¿‚f‚v                         PIC 9(6) VALUE ZERO.
004400 01 —âãª–@—¿‚f‚v                       PIC 9(4) VALUE ZERO.
004410 01 ‰·ãª–@—¿‚f‚v                       PIC 9(4) VALUE ZERO.
004420 01 “d—Ã—¿‚f‚v                         PIC 9(4) VALUE ZERO.
004430 01 ‹à‘®•›Žq—¿‚f‚v                     PIC 9(5) VALUE ZERO.
004440 01 Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v                 PIC 9(5) VALUE ZERO.
004450*/˜JÐ•ÛŒ¯—p
004460 01 Žw“±ŠÇ——¿‚f‚v                     PIC 9(4) VALUE ZERO.
004470 01 “Á•ÊÞ—¿”ï‚f‚v                     PIC 9(5) VALUE ZERO.
004480 01 ŒðŠ·•ï‘Ñ—¿‚f‚v                     PIC 9(5) VALUE ZERO.
004490 01 ‰^“®—Ã–@—¿‚f‚v                     PIC 9(5) VALUE ZERO.
004500 01 h”‘—¿‚f‚v                         PIC 9(5) VALUE ZERO.
004510 01 HŽ–—¿‚f‚v                         PIC 9(5) VALUE ZERO.
004520 01 ‹x‹ÆØ–¾‘—¿‚f‚v                   PIC 9(5) VALUE ZERO.
004530*/Ž©”…Ó•ÛŒ¯—p
004540 01 Ž{pØ–¾‘—¿‚f‚v                   PIC 9(5) VALUE ZERO.
004550 01 Ž{p–¾×‘—¿‚f‚v                   PIC 9(5) VALUE ZERO.
004560 01 ‚»‚Ì‘¼‹àŠz‚f‚v                     PIC 9(6) VALUE ZERO.
004570*
004580 01 •‰’SŠz‚f‚v                         PIC 9(5) VALUE ZERO.
004590 01 ”ï—pŠz˜Vl‚f‚v                     PIC 9(6) VALUE ZERO.
004600 01 •‰’SŠz˜Vl‚f‚v                     PIC 9(5) VALUE ZERO.
004610 01 ¿‹Šz˜Vl‚f‚v                     PIC 9(6) VALUE ZERO.
004620 01 •‰’SŠz•¬‚f‚v                     PIC 9(6) VALUE ZERO.
004630 01 ¿‹Šz•¬‚f‚v                     PIC 9(6) VALUE ZERO.
004640*
004650 01 ”ï—pŠz‚o‚f‚v                       PIC X(8) VALUE SPACE.
004660 01 •‰’SŠz‚o‚f‚v                       PIC X(8) VALUE SPACE.
004670 01 •‰’SŠz‚o‚f˜Vl‚v                   PIC X(8) VALUE SPACE.
004680 01 •‰’SŠz‚o‚f•¬‚v                   PIC X(8) VALUE SPACE.
004690*
004700 01 Œã—Ã—¿‚s‚v                         PIC 9(5) VALUE ZERO.
004710 01 —âãª–@—¿‚s‚v                       PIC 9(4) VALUE ZERO.
004720 01 ‰·ãª–@—¿‚s‚v                       PIC 9(4) VALUE ZERO.
004730 01 “d—Ã—¿‚s‚v                         PIC 9(4) VALUE ZERO.
004740 01 ‹à‘®•›Žq—¿‚v                       PIC 9(4) VALUE ZERO.
004750 01 ‰‰ñˆ’u—¿‚v                       PIC 9(5) VALUE ZERO.
004760 01 î•ñ’ñ‹Ÿ‰ñ”‚v                     PIC 9(2) VALUE ZERO.
004770 01 Ž{pî•ñ’ñ‹Ÿ—¿‚v                   PIC 9(4) VALUE ZERO.
004780*/˜JÐ•ÛŒ¯—pƒ[ƒN
004790 01 Žw“±ŠÇ—‰ñ”‚v                     PIC 9    VALUE ZERO.
004800 01 Žw“±ŠÇ——¿‚v                       PIC 9(4) VALUE ZERO.
004810 01 “Á•ÊÞ—¿”ï‚v                       PIC 9(5) VALUE ZERO.
004820 01 ŒðŠ·•ï‘Ñ—¿‚v                       PIC 9(5) VALUE ZERO.
004830 01 ‰^“®—Ã–@‰ñ”‚v                     PIC 9    VALUE ZERO.
004840 01 ‰^“®—Ã–@—¿‚v                       PIC 9(4) VALUE ZERO.
004850 01 h”‘‰ñ”‚v                         PIC 9(2) VALUE ZERO.
004860 01 h”‘—¿‚v                           PIC 9(4) VALUE ZERO.
004870 01 HŽ–‰ñ”‚v                         PIC 9(2) VALUE ZERO.
004880 01 HŽ–—¿‚v                           PIC 9(4) VALUE ZERO.
004890 01 ‹x‹ÆØ–¾‰ñ”‚v                     PIC 9(2) VALUE ZERO.
004900 01 ‹x‹ÆØ–¾‘—¿‚v                     PIC 9(4) VALUE ZERO.
004910*/Ž©”…Ó
004920 01 Ž{pØ–¾‘—¿‚v                     PIC 9(4) VALUE ZERO.
004930 01 Ž{pØ–¾‰ñ”‚v                     PIC 9(2) VALUE ZERO.
004940 01 Ž{p–¾×‘—¿‚v                     PIC 9(4) VALUE ZERO.
004950 01 Ž{p–¾×‰ñ”‚v                     PIC 9(2) VALUE ZERO.
004960 01 ‚»‚Ì‘¼‹àŠz‚v                       PIC 9(6) VALUE ZERO.
004970*
004980 01 Œã—Ã—¿‚v‚P                         PIC 9(5) VALUE ZERO.
004990 01 —âãª–@—¿‚v‚P                       PIC 9(4) VALUE ZERO.
005000 01 ‰·ãª–@—¿‚v‚P                       PIC 9(4) VALUE ZERO.
005010 01 “d—Ã—¿‚v‚P                         PIC 9(4) VALUE ZERO.
005020*
005030 01 ¼—ï”N‚v                           PIC 9(4) VALUE ZERO.
005040 01 ¼—ï”N·‚v                         PIC 9(4) VALUE ZERO.
005050 01 ŠJŽn”NŒŽâ‘Î’l‚v                   PIC 9(6) VALUE ZERO.
005060 01 Ž{p”NŒŽâ‘Î’l‚v                   PIC 9(6) VALUE ZERO.
005070 01 Ž{p”NŒŽ·‚v                       PIC 9(4) VALUE ZERO.
005080*
005090 01 •”ˆÊ”‚v                           PIC 9(2) VALUE ZERO.
005100 01 Œp‘±•”ˆÊ”‚v                       PIC 9(2) VALUE ZERO.
005110*
005120* ŒvŽZî•ñƒ[ƒN
005130* Šî–{
005140 01 ‰—ÃŠî–{‹——£‚j‚v                   PIC 9(2) VALUE ZERO.
005150 01 Œã—Ã‰Â”\ŽZ’è”‚j‚v                 PIC 9(2) VALUE ZERO.
005160*
005170 01 ’üŒ¸—¦‚r‚j‚v.
005180   03 ’üŒ¸—¦‚j‚v  OCCURS 9.
005190     05 ‘½•”ˆÊ’üŒ¸—¦‚j‚v               PIC 9(3) VALUE ZERO.
005200*
005210 01 •”ˆÊ’üŒ¸‚r‚v.
005220   03 •”ˆÊ’üŒ¸‚v  OCCURS 9.
005230     05 •”ˆÊ’üŒ¸î•ñ‚v                 PIC 9(2) VALUE ZERO.
005240*
005250 01 ’·Šú‹æ•ª‚r‚v.
005260    03 ’·Šú‹æ•ª‚v                      PIC 9(2) OCCURS 9 VALUE ZERO.
005270*
005280 01 ‰—Ã‰ÁŽZ—¦’†ŠÔ‚v                   PIC 9V9(3) VALUE ZERO.
005290*
005300 01 ’·Šú’üŒ¸—¦’†ŠÔ‚j‚v                 PIC 9V9(3) VALUE ZERO.
005310 01 ’·Šú’üŒ¸—¦‚j‚v                     PIC 9(3) VALUE ZERO.
005320 01 ’·Šú’üŒ¸ŒŽ‹«ŠE“ú‚j‚v               PIC 9(2) VALUE ZERO.
005330 01 ’·Šú’üŒ¸ŒŽ”‚j‚v                   PIC 9(2) VALUE ZERO.
005340 01 ˜Vl•‰’S‰ñ”‚j‚v                   PIC 9(2) VALUE ZERO.
005350 01 ’üŒ¸—¦’†ŠÔŒ‹‰Ê‚v                   PIC 9V9(3) VALUE ZERO.
005360* —¿‹àî•ñƒ[ƒN
005370* Šî–{
005380 01 ‰ŒŸ—¿‚`‚v                         PIC 9(4) VALUE ZERO.
005390 01 ŽžŠÔŠO‚`‚v                         PIC 9(4) VALUE ZERO.
005400 01 ‹x“ú‚`‚v                           PIC 9(4) VALUE ZERO.
005410 01 [–é‚`‚v                           PIC 9(4) VALUE ZERO.
005420 01 ‰—ÃŠî–{—¿‹à‚`‚v                   PIC 9(4) VALUE ZERO.
005430 01 ‰—Ã’Ç‰Á—¿‹à‚`‚v                   PIC 9(4) VALUE ZERO.
005440 01 ‰—Ã‰ÁŽZ—¦‚`‚v                     PIC 9(3) VALUE ZERO.
005450 01 ‰—Ã[–é‰ÁŽZ—¦‚`‚v                 PIC 9(3) VALUE ZERO.
005460 01 ‰—Ã“ï˜H‰ÁŽZ—¦‚`‚v                 PIC 9(3) VALUE ZERO.
005470 01 ‰—Ã–\•—‰J‰ÁŽZ—¦‚`‚v               PIC 9(3) VALUE ZERO.
005480 01 ÄŒŸ—¿‚`‚v                         PIC 9(4) VALUE ZERO.
005490 01 —âãª–@—¿‚`‚v                       PIC 9(4) VALUE ZERO.
005500 01 ‰·ãª–@—¿‚`‚v                       PIC 9(4) VALUE ZERO.
005510 01 “d—Ã—¿‚`‚v                         PIC 9(4) VALUE ZERO.
005520 01 ‘å‚`‚v                             PIC 9(4) VALUE ZERO.
005530 01 ’†‚`‚v                             PIC 9(4) VALUE ZERO.
005540 01 ¬‚`‚v                             PIC 9(4) VALUE ZERO.
005550 01 Ž{pî•ñ’ñ‹Ÿ—¿‚`‚v                 PIC 9(4) VALUE ZERO.
005560* •‰•Ê
005570 01 SkŒã—Ã—¿‚a‚v                     PIC 9(5) VALUE ZERO.
005580 01 Œã—Ã—¿‚a‚v                         PIC 9(5) VALUE ZERO.
005590 01 ®•œ—¿‚a‚v                         PIC 9(5) VALUE ZERO.
005600*
005610 01 •”ˆÊƒR[ƒh‚v.
005620    03 •‰Ží•Ê‚v                      PIC 9(2).
005630    03 •”ˆÊ‚v                          PIC 9(2).
005640    03 ¶‰E‹æ•ª‚v                      PIC 9.
005650    03 •‰ˆÊ’u”Ô†‚v                  PIC 9(2).
005660*
005670 01 •”ˆÊƒR[ƒh‚b‚v.
005680    03 •‰Ží•Ê‚b‚v                    PIC 9(2).
005690    03 •”ˆÊ‚b‚v                        PIC 9(2).
005700*
005710 01 Š³ŽÒƒR[ƒh‚v.
005720     03 Š³ŽÒ”Ô†‚v                     PIC 9(6).
005730     03 Ž}”Ô‚v                         PIC X.
005740*
005750 01 Š³ŽÒƒR[ƒh‚b‚v.
005760     03 Š³ŽÒ”Ô†‚b‚v                   PIC 9(6).
005770     03 Ž}”Ô‚b‚v                       PIC X.
005780*
005790 01 Ž{p˜a—ï”NŒŽ“ú‚b‚v.
005800   03 Ž{p˜a—ï”NŒŽ‚b‚v.
005810     05 Ž{p˜a—ï‚b‚v                   PIC 9.
005820     05 Ž{p”NŒŽ‚b‚v.
005830        07 Ž{p”N‚b‚v                  PIC 9(2).
005840        07 Ž{pŒŽ‚b‚v                  PIC 9(2).
005850   03 Ž{p“ú‚b‚v                       PIC 9(2).
005860*
005870 01 Ž{p˜a—ï”NŒŽ“ú‚c‚v.
005880   03 Ž{p˜a—ï”NŒŽ‚c‚v.
005890     05 Ž{p˜a—ï‚c‚v                   PIC 9.
005900     05 Ž{p”NŒŽ‚c‚v.
005910        07 Ž{p”N‚c‚v                  PIC 9(2).
005920        07 Ž{pŒŽ‚c‚v                  PIC 9(2).
005930   03 Ž{p“ú‚c‚v                       PIC 9(2).
005940*
005950 01 Ž{p˜a—ï”NŒŽ“ú‚v.
005960   03 Ž{p˜a—ï”NŒŽ‚v.
005970     05 Ž{p˜a—ï‚v                     PIC 9.
005980     05 Ž{p”NŒŽ‚v.
005990        07 Ž{p”N‚v                    PIC 9(2).
006000        07 Ž{pŒŽ‚v                    PIC 9(2).
006010   03 Ž{p“ú‚v                         PIC 9(2).
006020*
006030 01 ŠJŽn˜a—ï”NŒŽ“ú‚v.
006040   03 ŠJŽn˜a—ï”NŒŽ‚v.
006050     05 ŠJŽn˜a—ï‚v                     PIC 9.
006060     05 ŠJŽn”NŒŽ‚v.
006070        07 ŠJŽn”N‚v                    PIC 9(2).
006080        07 ŠJŽnŒŽ‚v                    PIC 9(2).
006090   03 ŠJŽn“ú‚v                         PIC 9(2).
006100*
006110 01 I—¹˜a—ï”NŒŽ“ú‚v.
006120   03 I—¹˜a—ï”NŒŽ‚v.
006130     05 I—¹˜a—ï‚v                     PIC 9.
006140     05 I—¹”NŒŽ‚v.
006150        07 I—¹”N‚v                    PIC 9(2).
006160        07 I—¹ŒŽ‚v                    PIC 9(2).
006170   03 I—¹“ú‚v                         PIC 9(2).
006180*
006190* •”ˆÊ–ˆ‚Ì‘½•”ˆÊ’üŒ¸•”ˆÊ–ˆ‚Ìƒ[ƒN
006200* ‚P•”ˆÊ–Ú
006210 01 Œã—Ã‰ñ”‚P‚q‚v                     PIC 9(2) VALUE ZERO.
006220 01 Œã—Ã—¿‚P‚q‚v                       PIC 9(5) VALUE ZERO.
006230 01 —âãª–@‰ñ”‚P‚q‚v                   PIC 9(2) VALUE ZERO.
006240 01 —âãª–@—¿‚P‚q‚v                     PIC 9(4) VALUE ZERO.
006250 01 ‰·ãª–@‰ñ”‚P‚q‚v                   PIC 9(2) VALUE ZERO.
006260 01 ‰·ãª–@—¿‚P‚q‚v                     PIC 9(4) VALUE ZERO.
006270 01 “d—Ã‰ñ”‚P‚q‚v                     PIC 9(2) VALUE ZERO.
006280 01 “d—Ã—¿‚P‚q‚v                       PIC 9(5) VALUE ZERO.
006290 01 ¬Œv‚P‚q‚v                         PIC 9(6) VALUE ZERO.
006300* ‚P•”ˆÊ–ÚSk
006310 01 Œã—Ã‰ñ”‚PSk‚q‚v                 PIC 9(2) VALUE ZERO.
006320 01 Œã—Ã—¿‚PSk‚q‚v                   PIC 9(5) VALUE ZERO.
006330 01 —âãª–@‰ñ”‚PSk‚q‚v               PIC 9(2) VALUE ZERO.
006340 01 —âãª–@—¿‚PSk‚q‚v                 PIC 9(4) VALUE ZERO.
006350 01 ‰·ãª–@‰ñ”‚PSk‚q‚v               PIC 9(2) VALUE ZERO.
006360 01 ‰·ãª–@—¿‚PSk‚q‚v                 PIC 9(4) VALUE ZERO.
006370 01 “d—Ã‰ñ”‚PSk‚q‚v                 PIC 9(2) VALUE ZERO.
006380 01 “d—Ã—¿‚PSk‚q‚v                   PIC 9(5) VALUE ZERO.
006390 01 ¬Œv‚PSk‚q‚v                     PIC 9(6) VALUE ZERO.
006400* ‚P•”ˆÊ–Ú‘½•”ˆÊ
006410 01 ‘½•”ˆÊŒã—Ã‰ñ”‚P‚q‚v               PIC 9(2) VALUE ZERO.
006420 01 ‘½•”ˆÊŒã—Ã—¿‚P‚q‚v                 PIC 9(5) VALUE ZERO.
006430* ‚P•”ˆÊ–Ú’·Šú
006440 01 ’·ŠúŒã—Ã‰ñ”‚P‚q‚v                 PIC 9(2) VALUE ZERO.
006450 01 ’·ŠúŒã—Ã—¿‚P‚q‚v                   PIC 9(5) VALUE ZERO.
006460* ‚P•”ˆÊ–Ú‘½•”ˆÊSk
006470 01 ‘½•”ˆÊSkŒã—Ã‰ñ”‚P‚q‚v           PIC 9(2) VALUE ZERO.
006480 01 ‘½•”ˆÊSkŒã—Ã—¿‚P‚q‚v             PIC 9(5) VALUE ZERO.
006490
006500
006510* ‚Q•”ˆÊ–Ú
006520 01 Œã—Ã‰ñ”‚Q‚q‚v                     PIC 9(2) VALUE ZERO.
006530 01 Œã—Ã—¿‚Q‚q‚v                       PIC 9(5) VALUE ZERO.
006540 01 —âãª–@‰ñ”‚Q‚q‚v                   PIC 9(2) VALUE ZERO.
006550 01 —âãª–@—¿‚Q‚q‚v                     PIC 9(4) VALUE ZERO.
006560 01 ‰·ãª–@‰ñ”‚Q‚q‚v                   PIC 9(2) VALUE ZERO.
006570 01 ‰·ãª–@—¿‚Q‚q‚v                     PIC 9(4) VALUE ZERO.
006580 01 “d—Ã‰ñ”‚Q‚q‚v                     PIC 9(2) VALUE ZERO.
006590 01 “d—Ã—¿‚Q‚q‚v                       PIC 9(5) VALUE ZERO.
006600 01 ¬Œv‚Q‚q‚v                         PIC 9(6) VALUE ZERO.
006610* ‚Q•”ˆÊ–ÚSk
006620 01 Œã—Ã‰ñ”‚QSk‚q‚v                 PIC 9(2) VALUE ZERO.
006630 01 Œã—Ã—¿‚QSk‚q‚v                   PIC 9(5) VALUE ZERO.
006640 01 —âãª–@‰ñ”‚QSk‚q‚v               PIC 9(2) VALUE ZERO.
006650 01 —âãª–@—¿‚QSk‚q‚v                 PIC 9(4) VALUE ZERO.
006660 01 ‰·ãª–@‰ñ”‚QSk‚q‚v               PIC 9(2) VALUE ZERO.
006670 01 ‰·ãª–@—¿‚QSk‚q‚v                 PIC 9(4) VALUE ZERO.
006680 01 “d—Ã‰ñ”‚QSk‚q‚v                 PIC 9(2) VALUE ZERO.
006690 01 “d—Ã—¿‚QSk‚q‚v                   PIC 9(5) VALUE ZERO.
006700 01 ¬Œv‚QSk‚q‚v                     PIC 9(6) VALUE ZERO.
006710* ‚Q•”ˆÊ–Ú‘½•”ˆÊ
006720 01 ‘½•”ˆÊŒã—Ã‰ñ”‚Q‚q‚v               PIC 9(2) VALUE ZERO.
006730 01 ‘½•”ˆÊŒã—Ã—¿‚Q‚q‚v                 PIC 9(5) VALUE ZERO.
006740* ‚Q•”ˆÊ–Ú’·Šú
006750 01 ’·ŠúŒã—Ã‰ñ”‚Q‚q‚v                 PIC 9(2) VALUE ZERO.
006760 01 ’·ŠúŒã—Ã—¿‚Q‚q‚v                   PIC 9(5) VALUE ZERO.
006770* ‚Q•”ˆÊ–Ú‘½•”ˆÊSk
006780 01 ‘½•”ˆÊSkŒã—Ã‰ñ”‚Q‚q‚v           PIC 9(2) VALUE ZERO.
006790 01 ‘½•”ˆÊSkŒã—Ã—¿‚Q‚q‚v             PIC 9(5) VALUE ZERO.
006800
006810* ‚R•”ˆÊ–Ú
006820 01 Œã—Ã‰ñ”‚R‚q‚v                     PIC 9(2) VALUE ZERO.
006830 01 Œã—Ã—¿‚R‚q‚v                       PIC 9(5) VALUE ZERO.
006840 01 —âãª–@‰ñ”‚R‚q‚v                   PIC 9(2) VALUE ZERO.
006850 01 —âãª–@—¿‚R‚q‚v                     PIC 9(4) VALUE ZERO.
006860 01 ‰·ãª–@‰ñ”‚R‚q‚v                   PIC 9(2) VALUE ZERO.
006870 01 ‰·ãª–@—¿‚R‚q‚v                     PIC 9(4) VALUE ZERO.
006880 01 “d—Ã‰ñ”‚R‚q‚v                     PIC 9(2) VALUE ZERO.
006890 01 “d—Ã—¿‚R‚q‚v                       PIC 9(5) VALUE ZERO.
006900 01 ¬Œv‚R‚q‚v                         PIC 9(6) VALUE ZERO.
006910* ‚R•”ˆÊ–ÚSk
006920 01 Œã—Ã‰ñ”‚RSk‚q‚v                     PIC 9(2) VALUE ZERO.
006930 01 Œã—Ã—¿‚RSk‚q‚v                       PIC 9(5) VALUE ZERO.
006940 01 —âãª–@‰ñ”‚RSk‚q‚v                   PIC 9(2) VALUE ZERO.
006950 01 —âãª–@—¿‚RSk‚q‚v                     PIC 9(4) VALUE ZERO.
006960 01 ‰·ãª–@‰ñ”‚RSk‚q‚v                   PIC 9(2) VALUE ZERO.
006970 01 ‰·ãª–@—¿‚RSk‚q‚v                     PIC 9(4) VALUE ZERO.
006980 01 “d—Ã‰ñ”‚RSk‚q‚v                     PIC 9(2) VALUE ZERO.
006990 01 “d—Ã—¿‚RSk‚q‚v                       PIC 9(5) VALUE ZERO.
007000 01 ¬Œv‚RSk‚q‚v                         PIC 9(6) VALUE ZERO.
007010* ‚R•”ˆÊ–Ú‘½•”ˆÊ
007020 01 ‘½•”ˆÊŒã—Ã‰ñ”‚R‚q‚v               PIC 9(2) VALUE ZERO.
007030 01 ‘½•”ˆÊŒã—Ã—¿‚R‚q‚v                 PIC 9(5) VALUE ZERO.
007040* ‚R•”ˆÊ–Ú’·Šú
007050 01 ’·ŠúŒã—Ã‰ñ”‚R‚q‚v                 PIC 9(2) VALUE ZERO.
007060 01 ’·ŠúŒã—Ã—¿‚R‚q‚v                   PIC 9(5) VALUE ZERO.
007070* ‚R•”ˆÊ–Ú‘½•”ˆÊSk
007080 01 ‘½•”ˆÊSkŒã—Ã‰ñ”‚R‚q‚v           PIC 9(2) VALUE ZERO.
007090 01 ‘½•”ˆÊSkŒã—Ã—¿‚R‚q‚v             PIC 9(5) VALUE ZERO.
007100
007110* ‚S•”ˆÊ–Ú
007120 01 Œã—Ã‰ñ”‚S‚q‚v                     PIC 9(2) VALUE ZERO.
007130 01 Œã—Ã—¿‚S‚q‚v                       PIC 9(5) VALUE ZERO.
007140 01 —âãª–@‰ñ”‚S‚q‚v                   PIC 9(2) VALUE ZERO.
007150 01 —âãª–@—¿‚S‚q‚v                     PIC 9(4) VALUE ZERO.
007160 01 ‰·ãª–@‰ñ”‚S‚q‚v                   PIC 9(2) VALUE ZERO.
007170 01 ‰·ãª–@—¿‚S‚q‚v                     PIC 9(4) VALUE ZERO.
007180 01 “d—Ã‰ñ”‚S‚q‚v                     PIC 9(2) VALUE ZERO.
007190 01 “d—Ã—¿‚S‚q‚v                       PIC 9(5) VALUE ZERO.
007200 01 ¬Œv‚S‚q‚v                         PIC 9(6) VALUE ZERO.
007210* ‚S•”ˆÊ–ÚSk
007220 01 Œã—Ã‰ñ”‚SSk‚q‚v                     PIC 9(2) VALUE ZERO.
007230 01 Œã—Ã—¿‚SSk‚q‚v                       PIC 9(5) VALUE ZERO.
007240 01 —âãª–@‰ñ”‚SSk‚q‚v                   PIC 9(2) VALUE ZERO.
007250 01 —âãª–@—¿‚SSk‚q‚v                     PIC 9(4) VALUE ZERO.
007260 01 ‰·ãª–@‰ñ”‚SSk‚q‚v                   PIC 9(2) VALUE ZERO.
007270 01 ‰·ãª–@—¿‚SSk‚q‚v                     PIC 9(4) VALUE ZERO.
007280 01 “d—Ã‰ñ”‚SSk‚q‚v                     PIC 9(2) VALUE ZERO.
007290 01 “d—Ã—¿‚SSk‚q‚v                       PIC 9(5) VALUE ZERO.
007300 01 ¬Œv‚SSk‚q‚v                         PIC 9(6) VALUE ZERO.
007310* ‚S•”ˆÊ–Ú‘½•”ˆÊ
007320 01 ‘½•”ˆÊŒã—Ã‰ñ”‚S‚q‚v               PIC 9(2) VALUE ZERO.
007330 01 ‘½•”ˆÊŒã—Ã—¿‚S‚q‚v                 PIC 9(5) VALUE ZERO.
007340* ‚S•”ˆÊ–Ú’·Šú
007350 01 ’·ŠúŒã—Ã‰ñ”‚S‚q‚v                 PIC 9(2) VALUE ZERO.
007360 01 ’·ŠúŒã—Ã—¿‚S‚q‚v                   PIC 9(5) VALUE ZERO.
007370* ‚S•”ˆÊ–Ú‘½•”ˆÊSk
007380 01 ‘½•”ˆÊSkŒã—Ã‰ñ”‚S‚q‚v           PIC 9(2) VALUE ZERO.
007390 01 ‘½•”ˆÊSkŒã—Ã—¿‚S‚q‚v             PIC 9(5) VALUE ZERO.
007400
007410* ‚T•”ˆÊ–Ú
007420 01 Œã—Ã‰ñ”‚T‚q‚v                     PIC 9(2) VALUE ZERO.
007430 01 Œã—Ã—¿‚T‚q‚v                       PIC 9(5) VALUE ZERO.
007440 01 —âãª–@‰ñ”‚T‚q‚v                   PIC 9(2) VALUE ZERO.
007450 01 —âãª–@—¿‚T‚q‚v                     PIC 9(4) VALUE ZERO.
007460 01 ‰·ãª–@‰ñ”‚T‚q‚v                   PIC 9(2) VALUE ZERO.
007470 01 ‰·ãª–@—¿‚T‚q‚v                     PIC 9(4) VALUE ZERO.
007480 01 “d—Ã‰ñ”‚T‚q‚v                     PIC 9(2) VALUE ZERO.
007490 01 “d—Ã—¿‚T‚q‚v                       PIC 9(5) VALUE ZERO.
007500 01 ¬Œv‚T‚q‚v                         PIC 9(6) VALUE ZERO.
007510* ‚T•”ˆÊ–ÚSk
007520 01 Œã—Ã‰ñ”‚TSk‚q‚v                     PIC 9(2) VALUE ZERO.
007530 01 Œã—Ã—¿‚TSk‚q‚v                       PIC 9(5) VALUE ZERO.
007540 01 —âãª–@‰ñ”‚TSk‚q‚v                   PIC 9(2) VALUE ZERO.
007550 01 —âãª–@—¿‚TSk‚q‚v                     PIC 9(4) VALUE ZERO.
007560 01 ‰·ãª–@‰ñ”‚TSk‚q‚v                   PIC 9(2) VALUE ZERO.
007570 01 ‰·ãª–@—¿‚TSk‚q‚v                     PIC 9(4) VALUE ZERO.
007580 01 “d—Ã‰ñ”‚TSk‚q‚v                     PIC 9(2) VALUE ZERO.
007590 01 “d—Ã—¿‚TSk‚q‚v                       PIC 9(5) VALUE ZERO.
007600 01 ¬Œv‚TSk‚q‚v                         PIC 9(6) VALUE ZERO.
007610* ‚T•”ˆÊ–Ú‘½•”ˆÊ
007620 01 ‘½•”ˆÊŒã—Ã‰ñ”‚T‚q‚v               PIC 9(2) VALUE ZERO.
007630 01 ‘½•”ˆÊŒã—Ã—¿‚T‚q‚v                 PIC 9(5) VALUE ZERO.
007640* ‚T•”ˆÊ–Ú’·Šú
007650 01 ’·ŠúŒã—Ã‰ñ”‚T‚q‚v                 PIC 9(2) VALUE ZERO.
007660 01 ’·ŠúŒã—Ã—¿‚T‚q‚v                   PIC 9(5) VALUE ZERO.
007670* ‚T•”ˆÊ–Ú‘½•”ˆÊSk
007680 01 ‘½•”ˆÊSkŒã—Ã‰ñ”‚T‚q‚v           PIC 9(2) VALUE ZERO.
007690 01 ‘½•”ˆÊSkŒã—Ã—¿‚T‚q‚v             PIC 9(5) VALUE ZERO.
007700
007710* ‚U•”ˆÊ–Ú
007720 01 Œã—Ã‰ñ”‚U‚q‚v                     PIC 9(2) VALUE ZERO.
007730 01 Œã—Ã—¿‚U‚q‚v                       PIC 9(5) VALUE ZERO.
007740 01 —âãª–@‰ñ”‚U‚q‚v                   PIC 9(2) VALUE ZERO.
007750 01 —âãª–@—¿‚U‚q‚v                     PIC 9(4) VALUE ZERO.
007760 01 ‰·ãª–@‰ñ”‚U‚q‚v                   PIC 9(2) VALUE ZERO.
007770 01 ‰·ãª–@—¿‚U‚q‚v                     PIC 9(4) VALUE ZERO.
007780 01 “d—Ã‰ñ”‚U‚q‚v                     PIC 9(2) VALUE ZERO.
007790 01 “d—Ã—¿‚U‚q‚v                       PIC 9(5) VALUE ZERO.
007800 01 ¬Œv‚U‚q‚v                         PIC 9(6) VALUE ZERO.
007810* ‚U•”ˆÊ–ÚSk
007820 01 Œã—Ã‰ñ”‚USk‚q‚v                     PIC 9(2) VALUE ZERO.
007830 01 Œã—Ã—¿‚USk‚q‚v                       PIC 9(5) VALUE ZERO.
007840 01 —âãª–@‰ñ”‚USk‚q‚v                   PIC 9(2) VALUE ZERO.
007850 01 —âãª–@—¿‚USk‚q‚v                     PIC 9(4) VALUE ZERO.
007860 01 ‰·ãª–@‰ñ”‚USk‚q‚v                   PIC 9(2) VALUE ZERO.
007870 01 ‰·ãª–@—¿‚USk‚q‚v                     PIC 9(4) VALUE ZERO.
007880 01 “d—Ã‰ñ”‚USk‚q‚v                     PIC 9(2) VALUE ZERO.
007890 01 “d—Ã—¿‚USk‚q‚v                       PIC 9(5) VALUE ZERO.
007900 01 ¬Œv‚USk‚q‚v                         PIC 9(6) VALUE ZERO.
007910* ‚U•”ˆÊ–Ú‘½•”ˆÊ
007920 01 ‘½•”ˆÊŒã—Ã‰ñ”‚U‚q‚v               PIC 9(2) VALUE ZERO.
007930 01 ‘½•”ˆÊŒã—Ã—¿‚U‚q‚v                 PIC 9(5) VALUE ZERO.
007940* ‚U•”ˆÊ–Ú’·Šú
007950 01 ’·ŠúŒã—Ã‰ñ”‚U‚q‚v                 PIC 9(2) VALUE ZERO.
007960 01 ’·ŠúŒã—Ã—¿‚U‚q‚v                   PIC 9(5) VALUE ZERO.
007970* ‚U•”ˆÊ–Ú‘½•”ˆÊSk
007980 01 ‘½•”ˆÊSkŒã—Ã‰ñ”‚U‚q‚v           PIC 9(2) VALUE ZERO.
007990 01 ‘½•”ˆÊSkŒã—Ã—¿‚U‚q‚v             PIC 9(5) VALUE ZERO.
008000
008010* ‚V•”ˆÊ–Ú
008020 01 Œã—Ã‰ñ”‚V‚q‚v                     PIC 9(2) VALUE ZERO.
008030 01 Œã—Ã—¿‚V‚q‚v                       PIC 9(5) VALUE ZERO.
008040 01 —âãª–@‰ñ”‚V‚q‚v                   PIC 9(2) VALUE ZERO.
008050 01 —âãª–@—¿‚V‚q‚v                     PIC 9(4) VALUE ZERO.
008060 01 ‰·ãª–@‰ñ”‚V‚q‚v                   PIC 9(2) VALUE ZERO.
008070 01 ‰·ãª–@—¿‚V‚q‚v                     PIC 9(4) VALUE ZERO.
008080 01 “d—Ã‰ñ”‚V‚q‚v                     PIC 9(2) VALUE ZERO.
008090 01 “d—Ã—¿‚V‚q‚v                       PIC 9(5) VALUE ZERO.
008100 01 ¬Œv‚V‚q‚v                         PIC 9(6) VALUE ZERO.
008110* ‚V•”ˆÊ–ÚSk
008120 01 Œã—Ã‰ñ”‚VSk‚q‚v                     PIC 9(2) VALUE ZERO.
008130 01 Œã—Ã—¿‚VSk‚q‚v                       PIC 9(5) VALUE ZERO.
008140 01 —âãª–@‰ñ”‚VSk‚q‚v                   PIC 9(2) VALUE ZERO.
008150 01 —âãª–@—¿‚VSk‚q‚v                     PIC 9(4) VALUE ZERO.
008160 01 ‰·ãª–@‰ñ”‚VSk‚q‚v                   PIC 9(2) VALUE ZERO.
008170 01 ‰·ãª–@—¿‚VSk‚q‚v                     PIC 9(4) VALUE ZERO.
008180 01 “d—Ã‰ñ”‚VSk‚q‚v                     PIC 9(2) VALUE ZERO.
008190 01 “d—Ã—¿‚VSk‚q‚v                       PIC 9(5) VALUE ZERO.
008200 01 ¬Œv‚VSk‚q‚v                         PIC 9(6) VALUE ZERO.
008210* ‚V•”ˆÊ–Ú‘½•”ˆÊ
008220 01 ‘½•”ˆÊŒã—Ã‰ñ”‚V‚q‚v               PIC 9(2) VALUE ZERO.
008230 01 ‘½•”ˆÊŒã—Ã—¿‚V‚q‚v                 PIC 9(5) VALUE ZERO.
008240* ‚V•”ˆÊ–Ú’·Šú
008250 01 ’·ŠúŒã—Ã‰ñ”‚V‚q‚v                 PIC 9(2) VALUE ZERO.
008260 01 ’·ŠúŒã—Ã—¿‚V‚q‚v                   PIC 9(5) VALUE ZERO.
008270* ‚V•”ˆÊ–Ú‘½•”ˆÊSk
008280 01 ‘½•”ˆÊSkŒã—Ã‰ñ”‚V‚q‚v           PIC 9(2) VALUE ZERO.
008290 01 ‘½•”ˆÊSkŒã—Ã—¿‚V‚q‚v             PIC 9(5) VALUE ZERO.
008300
008310* ‚W•”ˆÊ–Ú
008320 01 Œã—Ã‰ñ”‚W‚q‚v                     PIC 9(2) VALUE ZERO.
008330 01 Œã—Ã—¿‚W‚q‚v                       PIC 9(5) VALUE ZERO.
008340 01 —âãª–@‰ñ”‚W‚q‚v                   PIC 9(2) VALUE ZERO.
008350 01 —âãª–@—¿‚W‚q‚v                     PIC 9(4) VALUE ZERO.
008360 01 ‰·ãª–@‰ñ”‚W‚q‚v                   PIC 9(2) VALUE ZERO.
008370 01 ‰·ãª–@—¿‚W‚q‚v                     PIC 9(4) VALUE ZERO.
008380 01 “d—Ã‰ñ”‚W‚q‚v                     PIC 9(2) VALUE ZERO.
008390 01 “d—Ã—¿‚W‚q‚v                       PIC 9(5) VALUE ZERO.
008400 01 ¬Œv‚W‚q‚v                         PIC 9(6) VALUE ZERO.
008410* ‚W•”ˆÊ–ÚSk
008420 01 Œã—Ã‰ñ”‚WSk‚q‚v                     PIC 9(2) VALUE ZERO.
008430 01 Œã—Ã—¿‚WSk‚q‚v                       PIC 9(5) VALUE ZERO.
008440 01 —âãª–@‰ñ”‚WSk‚q‚v                   PIC 9(2) VALUE ZERO.
008450 01 —âãª–@—¿‚WSk‚q‚v                     PIC 9(4) VALUE ZERO.
008460 01 ‰·ãª–@‰ñ”‚WSk‚q‚v                   PIC 9(2) VALUE ZERO.
008470 01 ‰·ãª–@—¿‚WSk‚q‚v                     PIC 9(4) VALUE ZERO.
008480 01 “d—Ã‰ñ”‚WSk‚q‚v                     PIC 9(2) VALUE ZERO.
008490 01 “d—Ã—¿‚WSk‚q‚v                       PIC 9(5) VALUE ZERO.
008500 01 ¬Œv‚WSk‚q‚v                         PIC 9(6) VALUE ZERO.
008510* ‚W•”ˆÊ–Ú‘½•”ˆÊ
008520 01 ‘½•”ˆÊŒã—Ã‰ñ”‚W‚q‚v               PIC 9(2) VALUE ZERO.
008530 01 ‘½•”ˆÊŒã—Ã—¿‚W‚q‚v                 PIC 9(5) VALUE ZERO.
008540* ‚W•”ˆÊ–Ú’·Šú
008550 01 ’·ŠúŒã—Ã‰ñ”‚W‚q‚v                 PIC 9(2) VALUE ZERO.
008560 01 ’·ŠúŒã—Ã—¿‚W‚q‚v                   PIC 9(5) VALUE ZERO.
008570* ‚W•”ˆÊ–Ú‘½•”ˆÊSk
008580 01 ‘½•”ˆÊSkŒã—Ã‰ñ”‚W‚q‚v           PIC 9(2) VALUE ZERO.
008590 01 ‘½•”ˆÊSkŒã—Ã—¿‚W‚q‚v             PIC 9(5) VALUE ZERO.
008600
008610* ‚X•”ˆÊ–Ú
008620 01 Œã—Ã‰ñ”‚X‚q‚v                     PIC 9(2) VALUE ZERO.
008630 01 Œã—Ã—¿‚X‚q‚v                       PIC 9(5) VALUE ZERO.
008640 01 —âãª–@‰ñ”‚X‚q‚v                   PIC 9(2) VALUE ZERO.
008650 01 —âãª–@—¿‚X‚q‚v                     PIC 9(4) VALUE ZERO.
008660 01 ‰·ãª–@‰ñ”‚X‚q‚v                   PIC 9(2) VALUE ZERO.
008670 01 ‰·ãª–@—¿‚X‚q‚v                     PIC 9(4) VALUE ZERO.
008680 01 “d—Ã‰ñ”‚X‚q‚v                     PIC 9(2) VALUE ZERO.
008690 01 “d—Ã—¿‚X‚q‚v                       PIC 9(5) VALUE ZERO.
008700 01 ¬Œv‚X‚q‚v                         PIC 9(6) VALUE ZERO.
008710* ‚X•”ˆÊ–ÚSk
008720 01 Œã—Ã‰ñ”‚XSk‚q‚v                     PIC 9(2) VALUE ZERO.
008730 01 Œã—Ã—¿‚XSk‚q‚v                       PIC 9(5) VALUE ZERO.
008740 01 —âãª–@‰ñ”‚XSk‚q‚v                   PIC 9(2) VALUE ZERO.
008750 01 —âãª–@—¿‚XSk‚q‚v                     PIC 9(4) VALUE ZERO.
008760 01 ‰·ãª–@‰ñ”‚XSk‚q‚v                   PIC 9(2) VALUE ZERO.
008770 01 ‰·ãª–@—¿‚XSk‚q‚v                     PIC 9(4) VALUE ZERO.
008780 01 “d—Ã‰ñ”‚XSk‚q‚v                     PIC 9(2) VALUE ZERO.
008790 01 “d—Ã—¿‚XSk‚q‚v                       PIC 9(5) VALUE ZERO.
008800 01 ¬Œv‚XSk‚q‚v                         PIC 9(6) VALUE ZERO.
008810* ‚X•”ˆÊ–Ú‘½•”ˆÊ
008820 01 ‘½•”ˆÊŒã—Ã‰ñ”‚X‚q‚v               PIC 9(2) VALUE ZERO.
008830 01 ‘½•”ˆÊŒã—Ã—¿‚X‚q‚v                 PIC 9(5) VALUE ZERO.
008840* ‚X•”ˆÊ–Ú’·Šú
008850 01 ’·ŠúŒã—Ã‰ñ”‚X‚q‚v                 PIC 9(2) VALUE ZERO.
008860 01 ’·ŠúŒã—Ã—¿‚X‚q‚v                   PIC 9(5) VALUE ZERO.
008870* ‚X•”ˆÊ–Ú‘½•”ˆÊSk
008880 01 ‘½•”ˆÊSkŒã—Ã‰ñ”‚X‚q‚v           PIC 9(2) VALUE ZERO.
008890 01 ‘½•”ˆÊSkŒã—Ã—¿‚X‚q‚v             PIC 9(5) VALUE ZERO.
008900
008910*
008920 01 •‰Ží•Ê—ªÌ‚v.
008930    03 •‰Ží•Ê—ªÌ‚v‚P                PIC N(4).
008940    03 FILLER                          PIC N(2).
008950 01 Œ³†–¼Ì‚v.
008960    03 Œ³†–¼Ì‚v‚P                    PIC N(2).
008970    03 FILLER                          PIC N(4).
008980 01 “]‹A‹æ•ª—ªÌ‚v.
008990    03 “]‹A‹æ•ª—ªÌ‚v‚P                PIC N(4).
009000    03 FILLER                          PIC N(2).
009010*
009020 01  •ÛŒ¯ŽÒ”Ô†‚v‚Q.
009030     03  •Û”Ô‚P‚v                      PIC X(2).
009040     03  •Û”Ô‚Q‚v                      PIC X(8).
009050* ”ï—p•‰’SŽÒ”Ô†iŽs’¬‘º”Ô†j‚v‚n‚q‚j
009060 01 ”ï—p•‰’SŽÒ”Ô†‚v.
009070     03  –@•Ê”Ô†‚v                    PIC X(2).
009080     03  ••ÛŒ¯‚v.
009090          05  •{Œ§‚v                   PIC X(2).
009100          05  •”Ô‚v                   PIC X     OCCURS 3.
009110          05  •‚b‚c‚v                 PIC X.
009120     03  FILLER                        PIC X(2).
009130*
009140* ˜JÐ•ÛŒ¯—p—¿‹àî•ñƒ[ƒN
009150* Šî–{
009160 01 ‰ŒŸ—¿‚b‚v                         PIC 9(4) VALUE ZERO.
009170 01 ŽžŠÔŠO‚b‚v                         PIC 9(4) VALUE ZERO.
009180 01 ‹x“ú‚b‚v                           PIC 9(4) VALUE ZERO.
009190 01 [–é‚b‚v                           PIC 9(4) VALUE ZERO.
009200 01 ‰—ÃŠî–{—¿‹à‚b‚v                   PIC 9(4) VALUE ZERO.
009210 01 ‰—Ã’Ç‰Á—¿‹à‚b‚v                   PIC 9(4) VALUE ZERO.
009220 01 ‰—Ã‰ÁŽZ—¦‚b‚v                     PIC 9(3) VALUE ZERO.
009230 01 ‰—Ã–éŠÔ‰ÁŽZ—¦‚b‚v                 PIC 9(3) VALUE ZERO.
009240 01 ‰—Ã[–é‰ÁŽZ—¦‚b‚v                 PIC 9(3) VALUE ZERO.
009250 01 ‰—Ã“ï˜H‰ÁŽZ—¦‚b‚v                 PIC 9(3) VALUE ZERO.
009260 01 ‰—Ã–\•—‰J‰ÁŽZ—¦‚b‚v               PIC 9(3) VALUE ZERO.
009270 01 ÄŒŸ—¿‚b‚v                         PIC 9(4) VALUE ZERO.
009280 01 —âãª–@—¿‚b‚v                       PIC 9(4) VALUE ZERO.
009290 01 ‰·ãª–@—¿‚b‚v                       PIC 9(4) VALUE ZERO.
009300 01 “d—Ã—¿‚b‚v                         PIC 9(4) VALUE ZERO.
009310 01 Ž{pî•ñ’ñ‹Ÿ—¿‚b‚v                 PIC 9(4) VALUE ZERO.
009320 01 Žw“±ŠÇ——¿‚b‚v                     PIC 9(4) VALUE ZERO.
009330 01 ‰^“®—Ã–@—¿‚b‚v                     PIC 9(4) VALUE ZERO.
009340 01 h”‘—¿‚b‚v                         PIC 9(4) VALUE ZERO.
009350 01 HŽ–—¿‚b‚v                         PIC 9(4) VALUE ZERO.
009360 01 ‹x‹ÆØ–¾‘—¿‚b‚v                   PIC 9(4) VALUE ZERO.
009370* •‰•Ê
009380* 01 SkŒã—Ã—¿‚a‚v                     PIC 9(5) VALUE ZERO.
009390* 01 Œã—Ã—¿‚a‚v                         PIC 9(5) VALUE ZERO.
009400* 01 ®•œ—¿‚a‚v                         PIC 9(5) VALUE ZERO.
009410* •‰•Ê‚c
009420 01 SkŒã—Ã—¿‚c‚v                     PIC 9(5) VALUE ZERO.
009430 01 Œã—Ã—¿‚c‚v                         PIC 9(5) VALUE ZERO.
009440 01 ®•œ—¿‚c‚v                         PIC 9(5) VALUE ZERO.
009450 01 “Á•ÊÞ—¿”ï‚c‚v                     PIC 9(5) VALUE ZERO.
009460 01 ŒðŠ·•ï‘Ñ—¿‚c‚v                     PIC 9(5) VALUE ZERO.
009470*
009480* ƒGƒ‰[ƒƒbƒZ[ƒW—p
009490 01 ƒGƒ‰[ƒƒbƒZ[ƒW‚v.
009500    03 ƒGƒ‰[•”ˆÊƒR[ƒh‚v              PIC 9(5) VALUE ZERO.
009510    03 FILLER                          PIC X(15).
009520* â‘Î’lŒvŽZ—p
009530 01 ŒvŽZ˜a—ï”NŒŽ‚v.
009540     03 ŒvŽZ˜a—ï‚v                     PIC 9.
009550     03 ŒvŽZ”NŒŽ‚v.
009560        05 ŒvŽZ”N‚v                    PIC 9(2).
009570        05 ŒvŽZŒŽ‚v                    PIC 9(2).
009580     03 ŒvŽZ“ú‚v                       PIC 9(2).
009590 01 â‘Î’l‚v                           PIC 9(6).
009600 01 •‰Œo‰ß“ú‚v                       PIC 9(4).
009610 01 â‘Î’lŒŽ‚v‚Q                       PIC 9(3).
009620 01 ‰[”N‰ñ”‚v                         PIC 9(2).
009630 01 ’Êí”N‰ñ”‚v                       PIC 9(2).
009640 01 â‘Î’lŒŽ‚v                         PIC 9(3).
009650 01 ’Êí”Nâ‘Î’l‚v                     PIC 9(6).
009660 01 ‰[”Nâ‘Î’l‚v                       PIC 9(6).
009670 01 ”Nâ‘Î’l‚v                         PIC 9(6).
009680 01 •‰â‘Î’l‚v                       PIC 9(6).
009690 01 Ž{pâ‘Î’l‚v                       PIC 9(6).
009700 01 ¤‚v                               PIC 9(4) VALUE ZERO.
009710 01 è—]‚v                             PIC 9(4) VALUE ZERO.
009720 01 ‰ÁŽZ”N‚v                           PIC 9(4).
009730*/˜JÐ—pƒ[ƒN
009740 01 ‰ŒŸ‰ñ”‚v                         PIC 9    VALUE ZERO.
009750 01 ÄŒŸ‰ñ”‚v                         PIC 9    VALUE ZERO.
009760 01 •”ˆÊ–ˆƒf[ƒ^.
009770   03 “Á•ÊÞ—¿”ï‚Q‚v.
009780     05 Þ—¿”ï‚Q‚v                       PIC 9(5) OCCURS 9 VALUE ZERO.
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚Í‚R‰ñŽZ’è‚Å‚«‚éB/20180830
009780     05 Þ—¿”ï‚R‚v                       PIC 9(5) OCCURS 9 VALUE ZERO.
009880     05 Þ—¿‰ñ”‚v                       PIC 9    OCCURS 9 VALUE ZERO.
009790   03 •ï‘ÑŒðŠ·“ú‚v OCCURS 9.
009800     05 •ï‘ÑŒðŠ·“ú‚P‚v                   PIC 9(2) VALUE ZERO.
009810     05 •ï‘ÑŒðŠ·“ú‚Q‚v                   PIC 9(2) VALUE ZERO.
009820     05 •ï‘ÑŒðŠ·“ú‚R‚v                   PIC 9(2) VALUE ZERO.
009830     05 •ï‘ÑŒðŠ·“ú‚S‚v                   PIC 9(2) VALUE ZERO.
009840     05 •ï‘ÑŒðŠ·“ú‚T‚v                   PIC 9(2) VALUE ZERO.
009850     05 •ï‘ÑŒðŠ·“ú‚U‚v                   PIC 9(2) VALUE ZERO.
009860   03 •ï‘ÑŒðŠ·—¿‚v                       PIC 9(5) OCCURS 9 VALUE ZERO.
009870   03 •ï‘ÑŒðŠ·’P‰¿‚v                     PIC 9(4) OCCURS 9 VALUE ZERO.
009880   03 •ï‘ÑŒðŠ·‰ñ”‚v                     PIC 9    OCCURS 9 VALUE ZERO.
009890*/
009900 01 Skƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
009910*
009920*/Ž©”…Ó•ÛŒ¯—p
009930* Šî–{
009940 01 ‰ŒŸ—¿‚d‚v                         PIC 9(4) VALUE ZERO.
009950 01 ŽžŠÔŠO‚d‚v                         PIC 9(4) VALUE ZERO.
009960 01 ‹x“ú‚d‚v                           PIC 9(4) VALUE ZERO.
009970 01 [–é‚d‚v                           PIC 9(4) VALUE ZERO.
009980 01 ‰—ÃŠî–{—¿‹à‚d‚v                   PIC 9(4) VALUE ZERO.
009990 01 ‰—Ã’Ç‰Á—¿‹à‚d‚v                   PIC 9(4) VALUE ZERO.
010000 01 ‰—Ã‰ÁŽZ—¦‚d‚v                     PIC 9(3) VALUE ZERO.
010010 01 ‰—Ã–éŠÔ‰ÁŽZ—¦‚d‚v                 PIC 9(3) VALUE ZERO.
010020 01 ‰—Ã[–é‰ÁŽZ—¦‚d‚v                 PIC 9(3) VALUE ZERO.
010030 01 ‰—Ã“ï˜H‰ÁŽZ—¦‚d‚v                 PIC 9(3) VALUE ZERO.
010040 01 ‰—Ã–\•—‰J‰ÁŽZ—¦‚d‚v               PIC 9(3) VALUE ZERO.
010050 01 ÄŒŸ—¿‚d‚v                         PIC 9(4) VALUE ZERO.
010060 01 —âãª–@—¿‚d‚v                       PIC 9(4) VALUE ZERO.
010070 01 ‰·ãª–@—¿‚d‚v                       PIC 9(4) VALUE ZERO.
010080 01 “d—Ã—¿‚d‚v                         PIC 9(4) VALUE ZERO.
010090 01 Ž{pî•ñ’ñ‹Ÿ—¿‚d‚v                 PIC 9(4) VALUE ZERO.
010100 01 Žw“±ŠÇ——¿‚d‚v                     PIC 9(4) VALUE ZERO.
010110 01 ‰^“®—Ã–@—¿‚d‚v                     PIC 9(4) VALUE ZERO.
010120 01 h”‘—¿‚d‚v                         PIC 9(4) VALUE ZERO.
010130 01 HŽ–—¿‚d‚v                         PIC 9(4) VALUE ZERO.
010140 01 ‹x‹ÆØ–¾‘—¿‚d‚v                   PIC 9(4) VALUE ZERO.
010150 01 ‘å‚`‚v                             PIC 9(4) VALUE ZERO.
010160 01 ’†‚`‚v                             PIC 9(4) VALUE ZERO.
010170 01 ¬‚`‚v                             PIC 9(4) VALUE ZERO.
010180*/‰ŒŸŽž‘Š’k—¿/080919
010190 01 ‰ŒŸŽž‘Š’k—¿‚f‚v                   PIC 9(5) VALUE ZERO.
010200 01 ‰ŒŸŽž‘Š’k—¿‚v                     PIC 9(4) VALUE ZERO.
010210 01 ‰ŒŸŽž‘Š’k—¿‚d‚v                   PIC 9(4) VALUE ZERO.
010220*/Ž©”…Ó—p
010230 01 —âãª–@—¿‚Q‚R‚d‚v                   PIC 9(4) VALUE ZERO.
010240 01 —âãª–@—¿‚R‚O‚d‚v                   PIC 9(4) VALUE ZERO.
010250*
010260 01 ‰·ãª–@—¿‚Q‚R‚d‚v                   PIC 9(4) VALUE ZERO.
010270 01 ‰·ãª–@—¿‚Q‚SœÜ‚d‚v               PIC 9(4) VALUE ZERO.
010280 01 ‰·ãª–@—¿‚Q‚S‘¼‚d‚v                 PIC 9(4) VALUE ZERO.
010290 01 ‰·ãª–@—¿‚R‚O‚d‚v                   PIC 9(4) VALUE ZERO.
010300*
010310 01 “d—Ã—¿‚Q‚R‚d‚v                     PIC 9(4) VALUE ZERO.
010320 01 “d—Ã—¿‚Q‚SœÜ‚d‚v                 PIC 9(4) VALUE ZERO.
010330 01 “d—Ã—¿‚Q‚S‘¼‚d‚v                   PIC 9(4) VALUE ZERO.
010340 01 “d—Ã—¿‚R‚O‚d‚v                     PIC 9(4) VALUE ZERO.
010350*
010360 01 Ž{pØ–¾‘—¿‚d‚v                   PIC 9(4) VALUE ZERO.
010370 01 Ž{p–¾×‘—¿‚d‚v                   PIC 9(4) VALUE ZERO.
010380*
010390* •‰•Ê‚e
010400 01 SkŒã—Ã—¿‚e‚v                     PIC 9(5) VALUE ZERO.
010410 01 Œã—Ã—¿‚e‚v                         PIC 9(5) VALUE ZERO.
010420 01 ®•œ—¿‚e‚v                         PIC 9(5) VALUE ZERO.
010430 01 “Á•ÊÞ—¿”ï‚e‚v                     PIC 9(5) VALUE ZERO.
010440 01 ŒðŠ·•ï‘Ñ—¿‚e‚v                     PIC 9(5) VALUE ZERO.
010450*
010460* Ž©”…Ó—p
010470 01 ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v               PIC 9    VALUE ZERO.
010480*
010490*/ƒGƒ‰[•\Ž¦‚ÌC³
010500 01 ƒGƒ‰[•\Ž¦‚e                       PIC 9(1)  VALUE ZERO.
010510*
010520 01 ¿‹˜a—ï”NŒŽ‚v.
010530   03 ¿‹˜a—ï‚v                       PIC 9     VALUE ZERO.
010540   03 ¿‹”NŒŽ‚v.
010550      05 ¿‹”N‚v                      PIC 9(2)  VALUE ZERO.
010560      05 ¿‹ŒŽ‚v                      PIC 9(2)  VALUE ZERO.
010570
010580
010590
010600* •”ˆÊ–ˆ‚Ì‘½•”ˆÊ’üŒ¸•”ˆÊ–ˆ{“ú–ˆ‚Ìƒ[ƒN
010610* ‚P•”ˆÊ–Ú
010620 01 Œã—Ã—¿‚P‚m‚q‚v                       PIC 9(5) VALUE ZERO.
010630 01 —âãª–@—¿‚P‚m‚q‚v                     PIC 9(4) VALUE ZERO.
010640 01 ‰·ãª–@—¿‚P‚m‚q‚v                     PIC 9(4) VALUE ZERO.
010650 01 “d—Ã—¿‚P‚m‚q‚v                       PIC 9(5) VALUE ZERO.
010660* ‚P•”ˆÊ–ÚSk
010670 01 Œã—Ã—¿‚PSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
010680 01 —âãª–@—¿‚PSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
010690 01 ‰·ãª–@—¿‚PSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
010700 01 “d—Ã—¿‚PSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
010710* ‚P•”ˆÊ–Ú‘½•”ˆÊ
010720 01 ‘½•”ˆÊŒã—Ã—¿‚P‚m‚q‚v                 PIC 9(5) VALUE ZERO.
010730* ‚P•”ˆÊ–Ú’·Šú
010740 01 ’·ŠúŒã—Ã—¿‚P‚m‚q‚v                   PIC 9(5) VALUE ZERO.
010750* ‚P•”ˆÊ–Ú‘½•”ˆÊSk
010760 01 ‘½•”ˆÊSkŒã—Ã—¿‚P‚m‚q‚v             PIC 9(5) VALUE ZERO.
010770*
010780* ‚Q•”ˆÊ–Ú
010790 01 Œã—Ã—¿‚Q‚m‚q‚v                       PIC 9(5) VALUE ZERO.
010800 01 —âãª–@—¿‚Q‚m‚q‚v                     PIC 9(4) VALUE ZERO.
010810 01 ‰·ãª–@—¿‚Q‚m‚q‚v                     PIC 9(4) VALUE ZERO.
010820 01 “d—Ã—¿‚Q‚m‚q‚v                       PIC 9(5) VALUE ZERO.
010830* ‚Q•”ˆÊ–ÚSk
010840 01 Œã—Ã—¿‚QSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
010850 01 —âãª–@—¿‚QSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
010860 01 ‰·ãª–@—¿‚QSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
010870 01 “d—Ã—¿‚QSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
010880* ‚Q•”ˆÊ–Ú‘½•”ˆÊ
010890 01 ‘½•”ˆÊŒã—Ã—¿‚Q‚m‚q‚v                 PIC 9(5) VALUE ZERO.
010900* ‚Q•”ˆÊ–Ú’·Šú
010910 01 ’·ŠúŒã—Ã—¿‚Q‚m‚q‚v                   PIC 9(5) VALUE ZERO.
010920* ‚Q•”ˆÊ–Ú‘½•”ˆÊSk
010930 01 ‘½•”ˆÊSkŒã—Ã—¿‚Q‚m‚q‚v             PIC 9(5) VALUE ZERO.
010940*
010950* ‚R•”ˆÊ–Ú
010960 01 Œã—Ã—¿‚R‚m‚q‚v                       PIC 9(5) VALUE ZERO.
010970 01 —âãª–@—¿‚R‚m‚q‚v                     PIC 9(4) VALUE ZERO.
010980 01 ‰·ãª–@—¿‚R‚m‚q‚v                     PIC 9(4) VALUE ZERO.
010990 01 “d—Ã—¿‚R‚m‚q‚v                       PIC 9(5) VALUE ZERO.
011000* ‚R•”ˆÊ–ÚSk
011010 01 Œã—Ã—¿‚RSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011020 01 —âãª–@—¿‚RSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
011030 01 ‰·ãª–@—¿‚RSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
011040 01 “d—Ã—¿‚RSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011050* ‚R•”ˆÊ–Ú‘½•”ˆÊ
011060 01 ‘½•”ˆÊŒã—Ã—¿‚R‚m‚q‚v                 PIC 9(5) VALUE ZERO.
011070* ‚R•”ˆÊ–Ú’·Šú
011080 01 ’·ŠúŒã—Ã—¿‚R‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011090* ‚R•”ˆÊ–Ú‘½•”ˆÊSk
011100 01 ‘½•”ˆÊSkŒã—Ã—¿‚R‚m‚q‚v             PIC 9(5) VALUE ZERO.
011110*
011120* ‚S•”ˆÊ–Ú
011130 01 Œã—Ã—¿‚S‚m‚q‚v                       PIC 9(5) VALUE ZERO.
011140 01 —âãª–@—¿‚S‚m‚q‚v                     PIC 9(4) VALUE ZERO.
011150 01 ‰·ãª–@—¿‚S‚m‚q‚v                     PIC 9(4) VALUE ZERO.
011160 01 “d—Ã—¿‚S‚m‚q‚v                       PIC 9(5) VALUE ZERO.
011170* ‚S•”ˆÊ–ÚSk
011180 01 Œã—Ã—¿‚SSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011190 01 —âãª–@—¿‚SSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
011200 01 ‰·ãª–@—¿‚SSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
011210 01 “d—Ã—¿‚SSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011220* ‚S•”ˆÊ–Ú‘½•”ˆÊ
011230 01 ‘½•”ˆÊŒã—Ã—¿‚S‚m‚q‚v                 PIC 9(5) VALUE ZERO.
011240* ‚S•”ˆÊ–Ú’·Šú
011250 01 ’·ŠúŒã—Ã—¿‚S‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011260* ‚S•”ˆÊ–Ú‘½•”ˆÊSk
011270 01 ‘½•”ˆÊSkŒã—Ã—¿‚S‚m‚q‚v             PIC 9(5) VALUE ZERO.
011280*
011290* ‚T•”ˆÊ–Ú
011300 01 Œã—Ã—¿‚T‚m‚q‚v                       PIC 9(5) VALUE ZERO.
011310 01 —âãª–@—¿‚T‚m‚q‚v                     PIC 9(4) VALUE ZERO.
011320 01 ‰·ãª–@—¿‚T‚m‚q‚v                     PIC 9(4) VALUE ZERO.
011330 01 “d—Ã—¿‚T‚m‚q‚v                       PIC 9(5) VALUE ZERO.
011340* ‚T•”ˆÊ–ÚSk
011350 01 Œã—Ã—¿‚TSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011360 01 —âãª–@—¿‚TSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
011370 01 ‰·ãª–@—¿‚TSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
011380 01 “d—Ã—¿‚TSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011390* ‚T•”ˆÊ–Ú‘½•”ˆÊ
011400 01 ‘½•”ˆÊŒã—Ã—¿‚T‚m‚q‚v                 PIC 9(5) VALUE ZERO.
011410* ‚T•”ˆÊ–Ú’·Šú
011420 01 ’·ŠúŒã—Ã—¿‚T‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011430* ‚T•”ˆÊ–Ú‘½•”ˆÊSk
011440 01 ‘½•”ˆÊSkŒã—Ã—¿‚T‚m‚q‚v             PIC 9(5) VALUE ZERO.
011450*
011460* ‚U•”ˆÊ–Ú
011470 01 Œã—Ã—¿‚U‚m‚q‚v                       PIC 9(5) VALUE ZERO.
011480 01 —âãª–@—¿‚U‚m‚q‚v                     PIC 9(4) VALUE ZERO.
011490 01 ‰·ãª–@—¿‚U‚m‚q‚v                     PIC 9(4) VALUE ZERO.
011500 01 “d—Ã—¿‚U‚m‚q‚v                       PIC 9(5) VALUE ZERO.
011510* ‚U•”ˆÊ–ÚSk
011520 01 Œã—Ã—¿‚USk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011530 01 —âãª–@—¿‚USk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
011540 01 ‰·ãª–@—¿‚USk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
011550 01 “d—Ã—¿‚USk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011560* ‚U•”ˆÊ–Ú‘½•”ˆÊ
011570 01 ‘½•”ˆÊŒã—Ã—¿‚U‚m‚q‚v                 PIC 9(5) VALUE ZERO.
011580* ‚U•”ˆÊ–Ú’·Šú
011590 01 ’·ŠúŒã—Ã—¿‚U‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011600* ‚U•”ˆÊ–Ú‘½•”ˆÊSk
011610 01 ‘½•”ˆÊSkŒã—Ã—¿‚U‚m‚q‚v             PIC 9(5) VALUE ZERO.
011620*
011630* ‚V•”ˆÊ–Ú
011640 01 Œã—Ã—¿‚V‚m‚q‚v                       PIC 9(5) VALUE ZERO.
011650 01 —âãª–@—¿‚V‚m‚q‚v                     PIC 9(4) VALUE ZERO.
011660 01 ‰·ãª–@—¿‚V‚m‚q‚v                     PIC 9(4) VALUE ZERO.
011670 01 “d—Ã—¿‚V‚m‚q‚v                       PIC 9(5) VALUE ZERO.
011680* ‚V•”ˆÊ–ÚSk
011690 01 Œã—Ã—¿‚VSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011700 01 —âãª–@—¿‚VSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
011710 01 ‰·ãª–@—¿‚VSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
011720 01 “d—Ã—¿‚VSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011730* ‚V•”ˆÊ–Ú‘½•”ˆÊ
011740 01 ‘½•”ˆÊŒã—Ã—¿‚V‚m‚q‚v                 PIC 9(5) VALUE ZERO.
011750* ‚V•”ˆÊ–Ú’·Šú
011760 01 ’·ŠúŒã—Ã—¿‚V‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011770* ‚V•”ˆÊ–Ú‘½•”ˆÊSk
011780 01 ‘½•”ˆÊSkŒã—Ã—¿‚V‚m‚q‚v             PIC 9(5) VALUE ZERO.
011790*
011800* ‚W•”ˆÊ–Ú
011810 01 Œã—Ã—¿‚W‚m‚q‚v                       PIC 9(5) VALUE ZERO.
011820 01 —âãª–@—¿‚W‚m‚q‚v                     PIC 9(4) VALUE ZERO.
011830 01 ‰·ãª–@—¿‚W‚m‚q‚v                     PIC 9(4) VALUE ZERO.
011840 01 “d—Ã—¿‚W‚m‚q‚v                       PIC 9(5) VALUE ZERO.
011850* ‚W•”ˆÊ–ÚSk
011860 01 Œã—Ã—¿‚WSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011870 01 —âãª–@—¿‚WSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
011880 01 ‰·ãª–@—¿‚WSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
011890 01 “d—Ã—¿‚WSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011900* ‚W•”ˆÊ–Ú‘½•”ˆÊ
011910 01 ‘½•”ˆÊŒã—Ã—¿‚W‚m‚q‚v                 PIC 9(5) VALUE ZERO.
011920* ‚W•”ˆÊ–Ú’·Šú
011930 01 ’·ŠúŒã—Ã—¿‚W‚m‚q‚v                   PIC 9(5) VALUE ZERO.
011940* ‚W•”ˆÊ–Ú‘½•”ˆÊSk
011950 01 ‘½•”ˆÊSkŒã—Ã—¿‚W‚m‚q‚v             PIC 9(5) VALUE ZERO.
011960*
011970* ‚X•”ˆÊ–Ú
011980 01 Œã—Ã—¿‚X‚m‚q‚v                       PIC 9(5) VALUE ZERO.
011990 01 —âãª–@—¿‚X‚m‚q‚v                     PIC 9(4) VALUE ZERO.
012000 01 ‰·ãª–@—¿‚X‚m‚q‚v                     PIC 9(4) VALUE ZERO.
012010 01 “d—Ã—¿‚X‚m‚q‚v                       PIC 9(5) VALUE ZERO.
012020* ‚X•”ˆÊ–ÚSk
012030 01 Œã—Ã—¿‚XSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
012040 01 —âãª–@—¿‚XSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
012050 01 ‰·ãª–@—¿‚XSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
012060 01 “d—Ã—¿‚XSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
012070* ‚X•”ˆÊ–Ú‘½•”ˆÊ
012080 01 ‘½•”ˆÊŒã—Ã—¿‚X‚m‚q‚v                 PIC 9(5) VALUE ZERO.
012090* ‚X•”ˆÊ–Ú’·Šú
012100 01 ’·ŠúŒã—Ã—¿‚X‚m‚q‚v                   PIC 9(5) VALUE ZERO.
012110* ‚X•”ˆÊ–Ú‘½•”ˆÊSk
012120 01 ‘½•”ˆÊSkŒã—Ã—¿‚X‚m‚q‚v             PIC 9(5) VALUE ZERO.
012130*
012140 01 I—¹ƒtƒ‰ƒO‚R                       PIC X(3) VALUE SPACE.
012150 01 ƒŒƒZƒvƒgŽí—Þ‚v                     PIC X(4) VALUE SPACE.
012160 01 •¬ƒJƒEƒ“ƒ^                       PIC 9     VALUE ZERO.
012170 01 XVƒtƒ‰ƒO                         PIC X(3)  VALUE SPACE.
012180* ‰ŒŸ“ú‘Þ”ð—p
012190 01 ‰ŒŸ”NŒŽ“ú‚v‚s.
012200    03 ‰ŒŸ˜a—ï‚v‚s                    PIC 9     VALUE ZERO.
012210    03 ‰ŒŸ”N‚v‚s                      PIC 9(2)  VALUE ZERO.
012220    03 ‰ŒŸŒŽ‚v‚s                      PIC 9(2)  VALUE ZERO.
012230    03 ‰ŒŸ“ú‚v‚s                      PIC 9(2)  VALUE ZERO.
012240* ƒL[ƒŒƒZŽí•Ê—p
012250 01 ƒŒƒZŽí•Ê‚v                         PIC 9(2) VALUE ZERO.
012260 01 V‹Kƒ`ƒFƒbƒN‚v                     PIC N     VALUE SPACE.
012270 01 Œp‘±ƒ`ƒFƒbƒN‚v                     PIC N     VALUE SPACE.
012280 01 ‰ŒŸƒtƒ‰ƒO                         PIC X(3)  VALUE SPACE.
012290 01 ŽÀ“ú”‚v                           PIC 9(2)  VALUE ZERO.
      *
008540*/“ú–ˆ—pƒ[ƒN
008550 01 ‰ŒŸŽžŠÔŠO‚m‚v                     PIC 9 VALUE ZERO.
008560 01 ‰ŒŸ‹x“ú‚m‚v                       PIC 9 VALUE ZERO.
008570 01 ‰ŒŸ[–é‚m‚v                       PIC 9 VALUE ZERO.
008580 01 ‰—Ã–éŠÔ‚m‚v                       PIC 9 VALUE ZERO.
008590 01 ‰—Ã“ï˜H‚m‚v                       PIC 9 VALUE ZERO.
008600 01 ‰—Ã–\•—‰J‚m‚v                     PIC 9 VALUE ZERO.
008610 01 ‰—ÃŽžŠÔŠO‚m‚v                     PIC 9 VALUE ZERO.
008620 01 ‹à‘®•›Žq‘å‚m‚v                     PIC 9 VALUE ZERO.
008630 01 ‹à‘®•›Žq’†‚m‚v                     PIC 9 VALUE ZERO.
008640 01 ‹à‘®•›Žq¬‚m‚v                     PIC 9 VALUE ZERO.
008650*/“ú–ˆ•”ˆÊ–ˆƒ[ƒN
008660 01 ‰‰ñˆ’u‚m‚n‚v.
008670    03 ‰‰ñˆ’u—¿‚m‚n‚v                PIC 9(5) OCCURS 9 VALUE ZERO.
010440 01 ŒðŠ·•ï‘Ñ—¿‚P‚m‚v.
010440    03 ŒðŠ·•ï‘Ñ—¿‚m‚v                  PIC 9(5) OCCURS 9 VALUE ZERO.
      */“ú–ˆ—pÞ—¿”ïƒ[ƒN
009770 01 “Á•ÊÞ—¿”ï‚m‚v.
009780    03 Þ—¿”ï‚m‚v                      PIC 9(5) OCCURS 9 VALUE ZERO.
012300*ªªª************
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚ÌŒðŠ·‚ÍA‚Q‰ñ‚Ü‚ÅŽZ’è‚Å‚«‚éB/20180829
       01 •”ˆÊƒJƒEƒ“ƒ^‚R                     PIC 9(2) VALUE ZERO.
       01 •”ˆÊƒJƒEƒ“ƒ^‚S                     PIC 9(2) VALUE ZERO.
       01 “Á•ÊÞ—¿‚b‚m‚s                     PIC 9(1) VALUE ZERO.
      *
       01 Ž{p˜a—ï”NŒŽ“ú‚Q‚v.
         03 Ž{p˜a—ï”NŒŽ‚Q‚v.
           05 Ž{p˜a—ï‚Q‚v                   PIC 9    VALUE ZERO.
           05 Ž{p”NŒŽ‚Q‚v.
              07 Ž{p”N‚Q‚v                  PIC 9(2) VALUE ZERO.
              07 Ž{pŒŽ‚Q‚v                  PIC 9(2) VALUE ZERO.
         03 Ž{p“ú‚Q‚v                       PIC 9(2) VALUE ZERO.
      *
       01 ŠJŽn˜a—ï”NŒŽ“ú‚Q‚v.
         03 ŠJŽn˜a—ï”NŒŽ‚Q‚v.
           05 ŠJŽn˜a—ï‚Q‚v                   PIC 9    VALUE ZERO.
           05 ŠJŽn”NŒŽ‚Q‚v.
              07 ŠJŽn”N‚Q‚v                  PIC 9(2) VALUE ZERO.
              07 ŠJŽnŒŽ‚Q‚v                  PIC 9(2) VALUE ZERO.
         03 ŠJŽn“ú‚Q‚v                       PIC 9(2) VALUE ZERO.
      *
012310*
012320 01 ‰æ–Ê§Œä.
012330     03 ’è‹`‘Ì–¼                       PIC X(8) VALUE SPACE.
012340     03 €–ÚŒQ–¼                       PIC X(8) VALUE SPACE.
012350     03 €–ÚŒQ–¼‚q  REDEFINES  €–ÚŒQ–¼.
012360         05 €–ÚŒQ‚q–¼                 PIC X(4).
012370         05 €–ÚŒQ”Ô†                 PIC 9(2).
012380         05 FILLER                     PIC X(2).
012390     03 ˆ—Ží•Ê                       PIC X(2) VALUE SPACE.
012400     03 ƒAƒeƒ“ƒVƒ‡ƒ“                   PIC X(4) VALUE SPACE.
012410     03 ‹@”\ƒL[                       PIC X(4) VALUE SPACE.
012420     03 Šg’£§Œä.
012430         05 ’[––§Œä                   PIC X(4) VALUE SPACE.
012440         05 Ú×§Œä                   PIC X(2) VALUE SPACE.
012450     03 ’Ê’mî•ñ                       PIC X(2) VALUE SPACE.
012460     03 ƒ†ƒjƒbƒg–¼.
012470         05 FILLER                     PIC X(2)  VALUE SPACE.
012480         05 ’[––”Ô†                   PIC 9(3)  VALUE ZERO.
012490         05 FILLER                     PIC X(4)  VALUE SPACE.
012500     03 ƒƒbƒZ[ƒW‚v                   PIC N(20) VALUE SPACE.
012510* “ú•t‚v‚n‚q‚j
012520 01 ˜a—ïI—¹”N‚v                       PIC 9(4) VALUE ZERO.
012530 01 ŒvŽZ‹@˜a—ï”N‚v                     PIC 9(2) VALUE ZERO.
012540*
012550 01 ŒvŽZ‹@¼—ï.
012560    03 ŒvŽZ‹@¼—ï”N                    PIC 9(4).
012570    03 ŒvŽZ‹@¼—ïŒŽ“ú                  PIC 9(4).
012580 01 ŒvŽZ‹@¼—ï‚q REDEFINES ŒvŽZ‹@¼—ï.
012590    03 ŒvŽZ‹@¢‹I                      PIC 9(2).
012600    03 ŒvŽZ‹@“ú•t                      PIC 9(6).
012610    03 ŒvŽZ‹@“ú•t‚q REDEFINES ŒvŽZ‹@“ú•t.
012620       05 ŒvŽZ‹@”NŒŽ                   PIC 9(4).
012630       05 ŒvŽZ‹@”NŒŽ‚q REDEFINES ŒvŽZ‹@”NŒŽ.
012640         07 ŒvŽZ‹@”N                   PIC 9(2).
012650         07 ŒvŽZ‹@ŒŽ                   PIC 9(2).
012660       05 ŒvŽZ‹@“ú                     PIC 9(2).
012670*
012680* ‚e‚n‚q‚li‰æ–Ê’è‹`‘Ìj‚Ì“WŠJ
012690*
012700* ƒtƒ@ƒCƒ‹Ž¯•Ê–¼
012710*****************************************************************
012720*                          ˜AŒ‹€–Ú                             *
012730*****************************************************************
012740*
012750*    ŒŽ–ˆ‚Ì—¿‹à
012760***********************
012770 01 ˜A—¿˜JŽ©|—¿‹à‚P IS EXTERNAL.
012780   03 ˜A—¿˜JŽ©|‰ŒŸ.
012790      05 ˜A—¿˜JŽ©|•‰’S—¦                 PIC 9(3).
012800      05 ˜A—¿˜JŽ©|‰ŒŸ—¿                 PIC 9(5).
012810      05 ˜A—¿˜JŽ©|‰ŒŸ‰ñ”               PIC 9.
012820      05 ˜A—¿˜JŽ©|‰ŒŸ’P‰¿               PIC 9(5).
012830      05 ˜A—¿˜JŽ©|‰ŒŸ‰ÁŽZ.
012840        07 ˜A—¿˜JŽ©|‰ŒŸŽžŠÔŠO           PIC 9.
012850        07 ˜A—¿˜JŽ©|‰ŒŸ‹x“ú             PIC 9.
012860        07 ˜A—¿˜JŽ©|‰ŒŸ[–é             PIC 9.
012870      05 ˜A—¿˜JŽ©|‰ŒŸ‰ÁŽZ—¿             PIC 9(5).
012880   03 ˜A—¿˜JŽ©|ÄŒŸ.
012890      05 ˜A—¿˜JŽ©|ÄŒŸ—¿                 PIC 9(5).
012900      05 ˜A—¿˜JŽ©|ÄŒŸ‰ñ”               PIC 9.
012910      05 ˜A—¿˜JŽ©|ÄŒŸ’P‰¿               PIC 9(5).
012920   03 ˜A—¿˜JŽ©|‰—Ã.
012930      05 ˜A—¿˜JŽ©|‰—Ã‹——£               PIC 9(2)V9.
012940      05 ˜A—¿˜JŽ©|‰—Ã‰ñ”               PIC 9(2).
012950      05 ˜A—¿˜JŽ©|‰—Ã’P‰¿               PIC 9(5).
012960      05 ˜A—¿˜JŽ©|‰—Ã—¿                 PIC 9(6).
012970      05 ˜A—¿˜JŽ©|‰—Ã‰ÁŽZ—¿             PIC 9(5).
012980      05 ˜A—¿˜JŽ©|‰—Ã‰ÁŽZ.
012990        07 ˜A—¿˜JŽ©|‰—Ã–éŠÔ             PIC 9.
013000        07 ˜A—¿˜JŽ©|‰—Ã“ï˜H             PIC 9.
013010        07 ˜A—¿˜JŽ©|‰—Ã–\•—‰J           PIC 9.
013020        07 ˜A—¿˜JŽ©|‰—ÃŽžŠÔŠO           PIC 9.
013030   03 ˜A—¿˜JŽ©|‹à‘®•›Žq‰ÁŽZ—¿            PIC 9(5).
013040   03 ˜A—¿˜JŽ©|‹à‘®•›Žq”.
013050      05 ˜A—¿˜JŽ©|‘å                     PIC 9.
013060      05 ˜A—¿˜JŽ©|’†                     PIC 9.
013070      05 ˜A—¿˜JŽ©|¬                     PIC 9.
013080   03 ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ.
013090      05 ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ‰ñ”           PIC 9(2).
013100      05 ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ’P‰¿           PIC 9(4).
013110      05 ˜A—¿˜JŽ©|Ž{pî•ñ’ñ‹Ÿ—¿         PIC 9(5).
013120   03 ˜A—¿˜JŽ©|¬Œv                      PIC 9(7).
013130   03 ˜A—¿˜JŽ©|”ï—pŠz                    PIC 9(6).
013140   03 ˜A—¿˜JŽ©|ˆê•”•‰’S‹à                PIC 9(6).
013150   03 ˜A—¿˜JŽ©|¿‹Šz                    PIC 9(6).
013160   03 ˜A—¿˜JŽ©|‹‹•tŠ„‡                  PIC 9(3).
013170   03 ˜A—¿˜JŽ©|Žó‹‹ŽÒ•‰’SŠz              PIC 9(6).
013180   03 ˜A—¿˜JŽ©|•¬¿‹‹àŠz              PIC 9(6).
013190*
013200   03 ˜A—¿˜JŽ©|Žw“±ŠÇ—.
013210      05 ˜A—¿˜JŽ©|Žw“±ŠÇ—‰ñ”           PIC 9.
013220      05 ˜A—¿˜JŽ©|Žw“±ŠÇ—’P‰¿           PIC 9(4).
013230      05 ˜A—¿˜JŽ©|Žw“±ŠÇ——¿             PIC 9(4).
013240   03 ˜A—¿˜JŽ©|‰^“®—Ã–@.
013250      05 ˜A—¿˜JŽ©|‰^“®—Ã–@‰ñ”           PIC 9.
013260      05 ˜A—¿˜JŽ©|‰^“®—Ã–@’P‰¿           PIC 9(4).
013270      05 ˜A—¿˜JŽ©|‰^“®—Ã–@—¿             PIC 9(4).
013280   03 ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‘.
013290      05 ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‰ñ”       PIC 9(2).
013300      05 ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾’P‰¿       PIC 9(4).
013310      05 ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‘—¿       PIC 9(5).
013320   03 ˜A—¿˜JŽ©|h”‘.
013330      05 ˜A—¿˜JŽ©|h”‘‰ñ”               PIC 9(2).
013340      05 ˜A—¿˜JŽ©|h”‘’P‰¿               PIC 9(4).
013350      05 ˜A—¿˜JŽ©|h”‘—¿                 PIC 9(5).
013360   03 ˜A—¿˜JŽ©|HŽ–.
013370      05 ˜A—¿˜JŽ©|HŽ–‰ñ”               PIC 9(2).
013380      05 ˜A—¿˜JŽ©|HŽ–’P‰¿               PIC 9(4).
013390      05 ˜A—¿˜JŽ©|HŽ–—¿                 PIC 9(5).
013400*/Ž©”…Ó•ÛŒ¯‚Ì‚Ý
013410   03 ˜A—¿˜JŽ©|Ž{pØ–¾‘.
013420      05 ˜A—¿˜JŽ©|Ž{pØ–¾‰ñ”           PIC 9(2).
013430      05 ˜A—¿˜JŽ©|Ž{pØ–¾’P‰¿           PIC 9(4).
013440      05 ˜A—¿˜JŽ©|Ž{pØ–¾‘—¿           PIC 9(5).
013450*
013460   03 ˜A—¿˜JŽ©|Ž{p–¾×‘.
013470      05 ˜A—¿˜JŽ©|Ž{p–¾×‰ñ”           PIC 9(2).
013480      05 ˜A—¿˜JŽ©|Ž{p–¾×’P‰¿           PIC 9(4).
013490      05 ˜A—¿˜JŽ©|Ž{p–¾×‘—¿           PIC 9(5).
013500   03 ˜A—¿˜JŽ©|Ž©”…Ó‚»‚Ì‘¼‹àŠz          PIC 9(6).
013510**************************
013520*
013530   03 ˜A—¿˜JŽ©|Žó‹‹ŽÒ•‰’SŠz“à–ó   OCCURS  10.
013540      05 ˜A—¿˜JŽ©|“–“ú•ª•‰’SŠz           PIC 9(4).
013550*
013560*
013570* •‰•”ˆÊ–ˆ‚Ì—¿‹à
013580***********************
013590 01 ˜A—¿˜JŽ©|—¿‹à‚Q IS EXTERNAL.
013600   03 ˜A—¿˜JŽ©|‰‰ñˆ’u    OCCURS   9.
013610      05 ˜A—¿˜JŽ©|‰‰ñˆ’u—¿             PIC 9(5).
013620   03 ˜A—¿˜JŽ©|‰‰ñˆ’u—¿Œv              PIC 9(6).
013630   03 ˜A—¿˜JŽ©|Þ—¿”ï                    PIC 9(5) OCCURS 9.
013640   03 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú OCCURS 9.
013650     05 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚P            PIC 9(2).
013660     05 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚Q            PIC 9(2).
013670     05 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚R            PIC 9(2).
013680     05 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚S            PIC 9(2).
013690     05 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚T            PIC 9(2).
013700     05 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚U            PIC 9(2).
013710   03 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·—¿                PIC 9(5) OCCURS 9.
013720
013730   03 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·’P‰¿              PIC 9(4) OCCURS 9.
013740   03 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·‰ñ”              PIC 9    OCCURS 9.
013750*
013760* ’üŒ¸–ˆ‚Ì—¿‹à
013770***********************
013780 01 ˜A—¿˜JŽ©|—¿‹à‚R IS EXTERNAL.
013790**********
013800* ‚P•”ˆÊ *
013810**********
013820   03 ˜A—¿˜JŽ©|•”ˆÊ‚P.
013830      05 ˜A—¿˜JŽ©|Œã—Ã‚P.
013840         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚P          PIC 9(4).
013850         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚P          PIC 9(2).
013860         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚P            PIC 9(5).
013870      05 ˜A—¿˜JŽ©|—âãª–@‚P.
013880         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚P        PIC 9(4).
013890         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚P        PIC 9(2).
013900         07 ˜A—¿˜JŽ©|—âãª–@—¿‚P          PIC 9(4).
013910      05 ˜A—¿˜JŽ©|‰·ãª–@‚P.
013920         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚P        PIC 9(4).
013930         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚P        PIC 9(2).
013940         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚P          PIC 9(4).
013950      05 ˜A—¿˜JŽ©|“d—Ã‚P.
013960         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚P          PIC 9(4).
013970         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚P          PIC 9(2).
013980         07 ˜A—¿˜JŽ©|“d—Ã—¿‚P            PIC 9(5).
013990      05 ˜A—¿˜JŽ©|¬Œv‚P                 PIC 9(6).
014000      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚P           PIC 9(3).
014010      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚P           PIC 9(6).
014020**************
014030* ‚P•”ˆÊSk *
014040**************
014050   03 ˜A—¿˜JŽ©|•”ˆÊ‚PSk.
014060      05 ˜A—¿˜JŽ©|Œã—Ã‚PSk.
014070         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚PSk          PIC 9(4).
014080         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚PSk          PIC 9(2).
014090         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚PSk            PIC 9(5).
014100      05 ˜A—¿˜JŽ©|—âãª–@‚PSk.
014110         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚PSk        PIC 9(4).
014120         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚PSk        PIC 9(2).
014130         07 ˜A—¿˜JŽ©|—âãª–@—¿‚PSk          PIC 9(4).
014140      05 ˜A—¿˜JŽ©|‰·ãª–@‚PSk.
014150         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚PSk        PIC 9(4).
014160         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚PSk        PIC 9(2).
014170         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚PSk          PIC 9(4).
014180      05 ˜A—¿˜JŽ©|“d—Ã‚PSk.
014190         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚PSk          PIC 9(4).
014200         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚PSk          PIC 9(2).
014210         07 ˜A—¿˜JŽ©|“d—Ã—¿‚PSk            PIC 9(5).
014220      05 ˜A—¿˜JŽ©|¬Œv‚PSk                 PIC 9(6).
014230      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚PSk           PIC 9(3).
014240      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚PSk           PIC 9(6).
014250*/‚P•”ˆÊ–Ú‘½•”ˆÊ
014260      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚P           PIC 9(4).
014270      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚P           PIC 9(2).
014280      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚P             PIC 9(5).
014290*/‚P•”ˆÊ–Ú’·Šú
014300      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚P             PIC 9(4).
014310      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚P             PIC 9(2).
014320      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚P               PIC 9(5).
014330*/‚P•”ˆÊ–Ú‘½•”ˆÊSk
014340      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚P       PIC 9(4).
014350      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚P       PIC 9(2).
014360      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚P         PIC 9(5).
014370**********
014380* ‚Q•”ˆÊ *
014390**********
014400   03 ˜A—¿˜JŽ©|•”ˆÊ‚Q.
014410      05 ˜A—¿˜JŽ©|Œã—Ã‚Q.
014420         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚Q          PIC 9(4).
014430         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚Q          PIC 9(2).
014440         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚Q            PIC 9(5).
014450      05 ˜A—¿˜JŽ©|—âãª–@‚Q.
014460         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚Q        PIC 9(4).
014470         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚Q        PIC 9(2).
014480         07 ˜A—¿˜JŽ©|—âãª–@—¿‚Q          PIC 9(4).
014490      05 ˜A—¿˜JŽ©|‰·ãª–@‚Q.
014500         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚Q        PIC 9(4).
014510         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚Q        PIC 9(2).
014520         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚Q          PIC 9(4).
014530      05 ˜A—¿˜JŽ©|“d—Ã‚Q.
014540         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚Q          PIC 9(4).
014550         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚Q          PIC 9(2).
014560         07 ˜A—¿˜JŽ©|“d—Ã—¿‚Q            PIC 9(5).
014570      05 ˜A—¿˜JŽ©|¬Œv‚Q                 PIC 9(6).
014580      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚Q           PIC 9(3).
014590      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚Q           PIC 9(6).
014600**************
014610* ‚Q•”ˆÊSk *
014620**************
014630   03 ˜A—¿˜JŽ©|•”ˆÊ‚QSk.
014640      05 ˜A—¿˜JŽ©|Œã—Ã‚QSk.
014650         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚QSk          PIC 9(4).
014660         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚QSk          PIC 9(2).
014670         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚QSk            PIC 9(5).
014680      05 ˜A—¿˜JŽ©|—âãª–@‚QSk.
014690         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚QSk        PIC 9(4).
014700         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚QSk        PIC 9(2).
014710         07 ˜A—¿˜JŽ©|—âãª–@—¿‚QSk          PIC 9(4).
014720      05 ˜A—¿˜JŽ©|‰·ãª–@‚QSk.
014730         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚QSk        PIC 9(4).
014740         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚QSk        PIC 9(2).
014750         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚QSk          PIC 9(4).
014760      05 ˜A—¿˜JŽ©|“d—Ã‚QSk.
014770         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚QSk          PIC 9(4).
014780         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚QSk          PIC 9(2).
014790         07 ˜A—¿˜JŽ©|“d—Ã—¿‚QSk            PIC 9(5).
014800      05 ˜A—¿˜JŽ©|¬Œv‚QSk                 PIC 9(6).
014810      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚QSk           PIC 9(3).
014820      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚QSk           PIC 9(6).
014830*/‚Q•”ˆÊ–Ú‘½•”ˆÊ
014840      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚Q           PIC 9(4).
014850      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚Q           PIC 9(2).
014860      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚Q             PIC 9(5).
014870*/‚Q•”ˆÊ–Ú’·Šú
014880      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚Q             PIC 9(4).
014890      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚Q             PIC 9(2).
014900      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚Q               PIC 9(5).
014910*/‚Q•”ˆÊ–Ú‘½•”ˆÊSk
014920      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚Q       PIC 9(4).
014930      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚Q       PIC 9(2).
014940      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚Q         PIC 9(5).
014950**********
014960* ‚R•”ˆÊ *
014970**********
014980   03 ˜A—¿˜JŽ©|•”ˆÊ‚R.
014990      05 ˜A—¿˜JŽ©|Œã—Ã‚R.
015000         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚R          PIC 9(4).
015010         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚R          PIC 9(2).
015020         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚R            PIC 9(5).
015030      05 ˜A—¿˜JŽ©|—âãª–@‚R.
015040         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚R        PIC 9(4).
015050         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚R        PIC 9(2).
015060         07 ˜A—¿˜JŽ©|—âãª–@—¿‚R          PIC 9(4).
015070      05 ˜A—¿˜JŽ©|‰·ãª–@‚R.
015080         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚R        PIC 9(4).
015090         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚R        PIC 9(2).
015100         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚R          PIC 9(4).
015110      05 ˜A—¿˜JŽ©|“d—Ã‚R.
015120         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚R          PIC 9(4).
015130         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚R          PIC 9(2).
015140         07 ˜A—¿˜JŽ©|“d—Ã—¿‚R            PIC 9(5).
015150      05 ˜A—¿˜JŽ©|¬Œv‚R                 PIC 9(6).
015160      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚R           PIC 9(3).
015170      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚R           PIC 9(6).
015180**************
015190* ‚R•”ˆÊSk *
015200**************
015210   03 ˜A—¿˜JŽ©|•”ˆÊ‚RSk.
015220      05 ˜A—¿˜JŽ©|Œã—Ã‚RSk.
015230         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚RSk          PIC 9(4).
015240         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚RSk          PIC 9(2).
015250         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚RSk            PIC 9(5).
015260      05 ˜A—¿˜JŽ©|—âãª–@‚RSk.
015270         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚RSk        PIC 9(4).
015280         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚RSk        PIC 9(2).
015290         07 ˜A—¿˜JŽ©|—âãª–@—¿‚RSk          PIC 9(4).
015300      05 ˜A—¿˜JŽ©|‰·ãª–@‚RSk.
015310         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚RSk        PIC 9(4).
015320         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚RSk        PIC 9(2).
015330         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚RSk          PIC 9(4).
015340      05 ˜A—¿˜JŽ©|“d—Ã‚RSk.
015350         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚RSk          PIC 9(4).
015360         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚RSk          PIC 9(2).
015370         07 ˜A—¿˜JŽ©|“d—Ã—¿‚RSk            PIC 9(5).
015380      05 ˜A—¿˜JŽ©|¬Œv‚RSk                 PIC 9(6).
015390      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚RSk           PIC 9(3).
015400      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚RSk           PIC 9(6).
015410*/‚R•”ˆÊ–Ú‘½•”ˆÊ
015420      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚R           PIC 9(4).
015430      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚R           PIC 9(2).
015440      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚R             PIC 9(5).
015450*/‚R•”ˆÊ–Ú’·Šú
015460      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚R             PIC 9(4).
015470      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚R             PIC 9(2).
015480      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚R               PIC 9(5).
015490*/‚R•”ˆÊ–Ú‘½•”ˆÊSk
015500      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚R       PIC 9(4).
015510      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚R       PIC 9(2).
015520      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚R         PIC 9(5).
015530**********
015540* ‚S•”ˆÊ *
015550**********
015560   03 ˜A—¿˜JŽ©|•”ˆÊ‚S.
015570      05 ˜A—¿˜JŽ©|Œã—Ã‚S.
015580         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚S          PIC 9(4).
015590         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚S          PIC 9(2).
015600         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚S            PIC 9(5).
015610      05 ˜A—¿˜JŽ©|—âãª–@‚S.
015620         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚S        PIC 9(4).
015630         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚S        PIC 9(2).
015640         07 ˜A—¿˜JŽ©|—âãª–@—¿‚S          PIC 9(4).
015650      05 ˜A—¿˜JŽ©|‰·ãª–@‚S.
015660         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚S        PIC 9(4).
015670         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚S        PIC 9(2).
015680         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚S          PIC 9(4).
015690      05 ˜A—¿˜JŽ©|“d—Ã‚S.
015700         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚S          PIC 9(4).
015710         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚S          PIC 9(2).
015720         07 ˜A—¿˜JŽ©|“d—Ã—¿‚S            PIC 9(5).
015730      05 ˜A—¿˜JŽ©|¬Œv‚S                 PIC 9(6).
015740      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚S           PIC 9(3).
015750      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚S           PIC 9(6).
015760**************
015770* ‚S•”ˆÊSk *
015780**************
015790   03 ˜A—¿˜JŽ©|•”ˆÊ‚SSk.
015800      05 ˜A—¿˜JŽ©|Œã—Ã‚SSk.
015810         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚SSk          PIC 9(4).
015820         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚SSk          PIC 9(2).
015830         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚SSk            PIC 9(5).
015840      05 ˜A—¿˜JŽ©|—âãª–@‚SSk.
015850         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚SSk        PIC 9(4).
015860         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚SSk        PIC 9(2).
015870         07 ˜A—¿˜JŽ©|—âãª–@—¿‚SSk          PIC 9(4).
015880      05 ˜A—¿˜JŽ©|‰·ãª–@‚SSk.
015890         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚SSk        PIC 9(4).
015900         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚SSk        PIC 9(2).
015910         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚SSk          PIC 9(4).
015920      05 ˜A—¿˜JŽ©|“d—Ã‚SSk.
015930         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚SSk          PIC 9(4).
015940         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚SSk          PIC 9(2).
015950         07 ˜A—¿˜JŽ©|“d—Ã—¿‚SSk            PIC 9(5).
015960      05 ˜A—¿˜JŽ©|¬Œv‚SSk                 PIC 9(6).
015970      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚SSk           PIC 9(3).
015980      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚SSk           PIC 9(6).
015990*/‚S•”ˆÊ–Ú‘½•”ˆÊ
016000      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚S           PIC 9(4).
016010      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚S           PIC 9(2).
016020      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚S             PIC 9(5).
016030*/‚S•”ˆÊ–Ú’·Šú
016040      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚S             PIC 9(4).
016050      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚S             PIC 9(2).
016060      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚S               PIC 9(5).
016070*/‚S•”ˆÊ–Ú‘½•”ˆÊSk
016080      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚S       PIC 9(4).
016090      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚S       PIC 9(2).
016100      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚S         PIC 9(5).
016110**********
016120* ‚T•”ˆÊ *
016130**********
016140   03 ˜A—¿˜JŽ©|•”ˆÊ‚T.
016150      05 ˜A—¿˜JŽ©|Œã—Ã‚T.
016160         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚T          PIC 9(4).
016170         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚T          PIC 9(2).
016180         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚T            PIC 9(5).
016190      05 ˜A—¿˜JŽ©|—âãª–@‚T.
016200         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚T        PIC 9(4).
016210         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚T        PIC 9(2).
016220         07 ˜A—¿˜JŽ©|—âãª–@—¿‚T          PIC 9(4).
016230      05 ˜A—¿˜JŽ©|‰·ãª–@‚T.
016240         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚T        PIC 9(4).
016250         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚T        PIC 9(2).
016260         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚T          PIC 9(4).
016270      05 ˜A—¿˜JŽ©|“d—Ã‚T.
016280         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚T          PIC 9(4).
016290         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚T          PIC 9(2).
016300         07 ˜A—¿˜JŽ©|“d—Ã—¿‚T            PIC 9(5).
016310      05 ˜A—¿˜JŽ©|¬Œv‚T                 PIC 9(6).
016320      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚T           PIC 9(3).
016330      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚T           PIC 9(6).
016340**************
016350* ‚T•”ˆÊSk *
016360**************
016370   03 ˜A—¿˜JŽ©|•”ˆÊ‚TSk.
016380      05 ˜A—¿˜JŽ©|Œã—Ã‚TSk.
016390         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚TSk          PIC 9(4).
016400         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚TSk          PIC 9(2).
016410         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚TSk            PIC 9(5).
016420      05 ˜A—¿˜JŽ©|—âãª–@‚TSk.
016430         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚TSk        PIC 9(4).
016440         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚TSk        PIC 9(2).
016450         07 ˜A—¿˜JŽ©|—âãª–@—¿‚TSk          PIC 9(4).
016460      05 ˜A—¿˜JŽ©|‰·ãª–@‚TSk.
016470         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚TSk        PIC 9(4).
016480         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚TSk        PIC 9(2).
016490         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚TSk          PIC 9(4).
016500      05 ˜A—¿˜JŽ©|“d—Ã‚TSk.
016510         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚TSk          PIC 9(4).
016520         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚TSk          PIC 9(2).
016530         07 ˜A—¿˜JŽ©|“d—Ã—¿‚TSk            PIC 9(5).
016540      05 ˜A—¿˜JŽ©|¬Œv‚TSk                 PIC 9(6).
016550      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚TSk           PIC 9(3).
016560      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚TSk           PIC 9(6).
016570*/‚T•”ˆÊ–Ú‘½•”ˆÊ
016580      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚T           PIC 9(4).
016590      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚T           PIC 9(2).
016600      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚T             PIC 9(5).
016610*/‚T•”ˆÊ–Ú’·Šú
016620      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚T             PIC 9(4).
016630      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚T             PIC 9(2).
016640      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚T               PIC 9(5).
016650*/‚T•”ˆÊ–Ú‘½•”ˆÊSk
016660      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚T       PIC 9(4).
016670      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚T       PIC 9(2).
016680      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚T         PIC 9(5).
016690**********
016700* ‚U•”ˆÊ *
016710**********
016720   03 ˜A—¿˜JŽ©|•”ˆÊ‚U.
016730      05 ˜A—¿˜JŽ©|Œã—Ã‚U.
016740         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚U          PIC 9(4).
016750         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚U          PIC 9(2).
016760         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚U            PIC 9(5).
016770      05 ˜A—¿˜JŽ©|—âãª–@‚U.
016780         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚U        PIC 9(4).
016790         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚U        PIC 9(2).
016800         07 ˜A—¿˜JŽ©|—âãª–@—¿‚U          PIC 9(4).
016810      05 ˜A—¿˜JŽ©|‰·ãª–@‚U.
016820         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚U        PIC 9(4).
016830         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚U        PIC 9(2).
016840         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚U          PIC 9(4).
016850      05 ˜A—¿˜JŽ©|“d—Ã‚U.
016860         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚U          PIC 9(4).
016870         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚U          PIC 9(2).
016880         07 ˜A—¿˜JŽ©|“d—Ã—¿‚U            PIC 9(5).
016890      05 ˜A—¿˜JŽ©|¬Œv‚U                 PIC 9(6).
016900      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚U           PIC 9(3).
016910      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚U           PIC 9(6).
016920**************
016930* ‚U•”ˆÊSk *
016940**************
016950   03 ˜A—¿˜JŽ©|•”ˆÊ‚USk.
016960      05 ˜A—¿˜JŽ©|Œã—Ã‚USk.
016970         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚USk          PIC 9(4).
016980         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚USk          PIC 9(2).
016990         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚USk            PIC 9(5).
017000      05 ˜A—¿˜JŽ©|—âãª–@‚USk.
017010         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚USk        PIC 9(4).
017020         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚USk        PIC 9(2).
017030         07 ˜A—¿˜JŽ©|—âãª–@—¿‚USk          PIC 9(4).
017040      05 ˜A—¿˜JŽ©|‰·ãª–@‚USk.
017050         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚USk        PIC 9(4).
017060         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚USk        PIC 9(2).
017070         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚USk          PIC 9(4).
017080      05 ˜A—¿˜JŽ©|“d—Ã‚USk.
017090         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚USk          PIC 9(4).
017100         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚USk          PIC 9(2).
017110         07 ˜A—¿˜JŽ©|“d—Ã—¿‚USk            PIC 9(5).
017120      05 ˜A—¿˜JŽ©|¬Œv‚USk                 PIC 9(6).
017130      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚USk           PIC 9(3).
017140      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚USk           PIC 9(6).
017150*/‚U•”ˆÊ–Ú‘½•”ˆÊ
017160      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚U           PIC 9(4).
017170      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚U           PIC 9(2).
017180      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚U             PIC 9(5).
017190*/‚U•”ˆÊ–Ú’·Šú
017200      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚U             PIC 9(4).
017210      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚U             PIC 9(2).
017220      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚U               PIC 9(5).
017230*/‚U•”ˆÊ–Ú‘½•”ˆÊSk
017240      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚U       PIC 9(4).
017250      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚U       PIC 9(2).
017260      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚U         PIC 9(5).
017270**********
017280* ‚V•”ˆÊ *
017290**********
017300   03 ˜A—¿˜JŽ©|•”ˆÊ‚V.
017310      05 ˜A—¿˜JŽ©|Œã—Ã‚V.
017320         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚V          PIC 9(4).
017330         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚V          PIC 9(2).
017340         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚V            PIC 9(5).
017350      05 ˜A—¿˜JŽ©|—âãª–@‚V.
017360         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚V        PIC 9(4).
017370         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚V        PIC 9(2).
017380         07 ˜A—¿˜JŽ©|—âãª–@—¿‚V          PIC 9(4).
017390      05 ˜A—¿˜JŽ©|‰·ãª–@‚V.
017400         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚V        PIC 9(4).
017410         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚V        PIC 9(2).
017420         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚V          PIC 9(4).
017430      05 ˜A—¿˜JŽ©|“d—Ã‚V.
017440         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚V          PIC 9(4).
017450         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚V          PIC 9(2).
017460         07 ˜A—¿˜JŽ©|“d—Ã—¿‚V            PIC 9(5).
017470      05 ˜A—¿˜JŽ©|¬Œv‚V                 PIC 9(6).
017480      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚V           PIC 9(3).
017490      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚V           PIC 9(6).
017500**************
017510* ‚V•”ˆÊSk *
017520**************
017530   03 ˜A—¿˜JŽ©|•”ˆÊ‚VSk.
017540      05 ˜A—¿˜JŽ©|Œã—Ã‚VSk.
017550         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚VSk          PIC 9(4).
017560         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚VSk          PIC 9(2).
017570         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚VSk            PIC 9(5).
017580      05 ˜A—¿˜JŽ©|—âãª–@‚VSk.
017590         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚VSk        PIC 9(4).
017600         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚VSk        PIC 9(2).
017610         07 ˜A—¿˜JŽ©|—âãª–@—¿‚VSk          PIC 9(4).
017620      05 ˜A—¿˜JŽ©|‰·ãª–@‚VSk.
017630         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚VSk        PIC 9(4).
017640         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚VSk        PIC 9(2).
017650         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚VSk          PIC 9(4).
017660      05 ˜A—¿˜JŽ©|“d—Ã‚VSk.
017670         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚VSk          PIC 9(4).
017680         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚VSk          PIC 9(2).
017690         07 ˜A—¿˜JŽ©|“d—Ã—¿‚VSk            PIC 9(5).
017700      05 ˜A—¿˜JŽ©|¬Œv‚VSk                 PIC 9(6).
017710      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚VSk           PIC 9(3).
017720      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚VSk           PIC 9(6).
017730*/‚V•”ˆÊ–Ú‘½•”ˆÊ
017740      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚V           PIC 9(4).
017750      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚V           PIC 9(2).
017760      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚V             PIC 9(5).
017770*/‚V•”ˆÊ–Ú’·Šú
017780      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚V             PIC 9(4).
017790      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚V             PIC 9(2).
017800      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚V               PIC 9(5).
017810*/‚V•”ˆÊ–Ú‘½•”ˆÊSk
017820      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚V       PIC 9(4).
017830      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚V       PIC 9(2).
017840      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚V         PIC 9(5).
017850**********
017860* ‚W•”ˆÊ *
017870**********
017880   03 ˜A—¿˜JŽ©|•”ˆÊ‚W.
017890      05 ˜A—¿˜JŽ©|Œã—Ã‚W.
017900         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚W          PIC 9(4).
017910         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚W          PIC 9(2).
017920         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚W            PIC 9(5).
017930      05 ˜A—¿˜JŽ©|—âãª–@‚W.
017940         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚W        PIC 9(4).
017950         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚W        PIC 9(2).
017960         07 ˜A—¿˜JŽ©|—âãª–@—¿‚W          PIC 9(4).
017970      05 ˜A—¿˜JŽ©|‰·ãª–@‚W.
017980         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚W        PIC 9(4).
017990         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚W        PIC 9(2).
018000         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚W          PIC 9(4).
018010      05 ˜A—¿˜JŽ©|“d—Ã‚W.
018020         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚W          PIC 9(4).
018030         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚W          PIC 9(2).
018040         07 ˜A—¿˜JŽ©|“d—Ã—¿‚W            PIC 9(5).
018050      05 ˜A—¿˜JŽ©|¬Œv‚W                 PIC 9(6).
018060      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚W           PIC 9(3).
018070      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚W           PIC 9(6).
018080**************
018090* ‚W•”ˆÊSk *
018100**************
018110   03 ˜A—¿˜JŽ©|•”ˆÊ‚WSk.
018120      05 ˜A—¿˜JŽ©|Œã—Ã‚WSk.
018130         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚WSk          PIC 9(4).
018140         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚WSk          PIC 9(2).
018150         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚WSk            PIC 9(5).
018160      05 ˜A—¿˜JŽ©|—âãª–@‚WSk.
018170         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚WSk        PIC 9(4).
018180         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚WSk        PIC 9(2).
018190         07 ˜A—¿˜JŽ©|—âãª–@—¿‚WSk          PIC 9(4).
018200      05 ˜A—¿˜JŽ©|‰·ãª–@‚WSk.
018210         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚WSk        PIC 9(4).
018220         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚WSk        PIC 9(2).
018230         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚WSk          PIC 9(4).
018240      05 ˜A—¿˜JŽ©|“d—Ã‚WSk.
018250         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚WSk          PIC 9(4).
018260         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚WSk          PIC 9(2).
018270         07 ˜A—¿˜JŽ©|“d—Ã—¿‚WSk            PIC 9(5).
018280      05 ˜A—¿˜JŽ©|¬Œv‚WSk                 PIC 9(6).
018290      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚WSk           PIC 9(3).
018300      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚WSk           PIC 9(6).
018310*/‚W•”ˆÊ–Ú‘½•”ˆÊ
018320      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚W           PIC 9(4).
018330      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚W           PIC 9(2).
018340      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚W             PIC 9(5).
018350*/‚W•”ˆÊ–Ú’·Šú
018360      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚W             PIC 9(4).
018370      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚W             PIC 9(2).
018380      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚W               PIC 9(5).
018390*/‚W•”ˆÊ–Ú‘½•”ˆÊSk
018400      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚W       PIC 9(4).
018410      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚W       PIC 9(2).
018420      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚W         PIC 9(5).
018430**********
018440* ‚X•”ˆÊ *
018450**********
018460   03 ˜A—¿˜JŽ©|•”ˆÊ‚X.
018470      05 ˜A—¿˜JŽ©|Œã—Ã‚X.
018480         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚X          PIC 9(4).
018490         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚X          PIC 9(2).
018500         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚X            PIC 9(5).
018510      05 ˜A—¿˜JŽ©|—âãª–@‚X.
018520         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚X        PIC 9(4).
018530         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚X        PIC 9(2).
018540         07 ˜A—¿˜JŽ©|—âãª–@—¿‚X          PIC 9(4).
018550      05 ˜A—¿˜JŽ©|‰·ãª–@‚X.
018560         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚X        PIC 9(4).
018570         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚X        PIC 9(2).
018580         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚X          PIC 9(4).
018590      05 ˜A—¿˜JŽ©|“d—Ã‚X.
018600         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚X          PIC 9(4).
018610         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚X          PIC 9(2).
018620         07 ˜A—¿˜JŽ©|“d—Ã—¿‚X            PIC 9(5).
018630      05 ˜A—¿˜JŽ©|¬Œv‚X                 PIC 9(6).
018640      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚X           PIC 9(3).
018650      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚X           PIC 9(6).
018660**************
018670* ‚X•”ˆÊSk *
018680**************
018690   03 ˜A—¿˜JŽ©|•”ˆÊ‚XSk.
018700      05 ˜A—¿˜JŽ©|Œã—Ã‚XSk.
018710         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚XSk          PIC 9(4).
018720         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚XSk          PIC 9(2).
018730         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚XSk            PIC 9(5).
018740      05 ˜A—¿˜JŽ©|—âãª–@‚XSk.
018750         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚XSk        PIC 9(4).
018760         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚XSk        PIC 9(2).
018770         07 ˜A—¿˜JŽ©|—âãª–@—¿‚XSk          PIC 9(4).
018780      05 ˜A—¿˜JŽ©|‰·ãª–@‚XSk.
018790         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚XSk        PIC 9(4).
018800         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚XSk        PIC 9(2).
018810         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚XSk          PIC 9(4).
018820      05 ˜A—¿˜JŽ©|“d—Ã‚XSk.
018830         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚XSk          PIC 9(4).
018840         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚XSk          PIC 9(2).
018850         07 ˜A—¿˜JŽ©|“d—Ã—¿‚XSk            PIC 9(5).
018860      05 ˜A—¿˜JŽ©|¬Œv‚XSk                 PIC 9(6).
018870      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚XSk           PIC 9(3).
018880      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚XSk           PIC 9(6).
018890*/‚X•”ˆÊ–Ú‘½•”ˆÊ
018900      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚X           PIC 9(4).
018910      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚X           PIC 9(2).
018920      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚X             PIC 9(5).
018930*/‚X•”ˆÊ–Ú’·Šú
018940      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚X             PIC 9(4).
018950      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚X             PIC 9(2).
018960      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚X               PIC 9(5).
018970*/‚X•”ˆÊ–Ú‘½•”ˆÊSk
018980      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚X       PIC 9(4).
018990      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚X       PIC 9(2).
019000      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚X         PIC 9(5).
019010*/—âãª–@‚Æ‰·ãª–@‚Ì’P‰¿‚ªˆá‚¤ê‡‚Ì–¾×‘AŒvŽZ‘‚Ì‹L“ü‚Ìˆ×‚Ì‘Î‰žB/061024
019020 01 ˜A—¿˜JŽ©‚Q|—¿‹à‚S IS EXTERNAL.
019030    03 ˜A—¿˜JŽ©‚Q|ãª–@’P‰¿‰ñ”               OCCURS 9.
019040       05 ˜A—¿˜JŽ©‚Q|ãª–@’P‰¿                PIC 9(5) OCCURS 6.
019050       05 ˜A—¿˜JŽ©‚Q|ãª–@‰ñ”                PIC 9(2) OCCURS 6.
019060    03 ˜A—¿˜JŽ©‚Q|“d—Ã’P‰¿‰ñ”               OCCURS 9.
019070       05 ˜A—¿˜JŽ©‚Q|“d—Ã’P‰¿                PIC 9(5) OCCURS 2.
019080       05 ˜A—¿˜JŽ©‚Q|“d—Ã‰ñ”                PIC 9(2) OCCURS 2.
019090*‰ŒŸŽž‘Š’k—¿/080919
019100 01 ˜A—¿˜JŽ©|‰ŒŸŽž‘Š’k—¿ IS EXTERNAL        PIC 9(4).
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚Í‚R‰ñŽZ’è‚Å‚«‚éB/20180830
       01 ˜A—¿˜JŽ©|“Á•ÊÞ—¿”ï IS EXTERNAL.
          03 ˜A—¿˜JŽ©|Þ—¿”ï’P‰¿                   PIC 9(5) OCCURS 7.
          03 ˜A—¿˜JŽ©|Þ—¿”ï‰ñ”                   PIC 9(1) OCCURS 7.
019110*
019120 01 ˜A|‹¤’ÊƒL[ IS EXTERNAL.
019130    03  ˜A|ƒJƒŒƒ“ƒg˜a—ï               PIC 9.
019140*
019150 01 ˜AŒv|ƒL[ IS EXTERNAL.
019160    03  ˜AŒv|•‰’S‹àŒvŽZ‹æ•ª           PIC 9.
019170    03  ˜AŒv|–{l‰Æ‘°‹æ•ª             PIC 9.
019180    03  ˜AŒv|•ÛŒ¯Ží•Ê                 PIC 9(2).
019190    03  ˜AŒv|•ÛŒ¯ŽÒ”Ô†               PIC X(10).
019200    03  ˜AŒv|Œö”ïŽí•Ê                 PIC 9(2).
019210    03  ˜AŒv|Žs’¬‘º”Ô†˜Vl           PIC X(10).
019220    03  ˜AŒv|•¬Ží•Ê                 PIC 9(2).
019230    03  ˜AŒv|Žs’¬‘º”Ô†•¬           PIC X(10).
019240    03  ˜AŒv|Š³ŽÒƒR[ƒh.
019250      05  ˜AŒv|Š³ŽÒ”Ô†               PIC 9(6).
019260      05  ˜AŒv|Ž}”Ô                   PIC X.
019270    03  ˜AŒv|Ž{p˜a—ï”NŒŽ“ú.
019280      05  ˜AŒv|Ž{p˜a—ï”NŒŽ.
019290        07  ˜AŒv|Ž{p˜a—ï             PIC 9.
019300        07  ˜AŒv|Ž{p”NŒŽ.
019310          09  ˜AŒv|Ž{p”N             PIC 9(2).
019320          09  ˜AŒv|Ž{pŒŽ             PIC 9(2).
019330      05  ˜AŒv|Ž{p“ú                 PIC 9(2).
019340    03  ˜AŒv|”ï—pŠz                   PIC 9(6).
019350    03  ˜AŒv|•‰’SŠz                   PIC 9(6).
019360    03  ˜AŒv|¿‹Šz                   PIC 9(6).
019370    03  ˜AŒv|”ï—pŠz˜Vl               PIC 9(6).
019380    03  ˜AŒv|•‰’SŠz˜Vl               PIC 9(6).
019390    03  ˜AŒv|¿‹Šz˜Vl               PIC 9(6).
019400    03  ˜AŒv|”ï—pŠz•¬               PIC 9(6).
019410    03  ˜AŒv|•‰’SŠz•¬               PIC 9(5).
019420    03  ˜AŒv|¿‹Šz•¬               PIC 9(5).
019430    03  ˜AŒv|ŽÀ“ú”                   PIC 9(2).
019440    03  ˜AŒv|•‰’S—¦                   PIC 9(3).
019450    03  ˜AŒv|•‰’S—¦‹æ•ª               PIC 9.
019460*
019470 01 ˜AŒv‚o|ƒL[ IS EXTERNAL.
019480    03 ˜AŒv‚o|”ï—pŠz‚o‚f              PIC X(8).
019490    03 ˜AŒv‚o|•‰’SŠz‚o‚f              PIC X(8).
019500    03 ˜AŒv‚o|•‰’SŠz‚o‚f˜Vl          PIC X(8).
019510    03 ˜AŒv‚o|•‰’SŠz‚o‚f•¬          PIC X(8).
019520*
019530 01 ˜A‚n|ƒL[ IS EXTERNAL.
019540    03 ˜A‚n|•‰’S—¦                    PIC 9(2).
019550*
019560 01 ˜AƒŒ|ƒL[ IS EXTERNAL.
019570    03 ˜AƒŒ|•ÛŒ¯Ží•Ê                  PIC 9(2).
019580*
019590 01 ˜Aƒ|ƒL[ IS EXTERNAL.
019600    03  ˜Aƒ|ƒƒbƒZ[ƒW               PIC N(20).
019610*
019620 01 ˜Aƒ‚R|ƒL[ IS EXTERNAL.
019630    03  ˜Aƒ‚R|ƒƒbƒZ[ƒW             PIC N(20).
019640    03  ˜Aƒ‚R|ƒƒbƒZ[ƒW‚P           PIC X(20).
019650*
019660* Šm”FƒƒbƒZ[ƒW—p (‚Qs)
019670 01 ˜Aƒ‚V|ƒL[ IS EXTERNAL.
019680    03  ˜Aƒ‚V|ƒƒbƒZ[ƒW‚P             PIC X(40).
019690    03  ˜Aƒ‚V|ƒƒbƒZ[ƒW‚Q             PIC X(40).
019700*
019710******************************************************************
019720*                      PROCEDURE  DIVISION                       *
019730******************************************************************
019740 PROCEDURE               DIVISION.
019750************
019760*           *
019770* ‰Šúˆ—   *
019780*           *
019790************
019800*
019810* ˆÈ‰º‚æ‚è‚o‚fŠJŽn
019820     PERFORM ‰Šú‰».
019830*/V—¿‹à“™‚Ì•ÏX‚Í’ñoŽž‚©‚ç—LŒø‚Ìˆ×AŠe—¿‹à‚Í¿‹”NŒŽ‚Å”»’è0101
019840*/—¿‹àAŒvŽZðŒ‚Í¿‹˜a—ï”NŒŽ‚Å“Ç‚ÝA
019850*/Ž{p‹L˜^A•‰ƒf[ƒ^‚ÍŽ{p˜a—ï”NŒŽ‚Å“Ç‚ÞB
019860     PERFORM ¿‹”NŒŽŽæ“¾.
           PERFORM Ž©”…Óî•ñŽæ“¾.
019870     PERFORM —ÝŒv€–Ú‰Šú‰».
019880     PERFORM Šî–{—¿‹àŽæ“¾.
019890     PERFORM ŒvŽZî•ñŽæ“¾.
019900*/Ž©”…Ó‚Í•‰’S‹à‚Í–³‚µ
019910*     PERFORM ŒvŽZ‚o‚fŽæ“¾.
019920     PERFORM §Œäî•ñŽæ“¾.
019930*
019940*     PERFORM ’·Šú’üŒ¸‹æ•ªŽæ“¾.
019950*
019960************
019970*           *
019980* Žåˆ—     *
019990*           *
020000************
020010*
      */•‰“o˜^‚ª–³‚¢ê‡ŒvŽZ‚µ‚È‚¢/120713
           PERFORM •‰ƒf[ƒ^Žæ“¾
           IF •”ˆÊ”‚v NOT = ZERO
020060         MOVE ˜AŒv|Ž{p˜a—ï TO Ž{‹L|Ž{p˜a—ï Ž{p˜a—ï‚c‚v
020070         MOVE ˜AŒv|Ž{p”N   TO Ž{‹L|Ž{p”N   Ž{p”N‚c‚v
020080         MOVE ˜AŒv|Ž{pŒŽ   TO Ž{‹L|Ž{pŒŽ   Ž{pŒŽ‚c‚v
020090         MOVE 1              TO Ž{‹L|Ž{p“ú   Ž{p“ú‚c‚v
020110         MOVE ˜AŒv|Š³ŽÒ”Ô† TO Ž{‹L|Š³ŽÒ”Ô† Š³ŽÒƒR[ƒh‚b‚v
020120         MOVE ˜AŒv|Ž}”Ô     TO Ž{‹L|Ž}”Ô     Ž}”Ô‚b‚v
020130*
020140         START Ž{p‹L˜^‚e KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
020150         END-START
020160*
020170         IF ó‘ÔƒL[ = "00"
020180*
020190             PERFORM “–ŒŽ‰ïŒvƒtƒ@ƒCƒ‹íœ
020220             MOVE SPACE TO I—¹ƒtƒ‰ƒO
020230             PERFORM Ž{p‹L˜^‚e“Çž
020240             PERFORM Ž{p“úƒZƒbƒg
020250*
020260             PERFORM UNTIL ( Ž{‹L|Š³ŽÒƒR[ƒh   NOT = Š³ŽÒƒR[ƒh‚b‚v   ) OR
020270                           ( Ž{‹L|Ž{p˜a—ï”NŒŽ NOT = Ž{p˜a—ï”NŒŽ‚c‚v ) OR
020280                           ( I—¹ƒtƒ‰ƒO         NOT = SPACE )
020290*
020300*                 PERFORM “úŒv€–Ú‰Šú‰»
020310*
020320                 PERFORM ‰ŒŸ—¿ŽZ’è
020330                 PERFORM ÄŒŸ—¿ŽZ’è
020340                 PERFORM ‰—Ã—¿ŽZ’è
020350                 PERFORM Œã—Ã—¿ŽZ’è
020360                 PERFORM ‚»‚Ì‘¼ŽZ’è
020370*
020380*/˜JÐŽ©”…Ó‚Ìê‡‚É•K—v‚È—¿‹à
020390                 PERFORM ‰^“®—Ã–@—¿ŽZ’è
020400                 PERFORM Žw“±ŠÇ——¿ŽZ’è
020410                 PERFORM h”‘—¿ŽZ’è
020420                 PERFORM HŽ–—¿ŽZ’è
020430*                 PERFORM ‹x‹ÆØ–¾‘—¿ŽZ’è
020440*
020450                 PERFORM Ž{pØ–¾‘—¿ŽZ’è
020460                 PERFORM Ž{p–¾×‘—¿ŽZ’è
020470                 PERFORM Ž©”…Ó‚»‚Ì‘¼‹àŠzŽZ’è
020480* •”ˆÊŠÖ˜AˆÈŠO‚Ì€–Ú‚ð—ÝŒv‚µ‚Ä‚¨‚­B
020490                 PERFORM —ÝŒvˆ—
020500*
020510*********
020520                 PERFORM ‰ïŒvƒf[ƒ^‚e‘ž
020530*********
020550                 PERFORM Ž{p‹L˜^‚e“Çž
020560                 PERFORM Ž{p“úƒZƒbƒg
020570*
020580             END-PERFORM
020590         END-IF
020600*
020610*****************************************************************
020620* ‘½•”ˆÊŠÖ˜A€–Ú‚Ì•”ˆÊ–ˆ‚P‚O‚O“‚Ì‡Œv‚©‚ç’üŒ¸—¦A’·Šú’üŒ¸—¦‚ðæ‚¶‚½
020630* ‹àŠz‚ð•”ˆÊ–ˆ‚ÉŽZo‚·‚éB
020640*****************************************************************
020650*
020660         PERFORM ‘½•”ˆÊ‡Œvˆ—
020670*
020680*     Še•”ˆÊ–ˆ‚Ìi‚P‚O‚O“‚Ì‹àŠz‚Éj’üŒ¸—¦i‘½•”ˆÊE’·Šúj‚ð
020690*     æ‚¯‚½‹àŠz‚Æ•”ˆÊˆÈŠO€–Ú‚ð‰ÁŽZ‚·‚éB
020700*
020710         COMPUTE ˜AŒv|”ï—pŠz
020720               = •”ˆÊ‡Œv‚v‚P   + •”ˆÊ‡Œv‚v‚Q     + •”ˆÊ‡Œv‚v‚R +
020730                 •”ˆÊ‡Œv‚v‚S   + •”ˆÊ‡Œv‚v‚T     + •”ˆÊ‡Œv‚v‚U +
020740                 •”ˆÊ‡Œv‚v‚V   + •”ˆÊ‡Œv‚v‚W     + •”ˆÊ‡Œv‚v‚X +
020750                 ‰ŒŸ—¿‚f‚v     + ‰ŒŸ‰ÁŽZ—¿‚f‚v   +
020760                 ÄŒŸ—¿‚f‚v     + ‰—Ã—¿‚f‚v       + ‰—Ã‰ÁŽZ—¿‚f‚v +
020770                 ‹à‘®•›Žq—¿‚f‚v + ‰‰ñˆ’u—¿‚f‚v   + Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v + ‰ŒŸŽž‘Š’k—¿‚f‚v +
020780*/˜JÐ•ÛŒ¯—p
020790                 Žw“±ŠÇ——¿‚f‚v + “Á•ÊÞ—¿”ï‚f‚v + ŒðŠ·•ï‘Ñ—¿‚f‚v +
020800                 ‰^“®—Ã–@—¿‚f‚v + h”‘—¿‚f‚v     + HŽ–—¿‚f‚v     +
020810                 ‹x‹ÆØ–¾‘—¿‚f‚v +
020820*/Ž©”…Ó—p
020830                 Ž{pØ–¾‘—¿‚f‚v + Ž{p–¾×‘—¿‚f‚v + ‚»‚Ì‘¼‹àŠz‚f‚v
020840*
020850         EVALUATE ˜AƒŒ|•ÛŒ¯Ží•Ê
020860*
020870         WHEN 80
020880             MOVE    ˜AŒv|”ï—pŠz   TO ˜A—¿˜JŽ©|”ï—pŠz
020890             MOVE    ˜AŒv|•‰’SŠz   TO ˜A—¿˜JŽ©|ˆê•”•‰’S‹à
020900             COMPUTE ˜A—¿˜JŽ©|¿‹Šz =  ˜AŒv|”ï—pŠz - ˜AŒv|•‰’SŠz
020910             COMPUTE ˜AŒv|¿‹Šz     =  ˜AŒv|”ï—pŠz - ˜AŒv|•‰’SŠz
020920*
020930         WHEN OTHER
020940             CONTINUE
020950         END-EVALUATE
020960*
020970         PERFORM ˜AŒ‹€–ÚƒZƒbƒg
020980     
020990*/ƒŒƒZƒvƒgƒtƒ@ƒCƒ‹ì¬
021000         PERFORM ƒŒƒZƒvƒgƒtƒ@ƒCƒ‹ì¬
021010     
021020* ‘S_‚Ì‚Ý‚Ì‹@”\i‚n‚b‚qƒR[ƒh‚Ìˆê•”j
021030*     PERFORM •‰’S—¦‚n‚b‚qƒZƒbƒg
021040*
      */•‰“o˜^‚ª–³‚¢ê‡ŒvŽZ‚µ‚È‚¢/120713
      */•‰•”ˆÊ‚ª‚È‚¢ê‡‚Íƒtƒ@ƒCƒ‹‚ðƒNƒŠƒA‚·‚é/131206
           ELSE
017650         PERFORM “–ŒŽ‰ïŒvƒtƒ@ƒCƒ‹íœ
018390         PERFORM ˜AŒ‹€–ÚƒZƒbƒg
018420         PERFORM ƒŒƒZƒvƒgƒtƒ@ƒCƒ‹ì¬
           END-IF.
021050************
021060*           *
021070* I—¹ˆ—   *
021080*           *
021090************
021100*
021110     PERFORM I—¹ˆ—.
021120     MOVE 1 TO PROGRAM-STATUS.
021130     EXIT PROGRAM.
021140*
021150*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
021160*================================================================*
021170 ‰Šú‰» SECTION.
021180*
021190     OPEN INPUT Ž{p‹L˜^‚e.
021200         MOVE NC"Ž{‹L" TO ƒtƒ@ƒCƒ‹–¼‚v.
021210         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021220     OPEN INPUT —¿‹àƒ}ƒXƒ^.
021230         MOVE NC"—¿‹à" TO ƒtƒ@ƒCƒ‹–¼‚v.
021240         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021250     OPEN INPUT ŒvŽZƒ}ƒXƒ^.
021260         MOVE NC"ŒvŽZ" TO ƒtƒ@ƒCƒ‹–¼‚v.
021270         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021280*     OPEN INPUT ŒvŽZ‚o‚fƒ}ƒXƒ^.
021290*         MOVE NC"Œv‚o" TO ƒtƒ@ƒCƒ‹–¼‚v.
021300*         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021310*     OPEN INPUT ŽófŽÒî•ñ‚e.
021320     OPEN I-O ŽófŽÒî•ñ‚e.
021330         MOVE NC"Žóf" TO ƒtƒ@ƒCƒ‹–¼‚v.
021340         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021350     OPEN INPUT •‰ƒf[ƒ^‚e.
021360         MOVE NC"•‰" TO ƒtƒ@ƒCƒ‹–¼‚v.
021370         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021380     OPEN INPUT §Œäî•ñƒ}ƒXƒ^.
021390         MOVE NC"§Œä" TO ƒtƒ@ƒCƒ‹–¼‚v.
021400         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021410     OPEN INPUT Œ³†ƒ}ƒXƒ^.
021420         MOVE NC"Œ³†" TO ƒtƒ@ƒCƒ‹–¼‚v.
021430         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021440     OPEN INPUT –¼Ìƒ}ƒXƒ^.
021450         MOVE NC"–¼Ì" TO ƒtƒ@ƒCƒ‹–¼‚v.
021460         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021470********
021480     OPEN INPUT ‰ïŒv—ÌŽû‚e.
021490         MOVE NC"‰ïŒv—ÌŽû" TO ƒtƒ@ƒCƒ‹–¼‚v.
021500         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021510     OPEN I-O ‰ïŒvƒf[ƒ^‚e.
021520         MOVE NC"‰ïŒv" TO ƒtƒ@ƒCƒ‹–¼‚v.
021530         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021540     OPEN I-O   ƒŒƒZƒvƒg‚e.
021550         MOVE NC"ƒŒƒZ" TO ƒtƒ@ƒCƒ‹–¼‚v.
021560         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021570     OPEN I-O   ƒŒƒZƒvƒg˜JŽ©‚e.
021580         MOVE NC"˜JŽ©" TO ƒtƒ@ƒCƒ‹–¼‚v.
021590         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
005650     OPEN INPUT Ž©”…Óî•ñ‚e.
005660         MOVE NC"Ž©”…" TO ƒtƒ@ƒCƒ‹–¼‚v.
005670         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018610     OPEN I-O XVŽ{p‹L˜^‚e.
018620         MOVE NC"XŽ{" TO ƒtƒ@ƒCƒ‹–¼‚v.
018630         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
021600********
021610********
021620*
021630     MOVE ZERO TO ˜AŒv|”ï—pŠz ˜AŒv|•‰’SŠz ˜AŒv|¿‹Šz
021640                  ˜AŒv|”ï—pŠz˜Vl ˜AŒv|•‰’SŠz˜Vl ˜AŒv|¿‹Šz˜Vl
021650                  ˜AŒv|”ï—pŠz•¬ ˜AŒv|•‰’SŠz•¬ ˜AŒv|¿‹Šz•¬.
021660*
021670     MOVE SPACE TO ˜A—¿˜JŽ©|—¿‹à‚P.
021680     MOVE SPACE TO ˜A—¿˜JŽ©|—¿‹à‚Q.
021690     MOVE SPACE TO ˜A—¿˜JŽ©|—¿‹à‚R.
021700     MOVE ZERO  TO ˜A—¿˜JŽ©|‰ŒŸŽž‘Š’k—¿ ˜A—¿˜JŽ©|“Á•ÊÞ—¿”ï.
021710     INITIALIZE    ˜A—¿˜JŽ©|—¿‹à‚P.
021720     INITIALIZE    ˜A—¿˜JŽ©|—¿‹à‚Q.
021730     INITIALIZE    ˜A—¿˜JŽ©|—¿‹à‚R.
021740     INITIALIZE    ˜A—¿˜JŽ©|‰ŒŸŽž‘Š’k—¿ ˜A—¿˜JŽ©|“Á•ÊÞ—¿”ï.
021750*/061024
021760     MOVE SPACE TO ˜A—¿˜JŽ©‚Q|—¿‹à‚S.
021770     INITIALIZE    ˜A—¿˜JŽ©‚Q|—¿‹à‚S.
021780*
021790     MOVE ˜AŒv|•ÛŒ¯Ží•Ê TO •ÛŒ¯Ží•Ê‚v.
021800     MOVE ˜AŒv|Œö”ïŽí•Ê TO Œö”ïŽí•Ê‚v.
021810     MOVE ˜AŒv|•¬Ží•Ê TO •¬Ží•Ê‚v.
021820*================================================================*
021830 ƒI[ƒvƒ“ƒ`ƒFƒbƒN SECTION.
021840*
021850     IF ó‘ÔƒL[  NOT =  "00"
021860         DISPLAY ƒtƒ@ƒCƒ‹–¼‚v NC"‚eƒI[ƒvƒ“ƒGƒ‰[" UPON CONS
021870         DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[           UPON CONS
021880         DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢" UPON CONS
021890*-----------------------------------------*
021900         CALL "actcshm"  WITH C LINKAGE
021910*-----------------------------------------*
021920         ACCEPT  ƒL[“ü—Í FROM CONS
021930         PERFORM ƒtƒ@ƒCƒ‹•Â½
021940         MOVE ZERO TO PROGRAM-STATUS
021950         EXIT PROGRAM.
021960*================================================================*
021970 ƒtƒ@ƒCƒ‹•Â½ SECTION.
021980*
021990     CLOSE Ž{p‹L˜^‚e —¿‹àƒ}ƒXƒ^ ŒvŽZƒ}ƒXƒ^ 
022000           ŽófŽÒî•ñ‚e •‰ƒf[ƒ^‚e §Œäî•ñƒ}ƒXƒ^ –¼Ìƒ}ƒXƒ^
022010           Œ³†ƒ}ƒXƒ^   Ž©”…Óî•ñ‚e.
022020*         ŒvŽZ‚o‚fƒ}ƒXƒ^
022030****
022040     CLOSE ‰ïŒv—ÌŽû‚e ‰ïŒvƒf[ƒ^‚e ƒŒƒZƒvƒg‚e ƒŒƒZƒvƒg˜JŽ©‚e XVŽ{p‹L˜^‚e.
022050*================================================================*
022060 I—¹ˆ— SECTION.
022070*
022080     PERFORM ƒtƒ@ƒCƒ‹•Â½.
022090*================================================================*
022100 ƒGƒ‰[•\Ž¦ SECTION.
022110*
022120     DISPLAY ƒtƒ@ƒCƒ‹–¼‚v NC"ƒtƒ@ƒCƒ‹‘žƒGƒ‰["   UPON CONS.
022130     DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[               UPON CONS.
022140     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
022150     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢" UPON CONS.
022160*-----------------------------------------*
022170         CALL "actcshm"  WITH C LINKAGE
022180*-----------------------------------------*
022190     ACCEPT  ƒL[“ü—Í FROM CONS.
022200     PERFORM ƒtƒ@ƒCƒ‹•Â½.
022210     MOVE ZERO TO PROGRAM-STATUS
022220     EXIT PROGRAM.
022230*================================================================*
022240 ƒGƒ‰[•\Ž¦‚q SECTION.
022250*
022260     DISPLAY ƒtƒ@ƒCƒ‹–¼‚v NC"ƒtƒ@ƒCƒ‹“ÇžƒGƒ‰["   UPON CONS.
022270     DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[               UPON CONS.
022280     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
022290     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢" UPON CONS.
022300*-----------------------------------------*
022310         CALL "actcshm"  WITH C LINKAGE
022320*-----------------------------------------*
022330     ACCEPT  ƒL[“ü—Í FROM CONS.
022340     PERFORM ƒtƒ@ƒCƒ‹•Â½.
022350     MOVE ZERO TO PROGRAM-STATUS
022360     EXIT PROGRAM.
022370*================================================================*
022380 ƒGƒ‰[•\Ž¦‚»‚Ì‘¼ SECTION.
022390*
022400     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
022410     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢" UPON CONS.
022420*-----------------------------------------*
022430         CALL "actcshm"  WITH C LINKAGE
022440*-----------------------------------------*
022450     ACCEPT  ƒL[“ü—Í FROM CONS.
022460     PERFORM ƒtƒ@ƒCƒ‹•Â½.
022470     EXIT PROGRAM.
022480*================================================================*
022490 Ž{p‹L˜^‚e“Çž SECTION.
022500*
022510     READ Ž{p‹L˜^‚e NEXT
022520     AT END
022530         MOVE "YES" TO I—¹ƒtƒ‰ƒO
022540     END-READ.
022550*
022560*================================================================*
022570 ‰ŒŸ—¿ŽZ’è SECTION.
022580*
022590     MOVE ZERO TO ‰ŒŸ—¿‚v.
022600     MOVE ZERO TO ‰ŒŸ‰ÁŽZ—¿‚v.
022610     MOVE ZERO TO ‰ŒŸŽž‘Š’k—¿‚v.
019330*/“ú–ˆ€–Ú‚Ì‰Šú‰»/
019340     MOVE ZERO TO  ‰ŒŸŽžŠÔŠO‚m‚v ‰ŒŸ‹x“ú‚m‚v ‰ŒŸ[–é‚m‚v.
022620*
022630     IF ( Ž{‹L|f—Ã‹æ•ª     = 2 ) AND
022640        ( ‰ŒŸ¿‹‹æ•ª‚v NOT = 1 ) AND
022650        ( Ž{‹L|‰ŒŸ—¿¿‹‹æ•ª  = 1 )
022660         COMPUTE ‰ŒŸ—¿‚v   = ‰ŒŸ—¿‚v   + ‰ŒŸ—¿‚d‚v
022670         COMPUTE ‰ŒŸ‰ñ”‚v = ‰ŒŸ‰ñ”‚v + 1
022680* ‰ŒŸ‰ÁŽZ
022690         EVALUATE Ž{‹L|‰ŒŸ‰ÁŽZ
022700         WHEN ZERO
022710             CONTINUE
022720         WHEN 1
022730             MOVE 1 TO ‰ŒŸŽžŠÔŠO‚v ‰ŒŸŽžŠÔŠO‚m‚v
022740             COMPUTE ‰ŒŸ‰ÁŽZ—¿‚v = ‰ŒŸ‰ÁŽZ—¿‚v + ŽžŠÔŠO‚d‚v
022750         WHEN 2
022760             MOVE 1 TO ‰ŒŸ‹x“ú‚v   ‰ŒŸ‹x“ú‚m‚v
022770             COMPUTE ‰ŒŸ‰ÁŽZ—¿‚v = ‰ŒŸ‰ÁŽZ—¿‚v + ‹x“ú‚d‚v
022780         WHEN 3
022790             MOVE 1 TO ‰ŒŸ[–é‚v   ‰ŒŸ[–é‚m‚v
022800             COMPUTE ‰ŒŸ‰ÁŽZ—¿‚v = ‰ŒŸ‰ÁŽZ—¿‚v + [–é‚d‚v
022810         WHEN OTHER
022820             CONTINUE
022830         END-EVALUATE
022840     END-IF.
022850* ‰ŒŸŽž‘Š’k—¿/080919
022860     IF (Ž{‹L|f—Ã‹æ•ª = 2 ) AND (Ž{‹L|‰ŒŸŽž‘Š’k—¿‹æ•ª NOT = 1)
022870         COMPUTE ‰ŒŸŽž‘Š’k—¿‚v = ‰ŒŸŽž‘Š’k—¿‚v + ‰ŒŸŽž‘Š’k—¿‚d‚v
022880     END-IF.
022890*================================================================*
022900 ÄŒŸ—¿ŽZ’è SECTION.
022910*
022920     MOVE ZERO TO ÄŒŸ—¿‚v.
022930*
022940     IF Ž{‹L|ÄŒŸ—¿¿‹  = 1
022950         COMPUTE ÄŒŸ—¿‚v   = ÄŒŸ—¿‚v + ÄŒŸ—¿‚d‚v
022960         COMPUTE ÄŒŸ‰ñ”‚v = ÄŒŸ‰ñ”‚v + 1
022970     END-IF.
022980*
022990*================================================================*
023000 ‰—Ã—¿ŽZ’è SECTION.
023010******************************************************************
023020* ‰—Ã‰ÁŽZ‚ÍA’[”‚ðŠÜ‚ß‚½‹àŠz‚ðŠî–{‚Æ‚µAŒvŽZ‚·‚éB
023030* 100/100‚Ìê‡‚ÍA100‚ð‰ÁŽZ—¿i–éŠÔE–\•—‰Jj‚Ì•ª‚Æ‚µ‚ÄŒvŽZ‚·‚éB
023040******************************************************************
023050*
023060     MOVE ZERO TO ‰—Ã—¿‚v.
023080     MOVE ZERO TO ‰—Ã‰ÁŽZ—¿‚v.
019810*/“ú–ˆ€–Ú‚Ì‰Šú‰»
019820     MOVE ZERO TO ‰—Ã–éŠÔ‚m‚v ‰—Ã“ï˜H‚m‚v ‰—Ã–\•—‰J‚m‚v ‰—ÃŽžŠÔŠO‚m‚v.
023090*
023100     IF Ž{‹L|‰—Ã‹——£ NOT = ZERO
023110* ®”‚É‚·‚é‚½‚ßA‚P‚O‚ðŠ|‚¯‚é
023120         MOVE Ž{‹L|‰—Ã‹——£ TO ‰—Ã‹——£‚v
020500*/2”N9ŒŽ‚æ‚è4.1km‚ð’´‚¦‚é‰—Ã—¿‚Í“¯Šz«««/20200908
023130*         COMPUTE ‰—Ã”íœ”‚v = Ž{‹L|‰—Ã‹——£ * 10
020510         IF (˜AŒv|Ž{p˜a—ï”NŒŽ >= 50209) AND (Ž{‹L|‰—Ã‹——£ >= 4.1)
020520             COMPUTE ‰—Ã”íœ”‚v = 4.1 * 10
020530         ELSE
020540             COMPUTE ‰—Ã”íœ”‚v = Ž{‹L|‰—Ã‹——£ * 10
020550         END-IF
020500*/2”N9ŒŽ‚æ‚è4.1km‚ð’´‚¦‚é‰—Ã—¿‚Í“¯Šzªªª/20200908
023140         COMPUTE ‰—Ãœ”‚v   = ‰—ÃŠî–{‹——£‚j‚v * 10
023150         DIVIDE ‰—Ãœ”‚v INTO ‰—Ã”íœ”‚v GIVING ‰—Ã¤‚v
023160                           REMAINDER ‰—Ãè—]‚v
023170         COMPUTE ‰—Ã—¿‚v = ‰—ÃŠî–{—¿‹à‚d‚v +
023180                          ( ‰—Ã’Ç‰Á—¿‹à‚d‚v * ( ‰—Ã¤‚v - 1 ) )
023190         IF ‰—Ãè—]‚v > ZERO
023200             COMPUTE ‰—Ã—¿‚v = ‰—Ã—¿‚v + ‰—Ã’Ç‰Á—¿‹à‚d‚v
023210         END-IF
023220* ‰—Ã‰ÁŽZi–éŠÔE–\•—‰J“™j
023230* ƒŒƒZƒvƒg‚Ìê‡A‹——£‚Ì‰ÁŽZ‚ÍA‰—Ã—¿‚ÉŠÜ‚ßA‰—Ã‰ÁŽZ‚ÍA–éŠÔE–\•—‰J‚Ì
023240* ‰ÁŽZ‚Ì‚Ý‚Æ‚·‚é
023250         IF Ž{‹L|‰—Ã‰ÁŽZ NOT = ZERO
023260             EVALUATE Ž{‹L|‰—Ã‰ÁŽZ
023270             WHEN 4
023280                 MOVE ‰—Ã–éŠÔ‰ÁŽZ—¦‚d‚v   TO ‰—Ã‰ÁŽZ—¦‚d‚v
023290             WHEN 1
023300                 MOVE ‰—Ã[–é‰ÁŽZ—¦‚d‚v   TO ‰—Ã‰ÁŽZ—¦‚d‚v
023310             WHEN 2
023320                 MOVE ‰—Ã“ï˜H‰ÁŽZ—¦‚d‚v   TO ‰—Ã‰ÁŽZ—¦‚d‚v
023330             WHEN 3
023340                 MOVE ‰—Ã–\•—‰J‰ÁŽZ—¦‚d‚v TO ‰—Ã‰ÁŽZ—¦‚d‚v
023350             WHEN OTHER
023360                 DISPLAY NC"‰—Ã‰ÁŽZ‚ª•s³‚Å‚·" UPON CONS
023370             END-EVALUATE
023380             COMPUTE ‰—Ã‰ÁŽZ—¿‚v = ‰—Ã—¿‚v * ( ‰—Ã‰ÁŽZ—¦‚d‚v / 100 )
023390             COMPUTE ‰—Ã‰ÁŽZ—¦‚v     = ‰—Ã‰ÁŽZ—¦‚d‚v - 100
023400             COMPUTE ‰—Ã‰ÁŽZ—¦’†ŠÔ‚v = ‰—Ã‰ÁŽZ—¦‚v / 100
023410             COMPUTE ‰—Ã‰ÁŽZ—¿‚v     = ‰—Ã—¿‚v * ‰—Ã‰ÁŽZ—¦’†ŠÔ‚v
023420*
023430             EVALUATE Ž{‹L|‰—Ã‰ÁŽZ
023440             WHEN 1
023450                 MOVE 1 TO ‰—Ã–éŠÔ‚v   ‰—Ã–éŠÔ‚m‚v
023460             WHEN 2
023470                 MOVE 1 TO ‰—Ã“ï˜H‚v   ‰—Ã“ï˜H‚m‚v
023480             WHEN 3
023490                 MOVE 1 TO ‰—Ã–\•—‰J‚v ‰—Ã–\•—‰J‚m‚v
023500             WHEN 4
023510                 MOVE 1 TO ‰—ÃŽžŠÔŠO‚v ‰—ÃŽžŠÔŠO‚m‚v
023520             WHEN OTHER
023530                 CONTINUE
023540             END-EVALUATE
023550*
023560         END-IF
023570*
023580         COMPUTE ‰—Ã‰ñ”‚f‚v = ‰—Ã‰ñ”‚f‚v + 1
023590*
023600         MOVE ‰—Ã—¿‚v TO ‰—Ã—¿‚v‚Q
023610     END-IF.
023620*
023630*================================================================*
023640 Šî–{—¿‹àŽæ“¾ SECTION.
023650*
023660*Ž©”…ÓŠî–{—¿‹à—p‚Ì‹æ•ªƒR[ƒh‚Í‚O‚T
023670     MOVE 05             TO —¿|‹æ•ªƒR[ƒh.
023680     MOVE ZEROS          TO —¿|•‰Ží•Ê.
023690     MOVE ZEROS          TO —¿|•”ˆÊ.
023700     MOVE ZEROS          TO —¿|¶‰E‹æ•ª.
023710     MOVE ZEROS          TO —¿|•‰ˆÊ’u”Ô†.
023720*/V—¿‹à“™‚Ì•ÏX‚Í’ñoŽž‚©‚ç—LŒø‚Ìˆ×AŠe—¿‹à‚Í¿‹”NŒŽ‚Å”»’è0101
023730*     MOVE ˜AŒv|Ž{p˜a—ï TO —¿|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
023740*     MOVE ˜AŒv|Ž{p”N   TO —¿|ŠJŽn”N   Ž{p”N‚b‚v.
023750*     MOVE ˜AŒv|Ž{pŒŽ   TO —¿|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
023760     MOVE ¿‹˜a—ï‚v TO —¿|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
023770     MOVE ¿‹”N‚v   TO —¿|ŠJŽn”N   Ž{p”N‚b‚v.
023780     MOVE ¿‹ŒŽ‚v   TO —¿|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
023790*
023800     START —¿‹àƒ}ƒXƒ^ KEY IS <= —¿|‹æ•ªƒR[ƒh
023810                                —¿|•”ˆÊƒR[ƒh
023820                                —¿|ŠJŽn˜a—ï”NŒŽ
023830                                REVERSED
023840     END-START.
023850*
023860     IF ó‘ÔƒL[ = "00"
023870         READ —¿‹àƒ}ƒXƒ^ NEXT
023880         AT END
023890*/ƒGƒ‰[•\Ž¦‚ÌC³
023900             PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
023910             DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½Šî–{—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
023920                     " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
023930                     " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
023940             PERFORM I—¹ˆ—
023950             MOVE ZERO TO PROGRAM-STATUS
023960             EXIT PROGRAM
023970         NOT AT END
023980*/Ž©”…Ó‚Ì—¿‹à‚©”»’è
023990             IF —¿|‹æ•ªƒR[ƒh NOT = 05
024000*/ƒGƒ‰[•\Ž¦‚ÌC³
024010                 PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
024020                 DISPLAY "Ž©”…ÓŠî–{—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
024030                         " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
024040                         " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
024050                 PERFORM I—¹ˆ—
024060                 MOVE ZERO TO PROGRAM-STATUS
024070                 EXIT PROGRAM
024080             ELSE
024090*
024100                 IF ( Ž{p˜a—ï”NŒŽ‚b‚v >= —¿‚d|ŠJŽn˜a—ï”NŒŽ ) AND
024110                    ( Ž{p˜a—ï”NŒŽ‚b‚v <= —¿‚d|I—¹˜a—ï”NŒŽ )
024120*/‰ŒŸŠÖŒW
024130                     MOVE —¿‚d|‰ŒŸ—¿           TO ‰ŒŸ—¿‚d‚v
024140                     MOVE —¿‚d|ŽžŠÔŠO           TO ŽžŠÔŠO‚d‚v
024150                     MOVE —¿‚d|‹x“ú             TO ‹x“ú‚d‚v
024160                     MOVE —¿‚d|[–é             TO [–é‚d‚v
024170*
024180                     MOVE —¿‚d|ÄŒŸ—¿           TO ÄŒŸ—¿‚d‚v
024190*/‰—ÃŠÖŒW
024200                     MOVE —¿‚d|Šî–{—¿‹à         TO ‰—ÃŠî–{—¿‹à‚d‚v
024210                     MOVE —¿‚d|’Ç‰Á—¿‹à         TO ‰—Ã’Ç‰Á—¿‹à‚d‚v
024220                     MOVE —¿‚d|ŽžŠÔŠO‰ÁŽZ—¦     TO ‰—Ã–éŠÔ‰ÁŽZ—¦‚d‚v
024230                     MOVE —¿‚d|[–é‰ÁŽZ—¦       TO ‰—Ã[–é‰ÁŽZ—¦‚d‚v
024240                     MOVE —¿‚d|“ï˜H‰ÁŽZ—¦       TO ‰—Ã“ï˜H‰ÁŽZ—¦‚d‚v
024250                     MOVE —¿‚d|–\•—‰J‰ÁŽZ—¦     TO ‰—Ã–\•—‰J‰ÁŽZ—¦‚d‚v
024260*
024270                     MOVE —¿‚d|Žw“±ŠÇ——¿       TO Žw“±ŠÇ——¿‚d‚v
024280                     MOVE —¿‚d|‰^“®—Ã–@—¿       TO ‰^“®—Ã–@—¿‚d‚v
024290                     MOVE —¿‚d|h”‘—¿           TO h”‘—¿‚d‚v
024300                     MOVE —¿‚d|HŽ–—¿           TO HŽ–—¿‚d‚v
024310                     MOVE —¿‚d|Ž{pî•ñ’ñ‹Ÿ—¿   TO Ž{pî•ñ’ñ‹Ÿ—¿‚d‚v
024320                     MOVE —¿‚d|Ž{pØ–¾‘—¿     TO Ž{pØ–¾‘—¿‚d‚v
024330                     MOVE —¿‚d|Ž{p–¾×‘—¿     TO Ž{p–¾×‘—¿‚d‚v
024340*
024350                     MOVE —¿‚d|—âãª–@—¿         TO —âãª–@—¿‚Q‚R‚d‚v
024360                     MOVE —¿‚d|—âãª–@—¿‚R‚O     TO —âãª–@—¿‚R‚O‚d‚v
024370* @@@^–@•”ˆÊ”EŒo‰ßŒŽE•‰Ží•Ê@–^
024380                     MOVE —¿‚d|‰·ãª–@—¿‚Q‚R     TO ‰·ãª–@—¿‚Q‚R‚d‚v
024390                     MOVE —¿‚d|‰·ãª–@—¿‚Q‚SœÜ TO ‰·ãª–@—¿‚Q‚SœÜ‚d‚v
024400                     MOVE —¿‚d|‰·ãª–@—¿‚Q‚S‘¼   TO ‰·ãª–@—¿‚Q‚S‘¼‚d‚v
024410                     MOVE —¿‚d|‰·ãª–@—¿‚R‚O     TO ‰·ãª–@—¿‚R‚O‚d‚v
024420*
024430                     MOVE —¿‚d|“d—Ã—¿‚Q‚R       TO “d—Ã—¿‚Q‚R‚d‚v
024440                     MOVE —¿‚d|“d—Ã—¿‚Q‚SœÜ   TO “d—Ã—¿‚Q‚SœÜ‚d‚v
024450                     MOVE —¿‚d|“d—Ã—¿‚Q‚S‘¼     TO “d—Ã—¿‚Q‚S‘¼‚d‚v
024460                     MOVE —¿‚d|“d—Ã—¿‚R‚O       TO “d—Ã—¿‚R‚O‚d‚v
024470                     MOVE —¿‚d|‰ŒŸŽž‘Š’k—¿     TO ‰ŒŸŽž‘Š’k—¿‚d‚v
024480                 ELSE
024490*/ƒGƒ‰[•\Ž¦‚ÌC³
024500                     PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
024510                     DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
024520                             " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
024530                             " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
024540                     PERFORM I—¹ˆ—
024550                     MOVE ZERO TO PROGRAM-STATUS
024560                     EXIT PROGRAM
024570                 END-IF
024580             END-IF
024590         END-READ
024600     END-IF.
024610*
      */ŽófŽÒŒÅ—L’P‰¿
           IF Ž©”…|ŒÅ—L’P‰¿Žg—p‹æ•ª = 1
               MOVE Ž©”…|‰ŒŸ—¿           TO ‰ŒŸ—¿‚d‚v
               MOVE Ž©”…|ŽžŠÔŠO           TO ŽžŠÔŠO‚d‚v
               MOVE Ž©”…|‹x“ú             TO ‹x“ú‚d‚v
               MOVE Ž©”…|[–é             TO [–é‚d‚v
024170*
               MOVE Ž©”…|ÄŒŸ—¿           TO ÄŒŸ—¿‚d‚v
               MOVE Ž©”…|Žw“±ŠÇ——¿       TO Žw“±ŠÇ——¿‚d‚v
               MOVE Ž©”…|‰^“®—Ã–@—¿       TO ‰^“®—Ã–@—¿‚d‚v
               MOVE Ž©”…|Ž{pî•ñ’ñ‹Ÿ—¿   TO Ž{pî•ñ’ñ‹Ÿ—¿‚d‚v
               MOVE Ž©”…|Ž{pØ–¾‘—¿     TO Ž{pØ–¾‘—¿‚d‚v
               MOVE Ž©”…|Ž{p–¾×‘—¿     TO Ž{p–¾×‘—¿‚d‚v
024340*
               MOVE Ž©”…|—âãª–@—¿         TO —âãª–@—¿‚Q‚R‚d‚v
               MOVE Ž©”…|—âãª–@—¿‚R‚O     TO —âãª–@—¿‚R‚O‚d‚v
024370* @@@^–@•”ˆÊ”EŒo‰ßŒŽE•‰Ží•Ê@–^
               MOVE Ž©”…|‰·ãª–@—¿‚Q‚R     TO ‰·ãª–@—¿‚Q‚R‚d‚v
               MOVE Ž©”…|‰·ãª–@—¿‚Q‚SœÜ TO ‰·ãª–@—¿‚Q‚SœÜ‚d‚v
               MOVE Ž©”…|‰·ãª–@—¿‚Q‚S‘¼   TO ‰·ãª–@—¿‚Q‚S‘¼‚d‚v
               MOVE Ž©”…|‰·ãª–@—¿‚R‚O     TO ‰·ãª–@—¿‚R‚O‚d‚v
024420*
               MOVE Ž©”…|“d—Ã—¿‚Q‚R       TO “d—Ã—¿‚Q‚R‚d‚v
               MOVE Ž©”…|“d—Ã—¿‚Q‚SœÜ   TO “d—Ã—¿‚Q‚SœÜ‚d‚v
               MOVE Ž©”…|“d—Ã—¿‚Q‚S‘¼     TO “d—Ã—¿‚Q‚S‘¼‚d‚v
               MOVE Ž©”…|“d—Ã—¿‚R‚O       TO “d—Ã—¿‚R‚O‚d‚v
               MOVE Ž©”…|‰ŒŸŽž‘Š’k—¿     TO ‰ŒŸŽž‘Š’k—¿‚d‚v
           END-IF.
024620*================================================================*
024630 •‰•Ê—¿‹àŽæ“¾ SECTION.
024640*
024650*/œÜA•s‘SœÜ‚Ìê‡•‰“ú‚æ‚è‚Q‚Q–Ú‚©‚çSkŒã—Ã—¿‚ðŽg‚¤
024660     MOVE SPACE TO Skƒtƒ‰ƒO.
024670*
024680     MOVE •‰|•‰˜a—ï(•”ˆÊ‚b‚m‚s) TO ŒvŽZ˜a—ï‚v.
024690     MOVE •‰|•‰”N(•”ˆÊ‚b‚m‚s)   TO ŒvŽZ”N‚v.
024700     MOVE •‰|•‰ŒŽ(•”ˆÊ‚b‚m‚s)   TO ŒvŽZŒŽ‚v.
024710     MOVE •‰|•‰“ú(•”ˆÊ‚b‚m‚s)   TO ŒvŽZ“ú‚v.
024720     PERFORM ¼—ï”NŽæ“¾.
024730     PERFORM “ú•tâ‘Î’lŒvŽZ.
024740     MOVE â‘Î’l‚v                 TO •‰â‘Î’l‚v.
024750*
024760     MOVE ˜AŒv|Ž{p˜a—ï           TO ŒvŽZ˜a—ï‚v.
024770     MOVE ˜AŒv|Ž{p”N             TO ŒvŽZ”N‚v.
024780     MOVE ˜AŒv|Ž{pŒŽ             TO ŒvŽZŒŽ‚v.
024790     MOVE ˜AŒv|Ž{p“ú             TO ŒvŽZ“ú‚v.
024800     PERFORM ¼—ï”NŽæ“¾.
024810     PERFORM “ú•tâ‘Î’lŒvŽZ.
024820     MOVE â‘Î’l‚v                 TO Ž{pâ‘Î’l‚v.
024830*
024840     COMPUTE •‰Œo‰ß“ú‚v = Ž{pâ‘Î’l‚v - ( •‰â‘Î’l‚v - 1 ).
024850*/ŠJŽn“ú‚©‚ç‚Rƒ–ŒŽˆÈ“à‚©‚Sƒ–ŒŽˆÈ~‚©”»’è
024860     PERFORM ŠJŽnI—¹“úŽæ“¾
024870     PERFORM ”NŒŽâ‘Î’lŒvŽZ
024880*/•‰•ÊŽ©”…Ó—¿‹à—p‚Ì‹æ•ªƒR[ƒhiŽ©”…Ó06j
024890     MOVE 06               TO —¿|‹æ•ªƒR[ƒh.
024900     MOVE •‰Ží•Ê‚v       TO —¿|•‰Ží•Ê.
024910     MOVE •”ˆÊ‚v           TO —¿|•”ˆÊ.
024920     MOVE ¶‰E‹æ•ª‚v       TO —¿|¶‰E‹æ•ª.
024930     MOVE •‰ˆÊ’u”Ô†‚v   TO —¿|•‰ˆÊ’u”Ô†.
024940*/V—¿‹à“™‚Ì•ÏX‚Í’ñoŽž‚©‚ç—LŒø‚Ìˆ×AŠe—¿‹à‚Í¿‹”NŒŽ‚Å”»’è0101
024950*     MOVE ˜AŒv|Ž{p˜a—ï   TO —¿|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
024960*     MOVE ˜AŒv|Ž{p”N     TO —¿|ŠJŽn”N   Ž{p”N‚b‚v.
024970*     MOVE ˜AŒv|Ž{pŒŽ     TO —¿|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
024980     MOVE ¿‹˜a—ï‚v   TO —¿|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
024990     MOVE ¿‹”N‚v     TO —¿|ŠJŽn”N   Ž{p”N‚b‚v.
025000     MOVE ¿‹ŒŽ‚v     TO —¿|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
025010*
025020     START —¿‹àƒ}ƒXƒ^ KEY IS <= —¿|‹æ•ªƒR[ƒh
025030                                —¿|•”ˆÊƒR[ƒh
025040                                —¿|ŠJŽn˜a—ï”NŒŽ
025050                                REVERSED
025060     END-START.
025070*
025080     IF ó‘ÔƒL[ = "00"
025090         READ —¿‹àƒ}ƒXƒ^ NEXT
025100         AT END
025110*/ƒGƒ‰[•\Ž¦‚ÌC³
025120             PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
025130             DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
025140                     " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
025150                     " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
025160             PERFORM I—¹ˆ—
025170             MOVE ZERO TO PROGRAM-STATUS
025180             EXIT PROGRAM
025190         NOT AT END
025200*/•‰•ÊŽ©”…Ó—¿‹à—p‚ÌƒŒƒR[ƒh‚©”»’è
025210             IF ( —¿|‹æ•ªƒR[ƒh    NOT = 06             ) OR
025220                ( —¿|•‰Ží•Ê      NOT = •‰Ží•Ê‚v     ) OR
025230                ( —¿|•”ˆÊ          NOT = •”ˆÊ‚v         ) OR
025240                ( —¿|¶‰E‹æ•ª      NOT = ¶‰E‹æ•ª‚v     ) OR
025250                ( —¿|•‰ˆÊ’u”Ô†  NOT = •‰ˆÊ’u”Ô†‚v )   
025260*
025270*/ƒGƒ‰[•\Ž¦‚ÌC³
025280                  PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
025290                  DISPLAY "Ž©”…Ó—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
025300                          " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
025310                          " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ
025320                          " •”ˆÊƒR[ƒh=" •”ˆÊƒR[ƒh‚v UPON CONS
025330                 PERFORM I—¹ˆ—
025340                 MOVE ZERO TO PROGRAM-STATUS
025350                 EXIT PROGRAM
025360             ELSE
025370                 IF ( Ž{p˜a—ï”NŒŽ‚b‚v >= —¿‚e|ŠJŽn˜a—ï”NŒŽ ) AND
025380                    ( Ž{p˜a—ï”NŒŽ‚b‚v <= —¿‚e|I—¹˜a—ï”NŒŽ )
025390                     MOVE —¿‚e|®•œ—¿     TO ®•œ—¿‚e‚v
025400                     MOVE —¿‚e|“Á•ÊÞ—¿”ï TO “Á•ÊÞ—¿”ï‚e‚v
025410                     MOVE —¿‚e|ŒðŠ·•ï‘Ñ—¿ TO ŒðŠ·•ï‘Ñ—¿‚e‚v
025420*/Œã—Ã—¿
025430*/‚Q•”ˆÊˆÈ“à‚Å‚Rƒ–ŒŽˆÈ“à‚Ìê‡‚ÍuŒã—Ã—¿‚Q‚Rv‚ðŽg‚¤
025440*/    œÜ‚Ìê‡‚Å‚Q‚Q“ú‚©‚ç‚ÍuSkŒã—Ã—¿‚Q‚Ov‚ðŽg‚¤
025450*/‚Q•”ˆÊˆÈ“à‚Å‚Sƒ–ŒŽ–Ú‚©‚ç‚ÍuŒã—¿—¿‚Q‚Sv‚ðŽg‚¤
025460*/    œÜ‚Ìê‡‚Å‚Q‚Q“ú‚©‚ç‚ÍuSkŒã—Ã—¿‚Q‚Ov‚ðŽg‚¤
025470*/‚R•”ˆÊ–Ú‚©‚ç‚ÍuŒã—Ã—¿‚R‚Ov‚ðŽg‚¤
025480*/    œÜ‚Ìê‡‚Å‚Q‚Q“ú‚©‚ç‚ÍuSkŒã—Ã—¿‚R‚Ov‚ðŽg‚¤
025490                     EVALUATE TRUE
025500*/’üŒ¸•”ˆÊ–¢–žA’üŒ¸ŒŽ–¢–žA
025510                     WHEN (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
025520                          (Ž{p”NŒŽ·‚v < ’·Šú’üŒ¸ŒŽ”‚j‚v    )
025530                          IF ( •‰Œo‰ß“ú‚v >= 22 )       AND
025540                             ( —¿|•‰Ží•Ê  = 05 OR 06 ) AND
025550*/˜JÐSkŒã—Ã‹æ•ª/090317
025560                             ( •‰|SkŒã—Ã‹æ•ª = ZERO )
025570                               MOVE —¿‚e|SkŒã—Ã—¿‚Q‚O TO Œã—Ã—¿‚e‚v
025580                               MOVE "YES"                TO Skƒtƒ‰ƒO
025590                          ELSE
025600                               MOVE —¿‚e|Œã—Ã—¿‚Q‚R     TO Œã—Ã—¿‚e‚v
025610                               MOVE SPACE                TO Skƒtƒ‰ƒO
025620                          END-IF
025630*/’üŒ¸•”ˆÊ–¢–žA’üŒ¸ŒŽˆÈã
025640                     WHEN (•”ˆÊƒJƒEƒ“ƒ^ <  ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
025650                          (Ž{p”NŒŽ·‚v >= ’·Šú’üŒ¸ŒŽ”‚j‚v    )
025660                          IF ( •‰Œo‰ß“ú‚v >= 22 )       AND
025670                             ( —¿|•‰Ží•Ê  = 05 OR 06 ) AND
025680*/˜JÐSkŒã—Ã‹æ•ª/090317
025690                             ( •‰|SkŒã—Ã‹æ•ª = ZERO )
025700                               MOVE —¿‚e|SkŒã—Ã—¿‚Q‚O TO Œã—Ã—¿‚e‚v
025710                               MOVE "YES"                TO Skƒtƒ‰ƒO
025720                          ELSE
025730                               MOVE —¿‚e|Œã—Ã—¿‚Q‚S     TO Œã—Ã—¿‚e‚v
025740                               MOVE SPACE                TO Skƒtƒ‰ƒO
025750                          END-IF
025760*/’üŒ¸•”ˆÊˆÈã
025770                     WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
025780                          IF ( •‰Œo‰ß“ú‚v >= 22 )       AND
025790                             ( —¿|•‰Ží•Ê  = 05 OR 06 ) AND
025800*/˜JÐSkŒã—Ã‹æ•ª/090317
025810                             ( •‰|SkŒã—Ã‹æ•ª = ZERO )
025820                               MOVE —¿‚e|SkŒã—Ã—¿‚R‚O TO Œã—Ã—¿‚e‚v
025830                               MOVE "YES"                TO Skƒtƒ‰ƒO
025840                          ELSE
025850                               MOVE —¿‚e|Œã—Ã—¿‚R‚O     TO Œã—Ã—¿‚e‚v
025860                               MOVE SPACE                TO Skƒtƒ‰ƒO
025870                          END-IF
025880                     END-EVALUATE
025890                 ELSE
025900*/ƒGƒ‰[•\Ž¦‚ÌC³
025910                     PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
025920                     DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
025930                             " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
025940                             " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
025950                     PERFORM I—¹ˆ—
025960                     MOVE ZERO TO PROGRAM-STATUS
025970                     EXIT PROGRAM
025980                 END-IF
025990             END-IF
026000         END-READ
026010     END-IF.
026020*
           IF Ž©”…|ŒÅ—L’P‰¿Žg—p‹æ•ª = 1
               MOVE Ž©”…|®•œ—¿    (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ®•œ—¿‚e‚v
               MOVE Ž©”…|“Á•ÊÞ—¿”ï(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “Á•ÊÞ—¿”ï‚e‚v
               MOVE Ž©”…|ŒðŠ·•ï‘Ñ—¿(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ŒðŠ·•ï‘Ñ—¿‚e‚v
025490         EVALUATE TRUE
025500*/’üŒ¸•”ˆÊ–¢–žA’üŒ¸ŒŽ–¢–žA
025510         WHEN (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
025520              (Ž{p”NŒŽ·‚v < ’·Šú’üŒ¸ŒŽ”‚j‚v    )
025530              IF ( •‰Œo‰ß“ú‚v >= 22 )       AND
025540                 ( —¿|•‰Ží•Ê  = 05 OR 06 ) AND
025550*/˜JÐSkŒã—Ã‹æ•ª
025560                 ( •‰|SkŒã—Ã‹æ•ª = ZERO )
                         MOVE Ž©”…|SkŒã—Ã—¿‚Q‚O(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚e‚v
025590              ELSE
                         MOVE Ž©”…|Œã—Ã—¿‚Q‚R    (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚e‚v
025620              END-IF
025630*/’üŒ¸•”ˆÊ–¢–žA’üŒ¸ŒŽˆÈã
025640         WHEN (•”ˆÊƒJƒEƒ“ƒ^ <  ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
025650              (Ž{p”NŒŽ·‚v >= ’·Šú’üŒ¸ŒŽ”‚j‚v    )
025660              IF ( •‰Œo‰ß“ú‚v >= 22 )       AND
025670                 ( —¿|•‰Ží•Ê  = 05 OR 06 ) AND
025680*/˜JÐSkŒã—Ã‹æ•ª
025690                 ( •‰|SkŒã—Ã‹æ•ª = ZERO )
                         MOVE Ž©”…|SkŒã—Ã—¿‚Q‚O(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚e‚v
025720              ELSE
                         MOVE Ž©”…|Œã—Ã—¿‚Q‚S    (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚e‚v
025750              END-IF
025760*/’üŒ¸•”ˆÊˆÈã
025770         WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
025780              IF ( •‰Œo‰ß“ú‚v >= 22 )       AND
025790                 ( —¿|•‰Ží•Ê  = 05 OR 06 ) AND
025800*/˜JÐSkŒã—Ã‹æ•ª
025810                 ( •‰|SkŒã—Ã‹æ•ª = ZERO )
                         MOVE Ž©”…|SkŒã—Ã—¿‚R‚O(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚e‚v
025840              ELSE
                         MOVE Ž©”…|Œã—Ã—¿‚R‚O    (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚e‚v
025870              END-IF
025880         END-EVALUATE
           END-IF.
026030*================================================================*
026040 ”PÁ“™Œã—Ã—¿Žæ“¾ SECTION.
026050*
026060     MOVE SPACE TO Skƒtƒ‰ƒO.
026070*/ŠJŽn“ú‚©‚ç‚Rƒ–ŒŽˆÈ“à‚©‚Sƒ–ŒŽˆÈ~‚©”»’è
026080     PERFORM ŠJŽnI—¹“úŽæ“¾
026090     PERFORM ”NŒŽâ‘Î’lŒvŽZ
026100*/•‰•ÊŽ©”…Ó—¿‹à—p‚Ì‹æ•ªƒR[ƒhiŽ©”…Ó06j
026110     MOVE 06               TO —¿|‹æ•ªƒR[ƒh.
026120     MOVE •‰Ží•Ê‚v       TO —¿|•‰Ží•Ê.
026130     MOVE ZERO             TO —¿|•”ˆÊ.
026140     MOVE ZERO             TO —¿|¶‰E‹æ•ª.
026150     MOVE ZERO             TO —¿|•‰ˆÊ’u”Ô†.
026160*/V—¿‹à“™‚Ì•ÏX‚Í’ñoŽž‚©‚ç—LŒø‚Ìˆ×AŠe—¿‹à‚Í¿‹”NŒŽ‚Å”»’è0101
026170*     MOVE ˜AŒv|Ž{p˜a—ï   TO —¿|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
026180*     MOVE ˜AŒv|Ž{p”N     TO —¿|ŠJŽn”N   Ž{p”N‚b‚v.
026190*     MOVE ˜AŒv|Ž{pŒŽ     TO —¿|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
026200     MOVE ¿‹˜a—ï‚v   TO —¿|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
026210     MOVE ¿‹”N‚v     TO —¿|ŠJŽn”N   Ž{p”N‚b‚v.
026220     MOVE ¿‹ŒŽ‚v     TO —¿|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
026230*
026240     START —¿‹àƒ}ƒXƒ^ KEY IS <= —¿|‹æ•ªƒR[ƒh
026250                                —¿|•”ˆÊƒR[ƒh
026260                                —¿|ŠJŽn˜a—ï”NŒŽ
026270                                REVERSED
026280     END-START.
026290*
026300     IF ó‘ÔƒL[ = "00"
026310         READ —¿‹àƒ}ƒXƒ^ NEXT
026320         AT END
026330*/ƒGƒ‰[•\Ž¦‚ÌC³
026340             PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
026350             DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
026360                     " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
026370                     " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
026380             PERFORM I—¹ˆ—
026390             MOVE ZERO TO PROGRAM-STATUS
026400             EXIT PROGRAM
026410         NOT AT END
026420             IF ( —¿|‹æ•ªƒR[ƒh NOT = 06         ) OR
026430                ( —¿|•‰Ží•Ê   NOT = •‰Ží•Ê‚v )   
026440*/ƒGƒ‰[•\Ž¦‚ÌC³
026450                  PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
026460                  DISPLAY "Ž©”…Ó—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
026470                          " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
026480                          " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ
026490                          " •”ˆÊƒR[ƒh=" •”ˆÊƒR[ƒh‚v UPON CONS
026500                 PERFORM I—¹ˆ—
026510                 MOVE ZERO TO PROGRAM-STATUS
026520                 EXIT PROGRAM
026530             ELSE
026540                 IF ( Ž{p˜a—ï”NŒŽ‚b‚v >= —¿‚c|ŠJŽn˜a—ï”NŒŽ ) AND
026550                    ( Ž{p˜a—ï”NŒŽ‚b‚v <= —¿‚c|I—¹˜a—ï”NŒŽ )
026560                     MOVE —¿‚e|®•œ—¿     TO ®•œ—¿‚e‚v
026570                     MOVE —¿‚e|“Á•ÊÞ—¿”ï TO “Á•ÊÞ—¿”ï‚e‚v
026580                     MOVE —¿‚e|ŒðŠ·•ï‘Ñ—¿ TO ŒðŠ·•ï‘Ñ—¿‚e‚v
026590                     EVALUATE TRUE
026600*/’üŒ¸•”ˆÊ–¢–žA’üŒ¸ŒŽ–¢–žA
026610                     WHEN (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
026620                          (Ž{p”NŒŽ·‚v < ’·Šú’üŒ¸ŒŽ”‚j‚v    )
026630                         MOVE —¿‚e|Œã—Ã—¿‚Q‚R     TO Œã—Ã—¿‚e‚v
026640*/’üŒ¸•”ˆÊ–¢–žA’üŒ¸ŒŽˆÈã
026650                     WHEN (•”ˆÊƒJƒEƒ“ƒ^ <  ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
026660                          (Ž{p”NŒŽ·‚v >= ’·Šú’üŒ¸ŒŽ”‚j‚v    )
026670                         MOVE —¿‚e|Œã—Ã—¿‚Q‚S     TO Œã—Ã—¿‚e‚v
026680*/’üŒ¸•”ˆÊˆÈã
026690                     WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
026700                         MOVE —¿‚e|Œã—Ã—¿‚R‚O     TO Œã—Ã—¿‚e‚v
026710                     END-EVALUATE
026720                 ELSE
026730*/ƒGƒ‰[•\Ž¦‚ÌC³
026740                     PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
026750                     DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
026760                             " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
026770                             " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
026780                     PERFORM I—¹ˆ—
026790                     MOVE ZERO TO PROGRAM-STATUS
026800                     EXIT PROGRAM
026810                 END-IF
026820             END-IF
026830         END-READ
026840     END-IF.
026850*
           IF Ž©”…|ŒÅ—L’P‰¿Žg—p‹æ•ª = 1
               MOVE Ž©”…|®•œ—¿    (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ®•œ—¿‚e‚v
               MOVE Ž©”…|“Á•ÊÞ—¿”ï(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “Á•ÊÞ—¿”ï‚e‚v
               MOVE Ž©”…|ŒðŠ·•ï‘Ñ—¿(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ŒðŠ·•ï‘Ñ—¿‚e‚v
026590         EVALUATE TRUE
026600*/’üŒ¸•”ˆÊ–¢–žA’üŒ¸ŒŽ–¢–žA
026610         WHEN (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
026620              (Ž{p”NŒŽ·‚v < ’·Šú’üŒ¸ŒŽ”‚j‚v    )
                   MOVE Ž©”…|Œã—Ã—¿‚Q‚R(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚e‚v
026640*/’üŒ¸•”ˆÊ–¢–žA’üŒ¸ŒŽˆÈã
026650         WHEN (•”ˆÊƒJƒEƒ“ƒ^ <  ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
026660              (Ž{p”NŒŽ·‚v >= ’·Šú’üŒ¸ŒŽ”‚j‚v    )
                   MOVE Ž©”…|Œã—Ã—¿‚Q‚S(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚e‚v
026680*/’üŒ¸•”ˆÊˆÈã
026690         WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
                   MOVE Ž©”…|Œã—Ã—¿‚R‚O(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚e‚v
           END-IF.
026860*================================================================*
026870 ŒvŽZî•ñŽæ“¾ SECTION.
026880*
026890* §Œä‹æ•ªF‚O‚R@Ž©”…Ó•ÛŒ¯
026900     MOVE 03             TO Œv|§Œä‹æ•ª.
026910*/V—¿‹à“™‚Ì•ÏX‚Í’ñoŽž‚©‚ç—LŒø‚Ìˆ×AŠe—¿‹à‚Í¿‹”NŒŽ‚Å”»’è0101
026920*     MOVE ˜AŒv|Ž{p˜a—ï TO Œv|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
026930*     MOVE ˜AŒv|Ž{p”N   TO Œv|ŠJŽn”N   Ž{p”N‚b‚v.
026940*     MOVE ˜AŒv|Ž{pŒŽ   TO Œv|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
026950     MOVE ¿‹˜a—ï‚v TO Œv|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
026960     MOVE ¿‹”N‚v   TO Œv|ŠJŽn”N   Ž{p”N‚b‚v.
026970     MOVE ¿‹ŒŽ‚v   TO Œv|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
026980*
026990     START ŒvŽZƒ}ƒXƒ^ KEY IS <= Œv|§Œä‹æ•ª Œv|ŠJŽn˜a—ï”NŒŽ
027000                                                     REVERSED
027010     END-START.
027020*
027030     IF ó‘ÔƒL[ = "00"
027040         READ ŒvŽZƒ}ƒXƒ^ NEXT
027050         AT END
027060*/ƒGƒ‰[•\Ž¦‚ÌC³
027070             PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
027080             DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
027090                     " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
027100                     " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
027110             PERFORM I—¹ˆ—
027120             MOVE ZERO TO PROGRAM-STATUS
027130             EXIT PROGRAM
027140         NOT AT END
027150             IF ( Ž{p˜a—ï”NŒŽ‚b‚v >= Œv‚b|ŠJŽn˜a—ï”NŒŽ ) AND
027160                ( Ž{p˜a—ï”NŒŽ‚b‚v <= Œv‚b|I—¹˜a—ï”NŒŽ )
027170                 MOVE Œv‚b|‰—ÃŠî–{‹——£     TO ‰—ÃŠî–{‹——£‚j‚v
027180                 MOVE Œv‚b|Œã—Ã‰Â”\ŽZ’è”   TO Œã—Ã‰Â”\ŽZ’è”‚j‚v
027190                 MOVE Œv‚b|’·Šú’üŒ¸ŒŽ”     TO ’·Šú’üŒ¸ŒŽ”‚j‚v
027200                 MOVE Œv‚b|‘½•”ˆÊ’üŒ¸•”ˆÊ” TO ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v
027210             ELSE
027220*/ƒGƒ‰[•\Ž¦‚ÌC³
027230                 PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
027240                 DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
027250                         " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
027260                         " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
027270                 PERFORM I—¹ˆ—
027280                 MOVE ZERO TO PROGRAM-STATUS
027290                 EXIT PROGRAM
027300             END-IF
027310         END-READ
027320     END-IF.
027330*
027340     PERFORM ‰ŒŸ¿‹‹æ•ªŽæ“¾.
027350*
      */ŽófŽÒŒÅ—L’P‰¿
           IF Ž©”…|ŒÅ—L’P‰¿Žg—p‹æ•ª = 1
000595         MOVE Ž©”…|’·Šú’üŒ¸ŒŽ”     TO ’·Šú’üŒ¸ŒŽ”‚j‚v
000597         MOVE Ž©”…|‘½•”ˆÊ’üŒ¸•”ˆÊ” TO ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v
           END-IF.
027360**================================================================*
027370 Œã—Ã—¿ŽZ’è SECTION.
027380****************************************************************
027390* Œã—Ã—¿E—âãª–@—¿E‰·ãª–@—¿‚ðŠÜ‚Þ
027400* Œp‘±ˆÈŠOiŽ¡–üE’†Ž~E“]ˆã“™j‚ÅŽZ’è‚ÌŒJã‚è—L‚èi‚T•”ˆÊ‚Ü‚Åj
027410* Ž¡—Ã’†‚Ì•”ˆÊ‚ÌŽ¡–ü“™‚ÌƒP[ƒXB
027420* Œã—Ã—¿Eãª–@—¿E“d—Ã—¿‚ðŠÜ‚Þ
027430*****************************************************************
027440     MOVE ZERO TO 
027450     Œã—Ã—¿‚P‚m‚q‚v  —âãª–@—¿‚P‚m‚q‚v  ‰·ãª–@—¿‚P‚m‚q‚v  “d—Ã—¿‚P‚m‚q‚v
027460     Œã—Ã—¿‚PSk‚m‚q‚v  —âãª–@—¿‚PSk‚m‚q‚v  ‰·ãª–@—¿‚PSk‚m‚q‚v  “d—Ã—¿‚PSk‚m‚q‚v
027470     ‘½•”ˆÊŒã—Ã—¿‚P‚m‚q‚v  ’·ŠúŒã—Ã—¿‚P‚m‚q‚v  ‘½•”ˆÊSkŒã—Ã—¿‚P‚m‚q‚v
027480     Œã—Ã—¿‚Q‚m‚q‚v  —âãª–@—¿‚Q‚m‚q‚v  ‰·ãª–@—¿‚Q‚m‚q‚v  “d—Ã—¿‚Q‚m‚q‚v
027490     Œã—Ã—¿‚QSk‚m‚q‚v  —âãª–@—¿‚QSk‚m‚q‚v  ‰·ãª–@—¿‚QSk‚m‚q‚v  “d—Ã—¿‚QSk‚m‚q‚v
027500     ‘½•”ˆÊŒã—Ã—¿‚Q‚m‚q‚v  ’·ŠúŒã—Ã—¿‚Q‚m‚q‚v  ‘½•”ˆÊSkŒã—Ã—¿‚Q‚m‚q‚v
027510     Œã—Ã—¿‚R‚m‚q‚v —âãª–@—¿‚R‚m‚q‚v  ‰·ãª–@—¿‚R‚m‚q‚v  “d—Ã—¿‚R‚m‚q‚v
027520     Œã—Ã—¿‚RSk‚m‚q‚v  —âãª–@—¿‚RSk‚m‚q‚v  ‰·ãª–@—¿‚RSk‚m‚q‚v  “d—Ã—¿‚RSk‚m‚q‚v
027530     ‘½•”ˆÊŒã—Ã—¿‚R‚m‚q‚v  ’·ŠúŒã—Ã—¿‚R‚m‚q‚v  ‘½•”ˆÊSkŒã—Ã—¿‚R‚m‚q‚v
027540     Œã—Ã—¿‚S‚m‚q‚v  —âãª–@—¿‚S‚m‚q‚v  ‰·ãª–@—¿‚S‚m‚q‚v  “d—Ã—¿‚S‚m‚q‚v
027550     Œã—Ã—¿‚SSk‚m‚q‚v  —âãª–@—¿‚SSk‚m‚q‚v  ‰·ãª–@—¿‚SSk‚m‚q‚v  “d—Ã—¿‚SSk‚m‚q‚v
027560     ‘½•”ˆÊŒã—Ã—¿‚S‚m‚q‚v  ’·ŠúŒã—Ã—¿‚S‚m‚q‚v  ‘½•”ˆÊSkŒã—Ã—¿‚S‚m‚q‚v
027570     Œã—Ã—¿‚T‚m‚q‚v  —âãª–@—¿‚T‚m‚q‚v  ‰·ãª–@—¿‚T‚m‚q‚v  “d—Ã—¿‚T‚m‚q‚v
027580     Œã—Ã—¿‚TSk‚m‚q‚v  —âãª–@—¿‚TSk‚m‚q‚v  ‰·ãª–@—¿‚TSk‚m‚q‚v  “d—Ã—¿‚TSk‚m‚q‚v
027590     ‘½•”ˆÊŒã—Ã—¿‚T‚m‚q‚v  ’·ŠúŒã—Ã—¿‚T‚m‚q‚v  ‘½•”ˆÊSkŒã—Ã—¿‚T‚m‚q‚v
027600     Œã—Ã—¿‚U‚m‚q‚v  —âãª–@—¿‚U‚m‚q‚v  ‰·ãª–@—¿‚U‚m‚q‚v  “d—Ã—¿‚U‚m‚q‚v
027610     Œã—Ã—¿‚USk‚m‚q‚v  —âãª–@—¿‚USk‚m‚q‚v  ‰·ãª–@—¿‚USk‚m‚q‚v  “d—Ã—¿‚USk‚m‚q‚v
027620     ‘½•”ˆÊŒã—Ã—¿‚U‚m‚q‚v  ’·ŠúŒã—Ã—¿‚U‚m‚q‚v  ‘½•”ˆÊSkŒã—Ã—¿‚U‚m‚q‚v
027630     Œã—Ã—¿‚V‚m‚q‚v  —âãª–@—¿‚V‚m‚q‚v  ‰·ãª–@—¿‚V‚m‚q‚v  “d—Ã—¿‚V‚m‚q‚v
027640     Œã—Ã—¿‚VSk‚m‚q‚v  —âãª–@—¿‚VSk‚m‚q‚v  ‰·ãª–@—¿‚VSk‚m‚q‚v  “d—Ã—¿‚VSk‚m‚q‚v
027650     ‘½•”ˆÊŒã—Ã—¿‚V‚m‚q‚v  ’·ŠúŒã—Ã—¿‚V‚m‚q‚v  ‘½•”ˆÊSkŒã—Ã—¿‚V‚m‚q‚v
027660     Œã—Ã—¿‚W‚m‚q‚v  —âãª–@—¿‚W‚m‚q‚v  ‰·ãª–@—¿‚W‚m‚q‚v  “d—Ã—¿‚W‚m‚q‚v
027670     Œã—Ã—¿‚WSk‚m‚q‚v  —âãª–@—¿‚WSk‚m‚q‚v  ‰·ãª–@—¿‚WSk‚m‚q‚v  “d—Ã—¿‚WSk‚m‚q‚v
027680     ‘½•”ˆÊŒã—Ã—¿‚W‚m‚q‚v  ’·ŠúŒã—Ã—¿‚W‚m‚q‚v  ‘½•”ˆÊSkŒã—Ã—¿‚W‚m‚q‚v
027690     Œã—Ã—¿‚X‚m‚q‚v  —âãª–@—¿‚X‚m‚q‚v  ‰·ãª–@—¿‚X‚m‚q‚v  “d—Ã—¿‚X‚m‚q‚v
027700     Œã—Ã—¿‚XSk‚m‚q‚v  —âãª–@—¿‚XSk‚m‚q‚v  ‰·ãª–@—¿‚XSk‚m‚q‚v  “d—Ã—¿‚XSk‚m‚q‚v
027710     ‘½•”ˆÊŒã—Ã—¿‚X‚m‚q‚v  ’·ŠúŒã—Ã—¿‚X‚m‚q‚v  ‘½•”ˆÊSkŒã—Ã—¿‚X‚m‚q‚v.
027720* Œp‘±•”ˆÊ”‚v‚ÌƒZƒbƒg
027730     PERFORM Œp‘±•”ˆÊ”ƒZƒbƒg.
027740*
027750     MOVE ZERO TO Œã—Ã—¿‚v —âãª–@—¿‚v ‰·ãª–@—¿‚v “d—Ã—¿‚v.
027760     MOVE 1    TO •”ˆÊƒJƒEƒ“ƒ^.
027770* Œp‘±‚Ì•”ˆÊ‚Ì‚ÝŽZ’è‚·‚é
027780     PERFORM VARYING •”ˆÊƒJƒEƒ“ƒ^‚Q FROM 1 BY 1
027790             UNTIL ( •”ˆÊƒJƒEƒ“ƒ^‚Q > •”ˆÊ”‚v )
027800* •‰|I—¹˜a—ï”NŒŽ‚ª–¢“ü—Í‚Ìê‡‚ÍAALL 9 ‚ðƒZƒbƒg‚·‚é
027810         PERFORM ŠJŽnI—¹“úŽæ“¾
027820*         IF ( ŠJŽn˜a—ï”NŒŽ“ú‚v <= Ž{p˜a—ï”NŒŽ“ú‚b‚v ) AND
027830*            ( I—¹˜a—ï”NŒŽ“ú‚v >= Ž{p˜a—ï”NŒŽ“ú‚b‚v )
027840*            ( “]‹A‹æ•ª‚v = 9 )
027850*/‚b‚v‚Í‘¼‚ÅŽg‚¤ˆ×•Ê‚Ì’l‚ª“ü‚Á‚Ä‚¢‚é‚Ì‚ÅŽ{p‹L˜^‚Ì’l‚Å”»’è‚·‚éB
027860         IF ( ŠJŽn˜a—ï”NŒŽ“ú‚v <= Ž{‹L|Ž{p˜a—ï”NŒŽ“ú) AND
027870            ( I—¹˜a—ï”NŒŽ“ú‚v >= Ž{‹L|Ž{p˜a—ï”NŒŽ“ú)
027880             MOVE Ž{‹L|•‰Ží•Ê    (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO •‰Ží•Ê‚v
027890             MOVE Ž{‹L|•”ˆÊ        (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO •”ˆÊ‚v
027900             MOVE Ž{‹L|¶‰E‹æ•ª    (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ¶‰E‹æ•ª‚v
027910             MOVE Ž{‹L|•‰ˆÊ’u”Ô†(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO •‰ˆÊ’u”Ô†‚v
027920             IF •‰Ží•Ê‚v = 01 OR 02 OR 03
027930                 PERFORM ”PÁ“™Œã—Ã—¿Žæ“¾
027940             ELSE
027950                 PERFORM •‰•Ê—¿‹àŽæ“¾
027960             END-IF
027970* Œã—Ã’P‰¿‚Ì‘Þ”ð
027980*             IF •‰Ží•Ê‚v = 07 OR 08
027990*                 MOVE SkŒã—Ã—¿‚c‚v TO Œã—Ã’P‰¿‚n‚v( •”ˆÊƒJƒEƒ“ƒ^‚Q)
028000*             ELSE
028010*                 MOVE Œã—Ã—¿‚c‚v     TO Œã—Ã’P‰¿‚n‚v( •”ˆÊƒJƒEƒ“ƒ^‚Q)
028020*             END-IF
028030* Œã—Ã‚ÌŽž‚Ì‚ÝŽZ’èi‰‰ñˆ’u‚ðs‚í‚È‚¢‰ŒŸ“ú‚à‚r‚d‚j‚h‚q‚d‚b‚É‚Äl—¶Ïj
028040             IF ( Ž{‹L|Œã—Ã—¿¿‹‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q)   = 1 ) AND 
028050                ( Ž{‹L|®•œŽ{—Ã‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q) NOT = 1 )
028060                 IF ( Ž{‹L|ˆãŽtSk‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q) = 1 ) AND
028070                    ( •‰Ží•Ê‚v = 07 OR 08                  )
028080                     MOVE SkŒã—Ã—¿‚e‚v TO Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028090*                     PERFORM Œã—Ã‘½•”ˆÊ€–ÚŽæ“¾
028100                     PERFORM Œã—Ã‘½•”ˆÊ—ÝŒv
028110                 ELSE
028120*/œÜA•s‘SœÜ‚Ìê‡•‰“ú‚æ‚è‚Q‚Q“ú–Ú‚©‚çSkŒã—Ã—¿‚ðŽg‚¤
028130                     MOVE Œã—Ã—¿‚e‚v TO Œã—Ã—¿‚P‚O‚O‚v( •”ˆÊƒJƒEƒ“ƒ^‚Q)
028140*                     IF Skƒtƒ‰ƒO = SPACE
028150*                     IF (Skƒtƒ‰ƒO = SPACE) AND
028160*                        (’·Šúƒtƒ‰ƒO = SPACE) AND
028170*                        (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
028180*                         MOVE Œã—Ã—¿‚e‚v TO Œã—Ã’P‰¿‚n‚v    (•”ˆÊƒJƒEƒ“ƒ^‚Q)
028190*                     ELSE
028200*                         MOVE Œã—Ã—¿‚e‚v TO SkŒã—Ã’P‰¿‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028210*                     END-IF
028220                     EVALUATE TRUE
028230*                   */’Êí
028240                     WHEN (Skƒtƒ‰ƒO = SPACE) AND
028250                          (’·Šúƒtƒ‰ƒO = SPACE) AND
028260                          (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
028270                         MOVE Œã—Ã—¿‚e‚v TO Œã—Ã’P‰¿‚n‚v    (•”ˆÊƒJƒEƒ“ƒ^‚Q)
028280*                   */Sk
028290                     WHEN (Skƒtƒ‰ƒO = "YES" ) AND
028300                          (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
028310                         MOVE Œã—Ã—¿‚e‚v TO SkŒã—Ã’P‰¿‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028320*                   */‘½•”ˆÊ
028330                     WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
028340                          (Skƒtƒ‰ƒO    = SPACE )
028350                         MOVE Œã—Ã—¿‚e‚v TO  ‘½•”ˆÊŒã—Ã’P‰¿‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028360*                   */‘½•”ˆÊ‚ÌSk
028370                     WHEN (Skƒtƒ‰ƒO = "YES" ) AND
028380                          (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
028390                         MOVE Œã—Ã—¿‚e‚v TO  ‘½•”ˆÊSkŒã—Ã’P‰¿‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028400*                   */’·Šú
028410                     WHEN (’·Šúƒtƒ‰ƒO = "YES" )
028420                         MOVE Œã—Ã—¿‚e‚v TO  ’·ŠúŒã—Ã’P‰¿‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028430*                   */‚È‚µ
028440                     WHEN OTHER
028450                         CONTINUE
028460                     END-EVALUATE
028470*                     PERFORM Œã—Ã‘½•”ˆÊ€–ÚŽæ“¾
028480                     PERFORM Œã—Ã‘½•”ˆÊ—ÝŒv
028490                 END-IF
028500             ELSE IF ( Ž{‹L|Œã—Ã—¿¿‹‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q)   = 1 ) AND 
028510                     ( Ž{‹L|®•œŽ{—Ã‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q)     = 1 )
028520*/ƒGƒ‰[•\Ž¦‚ÌC³
028530                      PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
028540                      DISPLAY "®•œ—¿EŒã—Ã—¿‚Í“¯Žž‚É¿‹‚Å‚«‚Ü‚¹‚ñ"
028550                              " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
028560                              " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
028570                  END-IF
028580             END-IF
028590*
028600*/061024
028610             MOVE ZERO TO ãª–@’P‰¿‚v
028620             EVALUATE Ž{‹L|ãª–@‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q)
028630             WHEN 1
028640*/—âãª–@—¿
028650*/—âãª–@‚Í‘¼•”ˆÊ’üŒ¸‚Ì‚Ý
028660                 IF •”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v
028670                     MOVE —âãª–@—¿‚Q‚R‚d‚v TO —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028680                 ELSE
028690                     MOVE —âãª–@—¿‚R‚O‚d‚v TO —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028700                 END-IF
028710                  IF (’·Šúƒtƒ‰ƒO = SPACE) AND
028720*                     (Skƒtƒ‰ƒO = SPACE) AND
028730                     (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
028740                      MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@’P‰¿‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028750*/061024
028760                                                               ãª–@’P‰¿‚v
028770                  ELSE
028780                      MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@’P‰¿‚v’üŒ¸(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028790*/061024
028800                                                               ãª–@’P‰¿‚v
028810                  END-IF
028820*                 MOVE —âãª–@—¿‚d‚v TO —âãª–@—¿‚P‚O‚O‚v( •”ˆÊƒJƒEƒ“ƒ^‚Q)
028830*                 PERFORM —âãª–@‘½•”ˆÊ€–ÚŽæ“¾
028840                 PERFORM —âãª–@‘½•”ˆÊ—ÝŒv
028850*
028860             WHEN 2
028870*/‰·ãª–@—¿
028880*/‰·ãª–@‚Í‘½•”ˆÊA’·Šú’üŒ¸‚ ‚è
028890                 EVALUATE TRUE
028900*/’üŒ¸•”ˆÊ–¢–žA’üŒ¸ŒŽ–¢–žA
028910                 WHEN (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
028920                      (Ž{p”NŒŽ·‚v < ’·Šú’üŒ¸ŒŽ”‚j‚v    )
028930                      MOVE ‰·ãª–@—¿‚Q‚R‚d‚v TO ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028940*/’üŒ¸•”ˆÊ–¢–žA’üŒ¸ŒŽˆÈã
028950                  WHEN (•”ˆÊƒJƒEƒ“ƒ^ <  ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
028960                       (Ž{p”NŒŽ·‚v >= ’·Šú’üŒ¸ŒŽ”‚j‚v    )
028970                      IF (•‰Ží•Ê‚v = 05 OR 06)
028980                          MOVE ‰·ãª–@—¿‚Q‚SœÜ‚d‚v TO ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028990                      ELSE
029000                          MOVE ‰·ãª–@—¿‚Q‚S‘¼‚d‚v   TO ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029010                      END-IF
029020*/’üŒ¸•”ˆÊˆÈã
029030                  WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
029040                      MOVE ‰·ãª–@—¿‚R‚O‚d‚v TO ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029050                  END-EVALUATE
029060                  IF (’·Šúƒtƒ‰ƒO = SPACE) AND
029070*                     (Skƒtƒ‰ƒO = SPACE) AND
029080                     (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
029090                      MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@’P‰¿‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029100*/061024
029110                                                               ãª–@’P‰¿‚v
029120                  ELSE
029130                      MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@’P‰¿‚v’üŒ¸(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029140*/061024
029150                                                               ãª–@’P‰¿‚v
029160                  END-IF
029170*                 MOVE ‰·ãª–@—¿‚d‚v TO ‰·ãª–@—¿‚P‚O‚O‚v( •”ˆÊƒJƒEƒ“ƒ^‚Q)
029180*                 PERFORM ‰·ãª–@‘½•”ˆÊ€–ÚŽæ“¾
029190                 PERFORM ‰·ãª–@‘½•”ˆÊ—ÝŒv
029200             WHEN OTHER
029210                 CONTINUE
029220             END-EVALUATE
029230*/—âãª‰·ãª’P‰¿*/061024
029240             PERFORM ãª–@’P‰¿‰ñ”ƒZƒbƒg
029250*
029260             IF Ž{‹L|“d—Ã‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q) = 1
029270*/“d—Ã—¿
029280*/“d—Ã—¿‚Í‘½•”ˆÊA’·Šú’üŒ¸‚ ‚è
029290                 EVALUATE TRUE
029300*/’üŒ¸•”ˆÊ–¢–žA’üŒ¸ŒŽ–¢–žA
029310                 WHEN (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
029320                      (Ž{p”NŒŽ·‚v < ’·Šú’üŒ¸ŒŽ”‚j‚v    )
029330                      MOVE “d—Ã—¿‚Q‚R‚d‚v TO “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029340*/’üŒ¸•”ˆÊ–¢–žA’üŒ¸ŒŽˆÈã
029350                  WHEN (•”ˆÊƒJƒEƒ“ƒ^ <  ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
029360                       (Ž{p”NŒŽ·‚v >= ’·Šú’üŒ¸ŒŽ”‚j‚v    )
029370                      IF (•‰Ží•Ê‚v = 05 OR 06)
029380                          MOVE “d—Ã—¿‚Q‚SœÜ‚d‚v TO “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029390                      ELSE
029400                          MOVE “d—Ã—¿‚Q‚S‘¼‚d‚v   TO “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029410                      END-IF
029420*/’üŒ¸•”ˆÊˆÈã
029430                  WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
029440                      MOVE “d—Ã—¿‚R‚O‚d‚v TO “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029450                  END-EVALUATE
029460*/061026
029470                  MOVE ZERO TO “d—Ã’P‰¿‚v‚Q
029480                  IF(’·Šúƒtƒ‰ƒO = SPACE) AND
029490*                    (Skƒtƒ‰ƒO = SPACE) AND
029500                    (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
029510                      MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã’P‰¿‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029520*/061026
029530                                                             “d—Ã’P‰¿‚v‚Q
029540                  ELSE
029550                      MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã’P‰¿‚v’üŒ¸(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029560*/061026
029570                                                             “d—Ã’P‰¿‚v‚Q
029580                  END-IF
029590*                 MOVE “d—Ã—¿‚d‚v TO “d—Ã—¿‚P‚O‚O‚v( •”ˆÊƒJƒEƒ“ƒ^‚Q)
029600*                 PERFORM “d—Ã‘½•”ˆÊ€–ÚŽæ“¾
029610                 PERFORM “d—Ã‘½•”ˆÊ—ÝŒv
029620*/“d—Ã’P‰¿‰ñ”•s‹ï‡‚ÌC³*/061026
029630                 PERFORM “d—Ã’P‰¿‰ñ”ƒZƒbƒg
029640             END-IF
029650*
029660             COMPUTE •”ˆÊƒJƒEƒ“ƒ^ = •”ˆÊƒJƒEƒ“ƒ^ + 1
029670*
029680         END-IF
029690     END-PERFORM.
029700*
029710*================================================================*
029720 ‚»‚Ì‘¼ŽZ’è SECTION.
029730****************************************************************
029740* •›ŽqE‰‰ñˆ’uEî•ñ’ñ‹Ÿ—¿
029750* ‰‰ñˆ’u‚É‚Â‚¢‚Ä‚ÍA‚U•”ˆÊ–ÚˆÈ~‚àŽZ’è‚ ‚èB
029760****************************************************************
029770     MOVE ZERO TO ‹à‘®•›Žq—¿‚v.
029780     MOVE ZERO TO ‰‰ñˆ’u—¿‚v.
029790     MOVE ZERO TO Ž{pî•ñ’ñ‹Ÿ—¿‚v.
029800     MOVE ZERO TO “Á•ÊÞ—¿”ï‚v.
029810     MOVE ZERO TO ŒðŠ·•ï‘Ñ—¿‚v.
029820*     MOVE ZERO TO Žw“±ŠÇ——¿‚v.
           MOVE ZERO TO ‰‰ñˆ’u‚m‚n‚v.
           MOVE ZERO TO ŒðŠ·•ï‘Ñ—¿‚P‚m‚v.
           MOVE ZERO TO “Á•ÊÞ—¿”ï‚m‚v.
029830*
029840* Œp‘±‚Ì•”ˆÊ‚Ì‚ÝŽZ’è‚·‚é
029850* Œp‘±‚Ì•”ˆÊ‚Ì‚Ý•”ˆÊƒJƒEƒ“ƒ^‚ðƒJƒEƒ“ƒg‚t‚o‚·‚é
029860* ‰‰ñˆ’u‚Ì‚ÝŒã—Ã‰Â”\ŽZ’è”ˆÈã‚ÉŽZ’è‰Â”\
029870     MOVE 1 TO •”ˆÊƒJƒEƒ“ƒ^
029880     PERFORM VARYING •”ˆÊƒJƒEƒ“ƒ^‚Q FROM 1 BY 1
029890             UNTIL ( •”ˆÊƒJƒEƒ“ƒ^‚Q > •”ˆÊ”‚v ) OR
029900                   ( •”ˆÊƒJƒEƒ“ƒ^ > Œp‘±•”ˆÊ”‚v )
029910* •‰|I—¹˜a—ï”NŒŽ‚ª–¢“ü—Í‚Ìê‡‚ÍAALL 9 ‚ðƒZƒbƒg‚·‚é
029920         PERFORM ŠJŽnI—¹“úŽæ“¾
029930*
029940*         IF ( ŠJŽn˜a—ï”NŒŽ“ú‚v <= Ž{p˜a—ï”NŒŽ“ú‚b‚v ) AND
029950*            ( I—¹˜a—ï”NŒŽ“ú‚v >= Ž{p˜a—ï”NŒŽ“ú‚b‚v )
029960*            ( “]‹A‹æ•ª‚v = 9 )
029970*/‚b‚v‚Í‘¼‚ÅŽg‚¤ˆ×•Ê‚Ì’l‚ª“ü‚Á‚Ä‚¢‚é‚Ì‚ÅŽ{p‹L˜^‚Ì’l‚Å”»’è‚·‚éB
029980         IF ( ŠJŽn˜a—ï”NŒŽ“ú‚v <= Ž{‹L|Ž{p˜a—ï”NŒŽ“ú) AND
029990            ( I—¹˜a—ï”NŒŽ“ú‚v >= Ž{‹L|Ž{p˜a—ï”NŒŽ“ú)
030000*
030010             MOVE Ž{‹L|•‰Ží•Ê(•”ˆÊƒJƒEƒ“ƒ^‚Q)     TO •‰Ží•Ê‚v
030020             MOVE Ž{‹L|•”ˆÊ(•”ˆÊƒJƒEƒ“ƒ^‚Q)         TO •”ˆÊ‚v
030030             MOVE Ž{‹L|¶‰E‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q)     TO ¶‰E‹æ•ª‚v
030040             MOVE Ž{‹L|•‰ˆÊ’u”Ô†(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO •‰ˆÊ’u”Ô†‚v
030050             IF •‰Ží•Ê‚v = 01 OR 02 OR 03
030060                 PERFORM ”PÁ“™Œã—Ã—¿Žæ“¾
030070             ELSE
030080                 PERFORM •‰•Ê—¿‹àŽæ“¾
030090             END-IF
030100* ‰‰ñˆ’u—¿
030110             IF Ž{‹L|®•œŽ{—Ã‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1
030120                 COMPUTE ‰‰ñˆ’u—¿‚v = ‰‰ñˆ’u—¿‚v    + ®•œ—¿‚e‚v
030130                 COMPUTE ‰‰ñˆ’u—¿‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030140                       = ‰‰ñˆ’u—¿‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) + ®•œ—¿‚e‚v
                       MOVE ®•œ—¿‚e‚v TO ‰‰ñˆ’u—¿‚m‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030150             END-IF
030160*
030170*             IF •”ˆÊƒJƒEƒ“ƒ^ <= Œã—Ã‰Â”\ŽZ’è”‚j‚v
030180* Ž{pî•ñ’ñ‹Ÿ—¿  œÜE•s‘SœÜE’E‰P‚Ìê‡‚Ì‚Ý
030190                 IF ( Ž{‹L|î•ñ’ñ‹Ÿ‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1 ) AND
030200                    ( Ž{‹L|•‰Ží•Ê(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 04 OR 05 OR 06 ) AND
030210                    ( ŠJŽn˜a—ï”NŒŽ“ú‚v = Ž{p˜a—ï”NŒŽ“ú‚b‚v )
030220                     COMPUTE î•ñ’ñ‹Ÿ‰ñ”‚v   = î•ñ’ñ‹Ÿ‰ñ”‚v + 1
030230                     COMPUTE Ž{pî•ñ’ñ‹Ÿ—¿‚v =
030240                             Ž{pî•ñ’ñ‹Ÿ—¿‚v + Ž{pî•ñ’ñ‹Ÿ—¿‚d‚v
030250                 END-IF
030260* “Á•ÊÞ—¿”ï
030270                 IF Ž{‹L|“Á•ÊÞ—¿‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚ÌŒðŠ·‚ÍA‚Q‰ñ‚Ü‚ÅŽZ’è‚Å‚«‚éB/20180829
                           PERFORM “Á•ÊÞ—¿‰ñ””»’è
      */•½¬30”N9ŒŽ1“ú‚æ‚èŽd—l•ÏX«««/20180427
030280*                     COMPUTE “Á•ÊÞ—¿”ï‚v = “Á•ÊÞ—¿”ï‚v + “Á•ÊÞ—¿”ï‚e‚v
030290*                     MOVE “Á•ÊÞ—¿”ï‚e‚v TO Þ—¿”ï‚Q‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
      *                                            Þ—¿”ï‚m‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
                           IF ˜AŒv|Ž{p˜a—ï”NŒŽ < 43009
      */ˆÈ‘O‚ÌŽd—l‚à•ÏX‚·‚é(‰½“x‚Å‚àŽZ’è‰Â‚¾‚Á‚½‚Ì‚ð‰‰ñˆ’u‚ÌŽž‚¾‚¯‚É•ÏX‚·‚é)/180829
026470                         IF Ž{‹L|®•œŽ{—Ã‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1
026550                             COMPUTE “Á•ÊÞ—¿”ï‚v = “Á•ÊÞ—¿”ï‚v + “Á•ÊÞ—¿”ï‚e‚v
026560                             MOVE “Á•ÊÞ—¿”ï‚e‚v TO Þ—¿”ï‚Q‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
                                                          Þ—¿”ï‚m‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
                               ELSE
                                   PERFORM “Á•ÊÞ—¿‹æ•ªƒNƒŠƒA
026570                         END-IF
                           ELSE
026470                         IF (Ž{‹L|•‰Ží•Ê(•”ˆÊƒJƒEƒ“ƒ^‚Q)     = 04 OR 05 OR 06)   AND (“Á•ÊÞ—¿‚b‚m‚s < 3) OR
026470                            (Ž{‹L|•‰Ží•Ê(•”ˆÊƒJƒEƒ“ƒ^‚Q) NOT = 04 AND 05 AND 06) AND (Ž{‹L|®•œŽ{—Ã‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1)
026550                             COMPUTE “Á•ÊÞ—¿”ï‚v = “Á•ÊÞ—¿”ï‚v + “Á•ÊÞ—¿”ï‚e‚v
026560                             COMPUTE Þ—¿”ï‚Q‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = Þ—¿”ï‚Q‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) + “Á•ÊÞ—¿”ï‚e‚v
026560                             MOVE “Á•ÊÞ—¿”ï‚e‚v TO Þ—¿”ï‚R‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) Þ—¿”ï‚m‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
                                   COMPUTE Þ—¿‰ñ”‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = Þ—¿‰ñ”‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) + 1
HILO  *                             DISPLAY "O " ˜AŒv|Ž{p˜a—ï”NŒŽ“ú " " •”ˆÊƒJƒEƒ“ƒ^‚Q "•”ˆÊ  “Á•ÊÞ—¿‰ñ”=" “Á•ÊÞ—¿‚b‚m‚s
                               ELSE
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚ÌŒðŠ·‚ÍA‚Q‰ñ‚Ü‚ÅŽZ’è‚Å‚«‚éB/20180829
                                   PERFORM “Á•ÊÞ—¿‹æ•ªƒNƒŠƒA
HILO  *                             DISPLAY "X " ˜AŒv|Ž{p˜a—ï”NŒŽ“ú " " •”ˆÊƒJƒEƒ“ƒ^‚Q "•”ˆÊ  “Á•ÊÞ—¿‰ñ”=" “Á•ÊÞ—¿‚b‚m‚s
026570                         END-IF
026570                     END-IF
      */•½¬30”N9ŒŽ1“ú‚æ‚èŽd—l•ÏXªªª/20180427
030300                 END-IF
030310* •ï‘ÑŒðŠ·—¿
030320                 IF Ž{‹L|•ï‘ÑŒðŠ·‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1
030330
030340                     MOVE ŒðŠ·•ï‘Ñ—¿‚e‚v TO •ï‘ÑŒðŠ·’P‰¿‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030350                     COMPUTE •ï‘ÑŒðŠ·‰ñ”‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) =
030360                             •ï‘ÑŒðŠ·‰ñ”‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) + 1
030370
030380                     COMPUTE ŒðŠ·•ï‘Ñ—¿‚v = ŒðŠ·•ï‘Ñ—¿‚v + ŒðŠ·•ï‘Ñ—¿‚e‚v
030390                     COMPUTE •ï‘ÑŒðŠ·—¿‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) =
030400                             •ï‘ÑŒðŠ·—¿‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) + ŒðŠ·•ï‘Ñ—¿‚e‚v
                           MOVE ŒðŠ·•ï‘Ñ—¿‚e‚v TO ŒðŠ·•ï‘Ñ—¿‚m‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030410                     EVALUATE TRUE
030420                     WHEN •ï‘ÑŒðŠ·“ú‚P‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO
030430                          MOVE Ž{‹L|Ž{p“ú TO •ï‘ÑŒðŠ·“ú‚P‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030440                     WHEN •ï‘ÑŒðŠ·“ú‚Q‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO
030450                          MOVE Ž{‹L|Ž{p“ú TO •ï‘ÑŒðŠ·“ú‚Q‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030460                     WHEN •ï‘ÑŒðŠ·“ú‚R‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO
030470                          MOVE Ž{‹L|Ž{p“ú TO •ï‘ÑŒðŠ·“ú‚R‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030480                     WHEN •ï‘ÑŒðŠ·“ú‚S‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO
030490                          MOVE Ž{‹L|Ž{p“ú TO •ï‘ÑŒðŠ·“ú‚S‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030500                     WHEN •ï‘ÑŒðŠ·“ú‚T‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO
030510                          MOVE Ž{‹L|Ž{p“ú TO •ï‘ÑŒðŠ·“ú‚T‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030520                     WHEN •ï‘ÑŒðŠ·“ú‚U‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO
030530                          MOVE Ž{‹L|Ž{p“ú TO •ï‘ÑŒðŠ·“ú‚U‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030540                     END-EVALUATE
030550*                 END-IF
030560             END-IF
030570*
030580             COMPUTE •”ˆÊƒJƒEƒ“ƒ^ = •”ˆÊƒJƒEƒ“ƒ^ + 1
030590*
030600         END-IF
030610     END-PERFORM.
030620*
030630*================================================================*
030640 ŠJŽnI—¹“úŽæ“¾ SECTION.
030650*
030660     MOVE ˜AŒv|Ž{p˜a—ï TO •‰|Ž{p˜a—ï.
030670     MOVE ˜AŒv|Ž{p”N   TO •‰|Ž{p”N.
030680     MOVE ˜AŒv|Ž{pŒŽ   TO •‰|Ž{pŒŽ.
030690     MOVE ˜AŒv|Š³ŽÒ”Ô† TO •‰|Š³ŽÒ”Ô†.
030700     MOVE ˜AŒv|Ž}”Ô     TO •‰|Ž}”Ô.
030710     READ •‰ƒf[ƒ^‚e
030720     INVALID KEY
030730*/ƒGƒ‰[•\Ž¦‚ÌC³
030740         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
030750         DISPLAY "Žw’è‚ÌŽófŽÒ‚Ì•‰ƒf[ƒ^‚ª‚ ‚è‚Ü‚¹‚ñ"
030760                 " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
030770                 " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
030780*-----------------------------------------*
030790         CALL "actcshm"  WITH C LINKAGE
030800*-----------------------------------------*
030810         ACCEPT  ƒL[“ü—Í FROM CONS
030820         PERFORM ƒtƒ@ƒCƒ‹•Â½
030830         MOVE ZERO TO PROGRAM-STATUS
030840         EXIT PROGRAM
030850     NOT INVALID KEY
030860         MOVE •‰|“]‹A‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q)       TO “]‹A‹æ•ª‚v
030870         MOVE •‰|ŠJŽn˜a—ï”NŒŽ“ú(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ŠJŽn˜a—ï”NŒŽ“ú‚v
030880*
030890         IF ( •‰|I—¹˜a—ï(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO ) OR
030900            ( •‰|I—¹”N(•”ˆÊƒJƒEƒ“ƒ^‚Q)   = ZERO ) OR
030910            ( •‰|I—¹ŒŽ(•”ˆÊƒJƒEƒ“ƒ^‚Q)   = ZERO ) OR
030920            ( •‰|I—¹“ú(•”ˆÊƒJƒEƒ“ƒ^‚Q)   = ZERO )
030930             MOVE "9999999" TO I—¹˜a—ï”NŒŽ“ú‚v
030940         ELSE
030950             MOVE •‰|I—¹˜a—ï”NŒŽ“ú(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO I—¹˜a—ï”NŒŽ“ú‚v
030960         END-IF
030970     END-READ.
030980*
030990*================================================================*
031000 Œp‘±•”ˆÊ”ƒZƒbƒg SECTION.
031010*
031020     MOVE ZERO TO Œp‘±•”ˆÊ”‚v.
031030*
031040     MOVE ˜AŒv|Ž{p˜a—ï TO •‰|Ž{p˜a—ï.
031050     MOVE ˜AŒv|Ž{p”N   TO •‰|Ž{p”N.
031060     MOVE ˜AŒv|Ž{pŒŽ   TO •‰|Ž{pŒŽ.
031070     MOVE ˜AŒv|Š³ŽÒ”Ô† TO •‰|Š³ŽÒ”Ô†.
031080     MOVE ˜AŒv|Ž}”Ô     TO •‰|Ž}”Ô.
031090     READ •‰ƒf[ƒ^‚e
031100     INVALID KEY
031110*/ƒGƒ‰[•\Ž¦‚ÌC³
031120         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
031130         DISPLAY "Žw’è‚ÌŽófŽÒ‚Ì•‰ƒf[ƒ^‚ª‚ ‚è‚Ü‚¹‚ñ"
031140                 " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
031150                 " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
031160*-----------------------------------------*
031170         CALL "actcshm"  WITH C LINKAGE
031180*-----------------------------------------*
031190         ACCEPT  ƒL[“ü—Í FROM CONS
031200         PERFORM ƒtƒ@ƒCƒ‹•Â½
031210         MOVE ZERO TO PROGRAM-STATUS
031220         EXIT PROGRAM
031230     NOT INVALID KEY
031240         MOVE •‰|•”ˆÊ” TO •”ˆÊ”‚v
031250         MOVE ˜AŒv|Ž{p˜a—ï TO  Ž{p˜a—ï‚b‚v
031260         MOVE ˜AŒv|Ž{p”N   TO  Ž{p”N‚b‚v
031270         MOVE ˜AŒv|Ž{pŒŽ   TO  Ž{pŒŽ‚b‚v
031280         MOVE Ž{‹L|Ž{p“ú   TO  Ž{p“ú‚b‚v
031290         PERFORM VARYING •”ˆÊƒJƒEƒ“ƒ^ FROM 1 BY 1
031300                 UNTIL •”ˆÊƒJƒEƒ“ƒ^ > •”ˆÊ”‚v
031310*
031320             IF ( •‰|I—¹˜a—ï(•”ˆÊƒJƒEƒ“ƒ^) = ZERO ) OR
031330                ( •‰|I—¹”N(•”ˆÊƒJƒEƒ“ƒ^)   = ZERO ) OR
031340                ( •‰|I—¹ŒŽ(•”ˆÊƒJƒEƒ“ƒ^)   = ZERO ) OR
031350                ( •‰|I—¹“ú(•”ˆÊƒJƒEƒ“ƒ^)   = ZERO )
031360                 MOVE "9999999" TO I—¹˜a—ï”NŒŽ“ú‚v
031370             ELSE
031380                 MOVE •‰|I—¹˜a—ï”NŒŽ“ú(•”ˆÊƒJƒEƒ“ƒ^) TO I—¹˜a—ï”NŒŽ“ú‚v
031390             END-IF
031400*
031410             IF ( •‰|ŠJŽn˜a—ï”NŒŽ“ú(•”ˆÊƒJƒEƒ“ƒ^) <= Ž{‹L|Ž{p˜a—ï”NŒŽ“ú ) AND
031420                ( I—¹˜a—ï”NŒŽ“ú‚v                 >= Ž{‹L|Ž{p˜a—ï”NŒŽ“ú )
031430*                ( •‰|“]‹A‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^) = 9 )
031440                 COMPUTE Œp‘±•”ˆÊ”‚v = Œp‘±•”ˆÊ”‚v + 1
031450                 MOVE •”ˆÊƒJƒEƒ“ƒ^ TO •”ˆÊ’üŒ¸î•ñ‚v(Œp‘±•”ˆÊ”‚v)
031460             END-IF
031470         END-PERFORM
031480     END-READ.
031490*
031500*================================================================*
031510 —ÝŒvˆ— SECTION.
031520*
031530     COMPUTE ‰ŒŸ—¿‚f‚v         = ‰ŒŸ—¿‚f‚v     + ‰ŒŸ—¿‚v.
031540     COMPUTE ‰ŒŸ‰ÁŽZ—¿‚f‚v     = ‰ŒŸ‰ÁŽZ—¿‚f‚v + ‰ŒŸ‰ÁŽZ—¿‚v.
031550     COMPUTE ÄŒŸ—¿‚f‚v         = ÄŒŸ—¿‚f‚v     + ÄŒŸ—¿‚v.
031560     COMPUTE ‰—Ã—¿‚f‚v         = ‰—Ã—¿‚f‚v     + ‰—Ã—¿‚v.
031570     COMPUTE ‰—Ã‰ÁŽZ—¿‚f‚v     = ‰—Ã‰ÁŽZ—¿‚f‚v + ‰—Ã‰ÁŽZ—¿‚v.
031580     COMPUTE ‰ŒŸŽž‘Š’k—¿‚f‚v   = ‰ŒŸŽž‘Š’k—¿‚f‚v + ‰ŒŸŽž‘Š’k—¿‚v.
031590*
031600*     COMPUTE Œã—Ã—¿‚f‚v         = Œã—Ã—¿‚f‚v     + Œã—Ã—¿‚v.
031610*     COMPUTE —âãª–@—¿‚f‚v       = —âãª–@—¿‚f‚v   + —âãª–@—¿‚v.
031620*     COMPUTE ‰·ãª–@—¿‚f‚v       = ‰·ãª–@—¿‚f‚v   + ‰·ãª–@—¿‚v.
031630*     COMPUTE “d—Ã—¿‚f‚v         = “d—Ã—¿‚f‚v     + “d—Ã—¿‚v.
031640*
031650     COMPUTE ‹à‘®•›Žq—¿‚f‚v     = ‹à‘®•›Žq—¿‚f‚v + ‹à‘®•›Žq—¿‚v.
031660     COMPUTE ‰‰ñˆ’u—¿‚f‚v     = ‰‰ñˆ’u—¿‚f‚v + ‰‰ñˆ’u—¿‚v.
031670     COMPUTE Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v = Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v + Ž{pî•ñ’ñ‹Ÿ—¿‚v.
031680*/˜JÐ•ÛŒ¯—p
031690     COMPUTE Žw“±ŠÇ——¿‚f‚v     = Žw“±ŠÇ——¿‚f‚v   + Žw“±ŠÇ——¿‚v.
031700     COMPUTE “Á•ÊÞ—¿”ï‚f‚v     = “Á•ÊÞ—¿”ï‚f‚v   + “Á•ÊÞ—¿”ï‚v.
031710     COMPUTE ŒðŠ·•ï‘Ñ—¿‚f‚v     = ŒðŠ·•ï‘Ñ—¿‚f‚v   + ŒðŠ·•ï‘Ñ—¿‚v.
031720     COMPUTE ‰^“®—Ã–@—¿‚f‚v     = ‰^“®—Ã–@—¿‚f‚v   + ‰^“®—Ã–@—¿‚v.
031730     COMPUTE h”‘—¿‚f‚v         = h”‘—¿‚f‚v       + h”‘—¿‚v.
031740     COMPUTE HŽ–—¿‚f‚v         = HŽ–—¿‚f‚v       + HŽ–—¿‚v.
031750     COMPUTE ‹x‹ÆØ–¾‘—¿‚f‚v   = ‹x‹ÆØ–¾‘—¿‚f‚v + ‹x‹ÆØ–¾‘—¿‚v.
031760*/Ž©”…Ó•ÛŒ¯—p
031770     COMPUTE Ž{pØ–¾‘—¿‚f‚v   = Ž{pØ–¾‘—¿‚f‚v + Ž{pØ–¾‘—¿‚v.
031780     COMPUTE Ž{p–¾×‘—¿‚f‚v   = Ž{p–¾×‘—¿‚f‚v + Ž{p–¾×‘—¿‚v.
031790     COMPUTE ‚»‚Ì‘¼‹àŠz‚f‚v     = ‚»‚Ì‘¼‹àŠz‚f‚v   + ‚»‚Ì‘¼‹àŠz‚v.
031800*
031810*================================================================*
031820 Œã—Ã‘½•”ˆÊ—ÝŒv SECTION.
031830*
031840     EVALUATE •”ˆÊƒJƒEƒ“ƒ^‚Q
031850     WHEN 1
031860         EVALUATE TRUE
031870*       */’Êí
031880         WHEN (Skƒtƒ‰ƒO = SPACE) AND
031890              (’·Šúƒtƒ‰ƒO = SPACE) AND
031900              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
031910             COMPUTE Œã—Ã‰ñ”‚P‚q‚v = Œã—Ã‰ñ”‚P‚q‚v + 1
031920             COMPUTE Œã—Ã—¿‚P‚q‚v   =
031930                     Œã—Ã—¿‚P‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
031940             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚P‚m‚q‚v
031950*       */Sk
031960         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
031970              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
031980             COMPUTE Œã—Ã‰ñ”‚PSk‚q‚v = Œã—Ã‰ñ”‚PSk‚q‚v + 1
031990             COMPUTE Œã—Ã—¿‚PSk‚q‚v   =
032000                     Œã—Ã—¿‚PSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
032010             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚PSk‚m‚q‚v
032020*       */‘½•”ˆÊ
032030         WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
032040              (Skƒtƒ‰ƒO    = SPACE )
032050             COMPUTE ‘½•”ˆÊŒã—Ã‰ñ”‚P‚q‚v  = ‘½•”ˆÊŒã—Ã‰ñ”‚P‚q‚v + 1
032060             COMPUTE ‘½•”ˆÊŒã—Ã—¿‚P‚q‚v   = 
032070                     ‘½•”ˆÊŒã—Ã—¿‚P‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
032080             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊŒã—Ã—¿‚P‚m‚q‚v
032090*       */‘½•”ˆÊ‚ÌSk
032100         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
032110              (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
032120             COMPUTE ‘½•”ˆÊSkŒã—Ã‰ñ”‚P‚q‚v = 
032130                     ‘½•”ˆÊSkŒã—Ã‰ñ”‚P‚q‚v + 1
032140             COMPUTE ‘½•”ˆÊSkŒã—Ã—¿‚P‚q‚v   = 
032150                     ‘½•”ˆÊSkŒã—Ã—¿‚P‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
032160             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊSkŒã—Ã—¿‚P‚m‚q‚v
032170*       */’·Šú
032180         WHEN (’·Šúƒtƒ‰ƒO = "YES" )
032190             COMPUTE ’·ŠúŒã—Ã‰ñ”‚P‚q‚v = ’·ŠúŒã—Ã‰ñ”‚P‚q‚v + 1
032200             COMPUTE ’·ŠúŒã—Ã—¿‚P‚q‚v   = 
032210                     ’·ŠúŒã—Ã—¿‚P‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
032220             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ’·ŠúŒã—Ã—¿‚P‚m‚q‚v
032230*       */‚È‚µ
032240         WHEN OTHER
032250             CONTINUE
032260         END-EVALUATE
032270
032280*/‚Q•”ˆÊ–Ú
032290     WHEN 2
032300         EVALUATE TRUE
032310*       */’Êí
032320         WHEN (Skƒtƒ‰ƒO = SPACE) AND
032330              (’·Šúƒtƒ‰ƒO = SPACE) AND
032340              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
032350             COMPUTE Œã—Ã‰ñ”‚Q‚q‚v = Œã—Ã‰ñ”‚Q‚q‚v + 1
032360             COMPUTE Œã—Ã—¿‚Q‚q‚v   =
032370                     Œã—Ã—¿‚Q‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
032380             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚Q‚m‚q‚v
032390*       */Sk
032400         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
032410              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
032420             COMPUTE Œã—Ã‰ñ”‚QSk‚q‚v = Œã—Ã‰ñ”‚QSk‚q‚v + 1
032430             COMPUTE Œã—Ã—¿‚QSk‚q‚v   =
032440                     Œã—Ã—¿‚QSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
032450             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚QSk‚m‚q‚v
032460*       */‘½•”ˆÊ
032470         WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
032480              (Skƒtƒ‰ƒO    = SPACE )
032490             COMPUTE ‘½•”ˆÊŒã—Ã‰ñ”‚Q‚q‚v  = ‘½•”ˆÊŒã—Ã‰ñ”‚Q‚q‚v + 1
032500             COMPUTE ‘½•”ˆÊŒã—Ã—¿‚Q‚q‚v   = 
032510                     ‘½•”ˆÊŒã—Ã—¿‚Q‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
032520             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊŒã—Ã—¿‚Q‚m‚q‚v
032530*       */‘½•”ˆÊ‚ÌSk
032540         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
032550              (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
032560             COMPUTE ‘½•”ˆÊSkŒã—Ã‰ñ”‚Q‚q‚v = 
032570                     ‘½•”ˆÊSkŒã—Ã‰ñ”‚Q‚q‚v + 1
032580             COMPUTE ‘½•”ˆÊSkŒã—Ã—¿‚Q‚q‚v   = 
032590                     ‘½•”ˆÊSkŒã—Ã—¿‚Q‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
032600             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊSkŒã—Ã—¿‚Q‚m‚q‚v
032610*       */’·Šú
032620         WHEN (’·Šúƒtƒ‰ƒO = "YES" )
032630             COMPUTE ’·ŠúŒã—Ã‰ñ”‚Q‚q‚v = ’·ŠúŒã—Ã‰ñ”‚Q‚q‚v + 1
032640             COMPUTE ’·ŠúŒã—Ã—¿‚Q‚q‚v   = 
032650                     ’·ŠúŒã—Ã—¿‚Q‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
032660             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ’·ŠúŒã—Ã—¿‚Q‚m‚q‚v
032670*       */‚È‚µ
032680         WHEN OTHER
032690             CONTINUE
032700         END-EVALUATE
032710*/‚R•”ˆÊ–Ú
032720     WHEN 3
032730         EVALUATE TRUE
032740*       */’Êí
032750         WHEN (Skƒtƒ‰ƒO = SPACE) AND
032760              (’·Šúƒtƒ‰ƒO = SPACE) AND
032770              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
032780             COMPUTE Œã—Ã‰ñ”‚R‚q‚v = Œã—Ã‰ñ”‚R‚q‚v + 1
032790             COMPUTE Œã—Ã—¿‚R‚q‚v   =
032800                     Œã—Ã—¿‚R‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
032810             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚R‚m‚q‚v
032820*       */Sk
032830         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
032840              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
032850             COMPUTE Œã—Ã‰ñ”‚RSk‚q‚v = Œã—Ã‰ñ”‚RSk‚q‚v + 1
032860             COMPUTE Œã—Ã—¿‚RSk‚q‚v   =
032870                     Œã—Ã—¿‚RSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
032880             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚RSk‚m‚q‚v
032890*       */‘½•”ˆÊ
032900         WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
032910              (Skƒtƒ‰ƒO    = SPACE )
032920             COMPUTE ‘½•”ˆÊŒã—Ã‰ñ”‚R‚q‚v  = ‘½•”ˆÊŒã—Ã‰ñ”‚R‚q‚v + 1
032930             COMPUTE ‘½•”ˆÊŒã—Ã—¿‚R‚q‚v   = 
032940                     ‘½•”ˆÊŒã—Ã—¿‚R‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
032950             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊŒã—Ã—¿‚R‚m‚q‚v
032960*       */‘½•”ˆÊ‚ÌSk
032970         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
032980              (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
032990             COMPUTE ‘½•”ˆÊSkŒã—Ã‰ñ”‚R‚q‚v = 
033000                     ‘½•”ˆÊSkŒã—Ã‰ñ”‚R‚q‚v + 1
033010             COMPUTE ‘½•”ˆÊSkŒã—Ã—¿‚R‚q‚v   = 
033020                     ‘½•”ˆÊSkŒã—Ã—¿‚R‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
033030             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊSkŒã—Ã—¿‚R‚m‚q‚v
033040*       */’·Šú
033050         WHEN (’·Šúƒtƒ‰ƒO = "YES" )
033060             COMPUTE ’·ŠúŒã—Ã‰ñ”‚R‚q‚v = ’·ŠúŒã—Ã‰ñ”‚R‚q‚v + 1
033070             COMPUTE ’·ŠúŒã—Ã—¿‚R‚q‚v   = 
033080                     ’·ŠúŒã—Ã—¿‚R‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
033090             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ’·ŠúŒã—Ã—¿‚R‚m‚q‚v
033100*       */‚È‚µ
033110         WHEN OTHER
033120             CONTINUE
033130         END-EVALUATE
033140*/‚S•”ˆÊ–Ú
033150     WHEN 4
033160         EVALUATE TRUE
033170*       */’Êí
033180         WHEN (Skƒtƒ‰ƒO = SPACE) AND
033190              (’·Šúƒtƒ‰ƒO = SPACE) AND
033200              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
033210             COMPUTE Œã—Ã‰ñ”‚S‚q‚v = Œã—Ã‰ñ”‚S‚q‚v + 1
033220             COMPUTE Œã—Ã—¿‚S‚q‚v   =
033230                     Œã—Ã—¿‚S‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
033240             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚S‚m‚q‚v
033250*       */Sk
033260         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
033270              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
033280             COMPUTE Œã—Ã‰ñ”‚SSk‚q‚v = Œã—Ã‰ñ”‚SSk‚q‚v + 1
033290             COMPUTE Œã—Ã—¿‚SSk‚q‚v   =
033300                     Œã—Ã—¿‚SSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
033310             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚SSk‚m‚q‚v
033320*       */‘½•”ˆÊ
033330         WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
033340              (Skƒtƒ‰ƒO    = SPACE )
033350             COMPUTE ‘½•”ˆÊŒã—Ã‰ñ”‚S‚q‚v  = ‘½•”ˆÊŒã—Ã‰ñ”‚S‚q‚v + 1
033360             COMPUTE ‘½•”ˆÊŒã—Ã—¿‚S‚q‚v   = 
033370                     ‘½•”ˆÊŒã—Ã—¿‚S‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
033380             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊŒã—Ã—¿‚S‚m‚q‚v
033390*       */‘½•”ˆÊ‚ÌSk
033400         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
033410              (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
033420             COMPUTE ‘½•”ˆÊSkŒã—Ã‰ñ”‚S‚q‚v = 
033430                     ‘½•”ˆÊSkŒã—Ã‰ñ”‚S‚q‚v + 1
033440             COMPUTE ‘½•”ˆÊSkŒã—Ã—¿‚S‚q‚v   = 
033450                     ‘½•”ˆÊSkŒã—Ã—¿‚S‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
033460             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊSkŒã—Ã—¿‚S‚m‚q‚v
033470*       */’·Šú
033480         WHEN (’·Šúƒtƒ‰ƒO = "YES" )
033490             COMPUTE ’·ŠúŒã—Ã‰ñ”‚S‚q‚v = ’·ŠúŒã—Ã‰ñ”‚S‚q‚v + 1
033500             COMPUTE ’·ŠúŒã—Ã—¿‚S‚q‚v   = 
033510                     ’·ŠúŒã—Ã—¿‚S‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
033520             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ’·ŠúŒã—Ã—¿‚S‚m‚q‚v
033530*       */‚È‚µ
033540         WHEN OTHER
033550             CONTINUE
033560         END-EVALUATE
033570     WHEN 5
033580         EVALUATE TRUE
033590*       */’Êí
033600         WHEN (Skƒtƒ‰ƒO = SPACE) AND
033610              (’·Šúƒtƒ‰ƒO = SPACE) AND
033620              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
033630             COMPUTE Œã—Ã‰ñ”‚T‚q‚v = Œã—Ã‰ñ”‚T‚q‚v + 1
033640             COMPUTE Œã—Ã—¿‚T‚q‚v   =
033650                     Œã—Ã—¿‚T‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
033660             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚T‚m‚q‚v
033670*       */Sk
033680         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
033690              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
033700             COMPUTE Œã—Ã‰ñ”‚TSk‚q‚v = Œã—Ã‰ñ”‚TSk‚q‚v + 1
033710             COMPUTE Œã—Ã—¿‚TSk‚q‚v   =
033720                     Œã—Ã—¿‚TSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
033730             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚TSk‚m‚q‚v
033740*       */‘½•”ˆÊ
033750         WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
033760              (Skƒtƒ‰ƒO    = SPACE )
033770             COMPUTE ‘½•”ˆÊŒã—Ã‰ñ”‚T‚q‚v  = ‘½•”ˆÊŒã—Ã‰ñ”‚T‚q‚v + 1
033780             COMPUTE ‘½•”ˆÊŒã—Ã—¿‚T‚q‚v   = 
033790                     ‘½•”ˆÊŒã—Ã—¿‚T‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
033800             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊŒã—Ã—¿‚T‚m‚q‚v
033810*       */‘½•”ˆÊ‚ÌSk
033820         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
033830              (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
033840             COMPUTE ‘½•”ˆÊSkŒã—Ã‰ñ”‚T‚q‚v = 
033850                     ‘½•”ˆÊSkŒã—Ã‰ñ”‚T‚q‚v + 1
033860             COMPUTE ‘½•”ˆÊSkŒã—Ã—¿‚T‚q‚v   = 
033870                     ‘½•”ˆÊSkŒã—Ã—¿‚T‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
033880             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊSkŒã—Ã—¿‚T‚m‚q‚v
033890*       */’·Šú
033900         WHEN (’·Šúƒtƒ‰ƒO = "YES" )
033910             COMPUTE ’·ŠúŒã—Ã‰ñ”‚T‚q‚v = ’·ŠúŒã—Ã‰ñ”‚T‚q‚v + 1
033920             COMPUTE ’·ŠúŒã—Ã—¿‚T‚q‚v   = 
033930                     ’·ŠúŒã—Ã—¿‚T‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
033940             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ’·ŠúŒã—Ã—¿‚T‚m‚q‚v
033950*       */‚È‚µ
033960         WHEN OTHER
033970             CONTINUE
033980         END-EVALUATE
033990*/‚U•”ˆÊ–Ú
034000     WHEN 6
034010         EVALUATE TRUE
034020*       */’Êí
034030         WHEN (Skƒtƒ‰ƒO = SPACE) AND
034040              (’·Šúƒtƒ‰ƒO = SPACE) AND
034050              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
034060             COMPUTE Œã—Ã‰ñ”‚U‚q‚v = Œã—Ã‰ñ”‚U‚q‚v + 1
034070             COMPUTE Œã—Ã—¿‚U‚q‚v   =
034080                     Œã—Ã—¿‚U‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
034090             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚U‚m‚q‚v
034100         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
034110              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
034120             COMPUTE Œã—Ã‰ñ”‚USk‚q‚v = Œã—Ã‰ñ”‚USk‚q‚v + 1
034130             COMPUTE Œã—Ã—¿‚USk‚q‚v   =
034140                     Œã—Ã—¿‚USk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
034150             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚USk‚m‚q‚v
034160*       */‘½•”ˆÊ
034170         WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
034180              (Skƒtƒ‰ƒO    = SPACE )
034190             COMPUTE ‘½•”ˆÊŒã—Ã‰ñ”‚U‚q‚v  = ‘½•”ˆÊŒã—Ã‰ñ”‚U‚q‚v + 1
034200             COMPUTE ‘½•”ˆÊŒã—Ã—¿‚U‚q‚v   = 
034210                     ‘½•”ˆÊŒã—Ã—¿‚U‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
034220             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊŒã—Ã—¿‚U‚m‚q‚v
034230*       */‘½•”ˆÊ‚ÌSk
034240         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
034250              (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
034260             COMPUTE ‘½•”ˆÊSkŒã—Ã‰ñ”‚U‚q‚v = 
034270                     ‘½•”ˆÊSkŒã—Ã‰ñ”‚U‚q‚v + 1
034280             COMPUTE ‘½•”ˆÊSkŒã—Ã—¿‚U‚q‚v   = 
034290                     ‘½•”ˆÊSkŒã—Ã—¿‚U‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
034300             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊSkŒã—Ã—¿‚U‚m‚q‚v
034310*       */’·Šú
034320         WHEN (’·Šúƒtƒ‰ƒO = "YES" )
034330             COMPUTE ’·ŠúŒã—Ã‰ñ”‚U‚q‚v = ’·ŠúŒã—Ã‰ñ”‚U‚q‚v + 1
034340             COMPUTE ’·ŠúŒã—Ã—¿‚U‚q‚v   = 
034350                     ’·ŠúŒã—Ã—¿‚U‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
034360             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ’·ŠúŒã—Ã—¿‚U‚m‚q‚v
034370*       */‚È‚µ
034380         WHEN OTHER
034390             CONTINUE
034400         END-EVALUATE
034410*/‚V•”ˆÊ–Ú
034420     WHEN 7
034430         EVALUATE TRUE
034440*       */’Êí
034450         WHEN (Skƒtƒ‰ƒO = SPACE) AND
034460              (’·Šúƒtƒ‰ƒO = SPACE) AND
034470              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
034480             COMPUTE Œã—Ã‰ñ”‚V‚q‚v = Œã—Ã‰ñ”‚V‚q‚v + 1
034490             COMPUTE Œã—Ã—¿‚V‚q‚v   =
034500                     Œã—Ã—¿‚V‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
034510             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚V‚m‚q‚v
034520*       */Sk
034530         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
034540              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
034550             COMPUTE Œã—Ã‰ñ”‚VSk‚q‚v = Œã—Ã‰ñ”‚VSk‚q‚v + 1
034560             COMPUTE Œã—Ã—¿‚VSk‚q‚v   =
034570                     Œã—Ã—¿‚VSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
034580             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚VSk‚m‚q‚v
034590*       */‘½•”ˆÊ
034600         WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
034610              (Skƒtƒ‰ƒO    = SPACE )
034620             COMPUTE ‘½•”ˆÊŒã—Ã‰ñ”‚V‚q‚v  = ‘½•”ˆÊŒã—Ã‰ñ”‚V‚q‚v + 1
034630             COMPUTE ‘½•”ˆÊŒã—Ã—¿‚V‚q‚v   = 
034640                     ‘½•”ˆÊŒã—Ã—¿‚V‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
034650             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊŒã—Ã—¿‚V‚m‚q‚v
034660*       */‘½•”ˆÊ‚ÌSk
034670         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
034680              (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
034690             COMPUTE ‘½•”ˆÊSkŒã—Ã‰ñ”‚V‚q‚v = 
034700                     ‘½•”ˆÊSkŒã—Ã‰ñ”‚V‚q‚v + 1
034710             COMPUTE ‘½•”ˆÊSkŒã—Ã—¿‚V‚q‚v   = 
034720                     ‘½•”ˆÊSkŒã—Ã—¿‚V‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
034730             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊSkŒã—Ã—¿‚V‚m‚q‚v
034740*       */’·Šú
034750         WHEN (’·Šúƒtƒ‰ƒO = "YES" )
034760             COMPUTE ’·ŠúŒã—Ã‰ñ”‚V‚q‚v = ’·ŠúŒã—Ã‰ñ”‚V‚q‚v + 1
034770             COMPUTE ’·ŠúŒã—Ã—¿‚V‚q‚v   = 
034780                     ’·ŠúŒã—Ã—¿‚V‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
034790             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ’·ŠúŒã—Ã—¿‚V‚m‚q‚v
034800*       */‚È‚µ
034810         WHEN OTHER
034820             CONTINUE
034830         END-EVALUATE
034840*/‚W•”ˆÊ–Ú
034850     WHEN 8
034860         EVALUATE TRUE
034870*       */’Êí
034880         WHEN (Skƒtƒ‰ƒO = SPACE) AND
034890              (’·Šúƒtƒ‰ƒO = SPACE) AND
034900              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
034910             COMPUTE Œã—Ã‰ñ”‚W‚q‚v = Œã—Ã‰ñ”‚W‚q‚v + 1
034920             COMPUTE Œã—Ã—¿‚W‚q‚v   =
034930                     Œã—Ã—¿‚W‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
034940             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚W‚m‚q‚v
034950*       */Sk
034960         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
034970              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
034980             COMPUTE Œã—Ã‰ñ”‚WSk‚q‚v = Œã—Ã‰ñ”‚WSk‚q‚v + 1
034990             COMPUTE Œã—Ã—¿‚WSk‚q‚v   =
035000                     Œã—Ã—¿‚WSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
035010             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚WSk‚m‚q‚v
035020*       */‘½•”ˆÊ
035030         WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
035040              (Skƒtƒ‰ƒO    = SPACE )
035050             COMPUTE ‘½•”ˆÊŒã—Ã‰ñ”‚W‚q‚v  = ‘½•”ˆÊŒã—Ã‰ñ”‚W‚q‚v + 1
035060             COMPUTE ‘½•”ˆÊŒã—Ã—¿‚W‚q‚v   = 
035070                     ‘½•”ˆÊŒã—Ã—¿‚W‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
035080             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊŒã—Ã—¿‚W‚m‚q‚v
035090*       */‘½•”ˆÊ‚ÌSk
035100         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
035110              (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
035120             COMPUTE ‘½•”ˆÊSkŒã—Ã‰ñ”‚W‚q‚v = 
035130                     ‘½•”ˆÊSkŒã—Ã‰ñ”‚W‚q‚v + 1
035140             COMPUTE ‘½•”ˆÊSkŒã—Ã—¿‚W‚q‚v   = 
035150                     ‘½•”ˆÊSkŒã—Ã—¿‚W‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
035160             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊSkŒã—Ã—¿‚W‚m‚q‚v
035170*       */’·Šú
035180         WHEN (’·Šúƒtƒ‰ƒO = "YES" )
035190             COMPUTE ’·ŠúŒã—Ã‰ñ”‚W‚q‚v = ’·ŠúŒã—Ã‰ñ”‚T‚q‚v + 1
035200             COMPUTE ’·ŠúŒã—Ã—¿‚W‚q‚v   = 
035210                     ’·ŠúŒã—Ã—¿‚W‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
035220             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ’·ŠúŒã—Ã—¿‚W‚m‚q‚v
035230*       */‚È‚µ
035240         WHEN OTHER
035250             CONTINUE
035260         END-EVALUATE
035270*/‚X•”ˆÊ–Ú
035280     WHEN 9
035290         EVALUATE TRUE
035300*       */’Êí
035310         WHEN (Skƒtƒ‰ƒO = SPACE) AND
035320              (’·Šúƒtƒ‰ƒO = SPACE) AND
035330              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
035340             COMPUTE Œã—Ã‰ñ”‚X‚q‚v = Œã—Ã‰ñ”‚X‚q‚v + 1
035350             COMPUTE Œã—Ã—¿‚X‚q‚v   =
035360                     Œã—Ã—¿‚X‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
035370             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚X‚m‚q‚v
035380*       */Sk
035390         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
035400              (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
035410             COMPUTE Œã—Ã‰ñ”‚XSk‚q‚v = Œã—Ã‰ñ”‚XSk‚q‚v + 1
035420             COMPUTE Œã—Ã—¿‚XSk‚q‚v   =
035430                     Œã—Ã—¿‚XSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
035440             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚XSk‚m‚q‚v
035450*       */‘½•”ˆÊ
035460         WHEN (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v) AND
035470              (Skƒtƒ‰ƒO    = SPACE )
035480             COMPUTE ‘½•”ˆÊŒã—Ã‰ñ”‚X‚q‚v  = ‘½•”ˆÊŒã—Ã‰ñ”‚X‚q‚v + 1
035490             COMPUTE ‘½•”ˆÊŒã—Ã—¿‚X‚q‚v   = 
035500                     ‘½•”ˆÊŒã—Ã—¿‚X‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
035510             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊŒã—Ã—¿‚X‚m‚q‚v
035520*       */‘½•”ˆÊ‚ÌSk
035530         WHEN (Skƒtƒ‰ƒO = "YES" ) AND
035540              (•”ˆÊƒJƒEƒ“ƒ^ >= ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
035550             COMPUTE ‘½•”ˆÊSkŒã—Ã‰ñ”‚X‚q‚v = 
035560                     ‘½•”ˆÊSkŒã—Ã‰ñ”‚X‚q‚v + 1
035570             COMPUTE ‘½•”ˆÊSkŒã—Ã—¿‚X‚q‚v   = 
035580                     ‘½•”ˆÊSkŒã—Ã—¿‚X‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
035590             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‘½•”ˆÊSkŒã—Ã—¿‚X‚m‚q‚v
035600*       */’·Šú
035610         WHEN (’·Šúƒtƒ‰ƒO = "YES" )
035620             COMPUTE ’·ŠúŒã—Ã‰ñ”‚X‚q‚v = ’·ŠúŒã—Ã‰ñ”‚T‚q‚v + 1
035630             COMPUTE ’·ŠúŒã—Ã—¿‚X‚q‚v   = 
035640                     ’·ŠúŒã—Ã—¿‚X‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
035650             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ’·ŠúŒã—Ã—¿‚X‚m‚q‚v
035660*       */‚È‚µ
035670         WHEN OTHER
035680             CONTINUE
035690         END-EVALUATE
035700     WHEN OTHER
035710         CONTINUE
035720     END-EVALUATE.
035730*
035740*================================================================*
035750 —âãª–@‘½•”ˆÊ—ÝŒv SECTION.
035760*
035770     EVALUATE •”ˆÊƒJƒEƒ“ƒ^‚Q
035780     WHEN 1
035790         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
035800            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
035810             COMPUTE —âãª–@‰ñ”‚P‚q‚v   = —âãª–@‰ñ”‚P‚q‚v + 1
035820             COMPUTE —âãª–@—¿‚P‚q‚v     =
035830                     —âãª–@—¿‚P‚q‚v     + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
035840             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚P‚m‚q‚v
035850         ELSE
035860             COMPUTE —âãª–@‰ñ”‚PSk‚q‚v   = —âãª–@‰ñ”‚PSk‚q‚v + 1
035870             COMPUTE —âãª–@—¿‚PSk‚q‚v     =
035880                     —âãª–@—¿‚PSk‚q‚v     + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
035890             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚PSk‚m‚q‚v
035900         END-IF
035910     WHEN 2
035920         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
035930            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
035940             COMPUTE —âãª–@‰ñ”‚Q‚q‚v   = —âãª–@‰ñ”‚Q‚q‚v + 1
035950             COMPUTE —âãª–@—¿‚Q‚q‚v     =
035960                     —âãª–@—¿‚Q‚q‚v     + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
035970             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚Q‚m‚q‚v
035980         ELSE
035990             COMPUTE —âãª–@‰ñ”‚QSk‚q‚v   = —âãª–@‰ñ”‚QSk‚q‚v + 1
036000             COMPUTE —âãª–@—¿‚QSk‚q‚v     =
036010                     —âãª–@—¿‚QSk‚q‚v     + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036020             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚QSk‚m‚q‚v
036030         END-IF
036040     WHEN 3
036050         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
036060            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
036070             COMPUTE —âãª–@‰ñ”‚R‚q‚v = —âãª–@‰ñ”‚R‚q‚v + 1
036080             COMPUTE —âãª–@—¿‚R‚q‚v   =
036090                     —âãª–@—¿‚R‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036100             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚R‚m‚q‚v
036110         ELSE
036120             COMPUTE —âãª–@‰ñ”‚RSk‚q‚v = —âãª–@‰ñ”‚RSk‚q‚v + 1
036130             COMPUTE —âãª–@—¿‚RSk‚q‚v   =
036140                     —âãª–@—¿‚RSk‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036150             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚RSk‚m‚q‚v
036160         END-IF
036170     WHEN 4
036180         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
036190            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
036200             COMPUTE —âãª–@‰ñ”‚S‚q‚v = —âãª–@‰ñ”‚S‚q‚v + 1
036210             COMPUTE —âãª–@—¿‚S‚q‚v   =
036220                     —âãª–@—¿‚S‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036230             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚S‚m‚q‚v
036240         ELSE
036250             COMPUTE —âãª–@‰ñ”‚SSk‚q‚v = —âãª–@‰ñ”‚SSk‚q‚v + 1
036260             COMPUTE —âãª–@—¿‚SSk‚q‚v   =
036270                     —âãª–@—¿‚SSk‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036280             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚SSk‚m‚q‚v
036290         END-IF
036300     WHEN 5
036310         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
036320            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
036330             COMPUTE —âãª–@‰ñ”‚T‚q‚v = —âãª–@‰ñ”‚T‚q‚v + 1
036340             COMPUTE —âãª–@—¿‚T‚q‚v   =
036350                     —âãª–@—¿‚T‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036360             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚T‚m‚q‚v
036370         ELSE
036380             COMPUTE —âãª–@‰ñ”‚TSk‚q‚v = —âãª–@‰ñ”‚TSk‚q‚v + 1
036390             COMPUTE —âãª–@—¿‚TSk‚q‚v   =
036400                     —âãª–@—¿‚TSk‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036410             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚TSk‚m‚q‚v
036420         END-IF
036430     WHEN 6
036440         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
036450            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
036460             COMPUTE —âãª–@‰ñ”‚U‚q‚v = —âãª–@‰ñ”‚U‚q‚v + 1
036470             COMPUTE —âãª–@—¿‚U‚q‚v   =
036480                     —âãª–@—¿‚U‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036490             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚U‚m‚q‚v
036500         ELSE
036510             COMPUTE —âãª–@‰ñ”‚USk‚q‚v = —âãª–@‰ñ”‚USk‚q‚v + 1
036520             COMPUTE —âãª–@—¿‚USk‚q‚v   =
036530                     —âãª–@—¿‚USk‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036540             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚USk‚m‚q‚v
036550         END-IF
036560     WHEN 7
036570         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
036580            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
036590             COMPUTE —âãª–@‰ñ”‚V‚q‚v = —âãª–@‰ñ”‚V‚q‚v + 1
036600             COMPUTE —âãª–@—¿‚V‚q‚v   =
036610                     —âãª–@—¿‚V‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036620             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚V‚m‚q‚v
036630         ELSE
036640             COMPUTE —âãª–@‰ñ”‚VSk‚q‚v = —âãª–@‰ñ”‚VSk‚q‚v + 1
036650             COMPUTE —âãª–@—¿‚VSk‚q‚v   =
036660                     —âãª–@—¿‚VSk‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036670             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚VSk‚m‚q‚v
036680         END-IF
036690     WHEN 8
036700         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
036710            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
036720             COMPUTE —âãª–@‰ñ”‚W‚q‚v = —âãª–@‰ñ”‚W‚q‚v + 1
036730             COMPUTE —âãª–@—¿‚W‚q‚v   =
036740                     —âãª–@—¿‚W‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036750             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚W‚m‚q‚v
036760         ELSE
036770             COMPUTE —âãª–@‰ñ”‚WSk‚q‚v = —âãª–@‰ñ”‚WSk‚q‚v + 1
036780             COMPUTE —âãª–@—¿‚WSk‚q‚v   =
036790                     —âãª–@—¿‚WSk‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036800             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚WSk‚m‚q‚v
036810         END-IF
036820     WHEN 9
036830         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
036840            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
036850             COMPUTE —âãª–@‰ñ”‚X‚q‚v = —âãª–@‰ñ”‚X‚q‚v + 1
036860             COMPUTE —âãª–@—¿‚X‚q‚v   =
036870                     —âãª–@—¿‚X‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036880             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚X‚m‚q‚v
036890         ELSE
036900             COMPUTE —âãª–@‰ñ”‚XSk‚q‚v = —âãª–@‰ñ”‚XSk‚q‚v + 1
036910             COMPUTE —âãª–@—¿‚XSk‚q‚v   =
036920                     —âãª–@—¿‚XSk‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
036930             MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚XSk‚m‚q‚v
036940         END-IF
036950     WHEN OTHER
036960         CONTINUE
036970     END-EVALUATE.
036980*
036990*================================================================*
037000 ‰·ãª–@‘½•”ˆÊ—ÝŒv SECTION.
037010*
037020     EVALUATE •”ˆÊƒJƒEƒ“ƒ^‚Q
037030     WHEN 1
037040         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
037050            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
037060              COMPUTE ‰·ãª–@‰ñ”‚P‚q‚v = ‰·ãª–@‰ñ”‚P‚q‚v + 1
037070              COMPUTE ‰·ãª–@—¿‚P‚q‚v   =
037080                      ‰·ãª–@—¿‚P‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037090             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚P‚m‚q‚v
037100         ELSE
037110              COMPUTE ‰·ãª–@‰ñ”‚PSk‚q‚v = ‰·ãª–@‰ñ”‚PSk‚q‚v + 1
037120              COMPUTE ‰·ãª–@—¿‚PSk‚q‚v   =
037130                      ‰·ãª–@—¿‚PSk‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037140             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚PSk‚m‚q‚v
037150         END-IF
037160     WHEN 2
037170         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
037180            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
037190             COMPUTE ‰·ãª–@‰ñ”‚Q‚q‚v   = ‰·ãª–@‰ñ”‚Q‚q‚v + 1
037200             COMPUTE ‰·ãª–@—¿‚Q‚q‚v     =
037210                     ‰·ãª–@—¿‚Q‚q‚v     + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037220             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚Q‚m‚q‚v
037230         ELSE
037240             COMPUTE ‰·ãª–@‰ñ”‚QSk‚q‚v = ‰·ãª–@‰ñ”‚QSk‚q‚v + 1
037250             COMPUTE ‰·ãª–@—¿‚QSk‚q‚v   =
037260                     ‰·ãª–@—¿‚QSk‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037270             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚QSk‚m‚q‚v
037280         END-IF
037290     WHEN 3
037300         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
037310            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
037320             COMPUTE ‰·ãª–@‰ñ”‚R‚q‚v = ‰·ãª–@‰ñ”‚R‚q‚v + 1
037330             COMPUTE ‰·ãª–@—¿‚R‚q‚v   =
037340                     ‰·ãª–@—¿‚R‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037350             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚R‚m‚q‚v
037360         ELSE
037370             COMPUTE ‰·ãª–@‰ñ”‚RSk‚q‚v = ‰·ãª–@‰ñ”‚RSk‚q‚v + 1
037380             COMPUTE ‰·ãª–@—¿‚RSk‚q‚v   =
037390                     ‰·ãª–@—¿‚RSk‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037400             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚RSk‚m‚q‚v
037410         END-IF
037420     WHEN 4
037430         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
037440            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
037450             COMPUTE ‰·ãª–@‰ñ”‚S‚q‚v = ‰·ãª–@‰ñ”‚S‚q‚v + 1
037460             COMPUTE ‰·ãª–@—¿‚S‚q‚v   =
037470                     ‰·ãª–@—¿‚S‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037480             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚S‚m‚q‚v
037490         ELSE
037500             COMPUTE ‰·ãª–@‰ñ”‚SSk‚q‚v = ‰·ãª–@‰ñ”‚SSk‚q‚v + 1
037510             COMPUTE ‰·ãª–@—¿‚SSk‚q‚v   =
037520                     ‰·ãª–@—¿‚SSk‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037530             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚SSk‚m‚q‚v
037540         END-IF
037550     WHEN 5
037560         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
037570            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
037580             COMPUTE ‰·ãª–@‰ñ”‚T‚q‚v = ‰·ãª–@‰ñ”‚T‚q‚v + 1
037590             COMPUTE ‰·ãª–@—¿‚T‚q‚v   =
037600                     ‰·ãª–@—¿‚T‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037610             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚T‚m‚q‚v
037620         ELSE
037630             COMPUTE ‰·ãª–@‰ñ”‚TSk‚q‚v = ‰·ãª–@‰ñ”‚TSk‚q‚v + 1
037640             COMPUTE ‰·ãª–@—¿‚TSk‚q‚v   =
037650                     ‰·ãª–@—¿‚TSk‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037660             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚TSk‚m‚q‚v
037670         END-IF
037680     WHEN 6
037690         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
037700            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
037710             COMPUTE ‰·ãª–@‰ñ”‚U‚q‚v = ‰·ãª–@‰ñ”‚U‚q‚v + 1
037720             COMPUTE ‰·ãª–@—¿‚U‚q‚v   =
037730                     ‰·ãª–@—¿‚U‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037740             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚U‚m‚q‚v
037750         ELSE
037760             COMPUTE ‰·ãª–@‰ñ”‚USk‚q‚v = ‰·ãª–@‰ñ”‚USk‚q‚v + 1
037770             COMPUTE ‰·ãª–@—¿‚USk‚q‚v   =
037780                     ‰·ãª–@—¿‚USk‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037790             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚USk‚m‚q‚v
037800         END-IF
037810     WHEN 7
037820         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
037830            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
037840             COMPUTE ‰·ãª–@‰ñ”‚V‚q‚v = ‰·ãª–@‰ñ”‚V‚q‚v + 1
037850             COMPUTE ‰·ãª–@—¿‚V‚q‚v   =
037860                     ‰·ãª–@—¿‚V‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037870             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚V‚m‚q‚v
037880         ELSE
037890             COMPUTE ‰·ãª–@‰ñ”‚VSk‚q‚v = ‰·ãª–@‰ñ”‚VSk‚q‚v + 1
037900             COMPUTE ‰·ãª–@—¿‚VSk‚q‚v   =
037910                     ‰·ãª–@—¿‚VSk‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037920             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚VSk‚m‚q‚v
037930         END-IF
037940     WHEN 8
037950         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
037960            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
037970             COMPUTE ‰·ãª–@‰ñ”‚W‚q‚v = ‰·ãª–@‰ñ”‚W‚q‚v + 1
037980             COMPUTE ‰·ãª–@—¿‚W‚q‚v   =
037990                     ‰·ãª–@—¿‚W‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038000             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚W‚m‚q‚v
038010         ELSE
038020             COMPUTE ‰·ãª–@‰ñ”‚WSk‚q‚v = ‰·ãª–@‰ñ”‚WSk‚q‚v + 1
038030             COMPUTE ‰·ãª–@—¿‚WSk‚q‚v   =
038040                     ‰·ãª–@—¿‚WSk‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038050             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚WSk‚m‚q‚v
038060         END-IF
038070     WHEN 9
038080         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
038090            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
038100             COMPUTE ‰·ãª–@‰ñ”‚X‚q‚v = ‰·ãª–@‰ñ”‚X‚q‚v + 1
038110             COMPUTE ‰·ãª–@—¿‚X‚q‚v   =
038120                     ‰·ãª–@—¿‚X‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038130             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚X‚m‚q‚v
038140         ELSE
038150             COMPUTE ‰·ãª–@‰ñ”‚XSk‚q‚v = ‰·ãª–@‰ñ”‚XSk‚q‚v + 1
038160             COMPUTE ‰·ãª–@—¿‚XSk‚q‚v   =
038170                     ‰·ãª–@—¿‚XSk‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038180             MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚XSk‚m‚q‚v
038190         END-IF
038200     WHEN OTHER
038210         CONTINUE
038220     END-EVALUATE.
038230*
038240*================================================================*
038250 “d—Ã‘½•”ˆÊ—ÝŒv SECTION.
038260*
038270     EVALUATE •”ˆÊƒJƒEƒ“ƒ^‚Q
038280     WHEN 1
038290         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
038300            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
038310             COMPUTE “d—Ã‰ñ”‚P‚q‚v = “d—Ã‰ñ”‚P‚q‚v + 1
038320             COMPUTE “d—Ã—¿‚P‚q‚v   =
038330                     “d—Ã—¿‚P‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038340             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚P‚m‚q‚v
038350         ELSE
038360             COMPUTE “d—Ã‰ñ”‚PSk‚q‚v = “d—Ã‰ñ”‚PSk‚q‚v + 1
038370             COMPUTE “d—Ã—¿‚PSk‚q‚v   =
038380                     “d—Ã—¿‚PSk‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038390             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚PSk‚m‚q‚v
038400         END-IF
038410     WHEN 2
038420         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
038430            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
038440             COMPUTE “d—Ã‰ñ”‚Q‚q‚v = “d—Ã‰ñ”‚Q‚q‚v + 1
038450             COMPUTE “d—Ã—¿‚Q‚q‚v   =
038460                     “d—Ã—¿‚Q‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038470             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚Q‚m‚q‚v
038480         ELSE
038490             COMPUTE “d—Ã‰ñ”‚QSk‚q‚v = “d—Ã‰ñ”‚QSk‚q‚v + 1
038500             COMPUTE “d—Ã—¿‚QSk‚q‚v   =
038510                     “d—Ã—¿‚QSk‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038520             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚QSk‚m‚q‚v
038530         END-IF
038540     WHEN 3
038550         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
038560            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
038570             COMPUTE “d—Ã‰ñ”‚R‚q‚v = “d—Ã‰ñ”‚R‚q‚v + 1
038580             COMPUTE “d—Ã—¿‚R‚q‚v   =
038590                     “d—Ã—¿‚R‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038600             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚R‚m‚q‚v
038610         ELSE
038620             COMPUTE “d—Ã‰ñ”‚RSk‚q‚v = “d—Ã‰ñ”‚RSk‚q‚v + 1
038630             COMPUTE “d—Ã—¿‚RSk‚q‚v   =
038640                     “d—Ã—¿‚RSk‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038650             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚RSk‚m‚q‚v
038660         END-IF
038670     WHEN 4
038680         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
038690            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
038700             COMPUTE “d—Ã‰ñ”‚S‚q‚v = “d—Ã‰ñ”‚S‚q‚v + 1
038710             COMPUTE “d—Ã—¿‚S‚q‚v   =
038720                     “d—Ã—¿‚S‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038730             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚S‚m‚q‚v
038740         ELSE
038750             COMPUTE “d—Ã‰ñ”‚SSk‚q‚v = “d—Ã‰ñ”‚SSk‚q‚v + 1
038760             COMPUTE “d—Ã—¿‚SSk‚q‚v   =
038770                     “d—Ã—¿‚SSk‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038780             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚SSk‚m‚q‚v
038790         END-IF
038800     WHEN 5
038810         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
038820            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
038830             COMPUTE “d—Ã‰ñ”‚T‚q‚v = “d—Ã‰ñ”‚T‚q‚v + 1
038840             COMPUTE “d—Ã—¿‚T‚q‚v   =
038850                     “d—Ã—¿‚T‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038860             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚T‚m‚q‚v
038870         ELSE
038880             COMPUTE “d—Ã‰ñ”‚TSk‚q‚v = “d—Ã‰ñ”‚TSk‚q‚v + 1
038890             COMPUTE “d—Ã—¿‚TSk‚q‚v   =
038900                     “d—Ã—¿‚TSk‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038910             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚TSk‚m‚q‚v
038920         END-IF
038930     WHEN 6
038940         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
038950            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
038960             COMPUTE “d—Ã‰ñ”‚U‚q‚v = “d—Ã‰ñ”‚U‚q‚v + 1
038970             COMPUTE “d—Ã—¿‚U‚q‚v   =
038980                     “d—Ã—¿‚U‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
038990             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚U‚m‚q‚v
039000         ELSE
039010             COMPUTE “d—Ã‰ñ”‚USk‚q‚v = “d—Ã‰ñ”‚USk‚q‚v + 1
039020             COMPUTE “d—Ã—¿‚USk‚q‚v   =
039030                     “d—Ã—¿‚USk‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
039040             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚USk‚m‚q‚v
039050         END-IF
039060     WHEN 7
039070         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
039080            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
039090             COMPUTE “d—Ã‰ñ”‚V‚q‚v = “d—Ã‰ñ”‚V‚q‚v + 1
039100             COMPUTE “d—Ã—¿‚V‚q‚v   =
039110                     “d—Ã—¿‚V‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
039120             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚V‚m‚q‚v
039130         ELSE
039140             COMPUTE “d—Ã‰ñ”‚VSk‚q‚v = “d—Ã‰ñ”‚VSk‚q‚v + 1
039150             COMPUTE “d—Ã—¿‚VSk‚q‚v   =
039160                     “d—Ã—¿‚VSk‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
039170             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚VSk‚m‚q‚v
039180         END-IF
039190     WHEN 8
039200         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
039210            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
039220             COMPUTE “d—Ã‰ñ”‚W‚q‚v = “d—Ã‰ñ”‚W‚q‚v + 1
039230             COMPUTE “d—Ã—¿‚W‚q‚v   =
039240                     “d—Ã—¿‚W‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
039250             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚W‚m‚q‚v 
039260         ELSE
039270             COMPUTE “d—Ã‰ñ”‚WSk‚q‚v = “d—Ã‰ñ”‚WSk‚q‚v + 1
039280             COMPUTE “d—Ã—¿‚WSk‚q‚v   =
039290                     “d—Ã—¿‚WSk‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
039300             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚WSk‚m‚q‚v
039310         END-IF
039320     WHEN 9
039330         IF (’·Šúƒtƒ‰ƒO = SPACE) AND
039340            (•”ˆÊƒJƒEƒ“ƒ^ < ‘½•”ˆÊ’üŒ¸•”ˆÊ”‚j‚v)
039350             COMPUTE “d—Ã‰ñ”‚X‚q‚v = “d—Ã‰ñ”‚X‚q‚v + 1
039360             COMPUTE “d—Ã—¿‚X‚q‚v   =
039370                     “d—Ã—¿‚X‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
039380             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚X‚m‚q‚v
039390         ELSE
039400             COMPUTE “d—Ã‰ñ”‚XSk‚q‚v = “d—Ã‰ñ”‚XSk‚q‚v + 1
039410             COMPUTE “d—Ã—¿‚XSk‚q‚v   =
039420                     “d—Ã—¿‚XSk‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
039430             MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚XSk‚m‚q‚v
039440         END-IF
039450     WHEN OTHER
039460         CONTINUE
039470     END-EVALUATE.
039480*
039490*================================================================*
039500 —ÝŒv€–Ú‰Šú‰» SECTION.
039510*
039520     PERFORM VARYING •”ˆÊ‚b‚m‚s FROM 1 BY 1
039530            UNTIL   •”ˆÊ‚b‚m‚s > Å‘å“o˜^•”ˆÊ”‚v
039540         MOVE ZERO TO ‰‰ñˆ’u—¿‚n‚v(•”ˆÊ‚b‚m‚s)
039550*/˜JÐ
039560         MOVE ZERO TO Þ—¿”ï‚Q‚v    (•”ˆÊ‚b‚m‚s)
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚ÌŒðŠ·‚ÍA‚Q‰ñ‚Ü‚ÅŽZ’è‚Å‚«‚éB/20180830
039560         MOVE ZERO TO Þ—¿‰ñ”‚v    (•”ˆÊ‚b‚m‚s)
039560         MOVE ZERO TO Þ—¿”ï‚R‚v    (•”ˆÊ‚b‚m‚s)
039570     END-PERFORM.
039580*
039590     MOVE ZERO TO ‰ŒŸ—¿‚f‚v ‰ŒŸ‰ÁŽZ—¿‚f‚v
039600                  ÄŒŸ—¿‚f‚v ‰—Ã—¿‚f‚v ‰—Ã‰ÁŽZ—¿‚f‚v ‰—Ã‰ñ”‚f‚v
039610                  Œã—Ã—¿‚f‚v —âãª–@—¿‚f‚v ‰·ãª–@—¿‚f‚v “d—Ã—¿‚f‚v
039620                  ‹à‘®•›Žq—¿‚f‚v
039630                  ‰‰ñˆ’u—¿‚f‚v Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v
039640                  ”ï—pŠz˜Vl‚f‚v •‰’SŠz˜Vl‚f‚v •‰’SŠz•¬‚f‚v •‰’SŠz‚f‚v
039650                  ‰ŒŸŽž‘Š’k—¿‚f‚v.
039660*
039670     MOVE ZERO TO Œã—Ã—¿‚P‚q‚v Œã—Ã—¿‚Q‚q‚v
039680                  Œã—Ã—¿‚R‚q‚v Œã—Ã—¿‚S‚q‚v
039690                  Œã—Ã—¿‚T‚q‚v Œã—Ã—¿‚U‚q‚v
039700                  Œã—Ã—¿‚V‚q‚v Œã—Ã—¿‚W‚q‚v
039710                  Œã—Ã—¿‚X‚q‚v.
039720*Sk—p
039730     MOVE ZERO TO Œã—Ã—¿‚PSk‚q‚v Œã—Ã—¿‚QSk‚q‚v
039740                  Œã—Ã—¿‚RSk‚q‚v Œã—Ã—¿‚SSk‚q‚v 
039750                  Œã—Ã—¿‚TSk‚q‚v Œã—Ã—¿‚USk‚q‚v
039760                  Œã—Ã—¿‚VSk‚q‚v Œã—Ã—¿‚WSk‚q‚v
039770                  Œã—Ã—¿‚XSk‚q‚v.
039780*
039790     MOVE ZERO TO —âãª–@—¿‚P‚q‚v —âãª–@—¿‚Q‚q‚v —âãª–@—¿‚R‚q‚v
039800                  —âãª–@—¿‚S‚q‚v —âãª–@—¿‚T‚q‚v —âãª–@—¿‚U‚q‚v
039810                  —âãª–@—¿‚V‚q‚v —âãª–@—¿‚W‚q‚v —âãª–@—¿‚X‚q‚v.
039820*
039830     MOVE ZERO TO ‰·ãª–@—¿‚P‚q‚v ‰·ãª–@—¿‚Q‚q‚v ‰·ãª–@—¿‚R‚q‚v
039840                  ‰·ãª–@—¿‚S‚q‚v ‰·ãª–@—¿‚T‚q‚v ‰·ãª–@—¿‚U‚q‚v
039850                  ‰·ãª–@—¿‚V‚q‚v ‰·ãª–@—¿‚W‚q‚v ‰·ãª–@—¿‚X‚q‚v.
039860*
039870     MOVE ZERO TO “d—Ã—¿‚P‚q‚v “d—Ã—¿‚Q‚q‚v “d—Ã—¿‚R‚q‚v
039880                  “d—Ã—¿‚S‚q‚v “d—Ã—¿‚T‚q‚v “d—Ã—¿‚U‚q‚v
039890                  “d—Ã—¿‚V‚q‚v “d—Ã—¿‚W‚q‚v “d—Ã—¿‚X‚q‚v.
039900*
039910     MOVE ZERO TO Œã—Ã‰ñ”‚P‚q‚v Œã—Ã‰ñ”‚Q‚q‚v Œã—Ã‰ñ”‚R‚q‚v
039920                  Œã—Ã‰ñ”‚S‚q‚v Œã—Ã‰ñ”‚T‚q‚v Œã—Ã‰ñ”‚U‚q‚v
039930                  Œã—Ã‰ñ”‚V‚q‚v Œã—Ã‰ñ”‚W‚q‚v Œã—Ã‰ñ”‚X‚q‚v.
039940*
039950*/Sk—p
039960     MOVE ZERO TO Œã—Ã‰ñ”‚PSk‚q‚v   Œã—Ã—¿‚PSk‚q‚v
039970                  —âãª–@‰ñ”‚PSk‚q‚v —âãª–@—¿‚PSk‚q‚v
039980                  ‰·ãª–@‰ñ”‚PSk‚q‚v ‰·ãª–@—¿‚PSk‚q‚v
039990                  “d—Ã‰ñ”‚PSk‚q‚v   “d—Ã—¿‚PSk‚q‚v
040000                  ¬Œv‚PSk‚q‚v.
040010*
040020     MOVE ZERO TO Œã—Ã‰ñ”‚QSk‚q‚v   Œã—Ã—¿‚QSk‚q‚v
040030                  —âãª–@‰ñ”‚QSk‚q‚v —âãª–@—¿‚QSk‚q‚v
040040                  ‰·ãª–@‰ñ”‚QSk‚q‚v ‰·ãª–@—¿‚QSk‚q‚v
040050                  “d—Ã‰ñ”‚QSk‚q‚v   “d—Ã—¿‚QSk‚q‚v
040060                  ¬Œv‚QSk‚q‚v.
040070*
040080     MOVE ZERO TO Œã—Ã‰ñ”‚RSk‚q‚v   Œã—Ã—¿‚RSk‚q‚v
040090                  —âãª–@‰ñ”‚RSk‚q‚v —âãª–@—¿‚RSk‚q‚v
040100                  ‰·ãª–@‰ñ”‚RSk‚q‚v ‰·ãª–@—¿‚RSk‚q‚v
040110                  “d—Ã‰ñ”‚RSk‚q‚v   “d—Ã—¿‚RSk‚q‚v
040120                  ¬Œv‚RSk‚q‚v.
040130*
040140     MOVE ZERO TO Œã—Ã‰ñ”‚SSk‚q‚v   Œã—Ã—¿‚SSk‚q‚v
040150                  —âãª–@‰ñ”‚SSk‚q‚v —âãª–@—¿‚SSk‚q‚v
040160                  ‰·ãª–@‰ñ”‚SSk‚q‚v ‰·ãª–@—¿‚SSk‚q‚v
040170                  “d—Ã‰ñ”‚SSk‚q‚v   “d—Ã—¿‚SSk‚q‚v
040180                  ¬Œv‚SSk‚q‚v.
040190*
040200     MOVE ZERO TO Œã—Ã‰ñ”‚TSk‚q‚v   Œã—Ã—¿‚TSk‚q‚v
040210                  —âãª–@‰ñ”‚TSk‚q‚v —âãª–@—¿‚TSk‚q‚v
040220                  ‰·ãª–@‰ñ”‚TSk‚q‚v ‰·ãª–@—¿‚TSk‚q‚v
040230                  “d—Ã‰ñ”‚TSk‚q‚v   “d—Ã—¿‚TSk‚q‚v
040240                  ¬Œv‚TSk‚q‚v.
040250*
040260     MOVE ZERO TO Œã—Ã‰ñ”‚USk‚q‚v   Œã—Ã—¿‚USk‚q‚v
040270                  —âãª–@‰ñ”‚USk‚q‚v —âãª–@—¿‚USk‚q‚v
040280                  ‰·ãª–@‰ñ”‚USk‚q‚v ‰·ãª–@—¿‚USk‚q‚v
040290                  “d—Ã‰ñ”‚USk‚q‚v   “d—Ã—¿‚USk‚q‚v
040300                  ¬Œv‚USk‚q‚v.
040310*
040320     MOVE ZERO TO Œã—Ã‰ñ”‚VSk‚q‚v   Œã—Ã—¿‚VSk‚q‚v
040330                  —âãª–@‰ñ”‚VSk‚q‚v —âãª–@—¿‚VSk‚q‚v
040340                  ‰·ãª–@‰ñ”‚VSk‚q‚v ‰·ãª–@—¿‚VSk‚q‚v
040350                  “d—Ã‰ñ”‚VSk‚q‚v   “d—Ã—¿‚VSk‚q‚v
040360                  ¬Œv‚VSk‚q‚v.
040370*
040380     MOVE ZERO TO Œã—Ã‰ñ”‚WSk‚q‚v   Œã—Ã—¿‚WSk‚q‚v
040390                  —âãª–@‰ñ”‚WSk‚q‚v —âãª–@—¿‚WSk‚q‚v
040400                  ‰·ãª–@‰ñ”‚WSk‚q‚v ‰·ãª–@—¿‚WSk‚q‚v
040410                  “d—Ã‰ñ”‚WSk‚q‚v   “d—Ã—¿‚WSk‚q‚v
040420                  ¬Œv‚WSk‚q‚v.
040430*
040440     MOVE ZERO TO Œã—Ã‰ñ”‚XSk‚q‚v   Œã—Ã—¿‚XSk‚q‚v
040450                  —âãª–@‰ñ”‚XSk‚q‚v —âãª–@—¿‚XSk‚q‚v
040460                  ‰·ãª–@‰ñ”‚XSk‚q‚v ‰·ãª–@—¿‚XSk‚q‚v
040470                  “d—Ã‰ñ”‚XSk‚q‚v   “d—Ã—¿‚XSk‚q‚v
040480                  ¬Œv‚XSk‚q‚v.
040490*
040500     MOVE ZERO TO —âãª–@‰ñ”‚P‚q‚v —âãª–@‰ñ”‚Q‚q‚v —âãª–@‰ñ”‚R‚q‚v
040510                  —âãª–@‰ñ”‚S‚q‚v —âãª–@‰ñ”‚T‚q‚v —âãª–@‰ñ”‚U‚q‚v
040520                  —âãª–@‰ñ”‚V‚q‚v —âãª–@‰ñ”‚W‚q‚v —âãª–@‰ñ”‚X‚q‚v.
040530*
040540     MOVE ZERO TO ‰·ãª–@‰ñ”‚P‚q‚v ‰·ãª–@‰ñ”‚Q‚q‚v ‰·ãª–@‰ñ”‚R‚q‚v
040550                  ‰·ãª–@‰ñ”‚S‚q‚v ‰·ãª–@‰ñ”‚T‚q‚v ‰·ãª–@‰ñ”‚U‚q‚v
040560                  ‰·ãª–@‰ñ”‚V‚q‚v ‰·ãª–@‰ñ”‚W‚q‚v ‰·ãª–@‰ñ”‚X‚q‚v.
040570*
040580     MOVE ZERO TO “d—Ã‰ñ”‚P‚q‚v “d—Ã‰ñ”‚Q‚q‚v “d—Ã‰ñ”‚R‚q‚v
040590                  “d—Ã‰ñ”‚S‚q‚v “d—Ã‰ñ”‚T‚q‚v “d—Ã‰ñ”‚U‚q‚v
040600                  “d—Ã‰ñ”‚V‚q‚v “d—Ã‰ñ”‚W‚q‚v “d—Ã‰ñ”‚X‚q‚v.
040610*
040620     MOVE ZERO TO ‰ŒŸŽžŠÔŠO‚v ‰ŒŸ‹x“ú‚v   ‰ŒŸ[–é‚v 
040630                  ‰—Ã–éŠÔ‚v   ‰—Ã“ï˜H‚v   ‰—Ã–\•—‰J‚v ‰—ÃŽžŠÔŠO‚v
040640                  ‹à‘®•›Žq‘å‚v ‹à‘®•›Žq’†‚v ‹à‘®•›Žq¬‚v.
040650*
040660*/˜JÐê—pƒ[ƒN‚Ì‰Šú‰»
040670     MOVE ZERO TO ‰ŒŸ‰ñ”‚v      ÄŒŸ‰ñ”‚v      Žw“±ŠÇ—‰ñ”‚v
040680                  ‰^“®—Ã–@‰ñ”‚v  ‹x‹ÆØ–¾‰ñ”‚v  î•ñ’ñ‹Ÿ‰ñ”‚v
040690                  h”‘‰ñ”‚v      HŽ–‰ñ”‚v      ‰—Ã—¿‚v‚Q.
040700*Ž©”…Óê—p‚Ìƒ[ƒN‚Ì‰Šú‰»
040710     MOVE ZERO TO Ž{pØ–¾‘—¿‚v  Ž{pØ–¾‰ñ”‚v
040720                  Ž{p–¾×‘—¿‚v  Ž{p–¾×‰ñ”‚v ‚»‚Ì‘¼‹àŠz‚f‚v.
040730*
040740* ‘½•”ˆÊ
040750     MOVE ZERO TO ‘½•”ˆÊŒã—Ã‰ñ”‚P‚q‚v ‘½•”ˆÊŒã—Ã—¿‚P‚q‚v
040760                  ‘½•”ˆÊŒã—Ã‰ñ”‚Q‚q‚v ‘½•”ˆÊŒã—Ã—¿‚Q‚q‚v
040770                  ‘½•”ˆÊŒã—Ã‰ñ”‚R‚q‚v ‘½•”ˆÊŒã—Ã—¿‚R‚q‚v
040780                  ‘½•”ˆÊŒã—Ã‰ñ”‚S‚q‚v ‘½•”ˆÊŒã—Ã—¿‚S‚q‚v
040790                  ‘½•”ˆÊŒã—Ã‰ñ”‚T‚q‚v ‘½•”ˆÊŒã—Ã—¿‚T‚q‚v
040800                  ‘½•”ˆÊŒã—Ã‰ñ”‚U‚q‚v ‘½•”ˆÊŒã—Ã—¿‚U‚q‚v
040810                  ‘½•”ˆÊŒã—Ã‰ñ”‚V‚q‚v ‘½•”ˆÊŒã—Ã—¿‚V‚q‚v
040820                  ‘½•”ˆÊŒã—Ã‰ñ”‚W‚q‚v ‘½•”ˆÊŒã—Ã—¿‚W‚q‚v
040830                  ‘½•”ˆÊŒã—Ã‰ñ”‚X‚q‚v ‘½•”ˆÊŒã—Ã—¿‚X‚q‚v.
040840* ’·Šú
040850     MOVE ZERO TO ’·ŠúŒã—Ã‰ñ”‚P‚q‚v   ’·ŠúŒã—Ã—¿‚P‚q‚v
040860                  ’·ŠúŒã—Ã‰ñ”‚Q‚q‚v   ’·ŠúŒã—Ã—¿‚Q‚q‚v
040870                  ’·ŠúŒã—Ã‰ñ”‚R‚q‚v   ’·ŠúŒã—Ã—¿‚R‚q‚v
040880                  ’·ŠúŒã—Ã‰ñ”‚S‚q‚v   ’·ŠúŒã—Ã—¿‚S‚q‚v
040890                  ’·ŠúŒã—Ã‰ñ”‚T‚q‚v   ’·ŠúŒã—Ã—¿‚T‚q‚v
040900                  ’·ŠúŒã—Ã‰ñ”‚U‚q‚v   ’·ŠúŒã—Ã—¿‚U‚q‚v
040910                  ’·ŠúŒã—Ã‰ñ”‚V‚q‚v   ’·ŠúŒã—Ã—¿‚V‚q‚v
040920                  ’·ŠúŒã—Ã‰ñ”‚W‚q‚v   ’·ŠúŒã—Ã—¿‚W‚q‚v
040930                  ’·ŠúŒã—Ã‰ñ”‚X‚q‚v   ’·ŠúŒã—Ã—¿‚X‚q‚v.
040940*‘½•”ˆÊSk
040950     MOVE ZERO TO ‘½•”ˆÊSkŒã—Ã‰ñ”‚P‚q‚v ‘½•”ˆÊSkŒã—Ã—¿‚P‚q‚v
040960                  ‘½•”ˆÊSkŒã—Ã‰ñ”‚Q‚q‚v ‘½•”ˆÊSkŒã—Ã—¿‚Q‚q‚v
040970                  ‘½•”ˆÊSkŒã—Ã‰ñ”‚R‚q‚v ‘½•”ˆÊSkŒã—Ã—¿‚R‚q‚v
040980                  ‘½•”ˆÊSkŒã—Ã‰ñ”‚S‚q‚v ‘½•”ˆÊSkŒã—Ã—¿‚S‚q‚v
040990                  ‘½•”ˆÊSkŒã—Ã‰ñ”‚T‚q‚v ‘½•”ˆÊSkŒã—Ã—¿‚T‚q‚v
041000                  ‘½•”ˆÊSkŒã—Ã‰ñ”‚U‚q‚v ‘½•”ˆÊSkŒã—Ã—¿‚U‚q‚v
041010                  ‘½•”ˆÊSkŒã—Ã‰ñ”‚V‚q‚v ‘½•”ˆÊSkŒã—Ã—¿‚V‚q‚v
041020                  ‘½•”ˆÊSkŒã—Ã‰ñ”‚W‚q‚v ‘½•”ˆÊSkŒã—Ã—¿‚W‚q‚v
041030                  ‘½•”ˆÊSkŒã—Ã‰ñ”‚X‚q‚v ‘½•”ˆÊSkŒã—Ã—¿‚X‚q‚v.
041040*================================================================*
041050 Œ’•Û•‰’SŠzŒvŽZ SECTION.
041060*
041070****************************************************************
041080* •‰’S‹àŒvŽZ•û–@‚ÌŽw’è  •‰’S‹àŒvŽZ‹æ•ª  ”ï—pE¿‹Šz   •‰’SŠz  *
041090* @@@@@@@@@@@@@@‚O@@@@@‚P‰~’PˆÊ    ‚P‰~’PˆÊ *
041100*              @@@@ @@@‚P@@@@‚P‚O‰~’PˆÊ             *
041110*              @@@@@@@ ‚Q@@@@@‚P‰~’PˆÊ  ‚P‚O‰~’PˆÊ *
041120****************************************************************
041130*     MOVE ZERO TO ˜AŒv|•‰’S‹àŒvŽZ‹æ•ª
041140*
041150     CALL •‰’SŠz‚o‚f‚v
041160     IF PROGRAM-STATUS NOT = ZERO
041170         CANCEL •‰’SŠz‚o‚f‚v
041180*
041190     ELSE
041200         CANCEL •‰’SŠz‚o‚f‚v
041210*/ƒGƒ‰[•\Ž¦‚ÌC³
041220         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
041230         DISPLAY "•‰’SŠzŽæ“¾‚ÉŽ¸”s‚µ‚Ü‚µ‚½"
041240                 " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
041250                 " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
041260*-----------------------------------------*
041270         CALL "actcshm"  WITH C LINKAGE
041280*-----------------------------------------*
041290         ACCEPT  ƒL[“ü—Í FROM CONS
041300         PERFORM ƒtƒ@ƒCƒ‹•Â½
041310         EXIT PROGRAM
041320     END-IF.
041330*================================================================*
041340 ˜AŒ‹€–ÚƒZƒbƒg SECTION.
041350*
041360* ”ï—pŠzE•‰’SŠzE¿‹Šz‚ÍAŽåˆ—‚ÉƒZƒbƒg
041370*
041380* ‰Šú‰»ƒ‹[ƒ`ƒ“‚É‚Ä‰Šú‰»Ï
041390*
041400*/˜JÐ—p‚ÉV‚½‚É‰Á‚¦‚½€–Ú
041410     IF ‰ŒŸ‰ñ”‚v > ZERO
041420         MOVE ‰ŒŸ‰ñ”‚v     TO ˜A—¿˜JŽ©|‰ŒŸ‰ñ”
041430         MOVE ‰ŒŸ—¿‚d‚v     TO ˜A—¿˜JŽ©|‰ŒŸ’P‰¿
041440     END-IF.
041450     IF ÄŒŸ‰ñ”‚v > ZERO
041460         MOVE ÄŒŸ‰ñ”‚v     TO ˜A—¿˜JŽ©|ÄŒŸ‰ñ”
041470         MOVE ÄŒŸ—¿‚d‚v     TO ˜A—¿˜JŽ©|ÄŒŸ’P‰¿
041480     END-IF.
041490*
041500     MOVE ‰ŒŸ—¿‚f‚v     TO ˜A—¿˜JŽ©|‰ŒŸ—¿.
041510     MOVE ‰ŒŸ‰ÁŽZ—¿‚f‚v TO ˜A—¿˜JŽ©|‰ŒŸ‰ÁŽZ—¿.
041520     MOVE ÄŒŸ—¿‚f‚v     TO ˜A—¿˜JŽ©|ÄŒŸ—¿.
041530     MOVE ‰—Ã‹——£‚v     TO ˜A—¿˜JŽ©|‰—Ã‹——£.
041540     MOVE ‰—Ã‰ñ”‚f‚v   TO ˜A—¿˜JŽ©|‰—Ã‰ñ”.
041550*/‰—Ã’P‰¿‚ÍÅŒã‚É‰—Ã—¿‚ðŽZ’è‚µ‚½“ú‚Ì’P‰¿
041560     MOVE ‰—Ã—¿‚v‚Q     TO ˜A—¿˜JŽ©|‰—Ã’P‰¿.
041570     MOVE ‰—Ã—¿‚f‚v     TO ˜A—¿˜JŽ©|‰—Ã—¿.
041580     MOVE ‰—Ã‰ÁŽZ—¿‚f‚v TO ˜A—¿˜JŽ©|‰—Ã‰ÁŽZ—¿.
041590     MOVE ‹à‘®•›Žq—¿‚f‚v TO ˜A—¿˜JŽ©|‹à‘®•›Žq‰ÁŽZ—¿.
041600     MOVE ‰ŒŸŽž‘Š’k—¿‚f‚v TO ˜A—¿˜JŽ©|‰ŒŸŽž‘Š’k—¿.
041610*
041620     IF î•ñ’ñ‹Ÿ‰ñ”‚v > ZERO
041630         MOVE î•ñ’ñ‹Ÿ‰ñ”‚v     TO ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ‰ñ”
041640         MOVE Ž{pî•ñ’ñ‹Ÿ—¿‚d‚v TO ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ’P‰¿
041650         MOVE Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v TO ˜A—¿˜JŽ©|Ž{pî•ñ’ñ‹Ÿ—¿
041660     END-IF.
041670*
041680     MOVE ‰ŒŸŽžŠÔŠO‚v   TO ˜A—¿˜JŽ©|‰ŒŸŽžŠÔŠO.
041690     MOVE ‰ŒŸ‹x“ú‚v     TO ˜A—¿˜JŽ©|‰ŒŸ‹x“ú.  
041700     MOVE ‰ŒŸ[–é‚v     TO ˜A—¿˜JŽ©|‰ŒŸ[–é.  
041710     MOVE ‰—Ã–éŠÔ‚v     TO ˜A—¿˜JŽ©|‰—Ã–éŠÔ.  
041720     MOVE ‰—Ã“ï˜H‚v     TO ˜A—¿˜JŽ©|‰—Ã“ï˜H.  
041730     MOVE ‰—Ã–\•—‰J‚v   TO ˜A—¿˜JŽ©|‰—Ã–\•—‰J.
041740     MOVE ‰—ÃŽžŠÔŠO‚v   TO ˜A—¿˜JŽ©|‰—ÃŽžŠÔŠO.
041750*
041760     MOVE ‹à‘®•›Žq‘å‚v   TO ˜A—¿˜JŽ©|‘å.
041770     MOVE ‹à‘®•›Žq’†‚v   TO ˜A—¿˜JŽ©|’†.
041780     MOVE ‹à‘®•›Žq¬‚v   TO ˜A—¿˜JŽ©|¬.
041790*
041800     COMPUTE ˜A—¿˜JŽ©|¬Œv = ‰ŒŸ—¿‚f‚v + ‰ŒŸ‰ÁŽZ—¿‚f‚v +
041810                          ÄŒŸ—¿‚f‚v + ‰—Ã—¿‚f‚v +
041820                          ‰—Ã‰ÁŽZ—¿‚f‚v + ‹à‘®•›Žq—¿‚f‚v +
041830                          Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v +
041840                          ‰ŒŸŽž‘Š’k—¿‚f‚v.
041850*
041860     IF Žw“±ŠÇ—‰ñ”‚v > ZERO
041870         MOVE Žw“±ŠÇ—‰ñ”‚v   TO ˜A—¿˜JŽ©|Žw“±ŠÇ—‰ñ”
041880         MOVE Žw“±ŠÇ——¿‚d‚v   TO ˜A—¿˜JŽ©|Žw“±ŠÇ—’P‰¿
041890         MOVE Žw“±ŠÇ——¿‚f‚v   TO ˜A—¿˜JŽ©|Žw“±ŠÇ——¿
041900     END-IF.
041910*
041920     IF ‰^“®—Ã–@‰ñ”‚v > ZERO
041930         MOVE ‰^“®—Ã–@‰ñ”‚v   TO ˜A—¿˜JŽ©|‰^“®—Ã–@‰ñ”
041940         MOVE ‰^“®—Ã–@—¿‚d‚v   TO ˜A—¿˜JŽ©|‰^“®—Ã–@’P‰¿
041950         MOVE ‰^“®—Ã–@—¿‚f‚v   TO ˜A—¿˜JŽ©|‰^“®—Ã–@—¿
041960     END-IF.
041970*
041980     IF ‹x‹ÆØ–¾‰ñ”‚v > ZERO
041990         MOVE ‹x‹ÆØ–¾‰ñ”‚v   TO ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‰ñ”
042000         MOVE ‹x‹ÆØ–¾‘—¿‚d‚v TO ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾’P‰¿
042010         MOVE ‹x‹ÆØ–¾‘—¿‚f‚v TO ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‘—¿
042020     END-IF.
042030*/Ž©”…Ó•ÛŒ¯—p
042040*
042050     IF Ž{pØ–¾‰ñ”‚v > ZERO
042060         MOVE Ž{pØ–¾‰ñ”‚v   TO ˜A—¿˜JŽ©|Ž{pØ–¾‰ñ”
042070         MOVE Ž{pØ–¾‘—¿‚d‚v TO ˜A—¿˜JŽ©|Ž{pØ–¾’P‰¿
042080         MOVE Ž{pØ–¾‘—¿‚f‚v TO ˜A—¿˜JŽ©|Ž{pØ–¾‘—¿
042090     END-IF.
042100*
042110     IF Ž{p–¾×‰ñ”‚v > ZERO
042120         MOVE Ž{p–¾×‰ñ”‚v   TO ˜A—¿˜JŽ©|Ž{p–¾×‰ñ”
042130         MOVE Ž{p–¾×‘—¿‚d‚v TO ˜A—¿˜JŽ©|Ž{p–¾×’P‰¿
042140         MOVE Ž{p–¾×‘—¿‚f‚v TO ˜A—¿˜JŽ©|Ž{p–¾×‘—¿
042150     END-IF.
042160*
042170     IF h”‘‰ñ”‚v > ZERO
042180         MOVE h”‘‰ñ”‚v       TO ˜A—¿˜JŽ©|h”‘‰ñ”
042190         MOVE h”‘—¿‚d‚v       TO ˜A—¿˜JŽ©|h”‘’P‰¿
042200         MOVE h”‘—¿‚f‚v       TO ˜A—¿˜JŽ©|h”‘—¿
042210     END-IF.
042220*
042230     IF HŽ–‰ñ”‚v > ZERO
042240         MOVE HŽ–‰ñ”‚v       TO ˜A—¿˜JŽ©|HŽ–‰ñ”
042250         MOVE HŽ–—¿‚d‚v       TO ˜A—¿˜JŽ©|HŽ–’P‰¿
042260         MOVE HŽ–—¿‚f‚v       TO ˜A—¿˜JŽ©|HŽ–—¿  
042270     END-IF.
042280*
042290     MOVE ‚»‚Ì‘¼‹àŠz‚f‚v       TO ˜A—¿˜JŽ©|Ž©”…Ó‚»‚Ì‘¼‹àŠz
042300*
042310     PERFORM VARYING •”ˆÊ‚b‚m‚s FROM 1 BY 1
042320             UNTIL   •”ˆÊ‚b‚m‚s > •”ˆÊ”‚v 
042330         MOVE ‰‰ñˆ’u—¿‚n‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|‰‰ñˆ’u—¿(•”ˆÊ‚b‚m‚s)
042340         COMPUTE ˜A—¿˜JŽ©|‰‰ñˆ’u—¿Œv = ˜A—¿˜JŽ©|‰‰ñˆ’u—¿Œv + ‰‰ñˆ’u—¿‚n‚v(•”ˆÊ‚b‚m‚s)
042350*/Þ—¿”ï
042360         MOVE Þ—¿”ï‚Q‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|Þ—¿”ï(•”ˆÊ‚b‚m‚s)
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚ÌŒðŠ·‚ÍA‚Q‰ñ‚Ü‚ÅŽZ’è‚Å‚«‚éB/20180830
026560         MOVE Þ—¿‰ñ”‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|Þ—¿”ï‰ñ”(•”ˆÊ‚b‚m‚s)
               MOVE Þ—¿”ï‚R‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|Þ—¿”ï’P‰¿(•”ˆÊ‚b‚m‚s)
042370*/•ï‘ÑŒðŠ·—¿
042380         MOVE •ï‘ÑŒðŠ·“ú‚P‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚P(•”ˆÊ‚b‚m‚s)
042390         MOVE •ï‘ÑŒðŠ·“ú‚Q‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚Q(•”ˆÊ‚b‚m‚s)
042400         MOVE •ï‘ÑŒðŠ·“ú‚R‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚R(•”ˆÊ‚b‚m‚s)
042410         MOVE •ï‘ÑŒðŠ·“ú‚S‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚S(•”ˆÊ‚b‚m‚s)
042420         MOVE •ï‘ÑŒðŠ·“ú‚T‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚T(•”ˆÊ‚b‚m‚s)
042430         MOVE •ï‘ÑŒðŠ·“ú‚U‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚U(•”ˆÊ‚b‚m‚s)
042440         MOVE •ï‘ÑŒðŠ·—¿‚v  (•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·—¿  (•”ˆÊ‚b‚m‚s)
042450
042460         MOVE •ï‘ÑŒðŠ·’P‰¿‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·’P‰¿(•”ˆÊ‚b‚m‚s)
042470         MOVE •ï‘ÑŒðŠ·‰ñ”‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·‰ñ”(•”ˆÊ‚b‚m‚s)
042480
042490     END-PERFORM.
042500*
042510**********
042520* ‚P•”ˆÊ *
042530**********
042540     IF •”ˆÊ‡Œv‚v‚P > ZERO
042550         IF Œã—Ã‰ñ”‚P‚q‚v > ZERO
042560*             MOVE Œã—Ã’P‰¿‚n‚v(1, 1) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚P
042570             MOVE Œã—Ã’P‰¿‚n‚v(1)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚P
042580             MOVE Œã—Ã‰ñ”‚P‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚P
042590             MOVE Œã—Ã—¿‚P‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚P
042600         END-IF
042610         MOVE —âãª–@’P‰¿‚v(1)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚P
042620         MOVE ‰·ãª–@’P‰¿‚v(1)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚P
042630         MOVE “d—Ã’P‰¿‚v(1)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚P
042640         MOVE —âãª–@‰ñ”‚P‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚P
042650         MOVE —âãª–@—¿‚P‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚P
042660         MOVE ‰·ãª–@‰ñ”‚P‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚P
042670         MOVE ‰·ãª–@—¿‚P‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚P
042680         MOVE “d—Ã‰ñ”‚P‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚P
042690         MOVE “d—Ã—¿‚P‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚P
042700*/1•”ˆÊ–ÚSk
042710         IF Œã—Ã‰ñ”‚PSk‚q‚v > ZERO
042720             MOVE SkŒã—Ã’P‰¿‚n‚v(1) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚PSk
042730             MOVE Œã—Ã‰ñ”‚PSk‚q‚v  TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚PSk
042740             MOVE Œã—Ã—¿‚PSk‚q‚v    TO ˜A—¿˜JŽ©|Œã—Ã—¿‚PSk
042750         END-IF
042760         IF (—âãª–@‰ñ”‚PSk‚q‚v > ZERO) OR
042770            (‰·ãª–@‰ñ”‚PSk‚q‚v > ZERO) 
      */“d—Ã—¿‚ª³‚µ‚­WŒv‚³‚ê‚È‚¢•s‹ï‡‚ÌC³/20201118
044000         OR (“d—Ã‰ñ”‚PSk‚q‚v   > ZERO)
042780             MOVE ‰·ãª–@’P‰¿‚v’üŒ¸(1)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚PSk
042790             MOVE —âãª–@’P‰¿‚v’üŒ¸(1)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚PSk
042800             MOVE “d—Ã’P‰¿‚v’üŒ¸  (1)    TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚PSk
042810             MOVE —âãª–@‰ñ”‚PSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚PSk
042820             MOVE —âãª–@—¿‚PSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚PSk
042830             MOVE ‰·ãª–@‰ñ”‚PSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚PSk
042840             MOVE ‰·ãª–@—¿‚PSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚PSk
042850             MOVE “d—Ã‰ñ”‚PSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚PSk
042860             MOVE “d—Ã—¿‚PSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚PSk
042870         END-IF
042880*******************************************************************
042890*/‚P•”ˆÊ–Ú‘½•”ˆÊ
042900         IF ‘½•”ˆÊŒã—Ã‰ñ”‚P‚q‚v > ZERO
042910             MOVE ‘½•”ˆÊŒã—Ã’P‰¿‚n‚v(1) TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚P
042920             MOVE ‘½•”ˆÊŒã—Ã‰ñ”‚P‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚P
042930             MOVE ‘½•”ˆÊŒã—Ã—¿‚P‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚P
042940         END-IF
042950*/‚P•”ˆÊ–Ú’·Šú
042960         IF ’·ŠúŒã—Ã‰ñ”‚P‚q‚v > ZERO
042970             MOVE ’·ŠúŒã—Ã’P‰¿‚n‚v(1) TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚P
042980             MOVE ’·ŠúŒã—Ã‰ñ”‚P‚q‚v  TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚P
042990             MOVE ’·ŠúŒã—Ã—¿‚P‚q‚v    TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚P
043000         END-IF
043010*/‚P•”ˆÊ–Ú‘½•”ˆÊSk
043020         IF ‘½•”ˆÊSkŒã—Ã‰ñ”‚P‚q‚v > ZERO
043030             MOVE ‘½•”ˆÊSkŒã—Ã’P‰¿‚n‚v(1) TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚P
043040             MOVE ‘½•”ˆÊSkŒã—Ã‰ñ”‚P‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚P
043050             MOVE ‘½•”ˆÊSkŒã—Ã—¿‚P‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚P
043060         END-IF
043070**********************************************************************
043080* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
043090         MOVE •”ˆÊ‡Œv‚v‚P       TO ˜A—¿˜JŽ©|¬Œv‚P
043100         MOVE •”ˆÊ‡Œv’·Šú‚v‚P   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚P
043110     END-IF.
043120**********
043130* ‚Q•”ˆÊ *
043140**********
043150     IF •”ˆÊ‡Œv‚v‚Q > ZERO
043160         IF Œã—Ã‰ñ”‚Q‚q‚v > ZERO
043170*             MOVE Œã—Ã’P‰¿‚n‚v(1, 2) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚Q
043180*             MOVE Œã—Ã’P‰¿‚n‚v(2, 2) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚Q
043190             MOVE Œã—Ã’P‰¿‚n‚v(2)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚Q
043200             MOVE Œã—Ã‰ñ”‚Q‚q‚v TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚Q
043210             MOVE Œã—Ã—¿‚Q‚q‚v   TO ˜A—¿˜JŽ©|Œã—Ã—¿‚Q
043220         END-IF
043230         MOVE —âãª–@’P‰¿‚v(2)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚Q
043240         MOVE ‰·ãª–@’P‰¿‚v(2)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚Q
043250         MOVE “d—Ã’P‰¿‚v(2)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚Q
043260         MOVE —âãª–@‰ñ”‚Q‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚Q
043270         MOVE —âãª–@—¿‚Q‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚Q
043280         MOVE ‰·ãª–@‰ñ”‚Q‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚Q
043290         MOVE ‰·ãª–@—¿‚Q‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚Q
043300         MOVE “d—Ã‰ñ”‚Q‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚Q
043310         MOVE “d—Ã—¿‚Q‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚Q
043320*/‚Q•”ˆÊ–ÚSk
043330         IF Œã—Ã‰ñ”‚QSk‚q‚v > ZERO
043340             MOVE SkŒã—Ã’P‰¿‚n‚v(2) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚QSk
043350             MOVE Œã—Ã‰ñ”‚QSk‚q‚v  TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚QSk
043360             MOVE Œã—Ã—¿‚QSk‚q‚v    TO ˜A—¿˜JŽ©|Œã—Ã—¿‚QSk
043370         END-IF
043380         IF (—âãª–@‰ñ”‚QSk‚q‚v > ZERO) OR
043390            (‰·ãª–@‰ñ”‚QSk‚q‚v > ZERO) 
      */“d—Ã—¿‚ª³‚µ‚­WŒv‚³‚ê‚È‚¢•s‹ï‡‚ÌC³/20201118
044000         OR (“d—Ã‰ñ”‚QSk‚q‚v   > ZERO)
043400             MOVE ‰·ãª–@’P‰¿‚v’üŒ¸(2)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚QSk
043410             MOVE —âãª–@’P‰¿‚v’üŒ¸(2)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚QSk
043420             MOVE “d—Ã’P‰¿‚v’üŒ¸  (2)    TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚QSk
043430             MOVE —âãª–@‰ñ”‚QSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚QSk
043440             MOVE —âãª–@—¿‚QSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚QSk
043450             MOVE ‰·ãª–@‰ñ”‚QSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚QSk
043460             MOVE ‰·ãª–@—¿‚QSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚QSk
043470             MOVE “d—Ã‰ñ”‚QSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚QSk
043480             MOVE “d—Ã—¿‚QSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚QSk
043490         END-IF
043500*******************************************************************
043510*/‚Q•”ˆÊ–Ú‘½•”ˆÊ
043520         IF ‘½•”ˆÊŒã—Ã‰ñ”‚Q‚q‚v > ZERO
043530             MOVE ‘½•”ˆÊŒã—Ã’P‰¿‚n‚v(2) TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚Q
043540             MOVE ‘½•”ˆÊŒã—Ã‰ñ”‚Q‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚Q
043550             MOVE ‘½•”ˆÊŒã—Ã—¿‚Q‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚Q
043560         END-IF
043570*/‚Q•”ˆÊ–Ú’·Šú
043580         IF ’·ŠúŒã—Ã‰ñ”‚Q‚q‚v > ZERO
043590             MOVE ’·ŠúŒã—Ã’P‰¿‚n‚v(2) TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚Q
043600             MOVE ’·ŠúŒã—Ã‰ñ”‚Q‚q‚v  TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚Q
043610             MOVE ’·ŠúŒã—Ã—¿‚Q‚q‚v    TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚Q
043620         END-IF
043630*/‚Q•”ˆÊ–Ú‘½•”ˆÊSk
043640         IF ‘½•”ˆÊSkŒã—Ã‰ñ”‚Q‚q‚v > ZERO
043650             MOVE ‘½•”ˆÊSkŒã—Ã’P‰¿‚n‚v(2) TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚Q
043660             MOVE ‘½•”ˆÊSkŒã—Ã‰ñ”‚Q‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚Q
043670             MOVE ‘½•”ˆÊSkŒã—Ã—¿‚Q‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚Q
043680         END-IF
043690**********************************************************************
043700* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
043710         MOVE •”ˆÊ‡Œv‚v‚Q       TO ˜A—¿˜JŽ©|¬Œv‚Q
043720         MOVE •”ˆÊ‡Œv’·Šú‚v‚Q   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚Q
043730     END-IF.
043740**********
043750* ‚R•”ˆÊ *
043760**********
043770     IF •”ˆÊ‡Œv‚v‚R > ZERO
043780         IF Œã—Ã‰ñ”‚R‚q‚v > ZERO
043790             MOVE Œã—Ã’P‰¿‚n‚v(3) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚R
043800             MOVE Œã—Ã‰ñ”‚R‚q‚v  TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚R
043810             MOVE Œã—Ã—¿‚R‚q‚v    TO ˜A—¿˜JŽ©|Œã—Ã—¿‚R
043820         END-IF
043830         MOVE —âãª–@’P‰¿‚v(3)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚R
043840         MOVE ‰·ãª–@’P‰¿‚v(3)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚R
043850         MOVE “d—Ã’P‰¿‚v(3)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚R
043860         MOVE —âãª–@‰ñ”‚R‚q‚v    TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚R
043870         MOVE —âãª–@—¿‚R‚q‚v      TO ˜A—¿˜JŽ©|—âãª–@—¿‚R
043880         MOVE ‰·ãª–@‰ñ”‚R‚q‚v    TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚R
043890         MOVE ‰·ãª–@—¿‚R‚q‚v      TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚R
043900         MOVE “d—Ã‰ñ”‚R‚q‚v      TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚R
043910         MOVE “d—Ã—¿‚R‚q‚v        TO ˜A—¿˜JŽ©|“d—Ã—¿‚R
043920*/‚R•”ˆÊ–ÚSk
043930         IF Œã—Ã‰ñ”‚RSk‚q‚v > ZERO
043940             MOVE SkŒã—Ã’P‰¿‚n‚v(3) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚RSk
043950             MOVE Œã—Ã‰ñ”‚RSk‚q‚v  TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚RSk
043960             MOVE Œã—Ã—¿‚RSk‚q‚v    TO ˜A—¿˜JŽ©|Œã—Ã—¿‚RSk
043970         END-IF
043980         IF (—âãª–@‰ñ”‚RSk‚q‚v > ZERO) OR
043990            (‰·ãª–@‰ñ”‚RSk‚q‚v > ZERO) OR
044000            (“d—Ã‰ñ”‚RSk‚q‚v   > ZERO)
044010             MOVE ‰·ãª–@’P‰¿‚v’üŒ¸(3)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚RSk
044020             MOVE —âãª–@’P‰¿‚v’üŒ¸(3)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚RSk
044030             MOVE “d—Ã’P‰¿‚v’üŒ¸  (3)    TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚RSk
044040             MOVE —âãª–@‰ñ”‚RSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚RSk
044050             MOVE —âãª–@—¿‚RSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚RSk
044060             MOVE ‰·ãª–@‰ñ”‚RSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚RSk
044070             MOVE ‰·ãª–@—¿‚RSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚RSk
044080             MOVE “d—Ã‰ñ”‚RSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚RSk
044090             MOVE “d—Ã—¿‚RSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚RSk
044100         END-IF
044110*******************************************************************
044120*/‚R•”ˆÊ–Ú‘½•”ˆÊ
044130         IF ‘½•”ˆÊŒã—Ã‰ñ”‚R‚q‚v > ZERO
044140             MOVE ‘½•”ˆÊŒã—Ã’P‰¿‚n‚v(3) TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚R
044150             MOVE ‘½•”ˆÊŒã—Ã‰ñ”‚R‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚R
044160             MOVE ‘½•”ˆÊŒã—Ã—¿‚R‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚R
044170         END-IF
044180*/‚R•”ˆÊ–Ú’·Šú
044190         IF ’·ŠúŒã—Ã‰ñ”‚R‚q‚v > ZERO
044200             MOVE ’·ŠúŒã—Ã’P‰¿‚n‚v(3) TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚R
044210             MOVE ’·ŠúŒã—Ã‰ñ”‚R‚q‚v  TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚R
044220             MOVE ’·ŠúŒã—Ã—¿‚R‚q‚v    TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚R
044230         END-IF
044240*/‚R•”ˆÊ–Ú‘½•”ˆÊSk
044250         IF ‘½•”ˆÊSkŒã—Ã‰ñ”‚R‚q‚v > ZERO
044260             MOVE ‘½•”ˆÊSkŒã—Ã’P‰¿‚n‚v(3) TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚R
044270             MOVE ‘½•”ˆÊSkŒã—Ã‰ñ”‚R‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚R
044280             MOVE ‘½•”ˆÊSkŒã—Ã—¿‚R‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚R
044290         END-IF
044300**********************************************************************
044310* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
044320         MOVE •”ˆÊ‡Œv‚v‚R       TO ˜A—¿˜JŽ©|¬Œv‚R
044330         MOVE •”ˆÊ‡Œv’·Šú‚v‚R   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚R
044340     END-IF.
044350**********
044360* ‚S•”ˆÊ *
044370**********
044380     IF •”ˆÊ‡Œv‚v‚S > ZERO
044390         IF Œã—Ã‰ñ”‚S‚q‚v > ZERO
044400             MOVE Œã—Ã’P‰¿‚n‚v(4)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚S
044410             MOVE Œã—Ã‰ñ”‚S‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚S
044420             MOVE Œã—Ã—¿‚S‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚S
044430         END-IF
044440         MOVE —âãª–@’P‰¿‚v(4)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚S
044450         MOVE ‰·ãª–@’P‰¿‚v(4)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚S
044460         MOVE “d—Ã’P‰¿‚v(4)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚S
044470         MOVE —âãª–@‰ñ”‚S‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚S
044480         MOVE —âãª–@—¿‚S‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚S
044490         MOVE ‰·ãª–@‰ñ”‚S‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚S
044500         MOVE ‰·ãª–@—¿‚S‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚S
044510         MOVE “d—Ã‰ñ”‚S‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚S
044520         MOVE “d—Ã—¿‚S‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚S
044530*/‚S•”ˆÊ–ÚSk
044540         IF Œã—Ã‰ñ”‚SSk‚q‚v > ZERO
044550             MOVE SkŒã—Ã’P‰¿‚n‚v(4) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚SSk
044560             MOVE Œã—Ã‰ñ”‚SSk‚q‚v  TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚SSk
044570             MOVE Œã—Ã—¿‚SSk‚q‚v    TO ˜A—¿˜JŽ©|Œã—Ã—¿‚SSk
044580         END-IF
044590         IF (—âãª–@‰ñ”‚SSk‚q‚v > ZERO) OR
044600            (‰·ãª–@‰ñ”‚SSk‚q‚v > ZERO) OR
044610            (“d—Ã‰ñ”‚SSk‚q‚v   > ZERO)
044620              MOVE ‰·ãª–@’P‰¿‚v’üŒ¸(4)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚SSk
044630              MOVE —âãª–@’P‰¿‚v’üŒ¸(4)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚SSk
044640              MOVE “d—Ã’P‰¿‚v’üŒ¸  (4)    TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚SSk
044650              MOVE —âãª–@‰ñ”‚SSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚SSk
044660              MOVE —âãª–@—¿‚SSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚SSk
044670              MOVE ‰·ãª–@‰ñ”‚SSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚SSk
044680              MOVE ‰·ãª–@—¿‚SSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚SSk
044690              MOVE “d—Ã‰ñ”‚SSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚SSk
044700              MOVE “d—Ã—¿‚SSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚SSk
044710         END-IF
044720*******************************************************************
044730*/‚S•”ˆÊ–Ú‘½•”ˆÊ
044740         IF ‘½•”ˆÊŒã—Ã‰ñ”‚S‚q‚v > ZERO
044750             MOVE ‘½•”ˆÊŒã—Ã’P‰¿‚n‚v(4) TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚S
044760             MOVE ‘½•”ˆÊŒã—Ã‰ñ”‚S‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚S
044770             MOVE ‘½•”ˆÊŒã—Ã—¿‚S‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚S
044780         END-IF
044790*/‚S•”ˆÊ–Ú’·Šú
044800         IF ’·ŠúŒã—Ã‰ñ”‚S‚q‚v > ZERO
044810             MOVE ’·ŠúŒã—Ã’P‰¿‚n‚v(4) TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚S
044820             MOVE ’·ŠúŒã—Ã‰ñ”‚S‚q‚v  TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚S
044830             MOVE ’·ŠúŒã—Ã—¿‚S‚q‚v    TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚S
044840         END-IF
044850*/‚S•”ˆÊ–Ú‘½•”ˆÊSk
044860         IF ‘½•”ˆÊSkŒã—Ã‰ñ”‚S‚q‚v > ZERO
044870             MOVE ‘½•”ˆÊSkŒã—Ã’P‰¿‚n‚v(4) TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚S
044880             MOVE ‘½•”ˆÊSkŒã—Ã‰ñ”‚S‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚S
044890             MOVE ‘½•”ˆÊSkŒã—Ã—¿‚S‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚S
044900         END-IF
044910**********************************************************************
044920* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
044930         MOVE •”ˆÊ‡Œv‚v‚S           TO ˜A—¿˜JŽ©|¬Œv‚S
044940         MOVE •”ˆÊ‡Œv’·Šú‚v‚S       TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚S
044950     END-IF.
044960**********
044970* ‚T•”ˆÊ *
044980**********
044990     IF •”ˆÊ‡Œv‚v‚T > ZERO
045000         IF Œã—Ã‰ñ”‚T‚q‚v > ZERO
045010             MOVE Œã—Ã’P‰¿‚n‚v(5)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚T
045020             MOVE Œã—Ã‰ñ”‚T‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚T
045030             MOVE Œã—Ã—¿‚T‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚T
045040         END-IF
045050         MOVE —âãª–@’P‰¿‚v(5)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚T
045060         MOVE ‰·ãª–@’P‰¿‚v(5)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚T
045070         MOVE “d—Ã’P‰¿‚v(5)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚T
045080         MOVE —âãª–@‰ñ”‚T‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚T
045090         MOVE —âãª–@—¿‚T‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚T
045100         MOVE ‰·ãª–@‰ñ”‚T‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚T
045110         MOVE ‰·ãª–@—¿‚T‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚T
045120         MOVE “d—Ã‰ñ”‚T‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚T
045130         MOVE “d—Ã—¿‚T‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚T
045140*/‚T•”ˆÊ–ÚSk
045150         IF Œã—Ã‰ñ”‚TSk‚q‚v > ZERO
045160             MOVE SkŒã—Ã’P‰¿‚n‚v(5)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚TSk
045170             MOVE Œã—Ã‰ñ”‚TSk‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚TSk
045180             MOVE Œã—Ã—¿‚TSk‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚TSk
045190         END-IF
045200         IF (—âãª–@‰ñ”‚TSk‚q‚v > ZERO) OR
045210            (‰·ãª–@‰ñ”‚TSk‚q‚v > ZERO) OR
045220            (“d—Ã‰ñ”‚TSk‚q‚v   > ZERO)
045230             MOVE ‰·ãª–@’P‰¿‚v’üŒ¸(5)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚TSk
045240             MOVE —âãª–@’P‰¿‚v’üŒ¸(5)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚TSk
045250             MOVE “d—Ã’P‰¿‚v’üŒ¸  (5)    TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚TSk
045260             MOVE —âãª–@‰ñ”‚TSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚TSk
045270             MOVE —âãª–@—¿‚TSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚TSk
045280             MOVE ‰·ãª–@‰ñ”‚TSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚TSk
045290             MOVE ‰·ãª–@—¿‚TSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚TSk
045300             MOVE “d—Ã‰ñ”‚TSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚TSk
045310             MOVE “d—Ã—¿‚TSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚TSk
045320         END-IF
045330*******************************************************************
045340*/‚T•”ˆÊ–Ú‘½•”ˆÊ
045350         IF ‘½•”ˆÊŒã—Ã‰ñ”‚T‚q‚v > ZERO
045360             MOVE ‘½•”ˆÊŒã—Ã’P‰¿‚n‚v(5) TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚T
045370             MOVE ‘½•”ˆÊŒã—Ã‰ñ”‚T‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚T
045380             MOVE ‘½•”ˆÊŒã—Ã—¿‚T‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚T
045390         END-IF
045400*/‚T•”ˆÊ–Ú’·Šú
045410         IF ’·ŠúŒã—Ã‰ñ”‚T‚q‚v > ZERO
045420             MOVE ’·ŠúŒã—Ã’P‰¿‚n‚v(5) TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚T
045430             MOVE ’·ŠúŒã—Ã‰ñ”‚T‚q‚v  TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚T
045440             MOVE ’·ŠúŒã—Ã—¿‚T‚q‚v    TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚T
045450         END-IF
045460*/‚T•”ˆÊ–Ú‘½•”ˆÊSk
045470         IF ‘½•”ˆÊSkŒã—Ã‰ñ”‚T‚q‚v > ZERO
045480             MOVE ‘½•”ˆÊSkŒã—Ã’P‰¿‚n‚v(5) TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚T
045490             MOVE ‘½•”ˆÊSkŒã—Ã‰ñ”‚T‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚T
045500             MOVE ‘½•”ˆÊSkŒã—Ã—¿‚T‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚T
045510         END-IF
045520**********************************************************************
045530* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
045540         MOVE •”ˆÊ‡Œv‚v‚T       TO ˜A—¿˜JŽ©|¬Œv‚T
045550         MOVE •”ˆÊ‡Œv’·Šú‚v‚T   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚T
045560**********
045570* ‚U•”ˆÊ *
045580**********
045590     IF •”ˆÊ‡Œv‚v‚U > ZERO
045600         IF Œã—Ã‰ñ”‚U‚q‚v > ZERO
045610             MOVE Œã—Ã’P‰¿‚n‚v(6)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚U
045620             MOVE Œã—Ã‰ñ”‚U‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚U
045630             MOVE Œã—Ã—¿‚U‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚U
045640         END-IF
045650         MOVE —âãª–@’P‰¿‚v(6)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚U
045660         MOVE ‰·ãª–@’P‰¿‚v(6)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚U
045670         MOVE “d—Ã’P‰¿‚v(6)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚U
045680         MOVE —âãª–@‰ñ”‚U‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚U
045690         MOVE —âãª–@—¿‚U‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚U
045700         MOVE ‰·ãª–@‰ñ”‚U‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚U
045710         MOVE ‰·ãª–@—¿‚U‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚U
045720         MOVE “d—Ã‰ñ”‚U‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚U
045730         MOVE “d—Ã—¿‚U‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚U
045740*/‚U•”ˆÊ–ÚSk
045750         IF Œã—Ã‰ñ”‚USk‚q‚v > ZERO
045760             MOVE SkŒã—Ã’P‰¿‚n‚v(6)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚USk
045770             MOVE Œã—Ã‰ñ”‚USk‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚USk
045780             MOVE Œã—Ã—¿‚USk‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚USk
045790         END-IF
045800         IF (—âãª–@‰ñ”‚USk‚q‚v > ZERO) OR
045810            (‰·ãª–@‰ñ”‚USk‚q‚v > ZERO) OR
045820            (“d—Ã‰ñ”‚USk‚q‚v   > ZERO)
045830             MOVE ‰·ãª–@’P‰¿‚v’üŒ¸(6)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚USk
045840             MOVE —âãª–@’P‰¿‚v’üŒ¸(6)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚USk
045850             MOVE “d—Ã’P‰¿‚v’üŒ¸  (6)    TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚USk
045860             MOVE —âãª–@‰ñ”‚USk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚USk
045870             MOVE —âãª–@—¿‚USk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚USk
045880             MOVE ‰·ãª–@‰ñ”‚USk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚USk
045890             MOVE ‰·ãª–@—¿‚USk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚USk
045900             MOVE “d—Ã‰ñ”‚USk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚USk
045910             MOVE “d—Ã—¿‚USk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚USk
045920         END-IF
045930*******************************************************************
045940*/‚U•”ˆÊ–Ú‘½•”ˆÊ
045950         IF ‘½•”ˆÊŒã—Ã‰ñ”‚U‚q‚v > ZERO
045960             MOVE ‘½•”ˆÊŒã—Ã’P‰¿‚n‚v(6) TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚U
045970             MOVE ‘½•”ˆÊŒã—Ã‰ñ”‚U‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚U
045980             MOVE ‘½•”ˆÊŒã—Ã—¿‚U‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚U
045990         END-IF
046000*/‚U•”ˆÊ–Ú’·Šú
046010         IF ’·ŠúŒã—Ã‰ñ”‚U‚q‚v > ZERO
046020             MOVE ’·ŠúŒã—Ã’P‰¿‚n‚v(6) TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚U
046030             MOVE ’·ŠúŒã—Ã‰ñ”‚U‚q‚v  TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚U
046040             MOVE ’·ŠúŒã—Ã—¿‚U‚q‚v    TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚U
046050         END-IF
046060*/‚U•”ˆÊ–Ú‘½•”ˆÊSk
046070         IF ‘½•”ˆÊSkŒã—Ã‰ñ”‚U‚q‚v > ZERO
046080             MOVE ‘½•”ˆÊSkŒã—Ã’P‰¿‚n‚v(6) TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚U
046090             MOVE ‘½•”ˆÊSkŒã—Ã‰ñ”‚U‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚U
046100             MOVE ‘½•”ˆÊSkŒã—Ã—¿‚U‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚U
046110         END-IF
046120**********************************************************************
046130* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
046140         MOVE •”ˆÊ‡Œv‚v‚U       TO ˜A—¿˜JŽ©|¬Œv‚U
046150         MOVE •”ˆÊ‡Œv’·Šú‚v‚U   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚U
046160**********
046170* ‚V•”ˆÊ *
046180**********
046190     IF •”ˆÊ‡Œv‚v‚V > ZERO
046200         IF Œã—Ã‰ñ”‚V‚q‚v > ZERO
046210             MOVE Œã—Ã’P‰¿‚n‚v(7)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚V
046220             MOVE Œã—Ã‰ñ”‚V‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚V
046230             MOVE Œã—Ã—¿‚V‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚V
046240         END-IF
046250         MOVE —âãª–@’P‰¿‚v(7)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚V
046260         MOVE ‰·ãª–@’P‰¿‚v(7)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚V
046270         MOVE “d—Ã’P‰¿‚v(7)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚V
046280         MOVE —âãª–@‰ñ”‚V‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚V
046290         MOVE —âãª–@—¿‚V‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚V
046300         MOVE ‰·ãª–@‰ñ”‚V‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚V
046310         MOVE ‰·ãª–@—¿‚V‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚V
046320         MOVE “d—Ã‰ñ”‚V‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚V
046330         MOVE “d—Ã—¿‚V‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚V
046340*/‚V•”ˆÊ–ÚSk
046350         IF Œã—Ã‰ñ”‚VSk‚q‚v > ZERO
046360             MOVE SkŒã—Ã’P‰¿‚n‚v(7)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚VSk
046370             MOVE Œã—Ã‰ñ”‚VSk‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚VSk
046380             MOVE Œã—Ã—¿‚VSk‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚VSk
046390         END-IF
046400         IF (—âãª–@‰ñ”‚VSk‚q‚v > ZERO) OR
046410            (‰·ãª–@‰ñ”‚VSk‚q‚v > ZERO) OR
046420            (“d—Ã‰ñ”‚VSk‚q‚v   > ZERO)
046430             MOVE ‰·ãª–@’P‰¿‚v’üŒ¸(7)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚VSk
046440             MOVE —âãª–@’P‰¿‚v’üŒ¸(7)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚VSk
046450             MOVE “d—Ã’P‰¿‚v’üŒ¸  (7)    TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚VSk
046460             MOVE —âãª–@‰ñ”‚VSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚VSk
046470             MOVE —âãª–@—¿‚VSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚VSk
046480             MOVE ‰·ãª–@‰ñ”‚VSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚VSk
046490             MOVE ‰·ãª–@—¿‚VSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚VSk
046500             MOVE “d—Ã‰ñ”‚VSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚VSk
046510             MOVE “d—Ã—¿‚VSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚VSk
046520         END-IF
046530*******************************************************************
046540*/‚V•”ˆÊ–Ú‘½•”ˆÊ
046550         IF ‘½•”ˆÊŒã—Ã‰ñ”‚V‚q‚v > ZERO
046560             MOVE ‘½•”ˆÊŒã—Ã’P‰¿‚n‚v(7) TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚V
046570             MOVE ‘½•”ˆÊŒã—Ã‰ñ”‚V‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚V
046580             MOVE ‘½•”ˆÊŒã—Ã—¿‚V‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚V
046590         END-IF
046600*/‚V•”ˆÊ–Ú’·Šú
046610         IF ’·ŠúŒã—Ã‰ñ”‚V‚q‚v > ZERO
046620             MOVE ’·ŠúŒã—Ã’P‰¿‚n‚v(7) TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚V
046630             MOVE ’·ŠúŒã—Ã‰ñ”‚V‚q‚v  TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚V
046640             MOVE ’·ŠúŒã—Ã—¿‚V‚q‚v    TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚V
046650         END-IF
046660*/‚V•”ˆÊ–Ú‘½•”ˆÊSk
046670         IF ‘½•”ˆÊSkŒã—Ã‰ñ”‚V‚q‚v > ZERO
046680             MOVE ‘½•”ˆÊSkŒã—Ã’P‰¿‚n‚v(7) TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚V
046690             MOVE ‘½•”ˆÊSkŒã—Ã‰ñ”‚V‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚V
046700             MOVE ‘½•”ˆÊSkŒã—Ã—¿‚V‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚V
046710         END-IF
046720**********************************************************************
046730* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
046740         MOVE •”ˆÊ‡Œv‚v‚U       TO ˜A—¿˜JŽ©|¬Œv‚U
046750         MOVE •”ˆÊ‡Œv’·Šú‚v‚U   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚U
046760     END-IF.
046770*
046780**********
046790* ‚W•”ˆÊ *
046800**********
046810     IF •”ˆÊ‡Œv‚v‚W > ZERO
046820         IF Œã—Ã‰ñ”‚W‚q‚v > ZERO
046830             MOVE Œã—Ã’P‰¿‚n‚v(8)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚W
046840             MOVE Œã—Ã‰ñ”‚W‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚W
046850             MOVE Œã—Ã—¿‚W‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚W
046860         END-IF
046870         MOVE —âãª–@’P‰¿‚v(8)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚W
046880         MOVE ‰·ãª–@’P‰¿‚v(8)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚W
046890         MOVE “d—Ã’P‰¿‚v(8)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚W
046900         MOVE —âãª–@‰ñ”‚W‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚W
046910         MOVE —âãª–@—¿‚W‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚W
046920         MOVE ‰·ãª–@‰ñ”‚W‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚W
046930         MOVE ‰·ãª–@—¿‚W‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚W
046940         MOVE “d—Ã‰ñ”‚W‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚W
046950         MOVE “d—Ã—¿‚W‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚W
046960*/‚W•”ˆÊ–ÚSk
046970         IF Œã—Ã‰ñ”‚WSk‚q‚v > ZERO
046980             MOVE SkŒã—Ã’P‰¿‚n‚v(8)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚WSk
046990             MOVE Œã—Ã‰ñ”‚WSk‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚WSk
047000             MOVE Œã—Ã—¿‚WSk‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚WSk
047010         END-IF
047020         IF (—âãª–@‰ñ”‚WSk‚q‚v > ZERO) OR
047030            (‰·ãª–@‰ñ”‚WSk‚q‚v > ZERO) OR
047040            (“d—Ã‰ñ”‚WSk‚q‚v   > ZERO)
047050             MOVE ‰·ãª–@’P‰¿‚v’üŒ¸(8)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚WSk
047060             MOVE —âãª–@’P‰¿‚v’üŒ¸(8)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚WSk
047070             MOVE “d—Ã’P‰¿‚v’üŒ¸  (8)    TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚WSk
047080             MOVE —âãª–@‰ñ”‚WSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚WSk
047090             MOVE —âãª–@—¿‚WSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚WSk
047100             MOVE ‰·ãª–@‰ñ”‚WSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚WSk
047110             MOVE ‰·ãª–@—¿‚WSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚WSk
047120             MOVE “d—Ã‰ñ”‚WSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚WSk
047130             MOVE “d—Ã—¿‚WSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚WSk
047140         END-IF
047150*******************************************************************
047160*/‚W•”ˆÊ–Ú‘½•”ˆÊ
047170         IF ‘½•”ˆÊŒã—Ã‰ñ”‚W‚q‚v > ZERO
047180             MOVE ‘½•”ˆÊŒã—Ã’P‰¿‚n‚v(8) TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚W
047190             MOVE ‘½•”ˆÊŒã—Ã‰ñ”‚W‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚W
047200             MOVE ‘½•”ˆÊŒã—Ã—¿‚W‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚W
047210         END-IF
047220*/‚W•”ˆÊ–Ú’·Šú
047230         IF ’·ŠúŒã—Ã‰ñ”‚W‚q‚v > ZERO
047240             MOVE ’·ŠúŒã—Ã’P‰¿‚n‚v(8) TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚W
047250             MOVE ’·ŠúŒã—Ã‰ñ”‚W‚q‚v  TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚W
047260             MOVE ’·ŠúŒã—Ã—¿‚W‚q‚v    TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚W
047270         END-IF
047280*/‚W•”ˆÊ–Ú‘½•”ˆÊSk
047290         IF ‘½•”ˆÊSkŒã—Ã‰ñ”‚W‚q‚v > ZERO
047300             MOVE ‘½•”ˆÊSkŒã—Ã’P‰¿‚n‚v(8) TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚W
047310             MOVE ‘½•”ˆÊSkŒã—Ã‰ñ”‚W‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚W
047320             MOVE ‘½•”ˆÊSkŒã—Ã—¿‚W‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚W
047330         END-IF
047340**********************************************************************
047350* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
047360         MOVE •”ˆÊ‡Œv‚v‚W       TO ˜A—¿˜JŽ©|¬Œv‚W
047370         MOVE •”ˆÊ‡Œv’·Šú‚v‚W   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚W
047380     END-IF.
047390**********
047400* ‚X•”ˆÊ *
047410**********
047420     IF •”ˆÊ‡Œv‚v‚X > ZERO
047430         IF Œã—Ã‰ñ”‚X‚q‚v > ZERO
047440             MOVE Œã—Ã’P‰¿‚n‚v(9)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚X
047450             MOVE Œã—Ã‰ñ”‚X‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚X
047460             MOVE Œã—Ã—¿‚X‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚X
047470         END-IF
047480         MOVE —âãª–@’P‰¿‚v(9)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚X
047490         MOVE ‰·ãª–@’P‰¿‚v(9)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚X
047500         MOVE “d—Ã’P‰¿‚v(9)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚X
047510         MOVE —âãª–@‰ñ”‚X‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚X
047520         MOVE —âãª–@—¿‚X‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚X
047530         MOVE ‰·ãª–@‰ñ”‚X‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚X
047540         MOVE ‰·ãª–@—¿‚X‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚X
047550         MOVE “d—Ã‰ñ”‚X‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚X
047560         MOVE “d—Ã—¿‚X‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚X
047570*/‚X•”ˆÊ–ÚSk
047580         IF Œã—Ã‰ñ”‚XSk‚q‚v > ZERO
047590             MOVE SkŒã—Ã’P‰¿‚n‚v(9)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚XSk
047600             MOVE Œã—Ã‰ñ”‚XSk‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚XSk
047610             MOVE Œã—Ã—¿‚XSk‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚XSk
047620         END-IF
047630         IF (—âãª–@‰ñ”‚XSk‚q‚v > ZERO) OR
047640            (‰·ãª–@‰ñ”‚XSk‚q‚v > ZERO) OR
047650            (“d—Ã‰ñ”‚XSk‚q‚v   > ZERO)
047660             MOVE ‰·ãª–@’P‰¿‚v’üŒ¸(9)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚XSk
047670             MOVE —âãª–@’P‰¿‚v’üŒ¸(9)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚XSk
047680             MOVE “d—Ã’P‰¿‚v’üŒ¸  (9)    TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚XSk
047690             MOVE —âãª–@‰ñ”‚XSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚XSk
047700             MOVE —âãª–@—¿‚XSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚XSk
047710             MOVE ‰·ãª–@‰ñ”‚XSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚XSk
047720             MOVE ‰·ãª–@—¿‚XSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚XSk
047730             MOVE “d—Ã‰ñ”‚XSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚XSk
047740             MOVE “d—Ã—¿‚XSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚XSk
047750         END-IF
047760*******************************************************************
047770*/‚X•”ˆÊ–Ú‘½•”ˆÊ
047780         IF ‘½•”ˆÊŒã—Ã‰ñ”‚X‚q‚v > ZERO
047790             MOVE ‘½•”ˆÊŒã—Ã’P‰¿‚n‚v(9) TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚X
047800             MOVE ‘½•”ˆÊŒã—Ã‰ñ”‚X‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚X
047810             MOVE ‘½•”ˆÊŒã—Ã—¿‚X‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚X
047820         END-IF
047830*/‚X•”ˆÊ–Ú’·Šú
047840         IF ’·ŠúŒã—Ã‰ñ”‚X‚q‚v > ZERO
047850             MOVE ’·ŠúŒã—Ã’P‰¿‚n‚v(9) TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚X
047860             MOVE ’·ŠúŒã—Ã‰ñ”‚X‚q‚v  TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚X
047870             MOVE ’·ŠúŒã—Ã—¿‚X‚q‚v    TO ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚X
047880         END-IF
047890*/‚X•”ˆÊ–Ú‘½•”ˆÊSk
047900         IF ‘½•”ˆÊSkŒã—Ã‰ñ”‚X‚q‚v > ZERO
047910             MOVE ‘½•”ˆÊSkŒã—Ã’P‰¿‚n‚v(9) TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚X
047920             MOVE ‘½•”ˆÊSkŒã—Ã‰ñ”‚X‚q‚v  TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚X
047930             MOVE ‘½•”ˆÊSkŒã—Ã—¿‚X‚q‚v    TO ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚X
047940         END-IF
047950**********************************************************************
047960* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
047970         MOVE •”ˆÊ‡Œv‚v‚X           TO ˜A—¿˜JŽ©|¬Œv‚X
047980         MOVE •”ˆÊ‡Œv’·Šú‚v‚X       TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚X
047990     END-IF.
048000*================================================================*
048010 §Œäî•ñŽæ“¾ SECTION.
048020*
048030     MOVE ZERO TO §|§Œä‹æ•ª.
048040     READ §Œäî•ñƒ}ƒXƒ^
048050     NOT INVALID KEY
048060         MOVE §|Å‘å“o˜^•”ˆÊ”   TO Å‘å“o˜^•”ˆÊ”‚v
048070         MOVE §|•¬•‰’SŒvŽZ‹æ•ª TO •¬•‰’SŒvŽZ‹æ•ª‚v
048080     END-READ.
048090*
048100*================================================================*
048110 ‘½•”ˆÊ‡Œvˆ— SECTION.
048120*
048130     COMPUTE •”ˆÊ‡Œv‚v‚P = Œã—Ã—¿‚P‚q‚v        +
048140                            Œã—Ã—¿‚PSk‚q‚v    +
048150                            —âãª–@—¿‚P‚q‚v      +
048160                            ‰·ãª–@—¿‚P‚q‚v      +
048170                            “d—Ã—¿‚P‚q‚v        +
048180                            —âãª–@—¿‚PSk‚q‚v  +
048190                            ‰·ãª–@—¿‚PSk‚q‚v  +
048200                            “d—Ã—¿‚PSk‚q‚v    +
048210                            ‘½•”ˆÊŒã—Ã—¿‚P‚q‚v  +
048220                            ’·ŠúŒã—Ã—¿‚P‚q‚v    +
048230                            ‘½•”ˆÊSkŒã—Ã—¿‚P‚q‚v.
048240
048250*
048260     COMPUTE •”ˆÊ‡Œv‚v‚Q = Œã—Ã—¿‚Q‚q‚v       +
048270                            Œã—Ã—¿‚QSk‚q‚v   +
048280                            —âãª–@—¿‚Q‚q‚v     +
048290                            ‰·ãª–@—¿‚Q‚q‚v     +
048300                            “d—Ã—¿‚Q‚q‚v       +
048310                            —âãª–@—¿‚QSk‚q‚v +
048320                            ‰·ãª–@—¿‚QSk‚q‚v +
048330                            “d—Ã—¿‚QSk‚q‚v    +
048340                            ‘½•”ˆÊŒã—Ã—¿‚Q‚q‚v  +
048350                            ’·ŠúŒã—Ã—¿‚Q‚q‚v    +
048360                            ‘½•”ˆÊSkŒã—Ã—¿‚Q‚q‚v.
048370*
048380     COMPUTE •”ˆÊ‡Œv‚v‚R = Œã—Ã—¿‚R‚q‚v       +
048390                            Œã—Ã—¿‚RSk‚q‚v   +
048400                            —âãª–@—¿‚R‚q‚v     +
048410                            ‰·ãª–@—¿‚R‚q‚v     +
048420                            “d—Ã—¿‚R‚q‚v       +
048430                            —âãª–@—¿‚RSk‚q‚v +
048440                            ‰·ãª–@—¿‚RSk‚q‚v +
048450                            “d—Ã—¿‚RSk‚q‚v    +
048460                            ‘½•”ˆÊŒã—Ã—¿‚R‚q‚v  +
048470                            ’·ŠúŒã—Ã—¿‚R‚q‚v    +
048480                            ‘½•”ˆÊSkŒã—Ã—¿‚R‚q‚v.
048490*
048500     COMPUTE •”ˆÊ‡Œv‚v‚S = Œã—Ã—¿‚S‚q‚v       +
048510                            Œã—Ã—¿‚SSk‚q‚v   +
048520                            —âãª–@—¿‚S‚q‚v     +
048530                            ‰·ãª–@—¿‚S‚q‚v     +
048540                            “d—Ã—¿‚S‚q‚v       +
048550                            —âãª–@—¿‚SSk‚q‚v +
048560                            ‰·ãª–@—¿‚SSk‚q‚v +
048570                            “d—Ã—¿‚SSk‚q‚v    +
048580                            ‘½•”ˆÊŒã—Ã—¿‚S‚q‚v  +
048590                            ’·ŠúŒã—Ã—¿‚S‚q‚v    +
048600                            ‘½•”ˆÊSkŒã—Ã—¿‚S‚q‚v.
048610*
048620     COMPUTE •”ˆÊ‡Œv‚v‚T = Œã—Ã—¿‚T‚q‚v       +
048630                            Œã—Ã—¿‚TSk‚q‚v   +
048640                            —âãª–@—¿‚T‚q‚v     +
048650                            ‰·ãª–@—¿‚T‚q‚v     +
048660                            “d—Ã—¿‚T‚q‚v       +
048670                            —âãª–@—¿‚TSk‚q‚v +
048680                            ‰·ãª–@—¿‚TSk‚q‚v +
048690                            “d—Ã—¿‚TSk‚q‚v    +
048700                            ‘½•”ˆÊŒã—Ã—¿‚T‚q‚v  +
048710                            ’·ŠúŒã—Ã—¿‚T‚q‚v    +
048720                            ‘½•”ˆÊSkŒã—Ã—¿‚T‚q‚v.
048730*
048740     COMPUTE •”ˆÊ‡Œv‚v‚U = Œã—Ã—¿‚U‚q‚v       +
048750                            Œã—Ã—¿‚USk‚q‚v   +
048760                            —âãª–@—¿‚U‚q‚v     +
048770                            ‰·ãª–@—¿‚U‚q‚v     +
048780                            “d—Ã—¿‚U‚q‚v       +
048790                            —âãª–@—¿‚USk‚q‚v +
048800                            ‰·ãª–@—¿‚USk‚q‚v +
048810                            “d—Ã—¿‚USk‚q‚v    +
048820                            ‘½•”ˆÊŒã—Ã—¿‚U‚q‚v  +
048830                            ’·ŠúŒã—Ã—¿‚U‚q‚v    +
048840                            ‘½•”ˆÊSkŒã—Ã—¿‚U‚q‚v.
048850*
048860     COMPUTE •”ˆÊ‡Œv‚v‚V = Œã—Ã—¿‚V‚q‚v     +
048870                            Œã—Ã—¿‚VSk‚q‚v +
048880                            —âãª–@—¿‚V‚q‚v   +
048890                            ‰·ãª–@—¿‚V‚q‚v   +
048900                            “d—Ã—¿‚V‚q‚v     +
048910                            —âãª–@—¿‚VSk‚q‚v   +
048920                            ‰·ãª–@—¿‚VSk‚q‚v   +
048930                            “d—Ã—¿‚VSk‚q‚v    +
048940                            ‘½•”ˆÊŒã—Ã—¿‚V‚q‚v  +
048950                            ’·ŠúŒã—Ã—¿‚V‚q‚v    +
048960                            ‘½•”ˆÊSkŒã—Ã—¿‚V‚q‚v.
048970*
048980     COMPUTE •”ˆÊ‡Œv‚v‚W = Œã—Ã—¿‚W‚q‚v     +
048990                            Œã—Ã—¿‚WSk‚q‚v +
049000                            —âãª–@—¿‚W‚q‚v   +
049010                            ‰·ãª–@—¿‚W‚q‚v   +
049020                            “d—Ã—¿‚W‚q‚v     +
049030                            —âãª–@—¿‚WSk‚q‚v   +
049040                            ‰·ãª–@—¿‚WSk‚q‚v   +
049050                            “d—Ã—¿‚WSk‚q‚v    +
049060                            ‘½•”ˆÊŒã—Ã—¿‚W‚q‚v  +
049070                            ’·ŠúŒã—Ã—¿‚W‚q‚v    +
049080                            ‘½•”ˆÊSkŒã—Ã—¿‚W‚q‚v.
049090*
049100     COMPUTE •”ˆÊ‡Œv‚v‚X = Œã—Ã—¿‚X‚q‚v       +
049110                            Œã—Ã—¿‚XSk‚q‚v   +
049120                            —âãª–@—¿‚X‚q‚v     +
049130                            ‰·ãª–@—¿‚X‚q‚v     +
049140                            “d—Ã—¿‚X‚q‚v       +
049150                            —âãª–@—¿‚XSk‚q‚v +
049160                            ‰·ãª–@—¿‚XSk‚q‚v +
049170                            “d—Ã—¿‚XSk‚q‚v    +
049180                            ‘½•”ˆÊŒã—Ã—¿‚X‚q‚v  +
049190                            ’·ŠúŒã—Ã—¿‚X‚q‚v    +
049200                            ‘½•”ˆÊSkŒã—Ã—¿‚X‚q‚v.
049210*
049220**================================================================*
049230 Ž{p“úƒZƒbƒg SECTION.
049240*
049250* •¬•‰’S‚o‚f‚ÉŽ{p“ú‚ð˜AŒ‹‚·‚é
049260     MOVE Ž{‹L|Ž{p“ú TO ˜AŒv|Ž{p“ú.
049270*
049280*================================================================*
049290 ‰ŒŸ¿‹‹æ•ªŽæ“¾ SECTION.
049300*
049310* ‰ŒŸ—¿EÄŒŸ—¿‚ð¿‹‚·‚é‚©‚Ìƒtƒ‰ƒOŽæ“¾
049320     MOVE ˜AŒv|Ž{p˜a—ï TO •‰|Ž{p˜a—ï. 
049330     MOVE ˜AŒv|Ž{p”N   TO •‰|Ž{p”N.   
049340     MOVE ˜AŒv|Ž{pŒŽ   TO •‰|Ž{pŒŽ.   
049350     MOVE ˜AŒv|Š³ŽÒ”Ô† TO •‰|Š³ŽÒ”Ô†. 
049360     MOVE ˜AŒv|Ž}”Ô     TO •‰|Ž}”Ô.     
049370     READ •‰ƒf[ƒ^‚e
049380     INVALID KEY
049390         DISPLAY NC"ŠY“–ŒŽ‚Ìî•ñ‚ª‚ ‚è‚Ü‚¹‚ñ" UPON CONS
049400         DISPLAY NC"Ž{p˜a—ï" ˜AŒv|Ž{p˜a—ï  UPON CONS
049410         DISPLAY NC"Ž{p”N"   ˜AŒv|Ž{p”N    UPON CONS 
049420         DISPLAY NC"Ž{pŒŽ"   ˜AŒv|Ž{pŒŽ    UPON CONS 
049430         DISPLAY NC"Š³ŽÒ”Ô†" ˜AŒv|Š³ŽÒ”Ô†  UPON CONS
049440         DISPLAY NC"Ž}”Ô"     ˜AŒv|Ž}”Ô      UPON CONS 
049450         MOVE NC"•‰" TO ƒtƒ@ƒCƒ‹–¼‚v
049460         PERFORM ƒGƒ‰[•\Ž¦‚q
049470     NOT INVALID KEY
049480         MOVE •‰|‰ŒŸ¿‹‹æ•ª TO ‰ŒŸ¿‹‹æ•ª‚v
049490     END-READ.
049500*
049510*================================================================*
049520 ’·Šú’üŒ¸‹æ•ªŽæ“¾ SECTION.
049530*
049540     MOVE ˜AŒv|Ž{p˜a—ï TO •‰|Ž{p˜a—ï.
049550     MOVE ˜AŒv|Ž{p”N   TO •‰|Ž{p”N.
049560     MOVE ˜AŒv|Ž{pŒŽ   TO •‰|Ž{pŒŽ.
049570     MOVE ˜AŒv|Š³ŽÒ”Ô† TO •‰|Š³ŽÒ”Ô†.
049580     MOVE ˜AŒv|Ž}”Ô     TO •‰|Ž}”Ô.
049590     READ •‰ƒf[ƒ^‚e
049600     INVALID KEY
049610*/ƒGƒ‰[•\Ž¦‚ÌC³
049620         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
049630         DISPLAY "Žw’è‚ÌŽófŽÒ‚Ì•‰ƒf[ƒ^‚ª‚ ‚è‚Ü‚¹‚ñ"
049640                 " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
049650                 " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
049660*-----------------------------------------*
049670         CALL "actcshm"  WITH C LINKAGE
049680*-----------------------------------------*
049690         ACCEPT  ƒL[“ü—Í FROM CONS
049700         PERFORM ƒtƒ@ƒCƒ‹•Â½
049710         MOVE ZERO TO PROGRAM-STATUS
049720         EXIT PROGRAM
049730     NOT INVALID KEY
049740         MOVE •‰|•”ˆÊ” TO •”ˆÊ”‚v
049750     END-READ.
049760*
049770     PERFORM VARYING •”ˆÊƒJƒEƒ“ƒ^‚Q FROM 1 BY 1
049780               UNTIL •”ˆÊƒJƒEƒ“ƒ^‚Q > •”ˆÊ”‚v
049790         MOVE ZERO TO ’·Šú‹æ•ª‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
049800     END-PERFORM.
049810*
049820     PERFORM VARYING •”ˆÊƒJƒEƒ“ƒ^‚Q FROM 1 BY 1
049830              UNTIL  •”ˆÊƒJƒEƒ“ƒ^‚Q > •”ˆÊ”‚v
049840*
049850* •‰|I—¹˜a—ï”NŒŽ‚ª–¢“ü—Í‚Ìê‡‚ÍAALL 9 ‚ðƒZƒbƒg‚·‚é
049860* ‚O‚PF”PÁ@‚O‚QF‘Å–o@‚O‚RFÁ‚Ì‚Ý’·Šú’üŒ¸‚ ‚è
049870         IF •‰|•‰Ží•Ê(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 01 OR 02 OR 03
049880             PERFORM ŠJŽnI—¹“úŽæ“¾
049890             PERFORM ”NŒŽâ‘Î’lŒvŽZ
049900             IF ’·Šúƒtƒ‰ƒO NOT = SPACE 
049910                 MOVE 1 TO ’·Šú‹æ•ª‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
049920             END-IF
049930         END-IF
049940     END-PERFORM.
049950*
049960**================================================================*
049970 ‰^“®—Ã–@—¿ŽZ’è SECTION.
049980*
049990     MOVE ZERO TO ‰^“®—Ã–@—¿‚v.
050000     IF Ž{‹L|‰^“®—Ã–@‹æ•ª = 1
050010         COMPUTE ‰^“®—Ã–@‰ñ”‚v = ‰^“®—Ã–@‰ñ”‚v + 1
050020         MOVE ‰^“®—Ã–@—¿‚d‚v TO ‰^“®—Ã–@—¿‚v
050030     END-IF.
050040*================================================================*
050050 Žw“±ŠÇ——¿ŽZ’è SECTION.
050060*
050070     MOVE ZERO TO Žw“±ŠÇ——¿‚v.
050080     IF Ž{‹L|Žw“±ŠÇ—‹æ•ª‚Q = 1
050090         COMPUTE Žw“±ŠÇ—‰ñ”‚v = Žw“±ŠÇ—‰ñ”‚v + 1
050100         MOVE Žw“±ŠÇ——¿‚d‚v TO Žw“±ŠÇ——¿‚v
050110     END-IF.
050120*================================================================*
050130 h”‘—¿ŽZ’è SECTION.
050140*
050150     MOVE ZERO TO h”‘—¿‚v.
050160     IF Ž{‹L|h”‘‹æ•ª = 1
050170         COMPUTE h”‘‰ñ”‚v = h”‘‰ñ”‚v + 1
050180         MOVE h”‘—¿‚d‚v TO h”‘—¿‚v
050190     END-IF.
050200*================================================================*
050210 HŽ–—¿ŽZ’è SECTION.
050220*
050230     MOVE ZERO TO HŽ–—¿‚v.
050240     IF Ž{‹L|HŽ–‹æ•ª = 1
050250         COMPUTE HŽ–‰ñ”‚v = HŽ–‰ñ”‚v + 1
050260         MOVE HŽ–—¿‚d‚v TO HŽ–—¿‚v
050270     END-IF.
050280*================================================================*
050290 ‹x‹ÆØ–¾‘—¿ŽZ’è SECTION.
050300*
050310     MOVE ZERO TO ‹x‹ÆØ–¾‘—¿‚v.
050320*/˜JÐ‚Í‹x‹ÆØ–¾‚Æ‚µ‚ÄŽg—p
050330*/‚P“ú‚É•¡”‰ñ‚ÌŽZ’è‚ào—ˆ‚é‚æ‚¤‚É•ÏX
050340*     IF Ž{‹L|Ž{pØ–¾‹æ•ª = 1
050350*         MOVE ‹x‹ÆØ–¾‘—¿‚b‚v TO ‹x‹ÆØ–¾‘—¿‚v
050360*     END-IF.
050370     COMPUTE ‹x‹ÆØ–¾‰ñ”‚v = ‹x‹ÆØ–¾‰ñ”‚v   + Ž{‹L|Ž{pØ–¾‹æ•ª.
050380     COMPUTE ‹x‹ÆØ–¾‘—¿‚v = ‹x‹ÆØ–¾‘—¿‚d‚v * Ž{‹L|Ž{pØ–¾‹æ•ª.
050390*================================================================*
050400 Ž{pØ–¾‘—¿ŽZ’è SECTION.
050410*
050420     MOVE ZERO TO Ž{pØ–¾‘—¿‚v.
050430     COMPUTE Ž{pØ–¾‰ñ”‚v = Ž{pØ–¾‰ñ”‚v   + Ž{‹L|Ž{pØ–¾‹æ•ª.
050440     COMPUTE Ž{pØ–¾‘—¿‚v = Ž{pØ–¾‘—¿‚d‚v * Ž{‹L|Ž{pØ–¾‹æ•ª.
050450*================================================================*
050460 Ž{p–¾×‘—¿ŽZ’è SECTION.
050470*
050480     MOVE ZERO TO Ž{p–¾×‘—¿‚v.
050490     COMPUTE Ž{p–¾×‰ñ”‚v = Ž{p–¾×‰ñ”‚v   + Ž{‹L|Ž{p–¾×‹æ•ª.
050500     COMPUTE Ž{p–¾×‘—¿‚v = Ž{p–¾×‘—¿‚d‚v * Ž{‹L|Ž{p–¾×‹æ•ª.
050510*================================================================*
050520 Ž©”…Ó‚»‚Ì‘¼‹àŠzŽZ’è SECTION.
050530*
050540     MOVE ZERO TO ‚»‚Ì‘¼‹àŠz‚v.
050550     IF Ž{‹L|Ž©”…Ó‚»‚Ì‘¼‹æ•ª = 1
050560         MOVE ˜AŒv|Ž{p˜a—ï TO •‰|Ž{p˜a—ï 
050570         MOVE ˜AŒv|Ž{p”N   TO •‰|Ž{p”N   
050580         MOVE ˜AŒv|Ž{pŒŽ   TO •‰|Ž{pŒŽ   
050590         MOVE ˜AŒv|Š³ŽÒ”Ô† TO •‰|Š³ŽÒ”Ô† 
050600         MOVE ˜AŒv|Ž}”Ô     TO •‰|Ž}”Ô     
050610         READ •‰ƒf[ƒ^‚e
050620         INVALID KEY
050630             DISPLAY NC"ŠY“–ŒŽ‚Ìî•ñ‚ª‚ ‚è‚Ü‚¹‚ñ" UPON CONS
050640             DISPLAY NC"Ž{p˜a—ï" ˜AŒv|Ž{p˜a—ï  UPON CONS
050650             DISPLAY NC"Ž{p”N"   ˜AŒv|Ž{p”N    UPON CONS
050660             DISPLAY NC"Ž{pŒŽ"   ˜AŒv|Ž{pŒŽ    UPON CONS
050670             DISPLAY NC"Š³ŽÒ”Ô†" ˜AŒv|Š³ŽÒ”Ô†  UPON CONS
050680             DISPLAY NC"Ž}”Ô"     ˜AŒv|Ž}”Ô      UPON CONS
050690             MOVE NC"•‰" TO ƒtƒ@ƒCƒ‹–¼‚v
050700             PERFORM ƒGƒ‰[•\Ž¦‚q
050710         NOT INVALID KEY
050720             MOVE •‰|Ž©”…Ó‚»‚Ì‘¼‹àŠz TO ‚»‚Ì‘¼‹àŠz‚v
050730         END-READ
050740     END-IF.
050750*================================================================*
050760 ¼—ï”NŽæ“¾ SECTION.
050770*
050780     MOVE ŒvŽZ˜a—ï‚v TO Œ³|Œ³†‹æ•ª.
050790     READ Œ³†ƒ}ƒXƒ^
050800     INVALID KEY
050810         DISPLAY NC"Žw’è˜a—ï‚ª“o˜^‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ" UPON CONS
050820         DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢" UPON CONS
050830*-----------------------------------------*
050840         CALL "actcshm"  WITH C LINKAGE
050850*-----------------------------------------*
050860         ACCEPT  ƒL[“ü—Í FROM CONS
050870         PERFORM I—¹ˆ—
050880         EXIT PROGRAM
050890     NOT INVALID KEY
050900         COMPUTE ¼—ï”N‚v = Œ³|ŠJŽn¼—ï”N + ŒvŽZ”N‚v - 1
050910     END-READ.
050920*
050930*================================================================*
050940 “ú•tâ‘Î’lŒvŽZ SECTION.
050950*
050960** 1989”N‚ðŠî€‚Æ‚µA“–”N‚Ü‚Å‰[”N‚ª‰½‰ñ‚ ‚é‚©‚ðŒvŽZ
050970** Œ³†ƒ}ƒXƒ^‚Ì¼—ïŠJŽn”NŠm”FB
050980*     COMPUTE ¼—ï”N·‚v   = ¼—ï”N‚v - 1989 - 1.
050990*     DIVIDE 4 INTO ¼—ï”N·‚v GIVING ‰[”N‰ñ”‚v
051000*                            REMAINDER è—]‚v
051010*     END-DIVIDE.
051020*/Šî€”N1989”N‚©‚ç“–”N‚Ü‚Å‚Ì‰[”N‚Ì‰ñ”‚ðŒvŽZ(“–”N‚ÍŠÜ‚Ü‚È‚¢)
051030*/Žn‚ß‚Ì‚¤‚é‚¤”N‚Ì1992”N‚©‚ç”‚¦‚é
051040     COMPUTE ¼—ï”N·‚v   = ¼—ï”N‚v - 1989 - 1.
051050     MOVE 1992 TO ‰ÁŽZ”N‚v.
051060     MOVE ZERO TO ‰[”N‰ñ”‚v.
051070     PERFORM UNTIL ¼—ï”N‚v <= ‰ÁŽZ”N‚v
051080         COMPUTE ‰ÁŽZ”N‚v   = ‰ÁŽZ”N‚v   + 4
051090         COMPUTE ‰[”N‰ñ”‚v = ‰[”N‰ñ”‚v + 1
051100     END-PERFORM.
051110*
051120     COMPUTE ’Êí”N‰ñ”‚v     = ¼—ï”N·‚v     - ‰[”N‰ñ”‚v.
051130     COMPUTE ’Êí”Nâ‘Î’l‚v   = ’Êí”N‰ñ”‚v   * 365.
051140     COMPUTE ‰[”Nâ‘Î’l‚v     = ‰[”N‰ñ”‚v     * 366.
051150     COMPUTE ”Nâ‘Î’l‚v       = ’Êí”Nâ‘Î’l‚v + ‰[”Nâ‘Î’l‚v.
051160* “–”N‚ª‚¤‚é‚¤”N‚©‚Ç‚¤‚©‚ð’²‚×‚éi‚QŒŽ‚Ì––“úŒvŽZj
051170*     COMPUTE ¼—ï”N·‚Q‚v     = ¼—ï”N‚v + 1.
051180*     DIVIDE 4 INTO ¼—ï”N·‚Q‚v GIVING ‰[”N‰ñ”‚v
051190*                                REMAINDER è—]‚v
051200*     END-DIVIDE.
051210     DIVIDE 4 INTO ¼—ï”N‚v GIVING ‰[”N‰ñ”‚v
051220                            REMAINDER è—]‚v
051230     END-DIVIDE.
051240*
051250     MOVE ZERO TO â‘Î’lŒŽ‚v.
051260*
051270* ‘OŒŽ‚Ü‚Å‚Ìâ‘Î’l‚ð—ÝŒv
051280     COMPUTE â‘Î’lŒŽ‚v‚Q = ŒvŽZŒŽ‚v * 30.
051290*
051300     EVALUATE ŒvŽZŒŽ‚v
051310     WHEN 3
051320         COMPUTE â‘Î’lŒŽ‚v   = â‘Î’lŒŽ‚v‚Q - 1
051330     WHEN 2
051340     WHEN 6
051350     WHEN 7
051360         COMPUTE â‘Î’lŒŽ‚v   = â‘Î’lŒŽ‚v‚Q + 1
051370     WHEN 8
051380         COMPUTE â‘Î’lŒŽ‚v   = â‘Î’lŒŽ‚v‚Q + 2
051390     WHEN 9
051400     WHEN 10
051410         COMPUTE â‘Î’lŒŽ‚v   = â‘Î’lŒŽ‚v‚Q + 3
051420     WHEN 11
051430     WHEN 12
051440         COMPUTE â‘Î’lŒŽ‚v   = â‘Î’lŒŽ‚v‚Q + 4
051450     WHEN OTHER
051460         MOVE â‘Î’lŒŽ‚v‚Q TO â‘Î’lŒŽ‚v
051470     END-EVALUATE.
051480*
051490     IF ( è—]‚v = ZERO ) AND
051500        ( ŒvŽZŒŽ‚v > 2  )
051510         COMPUTE â‘Î’lŒŽ‚v = â‘Î’lŒŽ‚v + 1
051520     END-IF.
051530*
051540     COMPUTE â‘Î’l‚v   = ”Nâ‘Î’l‚v + â‘Î’lŒŽ‚v + ŒvŽZ“ú‚v.
051550*
051560*================================================================*
051570*================================================================*
051580 ”NŒŽâ‘Î’lŒvŽZ SECTION.
051590*
051600* 1989”N1ŒŽ‚ðŠî€‚Æ‚µA“–”N‚Ü‚Å‚Ìâ‘Î’l‚ðŒvŽZ
051610     MOVE SPACE TO ’·Šúƒtƒ‰ƒO.
051620*
051630* ˜a—ï‚©‚ç¼—ï‚ðŠ·ŽZ
051640     MOVE ŠJŽn˜a—ï‚v TO Œ³|Œ³†‹æ•ª.
051650     READ Œ³†ƒ}ƒXƒ^
051660     INVALID KEY
051670         MOVE NC"Œ³†" TO ƒtƒ@ƒCƒ‹–¼‚v
051680     NOT INVALID KEY
051690         COMPUTE ¼—ï”N‚v   = Œ³|ŠJŽn¼—ï”N + ŠJŽn”N‚v
051700         COMPUTE ¼—ï”N·‚v = ¼—ï”N‚v - 1989
051710     END-READ.
051720*
051790     COMPUTE ŠJŽn”NŒŽâ‘Î’l‚v = (¼—ï”N·‚v * 12) + ŠJŽnŒŽ‚v.
051800*
051810     MOVE ˜AŒv|Ž{p˜a—ï TO Œ³|Œ³†‹æ•ª.
051820     READ Œ³†ƒ}ƒXƒ^
051830     INVALID KEY
051840         MOVE NC"Œ³†" TO ƒtƒ@ƒCƒ‹–¼‚v
051850     NOT INVALID KEY
051860         COMPUTE ¼—ï”N‚v   = Œ³|ŠJŽn¼—ï”N + ˜AŒv|Ž{p”N
051870         COMPUTE ¼—ï”N·‚v = ¼—ï”N‚v - 1989
051880     END-READ.
051890* Ž{p“ú‚ÍA‹«ŠE“ú‚Ì”»’è‚Ì•K—v‚È‚µH
051900* “–ŒŽ‚©—‚ŒŽ‚©‚Ì”»’è
051910*     IF Ž{‹L|Ž{p“ú > ’·Šú’üŒ¸ŒŽ‹«ŠE“ú‚j‚v
051920*         COMPUTE Ž{p”NŒŽâ‘Î’l‚v = (¼—ï”N·‚v * 12) + ( ˜AŒv|Ž{pŒŽ + 1 )
051930*     ELSE
051940         COMPUTE Ž{p”NŒŽâ‘Î’l‚v = (¼—ï”N·‚v * 12) + ˜AŒv|Ž{pŒŽ
051950*     END-IF.
051960*
051970     COMPUTE Ž{p”NŒŽ·‚v = Ž{p”NŒŽâ‘Î’l‚v - ŠJŽn”NŒŽâ‘Î’l‚v.
051980*
051990*/ŠJŽn“ú‚Ì‰ž“š“ú‚æ‚èŽ{p“ú‚ªŒã‚Ìê‡‚Í‚Pƒ–ŒŽƒvƒ‰ƒX‚·‚é
052000     IF ŠJŽn“ú‚v <= ˜AŒv|Ž{p“ú
052010         COMPUTE Ž{p”NŒŽ·‚v = Ž{p”NŒŽ·‚v + 1
052020     END-IF.
052030*
052040     IF Ž{p”NŒŽ·‚v >= ’·Šú’üŒ¸ŒŽ”‚j‚v
052050         MOVE "YES" TO ’·Šúƒtƒ‰ƒO
052060     END-IF.
052070*
052080*================================================================*
052090 ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F SECTION.
052100*
052110     IF ƒGƒ‰[•\Ž¦‚e = ZERO
052120         MOVE  "ƒGƒ‰[‚ª”­¶‚µ‚½‚Ì‚ÅƒRƒ“ƒ\[ƒ‹‰æ–Ê‚Ì“à—e" TO ˜Aƒ‚V|ƒƒbƒZ[ƒW‚P
052130         MOVE  "‚ðŠm”F‚µAC³‚µ‚Ä‚­‚¾‚³‚¢B"             TO ˜Aƒ‚V|ƒƒbƒZ[ƒW‚Q
052140         CALL   "MSG007"
052150         CANCEL "MSG007"
052160     END-IF.
052170     MOVE 1 TO ƒGƒ‰[•\Ž¦‚e.
052180*================================================================*
052190 ¿‹”NŒŽŽæ“¾ SECTION.
052200*
052210     MOVE ˜AŒv|Ž{p˜a—ï TO Žó|Ž{p˜a—ï
052220     MOVE ˜AŒv|Ž{p”N   TO Žó|Ž{p”N  
052230     MOVE ˜AŒv|Ž{pŒŽ   TO Žó|Ž{pŒŽ  
052240     MOVE ˜AŒv|Š³ŽÒ”Ô† TO Žó|Š³ŽÒ”Ô†
052250     MOVE ˜AŒv|Ž}”Ô     TO Žó|Ž}”Ô    
052260     READ ŽófŽÒî•ñ‚e
052270     INVALID KEY
052280         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
052290         DISPLAY "Žw’è‚ÌŽófŽÒî•ñ‚ª‚ ‚è‚Ü‚¹‚ñ"
052300                 " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
052310                 " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
052320*-----------------------------------------*
052330         CALL "actcshm"  WITH C LINKAGE
052340*-----------------------------------------*
052350         ACCEPT  ƒL[“ü—Í FROM CONS
052360         PERFORM ƒtƒ@ƒCƒ‹•Â½
052370         MOVE ZERO TO PROGRAM-STATUS
052380         EXIT PROGRAM
052390     NOT INVALID KEY
052400          MOVE Žó|¿‹˜a—ï TO ¿‹˜a—ï‚v
052410          MOVE Žó|¿‹”N   TO ¿‹”N‚v
052420          MOVE Žó|¿‹ŒŽ   TO ¿‹ŒŽ‚v
052430     END-READ.
052440*================================================================*
052450 ãª–@’P‰¿‰ñ”ƒZƒbƒg SECTION.
052460*
052470     IF ãª–@’P‰¿‚v NOT = ZERO
052480         PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 6
052490             IF ˜A—¿˜JŽ©‚Q|ãª–@’P‰¿(•”ˆÊƒJƒEƒ“ƒ^‚Q ƒJƒEƒ“ƒ^) = ZERO
052500                 MOVE ãª–@’P‰¿‚v TO ˜A—¿˜JŽ©‚Q|ãª–@’P‰¿(•”ˆÊƒJƒEƒ“ƒ^‚Q ƒJƒEƒ“ƒ^)
052510                 MOVE 1          TO ˜A—¿˜JŽ©‚Q|ãª–@‰ñ”(•”ˆÊƒJƒEƒ“ƒ^‚Q ƒJƒEƒ“ƒ^)
052520                 MOVE 10 TO ƒJƒEƒ“ƒ^
052530             ELSE
052540                 IF ãª–@’P‰¿‚v = ˜A—¿˜JŽ©‚Q|ãª–@’P‰¿(•”ˆÊƒJƒEƒ“ƒ^‚Q ƒJƒEƒ“ƒ^)
052550                     COMPUTE ˜A—¿˜JŽ©‚Q|ãª–@‰ñ”(•”ˆÊƒJƒEƒ“ƒ^‚Q ƒJƒEƒ“ƒ^) = ˜A—¿˜JŽ©‚Q|ãª–@‰ñ”(•”ˆÊƒJƒEƒ“ƒ^‚Q ƒJƒEƒ“ƒ^) + 1
052560                     MOVE 10 TO ƒJƒEƒ“ƒ^
052570                 END-IF
052580             END-IF
052590         END-PERFORM
052600         IF ƒJƒEƒ“ƒ^ < 10
052610             DISPLAY " ŽófŽÒ‡‚=" ˜AŒv|Š³ŽÒƒR[ƒh "–¾×•”‚É‘‚«‚«‚ê‚È‚¢ãª–@’P‰¿‚ª‚ ‚è‚Ü‚·B" UPON CONS
052620         END-IF
052630     END-IF.
052640*================================================================*
052650 “d—Ã’P‰¿‰ñ”ƒZƒbƒg SECTION.
052660*
052670     IF “d—Ã’P‰¿‚v‚Q NOT = ZERO
052680         PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 2
052690             IF ˜A—¿˜JŽ©‚Q|“d—Ã’P‰¿(•”ˆÊƒJƒEƒ“ƒ^‚Q ƒJƒEƒ“ƒ^) = ZERO
052700                 MOVE “d—Ã’P‰¿‚v‚Q TO ˜A—¿˜JŽ©‚Q|“d—Ã’P‰¿(•”ˆÊƒJƒEƒ“ƒ^‚Q ƒJƒEƒ“ƒ^)
052710                 MOVE 1            TO ˜A—¿˜JŽ©‚Q|“d—Ã‰ñ”(•”ˆÊƒJƒEƒ“ƒ^‚Q ƒJƒEƒ“ƒ^)
052720                 MOVE 10 TO ƒJƒEƒ“ƒ^
052730             ELSE
052740                 IF “d—Ã’P‰¿‚v‚Q = ˜A—¿˜JŽ©‚Q|“d—Ã’P‰¿(•”ˆÊƒJƒEƒ“ƒ^‚Q ƒJƒEƒ“ƒ^)
052750                     COMPUTE ˜A—¿˜JŽ©‚Q|“d—Ã‰ñ”(•”ˆÊƒJƒEƒ“ƒ^‚Q ƒJƒEƒ“ƒ^) = ˜A—¿˜JŽ©‚Q|“d—Ã‰ñ”(•”ˆÊƒJƒEƒ“ƒ^‚Q ƒJƒEƒ“ƒ^) + 1
052760                     MOVE 10 TO ƒJƒEƒ“ƒ^
052770                 END-IF
052780             END-IF
052790         END-PERFORM
052800         IF ƒJƒEƒ“ƒ^ < 10
052810             DISPLAY " ŽófŽÒ‡‚=" ˜AŒv|Š³ŽÒƒR[ƒh "–¾×•”‚É‘‚«‚«‚ê‚È‚¢“d—Ã’P‰¿‚ª‚ ‚è‚Ü‚·B" UPON CONS
052820         END-IF
052830     END-IF.
052840*================================================================*
052850 “–ŒŽ‰ïŒvƒtƒ@ƒCƒ‹íœ SECTION.
052860*
052870     MOVE ˜AŒv|Š³ŽÒƒR[ƒh     TO ‰ï|Š³ŽÒƒR[ƒh    .
052880     MOVE ˜AŒv|Ž{p˜a—ï       TO ‰ï|Ž{p˜a—ï      .
052890     MOVE ˜AŒv|Ž{p”N         TO ‰ï|Ž{p”N        .
052900     MOVE ˜AŒv|Ž{pŒŽ         TO ‰ï|Ž{pŒŽ        .
052910     MOVE ZERO                 TO ‰ï|Ž{p“ú        .
052920     START ‰ïŒvƒf[ƒ^‚e KEY IS >= ‰ï|Š³ŽÒƒR[ƒh ‰ï|Ž{p˜a—ï”NŒŽ“ú
052930     END-START.
052940     IF ó‘ÔƒL[ = "00"
052950         MOVE SPACE TO I—¹ƒtƒ‰ƒO
052960         PERFORM ‰ïŒvƒf[ƒ^‚e“Çž
052970         PERFORM UNTIL (˜AŒv|Š³ŽÒƒR[ƒh NOT = ‰ï‚O‚Q|Š³ŽÒƒR[ƒh) OR
052980                       (˜AŒv|Ž{p˜a—ï   NOT = ‰ï‚O‚Q|Ž{p˜a—ï  ) OR
052990                       (˜AŒv|Ž{p”N     NOT = ‰ï‚O‚Q|Ž{p”N    ) OR
053000                       (˜AŒv|Ž{pŒŽ     NOT = ‰ï‚O‚Q|Ž{pŒŽ    ) OR
053010                       (I—¹ƒtƒ‰ƒO       NOT = SPACE         )
053020             DELETE ‰ïŒvƒf[ƒ^‚e
053030             INVALID KEY
053040                 MOVE NC"‰ïŒv" TO ƒtƒ@ƒCƒ‹–¼‚v
053050                 PERFORM ƒGƒ‰[•\Ž¦‚c
053060             END-DELETE
053070             PERFORM ‰ïŒvƒf[ƒ^‚e“Çž
053080         END-PERFORM
053090     END-IF.
053100
053110*================================================================*
053120 ƒGƒ‰[•\Ž¦‚c SECTION.
053130*
053140     DISPLAY ƒtƒ@ƒCƒ‹–¼‚v NC"ƒtƒ@ƒCƒ‹íœƒGƒ‰["   UPON CONS.
053150     DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[               UPON CONS.
053160     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
053170     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢" UPON CONS.
053180*-----------------------------------------*
053190         CALL "actcshm"  WITH C LINKAGE
053200*-----------------------------------------*
053210     ACCEPT  ƒL[“ü—Í FROM CONS.
053220     PERFORM ƒtƒ@ƒCƒ‹•Â½.
053230     EXIT PROGRAM.
053240*================================================================*
053250 ‰ïŒvƒf[ƒ^‚e“Çž SECTION.
053260*
053270     READ ‰ïŒvƒf[ƒ^‚e NEXT
053280     AT END
053290         MOVE SPACE TO ‰ï‚O‚Q|ƒŒƒR[ƒh
053300         MOVE "YES" TO I—¹ƒtƒ‰ƒO
053310     END-READ.
053320*================================================================*
053330 ‰ïŒvƒf[ƒ^‚e‘ž SECTION.
053340*
053350     MOVE SPACE TO ‰ï‚O‚Q|ƒŒƒR[ƒh.
053360     INITIALIZE    ‰ï‚O‚Q|ƒŒƒR[ƒh.
053370     MOVE ˜AŒv|Š³ŽÒƒR[ƒh     TO ‰ï‚O‚Q|Š³ŽÒƒR[ƒh.
053380     MOVE ˜AŒv|Ž{p˜a—ï       TO ‰ï‚O‚Q|Ž{p˜a—ï.
053390     MOVE ˜AŒv|Ž{p”N         TO ‰ï‚O‚Q|Ž{p”N.
053400     MOVE ˜AŒv|Ž{pŒŽ         TO ‰ï‚O‚Q|Ž{pŒŽ.
053410     MOVE ˜AŒv|Ž{p“ú         TO ‰ï‚O‚Q|Ž{p“ú.
053420*
053430     COMPUTE ‰ï‚O‚Q|”ï—pŠz = ‰ŒŸ—¿‚v + ‰ŒŸ‰ÁŽZ—¿‚v + ÄŒŸ—¿‚v + ‰—Ã—¿‚v + ‰—Ã‰ÁŽZ—¿‚v + 
053440     ‰ŒŸŽž‘Š’k—¿‚v + ‹à‘®•›Žq—¿‚v + ‰‰ñˆ’u—¿‚v + Ž{pî•ñ’ñ‹Ÿ—¿‚v + Žw“±ŠÇ——¿‚v +
053450     “Á•ÊÞ—¿”ï‚v + ŒðŠ·•ï‘Ñ—¿‚v + ‰^“®—Ã–@—¿‚v + h”‘—¿‚v + HŽ–—¿‚v + ‹x‹ÆØ–¾‘—¿‚v +
053460     Ž{pØ–¾‘—¿‚v + Ž{p–¾×‘—¿‚v + ‚»‚Ì‘¼‹àŠz‚v +
053470     + Œã—Ã—¿‚P‚m‚q‚v + —âãª–@—¿‚P‚m‚q‚v + ‰·ãª–@—¿‚P‚m‚q‚v + “d—Ã—¿‚P‚m‚q‚v
053480     + Œã—Ã—¿‚PSk‚m‚q‚v + —âãª–@—¿‚PSk‚m‚q‚v + ‰·ãª–@—¿‚PSk‚m‚q‚v + “d—Ã—¿‚PSk‚m‚q‚v
053490     + ‘½•”ˆÊŒã—Ã—¿‚P‚m‚q‚v + ’·ŠúŒã—Ã—¿‚P‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚P‚m‚q‚v
053500     + Œã—Ã—¿‚Q‚m‚q‚v + —âãª–@—¿‚Q‚m‚q‚v + ‰·ãª–@—¿‚Q‚m‚q‚v + “d—Ã—¿‚Q‚m‚q‚v
053510     + Œã—Ã—¿‚QSk‚m‚q‚v + —âãª–@—¿‚QSk‚m‚q‚v + ‰·ãª–@—¿‚QSk‚m‚q‚v + “d—Ã—¿‚QSk‚m‚q‚v
053520     + ‘½•”ˆÊŒã—Ã—¿‚Q‚m‚q‚v + ’·ŠúŒã—Ã—¿‚Q‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚Q‚m‚q‚v
053530     + Œã—Ã—¿‚R‚m‚q‚v + —âãª–@—¿‚R‚m‚q‚v + ‰·ãª–@—¿‚R‚m‚q‚v + “d—Ã—¿‚R‚m‚q‚v 
053540     + Œã—Ã—¿‚RSk‚m‚q‚v + —âãª–@—¿‚RSk‚m‚q‚v + ‰·ãª–@—¿‚RSk‚m‚q‚v + “d—Ã—¿‚RSk‚m‚q‚v
053550     + ‘½•”ˆÊŒã—Ã—¿‚R‚m‚q‚v + ’·ŠúŒã—Ã—¿‚R‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚R‚m‚q‚v
053560     + Œã—Ã—¿‚S‚m‚q‚v + —âãª–@—¿‚S‚m‚q‚v + ‰·ãª–@—¿‚S‚m‚q‚v + “d—Ã—¿‚S‚m‚q‚v
053570     + Œã—Ã—¿‚SSk‚m‚q‚v + —âãª–@—¿‚SSk‚m‚q‚v + ‰·ãª–@—¿‚SSk‚m‚q‚v + “d—Ã—¿‚SSk‚m‚q‚v
053580     + ‘½•”ˆÊŒã—Ã—¿‚S‚m‚q‚v + ’·ŠúŒã—Ã—¿‚S‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚S‚m‚q‚v
053590     + Œã—Ã—¿‚T‚m‚q‚v + —âãª–@—¿‚T‚m‚q‚v + ‰·ãª–@—¿‚T‚m‚q‚v + “d—Ã—¿‚T‚m‚q‚v
053600     + Œã—Ã—¿‚TSk‚m‚q‚v + —âãª–@—¿‚TSk‚m‚q‚v + ‰·ãª–@—¿‚TSk‚m‚q‚v + “d—Ã—¿‚TSk‚m‚q‚v
053610     + ‘½•”ˆÊŒã—Ã—¿‚T‚m‚q‚v + ’·ŠúŒã—Ã—¿‚T‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚T‚m‚q‚v
053620     + Œã—Ã—¿‚U‚m‚q‚v + —âãª–@—¿‚U‚m‚q‚v + ‰·ãª–@—¿‚U‚m‚q‚v + “d—Ã—¿‚U‚m‚q‚v
053630     + Œã—Ã—¿‚USk‚m‚q‚v + —âãª–@—¿‚USk‚m‚q‚v + ‰·ãª–@—¿‚USk‚m‚q‚v + “d—Ã—¿‚USk‚m‚q‚v
053640     + ‘½•”ˆÊŒã—Ã—¿‚U‚m‚q‚v + ’·ŠúŒã—Ã—¿‚U‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚U‚m‚q‚v
053650     + Œã—Ã—¿‚V‚m‚q‚v + —âãª–@—¿‚V‚m‚q‚v + ‰·ãª–@—¿‚V‚m‚q‚v + “d—Ã—¿‚V‚m‚q‚v
053660     + Œã—Ã—¿‚VSk‚m‚q‚v + —âãª–@—¿‚VSk‚m‚q‚v + ‰·ãª–@—¿‚VSk‚m‚q‚v + “d—Ã—¿‚VSk‚m‚q‚v
053670     + ‘½•”ˆÊŒã—Ã—¿‚V‚m‚q‚v + ’·ŠúŒã—Ã—¿‚V‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚V‚m‚q‚v
053680     + Œã—Ã—¿‚W‚m‚q‚v + —âãª–@—¿‚W‚m‚q‚v + ‰·ãª–@—¿‚W‚m‚q‚v + “d—Ã—¿‚W‚m‚q‚v
053690     + Œã—Ã—¿‚WSk‚m‚q‚v + —âãª–@—¿‚WSk‚m‚q‚v + ‰·ãª–@—¿‚WSk‚m‚q‚v + “d—Ã—¿‚WSk‚m‚q‚v
053700     + ‘½•”ˆÊŒã—Ã—¿‚W‚m‚q‚v + ’·ŠúŒã—Ã—¿‚W‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚W‚m‚q‚v
053710     + Œã—Ã—¿‚X‚m‚q‚v + —âãª–@—¿‚X‚m‚q‚v + ‰·ãª–@—¿‚X‚m‚q‚v + “d—Ã—¿‚X‚m‚q‚v
053720     + Œã—Ã—¿‚XSk‚m‚q‚v + —âãª–@—¿‚XSk‚m‚q‚v + ‰·ãª–@—¿‚XSk‚m‚q‚v + “d—Ã—¿‚XSk‚m‚q‚v
053730     + ‘½•”ˆÊŒã—Ã—¿‚X‚m‚q‚v + ’·ŠúŒã—Ã—¿‚X‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚X‚m‚q‚v.
053740***
053750*
053760*---------------------------------------------------------------------------------------*
053770* ¦ ã‹LŒãA“ü‹àŠz‚ÍA‰ïŒv—ÌŽûF‚É‚ ‚ê‚ÎƒZƒbƒg
053780     MOVE ZERO               TO —Ì|Ž{p‹æ•ª.
053790* 1:“úA2:ŒŽA3:”N
053800     MOVE 1                  TO —Ì|‰ïŒv—ÌŽû‹æ•ª.
053810     MOVE Ž{‹L|Ž{p˜a—ï     TO —Ì|Ž{p˜a—ï.
053820     MOVE Ž{‹L|Ž{p”N       TO —Ì|Ž{p”N.
053830     MOVE Ž{‹L|Ž{pŒŽ       TO —Ì|Ž{pŒŽ.
053840     MOVE Ž{‹L|Ž{p“ú       TO —Ì|Ž{p“ú.
053850     MOVE Ž{‹L|Š³ŽÒ”Ô†     TO —Ì|Š³ŽÒ”Ô†.
053860     MOVE Ž{‹L|Ž}”Ô         TO —Ì|Ž}”Ô.
053870     READ ‰ïŒv—ÌŽû‚e
053880     NOT INVALID KEY
053890        MOVE —Ì|“ü‹àŠz      TO ‰ï‚O‚Q|“ü‹àŠz
053900        MOVE —Ì|Ž©”ïŠz      TO ‰ï‚O‚Q|Ž©”ïŠz
053910     END-READ.
053920*---------------------------------------------------------------------------------------*
053930*
053940***
           PERFORM ‰ïŒv‚O‚QƒŒƒR[ƒhƒZƒbƒg.
053950*
053960     WRITE ‰ï‚O‚Q|ƒŒƒR[ƒh
053970     INVALID KEY
053980         MOVE NC"‰ï" TO ƒtƒ@ƒCƒ‹–¼‚v
053990         PERFORM ƒGƒ‰[•\Ž¦
054000         PERFORM I—¹ˆ—
054010         EXIT PROGRAM
054020     END-WRITE.
054030*================================================================*
054040 ƒŒƒZƒvƒgƒtƒ@ƒCƒ‹ì¬ SECTION.
054050*
054060     PERFORM ŽófŽÒî•ñŽæ“¾.
054070*    / Ž©”…Ó /
054080     MOVE  5   TO ƒŒƒZŽí•Ê‚v.
054090*
054100     PERFORM ƒŒƒR[ƒh‘¶Ýƒ`ƒFƒbƒN.
054110     PERFORM ‰ŒŸ“úˆÈ‘O‚Ìƒf[ƒ^”»’è.
054120*
054130*   / ˜JÐEŽ©”…ÓEŽ©”ï /
054140     PERFORM ‚»‚Ì‘¼ƒŒƒR[ƒhƒZƒbƒg.
054150     PERFORM ƒtƒ@ƒCƒ‹‘ž.
054160*
054170     MOVE ˜AŒv|Ž{p˜a—ï                TO ƒŒƒZ˜JŽ©|Ž{p˜a—ï.
054180     MOVE ˜AŒv|Ž{p”N                  TO ƒŒƒZ˜JŽ©|Ž{p”N.
054190     MOVE ˜AŒv|Ž{pŒŽ                  TO ƒŒƒZ˜JŽ©|Ž{pŒŽ.
054200*
054210     MOVE ˜AŒv|Š³ŽÒ”Ô†                TO ƒŒƒZ˜JŽ©|Š³ŽÒ”Ô†.
054220     MOVE ˜AŒv|Ž}”Ô                    TO ƒŒƒZ˜JŽ©|Ž}”Ô.
054230*   / 1:ˆê”Ê,2:˜Vl,3:•¬,4:˜JÐ,5:Ž©”…Ó,6:Ž©”ï,7:¶•Û’P“Æ,8•ÛŒ¯Ø–Y‚ê /
054240     MOVE ƒŒƒZŽí•Ê‚v                    TO ƒŒƒZ˜JŽ©|ƒŒƒZŽí•Ê.
054250     READ ƒŒƒZƒvƒg˜JŽ©‚e
054260     INVALID KEY
054270         PERFORM ƒŒƒZƒvƒg˜JŽ©ƒŒƒR[ƒhƒZƒbƒg
054280         WRITE ƒŒƒZ˜JŽ©|ƒŒƒR[ƒh
054290         IF ó‘ÔƒL[ NOT = "00"
054300            MOVE NC"ƒŒƒZXV" TO ƒtƒ@ƒCƒ‹–¼‚v
054310            PERFORM ƒGƒ‰[•\Ž¦
054320         END-IF
054330     NOT INVALID KEY
054340         PERFORM ƒŒƒZƒvƒg˜JŽ©ƒŒƒR[ƒhƒZƒbƒg
054350         REWRITE ƒŒƒZ˜JŽ©|ƒŒƒR[ƒh
054360         IF ó‘ÔƒL[ NOT = "00"
054370            MOVE NC"ƒŒƒZXV" TO ƒtƒ@ƒCƒ‹–¼‚v
054380            PERFORM ƒGƒ‰[•\Ž¦
054390         END-IF
054400     END-READ.
054410     PERFORM ŽófŽÒ_®’Ê‰@‚ ‚èXV.
054420*================================================================*
054430 ‚»‚Ì‘¼ƒŒƒR[ƒhƒZƒbƒg SECTION.
054440*
054450********************
054460* ŽófŽÒî•ñƒZƒbƒg *
054470********************
054480     MOVE ˜AŒv|Ž{p˜a—ï      TO ƒŒƒZ|Ž{p˜a—ï.
054490     MOVE ˜AŒv|Ž{p”N        TO ƒŒƒZ|Ž{p”N.
054500     MOVE ˜AŒv|Ž{pŒŽ        TO ƒŒƒZ|Ž{pŒŽ.
054510*
054520     MOVE ˜AŒv|Š³ŽÒ”Ô†      TO ƒŒƒZ|Š³ŽÒ”Ô†.
054530     MOVE ˜AŒv|Ž}”Ô          TO ƒŒƒZ|Ž}”Ô.
054540*
054550*  / ƒL[—pƒŒƒZŽí•Ê‚ÅŽæ“¾ /
054560     MOVE ƒŒƒZŽí•Ê‚v          TO ƒŒƒZ|ƒŒƒZŽí•Ê.
054570*
054580*
054590     MOVE Žó|•ÛŒ¯ŽÒ”Ô†      TO ƒŒƒZ|¿‹•ÛŒ¯ŽÒ”Ô†.
054600*
054610*
054620*
      */ƒŒƒZ[ƒŒƒZ¿‹‹æ•ª‚ðŽg‚¤‚o‚f‚ª‚ ‚éˆ×ƒRƒƒ“ƒg‰ðœ«««/170911
054630* V‹K/Œp‘± ƒ`ƒFƒbƒN
054640         IF ˜A—¿˜JŽ©|‰ŒŸ—¿  NOT = ZERO
054650             MOVE NC"›"   TO V‹Kƒ`ƒFƒbƒN‚v
054660         ELSE
054670             MOVE NC"›"   TO Œp‘±ƒ`ƒFƒbƒN‚v
054680         END-IF
054690
054700*   / 1:V‹K,2:Œp‘±,3:—¼•û /
054710     IF ( V‹Kƒ`ƒFƒbƒN‚v = NC"›" ) AND ( Œp‘±ƒ`ƒFƒbƒN‚v = NC"›" )
054720         MOVE 3  TO ƒŒƒZ|ƒŒƒZ¿‹‹æ•ª
054730     ELSE
054740        IF V‹Kƒ`ƒFƒbƒN‚v = NC"›" 
054750           MOVE 1  TO ƒŒƒZ|ƒŒƒZ¿‹‹æ•ª
054760        ELSE
054770           MOVE 2  TO ƒŒƒZ|ƒŒƒZ¿‹‹æ•ª
054780        END-IF
054790     END-IF.
      */ƒŒƒZ[ƒŒƒZ¿‹‹æ•ª‚ðŽg‚¤‚o‚f‚ª‚ ‚éˆ×ƒRƒƒ“ƒg‰ðœªªª/170911
054800
054810     PERFORM ŽÀ“ú”ƒZƒbƒg.
054820     MOVE ŽÀ“ú”‚v        TO  ƒŒƒZ|ƒŒƒZŽÀ“ú”.
054830*
054840* ˜JÐEŽ©”…ÓEŽ©”ï—p
054850     MOVE ˜AŒv|”ï—pŠz    TO ƒŒƒZ|‡Œv.
054860     MOVE ˜AŒv|•‰’SŠz    TO ƒŒƒZ|ˆê•”•‰’S‹à.
054870     MOVE ˜AŒv|¿‹Šz    TO ƒŒƒZ|¿‹‹àŠz.
054880*
054881*----------------------------------------------------------------------------*
054882     IF ƒŒƒZ|¿‹‹æ•ª = 9
054883        MOVE ZERO TO ƒŒƒZ|¿‹‘ÎÛ‹æ•ª
054884     ELSE
054885        IF ( ŽÀ“ú”‚v NOT = ZERO ) AND ( •‰|•”ˆÊ” NOT = ZERO )
054886           MOVE 1    TO ƒŒƒZ|¿‹‘ÎÛ‹æ•ª
054887        ELSE
054888           MOVE ZERO TO ƒŒƒZ|¿‹‘ÎÛ‹æ•ª
054889        END-IF
054890     END-IF.
054891*----------------------------------------------------------------------------*
054892*
054893*================================================================*
054900 ƒtƒ@ƒCƒ‹‘ž SECTION.
054910*
054920     IF XVƒtƒ‰ƒO NOT = "YES"
054930*        WRITE ƒŒƒZ|ƒŒƒR[ƒh
054940*        IF ó‘ÔƒL[ NOT = "00"
054950*            MOVE NC"ƒŒƒZ’Ç‰Á" TO ƒtƒ@ƒCƒ‹–¼‚v
054960*            PERFORM ƒGƒ‰[•\Ž¦
054970*        END-IF
054980         DISPLAY NC"ŠY“–ŒŽ‚ÌƒŒƒR[ƒh‚ª‚ ‚è‚Ü‚¹‚ñB" UPON CONS
054990         DISPLAY NC"Ž{p˜a—ïF" ˜AŒv|Ž{p˜a—ï  UPON CONS
055000         DISPLAY NC"Ž{p”NF"   ˜AŒv|Ž{p”N    UPON CONS 
055010         DISPLAY NC"Ž{pŒŽF"   ˜AŒv|Ž{pŒŽ    UPON CONS 
055020         DISPLAY NC"Š³ŽÒ”Ô†F" ˜AŒv|Š³ŽÒ”Ô†  UPON CONS
055030         DISPLAY NC"Ž}”ÔF"     ˜AŒv|Ž}”Ô      UPON CONS 
055040         DISPLAY NC"ƒŒƒZŽí•ÊF" ƒŒƒZŽí•Ê‚v      UPON CONS 
055050         MOVE NC"ƒŒƒZƒvƒg‚e" TO ƒtƒ@ƒCƒ‹–¼‚v
055060         PERFORM ƒGƒ‰[•\Ž¦‚q
055070     ELSE
055080        REWRITE ƒŒƒZ|ƒŒƒR[ƒh
055090        IF ó‘ÔƒL[ NOT = "00"
055100            MOVE NC"ƒŒƒZXV" TO ƒtƒ@ƒCƒ‹–¼‚v
055110            PERFORM ƒGƒ‰[•\Ž¦
055120        END-IF
055130     END-IF.
055140*
055150*================================================================*
055160 ƒŒƒR[ƒh‘¶Ýƒ`ƒFƒbƒN SECTION.
055170*
055180* / ƒŒƒZƒvƒg‚e‚ÉƒŒƒR[ƒh‚ª‚ ‚é‚©ƒ`ƒFƒbƒN /
055190*
055200     MOVE SPACE TO XVƒtƒ‰ƒO.
055210*     INITIALIZE ŠeŽíƒtƒ‰ƒO‚v.
055220*
055230     MOVE ˜AŒv|Ž{p˜a—ï        TO ƒŒƒZ|Ž{p˜a—ï.
055240     MOVE ˜AŒv|Ž{p”N          TO ƒŒƒZ|Ž{p”N.
055250     MOVE ˜AŒv|Ž{pŒŽ          TO ƒŒƒZ|Ž{pŒŽ.
055260     MOVE ˜AŒv|Š³ŽÒ”Ô†        TO ƒŒƒZ|Š³ŽÒ”Ô†.
055270     MOVE ˜AŒv|Ž}”Ô            TO ƒŒƒZ|Ž}”Ô.
055280*  / ƒL[—pƒŒƒZŽí•Ê‚ÅŽæ“¾ /
055290     MOVE ƒŒƒZŽí•Ê‚v            TO ƒŒƒZ|ƒŒƒZŽí•Ê.
055300*
055310* -- ƒŒƒR[ƒh‚ª‚È‚¢Žž‚ÍA¿‹”NŒŽ‚ÍŽ{pŒŽ
055320     READ ƒŒƒZƒvƒg‚e
055330     INVALID KEY
055340         MOVE SPACE TO ƒŒƒZ|ƒŒƒR[ƒh
055350         INITIALIZE ƒŒƒZ|ƒŒƒR[ƒh
055360         MOVE ˜AŒv|Ž{p˜a—ï    TO ƒŒƒZ|¿‹˜a—ï
055370         MOVE ˜AŒv|Ž{p”N      TO ƒŒƒZ|¿‹”N
055380         MOVE ˜AŒv|Ž{pŒŽ      TO ƒŒƒZ|¿‹ŒŽ
055390         MOVE ZERO              TO ƒŒƒZ|¿‹‹æ•ª
055400     NOT INVALID KEY
055410         MOVE "YES"             TO XVƒtƒ‰ƒO
055420     END-READ.
055430*
055440*================================================================*
055450 ‰ŒŸ“úˆÈ‘O‚Ìƒf[ƒ^”»’è SECTION.
055460*
055470*********************************************************************************
055480*  Å‰‚Ì‰ŒŸ“úˆÈ‘O‚Ì“–ŒŽ’†‚ÉŽ{p‹L˜^ƒŒƒR[ƒh‚ª‚ ‚Á‚½Žž(Ž¡–üA’†Ž~)‚ÍA¿‹‹æ•ª‚Ì
055490*  Œp‘±‚É‚àƒ`ƒFƒbƒN‚·‚éB(V‹K‚ÆŒp‘±‚Ì—¼•û)
055500*********************************************************************************
055510*
055520** Å‰‚Ì‰ŒŸ“ú‚ðŽæ“¾
055530     MOVE SPACE                 TO ‰ŒŸƒtƒ‰ƒO.
055540     MOVE ˜AŒv|Š³ŽÒ”Ô†        TO Ž{‹L|Š³ŽÒ”Ô†.
055550     MOVE ˜AŒv|Ž}”Ô            TO Ž{‹L|Ž}”Ô.
055560     MOVE ˜AŒv|Ž{p˜a—ï        TO Ž{‹L|Ž{p˜a—ï.
055570     MOVE ˜AŒv|Ž{p”N          TO Ž{‹L|Ž{p”N.
055580     MOVE ˜AŒv|Ž{pŒŽ          TO Ž{‹L|Ž{pŒŽ.
055590     MOVE ZERO                  TO Ž{‹L|Ž{p“ú.
055600     START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
055610                                  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
055620     END-START.
055630     IF ó‘ÔƒL[ = "00"
055640         MOVE ZERO  TO ‰ŒŸ˜a—ï‚v‚s
055650         MOVE ZERO  TO ‰ŒŸ”N‚v‚s
055660         MOVE ZERO  TO ‰ŒŸŒŽ‚v‚s
055670         MOVE ZERO  TO ‰ŒŸ“ú‚v‚s
055680         MOVE SPACE TO I—¹ƒtƒ‰ƒO
055690         PERFORM Ž{p‹L˜^‚e“Çž
055700         PERFORM UNTIL ( I—¹ƒtƒ‰ƒO           = "YES"           ) OR
055710                       ( Ž{‹L|Š³ŽÒƒR[ƒh NOT = ˜AŒv|Š³ŽÒƒR[ƒh) OR
055720                       ( Ž{‹L|Ž{p˜a—ï   NOT = ˜AŒv|Ž{p˜a—ï  ) OR
055730                       ( Ž{‹L|Ž{p”N     NOT = ˜AŒv|Ž{p”N    ) OR
055740                       ( Ž{‹L|Ž{pŒŽ     NOT = ˜AŒv|Ž{pŒŽ    ) 
055750*
055760*         /  Å‰‚Ì‰ŒŸ“ú‚ðŽæ“¾
055770               IF ( Ž{‹L|f—Ã‹æ•ª = 2 ) AND ( ‰ŒŸƒtƒ‰ƒO = SPACE )
055780                   MOVE Ž{‹L|Ž{p˜a—ï           TO ‰ŒŸ˜a—ï‚v‚s
055790                   MOVE Ž{‹L|Ž{p”N             TO ‰ŒŸ”N‚v‚s
055800                   MOVE Ž{‹L|Ž{pŒŽ             TO ‰ŒŸŒŽ‚v‚s
055810                   MOVE Ž{‹L|Ž{p“ú             TO ‰ŒŸ“ú‚v‚s
055820                   MOVE "YES"                    TO ‰ŒŸƒtƒ‰ƒO
055830                   MOVE "YES"                    TO I—¹ƒtƒ‰ƒO
055840               END-IF
055850               PERFORM Ž{p‹L˜^‚e“Çž
055860         END-PERFORM
055870     END-IF.
055880*
055890* ‰ŒŸ“úˆÈ‘O‚Ìƒf[ƒ^”»’è
055900     IF ‰ŒŸƒtƒ‰ƒO = "YES"
055910        MOVE ˜AŒv|Š³ŽÒ”Ô†        TO Ž{‹L|Š³ŽÒ”Ô†
055920        MOVE ˜AŒv|Ž}”Ô            TO Ž{‹L|Ž}”Ô
055930        MOVE ‰ŒŸ˜a—ï‚v‚s          TO Ž{‹L|Ž{p˜a—ï
055940        MOVE ‰ŒŸ”N‚v‚s            TO Ž{‹L|Ž{p”N
055950        MOVE ‰ŒŸŒŽ‚v‚s            TO Ž{‹L|Ž{pŒŽ
055960        MOVE ‰ŒŸ“ú‚v‚s            TO Ž{‹L|Ž{p“ú
055970        START Ž{p‹L˜^‚e   KEY IS <  Ž{‹L|Š³ŽÒƒR[ƒh
055980                                     Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
055990                                     REVERSED
056000        END-START
056010        IF ó‘ÔƒL[ = "00"
056020           MOVE SPACE  TO I—¹ƒtƒ‰ƒO
056030           PERFORM Ž{p‹L˜^‚e“Çž
056040           IF ( I—¹ƒtƒ‰ƒO      = SPACE         ) AND
056050              ( Ž{‹L|Š³ŽÒ”Ô†  = ˜AŒv|Š³ŽÒ”Ô†) AND
056060              ( Ž{‹L|Ž}”Ô      = ˜AŒv|Ž}”Ô    ) AND
056070              ( Ž{‹L|Ž{p˜a—ï  = ‰ŒŸ˜a—ï‚v‚s  ) AND
056080              ( Ž{‹L|Ž{p”N    = ‰ŒŸ”N‚v‚s    ) AND
056090              ( Ž{‹L|Ž{pŒŽ    = ‰ŒŸŒŽ‚v‚s    )
056100*  ‰ŒŸ“úˆÈ‘O‚Ì“–ŒŽ’†‚ÉŽ{p‹L˜^ƒŒƒR[ƒh‚ª‚ ‚Á‚½Žž
056110                IF Œp‘±ƒ`ƒFƒbƒN‚v = SPACE
056120                   MOVE NC"›"    TO Œp‘±ƒ`ƒFƒbƒN‚v
056130                END-IF
056140           END-IF
056150         END-IF
056160     END-IF.
056170*
056180*================================================================*
056190 ŽófŽÒî•ñŽæ“¾ SECTION.
056200*
056210     MOVE ˜AŒv|Ž{p˜a—ï     TO Žó|Ž{p˜a—ï.
056220     MOVE ˜AŒv|Ž{p”N       TO Žó|Ž{p”N  .
056230     MOVE ˜AŒv|Ž{pŒŽ       TO Žó|Ž{pŒŽ  .
056240     MOVE ˜AŒv|Š³ŽÒ”Ô†     TO Žó|Š³ŽÒ”Ô†.
056250     MOVE ˜AŒv|Ž}”Ô         TO Žó|Ž}”Ô    .
056260     READ ŽófŽÒî•ñ‚e
056270     INVALID KEY
056280         MOVE NC"Žóf" TO ƒtƒ@ƒCƒ‹–¼‚v
056290         PERFORM ƒGƒ‰[•\Ž¦
056300         PERFORM I—¹ˆ—
056310         EXIT PROGRAM
056320     END-READ.
056330*================================================================*
056340 ƒŒƒZƒvƒg˜JŽ©ƒŒƒR[ƒhƒZƒbƒg SECTION.
056350*
056360     MOVE ˜A—¿˜JŽ©|•‰’S—¦              TO ƒŒƒZ˜JŽ©|•‰’S—¦.
056370     MOVE ˜A—¿˜JŽ©|‰ŒŸ—¿              TO ƒŒƒZ˜JŽ©|‰ŒŸ—¿.
056380     MOVE ˜A—¿˜JŽ©|‰ŒŸ‰ñ”            TO ƒŒƒZ˜JŽ©|‰ŒŸ‰ñ”.
056390     MOVE ˜A—¿˜JŽ©|‰ŒŸ’P‰¿            TO ƒŒƒZ˜JŽ©|‰ŒŸ’P‰¿.
056400     MOVE ˜A—¿˜JŽ©|‰ŒŸŽžŠÔŠO          TO ƒŒƒZ˜JŽ©|‰ŒŸŽžŠÔŠO.
056410     MOVE ˜A—¿˜JŽ©|‰ŒŸ‹x“ú            TO ƒŒƒZ˜JŽ©|‰ŒŸ‹x“ú.
056420     MOVE ˜A—¿˜JŽ©|‰ŒŸ[–é            TO ƒŒƒZ˜JŽ©|‰ŒŸ[–é.
056430     MOVE ˜A—¿˜JŽ©|‰ŒŸ‰ÁŽZ—¿          TO ƒŒƒZ˜JŽ©|‰ŒŸ‰ÁŽZ—¿.
056440     MOVE ˜A—¿˜JŽ©|‰ŒŸŽž‘Š’k—¿        TO ƒŒƒZ˜JŽ©|‰ŒŸŽž‘Š’k—¿.
056450     MOVE ˜A—¿˜JŽ©|ÄŒŸ—¿              TO ƒŒƒZ˜JŽ©|ÄŒŸ—¿.
056460     MOVE ˜A—¿˜JŽ©|ÄŒŸ‰ñ”            TO ƒŒƒZ˜JŽ©|ÄŒŸ‰ñ”.
056470     MOVE ˜A—¿˜JŽ©|ÄŒŸ’P‰¿            TO ƒŒƒZ˜JŽ©|ÄŒŸ’P‰¿.
056480     MOVE ˜A—¿˜JŽ©|‰—Ã‹——£            TO ƒŒƒZ˜JŽ©|‰—Ã‹——£.
056490     MOVE ˜A—¿˜JŽ©|‰—Ã‰ñ”            TO ƒŒƒZ˜JŽ©|‰—Ã‰ñ”.
056500     MOVE ˜A—¿˜JŽ©|‰—Ã’P‰¿            TO ƒŒƒZ˜JŽ©|‰—Ã’P‰¿.
056510     MOVE ˜A—¿˜JŽ©|‰—Ã—¿              TO ƒŒƒZ˜JŽ©|‰—Ã—¿.
056520     MOVE ˜A—¿˜JŽ©|‰—Ã‰ÁŽZ—¿          TO ƒŒƒZ˜JŽ©|‰—Ã‰ÁŽZ—¿.
056530     MOVE ˜A—¿˜JŽ©|‰—Ã–éŠÔ            TO ƒŒƒZ˜JŽ©|‰—Ã–éŠÔ.
056540     MOVE ˜A—¿˜JŽ©|‰—Ã“ï˜H            TO ƒŒƒZ˜JŽ©|‰—Ã“ï˜H.
056550     MOVE ˜A—¿˜JŽ©|‰—Ã–\•—‰J          TO ƒŒƒZ˜JŽ©|‰—Ã–\•—‰J.
056560     MOVE ˜A—¿˜JŽ©|‰—ÃŽžŠÔŠO          TO ƒŒƒZ˜JŽ©|‰—ÃŽžŠÔŠO.
056570     MOVE ˜A—¿˜JŽ©|‹à‘®•›Žq‰ÁŽZ—¿      TO ƒŒƒZ˜JŽ©|‹à‘®•›Žq‰ÁŽZ—¿.
056580     MOVE ˜A—¿˜JŽ©|‘å                  TO ƒŒƒZ˜JŽ©|‘å.
056590     MOVE ˜A—¿˜JŽ©|’†                  TO ƒŒƒZ˜JŽ©|’†.
056600     MOVE ˜A—¿˜JŽ©|¬                  TO ƒŒƒZ˜JŽ©|¬.
056610     MOVE ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ‰ñ”        TO ƒŒƒZ˜JŽ©|î•ñ’ñ‹Ÿ‰ñ”.
056620     MOVE ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ’P‰¿        TO ƒŒƒZ˜JŽ©|î•ñ’ñ‹Ÿ’P‰¿.
056630     MOVE ˜A—¿˜JŽ©|Ž{pî•ñ’ñ‹Ÿ—¿      TO ƒŒƒZ˜JŽ©|Ž{pî•ñ’ñ‹Ÿ—¿.
056640     MOVE ˜A—¿˜JŽ©|¬Œv                TO ƒŒƒZ˜JŽ©|¬Œv.
056650     MOVE ˜A—¿˜JŽ©|”ï—pŠz              TO ƒŒƒZ˜JŽ©|”ï—pŠz.
056660     MOVE ˜A—¿˜JŽ©|ˆê•”•‰’S‹à          TO ƒŒƒZ˜JŽ©|ˆê•”•‰’S‹à.
056670     MOVE ˜A—¿˜JŽ©|¿‹Šz              TO ƒŒƒZ˜JŽ©|¿‹Šz.
056680     MOVE ˜A—¿˜JŽ©|‹‹•tŠ„‡            TO ƒŒƒZ˜JŽ©|‹‹•tŠ„‡.
056690     MOVE ˜A—¿˜JŽ©|Žó‹‹ŽÒ•‰’SŠz        TO ƒŒƒZ˜JŽ©|Žó‹‹ŽÒ•‰’SŠz.
056700     MOVE ˜A—¿˜JŽ©|•¬¿‹‹àŠz        TO ƒŒƒZ˜JŽ©|•¬¿‹‹àŠz.
056710     MOVE ˜A—¿˜JŽ©|Žw“±ŠÇ—‰ñ”        TO ƒŒƒZ˜JŽ©|Žw“±ŠÇ—‰ñ”.
056720     MOVE ˜A—¿˜JŽ©|Žw“±ŠÇ—’P‰¿        TO ƒŒƒZ˜JŽ©|Žw“±ŠÇ—’P‰¿.
056730     MOVE ˜A—¿˜JŽ©|Žw“±ŠÇ——¿          TO ƒŒƒZ˜JŽ©|Žw“±ŠÇ——¿.
056740     MOVE ˜A—¿˜JŽ©|‰^“®—Ã–@‰ñ”        TO ƒŒƒZ˜JŽ©|‰^“®—Ã–@‰ñ”.
056750     MOVE ˜A—¿˜JŽ©|‰^“®—Ã–@’P‰¿        TO ƒŒƒZ˜JŽ©|‰^“®—Ã–@’P‰¿.
056760     MOVE ˜A—¿˜JŽ©|‰^“®—Ã–@—¿          TO ƒŒƒZ˜JŽ©|‰^“®—Ã–@—¿.
056770     MOVE ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‰ñ”    TO ƒŒƒZ˜JŽ©|‹x‹Æ•âžØ–¾‰ñ”.
056780     MOVE ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾’P‰¿    TO ƒŒƒZ˜JŽ©|‹x‹Æ•âžØ–¾’P‰¿.
056790     MOVE ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‘—¿    TO ƒŒƒZ˜JŽ©|‹x‹Æ•âžØ–¾‘—¿.
056800     MOVE ˜A—¿˜JŽ©|h”‘‰ñ”            TO ƒŒƒZ˜JŽ©|h”‘‰ñ”.
056810     MOVE ˜A—¿˜JŽ©|h”‘’P‰¿            TO ƒŒƒZ˜JŽ©|h”‘’P‰¿.
056820     MOVE ˜A—¿˜JŽ©|h”‘—¿              TO ƒŒƒZ˜JŽ©|h”‘—¿.
056830     MOVE ˜A—¿˜JŽ©|HŽ–‰ñ”            TO ƒŒƒZ˜JŽ©|HŽ–‰ñ”.
056840     MOVE ˜A—¿˜JŽ©|HŽ–’P‰¿            TO ƒŒƒZ˜JŽ©|HŽ–’P‰¿.
056850     MOVE ˜A—¿˜JŽ©|HŽ–—¿              TO ƒŒƒZ˜JŽ©|HŽ–—¿.
056860     MOVE ˜A—¿˜JŽ©|Ž{pØ–¾‰ñ”        TO ƒŒƒZ˜JŽ©|Ž{pØ–¾‰ñ”.
056870     MOVE ˜A—¿˜JŽ©|Ž{pØ–¾’P‰¿        TO ƒŒƒZ˜JŽ©|Ž{pØ–¾’P‰¿.
056880     MOVE ˜A—¿˜JŽ©|Ž{pØ–¾‘—¿        TO ƒŒƒZ˜JŽ©|Ž{pØ–¾‘—¿.
056890     MOVE ˜A—¿˜JŽ©|Ž{p–¾×‰ñ”        TO ƒŒƒZ˜JŽ©|Ž{p–¾×‰ñ”.
056900     MOVE ˜A—¿˜JŽ©|Ž{p–¾×’P‰¿        TO ƒŒƒZ˜JŽ©|Ž{p–¾×’P‰¿.
056910     MOVE ˜A—¿˜JŽ©|Ž{p–¾×‘—¿        TO ƒŒƒZ˜JŽ©|Ž{p–¾×‘—¿.
056920     MOVE ˜A—¿˜JŽ©|Ž©”…Ó‚»‚Ì‘¼‹àŠz    TO ƒŒƒZ˜JŽ©|Ž©”…Ó‚»‚Ì‘¼‹àŠz.
056930     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 10
056940         MOVE ˜A—¿˜JŽ©|“–“ú•ª•‰’SŠz(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|“–“ú•ª•‰’SŠz(ƒJƒEƒ“ƒ^)
056950     END-PERFORM.
056960     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 7
056970         MOVE ˜A—¿˜JŽ©|‰‰ñˆ’u—¿  (ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|‰‰ñˆ’u—¿  (ƒJƒEƒ“ƒ^)
056980         MOVE ˜A—¿˜JŽ©|Þ—¿”ï      (ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|Þ—¿”ï      (ƒJƒEƒ“ƒ^)
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚ÌŒðŠ·‚ÍA‚Q‰ñ‚Ü‚ÅŽZ’è‚Å‚«‚éB/20180830
               MOVE ˜A—¿˜JŽ©|Þ—¿”ï’P‰¿  (ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|Þ—¿”ï’P‰¿  (ƒJƒEƒ“ƒ^)
026560         MOVE ˜A—¿˜JŽ©|Þ—¿”ï‰ñ”  (ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|Þ—¿”ï‰ñ”  (ƒJƒEƒ“ƒ^)
HILO  *         DISPLAY "Þ—¿”ï’P‰¿(" ƒJƒEƒ“ƒ^ ") " ˜A—¿˜JŽ©|Þ—¿”ï’P‰¿(ƒJƒEƒ“ƒ^)
HILO  *                " Þ—¿”ï‰ñ”(" ƒJƒEƒ“ƒ^ ") " ˜A—¿˜JŽ©|Þ—¿”ï‰ñ”(ƒJƒEƒ“ƒ^)
056990         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚P(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·“ú‚P(ƒJƒEƒ“ƒ^)
057000         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚Q(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·“ú‚Q(ƒJƒEƒ“ƒ^)
057010         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚R(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·“ú‚R(ƒJƒEƒ“ƒ^)
057020         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚S(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·“ú‚S(ƒJƒEƒ“ƒ^)
057030         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚T(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·“ú‚T(ƒJƒEƒ“ƒ^)
057040         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚U(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·“ú‚U(ƒJƒEƒ“ƒ^)
057050         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·—¿  (ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·—¿  (ƒJƒEƒ“ƒ^)
057060         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·’P‰¿(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·’P‰¿(ƒJƒEƒ“ƒ^)
057070         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·‰ñ”(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·‰ñ”(ƒJƒEƒ“ƒ^)
057080     END-PERFORM.
057090     MOVE ˜A—¿˜JŽ©|‰‰ñˆ’u—¿Œv               TO ƒŒƒZ˜JŽ©|‰‰ñˆ’u—¿Œv
057100     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚P                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚P.
057110     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚P                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚P.
057120     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚P                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚P.
057130     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚P               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚P.
057140     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚P               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚P.
057150     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚P                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚P.
057160     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚P               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚P.
057170     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚P               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚P.
057180     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚P                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚P.
057190     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚P                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚P.
057200     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚P                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚P.
057210     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚P                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚P.
057220     MOVE ˜A—¿˜JŽ©|¬Œv‚P                     TO ƒŒƒZ˜JŽ©|¬Œv‚P.
057230     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚P               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚P.
057240     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚P               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚P.
057250     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚PSk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚PSk.
057260     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚PSk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚PSk.
057270     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚PSk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚PSk.
057280     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚PSk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚PSk.
057290     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚PSk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚PSk.
057300     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚PSk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚PSk.
057310     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚PSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚PSk.
057320     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚PSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚PSk.
057330     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚PSk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚PSk.
057340     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚PSk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚PSk.
057350     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚PSk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚PSk.
057360     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚PSk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚PSk.
057370     MOVE ˜A—¿˜JŽ©|¬Œv‚PSk                 TO ƒŒƒZ˜JŽ©|¬Œv‚PSk.
057380     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚PSk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚PSk.
057390     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚PSk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚PSk.
057400     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚P           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚P.
057410     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚P           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚P.
057420     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚P             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚P.
057430     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚P             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚P.
057440     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚P             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚P.
057450     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚P               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚P.
057460     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚P       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚P.
057470     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚P       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚P.
057480     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚P         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚P.
057490     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚Q                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚Q.
057500     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚Q                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚Q.
057510     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚Q                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚Q.
057520     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚Q               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚Q.
057530     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚Q               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚Q.
057540     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚Q                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚Q.
057550     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚Q               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚Q.
057560     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚Q               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚Q.
057570     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚Q                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚Q.
057580     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚Q                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚Q.
057590     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚Q                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚Q.
057600     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚Q                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚Q.
057610     MOVE ˜A—¿˜JŽ©|¬Œv‚Q                     TO ƒŒƒZ˜JŽ©|¬Œv‚Q.
057620     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚Q               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚Q.
057630     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚Q               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚Q.
057640     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚QSk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚QSk.
057650     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚QSk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚QSk.
057660     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚QSk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚QSk.
057670     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚QSk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚QSk.
057680     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚QSk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚QSk.
057690     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚QSk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚QSk.
057700     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚QSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚QSk.
057710     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚QSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚QSk.
057720     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚QSk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚QSk.
057730     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚QSk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚QSk.
057740     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚QSk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚QSk.
057750     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚QSk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚QSk.
057760     MOVE ˜A—¿˜JŽ©|¬Œv‚QSk                 TO ƒŒƒZ˜JŽ©|¬Œv‚QSk.
057770     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚QSk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚QSk.
057780     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚QSk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚QSk.
057790     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚Q           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚Q.
057800     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚Q           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚Q.
057810     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚Q             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚Q.
057820     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚Q             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚Q.
057830     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚Q             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚Q.
057840     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚Q               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚Q.
057850     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚Q       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚Q.
057860     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚Q       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚Q.
057870     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚Q         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚Q.
057880     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚R                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚R.
057890     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚R                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚R.
057900     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚R                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚R.
057910     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚R               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚R.
057920     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚R               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚R.
057930     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚R                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚R.
057940     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚R               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚R.
057950     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚R               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚R.
057960     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚R                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚R.
057970     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚R                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚R.
057980     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚R                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚R.
057990     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚R                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚R.
058000     MOVE ˜A—¿˜JŽ©|¬Œv‚R                     TO ƒŒƒZ˜JŽ©|¬Œv‚R.
058010     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚R               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚R.
058020     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚R               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚R.
058030     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚RSk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚RSk.
058040     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚RSk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚RSk.
058050     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚RSk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚RSk.
058060     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚RSk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚RSk.
058070     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚RSk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚RSk.
058080     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚RSk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚RSk.
058090     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚RSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚RSk.
058100     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚RSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚RSk.
058110     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚RSk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚RSk.
058120     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚RSk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚RSk.
058130     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚RSk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚RSk.
058140     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚RSk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚RSk.
058150     MOVE ˜A—¿˜JŽ©|¬Œv‚RSk                 TO ƒŒƒZ˜JŽ©|¬Œv‚RSk.
058160     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚RSk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚RSk.
058170     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚RSk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚RSk.
058180     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚R           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚R.
058190     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚R           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚R.
058200     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚R             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚R.
058210     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚R             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚R.
058220     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚R             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚R.
058230     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚R               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚R.
058240     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚R       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚R.
058250     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚R       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚R.
058260     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚R         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚R.
058270     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚S                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚S.
058280     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚S                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚S.
058290     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚S                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚S.
058300     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚S               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚S.
058310     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚S               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚S.
058320     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚S                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚S.
058330     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚S               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚S.
058340     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚S               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚S.
058350     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚S                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚S.
058360     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚S                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚S.
058370     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚S                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚S.
058380     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚S                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚S.
058390     MOVE ˜A—¿˜JŽ©|¬Œv‚S                     TO ƒŒƒZ˜JŽ©|¬Œv‚S.
058400     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚S               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚S.
058410     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚S               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚S.
058420     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚SSk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚SSk.
058430     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚SSk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚SSk.
058440     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚SSk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚SSk.
058450     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚SSk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚SSk.
058460     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚SSk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚SSk.
058470     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚SSk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚SSk.
058480     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚SSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚SSk.
058490     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚SSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚SSk.
058500     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚SSk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚SSk.
058510     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚SSk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚SSk.
058520     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚SSk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚SSk.
058530     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚SSk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚SSk.
058540     MOVE ˜A—¿˜JŽ©|¬Œv‚SSk                 TO ƒŒƒZ˜JŽ©|¬Œv‚SSk.
058550     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚SSk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚SSk.
058560     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚SSk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚SSk.
058570     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚S           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚S.
058580     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚S           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚S.
058590     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚S             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚S.
058600     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚S             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚S.
058610     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚S             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚S.
058620     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚S               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚S.
058630     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚S       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚S.
058640     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚S       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚S.
058650     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚S         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚S.
058660     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚T                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚T.
058670     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚T                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚T.
058680     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚T                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚T.
058690     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚T               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚T.
058700     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚T               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚T.
058710     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚T                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚T.
058720     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚T               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚T.
058730     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚T               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚T.
058740     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚T                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚T.
058750     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚T                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚T.
058760     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚T                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚T.
058770     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚T                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚T.
058780     MOVE ˜A—¿˜JŽ©|¬Œv‚T                     TO ƒŒƒZ˜JŽ©|¬Œv‚T.
058790     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚T               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚T.
058800     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚T               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚T.
058810     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚TSk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚TSk.
058820     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚TSk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚TSk.
058830     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚TSk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚TSk.
058840     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚TSk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚TSk.
058850     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚TSk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚TSk.
058860     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚TSk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚TSk.
058870     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚TSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚TSk.
058880     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚TSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚TSk.
058890     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚TSk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚TSk.
058900     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚TSk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚TSk.
058910     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚TSk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚TSk.
058920     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚TSk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚TSk.
058930     MOVE ˜A—¿˜JŽ©|¬Œv‚TSk                 TO ƒŒƒZ˜JŽ©|¬Œv‚TSk.
058940     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚TSk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚TSk.
058950     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚TSk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚TSk.
058960     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚T           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚T.
058970     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚T           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚T.
058980     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚T             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚T.
058990     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚T             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚T.
059000     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚T             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚T.
059010     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚T               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚T.
059020     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚T       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚T.
059030     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚T       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚T.
059040     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚T         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚T.
059050     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚U                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚U.
059060     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚U                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚U.
059070     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚U                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚U.
059080     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚U               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚U.
059090     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚U               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚U.
059100     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚U                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚U.
059110     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚U               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚U.
059120     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚U               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚U.
059130     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚U                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚U.
059140     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚U                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚U.
059150     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚U                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚U.
059160     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚U                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚U.
059170     MOVE ˜A—¿˜JŽ©|¬Œv‚U                     TO ƒŒƒZ˜JŽ©|¬Œv‚U.
059180     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚U               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚U.
059190     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚U               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚U.
059200     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚USk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚USk.
059210     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚USk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚USk.
059220     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚USk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚USk.
059230     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚USk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚USk.
059240     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚USk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚USk.
059250     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚USk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚USk.
059260     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚USk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚USk.
059270     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚USk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚USk.
059280     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚USk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚USk.
059290     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚USk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚USk.
059300     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚USk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚USk.
059310     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚USk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚USk.
059320     MOVE ˜A—¿˜JŽ©|¬Œv‚USk                 TO ƒŒƒZ˜JŽ©|¬Œv‚USk.
059330     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚USk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚USk.
059340     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚USk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚USk.
059350     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚U           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚U.
059360     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚U           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚U.
059370     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚U             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚U.
059380     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚U             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚U.
059390     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚U             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚U.
059400     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚U               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚U.
059410     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚U       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚U.
059420     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚U       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚U.
059430     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚U         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚U.
059440     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚V                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚V.
059450     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚V                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚V.
059460     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚V                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚V.
059470     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚V               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚V.
059480     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚V               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚V.
059490     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚V                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚V.
059500     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚V               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚V.
059510     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚V               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚V.
059520     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚V                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚V.
059530     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚V                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚V.
059540     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚V                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚V.
059550     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚V                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚V.
059560     MOVE ˜A—¿˜JŽ©|¬Œv‚V                     TO ƒŒƒZ˜JŽ©|¬Œv‚V.
059570     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚V               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚V.
059580     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚V               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚V.
059590     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚VSk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚VSk.
059600     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚VSk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚VSk.
059610     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚VSk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚VSk.
059620     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚VSk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚VSk.
059630     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚VSk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚VSk.
059640     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚VSk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚VSk.
059650     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚VSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚VSk.
059660     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚VSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚VSk.
059670     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚VSk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚VSk.
059680     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚VSk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚VSk.
059690     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚VSk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚VSk.
059700     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚VSk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚VSk.
059710     MOVE ˜A—¿˜JŽ©|¬Œv‚VSk                 TO ƒŒƒZ˜JŽ©|¬Œv‚VSk.
059720     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚VSk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚VSk.
059730     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚VSk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚VSk.
059740     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚V           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚V.
059750     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚V           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚V.
059760     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚V             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚V.
059770     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚V             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚V.
059780     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚V             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚V.
059790     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚V               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚V.
059800     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚V       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚V.
059810     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚V       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚V.
059820     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚V         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚V.
059830*/—âãª–@‚Æ‰·ãª–@‚Ì’P‰¿‚ªˆá‚¤ê‡‚Ì–¾×‘AŒvŽZ‘‚Ì‹L“ü‚Ìˆ×‚Ì‘Î‰žB
059840     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 7
059850         PERFORM VARYING ƒJƒEƒ“ƒ^‚Q FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^‚Q > 6
059860             MOVE ˜A—¿˜JŽ©‚Q|ãª–@’P‰¿(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q) TO ƒŒƒZ˜JŽ©|ãª–@’P‰¿(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q)
059870             MOVE ˜A—¿˜JŽ©‚Q|ãª–@‰ñ”(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q) TO ƒŒƒZ˜JŽ©|ãª–@‰ñ”(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q)
059880         END-PERFORM
059890     END-PERFORM.
059900     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 7
059910         PERFORM VARYING ƒJƒEƒ“ƒ^‚Q FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^‚Q > 2
059920             MOVE ˜A—¿˜JŽ©‚Q|“d—Ã’P‰¿(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q) TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q)
059930             MOVE ˜A—¿˜JŽ©‚Q|“d—Ã‰ñ”(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q) TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q)
059940         END-PERFORM
059950     END-PERFORM.
059960*================================================================*
059970* ƒŒƒZƒvƒg˜JŽ©‘ž SECTION.
059980**
059990*     REWRITE ƒŒƒZ˜JŽ©|ƒŒƒR[ƒh.
060000*     IF ó‘ÔƒL[ NOT = "00"
060010*        MOVE NC"ƒŒƒZXV" TO ƒtƒ@ƒCƒ‹–¼‚v
060020*        PERFORM ƒGƒ‰[•\Ž¦
060030*     END-IF.
060040*================================================================*
060050 ŽófŽÒ_®’Ê‰@‚ ‚èXV SECTION.
060060*
060070* ŽÀ“ú”‚ ‚è‚È‚µ¨ŽófŽÒî•ñ‚e‚Ì_®—L‹æ•ª‚ðXV‚·‚éB
060080* 0F_®’Ê‰@‚È‚µ 1F_®’Ê‰@‚ ‚è
060090*     05 _®—L‹æ•ª                     PIC 9.
060100*
060110     MOVE ˜AŒv|Ž{p˜a—ï     TO Žó|Ž{p˜a—ï.
060120     MOVE ˜AŒv|Ž{p”N       TO Žó|Ž{p”N.
060130     MOVE ˜AŒv|Ž{pŒŽ       TO Žó|Ž{pŒŽ.
060140     MOVE ˜AŒv|Š³ŽÒ”Ô†     TO Žó|Š³ŽÒ”Ô†.
060150     MOVE ˜AŒv|Ž}”Ô         TO Žó|Ž}”Ô.
060160     READ ŽófŽÒî•ñ‚e
060170     NOT INVALID KEY
060180*
060190         IF ŽÀ“ú”‚v NOT = ZERO
060200            MOVE 1    TO Žó|_®—L‹æ•ª
060210         ELSE
060220            MOVE ZERO TO Žó|_®—L‹æ•ª
060230         END-IF
060240*
060250         REWRITE Žó|ƒŒƒR[ƒh
060260         IF ó‘ÔƒL[ NOT = "00"
060270            MOVE NC"ŽófŽÒXV" TO ƒtƒ@ƒCƒ‹–¼‚v
060280            PERFORM ƒGƒ‰[•\Ž¦
060290         END-IF
060300     END-READ.
060310*
060320*================================================================*
060330*================================================================*
060340 ŽÀ“ú”ƒZƒbƒg SECTION.
060350*
060360     MOVE ZERO      TO  ŽÀ“ú”‚v.
060370*
060380     MOVE ˜AŒv|Š³ŽÒ”Ô†        TO Ž{‹L|Š³ŽÒ”Ô†.
060390     MOVE ˜AŒv|Ž}”Ô            TO Ž{‹L|Ž}”Ô.
060400     MOVE ˜AŒv|Ž{p˜a—ï        TO Ž{‹L|Ž{p˜a—ï.
060410     MOVE ˜AŒv|Ž{p”N          TO Ž{‹L|Ž{p”N.
060420     MOVE ˜AŒv|Ž{pŒŽ          TO Ž{‹L|Ž{pŒŽ.
060430     MOVE ZERO                  TO Ž{‹L|Ž{p“ú
060440*
060450     START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
060460                                  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
060470     END-START.
060480     IF ó‘ÔƒL[ = "00"
060490         MOVE SPACE TO I—¹ƒtƒ‰ƒO
060500         PERFORM Ž{p‹L˜^‚e“Çž
060510         PERFORM UNTIL ( Ž{‹L|Ž{p˜a—ï   NOT = ˜AŒv|Ž{p˜a—ï ) OR
060520                       ( Ž{‹L|Ž{p”N     NOT = ˜AŒv|Ž{p”N   ) OR
060530                       ( Ž{‹L|Ž{pŒŽ     NOT = ˜AŒv|Ž{pŒŽ   ) OR
060540                       ( Ž{‹L|Š³ŽÒƒR[ƒh NOT = ˜AŒv|Š³ŽÒƒR[ƒh ) OR
060550                       ( I—¹ƒtƒ‰ƒO       NOT = SPACE )
060560*
060570             COMPUTE ŽÀ“ú”‚v = ŽÀ“ú”‚v + 1
060580*
060590             PERFORM Ž{p‹L˜^‚e“Çž
060600         END-PERFORM
060610     END-IF.
060620*
060630*================================================================*
       Ž©”…Óî•ñŽæ“¾ SECTION.
      *
           MOVE ˜AŒv|Š³ŽÒƒR[ƒh   TO Ž©”…|Š³ŽÒƒR[ƒh.
           MOVE ˜AŒv|Ž{p˜a—ï”NŒŽ TO Ž©”…|Ž{p˜a—ï”NŒŽ.
           READ Ž©”…Óî•ñ‚e
           INVALID KEY
               INITIALIZE Ž©”…|ƒŒƒR[ƒh
052280*         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
052290*         DISPLAY "Žw’è‚ÌŽófŽÒî•ñ‚ª‚ ‚è‚Ü‚¹‚ñ"
052300*                 " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
052310*                 " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
052320**-----------------------------------------*
052330*         CALL "actcshm"  WITH C LINKAGE
052340**-----------------------------------------*
052350*         ACCEPT  ƒL[“ü—Í FROM CONS
052360*         PERFORM ƒtƒ@ƒCƒ‹•Â½
052370*         MOVE ZERO TO PROGRAM-STATUS
052380*         EXIT PROGRAM
052430     END-READ.
060630*================================================================*
       ‰ïŒv‚O‚QƒŒƒR[ƒhƒZƒbƒg SECTION.
      *
           MOVE ‰ŒŸ—¿‚v          TO ‰ï‚O‚Q|‰ŒŸ—¿.
           MOVE ‰ŒŸŽžŠÔŠO‚m‚v    TO ‰ï‚O‚Q|‰ŒŸŽžŠÔŠO.
           MOVE ‰ŒŸ‹x“ú‚m‚v      TO ‰ï‚O‚Q|‰ŒŸ‹x“ú.
           MOVE ‰ŒŸ[–é‚m‚v      TO ‰ï‚O‚Q|‰ŒŸ[–é.
           MOVE ‰ŒŸ‰ÁŽZ—¿‚v      TO ‰ï‚O‚Q|‰ŒŸ‰ÁŽZ—¿.
           MOVE ‰ŒŸŽž‘Š’k—¿‚v    TO ‰ï‚O‚Q|‰ŒŸŽž‘Š’k—¿.
           MOVE ÄŒŸ—¿‚v          TO ‰ï‚O‚Q|ÄŒŸ—¿.
           MOVE ‰—Ã‹——£‚v        TO ‰ï‚O‚Q|‰—Ã‹——£.
           MOVE ‰—Ã—¿‚v          TO ‰ï‚O‚Q|‰—Ã—¿.
           MOVE ‰—Ã‰ÁŽZ—¿‚v      TO ‰ï‚O‚Q|‰—Ã‰ÁŽZ—¿.
           MOVE ‰—Ã–éŠÔ‚m‚v      TO ‰ï‚O‚Q|‰—Ã–éŠÔ.
           MOVE ‰—Ã“ï˜H‚m‚v      TO ‰ï‚O‚Q|‰—Ã“ï˜H.
           MOVE ‰—Ã–\•—‰J‚m‚v    TO ‰ï‚O‚Q|‰—Ã–\•—‰J.
           MOVE ‰—ÃŽžŠÔŠO‚m‚v    TO ‰ï‚O‚Q|‰—ÃŽžŠÔŠO.
           MOVE Ž{pî•ñ’ñ‹Ÿ—¿‚v  TO ‰ï‚O‚Q|Ž{pî•ñ’ñ‹Ÿ—¿.
           MOVE Žw“±ŠÇ——¿‚v      TO ‰ï‚O‚Q|Žw“±ŠÇ——¿.
           MOVE ‰^“®—Ã–@—¿‚v      TO ‰ï‚O‚Q|‰^“®—Ã–@—¿.
           MOVE ‹x‹ÆØ–¾‘—¿‚v    TO ‰ï‚O‚Q|‹x‹Æ•âžØ–¾‘—¿.
           MOVE h”‘—¿‚v          TO ‰ï‚O‚Q|h”‘—¿.
           MOVE HŽ–—¿‚v          TO ‰ï‚O‚Q|HŽ–—¿.
           MOVE Ž{pØ–¾‘—¿‚v    TO ‰ï‚O‚Q|Ž{pØ–¾‘—¿.
           MOVE Ž{p–¾×‘—¿‚v    TO ‰ï‚O‚Q|Ž{p–¾×‘—¿.
           MOVE ‚»‚Ì‘¼‹àŠz‚v      TO ‰ï‚O‚Q|Ž©”…Ó‚»‚Ì‘¼‹àŠz.
      * •‰•”ˆÊ–ˆ‚Ì—¿‹à
           PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 7
               MOVE ‰‰ñˆ’u—¿‚m‚n‚v(ƒJƒEƒ“ƒ^) TO ‰ï‚O‚Q|‰‰ñˆ’u—¿(ƒJƒEƒ“ƒ^)
      *         MOVE Þ—¿”ï‚Q‚v      (ƒJƒEƒ“ƒ^) TO ‰ï‚O‚Q|Þ—¿”ï    (ƒJƒEƒ“ƒ^)
               MOVE Þ—¿”ï‚m‚v      (ƒJƒEƒ“ƒ^) TO ‰ï‚O‚Q|Þ—¿”ï    (ƒJƒEƒ“ƒ^)
               MOVE ŒðŠ·•ï‘Ñ—¿‚m‚v  (ƒJƒEƒ“ƒ^) TO ‰ï‚O‚Q|•ï‘ÑŒðŠ·—¿(ƒJƒEƒ“ƒ^)
           END-PERFORM.
           MOVE ‰‰ñˆ’u—¿‚v      TO ‰ï‚O‚Q|‰‰ñˆ’u—¿Œv.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚P   = Œã—Ã—¿‚P‚m‚q‚v     + Œã—Ã—¿‚PSk‚m‚q‚v + ‘½•”ˆÊŒã—Ã—¿‚P‚m‚q‚v +
                                        ’·ŠúŒã—Ã—¿‚P‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚P‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚P = —âãª–@—¿‚P‚m‚q‚v   + —âãª–@—¿‚PSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚P = ‰·ãª–@—¿‚P‚m‚q‚v   + ‰·ãª–@—¿‚PSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚P   = “d—Ã—¿‚P‚m‚q‚v     + “d—Ã—¿‚PSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚Q   = Œã—Ã—¿‚Q‚m‚q‚v     + Œã—Ã—¿‚QSk‚m‚q‚v + ‘½•”ˆÊŒã—Ã—¿‚Q‚m‚q‚v +
                                        ’·ŠúŒã—Ã—¿‚Q‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚Q‚m‚q‚v .
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚Q = —âãª–@—¿‚Q‚m‚q‚v   + —âãª–@—¿‚QSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚Q = ‰·ãª–@—¿‚Q‚m‚q‚v   + ‰·ãª–@—¿‚QSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚Q   = “d—Ã—¿‚Q‚m‚q‚v     + “d—Ã—¿‚QSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚R   = Œã—Ã—¿‚R‚m‚q‚v     + Œã—Ã—¿‚RSk‚m‚q‚v + ‘½•”ˆÊŒã—Ã—¿‚R‚m‚q‚v +
                                        ’·ŠúŒã—Ã—¿‚R‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚R‚m‚q‚v .
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚R = —âãª–@—¿‚R‚m‚q‚v   + —âãª–@—¿‚RSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚R = ‰·ãª–@—¿‚R‚m‚q‚v   + ‰·ãª–@—¿‚RSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚R   = “d—Ã—¿‚R‚m‚q‚v     + “d—Ã—¿‚RSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚S   = Œã—Ã—¿‚S‚m‚q‚v     + Œã—Ã—¿‚SSk‚m‚q‚v + ‘½•”ˆÊŒã—Ã—¿‚S‚m‚q‚v +
                                        ’·ŠúŒã—Ã—¿‚S‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚S‚m‚q‚v .
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚S = —âãª–@—¿‚S‚m‚q‚v   + —âãª–@—¿‚SSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚S = ‰·ãª–@—¿‚S‚m‚q‚v   + ‰·ãª–@—¿‚SSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚S   = “d—Ã—¿‚S‚m‚q‚v     + “d—Ã—¿‚SSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚T   = Œã—Ã—¿‚T‚m‚q‚v     + Œã—Ã—¿‚TSk‚m‚q‚v + ‘½•”ˆÊŒã—Ã—¿‚T‚m‚q‚v +
                                        ’·ŠúŒã—Ã—¿‚T‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚T‚m‚q‚v .
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚T = —âãª–@—¿‚T‚m‚q‚v   + —âãª–@—¿‚TSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚T = ‰·ãª–@—¿‚T‚m‚q‚v   + ‰·ãª–@—¿‚TSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚T   = “d—Ã—¿‚T‚m‚q‚v     + “d—Ã—¿‚TSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚U   = Œã—Ã—¿‚U‚m‚q‚v     + Œã—Ã—¿‚USk‚m‚q‚v + ‘½•”ˆÊŒã—Ã—¿‚U‚m‚q‚v +
                                        ’·ŠúŒã—Ã—¿‚U‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚U‚m‚q‚v .
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚U = —âãª–@—¿‚U‚m‚q‚v   + —âãª–@—¿‚USk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚U = ‰·ãª–@—¿‚U‚m‚q‚v   + ‰·ãª–@—¿‚USk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚U   = “d—Ã—¿‚U‚m‚q‚v     + “d—Ã—¿‚USk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚V   = Œã—Ã—¿‚V‚m‚q‚v     + Œã—Ã—¿‚VSk‚m‚q‚v + ‘½•”ˆÊŒã—Ã—¿‚V‚m‚q‚v +
                                        ’·ŠúŒã—Ã—¿‚V‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚V‚m‚q‚v .
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚V = —âãª–@—¿‚V‚m‚q‚v   + —âãª–@—¿‚VSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚V = ‰·ãª–@—¿‚V‚m‚q‚v   + ‰·ãª–@—¿‚VSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚V   = “d—Ã—¿‚V‚m‚q‚v     + “d—Ã—¿‚VSk‚m‚q‚v.
060630*================================================================*
       •‰ƒf[ƒ^Žæ“¾ SECTION.
      *
           MOVE ˜AŒv|Ž{p˜a—ï TO •‰|Ž{p˜a—ï 
           MOVE ˜AŒv|Ž{p”N   TO •‰|Ž{p”N   
           MOVE ˜AŒv|Ž{pŒŽ   TO •‰|Ž{pŒŽ   
           MOVE ˜AŒv|Š³ŽÒ”Ô† TO •‰|Š³ŽÒ”Ô† 
           MOVE ˜AŒv|Ž}”Ô     TO •‰|Ž}”Ô     
           READ •‰ƒf[ƒ^‚e
           INVALID KEY
               DISPLAY NC"ŠY“–ŒŽ‚Ìî•ñ‚ª‚ ‚è‚Ü‚¹‚ñ" UPON CONS
               DISPLAY NC"Ž{p˜a—ï" ˜AŒv|Ž{p˜a—ï  UPON CONS
               DISPLAY NC"Ž{p”N"   ˜AŒv|Ž{p”N    UPON CONS
               DISPLAY NC"Ž{pŒŽ"   ˜AŒv|Ž{pŒŽ    UPON CONS
               DISPLAY NC"Š³ŽÒ”Ô†" ˜AŒv|Š³ŽÒ”Ô†  UPON CONS
               DISPLAY NC"Ž}”Ô"     ˜AŒv|Ž}”Ô      UPON CONS
               MOVE NC"•‰" TO ƒtƒ@ƒCƒ‹–¼‚v
               PERFORM ƒGƒ‰[•\Ž¦‚q
           NOT INVALID KEY
               MOVE •‰|•”ˆÊ” TO •”ˆÊ”‚v
           END-READ.
      *================================================================*
       “Á•ÊÞ—¿‰ñ””»’è SECTION.
      *
HILO  *     DISPLAY "“Á•ÊÞ—¿‰ñ””»’è " •”ˆÊƒJƒEƒ“ƒ^‚Q " ---------------"
      *ŠY“–•”ˆÊ‚Ì“Á•ÊÞ—¿‚Ì‰ñ”‚ð”‚¦‚éB(•”ˆÊƒR[ƒh‚ÆŠJŽn“ú‚ª“¯‚¶•”ˆÊ‚ðŠY“–•”ˆÊ‚Æ‚·‚é)
008580     MOVE ˜AŒv|Ž{p˜a—ï   TO •‰|Ž{p˜a—ï.
008590     MOVE ˜AŒv|Ž{p”N     TO •‰|Ž{p”N.
008600     MOVE ˜AŒv|Ž{pŒŽ     TO •‰|Ž{pŒŽ.
008610     MOVE ˜AŒv|Š³ŽÒ”Ô†   TO •‰|Š³ŽÒ”Ô†.
008620     MOVE ˜AŒv|Ž}”Ô       TO •‰|Ž}”Ô.
008630     READ •‰ƒf[ƒ^‚e
008640     INVALID KEY
               MOVE NC"•‰" TO ƒtƒ@ƒCƒ‹–¼‚v
               PERFORM ƒGƒ‰[•\Ž¦‚q
008660     NOT INVALID KEY
               MOVE •‰|•”ˆÊƒR[ƒh    (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO •”ˆÊƒR[ƒh‚v
               MOVE •‰|ŠJŽn˜a—ï”NŒŽ“ú(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ŠJŽn˜a—ï”NŒŽ“ú‚Q‚v
               MOVE •”ˆÊƒJƒEƒ“ƒ^‚Q                     TO •”ˆÊƒJƒEƒ“ƒ^‚R
               MOVE SPACE TO I—¹ƒtƒ‰ƒO‚Q
               MOVE ZERO  TO “Á•ÊÞ—¿‚b‚m‚s
               MOVE ˜AŒv|Š³ŽÒƒR[ƒh     TO Ž{‹L|Š³ŽÒƒR[ƒh
               MOVE ˜AŒv|Ž{p˜a—ï”NŒŽ“ú TO Ž{‹L|Ž{p˜a—ï”NŒŽ“ú Ž{p˜a—ï”NŒŽ“ú‚Q‚v
008770         START Ž{p‹L˜^‚e   KEY IS < Ž{‹L|Š³ŽÒƒR[ƒh
008780                                     Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
                                           REVERSED
008790         END-START
008800         IF ó‘ÔƒL[ = "00"
008810             MOVE SPACE TO I—¹ƒtƒ‰ƒO‚Q
008830             PERFORM Ž{p‹L˜^‚e“Çž‚Q
                   PERFORM UNTIL (˜AŒv|Š³ŽÒƒR[ƒh NOT = Ž{‹L|Š³ŽÒƒR[ƒh  ) OR 
                                 (Ž{‹L|Ž{p˜a—ï”NŒŽ“ú < ŠJŽn˜a—ï”NŒŽ“ú‚Q‚v) OR
                                 (I—¹ƒtƒ‰ƒO‚Q     NOT = SPACE)
                       IF Ž{‹L|Ž{p˜a—ï”NŒŽ NOT = Ž{p˜a—ï”NŒŽ‚Q‚v
                           PERFORM Þ—¿ŒŽŒ×ˆ—
                           MOVE Ž{‹L|Ž{p˜a—ï”NŒŽ“ú TO Ž{p˜a—ï”NŒŽ“ú‚Q‚v
                       END-IF
                       IF •”ˆÊƒJƒEƒ“ƒ^‚R NOT = ZERO
                           IF Ž{‹L|“Á•ÊÞ—¿‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚R) NOT = ZERO
                               COMPUTE “Á•ÊÞ—¿‚b‚m‚s = “Á•ÊÞ—¿‚b‚m‚s + 1
                           END-IF
                       END-IF
008830                 PERFORM Ž{p‹L˜^‚e“Çž‚Q
                   END-PERFORM
               END-IF
           END-READ.
      *
           MOVE ˜AŒv|Š³ŽÒƒR[ƒh     TO Ž{‹L|Š³ŽÒƒR[ƒh.
           MOVE ˜AŒv|Ž{p˜a—ï”NŒŽ“ú TO Ž{‹L|Ž{p˜a—ï”NŒŽ“ú.
008770     START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
008780                                  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
008790     END-START.
008800     IF ó‘ÔƒL[ = "00"
008810         MOVE SPACE TO I—¹ƒtƒ‰ƒO
008830         PERFORM Ž{p‹L˜^‚e“Çž‚Q
           END-IF.
HILO  *     DISPLAY "“Á•ÊÞ—¿‰ñ” " “Á•ÊÞ—¿‚b‚m‚s.
009040*================================================================*
       Ž{p‹L˜^‚e“Çž‚Q SECTION.
      *
           READ Ž{p‹L˜^‚e NEXT
           AT END
               MOVE "YES" TO I—¹ƒtƒ‰ƒO‚Q
           END-READ.
      *
      *================================================================*
       Þ—¿ŒŽŒ×ˆ— SECTION.
      *
HILO  *     DISPLAY "‹à‘®ŒŽŒ×ˆ—"
008580     MOVE Ž{‹L|Ž{p˜a—ï   TO •‰|Ž{p˜a—ï.
008590     MOVE Ž{‹L|Ž{p”N     TO •‰|Ž{p”N.
008600     MOVE Ž{‹L|Ž{pŒŽ     TO •‰|Ž{pŒŽ.
008610     MOVE Ž{‹L|Š³ŽÒ”Ô†   TO •‰|Š³ŽÒ”Ô†.
008620     MOVE Ž{‹L|Ž}”Ô       TO •‰|Ž}”Ô.
008630     READ •‰ƒf[ƒ^‚e
008640     INVALID KEY
               MOVE NC"•‰" TO ƒtƒ@ƒCƒ‹–¼‚v
               PERFORM ƒGƒ‰[•\Ž¦‚q
008660     NOT INVALID KEY
           MOVE ZERO TO •”ˆÊƒJƒEƒ“ƒ^‚R
               PERFORM VARYING •”ˆÊƒJƒEƒ“ƒ^‚S FROM 1 BY 1 UNTIL  •”ˆÊƒJƒEƒ“ƒ^‚S > •‰|•”ˆÊ”
                   IF (•‰|•”ˆÊƒR[ƒh    (•”ˆÊƒJƒEƒ“ƒ^‚S) = •”ˆÊƒR[ƒh‚v      ) AND
                      (•‰|ŠJŽn˜a—ï”NŒŽ“ú(•”ˆÊƒJƒEƒ“ƒ^‚S) = ŠJŽn˜a—ï”NŒŽ“ú‚Q‚v)
                       MOVE •”ˆÊƒJƒEƒ“ƒ^‚S TO •”ˆÊƒJƒEƒ“ƒ^‚R
                   END-IF
               END-PERFORM
           END-READ.
      *================================================================*
       “Á•ÊÞ—¿‹æ•ªƒNƒŠƒA SECTION.
      *
           MOVE Ž{‹L|Š³ŽÒƒR[ƒh     TO XVŽ{‹L|Š³ŽÒƒR[ƒh.
           MOVE Ž{‹L|Ž{p˜a—ï”NŒŽ“ú TO XVŽ{‹L|Ž{p˜a—ï”NŒŽ“ú.
           READ XVŽ{p‹L˜^‚e
008840     INVALID KEY
008850         MOVE NC"XŽ{" TO ƒtƒ@ƒCƒ‹–¼‚v
008860         PERFORM ƒGƒ‰[•\Ž¦‚q
           END-READ.
           MOVE ZERO TO XVŽ{‹L|“Á•ÊÞ—¿‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q).
008830     REWRITE XVŽ{‹L|ƒŒƒR[ƒh
008840     INVALID KEY
008850         MOVE NC"XŽ{" TO ƒtƒ@ƒCƒ‹–¼‚v
008860         PERFORM ƒGƒ‰[•\Ž¦
008870     END-REWRITE.
      *================================================================*
060640******************************************************************
060650 END PROGRAM JIB9709R.
060660******************************************************************
