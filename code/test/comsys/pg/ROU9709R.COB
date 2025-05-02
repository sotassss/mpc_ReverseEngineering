000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             ROU9709R.
000060 AUTHOR.                 ŽR“c _”V
000070*
000080*----------------------------------------------------------------*
000090*         ”ï—pŠzŒvŽZƒvƒƒOƒ‰ƒ€@ƒŒƒZƒvƒg”­sm˜JÐ•ÛŒ¯—pni_j
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
000420     SELECT  —¿‹àƒ}ƒXƒ^      ASSIGN      TO        RYOUKINL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS  —¿|‹æ•ªƒR[ƒh
000460                                                          —¿|•”ˆÊƒR[ƒh
000470                                                          —¿|ŠJŽn˜a—ï”NŒŽ
000480                             FILE STATUS              IS  ó‘ÔƒL[
000490                             LOCK        MODE         IS  AUTOMATIC.
000500     SELECT  ŒvŽZƒ}ƒXƒ^      ASSIGN      TO        KEISANL
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
001990**                           m‚q‚k  ‚P‚Q‚Wn
002000* FD  ŒvŽZ‚o‚fƒ}ƒXƒ^       BLOCK   CONTAINS   1   RECORDS.
002010*     COPY KEISAPG    OF  XFDLIB  JOINING   Œv‚o AS  PREFIX.
002020*                           m‚q‚k  ‚R‚Q‚On
002030 FD  ŽófŽÒî•ñ‚e        BLOCK   CONTAINS   1   RECORDS.
002040     COPY JUSINJ     OF  XFDLIB  JOINING   Žó   AS  PREFIX.
002050*                           m‚q‚k  ‚P‚Q‚Wn
002060 FD  •‰ƒf[ƒ^‚e        BLOCK   CONTAINS   1   RECORDS.
002070     COPY HUSYOU          OF  XFDLIB  JOINING   •‰   AS  PREFIX.
002080*                           m‚q‚k  ‚Q‚T‚Un
002090 FD  §Œäî•ñƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
002100     COPY SEIGYO     OF  XFDLIB  JOINING   §   AS  PREFIX.
002110*                           m‚q‚k  ‚P‚Q‚Wn
002120 FD  Œ³†ƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
002130     COPY GENGOU     OF  XFDLIB  JOINING   Œ³   AS  PREFIX.
002140*                           m‚q‚k  ‚P‚Q‚Wn
002150 FD  –¼Ìƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
002160     COPY MEISYO     OF  XFDLIB  JOINING   –¼   AS  PREFIX.
002170*******
002180 FD  ‰ïŒvƒf[ƒ^‚e        BLOCK   CONTAINS   1   RECORDS.
002190     COPY KAIKEI     OF  XFDLIB  JOINING   ‰ï     AS  PREFIX.
002200     COPY KAIKEI02   OF  XFDLIB  JOINING   ‰ï‚O‚Q AS  PREFIX.
002200*
002210 FD  ƒŒƒZƒvƒg‚e          BLOCK   CONTAINS   1   RECORDS.
002220     COPY RECEPT          OF  XFDLIB  JOINING   ƒŒƒZ   AS  PREFIX.
002230*
002240 FD  ƒŒƒZƒvƒg˜JŽ©‚e      BLOCK   CONTAINS   1   RECORDS.
002250     COPY RECERJ          OF  XFDLIB  JOINING   ƒŒƒZ˜JŽ© AS  PREFIX.
002260*
002270 FD  ‰ïŒv—ÌŽû‚e     BLOCK   CONTAINS   1   RECORDS.
002280     COPY RYOSYU    OF  XFDLIB  JOINING   —Ì  AS  PREFIX.
002290*
001830 FD  XVŽ{p‹L˜^‚e      BLOCK   CONTAINS   1   RECORDS.
001840     COPY SEKIROK         OF  XFDLIB  JOINING   XVŽ{‹L AS  PREFIX.
002290*
002300*----------------------------------------------------------------*
002310******************************************************************
002320*                WORKING-STORAGE SECTION                         *
002330******************************************************************
002340 WORKING-STORAGE         SECTION.
002350 01 ƒL[“ü—Í                           PIC X    VALUE SPACE.
002360 01 ó‘ÔƒL[                           PIC X(2) VALUE SPACE.
002370 01 I—¹ƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002380 01 I—¹ƒtƒ‰ƒO‚Q                       PIC X(3) VALUE SPACE.
002390 01 ‘žƒtƒ‰ƒO                         PIC X(4) VALUE SPACE.
002400 01 ‘žƒtƒ‰ƒO‚Q                       PIC X(4) VALUE SPACE.
002410 01 ‘ž‹–‰Âƒtƒ‰ƒO                     PIC X(3) VALUE SPACE.
002420 01 ”NŒŽƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002430 01 ’Ç‰Áƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002440 01 sŒ…ƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002450 01 €–Úƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002460 01 ’·Šúƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002470 01 ‰—Ã‰ÁŽZƒtƒ‰ƒO                     PIC X(3) VALUE SPACE.
002480 01 •ÛŒ¯ŽÒ”Ô†‚v                       PIC X(6) VALUE SPACE.
002490 01 ƒtƒ@ƒCƒ‹–¼‚v                       PIC N(2) VALUE SPACE.
002500 01 ƒGƒ‰[“à—e‚v                       PIC N(18) VALUE SPACE.
002510 01 ‰ïˆõ–¼‚v                           PIC N(16) VALUE SPACE.
002520 01 ‰æ–Ê–¼Ì‚v                         PIC N(2) VALUE SPACE.
002530 01 •‰‚b‚m‚s                         PIC 9(2)    VALUE ZERO.
002540 01 •”ˆÊƒJƒEƒ“ƒ^                       PIC 9(2)    VALUE ZERO.
002550 01 •”ˆÊƒJƒEƒ“ƒ^‚Q                     PIC 9(2)    VALUE ZERO.
002560 01 •”ˆÊ‚b‚m‚s                         PIC 9(2) VALUE ZERO.
002570 01 •”ˆÊ‚b‚m‚s‚Q                       PIC 9(2) VALUE ZERO.
002580 01 ƒJƒEƒ“ƒ^                           PIC 9(2) VALUE ZERO.
002590 01 ƒJƒEƒ“ƒ^‚Q                         PIC 9(2) VALUE ZERO.
002600 01 ‘½•”ˆÊ‚b‚m‚s                       PIC 9(2) VALUE ZERO.
002610 01 Å‘å“o˜^•”ˆÊ”‚v                   PIC 9(2) VALUE ZERO.
002620 01 ¿‹ˆÚ“®ƒL[                       PIC X(4) VALUE SPACE.
002630 01 ‰æ–ÊˆÚ“®ƒL[                       PIC X(4) VALUE SPACE.
002640 01 ˆ—ˆÚ“®ƒL[                       PIC X(4) VALUE SPACE.
002650 01 Ä“ü—ÍƒL[‚v                       PIC X(4) VALUE SPACE.
002660 01 “ü—Íó‘Ô‚v                         PIC X(4) VALUE SPACE.
002670 01 •”ˆÊ–¼Ì‚v                         PIC N(12) VALUE SPACE.
002680 01 s‚v                               PIC 9(4) VALUE ZERO.
002690 01 Œ…‚v                               PIC 9(4) VALUE ZERO.
002700 01 ‘O˜a—ï‚v                           PIC 9 VALUE ZERO.
002710 01 ƒJƒŒƒ“ƒg˜a—ï‚v                     PIC 9 VALUE ZERO.
002720 01 “ü‹àŠz‚v                           PIC 9(6) VALUE ZERO.
002730 01 ‰ŒŸ¿‹‹æ•ª‚v                     PIC 9 VALUE ZERO.
002740 01 “]‹A‹æ•ª‚v                         PIC 9 VALUE ZERO.
002750 01 •ÛŒ¯Ží•Ê‚v                         PIC 9(2) VALUE ZERO.
002760 01 Œö”ïŽí•Ê‚v                         PIC 9(2) VALUE ZERO.
002770 01 •¬Ží•Ê‚v                         PIC 9(2) VALUE ZERO.
002780 01 •¬Ží•Ê‚P‚v                       PIC 9(2) VALUE ZERO.
002790 01 •¬Ží•Ê‚Q‚v                       PIC 9(2) VALUE ZERO.
002800 01 ˜Vl•‰’S‰ñ”‚v                     PIC 9(2) VALUE ZERO.
002810 01 •¬•‰’SŒvŽZ‹æ•ª‚v                 PIC 9 VALUE ZERO.
002820*
002830 01 ‰ŒŸŽžŠÔŠO‚v                       PIC 9 VALUE ZERO.
002840 01 ‰ŒŸ‹x“ú‚v                         PIC 9 VALUE ZERO.
002850 01 ‰ŒŸ[–é‚v                         PIC 9 VALUE ZERO.
002860*
002870 01 ‰—Ã–éŠÔ‚v                         PIC 9 VALUE ZERO.
002880 01 ‰—Ã“ï˜H‚v                         PIC 9 VALUE ZERO.
002890 01 ‰—Ã–\•—‰J‚v                       PIC 9 VALUE ZERO.
002900 01 ‰—ÃŽžŠÔŠO‚v                       PIC 9 VALUE ZERO.
002910*
002920 01 ‹à‘®•›Žq‘å‚v                       PIC 9 VALUE ZERO.
002930 01 ‹à‘®•›Žq’†‚v                       PIC 9 VALUE ZERO.
002940 01 ‹à‘®•›Žq¬‚v                       PIC 9 VALUE ZERO.
002950*
002960 01 ‰—Ã”íœ”‚v                       PIC 9(3) VALUE ZERO.
002970 01 ‰—Ãœ”‚v                         PIC 9(3) VALUE ZERO.
002980 01 ‰—Ã¤‚v                           PIC 9(3) VALUE ZERO.
002990 01 ‰—Ãè—]‚v                         PIC 9(3) VALUE ZERO.
003000 01 ‰—Ã‹——£‚v                         PIC 9(3)V9 VALUE ZERO.
003010 01 ‰—Ã‰ÁŽZ—¦‚v                       PIC 9(3) VALUE ZERO.
003020*
003030 01 ‰ŒŸ—¿‚v                           PIC 9(4) VALUE ZERO.
003040 01 ‰ŒŸ‰ÁŽZ—¿‚v                       PIC 9(5) VALUE ZERO.
003050 01 ÄŒŸ—¿‚v                           PIC 9(4) VALUE ZERO.
003060 01 ‰—Ã—¿‚v                           PIC 9(5) VALUE ZERO.
003070*/‰—Ã’P‰¿‚Ìˆ×‚Ìƒ[ƒN
003080 01 ‰—Ã—¿‚v‚Q                         PIC 9(5) VALUE ZERO.
003090 01 ‰—Ã‰ÁŽZ—¿‚v                       PIC 9(6) VALUE ZERO.
003100 01 Œã—Ã—¿‚v                           PIC 9(5) VALUE ZERO.
003110 01 —âãª–@—¿‚v                         PIC 9(4) VALUE ZERO.
003120 01 ‰·ãª–@—¿‚v                         PIC 9(4) VALUE ZERO.
003130 01 “d—Ã—¿‚v                           PIC 9(4) VALUE ZERO.
003140*
003150 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚P                 PIC 9(6) VALUE ZERO.
003160 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚Q                 PIC 9(6) VALUE ZERO.
003170 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚R                 PIC 9(6) VALUE ZERO.
003180 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚R‚P               PIC 9(6) VALUE ZERO.
003190 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚S                 PIC 9(6) VALUE ZERO.
003200 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚S‚P               PIC 9(6) VALUE ZERO.
003210 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚T                 PIC 9(6) VALUE ZERO.
003220 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚T‚P               PIC 9(6) VALUE ZERO.
003230 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚T‚Q               PIC 9(6) VALUE ZERO.
003240 01 •”ˆÊ“úŒv‘½•”ˆÊ‚v‚T‚R               PIC 9(6) VALUE ZERO.
003250*
003260 01 •”ˆÊ“úŒv’·Šú‚v‚P                   PIC 9(6) VALUE ZERO.
003270 01 •”ˆÊ“úŒv’·Šú‚v‚Q                   PIC 9(6) VALUE ZERO.
003280 01 •”ˆÊ“úŒv’·Šú‚v‚R                   PIC 9(6) VALUE ZERO.
003290 01 •”ˆÊ“úŒv’·Šú‚v‚R‚P                 PIC 9(6) VALUE ZERO.
003300 01 •”ˆÊ“úŒv’·Šú‚v‚S                   PIC 9(6) VALUE ZERO.
003310 01 •”ˆÊ“úŒv’·Šú‚v‚S‚P                 PIC 9(6) VALUE ZERO.
003320 01 •”ˆÊ“úŒv’·Šú‚v‚S‚Q                 PIC 9(6) VALUE ZERO.
003330 01 •”ˆÊ“úŒv’·Šú‚v‚T                   PIC 9(6) VALUE ZERO.
003340 01 •”ˆÊ“úŒv’·Šú‚v‚T‚P                 PIC 9(6) VALUE ZERO.
003350 01 •”ˆÊ“úŒv’·Šú‚v‚T‚Q                 PIC 9(6) VALUE ZERO.
003360 01 •”ˆÊ“úŒv’·Šú‚v‚T‚R                 PIC 9(6) VALUE ZERO.
003370*
003380 01 •”ˆÊ“úŒv‚v‚P                       PIC 9(6) VALUE ZERO.
003390 01 •”ˆÊ“úŒv‚v‚Q                       PIC 9(6) VALUE ZERO.
003400 01 •”ˆÊ“úŒv‚v‚R                       PIC 9(6) VALUE ZERO.
003410 01 •”ˆÊ“úŒv‚v‚R‚P                     PIC 9(6) VALUE ZERO.
003420 01 •”ˆÊ“úŒv‚v‚S                       PIC 9(6) VALUE ZERO.
003430 01 •”ˆÊ“úŒv‚v‚S‚P                     PIC 9(6) VALUE ZERO.
003440 01 •”ˆÊ“úŒv‚v‚S‚Q                     PIC 9(6) VALUE ZERO.
003450 01 •”ˆÊ“úŒv‚v‚T                       PIC 9(6) VALUE ZERO.
003460 01 •”ˆÊ“úŒv‚v‚T‚P                     PIC 9(6) VALUE ZERO.
003470 01 •”ˆÊ“úŒv‚v‚T‚Q                     PIC 9(6) VALUE ZERO.
003480 01 •”ˆÊ“úŒv‚v‚T‚R                     PIC 9(6) VALUE ZERO.
003490*
003500 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚P                 PIC 9(6) VALUE ZERO.
003510 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚Q                 PIC 9(6) VALUE ZERO.
003520 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚R                 PIC 9(6) VALUE ZERO.
003530 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚R‚P               PIC 9(6) VALUE ZERO.
003540 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚S                 PIC 9(6) VALUE ZERO.
003550 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚S‚P               PIC 9(6) VALUE ZERO.
003560 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚T                 PIC 9(6) VALUE ZERO.
003570 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚T‚P               PIC 9(6) VALUE ZERO.
003580 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚T‚Q               PIC 9(6) VALUE ZERO.
003590 01 •”ˆÊ‡Œv‘½•”ˆÊ‚v‚T‚R               PIC 9(6) VALUE ZERO.
003600*
003610 01 •”ˆÊ‡Œv’·Šú‚v‚P                   PIC 9(6) VALUE ZERO.
003620 01 •”ˆÊ‡Œv’·Šú‚v‚Q                   PIC 9(6) VALUE ZERO.
003630 01 •”ˆÊ‡Œv’·Šú‚v‚R                   PIC 9(6) VALUE ZERO.
003640 01 •”ˆÊ‡Œv’·Šú‚v‚S                   PIC 9(6) VALUE ZERO.
003650 01 •”ˆÊ‡Œv’·Šú‚v‚T                   PIC 9(6) VALUE ZERO.
003660 01 •”ˆÊ‡Œv’·Šú‚v‚U                   PIC 9(6) VALUE ZERO.
003670 01 •”ˆÊ‡Œv’·Šú‚v‚V                   PIC 9(6) VALUE ZERO.
003680 01 •”ˆÊ‡Œv’·Šú‚v‚W                   PIC 9(6) VALUE ZERO.
003690 01 •”ˆÊ‡Œv’·Šú‚v‚X                   PIC 9(6) VALUE ZERO.
003700 01 •”ˆÊ‡Œv’·Šú‚v‚R‚P                 PIC 9(6) VALUE ZERO.
003710 01 •”ˆÊ‡Œv’·Šú‚v‚S‚P                 PIC 9(6) VALUE ZERO.
003720 01 •”ˆÊ‡Œv’·Šú‚v‚S‚Q                 PIC 9(6) VALUE ZERO.
003730 01 •”ˆÊ‡Œv’·Šú‚v‚T‚P                 PIC 9(6) VALUE ZERO.
003740 01 •”ˆÊ‡Œv’·Šú‚v‚T‚Q                 PIC 9(6) VALUE ZERO.
003750 01 •”ˆÊ‡Œv’·Šú‚v‚T‚R                 PIC 9(6) VALUE ZERO.
003760*
003770 01 •”ˆÊ‡Œv‚v‚P                       PIC 9(6) VALUE ZERO.
003780 01 •”ˆÊ‡Œv‚v‚Q                       PIC 9(6) VALUE ZERO.
003790 01 •”ˆÊ‡Œv‚v‚R                       PIC 9(6) VALUE ZERO.
003800 01 •”ˆÊ‡Œv‚v‚S                       PIC 9(6) VALUE ZERO.
003810 01 •”ˆÊ‡Œv‚v‚T                       PIC 9(6) VALUE ZERO.
003820 01 •”ˆÊ‡Œv‚v‚U                       PIC 9(6) VALUE ZERO.
003830 01 •”ˆÊ‡Œv‚v‚V                       PIC 9(6) VALUE ZERO.
003840 01 •”ˆÊ‡Œv‚v‚W                       PIC 9(6) VALUE ZERO.
003850 01 •”ˆÊ‡Œv‚v‚X                       PIC 9(6) VALUE ZERO.
003860 01 •”ˆÊ‡Œv‚v‚R‚P                     PIC 9(6) VALUE ZERO.
003870 01 •”ˆÊ‡Œv‚v‚S‚P                     PIC 9(6) VALUE ZERO.
003880 01 •”ˆÊ‡Œv‚v‚S‚Q                     PIC 9(6) VALUE ZERO.
003890 01 •”ˆÊ‡Œv‚v‚T‚P                     PIC 9(6) VALUE ZERO.
003900 01 •”ˆÊ‡Œv‚v‚T‚Q                     PIC 9(6) VALUE ZERO.
003910 01 •”ˆÊ‡Œv‚v‚T‚R                     PIC 9(6) VALUE ZERO.
003920*
003930 01 ‰‰ñˆ’u‚r‚v.
003940   03 ‰‰ñˆ’u‚n‚v OCCURS 9.
003950     05 ‰‰ñˆ’u—¿‚n‚v                 PIC 9(5) VALUE ZERO.
003960*
003970 01 Œã—Ã’P‰¿‚r‚v.
003980   03 Œã—Ã’P‰¿‚n‚v‚P OCCURS 9.
003990     05 Œã—Ã’P‰¿‚n‚v                   PIC 9(4) VALUE ZERO.
004000     05 SkŒã—Ã’P‰¿‚n‚v               PIC 9(4) VALUE ZERO.
004010*
004020*/˜JÐ‚Í‘½•”ˆÊ’üŒ¸‚ª–³‚¢
004030 01 Œã—Ã—¿‚P‚O‚O‚r‚v.
004040     05 Œã—Ã—¿‚P‚O‚O‚v                 PIC 9(5) OCCURS 9 VALUE ZERO.
004050 01 “d—Ã—¿‚P‚O‚O‚r‚v.
004060     05 “d—Ã—¿‚P‚O‚O‚v                 PIC 9(5) OCCURS 9 VALUE ZERO.
004070 01 —âãª–@—¿‚P‚O‚O‚r‚v.
004080     05 —âãª–@—¿‚P‚O‚O‚v               PIC 9(5) OCCURS 9 VALUE ZERO.
004090 01 ‰·ãª–@—¿‚P‚O‚O‚r‚v.
004100     05 ‰·ãª–@—¿‚P‚O‚O‚v               PIC 9(5) OCCURS 9 VALUE ZERO.
004110*
004120 01 “d—Ã’P‰¿‚r‚v.
004130     05 “d—Ã’P‰¿‚v                     PIC 9(5) OCCURS 9 VALUE ZERO.
004140 01 —âãª–@’P‰¿‚r‚v.
004150     05 —âãª–@’P‰¿‚v                   PIC 9(5) OCCURS 9 VALUE ZERO.
004160 01 ‰·ãª–@’P‰¿‚r‚v.
004170     05 ‰·ãª–@’P‰¿‚v                   PIC 9(5) OCCURS 9 VALUE ZERO.
004180*
004190 01 ‰ŒŸ—¿‚f‚v                         PIC 9(5) VALUE ZERO.
004200 01 ‰ŒŸ‰ÁŽZ—¿‚f‚v                     PIC 9(6) VALUE ZERO.
004210 01 ‰‰ñˆ’u—¿‚f‚v                     PIC 9(5) VALUE ZERO.
004220 01 ÄŒŸ—¿‚f‚v                         PIC 9(5) VALUE ZERO.
004230 01 ‰—Ã—¿‚f‚v                         PIC 9(6) VALUE ZERO.
004240 01 ‰—Ã‰ÁŽZ—¿‚f‚v                     PIC 9(6) VALUE ZERO.
004250 01 ‰—Ã‰ñ”‚f‚v                       PIC 9(2) VALUE ZERO.
004260 01 Œã—Ã—¿‚f‚v                         PIC 9(6) VALUE ZERO.
004270 01 —âãª–@—¿‚f‚v                       PIC 9(4) VALUE ZERO.
004280 01 ‰·ãª–@—¿‚f‚v                       PIC 9(4) VALUE ZERO.
004290 01 “d—Ã—¿‚f‚v                         PIC 9(4) VALUE ZERO.
004300 01 ‹à‘®•›Žq—¿‚f‚v                     PIC 9(5) VALUE ZERO.
004310 01 Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v                 PIC 9(5) VALUE ZERO.
004320*/˜JÐ•ÛŒ¯—p
004330 01 Žw“±ŠÇ——¿‚f‚v                     PIC 9(4) VALUE ZERO.
004340 01 “Á•ÊÞ—¿”ï‚f‚v                     PIC 9(5) VALUE ZERO.
004350 01 ŒðŠ·•ï‘Ñ—¿‚f‚v                     PIC 9(5) VALUE ZERO.
004360 01 ‰^“®—Ã–@—¿‚f‚v                     PIC 9(5) VALUE ZERO.
004370 01 h”‘—¿‚f‚v                         PIC 9(5) VALUE ZERO.
004380 01 HŽ–—¿‚f‚v                         PIC 9(5) VALUE ZERO.
004390 01 ‹x‹ÆØ–¾‘—¿‚f‚v                   PIC 9(5) VALUE ZERO.
004400*
004410 01 •‰’SŠz‚f‚v                         PIC 9(5) VALUE ZERO.
004420 01 ”ï—pŠz˜Vl‚f‚v                     PIC 9(6) VALUE ZERO.
004430 01 •‰’SŠz˜Vl‚f‚v                     PIC 9(5) VALUE ZERO.
004440 01 ¿‹Šz˜Vl‚f‚v                     PIC 9(6) VALUE ZERO.
004450 01 •‰’SŠz•¬‚f‚v                     PIC 9(6) VALUE ZERO.
004460 01 ¿‹Šz•¬‚f‚v                     PIC 9(6) VALUE ZERO.
004470*
004480 01 ”ï—pŠz‚o‚f‚v                       PIC X(8) VALUE SPACE.
004490 01 •‰’SŠz‚o‚f‚v                       PIC X(8) VALUE SPACE.
004500 01 •‰’SŠz‚o‚f˜Vl‚v                   PIC X(8) VALUE SPACE.
004510 01 •‰’SŠz‚o‚f•¬‚v                   PIC X(8) VALUE SPACE.
004520*
004530 01 Œã—Ã—¿‚s‚v                         PIC 9(5) VALUE ZERO.
004540 01 —âãª–@—¿‚s‚v                       PIC 9(4) VALUE ZERO.
004550 01 ‰·ãª–@—¿‚s‚v                       PIC 9(4) VALUE ZERO.
004560 01 “d—Ã—¿‚s‚v                         PIC 9(4) VALUE ZERO.
004570 01 ‹à‘®•›Žq—¿‚v                       PIC 9(4) VALUE ZERO.
004580 01 ‰‰ñˆ’u—¿‚v                       PIC 9(5) VALUE ZERO.
004590 01 î•ñ’ñ‹Ÿ‰ñ”‚v                     PIC 9(2) VALUE ZERO.
004600 01 Ž{pî•ñ’ñ‹Ÿ—¿‚v                   PIC 9(4) VALUE ZERO.
004610*/˜JÐ•ÛŒ¯—pƒ[ƒN
004620 01 Žw“±ŠÇ—‰ñ”‚v                     PIC 9    VALUE ZERO.
004630 01 Žw“±ŠÇ——¿‚v                       PIC 9(4) VALUE ZERO.
004640 01 “Á•ÊÞ—¿”ï‚v                       PIC 9(5) VALUE ZERO.
004650 01 ŒðŠ·•ï‘Ñ—¿‚v                       PIC 9(5) VALUE ZERO.
004660 01 ‰^“®—Ã–@‰ñ”‚v                     PIC 9    VALUE ZERO.
004670 01 ‰^“®—Ã–@—¿‚v                       PIC 9(4) VALUE ZERO.
004680 01 h”‘‰ñ”‚v                         PIC 9(2) VALUE ZERO.
004690 01 h”‘—¿‚v                           PIC 9(4) VALUE ZERO.
004700 01 HŽ–‰ñ”‚v                         PIC 9(2) VALUE ZERO.
004710 01 HŽ–—¿‚v                           PIC 9(4) VALUE ZERO.
004720 01 ‹x‹ÆØ–¾‰ñ”‚v                     PIC 9(2) VALUE ZERO.
004730 01 ‹x‹ÆØ–¾‘—¿‚v                     PIC 9(4) VALUE ZERO.
004740*
004750 01 Œã—Ã—¿‚v‚P                         PIC 9(5) VALUE ZERO.
004760 01 —âãª–@—¿‚v‚P                       PIC 9(4) VALUE ZERO.
004770 01 ‰·ãª–@—¿‚v‚P                       PIC 9(4) VALUE ZERO.
004780 01 “d—Ã—¿‚v‚P                         PIC 9(4) VALUE ZERO.
004790*
004800 01 ¼—ï”N‚v                           PIC 9(4) VALUE ZERO.
004810 01 ¼—ï”N·‚v                         PIC 9(4) VALUE ZERO.
004820 01 ŠJŽn”NŒŽâ‘Î’l‚v                   PIC 9(6) VALUE ZERO.
004830 01 Ž{p”NŒŽâ‘Î’l‚v                   PIC 9(6) VALUE ZERO.
004840 01 Ž{p”NŒŽ·‚v                       PIC 9(4) VALUE ZERO.
004850*
004860 01 •”ˆÊ”‚v                           PIC 9(2) VALUE ZERO.
004870 01 Œp‘±•”ˆÊ”‚v                       PIC 9(2) VALUE ZERO.
004880*
004890* ŒvŽZî•ñƒ[ƒN
004900* Šî–{
004910 01 ‰—ÃŠî–{‹——£‚j‚v                   PIC 9(2) VALUE ZERO.
004920 01 Œã—Ã‰Â”\ŽZ’è”‚j‚v                 PIC 9(2) VALUE ZERO.
004930*
004940 01 ’üŒ¸—¦‚r‚j‚v.
004950   03 ’üŒ¸—¦‚j‚v  OCCURS 9.
004960     05 ‘½•”ˆÊ’üŒ¸—¦‚j‚v               PIC 9(3) VALUE ZERO.
004970*
004980 01 •”ˆÊ’üŒ¸‚r‚v.
004990   03 •”ˆÊ’üŒ¸‚v  OCCURS 9.
005000     05 •”ˆÊ’üŒ¸î•ñ‚v                 PIC 9(2) VALUE ZERO.
005010*
005020 01 ’·Šú‹æ•ª‚r‚v.
005030    03 ’·Šú‹æ•ª‚v                      PIC 9(2) OCCURS 9 VALUE ZERO.
005040*
005050 01 ‰—Ã‰ÁŽZ—¦’†ŠÔ‚v                   PIC 9V9(3) VALUE ZERO.
005060*
005070 01 ’·Šú’üŒ¸—¦’†ŠÔ‚j‚v                 PIC 9V9(3) VALUE ZERO.
005080 01 ’·Šú’üŒ¸—¦‚j‚v                     PIC 9(3) VALUE ZERO.
005090 01 ’·Šú’üŒ¸ŒŽ‹«ŠE“ú‚j‚v               PIC 9(2) VALUE ZERO.
005100 01 ’·Šú’üŒ¸ŒŽ”‚j‚v                   PIC 9(2) VALUE ZERO.
005110 01 ˜Vl•‰’S‰ñ”‚j‚v                   PIC 9(2) VALUE ZERO.
005120 01 ’üŒ¸—¦’†ŠÔŒ‹‰Ê‚v                   PIC 9V9(3) VALUE ZERO.
005130* —¿‹àî•ñƒ[ƒN
005140* Šî–{
005150 01 ‰ŒŸ—¿‚`‚v                         PIC 9(4) VALUE ZERO.
005160 01 ŽžŠÔŠO‚`‚v                         PIC 9(4) VALUE ZERO.
005170 01 ‹x“ú‚`‚v                           PIC 9(4) VALUE ZERO.
005180 01 [–é‚`‚v                           PIC 9(4) VALUE ZERO.
005190 01 ‰—ÃŠî–{—¿‹à‚`‚v                   PIC 9(4) VALUE ZERO.
005200 01 ‰—Ã’Ç‰Á—¿‹à‚`‚v                   PIC 9(4) VALUE ZERO.
005210 01 ‰—Ã‰ÁŽZ—¦‚`‚v                     PIC 9(3) VALUE ZERO.
005220 01 ‰—Ã[–é‰ÁŽZ—¦‚`‚v                 PIC 9(3) VALUE ZERO.
005230 01 ‰—Ã“ï˜H‰ÁŽZ—¦‚`‚v                 PIC 9(3) VALUE ZERO.
005240 01 ‰—Ã–\•—‰J‰ÁŽZ—¦‚`‚v               PIC 9(3) VALUE ZERO.
005250 01 ÄŒŸ—¿‚`‚v                         PIC 9(4) VALUE ZERO.
005260 01 —âãª–@—¿‚`‚v                       PIC 9(4) VALUE ZERO.
005270 01 ‰·ãª–@—¿‚`‚v                       PIC 9(4) VALUE ZERO.
005280 01 “d—Ã—¿‚`‚v                         PIC 9(4) VALUE ZERO.
005290 01 ‘å‚`‚v                             PIC 9(4) VALUE ZERO.
005300 01 ’†‚`‚v                             PIC 9(4) VALUE ZERO.
005310 01 ¬‚`‚v                             PIC 9(4) VALUE ZERO.
005320 01 Ž{pî•ñ’ñ‹Ÿ—¿‚`‚v                 PIC 9(4) VALUE ZERO.
005330* •‰•Ê
005340 01 SkŒã—Ã—¿‚a‚v                     PIC 9(5) VALUE ZERO.
005350 01 Œã—Ã—¿‚a‚v                         PIC 9(5) VALUE ZERO.
005360 01 ®•œ—¿‚a‚v                         PIC 9(5) VALUE ZERO.
005370*
005380 01 •”ˆÊƒR[ƒh‚v.
005390    03 •‰Ží•Ê‚v                      PIC 9(2).
005400    03 •”ˆÊ‚v                          PIC 9(2).
005410    03 ¶‰E‹æ•ª‚v                      PIC 9.
005420    03 •‰ˆÊ’u”Ô†‚v                  PIC 9(2).
005430*
005440 01 •”ˆÊƒR[ƒh‚b‚v.
005450    03 •‰Ží•Ê‚b‚v                    PIC 9(2).
005460    03 •”ˆÊ‚b‚v                        PIC 9(2).
005470*
005480 01 Š³ŽÒƒR[ƒh‚v.
005490     03 Š³ŽÒ”Ô†‚v                     PIC 9(6).
005500     03 Ž}”Ô‚v                         PIC X.
005510*
005520 01 Š³ŽÒƒR[ƒh‚b‚v.
005530     03 Š³ŽÒ”Ô†‚b‚v                   PIC 9(6).
005540     03 Ž}”Ô‚b‚v                       PIC X.
005550*
005560 01 Ž{p˜a—ï”NŒŽ“ú‚b‚v.
005570   03 Ž{p˜a—ï”NŒŽ‚b‚v.
005580     05 Ž{p˜a—ï‚b‚v                   PIC 9.
005590     05 Ž{p”NŒŽ‚b‚v.
005600        07 Ž{p”N‚b‚v                  PIC 9(2).
005610        07 Ž{pŒŽ‚b‚v                  PIC 9(2).
005620   03 Ž{p“ú‚b‚v                       PIC 9(2).
005630*
005640 01 Ž{p˜a—ï”NŒŽ“ú‚v.
005650   03 Ž{p˜a—ï”NŒŽ‚v.
005660     05 Ž{p˜a—ï‚v                     PIC 9.
005670     05 Ž{p”NŒŽ‚v.
005680        07 Ž{p”N‚v                    PIC 9(2).
005690        07 Ž{pŒŽ‚v                    PIC 9(2).
005700   03 Ž{p“ú‚v                         PIC 9(2).
005710*
005720 01 ŠJŽn˜a—ï”NŒŽ“ú‚v.
005730   03 ŠJŽn˜a—ï”NŒŽ‚v.
005740     05 ŠJŽn˜a—ï‚v                     PIC 9.
005750     05 ŠJŽn”NŒŽ‚v.
005760        07 ŠJŽn”N‚v                    PIC 9(2).
005770        07 ŠJŽnŒŽ‚v                    PIC 9(2).
005780   03 ŠJŽn“ú‚v                         PIC 9(2).
005790*
005800 01 I—¹˜a—ï”NŒŽ“ú‚v.
005810   03 I—¹˜a—ï”NŒŽ‚v.
005820     05 I—¹˜a—ï‚v                     PIC 9.
005830     05 I—¹”NŒŽ‚v.
005840        07 I—¹”N‚v                    PIC 9(2).
005850        07 I—¹ŒŽ‚v                    PIC 9(2).
005860   03 I—¹“ú‚v                         PIC 9(2).
005870*
005880* •”ˆÊ–ˆ‚Ì‘½•”ˆÊ’üŒ¸•”ˆÊ–ˆ‚Ìƒ[ƒN
005890* ‚P•”ˆÊ–Ú
005900 01 Œã—Ã‰ñ”‚P‚q‚v                     PIC 9(2) VALUE ZERO.
005910 01 Œã—Ã—¿‚P‚q‚v                       PIC 9(5) VALUE ZERO.
005920 01 —âãª–@‰ñ”‚P‚q‚v                   PIC 9(2) VALUE ZERO.
005930 01 —âãª–@—¿‚P‚q‚v                     PIC 9(4) VALUE ZERO.
005940 01 ‰·ãª–@‰ñ”‚P‚q‚v                   PIC 9(2) VALUE ZERO.
005950 01 ‰·ãª–@—¿‚P‚q‚v                     PIC 9(4) VALUE ZERO.
005960 01 “d—Ã‰ñ”‚P‚q‚v                     PIC 9(2) VALUE ZERO.
005970 01 “d—Ã—¿‚P‚q‚v                       PIC 9(5) VALUE ZERO.
005980 01 ¬Œv‚P‚q‚v                         PIC 9(6) VALUE ZERO.
005990* ‚P•”ˆÊ–ÚSk
006000 01 Œã—Ã‰ñ”‚PSk‚q‚v                 PIC 9(2) VALUE ZERO.
006010 01 Œã—Ã—¿‚PSk‚q‚v                   PIC 9(5) VALUE ZERO.
006020 01 —âãª–@‰ñ”‚PSk‚q‚v               PIC 9(2) VALUE ZERO.
006030 01 —âãª–@—¿‚PSk‚q‚v                 PIC 9(4) VALUE ZERO.
006040 01 ‰·ãª–@‰ñ”‚PSk‚q‚v               PIC 9(2) VALUE ZERO.
006050 01 ‰·ãª–@—¿‚PSk‚q‚v                 PIC 9(4) VALUE ZERO.
006060 01 “d—Ã‰ñ”‚PSk‚q‚v                 PIC 9(2) VALUE ZERO.
006070 01 “d—Ã—¿‚PSk‚q‚v                   PIC 9(5) VALUE ZERO.
006080 01 ¬Œv‚PSk‚q‚v                     PIC 9(6) VALUE ZERO.
006090* ‚Q•”ˆÊ–Ú
006100 01 Œã—Ã‰ñ”‚Q‚q‚v                     PIC 9(2) VALUE ZERO.
006110 01 Œã—Ã—¿‚Q‚q‚v                       PIC 9(5) VALUE ZERO.
006120 01 —âãª–@‰ñ”‚Q‚q‚v                   PIC 9(2) VALUE ZERO.
006130 01 —âãª–@—¿‚Q‚q‚v                     PIC 9(4) VALUE ZERO.
006140 01 ‰·ãª–@‰ñ”‚Q‚q‚v                   PIC 9(2) VALUE ZERO.
006150 01 ‰·ãª–@—¿‚Q‚q‚v                     PIC 9(4) VALUE ZERO.
006160 01 “d—Ã‰ñ”‚Q‚q‚v                     PIC 9(2) VALUE ZERO.
006170 01 “d—Ã—¿‚Q‚q‚v                       PIC 9(5) VALUE ZERO.
006180 01 ¬Œv‚Q‚q‚v                         PIC 9(6) VALUE ZERO.
006190* ‚Q•”ˆÊ–ÚSk
006200 01 Œã—Ã‰ñ”‚QSk‚q‚v                 PIC 9(2) VALUE ZERO.
006210 01 Œã—Ã—¿‚QSk‚q‚v                   PIC 9(5) VALUE ZERO.
006220 01 —âãª–@‰ñ”‚QSk‚q‚v               PIC 9(2) VALUE ZERO.
006230 01 —âãª–@—¿‚QSk‚q‚v                 PIC 9(4) VALUE ZERO.
006240 01 ‰·ãª–@‰ñ”‚QSk‚q‚v               PIC 9(2) VALUE ZERO.
006250 01 ‰·ãª–@—¿‚QSk‚q‚v                 PIC 9(4) VALUE ZERO.
006260 01 “d—Ã‰ñ”‚QSk‚q‚v                 PIC 9(2) VALUE ZERO.
006270 01 “d—Ã—¿‚QSk‚q‚v                   PIC 9(5) VALUE ZERO.
006280 01 ¬Œv‚QSk‚q‚v                     PIC 9(6) VALUE ZERO.
006290* ‚R•”ˆÊ–Ú
006300 01 Œã—Ã‰ñ”‚R‚q‚v                     PIC 9(2) VALUE ZERO.
006310 01 Œã—Ã—¿‚R‚q‚v                       PIC 9(5) VALUE ZERO.
006320 01 —âãª–@‰ñ”‚R‚q‚v                   PIC 9(2) VALUE ZERO.
006330 01 —âãª–@—¿‚R‚q‚v                     PIC 9(4) VALUE ZERO.
006340 01 ‰·ãª–@‰ñ”‚R‚q‚v                   PIC 9(2) VALUE ZERO.
006350 01 ‰·ãª–@—¿‚R‚q‚v                     PIC 9(4) VALUE ZERO.
006360 01 “d—Ã‰ñ”‚R‚q‚v                     PIC 9(2) VALUE ZERO.
006370 01 “d—Ã—¿‚R‚q‚v                       PIC 9(5) VALUE ZERO.
006380 01 ¬Œv‚R‚q‚v                         PIC 9(6) VALUE ZERO.
006390* ‚R•”ˆÊ–ÚSk
006400 01 Œã—Ã‰ñ”‚RSk‚q‚v                     PIC 9(2) VALUE ZERO.
006410 01 Œã—Ã—¿‚RSk‚q‚v                       PIC 9(5) VALUE ZERO.
006420 01 —âãª–@‰ñ”‚RSk‚q‚v                   PIC 9(2) VALUE ZERO.
006430 01 —âãª–@—¿‚RSk‚q‚v                     PIC 9(4) VALUE ZERO.
006440 01 ‰·ãª–@‰ñ”‚RSk‚q‚v                   PIC 9(2) VALUE ZERO.
006450 01 ‰·ãª–@—¿‚RSk‚q‚v                     PIC 9(4) VALUE ZERO.
006460 01 “d—Ã‰ñ”‚RSk‚q‚v                     PIC 9(2) VALUE ZERO.
006470 01 “d—Ã—¿‚RSk‚q‚v                       PIC 9(5) VALUE ZERO.
006480 01 ¬Œv‚RSk‚q‚v                         PIC 9(6) VALUE ZERO.
006490* ‚S•”ˆÊ–Ú
006500 01 Œã—Ã‰ñ”‚S‚q‚v                     PIC 9(2) VALUE ZERO.
006510 01 Œã—Ã—¿‚S‚q‚v                       PIC 9(5) VALUE ZERO.
006520 01 —âãª–@‰ñ”‚S‚q‚v                   PIC 9(2) VALUE ZERO.
006530 01 —âãª–@—¿‚S‚q‚v                     PIC 9(4) VALUE ZERO.
006540 01 ‰·ãª–@‰ñ”‚S‚q‚v                   PIC 9(2) VALUE ZERO.
006550 01 ‰·ãª–@—¿‚S‚q‚v                     PIC 9(4) VALUE ZERO.
006560 01 “d—Ã‰ñ”‚S‚q‚v                     PIC 9(2) VALUE ZERO.
006570 01 “d—Ã—¿‚S‚q‚v                       PIC 9(5) VALUE ZERO.
006580 01 ¬Œv‚S‚q‚v                         PIC 9(6) VALUE ZERO.
006590* ‚S•”ˆÊ–ÚSk
006600 01 Œã—Ã‰ñ”‚SSk‚q‚v                     PIC 9(2) VALUE ZERO.
006610 01 Œã—Ã—¿‚SSk‚q‚v                       PIC 9(5) VALUE ZERO.
006620 01 —âãª–@‰ñ”‚SSk‚q‚v                   PIC 9(2) VALUE ZERO.
006630 01 —âãª–@—¿‚SSk‚q‚v                     PIC 9(4) VALUE ZERO.
006640 01 ‰·ãª–@‰ñ”‚SSk‚q‚v                   PIC 9(2) VALUE ZERO.
006650 01 ‰·ãª–@—¿‚SSk‚q‚v                     PIC 9(4) VALUE ZERO.
006660 01 “d—Ã‰ñ”‚SSk‚q‚v                     PIC 9(2) VALUE ZERO.
006670 01 “d—Ã—¿‚SSk‚q‚v                       PIC 9(5) VALUE ZERO.
006680 01 ¬Œv‚SSk‚q‚v                         PIC 9(6) VALUE ZERO.
006690* ‚T•”ˆÊ–Ú
006700 01 Œã—Ã‰ñ”‚T‚q‚v                     PIC 9(2) VALUE ZERO.
006710 01 Œã—Ã—¿‚T‚q‚v                       PIC 9(5) VALUE ZERO.
006720 01 —âãª–@‰ñ”‚T‚q‚v                   PIC 9(2) VALUE ZERO.
006730 01 —âãª–@—¿‚T‚q‚v                     PIC 9(4) VALUE ZERO.
006740 01 ‰·ãª–@‰ñ”‚T‚q‚v                   PIC 9(2) VALUE ZERO.
006750 01 ‰·ãª–@—¿‚T‚q‚v                     PIC 9(4) VALUE ZERO.
006760 01 “d—Ã‰ñ”‚T‚q‚v                     PIC 9(2) VALUE ZERO.
006770 01 “d—Ã—¿‚T‚q‚v                       PIC 9(5) VALUE ZERO.
006780 01 ¬Œv‚T‚q‚v                         PIC 9(6) VALUE ZERO.
006790* ‚T•”ˆÊ–ÚSk
006800 01 Œã—Ã‰ñ”‚TSk‚q‚v                     PIC 9(2) VALUE ZERO.
006810 01 Œã—Ã—¿‚TSk‚q‚v                       PIC 9(5) VALUE ZERO.
006820 01 —âãª–@‰ñ”‚TSk‚q‚v                   PIC 9(2) VALUE ZERO.
006830 01 —âãª–@—¿‚TSk‚q‚v                     PIC 9(4) VALUE ZERO.
006840 01 ‰·ãª–@‰ñ”‚TSk‚q‚v                   PIC 9(2) VALUE ZERO.
006850 01 ‰·ãª–@—¿‚TSk‚q‚v                     PIC 9(4) VALUE ZERO.
006860 01 “d—Ã‰ñ”‚TSk‚q‚v                     PIC 9(2) VALUE ZERO.
006870 01 “d—Ã—¿‚TSk‚q‚v                       PIC 9(5) VALUE ZERO.
006880 01 ¬Œv‚TSk‚q‚v                         PIC 9(6) VALUE ZERO.
006890* ‚U•”ˆÊ–Ú
006900 01 Œã—Ã‰ñ”‚U‚q‚v                     PIC 9(2) VALUE ZERO.
006910 01 Œã—Ã—¿‚U‚q‚v                       PIC 9(5) VALUE ZERO.
006920 01 —âãª–@‰ñ”‚U‚q‚v                   PIC 9(2) VALUE ZERO.
006930 01 —âãª–@—¿‚U‚q‚v                     PIC 9(4) VALUE ZERO.
006940 01 ‰·ãª–@‰ñ”‚U‚q‚v                   PIC 9(2) VALUE ZERO.
006950 01 ‰·ãª–@—¿‚U‚q‚v                     PIC 9(4) VALUE ZERO.
006960 01 “d—Ã‰ñ”‚U‚q‚v                     PIC 9(2) VALUE ZERO.
006970 01 “d—Ã—¿‚U‚q‚v                       PIC 9(5) VALUE ZERO.
006980 01 ¬Œv‚U‚q‚v                         PIC 9(6) VALUE ZERO.
006990* ‚U•”ˆÊ–ÚSk
007000 01 Œã—Ã‰ñ”‚USk‚q‚v                     PIC 9(2) VALUE ZERO.
007010 01 Œã—Ã—¿‚USk‚q‚v                       PIC 9(5) VALUE ZERO.
007020 01 —âãª–@‰ñ”‚USk‚q‚v                   PIC 9(2) VALUE ZERO.
007030 01 —âãª–@—¿‚USk‚q‚v                     PIC 9(4) VALUE ZERO.
007040 01 ‰·ãª–@‰ñ”‚USk‚q‚v                   PIC 9(2) VALUE ZERO.
007050 01 ‰·ãª–@—¿‚USk‚q‚v                     PIC 9(4) VALUE ZERO.
007060 01 “d—Ã‰ñ”‚USk‚q‚v                     PIC 9(2) VALUE ZERO.
007070 01 “d—Ã—¿‚USk‚q‚v                       PIC 9(5) VALUE ZERO.
007080 01 ¬Œv‚USk‚q‚v                         PIC 9(6) VALUE ZERO.
007090* ‚V•”ˆÊ–Ú
007100 01 Œã—Ã‰ñ”‚V‚q‚v                     PIC 9(2) VALUE ZERO.
007110 01 Œã—Ã—¿‚V‚q‚v                       PIC 9(5) VALUE ZERO.
007120 01 —âãª–@‰ñ”‚V‚q‚v                   PIC 9(2) VALUE ZERO.
007130 01 —âãª–@—¿‚V‚q‚v                     PIC 9(4) VALUE ZERO.
007140 01 ‰·ãª–@‰ñ”‚V‚q‚v                   PIC 9(2) VALUE ZERO.
007150 01 ‰·ãª–@—¿‚V‚q‚v                     PIC 9(4) VALUE ZERO.
007160 01 “d—Ã‰ñ”‚V‚q‚v                     PIC 9(2) VALUE ZERO.
007170 01 “d—Ã—¿‚V‚q‚v                       PIC 9(5) VALUE ZERO.
007180 01 ¬Œv‚V‚q‚v                         PIC 9(6) VALUE ZERO.
007190* ‚V•”ˆÊ–ÚSk
007200 01 Œã—Ã‰ñ”‚VSk‚q‚v                     PIC 9(2) VALUE ZERO.
007210 01 Œã—Ã—¿‚VSk‚q‚v                       PIC 9(5) VALUE ZERO.
007220 01 —âãª–@‰ñ”‚VSk‚q‚v                   PIC 9(2) VALUE ZERO.
007230 01 —âãª–@—¿‚VSk‚q‚v                     PIC 9(4) VALUE ZERO.
007240 01 ‰·ãª–@‰ñ”‚VSk‚q‚v                   PIC 9(2) VALUE ZERO.
007250 01 ‰·ãª–@—¿‚VSk‚q‚v                     PIC 9(4) VALUE ZERO.
007260 01 “d—Ã‰ñ”‚VSk‚q‚v                     PIC 9(2) VALUE ZERO.
007270 01 “d—Ã—¿‚VSk‚q‚v                       PIC 9(5) VALUE ZERO.
007280 01 ¬Œv‚VSk‚q‚v                         PIC 9(6) VALUE ZERO.
007290* ‚W•”ˆÊ–Ú
007300 01 Œã—Ã‰ñ”‚W‚q‚v                     PIC 9(2) VALUE ZERO.
007310 01 Œã—Ã—¿‚W‚q‚v                       PIC 9(5) VALUE ZERO.
007320 01 —âãª–@‰ñ”‚W‚q‚v                   PIC 9(2) VALUE ZERO.
007330 01 —âãª–@—¿‚W‚q‚v                     PIC 9(4) VALUE ZERO.
007340 01 ‰·ãª–@‰ñ”‚W‚q‚v                   PIC 9(2) VALUE ZERO.
007350 01 ‰·ãª–@—¿‚W‚q‚v                     PIC 9(4) VALUE ZERO.
007360 01 “d—Ã‰ñ”‚W‚q‚v                     PIC 9(2) VALUE ZERO.
007370 01 “d—Ã—¿‚W‚q‚v                       PIC 9(5) VALUE ZERO.
007380 01 ¬Œv‚W‚q‚v                         PIC 9(6) VALUE ZERO.
007390* ‚W•”ˆÊ–ÚSk
007400 01 Œã—Ã‰ñ”‚WSk‚q‚v                     PIC 9(2) VALUE ZERO.
007410 01 Œã—Ã—¿‚WSk‚q‚v                       PIC 9(5) VALUE ZERO.
007420 01 —âãª–@‰ñ”‚WSk‚q‚v                   PIC 9(2) VALUE ZERO.
007430 01 —âãª–@—¿‚WSk‚q‚v                     PIC 9(4) VALUE ZERO.
007440 01 ‰·ãª–@‰ñ”‚WSk‚q‚v                   PIC 9(2) VALUE ZERO.
007450 01 ‰·ãª–@—¿‚WSk‚q‚v                     PIC 9(4) VALUE ZERO.
007460 01 “d—Ã‰ñ”‚WSk‚q‚v                     PIC 9(2) VALUE ZERO.
007470 01 “d—Ã—¿‚WSk‚q‚v                       PIC 9(5) VALUE ZERO.
007480 01 ¬Œv‚WSk‚q‚v                         PIC 9(6) VALUE ZERO.
007490* ‚X•”ˆÊ–Ú
007500 01 Œã—Ã‰ñ”‚X‚q‚v                     PIC 9(2) VALUE ZERO.
007510 01 Œã—Ã—¿‚X‚q‚v                       PIC 9(5) VALUE ZERO.
007520 01 —âãª–@‰ñ”‚X‚q‚v                   PIC 9(2) VALUE ZERO.
007530 01 —âãª–@—¿‚X‚q‚v                     PIC 9(4) VALUE ZERO.
007540 01 ‰·ãª–@‰ñ”‚X‚q‚v                   PIC 9(2) VALUE ZERO.
007550 01 ‰·ãª–@—¿‚X‚q‚v                     PIC 9(4) VALUE ZERO.
007560 01 “d—Ã‰ñ”‚X‚q‚v                     PIC 9(2) VALUE ZERO.
007570 01 “d—Ã—¿‚X‚q‚v                       PIC 9(5) VALUE ZERO.
007580 01 ¬Œv‚X‚q‚v                         PIC 9(6) VALUE ZERO.
007590* ‚X•”ˆÊ–ÚSk
007600 01 Œã—Ã‰ñ”‚XSk‚q‚v                     PIC 9(2) VALUE ZERO.
007610 01 Œã—Ã—¿‚XSk‚q‚v                       PIC 9(5) VALUE ZERO.
007620 01 —âãª–@‰ñ”‚XSk‚q‚v                   PIC 9(2) VALUE ZERO.
007630 01 —âãª–@—¿‚XSk‚q‚v                     PIC 9(4) VALUE ZERO.
007640 01 ‰·ãª–@‰ñ”‚XSk‚q‚v                   PIC 9(2) VALUE ZERO.
007650 01 ‰·ãª–@—¿‚XSk‚q‚v                     PIC 9(4) VALUE ZERO.
007660 01 “d—Ã‰ñ”‚XSk‚q‚v                     PIC 9(2) VALUE ZERO.
007670 01 “d—Ã—¿‚XSk‚q‚v                       PIC 9(5) VALUE ZERO.
007680 01 ¬Œv‚XSk‚q‚v                         PIC 9(6) VALUE ZERO.
007690*
007700 01 •‰Ží•Ê—ªÌ‚v.
007710    03 •‰Ží•Ê—ªÌ‚v‚P                PIC N(4).
007720    03 FILLER                          PIC N(2).
007730 01 Œ³†–¼Ì‚v.
007740    03 Œ³†–¼Ì‚v‚P                    PIC N(2).
007750    03 FILLER                          PIC N(4).
007760 01 “]‹A‹æ•ª—ªÌ‚v.
007770    03 “]‹A‹æ•ª—ªÌ‚v‚P                PIC N(4).
007780    03 FILLER                          PIC N(2).
007790*
007800 01  •ÛŒ¯ŽÒ”Ô†‚v‚Q.
007810     03  •Û”Ô‚P‚v                      PIC X(2).
007820     03  •Û”Ô‚Q‚v                      PIC X(8).
007830* ”ï—p•‰’SŽÒ”Ô†iŽs’¬‘º”Ô†j‚v‚n‚q‚j
007840 01 ”ï—p•‰’SŽÒ”Ô†‚v.
007850     03  –@•Ê”Ô†‚v                    PIC X(2).
007860     03  ••ÛŒ¯‚v.
007870          05  •{Œ§‚v                   PIC X(2).
007880          05  •”Ô‚v                   PIC X     OCCURS 3.
007890          05  •‚b‚c‚v                 PIC X.
007900     03  FILLER                        PIC X(2).
007910*
007920* ˜JÐ•ÛŒ¯—p—¿‹àî•ñƒ[ƒN
007930* Šî–{
007940 01 ‰ŒŸ—¿‚b‚v                         PIC 9(4) VALUE ZERO.
007950 01 ŽžŠÔŠO‚b‚v                         PIC 9(4) VALUE ZERO.
007960 01 ‹x“ú‚b‚v                           PIC 9(4) VALUE ZERO.
007970 01 [–é‚b‚v                           PIC 9(4) VALUE ZERO.
007980 01 ‰—ÃŠî–{—¿‹à‚b‚v                   PIC 9(4) VALUE ZERO.
007990 01 ‰—Ã’Ç‰Á—¿‹à‚b‚v                   PIC 9(4) VALUE ZERO.
008000 01 ‰—Ã‰ÁŽZ—¦‚b‚v                     PIC 9(3) VALUE ZERO.
008010 01 ‰—Ã–éŠÔ‰ÁŽZ—¦‚b‚v                 PIC 9(3) VALUE ZERO.
008020 01 ‰—Ã[–é‰ÁŽZ—¦‚b‚v                 PIC 9(3) VALUE ZERO.
008030 01 ‰—Ã“ï˜H‰ÁŽZ—¦‚b‚v                 PIC 9(3) VALUE ZERO.
008040 01 ‰—Ã–\•—‰J‰ÁŽZ—¦‚b‚v               PIC 9(3) VALUE ZERO.
008050 01 ÄŒŸ—¿‚b‚v                         PIC 9(4) VALUE ZERO.
008060 01 —âãª–@—¿‚b‚v                       PIC 9(4) VALUE ZERO.
008070 01 ‰·ãª–@—¿‚b‚v                       PIC 9(4) VALUE ZERO.
008080 01 “d—Ã—¿‚b‚v                         PIC 9(4) VALUE ZERO.
008090 01 Ž{pî•ñ’ñ‹Ÿ—¿‚b‚v                 PIC 9(4) VALUE ZERO.
008100 01 Žw“±ŠÇ——¿‚b‚v                     PIC 9(4) VALUE ZERO.
008110 01 ‰^“®—Ã–@—¿‚b‚v                     PIC 9(4) VALUE ZERO.
008120 01 h”‘—¿‚b‚v                         PIC 9(4) VALUE ZERO.
008130 01 HŽ–—¿‚b‚v                         PIC 9(4) VALUE ZERO.
008140 01 ‹x‹ÆØ–¾‘—¿‚b‚v                   PIC 9(4) VALUE ZERO.
008150*/‰ŒŸŽž‘Š’k—¿/080829
008160 01 ‰ŒŸŽž‘Š’k—¿‚f‚v                   PIC 9(5) VALUE ZERO.
008170 01 ‰ŒŸŽž‘Š’k—¿‚v                     PIC 9(4) VALUE ZERO.
008180 01 ‰ŒŸŽž‘Š’k—¿‚b‚v                   PIC 9(4) VALUE ZERO.
008190* •‰•Ê
008200* 01 SkŒã—Ã—¿‚a‚v                     PIC 9(5) VALUE ZERO.
008210* 01 Œã—Ã—¿‚a‚v                         PIC 9(5) VALUE ZERO.
008220* 01 ®•œ—¿‚a‚v                         PIC 9(5) VALUE ZERO.
008230* •‰•Ê‚c
008240 01 SkŒã—Ã—¿‚c‚v                     PIC 9(5) VALUE ZERO.
008250 01 Œã—Ã—¿‚c‚v                         PIC 9(5) VALUE ZERO.
008260 01 ®•œ—¿‚c‚v                         PIC 9(5) VALUE ZERO.
008270 01 “Á•ÊÞ—¿”ï‚c‚v                     PIC 9(5) VALUE ZERO.
008280 01 ŒðŠ·•ï‘Ñ—¿‚c‚v                     PIC 9(5) VALUE ZERO.
008290*
008300* ƒGƒ‰[ƒƒbƒZ[ƒW—p
008310 01 ƒGƒ‰[ƒƒbƒZ[ƒW‚v.
008320    03 ƒGƒ‰[•”ˆÊƒR[ƒh‚v              PIC 9(5) VALUE ZERO.
008330    03 FILLER                          PIC X(15).
008340* â‘Î’lŒvŽZ—p
008350 01 ŒvŽZ˜a—ï”NŒŽ‚v.
008360     03 ŒvŽZ˜a—ï‚v                     PIC 9.
008370     03 ŒvŽZ”NŒŽ‚v.
008380        05 ŒvŽZ”N‚v                    PIC 9(2).
008390        05 ŒvŽZŒŽ‚v                    PIC 9(2).
008400     03 ŒvŽZ“ú‚v                       PIC 9(2).
008410 01 â‘Î’l‚v                           PIC 9(6).
008420 01 •‰Œo‰ß“ú‚v                       PIC 9(4).
008430 01 â‘Î’lŒŽ‚v‚Q                       PIC 9(3).
008440 01 ‰[”N‰ñ”‚v                         PIC 9(2).
008450 01 ’Êí”N‰ñ”‚v                       PIC 9(2).
008460 01 â‘Î’lŒŽ‚v                         PIC 9(3).
008470 01 ’Êí”Nâ‘Î’l‚v                     PIC 9(6).
008480 01 ‰[”Nâ‘Î’l‚v                       PIC 9(6).
008490 01 ”Nâ‘Î’l‚v                         PIC 9(6).
008500 01 •‰â‘Î’l‚v                       PIC 9(6).
008510 01 Ž{pâ‘Î’l‚v                       PIC 9(6).
008520 01 ¤‚v                               PIC 9(4) VALUE ZERO.
008530 01 è—]‚v                             PIC 9(4) VALUE ZERO.
008540 01 ‰ÁŽZ”N‚v                           PIC 9(4).
008550*/˜JÐ—pƒ[ƒN
008560 01 ‰ŒŸ‰ñ”‚v                         PIC 9    VALUE ZERO.
008570 01 ÄŒŸ‰ñ”‚v                         PIC 9    VALUE ZERO.
008580 01 •”ˆÊ–ˆƒf[ƒ^.
008590   03 “Á•ÊÞ—¿”ï‚Q‚v.
008600     05 Þ—¿”ï‚Q‚v                       PIC 9(5) OCCURS 9 VALUE ZERO.
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚Í‚R‰ñŽZ’è‚Å‚«‚éB/20180830
009780     05 Þ—¿”ï‚R‚v                       PIC 9(5) OCCURS 9 VALUE ZERO.
009880     05 Þ—¿‰ñ”‚v                       PIC 9    OCCURS 9 VALUE ZERO.
008610   03 •ï‘ÑŒðŠ·“ú‚v OCCURS 9.
008620     05 •ï‘ÑŒðŠ·“ú‚P‚v                   PIC 9(2) VALUE ZERO.
008630     05 •ï‘ÑŒðŠ·“ú‚Q‚v                   PIC 9(2) VALUE ZERO.
008640     05 •ï‘ÑŒðŠ·“ú‚R‚v                   PIC 9(2) VALUE ZERO.
008650     05 •ï‘ÑŒðŠ·“ú‚S‚v                   PIC 9(2) VALUE ZERO.
008660     05 •ï‘ÑŒðŠ·“ú‚T‚v                   PIC 9(2) VALUE ZERO.
008670     05 •ï‘ÑŒðŠ·“ú‚U‚v                   PIC 9(2) VALUE ZERO.
008680   03 •ï‘ÑŒðŠ·—¿‚v                       PIC 9(5) OCCURS 9 VALUE ZERO.
008690*/˜JÐV—pŽ†‘Î‰ž(1704~)/0506
008700   03 •ï‘ÑŒðŠ·’P‰¿‚v                     PIC 9(4) OCCURS 9 VALUE ZERO.
008710   03 •ï‘ÑŒðŠ·‰ñ”‚v                     PIC 9    OCCURS 9 VALUE ZERO.
008720*/
008730 01 Skƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
008740*
008750*/ƒGƒ‰[•\Ž¦‚ÌC³
008760 01 ƒGƒ‰[•\Ž¦‚e                       PIC 9(1)  VALUE ZERO.
008770
008780***********
008790* •”ˆÊ–ˆ‚Ì‘½•”ˆÊ’üŒ¸•”ˆÊ–ˆ{“ú–ˆ‚Ìƒ[ƒN
008800* ‚P•”ˆÊ–Ú
008810 01 Œã—Ã—¿‚P‚m‚q‚v                       PIC 9(5) VALUE ZERO.
008820 01 —âãª–@—¿‚P‚m‚q‚v                     PIC 9(4) VALUE ZERO.
008830 01 ‰·ãª–@—¿‚P‚m‚q‚v                     PIC 9(4) VALUE ZERO.
008840 01 “d—Ã—¿‚P‚m‚q‚v                       PIC 9(5) VALUE ZERO.
008850* ‚P•”ˆÊ–ÚSk
008860 01 Œã—Ã—¿‚PSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
008870 01 —âãª–@—¿‚PSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
008880 01 ‰·ãª–@—¿‚PSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
008890 01 “d—Ã—¿‚PSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
008900* ‚Q•”ˆÊ–Ú
008910 01 Œã—Ã—¿‚Q‚m‚q‚v                       PIC 9(5) VALUE ZERO.
008920 01 —âãª–@—¿‚Q‚m‚q‚v                     PIC 9(4) VALUE ZERO.
008930 01 ‰·ãª–@—¿‚Q‚m‚q‚v                     PIC 9(4) VALUE ZERO.
008940 01 “d—Ã—¿‚Q‚m‚q‚v                       PIC 9(5) VALUE ZERO.
008950* ‚Q•”ˆÊ–ÚSk
008960 01 Œã—Ã—¿‚QSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
008970 01 —âãª–@—¿‚QSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
008980 01 ‰·ãª–@—¿‚QSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
008990 01 “d—Ã—¿‚QSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009000* ‚R•”ˆÊ–Ú
009010 01 Œã—Ã—¿‚R‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009020 01 —âãª–@—¿‚R‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009030 01 ‰·ãª–@—¿‚R‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009040 01 “d—Ã—¿‚R‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009050* ‚R•”ˆÊ–ÚSk
009060 01 Œã—Ã—¿‚RSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009070 01 —âãª–@—¿‚RSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009080 01 ‰·ãª–@—¿‚RSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009090 01 “d—Ã—¿‚RSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009100* ‚S•”ˆÊ–Ú
009110 01 Œã—Ã—¿‚S‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009120 01 —âãª–@—¿‚S‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009130 01 ‰·ãª–@—¿‚S‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009140 01 “d—Ã—¿‚S‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009150* ‚S•”ˆÊ–ÚSk
009160 01 Œã—Ã—¿‚SSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009170 01 —âãª–@—¿‚SSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009180 01 ‰·ãª–@—¿‚SSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009190 01 “d—Ã—¿‚SSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009200* ‚T•”ˆÊ–Ú
009210 01 Œã—Ã—¿‚T‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009220 01 —âãª–@—¿‚T‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009230 01 ‰·ãª–@—¿‚T‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009240 01 “d—Ã—¿‚T‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009250* ‚T•”ˆÊ–ÚSk
009260 01 Œã—Ã—¿‚TSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009270 01 —âãª–@—¿‚TSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009280 01 ‰·ãª–@—¿‚TSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009290 01 “d—Ã—¿‚TSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009300* ‚U•”ˆÊ–Ú
009310 01 Œã—Ã—¿‚U‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009320 01 —âãª–@—¿‚U‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009330 01 ‰·ãª–@—¿‚U‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009340 01 “d—Ã—¿‚U‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009350* ‚U•”ˆÊ–ÚSk
009360 01 Œã—Ã—¿‚USk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009370 01 —âãª–@—¿‚USk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009380 01 ‰·ãª–@—¿‚USk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009390 01 “d—Ã—¿‚USk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009400* ‚V•”ˆÊ–Ú
009410 01 Œã—Ã—¿‚V‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009420 01 —âãª–@—¿‚V‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009430 01 ‰·ãª–@—¿‚V‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009440 01 “d—Ã—¿‚V‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009450* ‚V•”ˆÊ–ÚSk
009460 01 Œã—Ã—¿‚VSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009470 01 —âãª–@—¿‚VSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009480 01 ‰·ãª–@—¿‚VSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009490 01 “d—Ã—¿‚VSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009500* ‚W•”ˆÊ–Ú
009510 01 Œã—Ã—¿‚W‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009520 01 —âãª–@—¿‚W‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009530 01 ‰·ãª–@—¿‚W‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009540 01 “d—Ã—¿‚W‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009550* ‚W•”ˆÊ–ÚSk
009560 01 Œã—Ã—¿‚WSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009570 01 —âãª–@—¿‚WSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009580 01 ‰·ãª–@—¿‚WSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009590 01 “d—Ã—¿‚WSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009600* ‚X•”ˆÊ–Ú
009610 01 Œã—Ã—¿‚X‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009620 01 —âãª–@—¿‚X‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009630 01 ‰·ãª–@—¿‚X‚m‚q‚v                     PIC 9(4) VALUE ZERO.
009640 01 “d—Ã—¿‚X‚m‚q‚v                       PIC 9(5) VALUE ZERO.
009650* ‚X•”ˆÊ–ÚSk
009660 01 Œã—Ã—¿‚XSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009670 01 —âãª–@—¿‚XSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009680 01 ‰·ãª–@—¿‚XSk‚m‚q‚v                 PIC 9(4) VALUE ZERO.
009690 01 “d—Ã—¿‚XSk‚m‚q‚v                   PIC 9(5) VALUE ZERO.
009700*
009710 01 I—¹ƒtƒ‰ƒO‚R                       PIC X(3) VALUE SPACE.
009720 01 ƒŒƒZƒvƒgŽí—Þ‚v                     PIC X(4) VALUE SPACE.
009730 01 •¬ƒJƒEƒ“ƒ^                       PIC 9     VALUE ZERO.
009740 01 XVƒtƒ‰ƒO                         PIC X(3)  VALUE SPACE.
009750* ‰ŒŸ“ú‘Þ”ð—p
009760 01 ‰ŒŸ”NŒŽ“ú‚v‚s.
009770    03 ‰ŒŸ˜a—ï‚v‚s                    PIC 9     VALUE ZERO.
009780    03 ‰ŒŸ”N‚v‚s                      PIC 9(2)  VALUE ZERO.
009790    03 ‰ŒŸŒŽ‚v‚s                      PIC 9(2)  VALUE ZERO.
009800    03 ‰ŒŸ“ú‚v‚s                      PIC 9(2)  VALUE ZERO.
009810* ƒL[ƒŒƒZŽí•Ê—p
009820 01 ƒŒƒZŽí•Ê‚v                         PIC 9(2) VALUE ZERO.
009830 01 V‹Kƒ`ƒFƒbƒN‚v                     PIC N     VALUE SPACE.
009840 01 Œp‘±ƒ`ƒFƒbƒN‚v                     PIC N     VALUE SPACE.
009850 01 ‰ŒŸƒtƒ‰ƒO                         PIC X(3)  VALUE SPACE.
009860 01 ŽÀ“ú”‚v                           PIC 9(2)  VALUE ZERO.
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
009870*ªªª************
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
009880*
009890 01 ‰æ–Ê§Œä.
009900     03 ’è‹`‘Ì–¼                       PIC X(8) VALUE SPACE.
009910     03 €–ÚŒQ–¼                       PIC X(8) VALUE SPACE.
009920     03 €–ÚŒQ–¼‚q  REDEFINES  €–ÚŒQ–¼.
009930         05 €–ÚŒQ‚q–¼                 PIC X(4).
009940         05 €–ÚŒQ”Ô†                 PIC 9(2).
009950         05 FILLER                     PIC X(2).
009960     03 ˆ—Ží•Ê                       PIC X(2) VALUE SPACE.
009970     03 ƒAƒeƒ“ƒVƒ‡ƒ“                   PIC X(4) VALUE SPACE.
009980     03 ‹@”\ƒL[                       PIC X(4) VALUE SPACE.
009990     03 Šg’£§Œä.
010000         05 ’[––§Œä                   PIC X(4) VALUE SPACE.
010010         05 Ú×§Œä                   PIC X(2) VALUE SPACE.
010020     03 ’Ê’mî•ñ                       PIC X(2) VALUE SPACE.
010030     03 ƒ†ƒjƒbƒg–¼.
010040         05 FILLER                     PIC X(2)  VALUE SPACE.
010050         05 ’[––”Ô†                   PIC 9(3)  VALUE ZERO.
010060         05 FILLER                     PIC X(4)  VALUE SPACE.
010070     03 ƒƒbƒZ[ƒW‚v                   PIC N(20) VALUE SPACE.
010080* “ú•t‚v‚n‚q‚j
010090 01 ˜a—ïI—¹”N‚v                       PIC 9(4) VALUE ZERO.
010100 01 ŒvŽZ‹@˜a—ï”N‚v                     PIC 9(2) VALUE ZERO.
010110*
010120 01 ŒvŽZ‹@¼—ï.
010130    03 ŒvŽZ‹@¼—ï”N                    PIC 9(4).
010140    03 ŒvŽZ‹@¼—ïŒŽ“ú                  PIC 9(4).
010150 01 ŒvŽZ‹@¼—ï‚q REDEFINES ŒvŽZ‹@¼—ï.
010160    03 ŒvŽZ‹@¢‹I                      PIC 9(2).
010170    03 ŒvŽZ‹@“ú•t                      PIC 9(6).
010180    03 ŒvŽZ‹@“ú•t‚q REDEFINES ŒvŽZ‹@“ú•t.
010190       05 ŒvŽZ‹@”NŒŽ                   PIC 9(4).
010200       05 ŒvŽZ‹@”NŒŽ‚q REDEFINES ŒvŽZ‹@”NŒŽ.
010210         07 ŒvŽZ‹@”N                   PIC 9(2).
010220         07 ŒvŽZ‹@ŒŽ                   PIC 9(2).
010230       05 ŒvŽZ‹@“ú                     PIC 9(2).
010240*
010250* ‚e‚n‚q‚li‰æ–Ê’è‹`‘Ìj‚Ì“WŠJ
010260*
010270* ƒtƒ@ƒCƒ‹Ž¯•Ê–¼
010280*****************************************************************
010290*                          ˜AŒ‹€–Ú                             *
010300*****************************************************************
010310*
010320*    ŒŽ–ˆ‚Ì—¿‹à
010330***********************
010340 01 ˜A—¿˜JŽ©|—¿‹à‚P IS EXTERNAL.
010350   03 ˜A—¿˜JŽ©|‰ŒŸ.
010360      05 ˜A—¿˜JŽ©|•‰’S—¦                 PIC 9(3).
010370      05 ˜A—¿˜JŽ©|‰ŒŸ—¿                 PIC 9(5).
010380      05 ˜A—¿˜JŽ©|‰ŒŸ‰ñ”               PIC 9.
010390      05 ˜A—¿˜JŽ©|‰ŒŸ’P‰¿               PIC 9(5).
010400      05 ˜A—¿˜JŽ©|‰ŒŸ‰ÁŽZ.
010410        07 ˜A—¿˜JŽ©|‰ŒŸŽžŠÔŠO           PIC 9.
010420        07 ˜A—¿˜JŽ©|‰ŒŸ‹x“ú             PIC 9.
010430        07 ˜A—¿˜JŽ©|‰ŒŸ[–é             PIC 9.
010440      05 ˜A—¿˜JŽ©|‰ŒŸ‰ÁŽZ—¿             PIC 9(5).
010450   03 ˜A—¿˜JŽ©|ÄŒŸ.
010460      05 ˜A—¿˜JŽ©|ÄŒŸ—¿                 PIC 9(5).
010470      05 ˜A—¿˜JŽ©|ÄŒŸ‰ñ”               PIC 9.
010480      05 ˜A—¿˜JŽ©|ÄŒŸ’P‰¿               PIC 9(5).
010490   03 ˜A—¿˜JŽ©|‰—Ã.
010500      05 ˜A—¿˜JŽ©|‰—Ã‹——£               PIC 9(2)V9.
010510      05 ˜A—¿˜JŽ©|‰—Ã‰ñ”               PIC 9(2).
010520      05 ˜A—¿˜JŽ©|‰—Ã’P‰¿               PIC 9(5).
010530      05 ˜A—¿˜JŽ©|‰—Ã—¿                 PIC 9(6).
010540      05 ˜A—¿˜JŽ©|‰—Ã‰ÁŽZ—¿             PIC 9(5).
010550      05 ˜A—¿˜JŽ©|‰—Ã‰ÁŽZ.
010560        07 ˜A—¿˜JŽ©|‰—Ã–éŠÔ             PIC 9.
010570        07 ˜A—¿˜JŽ©|‰—Ã“ï˜H             PIC 9.
010580        07 ˜A—¿˜JŽ©|‰—Ã–\•—‰J           PIC 9.
010590        07 ˜A—¿˜JŽ©|‰—ÃŽžŠÔŠO           PIC 9.
010600   03 ˜A—¿˜JŽ©|‹à‘®•›Žq‰ÁŽZ—¿            PIC 9(5).
010610   03 ˜A—¿˜JŽ©|‹à‘®•›Žq”.
010620      05 ˜A—¿˜JŽ©|‘å                     PIC 9.
010630      05 ˜A—¿˜JŽ©|’†                     PIC 9.
010640      05 ˜A—¿˜JŽ©|¬                     PIC 9.
010650   03 ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ.
010660      05 ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ‰ñ”           PIC 9(2).
010670      05 ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ’P‰¿           PIC 9(4).
010680      05 ˜A—¿˜JŽ©|Ž{pî•ñ’ñ‹Ÿ—¿         PIC 9(5).
010690   03 ˜A—¿˜JŽ©|¬Œv                      PIC 9(7).
010700   03 ˜A—¿˜JŽ©|”ï—pŠz                    PIC 9(6).
010710   03 ˜A—¿˜JŽ©|ˆê•”•‰’S‹à                PIC 9(6).
010720   03 ˜A—¿˜JŽ©|¿‹Šz                    PIC 9(6).
010730   03 ˜A—¿˜JŽ©|‹‹•tŠ„‡                  PIC 9(3).
010740   03 ˜A—¿˜JŽ©|Žó‹‹ŽÒ•‰’SŠz              PIC 9(6).
010750   03 ˜A—¿˜JŽ©|•¬¿‹‹àŠz              PIC 9(6).
010760*
010770   03 ˜A—¿˜JŽ©|Žw“±ŠÇ—.
010780      05 ˜A—¿˜JŽ©|Žw“±ŠÇ—‰ñ”           PIC 9.
010790      05 ˜A—¿˜JŽ©|Žw“±ŠÇ—’P‰¿           PIC 9(4).
010800      05 ˜A—¿˜JŽ©|Žw“±ŠÇ——¿             PIC 9(4).
010810   03 ˜A—¿˜JŽ©|‰^“®—Ã–@.
010820      05 ˜A—¿˜JŽ©|‰^“®—Ã–@‰ñ”           PIC 9.
010830      05 ˜A—¿˜JŽ©|‰^“®—Ã–@’P‰¿           PIC 9(4).
010840      05 ˜A—¿˜JŽ©|‰^“®—Ã–@—¿             PIC 9(4).
010850   03 ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‘.
010860      05 ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‰ñ”       PIC 9(2).
010870      05 ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾’P‰¿       PIC 9(4).
010880      05 ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‘—¿       PIC 9(5).
010890   03 ˜A—¿˜JŽ©|h”‘.
010900      05 ˜A—¿˜JŽ©|h”‘‰ñ”               PIC 9(2).
010910      05 ˜A—¿˜JŽ©|h”‘’P‰¿               PIC 9(4).
010920      05 ˜A—¿˜JŽ©|h”‘—¿                 PIC 9(5).
010930   03 ˜A—¿˜JŽ©|HŽ–.
010940      05 ˜A—¿˜JŽ©|HŽ–‰ñ”               PIC 9(2).
010950      05 ˜A—¿˜JŽ©|HŽ–’P‰¿               PIC 9(4).
010960      05 ˜A—¿˜JŽ©|HŽ–—¿                 PIC 9(5).
010970*/Ž©”…Ó•ÛŒ¯‚Ì‚Ý
010980   03 ˜A—¿˜JŽ©|Ž{pØ–¾‘.
010990      05 ˜A—¿˜JŽ©|Ž{pØ–¾‰ñ”           PIC 9(2).
011000      05 ˜A—¿˜JŽ©|Ž{pØ–¾’P‰¿           PIC 9(4).
011010      05 ˜A—¿˜JŽ©|Ž{pØ–¾‘—¿           PIC 9(5).
011020*
011030   03 ˜A—¿˜JŽ©|Ž{p–¾×‘.
011040      05 ˜A—¿˜JŽ©|Ž{p–¾×‰ñ”           PIC 9(2).
011050      05 ˜A—¿˜JŽ©|Ž{p–¾×’P‰¿           PIC 9(4).
011060      05 ˜A—¿˜JŽ©|Ž{p–¾×‘—¿           PIC 9(5).
011070   03 ˜A—¿˜JŽ©|Ž©”…Ó‚»‚Ì‘¼‹àŠz          PIC 9(6).
011080**************************
011090*
011100   03 ˜A—¿˜JŽ©|Žó‹‹ŽÒ•‰’SŠz“à–ó   OCCURS  10.
011110      05 ˜A—¿˜JŽ©|“–“ú•ª•‰’SŠz           PIC 9(4).
011120*
011130*
011140* •‰•”ˆÊ–ˆ‚Ì—¿‹à
011150***********************
011160 01 ˜A—¿˜JŽ©|—¿‹à‚Q IS EXTERNAL.
011170   03 ˜A—¿˜JŽ©|‰‰ñˆ’u    OCCURS   9.
011180      05 ˜A—¿˜JŽ©|‰‰ñˆ’u—¿             PIC 9(5).
011190   03 ˜A—¿˜JŽ©|‰‰ñˆ’u—¿Œv              PIC 9(6).
011200   03 ˜A—¿˜JŽ©|Þ—¿”ï                    PIC 9(5) OCCURS 9.
011210   03 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú OCCURS 9.
011220     05 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚P            PIC 9(2).
011230     05 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚Q            PIC 9(2).
011240     05 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚R            PIC 9(2).
011250     05 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚S            PIC 9(2).
011260     05 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚T            PIC 9(2).
011270     05 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚U            PIC 9(2).
011280   03 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·—¿                PIC 9(5) OCCURS 9.
011290   03 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·’P‰¿              PIC 9(4) OCCURS 9.
011300   03 ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·‰ñ”              PIC 9    OCCURS 9.
011310*
011320* ’üŒ¸–ˆ‚Ì—¿‹à
011330***********************
011340 01 ˜A—¿˜JŽ©|—¿‹à‚R IS EXTERNAL.
011350**********
011360* ‚P•”ˆÊ *
011370**********
011380   03 ˜A—¿˜JŽ©|•”ˆÊ‚P.
011390      05 ˜A—¿˜JŽ©|Œã—Ã‚P.
011400         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚P          PIC 9(4).
011410         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚P          PIC 9(2).
011420         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚P            PIC 9(5).
011430      05 ˜A—¿˜JŽ©|—âãª–@‚P.
011440         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚P        PIC 9(4).
011450         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚P        PIC 9(2).
011460         07 ˜A—¿˜JŽ©|—âãª–@—¿‚P          PIC 9(4).
011470      05 ˜A—¿˜JŽ©|‰·ãª–@‚P.
011480         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚P        PIC 9(4).
011490         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚P        PIC 9(2).
011500         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚P          PIC 9(4).
011510      05 ˜A—¿˜JŽ©|“d—Ã‚P.
011520         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚P          PIC 9(4).
011530         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚P          PIC 9(2).
011540         07 ˜A—¿˜JŽ©|“d—Ã—¿‚P            PIC 9(5).
011550      05 ˜A—¿˜JŽ©|¬Œv‚P                 PIC 9(6).
011560      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚P           PIC 9(3).
011570      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚P           PIC 9(6).
011580**************
011590* ‚P•”ˆÊSk *
011600**************
011610   03 ˜A—¿˜JŽ©|•”ˆÊ‚PSk.
011620      05 ˜A—¿˜JŽ©|Œã—Ã‚PSk.
011630         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚PSk          PIC 9(4).
011640         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚PSk          PIC 9(2).
011650         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚PSk            PIC 9(5).
011660      05 ˜A—¿˜JŽ©|—âãª–@‚PSk.
011670         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚PSk        PIC 9(4).
011680         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚PSk        PIC 9(2).
011690         07 ˜A—¿˜JŽ©|—âãª–@—¿‚PSk          PIC 9(4).
011700      05 ˜A—¿˜JŽ©|‰·ãª–@‚PSk.
011710         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚PSk        PIC 9(4).
011720         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚PSk        PIC 9(2).
011730         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚PSk          PIC 9(4).
011740      05 ˜A—¿˜JŽ©|“d—Ã‚PSk.
011750         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚PSk          PIC 9(4).
011760         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚PSk          PIC 9(2).
011770         07 ˜A—¿˜JŽ©|“d—Ã—¿‚PSk            PIC 9(5).
011780      05 ˜A—¿˜JŽ©|¬Œv‚PSk                 PIC 9(6).
011790      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚PSk           PIC 9(3).
011800      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚PSk           PIC 9(6).
011810*/‚P•”ˆÊ–Ú‘½•”ˆÊ
011820      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚P           PIC 9(4).
011830      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚P           PIC 9(2).
011840      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚P             PIC 9(5).
011850*/‚P•”ˆÊ–Ú’·Šú
011860      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚P             PIC 9(4).
011870      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚P             PIC 9(2).
011880      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚P               PIC 9(5).
011890*/‚P•”ˆÊ–Ú‘½•”ˆÊSk
011900      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚P       PIC 9(4).
011910      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚P       PIC 9(2).
011920      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚P         PIC 9(5).
011930**********
011940* ‚Q•”ˆÊ *
011950**********
011960   03 ˜A—¿˜JŽ©|•”ˆÊ‚Q.
011970      05 ˜A—¿˜JŽ©|Œã—Ã‚Q.
011980         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚Q          PIC 9(4).
011990         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚Q          PIC 9(2).
012000         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚Q            PIC 9(5).
012010      05 ˜A—¿˜JŽ©|—âãª–@‚Q.
012020         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚Q        PIC 9(4).
012030         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚Q        PIC 9(2).
012040         07 ˜A—¿˜JŽ©|—âãª–@—¿‚Q          PIC 9(4).
012050      05 ˜A—¿˜JŽ©|‰·ãª–@‚Q.
012060         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚Q        PIC 9(4).
012070         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚Q        PIC 9(2).
012080         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚Q          PIC 9(4).
012090      05 ˜A—¿˜JŽ©|“d—Ã‚Q.
012100         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚Q          PIC 9(4).
012110         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚Q          PIC 9(2).
012120         07 ˜A—¿˜JŽ©|“d—Ã—¿‚Q            PIC 9(5).
012130      05 ˜A—¿˜JŽ©|¬Œv‚Q                 PIC 9(6).
012140      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚Q           PIC 9(3).
012150      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚Q           PIC 9(6).
012160**************
012170* ‚Q•”ˆÊSk *
012180**************
012190   03 ˜A—¿˜JŽ©|•”ˆÊ‚QSk.
012200      05 ˜A—¿˜JŽ©|Œã—Ã‚QSk.
012210         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚QSk          PIC 9(4).
012220         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚QSk          PIC 9(2).
012230         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚QSk            PIC 9(5).
012240      05 ˜A—¿˜JŽ©|—âãª–@‚QSk.
012250         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚QSk        PIC 9(4).
012260         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚QSk        PIC 9(2).
012270         07 ˜A—¿˜JŽ©|—âãª–@—¿‚QSk          PIC 9(4).
012280      05 ˜A—¿˜JŽ©|‰·ãª–@‚QSk.
012290         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚QSk        PIC 9(4).
012300         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚QSk        PIC 9(2).
012310         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚QSk          PIC 9(4).
012320      05 ˜A—¿˜JŽ©|“d—Ã‚QSk.
012330         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚QSk          PIC 9(4).
012340         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚QSk          PIC 9(2).
012350         07 ˜A—¿˜JŽ©|“d—Ã—¿‚QSk            PIC 9(5).
012360      05 ˜A—¿˜JŽ©|¬Œv‚QSk                 PIC 9(6).
012370      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚QSk           PIC 9(3).
012380      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚QSk           PIC 9(6).
012390*/‚Q•”ˆÊ–Ú‘½•”ˆÊ
012400      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚Q           PIC 9(4).
012410      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚Q           PIC 9(2).
012420      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚Q             PIC 9(5).
012430*/‚Q•”ˆÊ–Ú’·Šú
012440      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚Q             PIC 9(4).
012450      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚Q             PIC 9(2).
012460      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚Q               PIC 9(5).
012470*/‚Q•”ˆÊ–Ú‘½•”ˆÊSk
012480      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚Q       PIC 9(4).
012490      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚Q       PIC 9(2).
012500      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚Q         PIC 9(5).
012510**********
012520* ‚R•”ˆÊ *
012530**********
012540   03 ˜A—¿˜JŽ©|•”ˆÊ‚R.
012550      05 ˜A—¿˜JŽ©|Œã—Ã‚R.
012560         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚R          PIC 9(4).
012570         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚R          PIC 9(2).
012580         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚R            PIC 9(5).
012590      05 ˜A—¿˜JŽ©|—âãª–@‚R.
012600         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚R        PIC 9(4).
012610         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚R        PIC 9(2).
012620         07 ˜A—¿˜JŽ©|—âãª–@—¿‚R          PIC 9(4).
012630      05 ˜A—¿˜JŽ©|‰·ãª–@‚R.
012640         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚R        PIC 9(4).
012650         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚R        PIC 9(2).
012660         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚R          PIC 9(4).
012670      05 ˜A—¿˜JŽ©|“d—Ã‚R.
012680         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚R          PIC 9(4).
012690         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚R          PIC 9(2).
012700         07 ˜A—¿˜JŽ©|“d—Ã—¿‚R            PIC 9(5).
012710      05 ˜A—¿˜JŽ©|¬Œv‚R                 PIC 9(6).
012720      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚R           PIC 9(3).
012730      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚R           PIC 9(6).
012740**************
012750* ‚R•”ˆÊSk *
012760**************
012770   03 ˜A—¿˜JŽ©|•”ˆÊ‚RSk.
012780      05 ˜A—¿˜JŽ©|Œã—Ã‚RSk.
012790         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚RSk          PIC 9(4).
012800         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚RSk          PIC 9(2).
012810         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚RSk            PIC 9(5).
012820      05 ˜A—¿˜JŽ©|—âãª–@‚RSk.
012830         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚RSk        PIC 9(4).
012840         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚RSk        PIC 9(2).
012850         07 ˜A—¿˜JŽ©|—âãª–@—¿‚RSk          PIC 9(4).
012860      05 ˜A—¿˜JŽ©|‰·ãª–@‚RSk.
012870         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚RSk        PIC 9(4).
012880         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚RSk        PIC 9(2).
012890         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚RSk          PIC 9(4).
012900      05 ˜A—¿˜JŽ©|“d—Ã‚RSk.
012910         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚RSk          PIC 9(4).
012920         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚RSk          PIC 9(2).
012930         07 ˜A—¿˜JŽ©|“d—Ã—¿‚RSk            PIC 9(5).
012940      05 ˜A—¿˜JŽ©|¬Œv‚RSk                 PIC 9(6).
012950      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚RSk           PIC 9(3).
012960      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚RSk           PIC 9(6).
012970*/‚R•”ˆÊ–Ú‘½•”ˆÊ
012980      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚R           PIC 9(4).
012990      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚R           PIC 9(2).
013000      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚R             PIC 9(5).
013010*/‚R•”ˆÊ–Ú’·Šú
013020      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚R             PIC 9(4).
013030      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚R             PIC 9(2).
013040      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚R               PIC 9(5).
013050*/‚R•”ˆÊ–Ú‘½•”ˆÊSk
013060      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚R       PIC 9(4).
013070      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚R       PIC 9(2).
013080      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚R         PIC 9(5).
013090**********
013100* ‚S•”ˆÊ *
013110**********
013120   03 ˜A—¿˜JŽ©|•”ˆÊ‚S.
013130      05 ˜A—¿˜JŽ©|Œã—Ã‚S.
013140         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚S          PIC 9(4).
013150         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚S          PIC 9(2).
013160         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚S            PIC 9(5).
013170      05 ˜A—¿˜JŽ©|—âãª–@‚S.
013180         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚S        PIC 9(4).
013190         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚S        PIC 9(2).
013200         07 ˜A—¿˜JŽ©|—âãª–@—¿‚S          PIC 9(4).
013210      05 ˜A—¿˜JŽ©|‰·ãª–@‚S.
013220         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚S        PIC 9(4).
013230         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚S        PIC 9(2).
013240         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚S          PIC 9(4).
013250      05 ˜A—¿˜JŽ©|“d—Ã‚S.
013260         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚S          PIC 9(4).
013270         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚S          PIC 9(2).
013280         07 ˜A—¿˜JŽ©|“d—Ã—¿‚S            PIC 9(5).
013290      05 ˜A—¿˜JŽ©|¬Œv‚S                 PIC 9(6).
013300      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚S           PIC 9(3).
013310      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚S           PIC 9(6).
013320**************
013330* ‚S•”ˆÊSk *
013340**************
013350   03 ˜A—¿˜JŽ©|•”ˆÊ‚SSk.
013360      05 ˜A—¿˜JŽ©|Œã—Ã‚SSk.
013370         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚SSk          PIC 9(4).
013380         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚SSk          PIC 9(2).
013390         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚SSk            PIC 9(5).
013400      05 ˜A—¿˜JŽ©|—âãª–@‚SSk.
013410         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚SSk        PIC 9(4).
013420         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚SSk        PIC 9(2).
013430         07 ˜A—¿˜JŽ©|—âãª–@—¿‚SSk          PIC 9(4).
013440      05 ˜A—¿˜JŽ©|‰·ãª–@‚SSk.
013450         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚SSk        PIC 9(4).
013460         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚SSk        PIC 9(2).
013470         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚SSk          PIC 9(4).
013480      05 ˜A—¿˜JŽ©|“d—Ã‚SSk.
013490         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚SSk          PIC 9(4).
013500         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚SSk          PIC 9(2).
013510         07 ˜A—¿˜JŽ©|“d—Ã—¿‚SSk            PIC 9(5).
013520      05 ˜A—¿˜JŽ©|¬Œv‚SSk                 PIC 9(6).
013530      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚SSk           PIC 9(3).
013540      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚SSk           PIC 9(6).
013550*/‚S•”ˆÊ–Ú‘½•”ˆÊ
013560      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚S           PIC 9(4).
013570      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚S           PIC 9(2).
013580      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚S             PIC 9(5).
013590*/‚S•”ˆÊ–Ú’·Šú
013600      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚S             PIC 9(4).
013610      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚S             PIC 9(2).
013620      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚S               PIC 9(5).
013630*/‚S•”ˆÊ–Ú‘½•”ˆÊSk
013640      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚S       PIC 9(4).
013650      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚S       PIC 9(2).
013660      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚S         PIC 9(5).
013670**********
013680* ‚T•”ˆÊ *
013690**********
013700   03 ˜A—¿˜JŽ©|•”ˆÊ‚T.
013710      05 ˜A—¿˜JŽ©|Œã—Ã‚T.
013720         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚T          PIC 9(4).
013730         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚T          PIC 9(2).
013740         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚T            PIC 9(5).
013750      05 ˜A—¿˜JŽ©|—âãª–@‚T.
013760         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚T        PIC 9(4).
013770         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚T        PIC 9(2).
013780         07 ˜A—¿˜JŽ©|—âãª–@—¿‚T          PIC 9(4).
013790      05 ˜A—¿˜JŽ©|‰·ãª–@‚T.
013800         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚T        PIC 9(4).
013810         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚T        PIC 9(2).
013820         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚T          PIC 9(4).
013830      05 ˜A—¿˜JŽ©|“d—Ã‚T.
013840         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚T          PIC 9(4).
013850         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚T          PIC 9(2).
013860         07 ˜A—¿˜JŽ©|“d—Ã—¿‚T            PIC 9(5).
013870      05 ˜A—¿˜JŽ©|¬Œv‚T                 PIC 9(6).
013880      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚T           PIC 9(3).
013890      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚T           PIC 9(6).
013900**************
013910* ‚T•”ˆÊSk *
013920**************
013930   03 ˜A—¿˜JŽ©|•”ˆÊ‚TSk.
013940      05 ˜A—¿˜JŽ©|Œã—Ã‚TSk.
013950         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚TSk          PIC 9(4).
013960         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚TSk          PIC 9(2).
013970         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚TSk            PIC 9(5).
013980      05 ˜A—¿˜JŽ©|—âãª–@‚TSk.
013990         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚TSk        PIC 9(4).
014000         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚TSk        PIC 9(2).
014010         07 ˜A—¿˜JŽ©|—âãª–@—¿‚TSk          PIC 9(4).
014020      05 ˜A—¿˜JŽ©|‰·ãª–@‚TSk.
014030         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚TSk        PIC 9(4).
014040         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚TSk        PIC 9(2).
014050         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚TSk          PIC 9(4).
014060      05 ˜A—¿˜JŽ©|“d—Ã‚TSk.
014070         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚TSk          PIC 9(4).
014080         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚TSk          PIC 9(2).
014090         07 ˜A—¿˜JŽ©|“d—Ã—¿‚TSk            PIC 9(5).
014100      05 ˜A—¿˜JŽ©|¬Œv‚TSk                 PIC 9(6).
014110      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚TSk           PIC 9(3).
014120      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚TSk           PIC 9(6).
014130*/‚T•”ˆÊ–Ú‘½•”ˆÊ
014140      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚T           PIC 9(4).
014150      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚T           PIC 9(2).
014160      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚T             PIC 9(5).
014170*/‚T•”ˆÊ–Ú’·Šú
014180      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚T             PIC 9(4).
014190      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚T             PIC 9(2).
014200      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚T               PIC 9(5).
014210*/‚T•”ˆÊ–Ú‘½•”ˆÊSk
014220      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚T       PIC 9(4).
014230      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚T       PIC 9(2).
014240      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚T         PIC 9(5).
014250**********
014260* ‚U•”ˆÊ *
014270**********
014280   03 ˜A—¿˜JŽ©|•”ˆÊ‚U.
014290      05 ˜A—¿˜JŽ©|Œã—Ã‚U.
014300         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚U          PIC 9(4).
014310         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚U          PIC 9(2).
014320         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚U            PIC 9(5).
014330      05 ˜A—¿˜JŽ©|—âãª–@‚U.
014340         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚U        PIC 9(4).
014350         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚U        PIC 9(2).
014360         07 ˜A—¿˜JŽ©|—âãª–@—¿‚U          PIC 9(4).
014370      05 ˜A—¿˜JŽ©|‰·ãª–@‚U.
014380         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚U        PIC 9(4).
014390         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚U        PIC 9(2).
014400         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚U          PIC 9(4).
014410      05 ˜A—¿˜JŽ©|“d—Ã‚U.
014420         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚U          PIC 9(4).
014430         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚U          PIC 9(2).
014440         07 ˜A—¿˜JŽ©|“d—Ã—¿‚U            PIC 9(5).
014450      05 ˜A—¿˜JŽ©|¬Œv‚U                 PIC 9(6).
014460      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚U           PIC 9(3).
014470      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚U           PIC 9(6).
014480**************
014490* ‚U•”ˆÊSk *
014500**************
014510   03 ˜A—¿˜JŽ©|•”ˆÊ‚USk.
014520      05 ˜A—¿˜JŽ©|Œã—Ã‚USk.
014530         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚USk          PIC 9(4).
014540         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚USk          PIC 9(2).
014550         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚USk            PIC 9(5).
014560      05 ˜A—¿˜JŽ©|—âãª–@‚USk.
014570         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚USk        PIC 9(4).
014580         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚USk        PIC 9(2).
014590         07 ˜A—¿˜JŽ©|—âãª–@—¿‚USk          PIC 9(4).
014600      05 ˜A—¿˜JŽ©|‰·ãª–@‚USk.
014610         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚USk        PIC 9(4).
014620         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚USk        PIC 9(2).
014630         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚USk          PIC 9(4).
014640      05 ˜A—¿˜JŽ©|“d—Ã‚USk.
014650         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚USk          PIC 9(4).
014660         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚USk          PIC 9(2).
014670         07 ˜A—¿˜JŽ©|“d—Ã—¿‚USk            PIC 9(5).
014680      05 ˜A—¿˜JŽ©|¬Œv‚USk                 PIC 9(6).
014690      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚USk           PIC 9(3).
014700      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚USk           PIC 9(6).
014710*/‚U•”ˆÊ–Ú‘½•”ˆÊ
014720      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚U           PIC 9(4).
014730      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚U           PIC 9(2).
014740      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚U             PIC 9(5).
014750*/‚U•”ˆÊ–Ú’·Šú
014760      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚U             PIC 9(4).
014770      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚U             PIC 9(2).
014780      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚U               PIC 9(5).
014790*/‚U•”ˆÊ–Ú‘½•”ˆÊSk
014800      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚U       PIC 9(4).
014810      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚U       PIC 9(2).
014820      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚U         PIC 9(5).
014830**********
014840* ‚V•”ˆÊ *
014850**********
014860   03 ˜A—¿˜JŽ©|•”ˆÊ‚V.
014870      05 ˜A—¿˜JŽ©|Œã—Ã‚V.
014880         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚V          PIC 9(4).
014890         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚V          PIC 9(2).
014900         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚V            PIC 9(5).
014910      05 ˜A—¿˜JŽ©|—âãª–@‚V.
014920         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚V        PIC 9(4).
014930         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚V        PIC 9(2).
014940         07 ˜A—¿˜JŽ©|—âãª–@—¿‚V          PIC 9(4).
014950      05 ˜A—¿˜JŽ©|‰·ãª–@‚V.
014960         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚V        PIC 9(4).
014970         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚V        PIC 9(2).
014980         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚V          PIC 9(4).
014990      05 ˜A—¿˜JŽ©|“d—Ã‚V.
015000         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚V          PIC 9(4).
015010         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚V          PIC 9(2).
015020         07 ˜A—¿˜JŽ©|“d—Ã—¿‚V            PIC 9(5).
015030      05 ˜A—¿˜JŽ©|¬Œv‚V                 PIC 9(6).
015040      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚V           PIC 9(3).
015050      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚V           PIC 9(6).
015060**************
015070* ‚V•”ˆÊSk *
015080**************
015090   03 ˜A—¿˜JŽ©|•”ˆÊ‚VSk.
015100      05 ˜A—¿˜JŽ©|Œã—Ã‚VSk.
015110         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚VSk          PIC 9(4).
015120         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚VSk          PIC 9(2).
015130         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚VSk            PIC 9(5).
015140      05 ˜A—¿˜JŽ©|—âãª–@‚VSk.
015150         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚VSk        PIC 9(4).
015160         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚VSk        PIC 9(2).
015170         07 ˜A—¿˜JŽ©|—âãª–@—¿‚VSk          PIC 9(4).
015180      05 ˜A—¿˜JŽ©|‰·ãª–@‚VSk.
015190         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚VSk        PIC 9(4).
015200         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚VSk        PIC 9(2).
015210         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚VSk          PIC 9(4).
015220      05 ˜A—¿˜JŽ©|“d—Ã‚VSk.
015230         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚VSk          PIC 9(4).
015240         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚VSk          PIC 9(2).
015250         07 ˜A—¿˜JŽ©|“d—Ã—¿‚VSk            PIC 9(5).
015260      05 ˜A—¿˜JŽ©|¬Œv‚VSk                 PIC 9(6).
015270      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚VSk           PIC 9(3).
015280      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚VSk           PIC 9(6).
015290*/‚V•”ˆÊ–Ú‘½•”ˆÊ
015300      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚V           PIC 9(4).
015310      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚V           PIC 9(2).
015320      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚V             PIC 9(5).
015330*/‚V•”ˆÊ–Ú’·Šú
015340      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚V             PIC 9(4).
015350      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚V             PIC 9(2).
015360      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚V               PIC 9(5).
015370*/‚V•”ˆÊ–Ú‘½•”ˆÊSk
015380      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚V       PIC 9(4).
015390      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚V       PIC 9(2).
015400      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚V         PIC 9(5).
015410**********
015420* ‚W•”ˆÊ *
015430**********
015440   03 ˜A—¿˜JŽ©|•”ˆÊ‚W.
015450      05 ˜A—¿˜JŽ©|Œã—Ã‚W.
015460         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚W          PIC 9(4).
015470         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚W          PIC 9(2).
015480         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚W            PIC 9(5).
015490      05 ˜A—¿˜JŽ©|—âãª–@‚W.
015500         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚W        PIC 9(4).
015510         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚W        PIC 9(2).
015520         07 ˜A—¿˜JŽ©|—âãª–@—¿‚W          PIC 9(4).
015530      05 ˜A—¿˜JŽ©|‰·ãª–@‚W.
015540         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚W        PIC 9(4).
015550         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚W        PIC 9(2).
015560         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚W          PIC 9(4).
015570      05 ˜A—¿˜JŽ©|“d—Ã‚W.
015580         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚W          PIC 9(4).
015590         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚W          PIC 9(2).
015600         07 ˜A—¿˜JŽ©|“d—Ã—¿‚W            PIC 9(5).
015610      05 ˜A—¿˜JŽ©|¬Œv‚W                 PIC 9(6).
015620      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚W           PIC 9(3).
015630      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚W           PIC 9(6).
015640**************
015650* ‚W•”ˆÊSk *
015660**************
015670   03 ˜A—¿˜JŽ©|•”ˆÊ‚WSk.
015680      05 ˜A—¿˜JŽ©|Œã—Ã‚WSk.
015690         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚WSk          PIC 9(4).
015700         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚WSk          PIC 9(2).
015710         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚WSk            PIC 9(5).
015720      05 ˜A—¿˜JŽ©|—âãª–@‚WSk.
015730         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚WSk        PIC 9(4).
015740         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚WSk        PIC 9(2).
015750         07 ˜A—¿˜JŽ©|—âãª–@—¿‚WSk          PIC 9(4).
015760      05 ˜A—¿˜JŽ©|‰·ãª–@‚WSk.
015770         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚WSk        PIC 9(4).
015780         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚WSk        PIC 9(2).
015790         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚WSk          PIC 9(4).
015800      05 ˜A—¿˜JŽ©|“d—Ã‚WSk.
015810         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚WSk          PIC 9(4).
015820         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚WSk          PIC 9(2).
015830         07 ˜A—¿˜JŽ©|“d—Ã—¿‚WSk            PIC 9(5).
015840      05 ˜A—¿˜JŽ©|¬Œv‚WSk                 PIC 9(6).
015850      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚WSk           PIC 9(3).
015860      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚WSk           PIC 9(6).
015870*/‚W•”ˆÊ–Ú‘½•”ˆÊ
015880      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚W           PIC 9(4).
015890      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚W           PIC 9(2).
015900      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚W             PIC 9(5).
015910*/‚W•”ˆÊ–Ú’·Šú
015920      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚W             PIC 9(4).
015930      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚W             PIC 9(2).
015940      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚W               PIC 9(5).
015950*/‚W•”ˆÊ–Ú‘½•”ˆÊSk
015960      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚W       PIC 9(4).
015970      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚W       PIC 9(2).
015980      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚W         PIC 9(5).
015990**********
016000* ‚X•”ˆÊ *
016010**********
016020   03 ˜A—¿˜JŽ©|•”ˆÊ‚X.
016030      05 ˜A—¿˜JŽ©|Œã—Ã‚X.
016040         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚X          PIC 9(4).
016050         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚X          PIC 9(2).
016060         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚X            PIC 9(5).
016070      05 ˜A—¿˜JŽ©|—âãª–@‚X.
016080         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚X        PIC 9(4).
016090         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚X        PIC 9(2).
016100         07 ˜A—¿˜JŽ©|—âãª–@—¿‚X          PIC 9(4).
016110      05 ˜A—¿˜JŽ©|‰·ãª–@‚X.
016120         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚X        PIC 9(4).
016130         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚X        PIC 9(2).
016140         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚X          PIC 9(4).
016150      05 ˜A—¿˜JŽ©|“d—Ã‚X.
016160         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚X          PIC 9(4).
016170         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚X          PIC 9(2).
016180         07 ˜A—¿˜JŽ©|“d—Ã—¿‚X            PIC 9(5).
016190      05 ˜A—¿˜JŽ©|¬Œv‚X                 PIC 9(6).
016200      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚X           PIC 9(3).
016210      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚X           PIC 9(6).
016220**************
016230* ‚X•”ˆÊSk *
016240**************
016250   03 ˜A—¿˜JŽ©|•”ˆÊ‚XSk.
016260      05 ˜A—¿˜JŽ©|Œã—Ã‚XSk.
016270         07 ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚XSk          PIC 9(4).
016280         07 ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚XSk          PIC 9(2).
016290         07 ˜A—¿˜JŽ©|Œã—Ã—¿‚XSk            PIC 9(5).
016300      05 ˜A—¿˜JŽ©|—âãª–@‚XSk.
016310         07 ˜A—¿˜JŽ©|—âãª–@’P‰¿‚XSk        PIC 9(4).
016320         07 ˜A—¿˜JŽ©|—âãª–@‰ñ”‚XSk        PIC 9(2).
016330         07 ˜A—¿˜JŽ©|—âãª–@—¿‚XSk          PIC 9(4).
016340      05 ˜A—¿˜JŽ©|‰·ãª–@‚XSk.
016350         07 ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚XSk        PIC 9(4).
016360         07 ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚XSk        PIC 9(2).
016370         07 ˜A—¿˜JŽ©|‰·ãª–@—¿‚XSk          PIC 9(4).
016380      05 ˜A—¿˜JŽ©|“d—Ã‚XSk.
016390         07 ˜A—¿˜JŽ©|“d—Ã’P‰¿‚XSk          PIC 9(4).
016400         07 ˜A—¿˜JŽ©|“d—Ã‰ñ”‚XSk          PIC 9(2).
016410         07 ˜A—¿˜JŽ©|“d—Ã—¿‚XSk            PIC 9(5).
016420      05 ˜A—¿˜JŽ©|¬Œv‚XSk                 PIC 9(6).
016430      05 ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚XSk           PIC 9(3).
016440      05 ˜A—¿˜JŽ©|’·Šúž¬Œv‚XSk           PIC 9(6).
016450*/‚X•”ˆÊ–Ú‘½•”ˆÊ
016460      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚X           PIC 9(4).
016470      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚X           PIC 9(2).
016480      05 ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚X             PIC 9(5).
016490*/‚X•”ˆÊ–Ú’·Šú
016500      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚X             PIC 9(4).
016510      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚X             PIC 9(2).
016520      05 ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚X               PIC 9(5).
016530*/‚X•”ˆÊ–Ú‘½•”ˆÊSk
016540      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚X       PIC 9(4).
016550      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚X       PIC 9(2).
016560      05 ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚X         PIC 9(5).
016570*/—âãª–@‚Æ‰·ãª–@‚Ì’P‰¿‚ªˆá‚¤ê‡‚Ì–¾×‘AŒvŽZ‘‚Ì‹L“ü‚Ìˆ×‚Ì‘Î‰žB/061024
016580 01 ˜A—¿˜JŽ©‚Q|—¿‹à‚S IS EXTERNAL.
016590    03 ˜A—¿˜JŽ©‚Q|ãª–@’P‰¿‰ñ”               OCCURS 9.
016600       05 ˜A—¿˜JŽ©‚Q|ãª–@’P‰¿                PIC 9(5) OCCURS 6.
016610       05 ˜A—¿˜JŽ©‚Q|ãª–@‰ñ”                PIC 9(2) OCCURS 6.
016620    03 ˜A—¿˜JŽ©‚Q|“d—Ã’P‰¿‰ñ”               OCCURS 9.
016630       05 ˜A—¿˜JŽ©‚Q|“d—Ã’P‰¿                PIC 9(5) OCCURS 2.
016640       05 ˜A—¿˜JŽ©‚Q|“d—Ã‰ñ”                PIC 9(2) OCCURS 2.
016650*‰ŒŸŽž‘Š’k—¿/080829
016660 01 ˜A—¿˜JŽ©|‰ŒŸŽž‘Š’k—¿ IS EXTERNAL        PIC 9(4).
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚Í‚R‰ñŽZ’è‚Å‚«‚éB/20181015
       01 ˜A—¿˜JŽ©|“Á•ÊÞ—¿”ï IS EXTERNAL.
          03 ˜A—¿˜JŽ©|Þ—¿”ï’P‰¿                   PIC 9(5) OCCURS 7.
          03 ˜A—¿˜JŽ©|Þ—¿”ï‰ñ”                   PIC 9(1) OCCURS 7.
016670*
016680 01 ˜A|‹¤’ÊƒL[ IS EXTERNAL.
016690    03  ˜A|ƒJƒŒƒ“ƒg˜a—ï               PIC 9.
016700*
016710 01 ˜AŒv|ƒL[ IS EXTERNAL.
016720    03  ˜AŒv|•‰’S‹àŒvŽZ‹æ•ª           PIC 9.
016730    03  ˜AŒv|–{l‰Æ‘°‹æ•ª             PIC 9.
016740    03  ˜AŒv|•ÛŒ¯Ží•Ê                 PIC 9(2).
016750    03  ˜AŒv|•ÛŒ¯ŽÒ”Ô†               PIC X(10).
016760    03  ˜AŒv|Œö”ïŽí•Ê                 PIC 9(2).
016770    03  ˜AŒv|Žs’¬‘º”Ô†˜Vl           PIC X(10).
016780    03  ˜AŒv|•¬Ží•Ê                 PIC 9(2).
016790    03  ˜AŒv|Žs’¬‘º”Ô†•¬           PIC X(10).
016800    03  ˜AŒv|Š³ŽÒƒR[ƒh.
016810      05  ˜AŒv|Š³ŽÒ”Ô†               PIC 9(6).
016820      05  ˜AŒv|Ž}”Ô                   PIC X.
016830    03  ˜AŒv|Ž{p˜a—ï”NŒŽ“ú.
016840      05  ˜AŒv|Ž{p˜a—ï”NŒŽ.
016850        07  ˜AŒv|Ž{p˜a—ï             PIC 9.
016860        07  ˜AŒv|Ž{p”NŒŽ.
016870          09  ˜AŒv|Ž{p”N             PIC 9(2).
016880          09  ˜AŒv|Ž{pŒŽ             PIC 9(2).
016890      05  ˜AŒv|Ž{p“ú                 PIC 9(2).
016900    03  ˜AŒv|”ï—pŠz                   PIC 9(6).
016910    03  ˜AŒv|•‰’SŠz                   PIC 9(6).
016920    03  ˜AŒv|¿‹Šz                   PIC 9(6).
016930    03  ˜AŒv|”ï—pŠz˜Vl               PIC 9(6).
016940    03  ˜AŒv|•‰’SŠz˜Vl               PIC 9(6).
016950    03  ˜AŒv|¿‹Šz˜Vl               PIC 9(6).
016960    03  ˜AŒv|”ï—pŠz•¬               PIC 9(6).
016970    03  ˜AŒv|•‰’SŠz•¬               PIC 9(5).
016980    03  ˜AŒv|¿‹Šz•¬               PIC 9(5).
016990    03  ˜AŒv|ŽÀ“ú”                   PIC 9(2).
017000    03  ˜AŒv|•‰’S—¦                   PIC 9(3).
017010    03  ˜AŒv|•‰’S—¦‹æ•ª               PIC 9.
017020*
017030 01 ˜AŒv‚o|ƒL[ IS EXTERNAL.
017040    03 ˜AŒv‚o|”ï—pŠz‚o‚f              PIC X(8).
017050    03 ˜AŒv‚o|•‰’SŠz‚o‚f              PIC X(8).
017060    03 ˜AŒv‚o|•‰’SŠz‚o‚f˜Vl          PIC X(8).
017070    03 ˜AŒv‚o|•‰’SŠz‚o‚f•¬          PIC X(8).
017080*
017090 01 ˜A‚n|ƒL[ IS EXTERNAL.
017100    03 ˜A‚n|•‰’S—¦                    PIC 9(2).
017110*
017120 01 ˜AƒŒ|ƒL[ IS EXTERNAL.
017130    03 ˜AƒŒ|•ÛŒ¯Ží•Ê                  PIC 9(2).
017140*
017150 01 ˜Aƒ|ƒL[ IS EXTERNAL.
017160    03  ˜Aƒ|ƒƒbƒZ[ƒW               PIC N(20).
017170*
017180 01 ˜Aƒ‚R|ƒL[ IS EXTERNAL.
017190    03  ˜Aƒ‚R|ƒƒbƒZ[ƒW             PIC N(20).
017200    03  ˜Aƒ‚R|ƒƒbƒZ[ƒW‚P           PIC X(20).
017210*
017220* Šm”FƒƒbƒZ[ƒW—p (‚Qs)
017230 01 ˜Aƒ‚V|ƒL[ IS EXTERNAL.
017240    03  ˜Aƒ‚V|ƒƒbƒZ[ƒW‚P             PIC X(40).
017250    03  ˜Aƒ‚V|ƒƒbƒZ[ƒW‚Q             PIC X(40).
017260******************************************************************
017270*                      PROCEDURE  DIVISION                       *
017280******************************************************************
017290 PROCEDURE               DIVISION.
017300************
017310*           *
017320* ‰Šúˆ—   *
017330*           *
017340************
017350*
017360* ˆÈ‰º‚æ‚è‚o‚fŠJŽn
017370     PERFORM ‰Šú‰».
017380     PERFORM —ÝŒv€–Ú‰Šú‰».
017390     PERFORM Šî–{—¿‹àŽæ“¾.
017400     PERFORM ŒvŽZî•ñŽæ“¾.
017410*/˜JÐ‚Í•‰’S‹à‚Í–³‚µ
017420*     PERFORM ŒvŽZ‚o‚fŽæ“¾.
017430     PERFORM §Œäî•ñŽæ“¾.
017440*/˜JÐ‚É’·Šú’üŒ¸‚Í–³‚µ
017450*     PERFORM ’·Šú’üŒ¸‹æ•ªŽæ“¾.
017460*
017470************
017480*           *
017490* Žåˆ—     *
017500*           *
017510************
017520*
      */•‰“o˜^‚ª–³‚¢ê‡ŒvŽZ‚µ‚È‚¢/120713
           PERFORM •‰ƒf[ƒ^Žæ“¾
           IF •”ˆÊ”‚v NOT = ZERO
017530         MOVE ˜AŒv|Ž{p˜a—ï TO Ž{‹L|Ž{p˜a—ï Ž{p˜a—ï‚b‚v
017540         MOVE ˜AŒv|Ž{p”N   TO Ž{‹L|Ž{p”N   Ž{p”N‚b‚v
017550         MOVE ˜AŒv|Ž{pŒŽ   TO Ž{‹L|Ž{pŒŽ   Ž{pŒŽ‚b‚v
017560         MOVE 1              TO Ž{‹L|Ž{p“ú   Ž{p“ú‚b‚v
017570         MOVE ˜AŒv|Š³ŽÒ”Ô† TO Ž{‹L|Š³ŽÒ”Ô† Š³ŽÒƒR[ƒh‚b‚v
017580         MOVE ˜AŒv|Ž}”Ô     TO Ž{‹L|Ž}”Ô     Ž}”Ô‚b‚v
017590*
017600         START Ž{p‹L˜^‚e KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
017610         END-START
017620*
017630         IF ó‘ÔƒL[ = "00"
017640*
017650             PERFORM “–ŒŽ‰ïŒvƒtƒ@ƒCƒ‹íœ
017680             MOVE SPACE TO I—¹ƒtƒ‰ƒO
017690             PERFORM Ž{p‹L˜^‚e“Çž
017700             PERFORM Ž{p“úƒZƒbƒg
017710*
017720             PERFORM UNTIL ( Ž{‹L|Š³ŽÒƒR[ƒh   NOT = Š³ŽÒƒR[ƒh‚b‚v   ) OR
017730                           ( Ž{‹L|Ž{p˜a—ï”NŒŽ NOT = Ž{p˜a—ï”NŒŽ‚b‚v ) OR
017740                           ( I—¹ƒtƒ‰ƒO         NOT = SPACE )
017750*
017760*                 PERFORM “úŒv€–Ú‰Šú‰»
017770*
017780                 PERFORM ‰ŒŸ—¿ŽZ’è
017790                 PERFORM ÄŒŸ—¿ŽZ’è
017800                 PERFORM ‰—Ã—¿ŽZ’è
017810                 PERFORM Œã—Ã—¿ŽZ’è
017820                 PERFORM ‚»‚Ì‘¼ŽZ’è
017830*
017840*/˜JÐ‚Ìê‡‚É•K—v‚È—¿‹à
017850                 PERFORM ‰^“®—Ã–@—¿ŽZ’è
017860                 PERFORM Žw“±ŠÇ——¿ŽZ’è
017870                 PERFORM h”‘—¿ŽZ’è
017880                 PERFORM HŽ–—¿ŽZ’è
017890                 PERFORM ‹x‹ÆØ–¾‘—¿ŽZ’è
017900* •”ˆÊŠÖ˜AˆÈŠO‚Ì€–Ú‚ð—ÝŒv‚µ‚Ä‚¨‚­B
017910                 PERFORM —ÝŒvˆ—
017920*
017940*********
017950                 PERFORM ‰ïŒvƒf[ƒ^‚e‘ž
017960*********
017990                 PERFORM Ž{p‹L˜^‚e“Çž
018000                 PERFORM Ž{p“úƒZƒbƒg
018010*
018020             END-PERFORM
018030         END-IF
018040*
018050*****************************************************************
018060* ‘½•”ˆÊŠÖ˜A€–Ú‚Ì•”ˆÊ–ˆ‚P‚O‚O“‚Ì‡Œv‚©‚ç’üŒ¸—¦A’·Šú’üŒ¸—¦‚ðæ‚¶‚½
018070* ‹àŠz‚ð•”ˆÊ–ˆ‚ÉŽZo‚·‚éB
018080*****************************************************************
018090*
018100         PERFORM ‘½•”ˆÊ‡Œvˆ—
018110*
018120* Še•”ˆÊ–ˆ‚Ìi‚P‚O‚O“‚Ì‹àŠz‚Éj’üŒ¸—¦i‘½•”ˆÊE’·Šúj‚ð
018130* æ‚¯‚½‹àŠz‚Æ•”ˆÊˆÈŠO€–Ú‚ð‰ÁŽZ‚·‚éB
018140*
018150         COMPUTE ˜AŒv|”ï—pŠz
018160               = •”ˆÊ‡Œv‚v‚P   + •”ˆÊ‡Œv‚v‚Q     + •”ˆÊ‡Œv‚v‚R +
018170                 •”ˆÊ‡Œv‚v‚S   + •”ˆÊ‡Œv‚v‚T     + •”ˆÊ‡Œv‚v‚U +
018180                 •”ˆÊ‡Œv‚v‚V   + •”ˆÊ‡Œv‚v‚W     + •”ˆÊ‡Œv‚v‚X +
018190                 ‰ŒŸ—¿‚f‚v     + ‰ŒŸ‰ÁŽZ—¿‚f‚v   +
018200                 ÄŒŸ—¿‚f‚v     + ‰—Ã—¿‚f‚v       + ‰—Ã‰ÁŽZ—¿‚f‚v +
018210                 ‹à‘®•›Žq—¿‚f‚v + ‰‰ñˆ’u—¿‚f‚v   + Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v + ‰ŒŸŽž‘Š’k—¿‚f‚v +
018220*/˜JÐ•ÛŒ¯—p
018230                 Žw“±ŠÇ——¿‚f‚v + “Á•ÊÞ—¿”ï‚f‚v + ŒðŠ·•ï‘Ñ—¿‚f‚v +
018240                 ‰^“®—Ã–@—¿‚f‚v + h”‘—¿‚f‚v     + HŽ–—¿‚f‚v     +
018250                 ‹x‹ÆØ–¾‘—¿‚f‚v
018260*
018270         EVALUATE ˜AƒŒ|•ÛŒ¯Ží•Ê
018280*
018290         WHEN 70
018300             MOVE    ˜AŒv|”ï—pŠz   TO ˜A—¿˜JŽ©|”ï—pŠz
018310             MOVE    ˜AŒv|•‰’SŠz   TO ˜A—¿˜JŽ©|ˆê•”•‰’S‹à
018320             COMPUTE ˜A—¿˜JŽ©|¿‹Šz =  ˜AŒv|”ï—pŠz - ˜AŒv|•‰’SŠz
018330             COMPUTE ˜AŒv|¿‹Šz     =  ˜AŒv|”ï—pŠz - ˜AŒv|•‰’SŠz
018340*
018350         WHEN OTHER
018360             CONTINUE
018370         END-EVALUATE
018380*
018390         PERFORM ˜AŒ‹€–ÚƒZƒbƒg
018400     
018410*/ƒŒƒZƒvƒgƒtƒ@ƒCƒ‹ì¬
018420         PERFORM ƒŒƒZƒvƒgƒtƒ@ƒCƒ‹ì¬
018430
018440* ‘S_‚Ì‚Ý‚Ì‹@”\i‚n‚b‚qƒR[ƒh‚Ìˆê•”j
018450*     PERFORM •‰’S—¦‚n‚b‚qƒZƒbƒg
018460*
      */•‰“o˜^‚ª–³‚¢ê‡ŒvŽZ‚µ‚È‚¢/120713
      */•‰•”ˆÊ‚ª‚È‚¢ê‡‚Íƒtƒ@ƒCƒ‹‚ðƒNƒŠƒA‚·‚é/131206
           ELSE
017650         PERFORM “–ŒŽ‰ïŒvƒtƒ@ƒCƒ‹íœ
018390         PERFORM ˜AŒ‹€–ÚƒZƒbƒg
018420         PERFORM ƒŒƒZƒvƒgƒtƒ@ƒCƒ‹ì¬
           END-IF.
018470************
018480*           *
018490* I—¹ˆ—   *
018500*           *
018510************
018520*
018530     PERFORM I—¹ˆ—.
018540     MOVE 1 TO PROGRAM-STATUS.
018550     EXIT PROGRAM.
018560*
018570*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
018580*================================================================*
018590 ‰Šú‰» SECTION.
018600*
018610     OPEN INPUT Ž{p‹L˜^‚e.
018620         MOVE NC"Ž{‹L" TO ƒtƒ@ƒCƒ‹–¼‚v.
018630         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018640     OPEN INPUT —¿‹àƒ}ƒXƒ^.
018650         MOVE NC"—¿‹à" TO ƒtƒ@ƒCƒ‹–¼‚v.
018660         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018670     OPEN INPUT ŒvŽZƒ}ƒXƒ^.
018680         MOVE NC"ŒvŽZ" TO ƒtƒ@ƒCƒ‹–¼‚v.
018690         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018700*     OPEN INPUT ŒvŽZ‚o‚fƒ}ƒXƒ^.
018710*         MOVE NC"Œv‚o" TO ƒtƒ@ƒCƒ‹–¼‚v.
018720*         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018730*     OPEN INPUT ŽófŽÒî•ñ‚e.
018740     OPEN I-O ŽófŽÒî•ñ‚e.
018750         MOVE NC"Žóf" TO ƒtƒ@ƒCƒ‹–¼‚v.
018760         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018770     OPEN INPUT •‰ƒf[ƒ^‚e.
018780         MOVE NC"•‰" TO ƒtƒ@ƒCƒ‹–¼‚v.
018790         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018800     OPEN INPUT §Œäî•ñƒ}ƒXƒ^.
018810         MOVE NC"§Œä" TO ƒtƒ@ƒCƒ‹–¼‚v.
018820         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018830     OPEN INPUT Œ³†ƒ}ƒXƒ^.
018840         MOVE NC"Œ³†" TO ƒtƒ@ƒCƒ‹–¼‚v.
018850         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018860     OPEN INPUT –¼Ìƒ}ƒXƒ^.
018870         MOVE NC"–¼Ì" TO ƒtƒ@ƒCƒ‹–¼‚v.
018880         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018890********
018900     OPEN INPUT ‰ïŒv—ÌŽû‚e.
018910         MOVE NC"‰ïŒv—ÌŽû" TO ƒtƒ@ƒCƒ‹–¼‚v.
018920         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018930     OPEN I-O ‰ïŒvƒf[ƒ^‚e.
018940         MOVE NC"‰ïŒv" TO ƒtƒ@ƒCƒ‹–¼‚v.
018950         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018960     OPEN I-O   ƒŒƒZƒvƒg‚e.
018970         MOVE NC"ƒŒƒZ" TO ƒtƒ@ƒCƒ‹–¼‚v.
018980         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018990     OPEN I-O   ƒŒƒZƒvƒg˜JŽ©‚e.
019000         MOVE NC"˜JŽ©" TO ƒtƒ@ƒCƒ‹–¼‚v.
019010         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
018610     OPEN I-O XVŽ{p‹L˜^‚e.
018620         MOVE NC"XŽ{" TO ƒtƒ@ƒCƒ‹–¼‚v.
018630         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
019020********
019030*
019040     MOVE ZERO TO ˜AŒv|”ï—pŠz ˜AŒv|•‰’SŠz ˜AŒv|¿‹Šz
019050                  ˜AŒv|”ï—pŠz˜Vl ˜AŒv|•‰’SŠz˜Vl ˜AŒv|¿‹Šz˜Vl
019060                  ˜AŒv|”ï—pŠz•¬ ˜AŒv|•‰’SŠz•¬ ˜AŒv|¿‹Šz•¬.
019070*
019080     MOVE SPACE TO ˜A—¿˜JŽ©|—¿‹à‚P.
019090     MOVE SPACE TO ˜A—¿˜JŽ©|—¿‹à‚Q.
019100     MOVE SPACE TO ˜A—¿˜JŽ©|—¿‹à‚R.
019110     MOVE ZERO  TO ˜A—¿˜JŽ©|‰ŒŸŽž‘Š’k—¿ ˜A—¿˜JŽ©|“Á•ÊÞ—¿”ï.
019120     INITIALIZE    ˜A—¿˜JŽ©|—¿‹à‚P.
019130     INITIALIZE    ˜A—¿˜JŽ©|—¿‹à‚Q.
019140     INITIALIZE    ˜A—¿˜JŽ©|—¿‹à‚R.
019150     INITIALIZE    ˜A—¿˜JŽ©|‰ŒŸŽž‘Š’k—¿ ˜A—¿˜JŽ©|“Á•ÊÞ—¿”ï.
019160*
019170     MOVE ˜AŒv|•ÛŒ¯Ží•Ê TO •ÛŒ¯Ží•Ê‚v.
019180     MOVE ˜AŒv|Œö”ïŽí•Ê TO Œö”ïŽí•Ê‚v.
019190     MOVE ˜AŒv|•¬Ží•Ê TO •¬Ží•Ê‚v.
019200*================================================================*
019210 ƒI[ƒvƒ“ƒ`ƒFƒbƒN SECTION.
019220*
019230     IF ó‘ÔƒL[  NOT =  "00"
019240         DISPLAY ƒtƒ@ƒCƒ‹–¼‚v NC"‚eƒI[ƒvƒ“ƒGƒ‰[" UPON CONS
019250         DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[           UPON CONS
019260         DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢" UPON CONS
019270*-----------------------------------------*
019280         CALL "actcshm"  WITH C LINKAGE
019290*-----------------------------------------*
019300         ACCEPT  ƒL[“ü—Í FROM CONS
019310         PERFORM ƒtƒ@ƒCƒ‹•Â½
019320         MOVE ZERO TO PROGRAM-STATUS
019330         EXIT PROGRAM.
019340*================================================================*
019350 ƒtƒ@ƒCƒ‹•Â½ SECTION.
019360*
019370     CLOSE Ž{p‹L˜^‚e —¿‹àƒ}ƒXƒ^ ŒvŽZƒ}ƒXƒ^ 
019380           ŽófŽÒî•ñ‚e •‰ƒf[ƒ^‚e §Œäî•ñƒ}ƒXƒ^ –¼Ìƒ}ƒXƒ^
019390           Œ³†ƒ}ƒXƒ^.
019400*     ŒvŽZ‚o‚fƒ}ƒXƒ^
019410****
019420     CLOSE ‰ïŒv—ÌŽû‚e ‰ïŒvƒf[ƒ^‚e ƒŒƒZƒvƒg‚e ƒŒƒZƒvƒg˜JŽ©‚e XVŽ{p‹L˜^‚e.
019430*================================================================*
019440 I—¹ˆ— SECTION.
019450*
019460     PERFORM ƒtƒ@ƒCƒ‹•Â½.
019470*================================================================*
019480 ƒGƒ‰[•\Ž¦ SECTION.
019490*
019500     DISPLAY ƒtƒ@ƒCƒ‹–¼‚v NC"ƒtƒ@ƒCƒ‹‘žƒGƒ‰["   UPON CONS.
019510     DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[               UPON CONS.
019520     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
019530     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢" UPON CONS.
019540*-----------------------------------------*
019550         CALL "actcshm"  WITH C LINKAGE
019560*-----------------------------------------*
019570     ACCEPT  ƒL[“ü—Í FROM CONS.
019580     PERFORM ƒtƒ@ƒCƒ‹•Â½.
019590     MOVE ZERO TO PROGRAM-STATUS
019600     EXIT PROGRAM.
019610*================================================================*
019620 ƒGƒ‰[•\Ž¦‚q SECTION.
019630*
019640     DISPLAY ƒtƒ@ƒCƒ‹–¼‚v NC"ƒtƒ@ƒCƒ‹“ÇžƒGƒ‰["   UPON CONS.
019650     DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[               UPON CONS.
019660     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
019670     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢" UPON CONS.
019680*-----------------------------------------*
019690         CALL "actcshm"  WITH C LINKAGE
019700*-----------------------------------------*
019710     ACCEPT  ƒL[“ü—Í FROM CONS.
019720     PERFORM ƒtƒ@ƒCƒ‹•Â½.
019730     MOVE ZERO TO PROGRAM-STATUS
019740     EXIT PROGRAM.
019750*================================================================*
019760 ƒGƒ‰[•\Ž¦‚»‚Ì‘¼ SECTION.
019770*
019780     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
019790     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢" UPON CONS.
019800*-----------------------------------------*
019810         CALL "actcshm"  WITH C LINKAGE
019820*-----------------------------------------*
019830     ACCEPT  ƒL[“ü—Í FROM CONS.
019840     PERFORM ƒtƒ@ƒCƒ‹•Â½.
019850     EXIT PROGRAM.
019860*================================================================*
019870 Ž{p‹L˜^‚e“Çž SECTION.
019880*
019890     READ Ž{p‹L˜^‚e NEXT
019900     AT END
019910         MOVE "YES" TO I—¹ƒtƒ‰ƒO
019920     END-READ.
019930*
019940*================================================================*
019950 ‰ŒŸ—¿ŽZ’è SECTION.
019960*
019970     MOVE ZERO TO ‰ŒŸ—¿‚v.
019980     MOVE ZERO TO ‰ŒŸ‰ÁŽZ—¿‚v.
019990     MOVE ZERO TO ‰ŒŸŽž‘Š’k—¿‚v.
019330*/“ú–ˆ€–Ú‚Ì‰Šú‰»/
019340     MOVE ZERO TO  ‰ŒŸŽžŠÔŠO‚m‚v ‰ŒŸ‹x“ú‚m‚v ‰ŒŸ[–é‚m‚v.
020000*
020010     IF ( Ž{‹L|f—Ã‹æ•ª     = 2 ) AND
020020        ( ‰ŒŸ¿‹‹æ•ª‚v NOT = 1 ) AND
020030        ( Ž{‹L|‰ŒŸ—¿¿‹‹æ•ª  = 1 )
020040         COMPUTE ‰ŒŸ—¿‚v   = ‰ŒŸ—¿‚v   + ‰ŒŸ—¿‚b‚v
020050         COMPUTE ‰ŒŸ‰ñ”‚v = ‰ŒŸ‰ñ”‚v + 1
020060* ‰ŒŸ‰ÁŽZ
020070         EVALUATE Ž{‹L|‰ŒŸ‰ÁŽZ
020080         WHEN ZERO
020090             CONTINUE
020100         WHEN 1
020110             MOVE 1 TO ‰ŒŸŽžŠÔŠO‚v ‰ŒŸŽžŠÔŠO‚m‚v
020120             COMPUTE ‰ŒŸ‰ÁŽZ—¿‚v = ‰ŒŸ‰ÁŽZ—¿‚v + ŽžŠÔŠO‚b‚v
020130         WHEN 2
020140             MOVE 1 TO ‰ŒŸ‹x“ú‚v   ‰ŒŸ‹x“ú‚m‚v
020150             COMPUTE ‰ŒŸ‰ÁŽZ—¿‚v = ‰ŒŸ‰ÁŽZ—¿‚v + ‹x“ú‚b‚v
020160         WHEN 3
020170             MOVE 1 TO ‰ŒŸ[–é‚v   ‰ŒŸ[–é‚m‚v
020180             COMPUTE ‰ŒŸ‰ÁŽZ—¿‚v = ‰ŒŸ‰ÁŽZ—¿‚v + [–é‚b‚v
020190         WHEN OTHER
020200             CONTINUE
020210         END-EVALUATE
020220     END-IF.
020230* ‰ŒŸŽž‘Š’k—¿/080829
020240     IF (Ž{‹L|f—Ã‹æ•ª = 2 ) AND (Ž{‹L|‰ŒŸŽž‘Š’k—¿‹æ•ª NOT = 1)
020250         COMPUTE ‰ŒŸŽž‘Š’k—¿‚v = ‰ŒŸŽž‘Š’k—¿‚v + ‰ŒŸŽž‘Š’k—¿‚b‚v
020260     END-IF.
020270*================================================================*
020280 ÄŒŸ—¿ŽZ’è SECTION.
020290*
020300     MOVE ZERO TO ÄŒŸ—¿‚v.
020310*
020320     IF Ž{‹L|ÄŒŸ—¿¿‹  = 1
020330         COMPUTE ÄŒŸ—¿‚v   = ÄŒŸ—¿‚v + ÄŒŸ—¿‚b‚v
020340         COMPUTE ÄŒŸ‰ñ”‚v = ÄŒŸ‰ñ”‚v + 1
020350     END-IF.
020360*
020370*================================================================*
020380 ‰—Ã—¿ŽZ’è SECTION.
020390******************************************************************
020400* ‰—Ã‰ÁŽZ‚ÍA’[”‚ðŠÜ‚ß‚½‹àŠz‚ðŠî–{‚Æ‚µAŒvŽZ‚·‚éB
020410* 100/100‚Ìê‡‚ÍA100‚ð‰ÁŽZ—¿i–éŠÔE–\•—‰Jj‚Ì•ª‚Æ‚µ‚ÄŒvŽZ‚·‚éB
020420******************************************************************
020430*
020440     MOVE ZERO TO ‰—Ã—¿‚v.
020450     MOVE ZERO TO ‰—Ã‰ÁŽZ—¿‚v.
019810*/“ú–ˆ€–Ú‚Ì‰Šú‰»
019820     MOVE ZERO TO ‰—Ã–éŠÔ‚m‚v ‰—Ã“ï˜H‚m‚v ‰—Ã–\•—‰J‚m‚v ‰—ÃŽžŠÔŠO‚m‚v.
020460*
020470     IF Ž{‹L|‰—Ã‹——£ NOT = ZERO
020480* ®”‚É‚·‚é‚½‚ßA‚P‚O‚ðŠ|‚¯‚é
020490         MOVE Ž{‹L|‰—Ã‹——£ TO ‰—Ã‹——£‚v
020500*/18”N8ŒŽ‚æ‚è8km‚ð’´‚¦‚é‰—Ã—¿‚Í“¯Šz«««/060801
020510         IF (˜AŒv|Ž{p˜a—ï”NŒŽ >= 41808) AND (Ž{‹L|‰—Ã‹——£ >= 6.1)
020520             COMPUTE ‰—Ã”íœ”‚v = 6.1 * 10
020530         ELSE
020540             COMPUTE ‰—Ã”íœ”‚v = Ž{‹L|‰—Ã‹——£ * 10
020550         END-IF
020560*/18”N8ŒŽ‚æ‚è8km‚ð’´‚¦‚é‰—Ã—¿‚Í“¯Šzªªª«‚PsƒRƒƒ“ƒg/060801
020570*         COMPUTE ‰—Ã”íœ”‚v = Ž{‹L|‰—Ã‹——£ * 10
020580         COMPUTE ‰—Ãœ”‚v   = ‰—ÃŠî–{‹——£‚j‚v * 10
020590         DIVIDE ‰—Ãœ”‚v INTO ‰—Ã”íœ”‚v GIVING ‰—Ã¤‚v
020600                           REMAINDER ‰—Ãè—]‚v
020610         COMPUTE ‰—Ã—¿‚v = ‰—ÃŠî–{—¿‹à‚b‚v +
020620                          ( ‰—Ã’Ç‰Á—¿‹à‚b‚v * ( ‰—Ã¤‚v - 1 ) )
020630         IF ‰—Ãè—]‚v > ZERO
020640             COMPUTE ‰—Ã—¿‚v = ‰—Ã—¿‚v + ‰—Ã’Ç‰Á—¿‹à‚b‚v
020650         END-IF
020660* ‰—Ã‰ÁŽZi–éŠÔE–\•—‰J“™j
020670* ƒŒƒZƒvƒg‚Ìê‡A‹——£‚Ì‰ÁŽZ‚ÍA‰—Ã—¿‚ÉŠÜ‚ßA‰—Ã‰ÁŽZ‚ÍA–éŠÔE–\•—‰J‚Ì
020680* ‰ÁŽZ‚Ì‚Ý‚Æ‚·‚é
020690         IF Ž{‹L|‰—Ã‰ÁŽZ NOT = ZERO
020700             EVALUATE Ž{‹L|‰—Ã‰ÁŽZ
020710             WHEN 4
020720                 MOVE ‰—Ã–éŠÔ‰ÁŽZ—¦‚b‚v   TO ‰—Ã‰ÁŽZ—¦‚b‚v
020730             WHEN 1
020740                 MOVE ‰—Ã[–é‰ÁŽZ—¦‚b‚v   TO ‰—Ã‰ÁŽZ—¦‚b‚v
020750             WHEN 2
020760                 MOVE ‰—Ã“ï˜H‰ÁŽZ—¦‚b‚v   TO ‰—Ã‰ÁŽZ—¦‚b‚v
020770             WHEN 3
020780                 MOVE ‰—Ã–\•—‰J‰ÁŽZ—¦‚b‚v TO ‰—Ã‰ÁŽZ—¦‚b‚v
020790             WHEN OTHER
020800                 DISPLAY NC"‰—Ã‰ÁŽZ‚ª•s³‚Å‚·" UPON CONS
020810             END-EVALUATE
020820             COMPUTE ‰—Ã‰ÁŽZ—¿‚v = ‰—Ã—¿‚v * ( ‰—Ã‰ÁŽZ—¦‚b‚v / 100 )
020830             COMPUTE ‰—Ã‰ÁŽZ—¦‚v     = ‰—Ã‰ÁŽZ—¦‚b‚v - 100
020840             COMPUTE ‰—Ã‰ÁŽZ—¦’†ŠÔ‚v = ‰—Ã‰ÁŽZ—¦‚v / 100
020850             COMPUTE ‰—Ã‰ÁŽZ—¿‚v     = ‰—Ã—¿‚v * ‰—Ã‰ÁŽZ—¦’†ŠÔ‚v
020860*
020870             EVALUATE Ž{‹L|‰—Ã‰ÁŽZ
020880             WHEN 1
020890                 MOVE 1 TO ‰—Ã–éŠÔ‚v   ‰—Ã–éŠÔ‚m‚v
020900             WHEN 2
020910                 MOVE 1 TO ‰—Ã“ï˜H‚v   ‰—Ã“ï˜H‚m‚v
020920             WHEN 3
020930                 MOVE 1 TO ‰—Ã–\•—‰J‚v ‰—Ã–\•—‰J‚m‚v
020940             WHEN 4
020950                 MOVE 1 TO ‰—ÃŽžŠÔŠO‚v ‰—ÃŽžŠÔŠO‚m‚v
020960             WHEN OTHER
020970                 CONTINUE
020980             END-EVALUATE
020990*
021000         END-IF
021010*
021020         COMPUTE ‰—Ã‰ñ”‚f‚v = ‰—Ã‰ñ”‚f‚v + 1
021030*
021040         MOVE ‰—Ã—¿‚v TO ‰—Ã—¿‚v‚Q
021050     END-IF.
021060*
021070*================================================================*
021080 Šî–{—¿‹àŽæ“¾ SECTION.
021090*
021100*Šî–{˜JÐ—¿‹à—p‚Ì‹æ•ªƒR[ƒh‚Í‚O‚R
021110     MOVE 03             TO —¿|‹æ•ªƒR[ƒh.
021120     MOVE ZEROS          TO —¿|•‰Ží•Ê.
021130     MOVE ZEROS          TO —¿|•”ˆÊ.
021140     MOVE ZEROS          TO —¿|¶‰E‹æ•ª.
021150     MOVE ZEROS          TO —¿|•‰ˆÊ’u”Ô†.
021160     MOVE ˜AŒv|Ž{p˜a—ï TO —¿|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
021170     MOVE ˜AŒv|Ž{p”N   TO —¿|ŠJŽn”N   Ž{p”N‚b‚v.
021180     MOVE ˜AŒv|Ž{pŒŽ   TO —¿|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
021190*
021200     START —¿‹àƒ}ƒXƒ^ KEY IS <= —¿|‹æ•ªƒR[ƒh
021210                                —¿|•”ˆÊƒR[ƒh
021220                                —¿|ŠJŽn˜a—ï”NŒŽ
021230                                REVERSED
021240     END-START.
021250*
021260     IF ó‘ÔƒL[ = "00"
021270         READ —¿‹àƒ}ƒXƒ^ NEXT
021280         AT END
021290*/ƒGƒ‰[•\Ž¦‚ÌC³
021300             PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
021310             DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½Šî–{—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
021320                     " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
021330                     " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
021340             PERFORM I—¹ˆ—
021350             MOVE ZERO TO PROGRAM-STATUS
021360             EXIT PROGRAM
021370         NOT AT END
021380*/˜JÐ‚Ì—¿‹à‚©”»’è
021390             IF —¿|‹æ•ªƒR[ƒh NOT = 03
021400*/ƒGƒ‰[•\Ž¦‚ÌC³
021410             PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
021420                 DISPLAY "˜JÐŠî–{—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
021430                         " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
021440                         " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
021450                 PERFORM I—¹ˆ—
021460                 MOVE ZERO TO PROGRAM-STATUS
021470                 EXIT PROGRAM
021480             ELSE
021490*
021500                 IF ( Ž{p˜a—ï”NŒŽ‚b‚v >= —¿‚b|ŠJŽn˜a—ï”NŒŽ ) AND
021510                    ( Ž{p˜a—ï”NŒŽ‚b‚v <= —¿‚b|I—¹˜a—ï”NŒŽ )
021520*/‰ŒŸ—¿ŠÖŒW
021530                     MOVE —¿‚b|‰ŒŸ—¿          TO ‰ŒŸ—¿‚b‚v     
021540                     MOVE —¿‚b|ŽžŠÔŠO          TO ŽžŠÔŠO‚b‚v     
021550                     MOVE —¿‚b|‹x“ú            TO ‹x“ú‚b‚v       
021560                     MOVE —¿‚b|[–é            TO [–é‚b‚v       
021570*/‰—Ã—¿ŠÖŒW
021580                     MOVE —¿‚b|Šî–{—¿‹à        TO ‰—ÃŠî–{—¿‹à‚b‚v       
021590                     MOVE —¿‚b|’Ç‰Á—¿‹à        TO ‰—Ã’Ç‰Á—¿‹à‚b‚v       
021600                     MOVE —¿‚b|–éŠÔ‰ÁŽZ—¦      TO ‰—Ã–éŠÔ‰ÁŽZ—¦‚b‚v
021610                     MOVE —¿‚b|[–é‰ÁŽZ—¦      TO ‰—Ã[–é‰ÁŽZ—¦‚b‚v
021620                     MOVE —¿‚b|“ï˜H‰ÁŽZ—¦      TO ‰—Ã“ï˜H‰ÁŽZ—¦‚b‚v
021630                     MOVE —¿‚b|–\•—‰J‰ÁŽZ—¦    TO ‰—Ã–\•—‰J‰ÁŽZ—¦‚b‚v
021640*
021650                     MOVE —¿‚b|ÄŒŸ—¿          TO ÄŒŸ—¿‚b‚v    
021660                     MOVE —¿‚b|—âãª–@—¿        TO —âãª–@—¿‚b‚v  
021670                     MOVE —¿‚b|‰·ãª–@—¿        TO ‰·ãª–@—¿‚b‚v  
021680                     MOVE —¿‚b|“d—Ã—¿          TO “d—Ã—¿‚b‚v    
021690                     MOVE —¿‚b|Ž{pî•ñ’ñ‹Ÿ—¿  TO Ž{pî•ñ’ñ‹Ÿ—¿‚b‚v
021700                     MOVE —¿‚b|Žw“±ŠÇ——¿      TO Žw“±ŠÇ——¿‚b‚v
021710                     MOVE —¿‚b|‰^“®—Ã–@—¿      TO ‰^“®—Ã–@—¿‚b‚v
021720                     MOVE —¿‚b|h”‘—¿          TO h”‘—¿‚b‚v
021730                     MOVE —¿‚b|HŽ–—¿          TO HŽ–—¿‚b‚v
021740                     MOVE —¿‚b|‹x‹ÆØ–¾‘—¿    TO ‹x‹ÆØ–¾‘—¿‚b‚v
021750                     MOVE —¿‚b|‰ŒŸŽž‘Š’k—¿    TO ‰ŒŸŽž‘Š’k—¿‚b‚v
021760                 ELSE
021770*/ƒGƒ‰[•\Ž¦‚ÌC³
021780                     PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
021790                     DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
021800                             " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
021810                             " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
021820                     PERFORM I—¹ˆ—
021830                     MOVE ZERO TO PROGRAM-STATUS
021840                     EXIT PROGRAM
021850                 END-IF
021860             END-IF
021870         END-READ
021880     END-IF.
021890*
021900*================================================================*
021910 •‰•Ê—¿‹àŽæ“¾ SECTION.
021920*
021930*/œÜA•s‘SœÜ‚Ìê‡•‰“ú‚æ‚è‚Q‚Q–Ú‚©‚çSkŒã—Ã—¿‚ðŽg‚¤
021940     MOVE SPACE TO Skƒtƒ‰ƒO.
021950*
021960     MOVE •‰|•‰˜a—ï(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ŒvŽZ˜a—ï‚v.
021970     MOVE •‰|•‰”N  (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ŒvŽZ”N‚v.
021980     MOVE •‰|•‰ŒŽ  (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ŒvŽZŒŽ‚v.
021990     MOVE •‰|•‰“ú  (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ŒvŽZ“ú‚v.
022000     PERFORM ¼—ï”NŽæ“¾.
022010     PERFORM “ú•tâ‘Î’lŒvŽZ.
022020     MOVE â‘Î’l‚v                 TO •‰â‘Î’l‚v.
022030*
022040     MOVE ˜AŒv|Ž{p˜a—ï           TO ŒvŽZ˜a—ï‚v.
022050     MOVE ˜AŒv|Ž{p”N             TO ŒvŽZ”N‚v.
022060     MOVE ˜AŒv|Ž{pŒŽ             TO ŒvŽZŒŽ‚v.
022070     MOVE ˜AŒv|Ž{p“ú             TO ŒvŽZ“ú‚v.
022080     PERFORM ¼—ï”NŽæ“¾.
022090     PERFORM “ú•tâ‘Î’lŒvŽZ.
022100     MOVE â‘Î’l‚v                 TO Ž{pâ‘Î’l‚v.
022110*
022120     COMPUTE •‰Œo‰ß“ú‚v = Ž{pâ‘Î’l‚v - ( •‰â‘Î’l‚v - 1 ).
022130*/•‰•Ê˜JÐ—¿‹à—p‚Ì‹æ•ªƒR[ƒhi˜JÐ04j
022140     MOVE 04               TO —¿|‹æ•ªƒR[ƒh.
022150     MOVE •‰Ží•Ê‚v       TO —¿|•‰Ží•Ê.
022160     MOVE •”ˆÊ‚v           TO —¿|•”ˆÊ.
022170     MOVE ¶‰E‹æ•ª‚v       TO —¿|¶‰E‹æ•ª.
022180     MOVE •‰ˆÊ’u”Ô†‚v   TO —¿|•‰ˆÊ’u”Ô†.
022190     MOVE ˜AŒv|Ž{p˜a—ï   TO —¿|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
022200     MOVE ˜AŒv|Ž{p”N     TO —¿|ŠJŽn”N   Ž{p”N‚b‚v.
022210     MOVE ˜AŒv|Ž{pŒŽ     TO —¿|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
022220*
022230     START —¿‹àƒ}ƒXƒ^ KEY IS <= —¿|‹æ•ªƒR[ƒh
022240                                —¿|•”ˆÊƒR[ƒh
022250                                —¿|ŠJŽn˜a—ï”NŒŽ
022260                                REVERSED
022270     END-START.
022280*
022290     IF ó‘ÔƒL[ = "00"
022300         READ —¿‹àƒ}ƒXƒ^ NEXT
022310         AT END
022320*/ƒGƒ‰[•\Ž¦‚ÌC³
022330             PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
022340             DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
022350                     " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
022360                     " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
022370             PERFORM I—¹ˆ—
022380             MOVE ZERO TO PROGRAM-STATUS
022390             EXIT PROGRAM
022400         NOT AT END
022410*/•‰•Ê˜JÐ—¿‹à—p‚ÌƒŒƒR[ƒh‚©”»’è
022420             IF ( —¿|‹æ•ªƒR[ƒh    NOT = 04             ) OR
022430                ( —¿|•‰Ží•Ê      NOT = •‰Ží•Ê‚v     ) OR
022440                ( —¿|•”ˆÊ          NOT = •”ˆÊ‚v         ) OR
022450                ( —¿|¶‰E‹æ•ª      NOT = ¶‰E‹æ•ª‚v     ) OR
022460                ( —¿|•‰ˆÊ’u”Ô†  NOT = •‰ˆÊ’u”Ô†‚v )   
022470*
022480*/ƒGƒ‰[•\Ž¦‚ÌC³
022490                 PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
022500                 DISPLAY "˜JÐ—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
022510                         " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
022520                         " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ
022530                         " •”ˆÊƒR[ƒh=" •”ˆÊƒR[ƒh‚v UPON CONS
022540*                 MOVE SPACE                TO ƒGƒ‰[ƒƒbƒZ[ƒW‚v
022550*                 MOVE •”ˆÊƒR[ƒh‚v         TO ƒGƒ‰[•”ˆÊƒR[ƒh‚v
022560*                 MOVE  NC"˜JÐ—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ@•”ˆÊƒR[ƒh"
022570*                                           TO ˜Aƒ‚R|ƒƒbƒZ[ƒW
022580*                 MOVE  ƒGƒ‰[ƒƒbƒZ[ƒW‚v  TO ˜Aƒ‚R|ƒƒbƒZ[ƒW‚P
022590*                 CALL   "MSG003"
022600*                 CANCEL "MSG003"
022610                 PERFORM I—¹ˆ—
022620                 MOVE ZERO TO PROGRAM-STATUS
022630                 EXIT PROGRAM
022640             ELSE
022650                 IF ( Ž{p˜a—ï”NŒŽ‚b‚v >= —¿‚c|ŠJŽn˜a—ï”NŒŽ ) AND
022660                    ( Ž{p˜a—ï”NŒŽ‚b‚v <= —¿‚c|I—¹˜a—ï”NŒŽ )
022670                     MOVE —¿‚c|˜JÐ®•œ—¿     TO ®•œ—¿‚c‚v    
022680                     MOVE —¿‚c|˜JÐSkŒã—Ã—¿ TO SkŒã—Ã—¿‚c‚v
022690                     MOVE —¿‚c|˜JÐ“Á•ÊÞ—¿”ï TO “Á•ÊÞ—¿”ï‚c‚v
022700                     MOVE —¿‚c|˜JÐŒðŠ·•ï‘Ñ—¿ TO ŒðŠ·•ï‘Ñ—¿‚c‚v
022710                     IF ( •‰Œo‰ß“ú‚v >= 22 )      AND
022720                        ( —¿|•‰Ží•Ê = 05 OR 06 ) AND
022730*/˜JÐSkŒã—Ã‹æ•ª/090317
022740                        ( •‰|SkŒã—Ã‹æ•ª = ZERO )
022750                         MOVE —¿‚c|˜JÐSkŒã—Ã—¿ TO Œã—Ã—¿‚c‚v
022760                         MOVE "YES"                TO Skƒtƒ‰ƒO
022770                     ELSE
022780                         MOVE —¿‚c|˜JÐŒã—Ã—¿     TO Œã—Ã—¿‚c‚v
022790                         MOVE SPACE                TO Skƒtƒ‰ƒO
022800                     END-IF
022810                 ELSE
022820*/ƒGƒ‰[•\Ž¦‚ÌC³
022830                     PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
022840                     DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
022850                             " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
022860                             " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
022870                     PERFORM I—¹ˆ—
022880                     MOVE ZERO TO PROGRAM-STATUS
022890                     EXIT PROGRAM
022900                 END-IF
022910             END-IF
022920         END-READ
022930     END-IF.
022940*
022950*================================================================*
022960 ”PÁ“™Œã—Ã—¿Žæ“¾ SECTION.
022970*
022980     MOVE SPACE TO Skƒtƒ‰ƒO.
022990*/•‰•Ê˜JÐ—¿‹à—p‚Ì‹æ•ªƒR[ƒhi˜JÐ04j
023000     MOVE 04               TO —¿|‹æ•ªƒR[ƒh.
023010     MOVE •‰Ží•Ê‚v       TO —¿|•‰Ží•Ê.
023020     MOVE ZERO             TO —¿|•”ˆÊ.
023030     MOVE ZERO             TO —¿|¶‰E‹æ•ª.
023040     MOVE ZERO             TO —¿|•‰ˆÊ’u”Ô†.
023050     MOVE ˜AŒv|Ž{p˜a—ï   TO —¿|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
023060     MOVE ˜AŒv|Ž{p”N     TO —¿|ŠJŽn”N   Ž{p”N‚b‚v.
023070     MOVE ˜AŒv|Ž{pŒŽ     TO —¿|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
023080*
023090     START —¿‹àƒ}ƒXƒ^ KEY IS <= —¿|‹æ•ªƒR[ƒh
023100                                —¿|•”ˆÊƒR[ƒh
023110                                —¿|ŠJŽn˜a—ï”NŒŽ
023120                                REVERSED
023130     END-START.
023140*
023150     IF ó‘ÔƒL[ = "00"
023160         READ —¿‹àƒ}ƒXƒ^ NEXT
023170         AT END
023180*/ƒGƒ‰[•\Ž¦‚ÌC³
023190             PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
023200             DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
023210                     " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
023220                     " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
023230             PERFORM I—¹ˆ—
023240             MOVE ZERO TO PROGRAM-STATUS
023250             EXIT PROGRAM
023260         NOT AT END
023270             IF ( —¿|‹æ•ªƒR[ƒh NOT = 04         ) OR
023280                ( —¿|•‰Ží•Ê   NOT = •‰Ží•Ê‚v )   
023290*/ƒGƒ‰[•\Ž¦‚ÌC³
023300                 PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
023310                 DISPLAY "˜JÐ—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
023320                         " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
023330                         " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ
023340                         " •”ˆÊƒR[ƒh=" •”ˆÊƒR[ƒh‚v UPON CONS
023350*                 MOVE SPACE                TO ƒGƒ‰[ƒƒbƒZ[ƒW‚v
023360*                 MOVE •”ˆÊƒR[ƒh‚v         TO ƒGƒ‰[•”ˆÊƒR[ƒh‚v
023370*                 MOVE  NC"˜JÐ—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ@•”ˆÊƒR[ƒh"
023380*                                           TO ˜Aƒ‚R|ƒƒbƒZ[ƒW
023390*                 MOVE  ƒGƒ‰[ƒƒbƒZ[ƒW‚v  TO ˜Aƒ‚R|ƒƒbƒZ[ƒW‚P
023400*                 CALL   "MSG003"
023410*                 CANCEL "MSG003"
023420                 PERFORM I—¹ˆ—
023430                 MOVE ZERO TO PROGRAM-STATUS
023440                 EXIT PROGRAM
023450             ELSE
023460                 IF ( Ž{p˜a—ï”NŒŽ‚b‚v >= —¿‚c|ŠJŽn˜a—ï”NŒŽ ) AND
023470                    ( Ž{p˜a—ï”NŒŽ‚b‚v <= —¿‚c|I—¹˜a—ï”NŒŽ )
023480                     MOVE —¿‚c|˜JÐŒã—Ã—¿     TO Œã—Ã—¿‚c‚v
023490                     MOVE —¿‚c|˜JÐ®•œ—¿     TO ®•œ—¿‚c‚v
023500                     MOVE —¿‚c|˜JÐ“Á•ÊÞ—¿”ï TO “Á•ÊÞ—¿”ï‚c‚v
023510                     MOVE —¿‚c|˜JÐŒðŠ·•ï‘Ñ—¿ TO ŒðŠ·•ï‘Ñ—¿‚c‚v
023520                 ELSE
023530*/ƒGƒ‰[•\Ž¦‚ÌC³
023540                     PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
023550                     DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
023560                             " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
023570                             " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
023580                     PERFORM I—¹ˆ—
023590                     MOVE ZERO TO PROGRAM-STATUS
023600                     EXIT PROGRAM
023610                 END-IF
023620             END-IF
023630         END-READ
023640     END-IF.
023650*
023660*================================================================*
023670 ”PÁ“™®•œ—¿Žæ“¾ SECTION.
023680*
023690     MOVE 02               TO —¿|‹æ•ªƒR[ƒh.
023700     MOVE •‰Ží•Ê‚v       TO —¿|•‰Ží•Ê.
023710     MOVE ZERO             TO —¿|•”ˆÊ.
023720     MOVE ZERO             TO —¿|¶‰E‹æ•ª.
023730     MOVE ZERO             TO —¿|•‰ˆÊ’u”Ô†.
023740     MOVE ˜AŒv|Ž{p˜a—ï   TO —¿|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
023750     MOVE ˜AŒv|Ž{p”N     TO —¿|ŠJŽn”N   Ž{p”N‚b‚v.
023760     MOVE ˜AŒv|Ž{pŒŽ     TO —¿|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
023770*
023780     START —¿‹àƒ}ƒXƒ^ KEY IS <= —¿|‹æ•ªƒR[ƒh —¿|•”ˆÊƒR[ƒh
023790                                —¿|ŠJŽn˜a—ï”NŒŽ
023800                                                         REVERSED
023810     END-START.
023820*
023830     IF ó‘ÔƒL[ = "00"
023840         READ —¿‹àƒ}ƒXƒ^ NEXT
023850         AT END
023860*/ƒGƒ‰[•\Ž¦‚ÌC³
023870             PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
023880             DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
023890                     " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
023900                     " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
023910             PERFORM I—¹ˆ—
023920             MOVE ZERO TO PROGRAM-STATUS
023930             EXIT PROGRAM
023940         NOT AT END
023950             IF ( Ž{p˜a—ï”NŒŽ‚b‚v >= —¿‚a|ŠJŽn˜a—ï”NŒŽ ) AND
023960                ( Ž{p˜a—ï”NŒŽ‚b‚v <= —¿‚a|I—¹˜a—ï”NŒŽ )
023970                 MOVE —¿‚a|®•œ—¿     TO ®•œ—¿‚a‚v    
023980             ELSE
023990*/ƒGƒ‰[•\Ž¦‚ÌC³
024000                 PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
024010                 DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
024020                         " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
024030                         " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
024040                 PERFORM I—¹ˆ—
024050                 MOVE ZERO TO PROGRAM-STATUS
024060                 EXIT PROGRAM
024070             END-IF
024080         END-READ
024090     END-IF.
024100*
024110*================================================================*
024120 ŒvŽZî•ñŽæ“¾ SECTION.
024130*
024140* §Œä‹æ•ªF‚O‚Q@˜JÐ•ÛŒ¯
024150     MOVE 02             TO Œv|§Œä‹æ•ª.
024160     MOVE ˜AŒv|Ž{p˜a—ï TO Œv|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
024170     MOVE ˜AŒv|Ž{p”N   TO Œv|ŠJŽn”N   Ž{p”N‚b‚v.
024180     MOVE ˜AŒv|Ž{pŒŽ   TO Œv|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
024190*
024200     START ŒvŽZƒ}ƒXƒ^ KEY IS <= Œv|§Œä‹æ•ª Œv|ŠJŽn˜a—ï”NŒŽ
024210                                                     REVERSED
024220     END-START.
024230*
024240     IF ó‘ÔƒL[ = "00"
024250         READ ŒvŽZƒ}ƒXƒ^ NEXT
024260         AT END
024270*/ƒGƒ‰[•\Ž¦‚ÌC³
024280             PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
024290             DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
024300                     " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
024310                     " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
024320             PERFORM I—¹ˆ—
024330             MOVE ZERO TO PROGRAM-STATUS
024340             EXIT PROGRAM
024350         NOT AT END
024360             IF ( Ž{p˜a—ï”NŒŽ‚b‚v >= Œv‚a|ŠJŽn˜a—ï”NŒŽ ) AND
024370                ( Ž{p˜a—ï”NŒŽ‚b‚v <= Œv‚a|I—¹˜a—ï”NŒŽ )
024380                 MOVE Œv‚a|‰—ÃŠî–{‹——£     TO ‰—ÃŠî–{‹——£‚j‚v
024390                 MOVE Œv‚a|Œã—Ã‰Â”\ŽZ’è”   TO Œã—Ã‰Â”\ŽZ’è”‚j‚v
024400             ELSE
024410*/ƒGƒ‰[•\Ž¦‚ÌC³
024420                 PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
024430                 DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
024440                         " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
024450                         " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
024460                 PERFORM I—¹ˆ—
024470                 MOVE ZERO TO PROGRAM-STATUS
024480                 EXIT PROGRAM
024490             END-IF
024500         END-READ
024510     END-IF.
024520*
024530     PERFORM ‰ŒŸ¿‹‹æ•ªŽæ“¾.
024540*
024550*================================================================*
024560 Œã—Ã—¿ŽZ’è SECTION.
024570****************************************************************
024580* Œã—Ã—¿E—âãª–@—¿E‰·ãª–@—¿‚ðŠÜ‚Þ
024590* Œp‘±ˆÈŠOiŽ¡–üE’†Ž~E“]ˆã“™j‚ÅŽZ’è‚ÌŒJã‚è—L‚èi‚T•”ˆÊ‚Ü‚Åj
024600* Ž¡—Ã’†‚Ì•”ˆÊ‚ÌŽ¡–ü“™‚ÌƒP[ƒXB
024610* Œã—Ã—¿Eãª–@—¿E“d—Ã—¿‚ðŠÜ‚Þ
024620****************************************************************
024630*/“ú–ˆ€–Ú‚Ì‰Šú‰»
024640     MOVE ZERO TO Œã—Ã—¿‚P‚m‚q‚v Œã—Ã—¿‚PSk‚m‚q‚v —âãª–@—¿‚P‚m‚q‚v ‰·ãª–@—¿‚P‚m‚q‚v “d—Ã—¿‚P‚m‚q‚v
024650                  Œã—Ã—¿‚Q‚m‚q‚v Œã—Ã—¿‚QSk‚m‚q‚v —âãª–@—¿‚Q‚m‚q‚v ‰·ãª–@—¿‚Q‚m‚q‚v “d—Ã—¿‚Q‚m‚q‚v
024660                  Œã—Ã—¿‚R‚m‚q‚v Œã—Ã—¿‚RSk‚m‚q‚v —âãª–@—¿‚R‚m‚q‚v ‰·ãª–@—¿‚R‚m‚q‚v “d—Ã—¿‚R‚m‚q‚v
024670                  Œã—Ã—¿‚S‚m‚q‚v Œã—Ã—¿‚SSk‚m‚q‚v —âãª–@—¿‚S‚m‚q‚v ‰·ãª–@—¿‚S‚m‚q‚v “d—Ã—¿‚S‚m‚q‚v
024680                  Œã—Ã—¿‚T‚m‚q‚v Œã—Ã—¿‚TSk‚m‚q‚v —âãª–@—¿‚T‚m‚q‚v ‰·ãª–@—¿‚T‚m‚q‚v “d—Ã—¿‚T‚m‚q‚v
024690                  Œã—Ã—¿‚U‚m‚q‚v Œã—Ã—¿‚USk‚m‚q‚v —âãª–@—¿‚U‚m‚q‚v ‰·ãª–@—¿‚U‚m‚q‚v “d—Ã—¿‚U‚m‚q‚v
024700                  Œã—Ã—¿‚V‚m‚q‚v Œã—Ã—¿‚VSk‚m‚q‚v —âãª–@—¿‚V‚m‚q‚v ‰·ãª–@—¿‚V‚m‚q‚v “d—Ã—¿‚V‚m‚q‚v
024710                  Œã—Ã—¿‚W‚m‚q‚v Œã—Ã—¿‚WSk‚m‚q‚v —âãª–@—¿‚W‚m‚q‚v ‰·ãª–@—¿‚W‚m‚q‚v “d—Ã—¿‚W‚m‚q‚v
024720                  Œã—Ã—¿‚X‚m‚q‚v Œã—Ã—¿‚XSk‚m‚q‚v —âãª–@—¿‚X‚m‚q‚v ‰·ãª–@—¿‚X‚m‚q‚v “d—Ã—¿‚X‚m‚q‚v.
024730* Œp‘±•”ˆÊ”‚v‚ÌƒZƒbƒg
024740     PERFORM Œp‘±•”ˆÊ”ƒZƒbƒg.
024750*
024760     MOVE ZERO TO Œã—Ã—¿‚v —âãª–@—¿‚v ‰·ãª–@—¿‚v “d—Ã—¿‚v.
024770* Œp‘±‚Ì•”ˆÊ‚Ì‚ÝŽZ’è‚·‚é
024780     PERFORM VARYING •”ˆÊƒJƒEƒ“ƒ^‚Q FROM 1 BY 1
024790             UNTIL ( •”ˆÊƒJƒEƒ“ƒ^‚Q > •”ˆÊ”‚v )
024800* •‰|I—¹˜a—ï”NŒŽ‚ª–¢“ü—Í‚Ìê‡‚ÍAALL 9 ‚ðƒZƒbƒg‚·‚é
024810         PERFORM ŠJŽnI—¹“úŽæ“¾
024820         IF ( ŠJŽn˜a—ï”NŒŽ“ú‚v <= Ž{p˜a—ï”NŒŽ“ú‚b‚v ) AND
024830            ( I—¹˜a—ï”NŒŽ“ú‚v >= Ž{p˜a—ï”NŒŽ“ú‚b‚v )
024840*            ( “]‹A‹æ•ª‚v = 9 )
024850             MOVE Ž{‹L|•‰Ží•Ê    (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO •‰Ží•Ê‚v
024860             MOVE Ž{‹L|•”ˆÊ        (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO •”ˆÊ‚v
024870             MOVE Ž{‹L|¶‰E‹æ•ª    (•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ¶‰E‹æ•ª‚v
024880             MOVE Ž{‹L|•‰ˆÊ’u”Ô†(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO •‰ˆÊ’u”Ô†‚v
024890             IF •‰Ží•Ê‚v = 01 OR 02 OR 03
024900                 PERFORM ”PÁ“™Œã—Ã—¿Žæ“¾
024910             ELSE
024920                 PERFORM •‰•Ê—¿‹àŽæ“¾
024930             END-IF
024940* Œã—Ã’P‰¿‚Ì‘Þ”ð
024950*             IF •‰Ží•Ê‚v = 07 OR 08
024960*                 MOVE SkŒã—Ã—¿‚c‚v TO Œã—Ã’P‰¿‚n‚v( •”ˆÊƒJƒEƒ“ƒ^‚Q)
024970*             ELSE
024980*                 MOVE Œã—Ã—¿‚c‚v     TO Œã—Ã’P‰¿‚n‚v( •”ˆÊƒJƒEƒ“ƒ^‚Q)
024990*             END-IF
025000* Œã—Ã‚ÌŽž‚Ì‚ÝŽZ’èi‰‰ñˆ’u‚ðs‚í‚È‚¢‰ŒŸ“ú‚à‚r‚d‚j‚h‚q‚d‚b‚É‚Äl—¶Ïj
025010             IF ( Ž{‹L|Œã—Ã—¿¿‹‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q)   = 1 ) AND 
025020                ( Ž{‹L|®•œŽ{—Ã‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q) NOT = 1 )
025030                 IF ( Ž{‹L|ˆãŽtSk‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q) = 1 ) AND
025040                    ( •‰Ží•Ê‚v = 07 OR 08                  )
025050                     MOVE SkŒã—Ã—¿‚c‚v TO Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
025060                     MOVE SkŒã—Ã—¿‚c‚v TO SkŒã—Ã’P‰¿‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
025070*HILO                     PERFORM Œã—Ã‘½•”ˆÊ€–ÚŽæ“¾
025080                     PERFORM Œã—Ã‘½•”ˆÊ—ÝŒv
025090                 ELSE
025100*/œÜA•s‘SœÜ‚Ìê‡•‰“ú‚æ‚è‚Q‚Q“ú–Ú‚©‚çSkŒã—Ã—¿‚ðŽg‚¤
025110                     MOVE Œã—Ã—¿‚c‚v TO Œã—Ã—¿‚P‚O‚O‚v( •”ˆÊƒJƒEƒ“ƒ^‚Q)
025120                     IF Skƒtƒ‰ƒO = SPACE
025130                         MOVE Œã—Ã—¿‚c‚v TO Œã—Ã’P‰¿‚n‚v    (•”ˆÊƒJƒEƒ“ƒ^‚Q)
025140                     ELSE
025150                         MOVE Œã—Ã—¿‚c‚v TO SkŒã—Ã’P‰¿‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
025160                     END-IF
025170*HILO                     PERFORM Œã—Ã‘½•”ˆÊ€–ÚŽæ“¾
025180                     PERFORM Œã—Ã‘½•”ˆÊ—ÝŒv
025190                 END-IF
025200             ELSE IF ( Ž{‹L|Œã—Ã—¿¿‹‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q)   = 1 ) AND 
025210                     ( Ž{‹L|®•œŽ{—Ã‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q)     = 1 )
025220*/ƒGƒ‰[•\Ž¦‚ÌC³
025230                     PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
025240                     DISPLAY "®•œ—¿EŒã—Ã—¿‚Í“¯Žž‚É¿‹‚Å‚«‚Ü‚¹‚ñ"
025250                             " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
025260                             " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
025270                  END-IF
025280             END-IF
025290*
025300             EVALUATE Ž{‹L|ãª–@‹æ•ª( •”ˆÊƒJƒEƒ“ƒ^‚Q)
025310             WHEN 1
025320                 MOVE —âãª–@—¿‚b‚v TO —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
025330                 MOVE —âãª–@—¿‚b‚v TO —âãª–@’P‰¿‚v    (•”ˆÊƒJƒEƒ“ƒ^‚Q)
025340*                 PERFORM —âãª–@‘½•”ˆÊ€–ÚŽæ“¾
025350                 PERFORM —âãª–@‘½•”ˆÊ—ÝŒv
025360*
025370             WHEN 2
025380                 MOVE ‰·ãª–@—¿‚b‚v TO ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
025390                 MOVE ‰·ãª–@—¿‚b‚v TO ‰·ãª–@’P‰¿‚v    (•”ˆÊƒJƒEƒ“ƒ^‚Q)
025400*                 PERFORM ‰·ãª–@‘½•”ˆÊ€–ÚŽæ“¾
025410                 PERFORM ‰·ãª–@‘½•”ˆÊ—ÝŒv
025420             WHEN OTHER
025430                 CONTINUE
025440             END-EVALUATE
025450*
025460             IF Ž{‹L|“d—Ã‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1
025470                 MOVE “d—Ã—¿‚b‚v TO “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
025480                 MOVE “d—Ã—¿‚b‚v TO “d—Ã’P‰¿‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
025490*                 PERFORM “d—Ã‘½•”ˆÊ€–ÚŽæ“¾
025500                 PERFORM “d—Ã‘½•”ˆÊ—ÝŒv
025510             END-IF
025520*
025530         END-IF
025540     END-PERFORM.
025550*
025560*================================================================*
025570 ”NŒŽâ‘Î’lŒvŽZ SECTION.
025580*
025590* 1989”N1ŒŽ‚ðŠî€‚Æ‚µA“–”N‚Ü‚Å‚Ìâ‘Î’l‚ðŒvŽZ
025600     MOVE SPACE TO ’·Šúƒtƒ‰ƒO.
025610*
025620* ˜a—ï‚©‚ç¼—ï‚ðŠ·ŽZ
025630     MOVE ŠJŽn˜a—ï‚v TO Œ³|Œ³†‹æ•ª.
025640     READ Œ³†ƒ}ƒXƒ^
025650     INVALID KEY
025660         MOVE NC"Œ³†" TO ƒtƒ@ƒCƒ‹–¼‚v
025670     NOT INVALID KEY
025680         COMPUTE ¼—ï”N‚v   = Œ³|ŠJŽn¼—ï”N + ŠJŽn”N‚v
025690         COMPUTE ¼—ï”N·‚v = ¼—ï”N‚v - 1989
025700     END-READ.
025710*
025720* “–ŒŽ‚©—‚ŒŽ‚©‚Ì”»’è
025730     IF ŠJŽn“ú‚v > ’·Šú’üŒ¸ŒŽ‹«ŠE“ú‚j‚v
025740         COMPUTE ŠJŽn”NŒŽâ‘Î’l‚v = (¼—ï”N·‚v * 12) + ( ŠJŽnŒŽ‚v + 1 )
025750     ELSE
025760         COMPUTE ŠJŽn”NŒŽâ‘Î’l‚v = (¼—ï”N·‚v * 12) + ŠJŽnŒŽ‚v
025770     END-IF.
025780*
025790     MOVE ˜AŒv|Ž{p˜a—ï TO Œ³|Œ³†‹æ•ª.
025800     READ Œ³†ƒ}ƒXƒ^
025810     INVALID KEY
025820         MOVE NC"Œ³†" TO ƒtƒ@ƒCƒ‹–¼‚v
025830     NOT INVALID KEY
025840         COMPUTE ¼—ï”N‚v   = Œ³|ŠJŽn¼—ï”N + ˜AŒv|Ž{p”N
025850         COMPUTE ¼—ï”N·‚v = ¼—ï”N‚v - 1989
025860     END-READ.
025870* Ž{p“ú‚ÍA‹«ŠE“ú‚Ì”»’è‚Ì•K—v‚È‚µH
025880* “–ŒŽ‚©—‚ŒŽ‚©‚Ì”»’è
025890*     IF Ž{‹L|Ž{p“ú > ’·Šú’üŒ¸ŒŽ‹«ŠE“ú‚j‚v
025900*         COMPUTE Ž{p”NŒŽâ‘Î’l‚v = (¼—ï”N·‚v * 12) + ( ˜AŒv|Ž{pŒŽ + 1 )
025910*     ELSE
025920         COMPUTE Ž{p”NŒŽâ‘Î’l‚v = (¼—ï”N·‚v * 12) + ˜AŒv|Ž{pŒŽ
025930*     END-IF.
025940*
025950     COMPUTE Ž{p”NŒŽ·‚v = Ž{p”NŒŽâ‘Î’l‚v - ŠJŽn”NŒŽâ‘Î’l‚v.
025960*
025970     IF Ž{p”NŒŽ·‚v >= ’·Šú’üŒ¸ŒŽ”‚j‚v
025980         MOVE "YES" TO ’·Šúƒtƒ‰ƒO
025990     END-IF.
026000*
026010*================================================================*
026020 ‚»‚Ì‘¼ŽZ’è SECTION.
026030****************************************************************
026040* •›ŽqE‰‰ñˆ’uEî•ñ’ñ‹Ÿ—¿
026050* ‰‰ñˆ’u‚É‚Â‚¢‚Ä‚ÍA‚U•”ˆÊ–ÚˆÈ~‚àŽZ’è‚ ‚èB
026060****************************************************************
026070     MOVE ZERO TO ‹à‘®•›Žq—¿‚v.
026080     MOVE ZERO TO ‰‰ñˆ’u—¿‚v.
026090     MOVE ZERO TO Ž{pî•ñ’ñ‹Ÿ—¿‚v.
026100     MOVE ZERO TO “Á•ÊÞ—¿”ï‚v.
026110     MOVE ZERO TO ŒðŠ·•ï‘Ñ—¿‚v.
026120*     MOVE ZERO TO Žw“±ŠÇ——¿‚v.
           MOVE ZERO TO ‰‰ñˆ’u‚m‚n‚v.
           MOVE ZERO TO ŒðŠ·•ï‘Ñ—¿‚P‚m‚v.
           MOVE ZERO TO “Á•ÊÞ—¿”ï‚m‚v.
026130*
026140* Œp‘±‚Ì•”ˆÊ‚Ì‚ÝŽZ’è‚·‚é
026150* Œp‘±‚Ì•”ˆÊ‚Ì‚Ý•”ˆÊƒJƒEƒ“ƒ^‚ðƒJƒEƒ“ƒg‚t‚o‚·‚é
026160* ‰‰ñˆ’u‚Ì‚ÝŒã—Ã‰Â”\ŽZ’è”ˆÈã‚ÉŽZ’è‰Â”\
026170     MOVE 1 TO •”ˆÊƒJƒEƒ“ƒ^
026180     PERFORM VARYING •”ˆÊƒJƒEƒ“ƒ^‚Q FROM 1 BY 1
026190             UNTIL ( •”ˆÊƒJƒEƒ“ƒ^‚Q > •”ˆÊ”‚v ) OR
026200                   ( •”ˆÊƒJƒEƒ“ƒ^ > Œp‘±•”ˆÊ”‚v )
026210* •‰|I—¹˜a—ï”NŒŽ‚ª–¢“ü—Í‚Ìê‡‚ÍAALL 9 ‚ðƒZƒbƒg‚·‚é
026220         PERFORM ŠJŽnI—¹“úŽæ“¾
026230*
026240         IF ( ŠJŽn˜a—ï”NŒŽ“ú‚v <= Ž{p˜a—ï”NŒŽ“ú‚b‚v ) AND
026250            ( I—¹˜a—ï”NŒŽ“ú‚v >= Ž{p˜a—ï”NŒŽ“ú‚b‚v )
026260*            ( “]‹A‹æ•ª‚v = 9 )
026270*
026280             MOVE Ž{‹L|•‰Ží•Ê(•”ˆÊƒJƒEƒ“ƒ^‚Q)     TO •‰Ží•Ê‚v
026290             MOVE Ž{‹L|•”ˆÊ(•”ˆÊƒJƒEƒ“ƒ^‚Q)         TO •”ˆÊ‚v
026300             MOVE Ž{‹L|¶‰E‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q)     TO ¶‰E‹æ•ª‚v
026310             MOVE Ž{‹L|•‰ˆÊ’u”Ô†(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO •‰ˆÊ’u”Ô†‚v
026320             IF •‰Ží•Ê‚v = 01 OR 02 OR 03
026330                 PERFORM ”PÁ“™Œã—Ã—¿Žæ“¾
026340             ELSE
026350                 PERFORM •‰•Ê—¿‹àŽæ“¾
026360             END-IF
026370* ‰‰ñˆ’u—¿
026380             IF Ž{‹L|®•œŽ{—Ã‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1
026390                 COMPUTE ‰‰ñˆ’u—¿‚v = ‰‰ñˆ’u—¿‚v    + ®•œ—¿‚c‚v
026400                 COMPUTE ‰‰ñˆ’u—¿‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
026410                       = ‰‰ñˆ’u—¿‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) + ®•œ—¿‚c‚v
                       MOVE ®•œ—¿‚c‚v TO ‰‰ñˆ’u—¿‚m‚n‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
026420             END-IF
026430*
026440*             IF •”ˆÊƒJƒEƒ“ƒ^ <= Œã—Ã‰Â”\ŽZ’è”‚j‚v
026450* Ž{pî•ñ’ñ‹Ÿ—¿  œÜE•s‘SœÜE’E‰P‚Ìê‡‚Ì‚Ý
026460                 IF ( Ž{‹L|î•ñ’ñ‹Ÿ‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1 ) AND
026470                    ( Ž{‹L|•‰Ží•Ê(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 04 OR 05 OR 06 ) AND
026480                    ( ŠJŽn˜a—ï”NŒŽ“ú‚v = Ž{p˜a—ï”NŒŽ“ú‚b‚v )
026490                     COMPUTE î•ñ’ñ‹Ÿ‰ñ”‚v   = î•ñ’ñ‹Ÿ‰ñ”‚v + 1
026500                     COMPUTE Ž{pî•ñ’ñ‹Ÿ—¿‚v =
026510                             Ž{pî•ñ’ñ‹Ÿ—¿‚v + Ž{pî•ñ’ñ‹Ÿ—¿‚b‚v
026520                 END-IF
026530* “Á•ÊÞ—¿”ï
026540                 IF Ž{‹L|“Á•ÊÞ—¿‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚ÌŒðŠ·‚ÍA‚Q‰ñ‚Ü‚ÅŽZ’è‚Å‚«‚éB/20180829
                           PERFORM “Á•ÊÞ—¿‰ñ””»’è
      */•½¬30”N9ŒŽ1“ú‚æ‚èŽd—l•ÏX«««/20180427
026550*                     COMPUTE “Á•ÊÞ—¿”ï‚v = “Á•ÊÞ—¿”ï‚v + “Á•ÊÞ—¿”ï‚c‚v
026560*                     MOVE “Á•ÊÞ—¿”ï‚c‚v TO Þ—¿”ï‚Q‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
      *                                            Þ—¿”ï‚m‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
                           IF ˜AŒv|Ž{p˜a—ï”NŒŽ < 43009
      */ˆÈ‘O‚ÌŽd—l‚à•ÏX‚·‚é(‰½“x‚Å‚àŽZ’è‰Â‚¾‚Á‚½‚Ì‚ð‰‰ñˆ’u‚ÌŽž‚¾‚¯‚É•ÏX‚·‚é)/180829
026470                         IF Ž{‹L|®•œŽ{—Ã‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1
026550                             COMPUTE “Á•ÊÞ—¿”ï‚v = “Á•ÊÞ—¿”ï‚v + “Á•ÊÞ—¿”ï‚c‚v
026560                             MOVE “Á•ÊÞ—¿”ï‚c‚v TO Þ—¿”ï‚Q‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
                                                          Þ—¿”ï‚m‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
                               ELSE
                                   PERFORM “Á•ÊÞ—¿‹æ•ªƒNƒŠƒA
026570                         END-IF
                           ELSE
026470                         IF (Ž{‹L|•‰Ží•Ê(•”ˆÊƒJƒEƒ“ƒ^‚Q)     = 04 OR 05 OR 06)   AND (“Á•ÊÞ—¿‚b‚m‚s < 3) OR
026470                            (Ž{‹L|•‰Ží•Ê(•”ˆÊƒJƒEƒ“ƒ^‚Q) NOT = 04 AND 05 AND 06) AND (Ž{‹L|®•œŽ{—Ã‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1)
026550                             COMPUTE “Á•ÊÞ—¿”ï‚v = “Á•ÊÞ—¿”ï‚v + “Á•ÊÞ—¿”ï‚c‚v
026560                             COMPUTE Þ—¿”ï‚Q‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = Þ—¿”ï‚Q‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) + “Á•ÊÞ—¿”ï‚c‚v
026560                             MOVE “Á•ÊÞ—¿”ï‚c‚v TO Þ—¿”ï‚R‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) Þ—¿”ï‚m‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
                                   COMPUTE Þ—¿‰ñ”‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = Þ—¿‰ñ”‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) + 1
HILO  *                             DISPLAY "O " ˜AŒv|Ž{p˜a—ï”NŒŽ“ú " " •”ˆÊƒJƒEƒ“ƒ^‚Q "•”ˆÊ  “Á•ÊÞ—¿‰ñ”=" “Á•ÊÞ—¿‚b‚m‚s
                               ELSE
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚ÌŒðŠ·‚ÍA‚Q‰ñ‚Ü‚ÅŽZ’è‚Å‚«‚éB/20180829
                                   PERFORM “Á•ÊÞ—¿‹æ•ªƒNƒŠƒA
HILO  *                             DISPLAY "X " ˜AŒv|Ž{p˜a—ï”NŒŽ“ú " " •”ˆÊƒJƒEƒ“ƒ^‚Q "•”ˆÊ  “Á•ÊÞ—¿‰ñ”=" “Á•ÊÞ—¿‚b‚m‚s
026570                         END-IF
026570                     END-IF
      */•½¬30”N9ŒŽ1“ú‚æ‚èŽd—l•ÏXªªª/20180427
026570                 END-IF
026580* •ï‘ÑŒðŠ·—¿
026590                 IF Ž{‹L|•ï‘ÑŒðŠ·‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 1
026600*/˜JÐV—pŽ†‘Î‰ž(1704~)/0506
026610                     MOVE ŒðŠ·•ï‘Ñ—¿‚c‚v TO •ï‘ÑŒðŠ·’P‰¿‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
026620                     COMPUTE •ï‘ÑŒðŠ·‰ñ”‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) =
026630                             •ï‘ÑŒðŠ·‰ñ”‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) + 1
026640*
026650                     COMPUTE ŒðŠ·•ï‘Ñ—¿‚v = ŒðŠ·•ï‘Ñ—¿‚v + ŒðŠ·•ï‘Ñ—¿‚c‚v
026660                     COMPUTE •ï‘ÑŒðŠ·—¿‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) =
026670                             •ï‘ÑŒðŠ·—¿‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) + ŒðŠ·•ï‘Ñ—¿‚c‚v
                           MOVE ŒðŠ·•ï‘Ñ—¿‚c‚v TO ŒðŠ·•ï‘Ñ—¿‚m‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
026680                     EVALUATE TRUE
026690                     WHEN •ï‘ÑŒðŠ·“ú‚P‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO
026700                          MOVE Ž{‹L|Ž{p“ú TO •ï‘ÑŒðŠ·“ú‚P‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
026710                     WHEN •ï‘ÑŒðŠ·“ú‚Q‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO
026720                          MOVE Ž{‹L|Ž{p“ú TO •ï‘ÑŒðŠ·“ú‚Q‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
026730                     WHEN •ï‘ÑŒðŠ·“ú‚R‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO
026740                          MOVE Ž{‹L|Ž{p“ú TO •ï‘ÑŒðŠ·“ú‚R‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
026750                     WHEN •ï‘ÑŒðŠ·“ú‚S‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO
026760                          MOVE Ž{‹L|Ž{p“ú TO •ï‘ÑŒðŠ·“ú‚S‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
026770                     WHEN •ï‘ÑŒðŠ·“ú‚T‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO
026780                          MOVE Ž{‹L|Ž{p“ú TO •ï‘ÑŒðŠ·“ú‚T‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
026790                     WHEN •ï‘ÑŒðŠ·“ú‚U‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO
026800                          MOVE Ž{‹L|Ž{p“ú TO •ï‘ÑŒðŠ·“ú‚U‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
026810                     END-EVALUATE
026820*                 END-IF
026830             END-IF
026840*
026850             COMPUTE •”ˆÊƒJƒEƒ“ƒ^ = •”ˆÊƒJƒEƒ“ƒ^ + 1
026860*
026870         END-IF
026880     END-PERFORM.
026890*
026900*================================================================*
026910 ŠJŽnI—¹“úŽæ“¾ SECTION.
026920*
026930     MOVE ˜AŒv|Ž{p˜a—ï TO •‰|Ž{p˜a—ï.
026940     MOVE ˜AŒv|Ž{p”N   TO •‰|Ž{p”N.
026950     MOVE ˜AŒv|Ž{pŒŽ   TO •‰|Ž{pŒŽ.
026960     MOVE ˜AŒv|Š³ŽÒ”Ô† TO •‰|Š³ŽÒ”Ô†.
026970     MOVE ˜AŒv|Ž}”Ô     TO •‰|Ž}”Ô.
026980     READ •‰ƒf[ƒ^‚e
026990     INVALID KEY
027000*/ƒGƒ‰[•\Ž¦‚ÌC³
027010         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
027020         DISPLAY "Žw’è‚ÌŽófŽÒ‚Ì•‰ƒf[ƒ^‚ª‚ ‚è‚Ü‚¹‚ñ"
027030                 " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
027040                 " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
027050*-----------------------------------------*
027060         CALL "actcshm"  WITH C LINKAGE
027070*-----------------------------------------*
027080         ACCEPT  ƒL[“ü—Í FROM CONS
027090         PERFORM ƒtƒ@ƒCƒ‹•Â½
027100         MOVE ZERO TO PROGRAM-STATUS
027110         EXIT PROGRAM
027120     NOT INVALID KEY
027130         MOVE •‰|“]‹A‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^‚Q)       TO “]‹A‹æ•ª‚v
027140         MOVE •‰|ŠJŽn˜a—ï”NŒŽ“ú(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ŠJŽn˜a—ï”NŒŽ“ú‚v
027150*
027160         IF ( •‰|I—¹˜a—ï(•”ˆÊƒJƒEƒ“ƒ^‚Q) = ZERO ) OR
027170            ( •‰|I—¹”N(•”ˆÊƒJƒEƒ“ƒ^‚Q)   = ZERO ) OR
027180            ( •‰|I—¹ŒŽ(•”ˆÊƒJƒEƒ“ƒ^‚Q)   = ZERO ) OR
027190            ( •‰|I—¹“ú(•”ˆÊƒJƒEƒ“ƒ^‚Q)   = ZERO )
027200             MOVE "9999999" TO I—¹˜a—ï”NŒŽ“ú‚v
027210         ELSE
027220             MOVE •‰|I—¹˜a—ï”NŒŽ“ú(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO I—¹˜a—ï”NŒŽ“ú‚v
027230         END-IF
027240     END-READ.
027250*
027260*================================================================*
027270 Œp‘±•”ˆÊ”ƒZƒbƒg SECTION.
027280*
027290     MOVE ZERO TO Œp‘±•”ˆÊ”‚v.
027300*
027310     MOVE ˜AŒv|Ž{p˜a—ï TO •‰|Ž{p˜a—ï.
027320     MOVE ˜AŒv|Ž{p”N   TO •‰|Ž{p”N.
027330     MOVE ˜AŒv|Ž{pŒŽ   TO •‰|Ž{pŒŽ.
027340     MOVE ˜AŒv|Š³ŽÒ”Ô† TO •‰|Š³ŽÒ”Ô†.
027350     MOVE ˜AŒv|Ž}”Ô     TO •‰|Ž}”Ô.
027360     READ •‰ƒf[ƒ^‚e
027370     INVALID KEY
027380*/ƒGƒ‰[•\Ž¦‚ÌC³
027390         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
027400         DISPLAY "Žw’è‚ÌŽófŽÒ‚Ì•‰ƒf[ƒ^‚ª‚ ‚è‚Ü‚¹‚ñ"
027410                 " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
027420                 " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
027430*-----------------------------------------*
027440         CALL "actcshm"  WITH C LINKAGE
027450*-----------------------------------------*
027460         ACCEPT  ƒL[“ü—Í FROM CONS
027470         PERFORM ƒtƒ@ƒCƒ‹•Â½
027480         MOVE ZERO TO PROGRAM-STATUS
027490         EXIT PROGRAM
027500     NOT INVALID KEY
027510         MOVE •‰|•”ˆÊ” TO •”ˆÊ”‚v
027520         MOVE ˜AŒv|Ž{p˜a—ï TO  Ž{p˜a—ï‚b‚v
027530         MOVE ˜AŒv|Ž{p”N   TO  Ž{p”N‚b‚v
027540         MOVE ˜AŒv|Ž{pŒŽ   TO  Ž{pŒŽ‚b‚v
027550         MOVE Ž{‹L|Ž{p“ú   TO  Ž{p“ú‚b‚v
027560         PERFORM VARYING •”ˆÊƒJƒEƒ“ƒ^ FROM 1 BY 1
027570                 UNTIL •”ˆÊƒJƒEƒ“ƒ^ > •”ˆÊ”‚v
027580*
027590             IF ( •‰|I—¹˜a—ï(•”ˆÊƒJƒEƒ“ƒ^) = ZERO ) OR
027600                ( •‰|I—¹”N(•”ˆÊƒJƒEƒ“ƒ^)   = ZERO ) OR
027610                ( •‰|I—¹ŒŽ(•”ˆÊƒJƒEƒ“ƒ^)   = ZERO ) OR
027620                ( •‰|I—¹“ú(•”ˆÊƒJƒEƒ“ƒ^)   = ZERO )
027630                 MOVE "9999999" TO I—¹˜a—ï”NŒŽ“ú‚v
027640             ELSE
027650                 MOVE •‰|I—¹˜a—ï”NŒŽ“ú(•”ˆÊƒJƒEƒ“ƒ^) TO I—¹˜a—ï”NŒŽ“ú‚v
027660             END-IF
027670*
027680             IF ( •‰|ŠJŽn˜a—ï”NŒŽ“ú(•”ˆÊƒJƒEƒ“ƒ^) <= Ž{‹L|Ž{p˜a—ï”NŒŽ“ú ) AND
027690                ( I—¹˜a—ï”NŒŽ“ú‚v                 >= Ž{‹L|Ž{p˜a—ï”NŒŽ“ú )
027700*                ( •‰|“]‹A‹æ•ª(•”ˆÊƒJƒEƒ“ƒ^) = 9 )
027710                 COMPUTE Œp‘±•”ˆÊ”‚v = Œp‘±•”ˆÊ”‚v + 1
027720                 MOVE •”ˆÊƒJƒEƒ“ƒ^ TO •”ˆÊ’üŒ¸î•ñ‚v(Œp‘±•”ˆÊ”‚v)
027730             END-IF
027740         END-PERFORM
027750     END-READ.
027760*
027770*================================================================*
027780 —ÝŒvˆ— SECTION.
027790*
027800     COMPUTE ‰ŒŸ—¿‚f‚v         = ‰ŒŸ—¿‚f‚v     + ‰ŒŸ—¿‚v.
027810     COMPUTE ‰ŒŸ‰ÁŽZ—¿‚f‚v     = ‰ŒŸ‰ÁŽZ—¿‚f‚v + ‰ŒŸ‰ÁŽZ—¿‚v.
027820     COMPUTE ÄŒŸ—¿‚f‚v         = ÄŒŸ—¿‚f‚v     + ÄŒŸ—¿‚v.
027830     COMPUTE ‰—Ã—¿‚f‚v         = ‰—Ã—¿‚f‚v     + ‰—Ã—¿‚v.
027840     COMPUTE ‰—Ã‰ÁŽZ—¿‚f‚v     = ‰—Ã‰ÁŽZ—¿‚f‚v + ‰—Ã‰ÁŽZ—¿‚v.
027850     COMPUTE ‰ŒŸŽž‘Š’k—¿‚f‚v   = ‰ŒŸŽž‘Š’k—¿‚f‚v + ‰ŒŸŽž‘Š’k—¿‚v.
027860*
027870*     COMPUTE Œã—Ã—¿‚f‚v         = Œã—Ã—¿‚f‚v     + Œã—Ã—¿‚v.
027880*     COMPUTE —âãª–@—¿‚f‚v       = —âãª–@—¿‚f‚v   + —âãª–@—¿‚v.
027890*     COMPUTE ‰·ãª–@—¿‚f‚v       = ‰·ãª–@—¿‚f‚v   + ‰·ãª–@—¿‚v.
027900*     COMPUTE “d—Ã—¿‚f‚v         = “d—Ã—¿‚f‚v     + “d—Ã—¿‚v.
027910*
027920     COMPUTE ‹à‘®•›Žq—¿‚f‚v     = ‹à‘®•›Žq—¿‚f‚v + ‹à‘®•›Žq—¿‚v.
027930     COMPUTE ‰‰ñˆ’u—¿‚f‚v     = ‰‰ñˆ’u—¿‚f‚v + ‰‰ñˆ’u—¿‚v.
027940     COMPUTE Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v = Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v + Ž{pî•ñ’ñ‹Ÿ—¿‚v.
027950*/˜JÐ•ÛŒ¯—p
027960     COMPUTE Žw“±ŠÇ——¿‚f‚v     = Žw“±ŠÇ——¿‚f‚v   + Žw“±ŠÇ——¿‚v.
027970     COMPUTE “Á•ÊÞ—¿”ï‚f‚v     = “Á•ÊÞ—¿”ï‚f‚v   + “Á•ÊÞ—¿”ï‚v.
027980     COMPUTE ŒðŠ·•ï‘Ñ—¿‚f‚v     = ŒðŠ·•ï‘Ñ—¿‚f‚v   + ŒðŠ·•ï‘Ñ—¿‚v.
027990     COMPUTE ‰^“®—Ã–@—¿‚f‚v     = ‰^“®—Ã–@—¿‚f‚v   + ‰^“®—Ã–@—¿‚v.
028000     COMPUTE h”‘—¿‚f‚v         = h”‘—¿‚f‚v       + h”‘—¿‚v.
028010     COMPUTE HŽ–—¿‚f‚v         = HŽ–—¿‚f‚v       + HŽ–—¿‚v.
028020     COMPUTE ‹x‹ÆØ–¾‘—¿‚f‚v   = ‹x‹ÆØ–¾‘—¿‚f‚v + ‹x‹ÆØ–¾‘—¿‚v.
028030*
028040*================================================================*
028050 Œã—Ã‘½•”ˆÊ—ÝŒv SECTION.
028060*
028070*/˜JÐ•ÛŒ¯‚Í’üŒ¸§‚Í‚ ‚è‚Ü‚¹‚ñ
028080*
028090     EVALUATE •”ˆÊƒJƒEƒ“ƒ^‚Q
028100     WHEN 1
028110         IF Skƒtƒ‰ƒO = SPACE
028120             COMPUTE Œã—Ã‰ñ”‚P‚q‚v = Œã—Ã‰ñ”‚P‚q‚v + 1
028130             COMPUTE Œã—Ã—¿‚P‚q‚v   =
028140                     Œã—Ã—¿‚P‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028150             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚P‚m‚q‚v
028160         ELSE
028170             COMPUTE Œã—Ã‰ñ”‚PSk‚q‚v = Œã—Ã‰ñ”‚PSk‚q‚v + 1
028180             COMPUTE Œã—Ã—¿‚PSk‚q‚v   =
028190                     Œã—Ã—¿‚PSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028200             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚PSk‚m‚q‚v
028210         END-IF
028220     WHEN 2
028230         IF Skƒtƒ‰ƒO = SPACE
028240             COMPUTE Œã—Ã‰ñ”‚Q‚q‚v = Œã—Ã‰ñ”‚Q‚q‚v + 1
028250             COMPUTE Œã—Ã—¿‚Q‚q‚v   =
028260                     Œã—Ã—¿‚Q‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028270             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚Q‚m‚q‚v
028280         ELSE
028290             COMPUTE Œã—Ã‰ñ”‚QSk‚q‚v = Œã—Ã‰ñ”‚QSk‚q‚v + 1
028300             COMPUTE Œã—Ã—¿‚QSk‚q‚v   =
028310                     Œã—Ã—¿‚QSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028320             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚QSk‚m‚q‚v
028330         END-IF
028340     WHEN 3
028350         IF Skƒtƒ‰ƒO = SPACE
028360             COMPUTE Œã—Ã‰ñ”‚R‚q‚v = Œã—Ã‰ñ”‚R‚q‚v + 1
028370             COMPUTE Œã—Ã—¿‚R‚q‚v   =
028380                     Œã—Ã—¿‚R‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028390             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚R‚m‚q‚v
028400         ELSE
028410             COMPUTE Œã—Ã‰ñ”‚RSk‚q‚v = Œã—Ã‰ñ”‚RSk‚q‚v + 1
028420             COMPUTE Œã—Ã—¿‚RSk‚q‚v   =
028430                     Œã—Ã—¿‚RSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028440             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚RSk‚m‚q‚v
028450         END-IF
028460     WHEN 4
028470         IF Skƒtƒ‰ƒO = SPACE
028480             COMPUTE Œã—Ã‰ñ”‚S‚q‚v = Œã—Ã‰ñ”‚S‚q‚v + 1
028490             COMPUTE Œã—Ã—¿‚S‚q‚v   =
028500                     Œã—Ã—¿‚S‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028510             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚S‚m‚q‚v
028520         ELSE
028530             COMPUTE Œã—Ã‰ñ”‚SSk‚q‚v = Œã—Ã‰ñ”‚SSk‚q‚v + 1
028540             COMPUTE Œã—Ã—¿‚SSk‚q‚v   =
028550                     Œã—Ã—¿‚SSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028560             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚SSk‚m‚q‚v
028570         END-IF
028580     WHEN 5
028590         IF Skƒtƒ‰ƒO = SPACE
028600             COMPUTE Œã—Ã‰ñ”‚T‚q‚v = Œã—Ã‰ñ”‚T‚q‚v + 1
028610             COMPUTE Œã—Ã—¿‚T‚q‚v   =
028620                     Œã—Ã—¿‚T‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028630             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚T‚m‚q‚v
028640         ELSE
028650             COMPUTE Œã—Ã‰ñ”‚TSk‚q‚v = Œã—Ã‰ñ”‚TSk‚q‚v + 1
028660             COMPUTE Œã—Ã—¿‚TSk‚q‚v   =
028670                     Œã—Ã—¿‚TSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028680             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚TSk‚m‚q‚v
028690         END-IF
028700     WHEN 6
028710         IF Skƒtƒ‰ƒO = SPACE
028720             COMPUTE Œã—Ã‰ñ”‚U‚q‚v = Œã—Ã‰ñ”‚U‚q‚v + 1
028730             COMPUTE Œã—Ã—¿‚U‚q‚v   =
028740                     Œã—Ã—¿‚U‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028750             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚U‚m‚q‚v
028760         ELSE
028770             COMPUTE Œã—Ã‰ñ”‚USk‚q‚v = Œã—Ã‰ñ”‚USk‚q‚v + 1
028780             COMPUTE Œã—Ã—¿‚USk‚q‚v   =
028790                     Œã—Ã—¿‚USk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028800             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚USk‚m‚q‚v
028810         END-IF
028820     WHEN 7
028830         IF Skƒtƒ‰ƒO = SPACE
028840             COMPUTE Œã—Ã‰ñ”‚V‚q‚v = Œã—Ã‰ñ”‚V‚q‚v + 1
028850             COMPUTE Œã—Ã—¿‚V‚q‚v   =
028860                     Œã—Ã—¿‚V‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028870             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚V‚m‚q‚v
028880         ELSE
028890             COMPUTE Œã—Ã‰ñ”‚VSk‚q‚v = Œã—Ã‰ñ”‚VSk‚q‚v + 1
028900             COMPUTE Œã—Ã—¿‚VSk‚q‚v   =
028910                     Œã—Ã—¿‚VSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028920             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚VSk‚m‚q‚v
028930         END-IF
028940     WHEN 8
028950         IF Skƒtƒ‰ƒO = SPACE
028960             COMPUTE Œã—Ã‰ñ”‚W‚q‚v = Œã—Ã‰ñ”‚W‚q‚v + 1
028970             COMPUTE Œã—Ã—¿‚W‚q‚v   =
028980                     Œã—Ã—¿‚W‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
028990             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚W‚m‚q‚v
029000         ELSE
029010             COMPUTE Œã—Ã‰ñ”‚WSk‚q‚v = Œã—Ã‰ñ”‚WSk‚q‚v + 1
029020             COMPUTE Œã—Ã—¿‚WSk‚q‚v   =
029030                     Œã—Ã—¿‚WSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029040             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚WSk‚m‚q‚v
029050         END-IF
029060     WHEN 9
029070         IF Skƒtƒ‰ƒO = SPACE
029080             COMPUTE Œã—Ã‰ñ”‚X‚q‚v = Œã—Ã‰ñ”‚X‚q‚v + 1
029090             COMPUTE Œã—Ã—¿‚X‚q‚v   =
029100                     Œã—Ã—¿‚X‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029110             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚X‚m‚q‚v
029120         ELSE
029130             COMPUTE Œã—Ã‰ñ”‚XSk‚q‚v = Œã—Ã‰ñ”‚XSk‚q‚v + 1
029140             COMPUTE Œã—Ã—¿‚XSk‚q‚v   =
029150                     Œã—Ã—¿‚XSk‚q‚v   + Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029160             MOVE Œã—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO Œã—Ã—¿‚XSk‚m‚q‚v
029170         END-IF
029180     WHEN OTHER
029190         CONTINUE
029200     END-EVALUATE.
029210*
029220*================================================================*
029230 —âãª–@‘½•”ˆÊ—ÝŒv SECTION.
029240*
029250     EVALUATE •”ˆÊƒJƒEƒ“ƒ^‚Q
029260     WHEN 1
029270         COMPUTE —âãª–@‰ñ”‚P‚q‚v   = —âãª–@‰ñ”‚P‚q‚v + 1
029280         COMPUTE —âãª–@—¿‚P‚q‚v     =
029290                 —âãª–@—¿‚P‚q‚v     + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029300         MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚P‚m‚q‚v
029310     WHEN 2
029320         COMPUTE —âãª–@‰ñ”‚Q‚q‚v   = —âãª–@‰ñ”‚Q‚q‚v + 1
029330         COMPUTE —âãª–@—¿‚Q‚q‚v     =
029340                 —âãª–@—¿‚Q‚q‚v     + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029350         MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚Q‚m‚q‚v
029360     WHEN 3
029370         COMPUTE —âãª–@‰ñ”‚R‚q‚v = —âãª–@‰ñ”‚R‚q‚v + 1
029380         COMPUTE —âãª–@—¿‚R‚q‚v   =
029390                 —âãª–@—¿‚R‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029400         MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚R‚m‚q‚v
029410     WHEN 4
029420         COMPUTE —âãª–@‰ñ”‚S‚q‚v = —âãª–@‰ñ”‚S‚q‚v + 1
029430         COMPUTE —âãª–@—¿‚S‚q‚v   =
029440                 —âãª–@—¿‚S‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029450         MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚S‚m‚q‚v
029460     WHEN 5
029470         COMPUTE —âãª–@‰ñ”‚T‚q‚v = —âãª–@‰ñ”‚T‚q‚v + 1
029480         COMPUTE —âãª–@—¿‚T‚q‚v   =
029490                 —âãª–@—¿‚T‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029500         MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚T‚m‚q‚v
029510     WHEN 6
029520         COMPUTE —âãª–@‰ñ”‚U‚q‚v = —âãª–@‰ñ”‚U‚q‚v + 1
029530         COMPUTE —âãª–@—¿‚U‚q‚v   =
029540                 —âãª–@—¿‚U‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029550         MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚U‚m‚q‚v
029560     WHEN 7
029570         COMPUTE —âãª–@‰ñ”‚V‚q‚v = —âãª–@‰ñ”‚V‚q‚v + 1
029580         COMPUTE —âãª–@—¿‚V‚q‚v   =
029590                 —âãª–@—¿‚V‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029600         MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚V‚m‚q‚v
029610     WHEN 8
029620         COMPUTE —âãª–@‰ñ”‚W‚q‚v = —âãª–@‰ñ”‚W‚q‚v + 1
029630         COMPUTE —âãª–@—¿‚W‚q‚v   =
029640                 —âãª–@—¿‚W‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029650         MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚W‚m‚q‚v
029660     WHEN 9
029670         COMPUTE —âãª–@‰ñ”‚X‚q‚v = —âãª–@‰ñ”‚X‚q‚v + 1
029680         COMPUTE —âãª–@—¿‚X‚q‚v   =
029690                 —âãª–@—¿‚X‚q‚v   + —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029700         MOVE —âãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO —âãª–@—¿‚X‚m‚q‚v
029710     WHEN OTHER
029720         CONTINUE
029730     END-EVALUATE.
029740*
029750*================================================================*
029760 ‰·ãª–@‘½•”ˆÊ—ÝŒv SECTION.
029770*
029780     EVALUATE •”ˆÊƒJƒEƒ“ƒ^‚Q
029790     WHEN 1
029800         COMPUTE ‰·ãª–@‰ñ”‚P‚q‚v   = ‰·ãª–@‰ñ”‚P‚q‚v + 1
029810         COMPUTE ‰·ãª–@—¿‚P‚q‚v     =
029820                 ‰·ãª–@—¿‚P‚q‚v     + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029830         MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚P‚m‚q‚v
029840     WHEN 2
029850         COMPUTE ‰·ãª–@‰ñ”‚Q‚q‚v   = ‰·ãª–@‰ñ”‚Q‚q‚v + 1
029860         COMPUTE ‰·ãª–@—¿‚Q‚q‚v     =
029870                 ‰·ãª–@—¿‚Q‚q‚v     + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029880         MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚Q‚m‚q‚v
029890     WHEN 3
029900         COMPUTE ‰·ãª–@‰ñ”‚R‚q‚v = ‰·ãª–@‰ñ”‚R‚q‚v + 1
029910         COMPUTE ‰·ãª–@—¿‚R‚q‚v   =
029920                 ‰·ãª–@—¿‚R‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029930         MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚R‚m‚q‚v
029940     WHEN 4
029950         COMPUTE ‰·ãª–@‰ñ”‚S‚q‚v = ‰·ãª–@‰ñ”‚S‚q‚v + 1
029960         COMPUTE ‰·ãª–@—¿‚S‚q‚v   =
029970                 ‰·ãª–@—¿‚S‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
029980         MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚S‚m‚q‚v
029990     WHEN 5
030000         COMPUTE ‰·ãª–@‰ñ”‚T‚q‚v = ‰·ãª–@‰ñ”‚T‚q‚v + 1
030010         COMPUTE ‰·ãª–@—¿‚T‚q‚v   =
030020                 ‰·ãª–@—¿‚T‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030030         MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚T‚m‚q‚v
030040     WHEN 6
030050         COMPUTE ‰·ãª–@‰ñ”‚U‚q‚v = ‰·ãª–@‰ñ”‚U‚q‚v + 1
030060         COMPUTE ‰·ãª–@—¿‚U‚q‚v   =
030070                 ‰·ãª–@—¿‚U‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030080         MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚U‚m‚q‚v
030090     WHEN 7
030100         COMPUTE ‰·ãª–@‰ñ”‚V‚q‚v = ‰·ãª–@‰ñ”‚V‚q‚v + 1
030110         COMPUTE ‰·ãª–@—¿‚V‚q‚v   =
030120                 ‰·ãª–@—¿‚V‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030130         MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚V‚m‚q‚v
030140     WHEN 8
030150         COMPUTE ‰·ãª–@‰ñ”‚W‚q‚v = ‰·ãª–@‰ñ”‚W‚q‚v + 1
030160         COMPUTE ‰·ãª–@—¿‚W‚q‚v   =
030170                 ‰·ãª–@—¿‚W‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030180         MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚W‚m‚q‚v
030190     WHEN 9
030200         COMPUTE ‰·ãª–@‰ñ”‚X‚q‚v = ‰·ãª–@‰ñ”‚X‚q‚v + 1
030210         COMPUTE ‰·ãª–@—¿‚X‚q‚v   =
030220                 ‰·ãª–@—¿‚X‚q‚v   + ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030230         MOVE ‰·ãª–@—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO ‰·ãª–@—¿‚X‚m‚q‚v
030240     WHEN OTHER
030250         CONTINUE
030260     END-EVALUATE.
030270*
030280*================================================================*
030290 “d—Ã‘½•”ˆÊ—ÝŒv SECTION.
030300*
030310     EVALUATE •”ˆÊƒJƒEƒ“ƒ^‚Q
030320     WHEN 1
030330         COMPUTE “d—Ã‰ñ”‚P‚q‚v = “d—Ã‰ñ”‚P‚q‚v + 1
030340         COMPUTE “d—Ã—¿‚P‚q‚v   =
030350                 “d—Ã—¿‚P‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030360         MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚P‚m‚q‚v
030370     WHEN 2
030380         COMPUTE “d—Ã‰ñ”‚Q‚q‚v = “d—Ã‰ñ”‚Q‚q‚v + 1
030390         COMPUTE “d—Ã—¿‚Q‚q‚v   =
030400                 “d—Ã—¿‚Q‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030410         MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚Q‚m‚q‚v
030420     WHEN 3
030430         COMPUTE “d—Ã‰ñ”‚R‚q‚v = “d—Ã‰ñ”‚R‚q‚v + 1
030440         COMPUTE “d—Ã—¿‚R‚q‚v   =
030450                 “d—Ã—¿‚R‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030460         MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚R‚m‚q‚v
030470     WHEN 4
030480         COMPUTE “d—Ã‰ñ”‚S‚q‚v = “d—Ã‰ñ”‚S‚q‚v + 1
030490         COMPUTE “d—Ã—¿‚S‚q‚v   =
030500                 “d—Ã—¿‚S‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030510         MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚S‚m‚q‚v
030520     WHEN 5
030530         COMPUTE “d—Ã‰ñ”‚T‚q‚v = “d—Ã‰ñ”‚T‚q‚v + 1
030540         COMPUTE “d—Ã—¿‚T‚q‚v   =
030550                 “d—Ã—¿‚T‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030560         MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚T‚m‚q‚v
030570     WHEN 6
030580         COMPUTE “d—Ã‰ñ”‚U‚q‚v = “d—Ã‰ñ”‚U‚q‚v + 1
030590         COMPUTE “d—Ã—¿‚U‚q‚v   =
030600                 “d—Ã—¿‚U‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030610         MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚U‚m‚q‚v
030620     WHEN 7
030630         COMPUTE “d—Ã‰ñ”‚V‚q‚v = “d—Ã‰ñ”‚V‚q‚v + 1
030640         COMPUTE “d—Ã—¿‚V‚q‚v   =
030650                 “d—Ã—¿‚V‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030660         MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚V‚m‚q‚v
030670     WHEN 8
030680         COMPUTE “d—Ã‰ñ”‚W‚q‚v = “d—Ã‰ñ”‚W‚q‚v + 1
030690         COMPUTE “d—Ã—¿‚W‚q‚v   =
030700                 “d—Ã—¿‚W‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030710         MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚W‚m‚q‚v
030720     WHEN 9
030730         COMPUTE “d—Ã‰ñ”‚X‚q‚v = “d—Ã‰ñ”‚X‚q‚v + 1
030740         COMPUTE “d—Ã—¿‚X‚q‚v   =
030750                 “d—Ã—¿‚X‚q‚v   + “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
030760         MOVE “d—Ã—¿‚P‚O‚O‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q) TO “d—Ã—¿‚X‚m‚q‚v
030770     WHEN OTHER
030780         CONTINUE
030790     END-EVALUATE.
030800*
030810*================================================================*
030820 —ÝŒv€–Ú‰Šú‰» SECTION.
030830*
030840     PERFORM VARYING •”ˆÊ‚b‚m‚s FROM 1 BY 1
030850            UNTIL   •”ˆÊ‚b‚m‚s > Å‘å“o˜^•”ˆÊ”‚v
030860         MOVE ZERO TO ‰‰ñˆ’u—¿‚n‚v(•”ˆÊ‚b‚m‚s)
030870*/˜JÐ
030880         MOVE ZERO TO Þ—¿”ï‚Q‚v    (•”ˆÊ‚b‚m‚s)
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚ÌŒðŠ·‚ÍA‚Q‰ñ‚Ü‚ÅŽZ’è‚Å‚«‚éB/20180830
039560         MOVE ZERO TO Þ—¿‰ñ”‚v    (•”ˆÊ‚b‚m‚s)
039560         MOVE ZERO TO Þ—¿”ï‚R‚v    (•”ˆÊ‚b‚m‚s)
030890     END-PERFORM.
030900*
030910     MOVE ZERO TO ‰ŒŸ—¿‚f‚v ‰ŒŸ‰ÁŽZ—¿‚f‚v
030920                  ÄŒŸ—¿‚f‚v ‰—Ã—¿‚f‚v ‰—Ã‰ÁŽZ—¿‚f‚v ‰—Ã‰ñ”‚f‚v
030930                  Œã—Ã—¿‚f‚v —âãª–@—¿‚f‚v ‰·ãª–@—¿‚f‚v “d—Ã—¿‚f‚v
030940                  ‹à‘®•›Žq—¿‚f‚v
030950                  ‰‰ñˆ’u—¿‚f‚v Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v
030960                  ”ï—pŠz˜Vl‚f‚v •‰’SŠz˜Vl‚f‚v •‰’SŠz•¬‚f‚v •‰’SŠz‚f‚v
030970                  ‰ŒŸŽž‘Š’k—¿‚f‚v
030980*/˜JÐ•ÛŒ¯—p
030990                  Žw“±ŠÇ——¿‚f‚v “Á•ÊÞ—¿”ï‚f‚v ŒðŠ·•ï‘Ñ—¿‚f‚v
031000                  ‰^“®—Ã–@—¿‚f‚v h”‘—¿‚f‚v HŽ–—¿‚f‚v ‹x‹ÆØ–¾‘—¿‚f‚v.
031010*
031020     MOVE ZERO TO Œã—Ã—¿‚P‚q‚v Œã—Ã—¿‚Q‚q‚v
031030                  Œã—Ã—¿‚R‚q‚v Œã—Ã—¿‚S‚q‚v
031040                  Œã—Ã—¿‚T‚q‚v Œã—Ã—¿‚U‚q‚v
031050                  Œã—Ã—¿‚V‚q‚v Œã—Ã—¿‚W‚q‚v
031060                  Œã—Ã—¿‚X‚q‚v.
031070*Sk—p
031080     MOVE ZERO TO Œã—Ã—¿‚PSk‚q‚v Œã—Ã—¿‚QSk‚q‚v
031090                  Œã—Ã—¿‚RSk‚q‚v Œã—Ã—¿‚SSk‚q‚v 
031100                  Œã—Ã—¿‚TSk‚q‚v Œã—Ã—¿‚USk‚q‚v
031110                  Œã—Ã—¿‚VSk‚q‚v Œã—Ã—¿‚WSk‚q‚v
031120                  Œã—Ã—¿‚XSk‚q‚v.
031130*
031140     MOVE ZERO TO —âãª–@—¿‚P‚q‚v —âãª–@—¿‚Q‚q‚v —âãª–@—¿‚R‚q‚v
031150                  —âãª–@—¿‚S‚q‚v —âãª–@—¿‚T‚q‚v —âãª–@—¿‚U‚q‚v
031160                  —âãª–@—¿‚V‚q‚v —âãª–@—¿‚W‚q‚v —âãª–@—¿‚X‚q‚v.
031170*
031180     MOVE ZERO TO ‰·ãª–@—¿‚P‚q‚v ‰·ãª–@—¿‚Q‚q‚v ‰·ãª–@—¿‚R‚q‚v
031190                  ‰·ãª–@—¿‚S‚q‚v ‰·ãª–@—¿‚T‚q‚v ‰·ãª–@—¿‚U‚q‚v
031200                  ‰·ãª–@—¿‚V‚q‚v ‰·ãª–@—¿‚W‚q‚v ‰·ãª–@—¿‚X‚q‚v.
031210*
031220     MOVE ZERO TO “d—Ã—¿‚P‚q‚v “d—Ã—¿‚Q‚q‚v “d—Ã—¿‚R‚q‚v
031230                  “d—Ã—¿‚S‚q‚v “d—Ã—¿‚T‚q‚v “d—Ã—¿‚U‚q‚v
031240                  “d—Ã—¿‚V‚q‚v “d—Ã—¿‚W‚q‚v “d—Ã—¿‚X‚q‚v.
031250*
031260     MOVE ZERO TO Œã—Ã‰ñ”‚P‚q‚v Œã—Ã‰ñ”‚Q‚q‚v Œã—Ã‰ñ”‚R‚q‚v
031270                  Œã—Ã‰ñ”‚S‚q‚v Œã—Ã‰ñ”‚T‚q‚v Œã—Ã‰ñ”‚U‚q‚v
031280                  Œã—Ã‰ñ”‚V‚q‚v Œã—Ã‰ñ”‚W‚q‚v Œã—Ã‰ñ”‚X‚q‚v.
031290*
031300*/Sk—p
031310     MOVE ZERO TO Œã—Ã‰ñ”‚PSk‚q‚v   Œã—Ã—¿‚PSk‚q‚v
031320                  —âãª–@‰ñ”‚PSk‚q‚v —âãª–@—¿‚PSk‚q‚v
031330                  ‰·ãª–@‰ñ”‚PSk‚q‚v ‰·ãª–@—¿‚PSk‚q‚v
031340                  “d—Ã‰ñ”‚PSk‚q‚v   “d—Ã—¿‚PSk‚q‚v
031350                  ¬Œv‚PSk‚q‚v.
031360*
031370     MOVE ZERO TO Œã—Ã‰ñ”‚QSk‚q‚v   Œã—Ã—¿‚QSk‚q‚v
031380                  —âãª–@‰ñ”‚QSk‚q‚v —âãª–@—¿‚QSk‚q‚v
031390                  ‰·ãª–@‰ñ”‚QSk‚q‚v ‰·ãª–@—¿‚QSk‚q‚v
031400                  “d—Ã‰ñ”‚QSk‚q‚v   “d—Ã—¿‚QSk‚q‚v
031410                  ¬Œv‚QSk‚q‚v.
031420*
031430     MOVE ZERO TO Œã—Ã‰ñ”‚RSk‚q‚v   Œã—Ã—¿‚RSk‚q‚v
031440                  —âãª–@‰ñ”‚RSk‚q‚v —âãª–@—¿‚RSk‚q‚v
031450                  ‰·ãª–@‰ñ”‚RSk‚q‚v ‰·ãª–@—¿‚RSk‚q‚v
031460                  “d—Ã‰ñ”‚RSk‚q‚v   “d—Ã—¿‚RSk‚q‚v
031470                  ¬Œv‚RSk‚q‚v.
031480*
031490     MOVE ZERO TO Œã—Ã‰ñ”‚SSk‚q‚v   Œã—Ã—¿‚SSk‚q‚v
031500                  —âãª–@‰ñ”‚SSk‚q‚v —âãª–@—¿‚SSk‚q‚v
031510                  ‰·ãª–@‰ñ”‚SSk‚q‚v ‰·ãª–@—¿‚SSk‚q‚v
031520                  “d—Ã‰ñ”‚SSk‚q‚v   “d—Ã—¿‚SSk‚q‚v
031530                  ¬Œv‚SSk‚q‚v.
031540*
031550     MOVE ZERO TO Œã—Ã‰ñ”‚TSk‚q‚v   Œã—Ã—¿‚TSk‚q‚v
031560                  —âãª–@‰ñ”‚TSk‚q‚v —âãª–@—¿‚TSk‚q‚v
031570                  ‰·ãª–@‰ñ”‚TSk‚q‚v ‰·ãª–@—¿‚TSk‚q‚v
031580                  “d—Ã‰ñ”‚TSk‚q‚v   “d—Ã—¿‚TSk‚q‚v
031590                  ¬Œv‚TSk‚q‚v.
031600*
031610     MOVE ZERO TO Œã—Ã‰ñ”‚USk‚q‚v   Œã—Ã—¿‚USk‚q‚v
031620                  —âãª–@‰ñ”‚USk‚q‚v —âãª–@—¿‚USk‚q‚v
031630                  ‰·ãª–@‰ñ”‚USk‚q‚v ‰·ãª–@—¿‚USk‚q‚v
031640                  “d—Ã‰ñ”‚USk‚q‚v   “d—Ã—¿‚USk‚q‚v
031650                  ¬Œv‚USk‚q‚v.
031660*
031670     MOVE ZERO TO Œã—Ã‰ñ”‚VSk‚q‚v   Œã—Ã—¿‚VSk‚q‚v
031680                  —âãª–@‰ñ”‚VSk‚q‚v —âãª–@—¿‚VSk‚q‚v
031690                  ‰·ãª–@‰ñ”‚VSk‚q‚v ‰·ãª–@—¿‚VSk‚q‚v
031700                  “d—Ã‰ñ”‚VSk‚q‚v   “d—Ã—¿‚VSk‚q‚v
031710                  ¬Œv‚VSk‚q‚v.
031720*
031730     MOVE ZERO TO Œã—Ã‰ñ”‚WSk‚q‚v   Œã—Ã—¿‚WSk‚q‚v
031740                  —âãª–@‰ñ”‚WSk‚q‚v —âãª–@—¿‚WSk‚q‚v
031750                  ‰·ãª–@‰ñ”‚WSk‚q‚v ‰·ãª–@—¿‚WSk‚q‚v
031760                  “d—Ã‰ñ”‚WSk‚q‚v   “d—Ã—¿‚WSk‚q‚v
031770                  ¬Œv‚WSk‚q‚v.
031780*
031790     MOVE ZERO TO Œã—Ã‰ñ”‚XSk‚q‚v   Œã—Ã—¿‚XSk‚q‚v
031800                  —âãª–@‰ñ”‚XSk‚q‚v —âãª–@—¿‚XSk‚q‚v
031810                  ‰·ãª–@‰ñ”‚XSk‚q‚v ‰·ãª–@—¿‚XSk‚q‚v
031820                  “d—Ã‰ñ”‚XSk‚q‚v   “d—Ã—¿‚XSk‚q‚v
031830                  ¬Œv‚XSk‚q‚v.
031840*
031850     MOVE ZERO TO —âãª–@‰ñ”‚P‚q‚v —âãª–@‰ñ”‚Q‚q‚v —âãª–@‰ñ”‚R‚q‚v
031860                  —âãª–@‰ñ”‚S‚q‚v —âãª–@‰ñ”‚T‚q‚v —âãª–@‰ñ”‚U‚q‚v
031870                  —âãª–@‰ñ”‚V‚q‚v —âãª–@‰ñ”‚W‚q‚v —âãª–@‰ñ”‚X‚q‚v.
031880*
031890     MOVE ZERO TO ‰·ãª–@‰ñ”‚P‚q‚v ‰·ãª–@‰ñ”‚Q‚q‚v ‰·ãª–@‰ñ”‚R‚q‚v
031900                  ‰·ãª–@‰ñ”‚S‚q‚v ‰·ãª–@‰ñ”‚T‚q‚v ‰·ãª–@‰ñ”‚U‚q‚v
031910                  ‰·ãª–@‰ñ”‚V‚q‚v ‰·ãª–@‰ñ”‚W‚q‚v ‰·ãª–@‰ñ”‚X‚q‚v.
031920*
031930     MOVE ZERO TO “d—Ã‰ñ”‚P‚q‚v “d—Ã‰ñ”‚Q‚q‚v “d—Ã‰ñ”‚R‚q‚v
031940                  “d—Ã‰ñ”‚S‚q‚v “d—Ã‰ñ”‚T‚q‚v “d—Ã‰ñ”‚U‚q‚v
031950                  “d—Ã‰ñ”‚V‚q‚v “d—Ã‰ñ”‚W‚q‚v “d—Ã‰ñ”‚X‚q‚v.
031960*
031970     MOVE ZERO TO ‰ŒŸŽžŠÔŠO‚v ‰ŒŸ‹x“ú‚v   ‰ŒŸ[–é‚v 
031980                  ‰—Ã–éŠÔ‚v   ‰—Ã“ï˜H‚v   ‰—Ã–\•—‰J‚v ‰—ÃŽžŠÔŠO‚v
031990                  ‹à‘®•›Žq‘å‚v ‹à‘®•›Žq’†‚v ‹à‘®•›Žq¬‚v.
032000*
032010*/˜JÐê—pƒ[ƒN‚Ì‰Šú‰»
032020     MOVE ZERO TO ‰ŒŸ‰ñ”‚v      ÄŒŸ‰ñ”‚v      Žw“±ŠÇ—‰ñ”‚v
032030                  ‰^“®—Ã–@‰ñ”‚v  ‹x‹ÆØ–¾‰ñ”‚v  î•ñ’ñ‹Ÿ‰ñ”‚v
032040                  h”‘‰ñ”‚v      HŽ–‰ñ”‚v      ‰—Ã—¿‚v‚Q.
032050
032060*
032070*================================================================*
032080 Œ’•Û•‰’SŠzŒvŽZ SECTION.
032090*
032100****************************************************************
032110* •‰’S‹àŒvŽZ•û–@‚ÌŽw’è  •‰’S‹àŒvŽZ‹æ•ª  ”ï—pE¿‹Šz   •‰’SŠz  *
032120* @@@@@@@@@@@@@@‚O@@@@@‚P‰~’PˆÊ    ‚P‰~’PˆÊ *
032130*              @@@@ @@@‚P@@@@‚P‚O‰~’PˆÊ             *
032140*              @@@@@@@ ‚Q@@@@@‚P‰~’PˆÊ  ‚P‚O‰~’PˆÊ *
032150****************************************************************
032160*     MOVE ZERO TO ˜AŒv|•‰’S‹àŒvŽZ‹æ•ª
032170*
032180     CALL •‰’SŠz‚o‚f‚v
032190     IF PROGRAM-STATUS NOT = ZERO
032200         CANCEL •‰’SŠz‚o‚f‚v
032210*
032220     ELSE
032230         CANCEL •‰’SŠz‚o‚f‚v
032240*/ƒGƒ‰[•\Ž¦‚ÌC³
032250         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
032260         DISPLAY "•‰’SŠzŽæ“¾‚ÉŽ¸”s‚µ‚Ü‚µ‚½"
032270                 " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
032280                 " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
032290*-----------------------------------------*
032300         CALL "actcshm"  WITH C LINKAGE
032310*-----------------------------------------*
032320         ACCEPT  ƒL[“ü—Í FROM CONS
032330         PERFORM ƒtƒ@ƒCƒ‹•Â½
032340         EXIT PROGRAM
032350     END-IF.
032360*================================================================*
032370 ˜AŒ‹€–ÚƒZƒbƒg SECTION.
032380*
032390* ”ï—pŠzE•‰’SŠzE¿‹Šz‚ÍAŽåˆ—‚ÉƒZƒbƒg
032400*
032410* ‰Šú‰»ƒ‹[ƒ`ƒ“‚É‚Ä‰Šú‰»Ï
032420*
032430*/˜JÐ—p‚ÉV‚½‚É‰Á‚¦‚½€–Ú
032440     IF ‰ŒŸ‰ñ”‚v > ZERO
032450         MOVE ‰ŒŸ‰ñ”‚v     TO ˜A—¿˜JŽ©|‰ŒŸ‰ñ”
032460         MOVE ‰ŒŸ—¿‚b‚v     TO ˜A—¿˜JŽ©|‰ŒŸ’P‰¿
032470     END-IF.
032480     IF ÄŒŸ‰ñ”‚v > ZERO
032490         MOVE ÄŒŸ‰ñ”‚v     TO ˜A—¿˜JŽ©|ÄŒŸ‰ñ”
032500         MOVE ÄŒŸ—¿‚b‚v     TO ˜A—¿˜JŽ©|ÄŒŸ’P‰¿
032510     END-IF.
032520*
032530     MOVE ‰ŒŸ—¿‚f‚v     TO ˜A—¿˜JŽ©|‰ŒŸ—¿.
032540     MOVE ‰ŒŸ‰ÁŽZ—¿‚f‚v TO ˜A—¿˜JŽ©|‰ŒŸ‰ÁŽZ—¿.
032550     MOVE ÄŒŸ—¿‚f‚v     TO ˜A—¿˜JŽ©|ÄŒŸ—¿.
032560     MOVE ‰—Ã‹——£‚v     TO ˜A—¿˜JŽ©|‰—Ã‹——£.
032570     MOVE ‰—Ã‰ñ”‚f‚v   TO ˜A—¿˜JŽ©|‰—Ã‰ñ”.
032580*/‰—Ã’P‰¿‚ÍÅŒã‚É‰—Ã—¿‚ðŽZ’è‚µ‚½“ú‚Ì’P‰¿
032590     MOVE ‰—Ã—¿‚v‚Q     TO ˜A—¿˜JŽ©|‰—Ã’P‰¿.
032600     MOVE ‰—Ã—¿‚f‚v     TO ˜A—¿˜JŽ©|‰—Ã—¿.
032610     MOVE ‰—Ã‰ÁŽZ—¿‚f‚v TO ˜A—¿˜JŽ©|‰—Ã‰ÁŽZ—¿.
032620     MOVE ‹à‘®•›Žq—¿‚f‚v TO ˜A—¿˜JŽ©|‹à‘®•›Žq‰ÁŽZ—¿.
032630     MOVE ‰ŒŸŽž‘Š’k—¿‚f‚v TO ˜A—¿˜JŽ©|‰ŒŸŽž‘Š’k—¿.
032640*
032650     IF î•ñ’ñ‹Ÿ‰ñ”‚v > ZERO
032660         MOVE î•ñ’ñ‹Ÿ‰ñ”‚v     TO ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ‰ñ”
032670         MOVE Ž{pî•ñ’ñ‹Ÿ—¿‚b‚v TO ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ’P‰¿
032680         MOVE Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v TO ˜A—¿˜JŽ©|Ž{pî•ñ’ñ‹Ÿ—¿
032690     END-IF.
032700*
032710     MOVE ‰ŒŸŽžŠÔŠO‚v   TO ˜A—¿˜JŽ©|‰ŒŸŽžŠÔŠO.
032720     MOVE ‰ŒŸ‹x“ú‚v     TO ˜A—¿˜JŽ©|‰ŒŸ‹x“ú.  
032730     MOVE ‰ŒŸ[–é‚v     TO ˜A—¿˜JŽ©|‰ŒŸ[–é.  
032740     MOVE ‰—Ã–éŠÔ‚v     TO ˜A—¿˜JŽ©|‰—Ã–éŠÔ.  
032750     MOVE ‰—Ã“ï˜H‚v     TO ˜A—¿˜JŽ©|‰—Ã“ï˜H.  
032760     MOVE ‰—Ã–\•—‰J‚v   TO ˜A—¿˜JŽ©|‰—Ã–\•—‰J.
032770     MOVE ‰—ÃŽžŠÔŠO‚v   TO ˜A—¿˜JŽ©|‰—ÃŽžŠÔŠO.
032780*
032790     MOVE ‹à‘®•›Žq‘å‚v   TO ˜A—¿˜JŽ©|‘å.
032800     MOVE ‹à‘®•›Žq’†‚v   TO ˜A—¿˜JŽ©|’†.
032810     MOVE ‹à‘®•›Žq¬‚v   TO ˜A—¿˜JŽ©|¬.
032820*
032830     COMPUTE ˜A—¿˜JŽ©|¬Œv = ‰ŒŸ—¿‚f‚v + ‰ŒŸ‰ÁŽZ—¿‚f‚v +
032840                          ÄŒŸ—¿‚f‚v + ‰—Ã—¿‚f‚v +
032850                          ‰—Ã‰ÁŽZ—¿‚f‚v + ‹à‘®•›Žq—¿‚f‚v +
032860                          Ž{pî•ñ’ñ‹Ÿ—¿‚f‚v +
032870                          ‰ŒŸŽž‘Š’k—¿‚f‚v.
032880*
032890     IF Žw“±ŠÇ—‰ñ”‚v > ZERO
032900         MOVE Žw“±ŠÇ—‰ñ”‚v   TO ˜A—¿˜JŽ©|Žw“±ŠÇ—‰ñ”
032910         MOVE Žw“±ŠÇ——¿‚b‚v   TO ˜A—¿˜JŽ©|Žw“±ŠÇ—’P‰¿
032920         MOVE Žw“±ŠÇ——¿‚f‚v   TO ˜A—¿˜JŽ©|Žw“±ŠÇ——¿
032930     END-IF.
032940*
032950     IF ‰^“®—Ã–@‰ñ”‚v > ZERO
032960         MOVE ‰^“®—Ã–@‰ñ”‚v   TO ˜A—¿˜JŽ©|‰^“®—Ã–@‰ñ”
032970         MOVE ‰^“®—Ã–@—¿‚b‚v   TO ˜A—¿˜JŽ©|‰^“®—Ã–@’P‰¿
032980         MOVE ‰^“®—Ã–@—¿‚f‚v   TO ˜A—¿˜JŽ©|‰^“®—Ã–@—¿
032990     END-IF.
033000*
033010     IF ‹x‹ÆØ–¾‰ñ”‚v > ZERO
033020         MOVE ‹x‹ÆØ–¾‰ñ”‚v   TO ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‰ñ”
033030         MOVE ‹x‹ÆØ–¾‘—¿‚b‚v TO ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾’P‰¿
033040         MOVE ‹x‹ÆØ–¾‘—¿‚f‚v TO ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‘—¿
033050     END-IF.
033060*
033070     IF h”‘‰ñ”‚v > ZERO
033080         MOVE h”‘‰ñ”‚v       TO ˜A—¿˜JŽ©|h”‘‰ñ”
033090         MOVE h”‘—¿‚b‚v       TO ˜A—¿˜JŽ©|h”‘’P‰¿
033100         MOVE h”‘—¿‚f‚v       TO ˜A—¿˜JŽ©|h”‘—¿
033110     END-IF.
033120*
033130     IF HŽ–‰ñ”‚v > ZERO
033140         MOVE HŽ–‰ñ”‚v       TO ˜A—¿˜JŽ©|HŽ–‰ñ”
033150         MOVE HŽ–—¿‚b‚v       TO ˜A—¿˜JŽ©|HŽ–’P‰¿
033160         MOVE HŽ–—¿‚f‚v       TO ˜A—¿˜JŽ©|HŽ–—¿  
033170     END-IF.
033180*
033190     PERFORM VARYING •”ˆÊ‚b‚m‚s FROM 1 BY 1
033200             UNTIL   •”ˆÊ‚b‚m‚s > •”ˆÊ”‚v 
033210         MOVE ‰‰ñˆ’u—¿‚n‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|‰‰ñˆ’u—¿(•”ˆÊ‚b‚m‚s)
033220         COMPUTE ˜A—¿˜JŽ©|‰‰ñˆ’u—¿Œv = ˜A—¿˜JŽ©|‰‰ñˆ’u—¿Œv + ‰‰ñˆ’u—¿‚n‚v(•”ˆÊ‚b‚m‚s)
033230*/Þ—¿”ï
033240         MOVE Þ—¿”ï‚Q‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|Þ—¿”ï(•”ˆÊ‚b‚m‚s)
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚ÌŒðŠ·‚ÍA‚Q‰ñ‚Ü‚ÅŽZ’è‚Å‚«‚éB/20180830
026560         MOVE Þ—¿‰ñ”‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|Þ—¿”ï‰ñ”(•”ˆÊ‚b‚m‚s)
               MOVE Þ—¿”ï‚R‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|Þ—¿”ï’P‰¿(•”ˆÊ‚b‚m‚s)
033250*/•ï‘ÑŒðŠ·—¿
033260         MOVE •ï‘ÑŒðŠ·“ú‚P‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚P(•”ˆÊ‚b‚m‚s)
033270         MOVE •ï‘ÑŒðŠ·“ú‚Q‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚Q(•”ˆÊ‚b‚m‚s)
033280         MOVE •ï‘ÑŒðŠ·“ú‚R‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚R(•”ˆÊ‚b‚m‚s)
033290         MOVE •ï‘ÑŒðŠ·“ú‚S‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚S(•”ˆÊ‚b‚m‚s)
033300         MOVE •ï‘ÑŒðŠ·“ú‚T‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚T(•”ˆÊ‚b‚m‚s)
033310         MOVE •ï‘ÑŒðŠ·“ú‚U‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚U(•”ˆÊ‚b‚m‚s)
033320         MOVE •ï‘ÑŒðŠ·—¿‚v  (•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·—¿  (•”ˆÊ‚b‚m‚s)
033330*/˜JÐV—pŽ†‘Î‰ž(1704~)/0506
033340         MOVE •ï‘ÑŒðŠ·’P‰¿‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·’P‰¿(•”ˆÊ‚b‚m‚s)
033350         MOVE •ï‘ÑŒðŠ·‰ñ”‚v(•”ˆÊ‚b‚m‚s) TO ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·‰ñ”(•”ˆÊ‚b‚m‚s)
033360     END-PERFORM.
033370*
033380**********
033390* ‚P•”ˆÊ *
033400**********
033410     IF •”ˆÊ‡Œv‚v‚P > ZERO
033420         IF Œã—Ã‰ñ”‚P‚q‚v > ZERO
033430*             MOVE Œã—Ã’P‰¿‚n‚v(1, 1) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚P
033440             MOVE Œã—Ã’P‰¿‚n‚v(1)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚P
033450             MOVE Œã—Ã‰ñ”‚P‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚P
033460             MOVE Œã—Ã—¿‚P‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚P
033470         END-IF
033480         MOVE —âãª–@’P‰¿‚v(1)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚P
033490         MOVE ‰·ãª–@’P‰¿‚v(1)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚P
033500         MOVE “d—Ã’P‰¿‚v(1)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚P
033510         MOVE —âãª–@‰ñ”‚P‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚P
033520         MOVE —âãª–@—¿‚P‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚P
033530         MOVE ‰·ãª–@‰ñ”‚P‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚P
033540         MOVE ‰·ãª–@—¿‚P‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚P
033550         MOVE “d—Ã‰ñ”‚P‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚P
033560         MOVE “d—Ã—¿‚P‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚P
033570*/1•”ˆÊ–ÚSk
033580         IF Œã—Ã‰ñ”‚PSk‚q‚v > ZERO
033590             MOVE SkŒã—Ã’P‰¿‚n‚v(1) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚PSk
033600             MOVE Œã—Ã‰ñ”‚PSk‚q‚v  TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚PSk
033610             MOVE Œã—Ã—¿‚PSk‚q‚v    TO ˜A—¿˜JŽ©|Œã—Ã—¿‚PSk
033620         END-IF
033630         MOVE —âãª–@‰ñ”‚PSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚PSk
033640         MOVE —âãª–@—¿‚PSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚PSk
033650         MOVE ‰·ãª–@‰ñ”‚PSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚PSk
033660         MOVE ‰·ãª–@—¿‚PSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚PSk
033670         MOVE “d—Ã‰ñ”‚PSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚PSk
033680         MOVE “d—Ã—¿‚PSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚PSk
033690* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
033700         MOVE •”ˆÊ‡Œv‚v‚P       TO ˜A—¿˜JŽ©|¬Œv‚P
033710         IF ’·Šú‹æ•ª‚v(1) > ZERO
033720             MOVE ’·Šú’üŒ¸—¦‚j‚v     TO ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚P
033730         END-IF
033740         MOVE •”ˆÊ‡Œv’·Šú‚v‚P   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚P
033750     END-IF.
033760**********
033770* ‚Q•”ˆÊ *
033780**********
033790     IF •”ˆÊ‡Œv‚v‚Q > ZERO
033800         IF Œã—Ã‰ñ”‚Q‚q‚v > ZERO
033810*             MOVE Œã—Ã’P‰¿‚n‚v(1, 2) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚Q
033820*             MOVE Œã—Ã’P‰¿‚n‚v(2, 2) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚Q
033830             MOVE Œã—Ã’P‰¿‚n‚v(2)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚Q
033840             MOVE Œã—Ã‰ñ”‚Q‚q‚v TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚Q
033850             MOVE Œã—Ã—¿‚Q‚q‚v   TO ˜A—¿˜JŽ©|Œã—Ã—¿‚Q
033860         END-IF
033870         MOVE —âãª–@’P‰¿‚v(2)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚Q
033880         MOVE ‰·ãª–@’P‰¿‚v(2)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚Q
033890         MOVE “d—Ã’P‰¿‚v(2)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚Q
033900         MOVE —âãª–@‰ñ”‚Q‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚Q
033910         MOVE —âãª–@—¿‚Q‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚Q
033920         MOVE ‰·ãª–@‰ñ”‚Q‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚Q
033930         MOVE ‰·ãª–@—¿‚Q‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚Q
033940         MOVE “d—Ã‰ñ”‚Q‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚Q
033950         MOVE “d—Ã—¿‚Q‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚Q
033960*/‚Q•”ˆÊ–ÚSk
033970         IF Œã—Ã‰ñ”‚QSk‚q‚v > ZERO
033980             MOVE SkŒã—Ã’P‰¿‚n‚v(2) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚QSk
033990             MOVE Œã—Ã‰ñ”‚QSk‚q‚v  TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚QSk
034000             MOVE Œã—Ã—¿‚QSk‚q‚v    TO ˜A—¿˜JŽ©|Œã—Ã—¿‚QSk
034010         END-IF
034020         MOVE —âãª–@‰ñ”‚QSk‚q‚v    TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚QSk
034030         MOVE —âãª–@—¿‚QSk‚q‚v      TO ˜A—¿˜JŽ©|—âãª–@—¿‚QSk
034040         MOVE ‰·ãª–@‰ñ”‚QSk‚q‚v    TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚QSk
034050         MOVE ‰·ãª–@—¿‚QSk‚q‚v      TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚QSk
034060         MOVE “d—Ã‰ñ”‚QSk‚q‚v      TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚QSk
034070         MOVE “d—Ã—¿‚QSk‚q‚v        TO ˜A—¿˜JŽ©|“d—Ã—¿‚QSk
034080* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
034090         MOVE •”ˆÊ‡Œv‚v‚Q       TO ˜A—¿˜JŽ©|¬Œv‚Q
034100         IF ’·Šú‹æ•ª‚v(2) > ZERO
034110             MOVE ’·Šú’üŒ¸—¦‚j‚v TO ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚Q
034120         END-IF
034130         MOVE •”ˆÊ‡Œv’·Šú‚v‚Q   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚Q
034140     END-IF.
034150**********
034160* ‚R•”ˆÊ *
034170**********
034180     IF •”ˆÊ‡Œv‚v‚R > ZERO
034190         IF Œã—Ã‰ñ”‚R‚q‚v > ZERO
034200             MOVE Œã—Ã’P‰¿‚n‚v(3) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚R
034210             MOVE Œã—Ã‰ñ”‚R‚q‚v  TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚R
034220             MOVE Œã—Ã—¿‚R‚q‚v    TO ˜A—¿˜JŽ©|Œã—Ã—¿‚R
034230         END-IF
034240         MOVE —âãª–@’P‰¿‚v(3)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚R
034250         MOVE ‰·ãª–@’P‰¿‚v(3)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚R
034260         MOVE “d—Ã’P‰¿‚v(3)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚R
034270         MOVE —âãª–@‰ñ”‚R‚q‚v    TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚R
034280         MOVE —âãª–@—¿‚R‚q‚v      TO ˜A—¿˜JŽ©|—âãª–@—¿‚R
034290         MOVE ‰·ãª–@‰ñ”‚R‚q‚v    TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚R
034300         MOVE ‰·ãª–@—¿‚R‚q‚v      TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚R
034310         MOVE “d—Ã‰ñ”‚R‚q‚v      TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚R
034320         MOVE “d—Ã—¿‚R‚q‚v        TO ˜A—¿˜JŽ©|“d—Ã—¿‚R
034330*/‚R•”ˆÊ–ÚSk
034340         IF Œã—Ã‰ñ”‚RSk‚q‚v > ZERO
034350             MOVE SkŒã—Ã’P‰¿‚n‚v(3) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚RSk
034360             MOVE Œã—Ã‰ñ”‚RSk‚q‚v  TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚RSk
034370             MOVE Œã—Ã—¿‚RSk‚q‚v    TO ˜A—¿˜JŽ©|Œã—Ã—¿‚RSk
034380         END-IF
034390         MOVE —âãª–@‰ñ”‚RSk‚q‚v    TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚RSk
034400         MOVE —âãª–@—¿‚RSk‚q‚v      TO ˜A—¿˜JŽ©|—âãª–@—¿‚RSk
034410         MOVE ‰·ãª–@‰ñ”‚RSk‚q‚v    TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚RSk
034420         MOVE ‰·ãª–@—¿‚RSk‚q‚v      TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚RSk
034430         MOVE “d—Ã‰ñ”‚RSk‚q‚v      TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚RSk
034440         MOVE “d—Ã—¿‚RSk‚q‚v        TO ˜A—¿˜JŽ©|“d—Ã—¿‚RSk
034450* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
034460         MOVE •”ˆÊ‡Œv‚v‚R       TO ˜A—¿˜JŽ©|¬Œv‚R
034470         MOVE •”ˆÊ‡Œv’·Šú‚v‚R   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚R
034480     END-IF.
034490**********
034500* ‚S•”ˆÊ *
034510**********
034520     IF •”ˆÊ‡Œv‚v‚S > ZERO
034530         IF Œã—Ã‰ñ”‚S‚q‚v > ZERO
034540             MOVE Œã—Ã’P‰¿‚n‚v(4)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚S
034550             MOVE Œã—Ã‰ñ”‚S‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚S
034560             MOVE Œã—Ã—¿‚S‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚S
034570         END-IF
034580         MOVE —âãª–@’P‰¿‚v(4)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚S
034590         MOVE ‰·ãª–@’P‰¿‚v(4)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚S
034600         MOVE “d—Ã’P‰¿‚v(4)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚S
034610         MOVE —âãª–@‰ñ”‚S‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚S
034620         MOVE —âãª–@—¿‚S‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚S
034630         MOVE ‰·ãª–@‰ñ”‚S‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚S
034640         MOVE ‰·ãª–@—¿‚S‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚S
034650         MOVE “d—Ã‰ñ”‚S‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚S
034660         MOVE “d—Ã—¿‚S‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚S
034670*/‚S•”ˆÊ–ÚSk
034680         IF Œã—Ã‰ñ”‚SSk‚q‚v > ZERO
034690             MOVE SkŒã—Ã’P‰¿‚n‚v(4) TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚SSk
034700             MOVE Œã—Ã‰ñ”‚SSk‚q‚v  TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚SSk
034710             MOVE Œã—Ã—¿‚SSk‚q‚v    TO ˜A—¿˜JŽ©|Œã—Ã—¿‚SSk
034720         END-IF
034730         MOVE —âãª–@‰ñ”‚SSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚SSk
034740         MOVE —âãª–@—¿‚SSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚SSk
034750         MOVE ‰·ãª–@‰ñ”‚SSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚SSk
034760         MOVE ‰·ãª–@—¿‚SSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚SSk
034770         MOVE “d—Ã‰ñ”‚SSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚SSk
034780         MOVE “d—Ã—¿‚SSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚SSk
034790* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
034800         MOVE •”ˆÊ‡Œv‚v‚S           TO ˜A—¿˜JŽ©|¬Œv‚S
034810         MOVE •”ˆÊ‡Œv’·Šú‚v‚S       TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚S
034820     END-IF.
034830**********
034840* ‚T•”ˆÊ *
034850**********
034860     IF •”ˆÊ‡Œv‚v‚T > ZERO
034870         IF Œã—Ã‰ñ”‚T‚q‚v > ZERO
034880             MOVE Œã—Ã’P‰¿‚n‚v(5)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚T
034890             MOVE Œã—Ã‰ñ”‚T‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚T
034900             MOVE Œã—Ã—¿‚T‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚T
034910         END-IF
034920         MOVE —âãª–@’P‰¿‚v(5)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚T
034930         MOVE ‰·ãª–@’P‰¿‚v(5)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚T
034940         MOVE “d—Ã’P‰¿‚v(5)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚T
034950         MOVE —âãª–@‰ñ”‚T‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚T
034960         MOVE —âãª–@—¿‚T‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚T
034970         MOVE ‰·ãª–@‰ñ”‚T‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚T
034980         MOVE ‰·ãª–@—¿‚T‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚T
034990         MOVE “d—Ã‰ñ”‚T‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚T
035000         MOVE “d—Ã—¿‚T‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚T
035010*/‚T•”ˆÊ–ÚSk
035020         IF Œã—Ã‰ñ”‚TSk‚q‚v > ZERO
035030             MOVE SkŒã—Ã’P‰¿‚n‚v(5)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚TSk
035040             MOVE Œã—Ã‰ñ”‚TSk‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚TSk
035050             MOVE Œã—Ã—¿‚TSk‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚TSk
035060         END-IF
035070         MOVE —âãª–@‰ñ”‚TSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚TSk
035080         MOVE —âãª–@—¿‚TSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚TSk
035090         MOVE ‰·ãª–@‰ñ”‚TSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚TSk
035100         MOVE ‰·ãª–@—¿‚TSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚TSk
035110         MOVE “d—Ã‰ñ”‚TSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚TSk
035120         MOVE “d—Ã—¿‚TSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚TSk
035130* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
035140         MOVE •”ˆÊ‡Œv‚v‚T       TO ˜A—¿˜JŽ©|¬Œv‚T
035150         IF ’·Šú‹æ•ª‚v(5) > ZERO
035160             MOVE ’·Šú’üŒ¸—¦‚j‚v     TO ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚T
035170         END-IF
035180         MOVE •”ˆÊ‡Œv’·Šú‚v‚T   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚T
035190**********
035200* ‚U•”ˆÊ *
035210**********
035220     IF •”ˆÊ‡Œv‚v‚U > ZERO
035230         IF Œã—Ã‰ñ”‚U‚q‚v > ZERO
035240             MOVE Œã—Ã’P‰¿‚n‚v(6)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚U
035250             MOVE Œã—Ã‰ñ”‚U‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚U
035260             MOVE Œã—Ã—¿‚U‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚U
035270         END-IF
035280         MOVE —âãª–@’P‰¿‚v(6)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚U
035290         MOVE ‰·ãª–@’P‰¿‚v(6)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚U
035300         MOVE “d—Ã’P‰¿‚v(6)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚U
035310         MOVE —âãª–@‰ñ”‚U‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚U
035320         MOVE —âãª–@—¿‚U‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚U
035330         MOVE ‰·ãª–@‰ñ”‚U‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚U
035340         MOVE ‰·ãª–@—¿‚U‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚U
035350         MOVE “d—Ã‰ñ”‚U‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚U
035360         MOVE “d—Ã—¿‚U‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚U
035370*/‚U•”ˆÊ–ÚSk
035380         IF Œã—Ã‰ñ”‚USk‚q‚v > ZERO
035390             MOVE SkŒã—Ã’P‰¿‚n‚v(6)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚USk
035400             MOVE Œã—Ã‰ñ”‚USk‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚USk
035410             MOVE Œã—Ã—¿‚USk‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚USk
035420         END-IF
035430         MOVE —âãª–@‰ñ”‚USk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚USk
035440         MOVE —âãª–@—¿‚USk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚USk
035450         MOVE ‰·ãª–@‰ñ”‚USk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚USk
035460         MOVE ‰·ãª–@—¿‚USk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚USk
035470         MOVE “d—Ã‰ñ”‚USk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚USk
035480         MOVE “d—Ã—¿‚USk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚USk
035490* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
035500         MOVE •”ˆÊ‡Œv‚v‚U       TO ˜A—¿˜JŽ©|¬Œv‚U
035510         MOVE •”ˆÊ‡Œv’·Šú‚v‚U   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚U
035520**********
035530* ‚V•”ˆÊ *
035540**********
035550     IF •”ˆÊ‡Œv‚v‚V > ZERO
035560         IF Œã—Ã‰ñ”‚V‚q‚v > ZERO
035570             MOVE Œã—Ã’P‰¿‚n‚v(7)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚V
035580             MOVE Œã—Ã‰ñ”‚V‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚V
035590             MOVE Œã—Ã—¿‚V‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚V
035600         END-IF
035610         MOVE —âãª–@’P‰¿‚v(7)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚V
035620         MOVE ‰·ãª–@’P‰¿‚v(7)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚V
035630         MOVE “d—Ã’P‰¿‚v(7)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚V
035640         MOVE —âãª–@‰ñ”‚V‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚V
035650         MOVE —âãª–@—¿‚V‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚V
035660         MOVE ‰·ãª–@‰ñ”‚V‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚V
035670         MOVE ‰·ãª–@—¿‚V‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚V
035680         MOVE “d—Ã‰ñ”‚V‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚V
035690         MOVE “d—Ã—¿‚V‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚V
035700*/‚V•”ˆÊ–ÚSk
035710         IF Œã—Ã‰ñ”‚VSk‚q‚v > ZERO
035720             MOVE SkŒã—Ã’P‰¿‚n‚v(7)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚VSk
035730             MOVE Œã—Ã‰ñ”‚VSk‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚VSk
035740             MOVE Œã—Ã—¿‚VSk‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚VSk
035750         END-IF
035760         MOVE —âãª–@‰ñ”‚VSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚VSk
035770         MOVE —âãª–@—¿‚VSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚VSk
035780         MOVE ‰·ãª–@‰ñ”‚VSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚VSk
035790         MOVE ‰·ãª–@—¿‚VSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚VSk
035800         MOVE “d—Ã‰ñ”‚VSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚VSk
035810         MOVE “d—Ã—¿‚VSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚VSk
035820* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
035830         MOVE •”ˆÊ‡Œv‚v‚U       TO ˜A—¿˜JŽ©|¬Œv‚U
035840         MOVE •”ˆÊ‡Œv’·Šú‚v‚U   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚U
035850     END-IF.
035860*
035870**********
035880* ‚W•”ˆÊ *
035890**********
035900     IF •”ˆÊ‡Œv‚v‚W > ZERO
035910         IF Œã—Ã‰ñ”‚W‚q‚v > ZERO
035920             MOVE Œã—Ã’P‰¿‚n‚v(8)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚W
035930             MOVE Œã—Ã‰ñ”‚W‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚W
035940             MOVE Œã—Ã—¿‚W‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚W
035950         END-IF
035960         MOVE —âãª–@’P‰¿‚v(8)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚W
035970         MOVE ‰·ãª–@’P‰¿‚v(8)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚W
035980         MOVE “d—Ã’P‰¿‚v(8)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚W
035990         MOVE —âãª–@‰ñ”‚W‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚W
036000         MOVE —âãª–@—¿‚W‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚W
036010         MOVE ‰·ãª–@‰ñ”‚W‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚W
036020         MOVE ‰·ãª–@—¿‚W‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚W
036030         MOVE “d—Ã‰ñ”‚W‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚W
036040         MOVE “d—Ã—¿‚W‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚W
036050*/‚W•”ˆÊ–ÚSk
036060         IF Œã—Ã‰ñ”‚WSk‚q‚v > ZERO
036070             MOVE SkŒã—Ã’P‰¿‚n‚v(8)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚WSk
036080             MOVE Œã—Ã‰ñ”‚WSk‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚WSk
036090             MOVE Œã—Ã—¿‚WSk‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚WSk
036100         END-IF
036110         MOVE —âãª–@‰ñ”‚WSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚WSk
036120         MOVE —âãª–@—¿‚WSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚WSk
036130         MOVE ‰·ãª–@‰ñ”‚WSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚WSk
036140         MOVE ‰·ãª–@—¿‚WSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚WSk
036150         MOVE “d—Ã‰ñ”‚WSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚WSk
036160         MOVE “d—Ã—¿‚WSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚WSk
036170* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
036180         MOVE •”ˆÊ‡Œv‚v‚W       TO ˜A—¿˜JŽ©|¬Œv‚W
036190         MOVE •”ˆÊ‡Œv’·Šú‚v‚W   TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚W
036200     END-IF.
036210**********
036220* ‚X•”ˆÊ *
036230**********
036240     IF •”ˆÊ‡Œv‚v‚X > ZERO
036250         IF Œã—Ã‰ñ”‚X‚q‚v > ZERO
036260             MOVE Œã—Ã’P‰¿‚n‚v(9)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚X
036270             MOVE Œã—Ã‰ñ”‚X‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚X
036280             MOVE Œã—Ã—¿‚X‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚X
036290         END-IF
036300         MOVE —âãª–@’P‰¿‚v(9)    TO ˜A—¿˜JŽ©|—âãª–@’P‰¿‚X
036310         MOVE ‰·ãª–@’P‰¿‚v(9)    TO ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚X
036320         MOVE “d—Ã’P‰¿‚v(9)      TO ˜A—¿˜JŽ©|“d—Ã’P‰¿‚X
036330         MOVE —âãª–@‰ñ”‚X‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚X
036340         MOVE —âãª–@—¿‚X‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚X
036350         MOVE ‰·ãª–@‰ñ”‚X‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚X
036360         MOVE ‰·ãª–@—¿‚X‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚X
036370         MOVE “d—Ã‰ñ”‚X‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚X
036380         MOVE “d—Ã—¿‚X‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚X
036390*/‚X•”ˆÊ–ÚSk
036400         IF Œã—Ã‰ñ”‚XSk‚q‚v > ZERO
036410             MOVE SkŒã—Ã’P‰¿‚n‚v(9)    TO ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚XSk
036420             MOVE Œã—Ã‰ñ”‚XSk‚q‚v     TO ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚XSk
036430             MOVE Œã—Ã—¿‚XSk‚q‚v       TO ˜A—¿˜JŽ©|Œã—Ã—¿‚XSk
036440         END-IF
036450         MOVE —âãª–@‰ñ”‚XSk‚q‚v   TO ˜A—¿˜JŽ©|—âãª–@‰ñ”‚XSk
036460         MOVE —âãª–@—¿‚XSk‚q‚v     TO ˜A—¿˜JŽ©|—âãª–@—¿‚XSk
036470         MOVE ‰·ãª–@‰ñ”‚XSk‚q‚v   TO ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚XSk
036480         MOVE ‰·ãª–@—¿‚XSk‚q‚v     TO ˜A—¿˜JŽ©|‰·ãª–@—¿‚XSk
036490         MOVE “d—Ã‰ñ”‚XSk‚q‚v     TO ˜A—¿˜JŽ©|“d—Ã‰ñ”‚XSk
036500         MOVE “d—Ã—¿‚XSk‚q‚v       TO ˜A—¿˜JŽ©|“d—Ã—¿‚XSk
036510* ã‹L‚P‚O‚O“‚Ì‹àŠz‚ð‰ÁŽZ‚·‚é
036520         MOVE •”ˆÊ‡Œv‚v‚X           TO ˜A—¿˜JŽ©|¬Œv‚X
036530         MOVE •”ˆÊ‡Œv’·Šú‚v‚X       TO ˜A—¿˜JŽ©|’·Šúž¬Œv‚X
036540     END-IF.
036550*================================================================*
036560 §Œäî•ñŽæ“¾ SECTION.
036570*
036580     MOVE ZERO TO §|§Œä‹æ•ª.
036590     READ §Œäî•ñƒ}ƒXƒ^
036600     NOT INVALID KEY
036610         MOVE §|Å‘å“o˜^•”ˆÊ”   TO Å‘å“o˜^•”ˆÊ”‚v
036620         MOVE §|•¬•‰’SŒvŽZ‹æ•ª TO •¬•‰’SŒvŽZ‹æ•ª‚v
036630     END-READ.
036640*
036650*================================================================*
036660 ‘½•”ˆÊ‡Œvˆ— SECTION.
036670*
036680     COMPUTE •”ˆÊ‡Œv‚v‚P = Œã—Ã—¿‚P‚q‚v     +
036690                            Œã—Ã—¿‚PSk‚q‚v +
036700                            —âãª–@—¿‚P‚q‚v   +
036710                            ‰·ãª–@—¿‚P‚q‚v   +
036720                            “d—Ã—¿‚P‚q‚v.
036730*
036740     COMPUTE •”ˆÊ‡Œv‚v‚Q = Œã—Ã—¿‚Q‚q‚v     +
036750                            Œã—Ã—¿‚QSk‚q‚v +
036760                            —âãª–@—¿‚Q‚q‚v   +
036770                            ‰·ãª–@—¿‚Q‚q‚v   +
036780                            “d—Ã—¿‚Q‚q‚v.
036790*
036800     COMPUTE •”ˆÊ‡Œv‚v‚R = Œã—Ã—¿‚R‚q‚v     +
036810                            Œã—Ã—¿‚RSk‚q‚v +
036820                            —âãª–@—¿‚R‚q‚v   +
036830                            ‰·ãª–@—¿‚R‚q‚v   +
036840                            “d—Ã—¿‚R‚q‚v.
036850*
036860     COMPUTE •”ˆÊ‡Œv‚v‚S = Œã—Ã—¿‚S‚q‚v     +
036870                            Œã—Ã—¿‚SSk‚q‚v +
036880                            —âãª–@—¿‚S‚q‚v   +
036890                            ‰·ãª–@—¿‚S‚q‚v   +
036900                            “d—Ã—¿‚S‚q‚v.
036910*
036920     COMPUTE •”ˆÊ‡Œv‚v‚T = Œã—Ã—¿‚T‚q‚v     +
036930                            Œã—Ã—¿‚TSk‚q‚v +
036940                            —âãª–@—¿‚T‚q‚v   +
036950                            ‰·ãª–@—¿‚T‚q‚v   +
036960                            “d—Ã—¿‚T‚q‚v.
036970*
036980     COMPUTE •”ˆÊ‡Œv‚v‚U = Œã—Ã—¿‚U‚q‚v     +
036990                            Œã—Ã—¿‚USk‚q‚v +
037000                            —âãª–@—¿‚U‚q‚v   +
037010                            ‰·ãª–@—¿‚U‚q‚v   +
037020                            “d—Ã—¿‚U‚q‚v.
037030*
037040     COMPUTE •”ˆÊ‡Œv‚v‚V = Œã—Ã—¿‚V‚q‚v     +
037050                            Œã—Ã—¿‚VSk‚q‚v +
037060                            —âãª–@—¿‚V‚q‚v   +
037070                            ‰·ãª–@—¿‚V‚q‚v   +
037080                            “d—Ã—¿‚V‚q‚v.
037090*
037100     COMPUTE •”ˆÊ‡Œv‚v‚W = Œã—Ã—¿‚W‚q‚v     +
037110                            Œã—Ã—¿‚WSk‚q‚v +
037120                            —âãª–@—¿‚W‚q‚v   +
037130                            ‰·ãª–@—¿‚W‚q‚v   +
037140                            “d—Ã—¿‚W‚q‚v.
037150*
037160     COMPUTE •”ˆÊ‡Œv‚v‚X = Œã—Ã—¿‚X‚q‚v     +
037170                            Œã—Ã—¿‚XSk‚q‚v +
037180                            —âãª–@—¿‚X‚q‚v   +
037190                            ‰·ãª–@—¿‚X‚q‚v   +
037200                            “d—Ã—¿‚X‚q‚v.
037210*
037220**================================================================*
037230 Ž{p“úƒZƒbƒg SECTION.
037240*
037250* •¬•‰’S‚o‚f‚ÉŽ{p“ú‚ð˜AŒ‹‚·‚é
037260     MOVE Ž{‹L|Ž{p“ú TO ˜AŒv|Ž{p“ú.
037270*
037280*================================================================*
037290 ‰ŒŸ¿‹‹æ•ªŽæ“¾ SECTION.
037300*
037310* ‰ŒŸ—¿EÄŒŸ—¿‚ð¿‹‚·‚é‚©‚Ìƒtƒ‰ƒOŽæ“¾
037320     MOVE ˜AŒv|Ž{p˜a—ï TO •‰|Ž{p˜a—ï. 
037330     MOVE ˜AŒv|Ž{p”N   TO •‰|Ž{p”N.   
037340     MOVE ˜AŒv|Ž{pŒŽ   TO •‰|Ž{pŒŽ.   
037350     MOVE ˜AŒv|Š³ŽÒ”Ô† TO •‰|Š³ŽÒ”Ô†. 
037360     MOVE ˜AŒv|Ž}”Ô     TO •‰|Ž}”Ô.     
037370     READ •‰ƒf[ƒ^‚e
037380     INVALID KEY
037390         DISPLAY NC"ŠY“–ŒŽ‚Ìî•ñ‚ª‚ ‚è‚Ü‚¹‚ñ" UPON CONS
037400         DISPLAY NC"Ž{p˜a—ï" ˜AŒv|Ž{p˜a—ï  UPON CONS
037410         DISPLAY NC"Ž{p”N"   ˜AŒv|Ž{p”N    UPON CONS 
037420         DISPLAY NC"Ž{pŒŽ"   ˜AŒv|Ž{pŒŽ    UPON CONS 
037430         DISPLAY NC"Š³ŽÒ”Ô†" ˜AŒv|Š³ŽÒ”Ô†  UPON CONS
037440         DISPLAY NC"Ž}”Ô"     ˜AŒv|Ž}”Ô      UPON CONS 
037450         MOVE NC"•‰" TO ƒtƒ@ƒCƒ‹–¼‚v
037460         PERFORM ƒGƒ‰[•\Ž¦‚q
037470     NOT INVALID KEY
037480         MOVE •‰|‰ŒŸ¿‹‹æ•ª TO ‰ŒŸ¿‹‹æ•ª‚v
037490     END-READ.
037500*
037510*================================================================*
037520 ’·Šú’üŒ¸‹æ•ªŽæ“¾ SECTION.
037530*
037540     MOVE ˜AŒv|Ž{p˜a—ï TO •‰|Ž{p˜a—ï.
037550     MOVE ˜AŒv|Ž{p”N   TO •‰|Ž{p”N.
037560     MOVE ˜AŒv|Ž{pŒŽ   TO •‰|Ž{pŒŽ.
037570     MOVE ˜AŒv|Š³ŽÒ”Ô† TO •‰|Š³ŽÒ”Ô†.
037580     MOVE ˜AŒv|Ž}”Ô     TO •‰|Ž}”Ô.
037590     READ •‰ƒf[ƒ^‚e
037600     INVALID KEY
037610*/ƒGƒ‰[•\Ž¦‚ÌC³
037620         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
037630         DISPLAY "Žw’è‚ÌŽófŽÒ‚Ì•‰ƒf[ƒ^‚ª‚ ‚è‚Ü‚¹‚ñ"
037640                 " ŽófŽÒ‡‚="   ˜AŒv|Š³ŽÒƒR[ƒh
037650                 " Ž{p”NŒŽ="   ˜AŒv|Ž{p”N ˜AŒv|Ž{pŒŽ UPON CONS
037660*-----------------------------------------*
037670         CALL "actcshm"  WITH C LINKAGE
037680*-----------------------------------------*
037690         ACCEPT  ƒL[“ü—Í FROM CONS
037700         PERFORM ƒtƒ@ƒCƒ‹•Â½
037710         MOVE ZERO TO PROGRAM-STATUS
037720         EXIT PROGRAM
037730     NOT INVALID KEY
037740         MOVE •‰|•”ˆÊ” TO •”ˆÊ”‚v
037750     END-READ.
037760*
037770     PERFORM VARYING •”ˆÊƒJƒEƒ“ƒ^‚Q FROM 1 BY 1
037780               UNTIL •”ˆÊƒJƒEƒ“ƒ^‚Q > •”ˆÊ”‚v
037790         MOVE ZERO TO ’·Šú‹æ•ª‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037800     END-PERFORM.
037810*
037820     PERFORM VARYING •”ˆÊƒJƒEƒ“ƒ^‚Q FROM 1 BY 1
037830              UNTIL  •”ˆÊƒJƒEƒ“ƒ^‚Q > •”ˆÊ”‚v
037840*
037850* •‰|I—¹˜a—ï”NŒŽ‚ª–¢“ü—Í‚Ìê‡‚ÍAALL 9 ‚ðƒZƒbƒg‚·‚é
037860* ‚O‚PF”PÁ@‚O‚QF‘Å–o@‚O‚RFÁ‚Ì‚Ý’·Šú’üŒ¸‚ ‚è
037870         IF •‰|•‰Ží•Ê(•”ˆÊƒJƒEƒ“ƒ^‚Q) = 01 OR 02 OR 03
037880             PERFORM ŠJŽnI—¹“úŽæ“¾
037890             PERFORM ”NŒŽâ‘Î’lŒvŽZ
037900             IF ’·Šúƒtƒ‰ƒO NOT = SPACE 
037910                 MOVE 1 TO ’·Šú‹æ•ª‚v(•”ˆÊƒJƒEƒ“ƒ^‚Q)
037920             END-IF
037930         END-IF
037940     END-PERFORM.
037950*
037960**================================================================*
037970 ‰^“®—Ã–@—¿ŽZ’è SECTION.
037980*
037990     MOVE ZERO TO ‰^“®—Ã–@—¿‚v.
038000     IF Ž{‹L|‰^“®—Ã–@‹æ•ª = 1
038010         COMPUTE ‰^“®—Ã–@‰ñ”‚v = ‰^“®—Ã–@‰ñ”‚v + 1
038020         MOVE ‰^“®—Ã–@—¿‚b‚v TO ‰^“®—Ã–@—¿‚v
038030     END-IF.
038040*================================================================*
038050 Žw“±ŠÇ——¿ŽZ’è SECTION.
038060*
038070     MOVE ZERO TO Žw“±ŠÇ——¿‚v.
038080     IF Ž{‹L|Žw“±ŠÇ—‹æ•ª‚Q = 1
038090         COMPUTE Žw“±ŠÇ—‰ñ”‚v = Žw“±ŠÇ—‰ñ”‚v + 1
038100         MOVE Žw“±ŠÇ——¿‚b‚v TO Žw“±ŠÇ——¿‚v
038110     END-IF.
038120*================================================================*
038130 h”‘—¿ŽZ’è SECTION.
038140*
038150     MOVE ZERO TO h”‘—¿‚v.
038160     IF Ž{‹L|h”‘‹æ•ª = 1
038170         COMPUTE h”‘‰ñ”‚v = h”‘‰ñ”‚v + 1
038180         MOVE h”‘—¿‚b‚v TO h”‘—¿‚v
038190     END-IF.
038200*================================================================*
038210 HŽ–—¿ŽZ’è SECTION.
038220*
038230     MOVE ZERO TO HŽ–—¿‚v.
038240*/HŽ–—¿‚ð•¡”‰ñŽZ’èo—ˆ‚é‚æ‚¤‚É•ÏX««/060802
038250*     IF Ž{‹L|HŽ–‹æ•ª = 1
038260*         COMPUTE HŽ–‰ñ”‚v = HŽ–‰ñ”‚v + 1
038270*         MOVE HŽ–—¿‚b‚v TO HŽ–—¿‚v
038280*     END-IF.
038290      COMPUTE HŽ–‰ñ”‚v = HŽ–‰ñ”‚v + Ž{‹L|HŽ–‹æ•ª.
038300      COMPUTE HŽ–—¿‚v = HŽ–—¿‚b‚v * Ž{‹L|HŽ–‹æ•ª.
038310*================================================================*
038320 ‹x‹ÆØ–¾‘—¿ŽZ’è SECTION.
038330*
038340     MOVE ZERO TO ‹x‹ÆØ–¾‘—¿‚v.
038350*/˜JÐ‚Í‹x‹ÆØ–¾‚Æ‚µ‚ÄŽg—p
038360*/‚P“ú‚É•¡”‰ñ‚ÌŽZ’è‚ào—ˆ‚é‚æ‚¤‚É•ÏX
038370*     IF Ž{‹L|Ž{pØ–¾‹æ•ª = 1
038380*         MOVE ‹x‹ÆØ–¾‘—¿‚b‚v TO ‹x‹ÆØ–¾‘—¿‚v
038390*     END-IF.
038400     COMPUTE ‹x‹ÆØ–¾‰ñ”‚v = ‹x‹ÆØ–¾‰ñ”‚v   + Ž{‹L|Ž{pØ–¾‹æ•ª.
038410     COMPUTE ‹x‹ÆØ–¾‘—¿‚v = ‹x‹ÆØ–¾‘—¿‚b‚v * Ž{‹L|Ž{pØ–¾‹æ•ª.
038420*================================================================*
038430 ¼—ï”NŽæ“¾ SECTION.
038440*
038450     MOVE ŒvŽZ˜a—ï‚v TO Œ³|Œ³†‹æ•ª.
038460     READ Œ³†ƒ}ƒXƒ^
038470     INVALID KEY
038480         DISPLAY NC"Žw’è˜a—ï‚ª“o˜^‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ" UPON CONS
038490         DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢" UPON CONS
038500*-----------------------------------------*
038510         CALL "actcshm"  WITH C LINKAGE
038520*-----------------------------------------*
038530         ACCEPT  ƒL[“ü—Í FROM CONS
038540         PERFORM I—¹ˆ—
038550         EXIT PROGRAM
038560     NOT INVALID KEY
038570         COMPUTE ¼—ï”N‚v = Œ³|ŠJŽn¼—ï”N + ŒvŽZ”N‚v - 1
038580     END-READ.
038590*
038600*================================================================*
038610 “ú•tâ‘Î’lŒvŽZ SECTION.
038620*
038630** 1989”N‚ðŠî€‚Æ‚µA“–”N‚Ü‚Å‰[”N‚ª‰½‰ñ‚ ‚é‚©‚ðŒvŽZ
038640** Œ³†ƒ}ƒXƒ^‚Ì¼—ïŠJŽn”NŠm”FB
038650*     COMPUTE ¼—ï”N·‚v   = ¼—ï”N‚v - 1989 - 1.
038660*     DIVIDE 4 INTO ¼—ï”N·‚v GIVING ‰[”N‰ñ”‚v
038670*                            REMAINDER è—]‚v
038680*     END-DIVIDE.
038690*/Šî€”N1989”N‚©‚ç“–”N‚Ü‚Å‚Ì‰[”N‚Ì‰ñ”‚ðŒvŽZ(“–”N‚ÍŠÜ‚Ü‚È‚¢)
038700*/Žn‚ß‚Ì‚¤‚é‚¤”N‚Ì1992”N‚©‚ç”‚¦‚é
038710     COMPUTE ¼—ï”N·‚v   = ¼—ï”N‚v - 1989 - 1.
038720     MOVE 1992 TO ‰ÁŽZ”N‚v.
038730     MOVE ZERO TO ‰[”N‰ñ”‚v.
038740     PERFORM UNTIL ¼—ï”N‚v <= ‰ÁŽZ”N‚v
038750         COMPUTE ‰ÁŽZ”N‚v   = ‰ÁŽZ”N‚v   + 4
038760         COMPUTE ‰[”N‰ñ”‚v = ‰[”N‰ñ”‚v + 1
038770     END-PERFORM.
038780*
038790     COMPUTE ’Êí”N‰ñ”‚v     = ¼—ï”N·‚v     - ‰[”N‰ñ”‚v.
038800     COMPUTE ’Êí”Nâ‘Î’l‚v   = ’Êí”N‰ñ”‚v   * 365.
038810     COMPUTE ‰[”Nâ‘Î’l‚v     = ‰[”N‰ñ”‚v     * 366.
038820     COMPUTE ”Nâ‘Î’l‚v       = ’Êí”Nâ‘Î’l‚v + ‰[”Nâ‘Î’l‚v.
038830* “–”N‚ª‚¤‚é‚¤”N‚©‚Ç‚¤‚©‚ð’²‚×‚éi‚QŒŽ‚Ì––“úŒvŽZj
038840*     COMPUTE ¼—ï”N·‚Q‚v     = ¼—ï”N‚v + 1.
038850*     DIVIDE 4 INTO ¼—ï”N·‚Q‚v GIVING ‰[”N‰ñ”‚v
038860*                                REMAINDER è—]‚v
038870*     END-DIVIDE.
038880     DIVIDE 4 INTO ¼—ï”N‚v GIVING ‰[”N‰ñ”‚v
038890                            REMAINDER è—]‚v
038900     END-DIVIDE.
038910*
038920     MOVE ZERO TO â‘Î’lŒŽ‚v.
038930*
038940* ‘OŒŽ‚Ü‚Å‚Ìâ‘Î’l‚ð—ÝŒv
038950     COMPUTE â‘Î’lŒŽ‚v‚Q = ŒvŽZŒŽ‚v * 30.
038960*
038970     EVALUATE ŒvŽZŒŽ‚v
038980     WHEN 3
038990         COMPUTE â‘Î’lŒŽ‚v   = â‘Î’lŒŽ‚v‚Q - 1
039000     WHEN 2
039010     WHEN 6
039020     WHEN 7
039030         COMPUTE â‘Î’lŒŽ‚v   = â‘Î’lŒŽ‚v‚Q + 1
039040     WHEN 8
039050         COMPUTE â‘Î’lŒŽ‚v   = â‘Î’lŒŽ‚v‚Q + 2
039060     WHEN 9
039070     WHEN 10
039080         COMPUTE â‘Î’lŒŽ‚v   = â‘Î’lŒŽ‚v‚Q + 3
039090     WHEN 11
039100     WHEN 12
039110         COMPUTE â‘Î’lŒŽ‚v   = â‘Î’lŒŽ‚v‚Q + 4
039120     WHEN OTHER
039130         MOVE â‘Î’lŒŽ‚v‚Q TO â‘Î’lŒŽ‚v
039140     END-EVALUATE.
039150*
039160     IF ( è—]‚v = ZERO ) AND
039170        ( ŒvŽZŒŽ‚v > 2  )
039180         COMPUTE â‘Î’lŒŽ‚v = â‘Î’lŒŽ‚v + 1
039190     END-IF.
039200*
039210     COMPUTE â‘Î’l‚v   = ”Nâ‘Î’l‚v + â‘Î’lŒŽ‚v + ŒvŽZ“ú‚v.
039220*
039230*================================================================*
039240 ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F SECTION.
039250*
039260     IF ƒGƒ‰[•\Ž¦‚e = ZERO
039270         MOVE  "ƒGƒ‰[‚ª”­¶‚µ‚½‚Ì‚ÅƒRƒ“ƒ\[ƒ‹‰æ–Ê‚Ì“à—e" TO ˜Aƒ‚V|ƒƒbƒZ[ƒW‚P
039280         MOVE  "‚ðŠm”F‚µAC³‚µ‚Ä‚­‚¾‚³‚¢B"             TO ˜Aƒ‚V|ƒƒbƒZ[ƒW‚Q
039290         CALL   "MSG007"
039300         CANCEL "MSG007"
039310     END-IF.
039320     MOVE 1 TO ƒGƒ‰[•\Ž¦‚e.
039330*================================================================*
039340 “–ŒŽ‰ïŒvƒtƒ@ƒCƒ‹íœ SECTION.
039350*
039360     MOVE ˜AŒv|Š³ŽÒƒR[ƒh     TO ‰ï|Š³ŽÒƒR[ƒh    .
039370     MOVE ˜AŒv|Ž{p˜a—ï       TO ‰ï|Ž{p˜a—ï      .
039380     MOVE ˜AŒv|Ž{p”N         TO ‰ï|Ž{p”N        .
039390     MOVE ˜AŒv|Ž{pŒŽ         TO ‰ï|Ž{pŒŽ        .
039400     MOVE ZERO                 TO ‰ï|Ž{p“ú        .
039410     START ‰ïŒvƒf[ƒ^‚e KEY IS >= ‰ï|Š³ŽÒƒR[ƒh ‰ï|Ž{p˜a—ï”NŒŽ“ú
039420     END-START.
039430     IF ó‘ÔƒL[ = "00"
039440         MOVE SPACE TO I—¹ƒtƒ‰ƒO
039450         PERFORM ‰ïŒvƒf[ƒ^‚e“Çž
039460         PERFORM UNTIL (˜AŒv|Š³ŽÒƒR[ƒh NOT = ‰ï|Š³ŽÒƒR[ƒh) OR
039470                       (˜AŒv|Ž{p˜a—ï   NOT = ‰ï|Ž{p˜a—ï  ) OR
039480                       (˜AŒv|Ž{p”N     NOT = ‰ï|Ž{p”N    ) OR
039490                       (˜AŒv|Ž{pŒŽ     NOT = ‰ï|Ž{pŒŽ    ) OR
039500                       (I—¹ƒtƒ‰ƒO       NOT = SPACE         )
039510             DELETE ‰ïŒvƒf[ƒ^‚e
039520             INVALID KEY
039530                 MOVE NC"‰ïŒv" TO ƒtƒ@ƒCƒ‹–¼‚v
039540                 PERFORM ƒGƒ‰[•\Ž¦‚c
039550             END-DELETE
039560             PERFORM ‰ïŒvƒf[ƒ^‚e“Çž
039570         END-PERFORM
039580     END-IF.
039590
039600*================================================================*
039610 ƒGƒ‰[•\Ž¦‚c SECTION.
039620*
039630     DISPLAY ƒtƒ@ƒCƒ‹–¼‚v NC"ƒtƒ@ƒCƒ‹íœƒGƒ‰["   UPON CONS.
039640     DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[               UPON CONS.
039650     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
039660     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢" UPON CONS.
039670*-----------------------------------------*
039680         CALL "actcshm"  WITH C LINKAGE
039690*-----------------------------------------*
039700     ACCEPT  ƒL[“ü—Í FROM CONS.
039710     PERFORM ƒtƒ@ƒCƒ‹•Â½.
039720     EXIT PROGRAM.
039730*================================================================*
039740 ‰ïŒvƒf[ƒ^‚e“Çž SECTION.
039750*
039760     READ ‰ïŒvƒf[ƒ^‚e NEXT
039770     AT END
039780         MOVE SPACE TO ‰ï|ƒŒƒR[ƒh
039790         MOVE "YES" TO I—¹ƒtƒ‰ƒO
039800     END-READ.
039810*================================================================*
039820 ‰ïŒvƒf[ƒ^‚e‘ž SECTION.
039830*
039840     MOVE SPACE TO ‰ï‚O‚Q|ƒŒƒR[ƒh.
039850     INITIALIZE    ‰ï‚O‚Q|ƒŒƒR[ƒh.
039860     MOVE ˜AŒv|Š³ŽÒƒR[ƒh     TO ‰ï‚O‚Q|Š³ŽÒƒR[ƒh.
039870     MOVE ˜AŒv|Ž{p˜a—ï       TO ‰ï‚O‚Q|Ž{p˜a—ï.
039880     MOVE ˜AŒv|Ž{p”N         TO ‰ï‚O‚Q|Ž{p”N.
039890     MOVE ˜AŒv|Ž{pŒŽ         TO ‰ï‚O‚Q|Ž{pŒŽ.
039900     MOVE ˜AŒv|Ž{p“ú         TO ‰ï‚O‚Q|Ž{p“ú.
039910*
039920     COMPUTE ‰ï|”ï—pŠz = ‰ŒŸ—¿‚v + ‰ŒŸ‰ÁŽZ—¿‚v + ÄŒŸ—¿‚v + ‰—Ã—¿‚v + ‰—Ã‰ÁŽZ—¿‚v +
039930             ‰ŒŸŽž‘Š’k—¿‚v + ‹à‘®•›Žq—¿‚v + ‰‰ñˆ’u—¿‚v + Ž{pî•ñ’ñ‹Ÿ—¿‚v + Žw“±ŠÇ——¿‚v +
039940             “Á•ÊÞ—¿”ï‚v + ŒðŠ·•ï‘Ñ—¿‚v + ‰^“®—Ã–@—¿‚v + h”‘—¿‚v + HŽ–—¿‚v + ‹x‹ÆØ–¾‘—¿‚v +
039950             Œã—Ã—¿‚P‚m‚q‚v + Œã—Ã—¿‚PSk‚m‚q‚v + —âãª–@—¿‚P‚m‚q‚v + ‰·ãª–@—¿‚P‚m‚q‚v + “d—Ã—¿‚P‚m‚q‚v +
039960             Œã—Ã—¿‚Q‚m‚q‚v + Œã—Ã—¿‚QSk‚m‚q‚v + —âãª–@—¿‚Q‚m‚q‚v + ‰·ãª–@—¿‚Q‚m‚q‚v + “d—Ã—¿‚Q‚m‚q‚v +
039970             Œã—Ã—¿‚R‚m‚q‚v + Œã—Ã—¿‚RSk‚m‚q‚v + —âãª–@—¿‚R‚m‚q‚v + ‰·ãª–@—¿‚R‚m‚q‚v + “d—Ã—¿‚R‚m‚q‚v +
039980             Œã—Ã—¿‚S‚m‚q‚v + Œã—Ã—¿‚SSk‚m‚q‚v + —âãª–@—¿‚S‚m‚q‚v + ‰·ãª–@—¿‚S‚m‚q‚v + “d—Ã—¿‚S‚m‚q‚v +
039990             Œã—Ã—¿‚T‚m‚q‚v + Œã—Ã—¿‚TSk‚m‚q‚v + —âãª–@—¿‚T‚m‚q‚v + ‰·ãª–@—¿‚T‚m‚q‚v + “d—Ã—¿‚T‚m‚q‚v +
040000             Œã—Ã—¿‚U‚m‚q‚v + Œã—Ã—¿‚USk‚m‚q‚v + —âãª–@—¿‚U‚m‚q‚v + ‰·ãª–@—¿‚U‚m‚q‚v + “d—Ã—¿‚U‚m‚q‚v +
040010             Œã—Ã—¿‚V‚m‚q‚v + Œã—Ã—¿‚VSk‚m‚q‚v + —âãª–@—¿‚V‚m‚q‚v + ‰·ãª–@—¿‚V‚m‚q‚v + “d—Ã—¿‚V‚m‚q‚v +
040020             Œã—Ã—¿‚W‚m‚q‚v + Œã—Ã—¿‚WSk‚m‚q‚v + —âãª–@—¿‚W‚m‚q‚v + ‰·ãª–@—¿‚W‚m‚q‚v + “d—Ã—¿‚W‚m‚q‚v +
040030             Œã—Ã—¿‚X‚m‚q‚v + Œã—Ã—¿‚XSk‚m‚q‚v + —âãª–@—¿‚X‚m‚q‚v + ‰·ãª–@—¿‚X‚m‚q‚v + “d—Ã—¿‚X‚m‚q‚v.
040040*
040050***
040060*
040070*---------------------------------------------------------------------------------------*
040080* ¦ ã‹LŒãA“ü‹àŠz‚ÍA‰ïŒv—ÌŽûF‚É‚ ‚ê‚ÎƒZƒbƒg
040090     MOVE ZERO               TO —Ì|Ž{p‹æ•ª.
040100* 1:“úA2:ŒŽA3:”N
040110     MOVE 1                  TO —Ì|‰ïŒv—ÌŽû‹æ•ª.
040120     MOVE Ž{‹L|Ž{p˜a—ï     TO —Ì|Ž{p˜a—ï.
040130     MOVE Ž{‹L|Ž{p”N       TO —Ì|Ž{p”N.
040140     MOVE Ž{‹L|Ž{pŒŽ       TO —Ì|Ž{pŒŽ.
040150     MOVE Ž{‹L|Ž{p“ú       TO —Ì|Ž{p“ú.
040160     MOVE Ž{‹L|Š³ŽÒ”Ô†     TO —Ì|Š³ŽÒ”Ô†.
040170     MOVE Ž{‹L|Ž}”Ô         TO —Ì|Ž}”Ô.
040180     READ ‰ïŒv—ÌŽû‚e
040190     NOT INVALID KEY
040200        MOVE —Ì|“ü‹àŠz      TO ‰ï|“ü‹àŠz
040210        MOVE —Ì|Ž©”ïŠz      TO ‰ï|Ž©”ïŠz
040220     END-READ.
040230*---------------------------------------------------------------------------------------*
040240
040250
040260*
           PERFORM ‰ïŒv‚O‚QƒŒƒR[ƒhƒZƒbƒg.
040270***
040280     WRITE ‰ï‚O‚Q|ƒŒƒR[ƒh
040290     INVALID KEY
040300         MOVE NC"‰ï" TO ƒtƒ@ƒCƒ‹–¼‚v
040310         PERFORM ƒGƒ‰[•\Ž¦
040320         PERFORM I—¹ˆ—
040330         EXIT PROGRAM
040340     END-WRITE.
040350*================================================================*
040360 ƒŒƒZƒvƒgƒtƒ@ƒCƒ‹ì¬ SECTION.
040370*
040380     PERFORM ŽófŽÒî•ñŽæ“¾.
040390*    / ˜JÐ /
040400     MOVE  4   TO ƒŒƒZŽí•Ê‚v.
040410*
040420     PERFORM ƒŒƒR[ƒh‘¶Ýƒ`ƒFƒbƒN.
040430     PERFORM ‰ŒŸ“úˆÈ‘O‚Ìƒf[ƒ^”»’è.
040440*
040450*   / ˜JÐEŽ©”…ÓEŽ©”ï /
040460     PERFORM ‚»‚Ì‘¼ƒŒƒR[ƒhƒZƒbƒg.
040470     PERFORM ƒtƒ@ƒCƒ‹‘ž.
040480*
040490     MOVE ˜AŒv|Ž{p˜a—ï                TO ƒŒƒZ˜JŽ©|Ž{p˜a—ï.
040500     MOVE ˜AŒv|Ž{p”N                  TO ƒŒƒZ˜JŽ©|Ž{p”N.
040510     MOVE ˜AŒv|Ž{pŒŽ                  TO ƒŒƒZ˜JŽ©|Ž{pŒŽ.
040520*
040530     MOVE ˜AŒv|Š³ŽÒ”Ô†                TO ƒŒƒZ˜JŽ©|Š³ŽÒ”Ô†.
040540     MOVE ˜AŒv|Ž}”Ô                    TO ƒŒƒZ˜JŽ©|Ž}”Ô.
040550*   / 1:ˆê”Ê,2:˜Vl,3:•¬,4:˜JÐ,5:Ž©”…Ó,6:Ž©”ï,7:¶•Û’P“Æ,8•ÛŒ¯Ø–Y‚ê /
040560     MOVE ƒŒƒZŽí•Ê‚v                    TO ƒŒƒZ˜JŽ©|ƒŒƒZŽí•Ê.
040570     READ ƒŒƒZƒvƒg˜JŽ©‚e
040580     INVALID KEY
040590         PERFORM ƒŒƒZƒvƒg˜JŽ©ƒŒƒR[ƒhƒZƒbƒg
040600         WRITE ƒŒƒZ˜JŽ©|ƒŒƒR[ƒh
040610         IF ó‘ÔƒL[ NOT = "00"
040620            MOVE NC"ƒŒƒZXV" TO ƒtƒ@ƒCƒ‹–¼‚v
040630            PERFORM ƒGƒ‰[•\Ž¦
040640         END-IF
040650     NOT INVALID KEY
040660         PERFORM ƒŒƒZƒvƒg˜JŽ©ƒŒƒR[ƒhƒZƒbƒg
040670         REWRITE ƒŒƒZ˜JŽ©|ƒŒƒR[ƒh
040680         IF ó‘ÔƒL[ NOT = "00"
040690            MOVE NC"ƒŒƒZXV" TO ƒtƒ@ƒCƒ‹–¼‚v
040700            PERFORM ƒGƒ‰[•\Ž¦
040710         END-IF
040720     END-READ.
040730     PERFORM ŽófŽÒ_®’Ê‰@‚ ‚èXV.
040740*================================================================*
040750 ‚»‚Ì‘¼ƒŒƒR[ƒhƒZƒbƒg SECTION.
040760*
040770********************
040780* ŽófŽÒî•ñƒZƒbƒg *
040790********************
040800     MOVE ˜AŒv|Ž{p˜a—ï      TO ƒŒƒZ|Ž{p˜a—ï.
040810     MOVE ˜AŒv|Ž{p”N        TO ƒŒƒZ|Ž{p”N.
040820     MOVE ˜AŒv|Ž{pŒŽ        TO ƒŒƒZ|Ž{pŒŽ.
040830*
040840     MOVE ˜AŒv|Š³ŽÒ”Ô†      TO ƒŒƒZ|Š³ŽÒ”Ô†.
040850     MOVE ˜AŒv|Ž}”Ô          TO ƒŒƒZ|Ž}”Ô.
040860*
040870*  / ƒL[—pƒŒƒZŽí•Ê‚ÅŽæ“¾ /
040880     MOVE ƒŒƒZŽí•Ê‚v          TO ƒŒƒZ|ƒŒƒZŽí•Ê.
040890*
040900*
040910     MOVE Žó|•ÛŒ¯ŽÒ”Ô†      TO ƒŒƒZ|¿‹•ÛŒ¯ŽÒ”Ô†.
040920*
040930*
040940*
      */ƒŒƒZ[ƒŒƒZ¿‹‹æ•ª‚ðŽg‚¤‚o‚f‚ª‚ ‚éˆ×ƒRƒƒ“ƒg‰ðœ«««/170911
040950* V‹K/Œp‘± ƒ`ƒFƒbƒN
040960         IF ˜A—¿˜JŽ©|‰ŒŸ—¿  NOT = ZERO
040970             MOVE NC"›"   TO V‹Kƒ`ƒFƒbƒN‚v
040980         ELSE
040990             MOVE NC"›"   TO Œp‘±ƒ`ƒFƒbƒN‚v
041000         END-IF
041010
041020*   / 1:V‹K,2:Œp‘±,3:—¼•û /
041030     IF ( V‹Kƒ`ƒFƒbƒN‚v = NC"›" ) AND ( Œp‘±ƒ`ƒFƒbƒN‚v = NC"›" )
041040         MOVE 3  TO ƒŒƒZ|ƒŒƒZ¿‹‹æ•ª
041050     ELSE
041060        IF V‹Kƒ`ƒFƒbƒN‚v = NC"›" 
041070           MOVE 1  TO ƒŒƒZ|ƒŒƒZ¿‹‹æ•ª
041080        ELSE
041090           MOVE 2  TO ƒŒƒZ|ƒŒƒZ¿‹‹æ•ª
041100        END-IF
041110     END-IF.
      */ƒŒƒZ[ƒŒƒZ¿‹‹æ•ª‚ðŽg‚¤‚o‚f‚ª‚ ‚éˆ×ƒRƒƒ“ƒg‰ðœªªª/170911
041120
041130     PERFORM ŽÀ“ú”ƒZƒbƒg.
041140     MOVE ŽÀ“ú”‚v        TO  ƒŒƒZ|ƒŒƒZŽÀ“ú”.
041150*
041160* ˜JÐEŽ©”…ÓEŽ©”ï—p
041170     MOVE ˜AŒv|”ï—pŠz    TO ƒŒƒZ|‡Œv.
041180     MOVE ˜AŒv|•‰’SŠz    TO ƒŒƒZ|ˆê•”•‰’S‹à.
041190     MOVE ˜AŒv|¿‹Šz    TO ƒŒƒZ|¿‹‹àŠz.
041200*
041201*----------------------------------------------------------------------------*
041202     IF ƒŒƒZ|¿‹‹æ•ª = 9
041203        MOVE ZERO TO ƒŒƒZ|¿‹‘ÎÛ‹æ•ª
041204     ELSE
041205        IF ( ŽÀ“ú”‚v NOT = ZERO ) AND ( •‰|•”ˆÊ” NOT = ZERO )
041206           MOVE 1    TO ƒŒƒZ|¿‹‘ÎÛ‹æ•ª
041207        ELSE
041208           MOVE ZERO TO ƒŒƒZ|¿‹‘ÎÛ‹æ•ª
041209        END-IF
041210     END-IF.
041211*----------------------------------------------------------------------------*
      */•‰’SŠ„‡A‹‹•tŠ„‡‚ðƒZƒbƒg‚·‚é/121212
           MOVE ZERO TO ƒŒƒZ|•‰’SŠ„‡.
           MOVE 10   TO ƒŒƒZ|‹‹•tŠ„‡.
041212*
041213*================================================================*
041220 ƒtƒ@ƒCƒ‹‘ž SECTION.
041230*
041240     IF XVƒtƒ‰ƒO NOT = "YES"
041250*        WRITE ƒŒƒZ|ƒŒƒR[ƒh
041260*        IF ó‘ÔƒL[ NOT = "00"
041270*            MOVE NC"ƒŒƒZ’Ç‰Á" TO ƒtƒ@ƒCƒ‹–¼‚v
041280*            PERFORM ƒGƒ‰[•\Ž¦
041290*        END-IF
041300         DISPLAY NC"ŠY“–ŒŽ‚ÌƒŒƒR[ƒh‚ª‚ ‚è‚Ü‚¹‚ñB" UPON CONS
041310         DISPLAY NC"Ž{p˜a—ïF" ˜AŒv|Ž{p˜a—ï  UPON CONS
041320         DISPLAY NC"Ž{p”NF"   ˜AŒv|Ž{p”N    UPON CONS 
041330         DISPLAY NC"Ž{pŒŽF"   ˜AŒv|Ž{pŒŽ    UPON CONS 
041340         DISPLAY NC"Š³ŽÒ”Ô†F" ˜AŒv|Š³ŽÒ”Ô†  UPON CONS
041350         DISPLAY NC"Ž}”ÔF"     ˜AŒv|Ž}”Ô      UPON CONS 
041360         DISPLAY NC"ƒŒƒZŽí•ÊF" ƒŒƒZŽí•Ê‚v      UPON CONS 
041370         MOVE NC"ƒŒƒZƒvƒg‚e" TO ƒtƒ@ƒCƒ‹–¼‚v
041380         PERFORM ƒGƒ‰[•\Ž¦‚q
041390     ELSE
041400        REWRITE ƒŒƒZ|ƒŒƒR[ƒh
041410        IF ó‘ÔƒL[ NOT = "00"
041420            MOVE NC"ƒŒƒZXV" TO ƒtƒ@ƒCƒ‹–¼‚v
041430            PERFORM ƒGƒ‰[•\Ž¦
041440        END-IF
041450     END-IF.
041460*
041470*================================================================*
041480 ƒŒƒR[ƒh‘¶Ýƒ`ƒFƒbƒN SECTION.
041490*
041500* / ƒŒƒZƒvƒg‚e‚ÉƒŒƒR[ƒh‚ª‚ ‚é‚©ƒ`ƒFƒbƒN /
041510*
041520     MOVE SPACE TO XVƒtƒ‰ƒO.
041530*     INITIALIZE ŠeŽíƒtƒ‰ƒO‚v.
041540*
041550     MOVE ˜AŒv|Ž{p˜a—ï        TO ƒŒƒZ|Ž{p˜a—ï.
041560     MOVE ˜AŒv|Ž{p”N          TO ƒŒƒZ|Ž{p”N.
041570     MOVE ˜AŒv|Ž{pŒŽ          TO ƒŒƒZ|Ž{pŒŽ.
041580     MOVE ˜AŒv|Š³ŽÒ”Ô†        TO ƒŒƒZ|Š³ŽÒ”Ô†.
041590     MOVE ˜AŒv|Ž}”Ô            TO ƒŒƒZ|Ž}”Ô.
041600*  / ƒL[—pƒŒƒZŽí•Ê‚ÅŽæ“¾ /
041610     MOVE ƒŒƒZŽí•Ê‚v            TO ƒŒƒZ|ƒŒƒZŽí•Ê.
041620*
041630* -- ƒŒƒR[ƒh‚ª‚È‚¢Žž‚ÍA¿‹”NŒŽ‚ÍŽ{pŒŽ
041640     READ ƒŒƒZƒvƒg‚e
041650     INVALID KEY
041660         MOVE SPACE TO ƒŒƒZ|ƒŒƒR[ƒh
041670         INITIALIZE ƒŒƒZ|ƒŒƒR[ƒh
041680         MOVE ˜AŒv|Ž{p˜a—ï    TO ƒŒƒZ|¿‹˜a—ï
041690         MOVE ˜AŒv|Ž{p”N      TO ƒŒƒZ|¿‹”N
041700         MOVE ˜AŒv|Ž{pŒŽ      TO ƒŒƒZ|¿‹ŒŽ
041710         MOVE ZERO              TO ƒŒƒZ|¿‹‹æ•ª
041720     NOT INVALID KEY
041730         MOVE "YES"             TO XVƒtƒ‰ƒO
041740     END-READ.
041750*
041760*================================================================*
041770 ‰ŒŸ“úˆÈ‘O‚Ìƒf[ƒ^”»’è SECTION.
041780*
041790*********************************************************************************
041800*  Å‰‚Ì‰ŒŸ“úˆÈ‘O‚Ì“–ŒŽ’†‚ÉŽ{p‹L˜^ƒŒƒR[ƒh‚ª‚ ‚Á‚½Žž(Ž¡–üA’†Ž~)‚ÍA¿‹‹æ•ª‚Ì
041810*  Œp‘±‚É‚àƒ`ƒFƒbƒN‚·‚éB(V‹K‚ÆŒp‘±‚Ì—¼•û)
041820*********************************************************************************
041830*
041840** Å‰‚Ì‰ŒŸ“ú‚ðŽæ“¾
041850     MOVE SPACE                 TO ‰ŒŸƒtƒ‰ƒO.
041860     MOVE ˜AŒv|Š³ŽÒ”Ô†        TO Ž{‹L|Š³ŽÒ”Ô†.
041870     MOVE ˜AŒv|Ž}”Ô            TO Ž{‹L|Ž}”Ô.
041880     MOVE ˜AŒv|Ž{p˜a—ï        TO Ž{‹L|Ž{p˜a—ï.
041890     MOVE ˜AŒv|Ž{p”N          TO Ž{‹L|Ž{p”N.
041900     MOVE ˜AŒv|Ž{pŒŽ          TO Ž{‹L|Ž{pŒŽ.
041910     MOVE ZERO                  TO Ž{‹L|Ž{p“ú.
041920     START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
041930                                  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
041940     END-START.
041950     IF ó‘ÔƒL[ = "00"
041960         MOVE ZERO  TO ‰ŒŸ˜a—ï‚v‚s
041970         MOVE ZERO  TO ‰ŒŸ”N‚v‚s
041980         MOVE ZERO  TO ‰ŒŸŒŽ‚v‚s
041990         MOVE ZERO  TO ‰ŒŸ“ú‚v‚s
042000         MOVE SPACE TO I—¹ƒtƒ‰ƒO
042010         PERFORM Ž{p‹L˜^‚e“Çž
042020         PERFORM UNTIL ( I—¹ƒtƒ‰ƒO           = "YES"           ) OR
042030                       ( Ž{‹L|Š³ŽÒƒR[ƒh NOT = ˜AŒv|Š³ŽÒƒR[ƒh) OR
042040                       ( Ž{‹L|Ž{p˜a—ï   NOT = ˜AŒv|Ž{p˜a—ï  ) OR
042050                       ( Ž{‹L|Ž{p”N     NOT = ˜AŒv|Ž{p”N    ) OR
042060                       ( Ž{‹L|Ž{pŒŽ     NOT = ˜AŒv|Ž{pŒŽ    ) 
042070*
042080*         /  Å‰‚Ì‰ŒŸ“ú‚ðŽæ“¾
042090               IF ( Ž{‹L|f—Ã‹æ•ª = 2 ) AND ( ‰ŒŸƒtƒ‰ƒO = SPACE )
042100                   MOVE Ž{‹L|Ž{p˜a—ï           TO ‰ŒŸ˜a—ï‚v‚s
042110                   MOVE Ž{‹L|Ž{p”N             TO ‰ŒŸ”N‚v‚s
042120                   MOVE Ž{‹L|Ž{pŒŽ             TO ‰ŒŸŒŽ‚v‚s
042130                   MOVE Ž{‹L|Ž{p“ú             TO ‰ŒŸ“ú‚v‚s
042140                   MOVE "YES"                    TO ‰ŒŸƒtƒ‰ƒO
042150                   MOVE "YES"                    TO I—¹ƒtƒ‰ƒO
042160               END-IF
042170               PERFORM Ž{p‹L˜^‚e“Çž
042180         END-PERFORM
042190     END-IF.
042200*
042210* ‰ŒŸ“úˆÈ‘O‚Ìƒf[ƒ^”»’è
042220     IF ‰ŒŸƒtƒ‰ƒO = "YES"
042230        MOVE ˜AŒv|Š³ŽÒ”Ô†        TO Ž{‹L|Š³ŽÒ”Ô†
042240        MOVE ˜AŒv|Ž}”Ô            TO Ž{‹L|Ž}”Ô
042250        MOVE ‰ŒŸ˜a—ï‚v‚s          TO Ž{‹L|Ž{p˜a—ï
042260        MOVE ‰ŒŸ”N‚v‚s            TO Ž{‹L|Ž{p”N
042270        MOVE ‰ŒŸŒŽ‚v‚s            TO Ž{‹L|Ž{pŒŽ
042280        MOVE ‰ŒŸ“ú‚v‚s            TO Ž{‹L|Ž{p“ú
042290        START Ž{p‹L˜^‚e   KEY IS <  Ž{‹L|Š³ŽÒƒR[ƒh
042300                                     Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
042310                                     REVERSED
042320        END-START
042330        IF ó‘ÔƒL[ = "00"
042340           MOVE SPACE  TO I—¹ƒtƒ‰ƒO
042350           PERFORM Ž{p‹L˜^‚e“Çž
042360           IF ( I—¹ƒtƒ‰ƒO      = SPACE         ) AND
042370              ( Ž{‹L|Š³ŽÒ”Ô†  = ˜AŒv|Š³ŽÒ”Ô†) AND
042380              ( Ž{‹L|Ž}”Ô      = ˜AŒv|Ž}”Ô    ) AND
042390              ( Ž{‹L|Ž{p˜a—ï  = ‰ŒŸ˜a—ï‚v‚s  ) AND
042400              ( Ž{‹L|Ž{p”N    = ‰ŒŸ”N‚v‚s    ) AND
042410              ( Ž{‹L|Ž{pŒŽ    = ‰ŒŸŒŽ‚v‚s    )
042420*  ‰ŒŸ“úˆÈ‘O‚Ì“–ŒŽ’†‚ÉŽ{p‹L˜^ƒŒƒR[ƒh‚ª‚ ‚Á‚½Žž
042430                IF Œp‘±ƒ`ƒFƒbƒN‚v = SPACE
042440                   MOVE NC"›"    TO Œp‘±ƒ`ƒFƒbƒN‚v
042450                END-IF
042460           END-IF
042470         END-IF
042480     END-IF.
042490*
042500*================================================================*
042510 ŽófŽÒî•ñŽæ“¾ SECTION.
042520*
042530     MOVE ˜AŒv|Ž{p˜a—ï     TO Žó|Ž{p˜a—ï.
042540     MOVE ˜AŒv|Ž{p”N       TO Žó|Ž{p”N  .
042550     MOVE ˜AŒv|Ž{pŒŽ       TO Žó|Ž{pŒŽ  .
042560     MOVE ˜AŒv|Š³ŽÒ”Ô†     TO Žó|Š³ŽÒ”Ô†.
042570     MOVE ˜AŒv|Ž}”Ô         TO Žó|Ž}”Ô    .
042580     READ ŽófŽÒî•ñ‚e
042590     INVALID KEY
042600         MOVE NC"Žóf" TO ƒtƒ@ƒCƒ‹–¼‚v
042610         PERFORM ƒGƒ‰[•\Ž¦
042620         PERFORM I—¹ˆ—
042630         EXIT PROGRAM
042640     END-READ.
042650*================================================================*
042660 ƒŒƒZƒvƒg˜JŽ©ƒŒƒR[ƒhƒZƒbƒg SECTION.
042670*
042680     MOVE ˜A—¿˜JŽ©|•‰’S—¦              TO ƒŒƒZ˜JŽ©|•‰’S—¦.
042690     MOVE ˜A—¿˜JŽ©|‰ŒŸ—¿              TO ƒŒƒZ˜JŽ©|‰ŒŸ—¿.
042700     MOVE ˜A—¿˜JŽ©|‰ŒŸ‰ñ”            TO ƒŒƒZ˜JŽ©|‰ŒŸ‰ñ”.
042710     MOVE ˜A—¿˜JŽ©|‰ŒŸ’P‰¿            TO ƒŒƒZ˜JŽ©|‰ŒŸ’P‰¿.
042720     MOVE ˜A—¿˜JŽ©|‰ŒŸŽžŠÔŠO          TO ƒŒƒZ˜JŽ©|‰ŒŸŽžŠÔŠO.
042730     MOVE ˜A—¿˜JŽ©|‰ŒŸ‹x“ú            TO ƒŒƒZ˜JŽ©|‰ŒŸ‹x“ú.
042740     MOVE ˜A—¿˜JŽ©|‰ŒŸ[–é            TO ƒŒƒZ˜JŽ©|‰ŒŸ[–é.
042750     MOVE ˜A—¿˜JŽ©|‰ŒŸ‰ÁŽZ—¿          TO ƒŒƒZ˜JŽ©|‰ŒŸ‰ÁŽZ—¿.
042760     MOVE ˜A—¿˜JŽ©|‰ŒŸŽž‘Š’k—¿        TO ƒŒƒZ˜JŽ©|‰ŒŸŽž‘Š’k—¿.
042770     MOVE ˜A—¿˜JŽ©|ÄŒŸ—¿              TO ƒŒƒZ˜JŽ©|ÄŒŸ—¿.
042780     MOVE ˜A—¿˜JŽ©|ÄŒŸ‰ñ”            TO ƒŒƒZ˜JŽ©|ÄŒŸ‰ñ”.
042790     MOVE ˜A—¿˜JŽ©|ÄŒŸ’P‰¿            TO ƒŒƒZ˜JŽ©|ÄŒŸ’P‰¿.
042800     MOVE ˜A—¿˜JŽ©|‰—Ã‹——£            TO ƒŒƒZ˜JŽ©|‰—Ã‹——£.
042810     MOVE ˜A—¿˜JŽ©|‰—Ã‰ñ”            TO ƒŒƒZ˜JŽ©|‰—Ã‰ñ”.
042820     MOVE ˜A—¿˜JŽ©|‰—Ã’P‰¿            TO ƒŒƒZ˜JŽ©|‰—Ã’P‰¿.
042830     MOVE ˜A—¿˜JŽ©|‰—Ã—¿              TO ƒŒƒZ˜JŽ©|‰—Ã—¿.
042840     MOVE ˜A—¿˜JŽ©|‰—Ã‰ÁŽZ—¿          TO ƒŒƒZ˜JŽ©|‰—Ã‰ÁŽZ—¿.
042850     MOVE ˜A—¿˜JŽ©|‰—Ã–éŠÔ            TO ƒŒƒZ˜JŽ©|‰—Ã–éŠÔ.
042860     MOVE ˜A—¿˜JŽ©|‰—Ã“ï˜H            TO ƒŒƒZ˜JŽ©|‰—Ã“ï˜H.
042870     MOVE ˜A—¿˜JŽ©|‰—Ã–\•—‰J          TO ƒŒƒZ˜JŽ©|‰—Ã–\•—‰J.
042880     MOVE ˜A—¿˜JŽ©|‰—ÃŽžŠÔŠO          TO ƒŒƒZ˜JŽ©|‰—ÃŽžŠÔŠO.
042890     MOVE ˜A—¿˜JŽ©|‹à‘®•›Žq‰ÁŽZ—¿      TO ƒŒƒZ˜JŽ©|‹à‘®•›Žq‰ÁŽZ—¿.
042900     MOVE ˜A—¿˜JŽ©|‘å                  TO ƒŒƒZ˜JŽ©|‘å.
042910     MOVE ˜A—¿˜JŽ©|’†                  TO ƒŒƒZ˜JŽ©|’†.
042920     MOVE ˜A—¿˜JŽ©|¬                  TO ƒŒƒZ˜JŽ©|¬.
042930     MOVE ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ‰ñ”        TO ƒŒƒZ˜JŽ©|î•ñ’ñ‹Ÿ‰ñ”.
042940     MOVE ˜A—¿˜JŽ©|î•ñ’ñ‹Ÿ’P‰¿        TO ƒŒƒZ˜JŽ©|î•ñ’ñ‹Ÿ’P‰¿.
042950     MOVE ˜A—¿˜JŽ©|Ž{pî•ñ’ñ‹Ÿ—¿      TO ƒŒƒZ˜JŽ©|Ž{pî•ñ’ñ‹Ÿ—¿.
042960     MOVE ˜A—¿˜JŽ©|¬Œv                TO ƒŒƒZ˜JŽ©|¬Œv.
042970     MOVE ˜A—¿˜JŽ©|”ï—pŠz              TO ƒŒƒZ˜JŽ©|”ï—pŠz.
042980     MOVE ˜A—¿˜JŽ©|ˆê•”•‰’S‹à          TO ƒŒƒZ˜JŽ©|ˆê•”•‰’S‹à.
042990     MOVE ˜A—¿˜JŽ©|¿‹Šz              TO ƒŒƒZ˜JŽ©|¿‹Šz.
043000     MOVE ˜A—¿˜JŽ©|‹‹•tŠ„‡            TO ƒŒƒZ˜JŽ©|‹‹•tŠ„‡.
043010     MOVE ˜A—¿˜JŽ©|Žó‹‹ŽÒ•‰’SŠz        TO ƒŒƒZ˜JŽ©|Žó‹‹ŽÒ•‰’SŠz.
043020     MOVE ˜A—¿˜JŽ©|•¬¿‹‹àŠz        TO ƒŒƒZ˜JŽ©|•¬¿‹‹àŠz.
043030     MOVE ˜A—¿˜JŽ©|Žw“±ŠÇ—‰ñ”        TO ƒŒƒZ˜JŽ©|Žw“±ŠÇ—‰ñ”.
043040     MOVE ˜A—¿˜JŽ©|Žw“±ŠÇ—’P‰¿        TO ƒŒƒZ˜JŽ©|Žw“±ŠÇ—’P‰¿.
043050     MOVE ˜A—¿˜JŽ©|Žw“±ŠÇ——¿          TO ƒŒƒZ˜JŽ©|Žw“±ŠÇ——¿.
043060     MOVE ˜A—¿˜JŽ©|‰^“®—Ã–@‰ñ”        TO ƒŒƒZ˜JŽ©|‰^“®—Ã–@‰ñ”.
043070     MOVE ˜A—¿˜JŽ©|‰^“®—Ã–@’P‰¿        TO ƒŒƒZ˜JŽ©|‰^“®—Ã–@’P‰¿.
043080     MOVE ˜A—¿˜JŽ©|‰^“®—Ã–@—¿          TO ƒŒƒZ˜JŽ©|‰^“®—Ã–@—¿.
043090     MOVE ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‰ñ”    TO ƒŒƒZ˜JŽ©|‹x‹Æ•âžØ–¾‰ñ”.
043100     MOVE ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾’P‰¿    TO ƒŒƒZ˜JŽ©|‹x‹Æ•âžØ–¾’P‰¿.
043110     MOVE ˜A—¿˜JŽ©|‹x‹Æ•âžØ–¾‘—¿    TO ƒŒƒZ˜JŽ©|‹x‹Æ•âžØ–¾‘—¿.
043120     MOVE ˜A—¿˜JŽ©|h”‘‰ñ”            TO ƒŒƒZ˜JŽ©|h”‘‰ñ”.
043130     MOVE ˜A—¿˜JŽ©|h”‘’P‰¿            TO ƒŒƒZ˜JŽ©|h”‘’P‰¿.
043140     MOVE ˜A—¿˜JŽ©|h”‘—¿              TO ƒŒƒZ˜JŽ©|h”‘—¿.
043150     MOVE ˜A—¿˜JŽ©|HŽ–‰ñ”            TO ƒŒƒZ˜JŽ©|HŽ–‰ñ”.
043160     MOVE ˜A—¿˜JŽ©|HŽ–’P‰¿            TO ƒŒƒZ˜JŽ©|HŽ–’P‰¿.
043170     MOVE ˜A—¿˜JŽ©|HŽ–—¿              TO ƒŒƒZ˜JŽ©|HŽ–—¿.
043180     MOVE ˜A—¿˜JŽ©|Ž{pØ–¾‰ñ”        TO ƒŒƒZ˜JŽ©|Ž{pØ–¾‰ñ”.
043190     MOVE ˜A—¿˜JŽ©|Ž{pØ–¾’P‰¿        TO ƒŒƒZ˜JŽ©|Ž{pØ–¾’P‰¿.
043200     MOVE ˜A—¿˜JŽ©|Ž{pØ–¾‘—¿        TO ƒŒƒZ˜JŽ©|Ž{pØ–¾‘—¿.
043210     MOVE ˜A—¿˜JŽ©|Ž{p–¾×‰ñ”        TO ƒŒƒZ˜JŽ©|Ž{p–¾×‰ñ”.
043220     MOVE ˜A—¿˜JŽ©|Ž{p–¾×’P‰¿        TO ƒŒƒZ˜JŽ©|Ž{p–¾×’P‰¿.
043230     MOVE ˜A—¿˜JŽ©|Ž{p–¾×‘—¿        TO ƒŒƒZ˜JŽ©|Ž{p–¾×‘—¿.
043240     MOVE ˜A—¿˜JŽ©|Ž©”…Ó‚»‚Ì‘¼‹àŠz    TO ƒŒƒZ˜JŽ©|Ž©”…Ó‚»‚Ì‘¼‹àŠz.
043250     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 10
043260         MOVE ˜A—¿˜JŽ©|“–“ú•ª•‰’SŠz(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|“–“ú•ª•‰’SŠz(ƒJƒEƒ“ƒ^)
043270     END-PERFORM.
043280     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 7
043290         MOVE ˜A—¿˜JŽ©|‰‰ñˆ’u—¿  (ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|‰‰ñˆ’u—¿  (ƒJƒEƒ“ƒ^)
043300         MOVE ˜A—¿˜JŽ©|Þ—¿”ï      (ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|Þ—¿”ï      (ƒJƒEƒ“ƒ^)
      */œÜ•s‘SœÜ’E‰PA“Á•ÊÞ—¿‚ÌŒðŠ·‚ÍA‚Q‰ñ‚Ü‚ÅŽZ’è‚Å‚«‚éB/20181015
               MOVE ˜A—¿˜JŽ©|Þ—¿”ï’P‰¿  (ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|Þ—¿”ï’P‰¿  (ƒJƒEƒ“ƒ^)
026560         MOVE ˜A—¿˜JŽ©|Þ—¿”ï‰ñ”  (ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|Þ—¿”ï‰ñ”  (ƒJƒEƒ“ƒ^)
      *
043310         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚P(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·“ú‚P(ƒJƒEƒ“ƒ^)
043320         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚Q(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·“ú‚Q(ƒJƒEƒ“ƒ^)
043330         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚R(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·“ú‚R(ƒJƒEƒ“ƒ^)
043340         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚S(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·“ú‚S(ƒJƒEƒ“ƒ^)
043350         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚T(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·“ú‚T(ƒJƒEƒ“ƒ^)
043360         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·“ú‚U(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·“ú‚U(ƒJƒEƒ“ƒ^)
043370         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·—¿  (ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·—¿  (ƒJƒEƒ“ƒ^)
043380         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·’P‰¿(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·’P‰¿(ƒJƒEƒ“ƒ^)
043390         MOVE ˜A—¿˜JŽ©|•ï‘ÑŒðŠ·‰ñ”(ƒJƒEƒ“ƒ^) TO ƒŒƒZ˜JŽ©|•ï‘ÑŒðŠ·‰ñ”(ƒJƒEƒ“ƒ^)
043400     END-PERFORM.
043410     MOVE ˜A—¿˜JŽ©|‰‰ñˆ’u—¿Œv               TO ƒŒƒZ˜JŽ©|‰‰ñˆ’u—¿Œv
043420     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚P                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚P.
043430     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚P                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚P.
043440     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚P                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚P.
043450     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚P               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚P.
043460     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚P               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚P.
043470     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚P                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚P.
043480     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚P               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚P.
043490     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚P               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚P.
043500     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚P                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚P.
043510     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚P                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚P.
043520     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚P                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚P.
043530     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚P                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚P.
043540     MOVE ˜A—¿˜JŽ©|¬Œv‚P                     TO ƒŒƒZ˜JŽ©|¬Œv‚P.
043550     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚P               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚P.
043560     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚P               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚P.
043570     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚PSk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚PSk.
043580     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚PSk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚PSk.
043590     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚PSk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚PSk.
043600     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚PSk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚PSk.
043610     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚PSk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚PSk.
043620     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚PSk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚PSk.
043630     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚PSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚PSk.
043640     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚PSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚PSk.
043650     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚PSk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚PSk.
043660     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚PSk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚PSk.
043670     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚PSk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚PSk.
043680     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚PSk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚PSk.
043690     MOVE ˜A—¿˜JŽ©|¬Œv‚PSk                 TO ƒŒƒZ˜JŽ©|¬Œv‚PSk.
043700     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚PSk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚PSk.
043710     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚PSk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚PSk.
043720     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚P           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚P.
043730     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚P           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚P.
043740     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚P             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚P.
043750     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚P             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚P.
043760     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚P             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚P.
043770     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚P               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚P.
043780     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚P       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚P.
043790     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚P       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚P.
043800     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚P         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚P.
043810     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚Q                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚Q.
043820     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚Q                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚Q.
043830     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚Q                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚Q.
043840     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚Q               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚Q.
043850     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚Q               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚Q.
043860     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚Q                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚Q.
043870     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚Q               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚Q.
043880     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚Q               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚Q.
043890     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚Q                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚Q.
043900     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚Q                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚Q.
043910     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚Q                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚Q.
043920     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚Q                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚Q.
043930     MOVE ˜A—¿˜JŽ©|¬Œv‚Q                     TO ƒŒƒZ˜JŽ©|¬Œv‚Q.
043940     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚Q               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚Q.
043950     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚Q               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚Q.
043960     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚QSk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚QSk.
043970     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚QSk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚QSk.
043980     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚QSk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚QSk.
043990     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚QSk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚QSk.
044000     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚QSk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚QSk.
044010     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚QSk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚QSk.
044020     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚QSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚QSk.
044030     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚QSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚QSk.
044040     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚QSk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚QSk.
044050     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚QSk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚QSk.
044060     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚QSk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚QSk.
044070     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚QSk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚QSk.
044080     MOVE ˜A—¿˜JŽ©|¬Œv‚QSk                 TO ƒŒƒZ˜JŽ©|¬Œv‚QSk.
044090     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚QSk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚QSk.
044100     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚QSk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚QSk.
044110     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚Q           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚Q.
044120     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚Q           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚Q.
044130     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚Q             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚Q.
044140     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚Q             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚Q.
044150     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚Q             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚Q.
044160     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚Q               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚Q.
044170     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚Q       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚Q.
044180     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚Q       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚Q.
044190     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚Q         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚Q.
044200     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚R                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚R.
044210     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚R                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚R.
044220     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚R                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚R.
044230     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚R               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚R.
044240     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚R               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚R.
044250     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚R                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚R.
044260     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚R               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚R.
044270     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚R               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚R.
044280     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚R                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚R.
044290     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚R                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚R.
044300     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚R                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚R.
044310     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚R                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚R.
044320     MOVE ˜A—¿˜JŽ©|¬Œv‚R                     TO ƒŒƒZ˜JŽ©|¬Œv‚R.
044330     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚R               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚R.
044340     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚R               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚R.
044350     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚RSk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚RSk.
044360     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚RSk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚RSk.
044370     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚RSk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚RSk.
044380     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚RSk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚RSk.
044390     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚RSk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚RSk.
044400     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚RSk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚RSk.
044410     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚RSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚RSk.
044420     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚RSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚RSk.
044430     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚RSk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚RSk.
044440     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚RSk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚RSk.
044450     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚RSk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚RSk.
044460     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚RSk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚RSk.
044470     MOVE ˜A—¿˜JŽ©|¬Œv‚RSk                 TO ƒŒƒZ˜JŽ©|¬Œv‚RSk.
044480     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚RSk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚RSk.
044490     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚RSk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚RSk.
044500     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚R           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚R.
044510     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚R           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚R.
044520     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚R             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚R.
044530     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚R             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚R.
044540     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚R             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚R.
044550     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚R               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚R.
044560     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚R       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚R.
044570     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚R       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚R.
044580     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚R         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚R.
044590     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚S                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚S.
044600     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚S                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚S.
044610     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚S                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚S.
044620     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚S               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚S.
044630     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚S               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚S.
044640     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚S                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚S.
044650     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚S               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚S.
044660     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚S               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚S.
044670     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚S                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚S.
044680     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚S                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚S.
044690     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚S                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚S.
044700     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚S                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚S.
044710     MOVE ˜A—¿˜JŽ©|¬Œv‚S                     TO ƒŒƒZ˜JŽ©|¬Œv‚S.
044720     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚S               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚S.
044730     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚S               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚S.
044740     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚SSk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚SSk.
044750     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚SSk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚SSk.
044760     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚SSk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚SSk.
044770     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚SSk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚SSk.
044780     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚SSk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚SSk.
044790     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚SSk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚SSk.
044800     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚SSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚SSk.
044810     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚SSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚SSk.
044820     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚SSk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚SSk.
044830     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚SSk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚SSk.
044840     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚SSk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚SSk.
044850     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚SSk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚SSk.
044860     MOVE ˜A—¿˜JŽ©|¬Œv‚SSk                 TO ƒŒƒZ˜JŽ©|¬Œv‚SSk.
044870     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚SSk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚SSk.
044880     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚SSk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚SSk.
044890     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚S           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚S.
044900     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚S           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚S.
044910     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚S             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚S.
044920     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚S             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚S.
044930     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚S             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚S.
044940     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚S               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚S.
044950     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚S       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚S.
044960     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚S       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚S.
044970     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚S         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚S.
044980     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚T                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚T.
044990     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚T                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚T.
045000     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚T                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚T.
045010     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚T               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚T.
045020     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚T               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚T.
045030     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚T                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚T.
045040     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚T               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚T.
045050     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚T               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚T.
045060     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚T                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚T.
045070     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚T                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚T.
045080     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚T                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚T.
045090     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚T                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚T.
045100     MOVE ˜A—¿˜JŽ©|¬Œv‚T                     TO ƒŒƒZ˜JŽ©|¬Œv‚T.
045110     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚T               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚T.
045120     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚T               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚T.
045130     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚TSk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚TSk.
045140     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚TSk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚TSk.
045150     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚TSk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚TSk.
045160     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚TSk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚TSk.
045170     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚TSk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚TSk.
045180     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚TSk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚TSk.
045190     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚TSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚TSk.
045200     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚TSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚TSk.
045210     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚TSk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚TSk.
045220     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚TSk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚TSk.
045230     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚TSk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚TSk.
045240     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚TSk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚TSk.
045250     MOVE ˜A—¿˜JŽ©|¬Œv‚TSk                 TO ƒŒƒZ˜JŽ©|¬Œv‚TSk.
045260     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚TSk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚TSk.
045270     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚TSk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚TSk.
045280     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚T           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚T.
045290     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚T           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚T.
045300     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚T             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚T.
045310     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚T             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚T.
045320     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚T             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚T.
045330     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚T               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚T.
045340     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚T       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚T.
045350     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚T       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚T.
045360     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚T         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚T.
045370     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚U                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚U.
045380     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚U                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚U.
045390     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚U                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚U.
045400     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚U               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚U.
045410     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚U               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚U.
045420     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚U                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚U.
045430     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚U               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚U.
045440     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚U               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚U.
045450     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚U                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚U.
045460     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚U                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚U.
045470     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚U                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚U.
045480     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚U                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚U.
045490     MOVE ˜A—¿˜JŽ©|¬Œv‚U                     TO ƒŒƒZ˜JŽ©|¬Œv‚U.
045500     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚U               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚U.
045510     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚U               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚U.
045520     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚USk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚USk.
045530     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚USk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚USk.
045540     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚USk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚USk.
045550     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚USk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚USk.
045560     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚USk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚USk.
045570     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚USk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚USk.
045580     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚USk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚USk.
045590     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚USk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚USk.
045600     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚USk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚USk.
045610     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚USk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚USk.
045620     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚USk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚USk.
045630     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚USk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚USk.
045640     MOVE ˜A—¿˜JŽ©|¬Œv‚USk                 TO ƒŒƒZ˜JŽ©|¬Œv‚USk.
045650     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚USk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚USk.
045660     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚USk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚USk.
045670     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚U           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚U.
045680     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚U           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚U.
045690     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚U             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚U.
045700     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚U             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚U.
045710     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚U             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚U.
045720     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚U               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚U.
045730     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚U       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚U.
045740     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚U       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚U.
045750     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚U         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚U.
045760     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚V                 TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚V.
045770     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚V                 TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚V.
045780     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚V                   TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚V.
045790     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚V               TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚V.
045800     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚V               TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚V.
045810     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚V                 TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚V.
045820     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚V               TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚V.
045830     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚V               TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚V.
045840     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚V                 TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚V.
045850     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚V                 TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚V.
045860     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚V                 TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚V.
045870     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚V                   TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚V.
045880     MOVE ˜A—¿˜JŽ©|¬Œv‚V                     TO ƒŒƒZ˜JŽ©|¬Œv‚V.
045890     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚V               TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚V.
045900     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚V               TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚V.
045910     MOVE ˜A—¿˜JŽ©|Œã—Ã’P‰¿‚VSk             TO ƒŒƒZ˜JŽ©|Œã—Ã’P‰¿‚VSk.
045920     MOVE ˜A—¿˜JŽ©|Œã—Ã‰ñ”‚VSk             TO ƒŒƒZ˜JŽ©|Œã—Ã‰ñ”‚VSk.
045930     MOVE ˜A—¿˜JŽ©|Œã—Ã—¿‚VSk               TO ƒŒƒZ˜JŽ©|Œã—Ã—¿‚VSk.
045940     MOVE ˜A—¿˜JŽ©|—âãª–@’P‰¿‚VSk           TO ƒŒƒZ˜JŽ©|—âãª–@’P‰¿‚VSk.
045950     MOVE ˜A—¿˜JŽ©|—âãª–@‰ñ”‚VSk           TO ƒŒƒZ˜JŽ©|—âãª–@‰ñ”‚VSk.
045960     MOVE ˜A—¿˜JŽ©|—âãª–@—¿‚VSk             TO ƒŒƒZ˜JŽ©|—âãª–@—¿‚VSk.
045970     MOVE ˜A—¿˜JŽ©|‰·ãª–@’P‰¿‚VSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@’P‰¿‚VSk.
045980     MOVE ˜A—¿˜JŽ©|‰·ãª–@‰ñ”‚VSk           TO ƒŒƒZ˜JŽ©|‰·ãª–@‰ñ”‚VSk.
045990     MOVE ˜A—¿˜JŽ©|‰·ãª–@—¿‚VSk             TO ƒŒƒZ˜JŽ©|‰·ãª–@—¿‚VSk.
046000     MOVE ˜A—¿˜JŽ©|“d—Ã’P‰¿‚VSk             TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿‚VSk.
046010     MOVE ˜A—¿˜JŽ©|“d—Ã‰ñ”‚VSk             TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”‚VSk.
046020     MOVE ˜A—¿˜JŽ©|“d—Ã—¿‚VSk               TO ƒŒƒZ˜JŽ©|“d—Ã—¿‚VSk.
046030     MOVE ˜A—¿˜JŽ©|¬Œv‚VSk                 TO ƒŒƒZ˜JŽ©|¬Œv‚VSk.
046040     MOVE ˜A—¿˜JŽ©|’·Šú’üŒ¸—¦‚VSk           TO ƒŒƒZ˜JŽ©|’·Šú’üŒ¸—¦‚VSk.
046050     MOVE ˜A—¿˜JŽ©|’·Šúž¬Œv‚VSk           TO ƒŒƒZ˜JŽ©|’·Šúž¬Œv‚VSk.
046060     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚V           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã’P‰¿‚V.
046070     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚V           TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã‰ñ”‚V.
046080     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚V             TO ƒŒƒZ˜JŽ©|‘½•”ˆÊŒã—Ã—¿‚V.
046090     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã’P‰¿‚V             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã’P‰¿‚V.
046100     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã‰ñ”‚V             TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã‰ñ”‚V.
046110     MOVE ˜A—¿˜JŽ©|’·ŠúŒã—Ã—¿‚V               TO ƒŒƒZ˜JŽ©|’·ŠúŒã—Ã—¿‚V.
046120     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚V       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã’P‰¿‚V.
046130     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚V       TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã‰ñ”‚V.
046140     MOVE ˜A—¿˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚V         TO ƒŒƒZ˜JŽ©|‘½•”ˆÊSkŒã—Ã—¿‚V.
046150*/—âãª–@‚Æ‰·ãª–@‚Ì’P‰¿‚ªˆá‚¤ê‡‚Ì–¾×‘AŒvŽZ‘‚Ì‹L“ü‚Ìˆ×‚Ì‘Î‰žB
046160     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 7
046170         PERFORM VARYING ƒJƒEƒ“ƒ^‚Q FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^‚Q > 6
046180             MOVE ˜A—¿˜JŽ©‚Q|ãª–@’P‰¿(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q) TO ƒŒƒZ˜JŽ©|ãª–@’P‰¿(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q)
046190             MOVE ˜A—¿˜JŽ©‚Q|ãª–@‰ñ”(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q) TO ƒŒƒZ˜JŽ©|ãª–@‰ñ”(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q)
046200         END-PERFORM
046210     END-PERFORM.
046220     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 7
046230         PERFORM VARYING ƒJƒEƒ“ƒ^‚Q FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^‚Q > 2
046240             MOVE ˜A—¿˜JŽ©‚Q|“d—Ã’P‰¿(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q) TO ƒŒƒZ˜JŽ©|“d—Ã’P‰¿(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q)
046250             MOVE ˜A—¿˜JŽ©‚Q|“d—Ã‰ñ”(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q) TO ƒŒƒZ˜JŽ©|“d—Ã‰ñ”(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q)
046260         END-PERFORM
046270     END-PERFORM.
046280*================================================================*
046290* ƒŒƒZƒvƒg˜JŽ©‘ž SECTION.
046300**
046310*     REWRITE ƒŒƒZ˜JŽ©|ƒŒƒR[ƒh.
046320*     IF ó‘ÔƒL[ NOT = "00"
046330*        MOVE NC"ƒŒƒZXV" TO ƒtƒ@ƒCƒ‹–¼‚v
046340*        PERFORM ƒGƒ‰[•\Ž¦
046350*     END-IF.
046360*================================================================*
046370 ŽófŽÒ_®’Ê‰@‚ ‚èXV SECTION.
046380*
046390* ŽÀ“ú”‚ ‚è‚È‚µ¨ŽófŽÒî•ñ‚e‚Ì_®—L‹æ•ª‚ðXV‚·‚éB
046400* 0F_®’Ê‰@‚È‚µ 1F_®’Ê‰@‚ ‚è
046410*     05 _®—L‹æ•ª                     PIC 9.
046420*
046430     MOVE ˜AŒv|Ž{p˜a—ï     TO Žó|Ž{p˜a—ï.
046440     MOVE ˜AŒv|Ž{p”N       TO Žó|Ž{p”N.
046450     MOVE ˜AŒv|Ž{pŒŽ       TO Žó|Ž{pŒŽ.
046460     MOVE ˜AŒv|Š³ŽÒ”Ô†     TO Žó|Š³ŽÒ”Ô†.
046470     MOVE ˜AŒv|Ž}”Ô         TO Žó|Ž}”Ô.
046480     READ ŽófŽÒî•ñ‚e
046490     NOT INVALID KEY
046500*
046510         IF ŽÀ“ú”‚v NOT = ZERO
046520            MOVE 1    TO Žó|_®—L‹æ•ª
046530         ELSE
046540            MOVE ZERO TO Žó|_®—L‹æ•ª
046550         END-IF
046560*
046570         REWRITE Žó|ƒŒƒR[ƒh
046580         IF ó‘ÔƒL[ NOT = "00"
046590            MOVE NC"ŽófŽÒXV" TO ƒtƒ@ƒCƒ‹–¼‚v
046600            PERFORM ƒGƒ‰[•\Ž¦
046610         END-IF
046620     END-READ.
046630*
046640*================================================================*
046650*================================================================*
046660 ŽÀ“ú”ƒZƒbƒg SECTION.
046670*
046680     MOVE ZERO      TO  ŽÀ“ú”‚v.
046690*
046700     MOVE ˜AŒv|Š³ŽÒ”Ô†        TO Ž{‹L|Š³ŽÒ”Ô†.
046710     MOVE ˜AŒv|Ž}”Ô            TO Ž{‹L|Ž}”Ô.
046720     MOVE ˜AŒv|Ž{p˜a—ï        TO Ž{‹L|Ž{p˜a—ï.
046730     MOVE ˜AŒv|Ž{p”N          TO Ž{‹L|Ž{p”N.
046740     MOVE ˜AŒv|Ž{pŒŽ          TO Ž{‹L|Ž{pŒŽ.
046750     MOVE ZERO                  TO Ž{‹L|Ž{p“ú
046760*
046770     START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
046780                                  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
046790     END-START.
046800     IF ó‘ÔƒL[ = "00"
046810         MOVE SPACE TO I—¹ƒtƒ‰ƒO
046820         PERFORM Ž{p‹L˜^‚e“Çž
046830         PERFORM UNTIL ( Ž{‹L|Ž{p˜a—ï   NOT = ˜AŒv|Ž{p˜a—ï ) OR
046840                       ( Ž{‹L|Ž{p”N     NOT = ˜AŒv|Ž{p”N   ) OR
046850                       ( Ž{‹L|Ž{pŒŽ     NOT = ˜AŒv|Ž{pŒŽ   ) OR
046860                       ( Ž{‹L|Š³ŽÒƒR[ƒh NOT = ˜AŒv|Š³ŽÒƒR[ƒh ) OR
046870                       ( I—¹ƒtƒ‰ƒO       NOT = SPACE )
046880*
046890             COMPUTE ŽÀ“ú”‚v = ŽÀ“ú”‚v + 1
046900*
046910             PERFORM Ž{p‹L˜^‚e“Çž
046920         END-PERFORM
046930     END-IF.
046940*
046950*================================================================*
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
      *     MOVE Ž{pØ–¾‘—¿‚v    TO ‰ï‚O‚Q|Ž{pØ–¾‘—¿.
      *     MOVE Ž{p–¾×‘—¿‚v    TO ‰ï‚O‚Q|Ž{p–¾×‘—¿.
      *     MOVE ‚»‚Ì‘¼‹àŠz‚v      TO ‰ï‚O‚Q|Ž©”…Ó‚»‚Ì‘¼‹àŠz.
      * •‰•”ˆÊ–ˆ‚Ì—¿‹à
           PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 7
               MOVE ‰‰ñˆ’u—¿‚m‚n‚v(ƒJƒEƒ“ƒ^) TO ‰ï‚O‚Q|‰‰ñˆ’u—¿(ƒJƒEƒ“ƒ^)
      *         MOVE Þ—¿”ï‚Q‚v      (ƒJƒEƒ“ƒ^) TO ‰ï‚O‚Q|Þ—¿”ï    (ƒJƒEƒ“ƒ^)
               MOVE Þ—¿”ï‚m‚v      (ƒJƒEƒ“ƒ^) TO ‰ï‚O‚Q|Þ—¿”ï    (ƒJƒEƒ“ƒ^)
               MOVE ŒðŠ·•ï‘Ñ—¿‚m‚v  (ƒJƒEƒ“ƒ^) TO ‰ï‚O‚Q|•ï‘ÑŒðŠ·—¿(ƒJƒEƒ“ƒ^)
           END-PERFORM.
           MOVE ‰‰ñˆ’u—¿‚v      TO ‰ï‚O‚Q|‰‰ñˆ’u—¿Œv.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚P   = Œã—Ã—¿‚P‚m‚q‚v     + Œã—Ã—¿‚PSk‚m‚q‚v.
      *      + ‘½•”ˆÊŒã—Ã—¿‚P‚m‚q‚v + ’·ŠúŒã—Ã—¿‚P‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚P‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚P = —âãª–@—¿‚P‚m‚q‚v   + —âãª–@—¿‚PSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚P = ‰·ãª–@—¿‚P‚m‚q‚v   + ‰·ãª–@—¿‚PSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚P   = “d—Ã—¿‚P‚m‚q‚v     + “d—Ã—¿‚PSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚Q   = Œã—Ã—¿‚Q‚m‚q‚v     + Œã—Ã—¿‚QSk‚m‚q‚v.
      *     + ‘½•”ˆÊŒã—Ã—¿‚Q‚m‚q‚v + ’·ŠúŒã—Ã—¿‚Q‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚Q‚m‚q‚v .
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚Q = —âãª–@—¿‚Q‚m‚q‚v   + —âãª–@—¿‚QSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚Q = ‰·ãª–@—¿‚Q‚m‚q‚v   + ‰·ãª–@—¿‚QSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚Q   = “d—Ã—¿‚Q‚m‚q‚v     + “d—Ã—¿‚QSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚R   = Œã—Ã—¿‚R‚m‚q‚v     + Œã—Ã—¿‚RSk‚m‚q‚v.
      *     + ‘½•”ˆÊŒã—Ã—¿‚R‚m‚q‚v + ’·ŠúŒã—Ã—¿‚R‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚R‚m‚q‚v .
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚R = —âãª–@—¿‚R‚m‚q‚v   + —âãª–@—¿‚RSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚R = ‰·ãª–@—¿‚R‚m‚q‚v   + ‰·ãª–@—¿‚RSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚R   = “d—Ã—¿‚R‚m‚q‚v     + “d—Ã—¿‚RSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚S   = Œã—Ã—¿‚S‚m‚q‚v     + Œã—Ã—¿‚SSk‚m‚q‚v.
      *     + ‘½•”ˆÊŒã—Ã—¿‚S‚m‚q‚v + ’·ŠúŒã—Ã—¿‚S‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚S‚m‚q‚v .
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚S = —âãª–@—¿‚S‚m‚q‚v   + —âãª–@—¿‚SSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚S = ‰·ãª–@—¿‚S‚m‚q‚v   + ‰·ãª–@—¿‚SSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚S   = “d—Ã—¿‚S‚m‚q‚v     + “d—Ã—¿‚SSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚T   = Œã—Ã—¿‚T‚m‚q‚v     + Œã—Ã—¿‚TSk‚m‚q‚v.
      *     + ‘½•”ˆÊŒã—Ã—¿‚T‚m‚q‚v + ’·ŠúŒã—Ã—¿‚T‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚T‚m‚q‚v .
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚T = —âãª–@—¿‚T‚m‚q‚v   + —âãª–@—¿‚TSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚T = ‰·ãª–@—¿‚T‚m‚q‚v   + ‰·ãª–@—¿‚TSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚T   = “d—Ã—¿‚T‚m‚q‚v     + “d—Ã—¿‚TSk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚U   = Œã—Ã—¿‚U‚m‚q‚v     + Œã—Ã—¿‚USk‚m‚q‚v.
      *     + ‘½•”ˆÊŒã—Ã—¿‚U‚m‚q‚v + ’·ŠúŒã—Ã—¿‚U‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚U‚m‚q‚v .
           COMPUTE ‰ï‚O‚Q|—âãª–@—¿‚U = —âãª–@—¿‚U‚m‚q‚v   + —âãª–@—¿‚USk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|‰·ãª–@—¿‚U = ‰·ãª–@—¿‚U‚m‚q‚v   + ‰·ãª–@—¿‚USk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|“d—Ã—¿‚U   = “d—Ã—¿‚U‚m‚q‚v     + “d—Ã—¿‚USk‚m‚q‚v.
           COMPUTE ‰ï‚O‚Q|Œã—Ã—¿‚V   = Œã—Ã—¿‚V‚m‚q‚v     + Œã—Ã—¿‚VSk‚m‚q‚v.
      *     + ‘½•”ˆÊŒã—Ã—¿‚V‚m‚q‚v + ’·ŠúŒã—Ã—¿‚V‚m‚q‚v + ‘½•”ˆÊSkŒã—Ã—¿‚V‚m‚q‚v .
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
046960******************************************************************
046970 END PROGRAM ROU9709R.
046980******************************************************************
