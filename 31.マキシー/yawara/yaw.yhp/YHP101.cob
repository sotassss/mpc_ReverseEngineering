000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHP101.
000060 AUTHOR.                 ‰ª“c@Œ›˜a
000070*
000080*----------------------------------------------------------------*
000090*      ’ñoFPDì¬yÃÞ°Àì¬z_³¨ÝÄÞ³½Þ95”Å
000100*
000110* ¦ ¿‹”NŒŽVer‚Ì‚Ý. (•À‚Ñ‡‚ÍA‘Š‡•\581‚Æ“¯‚¶j
000120*@@‰æ–Ê‚ð‘Š‡•\‚Æˆê‚É‚µ‚½
000130*
000140*      MED = YHP100G 
000150*----------------------------------------------------------------*
000160 DATE-WRITTEN.           2012-09-20
000170 DATE-COMPILED.          2012-09-20
000180*----------------------------------------------------------------*
000190******************************************************************
000200*            ENVIRONMENT         DIVISION                        *
000210******************************************************************
000220 ENVIRONMENT             DIVISION.
000230 CONFIGURATION           SECTION.
000240 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000250 OBJECT-COMPUTER.        FMV-DESKPOWER.
000260 SPECIAL-NAMES.          CONSOLE  IS  CONS
000270                         SYSERR   IS  MSGBOX.
000280 INPUT-OUTPUT            SECTION.
000290 FILE-CONTROL.
000300     SELECT  §Œäî•ñƒ}ƒXƒ^  ASSIGN      TO        SEIGYOL
000310                             ORGANIZATION             IS  INDEXED
000320                             ACCESS MODE              IS  DYNAMIC
000330                             RECORD KEY               IS  §|§Œä‹æ•ª
000340                             FILE STATUS              IS  ó‘ÔƒL[
000350                             LOCK        MODE         IS  AUTOMATIC.
000360     SELECT  Œ³†ƒ}ƒXƒ^      ASSIGN      TO        GENGOUL
000370                             ORGANIZATION             IS  INDEXED
000380                             ACCESS MODE              IS  DYNAMIC
000390                             RECORD KEY               IS  Œ³|Œ³†‹æ•ª
000400                             FILE STATUS              IS  ó‘ÔƒL[
000410                             LOCK        MODE         IS  AUTOMATIC.
000420     SELECT  –¼Ìƒ}ƒXƒ^      ASSIGN      TO        MEISYOL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS  –¼|‹æ•ªƒR[ƒh
000460                                                          –¼|–¼ÌƒR[ƒh
000470                             FILE STATUS              IS  ó‘ÔƒL[
000480                             LOCK        MODE         IS  AUTOMATIC.
000490     SELECT  Ž{pŠî•ñƒ}ƒXƒ^ ASSIGN      TO        SEJOHOL
000500                             ORGANIZATION             IS  INDEXED
000510                             ACCESS MODE              IS  DYNAMIC
000520                             RECORD KEY               IS Ž{î|Ž{pŠ”Ô†
000530                             FILE STATUS              IS  ó‘ÔƒL[
000540                             LOCK        MODE         IS  AUTOMATIC.
000550     SELECT  Ž{p‹L˜^‚e      ASSIGN      TO        SEKIROKL
000560                             ORGANIZATION             IS  INDEXED
000570                             ACCESS MODE              IS  DYNAMIC
000580                             RECORD KEY           IS Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
000590                                                     Ž{‹L|Š³ŽÒƒR[ƒh
000600                             ALTERNATE RECORD KEY IS Ž{‹L|Š³ŽÒƒR[ƒh
000610                                                     Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
000620                             FILE STATUS              IS  ó‘ÔƒL[
000630                             LOCK        MODE         IS  AUTOMATIC.
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
000940                             RECORD KEY               IS •‰|Ž{p˜a—ï”NŒŽ
000950                                                         •‰|Š³ŽÒƒR[ƒh
000960                             ALTERNATE RECORD KEY     IS •‰|Š³ŽÒƒR[ƒh
000970                                                         •‰|Ž{p˜a—ï”NŒŽ
000980                             FILE STATUS              IS  ó‘ÔƒL[
000990                             LOCK        MODE         IS  AUTOMATIC.
001000     SELECT  Œo‰ßƒ}ƒXƒ^      ASSIGN      TO        KEIKAL
001001                             ORGANIZATION             IS  INDEXED
001002                             ACCESS MODE              IS  DYNAMIC
001003                             RECORD KEY               IS  Œo|‹æ•ªƒR[ƒh
001004                                                          Œo|Œo‰ßƒR[ƒh
001005                             FILE STATUS              IS  ó‘ÔƒL[
001006                             LOCK        MODE         IS  AUTOMATIC.
001007     SELECT  •‰Œ´ˆö‚e      ASSIGN      TO        HUGEINL
001010                             ORGANIZATION             IS  INDEXED
001020                             ACCESS MODE              IS  DYNAMIC
001030                             RECORD KEY               IS  •‰Œ´|‹æ•ªƒR[ƒh
001040                                                          •‰Œ´|•‰Œ´ˆöƒR[ƒh
001050                             FILE STATUS              IS  ó‘ÔƒL[
001060                             LOCK        MODE         IS  AUTOMATIC.
001070     SELECT  Žs’¬‘ºƒ}ƒXƒ^    ASSIGN      TO        SITYOSNL
001080                             ORGANIZATION             IS  INDEXED
001090                             ACCESS MODE              IS  DYNAMIC
001100                             RECORD KEY               IS  Žs|Œö”ïŽí•Ê
001110                                                          Žs|Žs’¬‘º”Ô†
001120                             ALTERNATE RECORD KEY     IS  Žs|Œö”ïŽí•Ê
001130                                                          Žs|Žs’¬‘º–¼Ì
001140                                                          Žs|Žs’¬‘º”Ô†
001150                             FILE STATUS              IS  ó‘ÔƒL[
001160                             LOCK        MODE         IS  AUTOMATIC.
001170     SELECT  ƒŒƒZƒvƒg‚e      ASSIGN      TO        RECEPTL
001180                             ORGANIZATION             IS  INDEXED
001190                             ACCESS MODE              IS  DYNAMIC
001200                             RECORD KEY               IS  ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001210                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
001220                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001230                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|Š³ŽÒƒR[ƒh
001240                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001250                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001260                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|¿‹˜a—ï”NŒŽ
001270                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001280                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
001290                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001300                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|¿‹˜a—ï”NŒŽ
001310                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001320                                                          ƒŒƒZ|¿‹•ÛŒ¯ŽÒ”Ô†
001330                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
001340                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001350                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|¿‹˜a—ï”NŒŽ
001360                                                          ƒŒƒZ|¿‹•ÛŒ¯ŽÒ”Ô†
001370                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
001380                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001390                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001400                             FILE STATUS              IS  ó‘ÔƒL[
001410                             LOCK        MODE         IS  AUTOMATIC.
001420     SELECT  ŒvŽZƒ}ƒXƒ^      ASSIGN      TO        KEISANL
001421                             ORGANIZATION             IS  INDEXED
001422                             ACCESS MODE              IS  DYNAMIC
001423                             RECORD KEY               IS  Œv|§Œä‹æ•ª
001424                                                          Œv|ŠJŽn˜a—ï”NŒŽ
001425                             FILE STATUS              IS  ó‘ÔƒL[.
001428     SELECT  ì‹Æƒtƒ@ƒCƒ‹‚P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001430                             ORGANIZATION             IS  SEQUENTIAL
001440                             ACCESS                   IS  SEQUENTIAL
001450                             FILE        STATUS       IS  ó‘ÔƒL[
001460                             LOCK        MODE         IS  AUTOMATIC.
001478*
001480*  ‘Š‡•\‚Æ“¯‚¶•ÛŒ¯ŽÒ”Ô†‡Ì§²Ù
001490     SELECT  ì‹Æƒtƒ@ƒCƒ‹‚R  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5803L.DAT"
001500                             ORGANIZATION             IS  INDEXED
001510                             ACCESS                   IS  DYNAMIC
001520                             RECORD      KEY          IS  ì‚R|¿‹˜a—ï”NŒŽ
001530                                                          ì‚R|•¬‹æ•ª
001540                                                          ì‚R|•ÛŒ¯ŽÒ”Ô†
001550                                                          ì‚R|–{l‰Æ‘°‹æ•ª
001560                                                          ì‚R|Ž{p˜a—ï”NŒŽ
001570                                                          ì‚R|”í•ÛŒ¯ŽÒƒJƒi
001580                                                          ì‚R|Š³ŽÒƒR[ƒh
001590                                                          ì‚R|eŽq‹æ•ª
001600                             FILE        STATUS       IS  ó‘ÔƒL[
001610                             LOCK        MODE         IS  AUTOMATIC.
001620*
001630******************************************************************
001640*                      DATA DIVISION                             *
001650******************************************************************
001660 DATA                    DIVISION.
001670 FILE                    SECTION.
001680*                           m‚q‚k  ‚Q‚T‚Un
001690 FD  §Œäî•ñƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
001700     COPY SEIGYO          OF  XFDLIB  JOINING   §   AS  PREFIX.
001710*                           m‚q‚k  ‚P‚Q‚Wn
001720 FD  Œ³†ƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
001730     COPY GENGOU          OF  XFDLIB  JOINING   Œ³   AS  PREFIX.
001740*                           m‚q‚k  ‚P‚Q‚Wn
001750 FD  –¼Ìƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
001760     COPY MEISYO          OF  XFDLIB  JOINING   –¼   AS  PREFIX.
001770*
001780 FD  Ž{pŠî•ñƒ}ƒXƒ^    BLOCK   CONTAINS   1   RECORDS.
001790     COPY SEJOHO         OF  XFDLIB  JOINING   Ž{î   AS  PREFIX.
001800*                           m‚q‚k  ‚Q‚T‚Un
001810 FD  Ž{p‹L˜^‚e          BLOCK   CONTAINS   1   RECORDS.
001820     COPY SEKIROK         OF  XFDLIB  JOINING   Ž{‹L AS  PREFIX.
001830*                           m‚q‚k  ‚R‚Q‚On
001840 FD  ŽófŽÒî•ñ‚e        BLOCK   CONTAINS   1   RECORDS.
001850     COPY JUSINJ          OF  XFDLIB  JOINING   Žó   AS  PREFIX.
001860*                           m‚q‚k  ‚P‚Q‚Wn
001870 FD  •‰ƒf[ƒ^‚e        BLOCK   CONTAINS   1   RECORDS.
001880     COPY HUSYOU          OF  XFDLIB  JOINING   •‰   AS  PREFIX.
001890*                           m‚q‚k  ‚P‚Q‚Wn
001891 FD  Œo‰ßƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
001892     COPY KEIKA          OF  XFDLIB  JOINING   Œo   AS  PREFIX.
001893*                           m‚q‚k  ‚P‚Q‚Wn
001900 FD  •‰Œ´ˆö‚e         BLOCK   CONTAINS   1   RECORDS.
001910     COPY HUGEIN          OF  XFDLIB  JOINING   •‰Œ´   AS  PREFIX.
001920*                           m‚q‚k  ‚Q‚T‚Un
001930 FD  Žs’¬‘ºƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
001940     COPY SITYOSN        OF  XFDLIB  JOINING   Žs   AS  PREFIX.
001950*                          m‚q‚k  ‚P‚T‚R‚Un
001960 FD  ƒŒƒZƒvƒg‚e          BLOCK   CONTAINS   1   RECORDS.
001970     COPY RECEPT          OF  XFDLIB  JOINING   ƒŒƒZ  AS  PREFIX.
001980*                           m‚q‚k  ‚Q‚T‚Un
001981 FD  ŒvŽZƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
001982     COPY KEISAN          OF  XFDLIB  JOINING   Œv   AS  PREFIX.
001983     COPY KEISANA         OF  XFDLIB  JOINING   Œv‚` AS  PREFIX.
001984**
002867 FD  ì‹Æƒtƒ@ƒCƒ‹‚P RECORD  CONTAINS 1920 CHARACTERS.
002868 01  ì‚P|ƒŒƒR[ƒh.
002869*   / ƒwƒbƒ_•”‚ÍŽg—p‚µ‚È‚¢ /
002870     03  ì‚P|ƒŒƒR[ƒhƒwƒbƒ_.
002871         05  ì‚P|¿‹˜a—ï”NŒŽƒL[.
002872             07  ì‚P|¿‹˜a—ï            PIC 9.
002873             07  ì‚P|¿‹”N              PIC 9(2).
002874             07  ì‚P|¿‹ŒŽ              PIC 9(2).
002875         05  ì‚P|Ž{p˜a—ï”NŒŽƒL[.
002876             07  ì‚P|Ž{p˜a—ï            PIC 9.
002877             07  ì‚P|Ž{p”N              PIC 9(2).
002878             07  ì‚P|Ž{pŒŽ              PIC 9(2).
002879         05  ì‚P|•ÛŒ¯‹æ•ªƒL[            PIC 9.
002880         05  ì‚P|•ÛŒ¯ŽÒ”Ô†ƒL[          PIC 9(8).
002881         05  ì‚P|–{l‰Æ‘°‹æ•ªƒL[        PIC 9.
002882         05  ì‚P|”í•ÛŒ¯ŽÒƒJƒiƒL[        PIC X(20).
002883         05  ì‚P|Š³ŽÒƒR[ƒhƒL[.
002884             07 ì‚P|Š³ŽÒ”Ô†ƒL[         PIC 9(6).
002885             07 ì‚P|Ž}”Ô                 PIC X(1).
002886     03  ì‚P|ƒŒƒR[ƒhƒf[ƒ^.
002887         05  ì‚P|¿‹”NŒŽ                PIC 9(6).
002888         05  ì‚P|Ž{p”NŒŽ                PIC 9(6).
002889         05  ì‚P|‰ïˆõ”Ô†                PIC 9(7).
002890         05  ì‚P|“o˜^‹L†”Ô†            PIC X(11).
002891         05  ì‚P|•ÛŒ¯ŽÒ”Ô†              PIC X(8).
002892         05  ì‚P|‹L†                    PIC X(30).
002893         05  ì‚P|”Ô†                    PIC X(16).
002894         05  ì‚P|ˆã—Ã•¬‹æ•ª            PIC 9.
002895         05  ì‚P|•¬•‰’SŽÒ”Ô†          PIC X(8).
002896         05  ì‚P|•¬Žó‹‹ŽÒ”Ô†          PIC X(16).
002897         05  ì‚P|•¬•‰’SŽÒ”Ô†‚Q        PIC X(8).
002898         05  ì‚P|•¬Žó‹‹ŽÒ”Ô†‚Q        PIC X(16).
002899         05  ì‚P|•ÛŒ¯Ží•Ê‹æ•ª            PIC 9.
002900         05  ì‚P|’P•¹‹æ•ª                PIC 9.
002901         05  ì‚P|–{‰Æ‹æ•ª                PIC 9.
002902         05  ì‚P|‹‹•tŠ„‡                PIC 9(2).
002903         05  ì‚P|–{l‰Æ‘°‹æ•ª            PIC 9.
002904         05  ì‚P|”í•ÛŒ¯ŽÒƒJƒi            PIC X(25).
002905         05  ì‚P|”í•ÛŒ¯ŽÒŽ–¼            PIC X(30).
002906         05  ì‚P|Š³ŽÒƒJƒi                PIC X(25).
002907         05  ì‚P|Š³ŽÒŽ–¼                PIC X(30).
002908         05  ì‚P|Š³ŽÒ«•Ê                PIC 9.
002909         05  ì‚P|Š³ŽÒ¶”NŒŽ“ú            PIC 9(8).
002910         05  ì‚P|‡Œv‹àŠz                PIC 9(6).
002911         05  ì‚P|ˆê•”•‰’S‹à              PIC 9(6).
002912         05  ì‚P|¿‹‹àŠz                PIC 9(6).
002913         05  ì‚P|Œö”ï•‰’S‹àŠz            PIC 9(6).
002914         05  ì‚P|Œö”ï¿‹‹àŠz            PIC 9(6).
002915         05  ì‚P|‘S‘ÌŽÀ“ú”              PIC 9(2).
002916         05  ì‚P|•”ˆÊ”                  PIC 9.
002917         05  ì‚P|Ä¿‹‹æ•ª              PIC 9.
002918         05  ì‚P|‹ÆŽÒ‹æ•ª                PIC 9(2).
002919         05  ì‚P|‚—îŽÒ‹æ•ª              PIC 9.
002920         05  ì‚P|Š³ŽÒ”Ô†                PIC 9(5).
      */‰^“®Œã—Ã’Ç‰Á«««/20180607
               05 ì‚P|‰^“®Œã—Ã—¿‰ñ”           PIC 9(1).
               05 ì‚P|‰^“®Œã—Ã—¿               PIC 9(5).
      */–¾×‘”­s‘Ì§‰ÁŽZ’Ç‰Á«««/20221020
             05 ì‚P|–¾×‘”­s‰ñ”          PIC 9(1).
             05 ì‚P|–¾×‘”­s              PIC 9(3).
             05 ì‚P|–¾×‘”­sŒŽ“ú          PIC 9(4).
001598*       05 ì‚P|—\”õ                    PIC X(57).
001598*       05 ì‚P|—\”õ                    PIC X(48).
001598       05 ì‚P|—\”õ                    PIC X(40).
      */–¾×‘”­s‘Ì§‰ÁŽZ’Ç‰Áªªª/20221020
      */‰^“®Œã—Ã’Ç‰Áªªª/20180607
002922*
002923         05  ì‚P|•‰ƒf[ƒ^  OCCURS 5.
002924             07  ì‚P|•‰‹æ•ª            PIC 9.
002925             07  ì‚P|•‰–¼              PIC X(32).
002926             07  ì‚P|•‰”NŒŽ“ú          PIC 9(8).
002927             07  ì‚P|‰ŒŸ”NŒŽ“ú          PIC 9(8).
002928             07  ì‚P|Ž{pŠJŽn”NŒŽ“ú      PIC 9(8).
002929             07  ì‚P|Ž{pI—¹”NŒŽ“ú      PIC 9(8).
002930             07  ì‚P|ŽÀ“ú”              PIC 9(2).
002931             07  ì‚P|“]‹A‹æ•ª            PIC 9.
002932             07  ì‚P|®•œŒÅ’èŽ{—Ã‰ñ”    PIC 9.
002933             07  ì‚P|®•œŒÅ’èŽ{—Ã—¿      PIC 9(5).
002934*
002935         05  ì‚P|V‹K‹æ•ª                PIC 9.
002936         05  ì‚P|Œp‘±‹æ•ª                PIC 9.
002937         05  ì‚P|Ž{p“ú                  PIC X(31).
002938         05  ì‚P|‰ŒŸ‰ñ”                PIC 9.
002939         05  ì‚P|‰ŒŸ—¿                  PIC 9(5).
002940         05  ì‚P|‰ŒŸ‹x“ú‰ÁŽZ‰ñ”        PIC 9.
002941         05  ì‚P|‰ŒŸ[–é‰ÁŽZ‰ñ”        PIC 9.
002942         05  ì‚P|‰ŒŸŽžŠÔŠO‰ÁŽZ‰ñ”      PIC 9.
002943         05  ì‚P|‰ŒŸ‰ÁŽZ                PIC 9(5).
002944         05  ì‚P|‘Š’kŽx‰‡‰ñ”            PIC 9.
002945         05  ì‚P|‘Š’kŽx‰‡—¿              PIC 9(5).
002946
002947         05  ì‚P|ÄŒŸ‰ñ”                PIC 9.
002948         05  ì‚P|ÄŒŸ—¿                  PIC 9(5).
002949         05  ì‚P|‰—Ã‹——£                PIC 9(3).
002950         05  ì‚P|‰—Ã‰ñ”                PIC 9(2).
002951         05  ì‚P|‰—Ã—¿                  PIC 9(5).
002952         05  ì‚P|–éŠÔ‰ÁŽZ‰—Ã‰ñ”        PIC 9.
002953         05  ì‚P|“ï˜H‰ÁŽZ‰—Ã‰ñ”        PIC 9.
002954         05  ì‚P|–\•—‰Já‰ÁŽZ‰—Ã‰ñ”    PIC 9.
002955         05  ì‚P|‰—Ã‰ÁŽZ                PIC 9(5).
      */‹à‘®•›Žq•ÏX«««/20180611
000561         05 ì‚P|‹à‘®•›Žq‰ñ”             PIC 9.
000561         05 ì‚P|ƒ_ƒ~[                   PIC X(2).
002956*         05  ì‚P|‹à‘®•›Žq‘å‰ñ”          PIC 9.
002957*         05  ì‚P|‹à‘®•›Žq’†‰ñ”          PIC 9.
002958*         05  ì‚P|‹à‘®•›Žq¬‰ñ”          PIC 9.
      */‹à‘®•›Žq•ÏXªªª/20180611
002959         05  ì‚P|‹à‘®•›Žq‰ÁŽZ            PIC 9(5).
002960         05  ì‚P|î•ñ’ñ‹Ÿ—¿‰ñ”          PIC 9.
002961         05  ì‚P|î•ñ’ñ‹Ÿ—¿              PIC 9(5).
002962*
002963         05  ì‚P|•‰•”ˆÊƒf[ƒ^  OCCURS 6.
002964             07  ì‚P|’üŒ¸ŠJŽnŒŽ“ú        PIC 9(4).
002965             07  ì‚P|Œã—Ã‰ñ”            PIC 9(2).
002966             07  ì‚P|Œã—Ã—¿              PIC 9(5).
002967             07  ì‚P|—âãª–@‰ñ”          PIC 9.
002968             07  ì‚P|—âãª–@—¿            PIC 9(5).
002969             07  ì‚P|‰·ãª–@‰ñ”          PIC 9(2).
002970             07  ì‚P|‰·ãª–@—¿            PIC 9(5).
002971             07  ì‚P|“d—Ã‰ñ”            PIC 9(2).
002972             07  ì‚P|“d—Ã—¿              PIC 9(5).
002973             07  ì‚P|‘½•”ˆÊ’üŒ¸—¦        PIC 9(2).
002974             07  ì‚P|‘½•”ˆÊ’üŒ¸Šz        PIC 9(5).
002975             07  ì‚P|’·Šú’üŒ¸—¦          PIC 9(2).
002976             07  ì‚P|—¿‹àŒv              PIC 9(5).
002977*
002978         05  ì‚P|”í•ÛŒ¯ŽÒZŠ            PIC X(60).
002979         05  ì‚P|•‰Œ´ˆö                PIC X(200).
002980         05  ì‚P|Œo‰ß                    PIC X(50).
002981         05  ì‚P|’·Šú——R                PIC X(400).
002982         05  ì‚P|‰üs•¶Žš                PIC X(2).
002983         05  FILLER                        PIC X(73).
002984*
002985***
002986* •ÛŒ¯ŽÒ”Ô†‡ƒtƒ@ƒCƒ‹
002987 FD  ì‹Æƒtƒ@ƒCƒ‹‚R RECORD  CONTAINS 64 CHARACTERS.
002988 01  ì‚R|ƒŒƒR[ƒh.
002989     03  ì‚R|ƒŒƒR[ƒhƒL[.
002990         05  ì‚R|¿‹˜a—ï”NŒŽ.
002991             07  ì‚R|¿‹˜a—ï            PIC 9.
002992             07  ì‚R|¿‹”N              PIC 9(2).
002993             07  ì‚R|¿‹ŒŽ              PIC 9(2).
002994         05  ì‚R|•¬‹æ•ª                PIC 9.
002995         05  ì‚R|•ÛŒ¯ŽÒ”Ô†              PIC 9(8).
002996         05  ì‚R|–{l‰Æ‘°‹æ•ª            PIC 9.
002997         05  ì‚R|Ž{p˜a—ï”NŒŽ.
002998             07  ì‚R|Ž{p˜a—ï            PIC 9.
002999             07  ì‚R|Ž{p”N              PIC 9(2).
003000             07  ì‚R|Ž{pŒŽ              PIC 9(2).
003010         05  ì‚R|”í•ÛŒ¯ŽÒƒJƒi            PIC X(20).
003020         05  ì‚R|Š³ŽÒƒR[ƒh.
003030             07 ì‚R|Š³ŽÒ”Ô†             PIC 9(6).
003040             07 ì‚R|Ž}”Ô                 PIC X(1).
003050         05  ì‚R|eŽq‹æ•ª                PIC 9.
003060     03  ì‚R|ƒŒƒR[ƒhƒf[ƒ^.
003070         05  FILLER                        PIC X(16).
003080*
003090*----------------------------------------------------------------*
003100******************************************************************
003110*                WORKING-STORAGE SECTION                         *
003120******************************************************************
003130 WORKING-STORAGE         SECTION.
003140 01 ƒL[“ü—Í                           PIC X    VALUE SPACE.
003150 01 ó‘ÔƒL[                           PIC X(2) VALUE SPACE.
003160 01 ‰ŒŸƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
003170 01 I—¹ƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
003180 01 I—¹ƒtƒ‰ƒO‚Q                       PIC X(3) VALUE SPACE.
003190 01 ŽÀsƒL[‚v                         PIC X(3)  VALUE SPACE.
003200 01 Ž{p‹L˜^—L‚v                       PIC X(3) VALUE SPACE.
003210 01 ƒtƒ@ƒCƒ‹–¼                         PIC N(8) VALUE SPACE.
003220*
003230 01 •ÛŒ¯Ží•Ê‚v‚q                       PIC 9(2) VALUE ZERO.
003240 01 Š³ŽÒƒR[ƒh‚v‚q.
003250    03 Š³ŽÒ”Ô†‚v‚q                    PIC 9(6) VALUE ZERO.
003260    03 Ž}”Ô‚v‚q                        PIC X    VALUE SPACE.
003270*
003280 01 _®Žt”Ô†‚v                       PIC X(11)  VALUE SPACE.
003281 01 ˆóüŒ`Ž®‚v‚q                       PIC 9    VALUE ZERO.
003290 01 •ÛŒ¯ŽÒ”Ô†‚v‚q                     PIC X(10) VALUE SPACE.
003302 01 Žs’¬‘º”Ô†‚v                     PIC X(10) VALUE SPACE.
003303 01 Žó‹‹ŽÒ”Ô†‚v                     PIC X(10) VALUE SPACE.
003304 01 ƒŒƒZƒvƒgŽí—Þ‚v‚q                   PIC X(4) VALUE SPACE.
003310 01 –{l‰Æ‘°‹æ•ª‚v‚q                   PIC 9    VALUE ZERO.
003320 01 ‘±•¿‚v                             PIC N(2) VALUE SPACE.
003330 01 Ž{p˜a—ï”NŒŽ‚v‚q.
003340    03 Ž{p˜a—ï‚v‚q                    PIC 9    VALUE ZERO.
003350    03 Ž{p”N‚v‚q                      PIC 9(2) VALUE ZERO.
003360    03 Ž{pŒŽ‚v‚q                      PIC 9(2) VALUE ZERO.
003370 01 ¿‹˜a—ï”NŒŽ‚v‚q.
003380    03 ¿‹˜a—ï‚v‚q                    PIC 9    VALUE ZERO.
003390    03 ¿‹”N‚v‚q                      PIC 9(2) VALUE ZERO.
003400    03 ¿‹ŒŽ‚v‚q                      PIC 9(2) VALUE ZERO.
003410
003411 01 ˆã—Ã•¬‹æ•ª‚v                     PIC 9    VALUE ZERO.
003422 01 •ÛŒ¯Ží•Ê‹æ•ª‚v                     PIC 9    VALUE ZERO.
003423 01 ’P•¹‹æ•ª‚v                         PIC 9    VALUE ZERO.
003424 01 –{‰Æ‹æ•ª‚v                         PIC 9    VALUE ZERO.
003425 01 ‹‹•tŠ„‡‚v                         PIC 9(2) VALUE ZERO.
003426 01 –{l‰Æ‘°‹æ•ª‚v                     PIC 9    VALUE ZERO.
003427 01 ‘S‘ÌŽÀ“ú”‚v                       PIC 9(2) VALUE ZERO.
003428 01 Ä¿‹‹æ•ª‚v                       PIC 9    VALUE ZERO.
003430 01 ‹ÆŽÒ‹æ•ª‚v                         PIC 9(2) VALUE ZERO.
003431 01 ‚Q•”ˆÊ–Ú’üŒ¸—¦‚v                   PIC 9(3) VALUE ZERO.
003432 01 ‚R•”ˆÊ–Ú’üŒ¸—¦‚v                   PIC 9(3) VALUE ZERO.
003434**
003435 01 ˜A”Ô‚v                             PIC 9(4) VALUE ZERO.
003436 01 •¬ƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
003440 01 •‰–¼Ì‚v                         PIC N(16) VALUE SPACE.
003451 01 •‰Ží•Ê•ÏŠ·‘O‚v                   PIC 9(2)  VALUE ZERO.
003460 01 •‰Ží•Ê•ÏŠ·Œã‚v                   PIC 9     VALUE ZERO.
003470 01 “]‹A•ÏŠ·‘O‚v                       PIC 9     VALUE ZERO.
003480 01 “]‹A•ÏŠ·Œã‚v                       PIC 9     VALUE ZERO.
003492 01 •ÛŒ¯Ží•Ê•ÏŠ·‘O‚v                   PIC 9     VALUE ZERO.
003493 01 •ÛŒ¯Ží•Ê•ÏŠ·Œã‚v                   PIC 9     VALUE ZERO.
003494
003495**
003500 01 •”ˆÊ‚b‚m‚s                         PIC 9     VALUE ZERO.
003510 01 ƒJƒEƒ“ƒ^                           PIC 9(2)  VALUE ZERO.
003520 01 ƒJƒEƒ“ƒ^‚Q                         PIC 9(3)  VALUE ZERO.
003530 01 ƒJƒEƒ“ƒ^‚R                         PIC 9(2)  VALUE ZERO.
003540 01 ‰üs                               PIC X(2)  VALUE X"0D0A" GLOBAL.
003543 01 ‘SŠp‹ó”’                           PIC X(2)  VALUE X"8140".
003550 01 ”¼Šp‹ó”’                           PIC X(2)  VALUE X"2020".
003560
003563 01 “ú–{Œê•ÏŠ·‚v‚w.
003564    03 “ú–{Œê•ÏŠ·‚v‚m                  PIC N(50) VALUE SPACE. 
003565**
003570 01 Ž–¼‚v.
003580    03 ‘SŠpŽ–¼‚v                      PIC X(30) VALUE SPACE.
003590** ƒGƒ‰[ƒƒbƒZ[ƒW—p
003600 01 ƒGƒ‰[ƒƒbƒZ[ƒW‚v.
003610    03 ƒGƒ‰[Š³ŽÒƒR[ƒh‚v              PIC X(7) VALUE SPACE.
003620    03 ƒGƒ‰[‹æØ‚è‚v                  PIC X(1) VALUE SPACE.
003630    03 ƒGƒ‰[•ÛŒ¯Ží•Ê‚v                PIC X(2) VALUE SPACE.
003640    03 FILLER                          PIC X(10) VALUE SPACE.
003650** •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß—p
003660 01 •ÛŒ¯ŽÒ”Ô†‚v‚s.
003670    03 •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v.
003680      05 •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P          PIC X OCCURS 8 VALUE SPACE.
003690    03 •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß‚v.
003700      05 •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß‚v‚P          PIC X OCCURS 8 VALUE ZERO.
003710    03 •ÛŒ¯ŽÒ”Ô†”Žš‚v                PIC 9(8)  VALUE ZERO.
003720    03 •ÛŒ¯ŽÒ”Ô†‚v                    PIC X(8)  VALUE SPACE.
003730** ‰ïˆõ”Ô†‰E‹l‚ß—p
003740 01 ‰ïˆõ”Ô†‚v‚s.
003750    03 ‰ïˆõ”Ô†¶‹l‚ß‚v.
003760      05 ‰ïˆõ”Ô†¶‹l‚ß‚v‚P            PIC X OCCURS 7 VALUE SPACE.
003770    03 ‰ïˆõ”Ô†‰E‹l‚ß‚v.
003780      05 ‰ïˆõ”Ô†‰E‹l‚ß‚v‚P            PIC X OCCURS 7 VALUE ZERO.
003790    03 ‰ïˆõ”Ô†”Žš‚v                  PIC 9(7)  VALUE ZERO.
003800    03 ‰ïˆõ”Ô†‚v                      PIC X(7)  VALUE SPACE.
003810** ¼—ï“ú•tƒ[ƒN—p
003820 01 ¼—ï”NŒŽ‚v.
003830    03 ¼—ï”N‚v                        PIC 9(4) VALUE ZERO.
003840    03 ¼—ïŒŽ‚v                        PIC 9(2) VALUE ZERO.
003850** ¼—ï¿‹”NŒŽ—p
003860 01 ¼—ï¿‹”NŒŽ‚v.
003870    03 ¼—ï¿‹”N‚v                    PIC 9(4) VALUE ZERO.
003880    03 ¼—ï¿‹ŒŽ‚v                    PIC 9(2) VALUE ZERO.
003890** ¼—ïŽ{p”NŒŽ—p
003900 01 ¼—ïŽ{p”NŒŽ‚v.
003910    03 ¼—ïŽ{p”N‚v                    PIC 9(4) VALUE ZERO.
003920    03 ¼—ïŽ{pŒŽ‚v                    PIC 9(2) VALUE ZERO.
003930** ‹L†¶‹l‚ß—p
003940 01 ‹L†‚v‚s.
003950    03 ‹L†Œ³‚v.
003960      05 ‹L†Œ³‚v‚P                    PIC N OCCURS 12 VALUE SPACE.
003970    03 ‹L†¶‹l‚ß‚v.
003980      05 ‹L†¶‹l‚ß‚v‚P                PIC N OCCURS 12 VALUE SPACE.
003990    03 ‹L†Œ³‚w‚v.
004000      05 ‹L†Œ³‚w‚v‚P                  PIC X OCCURS 24 VALUE SPACE.
004010    03 ‹L†¶‹l‚ß‚w‚v.
004020      05 ‹L†¶‹l‚ß‚w‚v‚P              PIC X OCCURS 24 VALUE SPACE.
004030    03 ‹L†‚v.
004040      05 ‹L†‚m‚v                      PIC N(12) VALUE SPACE.
004050    03 ‹L†‚o‚v.
004060      05 ‹L†‚o‚m‚v                    PIC X(24) VALUE SPACE.
004070** •¬•‰’SŽÒ”Ô†¶‹l‚ß—p
004080 01 •¬”Ô†‚v‚s.
004090    03 •¬”Ô†Œ³‚v.
004100      05 •¬”Ô†Œ³‚v‚P                PIC X OCCURS 10 VALUE SPACE.
004110    03 •¬”Ô†¶‹l‚ß‚v.
004120      05 •¬”Ô†¶‹l‚ß‚v‚P            PIC X OCCURS 10 VALUE SPACE.
004130    03 •¬”Ô†‚v                      PIC X(10) VALUE SPACE.
004140*
004150** ¼—ï”NŒŽ“úƒ[ƒN—p
004160 01 ŒvŽZ¼—ï”NŒŽ“ú‚v.
004170    03 ŒvŽZ¼—ï”N‚v                    PIC 9(4) VALUE ZERO.
004180    03 ŒvŽZ¼—ïŒŽ‚v                    PIC 9(2) VALUE ZERO.
004190    03 ŒvŽZ¼—ï“ú‚v                    PIC 9(2) VALUE ZERO.
004200 01 ŒvŽZ˜a—ï”NŒŽ“ú‚v.
004210    03 ŒvŽZ˜a—ï‚v                      PIC 9 VALUE ZERO.
004220    03 ŒvŽZ”N‚v                        PIC 9(2) VALUE ZERO.
004230    03 ŒvŽZŒŽ‚v                        PIC 9(2) VALUE ZERO.
004240    03 ŒvŽZ“ú‚v                        PIC 9(2) VALUE ZERO.
004250** Ž}”Ô”»’è—p
004260 01 ŠJŽnf—Ã“úŽè“®‹æ•ª‚v               PIC 9    VALUE ZERO.
004270*
004280* I—¹“ú‘Þ”ð—p
004290 01 I—¹”NŒŽ“ú‚v‚s.
004300    03 I—¹˜a—ï‚v‚s                    PIC 9     VALUE ZERO.
004310    03 I—¹”N‚v‚s                      PIC 9(2)  VALUE ZERO.
004320    03 I—¹ŒŽ‚v‚s                      PIC 9(2)  VALUE ZERO.
004330    03 I—¹“ú‚v‚s                      PIC 9(2)  VALUE ZERO.
004340* ‰ŒŸ“ú‘Þ”ð—p
004350 01 ‰ŒŸ”NŒŽ“ú‚v‚s.
004360    03 ‰ŒŸ˜a—ï‚v‚s                    PIC 9     VALUE ZERO.
004370    03 ‰ŒŸ”N‚v‚s                      PIC 9(2)  VALUE ZERO.
004380    03 ‰ŒŸŒŽ‚v‚s                      PIC 9(2)  VALUE ZERO.
004390    03 ‰ŒŸ“ú‚v‚s                      PIC 9(2)  VALUE ZERO.
004400*
004410* ˜AŒv‚Ì‹àŠz‘Þ”ð—p
004420 01 ˜AŒv‹àŠz‚v.
004430    03  ”ï—pŠz‚v                   PIC 9(6) VALUE ZERO.
004440    03  •‰’SŠz‚v                   PIC 9(6) VALUE ZERO.
004450    03  ¿‹Šz‚v                   PIC 9(6) VALUE ZERO.
004460    03  ”ï—pŠz˜Vl‚v               PIC 9(6) VALUE ZERO.
004470    03  •‰’SŠz˜Vl‚v               PIC 9(6) VALUE ZERO.
004480    03  ¿‹Šz˜Vl‚v               PIC 9(6) VALUE ZERO.
004490    03  ”ï—pŠz•¬‚v               PIC 9(6) VALUE ZERO.
004500    03  •‰’SŠz•¬‚v               PIC 9(5) VALUE ZERO.
004510    03  ¿‹Šz•¬‚v               PIC 9(5) VALUE ZERO.
004520    03  •‰’S—¦‚v                   PIC 9(3) VALUE ZERO.
004530*
004540* •‰Œ´ˆö—p
004550 01 •‰Œ´ˆö‚v‚s.
004560    03 •‰Œ´ˆö‚P‚v‚s                  PIC X(60) VALUE SPACE.
004570    03 •‰Œ´ˆö‚Q‚v‚s                  PIC X(60) VALUE SPACE.
004580    03 •‰Œ´ˆö‚R‚v‚s                  PIC X(60) VALUE SPACE.
004590    03 •‰Œ´ˆö‚S‚v‚s                  PIC X(60) VALUE SPACE.
004600    03 •‰Œ´ˆö‚T‚v‚s                  PIC X(60) VALUE SPACE.
004610    03 •‰Œ´ˆöƒiƒ“ƒo[‚v‚s.
004620       05 •‰Œ´ˆöƒiƒ“ƒo[‚v‚P         PIC X(2)  OCCURS 9 VALUE SPACE.
004630    03 •‰Œ´ˆöƒiƒ“ƒo[‚m‚v  REDEFINES •‰Œ´ˆöƒiƒ“ƒo[‚v‚s PIC X(18).
004640 01 •‰Š³ŽÒ”Ô†‚b‚v                   PIC 9(6)  VALUE ZERO.
004650 01 •‰˜A”Ô‚b‚v                       PIC 9(4)  VALUE ZERO.
004660 01 •‰Œ´ˆö‚s‚a‚k.
004670    03 •‰Œ´ˆöƒR[ƒh‚s‚a‚k            OCCURS 9.
004680       05 •‰Š³ŽÒ”Ô†‚v               PIC 9(6)  VALUE ZERO.
004690       05 •‰˜A”Ô‚v                   PIC 9(4)  VALUE ZERO.
004700       05 •‰Œ´ˆö•”ˆÊ‚v               PIC 9  OCCURS 9 VALUE ZERO.
004710 01 •‰Œ´ˆö“à—e‚v.
004720    03 •‰Œ´ˆö“à—e‡¬‚v              PIC X(318) OCCURS 9 VALUE SPACE.
004730    03 •‰Œ´ˆö“à—e•ª‰ð‚w‚v.
004740       05 •‰Œ´ˆö“à—e‚P‚w‚v           PIC X(74)  VALUE SPACE.
004750       05 •‰Œ´ˆö“à—e‚Q‚w‚v           PIC X(74)  VALUE SPACE.
004760       05 •‰Œ´ˆö“à—e‚R‚w‚v           PIC X(74)  VALUE SPACE.
004770       05 •‰Œ´ˆö“à—e‚S‚w‚v           PIC X(96)  VALUE SPACE.
004780*
004790** •‰Œ´ˆöE’·Šú——Rˆóü‹æ•ª—p
004800 01 •‰Œ´ˆöˆóü‹æ•ª‚v                 PIC 9 VALUE ZERO.
004810 01 ’·Šú——Rˆóü‹æ•ª‚v                 PIC 9 VALUE ZERO.
004820*
004830* •‰Œ´ˆöˆóü‹æ•ª
004831 01 ƒŒƒZ•‰Œ´ˆöˆóü‹æ•ª‚v             PIC 9    VALUE ZERO.
004832 01 ƒŒƒZ’·Šú——Rˆóü‹æ•ª‚v             PIC 9    VALUE ZERO.
004833*
004834** •¬ƒŒƒZ‚Ü‚Æ‚ß—p
004840 01 •¬ƒŒƒZ‚Ü‚Æ‚ßƒtƒ‰ƒO               PIC X(3)  VALUE SPACE.
004850*
004862 01 Œo‰ß•”ˆÊ‚v                         PIC N(1)  VALUE SPACE.
004863 01 •‰Œo‰ß‚v.
004864    03 •‰Œo‰ß•”ˆÊ‚v                  PIC X(10) OCCURS 5 VALUE SPACE.
004868*
004877**********************************************************************************
004878*
004880 01 ‘Þ”ð€–Ú‚f‚v.
004890   03 ƒŒƒZƒvƒgŽí—Þ‚v                   PIC X(4).
004900   03 ƒŒƒZƒvƒgŽí—Þ‚f‚v                 PIC X(4).
004910   03 ƒŒƒZƒvƒgŽí•Ê‚f‚v                 PIC 9(2).
004920*
004930****************
004940* •‰ƒf[ƒ^‚e *
004950****************
004960 01 •‰î•ñ‚v.
004970    03 •”ˆÊ”‚v                        PIC 9(1)  VALUE ZERO.
004980    03 •”ˆÊî•ñ‚v  OCCURS   9.
004990       05 •”ˆÊ‚b‚m‚s‚v                 PIC 9(1)  VALUE ZERO.
005000       05 •”ˆÊƒR[ƒh‚v.
005010          07 •‰Ží•Ê‚v                PIC 9(2)  VALUE ZERO.
005020          07 •”ˆÊ‚v                    PIC 9(2)  VALUE ZERO.
005030          07 ¶‰E‹æ•ª‚v                PIC 9(1)  VALUE ZERO.
005040          07 •‰ˆÊ’u”Ô†‚v            PIC 9(2)  VALUE ZERO.
005050       05 •‰–¼‚v                     PIC N(16) VALUE SPACE.
005060       05 •‰”NŒŽ“ú‚v.
005070          07 •‰˜a—ï‚v                PIC 9     VALUE ZERO.
005080          07 •‰”N‚v                  PIC 9(2)  VALUE ZERO.
005090          07 •‰ŒŽ‚v                  PIC 9(2)  VALUE ZERO.
005100          07 •‰“ú‚v                  PIC 9(2)  VALUE ZERO.
005110       05 ‰ŒŸ”NŒŽ“ú‚v.
005120          07 ‰ŒŸ˜a—ï‚v                PIC 9     VALUE ZERO.
005130          07 ‰ŒŸ”N‚v                  PIC 9(2)  VALUE ZERO.
005140          07 ‰ŒŸŒŽ‚v                  PIC 9(2)  VALUE ZERO.
005150          07 ‰ŒŸ“ú‚v                  PIC 9(2)  VALUE ZERO.
005160       05 ŠJŽn”NŒŽ“ú‚v.
005170          07 ŠJŽn˜a—ï‚v                PIC 9     VALUE ZERO.
005180          07 ŠJŽn”N‚v                  PIC 9(2)  VALUE ZERO.
005190          07 ŠJŽnŒŽ‚v                  PIC 9(2)  VALUE ZERO.
005200          07 ŠJŽn“ú‚v                  PIC 9(2)  VALUE ZERO.
005210       05 I—¹”NŒŽ“ú‚v.
005220          07 I—¹˜a—ï‚v                PIC 9     VALUE ZERO.
005230          07 I—¹”N‚v                  PIC 9(2)  VALUE ZERO.
005240          07 I—¹ŒŽ‚v                  PIC 9(2)  VALUE ZERO.
005250          07 I—¹“ú‚v                  PIC 9(2)  VALUE ZERO.
005260       05 ŽÀ“ú”‚v                     PIC 9(2)  VALUE ZERO.
005270       05 ‰‰ñˆ’u‰ñ”‚v               PIC 9     VALUE ZERO.
005280       05 “]‹A‹æ•ª‚v                   PIC 9(1)  VALUE ZERO.
005290    03 V‹K‹æ•ª‚v                      PIC 9(1)  VALUE ZERO.
005300    03 Œp‘±‹æ•ª‚v                      PIC 9(1)  VALUE ZERO.
005310    03 •‰Œ´ˆö‚v OCCURS 27.
005320       05 •‰Œ´ˆö‚v‚o                 PIC X(74) VALUE SPACE.
005330*
005340*********************************************************************
005350*    ************
005360*    * —¿‹àî•ñ *
005370*    ************
005380*    ŒŽ–ˆ‚Ì—¿‹à
005390***********************
005400 01 —¿‹à‚P‚v‚q.
005410   03 ‰ŒŸ‚v‚q.
005420      05 ‰ŒŸ‰ñ”‚v                 PIC 9(1)    VALUE ZERO.
005430      05 ‰ŒŸŽžŠÔŠO‰ñ”‚v           PIC 9(1)    VALUE ZERO.
005440      05 ‰ŒŸ‹x“ú‰ñ”‚v             PIC 9(1)    VALUE ZERO.
005450      05 ‰ŒŸ[–é‰ñ”‚v             PIC 9(1)    VALUE ZERO.
005461      05 •‰’SŠ„‡‚v                 PIC 9(3)    VALUE ZERO.
005462      05 ‰ŒŸ—¿‚v                   PIC 9(5)    VALUE ZERO.
005463      05 ‰ŒŸ‰ÁŽZ—¿‚v               PIC 9(5)    VALUE ZERO.
005464   03 ‰ŒŸŽž‘Š’k—¿‚v                PIC 9(4)    VALUE ZERO.
005466   03 ÄŒŸ‰ñ”‚v                    PIC 9(1)    VALUE ZERO.
005471   03 ÄŒŸ—¿‚v‚q                    PIC 9(5)    VALUE ZERO.
005472   03 ‰—Ã‚v‚q.
005480      05 ‰—Ã‰ñ”‚v                 PIC 9(2)    VALUE ZERO.
005490      05 ‰—Ã‹——£‚v                 PIC 9(3)V9  VALUE ZERO.
005500      05 ‰—Ã‹——£‚Q‚v               PIC 9(3)    VALUE ZERO.
005510      05 ‰—Ã–éŠÔ‚v                 PIC 9(1)    VALUE ZERO.
005520      05 ‰—Ã“ï˜H‚v                 PIC 9(2)    VALUE ZERO.
005530      05 ‰—Ã–\•—‚v                 PIC 9(2)    VALUE ZERO.
005542      05 ‰—Ã—¿‚v                   PIC 9(5)    VALUE ZERO.
005543      05 ‰—Ã‰ÁŽZ—¿‚v               PIC 9(5)    VALUE ZERO.
005544   03 ‹à‘®•›Žq‚v‚q.
      */‹à‘®•›Žq•ÏX/20180611
000561      05 ‹à‘®•›Žq‰ñ”‚v             PIC 9(2)    VALUE ZERO.
005546*      05 ‘å‰ñ”‚v                   PIC 9(1)    VALUE ZERO.
005550*      05 ’†‰ñ”‚v                   PIC 9(1)    VALUE ZERO.
005560*      05 ¬‰ñ”‚v                   PIC 9(1)    VALUE ZERO.
005573      05 ‹à‘®•›Žq‰ÁŽZ—¿‚v           PIC 9(5)    VALUE ZERO.
      */‰^“®Œã—Ã’Ç‰Á/20180607
         03 ‰^“®Œã—Ã—¿‚v‚q.
            05 ‰^“®Œã—Ã—¿‰ñ”‚v           PIC 9(1)    VALUE ZERO.
            05 ‰^“®Œã—Ã—¿‚v               PIC 9(5)    VALUE ZERO.
005574   03 î•ñ’ñ‹Ÿ‚v‚q.
005575      05 î•ñ’ñ‹Ÿ—¿‰ñ”‚v           PIC 9(1)    VALUE ZERO.
005580      05 î•ñ’ñ‹Ÿ—¿‚v               PIC 9(5)    VALUE ZERO.
005581   03 ˆê•”•‰’S‹à‚v‚q                PIC 9(6)    VALUE ZERO.
005590   03 ¿‹‹àŠz‚v‚q                  PIC 9(6)    VALUE ZERO.
005600   03 ‹‹•tŠ„‡‚v‚q                  PIC 9(1)    VALUE ZERO.
005610   03 Žó‹‹ŽÒ•‰’SŠz‚v‚q              PIC 9(6)    VALUE ZERO.
005620   03 •¬¿‹‹àŠz‚v‚q              PIC 9(6)    VALUE ZERO.
005630*/
005640   03 ‘Š’kŽx‰‡‰ñ”‚v                PIC 9(1)    VALUE ZERO.
005650   03 Ž{p“ú‚s‚v.
005660      05 Ž{p“ú‚v                   PIC 9(1) OCCURS 31 VALUE ZERO.
      */–¾×‘”­s‘Ì§‰ÁŽZ’Ç‰Á/20221020
         03 –¾×‘”­s‰ñ”‚v              PIC 9(1)    VALUE ZERO.
         03 –¾×‘”­s‚v                  PIC 9(3)    VALUE ZERO.
         03 –¾×‘”­sŒŽ“ú‚v.
            05 –¾×‘”­sŒŽ‚v             PIC 9(2)    VALUE ZERO.
            05 –¾×‘”­s“ú‚v             PIC 9(2)    VALUE ZERO.
005670*
005680* •‰•”ˆÊ–ˆ‚Ì—¿‹à
005690***********************
005700 01 —¿‹à‚Q‚v‚q.
005710   03 ‰‰ñˆ’u‚v‚q    OCCURS   9.
005720      05 ‰‰ñˆ’u—¿‚v‚q             PIC 9(5)    VALUE ZERO.
005770*
005780* ’üŒ¸–ˆ‚Ì—¿‹à
005790***********************
005800 01 —¿‹à‚R‚v‚q.
007792**********
007793* ‚P•”ˆÊ *
007794**********
007795   03 •”ˆÊ‚P‚v‚q.
007796      05 Œã—Ã‚P‚v‚q.
007797         07 Œã—Ã’P‰¿‚P‚v‚q              PIC 9(4)    VALUE ZERO.
007798         07 Œã—Ã‰ñ”‚P‚v‚q              PIC 9(2)    VALUE ZERO.
007799         07 Œã—Ã—¿‚P‚v‚q                PIC 9(5)    VALUE ZERO.
007800      05 —âãª–@‚P‚v‚q.
007801         07 —âãª–@‰ñ”‚P‚v‚q            PIC 9(2)    VALUE ZERO.
007802         07 —âãª–@—¿‚P‚v‚q              PIC 9(4)    VALUE ZERO.
007803      05 ‰·ãª–@‚P‚v‚q.
007804         07 ‰·ãª–@‰ñ”‚P‚v‚q            PIC 9(2)    VALUE ZERO.
007805         07 ‰·ãª–@—¿‚P‚v‚q              PIC 9(4)    VALUE ZERO.
007806      05 “d—Ã‚P‚v‚q.
007807         07 “d—Ã‰ñ”‚P‚v‚q              PIC 9(2)    VALUE ZERO.
007808         07 “d—Ã—¿‚P‚v‚q                PIC 9(4)    VALUE ZERO.
007809      05 ¬Œv‚P‚v‚q                     PIC 9(6)    VALUE ZERO.
007811      05 ’·Šú’üŒ¸—¦‚P‚v‚q               PIC 9(3)    VALUE ZERO.
007812      05 ’·Šúž¬Œv‚P‚v‚q               PIC 9(6)    VALUE ZERO.
007813**********
007814* ‚Q•”ˆÊ *
007815**********
007816   03 •”ˆÊ‚Q‚v‚q.
007817      05 Œã—Ã‚Q‚v‚q.
007818         07 Œã—Ã’P‰¿‚Q‚v‚q              PIC 9(4)    VALUE ZERO.
007819         07 Œã—Ã‰ñ”‚Q‚v‚q              PIC 9(2)    VALUE ZERO.
007820         07 Œã—Ã—¿‚Q‚v‚q                PIC 9(5)    VALUE ZERO.
007821      05 —âãª–@‚Q‚v‚q.
007822         07 —âãª–@‰ñ”‚Q‚v‚q            PIC 9(2)    VALUE ZERO.
007823         07 —âãª–@—¿‚Q‚v‚q              PIC 9(4)    VALUE ZERO.
007824      05 ‰·ãª–@‚Q‚v‚q.
007825         07 ‰·ãª–@‰ñ”‚Q‚v‚q            PIC 9(2)    VALUE ZERO.
007826         07 ‰·ãª–@—¿‚Q‚v‚q              PIC 9(4)    VALUE ZERO.
007827      05 “d—Ã‚Q‚v‚q.
007828         07 “d—Ã‰ñ”‚Q‚v‚q              PIC 9(2)    VALUE ZERO.
007829         07 “d—Ã—¿‚Q‚v‚q                PIC 9(4)    VALUE ZERO.
007830      05 ¬Œv‚Q‚v‚q                     PIC 9(6)    VALUE ZERO.
007831      05 ’·Šú’üŒ¸—¦‚Q‚v‚q               PIC 9(3)    VALUE ZERO.
007832      05 ’·Šúž¬Œv‚Q‚v‚q               PIC 9(6)    VALUE ZERO.
007833******************
007834* ‚R•”ˆÊ^‚WŠ„ *
007835******************
007836   03 •”ˆÊ‚R‚W‚v‚q.
007837      05 Œã—Ã‚R‚W‚v‚q.
007838         07 Œã—Ã’P‰¿‚R‚W‚v‚q              PIC 9(4)  VALUE ZERO.
007839         07 Œã—Ã‰ñ”‚R‚W‚v‚q              PIC 9(2)  VALUE ZERO.
007840         07 Œã—Ã—¿‚R‚W‚v‚q                PIC 9(5)  VALUE ZERO.
007841      05 —âãª–@‚R‚W‚v‚q.
007842         07 —âãª–@‰ñ”‚R‚W‚v‚q            PIC 9(2)  VALUE ZERO.
007843         07 —âãª–@—¿‚R‚W‚v‚q              PIC 9(4)  VALUE ZERO.
007844      05 ‰·ãª–@‚R‚W‚v‚q.
007845         07 ‰·ãª–@‰ñ”‚R‚W‚v‚q            PIC 9(2)  VALUE ZERO.
007846         07 ‰·ãª–@—¿‚R‚W‚v‚q              PIC 9(4)  VALUE ZERO.
007847      05 “d—Ã‚R‚W‚v‚q.
007848         07 “d—Ã‰ñ”‚R‚W‚v‚q              PIC 9(2)  VALUE ZERO.
007849         07 “d—Ã—¿‚R‚W‚v‚q                PIC 9(4)  VALUE ZERO.
007850      05 ¬Œv‚R‚W‚v‚q                     PIC 9(6)  VALUE ZERO.
007851      05 ‘½•”ˆÊž¬Œv‚R‚W‚v‚q             PIC 9(6)  VALUE ZERO.
007852      05 ’·Šú’üŒ¸—¦‚R‚W‚v‚q               PIC 9(3)  VALUE ZERO.
007853      05 ’·Šúž¬Œv‚R‚W‚v‚q               PIC 9(6)  VALUE ZERO.
007854******************
007855* ‚R•”ˆÊ^‚P‚OŠ„ *
007856******************
007857   03 •”ˆÊ‚R‚O‚v‚q.
007858      05 ’üŒ¸ŠJŽnŒŽ“ú‚R‚O‚v‚q.
007859         07 ’üŒ¸ŠJŽnŒŽ‚R‚O‚v‚q            PIC 9(2)  VALUE ZERO.
007860         07 ’üŒ¸ŠJŽn“ú‚R‚O‚v‚q            PIC 9(2)  VALUE ZERO.
007861      05 Œã—Ã‚R‚O‚v‚q.
007862         07 Œã—Ã’P‰¿‚R‚O‚v‚q              PIC 9(4)  VALUE ZERO.
007863         07 Œã—Ã‰ñ”‚R‚O‚v‚q              PIC 9(2)  VALUE ZERO.
007864         07 Œã—Ã—¿‚R‚O‚v‚q                PIC 9(5)  VALUE ZERO.
007865      05 —âãª–@‚R‚O‚v‚q.
007866         07 —âãª–@‰ñ”‚R‚O‚v‚q            PIC 9(2)  VALUE ZERO.
007867         07 —âãª–@—¿‚R‚O‚v‚q              PIC 9(4)  VALUE ZERO.
007868      05 ‰·ãª–@‚R‚O‚v‚q.
007869         07 ‰·ãª–@‰ñ”‚R‚O‚v‚q            PIC 9(2)  VALUE ZERO.
007870         07 ‰·ãª–@—¿‚R‚O‚v‚q              PIC 9(4)  VALUE ZERO.
007871      05 “d—Ã‚R‚O‚v‚q.
007872         07 “d—Ã‰ñ”‚R‚O‚v‚q              PIC 9(2)  VALUE ZERO.
007873         07 “d—Ã—¿‚R‚O‚v‚q                PIC 9(4)  VALUE ZERO.
007874      05 ¬Œv‚R‚O‚v‚q                     PIC 9(6)  VALUE ZERO.
007876      05 ’·Šú’üŒ¸—¦‚R‚O‚v‚q               PIC 9(3)  VALUE ZERO.
007877      05 ’·Šúž¬Œv‚R‚O‚v‚q               PIC 9(6)  VALUE ZERO.
007878****************
007879* ‚S•”ˆÊ^‚TŠ„ *
007880****************
007881   03 •”ˆÊ‚S‚T‚v‚q.
007882      05 Œã—Ã‚S‚T‚v‚q.
007883         07 Œã—Ã’P‰¿‚S‚T‚v‚q              PIC 9(4)  VALUE ZERO.
007884         07 Œã—Ã‰ñ”‚S‚T‚v‚q              PIC 9(2)  VALUE ZERO.
007885         07 Œã—Ã—¿‚S‚T‚v‚q                PIC 9(5)  VALUE ZERO.
007886      05 —âãª–@‚S‚T‚v‚q.
007887         07 —âãª–@‰ñ”‚S‚T‚v‚q            PIC 9(2)  VALUE ZERO.
007888         07 —âãª–@—¿‚S‚T‚v‚q              PIC 9(4)  VALUE ZERO.
007889      05 ‰·ãª–@‚S‚T‚v‚q.
007890         07 ‰·ãª–@‰ñ”‚S‚T‚v‚q            PIC 9(2)  VALUE ZERO.
007891         07 ‰·ãª–@—¿‚S‚T‚v‚q              PIC 9(4)  VALUE ZERO.
007892      05 “d—Ã‚S‚T‚v‚q.
007893         07 “d—Ã‰ñ”‚S‚T‚v‚q              PIC 9(2)  VALUE ZERO.
007894         07 “d—Ã—¿‚S‚T‚v‚q                PIC 9(4)  VALUE ZERO.
007895      05 ¬Œv‚S‚T‚v‚q                     PIC 9(6)  VALUE ZERO.
007896      05 ‘½•”ˆÊž¬Œv‚S‚T‚v‚q             PIC 9(6)  VALUE ZERO.
007897      05 ’·Šú’üŒ¸—¦‚S‚T‚v‚q               PIC 9(3)  VALUE ZERO.
007898      05 ’·Šúž¬Œv‚S‚T‚v‚q               PIC 9(6)  VALUE ZERO.
007899****************
007900* ‚S•”ˆÊ^‚WŠ„ *
007901****************
007902   03 •”ˆÊ‚S‚W‚v‚q.
007903      05 ’üŒ¸ŠJŽnŒŽ“ú‚S‚W‚v‚q.
007904         07 ’üŒ¸ŠJŽnŒŽ‚S‚W‚v‚q            PIC 9(2)  VALUE ZERO.
007905         07 ’üŒ¸ŠJŽn“ú‚S‚W‚v‚q            PIC 9(2)  VALUE ZERO.
007906      05 Œã—Ã‚S‚W‚v‚q.
007907         07 Œã—Ã’P‰¿‚S‚W‚v‚q              PIC 9(4)  VALUE ZERO.
007908         07 Œã—Ã‰ñ”‚S‚W‚v‚q              PIC 9(2)  VALUE ZERO.
007909         07 Œã—Ã—¿‚S‚W‚v‚q                PIC 9(5)  VALUE ZERO.
007910      05 —âãª–@‚S‚W‚v‚q.
007911         07 —âãª–@‰ñ”‚S‚W‚v‚q            PIC 9(2)  VALUE ZERO.
007912         07 —âãª–@—¿‚S‚W‚v‚q              PIC 9(4)  VALUE ZERO.
007913      05 ‰·ãª–@‚S‚W‚v‚q.
007914         07 ‰·ãª–@‰ñ”‚S‚W‚v‚q            PIC 9(2)  VALUE ZERO.
007915         07 ‰·ãª–@—¿‚S‚W‚v‚q              PIC 9(4)  VALUE ZERO.
007916      05 “d—Ã‚S‚W‚v‚q.
007917         07 “d—Ã‰ñ”‚S‚W‚v‚q              PIC 9(2)  VALUE ZERO.
007918         07 “d—Ã—¿‚S‚W‚v‚q                PIC 9(4)  VALUE ZERO.
007919      05 ¬Œv‚S‚W‚v‚q                     PIC 9(6)  VALUE ZERO.
007920      05 ‘½•”ˆÊž¬Œv‚S‚W‚v‚q             PIC 9(6)  VALUE ZERO.
007921      05 ’·Šú’üŒ¸—¦‚S‚W‚v‚q               PIC 9(3)  VALUE ZERO.
007922      05 ’·Šúž¬Œv‚S‚W‚v‚q               PIC 9(6)  VALUE ZERO.
007923******************
007924* ‚S•”ˆÊ^‚P‚OŠ„ *
007925******************
007926   03 •”ˆÊ‚S‚O‚v‚q.
007927      05 ’üŒ¸ŠJŽnŒŽ“ú‚S‚O‚v‚q.
007928         07 ’üŒ¸ŠJŽnŒŽ‚S‚O‚v‚q            PIC 9(2)  VALUE ZERO.
007929         07 ’üŒ¸ŠJŽn“ú‚S‚O‚v‚q            PIC 9(2)  VALUE ZERO.
007930      05 Œã—Ã‚S‚O‚v‚q.
007931         07 Œã—Ã’P‰¿‚S‚O‚v‚q              PIC 9(4)  VALUE ZERO.
007932         07 Œã—Ã‰ñ”‚S‚O‚v‚q              PIC 9(2)  VALUE ZERO.
007933         07 Œã—Ã—¿‚S‚O‚v‚q                PIC 9(5)  VALUE ZERO.
007934      05 —âãª–@‚S‚O‚v‚q.
007935         07 —âãª–@‰ñ”‚S‚O‚v‚q            PIC 9(2)  VALUE ZERO.
007936         07 —âãª–@—¿‚S‚O‚v‚q              PIC 9(4)  VALUE ZERO.
007937      05 ‰·ãª–@‚S‚O‚v‚q.
007938         07 ‰·ãª–@‰ñ”‚S‚O‚v‚q            PIC 9(2)  VALUE ZERO.
007939         07 ‰·ãª–@—¿‚S‚O‚v‚q              PIC 9(4)  VALUE ZERO.
007940      05 “d—Ã‚S‚O‚v‚q.
007941         07 “d—Ã‰ñ”‚S‚O‚v‚q              PIC 9(2)  VALUE ZERO.
007942         07 “d—Ã—¿‚S‚O‚v‚q                PIC 9(4)  VALUE ZERO.
007943      05 ¬Œv‚S‚O‚v‚q                     PIC 9(6)  VALUE ZERO.
007944      05 ’·Šú’üŒ¸—¦‚S‚O‚v‚q               PIC 9(3)  VALUE ZERO.
007945      05 ’·Šúž¬Œv‚S‚O‚v‚q               PIC 9(6)  VALUE ZERO.
007946********************
007947* ‚T•”ˆÊ^‚QD‚TŠ„ *
007948********************
007949   03 •”ˆÊ‚T‚Q‚v‚q.
007950      05 Œã—Ã‚T‚Q‚v‚q.
007951         07 Œã—Ã’P‰¿‚T‚Q‚v‚q              PIC 9(4)  VALUE ZERO.
007952         07 Œã—Ã‰ñ”‚T‚Q‚v‚q              PIC 9(2)  VALUE ZERO.
007953         07 Œã—Ã—¿‚T‚Q‚v‚q                PIC 9(5)  VALUE ZERO.
007954      05 —âãª–@‚T‚Q‚v‚q.
007955         07 —âãª–@‰ñ”‚T‚Q‚v‚q            PIC 9(2)  VALUE ZERO.
007956         07 —âãª–@—¿‚T‚Q‚v‚q              PIC 9(4)  VALUE ZERO.
007957      05 ‰·ãª–@‚T‚Q‚v‚q.
007958         07 ‰·ãª–@‰ñ”‚T‚Q‚v‚q            PIC 9(2)  VALUE ZERO.
007959         07 ‰·ãª–@—¿‚T‚Q‚v‚q              PIC 9(4)  VALUE ZERO.
007960      05 “d—Ã‚T‚Q‚v‚q.
007961         07 “d—Ã‰ñ”‚T‚Q‚v‚q              PIC 9(2)  VALUE ZERO.
007962         07 “d—Ã—¿‚T‚Q‚v‚q                PIC 9(4)  VALUE ZERO.
007963      05 ¬Œv‚T‚Q‚v‚q                     PIC 9(6)  VALUE ZERO.
007964      05 ‘½•”ˆÊž¬Œv‚T‚Q‚v‚q             PIC 9(6)  VALUE ZERO.
007965      05 ’·Šú’üŒ¸—¦‚T‚Q‚v‚q               PIC 9(3)  VALUE ZERO.
007966      05 ’·Šúž¬Œv‚T‚Q‚v‚q               PIC 9(6)  VALUE ZERO.
007967****************
007968* ‚T•”ˆÊ^‚TŠ„ *
007969****************
007970   03 •”ˆÊ‚T‚T‚v‚q.
007971      05 ’üŒ¸ŠJŽnŒŽ“ú‚T‚T‚v‚q.
007972         07 ’üŒ¸ŠJŽnŒŽ‚T‚T‚v‚q            PIC 9(2)  VALUE ZERO.
007973         07 ’üŒ¸ŠJŽn“ú‚T‚T‚v‚q            PIC 9(2)  VALUE ZERO.
007974      05 Œã—Ã‚T‚T‚v‚q.
007975         07 Œã—Ã’P‰¿‚T‚T‚v‚q              PIC 9(4)  VALUE ZERO.
007976         07 Œã—Ã‰ñ”‚T‚T‚v‚q              PIC 9(2)  VALUE ZERO.
007977         07 Œã—Ã—¿‚T‚T‚v‚q                PIC 9(5)  VALUE ZERO.
007978      05 —âãª–@‚T‚T‚v‚q.
007979         07 —âãª–@‰ñ”‚T‚T‚v‚q            PIC 9(2)  VALUE ZERO.
007980         07 —âãª–@—¿‚T‚T‚v‚q              PIC 9(4)  VALUE ZERO.
007981      05 ‰·ãª–@‚T‚T‚v‚q.
007982         07 ‰·ãª–@‰ñ”‚T‚T‚v‚q            PIC 9(2)  VALUE ZERO.
007983         07 ‰·ãª–@—¿‚T‚T‚v‚q              PIC 9(4)  VALUE ZERO.
007984      05 “d—Ã‚T‚T‚v‚q.
007985         07 “d—Ã‰ñ”‚T‚T‚v‚q              PIC 9(2)  VALUE ZERO.
007986         07 “d—Ã—¿‚T‚T‚v‚q                PIC 9(4)  VALUE ZERO.
007987      05 ¬Œv‚T‚T‚v‚q                     PIC 9(6)  VALUE ZERO.
007988      05 ‘½•”ˆÊž¬Œv‚T‚T‚v‚q             PIC 9(6)  VALUE ZERO.
007989      05 ’·Šú’üŒ¸—¦‚T‚T‚v‚q               PIC 9(3)  VALUE ZERO.
007990      05 ’·Šúž¬Œv‚T‚T‚v‚q               PIC 9(6)  VALUE ZERO.
007991****************
007992* ‚T•”ˆÊ^‚WŠ„ *
007993****************
007994   03 •”ˆÊ‚T‚W‚v‚q.
007995      05 ’üŒ¸ŠJŽnŒŽ“ú‚T‚W‚v‚q.
007996         07 ’üŒ¸ŠJŽnŒŽ‚T‚W‚v‚q            PIC 9(2)  VALUE ZERO.
007997         07 ’üŒ¸ŠJŽn“ú‚T‚W‚v‚q            PIC 9(2)  VALUE ZERO.
007998      05 Œã—Ã‚T‚W‚v‚q.
007999         07 Œã—Ã’P‰¿‚T‚W‚v‚q              PIC 9(4)  VALUE ZERO.
008000         07 Œã—Ã‰ñ”‚T‚W‚v‚q              PIC 9(2)  VALUE ZERO.
008001         07 Œã—Ã—¿‚T‚W‚v‚q                PIC 9(5)  VALUE ZERO.
008002      05 —âãª–@‚T‚W‚v‚q.
008003         07 —âãª–@‰ñ”‚T‚W‚v‚q            PIC 9(2)  VALUE ZERO.
008004         07 —âãª–@—¿‚T‚W‚v‚q              PIC 9(4)  VALUE ZERO.
008005      05 ‰·ãª–@‚T‚W‚v‚q.
008006         07 ‰·ãª–@‰ñ”‚T‚W‚v‚q            PIC 9(2)  VALUE ZERO.
008007         07 ‰·ãª–@—¿‚T‚W‚v‚q              PIC 9(4)  VALUE ZERO.
008008      05 “d—Ã‚T‚W‚v‚q.
008009         07 “d—Ã‰ñ”‚T‚W‚v‚q              PIC 9(2)  VALUE ZERO.
008010         07 “d—Ã—¿‚T‚W‚v‚q                PIC 9(4)  VALUE ZERO.
008011      05 ¬Œv‚T‚W‚v‚q                     PIC 9(6)  VALUE ZERO.
008012      05 ‘½•”ˆÊž¬Œv‚T‚W‚v‚q             PIC 9(6)  VALUE ZERO.
008013      05 ’·Šú’üŒ¸—¦‚T‚W‚v‚q               PIC 9(3)  VALUE ZERO.
008014      05 ’·Šúž¬Œv‚T‚W‚v‚q               PIC 9(6)  VALUE ZERO.
008015******************
008016* ‚T•”ˆÊ^‚P‚OŠ„ *
008017******************
008018   03 •”ˆÊ‚T‚O‚v‚q.
008019      05 ’üŒ¸ŠJŽnŒŽ“ú‚T‚O‚v‚q.
008020         07 ’üŒ¸ŠJŽnŒŽ‚T‚O‚v‚q            PIC 9(2)  VALUE ZERO.
008021         07 ’üŒ¸ŠJŽn“ú‚T‚O‚v‚q            PIC 9(2)  VALUE ZERO.
008022      05 Œã—Ã‚T‚O‚v‚q.
008023         07 Œã—Ã’P‰¿‚T‚O‚v‚q              PIC 9(4)  VALUE ZERO.
008024         07 Œã—Ã‰ñ”‚T‚O‚v‚q              PIC 9(2)  VALUE ZERO.
008025         07 Œã—Ã—¿‚T‚O‚v‚q                PIC 9(5)  VALUE ZERO.
008026      05 —âãª–@‚T‚O‚v‚q.
008027         07 —âãª–@‰ñ”‚T‚O‚v‚q            PIC 9(2)  VALUE ZERO.
008028         07 —âãª–@—¿‚T‚O‚v‚q              PIC 9(4)  VALUE ZERO.
008029      05 ‰·ãª–@‚T‚O‚v‚q.
008030         07 ‰·ãª–@‰ñ”‚T‚O‚v‚q            PIC 9(2)  VALUE ZERO.
008031         07 ‰·ãª–@—¿‚T‚O‚v‚q              PIC 9(4)  VALUE ZERO.
008032      05 “d—Ã‚T‚O‚v‚q.
008033         07 “d—Ã‰ñ”‚T‚O‚v‚q              PIC 9(2)  VALUE ZERO.
008034         07 “d—Ã—¿‚T‚O‚v‚q                PIC 9(4)  VALUE ZERO.
008035      05 ¬Œv‚T‚O‚v‚q                     PIC 9(6)  VALUE ZERO.
008036      05 ’·Šú’üŒ¸—¦‚T‚O‚v‚q               PIC 9(3)  VALUE ZERO.
008037      05 ’·Šúž¬Œv‚T‚O‚v‚q               PIC 9(6)  VALUE ZERO.
008038*
008039******************
008040* ‚R•”ˆÊ^‡Œv@ *
008041******************
008042   03 •”ˆÊ‚R‚v‚q.
008043      05 Œã—Ã‚R‚v‚q.
008044         07 Œã—Ã‰ñ”‚R‚v‚q                PIC 9(2)  VALUE ZERO.
008045         07 Œã—Ã—¿‚R‚v‚q                  PIC 9(6)  VALUE ZERO.
008046      05 —âãª–@‚R‚v‚q.
008047         07 —âãª–@‰ñ”‚R‚v‚q              PIC 9(2)  VALUE ZERO.
008048         07 —âãª–@—¿‚R‚v‚q                PIC 9(6)  VALUE ZERO.
008049      05 ‰·ãª–@‚R‚v‚q.
008050         07 ‰·ãª–@‰ñ”‚R‚v‚q              PIC 9(2)  VALUE ZERO.
008051         07 ‰·ãª–@—¿‚R‚v‚q                PIC 9(6)  VALUE ZERO.
008052      05 “d—Ã‚R‚v‚q.
008053         07 “d—Ã‰ñ”‚R‚v‚q                PIC 9(2)  VALUE ZERO.
008054         07 “d—Ã—¿‚R‚v‚q                  PIC 9(6)  VALUE ZERO.
008055******************
008056* ‚S•”ˆÊ^‡Œv@ *
008057******************
008058   03 •”ˆÊ‚S‚v‚q.
008059      05 Œã—Ã‚S‚v‚q.
008060         07 Œã—Ã‰ñ”‚S‚v‚q                PIC 9(2)  VALUE ZERO.
008061         07 Œã—Ã—¿‚S‚v‚q                  PIC 9(6)  VALUE ZERO.
008062      05 —âãª–@‚S‚v‚q.
008063         07 —âãª–@‰ñ”‚S‚v‚q              PIC 9(2)  VALUE ZERO.
008064         07 —âãª–@—¿‚S‚v‚q                PIC 9(6)  VALUE ZERO.
008065      05 ‰·ãª–@‚S‚v‚q.
008066         07 ‰·ãª–@‰ñ”‚S‚v‚q              PIC 9(2)  VALUE ZERO.
008067         07 ‰·ãª–@—¿‚S‚v‚q                PIC 9(6)  VALUE ZERO.
008068      05 “d—Ã‚S‚v‚q.
008069         07 “d—Ã‰ñ”‚S‚v‚q                PIC 9(2)  VALUE ZERO.
008070         07 “d—Ã—¿‚S‚v‚q                  PIC 9(6)  VALUE ZERO.
008071******************
008072* ‚T•”ˆÊ^‡Œv@ *
008073******************
008074   03 •”ˆÊ‚T‚v‚q.
008075      05 Œã—Ã‚T‚v‚q.
008076         07 Œã—Ã‰ñ”‚T‚v‚q                PIC 9(2)  VALUE ZERO.
008077         07 Œã—Ã—¿‚T‚v‚q                  PIC 9(6)  VALUE ZERO.
008078      05 —âãª–@‚T‚v‚q.
008079         07 —âãª–@‰ñ”‚T‚v‚q              PIC 9(2)  VALUE ZERO.
008080         07 —âãª–@—¿‚T‚v‚q                PIC 9(6)  VALUE ZERO.
008081      05 ‰·ãª–@‚T‚v‚q.
008082         07 ‰·ãª–@‰ñ”‚T‚v‚q              PIC 9(2)  VALUE ZERO.
008083         07 ‰·ãª–@—¿‚T‚v‚q                PIC 9(6)  VALUE ZERO.
008084      05 “d—Ã‚T‚v‚q.
008085         07 “d—Ã‰ñ”‚T‚v‚q                PIC 9(2)  VALUE ZERO.
008086         07 “d—Ã—¿‚T‚v‚q                  PIC 9(6)  VALUE ZERO.
008087*
008088*
008089*****************************************************************
008090 01 ŒvŽZ‹@¼—ï”N‚v                     PIC 9(2).
008091* “ú•t‚v‚n‚q‚j
008092 01 ŒvŽZ‹@¼—ï.
008093    03 ŒvŽZ‹@¼—ï”N                    PIC 9(4).
008094    03 ŒvŽZ‹@¼—ïŒŽ“ú                  PIC 9(4).
008095 01 ŒvŽZ‹@¼—ï‚q REDEFINES ŒvŽZ‹@¼—ï.
008096    03 ŒvŽZ‹@¢‹I                      PIC 9(2).
008097    03 ŒvŽZ‹@“ú•t                      PIC 9(6).
008098    03 ŒvŽZ‹@“ú•t‚q REDEFINES ŒvŽZ‹@“ú•t.
008099       05 ŒvŽZ‹@”NŒŽ                   PIC 9(4).
008100       05 ŒvŽZ‹@”NŒŽ‚q REDEFINES ŒvŽZ‹@”NŒŽ.
008101         07 ŒvŽZ‹@”N                   PIC 9(2).
008102         07 ŒvŽZ‹@ŒŽ                   PIC 9(2).
008103       05 ŒvŽZ‹@“ú                     PIC 9(2).
008104*
008105 01 Ž{p˜a—ï”NŒŽ“ú‚b‚v.
008106   03 Ž{p˜a—ï”NŒŽ‚b‚v.
008107     05 Ž{p˜a—ï‚b‚v                   PIC 9.
008108     05 Ž{p”NŒŽ‚b‚v.
008109        07 Ž{p”N‚b‚v                  PIC 9(2).
008110        07 Ž{pŒŽ‚b‚v                  PIC 9(2).
008111   03 Ž{p“ú‚b‚v                       PIC 9(2).
008112*
008113* C ˜AŒg—p
008114 01  •¶Žš‚P‚v        PIC X(4096).
008115 01  •¶Žš‚Q‚v        PIC X(512).
008116 01  ƒvƒƒOƒ‰ƒ€–¼‚v  PIC X(8)  VALUE "strmoji2".
008117*
008118 01 •¡‡ƒvƒƒOƒ‰ƒ€–¼‚v     PIC X(8) VALUE "MOJI2".
008119*
008120******************************************************************
008121*                          ˜AŒ‹€–Ú                              *
008122******************************************************************
008123*
008124********************
008125* ƒƒbƒZ[ƒW•\Ž¦ƒL[ *
008126********************
008127 01 ˜Aƒ|ƒL[ IS EXTERNAL.
008128    03  ˜Aƒ|ƒƒbƒZ[ƒW               PIC N(20).
008129*
008130 01 ˜Aƒ‚R|ƒL[ IS EXTERNAL.
008131    03  ˜Aƒ‚R|ƒƒbƒZ[ƒW             PIC N(20).
008132    03  ˜Aƒ‚R|ƒƒbƒZ[ƒW‚P           PIC X(20).
008133*
008134****************
008135* ‰æ–Ê“ü—Íî•ñ *
008136****************
008137 01 ˜A“ü|‰æ–Êî•ñ‚x‚g‚o‚T‚W‚O IS EXTERNAL.
008138    03 ˜A“ü|¿‹˜a—ï”NŒŽ.
008139       05 ˜A“ü|¿‹˜a—ï               PIC 9.
008140       05 ˜A“ü|¿‹”NŒŽ.
008141         07 ˜A“ü|¿‹”N               PIC 9(2).
008142         07 ˜A“ü|¿‹ŒŽ               PIC 9(2).
008143*
008144************************
008145* ’·Šú——R•¶ƒZƒbƒg     *
008146************************
008147 01 ˜A’·•¶|ƒL[ IS EXTERNAL.
008148    03 ˜A’·•¶|Ž{p”NŒŽ.
008149       05 ˜A’·•¶|Ž{p˜a—ï               PIC 9.
008150       05 ˜A’·•¶|Ž{p”N                 PIC 9(2).
008151       05 ˜A’·•¶|Ž{pŒŽ                 PIC 9(2).
008152    03  ˜A’·•¶|Š³ŽÒƒR[ƒh.
008153       05 ˜A’·•¶|Š³ŽÒ”Ô†               PIC 9(6).
008154       05 ˜A’·•¶|Ž}”Ô                   PIC X.
008155    03 ˜A’·•¶|•¶Œ…”                    PIC 9(2).
008156    03 ˜A’·•¶|——R•¶                    PIC N(63) OCCURS 15.
008157*
008158************************
008159* ƒŒƒZ•‰Œ´ˆöˆóü”»’è
008160************************
008161 01 ˜AƒŒƒZ•‰Œ´ˆó|ƒL[ IS EXTERNAL.
008162    03 ˜AƒŒƒZ•‰Œ´ˆó|Ž{p”NŒŽ.
008163       05 ˜AƒŒƒZ•‰Œ´ˆó|Ž{p˜a—ï               PIC 9.
008164       05 ˜AƒŒƒZ•‰Œ´ˆó|Ž{p”N                 PIC 9(2).
008165       05 ˜AƒŒƒZ•‰Œ´ˆó|Ž{pŒŽ                 PIC 9(2).
008166    03  ˜AƒŒƒZ•‰Œ´ˆó|Š³ŽÒƒR[ƒh.
008167       05 ˜AƒŒƒZ•‰Œ´ˆó|Š³ŽÒ”Ô†               PIC 9(6).
008168       05 ˜AƒŒƒZ•‰Œ´ˆó|Ž}”Ô                   PIC X.
008169    03 ˜AƒŒƒZ•‰Œ´ˆó|‘ÎÛƒtƒ‰ƒO                PIC X(3).
008170*
008171************************
008172* •¬ƒŒƒZ‚Ü‚Æ‚ß
008173************************
008174 01 ˜AƒŒƒZ‚Ü‚Æ‚ß|ƒL[ IS EXTERNAL.
008175    03 ˜AƒŒƒZ‚Ü‚Æ‚ß|Ž{p˜a—ï”NŒŽ.
008176       05 ˜AƒŒƒZ‚Ü‚Æ‚ß|Ž{p˜a—ï               PIC 9.
008177       05 ˜AƒŒƒZ‚Ü‚Æ‚ß|Ž{p”NŒŽ.
008180          07 ˜AƒŒƒZ‚Ü‚Æ‚ß|Ž{p”N              PIC 9(2).
008190          07 ˜AƒŒƒZ‚Ü‚Æ‚ß|Ž{pŒŽ              PIC 9(2).
008200    03 ˜AƒŒƒZ‚Ü‚Æ‚ß|Š³ŽÒƒR[ƒh.
008210       05 ˜AƒŒƒZ‚Ü‚Æ‚ß|Š³ŽÒ”Ô†               PIC 9(6).
008220       05 ˜AƒŒƒZ‚Ü‚Æ‚ß|Ž}”Ô                   PIC X(1).
008230**-------------------------------------------------------**
008240*   1:•¬ƒŒƒZƒvƒg‚È‚µ‚Ì–{‘Ì‚Ü‚Æ‚ß‚Ì”»’è
008250*   2:‰¡•lEìè—p‚ÌŽÐ•Û•¬ƒŒƒZ‚©‚Ì”»’è
008260    03 ˜AƒŒƒZ‚Ü‚Æ‚ß|”»’è‹æ•ª                  PIC 9.
008270**-------------------------------------------------------**
008280*  / OUT /@ 0:‘ÎÛŠOA1:‘ÎÛ
008290    03 ˜AƒŒƒZ‚Ü‚Æ‚ß|”»’èŒ‹‰Ê                  PIC 9.
008300**
008310*
008320* ˆÃ†•¡‡—p
008321 01 ˜AˆÃ†•¡‡|ˆÃ†î•ñ IS EXTERNAL.
008322    03 ˜AˆÃ†•¡‡|“ü—Íî•ñ.
008323       05 ˜AˆÃ†•¡‡|‹L†               PIC X(24).
008324       05 ˜AˆÃ†•¡‡|”Ô†               PIC X(30).
008325       05 ˜AˆÃ†•¡‡|ˆÃ†‰»€–Ú.
008326         07 ˜AˆÃ†•¡‡|ˆÃ†Š³ŽÒ”Ô†     PIC X(6).
008327         07 ˜AˆÃ†•¡‡|ˆÃ†”»’è‹L†     PIC X.
008328         07 ˜AˆÃ†•¡‡|ˆÃ†”»’è”Ô†     PIC X.
008329         07 ˜AˆÃ†•¡‡|ˆÃ†‹L†         PIC X(24).
008330         07 ˜AˆÃ†•¡‡|ˆÃ†”Ô†         PIC X(30).
008331    03 ˜AˆÃ†•¡‡|o—Íî•ñ.
008332       05 ˜AˆÃ†•¡‡|•¡‡‚µ‚½‹L†       PIC X(24).
008333       05 ˜AˆÃ†•¡‡|•¡‡‚µ‚½”Ô†       PIC X(30).
008334* 
008335******************************************************************
008336*                      PROCEDURE  DIVISION                       *
008340******************************************************************
008500 PROCEDURE               DIVISION.
008510************
008520*           *
008530* ‰Šúˆ—   *
008540*           *
008550************
008560     PERFORM ‰Šú‰».
008570     PERFORM §Œäî•ñŽæ“¾.
008580     PERFORM Ž{pŠî•ñŽæ“¾.
008590************
008600*           *
008610* Žåˆ—     *
008620*           *
008630************
008640     PERFORM ì‹Æƒtƒ@ƒCƒ‹ì¬.
008650************
008660*           *
008670* I—¹ˆ—   *
008680*           *
008690************
008700     PERFORM I—¹ˆ—.
008710     MOVE ZERO TO PROGRAM-STATUS.
008720     EXIT PROGRAM.
008730*
008740*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
008750*================================================================*
008760 ‰Šú‰» SECTION.
008770*
008780     PERFORM ƒtƒ@ƒCƒ‹ƒI[ƒvƒ“.
008790* ˜AŒ‹€–Ú‚Ì‘Ò”ð
008800     MOVE ˜A“ü|¿‹˜a—ï  TO ¿‹˜a—ï‚v‚q.
008810     MOVE ˜A“ü|¿‹”N    TO ¿‹”N‚v‚q.
008820     MOVE ˜A“ü|¿‹ŒŽ    TO ¿‹ŒŽ‚v‚q.
008830*
008840     MOVE ZERO            TO ˜A”Ô‚v.
008850*
008860* ¼—ï¿‹”NŒŽ‚ÌŽæ“¾
008870     MOVE ZERO          TO ¼—ï”NŒŽ‚v  ¼—ï¿‹”NŒŽ‚v.
008880     MOVE ¿‹˜a—ï‚v‚q  TO Œ³|Œ³†‹æ•ª.
008890     READ Œ³†ƒ}ƒXƒ^
008900     NOT INVALID KEY
008910         MOVE Œ³|ŠJŽn¼—ï”N TO ¼—ï”N‚v
008920     END-READ.
008930*
008940     IF ¼—ï”N‚v = ZERO
008950          MOVE  NC"Œ³†ƒ}ƒXƒ^‚ÉŠJŽn¼—ï”N‚ð“o˜^‚µ‚Ä‰º‚³‚¢" TO ˜Aƒ|ƒƒbƒZ[ƒW
008960          CALL   "MSG001"
008970          CANCEL "MSG001"
008980          PERFORM ƒtƒ@ƒCƒ‹•Â½
008990          MOVE 99 TO PROGRAM-STATUS
009000          EXIT PROGRAM
009010     ELSE
009020          COMPUTE ¼—ï”N‚v = ¼—ï”N‚v + ¿‹”N‚v‚q - 1
009030          MOVE ¿‹ŒŽ‚v‚q TO ¼—ïŒŽ‚v
009040     END-IF.
009050*
009060     MOVE ¼—ï”NŒŽ‚v   TO  ¼—ï¿‹”NŒŽ‚v.
009070*
009080*================================================================*
009090 ƒtƒ@ƒCƒ‹ƒI[ƒvƒ“ SECTION.
009100*
009110     OPEN INPUT §Œäî•ñƒ}ƒXƒ^.
009120         MOVE NC"§Œäî•ñ" TO ƒtƒ@ƒCƒ‹–¼.
009130         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009140     OPEN INPUT Œ³†ƒ}ƒXƒ^.
009150         MOVE NC"Œ³†ƒ}ƒXƒ^" TO ƒtƒ@ƒCƒ‹–¼.
009160         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009170     OPEN INPUT –¼Ìƒ}ƒXƒ^.
009180         MOVE NC"–¼Ìƒ}ƒXƒ^" TO ƒtƒ@ƒCƒ‹–¼.
009190         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009200     OPEN INPUT Ž{pŠî•ñƒ}ƒXƒ^
009210         MOVE NC"Ž{î" TO ƒtƒ@ƒCƒ‹–¼.
009220         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009230     OPEN INPUT Ž{p‹L˜^‚e.
009240         MOVE NC"Ž{p‹L˜^‚e" TO ƒtƒ@ƒCƒ‹–¼.
009250         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009260     OPEN INPUT ŽófŽÒî•ñ‚e.
009270         MOVE NC"ŽófŽÒî•ñ‚e" TO ƒtƒ@ƒCƒ‹–¼.
009280         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009290     OPEN INPUT Œo‰ßƒ}ƒXƒ^.
009300         MOVE NC"Œo‰ßƒ}ƒXƒ^" TO ƒtƒ@ƒCƒ‹–¼.
009310         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009320     OPEN INPUT •‰ƒf[ƒ^‚e.
009321         MOVE NC"•‰ƒf[ƒ^‚e" TO ƒtƒ@ƒCƒ‹–¼.
009322         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009323     OPEN INPUT •‰Œ´ˆö‚e.
009330         MOVE NC"•‰Œ´ˆö" TO ƒtƒ@ƒCƒ‹–¼.
009340         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009350     OPEN INPUT Žs’¬‘ºƒ}ƒXƒ^
009360         MOVE NC"Žs’¬‘º" TO ƒtƒ@ƒCƒ‹–¼.
009370         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009380     OPEN INPUT ƒŒƒZƒvƒg‚e.
009390         MOVE NC"ƒŒƒZ" TO ƒtƒ@ƒCƒ‹–¼.
009400         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009410     OPEN INPUT ŒvŽZƒ}ƒXƒ^.
009411         MOVE NC"ŒvŽZƒ}ƒXƒ^" TO ƒtƒ@ƒCƒ‹–¼.
009412         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009413     OPEN OUTPUT ì‹Æƒtƒ@ƒCƒ‹‚P.
009420         MOVE NC"ì‚P" TO ƒtƒ@ƒCƒ‹–¼.
009430         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
009440*
009450*================================================================*
009460 ƒI[ƒvƒ“ƒ`ƒFƒbƒN SECTION.
009470*
009480     IF ó‘ÔƒL[  NOT =  "00"
009490         DISPLAY ƒtƒ@ƒCƒ‹–¼ NC"‚eƒI[ƒvƒ“ƒGƒ‰[" UPON CONS
009500         DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[         UPON CONS
009510         DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢"
009520                                                 UPON CONS
009530*-----------------------------------------*
009540         CALL "actcshm"  WITH C LINKAGE
009550*-----------------------------------------*
009560         ACCEPT  ƒL[“ü—Í FROM CONS
009570         PERFORM ƒtƒ@ƒCƒ‹•Â½
009580         MOVE 99 TO PROGRAM-STATUS
009590         EXIT PROGRAM.
009600*================================================================*
009610 ƒtƒ@ƒCƒ‹•Â½ SECTION.
009620*
009630     CLOSE §Œäî•ñƒ}ƒXƒ^ Œ³†ƒ}ƒXƒ^ –¼Ìƒ}ƒXƒ^ ŽófŽÒî•ñ‚e
009640           •‰ƒf[ƒ^‚e   Œo‰ßƒ}ƒXƒ^ •‰Œ´ˆö‚e Ž{p‹L˜^‚e Ž{pŠî•ñƒ}ƒXƒ^
009650           Žs’¬‘ºƒ}ƒXƒ^   ƒŒƒZƒvƒg‚e ŒvŽZƒ}ƒXƒ^ ì‹Æƒtƒ@ƒCƒ‹‚P.
009660*================================================================*
009670 I—¹ˆ— SECTION.
009680*
009690     PERFORM ƒtƒ@ƒCƒ‹•Â½.
009700*================================================================*
009710 ƒGƒ‰[•\Ž¦‚q SECTION.
009720*
009730     DISPLAY NC"ƒtƒ@ƒCƒ‹“ÇžƒGƒ‰[" ƒtƒ@ƒCƒ‹–¼     UPON CONS.
009740     DISPLAY NC"ó‘ÔƒL[" ó‘ÔƒL[                 UPON CONS.
009750     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢"                                                                    UPON CONS.
009760*-----------------------------------------*
009770     CALL "actcshm"  WITH C LINKAGE.
009780*-----------------------------------------*
009790     ACCEPT  ƒL[“ü—Í FROM CONS.
009800     PERFORM ƒtƒ@ƒCƒ‹•Â½.
009810     MOVE 99 TO PROGRAM-STATUS.
009820     EXIT PROGRAM.
009830*================================================================*
009840 ƒGƒ‰[•\Ž¦ SECTION.
009850*
009860     DISPLAY NC"ó‘ÔƒL[" ó‘ÔƒL[  UPON CONS.
009870     DISPLAY NC"ƒtƒ@ƒCƒ‹‘žƒGƒ‰[F" ƒtƒ@ƒCƒ‹–¼   UPON CONS.
009880     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
009890     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢"                                                                    UPON CONS.
009900*-----------------------------------------*
009910     CALL "actcshm"  WITH C LINKAGE.
009920*-----------------------------------------*
009930     ACCEPT  ƒL[“ü—Í FROM CONS.
009940     PERFORM ƒtƒ@ƒCƒ‹•Â½.
009950     MOVE 99 TO PROGRAM-STATUS.
009960     EXIT PROGRAM.
009970*================================================================*
009980 §Œäî•ñŽæ“¾ SECTION.
009990*
010000     MOVE ZEROS TO §|§Œä‹æ•ª.
010010     READ §Œäî•ñƒ}ƒXƒ^
010020     NOT INVALID KEY
010030         MOVE §|ƒŒƒZ•‰Œ´ˆöˆóü‹æ•ª TO •‰Œ´ˆöˆóü‹æ•ª‚v
010040         MOVE §|ƒŒƒZ’·Šú——Rˆóü‹æ•ª TO ’·Šú——Rˆóü‹æ•ª‚v
010050     END-READ.
010060*
010070*================================================================*
010080 Ž{pŠî•ñŽæ“¾ SECTION.
010090*
010100     MOVE ZERO  TO Ž{î|Ž{pŠ”Ô†.
010110     READ Ž{pŠî•ñƒ}ƒXƒ^
010120     INVALID KEY
010130          MOVE  NC"Ž{pŠî•ñƒ}ƒXƒ^‚É“o˜^ŒãAŽÀs‚µ‚Ä‰º‚³‚¢" TO ˜Aƒ|ƒƒbƒZ[ƒW
010140          CALL   "MSG001"
010150          CANCEL "MSG001"
010160          PERFORM ƒtƒ@ƒCƒ‹•Â½
010170          MOVE 99 TO PROGRAM-STATUS
010180          EXIT PROGRAM
010190     NOT INVALID KEY
010200          IF Ž{î|V_®Žt”Ô†(1:2) = "Œ_"
010201              MOVE Ž{î|V_®Žt”Ô†(3:11)   TO _®Žt”Ô†‚v
010202          ELSE
010203              MOVE Ž{î|V_®Žt”Ô†         TO _®Žt”Ô†‚v
010204          END-IF
010205          IF  Ž{î|ÚœŽt‰ï‰ïˆõ”Ô† = SPACE
010210              MOVE  NC"Ž{pŠƒ}ƒXƒ^‚É‰ïˆõ”Ô†‚ð“o˜^‚µ‚Ä‰º‚³‚¢" TO ˜Aƒ|ƒƒbƒZ[ƒW
010220              CALL   "MSG001"
010230              CANCEL "MSG001"
010240              PERFORM ƒtƒ@ƒCƒ‹•Â½
010250              MOVE 99 TO PROGRAM-STATUS
010260              EXIT PROGRAM
010270          ELSE
010280              MOVE Ž{î|ÚœŽt‰ï‰ïˆõ”Ô†  TO ‰ïˆõ”Ô†‚v
010290              PERFORM ‰ïˆõ”Ô†‰E‹l‚ß
010300          END-IF
010310     END-READ.
010320*
010330*================================================================*
010340 ì‹Æƒtƒ@ƒCƒ‹ì¬ SECTION.
010350*
010360     PERFORM •ÛŒ¯ŽÒ”Ô†‡ƒtƒ@ƒCƒ‹ì¬.
010370*
010380     OPEN INPUT  ì‹Æƒtƒ@ƒCƒ‹‚R.
010390         MOVE NC"ì‚R" TO ƒtƒ@ƒCƒ‹–¼.
010400         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
010410*
010420     PERFORM ì‹Æƒtƒ@ƒCƒ‹‚Pì¬.
010430*
010440     CLOSE ì‹Æƒtƒ@ƒCƒ‹‚R.
010450*
010460*================================================================*
010470 •ÛŒ¯ŽÒ”Ô†‡ƒtƒ@ƒCƒ‹ì¬ SECTION.
010480**********************************************************************
010490**   ƒŒƒZƒvƒg‚e‚©‚çAŠY“–¿‹”NŒŽ‚Ìƒf[ƒ^‚ð’Šo‚µA
010500**   ì‹Æƒtƒ@ƒCƒ‹‚R(•ÛŒ¯ŽÒ”Ô†‡)‚É‘‚«o‚·.
010510**********************************************************************
010520*
010530     OPEN OUTPUT ì‹Æƒtƒ@ƒCƒ‹‚R.
010540         MOVE NC"ì‚R" TO ƒtƒ@ƒCƒ‹–¼.
010550         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
010560*
010570     MOVE ¿‹˜a—ï‚v‚q  TO ƒŒƒZ|¿‹˜a—ï.
010580     MOVE ¿‹”N‚v‚q    TO ƒŒƒZ|¿‹”N.
010590     MOVE ¿‹ŒŽ‚v‚q    TO ƒŒƒZ|¿‹ŒŽ.
010600     MOVE ZERO          TO ƒŒƒZ|ƒŒƒZŽí•Ê.
010610     MOVE ZERO          TO ƒŒƒZ|Ž{p˜a—ï.
010620     MOVE ZERO          TO ƒŒƒZ|Ž{p”N.
010630     MOVE ZERO          TO ƒŒƒZ|Ž{pŒŽ.
010640     MOVE ZERO          TO ƒŒƒZ|Š³ŽÒ”Ô†.
010650     MOVE SPACE         TO ƒŒƒZ|Ž}”Ô.
010660     START ƒŒƒZƒvƒg‚e   KEY IS >= ƒŒƒZ|¿‹˜a—ï”NŒŽ
010670                                  ƒŒƒZ|Ž{p˜a—ï”NŒŽ
010680                                  ƒŒƒZ|Š³ŽÒƒR[ƒh
010690                                  ƒŒƒZ|ƒŒƒZŽí•Ê
010700     END-START.
010710     IF ó‘ÔƒL[ = "00"
010720         MOVE SPACE  TO I—¹ƒtƒ‰ƒO
010730         PERFORM ƒŒƒZƒvƒg‚e“Çž
010740         PERFORM UNTIL ( I—¹ƒtƒ‰ƒO = "YES" ) OR
010750                       ( ƒŒƒZ|¿‹˜a—ï NOT = ¿‹˜a—ï‚v‚q ) OR
010760                       ( ƒŒƒZ|¿‹”N   NOT = ¿‹”N‚v‚q   ) OR
010770                       ( ƒŒƒZ|¿‹ŒŽ   NOT = ¿‹ŒŽ‚v‚q   )
010780            PERFORM ƒf[ƒ^ƒ`ƒFƒbƒN
010790**
010800            IF  ŽÀsƒL[‚v = "YES"
010810*/’·–ìŒ§“à‚Ì52•êŽqA53áŠQA55“û—cŽ™A60‚»‚Ì‘¼‚ÍžŠÒ•¥‚¢‚Ìˆ×ƒf[ƒ^‚ÉÚ‚¹‚È‚¢/110922
010820                 IF (Žó|•¬Ží•Ê = 52 OR 53 OR 55 OR 60) AND
010830                    (Žó|”ï—p•‰’SŽÒ”Ô†•¬(3:2) = "20" )
010840                     MOVE ZERO  TO Žó|•¬Ží•Ê
010850                     MOVE SPACE TO Žó|”ï—p•‰’SŽÒ”Ô†•¬
010860                 END-IF
010870*
010880                 MOVE SPACE TO ì‚R|ƒŒƒR[ƒh
010890                 INITIALIZE    ì‚R|ƒŒƒR[ƒh
010900                 MOVE Žó|¿‹˜a—ï   TO  ì‚R|¿‹˜a—ï
010910                 MOVE Žó|¿‹”N     TO  ì‚R|¿‹”N
010920                 MOVE Žó|¿‹ŒŽ     TO  ì‚R|¿‹ŒŽ
010930                 MOVE Žó|Ž{p˜a—ï   TO  ì‚R|Ž{p˜a—ï
010940                 MOVE Žó|Ž{p”N     TO  ì‚R|Ž{p”N
010950                 MOVE Žó|Ž{pŒŽ     TO  ì‚R|Ž{pŒŽ
010960                 IF  Žó|•¬Ží•Ê   = ZERO  OR 50
010970                     IF ( Žó|•ÛŒ¯Ží•Ê   NOT = ZERO ) AND
010980                        ( Žó|•ÛŒ¯ŽÒ”Ô† NOT = SPACE )
010990** •¬‚È‚µ(¶•Ûe‚ ‚è‚Í•¬‚È‚µˆµ‚¢)
011000                        MOVE ZERO  TO   ì‚R|•¬‹æ•ª
011010                     END-IF
011020                 ELSE
011030** •¬‚ ‚è
011040                     MOVE 1        TO   ì‚R|•¬‹æ•ª
011050*                   /“Á•ÊF •êŽq‚Ü‚½‚ÍáŠQ‚Å‚©‚ÂÃ‰ªŒ§ ‚ÍA•¬‚È‚µ‚É‚·‚é/
011060                     IF (( Žó|•¬Ží•Ê = "52" ) OR ( Žó|•¬Ží•Ê = "53" )) AND
011070                        ( Žó|”ï—p•‰’SŽÒ”Ô†•¬(3:2) = "22" ) 
011080                         MOVE ZERO  TO  ì‚R|•¬‹æ•ª
011090                     END-IF
011100                 END-IF
011110*
011210                 IF ( Žó|Œö”ïŽí•Ê       = ZERO  ) AND
011211                    ( Žó|”ï—p•‰’SŽÒ”Ô† = SPACE )
011212                     MOVE Žó|•ÛŒ¯ŽÒ”Ô†     TO •ÛŒ¯ŽÒ”Ô†‚v
011213                 ELSE
011214* ˜Vl‚ÍAŽs’¬‘º”Ô†
011215                     MOVE Žó|”ï—p•‰’SŽÒ”Ô† TO •ÛŒ¯ŽÒ”Ô†‚v
011216                 END-IF
011217                 PERFORM •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß
011218                 MOVE •ÛŒ¯ŽÒ”Ô†”Žš‚v   TO ì‚R|•ÛŒ¯ŽÒ”Ô†
011219*
011220                 MOVE Žó|–{l‰Æ‘°‹æ•ª   TO ì‚R|–{l‰Æ‘°‹æ•ª
011230                 MOVE Žó|”í•ÛŒ¯ŽÒƒJƒi   TO ì‚R|”í•ÛŒ¯ŽÒƒJƒi
011240                 MOVE Žó|Š³ŽÒƒR[ƒh     TO ì‚R|Š³ŽÒƒR[ƒh
011250*
011260                 EVALUATE ƒŒƒZ|ƒŒƒZŽí•Ê
011270                 WHEN 1
011280                 WHEN 2
011290                     MOVE ZERO           TO ì‚R|eŽq‹æ•ª
011300                 WHEN 3
011310                     MOVE 1              TO ì‚R|eŽq‹æ•ª
011320                 END-EVALUATE
011330*
011340                 IF (ƒŒƒZ|ƒŒƒZŽí•Ê = 3) AND (ì‚R|•¬‹æ•ª = ZERO)
011350                    CONTINUE
011360                 ELSE
011370                    WRITE ì‚R|ƒŒƒR[ƒh
011380                    INVALID KEY
011390                        MOVE NC"ì‚R"  TO ƒtƒ@ƒCƒ‹–¼
011400                    PERFORM ƒGƒ‰[•\Ž¦
011410                    END-WRITE
011420                 END-IF
011430             END-IF
011440             PERFORM ƒŒƒZƒvƒg‚e“Çž
011450         END-PERFORM
011460     END-IF.
011470*
011480     CLOSE ì‹Æƒtƒ@ƒCƒ‹‚R.
011490*
011500*================================================================*
011510 ì‹Æƒtƒ@ƒCƒ‹‚Pì¬ SECTION.
011520*
011530     MOVE SPACE  TO I—¹ƒtƒ‰ƒO.
011540     PERFORM ì‹Æƒtƒ@ƒCƒ‹‚R“Çž.
011550     PERFORM UNTIL  I—¹ƒtƒ‰ƒO = "YES" 
011560*
011570         MOVE SPACE TO •¬ƒtƒ‰ƒO
011580         MOVE SPACE TO •¬ƒŒƒZ‚Ü‚Æ‚ßƒtƒ‰ƒO
011590         MOVE "YES" TO ŽÀsƒL[‚v
011600*
011610** ˜JÐEŽ©”…ÓEŽ©—RE ¶•Û’P“Æ‚Í‘ÎÛŠO
011620            IF  Žó|•ÛŒ¯Ží•Ê = 70 OR 80 OR 85 OR 90
011630                MOVE SPACE  TO ŽÀsƒL[‚v
011640            END-IF
011650** Ž‘ŠiØ–¾‚Í‘ÎÛŠO
011660         IF  ( Žó|•ÛŒ¯Ží•Ê = 01 OR 08 ) AND
011670             ( Žó|Œö”ïŽí•Ê = ZERO     ) AND
011680             ( Žó|Ž‘ŠiØ–¾‹æ•ª = 1 )
011690            MOVE SPACE  TO ŽÀsƒL[‚v
011700         END-IF
011710**
011720         IF  ŽÀsƒL[‚v = "YES"
011730*
011741**«* “Á•Êˆ—i•¬ƒŒƒZ‚Ü‚Æ‚ßj
011750             IF Žó|•¬Ží•Ê NOT = ZERO
011760                 PERFORM •¬ƒŒƒZ‚Ü‚Æ‚ß”»’è
011770             ELSE
011780                 MOVE SPACE TO •¬ƒŒƒZ‚Ü‚Æ‚ßƒtƒ‰ƒO
011790             END-IF
011800**ª*
011810*            ********
011820*            * Œ’•Û *
011830*            ********
011840             IF ì‚R|eŽq‹æ•ª = ZERO
011850                IF ( Žó|•ÛŒ¯Ží•Ê   NOT = ZERO ) AND
011860                   ( Žó|•ÛŒ¯ŽÒ”Ô† NOT = SPACE )
011870*                **********************
011880*                * ì‹Æƒtƒ@ƒCƒ‹ì¬ *
011890*                **********************
011900                    IF ( Žó|Œö”ïŽí•Ê       = ZERO  ) AND
011910                       ( Žó|”ï—p•‰’SŽÒ”Ô† = SPACE )
011920                       MOVE 1        TO  ˆã—Ã•¬‹æ•ª‚v
011922                       IF ì‚R|•¬‹æ•ª  = ZERO
011930*   / •¬‚È‚µ /
011940                          MOVE SPACE TO •¬ƒtƒ‰ƒO
011950                       ELSE
011960*   / •¬‚ ‚è /
011970                          MOVE "YES" TO •¬ƒtƒ‰ƒO
011980                       END-IF
011990                       PERFORM ì‚PƒŒƒR[ƒhƒZƒbƒgŒ’•Û
012000                       PERFORM ì‚Pƒtƒ@ƒCƒ‹‘ž
012010                    END-IF
012020                END-IF
012030             END-IF
012040*            ********
012050*            * ˜Vl *
012060*            ********
012070             IF ì‚R|eŽq‹æ•ª = ZERO
012080                IF ( Žó|Œö”ïŽí•Ê       NOT = ZERO ) AND
012090                   ( Žó|”ï—p•‰’SŽÒ”Ô† NOT = SPACE )
012100*                **********************
012110*                * ì‹Æƒtƒ@ƒCƒ‹ì¬ *
012120*                **********************
012130                   MOVE 1        TO ˆã—Ã•¬‹æ•ª‚v
012131                   IF ì‚R|•¬‹æ•ª  = ZERO
012140*   / •¬‚È‚µ /
012150                      MOVE SPACE TO •¬ƒtƒ‰ƒO
012160                   ELSE
012170*   / •¬‚ ‚è /
012180                      MOVE "YES" TO •¬ƒtƒ‰ƒO
012190                   END-IF
012200                   PERFORM ì‚PƒŒƒR[ƒhƒZƒbƒg˜Vl
012210                   PERFORM ì‚Pƒtƒ@ƒCƒ‹‘ž
012220                END-IF
012230             END-IF
012240*            ********
012250*            * •¬ *
012260*            ********
012270             IF ì‚R|eŽq‹æ•ª = 1
005930*         / •¬‚Ì¿‹Šz‚O‚Í‘ÎÛŠO‚É‚·‚é /170621
005930*         / ‘åã‚Ì•¬‚Ì¿‹Šz‚O‚Í‘ÎÛ‚É‚·‚é /170621
006880                IF (ƒŒƒZ|•¬¿‹‹àŠz NOT = ZERO) OR
                         (Žó|”ï—p•‰’SŽÒ”Ô†•¬(3:2) = "27")
012280                   MOVE "YES" TO •¬ƒtƒ‰ƒO
012281                   MOVE 3     TO ˆã—Ã•¬‹æ•ª‚v
012290                   IF Žó|Œö”ïŽí•Ê = ZERO
012300                       PERFORM ì‚PƒŒƒR[ƒhƒZƒbƒgŒ’•Û•¬
012310                   ELSE
012320                       PERFORM ì‚PƒŒƒR[ƒhƒZƒbƒg˜Vl•¬
012330                   END-IF
012340                   PERFORM ì‚Pƒtƒ@ƒCƒ‹‘ž
012350                END-IF
                   END-IF
012360         END-IF
012370         PERFORM ì‹Æƒtƒ@ƒCƒ‹‚R“Çž
012380     END-PERFORM.
012390*
012400*================================================================*
012410 ì‹Æƒtƒ@ƒCƒ‹‚R“Çž SECTION.
012420*
012430     READ ì‹Æƒtƒ@ƒCƒ‹‚R NEXT
012440     AT END
012450         MOVE "YES" TO I—¹ƒtƒ‰ƒO
012460     NOT AT END
012470         MOVE ì‚R|Ž{p˜a—ï    TO Žó|Ž{p˜a—ï ƒŒƒZ|Ž{p˜a—ï
012480         MOVE ì‚R|Ž{p”N      TO Žó|Ž{p”N   ƒŒƒZ|Ž{p”N  
012490         MOVE ì‚R|Ž{pŒŽ      TO Žó|Ž{pŒŽ   ƒŒƒZ|Ž{pŒŽ  
012500         MOVE ì‚R|Š³ŽÒ”Ô†    TO Žó|Š³ŽÒ”Ô† ƒŒƒZ|Š³ŽÒ”Ô†
012510         MOVE ì‚R|Ž}”Ô        TO Žó|Ž}”Ô     ƒŒƒZ|Ž}”Ô    
012520         READ ŽófŽÒî•ñ‚e
012530         INVALID KEY
012540              MOVE NC"ŽófŽÒ"   TO ƒtƒ@ƒCƒ‹–¼
012550              PERFORM ƒGƒ‰[•\Ž¦‚q
012560         END-READ
012570         IF ì‚R|eŽq‹æ•ª = 1
012580             MOVE 3          TO ƒŒƒZ|ƒŒƒZŽí•Ê
012590         ELSE
012600            IF Žó|Œö”ïŽí•Ê = 5
012610                MOVE 2          TO ƒŒƒZ|ƒŒƒZŽí•Ê
012620            ELSE
012630                MOVE 1          TO ƒŒƒZ|ƒŒƒZŽí•Ê
012640            END-IF
012650         END-IF
012660         READ ƒŒƒZƒvƒg‚e
012670         INVALID KEY
012680              MOVE NC"ƒŒƒZƒvƒg"   TO ƒtƒ@ƒCƒ‹–¼
012690              PERFORM ƒGƒ‰[•\Ž¦‚q
012700         END-READ
012710     END-READ.
012720*
012730*================================================================*
012740 ƒf[ƒ^ƒ`ƒFƒbƒN SECTION.
012750*
012760     MOVE SPACE          TO ŽÀsƒL[‚v.
012770* *****************************************************************
012780* * ƒŒƒZƒvƒg‚e‚Ì¿‹‘ÎÛ‹æ•ª = 0 ‚Ìê‡ƒf[ƒ^ì¬‘ÎÛ‚Æ‚µ‚È‚¢ *
012790* *****************************************************************
012800     IF ( ƒŒƒZ|¿‹‘ÎÛ‹æ•ª NOT = ZERO ) AND
012810        ( ƒŒƒZ|žŠÒ•¥‚¢‹æ•ª NOT = 1 )
012820        IF(ƒŒƒZ|ƒŒƒZŽí•Ê = 3) AND ( ƒŒƒZ|‰ï‘Š‡•\ˆóü‘ÎÛ‹æ•ª = 1 )
012830           CONTINUE
012840        ELSE
012850           MOVE ƒŒƒZ|Ž{p˜a—ï  TO Žó|Ž{p˜a—ï
012860           MOVE ƒŒƒZ|Ž{p”N    TO Žó|Ž{p”N
012870           MOVE ƒŒƒZ|Ž{pŒŽ    TO Žó|Ž{pŒŽ
012880           MOVE ƒŒƒZ|Š³ŽÒ”Ô†  TO Žó|Š³ŽÒ”Ô†
012890           MOVE ƒŒƒZ|Ž}”Ô      TO Žó|Ž}”Ô
012900           READ ŽófŽÒî•ñ‚e
012910           NOT INVALID KEY
012920**      Œ’•Û‚Ì‚Ý
012930              IF Žó|•ÛŒ¯•ª—Þ = 1
012940                 MOVE "YES"  TO ŽÀsƒL[‚v
012950              END-IF
012960           END-READ
012970        END-IF
012980     END-IF.
012990*
013000*================================================================*
013010 ƒŒƒZƒvƒg‚e“Çž SECTION.
013020*
013030     READ ƒŒƒZƒvƒg‚e NEXT
013040     AT END
013050         MOVE "YES" TO I—¹ƒtƒ‰ƒO
013060     END-READ.
013070*
013080*================================================================*
013090 Ž{p‹L˜^‚e“Çž SECTION.
013100*
013110     READ Ž{p‹L˜^‚e NEXT
013120     AT END
013130         MOVE "YES"  TO I—¹ƒtƒ‰ƒO‚Q
013140     END-READ.
013150*================================================================*
013160*================================================================*
013170 ì‚PƒŒƒR[ƒhƒZƒbƒgŒ’•Û SECTION.
013180*
013190**********/  Œ’•Û•¬‚È‚µ‚ÌŽž  /**********
013200*
013210     MOVE SPACE TO ì‚P|ƒŒƒR[ƒh.
013220     INITIALIZE ì‚P|ƒŒƒR[ƒh.
013230*
013241*«* “Á•Êˆ—i•¬ƒŒƒZ‚Ü‚Æ‚ßj
013250     MOVE ƒŒƒZ|‡Œv               TO ì‚P|‡Œv‹àŠz.
013260     MOVE ƒŒƒZ|ˆê•”•‰’S‹à         TO ì‚P|ˆê•”•‰’S‹à.
013270     MOVE ƒŒƒZ|¿‹‹àŠz           TO ì‚P|¿‹‹àŠz.
013280     IF •¬ƒŒƒZ‚Ü‚Æ‚ßƒtƒ‰ƒO = "YES"
013290         MOVE ƒŒƒZ|Žó‹‹ŽÒ•‰’SŠz   TO ì‚P|Œö”ï•‰’S‹àŠz
013300         MOVE ƒŒƒZ|•¬¿‹‹àŠz   TO ì‚P|Œö”ï¿‹‹àŠz
013335     ELSE
013337         MOVE ZERO                 TO ì‚P|Œö”ï•‰’S‹àŠz
013338         MOVE ZERO                 TO ì‚P|Œö”ï¿‹‹àŠz
013339     END-IF.
013340*ª*
013350*
013360     MOVE 1          TO  ì‚P|•ÛŒ¯‹æ•ªƒL[.
013370*
013390* •¬ƒŒƒZ‚Ü‚Æ‚ßŽž‚ÍA•‰’SŽÒ”Ô†EŽó‹‹ŽÒ”Ô†‚ðƒZƒbƒg
013391     IF •¬ƒtƒ‰ƒO = "YES" AND •¬ƒŒƒZ‚Ü‚Æ‚ßƒtƒ‰ƒO = "YES"
013400*
013403         PERFORM •¬•‰’SŽÒ”Ô†Žæ“¾
013411*
013420         IF ( Žó|Žó‰vŽÒ”Ô†•¬(1:1) = "*"  ) OR
013430            ( Žó|Žó‰vŽÒ”Ô†•¬(1:2) = "–" )
013440            MOVE SPACE                TO ì‚P|•¬Žó‹‹ŽÒ”Ô† 
013450         ELSE
013460            MOVE Žó|Žó‰vŽÒ”Ô†•¬   TO ì‚P|•¬Žó‹‹ŽÒ”Ô†
013470         END-IF
013480     ELSE
013481         MOVE SPACE                   TO ì‚P|•¬•‰’SŽÒ”Ô†
013482         MOVE SPACE                   TO ì‚P|•¬Žó‹‹ŽÒ”Ô†
013483     END-IF.
013490*
013500     MOVE Žó|–{l‰Æ‘°‹æ•ª   TO ì‚P|–{l‰Æ‘°‹æ•ª.
013510*
013520* ZŠ(”í•ÛŒ¯ŽÒ)
013530     STRING Žó|ZŠ‚P    DELIMITED BY SPACE
013540            Žó|ZŠ‚Q    DELIMITED BY SPACE
013550            INTO ì‚P|”í•ÛŒ¯ŽÒZŠ
013560     END-STRING.
013570*
013580     PERFORM ‹¤’ÊƒŒƒR[ƒhƒZƒbƒg.
013590*
013600*================================================================*
013610 ì‚PƒŒƒR[ƒhƒZƒbƒg˜Vl SECTION.
013620*
013630**********/ 27˜Vl‚ÌŽž  /**********
013640*
013890*«* “Á•Êˆ—i•¬ƒŒƒZ‚Ü‚Æ‚ßj
013891     MOVE ƒŒƒZ|‡Œv               TO ì‚P|‡Œv‹àŠz.
013892     MOVE ƒŒƒZ|ˆê•”•‰’S‹à         TO ì‚P|ˆê•”•‰’S‹à.
013893     MOVE ƒŒƒZ|¿‹‹àŠz           TO ì‚P|¿‹‹àŠz.
013894     IF •¬ƒŒƒZ‚Ü‚Æ‚ßƒtƒ‰ƒO = "YES"
013895         MOVE ƒŒƒZ|Žó‹‹ŽÒ•‰’SŠz   TO ì‚P|Œö”ï•‰’S‹àŠz
013896         MOVE ƒŒƒZ|•¬¿‹‹àŠz   TO ì‚P|Œö”ï¿‹‹àŠz
013897     ELSE
013898         MOVE ZERO                 TO ì‚P|Œö”ï•‰’S‹àŠz
013899         MOVE ZERO                 TO ì‚P|Œö”ï¿‹‹àŠz
013900     END-IF.
013901*ª*
013902**
013903     IF Žó|Ž{p˜a—ï”NŒŽ < 42004
013910         MOVE 2              TO  ì‚P|•ÛŒ¯‹æ•ªƒL[
013920     ELSE
013930         MOVE 1              TO  ì‚P|•ÛŒ¯‹æ•ªƒL[
013940     END-IF.
013950*
014080* •¬ƒŒƒZ‚Ü‚Æ‚ßŽž‚ÍA•‰’SŽÒ”Ô†EŽó‹‹ŽÒ”Ô†‚ðƒZƒbƒg
014081     IF •¬ƒtƒ‰ƒO = "YES" AND •¬ƒŒƒZ‚Ü‚Æ‚ßƒtƒ‰ƒO = "YES"
014082*
014083         PERFORM •¬•‰’SŽÒ”Ô†Žæ“¾
014084*
014085         IF ( Žó|Žó‰vŽÒ”Ô†•¬(1:1) = "*"  ) OR
014086            ( Žó|Žó‰vŽÒ”Ô†•¬(1:2) = "–" )
014087            MOVE SPACE                TO ì‚P|•¬Žó‹‹ŽÒ”Ô† 
014088         ELSE
014089            MOVE Žó|Žó‰vŽÒ”Ô†•¬   TO ì‚P|•¬Žó‹‹ŽÒ”Ô†
014090         END-IF
014091     ELSE
014092         MOVE SPACE                   TO ì‚P|•¬•‰’SŽÒ”Ô†
014093         MOVE SPACE                   TO ì‚P|•¬Žó‹‹ŽÒ”Ô†
014094     END-IF.
014095*
014096* –{l‚Ì‚Ý
014097     MOVE 1   TO ì‚P|–{l‰Æ‘°‹æ•ª.
014100*
014110* ZŠ(Š³ŽÒ)
014120     STRING Žó|Š³ŽÒZŠ‚P    DELIMITED BY SPACE
014130            Žó|Š³ŽÒZŠ‚Q    DELIMITED BY SPACE
014140            INTO ì‚P|”í•ÛŒ¯ŽÒZŠ
014150     END-STRING.
014160*
014170     PERFORM ‹¤’ÊƒŒƒR[ƒhƒZƒbƒg.
014180*
014190*================================================================*
014200 ì‚PƒŒƒR[ƒhƒZƒbƒgŒ’•Û•¬ SECTION.
014210*
014220**********/  Œ’•Û•¬‚ ‚è‚ÌŽž  /**********
014230*
014240     MOVE SPACE TO ì‚P|ƒŒƒR[ƒh.
014250     INITIALIZE ì‚P|ƒŒƒR[ƒh.
014260*
014271     PERFORM •¬•‰’SŽÒ”Ô†Žæ“¾.
014280*
014290     IF ( Žó|Žó‰vŽÒ”Ô†•¬(1:1) = "*"  ) OR
014300        ( Žó|Žó‰vŽÒ”Ô†•¬(1:2) = "–" )
014310        MOVE SPACE                TO ì‚P|•¬Žó‹‹ŽÒ”Ô†
014320     ELSE
014330        MOVE Žó|Žó‰vŽÒ”Ô†•¬   TO ì‚P|•¬Žó‹‹ŽÒ”Ô†
014340     END-IF.
014350*
014511     MOVE ƒŒƒZ|‡Œv              TO ì‚P|‡Œv‹àŠz.
014512     MOVE ƒŒƒZ|ˆê•”•‰’S‹à        TO ì‚P|ˆê•”•‰’S‹à.
014513     MOVE ƒŒƒZ|¿‹‹àŠz          TO ì‚P|¿‹‹àŠz.
014515     MOVE ƒŒƒZ|Žó‹‹ŽÒ•‰’SŠz      TO ì‚P|Œö”ï•‰’S‹àŠz.
014516     MOVE ƒŒƒZ|•¬¿‹‹àŠz      TO ì‚P|Œö”ï¿‹‹àŠz.
014519*
014526     MOVE 3                       TO  ì‚P|•ÛŒ¯‹æ•ªƒL[.
014530*
014540* –{l‚Ì‚Ý
014550     MOVE 1   TO ì‚P|–{l‰Æ‘°‹æ•ª.
014560*
014570* ZŠ(Š³ŽÒ)
014580     STRING Žó|Š³ŽÒZŠ‚P    DELIMITED BY SPACE
014590            Žó|Š³ŽÒZŠ‚Q    DELIMITED BY SPACE
014600            INTO ì‚P|”í•ÛŒ¯ŽÒZŠ
014610     END-STRING.
014620*
014630     PERFORM ‹¤’ÊƒŒƒR[ƒhƒZƒbƒg.
014640*
014650*================================================================*
014660 ì‚PƒŒƒR[ƒhƒZƒbƒg˜Vl•¬ SECTION.
014670*
014680**********/  ˜Vl•¬‚ ‚è‚ÌŽž  /**********
014690*
014700     MOVE SPACE TO ì‚P|ƒŒƒR[ƒh.
014710     INITIALIZE ì‚P|ƒŒƒR[ƒh.
014820*
014830     PERFORM •¬•‰’SŽÒ”Ô†Žæ“¾.
014840*
014850	    IF ( Žó|Žó‰vŽÒ”Ô†•¬(1:1) = "*"  ) OR
014860        ( Žó|Žó‰vŽÒ”Ô†•¬(1:2) = "–" )
014870        MOVE SPACE                TO ì‚P|•¬Žó‹‹ŽÒ”Ô†
014880     ELSE
014890        MOVE Žó|Žó‰vŽÒ”Ô†•¬   TO ì‚P|•¬Žó‹‹ŽÒ”Ô†
014900     END-IF.
014910*
015211     MOVE ƒŒƒZ|‡Œv              TO ì‚P|‡Œv‹àŠz.
015212     MOVE ƒŒƒZ|ˆê•”•‰’S‹à        TO ì‚P|ˆê•”•‰’S‹à.
015213     MOVE ƒŒƒZ|¿‹‹àŠz          TO ì‚P|¿‹‹àŠz.
015215     MOVE ƒŒƒZ|Žó‹‹ŽÒ•‰’SŠz      TO ì‚P|Œö”ï•‰’S‹àŠz.
015216     MOVE ƒŒƒZ|•¬¿‹‹àŠz      TO ì‚P|Œö”ï¿‹‹àŠz.
015219*
015220     MOVE 3                       TO  ì‚P|•ÛŒ¯‹æ•ªƒL[.
015230*
015240* –{l‚Ì‚Ý
015250     MOVE 1   TO ì‚P|–{l‰Æ‘°‹æ•ª.
015260*
015270* ZŠ(Š³ŽÒ)
015280     STRING Žó|Š³ŽÒZŠ‚P    DELIMITED BY SPACE
015290            Žó|Š³ŽÒZŠ‚Q    DELIMITED BY SPACE
015300            INTO ì‚P|”í•ÛŒ¯ŽÒZŠ
015310     END-STRING.
015320*
015330     PERFORM ‹¤’ÊƒŒƒR[ƒhƒZƒbƒg.
015340*
015350*================================================================*
015360*================================================================*
015370 ‹¤’ÊƒŒƒR[ƒhƒZƒbƒg SECTION.
015380*
015390     MOVE Žó|¿‹˜a—ï       TO ì‚P|¿‹˜a—ï.
015400     MOVE Žó|¿‹”N         TO ì‚P|¿‹”N.
015410     MOVE Žó|¿‹ŒŽ         TO ì‚P|¿‹ŒŽ.
015420     MOVE Žó|Ž{p˜a—ï       TO ì‚P|Ž{p˜a—ï Ž{p˜a—ï‚v‚q.
015430     MOVE Žó|Ž{p”N         TO ì‚P|Ž{p”N Ž{p”N‚v‚q.
015440     MOVE Žó|Ž{pŒŽ         TO ì‚P|Ž{pŒŽ Ž{pŒŽ‚v‚q.
015450     MOVE Žó|Š³ŽÒƒR[ƒh     TO ì‚P|Š³ŽÒƒR[ƒhƒL[  Š³ŽÒƒR[ƒh‚v‚q.
015470
015493* ”NŒŽ
015500     MOVE ¼—ï¿‹”NŒŽ‚v     TO ì‚P|¿‹”NŒŽ.
015512*
015520     PERFORM ¼—ïŽ{p”NŒŽŽæ“¾.
015530     MOVE ¼—ïŽ{p”NŒŽ‚v     TO ì‚P|Ž{p”NŒŽ.
015541*
015542     MOVE ‰ïˆõ”Ô†”Žš‚v     TO ì‚P|‰ïˆõ”Ô†.
015580*
015590     MOVE _®Žt”Ô†‚v       TO ì‚P|“o˜^‹L†”Ô†.
015593
015594* •ÛŒ¯ŽÒ”Ô†ƒL[(”Žš)
015600     IF ( Žó|Œö”ïŽí•Ê       = ZERO  ) AND
015610        ( Žó|”ï—p•‰’SŽÒ”Ô† = SPACE )
015620          MOVE Žó|•ÛŒ¯ŽÒ”Ô†     TO •ÛŒ¯ŽÒ”Ô†‚v
015630     ELSE
015640* / ˜Vl‚ÍAŽs’¬‘º”Ô†‚ðƒL[‚É /
015650         IF Žó|Ž{p˜a—ï”NŒŽ < 42004
015660             MOVE Žó|”ï—p•‰’SŽÒ”Ô† TO •ÛŒ¯ŽÒ”Ô†‚v
015670         ELSE
015680             MOVE Žó|•ÛŒ¯ŽÒ”Ô†     TO •ÛŒ¯ŽÒ”Ô†‚v
015690         END-IF
015700     END-IF.
015710     PERFORM •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß.
015720     MOVE •ÛŒ¯ŽÒ”Ô†”Žš‚v   TO ì‚P|•ÛŒ¯ŽÒ”Ô†ƒL[.
015730*
015740* •ÛŒ¯ŽÒ”Ô†(”Žš)
015750     MOVE Žó|•ÛŒ¯ŽÒ”Ô†     TO ì‚P|•ÛŒ¯ŽÒ”Ô†.
015780*
015790** ‘S‘“y–Ø (133033) ‚ÌŽ}”Ôíœ‚µ‚ÄA•ÛŒ¯ŽÒ”Ô†‚ÉƒZƒbƒg
015800     IF ( Žó|•ÛŒ¯Ží•Ê = 01 ) AND ( Žó|•ÛŒ¯ŽÒ”Ô†(1:6) = "133033" )
015810         MOVE 133033         TO ì‚P|•ÛŒ¯ŽÒ”Ô†  ì‚P|•ÛŒ¯ŽÒ”Ô†ƒL[
015820     END-IF.
015830*
015840*-----------------------------------------------------------------*
015850     MOVE SPACE TO ˜AˆÃ†•¡‡|ˆÃ†î•ñ.
015860*
015870*    / ˜AˆÃ†•¡‡|“ü—Íî•ñƒZƒbƒg /
015880     MOVE Žó|‹L†       TO ˜AˆÃ†•¡‡|‹L†.
015890     MOVE Žó|”Ô†       TO ˜AˆÃ†•¡‡|”Ô†.
015900     MOVE Žó|ˆÃ†‰»€–Ú TO ˜AˆÃ†•¡‡|ˆÃ†‰»€–Ú.
015910*
015920     CALL   •¡‡ƒvƒƒOƒ‰ƒ€–¼‚v.
015930     CANCEL •¡‡ƒvƒƒOƒ‰ƒ€–¼‚v.
015940*
015950*-----------------------------------------------------------------*
015960* ‹L†
015980     IF ˜AˆÃ†•¡‡|•¡‡‚µ‚½‹L†(1:2)  = "–" 
015990        MOVE SPACE               TO ‹L†‚o‚m‚v
016010        MOVE ‹L†‚o‚v            TO ì‚P|‹L†
016020     ELSE
016030        PERFORM ‹L†¶‹l‚ß
016040        MOVE ‹L†‚o‚v            TO ì‚P|‹L†
016050     END-IF.
016060* ”Ô†
016090     IF ( ˜AˆÃ†•¡‡|•¡‡‚µ‚½”Ô†(1:1) = "*"  ) OR
016100        ( ˜AˆÃ†•¡‡|•¡‡‚µ‚½”Ô†(1:2) = "–" )
016110        MOVE SPACE           TO ì‚P|”Ô†
016120     ELSE
016140        MOVE ˜AˆÃ†•¡‡|•¡‡‚µ‚½”Ô† TO ì‚P|”Ô†
016150     END-IF.
016192
016193*/‘åã•{“à‚Ì•¬‚Í–{‘Ì‚É•‰’SŽÒ”Ô†AŽó‹‹ŽÒ”Ô†‚ð‹LÚ‚·‚é
016194     MOVE Žó|”ï—p•‰’SŽÒ”Ô†•¬ TO Žs’¬‘º”Ô†‚v
016195     MOVE Žó|Žó‰vŽÒ”Ô†•¬     TO Žó‹‹ŽÒ”Ô†‚v
016196     IF Žs’¬‘º”Ô†‚v(3:2) = "27"
016197         IF Žs’¬‘º”Ô†‚v(1:2) NOT = "99"
016198             MOVE Žs’¬‘º”Ô†‚v TO ì‚P|•¬•‰’SŽÒ”Ô†
016199         END-IF
016200         MOVE Žó‹‹ŽÒ”Ô†‚v     TO ì‚P|•¬Žó‹‹ŽÒ”Ô†
016201     END-IF.
016202* ˆã—Ã•¬‹æ•ª
016203     MOVE ˆã—Ã•¬‹æ•ª‚v       TO ì‚P|ˆã—Ã•¬‹æ•ª.
016211* •ÛŒ¯Ží•Ê
016212     MOVE Žó|•ÛŒ¯Ží•Ê         TO •ÛŒ¯Ží•Ê•ÏŠ·‘O‚v.
016213     PERFORM •ÛŒ¯Ží•Ê•ÏŠ·.
016214     MOVE •ÛŒ¯Ží•Ê•ÏŠ·Œã‚v     TO ì‚P|•ÛŒ¯Ží•Ê‹æ•ª.
016215*
016216* ’P•¹‹æ•ª
016217     IF Žó|•¬Ží•Ê = ZERO
016218*        ’P“Æ
016219         MOVE 1 TO ì‚P|’P•¹‹æ•ª
016220     ELSE
016221*        ‚Q•¹
016222         MOVE 2 TO ì‚P|’P•¹‹æ•ª
016223     END-IF
016224* –{‰Æ‹æ•ª
016225     IF Žó|•ÛŒ¯Ží•Ê = 05
016226         EVALUATE Žó|“Á•Ê‹æ•ª
016227         WHEN 1
016228*            ‚ˆê
016229             MOVE 8      TO ì‚P|–{‰Æ‹æ•ª
016230         WHEN 3
016231*            ‚‚V
016232             MOVE ZERO   TO ì‚P|–{‰Æ‹æ•ª
016233         END-EVALUATE
016234     ELSE
016235         EVALUATE Žó|“Á•Ê‹æ•ª
016236         WHEN 1
016237         WHEN 2
016238*            ‚ˆê
016239             MOVE 8      TO ì‚P|–{‰Æ‹æ•ª
016240         WHEN 3
016241*            ‚‚V
016242             MOVE ZERO   TO ì‚P|–{‰Æ‹æ•ª
016243         WHEN 6
016244*            ‚UÎ
016245             MOVE 4      TO ì‚P|–{‰Æ‹æ•ª
016246         WHEN OTHER
016247             IF Žó|–{l‰Æ‘°‹æ•ª = 1
016248*                –{l
016249                 MOVE 2  TO ì‚P|–{‰Æ‹æ•ª
016250             ELSE
016251*                ‰Æ‘°
016252                 MOVE 6  TO ì‚P|–{‰Æ‹æ•ª
016253             END-IF
016254         END-EVALUATE
016255     END-IF
016256* ‹‹•tŠ„‡
016257     MOVE ƒŒƒZ|‹‹•tŠ„‡ TO ì‚P|‹‹•tŠ„‡.
016258*
016259     IF Žó|–{l‰Æ‘°‹æ•ª = 1
016260*        –{l
016261         MOVE 1  TO ì‚P|–{l‰Æ‘°‹æ•ª
016262     ELSE
016263*        ‰Æ‘°
016264         MOVE 2  TO ì‚P|–{l‰Æ‘°‹æ•ª
016265     END-IF
016266
016267* Ž–¼
016268     MOVE Žó|”í•ÛŒ¯ŽÒŽ–¼   TO Ž–¼‚v.
016269     MOVE ‘SŠpŽ–¼‚v         TO ì‚P|”í•ÛŒ¯ŽÒŽ–¼.
016270     MOVE Žó|”í•ÛŒ¯ŽÒƒJƒi   TO ì‚P|”í•ÛŒ¯ŽÒƒJƒi ì‚P|”í•ÛŒ¯ŽÒƒJƒiƒL[.
016271     MOVE Žó|Š³ŽÒŽ–¼       TO Ž–¼‚v.
016272     MOVE ‘SŠpŽ–¼‚v         TO ì‚P|Š³ŽÒŽ–¼.
016273     MOVE Žó|Š³ŽÒƒJƒi       TO ì‚P|Š³ŽÒƒJƒi.
016274*
016275     MOVE Žó|Š³ŽÒ«•Ê       TO ì‚P|Š³ŽÒ«•Ê.
016276* ¶”NŒŽ“ú
016277     MOVE ZERO               TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016278     MOVE Žó|Š³ŽÒ¶”NŒŽ“ú   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016280     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
016290     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v   TO ì‚P|Š³ŽÒ¶”NŒŽ“ú.
016300*
016310****@/ ƒŒƒZƒvƒgƒf[ƒ^‚ÌŽæ“¾ /
016320*
016330     PERFORM •‰ƒf[ƒ^Žæ“¾.
016340     PERFORM —¿‹àî•ñŽæ“¾.
016350     PERFORM Ž{p‹L˜^Žæ“¾.
016351*
016352     MOVE ‘S‘ÌŽÀ“ú”‚v           TO ì‚P|‘S‘ÌŽÀ“ú”.
016353*
016354*ƒŒƒZƒRƒ“‹ÆŽÒ
016361     MOVE 8                      TO ì‚P|‹ÆŽÒ‹æ•ª.
016362*‚—îŽÒ‹æ•ª
016363     MOVE ZERO                   TO ì‚P|‚—îŽÒ‹æ•ª
016364     IF Žó|•ÛŒ¯Ží•Ê NOT = 05 AND Žó|“Á•Ê‹æ•ª NOT = ZERO
016365         EVALUATE Žó|“Á•Ê‹æ•ª
016366         WHEN 1
016367         WHEN 2
016368         WHEN 3
016369             MOVE Žó|“Á•Ê‹æ•ª   TO ì‚P|‚—îŽÒ‹æ•ª
016370         WHEN 4
016371             MOVE 6              TO ì‚P|‚—îŽÒ‹æ•ª
016373         END-EVALUATE
016374     END-IF
016375*Š³ŽÒ‡‚i–¢Žg—p€–Új
      */20180611
016376     MOVE ZERO                   TO ì‚P|Š³ŽÒ”Ô†.
016376     MOVE Žó|Š³ŽÒ”Ô†           TO ì‚P|Š³ŽÒ”Ô†.
016377*****
016378*
016380* ‚P•”ˆÊ–Ú
016390     MOVE •‰Ží•Ê‚v(1)          TO •‰Ží•Ê•ÏŠ·‘O‚v.
016400     PERFORM •‰Ží•Ê•ÏŠ·.
016410     MOVE •‰Ží•Ê•ÏŠ·Œã‚v       TO ì‚P|•‰‹æ•ª(1).
016421     IF •‰Ží•Ê•ÏŠ·Œã‚v = 9
016422         MOVE "–³"             TO “ú–{Œê•ÏŠ·‚v‚w
016423     ELSE
016424         MOVE •‰–¼‚v(1)        TO “ú–{Œê•ÏŠ·‚v‚m
016425     END-IF.
016426     MOVE “ú–{Œê•ÏŠ·‚v‚w         TO ì‚P|•‰–¼(1).
016440*
016450     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016460     MOVE •‰”NŒŽ“ú‚v(1)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016470     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
016480     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|•‰”NŒŽ“ú(1).
016490*
016500     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016510     MOVE ‰ŒŸ”NŒŽ“ú‚v(1)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016520     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
016530     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|‰ŒŸ”NŒŽ“ú(1).
016540*
016550     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016560     MOVE ŠJŽn”NŒŽ“ú‚v(1)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016570     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
016580     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|Ž{pŠJŽn”NŒŽ“ú(1).
016590*
016600     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016610     MOVE I—¹”NŒŽ“ú‚v(1)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016620     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
016630     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|Ž{pI—¹”NŒŽ“ú(1).
016640*
016650     MOVE ŽÀ“ú”‚v(1)            TO ì‚P|ŽÀ“ú”(1).
016660*
016670     MOVE “]‹A‹æ•ª‚v(1)          TO “]‹A•ÏŠ·‘O‚v.
016680     PERFORM “]‹A‹æ•ª•ÏŠ·.
016690     MOVE “]‹A•ÏŠ·Œã‚v           TO ì‚P|“]‹A‹æ•ª(1).
016700*
016701     MOVE ‰‰ñˆ’u‰ñ”‚v(1)      TO ì‚P|®•œŒÅ’èŽ{—Ã‰ñ”(1).
016828     MOVE ‰‰ñˆ’u—¿‚v‚q(1)      TO ì‚P|®•œŒÅ’èŽ{—Ã—¿(1).
016829*
016830* ‚Q•”ˆÊ–Ú
016840     MOVE •‰Ží•Ê‚v(2)          TO •‰Ží•Ê•ÏŠ·‘O‚v.
016850     PERFORM •‰Ží•Ê•ÏŠ·.
016860     MOVE •‰Ží•Ê•ÏŠ·Œã‚v       TO ì‚P|•‰‹æ•ª(2).
016871     IF •‰Ží•Ê•ÏŠ·Œã‚v = 9
016872         MOVE "–³"             TO “ú–{Œê•ÏŠ·‚v‚w
016873     ELSE
016874         MOVE •‰–¼‚v(2)        TO “ú–{Œê•ÏŠ·‚v‚m
016875     END-IF.
016876     MOVE “ú–{Œê•ÏŠ·‚v‚w         TO ì‚P|•‰–¼(2).
016890*
016900     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016910     MOVE •‰”NŒŽ“ú‚v(2)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016920     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
016930     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|•‰”NŒŽ“ú(2).
016940*
016950     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016960     MOVE ‰ŒŸ”NŒŽ“ú‚v(2)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
016970     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
016980     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|‰ŒŸ”NŒŽ“ú(2).
016990*
017000     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017010     MOVE ŠJŽn”NŒŽ“ú‚v(2)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017020     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
017030     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|Ž{pŠJŽn”NŒŽ“ú(2).
017040*
017050     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017060     MOVE I—¹”NŒŽ“ú‚v(2)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017070     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
017080     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|Ž{pI—¹”NŒŽ“ú(2).
017090*
017100     MOVE ŽÀ“ú”‚v(2)            TO ì‚P|ŽÀ“ú”(2).
017110*
017120     MOVE “]‹A‹æ•ª‚v(2)          TO “]‹A•ÏŠ·‘O‚v.
017130     PERFORM “]‹A‹æ•ª•ÏŠ·.
017140     MOVE “]‹A•ÏŠ·Œã‚v           TO ì‚P|“]‹A‹æ•ª(2).
017150*
017270     MOVE ‰‰ñˆ’u‰ñ”‚v(2)      TO ì‚P|®•œŒÅ’èŽ{—Ã‰ñ”(2).
017271     MOVE ‰‰ñˆ’u—¿‚v‚q(2)      TO ì‚P|®•œŒÅ’èŽ{—Ã—¿(2).
017272*
017280* ‚R•”ˆÊ–Ú
017290     MOVE •‰Ží•Ê‚v(3)          TO •‰Ží•Ê•ÏŠ·‘O‚v.
017300     PERFORM •‰Ží•Ê•ÏŠ·.
017310     MOVE •‰Ží•Ê•ÏŠ·Œã‚v       TO ì‚P|•‰‹æ•ª(3).
017321     IF •‰Ží•Ê•ÏŠ·Œã‚v = 9
017322         MOVE "–³"             TO “ú–{Œê•ÏŠ·‚v‚w
017323     ELSE
017324         MOVE •‰–¼‚v(3)        TO “ú–{Œê•ÏŠ·‚v‚m
017325     END-IF.
017326     MOVE “ú–{Œê•ÏŠ·‚v‚w         TO ì‚P|•‰–¼(3).
017340*
017350     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017360     MOVE •‰”NŒŽ“ú‚v(3)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017370     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
017380     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|•‰”NŒŽ“ú(3).
017390*
017400     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017410     MOVE ‰ŒŸ”NŒŽ“ú‚v(3)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017420     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
017430     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|‰ŒŸ”NŒŽ“ú(3).
017440*
017450     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017460     MOVE ŠJŽn”NŒŽ“ú‚v(3)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017470     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
017480     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|Ž{pŠJŽn”NŒŽ“ú(3).
017490*
017500     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017510     MOVE I—¹”NŒŽ“ú‚v(3)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017520     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
017530     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|Ž{pI—¹”NŒŽ“ú(3).
017540*
017550     MOVE ŽÀ“ú”‚v(3)            TO ì‚P|ŽÀ“ú”(3).
017560*
017570     MOVE “]‹A‹æ•ª‚v(3)          TO “]‹A•ÏŠ·‘O‚v.
017580     PERFORM “]‹A‹æ•ª•ÏŠ·.
017590     MOVE “]‹A•ÏŠ·Œã‚v           TO ì‚P|“]‹A‹æ•ª(3).
017600*
017720     MOVE ‰‰ñˆ’u‰ñ”‚v(3)      TO ì‚P|®•œŒÅ’èŽ{—Ã‰ñ”(3).
017721     MOVE ‰‰ñˆ’u—¿‚v‚q(3)      TO ì‚P|®•œŒÅ’èŽ{—Ã—¿(3).
017722*
017730* ‚S•”ˆÊ–Ú
017740     MOVE •‰Ží•Ê‚v(4)          TO •‰Ží•Ê•ÏŠ·‘O‚v.
017750     PERFORM •‰Ží•Ê•ÏŠ·.
017760     MOVE •‰Ží•Ê•ÏŠ·Œã‚v       TO ì‚P|•‰‹æ•ª(4).
017771     IF •‰Ží•Ê•ÏŠ·Œã‚v = 9
017772         MOVE "–³"             TO “ú–{Œê•ÏŠ·‚v‚w
017773     ELSE
017774         MOVE •‰–¼‚v(4)        TO “ú–{Œê•ÏŠ·‚v‚m
017775     END-IF.
017776     MOVE “ú–{Œê•ÏŠ·‚v‚w         TO ì‚P|•‰–¼(4).
017790*
017800     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017810     MOVE •‰”NŒŽ“ú‚v(4)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017820     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
017830     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|•‰”NŒŽ“ú(4).
017840*
017850     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017860     MOVE ‰ŒŸ”NŒŽ“ú‚v(4)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017870     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
017880     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|‰ŒŸ”NŒŽ“ú(4).
017890*
017900     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017910     MOVE ŠJŽn”NŒŽ“ú‚v(4)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017920     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
017930     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|Ž{pŠJŽn”NŒŽ“ú(4).
017940*
017950     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017960     MOVE I—¹”NŒŽ“ú‚v(4)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
017970     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
017980     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|Ž{pI—¹”NŒŽ“ú(4).
017990*
018000     MOVE ŽÀ“ú”‚v(4)            TO ì‚P|ŽÀ“ú”(4).
018010*
018020     MOVE “]‹A‹æ•ª‚v(4)          TO “]‹A•ÏŠ·‘O‚v.
018030     PERFORM “]‹A‹æ•ª•ÏŠ·.
018040     MOVE “]‹A•ÏŠ·Œã‚v           TO ì‚P|“]‹A‹æ•ª(4).
018050*
018170     MOVE ‰‰ñˆ’u‰ñ”‚v(4)      TO ì‚P|®•œŒÅ’èŽ{—Ã‰ñ”(4).
018171     MOVE ‰‰ñˆ’u—¿‚v‚q(4)      TO ì‚P|®•œŒÅ’èŽ{—Ã—¿(4).
018172*
018180* ‚T•”ˆÊ–Ú
018190     MOVE •‰Ží•Ê‚v(5)          TO •‰Ží•Ê•ÏŠ·‘O‚v.
018200     PERFORM •‰Ží•Ê•ÏŠ·.
018210     MOVE •‰Ží•Ê•ÏŠ·Œã‚v       TO ì‚P|•‰‹æ•ª(5).
018221     IF •‰Ží•Ê•ÏŠ·Œã‚v = 9
018222         MOVE "–³"             TO “ú–{Œê•ÏŠ·‚v‚w
018223     ELSE
018224         MOVE •‰–¼‚v(5)        TO “ú–{Œê•ÏŠ·‚v‚m
018225     END-IF.
018226     MOVE “ú–{Œê•ÏŠ·‚v‚w         TO ì‚P|•‰–¼(5).
018240*
018250     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
018260     MOVE •‰”NŒŽ“ú‚v(5)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
018270     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
018280     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|•‰”NŒŽ“ú(5).
018290*
018300     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
018310     MOVE ‰ŒŸ”NŒŽ“ú‚v(5)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
018320     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
018330     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|‰ŒŸ”NŒŽ“ú(5).
018340*
018350     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
018360     MOVE ŠJŽn”NŒŽ“ú‚v(5)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
018370     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
018380     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|Ž{pŠJŽn”NŒŽ“ú(5).
018390*
018400     MOVE ZERO                   TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
018410     MOVE I—¹”NŒŽ“ú‚v(5)        TO ŒvŽZ˜a—ï”NŒŽ“ú‚v.
018420     PERFORM ¼—ï”NŒŽ“úŽæ“¾.
018430     MOVE ŒvŽZ¼—ï”NŒŽ“ú‚v       TO ì‚P|Ž{pI—¹”NŒŽ“ú(5).
018440*
018450     MOVE ŽÀ“ú”‚v(5)            TO ì‚P|ŽÀ“ú”(5).
018460*
018470     MOVE “]‹A‹æ•ª‚v(5)          TO “]‹A•ÏŠ·‘O‚v.
018480     PERFORM “]‹A‹æ•ª•ÏŠ·.
018490     MOVE “]‹A•ÏŠ·Œã‚v           TO ì‚P|“]‹A‹æ•ª(5).
018620*
019080     MOVE ‰‰ñˆ’u‰ñ”‚v(5)      TO ì‚P|®•œŒÅ’èŽ{—Ã‰ñ”(5).
019081     MOVE ‰‰ñˆ’u—¿‚v‚q(5)      TO ì‚P|®•œŒÅ’èŽ{—Ã—¿(5).
019082**************************************************************
019090*
019100     MOVE •”ˆÊ”‚v               TO ì‚P|•”ˆÊ”.
019110*
019130     MOVE V‹K‹æ•ª‚v             TO ì‚P|V‹K‹æ•ª.
019140     MOVE Œp‘±‹æ•ª‚v             TO ì‚P|Œp‘±‹æ•ª.
019150*
019160     MOVE ‰ŒŸ‰ñ”‚v             TO ì‚P|‰ŒŸ‰ñ”.
019170     MOVE ‰ŒŸ—¿‚v               TO ì‚P|‰ŒŸ—¿.
019171     MOVE ‰ŒŸŽžŠÔŠO‰ñ”‚v       TO ì‚P|‰ŒŸŽžŠÔŠO‰ÁŽZ‰ñ”.
019180     MOVE ‰ŒŸ‹x“ú‰ñ”‚v         TO ì‚P|‰ŒŸ‹x“ú‰ÁŽZ‰ñ”.
019190     MOVE ‰ŒŸ[–é‰ñ”‚v         TO ì‚P|‰ŒŸ[–é‰ÁŽZ‰ñ”.
019200     MOVE ‰ŒŸ‰ÁŽZ—¿‚v           TO ì‚P|‰ŒŸ‰ÁŽZ.
019202     MOVE ‘Š’kŽx‰‡‰ñ”‚v         TO ì‚P|‘Š’kŽx‰‡‰ñ”.
019203     MOVE ‰ŒŸŽž‘Š’k—¿‚v         TO ì‚P|‘Š’kŽx‰‡—¿.
019204
019205     MOVE ÄŒŸ‰ñ”‚v             TO ì‚P|ÄŒŸ‰ñ”.
019210     MOVE ÄŒŸ—¿‚v‚q             TO ì‚P|ÄŒŸ—¿.
019211     MOVE ‰—Ã‹——£‚Q‚v           TO ì‚P|‰—Ã‹——£.
019220     MOVE ‰—Ã‰ñ”‚v             TO ì‚P|‰—Ã‰ñ”.
019230     MOVE ‰—Ã—¿‚v               TO ì‚P|‰—Ã—¿.
019231*
019240     MOVE ‰—Ã–éŠÔ‚v             TO ì‚P|–éŠÔ‰ÁŽZ‰—Ã‰ñ”.
019250     MOVE ‰—Ã–\•—‚v             TO ì‚P|–\•—‰Já‰ÁŽZ‰—Ã‰ñ”.
019260     MOVE ‰—Ã“ï˜H‚v             TO ì‚P|“ï˜H‰ÁŽZ‰—Ã‰ñ”.
019270     MOVE ‰—Ã‰ÁŽZ—¿‚v           TO ì‚P|‰—Ã‰ÁŽZ.
019271*
      */‹à‘®•›Žq•ÏX«««/20180611
           MOVE ‹à‘®•›Žq‰ñ”‚v         TO ì‚P|‹à‘®•›Žq‰ñ”.
019280*     MOVE ‘å‰ñ”‚v               TO ì‚P|‹à‘®•›Žq‘å‰ñ”.
019290*     MOVE ’†‰ñ”‚v               TO ì‚P|‹à‘®•›Žq’†‰ñ”.
019300*     MOVE ¬‰ñ”‚v               TO ì‚P|‹à‘®•›Žq¬‰ñ”.
      */‹à‘®•›Žq•ÏXªªª/20180611
      */‰^“®Œã—Ã’Ç‰Á/20180611
           MOVE ‰^“®Œã—Ã—¿‰ñ”‚v    TO ì‚P|‰^“®Œã—Ã—¿‰ñ”.
           MOVE ‰^“®Œã—Ã—¿‚v        TO ì‚P|‰^“®Œã—Ã—¿    .
      *
019310     MOVE ‹à‘®•›Žq‰ÁŽZ—¿‚v       TO ì‚P|‹à‘®•›Žq‰ÁŽZ.
019311*
019320     MOVE î•ñ’ñ‹Ÿ—¿‰ñ”‚v       TO ì‚P|î•ñ’ñ‹Ÿ—¿‰ñ”.
019330     MOVE î•ñ’ñ‹Ÿ—¿‚v           TO ì‚P|î•ñ’ñ‹Ÿ—¿.
019331*
      */–¾×‘”­s‘Ì§‰ÁŽZ’Ç‰Á/20221012
           MOVE –¾×‘”­sŒŽ“ú‚v       TO ì‚P|–¾×‘”­sŒŽ“ú.
           MOVE –¾×‘”­s‰ñ”‚v       TO ì‚P|–¾×‘”­s‰ñ”.
           MOVE –¾×‘”­s‚v           TO ì‚P|–¾×‘”­s.
      *
019361** / •”ˆÊ•Ê’üŒ¸—¦•Êƒf[ƒ^ / **
019362*    •”ˆÊ‚P
019363     MOVE ZERO                   TO ì‚P|’üŒ¸ŠJŽnŒŽ“ú(1).
019364     MOVE Œã—Ã‰ñ”‚P‚v‚q         TO ì‚P|Œã—Ã‰ñ”(1).
019365     MOVE Œã—Ã—¿‚P‚v‚q           TO ì‚P|Œã—Ã—¿(1).
019366     MOVE —âãª–@‰ñ”‚P‚v‚q       TO ì‚P|—âãª–@‰ñ”(1).
019367     MOVE —âãª–@—¿‚P‚v‚q         TO ì‚P|—âãª–@—¿(1).
019368     MOVE ‰·ãª–@‰ñ”‚P‚v‚q       TO ì‚P|‰·ãª–@‰ñ”(1).
019369     MOVE ‰·ãª–@—¿‚P‚v‚q         TO ì‚P|‰·ãª–@—¿(1).
019370     MOVE “d—Ã‰ñ”‚P‚v‚q         TO ì‚P|“d—Ã‰ñ”(1).
019371     MOVE “d—Ã—¿‚P‚v‚q           TO ì‚P|“d—Ã—¿(1).
019372     MOVE ZERO                   TO ì‚P|‘½•”ˆÊ’üŒ¸—¦(1).
019373     MOVE ZERO                   TO ì‚P|‘½•”ˆÊ’üŒ¸Šz(1).
019374     MOVE ’·Šú’üŒ¸—¦‚P‚v‚q       TO ì‚P|’·Šú’üŒ¸—¦(1).
019375     MOVE ’·Šúž¬Œv‚P‚v‚q       TO ì‚P|—¿‹àŒv(1).
019376*    •”ˆÊ‚Q
019377     MOVE ZERO                   TO ì‚P|’üŒ¸ŠJŽnŒŽ“ú(2).
019378     MOVE Œã—Ã‰ñ”‚Q‚v‚q         TO ì‚P|Œã—Ã‰ñ”(2).
019379     MOVE Œã—Ã—¿‚Q‚v‚q           TO ì‚P|Œã—Ã—¿(2).
019380     MOVE —âãª–@‰ñ”‚Q‚v‚q       TO ì‚P|—âãª–@‰ñ”(2).
019381     MOVE —âãª–@—¿‚Q‚v‚q         TO ì‚P|—âãª–@—¿(2).
019382     MOVE ‰·ãª–@‰ñ”‚Q‚v‚q       TO ì‚P|‰·ãª–@‰ñ”(2).
019383     MOVE ‰·ãª–@—¿‚Q‚v‚q         TO ì‚P|‰·ãª–@—¿(2).
019384     MOVE “d—Ã‰ñ”‚Q‚v‚q         TO ì‚P|“d—Ã‰ñ”(2).
019385     MOVE “d—Ã—¿‚Q‚v‚q           TO ì‚P|“d—Ã—¿(2).
019386     MOVE ZERO                   TO ì‚P|‘½•”ˆÊ’üŒ¸—¦(2).
019387     MOVE ZERO                   TO ì‚P|‘½•”ˆÊ’üŒ¸Šz(2).
019388     MOVE ’·Šú’üŒ¸—¦‚Q‚v‚q       TO ì‚P|’·Šú’üŒ¸—¦(2).
019389     MOVE ’·Šúž¬Œv‚Q‚v‚q       TO ì‚P|—¿‹àŒv(2).
019390*    •”ˆÊ‚R‚Ìã’ii’üŒ¸‚V‚Oj
019391     MOVE ZERO                   TO ì‚P|’üŒ¸ŠJŽnŒŽ“ú(3).
019392     MOVE Œã—Ã‰ñ”‚R‚W‚v‚q       TO ì‚P|Œã—Ã‰ñ”(3).
019393     MOVE Œã—Ã—¿‚R‚W‚v‚q         TO ì‚P|Œã—Ã—¿(3).
019394     MOVE —âãª–@‰ñ”‚R‚W‚v‚q     TO ì‚P|—âãª–@‰ñ”(3).
019395     MOVE —âãª–@—¿‚R‚W‚v‚q       TO ì‚P|—âãª–@—¿(3).
019396     MOVE ‰·ãª–@‰ñ”‚R‚W‚v‚q     TO ì‚P|‰·ãª–@‰ñ”(3).
019397     MOVE ‰·ãª–@—¿‚R‚W‚v‚q       TO ì‚P|‰·ãª–@—¿(3).
019398     MOVE “d—Ã‰ñ”‚R‚W‚v‚q       TO ì‚P|“d—Ã‰ñ”(3).
019399     MOVE “d—Ã—¿‚R‚W‚v‚q         TO ì‚P|“d—Ã—¿(3).
019400     MOVE ‚R•”ˆÊ–Ú’üŒ¸—¦‚v       TO ì‚P|‘½•”ˆÊ’üŒ¸—¦(3).
019401     MOVE ‘½•”ˆÊž¬Œv‚R‚W‚v‚q   TO ì‚P|‘½•”ˆÊ’üŒ¸Šz(3).
019402     MOVE ’·Šú’üŒ¸—¦‚R‚W‚v‚q     TO ì‚P|’·Šú’üŒ¸—¦(3).
019403     MOVE ’·Šúž¬Œv‚R‚W‚v‚q     TO ì‚P|—¿‹àŒv(3).
019404*    •”ˆÊ‚R‚Ì‰º’ii’üŒ¸‚P‚O‚Oj
019405     MOVE ’üŒ¸ŠJŽnŒŽ“ú‚R‚O‚v‚q   TO ì‚P|’üŒ¸ŠJŽnŒŽ“ú(4).
019406     MOVE Œã—Ã‰ñ”‚R‚O‚v‚q       TO ì‚P|Œã—Ã‰ñ”(4).
019407     MOVE Œã—Ã—¿‚R‚O‚v‚q         TO ì‚P|Œã—Ã—¿(4).
019408     MOVE —âãª–@‰ñ”‚R‚O‚v‚q     TO ì‚P|—âãª–@‰ñ”(4).
019409     MOVE —âãª–@—¿‚R‚O‚v‚q       TO ì‚P|—âãª–@—¿(4).
019410     MOVE ‰·ãª–@‰ñ”‚R‚O‚v‚q     TO ì‚P|‰·ãª–@‰ñ”(4).
019411     MOVE ‰·ãª–@—¿‚R‚O‚v‚q       TO ì‚P|‰·ãª–@—¿(4).
019412     MOVE “d—Ã‰ñ”‚R‚O‚v‚q       TO ì‚P|“d—Ã‰ñ”(4).
019413     MOVE “d—Ã—¿‚R‚O‚v‚q         TO ì‚P|“d—Ã—¿(4).
019414     MOVE ‚Q•”ˆÊ–Ú’üŒ¸—¦‚v       TO ì‚P|‘½•”ˆÊ’üŒ¸—¦(4).
019415     MOVE ¬Œv‚R‚O‚v‚q           TO ì‚P|‘½•”ˆÊ’üŒ¸Šz(4).
019416     MOVE ’·Šú’üŒ¸—¦‚R‚O‚v‚q     TO ì‚P|’·Šú’üŒ¸—¦(4).
019417     MOVE ’·Šúž¬Œv‚R‚O‚v‚q     TO ì‚P|—¿‹àŒv(4).
019418*    •”ˆÊ‚S‚Ìã’ii’üŒ¸‚V‚Oj
019419     MOVE ’üŒ¸ŠJŽnŒŽ“ú‚S‚W‚v‚q   TO ì‚P|’üŒ¸ŠJŽnŒŽ“ú(5).
019420     MOVE Œã—Ã‰ñ”‚S‚W‚v‚q       TO ì‚P|Œã—Ã‰ñ”(5).
019421     MOVE Œã—Ã—¿‚S‚W‚v‚q         TO ì‚P|Œã—Ã—¿(5).
019422     MOVE —âãª–@‰ñ”‚S‚W‚v‚q     TO ì‚P|—âãª–@‰ñ”(5).
019423     MOVE —âãª–@—¿‚S‚W‚v‚q       TO ì‚P|—âãª–@—¿(5).
019424     MOVE ‰·ãª–@‰ñ”‚S‚W‚v‚q     TO ì‚P|‰·ãª–@‰ñ”(5).
019425     MOVE ‰·ãª–@—¿‚S‚W‚v‚q       TO ì‚P|‰·ãª–@—¿(5).
019426     MOVE “d—Ã‰ñ”‚S‚W‚v‚q       TO ì‚P|“d—Ã‰ñ”(5).
019427     MOVE “d—Ã—¿‚S‚W‚v‚q         TO ì‚P|“d—Ã—¿(5).
019428     MOVE ‚R•”ˆÊ–Ú’üŒ¸—¦‚v       TO ì‚P|‘½•”ˆÊ’üŒ¸—¦(5).
019429     MOVE ‘½•”ˆÊž¬Œv‚S‚W‚v‚q   TO ì‚P|‘½•”ˆÊ’üŒ¸Šz(5).
019430     MOVE ’·Šú’üŒ¸—¦‚S‚W‚v‚q     TO ì‚P|’·Šú’üŒ¸—¦(5).
019431     MOVE ’·Šúž¬Œv‚S‚W‚v‚q     TO ì‚P|—¿‹àŒv(5).
019432*    •”ˆÊ‚S‚Ì‰º’ii’üŒ¸‚P‚O‚Oj
019433     MOVE ’üŒ¸ŠJŽnŒŽ“ú‚S‚O‚v‚q   TO ì‚P|’üŒ¸ŠJŽnŒŽ“ú(6).
019434     MOVE Œã—Ã‰ñ”‚S‚O‚v‚q       TO ì‚P|Œã—Ã‰ñ”(6).
019435     MOVE Œã—Ã—¿‚S‚O‚v‚q         TO ì‚P|Œã—Ã—¿(6).
019436     MOVE —âãª–@‰ñ”‚S‚O‚v‚q     TO ì‚P|—âãª–@‰ñ”(6).
019437     MOVE —âãª–@—¿‚S‚O‚v‚q       TO ì‚P|—âãª–@—¿(6).
019438     MOVE ‰·ãª–@‰ñ”‚S‚O‚v‚q     TO ì‚P|‰·ãª–@‰ñ”(6).
019439     MOVE ‰·ãª–@—¿‚S‚O‚v‚q       TO ì‚P|‰·ãª–@—¿(6).
019440     MOVE “d—Ã‰ñ”‚S‚O‚v‚q       TO ì‚P|“d—Ã‰ñ”(6).
019441     MOVE “d—Ã—¿‚S‚O‚v‚q         TO ì‚P|“d—Ã—¿(6).
019442     MOVE ‚Q•”ˆÊ–Ú’üŒ¸—¦‚v       TO ì‚P|‘½•”ˆÊ’üŒ¸—¦(6).
019443     MOVE ¬Œv‚S‚O‚v‚q           TO ì‚P|‘½•”ˆÊ’üŒ¸Šz(6).
019444     MOVE ’·Šú’üŒ¸—¦‚S‚O‚v‚q     TO ì‚P|’·Šú’üŒ¸—¦(6).
019445     MOVE ’·Šúž¬Œv‚S‚O‚v‚q     TO ì‚P|—¿‹àŒv(6).
019446**
019447** / •‰Œ´ˆöE’·Šú——R / **
019448*
019449     IF •‰Œ´ˆöˆóü‹æ•ª‚v  NOT = 1 
019450*      / •‰Œ´ˆö—pWORKƒNƒŠƒA[ /
019451         INITIALIZE •‰Œ´ˆö‚v‚s
019452         INITIALIZE •‰Š³ŽÒ”Ô†‚b‚v
019453         INITIALIZE •‰˜A”Ô‚b‚v
019454         INITIALIZE •‰Œ´ˆö‚s‚a‚k
019455         INITIALIZE •‰Œ´ˆö“à—e‚v
019456     END-IF.
019460*-----------------------------------------------*
019463     IF ( •‰Œ´ˆöˆóü‹æ•ª‚v  NOT = 1 ) AND ( ƒŒƒZ•‰Œ´ˆöˆóü‹æ•ª‚v NOT = 1 )
019464        IF ( •‰Œ´ˆöˆóü‹æ•ª‚v = 3 OR 4)
019465           PERFORM •‰Œ´ˆöˆóü‘ÎÛ”»’èˆ—
019466        ELSE
019467           PERFORM •‰Œ´ˆöŽæ“¾
019468        END-IF
019469     END-IF.
019472           PERFORM •‰Œ´ˆöŽæ“¾
019473*-----------------------------------------------*
019474*
019475     IF ’·Šú——Rˆóü‹æ•ª‚v  NOT = 1 
019480        PERFORM ’·Šú——R•¶Žæ“¾
019490     ELSE
019500        MOVE  SPACE TO  ˜A’·•¶|ƒL[
019510        INITIALIZE      ˜A’·•¶|ƒL[
019520     END-IF.
019530*
019541     INSPECT •‰Œ´ˆö‚v‚s REPLACING ALL ‘SŠp‹ó”’ BY ”¼Šp‹ó”’.
019542     INSPECT •‰Œ´ˆö‚v‚s REPLACING ALL ‰üs     BY ”¼Šp‹ó”’.
019543     MOVE SPACE TO •¶Žš‚P‚v •¶Žš‚Q‚v.
019544     MOVE •‰Œ´ˆö“à—e‡¬‚v(1) TO •¶Žš‚P‚v.
019545     MOVE •‰Œ´ˆö“à—e‡¬‚v(2) TO •¶Žš‚Q‚v.
019546     CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
019547          USING BY REFERENCE •¶Žš‚P‚v
019548          BY REFERENCE •¶Žš‚Q‚v.
019549     MOVE •‰Œ´ˆö“à—e‡¬‚v(3) TO •¶Žš‚Q‚v.
019550     CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
019551          USING BY REFERENCE •¶Žš‚P‚v
019552          BY REFERENCE •¶Žš‚Q‚v.
019553     MOVE •‰Œ´ˆö“à—e‡¬‚v(4) TO •¶Žš‚Q‚v.
019554     CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
019555          USING BY REFERENCE •¶Žš‚P‚v
019556          BY REFERENCE •¶Žš‚Q‚v.
019557     MOVE •‰Œ´ˆö“à—e‡¬‚v(5) TO •¶Žš‚Q‚v.
019558     CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
019559          USING BY REFERENCE •¶Žš‚P‚v
019560          BY REFERENCE •¶Žš‚Q‚v.
019564     MOVE •¶Žš‚P‚v TO ì‚P|•‰Œ´ˆö.
019565*
019566     INSPECT •‰Œ´ˆö‚v‚s REPLACING ALL ‘SŠp‹ó”’ BY ”¼Šp‹ó”’.
019567     INSPECT •‰Œ´ˆö‚v‚s REPLACING ALL ‰üs     BY ”¼Šp‹ó”’.
019568     MOVE SPACE TO •¶Žš‚P‚v •¶Žš‚Q‚v.
019569     MOVE •‰Œ´ˆö“à—e‡¬‚v(1) TO •¶Žš‚P‚v.
019570     MOVE •‰Œ´ˆö“à—e‡¬‚v(2) TO •¶Žš‚Q‚v.
019571     CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
019572          USING BY REFERENCE •¶Žš‚P‚v
019573          BY REFERENCE •¶Žš‚Q‚v.
019574     MOVE •‰Œ´ˆö“à—e‡¬‚v(3) TO •¶Žš‚Q‚v.
019575     CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
019576          USING BY REFERENCE •¶Žš‚P‚v
019577          BY REFERENCE •¶Žš‚Q‚v.
019578     MOVE •‰Œ´ˆö“à—e‡¬‚v(4) TO •¶Žš‚Q‚v.
019579     CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
019580          USING BY REFERENCE •¶Žš‚P‚v
019581          BY REFERENCE •¶Žš‚Q‚v.
019582     MOVE •‰Œ´ˆö“à—e‡¬‚v(5) TO •¶Žš‚Q‚v.
019583     CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
019584          USING BY REFERENCE •¶Žš‚P‚v
019585          BY REFERENCE •¶Žš‚Q‚v.
019586     MOVE •¶Žš‚P‚v TO ì‚P|•‰Œ´ˆö.
019587
019590     MOVE SPACE TO •¶Žš‚P‚v •¶Žš‚Q‚v.
019591     MOVE ˜A’·•¶|——R•¶(1) TO “ú–{Œê•ÏŠ·‚v‚m.
019592     MOVE “ú–{Œê•ÏŠ·‚v‚w    TO •¶Žš‚P‚v.
019593     INSPECT •¶Žš‚P‚v REPLACING ALL ‘SŠp‹ó”’ BY ”¼Šp‹ó”’.
019594     INSPECT •¶Žš‚P‚v REPLACING ALL ‰üs     BY ”¼Šp‹ó”’.
019595     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 2 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 15
019596         MOVE ˜A’·•¶|——R•¶(ƒJƒEƒ“ƒ^) TO “ú–{Œê•ÏŠ·‚v‚m
019597         MOVE “ú–{Œê•ÏŠ·‚v‚w           TO •¶Žš‚Q‚v
019598         INSPECT •¶Žš‚Q‚v REPLACING ALL ‘SŠp‹ó”’ BY "  "
019599         INSPECT •¶Žš‚Q‚v REPLACING ALL ‰üs     BY "  "
019600         CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
019601              USING BY REFERENCE •¶Žš‚P‚v
019602              BY REFERENCE •¶Žš‚Q‚v
019611     END-PERFORM.
019612     MOVE •¶Žš‚P‚v TO ì‚P|’·Šú——R.
019614*
      */20180611
019615*     MOVE 05                  TO ì‚P|‹ÆŽÒ‹æ•ª.
019615*     MOVE 12                  TO ì‚P|‹ÆŽÒ‹æ•ª.
      */20211027
019615     MOVE 08                  TO ì‚P|‹ÆŽÒ‹æ•ª.
019617     MOVE Ž{p“ú‚s‚v          TO ì‚P|Ž{p“ú.
019618     INSPECT •‰Œo‰ß‚v REPLACING ALL ‘SŠp‹ó”’ BY ”¼Šp‹ó”’.
019619     MOVE •‰Œo‰ß‚v          TO ì‚P|Œo‰ß.
019620     MOVE ‰üs                TO ì‚P|‰üs•¶Žš.
019621*================================================================*
019622 ì‚Pƒtƒ@ƒCƒ‹‘ž SECTION.
019623*
019630     WRITE ì‚P|ƒŒƒR[ƒh
019640     INVALID KEY
019650         MOVE NC"ì‚P"  TO ƒtƒ@ƒCƒ‹–¼
019660         PERFORM ƒGƒ‰[•\Ž¦
019670     END-WRITE.
019680*================================================================*
019690*================================================================*
019700 ‰ïˆõ”Ô†‰E‹l‚ß SECTION.
019710*
019720     MOVE ‰ïˆõ”Ô†‚v      TO  ‰ïˆõ”Ô†¶‹l‚ß‚v.
019730     MOVE ZERO            TO  ‰ïˆõ”Ô†‰E‹l‚ß‚v.
019740     MOVE ZERO            TO  ‰ïˆõ”Ô†”Žš‚v.
019750*
019760     MOVE  8  TO  ƒJƒEƒ“ƒ^.
019770*
019780     IF  ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(7) NOT = SPACE
019790         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
019800         MOVE ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(7)  TO  ‰ïˆõ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
019810     END-IF.
019820     IF  ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(6) NOT = SPACE
019830         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
019840         MOVE ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(6)  TO  ‰ïˆõ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
019850     END-IF.
019860     IF  ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(5) NOT = SPACE
019870         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
019880         MOVE ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(5)  TO  ‰ïˆõ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
019890     END-IF.
019900     IF  ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(4) NOT = SPACE
019910         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
019920         MOVE ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(4)  TO  ‰ïˆõ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
019930     END-IF.
019940     IF  ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(3) NOT = SPACE
019950         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
019960         MOVE ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(3)  TO  ‰ïˆõ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
019970     END-IF.
019980     IF  ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(2) NOT = SPACE
019990         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
020000         MOVE ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(2)  TO  ‰ïˆõ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
020010     END-IF.
020020     IF  ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(1) NOT = SPACE
020030         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
020040         MOVE ‰ïˆõ”Ô†¶‹l‚ß‚v‚P(1)  TO  ‰ïˆõ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
020050     END-IF.
020060*
020070     MOVE ‰ïˆõ”Ô†‰E‹l‚ß‚v TO ‰ïˆõ”Ô†”Žš‚v.
020080*
020090*================================================================*
020100 •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß SECTION.
020110*
020121     MOVE •ÛŒ¯ŽÒ”Ô†‚v    TO  •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v.
020130     MOVE ZERO            TO  •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß‚v.
020140     MOVE ZERO            TO  •ÛŒ¯ŽÒ”Ô†”Žš‚v.
020150*
020160     MOVE  9  TO  ƒJƒEƒ“ƒ^.
020170*
020180     IF  •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(8) NOT = SPACE
020190         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
020200         MOVE •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(8)  TO  •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
020210     END-IF.
020220     IF  •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(7) NOT = SPACE
020230         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
020240         MOVE •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(7)  TO  •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
020250     END-IF.
020260     IF  •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(6) NOT = SPACE
020270         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
020280         MOVE •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(6)  TO  •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
020290     END-IF.
020300     IF  •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(5) NOT = SPACE
020310         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
020320         MOVE •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(5)  TO  •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
020330     END-IF.
020340     IF  •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(4) NOT = SPACE
020350         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
020360         MOVE •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(4)  TO  •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
020370     END-IF.
020380     IF  •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(3) NOT = SPACE
020390         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
020400         MOVE •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(3)  TO  •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
020410     END-IF.
020420     IF  •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(2) NOT = SPACE
020430         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
020440         MOVE •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(2)  TO  •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
020450     END-IF.
020460     IF  •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(1) NOT = SPACE
020470         COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  -  1
020480         MOVE •ÛŒ¯ŽÒ”Ô†¶‹l‚ß‚v‚P(1)  TO  •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^)
020490     END-IF.
020500*
020510     MOVE •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß‚v TO •ÛŒ¯ŽÒ”Ô†”Žš‚v.
020521*
020530*================================================================*
020540*================================================================*
020550 ‹L†¶‹l‚ß SECTION.
020560*
020570***** ‹L†‚Ì–³‘Ê‚ÈSPACE‚ðŽæ‚èœ‚¢‚ÄA¶‹l‚ß‚É‚·‚éB
020580     MOVE SPACE           TO  ‹L†‚m‚v.
020590     MOVE SPACE           TO  ‹L†Œ³‚v.
020600     MOVE SPACE           TO  ‹L†¶‹l‚ß‚v.
020610*     MOVE Žó|‹L†        TO  ‹L†Œ³‚v.
020620*-----------------------------------------------------------------*
020630     MOVE SPACE TO ˜AˆÃ†•¡‡|ˆÃ†î•ñ.
020640*
020650*    / ˜AˆÃ†•¡‡|“ü—Íî•ñƒZƒbƒg /
020660     MOVE Žó|‹L†       TO ˜AˆÃ†•¡‡|‹L†.
020670     MOVE Žó|”Ô†       TO ˜AˆÃ†•¡‡|”Ô†.
020680     MOVE Žó|ˆÃ†‰»€–Ú TO ˜AˆÃ†•¡‡|ˆÃ†‰»€–Ú.
020690*
020700     CALL   •¡‡ƒvƒƒOƒ‰ƒ€–¼‚v.
020710     CANCEL •¡‡ƒvƒƒOƒ‰ƒ€–¼‚v.
020720*
020730     MOVE ˜AˆÃ†•¡‡|•¡‡‚µ‚½‹L† TO ‹L†Œ³‚v.
020740*
020750*-----------------------------------------------------------------*
020760*
020770     MOVE  ZERO  TO  ƒJƒEƒ“ƒ^‚Q.
020780     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 12
020790          IF  ‹L†Œ³‚v‚P(ƒJƒEƒ“ƒ^) NOT = SPACE
020800              COMPUTE ƒJƒEƒ“ƒ^‚Q = ƒJƒEƒ“ƒ^‚Q  +  1
020810              MOVE ‹L†Œ³‚v‚P(ƒJƒEƒ“ƒ^)  TO  ‹L†¶‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^‚Q)
020820          END-IF
020830     END-PERFORM.
020840*
020850     MOVE SPACE           TO  ‹L†‚o‚m‚v.
020860     MOVE SPACE           TO  ‹L†Œ³‚w‚v.
020870     MOVE SPACE           TO  ‹L†¶‹l‚ß‚w‚v.
020880     MOVE ‹L†¶‹l‚ß‚v    TO  ‹L†Œ³‚w‚v.
020890*
020900     MOVE  ZERO  TO  ƒJƒEƒ“ƒ^‚Q.
020910     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 24
020920          IF  ‹L†Œ³‚w‚v‚P(ƒJƒEƒ“ƒ^) NOT = SPACE
020930              COMPUTE ƒJƒEƒ“ƒ^‚Q = ƒJƒEƒ“ƒ^‚Q  +  1
020940              MOVE ‹L†Œ³‚w‚v‚P(ƒJƒEƒ“ƒ^)  TO  ‹L†¶‹l‚ß‚w‚v‚P(ƒJƒEƒ“ƒ^‚Q)
020950          END-IF
020960     END-PERFORM.
020970*
020980     MOVE ‹L†¶‹l‚ß‚w‚v    TO ‹L†‚o‚m‚v.
020990*
021000*”¼ŠpƒXƒy[ƒX‚ð‘SŠp‚É‚©‚¦‚é
021010*    INSPECT ‹L†‚v REPLACING ALL ”¼Šp‹ó”’ BY ‘SŠp‹ó”’.
021020*
021030*================================================================*
021040*================================================================*
021050 —¿‹àî•ñŽæ“¾ SECTION.
021060*
021070***********************************************
021080* —¿‹àƒf[ƒ^ƒZƒbƒg                            *
021090***********************************************
021100*    ****************************************************************
021110*    * —¿‹àiŒŽ–ˆji•‰–ˆji’üŒ¸–ˆj‚É‚Â‚¢‚Ä‚Í˜AŒ‹€–Ú‚æ‚èƒZƒbƒg *
021120*    ****************************************************************
021130     INITIALIZE —¿‹à‚P‚v‚q.
021140     INITIALIZE —¿‹à‚Q‚v‚q.
021150     INITIALIZE —¿‹à‚R‚v‚q.
021160*
021170     PERFORM ‘½•”ˆÊ’üŒ¸—¦Žæ“¾.
021171*
021172     IF ƒŒƒZ|ƒŒƒZŽí•Ê = 1 OR 2
021173         MOVE 1                        TO ˆã—Ã•¬‹æ•ª‚v
021174     ELSE
021175         MOVE 3                        TO ˆã—Ã•¬‹æ•ª‚v
021176     END-IF.
021177*
021185     MOVE ƒŒƒZ|ƒŒƒZŽÀ“ú”             TO ‘S‘ÌŽÀ“ú”‚v.
021186     IF ƒŒƒZ|¿‹‹æ•ª = ZERO OR 1
021187         MOVE ZERO                     TO Ä¿‹‹æ•ª‚v
021188     ELSE
021189         MOVE 1                        TO Ä¿‹‹æ•ª‚v
021190     END-IF.
021191*
021192     MOVE ƒŒƒZ|‰—Ã‰ñ”               TO  ‰—Ã‰ñ”‚v.
021193     MOVE ƒŒƒZ|‰—Ã‹——£               TO  ‰—Ã‹——£‚v.
021194* ’PˆÊ100m
021200     COMPUTE  ‰—Ã‹——£‚Q‚v  =  ‰—Ã‹——£‚v * 10.
021210*
021211     MOVE ƒŒƒZ|‰‰ñˆ’u—¿(1)          TO ‰‰ñˆ’u—¿‚v‚q(1).
021212     MOVE ƒŒƒZ|‰‰ñˆ’u—¿(2)          TO ‰‰ñˆ’u—¿‚v‚q(2).
021213     MOVE ƒŒƒZ|‰‰ñˆ’u—¿(3)          TO ‰‰ñˆ’u—¿‚v‚q(3).
021214     MOVE ƒŒƒZ|‰‰ñˆ’u—¿(4)          TO ‰‰ñˆ’u—¿‚v‚q(4).
021215     MOVE ƒŒƒZ|‰‰ñˆ’u—¿(5)          TO ‰‰ñˆ’u—¿‚v‚q(5).
021216*
021217     MOVE ƒŒƒZ|‰ŒŸ—¿                 TO ‰ŒŸ—¿‚v  .
021218     MOVE ƒŒƒZ|‰ŒŸ‰ÁŽZ—¿             TO ‰ŒŸ‰ÁŽZ—¿‚v  .
021219     MOVE ƒŒƒZ|‰ŒŸŽž‘Š’k—¿           TO ‰ŒŸŽž‘Š’k—¿‚v  .
021220     MOVE ƒŒƒZ|ÄŒŸ—¿                 TO ÄŒŸ—¿‚v‚q.
021221     MOVE ƒŒƒZ|‰—Ã—¿                 TO ‰—Ã—¿‚v  .
021222     MOVE ƒŒƒZ|‰—Ã‰ÁŽZ—¿             TO ‰—Ã‰ÁŽZ—¿‚v  .
021223     MOVE ƒŒƒZ|‹à‘®•›Žq‰ÁŽZ—¿         TO ‹à‘®•›Žq‰ÁŽZ—¿‚v  .
021224     MOVE ƒŒƒZ|Ž{pî•ñ’ñ‹Ÿ—¿         TO î•ñ’ñ‹Ÿ—¿‚v      .
      */–¾×‘”­s‘Ì§‰ÁŽZ’Ç‰Á/20221012
           MOVE ƒŒƒZ|–¾×‘”­s‰ÁŽZ—¿       TO –¾×‘”­s‚v.
           MOVE ƒŒƒZ|–¾×‘”­s‰ÁŽZ“ú       TO –¾×‘”­s“ú‚v.
           IF ƒŒƒZ|–¾×‘”­s‰ÁŽZ—¿ NOT = ZERO
               MOVE 1                        TO –¾×‘”­s‰ñ”‚v
               MOVE ƒŒƒZ|Ž{pŒŽ             TO –¾×‘”­sŒŽ‚v
           END-IF.
021227*
021228********************
021230* ’üŒ¸–ˆ—¿‹àƒZƒbƒg *
021240********************
021250*    **********
021260*    * ‚P•”ˆÊ *
021270*    **********
021280     MOVE ƒŒƒZ|Œã—Ã‰ñ”‚P             TO Œã—Ã‰ñ”‚P‚v‚q.
021290     MOVE ƒŒƒZ|—âãª–@‰ñ”‚P           TO —âãª–@‰ñ”‚P‚v‚q.
021300     MOVE ƒŒƒZ|‰·ãª–@‰ñ”‚P           TO ‰·ãª–@‰ñ”‚P‚v‚q.
021310     MOVE ƒŒƒZ|“d—Ã‰ñ”‚P             TO “d—Ã‰ñ”‚P‚v‚q.
021384     MOVE ƒŒƒZ|Œã—Ã—¿‚P               TO Œã—Ã—¿‚P‚v‚q.
021385     MOVE ƒŒƒZ|—âãª–@—¿‚P             TO —âãª–@—¿‚P‚v‚q.
021386     MOVE ƒŒƒZ|‰·ãª–@—¿‚P             TO ‰·ãª–@—¿‚P‚v‚q.
021387     MOVE ƒŒƒZ|“d—Ã—¿‚P               TO “d—Ã—¿‚P‚v‚q.
021389     MOVE ƒŒƒZ|¬Œv‚P                 TO ¬Œv‚P‚v‚q.
021390     MOVE ƒŒƒZ|’·Šú’üŒ¸—¦‚P           TO ’·Šú’üŒ¸—¦‚P‚v‚q.
021392     MOVE ƒŒƒZ|’·Šúž¬Œv‚P           TO ’·Šúž¬Œv‚P‚v‚q.
021393*    **********
021394*    * ‚Q•”ˆÊ *
021400*    **********
021410     MOVE ƒŒƒZ|Œã—Ã‰ñ”‚Q             TO Œã—Ã‰ñ”‚Q‚v‚q.
021420     MOVE ƒŒƒZ|—âãª–@‰ñ”‚Q           TO —âãª–@‰ñ”‚Q‚v‚q.
021430     MOVE ƒŒƒZ|‰·ãª–@‰ñ”‚Q           TO ‰·ãª–@‰ñ”‚Q‚v‚q.
021440     MOVE ƒŒƒZ|“d—Ã‰ñ”‚Q             TO “d—Ã‰ñ”‚Q‚v‚q.
021511     MOVE ƒŒƒZ|Œã—Ã—¿‚Q               TO Œã—Ã—¿‚Q‚v‚q.
021512     MOVE ƒŒƒZ|—âãª–@—¿‚Q             TO —âãª–@—¿‚Q‚v‚q.
021513     MOVE ƒŒƒZ|‰·ãª–@—¿‚Q             TO ‰·ãª–@—¿‚Q‚v‚q.
021514     MOVE ƒŒƒZ|“d—Ã—¿‚Q               TO “d—Ã—¿‚Q‚v‚q.
021516     MOVE ƒŒƒZ|¬Œv‚Q                 TO ¬Œv‚Q‚v‚q.
021517     MOVE ƒŒƒZ|’·Šú’üŒ¸—¦‚Q           TO ’·Šú’üŒ¸—¦‚Q‚v‚q.
021519     MOVE ƒŒƒZ|’·Šúž¬Œv‚Q           TO ’·Šúž¬Œv‚Q‚v‚q.
021520*    ****************
021521*    * ‚R•”ˆÊ^‚WŠ„ *
021530*    ****************
021560     MOVE ƒŒƒZ|Œã—Ã‰ñ”‚R‚W             TO Œã—Ã‰ñ”‚R‚W‚v‚q.
021570     MOVE ƒŒƒZ|—âãª–@‰ñ”‚R‚W           TO —âãª–@‰ñ”‚R‚W‚v‚q.
021580     MOVE ƒŒƒZ|‰·ãª–@‰ñ”‚R‚W           TO ‰·ãª–@‰ñ”‚R‚W‚v‚q.
021590     MOVE ƒŒƒZ|“d—Ã‰ñ”‚R‚W             TO “d—Ã‰ñ”‚R‚W‚v‚q.
021661     MOVE ƒŒƒZ|Œã—Ã—¿‚R‚W               TO Œã—Ã—¿‚R‚W‚v‚q.
021662     MOVE ƒŒƒZ|—âãª–@—¿‚R‚W             TO —âãª–@—¿‚R‚W‚v‚q.
021663     MOVE ƒŒƒZ|‰·ãª–@—¿‚R‚W             TO ‰·ãª–@—¿‚R‚W‚v‚q.
021664     MOVE ƒŒƒZ|“d—Ã—¿‚R‚W               TO “d—Ã—¿‚R‚W‚v‚q.
021666     MOVE ƒŒƒZ|¬Œv‚R‚W                 TO ¬Œv‚R‚W‚v‚q.
021667     MOVE ƒŒƒZ|‘½•”ˆÊž¬Œv‚R‚W         TO ‘½•”ˆÊž¬Œv‚R‚W‚v‚q.
021668     MOVE ƒŒƒZ|’·Šú’üŒ¸—¦‚R‚W           TO ’·Šú’üŒ¸—¦‚R‚W‚v‚q.
021669     MOVE ƒŒƒZ|’·Šúž¬Œv‚R‚W           TO ’·Šúž¬Œv‚R‚W‚v‚q.
021670*    ****************
021671*    * ‚R•”ˆÊ^10Š„ *
021680*    ****************
021690     MOVE ƒŒƒZ|’üŒ¸ŠJŽnŒŽ“ú‚R‚O         TO ’üŒ¸ŠJŽnŒŽ“ú‚R‚O‚v‚q
021691     MOVE ƒŒƒZ|Œã—Ã‰ñ”‚R‚O             TO Œã—Ã‰ñ”‚R‚O‚v‚q.
021700     MOVE ƒŒƒZ|—âãª–@‰ñ”‚R‚O           TO —âãª–@‰ñ”‚R‚O‚v‚q.
021710     MOVE ƒŒƒZ|‰·ãª–@‰ñ”‚R‚O           TO ‰·ãª–@‰ñ”‚R‚O‚v‚q.
021720     MOVE ƒŒƒZ|“d—Ã‰ñ”‚R‚O             TO “d—Ã‰ñ”‚R‚O‚v‚q.
021761     MOVE ƒŒƒZ|Œã—Ã—¿‚R‚O               TO Œã—Ã—¿‚R‚O‚v‚q.
021762     MOVE ƒŒƒZ|—âãª–@—¿‚R‚O             TO —âãª–@—¿‚R‚O‚v‚q.
021763     MOVE ƒŒƒZ|‰·ãª–@—¿‚R‚O             TO ‰·ãª–@—¿‚R‚O‚v‚q.
021764     MOVE ƒŒƒZ|“d—Ã—¿‚R‚O               TO “d—Ã—¿‚R‚O‚v‚q.
021766     MOVE ƒŒƒZ|¬Œv‚R‚O                 TO ¬Œv‚R‚O‚v‚q.
021768     MOVE ƒŒƒZ|’·Šú’üŒ¸—¦‚R‚O           TO ’·Šú’üŒ¸—¦‚R‚O‚v‚q.
021769     MOVE ƒŒƒZ|’·Šúž¬Œv‚R‚O           TO ’·Šúž¬Œv‚R‚O‚v‚q.
021834*    ****************
021840*    * ‚S•”ˆÊ^‚TŠ„ *
021850*    ****************
021880     MOVE ƒŒƒZ|Œã—Ã‰ñ”‚S‚T             TO Œã—Ã‰ñ”‚S‚T‚v‚q.
021890     MOVE ƒŒƒZ|—âãª–@‰ñ”‚S‚T           TO —âãª–@‰ñ”‚S‚T‚v‚q.
021900     MOVE ƒŒƒZ|‰·ãª–@‰ñ”‚S‚T           TO ‰·ãª–@‰ñ”‚S‚T‚v‚q.
021910     MOVE ƒŒƒZ|“d—Ã‰ñ”‚S‚T             TO “d—Ã‰ñ”‚S‚T‚v‚q.
021981     MOVE ƒŒƒZ|Œã—Ã—¿‚S‚T               TO Œã—Ã—¿‚S‚T‚v‚q.
021982     MOVE ƒŒƒZ|—âãª–@—¿‚S‚T             TO —âãª–@—¿‚S‚T‚v‚q.
021983     MOVE ƒŒƒZ|‰·ãª–@—¿‚S‚T             TO ‰·ãª–@—¿‚S‚T‚v‚q.
021984     MOVE ƒŒƒZ|“d—Ã—¿‚S‚T               TO “d—Ã—¿‚S‚T‚v‚q.
021986     MOVE ƒŒƒZ|¬Œv‚S‚T                 TO ¬Œv‚S‚T‚v‚q.
021987     MOVE ƒŒƒZ|‘½•”ˆÊž¬Œv‚S‚T         TO ‘½•”ˆÊž¬Œv‚S‚T‚v‚q.
021988     MOVE ƒŒƒZ|’·Šú’üŒ¸—¦‚S‚T           TO ’·Šú’üŒ¸—¦‚S‚T‚v‚q.
021989     MOVE ƒŒƒZ|’·Šúž¬Œv‚S‚T           TO ’·Šúž¬Œv‚S‚T‚v‚q.
021990*    ****************
021991*    * ‚S•”ˆÊ^‚WŠ„ *
022000*    ****************
022010     MOVE ƒŒƒZ|’üŒ¸ŠJŽnŒŽ“ú‚S‚W         TO ’üŒ¸ŠJŽnŒŽ“ú‚S‚W‚v‚q
022011     MOVE ƒŒƒZ|Œã—Ã‰ñ”‚S‚W             TO Œã—Ã‰ñ”‚S‚W‚v‚q.
022020     MOVE ƒŒƒZ|—âãª–@‰ñ”‚S‚W           TO —âãª–@‰ñ”‚S‚W‚v‚q.
022030     MOVE ƒŒƒZ|‰·ãª–@‰ñ”‚S‚W           TO ‰·ãª–@‰ñ”‚S‚W‚v‚q.
022040     MOVE ƒŒƒZ|“d—Ã‰ñ”‚S‚W             TO “d—Ã‰ñ”‚S‚W‚v‚q.
022111     MOVE ƒŒƒZ|Œã—Ã—¿‚S‚W               TO Œã—Ã—¿‚S‚W‚v‚q.
022112     MOVE ƒŒƒZ|—âãª–@—¿‚S‚W             TO —âãª–@—¿‚S‚W‚v‚q.
022113     MOVE ƒŒƒZ|‰·ãª–@—¿‚S‚W             TO ‰·ãª–@—¿‚S‚W‚v‚q.
022114     MOVE ƒŒƒZ|“d—Ã—¿‚S‚W               TO “d—Ã—¿‚S‚W‚v‚q.
022116     MOVE ƒŒƒZ|¬Œv‚S‚W                 TO ¬Œv‚S‚W‚v‚q.
022118     MOVE ƒŒƒZ|‘½•”ˆÊž¬Œv‚S‚W         TO ‘½•”ˆÊž¬Œv‚S‚W‚v‚q.
022119     MOVE ƒŒƒZ|’·Šú’üŒ¸—¦‚S‚W           TO ’·Šú’üŒ¸—¦‚S‚W‚v‚q.
022120     MOVE ƒŒƒZ|’·Šúž¬Œv‚S‚W           TO ’·Šúž¬Œv‚S‚W‚v‚q.
022121*    ****************
022122*    * ‚S•”ˆÊ^10Š„ *
022130*    ****************
022140     MOVE ƒŒƒZ|’üŒ¸ŠJŽnŒŽ“ú‚S‚O         TO ’üŒ¸ŠJŽnŒŽ“ú‚S‚O‚v‚q
022141     MOVE ƒŒƒZ|Œã—Ã‰ñ”‚S‚O             TO Œã—Ã‰ñ”‚S‚O‚v‚q.
022150     MOVE ƒŒƒZ|—âãª–@‰ñ”‚S‚O           TO —âãª–@‰ñ”‚S‚O‚v‚q.
022160     MOVE ƒŒƒZ|‰·ãª–@‰ñ”‚S‚O           TO ‰·ãª–@‰ñ”‚S‚O‚v‚q.
022170     MOVE ƒŒƒZ|“d—Ã‰ñ”‚S‚O             TO “d—Ã‰ñ”‚S‚O‚v‚q.
022211     MOVE ƒŒƒZ|Œã—Ã—¿‚S‚O               TO Œã—Ã—¿‚S‚O‚v‚q.
022212     MOVE ƒŒƒZ|—âãª–@—¿‚S‚O             TO —âãª–@—¿‚S‚O‚v‚q.
022213     MOVE ƒŒƒZ|‰·ãª–@—¿‚S‚O             TO ‰·ãª–@—¿‚S‚O‚v‚q.
022214     MOVE ƒŒƒZ|“d—Ã—¿‚S‚O               TO “d—Ã—¿‚S‚O‚v‚q.
022216     MOVE ƒŒƒZ|¬Œv‚S‚O                 TO ¬Œv‚S‚O‚v‚q.
022218     MOVE ƒŒƒZ|’·Šú’üŒ¸—¦‚S‚O           TO ’·Šú’üŒ¸—¦‚S‚O‚v‚q.
022219     MOVE ƒŒƒZ|’·Šúž¬Œv‚S‚O           TO ’·Šúž¬Œv‚S‚O‚v‚q.
022294*    *****************
022295*    * ‚T•”ˆÊ^2.5Š„ *
022300*    *****************
022330     MOVE ƒŒƒZ|Œã—Ã‰ñ”‚T‚Q             TO Œã—Ã‰ñ”‚T‚Q‚v‚q.
022340     MOVE ƒŒƒZ|—âãª–@‰ñ”‚T‚Q           TO —âãª–@‰ñ”‚T‚Q‚v‚q.
022350     MOVE ƒŒƒZ|‰·ãª–@‰ñ”‚T‚Q           TO ‰·ãª–@‰ñ”‚T‚Q‚v‚q.
022360     MOVE ƒŒƒZ|“d—Ã‰ñ”‚T‚Q             TO “d—Ã‰ñ”‚T‚Q‚v‚q.
022441     MOVE ƒŒƒZ|Œã—Ã—¿‚T‚Q               TO Œã—Ã—¿‚T‚Q‚v‚q.
022442     MOVE ƒŒƒZ|—âãª–@—¿‚T‚Q             TO —âãª–@—¿‚T‚Q‚v‚q.
022443     MOVE ƒŒƒZ|‰·ãª–@—¿‚T‚Q             TO ‰·ãª–@—¿‚T‚Q‚v‚q.
022444     MOVE ƒŒƒZ|“d—Ã—¿‚T‚Q               TO “d—Ã—¿‚T‚Q‚v‚q.
022446     MOVE ƒŒƒZ|¬Œv‚T‚Q                 TO ¬Œv‚T‚Q‚v‚q.
022448     MOVE ƒŒƒZ|‘½•”ˆÊž¬Œv‚T‚Q         TO ‘½•”ˆÊž¬Œv‚T‚Q‚v‚q.
022449     MOVE ƒŒƒZ|’·Šú’üŒ¸—¦‚T‚Q           TO ’·Šú’üŒ¸—¦‚T‚Q‚v‚q.
022450     MOVE ƒŒƒZ|’·Šúž¬Œv‚T‚Q           TO ’·Šúž¬Œv‚T‚Q‚v‚q.
022451*    ****************
022452*    * ‚T•”ˆÊ^‚TŠ„ *
022453*    ****************
022460     MOVE ƒŒƒZ|’üŒ¸ŠJŽnŒŽ“ú‚T‚T         TO ’üŒ¸ŠJŽnŒŽ“ú‚T‚T‚v‚q
022461     MOVE ƒŒƒZ|Œã—Ã‰ñ”‚T‚T             TO Œã—Ã‰ñ”‚T‚T‚v‚q.
022470     MOVE ƒŒƒZ|—âãª–@‰ñ”‚T‚T           TO —âãª–@‰ñ”‚T‚T‚v‚q.
022480     MOVE ƒŒƒZ|‰·ãª–@‰ñ”‚T‚T           TO ‰·ãª–@‰ñ”‚T‚T‚v‚q.
022490     MOVE ƒŒƒZ|“d—Ã‰ñ”‚T‚T             TO “d—Ã‰ñ”‚T‚T‚v‚q.
022561     MOVE ƒŒƒZ|Œã—Ã—¿‚T‚T               TO Œã—Ã—¿‚T‚T‚v‚q.
022562     MOVE ƒŒƒZ|—âãª–@—¿‚T‚T             TO —âãª–@—¿‚T‚T‚v‚q.
022563     MOVE ƒŒƒZ|‰·ãª–@—¿‚T‚T             TO ‰·ãª–@—¿‚T‚T‚v‚q.
022564     MOVE ƒŒƒZ|“d—Ã—¿‚T‚T               TO “d—Ã—¿‚T‚T‚v‚q.
022566     MOVE ƒŒƒZ|¬Œv‚T‚T                 TO ¬Œv‚T‚T‚v‚q.
022568     MOVE ƒŒƒZ|‘½•”ˆÊž¬Œv‚T‚T         TO ‘½•”ˆÊž¬Œv‚T‚T‚v‚q.
022569     MOVE ƒŒƒZ|’·Šú’üŒ¸—¦‚T‚T           TO ’·Šú’üŒ¸—¦‚T‚T‚v‚q.
022570     MOVE ƒŒƒZ|’·Šúž¬Œv‚T‚T           TO ’·Šúž¬Œv‚T‚T‚v‚q.
022571*    ****************
022572*    * ‚T•”ˆÊ^‚WŠ„ *
022580*    ****************
022590     MOVE ƒŒƒZ|’üŒ¸ŠJŽnŒŽ“ú‚T‚W         TO ’üŒ¸ŠJŽnŒŽ“ú‚T‚W‚v‚q
022591     MOVE ƒŒƒZ|Œã—Ã‰ñ”‚T‚W             TO Œã—Ã‰ñ”‚T‚W‚v‚q.
022600     MOVE ƒŒƒZ|—âãª–@‰ñ”‚T‚W           TO —âãª–@‰ñ”‚T‚W‚v‚q.
022610     MOVE ƒŒƒZ|‰·ãª–@‰ñ”‚T‚W           TO ‰·ãª–@‰ñ”‚T‚W‚v‚q.
022620     MOVE ƒŒƒZ|“d—Ã‰ñ”‚T‚W             TO “d—Ã‰ñ”‚T‚W‚v‚q.
022691     MOVE ƒŒƒZ|Œã—Ã—¿‚T‚W               TO Œã—Ã—¿‚T‚W‚v‚q.
022692     MOVE ƒŒƒZ|—âãª–@—¿‚T‚W             TO —âãª–@—¿‚T‚W‚v‚q.
022693     MOVE ƒŒƒZ|‰·ãª–@—¿‚T‚W             TO ‰·ãª–@—¿‚T‚W‚v‚q.
022694     MOVE ƒŒƒZ|“d—Ã—¿‚T‚W               TO “d—Ã—¿‚T‚W‚v‚q.
022696     MOVE ƒŒƒZ|¬Œv‚T‚W                 TO ¬Œv‚T‚W‚v‚q.
022697     MOVE ƒŒƒZ|‘½•”ˆÊž¬Œv‚T‚W         TO ‘½•”ˆÊž¬Œv‚T‚W‚v‚q.
022698     MOVE ƒŒƒZ|’·Šú’üŒ¸—¦‚T‚W           TO ’·Šú’üŒ¸—¦‚T‚W‚v‚q.
022699     MOVE ƒŒƒZ|’·Šúž¬Œv‚T‚W           TO ’·Šúž¬Œv‚T‚W‚v‚q.
022700*    ****************
022701*    * ‚T•”ˆÊ^10Š„ *
022710*    ****************
022720     MOVE ƒŒƒZ|Œã—Ã‰ñ”‚T‚O             TO Œã—Ã‰ñ”‚T‚O‚v‚q.
022730     MOVE ƒŒƒZ|—âãª–@‰ñ”‚T‚O           TO —âãª–@‰ñ”‚T‚O‚v‚q.
022740     MOVE ƒŒƒZ|‰·ãª–@‰ñ”‚T‚O           TO ‰·ãª–@‰ñ”‚T‚O‚v‚q.
022750     MOVE ƒŒƒZ|“d—Ã‰ñ”‚T‚O             TO “d—Ã‰ñ”‚T‚O‚v‚q.
022791     MOVE ƒŒƒZ|Œã—Ã—¿‚T‚O               TO Œã—Ã—¿‚T‚O‚v‚q.
022792     MOVE ƒŒƒZ|—âãª–@—¿‚T‚O             TO —âãª–@—¿‚T‚O‚v‚q.
022793     MOVE ƒŒƒZ|‰·ãª–@—¿‚T‚O             TO ‰·ãª–@—¿‚T‚O‚v‚q.
022794     MOVE ƒŒƒZ|“d—Ã—¿‚T‚O               TO “d—Ã—¿‚T‚O‚v‚q.
022796     MOVE ƒŒƒZ|¬Œv‚T‚O                 TO ¬Œv‚T‚O‚v‚q.
022798     MOVE ƒŒƒZ|’·Šú’üŒ¸—¦‚T‚O           TO ’·Šú’üŒ¸—¦‚T‚O‚v‚q.
022799     MOVE ƒŒƒZ|’·Šúž¬Œv‚T‚O           TO ’·Šúž¬Œv‚T‚O‚v‚q.
022908*
      */‹à‘®•›Žq•ÏX/20180611
           COMPUTE ‹à‘®•›Žq‰ñ”‚v = ƒŒƒZ|‘å + ƒŒƒZ|’† + ƒŒƒZ|¬.
           IF ‹à‘®•›Žq‰ñ”‚v > 9
               MOVE 9 TO ‹à‘®•›Žq‰ñ”‚v
           END-IF.
      */‰^“®Œã—Ã’Ç‰Á/20180611
           MOVE ƒŒƒZ|‰^“®Œã—Ã‰ñ”           TO ‰^“®Œã—Ã—¿‰ñ”‚v.
           MOVE ƒŒƒZ|‰^“®Œã—Ã—¿             TO ‰^“®Œã—Ã—¿‚v.
022910*================================================================*
022920 •‰ƒf[ƒ^Žæ“¾ SECTION.
022930*
022940     INITIALIZE •‰î•ñ‚v.
022950*
022960     MOVE Ž{p˜a—ï‚v‚q       TO •‰|Ž{p˜a—ï.
022970     MOVE Ž{p”N‚v‚q         TO •‰|Ž{p”N.
022980     MOVE Ž{pŒŽ‚v‚q         TO •‰|Ž{pŒŽ.
022990     MOVE Š³ŽÒƒR[ƒh‚v‚q     TO •‰|Š³ŽÒƒR[ƒh.
023000     READ •‰ƒf[ƒ^‚e
023010     INVALID KEY
023020         CONTINUE
023030     NOT INVALID KEY
023040         MOVE •‰|•”ˆÊ”                   TO •”ˆÊ”‚v
023050         PERFORM VARYING •”ˆÊ‚b‚m‚s FROM 1 BY 1
023060                 UNTIL ( •”ˆÊ‚b‚m‚s > •”ˆÊ”‚v )
023070             MOVE •‰|•‰Ží•Ê(•”ˆÊ‚b‚m‚s) TO •‰Ží•Ê‚v(•”ˆÊ‚b‚m‚s)
023080             MOVE •‰|•”ˆÊ(•”ˆÊ‚b‚m‚s)     TO •”ˆÊ‚v(•”ˆÊ‚b‚m‚s)
023090             MOVE •‰|¶‰E‹æ•ª(•”ˆÊ‚b‚m‚s) TO ¶‰E‹æ•ª‚v(•”ˆÊ‚b‚m‚s)
023100             MOVE •‰|•‰ˆÊ’u”Ô†(•”ˆÊ‚b‚m‚s)
023110                                           TO •‰ˆÊ’u”Ô†‚v(•”ˆÊ‚b‚m‚s)
023120* •‰Ží•Ê
023130             MOVE SPACE                     TO •‰–¼Ì‚v
023140             MOVE 03                        TO –¼|‹æ•ªƒR[ƒh
023150             MOVE •‰|•‰Ží•Ê(•”ˆÊ‚b‚m‚s)  TO –¼|–¼ÌƒR[ƒh
023160             READ –¼Ìƒ}ƒXƒ^
023170             INVALID KEY
023180                 MOVE SPACE        TO “ú–{Œê•ÏŠ·‚v‚m
023190             NOT INVALID KEY
023200                 MOVE –¼|³Ž®–¼Ì TO “ú–{Œê•ÏŠ·‚v‚m
023210             END-READ
023211             MOVE “ú–{Œê•ÏŠ·‚v‚w   TO •‰–¼Ì‚v
023220* •”ˆÊ
023230             STRING ƒŒƒZ|•”ˆÊ–¼Ì‚P(•”ˆÊ‚b‚m‚s)  DELIMITED BY SPACE
023240                    •‰–¼Ì‚v                    DELIMITED BY SPACE
023250                    ƒŒƒZ|•”ˆÊ–¼Ì‚Q(•”ˆÊ‚b‚m‚s)  DELIMITED BY SPACE
023260                    INTO •‰–¼‚v(•”ˆÊ‚b‚m‚s)
023270             END-STRING
023280*
023290             MOVE •‰|•‰˜a—ï(•”ˆÊ‚b‚m‚s)   TO •‰˜a—ï‚v(•”ˆÊ‚b‚m‚s)
023300             MOVE •‰|•‰”N(•”ˆÊ‚b‚m‚s)   TO •‰”N‚v(•”ˆÊ‚b‚m‚s)
023310             MOVE •‰|•‰ŒŽ(•”ˆÊ‚b‚m‚s)   TO •‰ŒŽ‚v(•”ˆÊ‚b‚m‚s)
023320             MOVE •‰|•‰“ú(•”ˆÊ‚b‚m‚s)   TO •‰“ú‚v(•”ˆÊ‚b‚m‚s)
023330             MOVE •‰|ŠJŽn˜a—ï(•”ˆÊ‚b‚m‚s)   TO ‰ŒŸ˜a—ï‚v(•”ˆÊ‚b‚m‚s)
023340             MOVE •‰|ŠJŽn”N(•”ˆÊ‚b‚m‚s)   TO ‰ŒŸ”N‚v(•”ˆÊ‚b‚m‚s)
023350             MOVE •‰|ŠJŽnŒŽ(•”ˆÊ‚b‚m‚s)   TO ‰ŒŸŒŽ‚v(•”ˆÊ‚b‚m‚s)
023360             MOVE •‰|ŠJŽn“ú(•”ˆÊ‚b‚m‚s)   TO ‰ŒŸ“ú‚v(•”ˆÊ‚b‚m‚s)
023370             IF •‰|“]‹A‹æ•ª(•”ˆÊ‚b‚m‚s) = 9
023380                 MOVE 99                   TO I—¹”N‚v(•”ˆÊ‚b‚m‚s)
023390                 MOVE 99                   TO I—¹ŒŽ‚v(•”ˆÊ‚b‚m‚s)
023400                 MOVE 99                   TO I—¹“ú‚v(•”ˆÊ‚b‚m‚s)
023410             ELSE
023420                 MOVE •‰|I—¹˜a—ï(•”ˆÊ‚b‚m‚s)   TO I—¹˜a—ï‚v(•”ˆÊ‚b‚m‚s)
023430                 MOVE •‰|I—¹”N(•”ˆÊ‚b‚m‚s)   TO I—¹”N‚v(•”ˆÊ‚b‚m‚s)
023440                 MOVE •‰|I—¹ŒŽ(•”ˆÊ‚b‚m‚s)   TO I—¹ŒŽ‚v(•”ˆÊ‚b‚m‚s)
023450                 MOVE •‰|I—¹“ú(•”ˆÊ‚b‚m‚s)   TO I—¹“ú‚v(•”ˆÊ‚b‚m‚s)
023460             END-IF
023470*
023480             MOVE •‰|“]‹A‹æ•ª(•”ˆÊ‚b‚m‚s) TO “]‹A‹æ•ª‚v(•”ˆÊ‚b‚m‚s)
023490*
023500* Œo‰ß—ªÌŽæ“¾
023501             MOVE 01                         TO Œo|‹æ•ªƒR[ƒh
023502             MOVE •‰|Œo‰ßƒR[ƒh(•”ˆÊ‚b‚m‚s) TO Œo|Œo‰ßƒR[ƒh
023503             READ Œo‰ßƒ}ƒXƒ^
023504             INVALID KEY
023507                 MOVE SPACE           TO •‰Œo‰ß•”ˆÊ‚v(•”ˆÊ‚b‚m‚s)
023508             NOT INVALID KEY
023509                 EVALUATE •”ˆÊ‚b‚m‚s
023510                 WHEN 1
023511                     MOVE NC"‡@" TO Œo‰ß•”ˆÊ‚v
023512                 WHEN 2
023513                     MOVE NC"‡A" TO Œo‰ß•”ˆÊ‚v
023514                 WHEN 3
023515                     MOVE NC"‡B" TO Œo‰ß•”ˆÊ‚v
023516                 WHEN 4
023517                     MOVE NC"‡C" TO Œo‰ß•”ˆÊ‚v
023518                 WHEN 5
023519                     MOVE NC"‡D" TO Œo‰ß•”ˆÊ‚v
023520                 END-EVALUATE
023521                 STRING  Œo‰ß•”ˆÊ‚v     DELIMITED BY SPACE
023522                         Œo|Œo‰ß—ªÌ   DELIMITED BY SPACE
023523                        INTO “ú–{Œê•ÏŠ·‚v‚m
023524                 END-STRING
023525                 MOVE “ú–{Œê•ÏŠ·‚v‚w TO •‰Œo‰ß•”ˆÊ‚v(•”ˆÊ‚b‚m‚s)
023532             END-READ
023533         END-PERFORM
023534
023535* V‹K/Œp‘± ƒ`ƒFƒbƒN
023536         EVALUATE ƒŒƒZ|ƒŒƒZ¿‹‹æ•ª
023537         WHEN 1
023538             MOVE 1                   TO V‹K‹æ•ª‚v
023539         WHEN 2
023540             MOVE 1                   TO Œp‘±‹æ•ª‚v
023541         WHEN 3
023542             MOVE 1                   TO V‹K‹æ•ª‚v
023543             MOVE 1                   TO Œp‘±‹æ•ª‚v
023544         WHEN OTHER
023545             MOVE 1                   TO Œp‘±‹æ•ª‚v
023546         END-EVALUATE
023570
023571         PERFORM ‰ŒŸ“úˆÈ‘O‚Ìƒf[ƒ^”»’è
023580* Ž}”Ô”»’è—p
023590         MOVE •‰|ŠJŽnf—Ã“úŽè“®‹æ•ª TO  ŠJŽnf—Ã“úŽè“®‹æ•ª‚v
023600*
023601* •‰Œ´ˆöˆóü‹æ•ª
023602         MOVE •‰|ƒŒƒZ•‰Œ´ˆöˆóü‹æ•ª TO ƒŒƒZ•‰Œ´ˆöˆóü‹æ•ª‚v
023603         MOVE •‰|ƒŒƒZ’·Šú——Rˆóü‹æ•ª TO ƒŒƒZ’·Šú——Rˆóü‹æ•ª‚v
023604*
023610     END-READ.
023620*
023630*================================================================*
023640 ‰ŒŸ“úˆÈ‘O‚Ìƒf[ƒ^”»’è SECTION.
023650*
023660*********************************************************************************
023670*  Å‰‚Ì‰ŒŸ“úˆÈ‘O‚Ì“–ŒŽ’†‚ÉŽ{p‹L˜^ƒŒƒR[ƒh‚ª‚ ‚Á‚½Žž(Ž¡–üA’†Ž~)‚ÍA¿‹‹æ•ª‚Ì
023680*  Œp‘±‚É‚àƒ`ƒFƒbƒN‚·‚éB(V‹K‚ÆŒp‘±‚Ì—¼•û)
023690*********************************************************************************
023700** Å‰‚Ì‰ŒŸ“ú‚ðŽæ“¾
023710     MOVE SPACE                 TO ‰ŒŸƒtƒ‰ƒO.
023720     MOVE Š³ŽÒ”Ô†‚v‚q          TO Ž{‹L|Š³ŽÒ”Ô†.
023730     MOVE Ž}”Ô‚v‚q              TO Ž{‹L|Ž}”Ô.
023740     MOVE Ž{p˜a—ï‚v‚q          TO Ž{‹L|Ž{p˜a—ï.
023750     MOVE Ž{p”N‚v‚q            TO Ž{‹L|Ž{p”N.
023760     MOVE Ž{pŒŽ‚v‚q            TO Ž{‹L|Ž{pŒŽ.
023770     MOVE ZERO                  TO Ž{‹L|Ž{p“ú.
023780     START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
023790                                  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
023800     END-START.
023810     IF ó‘ÔƒL[ = "00"
023820         MOVE ZERO  TO ‰ŒŸ˜a—ï‚v‚s
023830         MOVE ZERO  TO ‰ŒŸ”N‚v‚s
023840         MOVE ZERO  TO ‰ŒŸŒŽ‚v‚s
023850         MOVE ZERO  TO ‰ŒŸ“ú‚v‚s
023860         MOVE SPACE TO I—¹ƒtƒ‰ƒO‚Q
023870         PERFORM Ž{p‹L˜^‚e“Çž
023880         PERFORM UNTIL ( I—¹ƒtƒ‰ƒO‚Q         = "YES"           ) OR
023890                       ( Ž{‹L|Š³ŽÒƒR[ƒh NOT = Š³ŽÒƒR[ƒh‚v‚q  ) OR
023900                       ( Ž{‹L|Ž{p˜a—ï   NOT = Ž{p˜a—ï‚v‚q    ) OR
023910                       ( Ž{‹L|Ž{p”N     NOT = Ž{p”N‚v‚q      ) OR
023920                       ( Ž{‹L|Ž{pŒŽ     NOT = Ž{pŒŽ‚v‚q      ) OR
023930                       ( ‰ŒŸƒtƒ‰ƒO           = "YES"           ) 
023940               IF  Ž{‹L|f—Ã‹æ•ª = 2
023950                   MOVE Ž{‹L|Ž{p˜a—ï           TO ‰ŒŸ˜a—ï‚v‚s
023960                   MOVE Ž{‹L|Ž{p”N             TO ‰ŒŸ”N‚v‚s
023970                   MOVE Ž{‹L|Ž{pŒŽ             TO ‰ŒŸŒŽ‚v‚s
023980                   MOVE Ž{‹L|Ž{p“ú             TO ‰ŒŸ“ú‚v‚s
023990                   MOVE "YES"                    TO ‰ŒŸƒtƒ‰ƒO
024000               END-IF
024010               PERFORM Ž{p‹L˜^‚e“Çž
024020         END-PERFORM
024030     END-IF.
024040*
024050* ‰ŒŸ“úˆÈ‘O‚Ìƒf[ƒ^”»’è
024060     IF ‰ŒŸƒtƒ‰ƒO = "YES"
024070        MOVE Š³ŽÒ”Ô†‚v‚q          TO Ž{‹L|Š³ŽÒ”Ô†
024080        MOVE Ž}”Ô‚v‚q              TO Ž{‹L|Ž}”Ô
024090        MOVE ‰ŒŸ˜a—ï‚v‚s          TO Ž{‹L|Ž{p˜a—ï
024100        MOVE ‰ŒŸ”N‚v‚s            TO Ž{‹L|Ž{p”N
024110        MOVE ‰ŒŸŒŽ‚v‚s            TO Ž{‹L|Ž{pŒŽ
024120        MOVE ‰ŒŸ“ú‚v‚s            TO Ž{‹L|Ž{p“ú
024130        START Ž{p‹L˜^‚e   KEY IS <  Ž{‹L|Š³ŽÒƒR[ƒh
024140                                     Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
024150                                     REVERSED
024160        END-START
024170        IF ó‘ÔƒL[ = "00"
024180           MOVE SPACE  TO I—¹ƒtƒ‰ƒO‚Q
024190           PERFORM Ž{p‹L˜^‚e“Çž
024200           IF ( I—¹ƒtƒ‰ƒO‚Q    = SPACE        ) AND
024210              ( Ž{‹L|Š³ŽÒ”Ô†  = Š³ŽÒ”Ô†‚v‚q ) AND
024220              ( Ž{‹L|Ž}”Ô      = Ž}”Ô‚v‚q     ) AND
024230              ( Ž{‹L|Ž{p˜a—ï  = ‰ŒŸ˜a—ï‚v‚s ) AND
024240              ( Ž{‹L|Ž{p”N    = ‰ŒŸ”N‚v‚s   ) AND
024250              ( Ž{‹L|Ž{pŒŽ    = ‰ŒŸŒŽ‚v‚s   )
024260*  ‰ŒŸ“úˆÈ‘O‚Ì“–ŒŽ’†‚ÉŽ{p‹L˜^ƒŒƒR[ƒh‚ª‚ ‚Á‚½Žž
024270                IF Œp‘±‹æ•ª‚v = ZERO
024280                   MOVE 1    TO Œp‘±‹æ•ª‚v
024290                END-IF
024300           END-IF
024310         END-IF
024320     END-IF.
024330*
024340*================================================================*
024350 Ž{p‹L˜^Žæ“¾ SECTION.
024360*
024370     PERFORM VARYING •”ˆÊ‚b‚m‚s FROM 1 BY 1 UNTIL •”ˆÊ‚b‚m‚s > •”ˆÊ”‚v
024380         IF ( Ž{p”N‚v‚q = ‰ŒŸ”N‚v(•”ˆÊ‚b‚m‚s) ) AND
024390            ( Ž{pŒŽ‚v‚q = ‰ŒŸŒŽ‚v(•”ˆÊ‚b‚m‚s) )
024400             MOVE Š³ŽÒ”Ô†‚v‚q          TO Ž{‹L|Š³ŽÒ”Ô†
024410             MOVE Ž}”Ô‚v‚q              TO Ž{‹L|Ž}”Ô
024420             MOVE Ž{p˜a—ï‚v‚q          TO Ž{‹L|Ž{p˜a—ï
024430             MOVE ‰ŒŸ˜a—ï‚v(•”ˆÊ‚b‚m‚s)  TO ŠJŽn˜a—ï‚v(•”ˆÊ‚b‚m‚s)
024440             MOVE ‰ŒŸ”N‚v(•”ˆÊ‚b‚m‚s)  TO ŠJŽn”N‚v(•”ˆÊ‚b‚m‚s) Ž{‹L|Ž{p”N
024450             MOVE ‰ŒŸŒŽ‚v(•”ˆÊ‚b‚m‚s)  TO ŠJŽnŒŽ‚v(•”ˆÊ‚b‚m‚s) Ž{‹L|Ž{pŒŽ
024460             MOVE ‰ŒŸ“ú‚v(•”ˆÊ‚b‚m‚s)  TO ŠJŽn“ú‚v(•”ˆÊ‚b‚m‚s) Ž{‹L|Ž{p“ú
024470         ELSE
024480             MOVE Š³ŽÒ”Ô†‚v‚q          TO Ž{‹L|Š³ŽÒ”Ô†
024490             MOVE Ž}”Ô‚v‚q              TO Ž{‹L|Ž}”Ô
024500             MOVE Ž{p˜a—ï‚v‚q          TO Ž{‹L|Ž{p˜a—ï
024510             MOVE Ž{p”N‚v‚q            TO Ž{‹L|Ž{p”N
024520             MOVE Ž{pŒŽ‚v‚q            TO Ž{‹L|Ž{pŒŽ
024530             MOVE ZERO                  TO Ž{‹L|Ž{p“ú
024540         END-IF
024550         START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
024560                                      Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
024570         END-START
024580         IF ó‘ÔƒL[ = "00"
024590             MOVE ZERO  TO ŽÀ“ú”‚v(•”ˆÊ‚b‚m‚s)
024600             MOVE ZERO  TO ‰‰ñˆ’u‰ñ”‚v(•”ˆÊ‚b‚m‚s)
024610             MOVE ZERO  TO I—¹˜a—ï‚v‚s
024620             MOVE ZERO  TO I—¹”N‚v‚s
024630             MOVE ZERO  TO I—¹ŒŽ‚v‚s
024640             MOVE ZERO  TO I—¹“ú‚v‚s
024650             MOVE SPACE TO I—¹ƒtƒ‰ƒO‚Q
024660             PERFORM Ž{p‹L˜^‚e“Çž
024670             IF  ( I—¹ƒtƒ‰ƒO‚Q      = SPACE   ) AND
024680                 ( Ž{‹L|Š³ŽÒƒR[ƒh  = Š³ŽÒƒR[ƒh‚v‚q ) AND
024690                 ( Ž{‹L|Ž{p˜a—ï    = Ž{p˜a—ï‚v‚q   ) AND
024700                 ( Ž{‹L|Ž{p”N      = Ž{p”N‚v‚q     ) AND
024710                 ( Ž{‹L|Ž{pŒŽ      = Ž{pŒŽ‚v‚q     ) 
024720*
024730*        ************
024740*        * ‹à‘®•›Žq *
024750*        ************
      */‹à‘®•›Žq•ÏX«««/20180611
024760*             EVALUATE Ž{‹L|‹à‘®•›Žq‹æ•ª(•”ˆÊ‚b‚m‚s)
024770*             WHEN 1
024780*                 COMPUTE ‘å‰ñ”‚v = ‘å‰ñ”‚v + 1
024790*             WHEN 2
024800*                 COMPUTE ’†‰ñ”‚v = ’†‰ñ”‚v + 1
024810*             WHEN 3
024820*                 COMPUTE ¬‰ñ”‚v = ¬‰ñ”‚v + 1
024830*             END-EVALUATE
      */‹à‘®•›Žq•ÏXªªª/20180611
024840*        ****************
024850*        * î•ñ’ñ‹Ÿ‰ñ” *
024860*        ****************
024870             IF Ž{‹L|î•ñ’ñ‹Ÿ‹æ•ª(•”ˆÊ‚b‚m‚s) = 1
024880                 COMPUTE î•ñ’ñ‹Ÿ—¿‰ñ”‚v = î•ñ’ñ‹Ÿ—¿‰ñ”‚v + 1
024890             END-IF
024900*        *****************************************************************
024910*        * ŠJŽn”NŒŽ“ú ( ‚»‚Ì•”ˆÊ‚ª“–ŒŽ‰ŒŸ‚Å‚È‚¢‚©A
024920*                       “–ŒŽ‰ŒŸ‚Å‚àŽ}”Ô‚ª‚ ‚éŽž‚ÍAÅ‰‚ÌŽ{p“ú‚ðŠJŽn“ú)*
024930*        *****************************************************************
024940                 IF ( Ž{p”N‚v‚q NOT = ‰ŒŸ”N‚v(•”ˆÊ‚b‚m‚s) ) OR
024950                    ( Ž{pŒŽ‚v‚q NOT = ‰ŒŸŒŽ‚v(•”ˆÊ‚b‚m‚s) ) OR
024960                    ( ŠJŽnf—Ã“úŽè“®‹æ•ª‚v = 1 )
024970                     MOVE Ž{‹L|Ž{p˜a—ï TO ŠJŽn˜a—ï‚v(•”ˆÊ‚b‚m‚s)
024980                     MOVE Ž{‹L|Ž{p”N   TO ŠJŽn”N‚v(•”ˆÊ‚b‚m‚s)
024990                     MOVE Ž{‹L|Ž{pŒŽ   TO ŠJŽnŒŽ‚v(•”ˆÊ‚b‚m‚s)
025000                     MOVE Ž{‹L|Ž{p“ú   TO ŠJŽn“ú‚v(•”ˆÊ‚b‚m‚s)
025010                 END-IF
025020             END-IF
025030             PERFORM UNTIL ( I—¹ƒtƒ‰ƒO‚Q         = "YES"            ) OR
025040                           ( Ž{‹L|Š³ŽÒƒR[ƒh NOT = Š³ŽÒƒR[ƒh‚v‚q   ) OR
025050                           ( Ž{‹L|Ž{p˜a—ï   NOT = Ž{p˜a—ï‚v‚q     ) OR
025060                           ( Ž{‹L|Ž{p”N     NOT = Ž{p”N‚v‚q       ) OR
025070                           ( Ž{‹L|Ž{pŒŽ     NOT = Ž{pŒŽ‚v‚q       ) OR
025080                           ( Ž{‹L|Ž{p“ú         > I—¹“ú‚v(•”ˆÊ‚b‚m‚s))
025090*               **********
025100*               * ŽÀ“ú” *
025110*               **********
025120                COMPUTE ŽÀ“ú”‚v(•”ˆÊ‚b‚m‚s) = ŽÀ“ú”‚v(•”ˆÊ‚b‚m‚s) + 1
025130                MOVE Ž{‹L|Ž{p˜a—ï             TO I—¹˜a—ï‚v‚s
025140                MOVE Ž{‹L|Ž{p”N               TO I—¹”N‚v‚s
025150                MOVE Ž{‹L|Ž{pŒŽ               TO I—¹ŒŽ‚v‚s
025160                MOVE Ž{‹L|Ž{p“ú               TO I—¹“ú‚v‚s
025170*            /@‰‰ñˆ’u‚ÌƒJƒEƒ“ƒg@/
025180                IF Ž{‹L|®•œŽ{—Ã‹æ•ª(•”ˆÊ‚b‚m‚s) = 1
025190                    COMPUTE ‰‰ñˆ’u‰ñ”‚v(•”ˆÊ‚b‚m‚s) = ‰‰ñˆ’u‰ñ”‚v(•”ˆÊ‚b‚m‚s) + 1
025200                END-IF
025210*
025220                PERFORM Ž{p‹L˜^‚e“Çž
025230            END-PERFORM
025240        END-IF
025250*       **************************
025260*       * Œp‘±FI—¹”NŒŽ“úƒZƒbƒg *
025270*       **************************
025280        IF “]‹A‹æ•ª‚v(•”ˆÊ‚b‚m‚s) = 9
025290            MOVE I—¹˜a—ï‚v‚s  TO I—¹˜a—ï‚v(•”ˆÊ‚b‚m‚s)
025300            MOVE I—¹”N‚v‚s    TO I—¹”N‚v(•”ˆÊ‚b‚m‚s)
025310            MOVE I—¹ŒŽ‚v‚s    TO I—¹ŒŽ‚v(•”ˆÊ‚b‚m‚s)
025320            MOVE I—¹“ú‚v‚s    TO I—¹“ú‚v(•”ˆÊ‚b‚m‚s)
025330        END-IF
025340     END-PERFORM.
025350***
025360     MOVE Š³ŽÒ”Ô†‚v‚q          TO Ž{‹L|Š³ŽÒ”Ô†.
025370     MOVE Ž}”Ô‚v‚q              TO Ž{‹L|Ž}”Ô.
025380     MOVE Ž{p˜a—ï‚v‚q          TO Ž{‹L|Ž{p˜a—ï.
025390     MOVE Ž{p”N‚v‚q            TO Ž{‹L|Ž{p”N.
025400     MOVE Ž{pŒŽ‚v‚q            TO Ž{‹L|Ž{pŒŽ.
025410     MOVE ZERO                  TO Ž{‹L|Ž{p“ú.
025420     START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
025430                                  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
025440     END-START.
025450     IF ó‘ÔƒL[ = "00"
025460         MOVE SPACE TO I—¹ƒtƒ‰ƒO‚Q
025470         PERFORM Ž{p‹L˜^‚e“Çž
025480         PERFORM UNTIL ( I—¹ƒtƒ‰ƒO‚Q         = "YES"            ) OR
025490                       ( Ž{‹L|Š³ŽÒƒR[ƒh NOT = Š³ŽÒƒR[ƒh‚v‚q   ) OR
025500                       ( Ž{‹L|Ž{p˜a—ï   NOT = Ž{p˜a—ï‚v‚q     ) OR
025510                       ( Ž{‹L|Ž{p”N     NOT = Ž{p”N‚v‚q       ) OR
025520                       ( Ž{‹L|Ž{pŒŽ     NOT = Ž{pŒŽ‚v‚q       )
025530*        ************
025540*        * ‰ŒŸ‰ñ” *
025550*        ************
025560             IF Ž{‹L|‰ŒŸ—¿¿‹‹æ•ª = 1
025570                 COMPUTE ‰ŒŸ‰ñ”‚v = ‰ŒŸ‰ñ”‚v + 1
025580             END-IF
025590*        ************
025600*        * ‰ŒŸ‰ÁŽZ *
025610*        ************
025620             EVALUATE Ž{‹L|‰ŒŸ‰ÁŽZ
025630             WHEN 1
025640                 COMPUTE ‰ŒŸŽžŠÔŠO‰ñ”‚v = ‰ŒŸŽžŠÔŠO‰ñ”‚v + 1
025650             WHEN 2
025660                 COMPUTE ‰ŒŸ‹x“ú‰ñ”‚v   = ‰ŒŸ‹x“ú‰ñ”‚v + 1
025670             WHEN 3
025680                 COMPUTE ‰ŒŸ[–é‰ñ”‚v   = ‰ŒŸ[–é‰ñ”‚v + 1
025690             END-EVALUATE
025700*        ************
025710*        * ÄŒŸ‰ñ” *
025720*        ************
025730             IF Ž{‹L|ÄŒŸ—¿¿‹ = 1
025740                 COMPUTE ÄŒŸ‰ñ”‚v = ÄŒŸ‰ñ”‚v + 1
025750             END-IF
025760*        ************
025770*        * ‰—Ã‰ÁŽZ *
025780*        ************
025790             EVALUATE Ž{‹L|‰—Ã‰ÁŽZ
025800             WHEN 1
025810                 COMPUTE ‰—Ã–éŠÔ‚v = ‰—Ã–éŠÔ‚v + 1
025820             WHEN 2
025830                 COMPUTE ‰—Ã“ï˜H‚v = ‰—Ã“ï˜H‚v + 1
025840             WHEN 3
025850                 COMPUTE ‰—Ã–\•—‚v = ‰—Ã–\•—‚v + 1
025860             END-EVALUATE
025870*        ****************
025880*        * ‰ŒŸŽž‘Š’k—¿ *
025890*        ****************
025900             IF (Ž{‹L|f—Ã‹æ•ª = 2 ) AND (Ž{‹L|‰ŒŸŽž‘Š’k—¿‹æ•ª NOT = 1)
025910                 COMPUTE ‘Š’kŽx‰‡‰ñ”‚v = ‘Š’kŽx‰‡‰ñ”‚v + 1
025920             END-IF
025930*        **********
025940*        * Ž{p“ú *
025950*        **********
025960             MOVE 1 TO Ž{p“ú‚v(Ž{‹L|Ž{p“ú)
025970*
025980             PERFORM Ž{p‹L˜^‚e“Çž
025990         END-PERFORM
026000     END-IF.
026010*
026020*================================================================*
026030 ¼—ïŽ{p”NŒŽŽæ“¾ SECTION.
026040* 
026050     MOVE ZERO          TO ¼—ï”NŒŽ‚v  ¼—ïŽ{p”NŒŽ‚v.
026060     MOVE Žó|Ž{p˜a—ï  TO Œ³|Œ³†‹æ•ª.
026070     READ Œ³†ƒ}ƒXƒ^
026080     NOT INVALID KEY
026090         MOVE Œ³|ŠJŽn¼—ï”N TO ¼—ï”N‚v
026100     END-READ.
026110**
026120     IF ¼—ï”N‚v = ZERO
026130          MOVE  NC"Œ³†ƒ}ƒXƒ^‚ÉŠJŽn¼—ï”N‚ð“o˜^‚µ‚Ä‰º‚³‚¢" TO ˜Aƒ|ƒƒbƒZ[ƒW
026140          CALL   "MSG001"
026150          CANCEL "MSG001"
026160          PERFORM ƒtƒ@ƒCƒ‹•Â½
026170          MOVE 99 TO PROGRAM-STATUS
026180          EXIT PROGRAM
026190     ELSE
026200          COMPUTE ¼—ï”N‚v = ¼—ï”N‚v + Žó|Ž{p”N - 1
026210          MOVE Žó|Ž{pŒŽ TO ¼—ïŒŽ‚v
026220     END-IF.
026230*
026240     MOVE ¼—ï”NŒŽ‚v   TO  ¼—ïŽ{p”NŒŽ‚v.
026250*
026260*================================================================*
026270 ¼—ï”NŒŽ“úŽæ“¾ SECTION.
026280*
026290     MOVE ZERO  TO ŒvŽZ¼—ï”NŒŽ“ú‚v.
026300*
026310     IF ŒvŽZ˜a—ï‚v  NOT = ZERO
026320         MOVE ŒvŽZ˜a—ï‚v    TO Œ³|Œ³†‹æ•ª
026330         READ Œ³†ƒ}ƒXƒ^
026340         NOT INVALID KEY
026350             MOVE Œ³|ŠJŽn¼—ï”N TO ŒvŽZ¼—ï”N‚v
026360         END-READ
026370**
026380         IF ŒvŽZ¼—ï”N‚v = ZERO
026390              MOVE  NC"Œ³†ƒ}ƒXƒ^‚ÉŠJŽn¼—ï”N‚ð“o˜^‚µ‚Ä‰º‚³‚¢" TO ˜Aƒ|ƒƒbƒZ[ƒW
026400              CALL   "MSG001"
026410              CANCEL "MSG001"
026420              PERFORM ƒtƒ@ƒCƒ‹•Â½
026430              MOVE 99 TO PROGRAM-STATUS
026440              EXIT PROGRAM
026450         ELSE
026460              COMPUTE ŒvŽZ¼—ï”N‚v = ŒvŽZ¼—ï”N‚v + ŒvŽZ”N‚v - 1
026470              MOVE ŒvŽZŒŽ‚v TO ŒvŽZ¼—ïŒŽ‚v
026480              MOVE ŒvŽZ“ú‚v TO ŒvŽZ¼—ï“ú‚v
026490         END-IF
026500     END-IF.
026510*
026520*================================================================*
026530 •¬•‰’SŽÒ”Ô†Žæ“¾ SECTION.
026540*
026550*--------------------------------------------------------------------------
026560* •‰’SŽÒ”Ô†‚ª ÀÞÐ° 99XXXXXX‚Í 26XXXXXX ‚É‚·‚éB
026570* •‰’SŽÒ”Ô†‚ª”ŽšˆÈŠO‚ÅŽn‚Ü‚é(ˆï-)Žž‚ÍA26XXXXXX ‚É‚·‚éBXXXXXX‚ÍA‘•Û”Ô†
026580*   XXXXXX‚ÍAŽs’¬‘ºƒ}ƒXƒ^‚Ì’†‚Ì •ÛŒ¯ŽÒ”Ô† ‚ðŽg—p‚·‚éB(Ï½À‚É“ü—Í‚µ‚Ä‚¨‚­)
026590*--------------------------------------------------------------------------
026600*
026613     PERFORM •¬”Ô†¶‹l‚ß.
026620*
026630     IF •¬”Ô†‚v(1:2)  = "99"
026640*  / ÀÞÐ° ”Ô† /
026650         MOVE •¬”Ô†‚v              TO •ÛŒ¯ŽÒ”Ô†‚v
026660         MOVE "26"                    TO •ÛŒ¯ŽÒ”Ô†‚v(1:2)
026670         PERFORM •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß
026681         MOVE •ÛŒ¯ŽÒ”Ô†”Žš‚v        TO ì‚P|•¬•‰’SŽÒ”Ô†
026690     ELSE
026700*  / ”Žš /
026710         IF •¬”Ô†‚v(1:1)  = "0" OR "1" OR "2" OR "3" OR "4" OR
026720                               "5" OR "6" OR "7" OR "8" OR "9" OR SPACE
026731             MOVE •¬”Ô†‚v         TO •ÛŒ¯ŽÒ”Ô†‚v
026740             PERFORM •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß
026751             MOVE •ÛŒ¯ŽÒ”Ô†”Žš‚v   TO ì‚P|•¬•‰’SŽÒ”Ô†
026760         ELSE
026770*  / ”ŽšˆÈŠO /
026780             MOVE Žó|•¬Ží•Ê       TO Žs|Œö”ïŽí•Ê
026790             MOVE •¬”Ô†‚v         TO Žs|Žs’¬‘º”Ô†
026800             READ Žs’¬‘ºƒ}ƒXƒ^
026810             INVALID KEY
026821                 MOVE SPACE          TO ì‚P|•¬•‰’SŽÒ”Ô†
026830             NOT INVALID KEY
026840                 MOVE SPACE          TO •ÛŒ¯ŽÒ”Ô†‚v
026850                 MOVE "26"           TO •ÛŒ¯ŽÒ”Ô†‚v(1:2)
026860                 MOVE Žs|•ÛŒ¯ŽÒ”Ô† TO •ÛŒ¯ŽÒ”Ô†‚v(3:6)
026870                 PERFORM •ÛŒ¯ŽÒ”Ô†‰E‹l‚ß
026881                 MOVE •ÛŒ¯ŽÒ”Ô†”Žš‚v   TO ì‚P|•¬•‰’SŽÒ”Ô†
026890             END-READ
026900         END-IF
026910     END-IF.
026920*
026930*================================================================*
026940*================================================================*
026950 •¬”Ô†¶‹l‚ß SECTION.
026960*
026970***** •¬‚Ì•‰’SŽÒ”Ô†‚Ì–³‘Ê‚ÈSPACE‚ðŽæ‚èœ‚¢‚ÄA¶‹l‚ß‚É‚·‚éB
026980     MOVE SPACE           TO  •¬”Ô†‚v.
026990     MOVE SPACE           TO  •¬”Ô†Œ³‚v.
027000     MOVE SPACE           TO  •¬”Ô†¶‹l‚ß‚v.
027010     MOVE Žó|”ï—p•‰’SŽÒ”Ô†•¬   TO  •¬”Ô†Œ³‚v.
027020*
027030     MOVE  ZERO  TO  ƒJƒEƒ“ƒ^‚Q.
027040     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1 UNTIL ƒJƒEƒ“ƒ^ > 10
027050          IF  •¬”Ô†Œ³‚v‚P(ƒJƒEƒ“ƒ^) NOT = SPACE
027060              COMPUTE ƒJƒEƒ“ƒ^‚Q = ƒJƒEƒ“ƒ^‚Q  +  1
027070              MOVE •¬”Ô†Œ³‚v‚P(ƒJƒEƒ“ƒ^)  TO  •¬”Ô†¶‹l‚ß‚v‚P(ƒJƒEƒ“ƒ^‚Q)
027080          END-IF
027090     END-PERFORM.
027100*
027110     MOVE •¬”Ô†¶‹l‚ß‚v    TO •¬”Ô†‚v.
027120*
027130*================================================================*
027140 •‰Ží•Ê•ÏŠ· SECTION.
027150*
027160     MOVE ZERO  TO •‰Ží•Ê•ÏŠ·Œã‚v.
027170*
027180     EVALUATE •‰Ží•Ê•ÏŠ·‘O‚v
027190     WHEN  ZERO
027200        MOVE ZERO TO •‰Ží•Ê•ÏŠ·Œã‚v
027210* ”PÁ
027220     WHEN  01
027230        MOVE  4   TO •‰Ží•Ê•ÏŠ·Œã‚v
027240* ‘Å–o
027250     WHEN  02
027260        MOVE  5   TO •‰Ží•Ê•ÏŠ·Œã‚v
027270* Á
027280     WHEN  03
027290        MOVE  6   TO •‰Ží•Ê•ÏŠ·Œã‚v
027300* ’E‰P
027310     WHEN  04
027320        MOVE  3   TO •‰Ží•Ê•ÏŠ·Œã‚v
027330* œÜ
027340     WHEN  05
027350        MOVE  1   TO •‰Ží•Ê•ÏŠ·Œã‚v
027360* •s‘SœÜ
027370     WHEN  06
027380        MOVE  2   TO •‰Ží•Ê•ÏŠ·Œã‚v
027390* œÜE•s‘SœÜSk
027400     WHEN  07
027410     WHEN  08
027420        MOVE  7   TO •‰Ží•Ê•ÏŠ·Œã‚v
027430* •‰–¼‚È‚µi–³•aj
027440     WHEN  09
027450        MOVE  9   TO •‰Ží•Ê•ÏŠ·Œã‚v
027460     WHEN OTHER
027470        CONTINUE
027480     END-EVALUATE.
027490*
027502*================================================================*
027510 •ÛŒ¯Ží•Ê•ÏŠ· SECTION.
027520*
027530     MOVE ZERO  TO •ÛŒ¯Ží•Ê•ÏŠ·Œã‚v.
027540*
027550     EVALUATE •ÛŒ¯Ží•Ê•ÏŠ·‘O‚v
027560     WHEN  ZERO
027570        MOVE ZERO TO •ÛŒ¯Ží•Ê•ÏŠ·Œã‚v
027580* ‘•Û
027590     WHEN  1
027600        MOVE  4   TO •ÛŒ¯Ží•Ê•ÏŠ·Œã‚v
027601* ‹¦‰ï‚¯‚ñ‚Û
027602     WHEN  2
027603        MOVE  1   TO •ÛŒ¯Ží•Ê•ÏŠ·Œã‚v
027604* Œ’•Û‘g‡
027605     WHEN  3
027606        MOVE  2   TO •ÛŒ¯Ží•Ê•ÏŠ·Œã‚v
027607* ‹¤Ï‘g‡
027608     WHEN  4
027609        MOVE  3   TO •ÛŒ¯Ží•Ê•ÏŠ·Œã‚v
027610* ŒãŠú‚—î
027611     WHEN  5
027612        MOVE  6   TO •ÛŒ¯Ží•Ê•ÏŠ·Œã‚v
027613* ‘ÞE
027614     WHEN  8
027615        MOVE  5   TO •ÛŒ¯Ží•Ê•ÏŠ·Œã‚v
027619* ‚»‚Ì‘¼
027720     WHEN OTHER
027740        MOVE 9    TO •ÛŒ¯Ží•Ê•ÏŠ·Œã‚v
027741     END-EVALUATE.
027750*
027760*================================================================*
027761*================================================================*
027762 “]‹A‹æ•ª•ÏŠ· SECTION.
027763*
027764     MOVE ZERO  TO “]‹A•ÏŠ·Œã‚v.
027765*
027766     EVALUATE “]‹A•ÏŠ·‘O‚v
027767     WHEN  ZERO
027768        MOVE ZERO TO “]‹A•ÏŠ·Œã‚v
027769* Ž¡–ü
027770     WHEN  1
027771     WHEN  2
027772     WHEN  5
027773        MOVE  1   TO “]‹A•ÏŠ·Œã‚v
027774* ’†Ž~
027775     WHEN  3
027776        MOVE  2   TO “]‹A•ÏŠ·Œã‚v
027777* “]ˆã
027778     WHEN  4
027779        MOVE  3   TO “]‹A•ÏŠ·Œã‚v
027780* Œp‘±
027781     WHEN  9
027782        MOVE  ZERO TO “]‹A•ÏŠ·Œã‚v
027783     WHEN OTHER
027784        CONTINUE
027785     END-EVALUATE.
027786*
027787*================================================================*
027788 •‰Œ´ˆöˆóü‘ÎÛ”»’èˆ— SECTION.
027789*------------------------------------------------------------------------------------*
027790* §Œäƒ}ƒXƒ^‚Ìu•‰Œ´ˆöˆóü‹æ•ªv‚ª 3 i‚R•”ˆÊˆÈãˆóüj‚ÌŽžA‚R•”ˆÊˆÈã‚©”»’è‚µ‚ÄA
027791* ‚»‚ÌŽž‚Ì‚ÝA•‰Œ´ˆö‚ðˆóü‚·‚éB
027792*------------------------------------------------------------------------------------*
027793*
027794     MOVE  SPACE TO  ˜AƒŒƒZ•‰Œ´ˆó|ƒL[.
027795     INITIALIZE      ˜AƒŒƒZ•‰Œ´ˆó|ƒL[.
027796     MOVE Ž{p˜a—ï‚v‚q  TO  ˜AƒŒƒZ•‰Œ´ˆó|Ž{p˜a—ï.
027797     MOVE Ž{p”N‚v‚q    TO  ˜AƒŒƒZ•‰Œ´ˆó|Ž{p”N.
027798     MOVE Ž{pŒŽ‚v‚q    TO  ˜AƒŒƒZ•‰Œ´ˆó|Ž{pŒŽ.
027799     MOVE Š³ŽÒ”Ô†‚v‚q  TO  ˜AƒŒƒZ•‰Œ´ˆó|Š³ŽÒ”Ô†.
027800     MOVE Ž}”Ô‚v‚q      TO  ˜AƒŒƒZ•‰Œ´ˆó|Ž}”Ô.
027801     CALL   "RECEHUGE".
027802     CANCEL "RECEHUGE".
027803*
027804     IF ˜AƒŒƒZ•‰Œ´ˆó|‘ÎÛƒtƒ‰ƒO = "YES"
027805        PERFORM •‰Œ´ˆöŽæ“¾
027806     END-IF.
027807*
027808*================================================================*
027809 •‰Œ´ˆöŽæ“¾ SECTION.
027810*
027811********************************************************************
027812*  •‰Œ´ˆöƒR[ƒh‚ª“¯‚¶‚à‚Ì‚ÍA1s‚É‚Ü‚Æ‚ß‚ÄˆóŽš‚·‚éB
027820*  —á: ‡@‡A ‰Æ‚Å“]‚ñ‚¾.
027830*     •‰Œ´ˆöƒR[ƒh‚ª“¯‚¶‚à‚Ì‚ð‚Ü‚Æ‚ßAƒe[ƒuƒ‹‚ÉƒZƒbƒg
027840*     (‚½‚¾‚µA•”ˆÊ‚ð”ò‚ñ‚Å“¯‚¶‚à‚Ì‚ÍA2s‚É‚È‚é)
027850********************************************************************
027860     MOVE  ZERO   TO  ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q.
027870     PERFORM VARYING •”ˆÊ‚b‚m‚s FROM 1 BY 1
027880             UNTIL ( •”ˆÊ‚b‚m‚s > •”ˆÊ”‚v )
027890*
027900****        IF ( •‰|•‰Š³ŽÒ”Ô†(•”ˆÊ‚b‚m‚s)  NOT = ZERO )  AND
027910        IF ( •‰|•‰˜A”Ô(•”ˆÊ‚b‚m‚s)      NOT = ZERO )
027920*
027930           IF ƒJƒEƒ“ƒ^ = ZERO
027940               MOVE 1   TO  ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q
027950               MOVE •‰|•‰Š³ŽÒ”Ô†(•”ˆÊ‚b‚m‚s) TO •‰Š³ŽÒ”Ô†‚v(ƒJƒEƒ“ƒ^)  •‰Š³ŽÒ”Ô†‚b‚v
027960               MOVE •‰|•‰˜A”Ô(•”ˆÊ‚b‚m‚s)     TO •‰˜A”Ô‚v(ƒJƒEƒ“ƒ^)   •‰˜A”Ô‚b‚v
027970               MOVE •”ˆÊ‚b‚m‚s                   TO •‰Œ´ˆö•”ˆÊ‚v(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q)
027980           ELSE
027990              IF ( •‰|•‰Š³ŽÒ”Ô†(•”ˆÊ‚b‚m‚s)  = •‰Š³ŽÒ”Ô†‚b‚v )  AND
028000                 ( •‰|•‰˜A”Ô(•”ˆÊ‚b‚m‚s)      = •‰˜A”Ô‚b‚v     )
028010                 COMPUTE ƒJƒEƒ“ƒ^‚Q = ƒJƒEƒ“ƒ^‚Q  +  1
028020                 MOVE •”ˆÊ‚b‚m‚s                  TO •‰Œ´ˆö•”ˆÊ‚v(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q)
028030              ELSE
028040                 COMPUTE ƒJƒEƒ“ƒ^ = ƒJƒEƒ“ƒ^  +  1
028050                 MOVE 1   TO  ƒJƒEƒ“ƒ^‚Q
028060                 MOVE •‰|•‰Š³ŽÒ”Ô†(•”ˆÊ‚b‚m‚s) TO •‰Š³ŽÒ”Ô†‚v(ƒJƒEƒ“ƒ^)  •‰Š³ŽÒ”Ô†‚b‚v
028070                 MOVE •‰|•‰˜A”Ô(•”ˆÊ‚b‚m‚s)     TO •‰˜A”Ô‚v(ƒJƒEƒ“ƒ^)  •‰˜A”Ô‚b‚v
028080                 MOVE •”ˆÊ‚b‚m‚s                   TO •‰Œ´ˆö•”ˆÊ‚v(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q)
028090              END-IF
028100           END-IF
028110        END-IF
028120     END-PERFORM.
028130**************************************************************************
028140*  •‰Œ´ˆöƒ}ƒXƒ^‚æ‚è•¶ÍŽæ“¾
028150**************************************************************************
028161     MOVE  ZERO   TO  ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q.
028170     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1
028180             UNTIL ( ƒJƒEƒ“ƒ^ > 9 )  OR ( •‰˜A”Ô‚v(ƒJƒEƒ“ƒ^) = ZERO )
028190** Œ’•Û‚Í ‹æ•ª 01
028200         MOVE 01                        TO •‰Œ´|‹æ•ªƒR[ƒh
028210         MOVE •‰Š³ŽÒ”Ô†‚v(ƒJƒEƒ“ƒ^)  TO •‰Œ´|Š³ŽÒ”Ô†
028220         MOVE •‰˜A”Ô‚v(ƒJƒEƒ“ƒ^)      TO •‰Œ´|•‰Œ´ˆö˜A”Ô
028230         READ •‰Œ´ˆö‚e
028240         NOT INVALID KEY
028250             INITIALIZE •‰Œ´ˆö‚v‚s
028260             MOVE •‰Œ´|•‰Œ´ˆö‚b‚l(1) TO  •‰Œ´ˆö‚P‚v‚s
028270             MOVE •‰Œ´|•‰Œ´ˆö‚b‚l(2) TO  •‰Œ´ˆö‚Q‚v‚s
028280             MOVE •‰Œ´|•‰Œ´ˆö‚b‚l(3) TO  •‰Œ´ˆö‚R‚v‚s
028290             MOVE •‰Œ´|•‰Œ´ˆö‚b‚l(4) TO  •‰Œ´ˆö‚S‚v‚s
028300             MOVE •‰Œ´|•‰Œ´ˆö‚b‚l(5) TO  •‰Œ´ˆö‚T‚v‚s
028310             PERFORM VARYING ƒJƒEƒ“ƒ^‚Q FROM 1 BY 1
028320                     UNTIL ( ƒJƒEƒ“ƒ^‚Q > 9 )  OR 
028330                           ( •‰Œ´ˆö•”ˆÊ‚v(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q) = ZERO )
028340                EVALUATE •‰Œ´ˆö•”ˆÊ‚v(ƒJƒEƒ“ƒ^ ƒJƒEƒ“ƒ^‚Q)
028350                WHEN 1
028360                   MOVE "‡@"  TO  •‰Œ´ˆöƒiƒ“ƒo[‚v‚P(ƒJƒEƒ“ƒ^‚Q)
028370                WHEN 2
028380                   MOVE "‡A"  TO  •‰Œ´ˆöƒiƒ“ƒo[‚v‚P(ƒJƒEƒ“ƒ^‚Q)
028390                WHEN 3
028400                   MOVE "‡B"  TO  •‰Œ´ˆöƒiƒ“ƒo[‚v‚P(ƒJƒEƒ“ƒ^‚Q)
028410                WHEN 4
028420                   MOVE "‡C"  TO  •‰Œ´ˆöƒiƒ“ƒo[‚v‚P(ƒJƒEƒ“ƒ^‚Q)
028430                WHEN 5
028440                   MOVE "‡D"  TO  •‰Œ´ˆöƒiƒ“ƒo[‚v‚P(ƒJƒEƒ“ƒ^‚Q)
028410                WHEN 6
028420                   MOVE "‡E"  TO  •‰Œ´ˆöƒiƒ“ƒo[‚v‚P(ƒJƒEƒ“ƒ^‚Q)
028430                WHEN 7
028440                   MOVE "‡F"  TO  •‰Œ´ˆöƒiƒ“ƒo[‚v‚P(ƒJƒEƒ“ƒ^‚Q)
028450                WHEN OTHER
028460                   CONTINUE
028470                END-EVALUATE
028480             END-PERFORM
028490*
028500             IF •‰Œ´|•‰Œ´ˆö“ü—Í‹æ•ª = 1
028510                 STRING •‰Œ´ˆöƒiƒ“ƒo[‚m‚v  DELIMITED BY SPACE
028520                        •‰Œ´ˆö‚P‚v‚s  DELIMITED BY SIZE
028530                        •‰Œ´ˆö‚Q‚v‚s  DELIMITED BY SIZE
028540                        •‰Œ´ˆö‚R‚v‚s  DELIMITED BY SIZE
028550                        •‰Œ´ˆö‚S‚v‚s  DELIMITED BY SIZE
028560                        •‰Œ´ˆö‚T‚v‚s  DELIMITED BY SIZE
028570                        INTO •‰Œ´ˆö“à—e‡¬‚v(ƒJƒEƒ“ƒ^)
028580                 END-STRING
028590             ELSE
028600                 INSPECT •‰Œ´ˆö‚v‚s REPLACING ALL ‘SŠp‹ó”’ BY ”¼Šp‹ó”’
028610                 MOVE SPACE TO •¶Žš‚P‚v •¶Žš‚Q‚v
028620                 MOVE •‰Œ´ˆöƒiƒ“ƒo[‚m‚v TO •¶Žš‚P‚v
028630                 MOVE •‰Œ´ˆö‚P‚v‚s       TO •¶Žš‚Q‚v
028640                 CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
028650                      USING BY REFERENCE •¶Žš‚P‚v
028660                            BY REFERENCE •¶Žš‚Q‚v
028670                 MOVE •‰Œ´ˆö‚Q‚v‚s       TO •¶Žš‚Q‚v
028680                 CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
028690                      USING BY REFERENCE •¶Žš‚P‚v
028700                            BY REFERENCE •¶Žš‚Q‚v
028710                 MOVE •‰Œ´ˆö‚R‚v‚s       TO •¶Žš‚Q‚v
028720                 CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
028730                      USING BY REFERENCE •¶Žš‚P‚v
028740                            BY REFERENCE •¶Žš‚Q‚v
028750                 MOVE •‰Œ´ˆö‚S‚v‚s       TO •¶Žš‚Q‚v
028760                 CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
028770                      USING BY REFERENCE •¶Žš‚P‚v
028780                            BY REFERENCE •¶Žš‚Q‚v
028790                 MOVE •‰Œ´ˆö‚T‚v‚s       TO •¶Žš‚Q‚v
028800                 CALL ƒvƒƒOƒ‰ƒ€–¼‚v WITH C LINKAGE
028810                      USING BY REFERENCE •¶Žš‚P‚v
028820                            BY REFERENCE •¶Žš‚Q‚v
028830                  MOVE •¶Žš‚P‚v            TO •‰Œ´ˆö“à—e‡¬‚v(ƒJƒEƒ“ƒ^)
028841             END-IF
028850*
028860         END-READ
028870     END-PERFORM.
028880*
029220*================================================================*
029230 ’·Šú——R•¶Žæ“¾ SECTION.
029240*
029250* ’·Šú——R•¶Žæ“¾‚Í "CHOUBUN" ‚ðŒÄ‚Ô. 
029260     MOVE  SPACE TO  ˜A’·•¶|ƒL[.
029270     INITIALIZE      ˜A’·•¶|ƒL[.
029280     MOVE Ž{p˜a—ï‚v‚q  TO  ˜A’·•¶|Ž{p˜a—ï.
029290     MOVE Ž{p”N‚v‚q    TO  ˜A’·•¶|Ž{p”N.
029300     MOVE Ž{pŒŽ‚v‚q    TO  ˜A’·•¶|Ž{pŒŽ.
029310     MOVE Š³ŽÒ”Ô†‚v‚q  TO  ˜A’·•¶|Š³ŽÒ”Ô†.
029320     MOVE Ž}”Ô‚v‚q      TO  ˜A’·•¶|Ž}”Ô.
029330** “úÚ—p‚Í56Œ…
029340     MOVE 56            TO  ˜A’·•¶|•¶Œ…”.
029350*
029360     CALL   "CHOUBUN".
029370     CANCEL "CHOUBUN".
029380*
029790*================================================================*
029791 •¬ƒŒƒZ‚Ü‚Æ‚ß”»’è SECTION.
029792*---------------------------------------------------------------------------*
029793* –{‘Ì‚Ü‚Æ‚ß‹æ•ª‚P
029794* ‚ÌŽž‚ÍAƒtƒ‰ƒOYES (‹àŠz‚ð•¬ž‚Ý‚ÅˆóŽšj
029795*i—áF‰¡•lŽs‚ÌáŠQ‚ÍA–{‘Ì•ÛŒ¯i‘•ÛŒnj‚ÌƒŒƒZƒvƒg‚P–‡‚Å¿‹A•¬ƒŒƒZ‚Í‚È‚µj
029796*---------------------------------------------------------------------------*
029797*
029798     MOVE SPACE TO •¬ƒŒƒZ‚Ü‚Æ‚ßƒtƒ‰ƒO.
029799*
029800     IF ƒŒƒZ|–{‘Ì‚Ü‚Æ‚ß‹æ•ª = 1 
029801        MOVE "YES" TO •¬ƒŒƒZ‚Ü‚Æ‚ßƒtƒ‰ƒO
029802     END-IF.
029803*
029851*================================================================*
029852 ‘½•”ˆÊ’üŒ¸—¦Žæ“¾ SECTION.
029853*
029854     MOVE 01             TO Œv|§Œä‹æ•ª.
029855     MOVE ƒŒƒZ|Ž{p˜a—ï TO Œv|ŠJŽn˜a—ï Ž{p˜a—ï‚b‚v.
029856     MOVE ƒŒƒZ|Ž{p”N   TO Œv|ŠJŽn”N   Ž{p”N‚b‚v.
029857     MOVE ƒŒƒZ|Ž{pŒŽ   TO Œv|ŠJŽnŒŽ   Ž{pŒŽ‚b‚v.
029858*
029859     START ŒvŽZƒ}ƒXƒ^ KEY IS <= Œv|§Œä‹æ•ª Œv|ŠJŽn˜a—ï”NŒŽ REVERSED
029860     END-START.
029861*
029862     IF ó‘ÔƒL[ = "00"
029863         READ ŒvŽZƒ}ƒXƒ^ NEXT
029864         AT END
029865*/ƒGƒ‰[•\Ž¦‚ÌC³
029866             DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
029867                     " ŽófŽÒ‡‚=" ƒŒƒZ|Š³ŽÒƒR[ƒh
029868                     " Ž{p”NŒŽ=" ƒŒƒZ|Ž{p”N ƒŒƒZ|Ž{pŒŽ   UPON CONS
029869*-----------------------------------------*
029870             CALL "actcshm"  WITH C LINKAGE
029871*-----------------------------------------*
029872             ACCEPT  ƒL[“ü—Í FROM CONS
029873             PERFORM ƒtƒ@ƒCƒ‹•Â½
029874             MOVE ZERO TO PROGRAM-STATUS
029875             EXIT PROGRAM
029876         NOT AT END
029877             IF ( Ž{p˜a—ï”NŒŽ‚b‚v >= Œv‚`|ŠJŽn˜a—ï”NŒŽ ) AND
029878                ( Ž{p˜a—ï”NŒŽ‚b‚v <= Œv‚`|I—¹˜a—ï”NŒŽ )
029879                 MOVE Œv‚`|‘½•”ˆÊ’üŒ¸—¦(2) TO ‚Q•”ˆÊ–Ú’üŒ¸—¦‚v
029880                 MOVE Œv‚`|‘½•”ˆÊ’üŒ¸—¦(3) TO ‚R•”ˆÊ–Ú’üŒ¸—¦‚v
029881             ELSE
029882*/ƒGƒ‰[•\Ž¦‚ÌC³
029883                 DISPLAY "Ž{p”NŒŽ‚É‘Î‰ž‚µ‚½—¿‹à‚ª‚Ý‚Â‚©‚è‚Ü‚¹‚ñ"
029884                         " ŽófŽÒ‡‚=" ƒŒƒZ|Š³ŽÒƒR[ƒh
029885                         " Ž{p”NŒŽ=" ƒŒƒZ|Ž{p”N ƒŒƒZ|Ž{pŒŽ   UPON CONS
029886*-----------------------------------------*
029887                 CALL "actcshm"  WITH C LINKAGE
029888*-----------------------------------------*
029889                 ACCEPT  ƒL[“ü—Í FROM CONS
029890                 PERFORM ƒtƒ@ƒCƒ‹•Â½
029891                 MOVE ZERO TO PROGRAM-STATUS
029892                 EXIT PROGRAM
029893             END-IF
029894         END-READ
029895     END-IF.
029896*
029897*================================================================*
029898*================================================================*
029899******************************************************************
029900 END PROGRAM YHP101.
029901******************************************************************
