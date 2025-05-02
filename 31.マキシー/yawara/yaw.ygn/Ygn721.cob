000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YGN721.
000060 AUTHOR.                 ’r“c@KŽq
000070*
000080*----------------------------------------------------------------*
000090*         ƒJƒ‹ƒei— jyÃÞ°Àì¬z_+³¨ÝÄÞ³½Þ”Å
000100*         MED = YGN720 
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2016-04-07
000130 DATE-COMPILED.          2016-04-07
      *
      */2019/08/01 V—pŽ†‘Î‰žB‰^“®—Ã–@—¿‚ð‹à‘®•›Žq—“‚Ö
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
000260     SELECT  Œ³†ƒ}ƒXƒ^      ASSIGN      TO        GENGOUL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  Œ³|Œ³†‹æ•ª
000300                             FILE STATUS              IS  ó‘ÔƒL[
000310                             LOCK        MODE         IS  AUTOMATIC.
000320     SELECT  §Œäî•ñƒ}ƒXƒ^  ASSIGN      TO        SEIGYOL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  §|§Œä‹æ•ª
000360                             FILE STATUS              IS  ó‘ÔƒL[
000370                             LOCK        MODE         IS  AUTOMATIC.
000320     SELECT  ŽófŽÒî•ñ‚e    ASSIGN      TO        JUSINJL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  Žó|Ž{p˜a—ï”NŒŽ
000360                                                          Žó|Š³ŽÒƒR[ƒh
000370                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000380                                                          Žó|Š³ŽÒƒJƒi
000390                                                          Žó|Š³ŽÒƒR[ƒh
000400                             ALTERNATE RECORD KEY     IS  Žó|Š³ŽÒƒR[ƒh
000410                                                          Žó|Ž{p˜a—ï”NŒŽ
000420                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000430                                                          Žó|•ÛŒ¯Ží•Ê
000440                                                          Žó|•ÛŒ¯ŽÒ”Ô†
000450                                                          Žó|Š³ŽÒƒR[ƒh
000460                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000470                                                          Žó|Œö”ïŽí•Ê
000480                                                          Žó|”ï—p•‰’SŽÒ”Ô†
000490                                                          Žó|Š³ŽÒƒR[ƒh
000500                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000510                                                          Žó|•¬Ží•Ê
000520                                                          Žó|”ï—p•‰’SŽÒ”Ô†•¬
000530                                                          Žó|Š³ŽÒƒR[ƒh
000540                             ALTERNATE RECORD KEY     IS  Žó|¿‹˜a—ï”NŒŽ
000550                                                          Žó|Ž{p˜a—ï”NŒŽ
000560                                                          Žó|Š³ŽÒƒR[ƒh
000570                             FILE STATUS              IS  ó‘ÔƒL[
000580                             LOCK        MODE         IS  AUTOMATIC.
000590     SELECT  •‰ƒf[ƒ^‚e    ASSIGN      TO        HUSYOUL
000600                             ORGANIZATION             IS  INDEXED
000610                             ACCESS MODE              IS  DYNAMIC
000620                             RECORD KEY               IS  •‰|Ž{p˜a—ï”NŒŽ
000630                                                          •‰|Š³ŽÒƒR[ƒh
000640                             ALTERNATE RECORD KEY     IS  •‰|Š³ŽÒƒR[ƒh
000650                                                          •‰|Ž{p˜a—ï”NŒŽ
000660                             FILE STATUS              IS  ó‘ÔƒL[
000670                             LOCK        MODE         IS  AUTOMATIC.
000680     SELECT  Ž{p‹L˜^‚e      ASSIGN      TO      SEKIROKL
000690                             ORGANIZATION        IS  INDEXED
000700                             ACCESS MODE         IS  DYNAMIC
000710                             RECORD KEY          IS  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
000720                                                     Ž{‹L|Š³ŽÒƒR[ƒh
000730                             ALTERNATE RECORD KEY IS Ž{‹L|Š³ŽÒƒR[ƒh
000740                                                     Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
000750                             FILE STATUS              IS  ó‘ÔƒL[
000760                             LOCK        MODE         IS  AUTOMATIC.
000770     SELECT  ƒƒ‚ƒtƒ@ƒCƒ‹    ASSIGN      TO        MEMOL
000780                             ORGANIZATION             IS  INDEXED
000790                             ACCESS MODE              IS  DYNAMIC
000800                             RECORD KEY               IS  ƒƒ‚|§Œä‹æ•ª
000810                                                          ƒƒ‚|Š³ŽÒƒR[ƒh
000820                                                          ƒƒ‚|Ž{p˜a—ï”NŒŽ“ú
000830                             ALTERNATE RECORD KEY     IS  ƒƒ‚|§Œä‹æ•ª
000840                                                          ƒƒ‚|Ž{p˜a—ï”NŒŽ“ú
000850                                                          ƒƒ‚|Š³ŽÒƒR[ƒh
000860                             ALTERNATE RECORD KEY     IS  ƒƒ‚|Š³ŽÒƒR[ƒh
000870                                                          ƒƒ‚|Ž{p˜a—ï”NŒŽ“ú
000880                                                          ƒƒ‚|§Œä‹æ•ª
000890                             FILE STATUS              IS  ó‘ÔƒL[
000900                             LOCK        MODE         IS  AUTOMATIC.
000910     SELECT  ƒŒƒZƒvƒg‚e      ASSIGN      TO        RECEPTL
000920                             ORGANIZATION             IS  INDEXED
000930                             ACCESS MODE              IS  DYNAMIC
000940                             RECORD KEY               IS  ƒŒƒZ|Ž{p˜a—ï”NŒŽ
000950                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
000960                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
000970                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|Š³ŽÒƒR[ƒh
000980                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
000990                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001000                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|¿‹˜a—ï”NŒŽ
001010                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001020                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
001030                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001040                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|¿‹˜a—ï”NŒŽ
001050                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001060                                                          ƒŒƒZ|¿‹•ÛŒ¯ŽÒ”Ô†
001070                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
001080                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001090                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|¿‹˜a—ï”NŒŽ
001100                                                          ƒŒƒZ|¿‹•ÛŒ¯ŽÒ”Ô†
001110                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
001120                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
001130                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
001140                             FILE STATUS              IS  ó‘ÔƒL[
001150                             LOCK        MODE         IS  AUTOMATIC.
001160     SELECT  ‰ïŒvƒf[ƒ^‚e    ASSIGN      TO        KAIKEIL
001170                             ORGANIZATION             IS  INDEXED
001180                             ACCESS MODE              IS  DYNAMIC
001190                             RECORD KEY               IS  ‰ï|Ž{p˜a—ï”NŒŽ“ú
001200                                                          ‰ï|Š³ŽÒƒR[ƒh
001210                             ALTERNATE RECORD KEY     IS  ‰ï|Š³ŽÒƒR[ƒh
001220                                                          ‰ï|Ž{p˜a—ï”NŒŽ“ú
001230                             FILE STATUS              IS  ó‘ÔƒL[
001240                             LOCK        MODE         IS  AUTOMATIC.
001250     SELECT  ì‹Æƒtƒ@ƒCƒ‹‚P  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W7211L.DAT"
001260                             ORGANIZATION             IS  INDEXED
001270                             ACCESS                   IS  DYNAMIC
001280                             RECORD      KEY          IS  ì‚P|Ž{p˜a—ï”NŒŽ“ú
001290                                                          ì‚P|Š³ŽÒƒR[ƒh
001300                             ALTERNATE RECORD KEY     IS  ì‚P|Ž{p˜a—ï”NŒŽ“ú
001310                                                          ì‚P|Š³ŽÒƒJƒi
001320                                                          ì‚P|Š³ŽÒƒR[ƒh
001330                             ALTERNATE RECORD KEY     IS  ì‚P|Š³ŽÒƒR[ƒh
001340                                                          ì‚P|Ž{p˜a—ï”NŒŽ“ú
001350                             ALTERNATE RECORD KEY     IS  ì‚P|Š³ŽÒƒJƒi
001360                                                          ì‚P|Š³ŽÒƒR[ƒh
001370                                                          ì‚P|Ž{p˜a—ï”NŒŽ“ú
001380                             FILE        STATUS       IS  ó‘ÔƒL[
001390                             LOCK        MODE         IS  AUTOMATIC.
001400     SELECT  ì‹Æƒtƒ@ƒCƒ‹‚Q  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W7212L.DAT"
001410                             ORGANIZATION             IS  INDEXED
001420                             ACCESS                   IS  DYNAMIC
001430                             RECORD      KEY          IS  ì‚Q|Ž{p˜a—ï”NŒŽ
001440                                                          ì‚Q|Š³ŽÒƒR[ƒh
001450                             FILE        STATUS       IS  ó‘ÔƒL[
001460                             LOCK        MODE         IS  AUTOMATIC.
001610******************************************************************
001620*                      DATA DIVISION                             *
001630******************************************************************
001640 DATA                    DIVISION.
001650 FILE                    SECTION.
001660*                           m‚q‚k  ‚P‚Q‚Wn
001670 FD  Œ³†ƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
001680     COPY GENGOU          OF  XFDLIB  JOINING   Œ³   AS  PREFIX.
001230*                           m‚q‚k  ‚Q‚T‚Un
001240 FD  §Œäî•ñƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
001250     COPY SEIGYO          OF  XFDLIB  JOINING   §   AS  PREFIX.
001250     COPY SEIGYO01        OF  XFDLIB  JOINING   §‚O‚P   AS  PREFIX.
001690*                           m‚q‚k  ‚R‚Q‚On
001700 FD  ŽófŽÒî•ñ‚e        BLOCK   CONTAINS   1   RECORDS.
001710     COPY JUSINJ          OF  XFDLIB  JOINING   Žó   AS  PREFIX.
001720*                           m‚q‚k  ‚Q‚T‚Un
001730 FD  Ž{p‹L˜^‚e          BLOCK   CONTAINS   1   RECORDS.
001740    COPY SEKIROK         OF  XFDLIB  JOINING   Ž{‹L AS  PREFIX.
001750*                           m‚q‚k  ‚P‚Q‚Wn
001760 FD  •‰ƒf[ƒ^‚e        BLOCK   CONTAINS   1   RECORDS.
001770     COPY HUSYOU          OF  XFDLIB  JOINING   •‰   AS  PREFIX.
001780*                           m‚q‚k  ‚W‚R‚Qn
001790 FD  ƒƒ‚ƒtƒ@ƒCƒ‹        BLOCK CONTAINS 1     RECORDS.
001800     COPY MEMO           OF    XFDLIB JOINING ƒƒ‚ AS PREFIX.
001810*                          m‚q‚k  ‚P‚T‚R‚Un
001820 FD  ƒŒƒZƒvƒg‚e          BLOCK   CONTAINS   1   RECORDS.
001830     COPY RECEPT          OF  XFDLIB  JOINING   ƒŒƒZ  AS  PREFIX.
001840*                           m‚q‚k  ‚T‚P‚Qn
001850 FD  ‰ïŒvƒf[ƒ^‚e        BLOCK   CONTAINS   1   RECORDS.
001860     COPY KAIKEI     OF  XFDLIB  JOINING   ‰ï   AS  PREFIX.
001870**************************
001880* ì‹Æƒtƒ@ƒCƒ‹‚P^ƒJƒ‹ƒe *
001890**************************
001900*                         m‚q‚k  ‚Q‚V‚Qn
001910 FD  ì‹Æƒtƒ@ƒCƒ‹‚P RECORD  CONTAINS 272 CHARACTERS.
001920 01 ì‚P|ƒŒƒR[ƒh.
001930    03 ì‚P|ƒŒƒR[ƒhƒL[.
001940       05 ì‚P|Ž{p˜a—ï”NŒŽ“ú.
001950          07 ì‚P|Ž{p˜a—ï               PIC 9.
001960          07 ì‚P|Ž{p”NŒŽ.
001970             09 ì‚P|Ž{p”N              PIC 9(2).
001980             09 ì‚P|Ž{pŒŽ              PIC 9(2).
001990          07 ì‚P|Ž{p“ú                 PIC 9(2).
002000       05 ì‚P|Š³ŽÒƒR[ƒh.
002010          07 ì‚P|Š³ŽÒ”Ô†                PIC 9(6).
002020          07 ì‚P|Ž}”Ô                    PIC X(1).
002030    03 ì‚P|ƒŒƒR[ƒhƒf[ƒ^.
002100       05 ì‚P|Š³ŽÒƒJƒi                   PIC X(50).
002110       05 ì‚P|Š³ŽÒŽ–¼                   PIC X(50).
002130       05 ì‚P|—¿‹à.
001550          07 ì‚P|‰ŒŸ‹àŠz                PIC 9(5).
                07 ì‚P|®•œ                    OCCURS 4.
001550             09 ì‚P|®•œ‹àŠz             PIC 9(5).
001550          07 ì‚P|‚»‚Ì‘¼                  PIC 9(5).
001550          07 ì‚P|ãª–@“™                  PIC 9(5).
001550          07 ì‚P|‹à‘®•›Žq                PIC 9(5).
001550          07 ì‚P|‰—Ã—¿                  PIC 9(5).
002300          07 ì‚P|”ï—pŠz                  PIC 9(6).
002270          07 ì‚P|ˆê•”•‰’S‹à              PIC 9(5).
002280          07 ì‚P|ƒRƒƒ“ƒg                PIC X(100).
002310       05 FILLER                           PIC X(2).
002320*
002330*                         m‚q‚k  ‚P‚Q‚Wn
002340 FD  ì‹Æƒtƒ@ƒCƒ‹‚Q RECORD  CONTAINS 128 CHARACTERS.
002350 01 ì‚Q|ƒŒƒR[ƒh.
002360    03 ì‚Q|ƒŒƒR[ƒhƒL[.
002370       05 ì‚Q|Ž{p˜a—ï”NŒŽ.
002380          07 ì‚Q|Ž{p˜a—ï                PIC 9.
002390          07 ì‚Q|Ž{p”NŒŽ.
002400             09 ì‚Q|Ž{p”N               PIC 9(2).
002410             09 ì‚Q|Ž{pŒŽ               PIC 9(2).
002420       05 ì‚Q|Š³ŽÒƒR[ƒh.
002430          07 ì‚Q|Š³ŽÒ”Ô†                PIC 9(6).
002440          07 ì‚Q|Ž}”Ô                    PIC X(1).
002450    03 ì‚Q|ƒŒƒR[ƒhƒf[ƒ^.
002610       05 ì‚Q|¿‹Šz                     PIC 9(6).
002620       05 ì‚Q|‰ŒŸ“™                     PIC 9(6).
             05 ì‚Q|®•œ                       OCCURS 4.
001550          07 ì‚Q|®•œ‹àŠz                PIC 9(6).
001550       05 ì‚Q|‚»‚Ì‘¼                     PIC 9(6).
001550       05 ì‚Q|ãª–@“™                     PIC 9(6).
001550       05 ì‚Q|‹à‘®•›Žq                   PIC 9(6).
001550       05 ì‚Q|‰—Ã—¿                     PIC 9(6).
002600       05 ì‚Q|”ï—pŠz                     PIC 9(6).
002710       05 FILLER                           PIC X(50).
002720*
003570*----------------------------------------------------------------*
003580******************************************************************
003590*                WORKING-STORAGE SECTION                         *
003600******************************************************************
003610 WORKING-STORAGE         SECTION.
003620 01 ƒL[“ü—Í                           PIC X    VALUE SPACE.
003630 01 ó‘ÔƒL[                           PIC X(2) VALUE SPACE.
003640 01 I—¹ƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
003650 01 I—¹ƒtƒ‰ƒO‚Q                       PIC X(3) VALUE SPACE.
003660 01 I—¹ƒtƒ‰ƒO‚R                       PIC X(3) VALUE SPACE.
003670 01 ƒtƒ@ƒCƒ‹–¼                         PIC N(2) VALUE SPACE.
003680 01 ŽÀsƒL[‚v                         PIC X(4) VALUE SPACE.
003690 01 Ž{p‹L˜^—L‚v                       PIC X(3) VALUE SPACE.
003700 01 Œp‘±ƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
003710 01 ‘ÎÛƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
003720 01 •”ˆÊ‚b‚m‚s                         PIC 9    VALUE ZERO.
003730 01 ƒJƒEƒ“ƒ^                           PIC 9    VALUE ZERO.
003740 01 ˆóü‚ ‚èƒtƒ‰ƒO                     PIC 9    VALUE ZERO.
003750 01 ˆóüƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
003730 01 —pŽ†Ží•Ê‚v                         PIC 9    VALUE ZERO.
003760* **************
003770* * €–Ú‘Ò”ð—p 
003780* **************
003790 01 ‘Ò”ð€–Ú‚v‚q.
003800    03 Ž{p˜a—ï”NŒŽ‚v‚q.
003810       05 Ž{p˜a—ï‚v‚q                 PIC 9(1) VALUE ZERO.
003820       05 Ž{p”N‚v‚q                   PIC 9(2) VALUE ZERO.
003830       05 Ž{pŒŽ‚v‚q                   PIC 9(2) VALUE ZERO.
003840    03 Ž{p“ú‚v‚q                      PIC 9(2) VALUE ZERO.
003850    03 ŠJŽn“ú‚v‚o                      PIC 9(2) VALUE ZERO.
003860    03 I—¹“ú‚v‚o                      PIC 9(2) VALUE ZERO.
003870    03 ŠJŽn˜a—ï”NŒŽ“ú‚v‚q.
003880       05 ŠJŽn˜a—ï”NŒŽ‚v‚q.
003890          07 ŠJŽn˜a—ï‚v‚q              PIC 9(1) VALUE ZERO.
003900          07 ŠJŽn”N‚v‚q                PIC 9(2) VALUE ZERO.
003910          07 ŠJŽnŒŽ‚v‚q                PIC 9(2) VALUE ZERO.
003920       05 ŠJŽn“ú‚v‚q                   PIC 9(2) VALUE ZERO.
003930    03 I—¹˜a—ï”NŒŽ“ú‚v‚q.
003940       05 I—¹˜a—ï”NŒŽ‚v‚q.
003950          07 I—¹˜a—ï‚v‚q              PIC 9(1) VALUE ZERO.
003960          07 I—¹”N‚v‚q                PIC 9(2) VALUE ZERO.
003970          07 I—¹ŒŽ‚v‚q                PIC 9(2) VALUE ZERO.
003980       05 I—¹“ú‚v‚q                   PIC 9(2) VALUE ZERO.
004000    03 •ÛŒ¯Ží•Ê‚v‚q                    PIC 9(2) VALUE ZERO.
004010    03 Š³ŽÒƒR[ƒh‚v‚q.
004020       05 Š³ŽÒ”Ô†‚v‚q                 PIC 9(6) VALUE ZERO.
004030       05 Ž}”Ô‚v‚q                     PIC X(1) VALUE SPACE.
004040    03 –{l‰Æ‘°‹æ•ª‚v‚q                PIC 9(1) VALUE ZERO.
004050*
004060    03 ˆóüðŒ‚v‚q                    PIC 9(2) VALUE ZERO.
004070*
004080 01 Œö”ï•‰’SŽÒ”Ô†‚v.
004090    03 –@•Ê”Ô†‚v                      PIC X(2) VALUE SPACE.
004100    03 FILLER                          PIC X(8) VALUE SPACE.
004110 01 ‘Þ”ð€–Ú‚f‚v.
004120   03 ƒŒƒZƒvƒgŽí—Þ‚v                 PIC X(4).
004130   03 ƒŒƒZƒvƒgŽí—Þ‚f‚v               PIC X(4).
004140   03 ƒŒƒZƒvƒgŽí•Ê‚f‚v               PIC 9(2).
004150*
004160 01 –¾×.
004170    03 ‰ŒŸ—¿‚v‚q                    PIC 9(4)  VALUE ZERO.
004180    03 ‘Š’k—¿‚v‚q                    PIC 9(4)  VALUE ZERO.
004190    03 ‰ŒŸ‰ÁŽZ—¿‚v‚q                PIC 9(4)  VALUE ZERO.
004200    03 ‹x“ú‚v‚q                      PIC 9     VALUE ZERO.
004210    03 [–é‚v‚q                      PIC 9     VALUE ZERO.
004220    03 ŽžŠÔŠO‚v‚q                    PIC 9     VALUE ZERO.
004230    03 f—ÃŽž‚v‚q                    PIC 9(2)  VALUE ZERO.
004240    03 f—Ã•ª‚v‚q                    PIC 9(2)  VALUE ZERO.
          03 ‰ŒŸŽž‘Š’k—¿‚v‚q              PIC 9(4)  VALUE ZERO.
004250    03 ÄŒŸ—¿‚v‚q                    PIC 9(4)  VALUE ZERO.
004260    03 ‰—Ã–éŠÔ‚v‚q                  PIC 9     VALUE ZERO.
004270    03 ‰—Ã“ï˜H‚v‚q                  PIC 9     VALUE ZERO.
004280    03 ‰—Ã–\•—‚v‚q                  PIC 9     VALUE ZERO.
004290    03 ‰—Ã‰ñ”‚v‚q                  PIC 9(2)  VALUE ZERO.
004300    03 ‰—Ã‹——£‚v‚q                  PIC 9(3)V9 VALUE ZERO.
004310    03 ‰—Ã—¿‚v‚q                    PIC 9(6)  VALUE ZERO.
004320    03 ‰—Ã‰ÁŽZ—¿‚v‚q                PIC 9(5)  VALUE ZERO.
004330    03 “dãª—¿‚v‚q                    PIC 9(4)  VALUE ZERO.
004340    03 ˆê•”•‰’S‹à‚v‚q                PIC 9(5)  VALUE ZERO.
004350    03 ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q            PIC 9(5)  VALUE ZERO.
004330    03 ‰^“®—Ã–@—¿‚v‚q                PIC 9(4)  VALUE ZERO.
004360    03 Ž{pî•ñ’ñ‹Ÿ—¿‚v‚q            PIC 9(6)  VALUE ZERO.
004370    03 •”ˆÊ‚v                        OCCURS 7.
004380       05 ‰‰ñˆ’u—¿‚v‚q             PIC 9(4)  VALUE ZERO.
004390       05 Œã—Ã—¿‚v‚q                 PIC 9(4)  VALUE ZERO.
004400       05 —âãª—¿‚v‚q                 PIC 9(4)  VALUE ZERO.
004410       05 ‰·ãª—¿‚v‚q                 PIC 9(4)  VALUE ZERO.
004420       05 “d—Ã—¿‚v‚q                 PIC 9(4)  VALUE ZERO.
004430       05 •”ˆÊŒv‚v‚q                 PIC 9(4)  VALUE ZERO.
004440       05 ‹à‘®‚v‚q                   PIC 9     VALUE ZERO.
004450    03 “ú”‚v‚q                      PIC 9(2)  OCCURS 7 VALUE ZERO.
004460    03 —âãª–@—¿‚R‚O‚v‚q              PIC 9(4)  VALUE ZERO.
004470    03 —âãª–@—¿‚R‚W‚v‚q              PIC 9(4)  VALUE ZERO.
004480    03 —âãª–@—¿‚S‚O‚v‚q              PIC 9(4)  VALUE ZERO.
004490    03 —âãª–@—¿‚S‚T‚v‚q              PIC 9(4)  VALUE ZERO.
004500    03 —âãª–@—¿‚S‚W‚v‚q              PIC 9(4)  VALUE ZERO.
004480    03 —âãª–@—¿‚T‚O‚v‚q              PIC 9(4)  VALUE ZERO.
004480    03 —âãª–@—¿‚T‚Q‚v‚q              PIC 9(4)  VALUE ZERO.
004490    03 —âãª–@—¿‚T‚T‚v‚q              PIC 9(4)  VALUE ZERO.
004500    03 —âãª–@—¿‚T‚W‚v‚q              PIC 9(4)  VALUE ZERO.
004480    03 —âãª–@—¿‚U‚O‚v‚q              PIC 9(4)  VALUE ZERO.
004500    03 —âãª–@—¿‚U‚W‚v‚q              PIC 9(4)  VALUE ZERO.
004480    03 —âãª–@—¿‚V‚O‚v‚q              PIC 9(4)  VALUE ZERO.
004500    03 —âãª–@—¿‚V‚W‚v‚q              PIC 9(4)  VALUE ZERO.
004510    03 ‰·ãª–@—¿‚R‚O‚v‚q              PIC 9(4)  VALUE ZERO.
004520    03 ‰·ãª–@—¿‚R‚W‚v‚q              PIC 9(4)  VALUE ZERO.
004530    03 ‰·ãª–@—¿‚S‚O‚v‚q              PIC 9(4)  VALUE ZERO.
004540    03 ‰·ãª–@—¿‚S‚T‚v‚q              PIC 9(4)  VALUE ZERO.
004550    03 ‰·ãª–@—¿‚S‚W‚v‚q              PIC 9(4)  VALUE ZERO.
004480    03 ‰·ãª–@—¿‚T‚O‚v‚q              PIC 9(4)  VALUE ZERO.
004480    03 ‰·ãª–@—¿‚T‚Q‚v‚q              PIC 9(4)  VALUE ZERO.
004490    03 ‰·ãª–@—¿‚T‚T‚v‚q              PIC 9(4)  VALUE ZERO.
004500    03 ‰·ãª–@—¿‚T‚W‚v‚q              PIC 9(4)  VALUE ZERO.
004480    03 ‰·ãª–@—¿‚U‚O‚v‚q              PIC 9(4)  VALUE ZERO.
004500    03 ‰·ãª–@—¿‚U‚W‚v‚q              PIC 9(4)  VALUE ZERO.
004480    03 ‰·ãª–@—¿‚V‚O‚v‚q              PIC 9(4)  VALUE ZERO.
004500    03 ‰·ãª–@—¿‚V‚W‚v‚q              PIC 9(4)  VALUE ZERO.
004560    03 “d—Ã—¿‚R‚O‚v‚q                PIC 9(4)  VALUE ZERO.
004570    03 “d—Ã—¿‚R‚W‚v‚q                PIC 9(4)  VALUE ZERO.
004580    03 “d—Ã—¿‚S‚O‚v‚q                PIC 9(4)  VALUE ZERO.
004590    03 “d—Ã—¿‚S‚T‚v‚q                PIC 9(4)  VALUE ZERO.
004600    03 “d—Ã—¿‚S‚W‚v‚q                PIC 9(4)  VALUE ZERO.
004480    03 “d—Ã—¿‚T‚O‚v‚q                PIC 9(4)  VALUE ZERO.
004480    03 “d—Ã—¿‚T‚Q‚v‚q                PIC 9(4)  VALUE ZERO.
004490    03 “d—Ã—¿‚T‚T‚v‚q                PIC 9(4)  VALUE ZERO.
004500    03 “d—Ã—¿‚T‚W‚v‚q                PIC 9(4)  VALUE ZERO.
004480    03 “d—Ã—¿‚U‚O‚v‚q                PIC 9(4)  VALUE ZERO.
004500    03 “d—Ã—¿‚U‚W‚v‚q                PIC 9(4)  VALUE ZERO.
004480    03 “d—Ã—¿‚V‚O‚v‚q                PIC 9(4)  VALUE ZERO.
004500    03 “d—Ã—¿‚V‚W‚v‚q                PIC 9(4)  VALUE ZERO.
004610    03 Œã—Ã—¿‚R‚O‚v‚q                PIC 9(4)  VALUE ZERO.
004620    03 Œã—Ã—¿‚R‚W‚v‚q                PIC 9(4)  VALUE ZERO.
004630    03 Œã—Ã—¿‚S‚O‚v‚q                PIC 9(4)  VALUE ZERO.
004640    03 Œã—Ã—¿‚S‚T‚v‚q                PIC 9(4)  VALUE ZERO.
004650    03 Œã—Ã—¿‚S‚W‚v‚q                PIC 9(4)  VALUE ZERO.
004480    03 Œã—Ã—¿‚T‚O‚v‚q                PIC 9(4)  VALUE ZERO.
004480    03 Œã—Ã—¿‚T‚Q‚v‚q                PIC 9(4)  VALUE ZERO.
004490    03 Œã—Ã—¿‚T‚T‚v‚q                PIC 9(4)  VALUE ZERO.
004500    03 Œã—Ã—¿‚T‚W‚v‚q                PIC 9(4)  VALUE ZERO.
004480    03 Œã—Ã—¿‚U‚O‚v‚q                PIC 9(4)  VALUE ZERO.
004500    03 Œã—Ã—¿‚U‚W‚v‚q                PIC 9(4)  VALUE ZERO.
004480    03 Œã—Ã—¿‚V‚O‚v‚q                PIC 9(4)  VALUE ZERO.
004500    03 Œã—Ã—¿‚V‚W‚v‚q                PIC 9(4)  VALUE ZERO.
004660    03 —âãª—¿Œv‚v‚q                  PIC 9(4)  VALUE ZERO.
004670    03 ‰·ãª—¿Œv‚v‚q                  PIC 9(4)  VALUE ZERO.
004680    03 “d—Ã—¿Œv‚v‚q                  PIC 9(4)  VALUE ZERO.
002100*
       01 WŒv‚v.
001600    03 ‰ŒŸŒv‚v                      PIC 9(6)  VALUE ZERO.
          03 ®•œ‚v                        OCCURS 4.
001600       05 ®•œŒv‚v                   PIC 9(6)  VALUE ZERO.
001800    03 ‰—ÃŒv‚v                      PIC 9(6)  VALUE ZERO.
001810    03 ãª–@Œv‚v                      PIC 9(6)  VALUE ZERO.
001830    03 “d—ÃŒv‚v                      PIC 9(6)  VALUE ZERO.
001830    03 ”ï—pŒv‚v                      PIC 9(6)  VALUE ZERO.
004690*
004770** ŒŽ––“ú—p
004780 01 Ž{p¼—ï”N‚v                     PIC 9(4)  VALUE ZERO.
004790 01 ¤‚v                             PIC 9(3)  VALUE ZERO.
004800 01 —]‚v                             PIC 9(3)  VALUE ZERO.
004810*
004820*/ƒfƒoƒbƒN—p
004830 01 Žž.
004840   03 Žž PIC 9(2) VALUE ZERO.
004850   03 •ª PIC 9(2) VALUE ZERO.
004860   03 •b PIC 9(2) VALUE ZERO.
004870 01 Žž‚v PIC 9(6) VALUE ZERO.
004880******************************************************************
004890*                          ˜AŒ‹€–Ú                              *
004900******************************************************************
004910*
004920************
004930* “ü—ÍƒL[ *
004940************
004950*
004960 01 ˜A“ü|“ü—Íƒf[ƒ^‚x‚f‚m‚V‚Q‚O IS EXTERNAL.
004970    03 ˜A“ü|ŠJŽn˜a—ï”NŒŽ“ú.
004980       05 ˜A“ü|ŠJŽn˜a—ï                  PIC 9(1).
004990       05 ˜A“ü|ŠJŽn”N                    PIC 9(2).
005000       05 ˜A“ü|ŠJŽnŒŽ                    PIC 9(2).
005010       05 ˜A“ü|ŠJŽn“ú                    PIC 9(2).
005020    03 ˜A“ü|I—¹˜a—ï”NŒŽ“ú.
005030       05 ˜A“ü|I—¹˜a—ï                  PIC 9(1).
005040       05 ˜A“ü|I—¹”N                    PIC 9(2).
005050       05 ˜A“ü|I—¹ŒŽ                    PIC 9(2).
005060       05 ˜A“ü|I—¹“ú                    PIC 9(2).
005080    03 ˜A“ü|•ÛŒ¯Ží•Ê                     PIC 9(2).
005090    03 ˜A“ü|–{l‰Æ‘°‹æ•ª                 PIC 9(1).
005100    03 ˜A“ü|Š³ŽÒƒR[ƒh.
005110       05 ˜A“ü|Š³ŽÒ”Ô†                  PIC 9(6).
005120       05 ˜A“ü|Ž}”Ô                      PIC X(1).
005140    03 ˜A“ü|ˆóüðŒ                     PIC 9(2).
005420**********************
005430* ƒƒbƒZ[ƒW•\Ž¦ƒL[ *
005440**********************
005450 01 ˜Aƒ|ƒL[ IS EXTERNAL.
005460    03  ˜Aƒ|ƒƒbƒZ[ƒW                 PIC N(20).
005470**
005660******************************************************************
005670*                      PROCEDURE  DIVISION                       *
005680******************************************************************
005690 PROCEDURE               DIVISION.
005700************
005710*           *
005720* ‰Šúˆ—   *
005730*           *
005740************
005750     PERFORM ‰Šú‰».
005760************
005770*           *
005780* Žåˆ—     *
005790*           *
005800************
005810     MOVE ZERO TO ˆóü‚ ‚èƒtƒ‰ƒO.
005940     PERFORM ì‹Æƒtƒ@ƒCƒ‹ì¬
005970     IF ˆóü‚ ‚èƒtƒ‰ƒO = ZERO
006000         MOVE  NC"@ˆóü‚Ì‘ÎÛ‚É‚È‚éƒf[ƒ^‚ª‚ ‚è‚Ü‚¹‚ñB" TO ˜Aƒ|ƒƒbƒZ[ƒW
006010         CALL   "MSG001"
006020         CANCEL "MSG001"
006040         MOVE 01   TO PROGRAM-STATUS
006050     ELSE
006060         MOVE ZERO TO PROGRAM-STATUS
006080     END-IF.
006090************
006100*           *
006110* I—¹ˆ—   *
006120*           *
006130************
006140     PERFORM I—¹ˆ—.
006150*     MOVE ZERO TO PROGRAM-STATUS.
006160     EXIT PROGRAM.
006170*
006180*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
006190*================================================================*
006200 ‰Šú‰» SECTION.
006210*
006220     PERFORM ƒtƒ@ƒCƒ‹ƒI[ƒvƒ“.
006230* ˜AŒ‹€–Ú‚Ì‘Ò”ð
006240     INITIALIZE ‘Ò”ð€–Ú‚v‚q.
006260     MOVE ˜A“ü|•ÛŒ¯Ží•Ê      TO •ÛŒ¯Ží•Ê‚v‚q.
006270     MOVE ˜A“ü|Š³ŽÒ”Ô†      TO Š³ŽÒ”Ô†‚v‚q.
006280     MOVE ˜A“ü|Ž}”Ô          TO Ž}”Ô‚v‚q.
006290     MOVE ˜A“ü|ŠJŽn˜a—ï      TO ŠJŽn˜a—ï‚v‚q.
006300     MOVE ˜A“ü|ŠJŽn”N        TO ŠJŽn”N‚v‚q.
006310     MOVE ˜A“ü|ŠJŽnŒŽ        TO ŠJŽnŒŽ‚v‚q.
006320     MOVE ˜A“ü|ŠJŽn“ú        TO ŠJŽn“ú‚v‚q.
006330     MOVE ˜A“ü|I—¹˜a—ï      TO I—¹˜a—ï‚v‚q.
006340     MOVE ˜A“ü|I—¹”N        TO I—¹”N‚v‚q.
006350     MOVE ˜A“ü|I—¹ŒŽ        TO I—¹ŒŽ‚v‚q.
006360     MOVE ˜A“ü|I—¹“ú        TO I—¹“ú‚v‚q.
006370     MOVE ˜A“ü|–{l‰Æ‘°‹æ•ª  TO –{l‰Æ‘°‹æ•ª‚v‚q.
006380*
006390     MOVE ˜A“ü|ˆóüðŒ      TO ˆóüðŒ‚v‚q.
005650     PERFORM §Œäî•ñŽæ“¾.
005680*================================================================*
005690 §Œäî•ñŽæ“¾ SECTION.
005700*
005710     MOVE 01 TO §|§Œä‹æ•ª.
005720     READ §Œäî•ñƒ}ƒXƒ^
005730     NOT INVALID KEY
005740         MOVE §‚O‚P|ƒJƒ‹ƒe— —pŽ†Ží•Ê TO —pŽ†Ží•Ê‚v
005750     END-READ.
005760*
006400*================================================================*
006410 ƒtƒ@ƒCƒ‹ƒI[ƒvƒ“ SECTION.
006420*
006430     OPEN INPUT   Œ³†ƒ}ƒXƒ^
006440         MOVE NC"Œ³†" TO ƒtƒ@ƒCƒ‹–¼.
006450         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
006180     OPEN INPUT   §Œäî•ñƒ}ƒXƒ^
006190         MOVE NC"§Œäî•ñ" TO ƒtƒ@ƒCƒ‹–¼.
006200         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
006460     OPEN INPUT ŽófŽÒî•ñ‚e.
006470         MOVE NC"ŽófŽÒî•ñ‚e" TO ƒtƒ@ƒCƒ‹–¼.
006480         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
006490     OPEN INPUT •‰ƒf[ƒ^‚e.
006500         MOVE NC"•‰ƒf[ƒ^‚e" TO ƒtƒ@ƒCƒ‹–¼.
006510         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
006520     OPEN INPUT Ž{p‹L˜^‚e.
006530         MOVE NC"Ž{p‹L˜^‚e"   TO ƒtƒ@ƒCƒ‹–¼.
006540         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
006550     OPEN INPUT ƒƒ‚ƒtƒ@ƒCƒ‹.
006560         MOVE NC"ƒƒ‚"         TO ƒtƒ@ƒCƒ‹–¼.
006570         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
006580     OPEN INPUT ƒŒƒZƒvƒg‚e.
006590         MOVE NC"ƒŒƒZ"         TO ƒtƒ@ƒCƒ‹–¼.
006600         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
006610     OPEN INPUT ‰ïŒvƒf[ƒ^‚e.
006620         MOVE NC"‰ïŒv" TO ƒtƒ@ƒCƒ‹–¼.
006630         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
006640*================================================================*
006650 ƒI[ƒvƒ“ƒ`ƒFƒbƒN SECTION.
006660*
006670     IF ó‘ÔƒL[  NOT =  "00"
006680         DISPLAY ƒtƒ@ƒCƒ‹–¼ NC"‚eƒI[ƒvƒ“ƒGƒ‰[" UPON CONS
006690         DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[         UPON CONS
006700         DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢"
006710                                                 UPON CONS
006720*-----------------------------------------*
006730         CALL "actcshm"  WITH C LINKAGE
006740*-----------------------------------------*
006750         ACCEPT  ƒL[“ü—Í FROM CONS
006760         PERFORM ƒtƒ@ƒCƒ‹•Â½
006770         MOVE 99 TO PROGRAM-STATUS
006780         EXIT PROGRAM.
006790*================================================================*
006800 ƒtƒ@ƒCƒ‹•Â½ SECTION.
006810*
006820     CLOSE Œ³†ƒ}ƒXƒ^  ŽófŽÒî•ñ‚e •‰ƒf[ƒ^‚e Ž{p‹L˜^‚e
006830           ƒƒ‚ƒtƒ@ƒCƒ‹ ƒŒƒZƒvƒg‚e  ‰ïŒvƒf[ƒ^‚e §Œäî•ñƒ}ƒXƒ^.
006840*================================================================*
006850 I—¹ˆ— SECTION.
006860*
006870     PERFORM ƒtƒ@ƒCƒ‹•Â½.
006880*================================================================*
006890 ƒGƒ‰[•\Ž¦ SECTION.
006900*
006910     DISPLAY NC"ó‘ÔƒL[" ó‘ÔƒL[  UPON CONS.
006920     DISPLAY NC"‚Tƒtƒ@ƒCƒ‹‘žƒGƒ‰[F" ƒtƒ@ƒCƒ‹–¼   UPON CONS.
006930     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
006940     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢"                                                                    UPON CONS.
006950*-----------------------------------------*
006960     CALL "actcshm"  WITH C LINKAGE.
006970*-----------------------------------------*
006980     ACCEPT  ƒL[“ü—Í FROM CONS.
006990     PERFORM ƒtƒ@ƒCƒ‹•Â½.
007000     MOVE 99 TO PROGRAM-STATUS.
007010     EXIT PROGRAM.
007020*================================================================*
007030 ŽófŽÒî•ñ‚e“Çž SECTION.
007040*
007050     READ ŽófŽÒî•ñ‚e NEXT
007060     AT END
007070         MOVE "YES" TO I—¹ƒtƒ‰ƒO
007080     END-READ.
007090*================================================================*
007100 ì‹Æƒtƒ@ƒCƒ‹ì¬ SECTION.
007110*
007120     OPEN OUTPUT ì‹Æƒtƒ@ƒCƒ‹‚P
007130          MOVE NC"ì‚P" TO ƒtƒ@ƒCƒ‹–¼
007140          PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN
007150     OPEN OUTPUT ì‹Æƒtƒ@ƒCƒ‹‚Q
007160          MOVE NC"ì‚Q" TO ƒtƒ@ƒCƒ‹–¼
007170          PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN
007180*
007190     MOVE ŠJŽn˜a—ï‚v‚q      TO Žó|Ž{p˜a—ï.
007200     MOVE ŠJŽn”N‚v‚q        TO Žó|Ž{p”N.
007210     MOVE ŠJŽnŒŽ‚v‚q        TO Žó|Ž{pŒŽ.
007220     MOVE SPACE             TO Žó|Š³ŽÒƒJƒi.
007230     MOVE SPACE             TO Žó|Š³ŽÒƒR[ƒh.
007240     START ŽófŽÒî•ñ‚e   KEY IS >= Žó|Ž{p˜a—ï”NŒŽ
007250                                    Žó|Š³ŽÒƒJƒi
007260                                    Žó|Š³ŽÒƒR[ƒh
007270     END-START.
007280     IF ó‘ÔƒL[ = "00"
007290         MOVE SPACE  TO I—¹ƒtƒ‰ƒO
007300         PERFORM ŽófŽÒî•ñ‚e“Çž
007310         PERFORM UNTIL ( I—¹ƒtƒ‰ƒO = "YES" ) OR
007320                       ( Žó|Ž{p˜a—ï”NŒŽ > I—¹˜a—ï”NŒŽ‚v‚q )
007330*/ˆóü‘ÎÛðŒ‚Ì”»’è
007340                 IF ((•ÛŒ¯Ží•Ê‚v‚q     NOT = ZERO)              AND
007350                     (•ÛŒ¯Ží•Ê‚v‚q     NOT = Žó|•ÛŒ¯Ží•Ê))     OR
007360                    ((Š³ŽÒ”Ô†‚v‚q    NOT = ZERO)               AND
007370                     (Š³ŽÒƒR[ƒh‚v‚q   NOT = Žó|Š³ŽÒƒR[ƒh))   OR
007380                    ((–{l‰Æ‘°‹æ•ª‚v‚q NOT = ZERO)              AND
007390                     (–{l‰Æ‘°‹æ•ª‚v‚q NOT = Žó|–{l‰Æ‘°‹æ•ª)) OR
007400                     (Žó|•ÛŒ¯Ží•Ê         = 70 OR 80 OR 90)
007410*/‘ÎÛŠO
007420                     CONTINUE
007430                 ELSE
007440*/ˆóüðŒ‚v‚q©•Û—¯
007450                     MOVE "YES" TO ‘ÎÛƒtƒ‰ƒO
007460                     IF ˆóüðŒ‚v‚q NOT = ZERO
007470                         PERFORM ˆóüðŒ”»’è
007480                     END-IF
007490                     IF ‘ÎÛƒtƒ‰ƒO = "YES"
007500                         PERFORM ƒf[ƒ^ƒ`ƒFƒbƒN
007510                         IF ŽÀsƒL[‚v = "YES"
007520                             MOVE Žó|Ž{p˜a—ï”NŒŽ TO Ž{p˜a—ï”NŒŽ‚v‚q
007530                             IF Žó|Ž{p˜a—ï”NŒŽ = ŠJŽn˜a—ï”NŒŽ‚v‚q
007540                                 MOVE ŠJŽn“ú‚v‚q TO ŠJŽn“ú‚v‚o
007550                             ELSE
007560                                 MOVE 1          TO ŠJŽn“ú‚v‚o
007570                             END-IF
007580                             IF Žó|Ž{p˜a—ï”NŒŽ = I—¹˜a—ï”NŒŽ‚v‚q
007590                                 MOVE I—¹“ú‚v‚q TO I—¹“ú‚v‚o
007600                             ELSE
007610                                 PERFORM ŒŽ––“úŽæ“¾
007620                             END-IF
007630                             PERFORM ì‚PƒŒƒR[ƒhƒZƒbƒg
007640*/‘ÎÛƒf[ƒ^‚Ì‚ÝŒvŽZ‚·‚é‚æ‚¤‚É•ÏX
007650                             IF ˆóüƒtƒ‰ƒO = "YES"
007660                                 PERFORM ì‚QƒŒƒR[ƒhƒZƒbƒg
007670                             END-IF
                               END-IF
007690                     END-IF
007700                 END-IF
007710                 PERFORM ŽófŽÒî•ñ‚e“Çž
007720         END-PERFORM
007730     END-IF.
007740     CLOSE ì‹Æƒtƒ@ƒCƒ‹‚P ì‹Æƒtƒ@ƒCƒ‹‚Q.
007750*================================================================*
007760 ì‚PƒŒƒR[ƒhƒZƒbƒg SECTION.
007770*
007780     MOVE SPACE TO ˆóüƒtƒ‰ƒO.
007790     INITIALIZE ì‚P|ƒŒƒR[ƒh.
           INITIALIZE –¾×.
007860     MOVE Žó|Š³ŽÒƒR[ƒh   TO ì‚P|Š³ŽÒƒR[ƒh.
007870     MOVE Žó|Š³ŽÒƒJƒi     TO ì‚P|Š³ŽÒƒJƒi.
007880     MOVE Žó|Š³ŽÒŽ–¼     TO ì‚P|Š³ŽÒŽ–¼.
007890     PERFORM VARYING Ž{p“ú‚v‚q FROM ŠJŽn“ú‚v‚o BY 1
007900             UNTIL ( Ž{p“ú‚v‚q > I—¹“ú‚v‚o )
007910         INITIALIZE ì‚P|—¿‹à
007920         MOVE Žó|Š³ŽÒ”Ô†  TO ‰ï|Š³ŽÒ”Ô† Ž{‹L|Š³ŽÒ”Ô†
007930         MOVE Žó|Ž}”Ô      TO ‰ï|Ž}”Ô     Ž{‹L|Ž}”Ô
007940         MOVE Žó|Ž{p˜a—ï  TO ‰ï|Ž{p˜a—ï Ž{‹L|Ž{p˜a—ï
007950         MOVE Žó|Ž{p”N    TO ‰ï|Ž{p”N   Ž{‹L|Ž{p”N
007960         MOVE Žó|Ž{pŒŽ    TO ‰ï|Ž{pŒŽ   Ž{‹L|Ž{pŒŽ
007970         MOVE Ž{p“ú‚v‚q    TO ‰ï|Ž{p“ú   Ž{‹L|Ž{p“ú
007980         READ ‰ïŒvƒf[ƒ^‚e
007990         NOT INVALID KEY
008050             MOVE "YES" TO ˆóüƒtƒ‰ƒO
008060*
008390             READ Ž{p‹L˜^‚e
008420             END-READ
008630             PERFORM —¿‹àî•ñŽæ“¾
008080             PERFORM €–Ú‚²‚ÆŒvŽZ
008090             PERFORM ì‹Æƒtƒ@ƒCƒ‹ƒZƒbƒg
008100             PERFORM ì‚PƒŒƒR[ƒh‘ž
008110         END-READ
008120     END-PERFORM.
008290*================================================================*
008300 ì‹Æƒtƒ@ƒCƒ‹ƒZƒbƒg SECTION.
008310*
008320     MOVE ‰ï|Ž{p˜a—ï       TO ì‚P|Ž{p˜a—ï ƒƒ‚|Ž{p˜a—ï.
008330     MOVE ‰ï|Ž{p”N         TO ì‚P|Ž{p”N ƒƒ‚|Ž{p”N.
008340     MOVE ‰ï|Ž{pŒŽ         TO ì‚P|Ž{pŒŽ ƒƒ‚|Ž{pŒŽ.
008350     MOVE Ž{p“ú‚v‚q         TO ì‚P|Ž{p“ú ƒƒ‚|Ž{p“ú.
008370     MOVE 1                  TO ƒƒ‚|§Œä‹æ•ª.
008380     MOVE Žó|Š³ŽÒƒR[ƒh     TO ƒƒ‚|Š³ŽÒƒR[ƒh.
008390     READ ƒƒ‚ƒtƒ@ƒCƒ‹
008400     NOT INVALID KEY
008410        MOVE ƒƒ‚|Ž{pƒRƒƒ“ƒg TO ì‚P|ƒRƒƒ“ƒg
           END-READ.
008450*================================================================*
008460 ì‹Æ‚Qƒtƒ@ƒCƒ‹ƒZƒbƒg SECTION.
008470*
008480     MOVE ‰ï|Ž{p˜a—ï       TO ì‚Q|Ž{p˜a—ï.
008490     MOVE ‰ï|Ž{p”N         TO ì‚Q|Ž{p”N.
008500     MOVE ‰ï|Ž{pŒŽ         TO ì‚Q|Ž{pŒŽ.
           MOVE ‰ŒŸŒv‚v           TO ì‚Q|‰ŒŸ“™.
013060     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1
013070             UNTIL ( ƒJƒEƒ“ƒ^ > 4 )
               MOVE ®•œŒv‚v(ƒJƒEƒ“ƒ^)    TO ì‚Q|®•œ‹àŠz(ƒJƒEƒ“ƒ^)
           END-PERFORM.
           MOVE Ž{pî•ñ’ñ‹Ÿ—¿‚v‚q TO ì‚Q|‚»‚Ì‘¼.
           MOVE ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q TO ì‚Q|‹à‘®•›Žq.
           MOVE ãª–@Œv‚v           TO ì‚Q|ãª–@“™.
           MOVE ‰—ÃŒv‚v           TO ì‚Q|‰—Ã—¿.
008570*================================================================*
008580 ì‚QƒŒƒR[ƒhƒZƒbƒg SECTION.
008590*
008600     INITIALIZE ì‚Q|ƒŒƒR[ƒh.
           INITIALIZE WŒv‚v –¾×.
008620     MOVE Žó|Š³ŽÒƒR[ƒh   TO ì‚Q|Š³ŽÒƒR[ƒh.
           PERFORM VARYING Ž{p“ú‚v‚q FROM 1 BY 1
                   UNTIL ( Ž{p“ú‚v‚q > 31 )
               INITIALIZE ì‚P|—¿‹à
009770         MOVE Žó|Š³ŽÒ”Ô†  TO ‰ï|Š³ŽÒ”Ô†
009780         MOVE Žó|Ž}”Ô      TO ‰ï|Ž}”Ô
009790         MOVE Žó|Ž{p˜a—ï  TO ‰ï|Ž{p˜a—ï
009800         MOVE Žó|Ž{p”N    TO ‰ï|Ž{p”N
009810         MOVE Žó|Ž{pŒŽ    TO ‰ï|Ž{pŒŽ
009820         MOVE Ž{p“ú‚v‚q    TO ‰ï|Ž{p“ú
009830         READ ‰ïŒvƒf[ƒ^‚e
               NOT INVALID KEY
                   PERFORM WŒv
               END-READ
           END-PERFORM.
008630     PERFORM —¿‹àî•ñŽæ“¾‚Q.
008780     PERFORM ì‹Æ‚Qƒtƒ@ƒCƒ‹ƒZƒbƒg.
008790     PERFORM ì‚QƒŒƒR[ƒh‘ž.
010900*================================================================*
010910 —¿‹àî•ñŽæ“¾ SECTION.
010920*
010930***********************************************
010940* —¿‹àƒf[ƒ^ƒZƒbƒg                            *
010950***********************************************
010960*    ****************************************************************
010970*    * —¿‹àiŒŽ–ˆji•‰–ˆji’üŒ¸–ˆj‚É‚Â‚¢‚Ä‚Í˜AŒ‹€–Ú‚æ‚èƒZƒbƒg *
010980*    ****************************************************************
010990     MOVE ‰ï|‰ŒŸ—¿             TO  ‰ŒŸ—¿‚v‚q.
011000*
011010     MOVE ‰ï|‰ŒŸ‰ÁŽZ—¿         TO  ‰ŒŸ‰ÁŽZ—¿‚v‚q.
           MOVE ‰ï|‰ŒŸŽž‘Š’k—¿       TO  ‰ŒŸŽž‘Š’k—¿‚v‚q.
011050     MOVE ‰ï|ÄŒŸ—¿             TO  ÄŒŸ—¿‚v‚q.
011080     MOVE ‰ï|‰—Ã—¿             TO  ‰—Ã—¿‚v‚q.
011090     MOVE ‰ï|‰—Ã‰ÁŽZ—¿         TO  ‰—Ã‰ÁŽZ—¿‚v‚q.
011130*
010840     MOVE ‰ï|‰^“®Œã—Ã—¿         TO  ‰^“®—Ã–@—¿‚v‚q.
011150*
011160     MOVE ‰ï|Ž{pî•ñ’ñ‹Ÿ—¿     TO  ì‚P|‚»‚Ì‘¼.
      *
           IF —pŽ†Ží•Ê‚v = 2
011140        COMPUTE ì‚P|‹à‘®•›Žq = ‰ï|‹à‘®•›Žq‰ÁŽZ—¿ + ‰^“®—Ã–@—¿‚v‚q
              COMPUTE ì‚P|‰ŒŸ‹àŠz = ‰ŒŸ—¿‚v‚q + ‰ŒŸ‰ÁŽZ—¿‚v‚q + ÄŒŸ—¿‚v‚q + ‰ŒŸŽž‘Š’k—¿‚v‚q
           ELSE
011140        MOVE ‰ï|‹à‘®•›Žq‰ÁŽZ—¿     TO  ì‚P|‹à‘®•›Žq
              COMPUTE ì‚P|‰ŒŸ‹àŠz = ‰ŒŸ—¿‚v‚q + ‰ŒŸ‰ÁŽZ—¿‚v‚q + ÄŒŸ—¿‚v‚q + ‰ŒŸŽž‘Š’k—¿‚v‚q
                                     + ‰^“®—Ã–@—¿‚v‚q
           END-IF.
      *
           COMPUTE ì‚P|‰—Ã—¿   = ‰—Ã—¿‚v‚q + ‰—Ã‰ÁŽZ—¿‚v‚q.
011360*
           MOVE ‰ï|ˆê•”•‰’S‹à     TO ì‚P|ˆê•”•‰’S‹à.
008440     MOVE ‰ï|”ï—pŠz         TO ì‚P|”ï—pŠz.
010980*================================================================*
010990 —¿‹àî•ñŽæ“¾‚Q SECTION.
011000*
011010     IF Žó|•¬Ží•Ê NOT = ZERO
011020        MOVE  3   TO ƒŒƒZ|ƒŒƒZŽí•Ê
011030     ELSE
011040        IF Žó|Œö”ïŽí•Ê NOT = ZERO
011050           MOVE  2   TO ƒŒƒZ|ƒŒƒZŽí•Ê
011060        ELSE
011040           IF Žó|•ÛŒ¯Ží•Ê = 90
011070              MOVE  6   TO ƒŒƒZ|ƒŒƒZŽí•Ê
                 ELSE
011070              MOVE  1   TO ƒŒƒZ|ƒŒƒZŽí•Ê
011080           END-IF
011080        END-IF
011090     END-IF.
011100     MOVE Žó|Ž{p˜a—ï  TO ƒŒƒZ|Ž{p˜a—ï.
011110     MOVE Žó|Ž{p”N    TO ƒŒƒZ|Ž{p”N.  
011120     MOVE Žó|Ž{pŒŽ    TO ƒŒƒZ|Ž{pŒŽ.  
011130     MOVE Žó|Š³ŽÒ”Ô†  TO ƒŒƒZ|Š³ŽÒ”Ô†.
011140     MOVE Žó|Ž}”Ô      TO ƒŒƒZ|Ž}”Ô.    
011150     READ ƒŒƒZƒvƒg‚e
011160     NOT INVALID KEY
011180        MOVE ƒŒƒZ|‡Œv         TO ì‚Q|”ï—pŠz
011190        MOVE ƒŒƒZ|¿‹‹àŠz     TO ì‚Q|¿‹Šz
011200     END-READ.
013010*================================================================*
013020 €–Ú‚²‚ÆŒvŽZ SECTION.
013030***********************************************
013040* —¿‹àƒf[ƒ^ƒZƒbƒg                            *
013050***********************************************
013060     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1
013070             UNTIL ( ƒJƒEƒ“ƒ^ > 4 )
013080         MOVE ‰ï|‰‰ñˆ’u—¿(ƒJƒEƒ“ƒ^) TO ‰‰ñˆ’u—¿‚v‚q(ƒJƒEƒ“ƒ^)
013270     END-PERFORM.
013290*
015910     MOVE ‰ï|Œã—Ã—¿‚P     TO Œã—Ã—¿‚v‚q(1).
015920     MOVE ‰ï|Œã—Ã—¿‚Q     TO Œã—Ã—¿‚v‚q(2).
015930     MOVE ‰ï|Œã—Ã—¿‚R‚W   TO Œã—Ã—¿‚R‚W‚v‚q.
015940     MOVE ‰ï|Œã—Ã—¿‚R‚O   TO Œã—Ã—¿‚R‚O‚v‚q.
015950     COMPUTE Œã—Ã—¿‚v‚q(3)   = Œã—Ã—¿‚R‚W‚v‚q   + Œã—Ã—¿‚R‚O‚v‚q.
015960     MOVE ‰ï|Œã—Ã—¿‚S‚T   TO Œã—Ã—¿‚S‚T‚v‚q.
015970     MOVE ‰ï|Œã—Ã—¿‚S‚W   TO Œã—Ã—¿‚S‚W‚v‚q.
015980     MOVE ‰ï|Œã—Ã—¿‚S‚O   TO Œã—Ã—¿‚S‚O‚v‚q.
015990     COMPUTE Œã—Ã—¿‚v‚q(4)   = Œã—Ã—¿‚S‚T‚v‚q   + Œã—Ã—¿‚S‚W‚v‚q   + Œã—Ã—¿‚S‚O‚v‚q.
013060     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1
013070             UNTIL ( ƒJƒEƒ“ƒ^ > 4 )
013080         COMPUTE ì‚P|®•œ‹àŠz(ƒJƒEƒ“ƒ^) = ‰‰ñˆ’u—¿‚v‚q(ƒJƒEƒ“ƒ^) + Œã—Ã—¿‚v‚q(ƒJƒEƒ“ƒ^)
013270     END-PERFORM.
013400********************
013410* ’üŒ¸–ˆ—¿‹àƒZƒbƒg *
013420********************
013430     MOVE ‰ï|—âãª–@—¿‚P             TO —âãª—¿‚v‚q(1).
013440     MOVE ‰ï|—âãª–@—¿‚Q             TO —âãª—¿‚v‚q(2).
013450     MOVE ‰ï|—âãª–@—¿‚R‚W           TO —âãª–@—¿‚R‚W‚v‚q.
013460     MOVE ‰ï|—âãª–@—¿‚R‚O           TO —âãª–@—¿‚R‚O‚v‚q.
013470     COMPUTE —âãª—¿‚v‚q(3)   = —âãª–@—¿‚R‚W‚v‚q  + —âãª–@—¿‚R‚O‚v‚q.
013480     MOVE ‰ï|—âãª–@—¿‚S‚T           TO —âãª–@—¿‚S‚T‚v‚q.
013490     MOVE ‰ï|—âãª–@—¿‚S‚W           TO —âãª–@—¿‚S‚W‚v‚q.
013500     MOVE ‰ï|—âãª–@—¿‚S‚O           TO —âãª–@—¿‚S‚O‚v‚q.
013510     COMPUTE —âãª—¿‚v‚q(4)   = —âãª–@—¿‚S‚T‚v‚q  + —âãª–@—¿‚S‚W‚v‚q  + —âãª–@—¿‚S‚O‚v‚q.
013480     MOVE ‰ï|—âãª–@—¿‚T‚Q           TO —âãª–@—¿‚T‚Q‚v‚q.
013480     MOVE ‰ï|—âãª–@—¿‚T‚T           TO —âãª–@—¿‚T‚T‚v‚q.
013490     MOVE ‰ï|—âãª–@—¿‚T‚W           TO —âãª–@—¿‚T‚W‚v‚q.
013500     MOVE ‰ï|—âãª–@—¿‚T‚O           TO —âãª–@—¿‚T‚O‚v‚q.
013510     COMPUTE —âãª—¿‚v‚q(5)   = —âãª–@—¿‚T‚Q‚v‚q + —âãª–@—¿‚T‚T‚v‚q + —âãª–@—¿‚T‚W‚v‚q + —âãª–@—¿‚T‚O‚v‚q.
013450     MOVE ‰ï|—âãª–@—¿‚U‚W           TO —âãª–@—¿‚U‚W‚v‚q.
013460     MOVE ‰ï|—âãª–@—¿‚U‚O           TO —âãª–@—¿‚U‚O‚v‚q.
013470     COMPUTE —âãª—¿‚v‚q(6)   = —âãª–@—¿‚U‚W‚v‚q  + —âãª–@—¿‚U‚O‚v‚q.
013450     MOVE ‰ï|—âãª–@—¿‚V‚W           TO —âãª–@—¿‚V‚W‚v‚q.
013460     MOVE ‰ï|—âãª–@—¿‚V‚O           TO —âãª–@—¿‚V‚O‚v‚q.
013470     COMPUTE —âãª—¿‚v‚q(7)   = —âãª–@—¿‚V‚W‚v‚q  + —âãª–@—¿‚V‚O‚v‚q.
011690     COMPUTE —âãª—¿Œv‚v‚q = —âãª—¿‚v‚q(1) + —âãª—¿‚v‚q(2) + —âãª—¿‚v‚q(3) + —âãª—¿‚v‚q(4) +
                                  —âãª—¿‚v‚q(5) + —âãª—¿‚v‚q(6) + —âãª—¿‚v‚q(7).
013740*
013750     MOVE ‰ï|‰·ãª–@—¿‚P             TO ‰·ãª—¿‚v‚q(1).
013760     MOVE ‰ï|‰·ãª–@—¿‚Q             TO ‰·ãª—¿‚v‚q(2).
013770     MOVE ‰ï|‰·ãª–@—¿‚R‚W           TO ‰·ãª–@—¿‚R‚W‚v‚q.
013780     MOVE ‰ï|‰·ãª–@—¿‚R‚O           TO ‰·ãª–@—¿‚R‚O‚v‚q.
013790     COMPUTE ‰·ãª—¿‚v‚q(3)   = ‰·ãª–@—¿‚R‚W‚v‚q  + ‰·ãª–@—¿‚R‚O‚v‚q.
013800     MOVE ‰ï|‰·ãª–@—¿‚S‚T           TO ‰·ãª–@—¿‚S‚T‚v‚q.
013810     MOVE ‰ï|‰·ãª–@—¿‚S‚W           TO ‰·ãª–@—¿‚S‚W‚v‚q.
013820     MOVE ‰ï|‰·ãª–@—¿‚S‚O           TO ‰·ãª–@—¿‚S‚O‚v‚q.
013830     COMPUTE ‰·ãª—¿‚v‚q(4)   = ‰·ãª–@—¿‚S‚T‚v‚q  + ‰·ãª–@—¿‚S‚W‚v‚q  + ‰·ãª–@—¿‚S‚O‚v‚q.
013800     MOVE ‰ï|‰·ãª–@—¿‚T‚Q           TO ‰·ãª–@—¿‚T‚Q‚v‚q.
013800     MOVE ‰ï|‰·ãª–@—¿‚T‚T           TO ‰·ãª–@—¿‚T‚T‚v‚q.
013810     MOVE ‰ï|‰·ãª–@—¿‚T‚W           TO ‰·ãª–@—¿‚T‚W‚v‚q.
013820     MOVE ‰ï|‰·ãª–@—¿‚T‚O           TO ‰·ãª–@—¿‚T‚O‚v‚q.
013830     COMPUTE ‰·ãª—¿‚v‚q(5)   = ‰·ãª–@—¿‚T‚Q‚v‚q + ‰·ãª–@—¿‚T‚T‚v‚q + ‰·ãª–@—¿‚T‚W‚v‚q + ‰·ãª–@—¿‚T‚O‚v‚q.
013770     MOVE ‰ï|‰·ãª–@—¿‚U‚W           TO ‰·ãª–@—¿‚U‚W‚v‚q.
013780     MOVE ‰ï|‰·ãª–@—¿‚U‚O           TO ‰·ãª–@—¿‚U‚O‚v‚q.
013790     COMPUTE ‰·ãª—¿‚v‚q(6)   = ‰·ãª–@—¿‚U‚W‚v‚q  + ‰·ãª–@—¿‚U‚O‚v‚q.
013770     MOVE ‰ï|‰·ãª–@—¿‚V‚W           TO ‰·ãª–@—¿‚V‚W‚v‚q.
013780     MOVE ‰ï|‰·ãª–@—¿‚V‚O           TO ‰·ãª–@—¿‚V‚O‚v‚q.
013790     COMPUTE ‰·ãª—¿‚v‚q(7)   = ‰·ãª–@—¿‚V‚W‚v‚q  + ‰·ãª–@—¿‚V‚O‚v‚q.
011800     COMPUTE ‰·ãª—¿Œv‚v‚q = ‰·ãª—¿‚v‚q(1) + ‰·ãª—¿‚v‚q(2) + ‰·ãª—¿‚v‚q(3) + ‰·ãª—¿‚v‚q(4) +
                                  ‰·ãª—¿‚v‚q(5) + ‰·ãª—¿‚v‚q(6) + ‰·ãª—¿‚v‚q(7).
013860*
013870     MOVE ‰ï|“d—Ã—¿‚P             TO “d—Ã—¿‚v‚q(1).
013880     MOVE ‰ï|“d—Ã—¿‚Q             TO “d—Ã—¿‚v‚q(2).
013890     MOVE ‰ï|“d—Ã—¿‚R‚W           TO “d—Ã—¿‚R‚W‚v‚q.
013900     MOVE ‰ï|“d—Ã—¿‚R‚O           TO “d—Ã—¿‚R‚O‚v‚q.
013910     COMPUTE “d—Ã—¿‚v‚q(3)  = “d—Ã—¿‚R‚W‚v‚q  + “d—Ã—¿‚R‚O‚v‚q.
013920     MOVE ‰ï|“d—Ã—¿‚S‚T           TO “d—Ã—¿‚S‚T‚v‚q.
013930     MOVE ‰ï|“d—Ã—¿‚S‚W           TO “d—Ã—¿‚S‚W‚v‚q.
013940     MOVE ‰ï|“d—Ã—¿‚S‚O           TO “d—Ã—¿‚S‚O‚v‚q.
013950     COMPUTE “d—Ã—¿‚v‚q(4)  = “d—Ã—¿‚S‚T‚v‚q  + “d—Ã—¿‚S‚W‚v‚q  + “d—Ã—¿‚S‚O‚v‚q.
013920     MOVE ‰ï|“d—Ã—¿‚T‚Q           TO “d—Ã—¿‚T‚Q‚v‚q.
013920     MOVE ‰ï|“d—Ã—¿‚T‚T           TO “d—Ã—¿‚T‚T‚v‚q.
013930     MOVE ‰ï|“d—Ã—¿‚T‚W           TO “d—Ã—¿‚T‚W‚v‚q.
013940     MOVE ‰ï|“d—Ã—¿‚T‚O           TO “d—Ã—¿‚T‚O‚v‚q.
013950     COMPUTE “d—Ã—¿‚v‚q(5)  = “d—Ã—¿‚T‚Q‚v‚q + “d—Ã—¿‚T‚T‚v‚q + “d—Ã—¿‚T‚W‚v‚q + “d—Ã—¿‚T‚O‚v‚q.
013890     MOVE ‰ï|“d—Ã—¿‚U‚W           TO “d—Ã—¿‚U‚W‚v‚q.
013900     MOVE ‰ï|“d—Ã—¿‚U‚O           TO “d—Ã—¿‚U‚O‚v‚q.
013910     COMPUTE “d—Ã—¿‚v‚q(6)  = “d—Ã—¿‚U‚W‚v‚q  + “d—Ã—¿‚U‚O‚v‚q.
013890     MOVE ‰ï|“d—Ã—¿‚V‚W           TO “d—Ã—¿‚V‚W‚v‚q.
013900     MOVE ‰ï|“d—Ã—¿‚V‚O           TO “d—Ã—¿‚V‚O‚v‚q.
013910     COMPUTE “d—Ã—¿‚v‚q(7)  = “d—Ã—¿‚V‚W‚v‚q  + “d—Ã—¿‚V‚O‚v‚q.
           COMPUTE “d—Ã—¿Œv‚v‚q = “d—Ã—¿‚v‚q(1) + “d—Ã—¿‚v‚q(2) + “d—Ã—¿‚v‚q(3) + “d—Ã—¿‚v‚q(4) +
                                  “d—Ã—¿‚v‚q(5) + “d—Ã—¿‚v‚q(6) + “d—Ã—¿‚v‚q(7).
           COMPUTE ì‚P|ãª–@“™ = —âãª—¿Œv‚v‚q + ‰·ãª—¿Œv‚v‚q + “d—Ã—¿Œv‚v‚q.
013970*
011370*================================================================*
011380 WŒv SECTION.
011390***********************************************
011400* —¿‹àƒf[ƒ^ƒZƒbƒg                            *
011410***********************************************
010990     MOVE ‰ï|‰ŒŸ—¿             TO  ‰ŒŸ—¿‚v‚q.
011000*
011010     MOVE ‰ï|‰ŒŸ‰ÁŽZ—¿         TO  ‰ŒŸ‰ÁŽZ—¿‚v‚q.
           MOVE ‰ï|‰ŒŸŽž‘Š’k—¿       TO  ‰ŒŸŽž‘Š’k—¿‚v‚q.
011050     MOVE ‰ï|ÄŒŸ—¿             TO  ÄŒŸ—¿‚v‚q.
011080     MOVE ‰ï|‰—Ã—¿             TO  ‰—Ã—¿‚v‚q.
011090     MOVE ‰ï|‰—Ã‰ÁŽZ—¿         TO  ‰—Ã‰ÁŽZ—¿‚v‚q.
010840     MOVE ‰ï|‰^“®Œã—Ã—¿         TO  ‰^“®—Ã–@—¿‚v‚q.
           IF —pŽ†Ží•Ê‚v = 2
011140        COMPUTE ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q = ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q + ‰ï|‹à‘®•›Žq‰ÁŽZ—¿ + ‰^“®—Ã–@—¿‚v‚q
              COMPUTE ‰ŒŸŒv‚v = ‰ŒŸŒv‚v + ‰ŒŸ—¿‚v‚q + ‰ŒŸ‰ÁŽZ—¿‚v‚q + ÄŒŸ—¿‚v‚q + 
                                 ‰ŒŸŽž‘Š’k—¿‚v‚q
           ELSE
011140        COMPUTE ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q = ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q + ‰ï|‹à‘®•›Žq‰ÁŽZ—¿
              COMPUTE ‰ŒŸŒv‚v = ‰ŒŸŒv‚v + ‰ŒŸ—¿‚v‚q + ‰ŒŸ‰ÁŽZ—¿‚v‚q + ÄŒŸ—¿‚v‚q + 
                                 ‰ŒŸŽž‘Š’k—¿‚v‚q + ‰^“®—Ã–@—¿‚v‚q
           END-IF.
011160     COMPUTE Ž{pî•ñ’ñ‹Ÿ—¿‚v‚q = Ž{pî•ñ’ñ‹Ÿ—¿‚v‚q + ‰ï|Ž{pî•ñ’ñ‹Ÿ—¿.
           COMPUTE ‰—ÃŒv‚v = ‰—Ã—¿‚v‚q + ‰—Ã‰ÁŽZ—¿‚v‚q.
      *
011450     MOVE ‰ï|Œã—Ã—¿‚P     TO Œã—Ã—¿‚v‚q(1).
011460     MOVE ‰ï|Œã—Ã—¿‚Q     TO Œã—Ã—¿‚v‚q(2).
011470     MOVE ‰ï|Œã—Ã—¿‚R‚W   TO Œã—Ã—¿‚R‚W‚v‚q.
011480     MOVE ‰ï|Œã—Ã—¿‚R‚O   TO Œã—Ã—¿‚R‚O‚v‚q.
011490     COMPUTE Œã—Ã—¿‚v‚q(3)   = Œã—Ã—¿‚R‚W‚v‚q   + Œã—Ã—¿‚R‚O‚v‚q.
011500     MOVE ‰ï|Œã—Ã—¿‚S‚T   TO Œã—Ã—¿‚S‚T‚v‚q.
011510     MOVE ‰ï|Œã—Ã—¿‚S‚W   TO Œã—Ã—¿‚S‚W‚v‚q.
011520     MOVE ‰ï|Œã—Ã—¿‚S‚O   TO Œã—Ã—¿‚S‚O‚v‚q.
011530     COMPUTE Œã—Ã—¿‚v‚q(4)   = Œã—Ã—¿‚S‚T‚v‚q   + Œã—Ã—¿‚S‚W‚v‚q   + Œã—Ã—¿‚S‚O‚v‚q.
013060     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1
013070             UNTIL ( ƒJƒEƒ“ƒ^ > 4 )
013080         COMPUTE ®•œŒv‚v(ƒJƒEƒ“ƒ^) = ®•œŒv‚v(ƒJƒEƒ“ƒ^) + ‰ï|‰‰ñˆ’u—¿(ƒJƒEƒ“ƒ^) + Œã—Ã—¿‚v‚q(ƒJƒEƒ“ƒ^)
013270     END-PERFORM.
011570********************
011580* ’üŒ¸–ˆ—¿‹àƒZƒbƒg *
011590********************
011600     MOVE ‰ï|—âãª–@—¿‚P             TO —âãª—¿‚v‚q(1).
011610     MOVE ‰ï|—âãª–@—¿‚Q             TO —âãª—¿‚v‚q(2).
011620     MOVE ‰ï|—âãª–@—¿‚R‚W           TO —âãª–@—¿‚R‚W‚v‚q.
011630     MOVE ‰ï|—âãª–@—¿‚R‚O           TO —âãª–@—¿‚R‚O‚v‚q.
011640     COMPUTE —âãª—¿‚v‚q(3)   = —âãª–@—¿‚R‚W‚v‚q  + —âãª–@—¿‚R‚O‚v‚q.
011650     MOVE ‰ï|—âãª–@—¿‚S‚T           TO —âãª–@—¿‚S‚T‚v‚q.
011660     MOVE ‰ï|—âãª–@—¿‚S‚W           TO —âãª–@—¿‚S‚W‚v‚q.
011670     MOVE ‰ï|—âãª–@—¿‚S‚O           TO —âãª–@—¿‚S‚O‚v‚q.
011680     COMPUTE —âãª—¿‚v‚q(4)   = —âãª–@—¿‚S‚T‚v‚q  + —âãª–@—¿‚S‚W‚v‚q  + —âãª–@—¿‚S‚O‚v‚q.
013480     MOVE ‰ï|—âãª–@—¿‚T‚Q           TO —âãª–@—¿‚T‚Q‚v‚q.
013480     MOVE ‰ï|—âãª–@—¿‚T‚T           TO —âãª–@—¿‚T‚T‚v‚q.
013490     MOVE ‰ï|—âãª–@—¿‚T‚W           TO —âãª–@—¿‚T‚W‚v‚q.
013500     MOVE ‰ï|—âãª–@—¿‚T‚O           TO —âãª–@—¿‚T‚O‚v‚q.
013510     COMPUTE —âãª—¿‚v‚q(5)   = —âãª–@—¿‚T‚Q‚v‚q + —âãª–@—¿‚T‚T‚v‚q + —âãª–@—¿‚T‚W‚v‚q + —âãª–@—¿‚T‚O‚v‚q.
013450     MOVE ‰ï|—âãª–@—¿‚U‚W           TO —âãª–@—¿‚U‚W‚v‚q.
013460     MOVE ‰ï|—âãª–@—¿‚U‚O           TO —âãª–@—¿‚U‚O‚v‚q.
013470     COMPUTE —âãª—¿‚v‚q(6)   = —âãª–@—¿‚U‚W‚v‚q  + —âãª–@—¿‚U‚O‚v‚q.
013450     MOVE ‰ï|—âãª–@—¿‚V‚W           TO —âãª–@—¿‚V‚W‚v‚q.
013460     MOVE ‰ï|—âãª–@—¿‚V‚O           TO —âãª–@—¿‚V‚O‚v‚q.
013470     COMPUTE —âãª—¿‚v‚q(7)   = —âãª–@—¿‚V‚W‚v‚q  + —âãª–@—¿‚V‚O‚v‚q.
011690     COMPUTE —âãª—¿Œv‚v‚q = —âãª—¿‚v‚q(1) + —âãª—¿‚v‚q(2) + —âãª—¿‚v‚q(3) + —âãª—¿‚v‚q(4) +
                                  —âãª—¿‚v‚q(5) + —âãª—¿‚v‚q(6) + —âãª—¿‚v‚q(7).
011700*
011710     MOVE ‰ï|‰·ãª–@—¿‚P             TO ‰·ãª—¿‚v‚q(1).
011720     MOVE ‰ï|‰·ãª–@—¿‚Q             TO ‰·ãª—¿‚v‚q(2).
011730     MOVE ‰ï|‰·ãª–@—¿‚R‚W           TO ‰·ãª–@—¿‚R‚W‚v‚q.
011740     MOVE ‰ï|‰·ãª–@—¿‚R‚O           TO ‰·ãª–@—¿‚R‚O‚v‚q.
011750     COMPUTE ‰·ãª—¿‚v‚q(3)   = ‰·ãª–@—¿‚R‚W‚v‚q  + ‰·ãª–@—¿‚R‚O‚v‚q.
011760     MOVE ‰ï|‰·ãª–@—¿‚S‚T           TO ‰·ãª–@—¿‚S‚T‚v‚q.
011770     MOVE ‰ï|‰·ãª–@—¿‚S‚W           TO ‰·ãª–@—¿‚S‚W‚v‚q.
011780     MOVE ‰ï|‰·ãª–@—¿‚S‚O           TO ‰·ãª–@—¿‚S‚O‚v‚q.
011790     COMPUTE ‰·ãª—¿‚v‚q(4)   = ‰·ãª–@—¿‚S‚T‚v‚q  + ‰·ãª–@—¿‚S‚W‚v‚q  + ‰·ãª–@—¿‚S‚O‚v‚q.
013800     MOVE ‰ï|‰·ãª–@—¿‚T‚Q           TO ‰·ãª–@—¿‚T‚Q‚v‚q.
013800     MOVE ‰ï|‰·ãª–@—¿‚T‚T           TO ‰·ãª–@—¿‚T‚T‚v‚q.
013810     MOVE ‰ï|‰·ãª–@—¿‚T‚W           TO ‰·ãª–@—¿‚T‚W‚v‚q.
013820     MOVE ‰ï|‰·ãª–@—¿‚T‚O           TO ‰·ãª–@—¿‚T‚O‚v‚q.
013830     COMPUTE ‰·ãª—¿‚v‚q(5)   = ‰·ãª–@—¿‚T‚Q‚v‚q + ‰·ãª–@—¿‚T‚T‚v‚q + ‰·ãª–@—¿‚T‚W‚v‚q + ‰·ãª–@—¿‚T‚O‚v‚q.
013770     MOVE ‰ï|‰·ãª–@—¿‚U‚W           TO ‰·ãª–@—¿‚U‚W‚v‚q.
013780     MOVE ‰ï|‰·ãª–@—¿‚U‚O           TO ‰·ãª–@—¿‚U‚O‚v‚q.
013790     COMPUTE ‰·ãª—¿‚v‚q(6)   = ‰·ãª–@—¿‚U‚W‚v‚q  + ‰·ãª–@—¿‚U‚O‚v‚q.
013770     MOVE ‰ï|‰·ãª–@—¿‚V‚W           TO ‰·ãª–@—¿‚V‚W‚v‚q.
013780     MOVE ‰ï|‰·ãª–@—¿‚V‚O           TO ‰·ãª–@—¿‚V‚O‚v‚q.
013790     COMPUTE ‰·ãª—¿‚v‚q(7)   = ‰·ãª–@—¿‚V‚W‚v‚q  + ‰·ãª–@—¿‚V‚O‚v‚q.
011800     COMPUTE ‰·ãª—¿Œv‚v‚q = ‰·ãª—¿‚v‚q(1) + ‰·ãª—¿‚v‚q(2) + ‰·ãª—¿‚v‚q(3) + ‰·ãª—¿‚v‚q(4) +
                                  ‰·ãª—¿‚v‚q(5) + ‰·ãª—¿‚v‚q(6) + ‰·ãª—¿‚v‚q(7).
011810*
011820     MOVE ‰ï|“d—Ã—¿‚P             TO “d—Ã—¿‚v‚q(1).
011830     MOVE ‰ï|“d—Ã—¿‚Q             TO “d—Ã—¿‚v‚q(2).
011840     MOVE ‰ï|“d—Ã—¿‚R‚W           TO “d—Ã—¿‚R‚W‚v‚q.
011850     MOVE ‰ï|“d—Ã—¿‚R‚O           TO “d—Ã—¿‚R‚O‚v‚q.
011860     COMPUTE “d—Ã—¿‚v‚q(3)  = “d—Ã—¿‚R‚W‚v‚q  + “d—Ã—¿‚R‚O‚v‚q.
011870     MOVE ‰ï|“d—Ã—¿‚S‚T           TO “d—Ã—¿‚S‚T‚v‚q.
011880     MOVE ‰ï|“d—Ã—¿‚S‚W           TO “d—Ã—¿‚S‚W‚v‚q.
011890     MOVE ‰ï|“d—Ã—¿‚S‚O           TO “d—Ã—¿‚S‚O‚v‚q.
011900     COMPUTE “d—Ã—¿‚v‚q(4)  = “d—Ã—¿‚S‚T‚v‚q  + “d—Ã—¿‚S‚W‚v‚q  + “d—Ã—¿‚S‚O‚v‚q.
013920     MOVE ‰ï|“d—Ã—¿‚T‚Q           TO “d—Ã—¿‚T‚Q‚v‚q.
013920     MOVE ‰ï|“d—Ã—¿‚T‚T           TO “d—Ã—¿‚T‚T‚v‚q.
013930     MOVE ‰ï|“d—Ã—¿‚T‚W           TO “d—Ã—¿‚T‚W‚v‚q.
013940     MOVE ‰ï|“d—Ã—¿‚T‚O           TO “d—Ã—¿‚T‚O‚v‚q.
013950     COMPUTE “d—Ã—¿‚v‚q(5)  = “d—Ã—¿‚T‚Q‚v‚q + “d—Ã—¿‚T‚T‚v‚q + “d—Ã—¿‚T‚W‚v‚q + “d—Ã—¿‚T‚O‚v‚q.
013890     MOVE ‰ï|“d—Ã—¿‚U‚W           TO “d—Ã—¿‚U‚W‚v‚q.
013900     MOVE ‰ï|“d—Ã—¿‚U‚O           TO “d—Ã—¿‚U‚O‚v‚q.
013910     COMPUTE “d—Ã—¿‚v‚q(6)  = “d—Ã—¿‚U‚W‚v‚q  + “d—Ã—¿‚U‚O‚v‚q.
013890     MOVE ‰ï|“d—Ã—¿‚V‚W           TO “d—Ã—¿‚V‚W‚v‚q.
013900     MOVE ‰ï|“d—Ã—¿‚V‚O           TO “d—Ã—¿‚V‚O‚v‚q.
013910     COMPUTE “d—Ã—¿‚v‚q(7)  = “d—Ã—¿‚V‚W‚v‚q  + “d—Ã—¿‚V‚O‚v‚q.
           COMPUTE “d—ÃŒv‚v = “d—Ã—¿‚v‚q(1) + “d—Ã—¿‚v‚q(2) + “d—Ã—¿‚v‚q(3) + “d—Ã—¿‚v‚q(4) +
                              “d—Ã—¿‚v‚q(5) + “d—Ã—¿‚v‚q(6) + “d—Ã—¿‚v‚q(7).
           COMPUTE ãª–@Œv‚v = ãª–@Œv‚v + —âãª—¿Œv‚v‚q + ‰·ãª—¿Œv‚v‚q + “d—ÃŒv‚v.
011920*
           COMPUTE ”ï—pŒv‚v = ”ï—pŒv‚v + ‰ï|”ï—pŠz.
      *
018960*================================================================*
018970 ì‚PƒŒƒR[ƒh‘ž SECTION.
018980*
018990     WRITE ì‚P|ƒŒƒR[ƒh
019000     INVALID KEY
019010         MOVE NC"ì‚P"  TO ƒtƒ@ƒCƒ‹–¼
019020         PERFORM ƒGƒ‰[•\Ž¦
019030     END-WRITE.
019040     MOVE 1 TO ˆóü‚ ‚èƒtƒ‰ƒO.
019050*================================================================*
019060 ì‚QƒŒƒR[ƒh‘ž SECTION.
019070*
019080     WRITE ì‚Q|ƒŒƒR[ƒh
019090     INVALID KEY
019100         MOVE NC"ì‚Q"  TO ƒtƒ@ƒCƒ‹–¼
019110         PERFORM ƒGƒ‰[•\Ž¦
019120     END-WRITE.
019290*================================================================*
019300 ƒf[ƒ^ƒ`ƒFƒbƒN SECTION.
019310*
019320     MOVE SPACE          TO ŽÀsƒL[‚v.
019330* *****************************************************************
019340* * •‰•”ˆÊ—L–³ƒ`ƒFƒbƒNF•”ˆÊ” = 0 ‚Ìê‡ƒf[ƒ^ì¬‘ÎÛ‚Æ‚µ‚È‚¢ *
019350* *****************************************************************
019360     MOVE Žó|Ž{p˜a—ï   TO •‰|Ž{p˜a—ï.
019370     MOVE Žó|Ž{p”N     TO •‰|Ž{p”N.
019380     MOVE Žó|Ž{pŒŽ     TO •‰|Ž{pŒŽ.
019390     MOVE Žó|Š³ŽÒ”Ô†   TO •‰|Š³ŽÒ”Ô†.
019400     MOVE Žó|Ž}”Ô       TO •‰|Ž}”Ô.
019410     READ •‰ƒf[ƒ^‚e
019420     INVALID KEY
019430         MOVE SPACE  TO ŽÀsƒL[‚v
019440     NOT INVALID KEY
019450         IF •‰|•”ˆÊ” NOT = ZERO
019460*        *************************************************************
019470*        * Ž{p‹L˜^ƒ`ƒFƒbƒNF’Ê‰@” = 0 ‚Ìê‡ƒf[ƒ^ì¬‘ÎÛ‚Æ‚µ‚È‚¢ *
019480*        *************************************************************
019490             MOVE •‰|Š³ŽÒ”Ô†  TO Ž{‹L|Š³ŽÒ”Ô†
019500             MOVE •‰|Ž}”Ô      TO Ž{‹L|Ž}”Ô
019510             MOVE •‰|Ž{p˜a—ï  TO Ž{‹L|Ž{p˜a—ï
019520             MOVE •‰|Ž{p”N    TO Ž{‹L|Ž{p”N
019530             MOVE •‰|Ž{pŒŽ    TO Ž{‹L|Ž{pŒŽ
019540             MOVE ZERO          TO Ž{‹L|Ž{p“ú
019550             START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
019560                                          Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
019570             END-START
019580             IF ó‘ÔƒL[ = "00"
019590                 MOVE SPACE TO I—¹ƒtƒ‰ƒO‚Q
019600                 MOVE SPACE TO Ž{p‹L˜^—L‚v
019610                 PERFORM Ž{p‹L˜^‚e“Çž
019620                 PERFORM UNTIL (I—¹ƒtƒ‰ƒO‚Q         = "YES"         ) OR
019630                               (Ž{‹L|Š³ŽÒƒR[ƒh NOT = •‰|Š³ŽÒƒR[ƒh) OR
019640                               (Ž{‹L|Ž{p˜a—ï   NOT = •‰|Ž{p˜a—ï  ) OR
019650                               (Ž{‹L|Ž{p”N     NOT = •‰|Ž{p”N    ) OR
019660                               (Ž{‹L|Ž{pŒŽ     NOT = •‰|Ž{pŒŽ    ) OR
019670                               (Ž{p‹L˜^—L‚v         = "YES"         )
019680                     MOVE "YES"  TO Ž{p‹L˜^—L‚v
019690                     MOVE "YES"  TO ŽÀsƒL[‚v
019700                 END-PERFORM
019710             ELSE
019720                 MOVE SPACE  TO ŽÀsƒL[‚v
019730             END-IF
019740         ELSE
019750             MOVE SPACE  TO ŽÀsƒL[‚v
019760         END-IF
019770     END-READ.
019780*
019790*================================================================*
019800 Ž{p‹L˜^‚e“Çž SECTION.
019810*
019820     READ Ž{p‹L˜^‚e NEXT
019830     AT END
019840         MOVE "YES"  TO I—¹ƒtƒ‰ƒO‚Q
019850     END-READ.
019860*================================================================*
019870 ˆóüðŒ”»’è SECTION.
019880*
019890     MOVE SPACE TO ‘ÎÛƒtƒ‰ƒO
019900*/˜JÐŽ©”…Ó‚Ìƒf[ƒ^‚Í‘ÎÛŠO
019910     IF Žó|•ÛŒ¯Ží•Ê NOT = 70 AND 80
019920         EVALUATE ˆóüðŒ‚v‚q
019930         WHEN 01
019940             PERFORM ‰ŒŸ”»’è
019950         WHEN 02
019960             PERFORM Œã—Ã‚Ì‚Ý”»’è
019970         WHEN 03
019980             PERFORM I—¹”»’è
019990         WHEN 04
020000             PERFORM ‰ŒŸ”»’è
020010             IF ‘ÎÛƒtƒ‰ƒO = "YES"
020020                 PERFORM I—¹”»’è
020030             END-IF
020040         WHEN OTHER
020050             CONTINUE
020060         END-EVALUATE
020070     END-IF.
020080*================================================================*
020090 ‰ŒŸ”»’è SECTION.
020100*
020110     MOVE Žó|Š³ŽÒ”Ô†  TO Ž{‹L|Š³ŽÒ”Ô†
020120     MOVE Žó|Ž}”Ô      TO Ž{‹L|Ž}”Ô
020130     MOVE Žó|Ž{p˜a—ï  TO Ž{‹L|Ž{p˜a—ï
020140     MOVE Žó|Ž{p”N    TO Ž{‹L|Ž{p”N
020150     MOVE Žó|Ž{pŒŽ    TO Ž{‹L|Ž{pŒŽ
020160     MOVE ZERO          TO Ž{‹L|Ž{p“ú
020170     START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
020180                                  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
020190     END-START
020200     IF ó‘ÔƒL[ = "00"
020210         MOVE SPACE TO I—¹ƒtƒ‰ƒO‚Q
020220         PERFORM Ž{p‹L˜^‚e“Çž
020230         PERFORM UNTIL (I—¹ƒtƒ‰ƒO‚Q         = "YES"         ) OR
020240                       (Ž{‹L|Š³ŽÒƒR[ƒh NOT = Žó|Š³ŽÒƒR[ƒh) OR
020250                       (Ž{‹L|Ž{p˜a—ï   NOT = Žó|Ž{p˜a—ï  ) OR
020260                       (Ž{‹L|Ž{p”N     NOT = Žó|Ž{p”N    ) OR
020270                       (Ž{‹L|Ž{pŒŽ     NOT = Žó|Ž{pŒŽ    )
020280*
020290             IF Ž{‹L|f—Ã‹æ•ª = 2
020300                 MOVE "YES" TO ‘ÎÛƒtƒ‰ƒO
020310                 MOVE "YES" TO I—¹ƒtƒ‰ƒO‚Q
020320             END-IF
020330             PERFORM Ž{p‹L˜^‚e“Çž
020340         END-PERFORM
020350     END-IF.
020360*================================================================*
020370 Œã—Ã‚Ì‚Ý”»’è SECTION.
020380*
020390     MOVE Žó|Š³ŽÒ”Ô†  TO Ž{‹L|Š³ŽÒ”Ô†
020400     MOVE Žó|Ž}”Ô      TO Ž{‹L|Ž}”Ô
020410     MOVE Žó|Ž{p˜a—ï  TO Ž{‹L|Ž{p˜a—ï
020420     MOVE Žó|Ž{p”N    TO Ž{‹L|Ž{p”N
020430     MOVE Žó|Ž{pŒŽ    TO Ž{‹L|Ž{pŒŽ
020440     MOVE ZERO          TO Ž{‹L|Ž{p“ú
020450     START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
020460                                  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
020470     END-START
020480     IF ó‘ÔƒL[ = "00"
020490         MOVE SPACE TO I—¹ƒtƒ‰ƒO‚Q
020500         PERFORM Ž{p‹L˜^‚e“Çž
020510         PERFORM UNTIL (I—¹ƒtƒ‰ƒO‚Q         = "YES"         ) OR
020520                       (Ž{‹L|Š³ŽÒƒR[ƒh NOT = Žó|Š³ŽÒƒR[ƒh) OR
020530                       (Ž{‹L|Ž{p˜a—ï   NOT = Žó|Ž{p˜a—ï  ) OR
020540                       (Ž{‹L|Ž{p”N     NOT = Žó|Ž{p”N    ) OR
020550                       (Ž{‹L|Ž{pŒŽ     NOT = Žó|Ž{pŒŽ    )
020560*
020570             IF Ž{‹L|f—Ã‹æ•ª = 1
020580                 MOVE "YES" TO ‘ÎÛƒtƒ‰ƒO
020590             ELSE
020600                 MOVE SPACE TO ‘ÎÛƒtƒ‰ƒO
020610                 MOVE "YES" TO I—¹ƒtƒ‰ƒO‚Q
020620             END-IF
020630             PERFORM Ž{p‹L˜^‚e“Çž
020640         END-PERFORM
020650     END-IF.
020660*================================================================*
020670 I—¹”»’è SECTION.
020680*
020690     MOVE "YES" TO ‘ÎÛƒtƒ‰ƒO.
020700*
020710     MOVE Žó|Ž{p˜a—ï   TO •‰|Ž{p˜a—ï.
020720     MOVE Žó|Ž{p”N     TO •‰|Ž{p”N.
020730     MOVE Žó|Ž{pŒŽ     TO •‰|Ž{pŒŽ.
020740     MOVE Žó|Š³ŽÒ”Ô†   TO •‰|Š³ŽÒ”Ô†.
020750     MOVE Žó|Ž}”Ô       TO •‰|Ž}”Ô.
020760     READ •‰ƒf[ƒ^‚e
020770     INVALID KEY
020780         MOVE SPACE  TO ‘ÎÛƒtƒ‰ƒO
020790     NOT INVALID KEY
020800         IF •‰|•”ˆÊ” = ZERO
020810             MOVE SPACE TO ‘ÎÛƒtƒ‰ƒO
020820         ELSE
020830             PERFORM VARYING •”ˆÊ‚b‚m‚s FROM 1 BY 1
020840                      UNTIL (•”ˆÊ‚b‚m‚s > •‰|•”ˆÊ”)
020850                 IF •‰|“]‹A‹æ•ª(•”ˆÊ‚b‚m‚s) = 9 OR 5
020860                     MOVE SPACE TO ‘ÎÛƒtƒ‰ƒO
020870                 END-IF
020880             END-PERFORM
020890         END-IF
020900     END-READ.
020910*================================================================*
020920 ŒŽ––“úŽæ“¾ SECTION.
020930*
020940     MOVE Ž{p˜a—ï‚v‚q TO Œ³|Œ³†‹æ•ª.
020950     READ Œ³†ƒ}ƒXƒ^
020960     NOT INVALID KEY
020970         MOVE Œ³|ŠJŽn¼—ï”N TO Ž{p¼—ï”N‚v
020980     END-READ.
020990     IF Ž{p¼—ï”N‚v NOT = ZERO
021000        COMPUTE Ž{p¼—ï”N‚v = Ž{p¼—ï”N‚v + Ž{p”N‚v‚q - 1
021010     END-IF.
021020*
021030     EVALUATE Ž{pŒŽ‚v‚q
021040     WHEN 4
021050     WHEN 6
021060     WHEN 9
021070     WHEN 11
021080         MOVE 30 TO I—¹“ú‚v‚o
021090     WHEN 2
021100         DIVIDE 4 INTO Ž{p¼—ï”N‚v GIVING    ¤‚v
021110                                    REMAINDER —]‚v
021120         END-DIVIDE
021130         IF —]‚v = ZERO
021140             MOVE 29 TO I—¹“ú‚v‚o
021150         ELSE
021160             MOVE 28 TO I—¹“ú‚v‚o
021170         END-IF
021180     WHEN 1
021190     WHEN 3
021200     WHEN 5
021210     WHEN 7
021220     WHEN 8
021230     WHEN 10
021240     WHEN 12
021250         MOVE 31 TO I—¹“ú‚v‚o
021260     WHEN OTHER
021270          CONTINUE
021280     END-EVALUATE.
021290*
022250*================================================================*
022260******************************************************************
022270 END PROGRAM YGN721.
022280******************************************************************
