000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YIW721.
000060 AUTHOR.                 ’r“c@KŽq
000070*
000080*----------------------------------------------------------------*
000090*         Ž{p–¾×yÃÞ°Àì¬z_+³¨ÝÄÞ³½Þ”Å
000100*         MED = YIW720 
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
000370     SELECT  Œ³†ƒ}ƒXƒ^      ASSIGN      TO        GENGOUL
000380                             ORGANIZATION             IS  INDEXED
000390                             ACCESS MODE              IS  DYNAMIC
000400                             RECORD KEY               IS  Œ³|Œ³†‹æ•ª
000410                             FILE STATUS              IS  ó‘ÔƒL[
000420                             LOCK        MODE         IS  AUTOMATIC.
000260     SELECT  ŽófŽÒî•ñ‚e    ASSIGN      TO        JUSINJL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  Žó|Ž{p˜a—ï”NŒŽ
000300                                                          Žó|Š³ŽÒƒR[ƒh
000310                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000320                                                          Žó|Š³ŽÒƒJƒi
000330                                                          Žó|Š³ŽÒƒR[ƒh
000340                             ALTERNATE RECORD KEY     IS  Žó|Š³ŽÒƒR[ƒh
000350                                                          Žó|Ž{p˜a—ï”NŒŽ
000360                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000370                                                          Žó|•ÛŒ¯Ží•Ê
000380                                                          Žó|•ÛŒ¯ŽÒ”Ô†
000390                                                          Žó|Š³ŽÒƒR[ƒh
000400                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000410                                                          Žó|Œö”ïŽí•Ê
000420                                                          Žó|”ï—p•‰’SŽÒ”Ô†
000430                                                          Žó|Š³ŽÒƒR[ƒh
000440                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000450                                                          Žó|•¬Ží•Ê
000460                                                          Žó|”ï—p•‰’SŽÒ”Ô†•¬
000470                                                          Žó|Š³ŽÒƒR[ƒh
000480                             ALTERNATE RECORD KEY     IS  Žó|¿‹˜a—ï”NŒŽ
000490                                                          Žó|Ž{p˜a—ï”NŒŽ
000500                                                          Žó|Š³ŽÒƒR[ƒh
000510                             FILE STATUS              IS  ó‘ÔƒL[
000520                             LOCK        MODE         IS  AUTOMATIC.
           SELECT  •‰ƒf[ƒ^‚e    ASSIGN      TO        HUSYOUL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  •‰|Ž{p˜a—ï”NŒŽ
                                                                •‰|Š³ŽÒƒR[ƒh
                                   ALTERNATE RECORD KEY     IS  •‰|Š³ŽÒƒR[ƒh
                                                                •‰|Ž{p˜a—ï”NŒŽ
                                   FILE STATUS              IS  ó‘ÔƒL[
                                   LOCK        MODE         IS  AUTOMATIC.
           SELECT  Ž{p‹L˜^‚e      ASSIGN      TO      SEKIROKL
                                   ORGANIZATION        IS  INDEXED
                                   ACCESS MODE         IS  DYNAMIC
                                   RECORD KEY          IS  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
                                                           Ž{‹L|Š³ŽÒƒR[ƒh
                                   ALTERNATE RECORD KEY IS Ž{‹L|Š³ŽÒƒR[ƒh
                                                           Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
                                   FILE STATUS              IS  ó‘ÔƒL[
                                   LOCK        MODE         IS  AUTOMATIC.
000400     SELECT  ƒƒ‚ƒtƒ@ƒCƒ‹    ASSIGN      TO        MEMOL
000410                             ORGANIZATION             IS  INDEXED
000420                             ACCESS MODE              IS  DYNAMIC
000430                             RECORD KEY               IS  ƒƒ‚|§Œä‹æ•ª
                                                                ƒƒ‚|Š³ŽÒƒR[ƒh
                                                                ƒƒ‚|Ž{p˜a—ï”NŒŽ“ú
000360                             ALTERNATE RECORD KEY     IS  ƒƒ‚|§Œä‹æ•ª
                                                                ƒƒ‚|Ž{p˜a—ï”NŒŽ“ú
                                                                ƒƒ‚|Š³ŽÒƒR[ƒh
000360                             ALTERNATE RECORD KEY     IS  ƒƒ‚|Š³ŽÒƒR[ƒh
                                                                ƒƒ‚|Ž{p˜a—ï”NŒŽ“ú
                                                                ƒƒ‚|§Œä‹æ•ª
000440                             FILE STATUS              IS  ó‘ÔƒL[
000450                             LOCK        MODE         IS  AUTOMATIC.
000130     SELECT  ƒŒƒZƒvƒg‚e      ASSIGN      TO        RECEPTL
000140                             ORGANIZATION             IS  INDEXED
000150                             ACCESS MODE              IS  DYNAMIC
000160                             RECORD KEY               IS  ƒŒƒZ|Ž{p˜a—ï”NŒŽ
000170                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
000180                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
000190                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|Š³ŽÒƒR[ƒh
000200                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
000210                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
000220                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|¿‹˜a—ï”NŒŽ
000230                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
000240                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
000250                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
000260                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|¿‹˜a—ï”NŒŽ
000270                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
000280                                                          ƒŒƒZ|¿‹•ÛŒ¯ŽÒ”Ô†
000290                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
000300                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
000310                             ALTERNATE RECORD KEY     IS  ƒŒƒZ|¿‹˜a—ï”NŒŽ
000320                                                          ƒŒƒZ|¿‹•ÛŒ¯ŽÒ”Ô†
000330                                                          ƒŒƒZ|Š³ŽÒƒR[ƒh
000340                                                          ƒŒƒZ|ƒŒƒZŽí•Ê
000350                                                          ƒŒƒZ|Ž{p˜a—ï”NŒŽ
000360                             FILE STATUS              IS  ó‘ÔƒL[
000370                             LOCK        MODE         IS  AUTOMATIC.
000690     SELECT  ‰ïŒvƒf[ƒ^‚e    ASSIGN      TO        KAIKEIL
000700                             ORGANIZATION             IS  INDEXED
000710                             ACCESS MODE              IS  DYNAMIC
000089                             RECORD KEY               IS  ‰ï|Ž{p˜a—ï”NŒŽ“ú
000090                                                          ‰ï|Š³ŽÒƒR[ƒh
000092                             ALTERNATE RECORD KEY     IS  ‰ï|Š³ŽÒƒR[ƒh
000093                                                          ‰ï|Ž{p˜a—ï”NŒŽ“ú
000790                             FILE STATUS              IS  ó‘ÔƒL[
000800                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  ì‹Æƒtƒ@ƒCƒ‹‚P  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7211L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
                                   RECORD KEY               IS  ì‚P|Ž{p˜a—ï”NŒŽ“ú
000620                             FILE        STATUS       IS  ó‘ÔƒL[
000630                             LOCK        MODE         IS  AUTOMATIC.
000640******************************************************************
000650*                      DATA DIVISION                             *
000660******************************************************************
000670 DATA                    DIVISION.
000680 FILE                    SECTION.
001500*                           m‚q‚k  ‚P‚Q‚Wn
001510 FD  Œ³†ƒ}ƒXƒ^          BLOCK   CONTAINS   1   RECORDS.
001520     COPY GENGOU          OF  XFDLIB  JOINING   Œ³   AS  PREFIX.
000690*                           m‚q‚k  ‚R‚Q‚On
000700 FD  ŽófŽÒî•ñ‚e        BLOCK   CONTAINS   1   RECORDS.
000710     COPY JUSINJ          OF  XFDLIB  JOINING   Žó   AS  PREFIX.
      *                           m‚q‚k  ‚Q‚T‚Un
       FD  Ž{p‹L˜^‚e          BLOCK   CONTAINS   1   RECORDS.
          COPY SEKIROK         OF  XFDLIB  JOINING   Ž{‹L AS  PREFIX.
      *                           m‚q‚k  ‚P‚Q‚Wn
       FD  •‰ƒf[ƒ^‚e        BLOCK   CONTAINS   1   RECORDS.
           COPY HUSYOU          OF  XFDLIB  JOINING   •‰   AS  PREFIX.
000600*                           m‚q‚k  ‚W‚R‚Qn
000610 FD  ƒƒ‚ƒtƒ@ƒCƒ‹        BLOCK CONTAINS 1     RECORDS.
000620     COPY MEMO           OF    XFDLIB JOINING ƒƒ‚ AS PREFIX.
      *                          m‚q‚k  ‚P‚T‚R‚Un
       FD  ƒŒƒZƒvƒg‚e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ƒŒƒZ  AS  PREFIX.
001060*                           m‚q‚k  ‚T‚P‚Qn
001070 FD  ‰ïŒvƒf[ƒ^‚e        BLOCK   CONTAINS   1   RECORDS.
001080     COPY KAIKEI     OF  XFDLIB  JOINING   ‰ï   AS  PREFIX.
001310*****************
001320* ì‹Æƒtƒ@ƒCƒ‹‚P *
001330*****************
001340*                         m‚q‚k  ‚P‚U‚On
001350 FD  ì‹Æƒtƒ@ƒCƒ‹‚P RECORD  CONTAINS 160 CHARACTERS.
001360 01 ì‚P|ƒŒƒR[ƒh.
001370    03 ì‚P|ƒŒƒR[ƒhƒL[.
001535       05 ì‚P|Ž{p˜a—ï”NŒŽ“ú.
001536          07 ì‚P|Ž{p˜a—ï                PIC 9.
001537          07 ì‚P|Ž{p”NŒŽ.
001538             09 ì‚P|Ž{p”N               PIC 9(2).
001539             09 ì‚P|Ž{pŒŽ               PIC 9(2).
001540          07 ì‚P|Ž{p“ú                  PIC 9(2).
001490    03 ì‚P|ƒŒƒR[ƒhƒf[ƒ^.
             05 ì‚P|—¿‹à.
001550          07 ì‚P|‰ŒŸŽžŠÔ.
                   09 ì‚P|‰ŒŸŽž               PIC 9(2).
                   09 ì‚P|‰ŒŸ•ª               PIC 9(2).
001550          07 ì‚P|‰ŒŸ—¿                  PIC 9(5).
001550          07 ì‚P|Ž{—Ã—¿                  PIC 9(5).
001550          07 ì‚P|ÄŒŸ—¿                  PIC 9(5).
001550          07 ì‚P|Œã—Ã—¿                  PIC 9(5).
001550          07 ì‚P|‰—Ã—¿                  PIC 9(5).
001550          07 ì‚P|ãª–@—¿                  PIC 9(5).
001550          07 ì‚P|“d—Ã—¿                  PIC 9(5).
001551          07 ì‚P|”ï—pŠz                  PIC 9(5).
001551          07 ì‚P|ˆê•”•‰’S‹à              PIC 9(5).
001551       05 ì‚P|ƒRƒƒ“ƒg                   PIC X(100).
001470       05 ì‚P|‰ŒŸ                       PIC 9(1).
001500       05 FILLER                           PIC X(3).
      *
000930*----------------------------------------------------------------*
000940******************************************************************
000950*                WORKING-STORAGE SECTION                         *
000960******************************************************************
000970 WORKING-STORAGE         SECTION.
000980 01 ƒL[“ü—Í                           PIC X    VALUE SPACE.
000990 01 ó‘ÔƒL[                           PIC X(2) VALUE SPACE.
001000 01 I—¹ƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
002120 01 I—¹ƒtƒ‰ƒO‚Q                       PIC X(3) VALUE SPACE.
002120 01 I—¹ƒtƒ‰ƒO‚R                       PIC X(3) VALUE SPACE.
001010 01 ƒtƒ@ƒCƒ‹–¼                         PIC N(2) VALUE SPACE.
001180 01 ŽÀsƒL[‚v                         PIC X(4) VALUE SPACE.
       01 Ž{p‹L˜^—L‚v                       PIC X(3) VALUE SPACE.
001540 01 Œp‘±ƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
001540 01 ‘ÎÛƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
001891 01 •”ˆÊ‚b‚m‚s                         PIC 9    VALUE ZERO.
001891 01 ƒJƒEƒ“ƒ^                           PIC 9    VALUE ZERO.
001020* **************
001030* * €–Ú‘Ò”ð—p 
001040* **************
001050 01 ‘Ò”ð€–Ú‚v‚q.
001060    03 Ž{p˜a—ï”NŒŽ‚v‚q.
001070       05 Ž{p˜a—ï‚v‚q                 PIC 9(1) VALUE ZERO.
001080       05 Ž{p”N‚v‚q                   PIC 9(2) VALUE ZERO.
001090       05 Ž{pŒŽ‚v‚q                   PIC 9(2) VALUE ZERO.
001090    03 Ž{p“ú‚v‚q                      PIC 9(2) VALUE ZERO.
          03 ÅI’Ê‰@ŒŽ‚v                    PIC 9(2) VALUE ZERO.
001090    03 ÅI’Ê‰@“ú‚v                    PIC 9(2) VALUE ZERO.
          03 Žó—ŒŽ‚v                        PIC 9(2) VALUE ZERO.
001090    03 Žó—“ú‚v                        PIC 9(2) VALUE ZERO.
001090    03 ŠJŽn“ú‚v‚q                      PIC 9(2) VALUE ZERO.
001090    03 I—¹“ú‚v‚q                      PIC 9(2) VALUE ZERO.
001120    03 Š³ŽÒƒR[ƒh‚v‚q.
001130       05 Š³ŽÒ”Ô†‚v‚q                 PIC 9(6) VALUE ZERO.
001140       05 Ž}”Ô‚v‚q                     PIC X(1) VALUE SPACE.
001160*
001170 01 Œö”ï•‰’SŽÒ”Ô†‚v.
001180    03 –@•Ê”Ô†‚v                      PIC X(2) VALUE SPACE.
001190    03 FILLER                          PIC X(8) VALUE SPACE.
001530 01 ‘Þ”ð€–Ú‚f‚v.
001540   03 ƒŒƒZƒvƒgŽí—Þ‚v                 PIC X(4).
001550   03 ƒŒƒZƒvƒgŽí—Þ‚f‚v               PIC X(4).
001560   03 ƒŒƒZƒvƒgŽí•Ê‚f‚v               PIC 9(2).
001580*
001590 01 –¾×.
001600    03 ‰ŒŸ—¿‚v‚q                    PIC 9(6)  VALUE ZERO.
001610    03 ‰ŒŸ‰ÁŽZ—¿‚v‚q                PIC 9(5)  VALUE ZERO.
001620    03 ‹x“ú‚v‚q                      PIC 9     VALUE ZERO.
001630    03 [–é‚v‚q                      PIC 9     VALUE ZERO.
001640    03 ŽžŠÔŠO‚v‚q                    PIC 9     VALUE ZERO.
001650    03 f—ÃŽž‚v‚q                    PIC 9(2)  VALUE ZERO.
001660    03 f—Ã•ª‚v‚q                    PIC 9(2)  VALUE ZERO.
          03 ‰ŒŸŽž‘Š’k—¿‚v‚q              PIC 9(4)  VALUE ZERO.
001670    03 ÄŒŸ—¿‚v‚q                    PIC 9(4)  VALUE ZERO.
001680    03 ‰—Ã–éŠÔ‚v‚q                  PIC 9     VALUE ZERO.
001690    03 ‰—Ã“ï˜H‚v‚q                  PIC 9     VALUE ZERO.
001700    03 ‰—Ã–\•—‚v‚q                  PIC 9     VALUE ZERO.
001710    03 ‰—Ã‰ñ”‚v‚q                  PIC 9(2)  VALUE ZERO.
001720    03 ‰—Ã‹——£‚v‚q                  PIC 9(3)V9 VALUE ZERO.
001730    03 ‰—Ã—¿‚v‚q                    PIC 9(6)  VALUE ZERO.
001740    03 ‰—Ã‰ÁŽZ—¿‚v‚q                PIC 9(5)  VALUE ZERO.
001750    03 “dãª—¿‚v‚q                    PIC 9(4)  VALUE ZERO.
001760    03 ”ï—pŠz‚v‚q                    PIC 9(5)  VALUE ZERO.
001760    03 ˆê•”•‰’S‹à‚v‚q                PIC 9(5)  VALUE ZERO.
          03 –¾×‘”­s“ú‚v‚q              PIC 9(2)  VALUE ZERO.
          03 –¾×‘”­s—¿‚v‚q              PIC 9(3)  VALUE ZERO.
          03 ‹à‘®“ú‚v‚q                    PIC 9(2)  VALUE ZERO OCCURS 3.
          03 ‰^“®“ú‚v‚q                    PIC 9(2)  VALUE ZERO OCCURS 5.
004330    03 ‰^“®—Ã–@—¿‚v‚q                PIC 9(4)  VALUE ZERO.
003530    03 ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q            PIC 9(5)  VALUE ZERO.
          03 ‘å‚v                          PIC N(1)  VALUE SPACE.
          03 ’†‚v                          PIC N(1)  VALUE SPACE.
          03 ¬‚v                          PIC N(1)  VALUE SPACE.
001770    03 Ž{pî•ñ’ñ‹Ÿ—¿‚v‚q            PIC 9(6)  VALUE ZERO.
001780    03 •”ˆÊ‚v                        OCCURS 5.
001790       05 ‰‰ñˆ’u—¿‚v‚q             PIC 9(4)  VALUE ZERO.
001800       05 Œã—Ã—¿‚v‚q                 PIC 9(4)  VALUE ZERO.
001810       05 —âãª—¿‚v‚q                 PIC 9(4)  VALUE ZERO.
001820       05 ‰·ãª—¿‚v‚q                 PIC 9(4)  VALUE ZERO.
001830       05 “d—Ã—¿‚v‚q                 PIC 9(4)  VALUE ZERO.
001840       05 •”ˆÊŒv‚v‚q                 PIC 9(4)  VALUE ZERO.
001850       05 ‹à‘®‚v‚q                   PIC 9     VALUE ZERO.
001860    03 “ú”‚v‚q                      PIC 9(2)  OCCURS 5 VALUE ZERO.
001870    03 —âãª–@—¿‚R‚O‚v‚q              PIC 9(4)  VALUE ZERO.
001880    03 —âãª–@—¿‚R‚W‚v‚q              PIC 9(4)  VALUE ZERO.
001890    03 —âãª–@—¿‚S‚O‚v‚q              PIC 9(4)  VALUE ZERO.
001900    03 —âãª–@—¿‚S‚T‚v‚q              PIC 9(4)  VALUE ZERO.
001910    03 —âãª–@—¿‚S‚W‚v‚q              PIC 9(4)  VALUE ZERO.
001890    03 —âãª–@—¿‚T‚O‚v‚q              PIC 9(4)  VALUE ZERO.
001900    03 —âãª–@—¿‚T‚Q‚v‚q              PIC 9(4)  VALUE ZERO.
001900    03 —âãª–@—¿‚T‚T‚v‚q              PIC 9(4)  VALUE ZERO.
001910    03 —âãª–@—¿‚T‚W‚v‚q              PIC 9(4)  VALUE ZERO.
001920    03 ‰·ãª–@—¿‚R‚O‚v‚q              PIC 9(4)  VALUE ZERO.
001930    03 ‰·ãª–@—¿‚R‚W‚v‚q              PIC 9(4)  VALUE ZERO.
001940    03 ‰·ãª–@—¿‚S‚O‚v‚q              PIC 9(4)  VALUE ZERO.
001950    03 ‰·ãª–@—¿‚S‚T‚v‚q              PIC 9(4)  VALUE ZERO.
001960    03 ‰·ãª–@—¿‚S‚W‚v‚q              PIC 9(4)  VALUE ZERO.
001940    03 ‰·ãª–@—¿‚T‚O‚v‚q              PIC 9(4)  VALUE ZERO.
001950    03 ‰·ãª–@—¿‚T‚Q‚v‚q              PIC 9(4)  VALUE ZERO.
001950    03 ‰·ãª–@—¿‚T‚T‚v‚q              PIC 9(4)  VALUE ZERO.
001960    03 ‰·ãª–@—¿‚T‚W‚v‚q              PIC 9(4)  VALUE ZERO.
001970    03 “d—Ã—¿‚R‚O‚v‚q                PIC 9(4)  VALUE ZERO.
001980    03 “d—Ã—¿‚R‚W‚v‚q                PIC 9(4)  VALUE ZERO.
001990    03 “d—Ã—¿‚S‚O‚v‚q                PIC 9(4)  VALUE ZERO.
002000    03 “d—Ã—¿‚S‚T‚v‚q                PIC 9(4)  VALUE ZERO.
002010    03 “d—Ã—¿‚S‚W‚v‚q                PIC 9(4)  VALUE ZERO.
001990    03 “d—Ã—¿‚T‚O‚v‚q                PIC 9(4)  VALUE ZERO.
002000    03 “d—Ã—¿‚T‚Q‚v‚q                PIC 9(4)  VALUE ZERO.
002000    03 “d—Ã—¿‚T‚T‚v‚q                PIC 9(4)  VALUE ZERO.
002010    03 “d—Ã—¿‚T‚W‚v‚q                PIC 9(4)  VALUE ZERO.
002020    03 Œã—Ã—¿‚R‚O‚v‚q                PIC 9(4)  VALUE ZERO.
002030    03 Œã—Ã—¿‚R‚W‚v‚q                PIC 9(4)  VALUE ZERO.
002040    03 Œã—Ã—¿‚S‚O‚v‚q                PIC 9(4)  VALUE ZERO.
002050    03 Œã—Ã—¿‚S‚T‚v‚q                PIC 9(4)  VALUE ZERO.
002060    03 Œã—Ã—¿‚S‚W‚v‚q                PIC 9(4)  VALUE ZERO.
002040    03 Œã—Ã—¿‚T‚O‚v‚q                PIC 9(4)  VALUE ZERO.
002050    03 Œã—Ã—¿‚T‚Q‚v‚q                PIC 9(4)  VALUE ZERO.
002050    03 Œã—Ã—¿‚T‚T‚v‚q                PIC 9(4)  VALUE ZERO.
002060    03 Œã—Ã—¿‚T‚W‚v‚q                PIC 9(4)  VALUE ZERO.
002070    03 —âãª—¿Œv‚v‚q                  PIC 9(6)  VALUE ZERO.
002080    03 ‰·ãª—¿Œv‚v‚q                  PIC 9(6)  VALUE ZERO.
002070    03 ãª–@—¿Œv‚v‚q                  PIC 9(6)  VALUE ZERO.
002080    03 Œã—Ã—¿Œv‚v‚q                  PIC 9(6)  VALUE ZERO.
002090    03 “d—Ã—¿Œv‚v‚q                  PIC 9(6)  VALUE ZERO.
002100*
002149** ƒŒƒZ‰º’i‚Ì“ú•t‹æ•ª—p (0:ÅI’Ê‰@“úA1:ŒŽ––“úA9:ˆóŽš‚È‚µ)
002150 01 ƒŒƒZƒvƒg“ú•t‹æ•ª‚v                 PIC 9 VALUE ZERO.
002151 01 ƒŒƒZƒvƒgŠ³ŽÒ“ú•t‹æ•ª‚v             PIC 9 VALUE ZERO.
002617** ŒŽ––“ú—p
002618 01 Ž{p¼—ï”N‚v                       PIC 9(4)  VALUE ZERO.
002619 01 ¤‚v                               PIC 9(3)  VALUE ZERO.
002620 01 —]‚v                               PIC 9(3)  VALUE ZERO.
002621*
001200******************************************************************
001210*                          ˜AŒ‹€–Ú                              *
001220******************************************************************
001230*
003080****************
003090* ‰æ–Ê“ü—Íî•ñ *
003100****************
003110 01 ˜A“ü|“ü—Íƒf[ƒ^‚x‚h‚v‚V‚Q‚O IS EXTERNAL.
          03 ˜A“ü|Ž{p˜a—ï”NŒŽ.
             05 ˜A“ü|Ž{p˜a—ï                  PIC 9(1).
             05 ˜A“ü|Ž{p”N                    PIC 9(2).
             05 ˜A“ü|Ž{pŒŽ                    PIC 9(2).
          03 ˜A“ü|ŠJŽn“ú•t.
             05 ˜A“ü|ŠJŽn“ú                    PIC 9(2).
          03 ˜A“ü|I—¹“ú•t.
             05 ˜A“ü|I—¹“ú                    PIC 9(2).
          03 ˜A“ü|Š³ŽÒƒR[ƒh.
             05 ˜A“ü|Š³ŽÒ”Ô†                  PIC 9(6).
             05 ˜A“ü|Ž}”Ô                      PIC X(1).
          03 ˜A“ü|ˆóüƒ‚[ƒh‚e                 PIC 9(1).
          03 ˜A“ü|Ž–¼ƒ‚[ƒh                   PIC 9(1).
          03 ˜A“ü|”NŒŽƒ‚[ƒh                   PIC 9(1).
          03 ˜A“ü|“ú‚²‚Æƒ‚[ƒh                 PIC 9(1).
          03 ˜A“ü|ƒRƒƒ“ƒgƒ‚[ƒh               PIC 9(1).
          03 ˜A“ü|‡Œvƒ‚[ƒh                   PIC 9(1).
          03 ˜A“ü|ˆóü’i”                     PIC 9(2).
      */˜A‘±Žž‚ÌŽ–¼ˆóü‚Ì—L–³0302
          03 ˜A“ü|ˆ—ƒ‚[ƒh                   PIC X(4).
      *
002830 01 ˜Aˆó|‡Œvƒf[ƒ^‚x‚h‚v‚V‚Q‚O IS EXTERNAL.
          03 ˜Aˆó|Š³ŽÒŽ–¼                     PIC X(50).
          03 ˜Aˆó|‰ŒŸ—¿                       PIC 9(6).
          03 ˜Aˆó|Œã—Ã—¿                       PIC 9(6).
          03 ˜Aˆó|‰—Ã—¿                       PIC 9(6).
          03 ˜Aˆó|ãª–@—¿                       PIC 9(6).
          03 ˜Aˆó|“d—Ã—¿                       PIC 9(6).
          03 ˜Aˆó|”ï—pŠz                       PIC 9(7).
          03 ˜Aˆó|•‰’SŠz                       PIC 9(7).
          03 ˜Aˆó|î•ñ’ñ‹Ÿ—¿                   PIC 9(6).
          03 ˜Aˆó|‘å                           PIC N(1).
          03 ˜Aˆó|’†                           PIC N(1).
          03 ˜Aˆó|¬                           PIC N(1).
          03 ˜Aˆó|‹à‘®•›Žq—¿                   PIC 9(6).
          03 ˜Aˆó|‚»‚Ì‘¼                       PIC 9(6).
          03 ˜Aˆó|‘Š’k—¿                       PIC 9(4).
          03 ˜Aˆó|–¾×—¿                       PIC 9(3).
          03 ˜Aˆó|ƒRƒƒ“ƒg.
             05 ˜Aˆó|ƒRƒƒ“ƒg‚P                PIC X(100).
             05 ˜Aˆó|ƒRƒƒ“ƒg‚Q                PIC X(100).
007810*
001390******************************************************************
001400*                      PROCEDURE  DIVISION                       *
001410******************************************************************
001420 PROCEDURE               DIVISION.
001430************
001440*           *
001450* ‰Šúˆ—   *
001460*           *
001470************
001480     PERFORM ‰Šú‰».
           INITIALIZE ˜Aˆó|‡Œvƒf[ƒ^‚x‚h‚v‚V‚Q‚O.
001490************
001500*           *
001510* Žåˆ—     *
001520*           *
001530************
      *
001540     PERFORM ì‹Æƒtƒ@ƒCƒ‹ì¬.
001550************
001560*           *
001570* I—¹ˆ—   *
001580*           *
001590************
001600     PERFORM I—¹ˆ—.
001610     MOVE ZERO TO PROGRAM-STATUS.
001620     EXIT PROGRAM.
001630*
001640*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
001650*================================================================*
001660 ‰Šú‰» SECTION.
001670*
001680     PERFORM ƒtƒ@ƒCƒ‹ƒI[ƒvƒ“.
001690* ˜AŒ‹€–Ú‚Ì‘Ò”ð
001700     INITIALIZE ‘Ò”ð€–Ú‚v‚q.
001730     MOVE ˜A“ü|Š³ŽÒ”Ô†      TO Š³ŽÒ”Ô†‚v‚q.
001740     MOVE ˜A“ü|Ž}”Ô          TO Ž}”Ô‚v‚q.
001750     MOVE ˜A“ü|Ž{p˜a—ï      TO Ž{p˜a—ï‚v‚q.
001760     MOVE ˜A“ü|Ž{p”N        TO Ž{p”N‚v‚q.
001770     MOVE ˜A“ü|Ž{pŒŽ        TO Ž{pŒŽ‚v‚q.
001770     MOVE ˜A“ü|ŠJŽn“ú        TO ŠJŽn“ú‚v‚q.
001770     MOVE ˜A“ü|I—¹“ú        TO I—¹“ú‚v‚q.
      *
001790*================================================================*
001800 ƒtƒ@ƒCƒ‹ƒI[ƒvƒ“ SECTION.
001810*
012140     OPEN INPUT   Œ³†ƒ}ƒXƒ^
012150         MOVE NC"Œ³†" TO ƒtƒ@ƒCƒ‹–¼.
012160         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
001820     OPEN INPUT ŽófŽÒî•ñ‚e.
001830         MOVE NC"ŽófŽÒî•ñ‚e" TO ƒtƒ@ƒCƒ‹–¼.
001840         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
           OPEN INPUT •‰ƒf[ƒ^‚e.
               MOVE NC"•‰ƒf[ƒ^‚e" TO ƒtƒ@ƒCƒ‹–¼.
               PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
           OPEN INPUT Ž{p‹L˜^‚e.
               MOVE NC"Ž{p‹L˜^‚e"   TO ƒtƒ@ƒCƒ‹–¼.
               PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
002780     OPEN INPUT ƒƒ‚ƒtƒ@ƒCƒ‹.
002790         MOVE NC"ƒƒ‚"         TO ƒtƒ@ƒCƒ‹–¼.
002800         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
007870     OPEN INPUT ƒŒƒZƒvƒg‚e.
007880         MOVE NC"ƒŒƒZ"         TO ƒtƒ@ƒCƒ‹–¼.
007890         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
003060     OPEN INPUT ‰ïŒvƒf[ƒ^‚e.
003070         MOVE NC"‰ïŒv" TO ƒtƒ@ƒCƒ‹–¼.
003080         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
001850*================================================================*
001860 ƒI[ƒvƒ“ƒ`ƒFƒbƒN SECTION.
001870*
001880     IF ó‘ÔƒL[  NOT =  "00"
001890         DISPLAY ƒtƒ@ƒCƒ‹–¼ NC"‚eƒI[ƒvƒ“ƒGƒ‰[" UPON CONS
001900         DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[         UPON CONS
001910         DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢"
001920                                                 UPON CONS
003131*-----------------------------------------*
003132         CALL "actcshm"  WITH C LINKAGE
003133*-----------------------------------------*
001930         ACCEPT  ƒL[“ü—Í FROM CONS
001940         PERFORM ƒtƒ@ƒCƒ‹•Â½
001950         MOVE 99 TO PROGRAM-STATUS
001960         EXIT PROGRAM.
001970*================================================================*
001980 ƒtƒ@ƒCƒ‹•Â½ SECTION.
001990*
002000     CLOSE Œ³†ƒ}ƒXƒ^  ŽófŽÒî•ñ‚e •‰ƒf[ƒ^‚e Ž{p‹L˜^‚e
                 ƒƒ‚ƒtƒ@ƒCƒ‹ ƒŒƒZƒvƒg‚e ‰ïŒvƒf[ƒ^‚e.
002010*================================================================*
002020 I—¹ˆ— SECTION.
002030*
002040     PERFORM ƒtƒ@ƒCƒ‹•Â½.
002050*================================================================*
002060 ƒGƒ‰[•\Ž¦ SECTION.
002070*
002080     DISPLAY NC"ó‘ÔƒL[" ó‘ÔƒL[  UPON CONS.
002090     DISPLAY NC"‚Tƒtƒ@ƒCƒ‹‘žƒGƒ‰[F" ƒtƒ@ƒCƒ‹–¼   UPON CONS.
002100     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
002110     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢"                                                                    UPON CONS.
003131*-----------------------------------------*
003132     CALL "actcshm"  WITH C LINKAGE.
003133*-----------------------------------------*
002120     ACCEPT  ƒL[“ü—Í FROM CONS.
002130     PERFORM ƒtƒ@ƒCƒ‹•Â½.
002140     MOVE 99 TO PROGRAM-STATUS.
002150     EXIT PROGRAM.
003960*================================================================*
003970 ŽófŽÒî•ñ‚e“Çž SECTION.
003980*
003990     READ ŽófŽÒî•ñ‚e NEXT
004000     AT END
004010         MOVE "YES" TO I—¹ƒtƒ‰ƒO
004020     END-READ.
002160*================================================================*
002170 ì‹Æƒtƒ@ƒCƒ‹ì¬ SECTION.
002180*
002210     OPEN OUTPUT ì‹Æƒtƒ@ƒCƒ‹‚P
002220          MOVE NC"ì‚P" TO ƒtƒ@ƒCƒ‹–¼
002230          PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN
      *
003720     MOVE Ž{p˜a—ï‚v‚q      TO Žó|Ž{p˜a—ï.
003730     MOVE Ž{p”N‚v‚q        TO Žó|Ž{p”N.
003740     MOVE Ž{pŒŽ‚v‚q        TO Žó|Ž{pŒŽ.
           MOVE Š³ŽÒ”Ô†‚v‚q      TO Žó|Š³ŽÒ”Ô†.
           MOVE Ž}”Ô‚v‚q          TO Žó|Ž}”Ô.
003790     START ŽófŽÒî•ñ‚e   KEY IS >= Žó|Ž{p˜a—ï”NŒŽ
                                          Žó|Š³ŽÒƒR[ƒh
003830     IF ó‘ÔƒL[ = "00"
003850         PERFORM ŽófŽÒî•ñ‚e“Çž
009060         PERFORM ƒf[ƒ^ƒ`ƒFƒbƒN
009070         IF ŽÀsƒL[‚v = "YES"
009090             PERFORM VARYING Ž{p“ú‚v‚q FROM 1 BY 1 UNTIL Ž{p“ú‚v‚q > 31
009100                 INITIALIZE –¾×
                       MOVE Žó|Š³ŽÒ”Ô†  TO Ž{‹L|Š³ŽÒ”Ô†
                       MOVE Žó|Ž}”Ô      TO Ž{‹L|Ž}”Ô
                       MOVE Žó|Ž{p˜a—ï  TO Ž{‹L|Ž{p˜a—ï
                       MOVE Žó|Ž{p”N    TO Ž{‹L|Ž{p”N
                       MOVE Žó|Ž{pŒŽ    TO Ž{‹L|Ž{pŒŽ
                       MOVE Ž{p“ú‚v‚q    TO Ž{‹L|Ž{p“ú
                       READ Ž{p‹L˜^‚e
                       NOT INVALID KEY
008270                     INITIALIZE ì‚P|ƒŒƒR[ƒh
                           MOVE Ž{‹L|Žó•tŽž       TO ì‚P|‰ŒŸŽž
                           MOVE Ž{‹L|Žó•t•ª       TO ì‚P|‰ŒŸ•ª
                           IF Ž{‹L|f—Ã‹æ•ª = 2
                               MOVE 1              TO ì‚P|‰ŒŸ
                           ELSE
                               MOVE 0              TO ì‚P|‰ŒŸ
                           END-IF
009110                     PERFORM ƒŒƒZƒvƒgŒÄo‚µ‚Q
009130                     PERFORM €–Ú‚²‚ÆŒvŽZ
009140                     PERFORM ì‹Æƒtƒ@ƒCƒ‹ƒZƒbƒg
009150                     PERFORM ì‚PƒŒƒR[ƒh‘ž
                       END-READ
009170             END-PERFORM
009160             PERFORM ‡ŒvŒvŽZ
009190         END-IF
003950     END-IF.
002810     CLOSE ì‹Æƒtƒ@ƒCƒ‹‚P.
012730*================================================================*
012740 ƒŒƒZƒvƒgŒÄo‚µ‚P SECTION.
012750*
0           IF Žó|•¬Ží•Ê NOT = ZERO
              MOVE  3   TO ƒŒƒZ|ƒŒƒZŽí•Ê
           ELSE
              IF Žó|Œö”ïŽí•Ê NOT = ZERO
                 MOVE  2   TO ƒŒƒZ|ƒŒƒZŽí•Ê
              ELSE
                 IF Žó|•ÛŒ¯Ží•Ê = 85
                    MOVE  7   TO ƒŒƒZ|ƒŒƒZŽí•Ê
                 ELSE
                    MOVE  1   TO ƒŒƒZ|ƒŒƒZŽí•Ê
                 END-IF
              END-IF
           END-IF.
005200     MOVE Žó|Ž{p˜a—ï  TO ƒŒƒZ|Ž{p˜a—ï.
005210     MOVE Žó|Ž{p”N    TO ƒŒƒZ|Ž{p”N.  
005220     MOVE Žó|Ž{pŒŽ    TO ƒŒƒZ|Ž{pŒŽ.  
005230     MOVE Žó|Š³ŽÒ”Ô†  TO ƒŒƒZ|Š³ŽÒ”Ô†.
005240     MOVE Žó|Ž}”Ô      TO ƒŒƒZ|Ž}”Ô.    
           READ ƒŒƒZƒvƒg‚e
           INVALID KEY
               MOVE SPACE     TO ƒŒƒZ|ƒŒƒR[ƒh
           END-READ.
013080*
013090*================================================================*
013100 ƒŒƒZƒvƒgŒÄo‚µ‚Q SECTION.
013110*
009180     MOVE Žó|Š³ŽÒ”Ô†  TO ‰ï|Š³ŽÒ”Ô†.
009190     MOVE Žó|Ž}”Ô      TO ‰ï|Ž}”Ô.
009200     MOVE Žó|Ž{p˜a—ï  TO ‰ï|Ž{p˜a—ï.
009210     MOVE Žó|Ž{p”N    TO ‰ï|Ž{p”N.
009220     MOVE Žó|Ž{pŒŽ    TO ‰ï|Ž{pŒŽ.
009230     MOVE Ž{p“ú‚v‚q    TO ‰ï|Ž{p“ú.
009240     READ ‰ïŒvƒf[ƒ^‚e
           INVALID KEY
               MOVE SPACE     TO ‰ï|ƒŒƒR[ƒh
           END-READ.
009710*
009750     MOVE ‰ï|ˆê•”•‰’S‹à   TO ˆê•”•‰’S‹à‚v‚q.
013480     MOVE ‰ï|”ï—pŠz       TO ”ï—pŠz‚v‚q.
013490*
004030*================================================================*
004040 ì‹Æƒtƒ@ƒCƒ‹ƒZƒbƒg SECTION.
004050*
008280     MOVE Ž{p˜a—ï‚v‚q       TO ì‚P|Ž{p˜a—ï ƒƒ‚|Ž{p˜a—ï.
008290     MOVE Ž{p”N‚v‚q         TO ì‚P|Ž{p”N ƒƒ‚|Ž{p”N.
008300     MOVE Ž{pŒŽ‚v‚q         TO ì‚P|Ž{pŒŽ ƒƒ‚|Ž{pŒŽ.
008310     MOVE Ž{p“ú‚v‚q         TO ì‚P|Ž{p“ú ƒƒ‚|Ž{p“ú.
009320     MOVE ˆê•”•‰’S‹à‚v‚q     TO ì‚P|ˆê•”•‰’S‹à.
009330     MOVE ”ï—pŠz‚v‚q         TO ì‚P|”ï—pŠz.
           MOVE 1                  TO ƒƒ‚|§Œä‹æ•ª.
004130     MOVE Žó|Š³ŽÒƒR[ƒh     TO ƒƒ‚|Š³ŽÒƒR[ƒh.
           READ ƒƒ‚ƒtƒ@ƒCƒ‹
           NOT INVALID KEY
               MOVE ƒƒ‚|Ž{pƒRƒƒ“ƒg TO ì‚P|ƒRƒƒ“ƒg
           END-READ.
011370*================================================================*
011380 €–Ú‚²‚ÆŒvŽZ SECTION.
011390***********************************************
011400* —¿‹àƒf[ƒ^ƒZƒbƒg                            *
011410***********************************************
011130     COMPUTE ì‚P|‰ŒŸ—¿ = ‰ï|‰ŒŸ—¿ + ‰ï|‰ŒŸ‰ÁŽZ—¿.
      */Ž{—Ã—¿—“‚É‰^“®Œã—Ã—¿‚ðo‚· /180519
      *     MOVE ‰ï|‰‰ñˆ’u—¿‡Œv TO ì‚P|Ž{—Ã—¿.
           COMPUTE ì‚P|Ž{—Ã—¿ = ‰ï|‰‰ñˆ’u—¿‡Œv + ‰ï|‰^“®Œã—Ã—¿ + ‰ï|‹à‘®•›Žq‰ÁŽZ—¿.
015090     COMPUTE ì‚P|‰—Ã—¿ = ‰ï|‰—Ã—¿ + ‰ï|‰—Ã‰ÁŽZ—¿.
011130     MOVE ‰ï|‰—Ã‹——£     TO ‰—Ã‹——£‚v‚q.
           MOVE ‰ï|ÄŒŸ—¿       TO ì‚P|ÄŒŸ—¿.
011450     MOVE ‰ï|Œã—Ã—¿‚P     TO Œã—Ã—¿‚v‚q(1).
011460     MOVE ‰ï|Œã—Ã—¿‚Q     TO Œã—Ã—¿‚v‚q(2).
011470     MOVE ‰ï|Œã—Ã—¿‚R‚W   TO Œã—Ã—¿‚R‚W‚v‚q.
011480     MOVE ‰ï|Œã—Ã—¿‚R‚O   TO Œã—Ã—¿‚R‚O‚v‚q.
011490     COMPUTE Œã—Ã—¿‚v‚q(3)   = Œã—Ã—¿‚R‚W‚v‚q   + Œã—Ã—¿‚R‚O‚v‚q.
011500     MOVE ‰ï|Œã—Ã—¿‚S‚T   TO Œã—Ã—¿‚S‚T‚v‚q.
011510     MOVE ‰ï|Œã—Ã—¿‚S‚W   TO Œã—Ã—¿‚S‚W‚v‚q.
011520     MOVE ‰ï|Œã—Ã—¿‚S‚O   TO Œã—Ã—¿‚S‚O‚v‚q.
011530     COMPUTE Œã—Ã—¿‚v‚q(4)   = Œã—Ã—¿‚S‚T‚v‚q   + Œã—Ã—¿‚S‚W‚v‚q   + Œã—Ã—¿‚S‚O‚v‚q.
011500     MOVE ‰ï|Œã—Ã—¿‚T‚Q   TO Œã—Ã—¿‚T‚Q‚v‚q.
011500     MOVE ‰ï|Œã—Ã—¿‚T‚T   TO Œã—Ã—¿‚T‚T‚v‚q.
011510     MOVE ‰ï|Œã—Ã—¿‚T‚W   TO Œã—Ã—¿‚T‚W‚v‚q.
011520     MOVE ‰ï|Œã—Ã—¿‚T‚O   TO Œã—Ã—¿‚T‚O‚v‚q.
011530     COMPUTE Œã—Ã—¿‚v‚q(5)   = Œã—Ã—¿‚T‚Q‚v‚q + Œã—Ã—¿‚T‚T‚v‚q + Œã—Ã—¿‚T‚W‚v‚q + Œã—Ã—¿‚T‚O‚v‚q.
011530     COMPUTE ì‚P|Œã—Ã—¿ = Œã—Ã—¿‚v‚q(1) + Œã—Ã—¿‚v‚q(2) + Œã—Ã—¿‚v‚q(3) + Œã—Ã—¿‚v‚q(4) + Œã—Ã—¿‚v‚q(5).
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
011650     MOVE ‰ï|—âãª–@—¿‚T‚Q           TO —âãª–@—¿‚T‚Q‚v‚q.
011650     MOVE ‰ï|—âãª–@—¿‚T‚T           TO —âãª–@—¿‚T‚T‚v‚q.
011660     MOVE ‰ï|—âãª–@—¿‚T‚W           TO —âãª–@—¿‚T‚W‚v‚q.
011670     MOVE ‰ï|—âãª–@—¿‚T‚O           TO —âãª–@—¿‚T‚O‚v‚q.
011680     COMPUTE —âãª—¿‚v‚q(5)   = —âãª–@—¿‚T‚Q‚v‚q + —âãª–@—¿‚T‚T‚v‚q + —âãª–@—¿‚T‚W‚v‚q  + —âãª–@—¿‚T‚O‚v‚q.
011690     COMPUTE —âãª—¿Œv‚v‚q = —âãª—¿‚v‚q(1) + —âãª—¿‚v‚q(2) + —âãª—¿‚v‚q(3) + —âãª—¿‚v‚q(4) + —âãª—¿‚v‚q(5).
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
011760     MOVE ‰ï|‰·ãª–@—¿‚T‚Q           TO ‰·ãª–@—¿‚T‚Q‚v‚q.
011760     MOVE ‰ï|‰·ãª–@—¿‚T‚T           TO ‰·ãª–@—¿‚T‚T‚v‚q.
011770     MOVE ‰ï|‰·ãª–@—¿‚T‚W           TO ‰·ãª–@—¿‚T‚W‚v‚q.
011780     MOVE ‰ï|‰·ãª–@—¿‚T‚O           TO ‰·ãª–@—¿‚T‚O‚v‚q.
011790     COMPUTE ‰·ãª—¿‚v‚q(5)   = ‰·ãª–@—¿‚T‚Q‚v‚q + ‰·ãª–@—¿‚T‚T‚v‚q + ‰·ãª–@—¿‚T‚W‚v‚q + ‰·ãª–@—¿‚T‚O‚v‚q.
011800     COMPUTE ‰·ãª—¿Œv‚v‚q = ‰·ãª—¿‚v‚q(1) + ‰·ãª—¿‚v‚q(2) + ‰·ãª—¿‚v‚q(3) + ‰·ãª—¿‚v‚q(4) + ‰·ãª—¿‚v‚q(5).
           COMPUTE ì‚P|ãª–@—¿ = —âãª—¿Œv‚v‚q + ‰·ãª—¿Œv‚v‚q.
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
011870     MOVE ‰ï|“d—Ã—¿‚T‚Q           TO “d—Ã—¿‚T‚Q‚v‚q.
011870     MOVE ‰ï|“d—Ã—¿‚T‚T           TO “d—Ã—¿‚T‚T‚v‚q.
011880     MOVE ‰ï|“d—Ã—¿‚T‚W           TO “d—Ã—¿‚T‚W‚v‚q.
011890     MOVE ‰ï|“d—Ã—¿‚T‚O           TO “d—Ã—¿‚T‚O‚v‚q.
011900     COMPUTE “d—Ã—¿‚v‚q(5)  = “d—Ã—¿‚T‚Q‚v‚q + “d—Ã—¿‚T‚T‚v‚q + “d—Ã—¿‚T‚W‚v‚q + “d—Ã—¿‚T‚O‚v‚q.
           COMPUTE ì‚P|“d—Ã—¿ = “d—Ã—¿‚v‚q(1) + “d—Ã—¿‚v‚q(2) + “d—Ã—¿‚v‚q(3) + “d—Ã—¿‚v‚q(4) + “d—Ã—¿‚v‚q(5).
011920*
004140*================================================================*
004150 ì‚PƒŒƒR[ƒh‘ž SECTION.
004160*
004170     WRITE ì‚P|ƒŒƒR[ƒh
004180     INVALID KEY
004190         MOVE NC"ì‚P"  TO ƒtƒ@ƒCƒ‹–¼
004200         PERFORM ƒGƒ‰[•\Ž¦
004210     END-WRITE.
011020*================================================================*
011030 ‡ŒvŒvŽZ SECTION.
011040*
011050     PERFORM ƒŒƒZƒvƒgŒÄo‚µ‚P.
007720     PERFORM —¿‹àî•ñŽæ“¾.
007730     MOVE ‰ŒŸ—¿‚v‚q              TO ˜Aˆó|‰ŒŸ—¿.
007840     MOVE Œã—Ã—¿Œv‚v‚q            TO ˜Aˆó|Œã—Ã—¿.
007840     MOVE ‰—Ã—¿‚v‚q              TO ˜Aˆó|‰—Ã—¿.
007840     MOVE ãª–@—¿Œv‚v‚q            TO ˜Aˆó|ãª–@—¿.
007840     MOVE “d—Ã—¿Œv‚v‚q            TO ˜Aˆó|“d—Ã—¿.
011060     MOVE ƒŒƒZ|ˆê•”•‰’S‹à        TO ˜Aˆó|•‰’SŠz.
011110     MOVE ƒŒƒZ|‡Œv              TO ˜Aˆó|”ï—pŠz.
011110     MOVE Ž{pî•ñ’ñ‹Ÿ—¿‚v‚q      TO ˜Aˆó|î•ñ’ñ‹Ÿ—¿.
011110     MOVE ‘å‚v                    TO ˜Aˆó|‘å.
011110     MOVE ’†‚v                    TO ˜Aˆó|’†.
011110     MOVE ¬‚v                    TO ˜Aˆó|¬.
011110     MOVE ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q      TO ˜Aˆó|‹à‘®•›Žq—¿.
011110     COMPUTE ˜Aˆó|‚»‚Ì‘¼ = ‰ŒŸŽž‘Š’k—¿‚v‚q + –¾×‘”­s—¿‚v‚q.
011110     MOVE ‰ŒŸŽž‘Š’k—¿‚v‚q        TO ˜Aˆó|‘Š’k—¿.
011110     MOVE –¾×‘”­s—¿‚v‚q        TO ˜Aˆó|–¾×—¿.
      *
008280     MOVE Ž{p˜a—ï‚v‚q            TO ƒƒ‚|Ž{p˜a—ï.
008290     MOVE Ž{p”N‚v‚q              TO ƒƒ‚|Ž{p”N.
008300     MOVE Ž{pŒŽ‚v‚q              TO ƒƒ‚|Ž{pŒŽ.
008310     MOVE 99                      TO ƒƒ‚|Ž{p“ú.
           MOVE 1                       TO ƒƒ‚|§Œä‹æ•ª.
004130     MOVE Žó|Š³ŽÒƒR[ƒh          TO ƒƒ‚|Š³ŽÒƒR[ƒh.
           READ ƒƒ‚ƒtƒ@ƒCƒ‹
           NOT INVALID KEY
               MOVE ƒƒ‚|Ž{pƒRƒƒ“ƒg  TO ˜Aˆó|ƒRƒƒ“ƒg
           END-READ.
      *
           MOVE Žó|Š³ŽÒŽ–¼            TO ˜Aˆó|Š³ŽÒŽ–¼.
      *
010960*================================================================*
010970 —¿‹àî•ñŽæ“¾ SECTION.
010980*
011050     COMPUTE ‰ŒŸ—¿‚v‚q = ƒŒƒZ|‰ŒŸ—¿ + ƒŒƒZ|‰ŒŸ‰ÁŽZ—¿ +
                                ƒŒƒZ|‰‰ñˆ’u—¿‡Œv + ƒŒƒZ|ÄŒŸ—¿ +
                                ƒŒƒZ|‰^“®Œã—Ã—¿ + ƒŒƒZ|‹à‘®•›Žq‰ÁŽZ—¿.
           MOVE ƒŒƒZ|‰ŒŸŽž‘Š’k—¿           TO  ‰ŒŸŽž‘Š’k—¿‚v‚q.
           MOVE ƒŒƒZ|–¾×‘”­s‰ÁŽZ—¿       TO  –¾×‘”­s—¿‚v‚q.
011060*
011140     COMPUTE ‰—Ã—¿‚v‚q = ƒŒƒZ|‰—Ã—¿ + ƒŒƒZ|‰—Ã‰ÁŽZ—¿.
011190*
           IF Žó|Ž{p˜a—ï”NŒŽ < 43006
              IF ƒŒƒZ|‘å >= 1
                 MOVE NC"›"                    TO ‘å‚v
              END-IF
              IF ƒŒƒZ|’† >= 1
                 MOVE NC"›"                    TO ’†‚v
              END-IF
              IF ƒŒƒZ|¬ >= 1
                 MOVE NC"›"                    TO ¬‚v
              END-IF
           END-IF.
011200     MOVE ƒŒƒZ|‹à‘®•›Žq‰ÁŽZ—¿         TO  ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q.
011210*
011220     MOVE ƒŒƒZ|Ž{pî•ñ’ñ‹Ÿ—¿         TO  Ž{pî•ñ’ñ‹Ÿ—¿‚v‚q.
      *
011450     MOVE ƒŒƒZ|Œã—Ã—¿‚P     TO Œã—Ã—¿‚v‚q(1).
011460     MOVE ƒŒƒZ|Œã—Ã—¿‚Q     TO Œã—Ã—¿‚v‚q(2).
011470     MOVE ƒŒƒZ|Œã—Ã—¿‚R‚W   TO Œã—Ã—¿‚R‚W‚v‚q.
011480     MOVE ƒŒƒZ|Œã—Ã—¿‚R‚O   TO Œã—Ã—¿‚R‚O‚v‚q.
011490     COMPUTE Œã—Ã—¿‚v‚q(3)   = Œã—Ã—¿‚R‚W‚v‚q   + Œã—Ã—¿‚R‚O‚v‚q.
011500     MOVE ƒŒƒZ|Œã—Ã—¿‚S‚T   TO Œã—Ã—¿‚S‚T‚v‚q.
011510     MOVE ƒŒƒZ|Œã—Ã—¿‚S‚W   TO Œã—Ã—¿‚S‚W‚v‚q.
011520     MOVE ƒŒƒZ|Œã—Ã—¿‚S‚O   TO Œã—Ã—¿‚S‚O‚v‚q.
011530     COMPUTE Œã—Ã—¿‚v‚q(4)   = Œã—Ã—¿‚S‚T‚v‚q   + Œã—Ã—¿‚S‚W‚v‚q   + Œã—Ã—¿‚S‚O‚v‚q.
011500     MOVE ƒŒƒZ|Œã—Ã—¿‚T‚Q   TO Œã—Ã—¿‚T‚Q‚v‚q.
011500     MOVE ƒŒƒZ|Œã—Ã—¿‚T‚T   TO Œã—Ã—¿‚T‚T‚v‚q.
011510     MOVE ƒŒƒZ|Œã—Ã—¿‚T‚W   TO Œã—Ã—¿‚T‚W‚v‚q.
011520     MOVE ƒŒƒZ|Œã—Ã—¿‚T‚O   TO Œã—Ã—¿‚T‚O‚v‚q.
011530     COMPUTE Œã—Ã—¿‚v‚q(5)   = Œã—Ã—¿‚T‚Q‚v‚q + Œã—Ã—¿‚T‚T‚v‚q + Œã—Ã—¿‚T‚W‚v‚q + Œã—Ã—¿‚T‚O‚v‚q.
011530     COMPUTE Œã—Ã—¿Œv‚v‚q = Œã—Ã—¿‚v‚q(1) + Œã—Ã—¿‚v‚q(2) + Œã—Ã—¿‚v‚q(3) + Œã—Ã—¿‚v‚q(4) + Œã—Ã—¿‚v‚q(5).
      *
011330     MOVE ƒŒƒZ|—âãª–@—¿‚P             TO —âãª—¿‚v‚q(1).
011340     MOVE ƒŒƒZ|—âãª–@—¿‚Q             TO —âãª—¿‚v‚q(2).
011350     MOVE ƒŒƒZ|—âãª–@—¿‚R‚W           TO —âãª–@—¿‚R‚W‚v‚q.
011360     MOVE ƒŒƒZ|—âãª–@—¿‚R‚O           TO —âãª–@—¿‚R‚O‚v‚q.
011370     COMPUTE —âãª—¿‚v‚q(3)   = —âãª–@—¿‚R‚W‚v‚q  + —âãª–@—¿‚R‚O‚v‚q.
011380     MOVE ƒŒƒZ|—âãª–@—¿‚S‚T           TO —âãª–@—¿‚S‚T‚v‚q.
011390     MOVE ƒŒƒZ|—âãª–@—¿‚S‚W           TO —âãª–@—¿‚S‚W‚v‚q.
011400     MOVE ƒŒƒZ|—âãª–@—¿‚S‚O           TO —âãª–@—¿‚S‚O‚v‚q.
011410     COMPUTE —âãª—¿‚v‚q(4)   = —âãª–@—¿‚S‚T‚v‚q  + —âãª–@—¿‚S‚W‚v‚q  + —âãª–@—¿‚S‚O‚v‚q.
011650     MOVE ƒŒƒZ|—âãª–@—¿‚T‚Q           TO —âãª–@—¿‚T‚Q‚v‚q.
011650     MOVE ƒŒƒZ|—âãª–@—¿‚T‚T           TO —âãª–@—¿‚T‚T‚v‚q.
011660     MOVE ƒŒƒZ|—âãª–@—¿‚T‚W           TO —âãª–@—¿‚T‚W‚v‚q.
011670     MOVE ƒŒƒZ|—âãª–@—¿‚T‚O           TO —âãª–@—¿‚T‚O‚v‚q.
011680     COMPUTE —âãª—¿‚v‚q(5)   = —âãª–@—¿‚T‚Q‚v‚q + —âãª–@—¿‚T‚T‚v‚q + —âãª–@—¿‚T‚W‚v‚q  + —âãª–@—¿‚T‚O‚v‚q.
011690     COMPUTE —âãª—¿Œv‚v‚q = —âãª—¿‚v‚q(1) + —âãª—¿‚v‚q(2) + —âãª—¿‚v‚q(3) + —âãª—¿‚v‚q(4) + —âãª—¿‚v‚q(5).
011700*
011710     MOVE ƒŒƒZ|‰·ãª–@—¿‚P             TO ‰·ãª—¿‚v‚q(1).
011720     MOVE ƒŒƒZ|‰·ãª–@—¿‚Q             TO ‰·ãª—¿‚v‚q(2).
011730     MOVE ƒŒƒZ|‰·ãª–@—¿‚R‚W           TO ‰·ãª–@—¿‚R‚W‚v‚q.
011740     MOVE ƒŒƒZ|‰·ãª–@—¿‚R‚O           TO ‰·ãª–@—¿‚R‚O‚v‚q.
011750     COMPUTE ‰·ãª—¿‚v‚q(3)   = ‰·ãª–@—¿‚R‚W‚v‚q  + ‰·ãª–@—¿‚R‚O‚v‚q.
011760     MOVE ƒŒƒZ|‰·ãª–@—¿‚S‚T           TO ‰·ãª–@—¿‚S‚T‚v‚q.
011770     MOVE ƒŒƒZ|‰·ãª–@—¿‚S‚W           TO ‰·ãª–@—¿‚S‚W‚v‚q.
011780     MOVE ƒŒƒZ|‰·ãª–@—¿‚S‚O           TO ‰·ãª–@—¿‚S‚O‚v‚q.
011790     COMPUTE ‰·ãª—¿‚v‚q(4)   = ‰·ãª–@—¿‚S‚T‚v‚q  + ‰·ãª–@—¿‚S‚W‚v‚q  + ‰·ãª–@—¿‚S‚O‚v‚q.
011760     MOVE ƒŒƒZ|‰·ãª–@—¿‚T‚Q           TO ‰·ãª–@—¿‚T‚Q‚v‚q.
011760     MOVE ƒŒƒZ|‰·ãª–@—¿‚T‚T           TO ‰·ãª–@—¿‚T‚T‚v‚q.
011770     MOVE ƒŒƒZ|‰·ãª–@—¿‚T‚W           TO ‰·ãª–@—¿‚T‚W‚v‚q.
011780     MOVE ƒŒƒZ|‰·ãª–@—¿‚T‚O           TO ‰·ãª–@—¿‚T‚O‚v‚q.
011790     COMPUTE ‰·ãª—¿‚v‚q(5)   = ‰·ãª–@—¿‚T‚Q‚v‚q + ‰·ãª–@—¿‚T‚T‚v‚q + ‰·ãª–@—¿‚T‚W‚v‚q + ‰·ãª–@—¿‚T‚O‚v‚q.
011800     COMPUTE ‰·ãª—¿Œv‚v‚q = ‰·ãª—¿‚v‚q(1) + ‰·ãª—¿‚v‚q(2) + ‰·ãª—¿‚v‚q(3) + ‰·ãª—¿‚v‚q(4) + ‰·ãª—¿‚v‚q(5).
           COMPUTE ãª–@—¿Œv‚v‚q = —âãª—¿Œv‚v‚q + ‰·ãª—¿Œv‚v‚q.
011810*
011820     MOVE ƒŒƒZ|“d—Ã—¿‚P             TO “d—Ã—¿‚v‚q(1).
011830     MOVE ƒŒƒZ|“d—Ã—¿‚Q             TO “d—Ã—¿‚v‚q(2).
011840     MOVE ƒŒƒZ|“d—Ã—¿‚R‚W           TO “d—Ã—¿‚R‚W‚v‚q.
011850     MOVE ƒŒƒZ|“d—Ã—¿‚R‚O           TO “d—Ã—¿‚R‚O‚v‚q.
011860     COMPUTE “d—Ã—¿‚v‚q(3)  = “d—Ã—¿‚R‚W‚v‚q  + “d—Ã—¿‚R‚O‚v‚q.
011870     MOVE ƒŒƒZ|“d—Ã—¿‚S‚T           TO “d—Ã—¿‚S‚T‚v‚q.
011880     MOVE ƒŒƒZ|“d—Ã—¿‚S‚W           TO “d—Ã—¿‚S‚W‚v‚q.
011890     MOVE ƒŒƒZ|“d—Ã—¿‚S‚O           TO “d—Ã—¿‚S‚O‚v‚q.
011900     COMPUTE “d—Ã—¿‚v‚q(4)  = “d—Ã—¿‚S‚T‚v‚q  + “d—Ã—¿‚S‚W‚v‚q  + “d—Ã—¿‚S‚O‚v‚q.
011870     MOVE ƒŒƒZ|“d—Ã—¿‚T‚Q           TO “d—Ã—¿‚T‚Q‚v‚q.
011870     MOVE ƒŒƒZ|“d—Ã—¿‚T‚T           TO “d—Ã—¿‚T‚T‚v‚q.
011880     MOVE ƒŒƒZ|“d—Ã—¿‚T‚W           TO “d—Ã—¿‚T‚W‚v‚q.
011890     MOVE ƒŒƒZ|“d—Ã—¿‚T‚O           TO “d—Ã—¿‚T‚O‚v‚q.
011900     COMPUTE “d—Ã—¿‚v‚q(5)  = “d—Ã—¿‚T‚Q‚v‚q + “d—Ã—¿‚T‚T‚v‚q + “d—Ã—¿‚T‚W‚v‚q + “d—Ã—¿‚T‚O‚v‚q.
           COMPUTE “d—Ã—¿Œv‚v‚q = “d—Ã—¿‚v‚q(1) + “d—Ã—¿‚v‚q(2) + “d—Ã—¿‚v‚q(3) + “d—Ã—¿‚v‚q(4) + “d—Ã—¿‚v‚q(5).
011920*
      *================================================================*
       ƒf[ƒ^ƒ`ƒFƒbƒN SECTION.
      *
           MOVE SPACE          TO ŽÀsƒL[‚v.
      * *****************************************************************
      * * •‰•”ˆÊ—L–³ƒ`ƒFƒbƒNF•”ˆÊ” = 0 ‚Ìê‡ƒf[ƒ^ì¬‘ÎÛ‚Æ‚µ‚È‚¢ *
      * *****************************************************************
           MOVE Žó|Ž{p˜a—ï   TO •‰|Ž{p˜a—ï.
           MOVE Žó|Ž{p”N     TO •‰|Ž{p”N.
           MOVE Žó|Ž{pŒŽ     TO •‰|Ž{pŒŽ.
           MOVE Žó|Š³ŽÒ”Ô†   TO •‰|Š³ŽÒ”Ô†.
           MOVE Žó|Ž}”Ô       TO •‰|Ž}”Ô.
           READ •‰ƒf[ƒ^‚e
           INVALID KEY
               MOVE SPACE  TO ŽÀsƒL[‚v
           NOT INVALID KEY
               IF •‰|•”ˆÊ” NOT = ZERO
      *        *************************************************************
      *        * Ž{p‹L˜^ƒ`ƒFƒbƒNF’Ê‰@” = 0 ‚Ìê‡ƒf[ƒ^ì¬‘ÎÛ‚Æ‚µ‚È‚¢ *
      *        *************************************************************
                   MOVE •‰|Š³ŽÒ”Ô†  TO Ž{‹L|Š³ŽÒ”Ô†
                   MOVE •‰|Ž}”Ô      TO Ž{‹L|Ž}”Ô
                   MOVE •‰|Ž{p˜a—ï  TO Ž{‹L|Ž{p˜a—ï
                   MOVE •‰|Ž{p”N    TO Ž{‹L|Ž{p”N
                   MOVE •‰|Ž{pŒŽ    TO Ž{‹L|Ž{pŒŽ
                   MOVE ZERO          TO Ž{‹L|Ž{p“ú
                   START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
                                                Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
                   END-START
                   IF ó‘ÔƒL[ = "00"
                       MOVE SPACE TO I—¹ƒtƒ‰ƒO‚Q
                       MOVE SPACE TO Ž{p‹L˜^—L‚v
                       PERFORM Ž{p‹L˜^‚e“Çž
                       PERFORM UNTIL (I—¹ƒtƒ‰ƒO‚Q         = "YES"         ) OR
                                     (Ž{‹L|Š³ŽÒƒR[ƒh NOT = •‰|Š³ŽÒƒR[ƒh) OR
                                     (Ž{‹L|Ž{p˜a—ï   NOT = •‰|Ž{p˜a—ï  ) OR
                                     (Ž{‹L|Ž{p”N     NOT = •‰|Ž{p”N    ) OR
                                     (Ž{‹L|Ž{pŒŽ     NOT = •‰|Ž{pŒŽ    ) OR
                                     (Ž{p‹L˜^—L‚v         = "YES"         )
                           MOVE "YES"  TO Ž{p‹L˜^—L‚v
                           MOVE "YES"  TO ŽÀsƒL[‚v
                       END-PERFORM
                   ELSE
                       MOVE SPACE  TO ŽÀsƒL[‚v
                   END-IF
               ELSE
                   MOVE SPACE  TO ŽÀsƒL[‚v
               END-IF
           END-READ.
      *
      *================================================================*
       Ž{p‹L˜^‚e“Çž SECTION.
      *
           READ Ž{p‹L˜^‚e NEXT
           AT END
               MOVE "YES"  TO I—¹ƒtƒ‰ƒO‚Q
           END-READ.
027377*================================================================*
027378 ŒŽ––“úŽæ“¾ SECTION.
027379*
027382     MOVE Ž{p˜a—ï‚v‚q TO Œ³|Œ³†‹æ•ª.
027383     READ Œ³†ƒ}ƒXƒ^
027384     NOT INVALID KEY
027385         MOVE Œ³|ŠJŽn¼—ï”N TO Ž{p¼—ï”N‚v
027386     END-READ.
027387     IF Ž{p¼—ï”N‚v NOT = ZERO
027388        COMPUTE Ž{p¼—ï”N‚v = Ž{p¼—ï”N‚v + Ž{p”N‚v‚q - 1
027389     END-IF.
027390*
           MOVE Ž{pŒŽ‚v‚q   TO Žó—ŒŽ‚v.
027391     EVALUATE Ž{pŒŽ‚v‚q
027392     WHEN 4
027393     WHEN 6
027394     WHEN 9
027395     WHEN 11
027396         MOVE 30 TO Žó—“ú‚v
027397     WHEN 2
027398         DIVIDE 4 INTO Ž{p¼—ï”N‚v GIVING    ¤‚v
027399                                    REMAINDER —]‚v
027400         END-DIVIDE
027401         IF —]‚v = ZERO
027402             MOVE 29 TO Žó—“ú‚v
027403         ELSE
027404             MOVE 28 TO Žó—“ú‚v
027405         END-IF
027406     WHEN 1
027407     WHEN 3
027408     WHEN 5
027409     WHEN 7
027410     WHEN 8
027411     WHEN 10
027412     WHEN 12
027413         MOVE 31 TO Žó—“ú‚v
027414     WHEN OTHER
027415          CONTINUE
027416     END-EVALUATE.
027417*
004230******************************************************************
004240 END PROGRAM YIW721.
004250******************************************************************
