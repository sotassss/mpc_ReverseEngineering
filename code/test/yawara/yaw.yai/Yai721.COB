000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAI721.
000060 AUTHOR.                 ’r“c@KŽq
000070*
000080*----------------------------------------------------------------*
000090*         Ž{p‚ÌŽ–ŽÀyÃÞ°Àì¬z_+³¨ÝÄÞ³½Þ”Å
000100*         MED = YAI720
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2013-07-02
000130 DATE-COMPILED.          2013-07-02
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
000530     SELECT  ì‹Æƒtƒ@ƒCƒ‹‚P  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7211L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  ì‚P|Ž{p˜a—ï”NŒŽ“ú
000610                                                          ì‚P|Š³ŽÒƒR[ƒh
000910                             ALTERNATE RECORD KEY     IS  ì‚P|Ž{p˜a—ï”NŒŽ“ú
                                                                ì‚P|Š³ŽÒƒJƒi
                                                                ì‚P|Š³ŽÒƒR[ƒh
000910                             ALTERNATE RECORD KEY     IS  ì‚P|Š³ŽÒƒR[ƒh
                                                                ì‚P|Ž{p˜a—ï”NŒŽ“ú
000910                             ALTERNATE RECORD KEY     IS  ì‚P|Š³ŽÒƒJƒi
                                                                ì‚P|Š³ŽÒƒR[ƒh
                                                                ì‚P|Ž{p˜a—ï”NŒŽ“ú
000620                             FILE        STATUS       IS  ó‘ÔƒL[
000630                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  ì‹Æƒtƒ@ƒCƒ‹‚Q  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7212L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  ì‚Q|Ž{p˜a—ï”NŒŽ
000610                                                          ì‚Q|Š³ŽÒƒR[ƒh
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
      *                          m‚q‚k  ‚P‚T‚R‚Un
       FD  ƒŒƒZƒvƒg‚e          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   ƒŒƒZ  AS  PREFIX.
001060*                           m‚q‚k  ‚T‚P‚Qn
001070 FD  ‰ïŒvƒf[ƒ^‚e        BLOCK   CONTAINS   1   RECORDS.
001080     COPY KAIKEI     OF  XFDLIB  JOINING   ‰ï   AS  PREFIX.
      *                           m‚q‚k  ‚Q‚T‚Un
       FD  Ž{p‹L˜^‚e          BLOCK   CONTAINS   1   RECORDS.
          COPY SEKIROK         OF  XFDLIB  JOINING   Ž{‹L AS  PREFIX.
      *                           m‚q‚k  ‚P‚Q‚Wn
       FD  •‰ƒf[ƒ^‚e        BLOCK   CONTAINS   1   RECORDS.
           COPY HUSYOU          OF  XFDLIB  JOINING   •‰   AS  PREFIX.
001310*****************
001320* ì‹Æƒtƒ@ƒCƒ‹‚P *
001330*****************
001340*                         m‚q‚k  ‚P‚U‚On
001350 FD  ì‹Æƒtƒ@ƒCƒ‹‚P RECORD  CONTAINS 160 CHARACTERS.
001360 01 ì‚P|ƒŒƒR[ƒh.
001370    03 ì‚P|ƒŒƒR[ƒhƒL[.
001535       05 ì‚P|Ž{p˜a—ï”NŒŽ“ú.
001536          07 ì‚P|Ž{p˜a—ï               PIC 9.
001537          07 ì‚P|Ž{p”NŒŽ.
001538             09 ì‚P|Ž{p”N              PIC 9(2).
001539             09 ì‚P|Ž{pŒŽ              PIC 9(2).
001540          07 ì‚P|Ž{p“ú                 PIC 9(2).
001460       05 ì‚P|Š³ŽÒƒR[ƒh.
001470          07 ì‚P|Š³ŽÒ”Ô†                PIC 9(6).
001480          07 ì‚P|Ž}”Ô                    PIC X(1).
001490    03 ì‚P|ƒŒƒR[ƒhƒf[ƒ^.
             05 ì‚P|Š³ŽÒƒJƒi                   PIC X(50).
             05 ì‚P|Š³ŽÒŽ–¼                   PIC X(50).
             05 ì‚P|—¿‹à.
001550          07 ì‚P|‰ŒŸ‹àŠz                PIC 9(5).
                07 ì‚P|‰ÁŽZ‹àŠz                PIC 9(5).
001550          07 ì‚P|®•œ‹àŠz                PIC 9(5).
001550          07 ì‚P|Œã—Ã‹àŠz                PIC 9(5).
001550          07 ì‚P|ãª–@‹àŠz                PIC 9(5).
001550          07 ì‚P|“d—Ã‹àŠz                PIC 9(5).
001551          07 ì‚P|”ï—pŠz                  PIC 9(5).
001551          07 ì‚P|ˆê•”•‰’S‹à              PIC 9(5).
001500       05 FILLER                           PIC X(6).
000920*
001340*                         m‚q‚k  ‚P‚Q‚Wn
001350 FD  ì‹Æƒtƒ@ƒCƒ‹‚Q RECORD  CONTAINS 128 CHARACTERS.
001360 01 ì‚Q|ƒŒƒR[ƒh.
001370    03 ì‚Q|ƒŒƒR[ƒhƒL[.
001535       05 ì‚Q|Ž{p˜a—ï”NŒŽ.
001536          07 ì‚Q|Ž{p˜a—ï                PIC 9.
001537          07 ì‚Q|Ž{p”NŒŽ.
001538             09 ì‚Q|Ž{p”N               PIC 9(2).
001539             09 ì‚Q|Ž{pŒŽ               PIC 9(2).
001460       05 ì‚Q|Š³ŽÒƒR[ƒh.
001470          07 ì‚Q|Š³ŽÒ”Ô†                PIC 9(6).
001480          07 ì‚Q|Ž}”Ô                    PIC X(1).
001490    03 ì‚Q|ƒŒƒR[ƒhƒf[ƒ^.
             05 ì‚Q|‰ŒŸŒv                     PIC 9(5).
001550       05 ì‚Q|‰ÁŽZŒv                     PIC 9(5).
001550       05 ì‚Q|®•œŒv                     PIC 9(5).
001550       05 ì‚Q|Œã—ÃŒv                     PIC 9(5).
001550       05 ì‚Q|ãª–@Œv                     PIC 9(5).
001550       05 ì‚Q|“d—ÃŒv                     PIC 9(5).
001551       05 ì‚Q|”ï—pŒv                     PIC 9(6).
001551       05 ì‚Q|•‰’SŒv                     PIC 9(5).
             05 ì‚Q|ŒŽ‡Œv.
001551          07 ì‚Q|ŽÀ“ú”‡Œv              PIC 9(2).
001551          07 ì‚Q|”ï—p‡Œv                PIC 9(7).
001551          07 ì‚Q|•‰’S‡Œv                PIC 9(6).
001551          07 ì‚Q|¿‹‡Œv                PIC 9(7).
001500       05 FILLER                           PIC X(53).
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
001090    03 ŠJŽn“ú‚v‚o                      PIC 9(2) VALUE ZERO.
001090    03 I—¹“ú‚v‚o                      PIC 9(2) VALUE ZERO.
001060    03 ŠJŽn˜a—ï”NŒŽ“ú‚v‚q.
001060       05 ŠJŽn˜a—ï”NŒŽ‚v‚q.
001070          07 ŠJŽn˜a—ï‚v‚q              PIC 9(1) VALUE ZERO.
001080          07 ŠJŽn”N‚v‚q                PIC 9(2) VALUE ZERO.
001090          07 ŠJŽnŒŽ‚v‚q                PIC 9(2) VALUE ZERO.
001090       05 ŠJŽn“ú‚v‚q                   PIC 9(2) VALUE ZERO.
001060    03 I—¹˜a—ï”NŒŽ“ú‚v‚q.
001060       05 I—¹˜a—ï”NŒŽ‚v‚q.
001070          07 I—¹˜a—ï‚v‚q              PIC 9(1) VALUE ZERO.
001080          07 I—¹”N‚v‚q                PIC 9(2) VALUE ZERO.
001090          07 I—¹ŒŽ‚v‚q                PIC 9(2) VALUE ZERO.
001090       05 I—¹“ú‚v‚q                   PIC 9(2) VALUE ZERO.
001100    03 Ží•Ê‹æ•ª‚v‚q                    PIC X(4) VALUE SPACE.
001110    03 •ÛŒ¯Ží•Ê‚v‚q                    PIC 9(2) VALUE ZERO.
001120    03 Š³ŽÒƒR[ƒh‚v‚q.
001130       05 Š³ŽÒ”Ô†‚v‚q                 PIC 9(6) VALUE ZERO.
001140       05 Ž}”Ô‚v‚q                     PIC X(1) VALUE SPACE.
001150    03 –{l‰Æ‘°‹æ•ª‚v‚q                PIC 9(1) VALUE ZERO.
      *
          03 ˆóüðŒ‚v‚q                    PIC 9(2) VALUE ZERO.
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
001600    03 ‰ŒŸ—¿‚v‚q                    PIC 9(4)  VALUE ZERO.
001610    03 ‰ŒŸ‰ÁŽZ—¿‚v‚q                PIC 9(4)  VALUE ZERO.
001620    03 ‹x“ú‚v‚q                      PIC 9     VALUE ZERO.
001630    03 [–é‚v‚q                      PIC 9     VALUE ZERO.
001640    03 ŽžŠÔŠO‚v‚q                    PIC 9     VALUE ZERO.
001650    03 f—ÃŽž‚v‚q                    PIC 9(2)  VALUE ZERO.
001660    03 f—Ã•ª‚v‚q                    PIC 9(2)  VALUE ZERO.
          03 ‘Š’k—¿‚v‚q                    PIC 9(4)  VALUE ZERO.
001670    03 ÄŒŸ—¿‚v‚q                    PIC 9(4)  VALUE ZERO.
001680    03 ‰—Ã–éŠÔ‚v‚q                  PIC 9     VALUE ZERO.
001690    03 ‰—Ã“ï˜H‚v‚q                  PIC 9     VALUE ZERO.
001700    03 ‰—Ã–\•—‚v‚q                  PIC 9     VALUE ZERO.
001710    03 ‰—Ã‰ñ”‚v‚q                  PIC 9(2)  VALUE ZERO.
001720    03 ‰—Ã‹——£‚v‚q                  PIC 9(3)V9 VALUE ZERO.
001730    03 ‰—Ã—¿‚v‚q                    PIC 9(6)  VALUE ZERO.
001740    03 ‰—Ã‰ÁŽZ—¿‚v‚q                PIC 9(5)  VALUE ZERO.
001750    03 “dãª—¿‚v‚q                    PIC 9(4)  VALUE ZERO.
001760    03 ˆê•”•‰’S‹à‚v‚q                PIC 9(5)  VALUE ZERO.
001760    03 •‰’S‹àÚ×‚v‚q                PIC 9(5)  VALUE ZERO.
003530    03 ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q            PIC 9(5)  VALUE ZERO.
003140    03 ‰^“®Œã—Ã—¿‚v‚q                PIC 9(4)  VALUE ZERO.
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
002070    03 —âãª—¿Œv‚v‚q                  PIC 9(4)  VALUE ZERO.
002080    03 ‰·ãª—¿Œv‚v‚q                  PIC 9(4)  VALUE ZERO.
002090    03 “d—Ã—¿Œv‚v‚q                  PIC 9(4)  VALUE ZERO.
002100*
       01 WŒv‚v.
001600    03 ‰ŒŸŒv‚v                      PIC 9(5)  VALUE ZERO.
001830    03 ‰ÁŽZŒv‚v                      PIC 9(5)  VALUE ZERO.
001600    03 ®•œŒv‚v                      PIC 9(5)  VALUE ZERO.
001800    03 Œã—ÃŒv‚v                      PIC 9(5)  VALUE ZERO.
001810    03 ãª–@Œv‚v                      PIC 9(5)  VALUE ZERO.
001830    03 “d—ÃŒv‚v                      PIC 9(5)  VALUE ZERO.
001830    03 ”ï—pŒv‚v                      PIC 9(5)  VALUE ZERO.
001760    03 ˆê•”•‰’S‹àŒv‚v                PIC 9(5)  VALUE ZERO.
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
003110 01 ˜A“ü|“ü—Íƒf[ƒ^‚x‚`‚h‚V‚Q‚O IS EXTERNAL.
003120    03 ˜A“ü|ŠJŽn˜a—ï”NŒŽ“ú.
003130       05 ˜A“ü|ŠJŽn˜a—ï                  PIC 9(1).
003140       05 ˜A“ü|ŠJŽn”N                    PIC 9(2).
003150       05 ˜A“ü|ŠJŽnŒŽ                    PIC 9(2).
003150       05 ˜A“ü|ŠJŽn“ú                    PIC 9(2).
003120    03 ˜A“ü|I—¹˜a—ï”NŒŽ“ú.
003130       05 ˜A“ü|I—¹˜a—ï                  PIC 9(1).
003140       05 ˜A“ü|I—¹”N                    PIC 9(2).
003150       05 ˜A“ü|I—¹ŒŽ                    PIC 9(2).
003150       05 ˜A“ü|I—¹“ú                    PIC 9(2).
003170    03 ˜A“ü|•ÛŒ¯Ží•Ê                     PIC 9(2).
003180    03 ˜A“ü|–{l‰Æ‘°‹æ•ª                 PIC 9(1).
003190    03 ˜A“ü|Š³ŽÒƒR[ƒh.
003200       05 ˜A“ü|Š³ŽÒ”Ô†                  PIC 9(6).
003210       05 ˜A“ü|Ž}”Ô                      PIC X(1).
          03 ˜A“ü|ˆóüðŒ                     PIC 9(2).
006550*
006560 01 ˜AƒŒ|ƒL[ IS EXTERNAL.
006570    03 ˜AƒŒ|•ÛŒ¯Ží•Ê                     PIC 9(2).
006550*
003110 01 ˜Aˆó|ˆóüƒf[ƒ^‚x‚`‚h‚V‚Q‚P IS EXTERNAL.
          03 ˜Aˆó|‘OŠú‚—î                     PIC 9(1).
          03 ˜Aˆó|•‰’SŠ„‡                     PIC 9(2).
003170    03 ˜Aˆó|‹à‘®•›Žq                     PIC 9(5).
003170    03 ˜Aˆó|‘å                           PIC 9(1).
003170    03 ˜Aˆó|’†                           PIC 9(1).
003170    03 ˜Aˆó|¬                           PIC 9(1).
003170    03 ˜Aˆó|î•ñ’ñ‹Ÿ                     PIC 9(5).
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
001490************
001500*           *
001510* Žåˆ—     *
001520*           *
001530************
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
001720     MOVE ˜A“ü|•ÛŒ¯Ží•Ê      TO •ÛŒ¯Ží•Ê‚v‚q.
001730     MOVE ˜A“ü|Š³ŽÒ”Ô†      TO Š³ŽÒ”Ô†‚v‚q.
001740     MOVE ˜A“ü|Ž}”Ô          TO Ž}”Ô‚v‚q.
001750     MOVE ˜A“ü|ŠJŽn˜a—ï      TO ŠJŽn˜a—ï‚v‚q.
001760     MOVE ˜A“ü|ŠJŽn”N        TO ŠJŽn”N‚v‚q.
001770     MOVE ˜A“ü|ŠJŽnŒŽ        TO ŠJŽnŒŽ‚v‚q.
001770     MOVE ˜A“ü|ŠJŽn“ú        TO ŠJŽn“ú‚v‚q.
001750     MOVE ˜A“ü|I—¹˜a—ï      TO I—¹˜a—ï‚v‚q.
001760     MOVE ˜A“ü|I—¹”N        TO I—¹”N‚v‚q.
001770     MOVE ˜A“ü|I—¹ŒŽ        TO I—¹ŒŽ‚v‚q.
001770     MOVE ˜A“ü|I—¹“ú        TO I—¹“ú‚v‚q.
001780     MOVE ˜A“ü|–{l‰Æ‘°‹æ•ª  TO –{l‰Æ‘°‹æ•ª‚v‚q.
      *
           MOVE ˜A“ü|ˆóüðŒ      TO ˆóüðŒ‚v‚q.

001790*================================================================*
001800 ƒtƒ@ƒCƒ‹ƒI[ƒvƒ“ SECTION.
001810*
012140     OPEN INPUT   Œ³†ƒ}ƒXƒ^
012150         MOVE NC"Œ³†" TO ƒtƒ@ƒCƒ‹–¼.
012160         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
001820     OPEN INPUT ŽófŽÒî•ñ‚e.
001830         MOVE NC"ŽófŽÒî•ñ‚e" TO ƒtƒ@ƒCƒ‹–¼.
001840         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
007870     OPEN INPUT ƒŒƒZƒvƒg‚e.
007880         MOVE NC"ƒŒƒZ"         TO ƒtƒ@ƒCƒ‹–¼.
007890         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
003060     OPEN INPUT ‰ïŒvƒf[ƒ^‚e.
003070         MOVE NC"‰ïŒv" TO ƒtƒ@ƒCƒ‹–¼.
003080         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
           OPEN INPUT •‰ƒf[ƒ^‚e.
               MOVE NC"•‰ƒf[ƒ^‚e" TO ƒtƒ@ƒCƒ‹–¼.
               PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
           OPEN INPUT Ž{p‹L˜^‚e.
               MOVE NC"Ž{p‹L˜^‚e"   TO ƒtƒ@ƒCƒ‹–¼.
               PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
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
002000     CLOSE Œ³†ƒ}ƒXƒ^   ŽófŽÒî•ñ‚e ƒŒƒZƒvƒg‚e
                 ‰ïŒvƒf[ƒ^‚e •‰ƒf[ƒ^‚e Ž{p‹L˜^‚e.
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
002210     OPEN OUTPUT ì‹Æƒtƒ@ƒCƒ‹‚Q
002220          MOVE NC"ì‚Q" TO ƒtƒ@ƒCƒ‹–¼
002230          PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN
      *
003720     MOVE ŠJŽn˜a—ï‚v‚q      TO Žó|Ž{p˜a—ï.
003730     MOVE ŠJŽn”N‚v‚q        TO Žó|Ž{p”N.
003740     MOVE ŠJŽnŒŽ‚v‚q        TO Žó|Ž{pŒŽ.
           MOVE SPACE             TO Žó|Š³ŽÒƒJƒi.
           MOVE SPACE             TO Žó|Š³ŽÒƒR[ƒh.
003790     START ŽófŽÒî•ñ‚e   KEY IS >= Žó|Ž{p˜a—ï”NŒŽ
                                          Žó|Š³ŽÒƒJƒi
                                          Žó|Š³ŽÒƒR[ƒh
003830     IF ó‘ÔƒL[ = "00"
003840         MOVE SPACE  TO I—¹ƒtƒ‰ƒO
003850         PERFORM ŽófŽÒî•ñ‚e“Çž
003860         PERFORM UNTIL ( I—¹ƒtƒ‰ƒO = "YES" ) OR
003870                       ( Žó|Ž{p˜a—ï”NŒŽ > I—¹˜a—ï”NŒŽ‚v‚q )
      */ˆóü‘ÎÛðŒ‚Ì”»’è
001110                 IF ((•ÛŒ¯Ží•Ê‚v‚q     NOT = ZERO)              AND
                           (•ÛŒ¯Ží•Ê‚v‚q     NOT = Žó|•ÛŒ¯Ží•Ê))     OR
HILO  */Š³ŽÒŽw’èŽžAŽ}”Ô‚àŠÜ‚Ü‚ê‚Ä‚µ‚Ü‚¤ˆ×•ÏX20405
001120*                    ((Š³ŽÒ”Ô†‚v‚q     NOT = ZERO)              AND
001120*                     (Š³ŽÒ”Ô†‚v‚q     NOT = Žó|Š³ŽÒ”Ô†))     OR
      *                    ((Ž}”Ô‚v‚q         NOT = SPACE)             AND
      *                     (Ž}”Ô‚v‚q         NOT = Žó|Ž}”Ô))         OR
HILO                      ((Š³ŽÒ”Ô†‚v‚q     NOT = ZERO)              AND
HILO                       (Š³ŽÒƒR[ƒh‚v‚q   NOT = Žó|Š³ŽÒƒR[ƒh))   OR
001150                    ((–{l‰Æ‘°‹æ•ª‚v‚q NOT = ZERO)              AND
                           (–{l‰Æ‘°‹æ•ª‚v‚q NOT = Žó|–{l‰Æ‘°‹æ•ª)) OR
                           (Žó|•ÛŒ¯Ží•Ê         = 70 OR 80 OR 90)
      */‘ÎÛŠO
                           CONTINUE
                       ELSE
      */ˆóüðŒ‚v‚q©•Û—¯
                           MOVE "YES" TO ‘ÎÛƒtƒ‰ƒO
                           IF ˆóüðŒ‚v‚q NOT = ZERO
                               PERFORM ˆóüðŒ”»’è
                           END-IF
                           IF ‘ÎÛƒtƒ‰ƒO = "YES"
                               PERFORM ƒf[ƒ^ƒ`ƒFƒbƒN
                               IF ŽÀsƒL[‚v = "YES"
                                   MOVE Žó|Ž{p˜a—ï”NŒŽ TO Ž{p˜a—ï”NŒŽ‚v‚q
                                   IF Žó|Ž{p˜a—ï”NŒŽ = ŠJŽn˜a—ï”NŒŽ‚v‚q
                                       MOVE ŠJŽn“ú‚v‚q TO ŠJŽn“ú‚v‚o
                                   ELSE
                                       MOVE 1          TO ŠJŽn“ú‚v‚o
                                   END-IF
                                   IF Žó|Ž{p˜a—ï”NŒŽ = I—¹˜a—ï”NŒŽ‚v‚q
                                       MOVE I—¹“ú‚v‚q TO I—¹“ú‚v‚o
                                   ELSE
                                       PERFORM ŒŽ––“úŽæ“¾
                                       MOVE Žó—“ú‚v   TO I—¹“ú‚v‚o
                                   END-IF
003910                             PERFORM ì‚PƒŒƒR[ƒhƒZƒbƒg
003910                             PERFORM ì‚QƒŒƒR[ƒhƒZƒbƒg
                               END-IF
                           END-IF
                       END-IF
003930                 PERFORM ŽófŽÒî•ñ‚e“Çž
003940         END-PERFORM
003950     END-IF.
002810     CLOSE ì‹Æƒtƒ@ƒCƒ‹‚P ì‹Æƒtƒ@ƒCƒ‹‚Q.
004030*================================================================*
004040 ì‚PƒŒƒR[ƒhƒZƒbƒg SECTION.
004050*
008270     INITIALIZE ì‚P|ƒŒƒR[ƒh.
004130     MOVE Žó|Š³ŽÒƒR[ƒh   TO ì‚P|Š³ŽÒƒR[ƒh.
           MOVE Žó|Š³ŽÒƒJƒi     TO ì‚P|Š³ŽÒƒJƒi.
           MOVE Žó|Š³ŽÒŽ–¼     TO ì‚P|Š³ŽÒŽ–¼.
           PERFORM VARYING Ž{p“ú‚v‚q FROM ŠJŽn“ú‚v‚o BY 1
                   UNTIL ( Ž{p“ú‚v‚q > I—¹“ú‚v‚o )
               INITIALIZE ì‚P|—¿‹à
               MOVE Žó|Š³ŽÒ”Ô†  TO Ž{‹L|Š³ŽÒ”Ô†
               MOVE Žó|Ž}”Ô      TO Ž{‹L|Ž}”Ô
               MOVE Žó|Ž{p˜a—ï  TO Ž{‹L|Ž{p˜a—ï
               MOVE Žó|Ž{p”N    TO Ž{‹L|Ž{p”N
               MOVE Žó|Ž{pŒŽ    TO Ž{‹L|Ž{pŒŽ
               MOVE Ž{p“ú‚v‚q    TO Ž{‹L|Ž{p“ú
               READ Ž{p‹L˜^‚e
               NOT INVALID KEY
                   PERFORM ƒŒƒZƒvƒgŒÄo‚µ‚P
                   PERFORM —¿‹àî•ñŽæ“¾
                   PERFORM €–Ú‚²‚ÆŒvŽZ
                   PERFORM •‰’S‹àÚ×WŒv
                   PERFORM ì‹Æƒtƒ@ƒCƒ‹ƒZƒbƒg
                   PERFORM ì‚PƒŒƒR[ƒh‘ž
               END-READ
           END-PERFORM.
008240*================================================================*
008250 ì‹Æƒtƒ@ƒCƒ‹ƒZƒbƒg SECTION.
008260*
008280     MOVE Žó|Ž{p˜a—ï       TO ì‚P|Ž{p˜a—ï.
008290     MOVE Žó|Ž{p”N         TO ì‚P|Ž{p”N.
008300     MOVE Žó|Ž{pŒŽ         TO ì‚P|Ž{pŒŽ.
008310     MOVE Ž{p“ú‚v‚q         TO ì‚P|Ž{p“ú.
008330     MOVE ˆê•”•‰’S‹à‚v‚q     TO ì‚P|ˆê•”•‰’S‹à.
008240*================================================================*
008250 ì‹Æ‚Qƒtƒ@ƒCƒ‹ƒZƒbƒg SECTION.
008260*
008280     MOVE Žó|Ž{p˜a—ï       TO ì‚Q|Ž{p˜a—ï.
008290     MOVE Žó|Ž{p”N         TO ì‚Q|Ž{p”N.
008300     MOVE Žó|Ž{pŒŽ         TO ì‚Q|Ž{pŒŽ.
           MOVE ‰ŒŸŒv‚v           TO ì‚Q|‰ŒŸŒv.
           MOVE ‰ÁŽZŒv‚v           TO ì‚Q|‰ÁŽZŒv.
           MOVE ®•œŒv‚v           TO ì‚Q|®•œŒv.
           MOVE Œã—ÃŒv‚v           TO ì‚Q|Œã—ÃŒv.
           MOVE ãª–@Œv‚v           TO ì‚Q|ãª–@Œv.
           MOVE “d—ÃŒv‚v           TO ì‚Q|“d—ÃŒv.
           MOVE ”ï—pŒv‚v           TO ì‚Q|”ï—pŒv.
           MOVE ˆê•”•‰’S‹àŒv‚v     TO ì‚Q|•‰’SŒv.
004030*================================================================*
004040 ì‚QƒŒƒR[ƒhƒZƒbƒg SECTION.
004050*
008270     INITIALIZE ì‚Q|ƒŒƒR[ƒh.
           MOVE SPACE TO WŒv‚v.
004130     MOVE Žó|Š³ŽÒƒR[ƒh   TO ì‚Q|Š³ŽÒƒR[ƒh.
           PERFORM VARYING Ž{p“ú‚v‚q FROM 1 BY 1
                   UNTIL ( Ž{p“ú‚v‚q > 31 )
               INITIALIZE ì‚P|—¿‹à
               MOVE Žó|Š³ŽÒ”Ô†  TO Ž{‹L|Š³ŽÒ”Ô†
               MOVE Žó|Ž}”Ô      TO Ž{‹L|Ž}”Ô
               MOVE Žó|Ž{p˜a—ï  TO Ž{‹L|Ž{p˜a—ï
               MOVE Žó|Ž{p”N    TO Ž{‹L|Ž{p”N
               MOVE Žó|Ž{pŒŽ    TO Ž{‹L|Ž{pŒŽ
               MOVE Ž{p“ú‚v‚q    TO Ž{‹L|Ž{p“ú
               READ Ž{p‹L˜^‚e
               NOT INVALID KEY
                   PERFORM ƒŒƒZƒvƒgŒÄo‚µ‚P
                   PERFORM WŒv
                   PERFORM •‰’S‹àÚ×WŒv
               END-READ
           END-PERFORM.
           PERFORM ì‹Æ‚Qƒtƒ@ƒCƒ‹ƒZƒbƒg.
           PERFORM ŒŽ‡ŒvƒZƒbƒg.
           PERFORM ì‚QƒŒƒR[ƒh‘ž.
009320*================================================================*
009330 ƒŒƒZƒvƒgŒÄo‚µ‚P SECTION.
009340*
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
009720*     IF  Žó|•¬Ží•Ê NOT = ZERO
009730*         MOVE ‰ï|•¬ŒvŽZ•‰’SŠz TO ˆê•”•‰’S‹à‚v‚q
009740*     ELSE
009750         MOVE ‰ï|ˆê•”•‰’S‹à     TO ˆê•”•‰’S‹à‚v‚q.
009760*     END-IF.
009710*
005200     MOVE Žó|Ž{p˜a—ï  TO ƒŒƒZ|Ž{p˜a—ï.
005210     MOVE Žó|Ž{p”N    TO ƒŒƒZ|Ž{p”N.  
005220     MOVE Žó|Ž{pŒŽ    TO ƒŒƒZ|Ž{pŒŽ.  
005230     MOVE Žó|Š³ŽÒ”Ô†  TO ƒŒƒZ|Š³ŽÒ”Ô†.
005240     MOVE Žó|Ž}”Ô      TO ƒŒƒZ|Ž}”Ô.    
           READ ƒŒƒZƒvƒg‚e
           INVALID KEY
               MOVE SPACE     TO ƒŒƒZ|ƒŒƒR[ƒh
           END-READ.
009710*
010900*================================================================*
010910 —¿‹àî•ñŽæ“¾ SECTION.
010920*
010930***********************************************
010940* —¿‹àƒf[ƒ^ƒZƒbƒg                            *
010950***********************************************
010960*    ****************************************************************
010970*    * —¿‹àiŒŽ–ˆji•‰–ˆji’üŒ¸–ˆj‚É‚Â‚¢‚Ä‚Í˜AŒ‹€–Ú‚æ‚èƒZƒbƒg *
010980*    ****************************************************************
           MOVE 1  TO ƒJƒEƒ“ƒ^.
010990     MOVE ‰ï|‰ŒŸ—¿             TO  ‰ŒŸ—¿‚v‚q.
011000*
011010     MOVE ‰ï|‰ŒŸ‰ÁŽZ—¿         TO  ‰ŒŸ‰ÁŽZ—¿‚v‚q.
           MOVE ‰ï|‰ŒŸŽž‘Š’k—¿       TO  ‘Š’k—¿‚v‚q.
011050     MOVE ‰ï|ÄŒŸ—¿             TO  ÄŒŸ—¿‚v‚q.
      *
011080     MOVE ‰ï|‰—Ã—¿             TO  ‰—Ã—¿‚v‚q.
011090     MOVE ‰ï|‰—Ã‰ÁŽZ—¿         TO  ‰—Ã‰ÁŽZ—¿‚v‚q.
011150*
011160     MOVE ‰ï|Ž{pî•ñ’ñ‹Ÿ—¿     TO  Ž{pî•ñ’ñ‹Ÿ—¿‚v‚q.
           COMPUTE ì‚P|‰ŒŸ‹àŠz = ‰ŒŸ—¿‚v‚q + ÄŒŸ—¿‚v‚q + ‰—Ã—¿‚v‚q + ‰—Ã‰ÁŽZ—¿‚v‚q.
           COMPUTE ì‚P|‰ÁŽZ‹àŠz = ‰ŒŸ‰ÁŽZ—¿‚v‚q + ‘Š’k—¿‚v‚q.
           MOVE ‰ï|”ï—pŠz             TO ì‚P|”ï—pŠz.
011360*
011370*================================================================*
011380 €–Ú‚²‚ÆŒvŽZ SECTION.
011390***********************************************
011400* —¿‹àƒf[ƒ^ƒZƒbƒg                            *
011410***********************************************
013980     PERFORM VARYING ƒJƒEƒ“ƒ^ FROM 1 BY 1
013990             UNTIL ( ƒJƒEƒ“ƒ^ > 5 )
017100         MOVE ‰ï|‰‰ñˆ’u—¿(ƒJƒEƒ“ƒ^) TO ‰‰ñˆ’u—¿‚v‚q(ƒJƒEƒ“ƒ^)
018031     END-PERFORM.
011130*
      */‰^“®—Ã–@A‹à‘®•›Žq’Ç‰Á/180518
011140     MOVE ‰ï|‹à‘®•›Žq‰ÁŽZ—¿     TO  ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q.
015100     MOVE ‰ï|‰^“®Œã—Ã—¿         TO  ‰^“®Œã—Ã—¿‚v‚q.
           IF Žó|Ž{p˜a—ï”NŒŽ < 43006
017120        MOVE ‰ï|‰‰ñˆ’u—¿‡Œv  TO ì‚P|®•œ‹àŠz
           ELSE
017120        COMPUTE ì‚P|®•œ‹àŠz = ‰ï|‰‰ñˆ’u—¿‡Œv + ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q + ‰^“®Œã—Ã—¿‚v‚q
           END-IF.
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
011500     MOVE ‰ï|Œã—Ã—¿‚T‚Q   TO Œã—Ã—¿‚T‚Q‚v‚q.
011500     MOVE ‰ï|Œã—Ã—¿‚T‚T   TO Œã—Ã—¿‚T‚T‚v‚q.
011510     MOVE ‰ï|Œã—Ã—¿‚T‚W   TO Œã—Ã—¿‚T‚W‚v‚q.
011520     MOVE ‰ï|Œã—Ã—¿‚T‚O   TO Œã—Ã—¿‚T‚O‚v‚q.
011530     COMPUTE Œã—Ã—¿‚v‚q(5)   = Œã—Ã—¿‚T‚Q‚v‚q + Œã—Ã—¿‚T‚T‚v‚q + Œã—Ã—¿‚T‚W‚v‚q + Œã—Ã—¿‚T‚O‚v‚q.
011530     COMPUTE ì‚P|Œã—Ã‹àŠz = Œã—Ã—¿‚v‚q(1) + Œã—Ã—¿‚v‚q(2) +
                                    Œã—Ã—¿‚v‚q(3) + Œã—Ã—¿‚v‚q(4) + Œã—Ã—¿‚v‚q(5).
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
           COMPUTE “d—Ã—¿Œv‚v‚q = “d—Ã—¿‚v‚q(1) + “d—Ã—¿‚v‚q(2) + “d—Ã—¿‚v‚q(3) + “d—Ã—¿‚v‚q(4) + “d—Ã—¿‚v‚q(5).
           MOVE “d—Ã—¿Œv‚v‚q             TO ì‚P|“d—Ã‹àŠz.
           COMPUTE ì‚P|ãª–@‹àŠz = —âãª—¿Œv‚v‚q + ‰·ãª—¿Œv‚v‚q.
011920*
011370*================================================================*
011380 WŒv SECTION.
011390***********************************************
011400* —¿‹àƒf[ƒ^ƒZƒbƒg                            *
011410***********************************************
010990     MOVE ‰ï|‰ŒŸ—¿             TO  ‰ŒŸ—¿‚v‚q.
011000*
011010     MOVE ‰ï|‰ŒŸ‰ÁŽZ—¿         TO  ‰ŒŸ‰ÁŽZ—¿‚v‚q.
           MOVE ‰ï|‰ŒŸŽž‘Š’k—¿       TO  ‘Š’k—¿‚v‚q.
011050     MOVE ‰ï|ÄŒŸ—¿             TO  ÄŒŸ—¿‚v‚q.
011080     MOVE ‰ï|‰—Ã—¿             TO  ‰—Ã—¿‚v‚q.
011090     MOVE ‰ï|‰—Ã‰ÁŽZ—¿         TO  ‰—Ã‰ÁŽZ—¿‚v‚q.
011140     MOVE ‰ï|‹à‘®•›Žq‰ÁŽZ—¿     TO  ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q.
015100     MOVE ‰ï|‰^“®Œã—Ã—¿         TO  ‰^“®Œã—Ã—¿‚v‚q.
011160     MOVE ‰ï|Ž{pî•ñ’ñ‹Ÿ—¿     TO  Ž{pî•ñ’ñ‹Ÿ—¿‚v‚q.
           COMPUTE ‰ŒŸŒv‚v = ‰ŒŸŒv‚v + ‰ŒŸ—¿‚v‚q + ÄŒŸ—¿‚v‚q + ‰—Ã—¿‚v‚q + ‰—Ã‰ÁŽZ—¿‚v‚q.
           COMPUTE ‰ÁŽZŒv‚v = ‰ÁŽZŒv‚v + ‰ŒŸ‰ÁŽZ—¿‚v‚q + ‘Š’k—¿‚v‚q.
      *
      */‰^“®—Ã–@A‹à‘®•›Žq’Ç‰Á/180518
           IF Žó|Ž{p˜a—ï”NŒŽ < 43006
017120         COMPUTE ®•œŒv‚v = ®•œŒv‚v + ‰ï|‰‰ñˆ’u—¿‡Œv
           ELSE
017120         COMPUTE ®•œŒv‚v = ®•œŒv‚v + ‰ï|‰‰ñˆ’u—¿‡Œv + ‹à‘®•›Žq‰ÁŽZ—¿‚v‚q + ‰^“®Œã—Ã—¿‚v‚q
           END-IF.
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
011500     MOVE ‰ï|Œã—Ã—¿‚T‚Q   TO Œã—Ã—¿‚T‚Q‚v‚q.
011500     MOVE ‰ï|Œã—Ã—¿‚T‚T   TO Œã—Ã—¿‚T‚T‚v‚q.
011510     MOVE ‰ï|Œã—Ã—¿‚T‚W   TO Œã—Ã—¿‚T‚W‚v‚q.
011520     MOVE ‰ï|Œã—Ã—¿‚T‚O   TO Œã—Ã—¿‚T‚O‚v‚q.
011530     COMPUTE Œã—Ã—¿‚v‚q(5)   = Œã—Ã—¿‚T‚Q‚v‚q + Œã—Ã—¿‚T‚T‚v‚q + Œã—Ã—¿‚T‚W‚v‚q + Œã—Ã—¿‚T‚O‚v‚q.
011530     COMPUTE Œã—ÃŒv‚v = Œã—ÃŒv‚v + Œã—Ã—¿‚v‚q(1) + Œã—Ã—¿‚v‚q(2) + Œã—Ã—¿‚v‚q(3) + Œã—Ã—¿‚v‚q(4) + Œã—Ã—¿‚v‚q(5).
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
011680     COMPUTE —âãª—¿‚v‚q(5)   = —âãª–@—¿‚T‚Q‚v‚q + —âãª–@—¿‚T‚T‚v‚q + —âãª–@—¿‚T‚W‚v‚q + —âãª–@—¿‚T‚O‚v‚q.
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
           COMPUTE “d—Ã—¿Œv‚v‚q = “d—Ã—¿‚v‚q(1) + “d—Ã—¿‚v‚q(2) + “d—Ã—¿‚v‚q(3) + “d—Ã—¿‚v‚q(4) + “d—Ã—¿‚v‚q(5).
           COMPUTE ãª–@Œv‚v = ãª–@Œv‚v + —âãª—¿Œv‚v‚q + ‰·ãª—¿Œv‚v‚q.
           COMPUTE “d—ÃŒv‚v = “d—ÃŒv‚v + “d—Ã—¿Œv‚v‚q.
011920*
           COMPUTE ”ï—pŒv‚v = ”ï—pŒv‚v + ‰ï|”ï—pŠz.
           COMPUTE ˆê•”•‰’S‹àŒv‚v = ˆê•”•‰’S‹àŒv‚v + ˆê•”•‰’S‹à‚v‚q.
      *
011370*================================================================*
011380 •‰’S‹àÚ×WŒv SECTION.
      *
009720*     IF  Žó|•¬Ží•Ê NOT = ZERO
009670         MOVE ‰ï|•¬ŒvŽZ•‰’SŠz TO •‰’S‹àÚ×‚v‚q.
009680*     ELSE
009690*         MOVE ‰ï|ˆê•”•‰’S‹à   TO •‰’S‹àÚ×‚v‚q
009700*     END-IF.
009710*
004140*================================================================*
004150 ì‚PƒŒƒR[ƒh‘ž SECTION.
004160*
004170     WRITE ì‚P|ƒŒƒR[ƒh
004180     INVALID KEY
004190         MOVE NC"ì‚P"  TO ƒtƒ@ƒCƒ‹–¼
004200         PERFORM ƒGƒ‰[•\Ž¦
004210     END-WRITE.
004140*================================================================*
004150 ì‚QƒŒƒR[ƒh‘ž SECTION.
004160*
004170     WRITE ì‚Q|ƒŒƒR[ƒh
004180     INVALID KEY
004190         MOVE NC"ì‚Q"  TO ƒtƒ@ƒCƒ‹–¼
004200         PERFORM ƒGƒ‰[•\Ž¦
004210     END-WRITE.
      *================================================================*
       ŒŽ‡ŒvƒZƒbƒg SECTION.
      *
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
      *
008330     MOVE ƒŒƒZ|‡Œv         TO ì‚Q|”ï—p‡Œv.
008340     MOVE ƒŒƒZ|ˆê•”•‰’S‹à   TO ì‚Q|•‰’S‡Œv.
008220     MOVE ƒŒƒZ|¿‹‹àŠz     TO ì‚Q|¿‹‡Œv.
           MOVE ƒŒƒZ|ƒŒƒZŽÀ“ú”   TO ì‚Q|ŽÀ“ú”‡Œv.
           MOVE ƒŒƒZ|•‰’SŠ„‡       TO ˜Aˆó|•‰’SŠ„‡.
           IF Žó|Ž{p˜a—ï”NŒŽ < 43006
              MOVE ƒŒƒZ|‹à‘®•›Žq‰ÁŽZ—¿ TO ˜Aˆó|‹à‘®•›Žq
              MOVE ƒŒƒZ|‘å             TO ˜Aˆó|‘å
              MOVE ƒŒƒZ|’†             TO ˜Aˆó|’†
              MOVE ƒŒƒZ|¬             TO ˜Aˆó|¬
           ELSE
              MOVE ZERO                 TO ˜Aˆó|‹à‘®•›Žq
           END-IF.
           MOVE ƒŒƒZ|Ž{pî•ñ’ñ‹Ÿ—¿ TO ˜Aˆó|î•ñ’ñ‹Ÿ.
      *‘OŠú‚—î
           IF Žó|“Á•Ê‹æ•ª = 1 OR 2 OR 3
016860         MOVE 1                TO ˜Aˆó|‘OŠú‚—î
           ELSE
016860         MOVE ZERO             TO ˜Aˆó|‘OŠú‚—î
           END-IF.
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
002820*================================================================*
       ˆóüðŒ”»’è SECTION.
      *
           MOVE SPACE TO ‘ÎÛƒtƒ‰ƒO
      */˜JÐŽ©”…Ó‚Ìƒf[ƒ^‚Í‘ÎÛŠO
           IF Žó|•ÛŒ¯Ží•Ê NOT = 70 AND 80
               EVALUATE ˆóüðŒ‚v‚q
               WHEN 01
                   PERFORM ‰ŒŸ”»’è
               WHEN 02
                   PERFORM Œã—Ã‚Ì‚Ý”»’è
               WHEN 03
                   PERFORM I—¹”»’è
               WHEN 04
                   PERFORM ‰ŒŸ”»’è
                   IF ‘ÎÛƒtƒ‰ƒO = "YES"
                       PERFORM I—¹”»’è
                   END-IF
               WHEN OTHER
                   CONTINUE
               END-EVALUATE
           END-IF.
040770*================================================================*
       ‰ŒŸ”»’è SECTION.
      *
014120     MOVE Žó|Š³ŽÒ”Ô†  TO Ž{‹L|Š³ŽÒ”Ô†
014130     MOVE Žó|Ž}”Ô      TO Ž{‹L|Ž}”Ô
014140     MOVE Žó|Ž{p˜a—ï  TO Ž{‹L|Ž{p˜a—ï
014150     MOVE Žó|Ž{p”N    TO Ž{‹L|Ž{p”N
014160     MOVE Žó|Ž{pŒŽ    TO Ž{‹L|Ž{pŒŽ
014170     MOVE ZERO          TO Ž{‹L|Ž{p“ú
014180     START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
014190                                  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
014200     END-START
014210     IF ó‘ÔƒL[ = "00"
014120         MOVE SPACE TO I—¹ƒtƒ‰ƒO‚Q
014140         PERFORM Ž{p‹L˜^‚e“Çž
014150         PERFORM UNTIL (I—¹ƒtƒ‰ƒO‚Q         = "YES"         ) OR
014160                       (Ž{‹L|Š³ŽÒƒR[ƒh NOT = Žó|Š³ŽÒƒR[ƒh) OR
014170                       (Ž{‹L|Ž{p˜a—ï   NOT = Žó|Ž{p˜a—ï  ) OR
014180                       (Ž{‹L|Ž{p”N     NOT = Žó|Ž{p”N    ) OR
014190                       (Ž{‹L|Ž{pŒŽ     NOT = Žó|Ž{pŒŽ    )
      *
                   IF Ž{‹L|f—Ã‹æ•ª = 2
                       MOVE "YES" TO ‘ÎÛƒtƒ‰ƒO
                       MOVE "YES" TO I—¹ƒtƒ‰ƒO‚Q
                   END-IF
040620             PERFORM Ž{p‹L˜^‚e“Çž
               END-PERFORM
           END-IF.
040770*================================================================*
       Œã—Ã‚Ì‚Ý”»’è SECTION.
      *
014120     MOVE Žó|Š³ŽÒ”Ô†  TO Ž{‹L|Š³ŽÒ”Ô†
014130     MOVE Žó|Ž}”Ô      TO Ž{‹L|Ž}”Ô
014140     MOVE Žó|Ž{p˜a—ï  TO Ž{‹L|Ž{p˜a—ï
014150     MOVE Žó|Ž{p”N    TO Ž{‹L|Ž{p”N
014160     MOVE Žó|Ž{pŒŽ    TO Ž{‹L|Ž{pŒŽ
014170     MOVE ZERO          TO Ž{‹L|Ž{p“ú
014180     START Ž{p‹L˜^‚e   KEY IS >= Ž{‹L|Š³ŽÒƒR[ƒh
014190                                  Ž{‹L|Ž{p˜a—ï”NŒŽ“ú
014200     END-START
014210     IF ó‘ÔƒL[ = "00"
014120         MOVE SPACE TO I—¹ƒtƒ‰ƒO‚Q
014140         PERFORM Ž{p‹L˜^‚e“Çž
014150         PERFORM UNTIL (I—¹ƒtƒ‰ƒO‚Q         = "YES"         ) OR
014160                       (Ž{‹L|Š³ŽÒƒR[ƒh NOT = Žó|Š³ŽÒƒR[ƒh) OR
014170                       (Ž{‹L|Ž{p˜a—ï   NOT = Žó|Ž{p˜a—ï  ) OR
014180                       (Ž{‹L|Ž{p”N     NOT = Žó|Ž{p”N    ) OR
014190                       (Ž{‹L|Ž{pŒŽ     NOT = Žó|Ž{pŒŽ    )
      *
                   IF Ž{‹L|f—Ã‹æ•ª = 1
                       MOVE "YES" TO ‘ÎÛƒtƒ‰ƒO
                   ELSE
                       MOVE SPACE TO ‘ÎÛƒtƒ‰ƒO
                       MOVE "YES" TO I—¹ƒtƒ‰ƒO‚Q
                   END-IF
040620             PERFORM Ž{p‹L˜^‚e“Çž
               END-PERFORM
           END-IF.
040770*================================================================*
       I—¹”»’è SECTION.
      *
           MOVE "YES" TO ‘ÎÛƒtƒ‰ƒO.
      *
014190     MOVE Žó|Ž{p˜a—ï   TO •‰|Ž{p˜a—ï.
014200     MOVE Žó|Ž{p”N     TO •‰|Ž{p”N.
014210     MOVE Žó|Ž{pŒŽ     TO •‰|Ž{pŒŽ.
014120     MOVE Žó|Š³ŽÒ”Ô†   TO •‰|Š³ŽÒ”Ô†.
014130     MOVE Žó|Ž}”Ô       TO •‰|Ž}”Ô.
014140     READ •‰ƒf[ƒ^‚e
014150     INVALID KEY
014160         MOVE SPACE  TO ‘ÎÛƒtƒ‰ƒO
014170     NOT INVALID KEY
               IF •‰|•”ˆÊ” = ZERO
                   MOVE SPACE TO ‘ÎÛƒtƒ‰ƒO
               ELSE
020600             PERFORM VARYING •”ˆÊ‚b‚m‚s FROM 1 BY 1
020610                      UNTIL (•”ˆÊ‚b‚m‚s > •‰|•”ˆÊ”)
                       IF •‰|“]‹A‹æ•ª(•”ˆÊ‚b‚m‚s) = 9 OR 5
                           MOVE SPACE TO ‘ÎÛƒtƒ‰ƒO
                       END-IF
                   END-PERFORM
               END-IF
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
004240 END PROGRAM YAI721.
004250******************************************************************
