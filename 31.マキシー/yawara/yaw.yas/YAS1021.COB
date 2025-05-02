000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAS1021.
000060 AUTHOR.                 ŽR“c _”V
000070*
000080*----------------------------------------------------------------*
      *    y‰ï’ño—pƒtƒƒbƒs[ì¬(SSB)z ‚ ‚³‚ÐÚœŽt‰ï—p
000100*    yƒf[ƒ^ì¬z
000110*         MED = YAS580
000120*----------------------------------------------------------------*
       DATE-WRITTEN.           2012-07-05
       DATE-COMPILED.          2012-07-05
000150*----------------------------------------------------------------*
000160******************************************************************
000170*            ENVIRONMENT         DIVISION                        *
000180******************************************************************
000190 ENVIRONMENT             DIVISION.
000200 CONFIGURATION           SECTION.
000210 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000220 OBJECT-COMPUTER.        FMV-DESKPOWER.
000230 SPECIAL-NAMES.          CONSOLE  IS  CONS
000240                         SYSERR   IS  MSGBOX.
000250 INPUT-OUTPUT            SECTION.
000260 FILE-CONTROL.
001520     SELECT ì‹Æƒtƒ@ƒCƒ‹‚P ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
000861                             RECORD      KEY          IS  ì‚P|¿‹˜a—ï”NŒŽ
000890                                                          ì‚P|•ÛŒ¯ŽÒ”Ô†
000900                                                          ì‚P|Ž{p˜a—ï”NŒŽ
000910                                                          ì‚P|”í•ÛŒ¯ŽÒƒJƒi
000920                                                          ì‚P|Š³ŽÒƒR[ƒh
000921                                                          ì‚P|•¬‹æ•ª
000680                              ALTERNATE RECORD KEY    IS  ì‚P|WŒv‡
000900                                                          ì‚P|–{l‰Æ‘°‹æ•ª
000910                                                          ì‚P|Š³ŽÒƒR[ƒh
000900                                                          ì‚P|Ž{p˜a—ï”NŒŽ
000921                                                          ì‚P|•¬‹æ•ª
000952                              FILE STATUS              IS  ó‘ÔƒL[
001600                             LOCK        MODE         IS  AUTOMATIC.
001270     SELECT ‰ï’ñoƒtƒ@ƒCƒ‹ ASSIGN    TO    FD-NAME
000580                             ORGANIZATION             IS  LINE SEQUENTIAL
000590                             ACCESS MODE              IS  SEQUENTIAL
000600                             FILE STATUS              IS  ó‘ÔƒL[
000610                             LOCK      MODE           IS  AUTOMATIC.
000620******************************************************************
000630*                      DATA DIVISION                             *
000640******************************************************************
000650 DATA                    DIVISION.
000660 FILE                    SECTION.
004330*                           m‚q‚k  1742n
004340 FD  ì‹Æƒtƒ@ƒCƒ‹‚P RECORD  CONTAINS 1742  CHARACTERS.
004350 01 ì‚P|ƒŒƒR[ƒh.
004360    03 ì‚P|ƒŒƒR[ƒhƒL[.
002150       05 ì‚P|¿‹˜a—ï”NŒŽ.
003480          07 ì‚P|¿‹˜a—ï                  PIC 9(1).
002330          07 ì‚P|¿‹”N                    PIC 9(2).
002350          07 ì‚P|¿‹ŒŽ                    PIC 9(2).
004380       05 ì‚P|•ÛŒ¯ŽÒ”Ô†                   PIC X(8).
004420       05 ì‚P|Ž{p˜a—ï”NŒŽ.
004430          07 ì‚P|Ž{p˜a—ï                  PIC 9(1).
004450          07 ì‚P|Ž{p”N                    PIC 9(2).
004460          07 ì‚P|Ž{pŒŽ                    PIC 9(2).
003570       05 ì‚P|”í•ÛŒ¯ŽÒƒJƒi                 PIC X(25).
004390       05 ì‚P|Š³ŽÒƒR[ƒh.
004400          07 ì‚P|Š³ŽÒ”Ô†                  PIC 9(6).
004410          07 ì‚P|Ž}”Ô                      PIC X.
003610       05 ì‚P|•¬‹æ•ª                     PIC 9(1).
003620    03 ì‚P|ƒŒƒR[ƒhƒf[ƒ^.
             05 ì‚P|WŒv‡                       PIC 9(1).
002320       05 ì‚P|¿‹”NŒŽ                     PIC 9(6).
002310       05 ì‚P|‰ïˆõ”Ô†                     PIC 9(7).
002470       05 ì‚P|‹L†.
                07 ì‚P|•ÛŒ¯Ø‹L†                PIC X(24).
002480       05 ì‚P|”Ô†                         PIC X(16).
002510       05 ì‚P|•¬•‰’SŽÒ”Ô†               PIC X(8).
002520       05 ì‚P|•¬Žó‹‹ŽÒ”Ô†               PIC X(16).
002210       05 ì‚P|–{l‰Æ‘°‹æ•ª                 PIC 9(1).
002530       05 ì‚P|”í•ÛŒ¯ŽÒŽ–¼.
                07 ì‚P|”í•ÛŒ¯ŽÒ–¼                PIC X(30).
002560       05 ì‚P|Š³ŽÒƒJƒi                     PIC X(25).
002550       05 ì‚P|Š³ŽÒŽ–¼.
                07 ì‚P|Š³ŽÒ–¼                    PIC X(30).
002680       05 ì‚P|‡Œv‹àŠz                     PIC 9(7).
002700       05 ì‚P|ˆê•”•‰’S‹à                   PIC 9(6).
002710       05 ì‚P|¿‹‹àŠz                     PIC 9(6).
002690       05 ì‚P|‹‹•tŠ„‡                     PIC 9(2).
003020       05 ì‚P|ŽÀ“ú”                       PIC 9(3).
002390       05 ì‚P|Ž{p”NŒŽ                     PIC 9(6).
002210       05 ì‚P|Ä¿‹‹æ•ª                   PIC 9(1).
002610       05 ì‚P|Š³ŽÒ¶”NŒŽ“ú                 PIC 9(8).
002600       05 ì‚P|«•Ê                         PIC 9(1).
002720       05 ì‚P|•”ˆÊ”                       PIC 9(1).
002210       05 ì‚P|ƒŒƒZƒRƒ“‹æ•ª                 PIC 9(2).
002210       05 ì‚P|‚—îŽÒ‹æ•ª                   PIC 9(1).
      */‰^“®Œã—Ã’Ç‰Á«««/20180607
             05 ì‚P|‰^“®Œã—Ã—¿‰ñ”               PIC 9(1).
             05 ì‚P|‰^“®Œã—Ã—¿                   PIC 9(5).
      */‰^“®Œã—Ã’Ç‰Áªªª/20180607
002730       05 ì‚P|ZŠ                         PIC X(60).
002730       05 ì‚P|•‰Œ´ˆö                     PIC X(200).
002730       05 ì‚P|Œo‰ß                         PIC X(50).
002740       05 ì‚P|’·Šú——R                     PIC X(400).
002210       05 ì‚P|V‹K‹æ•ª                     PIC 9(1).
002210       05 ì‚P|Œp‘±‹æ•ª                     PIC 9(1).
002210       05 ì‚P|‰ŒŸ‰ñ”                     PIC 9(1).
002210       05 ì‚P|‰ŒŸ—¿                       PIC 9(5).
002210       05 ì‚P|‰ŒŸŽžŠÔŠO‰ñ”               PIC 9(1).
002210       05 ì‚P|‰ŒŸ‹x“ú‰ñ”                 PIC 9(1).
002210       05 ì‚P|‰ŒŸ[–é‰ñ”                 PIC 9(1).
002210       05 ì‚P|‰ŒŸ‰ÁŽZ                     PIC 9(5).
002210       05 ì‚P|ÄŒŸ‰ñ”                     PIC 9(1).
002210       05 ì‚P|ÄŒŸ—¿                       PIC 9(5).
002210       05 ì‚P|‰—Ã‹——£                     PIC 9(3).
002210       05 ì‚P|‰—Ã‰ñ”                     PIC 9(2).
002210       05 ì‚P|‰—Ã—¿                       PIC 9(5).
002210       05 ì‚P|–éŠÔ‰ñ”                     PIC 9(1).
002210       05 ì‚P|“ï˜H‰ñ”                     PIC 9(1).
002210       05 ì‚P|–\•—‰J‰ñ”                   PIC 9(1).
002210       05 ì‚P|‰—Ã‰ÁŽZ                     PIC 9(5).
      */‹à‘®•›Žq•ÏX«««/20180611
000561       05 ì‚P|‹à‘®•›Žq‰ñ”            PIC 9.
000561       05 ì‚P|ƒ_ƒ~[                  PIC X(2).
002210*       05 ì‚P|‹à‘®‘å‰ñ”                   PIC 9(1).
002210*       05 ì‚P|‹à‘®’†‰ñ”                   PIC 9(1).
002210*       05 ì‚P|‹à‘®¬‰ñ”                   PIC 9(1).
      */‹à‘®•›Žq•ÏXªªª/20180611
002210       05 ì‚P|‹à‘®•›Žq‰ÁŽZ                 PIC 9(5).
002210       05 ì‚P|î•ñ’ñ‹Ÿ‰ñ”                 PIC 9(1).
002210       05 ì‚P|î•ñ’ñ‹Ÿ—¿                   PIC 9(5).
002210       05 ì‚P|‘Š’kŽx‰‡‰ñ”                 PIC 9(1).
002210       05 ì‚P|‘Š’kŽx‰‡—¿                   PIC 9(5).
002210       05 ì‚P|“ú.
002210          07 ì‚P|Ž{p“ú                    PIC X(1) OCCURS 31.
002760       05 ì‚P|•‰•”ˆÊƒf[ƒ^  OCCURS 5.
002770          07 ì‚P|•‰‹æ•ª                  PIC 9(1).
002770          07 ì‚P|•‰–¼                    PIC X(32).
002770          07 ì‚P|®•œ‰ñ”                  PIC 9(1).
002210          07 ì‚P|®•œ—¿                    PIC 9(5).
002780          07 ì‚P|•‰”NŒŽ“ú                PIC 9(8).
002840          07 ì‚P|‰ŒŸ”NŒŽ“ú                PIC 9(8).
002900          07 ì‚P|ŠJŽn”NŒŽ“ú                PIC 9(8).
002960          07 ì‚P|I—¹”NŒŽ“ú                PIC 9(8).
003020          07 ì‚P|•”ˆÊŽÀ“ú”                PIC 9(2).
003030          07 ì‚P|“]‹A‹æ•ª                  PIC 9(1).
002770*          07 ì‚P|Œã—Ã‰ñ”                  PIC 9(2).
002770*          07 ì‚P|—âãª–@‰ñ”                PIC 9(1).
002770*          07 ì‚P|‰·ãª–@‰ñ”                PIC 9(2).
002770*          07 ì‚P|“d—Ã‰ñ”                  PIC 9(2).
003030          07 ì‚P|‘½•”ˆÊ‹æ•ª                PIC 9(1).
003030          07 ì‚P|’·Šú‹æ•ª                  PIC 9(1).
             05 ì‚P|“o˜^‹L†”Ô†                 PIC X(11).
             05 ì‚P|•ÛŒ¯Ží•Ê                     PIC 9(1).
             05 ì‚P|’P•¹‹æ•ª                     PIC 9(1).
             05 ì‚P|–{‰Æ‹æ•ª                     PIC 9(1).
             05 ì‚P|Œö”ïˆê•”•‰’S‹à               PIC 9(6).
             05 ì‚P|Œö”ï¿‹Šz                   PIC 9(6).
002760       05 ì‚P|•”ˆÊ•Ê—¿‹àˆÊƒf[ƒ^  OCCURS 6.
                07 ì‚P|’üŒ¸ŠJŽnŒŽ“ú              PIC 9(4).
                07 ì‚P|Œã—Ã‰ñ”                  PIC 9(2).
                07 ì‚P|Œã—Ã—¿                    PIC 9(5).
                07 ì‚P|—âãª–@‰ñ”                PIC 9(1).
                07 ì‚P|—âãª–@—¿                  PIC 9(5).
                07 ì‚P|‰·ãª–@‰ñ”                PIC 9(2).
                07 ì‚P|‰·ãª–@—¿                  PIC 9(5).
                07 ì‚P|“d—Ã‰ñ”                  PIC 9(2).
                07 ì‚P|“d—Ã—¿                    PIC 9(5).
                07 ì‚P|‘½•”ˆÊ’üŒ¸—¦              PIC 9(2).
                07 ì‚P|‘½•”ˆÊ’üŒ¸Šz              PIC 9(5).
                07 ì‚P|’·Šú’üŒ¸—¦                PIC 9(2).
                07 ì‚P|•”ˆÊ’üŒ¸—¦•Ê—¿‹àŒv        PIC 9(5).
004470*       05 FILLER                             PIC X(14).
004471*
000990*                           m‚q‚k  ‚P‚W‚O‚On
001000 FD  ‰ï’ñoƒtƒ@ƒCƒ‹ RECORD  CONTAINS 1798 CHARACTERS.
001010 01 ‰ï’ñ|ƒŒƒR[ƒh.
004474    03 ‰ï’ñ|ƒŒƒR[ƒhƒf[ƒ^.
002320       05 ‰ï’ñ|¿‹”NŒŽ                     PIC 9(6).
002390       05 ‰ï’ñ|Ž{p”NŒŽ                     PIC 9(6).
002310       05 ‰ï’ñ|‰ïˆõ”Ô†                     PIC 9(7).
             05 ‰ï’ñ|“o˜^‹L†”Ô†                 PIC X(11).
004380       05 ‰ï’ñ|•ÛŒ¯ŽÒ”Ô†                   PIC X(8).
002470       05 ‰ï’ñ|‹L†                         PIC X(30).
002480       05 ‰ï’ñ|”Ô†                         PIC X(16).
003610       05 ‰ï’ñ|•¬‹æ•ª                     PIC 9.
002510       05 ‰ï’ñ|•¬•‰’SŽÒ”Ô†               PIC X(8).
002520       05 ‰ï’ñ|•¬Žó‹‹ŽÒ”Ô†               PIC X(16).
002510       05 ‰ï’ñ|•¬•‰’SŽÒ”Ô†‚Q             PIC X(8).
002520       05 ‰ï’ñ|•¬Žó‹‹ŽÒ”Ô†‚Q             PIC X(16).
             05 ‰ï’ñ|•ÛŒ¯Ží•Ê                     PIC 9(1).
             05 ‰ï’ñ|’P•¹‹æ•ª                     PIC 9(1).
             05 ‰ï’ñ|–{‰Æ‹æ•ª                     PIC 9(1).
002690       05 ‰ï’ñ|‹‹•tŠ„‡                     PIC 9(2).
002210       05 ‰ï’ñ|–{l‰Æ‘°‹æ•ª                 PIC 9.
003570       05 ‰ï’ñ|”í•ÛŒ¯ŽÒƒJƒi                 PIC X(25).
002530       05 ‰ï’ñ|”í•ÛŒ¯ŽÒŽ–¼                 PIC X(30).
002560       05 ‰ï’ñ|Š³ŽÒƒJƒi                     PIC X(25).
002550       05 ‰ï’ñ|Š³ŽÒŽ–¼                     PIC X(30).
002600       05 ‰ï’ñ|«•Ê                         PIC 9(1).
002610       05 ‰ï’ñ|Š³ŽÒ¶”NŒŽ“ú                 PIC 9(8).
002680       05 ‰ï’ñ|‡Œv‹àŠz                     PIC 9(6).
002700       05 ‰ï’ñ|ˆê•”•‰’S‹à                   PIC 9(6).
002710       05 ‰ï’ñ|¿‹‹àŠz                     PIC 9(6).
             05 ‰ï’ñ|Œö”ïˆê•”•‰’S‹à               PIC 9(6).
             05 ‰ï’ñ|Œö”ï¿‹Šz                   PIC 9(6).
003020       05 ‰ï’ñ|ŽÀ“ú”                       PIC 9(2).
002720       05 ‰ï’ñ|•”ˆÊ”                       PIC 9.
002210       05 ‰ï’ñ|Ä¿‹‹æ•ª                   PIC 9.
002210       05 ‰ï’ñ|ƒŒƒZƒRƒ“‹æ•ª                 PIC 9(2).
002210       05 ‰ï’ñ|‚—îŽÒ‹æ•ª                   PIC 9.
             05 ‰ï’ñ|Š³ŽÒ”Ô†                     PIC 9(8).
      */‰^“®Œã—Ã’Ç‰Á«««/20180611
             05 ‰ï’ñ|‰^“®Œã—Ã—¿‰ñ”               PIC 9(1).
             05 ‰ï’ñ|‰^“®Œã—Ã—¿                   PIC 9(5).
      *       05 FILLER                             PIC X(54).
             05 FILLER                             PIC X(48).
      */‰^“®Œã—Ã’Ç‰Áªªª/20180611
002760       05 ‰ï’ñ|•‰•”ˆÊƒf[ƒ^  OCCURS 5.
002770          07 ‰ï’ñ|•‰‹æ•ª                  PIC 9.
002770          07 ‰ï’ñ|•‰–¼                    PIC X(32).
002780          07 ‰ï’ñ|•‰”NŒŽ“ú                PIC 9(8).
002840          07 ‰ï’ñ|‰ŒŸ”NŒŽ“ú                PIC 9(8).
002900          07 ‰ï’ñ|ŠJŽn”NŒŽ“ú                PIC 9(8).
002960          07 ‰ï’ñ|I—¹”NŒŽ“ú                PIC 9(8).
003020          07 ‰ï’ñ|•”ˆÊŽÀ“ú”                PIC 9(2).
003030          07 ‰ï’ñ|“]‹A‹æ•ª                  PIC 9.
002770          07 ‰ï’ñ|®•œ‰ñ”                  PIC 9.
002210          07 ‰ï’ñ|®•œ—¿                    PIC 9(5).
002210       05 ‰ï’ñ|V‹K‹æ•ª                     PIC 9.
002210       05 ‰ï’ñ|Œp‘±‹æ•ª                     PIC 9.
002210       05 ‰ï’ñ|Ž{p“ú                       PIC X(31).
002210       05 ‰ï’ñ|‰ŒŸ‰ñ”                     PIC 9.
002210       05 ‰ï’ñ|‰ŒŸ—¿                       PIC 9(5).
002210       05 ‰ï’ñ|‰ŒŸ‹x“ú‰ñ”                 PIC 9.
002210       05 ‰ï’ñ|‰ŒŸ[–é‰ñ”                 PIC 9.
002210       05 ‰ï’ñ|‰ŒŸŽžŠÔŠO‰ñ”               PIC 9.
002210       05 ‰ï’ñ|‰ŒŸ‰ÁŽZ                     PIC 9(5).
002210       05 ‰ï’ñ|‘Š’kŽx‰‡‰ñ”                 PIC 9.
002210       05 ‰ï’ñ|‘Š’kŽx‰‡—¿                   PIC 9(5).
002210       05 ‰ï’ñ|ÄŒŸ‰ñ”                     PIC 9.
002210       05 ‰ï’ñ|ÄŒŸ—¿                       PIC 9(5).
002210       05 ‰ï’ñ|‰—Ã‹——£                     PIC 9(3).
002210       05 ‰ï’ñ|‰—Ã‰ñ”                     PIC 9(2).
002210       05 ‰ï’ñ|‰—Ã—¿                       PIC 9(5).
002210       05 ‰ï’ñ|–éŠÔ‰ñ”                     PIC 9.
002210       05 ‰ï’ñ|“ï˜H‰ñ”                     PIC 9(1).
002210       05 ‰ï’ñ|–\•—‰J‰ñ”                   PIC 9(1).
002210       05 ‰ï’ñ|‰—Ã‰ÁŽZ                     PIC 9(5).
      */‹à‘®•›Žq•ÏX«««/20180611
000561       05 ‰ï’ñ|‹à‘®•›Žq‰ñ”                 PIC 9.
000561       05 ‰ï’ñ|ƒ_ƒ~[                       PIC X(2).
002210*       05 ‰ï’ñ|‹à‘®‘å‰ñ”                   PIC 9.
002210*       05 ‰ï’ñ|‹à‘®’†‰ñ”                   PIC 9.
002210*       05 ‰ï’ñ|‹à‘®¬‰ñ”                   PIC 9.
      */‹à‘®•›Žq•ÏXªªª/20180607
002210       05 ‰ï’ñ|‹à‘®•ŸŽƒ‰ÁŽZ                 PIC 9(5).
002210       05 ‰ï’ñ|î•ñ’ñ‹Ÿ‰ñ”                 PIC 9.
002210       05 ‰ï’ñ|î•ñ’ñ‹Ÿ—¿                   PIC 9(5).
002760       05 ‰ï’ñ|•”ˆÊ•Ê—¿‹àˆÊƒf[ƒ^  OCCURS 6.
                07 ‰ï’ñ|’üŒ¸ŠJŽnŒŽ“ú              PIC 9(4).
002770          07 ‰ï’ñ|Œã—Ã‰ñ”                  PIC 9(2).
                07 ‰ï’ñ|Œã—Ã—¿                    PIC 9(5).
002770          07 ‰ï’ñ|—âãª–@‰ñ”                PIC 9.
                07 ‰ï’ñ|—âãª–@—¿                  PIC 9(5).
002770          07 ‰ï’ñ|‰·ãª–@‰ñ”                PIC 9(2).
                07 ‰ï’ñ|‰·ãª–@—¿                  PIC 9(5).
002770          07 ‰ï’ñ|“d—Ã‰ñ”                  PIC 9(2).
                07 ‰ï’ñ|“d—Ã—¿                    PIC 9(5).
                07 ‰ï’ñ|‘½•”ˆÊ’üŒ¸—¦              PIC 9(2).
                07 ‰ï’ñ|‘½•”ˆÊ’üŒ¸Šz              PIC 9(5).
                07 ‰ï’ñ|’·Šú’üŒ¸—¦                PIC 9(2).
                07 ‰ï’ñ|•”ˆÊ’üŒ¸—¦•Ê—¿‹àŒv        PIC 9(5).
002730       05 ‰ï’ñ|ZŠ                         PIC X(60).
002730       05 ‰ï’ñ|•‰Œ´ˆö                     PIC X(200).
002730       05 ‰ï’ñ|Œo‰ß                         PIC X(50).
002740       05 ‰ï’ñ|’·Šú——R                     PIC X(400).
001340*----------------------------------------------------------------*
001350******************************************************************
001360*                WORKING-STORAGE SECTION                         *
001370******************************************************************
001380 WORKING-STORAGE         SECTION.
001650 01 ƒL[“ü—Í                         PIC X     VALUE SPACE.
001660 01 ó‘ÔƒL[                         PIC X(2)  VALUE SPACE.
001670 01 I—¹ƒtƒ‰ƒO                       PIC X(3)  VALUE SPACE.
001680 01 I—¹ƒtƒ‰ƒO‚Q                     PIC X(3)  VALUE SPACE.
001690 01 •”ˆÊ‚b‚m‚s                       PIC 9(2)  VALUE ZERO.
001700 01 “ú•t‚b‚m‚s                       PIC 9(2)  VALUE ZERO.
001710 01 ŽÀsƒL[‚v                       PIC X(3)  VALUE SPACE.
001780 01 ‘SŠp‹ó”’                         PIC X(2)  VALUE X"8140".
001790 01 ”¼Šp‹ó”’                         PIC X(2)  VALUE X"2020".
001860* **************
001870* * €–Ú‘Ò”ð—p *
001880* **************
001940 01 ƒtƒ@ƒCƒ‹–¼                       PIC N(10) VALUE SPACE.
001950*
002250* ƒtƒƒbƒs[ƒpƒX—p
002251 01 ’èŠúC³‚e‚c‚v                     PIC X VALUE SPACE.
002252 01 FD-NAME                            PIC X(30) VALUE SPACE.
      *
      *0:’Êí 1:ŒÅ’èƒpƒX
       01 ˜A“ü|ŒÅ’èƒtƒ‰ƒO‚R‚Q‚P‚P IS EXTERNAL.
          03 ˜A“ü|ŒÅ’èƒtƒ‰ƒO                PIC 9(1).
001490*
002310******************************************************************
002320*                          ˜AŒ‹€–Ú                              *
002330******************************************************************
002340*
002350****************
002360* ‰æ–Ê“ü—Íî•ñ *
002370****************
        01 ˜A“ü|‰æ–Êî•ñ‚x‚`‚r‚P‚O‚O IS EXTERNAL.
           03 ˜A“ü|ƒhƒ‰ƒCƒu                  PIC X(1).
002440**********************
002450* ƒƒbƒZ[ƒW•\Ž¦ƒL[ *
002460**********************
002470 01 ˜Aƒ|ƒL[ IS EXTERNAL.
002480    03  ˜Aƒ|ƒƒbƒZ[ƒW                 PIC N(20).
002490*
004091 01 ˜Aƒ‚V|ƒL[ IS EXTERNAL.
004092    03  ˜Aƒ‚V|ƒƒbƒZ[ƒW‚P               PIC X(40).
004102    03  ˜Aƒ‚V|ƒƒbƒZ[ƒW‚Q               PIC X(40).
002520******************************************************************
002530*                      PROCEDURE  DIVISION                       *
002540******************************************************************
002550 PROCEDURE               DIVISION.
002560************
002570*           *
002580* ‰Šúˆ—   *
002590*           *
002600************
002620     PERFORM ƒtƒ@ƒCƒ‹ƒI[ƒvƒ“.
002640************
002650*           *
002660* Žåˆ—     *
002670*           *
002680************
002690     MOVE SPACE TO I—¹ƒtƒ‰ƒO.
002700     PERFORM ì‹Æƒtƒ@ƒCƒ‹‚P“Çž.
002710     IF I—¹ƒtƒ‰ƒO = "YES"
002720             MOVE  NC"@ƒf[ƒ^‚ª‚OŒ‚Å‚·BŠm”F‚µ‚Ä‰º‚³‚¢B" TO ˜Aƒ|ƒƒbƒZ[ƒW
002730             CALL   "MSG001"
002740             CANCEL "MSG001"
002750             PERFORM I—¹ˆ—
002760             MOVE 99 TO PROGRAM-STATUS
002770             EXIT PROGRAM
002780     END-IF.
002810     PERFORM UNTIL I—¹ƒtƒ‰ƒO = "YES"
002820         PERFORM ƒf[ƒ^ƒŒƒR[ƒhƒZƒbƒg
002830         PERFORM ƒŒƒR[ƒh‘ž
002840         PERFORM ì‹Æƒtƒ@ƒCƒ‹‚P“Çž
002850     END-PERFORM.
002880************
002890*           *
002900* I—¹ˆ—   *
002910*           *
002920************
002930     PERFORM I—¹ˆ—.
002940     MOVE ZERO TO PROGRAM-STATUS.
002950     EXIT PROGRAM.
002960*
002970*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
003610*================================================================*
003620 ƒtƒ@ƒCƒ‹ƒI[ƒvƒ“ SECTION.
003630*
           IF ˜A“ü|ŒÅ’èƒtƒ‰ƒO = 1
002890        MOVE "C:\makishisys\RYOYOHI.DAT" TO FD-NAME
           ELSE
003324        MOVE ˜A“ü|ƒhƒ‰ƒCƒu TO ’èŠúC³‚e‚c‚v
003326        IF ’èŠúC³‚e‚c‚v = SPACE
003327           MOVE "A" TO ’èŠúC³‚e‚c‚v
003328        END-IF
003330        STRING ’èŠúC³‚e‚c‚v DELIMITED BY SIZE
                    ":\RYOYOHI.DAT" DELIMITED BY SIZE
003334              INTO FD-NAME
003335        END-STRING
           END-IF.
      *
003760     OPEN OUTPUT ‰ï’ñoƒtƒ@ƒCƒ‹.
003770     IF ó‘ÔƒL[  NOT =  "00"
002690         MOVE  "Žw’è‚³‚ê‚½ƒhƒ‰ƒCƒu‚ª‚È‚¢‚©A" TO ˜Aƒ‚V|ƒƒbƒZ[ƒW‚P
               MOVE  "‘‚«ž‚Þ‚±‚Æ‚ªo—ˆ‚Ü‚¹‚ñB" TO ˜Aƒ‚V|ƒƒbƒZ[ƒW‚Q
002700         CALL   "MSG007"
002710         CANCEL "MSG007"
003820         MOVE 99 TO PROGRAM-STATUS
003830         EXIT PROGRAM
003870     END-IF.
      *
003730     OPEN INPUT ì‹Æƒtƒ@ƒCƒ‹‚P.
003740         MOVE NC"ì‹Æƒtƒ@ƒCƒ‹‚P" TO ƒtƒ@ƒCƒ‹–¼.
003750         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
      *
003880*================================================================*
003890 ƒI[ƒvƒ“ƒ`ƒFƒbƒN SECTION.
003900*
003910     IF ó‘ÔƒL[  NOT =  "00"
003920         DISPLAY ƒtƒ@ƒCƒ‹–¼ NC"‚eƒI[ƒvƒ“ƒGƒ‰[" UPON CONS
003930         DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[         UPON CONS
003940         DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢"
003950                                                 UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
003960         ACCEPT  ƒL[“ü—Í FROM CONS
003970         PERFORM ƒtƒ@ƒCƒ‹•Â½
003980         MOVE 99 TO PROGRAM-STATUS
003990         EXIT PROGRAM.
004000*================================================================*
004010 ƒGƒ‰[•\Ž¦ SECTION.
004020*
004030     DISPLAY NC"ó‘ÔƒL[" ó‘ÔƒL[  UPON CONS.
004040     DISPLAY NC"ƒtƒ@ƒCƒ‹‘žƒGƒ‰[F" ƒtƒ@ƒCƒ‹–¼   UPON CONS.
004050     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
004060     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢"                                                                    UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
004070     ACCEPT  ƒL[“ü—Í FROM CONS.
004080     PERFORM ƒtƒ@ƒCƒ‹•Â½.
004090     MOVE 99 TO PROGRAM-STATUS.
004100     EXIT PROGRAM.
004110*================================================================*
004120 ì‹Æƒtƒ@ƒCƒ‹‚P“Çž SECTION.
004130*
004140     READ ì‹Æƒtƒ@ƒCƒ‹‚P NEXT
004150     AT END
004160         MOVE "YES" TO I—¹ƒtƒ‰ƒO
004170     END-READ.
004290*================================================================*
004300 ƒŒƒR[ƒh‘ž SECTION.
004310*
004320     WRITE ‰ï’ñ|ƒŒƒR[ƒh.
004330     IF ó‘ÔƒL[  NOT =  "00"
004340         MOVE NC"‰ï’ñ"  TO ƒtƒ@ƒCƒ‹–¼
004350         PERFORM ƒGƒ‰[•\Ž¦
004360     END-IF.
004370*================================================================*
004380 ƒtƒ@ƒCƒ‹•Â½ SECTION.
004390*
004400     CLOSE ‰ï’ñoƒtƒ@ƒCƒ‹.
004410*================================================================*
004420 I—¹ˆ— SECTION.
004430*
004440     PERFORM ƒtƒ@ƒCƒ‹•Â½.
005340*================================================================*
005350 ƒf[ƒ^ƒŒƒR[ƒhƒZƒbƒg SECTION.
005360*
005907     MOVE SPACE TO ‰ï’ñ|ƒŒƒR[ƒh.
005916     INITIALIZE    ‰ï’ñ|ƒŒƒR[ƒh.
005926*
006004     MOVE ì‚P|¿‹”NŒŽ         TO  ‰ï’ñ|¿‹”NŒŽ.
005972     MOVE ì‚P|‰ïˆõ”Ô†         TO  ‰ï’ñ|‰ïˆõ”Ô†.
005976     MOVE ì‚P|•ÛŒ¯ŽÒ”Ô†       TO  ‰ï’ñ|•ÛŒ¯ŽÒ”Ô†.
005977     MOVE ì‚P|‹L†             TO  ‰ï’ñ|‹L†.
005978     MOVE ì‚P|”Ô†             TO  ‰ï’ñ|”Ô†.
005978     MOVE ì‚P|•¬‹æ•ª         TO  ‰ï’ñ|•¬‹æ•ª.
005995     MOVE ì‚P|•¬•‰’SŽÒ”Ô†   TO  ‰ï’ñ|•¬•‰’SŽÒ”Ô†.
005995     MOVE ì‚P|•¬Žó‹‹ŽÒ”Ô†   TO  ‰ï’ñ|•¬Žó‹‹ŽÒ”Ô†.
005979     MOVE ì‚P|–{l‰Æ‘°‹æ•ª     TO  ‰ï’ñ|–{l‰Æ‘°‹æ•ª.
005980     MOVE ì‚P|”í•ÛŒ¯ŽÒƒJƒi     TO  ‰ï’ñ|”í•ÛŒ¯ŽÒƒJƒi.
005981     MOVE ì‚P|”í•ÛŒ¯ŽÒŽ–¼     TO  ‰ï’ñ|”í•ÛŒ¯ŽÒŽ–¼.
005983     MOVE ì‚P|Š³ŽÒƒJƒi         TO  ‰ï’ñ|Š³ŽÒƒJƒi.
005984     MOVE ì‚P|Š³ŽÒŽ–¼         TO  ‰ï’ñ|Š³ŽÒŽ–¼.
005992     MOVE ì‚P|‡Œv‹àŠz         TO  ‰ï’ñ|‡Œv‹àŠz.
005992     MOVE ì‚P|ˆê•”•‰’S‹à       TO  ‰ï’ñ|ˆê•”•‰’S‹à.
005992     MOVE ì‚P|¿‹‹àŠz         TO  ‰ï’ñ|¿‹‹àŠz.
006003     MOVE ì‚P|‹‹•tŠ„‡         TO  ‰ï’ñ|‹‹•tŠ„‡.
005992     MOVE ì‚P|ŽÀ“ú”           TO  ‰ï’ñ|ŽÀ“ú”.
006004     MOVE ì‚P|Ž{p”NŒŽ         TO  ‰ï’ñ|Ž{p”NŒŽ.
006004     MOVE ì‚P|Ä¿‹‹æ•ª       TO  ‰ï’ñ|Ä¿‹‹æ•ª.
006004     MOVE ì‚P|Š³ŽÒ¶”NŒŽ“ú     TO  ‰ï’ñ|Š³ŽÒ¶”NŒŽ“ú.
005977     MOVE ì‚P|«•Ê             TO  ‰ï’ñ|«•Ê.
006003     MOVE ì‚P|•”ˆÊ”           TO  ‰ï’ñ|•”ˆÊ”.
005977     MOVE ì‚P|ƒŒƒZƒRƒ“‹æ•ª     TO  ‰ï’ñ|ƒŒƒZƒRƒ“‹æ•ª.
005993     MOVE ì‚P|‚—îŽÒ‹æ•ª       TO  ‰ï’ñ|‚—îŽÒ‹æ•ª.
005977     MOVE ì‚P|ZŠ             TO  ‰ï’ñ|ZŠ.
005977     MOVE ì‚P|•‰Œ´ˆö         TO  ‰ï’ñ|•‰Œ´ˆö.
005977     MOVE ì‚P|Œo‰ß             TO  ‰ï’ñ|Œo‰ß.
005977     MOVE ì‚P|’·Šú——R         TO  ‰ï’ñ|’·Šú——R.
005977     MOVE ì‚P|V‹K‹æ•ª         TO  ‰ï’ñ|V‹K‹æ•ª.
005977     MOVE ì‚P|Œp‘±‹æ•ª         TO  ‰ï’ñ|Œp‘±‹æ•ª.
005977     MOVE ì‚P|‰ŒŸ‰ñ”         TO  ‰ï’ñ|‰ŒŸ‰ñ”.
005977     MOVE ì‚P|‰ŒŸ—¿           TO  ‰ï’ñ|‰ŒŸ—¿.
005977     MOVE ì‚P|‰ŒŸŽžŠÔŠO‰ñ”   TO  ‰ï’ñ|‰ŒŸŽžŠÔŠO‰ñ”.
005977     MOVE ì‚P|‰ŒŸ‹x“ú‰ñ”     TO  ‰ï’ñ|‰ŒŸ‹x“ú‰ñ”.
005977     MOVE ì‚P|‰ŒŸ[–é‰ñ”     TO  ‰ï’ñ|‰ŒŸ[–é‰ñ”.
005977     MOVE ì‚P|‰ŒŸ‰ÁŽZ         TO  ‰ï’ñ|‰ŒŸ‰ÁŽZ.
005977     MOVE ì‚P|ÄŒŸ‰ñ”         TO  ‰ï’ñ|ÄŒŸ‰ñ”.
005977     MOVE ì‚P|ÄŒŸ—¿           TO  ‰ï’ñ|ÄŒŸ—¿.
005977     MOVE ì‚P|‰—Ã‹——£         TO  ‰ï’ñ|‰—Ã‹——£.
005977     MOVE ì‚P|‰—Ã‰ñ”         TO  ‰ï’ñ|‰—Ã‰ñ”.
005977     MOVE ì‚P|‰—Ã—¿           TO  ‰ï’ñ|‰—Ã—¿.
005977     MOVE ì‚P|–éŠÔ‰ñ”         TO  ‰ï’ñ|–éŠÔ‰ñ”.
005977     MOVE ì‚P|“ï˜H‰ñ”         TO  ‰ï’ñ|“ï˜H‰ñ”.
005977     MOVE ì‚P|–\•—‰J‰ñ”       TO  ‰ï’ñ|–\•—‰J‰ñ”.
005977     MOVE ì‚P|‰—Ã‰ÁŽZ         TO  ‰ï’ñ|‰—Ã‰ÁŽZ.
      */‹à‘®•›Žq‰^“®Œã—Ã’Ç‰Á«««/20180611
004610     MOVE ì‚P|‹à‘®•›Žq‰ñ”     TO  ‰ï’ñ|‹à‘®•›Žq‰ñ”.
004610     MOVE SPACE                  TO  ‰ï’ñ|ƒ_ƒ~[.
004610     MOVE ì‚P|‰^“®Œã—Ã—¿‰ñ”   TO  ‰ï’ñ|‰^“®Œã—Ã—¿‰ñ”.
004610     MOVE ì‚P|‰^“®Œã—Ã—¿       TO  ‰ï’ñ|‰^“®Œã—Ã—¿.
005977*     MOVE ì‚P|‹à‘®‘å‰ñ”       TO  ‰ï’ñ|‹à‘®‘å‰ñ”.
005977*     MOVE ì‚P|‹à‘®’†‰ñ”       TO  ‰ï’ñ|‹à‘®’†‰ñ”.
005977*     MOVE ì‚P|‹à‘®¬‰ñ”       TO  ‰ï’ñ|‹à‘®¬‰ñ”.
      */‹à‘®•›Žq‰^“®Œã—Ã’Ç‰Áªªª/20180611
005977     MOVE ì‚P|‹à‘®•›Žq‰ÁŽZ     TO  ‰ï’ñ|‹à‘®•ŸŽƒ‰ÁŽZ.
005977     MOVE ì‚P|î•ñ’ñ‹Ÿ‰ñ”     TO  ‰ï’ñ|î•ñ’ñ‹Ÿ‰ñ”.
005977     MOVE ì‚P|î•ñ’ñ‹Ÿ—¿       TO  ‰ï’ñ|î•ñ’ñ‹Ÿ—¿.
005977     MOVE ì‚P|‘Š’kŽx‰‡‰ñ”     TO  ‰ï’ñ|‘Š’kŽx‰‡‰ñ”.
005977     MOVE ì‚P|‘Š’kŽx‰‡—¿       TO  ‰ï’ñ|‘Š’kŽx‰‡—¿.
005977     MOVE ì‚P|“ú               TO  ‰ï’ñ|Ž{p“ú.
006006*     MOVE SPACE                  TO  ‰ï’ñ|—\”õ.
           PERFORM VARYING •”ˆÊ‚b‚m‚s FROM 1 BY 1
                   UNTIL   •”ˆÊ‚b‚m‚s > 5
005977         MOVE ì‚P|•‰‹æ•ª(•”ˆÊ‚b‚m‚s)    TO  ‰ï’ñ|•‰‹æ•ª(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|•‰–¼(•”ˆÊ‚b‚m‚s)      TO  ‰ï’ñ|•‰–¼(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|®•œ‰ñ”(•”ˆÊ‚b‚m‚s)    TO  ‰ï’ñ|®•œ‰ñ”(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|®•œ—¿(•”ˆÊ‚b‚m‚s)      TO  ‰ï’ñ|®•œ—¿(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|•‰”NŒŽ“ú(•”ˆÊ‚b‚m‚s)  TO  ‰ï’ñ|•‰”NŒŽ“ú(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|‰ŒŸ”NŒŽ“ú(•”ˆÊ‚b‚m‚s)  TO  ‰ï’ñ|‰ŒŸ”NŒŽ“ú(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|ŠJŽn”NŒŽ“ú(•”ˆÊ‚b‚m‚s)  TO  ‰ï’ñ|ŠJŽn”NŒŽ“ú(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|I—¹”NŒŽ“ú(•”ˆÊ‚b‚m‚s)  TO  ‰ï’ñ|I—¹”NŒŽ“ú(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|•”ˆÊŽÀ“ú”(•”ˆÊ‚b‚m‚s)  TO  ‰ï’ñ|•”ˆÊŽÀ“ú”(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|“]‹A‹æ•ª(•”ˆÊ‚b‚m‚s)    TO  ‰ï’ñ|“]‹A‹æ•ª(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|Œã—Ã‰ñ”(•”ˆÊ‚b‚m‚s)    TO  ‰ï’ñ|Œã—Ã‰ñ”(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|—âãª–@‰ñ”(•”ˆÊ‚b‚m‚s)  TO  ‰ï’ñ|—âãª–@‰ñ”(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|‰·ãª–@‰ñ”(•”ˆÊ‚b‚m‚s)  TO  ‰ï’ñ|‰·ãª–@‰ñ”(•”ˆÊ‚b‚m‚s)
005977         MOVE ì‚P|“d—Ã‰ñ”(•”ˆÊ‚b‚m‚s)    TO  ‰ï’ñ|“d—Ã‰ñ”(•”ˆÊ‚b‚m‚s)
005977*         MOVE ì‚P|‘½•”ˆÊ‹æ•ª(•”ˆÊ‚b‚m‚s)  TO  ‰ï’ñ|‘½•”ˆÊ‹æ•ª(•”ˆÊ‚b‚m‚s)
005977*         MOVE ì‚P|’·Šú‹æ•ª(•”ˆÊ‚b‚m‚s)    TO  ‰ï’ñ|’·Šú‹æ•ª(•”ˆÊ‚b‚m‚s)
           END-PERFORM.
           MOVE ì‚P|“o˜^‹L†”Ô†   TO ‰ï’ñ|“o˜^‹L†”Ô†  .
           MOVE ì‚P|•ÛŒ¯Ží•Ê       TO ‰ï’ñ|•ÛŒ¯Ží•Ê      .
           MOVE ì‚P|’P•¹‹æ•ª       TO ‰ï’ñ|’P•¹‹æ•ª      .
           MOVE ì‚P|–{‰Æ‹æ•ª       TO ‰ï’ñ|–{‰Æ‹æ•ª      .
           MOVE ì‚P|Œö”ïˆê•”•‰’S‹à TO ‰ï’ñ|Œö”ïˆê•”•‰’S‹à.
           MOVE ì‚P|Œö”ï¿‹Šz     TO ‰ï’ñ|Œö”ï¿‹Šz    .
           PERFORM VARYING •”ˆÊ‚b‚m‚s FROM 1 BY 1 UNTIL •”ˆÊ‚b‚m‚s > 6
               MOVE ì‚P|’üŒ¸ŠJŽnŒŽ“ú      (•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|’üŒ¸ŠJŽnŒŽ“ú      (•”ˆÊ‚b‚m‚s)
               MOVE ì‚P|Œã—Ã‰ñ”          (•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|Œã—Ã‰ñ”          (•”ˆÊ‚b‚m‚s)
               MOVE ì‚P|Œã—Ã—¿            (•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|Œã—Ã—¿            (•”ˆÊ‚b‚m‚s)
               MOVE ì‚P|—âãª–@‰ñ”        (•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|—âãª–@‰ñ”        (•”ˆÊ‚b‚m‚s)
               MOVE ì‚P|—âãª–@—¿          (•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|—âãª–@—¿          (•”ˆÊ‚b‚m‚s)
               MOVE ì‚P|‰·ãª–@‰ñ”        (•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|‰·ãª–@‰ñ”        (•”ˆÊ‚b‚m‚s)
               MOVE ì‚P|‰·ãª–@—¿          (•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|‰·ãª–@—¿          (•”ˆÊ‚b‚m‚s)
               MOVE ì‚P|“d—Ã‰ñ”          (•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|“d—Ã‰ñ”          (•”ˆÊ‚b‚m‚s)
               MOVE ì‚P|“d—Ã—¿            (•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|“d—Ã—¿            (•”ˆÊ‚b‚m‚s)
               MOVE ì‚P|‘½•”ˆÊ’üŒ¸—¦      (•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|‘½•”ˆÊ’üŒ¸—¦      (•”ˆÊ‚b‚m‚s)
               MOVE ì‚P|‘½•”ˆÊ’üŒ¸Šz      (•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|‘½•”ˆÊ’üŒ¸Šz      (•”ˆÊ‚b‚m‚s)
               MOVE ì‚P|’·Šú’üŒ¸—¦        (•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|’·Šú’üŒ¸—¦        (•”ˆÊ‚b‚m‚s)
               MOVE ì‚P|•”ˆÊ’üŒ¸—¦•Ê—¿‹àŒv(•”ˆÊ‚b‚m‚s) TO ‰ï’ñ|•”ˆÊ’üŒ¸—¦•Ê—¿‹àŒv(•”ˆÊ‚b‚m‚s)
           END-PERFORM.
      */ƒŒƒCƒAƒEƒg•ÏX130718
           MOVE ì‚P|Š³ŽÒ”Ô† TO ‰ï’ñ|Š³ŽÒ”Ô†.
005600*================================================================*
005610******************************************************************
005620 END PROGRAM YAS1021.
005630******************************************************************
