000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             KHT41410.
000060 AUTHOR.                 ŽR“c  _”V
000070*
000080*----------------------------------------------------------------*
000090*         ‚Í‚è—p •‰’SŠzŒvŽZƒvƒƒOƒ‰ƒ€@
000100*         MED = 
000110* •½¬‚P‚S”N‚P‚OŒŽ•ª¿‹‚©‚çŽg—pŠJŽn
000120* ‚RÎ–¢–ž‚QŠ„•‰’SA‚—îŽÒ‚PC‚QŠ„•‰’S‚Ì‘Î‰žB
000130*
000140* ‘åã˜Vl•‰’S‹à–Æœ 500‰~~2‰ñ ‚Ì‘Î‰ž(“úŒvŽž‚Ì‚ÝŒvŽZ) /20061120
000150*
000151* •¬‚Ì“ÁŽê•‰’S‚ÍˆÈ‰º‚Ì‚Ý‘Î‰žB
000152* •‰’S‹æ•ª:6,8,14 (–ˆŒŽˆê’è‹àŠzAˆê’è‰ñ”‚ð•‰’S)
000153*          13  (–ˆŒŽˆê’è‹àŠzAˆê’è‰ñ”‚ð•‰’Si‚P‚O‰~’PˆÊj)
000154*
000155* •‰’S‹æ•ª:5,16,17‘Î‰ž(2014.11.05)
000156*
000160*----------------------------------------------------------------*
000170 DATE-WRITTEN.           2004-08-31
000180 DATE-COMPILED.          2004-08-31
000190*----------------------------------------------------------------*
000200******************************************************************
000210*            ENVIRONMENT         DIVISION                        *
000220******************************************************************
000230 ENVIRONMENT             DIVISION.
000240 CONFIGURATION           SECTION.
000250 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000260 OBJECT-COMPUTER.        FMV-DESKPOWER.
000270 SPECIAL-NAMES.          CONSOLE  IS  CONS
000280                         SYSERR   IS  MSGBOX.
000290 INPUT-OUTPUT            SECTION.
000300 FILE-CONTROL.
000310* EXTERNAL—p
000320     SELECT  •ÛŒ¯ŽÒƒ}ƒXƒ^    ASSIGN      TO        HOKENSL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  •Û|•ÛŒ¯Ží•Ê
000360                                                          •Û|•ÛŒ¯ŽÒ”Ô†
000370                             ALTERNATE RECORD KEY     IS  •Û|•ÛŒ¯Ží•Ê
000380                                                          •Û|•ÛŒ¯ŽÒ–¼Ì
000390                                                          •Û|•ÛŒ¯ŽÒ”Ô†
000400                             FILE STATUS              IS  ó‘ÔƒL[
000410                             LOCK        MODE         IS  AUTOMATIC.
000420     SELECT  •‰’S—¦ƒ}ƒXƒ^    ASSIGN      TO        HUTANRIL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS  •‰—¦|•ÛŒ¯Ží•Ê
000460                                                          •‰—¦|ŠJŽn˜a—ï”NŒŽ
000470                             FILE STATUS              IS  ó‘ÔƒL[
000480                             LOCK        MODE         IS  AUTOMATIC.
000490     SELECT  ŽófŽÒî•ñ‚e    ASSIGN      TO        JUSINJL
000500                             ORGANIZATION             IS  INDEXED
000510                             ACCESS MODE              IS  DYNAMIC
000520                             RECORD KEY               IS  Žó|Ž{p˜a—ï”NŒŽ
000530                                                          Žó|Š³ŽÒƒR[ƒh
000540                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000550                                                          Žó|Š³ŽÒƒJƒi
000560                                                          Žó|Š³ŽÒƒR[ƒh
000570                             ALTERNATE RECORD KEY     IS  Žó|Š³ŽÒƒR[ƒh
000580                                                          Žó|Ž{p˜a—ï”NŒŽ
000590                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000600                                                          Žó|•ÛŒ¯Ží•Ê
000610                                                          Žó|•ÛŒ¯ŽÒ”Ô†
000620                                                          Žó|Š³ŽÒƒR[ƒh
000630                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000640                                                          Žó|Œö”ïŽí•Ê
000650                                                          Žó|”ï—p•‰’SŽÒ”Ô†
000660                                                          Žó|Š³ŽÒƒR[ƒh
000670                             ALTERNATE RECORD KEY     IS  Žó|Ž{p˜a—ï”NŒŽ
000680                                                          Žó|•¬Ží•Ê
000690                                                          Žó|”ï—p•‰’SŽÒ”Ô†•¬
000700                                                          Žó|Š³ŽÒƒR[ƒh
000710                             ALTERNATE RECORD KEY     IS  Žó|¿‹˜a—ï”NŒŽ
000720                                                          Žó|Ž{p˜a—ï”NŒŽ
000730                                                          Žó|Š³ŽÒƒR[ƒh
000740                             FILE STATUS              IS  ó‘ÔƒL[
000750                             LOCK        MODE         IS  AUTOMATIC.
000760     SELECT  Žs’¬‘ºƒ}ƒXƒ^    ASSIGN      TO        SITYOSNL
000770                             ORGANIZATION             IS  INDEXED
000780                             ACCESS MODE              IS  DYNAMIC
000790                             RECORD KEY               IS  Žs|Œö”ïŽí•Ê
000800                                                          Žs|Žs’¬‘º”Ô†
000810                             ALTERNATE RECORD KEY     IS  Žs|Œö”ïŽí•Ê
000820                                                          Žs|Žs’¬‘º–¼Ì
000830                                                          Žs|Žs’¬‘º”Ô†
000840                             FILE STATUS              IS  ó‘ÔƒL[
000850                             LOCK        MODE         IS  AUTOMATIC.
000860*
000861**
000870     SELECT  •ÛŒ¯ŽÒ“Á•Ê•‰’Sƒ}ƒXƒ^   ASSIGN      TO     HOKENTKL
000871                             ORGANIZATION          IS  INDEXED
000872                             ACCESS MODE           IS  DYNAMIC
000873                             RECORD KEY            IS  •Û“Á|•ÛŒ¯Ží•Ê
000874                                                       •Û“Á|•ÛŒ¯ŽÒ”Ô†
000875                                                       •Û“Á|ŠJŽn˜a—ï”NŒŽ
000876                             FILE STATUS           IS  ó‘ÔƒL[
000877                             LOCK        MODE      IS  AUTOMATIC.
000520     SELECT  ‚d‚wŽófŽÒî•ñ‚e    ASSIGN      TO        JUSINJL
000530                             ORGANIZATION             IS  INDEXED
000540                             ACCESS MODE              IS  DYNAMIC
000550                             RECORD KEY               IS  ‚dŽó|Ž{p˜a—ï”NŒŽ
000560                                                          ‚dŽó|Š³ŽÒƒR[ƒh
000570                             ALTERNATE RECORD KEY     IS  ‚dŽó|Ž{p˜a—ï”NŒŽ
000580                                                          ‚dŽó|Š³ŽÒƒJƒi
000590                                                          ‚dŽó|Š³ŽÒƒR[ƒh
000600                             ALTERNATE RECORD KEY     IS  ‚dŽó|Š³ŽÒƒR[ƒh
000610                                                          ‚dŽó|Ž{p˜a—ï”NŒŽ
000620                             ALTERNATE RECORD KEY     IS  ‚dŽó|Ž{p˜a—ï”NŒŽ
000630                                                          ‚dŽó|•ÛŒ¯Ží•Ê
000640                                                          ‚dŽó|•ÛŒ¯ŽÒ”Ô†
000650                                                          ‚dŽó|Š³ŽÒƒR[ƒh
000660                             ALTERNATE RECORD KEY     IS  ‚dŽó|Ž{p˜a—ï”NŒŽ
000670                                                          ‚dŽó|Œö”ïŽí•Ê
000680                                                          ‚dŽó|”ï—p•‰’SŽÒ”Ô†
000690                                                          ‚dŽó|Š³ŽÒƒR[ƒh
000700                             ALTERNATE RECORD KEY     IS  ‚dŽó|Ž{p˜a—ï”NŒŽ
000710                                                          ‚dŽó|•¬Ží•Ê
000720                                                          ‚dŽó|”ï—p•‰’SŽÒ”Ô†•¬
000730                                                          ‚dŽó|Š³ŽÒƒR[ƒh
000740                             ALTERNATE RECORD KEY     IS  ‚dŽó|¿‹˜a—ï”NŒŽ
000750                                                          ‚dŽó|Ž{p˜a—ï”NŒŽ
000760                                                          ‚dŽó|Š³ŽÒƒR[ƒh
000770                             FILE STATUS              IS  ó‘ÔƒL[
000780                             LOCK        MODE         IS  AUTOMATIC.
000878**
001570     SELECT  ‚g“úŒvƒf[ƒ^‚e  ASSIGN      TO        HNIKEIL
001580                             ORGANIZATION             IS  INDEXED
001590                             ACCESS MODE              IS  DYNAMIC
001600                             RECORD KEY               IS  ‚g“ú|Ž{p‹æ•ª
001610                                                          ‚g“ú|Ž{p˜a—ï”NŒŽ“ú
001620                                                          ‚g“ú|Š³ŽÒƒR[ƒh
001630                             ALTERNATE RECORD KEY     IS  ‚g“ú|Ž{p‹æ•ª
001640                                                          ‚g“ú|Š³ŽÒƒR[ƒh
001650                                                          ‚g“ú|Ž{p˜a—ï”NŒŽ“ú
001660                             ALTERNATE RECORD KEY     IS  ‚g“ú|Ž{p˜a—ï”NŒŽ“ú
001670                                                          ‚g“ú|“o˜^‡
001680                             FILE STATUS              IS  ó‘ÔƒL[
001690                             LOCK        MODE         IS  AUTOMATIC.
001700*
000879******************************************************************
000880*                      DATA DIVISION                             *
000890******************************************************************
000900 DATA                    DIVISION.
000910 FILE                    SECTION.
000920*-----------------------------------------------------------------------*
000930** EXTERNAL—p
000940 FD  ŽófŽÒî•ñ‚e IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
000950     COPY JUSINJ     OF  XFDLIB  JOINING   Žó   AS  PREFIX.
000960 FD  •ÛŒ¯ŽÒƒ}ƒXƒ^ IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
000970     COPY HOKENS     OF  XFDLIB  JOINING   •Û   AS  PREFIX.
000980 FD  •‰’S—¦ƒ}ƒXƒ^ IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
000990     COPY HUTANRI    OF  XFDLIB  JOINING   •‰—¦ AS  PREFIX.
001000 FD  Žs’¬‘ºƒ}ƒXƒ^ IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
001010     COPY SITYOSN    OF  XFDLIB  JOINING   Žs   AS  PREFIX.
001011 FD  •ÛŒ¯ŽÒ“Á•Ê•‰’Sƒ}ƒXƒ^  IS EXTERNAL BLOCK   CONTAINS   1   RECORDS.
001012     COPY HOKENTK    OF  XFDLIB  JOINING   •Û“Á AS PREFIX.
001620*                           m‚q‚k  ‚R‚Q‚On
001630 FD  ‚d‚wŽófŽÒî•ñ‚e  IS EXTERNAL      BLOCK   CONTAINS   1   RECORDS.
001640     COPY JUSINJ     OF  XFDLIB  JOINING   ‚dŽó   AS  PREFIX.
003060*                           m‚q‚k  ‚T‚P‚Qn
003070 FD  ‚g“úŒvƒf[ƒ^‚e      BLOCK   CONTAINS   1   RECORDS.
003080     COPY H_NIKEI    OF  XFDLIB  JOINING   ‚g“ú   AS  PREFIX.
001020*
001030*-----------------------------------------------------------------------//
001040
001050*----------------------------------------------------------------*
001060******************************************************************
001070*                WORKING-STORAGE SECTION                         *
001080******************************************************************
001090 WORKING-STORAGE         SECTION.
001100 01 ƒL[“ü—Í                           PIC X    VALUE SPACE.
001110 01 ó‘ÔƒL[                           PIC X(2) VALUE SPACE.
001120 01 I—¹ƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
001130 01 ƒtƒ@ƒCƒ‹–¼‚v                       PIC N(2) VALUE SPACE.
001140*/ƒGƒ‰[•\Ž¦‚ÌC³
001150 01 ƒGƒ‰[•\Ž¦‚e                       PIC 9(1) VALUE ZERO.
001160*/•‰’SŠz‚Æ¿‹Šz‚ð‚UŒ…‚ÉC³0311
001170 01 •‰’SŠz‚v                           PIC 9(6) VALUE ZERO.
001180 01 •‰’SŠz‚v‚Q                         PIC 9(6) VALUE ZERO.
001190 01 •‰’SŠz‚v‚P‚O                       PIC 9(6)V9 VALUE ZERO.
001200 01 ¿‹Šz‚v                           PIC 9(6) VALUE ZERO.
001210 01 •‰’S—¦‚v                           PIC 9(3) VALUE ZERO.
001220 01 •‰’S—¦’†ŠÔŒ‹‰Ê‚v                   PIC 9V9(3) VALUE ZERO.
001230 01 ¤‚v                               PIC 9(4) VALUE ZERO.
001240 01 è—]‚v                             PIC 9(4) VALUE ZERO.
001170 01 •‰’SŠz‚S‚v                         PIC 9(6) VALUE ZERO.
001250*/•¬
001260 01 •‰’Sˆê’è‹àŠz‚v                     PIC 9(6) VALUE ZERO.
001270 01 •‰’S‹æ•ª‚v                         PIC 9(2) VALUE ZERO.
001280 01 •¬•‰’S‰ñ”‚v                     PIC 9(2) VALUE ZERO.
001290*/ˆ¤’mŒ§‚Ì“û—cŽ™‚Ì‘Î‰ž0109
001300 01 •¬•‰’S—¦‚v                       PIC 9(3) VALUE ZERO.
001310 01 •¬•‰’SŠz‚v                       PIC 9(5) VALUE ZERO.
001320 01 •‰’SŠz•¬‚v                       PIC 9(6) VALUE ZERO.
001330 01 •‰’SŠz•¬‚v‚Q                     PIC 9(6) VALUE ZERO.
001340*
001350*/0506•ºŒÉŒ§‚Ì‰ü’è170701‚æ‚è
001360 01 Ž––±Žè”—¿‚v                       PIC 9(4) VALUE ZERO.
001370*/•‰’S‹à–Æœ‚Ìˆ—‚ª”²‚¯‚Ä‚¢‚½/050629
001380 01 •¬•‰’S‹à–Æœ‚v                   PIC 9    VALUE ZERO.
001390*
001400 01 Žs’¬‘º”Ô†˜Vl‚v.
001410   03 –@•Ê”Ô†˜Vl‚v                   PIC X(2) VALUE SPACE.
001420   03 ••ÛŒ¯˜Vl.
001430     05 •{Œ§˜Vl                       PIC X(2) VALUE SPACE.
001440     05 •”Ô˜Vl                       PIC X(3) VALUE SPACE.
001450     05 •‚b‚c˜Vl                     PIC X    VALUE SPACE.
001460   03 FILLER                           PIC X(2) VALUE SPACE.
001470*
001480 01 Žs’¬‘º”Ô†•¬‚v.
001490   03 –@•Ê”Ô†•¬‚v                   PIC X(2) VALUE SPACE.
001500   03 ••ÛŒ¯•¬‚v.
001510     05 •{Œ§•¬‚v                     PIC X(2) VALUE SPACE.
001520     05 •”Ô•¬‚v                     PIC X(3) VALUE SPACE.
001530     05 •‚b‚c•¬‚v                   PIC X    VALUE SPACE.
001540   03 FILLER                           PIC X(2) VALUE SPACE.
001550*
001560*/‚S‚P˜Vl—p‚Ìƒ[ƒN0101
001570 01 •‰’SŠz‚S‚P‚v                       PIC 9(5)V9(3) VALUE ZERO.
001580 01 •‰’SŠz‚R‚v                         PIC 9(5) VALUE ZERO.
001590*
001591*
001592 01 ŒÀ“xŠz‚P‚v                         PIC 9(5) VALUE ZERO.
001593 01 ŒÀ“xŠz‚Q‚v                         PIC 9(5) VALUE ZERO.
001594 01 ŒÀ“xŠz‚R‚v                         PIC 9(5) VALUE ZERO.
001595 01 ŒvŽZŒÀ“xŠz‚v                       PIC 9(5) VALUE ZERO.
001596 01 ŒvŽZ—p—ÝŒvŠz‚v                     PIC 9(5) VALUE ZERO.
001597 01 •‰’SŠz“úŒv‘‹Œû‚v                   PIC 9(6) VALUE ZERO.
001598**
001600*
      */“–ŒŽŽ}”Ô‚Ì—ÝŒv‹àŠz‚ðŽæ“¾«««/20240424
002090 01 I—¹ƒtƒ‰ƒO‚R                       PIC X(3) VALUE SPACE.
002700 01 Ž}”Ôƒtƒ‰ƒO                         PIC X(3) VALUE SPACE.
003200 01 Š³ŽÒƒR[ƒh‚v.
003210     03 Š³ŽÒ”Ô†‚v                     PIC 9(6).
003220     03 Ž}”Ô‚v                         PIC X.
003370 01 Ž{p˜a—ï”NŒŽ‚v.
003380     03 Ž{p˜a—ï‚v                     PIC 9    VALUE ZERO.
003390     03 Ž{p”NŒŽ‚v.
003400        05 Ž{p”N‚v                    PIC 9(2) VALUE ZERO.
003410        05 Ž{pŒŽ‚v                    PIC 9(2) VALUE ZERO.
003440 01 •¬Ží•Ê‚v‚R                       PIC 9(2) VALUE ZERO.
003450 01 ”ï—p•‰’SŽÒ”Ô†•¬‚v‚R             PIC X(8) VALUE SPACE.
002380 01 ŽÀ“ú”‚v                           PIC 9(2) VALUE ZERO.
003240 01 Š³ŽÒƒR[ƒh‚b‚v.
003250     03 Š³ŽÒ”Ô†‚b‚v                   PIC 9(6).
003260     03 Ž}”Ô‚b‚v                       PIC X.
003270*
003280 01 Ž{p˜a—ï”NŒŽ“ú‚b‚v.
003290   03 Ž{p˜a—ï”NŒŽ‚b‚v.
003300     05 Ž{p˜a—ï‚b‚v                   PIC 9.
003310     05 Ž{p”NŒŽ‚b‚v.
003320        07 Ž{p”N‚b‚v                  PIC 9(2).
003330        07 Ž{pŒŽ‚b‚v                  PIC 9(2).
003340   03 Ž{p“ú‚b‚v                       PIC 9(2).
002710 01 Œö”ïŽí•Ê‚v                         PIC 9(2) VALUE ZERO.
002720 01 •¬Ží•Ê‚v                         PIC 9(2) VALUE ZERO.
002730 01 Œö”ïŽí•Ê‚v‚Q                       PIC 9(2) VALUE ZERO.
002740 01 •¬Ží•Ê‚v‚Q                       PIC 9(2) VALUE ZERO.
002750 01 ‰f—Ã”NŒŽ“ú‚b‚v                   PIC X(7) VALUE ZERO.
       01 ‘O—ÝŒv‹àŠz‚v                       PIC 9(6) VALUE ZERO.
      */“–ŒŽŽ}”Ô‚Ì—ÝŒv‹àŠz‚ðŽæ“¾ªªª/20240424
001610*****************************************************************
001620*                          ˜AŒ‹€–Ú                             *
001630*****************************************************************
001640*
001650* ŒvŽZ˜AŒgƒL[
001660 01 ‚g˜AŒvŽZ|ƒL[ IS EXTERNAL.
001670* / inƒpƒ‰ƒƒ^ /
001680   03 ‚g˜AŒvŽZ|Ž{p‹æ•ª                   PIC 9.
001690   03 ‚g˜AŒvŽZ|Ž{p˜a—ï”NŒŽ“ú.
001700      05 ‚g˜AŒvŽZ|Ž{p˜a—ï”NŒŽ.
001710        07 ‚g˜AŒvŽZ|Ž{p˜a—ï              PIC 9.
001720        07 ‚g˜AŒvŽZ|Ž{p”NŒŽ.
001730          09 ‚g˜AŒvŽZ|Ž{p”N              PIC 9(2).
001740          09 ‚g˜AŒvŽZ|Ž{pŒŽ              PIC 9(2).
001750      05 ‚g˜AŒvŽZ|Ž{p“ú                  PIC 9(2).
001760   03 ‚g˜AŒvŽZ|Š³ŽÒƒR[ƒh.
001770      05 ‚g˜AŒvŽZ|Š³ŽÒ”Ô†                PIC 9(6).
001780      05 ‚g˜AŒvŽZ|Ž}”Ô                    PIC X.
001790   03 ‚g˜AŒvŽZ|•ÛŒ¯Ží•Ê                   PIC 9(2).
001800* / “à•”ƒpƒ‰ƒƒ^/
001810   03 ‚g˜AŒvŽZ|“à•”ƒpƒ‰ƒƒ^.
001820      05 ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª          PIC 9.
001830      05 ‚g˜AŒvŽZ|”ï—pŠz                  PIC 9(6).
001840*  ‰ŒŸ“ú‚É"YES"(•‰’S‹æ•ª1,2,3,4)
001850      05 ‚g˜AŒvŽZ|‰ŒŸ“úƒtƒ‰ƒO            PIC X(3).
001860*  “–ŒŽ‚Ì‰‰ñ‚È‚ç"YES"(•‰’S‹æ•ª5)
001870      05 ‚g˜AŒvŽZ|‰f—Ãƒtƒ‰ƒO            PIC X(3).
001880*  ‰ŒŸ—¿‚Ì‹àŠz‚ð“]‹L(•‰’S‹æ•ª1)
001890      05 ‚g˜AŒvŽZ|‰ŒŸ—¿                  PIC 9(4).
001900*  “–ŒŽ‚Ì“–“ú‚Ü‚Å‚ÌŽÀ“ú”(Ž}”Ô‚àŠÜ‚Þ)(•‰’S‹æ•ª6,8,13)
001910      05 ‚g˜AŒvŽZ|ŽÀ“ú”                  PIC 9(2).
001920*  “ú®Ã‰ª“û—cŽ™—pB“–“ú‚Ì•‰’S‚Ì‚ ‚è‚È‚µ  ‚OF‚È‚µ@‚PF‚ ‚è (•‰’S‹æ•ª7)
001930      05 ‚g˜AŒvŽZ|‚»‚Ì‘¼ŒvŽZ‹æ•ª‚P        PIC 9.
001940*  out
001950      05 ‚g˜AŒvŽZ|•‰’SŠz                  PIC 9(6).
001960      05 ‚g˜AŒvŽZ|¿‹Šz                  PIC 9(6).
001970      05 ‚g˜AŒvŽZ|•‰’SŠz•¬              PIC 9(5).
001980      05 ‚g˜AŒvŽZ|¿‹Šz•¬              PIC 9(5).
001990      05 ‚g˜AŒvŽZ|•‰’S—¦                  PIC 9(3).
002000*
      */¿‹Šz•¬‚Ì‚UŒ…ver/20231117
001980  01 ‚g˜AŒvŽZ|¿‹Šz•¬‚Q                PIC 9(6) IS EXTERNAL.
      *
      */“úŒv•‰’SŠz‚Í­”ˆÈ‰ºˆêˆÊ‚ðŽlŽÌŒÜ“ü«««/20220601
006460 01 ‚g˜AŒvŽZ‚Q|ƒL[ IS EXTERNAL.
006620      05 ‚g˜AŒvŽZ‚Q|“úŒv‹æ•ª              PIC 9.
002010*
      */•‰’SŠzØ‚èã‚°—p/20221125
001660 01 ‚g˜AŒvŽZ‚R|ƒL[ IS EXTERNAL.
001950    03 ‚g˜AŒvŽZ‚R|•‰’SŠz                  PIC 9(6).
001970    03 ‚g˜AŒvŽZ‚R|•‰’SŠz•¬              PIC 9(5).
002020*---------------------------------------------------------*
002030* ‘åãŠÛØƒtƒ‰ƒO—p
002040 01 ‚g˜AŒvŽZ‘åã|ƒL[ IS EXTERNAL.
002050   03 ‚g˜AŒvŽZ‘åã|‘åãŠÛØƒtƒ‰ƒO         PIC X(3).
002060*
002070*---------------------------------------------------------*
002080*
002090* •¬‹àŠzŒvŽZ˜AŒgƒL[
002100 01 ‚g˜A•¬ŒvŽZ|ƒL[ IS EXTERNAL.
002110   03 ‚g˜A•¬ŒvŽZ|•‰’S‹æ•ª               PIC 9(2).
002120*
002121*---------------------------------------------------------*
002122* •¬ŒŽ“r’†—p
002123 01 ‚g˜AŒvŽZ•¬ŒŽ“r’†|ƒL[ IS EXTERNAL.
002124   03 ‚g˜AŒvŽZ•¬ŒŽ“r’†|•¬ŒŽ“r’†ŠJŽn“ú   PIC 9(2).
002125*---------------------------------------------------------*
002126*
002128*---------------------------------------------------------*
002129* •¬—ÝŒv—p
002130 01 ‚g˜AŒvŽZ•¬—ÝŒvŠz|•‰’SŠz•¬       PIC 9(6) IS EXTERNAL.
002131 01 ‚g˜AŒvŽZ•¬—ÝŒvŠz|•¬•‰’S‹à–Æœ   PIC 9 IS EXTERNAL.
002132*---------------------------------------------------------*
002134*
002135*-------------------------------------------------------*
002140* •‰’S—¦Žæ“¾—p14/10`
002150 01 ˜A—¦|•‰’S—¦Žæ“¾ƒL[ IS EXTERNAL.
002160    03 ˜A—¦|Ž{p˜a—ï”NŒŽ.
002170       05 ˜A—¦|Ž{p˜a—ï               PIC 9.
002180       05 ˜A—¦|Ž{p”NŒŽ.
002190          07 ˜A—¦|Ž{p”N              PIC 9(2).
002200          07 ˜A—¦|Ž{pŒŽ              PIC 9(2).
002210    03 ˜A—¦|Š³ŽÒƒR[ƒh.
002220       05 ˜A—¦|Š³ŽÒ”Ô†               PIC 9(6).
002230       05 ˜A—¦|Ž}”Ô                   PIC X.
002240    03 ˜A—¦|ŽÀÛ•‰’S—¦                PIC 9(3).
002250    03 ˜A—¦|ŽÀÛ–{‘Ì•‰’S—¦            PIC 9(3).
002260    03 ˜A—¦|Œ’•Û•‰’S—¦                PIC 9(3).
002270    03 ˜A—¦|‚Q‚V˜V•‰’S—¦              PIC 9(3).
002280    03 ˜A—¦|•¬•‰’S—¦                PIC 9(3).
002290    03 ˜A—¦|“Á•Ê—p•‰’S—¦              PIC 9(3).
002300*
002310*
002320* Šm”FƒƒbƒZ[ƒW—p (‚Qs)
002330 01 ˜Aƒ‚V|ƒL[ IS EXTERNAL.
002340    03  ˜Aƒ‚V|ƒƒbƒZ[ƒW‚P             PIC X(40).
002350    03  ˜Aƒ‚V|ƒƒbƒZ[ƒW‚Q             PIC X(40).
002360*
      */•‰’S‹æ•ª‚Q‚Q(‘åã•{•½¬30”N4ŒŽ`)/20180306
004887* 01 ˜AŒvŽZ•¬—ÝŒvŠz|•‰’SŠz•¬‚P‚O   PIC 9(6) IS EXTERNAL.
004887 01 ˜A|•‰’SŠz•¬‚P‚f‚v               PIC 9(6) IS EXTERNAL.
004887 01 ˜A|•‰’SŠz•¬‚P‚O‚f‚v             PIC 9(6) IS EXTERNAL.
004887 01 ˜A|•‰’SŠz•¬‚P‚f‚v‚Q             PIC 9(6) IS EXTERNAL.
004887 01 ˜A|“úŒvƒŒƒZ‚e                     PIC 9    IS EXTERNAL.
      */‘åã•¬F“ú‚ÌãŒÀ‚ð1“x‚à‰z‚¦‚È‚¢ê‡A–{‘Ì‚Ì•‰’SŠz‚Æ·‚ª‡‚Á‚Ä‚à¿‹‚µ‚È‚¢/20220830
       01 ‘SŠzŠ³ŽÒ•‰’S‚e                     PIC X(3) IS EXTERNAL.
002370******************************************************************
002380*                      PROCEDURE  DIVISION                       *
002390******************************************************************
002400 PROCEDURE               DIVISION.
002410************
002420*           *
002430* ‰Šúˆ—   *
002440*           *
002450************
002460     PERFORM ‰Šú‰».
002470     PERFORM •‰’S—¦Žæ“¾.
002480     PERFORM ŽófŽÒî•ñŽæ“¾.
002490     IF Žó|•¬Ží•Ê NOT = ZERO
002500         PERFORM •¬ŒvŽZî•ñŽæ“¾
002510     END-IF.
002520*
002530************
002540*           *
002550* Žåˆ—     *
002560*           *
002570************
002580*    /out‰Šú‰»i•‰’S—¦‚Í‰Šú‰»‚µ‚È‚¢j
002590     MOVE ZERO TO ‚g˜AŒvŽZ|•‰’SŠz.
002600     MOVE ZERO TO ‚g˜AŒvŽZ|¿‹Šz.
002610     MOVE ZERO TO ‚g˜AŒvŽZ|•‰’SŠz•¬.
002620     MOVE ZERO TO ‚g˜AŒvŽZ|¿‹Šz•¬.
      */•‰’SŠzØ‚èã‚°—p/20221125
001660     MOVE ZERO TO ‚g˜AŒvŽZ‚R|•‰’SŠz ‚g˜AŒvŽZ‚R|•‰’SŠz•¬.
      */¿‹Šz•¬‚Ì‚UŒ…ver/20240607
001980     MOVE ZERO TO ‚g˜AŒvŽZ|¿‹Šz•¬‚Q.
001980     INITIALIZE   ‚g˜AŒvŽZ|¿‹Šz•¬‚Q.
      *
002630*
002631*    /•ÛŒ¯Ø–Y‚ê/
002632     IF Žó|•ÛŒ¯Ží•Ê = 91
002633        PERFORM •‰’SŠzŒvŽZ
002635     ELSE
002640        EVALUATE TRUE
002650        WHEN (Žó|Œö”ïŽí•Ê = 05) 
002660*   /27˜VlA˜Vl+•¬
002670            PERFORM ˜Vl•‰’SŠzŒvŽZ
002680        WHEN (Žó|•¬Ží•Ê = 51)
002690        WHEN (Žó|•¬Ží•Ê NOT = ZERO) AND (•‰’S‹æ•ª‚v = 09 OR 10)
002700*   /41˜VlA41‚É€‚¸‚é•¬
002710            PERFORM •‰’SŠzŒvŽZ
002720            PERFORM ˜Vl•‰’SŠzŒvŽZ
002730        WHEN (Žó|•¬Ží•Ê NOT = ZERO)
002740*   /•¬(41˜VlA41‚É€‚¸‚é•¬‚Íã‚ÅŒvŽZÏ‚Ý)
002750            PERFORM •‰’SŠzŒvŽZ
002760            PERFORM •¬•‰’SŠzŒvŽZ
002770        WHEN OTHER
002780*   /Œ’•Û‚Ì‚Ý
002790            PERFORM •‰’SŠzŒvŽZ
002800        END-EVALUATE
002801     END-IF.
002810*
002812*
002813*----------------------------------------------------------*
002814*    / •‰’SŠz‚ª”ï—pŠz‚æ‚è‘å‚«‚¢ê‡‚Í”ï—pŠz‚É‚·‚é/ 
002815     IF ‚g˜AŒvŽZ|•‰’SŠz > ‚g˜AŒvŽZ|”ï—pŠz
002816        MOVE ‚g˜AŒvŽZ|”ï—pŠz TO ‚g˜AŒvŽZ|•‰’SŠz
002817     END-IF.
002818*----------------------------------------------------------*
002819*
002820*----------------------------------------------------------*
002821* “úŒv‚Å•¬ŒŽ“r’†‚ÌŽžA•¬‚É‚È‚éˆÈ‘O‚ÍA–{‘Ì•‰’SŠz‚ð•‰’SŠz•¬‚ÖƒZƒbƒg
002822     IF ( ‚g˜AŒvŽZ|Ž{p“ú NOT = ZERO ) AND ( ‚g˜AŒvŽZ•¬ŒŽ“r’†|•¬ŒŽ“r’†ŠJŽn“ú NOT = ZERO ) AND
002823        ( ‚g˜AŒvŽZ|Ž{p“ú < ‚g˜AŒvŽZ•¬ŒŽ“r’†|•¬ŒŽ“r’†ŠJŽn“ú )
002824         MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|•‰’SŠz•¬
002829         MOVE ZERO             TO ‚g˜AŒvŽZ|¿‹Šz•¬
002830     END-IF.
002831*----------------------------------------------------------*
002832*
002835************
002836*           *
002840* I—¹ˆ—   *
002850*           *
002860************
002870*
HILO  *     DISPLAY "HKT41410 " ‚g˜AŒvŽZ|Ž{p˜a—ï”NŒŽ“ú " ”ï—pŠz •‰’SŠz ¿‹Šz ••‰  •¿  ‚R•‰’S ‚R••‰".
HILO  *     DISPLAY "HKT41410 " ‚g˜AŒvŽZ|Ž{p˜a—ï”NŒŽ“ú " " ‚g˜AŒvŽZ|”ï—pŠz " " ‚g˜AŒvŽZ|•‰’SŠz " " ‚g˜AŒvŽZ|¿‹Šz " " ‚g˜AŒvŽZ|•‰’SŠz•¬ " " ‚g˜AŒvŽZ|¿‹Šz•¬ " " ‚g˜AŒvŽZ‚R|•‰’SŠz " " ‚g˜AŒvŽZ‚R|•‰’SŠz•¬.
002880     PERFORM I—¹ˆ—.
002890     MOVE 1 TO PROGRAM-STATUS.
002900     EXIT PROGRAM.
002910*
002920*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002930*================================================================*
002940 ‰Šú‰» SECTION.
002950*
002960* EXTERNAL-FILE OPEN‚È‚µ
002970*
002980*
002990*
003000*================================================================*
003010 ƒI[ƒvƒ“ƒ`ƒFƒbƒN SECTION.
003020*
003030     IF ó‘ÔƒL[  NOT =  "00"
003040         DISPLAY ƒtƒ@ƒCƒ‹–¼‚v NC"‚eƒI[ƒvƒ“ƒGƒ‰[" UPON CONS
003050         DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[           UPON CONS
003060         DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢"
003070                                                   UPON CONS
003071*-----------------------------------------*
003072         CALL "actcshm"  WITH C LINKAGE
003073*-----------------------------------------*
003080         ACCEPT  ƒL[“ü—Í FROM CONS
003090         PERFORM ƒtƒ@ƒCƒ‹•Â½
003100         MOVE ZERO TO PROGRAM-STATUS
003110         EXIT PROGRAM.
003120*================================================================*
003130 ƒtƒ@ƒCƒ‹•Â½ SECTION.
003140*
003150*
003160*================================================================*
003170 I—¹ˆ— SECTION.
003180*
003190     PERFORM ƒtƒ@ƒCƒ‹•Â½.
003200*================================================================*
003210 ƒGƒ‰[•\Ž¦ SECTION.
003220*
003230     DISPLAY ƒtƒ@ƒCƒ‹–¼‚v NC"ƒtƒ@ƒCƒ‹‘žƒGƒ‰["   UPON CONS.
003240     DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[               UPON CONS.
003250     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"  UPON CONS.
003260     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢"
003270                                                   UPON CONS.
003271*-----------------------------------------*
003272     CALL "actcshm"  WITH C LINKAGE.
003273*-----------------------------------------*
003280     ACCEPT  ƒL[“ü—Í FROM CONS.
003290     PERFORM ƒtƒ@ƒCƒ‹•Â½.
003300     MOVE ZERO TO PROGRAM-STATUS
003310     EXIT PROGRAM.
003320*================================================================*
003330 •‰’S—¦Žæ“¾ SECTION.
003340*/•‰’S—¦‚ÍŒÄ‚Ô•û‚Ì‚o‚f‚ÅƒZƒbƒgÏ‚ÝB
003350*
003360*     MOVE SPACE TO ˜A—¦|•‰’S—¦Žæ“¾ƒL[.
003370*     INITIALIZE    ˜A—¦|•‰’S—¦Žæ“¾ƒL[.
003380*     MOVE ‚g˜AŒvŽZ|Ž{p˜a—ï”NŒŽ TO ˜A—¦|Ž{p˜a—ï”NŒŽ.
003390*     MOVE ‚g˜AŒvŽZ|Š³ŽÒƒR[ƒh   TO ˜A—¦|Š³ŽÒƒR[ƒh.
003400**
003410*     CALL   "HUTANRIT".
003420*     CANCEL "HUTANRIT".
003430*
003440     MOVE ˜A—¦|ŽÀÛ–{‘Ì•‰’S—¦ TO •‰’S—¦‚v ‚g˜AŒvŽZ|•‰’S—¦.
003450*
003460*================================================================*
003470*================================================================*
003480 •‰’SŠzŒvŽZ SECTION.
003490*******************************************************************
003500* ™’ˆÓ™
003510* ‘‹Œû‰ïŒvEƒJƒ‹ƒe‚ÍA•‰’S‹à‚Ì‚P‰~‚ÌˆÊ‚ðŽlŽÌŒÜ“ü‚·‚éi‚P‚O‰~’PˆÊj
003520* ƒŒƒZƒvƒg‚ÍA•‰’S‹à‚Ì¬”“_‘æ‚PˆÊ‚ðØ‚èã‚°‚é
003530******************************************************************
003540*
003550     MOVE ZERO TO •‰’SŠz‚v.
003560     MOVE ZERO TO ¿‹Šz‚v.
003570****************************************************************
003580* •‰’S‹àŒvŽZ•û–@‚ÌŽw’è  •‰’S‹àŒvŽZ‹æ•ª  ”ï—pE¿‹Šz   •‰’SŠz  *
003590* @@@@@@@@@@@@@@‚O@@@@@‚P‰~’PˆÊ    ‚P‰~’PˆÊ *
003600*              @@@@ @@@‚P@@@@‚P‚O‰~’PˆÊ             *
003610*              @@@@@@@ ‚Q@@@@@‚P‰~’PˆÊ  ‚P‚O‰~’PˆÊ *
003620*----------------------------*
003630* ŽÀÛ¨“úŒv‚Í‚QAŒŽŒv‚Í‚O
003640*----------------------------*
003650****************************************************************
003660*****************************************
003670* •‰’SŠz‚P‚O‰~’PˆÊi‚P‰~‚ÌˆÊ‚ðŽlŽÌŒÜ“üj*
003680*****************************************
003690     EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
003700     WHEN 2
003710* •‰’SŠz‚ÍA¬”“_ˆÈ‰º‘æ‚PˆÊ‚ðØ‚èã‚°‚é
003720         COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
003730         COMPUTE •‰’SŠz‚v‚P‚O = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
003740         COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚P‚O * 10
003750         DIVIDE  •‰’SŠz‚v BY 10 GIVING    ¤‚v
003760                                  REMAINDER è—]‚v
003770         END-DIVIDE
003780         IF è—]‚v = ZERO
003790             COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
003800             COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
003810*
003820* •‰’SŠz‚v‚ª‚P‰~’PˆÊ‚Ìó‘Ô‚Å¿‹Šz‚ðŒvŽZ
003830             COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|”ï—pŠz - •‰’SŠz‚v
003840*
003850             COMPUTE •‰’SŠz‚v‚Q ROUNDED = •‰’SŠz‚v / 10
003860             COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
003870         ELSE
003880             COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
003890             COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
003900* ¬”“_ˆÈ‰º‘æ‚PˆÊ‚ª”­¶‚µ‚½ê‡A®”•”‚É‚P‚ð‰Á‚¦Ø‚èã‚°‚é
003910*/“úŒv‚Ìê‡•‰’SŠz‚ðØ‚èã‚°‚¸¿‹Šz‚©‚ç‚P‚ðˆø‚­ª‚ÍŽæ‚èÁ‚µ
003920*             COMPUTE •‰’SŠz‚v = •‰’SŠz‚v + 1
003930* •‰’SŠz‚v‚ª‚P‰~’PˆÊ‚Ìó‘Ô‚Å¿‹Šz‚ðŒvŽZ
003940             COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|”ï—pŠz - •‰’SŠz‚v - 1
003950*
003960             COMPUTE •‰’SŠz‚v‚Q ROUNDED = •‰’SŠz‚v / 10
003970             COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
003980         END-IF
003990*
004003******************
004010* •‰’SŠz‚P‰~’PˆÊ *
004020******************
004030     WHEN ZERO
004040         COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v = ( •‰’S—¦‚v / 100 )
004050         COMPUTE •‰’SŠz‚v‚P‚O = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
004060         COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚P‚O * 10
004070         DIVIDE  •‰’SŠz‚v BY 10 GIVING    ¤‚v
004080                                REMAINDER è—]‚v
004090         END-DIVIDE
004100         IF è—]‚v = ZERO
004110             COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
004120             COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
004130             COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|”ï—pŠz - •‰’SŠz‚v
      */•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž/20221124
004170             COMPUTE •‰’SŠz‚S‚v  = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v + 0.9
004140         ELSE
      */“úŒv•‰’SŠz‚Í­”ˆÈ‰ºˆêˆÊ‚ðŽlŽÌŒÜ“ü«««/20220601
006620             IF ‚g˜AŒvŽZ‚Q|“úŒv‹æ•ª = 1
004160                 COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
004170                 COMPUTE •‰’SŠz‚v ROUNDED = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
      */•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž/20221124
004170                 COMPUTE •‰’SŠz‚S‚v  = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v + 0.9
006620*                 MOVE ZERO TO ‚g˜AŒvŽZ‚Q|“úŒv‹æ•ª
                   ELSE
      */“úŒv•‰’SŠz‚Í­”ˆÈ‰ºˆêˆÊ‚ðŽlŽÌŒÜ“üªªª/20220601
004150* ˆê•”•‰’S‹à‚Ì’[”Ø‚èã‚°
004160                 COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
004170                 COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
004180* DEBUG
004190*     DISPLAY NC"•‰’SŠz‚v|‚P" •‰’SŠz‚v UPON MSGBOX
004200*
004210                 COMPUTE •‰’SŠz‚v = •‰’SŠz‚v + 1
004220* DEBUG
004230*     DISPLAY NC"•‰’SŠz‚v|‚Q" •‰’SŠz‚v UPON MSGBOX
004240*
      */•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž/20221202
004170                 MOVE •‰’SŠz‚v TO •‰’SŠz‚S‚v
                   END-IF
004250             COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|”ï—pŠz - •‰’SŠz‚v
004260         END-IF
004270     WHEN OTHER
004280*/ƒGƒ‰[•\Ž¦‚ÌC³
004290         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
004300         DISPLAY "•‰’S‹àŒvŽZ‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
004310                 " ŽófŽÒ‡‚="   ‚g˜AŒvŽZ|Š³ŽÒƒR[ƒh
004320                 " Ž{p”NŒŽ="   ‚g˜AŒvŽZ|Ž{p”N ‚g˜AŒvŽZ|Ž{pŒŽ UPON CONS
004330*         DISPLAY NC"•‰’S‹àŒvŽZ‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ" UPON MSGBOX
004340         PERFORM ƒtƒ@ƒCƒ‹•Â½
004350         MOVE ZERO TO PROGRAM-STATUS
004360         EXIT PROGRAM
004370     END-EVALUATE.
004380*
004390*****
004400     MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz.
004410     MOVE ¿‹Šz‚v TO ‚g˜AŒvŽZ|¿‹Šz.
      */•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž/20221202
004170     MOVE •‰’SŠz‚S‚v TO ‚g˜AŒvŽZ‚R|•‰’SŠz.
004420****
004430*
004440*================================================================*
004450 ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F SECTION.
004460*
004470     IF ƒGƒ‰[•\Ž¦‚e = ZERO
004480         MOVE  "ƒGƒ‰[‚ª”­¶‚µ‚½‚Ì‚ÅƒRƒ“ƒ\[ƒ‹‰æ–Ê‚Ì“à—e" TO ˜Aƒ‚V|ƒƒbƒZ[ƒW‚P
004490         MOVE  "‚ðŠm”F‚µAC³‚µ‚Ä‚­‚¾‚³‚¢B"             TO ˜Aƒ‚V|ƒƒbƒZ[ƒW‚Q
004500         CALL   "MSG007"
004510         CANCEL "MSG007"
004520     END-IF.
004530     MOVE 1 TO ƒGƒ‰[•\Ž¦‚e.
004540*================================================================*
004550 ŽófŽÒî•ñŽæ“¾ SECTION.
004560*
004570     MOVE ‚g˜AŒvŽZ|Ž{p˜a—ï TO Žó|Ž{p˜a—ï.
004580     MOVE ‚g˜AŒvŽZ|Ž{p”N   TO Žó|Ž{p”N.
004590     MOVE ‚g˜AŒvŽZ|Ž{pŒŽ   TO Žó|Ž{pŒŽ.
004600     MOVE ‚g˜AŒvŽZ|Š³ŽÒ”Ô† TO Žó|Š³ŽÒ”Ô†.
004610     MOVE ‚g˜AŒvŽZ|Ž}”Ô     TO Žó|Ž}”Ô.
004620     READ ŽófŽÒî•ñ‚e
004630     INVALID KEY
004640*/ƒGƒ‰[•\Ž¦‚ÌC³
004650         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
004660         DISPLAY "Ž{pŒŽ‚ÌŽófŽÒî•ñ‚ª‚ ‚è‚Ü‚¹‚ñ"
004670                 " ŽófŽÒ‡‚=" ‚g˜AŒvŽZ|Š³ŽÒƒR[ƒh
004680                 " Ž{p”NŒŽ=" ‚g˜AŒvŽZ|Ž{p”N ‚g˜AŒvŽZ|Ž{pŒŽ UPON CONS
004690         PERFORM ƒtƒ@ƒCƒ‹•Â½
004700         MOVE ZERO TO PROGRAM-STATUS
004710         EXIT PROGRAM
004720     NOT INVALID KEY
004730         MOVE Žó|•¬•‰’S‹à–Æœ TO •¬•‰’S‹à–Æœ‚v
004740     END-READ.
004750*================================================================*
004760  •¬•‰’SŠzŒvŽZ SECTION.
004770*
004780     EVALUATE TRUE
004790*/Ž‘ŠiØ–¾‚Ì‘Î‰ž/*((‘•Û•‘•Û‘g‡ˆÈŠO)‚Ü‚½‚Í‘ÞE)•Ž‘ŠiØ–¾‹æ•ª‚ª‚P‚ÌŽž‚Í‘SŠzŠ³ŽÒ•‰’S
004800     WHEN (((Žó|•ÛŒ¯Ží•Ê = 01)  AND (Žó|•ÛŒ¯ŽÒ”Ô†(3:1) NOT = "3")) OR
004810           ( Žó|•ÛŒ¯Ží•Ê = 08)) AND
004820           ( Žó|Ž‘ŠiØ–¾‹æ•ª = 1)
004830         MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|•‰’SŠz•¬
004840         MOVE ZERO             TO ‚g˜AŒvŽZ|¿‹Šz•¬
004850* •¬
004860     WHEN (( Žó|•ÛŒ¯Ží•Ê >= 01 ) AND ( Žó|•ÛŒ¯Ží•Ê <= 09 )) AND
004870          ( Žó|•ÛŒ¯ŽÒ”Ô†         NOT = SPACE ) AND
004880          ( Žó|Œö”ïŽí•Ê               = ZERO  ) AND
004890          ( Žó|”ï—p•‰’SŽÒ”Ô†         = SPACE ) AND
004900          ( Žó|•¬Ží•Ê >= 50 AND Žó|•¬Ží•Ê <= 60 ) AND
004910          ( Žó|”ï—p•‰’SŽÒ”Ô†•¬ NOT = SPACE )
004920         PERFORM •¬ŽZ’èˆ—
004930* ¶•Û’P“Æ
004940     WHEN ( Žó|•ÛŒ¯Ží•Ê               = ZERO  ) AND
004950          ( Žó|•ÛŒ¯ŽÒ”Ô†             = SPACE ) AND
004960          ( Žó|Œö”ïŽí•Ê               = ZERO  ) AND
004970          ( Žó|”ï—p•‰’SŽÒ”Ô†         = SPACE ) AND
004980          ( Žó|•¬Ží•Ê               = 50    ) AND
004990          ( Žó|”ï—p•‰’SŽÒ”Ô†•¬ NOT = SPACE )
005000         PERFORM •¬ŽZ’èˆ—
005010     WHEN OTHER
005020*/ƒGƒ‰[•\Ž¦‚ÌC³
005030         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
005040         DISPLAY "•¬ŽZ’èðŒ‚ª–ž‚½‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
005050                 " ŽófŽÒ‡‚=" Žó|Š³ŽÒƒR[ƒh
005060                 " Ž{p”NŒŽ=" Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
005070     END-EVALUATE.
005080*================================================================*
005090 •¬ŒvŽZî•ñŽæ“¾ SECTION.
005100*
005110     MOVE Žó|•¬Ží•Ê           TO Žs|Œö”ïŽí•Ê.
005120     MOVE Žó|”ï—p•‰’SŽÒ”Ô†•¬ TO Žs|Žs’¬‘º”Ô†.
005130     READ Žs’¬‘ºƒ}ƒXƒ^
005140     INVALID KEY
005150*/ƒGƒ‰[•\Ž¦‚ÌC³
005160         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
005170         DISPLAY "Žs’¬‘º”Ô†‚ª“o˜^‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
005180                 " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
005190                 " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ
005200                 " •¬Ží•Ê="   Žó|•¬Ží•Ê
005210                 " Žs’¬‘º”Ô†=" Žó|”ï—p•‰’SŽÒ”Ô†•¬ UPON CONS
005220         PERFORM ƒtƒ@ƒCƒ‹•Â½
005230         MOVE ZERO TO PROGRAM-STATUS
005240         EXIT PROGRAM
005250     NOT INVALID KEY
005714*******
005715         MOVE ZERO   TO •‰’S‹æ•ª‚v
005716         MOVE ZERO   TO •‰’Sˆê’è‹àŠz‚v
005717         MOVE ZERO   TO •¬•‰’S‰ñ”‚v
005718         MOVE ZERO   TO •¬•‰’S—¦‚v
005719         MOVE ZERO   TO Ž––±Žè”—¿‚v
005720**
005721* “Á•Ê•‰’S
005722         MOVE Žó|•¬Ží•Ê           TO •Û“Á|•ÛŒ¯Ží•Ê
005723         MOVE Žó|”ï—p•‰’SŽÒ”Ô†•¬ TO •Û“Á|•ÛŒ¯ŽÒ”Ô†
005724         MOVE ‚g˜AŒvŽZ|Ž{p˜a—ï     TO •Û“Á|ŠJŽn˜a—ï
005725         MOVE ‚g˜AŒvŽZ|Ž{p”N       TO •Û“Á|ŠJŽn”N
005726         MOVE ‚g˜AŒvŽZ|Ž{pŒŽ       TO •Û“Á|ŠJŽnŒŽ
005727         START •ÛŒ¯ŽÒ“Á•Ê•‰’Sƒ}ƒXƒ^ KEY IS <= •Û“Á|•ÛŒ¯Ží•Ê 
005728                                              •Û“Á|•ÛŒ¯ŽÒ”Ô†
005729                                              •Û“Á|ŠJŽn˜a—ï”NŒŽ
005730                                              REVERSED
005731         END-START
005732         IF ó‘ÔƒL[ = "00" 
005733             READ •ÛŒ¯ŽÒ“Á•Ê•‰’Sƒ}ƒXƒ^ NEXT
005734             NOT AT END
005735                 IF ( •Û“Á|•ÛŒ¯Ží•Ê   = Žó|•¬Ží•Ê ) AND
005736                    ( •Û“Á|•ÛŒ¯ŽÒ”Ô† = Žó|”ï—p•‰’SŽÒ”Ô†•¬ )
005737                    MOVE •Û“Á|•‰’S‹æ•ª       TO •‰’S‹æ•ª‚v
005738                    MOVE •Û“Á|•‰’Sˆê’è‹àŠz   TO •‰’Sˆê’è‹àŠz‚v
005739                    MOVE •Û“Á|•¬•‰’S‰ñ”   TO •¬•‰’S‰ñ”‚v
005740                    MOVE •Û“Á|•‰’S—¦‚P       TO •¬•‰’S—¦‚v
005741                    MOVE •Û“Á|Ž––±Žè”—¿     TO Ž––±Žè”—¿‚v
005749                    MOVE •Û“Á|ŒÀ“xŠz‚P       TO ŒÀ“xŠz‚P‚v
005750                    MOVE •Û“Á|ŒÀ“xŠz‚Q       TO ŒÀ“xŠz‚Q‚v
005751                    MOVE •Û“Á|ŒÀ“xŠz‚R       TO ŒÀ“xŠz‚R‚v
005754                 END-IF
005755             END-READ
005756         END-IF
005757*******
005758     END-READ.
005759*
005760*================================================================*
005761 •¬ŽZ’èˆ— SECTION.
005762* m•¬n
005763******************************************************************
005764* “ú–ˆ‚Ì”ï—pŠz‚Ìe•ÛŒ¯•‰’S—¦•ª‚©‚çŠeŽs’¬‘º–ˆ‚Ì•¬ŽZ’è‚ðs‚¤B
005765******************************************************************
005766*
005767*     PERFORM •¬ŒvŽZî•ñŽæ“¾.
005768*/•‰’S–Æœ‚Ì”»’èB_®‚Å‚Í•‰’SŒvŽZPG‚Ì‘O‚É‚µ‚Ä‚¢‚é‚ªA‚Í‚è‚Å‚Í‚±‚±‚Ås‚È‚¤B/0506
005769*     PERFORM •¬•‰’SŽZ’è.
005770     IF •¬•‰’S‹à–Æœ‚v NOT = 1
005771         PERFORM •¬•‰’SŽZ’è
005772     ELSE
005780         MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
005790         MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
005800     END-IF.
005810*
005820*================================================================*
005830 •¬•‰’SŽZ’è SECTION.
005840*
005850* •¬•ª
005860     EVALUATE •‰’S‹æ•ª‚v
005870*@‚OF’Êí¿‹i‘SŠz•‰’Sj
005880     WHEN ZERO
005890         MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
005900         MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
      */¿‹Šz•¬‚Ì‚UŒ…ver/20231117
005900         MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬‚Q
005910*@‚PF‰ŒŸ—¿(‰ŒŸ—¿‚ª”­¶‚·‚é“ú)‚É‘Î‚·‚é•‰’S—¦•ª
005920     WHEN 1
005930*
005940         IF ‚g˜AŒvŽZ|‰ŒŸ“úƒtƒ‰ƒO NOT = SPACE
005950*             MOVE Žó|Ž{p˜a—ï TO ˜A—¦|Ž{p˜a—ï
005960*             MOVE Žó|Ž{p”N   TO ˜A—¦|Ž{p”N
005970*             MOVE Žó|Ž{pŒŽ   TO ˜A—¦|Ž{pŒŽ
005980*             MOVE Žó|Š³ŽÒ”Ô† TO ˜A—¦|Š³ŽÒ”Ô†
005990*             MOVE Žó|Ž}”Ô     TO ˜A—¦|Ž}”Ô
006000*             CALL   "HUTANRIT"
006010*             CANCEL "HUTANRIT"
006020             MOVE ˜A—¦|ŽÀÛ–{‘Ì•‰’S—¦ TO •‰’S—¦‚v
006030             PERFORM •‰’SŠzŒvŽZ‰ŒŸ
006040         ELSE
006050             MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
006060             MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
006070         END-IF
006080*@‚QF‰ŒŸ—¿(‰ŒŸ—¿‚ª”­¶‚·‚é“ú)‚É‘Î‚·‚éˆê’è‹àŠz
006090     WHEN 2
006100         IF ‚g˜AŒvŽZ|‰ŒŸ“úƒtƒ‰ƒO NOT = SPACE
006110             MOVE •‰’Sˆê’è‹àŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
006120             COMPUTE ¿‹Šz‚v     = ‚g˜AŒvŽZ|•‰’SŠz - •‰’Sˆê’è‹àŠz‚v
006130             MOVE ¿‹Šz‚v       TO ‚g˜AŒvŽZ|¿‹Šz•¬
006140         ELSE
006150             MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
006160             MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
006170         END-IF
006180*@‚RF‰ŒŸ“ú(‰ŒŸ—¿‚Ì”­¶‚·‚é“ú)‚Ì”ï—pŠz‚Ì–{‘Ì•‰’S—¦•ª‚É‘Î‚·‚é•‰’S—¦
006190     WHEN 3
006200         IF ‚g˜AŒvŽZ|‰ŒŸ“úƒtƒ‰ƒO NOT = SPACE
006210*             MOVE Žó|Ž{p˜a—ï TO ˜A—¦|Ž{p˜a—ï
006220*             MOVE Žó|Ž{p”N   TO ˜A—¦|Ž{p”N
006230*             MOVE Žó|Ž{pŒŽ   TO ˜A—¦|Ž{pŒŽ
006240*             MOVE Žó|Š³ŽÒ”Ô† TO ˜A—¦|Š³ŽÒ”Ô†
006250*             MOVE Žó|Ž}”Ô     TO ˜A—¦|Ž}”Ô
006260*             CALL   "HUTANRIT"
006270*             CANCEL "HUTANRIT"
006280             MOVE ˜A—¦|ŽÀÛ–{‘Ì•‰’S—¦ TO •‰’S—¦‚v
006290* “r’†‚Ì•‰’S‹à‚ÍA‚P‰~’PˆÊ‚Ì‚Ü‚ÜA•¬‚ÌŒvŽZ‚ðs‚¤B
006300* ‰ŒŸ“ú‚Ì”ï—pŠz‚Ì–{‘Ì•‰’S—¦•ª‚ðŒvŽZ‚·‚éB
006310             PERFORM •‰’SŠzŒvŽZ•¬
006320         ELSE
006330             MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
006340             MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
006350         END-IF
006360*@‚SF‰ŒŸ“ú(‰ŒŸ—¿‚Ì”­¶‚·‚é“ú)‚Ì”ï—pŠz‚Ì–{‘Ì•‰’S—¦•ª‚É‘Î‚·‚éˆê’è‹àŠz
006370     WHEN 4
006380         IF ‚g˜AŒvŽZ|‰ŒŸ“úƒtƒ‰ƒO NOT = SPACE
006390* “r’†‚Ì•‰’S‹à‚ÍA‚P‰~’PˆÊ‚Ì‚Ü‚ÜA•¬‚ÌŒvŽZ‚ðs‚¤B
006400             IF •‰’Sˆê’è‹àŠz‚v >= ‚g˜AŒvŽZ|•‰’SŠz
006410                 MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|•‰’SŠz•¬
006420                 MOVE ZERO             TO ‚g˜AŒvŽZ|¿‹Šz•¬
006430             ELSE
006440                 MOVE •‰’Sˆê’è‹àŠz‚v   TO ‚g˜AŒvŽZ|•‰’SŠz•¬
006450                 COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|•‰’SŠz - •‰’Sˆê’è‹àŠz‚v
006460                 MOVE ¿‹Šz‚v         TO ‚g˜AŒvŽZ|¿‹Šz•¬
006470             END-IF
006480         ELSE
006490             MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
006500             MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
006510         END-IF
006520*@‚TF–ˆŒŽˆê’è‹àŠz‚ð•‰’Si‘‹Œû‰ïŒv‚Å‚ÍŒŽ‚Ì‰‰ñ‚ÉŠ·ŽZHj
006530* 
006540     WHEN 5
006542         PERFORM •¬ˆê•”•‰’SŽZ’è‚O‚T
006543*
006630*@‚UF–ˆŒŽˆê’è‹àŠzAˆê’è‰ñ”‚ð•‰’S
006640* 
006650     WHEN 6
006660     WHEN 8
006670         PERFORM •¬ˆê•”•‰’SŽZ’è
006680*@‚VFŽw’è‚µ‚½“ú‚Ì”ï—pŠz‚Ì–{‘Ì•‰’S—¦•ª‚É‘Î‚·‚éˆê’è‹àŠz
006690* 
006700     WHEN 7
006710*         PERFORM Ž{p‹L˜^‚eŽæ“¾
006720         IF ‚g˜AŒvŽZ|‚»‚Ì‘¼ŒvŽZ‹æ•ª‚P = 1
006730             IF •‰’Sˆê’è‹àŠz‚v >= ‚g˜AŒvŽZ|•‰’SŠz
006740                 MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|•‰’SŠz•¬
006750                 MOVE ZERO             TO ‚g˜AŒvŽZ|¿‹Šz•¬
006760             ELSE
006770                 MOVE •‰’Sˆê’è‹àŠz‚v  TO ‚g˜AŒvŽZ|•‰’SŠz•¬
006780                 COMPUTE ¿‹Šz‚v      = ‚g˜AŒvŽZ|•‰’SŠz - •‰’Sˆê’è‹àŠz‚v
006790                 MOVE ¿‹Šz‚v        TO ‚g˜AŒvŽZ|¿‹Šz•¬
006800             END-IF
006810         ELSE
006820             MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
006830             MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
006840         END-IF
006850*/ˆ¤’mŒ§‚Ì“û—cŽ™‚Ì‘Î‰ž0109
006860* ‚P‚PF–ˆ“ú‚Ì”ï—pŠz‚É‘Î‚·‚é•‰’S—¦
006870     WHEN 11
006880         PERFORM •¬•‰’SŠzŒvŽZ‚P‚P
006890*
006900         MOVE •¬•‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
006910         COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬ = ‚g˜AŒvŽZ|•‰’SŠz - ‚g˜AŒvŽZ|•‰’SŠz•¬
006920* ‚P‚QF–{‘Ì•‰’SŠz‚ð‘SŠz•‰’S030717
006930     WHEN 12
006940         MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|•‰’SŠz•¬
006950         MOVE ZERO             TO ‚g˜AŒvŽZ|¿‹Šz•¬
006960* ‚P‚RF–ˆŒŽˆê’è‹àŠzAˆê’è‰ñ”‚ð•‰’Si‚P‚O‰~’PˆÊj
006970     WHEN 13
006980         PERFORM •¬ˆê•”•‰’SŽZ’è‚P‚O
006990*/0506•ºŒÉŒ§‚Ì‰ü’è170701‚æ‚è
007000* ‚P‚SF–ˆŒŽˆê’è‹àŠz‚ðˆê’è‰ñ”‚Ü‚Å•‰’SiŽè”—¿‚ð’áŠ“¾ŽÒ—p•‰’SŠz‚Æ‚·‚éj
007010     WHEN 14
007020         PERFORM •¬ˆê•”•‰’SŽZ’è‚P‚S
007021*
007022*/Žw’è•‰’S—¦‚Å–ˆŒŽˆê’è‹àŠz‚Ü‚Å•‰’SiãŒÀ‚ ‚èj
007023     WHEN 16
007024         PERFORM •¬ˆê•”•‰’SŽZ’è‚P‚U
007025*
007026*/_ŒËŽs‘Î‰ž/
007027* ‚P‚VFŽw’èŠ„‡‚©‚Âˆê’è‹àŠz‚Ü‚Åˆê’è‰ñ”•‰’S
007028     WHEN 17
007029         PERFORM •¬ˆê•”•‰’SŽZ’è‚P‚V
007030*
      */‘åã•{‘Î‰ž/20180306
      * 22F–ˆŒŽˆê’è‹àŠz‚Åˆê’è‰ñ”‚ðŒÀ“xŠz‚Ü‚Å•‰’S(‚P‰~’PˆÊ)
012130     WHEN 22
012140         PERFORM •¬ˆê•”•‰’SŽZ’è‚Q‚Q
007030*
      */¡Œãì‚é—\’è«««
      ** 23F–ˆŒŽˆê’è‹àŠz‚Åˆê’è‰ñ”‚ðŒÀ“xŠz‚Ü‚Å•‰’S(‚P‚O‰~’PˆÊ)
012130*     WHEN 23
012140*         PERFORM •¬ˆê•”•‰’SŽZ’è‚Q‚R
      */¡Œãì‚é—\’èªªª
007030*
007031     WHEN OTHER
007040         MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
007050         MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
007060     END-EVALUATE.
007070*
007080*================================================================*
007090 •‰’SŠzŒvŽZ‰ŒŸ SECTION.
007100******************************************************************
007110* ™’ˆÓ™
007120* ‘‹Œû‰ïŒvEƒJƒ‹ƒe‚ÍA•‰’S‹à‚Ì‚P‰~‚ÌˆÊ‚ðŽlŽÌŒÜ“ü‚·‚éi‚P‚O‰~’PˆÊj
007130* ƒŒƒZƒvƒg‚ÍA•‰’S‹à‚Ì¬”“_‘æ‚PˆÊ‚ðØ‚èã‚°‚é
007140******************************************************************
007150*
007160* •‰’SŠz‚P‚O‰~’PˆÊi‚P‰~‚ÌˆÊ‚ðŽlŽÌŒÜ“üj
007170     EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
007180     WHEN 2
007190         COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v = •‰’S—¦‚v / 100
007200         COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|‰ŒŸ—¿ * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
007210
007220         COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|•‰’SŠz - •‰’SŠz‚v
007230         COMPUTE •‰’SŠz‚v‚Q ROUNDED = •‰’SŠz‚v / 10
007240         COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
007250*
007260     WHEN ZERO
007270         COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v = •‰’S—¦‚v / 100
007280         COMPUTE •‰’SŠz‚v‚P‚O = ‚g˜AŒvŽZ|‰ŒŸ—¿ * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
007290         COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚P‚O * 10
007300         DIVIDE  •‰’SŠz‚v BY 10 GIVING    ¤‚v
007310                                  REMAINDER è—]‚v
007320         END-DIVIDE
007330         IF è—]‚v = ZERO
007340             COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v = •‰’S—¦‚v / 100
007350             COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|‰ŒŸ—¿ * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
007360             COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|•‰’SŠz - •‰’SŠz‚v
007370         ELSE
007380* ˆê•”•‰’S‹à‚Ì’[”Ø‚èã‚°
007390             COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v = •‰’S—¦‚v / 100
007400             COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|‰ŒŸ—¿ * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
007410             COMPUTE •‰’SŠz‚v = •‰’SŠz‚v + 1
007420             COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|•‰’SŠz - •‰’SŠz‚v
007430         END-IF
007440     WHEN OTHER
007450*/ƒGƒ‰[•\Ž¦‚ÌC³
007460         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
007470         DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
007480                 " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
007490                 " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
007500*         DISPLAY NC"•‰’S‹àŒvŽZ‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ" UPON MSGBOX
007510         PERFORM ƒtƒ@ƒCƒ‹•Â½
007520         MOVE ZERO TO PROGRAM-STATUS
007530         EXIT PROGRAM
007540     END-EVALUATE.
007550*
007560     MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬.
007570     MOVE ¿‹Šz‚v TO ‚g˜AŒvŽZ|¿‹Šz•¬.
007580*
007590*================================================================*
007600 •‰’SŠzŒvŽZ•¬ SECTION.
007610******************************************************************
007620* ™’ˆÓ™
007630* ‘‹Œû‰ïŒvEƒJƒ‹ƒe‚ÍA•‰’S‹à‚Ì‚P‰~‚ÌˆÊ‚ðŽlŽÌŒÜ“ü‚·‚éi‚P‚O‰~’PˆÊj
007640* ƒŒƒZƒvƒg‚ÍA•‰’S‹à‚Ì¬”“_‘æ‚PˆÊ‚ðØ‚èã‚°‚é
007650******************************************************************
007660*
007670* •‰’SŠz‚P‚O‰~’PˆÊi‚P‰~‚ÌˆÊ‚ðŽlŽÌŒÜ“üj
007680     EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
007690     WHEN 2
007700         COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v = •‰’S—¦‚v / 100
007710         COMPUTE •‰’SŠz‚v‚P‚O = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
007720         COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚P‚O * 10
007730         DIVIDE  •‰’SŠz‚v BY 10 GIVING    ¤‚v
007740                                  REMAINDER è—]‚v
007750         END-DIVIDE
007760         IF è—]‚v = ZERO
007770             COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
007780             COMPUTE •‰’SŠz•¬‚v‚Q ROUNDED = •‰’SŠz‚v / 10
007790             COMPUTE •‰’SŠz•¬‚v = •‰’SŠz•¬‚v‚Q * 10
007800             MOVE •‰’SŠz•¬‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
007810             MOVE ZERO         TO ‚g˜AŒvŽZ|¿‹Šz•¬
007820         ELSE
007830             COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
007840* ¬”“_ˆÈ‰º‘æ‚PˆÊ‚ª”­¶‚µ‚½ê‡A®”•”‚É‚P‚ð‰Á‚¦Ø‚èã‚°‚é
007850*/ª‚ÍŽæ‚èÁ‚µ
007860*             COMPUTE •‰’SŠz•¬‚v = •‰’SŠz‚v + 1
007870             COMPUTE •‰’SŠz•¬‚v‚Q ROUNDED = •‰’SŠz•¬‚v / 10
007880             COMPUTE •‰’SŠz•¬‚v = •‰’SŠz•¬‚v‚Q * 10
007890             MOVE •‰’SŠz•¬‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
007900             MOVE ZERO         TO ‚g˜AŒvŽZ|¿‹Šz•¬
007910         END-IF
007920     WHEN ZERO
007930         COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v = •‰’S—¦‚v / 100
007940         COMPUTE •‰’SŠz‚v‚P‚O = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
007950         COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚P‚O * 10
007960         DIVIDE  •‰’SŠz‚v BY 10 GIVING    ¤‚v
007970                                  REMAINDER è—]‚v
007980         END-DIVIDE
007990         IF è—]‚v = ZERO
008000             COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
008010             MOVE •‰’SŠz‚v     TO ‚g˜AŒvŽZ|•‰’SŠz•¬
008020             MOVE ZERO         TO ‚g˜AŒvŽZ|¿‹Šz•¬
008030         ELSE
008040* ˆê•”•‰’S‹à‚Ì’[”Ø‚èã‚°
008050             COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
008060             COMPUTE •‰’SŠz•¬‚v = •‰’SŠz‚v + 1
008070             MOVE •‰’SŠz•¬‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
008080             MOVE ZERO         TO ‚g˜AŒvŽZ|¿‹Šz•¬
008090         END-IF
008100     WHEN OTHER
008110*/ƒGƒ‰[•\Ž¦‚ÌC³
008120         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
008130         DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
008140                 " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
008150                 " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
008160         PERFORM ƒtƒ@ƒCƒ‹•Â½
008170         MOVE ZERO TO PROGRAM-STATUS
008180         EXIT PROGRAM
008190     END-EVALUATE.
008200*
008210*================================================================*
008220 •¬ˆê•”•‰’SŽZ’è SECTION.
008230* m•¬n
008240******************************************************************
008250* “ú–ˆ‚Ì”ï—pŠz‚Ìe•ÛŒ¯•‰’S—¦•ª‚©‚ç˜Vlˆê•”•‰’S‹à‚Ì‚Ý‚ðŠ³ŽÒ•‰’SB
008260*iŒŽ‚ÉÅ‘åAŒv‚`|˜Vl•‰’S‰ñ”•ªj
008270* “¯ˆêŒŽ“à‚Å‰ŒŸ“ú‚ª‚ ‚Á‚Ä‚àA•ÛŒ¯‚ªØ‚è‘Ö‚í‚Á‚Ä‚à˜Vl•‰’S‰ñ”•ª‚Í
008280* ˆø‘±‚«ƒJƒEƒ“ƒg‚³‚ê‚éB
008290******************************************************************
008300*
008310* e•ÛŒ¯‚ªŒŽ‚Ì“r’†‚Å•Ï‚í‚èAŽ}”Ôˆ—‚ðs‚Á‚½ê‡‚ðl—¶‚µA‘Oe•ÛŒ¯‚Å
008320* f—Ã‚µ‚½“ú”‚ðŽó‚¯Œp‚®
008330*
008340***     PERFORM f—Ã‰ñ”Žæ“¾.
008350*
008360*  / ƒŒƒZ(ŒŽŒv)‚ÍA“ú‚²‚Æ‚Ì•¬ŒvŽZ•‰’SŠz‚ðWŒv‚·‚éPG‚ðCALL. ŒÄ‚Ño‚³‚ê‚½PG‘¤‚ÅA
008370*    •‰’SŠz•¬,¿‹Šz•¬‚ðƒZƒbƒg /
008380     IF ‚g˜AŒvŽZ|Ž{p“ú = ZERO
008390        PERFORM “ÁŽê•‰’SŒvŽZŒÄo‚µ
008400     ELSE
008410        IF (‚g˜AŒvŽZ|ŽÀ“ú” > •¬•‰’S‰ñ”‚v) OR
008420           (‚g˜AŒvŽZ|ŽÀ“ú” = ZERO)
008430            MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
008440            MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
008450        ELSE
008460*
008470            IF ‚g˜AŒvŽZ|•‰’SŠz <= •‰’Sˆê’è‹àŠz‚v
008480                EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
008490                WHEN ZERO
008500                    MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|•‰’SŠz•¬
008510                    MOVE ZERO             TO ‚g˜AŒvŽZ|¿‹Šz•¬
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž«««/20221124
                          IF Žó|”ï—p•‰’SŽÒ”Ô†•¬(3:2) = "27"
008500                        MOVE ‚g˜AŒvŽZ‚R|•‰’SŠz TO ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                          END-IF
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰žªªª/20221124
008520*
008530                WHEN 2
008540                    COMPUTE •‰’SŠz‚v‚Q ROUNDED = ‚g˜AŒvŽZ|•‰’SŠz / 10
008550                    COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
008560                    MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
008570                    MOVE ZERO     TO ‚g˜AŒvŽZ|¿‹Šz•¬
008580                WHEN OTHER
008590*/ƒGƒ‰[•\Ž¦‚ÌC³
008600                    PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
008610                    DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
008620                            " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
008630                            " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
008640                    PERFORM ƒtƒ@ƒCƒ‹•Â½
008650                    MOVE ZERO TO PROGRAM-STATUS
008660                    EXIT PROGRAM
008670                END-EVALUATE
008680*
008690            ELSE
008700                EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
008710                WHEN ZERO
008720* Œ»Ý‚ÍA˜Vlˆê•”•‰’S‹à‚ªA‚T‚O‚O‰~‚Ì‚½‚ßA‚P‚O‰~’PˆÊ‚É‚·‚é•K—v‚È‚µ
008730                WHEN 2
008740                    MOVE •‰’Sˆê’è‹àŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
008750                    COMPUTE ¿‹Šz‚v     = ‚g˜AŒvŽZ|•‰’SŠz - •‰’Sˆê’è‹àŠz‚v
008760                    MOVE ¿‹Šz‚v       TO ‚g˜AŒvŽZ|¿‹Šz•¬
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž«««/20230228
                          IF Žó|”ï—p•‰’SŽÒ”Ô†•¬(3:2) = "27"
008740                        MOVE •‰’Sˆê’è‹àŠz‚v TO ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                          END-IF
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰žªªª/20230228
008770*
008780                WHEN OTHER
008790*/ƒGƒ‰[•\Ž¦‚ÌC³
008800                    PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
008810                    DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
008820                            " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
008830                            " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
008840                    PERFORM ƒtƒ@ƒCƒ‹•Â½
008850                    MOVE ZERO TO PROGRAM-STATUS
008860                    EXIT PROGRAM
008870                END-EVALUATE
008880            END-IF
008890        END-IF
008900     END-IF.
HILO  *     IF ‚g˜AŒvŽZ|Ž{p“ú = ZERO DISPLAY "•¬ˆê•”•‰’SŽZ’è " ‚g˜AŒvŽZ|•‰’SŠz•¬ " " ‚g˜AŒvŽZ|¿‹Šz•¬.
008910*
008920*================================================================*
008930 •¬•‰’SŠzŒvŽZ‚P‚P SECTION.
008940*
008950* ‚P‚PF–ˆ“ú‚Ì”ï—pŠz‚É‘Î‚·‚é•‰’S—¦
008960****************************************************************
008970* •‰’S‹àŒvŽZ•û–@‚ÌŽw’è  •‰’S‹àŒvŽZ‹æ•ª  ”ï—pE¿‹Šz   •‰’SŠz  *
008980* @@@@@@@@@@@@@@‚O@@@@@‚P‰~’PˆÊ    ‚P‰~’PˆÊ *
008990*              @@@@ @@@‚P@@@@‚P‚O‰~’PˆÊ             *
009000*              @@@@@@@ ‚Q@@@@@‚P‰~’PˆÊ  ‚P‚O‰~’PˆÊ *
009010****************************************************************
009020*
009030     EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
009040     WHEN ZERO
009050*/¬”‘æˆêˆÊ‚ðŽlŽÌŒÜ“ü‚µ‚Ä‚P‚ÌˆÊ‚Ü‚ÅŒvŽZ
009060         COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v     =  •¬•‰’S—¦‚v / 100
009070         COMPUTE •¬•‰’SŠz‚v ROUNDED = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
009080     WHEN 2
009090*/‚P‚ÌˆÊ‚ðŽlŽÌŒÜ“ü‚µ‚Ä‚P‚O‚ÌˆÊ‚Ü‚ÅŒvŽZ
009100         COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v = •¬•‰’S—¦‚v / 100
009110         COMPUTE •¬•‰’SŠz‚v     = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
009120*       /*‚P‚ÌˆÊ‚ðŽlŽÌŒÜ“ü‚µ‚Ä‚P‚O‰~’PˆÊ‚É‚·‚é
009130         COMPUTE •¬•‰’SŠz‚v ROUNDED = •¬•‰’SŠz‚v / 10
009140         COMPUTE •¬•‰’SŠz‚v         = •¬•‰’SŠz‚v * 10
009150     END-EVALUATE.
009160*
009170*================================================================*
009180 ˜Vl•‰’SŠzŒvŽZ SECTION.
009190*
009200     MOVE Žó|”ï—p•‰’SŽÒ”Ô†     TO Žs’¬‘º”Ô†˜Vl‚v.
009210     MOVE Žó|”ï—p•‰’SŽÒ”Ô†•¬ TO Žs’¬‘º”Ô†•¬‚v.
009220*
009230     EVALUATE TRUE
009240* ‚Q‚V˜Vl
009250     WHEN (( Žó|•ÛŒ¯Ží•Ê >= 01 ) AND ( Žó|•ÛŒ¯Ží•Ê <= 09 )) AND
009260          ( Žó|•ÛŒ¯ŽÒ”Ô†     NOT = SPACE ) AND
009270          ( Žó|Œö”ïŽí•Ê           = 05    ) AND
009280          ( Žó|”ï—p•‰’SŽÒ”Ô† NOT = SPACE ) AND
009290          ( –@•Ê”Ô†˜Vl‚v         = "27"  ) AND
009300          ( Žó|•¬Ží•Ê           = ZERO  ) AND
009310          ( Žó|”ï—p•‰’SŽÒ”Ô†•¬ = SPACE )
009320*/ŒãŠú‚—îŽÒ/0801
009330     WHEN (( Žó|•ÛŒ¯Ží•Ê >= 01 ) AND ( Žó|•ÛŒ¯Ží•Ê <= 09 )) AND
009340          ( Žó|•ÛŒ¯ŽÒ”Ô†     NOT = SPACE ) AND
009350          ( Žó|Œö”ïŽí•Ê           = 05    ) AND
009360          ( Žó|”ï—p•‰’SŽÒ”Ô† NOT = SPACE ) AND
009370          ( Žó|•¬Ží•Ê           = ZERO  ) AND
009380          ( Žó|”ï—p•‰’SŽÒ”Ô†•¬ = SPACE ) AND
009390          ( Žó|Ž{p˜a—ï”NŒŽ      >= 42004 )
009400         PERFORM ’è—¦Œö”ï˜Vl•‰’SŽZ’è
009410*/Ž‘ŠiØ–¾‚Ì‘Î‰ž/*‚S‚P˜Vl‚Æ‚S‚P‚É€‚¸‚é•¬‚Å)
009420*/‚Q‚V˜Vl‚Å‚È‚­((‘•Û•‘•Û‘g‡ˆÈŠO)‚Ü‚½‚Í‘ÞE)•Ž‘ŠiØ–¾‹æ•ª‚ª‚P‚ÌŽž‚Í‘SŠzŠ³ŽÒ•‰’S
009430     WHEN (((Žó|•ÛŒ¯Ží•Ê = 01)  AND (Žó|•ÛŒ¯ŽÒ”Ô†(3:1) NOT = "3")) OR
009440           ( Žó|•ÛŒ¯Ží•Ê = 08)) AND
009450           ( Žó|Ž‘ŠiØ–¾‹æ•ª = 1) AND
009460           ( Žó|Œö”ïŽí•Ê NOT = 05)
009470         MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|•‰’SŠz•¬
009480         MOVE ZERO             TO ‚g˜AŒvŽZ|¿‹Šz•¬
009490* ‚S‚P˜Vl
009500     WHEN (( Žó|•ÛŒ¯Ží•Ê >= 01 ) AND ( Žó|•ÛŒ¯Ží•Ê <= 09 )) AND
009510          ( Žó|•ÛŒ¯ŽÒ”Ô†         NOT = SPACE ) AND
009520          ( Žó|Œö”ïŽí•Ê               = ZERO  ) AND
009530          ( Žó|”ï—p•‰’SŽÒ”Ô†         = SPACE ) AND
009540          ( Žó|•¬Ží•Ê               = 51    ) AND
009550          ( Žó|”ï—p•‰’SŽÒ”Ô†•¬ NOT = SPACE )
009560* ‚S‚P˜V‚É€‚¸‚é•¬‚Ì‘Î‰ž
009570     WHEN ( Žó|•¬Ží•Ê NOT = ZERO ) AND (•‰’S‹æ•ª‚v = 09 OR 10) AND
009580* 27+41€‚ðl—¶‚µ‚Ä‚¢‚È‚©‚Á‚½B27•¬‚Í27‚ÌƒƒWƒbƒN‚ÖB(•¬•‰’S‚Íƒ[ƒ)
009590          ( Žó|Œö”ïŽí•Ê       = ZERO ) AND
009600          ( Žó|”ï—p•‰’SŽÒ”Ô† = SPACE)
009610         PERFORM ’è—¦•¬˜Vl•‰’SŽZ’è
009620* ‚Q‚V˜Vl{gáE”í”š
009630     WHEN (( Žó|•ÛŒ¯Ží•Ê >= 01 ) AND ( Žó|•ÛŒ¯Ží•Ê <= 09 )) AND
009640          ( Žó|•ÛŒ¯ŽÒ”Ô†     NOT = SPACE ) AND
009650          ( Žó|Œö”ïŽí•Ê           = 05    ) AND
009660          ( Žó|”ï—p•‰’SŽÒ”Ô† NOT = SPACE ) AND
009670          ( –@•Ê”Ô†˜Vl‚v         = "27"  ) AND
009680          ( Žó|•¬Ží•Ê >= 50 AND Žó|•¬Ží•Ê <= 60 ) AND
009690          ( Žó|”ï—p•‰’SŽÒ”Ô†•¬ NOT = SPACE )
009700*/ŒãŠú‚—îŽÒ/0801
009710     WHEN (( Žó|•ÛŒ¯Ží•Ê >= 01 ) AND ( Žó|•ÛŒ¯Ží•Ê <= 09 )) AND
009720          ( Žó|•ÛŒ¯ŽÒ”Ô†     NOT = SPACE ) AND
009730          ( Žó|Œö”ïŽí•Ê           = 05    ) AND
009740          ( Žó|”ï—p•‰’SŽÒ”Ô† NOT = SPACE ) AND
009750          ( Žó|•¬Ží•Ê >= 50 AND Žó|•¬Ží•Ê <= 60 ) AND
009760          ( Žó|”ï—p•‰’SŽÒ”Ô†•¬ NOT = SPACE ) AND
009770          ( Žó|Ž{p˜a—ï”NŒŽ      >= 42004 )
009780         PERFORM Œö”ï˜Vl•¬•‰’SŽZ’è
009790     WHEN OTHER
009800*/ƒGƒ‰[•\Ž¦‚ÌC³
009810         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
009820         DISPLAY "˜VlŽZ’èðŒ‚ª–ž‚½‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
009830                 " ŽófŽÒ‡‚=" Žó|Š³ŽÒƒR[ƒh
009840                 " Ž{p”NŒŽ=" Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
009850         PERFORM ƒtƒ@ƒCƒ‹•Â½
009860         MOVE ZERO TO PROGRAM-STATUS
009870         EXIT PROGRAM
009880     END-EVALUATE.
009890*
009900*================================================================*
009910 ’è—¦Œö”ï˜Vl•‰’SŽZ’è SECTION.
009920* m‚Q‚V˜Vln
009930******************************************************************
009940* “ú–ˆ‚Ì”ï—pŠz‚©‚ç˜Vl‚Ì•‰’S—¦•ª‚ðŠ³ŽÒ•‰’S
009950******************************************************************
009960*     MOVE ˜AŒv|”ï—pŠz TO ˜AŒv|”ï—pŠz˜Vl.
009970     MOVE ZERO         TO •‰’SŠz‚v.
009980     MOVE ZERO         TO ¿‹Šz‚v.
009990*
010000**     PERFORM ˜Vl•‰’S—¦Žæ“¾.
010010     MOVE ˜A—¦|‚Q‚V˜V•‰’S—¦ TO •‰’S—¦‚v.
010020******************************************************************
010030*™’ˆÓ™
010040*‘‹Œû‰ïŒvEƒJƒ‹ƒe‚ÍA•‰’S‹à‚Ì‚P‰~‚ÌˆÊ‚ðŽlŽÌŒÜ“ü‚·‚éi‚P‚O‰~’PˆÊj
010050*ƒŒƒZƒvƒg‚ÍA•‰’S‹à‚Ì¬”“_‘æ‚PˆÊ‚ðØ‚èã‚°‚é
010060******************************************************************
010070*
010080****************************************************************
010090* •‰’S‹àŒvŽZ•û–@‚ÌŽw’è  •‰’S‹àŒvŽZ‹æ•ª  ”ï—pE¿‹Šz   •‰’SŠz  *
010100* @@@@@@@@@@@@@@‚O@@@@@‚P‰~’PˆÊ    ‚P‰~’PˆÊ *
010110*              @@@@ @@@‚P@@@@‚P‚O‰~’PˆÊ             *
010120*              @@@@@@@ ‚Q@@@@@‚P‰~’PˆÊ  ‚P‚O‰~’PˆÊ *
010130****************************************************************
010140*****************************************
010150* •‰’SŠz‚P‚O‰~’PˆÊi‚P‰~‚ÌˆÊ‚ðŽlŽÌŒÜ“üj*
010160*****************************************
010170     EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
010180     WHEN 2
010190*     •‰’SŠz‚ÍA¬”“_ˆÈ‰º‘æ‚PˆÊ‚ðØ‚èã‚°‚é
010200         COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
010210         COMPUTE •‰’SŠz‚v‚P‚O = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
010220         COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚P‚O * 10
010230         DIVIDE  •‰’SŠz‚v BY 10 GIVING    ¤‚v
010240                                  REMAINDER è—]‚v
010250         END-DIVIDE
010260         IF è—]‚v = ZERO
010270             COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
010280             COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
010290*
010300             COMPUTE •‰’SŠz‚v‚Q ROUNDED = •‰’SŠz‚v / 10
010310             COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
010320*
010330*     •‰’SŠz‚v‚ª‚P‚O‰~’PˆÊ‚Ìó‘Ô‚Å¿‹Šz‚ðŒvŽZ
010340             COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|”ï—pŠz - •‰’SŠz‚v
010350         ELSE
010360             COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
010370             COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
010380*
010390             COMPUTE •‰’SŠz‚v‚Q ROUNDED = •‰’SŠz‚v / 10
010400             COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
010410*
010420*     •‰’SŠz‚v‚ª‚P‚O‰~’PˆÊ‚Ìó‘Ô‚Å¿‹Šz‚ðŒvŽZ
010430             COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|”ï—pŠz - •‰’SŠz‚v
010440         END-IF
010450*
010460*    *****************
010470*     •‰’SŠz‚P‰~’PˆÊ *
010480*    *****************
010490     WHEN ZERO
010500         COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v = ( •‰’S—¦‚v / 100 )
010510         COMPUTE •‰’SŠz‚v‚P‚O = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
010520         COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚P‚O * 10
010530         DIVIDE  •‰’SŠz‚v BY 10 GIVING    ¤‚v
010540                                REMAINDER è—]‚v
010550         END-DIVIDE
010560         IF è—]‚v = ZERO
010570             COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
010580             COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
010590             COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|”ï—pŠz - •‰’SŠz‚v
      */•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž/20230131
004170             COMPUTE •‰’SŠz‚S‚v  = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v + 0.9
010600         ELSE
      */“úŒv•‰’SŠz‚Í­”ˆÈ‰ºˆêˆÊ‚ðŽlŽÌŒÜ“ü«««/20220607
006620             IF ‚g˜AŒvŽZ‚Q|“úŒv‹æ•ª = 1
004160                 COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
004170                 COMPUTE •‰’SŠz‚v ROUNDED = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
006620*                 MOVE ZERO TO ‚g˜AŒvŽZ‚Q|“úŒv‹æ•ª
      */•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž/20221129
004170                 COMPUTE •‰’SŠz‚S‚v  = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v + 0.9
      */‘åã‚Ì•‰’SŠz‚ÌØ‚èã‚°‚ð‚â‚ß‚é«««/20240612  *>HILO TEST
                       MOVE •‰’SŠz‚v TO •‰’SŠz‚S‚v
      */‘åã‚Ì•‰’SŠz‚ÌØ‚èã‚°‚ð‚â‚ß‚éªªª/20240612  *>HILO TEST
                   ELSE
      */“úŒv•‰’SŠz‚Í­”ˆÈ‰ºˆêˆÊ‚ðŽlŽÌŒÜ“üªªª/20220607
010610*     ˆê•”•‰’S‹à‚Ì’[”Ø‚èã‚°
010620                 COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
010630                 COMPUTE •‰’SŠz‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
010640*
010650                 COMPUTE •‰’SŠz‚v = •‰’SŠz‚v + 1
      */•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž/20221129
004170                 MOVE •‰’SŠz‚v TO •‰’SŠz‚S‚v
010660*
                   END-IF
010670             COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|”ï—pŠz - •‰’SŠz‚v
010680         END-IF
010690     WHEN OTHER
010700*    /ƒGƒ‰[•\Ž¦‚ÌC³
010710         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
010720         DISPLAY "•‰’S‹àŒvŽZ‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
010730                 " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
010740                 " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
010750         PERFORM ƒtƒ@ƒCƒ‹•Â½
010760         MOVE ZERO TO PROGRAM-STATUS
010770         EXIT PROGRAM
010780     END-EVALUATE.
010790*
010800     MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz.
010810     MOVE ¿‹Šz‚v TO ‚g˜AŒvŽZ|¿‹Šz.
      */•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž/20221129
004170     MOVE •‰’SŠz‚S‚v TO ‚g˜AŒvŽZ‚R|•‰’SŠz.
010820*
010830*----------------------------------------------------------------------------------*
010840*/‘åã˜Vl•‰’S‹à–Æœ 500‰~~2‰ñ ‚Ì‘Î‰ž(“úŒvŽž‚Ì‚ÝŒvŽZ)«««/061120
010850     IF ( ‚g˜AŒvŽZ|Ž{p“ú NOT = ZERO ) AND ( ‚g˜AŒvŽZ‘åã|‘åãŠÛØƒtƒ‰ƒO = "YES" )
010860          MOVE 500 TO •‰’Sˆê’è‹àŠz‚v
010870          MOVE 2   TO •¬•‰’S‰ñ”‚v
010880          PERFORM ‘åã•‰’S‹à–Æœ‚Q‚V
010890     END-IF.
010900*/‘åã˜Vl•‰’S‹à–Æœ 500‰~~2‰ñ ‚Ì‘Î‰ž(“úŒvŽž‚Ì‚ÝŒvŽZ)ªªª/061120
010910*----------------------------------------------------------------------------------*
010920*
010930*
010940*================================================================*
010950 ’è—¦•¬˜Vl•‰’SŽZ’è SECTION.
010960*
010970* m‚S‚P˜Vln
010980*/ŒŽ‚Ì•‰’SŠz
010990*/	“úX‚Ì”ï—pŠz‚É‚S‚P˜Vl•‰’SŠ„‡‚PŠ„‚ð‚©‚¯‚Ä‚Pƒ–ŒŽ•ª‘«‚µž‚ÞB
011000*/	ƒ[ƒN‚Í¬”“_ˆÈ‰º‚RˆÊ‚Ü‚ÅŽ‚ÂŽ–‚Æ‚·‚éB
011010*/“ú‚Ì•‰’SŠz
011020*/	“ú‚Ì”ï—pŠz‚É‚S‚P˜Vl•‰’SŠ„‡‚PŠ„‚ð‚©‚¯‚Ä‚P‚Ì‚­‚ç‚¢‚ðŽlŽÌŒÜ“ü‚·‚éB
011030*
011040*     MOVE ˜AŒv|”ï—pŠz TO ˜AŒv|”ï—pŠz•¬.
011050     MOVE ZERO         TO •‰’SŠz‚S‚P‚v.
011060     MOVE ZERO         TO •‰’SŠz‚v.
011070     MOVE ZERO         TO ¿‹Šz‚v.
011080     MOVE ZERO         TO •‰’SŠz‚R‚v.
011090*
011100*     PERFORM •¬•‰’S—¦Žæ“¾.
011110     MOVE ˜A—¦|•¬•‰’S—¦ TO •‰’S—¦‚v.
011120******************************************************************
011130*™’ˆÓ™
011140*‘‹Œû‰ïŒvEƒJƒ‹ƒe‚ÍA•‰’S‹à‚Ì‚P‰~‚ÌˆÊ‚ðŽlŽÌŒÜ“ü‚·‚éi‚P‚O‰~’PˆÊj
011150*ƒŒƒZƒvƒg‚ÍA•‰’S‹à‚Ì¬”“_‘æ‚PˆÊ‚ðØ‚èã‚°‚é
011160******************************************************************
011170*
011180****************************************************************
011190* •‰’S‹àŒvŽZ•û–@‚ÌŽw’è  •‰’S‹àŒvŽZ‹æ•ª  ”ï—pE¿‹Šz   •‰’SŠz  *
011200* @@@@@@@@@@@@@@‚O@@@@@‚P‰~’PˆÊ    ‚P‰~’PˆÊ *
011210*              @@@@ @@@‚P@@@@‚P‚O‰~’PˆÊ             *
011220*              @@@@@@@ ‚Q@@@@@‚P‰~’PˆÊ  ‚P‚O‰~’PˆÊ *
011230****************************************************************
011240*****************************************
011250* •‰’SŠz‚P‚O‰~’PˆÊi‚P‰~‚ÌˆÊ‚ðŽlŽÌŒÜ“üj*
011260*****************************************
011270*/0101˜Vl‚Ì•‰’S‹àŠz‚Í“úX‚Ì‚P‚O‰~’PˆÊ‚Ì‹àŠz‚Ì‡Œv
011280     EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
011290     WHEN 2
011300*     •‰’SŠz‚ÍA¬”“_ˆÈ‰º‘æ‚PˆÊ‚ðØ‚èã‚°‚é
011310         COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
011320         COMPUTE •‰’SŠz‚v‚P‚O = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
011330         COMPUTE •‰’SŠz‚S‚P‚v = •‰’SŠz‚v‚P‚O * 10
011340         DIVIDE  •‰’SŠz‚S‚P‚v BY 10 GIVING    ¤‚v
011350                                  REMAINDER è—]‚v
011360         END-DIVIDE
011370         IF è—]‚v = ZERO
011380             COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
011390             COMPUTE •‰’SŠz‚S‚P‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
011400*
011410             COMPUTE •‰’SŠz‚v‚Q ROUNDED = •‰’SŠz‚S‚P‚v / 10
011420             COMPUTE •‰’SŠz‚S‚P‚v = •‰’SŠz‚v‚Q * 10
011430*
011440*     •‰’SŠz‚v‚ª‚P‚O‰~’PˆÊ‚Ìó‘Ô‚Å¿‹Šz‚ðŒvŽZ
011450             COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|•‰’SŠz - •‰’SŠz‚S‚P‚v
011460         ELSE
011470             COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •‰’S—¦‚v / 100
011480             COMPUTE •‰’SŠz‚S‚P‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
011490*
011500             COMPUTE •‰’SŠz‚v‚Q ROUNDED = •‰’SŠz‚S‚P‚v / 10
011510             COMPUTE •‰’SŠz‚S‚P‚v = •‰’SŠz‚v‚Q * 10
011520*
011530*     •‰’SŠz‚v‚ª‚P‚O‰~’PˆÊ‚Ìó‘Ô‚Å¿‹Šz‚ðŒvŽZ
011540             COMPUTE ¿‹Šz‚v = ‚g˜AŒvŽZ|•‰’SŠz - •‰’SŠz‚S‚P‚v
011550         END-IF
011560*
011570*    *****************‚S‚P˜Vl‚ÌŒŽŒv‚Ì•‰’SŠz‚Í“ú–ˆ‚Ì”ï—pŠz~•‰’S—¦‚ð
011580*     •‰’SŠz‚P‰~’PˆÊ *‚»‚Ì‚Ü‚Ü‘«‚µž‚ÞB¦“ú–ˆ‚É‚ÍØ‚èã‚°‚È‚¢Ž–
011590*    *****************ŒŽ‚Ì‡Œv‚Ì¬”“_ˆÈ‰º‚ÍØ‚èã‚°‚éB
011600     WHEN ZERO
011610         COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v = ( •‰’S—¦‚v / 100 )
011620         COMPUTE •‰’SŠz‚S‚P‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
011630*
011640*/ŒŽ‚Ì‡Œv‚É•‰’S—¦‚ð‚©‚¯A¬”“_ˆÈ‰º‚ðØ‚èã‚°B
011650*       */Ø‚èã‚°ˆ—
011660         IF •‰’SŠz‚S‚P‚v(6:3) NOT = ZERO
011670             COMPUTE •‰’SŠz‚S‚P‚v = •‰’SŠz‚S‚P‚v + 1
011680             MOVE ZERO TO •‰’SŠz‚S‚P‚v(6:3)
011690         END-IF
011700         COMPUTE ¿‹Šz‚v   = ‚g˜AŒvŽZ|•‰’SŠz - •‰’SŠz‚S‚P‚v
011710*
011720     WHEN OTHER
011730*    /ƒGƒ‰[•\Ž¦‚ÌC³
011740         PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
011750         DISPLAY "•‰’S‹àŒvŽZ‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
011760                 " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
011770                 " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
011780         PERFORM ƒtƒ@ƒCƒ‹•Â½
011790         MOVE ZERO TO PROGRAM-STATUS
011800         EXIT PROGRAM
011810     END-EVALUATE.
011820*
011830     MOVE •‰’SŠz‚S‚P‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬.
011840     MOVE ¿‹Šz‚v     TO ‚g˜AŒvŽZ|¿‹Šz•¬.
011850*
011860*----------------------------------------------------------------------------------*
011870*/‘åã˜Vl•‰’S‹à–Æœ 500‰~~2‰ñ ‚Ì‘Î‰ž(“úŒvŽž‚Ì‚ÝŒvŽZ)«««/061120
011880     IF ( ‚g˜AŒvŽZ|Ž{p“ú NOT = ZERO ) AND ( ‚g˜AŒvŽZ‘åã|‘åãŠÛØƒtƒ‰ƒO = "YES" )
011890          MOVE 500 TO •‰’Sˆê’è‹àŠz‚v
011900          MOVE 2   TO •¬•‰’S‰ñ”‚v
011910          PERFORM ‘åã•‰’S‹à–Æœ‚S‚P
011920     END-IF.
011930*/‘åã˜Vl•‰’S‹à–Æœ 500‰~~2‰ñ ‚Ì‘Î‰ž(“úŒvŽž‚Ì‚ÝŒvŽZ)ªªª/061120
011940*----------------------------------------------------------------------------------*
011950*
011960*
011970*================================================================*
011980 Œö”ï˜Vl•¬•‰’SŽZ’è SECTION.
011990*
012000* m‚Q‚V˜Vl{•¬n
012010*
012020     PERFORM ’è—¦Œö”ï˜Vl•‰’SŽZ’è.
012030*/‚Q‚V{•¬‚Ìê‡‚ÍŠ³ŽÒ•‰’S‚Í‚O‰~‚Æ‚·‚éB
012040*/ŽRŒ`Œ§‚Ìgá‚Í•‰’S‹à‚ª‚ ‚éB
012050     IF Žó|•¬•‰’S‹à–Æœ   NOT = 1
012060         PERFORM •¬•‰’SŽZ’è
012070     ELSE
012080         MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
012090         MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
012100     END-IF.
012110*
012120*================================================================*
012130 •¬ˆê•”•‰’SŽZ’è‚P‚O SECTION.
012140* m•¬n
012150******************************************************************
012160* “ú–ˆ‚Ì”ï—pŠz‚Ìe•ÛŒ¯•‰’S—¦•ª‚©‚ç•¬ˆê•”•‰’S‹à‚Ì‚Ý‚ðŠ³ŽÒ•‰’SB
012170*iŒŽ‚ÉÅ‘åAŒv‚`|˜Vl•‰’S‰ñ”•ªj
012180* “¯ˆêŒŽ“à‚Å‰ŒŸ“ú‚ª‚ ‚Á‚Ä‚àA•ÛŒ¯‚ªØ‚è‘Ö‚í‚Á‚Ä‚à•¬•‰’S‰ñ”•ª‚Í
012190* ˆø‘±‚«ƒJƒEƒ“ƒg‚³‚ê‚éB
012200******************************************************************
012210*
012220* e•ÛŒ¯‚ªŒŽ‚Ì“r’†‚Å•Ï‚í‚èAŽ}”Ôˆ—‚ðs‚Á‚½ê‡‚ðl—¶‚µA‘Oe•ÛŒ¯‚Å
012230* f—Ã‚µ‚½“ú”‚ðŽó‚¯Œp‚®
012240*
012250***     PERFORM f—Ã‰ñ”Žæ“¾.
012260*
012270*  / ƒŒƒZ(ŒŽŒv)‚ÍA“ú‚²‚Æ‚Ì•¬ŒvŽZ•‰’SŠz‚ðWŒv‚·‚éPG‚ðCALL. ŒÄ‚Ño‚³‚ê‚½PG‘¤‚ÅA
012280*    •‰’SŠz•¬,¿‹Šz•¬‚ðƒZƒbƒg /
012290     IF ‚g˜AŒvŽZ|Ž{p“ú = ZERO
012300        PERFORM “ÁŽê•‰’SŒvŽZŒÄo‚µ
012310     ELSE
012320        IF (‚g˜AŒvŽZ|ŽÀ“ú” > •¬•‰’S‰ñ”‚v) OR
012330           (‚g˜AŒvŽZ|ŽÀ“ú” = ZERO)
012340            MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
012350            MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
012360        ELSE
012370*
012380            IF ‚g˜AŒvŽZ|•‰’SŠz <= •‰’Sˆê’è‹àŠz‚v
012390                EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
012400*/“úX‚Ì•‰’SŠz‚Ì‡Œv‚ðŒŽ‚Ì•‰’SŠz‚Æ‚·‚éB(–ˆ“úŽlŽÌŒÜ“ü‚µ‚½‹àŠz‚Ì‡Œv)
012410                WHEN ZERO
012420                WHEN 2
012430                    COMPUTE •‰’SŠz‚v‚Q ROUNDED = ‚g˜AŒvŽZ|•‰’SŠz / 10
012440                    COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
012450                    MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
012460                    MOVE ZERO     TO ‚g˜AŒvŽZ|¿‹Šz•¬
012470                WHEN OTHER
012480*/ƒGƒ‰[•\Ž¦‚ÌC³
012490                    PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
012500                    DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
012510                            " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
012520                            " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
012530                    PERFORM ƒtƒ@ƒCƒ‹•Â½
012540                    MOVE ZERO TO PROGRAM-STATUS
012550                    EXIT PROGRAM
012560                END-EVALUATE
012570*
012580            ELSE
012590                EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
012600                WHEN ZERO
012610*/ Ý’è‚³‚ê‚½‹àŠz‚ð‚»‚Ì‚Ü‚Ü“]‹L‚·‚éB
012620                WHEN 2
012630                    MOVE •‰’Sˆê’è‹àŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
012640                    COMPUTE ¿‹Šz‚v     = ‚g˜AŒvŽZ|•‰’SŠz - •‰’Sˆê’è‹àŠz‚v
012650                    MOVE ¿‹Šz‚v       TO ‚g˜AŒvŽZ|¿‹Šz•¬
012660*
012670                WHEN OTHER
012680*/ƒGƒ‰[•\Ž¦‚ÌC³
012690                    PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
012700                    DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
012710                            " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
012720                            " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
012730                    PERFORM ƒtƒ@ƒCƒ‹•Â½
012740                    MOVE ZERO TO PROGRAM-STATUS
012750                    EXIT PROGRAM
012760                END-EVALUATE
012770            END-IF
012780        END-IF
012790     END-IF.
012800*
012810*================================================================*
012820 “ÁŽê•‰’SŒvŽZŒÄo‚µ SECTION.
012830*
012840* •¬‹àŠzŒvŽZ˜AŒgƒL[‚Ö•‰’S‹æ•ªƒZƒbƒg
012850     INITIALIZE ‚g˜A•¬ŒvŽZ|ƒL[.
012860     MOVE •‰’S‹æ•ª‚v TO ‚g˜A•¬ŒvŽZ|•‰’S‹æ•ª.
012870*
012880     CALL   "KHTJYOS".
012890     CANCEL "KHTJYOS".
012900*
012910*================================================================*
012920 ƒGƒ‰[•\Ž¦‚q SECTION.
012930*
012940     DISPLAY ƒtƒ@ƒCƒ‹–¼‚v NC"ƒtƒ@ƒCƒ‹“ÇžƒGƒ‰["     UPON CONS.
012950     DISPLAY NC"ó‘ÔƒL[F" ó‘ÔƒL[                 UPON CONS.
012960     DISPLAY NC"ƒVƒXƒeƒ€ŠÇ—ŽÒ‚É˜A—‚µ‚Ä‚­‚¾‚³‚¢"    UPON CONS.
012970     DISPLAY NC"”Žš‚P•¶Žš“ü—Í‚µ‚d‚m‚s‚d‚qƒL[‚ð‰Ÿ‚µ‚Ä‚­‚¾‚³‚¢"
012980                                                     UPON CONS.
012981*-----------------------------------------*
012982     CALL "actcshm"  WITH C LINKAGE.
012983*-----------------------------------------*
012990     ACCEPT  ƒL[“ü—Í FROM CONS.
013000     PERFORM ƒtƒ@ƒCƒ‹•Â½.
013010     MOVE ZERO TO PROGRAM-STATUS
013020     EXIT PROGRAM.
013030*================================================================*
013040 •¬ˆê•”•‰’SŽZ’è‚P‚S SECTION.
013050* m•¬n
013060******************************************************************
013070* “ú–ˆ‚Ì”ï—pŠz‚Ìe•ÛŒ¯•‰’S—¦•ª‚©‚ç˜Vlˆê•”•‰’S‹à‚Ì‚Ý‚ðŠ³ŽÒ•‰’SB
013080*iŒŽ‚ÉÅ‘åAŒv‚`|˜Vl•‰’S‰ñ”•ªj
013090* “¯ˆêŒŽ“à‚Å‰ŒŸ“ú‚ª‚ ‚Á‚Ä‚àA•ÛŒ¯‚ªØ‚è‘Ö‚í‚Á‚Ä‚à˜Vl•‰’S‰ñ”•ª‚Í
013100* ˆø‘±‚«ƒJƒEƒ“ƒg‚³‚ê‚éB
013110******************************************************************
013120*
013130* e•ÛŒ¯‚ªŒŽ‚Ì“r’†‚Å•Ï‚í‚èAŽ}”Ôˆ—‚ðs‚Á‚½ê‡‚ðl—¶‚µA‘Oe•ÛŒ¯‚Å
013140* f—Ã‚µ‚½“ú”‚ðŽó‚¯Œp‚®
013150*
013160***     PERFORM f—Ã‰ñ”Žæ“¾.
013170*
013180*  / ƒŒƒZ(ŒŽŒv)‚ÍA“ú‚²‚Æ‚Ì•¬ŒvŽZ•‰’SŠz‚ðWŒv‚·‚éPG‚ðCALL. ŒÄ‚Ño‚³‚ê‚½PG‘¤‚ÅA
013190*    •‰’SŠz•¬,¿‹Šz•¬‚ðƒZƒbƒg /
013200     IF ‚g˜AŒvŽZ|Ž{p“ú = ZERO
013210        PERFORM “ÁŽê•‰’SŒvŽZŒÄo‚µ
013220     ELSE
013230*
013240*/•¬•‰’S‹à–ÆœF‚X ‚È‚çA’áŠ“¾ŽÒ—p‚Ì•‰’SŠz‚ðŽg‚¤
013250         MOVE ‚g˜AŒvŽZ|Ž{p˜a—ï TO Žó|Ž{p˜a—ï
013260         MOVE ‚g˜AŒvŽZ|Ž{p”N   TO Žó|Ž{p”N
013270         MOVE ‚g˜AŒvŽZ|Ž{pŒŽ   TO Žó|Ž{pŒŽ
013280         MOVE ‚g˜AŒvŽZ|Š³ŽÒ”Ô† TO Žó|Š³ŽÒ”Ô†
013290         MOVE ‚g˜AŒvŽZ|Ž}”Ô     TO Žó|Ž}”Ô
013300         READ ŽófŽÒî•ñ‚e
013310         INVALID KEY
013320*/ƒGƒ‰[•\Ž¦‚ÌC³
013330             PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
013340             DISPLAY "Ž{pŒŽ‚ÌŽófŽÒî•ñ‚ª‚ ‚è‚Ü‚¹‚ñ"
013350                     " ŽófŽÒ‡‚=" ‚g˜AŒvŽZ|Š³ŽÒƒR[ƒh
013360                     " Ž{p”NŒŽ=" ‚g˜AŒvŽZ|Ž{p”N ‚g˜AŒvŽZ|Ž{pŒŽ UPON CONS
013370             PERFORM ƒtƒ@ƒCƒ‹•Â½
013380             MOVE ZERO TO PROGRAM-STATUS
013390             EXIT PROGRAM
013400         NOT INVALID KEY
013431             IF Žó|•¬•‰’S‹à–Æœ = 9
013432                 MOVE Ž––±Žè”—¿‚v TO •‰’Sˆê’è‹àŠz‚v
013433             END-IF
013434*----------------------------------------------*
013435*/21”N7ŒŽ‚æ‚èŒo‰ß‘[’u(‚Q”NŠÔ)090630
013436             IF Žó|•¬•‰’S‹à–Æœ = 8
013437                 EVALUATE Žó|•¬Ží•Ê
013438                 WHEN 52
013439                 WHEN 53
013440                     MOVE  900 TO •‰’Sˆê’è‹àŠz‚v
013441                 WHEN 55
013442                     MOVE 1200 TO •‰’Sˆê’è‹àŠz‚v
013443                 END-EVALUATE
013444             END-IF
013445*----------------------------------------------*
013450         END-READ
013451*
013460        IF (‚g˜AŒvŽZ|ŽÀ“ú” > •¬•‰’S‰ñ”‚v) OR
013470           (‚g˜AŒvŽZ|ŽÀ“ú” = ZERO)
013480            MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
013490            MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
013500        ELSE
013510*
013520            IF ‚g˜AŒvŽZ|•‰’SŠz <= •‰’Sˆê’è‹àŠz‚v
013530                EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
013540                WHEN ZERO
      *•‰’S‹æ•ª14‚Í‚·‚×‚Ä­”ˆÈ‰º‚ðØ‚èã‚°‚½•‰’SŠz(‚g˜AŒvŽZ‚R|•‰’SŠz)‚ðŽg—p‚·‚é/20221223
013550*                    MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|•‰’SŠz•¬
013550                    MOVE ‚g˜AŒvŽZ‚R|•‰’SŠz TO ‚g˜AŒvŽZ|•‰’SŠz•¬
013560                    MOVE ZERO             TO ‚g˜AŒvŽZ|¿‹Šz•¬
013570*
013580                WHEN 2
013590                    COMPUTE •‰’SŠz‚v‚Q ROUNDED = ‚g˜AŒvŽZ|•‰’SŠz / 10
013600                    COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
013610                    MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
013620                    MOVE ZERO     TO ‚g˜AŒvŽZ|¿‹Šz•¬
013630                WHEN OTHER
013640*/ƒGƒ‰[•\Ž¦‚ÌC³
013650                    PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
013660                    DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
013670                            " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
013680                            " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
013690                    PERFORM ƒtƒ@ƒCƒ‹•Â½
013700                    MOVE ZERO TO PROGRAM-STATUS
013710                    EXIT PROGRAM
013720                END-EVALUATE
013730*
013740            ELSE
013750                EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
013760                WHEN ZERO
013770* Œ»Ý‚ÍA˜Vlˆê•”•‰’S‹à‚ªA‚T‚O‚O‰~‚Ì‚½‚ßA‚P‚O‰~’PˆÊ‚É‚·‚é•K—v‚È‚µ
013780                WHEN 2
013790                    MOVE •‰’Sˆê’è‹àŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
013800                    COMPUTE ¿‹Šz‚v     = ‚g˜AŒvŽZ|•‰’SŠz - •‰’Sˆê’è‹àŠz‚v
013810                    MOVE ¿‹Šz‚v       TO ‚g˜AŒvŽZ|¿‹Šz•¬
013820*
013830                WHEN OTHER
013840*/ƒGƒ‰[•\Ž¦‚ÌC³
013850                    PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
013860                    DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
013870                            " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
013880                            " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
013890                    PERFORM ƒtƒ@ƒCƒ‹•Â½
013900                    MOVE ZERO TO PROGRAM-STATUS
013910                    EXIT PROGRAM
013920                END-EVALUATE
013930            END-IF
013940        END-IF
013950     END-IF.
013960*
013970*================================================================*
013980 ‘åã•‰’S‹à–Æœ‚Q‚V SECTION.
013990*
014000*
014010* e•ÛŒ¯‚ªŒŽ‚Ì“r’†‚Å•Ï‚í‚èAŽ}”Ôˆ—‚ðs‚Á‚½ê‡‚ðl—¶‚µA‘Oe•ÛŒ¯‚Å
014020* f—Ã‚µ‚½“ú”‚ðŽó‚¯Œp‚®
014030*
014040*     PERFORM f—Ã‰ñ”Žæ“¾.
014050*
014060     IF (‚g˜AŒvŽZ|ŽÀ“ú” > •¬•‰’S‰ñ”‚v) OR
014070        (‚g˜AŒvŽZ|ŽÀ“ú” = ZERO)
014080         MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz
014090         MOVE ‚g˜AŒvŽZ|”ï—pŠz TO ‚g˜AŒvŽZ|¿‹Šz
014100     ELSE
014110*
014120         IF ‚g˜AŒvŽZ|•‰’SŠz <= •‰’Sˆê’è‹àŠz‚v
014130             EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
014140             WHEN ZERO
014150                 CONTINUE
014160             WHEN 2
014170                 COMPUTE •‰’SŠz‚v‚Q ROUNDED = ‚g˜AŒvŽZ|•‰’SŠz / 10
014180                 COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
014190                 MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz
014200                 COMPUTE ‚g˜AŒvŽZ|¿‹Šz = ‚g˜AŒvŽZ|”ï—pŠz - ‚g˜AŒvŽZ|•‰’SŠz
014210             WHEN OTHER
014220*/ƒGƒ‰[•\Ž¦‚ÌC³
014230                 PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
014240                 DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
014250                         " ŽófŽÒ‡‚="   ‚g˜AŒvŽZ|Š³ŽÒƒR[ƒh
014260                         " Ž{p”NŒŽ="   ‚g˜AŒvŽZ|Ž{p”N ‚g˜AŒvŽZ|Ž{pŒŽ UPON CONS
014270                 PERFORM ƒtƒ@ƒCƒ‹•Â½
014280                 MOVE ZERO TO PROGRAM-STATUS
014290                 EXIT PROGRAM
014300             END-EVALUATE
014310*
014320         ELSE
014330             EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
014340             WHEN ZERO
014350             WHEN 2
014360                 MOVE    •‰’Sˆê’è‹àŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz
014370                 COMPUTE ¿‹Šz‚v        = ‚g˜AŒvŽZ|”ï—pŠz - •‰’Sˆê’è‹àŠz‚v
014380                 MOVE    ¿‹Šz‚v       TO ‚g˜AŒvŽZ|¿‹Šz
014390*
014400             WHEN OTHER
014410*/ƒGƒ‰[•\Ž¦‚ÌC³
014420                 PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
014430                 DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
014440                         " ŽófŽÒ‡‚="   ‚g˜AŒvŽZ|Š³ŽÒƒR[ƒh
014450                         " Ž{p”NŒŽ="   ‚g˜AŒvŽZ|Ž{p”N ‚g˜AŒvŽZ|Ž{pŒŽ UPON CONS
014460                 PERFORM ƒtƒ@ƒCƒ‹•Â½
014470                 MOVE ZERO TO PROGRAM-STATUS
014480                 EXIT PROGRAM
014490             END-EVALUATE
014500         END-IF
014510     END-IF.
014520*
014530*================================================================*
014540 ‘åã•‰’S‹à–Æœ‚S‚P SECTION.
014550*
014560* e•ÛŒ¯‚ªŒŽ‚Ì“r’†‚Å•Ï‚í‚èAŽ}”Ôˆ—‚ðs‚Á‚½ê‡‚ðl—¶‚µA‘Oe•ÛŒ¯‚Å
014570* f—Ã‚µ‚½“ú”‚ðŽó‚¯Œp‚®
014580*
014590*     PERFORM f—Ã‰ñ”Žæ“¾.
014600*
014610     IF (‚g˜AŒvŽZ|ŽÀ“ú” > •¬•‰’S‰ñ”‚v) OR
014620        (‚g˜AŒvŽZ|ŽÀ“ú” = ZERO)
014630         MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
014640         MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
014650     ELSE
014660*
014670         IF ‚g˜AŒvŽZ|•‰’SŠz•¬ <= •‰’Sˆê’è‹àŠz‚v
014680             EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
014690             WHEN ZERO
014700                 CONTINUE
014710             WHEN 2
014720                 COMPUTE •‰’SŠz‚v‚Q ROUNDED = ‚g˜AŒvŽZ|•‰’SŠz•¬ / 10
014730                 COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
014740                 MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
014750                 MOVE ZERO     TO ‚g˜AŒvŽZ|¿‹Šz•¬
014760             WHEN OTHER
014770*/ƒGƒ‰[•\Ž¦‚ÌC³
014780                 PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
014790                 DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
014800                         " ŽófŽÒ‡‚="   ‚g˜AŒvŽZ|Š³ŽÒƒR[ƒh
014810                         " Ž{p”NŒŽ="   ‚g˜AŒvŽZ|Ž{p”N ‚g˜AŒvŽZ|Ž{pŒŽ UPON CONS
014820                 PERFORM ƒtƒ@ƒCƒ‹•Â½
014830                 MOVE ZERO TO PROGRAM-STATUS
014840                 EXIT PROGRAM
014850             END-EVALUATE
014860*
014870         ELSE
014880             EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
014890             WHEN ZERO
014900             WHEN 2
014910                 MOVE    •‰’Sˆê’è‹àŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
014920                 COMPUTE ¿‹Šz‚v        = ‚g˜AŒvŽZ|•‰’SŠz - •‰’Sˆê’è‹àŠz‚v
014930                 MOVE    ¿‹Šz‚v       TO ‚g˜AŒvŽZ|¿‹Šz•¬
014940*
014950             WHEN OTHER
014960*/ƒGƒ‰[•\Ž¦‚ÌC³
014970                 PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
014980                 DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
014990                         " ŽófŽÒ‡‚="   ‚g˜AŒvŽZ|Š³ŽÒƒR[ƒh
015000                         " Ž{p”NŒŽ="   ‚g˜AŒvŽZ|Ž{p”N ‚g˜AŒvŽZ|Ž{pŒŽ UPON CONS
015010                 PERFORM ƒtƒ@ƒCƒ‹•Â½
015020                 MOVE ZERO TO PROGRAM-STATUS
015030                 EXIT PROGRAM
015040             END-EVALUATE
015050         END-IF
015060     END-IF.
015070*
015080*================================================================*
015081*================================================================*
015082 •¬ˆê•”•‰’SŽZ’è‚O‚T SECTION.
015083* m•¬n
015084*
015085* •‰’Sˆê’è‹àŠz‚v‚ÉãŒÀ‚Pi’á‚¢‹àŠzjAŽ––±Žè”—¿‚v‚ÉãŒÀ‚Qi‚‚¢‹àŠzj‚ª“ü‚Á‚Ä‚¢‚éB
015086*
015087*
015088*  / ƒŒƒZ(ŒŽŒv)‚ÍA“ú‚²‚Æ‚Ì•¬ŒvŽZ•‰’SŠz‚ðWŒv‚·‚éPG‚ðCALL. ŒÄ‚Ño‚³‚ê‚½PG‘¤‚ÅA
015089*    •‰’SŠz•¬,¿‹Šz•¬‚ðƒZƒbƒg /
015090     IF ‚g˜AŒvŽZ|Ž{p“ú = ZERO
015091        PERFORM “ÁŽê•‰’SŒvŽZŒÄo‚µ
015092     ELSE
015096*
015097*       / •¬•‰’S‹à–ÆœF‚X ‚È‚çAŽ––±Žè”—¿‚Ì•‰’SŠz‚ðŽg‚¤
015098        EVALUATE ‚g˜AŒvŽZ•¬—ÝŒvŠz|•¬•‰’S‹à–Æœ
015099        WHEN ZERO
015100            MOVE •‰’Sˆê’è‹àŠz‚v TO ŒvŽZŒÀ“xŠz‚v
015101        WHEN 1
015102            MOVE ZERO           TO ŒvŽZŒÀ“xŠz‚v
015103        WHEN 9
015104            MOVE Ž––±Žè”—¿‚v   TO ŒvŽZŒÀ“xŠz‚v
015105        WHEN OTHER
015106            MOVE •‰’Sˆê’è‹àŠz‚v TO ŒvŽZŒÀ“xŠz‚v
015107        END-EVALUATE
015108*
015110*       / “úŒv /
015111        IF ‚g˜AŒvŽZ•¬—ÝŒvŠz|•‰’SŠz•¬ >= ŒvŽZŒÀ“xŠz‚v
015115            MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
015116            MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
015119        ELSE
015120*
015121             EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
015122             WHEN ZERO
015123                 MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|•‰’SŠz•¬  •‰’SŠz‚v
015124                 MOVE ZERO             TO ‚g˜AŒvŽZ|¿‹Šz•¬
015125*
015126                 COMPUTE ŒvŽZ—p—ÝŒvŠz‚v = ‚g˜AŒvŽZ•¬—ÝŒvŠz|•‰’SŠz•¬ + •‰’SŠz‚v
015127                 IF  ŒvŽZ—p—ÝŒvŠz‚v > ŒvŽZŒÀ“xŠz‚v
015128                     COMPUTE ‚g˜AŒvŽZ|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ‚g˜AŒvŽZ•¬—ÝŒvŠz|•‰’SŠz•¬
015129                     COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬ = •‰’SŠz‚v - ‚g˜AŒvŽZ|•‰’SŠz•¬
015130                 END-IF
015135             WHEN 2
015136                 COMPUTE •‰’SŠz‚v‚Q ROUNDED = ‚g˜AŒvŽZ|•‰’SŠz / 10
015137                 COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
015138                 MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
015139                 MOVE ZERO     TO ‚g˜AŒvŽZ|¿‹Šz•¬
015140*
015141                 COMPUTE ŒvŽZ—p—ÝŒvŠz‚v = ‚g˜AŒvŽZ•¬—ÝŒvŠz|•‰’SŠz•¬ + •‰’SŠz‚v
015142                 IF  ŒvŽZ—p—ÝŒvŠz‚v > ŒvŽZŒÀ“xŠz‚v
015143                     COMPUTE ‚g˜AŒvŽZ|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ‚g˜AŒvŽZ•¬—ÝŒvŠz|•‰’SŠz•¬
015144                     COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬ = •‰’SŠz‚v - ‚g˜AŒvŽZ|•‰’SŠz•¬
015145                 END-IF
015146             END-EVALUATE
015147        END-IF
015193*
015203     END-IF.
015241*
015242*================================================================*
015243 •¬ˆê•”•‰’SŽZ’è‚P‚U SECTION.
015244* m•¬n
015245*
015246* ŒÀ“xŠz‚P‚vAŒÀ“xŠz‚Q‚vAŒÀ“xŠz‚R‚v‚É‹àŠz‚Ì‚‚¢‡‚É“ü‚Á‚Ä‚¢‚éBB
015256*
015257*
015258*  / ƒŒƒZ(ŒŽŒv)‚ÍA“ú‚²‚Æ‚Ì•¬ŒvŽZ•‰’SŠz‚ðWŒv‚·‚éPG‚ðCALL. ŒÄ‚Ño‚³‚ê‚½PG‘¤‚ÅA
015259*    •‰’SŠz•¬,¿‹Šz•¬‚ðƒZƒbƒg /
015260     IF ‚g˜AŒvŽZ|Ž{p“ú = ZERO
015261        PERFORM “ÁŽê•‰’SŒvŽZŒÄo‚µ
015262     ELSE
015263*
015264*       / •¬•‰’S‹à–ÆœF‚X ‚È‚çAŽ––±Žè”—¿‚Ì•‰’SŠz‚ðŽg‚¤
015265        EVALUATE ‚g˜AŒvŽZ•¬—ÝŒvŠz|•¬•‰’S‹à–Æœ
015278         WHEN ZERO
015279             MOVE ŒÀ“xŠz‚P‚v TO ŒvŽZŒÀ“xŠz‚v
015280         WHEN 1
015281             MOVE ZERO       TO ŒvŽZŒÀ“xŠz‚v
015282         WHEN 8
015283             MOVE ŒÀ“xŠz‚Q‚v TO ŒvŽZŒÀ“xŠz‚v
015284         WHEN 9
015285             MOVE ŒÀ“xŠz‚R‚v TO ŒvŽZŒÀ“xŠz‚v
015286         WHEN OTHER
015287             MOVE ŒÀ“xŠz‚P‚v TO ŒvŽZŒÀ“xŠz‚v
015288         END-EVALUATE
015289*
015290**
015291*       / “úŒv /
015292        IF ‚g˜AŒvŽZ•¬—ÝŒvŠz|•‰’SŠz•¬ >= ŒvŽZŒÀ“xŠz‚v
015293            MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
015294            MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
015295        ELSE
015296*
015297             EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
015298             WHEN ZERO
015302                 MOVE ZERO TO •‰’SŠz“úŒv‘‹Œû‚v
015303                 COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •¬•‰’S—¦‚v / 100
015304                 COMPUTE •‰’SŠz“úŒv‘‹Œû‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
015306                 MOVE •‰’SŠz“úŒv‘‹Œû‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬  •‰’SŠz‚v
015307                 MOVE ZERO     TO ‚g˜AŒvŽZ|¿‹Šz•¬
015309*
015310                 COMPUTE ŒvŽZ—p—ÝŒvŠz‚v = ‚g˜AŒvŽZ•¬—ÝŒvŠz|•‰’SŠz•¬ + •‰’SŠz‚v
015311                 IF  ŒvŽZ—p—ÝŒvŠz‚v > ŒvŽZŒÀ“xŠz‚v
015312                     COMPUTE ‚g˜AŒvŽZ|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ‚g˜AŒvŽZ•¬—ÝŒvŠz|•‰’SŠz•¬
015313                     COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬ = •‰’SŠz‚v - ‚g˜AŒvŽZ|•‰’SŠz•¬
015314                 END-IF
015315             WHEN 2
015316                 MOVE ZERO TO •‰’SŠz“úŒv‘‹Œû‚v
015317                 COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •¬•‰’S—¦‚v / 100
015318                 COMPUTE •‰’SŠz“úŒv‘‹Œû‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
015319                 COMPUTE •‰’SŠz‚v‚Q ROUNDED = •‰’SŠz“úŒv‘‹Œû‚v / 10
015320                 COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
015321                 MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
015322                 MOVE ZERO     TO ‚g˜AŒvŽZ|¿‹Šz•¬
015323*
015324                 COMPUTE ŒvŽZ—p—ÝŒvŠz‚v = ‚g˜AŒvŽZ•¬—ÝŒvŠz|•‰’SŠz•¬ + •‰’SŠz‚v
015325                 IF  ŒvŽZ—p—ÝŒvŠz‚v > ŒvŽZŒÀ“xŠz‚v
015326                     COMPUTE ‚g˜AŒvŽZ|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ‚g˜AŒvŽZ•¬—ÝŒvŠz|•‰’SŠz•¬
015327                     COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬ = •‰’SŠz‚v - ‚g˜AŒvŽZ|•‰’SŠz•¬
015328                 END-IF
015329             END-EVALUATE
015330        END-IF
015342*
015392     END-IF.
015393*
015394*================================================================*
015395*================================================================*
015396 •¬ˆê•”•‰’SŽZ’è‚P‚V SECTION.
015397* m•¬n
015407*
015409*  / ƒŒƒZ(ŒŽŒv)‚ÍA“ú‚²‚Æ‚Ì•¬ŒvŽZ•‰’SŠz‚ðWŒv‚·‚éPG‚ðCALL. ŒÄ‚Ño‚³‚ê‚½PG‘¤‚ÅA
015410*    •‰’SŠz•¬,¿‹Šz•¬‚ðƒZƒbƒg /
015411     IF ‚g˜AŒvŽZ|Ž{p“ú = ZERO
015412        PERFORM “ÁŽê•‰’SŒvŽZŒÄo‚µ
015413     ELSE
015414        IF (‚g˜AŒvŽZ|ŽÀ“ú” > •¬•‰’S‰ñ”‚v) OR
015415           (‚g˜AŒvŽZ|ŽÀ“ú” = ZERO)
015416            MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
015417            MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
015418        ELSE
015419*
015422*           / •¬•‰’S‹à–ÆœF‚X ‚È‚çAŽ––±Žè”—¿‚Ì•‰’SŠz‚ðŽg‚¤
015423            IF ‚g˜AŒvŽZ•¬—ÝŒvŠz|•¬•‰’S‹à–Æœ = 9
015424               MOVE Ž––±Žè”—¿‚v TO •‰’Sˆê’è‹àŠz‚v
015425            END-IF
015426*
015427***
015428             EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
015429             WHEN ZERO
015430                 MOVE ZERO TO •‰’SŠz“úŒv‘‹Œû‚v
015431                 COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •¬•‰’S—¦‚v / 100
015432                 COMPUTE •‰’SŠz“úŒv‘‹Œû‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
015433                 MOVE •‰’SŠz“úŒv‘‹Œû‚v TO  •‰’SŠz‚v
015434**
015435                 IF •‰’SŠz‚v <= •‰’Sˆê’è‹àŠz‚v
015436                    MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
015437                    MOVE ZERO     TO ‚g˜AŒvŽZ|¿‹Šz•¬
015438                 ELSE
015439                    MOVE •‰’Sˆê’è‹àŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
015440                    COMPUTE ¿‹Šz‚v     = ‚g˜AŒvŽZ|•‰’SŠz - •‰’Sˆê’è‹àŠz‚v
015441                    MOVE ¿‹Šz‚v       TO ‚g˜AŒvŽZ|¿‹Šz•¬
015442                 END-IF
015444             WHEN 2
015446                 MOVE ZERO TO •‰’SŠz“úŒv‘‹Œû‚v
015447                 COMPUTE •‰’S—¦’†ŠÔŒ‹‰Ê‚v =  •¬•‰’S—¦‚v / 100
015448                 COMPUTE •‰’SŠz“úŒv‘‹Œû‚v = ‚g˜AŒvŽZ|”ï—pŠz * •‰’S—¦’†ŠÔŒ‹‰Ê‚v
015449                 COMPUTE •‰’SŠz‚v‚Q ROUNDED = •‰’SŠz“úŒv‘‹Œû‚v / 10
015450                 COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
015451**
015452                 IF •‰’SŠz‚v <= •‰’Sˆê’è‹àŠz‚v
015453                    MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
015454                    MOVE ZERO     TO ‚g˜AŒvŽZ|¿‹Šz•¬
015455                 ELSE
015456                    MOVE •‰’Sˆê’è‹àŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
015457                    COMPUTE ¿‹Šz‚v     = ‚g˜AŒvŽZ|•‰’SŠz - •‰’Sˆê’è‹àŠz‚v
015458                    MOVE ¿‹Šz‚v       TO ‚g˜AŒvŽZ|¿‹Šz•¬
015459                 END-IF
015463             END-EVALUATE
015465**
015474        END-IF
015475     END-IF.
015566*
015567*================================================================*
008220 •¬ˆê•”•‰’SŽZ’è‚Q‚Q SECTION.
008230* m•¬n
008240******************************************************************
008250* “ú–ˆ‚Ì”ï—pŠz‚Ìe•ÛŒ¯•‰’S—¦•ª‚©‚ç˜Vlˆê•”•‰’S‹à‚Ì‚Ý‚ðŠ³ŽÒ•‰’SB
008260*iŒŽ‚ÉÅ‘åAŒv‚`|˜Vl•‰’S‰ñ”•ªj
008270* “¯ˆêŒŽ“à‚Å‰ŒŸ“ú‚ª‚ ‚Á‚Ä‚àA•ÛŒ¯‚ªØ‚è‘Ö‚í‚Á‚Ä‚à˜Vl•‰’S‰ñ”•ª‚Í
008280* ˆø‘±‚«ƒJƒEƒ“ƒg‚³‚ê‚éB
008290******************************************************************
008300*
008310* e•ÛŒ¯‚ªŒŽ‚Ì“r’†‚Å•Ï‚í‚èAŽ}”Ôˆ—‚ðs‚Á‚½ê‡‚ðl—¶‚µA‘Oe•ÛŒ¯‚Å
008320* f—Ã‚µ‚½“ú”‚ðŽó‚¯Œp‚®
008330*
008340***     PERFORM f—Ã‰ñ”Žæ“¾.
      */“–ŒŽŽ}”Ô‚Ì—ÝŒv‹àŠz‚ðŽæ“¾/20240424
013583        PERFORM Ž}”Ô‹àŠzŽæ“¾.
008350*
008360*  / ƒŒƒZ(ŒŽŒv)‚ÍA“ú‚²‚Æ‚Ì•¬ŒvŽZ•‰’SŠz‚ðWŒv‚·‚éPG‚ðCALL. ŒÄ‚Ño‚³‚ê‚½PG‘¤‚ÅA
008370*    •‰’SŠz•¬,¿‹Šz•¬‚ðƒZƒbƒg /
008380     IF ‚g˜AŒvŽZ|Ž{p“ú = ZERO
008390        PERFORM “ÁŽê•‰’SŒvŽZŒÄo‚µ
008400     ELSE
015263*
015264*       / •¬•‰’S‹à–ÆœF‚X ‚È‚çAŽ––±Žè”—¿‚Ì•‰’SŠz‚ðŽg‚¤
015265        EVALUATE ‚g˜AŒvŽZ•¬—ÝŒvŠz|•¬•‰’S‹à–Æœ
015278         WHEN ZERO
015279             MOVE ŒÀ“xŠz‚P‚v TO ŒvŽZŒÀ“xŠz‚v
015280         WHEN 1
015281             MOVE ZERO       TO ŒvŽZŒÀ“xŠz‚v
015282         WHEN 8
015283             MOVE ŒÀ“xŠz‚Q‚v TO ŒvŽZŒÀ“xŠz‚v
015284         WHEN 9
015285             MOVE ŒÀ“xŠz‚R‚v TO ŒvŽZŒÀ“xŠz‚v
015286         WHEN OTHER
015287             MOVE ŒÀ“xŠz‚P‚v TO ŒvŽZŒÀ“xŠz‚v
015288         END-EVALUATE
015289*
      *Ž}”Ô‚ª‚ ‚éê‡‘O‚ÌŽ}”Ô‚Å•‰’S‚µ‚½‹àŠz‚ðœ‚­«««/20240424
013970        IF Ž}”Ôƒtƒ‰ƒO NOT = SPACE
                  COMPUTE ŒvŽZŒÀ“xŠz‚v = ŒvŽZŒÀ“xŠz‚v - ‘O—ÝŒv‹àŠz‚v
              END-IF
      *Ž}”Ô‚ª‚ ‚éê‡‘O‚ÌŽ}”Ô‚Å•‰’S‚µ‚½‹àŠz‚ðœ‚­ªªª/20240424
008410        IF (‚g˜AŒvŽZ|ŽÀ“ú” > •¬•‰’S‰ñ”‚v) OR
008420           (‚g˜AŒvŽZ|ŽÀ“ú” = ZERO)
008430            MOVE ZERO             TO ‚g˜AŒvŽZ|•‰’SŠz•¬
008440            MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž«««/20221129
                  IF Žó|”ï—p•‰’SŽÒ”Ô†•¬(3:2) = "27"
008430                MOVE ZERO               TO ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
008440                MOVE ‚g˜AŒvŽZ‚R|•‰’SŠz TO ‚g˜AŒvŽZ|¿‹Šz•¬
                  END-IF
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰žªªª/20221129
008450        ELSE
008460*
008470            IF ‚g˜AŒvŽZ|•‰’SŠz <= •‰’Sˆê’è‹àŠz‚v
008480                EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
008490                WHEN ZERO
008500                    MOVE ‚g˜AŒvŽZ|•‰’SŠz TO ‚g˜AŒvŽZ|•‰’SŠz•¬
008510                    MOVE ZERO             TO ‚g˜AŒvŽZ|¿‹Šz•¬
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž«««/20221128
                          IF Žó|”ï—p•‰’SŽÒ”Ô†•¬(3:2) = "27"
008500                        MOVE ‚g˜AŒvŽZ‚R|•‰’SŠz TO ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                          END-IF
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰žªªª/20221128
                          IF ˜A|“úŒvƒŒƒZ‚e = ZERO
      */—ÝŒv‚ªŒÀ“xŠz‚ð’´‚¦‚é‚Ü‚Å•‰’S‚·‚é(1‰~’PˆÊ)
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž/20221129
                              IF Žó|”ï—p•‰’SŽÒ”Ô†•¬(3:2) NOT = "27"
                              IF ˜A|•‰’SŠz•¬‚P‚f‚v + ‚g˜AŒvŽZ|•‰’SŠz•¬ > ŒvŽZŒÀ“xŠz‚v
                                  COMPUTE ‚g˜AŒvŽZ|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ˜A|•‰’SŠz•¬‚P‚f‚v
      */‘åã•¬F“ú‚ÌãŒÀ‚ð1“x‚à‰z‚¦‚È‚¢ê‡A–{‘Ì‚Ì•‰’SŠz‚Æ·‚ª‡‚Á‚Ä‚à¿‹‚µ‚È‚¢/20220830
                                  MOVE SPACE TO ‘SŠzŠ³ŽÒ•‰’S‚e
      *
                              END-IF
                              COMPUTE ˜A|•‰’SŠz•¬‚P‚f‚v = ˜A|•‰’SŠz•¬‚P‚f‚v + ‚g˜AŒvŽZ|•‰’SŠz•¬
                              COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬ = ‚g˜AŒvŽZ|•‰’SŠz - ‚g˜AŒvŽZ|•‰’SŠz•¬
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž«««/20221129
                              ELSE
      */—ÝŒv‚ªŒÀ“xŠz‚ð’´‚¦‚é‚Ü‚Å•‰’S‚·‚é(1‰~’PˆÊ)
                                  IF ˜A|•‰’SŠz•¬‚P‚f‚v + ‚g˜AŒvŽZ‚R|•‰’SŠz•¬ > ŒvŽZŒÀ“xŠz‚v
                                      COMPUTE ‚g˜AŒvŽZ‚R|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ˜A|•‰’SŠz•¬‚P‚f‚v
      */‘åã•¬F“ú‚ÌãŒÀ‚ð1“x‚à‰z‚¦‚È‚¢ê‡A–{‘Ì‚Ì•‰’SŠz‚Æ·‚ª‡‚Á‚Ä‚à¿‹‚µ‚È‚¢/20220830
                                      MOVE SPACE TO ‘SŠzŠ³ŽÒ•‰’S‚e
      *
                                  END-IF
                                  COMPUTE ˜A|•‰’SŠz•¬‚P‚f‚v = ˜A|•‰’SŠz•¬‚P‚f‚v + ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                                  COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬ = ‚g˜AŒvŽZ‚R|•‰’SŠz - ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                              END-IF
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰žªªª/20221129
                          ELSE
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž/20221129
                              IF Žó|”ï—p•‰’SŽÒ”Ô†•¬(3:2) NOT = "27"
      */—ÝŒv‚ªŒÀ“xŠz‚ð’´‚¦‚é‚Ü‚Å•‰’S‚·‚é(1‰~’PˆÊ(ƒŒƒZƒvƒg—p))
                              IF ˜A|•‰’SŠz•¬‚P‚f‚v‚Q + ‚g˜AŒvŽZ|•‰’SŠz•¬ > ŒvŽZŒÀ“xŠz‚v
      */‘åã•¬F“ú‚ÌãŒÀ‚ð1“x‚à‰z‚¦‚È‚¢ê‡A–{‘Ì‚Ì•‰’SŠz‚Æ·‚ª‡‚Á‚Ä‚à¿‹‚µ‚È‚¢/20220830
                                  MOVE SPACE TO ‘SŠzŠ³ŽÒ•‰’S‚e
      *
                                  COMPUTE ‚g˜AŒvŽZ|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ˜A|•‰’SŠz•¬‚P‚f‚v‚Q
                              END-IF
                              COMPUTE ˜A|•‰’SŠz•¬‚P‚f‚v‚Q = ˜A|•‰’SŠz•¬‚P‚f‚v‚Q + ‚g˜AŒvŽZ|•‰’SŠz•¬
                              COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬   = ‚g˜AŒvŽZ|•‰’SŠz - ‚g˜AŒvŽZ|•‰’SŠz•¬
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž«««/20221129
                              ELSE
      */—ÝŒv‚ªŒÀ“xŠz‚ð’´‚¦‚é‚Ü‚Å•‰’S‚·‚é(1‰~’PˆÊ(ƒŒƒZƒvƒg—p))
                                  IF ˜A|•‰’SŠz•¬‚P‚f‚v‚Q + ‚g˜AŒvŽZ‚R|•‰’SŠz•¬ > ŒvŽZŒÀ“xŠz‚v
      */‘åã•¬F“ú‚ÌãŒÀ‚ð1“x‚à‰z‚¦‚È‚¢ê‡A–{‘Ì‚Ì•‰’SŠz‚Æ·‚ª‡‚Á‚Ä‚à¿‹‚µ‚È‚¢/20220830
                                      MOVE SPACE TO ‘SŠzŠ³ŽÒ•‰’S‚e
      *
                                      COMPUTE ‚g˜AŒvŽZ‚R|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ˜A|•‰’SŠz•¬‚P‚f‚v‚Q
                                  END-IF
                                  COMPUTE ˜A|•‰’SŠz•¬‚P‚f‚v‚Q = ˜A|•‰’SŠz•¬‚P‚f‚v‚Q + ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                                  COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬   = ‚g˜AŒvŽZ‚R|•‰’SŠz - ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                              END-IF
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰žªªª/20221129
                          END-IF
008520*
008530                WHEN 2
008540                    COMPUTE •‰’SŠz‚v‚Q ROUNDED = ‚g˜AŒvŽZ|•‰’SŠz / 10
008550                    COMPUTE •‰’SŠz‚v = •‰’SŠz‚v‚Q * 10
008560                    MOVE •‰’SŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
008570                    MOVE ZERO     TO ‚g˜AŒvŽZ|¿‹Šz•¬
      */—ÝŒv‚ªŒÀ“xŠz‚ð’´‚¦‚é‚Ü‚Å•‰’S‚·‚é(10‰~’PˆÊ)
                          IF ˜A|•‰’SŠz•¬‚P‚O‚f‚v + ‚g˜AŒvŽZ|•‰’SŠz•¬ > ŒvŽZŒÀ“xŠz‚v
      */‘åã•¬F“ú‚ÌãŒÀ‚ð1“x‚à‰z‚¦‚È‚¢ê‡A–{‘Ì‚Ì•‰’SŠz‚Æ·‚ª‡‚Á‚Ä‚à¿‹‚µ‚È‚¢/20220830
                              MOVE SPACE TO ‘SŠzŠ³ŽÒ•‰’S‚e
      *
                              COMPUTE ‚g˜AŒvŽZ|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ˜A|•‰’SŠz•¬‚P‚O‚f‚v
                          END-IF
                          COMPUTE ˜A|•‰’SŠz•¬‚P‚O‚f‚v = ˜A|•‰’SŠz•¬‚P‚O‚f‚v + ‚g˜AŒvŽZ|•‰’SŠz•¬
                          COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬ = ‚g˜AŒvŽZ|•‰’SŠz - ‚g˜AŒvŽZ|•‰’SŠz•¬
008580                WHEN OTHER
008590*/ƒGƒ‰[•\Ž¦‚ÌC³
008600                    PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
008610                    DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
008620                            " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
008630                            " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
008640                    PERFORM ƒtƒ@ƒCƒ‹•Â½
008650                    MOVE ZERO TO PROGRAM-STATUS
008660                    EXIT PROGRAM
008670                END-EVALUATE
008680*
008690            ELSE
      */‘åã•¬F“ú‚ÌãŒÀ‚ð1“x‚à‰z‚¦‚È‚¢ê‡A–{‘Ì‚Ì•‰’SŠz‚Æ·‚ª‡‚Á‚Ä‚à¿‹‚µ‚È‚¢/20220830
                      MOVE SPACE TO ‘SŠzŠ³ŽÒ•‰’S‚e
      *
008700                EVALUATE ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª
008710                WHEN ZERO
008720* Œ»Ý‚ÍA˜Vlˆê•”•‰’S‹à‚ªA‚T‚O‚O‰~‚Ì‚½‚ßA‚P‚O‰~’PˆÊ‚É‚·‚é•K—v‚È‚µ
008730                WHEN 2
008740                    MOVE •‰’Sˆê’è‹àŠz‚v TO ‚g˜AŒvŽZ|•‰’SŠz•¬
008750                    COMPUTE ¿‹Šz‚v     = ‚g˜AŒvŽZ|•‰’SŠz - •‰’Sˆê’è‹àŠz‚v
008760                    MOVE ¿‹Šz‚v       TO ‚g˜AŒvŽZ|¿‹Šz•¬
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž/20221129
008740                    MOVE •‰’Sˆê’è‹àŠz‚v TO ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž/20221129
                          IF Žó|”ï—p•‰’SŽÒ”Ô†•¬(3:2) NOT = "27"
      */—ÝŒv‚ªŒÀ“xŠz‚ð’´‚¦‚é‚Ü‚Å•‰’S‚·‚é((ƒŒƒZƒvƒg—p)(1‰~’PˆÊ)(10‰~’PˆÊ)‚»‚ê‚¼‚ê‚É—ÝŒv‚·‚é)
                          IF ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª = ZERO
      */1‰~’PˆÊ
                              IF ˜A|“úŒvƒŒƒZ‚e = ZERO
                                  IF ˜A|•‰’SŠz•¬‚P‚f‚v + ‚g˜AŒvŽZ|•‰’SŠz•¬ > ŒvŽZŒÀ“xŠz‚v
                                      COMPUTE ‚g˜AŒvŽZ|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ˜A|•‰’SŠz•¬‚P‚f‚v
                                  END-IF
                                  COMPUTE ˜A|•‰’SŠz•¬‚P‚f‚v = ˜A|•‰’SŠz•¬‚P‚f‚v + ‚g˜AŒvŽZ|•‰’SŠz•¬
                                  COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬ = ‚g˜AŒvŽZ|•‰’SŠz - ‚g˜AŒvŽZ|•‰’SŠz•¬
                              ELSE
      */ƒŒƒZƒvƒg—p
                                  IF ˜A|•‰’SŠz•¬‚P‚f‚v‚Q + ‚g˜AŒvŽZ|•‰’SŠz•¬ > ŒvŽZŒÀ“xŠz‚v
                                      COMPUTE ‚g˜AŒvŽZ|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ˜A|•‰’SŠz•¬‚P‚f‚v‚Q
                                  END-IF
                                  COMPUTE ˜A|•‰’SŠz•¬‚P‚f‚v‚Q = ˜A|•‰’SŠz•¬‚P‚f‚v‚Q + ‚g˜AŒvŽZ|•‰’SŠz•¬
                                  COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬   = ‚g˜AŒvŽZ|•‰’SŠz - ‚g˜AŒvŽZ|•‰’SŠz•¬
                              END-IF
                          ELSE
      */10‰~’PˆÊ
                              IF ˜A|•‰’SŠz•¬‚P‚O‚f‚v + ‚g˜AŒvŽZ|•‰’SŠz•¬ > ŒvŽZŒÀ“xŠz‚v
                                  COMPUTE ‚g˜AŒvŽZ|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ˜A|•‰’SŠz•¬‚P‚O‚f‚v
                              END-IF
                              COMPUTE ˜A|•‰’SŠz•¬‚P‚O‚f‚v = ˜A|•‰’SŠz•¬‚P‚O‚f‚v + ‚g˜AŒvŽZ|•‰’SŠz•¬
                              COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬ = ‚g˜AŒvŽZ|•‰’SŠz - ‚g˜AŒvŽZ|•‰’SŠz•¬
                          END-IF
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰ž«««/20221130
                          ELSE
      */—ÝŒv‚ªŒÀ“xŠz‚ð’´‚¦‚é‚Ü‚Å•‰’S‚·‚é((ƒŒƒZƒvƒg—p)(1‰~’PˆÊ)(10‰~’PˆÊ)‚»‚ê‚¼‚ê‚É—ÝŒv‚·‚é)
                              IF ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª = ZERO
      */1‰~’PˆÊ
                                  IF ˜A|“úŒvƒŒƒZ‚e = ZERO
                                      IF ˜A|•‰’SŠz•¬‚P‚f‚v + ‚g˜AŒvŽZ‚R|•‰’SŠz•¬ > ŒvŽZŒÀ“xŠz‚v
                                          COMPUTE ‚g˜AŒvŽZ‚R|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ˜A|•‰’SŠz•¬‚P‚f‚v
                                      END-IF
                                      COMPUTE ˜A|•‰’SŠz•¬‚P‚f‚v = ˜A|•‰’SŠz•¬‚P‚f‚v + ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                                      COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬ = ‚g˜AŒvŽZ‚R|•‰’SŠz - ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                                  ELSE
      */ƒŒƒZƒvƒg—p
                                      IF ˜A|•‰’SŠz•¬‚P‚f‚v‚Q + ‚g˜AŒvŽZ‚R|•‰’SŠz•¬ > ŒvŽZŒÀ“xŠz‚v
                                          COMPUTE ‚g˜AŒvŽZ‚R|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ˜A|•‰’SŠz•¬‚P‚f‚v‚Q
                                      END-IF
                                      COMPUTE ˜A|•‰’SŠz•¬‚P‚f‚v‚Q = ˜A|•‰’SŠz•¬‚P‚f‚v‚Q + ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                                      COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬   = ‚g˜AŒvŽZ‚R|•‰’SŠz - ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                                  END-IF
                              ELSE
      */10‰~’PˆÊ
                                  IF ˜A|•‰’SŠz•¬‚P‚O‚f‚v + ‚g˜AŒvŽZ‚R|•‰’SŠz•¬ > ŒvŽZŒÀ“xŠz‚v
                                      COMPUTE ‚g˜AŒvŽZ‚R|•‰’SŠz•¬ = ŒvŽZŒÀ“xŠz‚v - ˜A|•‰’SŠz•¬‚P‚O‚f‚v
                                  END-IF
                                  COMPUTE ˜A|•‰’SŠz•¬‚P‚O‚f‚v = ˜A|•‰’SŠz•¬‚P‚O‚f‚v + ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                                  COMPUTE ‚g˜AŒvŽZ|¿‹Šz•¬ = ‚g˜AŒvŽZ‚R|•‰’SŠz - ‚g˜AŒvŽZ‚R|•‰’SŠz•¬
                              END-IF
                          END-IF
      */(‘åã)•‰’SŠz‚ðØ‚èã‚°‚éƒo[ƒWƒ‡ƒ“‚É‘Î‰žªªª/20221130
008770*
008780                WHEN OTHER
008790*/ƒGƒ‰[•\Ž¦‚ÌC³
008800                    PERFORM ƒRƒ“ƒ\[ƒ‹ƒGƒ‰[Šm”F
008810                    DISPLAY "•‰’S‹àŽZ’è‹æ•ª‚ªŽw’è‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ"
008820                            " ŽófŽÒ‡‚="   Žó|Š³ŽÒƒR[ƒh
008830                            " Ž{p”NŒŽ="   Žó|Ž{p”N Žó|Ž{pŒŽ UPON CONS
008840                    PERFORM ƒtƒ@ƒCƒ‹•Â½
008850                    MOVE ZERO TO PROGRAM-STATUS
008860                    EXIT PROGRAM
008870                END-EVALUATE
008880            END-IF
008890        END-IF
008900     END-IF.
HILO  *     DISPLAY "410-1 " ‚g˜AŒvŽZ|Ž{p˜a—ï”NŒŽ“ú " " ‚g˜AŒvŽZ|•‰’SŠz " " ‚g˜AŒvŽZ|•‰’SŠz•¬
HILO  *             " " ‚g˜AŒvŽZ|¿‹Šz•¬
HILO  *             "         " ˜A|•‰’SŠz•¬‚P‚f‚v  " " ˜A|•‰’SŠz•¬‚P‚O‚f‚v " " ˜A|•‰’SŠz•¬‚P‚f‚v‚Q " " ‚g˜AŒvŽZ|•‰’S‹àŒvŽZ‹æ•ª.
008910*
015568*================================================================*







014820*================================================================*
014830 ‚g“úŒvƒf[ƒ^‚e“Çž SECTION.
014840*
014850     READ ‚g“úŒvƒf[ƒ^‚e NEXT
014860     AT END
014870         MOVE "YES" TO I—¹ƒtƒ‰ƒO‚R
014880     END-READ.
014890*================================================================*
014361*================================================================*
014362 ŽófŽÒî•ñ‚e“Çž SECTION.
014363*
014364     READ ‚d‚wŽófŽÒî•ñ‚e NEXT
014370     AT END
014380         MOVE "YES" TO I—¹ƒtƒ‰ƒO‚R
014390     END-READ.
014400*================================================================*
013591*================================================================*
013592 Ž}”Ô‹àŠzŽæ“¾ SECTION.
013593*
013594* •ÛŒ¯‚ª•ÏX‚É‚È‚Á‚½“™‚Ìê‡A“–ŒŽ‚Ì’†‚Å“¯Š³ŽÒ”Ô†‚ÌŽÀ“ú”‚àƒJƒEƒ“ƒg‚·‚é
013595*

005580     OPEN INPUT ‚d‚wŽófŽÒî•ñ‚e.
005590         MOVE NC"‚dŽó" TO ƒtƒ@ƒCƒ‹–¼‚v.
005600         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.
005580     OPEN INPUT ‚g“úŒvƒf[ƒ^‚e.
005590         MOVE NC"‚g“úŒv" TO ƒtƒ@ƒCƒ‹–¼‚v.
005600         PERFORM ƒI[ƒvƒ“ƒ`ƒFƒbƒN.

013600* Žw’èŒŽ‚ÅŽ}”Ô‚ª‘¶Ý‚·‚é‚©‚Ì”»’è
013601*     MOVE ˜AŒv|Š³ŽÒ”Ô† TO Žó|Š³ŽÒ”Ô†.
013602*     MOVE ˜AŒv|Ž}”Ô     TO Žó|Ž}”Ô.
013610*     MOVE ˜AŒv|Ž{p˜a—ï TO Žó|Ž{p˜a—ï.
013620*     MOVE ˜AŒv|Ž{p”N   TO Žó|Ž{p”N.
013630*     MOVE ˜AŒv|Ž{pŒŽ   TO Žó|Ž{pŒŽ.
001700     MOVE ‚g˜AŒvŽZ|Ž{p˜a—ï”NŒŽ TO ‚dŽó|Ž{p˜a—ï”NŒŽ.
001760     MOVE ‚g˜AŒvŽZ|Š³ŽÒƒR[ƒh   TO ‚dŽó|Š³ŽÒƒR[ƒh
013670     START ‚d‚wŽófŽÒî•ñ‚e KEY IS >= ‚dŽó|Ž{p˜a—ï”NŒŽ ‚dŽó|Š³ŽÒƒR[ƒh
013680     END-START.
013690     IF ó‘ÔƒL[ = "00"
013700         MOVE SPACE TO I—¹ƒtƒ‰ƒO‚R
013710         MOVE SPACE TO Ž}”Ôƒtƒ‰ƒO
013720         MOVE ‚g˜AŒvŽZ|Š³ŽÒ”Ô†     TO Š³ŽÒ”Ô†‚v
013740         MOVE ‚g˜AŒvŽZ|Ž{p˜a—ï”NŒŽ TO Ž{p˜a—ï”NŒŽ‚v
013750         MOVE Žó|•¬Ží•Ê           TO •¬Ží•Ê‚v‚R
013760         MOVE Žó|”ï—p•‰’SŽÒ”Ô†•¬ TO ”ï—p•‰’SŽÒ”Ô†•¬‚v‚R
013780         PERFORM ŽófŽÒî•ñ‚e“Çž
013790         PERFORM UNTIL ( I—¹ƒtƒ‰ƒO‚R   NOT = SPACE        ) OR 
013800                       ( Š³ŽÒ”Ô†‚v     NOT = ‚dŽó|Š³ŽÒ”Ô† ) OR
013820                       ( Ž{p˜a—ï”NŒŽ‚v NOT = ‚dŽó|Ž{p˜a—ï”NŒŽ)
013830             IF (‚dŽó|Ž}”Ô           NOT = SPACE       ) AND
013840                (‚dŽó|Œp‘±‹æ•ª           = ZERO        ) AND
013860                (‚dŽó|•¬Ží•Ê           = •¬Ží•Ê‚v‚R) AND
013870                (‚dŽó|”ï—p•‰’SŽÒ”Ô†•¬ = ”ï—p•‰’SŽÒ”Ô†•¬‚v‚R)
013890                 MOVE "YES" TO Ž}”Ôƒtƒ‰ƒO
013900                 MOVE "YES" TO I—¹ƒtƒ‰ƒO‚R
013910             END-IF
013920             PERFORM ŽófŽÒî•ñ‚e“Çž
013930         END-PERFORM
013940     END-IF.
013950*
013960     MOVE ZERO  TO ŽÀ“ú”‚v.
           MOVE ZERO  TO ‘O—ÝŒv‹àŠz‚v.
013970     IF Ž}”Ôƒtƒ‰ƒO NOT = SPACE
013980         PERFORM ‘OŽÀ“ú”Žæ“¾
013990*         IF ŽÀ“ú”‚v <= •¬•‰’S‰ñ”‚v
014000*             PERFORM ŽÀ“ú”Žæ“¾
014010*         END-IF
014020*     ELSE
014030*         PERFORM ŽÀ“ú”Žæ“¾
014040     END-IF.
014050*

005580     CLOSE  ‚d‚wŽófŽÒî•ñ‚e ‚g“úŒvƒf[ƒ^‚e.

014060*================================================================*
014410*================================================================*
014420 ‘OŽÀ“ú”Žæ“¾ SECTION.
014430*
014440     PERFORM ‰f—Ã“úŽæ“¾.
014450*
014460     MOVE ‚g˜AŒvŽZ|Ž{p˜a—ï  TO  ‚g“ú|Ž{p˜a—ï Ž{p˜a—ï‚b‚v.
014470     MOVE ‚g˜AŒvŽZ|Ž{p”N    TO  ‚g“ú|Ž{p”N   Ž{p”N‚b‚v.
014480     MOVE ‚g˜AŒvŽZ|Ž{pŒŽ    TO  ‚g“ú|Ž{pŒŽ   Ž{pŒŽ‚b‚v.  
014490     MOVE 1                   TO  ‚g“ú|Ž{p“ú.
014500     MOVE ‚g˜AŒvŽZ|Š³ŽÒ”Ô†  TO  ‚g“ú|Š³ŽÒ”Ô† Š³ŽÒ”Ô†‚b‚v.
014510     MOVE SPACE               TO  ‚g“ú|Ž}”Ô.
014520*
014530     MOVE ‚g˜AŒvŽZ|Ž}”Ô      TO  Ž}”Ô‚b‚v.
           MOVE ‚g˜AŒvŽZ|Ž{p‹æ•ª  TO  ‚g“ú|Ž{p‹æ•ª.
014540*
014550     START ‚g“úŒvƒf[ƒ^‚e KEY IS >= ‚g“ú|Ž{p‹æ•ª ‚g“ú|Š³ŽÒƒR[ƒh ‚g“ú|Ž{p˜a—ï”NŒŽ“ú
014560     END-START.
014570*
014580     IF ó‘ÔƒL[ = "00"
014590         MOVE SPACE TO I—¹ƒtƒ‰ƒO‚R
014600         PERFORM ‚g“úŒvƒf[ƒ^‚e“Çž
014610         PERFORM UNTIL ( ‚g“ú|Š³ŽÒ”Ô† NOT = Š³ŽÒ”Ô†‚b‚v ) OR
014640                       ( I—¹ƒtƒ‰ƒO‚R   NOT = SPACE        )
014650             PERFORM ˜VlŽí•ÊŽæ“¾‚Q
014660             IF ( ‚g“ú|Š³ŽÒƒR[ƒh NOT = Š³ŽÒƒR[ƒh‚b‚v ) AND
014670                ( ‚g“ú|Š³ŽÒ”Ô†       = Š³ŽÒ”Ô†‚b‚v   ) AND
014680                ( ‚g“ú|Ž{p˜a—ï”NŒŽ“ú < ‰f—Ã”NŒŽ“ú‚b‚v ) AND
014700                ( ‚g“ú|Ž{p˜a—ï”NŒŽ  = Ž{p˜a—ï”NŒŽ‚b‚v ) AND
014710                ( ( Œö”ïŽí•Ê‚v = Œö”ïŽí•Ê‚v‚Q ) OR 
014720                  ( •¬Ží•Ê‚v = •¬Ží•Ê‚v‚Q )         )
      */Ž}”Ô‚ ‚è‚Ì”»’è‚©‚çAŽ©”ïxE¶•ÛhEŽ©”…ÓaE˜JÐr‚ðœ‚­/160709
                   AND (‚g“ú|Ž}”Ô NOT = "x" AND "h" AND "a" AND "r")
014730                 COMPUTE ŽÀ“ú”‚v = ŽÀ“ú”‚v + 1
                       COMPUTE ‘O—ÝŒv‹àŠz‚v = ‘O—ÝŒv‹àŠz‚v + ‚g“ú|ˆê•”•‰’S‹à
014740             END-IF
014750             PERFORM ‚g“úŒvƒf[ƒ^‚e“Çž
014760         END-PERFORM
014770     END-IF.
014780*
HILO  *     DISPLAY "KHT-3 ŽÀ“ú”‚v=" ŽÀ“ú”‚v  " ‘O—ÝŒv‹àŠz‚v=" ‘O—ÝŒv‹àŠz‚v. 
014820*================================================================*
014900*================================================================*
014910 ‰f—Ã“úŽæ“¾ SECTION.
014920*
014930* ‚QD“¯ŒŽE“¯Š³ŽÒ”Ô†E˜Vl•ÛŒ¯‚Ìê‡ie•ÛŒ¯‚Ì•ÏX“™j
014940* @@‚ÅŠ³ŽÒƒR[ƒh‚ð•ÏX‚µ‚½ê‡A‚»‚Ì•ª‚ÌŽÀ“ú”‚àƒJƒEƒ“ƒg‚·‚éB
014950*
014960     PERFORM ˜VlŽí•ÊŽæ“¾.
014970     MOVE ‚g˜AŒvŽZ|Ž{p˜a—ï  TO  ‚g“ú|Ž{p˜a—ï Ž{p˜a—ï‚b‚v.
014980     MOVE ‚g˜AŒvŽZ|Ž{p”N    TO  ‚g“ú|Ž{p”N   Ž{p”N‚b‚v.  
014990     MOVE ‚g˜AŒvŽZ|Ž{pŒŽ    TO  ‚g“ú|Ž{pŒŽ   Ž{pŒŽ‚b‚v.  
015000     MOVE 1                   TO  ‚g“ú|Ž{p“ú.
015010     MOVE ‚g˜AŒvŽZ|Š³ŽÒ”Ô†  TO  ‚g“ú|Š³ŽÒ”Ô† Š³ŽÒ”Ô†‚b‚v.
015020     MOVE ‚g˜AŒvŽZ|Ž}”Ô      TO  ‚g“ú|Ž}”Ô     Ž}”Ô‚b‚v.
           MOVE ‚g˜AŒvŽZ|Ž{p‹æ•ª  TO  ‚g“ú|Ž{p‹æ•ª.
015030*
014550     START ‚g“úŒvƒf[ƒ^‚e KEY IS >= ‚g“ú|Ž{p‹æ•ª ‚g“ú|Š³ŽÒƒR[ƒh ‚g“ú|Ž{p˜a—ï”NŒŽ“ú
015050     END-START.
015060*
015070     IF ó‘ÔƒL[ = "00"
015080         PERFORM ‚g“úŒvƒf[ƒ^‚e“Çž
015090         MOVE ‚g“ú|Ž{p˜a—ï”NŒŽ“ú TO ‰f—Ã”NŒŽ“ú‚b‚v
015100     END-IF.
015110*
015120*================================================================*
015120*================================================================*
015130 ˜VlŽí•ÊŽæ“¾ SECTION.
015140*
015150     MOVE ‚g˜AŒvŽZ|Ž{p˜a—ï   TO ‚dŽó|Ž{p˜a—ï.
015160     MOVE ‚g˜AŒvŽZ|Ž{p”N     TO ‚dŽó|Ž{p”N.
015170     MOVE ‚g˜AŒvŽZ|Ž{pŒŽ     TO ‚dŽó|Ž{pŒŽ.
015180     MOVE ‚g˜AŒvŽZ|Š³ŽÒ”Ô†   TO ‚dŽó|Š³ŽÒ”Ô†.
015190     MOVE ‚g˜AŒvŽZ|Ž}”Ô       TO ‚dŽó|Ž}”Ô.
015200     READ ‚d‚wŽófŽÒî•ñ‚e
015210     INVALID KEY
015220         MOVE NC"Žóf" TO ƒtƒ@ƒCƒ‹–¼‚v
015230     NOT INVALID KEY
015240         MOVE ‚dŽó|Œö”ïŽí•Ê TO Œö”ïŽí•Ê‚v
015250         MOVE ‚dŽó|•¬Ží•Ê TO •¬Ží•Ê‚v
015260     END-READ.
015270*
015280*================================================================*
015290 ˜VlŽí•ÊŽæ“¾‚Q SECTION.
015300*
015310     MOVE ‚g“ú|Ž{p˜a—ï   TO ‚dŽó|Ž{p˜a—ï.
015320     MOVE ‚g“ú|Ž{p”N     TO ‚dŽó|Ž{p”N.
015330     MOVE ‚g“ú|Ž{pŒŽ     TO ‚dŽó|Ž{pŒŽ.
015340     MOVE ‚g“ú|Š³ŽÒ”Ô†   TO ‚dŽó|Š³ŽÒ”Ô†.
015350     MOVE ‚g“ú|Ž}”Ô       TO ‚dŽó|Ž}”Ô.
015360     READ ‚d‚wŽófŽÒî•ñ‚e
015370     INVALID KEY
015380         MOVE NC"Žóf" TO ƒtƒ@ƒCƒ‹–¼‚v
015390     NOT INVALID KEY
015400         MOVE ‚dŽó|Œö”ïŽí•Ê TO Œö”ïŽí•Ê‚v‚Q
015410         MOVE ‚dŽó|•¬Ží•Ê TO •¬Ží•Ê‚v‚Q
015420     END-READ.
015430*
015440*================================================================*
015569******************************************************************
015570 END PROGRAM KHT41410.
015571******************************************************************
