000010*****************************************************************
000020* JÐîñe@ROUSAIJ.DAT mqk1024BYTEn     *
000030*****************************************************************
000080* SYSYTEM NAME:¤Ê                                             *
000081* LANGUAGE PG :NetCOBOL                                         *
000082* PRODUCT DATE:2009.07.08                                       *
000083* PURPOSE     :{³ÒÌJÐîñðL^                     *
000140*****************************************************************
000150*---------------------------------------------------------------*
000160*	VKt@C
000170*---------------------------------------------------------------*
000240**
000241*     SELECT  JÐîñe      ASSIGN      TO        ROUSAIJL
000242*                             ORGANIZATION             IS INDEXED
000243*                             ACCESS MODE              IS DYNAMIC
000244*                             RECORD KEY               IS JÐ|{païN
000245*                                                         JÐ|³ÒR[h
000255*                             ALTERNATE RECORD KEY     IS JÐ|³ÒR[h
000265*                                                         JÐ|{païN
000277*                             FILE STATUS              IS óÔL[
000278*                             LOCK        MODE         IS AUTOMATIC.
000279***
000280* FD  JÐîñe        BLOCK   CONTAINS   1   RECORDS.
000281*     COPY ROUSAIJ      OF  XFDLIB  JOINING   JÐ   AS  PREFIX.
000282*
000283*****************************************************************
000284 01 R[h.
000285   03 R[hL[.
000286     05 {païN.
000287       07 {paï                     PIC 9.
000290       07 {pN.
000300         09 {pN                     PIC 9(2).
000310         09 {p                     PIC 9(2).
000320     05 ³ÒR[h.
000330       07 ³ÒÔ                     PIC 9(6).
000340       07 }Ô                         PIC X.
000350   03 R[hf[^.
000493     05 JÐíÊ                       PIC 9(2).
000494     05 J­Û¯Ô                   PIC X(20).
000495     05 JÐÆ¼Ì                 PIC X(50).
000496     05 JÐÆXÖÔ.
000497       07 JÐÆXÖÔP         PIC X(3).
000498       07 JÐÆXÖÔQ         PIC X(4).
000499     05 JÐÆZP               PIC X(50).
000500     05 JÐÆZQ               PIC X(50).
000502     05 JÐÆdbÔ             PIC X(40).
000503     05 JÐÆå¼                 PIC X(80).
000504     05 JÐõl                       PIC X(400).
000511*
002278     05 FILLER                         PIC X(313).
002280*
