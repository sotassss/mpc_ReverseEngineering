000090*****************************************************************
000091* VXeXVðe@dmkirok.dat mRL=256BYTEn                *
000092*****************************************************************
000093* SYSYTEM NAME:¤Ê                                             *
000094* LANGUAGE PG :NetCOBOL                                         *
000095* PRODUCT DATE:2009.11.05                                       *
000097* PURPOSE     :DMoÍL^ðÇ                                 *
000098*****************************************************************
000099*---------------------------------------------------------------*
000100*	VKt@C
000101*---------------------------------------------------------------*
000102*
000103*     SELECT  clL^e          ASSIGN      TO        DMKIROKL
000104*                                 ORGANIZATION             IS  INDEXED
000105*                                 ACCESS MODE              IS  DYNAMIC
000106*                                 RECORD KEY               IS  cl|­saïNú
000107*                                                              cl|­s}Ô
000108*                                                              cl|³ÒR[h
000109*                                 ALTERNATE RECORD KEY     IS  cl|³ÒR[h
000110*                                                              cl|­saïNú
000111*                                                              cl|­s}Ô
000113*                                 FILE STATUS              IS  óÔL[
000114*                                 LOCK        MODE         IS  AUTOMATIC.
000115*
000116* FD  clL^e    BLOCK   CONTAINS   1   RECORDS.
000117*     COPY DMKIROK          OF  XFDLIB  JOINING   cl   AS  PREFIX.
000118*
000119*****************************************************************
000120*****************************************************************
000121*****************************************************************
000122 01 R[h.
000123   03 R[hL[.
000132     05 ­saïNú.
000133       07 ­saïN.
000134         09 ­saï                   PIC 9.
000135         09 ­sN.
000136           11 ­sN                   PIC 9(2).
000137           11 ­s                   PIC 9(2).
000138       07 ­sú                       PIC 9(2).
000139     05 ­s}Ô                       PIC 9(2).
000144     05 ³ÒR[h.
000145       07 ³ÒÔ                     PIC 9(6).
000146       07 }Ô                         PIC X.
000161   03 R[hf[^.
000162*     / 1:Íª«A2:[
000163     05 }Ìæª                       PIC 9.
000164*     / UMEISYOæè /
000166     05 oÍªÞ                       PIC 9(3).
000167     05 oÍªÞ¼Ì                   PIC X(50).
000168     05 ÂÊoÍªÞ                   PIC 9(3).
000169     05 ÂÊoÍªÞ¼Ì               PIC X(50).
000170*
000171*     / øÊ è1
000172     05 øÊ                           PIC X.
000173     05 øÊaïNú.
000174       07 øÊaïN.
000175         09 øÊaï                   PIC 9.
000176         09 øÊN.
000177           11 øÊN                   PIC 9(2).
000178           11 øÊ                   PIC 9(2).
000179       07 øÊú                       PIC 9(2).
000180*
000181      05 FILLER                        PIC X(125).
000182*
