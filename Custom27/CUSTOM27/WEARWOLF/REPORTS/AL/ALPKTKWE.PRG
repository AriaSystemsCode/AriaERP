*!********************************************************************
*: Program file  : ALPKTKWE.PRG
*: Program desc. : CUSTOMIZED P/T FORMAT FOR WEARWOLF GROUP LTD
*: For screen    : None.
*:         System: ARIA APPAREL SERIES
*:         Module: ALLOCATION (AL)
*:      Developer: KHALID MOHI EL-DIN . KHM
*!********************************************************************
*: C101501,1 KHM 04/11/99
*!********************************************************************
*!B803919,1 HBG 10/01/2001 Fix bug of variabels StAddr1,StAddr2,StCity not found
*!B804085,1 HBG 03/04/2001 Moving Store # and Total Pick 
*!B605671,1 BWA 03/10/2002 Fix the bug of the first line in the 2nd piktkt is not printing.
*!********************************************************************

*-- Initializing the necessary variables.
lnRow      = 0
lnCol      = 0
lcStore    = SPACE(8)
lcCustPo   = SPACE(10)
lcDept     = SPACE(5)
lcShipVia  = SPACE(15)
lnTotPik   = 0
lnTotOrd   = 0
STORE .F. To llNoRec,llChkFlag
NEWDOC     = .T.
STORE "" TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4

STORE 0 TO lnPage
lcPrnComp  = IIF(llPrntComp,'Y','N')
lcNonMajTl = ''
lcNonMajPi = ''
STORE 0 TO lnFreeLen , lnColorLen , lnMajSeg,lnNonMajSt
lnMajLen = LEN(gfItemMask('PM'))
=lfGetColor()

SET DEVICE TO PRINT

*-- This file holds all the lines of the selected pick tickets.
SELECT (lcTmpOrdL)
GOTO TOP
IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

DO WHILE !EOF() AND INKEY() <>32
  WAIT WINDOW 'PRINT PICK TICKETS - <Space Bar> TO ABORT' NOWAIT
  SELECT (lcTmpOrdL)
  IF NEWDOC
    lnPage = 1
    lcPikTkt   = PIKTKT
    lcOrder    = ORDER
    lcPikDate  = PikDate
    lcStore    = PikTkt.Store
    lcAccount  = PikTkt.Account
    lcCustPO   = PikTkt.CustPo
    lcDept     = OrdHdr.Dept
    *B803919,1 HBG 10/01/2001 Get the Ship TO Address from the correct Fields in 'OrdHdr'[Begin]
    IF OrdHdr.Alt_ShpTo
     * lcStName  = OrdHdr.StName
     * lcStAddr1 = OrdHdr.StAddr1
     * lcStAddr2 = OrdHdr.StAddr2
     * lcStAddr3 = TRIM(OrdHdr.StCity) + ' ' +TRIM(OrdHdr.StState) +;
     * ' ' + OrdHdr->StZip
     * IF EMPTY(lcStAddr2)
     *   lcStAddr2 = lcStAddr3
     *   lcStAddr3 = ''
     * ENDIF
      lcStName  = OrdHdr.StName
      lcStAddr1 = OrdHdr.cAddress1
      lcStAddr2 = OrdHdr.cAddress2
      lcStAddr3 = ALLTRIM(OrdHdr.cAddress3) + ' ' +ALLTRIM(OrdHdr.cAddress4) +;
      ' ' + ALLTRIM(OrdHdr->cAddress5)
      IF EMPTY(lcStAddr2)
        lcStAddr2 = lcStAddr3
        lcStAddr3 = ''
      ENDIF 
      *B803919,1 [End]
    ELSE
      DIMENSION laAddress[6,3]
      *-- ShipTo address
      SELECT Customer
      =gfGetAdr('CUSTOMER','','','',1,'')
      lcStName  = CUSTOMER.StName
      lcStAddr1 = laAddress[1,2]
      lcStAddr2 = laAddress[2,2]
      lcStAddr3 = TRIM(laAddress[3,2]) + ' ' +TRIM(laAddress[4,2]) +;
      ' ' + ALLTRIM(laAddress[5,2])
      IF EMPTY(lcStAddr2)
        lcStAddr2 = lcStAddr3
        lcStAddr3 = ''
      ENDIF
    ENDIF
    
    lcShipVia = ALLTRIM(gfCodDes(OrdHdr.ShipVia,'SHIPVIA   '))
  ENDIF
  NEWDOC  = .T.

  *-- Function to print the P/T header
  =lfPrntHdr()
  SELECT (lcTmpOrdL)  
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6)=lcPikTkt+lcOrder+'D'
     DO CASE
        CASE TOTPIK <= 0
          LOOP
        CASE lnRow >= 46
          lnPage = lnPage + 1
          NEWDOC = .F.
          EXIT
     ENDCASE
     @ lnRow,02 SAY ALLTRIM(STR(LineNo))
     @ lnRow,05 SAY SUBSTR(Style,1,7)
     IF SEEK('S'+lcAccount+Style,'Spck_Lin')
       @ lnRow,12 SAY SUBSTR(Spck_Lin.Pack_ID,1,8)
     ENDIF
     lcColor = SUBSTR(&lcTmpOrdL..Style,lnNonMajSt,lnColorLen)
     @ lnRow,23 SAY SUBSTR(lcColor,1,4)     
     @ lnRow,28 SAY SUBSTR(gfCodDes(lcColor,'COLOR     '),1,7)
     =SEEK('S'+Style.Scale,'Scale')
     lnCol = 38
     FOR lnCount = 1 TO 8
       lcCount = STR(lnCount,1)
       @ lnRow,lnCol SAY Scale.SZ&lcCount
       lnCol = lnCol + 6
     ENDFOR
     lnRow = lnRow + 1
     @ lnRow,05 SAY SCALE.cScl_Desc      
     lnCol = 37
     FOR lnCount = 1 TO 8
       lcCount = STR(lnCount,1)
       @ lnRow,lnCol SAY IIF(Pik&lcCount=0,'',STR(Pik&lcCount,6))
       lnCol = lnCol + 6
     ENDFOR
     @ lnRow,102 SAY ALLTRIM(STR(TotPik,7))
     lnRow = lnRow + 2
     lnTotPik = lnTotPik + TotPik
  ENDSCAN

  *B605671,1 BWA 03/10/2002 Fix the bug of the first line in the 2nd piktkt is not printing.[START]
  IF (lcOrder # &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL)) OR (lcOrder = &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) AND lcPikTkt # &lcTmpOrdL..PIKTKT )
    SKIP - 1
  ENDIF
  *B605671,1 BWA 03/10/2002.[END]

  IF !NEWDOC       
    =lfprntLab()
    LOOP
  ENDIF

  *-- Printing the order notepad 
  IF llRpOrdNot
    =lfPrnNotPd()
  ENDIF

  *-- Function to print the Footer
  =lfprntLab()    
  SELE PIKTKT

  *B605671,1 BWA 03/10/2002 Comment this lines it updated in the main program.[START]
  *SEEK &lcTmpOrdL..PIKTKT
  *REPL NEXT 1 PRTFLAG WITH 'P'
  *B605671,1 BWA 03/10/2002.[END]

  lnTotPik = 0
  lnTotOrd = 0
  SELECT (lcTmpOrdL)

  IF !EOF()
    SKIP
  ENDIF
ENDDO
WAIT CLEAR
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lfPrntHdr
*! Developer : KHALID MOHI EL-DIN
*! Date      : 04/11/1999
*! Purpose   : Printing the P/T header
*!*************************************************************
*! Example            :  lfPrntHdr()
*!*************************************************************
FUNCTION lfPrntHdr

@ 02,04 SAY lcStName
@ 03,04 SAY lcStAddr1

@ 03,72 SAY DATE()
@ 03,102 SAY lcPikTkt

@ 04,04 SAY lcStAddr2
@ 05,04 SAY PADR(lcStAddr3,30)

*B804085,1 HBG 03/04/2001 Moving Store # 4 char [Begin]
*@ 06,35 SAY lcCustPo
@ 06,35 SAY PADR(lcCustPo,11)
*B804085,1 [End]
@ 06,46 SAY RIGHT(ALLTRIM(lcStore),5)
@ 06,52 SAY lcDept
@ 06,61 SAY OrdHdr.Start
@ 06,72 SAY OrdHdr.Complete
@ 06,84 SAY lcAccount
@ 06,92 SAY lcOrder

=IIF(EMPTY(lcStore),SEEK('M'+lcAccount,'Customer'),SEEK('S'+lcAccount+lcStore,'Customer'))

@ 08,02 SAY IIF(EMPTY(lcStore),CUSTOMER.StName,CUSTOMER.STORE + ' ' + CUSTOMER.StName)
@ 08,57 SAY "Page # " +ALLTRIM(STR(lnPage,2))
@ 08,67 SAY lcShipVia

IF llRpOrdNot
  @ 09,02 SAY OrdHdr.Note1 
  @ 10,02 SAY OrdHdr.Note2
ENDIF

lnRow = 19


*!*************************************************************
*! Name      : lfprntLab
*! Developer : KHALID MOHI EL-DIN
*! Date      : 04/11/1999
*! Purpose   : Printing the P/T Footer
*!*************************************************************
*! Example            :  lfprntLab()
*!*************************************************************
FUNCTION lfprntLab

IF !NEWDOC
  @ 47,40 SAY "******CONTINUED NEXT PAGE******"
  @ 47,88 SAY "SUBTOTAL " + PADL(ALLTRIM(STR(lnTotPik,7)),7," ")
ELSE
  *B804085,1 HBG 03/04/2001 Moving Total Pick 3 char [Begin]
  *@ 47,97 SAY lnTotPik picture "@Z 9999999"
  @ 47,100 SAY lnTotPik picture "@Z 9999999"
  *B804085,1 [End]
ENDIF
@ 57,07 SAY lcStName    && First Lable
@ 57,43 SAY lcStName    && Second Lable
@ 57,80 SAY lcStName    && Third Lable

@ 58,07 SAY lcStAddr1   && First Lable
@ 58,43 SAY lcStAddr1   && Second Lable
@ 58,80 SAY lcStAddr1   && Third Lable

@ 59,07 SAY lcStAddr2   && First Lable
@ 59,43 SAY lcStAddr2   && Second Lable
@ 59,80 SAY lcStAddr2   && Third Lable


@ 60,07 SAY PADR(lcStAddr3,30)   && First Lable
@ 60,43 SAY PADR(lcStAddr3,30)   && Second Lable
@ 60,80 SAY PADR(lcStAddr3,30)   && Third Lable

*B804085,1 HBG 03/04/2001 Moving Store # 4 char [Begin]
*@ 64,03 SAY lcCustPo                   && First Lable
@ 64,03 SAY PADR(lcCustPo,11)           && First Lable
@ 64,14 SAY RIGHT(ALLTRIM(lcStore),5)  && First Lable
*@ 64,20 SAY lcDept                     && First Lable
@ 64,23 SAY lcDept                     && First Lable
*B804085,1 HBG 03/04/2001 Moving Store # 4 char [END]

*B804085,1 HBG 03/04/2001 Moving Store # 4 char [Begin]
*@ 64,41 SAY lcCustPo                 && Second Lable
@ 64,41 SAY PADR(lcCustPo,11)          && Second Lable
@ 64,52 SAY RIGHT(ALLTRIM(lcStore),5)  && Second Lable
*@ 64,58 SAY lcDept                     && Second Lable
@ 64,61 SAY lcDept                     && Second Lable
*B804085,1 [End]

*B804085,1 HBG 03/04/2001 Moving Store # 4 char [Begin]
*@ 64,78 SAY lcCustPo                   && Third Lable
@ 64,78 SAY PADR(lcCustPo,11)           && Third Lable
*@ 64,90 SAY RIGHT(ALLTRIM(lcStore),5)  && Third Lable
@ 64,89 SAY RIGHT(ALLTRIM(lcStore),5)  && Third Lable
*B804085,1 [End]
@ 64,96 SAY lcDept                     && Third Lable

*!*************************************************************
*! Name      : lfPrnNotPd
*! Developer : KHALID MOHI EL-DIN
*! Date      : 04/11/1999
*! Purpose   : Printing the Order notepad
*!*************************************************************
*! Example            :  lfPrnNotPd()
*!*************************************************************
FUNCTION lfPrnNotPd
PRIVATE lnAlias, lnOldMemW, lnMemLins, lnNotLine

lnAlias   = SELECT()
lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
SET MEMOWIDTH TO 66

SELECT NotePad
IF SEEK ('B' + lcOrder)
  lnMemLins = MEMLINES(NOTEPAD.MNOTES)
  DO WHILE lnNotLine <= lnMemLins
    
    IF lnRow >= 46
      lnPage =lnPage + 1
      NEWDOC = .F.
      =lfprntLab()
      =lfPrntHdr()
    ENDIF
    IF !EMPTY(MLINE(MNOTES,lnNotLine))
      @ lnRow,15 SAY MLINE(MNOTES,lnNotLine)
      lnRow       = lnRow + 1
    ENDIF
    lnNotLine = lnNotLine + 1
  ENDDO     
  NEWDOC = .T.
ENDIF
SET MEMOWIDTH TO lnOldMemW
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/09/1998
*! Purpose   : To get the length to of the color.
*!*************************************************************
*! Calls       : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

lcNonMajTl = ''
lcNonMajPi = ''
lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
 *-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen


