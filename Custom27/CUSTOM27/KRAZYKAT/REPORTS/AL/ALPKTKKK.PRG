*:***************************************************************************
*: Program file  : ALPKTKKK.PRG
*: Program desc. : PRINT PICKING TICKETS.
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : ALLOCATION (AL)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : Krazy Kat
*:***************************************************************************
*: C101648,1 KHM 10/13/1999
*B804151,1 HBG 20/09/2001 Fix bug of not printing order short notes in case of 
*B804151,1                print order notpad = No
*B804071,1 BWA 11/01/2001 Fix the bug of shipvia print "ALL".
*B605526,1 BWA 02/17/2002 1) Sort the printing with the Piktkt then Style.
*B605526,1 BWA            2) Fix the bug of print empty page in case there is no records
*B606058,1 BWA 06/09/2002 Add a filter in the grid to let the user select the data print sort by style or line no.
*B606389,1 BWA 08/20/2002 Printing the Pack_Id instead of the SKU in case
*B606389,1                of not empty Pack_Id, otherwise print the SKU
*B123242,1 BWA 06/16/2004 Fix the bug of Hanging the program when select to notepad.
*B608369,1 NNA 11/30/2007 (T20071120.0006) Fix bug that pick ticket form dosen't sort by line even you select to sort by it
*B608369,1 NNA            it just sort by style.
*B609937,1 SAB 05/22/2012 Fix bug of ShipVia in PickTick custom form for krazy kat [T20120516.0005]
*B610947,1 MMT 02/15/2015 Increase Custpo Field to be 15 chars[T20150211.0035]
*:**************************************************************************

*-- Initializing the necessary variables.
lnNotLine = 1
NewDoc    = .T.
ROW       = 20
STORE .F. TO llNoRec                && Variable used to make the endreport function in the main program.
STORE 0 TO lnLineNo,lnTotPik
STORE "" TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,;
            lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3
lcNonMajTl = ''
lcNonMajPi = ''
STORE 0 TO lnFreeLen , lnColorLen , lnMajSeg,lnNonMajSt
lnMajLen = LEN(gfItemMask('PM'))
=lfGetColor()

*-- Opening the SalesRep file to get the name of the salesrep.
= gfOpenFile (gcDataDir+'SALESREP','SALESREP','SH')

*-- Saving the index of the Spck_lin
SELECT Spck_Lin
lcSpckTag = SET('ORDER')
SET ORDER TO SPCKLINS

*B605526,1 BWA 02/17/2002 Fix the bug of print empty page in case there is no records.[START]
*--If EOF it will return to the main program
IF EOF(lcTmpOrdL)
  llNoRec = .T.
  *-- Message <No records to display.>
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
*B605526,1 BWA 02/17/2002.[END]

SET DEVICE TO PRINT

SELECT (lcTmpOrdL)

*B605526,1 BWA 02/17/2002 Sort the printing with the Piktkt then Style.[START]
*B606058,1 BWA 06/09/2002 Add a filter in the grid to let the user select the data print sort by style or line no.[START]
*INDEX ON PikTkt + Order + cGrupDetal + STYLE TAG (lcTmpOrdL)
IF lcRpStyLin == "S"
  INDEX ON PikTkt + Order + cGrupDetal + STYLE TAG (lcTmpOrdL)

*B608369,1 NNA 11/30/2007 (Begin) if "cRpStyLin" <>'S' the form keeps the original sort done by the standard program
*B608369,1 NNA            the refer to sort by style as a default
ELSE
  INDEX ON PikTkt + Order + cGrupDetal + STR(LineNo , 6) TAG (lcTmpOrdL)
*B608369,1 NNA (End)

ENDIF
*B606058,1 BWA 06/09/2002.[END]
*B605526,1 BWA 02/17/2002.[END]

LOCATE

*B123242,1 BWA 06/16/2004 Use scan.[START]
*DO WHILE !EOF() AND INKEY() <> 32
SCAN
*B123242,1 BWA 06/16/2004.[END]

  WAIT WINDOW 'Printing Pick Ticket ...' + PIKTKT NOWAIT

  *--B804071,1 new variable to print all the lines.[START]
  llReturnKK = .F.
  *--B804071,1 new variable to print all the lines.[END]

  IF NewDoc
    SELECT (lcTmpOrdL)
    lcPikTkt  = PIKTKT
    lcOrder   = ORDER
    lcStore   = PikTkt.STORE
    lcAccount = PikTkt.ACCOUNT
    lcCustPO  = PikTkt.CustPO
    lnValue   = 0
    lnPieces  = 0

    *--Get the Bill To adddess.
    DIMENSION laAddress[6,3]
    lcBtName  = CUSTOMER->BTNAME
    =gfGetAdr('Customer','','','',1,'2')
    lcBtAddr1 = laAddress[1,2]
    lcBtAddr2 = laAddress[2,2]
    lcBtAddr3 = ALLTRIM(laAddress[3,2]) + ' ' +ALLTRIM(laAddress[4,2])+ ' '+;
               ALLTRIM(laAddress[5,2])
    IF EMPTY(lcBtAddr2)
      lcBtAddr2 = lcBtAddr3
      lcBtAddr3 = ''
    ENDIF
    
    *-- Get the Ship To address.
    IF OrdHdr.Alt_ShpTo
      *hbg Select the file to fix bug of variable not found [Begin]
      SELECT ORDHDR 
      *hbg [End]
      lcStName  = STNAME
      lcStAddr1 = cAddress1
      lcStAddr2 = cAddress2
      lcStAddr3 = TRIM(cAddress3) + ' ' +TRIM(cAddress4) + ' ' + cAddress5
      IF EMPTY(lcStAddr2)
        lcStAddr2 = lcStAddr3
        lcStAddr3 = ''
      ENDIF
    ELSE
      DIMENSION laAddress[6,3]
      SELECT Customer
      =gfGetAdr('CUSTOMER','','','',1,'')
      lcStName  = CUSTOMER.StName
      lcStAddr1 = laAddress[1,2]
      lcStAddr2 = laAddress[2,2]
      lcStAddr3 = ALLTRIM(laAddress[3,2]) + ' ' +ALLTRIM(laAddress[4,2]) +;
      ' ' + ALLTRIM(laAddress[5,2])
      IF EMPTY(lcStAddr2)
        lcStAddr2 = lcStAddr3
        lcStAddr3 = ''
      ENDIF
    ENDIF

    *-- GET DESCRIPTIONS FOR CODED FIELDS
    *-- Terms
    XTERM_DATA = ALLTRIM(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'))
    
    *-- ShipVia
    XSHIP_DATA = ALLTRIM(gfCodDes(OrdHdr.ShipVia,'SHIPVIA   '))
    
    *-- Customer alternative shipvia.
    lcAltShip = ALLTRIM(gfCodDes(Customer.cAltShpVia,'SHIPVIA   '))
    
    *-- Special Instruction.
    lcSpcInst = gfCodDes(OrdHdr.SpcInst , 'SPCINST')

    lnBrkWght = Customer.nBrkWeight        
  ENDIF
  NEWDOC = .T.
  ROW    = 20   
  *-- Printing the pick ticket header.
  SELECT (lcTmpOrdL)
  DO lpHrdLabls

  *-- Printing the pick ticket details.
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6)=lcPikTkt+lcOrder+'D'
    DO CASE
      CASE TOTPIK <= 0
        LOOP
      CASE ROW >= 43
        NEWDOC = .F.
        EXIT
    ENDCASE
    
    lnLineNo = lnLineNo + 1
    lnNum = 36
    FOR lnCount = 1 to Scale.Cnt
      lcCount = STR(lnCount,1)
      IF PIK&lcCount > 0
        @ ROW,lnNum SAY PADL(ALLTRIM(SCALE.SZ&lcCount),5," ")
        lnNum = lnNum + 6
      ENDIF
    ENDFOR
	ROW = ROW + 1
    IF ROW >= 43
      DO lpHrdLabls
      ROW = 20
    ENDIF
    
    @ ROW,01 SAY SUBSTR(Style,1,lnMajLen)
    lcColor = SUBSTR(Style,lnNonMajSt,lnColorLen)
    *@ ROW,15 SAY IIF(SEEK('C'+COLOR,'CODE'),CODE.Cdata,SPACE(15))   
    @ ROW,15 SAY SUBSTR(gfCodDes(lcColor,'COLOR     '),1,15)    
    @ ROW,30 SAY TotPik PICTURE '99999'   
    @ ROW,37 SAY SUBSTR(Style.Desc,1,19)
     
    ROW = ROW + 1
    IF ROW >= 43
      DO lpHrdLabls
      ROW = 20
    ENDIF

    *B606389,1 BWA 08/20/2002 Check if the pack id of the style/color
    *B606389,1                is not empty then print it, otherwise print 
    *B606389,1                the sku.[START]
    IF EMPTY(Pack_Id)
    *B606389,1 BWA 08/20/2002.[END]

      lcAccStyCr = lcAccount+Style
      IF SEEK ('S'+lcAccStyCr,'Spck_Lin')
        SELECT Spck_Lin
        FOR lnCountr = 1 TO Scale.Cnt
          lcCountr = STR(lnCountr,1)
          =SEEK ('S'+lcAccStyCr,'Spck_Lin')
          IF EVALUATE(lcTmpOrdL+'.Pik'+lcCountr) > 0
            LOCATE REST WHILE Type+Account+Style = 'S'+lcAccStyCr;
                        FOR EVALUATE('Qty'+lcCountr) = 1
            IF FOUND()
              @ ROW,01 SAY Pack_Id
              lcPack_Id = Pack_Id
              EXIT
            ENDIF
          ENDIF
        ENDFOR
      ENDIF

    *B606389,1 BWA 08/20/2002 Print the Pack_Id.[START]
    ELSE
      @ ROW,01 SAY Pack_Id
    ENDIF
    *B606389,1 BWA 08/20/2002.[END]

    SELECT (lcTmpOrdL)
        
    lnNum = 36
    FOR lnCount =1 to SCALE.CNT
      lcCount = STR(lnCount,1)
      IF PIK&lcCount > 0
        @ ROW,lnNum SAY PIK&lcCount
        lnNum = lnNum + 6 
      ENDIF
    ENDFOR

    *-- Printing the rest of the Style's Skus for none 0 picked qty
    ROW = ROW + 1

    *B606389,1 BWA 08/20/2002 Print the rest of the sku in case of
    *B606389,1                empty of pack id of the printed style\color.[START]
    IF EMPTY(Pack_id)
    *B606389,1 BWA 08/20/2002.[END]

      IF SEEK ('S'+lcAccStyCr,'Spck_Lin')
        SELECT Spck_Lin
        FOR lnCountr = 1 TO Scale.Cnt
          IF ROW >= 44

            *B605526,1 BWA 02/17/2002 Add the messing message.[START]
            @ 47,15 SAY '*** CONTINUED NEXT PAGE ***'
            @ 50,51 SAY '***'
            @ 53,02 SAY ALLTRIM(OrdHdr.Note1)+' '+OrdHdr.Note2
            @ 55,02 SAY "SPECIAL INSTRUCTION: **** " +ALLTRIM(lcSpcInst)+ " ****"
            *B605526,1 BWA 02/17/2002.[END]

            DO lpHrdLabls
            ROW = 20
          ENDIF
          lcCountr = STR(lnCountr,1)
          IF EVALUATE(lcTmpOrdL+'.Pik'+lcCountr) > 0
            =SEEK ('S'+lcAccStyCr,'Spck_Lin')
            LOCATE REST WHILE Type+Account+Style = 'S'+lcAccStyCr;
                        FOR EVALUATE('Qty'+lcCountr) = 1
            IF FOUND() AND Pack_Id <> lcPack_Id
              @ ROW,01 SAY Pack_Id
              ROW = ROW + 1
            ENDIF  
          ENDIF
        ENDFOR
      ENDIF

    *B606389,1 BWA 08/20/2002.[START]
    ENDIF
    *B606389,1 BWA 08/20/2002.[END]

    SELECT (lcTmpOrdL)
    *-- Accumolate Pick Ticket's Totals. 
    lnTotPik = lnTotPik + TotPik

    SELECT &lcTmpOrdL
    ROW = ROW + 1
  ENDSCAN

  *B123242,1 BWA 06/16/2004 Add this line due to useinf 2 scan with each other.[START]
  IF (lcOrder # &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) ) OR (lcOrder = &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) AND lcPikTkt # &lcTmpOrdL..PIKTKT )
    SKIP - 1
  ENDIF
  *B123242,1 BWA 06/16/2004.[END]

  IF NewDoc
    *-- To print the notepad.
    IF llRpOrdNot

      *B123242,1 BWA 06/16/2004 Use the variable hold the order.[START]
      *IF SEEK('B' + OrdHdr->Order,'NotePad')
      IF SEEK('B' + lcOrder ,'NotePad')
      *B123242,1 BWA 06/16/2004.[END]

        SELECT NotePad
        lnOldMemW = SET("MEMOWIDTH")
        SET MEMOWIDTH TO 75
        lnMemLins = MEMLINES(NOTEPAD.MNOTES)

        @ Row,02 SAY '* -- N O T E S -- *' 
        Row = Row + 1
        DO WHILE lnNotLine <= lnMemLins
          IF Row >= 44
            @ 47,15 SAY '*** CONTINUED NEXT PAGE ***'
            @ 50,51 SAY '***'
            @ 53,02 SAY ALLTRIM(OrdHdr.Note1)+' '+OrdHdr.Note2
            @ 55,02 SAY "SPECIAL INSTRUCTION: **** " +ALLTRIM(lcSpcInst)+ " ****"
            DO lpHrdLabls
            Row = 23
          ENDIF
          @ ROW,02 SAY MLINE(MNOTES,lnNotLine)
          ROW = ROW + 1
          lnNotLine = lnNotLine + 1
        ENDDO
        SET MEMOWIDTH TO lnOldMemW        
      ENDIF
    ENDIF      

*    *B804151,1 HBG 20/09/2001 Get the correct line to print order notes [Begin]
*    IF !llRpOrdNot AND !BOF()
*      SKIP -1
*      *--B804071,1 variable to print all the lines.[START]
*      llReturnKK = .T.
*      *--B804071,1 variable to print all the lines.[START]
*    ENDIF
*    *B804151,1 [End]

    lnNotLine = 1    
    DO lpNewFoter
  ELSE
    @ 45,15 SAY '*** CONTINUED NEXT PAGE ***'  &&@ 47,15 SAY '*** CONTINUED NEXT PAGE ***'
    @ 48,51 SAY '***'                          &&@ 50,13 SAY '***'
    @ 50,02 SAY ALLTRIM(OrdHdr.Note1)+' '+OrdHdr.Note2
    @ 52,02 SAY "SPECIAL INSTRUCTION: **** " +ALLTRIM(lcSpcInst)+ " ****"
    ROW = ROW + 1

    *--bas
    IF (lcOrder = &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) ) OR (lcOrder = &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) AND lcPikTkt = &lcTmpOrdL..PIKTKT )
      SKIP - 1
    ENDIF
    *--bas

    LOOP
  ENDIF

  *B123242,1 BWA 06/16/2004 Comment this lines due to the printing is handling in the main program.[START]
  *SELE PIKTKT
  *SEEK &lcTmpOrdL..PIKTKT
  *REPL NEXT 1 PRTFLAG WITH 'P'
  *B123242,1 BWA 06/16/2004.[END]

  lnTotPik = 0
  SELECT (lcTmpOrdL)
 
*  *B804151,1 HBG 20/09/2001 Get the correct line in both cases Print order notpad 'Yes' and 'No' [Begin]
*  *IF !EOF() AND llRpOrdNot
*  IF !EOF() AND llReturnKK
*  *B804151,1 [End]  
*    SKIP
*    *--B804071,1 new variable to print all the lines.[START]
*    llReturnKK = .F.
*    *--B804071,1 new variable to print all the lines.[START]
*  ENDIF

*B123242,1 BWA 06/16/2004 Closing the scan.[START]
*ENDDO
ENDSCAN
*B123242,1 BWA 06/16/2004.[END]

*-- Restoring the old index of the spck_lin file
SELECT Spck_Lin
lcSpckTag = SET('ORDER')
SET ORDER TO &lcSpckTag

WAIT CLEAR
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lpHrdLabls
*! Developer : KHALID MOHI EL-DIN
*! Date      : 13/10/1999
*! Purpose   : Printing the P/T header
*!*************************************************************
*! Example            :  DO lpHrdLabls
*!*************************************************************
PROCEDURE lpHrdLabls
PRIVATE lnAlias, lcExp

lnAlias = SELECT()
SELECT Customer
lcExp   = Type+Account+Store

@ 02,17 SAY lcOrder
@ 04,72 SAY lcPikTkt
@ 06,15 SAY IIF(SEEK('M'+lcAccount,'CUSTOMER'),SUBSTR(CUSTOMER.NOTE,1,10),SPACE(10))
@ 06,72 SAY PIKTKT.DATE

@ 09,12 SAY lcBtName
@ 09,50 SAY lcStName

@ 10,12 SAY lcBtAddr1
@ 10,50 SAY lcStAddr1

IF EMPTY(lcBtAddr2)
  @ 11,12 SAY lcBtAddr3
ELSE
  @ 11,12 SAY lcBtAddr2  
ENDIF
    
IF EMPTY(lcStAddr2)
  @ 11,50 SAY lcStAddr3
ELSE
  @ 11,50 SAY lcStAddr2
ENDIF    

IF !EMPTY(lcBtAddr2)
  @ 12,12 SAY lcBtAddr3
ENDIF

IF !EMPTY(lcStAddr2)
  @ 12,50 SAY lcStAddr3
ENDIF
 
*B804071,1 BWA 11/01/2001 Fix the bug of shipvia print "ALL".[START]
*@ 15,32 SAY XSHIP_DATA
@ 15,32 SAY lfShipvia()
*B804071,1 BWA 11/01/2001.[END]

*B610947,1 MMT 02/15/2015 Increase Custpo Field to be 15 chars[T20150211.0035][Start]
*@ 16,02 SAY IIF(ORDHDR->MultiPO,SUBSTR(lcCustPO,1,10),SUBSTR(OrdHdr->CUSTPO,1,10))  &&@ 17,02 SAY IIF(ORDHDR->MultiPO,lcCustPO,OrdHdr->CUSTPO)
@ 16,02 SAY IIF(ORDHDR->MultiPO,SUBSTR(lcCustPO,1,15),SUBSTR(OrdHdr->CUSTPO,1,15))  &&@ 17,02 SAY IIF(ORDHDR->MultiPO,lcCustPO,OrdHdr->CUSTPO)
*B610947,1 MMT 02/15/2015 Increase Custpo Field to be 15 chars[T20150211.0035][End]
@ 16,20 SAY OrdHdr.Dept
@ 16,27 SAY 'ALT: '
@ 16,32 SAY lcAltShip
@ 16,50 SAY lnBrkWght
@ 16,66 SAY IIF(SEEK(ORDHDR->REP1,'SALESREP'),SUBSTR(SALESREP.NAME,1,18),SPACE(18))  &&@ 17,64 SAY IIF(SEEK(ORDHDR->REP1,'SALESREP'),SUBSTR(SALESREP.NAME,1,18),SPACE(18))

SELECT Customer
SEEK lcExp 
SELECT(lnAlias)

*!*************************************************************
*! Name      : lpNewFoter
*! Developer : KHALID MOHI EL-DIN
*! Date      : 10/13/1999
*! Purpose   : Printing the P/T footer
*!*************************************************************
*! Example            :  DO lpNewFoter
*!*************************************************************
PROCEDURE lpNewFoter

@ 48,51 SAY lnTotPik  PICTURE '999999'
@ 50,02 SAY ALLTRIM(OrdHdr.Note1)+' '+OrdHdr.Note2
@ 52,02 SAY "SPECIAL INSTRUCTION: **** " +ALLTRIM(lcSpcInst)+ " ****"
lnTotPik = 0
ROW = ROW + 1

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/13/1998
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

*B804071,1 BWA 11/01/2001 Fix the bug of shipvia print "ALL".[START]
*!*************************************************************
*! Name      : lfShipvia
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/01/2001
*! Purpose   : Function to get the address.
*!*************************************************************
*! Called from : ALPKTKTKK.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfShipvia
PRIVATE lcalias , lcOrder , lcShipVia

lcalias = SELECT(0)
lcShipVia = SPACE(0)
*B609937,1 SAB 05/22/2012 Fix bug of ShipVia in PickTick custom form for krazy kat [T20120516.0005][Start]
IF ORDHDR.SHIPVIA <> '*' 
  lcShipVia = ALLTRIM(gfCodDes(ORDHDR.SHIPVIA,'SHIPVIA   '))
ELSE
*B609937,1 SAB 05/22/2012 Fix bug of ShipVia in PickTick custom form for krazy kat [T20120516.0005][End]
  SELECT CUSTOMER
  lcOrder = EVAL(KEY())
  IF SEEK(IIF(EMPTY(PIKTKT.STORE) , 'M' + CUSTOMER.ACCOUNT , 'S' + CUSTOMER.ACCOUNT + PIKTKT.STORE ),'CUSTOMER')
    lcShipVia = ALLTRIM(gfCodDes(CUSTOMER.SHIPVIA,'SHIPVIA   '))
  ENDIF
  
  =SEEK(lcOrder , 'CUSTOMER')
*B609937,1 SAB 05/22/2012 Fix bug of ShipVia in PickTick custom form for krazy kat [T20120516.0005][Start]
ENDIF
*B609937,1 SAB 05/22/2012 Fix bug of ShipVia in PickTick custom form for krazy kat [T20120516.0005][End]  

SELECT(lcalias)
RETURN lcShipVia
*--End of lfShipvia.
*B804071,1 BWA 11/01/2001.[START]