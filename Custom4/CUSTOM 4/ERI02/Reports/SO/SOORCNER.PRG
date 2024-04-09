*:***************************************************************************
*: Program file  : SOORCNER
*: Program desc. : Custom Order confirmation Form for Eric Javits (ER)
*: For Report    : Order confirmation
*: System        : Aria 4XP
*: Module        : Sales Order (SO)
*: Developer     : Walid Hamed (WLD)
*: Ticket NO     : T20070502.0023
*: Track  NO     : C200796
*: Date          :  06/10/2007
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  :..
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOORCNER
*B608183 ,1 SSH Change Price picture to be 12,2
*B611449 ,1 HMS  Aria 4XP - margins needs to be adjusted (Eric Javits Inc)[T20171019.0009]
*:***************************************************************************

IF llOGFltCh
  llDonprnt=.F.

  lcRpExp=loogscroll.lcRpExp + IIF(lcRpEDIFlt='O',[ AND !ORDHDR.lEDIOrder],;
    IIF(lcRpEDIFlt='E',[ AND ORDHDR.lEDIOrder],""))

  *-- If No records selected.

  SELECT ORDHDR
  SET SKIP TO
  LOCATE FOR  &lcRpExp
  IF !FOUND()
    llDonprnt=.T.
    *--No records to display.
    = gfModalGen('TRM00052B00000','DIALOG' )
    RETURN
  ENDIF
  *C200796,1 WLD custom report of Eric Javits (ER)  06/10/2007 [Begin]
*!*	  IF FILE(oAriaApplication.DataDir+'SOORCNZ.MEM')
*!*	    RESTORE FROM (oAriaApplication.DataDir+'SOORCNZ.MEM') ADDITIVE
*!*	  ELSE
*!*	    lcDevice = SET('DEVICE')
*!*	    *--Would you like to print the report by Pack_Id ?
*!*	    lcMpack  = IIF(gfModalGen('TRM32063B32000','DIALOG' )=1,'Y','N')
*!*	  ENDIF
*!*	  llPack = (lcMpack = 'Y')
*!*	  SAVE ALL LIKE lcM* TO (oAriaApplication.DataDir+'SOORCNZ.MEM')
  IF FILE(oAriaApplication.DataDir+'SOORCNK.MEM')
    RESTORE FROM (oAriaApplication.DataDir+'SOORCNK.MEM') ADDITIVE
  ELSE
    lcDevice = SET('DEVICE')
    *--Would you like to print the report by Pack_Id ?
    lcMpack  = IIF(gfModalGen('TRM32063B32000','DIALOG' )=1,'Y','N')
  ENDIF
  llPack = (lcMpack = 'Y')
  SAVE ALL LIKE lcM* TO (oAriaApplication.DataDir+'SOORCNK.MEM')
  *C200796,1 WLD custom report of Eric Javits (ER)  06/10/2007 [End]
  RELEASE ALL LIKE lcM*
  *--Open needed files.
  =gfOpenFile(oAriaApplication.DataDir+'Spck_Lin',oAriaApplication.DataDir+'Spck_Lin','SH')
  =gfOpenFile(oAriaApplication.DataDir+'Spck_Hdr',oAriaApplication.DataDir+'Spck_Hdr','SH')

  *--Initilize variables
  DIMENSION laAddress[1]
  STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6,laAddress
  STORE '' TO lcOrder,lcAccount,lcMulti,lcStore,laAddress,lcTermData,lcShipVia,lcDivison
  STORE ' ' TO lcSpcInst,lcSeason,lcPrePack,XMER_DISC,lcScale,lcPrevScale
  STORE '' TO lcDcStore


  llNrmPrt = .T.
  DECLARE laDivLName[1,2]
  laDivLName[1,1] = 'CDIVISION'
  laDivLName[1,2] = 'lcDivLName'
  *-- Get the style and color
  lcMajTitl  = ALLTRIM(gfItemMask('HM'))
  lcNonMajTl = ''
  =lfGetColor()

  lcRpTmpMain=loOgScroll.gfTempName()
  lcRpTmpLines=loOgScroll.gfTempName()
  =lfBuildTmp()

  SELECT (lcRpTmpMain)
  INDEX ON ORDER+STORE TAG (lcRpTmpMain)

  SELECT (lcRpTmpLines)
  INDEX ON ORDER+STORE+cline TAG (lcRpTmpLines)



  STORE 1 TO lnNoteLn,lnSvNoteLn
  lcMulTOrDT = loOgScroll.gfTempName()

  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET DEVICE TO PRINT
  *------------------------------
  * SECTION: MAIN LOOP
  *------------------------------
  SELECT ORDHDR
  *--Refresh the relation.
  GO TOP

  SCAN REST WHILE INKEY()<> 32 FOR &lcRpExp
    WAIT WINDOW 'PRINTING - <Space Bar> TO ABORT' NOWAIT
    =lfGetOdHdr()
    SELECT OrdHdr
    =lfPrintHdr()
    SET ORDER TO TAG ORDLINE IN ORDLINE
    IF lcMulti = 'Y'
      IF !USED(lcMulTOrDT)
        =gfOpenFile(oAriaApplication.DataDir+lcMulTOrDT,'','EX')
      ELSE
        SELECT (lcMulTOrDT)
      ENDIF
    ELSE
      SELECT ORDLINE

      SET ORDER TO TAG IIF(lcRpSortBy = 'L' , 'ORDLINE' , 'ORDLINST' ) IN ORDLINE
      LOCATE
      =SEEK(ORDHDR.cOrdType+lcOrder)
    ENDIF
    SCAN WHILE EVAL(KEY()) = ORDHDR.cOrdType+lcOrder FOR IIF(lcRpBook = 'N',TOTQTY > 0,TOTQTY > 0 OR TOTQTY = 0 )
      IF lcMulti # 'Y' OR &lcMulTOrDT..STORE = lcStore  &&this is the reverse of the second case

      ELSE
        =lfGetStAd()
        =lfPrintHdr()
      ENDIF  && lcMulti # 'Y' OR &lcMulTOrDT..Store = lcStore
      =SEEK(IIF(lcMulti = 'Y' , &lcMulTOrDT..STYLE,Ordline.STYLE),'STYLE' )
      lcStyDesc  = STYLE.DESC
      IF lcMulti = 'Y'
        =SEEK(ORDHDR.cOrdType+ORDER+STR(LINENO,6),'OrdLine')
      ENDIF
      lcPackKey = 'P'+lcAccount+OrdLine.Pack_Id
      IF !SEEK('P'+lcAccount+OrdLine.Pack_Id+STYLE,'Spck_Lin')
        =SEEK('P'+'*****'+OrdLine.Pack_Id+STYLE,'Spck_Lin')
        lcPackKey = 'P'+'*****'+OrdLine.Pack_Id
      ENDIF
      IF !SEEK('P'+lcAccount+OrdLine.Pack_Id,'Spck_Hdr')
        =SEEK('P'+'*****'+OrdLine.Pack_Id,'Spck_Hdr')
      ENDIF
      IF OrdLine.Pack_Id <> lcPrePack .AND. llPack
        lcOldAlias = ALIAS()
        SELECT Spck_Lin
        SUM REST WHILE TYPE+Account+Pack_Id+STYLE = lcPackKey ;
          Spck_Lin.Qty1,Spck_Lin.Qty2,Spck_Lin.Qty3,;
          Spck_Lin.Qty4,Spck_Lin.Qty5,Spck_Lin.Qty6 TO ARRAY lnQty

        SELECT (lcOldAlias)
        lcPrePack = OrdLine.Pack_Id
        IF !EMPTY(OrdLine.Pack_Id)
          m.PACKID= 'PACK_ID : '+OrdLine.Pack_Id+'  '+'DESC.:'+Spck_Hdr.DESC+' ' +;
            +STR(lnQty[1],3)+' '+ STR(lnQty[2],3)+' '+STR(lnQty[3],3)+' ' +;
            STR(lnQty[4],3)+' '+STR(lnQty[5],3)+' '+STR(lnQty[6],3)
        ENDIF
      ENDIF
      m.NoteMem=''
      IF llRpOrdLnt
        m.NoteMem=Note_Mem
      ENDIF
      m.NLINE= LINENO
      m.CLINE=RIGHT('000'+ ALLTRIM(STR(LINENO)),4)
      m.GROUP= GROUP
      *B611449 HMS  Aria 4XP - margins needs to be adjusted (Eric Javits Inc)[T20171019.0009] [Begin]
      
      * m.STYLE= LEFT(STYLE,19-lnMajorLen)
        m.STYLE= SUBSTR(STYLE,1,lnMajorLen)
        
      *B611449 HMS  Aria 4XP - margins needs to be adjusted (Eric Javits Inc)[T20171019.0009] [END]
      
      
      *C200796,1 WLD Get Color Desc instead of color code 06/10/2007 [Begin]
      *M.COLOR=RIGHT(STYLE,19-lnMajorLen-1 )
      m.COLOR=ALLTRIM(SUBSTR(gfCodDes(RIGHT(STYLE,19-lnMajorLen-1 ) , 'COLOR'),1,12))
      *C200796,1 WLD Get Color Desc instead of color code 06/10/2007 [End]
      m.DESC=lcStyDesc
      m.PRICE=Price
      m.PIECES= TotQty
      m.STORE=LCSTORE
      m.Amount= TotQty * Price
      m.TOTBOOK=TOTBOOK
      *C200796,1 WLD Comment the following lines because the customer has styles with only one size 06/10/2007 [Begin]
      *FOR lnI =1 TO 8
      *  lcI=ALLTRIM(STR(lnI))
      *  m.BOOK&lcI=BOOK&lcI
      *  m.QTY&lcI=QTY&lcI
      *ENDFOR
      *C200796,1 wld [End]
      =SEEK('S'+SCALE,'SCALE')
      FOR lnI =1 TO 8
        lcI=ALLTRIM(STR(lnI))
        m.scale&lcI=SCALE.sz&lcI
      ENDFOR
      m.SC_DESC=SCALE.SCALE
      *-- Print the order line notes.

      INSERT INTO (lcRpTmpLines)FROM MEMVAR


    ENDSCAN
  ENDSCAN


  SELECT (lcRpTmpMain)
  IF !RECCOUNT()>0
      llDonprnt=.T.
      *-- Message : There are no records to display...!
      *--                < Ok >
      =gfModalGen('TRM00052B40011','ALERT')
      RETURN
  ENDIF
  =lfAdjustCRSettings()

  IF USED(lcRpTmpMain)
    USE IN (lcRpTmpMain)
  ENDIF

  IF USED(lcRpTmpLines)
    USE IN (lcRpTmpLines)
  ENDIF
  *C200796,1 WLD Check if print the report from the main or from the optional prorgram 06/10/2007 [Begin]
  *=gfDispRe()
  IF  llSalsOrd 
    =gfDispRe()
    llSalsOrd = .F.
  ENDIF
  *C200796,1 WLD Check if print the report from the main or from the optional prorgram 06/10/2007 [End]
ELSE
    IF llDonprnt
      *-- Message : There are no records to display...!
      *--                < Ok >
      =gfModalGen('TRM00052B40011','ALERT')
      RETURN
    ELSE
    *C200796,1 WLD Check if print the report from the main or from the optional prorgram 06/10/2007 [Begin]
    *=gfDispRe()
    IF  llSalsOrd 
      =gfDispRe()
      llSalsOrd = .F.
    endif  
    *C200796,1 WLD Check if print the report from the main or from the optional prorgram 06/10/2007 [End]
    ENDIF
ENDIF  &&FILTER CHANGE




*!*************************************************************
*! Name      : lfGetOdHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/24/99
*! Purpose   : TO get the information of the ORDER header.
*! Refer to  : (C101403)
*!*************************************************************
FUNCTION lfGetOdHdr
  PRIVATE lcAlias , lcCurrKey
  lcCurrKey = ""
  lcAlias = ALIAS()
  SELECT ORDHDR
  lcOrder   = ORDER
  lcAccount = Account
  lcMulti   = MULTI
  IF lcMulti = 'Y'
    IF USED(lcMulTOrDT)
      SELECT (lcMulTOrDT)
      USE
    ENDIF
    SELECT ORDLINE
    =SEEK(ordhdr.cOrdtype+lcOrder)
    COPY REST TO (oAriaApplication.WorkDir+lcMulTOrDT) WHILE cordtype+ORDER+STR(LINENO,6) = ordhdr.cOrdtype+lcOrder
    =gfOpenFile(oAriaApplication.WorkDir+lcMulTOrDT,'','EX')
    IF lcRpSortBy = 'S'
      IF llPack
        INDEX ON cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)+Pack_Id TAG (lcMulTOrDT)
      ELSE
        INDEX ON cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) TAG (lcMulTOrDT)
      ENDIF
    ELSE
      INDEX ON CORDTYPE+ORDER+STORE+STR(LINENO,6) TAG (lcMulTOrDT)
    ENDIF
    GO TOP
    lcStore = STORE
    =SEEK('S'+lcAccount+lcStore,'CUSTOMER')
  ENDIF
  lcStore = IIF(lcMulti <> 'Y',ORDHDR.STORE,lcStore)
  IF lcMulti = 'Y'
    IF !EMPTY(CUSTOMER.DIST_CTR)
      lcCurrKey = 'S' + Customer.Account + Customer.STORE
      =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
      lcDCStore = lcStore + ' DC:' + Customer.STORE
    ELSE
      lcDcStore = lcStore
    ENDIF
    IF !EMPTY(lcCurrKey)
      = SEEK(lcCurrKey , 'CUSTOMER')
    ENDIF
  ELSE
    lcDcStore = lcStore
  ENDIF

  STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6
  lcBtName  = CUSTOMER.BTNAME
  =gfGetAdr('Customer','','','',1,'2')
  *--Get the Bill To adddess except the country.
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcBtAddr&lcCount = lcBtAddr&lcCount + IIF(EMPTY(lcBtAddr&lcCount),'',',')+;
      ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  ENDFOR
  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF OrdHdr.Alt_ShpTo
    lcStName  = ALLTRIM(OrdHdr.STNAME)
    lcStAddr1 = ALLTRIM(OrdHdr.cAddress1)
    lcStAddr2 = ALLTRIM(OrdHdr.cAddress2)
    lcStAddr3 = ALLTRIM(OrdHdr.cAddress3)+','+ALLTRIM(OrdHdr.cAddress4)+','+ALLTRIM(OrdHdr.cAddress5)
  ELSE
    IF !EMPTY(CUSTOMER.DIST_CTR)
      lcCurrKey = 'S' + Customer.Account + Customer.STORE
      =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    ENDIF
    lcStName  = ALLTRIM(IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA))
    lcStAddr1 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
    lcStAddr2 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
    lcStAddr3 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
    lcStAddr4 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
    lcStAddr5 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5))
    IF !EMPTY(lcCurrKey)
      = SEEK(lcCurrKey , 'CUSTOMER')
    ENDIF
  ENDIF
  *-- GET DESCRIPTIONS FOR CODES FIELDS
  SELECT CODES
  *--Terms
  lcTermData =SUBSTR(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'),1,30)
  *--ShipVia

  IF lcMulti = 'Y'
    IF ALLTRIM(ORDHDR.ShipVia) = '*'
      lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
    ELSE
      lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,30)
    ENDIF
  ELSE
    lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,30)
  ENDIF
  *--Division long name.
  STORE '' TO lcDivLName
  =gfRltFld(OrdHdr.cDivision,@laDivLName,'CDIVISION')
  *--Special instruction
  lcSpcInst = SUBSTR(gfCodDes(OrdHdr.SPCINST , 'SPCINST'),1,30)
  *---Season
  lcSeason = SUBSTR(gfCodDes(OrdHdr.SEASON , 'SEASON'),1,30)
  *--Get the first line of the company address.

  SELECT (lcAlias)

  *!*************************************************************
  *! Name      : lfPrintHdr
  *! Developer : Adel Mohammed El Gazzar (ADEL)
  *! Date      : 02/24/99
  *! Purpose   : TO print the information of the ORDER header.
  *! Refer to  : (C101403)
  *!*************************************************************
FUNCTION lfPrintHdr
  PRIVATE lcAlias

  lcAlias = ALIAS()

  SELECT OrdHdr
  m.ORDER=ORDER
  m.ENTERED=ENTERED
  m.STORE= lcStore

  m.CUSTPO=IIF(MultiPO,&lcMulTOrDT..CUSTPO,CUSTPO)
  m.CNOTE=NOTE1+NOTE2

  m.MESSAGE1=lcRpMsg1
  m.MESSAGE2=lcRpMsg2
  m.MESSAGE3=lcRpMsg3

  m.ACCOUNT= Account
  m.DCSTORE= lcDCStore
  m.DEPT= Dept
  m.BTNAME= lcBtName

  m.BTADD1=lcBtAddr1
  IF EMPTY(M.BTADD1)
    m.BTADD1=lcBtAddr2
    m.BTADD2=""
  ELSE
    m.BTADD2=lcBtAddr2
  ENDIF

  IF EMPTY(M.BTADD1)
    m.BTADD1=lcBtAddr3
    m.BTADD3=""
  ELSE
    IF EMPTY(M.BTADD2)
      m.BTADD2=lcBtAddr3
      m.BTADD3=""
    ELSE
      m.BTADD3=lcBtAddr3
    ENDIF
  ENDIF

  m.STNAME=lcStName

  m.STADD1=lcStAddr1
  IF EMPTY(M.STADD1)
    m.STADD1=lcStAddr2
    m.STADD2=""
  ELSE
    m.STADD2=lcStAddr2
  ENDIF

  IF EMPTY(M.STADD1)
    m.STADD1=lcStAddr3
    m.STADD3=""
  ELSE
    IF EMPTY(M.STADD2)
      m.STADD2=lcStAddr3
      m.STADD3=""
    ELSE
      m.STADD3=lcStAddr3
    ENDIF
  ENDIF

  m.START=START
  m.COMPLETE= COMPLETE
  m.CTERM =lcTermData
  m.CSHIP = SUBSTR(lcShipVia,1,15)
  m.CSEASON= lcSeason
  m.REP1=REP1
  m.REP2=REP2
  m.Disc=Disc
  m.NOTE=''
  IF !EMPTY(Note1) .AND. SUBSTR(Note1,1,1) <> '*'
    m.NOTE=M.NOTE+  Note1
  ENDIF
  IF !EMPTY(Note2) .AND. SUBSTR(Note2,1,1) <> '*'
    m.NOTE=M.NOTE+  Note1
  ENDIF
  m.mNotepad=''
  IF llRpOrdNot AND SEEK( 'B'+lcOrder,'NotePad')
    m.mNotepad=NotePad.MNOTES
  ENDIF
  INSERT INTO (lcRpTmpMain)FROM MEMVAR

  SELECT (lcAlias)

  *!*************************************************************
  *! Name      : lfGetStAd
  *! Developer : Adel Mohammed El Gazzar (ADEL)
  *! Date      : 02/24/99
  *! Purpose   : TO get the store address.
  *! Refer to  : (C101403)
  *!*************************************************************
FUNCTION lfGetStAd
  PRIVATE lcAlias , lcCurrKey
  lcCurrKey = ""

  lcAlias = ALIAS()
  lcStore   = STORE
  lcAccount = Account
  =SEEK('S'+lcAccount+lcStore,'CUSTOMER')
  STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6

  IF ALLTRIM(ORDHDR.ShipVia) = '*'
    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
  ELSE
    lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
  ENDIF

  * Print distribution center address if found
  IF lcMulti = 'Y'
    IF !EMPTY(CUSTOMER.DIST_CTR)
      lcCurrKey = 'S' + Customer.Account + Customer.STORE
      =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
      lcDCStore = lcStore + ' DC:' + Customer.STORE
    ELSE
      lcDcStore = lcStore
    ENDIF
    IF !EMPTY(lcCurrKey)
      = SEEK(lcCurrKey , 'CUSTOMER')
    ENDIF
  ELSE
    lcDcStore = lcStore
  ENDIF

  lcBtName  = CUSTOMER.BTNAME
  =gfGetAdr('Customer','','','',1,'2')
  *--Get the Bill To adddess except the country.
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcBtAddr&lcCount = lcBtAddr&lcCount + IIF(EMPTY(lcBtAddr&lcCount),'',',')+;
      ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  ENDFOR


  IF !EMPTY(CUSTOMER.DIST_CTR)
    lcCurrKey = 'S' + Customer.Account + Customer.STORE
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
  ENDIF

  lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
  lcStAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  lcStAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  lcStAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  lcStAddr4 = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  lcStAddr5 = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

  IF !EMPTY(lcCurrKey)
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF

  SELECT (lcAlias)


  *!*************************************************************
  *! Name      : lfGetColor
  *! Developer : Adel Mohammed El Gazzar (ADEL)
  *! DATE      : 02/23/99
  *! Purpose   : To get the length to of the color.
  *! Refer To  : (C101403)
  *!*************************************************************
FUNCTION lfGetColor

  lcNonMajTl = ''
  *-- No. of major segments.
  lnMajSeg    = gfItemMask('SM')
  *-- Compute Free/Color Items in Style code Structure. [Begin]
  DIMENSION laMajSegs[1,1]
  = gfItemMask(@laMajSegs)
  *-- Loop Around Non Major elements.
  FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
    IF laMajSegs[lnI,1] = 'C'
      lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
        PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
        lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
      EXIT
    ENDIF
  ENDFOR



  *************************************************************
  *! Name      : lfAdjustCRSettings
  *! Developer : AYMAN MAHMOUD AHMED (SMM)
  *! Date      : 06/26/2006
  *! Purpose   : To set the report data files and parameters
  *!*************************************************************
FUNCTION lfAdjustCRSettings

  DIMENSION loOgScroll.laCRTables[2]
  DIMENSION loOgScroll.laCRParams[13,2]

  *C200796,1 WLD Print Custom Form 06/10/2007 [Begin]
  *loOgScroll.lcOGLastForm ='SOORCNB'
  loOgScroll.lcOGLastForm ='SOORCNER'
  *C200796,1 WLD Print Custom Form 06/10/2007 [End]
  loOGScroll.cCROrientation='P'
  loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcRpTmpMain+ ".DBF"
  loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcRpTmpLines+ ".DBF"


  loOgScroll.laCRParams[1,1] = 'ReportName'
  loOgScroll.laCRParams[1,2]= 'ORDER CONFIRMATION'

  loOgScroll.laCRParams[2,1] = 'lcEdiOrder'
  IF lcRpOrdTyp = 'T'
    loOgScroll.laCRParams[2,2]= 'Y'
  ELSE
    loOgScroll.laCRParams[2,2]= 'N'
  ENDIF


  loOgScroll.laCRParams[3,1] = 'lcBOOK'
  IF lcRpBOOK = 'N'
    loOgScroll.laCRParams[3,2]= 'Y'
  ELSE
    loOgScroll.laCRParams[3,2]= 'N'
  ENDIF

  loOgScroll.laCRParams[4,1] = 'lCDEC'
  IF lcRpDeciml = "Y"
    loOgScroll.laCRParams[4,2]= 'Y'
  ELSE
    loOgScroll.laCRParams[4,2]= 'N'
  ENDIF
  lcPhonPict = gfPhoneTem()

  loOgScroll.laCRParams[5,1] = 'lCADD2'
  loOgScroll.laCRParams[6,1] = 'lCADD3'
  loOgScroll.laCRParams[7,1] = 'lCADD4'
  loOgScroll.laCRParams[10,1] = 'lCADD1'
  loOgScroll.laCRParams[13,1] = 'lCADD5'


  IF EMPTY(lcDivLName)
    loOgScroll.laCRParams[5,2] = laCompAdd[2]
    loOgScroll.laCRParams[6,2] = laCompAdd[3]
    loOgScroll.laCRParams[7,2] = laCompAdd[4]
    loOgScroll.laCRParams[10,2] = laCompAdd[1]
    loOgScroll.laCRParams[13,2] = ''

  ELSE
    loOgScroll.laCRParams[5,2] = laCompAdd[1]
    loOgScroll.laCRParams[6,2] = laCompAdd[2]
    loOgScroll.laCRParams[7,2] = laCompAdd[3]
    loOgScroll.laCRParams[13,2] = laCompAdd[4]
    loOgScroll.laCRParams[10,2] = lcDivLName

  ENDIF




  loOgScroll.laCRParams[8,1] = 'lCFAX'
  loOgScroll.laCRParams[8,2] = TRANSFORM(SyCcomp.Ccom_Fax,'@R '+ lcPhonPict)

  loOgScroll.laCRParams[9,1] = 'lCPHON'
  loOgScroll.laCRParams[9,2] = TRANSFORM(SyCcomp.Ccom_phon,'@R '+ lcPhonPict)


  loOgScroll.laCRParams[11,1] = 'lCNOTLN'
  IF llRpOrdLnt
    loOgScroll.laCRParams[11,2]= 'Y'
  ELSE
    loOgScroll.laCRParams[11,2]= 'N'
  ENDIF

  loOgScroll.laCRParams[12,1] = 'lCPACK'
  IF LLPACK
    loOgScroll.laCRParams[12,2]= 'Y'
  ELSE
    loOgScroll.laCRParams[12,2]= 'N'
  ENDIF

  *************************************************************
  *! Name      : lfCheckFilter
  *! Developer : Saeed Mohammed (SMM)
  *! Date      : 09/07/2004
  *! Purpose   : Check if the filter was selected
  *!*************************************************************
FUNCTION lfCheckFilter
  LPARAMETERS lnArrayType, lcFilter
  LOCAL lcReturn, lnPOS
  DO CASE
    CASE lnArrayType = 1
      lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
      IF lnPos > 0
        lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
        lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
      ELSE
        lcReturn = ""
      ENDIF
    CASE lnArrayType = 2
      lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
      IF lnPos > 0
        lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
        lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
      ELSE
        lcReturn = ""
      ENDIF
    CASE lnArrayType = 3
      lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
      IF lnPos > 0
        lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
        lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
      ELSE
        lcReturn = ""
      ENDIF
    OTHERWISE
      lcReturn = ""
  ENDCASE

  RETURN lcReturn


  *!*************************************************************
  *! Name      : lfStr2Curs
  *! Developer : Ayman Mahmoud Ahmed(AYM)
  *! Date      : 02/11/2005
  *! Purpose   : Create cursor from string filters
  *!*************************************************************
  *! Passed Parameters  : File Name
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Modification : ....
  *!*************************************************************
FUNCTION lfStr2Curs
  PARAMETERS lcString , lccursor , lcFieldsName

  CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
  DO WHILE AT('|',lcString)> 0
    lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
    lcString = SUBSTR(lcString,AT('|',lcString)+1)
    SELECT (lcCursor)
    APPEND BLANK
    REPLACE &lcFieldsName. WITH lcFieldsValue
  ENDDO
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcString





  *************************************************************
  *! Name      : lfBuildTmp
  *! Developer : AYMAN MAHMOUD AHMED (AYM)
  *! Date      : 05/30/2006
  *! Purpose   :
  *!*************************************************************
FUNCTION lfBuildTmp

  DIMENSION laTempStru[28,18]
  STORE '' TO laTempStru
  STORE 0 TO lnIndex

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'ORDER'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 6
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'STORE'
  laTempStru[lnIndex,2] = 'C'
  *C200796,1 WLD Store # need to be 8 digits 06/10/2007 [Begin]
  *laTempStru[lnIndex,3] = 6
  laTempStru[lnIndex,3] = 8
   *C200796,1 WLD 06/10/2007 [End]
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'CUSTPO'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'ENTERED'
  laTempStru[lnIndex,2] = 'D'
  laTempStru[lnIndex,3] = 10
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'ACCOUNT'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 6
  laTempStru[lnIndex,4] = 0
  *5
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'DCSTORE'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'DEPT'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'BTNAME'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'BTADD1'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'BTADD2'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0
  *10
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'BTADD3'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'STNAME'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'STADD1'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'STADD2'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'STADD3'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0
  *15
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'START'
  laTempStru[lnIndex,2] = 'D'
  laTempStru[lnIndex,3] = 8
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'COMPLETE'
  laTempStru[lnIndex,2] = 'D'
  laTempStru[lnIndex,3] = 8
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'CTERM'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'CSHIP'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'CSEASON'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0
  *20
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'REP1'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 3
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'REP2'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 3
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'CNOTE'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 125
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'Disc'
  laTempStru[lnIndex,2] = 'N'
  laTempStru[lnIndex,3] = 6
  laTempStru[lnIndex,4] = 2

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'mNotepad'
  laTempStru[lnIndex,2] = 'M'
  laTempStru[lnIndex,3] = 0
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'MESSAGE1'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 125
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'MESSAGE2'

  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 125
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'MESSAGE3'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 125
  laTempStru[lnIndex,4] = 0


  *Create Temp*
  =gfCrtTmp(lcRpTmpMain,@laTempstru,,"",.F.)

  DIMENSION laTempStru[38,18]
  STORE '' TO laTempStru
  STORE 0 TO lnIndex
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'ORDER'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 6
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'GROUP'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 6
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'COLOR'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 30
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'STYLE'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 19
  laTempStru[lnIndex,4] = 0

  FOR lnI=1 TO 8
    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'QTY'+ALLTRIM(STR(lnI))
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 2
  ENDFOR

  FOR lnI=1 TO 8
    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'BOOK'+ALLTRIM(STR(lnI))
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 2
  ENDFOR

  FOR lnI=1 TO 8
    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'SCALE'+ALLTRIM(STR(lnI))
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 6
    laTempStru[lnIndex,4] = 0
  ENDFOR

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'TOTBOOK'
  laTempStru[lnIndex,2] = 'N'
  laTempStru[lnIndex,3] = 10
  laTempStru[lnIndex,4] = 2

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'NoteMem'
  laTempStru[lnIndex,2] = 'M'
  laTempStru[lnIndex,3] = 0
  laTempStru[lnIndex,4] = 0
*B608183,1 SSH Change Price picture to be 12,2
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'PRICE'
  laTempStru[lnIndex,2] = 'N'
  laTempStru[lnIndex,3] = 12
  laTempStru[lnIndex,4] = 2
*B608183,1 SSH Change Price picture to be 12,2
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'PIECES'
  laTempStru[lnIndex,2] = 'N'
  laTempStru[lnIndex,3] = 6
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'NLINE'
  laTempStru[lnIndex,2] = 'N'
  laTempStru[lnIndex,3] = 4
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'CLINE'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 4
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'PACKID'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 244
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'STORE'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 6
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'DESC'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 50
  laTempStru[lnIndex,4] = 0

  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'SC_DESC'
  laTempStru[lnIndex,2] = 'C'
  laTempStru[lnIndex,3] = 5
  laTempStru[lnIndex,4] = 0




  *Create Temp*
  =gfCrtTmp(lcRpTmpLines,@laTempstru,,"",.F.)
