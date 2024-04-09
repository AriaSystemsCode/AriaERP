*:***************************************************************************
*: Program file  : ALPKTKBH.PRG
*: Program desc. : Custom Picking ticket form for (BONG HWA).
*: Date          : 03/06/2007
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : HBG - Hend Ghanem
*: Tracking #    : C200763
*: Ticket #      : T20070206.0011
*:***************************************************************************
*: Example : DO ALPKTKBH
*:***************************************************************************

STORE 0 TO lnMajorLen, lnColorLen,lnNonMajst
=lfEvalSegs()

loSpck_Lin.SetOrder('SpckLins')	

STORE 0 TO lnTotPik, lnTotPrice, lnRow
DIMENSION laAddress[1,1]
STORE '' TO laAddress

*- Ship to and Bill to Addresses
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6

SELECT (lcTmpOrdL)
SET FILTER TO cGrupDetal = "D"
LOCATE
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

IF loOgScroll.llOGFltCh 
  =lfCrtTemp()
  =lfGetLines()  
ENDIF
*-- Add parameters to the report
DIMENSION loOgScroll.laCRParams[2,2]

loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2] = 'Pick Ticket Form'

loOgScroll.laCRParams[2,1] = 'DateTime'
loOgScroll.laCRParams[2,2] = oAriaApplication.SystemDate

*-- Send the temp file to the crystal report
DIMENSION LOogsCROLL.laCRTables[1]

loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTmOrdLin + ".DBF"

IF USED(lcTmOrdLin)
  USE IN (lcTmOrdLin)
ENDIF

gfDispRe()

*!**************************************************************************
*! Name      : lfCrtTemp
*: Developer : HBG - Hend Ghanem
*: Date      : 03/06/2007
*! Purpose   : Create Temp File
*!**************************************************************************
*! Example   :  =lfCrtTemp()
*!**************************************************************************
FUNCTION lfCrtTemp

DIMENSION laTmOrdLin[25,18]
lnField = 1
laTmOrdLin[lnField , 1] = 'Account'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 5
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'Piktkt'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 6
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'Store'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 8
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'BtName'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 30
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'StName'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 30
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'BtAdd1'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 30
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'StAdd1'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 30
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'BtAdd2'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 30
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'StAdd2'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 30
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'BtAdd3'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 30
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'StAdd3'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 30
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'ShipViaDsc'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 30
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'cTrmCodDsc'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 30
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'cCustPo'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 15
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'EneterDt'
laTmOrdLin[lnField , 2] = 'D'
laTmOrdLin[lnField , 3] = 8
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'Rep1'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 3
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'Order'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 6
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'Style'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = lnMajorLen
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'Color'
laTmOrdLin[lnField , 2] = 'C'
laTmOrdLin[lnField , 3] = 95
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'TotPik'
laTmOrdLin[lnField , 2] = 'N'
laTmOrdLin[lnField , 3] = 6
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'Price'
laTmOrdLin[lnField , 2] = 'N'
laTmOrdLin[lnField , 3] = 12
laTmOrdLin[lnField , 4] = 2

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'Amount'
laTmOrdLin[lnField , 2] = 'N'
laTmOrdLin[lnField , 3] = 14
laTmOrdLin[lnField , 4] = 2

lnField = lnField + 1
laTmOrdLin[lnField , 1] = 'Note_Mem'
laTmOrdLin[lnField , 2] = 'M'
laTmOrdLin[lnField , 3] = 1
laTmOrdLin[lnField , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField  , 1] = 'cGrupDetal'
laTmOrdLin[lnField  , 2] = 'C'
laTmOrdLin[lnField  , 3] = 1
laTmOrdLin[lnField  , 4] = 0

lnField = lnField + 1
laTmOrdLin[lnField  , 1] = 'llstLinGrp'
laTmOrdLin[lnField  , 2] = 'L'
laTmOrdLin[lnField  , 3] = 1
laTmOrdLin[lnField  , 4] = 0

FOR lnLoop = 1 TO  25
  STORE ' ' TO  laTmOrdLin[lnLoop,7],laTmOrdLin[lnLoop,8],;
                laTmOrdLin[lnLoop,9],laTmOrdLin[lnLoop,10],;
                laTmOrdLin[lnLoop,11],laTmOrdLin[lnLoop,12],;
                laTmOrdLin[lnLoop,13],laTmOrdLin[lnLoop,14],;
                laTmOrdLin[lnLoop,15],laTmOrdLin[lnLoop,16]
  STORE 0 TO    laTmOrdLin[lnLoop,17] ,laTmOrdLin[lnLoop,18]
ENDFOR

=gfCrtTmp(lcTmOrdLin,@laTmOrdLin,"PikTkt + Order + cGrupDetal",lcTmOrdLin)

*!**************************************************************************
*! Name      : lfGetLines
*: Developer : HBG - Hend Ghanem
*: Date      : 03/06/2007
*! Purpose   : To print pick tikcet lines
*!**************************************************************************
*! Example   :  =lfPrnLines()
*!**************************************************************************
FUNCTION lfGetLines

SET DEVICE TO PRINTER
STORE 0 TO lnTotPik, lnTotPrice
SELECT (lcTmpOrdL)
LOCATE

DO WHILE !EOF() 
  WAIT WINDOW 'PRINT PICK TICKETS' NOWAIT
  loOrdHdr.Seek('O'+Order)
  lnLine       = 0
  m.Piktkt     = PikTkt
  m.Order      = Order
  m.cGrupDetal = cGrupDetal
  m.Store      = Store
  m.Account    = &lcTempOrdHdr..Account
  STORE '' TO m.BtName,m.BtAdd1,m.BtAdd2,m.BtAdd3,m.BtAdd4,m.BtAdd5,m.BtAdd6
  STORE '' TO m.StName,m.StAdd1,m.StAdd2,m.StAdd3,m.StAdd4,m.StAdd5,m.StAdd6
 
  =lfCustInfo()

  llBtAdd2 = EMPTY(m.BtAdd2)
  llStAdd2 = EMPTY(m.StAdd2)
  m.BtAdd2 = IIF(!llBtAdd2,m.BtAdd2,m.BtAdd3)
  m.StAdd2 = IIF(!llStAdd2,m.StAdd2,m.StAdd3)
  m.BtAdd3 = IIF(!llBtAdd2,m.BtAdd3,'')
  m.StAdd3 = IIF(!llStAdd2,m.StAdd3,'')

  m.cCustPO    = IIF(&lcTempOrdHdr..MultiPO,&lcTempOrdHdr..CustPo,&lcTempOrdHdr..CustPO)
  m.EneterDt   = &lcTempOrdHdr..ENTERED 
  lcTerms      = &lcTempOrdHdr..CTERMCODE 
  lcShipVia    = IIF( &lcTempOrdHdr..MULTI = 'Y' , lfShipVia() , &lcTempOrdHdr..ShipVia )
  m.ShipViaDsc = ALLTRIM(gfCodDes(lcShipVia,'SHIPVIA  '))
  m.cTrmCodDsc = ALLTRIM(gfCodDes(lcTerms,'CTERMCODE '))  
  m.Rep1       = &lcTempOrdHdr..Rep1
  SCAN REST WHILE PikTkt+Order+cGrupDetal = m.Piktkt+m.Order+'D'
    lnLine = lnLine + 1
    m.llstLinGrp = .F.
    IF lnLine = 15
      lnline = 0
      m.llstLinGrp = .T.
    ENDIF

    lcClr = SUBSTR(Style,lnNonMajSt,lnColorLen)
    m.TOTPIK = TOTPIK
    m.Style  = SUBSTR(ALLTRIM(SUBSTR(STYLE,1,lnMajorLen))+ALLTRIM(lcClr) , 1 , 10 )
    m.Color  = SUBSTR(IIF(!EMPTY(&lcStyleFile..PREPAK),&lcStyleFile..PREPAK+'-','')+ALLTRIM(gfCodDes(lcClr,'COLOR     '))+' '+;
                    ALLTRIM(SUBSTR(&lcTmpOrdL..DESC1,1,22)),1,27)
    m.Price  = PRICE
    m.Amount = TOTPIK * PRICE
    
    IF llRpOrdNot AND loNotePad.SEEK('B' + m.Order)
      m.Note_Mem = &lcNotePad..mNotes
    ENDIF
    
    INSERT INTO (lcTmOrdLin) FROM MEMVAR
  ENDSCAN
ENDDO
WAIT CLEAR

*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : HBG - Hend Ghanem
*: Date        : 03/06/2007
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs
STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF

*!**************************************************************************
*! Name      : lfCustInfo
*: Developer : HBG - Hend Ghanem
*: Date      : 03/06/2007
*! Purpose   : To get the customer addresses.
*!**************************************************************************
*! Example   :  =lfCustInfo()
*!**************************************************************************
FUNCTION lfCustInfo
PRIVATE lcAlias,lnCount

lcAlias = SELECT(0)
DIMENSION laAddress[1,1]
STORE '' TO laAddress

lcCusExp   = IIF(EMPTY(m.Store),"M","S") + m.Account +IIF(EMPTY(m.Store),"",m.Store)
loCustomer.SEEK(lcCusExp)

*-- Bill to Address
m.BtName  = &lcTempCustomer..BtName
=gfGetAdr(lcTempCustomer,'','','',1,'2')

*--Get the Bill To adddess except the country.
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  m.BtAdd&lcCount = ALLTRIM(m.BtAdd&lcCount) + IIF(EMPTY(m.BtAdd&lcCount),'',',')+;
  ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
ENDFOR

*-- Sold to Address  
IF &lcTempOrdHdr..Alt_ShpTo
  m.StName = &lcTempOrdHdr..STNAME
  m.StAdd1 = &lcTempOrdHdr..cAddress1
  m.StAdd2 = &lcTempOrdHdr..cAddress2
  m.StAdd3 = &lcTempOrdHdr..cAddress3
  m.StAdd4 = &lcTempOrdHdr..cAddress4
  m.StAdd5 = &lcTempOrdHdr..cAddress5
ELSE
  SELECT (lcTempCustomer)
  lcDistCntr = &lcTempCustomer..Dist_Ctr
  *--If there is a distribution center
  IF !EMPTY(lcDistCntr)
    =loCustomer.SEEK('S' + &lcTempOrdHdr..Account + lcDistCntr)
  ELSE
    =loCustomer.SEEK(IIF(EMPTY(&lcTmpOrdL..Store) , 'M'+ &lcTempOrdHdr..Account,'S' + &lcTempOrdHdr..Account + &lcTmpOrdL..Store))
  ENDIF

  m.StName = &lcTempCustomer..STNAME
  =gfGetAdr(lcTempCustomer,'','','',1 )

  *--Get the Ship To address.
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    m.StAdd&lcCount = ALLTRIM(m.StAdd&lcCount) + IIF(EMPTY(m.StAdd&lcCount),'',',')+;
    ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  ENDFOR  	
ENDIF  
SELECT (lcAlias)

*:**************************************************************************
*: Name        : lfShipVia
*: Developer   : HBG - Hend Ghanem
*: Date        : 03/06/2007
*: Purpose     : Get ship via for each store in multi store case
*:***************************************************************************
FUNCTION lfShipVia
PRIVATE lnSlct,lcShipVia
lnSlct = SELECT()

SELECT (lcTempCustomer)
=loCustomer.SEEK('S'+m.Account+m.Store)
IF &lcTempCustomer..nBrkWeight <> 0 .AND. &lcTempOrdHdr..nWeight > &lcTempCustomer..nBrkWeight
  lcShipVia = &lcTempCustomer..cAltShpvia
ELSE  
  lcShipVia = &lcTempCustomer..SHIPVIA
ENDIF  

SELECT (lnSlct)
RETURN lcShipVia
*-- end of lfShipVia.