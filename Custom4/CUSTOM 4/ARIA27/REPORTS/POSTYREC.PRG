*:***************************************************************************
*: Program file  : POSTYREC
*: Program desc. : STYLE PURCHASE ORDER RECEIPTS REPORT
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : STYLE PURCHASE ORDER (PO)
*: Developer     : AHMED AMER (AHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POSTYREC
*:***************************************************************************
*!E301180,1 Walid 03/22/1999 Make all needed modification to make this report 
*           called by POSTREC.prg (Style P/o & C/t Receiving.)
*E301183,1  WALID 03/24/1999 add new function to print a new form if and only
*           if this report is called from the postrec program and the receiving
*           is by SHIPMENT , also make some modifications on the oreginal
*           code to skip unneeded porcessing in this case .
*-- lcRepNmTtl hold the header of the non major segment for the frx
*B603001,1 AMM 06/21/1999 Print the scale titles each time the scale changes.
*B603017,1 AMM 07/04/1999 Don't print any costing if the user has no access
*B602886,1 AMM 08/07/1999 Add The choice to print Return PO's
*B803076,1 RAMY 03/02/2000 Increase the picture of the grand total field to
*                          print 7 digits insted of 5]
*B603348,1  ABD 03/29/2000 Increase The Picture Of The Fildes Upon Width of 
*B603348,1                 the fileds UntCost & TotCost.
*B803594,1  MHM 09/10/2000 Add option to print decimal to get some space
*B603906,1 HBG 10/15/2000 Fix bug of wrong Total in case of usinf the foren currency 
*B603713,10 HBG 12/17/2000 fix bug of ***** appeared in price
*B603625,1 AMH 10/25/2001 Fis bug of wronge browse of Daye order in the first time.
*B606002,1 ALB 10/17/2002 Fix rounding problem in the Equ. Actual cost that update from AP
*B607302,1 ALB 08/13/2003 Print dollar sign is printed in the headr of the report beside Price column header
*B122651,1 NNA 05/24/2004 Fix bug that the Report not Showing the Second or Damaged Recipts.
*T20060831.0029 TMI ( B607797) 10/08/2006 make the report 130 width only 
*:************************************************************************************************************
lcRepNmTtl =  gfItemMask("HN")
lcTime     =  gfGetTime()
****** Variables Initialization ******
DIMENSION XTOTAL(1,10),XTOTAL1(1,10)
XTOTAL = 0.00
XTOTAL1= 0.00
XTIME  = TIME()
PAGENO = 0
ROW    = 99
R_WIDTH  = 'W'                                  && Standard Report is Wide
XREPORT  = 'POSTYREC'
*TTTT
R_TITLE  = IIF(gcAct_Appl='MF','DYE ORDER RECEIPTS REPORT',;
                          'STYLE PURCHASE ORDER RECEIPTS REPORT')
XTITLE   = lcRPTitle
*--
lcCstDtl = IIF(llRPCostDt,'Y','N')

*B803594,1  MHM 09/10/2000 [START]
*khalid
*lcRpDec  = IIF(llRpDec,'Y','N')
lcRpDec  = IIF(TYPE('llRpDec')="L",IIF(llRpDec,'Y','N'),'Y')
*khalid
*B803594,1  MHM 09/10/2000 [END]
*E301183,1 [BEGIN]
*-- If you came from the program not from OG AND the case is receiving by
*-- Shipment skip the following block of code till ENDIF .
IF !(TYPE("lcOGPrtDir")<>"C" AND lcRpSortBy='H')
  lcWareCode = SPACE( 6 )
  IF llMultiWH AND !USED('WAREHOUS')
    SELECT 0
    USE &gcDataDir.WareHous ORDER TAG WareHous SHARED
  ENDIF

  *-- laCost array that hold costing items label
  *-- laEst  array that hold estimated cost
  *-- laLan  array that hold landed cost
  *-- laAct  array that hold actual cost
  *-- laSEst array that hold subtotal estimated cost
  *-- laSLan array that hold subtotal landed cost
  *-- laSAct array that hold subtotal actual cost

  DIMENSION laCost [5]

  DIMENSION laEst [5]
  DIMENSION laLan [5]
  DIMENSION laAct [5]

  DIMENSION laSEst [5]
  DIMENSION laSLan [5]
  DIMENSION laSAct [5]

  STORE 0 TO laSEst , laSAct , laSlan ,laEst , laAct , laLan ,lnLan ,lnSubEst,lnSubLan,lnSubAct
  STORE SPACE(9) TO laCost [5]

  *-- RESTORE Memory variables (Start)

  DIMENSION laSetUps[5,2]
  
  *B603625,1 AMH Get the correct cost elements [Start]
  *laSetUps[1,1]  = 'M_CISLBL1'
  *laSetUps[2,1]  = 'M_CISLBL2'
  *laSetUps[3,1]  = 'M_CISLBL3'
  *laSetUps[4,1]  = 'M_CISLBL4'
  *laSetUps[5,1]  = 'M_CISLBL5'
  FOR lnI = 1 TO 5
    laSetUps[lnI,1] = 'M_C'+IIF(gcAct_Appl = "MF",'M','I')+'SLBL'+STR(lnI,1)
  ENDFOR
  *B603625,1 AMH [End]
  
  = gfGetMemVar(@laSetups)
  laCost[1] = LEFT(laSetUps[1,2],9)
  laCost[2] = LEFT(laSetUps[2,2],9)
  laCost[3] = LEFT(laSetUps[3,2],9)
  laCost[4] = LEFT(laSetUps[4,2],9)
  laCost[5] = LEFT(laSetUps[5,2],9)

  STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est
  STORE 0 TO lnFinEst  , lnFinLan ,lnFinAct

  DIMENSION laFinal[5,3]
  DIMENSION laSub[5,3]
  STORE 0 TO laFinal ,laSub, lnEst ,lnACT , lnLand , lnX,lnY,lnZ
  SORTFIELD = ' '
  BREAK     = ' '
  BREAKD    = ' '

  DO CASE
    * SUMMARY BY ORDER#
    CASE lcRPSortBy='P'
      SORTFIELD = 'PO+TRANCD+STYLE+STR(RECNO(),7)'
      BREAK     = 'PO'
      BREAKD    = 'PO'
    * SUMMARY BY VENDOR ACCOUNT
    CASE lcRPSortBy='V'
      SORTFIELD = 'POSHDR->VENDOR+PO+TRANCD+STYLE+STR(RECNO(),7)'
      BREAK     = 'POSHDR->VENDOR'
      BREAKD    = 'VENDOR'
    * SUMMARY BY PRIOIRTY
    CASE lcRPSortBy='S'
      SORTFIELD = 'STYLE+TRANCD+PO+STR(RECNO(),7)'
      BREAK     = 'STYLE'
      BREAKD    = 'STYLE'
    CASE lcRPSortBy='H'
      SORTFIELD = 'SHIPNO+CRSESSION+CWARECODE+STYLE+STR(RECNO(),7)'
      BREAK     = 'SHIPNO'
      BREAKD    = 'SHIPNO'
     
  ENDCASE

  IF lcRPPoType = "A"
    *B602886,1 AMM Add return PO type
    *lcPoType = "PN"
    lcPoType = "PNR"
    *B602886,1 AMM end
  ELSE
    lcPoType = lcRPPoType
  ENDIF
  *TTTT
  lcRpPoType = IIF(gcAct_Appl='MF','D',lcRpPoType)

  *B122651,1 NNA 05/24/2004 (Begin) Add the Second and The Damaged Quality to the Filter
  *lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),;
                        "TRANCD $ '2' .AND. cStyType $ lcPoType",;
                        " .AND. TRANCD $ '2' .AND. cStyType $ lcPoType")
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),;
                        "TRANCD $ '24' .AND. cStyType $ lcPoType",;
                        " .AND. TRANCD $ '24' .AND. cStyType $ lcPoType")
  *B122651,1 NNA (End)
  ****** Report File & Memory Loop Begin ******
  SELECT POSLN
  SET FILTER TO
  SET RELATION TO
  SET RELATION TO cStyType+PO INTO POSHDR
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  WAIT WINDOW 'Selecting records for report ...' NOWAIT
  LOCATE ALL FOR &lcRpExp
   IF EOF()
    *B602886,1 AMM Change to use message standards
    *WAIT WINDOW 'NO RECORDS SELECTED FOR REPORT!' NOWAIT
    *Message 'There are no records to display...!'
    =gfModalGen('TRM00052B00000','DIALOG')
    *B602886,1 AMM end
    SET DEVICE TO SCREEN
    RETURN
  ENDIF
  WORKFILE = gfTEMPNAME()
  COPY REST TO &gcWorkDir.&WORKFILE FOR &lcRpExp

  SELECT POSLN
  SET RELATION TO
  SET ORDER TO TAG PosLn

  SELECT 0
  USE &gcWorkDir.&WORKFILE EXCLUSIVE

  SET RELATION TO cStyType+PO INTO POSHDR
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  *B603001,1 AMM SET RELATION TO SCALE to get size scale titles
  SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE
  *B603001,1 AMM end
  WAIT CLEAR

  ****** Sort to Workfile Index ******
  IF SORTFIELD<>' '
    Z = LTRIM(STR(RECCOUNT(),7))
    WAIT WINDOW 'SORTING &Z RECORDS FOR STYLE PURCHASE ORDER DETAIL REPORT ...' NOWAIT
    INDEX ON &SORTFIELD TAG &WORKFILE
    SET ORDER TO TAG &WORKFILE
    WAIT CLEAR
  ENDIF
  SELECT &WORKFILE
  GOTO TOP
  IF BREAKD = 'SHIPNO'
    DELETE ALL FOR EMPTY(SHIPNO)
    GOTO TOP
    IF EOF()
      *B602886,1 AMM Change to use message standards 
      *WAIT WINDOW 'No records selected for the report .' NOWAIT
      *Message 'There are no records to display...!'
      =gfModalGen('TRM00052B00000','DIALOG')
      *B602886,1 AMM end
      SET DEVICE TO SCREEN
      RETURN
    ENDIF
  ENDIF
  IF LEN(TRIM(BREAK)) <> 0
    HBREAK = &BREAK
  ENDIF

  ****** Begin Printing ******
  * MAB 03/21/1999 WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT

  SELE &WORKFILE
  R_Title = R_Title + IIF( llMultiWH .AND. !EMPTY( lcWareCode ),;
                        ' FOR WAREHOUSE: '+ lcWareCode , '')

ENDIF &&-- IF TYPE("lcOGPrtDir")<>"C" AND lcRpSortBy='H'
*E301183,1 [END..]

SET DEVICE TO PRINT

IF lcCstDtl = 'N'
  lcCurr = IIF(llRPFrnCur,'Y','N')
  IF llRPFrnCur AND (EMPTY(lcCurr1) OR EMPTY(lcCurr2))
    *-- "You have to select price currency and duty cuurency to 
    *--  print in foreign currency."
    *-- OK
    = gfModalGen("INM34124B00000","Dialog")  
  ELSE
  *E301183,1 DO THE NEW FORM [BEGIN]
    IF TYPE("lcOGPrtDir")<>"C" AND lcRpSortBy='H'
      DO lpRecShip
      DO ENDREPORT
  *E301183,1 DO THE NEW FORM [END..]
    ELSE
      DO lpNoCosting
      DO ENDREPORT
    ENDIF
  ENDIF
ELSE
  DO lpPrint
  DO ENDREPORT
ENDIF
  
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************

FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************

FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'

      lnClrPo    = laMajSeg[lnI,4]

      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
  
      lcNonMajPi = laMajSeg[lnI,3]

      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
  
      EXIT
  
    ELSE
      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

*B603017,1 AMM If the user has access on costing
IF llCostPrv
*B603017,1 AMM end
  *-- This function to adjust Enabling stauts of currency fields.
  = lfvPrnForn()
  *-- This function to adjust enabling status 'print in foreign curr.' option
  = lfvCostDtl()
  *-- This function to adjust enabling status 'print costing detail.' option
  = lfvPrice()
  = lfPrnFrnSt()
*B603017,1 AMM End if
ENDIF
*B603017,1 AMM end
*-- end of lfwRepWhen.


*!*************************************************************
*! Name      : lfChkDye
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Check if the system uses dyelot
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfChkDye()
*!*************************************************************

FUNCTION lfChkDye

RETURN (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')

*!*************************************************************
*! Name      : lfChkMulWH
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : check system using for multi warehouse
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfChkMulWH()
*!*************************************************************

FUNCTION lfChkMulWH

RETURN (ALLTRIM(UPPER(gfGetMemVar('M_WAREHOUS')))  = 'Y')

*!*************************************************************
*! Name      : lfChkMulCu
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : check system using for multi currency
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfChkMulCu()
*!*************************************************************

FUNCTION lfChkMulCu

RETURN gfGetMemVar('llMulCurr')

*!*************************************************************
*! Name      : lfMajPic
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*!*************************************************************
*! Name      : lfvVendor
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************

FUNCTION lfvVendor

lcVenFld = VARREAD()

lcVendor = EVAL(lcVenFld)

SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF

&lcVenFld = lcVendor 

*!*************************************************************
*! Name      : lfvPO
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate purchase order
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPO()
*!*************************************************************

FUNCTION lfvPO

lcPOFld = VARREAD()

lcPONo = &lcPOFld

*TTTT
*IF !EMPTY(lcPONo) .AND. ('?' $ lcPONo .OR. !SEEK('P'+lcPONo , 'POSHDR'))
IF !EMPTY(lcPONo) .AND. ('?' $ lcPONo .OR. !SEEK(IIF(gcAct_Appl='MF','D','P')+lcPONo , 'POSHDR'))
  lcPONo = IIF(lfPOSBrow(),POSHDR.Po,'')
ENDIF

&lcPOFld = lcPONo

*!**************************************************************************

FUNCTION lfPOSBrow

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcVenTag,;
        lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)

lcFields    = "PO"
*TTTT
IF gcAct_Appl='MF'
lcBrFields  =  [PO        :R :H='Dye Order #':8,]+;
  	           [Status    :R :H='S':2,]+;
               [Vendor    :R :H='Contractor':11,]+;
               [lcVnName = ApVendor.cVenComp :R :H='Name':18,]+;
	           [Complete  :R :H='Complete':8,]+;
  	           [nStyOrder :R :H='Tot.Qty.':7,]+;
               [POTotal   :R :H='Amount':10,]+;
               [Receive   :R :H='Receive':7,]+;
               [Open      :R :H='Open':7]

  lcFile_Ttl  = 'Dye Order'
ELSE
*B602886,1 AMM Consider case of Return PO's
*lcBrFields  =  [Type = IIF(cStyType='N','Inter-Location','Purchase order') :H='Type':16,] +;
               [PO        :R :H='Po #':8,]+;
  	           [Status    :R :H='S':2,]+;
               [Vendor    :R :H='From':11,]+;
               [lcVnName = IIF(cStyType='N',WAREHOUS.cDesc,ApVendor.cVenComp) :R :H='Name':18,]+;
	           [Complete  :R :H='Complete':8,]+;
  	           [nStyOrder :R :H='Tot.Qty.':7,]+;
               [POTotal   :R :H='Amount':10,]+;
               [Receive   :R :H='Receive':7,]+;
               [Open      :R :H='Open':7]
lcBrFields  =  [Type = IIF(cStyType='N','Inter-Location',IIF(cStyType='R','Return PO','Purchase order')) :H='Type':16,] +;
               [PO        :R :H='Po #':8,]+;
  	           [Status    :R :H='S':2,]+;
               [Vendor    :R :H='From':11,]+;
               [lcVnName = IIF(cStyType='N',WAREHOUS.cDesc,ApVendor.cVenComp) :R :H='Name':18,]+;
	           [Complete  :R :H='Complete':8,]+;
  	           [nStyOrder :R :H='Tot.Qty.':7,]+;
               [POTotal   :R :H='Amount':10,]+;
               [Receive   :R :H='Receive':7,]+;
               [Open      :R :H='Open':7]
*B602886,1 AMM end

lcFile_Ttl  = 'Style purchase order'
ENDIF
SELECT POSHDR
lcCurTag = Order('POSHDR')
SET ORDER TO POSHDR

lcVenTag = ORDER('APVENDOR')
SET ORDER TO Vencode IN APVENDOR
SET RELATION TO PADR(Vendor,6) INTO WareHous ADDITIVE
SET RELATION TO Vendor INTO APVendor AADITIVE

IF lcRpPoType = "A"
  *B602886,1 AMM Add Return PO type
  *lcPoType = 'NP'
  lcPoType = 'NPR'
  *B602886,1 AMM end
ELSE
  lcPoType = lcRpPoType
ENDIF  
*TTTT
lcRpPoType = IIF(gcAct_Appl='MF','D',lcRpPoType)

    
llReturn = AriaBrow("FOR cStyType $ lcPoType",lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'POSHDR',.F.)

SET RELATION OFF INTO WareHous
SET RELATION OFF INTO APVendor
SET ORDER TO lcVenTag IN APVENDOR

SET ORDER TO lcCurTag IN POSHDR
SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfvStyle
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate Style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************

FUNCTION lfvStyle

lcStyle = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE

*!*************************************************************
*! Name      : lfvWareHo
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate warehouse
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWareHo()
*!*************************************************************

FUNCTION lfvWareHo

lcWareHo = VARREAD()

lcTag = ORDER('WAREHOUS')

SET ORDER TO WAREHOUS IN WAREHOUS

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcWareHo.,'WAREHOUS') 
    &lcWareHo = WAREHOUS.cWareCode
  ELSE
    &lcWareHo = gfBrowWare(.T.)
  ENDIF
ELSE
  &lcWareHo = ''
ENDIF

SET ORDER TO WAREHOUS IN WAREHOUS

*!*************************************************************

FUNCTION lfPrnFrnSt

*B603017,1 AMM If the element exists
IF ASCAN(laOGObjType,'llRPFrnCur') # 0
*B603017,1 AMM end
  lnFnrCurPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPFrnCur'),1)
  IF llMultiCur AND !llRPCostDt AND lcRPPrice = 'C'
    laOGObjCnt[lnFnrCurPo] = .T.
  ELSE
    laOGObjCnt[lnFnrCurPo] = .F.
    llRPFrnCur = .F.
  ENDIF  
  = lfOGShowGet('llRPFrnCur')
  = lfvPrnForn()
*B603017,1 AMM End if
ENDIF
*B603017,1 AMM end
*!*************************************************************
*! Name      : lfvPrice
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate print in option (Costing price/Retail price)
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrice()
*!*************************************************************

FUNCTION lfvPrice
*B603017,1 AMM If the element exists
IF ASCAN(laOGObjType,'llRPCostDt') # 0
*B603017,1 AMM end
  lnCostDtPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPCostDt'),1)
  laOGObjCnt[lnCostDtPo] = lcRPPrice = 'C'
  IF lcRPPrice = 'R'
    llRPCostDt = .F.
  ENDIF  
  = lfOGShowGet('llRPCostDt')
  = lfPrnFrnSt()
*B603017,1 AMM End if
ENDIF
*B603017,1 AMM end
*!*************************************************************
*! Name      : lfvCostDtl
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate print cost detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCostDtl()
*!*************************************************************

FUNCTION lfvCostDtl

= lfPrnFrnSt()

*!*************************************************************
*! Name      : lfvPrnForn
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate print in foreign cuurency option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrnForn()
*!*************************************************************

FUNCTION lfvPrnForn

*-- Adjust Enabling stauts of currency fields (Start)
lncDuCurPO = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.CDUTYCUR'),1)
lncPrCurPO = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.CPRICECUR'),1)

IF !llRPFrnCur
  STORE SPACE(0) TO laOGFxFlt[lncPrCurPO,6],laOGFxFlt[lncDuCurPO,6]
ELSE
  lcCurr1 = laOGFxFlt[lncPrCurPO,6]
  lcCurr2 = laOGFxFlt[lncDuCurPO,6]
ENDIF

lnVarCnt = ALEN(laOGObjType,1) - (ALEN(laOGFxFlt,1) + ALEN(laOGVrFlt,1))
lncDuCurPO = lnVarCnt + lncDuCurPO
lncPrCurPO = lnVarCnt + lncPrCurPO

STORE llRPFrnCur TO laOGObjCnt[lncDuCurPO],laOGObjCnt[lncPrCurPO]

= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lncPrCurPo-lnVarCnt)) + ',6]')
= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lncDuCurPo-lnVarCnt)) + ',6]')

*-- Adjust Enabling stauts of currency fields (End)

*!*************************************************************
*! Name      : lfvCurr
*! Developer : Ahmed Amer (AHM)
*! Date      : 06/15/98
*! Purpose   : Validate Currency code in SYCCURR file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Currency Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurr()
*!*************************************************************

FUNCTION lfvCurr
PRIVATE lcVar , lcObj , laTemp,lcCurTag

lcCurFld = VARREAD()

lcCurVal = &lcCurFld

lcCurTag = ORDER('SYCCURR')
SET ORDER TO Ccurrcode IN SYCCURR

IF !EMPTY(lcCurVal) AND ('?' $ lcCurVal OR !SEEK(lcCurVal , 'SYCCURR'))
  = gfcurrbrow(@lcCurVal)
ENDIF  

SET ORDER TO lcCurTag IN SYCCURR

lncDuCurPO = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.CDUTYCUR'),1)
lncPrCurPO = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.CPRICECUR'),1)
llEnabCur = llMultiCur AND llRPFrnCur

IF !llEnabCur
  STORE SPACE(0) TO laOGFxFlt[lncPrCurPO,6],laOGFxFlt[lncDuCurPO,6]
ELSE
  lcCurr1 = laOGFxFlt[lncPrCurPO,6]
  lcCurr2 = laOGFxFlt[lncDuCurPO,6]
ENDIF

&lcCurFld = lcCurVal

*!*************************************************************
*! Name      : lpPrint
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Print the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************

PROCEDURE lpPrint

*-- MAB 03/21/1999 No break condition[Begin]
*DO WHILE INKEY() <>32
DO WHILE .T.
*-- MAB 03/21/1999 No break condition[End  ]
  IF ROW >=53
    PAGENO = PAGENO+1
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    *@ 06,00 SAY 'P.O. # Status   Style                DYELOT      Vendor                         Division     Work In Process Account'
    *TTTT
    *@ 06,00 SAY 'P.O. # Status   ' + PADR(lcMajTtl,21) + 'DYELOT      Vendor                         Division     Work In Process Account'    
    @ 06,00 SAY IIF(gcAct_Appl='MF','D','P')+'.O. # Status   ' + PADR(lcMajTtl,21) + 'DYELOT      Vendor                         Division     Work In Process Account'      
    @ 07,00 SAY 'Ship.# WHouse   Description                      Vendor Name                    Description  Received On Total Received Qty'
    @ 08,00 SAY REPLICATE('=',132)
    ROW = 09
  ENDIF

  ******---- Begin Subtotals Loop ----******
  DO WHILE LEN(TRIM(BREAK)) <>0
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    ROW=ROW+1
    DO CASE
      CASE BREAKD ='PO'
        lnSort    = 'PO'
      CASE BREAKD ='VENDOR'
        lnSort    = 'VENDOR'
      CASE BREAKD ='STYLE'
        lnSort    = 'STYLE'
      CASE BREAKD ='SHIPNO'
        lnSort    = 'SHIPNO'
    ENDCASE
    *B603348,1 ABD Remark the next line & change the layout to be fit at printing.[Begin]
    *@ ROW,00 SAY '************************************ TOTAL FOR &lnSort &HBREAK ***************************************************'
    @ ROW,00 SAY '********************************************** TOTAL FOR &lnSort &HBREAK ************************************************************'
    *B603348,1 ABD [End]
    
    ROW=ROW+1
    @ ROW,000 SAY '**SUB TOTAL**'
    ROW=ROW+1
    
    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,003 SAY laCost[1]
    *@ROW ,013 SAY laSub[1,1]   PICTURE '9999999.999'
    @ROW ,000 SAY laCost[1]
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,010 SAY laSub[1,1]   PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,014 SAY laSub[1,1]   PICTURE '9999999999'
    ELSE
      @ROW ,010 SAY laSub[1,1]   PICTURE '9999999999.999'
    ENDIF
    *B803594,1  MHM 09/10/2000[END]
    *B603348,1 ABD [End]

    laFinal[1,1] = laFinal[1,1] + laSub[1,1]
    lnSubEst=lnSubEst+laSEST[1]

    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,024 SAY laSub[1,2]  PICTURE '9999999.999'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,025 SAY laSub[1,2]  PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,029 SAY laSub[1,2]  PICTURE '9999999999'
    ELSE
      @ROW ,025 SAY laSub[1,2]  PICTURE '9999999999.999'
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    
    laFinal[1,2] = laFinal[1,2] + laSub[1,2]
    lnSubLan=lnSubLan+laSLAN[1]
    
    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,35 SAY  laSub[1,3]  PICTURE '9999999.999'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,40 SAY  laSub[1,3]  PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,44 SAY  laSub[1,3]  PICTURE '9999999999'
    ELSE
      @ROW ,40 SAY  laSub[1,3]  PICTURE '9999999999.999'
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    laFinal[1,3] = laFinal[1,3] +laSub[1,3]
    lnSubAct=lnSubAct+laSAct[1]
    
    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,46 SAY (laSub[1,3] - laSub[1,2])                    PICTURE '9999999.999'
    *@ROW ,57 SAY (laSub[1,2] - laSub[1,1])                    PICTURE '9999999.999'
    *@ROW ,68 SAY (laSub[1,3] - laSub[1,1])                    PICTURE '9999999.999'
    *@ROW ,81 SAY IIF(laSub[1,2]=0,'N/A',(laSub[1,3] / laSub[1,2])*100) PICTURE '9999.99'
    *@ROW ,91 SAY IIF(laSub[1,1]=0,'N/A',(laSub[1,2] / laSub[1,1])*100) PICTURE '9999.99'
    *@ROW ,100 SAY IIF(laSub[1,1]=0,'N/A',(laSub[1,3] / laSub[1,1])*100) PICTURE '9999.99'
    
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,055 SAY (laSub[1,3] - laSub[1,2])                    PICTURE '99999999999.999'
    *@ROW ,071 SAY (laSub[1,2] - laSub[1,1])                    PICTURE '99999999999.999'
    *@ROW ,087 SAY (laSub[1,3] - laSub[1,1])                    PICTURE '99999999999.999'
    *@ROW ,103 SAY IIF(laSub[1,2]=0,'N/A',(laSub[1,3] / laSub[1,2])*100) PICTURE '9999.99'
    *@ROW ,111 SAY IIF(laSub[1,1]=0,'N/A',(laSub[1,2] / laSub[1,1])*100) PICTURE '9999.99'
    *@ROW ,119 SAY IIF(laSub[1,1]=0,'N/A',(laSub[1,3] / laSub[1,1])*100) PICTURE '9999.99'
     
     IF lcRpDec ='N'
       @ROW ,059 SAY (laSub[1,3] - laSub[1,2])                    PICTURE '99999999999'
       @ROW ,075 SAY (laSub[1,2] - laSub[1,1])                    PICTURE '99999999999'
       @ROW ,091 SAY (laSub[1,3] - laSub[1,1])                    PICTURE '99999999999'
       @ROW ,106 SAY IIF(laSub[1,2]=0,'N/A',(laSub[1,3] / laSub[1,2])*100) PICTURE '9999'
       @ROW ,114 SAY IIF(laSub[1,1]=0,'N/A',(laSub[1,2] / laSub[1,1])*100) PICTURE '9999'
       *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
       *@ROW ,122 SAY IIF(laSub[1,1]=0,'N/A',(laSub[1,3] / laSub[1,1])*100) PICTURE '9999'
       @ROW ,119 SAY IIF(laSub[1,1]=0,'N/A',(laSub[1,3] / laSub[1,1])*100) PICTURE '9999'
       *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
     ELSE  
       @ROW ,055 SAY (laSub[1,3] - laSub[1,2])                    PICTURE '99999999999.999'
       @ROW ,071 SAY (laSub[1,2] - laSub[1,1])                    PICTURE '99999999999.999'
       @ROW ,087 SAY (laSub[1,3] - laSub[1,1])                    PICTURE '99999999999.999'
       @ROW ,103 SAY IIF(laSub[1,2]=0,'N/A',(laSub[1,3] / laSub[1,2])*100) PICTURE '9999.99'
       @ROW ,111 SAY IIF(laSub[1,1]=0,'N/A',(laSub[1,2] / laSub[1,1])*100) PICTURE '9999.99'
       *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
       *@ROW ,119 SAY IIF(laSub[1,1]=0,'N/A',(laSub[1,3] / laSub[1,1])*100) PICTURE '9999.99'
       @ROW ,115 SAY IIF(laSub[1,1]=0,'N/A',(laSub[1,3] / laSub[1,1])*100) PICTURE '9999.99'
       *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
     ENDIF
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *B603348,1 ABD [End]

    ROW=ROW+1
    
    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,003 SAY laCost[2]
    *@ROW ,013 SAY laSub[2,1]   PICTURE '9999999.999'
    @ROW ,000 SAY laCost[2]
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,010 SAY laSub[2,1]   PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,014 SAY laSub[2,1]   PICTURE '9999999999'
    ELSE
      @ROW ,010 SAY laSub[2,1]   PICTURE '9999999999.999'
    ENDIF  
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    laFinal[2,1] = laFinal[2,1] + laSub[2,1]
    lnSubEst=lnSubEst+laSEST[2]
    
    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,024 SAY  laSub[2,2]  PICTURE '9999999.999'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,025 SAY  laSub[2,2]  PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,029 SAY  laSub[2,2]  PICTURE '9999999999'
    ELSE
      @ROW ,025 SAY  laSub[2,2]  PICTURE '9999999999.999'
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    laFinal[2,2] = laFinal[2,2] + laSub[2,2]
    lnSubLan=lnSubLan+laSLAN[2]

    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,35 SAY  laSub[2,3]            PICTURE '9999999.999'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,40 SAY  laSub[2,3]            PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,44 SAY  laSub[2,3]            PICTURE '9999999999'
    ELSE
      @ROW ,40 SAY  laSub[2,3]            PICTURE '9999999999.999'
    ENDIF  
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    laFinal[2,3] = laFinal[2,3] + laSub[2,3]
    lnSubAct=lnSubAct+laSACT[2]

    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,46 SAY (laSub[2,3] - laSub[2,2])                    PICTURE '9999999.999'
    *@ROW ,57 SAY (laSub[2,2] - laSub[2,1])                    PICTURE '9999999.999'
    *@ROW ,68 SAY (laSub[2,3] - laSub[2,1])                    PICTURE '9999999.999'
    *@ROW ,81 SAY IIF(laSub[2,2]=0,'N/A',(laSub[2,3] / laSub[2,2])*100) PICTURE '9999.99'
    *@ROW ,91 SAY IIF(laSub[2,1]=0,'N/A',(laSub[2,2] / laSub[2,1])*100) PICTURE '9999.99'
    *@ROW ,100 SAY IIF(laSub[2,1]=0,'N/A',(laSub[2,3] / laSub[2,1])*100) PICTURE '9999.99'

    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,055 SAY (laSub[2,3] - laSub[2,2])                    PICTURE '99999999999.999'
    *@ROW ,071 SAY (laSub[2,2] - laSub[2,1])                    PICTURE '99999999999.999'
    *@ROW ,087 SAY (laSub[2,3] - laSub[2,1])                    PICTURE '99999999999.999'
    *@ROW ,103 SAY IIF(laSub[2,2]=0,'N/A',(laSub[2,3] / laSub[2,2])*100) PICTURE '9999.99'
    *@ROW ,111 SAY IIF(laSub[2,1]=0,'N/A',(laSub[2,2] / laSub[2,1])*100) PICTURE '9999.99'
    *@ROW ,119 SAY IIF(laSub[2,1]=0,'N/A',(laSub[2,3] / laSub[2,1])*100) PICTURE '9999.99'

    IF lcRpDec ='N'
      @ROW ,059 SAY (laSub[2,3] - laSub[2,2])                    PICTURE '99999999999'
      @ROW ,075 SAY (laSub[2,2] - laSub[2,1])                    PICTURE '99999999999'
      @ROW ,091 SAY (laSub[2,3] - laSub[2,1])                    PICTURE '99999999999'
      @ROW ,106 SAY IIF(laSub[2,2]=0,'N/A',(laSub[2,3] / laSub[2,2])*100) PICTURE '9999'
      @ROW ,114 SAY IIF(laSub[2,1]=0,'N/A',(laSub[2,2] / laSub[2,1])*100) PICTURE '9999'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
      *@ROW ,122 SAY IIF(laSub[2,1]=0,'N/A',(laSub[2,3] / laSub[2,1])*100) PICTURE '9999'
      @ROW ,119 SAY IIF(laSub[2,1]=0,'N/A',(laSub[2,3] / laSub[2,1])*100) PICTURE '9999'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
    ELSE
      @ROW ,055 SAY (laSub[2,3] - laSub[2,2])                    PICTURE '99999999999.999'
      @ROW ,071 SAY (laSub[2,2] - laSub[2,1])                    PICTURE '99999999999.999'
      @ROW ,087 SAY (laSub[2,3] - laSub[2,1])                    PICTURE '99999999999.999'
      @ROW ,103 SAY IIF(laSub[2,2]=0,'N/A',(laSub[2,3] / laSub[2,2])*100) PICTURE '9999.99'
      @ROW ,111 SAY IIF(laSub[2,1]=0,'N/A',(laSub[2,2] / laSub[2,1])*100) PICTURE '9999.99'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
      *@ROW ,119 SAY IIF(laSub[2,1]=0,'N/A',(laSub[2,3] / laSub[2,1])*100) PICTURE '9999.99'
      @ROW ,115 SAY IIF(laSub[2,1]=0,'N/A',(laSub[2,3] / laSub[2,1])*100) PICTURE '9999.99'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    ROW=ROW+1
    

    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,003 SAY laCost[3]
    *@ROW ,013 SAY laSub[3,1]            PICTURE '9999999.999'
    @ROW ,000 SAY laCost[3]
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,010 SAY laSub[3,1]            PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,014 SAY laSub[3,1]            PICTURE '9999999999'
    ELSE
      @ROW ,010 SAY laSub[3,1]            PICTURE '9999999999.999'
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    laFinal[3,1] = laFinal[3,1] + laSub[3,1]
    lnSubEst=lnSubEst+laSEST[3]
    
    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,024 SAY laSub[3,2]  PICTURE '9999999.999'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,025 SAY laSub[3,2]  PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,029 SAY laSub[3,2]  PICTURE '9999999999'
    ELSE
      @ROW ,025 SAY laSub[3,2]  PICTURE '9999999999.999'
    ENDIF  
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    laFinal[3,2] = laFinal[3,2] + laSub[3,2]
    lnSubLan=lnSubLan+laSLAN[3]
    
    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,35 SAY laSub[3,3]   PICTURE '9999999.999'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,40 SAY laSub[3,3]   PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,44 SAY laSub[3,3]   PICTURE '9999999999'
    ELSE
      @ROW ,40 SAY laSub[3,3]   PICTURE '9999999999.999'
    ENDIF  
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]
   
    laFinal[3,3] = laFinal[3,3] + laSub[3,3]
    lnSubAct=lnSubAct+laSACT[3]

    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,46 SAY (laSub[3,3] - laSub[3,2])                   PICTURE '9999999.999'
    *@ROW ,57 SAY (laSub[3,2] - laSub[3,1])                   PICTURE '9999999.999'
    *@ROW ,68 SAY (laSub[3,3] - laSub[3,1])                   PICTURE '9999999.999'
    *@ROW ,81 SAY IIF(laSub[3,2]=0,'N/A',(laSub[3,3] / laSub[3,2])*100) PICTURE '9999.99'
    *@ROW ,91 SAY IIF(laSub[3,1]=0,'N/A',(laSub[3,2] / laSub[3,1])*100) PICTURE '9999.99'
    *@ROW ,100 SAY IIF(laSub[3,1]=0,'N/A',(laSub[3,3] / laSub[3,1])*100) PICTURE '9999.99'

    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,055 SAY (laSub[3,3] - laSub[3,2])                   PICTURE '99999999999.999'
    *@ROW ,071 SAY (laSub[3,2] - laSub[3,1])                   PICTURE '99999999999.999'
    *@ROW ,087 SAY (laSub[3,3] - laSub[3,1])                   PICTURE '99999999999.999'
    *@ROW ,103 SAY IIF(laSub[3,2]=0,'N/A',(laSub[3,3] / laSub[3,2])*100) PICTURE '9999.99'
    *@ROW ,111 SAY IIF(laSub[3,1]=0,'N/A',(laSub[3,2] / laSub[3,1])*100) PICTURE '9999.99'
    *@ROW ,119 SAY IIF(laSub[3,1]=0,'N/A',(laSub[3,3] / laSub[3,1])*100) PICTURE '9999.99'

    IF lcRpDec ='N'
      @ROW ,059 SAY (laSub[3,3] - laSub[3,2])                   PICTURE '99999999999'
      @ROW ,075 SAY (laSub[3,2] - laSub[3,1])                   PICTURE '99999999999'
      @ROW ,091 SAY (laSub[3,3] - laSub[3,1])                   PICTURE '99999999999'
      @ROW ,106 SAY IIF(laSub[3,2]=0,'N/A',(laSub[3,3] / laSub[3,2])*100) PICTURE '9999'
      @ROW ,114 SAY IIF(laSub[3,1]=0,'N/A',(laSub[3,2] / laSub[3,1])*100) PICTURE '9999'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
      *@ROW ,122 SAY IIF(laSub[3,1]=0,'N/A',(laSub[3,3] / laSub[3,1])*100) PICTURE '9999'
      @ROW ,119 SAY IIF(laSub[3,1]=0,'N/A',(laSub[3,3] / laSub[3,1])*100) PICTURE '9999'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
    ELSE  
      @ROW ,055 SAY (laSub[3,3] - laSub[3,2])                   PICTURE '99999999999.999'
      @ROW ,071 SAY (laSub[3,2] - laSub[3,1])                   PICTURE '99999999999.999'
      @ROW ,087 SAY (laSub[3,3] - laSub[3,1])                   PICTURE '99999999999.999'
      @ROW ,103 SAY IIF(laSub[3,2]=0,'N/A',(laSub[3,3] / laSub[3,2])*100) PICTURE '9999.99'
      @ROW ,111 SAY IIF(laSub[3,1]=0,'N/A',(laSub[3,2] / laSub[3,1])*100) PICTURE '9999.99'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
      *@ROW ,119 SAY IIF(laSub[3,1]=0,'N/A',(laSub[3,3] / laSub[3,1])*100) PICTURE '9999.99'
      @ROW ,115 SAY IIF(laSub[3,1]=0,'N/A',(laSub[3,3] / laSub[3,1])*100) PICTURE '9999.99'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]


    ROW=ROW+1
    
    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,003 SAY laCost[4]
    *@ROW ,013 SAY laSub[4,1]  PICTURE '9999999.999'
    @ROW ,000 SAY laCost[4]
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,010 SAY laSub[4,1]  PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,014 SAY laSub[4,1]  PICTURE '9999999999'
    ELSE
      @ROW ,010 SAY laSub[4,1]  PICTURE '9999999999.999'
    ENDIF      
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    laFinal[4,1] = laFinal[4,1] + laSub[4,1]
    lnSubEst=lnSubEst+laSEST[4]
    
    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,024 SAY laSub[4,2]  PICTURE '9999999.999'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,025 SAY laSub[4,2]  PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,029 SAY laSub[4,2]  PICTURE '9999999999'
    ELSE
      @ROW ,025 SAY laSub[4,2]  PICTURE '9999999999.999'
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    laFinal[4,2] = laFinal[4,2] + laSub[4,2]
    lnSubLan=lnSubLan+laSLAN[4]
    
    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,35 SAY  laSub[4,3]  PICTURE '9999999.999'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,40 SAY  laSub[4,3]  PICTURE '9999999999.999'    
    IF lcRpDec ='N'
      @ROW ,44 SAY  laSub[4,3]  PICTURE '9999999999'    
    ELSE
      @ROW ,40 SAY  laSub[4,3]  PICTURE '9999999999.999'    
    ENDIF  
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    
    laFinal[4,3] = laFinal[4,3] + laSub[4,3]
    lnSubAct=lnSubAct+laSACT[4]


    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,46 SAY (laSub[4,3] - laSub[4,2])                    PICTURE '9999999.999'
    *@ROW ,57 SAY (laSub[4,2] - laSub[4,1])                    PICTURE '9999999.999'
    *@ROW ,68 SAY (laSub[4,3] - laSub[4,1])                    PICTURE '9999999.999'
    *@ROW ,81 SAY IIF(laSub[4,2]=0,'N/A',(laSub[4,3] / laSub[4,2])*100) PICTURE '9999.99'
    *@ROW ,91 SAY IIF(laSub[4,1]=0,'N/A',(laSub[4,2] / laSub[4,1])*100) PICTURE '9999.99'
    *@ROW ,100 SAY IIF(laSub[4,1]=0,'N/A',(laSub[4,3] / laSub[4,1])*100) PICTURE '9999.99'
    
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,055 SAY (laSub[4,3] - laSub[4,2])                    PICTURE '99999999999.999'
    *@ROW ,071 SAY (laSub[4,2] - laSub[4,1])                    PICTURE '99999999999.999'
    *@ROW ,087 SAY (laSub[4,3] - laSub[4,1])                    PICTURE '99999999999.999'
    *@ROW ,102 SAY IIF(laSub[4,2]=0,'N/A',(laSub[4,3] / laSub[4,2])*100) PICTURE '9999.99'
    *@ROW ,111 SAY IIF(laSub[4,1]=0,'N/A',(laSub[4,2] / laSub[4,1])*100) PICTURE '9999.99'
    *@ROW ,119 SAY IIF(laSub[4,1]=0,'N/A',(laSub[4,3] / laSub[4,1])*100) PICTURE '9999.99'
    IF lcRpDec ='N'
      @ROW ,059 SAY (laSub[4,3] - laSub[4,2])                    PICTURE '99999999999'
      @ROW ,075 SAY (laSub[4,2] - laSub[4,1])                    PICTURE '99999999999'
      @ROW ,091 SAY (laSub[4,3] - laSub[4,1])                    PICTURE '99999999999'
      @ROW ,106 SAY IIF(laSub[4,2]=0,'N/A',(laSub[4,3] / laSub[4,2])*100) PICTURE '9999'
      @ROW ,114 SAY IIF(laSub[4,1]=0,'N/A',(laSub[4,2] / laSub[4,1])*100) PICTURE '9999'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
      *@ROW ,122 SAY IIF(laSub[4,1]=0,'N/A',(laSub[4,3] / laSub[4,1])*100) PICTURE '9999'
      @ROW ,119 SAY IIF(laSub[4,1]=0,'N/A',(laSub[4,3] / laSub[4,1])*100) PICTURE '9999'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
    ELSE
      @ROW ,055 SAY (laSub[4,3] - laSub[4,2])                    PICTURE '99999999999.999'
      @ROW ,071 SAY (laSub[4,2] - laSub[4,1])                    PICTURE '99999999999.999'
      @ROW ,087 SAY (laSub[4,3] - laSub[4,1])                    PICTURE '99999999999.999'
      @ROW ,103 SAY IIF(laSub[4,2]=0,'N/A',(laSub[4,3] / laSub[4,2])*100) PICTURE '9999.99'
      @ROW ,111 SAY IIF(laSub[4,1]=0,'N/A',(laSub[4,2] / laSub[4,1])*100) PICTURE '9999.99'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
      *@ROW ,119 SAY IIF(laSub[4,1]=0,'N/A',(laSub[4,3] / laSub[4,1])*100) PICTURE '9999.99'
      @ROW ,115 SAY IIF(laSub[4,1]=0,'N/A',(laSub[4,3] / laSub[4,1])*100) PICTURE '9999.99'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]
    ROW=ROW+1

    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,003 SAY laCost[5]
    *@ROW ,013 SAY laSub[5,1]  PICTURE '9999999.999'
    @ROW ,000 SAY laCost[5]
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,010 SAY laSub[5,1]  PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,014 SAY laSub[5,1]  PICTURE '9999999999'
    ELSE
      @ROW ,010 SAY laSub[5,1]  PICTURE '9999999999.999'
    ENDIF  
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]
    
    laFinal[5,1] = laFinal[5,1] + laSub[5,1]
    lnSubEst=lnSubEst+laSEST[5]
    
    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,024 SAY laSub[5,2]  PICTURE '9999999.999'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,025 SAY laSub[5,2]  PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,029 SAY laSub[5,2]  PICTURE '9999999999'
    ELSE
      @ROW ,025 SAY laSub[5,2]  PICTURE '9999999999.999'
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    laFinal[5,2] = laFinal[5,2] + laSub[5,2]
    lnSubLan=lnSubLan+laSLAN[5]

    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,35 SAY  laSub[5,3]  PICTURE '9999999.999'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,40 SAY  laSub[5,3]  PICTURE '9999999999.999'
    IF lcRpDec ='N'
      @ROW ,44 SAY  laSub[5,3]  PICTURE '9999999999'
    ELSE
      @ROW ,40 SAY  laSub[5,3]  PICTURE '9999999999.999'
    ENDIF  
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]
    
    laFinal[5,3] = laFinal[5,3] + (laSAct[5]*lnTOTQTY)
    lnSubAct=lnSubAct+laSact[5]

    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ROW ,46 SAY (laSub[5,3] - laSub[5,2])                    PICTURE '9999999.999'
    *@ROW ,57 SAY (laSub[5,2] - laSub[5,1])                    PICTURE '9999999.999'
    *@ROW ,68 SAY (laSub[5,3] - laSub[5,1])                    PICTURE '9999999.999'
    *@ROW ,81 SAY IIF(laSub[5,2]=0,'N/A',(laSub[5,3] / laSub[5,2])*100) PICTURE '9999.99'
    *@ROW ,91 SAY IIF(laSub[5,1]=0,'N/A',(laSub[5,2] / laSub[5,1])*100) PICTURE '9999.99'
    *@ROW ,100 SAY IIF(laSub[5,1]=0,'N/A',(laSub[5,3] / laSub[5,1])*100) PICTURE '9999.99'

    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ROW ,055 SAY (laSub[5,3] - laSub[5,2])                    PICTURE '99999999999.999'
    *@ROW ,071 SAY (laSub[5,2] - laSub[5,1])                    PICTURE '99999999999.999'
    *@ROW ,087 SAY (laSub[5,3] - laSub[5,1])                    PICTURE '99999999999.999'
    *@ROW ,103 SAY IIF(laSub[5,2]=0,'N/A',(laSub[5,3] / laSub[5,2])*100) PICTURE '9999.99'
    *@ROW ,111 SAY IIF(laSub[5,1]=0,'N/A',(laSub[5,2] / laSub[5,1])*100) PICTURE '9999.99'
    *@ROW ,119 SAY IIF(laSub[5,1]=0,'N/A',(laSub[5,3] / laSub[5,1])*100) PICTURE '9999.99'
    IF lcRpDec ='N'
      @ROW ,059 SAY (laSub[5,3] - laSub[5,2])                    PICTURE '99999999999'
      @ROW ,075 SAY (laSub[5,2] - laSub[5,1])                    PICTURE '99999999999'
      @ROW ,091 SAY (laSub[5,3] - laSub[5,1])                    PICTURE '99999999999'
      @ROW ,106 SAY IIF(laSub[5,2]=0,'N/A',(laSub[5,3] / laSub[5,2])*100) PICTURE '9999'
      @ROW ,114 SAY IIF(laSub[5,1]=0,'N/A',(laSub[5,2] / laSub[5,1])*100) PICTURE '9999'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
      *@ROW ,122 SAY IIF(laSub[5,1]=0,'N/A',(laSub[5,3] / laSub[5,1])*100) PICTURE '9999'
      @ROW ,119 SAY IIF(laSub[5,1]=0,'N/A',(laSub[5,3] / laSub[5,1])*100) PICTURE '9999'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
    ELSE
      @ROW ,055 SAY (laSub[5,3] - laSub[5,2])                    PICTURE '99999999999.999'
      @ROW ,071 SAY (laSub[5,2] - laSub[5,1])                    PICTURE '99999999999.999'
      @ROW ,087 SAY (laSub[5,3] - laSub[5,1])                    PICTURE '99999999999.999'
      @ROW ,103 SAY IIF(laSub[5,2]=0,'N/A',(laSub[5,3] / laSub[5,2])*100) PICTURE '9999.99'
      @ROW ,111 SAY IIF(laSub[5,1]=0,'N/A',(laSub[5,2] / laSub[5,1])*100) PICTURE '9999.99'
      *Alb
      *@ROW ,119 SAY IIF(laSub[5,1]=0,'N/A',(laSub[5,3] / laSub[5,1])*100) PICTURE '9999.99'
      @ROW ,115 SAY IIF(laSub[5,1]=0,'N/A',(laSub[5,3] / laSub[5,1])*100) PICTURE '9999.99'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]
    
    ROW = ROW + 1

    *B603348,1 ABD Remark the next lines & Move them to Right. .[Begin]
    *@ ROW ,14 SAY '---------- ----------  --------- ----------  --------- ----------  -------   ------- --------'
     @ ROW ,14 SAY '----------     ----------      ---------      ----------       ---------      ---------- ------- ------- --------'    
    *B603348,1 ABD [End]

    ROW = ROW + 1
    
    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ ROW , 03 SAY 'Total Cost'
    *@ ROW , 13 SAY laSub[1,1]+laSub[2,1]+laSub[3,1]+laSub[4,1]+laSub[5,1]    PICTURE '9999999.999'

    @ ROW , 00 SAY 'Total Cost'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ ROW , 10 SAY laSub[1,1]+laSub[2,1]+laSub[3,1]+laSub[4,1]+laSub[5,1]    PICTURE '99999999999.99'
    IF lcRpDec ='N'
      @ ROW , 13 SAY laSub[1,1]+laSub[2,1]+laSub[3,1]+laSub[4,1]+laSub[5,1]    PICTURE '99999999999'
    ELSE
      @ ROW , 10 SAY laSub[1,1]+laSub[2,1]+laSub[3,1]+laSub[4,1]+laSub[5,1]    PICTURE '99999999999.99'
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]
    
    lnX= laSub[1,1]+laSub[2,1]+laSub[3,1]+laSub[4,1]+laSub[5,1]
    lnEst = lnEst + laSEST[1]

    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ ROW , 24 SAY laSub[1,2]+laSub[2,2]+laSub[3,2]+laSub[4,2]+laSub[5,2]   PICTURE '9999999.999'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ ROW , 25 SAY laSub[1,2]+laSub[2,2]+laSub[3,2]+laSub[4,2]+laSub[5,2]   PICTURE '99999999999.99'
    IF lcRpDec ='N'
      @ ROW , 28 SAY laSub[1,2]+laSub[2,2]+laSub[3,2]+laSub[4,2]+laSub[5,2]   PICTURE '99999999999'
    ELSE
      @ ROW , 25 SAY laSub[1,2]+laSub[2,2]+laSub[3,2]+laSub[4,2]+laSub[5,2]   PICTURE '99999999999.99'
    ENDIF    
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]
    
    lnY=laSub[1,2]+laSub[2,2]+laSub[3,2]+laSub[4,2]+laSub[5,2]
    lnLan = lnLan+laSlan[1]
 

    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ ROW , 35 SAY laSub[1,3]+laSub[2,3]+laSub[3,3]+laSub[4,3]+laSub[5,3]  PICTURE '9999999.999'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ ROW , 40 SAY laSub[1,3]+laSub[2,3]+laSub[3,3]+laSub[4,3]+laSub[5,3]  PICTURE '99999999999.99'
    IF lcRpDec ='N'
      @ ROW , 43 SAY laSub[1,3]+laSub[2,3]+laSub[3,3]+laSub[4,3]+laSub[5,3]  PICTURE '99999999999'
    ELSE
      @ ROW , 40 SAY laSub[1,3]+laSub[2,3]+laSub[3,3]+laSub[4,3]+laSub[5,3]  PICTURE '99999999999.99'
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]
     
    lnZ=  laSub[1,3]+laSub[2,3]+laSub[3,3]+laSub[4,3]+laSub[5,3]
    lnAcT  = lnAcT + laSact[1]

    *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
    *@ ROW , 46 SAY lnZ- lnY                                                PICTURE '9999999.999'
    *@ ROW , 57 SAY (lnY - lnX)                                                PICTURE '9999999.999'
    *@ ROW , 68 SAY (lnZ - lnX)                                                PICTURE '9999999.999'
    *@ ROW , 81 SAY IIF(lnY=0,'N/A',(lnZ / lnY)*100)                 PICTURE '9999.99'
    *@ ROW , 91 SAY IIF(lnX=0,'N/A',(lnY / lnX)*100)                  PICTURE '9999.99'
    *@ ROW , 100 SAY IIF(lnX=0,'N/A',(lnZ / lnX)*100)                   PICTURE '9999.99'
    
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ ROW , 055 SAY lnZ- lnY                         PICTURE '999999999999.99'
    *@ ROW , 071 SAY (lnY - lnX)                      PICTURE '999999999999.99'
    *@ ROW , 087 SAY (lnZ - lnX)                      PICTURE '999999999999.99'
    *@ ROW , 103 SAY IIF(lnY=0,'N/A',(lnZ / lnY)*100) PICTURE '9999.99'
    *@ ROW , 111 SAY IIF(lnX=0,'N/A',(lnY / lnX)*100) PICTURE '9999.99'
    *@ ROW , 119 SAY IIF(lnX=0,'N/A',(lnZ / lnX)*100) PICTURE '9999.99'
    IF lcRpDec ='N'
      @ ROW , 058 SAY lnZ- lnY                         PICTURE '999999999999'
      @ ROW , 074 SAY (lnY - lnX)                      PICTURE '999999999999'
      @ ROW , 090 SAY (lnZ - lnX)                      PICTURE '999999999999'
      @ ROW , 106 SAY IIF(lnY=0,'N/A',(lnZ / lnY)*100) PICTURE '9999'
      @ ROW , 114 SAY IIF(lnX=0,'N/A',(lnY / lnX)*100) PICTURE '9999'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
      *@ ROW , 122 SAY IIF(lnX=0,'N/A',(lnZ / lnX)*100) PICTURE '9999'
      @ ROW , 119 SAY IIF(lnX=0,'N/A',(lnZ / lnX)*100) PICTURE '9999'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
    ELSE
      @ ROW , 055 SAY lnZ- lnY                         PICTURE '999999999999.99'
      @ ROW , 071 SAY (lnY - lnX)                      PICTURE '999999999999.99'
      @ ROW , 087 SAY (lnZ - lnX)                      PICTURE '999999999999.99'
      @ ROW , 103 SAY IIF(lnY=0,'N/A',(lnZ / lnY)*100) PICTURE '9999.99'
      @ ROW , 111 SAY IIF(lnX=0,'N/A',(lnY / lnX)*100) PICTURE '9999.99'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
      *@ ROW , 119 SAY IIF(lnX=0,'N/A',(lnZ / lnX)*100) PICTURE '9999.99'
      @ ROW , 115 SAY IIF(lnX=0,'N/A',(lnZ / lnX)*100) PICTURE '9999.99'
      *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]

    ROW = ROW+1

    *B603348,1 ABD Remark the next lines & Move them to right. .[Begin]
    *@ ROW ,14 SAY '========== ========== ==========  ========= ==========  =========  =======  ======== ======== '
    @ ROW ,14 SAY '==========     ==========      =========      ==========       =========      ========== ======= ======= ========'
    *B603348,1 ABD [End]
    
    STORE 0 TO laSEst , laSAct , laSlan
    STORE 0 TO lnSubEst,lnSubLan,lnSubAct  ,laSub
    HBREAK = &BREAK
     
    EXIT
  ENDDO
    
  ******---- End Subtotals Loop ----******
  IF EOF()
    EXIT
  ENDIF

  *B603625,1 AMH Set lcStatus to empty value [Start]
  lcSTATUS = ''
  *B603625,1 AMH [End]

  DO CASE
    CASE POSHDR.STATUS = 'O'
       lcSTATUS='OPEN'
    CASE POSHDR.STATUS = 'C'
       lcSTATUS= 'COMPLETE'
    CASE POSHDR.STATUS = 'X'
       lcSTATUS='CANCEL'
    CASE POSHDR.STATUS = 'S'
       lcSTATUS= 'CLOSED'
    CASE POSHDR.STATUS = 'H'
       lcSTATUS= 'HOLD'
    
    *B603625,1 AMH Consider the case of actual status [Start]
    CASE POSHDR.STATUS = 'A'
       lcSTATUS= 'ACTUAL.'
    *B603625,1 AMH [End]
    
  ENDCASE

  IF ROW >=53
    ROW = 99
    LOOP
  ENDIF
  SELECT &WORKFILE
  ROW=ROW+1
  @ ROW,000 SAY PO
  @ ROW,007 SAY lcSTATUS
  @ ROW,016 SAY STYLE
  @ ROW,037 SAY Dyelot
  @ ROW,049 SAY POSHDR->VENDOR
  @ ROW,080 SAY POSHDR.cDIVISION
  SELECT GL_LINK
  @ ROW , 93 SAY LINK_CODE + '-'
  @ ROW , 97 SAY IIF(SEEK(POSHDR.LINK_CODE+'013'),GLACNT,'')
  SELECT &WORKFILE
  ROW= ROW+1
  @ ROW,000 SAY SHIPNO
  @ ROW,007 SAY &WORKFILE..CWARECODE
  @ ROW,016 SAY IIF(SEEK(STYLE,'STYLE'),SUBSTR(STYLE.DESC,1,19),SPACE(19))
  @ ROW,049 SAY IIF(SEEK(Vendor,'ApVendor'),SUBSTR(ApVendor.CVENCOMP,1,15),SPACE(15))

  lcDivDesc = gfCodDes(STYLE.cDivision,'cDivision')
  @ ROW,080 SAY lcDivDesc
  @ ROW,93 SAY DATE
  @ ROW,116 SAY TOTQTY
  ROW=ROW+1

  *B603348,1 ABD Remark the next lines & Move them to left to indicate the layout .[Begin]
  *@ ROW,015 SAY 'Estimated     Landed     Actual  Act - Lan  Lan - Est  Act - Est  Act/Lan   Lan/Est  Act/Est'
  @ ROW,015 SAY 'Estimated         Landed         Actual       Act - Lan       Lan - Est       Act - Est Act/Lan Lan/Est Act/Est'
  *B603348,1 ABD [End]
  
  ROW=ROW+1

  lnAlias = SELECT(0)
  SELECT PosLn
  =SEEK('P'+&WorkFile..Po+&WorkFile..Style)
  LOCATE REST WHILE cStyType+Po+Style+STR(LINENO,4)+TRANCD+STR(RECNO(),7)=;
                    'P'+&WorkFile..Po+&WorkFile..Style;
              FOR TranCd = "1"                    
  SELECT(lnAlias)

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 03 SAY laCost[1]
  @ ROW , 00 SAY laCost[1]
  *B603348,1 ABD [End]

  *-- B602336 AAMER 12/14/98 (Start)
  *@ ROW , 13 SAY PosLn.NCost1*TotQty/PosHdr.Rate PICTURE '9999999.999'
  *laSub[1,1]=laSub[1,1]+(PosLn.NCost1*TotQty/PosHdr.Rate)

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 13 SAY nECost1*TotQty PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 010 SAY nECost1*TotQty PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 014 SAY nECost1*TotQty PICTURE '9999999999'
  ELSE
    @ ROW , 010 SAY nECost1*TotQty PICTURE '9999999999.999'
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]

  laSub[1,1]=laSub[1,1]+(nECost1*TotQty)
  *-- B602336 AAMER 12/14/98 (End)

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 24 SAY nELanCost1*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 25 SAY nELanCost1*TOTQTY  PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 29 SAY nELanCost1*TOTQTY  PICTURE '9999999999'
  ELSE
    @ ROW , 25 SAY nELanCost1*TOTQTY  PICTURE '9999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]


  laSub[1,2]=laSub[1,2]+(NELanCost1*TOTQTY)
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 35 SAY NEActCost1*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 40 SAY NEActCost1*TOTQTY                    PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 44 SAY NEActCost1*TOTQTY                    PICTURE '9999999999'
  ELSE
    *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
    *@ ROW , 40 SAY NEActCost1*TOTQTY           PICTURE '9999999999.999'    
    @ ROW , 40 SAY ROUND(NEActCost1*TOTQTY,3)           PICTURE '9999999999.999'    
    *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
  *laSub[1,3]=laSub[1,3]+(NEActCost1*TOTQTY)
  laSub[1,3]=laSub[1,3]+ROUND((NEActCost1*TOTQTY),3)
  *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 46 SAY (NEActCost1 -  NELanCost1)*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 55 SAY (NEActCost1 -  NELanCost1)*TOTQTY                    PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 59 SAY (NEActCost1 -  NELanCost1)*TOTQTY                    PICTURE '99999999999'
  ELSE
    @ ROW , 55 SAY (NEActCost1 -  NELanCost1)*TOTQTY                    PICTURE '99999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  lnAct_Lan= lnAct_Lan+(NEActCost1 -  NELanCost1)*TOTQTY
      
  *-- B602336 AAMER 12/14/98 (Start)
  *@ ROW , 57 SAY (NELanCost1 - PosLn.NCost1/PosHdr.Rate)*TotQty PICTURE '9999999.999'

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 57 SAY (NELanCost1 - nECost1)*TotQty PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 71 SAY (NELanCost1 - nECost1)*TotQty PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 75 SAY (NELanCost1 - nECost1)*TotQty PICTURE '99999999999'
  ELSE
    @ ROW , 71 SAY (NELanCost1 - nECost1)*TotQty PICTURE '99999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  *lnLan_Est=lnLan_Est+(NELanCost1 - PosLn.NCOST1/POSHDR.RATE)*TOTQTY
  lnLan_Est=lnLan_Est+(NELanCost1 - nECOST1)*TOTQTY
  *@ ROW , 68 SAY (NEActCost1 - PosLn.NCOST1/POSHDR.RATE)*TOTQTY PICTURE '9999999.999'

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 68 SAY (NEActCost1 - nECOST1)*TOTQTY PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 87 SAY (NEActCost1 - nECOST1)*TOTQTY PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 91 SAY (NEActCost1 - nECOST1)*TOTQTY PICTURE '99999999999'
  ELSE
    @ ROW , 87 SAY (NEActCost1 - nECOST1)*TOTQTY PICTURE '99999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]

  *lnAct_Est=lnAct_Est+(NEActCost1 - PosLn.NCOST1/POSHDR.RATE)*TOTQTY
  lnAct_Est=lnAct_Est+(NEActCost1 - NECOST1)*TOTQTY
  *-- B602336 AAMER 12/14/98 (End)

  *B603348,1 ABD Remark the next lines & change the picture & Move them to right. .[Begin]
  *@ ROW , 81 SAY IIF(NELanCost1=0,'N/A',(NEActCost1 / NELanCost1)*100)  PICTURE '9999.99'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 103 SAY IIF(NELanCost1=0,'N/A',(NEActCost1 / NELanCost1)*100)  PICTURE '9999.99'
  IF lcRpDec ='N'
    @ ROW , 106 SAY IIF(NELanCost1=0,'N/A',(NEActCost1 / NELanCost1)*100)  PICTURE '9999'
  ELSE
    @ ROW , 103 SAY IIF(NELanCost1=0,'N/A',(NEActCost1 / NELanCost1)*100)  PICTURE '9999.99'
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]


  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *-- B602336 AAMER 12/14/98 (Start)
  *@ ROW , 91 SAY IIF(PosLn.NCOST1=0,'N/A',;
    (NELanCost1 / (PosLn.NCOST1/POSHDR.RATE))*100)   PICTURE '9999.99'
  *@ ROW , 100 SAY IIF(PosLn.NCost1=0,'N/A',;
    (NEActCost1 /(PosLn.NCOST1/POSHDR.RATE))*100) PICTURE '9999.99'

  *@ ROW , 91 SAY IIF(NECOST1=0,'N/A',;
    (NELanCost1 / (NECOST1))*100)   PICTURE '9999.99'
  *@ ROW , 100 SAY IIF(NECost1=0,'N/A',;
    (NEActCost1 /(NECOST1))*100) PICTURE '9999.99'

  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 111 SAY IIF(NECOST1=0,'N/A',;
    (NELanCost1 / (NECOST1))*100)   PICTURE '9999.99'
  *@ ROW , 119 SAY IIF(NECost1=0,'N/A',;
    (NEActCost1 /(NECOST1))*100) PICTURE '9999.99'
  IF lcRpDec ='N'
    @ ROW , 114 SAY IIF(NECOST1=0,'N/A',;
      (NELanCost1 / (NECOST1))*100)   PICTURE '9999'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW , 122 SAY IIF(NECost1=0,'N/A',;
      (NEActCost1 /(NECOST1))*100) PICTURE '9999'
    @ ROW , 119 SAY IIF(NECost1=0,'N/A',;
      (NEActCost1 /(NECOST1))*100) PICTURE '9999'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ELSE
    @ ROW , 111 SAY IIF(NECOST1=0,'N/A',;
      (NELanCost1 / (NECOST1))*100)   PICTURE '9999.99'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW , 119 SAY IIF(NECost1=0,'N/A',;
      (NEActCost1 /(NECOST1))*100) PICTURE '9999.99'
    @ ROW , 115 SAY IIF(NECost1=0,'N/A',;
      (NEActCost1 /(NECOST1))*100) PICTURE '9999.99'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

  *-- B602336 AAMER 12/14/98 (End)
  
  ROW = ROW+1

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 03 SAY laCost[2]
  @ ROW , 00 SAY laCost[2]
  *B603348,1 ABD [End]

  *-- B602352 AAMER 12/14/98 (Start)
  *@ ROW , 13 SAY PosLn.NCost2 * TotQty   PICTURE '9999999.999'      
  *laSub[2,1]=laSub[2,1]+(PosLn.NCOST2*TOTQTY)
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 13 SAY NECost2 * TotQty   PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 010 SAY NECost2 * TotQty   PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 014 SAY NECost2 * TotQty   PICTURE '9999999999'
  ELSE
    @ ROW , 010 SAY NECost2 * TotQty   PICTURE '9999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  laSub[2,1]=laSub[2,1]+(NECOST2*TOTQTY)
  *-- B602352 AAMER 12/14/98 (End)
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 24 SAY NELanCost2*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 25 SAY NELanCost2*TOTQTY                    PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 29 SAY NELanCost2*TOTQTY                    PICTURE '9999999999'
  ELSE
    @ ROW , 25 SAY NELanCost2*TOTQTY                    PICTURE '9999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  laSub[2,2]=laSub[2,2]+(NELanCost2*TOTQTY)
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 35 SAY NEActCost2*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 40 SAY NEActCost2*TOTQTY                    PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 44 SAY NEActCost2*TOTQTY                   PICTURE '9999999999'
  ELSE
    *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
    *@ ROW , 40 SAY NEActCost2*TOTQTY          PICTURE '9999999999.999'
    @ ROW , 40 SAY ROUND(NEActCost2*TOTQTY,3)          PICTURE '9999999999.999'
    *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
  *laSub[2,3]=laSub[2,3]+(NEActCost2*TOTQTY)
  laSub[2,3]=laSub[2,3]+ROUND((NEActCost2*TOTQTY),3)
  *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]
 
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 46 SAY (NEActCost2 -  NELanCost2)*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 55 SAY (NEActCost2 -  NELanCost2)*TOTQTY                    PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 59 SAY (NEActCost2 -  NELanCost2)*TOTQTY                    PICTURE '99999999999'
  ELSE
    @ ROW , 55 SAY (NEActCost2 -  NELanCost2)*TOTQTY                    PICTURE '99999999999.999'
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
 
  lnAct_Lan= lnAct_Lan+(NEActCost1 -  NELanCost2)*TOTQTY
      
  *-- B602352 AAMER 12/14/98 (Start)
  *@ ROW , 57 SAY (NELanCost2 - PosLn.NCOST2)*TOTQTY PICTURE '9999999.999'
  *lnLan_Est=lnLan_Est+(NELanCost2 - PosLn.NCost2)*TOTQTY
  *@ ROW , 68 SAY (NEActCost2 - PosLn.NCOST2)*TOTQTY PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(NEActCost2 - PosLn.NCOST2)*TOTQTY
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 57 SAY (NELanCost2 - NECOST2)*TOTQTY PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 71 SAY (NELanCost2 - NECOST2)*TOTQTY PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 75 SAY (NELanCost2 - NECOST2)*TOTQTY PICTURE '99999999999'
  ELSE
    @ ROW , 71 SAY (NELanCost2 - NECOST2)*TOTQTY PICTURE '99999999999.999'
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  lnLan_Est=lnLan_Est+(NELanCost2 - NECost2)*TOTQTY
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 68 SAY (NEActCost2 - NECOST2)*TOTQTY PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 87 SAY (NEActCost2 - NECOST2)*TOTQTY PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 91 SAY (NEActCost2 - NECOST2)*TOTQTY PICTURE '99999999999'
  ELSE
    @ ROW , 87 SAY (NEActCost2 - NECOST2)*TOTQTY PICTURE '99999999999.999'
  ENDIF  
 *B803594,1  MHM 09/10/2000 [END]
 *B603348,1 ABD [End]
  
  lnAct_Est=lnAct_Est+(NEActCost2 - NECOST2)*TOTQTY
  *-- B602352 AAMER 12/14/98 (End)
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 81 SAY IIF(NELanCost2=0,'N/A',(NEActCost2 / NELanCost2)*100)  PICTURE '9999.99'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 103 SAY IIF(NELanCost2=0,'N/A',(NEActCost2 / NELanCost2)*100)  PICTURE '9999.99'
  IF lcRpDec ='N'
    @ ROW , 106 SAY IIF(NELanCost2=0,'N/A',(NEActCost2 / NELanCost2)*100)  PICTURE '9999'
  ELSE
    @ ROW , 103 SAY IIF(NELanCost2=0,'N/A',(NEActCost2 / NELanCost2)*100)  PICTURE '9999.99'
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  *-- B602352 AAMER 12/14/98 (Start)
  *@ ROW , 91 SAY IIF(PosLn.NCOST2=0,'N/A',(NELanCost2 / PosLn.NCOST2)*100)     PICTURE '9999.99'
  *@ ROW , 100 SAY IIF(PosLn.NCOST2=0,'N/A',(NEActCost2 / PosLn.NCOST2)*100)   PICTURE '9999.99'

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 91 SAY IIF(NECOST2=0,'N/A',(NELanCost2 / NECOST2)*100)     PICTURE '9999.99'
  *@ ROW , 100 SAY IIF(NECOST2=0,'N/A',(NEActCost2 / NECOST2)*100)   PICTURE '9999.99'

  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 111 SAY IIF(NECOST2=0,'N/A',(NELanCost2 / NECOST2)*100)     PICTURE '9999.99'
  *@ ROW , 119 SAY IIF(NECOST2=0,'N/A',(NEActCost2 / NECOST2)*100)   PICTURE '9999.99'
  IF lcRpDec ='N'
    @ ROW , 114 SAY IIF(NECOST2=0,'N/A',(NELanCost2 / NECOST2)*100)     PICTURE '9999'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW , 122 SAY IIF(NECOST2=0,'N/A',(NEActCost2 / NECOST2)*100)   PICTURE '9999'
    @ ROW , 119 SAY IIF(NECOST2=0,'N/A',(NEActCost2 / NECOST2)*100)   PICTURE '9999'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ELSE
    @ ROW , 111 SAY IIF(NECOST2=0,'N/A',(NELanCost2 / NECOST2)*100)     PICTURE '9999.99'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW , 119 SAY IIF(NECOST2=0,'N/A',(NEActCost2 / NECOST2)*100)   PICTURE '9999.99'
    @ ROW , 115 SAY IIF(NECOST2=0,'N/A',(NEActCost2 / NECOST2)*100)   PICTURE '9999.99'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  *-- B602352 AAMER 12/14/98 (End)
  
  ROW = ROW+1

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 03 SAY laCost[3]
  *@ ROW , 13 SAY NECost3*TotQty  PICTURE '9999999.999'
  @ ROW , 00 SAY laCost[3]
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 10 SAY NECost3*TotQty  PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 14 SAY NECost3*TotQty  PICTURE '9999999999'
  ELSE
    @ ROW , 10 SAY NECost3*TotQty  PICTURE '9999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  laSub[3,1]=laSub[3,1]+(NECOST3*TOTQTY)

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 24 SAY NELanCost3*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 25 SAY NELanCost3*TOTQTY                    PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 29 SAY NELanCost3*TOTQTY                    PICTURE '9999999999'
  ELSE
    @ ROW , 25 SAY NELanCost3*TOTQTY                    PICTURE '9999999999.999'
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  laSub[3,2]=laSub[3,2]+(NELanCost3*TOTQTY)
   
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 35 SAY NEActCost3*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 40 SAY NEActCost3*TOTQTY                    PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 44 SAY NEActCost3*TOTQTY                    PICTURE '9999999999'
  ELSE
    *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
    *@ ROW , 40 SAY NEActCost3*TOTQTY           PICTURE '9999999999.999'
    @ ROW , 40 SAY ROUND(NEActCost3*TOTQTY,3)           PICTURE '9999999999.999'
    *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
  *laSub[3,3]=laSub[3,3]+(NEActCost3*TOTQTY)
  laSub[3,3]=laSub[3,3]+ROUND((NEActCost3*TOTQTY),3)
  *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 46 SAY (NEActCost3 -  NELanCost3)*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 55 SAY (NEActCost3 -  NELanCost3)*TOTQTY                    PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 59 SAY (NEActCost3 -  NELanCost3)*TOTQTY                    PICTURE '99999999999'
  ELSE
    @ ROW , 55 SAY (NEActCost3 -  NELanCost3)*TOTQTY                    PICTURE '99999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  lnAct_Lan= lnAct_Lan+(NEActCost1 -  NELanCost3)*TOTQTY

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 57 SAY (NELanCost3 - NECOST3)*TOTQTY PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 71 SAY (NELanCost3 - NECOST3)*TOTQTY PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 75 SAY (NELanCost3 - NECOST3)*TOTQTY PICTURE '99999999999'
  ELSE
    @ ROW , 71 SAY (NELanCost3 - NECOST3)*TOTQTY PICTURE '99999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
    
  lnLan_Est=lnLan_Est+(NELanCost3 - NECOST3)*TOTQTY      
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 68 SAY (NEActCost3 - NECOST3)*TOTQTY PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 87 SAY (NEActCost3 - NECOST3)*TOTQTY PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 91 SAY (NEActCost3 - NECOST3)*TOTQTY PICTURE '99999999999'
  ELSE
    @ ROW , 87 SAY (NEActCost3 - NECOST3)*TOTQTY PICTURE '99999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  lnAct_Est=lnAct_Est+(NEActCost3 - NECOST3)*TOTQTY

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 81 SAY IIF(NELanCost3=0,'N/A',(NEActCost3 / NELanCost3)*100)  PICTURE '9999.99'
  *@ ROW , 91 SAY IIF(NECOST3=0,'N/A',(NELanCost3 / NECOST3)*100)     PICTURE '9999.99'
  *@ ROW , 100 SAY IIF(NECOST3=0,'N/A',(NEActCost3 / NECOST3)*100) PICTURE '9999.99'

  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 102 SAY IIF(NELanCost3=0,' N/A',(NEActCost3 / NELanCost3)*100)  PICTURE '9999.99'
  *@ ROW , 111 SAY IIF(NECOST3=0,'N/A',(NELanCost3 / NECOST3)*100)     PICTURE '9999.99'
  *@ ROW , 119 SAY IIF(NECOST3=0,'N/A',(NEActCost3 / NECOST3)*100) PICTURE '9999.99'
  IF lcRpDec ='N'
    @ ROW , 105 SAY IIF(NELanCost3=0,' N/A',(NEActCost3 / NELanCost3)*100)  PICTURE '9999'
    @ ROW , 114 SAY IIF(NECOST3=0,'N/A',(NELanCost3 / NECOST3)*100)     PICTURE '9999'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW , 122 SAY IIF(NECOST3=0,'N/A',(NEActCost3 / NECOST3)*100) PICTURE '9999'
    @ ROW , 119 SAY IIF(NECOST3=0,'N/A',(NEActCost3 / NECOST3)*100) PICTURE '9999'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ELSE
    @ ROW , 103 SAY IIF(NELanCost3=0,' N/A',(NEActCost3 / NELanCost3)*100)  PICTURE '9999.99'
    @ ROW , 111 SAY IIF(NECOST3=0,'N/A',(NELanCost3 / NECOST3)*100)     PICTURE '9999.99'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW , 119 SAY IIF(NECOST3=0,'N/A',(NEActCost3 / NECOST3)*100) PICTURE '9999.99'
    @ ROW , 115 SAY IIF(NECOST3=0,'N/A',(NEActCost3 / NECOST3)*100) PICTURE '9999.99'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]

  ROW = ROW+1

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 03 SAY laCost[4]
  @ ROW , 00 SAY laCost[4]
  *B603348,1 ABD [End]

  *-- B602352 AAMER 12/14/98 (Start)
  *@ ROW , 13 SAY PosLn.NCost4*TotQty  PICTURE '9999999.999'      
  *laSub[4,1]=laSub[4,1]+(PosLn.NCOST4*TOTQTY)
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 13 SAY NECost4*TotQty  PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 10 SAY NECost4*TotQty  PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 14 SAY NECost4*TotQty  PICTURE '9999999999'
  ELSE
    @ ROW , 10 SAY NECost4*TotQty  PICTURE '9999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  laSub[4,1]=laSub[4,1]+(NECOST4*TOTQTY)
  *-- B602352 AAMER 12/14/98 (End) 

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 24 SAY NELanCost4*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 25 SAY NELanCost4*TOTQTY                    PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 29 SAY NELanCost4*TOTQTY                    PICTURE '9999999999'
  ELSE
    @ ROW , 25 SAY NELanCost4*TOTQTY                    PICTURE '9999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  laSub[4,2]=laSub[4,2]+(NELanCost4*TOTQTY)
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 35 SAY NEActCost4*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 40 SAY NEActCost4*TOTQTY                    PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 44 SAY NEActCost4*TOTQTY                    PICTURE '9999999999'
  ELSE
    *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
    *@ ROW , 40 SAY NEActCost4*TOTQTY           PICTURE '9999999999.999'
    @ ROW , 40 SAY ROUND(NEActCost4*TOTQTY,3)           PICTURE '9999999999.999'
    *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
  *laSub[4,3]=laSub[4,3]+(NEActCost4*TOTQTY)
  laSub[4,3]=laSub[4,3]+ROUND((NEActCost4*TOTQTY),3)
  *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 46 SAY (NEActCost4 -  NELanCost4)*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 55 SAY (NEActCost4 -  NELanCost4)*TOTQTY                    PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 59 SAY (NEActCost4 -  NELanCost4)*TOTQTY                    PICTURE '99999999999'
  ELSE
    @ ROW , 55 SAY (NEActCost4 -  NELanCost4)*TOTQTY                    PICTURE '99999999999.999'
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  lnAct_Lan= lnAct_Lan+(NEActCost1 -  NELanCost4)*TOTQTY

  *-- B602352 AAMER 12/14/98 (Start)
  *@ ROW , 57 SAY (NELanCost4 - PosLn.NCOST4)*TOTQTY PICTURE '9999999.999'
  *lnLan_Est=lnLan_Est+(NELanCost4 - PosLn.NCOST4)*TOTQTY
  *@ ROW , 68 SAY (NEActCost4 - PosLn.NCOST4)*TOTQTY PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(NEActCost4 - NCOST4)*TOTQTY

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 57 SAY (NELanCost4 - NECOST4)*TOTQTY PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 71 SAY (NELanCost4 - NECOST4)*TOTQTY PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 75 SAY (NELanCost4 - NECOST4)*TOTQTY PICTURE '99999999999'
  ELSE
    @ ROW , 71 SAY (NELanCost4 - NECOST4)*TOTQTY PICTURE '99999999999.999'
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]

  lnLan_Est=lnLan_Est+(NELanCost4 - NECOST4)*TOTQTY
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 68 SAY (NEActCost4 - NECOST4)*TOTQTY PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 87 SAY (NEActCost4 - NECOST4)*TOTQTY PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 91 SAY (NEActCost4 - NECOST4)*TOTQTY PICTURE '99999999999'
  ELSE
    @ ROW , 87 SAY (NEActCost4 - NECOST4)*TOTQTY PICTURE '99999999999.999'
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  lnAct_Est=lnAct_Est+(NEActCost4 - NECOST4)*TOTQTY
  *-- B602352 AAMER 12/14/98 (End)

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 81 SAY IIF(NELanCost4=0,'N/A',(NEActCost4 / NELanCost4)*100)  PICTURE '9999.99'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 103 SAY IIF(NELanCost4=0,'N/A',(NEActCost4 / NELanCost4)*100)  PICTURE '9999.99'
  IF lcRpDec ='N'
    @ ROW , 106 SAY IIF(NELanCost4=0,'N/A',(NEActCost4 / NELanCost4)*100)  PICTURE '9999'
  ELSE
    @ ROW , 103 SAY IIF(NELanCost4=0,'N/A',(NEActCost4 / NELanCost4)*100)  PICTURE '9999.99'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  *-- B602352 AAMER 12/14/98 (Start)
  *@ ROW , 91 SAY IIF(PosLn.NCOST4=0,'N/A',(NELanCost4 / PosLn.NCOST4)*100) PICTURE '9999.99'
  *@ ROW , 100 SAY IIF(PosLn.NCOST4=0,'N/A',(NEActCost4 / PosLn.NCOST4)*100) PICTURE '9999.99'
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 91 SAY IIF(NECOST4=0,'N/A',(NELanCost4 / NECOST4)*100) PICTURE '9999.99'
  *@ ROW , 100 SAY IIF(NECOST4=0,'N/A',(NEActCost4 / NECOST4)*100) PICTURE '9999.99'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 111 SAY IIF(NECOST4=0,'N/A',(NELanCost4 / NECOST4)*100) PICTURE '9999.99'
  *@ ROW , 119 SAY IIF(NECOST4=0,'N/A',(NEActCost4 / NECOST4)*100) PICTURE '9999.99'
  IF lcRpDec ='N'
    @ ROW , 114 SAY IIF(NECOST4=0,'N/A',(NELanCost4 / NECOST4)*100) PICTURE '9999'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW , 122 SAY IIF(NECOST4=0,'N/A',(NEActCost4 / NECOST4)*100) PICTURE '9999'
    @ ROW , 119 SAY IIF(NECOST4=0,'N/A',(NEActCost4 / NECOST4)*100) PICTURE '9999'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ELSE
    @ ROW , 111 SAY IIF(NECOST4=0,'N/A',(NELanCost4 / NECOST4)*100) PICTURE '9999.99'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW , 119 SAY IIF(NECOST4=0,'N/A',(NEActCost4 / NECOST4)*100) PICTURE '9999.99'
    @ ROW , 115 SAY IIF(NECOST4=0,'N/A',(NEActCost4 / NECOST4)*100) PICTURE '9999.99'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  *-- B602352 AAMER 12/14/98 (End)

  ROW = ROW+1

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 03 SAY laCost[5]
  *@ ROW , 13 SAY NECost5*TotQty  PICTURE '9999999.999'  
  @ ROW , 00 SAY laCost[5]
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 10 SAY NECost5*TotQty  PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 14 SAY NECost5*TotQty  PICTURE '9999999999'
  ELSE
    @ ROW , 10 SAY NECost5*TotQty  PICTURE '9999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]

  laSub[5,1] =laSub[5,1]+ (NECOST5*TOTQTY)

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 24 SAY NELanCost5*TOTQTY                PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 25 SAY NELanCost5*TOTQTY                PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 29 SAY NELanCost5*TOTQTY                PICTURE '9999999999'
  ELSE
    @ ROW , 25 SAY NELanCost5*TOTQTY                PICTURE '9999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  laSub[5,2] =laSub[5,2]+(NELanCost5*TOTQTY)
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 35 SAY NEActCost5*TOTQTY                PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 40 SAY NEActCost5*TOTQTY                PICTURE '9999999999.999'
  IF lcRpDec ='N'
    @ ROW , 44 SAY NEActCost5*TOTQTY                PICTURE '9999999999'
  ELSE
    *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
    *@ ROW , 40 SAY NEActCost5*TOTQTY           PICTURE '9999999999.999'
    @ ROW , 40 SAY ROUND(NEActCost5*TOTQTY,3)           PICTURE '9999999999.999'
    *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
  *laSub[5,3] =laSub[5,3]+(NEActCost5*TOTQTY)
  laSub[5,3] =laSub[5,3]+ROUND((NEActCost5*TOTQTY),3)
  *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]

  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 46 SAY (NEActCost5 -  NELanCost5)*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 55 SAY (NEActCost5 -  NELanCost5)*TOTQTY                    PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 59 SAY (NEActCost5 -  NELanCost5)*TOTQTY                    PICTURE '99999999999'
  ELSE
    @ ROW , 55 SAY (NEActCost5 -  NELanCost5)*TOTQTY                    PICTURE '99999999999.999'
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  lnAct_Lan= lnAct_Lan+(NEActCost1 -  NELanCost5)*TOTQTY

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 57 SAY (NELanCost5 - NECOST5)*TOTQTY PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 71 SAY (NELanCost5 - NECOST5)*TOTQTY PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 75 SAY (NELanCost5 - NECOST5)*TOTQTY PICTURE '99999999999'
  ELSE
    @ ROW , 71 SAY (NELanCost5 - NECOST5)*TOTQTY PICTURE '99999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]

  lnLan_Est=lnLan_Est+(NELanCost5 - NECOST5)*TOTQTY
  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 68 SAY (NEActCost5 - NECOST5)*TOTQTY PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 87 SAY (NEActCost5 - NECOST5)*TOTQTY PICTURE '99999999999.999'
  IF lcRpDec ='N'
    @ ROW , 91 SAY (NEActCost5 - NECOST5)*TOTQTY PICTURE '99999999999'
  ELSE
    @ ROW , 87 SAY (NEActCost5 - NECOST5)*TOTQTY PICTURE '99999999999.999'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  lnAct_Est=lnAct_Est+(NEActCost5 - NECOST5)*TOTQTY

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 81 SAY IIF(NELanCost5=0,'N/A',(NEActCost5 / NELanCost5)*100)  PICTURE '9999.99'
  *@ ROW , 91 SAY IIF(NECOST5=0,'N/A',(NELanCost5 / NECOST5)*100) PICTURE '9999.99'
  *@ ROW , 100 SAY IIF(NECOST5=0,'N/A',(NEActCost5 / NECOST5)*100)   PICTURE '9999.99'

  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 103 SAY IIF(NELanCost5=0,'N/A',(NEActCost5 / NELanCost5)*100)  PICTURE '9999.99'
  *@ ROW , 111 SAY IIF(NECOST5=0,'N/A',(NELanCost5 / NECOST5)*100) PICTURE '9999.99'
  *@ ROW , 119 SAY IIF(NECOST5=0,'N/A',(NEActCost5 / NECOST5)*100)   PICTURE '9999.99'
  IF lcRpDec ='N'
    @ ROW , 106 SAY IIF(NELanCost5=0,'N/A',(NEActCost5 / NELanCost5)*100)  PICTURE '9999'
    @ ROW , 114 SAY IIF(NECOST5=0,'N/A',(NELanCost5 / NECOST5)*100) PICTURE '9999'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW , 122 SAY IIF(NECOST5=0,'N/A',(NEActCost5 / NECOST5)*100)   PICTURE '9999'
    @ ROW , 119 SAY IIF(NECOST5=0,'N/A',(NEActCost5 / NECOST5)*100)   PICTURE '9999'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ELSE
    @ ROW , 103 SAY IIF(NELanCost5=0,'N/A',(NEActCost5 / NELanCost5)*100)  PICTURE '9999.99'
    @ ROW , 111 SAY IIF(NECOST5=0,'N/A',(NELanCost5 / NECOST5)*100) PICTURE '9999.99'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW , 119 SAY IIF(NECOST5=0,'N/A',(NEActCost5 / NECOST5)*100)   PICTURE '9999.99''
    @ ROW , 115 SAY IIF(NECOST5=0,'N/A',(NEActCost5 / NECOST5)*100)   PICTURE '9999.99'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
    
  ROW = ROW + 1
  
  *B603348,1 ABD Remark the next lines & Move them to right. .[Begin]
  *@ ROW ,14 SAY '---------- ----------  --------- ----------  --------- ----------  -------   ------- --------'
  @ ROW ,14 SAY '----------     ----------      ---------      ----------       ---------      ---------- ------- ------- --------'
  *B603348,1 ABD [End]
  
  ROW = ROW + 1

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 03 SAY 'Total Cost'
  @ ROW , 00 SAY 'Total Cost'
  *B603348,1 ABD [End]
  
  *-- B602336 AAMER 12/14/98 (Start)
  *@ ROW , 13 SAY (PosLn.NCOST1/POSHDR.RATE+PosLn.NCOST2+PosLn.NCOST3+;
                  PosLn.NCOST4+PosLn.NCOST5)*TOTQTY PICTURE '9999999.999'
                  
  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 13 SAY (NECOST1+NECOST2+NECOST3+;
                  NECOST4+NECOST5)*TOTQTY PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 10 SAY (NECOST1+NECOST2+NECOST3+;
                  NECOST4+NECOST5)*TOTQTY PICTURE '99999999999.99'
  IF lcRpDec ='N'
    @ ROW , 13 SAY (NECOST1+NECOST2+NECOST3+;
                    NECOST4+NECOST5)*TOTQTY PICTURE '99999999999'
  ELSE
    @ ROW , 10 SAY (NECOST1+NECOST2+NECOST3+;
                    NECOST4+NECOST5)*TOTQTY PICTURE '99999999999.99'
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]

  *lnEst = (PosLn.NCOST1/POSHDR.RATE+PosLn.NCOST2+PosLn.NCOST3+;
           PosLn.NCOST4+PosLn.NCOST5)*TOTQTY
  lnEst = (NECOST1+NECOST2+NECOST3+;
           NECOST4+NCOST5)*TOTQTY
  *-- B602336 AAMER 12/14/98 (End) 
  

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 24 SAY (NELanCost1+NELanCost2+NELanCost3+NELanCost4+NELanCost5)*TOTQTY    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 25 SAY (NELanCost1+NELanCost2+NELanCost3+NELanCost4+NELanCost5)*TOTQTY    PICTURE '99999999999.99'
  IF lcRpDec ='N'
    @ ROW , 28 SAY (NELanCost1+NELanCost2+NELanCost3+NELanCost4+NELanCost5)*TOTQTY    PICTURE '99999999999'
  ELSE
    @ ROW , 25 SAY (NELanCost1+NELanCost2+NELanCost3+NELanCost4+NELanCost5)*TOTQTY    PICTURE '99999999999.99'
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  
  lnLand = lnLand + (NELanCost1+NELanCost2+NELanCost3+NELanCost4+NELanCost5)*TOTQTY

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 35 SAY (NEActCost1+NEActCost2+NEActCost3+NEActCost4+NEActCost5)*TOTQTY                    PICTURE '9999999.999'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 40 SAY (NEActCost1+NEActCost2+NEActCost3+NEActCost4+NEActCost5)*TOTQTY           PICTURE '99999999999.99'
  IF lcRpDec ='N'
    @ ROW , 43 SAY (NEActCost1+NEActCost2+NEActCost3+NEActCost4+NEActCost5)*TOTQTY          PICTURE '99999999999'
  ELSE
    *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
    *@ ROW , 40 SAY (NEActCost1+NEActCost2+NEActCost3+NEActCost4+NEActCost5)*TOTQTY         PICTURE '99999999999.99'
    @ ROW , 40 SAY ROUND((NEActCost1+NEActCost2+NEActCost3+NEActCost4+NEActCost5)*TOTQTY,3) PICTURE '99999999999.99'
    *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]
  *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
  *lnAcT  = lnAcT + (NEActCost1+NEActCost2+NEActCost3+NEActCost4+NEActCost5)*TOTQTY
  lnAcT  = lnAcT + ROUND((NEActCost1+NEActCost2+NEActCost3+NEActCost4+NEActCost5)*TOTQTY,3)
  *B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]

  *B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
  *@ ROW , 46 SAY lnAct -lnLand                                                PICTURE '9999999.999'
  *@ ROW , 57 SAY lnLand -lnEst                                                PICTURE '9999999.999'
  *@ ROW , 68 SAY lnAct - lnEst                                                PICTURE '9999999.999'
  *@ ROW , 81 SAY IIF(lnLand=0,'N/A',(lnAcT / lnLand)*100)                 PICTURE '9999.99'
  *@ ROW , 91 SAY IIF(lnEst=0,'N/A',(lnLand / lnEst)*100)                  PICTURE '9999.99'
  *@ ROW , 100 SAY IIF(lnEst=0,'N/A',(lnAcT / lnEst)*100)                   PICTURE '9999.99'
  *ROW = ROW + 1
  *@ ROW ,14 SAY '========== ========== ==========  ========= ==========  =========  =======  ======== ======== '

  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW , 055 SAY lnAct -lnLand                                                PICTURE '999999999999.99'
  *@ ROW , 071 SAY lnLand -lnEst                                                PICTURE '99999999999.99'
  *@ ROW , 087 SAY lnAct - lnEst                                                PICTURE '999999999999.99'
  *@ ROW , 103 SAY IIF(lnLand=0,'N/A',(lnAcT / lnLand)*100)                 PICTURE '9999.99'
  *@ ROW , 111 SAY IIF(lnEst=0,'N/A',(lnLand / lnEst)*100)                  PICTURE '9999.99'
  *@ ROW , 119 SAY IIF(lnEst=0,'N/A',(lnAcT / lnEst)*100)                   PICTURE '9999.99'
  IF lcRpDec ='N'

    @ ROW , 058 SAY lnAct -lnLand                                                PICTURE '999999999999'
    @ ROW , 075 SAY lnLand -lnEst                                                PICTURE '99999999999'
    @ ROW , 090 SAY lnAct - lnEst                                                PICTURE '999999999999'
    @ ROW , 106 SAY IIF(lnLand=0,'N/A',(lnAcT / lnLand)*100)                 PICTURE '9999'
    @ ROW , 114 SAY IIF(lnEst=0,'N/A',(lnLand / lnEst)*100)                  PICTURE '9999'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW , 122 SAY IIF(lnEst=0,'N/A',(lnAcT / lnEst)*100)                   PICTURE '9999'
    @ ROW , 119 SAY IIF(lnEst=0,'N/A',(lnAcT / lnEst)*100)                   PICTURE '9999'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ELSE
    @ ROW , 055 SAY lnAct -lnLand                                                PICTURE '999999999999.99'
    @ ROW , 071 SAY lnLand -lnEst                                                PICTURE '99999999999.99'
    @ ROW , 087 SAY lnAct - lnEst                                                PICTURE '999999999999.99'
    @ ROW , 103 SAY IIF(lnLand=0,'N/A',(lnAcT / lnLand)*100)                 PICTURE '9999.99'
    @ ROW , 111 SAY IIF(lnEst=0,'N/A',(lnLand / lnEst)*100)                  PICTURE '9999.99'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW , 119 SAY IIF(lnEst=0,'N/A',(lnAcT / lnEst)*100)                   PICTURE '9999.99'
    @ ROW , 115 SAY IIF(lnEst=0,'N/A',(lnAcT / lnEst)*100)                   PICTURE '9999.99'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  ROW = ROW + 1
  @ ROW ,14 SAY '==========     ==========      =========      ==========       =========      ========== ======= ======= ========'
  *B603348,1 ABD [End]

  FOR I = 1 TO 5
    H= STR(I,1)
    laSEST[I] = laSEST[I] + (NCOST&H+TOTQTY)
    laEST[I]  = laEST[I]  + NCOST&H
  ENDFOR

  FOR I = 1 TO 5
    H= STR(I,1)
    laSACT[I] = laSACT[I] + NEActCost&H
    laACT[I]  = laACT[I]  + NEActCost&H
  ENDFOR

  FOR I = 1 TO 5
    H= STR(I,1)
    laSLAN[I] = laSLAN[I] + NELanCost&H
    laLAN[I]  = laLAN[I]  + NELanCost&H
  ENDFOR
  STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est ,lnAct ,lnLand ,lnEst
  *B602886,1 AMM Subtract return PO's quantities
  *XTOTAL(1,1)=XTOTAL(1,1)+QTY1
  *XTOTAL(1,2)=XTOTAL(1,2)+QTY2
  *XTOTAL(1,3)=XTOTAL(1,3)+QTY3
  *XTOTAL(1,4)=XTOTAL(1,4)+QTY4
  *XTOTAL(1,5)=XTOTAL(1,5)+QTY5
  *XTOTAL(1,6)=XTOTAL(1,6)+QTY6
  *XTOTAL(1,7)=XTOTAL(1,7)+QTY7
  *XTOTAL(1,8)=XTOTAL(1,8)+QTY8
  *XTOTAL(1,9)=XTOTAL(1,9)+TOTQTY
  
  XTOTAL(1,1)=XTOTAL(1,1)+IIF(cStyType = 'R',-QTY1,QTY1)
  XTOTAL(1,2)=XTOTAL(1,2)+IIF(cStyType = 'R',-QTY2,QTY2)
  XTOTAL(1,3)=XTOTAL(1,3)+IIF(cStyType = 'R',-QTY3,QTY3)
  XTOTAL(1,4)=XTOTAL(1,4)+IIF(cStyType = 'R',-QTY4,QTY4)
  XTOTAL(1,5)=XTOTAL(1,5)+IIF(cStyType = 'R',-QTY5,QTY5)
  XTOTAL(1,6)=XTOTAL(1,6)+IIF(cStyType = 'R',-QTY6,QTY6)
  XTOTAL(1,7)=XTOTAL(1,7)+IIF(cStyType = 'R',-QTY7,QTY7)
  XTOTAL(1,8)=XTOTAL(1,8)+IIF(cStyType = 'R',-QTY8,QTY8)
  XTOTAL(1,9)=XTOTAL(1,9)+IIF(cStyType = 'R',-TOTQTY,TOTQTY)
  *B602886,1 AMM end

  *-- B602336 AAMER 12/14/98 (Start)
  *XTOTAL(1,10)=XTOTAL(1,10)+(TOTQTY*(PRICE/POSHDR->RATE))
  *B602886,1 AMM Sum negative amount if return PO
  *XTOTAL(1,10)=XTOTAL(1,10)+(TOTQTY*(nECost1))
  XTOTAL(1,10)=XTOTAL(1,10)+IIF(cStyType='R',-1,1) * (TOTQTY*(nECost1))
  *B602886,1 AMM end
  *-- B602336 AAMER 12/14/98 (End)

  SELECT &WORKFILE
  lnTOTqty = TOTQTY
  SKIP
ENDDO
*------------------ END [MAIN REPORT] LOOP --------------------

ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW+1
@ ROW,000 SAY '* GRAND TOTAL *'
ROW = ROW+1

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 03 SAY laCost[1]
*@ ROW , 13 SAY laFinal[1,1]                              PICTURE '9999999.999'
*@ ROW , 24 SAY laFinal[1,2]                              PICTURE '9999999.999'
*@ ROW , 35 SAY laFinal[1,3]                              PICTURE '9999999.999'
*@ ROW , 46 SAY laFinal[1,3]-laFinal[1,2]                 PICTURE '9999999.999'

*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 00 SAY laCost[1]
*@ ROW , 10 SAY laFinal[1,1]                              PICTURE '9999999999.999'
*@ ROW , 25 SAY laFinal[1,2]                              PICTURE '9999999999.999'
*@ ROW , 40 SAY laFinal[1,3]                              PICTURE '9999999999.999'
*@ ROW , 55 SAY laFinal[1,3]-laFinal[1,2]                 PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 04 SAY laCost[1]
  @ ROW , 14 SAY laFinal[1,1]                              PICTURE '9999999999'
  @ ROW , 29 SAY laFinal[1,2]                              PICTURE '9999999999'
  @ ROW , 44 SAY laFinal[1,3]                              PICTURE '9999999999'
  @ ROW , 59 SAY laFinal[1,3]-laFinal[1,2]                 PICTURE '99999999999'
ELSE
  @ ROW , 00 SAY laCost[1]
  @ ROW , 10 SAY laFinal[1,1]                              PICTURE '9999999999.999'
  @ ROW , 25 SAY laFinal[1,2]                              PICTURE '9999999999.999'
  @ ROW , 40 SAY laFinal[1,3]                              PICTURE '9999999999.999'
  @ ROW , 55 SAY laFinal[1,3]-laFinal[1,2]                 PICTURE '99999999999.999'
ENDIF  
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnAct_Lan= lnAct_Lan+(laAct[1] -  lalan[1])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 57 SAY laFinal[1,2] -laFinal[1,1]                PICTURE '9999999.999'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 71 SAY laFinal[1,2] -laFinal[1,1]                PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 75 SAY laFinal[1,2] -laFinal[1,1]                PICTURE '99999999999'
ELSE
  @ ROW , 71 SAY laFinal[1,2] -laFinal[1,1]                PICTURE '99999999999.999'
ENDIF  
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnLan_Est=lnLan_Est+(lalan[1] - laEst[1])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 68 SAY laFinal[1,3] -laFinal[1,1]                PICTURE '9999999.999'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 87 SAY laFinal[1,3] -laFinal[1,1]                PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 91 SAY laFinal[1,3] -laFinal[1,1]                PICTURE '99999999999'
ELSE
  @ ROW , 87 SAY laFinal[1,3] -laFinal[1,1]                PICTURE '99999999999.999'
ENDIF  
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnAct_Est=lnAct_Est+(laAct[1] - laEst[1])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 81 SAY IIF(laFinal[1,2]=0,'N/A',(laFinal[1,3] / laFinal[1,2])*100)     PICTURE '9999.99'
*@ ROW , 91 SAY IIF(laFinal[1,1]=0,'N/A',(laFinal[1,2] / laFinal[1,1])*100)     PICTURE '9999.99'
*@ ROW , 100 SAY IIF(laFinal[1,1]=0,'N/A',(laFinal[1,3] / laFinal[1,1])*100)    PICTURE '9999.99'

*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 103 SAY IIF(laFinal[1,2]=0,'N/A',(laFinal[1,3] / laFinal[1,2])*100)     PICTURE '9999.99'
*@ ROW , 111 SAY IIF(laFinal[1,1]=0,'N/A',(laFinal[1,2] / laFinal[1,1])*100)     PICTURE '9999.99'
*@ ROW , 119 SAY IIF(laFinal[1,1]=0,'N/A',(laFinal[1,3] / laFinal[1,1])*100)    PICTURE '9999.99'
IF lcRpDec ='N'
  @ ROW , 106 SAY IIF(laFinal[1,2]=0,'N/A',(laFinal[1,3] / laFinal[1,2])*100)     PICTURE '9999'
  @ ROW , 114 SAY IIF(laFinal[1,1]=0,'N/A',(laFinal[1,2] / laFinal[1,1])*100)     PICTURE '9999'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
  *@ ROW , 122 SAY IIF(laFinal[1,1]=0,'N/A',(laFinal[1,3] / laFinal[1,1])*100)    PICTURE '9999'
  @ ROW , 119 SAY IIF(laFinal[1,1]=0,'N/A',(laFinal[1,3] / laFinal[1,1])*100)    PICTURE '9999'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
ELSE
  @ ROW , 103 SAY IIF(laFinal[1,2]=0,'N/A',(laFinal[1,3] / laFinal[1,2])*100)     PICTURE '9999.99'
  @ ROW , 111 SAY IIF(laFinal[1,1]=0,'N/A',(laFinal[1,2] / laFinal[1,1])*100)     PICTURE '9999.99'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
  *@ ROW , 119 SAY IIF(laFinal[1,1]=0,'N/A',(laFinal[1,3] / laFinal[1,1])*100)    PICTURE '9999.99'
  @ ROW , 115 SAY IIF(laFinal[1,1]=0,'N/A',(laFinal[1,3] / laFinal[1,1])*100)    PICTURE '9999.99'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

ROW = ROW + 1

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 03 SAY laCost[2]
*@ ROW , 13 SAY laFinal[2,1]                              PICTURE '9999999.999'
*@ ROW , 24 SAY laFinal[2,2]                              PICTURE '9999999.999'
*@ ROW , 35 SAY laFinal[2,3]                              PICTURE '9999999.999'
*@ ROW , 46 SAY laFinal[2,3]-laFinal[2,2]                 PICTURE '9999999.999'

*@ ROW , 00 SAY laCost[2]
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 10 SAY laFinal[2,1]                              PICTURE '9999999999.999'
*@ ROW , 25 SAY laFinal[2,2]                              PICTURE '9999999999.999'
*@ ROW , 40 SAY laFinal[2,3]                              PICTURE '9999999999.999'
*@ ROW , 55 SAY laFinal[2,3]-laFinal[2,2]                 PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 04 SAY laCost[2]
  @ ROW , 14 SAY laFinal[2,1]                              PICTURE '9999999999'
  @ ROW , 29 SAY laFinal[2,2]                              PICTURE '9999999999'
  @ ROW , 44 SAY laFinal[2,3]                              PICTURE '9999999999'
  @ ROW , 59 SAY laFinal[2,3]-laFinal[2,2]                 PICTURE '99999999999'
ELSE
  @ ROW , 00 SAY laCost[2]
  @ ROW , 10 SAY laFinal[2,1]                              PICTURE '9999999999.999'
  @ ROW , 25 SAY laFinal[2,2]                              PICTURE '9999999999.999'
  @ ROW , 40 SAY laFinal[2,3]                              PICTURE '9999999999.999'
  @ ROW , 55 SAY laFinal[2,3]-laFinal[2,2]                 PICTURE '99999999999.999'
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnAct_Lan= lnAct_Lan+(laAct[2] -  lalan[2])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 57 SAY laFinal[2,2] -laFinal[2,1]                PICTURE '9999999.999'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 71 SAY laFinal[2,2] -laFinal[2,1]                PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 75 SAY laFinal[2,2] -laFinal[2,1]                PICTURE '99999999999'
ELSE
  @ ROW , 71 SAY laFinal[2,2] -laFinal[2,1]                PICTURE '99999999999.999'
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnLan_Est=lnLan_Est+(lalan[2] - laEst[2])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 68 SAY laFinal[2,3] -laFinal[2,1]                PICTURE '9999999.999'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 91 SAY laFinal[2,3] -laFinal[2,1]                PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 91 SAY laFinal[2,3] -laFinal[2,1]                PICTURE '99999999999'
ELSE
  @ ROW , 87 SAY laFinal[2,3] -laFinal[2,1]                PICTURE '99999999999.999'
ENDIF  
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnAct_Est=lnAct_Est+(laAct[2] - laEst[2])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 81 SAY IIF(laFinal[2,2]=0,'N/A',(laFinal[2,3] / laFinal[2,2])*100)      PICTURE '9999.99'
*@ ROW , 91 SAY IIF(laFinal[2,1]=0,'N/A',(laFinal[2,2] / laFinal[2,1])*100)      PICTURE '9999.99'
*@ ROW , 100 SAY IIF(laFinal[2,1]=0,'N/A',(laFinal[2,3] / laFinal[2,1])*100)     PICTURE '9999.99'

*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 103 SAY IIF(laFinal[2,2]=0,'N/A',(laFinal[2,3] / laFinal[2,2])*100)      PICTURE '9999.99'
*@ ROW , 111 SAY IIF(laFinal[2,1]=0,'N/A',(laFinal[2,2] / laFinal[2,1])*100)      PICTURE '9999.99'
*@ ROW , 119 SAY IIF(laFinal[2,1]=0,'N/A',(laFinal[2,3] / laFinal[2,1])*100)     PICTURE '9999.99'
IF lcRpDec ='N'
  @ ROW , 106 SAY IIF(laFinal[2,2]=0,'N/A',(laFinal[2,3] / laFinal[2,2])*100)      PICTURE '9999'
  @ ROW , 114 SAY IIF(laFinal[2,1]=0,'N/A',(laFinal[2,2] / laFinal[2,1])*100)      PICTURE '9999'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
  *@ ROW , 122 SAY IIF(laFinal[2,1]=0,'N/A',(laFinal[2,3] / laFinal[2,1])*100)     PICTURE '9999'
  @ ROW , 119 SAY IIF(laFinal[2,1]=0,'N/A',(laFinal[2,3] / laFinal[2,1])*100)     PICTURE '9999'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
ELSE
  @ ROW , 103 SAY IIF(laFinal[2,2]=0,'N/A',(laFinal[2,3] / laFinal[2,2])*100)      PICTURE '9999.99'
  @ ROW , 111 SAY IIF(laFinal[2,1]=0,'N/A',(laFinal[2,2] / laFinal[2,1])*100)      PICTURE '9999.99'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
  *@ ROW , 119 SAY IIF(laFinal[2,1]=0,'N/A',(laFinal[2,3] / laFinal[2,1])*100)     PICTURE '9999.99'
  @ ROW , 115 SAY IIF(laFinal[2,1]=0,'N/A',(laFinal[2,3] / laFinal[2,1])*100)     PICTURE '9999.99'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

ROW= ROW+1

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 03 SAY laCost[3]
*@ ROW , 13 SAY laFinal[3,1]                              PICTURE '9999999.999'
*@ ROW , 24 SAY laFinal[3,2]                              PICTURE '9999999.999'
*@ ROW , 35 SAY laFinal[3,3]                              PICTURE '9999999.999'
*@ ROW , 46 SAY laFinal[3,3]-laFinal[3,2]                 PICTURE '9999999.999'

@ ROW , 00 SAY laCost[3]
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 10 SAY laFinal[3,1]                              PICTURE '9999999999.999'
*@ ROW , 25 SAY laFinal[3,2]                              PICTURE '9999999999.999'
*@ ROW , 40 SAY laFinal[3,3]                              PICTURE '9999999999.999'
*@ ROW , 55 SAY laFinal[3,3]-laFinal[3,2]                 PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 14 SAY laFinal[3,1]                              PICTURE '9999999999'
  @ ROW , 29 SAY laFinal[3,2]                              PICTURE '9999999999'
  @ ROW , 44 SAY laFinal[3,3]                              PICTURE '9999999999'
  @ ROW , 59 SAY laFinal[3,3]-laFinal[3,2]                 PICTURE '99999999999'
ELSE
  @ ROW , 10 SAY laFinal[3,1]                              PICTURE '9999999999.999'
  @ ROW , 25 SAY laFinal[3,2]                              PICTURE '9999999999.999'
  @ ROW , 40 SAY laFinal[3,3]                              PICTURE '9999999999.999'
  @ ROW , 55 SAY laFinal[3,3]-laFinal[3,2]                 PICTURE '99999999999.999'
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]


lnAct_Lan= lnAct_Lan+(laAct[3] -  lalan[3])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 57 SAY laFinal[3,2] -laFinal[3,1]                PICTURE '9999999.999'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 71 SAY laFinal[3,2] -laFinal[3,1]                PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 75 SAY laFinal[3,2] -laFinal[3,1]                PICTURE '99999999999'
ELSE
  @ ROW , 71 SAY laFinal[3,2] -laFinal[3,1]                PICTURE '99999999999.999'
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]


lnLan_Est=lnLan_Est+(lalan[3] - laEst[3])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 68 SAY laFinal[3,3] -laFinal[3,1]                PICTURE '9999999.999'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 87 SAY laFinal[3,3] -laFinal[3,1]                PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 91 SAY laFinal[3,3] -laFinal[3,1]                PICTURE '99999999999'
ELSE
  @ ROW , 87 SAY laFinal[3,3] -laFinal[3,1]                PICTURE '99999999999.999'
ENDIF
*--MHM
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnAct_Est=lnAct_Est+(laAct[3] - laEst[3])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 81 SAY IIF(laFinal[3,2]=0,'N/A',(laFinal[3,3] / laFinal[3,2])*100)  PICTURE '9999.99'
*@ ROW , 91 SAY IIF(laFinal[3,1]=0,'N/A',(laFinal[3,2] / laFinal[3,1])*100)  PICTURE '9999.99'
*@ ROW , 100 SAY IIF(laFinal[3,1]=0,'N/A',(laFinal[3,3] / laFinal[3,1])*100) PICTURE '9999.99'

*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 103 SAY IIF(laFinal[3,2]=0,'N/A',(laFinal[3,3] / laFinal[3,2])*100)  PICTURE '9999.99'
*@ ROW , 111 SAY IIF(laFinal[3,1]=0,'N/A',(laFinal[3,2] / laFinal[3,1])*100)  PICTURE '9999.99'
*@ ROW , 119 SAY IIF(laFinal[3,1]=0,'N/A',(laFinal[3,3] / laFinal[3,1])*100) PICTURE '9999.99'
IF lcRpDec ='N'
  @ ROW , 106 SAY IIF(laFinal[3,2]=0,'N/A',(laFinal[3,3] / laFinal[3,2])*100)  PICTURE '9999'
  @ ROW , 114 SAY IIF(laFinal[3,1]=0,'N/A',(laFinal[3,2] / laFinal[3,1])*100)  PICTURE '9999'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
  *@ ROW , 122 SAY IIF(laFinal[3,1]=0,'N/A',(laFinal[3,3] / laFinal[3,1])*100) PICTURE '9999'
  @ ROW , 119 SAY IIF(laFinal[3,1]=0,'N/A',(laFinal[3,3] / laFinal[3,1])*100) PICTURE '9999'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
ELSE
  @ ROW , 103 SAY IIF(laFinal[3,2]=0,'N/A',(laFinal[3,3] / laFinal[3,2])*100)  PICTURE '9999.99'
  @ ROW , 111 SAY IIF(laFinal[3,1]=0,'N/A',(laFinal[3,2] / laFinal[3,1])*100)  PICTURE '9999.99'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
  *@ ROW , 119 SAY IIF(laFinal[3,1]=0,'N/A',(laFinal[3,3] / laFinal[3,1])*100) PICTURE '9999.99'
  @ ROW , 115 SAY IIF(laFinal[3,1]=0,'N/A',(laFinal[3,3] / laFinal[3,1])*100) PICTURE '9999.99'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
ENDIF  
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]


ROW= ROW+1

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 03 SAY laCost[4]
*@ ROW , 13 SAY laFinal[4,1]                              PICTURE '9999999.999'
*@ ROW , 24 SAY laFinal[4,2]                              PICTURE '9999999.999'
*@ ROW , 35 SAY laFinal[4,3]                              PICTURE '9999999.999'
*@ ROW , 46 SAY laFinal[4,3]-laFinal[4,2]                 PICTURE '9999999.999'

@ ROW , 00 SAY laCost[4]
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 10 SAY laFinal[4,1]              PICTURE '9999999999.999'
*@ ROW , 25 SAY laFinal[4,2]              PICTURE '9999999999.999'
*@ ROW , 40 SAY laFinal[4,3]              PICTURE '9999999999.999'
*@ ROW , 55 SAY laFinal[4,3]-laFinal[4,2] PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 14 SAY laFinal[4,1]              PICTURE '9999999999'
  @ ROW , 29 SAY laFinal[4,2]              PICTURE '9999999999'
  @ ROW , 44 SAY laFinal[4,3]              PICTURE '9999999999'
  @ ROW , 59 SAY laFinal[4,3]-laFinal[4,2] PICTURE '99999999999'
ELSE
  @ ROW , 10 SAY laFinal[4,1]              PICTURE '9999999999.999'
  @ ROW , 25 SAY laFinal[4,2]              PICTURE '9999999999.999'
  @ ROW , 40 SAY laFinal[4,3]              PICTURE '9999999999.999'
  @ ROW , 55 SAY laFinal[4,3]-laFinal[4,2] PICTURE '99999999999.999'
ENDIF  
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnAct_Lan= lnAct_Lan+(laAct[4] -  lalan[4])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 57 SAY laFinal[4,2] -laFinal[4,1]                 PICTURE '9999999.999'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 71 SAY laFinal[4,2] -laFinal[4,1]                 PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 75 SAY laFinal[4,2] -laFinal[4,1]                 PICTURE '99999999999'
ELSE
  @ ROW , 71 SAY laFinal[4,2] -laFinal[4,1]                 PICTURE '99999999999.999'
ENDIF  
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnLan_Est=lnLan_Est+(lalan[4] - laEst[4])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 68 SAY laFinal[4,3] -laFinal[4,1]                 PICTURE '9999999.999'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 91 SAY laFinal[4,3] -laFinal[4,1]                 PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 91 SAY laFinal[4,3] -laFinal[4,1]                 PICTURE '99999999999'
ELSE
  @ ROW , 87 SAY laFinal[4,3] -laFinal[4,1]                 PICTURE '99999999999.999'
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnAct_Est=lnAct_Est+(laAct[4] - laEst[4])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 81 SAY IIF(laFinal[4,2]=0,'N/A',(laFinal[4,3] / laFinal[4,2])*100)  PICTURE '9999.99'
*@ ROW , 91 SAY IIF(laFinal[4,1]=0,'N/A',(laFinal[4,2] / laFinal[4,1])*100)  PICTURE '9999.99'
*@ ROW , 100 SAY IIF(laFinal[4,1]=0,'N/A',(laFinal[4,3] / laFinal[4,1])*100) PICTURE '9999.99'

*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 103 SAY IIF(laFinal[4,2]=0,'N/A',(laFinal[4,3] / laFinal[4,2])*100)  PICTURE '9999.99'
*@ ROW , 111 SAY IIF(laFinal[4,1]=0,'N/A',(laFinal[4,2] / laFinal[4,1])*100)  PICTURE '9999.99'
*@ ROW , 119 SAY IIF(laFinal[4,1]=0,'N/A',(laFinal[4,3] / laFinal[4,1])*100) PICTURE '9999.99'
IF lcRpDec ='N'
  @ ROW , 106 SAY IIF(laFinal[4,2]=0,'N/A',(laFinal[4,3] / laFinal[4,2])*100)  PICTURE '9999'
  @ ROW , 114 SAY IIF(laFinal[4,1]=0,'N/A',(laFinal[4,2] / laFinal[4,1])*100)  PICTURE '9999'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
  *@ ROW , 122 SAY IIF(laFinal[4,1]=0,'N/A',(laFinal[4,3] / laFinal[4,1])*100) PICTURE '9999'
  @ ROW , 119 SAY IIF(laFinal[4,1]=0,'N/A',(laFinal[4,3] / laFinal[4,1])*100) PICTURE '9999'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
ELSE  
  @ ROW , 103 SAY IIF(laFinal[4,2]=0,'N/A',(laFinal[4,3] / laFinal[4,2])*100)  PICTURE '9999.99'
  @ ROW , 111 SAY IIF(laFinal[4,1]=0,'N/A',(laFinal[4,2] / laFinal[4,1])*100)  PICTURE '9999.99'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
  *@ ROW , 119 SAY IIF(laFinal[4,1]=0,'N/A',(laFinal[4,3] / laFinal[4,1])*100) PICTURE '9999.99'
  @ ROW , 115 SAY IIF(laFinal[4,1]=0,'N/A',(laFinal[4,3] / laFinal[4,1])*100) PICTURE '9999.99'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

ROW= ROW+1

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 03 SAY laCost[5]
*@ ROW , 13 SAY laFinal[5,1]                              PICTURE '9999999.999'
*@ ROW , 24 SAY laFinal[5,2]                              PICTURE '9999999.999'
*@ ROW , 35 SAY laFinal[5,3]                              PICTURE '9999999.999'
*@ ROW , 46 SAY laFinal[5,3]-laFinal[5,2]                 PICTURE '9999999.999'

@ ROW , 00 SAY laCost[5]
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 10 SAY laFinal[5,1]                              PICTURE '9999999999.999'
*@ ROW , 25 SAY laFinal[5,2]                              PICTURE '9999999999.999'
*@ ROW , 40 SAY laFinal[5,3]                              PICTURE '9999999999.999'
*@ ROW , 55 SAY laFinal[5,3]-laFinal[5,2]                 PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 14 SAY laFinal[5,1]                              PICTURE '9999999999'
  @ ROW , 29 SAY laFinal[5,2]                              PICTURE '9999999999'
  @ ROW , 44 SAY laFinal[5,3]                              PICTURE '9999999999'
  @ ROW , 59 SAY laFinal[5,3]-laFinal[5,2]                 PICTURE '99999999999'
ELSE
  @ ROW , 10 SAY laFinal[5,1]                              PICTURE '9999999999.999'
  @ ROW , 25 SAY laFinal[5,2]                              PICTURE '9999999999.999'
  @ ROW , 40 SAY laFinal[5,3]                              PICTURE '9999999999.999'
  @ ROW , 55 SAY laFinal[5,3]-laFinal[5,2]                 PICTURE '99999999999.999'
ENDIF  
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnAct_Lan= lnAct_Lan+(laAct[5] -  lalan[5])


*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 57 SAY laFinal[5,2] -laFinal[5,1]                 PICTURE '9999999.999'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 71 SAY laFinal[5,2] -laFinal[5,1]                 PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 75 SAY laFinal[5,2] -laFinal[5,1]                 PICTURE '99999999999'
ELSE
  @ ROW , 71 SAY laFinal[5,2] -laFinal[5,1]                 PICTURE '99999999999.999'
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]


lnLan_Est=lnLan_Est+(lalan[5] - laEst[5])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 68 SAY laFinal[5,3] -laFinal[5,1]                 PICTURE '9999999.999'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 91 SAY laFinal[5,3] -laFinal[5,1]                 PICTURE '99999999999.999'
IF lcRpDec ='N'
  @ ROW , 91 SAY laFinal[5,3] -laFinal[5,1]                 PICTURE '99999999999'
ELSE
  @ ROW , 87 SAY laFinal[5,3] -laFinal[5,1]                 PICTURE '99999999999.999'
ENDIF  
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnAct_Est=lnAct_Est+(laAct[5] - laEst[5])

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 81 SAY IIF(laFinal[5,2]=0,'N/A',(laFinal[5,3] / laFinal[5,2])*100)  PICTURE '9999.99'
*@ ROW , 91 SAY IIF(laFinal[5,1]=0,'N/A',(laFinal[5,2] / laFinal[5,1])*100)  PICTURE '9999.99'
*@ ROW , 100 SAY IIF(laFinal[5,1]=0,'N/A',(laFinal[5,3] / laFinal[5,1])*100) PICTURE '9999.99'

*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 103 SAY IIF(laFinal[5,2]=0,'N/A',(laFinal[5,3] / laFinal[5,2])*100)  PICTURE '9999.99'
*@ ROW , 111 SAY IIF(laFinal[5,1]=0,'N/A',(laFinal[5,2] / laFinal[5,1])*100)  PICTURE '9999.99'
*@ ROW , 119 SAY IIF(laFinal[5,1]=0,'N/A',(laFinal[5,3] / laFinal[5,1])*100) PICTURE '9999.99'
IF lcRpDec ='N'
  @ ROW , 106 SAY IIF(laFinal[5,2]=0,'N/A',(laFinal[5,3] / laFinal[5,2])*100)  PICTURE '9999'
  @ ROW , 114 SAY IIF(laFinal[5,1]=0,'N/A',(laFinal[5,2] / laFinal[5,1])*100)  PICTURE '9999'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
  *@ ROW , 122 SAY IIF(laFinal[5,1]=0,'N/A',(laFinal[5,3] / laFinal[5,1])*100) PICTURE '9999'
  @ ROW , 119 SAY IIF(laFinal[5,1]=0,'N/A',(laFinal[5,3] / laFinal[5,1])*100) PICTURE '9999'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
ELSE
  @ ROW , 103 SAY IIF(laFinal[5,2]=0,'N/A',(laFinal[5,3] / laFinal[5,2])*100)  PICTURE '9999.99'
  @ ROW , 111 SAY IIF(laFinal[5,1]=0,'N/A',(laFinal[5,2] / laFinal[5,1])*100)  PICTURE '9999.99'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
  *@ ROW , 119 SAY IIF(laFinal[5,1]=0,'N/A',(laFinal[5,3] / laFinal[5,1])*100) PICTURE '9999.99'
  @ ROW , 115 SAY IIF(laFinal[5,1]=0,'N/A',(laFinal[5,3] / laFinal[5,1])*100) PICTURE '9999.99'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

ROW= ROW+1

FOR I = 1 TO 5
  lnFinEst=lnFinEst+ laEST[I]
  lnFinLan=lnFinLan+ laLan[I]
  lnFinAct=lnFinAct+ laAct[I]
ENDFOR

*B603348,1 ABD Remark the next lines &Move them to right. .[Begin]
*@ ROW ,|14 SAY '---------- ----------  --------- ----------  --------- ----------  -------   ------- --------'
@ ROW ,14 SAY '----------     ----------      ---------      ----------       ---------      ---------- ------- ------- --------'    
*B603348,1 ABD [End]

ROW = ROW + 1

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 03 SAY 'Total Cost'
*@ ROW , 14 SAY laFinal[1,1]+laFinal[2,1]+laFinal[3,1]+laFinal[4,1]+laFinal[5,1]    PICTURE '999999.999'
@ ROW , 00 SAY 'Total Cost'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 10 SAY laFinal[1,1]+laFinal[2,1]+laFinal[3,1]+laFinal[4,1]+laFinal[5,1]    PICTURE '99999999999.99'
IF lcRpDec ='N'
  @ ROW , 13 SAY laFinal[1,1]+laFinal[2,1]+laFinal[3,1]+laFinal[4,1]+laFinal[5,1]    PICTURE '99999999999'
ELSE
  @ ROW , 10 SAY laFinal[1,1]+laFinal[2,1]+laFinal[3,1]+laFinal[4,1]+laFinal[5,1]    PICTURE '99999999999.99'
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnFin1 =  laFinal[1,1]+laFinal[2,1]+laFinal[3,1]+laFinal[4,1]+laFinal[5,1]
*-- B602352 AAMER 12/14/98 (Start)
*lnEst = (PosLn.NCost1+PosLn.NCost2+PosLn.NCost3+PosLn.NCost4+;
         PosLn.NCost5)*lnTOTQTY

lnEst = (NCost1+NECost2+NCost3+NECost4+NCost5)*lnTOTQTY
*-- B602352 AAMER 12/14/98 (End)    

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 25 SAY laFinal[1,2]+laFinal[2,2]+laFinal[3,2]+laFinal[4,2]+laFinal[5,2]    PICTURE '999999.999'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 25 SAY laFinal[1,2]+laFinal[2,2]+laFinal[3,2]+laFinal[4,2]+laFinal[5,2]    PICTURE '99999999999.99'
IF lcRpDec ='N'
  @ ROW , 28 SAY laFinal[1,2]+laFinal[2,2]+laFinal[3,2]+laFinal[4,2]+laFinal[5,2]    PICTURE '99999999999'
ELSE
  @ ROW , 25 SAY laFinal[1,2]+laFinal[2,2]+laFinal[3,2]+laFinal[4,2]+laFinal[5,2]    PICTURE '99999999999.99'
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnFin2 =  laFinal[1,2]+laFinal[2,2]+laFinal[3,2]+laFinal[4,2]+laFinal[5,2]

lnLand = (NELanCost1+NELanCost2+NELanCost3+NELanCost4+NELanCost5)*lnTOTQTY

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 36 SAY laFinal[1,3]+laFinal[2,3]+laFinal[3,3]+laFinal[4,3]+laFinal[5,3]    PICTURE '999999.999'
*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 40 SAY laFinal[1,3]+laFinal[2,3]+laFinal[3,3]+laFinal[4,3]+laFinal[5,3]    PICTURE '99999999999.99'
IF lcRpDec ='N'
  @ ROW , 43 SAY laFinal[1,3]+laFinal[2,3]+laFinal[3,3]+laFinal[4,3]+laFinal[5,3]    PICTURE '99999999999'
ELSE
  @ ROW , 40 SAY laFinal[1,3]+laFinal[2,3]+laFinal[3,3]+laFinal[4,3]+laFinal[5,3]    PICTURE '99999999999.99'
ENDIF
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

lnFin3 =laFinal[1,3]+laFinal[2,3]+laFinal[3,3]+laFinal[4,3]+laFinal[5,3]
*B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [Begin]
*lnAcT  = (NEActCost1+NEActCost2+NEActCost3+NEActCost4+NEActCost5)*lnTOTQTY
lnAcT  = ROUND((NEActCost1+NEActCost2+NEActCost3+NEActCost4+NEActCost5)*lnTOTQTY,3)
*B606002,1 ALB Fix rounding problem in the Equ. Actual cost that update from AP [end]

*B603348,1 ABD Remark the next lines & change the picture & Move them to left. .[Begin]
*@ ROW , 46 SAY  lnFin3 - lnFin2                     PICTURE '9999999.999'
*@ ROW , 57 SAY lnFin2 - lnFin1                      PICTURE '9999999.999'
*@ ROW , 68 SAY lnFin3 - lnFin1                      PICTURE '9999999.999'
*@ ROW , 81 SAY IIF(lnFin2=0,'N/A',(lnFin3 / lnFin2)*100)         PICTURE '9999.99'
*@ ROW , 91 SAY IIF(lnFin1=0,'N/A',(lnFin2 / lnFin1)*100)         PICTURE '9999.99'
*@ ROW , 100 SAY IIF(lnFin1=0,'N/A',(lnFin3 / lnFin1)*100)        PICTURE '9999.99'

*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW , 55 SAY  lnFin3 - lnFin2                     PICTURE '999999999999.99'
*@ ROW , 71 SAY lnFin2 - lnFin1                      PICTURE '999999999999.99'
*@ ROW , 87 SAY lnFin3 - lnFin1                      PICTURE '999999999999.99'
*@ ROW , 103 SAY IIF(lnFin2=0,'N/A',(lnFin3 / lnFin2)*100)    PICTURE '9999.99'
*@ ROW , 111 SAY IIF(lnFin1=0,'N/A',(lnFin2 / lnFin1)*100)    PICTURE '9999.99'
*@ ROW , 119 SAY IIF(lnFin1=0,'N/A',(lnFin3 / lnFin1)*100)    PICTURE '9999.99'
IF lcRpDec ='N'
  @ ROW , 58 SAY  lnFin3 - lnFin2                     PICTURE '999999999999'
  @ ROW , 74 SAY lnFin2 - lnFin1                      PICTURE '999999999999'
  @ ROW , 90 SAY lnFin3 - lnFin1                      PICTURE '999999999999'
  @ ROW , 106 SAY IIF(lnFin2=0,'N/A',(lnFin3 / lnFin2)*100)    PICTURE '9999'
  @ ROW , 114 SAY IIF(lnFin1=0,'N/A',(lnFin2 / lnFin1)*100)    PICTURE '9999'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
  *@ ROW , 122 SAY IIF(lnFin1=0,'N/A',(lnFin3 / lnFin1)*100)    PICTURE '9999'
  @ ROW , 119 SAY IIF(lnFin1=0,'N/A',(lnFin3 / lnFin1)*100)    PICTURE '9999'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
ELSE
  @ ROW , 55 SAY  lnFin3 - lnFin2                     PICTURE '999999999999.99'
  @ ROW , 71 SAY lnFin2 - lnFin1                      PICTURE '999999999999.99'
  @ ROW , 87 SAY lnFin3 - lnFin1                      PICTURE '999999999999.99'
  @ ROW , 103 SAY IIF(lnFin2=0,'N/A',(lnFin3 / lnFin2)*100)    PICTURE '9999.99'
  @ ROW , 111 SAY IIF(lnFin1=0,'N/A',(lnFin2 / lnFin1)*100)    PICTURE '9999.99'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
  *@ ROW , 119 SAY IIF(lnFin1=0,'N/A',(lnFin3 / lnFin1)*100)    PICTURE '9999.99'
  @ ROW , 115 SAY IIF(lnFin1=0,'N/A',(lnFin3 / lnFin1)*100)    PICTURE '9999.99'
  *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
ENDIF  
*B803594,1  MHM 09/10/2000 [END]
*B603348,1 ABD [End]

ROW = ROW + 1

*B603348,1 ABD Remark the next lines & Move them to right. .[Begin]
*@ ROW ,14 SAY '========== ========== ==========  ========= ==========  =========  =======  ======== ======== '
@ ROW ,14 SAY '==========     ==========      =========      ==========       =========      ========== ======= ======= ========'
*B603348,1 ABD [End]

ROW = ROW+1
@ ROW,00 SAY REPLICATE('=',132)

*---------------------------
*   END STY6C0.PRG
*---------------------------

*:************************************************************************
*: Program file  : lpNoCosting
*: Program desc. : print the report with no style costing details.
*: B800014,1 MFM 07/24/95.
*:*************************************************************
PROCEDURE lpNoCosting

*---------------------------------------------------------
* BEGIN [MAIN REPORT] LOOP
*---------------------------------------------------------
*B603001,1 AMM Initialize
lcScale = SPACE(0)
llNewPg = .T.
*B603001,1 AMM end
DO WHILE INKEY() <>32

  IF ROW >=53
    PAGENO = PAGENO+1
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    *B603348,1 ABD Remark the next lines & Move them to left & Change the place wher that print .[Begin]
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ 05,00 SAY PADR(lcMajTtl,21)
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *B603348,1 ABD [End]
    IF lcCurr = 'Y'
       *--B602529 (Start)
       *@ 06,00 SAY 'PO#    VENDOR   RECEIVED SHPMT# S ' + PADR(lcMajTtl,21) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY  PRICE   AMOUNT'
       *E301180,1
       *@ 06,00 SAY 'PO#    VENDOR   RECEIVED ' + IIF(llDyelot,'DYELOT','SHPMT#') + ' S ' + PADR(lcMajTtl,21) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY  PRICE   AMOUNT'
       *B603348,1 ABD Remark the next lines & Move them to left chnage the layout.[Begin]
       *@ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED ' + IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#')) + ' S ' + PADR(lcMajTtl,21) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY  PRICE   AMOUNT'
       *B803594,1  MHM 09/10/2000 [BEGIN]
       IF lcRpDec ='N'
          *T20060831.0029 TMI [Start] fit report width to be 130 char 
          *@ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED ' + IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#')) + ' S ' + gfItemMask("HI") + '  QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY  PRICE      AMOUNT'
          @ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED ' + IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#')) + ' S ' + gfItemMask("HI") + '  QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY   PRICE  AMOUNT'
          *T20060831.0029 TMI [End  ] 
         *@ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED '+IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#'))+'S ' +STYLE + '  QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY     PRICE        AMOUNT'
       ELSE
         @ 05,00 SAY PADR(lcMajTtl,21)
         
         *B122651,1 NNA 05/24/2004 (Begin) change the positions of the header to be aligned if print decimal [Yes]
         *@ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED '+IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#'))+'     STATUS ' +SPACE(4) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY     PRICE         AMOUNT'
         *T20060831.0029 TMI [Start]  fit report width to be 130 char 
         *@ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED '+IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#'))+'     STATUS ' +SPACE(4) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6   QTY7   QTY8   TOTQTY      PRICE      AMOUNT'
         @ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED '+IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#'))+'    STATUS  '  + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6   QTY7   QTY8   TOTQTY     PRICE         AMOUNT'
         *T20060831.0029 TMI [End  ] 
         *B122651,1 NNA (End)
       ENDIF
       *B803594,1  MHM 09/10/2000 [END]
       *B603348,1 ABD [End]
       *--B602529 (End)
    ELSE
       *--B602529 (Start)
       *@ 06,00 SAY 'PO#    VENDOR   RECEIVED SHPMT# S ' + PADR(lcMajTtl,21) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY $PRICE   AMOUNT'
       *E301180,1
       *@ 06,00 SAY 'PO#    VENDOR   RECEIVED ' + IIF(llDyelot,'DYELOT','SHPMT#') + ' S ' + PADR(lcMajTtl,21) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY $PRICE   AMOUNT'
       *B603348,1 ABD Remark the next lines & Move them to left chnage the layout.[Begin]
       *@ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED ' + IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#')) + ' S ' + PADR(lcMajTtl,21) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY  $PRICE   AMOUNT'
       *B803594,1  MHM 09/10/2000 [BEGIN]
       *@ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED '+IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#'))+'     STATUS ' +SPACE(4) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY     $PRICE        AMOUNT'
       IF lcRpDec ='N' 
         *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
         *@ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED ' + IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#')) + ' S ' + gfItemMask("HI")+ '  QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY  $PRICE    AMOUNT'
         *T20060831.0029 TMI [Start] make the report 130 width only 
         *@ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED ' + IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#')) + ' S ' + gfItemMask("HI")+ '  QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY   PRICE    AMOUNT'
         @ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED ' + IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#')) + ' S ' + gfItemMask("HI")+ '  QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY   PRICE  AMOUNT'
         *T20060831.0029 TMI [End  ] 
         *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
       ELSE 
         *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
         *B603625,1 AMH Set lcStatus to empty value [Start]
         *@ 06,00 SAY 'PO#    '+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED '+IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#'))+'     STATUS ' +SPACE(4) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY     $PRICE         AMOUNT'
         *@ 06,00 SAY IIF(gcAct_Appl = "MF",'D.O. # ','PO#    ')+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED '+IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#'))+'     STATUS ' +SPACE(4) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY     $PRICE         AMOUNT'
         *B603625,1 AMH [End]

         *B122651,1 NNA 05/24/2004 (Begin) change the positions of the header to be aligned if print decimal [Yes]
         *@ 06,00 SAY IIF(gcAct_Appl = "MF",'D.O. # ','PO#    ')+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED '+IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#'))+'   STATUS ' +SPACE(2) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY        PRICE     AMOUNT'
         *T20060831.0029 TMI [Start]  fit report width to be 130 char 
         *@ 06,00 SAY IIF(gcAct_Appl = "MF",'D.O. # ','PO#    ')+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED '+IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#'))+'   STATUS ' +SPACE(2) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6   QTY7   QTY8   TOTQTY     PRICE           AMOUNT'
         @ 06,00 SAY IIF(gcAct_Appl = "MF",'D.O. # ','PO#    ')+IIF(lcRPPoType="N","FROM  ",'VENDOR')+'   RECEIVED '+IIF(llDyelot,'DYELOT',IIF(lcRPPoType="N","TARGET",'SHPMT#'))+'   STATUS ' +SPACE(2) + 'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6   QTY7   QTY8   TOTQTY     PRICE         AMOUNT'
         *T20060831.0029 TMI [End  ] 
         *B122651,1 NNA  (End)

         *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
       ENDIF  
       *B803594,1  MHM 09/10/2000 [END]
       *B603348,1 ABD [End]
       *--B602529 (End)       
    ENDIF
     @ 07,00 SAY REPLICATE('=',132)
     ROW = 08
     *B603001,1 AMM This variable indicate new page to print size scale titles if new page
     llNewPg = .T.
     *B603001,1 AMM end
  ENDIF

  DO WHILE LEN(TRIM(BREAK)) <>0
    IF &BREAK = HBREAK
      EXIT
    ENDIF

    IF (lcCurr = 'N' .OR. (lcCurr = 'Y' .AND. BREAKD= 'STYLE') )
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('-',132)
      ROW=ROW+1

      @ ROW,000 SAY '**SUB TOTAL**'
      @ ROW,015 SAY HBREAK

      *B603348,1 ABD Remark the next lines & change the picture & Move them to right. .[Begin]
      *@ ROW,053 SAY XTOTAL(1,1)  PICTURE '999999'
      *@ ROW,060 SAY XTOTAL(1,2)  PICTURE '999999'
      *@ ROW,067 SAY XTOTAL(1,3)  PICTURE '999999'
      *@ ROW,074 SAY XTOTAL(1,4)  PICTURE '999999'
      *@ ROW,081 SAY XTOTAL(1,5)  PICTURE '999999'
      *@ ROW,088 SAY XTOTAL(1,6)  PICTURE '999999'
      *@ ROW,094 SAY XTOTAL(1,7)  PICTURE '999999'
      *@ ROW,100 SAY XTOTAL(1,8)  PICTURE '999999'
      *@ ROW,107 SAY XTOTAL(1,9)  PICTURE '9999999'

      *B803594,1  MHM 09/10/2000 [BEGIN]
      *@ ROW,045 SAY XTOTAL(1,1)  PICTURE '999999'
      *@ ROW,052 SAY XTOTAL(1,2)  PICTURE '999999'
      *@ ROW,059 SAY XTOTAL(1,3)  PICTURE '999999'
      *@ ROW,066 SAY XTOTAL(1,4)  PICTURE '999999'
      *@ ROW,073 SAY XTOTAL(1,5)  PICTURE '999999'
      *@ ROW,080 SAY XTOTAL(1,6)  PICTURE '999999'
      *@ ROW,086 SAY XTOTAL(1,7)  PICTURE '999999'
      *@ ROW,092 SAY XTOTAL(1,8)  PICTURE '999999'
      *@ ROW,099 SAY XTOTAL(1,9)  PICTURE '9999999'
      IF lcRpDec ='N' 
        *B603713,10 HBG 12/17/2000 change the positions of the totals [Begin]

        *B122651,1 NNA 05/24/2004 (Begin) change the positions of Sub Totals Qty. if print decimal [NO]
        @ ROW,053 SAY XTOTAL(1,1)  PICTURE '999999'
        @ ROW,060 SAY XTOTAL(1,2)  PICTURE '999999'
        @ ROW,067 SAY XTOTAL(1,3)  PICTURE '999999'
        @ ROW,074 SAY XTOTAL(1,4)  PICTURE '999999'
        @ ROW,081 SAY XTOTAL(1,5)  PICTURE '999999'
        @ ROW,088 SAY XTOTAL(1,6)  PICTURE '999999'
        @ ROW,094 SAY XTOTAL(1,7)  PICTURE '999999'
        @ ROW,100 SAY XTOTAL(1,8)  PICTURE '999999'
        @ ROW,107 SAY XTOTAL(1,9)  PICTURE '9999999'
        *@ ROW,049 SAY XTOTAL(1,1)  PICTURE '999999'
        *@ ROW,056 SAY XTOTAL(1,2)  PICTURE '999999'
        *@ ROW,063 SAY XTOTAL(1,3)  PICTURE '999999'
        *@ ROW,070 SAY XTOTAL(1,4)  PICTURE '999999'
        *@ ROW,077 SAY XTOTAL(1,5)  PICTURE '999999'
        *@ ROW,084 SAY XTOTAL(1,6)  PICTURE '999999'
        *@ ROW,091 SAY XTOTAL(1,7)  PICTURE '999999'
        *@ ROW,096 SAY XTOTAL(1,8)  PICTURE '999999'
        *@ ROW,103 SAY XTOTAL(1,9)  PICTURE '9999999'
        *B122651,1 NNA  (End)
        
        *B603713,10 [End]
      ELSE
        *B603713,10 HBG 12/17/2000 change the positions of the totals [Begin]
        *@ ROW,045 SAY XTOTAL(1,1)  PICTURE '999999'
        *@ ROW,052 SAY XTOTAL(1,2)  PICTURE '999999'
        *@ ROW,059 SAY XTOTAL(1,3)  PICTURE '999999'
        *@ ROW,066 SAY XTOTAL(1,4)  PICTURE '999999'
        *@ ROW,073 SAY XTOTAL(1,5)  PICTURE '999999'
        *@ ROW,080 SAY XTOTAL(1,6)  PICTURE '999999'
        *@ ROW,086 SAY XTOTAL(1,7)  PICTURE '999999'
        *@ ROW,092 SAY XTOTAL(1,8)  PICTURE '999999'
        *@ ROW,099 SAY XTOTAL(1,9)  PICTURE '9999999'
        @ ROW,041 SAY XTOTAL(1,1)  PICTURE '999999'
        @ ROW,048 SAY XTOTAL(1,2)  PICTURE '999999'
        @ ROW,055 SAY XTOTAL(1,3)  PICTURE '999999'
        @ ROW,062 SAY XTOTAL(1,4)  PICTURE '999999'
        @ ROW,069 SAY XTOTAL(1,5)  PICTURE '999999'
        @ ROW,076 SAY XTOTAL(1,6)  PICTURE '999999'

        *B122651,1 NNA 05/24/2004 (Begin) change the positions of the last three Sub Totals Qty. if print decimal [Yes]
        *@ ROW,082 SAY XTOTAL(1,7)  PICTURE '999999'
        *@ ROW,088 SAY XTOTAL(1,8)  PICTURE '999999'
        *@ ROW,095 SAY XTOTAL(1,9)  PICTURE '9999999'
        @ ROW,083 SAY XTOTAL(1,7)  PICTURE '999999'
        @ ROW,090 SAY XTOTAL(1,8)  PICTURE '999999'
        @ ROW,097 SAY XTOTAL(1,9)  PICTURE '9999999'
        *B122651,1 NNA  (End)
        
        *B603713,10 [End]
      ENDIF
      *B803594,1  MHM 09/10/2000 [END]
      *B603348,1 ABD [End]
      
      *B603017,1 AMM If the user has access on costing
      IF llCostPrv
        *B603017,1 AMM end
        *B603348,1 ABD Remark the next lines & change the picture & Move them to right. .[Begin]
        *@ ROW,122 SAY XTOTAL(1,10) PICTURE '99999.99'
        *B803594,1  MHM 09/10/2000 [BEGIN]
        *@ ROW,117 SAY XTOTAL(1,10) PICTURE '99999999999.99'

        IF lcRpDec ='N'
          *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
          *@ ROW,122 SAY XTOTAL(1,10) PICTURE  '99999999999'

          *B122651,1 NNA 05/24/2004 (Begin) change the positions of Sub Totals Amount if print decimal [NO]
          *@ ROW,119 SAY XTOTAL(1,10) PICTURE '99999999'
          *T20060831.0029 TMI [Start] make the report 130 width only
          *@ ROW,124 SAY XTOTAL(1,10) PICTURE '99999999'
          @ ROW,122 SAY XTOTAL(1,10) PICTURE  '99999999'
          *T20060831.0029 TMI [End  ] 
          *B122651,1 NNA  (End)

          *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
        ELSE
          *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]

         *B122651,1 NNA 05/24/2004 (Begin) change the positions of Sub Totals Amount if print decimal [Yes]
          *T20060831.0029 TMI [Start] make the report 130 width only
          *@ ROW,118 SAY XTOTAL(1,10) PICTURE '99999999999.99'          
          @ ROW,118 SAY XTOTAL(1,10) PICTURE '999999999.99'
          *T20060831.0029 TMI [End  ] 
          *@ ROW,115 SAY XTOTAL(1,10) PICTURE '99999999.99'
          *B122651,1 NNA  (End)
          
          *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
        ENDIF  
        *B803594,1  MHM 09/10/2000 [END]
        *B603348,1 ABD [End]
        
        *B603017,1 AMM End if
      ENDIF
      *B603017,1 AMM end
      ROW = ROW+1
      @ ROW,00 SAY REPLICATE('-',132)
      ROW = ROW+1
    ENDIF

    X = 1
    DO WHILE (X <= 10)
      XTOTAL1(1,X) = XTOTAL1(1,X) + XTOTAL(1,X)
      XTOTAL(1,X)  = 0.00
      X = X + 1
    ENDDO

    HBREAK = &BREAK
    EXIT

  ENDDO
  ******---- End Subtotals Loop ----******

  IF EOF()
    EXIT
  ENDIF

  IF ROW >=53
    ROW = 99
    LOOP
  ENDIF

  SELECT &WORKFILE

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3.
*PO#    VENDOR   RECEIVED SHPMT# S STYLE                QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY  PRICE   AMOUNT
*PO#    VENDOR   RECEIVED SHPMT# S STYLE                QTY1   QTY2   QTY3   QTY4   QTY5   QTY6  QTY7  QTY8  TOTQTY  PRICE   AMOUNT

  *B603348,1 ABD increse row and print the style before the scale.[Begin]
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *ROW = ROW + 1
  *@ ROW,000 SAY STYLE
  IF lcRpDec ='Y' 
    ROW = ROW + 1

    *B122651,1 NNA 05/24/2004 (Begin) If there is Second or Damaged Receipts we add the Quality 
    *B122651,1 NNA            to the style to know its quality     
    *@ ROW,000 SAY STYLE
    IF llNewPg .OR. (!EOF('SCALE') .AND. lcScale # SCALE.SCALE)
      *-- in case scale changed the report print 'Scale Desc.' so I Print the quality Desc.
      *-- in a separate Line
      @ ROW,000 SAY IIF(cStyGrade = '2' ,'Second Qty. #' ,IIF(cStyGrade = '3' ,'Damaged #',''))
      IF cStyGrade = '2' .OR. cStyGrade = '3' 
        ROW = ROW + 1
      ENDIF
      @ ROW,000 SAY STYLE
    ELSE
      lcSty = IIF(cStyGrade = '2', STYLE + ' : Second Q.',;
              IIF(cStyGrade = '3', STYLE + ' : Damaged ',STYLE))
      @ ROW,000 SAY lcSty
    ENDIF
    *B122651,1 NNA (End)
    
  ENDIF  
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]


  *B603001,1 AMM If the scale changes, print size scale titles
  IF llNewPg .OR. (!EOF('SCALE') .AND. lcScale # SCALE.SCALE)
    llNewPg = .F.
   *B603348,1 ABD Remark the next lines & Move them to right. .[Begin]
    *ROW = ROW + 1
    *@ ROW,025 SAY 'Scale Desc. :' + SCALE.cscl_desc
    *@ ROW,052 SAY PADL(ALLTRIM(SCALE.SZ1),7)+PADL(ALLTRIM(SCALE.SZ2),7)+;
                  PADL(ALLTRIM(SCALE.SZ3),7)+PADL(ALLTRIM(SCALE.SZ4),7)+;
                  PADL(ALLTRIM(SCALE.SZ5),7)+ PADL(ALLTRIM(SCALE.SZ6),7)+;
                  PADL(ALLTRIM(SCALE.SZ7),7)+PADL(ALLTRIM(SCALE.SZ8),7)

    *B803594,1  MHM 09/10/2000 [END]
    *@ ROW,022 SAY 'Scale Desc.:' + SCALE.cscl_desc                  
    *@ ROW,044 SAY PADL(ALLTRIM(SCALE.SZ1),7)+PADL(ALLTRIM(SCALE.SZ2),7)+;
                  PADL(ALLTRIM(SCALE.SZ3),7)+PADL(ALLTRIM(SCALE.SZ4),7)+;
                  PADL(ALLTRIM(SCALE.SZ5),7)+ PADL(ALLTRIM(SCALE.SZ6),7)+;
                  PADL(ALLTRIM(SCALE.SZ7),7)+PADL(ALLTRIM(SCALE.SZ8),7)
    IF lcRpDec ='Y' 
      @ ROW,020 SAY 'Scale Desc.:' + PADR(SCALE.cscl_desc,8)
      *B603713,10 HBG 12/17/2000 change the positions of the scales [Begin]
      *@ ROW,044 SAY PADL(ALLTRIM(SCALE.SZ1),7)+PADL(ALLTRIM(SCALE.SZ2),7)+;
                  PADL(ALLTRIM(SCALE.SZ3),7)+PADL(ALLTRIM(SCALE.SZ4),7)+;
                  PADL(ALLTRIM(SCALE.SZ5),7)+ PADL(ALLTRIM(SCALE.SZ6),7)+;
                  PADL(ALLTRIM(SCALE.SZ7),7)+PADL(ALLTRIM(SCALE.SZ8),7)
      @ ROW,040 SAY PADL(ALLTRIM(SCALE.SZ1),7)+PADL(ALLTRIM(SCALE.SZ2),7)+;
                  PADL(ALLTRIM(SCALE.SZ3),7)+PADL(ALLTRIM(SCALE.SZ4),7)+;
                  PADL(ALLTRIM(SCALE.SZ5),7)+ PADL(ALLTRIM(SCALE.SZ6),7)+;
                  PADL(ALLTRIM(SCALE.SZ7),7)+PADL(ALLTRIM(SCALE.SZ8),7)
      *B603713,10 [End]
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
      *B603348,1 ABD [End]

    lcScale = SCALE.SCALE
  ENDIF
  *B603001,1 AMM end

  *B122651,1 NNA 05/24/2004 (Begin) If there is Second or Damaged Receipts we Print the Quality 
  *B122651,1 NNA             Desc. Before the line Qty.
  IF cStyGrade = '2' .OR. cStyGrade = '3' 
    ROW = ROW + 1
  ENDIF
  @ ROW,000 SAY IIF(cStyGrade = '2' ,'Second Qty. #' ,IIF(cStyGrade = '3' ,'Damaged #',''))
  *B122651,1 NNA (End)

  ROW=ROW+1
  @ ROW,000 SAY PO
  @ ROW,007 SAY POSHDR->VENDOR
  @ ROW,015 SAY DATE
  *--B602529 (Start)
  *@ ROW,025 SAY SHIPNO
  *E301180,1
  *  @ ROW,025 SAY IIF(llDyelot,SUBSTR(DYELOT,1,6),SHIPNO)

  *B603348,1 ABD Remark the next lines & Print all char. from the dyelot.[Begin]
  *@ ROW,025 SAY IIF(llDyelot,SUBSTR(DYELOT,1,6),IIF(cStyType="N",cWareCode,SHIPNO))
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW,025 SAY IIF(llDyelot,DYELOT,IIF(cStyType="N",cWareCode,SHIPNO))  
  IF lcRpDec ='Y' 
    @ ROW,025 SAY IIF(llDyelot,DYELOT,IIF(cStyType="N",cWareCode,SHIPNO))  
  ELSE
    @ ROW,025 SAY IIF(llDyelot,SUBSTR(DYELOT,1,6),IIF(cStyType="N",cWareCode,SHIPNO))     
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]

  *--B602529 (End)
  *B603348,1 ABD Remark the next lines & print complete word for status .[Begin]
  *@ ROW,032 SAY POSHDR->STATUS
  *@ ROW,034 SAY STYLE
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *DO CASE
  *  CASE POSHDR.STATUS = 'O'
  *     lcSTATUS='OPEN'
  *  CASE POSHDR.STATUS = 'C'
  *     lcSTATUS= 'COMPLETE'
  *  CASE POSHDR.STATUS = 'X'
  *     lcSTATUS='CANCEL'
  *  CASE POSHDR.STATUS = 'S'
  *     lcSTATUS= 'CLOSED'
  *  CASE POSHDR.STATUS = 'H'
  *     lcSTATUS= 'HOLD'
  *ENDCASE
  **@ ROW,036 SAY POSHDR->STATUS
  *@ ROW,036 SAY lcSTATUS
  IF lcRpDec ='Y' 
  
    *B603625,1 AMH Set lcStatus to empty value [Start]
    lcSTATUS = ''
    *B603625,1 AMH [End]
    
    DO CASE
      CASE POSHDR.STATUS = 'O'
         lcSTATUS='OPEN'
      CASE POSHDR.STATUS = 'C'
         lcSTATUS= 'COMPLETE'
      CASE POSHDR.STATUS = 'X'
         lcSTATUS='CANCEL'
      CASE POSHDR.STATUS = 'S'
         lcSTATUS= 'CLOSED'
      CASE POSHDR.STATUS = 'H'
         lcSTATUS= 'HOLD'
    
      *B603625,1 AMH Consider the case of actual status [Start]
      CASE POSHDR.STATUS = 'A'
         lcSTATUS= 'ACTUAL.'
      *B603625,1 AMH [End]
    
    ENDCASE
    *@ ROW,036 SAY POSHDR->STATUS
    *B603713,10 HBG 12/17/2000 print only 6 char from the status to allow
    *B603713,10                increasing the picture of the price [Begin]
    *@ ROW,034 SAY lcSTATUS
    @ ROW,034 SAY PADL(lcSTATUS,6)
    *B603713,10 [End]
  ELSE
    @ ROW,032 SAY POSHDR->STATUS
    @ ROW,034 SAY STYLE
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]

  
  *B602886,1 AMM Display return PO's with minus quantities
  *@ ROW,053 SAY QTY1 PICTURE '999999'
  *@ ROW,060 SAY QTY2 PICTURE '999999'
  *@ ROW,067 SAY QTY3 PICTURE '999999'
  *@ ROW,074 SAY QTY4 PICTURE '999999'
  *@ ROW,081 SAY QTY5 PICTURE '999999'
  *@ ROW,088 SAY QTY6 PICTURE '999999'
  *@ ROW,094 SAY QTY7 PICTURE '999999'
  *@ ROW,100 SAY QTY8 PICTURE '999999'
  *@ ROW,107 SAY TOTQTY PICTURE '9999999'

  *B603348,1 ABD Remark the next lines & change the picture & Move them to right. .[Begin]
  *@ ROW,053 SAY IIF(cStyType='R',-QTY1,QTY1)  PICTURE '999999'
  *@ ROW,060 SAY IIF(cStyType='R',-QTY2,QTY2) PICTURE '999999'
  *@ ROW,067 SAY IIF(cStyType='R',-QTY3,QTY3) PICTURE '999999'
  *@ ROW,074 SAY IIF(cStyType='R',-QTY4,QTY4) PICTURE '999999'
  *@ ROW,081 SAY IIF(cStyType='R',-QTY5,QTY5) PICTURE '999999'
  *@ ROW,088 SAY IIF(cStyType='R',-QTY6,QTY6) PICTURE '999999'
  *@ ROW,094 SAY IIF(cStyType='R',-QTY7,QTY7) PICTURE '999999'
  *@ ROW,100 SAY IIF(cStyType='R',-QTY8,QTY8) PICTURE '999999'
  *@ ROW,107 SAY IIF(cStyType='R',-TotQTY,TOTQTY) PICTURE '9999999'
 
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW,045 SAY IIF(cStyType='R',-QTY1,QTY1)     PICTURE '999999'
  *@ ROW,052 SAY IIF(cStyType='R',-QTY2,QTY2)     PICTURE '999999'
  *@ ROW,059 SAY IIF(cStyType='R',-QTY3,QTY3)     PICTURE '999999'
  *@ ROW,066 SAY IIF(cStyType='R',-QTY4,QTY4)     PICTURE '999999'
  *@ ROW,073 SAY IIF(cStyType='R',-QTY5,QTY5)     PICTURE '999999'
  *@ ROW,080 SAY IIF(cStyType='R',-QTY6,QTY6)     PICTURE '999999'
  *@ ROW,086 SAY IIF(cStyType='R',-QTY7,QTY7)     PICTURE '999999'
  *@ ROW,092 SAY IIF(cStyType='R',-QTY8,QTY8)     PICTURE '999999'
  *@ ROW,099 SAY IIF(cStyType='R',-TotQTY,TOTQTY) PICTURE '9999999'
  IF lcRpDec ='Y' 
    *B603713,10 HBG 12/17/2000 change the positions of the QTY [Begin]
    *@ ROW,045 SAY IIF(cStyType='R',-QTY1,QTY1)     PICTURE '999999'
    *@ ROW,052 SAY IIF(cStyType='R',-QTY2,QTY2)     PICTURE '999999'
    *@ ROW,059 SAY IIF(cStyType='R',-QTY3,QTY3)     PICTURE '999999'
    *@ ROW,066 SAY IIF(cStyType='R',-QTY4,QTY4)     PICTURE '999999'
    *@ ROW,073 SAY IIF(cStyType='R',-QTY5,QTY5)     PICTURE '999999'
    *@ ROW,080 SAY IIF(cStyType='R',-QTY6,QTY6)     PICTURE '999999'
    *@ ROW,086 SAY IIF(cStyType='R',-QTY7,QTY7)     PICTURE '999999'
    *@ ROW,092 SAY IIF(cStyType='R',-QTY8,QTY8)     PICTURE '999999'
    *@ ROW,099 SAY IIF(cStyType='R',-TotQTY,TOTQTY) PICTURE '9999999'
    @ ROW,041 SAY IIF(cStyType='R',-QTY1,QTY1)     PICTURE '999999'
    @ ROW,048 SAY IIF(cStyType='R',-QTY2,QTY2)     PICTURE '999999'
    @ ROW,055 SAY IIF(cStyType='R',-QTY3,QTY3)     PICTURE '999999'
    @ ROW,062 SAY IIF(cStyType='R',-QTY4,QTY4)     PICTURE '999999'
    @ ROW,069 SAY IIF(cStyType='R',-QTY5,QTY5)     PICTURE '999999'
    @ ROW,076 SAY IIF(cStyType='R',-QTY6,QTY6)     PICTURE '999999'

    *B122651,1 NNA 05/24/2004 (Begin) change the positions of the last three qty. if print decimal [Yes]
    *@ ROW,082 SAY IIF(cStyType='R',-QTY7,QTY7)     PICTURE '999999'
    *@ ROW,088 SAY IIF(cStyType='R',-QTY8,QTY8)     PICTURE '999999'
    *@ ROW,095 SAY IIF(cStyType='R',-TotQTY,TOTQTY) PICTURE '9999999'
    @ ROW,083 SAY IIF(cStyType='R',-QTY7,QTY7)     PICTURE '999999'
    @ ROW,090 SAY IIF(cStyType='R',-QTY8,QTY8)     PICTURE '999999'
    @ ROW,097 SAY IIF(cStyType='R',-TotQTY,TOTQTY) PICTURE '9999999'
    *B122651,1 NNA  (End)
    
    *B603713,10 [End]
  ELSE
    *B603713,10 HBG 12/17/2000 change the positions of the QTY [Begin]

    *B122651,1 NNA 05/24/2004 (Begin) Clear Fixing by bug #603713
    @ ROW,053 SAY IIF(cStyType='R',-QTY1,QTY1) PICTURE '999999'
    @ ROW,060 SAY IIF(cStyType='R',-QTY2,QTY2) PICTURE '999999'
    @ ROW,067 SAY IIF(cStyType='R',-QTY3,QTY3) PICTURE '999999'
    @ ROW,074 SAY IIF(cStyType='R',-QTY4,QTY4) PICTURE '999999'
    @ ROW,081 SAY IIF(cStyType='R',-QTY5,QTY5) PICTURE '999999'
    @ ROW,088 SAY IIF(cStyType='R',-QTY6,QTY6) PICTURE '999999'
    @ ROW,094 SAY IIF(cStyType='R',-QTY7,QTY7) PICTURE '999999'
    @ ROW,100 SAY IIF(cStyType='R',-QTY8,QTY8) PICTURE '999999'
    @ ROW,107 SAY IIF(cStyType='R',-TotQTY,TOTQTY) PICTURE '9999999'
    *@ ROW,049 SAY IIF(cStyType='R',-QTY1,QTY1) PICTURE '999999'
    *@ ROW,056 SAY IIF(cStyType='R',-QTY2,QTY2) PICTURE '999999'
    *@ ROW,063 SAY IIF(cStyType='R',-QTY3,QTY3) PICTURE '999999'
    *@ ROW,070 SAY IIF(cStyType='R',-QTY4,QTY4) PICTURE '999999'
    *@ ROW,077 SAY IIF(cStyType='R',-QTY5,QTY5) PICTURE '999999'
    *@ ROW,084 SAY IIF(cStyType='R',-QTY6,QTY6) PICTURE '999999'
    *@ ROW,090 SAY IIF(cStyType='R',-QTY7,QTY7) PICTURE '999999'
    *@ ROW,096 SAY IIF(cStyType='R',-QTY8,QTY8) PICTURE '999999'
    *@ ROW,103 SAY IIF(cStyType='R',-TotQTY,TOTQTY) PICTURE '999999'
    *B122651,1 NNA  (End)

    *B603713,10 [End]
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]

  *B602886,1 AMM end

  *-- B602336 AAMER 12/14/98 (Start)
  *@ ROW,117 SAY IIF(lcCurr = 'Y',nCost1,nCost1/POSHDR->RATE) PICTURE '999.99'

  *-- E301096 AAMER 12/16/98 (Start)
  *@ ROW,117 SAY IIF(lcCurr = 'Y',nCost1,nECost1) PICTURE '999.99'
  *MAN
  *@ ROW,115 SAY IIF(lcRPPrice='R',Style.nSugRetPri,IIF(lcCurr = 'Y',nCost1,nECost1)) PICTURE '999.99'
  *B603017,1 AMM If the user has access on costing
  IF llCostPrv
    *B603017,1 AMM end

    *B603348,1 ABD Remark the next lines & change the picture & Move them to right. .[Begin]
    *@ ROW,115 SAY IIF(lcRPPrice='R',Style.nSugRetPri,IIF(lcCurr = 'Y',nLan_Cst1,nELanCost1)) PICTURE '999.99'
    *B803594,1  MHM 09/10/2000 [BEGIN]
    *@ ROW,107 SAY IIF(lcRPPrice='R',Style.nSugRetPri,IIF(lcCurr = 'Y',nLan_Cst1,nELanCost1)) PICTURE '9999999.99'
    IF lcRpDec ='N'
      *B603713,10 HBG 12/17/2000 change the Picture of the price[Begin]
      *@ ROW,114 SAY IIF(lcRPPrice='R',Style.nSugRetPri,IIF(lcCurr = 'Y',nLan_Cst1,nELanCost1)) PICTURE '9999999'

      *B122651,1 NNA 05/24/2004 (Begin)
      *@ ROW,111 SAY IIF(lcRPPrice='R',Style.nSugRetPri,IIF(lcCurr = 'Y',nLan_Cst1,nELanCost1)) PICTURE '9999999'
      @ ROW,115 SAY IIF(lcRPPrice='R',Style.nSugRetPri,IIF(lcCurr = 'Y',nLan_Cst1,nELanCost1)) PICTURE '9999999'
      *B122651,1 NNA  (End)
      
      *B603713,10 [End]
    ELSE
      *B603713,10 HBG 12/17/2000 change the Picture of the price[Begin]
      *@ ROW,107 SAY IIF(lcRPPrice='R',Style.nSugRetPri,IIF(lcCurr = 'Y',nLan_Cst1,nELanCost1)) PICTURE '9999999.99'
      @ ROW,104 SAY IIF(lcRPPrice='R',Style.nSugRetPri,IIF(lcCurr = 'Y',nLan_Cst1,nELanCost1)) PICTURE '9999999.999'
      *B603713,10 [End]
    ENDIF
    *B803594,1  MHM 09/10/2000 [END]
    *B603348,1 ABD [End]
    
    *B603017,1 AMM End if
  ENDIF
  *B603017,1 AMM end
  *-- E301096 AAMER 12/16/98 (End)

  *-- B602336 AAMER 12/14/98 (End)
  
  IF lcCurr = 'N'
    *-- E301096 AAMER 12/16/98 (Start)
    *@ ROW,123 SAY (TOTQTY*nELanCost1) PICTURE '999999.99'
    *B603017,1 AMM If the user has access on costing
    IF llCostPrv
    *B603017,1 AMM end
      *B602886,1 AMM Display negative amount if return PO 
      *@ ROW,122 SAY (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999.99'

      *B603348,1 ABD Remark the next lines & change the picture & Move them to right. .[Begin]
      *@ ROW,122 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999.99'
      *B803594,1  MHM 09/10/2000 [BEGIN]
      *@ ROW,116 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999999999.99'
      IF lcRpDec ='N'
        *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
        *@ ROW,120 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999999999'

        *B122651,1 NNA 05/24/2004 (Begin)
        *@ ROW,117 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999999'
        *T20060831.0029 TMI [Start] make the report 130 width only
        *@ ROW,124 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999999'
        @ ROW,122 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999999'
        *T20060831.0029 TMI [End  ] 
        *B122651,1 NNA  (End)
        
        *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
      ELSE
        *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
        *@ ROW,116 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999999999.99'

        *B122651,1 NNA 05/24/2004 (Begin)
        *@ ROW,113 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999999.99'
        *T20060831.0029 TMI [Start] make the report 130 width only
        *@ ROW,121 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999999.99'
        @ ROW,119 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999999.99'
        *T20060831.0029 TMI [End  ] 
        *B122651,1 NNA  (End)
        
        *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
      ENDIF
     *B803594,1  MHM 09/10/2000 [END]
      *B603348,1 ABD [End]

      *B602886,1 AMM end
    *B603017,1 AMM End if
    ENDIF
    *B603017,1 AMM end
    
    *-- E301096 AAMER 12/16/98 (End)
  ELSE
    *-- E301096 AAMER 12/16/98 (End)
    *@ ROW,123 SAY (TOTQTY*nLan_Cst1) PICTURE '999999.99'
    *B603017,1 AMM If the user has access on costing
    IF llCostPrv
    *B603017,1 AMM end
      *B602886,1 AMM Display negative amount if negative PO
      *@ ROW,122 SAY (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999.99'
      *B803594,1  MHM 09/10/2000 [BEGIN]
      *@ ROW,122 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999.99'
      IF lcRpDec ='N'
        *B603906,1 HBG 10/15/2000 Use the land cost bt the foren currency [Begin]
        *@ ROW,127 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999'
        *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
        *@ ROW,127 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nLan_Cst1)) PICTURE '99999'
        *T20060831.0029 TMI [Start] make the report 130 width only 
        *@ ROW,124 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nLan_Cst1)) PICTURE '99999'
        @ ROW,125 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nLan_Cst1)) PICTURE '99999'
        *T20060831.0029 TMI [End  ] 
        *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
        *B603906,1 [End]
      ELSE
        *B603906,1 HBG 10/15/2000 Use the land cost bt the foren currency [Begin]
        *@ ROW,122 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1)) PICTURE '99999.99'
        *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
        *@ ROW,123 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nLan_Cst1)) PICTURE '99999.99'
        *T20060831.0029 TMI [Start] make the report 130 width only 
        *@ ROW,120 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nLan_Cst1)) PICTURE '99999.99'
        @ ROW,122 SAY IIF(cStyType='R',-1,1) * (TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nLan_Cst1)) PICTURE '99999.99'
        *T20060831.0029 TMI [End  ] 
        *B603906,1 [End]
        *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
      ENDIF
      *B803594,1  MHM 09/10/2000 [END]
      *B602886,1 AMM end
    *B603017,1 AMM End if
    ENDIF
    *B603017,1 AMM end

    *-- E301096 AAMER 12/16/98 (End)
  ENDIF
 
  *B602886,1 AMM Subtract return PO's quantities from totals
  *XTOTAL(1,1)=XTOTAL(1,1)+QTY1
  *XTOTAL(1,2)=XTOTAL(1,2)+QTY2
  *XTOTAL(1,3)=XTOTAL(1,3)+QTY3
  *XTOTAL(1,4)=XTOTAL(1,4)+QTY4
  *XTOTAL(1,5)=XTOTAL(1,5)+QTY5
  *XTOTAL(1,6)=XTOTAL(1,6)+QTY6
  *XTOTAL(1,7)=XTOTAL(1,7)+QTY7
  *XTOTAL(1,8)=XTOTAL(1,8)+QTY8
  *XTOTAL(1,9)=XTOTAL(1,9)+TOTQTY
  
  XTOTAL(1,1)=XTOTAL(1,1)+IIF(cStyType='R',-QTY1,QTY1)
  XTOTAL(1,2)=XTOTAL(1,2)+IIF(cStyType='R',-QTY2,QTY2)
  XTOTAL(1,3)=XTOTAL(1,3)+IIF(cStyType='R',-QTY3,QTY3)
  XTOTAL(1,4)=XTOTAL(1,4)+IIF(cStyType='R',-QTY4,QTY4)
  XTOTAL(1,5)=XTOTAL(1,5)+IIF(cStyType='R',-QTY5,QTY5)
  XTOTAL(1,6)=XTOTAL(1,6)+IIF(cStyType='R',-QTY6,QTY6)
  XTOTAL(1,7)=XTOTAL(1,7)+IIF(cStyType='R',-QTY7,QTY7)
  XTOTAL(1,8)=XTOTAL(1,8)+IIF(cStyType='R',-QTY8,QTY8)
  XTOTAL(1,9)=XTOTAL(1,9)+IIF(cStyType='R',-TotQTY,TOTQTY)
  *B602886,1 AMM end
  IF lcCurr = 'N'
    *-- B602336 AAMER 12/14/98 (Start)
    *XTOTAL(1,10)=XTOTAL(1,10)+(TOTQTY*(nCost1/POSHDR->RATE))
    
    *-- E301096 AAMER 12/16/98 (Start)
    *XTOTAL(1,10)=XTOTAL(1,10)+(TOTQTY*nECost1)
    *MAN
    *XTOTAL(1,10)=XTOTAL(1,10)+(TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nECost1))
    *B602886,1 AMM Sum negative amount if return PO 
    *XTOTAL(1,10)=XTOTAL(1,10)+(TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1))
    XTOTAL(1,10)=XTOTAL(1,10)+(IIF(cStyType='R',-1,1) * TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nELanCost1))
    *B602886,1 AMM end
    *-- E301096 AAMER 12/16/98 (End)
    
    *-- B602336 AAMER 12/14/98 (End)
  ELSE
    *-- E301096 AAMER 12/16/98 (Start)
    *XTOTAL(1,10)=XTOTAL(1,10)+(TOTQTY*nCost1)
    *MAN
    *XTOTAL(1,10)=XTOTAL(1,10)+(TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nCost1))
    *B602886,1 AMM Sum negative amount if return PO 
    *XTOTAL(1,10)=XTOTAL(1,10)+(TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nLan_Cst1))
    XTOTAL(1,10)=XTOTAL(1,10)+(IIF(cStyType='R',-1,1) * TOTQTY*IIF(lcRPPrice='R',Style.nSugRetPri,nLan_Cst1))
    *B602886,1 AMM end
    *-- E301096 AAMER 12/16/98 (End)
  ENDIF

  SELECT &WORKFILE
  SKIP

ENDDO
*------------------ END [MAIN REPORT] LOOP ---)----------------

X = 2
IF LEN(TRIM(BREAK)) = 0
  X = 1
ENDIF
ROW=ROW+2
@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW+1
@ ROW,000 SAY '* GRAND TOTAL *'

*B603348,1 ABD Remark the next lines & change the picture & Move them to right. .[Begin]
*@ ROW,053 SAY XTOTAL1(1,1)  PICTURE '999999'
*@ ROW,060 SAY XTOTAL1(1,2)  PICTURE '999999'
*@ ROW,067 SAY XTOTAL1(1,3)  PICTURE '999999'
*@ ROW,074 SAY XTOTAL1(1,4)  PICTURE '999999'
*@ ROW,081 SAY XTOTAL1(1,5)  PICTURE '999999'
*@ ROW,088 SAY XTOTAL1(1,6)  PICTURE '999999'
*@ ROW,094 SAY XTOTAL1(1,7)  PICTURE '999999'
*@ ROW,100 SAY XTOTAL1(1,8)  PICTURE '999999'
*@ ROW,107 SAY XTOTAL1(1,9)  PICTURE '9999999'

*B803594,1  MHM 09/10/2000 [BEGIN]
*@ ROW,045 SAY XTOTAL1(1,1)  PICTURE '999999'
*@ ROW,052 SAY XTOTAL1(1,2)  PICTURE '999999'
*@ ROW,059 SAY XTOTAL1(1,3)  PICTURE '999999'
*@ ROW,066 SAY XTOTAL1(1,4)  PICTURE '999999'
*@ ROW,073 SAY XTOTAL1(1,5)  PICTURE '999999'
*@ ROW,080 SAY XTOTAL1(1,6)  PICTURE '999999'
*@ ROW,086 SAY XTOTAL1(1,7)  PICTURE '999999'
*@ ROW,092 SAY XTOTAL1(1,8)  PICTURE '999999'
*@ ROW,099 SAY XTOTAL1(1,9)  PICTURE '9999999'
IF lcRpDec ='Y' 
  *B603713,10 HBG 12/17/2000 change the positions of the totales [Begin]
  *@ ROW,045 SAY XTOTAL1(1,1)  PICTURE '999999'
  *@ ROW,052 SAY XTOTAL1(1,2)  PICTURE '999999'
  *@ ROW,059 SAY XTOTAL1(1,3)  PICTURE '999999'
  *@ ROW,066 SAY XTOTAL1(1,4)  PICTURE '999999'
  *@ ROW,073 SAY XTOTAL1(1,5)  PICTURE '999999'
  *@ ROW,080 SAY XTOTAL1(1,6)  PICTURE '999999'
  *@ ROW,086 SAY XTOTAL1(1,7)  PICTURE '999999'
  *@ ROW,092 SAY XTOTAL1(1,8)  PICTURE '999999'
  *@ ROW,099 SAY XTOTAL1(1,9)  PICTURE '9999999'
  @ ROW,041 SAY XTOTAL1(1,1)  PICTURE '999999'
  @ ROW,048 SAY XTOTAL1(1,2)  PICTURE '999999'
  @ ROW,055 SAY XTOTAL1(1,3)  PICTURE '999999'
  @ ROW,062 SAY XTOTAL1(1,4)  PICTURE '999999'
  @ ROW,069 SAY XTOTAL1(1,5)  PICTURE '999999'
  @ ROW,076 SAY XTOTAL1(1,6)  PICTURE '999999'

  *B122651,1 NNA 05/24/2004 (Begin)
  *@ ROW,082 SAY XTOTAL1(1,7)  PICTURE '999999'
  *@ ROW,088 SAY XTOTAL1(1,8)  PICTURE '999999'
  *@ ROW,095 SAY XTOTAL1(1,9)  PICTURE '9999999'
  @ ROW,083 SAY XTOTAL1(1,7)  PICTURE '999999'
  @ ROW,090 SAY XTOTAL1(1,8)  PICTURE '999999'
  @ ROW,097 SAY XTOTAL1(1,9)  PICTURE '9999999'
  *B122651,1 NNA  (End)
  
  *B603713,10 [End]
ELSE
  *B603713,10 HBG 12/17/2000 change the positions of the totales [Begin]
 
  *B122651,1 NNA 05/24/2004 (Begin) Clear Fixing by bug #603713
  @ ROW,053 SAY XTOTAL1(1,1)  PICTURE '999999'
  @ ROW,060 SAY XTOTAL1(1,2)  PICTURE '999999'
  @ ROW,067 SAY XTOTAL1(1,3)  PICTURE '999999'
  @ ROW,074 SAY XTOTAL1(1,4)  PICTURE '999999'
  @ ROW,081 SAY XTOTAL1(1,5)  PICTURE '999999'
  @ ROW,088 SAY XTOTAL1(1,6)  PICTURE '999999'
  @ ROW,094 SAY XTOTAL1(1,7)  PICTURE '999999'
  @ ROW,100 SAY XTOTAL1(1,8)  PICTURE '999999'
  @ ROW,107 SAY XTOTAL1(1,9)  PICTURE '9999999'
  *@ ROW,049 SAY XTOTAL1(1,1)  PICTURE '999999'
  *@ ROW,056 SAY XTOTAL1(1,2)  PICTURE '999999'
  *@ ROW,063 SAY XTOTAL1(1,3)  PICTURE '999999'
  *@ ROW,070 SAY XTOTAL1(1,4)  PICTURE '999999'
  *@ ROW,077 SAY XTOTAL1(1,5)  PICTURE '999999'
  *@ ROW,084 SAY XTOTAL1(1,6)  PICTURE '999999'
  *@ ROW,090 SAY XTOTAL1(1,7)  PICTURE '999999'
  *@ ROW,096 SAY XTOTAL1(1,8)  PICTURE '999999'
  *@ ROW,103 SAY XTOTAL1(1,9)  PICTURE '9999999'
  *B122651,1 NNA  (End)
  
  *B603713,10 [End]
ENDIF
*B803594,1  MHM 09/10/2000 [BEGIN]
*B603348,1 ABD [End]

*B603017,1 AMM If the user has access on costing
IF llCostPrv
*B603017,1 AMM end
  *B803076,1 [START]
  *@ ROW,122 SAY XTOTAL1(1,10) PICTURE '99999.99'

  *B603348,1 ABD Remark the next lines & change the picture & Move them to right. .[Begin]
  *@ ROW,120 SAY XTOTAL1(1,10) PICTURE '9999999.99'
  *B803594,1  MHM 09/10/2000 [BEGIN]
  *@ ROW,117 SAY XTOTAL1(1,10) PICTURE '99999999999.99'
  IF lcRpDec ='N'
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW,121 SAY XTOTAL1(1,10) PICTURE '99999999999'

    *B122651,1 NNA 05/24/2004 (Begin)
    *@ ROW,119 SAY XTOTAL1(1,10) PICTURE '99999999'
    *T20060831.0029 TMI [Start] make the report 130 width only
    *@ ROW,124 SAY XTOTAL1(1,10) PICTURE '99999999'
    @ ROW,122 SAY XTOTAL1(1,10) PICTURE '99999999'
    *T20060831.0029 TMI [End  ] 
    *B122651,1 NNA  (End)
    
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ELSE
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [BEGIN]
    *@ ROW,117 SAY XTOTAL1(1,10) PICTURE '99999999999.99'

    *B122651,1 NNA 05/24/2004 (Begin)
    *@ ROW,115 SAY XTOTAL1(1,10) PICTURE '99999999.99'
    *T20060831.0029 TMI [Start] make the report 130 width only
    *@ ROW,121 SAY XTOTAL1(1,10) PICTURE '99999999.99'    
    @ ROW,119 SAY XTOTAL1(1,10) PICTURE '99999999.99'    
    *T20060831.0029 TMI [End  ] 
    *B122651,1 NNA  (End)
    
    *B607302,1 ALB Print dollar sign is printed in the headr of the report beside Price column header [END]
  ENDIF
  *B803594,1  MHM 09/10/2000 [END]
  *B603348,1 ABD [End]

  *B803076,1 [END]
*B603017,1 AMM End if
ENDIF
*B603017,1 AMM end
ROW = ROW+1
@ ROW,00 SAY REPLICATE('=',132)
*!*************************************************************
*! Name      : lpRecShip
*! Developer : WALID (WAM)
*! Date      : 03/24/1999
*! Purpose   : Print log if receive by shipment
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : 
*!*************************************************************
*!E301183,1
PROCEDURE lpRecShip
PRIVATE    lnSubOrg,lnSubRec,lnTotOrg,lnTotRec,;
           lnSubDam,lnTotDam,lnSubCan,lnTotCan , lnRecNo , lnColNum
DIMENSION laSubOrg(8),laTotOrg(8),laSubRec(8),laTotRec(8),laSubDam(8),laTotDam(8),;
          laSubCan(8),laTotCan(8)
lcPoNum   =SPACE(6)
lcPrvPoNum=SPACE(6)
STORE 0 TO PAGENO,laSubOrg,laTotOrg,laSubRec,laTotRec,lnSubOrg,lnSubRec,lnTotOrg,lnTotRec,;
           laSubDam,laTotDam,laSubCan,laTotCan,lnSubDam,lnTotDam,lnSubCan,lnTotCan
R_WIDTH='W'
R_TITLE='RECEIVING LOG BY SHIPMENT'

*....+....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*
*SHIPMT# ENTERED  CARTONS AIRWAY BILL#    E.T.A.   LCNO
*123456  99/99/99 1234    123456789012345 99/99/99 123456789012345
*....+....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8.
*PO#: 123456  123456789012345678901234567890
*    STYLE   CLR   SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TotQty
*Org:1234567 123 12345  12345  12345  12345  12345  12345  12345  12345  123456
*Rec:            12345  12345  12345  12345  12345  12345  12345  12345  123456
*Dam:            12345  12345  12345  12345  12345  12345  12345  12345  123456
*Can:            12345  12345  12345  12345  12345  12345  12345  12345  123456
*Sub Org 123456:123456 123456 123456 123456 123456 123456 123456 123456 1234567
*Sub Rec 123456:
*Sub Dam 123456:
*Sub Can 123456:
*Tot. Org Ship: 123456 123456 123456 123456 123456 123456 123456 123456 1234567
*Tot. Received: 123456 123456 123456 123456 123456 123456 123456 123456 1234567
*Tot. Damaged : 123456 123456 123456 123456 123456 123456 123456 123456 1234567
*Tot. Canceled: 123456 123456 123456 123456 123456 123456 123456 123456 1234567

SELECT POSLN
SET FILTER TO
SET RELATION TO
GO TOP
lcShipNo=ShipNo

SELECT SHPMTHDR
IF SEEK (lcShipNo)         &&Record is Already Exist no need for check
ldEntered=ENTERED
lnCartons=CARTONS
lcAirWayb=AIRWAYB
ldETA=ETA
ENDIF
SELECT POSLN

SET ORDER TO TAG POSLNSH  &&--shipno+cstytype+po+style+STR(lineno,6)+trancd
*-- this is not our tag in postrec
GO TOP
lcPrvPoNum=PO

SCAN  FOR TRANCD='1' &&-- in my posln file

*-- Note that he scan in posln file to collect all PO in this shipment
*-- as the file may contain another shipments
*-- but my file can hold only one shipment and the contents of the file
*-- in case of receive by shipment has no trancd=3
*-- because thay are the records represent this condition
*-- i.e when receive by shipment we collect only records that match this condition

  IF ROW>55   &&.T. in first time         &&Print Heading If Necessary
    ROW=0
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'STY651','',R_WIDTH
    
    @ 05,01 SAY 'SHIPMT# ENTERED  CARTONS AIRWAY BILL#    E.T.A.'   && MFM 12/26/94.
    @ 06,01 SAY REPLICATE('-',100)
    @ 07,01 SAY lcShipNo
    @ 07,09 SAY ldEntered
    @ 07,18 SAY lnCartons  PICTURE '9999'
    @ 07,26 SAY lcAirWayb
    @ 07,42 SAY ldETA
    @ 09,05 SAY 'STYLE        COLOR     SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY'
    @ 10,01 SAY REPLICATE('-',100)
    ROW=10
  ENDIF

  IF PO<>lcPrvPoNum  &&-- .F. in first time   &&-- if PO is changed

    *** Print SubTotal Of Original Shipment & Total Received For Each PO
    @ ROW,01 SAY REPLICATE('-',100)
    ROW=ROW+1
    @ ROW,01 SAY 'Sub Org '+lcPoNum+':'
    lnColNum=25
    FOR I=1 TO 8
      @ ROW,lnColNum SAY laSubOrg(I)  PICTURE '@Z 999999'
      lnColNum=lnColNum+7
    ENDFOR
    @ ROW,81 SAY lnSubOrg     PICTURE '9999999'
    ROW=ROW+1
    @ ROW,01 SAY 'Sub Rec '+lcPoNum+':'
    lnColNum=25
    FOR I=1 TO 8
      @ ROW,lnColNum SAY laSubRec(I)  PICTURE '@Z 999999'
      lnColNum=lnColNum+7
    ENDFOR
    @ ROW,81 SAY lnSubRec     PICTURE '9999999'

    ROW=ROW+1
    @ ROW,01 SAY 'Sub Dam '+lcPoNum+':'
    lnColNum=25
    FOR I=1 TO 8
      @ ROW,lnColNum SAY laSubDam(I)  PICTURE '@Z 999999'
      lnColNum=lnColNum+7
    ENDFOR
    @ ROW,81 SAY lnSubDam     PICTURE '9999999'
    ROW=ROW+1
    @ ROW,01 SAY 'Sub Can '+lcPoNum+':'
    lnColNum=25
    FOR I=1 TO 8
      @ ROW,lnColNum SAY laSubCan(I)  PICTURE '@Z 999999'
      lnColNum=lnColNum+7
    ENDFOR
    @ ROW,81 SAY lnSubCan     PICTURE '9999999'

    STORE 0 TO laSubOrg,laSubRec,lnSubOrg,lnSubRec,;
            laSubDam,laSubCan,lnSubDam,lnSubCan
    lcPrvPoNum = PO
  ENDIF &&-- end if po is changed



  
  IF PO=lcPoNum &&-- .F. in the first time
    llFirstTime=.F.
  ELSE
    llFirstTime=.T.
  ENDIF

  *-- get information for the shipment only one time .
  IF llFirstTime &&-- must be done at first time
    lcPoNum=PO
    XVENDOR=posln.VENDOR
             * apvendor
    *-- in posln vendor is tha same thing as apvendor.cvendcode  
    *-- apvendor.cvencomp is vendor's company name
    
    SELECT APVENDOR                           
    xVendName = IIF(SEEK(xVendor), APVENDOR.cVenComp, SPACE(30))   && YMA 07/26/94.
    ROW=ROW+2
    @ ROW,01 SAY 'PO#: '+lcPoNum+'  '+XVENDNAME
    ROW=ROW+1
    @ ROW,01 SAY REPLICATE('-',100)
    SELECT POSLN
  ENDIF  &&-- end if must be done at first time


  lcStyle=POSLN.STYLE
  lcLineNo = posln.LineNo
  lcWareCode = posln.cWareCode
  ROW=ROW+1
    @ ROW,01 SAY 'Org:'+lcStyle
  *** Print Original Qty
  lnColNum=26
  FOR I=1 TO 8
    Z=STR(I,1)
    @ ROW,lnColNum SAY QTY&Z  PICTURE '@Z 99999'
    laSubOrg(I)=laSubOrg(I)+QTY&Z &&-- qty? are fields in posln
    laTotOrg(I)=laTotOrg(I)+QTY&Z
    lnColNum=lnColNum+7
  ENDFOR
  @ ROW,82 SAY TOTQTY PICTURE '999999'
  lnSubOrg=lnSubOrg+TOTQTY
  lnTotOrg=lnTotOrg+TOTQTY
  ROW=ROW+1

  *** Print Received Qty
  *SELECT (POSLTEMP)
  *-- MFM 05/20/94.
  *-- in our case , our posln file carry these records with trancd=2 i.e receive
  *-- so we need not to select any new file .
  *-- ?? index
  
  
  
  lnRecNo= RECNO()
  IF SEEK(lcShipNo+'P'+lcponum+lcStyle+STR(lineno,6)+'2','POSLN')
  
    @ ROW,01 SAY 'Rec:'
    lnColNum=26
    FOR I=1 TO 8
      Z=STR(I,1)
      @ ROW,lnColNum SAY QTY&Z   PICTURE '@Z 99999'
      laSubRec(I)=laSubRec(I)+QTY&Z
      laTotRec(I)=laTotRec(I)+QTY&Z
      lnColNum=lnColNum+7
    ENDFOR
    @ ROW,82 SAY TOTQTY PICTURE '9999999'
    lnSubRec=lnSubRec+TOTQTY
    lnTotRec=lnTotRec+TOTQTY
    ROW=ROW+1
  ENDIF
  GO lnRecNo
  
  *** Print Damage Qty
  IF SEEK(lcShipNo+'P'+lcponum+lcStyle+STR(lineno,6)+'4','POSLN')
    @ ROW,01 SAY 'Dam:'
    lnColNum=26
    FOR I=1 TO 8
      Z=STR(I,1)
      @ ROW,lnColNum SAY QTY&Z   PICTURE '@Z 99999'
      laSubDam(I)=laSubDam(I)+QTY&Z
      laTotDam(I)=laTotDam(I)+QTY&Z
      lnColNum=lnColNum+7
    ENDFOR
    @ ROW,82 SAY TOTQTY PICTURE '9999999'
    lnSubDam=lnSubDam+TOTQTY
    lnTotDam=lnTotDam+TOTQTY
    ROW=ROW+1
  ENDIF
  GO lnRecNo
  *** Print Cancel Qty
  IF SEEK(lcShipNo+'P'+lcponum+lcStyle+STR(lineno,6)+'5','POSLN')
    @ ROW,01 SAY 'Can:'
    lnColNum=26
    FOR I=1 TO 8
      Z=STR(I,1)
      @ ROW,lnColNum SAY QTY&Z   PICTURE '@Z 99999'
      laSubCan(I)=laSubCan(I)+QTY&Z
      laTotCan(I)=laTotCan(I)+QTY&Z
      lnColNum=lnColNum+7
    ENDFOR
    @ ROW,82 SAY TOTQTY PICTURE '9999999'
    lnSubCan=lnSubCan+TOTQTY
    lnTotCan=lnTotCan+TOTQTY
    ROW=ROW+1
  ENDIF
  GO lnRecNo
  SELECT POSLN
ENDSCAN

IF ROW>52   &&Print Heading If Necessary
  ROW=0
  PAGENO=PAGENO+1
  DO RPT_HDR WITH 'STY651','',R_WIDTH
  @ 05,01 SAY 'SHIPMT# ENTERED  CARTONS AIRWAY BILL#    E.T.A.'   && MFM 12/26/94.
  @ 06,01 SAY REPLICATE('-',100)
  @ 07,01 SAY lcShipNo
  @ 07,09 SAY ldEntered
  @ 07,18 SAY lnCartons  PICTURE '9999'
  @ 07,26 SAY lcAirWayb
  @ 07,42 SAY ldETA
  @ 09,05 SAY '                       SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY'
  ROW=10
ENDIF

*** Print Subtotal Of Very Last PO
@ ROW,01 SAY REPLICATE('-',100)
ROW=ROW+1
@ ROW,01 SAY 'Sub Org '+lcPoNum+':'
lnColNum=25
FOR I=1 TO 8
  @ ROW,lnColNum SAY laSubOrg(I)  PICTURE '@Z 999999'
  lnColNum=lnColNum+7
ENDFOR
@ ROW,81 SAY lnSubOrg     PICTURE '9999999'
ROW=ROW+1
@ ROW,01 SAY 'Sub Rec '+lcPoNum+':'
lnColNum=25
FOR I=1 TO 8
  @ ROW,lnColNum SAY laSubRec(I)  PICTURE '@Z 999999'
  lnColNum=lnColNum+7
ENDFOR
@ ROW,81 SAY lnSubRec     PICTURE '9999999'

ROW=ROW+1
@ ROW,01 SAY 'Sub Dam '+lcPoNum+':'
lnColNum=25
FOR I=1 TO 8
  @ ROW,lnColNum SAY laSubDam(I)  PICTURE '@Z 999999'
  lnColNum=lnColNum+7
ENDFOR
@ ROW,81 SAY lnSubDam     PICTURE '9999999'
ROW=ROW+1
@ ROW,01 SAY 'Sub Can '+lcPoNum+':'
lnColNum=25
FOR I=1 TO 8
  @ ROW,lnColNum SAY laSubCan(I)  PICTURE '@Z 999999'
  lnColNum=lnColNum+7
ENDFOR
@ ROW,81 SAY lnSubCan     PICTURE '9999999'

ROW=ROW+2

*** Print Grand Total Of Original Shipment & Total Received
@ ROW,01 SAY REPLICATE('-',100)
ROW=ROW+1
@ ROW,01 SAY 'TOT. ORG SHIP:'
lnColNum=25
FOR I=1 TO 8
  @ ROW,lnColNum SAY laTotOrg(I)  PICTURE '@Z 999999'
  lnColNum=lnColNum+7
ENDFOR
@ ROW,81 SAY lnTotOrg     PICTURE '9999999'
ROW=ROW+1
@ ROW,01 SAY 'TOT. RECEIVED:'
lnColNum=25
FOR I=1 TO 8
  @ ROW,lnColNum SAY laTotRec(I)  PICTURE '@Z 999999'
  lnColNum=lnColNum+7
ENDFOR
@ ROW,81 SAY lnTotRec     PICTURE '9999999'
ROW=ROW+1
@ ROW,01 SAY 'TOT. DAMAGED :'
lnColNum=25
FOR I=1 TO 8
  @ ROW,lnColNum SAY laTotDam(I)  PICTURE '@Z 999999'
  lnColNum=lnColNum+7
ENDFOR
@ ROW,81 SAY lnTotDam     PICTURE '9999999'
ROW=ROW+1
@ ROW,01 SAY 'TOT. CANCELED:'
lnColNum=25
FOR I=1 TO 8
  @ ROW,lnColNum SAY laTotCan(I)  PICTURE '@Z 999999'
  lnColNum=lnColNum+7
ENDFOR
@ ROW,81 SAY lnTotCan     PICTURE '9999999'


*tttt
FUNCTION lfArrDumy
DIMENSION laSortDesc[3,1] , laSortVal[3,1]
laSortDesc[2,1] = "Style"
laSortDesc[3,1] = "Vendor"
laSortVal[2,1]  = "S"
laSortVal[3,1]  = "V"
laSortVal[1,1]  = "P" 

IF gcAct_Appl = "MF"
  laSortDesc[1,1] = "Dye Order"
ELSE
  laSortDesc[1,1] = "PO"
  DIMENSION laSortDesc[4,1] , laSortVal[4,1]
  laSortDesc[4,1] = "Shipment"
  laSortVal[4,1]  = "H"
ENDIF
lcPoDyeOrd = laSortDesc[1,1]
*-- 

*!*************************************************************
*! Name      : lfGetPoTyp
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/25/2001
*! Purpose   : Get the initial value of lcRpPoType variable.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : 
*!*************************************************************
*!B603625,1
FUNCTION lfGetPoTyp

PRIVATE lcRet
IF gcAct_Appl = "MF"
  lcRet = 'D'
ELSE
  lcRet = 'A'
ENDIF
RETURN lcRet
*--end of lfGetPoTyp.