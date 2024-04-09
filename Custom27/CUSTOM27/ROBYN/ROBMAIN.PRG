*!**************************************************************************
*! Name      : ROBMAIN.PRG
*! Developer : AHMED MAHER
*! Date      : 01/01/2002
*! Purpose   : ROBYN MERDITH Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C102524,1
*!**************************************************************************
*! Modifications
*! C102524,1 AMH 01/01/2002 Custom broser of sales orders in C/T screen.
*! C102525,1 RAE 01/23/2002 view the PO returns issued against the selected PO.
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
lcFromPO = ''
*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : Ahmed Maher (AMH)
*! Date      : 01/01/2002
*! Purpose   : Custom broser of sales orders in C/T screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Due to C102524
*:*************************************************************
*
FUNCTION lfOrdBrow

IF !USED('ORDHDR')
  = gfOpenFile(gcDataDir+"OrdHdr" , "OrdHdr" , "SH")
ENDIF
IF !USED('ORDLINE')
  = gfOpenFile(gcDataDir+"ORDLINE" , "ORDLINE" , "SH")
ENDIF
IF !USED(lcStyleBrw)
  =gfOpenFile(gcDataDir+"STYLE", 'STYLE', "SH" , @lcStyleBrw , .T.)
ENDIF
IF !USED('Customer')
  = gfOpenFile(gcDataDir+"Customer" , "Customer" , "SH")
ENDIF

lcFields   = "cMarker=IIF(RECNO()=lnLnNo,'>',' '):1:R:H=' ':W=.F.,"+;
             "Order:R:H='Order#':"+ALLTRIM(STR(lnMinWid*2.1))+","+;
             "ORDHDR.Account:10:H='Account':R,"+;
             "CUSTOMER.STName:30:H='Name':R,"+;
             "ORDHDR.Complete:12:H='Comp. Date':R,"+;
             "lcCustPo=IIF(ORDHDR.multipo,ORDLINE.CUSTPO,ORDHDR.CUSTPO):20:H='Cust. Po':R,"+;
             lcStyleBrw+".PriceA:12:H='S. price':R,"+;
             "Qty1:6:H='Size1':R,"+"Qty2:6:H='Size2':R,"+"Qty3:6:H='Size3':R,"+;
             "Qty4:6:H='Size4':R,"+"Qty5:6:H='Size5':R,"+"Qty6:6:H='Size6':R,"+;
             "Qty7:6:H='Size7':R,"+"Qty8:6:H='Size8':R,"+"TotQty:6:H='Total':R"

SELECT (lcToBrow)
SET RELATION TO
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
SET RELATION TO 'O'+ORDER+CORDLINE INTO ORDLINE ADDITIVE
SET RELATION TO STYLE INTO (lcStyleBrw) ADDITIVE
SELECT OrdLine
SET RELATION TO IIF(EMPTY(OrdLine.Store),'M'+ORDLINE.Account,'S'+ORDLINE.Account+ORDLINE.Store) INTO Customer
SELECT (lcToBrow)
*-- end of lfOrdBrow.

*!*************************************************************
*! Name               : lfAddToTmp
*! Developer          : RANIA ABDEL RAZIK (RAE)
*! Date               : 01/23/2002
*! Purpose            : Add a new field to hold the PO number
*!*************************************************************
*! Calls              : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! C102525
*:*************************************************************

FUNCTION lfAddToTmp

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'cFromPO'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

*!*************************************************************
*! Name               : lfAppFrmPO
*! Developer          : RANIA ABDEL RAZIK (RAE)
*! Date               : 01/23/2002
*! Purpose            : Append the PO number in POFLN for TranCd = '1'
*!*************************************************************
*! Calls              : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! C102525
*:*************************************************************

FUNCTION lfAppFrmPO

REPLACE cFromPO WITH &lcPoline..POMAT

*!*************************************************************
*! Name               : lfAddRetLn
*! Developer          : RANIA ABDEL RAZIK (RAE)
*! Date               : 01/24/2002
*! Purpose            : Add The Returned lines to the Receiving 
*!                      lines browse in PO screen.
*!*************************************************************
*! Calls              : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! C102525
*!*************************************************************

FUNCTION lfAddRetLn

lcBrowFlds = [cMarker=IIF(RECNO('POFLN')=lnRecRec,'>',''):H='':1,cRsession:R:H='Session',]+;
             [Tran=IIF(cMatType='R','Returned',IIF(cFabGrade='1',IIF(TranCd='4','First quality (Canceled)','First quality'),IIF(cFabGrade='2','Second quality',IIF(cFabGrade='3','Damaged','')))):R:H='Receiving type':18,]+;
             [CtranDate=IIF(cMatType='R' ,EVAL(lcTmpHdr+'.ENTERED'),Date):R:H='Date' :10,] +;
             [Item=IIF(TranCd $ '24' OR cMatType='R',Fabric,cFabric1) :R:H='Item' :7,]+;
             [Clr =IIF(TranCd $ '24' OR cMatType='R',Color ,cColor1 ) :R:H='Color':6,]+;
             IIF(llDyelot,[Dyelot:R:H='Dyelot':10,],'')+;
             IIF(llMultiWH,[cWareCode:R:H='Location':11,],'')+;
             [nFabTotQty:R:13:H='Quantity':P='9999999.999',]+;
             [Amount=IIF(cMatType ='R',Ncost1,nLan_Cost1)*nFabTotQty:16:R:H='Amount':P='9999999999999.999']
lcFlt = "(cMatType+POMat+Fabric+Color+TranCd = 'P'+laData[1]+ &lcDetTmp..Fabric + &lcDetTmp..Color ;
          AND TranCd <> '1');
         OR (cMatType+POMAT+Fabric+Color+TranCd = 'R' AND cFrompo+Fabric+Color=laData[1]+ &lcDetTmp..Fabric + &lcDetTmp..Color AND TranCd='1')"
*!*************************************************************
*! Name               : lfPoHAlias
*! Developer          : RANIA ABDEL RAZIK (RAE)
*! Date               : 01/24/2002
*! Purpose            : Open POFHDR with new name
*!*************************************************************
*! Calls              : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! C102525
*!*************************************************************

FUNCTION lfPoHAlias

lnAlias = SELECT()

llTmpHdr = gfOpenFile(gcDataDir+'POFHDR',gcDataDir+'POFHDR','SH',@lcTmpHdr,.T.)
SELECT POFLN
SET RELATION TO cMatType+POMat INTO (lcTmpHdr)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/10/2002
*! Purpose   : Valid function to validate cordercut field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Due to C102581
*:*************************************************************
*
FUNCTION lfvOrder
PARAMETER llRetVal

PRIVATE llRetVal,lnAlias,lnOrderPos,lcOrder
STORE .T. TO llRetVal

lnAlias = SELECT(0)
lcVar = SYS(18)
IF TYPE('LAUSRFIELD') = "U"
  RETURN
ENDIF
lnOrderPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CORDERCUT'),1)
IF !(lcVar == "LAOGFXFLT(" + ALLTRIM(STR(lnOrderPos)) + ",6)")
  RETURN
ENDIF
lcOrder = EVALUATE(lcVar)
IF !USED('ORDHDR')
  = gfOpenFile(gcDataDir+"OrdHdr" , "OrdHdr" , "SH")
ENDIF
IF !USED('Customer')
  = gfOpenFile(gcDataDir+"Customer" , "Customer" , "SH")
ENDIF
IF !EMPTY(lcOrder) .AND. !SEEK('O'+lcOrder,'ORDHDR')
  =lfBrowOrd(@lcOrder,'','','')
  laUsrFields[lnOrderPos,6] = lcOrder
  laOGFxFlt[lnOrderPos,6] = lcOrder
ENDIF
IF !EMPTY(lcOrder) .AND. !(lcOrder == lcOldValue)
  SELECT ORDHDR
  SEEK 'O'+lcOrder
  =SEEK('M'+ACCOUNT,'CUSTOMER')
  lnAccPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CACCOUNT'),1)
  IF lnAccPos > 0
    laUsrFields[lnAccPos,6] = ORDHDR.ACCOUNT
    laOGFxFlt[lnAccPos,6] = ORDHDR.ACCOUNT
  ENDIF
  lnAcnPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CACCNAME'),1)
  IF lnAcnPos > 0
    laUsrFields[lnAcnPos,6] = CUSTOMER.BTNAME
    laOGFxFlt[lnAcnPos,6] = CUSTOMER.BTNAME
  ENDIF
  lnDelPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'DDEL_DATE'),1)
  IF lnDelPos > 0
    laUsrFields[lnDelPos,6] = ORDHDR.COMPLETE
    laOGFxFlt[lnDelPos,6] = ORDHDR.COMPLETE
  ENDIF
  lnCutPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CCUSTPO'),1)
  IF lnCutPos > 0
    laUsrFields[lnCutPos,6] = ORDHDR.CUSTPO
    laOGFxFlt[lnCutPos,6] = ORDHDR.CUSTPO
  ENDIF
  CLEAR READ
ENDIF
SELECT (lnAlias)
RETURN llRetVal
*-- end of lfvOrder.

*!*************************************************************
*! Name      : lfBrowOrd
*! Developer : AMH (AHMED MAHER)
*! Date      : 03/10/2002
*! Purpose   : Browse sales order
*!*************************************************************
*! Due to C102581
*:*************************************************************
FUNCTION lfBrowOrd
PARAMETERS lcOrder,lcAccount,lcStore,lcCustPo

SELECT ORDHDR
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE

lcBrFields = [ORDHDR.Order:H="Order#",ORDHDR.Status:1:H="Status",ORDHDR.Account:H="Acct",]+;
             [Store=IIF(ORDHDR.MULTI='Y','*Multi*',ORDHDR.STORE):H="Store",]+;
             [Customer.stname:30:H="Name",]
lcBrFields = lcBrFields + [ORDHDR.Open:H="Open.Qty.",ORDHDR.OpenAmt:H="Open.Amt.",]+;
             [ORDHDR.Ship:H="Ship.Qty.",ORDHDR.ShipAmt:H="Ship.Amt.",]+;
             [ORDHDR.Book:H="Book.Qty.",ORDHDR.BookAmt:H="Book.Amt.",]+;
             [ORDHDR.Cancel:H="Cancel.Qty.",ORDHDR.CancelAmt:H="Cancel.Amt."]
lcBrFields = lcBrFields + [,ORDHDR.Entered:H="Entered",ORDHDR.Start:H="Start",ORDHDR.Complete:H="Complete",]+;
             [ORDHDR.Cancelled:H="Cancelled",]+;
             [ORDHDR.cWareCode:H="Warehouse",]+;
             [lcSesDesc=gfCodDes(ORDHDR.Season,'SEASON'):H="Season",]+;
             [lcDivDesc=gfCodDes(ORDHDR.cDivision,'CDIVISION'):H="Division",]+;
             [lcShipVia=gfCodDes(ORDHDR.ShipVia,'SHIPVIA'):H="ShipVia"]
lcOrder = IIF(ARIABROW("'O'","Orders",gnBrFSRow1,;
                       gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),OrdHdr.Order,SPACE(6))
SET RELATION OFF INTO CUSTOMER
*-- end of lfBrowOrd.

*!*************************************************************
*! Name      : lfwOrder
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/10/2002
*! Purpose   : When function of cordercut field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Due to C102581
*:*************************************************************
*
FUNCTION lfwOrder
PARAMETER llRetVal

PRIVATE llRetVal
llRetVal = IIF(SUBSTR(gcbasewind,4) = "MFCUTKT",.T.,.F.)
lcOldValue = EVALUATE(SYS(18))
RETURN llRetVal
*-- end of lfWOrder.

*!*************************************************************
*! Name      : lfGetDefa
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/11/2002
*! Purpose   : Function to get the defaulte value of nsel_price field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Due to C102581
*:*************************************************************
*
FUNCTION lfGetDefa

IF !EMPTY(PADR(ALLTRIM(laData[2]),lnStyleWid))
  lnSelPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'NSEL_PRICE'),1)
  IF lnSelPos > 0 .AND. SEEK(PADR(ALLTRIM(laData[2]),lnStyleWid),'STYLE')
    laUsrFields[lnSelPos,6] = STYLE.PRICEA
  ENDIF
ENDIF
*-- end of lfGetDefa.

*!*************************************************************
*! Name      : lfIniUsrFl
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/11/2002
*! Purpose   : Initialize the user defined fields.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Due to C102581
*:*************************************************************
*
FUNCTION lfIniUsrFl

lnOrderPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CORDERCUT'),1)
IF lnOrderPos > 0
  laUsrFields[lnOrderPos,6] = ""
ENDIF
lnAccPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CACCOUNT'),1)
IF lnAccPos > 0
  laUsrFields[lnAccPos,6] = ""
ENDIF
lnAcNPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CACCNAME'),1)
IF lnAcNPos > 0
  laUsrFields[lnAcNPos,6] = ""
ENDIF
lnDelPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'DDEL_DATE'),1)
IF lnDelPos > 0
  laUsrFields[lnDelPos,6] = {}
ENDIF
lnCutPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CCUSTPO'),1)
IF lnCutPos > 0
  laUsrFields[lnCutPos,6] = ""
ENDIF
lnSelPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'NSEL_PRICE'),1)
IF lnSelPos > 0
  laUsrFields[lnSelPos,6] = 0
ENDIF
*-- end of lfIniUsrFl.

*!**************************************************************************
*! Name      : lfGetProft
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/11/2002
*! Purpose   : calculate the total selling price & gros margin when call 
*!             Profitability screen
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfGetProft()
*!**************************************************************************
FUNCTION lfGetProft
Private lnRecNo , lnAvrgPrc,lnTotQty,llMastMode
lnAlias=SELECT(0)

llMastMode = laScrMode[1] OR laScrMode[2]
lcFile     = IIF(llMastMode, "CutTktL", lcTmpCutLn)
SELECT (lcFile)
lnRecNo = RECNO()
STORE 0 TO lnPoctSel,lnUnitSel,lnTotQty,lnuntMrgn1,lnuntMrgn2,lnuntMrgn3,lnRotSub,lnTotcost

*--- the total quantity for CT
SUM TotQty TO lnTotQty

*--- selling price for CT
lnSelPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'NSEL_PRICE'),1)
IF lnSelPos > 0
  lnUnitSel = laUsrFields[lnSelPos,6]          && selling price
  lnPoCtSel = lnUnitSel * lnTotQty
ENDIF

*--- ESTIMATED
lnTotcost  = laData[19]+laData[20]+laData[21]+laData[22]+laData[23]
lnRotSub   = IIF(llStyMark,lnTotcost,lnPoCtSel)
lnuntMrgn1 = IIF(lnRotSub = 0,0,((lnPoCtSel - lnTotcost)/lnRotSub)*100)   && Gross Margin

*--- LANDED
lnTotcost  = laData[24]+laData[25]+laData[26]+laData[27]+laData[28]
lnRotSub   = IIF(llStyMark,lnTotcost,lnPoCtSel)
lnuntMrgn2 = IIF(lnRotSub = 0,0,((lnPoCtSel - lnTotcost)/lnRotSub)*100)   && Gross Margin

*--- ACTUAL
lnTotcost  = laData[29]+laData[30]+laData[31]+laData[32]+laData[33]
lnRotSub   = IIF(llStyMark,lnTotcost,lnPoCtSel)
lnuntMrgn3 = IIF(lnRotSub = 0,0,((lnPoCtSel - lnTotcost)/lnRotSub)*100)   && Gross Margin

SELECT (lcFile)
IF BETWEEN(lnRecNo, 1, RECCOUNT())
  GO  lnRecNo
ELSE
  LOCATE
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/19/2002
*! Purpose   : Valid function to validate cordercut field.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Due to C102581
*:*************************************************************
*
FUNCTION lfvAccount
PARAMETER llRetVal

PRIVATE llRetVal,lnAlias,lnAccPos,lcAccount
STORE .T. TO llRetVal

lnAlias = SELECT(0)
lcVar = SYS(18)
IF TYPE('LAUSRFIELD') = "U"
  RETURN
ENDIF
lnAccPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CACCOUNT'),1)
IF !(lcVar == "LAOGFXFLT(" + ALLTRIM(STR(lnAccPos)) + ",6)")
  RETURN
ENDIF
lcAccount = EVALUATE(lcVar)
IF !(lcAccount == lcOldValue)
  IF !USED('Customer')
    = gfOpenFile(gcDataDir+"Customer" , "Customer" , "SH")
  ENDIF
  =SEEK('M'+lcAccount,'CUSTOMER')
  lnAcnPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CACCNAME'),1)
  IF lnAcnPos > 0
    laUsrFields[lnAcnPos,6] = CUSTOMER.BTNAME
    laOGFxFlt[lnAcnPos,6] = CUSTOMER.BTNAME
  ENDIF
  CLEAR READ
ENDIF
SELECT (lnAlias)
RETURN llRetVal
*-- end of lfvAccount.

*!**************************************************************************
*! Name      : lfSaveFld
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/25/2002
*! Purpose   : Saving Receive by (Lot/Invoice)
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Due to C102581
*:**************************************************************************
*
FUNCTION lfSaveFld

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpLine)
LOCATE
=lfUpdUsrFl('CORDERCUT',CUTTKTH.CORDERCUT)
=lfUpdUsrFl('CACCOUNT',CUTTKTH.CACCOUNT)
=lfUpdUsrFl('CACCNAME',CUTTKTH.CACCNAME)
=lfUpdUsrFl('DDEL_DATE',CUTTKTH.DDEL_DATE)
=lfUpdUsrFl('CCUSTPO',CUTTKTH.CCUSTPO)
=lfUpdUsrFl('NSEL_PRICE',CUTTKTH.NSEL_PRICE)
SELECT (lnAlias)
*-- end of lfSaveFld.

*!**************************************************************************
*! Name      : lfUpdUsrFl
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/25/2002
*! Purpose   : Updating User defined fields.
*!**************************************************************************
*! Reference : C102581
*!**************************************************************************
*! Example   : =lfUpdUsrFl()
*!**************************************************************************
*
FUNCTION lfUpdUsrFl
PARAMETERS lcFldName,lcFldValue

lnFldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,lcFldName),1)
laUsrFields[lnFldPos,6] = lcFldValue
*-- end of lfUpdUsrFl.