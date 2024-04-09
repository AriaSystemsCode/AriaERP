*:***************************************************************************
*: Program file  : ARKRA700.PRG
*: Program desc. : Custom Customer History Report for Krazy Kat
*: For Report    : ARKRA70A.FRX , ARKRA70B.FRX
*: Date          : 03/11/2002
*: System        : Aria Advantage Series
*: Module        : Account Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reeference    : C200296,1 
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetCodes, lfEndGroup, lfwOldVal, lfvPO, lfvVend, lfGetLogo,
*:                 lfShiftArr
*:***************************************************************************
*: Notes         : This custom program is an exact copy of Customer History
*:                 Report but only we allowed to print the report in both
*:                 Graphics and Text format.
*:***************************************************************************
*: Example : DO ARKRA700
*:***************************************************************************

*C200296,1 Define 2 variables used in FRX. [Begin]
PRIVATE lcAcctGrp , llPrintLin , llPrintTot , lcAcctLine , llAcctChng
STORE '' TO lcAcctLine , lcAcctGrp
STORE .F. TO llPrintLin , llPrintTot , llAcctChng
*C200296,1 Define 2 variables used in FRX. [End]

*-- Variable used to print the account one time per history ID
llAccPrtd = .F.

*-- Variable hold the date period which to be printed on the .FRX
lcDatPrd  = SPACE(0)
lnDatapos = ASCAN(laOGFxFlt,'ARHIST.HISTDATE')
*-- Check if The user entered a date and get the date period to be printed.
IF lnDatapos > 0
  lnDatapos = ASUBSCRIPT(laOGFxFlt,lnDatapos,1)
  *-- The user whether entered the two date or the end date only.
  IF !EMPTY(STRTRAN(laOGFxFlt[lnDatapos,6],'|'))
    IF EMPTY(CTOD(LEFT(laOGFxFlt[lnDatapos,6],10)))
      lcDatPrd = "Period: To " + LEFT(laOGFxFlt[lnDatapos,6],10)
    ELSE
      lcDatPrd = "Period: " + LEFT(laOGFxFlt[lnDatapos,6],10) + " - "+RIGHT(laOGFxFlt[lnDatapos,6],10)
    ENDIF
  ENDIF
ENDIF
=lfCreatNdx()
IF llOGFltCh
  IF lcRpFrmt = 'A'
    *-- Get the maximum transaction # in each selected history number less than the chargeback (8)
    SELECT ACCOUNT,HISTORY,MAX(VAL(trantype)) AS NMAXTR WHERE VAL(trantype)<8 .AND. &lcRpExp ;
       FROM ARHIST GROUP BY history INTO CURSOR (lcTmpMaxN)
    SELECT (lcTmpMaxN)
    INDEX ON ACCOUNT+HISTORY TAG (lcTmpMaxN)
  ENDIF
ENDIF
SET ORDER TO TAG Ccode_no IN CODES
SELECT ARHIST
*-- set relation to get the related field ALLOW_TYPE directly from file
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER,;
   'N'+IIF(TRANTYPE='7','CCREDITCOD',IIF(TRANTYPE='2','TRANCODE  ',''))+arhist.trancode+SPACE(30)+'ALLOW_TYPE' INTO CODES
   
IF lcRpFrmt='A'
  SET RELATION TO ACCOUNT+HISTORY INTO (lcTmpMaxN)  ADDITIVE
ENDIF
GO TOP
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
ELSE
  *DO gfDispRe WITH EVAL('lcRpFormNa'),'FOR '+ IIF(EMPTY(lcRpExp) , .T. , lcRpExp)
  =gfDispRe(lcRpFormNa,'FOR '+ IIF(EMPTY(lcRpExp) , .T. , lcRpExp))
ENDIF
SELECT ARHIST
SET RELATION TO
*-- end of main code.

*!*************************************************************
*! Name      : lfwGrid
*! Developer : Ashraf Medhat
*! Date      : 08/04/1999
*! Purpose   : When OG Fuction.
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwGrid()
*!*************************************************************
FUNCTION lfwGrid

=lfvFormat()
*-- end of lfwGrid.

*!*************************************************************
*! Name      : lfwGrid
*! Developer : Ashraf Medhat
*! Date      : 08/04/1999
*! Purpose   : Report format validation
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwGrid()
*!*************************************************************
FUNCTION lfvFormat
*-- lcRpForm = (D or S) Details/Summary
*-- lcRpFrmt = (A or B)   
PRIVATE lnFormPos
lnFormPos = ASCAN(laOGObjType,'lcRpForm')
IF lnFormPos > 0
  *-- If Enable the Detail/Summary setting if form B only.
  lnFormPos = ASUBSCRIPT(laOGObjType,lnFormPos,1)
  laOGObjCnt[lnFormPos] = (lcRpFrmt = "B")
  
  *C200296,1 Default to form A. [Begin]
  lcRpForm = "D"
  *C200296,1 Default to form A. [End]

  =lfOGShowGet('lcRpForm')
ENDIF

*C200296,1 Change the report form name. [Begin]
*lcRpFormNa = "ARHIST" + lcRpFrmt
lcRpFormNa = "ARKRA70" + lcRpFrmt
*C200296,1 Change the report form name. [Begin]

= lfRepPltFr(lcRpFormNa)
*-- end of lfvFormat.
*
FUNCTION lfSetAcct
PARAMETERS lcParm
SET ORDER TO CUSTOMER IN CUSTOMER
GO TOP IN CUSTOMER

*-- end of lfSetAcct.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Ashraf Medhat
*! Date      : 08/04/1999
*! Purpose   : Close files when closing the option grid
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************

FUNCTION lfClearRep
IF FILE(gcWorkDir +lcArHist+ '.CDX') 
  SELECT ARHIST
  CLOSE INDEX
  ERASE (gcWorkDir +lcArHist+ '.CDX')
ENDIF
*-- Close the cursor if any
IF USED(lcTmpMaxN)
  USE IN (lcTmpMaxN)
ENDIF

*!*************************************************************
*! Name      : lfCreatNdx
*! Developer : Ashraf Medhat
*! Date      : 08/04/1999
*! Purpose   : Create a custom index on the ARHIST file
*!*************************************************************
*! Called from : ARHIST.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCreatNdx()
*!*************************************************************
FUNCTION lfCreatNdx
SELECT ARHIST
*-- If index file is already created 
IF !FILE(gcWorkDir +lcArHist+ '.CDX') 
  INDEX ON ACCOUNT+HISTORY+TRANTYPE+TRANCODE TAG cAHTTTag OF (gcWorkDir +  lcArHist + '.CDX')
ELSE
  SET ORDER TO TAG cAHTTTag OF (gcWorkDir +  lcArHist + '.CDX')
ENDIF

*!*************************************************************
*! Name      : lfAccPrtd
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 08/04/1999
*! Purpose   : to Print the customer one time per history ID
*!*************************************************************
*! Called from : ARHISTA.FRX, ARHISTB.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAccPrtd()
*!*************************************************************
FUNCTION lfAccPrtd
PARAMETERS lcCallFrm
llAccPrtd = (lcCallFrm = 'D')

*!*************************************************************
*! Name      : lfGetCodDes
*! Developer : 
*! Date      : 
*! Purpose   : 
*!*************************************************************
*! Called from : ARHISTA.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : lcTranCode
*!*************************************************************
*! Return      : lcRetVal
*!*************************************************************
*! Example     : = lfGetCodDes()
*!*************************************************************
FUNCTION lfGetCodDes
PARAMETERS lcTranCode
PRIVATE lcRetVal

lcRetVal = ''
DO CASE
  CASE TranType $ "579"
    lcRetVal = gfCodDes(lcTranCode,'CCREDITCOD',.T.)
  CASE TranType $ "28"
    lcRetVal = gfCodDes(lcTranCode,'TRANCODE',.T.)
  CASE TranType = '4'
    lcRetVal = ALLTRIM(Store)+IIF(!EMPTY(Store),'-','')+Desc 
  OTHERWISE
    lcRetVal = Desc
ENDCASE   
RETURN lcRetVal

*!**************************************************************************
*! Name      : lfAcctGrp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/21/2002
*! Purpose   : To reset llPrintLin flag.
*!**************************************************************************
*! Called from : ArKra70a.Frx DOS.
*!**************************************************************************
*! Example     : = lfAcctGrp()
*!**************************************************************************
*
FUNCTION lfAcctGrp
llPrintLin = .F.
lcAcctGrp = ''
*-- End of lfAcctGrp.

*!**************************************************************************
*! Name      : lfBeforLin
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/21/2002
*! Purpose   : To raise llPrintLin flag.
*!**************************************************************************
*! Called from : ArKra70a.Frx DOS.
*!**************************************************************************
*! Example     : = lfBeforLin()
*!**************************************************************************
*
FUNCTION lfBeforLin
llPrintLin = IIF((TranType='8' OR TranType='9') AND !(Account + History + TranType==lcAcctGrp),.T.,.F.)
lcAcctGrp = IIF(TranType='8' OR TranType='9',Account + History + TranType,'')
IF !(Account + History == lcAcctLine)
  llAcctChng = .T.
  lcAcctLine = Account + History
ENDIF
*-- End of lfBeforLin.

*!**************************************************************************
*! Name      : lfAfterLin
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/21/2002
*! Purpose   : To check whether to print Total or not.
*!**************************************************************************
*! Called from : ArKra70a.Frx DOS.
*!**************************************************************************
*! Example     : = lfAfterLin()
*!**************************************************************************
*
FUNCTION lfAfterLin
llAcctChng = .F.
llPrintTot = .F.
PRIVATE lcAcctHist , lcAccTrnTy
lcAcctHist = Account + History
lcAccTrnTy = TranType

SKIP 
  
IF (lcAcctHist == Account + History)
  IF TranType $ '89'
    llPrintTot = !(TranType = lcAccTrnTy)
  ELSE    
    llPrintTot = .F.
    IF !(TranType $ '1234567')
      llPrintTot = .T.
    ENDIF
  ENDIF
ELSE
  llPrintTot = .T.
  IF !(lcAccTrnTy $ '123456789')
    llPrintTot = .F.
  ENDIF
ENDIF
  
SKIP -1
*-- End of lfAfterLin.
