*:***************************************************************************
*: Program file  : ARSHPMN
*: Program desc. : Shipping Manifest Report
*: For Report    : ....
*: System        : Aria 4XP
*: Module        : Account Receivable (AR)
*: Developer     : Walid Hamed  (WLD)
*: Date          : 03/15/2007
*:***************************************************************************
*: Calls :
*:    Procedures : lpPrnCode,CusBrowM
*:    Functions  : gfModalGen,gfPhoneTem,gfOpenFile,gfBrows,lfwRepWhen,lfAdrShift,lfWorkFile,
*:               : lfvAcct,lfvCons,lfwOldVal,lfEvalVars,lfSRInv,lfvInv,lfClearSel,lfCollTime,
*:               : lfClearRep,lfCollectDetail.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARSHPMN
*:***************************************************************************
*: This Report Program is due to N000599 ... T20061226.0026 
*----------------------- Report Code Begin -----------------------------

lcStTime = TIME()                          && Time in which we start collect data.
XINV     = IIF(lcRpInvTyp='Y',lcRpInv,'')  && Variable hold Consolidated Invoice.

XCONS = IIF(lcRpInvTyp='Y','Y','N')

&&*C102522,1 Check if Customer is Revue. [Begin]
&&PRIVATE llRevue
&&llRevue = gfDoTriger('ARSHPMN',PADR('REVUE',10))
&&*C102522,1 Check if Customer is Revue. [End]

*-- if user change report expression.
IF llClearFn OR llOGFltCh
  STORE .F. TO llClearFn,llChInv  && Avoid Recollecting data again.
  lcLastExpr = lcRpExp   && Save current report expression, To later know that user change critria.
  lcOldAcct  = lcRpAcct
  lcOldType  = lcRpInvTyp
  lcOldCurr  = lcRpCurr
  IF TYPE("loogscroll.lcLogoPath") = 'C' .AND. !EMPTY(loogscroll.lcLogoPath)
    loogscroll.lcLogoPath=''
  ENDIF
  *-- No Selection or Consolidated invoice
  IF '.T.' $ lcRpExp

    IF lcRpInvTyp='Y'
      lcRpExp = [INVOICE = XINV]
    ENDIF

  ELSE  && Normal (Single Store/Order Invoice)

    lcRpExp = STRTRAN(lcRpExp,'INVHDR.','')

  ENDIF

  *-- if you have previous data clear workfile then recreate it. [begin]
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF
    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear workfile then recreate it. [end]
  SELECT INVHDR
  gfSetOrder('INVHDR')
  SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER

  m.Flag = 'Y'
  SCAN FOR &lcRpExp

    SCATTER MEMVAR FIELDS ;
      Invoice , Account, STORE, CustPo, Note1, Note2, ;
      Weight, Cartons, Ship, ShipAmt, Discount
    m.ConSol = IIF(EMPTY(CONSOL),'N',CONSOL)

    *-- Get customer Addresses [Begin
    laCustAdd[1] = Customer.BtName
    laCustAdd[2] = Customer.cAddress1
    laCustAdd[3] = Customer.cAddress2
    laCustAdd[4] = IIF(EMPTY(Customer.cAddress3),'', PADR(ALLTRIM(Customer.cAddress3),12))          +;
      IIF(EMPTY(Customer.cAddress4),'', ' - ' + PADR(ALLTRIM(Customer.cAddress4),6)) +;
      IIF(EMPTY(Customer.cAddress5),'', ' - ' + PADR(ALLTRIM(Customer.cAddress5),12))
    laCustAdd[5] = IIF(EMPTY(Customer.Phone1),'',TRANSFORM(Customer.Phone1 ,'@R '+ lcPhonPict))

    = lfAdrShift('laCustAdd')    && Shift Company address if there is empty line.

    FOR lnI = 1 TO 5
      lcI = STR(lnI,1)
      m.HLine&lcI = IIF(lnI > ALEN(laCustAdd,1),'',laCustAdd[lnI])
    ENDFOR
    *-- Get customer Addresses [End..
    *Get Store addresses
    =gfseek('S'+m.ACCOUNT+m.STORE,'CUSTOMER')
    m.cStCity = PADR(ALLTRIM(CUSTOMER.cAddress3),19)
    m.cStState= ALLTRIM(CUSTOMER.cAddress4)


    INSERT INTO (lcWorkFile) FROM MEMVAR


  ENDSCAN

  SELECT INVHDR
  SET RELATION TO

  *-COLLECT DETAIL Inv Line
  =lfCollectDetail()

  SELECT (lcWorkFile)
  COPY TO (oAriaApplication.WorkDir+lcShpMnfHdr+'.DBF') WITH CDX
  USE IN (lcShpMnfLine)
ENDIF

IF RECCOUNT(lcWorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok >
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

*-- Do Report Program [Start
DO lpPrnCode
*-- Do Report Program [End..

*-- end of report code.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Walid Hamed  (WLD)
*! Date      : 03/15/2007
*! Purpose   : OG when function
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*!
FUNCTION lfwRepWhen

  lcShpMnfHdr =loOGScroll.gfTempName()
  lcShpMnfLine = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir+lcShpMnfLine) ;
    (Invoice C(6), Account C(5), STORE C(8),cStCity C(30),cStState C(30),STYLE C(19),StyDesc c(20),TotQty N(6),Price N(12,2),UAmnt N(12,2))
  SELECT (lcShpMnfLine)
  INDEX ON INVOICE+STYLE TAG (lcShpMnfLine)

  *-- if it's first time to run the report.
  IF TYPE('lcLastExpr') = 'N'

    R_WIDTH = 'N'

    DECLARE laCompAdd[5],laCustAdd[5]
    laCompAdd = ''                   && Array to hold the Company  address
    laCustAdd = ''                   && Array to hold the Customer address

    *-- Get company Address [begin].
    SELECT SYCCOMP
    =gfSeek(oAriaApplication.ActiveCompanyID)
    lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.

    laCompAdd[1] = cCom_Name
    laCompAdd[2] = cAddress1
    laCompAdd[3] = cAddress2
    laCompAdd[4] = IIF(EMPTY(cAddress3),'', PADR(ALLTRIM(cAddress3),12))          +;
      IIF(EMPTY(cAddress4),'', ' - ' + PADR(ALLTRIM(cAddress4),6)) +;
      IIF(EMPTY(cAddress5),'', ' - ' + PADR(ALLTRIM(cAddress5),12))
    laCompAdd[5] = IIF(EMPTY(cCom_Phon),'',TRANSFORM(cCom_Phon ,'@R '+ lcPhonPict))

    = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
    *-- Get company Address [end].

    IF llMultCurr AND EMPTY(lcRpCurr)
      lcRpCurr = gcBaseCurr
    ENDIF

    =lfWorkFile()

  ENDIF  && END IF you first time enter when function.

  *-- Preview and Run buttons are enabled if we have data to preview.
  IF EMPTY(lcRpAcct) OR (lcRpInvTyp='Y' AND EMPTY(lcRpInv)) OR ;
      (EMPTY(lcRpInvTyp) AND !(USED(laOGFxFlt[1,6]) AND RECCOUNT(laOGFxFlt[1,6]) > 0))
    loogscroll.PARENT.ogtoolbar.cntPrint.cmdPrint.ENABLED    = .F.
    loogscroll.PARENT.ogtoolbar.cntPrint.cmdPreview.ENABLED  = .F.
    loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .F.
    loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled   = .F.
  ENDIF

  *-- end of lfwRepWhen.

  *!*************************************************************
  *! Name      : lfAdrShift
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : Function to Shift the Address array if there is any
  *!             empty lines in the address
  *!*************************************************************
  *! Called from : Program code, OG When
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : Array name
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfAdrShift()
  *!*************************************************************
  *!
FUNCTION lfAdrShift
  PARAMETERS lcArrayNam

  *FOR Loop to loop the Address Array
  FOR lnCount = 1 TO ALEN(&lcArrayNam)

    *IF The current Array element is of type character and empty
    IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
        EMPTY(&lcArrayNam.[lnCount])

      =ADEL(&lcArrayNam , lnCount)
      lnCount = lnCount - 1
    ENDIF    && End of IF
  ENDFOR    && End of FOR Loop

  *FOR Loop to loop the Address Array
  FOR lnCount = 1 TO ALEN(&lcArrayNam)

    *IF The current Array element is not of type character
    IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
      &lcArrayNam.[lnCount] = ''
    ENDIF    && End of IF
  ENDFOR    && End of FOR Loop
  *-- end of lfAdrShift.

  *!*************************************************************
  *! Name      : lfWorkFile
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : Create work cursor.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Report code.
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : =lfWorkFile()
  *!*************************************************************
  *!
FUNCTION lfWorkFile
  CREATE TABLE (oAriaApplication.WorkDir+lcWorkFile) ;
    (Invoice C(6), Account C(5), STORE C(8),cStCity C(30),cStState C(30), CONSOL C(1), CustPo C(15), Note1 C(30), Note2 C(30),;
    Weight N(6,0), Cartons N(5,0), Ship N(7,0), ShipAmt N(13,2), Discount N(13,2),;
    HLine1 C(30), HLine2 C(30), HLine3 C(30), HLine4 C(30), HLine5 C(30), FLAG C(1))

  SELECT (lcWorkFile)
  INDEX ON INVOICE TAG (lcWorkFile)

  CREATE TABLE (oAriaApplication.WorkDir+lcShpMnfLine) ;
    (Invoice C(6), Account C(5), STORE C(8),cStCity C(30),cStState C(30),STYLE C(19),StyDesc c(20),TotQty N(6),Price N(12,2),UAmnt N(12,2))
  SELECT (lcShpMnfLine)
  INDEX ON INVOICE+STYLE TAG (lcShpMnfLine)

  *-- end of lfWorkFile.

  *!*************************************************************
  *! Name      : lfvAcct
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : Account Validation.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Report code.
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : =lfvAcct()
  *!*************************************************************
  *!
FUNCTION lfvAcct
  PRIVATE lcObjVal
  IF !(lcRpAcct == laOldVal)
    PRIVATE lnAlsNo,lcCustOrd,lcObjName
    lnAlsNo = SELECT(0)
    SELECT CUSTOMER
    lcCustOrd = ORDER()
    gfSetOrder('CUSTOMER')
    IF '?' $ lcRpAcct .OR. (!EMPTY(lcRpAcct) .AND. !GFSEEK('M' + lcRpAcct , 'CUSTOMER'))
      llObjRet = CusBrowM(@lcRpAcct , '' , 'M')
      lcObjVal = IIF(llObjRet , lcRpAcct , laOldVal)
    ENDIF    && End of IF

    IF !(lcRpAcct == laOldVal)
      =lfClearSel()

      IF lcRpInvTyp = 'Y'
        lcRpInv = ''
      ELSE
        IF USED(laOGFxFlt[1,6])
          PRIVATE lcSavAlias
          lcSavAlias = ALIAS()
          SELECT (laOGFxFlt[1,6])
          ZAP
          SELECT (lcSavAlias)
        ENDIF
      ENDIF
      CLEARREAD()   && Clear current read cycle, to activate new one.

    ENDIF
    SELECT CUSTOMER
    gfsetOrder(lcCustOrd)
    SELECT(lnAlsNo)
  ENDIF

  *Blank the Invoice temp name. [Begin]
  IF !EMPTY(lcRpAcct) AND lcRpInvTyp = "Y"
    laOGFxFlt[1,6] = ''
  ENDIF
  *-- end of lfvAcct.

  *!*************************************************************
  *! Name      : lfvCons
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : Single (Order/Store) Invoice / Consolidated Invoice Validation.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Report code.
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : =lfvCons()
  *!*************************************************************
  *!
FUNCTION lfvCons

  *-- if single type.
  IF EMPTY(lcRpInvTyp)
    lcRpInv = SPACE(6)
  ENDIF

  *Blank the Invoice temp name.
  IF lcRpInvTyp = "Y"
    laOGFxFlt[1,6] = ''
  ENDIF
  CLEARREAD()  && Clear current read cycle, to activate new one.
  *-- end of lfvCons.

  *!*************************************************************
  *! Name      : lfwOldVal
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : Capture current object old value.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : All Validations.
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : =lfwOldVal()
  *!*************************************************************
  *!
FUNCTION lfwOldVal

  *  IF !EMPTY(SYS(18))
  *    laOldVal = EVALUATE(SYS(18))
  *  ENDIF
  *-- end of lfwOldVal.

  *!*************************************************************
  *! Name      : lfEvalVars
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : Fill Default values used in both OG and Report.
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : gfOpenFile
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : ....
  *!*************************************************************
  *! Example     : = lfEvalVars()
  *!*************************************************************
  *!
FUNCTION lfEvalVars

  *-- if multi currency evaluate currency arrays [Begin]
  IF llMultCurr
    PRIVATE llOpenCurr
    llOpenCurr = .F.

    DIMENSION laCurrVal[1,1]

    IF !USED('SYCCURR')
      llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
    ENDIF

    SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
    DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

    SELECT SYCCURR
    gfSetOrder('CCURRCODE')
    FOR lnI = 1 TO ALEN(laCurrVal,1)
      = GFSEEK(ALLTRIM(laCurrVal[lnI,1]))
      laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
    ENDFOR

    IF llOpenCurr
      USE IN SYCCURR
    ENDIF

  ENDIF
  *-- end of lfEvalVars.

  *!*************************************************************
  *! Name      : lfSRInv
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : control browse Invoices for InvHdr File
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : =lfSRInv()
  *!*************************************************************
  *! Note      : SR symbol is [S,Set--R,Reset]
  *!*************************************************************
  *!
FUNCTION lfSRInv
  PARAMETERS lcParm

  PRIVATE lnAlias,lcInvFlt
  lnAlias = SELECT(0)

  SELECT CUSTOMER
  gfSetOrder('CUSTOMER')
  SELECT INVHDR
  gfSetOrder('INVHDRA')

  SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER
  lcInvFlt = FILTER()

  DO CASE

    CASE lcParm = 'S'  && Set code
      llChInv = .T.

      SET FILTER TO ACCOUNT + INVOICE = IIF(EMPTY(lcRpAcct),'',lcRpAcct)    AND ;
        STATUS <> 'V'                                            AND ;
        IIF(lcRpInvTyp='Y', CONSOL = lcRpInvTyp , CONSOL $ ' N') AND ;
        IIF(llMultCurr,CCURRCODE = PADR(lcRpCurr,3),.T.)

      gfSetOrder('INVHDR')
      GO TOP

    CASE lcParm = 'R'  && Reset code
      llClearInv = .F.
      SET FILTER TO &lcInvFlt
      SET RELATION TO
      IF USED(laOGFxFlt[1,6])
        SELECT (lnAlias)


        *-- if user select invoices.
        IF RECCOUNT(laOGFxFlt[1,6]) > 0

          IF RECCOUNT(laOGFxFlt[1,6]) = 1
            GO TOP IN (laOGFxFlt[1,6])
            IF EMPTY(EVALUATE(laOGFxFlt[1,6]+'.Invoice'))
              loogscroll.PARENT.ogtoolbar.cntPrint.cmdPrint.ENABLED    = .F.
              loogscroll.PARENT.ogtoolbar.cntPrint.cmdPreview.ENABLED  = .F.
              loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .F.
              loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled   = .F.
            ELSE
              loogscroll.PARENT.ogtoolbar.cntPrint.cmdPrint.ENABLED    = .T.
              loogscroll.PARENT.ogtoolbar.cntPrint.cmdPreview.ENABLED  = .T.
              loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .T.
              loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled   = .T.

            ENDIF
          ELSE
            loogscroll.PARENT.ogtoolbar.cntPrint.cmdPrint.ENABLED   = .T.
            loogscroll.PARENT.ogtoolbar.cntPrint.cmdPreview.ENABLED = .T.
           loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .T.
            loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled  = .T.

          ENDIF
        ELSE  && else user does not select invoices.

          loogscroll.PARENT.ogtoolbar.cntPrint.cmdPrint.ENABLED    = .F.
          loogscroll.PARENT.ogtoolbar.cntPrint.cmdPreview.ENABLED  = .F.
          loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .F.
          loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled   = .F.

        ENDIF
      ENDIF

  ENDCASE
  *-- end of lfSRInv.

  *!*************************************************************
  *! Name      : lfvInv
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : Validate consolidated invoice.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : =lfvInv()
  *!*************************************************************
  *!
FUNCTION lfvInv
  PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag , lcCstmrTag

  lcInvHdTag = ORDER('INVHDR')
  lcCstmrTag = ORDER('CUSTOMER')
  SELECT INVHDR
  gfSetOrder('INVHDR')
  SELECT CUSTOMER
  gfSetOrder('CUSTOMER')

  *IF The user want to Browse or if the Account he entered is not in the file
  *  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !GFSEEK(lcObjVal , 'INVHDR')) OR ;
  (!EMPTY(lcObjVal) .AND. GFSEEK(lcObjVal , 'INVHDR') AND InvHdr.CONSOL <> "Y")
  IF '?' $ lcRpInv .OR. (!EMPTY(lcRpInv) .AND. !GFSEEK(lcRpInv , 'INVHDR')) OR ;
      (!EMPTY(lcRpInv) .AND. GFSEEK(lcRpInv , 'INVHDR') AND InvHdr.CONSOL <> "Y")

    lcBrFields = "Invoice :R :H= 'Invoice' , " +;
      "Printed = IIF(PrtFlag = 'P' , 'Yes' , 'No') :R :H= 'Printed' , " +;
      "InvDate :R :H= 'Date' , " +;
      "Account :R :H= 'Account' , " +;
      "Order   :R :H= 'Order' , " +;
      "CustPO  :R :H= 'Reference' , " +;
      "CUSTOMER.BTName :R :H= 'Bill to' , " +;
      "Rep1    :R :H= 'Sales Rep.' , " +;
      "Ship    :R :H= 'Pieces' , " +;
      "ShipAmt :R :H= 'Merchandise'"

    lcFile_Ttl = 'Receivable invoices'

    lcBrowCond = [FOR ACCOUNT = lcRpAcct AND STATUS <> "V" AND CONSOL="Y" AND ] +;
      [IIF(llMultCurr,CCURRCODE = PADR(lcRpCurr,3),.T.)]

    SELECT INVHDR
    SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
    DECLARE laRetVal[1]


    IF gfBrows(lcBrowCond , 'Invoice' , 'laRetVal')
      * &lcObjName = laRetVal[1]
      lcRpInv = laRetVal[1]
    ELSE    && Else
      *   &lcObjName = laOldVal
      lcRpInv = laOldVal
    ENDIF    && End of IF

    SET RELATION OFF INTO CUSTOMER
  ENDIF    && End of IF

  *IF The INVHDR file did not have an active index
  IF EMPTY(lcInvHdTag)
    SELECT INVHDR
    gfSetOrder('')
  ELSE    && Else
    SET ORDER TO TAG (lcInvHdTag) IN INVHDR
    SELECT INVHDR
    gfSetOrder(lcInvHdTag)
  ENDIF    && End of IF

  *IF The CUSTOMER file did not have an active index
  IF EMPTY(lcCstmrTag)
    SELECT CUSTOMER
    gfSetOrder('')
  ELSE    && Else
    SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
    SELECT CUSTOMER
    gfSetOrder(lcCstmrTag)
  ENDIF    && End of IF
  *-- if no invoices selected.
  IF EMPTY(lcRpInv)
    loogscroll.PARENT.ogtoolbar.cntPrint.cmdPrint.ENABLED    = .F.
    loogscroll.PARENT.ogtoolbar.cntPrint.cmdPreview.ENABLED  = .F.
    loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .F.
    loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled   = .F.


  ELSE  && else user select invoice.
    loogscroll.PARENT.ogtoolbar.cntPrint.cmdPrint.ENABLED    = .T.
    loogscroll.PARENT.ogtoolbar.cntPrint.cmdPreview.ENABLED  = .T.
    loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .T.
    loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled   = .T.

  ENDIF
  *-- end of lfvInv.

  *!*************************************************************
  *! Name      : lfClearSel
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : User change one of the following [Account, Invoice Type, or Currency]
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : =lfClearSel()
  *!*************************************************************
  *!
FUNCTION lfClearSel
  llClearInv = .T.  && Clear previous Invoice Range
  *-- end of lfClearSel.

  *!*************************************************************
  *! Name      : lfCollTime
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : Calcualte spent time in data collection.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Report code section.
  *!*************************************************************
  *! Passed Parameters  : Start collection date,End collection date
  *!*************************************************************
  *! Returns            : Spent time.
  *!*************************************************************
  *! Example   : =lfCollTime()
  *!*************************************************************
  *!
FUNCTION lfCollTime
  PARAMETERS lcStart,lcEnd
  lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
  lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
  lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
  lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
  RETURN (lnEnd - lnStart)
  *-- end of lfCollTime.

  *!*************************************************************
  *! Name      : lfClearRep
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : Function that we call when Close the option grid.
  *!*************************************************************
  *! Called from : [Option Grid] < Close > button.
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfClearRep()
  *!*************************************************************
  *!
FUNCTION lfClearRep

  llClearFn = .T.    && If you run filter you must create cursor again.

  *-- Close temp. opended files, if it used.
  IF USED(lcWorkFile)
    USE IN (lcWorkFile)
  ENDIF

  IF USED(lcShpMnfLine)
    USE IN (lcShpMnfLine)
    ERASE (oAriaApplication.WorkDir+lcShpMnfLine+'*.*')
  ENDIF

  *-- end of lfClearRep.

  *-------------------- Print code -----------------------------
  *!*************************************************************
  *! Name      : lpPrnCode
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : Print procedure.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Report code section.
  *!*************************************************************
  *! Passed Parameters  : ....
  *!*************************************************************
  *! Returns            : ....
  *!*************************************************************
  *! Example   : DO lpPrnCode
  *!*************************************************************
  *!
PROCEDURE lpPrnCode

  XCONS = IIF(lcRpInvTyp='Y','Y','N')

  DIMENSION loOGScroll.laCRParams[7,2]
  loOGScroll.laCRParams[1,1] = 'ReportName'
  loOGScroll.laCRParams[1,2] = 'Shipping Manifest'
  loOGScroll.laCRParams[2,1] = 'lcCompAdd1'
  loOGScroll.laCRParams[2,2] = laCompAdd[1]
  loOGScroll.laCRParams[3,1] = 'lcCompAdd2'
  loOGScroll.laCRParams[3,2] = laCompAdd[2]
  loOGScroll.laCRParams[4,1] = 'lcCompAdd3'
  loOGScroll.laCRParams[4,2] = laCompAdd[3]
  loOGScroll.laCRParams[5,1] = 'lcCompAdd4'
  loOGScroll.laCRParams[5,2] = laCompAdd[4]
  loOGScroll.laCRParams[6,1] = 'lcCompAdd5'
  loOGScroll.laCRParams[6,2] = laCompAdd[5]
  loOGScroll.laCRParams[7,1] = 'lcStyTitle'
  loOGScroll.laCRParams[7,2] = lcStyTitle

  DIMENSION loOgScroll.lacrTABLES[2]  && array For Temp Table & pathes
  loOgScroll.lacrTABLES[1] = oAriaApplication.WorkDir+lcShpMnfHdr+'.DBF'
  loOgScroll.lacrTABLES[2] = oAriaApplication.WorkDir+lcShpMnfLine+'.DBF'

  loogScroll.cCROrientation = 'P'
  = gfDispRe()
  DO ENDREPORT
  *-- end of lpPrnCode.

  *!**************************************************************************
  *! Name      : lfCurrency
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : Validation for currency pop up
  *!**************************************************************************
FUNCTION lfCurrency

  loogscroll.PARENT.ogtoolbar.cntPrint.cmdPrint.ENABLED    = .F.
  loogscroll.PARENT.ogtoolbar.cntPrint.cmdPreview.ENABLED  = .F.
  loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .F.
  loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled   = .F.

  IF USED(laOGFxFlt[1,6])
    PRIVATE lcSavAlias
    lcSavAlias = ALIAS()
    SELECT (laOGFxFlt[1,6])
    ZAP
    SELECT (lcSavAlias)
  ENDIF
  lfSRInv('R')
  *-- End of lfCurrency.

  *!**************************************************************************
  *! Name      : lfCollectDetail
  *! Developer : Walid Hamed  (WLD)
  *! Date      : 03/15/2007
  *! Purpose   : Collect Invoice lines
  *!**************************************************************************
FUNCTION lfCollectDetail

  SELECT (lcWorkFile)
  GOTO TOP
  XSTR   = SPACE(8)
  lcOrder= SPACE(6)

  IF XCONS = 'Y'
    SCAN
      XSTR = STORE
      SELECT CONSINVL
      =GFSEEK(XINV)
      SCAN REST WHILE invoice = XINV
        IF ((XSTR <> STORE) .AND. (.NOT. EMPTY(XSTR))) OR ((lcOrder <> ORDER) AND !EMPTY(lcOrder))
          =GFSEEK(XINV+XSTR+lcOrder,'CONSINVH')
          REPLACE Discount WITH CONSINVH.Discount IN  (lcWorkFile)
        ENDIF
        XSTR    = STORE
        lcOrder = ORDER
        =gfseek('S'+ACCOUNT+STORE,'CUSTOMER')
        lcCity = PADR(ALLTRIM(CUSTOMER.cAddress3),19)
        lcstate= ALLTRIM(CUSTOMER.cAddress4)

        lcSTYDESC = IIF(GFSEEK(CONSINVL.STYLE,'STYLE'),STYLE.DESC,' ')
        INSERT INTO (lcShpMnfLine) (Invoice,Account,STORE,cStCity,cStState,STYLE,StyDesc,TotQty,Price,UAmnt) VALUES ;
          (CONSINVL.INVOICE,CONSINVL.account,CONSINVL.STORE,lcCity,lcstate,CONSINVL.STYLE,lcSTYDESC,CONSINVL.totqty,CONSINVL.price,CONSINVL.totqty*CONSINVL.price)

      ENDSCAN
    ENDSCAN

  ELSE         && NOT CONSOLIDATED INVOICES
    SELECT (lcWorkFile)
    SCAN
      XINV   =INVOICE
      SELECT INVLINE
      =GFSEEK(XINV)
      SCAN REST WHILE invoice = XINV
        lcSTYDESC = IIF(GFSEEK(INVLINE.STYLE,'STYLE'),STYLE.DESC,' ')
        INSERT INTO (lcShpMnfLine) (Invoice,Account,STORE,STYLE,StyDesc,TotQty,Price,UAmnt) VALUES ;
          (Invline.Invoice,invline.account,invline.STORE,invline.STYLE,lcSTYDESC,invline.totqty,invline.price,invline.totqty*invline.price)
      ENDSCAN
    ENDSCAN
  ENDIF
  *-- End of lfCollectDetail.
