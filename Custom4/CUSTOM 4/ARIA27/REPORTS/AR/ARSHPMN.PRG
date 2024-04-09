*:***************************************************************************
*: Program file  : ARSHPMN
*: Program desc. : Shipping Manifest Report
*: For Report    : ....
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Mohamed Atia Badran (MAB)
*: Date          : 12/26/1998
*:***************************************************************************
*: Calls : 
*:    Procedures : lpPrnCode,lpPrnLoop,CusBrowM
*:    Functions  : gfModalGen,gfPhoneTem,gfOpenFile,gfBrows,lfwRepWhen,lfAdrShift,lfWorkFile,
*:               : lfvAcct,lfvCons,lfwOldVal,lfEvalVars,lfSRInv,lfvInv,lfClearSel,lfCollTime,
*:               : lfClearRep.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARSHPMN
*:***************************************************************************
*: This Report Program is due to E301105 ...
*:E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.
*:C102522,1 SSE 01/23/2002 Custom modifications in Shipping Manifest Report.

*----------------------- Report Code Begin -----------------------------
lcStTime = TIME()                          && Time in which we start collect data.
XINV     = IIF(lcRpInvTyp='Y',lcRpInv,'')  && Variable hold Consolidated Invoice.

*C102522,1 Check if Customer is Revue. [Begin]
PRIVATE llRevue
llRevue = gfDoTriger('ARSHPMN',PADR('REVUE',10))
*C102522,1 Check if Customer is Revue. [End]

*-- if user change report expression.
*E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes. [Begin
*IF llClearFn OR llChInv OR (TYPE('lcLastExpr') = 'N') OR !(lcRpExp == lcLastExpr) OR ;
*  !(lcOldAcct == lcRpAcct) OR !(lcOldType == lcRpInvTyp) OR !(lcOldCurr == lcRpCurr)
IF llClearFn OR llOGFltCh
*E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes. [End

  STORE .F. TO llClearFn,llChInv  && Avoid Recollecting data again.
  lcLastExpr = lcRpExp   && Save current report expression, To later know that user change critria.
  lcOldAcct  = lcRpAcct
  lcOldType  = lcRpInvTyp
  lcOldCurr  = lcRpCurr
  
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
  SET ORDER TO TAG INVHDR
  SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER

  m.Flag = 'Y'
  SCAN FOR &lcRpExp

    SCATTER MEMVAR FIELDS ;
        Invoice , Account, Store, CustPo, Note1, Note2, ;
        Weight, Cartons, Ship, ShipAmt, Discount
    m.ConSol = IIF(EMPTY(ConSol),'N',ConSol)
    
    *-- Get customer Addresses [Begin
    laCustAdd[1] = Customer.BtName
    laCustAdd[2] = Customer.cAddress1
    laCustAdd[3] = Customer.cAddress2
    laCustAdd[4] = IIF(EMPTY(Customer.cAddress3),'', PADR(ALLTRIM(Customer.cAddress3),12))          +;
                   IIF(EMPTY(Customer.cAddress4),'', ' - ' + PADR(ALLTRIM(Customer.cAddress4),6)) +;
                   IIF(EMPTY(Customer.cAddress5),'', ' - ' + PADR(ALLTRIM(Customer.cAddress5),12))
    laCustAdd[5] = IIF(EMPTY(Customer.Phone1),'',TRANSFORM(Customer.Phone1 , lcPhonPict))

    = lfAdrShift('laCustAdd')    && Shift Company address if there is empty line.

    FOR lnI = 1 TO 5
      lcI = STR(lnI,1)
      m.HLine&lcI = IIF(lnI > ALEN(laCustAdd,1),'',laCustAdd[lnI])
    ENDFOR
    *-- Get customer Addresses [End..
    
    INSERT INTO (lcWorkFile) FROM MEMVAR
  
  ENDSCAN

  SELECT INVHDR
  SET RELATION TO
  
ENDIF

IF RECCOUNT(lcWorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

SELECT (lcWorkFile)
GO TOP

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

*-- Do Report Program [Start
DO lpPrnCode
*-- Do Report Program [End..

*-- end of report code.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 12/26/1998
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

*-- if it's first time to run the report.
IF TYPE('lcLastExpr') = 'N'

  R_WIDTH = 'N'

  DECLARE laCompAdd[5],laCustAdd[5]
  laCompAdd = ''                   && Array to hold the Company  address
  laCustAdd = ''                   && Array to hold the Customer address

  *-- Get company Address [begin].
  *E301077,72 Open Company file [Begin]
  IF !USED('SYCCOMP')
    USE (gcSysHome+'SYCCOMP') ORDER TAG Ccomp_id IN 0 SHARED
  ENDIF
  *E301077,72 Open Company file [End  ]

  SELECT SYCCOMP
  SEEK gcAct_Comp
  lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.

  laCompAdd[1] = cCom_Name
  laCompAdd[2] = cAddress1
  laCompAdd[3] = cAddress2
  laCompAdd[4] = IIF(EMPTY(cAddress3),'', PADR(ALLTRIM(cAddress3),12))          +;
                 IIF(EMPTY(cAddress4),'', ' - ' + PADR(ALLTRIM(cAddress4),6)) +;
                 IIF(EMPTY(cAddress5),'', ' - ' + PADR(ALLTRIM(cAddress5),12))
  laCompAdd[5] = IIF(EMPTY(cCom_Phon),'',TRANSFORM(cCom_Phon , lcPhonPict))
  
  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  *-- Get company Address [end].

  IF llMultCurr AND EMPTY(lcRpCurr)
    lcRpCurr = gcBaseCurr
    = lfOGShowGet('lcRpCurr')  && Show get Object .
  ENDIF

  =lfWorkFile()
  
ENDIF  && END IF you first time enter when function.

*-- Preview and Run buttons are enabled if we have data to preview.
IF EMPTY(lcRpAcct) OR (lcRpInvTyp='Y' AND EMPTY(lcRpInv)) OR ;
   (EMPTY(lcRpInvTyp) AND !(USED(laOGFxFlt[1,6]) AND RECCOUNT(laOGFxFlt[1,6]) > 0)))
  SHOW GET pbOGPrevie DISABLE
  SHOW GET pbRun      DISABLE   
ENDIF   

*-- if empty of in range cursor name, Get it.
lnInvFlt   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVOICE'),1)
IF lnInvFlt > 0 AND EMPTY(laOGFxFlt[lnInvFlt,6])
  lnInCurFlt = ASCAN(laFxFltCur,lnInvFlt)
  IF lnInCurFlt > 0
    lnInCurFlt = ASUBSCRIPT(laFxFltCur,lnInCurFlt,1)
    laOGFxFlt[lnInvFlt,6] = laFxFltCur[lnInCurFlt,2]  && Name of invoice cursor (InRange func.).
  ENDIF  
ENDIF

*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 12/26/1998
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/26/1998
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
CREATE CURSOR (lcWorkFile) ;
  (Invoice C(6), Account C(5), Store C(8), Consol C(1), CustPo C(15), Note1 C(30), Note2 C(30),;
   Weight N(6,0), Cartons N(5,0), Ship N(7,0), ShipAmt N(13,2), Discount N(13,2),;
   HLine1 C(30), HLine2 C(30), HLine3 C(30), HLine4 C(30), HLine5 C(30), Flag C(1)  )

SELECT (lcWorkFile)
INDEX ON INVOICE TAG (lcWorkFile)
*-- end of lfWorkFile.

*!*************************************************************
*! Name      : lfvAcct
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/26/1998
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
lcObjVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field

IF !(lcObjVal == laOldVal)
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER
  
  lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field
  *IF The user want to Browse or if the Account he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
    llObjRet = CusBrowM(@lcObjVal , '' , 'M')
    lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
    &lcObjName = lcObjVal
  ENDIF    && End of IF
  
  IF !(lcObjVal == laOldVal)
    =lfClearSel()
    
    *C102522,1 Clear the invoice no in case of consolidated invoice. [Begin]
    *CLEAR READ   && Clear current read cycle, to activate new one.
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
    CLEAR READ   && Clear current read cycle, to activate new one.
    *C102522,1 Clear the invoice no in case of consolidated invoice. [End]

  ENDIF  
  
  SET ORDER TO &lcCustOrd
  SELECT(lnAlsNo)

ENDIF

*C102522,1 Blank the Invoice temp name. [Begin]
IF !EMPTY(lcObjVal) AND lcRpInvTyp = "Y"
  laOGFxFlt[1,6] = ''
ENDIF
*C102522,1 Blank the Invoice temp name. [End]
*-- end of lfvAcct.

*!*************************************************************
*! Name      : lfvCons
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/26/1998
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

*C102522,1 Blank the Invoice temp name. [Begin]
IF lcRpInvTyp = "Y"
  laOGFxFlt[1,6] = ''
ENDIF
*C102522,1 Blank the Invoice temp name. [End]

CLEAR READ  && Clear current read cycle, to activate new one.
*-- end of lfvCons.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/26/1998
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
laOldVal = EVALUATE(SYS(18))
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfEvalVars
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/26/1998
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
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  
  IF llOpenCurr
    USE IN SYCCURR
  ENDIF
  
ENDIF
*-- if multi currency evaluate currency arrays [Begin]
*-- end of lfEvalVars.

*!*************************************************************
*! Name      : lfSRInv
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/26/1998
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

SET ORDER TO CUSTOMER IN CUSTOMER
SELECT INVHDR
SET ORDER TO INVHDRA

SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER
lcInvFlt = FILTER()

DO CASE

  CASE lcParm = 'S'  && Set code
    llChInv = .T.

    *C102522,1 Change currency code to 3 characters. [Begin]
    *SET FILTER TO ACCOUNT + INVOICE = IIF(EMPTY(lcRpAcct),'',lcRpAcct)    AND ;
    *             STATUS <> 'V'                                            AND ;
    *             IIF(lcRpInvTyp='Y', CONSOL = lcRpInvTyp , CONSOL $ ' N') AND ;
    *             IIF(llMultCurr,CCURRCODE = PADR(lcRpCurr,5),.T.)
    SET FILTER TO ACCOUNT + INVOICE = IIF(EMPTY(lcRpAcct),'',lcRpAcct)    AND ;
                 STATUS <> 'V'                                            AND ;
                 IIF(lcRpInvTyp='Y', CONSOL = lcRpInvTyp , CONSOL $ ' N') AND ;
                 IIF(llMultCurr,CCURRCODE = PADR(lcRpCurr,3),.T.)
    *C102522,1 Change currency code to 3 characters. [End]

    SET ORDER TO TAG INVHDR
    GO TOP

  CASE lcParm = 'R'  && Reset code
    llClearInv = .F.
    SET FILTER TO &lcInvFlt
    SET RELATION TO
    SELECT (lnAlias)

    *-- if user select invoices.
    IF RECCOUNT(laOGFxFlt[1,6]) > 0
      
      *C102522,1 Disable if empty of first record in temp invoice file. [Begin]      
      *SHOW GET pbOGPrevie ENABLE
      *SHOW GET pbRun      ENABLE
      IF RECCOUNT(laOGFxFlt[1,6]) = 1
        GO TOP IN (laOGFxFlt[1,6])
        IF EMPTY(EVALUATE(laOGFxFlt[1,6]+'.Invoice'))
          SHOW GET pbOGPrevie DISABLE
          SHOW GET pbRun      DISABLE
        ELSE
          SHOW GET pbOGPrevie ENABLE
          SHOW GET pbRun      ENABLE
        ENDIF  
      ELSE
        SHOW GET pbOGPrevie ENABLE
        SHOW GET pbRun      ENABLE        
      ENDIF     
      *C102522,1 Disable if empty of first record in temp invoice file. [Begin]
          
    ELSE  && else user does not select invoices.
    
      SHOW GET pbOGPrevie DISABLE
      SHOW GET pbRun      DISABLE
    ENDIF

ENDCASE
*-- end of lfSRInv.

*!*************************************************************
*! Name      : lfvInv
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/26/1998
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

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

lcInvHdTag = ORDER('INVHDR')
lcCstmrTag = ORDER('CUSTOMER')
SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER

*IF The user want to Browse or if the Account he entered is not in the file

*C102522,1 Correct the seek expression. [Begin]
*IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR'))
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR')) OR ;
  (!EMPTY(lcObjVal) .AND. SEEK(lcObjVal , 'INVHDR') AND InvHdr.Consol <> "Y")
*C102522,1 Correct the seek expression. [End]
  
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

  *C102522,1 Change currency code to 3 characters. [Begin]  
  *lcBrowCond = [FOR ACCOUNT = lcRpAcct AND STATUS <> "V" AND CONSOL="Y" AND ] +;
  *             [IIF(llMultCurr,CCURRCODE = PADR(lcRpCurr,5),.T.)]
  lcBrowCond = [FOR ACCOUNT = lcRpAcct AND STATUS <> "V" AND CONSOL="Y" AND ] +;
               [IIF(llMultCurr,CCURRCODE = PADR(lcRpCurr,3),.T.)]
  *C102522,1 Change currency code to 3 characters. [End]
  
  SELECT INVHDR
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  DECLARE laRetVal[1]
  
  
  IF gfBrows(lcBrowCond , 'Invoice' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE    && Else
    &lcObjName = laOldVal
  ENDIF    && End of IF
  
  SET RELATION OFF INTO CUSTOMER
ENDIF    && End of IF

*IF The INVHDR file did not have an active index
IF EMPTY(lcInvHdTag)
  SET ORDER TO IN INVHDR
ELSE    && Else
  SET ORDER TO TAG (lcInvHdTag) IN INVHDR
ENDIF    && End of IF

*IF The CUSTOMER file did not have an active index
IF EMPTY(lcCstmrTag)
  SET ORDER TO IN CUSTOMER
ELSE    && Else
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF    && End of IF

*-- if no invoices selected.
IF EMPTY(&lcObjName)
  SHOW GET pbOGPrevie DISABLE
  SHOW GET pbRun      DISABLE

ELSE  && else user select invoice.
  SHOW GET pbOGPrevie ENABLE
  SHOW GET pbRun      ENABLE
ENDIF
*-- end of lfvInv.

*!*************************************************************
*! Name      : lfClearSel
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/26/1998
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/26/1998
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/26/1998
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

*E301077,72 Close Company file [Begin]
IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF
*E301077,72 Close Company file [End  ]

*-- Close lcLinkChar file if it was oppened.
IF (ASCAN(laSelFile,'LCLINKCHAR') = 0) AND USED('LCLINKCHAR')
  USE IN LCLINKCHAR
ENDIF

*-- Close SYCINT file if it was oppened.
IF (ASCAN(laSelFile,'SYCINT') = 0) AND USED('SYCINT')
  USE IN SYCINT
ENDIF

*-- end of lfClearRep.

*-------------------- Print code -----------------------------
*!*************************************************************
*! Name      : lpPrnCode
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/26/1998
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

R_TITLE = 'Manifest'

SET DEVICE TO PRINT
DO lpPrnLoop
DO ENDREPORT
SET DEVICE TO SCREEN
*-- end of lpPrnCode. 

*!*************************************************************
*! Name      : lpPrnLoop
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/26/1998
*! Purpose   : Print Loop.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : lpPrnCode
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : DO lpPrnLoop
*!*************************************************************
*!
PROCEDURE lpPrnLoop
DO WHILE .T.
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*STYLE        COLOR  DESCRIPTION             UNITS     PRICE       AMOUNT
*123456789012 XXXXXX XXXXXXXXXXXXXXXXXXXX  9999999  99999.99  99999999.99

  SELECT (lcWorkFile)
  GOTO TOP
  IF XCONS = 'Y'
    XCUSTPO=CUSTPO
    XNOTE1 =NOTE1
    XNOTE2 =NOTE2
    XWGHT  =WEIGHT
    XCART  =CARTONS
    XSTR   = SPACE(8)
    lcOrder= SPACE(6)
    TUNT   = 0
    TAMT   = 0
    GUNT   = 0
    GAMT   = 0
    ROW    = 99
    PAGENO = 0

    SELECT CONSINVL
    SEEK XINV

    DO WHILE .T.

      IF ((XSTR <> STORE) .AND. (.NOT. EMPTY(XSTR))) OR ((lcOrder <> Order) AND !EMPTY(lcOrder))
        ROW=ROW+1
        
        *-- MAB 12/30/98 Print Consolidate invoice Merchandise discount if found [Begin]
        *@ROW,10 SAY '***** SUB TOTAL  ====>'
        =SEEK(XINV+XSTR+lcOrder,'CONSINVH')
        
        *C102522,1 Suppress Sub Total for Customer Revue. [Begin]
        *@ROW,01 SAY '  Sub Total ==> ' + IIF(CONSINVH.DISCOUNT<>0,', Merch. Disc = ' + TRANSFORM(CONSINVH.DISCOUNT,'99999.99'),'')
        *@ ROW,43 SAY TUNT PICTURE '9999999'
        *@ ROW,61 SAY TAMT PICTURE '999999999.99'
        *ROW=ROW+1
        IF !llRevue
          @ ROW,01 SAY '  Sub Total ==> ' + IIF(CONSINVH.DISCOUNT<>0,', Merch. Disc = ' + TRANSFORM(CONSINVH.DISCOUNT,'99999.99'),'')
          @ ROW,43 SAY TUNT PICTURE '9999999'
          @ ROW,61 SAY TAMT PICTURE '999999999.99'
          ROW=ROW+1          
        ENDIF  
        *C102522,1 Suppress Sub Total for Customer Revue. [End]
        
        *-- MAB 12/30/98 Print Consolidate invoice Merchandise discount if found [End..]
        
        @ ROW,00 SAY REPLICATE('=',80)
        GUNT=GUNT+TUNT
        GAMT=GAMT+TAMT
        TUNT=0
        TAMT=0
        ROW=ROW+2
        IF EOF() .OR. (INVOICE<>XINV)
          EXIT
        ENDIF
      ENDIF

      IF ROW >= 52
        PAGENO=PAGENO+1
        @ 01,31 SAY 'Shipping Manifest'
        @ 02,31 SAY '-----------------'

        *C102522,1 Correct spelling. [Begin]
        *@ 02,65 SAY 'PAGENO : '
        @ 02,65 SAY 'PAGE NO : '
        *C102522,1 Correct spelling. [End]

        @ 02,75 SAY PAGENO PICTURE '99'
        @ 04,01 SAY 'FROM :'
        @ 04,45 SAY 'TO :'
        @ 05,01 SAY laCompAdd[1]
        @ 05,45 SAY &lcWorkFile..HLINE1
        @ 06,01 SAY laCompAdd[2]
        @ 06,45 SAY &lcWorkFile..HLINE2
        @ 07,01 SAY laCompAdd[3]
        @ 07,45 SAY &lcWorkFile..HLINE3
        @ 08,01 SAY laCompAdd[4]
        @ 08,45 SAY &lcWorkFile..HLINE4

        *B500772,1 HISH 06/27/95. ( Begin ) Display phone with new format.

        *BAD @ 09,01 SAY HDRL5  PICTURE "@R "+gcPhnFrmt SIZE 1,16
        @ 09,01 SAY laCompAdd[5]
        *B500772,1 HISH 06/27/95.  ( End )

        *B500772,1 HISH 06/27/95. ( Begin ) Display phone with new format.
        *BAD @ 09,45 SAY HLINE5  PICTURE "@R "+gcPhnFrmt SIZE 1,16
        @ 09,45 SAY &lcWorkFile..HLINE5
        *B500772,1 HISH 06/27/95.  ( End )

        @ 11,01 SAY 'Invoice #: '
        @ 11,12 SAY XINV
        @ 11,20 SAY 'Cust. PO: '
        @ 11,31 SAY XCUSTPO
        @ 11,45 SAY 'Weight: '
        @ 11,53 SAY XWGHT PICTURE '999999'
        @ 11,63 SAY 'Cartons: '
        @ 11,72 SAY XCART PICTURE '9999'
        @ 12,01 SAY 'Notes    :'
        @ 12,12 SAY XNOTE1+XNOTE2
        ROW = 13
        NEWPAGE=.T.
      ENDIF

      IF (XSTR <> STORE) .OR. (lcOrder <> Order) .OR. (NEWPAGE)
        @ ROW,01 SAY 'Store    : '
        @ ROW,12 SAY STORE

        XKEY='S'+ACCOUNT+STORE
        SELECT CUSTOMER
        SEEK XKEY
        IF FOUND()

          *City
          @ ROW,25 SAY PADR(ALLTRIM(cAddress3),19)

          *State
          @ ROW,45 SAY cAddress4

        ENDIF
        SELECT CONSINVL

        ROW=ROW+1
        @ ROW,00 SAY REPLICATE('-',80)
        ROW=ROW+1
        
        *C102522,1 Remove the Price and Amount for customer Revue. [Begin]
        *@ ROW,00 SAY ' ' + lcStyTitle+ ' DESCRIPTION             UNITS     PRICE       AMOUNT'
        IF llRevue
          @ ROW,00 SAY ' ' + lcStyTitle+ ' DESCRIPTION                                    UNITS'
        ELSE
          @ ROW,00 SAY ' ' + lcStyTitle+ ' DESCRIPTION             UNITS     PRICE       AMOUNT'
        ENDIF  
        *C102522,1 Remove the Price and Amount for customer Revue. [End]

        ROW=ROW+1
        @ ROW,00 SAY REPLICATE('-',80)
        ROW=ROW+1
        XSTR    = STORE
        lcOrder = ORDER
        NEWPAGE=.F.
      ENDIF

       XKEY=STYLE
       SELECT STYLE
       SEEK XKEY
       IF FOUND()
         STYDESC=DESC
       ELSE
         STYDESC=' '
       ENDIF

       SELECT CONSINVL

       @ ROW,01 SAY STYLE
       *@ ROW,14 SAY COLOR    &&TAK 06/15/94
       @ ROW,21 SAY STYDESC
       
      *C102522,1 Remove the Price and Amount for customer Revue. [Begin]
      *@ ROW,43 SAY TOTQTY PICTURE '9999999'
      *@ ROW,52 SAY PRICE PICTURE '99999.99'
      *@ ROW,62 SAY (TOTQTY*PRICE) PICTURE '99999999.99'
      IF llRevue
         @ ROW,66 SAY TOTQTY PICTURE '9999999'      
      ELSE
        @ ROW,43 SAY TOTQTY PICTURE '9999999'      
        @ ROW,52 SAY PRICE PICTURE '99999.99'
        @ ROW,62 SAY (TOTQTY*PRICE) PICTURE '99999999.99'
      ENDIF  
      *C102522,1 Remove the Price and Amount for customer Revue. [End]
       
      ROW=ROW+1

       TUNT=TUNT+TOTQTY
       TAMT=TAMT+(TOTQTY*PRICE)
       SKIP
    ENDDO
    ROW=ROW+1

    *-- MAB 12/30/98 Print Invoice Merchandise discount if found [Begin]
    =SEEK(XINV,'INVHDR')
    
    *C102522,1 Suppress Grand Total for Customer Revue. [Begin]
    *@ROW,01 SAY ' Grand Total => ' + IIF(INVHDR.DISCOUNT<>0,', Merch. Disc = ' + TRANSFORM(INVHDR.DISCOUNT,'99999.99'),'')
    *@ ROW,43 SAY GUNT PICTURE '9999999'
    *@ ROW,61 SAY GAMT PICTURE '999999999.99'
    IF !llRevue
      @ROW,01 SAY ' Grand Total => ' + IIF(INVHDR.DISCOUNT<>0,', Merch. Disc = ' + TRANSFORM(INVHDR.DISCOUNT,'99999.99'),'')
      *@ ROW,10 SAY '***** GRAND TOTAL  ====>'
      *-- MAB 12/30/98 Print Invoice Merchandise discount if found [End..]
      @ ROW,43 SAY GUNT PICTURE '9999999'
      @ ROW,61 SAY GAMT PICTURE '999999999.99'
    ENDIF  
    *C102522,1 Suppress Grand Total for Customer Revue. [End]
    
  ELSE         && NOT CONSOLIDATED INVOICES
   SELECT (lcWorkFile)
   GOTO TOP
   TUNT   = 0
   TAMT   = 0
   GUNT   = 0
   GAMT   = 0
   ROW    = 99
   PAGENO = 0

   DO WHILE (.NOT. EOF()) .AND. (INKEY() <> 32)
    XINV   =INVOICE
    XCUSTPO=CUSTPO
    XNOTE1 =NOTE1
    XNOTE2 =NOTE2
    XWGHT  =WEIGHT
    XCART  =CARTONS
    XSTR   =STORE
    NEWINV =.T.
    SELECT INVLINE
    IF SEEK(XINV)
     DO WHILE INKEY() <> 32
      IF (XINV <> INVOICE) .OR. (EOF())
        llPrint = .T.
        ROW=ROW+1

        *-- MAB 12/30/98 Print invoice Merchandise discount if found [Begin]
        *@ROW,10 SAY '***** SUB TOTAL  ====>'
        =SEEK(XINV,'INVHDR')
        
        *C102522,1 Suppress Sub Total for Customer Revue. [Begin]
        *@ROW,01 SAY '  Sub Total ==> ' + IIF(INVHDR.DISCOUNT<>0,', Merch. Disc = ' + TRANSFORM(INVHDR.DISCOUNT,'99999.99'),'')
        *@ ROW,43 SAY TUNT PICTURE '9999999'
        *@ ROW,61 SAY TAMT PICTURE '999999999.99'
        *ROW=ROW+1
        IF !llRevue
          @ROW,01 SAY '  Sub Total ==> ' + IIF(INVHDR.DISCOUNT<>0,', Merch. Disc = ' + TRANSFORM(INVHDR.DISCOUNT,'99999.99'),'')
          @ ROW,43 SAY TUNT PICTURE '9999999'
          @ ROW,61 SAY TAMT PICTURE '999999999.99'
          ROW=ROW+1
        ENDIF  
        *C102522,1 Suppress Sub Total for Customer Revue. [End]

        *-- MAB 12/30/98 Print invoice Merchandise discount if found [End..]

        @ ROW,00 SAY REPLICATE('=',80)
        GUNT=GUNT+TUNT
        GAMT=GAMT+TAMT
        TUNT=0
        TAMT=0
        ROW=ROW+2
        EXIT
      ENDIF
       IF ROW >= 52
        PAGENO=PAGENO+1
        @ 01,31 SAY 'Shipping Manifest'
        @ 02,31 SAY '-----------------'

        *C102522,1 Correct spelling. [Begin]
        *@ 02,65 SAY 'PAGENO : '
        @ 02,65 SAY 'PAGE NO : '
        *C102522,1 Correct spelling. [End]

        @ 02,75 SAY PAGENO PICTURE '99'
        @ 04,01 SAY 'FROM :'
        @ 04,45 SAY 'TO :'
        @ 05,01 SAY laCompAdd[1]
        @ 05,45 SAY &lcWorkFile..HLINE1
        @ 06,01 SAY laCompAdd[2]
        @ 06,45 SAY &lcWorkFile..HLINE2
        @ 07,01 SAY laCompAdd[3]
        @ 07,45 SAY &lcWorkFile..HLINE3
        @ 08,01 SAY laCompAdd[4]
        @ 08,45 SAY &lcWorkFile..HLINE4

        *B500772,1 HISH 06/27/95. ( Begin ) Display phone with new format.
        @ 09,01 SAY laCompAdd[5]
        *@ 09,01 SAY HDRL5
        *B500772,1 HISH 06/27/95.  ( End )

        *B500772,1 HISH 06/27/95. ( Begin ) Display phone with new format.
        @ 09,45 SAY &lcWorkFile..HLINE5
        *@ 09,45 SAY HLINE5
        *B500772,1 HISH 06/27/95.  ( End )

        ROW = 11
        NEWPAGE=.T.
      ENDIF
      IF NEWINV .OR. NEWPAGE
        @ ROW,01 SAY 'Inv#     : '
        @ ROW,12 SAY XINV
        @ ROW,30 SAY 'Store  : '
        @ ROW,39 SAY XSTR
        IF .NOT. EMPTY(XSTR)
          XKEY='S'+ACCOUNT+XSTR
          SELECT CUSTOMER
          SEEK XKEY
          IF FOUND()

            *City
            @ ROW,52 SAY PADR(ALLTRIM(cAddress3),17)

            *State
            @ ROW,70 SAY cAddress4

          ENDIF
          SELECT INVLINE
        ENDIF
        ROW=ROW+1
        @ ROW,01 SAY 'Cust PO# : '
        @ ROW,12 SAY XCUSTPO
        @ ROW,30 SAY 'Weight : '
        @ ROW,39 SAY XWGHT PICTURE '999999'
        @ ROW,52 SAY 'Cartons : '
        @ ROW,62 SAY XCART PICTURE '9999'
        ROW=ROW+1
        @ ROW,01 SAY 'Notes    : '
        @ ROW,12 SAY XNOTE1+' '+XNOTE2
        ROW=ROW+1
        @ ROW,00 SAY REPLICATE('-',80)
        ROW=ROW+1

        *C102522,1 Remove the Price and Amount for customer Revue. [Begin]
        *@ ROW,00 SAY ' ' + lcStyTitle + ' DESCRIPTION             UNITS     PRICE       AMOUNT'
        IF llRevue
          @ ROW,00 SAY ' ' + lcStyTitle + ' DESCRIPTION                                    UNITS'
        ELSE
          @ ROW,00 SAY ' ' + lcStyTitle + ' DESCRIPTION             UNITS     PRICE       AMOUNT'
        ENDIF  
        *C102522,1 Remove the Price and Amount for customer Revue. [End]

        ROW=ROW+1
        @ ROW,00 SAY REPLICATE('-',80)
        ROW=ROW+1
        NEWINV=.F.
        NEWPAGE=.F.
      ENDIF
       XKEY=STYLE
       SELECT STYLE
       SEEK XKEY
       IF FOUND()
         STYDESC=DESC
       ELSE
         STYDESC=' '
       ENDIF
       SELECT INVLINE
       @ ROW,01 SAY STYLE
       *@ ROW,14 SAY COLOR    &&TAK 06/15/94
       @ ROW,21 SAY STYDESC       

      *C102522,1 Remove the Price and Amount for customer Revue. [Begin]
      *@ ROW,52 SAY PRICE PICTURE '99999.99'
      *@ ROW,62 SAY (TOTQTY*PRICE) PICTURE '99999999.99'
      IF llRevue
        @ ROW,66 SAY TOTQTY PICTURE '9999999'
      ELSE
        @ ROW,43 SAY TOTQTY PICTURE '9999999'
        @ ROW,52 SAY PRICE PICTURE '99999.99'
        @ ROW,62 SAY (TOTQTY*PRICE) PICTURE '99999999.99'
      ENDIF  
      *C102522,1 Remove the Price and Amount for customer Revue. [End]

       ROW=ROW+1
       TUNT=TUNT+TOTQTY
       TAMT=TAMT+(TOTQTY*PRICE)
       SKIP
     ENDDO
    ENDIF
    SELECT (lcWorkFile)
    SKIP
    IF EOF() .AND. GUNT <>0      && TMI 01/23/95
      ROW=ROW+1

      *C102522,1 Suppress Grand Total for Customer Revue. [Begin]
      *@ ROW,10 SAY '***** GRAND TOTAL  ====>'
      *@ ROW,43 SAY GUNT PICTURE '9999999'
      *@ ROW,61 SAY GAMT PICTURE '999999999.99'
      IF !llRevue
        @ ROW,10 SAY '***** GRAND TOTAL  ====>'
        @ ROW,43 SAY GUNT PICTURE '9999999'
        @ ROW,61 SAY GAMT PICTURE '999999999.99'
      ENDIF
      *C102522,1 Suppress Grand Total for Customer Revue. [End]
      
      EXIT
    ENDIF
   ENDDO
  ENDIF
  EXIT
ENDDO
*-- end of PrnLoop.

*!**************************************************************************
*! Name      : lfCurrency
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/19/2002
*! Purpose   : Validation for currency pop up
*!**************************************************************************
*C102522,1 
FUNCTION lfCurrency
SHOW GET pbOGPrevie DISABLE
SHOW GET pbRun      DISABLE   
*-- End of lfCurrency.