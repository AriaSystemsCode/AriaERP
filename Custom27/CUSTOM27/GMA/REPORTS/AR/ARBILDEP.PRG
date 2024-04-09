*:***************************************************************************
*: Program file  : ARBILDEP.PRG
*: Program desc. : BILLING DEPARTEMENT REPORT FOR GMA
*: For Report    : (ARBILDED.FRX, ARBILDES.FRX)
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Date          : 03/27/2005
*: Tracking Job Number: C125898
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfsrOrder
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARBILDEP
*:***************************************************************************
*:Modifications :
*B039366,1 TMI 04/19/2006 Fix the bug that the qty in summary is taken only from the first invoice
*                         and the invoice # in summary is cut off ( in frx )
*:***************************************************************************

*--Add the factor option to the lcRpExp.
lcRpExp = STRTRAN(lcRpExp,"INVHDR.","")
lcRpExp = lcRpExp + IIF (!EMPTY(lcRpExp) .AND. lcRpFact <> 'B' ,;
          ' .AND. ' , '') + IIF(lcRpFact = 'F' , '!EMPTY(cFacCode)' ,;
          IIF(lcRpFact = 'N' , 'EMPTY(cFacCode)' , ''))


lnInvPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVDATE'),1)

*--Every time make the relation to use the lcRpExp in the 2 cases.
=lfRelation()

IF lcRpFormat == "S"
  SELECT (lcInvSumry)
  ZAP
  *--Add this Index to use it in case Summary.
  INDEX ON ORDER TAG ORDER of (gcWorkDir + lcInvSumry) ADDITIVE

  =lfCollDtal()
  *--Break the relation to make new one with the summary file.
  SELECT (lcInvData)
  SET RELATION TO  

  *--The new relation to the summary file.
  =lfRelSumry()
  
    *--Account|Date|Ship Via ~ A|D|S
  IF lcRpSubBy == "A"
    INDEX ON ACCOUNT + INVOICE  TAG InvAcc of (gcWorkDir + lcInvSumry) ADDITIVE
  ELSE
    IF lcRpSubBy == "D"
      INDEX ON DTOS(INVDATE) + INVOICE  TAG InvDat of (gcWorkDir + lcInvSumry) ADDITIVE
    ELSE
      INDEX ON SHIPVIA + INVOICE TAG InvShp of (gcWorkDir + lcInvSumry) ADDITIVE
    ENDIF
  ENDIF
ELSE
  SELECT (lcInvData)
  IF lcRpSubBy == "A"
    INDEX ON ACCOUNT + INVOICE TAG InvAcc of (gcWorkDir + lcInvData) ADDITIVE
  ELSE
    IF lcRpSubBy == "D"
      INDEX ON DTOS(INVDATE) + INVOICE TAG InvDat of (gcWorkDir + lcInvData) ADDITIVE
    ELSE
      INDEX ON SHIPVIA + INVOICE TAG InvShp of (gcWorkDir + lcInvData) ADDITIVE
    ENDIF
  ENDIF
ENDIF

PRIVATE ldDatFrom , ldDatTo , lcGrpValue
STORE SPACE(0) TO ldDatFrom , ldDatTo , lcGrpValue
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVDATE'),1)
IF !EMPTY(laOGFxFlt[lnDatePos,6])
  ldDatFrom = LEFT(laOGFxFlt[lnDatePos,6] , ATC( "|" , laOGFxFlt[lnDatePos,6])-1)
  ldDatTo   = SUBSTR(laOGFxFlt[lnDatePos,6] , ATC( "|" , laOGFxFlt[lnDatePos,6]) + 1)
ENDIF

*--Set the fields of the grouping.
IF lcRpSubBy == "A"
  lcGrpValue = [ACCOUNT]
ELSE
  IF lcRpSubBy == "D"
    lcGrpValue = [DTOS(INVDATE)]
  ELSE
    lcGrpValue = [SHIPVIA]
  ENDIF
ENDIF

SELECT IIF(lcRpFormat == "D" , (lcInvData) , (lcInvSumry))
IF lcRpFormat == "D"
  lcRpExp = IIF (!EMPTY(lcRpExp),lcRpExp + ' AND TOTALCHG > 0', 'TOTALCHG > 0')
  SET FILTER TO &lcRpExp 
ELSE
  SET FILTER TO TOTALCHG > 0
ENDIF
*--To print the grand total line.
REPLACE ALL llPrnGrand WITH .F.
GOTO BOTTOM
REPLACE llPrnGrand WITH .T.
LOCATE

IF EMPTY(laOGFxFlt[lnInvPos,6])
  DO gfDispRe WITH EVALUATE('lcRpForm'), 'FOR ' + lcRpExp
ELSE
  IF lcRpFormat == "D"
    DO gfDispRe WITH EVALUATE('lcRpForm'), 'FOR ' + lcRpExp  
  ELSE
    DO gfDispRe WITH EVALUATE('lcRpForm')
  ENDIF
ENDIF

SELECT IIF(lcRpFormat == "D" , (lcInvData) , (lcInvSumry))
SET RELATION TO
SET FILTER TO
WAIT CLEAR

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Creat the Tmp. file.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp

SELECT INVHDR
=AFIELDS(laTmpStru)
=lfAddField("laTmpStru", "llPrnGrand" , "L" ,1 ,0)

CREATE TABLE (gcWorkDir + lcInvSumry) FROM ARRAY laTmpStru
CREATE TABLE (gcWorkDir + lcInvData) FROM ARRAY laTmpStru

*--Add this Index to use it in case Summary.
INDEX ON ORDER TAG ORDER of (gcWorkDir + lcInvData) ADDITIVE
  
*--Account|Date|Ship Via ~ A|D|S
IF lcRpSubBy == "A"
  INDEX ON ACCOUNT + INVOICE TAG InvAcc of (gcWorkDir + lcInvData) ADDITIVE
ELSE
  IF lcRpSubBy == "D"
    INDEX ON DTOS(INVDATE) + INVOICE TAG InvDat of (gcWorkDir + lcInvData) ADDITIVE
  ELSE
    INDEX ON SHIPVIA + INVOICE TAG InvShp of (gcWorkDir + lcInvData) ADDITIVE
  ENDIF
ENDIF

*B039366,1  TMI [Start] always fill the lcInvData temp file
*IF lcRpFormat == "D"
*B039366,1  TMI [End  ] 

  SELECT (lcInvData)
  APPEND FROM (gcDataDir + "INVHDR" + '.DBF')
  
*B039366,1  TMI [Start] comment out the following lines
*ELSE
*  SELECT (lcInvSumry)
*ENDIF
WAIT CLEAR
*B039366,1  TMI [End  ] 

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfRelation
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Function to prepare the relations of the program.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRelation()
*!*************************************************************
FUNCTION lfRelation

SELECT (lcInvData)
SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
SET RELATION TO SHIPVIA INTO lcHldShVia ADDITIVE
SET RELATION TO 'O' + ORDER INTO ORDHDR ADDITIVE


*-- End of lfRelation.
*!*************************************************************
*! Name      : lfsrOrder
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Rise change order flag, in range browse screen.
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
*! Note      : S symbol is [S,Set]
*!*************************************************************
*! Example   : =lfsrOrder()
*!*************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm

SELECT ORDHDR
SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
IF lcParm = "S"
  LOCATE
ENDIF  

*--End of lfsrOrder.
*!*************************************************************
*! Name      : lfInvSet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Set function for the invoice number option in case
*!             of In Range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInvSet()
*!*************************************************************
FUNCTION lfInvSet
PARAMETERS lcParm

IF lcParm = 'S'
  lcInvHdTag = ORDER('INVHDR')
  lcCstmrTag = ORDER('CUSTOMER')
  SET ORDER TO TAG INVHDR IN INVHDR
  SET ORDER TO TAG CUSTOMER IN CUSTOMER
ELSE
  *--IF The INVHDR file did not have an active index
  IF EMPTY(lcInvHdTag)
    SET ORDER TO 0 IN INVHDR
  ELSE
    SET ORDER TO TAG (lcInvHdTag) IN INVHDR
  ENDIF
  
  *--IF The CUSTOMER file did not have an active index
  IF EMPTY(lcCstmrTag)
    SET ORDER TO 0 IN CUSTOMER
  ELSE
    SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
  ENDIF
ENDIF

*--End of lfInvSet.
*!*************************************************************
*! Name      : lfvInvNo
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Validation function for the Invoice number field
*!*************************************************************
*! Called from : Invoice number field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvInvNo()
*!*************************************************************
FUNCTION lfvInvNo

PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag , lcCstmrTag

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

lcInvHdTag = ORDER('INVHDR')
lcCstmrTag = ORDER('CUSTOMER')
SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER

*--IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR'))
  
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
  
  SELECT INVHDR
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  DECLARE laRetVal[1]
  
  IF gfBrows('' , 'Invoice' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE
    &lcObjName = laOldVal
  ENDIF
  SET RELATION OFF INTO CUSTOMER
ENDIF

*--IF The INVHDR file did not have an active index
IF EMPTY(lcInvHdTag)
  SET ORDER TO 0 IN INVHDR
ELSE
  SET ORDER TO TAG (lcInvHdTag) IN INVHDR
ENDIF

*--IF The CUSTOMER file did not have an active index
IF EMPTY(lcCstmrTag)
  SET ORDER TO 0 IN CUSTOMER
ELSE
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF

*--End of lfvInvNo.
*!*************************************************************
*! Name      : lfSetAcct
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Customer account in range browse screen.
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSetAcct()
*!*************************************************************
*! Note      : S symbol is [S,Set] ,R symbol is [R ,Reset]
*!*************************************************************
FUNCTION lfSetAcct
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    SET ORDER TO CUSTOMER IN CUSTOMER
    GO TOP IN CUSTOMER
ENDCASE

*--End of lfSetAcct.
*!*************************************************************
*! Name      : lfvDate
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Validate Date Range.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDate()
*!*************************************************************
FUNCTION lfvDate

*-- date range can not be empty
IF EMPTY(lcOGValueF) OR EMPTY(lcOGValueTo)
  WAIT WINDOW 'Date Range is Mandatory ! Please Retry'
  RETURN .F.
ENDIF

*--End of lfvDate.
*!*************************************************************
*! Name      : lfwhengrid
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : valid function when run grid.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns    :  None
*!*************************************************************
*! Example    :  =lfwhengrid()
*!*************************************************************
FUNCTION lfwhengrid
PRIVATE lcalias

lcalias = SELECT(0)

*--Fill date field in OG.
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVDATE'),1)
IF EMPTY(laOGFxFlt[lnDatePos,6])
  laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate)+'|'+DTOC(gdSysDate)
ENDIF

*--The Ship_Via.
SELECT CODES
SELECT DISTINCT cCode_NO , cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'SHIPVIA   ' AND crltField = 'N';
                                        INTO CURSOR lcHldShVia

SELECT lcHldShVia
INDEX ON cCode_NO TAG lcHldShVia OF (gcWorkDir + 'lcHldShVia' + '.CDX')

*--Create the temp file and fill it in the same time.
=lfCreatTmp()

SELECT(lcalias)
*--End of lfwhengrid.
*!*************************************************************
*! Name      : lfChngForm
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Change printed form.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns    :  None
*!*************************************************************
*! Example    :  =lfChngForm()
*!*************************************************************
FUNCTION lfChngForm

IF lcRpFormat =='D'
  lcRpForm = "ARBILDED"
ELSE
  lcRpForm = "ARBILDES"
ENDIF

*--End of lfChngForm.
*!*************************************************************
*! Name      : lfClearRep
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Close any opened files if user press OG <Close> Button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*-- Close post dated checks file.
IF USED('lcHldShVia')
  USE IN lcHldShVia
ENDIF

*--End of lfClearRep.
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Add fields to the array of file structure.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : lcFldName -- Field Name
*!                   : lcFldType -- Field Type (C;N;L....M)
*!                   : lnFldLen  -- Field Length
*!                   : lnFldDec  -- Field Decimal
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec

lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.
*!*************************************************************
*! Name      : lfRelSumry
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Function to prepare the relations of the summary.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRelSumry()
*!*************************************************************
FUNCTION lfRelSumry

SELECT (lcInvSumry)
SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
SET RELATION TO SHIPVIA INTO lcHldShVia ADDITIVE
SET RELATION TO 'O' + ORDER INTO ORDHDR ADDITIVE

*-- End of lfRelSumry.
*!*************************************************************
*! Name      : lfCollDtal
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Add records in case Summary.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCollDtal()
*!*************************************************************
FUNCTION lfCollDtal
PRIVATE lcAlias , lcTag , lcOrder , lnCount , lcKey
STORE SPACE(0) TO lcAlias , lcTag , lcOrder , lcKey
STORE 0 TO lnCount

*B040155,1  TMI [Start] Open lcInvData with another alias to seek in
lcInvData2 = '__'+SUBSTR(lcInvData,3)
IF !USED(lcInvData2)
  USE (gcWorkDir+lcInvData) AGAIN IN 0 ORDER ORDER ALIAS &lcInvData2
ENDIF
*B040155,1  TMI [End  ] 

lcAlias = SELECT(0)
SELECT (lcInvData)
LOCATE
SET FILTER TO &lcRpExp
lcTag = ORDER()
SET ORDER TO TAG ORDER

SCAN
  lcOrder   = &lcInvData..Order
  IF !SEEK(lcOrder , lcInvSumry)
    SCATTER MEMVAR MEMO
    SELECT (lcInvSumry)
    APPEND BLANK
    GATHER MEMVAR MEMO
  
    *B039366,1  TMI [Start] write only a single scan in lcInvData2 file instead
    *-*IF EMPTY(laOGFxFlt[lnInvPos,6])
    *-*  SELECT INVHDR
    *-*  SCAN
    *-*    WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
    *-*    IF INVHDR.ORDER == lcOrder
    *-*      lnCount = lnCount + 1
    *-*    ENDIF
    *-*  ENDSCAN
    *-*ELSE
	*-*  SELECT (lcInvData)
	*-*  lcKey = EVAL(KEY())
    *-*  SCAN
    *-*    WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
    *-*    IF &lcInvData..ORDER == lcOrder
    *-*      lnCount = lnCount + 1
    *-*    ENDIF
    *-*  ENDSCAN
    *-*  =seek(lcKey)
    *-*ENDIF
    
    *-*IF lnCount > 1
    *-*  SELECT (lcInvSumry)
    *-*  =SEEK(EVAL(lcInvData+'.ORDER'))
    *-*  REPLACE &lcInvSumry..INVOICE WITH "******"
    *-*ENDIF
    
    WAIT WINDOW 'Selecting Records For The Report ...' + m.Invoice NOWAIT
	=SEEK(lcOrder,lcInvData2)
	SELECT (lcInvData2)
	SCAN REST WHILE ORDER = lcOrder
      lnCount = lnCount + 1
      IF lnCount > 1
        REPLACE &lcInvSumry..INVOICE WITH "******"
        EXIT
      ENDIF 
    ENDSCAN    
    *B039366,1  TMI [End  ] 
    
  ELSE
    =SEEK(lcOrder , lcInvSumry)
    *B039366,1  TMI [Start] Select the "lcInvSumry" work aria to avoid macro evaluation and speed up summing values 
    *                       and add the sum of m.ship for all invoices related a certain order
    *REPLACE &lcInvSumry..WEIGHT   WITH &lcInvSumry..WEIGHT   + &lcInvData..WEIGHT  ;
    *        &lcInvSumry..CARTONS  WITH &lcInvSumry..CARTONS  + &lcInvData..CARTONS ;
    *        &lcInvSumry..TOTALCHG WITH &lcInvSumry..TOTALCHG + &lcInvData..TOTALCHG
    SELECT (lcInvData)
    SCATTER FIELDS WEIGHT,CARTONS,TOTALCHG,SHIP MEMVAR
    SELECT (lcInvSumry)
    REPLACE WEIGHT   WITH WEIGHT   + m.WEIGHT  ;
            CARTONS  WITH CARTONS  + m.CARTONS ;
            TOTALCHG WITH TOTALCHG + m.TOTALCHG ;
            SHIP     WITH SHIP     + m.SHIP
    *B039366,1  TMI [End  ] 

  ENDIF
  lnCount = 0
ENDSCAN

*C101945,1 TMI [Start] Close the other opened alias of lcInvData
USE IN &lcInvData2
*C101945,1 TMI [End  ] 

SELECT (lcInvData)
SET ORDER TO TAG &lcTag
SET FILTER TO
SELECT(lcAlias)
LOCATE

*--End of lfCollDtal.