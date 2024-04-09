*:***************************************************************************
*: Program file  : SRSLJOR
*: Program desc. : Summary Sales Journal Report.
*: For Report    : ....
*: System        : Aria Advantage Series.
*: Module        : Sales Representative (SR)
*: Developer     : Mohamed Atia Badran (MAB)
*: Date          : 12/29/1998
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfModalGen,gfRepCur,gfBrows,gfDispRe.
*:               : lfwRepWhen,lfwOldVal,lfvChFact,lfvList,lfvCurDisp,lfvCurr,lfCollTime,
*:               : lfCreatCur,lfClearRep,lfPreRun,lfMakeExpr,lfChRp1To2.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SRSLJOR
*:***************************************************************************
*: This Report Program is due to E301106 ...
*:E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.
*:B603713,8  MHM 10/16/2000 increase the length of the fields (Gros_Price ,Price, 
*:B603713,8                 PriceA , PriceB , PriceC , Amount) With 2 digit in FRX and add 
*:B603713,8  MHM 10/16/2000 record in SYREPUVR to get print dicemal yes or no 
*:B605948,1 ADEL 07/08/02 Functionate the variable filter options work (Division
*:B605948,1               season and Term code)
*:***************************************************************************

*----------------------- Report Code Begin -----------------------------
lcStTime   = TIME()    && Time in which we start collect data.

IF TYPE('lcLastExpr') = 'N'
  lcLastExpr = ''
ENDIF

*-- lcLastExpr : Last <Run> OR <Preview> lcRpExp.
*-- llChFactor : .T. if user change Factored/Non Factored/Both selection.
*-- llChInv    : .T. if user change Invoces/Void Invoices/Both selection.

*-- If user change report critria, Collect report data. 
*E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.[Begin
*IF llClearFn OR !(lcRpExp == lcLastExpr) OR ;
*   (llChFactor OR llChInv)
IF llClearFn OR llOGFltCh
*E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.[End

  STORE .F. TO llClearFn,llChFactor,llChInv    && Unrise all hidden Critria variables.
  lcLastExpr = lcRpExp   && Save current report expression, To later know that user change critria.

  *-- if you have previous data clear workfile then recreate it. [begin]
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF  
    = lfCreatCur()  && Create work cursor.
  ENDIF
  *-- if you have previous data clear workfile then recreate it. [end]
  
  PRIVATE lcFltExpr,lcJorExpr,lcSlRepExp
  lcJorExpr = [INVOICE = '']
  *-- loop fixed filter array to make filter expressions.
  FOR lnI = 1 TO ALEN(laOGFxFlt,1)
    *-- if User enter filtered data [Begin]
    IF !EMPTY(laOGFxFlt[lnI,6]) AND IIF(laOGFxFlt[lnI,7]='V', .T. , ;
       USED(laOGFxFlt[lnI,6]) AND RECCOUNT(laOGFxFlt[lnI,6]) > 0)
       
       
       lcFltExpr = lfMakeExpr(lnI)  && Make Expression

       *-- if line is for sales representative.
       IF 'REP1' $ lcFltExpr
         lcSlRepExp = STRTRAN(lcFltExpr,'INVHDR.','')
         lcFltExpr  = '(' + lcFltExpr + ' OR ' + STRTRAN(lcFltExpr,'REP1','REP2') + ')'
       ENDIF
       
       IF !EMPTY(lcFltExpr)
         lcJorExpr = lcJorExpr + ' AND ' + STRTRAN(lcFltExpr,'INVHDR.','')
       ENDIF
    
    ENDIF
    *-- if User enter filtered data [Begin]
  ENDFOR  && end loop fixed filter array to make filter expressions.
  *B605948,1 (Begin) Get variable filter Options enterd in OG
  lcJorExpr = lcJorExpr + IIF(EMPTY(lcJorExpr),'', ' AND ') + lfGetVrFlt()
  *B605948,1 (End)

  *-- Customize lcJorExpr [Begin]
  IF lcRpType <> 'B'
    lcFactOper = IIF(lcRpType = 'F','!','')
    lcJorExpr  = lcJorExpr + [ AND  &lcFactOper.EMPTY(cFacCode)]
  ENDIF
  
  IF lcRpList <> 'B'
    lcStatOper = IIF(lcRpList = 'C','!','')
    lcJorExpr  = lcJorExpr + [ AND  STATUS &lcStatOper.= 'V']
  ENDIF

  *-- If you print Void invoices only.
  IF llVoidOnly
    lcJorExpr = STRTRAN(lcJorExpr,'INVDATE','VDATE')
  ENDIF
  *-- Customize lcJorExpr [End..]

  *-- Data collection Start
  SELECT INVHDR
  SET ORDER TO
  PRIVATE lcSlRep2
  SCAN FOR &lcJorExpr
    SCATTER MEMVAR MEMO
  
    IF 'REP1' $ lcJorExpr
    
      IF EVALUATE(lcSlRepExp)
        INSERT INTO (lcWorkFile) FROM MEMVAR
      ENDIF
      
      lcSlRep2 = STRTRAN(lcSlRepExp,'REP1','REP2')
      IF EVALUATE(lcSlRep2)
        =lfChRp1To2()     && Change Rep1 to Rep2 and insert new line.
      ENDIF
    
    ELSE  && No rep filter
    
      INSERT INTO (lcWorkFile) FROM MEMVAR
      *-- Add Record for Rep2 [Begin
      IF !EMPTY(m.Rep2)
        =lfChRp1To2()     && Change Rep1 to Rep2 and insert new line.
      ENDIF
      *-- Add Record for Rep2 [End..
    ENDIF  
  ENDSCAN  
  *-- Data collection End

ENDIF       && end If user change report critria, Collect report data. 

*-- Select Master report file.
SELECT (lcWorkFile)
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer ,;
                Rep1 INTO Salesrep , ccurrcode INTO Syccurr

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' TIMEOUT 2

SELECT (lcWorkFile)
GO TOP

IF RECCOUNT(lcWorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

*-- Calculate From and To dates. [Begin]
IF EMPTY(laOGFxFlt[lnDatePos,6])
  lcInvDateF = DTOC(CTOD(SPACE(10)))
  lcInvDateT = DTOC(gdSysDate)
ELSE
  lcInvDateF = PADR(SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1),10)
  lcInvDateT = PADL(SUBSTR(laOGFxFlt[lnDatePos,6],ATC('|',laOGFxFlt[lnDatePos,6])+1),10)
ENDIF  

IF EMPTY(CTOD(lcInvDateT))
  lcInvDateT = DTOC(gdSysDate)
ENDIF
*-- Calculate From and To dates. [End..]

*-- Call Report [lcRpForm = 'SRSLJOR.FRX']
DO gfDispRe WITH EVAL('lcRpForm')
*----------------------- Report Code End -----------------------------


*-- Function Section 
*-------------------------------------------

*!*************************************************************
*! Name      : lfMakeExpr
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/30/1998
*! Purpose   : Make an expression for one item in fixed filter array
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Item row in fixed filter array
*!*************************************************************
*! Returns            : line expression
*!*************************************************************
*! Example   : =lfMakeExpr()
*!*************************************************************
*!
FUNCTION lfMakeExpr
PARAMETERS lnFxFltItm
PRIVATE lcItmExpr

lcItmExpr = ''
*-- if operator is like
IF 'LIKE' $ UPPER(ALLTRIM(laOGFxFlt[lnFxFltItm,5]))
  lcItmExpr = ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ' = "' +  laOGFxFlt[lnFxFltItm,6] + '"'

ELSE  && operator is either Between or InList

  *-- if operator is between.
  IF 'BETWEEN' $ UPPER(ALLTRIM(laOGFxFlt[lnFxFltItm,5]))
    *-- if Type is Date
    PRIVATE lcFrom,lcTo
    lcFrom = SUBSTR(laOGFxFlt[lnFxFltItm,6],1,ATC('|',laOGFxFlt[lnFxFltItm,6]) - 1)
    lcTo   = SUBSTR(laOGFxFlt[lnFxFltItm,6],ATC('|',laOGFxFlt[lnFxFltItm,6]) + 1)

    IF laOGFxFlt[lnFxFltItm,3] = 'D'
      
      lcFrom = CTOD(lcFrom)
      lcTo   = CTOD(lcTo)

      IF !EMPTY(lcFrom)
        lcFrom = DTOS(lcFrom)
        lcTo   = DTOS(lcTo)
        lcItmExpr = 'BETWEEN( DTOS(' + ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ') , "' + ;
                                 lcFrom + '" , "' + lcTo + '")'
      ENDIF  

    
    ELSE  && Type is not date (this case not occured in this report)
    
    ENDIF

  ELSE  && else operator is in list

    *-- if in range case.
    IF laOGFxFlt[lnFxFltItm,7] = 'R'
      lcItmExpr = 'SEEK(' + ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ', "' +;
                            ALLTRIM(laOGFxFlt[lnFxFltItm,6]) + '")'
    ELSE  && default in list case
      lcItmExpr = ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ' $ "' + laOGFxFlt[lnFxFltItm,6] + '"'
    ENDIF                           
    
  ENDIF

ENDIF
RETURN IIF(EMPTY(lcItmExpr),lcItmExpr,'(' + lcItmExpr + ')')
*-- end of lfMakeExpr.

*!*************************************************************
*! Name      : lfChRp1To2
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/30/1998
*! Purpose   : Change line for Rep1 to have information for Rep2.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code Scan loop section.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : =lfChRp1To2()
*!*************************************************************
*!
FUNCTION lfChRp1To2
m.Rep1      = m.Rep2
m.Comm1     = m.Comm2
m.CommAmt1  = m.CommAmt2
m.VCommAmt1 = m.VCommAmt2
INSERT INTO (lcWorkFile) FROM MEMVAR
*-- end of lfChRp1To2.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/28/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfCreatCur
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*!
FUNCTION lfwRepWhen
*-- if you first time enter when function.
IF TYPE('lcLastExpr') = 'N'
  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO SALESREP IN SALESREP  && To use it to validate REP     # in option grid.
  SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate LOCATION# in option grid.
  SET ORDER TO Codes IN CODES        && To use it to validate STATE# in option grid.

  DIMENSION laTempStru[1,4]
  laTempStru = ''

  SELECT INVHDR
  = AFIELDS(laTempStru)
  = lfCreatCur()  && Create work cursor.

  IF llMultCurr
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
  ENDIF

  lnDatePos = ASCAN(laOGFxFlt,'INVHDR.INVDATE')
  IF lnDatePos > 0
    lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
    IF EMPTY(laOGFxFlt[lnDatePos,6])
      laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
    ENDIF
  ENDIF

ENDIF  && end if you first time enter when function.
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/1998
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
*!
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvRepCode
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/98
*! Purpose   : Validate Sales Rep. in SALESREP file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRepCode()
*!*************************************************************
*!
FUNCTION lfvRepCode
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SALESREP'))
  SELECT SALESREP
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "REPCODE   :R :H= 'Code' , "   +;
               "NAME      :R :H= 'Name' ,"    +;
               "cAddress6 :R :H= 'Country' ," +;
               "PHONE     :R :H= 'Phone' ,"   +;
               "BALANCE   :R :H= 'Balance' "
  
  lcFile_Ttl = "Sales Representative ..."
  = gfBrows('','REPCODE','laTemp')
    
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvRepCode.

*!*************************************************************
*! Name      : lfvChFact
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/98
*! Purpose   : 1- Change Factored/Non Factored logical variable To recollect data.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid [Factored/Non Factored/Both]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvChFact()
*!*************************************************************
*!
FUNCTION lfvChFact
llChFactor = .T.
*-- end of lfvChFact.

*!*************************************************************
*! Name      : lfvList
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/98
*! Purpose   : Change Invoices/Void Invoices logical variable To recollect data.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid [Invoices/Void Invoices/Both]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvList()
*!*************************************************************
*!
FUNCTION lfvList
llChInv = .T.  && Logical invoice variable.
llVoidOnly = IIF(lcRpList='V',.T.,.F.)  && To use it in .FRX 
*-- end of lfvList.

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/29/98
*! Purpose   : Activate currency display screen to get user 
*!           : selection for currencies.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfRepCur
*!*************************************************************
*! Called from : Option Grid [Currency Display Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*!
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfvCurr
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/29/98
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
*!
FUNCTION lfvCurr
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SYCCURR'))

  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "CCURRCODE :R :H= 'Currency' , "   +;
               "CCURRSMBL :R :H= 'Symbol' ,"      +;
               "CCURRDESC :R :H= 'Description' ," +;
               "NCURRUNIT :R :H= 'Units' "
  
  lcFile_Ttl = "Currencies ..."
  = gfBrows('','CCURRCODE','laTemp')
    
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvCurr.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/98
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
*! Name      : lfCreatCur
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/98
*! Purpose   : Create cursor
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : Cursor Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCreatCur()
*!*************************************************************
*!
FUNCTION lfCreatCur
CREATE CURSOR (lcWorkFile) ;
   FROM ARRAY laTempStru
INDEX ON Rep1 + Invoice TAG (lcWorkFile)
*-- end of lfCreatCur.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/1998
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
*-- Delete temporary work file.
IF USED(lcWorkFile)
 USE IN (lcWorkFile)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfPreRun
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/29/98
*! Purpose   : Pre_Preview Function To call Temporary .FRX again
*!           : to evaluate #OBJDISP objects again.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report Generator when press <Preview> or <Run>
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPreRun()
*!*************************************************************
FUNCTION lfPreRun
RETURN .T.

*-- end of lfPreRun.

*!*************************************************************
*! Name      : lfGetVrFlt
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 07/08/02
*! Purpose   : Functunate the variable filter options (Division,
*!           : Season and Term code.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report Generator when press <Preview> or <Run>
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetVrFlt()
*!*************************************************************
*! B605948,1
FUNCTION lfGetVrFlt

PRIVATE lcVarFlt
lcVarFlt = ""
*--Get division filter
DO CASE
  *--Division Filter
  CASE ATC("INVHDR.CDIVISIO",lcRpExp) <> 0
    *-- Case of INLIST
    IF ATC("INLIST(INVHDR.CDIVISIO",lcRpExp) <> 0
      lcVarFlt = lcVarFlt + IIF(EMPTY(lcVarFlt),""," AND ") + ;
                 SUBSTR(lcRpExp,ATC("INLIST(INVHDR.CDIVISION",lcRpExp),IIF(ATC(")",SUBSTR(lcRpExp,ATC("INLIST(INVHDR.CDIVISION",lcRpExp)))<>0,ATC(")",SUBSTR(lcRpExp,ATC("INLIST(INVHDR.CDIVISION",lcRpExp))),LEN(lcRpExp)))
    *--Case of = CASE
    ELSE
      *--Don't rely on the last ")" as aria27 builds the expression deffirently whe Prev and Run
      lcVarFlt = lcVarFlt + IIF(EMPTY(lcVarFlt),""," AND ") + ;
                 SUBSTR(lcRpExp,ATC("INVHDR.CDIVISION",lcRpExp),IIF(ATC(")",SUBSTR(lcRpExp,ATC("INVHDR.CDIVISION",lcRpExp))) <> 0,ATC(")",SUBSTR(lcRpExp,ATC("INVHDR.CDIVISION",lcRpExp)))-1,LEN(lcRpExp)))
    ENDIF
  *--Season Filter    
  CASE ATC("INVHDR.SEASON",lcRpExp) <> 0    
    *-- Case of INLIST
    IF ATC("INLIST(INVHDR.SEASON",lcRpExp) <> 0
      lcVarFlt = lcVarFlt + IIF(EMPTY(lcVarFlt),""," AND ") + ;
                 SUBSTR(lcRpExp,ATC("INLIST(INVHDR.SEASON",lcRpExp),IIF(ATC(")",SUBSTR(lcRpExp,ATC("INLIST(INVHDR.SEASON",lcRpExp)))<>0,ATC(")",SUBSTR(lcRpExp,ATC("INLIST(INVHDR.SEASON",lcRpExp))),LEN(lcRpExp)))
    *--Case of = CASE
    ELSE
      lcVarFlt = lcVarFlt + IIF(EMPTY(lcVarFlt),""," AND ") + ;
                 SUBSTR(lcRpExp,ATC("INVHDR.SEASON",lcRpExp),IIF(ATC(")",SUBSTR(lcRpExp,ATC("INVHDR.SEASON",lcRpExp)))<>0,ATC(")",SUBSTR(lcRpExp,ATC("INVHDR.SEASON",lcRpExp)))-1,LEN(lcRpExp)))
    ENDIF
  *--Payment Term Filter  
  CASE ATC("INVHDR.CTERMCODE",lcRpExp) <> 0
    *-- Case of INLIST
    IF ATC("INLIST(INVHDR.CTERMCODE",lcRpExp) <> 0
      lcVarFlt = lcVarFlt + IIF(EMPTY(lcVarFlt),""," AND ") + ;
                 SUBSTR(lcRpExp,ATC("INLIST(INVHDR.CTERMCODE",lcRpExp),IIF(ATC(")",SUBSTR(lcRpExp,ATC("INLIST(INVHDR.CTERMCODE",lcRpExp)))<>0,ATC(")",SUBSTR(lcRpExp,ATC("INLIST(INVHDR.CTERMCODE",lcRpExp))),LEN(lcRpExp)))
    *--Case of = CASE
    ELSE
      lcVarFlt = lcVarFlt + IIF(EMPTY(lcVarFlt),""," AND ") + ;
                 SUBSTR(lcRpExp,ATC("INVHDR.CTERMCODE",lcRpExp),IIF(ATC(")",SUBSTR(lcRpExp,ATC("INVHDR.CTERMCODE",lcRpExp)))<>0,ATC(")",SUBSTR(lcRpExp,ATC("INVHDR.CTERMCODE",lcRpExp)))-1,LEN(lcRpExp)))
    ENDIF
ENDCASE
RETURN IIF(!EMPTY(lcVarFlt),lcVarFlt,'.T.')
