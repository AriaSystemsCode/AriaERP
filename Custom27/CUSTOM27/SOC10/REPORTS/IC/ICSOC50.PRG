*:***************************************************************************
*: Program file  : ICSOC50
*: Program desc. : Custom Style Report for Soccer.
*: For Report    : (ICSOC50.FRX)
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Ahmed Mohamed El_Anwar (AME)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,gfModalGen,gfItemMask.
*:               : lfwOldVal,lfClearRep,lfvFabric,
*:               : lfEvalSegs,lfCollTime,lfCrtCurs,
*:               : lfSRVSty,lfStySum,FaBrow.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICSOC50
*:***************************************************************************
*: This Report Program is due to C102133 ...
*: In Aria 2.6 it was Soc500 with C101728
*:***************************************************************************
*: Modifications :
*:***************************************************************************
lcStTime = TIME()  && STARTING TIME OF COLLECTING DATA.
llCalRetrn = .F.  && Compute returns.

** Get the begining date of the year.
ldBDate = CTOD("01/01/"+STR(YEAR(DATE()),4))

** Add The (Make/Buy) and status to filter expression.
lcMakeOp  = IIF(lcRpMake='M','','!')
lcRepExpr = [style = ''] +;
            IIF(lcRpMake='B','', [ AND ] + lcMakeOp+[Style.Make]) +;
            IIF(lcRpStatus='B','',[ AND (Style.Status="] + lcRpStatus + [")])

lcRpExp =  lcRepExpr + IIF( '.T.' $ lcRpExp , '' , [ AND ] + lcRpExp )

XTitle = lcRpOpTlt

*---- IF The cursor is not exist creat it.
IF !USED(lcTemp)
  =lfCrtCurs()
ENDIF

SELECT ORDLINE
SET RELATION TO "O"+Order INTO OrdHdr ADDITIVE

SELECT Style
SET RELATION TO 'S'+Scale INTO Scale ADDITIVE

IF llClearFn OR llOGFltCh   && if first time  or the OG options have been changed.
  STORE .F. TO llClearFn
  IF RECCOUNT(lcTemp) > 0  && in case of old selection.
    USE IN (lcTemp)
    =lfCrtCurs()
  ENDIF
  SELECT STYLE
  SCAN FOR &lcRpExp  && main scan loop
    WAIT WINDOW "Collecting Data...  Style : "+Style NOWAIT
    *---------- Initialize conditions
    FOR I = 1 TO 8
      Z=STR(I,1)
      lnYTD&Z    = 0     && this year invoices qtys.
      lnCommit&Z = 0     && commissions.
      lnTotSld&Z = 0     &&  Total invoices qtys.
      lnOnHnd&Z  = Style.Stk&Z   
      lnOnOrd&Z  = Style.Wip&Z
    ENDFOR
    
    *---- if there's invoices on this style
    *---- calculate  invoices qtys total & for this year.
    IF SEEK(Style,'InvLine')
      SELECT InvLine
      SCAN WHILE Style = Style.Style
        FOR I = 1 TO Scale.Cnt
          Z=STR(I,1)
          lnTotSld&Z = lnTotSld&Z + Qty&Z  && Calculate  invoices qtys for this style. 
        ENDFOR
        IF InvDate >= ldBDate AND InvDate <= DATE()
          FOR I = 1 TO Scale.Cnt
            Z=STR(I,1)
            lnYTD&Z = lnYTD&Z + Qty&Z   &&Calculate this year invoices qtys. 
          ENDFOR
        ENDIF
      ENDSCAN
    ENDIF

    *---- if there's Orders on this style.
    *---- calculate  calculate commissions for them.
    SELECT Style
    IF SEEK(Style,'OrdLine')
      SELECT OrdLine   
      SCAN WHILE Style = Style.Style FOR OrdHdr.Status $ 'OH'
        FOR I = 1 TO Scale.Cnt
          Z=STR(I,1)
          lnCommit&Z = lnCommit&Z + Qty&Z   && calculate commissions.
        ENDFOR
      ENDSCAN
    ENDIF

    *---- fill in the temp cursor with values have been calculated.
    SELECT &lcTemp
    FOR I = 1 TO Scale.cnt
      Z=STR(I,1)
      APPEND BLANK
      REPLACE Style  WITH Style.Style ,;
              Size   WITH Scale.Sz&Z  ,;
              Desc   WITH Style.Desc  ,;
              YTD    WITH lnYtd&Z     ,;     
              Commit WITH lnCommit&Z  ,;
              TotSld WITH lnTotSld&Z  ,;
              OnHand WITH lnOnHnd&Z   ,;
              OnOrd  WITH lnOnOrd&Z   ,;
              OTS    WITH OnOrd+OnHand-Commit
    ENDFOR

  ENDSCAN   && style file for lcrpexp (main scan loop)
ENDIF
R_WIDTH = 'N'
SELECT &lcTemp
GO TOP
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcTemp))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
DO gfDispRe WITH EVAL('lcRpForm')
*-- end of report code.

  
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs
  lnMajSeg    = gfItemMask('SM')  && No. of major segments.
  *-- Compute Free/Color Items in Style code Structure. [Begin]
  DIMENSION laMajSegs[1,1]
  = gfItemMask(@laMajSegs)

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

  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
  *-- Compute Free/Color Items in Style code Structure. [End]

  *-- Compute Variables titles. [begin]
  lcShowVar = 'Show details by ' + lcNonMajTl
  lcStyStat = lcStyMajor + ' Status'
  lcStyMake = lcStyMajor + ' (Make/Buy)'
  *-- Compute Variables titles. [end]

RETURN ''
*-- end of lfEvalSegs.


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
*! Purpose   : Rise change style flag, in range browse screen.
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
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.


*!*************************************************************
*! Name      : lfvFabric
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
*! Purpose   : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvFabric

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- end of lfvFabric.


*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.
*!*************************************************************
*! Name      : lfCollTime
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
FUNCTION lfClearRep

llClearFn = .T.  && Rise clear done flag.
*-- end of lfClearRep.


*!*************************************************************
*! Name      : lfCrtCurs
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
*! Purpose   : Create temporary cursor.
*!*************************************************************
*! Called from : Program code, OG when function.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtCurs()
*!*************************************************************
FUNCTION lfCrtCurs

CREATE CURSOR (lcTemp) (Style C(19),Size   C(05),Desc   C(20),;
                         YTD   N(08),Commit N(08),TotSld N(08),OnHand N(08),;
                         OnOrd N(08),OTS    N(08))
INDEX ON Style Tag &lcTemp                            



