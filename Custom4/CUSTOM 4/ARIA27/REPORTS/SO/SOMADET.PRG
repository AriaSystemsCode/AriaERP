*:******************************************************************************************
*: Program file  : SOMADET
*: Program desc. : Material Sales Order Detail Report
*: For Report    : (SOMADET.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : RANIA ABDEL RAZIK (RAE)
*:******************************************************************************************
*: Calls : 
*:    Functions  : gfItemMask(),gfPhoneTem(),gfDispRe() , gfOptMsg()
*:               : lfGetNotes(),lfvOptMsg(),lfwOldVal(),lfClearRep()
*:               : lfwRepWhen()
*:******************************************************************************************
*: Passed Parameters  : None
*:******************************************************************************************
*: Notes   : 1- All IF llFrTime Blocks executed one time in the option grid
*:         :    and if user selection opens any temp. files that may
*:         :    be used later in another selection I take this file open to 
*:         :    use untill user choice is to press < Close > button, 
*:         :    to save more time.
*:         : 2- Any variable start by (llCh) means that some thing in 
*:         :    selected critria was changed, you must collect data again.
*:*****************************************************************************************
*: Example : DO SOMADET
*:*****************************************************************************************
*: Modifications ..
*:B607223,1 ALB 05/26/2003 Fix error massage when print complete Material Order
*:E126684,1 TMI 03/03/2005 Remove zeros in the right of the decimal point.
*:N125956,1 TMI 03/23/2005 if there is no numbers after the decimal , remove the decimal point
*:******************************************************************************************

lcTime     = TIME()                     && Variable to hold the Time.
lcStTime   = lcTime                     && Time in which we start collect data.
llPrntBoth = llRpOrdNot AND llRpOrdLnt  && True if you print both types of notes.
lnLastRec  = 2                          && Record No. Of last record in order group.
lcTitle    = ''                         && Title of Note.
lcNotes    = ''                         && Notes.
llNoIndex  = .F.                        && I don't make index for file.

*-- Define variables used in Print dos format and multi currency format.
lcPhonPict = gfPhoneTem()
*-- Print totals if and only if [Not multi currency or user want any equavelent method or 
*-- select only one currency to print]
llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
             ((!EMPTY(laOGFxFlt[lnCurrPos,6]) AND ATC("|",laOGFxFlt[lnCurrPos,6])=0) OR ;
              (!EMPTY(laOGFxFlt[lnOrdPos ,6]) AND USED(laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(laOGFxFlt[lnOrdPos,6])=1))

llCurInGrp = !llPrintTot

STORE '' TO lcLineCurr
STORE .T. TO llInnTotal,llOutTotal
llGrdTotal = llPrintTot

lcSeekVal  = ''                                   && Get Last record seek value.
llTextMode = (UPPER(ALLTRIM(lcRepMode))=="TEXT")  && Print Text Format.
lcStorCond = ''

lcOldScale = SPACE(3)
*-- Declare a variable for the first scale.
llFirstSc = .F.

DIMENSION laStock[9],laWip[9]
STORE '' TO laStock,laWip                            && Work process arrays.
STORE '' TO lcGrpExp,lcSeaExp,lcDivExp,lcStatusEx
*-- Show messages in status bar when collecting data.
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON

*-- if it's first time you run option Grid, i.e: you have unknown variables.

IF llFrTime
  *-- Create temporary file that holding order line data.
  lcWorkFile = gfTempName()
  lcTempLine = gfTempName()
  
  DIMENSION laTempStru[1,4]
  laTempStru = ''
  SELECT MASOLIN
  = AFIELDS(laTempStru)
  DIMENSION laTempStru[ALEN(laTempStru,1) + 2, 4]

  *-- cTempKey :  field used in most sort by case as the master key ,
  *--          :  and in case of summarize multi store as the total amount.
  laTempStru[ALEN(laTempStru,1) -1  ,1] = 'cTempKey'
  laTempStru[ALEN(laTempStru,1) -1  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) -1  ,3] = 16
  laTempStru[ALEN(laTempStru,1) -1  ,4] = 0

  *-- cCurrCode :  used if multi currency only to sort by it.
  laTempStru[ALEN(laTempStru,1)  ,1] = 'cCurrCode'
  laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)  ,3] = 3
  laTempStru[ALEN(laTempStru,1)  ,4] = 0

  llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.
ENDIF

*-- Create temporary cursors from structure array.
IF EMPTY(lcMastFile) OR !USED(lcMastFile)

  *-- Setting for report.
  lcSetHour = SET('HOURS')
  SET HOURS TO 24

  *-- lcNoteLns : Name of Temp. Loop File which is used to print both line notes 
  *--           : and notepad from notepad file.
  *--           : note that this name and temp. file is created 
  *--           : one for every optional grid seasson run.
  lcNoteLns = gfTempName()
  *-- create temp. file that used if you have both types of notes.
  
  CREATE CURSOR (lcNoteLns)  (cRecord C(2))
  INDEX ON cRecord TAG (lcNoteLns) OF (gcWorkDir+lcNoteLns)
  FOR lnI = 1 TO 2
    APPEND BLANK
   REPLACE cRecord WITH "N"+ALLTRIM(STR(lnI))
  ENDFOR

  *-- Create work file.
  = lfCreatCur(lcWorkFile)  && Create work cursor.
  = lfCreatCur(lcTempLine)  && Create line cursor.  
ENDIF

=lfGetRepVr()      && Get Report variables such as groups and index.

*-- If user change report critria, Collect report data. 
*-- Use variable llOGFltCh that detect OG filter changes.
IF llClearFn OR llOGFltCh
  llClearFn = .F.
  lcStartSt = DTOS(CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],1,;
                  ATC('|',laOGFxFlt[lnStartPos,6])-1)))
  lcStartEd = DTOS(CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],;
                   ATC('|',laOGFxFlt[lnStartPos,6])+1)))
  llSrtSides = EMPTY(ALLTRIM(lcStartSt+lcStartEd))

  lcCompSt  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],1,;
                   ATC('|',laOGFxFlt[lnCompPos,6])-1)))
  lcCompEd  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],;
                   ATC('|',laOGFxFlt[lnCompPos,6])+1)))
  llCmpSides = EMPTY(ALLTRIM(lcCompSt + lcCompEd))
  
  lcStatusEx = [MASOHDR.STATUS $ lcRpStatus]
  
  *-- All in list codes Changed to be $ not INLIST function
  *-- 1- purchase group.
  IF !EMPTY(laOGFxFlt[lnGrpPos,6])
    lcGrpExp  = "&laOGFxFlt[lnGrpPos,1]." + ' $ laOGFxFlt[lnGrpPos,6]'
  ENDIF
  
  *-- Evaluate Color/Free Expression.
  *-- Note that: We use either Only This XXX color object or direct XXX 
  *--            Free object, and you must know that both types of 
  *--            expressions can't be enable at the same time.

  *-- Adjust color filter to depend to variable postions.
   
  *-- lcCrFrExp : Color expr.
  lcCrFrExp = ''
  IF !EMPTY(laOGFxFlt[lnClrSgPos,6])
    lcCrFrExp  = "MASOLIN.COLOR" + ' $ laOGFxFlt[lnClrSgPos,6]'
  ENDIF
  lcLastExpr = lcRpExp   && To later know that user change critria.

  =lfScanData()  && Scan around master file.
  
  *-- Unrise all Critria variables.
  STORE .F. TO llChSelect,llChStatus,llChSumm,;
               llChAcc,llChFabric,llChRep,llChOrder,llChLoc

ELSE  &&  user does not change report critria.
  IF lcLastTag != lcIndexTg
    SELECT (lcMastFile)
    INDEX ON &lcIndexTg TAG (lcMastFile)
    lcLastTag = lcIndexTg  && To later know that user change Sort case.
  ENDIF		&& end if user Sort By . 
ENDIF       && end If user change report critria, Collect report data. 

SELECT MASOLIN
SET RELATION OFF INTO MASOHDR  && break relation.
SET RELATION OFF INTO FABRIC  && break relation.

*-- If Sort by Sales Rep. , set relation to Primary sales rep. file.
IF lcRpSortBy = 'R'
  SELECT MASOHDR
  SET RELATION TO REP1 INTO SALESREP ADDITIVE
ENDIF

*-- Temporary File relations, in the way that help us in report form.
SELECT (lcMastFile)
SET RELATION TO cOrdType + cmOrder INTO MASOHDR      && To order header file.
SET RELATION TO Fabric INTO Fabric ADDITIVE         && To Fabric file.
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE   && To scale file.
SET RELATION TO cWareCode INTO WAREHOUS ADDITIVE  && To warehouse file.

lcCustRel = IIF(llRpSummMt,['M' + Account],;
            [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
            
SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.

*-- If sort by purchase group , set relation to codes file.
IF lcRpSortBy = 'G'
  SET RELATION TO gcAct_Comp+SUBSTR(cTempKey,8,6) INTO CODES ADDITIVE
ENDIF

*-- if you print both type of notes.
IF llPrntBoth
  SELECT (lcMastFile)
  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
  SET SKIP TO &lcNoteLns
ENDIF

*-- if you print order notepad, open master file in another alias to 
*-- help us to know what last line in order group to print notepad
*-- after it, note we do this because we print notepad in detail band
*-- not in order group band .

IF llRpOrdNot
  USE (gcWorkDir+lcMastFile) ORDER TAG (lcMastFile) IN 0 AGAIN ALIAS GETLAST
ENDIF
lcRepExpr = [IIF(llPrntBoth,;
             IIF(&lcNoteLns..cRecord = 'N2',RECNO(lcMastFile) = lnLastRec ,.T.),;
             .T.)]    && Report expression.

*-- Select Master report file.
SELECT (lcMastFile)
SET FILTER TO FABQTY <> 0

GO BOTTOM
lnLastOne  = RECNO()
GO TOP    && Refresh Relation

*--if DOS mode.
IF llTextMode
  STORE ' ' TO lcInnGrpIn,lcOutGrpIn,lcInnGrpOp,lcOutGrpOp

  STORE '' TO lcPrnInnL
  STORE 0 TO lnInnQty1,lnInnQty2,lnInnQty3,lnInnQty4,;
             lnInnQty5,lnInnQty6,lnInnQty7,lnInnQty8,;
             lnInnTtQty,lnGrInnAmt
  STORE 0 TO lnPrnInnQ1,lnPrnInnQ2,lnPrnInnQ3,lnPrnInnQ4,;
             lnPrnInnQ5,lnPrnInnQ6,lnPrnInnQ7,lnPrnInnQ8,;
             lnPrnInnTQ,lnPrnInnAm
  
  *-- Add sort by store if first sort is by order.
  llLstMulti = (MASOHDR.MULTI = "Y")

  =lfInnGrpOp()

  STORE '' TO lcPrnOutL
  STORE 0 TO lnOutQty1,lnOutQty2,lnOutQty3,lnOutQty4,;
             lnOutQty5,lnOutQty6,lnOutQty7,lnOutQty8,;
             lnOutTtQty,lnGrOutAmt
  STORE 0 TO lnPrnOutQ1,lnPrnOutQ2,lnPrnOutQ3,lnPrnOutQ4,;
             lnPrnOutQ5,lnPrnOutQ6,lnPrnOutQ7,lnPrnOutQ8,;
             lnPrnOutTQ,lnPrnOutAm

  =lfOutGrpOp()
  
  *-- Avoid do any thing in .FRXs
  lcRpSort2 = IIF(lcRpSrt2$"TY","T",lcRpSrt2)
ENDIF             

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)

*--if DOS mode.
IF llTextMode
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMastFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
ELSE  && else window format
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMastFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' TIMEOUT 1
ENDIF
DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRepExpr

WAIT CLEAR
*-- If Sort by Sales Rep. , set relation to Primary sales rep. file.
IF lcRpSortBy = 'R'
  PRIVATE lcCurSel
  lcCurSel = ALIAS()
  SELECT MASOHDR
  SET RELATION OFF INTO SALESREP
  SELECT (lcCurSel)
ENDIF

IF llRpOrdNot
  USE IN GETLAST
ENDIF  

SET STATUS BAR &lcStatusBr
RETURN
*-- end of Report Code.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
*-- if it's first time to run the report.
*-- using TYPE of variable instead of global llFirstTime, to control
*-- reset case which does not rise llFirsttime, but restore initial
*-- value for lnVarbEnd and advanced case which keep the variables same.
IF TYPE('lnVarbEnd') = 'C'
  lnVarbEnd = 0
  DECLARE laRpSource[5]  
  IF TYPE('laRpTarget[1]') = 'C'
    IF EMPTY(laRpTarget[1])
      DECLARE laRpTarget[1]
    ELSE
      FOR lnI = 2 TO ALEN(laRpTarget)
        IF TYPE('laRpTarget[lnI]') = 'U'
          laRpTarget[lnI] = ""
        ENDIF
      ENDFOR    
    ENDIF    
  ELSE
    DECLARE laRpTarget[1]
  ENDIF

  STORE 'Bid'      TO laRpSource[1]  
  STORE 'Open'     TO laRpSource[2]
  STORE 'Hold'     TO laRpSource[3]
  STORE 'Complete' TO laRpSource[4]
  STORE 'Canceled' TO laRpSource[5]
  lcRpStatus = 'BOHCX'

  SET ORDER TO MASOHDR IN MASOHDR      && To use it to validate ORDER   # in option grid.
  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO Fabric IN Fabric        && To use it to validate Fabric   # in option grid.
  SET ORDER TO SALESREP IN SALESREP  && To use it to validate REP     # in option grid.
  SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate LOCATION# in option grid.

  IF 'MA' $ gcCmpModules
    SET ORDER TO FABRIC IN FABRIC      && To use it to validate FABRIC  # in option grid.
  ENDIF  

  lnAccPos   = lfItmPos('CUSTOMER.ACCOUNT')
  lnFabPos   = lfItmPos('FABRIC.FABRIC')
  lnLocPos   = lfItmPos('WAREHOUS.CWARECODE')
  lnRepPos   = lfItmPos('SALESREP.REPCODE')
  lnOrdPos   = lfItmPos('MASOHDR.CMORDER')
  lnStartPos = lfItmPos('MASOLIN.START')
  lnCompPos  = lfItmPos('MASOLIN.COMPLETE')
  lnGrpPos   = lfItmPos('FABRIC.CPURCODE')
  lnPriPos   = lfItmPos('MASOHDR.PRIORITY')  
  lnClrSgPos = lfItmPos('FABRIC.COLOR')

  *-- Adjust currency symbol.
  IF llMultCurr
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.    
    lnCurrPos  = lfItmPos('MASOHDR.CCURRCODE')    
    *-- No default currency.    
  ENDIF
  *-- Compute Start of variable filter to control its apperance
  *-- in option grid.
  *-- laRpFltVal : Array to hold D for disable and E for enable, to control
  *--            : INLIST appearance in option grid.
  *-- laRpVarNow : Array to hold .T. or .F., to control variables 
  *--            : appearance in option grid. 
  *-- Disable objects that disabled before.
ELSE
  FOR lnElm = 1 TO ALEN(laOgObjType,1) 
   IF laOgObjType[lnElm,1] == "lcRpSortBy"
     _CUROBJ= OBJNUM(&laOgObjType[lnElm,2].)+1
   ENDIF
   IF lcDummy = "Y" AND laOgObjType[lnElm,1] == "lcRpSelcBy"
     _CUROBJ= OBJNUM(&laOgObjType[lnElm,2].)+1
     lcDummy = "N"
   ENDIF
  ENDFOR
ENDIF

*-- Disable/enable By account, fabric, location, sales representative.
*-- note that disable and enable is according to value of laRpFltVal.

IF lcRpSelcBy = "L" OR !(lcRpSelcBy == lcOldSelc)
  STORE .T. TO llClearAcc,llClearFab,llClearLoc,llClearRep
ENDIF
lcOldSelc = lcRpSelcBy

*-- Add sort by store if first sort is by order.
IF lcRpSortBy = "O" AND lcRpKind = "D" AND ALEN(laSort2Des,1) = 2
  DIMENSION laSort2Des[4,1] , laSort2Val[4,1]
  laSort2Des[3,1] = "Store/Line#"
  laSort2Des[4,1] = "Store/Fabric"
  laSort2Val[3,1] = "T"
  laSort2Val[4,1] = "Y"
  CLEAR READ
ENDIF

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;                                                            
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))))
  ENDFOR
ENDIF
lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))

*-- Checking for the Order Status to Enable/Disable the 2 options which
*-- was added to the selection grid.
DO lpChkStat
*-- end of lfwRepWhen.

*!**************************************************************************
*! Name      : lfwOldSelc
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : To store the Old Select By value
*!**************************************************************************
*! Example   : = lfwOldSelc()
*!**************************************************************************

FUNCTION lfwOldSelc
lcOldSelc = lcRpSelcBy
*-- End of lfwOldSelc.

*!**************************************************************************
*! Name      : lfVarPos
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : To get the position of the Variable in OG.
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : = lfVarPos()
*!**************************************************************************
*-- This function was added in order to get the Position of the 2 options
*-- Added in Selection grid so as to Enable/Disable them.
FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGObjType,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfVarPos.

*!**************************************************************************
*! Name      : lpChkStat
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : To check for Order status in Order to Enable/Disable the 2
*!             Option added to the Selection grid.
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : DO lpChkStat
*!**************************************************************************

PROCEDURE lpChkStat
PRIVATE llStatus
llStatus = IIF('O' $ lcRpStatus OR 'H' $ lcRpStatus,.T.,.F.)
IF !llStatus
  llRpPrtShp = .F.
ENDIF

*-- Disable (Partially shipped Orders & Include Allocated Order Lines) if Order Status
*-- contains only Complete or Cancelled
laOGObjCnt[lfVarPos('llRpPrtShp')] = llStatus
=lfOGShowGet('llRpPrtShp')

*-- End of lpChkStat.

*!*************************************************************
*! Name      : lfScanData
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Collect report data.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfScanData()
*!*************************************************************
*! Notes     : To easy understand documentaion of this function
*!           : keep in your mind that 
*!           : 1- <MASOHDR filter>  is for : open quantity, order status,; 
*!           :                              and priority. 
*!           : 2- <MASOLIN filter> is for : total quantity, line group,;
*!           :                              start date, complete date.
*!           : 3- <purchase group filter> is for   : purchase group
*!           : 4- <Coler Filter>  is for   : Color/Free.
*!           : 5- There is relation between MASOLIN file and both MASOHDR and ; 
*!           :    Fabric files, to easy lock its data.  
*!           : 6- Because we have a field called cTempKey in temp. files
*!           :    we fill its memory(m.cTempKey) with the required data
*!           :    that help us collecting data rush more and fast printing
*!           :    in the report without evaluating values that spent time. 
*!           : 7- lcSeekExp, is expression we sum rush more for it in case 
*!           :    of summarize multi store orders.  
*!*************************************************************
FUNCTION lfScanData

*-- If you find any data (i.e: not first time you run), clear it. 
IF RECCOUNT(lcTempLine) > 0
  *-- We need temp. files to be files not cursor to open it in another alias.
  SELECT (lcTempLine)
  ZAP
  *-- Rest any relation before data collection.
  SET RELATION TO  
  llNoIndex = .T.
ENDIF

*-- Change index due to changes to Sort By type.
IF llNoIndex OR (lcLastTag != lcIndexTg)
  SELECT (lcTempLine)
  INDEX ON &lcIndexTg TAG (lcTempLine)
  IF llNoIndex
    llNoIndex = .F.
  ELSE  
    lcLastTag = lcIndexTg
  ENDIF
ENDIF

*-- lcWorkTag : Variable which hold value of working file index.
*--           : Note that putting index this way help us in collecting
*--           : data rush more.
lcWorkTag = IIF(lcRpSelcBy = 'A',[ACCOUNT+CORDTYPE+cmORDER+STR(LINENO,6)]                ,;
            IIF(lcRpSelcBy = 'F',[LEFT(cTempKey,7) +Fabric+CORDTYPE+cmORDER+STR(LINENO,6)],;
            IIF(lcRpSelcBy = 'W',[CWARECODE+CORDTYPE+cmORDER+STR(LINENO,6)]              ,;
            IIF(lcRpSelcBy = 'R',[RIGHT(cTempKey,3)+CORDTYPE+cmORDER+STR(LINENO,6)]      ,;
            lcIndexTg))))

SELECT (lcWorkFile)
IF RECCOUNT(lcWorkFile) > 0
  *-- We need temp. files to be files not cursor to open it in another alias.
  SELECT (lcWorkFile)
  ZAP
  *-- Rest any relation before data collection.
  SET RELATION TO  
  SELECT (lcWorkFile)
ENDIF
INDEX ON &lcWorkTag TAG (lcWorkFile)

*-- Relation with master order file to help data collecting.
SELECT MASOLIN
SET RELATION TO cOrdType + cmOrder INTO MASOHDR
SET RELATION TO Fabric INTO Fabric ADDITIVE

*-- llWorkDeal : Flag to know that we start dealing with work file.
*-- llLineDeal : Flag to know that we deal with temp. line file.
STORE .F. TO llWorkDeal , llLineDeal

lcTOrdFile = laOGFxFlt[lnOrdPos,6]
llWorkDeal = !EMPTY(lcTOrdFile) AND USED(lcTOrdFile) AND RECCOUNT(lcTOrdFile) > 0
*-- If user select specific orders, collect data of this orders only.
IF llWorkDeal
  SELECT (lcTOrdFile)
  *-- Scan order cursor.
  SCAN  
    SELECT MASOLIN
    SET ORDER TO TAG MASOLIN
    *-- if find first order record in MASOLIN file and <MASOHDR filter>
    *-- Get proper orders not only 'O' type.
    IF IIF(EMPTY(lcRpOrdTyp),SEEK('O'+&lcTOrdFile..CMORDER) OR SEEK('T'+&lcTOrdFile..CMORDER),SEEK(lcRpOrdTyp+&lcTOrdFile..CMORDER)) AND;
       EVALUATE(lcStatusEx) AND ;      
       IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,MASOHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
       IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),MASOHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
       IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,MASOHDR.FabOpen > 0 AND MASOHDR.FabShip > 0),.T.)
      
      *-- Print Partially Shipped Orders (Y/N)

      *-- Scan MASOLIN file for rest order data.
      *-- Get proper orders not only 'O' type.
      SCAN REST WHILE cOrdType+cmOrder+STR(LineNo,6) = cOrdType+&lcTOrdFile..cmOrder
        *-- if <MASOLIN filter> and <purchase group filter> and <Color Filter>
        IF (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
           (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
           IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
           IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
           IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
           IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp)) AND ;
           IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,FABQTY > 0),.T.)
           *-- Include Allocated Order Lines (Y/N)           
           *--In case of print the report by Account , allow 
          SCATTER MEMVAR MEMO
          IF MASOHDR.STATUS = "C"
            *-- Get qty from book qty becauce qty =0
            *B607223,1 ALB Fix error massage when print complete Material Order [Begin]
            *FOR I = 1 To 8
            *  lcCount = ALLTRIM(STR(I))
            *  m.QTY&lcCount = BOOK&lcCount
            *ENDFOR
            *m.FABQTY = TOTBOOK
            m.FABQTY = FABBOOK
            *B607223,1 ALB Fix error massage when print complete Material Order [end]
          ENDIF          
          m.cTempKey  = PADR(fabric.FABRIC,7) + PADR(FABRIC.CPURCODE,6) + PADR(MASOHDR.REP1,3)
          m.cCurrCode = MASOHDR.cCurrCode
          INSERT INTO (lcWorkFile) FROM MEMVAR
        ENDIF  && end if <MASOLIN filter>.
      ENDSCAN  && end Scan MASOLIN file for rest order data.
    ENDIF      && end if find first order record in MASOLIN.
  ENDSCAN      && end Scan order cursor.
  lcMastFile = lcWorkFile
ENDIF

*-- Know which type of select we use and its position
lnUsedItem = IIF(lcRpSelcBy = 'A',lnAccPos,;
             IIF(lcRpSelcBy = 'F',lnFabPos,IIF(lcRpSelcBy = 'W',lnLocPos,;
             IIF(lcRpSelcBy = 'R',lnRepPos,0))))

IF lnUsedItem > 0
  lcSlctFile = laOGFxFlt[lnUsedItem,6]
  llLineDeal = !EMPTY(lcSlctFile) AND USED(lcSlctFile) AND RECCOUNT(lcSlctFile) > 0
ENDIF

llRpStyLoc = (lcRpSelcBy = 'W') AND llLineDeal

*-- If User select data by any select case, beside selecting orders. 
            *-- IMPORT must be good described before add any line in it.

IF RECCOUNT(lcWorkFile) > 0 AND llLineDeal
  lcSlctKey = IIF(lcRpSelcBy = 'A',"ACCOUNT"   ,;
              IIF(lcRpSelcBy = 'W',"CWARECODE" ,;
              IIF(lcRpSelcBy = 'F',"FABRIC"    ,;
              "REPCODE")))  && Field which we seek for in workfile.
  PRIVATE lcScaned
  SELECT (lcSlctFile)
  *-- Scan selected cursor
  SCAN    
    lcScaned = EVALUATE(lcSlctKey)    
    *-- if you find seeking critria in work file. 
    IF SEEK(&lcSlctKey,lcWorkFile)
      SELECT (lcWorkFile)
      *-- scan work file for the rest data have the same seek critria.
      SCAN REST WHILE &lcWorkTag = lcScaned
        *-- if Summarize multi store orders.
        IF llRpSummMt
          lcSeekExp = Fabric + DTOS(Complete) + cordtype + cmorder
          *-- if you do not find this (Fabric + order) in line file, add record for it.
          IF !SEEK(lcSeekExp,lcTempLine)
            SCATTER MEMVAR MEMO
            m.cCurrCode = MASOHDR.cCurrCode
            INSERT INTO (lcTempLine) FROM MEMVAR
          ENDIF        
        ELSE  && normal case, add line to temp. line file.
          SCATTER MEMVAR MEMO
          m.cCurrCode = MASOHDR.cCurrCode
          INSERT INTO (lcTempLine) FROM MEMVAR
        ENDIF  
      ENDSCAN  && end scan work file for the rest data have the same seek critria.
    ENDIF      && end if you find seeking critria in work file.
  ENDSCAN      && end Scan selected cursor.
  lcMastFile = lcTempLine
ELSE  && User either Does not select orders or does not use any select type.
  *-- if User does not select orders but use select type.
  IF llLineDeal
    *-- set files order.
    lcOrdVar = IIF(INLIST(lcRpSelcBy,'S','F') , 'MASOLIN' , 'MASOLIN')	
    SET ORDER TO &lcOrdVar IN MASOLIN    
    *-- if select by account.
    IF lcRpSelcBy = 'A'
      SET ORDER TO ORDACCT IN MASOHDR
    ELSE
      SET ORDER TO MASOHDR IN MASOHDR
    ENDIF

    *-- Different select by cases.
    DO CASE
      CASE lcRpSelcBy = 'A'   && Account case        
        SELECT MASOLIN 
        SET RELATION OFF INTO MASOHDR  && break relation.        
        SELECT (lcSlctFile)
        *-- scan selected cursor.
        SCAN
          *-- if you find this account in MASOHDR file.
          IF SEEK(ACCOUNT,'MASOHDR')          
            SELECT MASOHDR
            *-- scan MASOHDR file rest for this account. 
            SCAN REST WHILE account+cordtype+cmorder = &lcSlctFile..ACCOUNT FOR ;
                           IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,MASOHDR.FabOpen > 0 AND MASOHDR.FabShip > 0),.T.)
              *-- if order type is 'O' , <MASOHDR filter>, and seek for this
              *-- order in order line file.
              *-- Get proper orders not only 'O' type.
              *-- Include Web Orders.
              *-- Seek either by "O" or "T" in MASOLIN file.
              IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
                 EVALUATE(lcStatusEx) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                 IIF(EMPTY(lcRpOrdTyp),SEEK('O'+CMORDER,'MASOLIN') OR SEEK('T'+CMORDER,'MASOLIN'),SEEK(lcRpOrdTyp+CMORDER,'MASOLIN'))

                SELECT MASOLIN 
                *-- scan MASOLIN for rest order lines.
                SCAN REST WHILE cordtype+cmorder+STR(lineno,6) = MASOHDR.CORDTYPE+MASOHDR.CMORDER
                  *-- if <MASOLIN filter> and <Fabric group filter> and <Color Filter>
                  IF (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                     (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                     IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
                     IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
                     IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))   AND ;
                     IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp)) AND ;
                     IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,FABQTY > 0),.T.)
                    SCATTER MEMVAR MEMO
                    IF MASOHDR.STATUS = "C"
                      *-- Get qty from book qty becauce qty =0
                       *B607223,1 ALB Fix error massage when print complete Material Order [Begin]
                       *FOR I = 1 To 8
                       * lcCount = ALLTRIM(STR(I))
                       *  m.QTY&lcCount = BOOK&lcCount
                       *ENDFOR
                       *m.FABQTY = TOTBOOK
                       m.FABQTY = FABBOOK
                       *B607223,1 ALB Fix error massage when print complete Material Order [end]
                    ENDIF                    
                    m.cTempKey = PADR(FABRIC.FABRIC,7) + ;
                                 PADR(FABRIC.CPURCODE,6) + PADR(MASOHDR.REP1,3)
                    m.cCurrCode = MASOHDR.cCurrCode

                    INSERT INTO (lcWorkFile) FROM MEMVAR
                  ENDIF
                ENDSCAN  && end scan MASOLIN for rest order lines.
              ENDIF      && end if order type is 'O' , <MASOHDR filter>.
            ENDSCAN      && end scan MASOHDR file rest for this account.
          ENDIF          && end if you find this account in MASOHDR file.
        ENDSCAN  
 
        *-- Set relation again.
        SET ORDER TO MASOHDR IN MASOHDR
        SELECT MASOLIN 
        SET RELATION TO cOrdType + cmOrder INTO MASOHDR

      CASE lcRpSelcBy = 'F'   && Fabric case
        
        SELECT MASOLIN
        SET RELATION OFF INTO STYLE  && break relation.

        SELECT (lcSlctFile)
        SCAN
          *-- in this case you can not rushmore data, there is no index in master files. 
          SELECT FABRIC
          SET FILTER TO FABRIC = &lcSlctFile..FABRIC
          SCAN 
            IF IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
               IIF(EMPTY(lcSeaExp),.T.,  EVALUATE(lcSeaExp)) AND ;
               IIF(EMPTY(lcDivExp),.T.,  EVALUATE(lcDivExp)) AND ;
               IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
               
              SELECT MASOLIN
              SCAN REST WHILE fabric+DTOS(complete)+cordtype+cmorder+store+STR(lineno,6) = ;
                              fabric.fabric FOR IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,MASOHDR.FabOpen > 0 AND MASOHDR.FabShip > 0),.T.)
                *-- if <MASOHDR filter> and <MASOLIN filter>
                IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
                   EVALUATE(lcStatusEx) AND ;
                   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,MASOHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),MASOHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                   IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,FABQTY > 0),.T.)
                  SCATTER MEMVAR MEMO
                  m.cTempKey = PADR(FABRIC.FABRIC,7) + PADR(FABRIC.CPURCODE,6) + PADR(MASOHDR.REP1,3)
                  m.cCurrCode = MASOHDR.cCurrCode                  
                  IF MASOHDR.STATUS = "C"
                  *-- Get qty from book qty becauce qty =0
                    *FOR I = 1 To 8
                    *  lcCount = ALLTRIM(STR(I))
                    *  m.QTY&lcCount = BOOK&lcCount
                    *ENDFOR
                    *m.FABQTY = TOTBOOK
                    m.FABQTY = FABBOOK
                  ENDIF
                  INSERT INTO (lcWorkFile) FROM MEMVAR
                ENDIF    && end if <MASOHDR filter> and <MASOLIN filter>  
              ENDSCAN
            ENDIF
          ENDSCAN
        ENDSCAN
        
        *-- Refilter fabric file.
        SELECT fabric
        SET FILTER TO

      CASE lcRpSelcBy = 'W'   && Location case
        SELECT MASOLIN 
        SET RELATION OFF INTO MASOHDR  && break relation.
        SELECT (lcSlctFile)
        SCAN
          SELECT MASOHDR
          *-- filter to cwarecode and <MASOHDR filter>
          SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
                        IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND  ;
                        EVALUATE(lcStatusEx) AND ;
                        IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,MASOHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),MASOHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                        IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,MASOHDR.FabOpen > 0 AND MASOHDR.FabShip > 0),.T.)
          *-- scan MASOHDR for this Location.
          SCAN
            *= SEEK('O'+CMORDER,'MASOLIN')  && seek MASOLIN for MASOHDR order.
            = SEEK(IIF(EMPTY(lcRpOrdTyp),'O',lcRpOrdTyp)+CMORDER,'MASOLIN')  && seek MASOLIN for MASOHDR order.
            IF EOF('MASOLIN') AND EMPTY(lcRpOrdTyp)
              = SEEK('T'+CMORDER,'MASOLIN')  && seek MASOLIN for MASOHDR order.
            ENDIF
            SELECT MASOLIN
            *-- Scan MASOLIN file to get lines of this order
            *-- that evaluate critria.
            SCAN REST WHILE CORDTYPE+CMORDER+STR(LINENO,6) = cOrdType + MASOHDR.CMORDER
            
              *-- if <MASOLIN filter> and <purchase group filter> and <Color Filter>              
              IF (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
                 IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
                 IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
                 IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,FABQTY > 0),.T.)
                SCATTER MEMVAR MEMO
                m.cTempKey = PADR(FABRIC.FABRIC,7) + PADR(FABRIC.CPURCODE,6) + PADR(MASOHDR.REP1,3)
                m.cCurrCode = MASOHDR.cCurrCode
                IF MASOHDR.STATUS = "C"
                  *-- Get qty from book qty becauce qty =0
                  *FOR I = 1 To 8
                  *  lcCount = ALLTRIM(STR(I))
                  *  m.QTY&lcCount = BOOK&lcCount
                  *ENDFOR
                  *m.FABQTY = TOTBOOK
                  m.FABQTY = FABBOOK
                ENDIF
                INSERT INTO (lcWorkFile) FROM MEMVAR
              ENDIF    && end if <MASOLIN filter>
            ENDSCAN
          ENDSCAN      && end scan MASOHDR for this sales rep.
        ENDSCAN  

        *-- Refilter MASOHDR file
        SELECT MASOHDR
        SET FILTER TO
        SELECT MASOLIN 
        SET RELATION TO cOrdType + cmOrder INTO MASOHDR

      CASE lcRpSelcBy = 'R'   && Sales rep. case
        SELECT MASOLIN 
        SET RELATION OFF INTO MASOHDR  && break relation.
        SELECT (lcSlctFile)
        SCAN
          SELECT MASOHDR
          *-- filter to repcode and <MASOHDR filter>
          SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
                        IIF(EMPTY(lcRpOrdTyp),CORDTYPE <> 'C',CORDTYPE = lcRpOrdTyp) AND ;
                        EVALUATE(lcStatusEx) AND ;
                        (EMPTY(laOGFxFlt[lnPriPos,6]) OR MASOHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),MASOHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                        IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,MASOHDR.FabOpen > 0 AND MASOHDR.FabShip > 0),.T.)
          *-- scan MASOHDR for this sales rep.
          SCAN
            = SEEK(IIF(EMPTY(lcRpOrdTyp),'O',lcRpOrdTyp)+CMORDER,'MASOLIN')  && seek MASOLIN for MASOHDR order.
            IF EOF('MASOLIN') AND EMPTY(lcRpOrdTyp)
              = SEEK('T'+CMORDER,'MASOLIN')  && seek MASOLIN for MASOHDR order.
            ENDIF
            SELECT MASOLIN
            *-- Scan MASOLIN file to get lines of this order
            *-- that evaluate critria.
            SCAN REST WHILE CORDTYPE+CMORDER+STR(LINENO,6) = cOrdType + MASOHDR.CMORDER
              IF (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 (EMPTY(lcSeaExp) OR EVALUATE(lcSeaExp)) AND ;
                 (EMPTY(lcDivExp) OR EVALUATE(lcDivExp)) AND ;
                 IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
                 IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,FABQTY > 0),.T.)
                SCATTER MEMVAR MEMO
                m.cTempKey = PADR(FABRIC.FABRIC,7) + PADR(FABRIC.CPURCODE,6) + PADR(MASOHDR.REP1,3)
                m.cCurrCode = MASOHDR.cCurrCode

                IF MASOHDR.STATUS = "C"
                *-- Get qty from book qty becauce qty =0
                  *FOR I = 1 To 8
                  *  lcCount = ALLTRIM(STR(I))
                  *  m.QTY&lcCount = BOOK&lcCount
                  *ENDFOR
                  *m.FABQTY = TOTBOOK
                  m.FABQTY = FABBOOK
                ENDIF
                INSERT INTO (lcWorkFile) FROM MEMVAR
              ENDIF    && end if <MASOLIN filter>
            ENDSCAN
          ENDSCAN      && end scan MASOHDR for this sales rep.
        ENDSCAN  

        *-- Refilter MASOHDR file 
        SELECT MASOHDR
        SET FILTER TO
        SELECT MASOLIN 
        SET RELATION TO cOrdType + cmOrder INTO MASOHDR
    ENDCASE
    lcMastFile = lcWorkFile
  ELSE  && user does not use any select type.
    *-- if user does not select any orders [no data found],
    *-- in this case we select all file.
    IF (RECCOUNT(lcWorkFile) = 0) AND !llWorkDeal
      SELECT MASOLIN
      SET ORDER TO    && To activate rushmore.
      * IMPORT MUST BE GOOD DESCRIPED BEFORE ADD ANY LINE OF CODE.
      *-- if summarize multi store orders.
      IF llRpSummMt
        USE (gcDataDir+'MASOLIN') AGAIN ALIAS SUMMULTI ORDER TAG MASOLIN IN 0
        
        m.cTempKey = 0  && initially define it to have total amount.

        SELECT MASOLIN
        *-- scan MASOLIN file for full index expression (rushmore)
        SCAN FOR fabric+DTOS(complete)+cordtype+cmorder+store+STR(lineno,6) = ''
          lcSeekExp = fabric + DTOS(Complete) + cordtype + cmorder
          
          *-- if you does not find line in temp line file,;
          *-- and order type is 'O', <MASOHDR filter>, <MASOLIN filter> ;
          *-- and <Fabric group filter> and <Color Filter>
          IF !SEEK(lcSeekExp,lcTempLine) AND IIF(EMPTY(lcRpOrdTyp),CORDTYPE <> 'C',CORDTYPE= lcRpOrdTyp) AND ;
             EVALUATE(lcStatusEx) AND ;
             (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
             (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
             IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
             IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
             IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,MASOHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
             IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),MASOHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
             IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
             IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
             IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,MASOHDR.FabOpen > 0 AND MASOHDR.FabShip > 0),.T.) AND ;
             IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,FABQTY > 0),.T.)
            SCATTER MEMVAR MEMO
            m.cCurrCode = MASOHDR.cCurrCode

            IF MASOHDR.STATUS = "C"
            *-- Get qty from book qty becauce qty =0
              *FOR I = 1 To 8
              *  lcCount = ALLTRIM(STR(I))
              *  m.QTY&lcCount = BOOK&lcCount
              *ENDFOR
              *m.FABQTY = TOTBOOK
              m.FABQTY = FABBOOK
            ENDIF
            INSERT INTO (lcTempLine) FROM MEMVAR  
          ENDIF    && end if you does not find line in temp line file,
        ENDSCAN    && end scan file for full index expression (rushmore).
        USE IN SUMMULTI
      ELSE  && Normal collection case for all data in MASOLIN file.
        *-- scan MASOLIN file for full index expression (rushmore)
        SCAN FOR CORDTYPE + CMORDER + STR(LINENO,6) = lcRpOrdTyp
          *-- if <MASOHDR filter>, <MASOLIN filter> and <Fabric group filter> 
          *-- and <Color Filter>.          
          IF EVALUATE(lcStatusEx) AND ;
             (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
             (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
             IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
             IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
             IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,MASOHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
             IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),MASOHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
             IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
             IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
             IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,MASOHDR.FabOpen > 0 AND MASOHDR.FabShip > 0),.T.) AND ;
             IIF(llRpPrtShp,IIF(MASOHDR.Status $ 'BCX',.F.,FABQTY > 0),.T.)
            SCATTER MEMVAR MEMO
            m.cTempKey = PADR(FABRIC.FABRIC,7)+PADR(FABRIC.CPURCODE,6)+PADR(MASOHDR.REP1,3)
            m.cCurrCode = MASOHDR.cCurrCode

            IF MASOHDR.STATUS = "C"
              *-- Get qty from book qty becauce qty =0
              *FOR I = 1 To 8
              *  lcCount = ALLTRIM(STR(I))
              *  m.QTY&lcCount = BOOK&lcCount
              *ENDFOR
              *m.FABQTY = TOTBOOK
              m.FABQTY = FABBOOK
            ENDIF
            INSERT INTO (lcTempLine) FROM MEMVAR          
          ENDIF    && end if <MASOHDR filter>, <MASOLIN filter> and <Fabric group filter>        
        ENDSCAN    && end scan file for full index expression (rushmore).
      ENDIF        && end if summarize multi store orders.
      SELECT MASOLIN
      SET ORDER TO MASOLIN IN MASOLIN
      lcMastFile = lcTempLine
    ENDIF          && end if user does not select any orders [no data found],
  ENDIF            && end if User does not select orders but use select type.
ENDIF  && end If User select data by any select case, beside selecting orders. 

*-- if user select by orders only, and want to summarize data.
IF (RECCOUNT(lcTempLine) = 0 AND RECCOUNT(lcWorkFile) > 0) AND llRpSummMt
  SELECT(lcWorkFile)
  SCAN
    lcSeekExp = fabric + DTOS(Complete) + cordtype + cmorder
    IF !SEEK(lcSeekExp,lcTempLine)
      SCATTER MEMVAR MEMO
      m.cCurrCode = MASOHDR.cCurrCode
      INSERT INTO (lcTempLine) FROM MEMVAR
    ENDIF  
  ENDSCAN
  lcMastFile = lcTempLine
ENDIF

*-- if report master file is the work file and index 
*-- does not match sort by case, reindex data. 
IF (lcMastFile = lcWorkFile) AND (lcWorkTag != lcIndexTg)
  SELECT (lcWorkFile)
  INDEX ON &lcIndexTg TAG (lcWorkFile)
ENDIF
*-- end of lfScanData.

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOptMsg()
*!*************************************************************
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[1,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[1,2] = 75                && Line length
= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- end of lfvOptMsg.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
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
*! Name      : lfvSelcBy
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Validate select by option in option grid.
*!           : [Simply it enable and disable selecting buttons]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfSelcObjs
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : In this function we want to know old value to disable
*!           : last object, and we transfer it to its corressponding 
*!           : character because option grid returns its item number in popup. 
*!*************************************************************
*! Example   : =lfvSelcBy()
*!*************************************************************
FUNCTION lfvSelcBy

lcDummy = "Y"
llChSelect = .T.

llClearAcc = (lcRpSelcBy # 'A')
llClearFab = (lcRpSelcBy # 'F')
llClearLoc = (lcRpSelcBy # 'L')
llClearRep = (lcRpSelcBy # 'R')
CLEAR READ

*!*************************************************************
*! Name      : lfSelcObjs
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Enable and disable selected objects.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSelcObjs()
*!*************************************************************
FUNCTION lfSelcObjs
PARAMETERS lnObjNum,lcObjState,llClearVal
IF llClearVal AND (lcObjState = 'D' AND !EMPTY(laOGFxFlt[lnObjNum,6]))
  laOGFxFlt[lnObjNum,6] = ''
ENDIF  
laOGObjCnt[lnObjNum + lnVarbEnd] = (lcObjState = 'E')
= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnObjNum)) + ',6]')  && Enable / Disable Object .
*-- end of lfSelcObjs.

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : 1- Enable and disable some variavle objects due to sort case
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState,lfPreObjs
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : 1- In this function we want to know old value to enable/disable
*!           :    objects due to some sort cases, and we transfer it to 
*!           :    its corressponding character because option grid returns
*!           :    its item number in popup, the idea of enable/disable in
*!           :    this function is to full control printing and do not enable
*!           :    enabled button or disable disabled button.
*!           : 2- In some cases we rise summarization flag to Recollect data again.
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
laOldVal = IIF(laOldVal = 1 , 'A' , IIF(laOldVal = 2 , 'O',;
           IIF(laOldVal = 3 , 'S' , IIF(laOldVal = 4 , 'G',;
           IIF(laOldVal = 5 , 'F' , IIF(laOldVal = 6 , 'W',;
           IIF(laOldVal = 7 , 'R' , 'D')))))))

*-- Add sort by store if first sort is by order.
IF lcRpKind = 'D'
  IF lcRpSortBy = "O"
    DIMENSION laSort2Des[4,1] , laSort2Val[4,1]
    laSort2Des[3,1] = "Store/Line#"
    laSort2Des[4,1] = "Store/Fabric"
    laSort2Val[3,1] = "T"
    laSort2Val[4,1] = "Y"

  ELSE  && if sort by any thing rather than order.

    *-- last sort is by order.
    IF laOldVal = "O"
      DIMENSION laSort2Des[2,1] , laSort2Val[2,1]
      lcRpSrt2 = 'L'
    ENDIF

  ENDIF
ENDIF
llRpSummMt = IIF(lcRpSortBy <> 'S' ,.F.,llRpSummMt)
llRpOrdNot = IIF(!(lcRpSortBy $ 'AO'),.F.,llRpOrdNot)
CLEAR READ

*-- if In case of sort by (FABRIC OR PURCHASE GROUP) and old value is
*-- another sort type and user want to print sizes, we must disable
*-- reprint scale when diff. because it is the normal print case here,
*-- and vice versa.

IF lcRpSortBy != laOldVal

  *-- Different sort by cases.
  DO CASE
    CASE lcRpSortBy = 'A'   && Sort by account 

      *-- Enable/disable variable objects due to sort case.
      *-- if report kind is detail
      IF lcRpKind = 'D'
        IF laOldVal = 'S'
          llChSumm = IIF(llRpSummMt,.T.,llChSumm)  && Rise summarize flag.
        ENDIF
      ENDIF
      
    CASE lcRpSortBy = 'O'    && Sort by order
      *-- Enable/disable variable objects due to sort case.
      IF lcRpKind = 'D'
        IF laOldVal = 'S'
          llChSumm = IIF(llRpSummMt,.T.,llChSumm)
        ENDIF
      ENDIF
      *-- Enable/disable variable objects due to sort case. 


    CASE lcRpSortBy = 'G'    && Sort by Fabric group
      *-- Enable/disable variable objects due to sort case. 
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() && Prepair objects.
      ENDIF  
      *-- Enable/disable variable objects due to sort case. 

    CASE lcRpSortBy = 'F'    && Sort by fabric

      *-- Enable/disable variable objects due to sort case.
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
      *-- Enable/disable variable objects due to sort case.

    CASE lcRpSortBy = 'W'    && Sort by location

      *-- Enable/disable variable objects due to sort case.
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
      *-- Enable/disable variable objects due to sort case.

    CASE lcRpSortBy = 'R'    && Sort by sales representative

      *-- Enable/disable variable objects due to sort case.
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
      *-- Enable/disable variable objects due to sort case.

    CASE lcRpSortBy = 'D'    && Sort by complete date

      *-- Enable/disable variable objects due to sort case.
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
      *-- Enable/disable variable objects due to sort case.

  ENDCASE          && end Different sort by cases.
ENDIF
*-- end of lfvSortBy.

*!*************************************************************
*! Name      : lfPreObjs
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Enable/Disable controled objects in 4 sort cases
*!           : - Fabric group, Fabric, Sales Rep., Complete date
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPreObjs()
*!*************************************************************
FUNCTION lfPreObjs

IF lcRpKind = 'D'
  IF laOldVal = 'S'
    llChSumm = IIF(llRpSummMt,.T.,llChSumm)
  ENDIF
ENDIF
*-- end of lfPreObjs.

*!*************************************************************
*! Name      : lfObjState
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : This function used to calculate object number and call 
*!           : global show function to enable/disable object due to passed state.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!*************************************************************
*! Passed Parameters  : 1- ('E' -> enable,'D' disable)
*!                    : 2- Object number
*!                    : 3- Object variable
*!                    : 2- Object value
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfObjState()
*!*************************************************************
FUNCTION lfObjState
PARAMETERS lcObjState,lnObjNum,lcObjVar,laObjVal
IF lnObjNum != 0
  *-- If you disable object you must restore its initial value.
  IF lcObjState = 'D'
    &lcObjVar = laObjVal
  ENDIF  

  laOGObjCnt[lnObjNum] = (lcObjState = 'E')    && Enable if 'E'
  laRpVarNow[lnObjNum] = laOGObjCnt[lnObjNum]  && Save Variable value.

  = lfOGShowGet(lcObjVar)  && Called to show object get.
ENDIF  
*-- end of lfObjState.

*!*************************************************************
*! Name      : lfvKind
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Enable/disable some objects due to report kind (Detail/Summary)
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvKind()
*!*************************************************************
FUNCTION lfvKind

IF lcRpKind = 'S'
  STORE .F. TO llRpSummMt,llRpOrdLnt,llRpOrdNot
  lcRpSrt2 = "L"
ENDIF
CLEAR READ

*!*************************************************************
*! Name      : lfvSumm
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Enable/disable Order line notes object
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSumm()
*!*************************************************************
FUNCTION lfvSumm
llRpOrdLnt = IIF(llRpSummMt ,.F.,llRpOrdLnt)
llChSumm   = .T.
CLEAR READ
*-- end of lfvSumm.

*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR STYLE.Fabric = Fabric.Fabric
    SELECT (lcAlias)
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')

  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Rise change account flag, in range browse screen.
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
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfsrLoc
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Rise change Location flag, in range browse screen.
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
*! Example   : =lfsrLoc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrLoc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChLoc = .T.
    GO TOP IN WAREHOUS
  CASE lcParm = 'R'
    llClearLoc = .F.
ENDCASE
*-- end of lfsrLoc.

*!*************************************************************
*! Name      : lfsrRep
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Rise change sales rep. flag, in range browse screen.
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
*! Example   : =lfsrRep()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************

FUNCTION lfsrRep
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChRep = .T.
    GO TOP IN SALESREP
  CASE lcParm = 'R'
    llClearRep = .F.
ENDCASE
*-- end of lfsrRep.

*!*************************************************************
*! Name      : lfSROrder
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
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
*! Example   : =lfSROrder()
*!*************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm
llChOrder = .T.
*-- Set Relation with customer file.
DO CASE
  CASE lcParm = 'S'
    SELECT MASOHDR
    lcCustRel = IIF(llRpSummMt,['M' + Account],;
                [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
    GO TOP
  
  CASE lcParm = 'R'
    SELECT MASOHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClearOrd = .F.
ENDCASE
*-- end of lfsChOrder.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
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
*! Name        : lfGetNotes
*! Developer   : Rania Abdel Razik (RAE)
*! Date        : 05/27/1998
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : SOMADET.FRX,[Variable lcDum in the report]
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes
lcTitle = ''     && Title of the note (Line Note OR NotePad).
lcNotes = ''     && Note Data.
*-- if you print both notes.
IF llPrntBoth
  *-- Note that the following Scheme
  *-- ....... cRecord = 'N1' ............. Line Notepad.
  *-- ....... cRecord = 'N2' ............. Order Notepad.
  DO CASE 
    CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcMastFile..Note_Mem))
      lcTitle = 'Order : ' + CMORDER + ' - Fabric : ' + Fabric + ' - Line #  ' + ALLTRIM(STR(LINENO)) + '    Notes.'
      lcNotes  =  ALLTRIM(Note_Mem) + CHR(10)
    CASE &lcNoteLns..cRecord = 'N2' AND SEEK('V' + IIF(MASOHDR.cOrdType = 'T','T','')+CMOrder , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))    
      lcTitle = IIF(RECNO(lcMastFile) = lnLastRec,;
                'Order :' + CMORDER + ' Notepad.','')
      lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10) ,'')
  ENDCASE
ELSE && Else You print either Line or Order/contract Notepad.
  *-- Note that the following Scheme
  *-- ....... llRoOrdLnt ............. Line Notepad.
  *-- ....... llRoOrdNot ............. Order or Contract Notepad.
  DO CASE
    CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcMastFile..Note_Mem))
      lcTitle = 'Order : ' + CMORDER + ' - Fabric : ' + Fabric + ' - Line #  ' + ALLTRIM(STR(LINENO)) + '    Notes.'
      lcNotes  =  ALLTRIM(Note_Mem) + CHR(10)
    CASE llRpOrdNot AND SEEK('V' + IIF(MASOHDR.cOrdType = 'T','T','')+CMOrder , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcTitle = IIF(RECNO(lcMastFile) = lnLastRec,;
                'Order :' + CMORDER + ' Notepad.','')
      lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10),'')
  ENDCASE
ENDIF
RETURN ''
*-- end of lfGetNotes.

*!*************************************************************
*! Name      : lfLastRec
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Calculate last Record in order details. [ORDER GROUP]
*!           : we use another alias to unchange record pointer of report file.
*!*************************************************************
*! Called from : [SOMADET.FRX ,ORDER GROUP HEADER BAND] 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : NULL
*!*************************************************************
*! Example     : = lfLastRec()
*!*************************************************************
FUNCTION lfLastRec
IF llRpOrdNot
  PRIVATE lcThAlias , lcCurrOrd , lcToSeekVl
  lcThAlias = ALIAS()           && Save Current Alias.
  lcCurrOrd  = ORDER()
  lcToSeekVl = EVALUATE(lcSeekVal)
  SELECT GETLAST
  SET ORDER TO (lcCurrOrd) DESCENDING
  =SEEK(lcToSeekVl)
  lnLastRec = RECNO('GETLAST')  && Evaluate record Number of last record in detail lines.
  SET ORDER TO (lcCurrOrd) ASCENDING
  SELECT (lcThAlias)             && Restore Alias.
ENDIF
RETURN ''
*-- end of lfLastRec.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
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
llClearFn = .T.
*-- Close temp. opended files, if it used.

*-- Delete temporary line file.
IF USED('GETLAST')
  USE IN GETLAST
ENDIF

IF USED(lcTempLine)
  USE IN (lcTempLine)

  IF FILE(gcWorkDir+lcTempLine+'.DBF') 
    ERASE(gcWorkDir+lcTempLine+'.DBF')
  ENDIF

  IF FILE(gcWorkDir+lcTempLine+'.CDX') 
    ERASE(gcWorkDir+lcTempLine+'.CDX')
  ENDIF

  IF FILE(gcWorkDir+lcTempLine+'.FPT') 
    ERASE(gcWorkDir+lcTempLine+'.FPT')
  ENDIF
ENDIF

*-- Delete temporary work file.
IF USED(lcWorkFile)
  USE IN (lcWorkFile)

  IF FILE(gcWorkDir+lcWorkFile+'.DBF') 
    ERASE(gcWorkDir+lcWorkFile+'.DBF')
  ENDIF

  IF FILE(gcWorkDir+lcWorkFile+'.CDX') 
    ERASE(gcWorkDir+lcWorkFile+'.CDX')
  ENDIF

  IF FILE(gcWorkDir+lcWorkFile+'.FPT') 
    ERASE(gcWorkDir+lcWorkFile+'.FPT')
  ENDIF
  
ENDIF

IF USED(lcNoteLns) 
  USE IN (lcNoteLns)
ENDIF

*-- if user change setting [enter report <Preview> or <Run>]
IF !llFrTime
  SET HOURS TO &lcSetHour
ENDIF  && end if user change setting [enter report <Preview> or <Run>].
IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
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
*-- Compute Free/Color Items in Fabric Structure.
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
=gfItemMask(@laMajSegs)

*-- Adjust currency symbol.
*-- if multi currency evaluate currency arrays 
IF llMultCurr
  DIMENSION laCurrVal[1,1]

  IF !USED('SYCCURR')
    =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1],laCurrSmbl[ALEN(laCurrVal,1),1]

  SELECT SYCCURR
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrSmbl[lnI,1] = ALLTRIM(PADR(CCURRSMBL,3))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF
*-- if multi currency evaluate currency arrays 

RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfGetRepVr
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : 1- Put both index and group expressions for all sort cases.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Notes     : 1- lcIndexTg : is master report file index due to sort case.
*!*************************************************************
*! Example   : = lfGetRepVr()
*!*************************************************************
FUNCTION lfGetRepVr

*-- in Case of ACCOUNT,ORDER,LOCATION,SALESREP another Sort by (Sort# 2)
*-- appear in option grid (Sort by Line#/Fabric)           
STORE [''] TO lcOutHeadL,lcOutHeadR,lcInnHeadL,lcInnHeadR
lcLineCurr = lfCurrPrnt()

*-- Different sort by cases.
DO CASE
  CASE lcRpSortBy = 'A'   && Sort by account 
    *-- lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Fabric.
    IF lcRpSrt2 = 'L'    && Sort by Line#
      IF llCurInGrp
        lcIndexTg = 'ACCOUNT+CCURRCODE+CORDTYPE+cmORDER+STR(LINENO,6)+Fabric'
      ELSE
        lcIndexTg = 'ACCOUNT+CORDTYPE+cmORDER+STR(LINENO,6)+Fabric'
      ENDIF  
	ELSE				  && Else Sort by Fabric	
      IF llCurInGrp
        lcIndexTg = 'ACCOUNT+CCURRCODE+CORDTYPE+CMORDER+Fabric+STR(LINENO,6)'
      ELSE
        lcIndexTg = 'ACCOUNT+CORDTYPE+cmORDER+Fabric+STR(LINENO,6)'
      ENDIF  
	ENDIF
          
    *-- report variables data account case 
    lcSubTitle = 'Account Number'  && Report sort title
    lcInnGrp   = [CMORDER]           && Inner .FRX group field.

    *-- changing these lines so as in layout, Customer PO# 
    *-- field need to be closer to its title "Cust PO #".
    IF llTextMode
      lcInnFootL = [CMORDER + ', Cust PO#'+] +;
                   [IIF(MASOHDR.MultiPo,'*Multi PO*',PADL(MASOHDR.CustPo,10)) + ' Enter in ' + ALLTRIM(DTOC(MASOHDR.Entered))]

      lcOutFootL = [Customer.Account+' '+PADR(ALLTRIM(CUSTOMER.BtName),15)+"/"+EVALUATE(lcLineCurr)+', ' + TRANSFORM(CUSTOMER.Phone1,lcPhonPict)]
    ELSE
      lcInnFootL = ['Order # ' + CMORDER + '     Cust PO #']    
      lcInnFootR = [IIF(MASOHDR.MultiPo,'*Multi PO*', PADL(MASOHDR.CustPo,15)) + ' Entered in: ' + ALLTRIM(DTOC(MASOHDR.Entered))]  && Right title of inner group footer.
    
      lcOutFootL = ['Account # ' + Customer.Account]  && Left title of outer group footer.
      lcOutFootR = [PADR(ALLTRIM(CUSTOMER.BtName),25) +"/"+EVALUATE(lcLineCurr)+ ", " + TRANSFORM(CUSTOMER.Phone1,lcPhonPict)]  && Right title of outer group footer.
    ENDIF
    IF llCurInGrp
      lcSeekVal  = [ACCOUNT+cCurrCode+cOrdType+cmORDER]

      lcOutGrp   = [ACCOUNT+CCURRCODE]         && Outer .FRX group field.
    ELSE
      lcSeekVal  =  [ACCOUNT+cOrdType+cmORDER]
      lcOutGrp   = [ACCOUNT]         && Outer .FRX group field.
    ENDIF

  CASE lcRpSortBy = 'O'    && Sort by order
    lcSubTitle = 'Order'
    lcInnGrp   = ['']
    lcInnFootL = [''] 

    *-- lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Fabric.
    DO CASE
      CASE lcRpSrt2 = 'L'    && Sort by Line#
        IF llCurInGrp
          lcIndexTg = 'CCURRCODE+CORDTYPE+cmORDER+STR(LINENO,6)+Fabric'
        ELSE
          lcIndexTg = 'CORDTYPE+cmORDER+STR(LINENO,6)+Fabric'
        ENDIF
      CASE lcRpSrt2 = 'S'    && Sort by Fabric
        IF llCurInGrp
          lcIndexTg = 'CCURRCODE+CORDTYPE+cmORDER+Fabric+STR(LINENO,6)'
        ELSE
          lcIndexTg = 'CORDTYPE+cmORDER+Fabric+STR(LINENO,6)'
        ENDIF  

      CASE lcRpSrt2 $ 'TY'    
        
        IF llCurInGrp
          lcIndexTg = 'CCURRCODE+CORDTYPE+cmORDER+STORE'
        ELSE
          lcIndexTg = 'CORDTYPE+cmORDER+STORE'
        ENDIF
        
        *-- Sort by Store/Line#
        IF lcRpSrt2="T"
          lcIndexTg = lcIndexTg + '+STR(LINENO,6)+Fabric'
        ELSE  && Sort by Store/Fabric
          lcIndexTg = lcIndexTg + '+Fabric+STR(LINENO,6)'
        ENDIF  
        
        lcStorCond = IIF(llTextMode,[.T.],[MASOHDR.MULTI="Y"])
        lcInnGrp   = [IIF(EVALUATE(lcStorCond),STORE,"")]
        lcInnFootL = [IIF(EVALUATE(lcStorCond),"Store : "+STORE,"")] 

    ENDCASE
    *-- report variables data order case 
    IF llTextMode
      IF lcRpSrt2 $ 'TY'    
        lcOutFootL = [CMORDER+ '/' + EVALUATE(lcLineCurr)+] +;
                     [IIF(MASOHDR.MULTI="Y",',Acct#' + Account,',Stor#' + Store) + ',Entered :' + ALLTRIM(DTOC(MASOHDR.Entered))]
      ELSE
        lcOutFootL = [CMORDER+ '/' + EVALUATE(lcLineCurr)+] +;
                     [',Acct#' + Account + ',Entered in:' + ALLTRIM(DTOC(MASOHDR.Entered))]
      ENDIF    
    ELSE
      lcInnFootR = ['']
      IF lcRpSrt2 $ 'TY'    
        lcOutFootL = [EVALUATE(lcLineCurr)+ "/"+'Order # ' + CMOrder + IIF(EVALUATE(lcStorCond),' Account:',' Store :')]
        lcOutFootR = [IIF(EVALUATE(lcStorCond),Account,Store) + ' Entered in: ' + ALLTRIM(DTOC(MASOHDR.Entered))]
      ELSE
        lcOutFootL = [EVALUATE(lcLineCurr)+ "/"+'Order # ' + CMOrder + '    Account # ']
        lcOutFootR = [Account  + ' Entered in: ' + ALLTRIM(DTOC(MASOHDR.Entered))]
      ENDIF
    ENDIF  
    IF llCurInGrp
      lcSeekVal = [cCurrCode+cOrdType+cmORDER]
      lcOutGrp  = [CCURRCODE+CMORDER]
    ELSE
      lcSeekVal = [cOrdType+cmORDER]
      lcOutGrp  = [CMORDER]
    ENDIF

  CASE lcRpSortBy = 'G'    && Sort by purchase group
    IF llCurInGrp
      lcIndexTg = 'SUBSTR(cTempKey,8,6)+CCURRCODE+Fabric+CORDTYPE+cmORDER+STR(LINENO,6)'
      lcSeekVal = [SUBSTR(cTempKey,8,6)+CCURRCODE+Fabric]
      lcOutGrp  = [SUBSTR(cTempKey,8,6)+CCURRCODE]
    ELSE
      lcIndexTg = 'SUBSTR(cTempKey,8,6)+Fabric+CORDTYPE+cmORDER+STR(LINENO,6)'
      lcSeekVal = [SUBSTR(cTempKey,8,6)+Fabric]
      lcOutGrp  = [SUBSTR(cTempKey,8,6)]
    ENDIF  

    *-- report variables data Fabric group case 
    lcSubTitle = 'Fabric Group'
    lcInnGrp   = [FABRIC]

    IF llTextMode
      lcInnFootL = ['Fabric (' + Fabric + ') :'+ALLTRIM(Fabric.Desc)]
      lcOutFootL = [gfCodDes(SUBSTR(cTempKey,8,6),'CPURCODE',.T.)+"/"+EVALUATE(lcLineCurr)]
    ELSE
      lcInnFootL = ['Fabric  ( ' + Fabric + ' )  :']
      lcInnFootR = [ALLTRIM(Fabric.Desc)]
      lcOutFootL = ['Group  :']
      lcOutFootR = [gfCodDes(SUBSTR(cTempKey,8,6),'CPURCODE',.T.)+"/"+EVALUATE(lcLineCurr)]
    ENDIF

  CASE lcRpSortBy = 'F'    && Sort by fabric
    IF llCurInGrp
      lcIndexTg = 'LEFT(cTempKey,7)+CCURRCODE+Fabric+CORDTYPE+cmORDER+STR(LINENO,6)'
      lcSeekVal = [LEFT(cTempKey,7)+CCURRCODE+Fabric]
      lcOutGrp  = [LEFT(cTempKey,7)+CCURRCODE]
    ELSE
      lcIndexTg = 'LEFT(cTempKey,7)+Fabric+CORDTYPE+cmORDER+STR(LINENO,6)'
      lcSeekVal = [LEFT(cTempKey,7)+Fabric]
      lcOutGrp  = [LEFT(cTempKey,7)]
    ENDIF  

    *-- report variables data fabric case 
    lcSubTitle = 'Primary Fabric'
    lcInnGrp   = [FABRIC]

    *-- Define report variables in all sort cases in both text and graphic
    *-- format keep in mind multi currency situation. 
    IF llTextMode
      lcInnFootL = ['Fabric (' + Fabric + ') :'+ALLTRIM(Fabric.Desc)]
      lcOutFootL = [LEFT(cTempKey,7) + ':'+ALLTRIM(Fabric.Desc)+"/"+EVALUATE(lcLineCurr)]
    ELSE
      lcInnFootL = ['Fabric  ( ' + Fabric + ' )  :']
      lcInnFootR = [ALLTRIM(Fabric.Desc)]
      lcOutFootL = ['Fabric  ( ' + LEFT(cTempKey,7) + ' )  :']
      lcOutFootR = [ALLTRIM(Fabric.Desc)+"/"+EVALUATE(lcLineCurr)]
    ENDIF

  CASE lcRpSortBy = 'W'    && Sort by location
    *-- lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Fabric.
    IF lcRpSrt2 = 'L'       && Sort by Line#
      IF llCurInGrp
        lcIndexTg = 'CWARECODE+CCURRCODE+Fabric+CORDTYPE+cmORDER+STR(LINENO,6)'
      ELSE
        lcIndexTg = 'CWARECODE+Fabric+CORDTYPE+cmORDER+STR(LINENO,6)'
      ENDIF  
	ELSE                     && Else Sort by Fabric	
      IF llCurInGrp
        lcIndexTg = 'CWARECODE+CCURRCODE+CORDTYPE+cmORDER+STR(LINENO,6)+Fabric'	
      ELSE
        lcIndexTg = 'CWARECODE+CORDTYPE+cmORDER+STR(LINENO,6)+Fabric'	
      ENDIF  
	ENDIF

    IF llCurInGrp
      lcOutGrp   = [CWARECODE+CCURRCODE]
    ELSE
      lcOutGrp   = [CWARECODE]
    ENDIF
	    
    *-- report variables data location case 
    lcSubTitle = 'Location'
    lcInnGrp   = ['']
    lcInnFootL = [''] 
    IF llTextMode
      lcOutFootL = [cWareCode + ':'+ALLTRIM(Warehous.cDesc)+"/"+EVALUATE(lcLineCurr)]
    ELSE
      lcInnFootR = ['']
      lcOutFootL = ['Location # ' + cWareCode + ' :']
      lcOutFootR = [ALLTRIM(Warehous.cDesc)+"/"+EVALUATE(lcLineCurr)]
    ENDIF

  CASE lcRpSortBy = 'R'    && Sort by sales representative
    *-- lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Fabric.
    IF lcRpSrt2 = 'L'       && Sort by Line#
      IF llCurInGrp
	    lcIndexTg = 'RIGHT(cTempKey,3)+CCURRCODE+Fabric+CORDTYPE+cmORDER+STR(LINENO,6)'
	  ELSE
	    lcIndexTg = 'RIGHT(cTempKey,3)+Fabric+CORDTYPE+cmORDER+STR(LINENO,6)'
	  ENDIF  
	ELSE                     && Else Sort by Fabric	
      IF llCurInGrp
		lcIndexTg = 'RIGHT(cTempKey,3)+CCURRCODE+CORDTYPE+cmORDER+STR(LINENO,6)+Fabric'
      ELSE
		lcIndexTg = 'RIGHT(cTempKey,3)+CORDTYPE+cmORDER+STR(LINENO,6)+Fabric'
      ENDIF		
	ENDIF
    	
    IF llCurInGrp
      lcOutGrp   = [RIGHT(cTempKey,3)+CCURRCODE]
    ELSE
      lcOutGrp   = [RIGHT(cTempKey,3)]
    ENDIF  

    *-- report variables data sales Rep. case 
    lcSubTitle = 'Primary Sales Representative'
    lcInnGrp   = ['']
    lcInnFootL = [''] 
    IF llTextMode
      lcOutFootL = [RIGHT(cTempKey,3) + ':'+PADR(SalesRep.Name,21)+"/"+EVALUATE(lcLineCurr)]  
    ELSE
      lcInnFootR = ['']
      lcOutFootL = ['Primary Sales Rep. # ' + RIGHT(cTempKey,3) + ' :']
      lcOutFootR = [SalesRep.Name+"/"+EVALUATE(lcLineCurr)]
    ENDIF

  CASE lcRpSortBy = 'D'    && Sort by complete date
    
    IF llCurInGrp
      lcIndexTg = 'DTOS(COMPLETE)+CCURRCODE+Fabric+CORDTYPE+cmORDER+STR(LINENO,6)'
      lcOutGrp  = [DTOS(COMPLETE)+CCURRCODE]
    ELSE
      lcIndexTg = 'DTOS(COMPLETE)+Fabric+CORDTYPE+cmORDER+STR(LINENO,6)'
      lcOutGrp  = [DTOS(COMPLETE)]
    ENDIF  

    *-- report variables data Complete date case 
    lcSubTitle = 'Complete Date'
    lcInnGrp   = ['']
    lcInnFootL = [''] 
    IF llTextMode
      lcOutFootL = ["Date Completed : " + DTOC(Complete)+"/"+EVALUATE(lcLineCurr)]
    ELSE  
      lcInnFootR = ['']
      lcOutFootL = ['******']
      lcOutFootR = [DTOC(Complete)+"/"+EVALUATE(lcLineCurr)]
    ENDIF
  
  CASE lcRpSortBy = 'U'    && Sort by currency

    IF lcRpSrt2 = 'L'    && Sort by Line#
      lcIndexTg = 'CCURRCODE+CORDTYPE+cmORDER+STR(LINENO,6)+Fabric'
    ELSE                  && Else Sort by Fabric	
      lcIndexTg = 'CCURRCODE+CORDTYPE+cmORDER+Fabric+STR(LINENO,6)'
	ENDIF
    
    *-- report variables data order case 
    lcSubTitle = 'Currency'
    lcInnGrp   = [CMORDER]

    IF llTextMode
      lcInnFootL = [CMORDER+', Account#'+Account  + ' Entered in: ' + ALLTRIM(DTOC(MASOHDR.Entered))] 
      lcOutFootL = ["Currency : " + lfCurrDesc()]    
    ELSE
      lcInnFootR = ['Order:']
      lcInnFootL = [CMORDER+', Account#'+Account  + ' Entered in: ' + ALLTRIM(DTOC(MASOHDR.Entered))] 
      lcOutFootL = ['Currency : ']
      lcOutFootR = [lfCurrDesc()]
    ENDIF

    *-- Sort on proper orders not only 'O' type.
    lcSeekVal = [cCurrCode+cOrdType+cmORDER]
    lcOutGrp  = [CCURRCODE]    
ENDCASE          && end Different sort by cases.

IF llCurInGrp AND (lcRpSortBy <> 'U')
  lcSubTitle = lcSubTitle+"/Currency"
ENDIF
*-- end of lfGetRepVr.

*!*************************************************************
*! Name      : lfCreatCur
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
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
FUNCTION lfCreatCur
PARAMETERS lcCurName
*-- We need temp. files to be files not cursor to open it in another alias 
CREATE TABLE (gcWorkDir+lcCurName) ;
   FROM ARRAY laTempStru
*-- end of lfCreatCur.

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;                                                            
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))))
  ENDFOR  && end Loop to make Status expression.
ENDIF

*-- if empty of status is like select all available values.
lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llOGFltCh = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llOGFltCh = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF

DO lpChkStat
*-- end of lfvOStatus.

*!*************************************************************
*! Name      : lfArrDumy
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Fill Sort and select arrays
*!*************************************************************
*! Example   : = lfArrDumy()
*!*************************************************************

*-- Function to fill select by and sort by arrays.
FUNCTION lfArrDumy
PRIVATE lnSelElms , lnSrtElms
lnSelElms = 3
lnSrtElms = 5
DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1],;
          laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1],;
          laSort2Des[2,1],laSort2Val[2,1]

*-- Sort 2 elementes [Begin]
laSort2Des[1,1] = "Line#"
laSort2Des[2,1] = "Fabric"
laSort2Val[1,1] = "L"
laSort2Val[2,1] = "S"

*-- Sort 1 base elements [Begin]
laSortDesc[1,1] = 'Account'
laSortDesc[2,1] = 'Order'
laSortDesc[3,1] = 'Purchase Group'
laSortDesc[4,1] = 'Primary Sales Representative'
laSortDesc[5,1] = 'Complete Date'

laSortVal[1,1]  = 'A'
laSortVal[2,1]  = 'O'
laSortVal[3,1]  = 'G'
laSortVal[4,1]  = 'R'
laSortVal[5,1]  = 'D'

*-- Fill Select by array base elements. [Begin]
laSlctDesc[1,1] = 'Account'
laSlctDesc[2,1] = 'Primary Sales Representative'
laSlctDesc[3,1] = 'All'

laSlctVal[1,1]  = 'A'
laSlctVal[2,1]  = 'R'
laSlctVal[3,1]  = 'L'

IF llMultLoc
  lnSelElms = lnSelElms + 1
  lnSrtElms = lnSrtElms + 1
  DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1],;
            laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1]

  =AINS(laSortDesc,5,1)
  =AINS(laSortVal,5,1)
  =AINS(laSlctDesc,2,1)
  =AINS(laSlctVal,2,1)
  STORE 'Location' TO laSortDesc[5,1],laSlctDesc[2,1]
  STORE 'W' TO laSortVal[5,1],laSlctVal[2,1]
ENDIF

IF 'MA' $ gcCmpModules
  lnSelElms = lnSelElms + 1
  lnSrtElms = lnSrtElms + 1
  DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1],;
            laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1]

  =AINS(laSortDesc,5,1)
  =AINS(laSortVal,5,1)

  lnInsFabIn = ASCAN(laSlctDesc,'All',1)
  =AINS(laSlctDesc,lnInsFabIn,1)
  =AINS(laSlctVal,lnInsFabIn,1)
  STORE 'Fabric' TO laSortDesc[5,1],laSlctDesc[lnInsFabIn,1]
  STORE 'F' TO laSortVal[5,1],laSlctVal[lnInsFabIn,1]
ENDIF

IF llMultCurr
  lnSrtElms = lnSrtElms + 1
  DIMENSION laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1]
  laSortDesc[ALEN(laSortDesc,1),1] = "Currency"
  laSortVal[ALEN(laSortDesc,1),1]  = "U"
ENDIF
*-- end of lfArrDumy.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

*!**************************************************************************
*! Name      : lfAssignSc
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : to save the current Scale after printing it in order not to 
*!             print it except when Scale changes 
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : SOMADET.FRX
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns            : ....
*!**************************************************************************
*! Example   : = lfAssignSc()
*!**************************************************************************

FUNCTION lfAssignSc
*-- Remark the following line and don't get the scale unless 
*-- it's printed once.
llFirstSc = .T.

RETURN ''
*-- end of lfAssignSc.

*!**************************************************************************
*! Name      : lfScalePgH
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : to empty lcOldScale var. in each Page Header Band in  
*!             order to be printed once at the start of the Page if 
*!             the Scale is not changed 
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : SOMADET.FRX
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns            : ....
*!**************************************************************************
*! Example   : = lfScalePgH()
*!**************************************************************************

FUNCTION lfScalePgH
lcOldScale = SPACE(3)
RETURN ''
*-- end of lfScalePgH.

*!*************************************************************
*! Name      : lfInnGrpIn
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Evaluate inner group values, when you enter group header.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : All FRXs (DOS Format)
*!**************************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : =lfInnGrpIn()
*!*************************************************************

FUNCTION lfInnGrpIn
lcInnGrpIn  = EVALUATE(lcInnGrp)
RETURN ''
*-- end of lfInnGrpIn.

*!*************************************************************
*! Name      : lfOutGrpIn
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Evaluate outer group values, when you enter group header.
*!*************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Called from : All FRXs (DOS Format)
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : =lfOutGrpIn()
*!*************************************************************

FUNCTION lfOutGrpIn
lcOutGrpIn = EVALUATE(lcOutGrp)
RETURN ''
*-- end of lfOutGrpIn.

*!*************************************************************
*! Name      : lfInnGrpOp
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Evaluate inner group values, when you in group Footer.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : All FRXs (DOS Format)
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : =lfInnGrpOp()
*!*************************************************************

FUNCTION lfInnGrpOp

llLstMulti = (MASOHDR.MULTI = "Y")

lcInnGrpOp = EVALUATE(lcInnGrp)
lcPrnInnL  = EVALUATE(lcInnFootL)

lnPrnInnQ1 = lnInnQty1
lnPrnInnQ2 = lnInnQty2
lnPrnInnQ3 = lnInnQty3
lnPrnInnQ4 = lnInnQty4
lnPrnInnQ5 = lnInnQty5
lnPrnInnQ6 = lnInnQty6
lnPrnInnQ7 = lnInnQty7
lnPrnInnQ8 = lnInnQty8
lnPrnInnTQ = lnInnTtQty
lnPrnInnAm = lnGrInnAmt
RETURN ''
*-- end of lfInnGrpOp.

*!*************************************************************
*! Name      : lfOutGrpOp
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Evaluate outer group values, when you in group Footer.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : All FRXs (DOS Format)
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : =lfOutGrpOp()
*!*************************************************************

FUNCTION lfOutGrpOp
lcOutGrpOp = EVALUATE(lcOutGrp)
lcPrnOutL  = EVALUATE(lcOutFootL)
lnPrnOutQ1 = lnOutQty1
lnPrnOutQ2 = lnOutQty2
lnPrnOutQ3 = lnOutQty3
lnPrnOutQ4 = lnOutQty4
lnPrnOutQ5 = lnOutQty5
lnPrnOutQ6 = lnOutQty6
lnPrnOutQ7 = lnOutQty7
lnPrnOutQ8 = lnOutQty8
lnPrnOutTQ = lnOutTtQty
lnPrnOutAm = lnGrOutAmt
RETURN ''
*-- end of lfOutGrpOp.

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Activate currency display screen to get user 
*!           : selection for currencies.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************

FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfCurrPrnt
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Compute Currency symbol to print.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCurrPrnt()
*!*************************************************************

FUNCTION lfCurrPrnt
PRIVATE lcCurrCode
*-- Not Multi Currency Or it is and any Equavelent method.
IF !llMultCurr OR lcRpCurr <> "F"
  lcCurrCode = [gcBaseCurr]
ELSE && Multi Currency and Print forign currency.
  lcCurrCode = [MASOHDR.cCurrCode]
ENDIF
RETURN lcCurrCode
*-- end of lfCurrPrnt.

*!*************************************************************
*! Name      : lfChCurSm
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Share with last function to Compute Currency symbol to print.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : All FRXs (DOS Format)
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfChCurSm()
*!*************************************************************

FUNCTION lfChCurSm
PRIVATE lcCurrCurr
lcCurrCurr = ALLTRIM(EVALUATE(lcLineCurr))
SELECT SYCINT
LOCATE FOR cCurrCode = lcCurrCurr
IF FOUND()
  lcCurrRtL = ALLTRIM(cCurrency)
  lcCurrSet = ALLTRIM(cCurrencyI)
  SET CURRENCY TO lcCurrSet
  SET CURRENCY &lcCurrRtL
ENDIF  
RETURN ''
*-- end of lfChCurSm.

*!*************************************************************
*! Name      : lfCurrDesc
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/29/2002
*! Purpose   : Currency description if sort by currency.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Currency description.
*!*************************************************************
*! Example   : =lfCurrDesc()
*!*************************************************************

FUNCTION lfCurrDesc
PRIVATE lcCurrVal , lcCurDesc
lcCurDesc = ''
lcCurrVal  = ALLTRIM(cCurrCode)
lnCurVlPos = ASCAN(laCurrVal,lcCurrVal)
IF lnCurVlPos > 0
  lcCurDesc  = laCurrDesc[lnCurVlPos,1]
ENDIF  
RETURN lcCurDesc
*-- end of lfCurrDesc.

*!**************************************************************************
*! Name      : lfvDisMesg
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 09/10/2000
*! Purpose   : Diaplay message to inform user to ignore Complete,Cancel,Bid Orders
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : = lfvDisMesg()
*!**************************************************************************

FUNCTION lfvDisMesg
PARAMETERS lcLine
IF lcLine = "A"
  IF llRpPrtShp AND IIF('B' $ lcRpStatus OR 'C' $ lcRpStatus OR 'X' $ lcRpStatus,.T.,.F.)
    *-- Message < This option will be applied only on Hold and Open Orders, it will >
    *--         < ignore other orders status.                                       >
    *-- Buttons <                                OK                                 >
    = gfModalGen("INM32088B00000","Dialog")
  ENDIF
ELSE
  IF !llRpAloOrd AND IIF('B' $ lcRpStatus OR 'C' $ lcRpStatus OR 'X' $ lcRpStatus,.T.,.F.)
    *-- Message < This option will be applied only on Hold and Open Orders, it will >
    *--         < ignore other orders status.                                       >
    *-- Buttons <                                OK                                 >
    = gfModalGen("INM32088B00000","Dialog")
  ENDIF 
ENDIF  
*-- End of lfvDisMesg.

*:**************************************************************************
*:* Name        : lfRmZr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/03/2005
*:* Purpose     : Remove zeros in the right of the decimal point
*:***************************************************************************
*:* Called from : ARMATIA.FRX
*:***************************************************************************
FUNCTION lfRmZr
PARAMETERS lcValue
lcValue = ALLTRIM(lcValue) + ' '
PRIVATE lnI
FOR lnI = 1 TO 5
  lcValue = STRTRAN(lcValue,'0 ','  ')
ENDFOR
*N125956,1  TMI [Start] if there is no numbers after the decimal , remove the decimal point
lcValue = STRTRAN(lcValue,'. ','  ')
*N125956,1  TMI [End  ] 
lcValue = LEFT(lcValue,LEN(lcValue)-1)
lcValue = PADL(lcValue,10)
RETURN  lcValue
*-- end of lfRmZr.

*:**************************************************************************
*:* Name        : lfRmZr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/03/2005
*:* Purpose     : Remove zeros in the right of the decimal point
*:***************************************************************************
*:* Called from : ARMATIA.FRX
*:***************************************************************************
FUNCTION lfRmZro
PARAMETERS lnValue
PRIVATE lcValue,lnI
lcValue = gfAddCurSm(MASOHDR.CCURRCODE , lnValue , 5)
lcValue = lcValue + ' '
PRIVATE lnI
FOR lnI = 1 TO 5
  lcValue = STRTRAN(lcValue,'0 ','  ')
ENDFOR
*N125956,1  TMI [Start] if there is no numbers after the decimal , remove the decimal point
lcValue = STRTRAN(lcValue,'. ','  ')
*N125956,1  TMI [End  ] 
lcValue = LEFT(lcValue,LEN(lcValue)-1)


RETURN  lcValue
*-- end of lfRmZr.