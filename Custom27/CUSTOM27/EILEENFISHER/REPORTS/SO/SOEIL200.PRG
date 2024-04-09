*:***********************************************************************************
*: Program file  : Soeil200.prg 
*: Program desc. : Order Detail Report (For Customer Eleen Fisher)
*: For Report    : (Soeil20A.FRX,Soeil20B.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : ABDOU ELGENDI        - (ABD)
*: DATE          : 05/09/2000
*: REFER TO      : (C101825)
*:***********************************************************************************
*: Calls : 
*:    -----------------------------------------------------------------------
*:    Functions  : lfwRepWhen , lfvOptMsg , lfStySum   , lfvEdiOrd  , lfSROrder
*:               : lfSRVFab   , lfFabSum  , lfEvalSegs , lfNMajType , lfvOStatus
*:               : lfFillVars , lfvSizes  , lfvSortBy  , lfvPrint   , lfLastRec
*:               : lfAssignSc , lfScalePgH, lfInnGrpIn , lfOutGrpIn , lfInnGrpOp
*:               : lfOutGrpOp , lfGetWork , lfChCurSm  , lfWorkEnd  , lfGetRepVr
*:               : lfCollData , lfCurrPrnt, lfItmPos   , lfCollTime , lfsrAcc ,
*:    -----------------------------------------------------------------------
*:    Procedures : ....
*:***********************************************************************************
*: Passed Parameters  : None
*:***********************************************************************************
*: Example : DO Soeil200
*:***********************************************************************************
*: This Program is due to C101825 ...
*:***********************************************************************************
*:Modifications
*B803396,1 ABD 07/18/2000 Fix Some bugs  When printing the report in the summary format.
*B803396,1 ABD            Fix in Frxs Dos & Graphic.
*B803564,1 ABD 08/02/2000 Fix the following bugs:-
*B803564,1 ABD            1- Remove the header in case of print summary format.
*B803564,1 ABD            2- Remove the columns Piktkt and Diff. from the report 
*B803564,1 ABD               (summary and detail format)
*B803564,1 ABD               this change made in frx only.
*B803564,1 ABD            3- Fix bug that the figure show in the text format is wrong
*B803564,1 ABD               However, when printing the graphics format is correct.
*B603816,1 ABD 08/16/2000 Fix bug that the unit price will always be picked from the 
*B603816,1 ABD            STYLE file, It will never be picked from the ORDLINE file.
*B803648,1 SSE 09/12/2000 Fix bug of doing the following (All modifications in FRXs and Sydreprt). 
*B803648,1                1) Change name of report to "Order Booking Report" Instead of "Order Detail Reprt".
*B803648,1                2) Detail Format "Price" columns needs to be "Net Price".
*B803648,1                3) When Printing Summary format and Selecting Print either Sales Value AND/OR
*B803648,1                   Cost value , nothing is printed.
*B803648,1                4) Allow printing of Bid and Complete Status 
*B803691,1 SSE 10/01/2000 Fix bug of spacing in custom Order Booking report when sort by 
*B803691,1                Fabric Summary reports (all modifications is in FRXs)
*C101995,1 SSE 10/30/2000 1) New Account Inlist Selection is added to Option Grid.
*C101995,1                2) Add an option of Sort By Account.
*C101995,1                3) Enable user to select more than one Priority option. 
*B803922,1 AAN 05/14/2001 Enhance the performance when th euser select on account or one fabric.
*B603810,1 AAN 09/23/2001 Increase the width of fabric# browse screen in O.G. This bug done in SyRepUvr.DBF file
*:***********************************************************************************
*:           
*-- Initialize variables.
*-- lcOutHeadL : Left  title of outer group header.
*-- lcOutHeadR : Right title of outer group header.  
*-- lcInnHeadL : Left  title of inner group header.
*-- lcInnHeadR : Right title of inner group header.
*-- Sort by "S" --> Style-Color
*-- Sort by "F" --> Fabric
*-- Sort by "C" --> Fabric-Color
lcTime     = TIME()                     && Variable to hold the Time
lcStTime   = lcTime                     && Time in which we start collect data.
lnLastRec  = 2                          && Record No. Of last record in order group.
lcTitle    = ''                         && Title of Note. 
lcNotes    = ''                         && Notes.
llNoIndex  = .F.                        && I don't make index for file.
lnCostValu = 0
*B803564,1 ABD Fix Bug wrong figures in the text format. [Begin]
lnTotCostv = 0
lnPriceA   = 0
*B603816,1 ABD [Begin]
lnGrndtotP = 0
*B603816,1 ABD [End]
*B803564,1  ABD [END]

STORE '' TO lcLineCurr ,lcSale ,lcCost
STORE .T. TO llInnTotal,llOutTotal

lcSeekVal  = ''                         && Get Last record seek value.
llTextMode = (UPPER(ALLTRIM(lcRepMode))=="TEXT")  && Print Text Format
lcOldScale = SPACE(3)
DIMENSION laStock[9],laWip[9]
STORE '' TO laStock,laWip               && Work process arrays

*C101995,1 1) [Begin]
*STORE '' TO lcGrpExp,lcSeaExp,lcDivExp,lcStatusEx,lcCatExp
STORE '' TO lcGrpExp,lcSeaExp,lcDivExp,lcStatusEx,lcCatExp,lcAcctExpr,lcPriorExp
*C101995,1 1) [End]

lcPhonPict = gfPhoneTem()


*-- Print totals if and only if [Not multi currency or user want any equavelent method or 
*-- select only one currency to print]
llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
             ((!EMPTY(laOGFxFlt[lnCurrPos,6]) AND ATC("|",laOGFxFlt[lnCurrPos,6])=0) OR ;
              (!EMPTY(laOGFxFlt[lnOrdPos ,6]) AND USED(laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(laOGFxFlt[lnOrdPos,6])=1))

llGrdTotal = llPrintTot
llCurInGrp = !llPrintTot


*-- Show messages in status bar when collecting data. [begin]
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON
*-- Show messages in status bar when collecting data. [begin]

*--Get Report variables such as groups and index.     
= lfGetRepVr()


IF llOGFltCh
  lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.
  DIMENSION laTempStru[1,4]
  laTempStru = ''
  SELECT ORDLINE
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

  *-- Create temporary file that holding order line data.
  CREATE TABLE (gcWorkDir+lcWorkFile) FROM ARRAY laTempStru  
  CREATE TABLE (gcWorkDir+lcTempLine) FROM ARRAY laTempStru

  *-- start fill variabls.
  lcStartSt = DTOS(CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],1,;
                   ATC('|',laOGFxFlt[lnStartPos,6])-1)))
  lcStartEd = DTOS(CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],;
                   ATC('|',laOGFxFlt[lnStartPos,6])+1)))

  *--Depend on both sides Flag when collecting data.
  llSrtSides = EMPTY(ALLTRIM(lcStartSt+lcStartEd))


  lcCompSt  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],1,;
                   ATC('|',laOGFxFlt[lnCompPos,6])-1)))
  lcCompEd  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],;
                   ATC('|',laOGFxFlt[lnCompPos,6])+1)))

  *-- Depend on both sides Flag when collecting data [Begin]
  llCmpSides = EMPTY(ALLTRIM(lcCompSt + lcCompEd))
  *-- get status of printing orders.  
  lcStatusEx = [ORDHDR.STATUS $ lcRpStatus]
  

  *-- if user select Season, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnSeaPos,6])
    lcSeaExp  = "&laOGFxFlt[lnSeaPos,1]." + ' $ laOGFxFlt[lnSeaPos,6]'
  ENDIF  && end if user select Season, evaluate its expression.

  *-- if user select Division, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnDivPos,6])
    lcDivExp  = "&laOGFxFlt[lnDivPos,1]." + ' $ laOGFxFlt[lnDivPos,6]'

  ENDIF  && if user select Division, evaluate its expression.
  
  *--Add Order Category to filter. [Begin]
  IF !EMPTY(laOGFxFlt[lnCatPos,6])
    lcCatExp  = "&laOGFxFlt[lnCatPos,1]." + ' $ laOGFxFlt[lnCatPos,6]'
  ENDIF
    
  *-- Evaluate Color/Free Expression. [begin]
  *-- Note that: We use either Only This XXX color object or direct XXX 
  *--            Free object, and you must know that both types of 
  *--            expressions can't be enable at the same time.

  

  *-- lcCrFrExp : Color Or free seg. expr.
  *-- if you have Style non major Coler or free segment.

  lcCrFrExp = ''
  IF EMPTY(laOGFxFlt[lnClrSgPos,6]) 
    IF !EMPTY(laOGFxFlt[lnFreSgPos,6])
      lcCrFrExp  = "&laOGFxFlt[lnFreSgPos,1]." + ' $ laOGFxFlt[lnFreSgPos,6]'
    ENDIF
  ELSE
    lcCrFrExp  = "&laOGFxFlt[lnClrSgPos,1]." + ' $ laOGFxFlt[lnClrSgPos,6]'
  ENDIF
  

  *-- Evaluate Color/Free Expression. [end]
  lcLastExpr = lcRpExp   && To later know that user change critria.

  *C101995,1 1) Get the Account Expression from lcRpExp , also Priority Expr. [Begin]
  STORE ".T." TO lcAcctExpr , lcPriorExp 
  *-- If user selected Account from Customer InList file
  IF (USED(laOGFxFlt[lnAcctPos,6]) AND RECCOUNT(laOGFxFlt[lnAcctPos,6]) > 0) OR !EMPTY(laOGFxFlt[lnPriPos,6])
    DO lpGenExp
  ENDIF
  *C101995,1 1) Get the Account Expression from lcRpExp , also Priority Expr. [End]

  *-- Scan around master file and Collect data .
  = lfCollData()
ENDIF


SELECT OrdLine
SET RELATION OFF INTO ORDHDR  && break relation.
SET RELATION OFF INTO STYLE  && break relation.

*-- Temporary File relations, in the way that help us in report form [begin]
SELECT (lcMastFile)
SET RELATION TO cOrdType + Order INTO OrdHdr      && To order header file.
SET RELATION TO style INTO Style ADDITIVE         && To style file.
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE   && To scale file.
SET RELATION TO cWareCode INTO WAREHOUS ADDITIVE  && To warehouse file.

lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store) ]
SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.

*-- If sort by fabric , set relation to fabric file Index on Fabric.
IF lcRpSortBy = 'F'
  SET ORDER TO CFABRIC IN FABRIC 
  SET RELATION TO LEFT(cTempKey,7) INTO Fabric ADDITIVE
ENDIF  && end If sort by fabric.

*-- If sort by fabric , set relation to fabric file Index on Fabric - Color.
IF lcRpSortBy = 'C'
  SET RELATION TO LEFT(cTempKey,13) INTO Fabric ADDITIVE
ENDIF  && end If sort by fabric.

*-- If you need to find the last recored ,open master file in another alias to 
*-- help us to know what last line in order group .

USE (gcWorkDir+lcMastFile) ORDER TAG (lcMastFile) IN 0 AGAIN ALIAS GETLAST

*-- Select Master report file.
SELECT (lcMastFile)
GO BOTTOM
lnLastOne  = RECNO()
GO TOP    && Refresh Relation

*-- if dos mode do the following [Begin
IF llTextMode

  STORE ' ' TO lcInnGrpIn,lcOutGrpIn,lcInnGrpOp,lcOutGrpOp
  STORE '' TO lcPrnInnL
  STORE 0 TO lnInnQty1 ,lnInnQty2 ,lnInnQty3 ,lnInnQty4,;
             lnInnQty5 ,lnInnQty6 ,lnInnQty7 ,lnInnQty8,;
             lnInnTtQty,lnGrInnAmt
  STORE 0 TO lnPrnInnQ1,lnPrnInnQ2,lnPrnInnQ3,lnPrnInnQ4,;
             lnPrnInnQ5,lnPrnInnQ6,lnPrnInnQ7,lnPrnInnQ8,;
             lnPrnInnTQ,lnPrnInnAm
  lnCostValu = 0
  *-- Function to Evaluate inner group values, when you in group Footer.
  =lfInnGrpOp()

  STORE '' TO lcPrnOutL
  STORE 0 TO lnOutQty1 ,lnOutQty2 ,lnOutQty3 ,lnOutQty4,;
             lnOutQty5 ,lnOutQty6 ,lnOutQty7 ,lnOutQty8,;
             lnOutTtQty,lnGrOutAmt
  STORE 0 TO lnPrnOutQ1,lnPrnOutQ2,lnPrnOutQ3,lnPrnOutQ4,;
             lnPrnOutQ5,lnPrnOutQ6,lnPrnOutQ7,lnPrnOutQ8,;
             lnPrnOutTQ,lnPrnOutAm

  *B803648,1 Initialize Total Cost & Total Sale before FRX Text runs [Begin]
  STORE 0 TO lnOutCost , lnOutToCst , lnOutSale , lnOutToSal
  *B803648,1 Initialize Total Cost & Total Sale before FRX Text runs [End]
  
  *-- Function to Evaluate outer group values, when you in group Footer.
  =lfOutGrpOp()
ENDIF             

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)

*--- If dos mode do the following [Begin
IF llTextMode
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMastFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
ELSE  && else window format
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMastFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' TIMEOUT 1
ENDIF  

DO gfDispRe WITH EVAL('lcRpForm') 

*-- Close master file that open in another alias that
*-- help us to know what last line in order group .
USE IN GETLAST
WAIT CLEAR

*-- End Of my Code [End Report] 
*-------------------------------------------------------------*
*----            FunCtion & Procedures                    ----*
*-------------------------------------------------------------*
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Option Grid When function.
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
*
FUNCTION lfwRepWhen
*-- if it's first time to run the report.
*-- using TYPE of variable instead of global llFirstTime, to control
*-- reset case which does not rise llFirsttime, but restore initial
*-- value for lnVarbEnd and advanced case which keep the variables same.
IF TYPE('lnVarbEnd') = 'C'
  lnVarbEnd = 0
  DIMENSION laSetupI[5,2]
  *-- Default all status was selected.. [Begin]
  
  *B803648,1 Change size of array in order to add the Bid and Complete [Begin]
  *DECLARE laRpSource[3],laRpTarget[1]
  *STORE 'Open'     TO laRpSource[1]
  *STORE 'Hold'     TO laRpSource[2]
  *STORE 'Canceled' TO laRpSource[3]
  *lcRpStatus = 'OHX'
  DECLARE laRpSource[5],laRpTarget[1]
  STORE 'Bid'      TO laRpSource[1]  
  STORE 'Open'     TO laRpSource[2]
  STORE 'Hold'     TO laRpSource[3]
  STORE 'Complete' TO laRpSource[4]
  STORE 'Canceled' TO laRpSource[5]
  lcRpStatus = 'BOHCX'  
  *B803648,1 Change size of array in order to add the Bid and Complete [Begin]
ENDIF

*C101995,1 1) Getting the Account position in fixed filter [Begin]
lnAcctPos  = lfItmPos('ORDHDR.ACCOUNT')    && Account Position
*C101995,1 1) Getting the Account position in fixed filter [End]

lnOrdPos   = lfItmPos('ORDHDR.ORDER')
lnStartPos = lfItmPos('ORDLINE.START')
lnCompPos  = lfItmPos('ORDLINE.COMPLETE')
lnGrpPos   = lfItmPos('STYLE.CSTYGROUP')
lnSeaPos   = lfItmPos('STYLE.SEASON')
lnDivPos   = lfItmPos('STYLE.CDIVISION')
lnPriPos   = lfItmPos('ORDHDR.PRIORITY')
lnCatPos   = lfItmPos('ORDHDR.CORDERCAT')
lnClrSgPos = lfItmPos('SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)')
lnFreSgPos = lnClrSgPos + 1


IF llCostAccs
  llShowCost = lcRPShow $ 'CB'
ELSE  
  llShowCost = .F.
ENDIF  

IF llShowCost
  lcCostMth = ALLTRIM(UPPER(gfGetMemVar('M_COST_MET'))) 
ENDIF
llShowSale = lcRPShow $ 'SB'

*-- If company have material material. 
IF 'MA' $ gcCmpModules
  lnFabPos   = lfItmPos('FABRIC.FABRIC')
  SET ORDER TO FABRIC IN FABRIC      && To use it to validate FABRIC  # in option grid.
    *-- Get all need memory variables [START]
    DIMENSION laSetupM[5,2]
    *-- Save Setup Parameters in memvar [Begin]
    laSetupM[1,1]  = 'M_CMTYPE1'
    laSetupM[2,1]  = 'M_CMTYPE2'
    laSetupM[3,1]  = 'M_CMTYPE3'
    laSetupM[4,1]  = 'M_CMTYPE4'
    laSetupM[5,1]  = 'M_CMTYPE5'
    =gfGetMemVar(@laSetupM,gcAct_Comp)
    FOR I = 1 To 5
      IF  laSetupM[I,2] = "F"
        lnFabColoM = I
        EXIT
      ENDIF 
    ENDFOR
ENDIF  
*----- M_CMTYPE1 
*-- Save Setup Parameters in memvar [Begin]
laSetupI[1,1]  = 'M_CITYPE1'
laSetupI[2,1]  = 'M_CITYPE2'      
laSetupI[3,1]  = 'M_CITYPE3'
laSetupI[4,1]  = 'M_CITYPE4'
laSetupI[5,1]  = 'M_CITYPE5'
=gfGetMemVar(@laSetupI,gcAct_Comp)
FOR I = 1 To 5
  IF  laSetupI[I,2] = "F" 
    lnFabColoI = I
    EXIT
  ENDIF 
ENDFOR



IF llMultCurr
  SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
  lnCurrPos  = lfItmPos('ORDHDR.CCURRCODE')
ENDIF

*-- End of lfwRepWhen.
*!*************************************************************

*-------------------------------------------------------------*
*----            FunCtion & Procedures                    ----*
*----         FunCtion Call From Option Grid              ----*
*-------------------------------------------------------------*
*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Function to get Optional Message from the User
*!           : [Validation function for the Push button Optional Message]
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
*
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[1,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[1,2] = 75                && Line length
= gfOptMsg('laOptMsg')            && Call Function to write optional message.

*-- End of lfvOptMsg.
*!*************************************************************
*! Name      : lfStySum
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
*
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)

*-- End of lfStySum.
*!***************************************************************
*! Name      : lfvEdiOrd
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!***************************************************************
*! Calls     : None.
*!***************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!***************************************************************
*! Passed Parameters  : None
*!***************************************************************
*! Returns            : Calculated field value.
*!***************************************************************
*! Example   : =lfvEdiOrd()
*!***************************************************************
*
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
IF 'EB' $ gcCmpModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF

*-- End of lfvEdiOrd.
*!*************************************************************
*! Name      : lfSROrder
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Calls     : None.
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
*
FUNCTION lfSROrder
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
    GO TOP
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClearOrd = .F.
ENDCASE

*-- End of lfSROrder.
*!*************************************************************
*! Name      : lfSRVFab
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Control browsing primary fabric and validate 
*!           : Selecting it in inlist function.
*!*************************************************************
*! Calls     : 
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
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR STYLE.Fabric = Fabric.Fabric
    llHaveSty = FOUND()
    *-- If no styles found for this fabric
    IF !llHaveSty
      *-- the following message is
      *-- No styles in fabric group XXX .
      *--           <Ok>
      = gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE

*-- End of lfSRVFab.
*!*************************************************************
*! Name      : lfFabSum
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
*
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

*-- End of lfFabSum.
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
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
*
FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
  
*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you does not find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*-- End of lfEvalSegs.
*!*************************************************************
*! Name      : lfNMajType
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Mask NonMajor segments .
*!*************************************************************
*! Called from : lfEvalSegs.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNMajType()
*!*************************************************************
*
FUNCTION lfNMajType
PARAMETERS lcNMajType,lnMajSegs

*-- Loop Around Non Major elements.
FOR lnI = lnMajSegs + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = lcNMajType
    lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
    lnNonMajSt = IIF(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSegs[lnI,3],;
                     lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl),PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                     lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && End If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && End Loop Around Non Major elements.

RETURN !EMPTY(lcFree_Clr)

*-- End of lfNMajType. 
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
*
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    *B803648,1 Add feature to allow printing Bid and Complete [Begin]
    *lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
    *                          IIF(laRpTarget[lnI] = 'Hold','H',;
    *                          IIF(laRpTarget[lnI] = 'Canceled','X','')))
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;                                                            
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))))
    *B803648,1 Add feature to allow printing Bid and Complete [End]
  ENDFOR  && end Loop to make Status expression.
ENDIF

*-- if empty of status is like select all available values.
*B803648,1 Add feature to allow printing Bid and Complete [Begin]
*lcRpStatus = IIF(EMPTY(lcRpStatus),'OHX',ALLTRIM(lcRpStatus))
lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))
*B803648,1 Add feature to allow printing Bid and Complete [End]

*-- Compare current selected status with old value  [begin]
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
*-- Compare current selected status with old value  [end]

*-- End of lfvOStatus.
*!*************************************************************
*! Name      : lfFillVars
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
*
FUNCTION lfFillVars

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  
IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF

*-- Get the Cost Priv.
llCostAccs = gfUserPriv('IC','ICSTYLE','COSTING')

*-- End Of lfFillVars.
*!*************************************************************
*! Name      : lfvSizes
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Control Form name.
*!*************************************************************
*! Calls     : 
*!           : Functions  : lfObjState
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSizes()
*!*************************************************************
*
FUNCTION lfvSizes
lcRpForm = IIF(llRpSizes,'SOEIL20A','SOEIL20B')
= lfRepPltFr(lcRpForm)

*-- End of lfvSizes.
*!*************************************************************
*! Name      : lfvSortBy
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : 1- Enable and disable some variavle objects due to sort case
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
*
FUNCTION lfvSortBy

CLEAR READ

*-- End Of lfvSortBy
*!*************************************************************
*! Name      : lfvPrint
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Validate print Cost and price option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrint()
*!*************************************************************
*
FUNCTION lfvPrint

IF llCostAccs
  llShowCost = lcRPShow $ 'CB'
ELSE  
  llShowCost = .F.
ENDIF  

IF llShowCost
  lcCostMth = ALLTRIM(UPPER(gfGetMemVar('M_COST_MET'))) 
ENDIF
  
llShowSale = lcRPShow $ 'SB'

*-- End Of lfvPrint.

*!*************************************************************

*-------------------------------------------------------------*
*----            FunCtion & Procedures                    ----*
*----         FunCtion Call From FRX DOS & WINDOWS        ----*
*-------------------------------------------------------------*

*!*************************************************************
*! Name      : lfLastRec
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Calculate last Record in order details. [ORDER GROUP]
*!           : we use another alias to unchange record pointer of report file.
*!*************************************************************
*! Called from : [SORDDETA.FRX OR SORDDETB.FRX, ORDER GROUP HEADER BAND] 
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : NULL.
*!*************************************************************
*! Example     : = lfLastRec()
*!*************************************************************
*
FUNCTION lfLastRec

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

RETURN ''

*-- End of lfLastRec.
*!**************************************************************************
*! Name      : lfAssignSc
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : To save the current Scale after printing it in order not to 
*!           : Print it except when Scale changes.
*!**************************************************************************
*! Calls     : None.
*!**************************************************************************
*! Called from : All FRXs
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns            : ....
*!**************************************************************************
*! Example   : = lfAssignSc()
*!**************************************************************************
*
FUNCTION lfAssignSc

lcOldScale = Scale
RETURN ''

*-- End of lfAssignSc.
*!**************************************************************************
*! Name      : lfScalePgH
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : To empty lcOldScale var. in each Page Header Band in  
*!           : Order to be printed once at the start of the Page if 
*!           : The Scale is not changed.
*!**************************************************************************
*! Calls     : None.
*!**************************************************************************
*! Called from : All FRXs
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns            : ....
*!**************************************************************************
*! Example   : = lfScalePgH()
*!**************************************************************************
*
FUNCTION lfScalePgH
lcOldScale = SPACE(3)
RETURN ''

*-- End of lfScalePgH.
*!*************************************************************
*! Name      : lfInnGrpIn
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
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
*
FUNCTION lfInnGrpIn
lcInnGrpIn  = EVALUATE(lcInnGrp)
RETURN ''

*-- End of lfInnGrpIn.
*!*************************************************************
*! Name      : lfOutGrpIn
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
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
*
FUNCTION lfOutGrpIn
lcOutGrpIn = EVALUATE(lcOutGrp)
RETURN ''

*-- End of lfOutGrpIn.
*!*************************************************************
*! Name      : lfInnGrpOp
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
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
*
FUNCTION lfInnGrpOp

lnCostValu  = IIF(lcCostMth='S',Style.TotCost,Style.Ave_Cost)
*B803564,1  Fix Bug wrong figures in the text format. [Begin]
lnTotCostv = IIF(lcCostMth='S' ,Style.TotCost,Style.Ave_Cost)
*B603816,1 ABD Fix Bug that price come from Ordline file.[Begin]
lnPriceA   = Style.PriceA
lnGrndtotP = Style.PriceA*&lcMastFile..TotQty
*B603816,1 ABD [End]
*B803564,1 [End]

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

*-- End of lfInnGrpOp.
*!*************************************************************
*! Name      : lfOutGrpOp
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
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
*
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
lnOutCost  = lnOutToCst
lnOutSale  = lnOutToSal
RETURN ''

*-- End of lfOutGrpOp.
*!*************************************************************
*! Name      : lfGetWork
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Compute work proccessing
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : = lfGetWork()
*!*************************************************************
FUNCTION lfGetWork
STORE '' TO laStock,laWip,lnStkOrWip
*-- Calculate Wip and Stock Values [Begin]

  IF INLIST(lcRpStyPrn,'S','P')
    laStock[1] = IIF(STYLE.Stk1   = 0,'',TRANSFORM(STYLE.Stk1,'99999'))
    laStock[2] = IIF(STYLE.Stk2   = 0,'',TRANSFORM(STYLE.Stk2,'99999'))
    laStock[3] = IIF(STYLE.Stk3   = 0,'',TRANSFORM(STYLE.Stk3,'99999'))
    laStock[4] = IIF(STYLE.Stk4   = 0,'',TRANSFORM(STYLE.Stk4,'99999'))
    laStock[5] = IIF(STYLE.Stk5   = 0,'',TRANSFORM(STYLE.Stk5,'99999'))
    laStock[6] = IIF(STYLE.Stk6   = 0,'',TRANSFORM(STYLE.Stk6,'99999'))
    laStock[7] = IIF(STYLE.Stk7   = 0,'',TRANSFORM(STYLE.Stk7,'99999'))
    laStock[8] = IIF(STYLE.Stk8   = 0,'',TRANSFORM(STYLE.Stk8,'99999'))
    laStock[9] = IIF(STYLE.TotStk = 0,'',TRANSFORM(STYLE.TotStk,'9999999'))
  ENDIF

  IF INLIST(lcRpStyPrn,'W','P')
    laWip[1] = IIF(STYLE.Wip1   = 0,'',TRANSFORM(STYLE.Wip1,'99999'))
    laWip[2] = IIF(STYLE.Wip2   = 0,'',TRANSFORM(STYLE.Wip2,'99999'))
    laWip[3] = IIF(STYLE.Wip3   = 0,'',TRANSFORM(STYLE.Wip3,'99999'))
    laWip[4] = IIF(STYLE.Wip4   = 0,'',TRANSFORM(STYLE.Wip4,'99999'))
    laWip[5] = IIF(STYLE.Wip5   = 0,'',TRANSFORM(STYLE.Wip5,'99999'))
    laWip[6] = IIF(STYLE.Wip6   = 0,'',TRANSFORM(STYLE.Wip6,'99999'))
    laWip[7] = IIF(STYLE.Wip7   = 0,'',TRANSFORM(STYLE.Wip7,'99999'))
    laWip[8] = IIF(STYLE.Wip8   = 0,'',TRANSFORM(STYLE.Wip8,'99999'))
    laWip[9] = IIF(STYLE.TotWip = 0,'',TRANSFORM(STYLE.TotWip,'9999999'))
  ENDIF

  *-- Calculate Wip and Stock Values [End]

  lnStkOrWip = TRANSFORM(STYLE.TotStk + STYLE.TotWip,'999999')  && Calculate wip + stock values
  lnStkOrWip = IIF(VAL(lnStkOrWip) = 0 , '' , lnStkOrWip)
RETURN ''
*-- End of lfGetWork.
*!*************************************************************
*! Name      : lfChCurSm
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
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
*!
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

*-- End of lfChCurSm.
*!*************************************************************
*! Name      : lfWorkEnd
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/20/98
*! Purpose   : - End Compute work proccessing
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : = lfWorkEnd()
*!*************************************************************
*
FUNCTION lfWorkEnd
STORE '' TO laStock,laWip,lnStkOrWip

RETURN ''

*-- End Of lfWorkEnd.
*!*************************************************************
*-------------------------------------------------------------*
*----            FunCtion & Procedures                    ----*
*----         FunCtion Call From Inside The Program       ----*
*-------------------------------------------------------------*
*!*************************************************************
*! Name      : lfGetRepVr
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Put both index and group expressions for all sort cases.
*!*************************************************************
*! Calls     : 
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
*-- lcOutHeadL : Left  title of outer group header.
*-- lcOutHeadR : Right title of outer group header.  
*-- lcInnHeadL : Left  title of inner group header.
*-- lcInnHeadR : Right title of inner group header.

*C101995,1 2) Case of Sorting by Account [Begin]
*-- Sort by "A" --> Account
*C101995,1 2) Case of Sorting by Account [End]
*-- Sort by "S" --> Style-Color
*-- Sort by "F" --> Fabric
*-- Sort by "C" --> Fabric-Color

*C101995,1 2) Case of Sorting by Account [Begin]
*IF lcRpSortBy = 'S'
*  lcOutHeadL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen),19) + '  : ']
*  lcOutHeadR = [Style.cStyMajor]
*  lcInnHeadL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19)+ '  : ']
*  lcInnHeadR = [SUBSTR(Style,lnMajorLen + 2) + '  ----  ' + ALLTRIM(STYLE.Desc1)]
*ELSE
*  STORE [''] TO lcOutHeadL,lcOutHeadR,lcInnHeadL,lcInnHeadR
*ENDIF

DO CASE
  CASE lcRpSortBy = 'A'
    lcOutHeadL = ['Account : ']
    lcOutHeadR = [Account]
    lcInnHeadL = ['Name : ']
    lcInnHeadR = [ALLTRIM(Customer.StName) + ' ---- ' + EVALUATE(lcLineCurr)]
  CASE lcRpSortBy = 'S'
    lcOutHeadL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen),19) + '  : ']
    lcOutHeadR = [Style.cStyMajor]
    lcInnHeadL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19)+ '  : ']
    lcInnHeadR = [SUBSTR(Style,lnMajorLen + 2) + '  ----  ' + ALLTRIM(STYLE.Desc1)]
  OTHERWISE
    STORE [''] TO lcOutHeadL,lcOutHeadR,lcInnHeadL,lcInnHeadR  
ENDCASE 
*C101995,1 2) Case of Sorting by Account [End]

*-- Compute Currency symbol to print.
lcLineCurr = lfCurrPrnt()

*-- Different sort by cases.
DO CASE
  
  *C101995,1 2) Enable user to Sort By Account [Begin]
  CASE lcRpSortBy = 'A'      && Sort by Account 
    IF llCurInGrp
      lcIndexTg = 'ACCOUNT+STYLE+CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)'
      lcSeekVal = [ACCOUNT+CCURRCODE]
      lcOutGrp  = [ACCOUNT+CCURRCODE]
    ELSE
      lcIndexTg = 'ACCOUNT+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
      lcSeekVal = [ACCOUNT]
      lcOutGrp  = [ACCOUNT]
    ENDIF  
    *-- report variables data style case [begin]
    lcSubTitle = "Account"
    lcInnGrp   = [STYLE]

    *-- Define report variables in all sort cases in both text and graphic
    *-- Format keep in mind multi currency situation. [Begin]
    IF llTextMode
      lcInnFootL = ['Style (' + Style + ') :'+ALLTRIM(Style.Desc)]
      lcOutFootL = ['Account ' + Account + ': ' + ALLTRIM(Customer.StName)+"/"+EVALUATE(lcLineCurr)]
    ELSE
      lcInnFootL = ['Style  ( ' + Style + ' )  :']
      lcInnFootR = [ALLTRIM(Style.Desc)]
      lcOutFootL = ['Account ' + Account + ': ']
      lcOutFootR = [ALLTRIM(Customer.StName)+"/"+EVALUATE(lcLineCurr)]
      *-- report variables data fabric case [end]
    ENDIF  
  *C101995,1 2) Enable user to Sort By Account [End]

  CASE lcRpSortBy = 'S'      && Sort by style
    IF llCurInGrp
      lcIndexTg = 'STYLE+CCURRCODE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)'
      lcSeekVal = [STYLE+CCURRCODE]
      lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)+CCURRCODE]
    ELSE
      lcIndexTg = 'STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)'
      lcSeekVal = [STYLE]
      lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)]
    ENDIF  
    *-- report variables data style case [begin]
    lcSubTitle = lcStyTitle
    lcInnGrp   = [SUBSTR(Style,lnMajorLen + 2)]

    *-- Define report variables in all sort cases in both text and graphic
    *-- Format keep in mind multi currency situation. [Begin]
    IF llTextMode
      lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19) + ' : '+]+;
                   [SUBSTR(Style,lnMajorLen + 2)]   

      lcOutFootL = [ALLTRIM(Style.cStyMajor) + "/"+EVALUATE(lcLineCurr)+ ", "+ '(' + ALLTRIM(Style.Fabric) + ')']  
    ELSE
      lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19) + '  : ']
      lcInnFootR = [SUBSTR(Style,lnMajorLen + 2)]
      lcOutFootL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen),19) + '  : ']
      lcOutFootR = [ALLTRIM(Style.cStyMajor) +"/"+EVALUATE(lcLineCurr)+ ", "+ '( ' + ALLTRIM(Style.Fabric) + ' )']
      *-- report variables data style case [end]
    ENDIF  

  CASE lcRpSortBy = 'F'     && Sort by fabric
    IF llCurInGrp
      lcIndexTg = 'LEFT(cTempKey,7)+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
      lcSeekVal = [LEFT(cTempKey,7)+CCURRCODE+STYLE]
      lcOutGrp  = [LEFT(cTempKey,7)+CCURRCODE]
    ELSE
      lcIndexTg = 'LEFT(cTempKey,7)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
      lcSeekVal = [LEFT(cTempKey,7)+STYLE]
      lcOutGrp  = [LEFT(cTempKey,7)]
    ENDIF  

    *-- report variables data fabric case [begin]
    lcSubTitle = 'Primary Fabric'
    lcInnGrp   = [STYLE]

    *-- Define report variables in all sort cases in both text and graphic
    *-- Format keep in mind multi currency situation. [Begin]
    IF llTextMode
      lcInnFootL = ['Style (' + Style + ') :'+ALLTRIM(Style.Desc)]
      lcOutFootL = [LEFT(cTempKey,7) + ':'+ALLTRIM(Fabric.Desc)+"/"+EVALUATE(lcLineCurr)]
    ELSE
      lcInnFootL = ['Style  ( ' + Style + ' )  :']
      lcInnFootR = [ALLTRIM(Style.Desc)]
      lcOutFootL = ['Fabric  ( ' + LEFT(cTempKey,7) + ' )  :']
      lcOutFootR = [ALLTRIM(Fabric.Desc)+"/"+EVALUATE(lcLineCurr)]
      *-- report variables data fabric case [end]
    ENDIF  
    
  CASE lcRpSortBy = 'C'     && Sort by fabric - color
    IF llCurInGrp
      lcIndexTg = 'LEFT(cTempKey,13)+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
      lcSeekVal = [LEFT(cTempKey,13)+CCURRCODE+STYLE]
      lcOutGrp  = [LEFT(cTempKey,13)+CCURRCODE]
    ELSE
      lcIndexTg = 'LEFT(cTempKey,13)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
      lcSeekVal = [LEFT(cTempKey,13)+STYLE]
      lcOutGrp  = [LEFT(cTempKey,13)]
    ENDIF  

    *-- report variables data fabric case [begin]
    lcSubTitle = 'Primary Fabric -'+ALLTRIM(lcNonMajTlt)
    lcInnGrp   = [STYLE]    
    
    
    *-- Define report variables in all sort cases in both text and graphic
    *-- Format keep in mind multi currency situation. [Begin]
    IF llTextMode
      lcInnFootL = ['Style (' + Style + ') :'+ALLTRIM(Style.Desc)]
      lcOutFootL = [LEFT(cTempKey,13) + ':'+ALLTRIM(Fabric.Desc)+"/"+EVALUATE(lcLineCurr)]
    ELSE
      lcInnFootL = ['Style  ( ' + Style + ' )  :']
      lcInnFootR = [ALLTRIM(Style.Desc)]
      lcOutFootL = ['Fabric  ( ' + LEFT(cTempKey,13) + ' )  :']
      lcOutFootR = [ALLTRIM(Fabric.Desc)+"/"+EVALUATE(lcLineCurr)]
      *-- report variables data fabric case [end]
    ENDIF  
    
ENDCASE          && end Different sort by cases.

*-- End of lfGetRepVr.
*!*************************************************************
*! Name      : lfCollData
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Collect report data.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCollData()
*!*************************************************************
*
FUNCTION lfCollData
*-- If you find any data ( not first time you run), clear it. 
IF RECCOUNT(lcTempLine) > 0
  SELECT (lcTempLine)
  ZAP
  SET RELATION TO
ENDIF		&& end If you find any data.

*-- Change index due to changes to Sort By type.
SELECT (lcTempLine)
INDEX ON &lcIndexTg TAG (lcTempLine)
SELECT (lcWorkFile)
IF RECCOUNT(lcWorkFile) > 0
  *-- We need temp. files to be files not cursor to open it in another alias [Begin]
  SELECT (lcWorkFile)
  ZAP
  SET RELATION TO
  SELECT (lcWorkFile)
ENDIF
*--LEFT(cTempKey,7) +STYLE+CORDTYPE+ORDER+STR(LINENO,6)
INDEX ON LEFT(cTempKey,7) +STYLE+CORDTYPE+ORDER+STR(LINENO,6) TAG (lcWorkFile)

*-- Relation with master order file to help data collecting. [begin]
SELECT OrdLine
SET RELATION TO cOrdType + Order INTO OrdHdr
SET RELATION TO Style INTO Style ADDITIVE
*-- Relation with master order file to help data collecting. [end]

*-- llWorkDeal : Flag to know that we start dealing with work file.
*-- llLineDeal : Flag to know that we deal with temp. line file.
STORE .F. TO llWorkDeal , llLineDeal

lcTOrdFile = laOGFxFlt[lnOrdPos,6]
llWorkDeal = !EMPTY(lcTOrdFile) AND USED(lcTOrdFile) AND RECCOUNT(lcTOrdFile) > 0

*-- If user select specific orders, collect data of this orders only. [begin]
IF llWorkDeal
  SELECT (lcTOrdFile)
  *-- Scan order cursor.
  SCAN  
    SELECT ORDLINE
    SET ORDER TO TAG ORDLINE
    *-- If find first order record in ordline file.
    *-- Add the lcRpEdiFlt to the expression.

    *C101995,1 1) Adding the Account Expression [Begin]
    *IF SEEK('O'+&lcTOrdFile..ORDER) AND;
    *   EVALUATE(lcStatusEx) AND ;
    *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
    *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
    *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
    *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
    IF SEEK('O'+&lcTOrdFile..ORDER) AND;
       EVALUATE(lcStatusEx) AND ;
       (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
       IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
       &lcPriorExp AND ;
       IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
       &lcAcctExpr
    *C101995,1 1) Adding the Account Expression [End]
   
      *-- Scan ordline file for rest order data.
      SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+&lcTOrdFile..Order
        *-- if <ordline filter> and <style group filter> and <Color Filter>
        *-- insert this data into workfile.

        *B803648,1 Add Feature to allow printing the Bid and Complete status [Begin]
        *IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
        *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
        *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
        *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
        *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
        *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
        *   IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))
        IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
           (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
           (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
           IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
           IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
           IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
           IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))
        *B803648,1 Add Feature to allow printing the Bid and Complete status [End]

          *-- If style make or by. 
          IF STYLE.MAKE
           lcFabColor = ALLTRIM(STR(lnFabColoM))
          ELSE
           lcFabColor = ALLTRIM(STR(lnFabColoI))          
          ENDIF
          *-- Seek in the BOM file and get the color of the style - fabric color 
          *-- If style have a cost sheet.
          IF SEEK(PADR(LEFT(Style,lnMajorLen),19)+lcFabColor,'BOM')
            *-- If find the style cheak for the color.
            IF  SUBSTR(BOM.cItmmask,lnNonMajSt,lnColorLen) = '******' 
              *-- If all style color have same as fabric color.
              IF  BOM.ICLR ='******'
                lcColor = SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)
              ELSE  && else it have constant color for all style colors.
                lcColor = BOM.ICLR
              ENDIF
            ELSE
              *-- if styles colors use different fabric color.
              =SEEK(PADR(LEFT(Style,lnMajorLen),19)+lcFabColor+STYLE.STYLE,'BOM') 
              lcColor = BOM.ICLR
            ENDIF
          ELSE
            lcColor = SPACE(06)
          ENDIF
          SCATTER MEMVAR MEMO
          m.cTempKey = PADR(STYLE.FABRIC,7) + lcColor +PADR(ORDHDR.REP1,3)          
          m.cCurrCode = ORDHDR.cCurrCode

          *B803648,1 IF Complete status print Booked Qty Because TotQty is equal to Zero [Begin]
          IF ORDHDR.STATUS = "C"
            *-- Get qty from book qty becauce qty =0
            FOR I = 1 To 8
              lcCount = ALLTRIM(STR(I))
              m.QTY&lcCount = BOOK&lcCount
            ENDFOR
            m.TOTQTY = TOTBOOK
          ENDIF
          *B803648,1 IF Complete status print Booked Qty Because TotQty is equal to Zero [End]
                              
          INSERT INTO (lcWorkFile) FROM MEMVAR
        ENDIF  && end if <ordline filter>.
      ENDSCAN  && end Scan ordline file for rest order data.
    ENDIF      && end if find first order record in ordline.
  ENDSCAN      && end Scan order cursor.
  lcMastFile = lcWorkFile
ENDIF  

*-- If user select specific orders, collect data of this orders only. [end]
IF lnFabPos > 0
  lcSlctFile = laOGFxFlt[lnFabPos,6]
  llLineDeal = !EMPTY(lcSlctFile) AND USED(lcSlctFile) AND RECCOUNT(lcSlctFile) > 0
ENDIF  

*-- If User select data by any select case, beside selecting orders. 
*-- IMPORT must be good described before add any line in it.

IF RECCOUNT(lcWorkFile) > 0 AND llLineDeal
  lcSlctKey = "FABRIC"
  PRIVATE lcScaned
  SELECT (lcSlctFile)
  *-- Scan selected cursor
  SCAN
    lcScaned = EVALUATE(lcSlctKey)
    *-- if you find seeking critria in work file. 
    IF SEEK(&lcSlctKey,lcWorkFile)
      SELECT (lcWorkFile)
      *-- scan work file for the rest data have the same seek critria.
      SCAN REST WHILE LEFT(cTempKey,7) +STYLE+CORDTYPE+ORDER+STR(LINENO,6) = lcScaned
          SCATTER MEMVAR MEMO
          m.cCurrCode = ORDHDR.cCurrCode
          INSERT INTO (lcTempLine) FROM MEMVAR
      ENDSCAN  && end scan work file for the rest data have the same seek critria.
    ENDIF      && end if you find seeking critria in work file.
  ENDSCAN      && end Scan selected cursor.
  lcMastFile = lcTempLine

ELSE  && User either Does not select orders or does not use any select type.
  *-- if User does not select orders but use select type.
  IF llLineDeal
    *-- We know he select by fabric.
    SET ORDER TO Ordlines IN ORDLINE
    SELECT ORDLINE
    SET RELATION OFF INTO STYLE  && break relation.
    SELECT (lcSlctFile)
    SCAN
      *-- in this case you can not rushmore data, there is no index in master files. 
      
      *B803922,1 AAN Create new index in style file and change the scan stat.[Begin]
      SELECT STYLE
      IF !FILE(gcWorkDir+lcStyIndex)
        INDEX ON Fabric TAG (lcStyIndex) OF (gcWorkDir+lcStyIndex+'.CDX')
      ENDIF
      SET ORDER TO TAG (lcStyIndex)
      
      *SET FILTER TO FABRIC = &lcSlctFile..FABRIC      
      *-- scan style file for fabric filter
      *SCAN 
      =SEEK(&lcSlctFile..Fabric,"Style")
      SCAN REST WHILE Fabric = &lcSlctFile..Fabric
      *B803922,1 AAN Create new index in style file and change the scan stat.[End]

        *-- if <style group filter> and <Color Filter> and find this 
        *-- style in order line file.
        IF IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
           IIF(EMPTY(lcSeaExp),.T.,  EVALUATE(lcSeaExp)) AND ;
           IIF(EMPTY(lcDivExp),.T.,  EVALUATE(lcDivExp)) AND ;
           IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ; 
               SEEK(STYLE.STYLE,'ORDLINE')
          SELECT ORDLINE 
          *-- scan ordline for the rest of this style.
          SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = ;
                   STYLE.STYLE
            *-- if <ordhdr filter> and <ordline filter>
            *-- Add the lcRpEdiFlt to the expression.
  
            *B803648,1 Add feature to print Bid and Complete Status [Begin]
            *IF CORDTYPE = 'O' AND TotQty > 0 AND ;
            *  EVALUATE(lcStatusEx) AND ;
            *  IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
            *  (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
            *  (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
            *  (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
            *  IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
            *  IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
            *  IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)

            *C101995,1 1) Adding the Account Expression [Begin]
            *IF CORDTYPE = 'O' AND ;
            *  EVALUATE(lcStatusEx) AND ;
            *  IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
            *  (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
            *  (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
            *  (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
            *  IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
            *  IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
            *  IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
            IF CORDTYPE = 'O' AND ;
              EVALUATE(lcStatusEx) AND ;
              IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
              (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
              (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
              IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
              &lcPriorExp AND ;
              IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
              &lcAcctExpr
            *C101995,1 1) Adding the Account Expression [End]
            *B803648,1 Add feature to print Bid and Complete Status [End]
              
              *-- If style make or by. 
              IF STYLE.MAKE
                lcFabColor = ALLTRIM(STR(lnFabColoM))
              ELSE
                lcFabColor = ALLTRIM(STR(lnFabColoI))          
              ENDIF
              
              *-- Seek in the BOM file and get the color of the style - fabric color 
              IF SEEK(PADR(LEFT(Style,lnMajorLen),19)+lcFabColor,'BOM')
                *-- If find the style cheak for the color.
                IF  SUBSTR(BOM.cItmmask,lnNonMajSt,lnColorLen) = '******' 
                  *-- If all style color have same as fabric color.
                  IF  BOM.ICLR ='******'
                    lcColor = SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)
                  ELSE  && else it have constant color for all style colors.
                    lcColor = BOM.ICLR
                  ENDIF
                ELSE
                  *-- if styles colors use different fabric color.
                  =SEEK(PADR(LEFT(Style,lnMajorLen),19)+lcFabColor+STYLE.STYLE,'BOM') 
                  lcColor = BOM.ICLR
                ENDIF
              ELSE
                lcColor = SPACE(06)
              ENDIF
              SCATTER MEMVAR MEMO

              *B803648,1 IF Complete status print Booked Qty Because TotQty is equal to Zero [Begin]
              IF ORDHDR.STATUS = "C"
                *-- Get qty from book qty becauce qty =0
                FOR I = 1 To 8
                  lcCount = ALLTRIM(STR(I))
                  m.QTY&lcCount = BOOK&lcCount
                ENDFOR
                m.TOTQTY = TOTBOOK
              ENDIF
              *B803648,1 IF Complete status print Booked Qty Because TotQty is equal to Zero [End]

              m.cTempKey = PADR(STYLE.FABRIC,7) + lcColor +PADR(ORDHDR.REP1,3)
              m.cCurrCode = ORDHDR.cCurrCode
              INSERT INTO (lcWorkFile) FROM MEMVAR
            ENDIF    && end if <ordhdr filter> and <ordline filter>  
          ENDSCAN    && end scan ordline for the rest of this style.
        ENDIF        && end if <style group filter> and find this style in order line file.
      ENDSCAN        && end scan style file for fabric filter
    ENDSCAN
    *-- Refilter style file [begin]
    SELECT STYLE
    SET FILTER TO
    *-- Refilter style file [end]
    *-- Restore style relation.
    SELECT ORDLINE
    SET RELATION TO style INTO Style ADDITIVE         && To style file.
    lcMastFile = lcWorkFile
  ELSE  && user does not use any select type.
    *-- if user does not select any orders [no data found],
    *-- in this case we select all file.
    IF (RECCOUNT(lcWorkFile) = 0) AND !llWorkDeal

    *B803922,1 AAN Enhance the performance when the user select one account[Begin]
    SELECT OrdLine
    SET RELATION OFF INTO ORDHDR  && break relation.
    SET RELATION OFF INTO STYLE  && break relation.
    SET ORDER TO OrdLine
    SELECT OrdHdr
    SET ORDER TO OrdAcct
    SET RELATION TO cOrdType + Order INTO OrdLine
    SELECT OrdLine
    SET RELATION TO style INTO Style ADDITIVE
    * The user selects the Account without Order or Fabric. 
    IF USED(laOgFxFLt[4,6]) 
      SELECT &laOgFxFLt[4,6]
      SCAN
        =SEEK(Account,"OrdHdr")
        IF FOUND()
          SELECT OrdHdr
          SCAN REST WHILE Account+cOrdType+Order = &laOgFxFLt[4,6]..account
          SELECT ORDLINE
          *SET ORDER TO    && To activate rushmore.
          *SCAN FOR CORDTYPE + ORDER + STR(LINENO,6) = 'O'
          SCAN REST WHILE CORDTYPE + ORDER + STR(LINENO,6) = 'O'+OrdHdr.Order
          *B803922,1 AAN Enhance the performance when the user select one account[End]
            *-- if <ordhdr filter>, <ordline filter> and <style group filter> 
            *-- and <Color Filter>.
            *-- Add the lcRpEdiFlt to the expression .

            *B803648,1 Add feature to print Bid and Complete Status [Begin]
            *IF TotQty > 0 AND ;
            *   EVALUATE(lcStatusEx) AND ;
            *   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
            *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
            *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
            *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
            *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
            *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
            *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
            *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
            *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
            *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
            *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))         

            *C101995,1 1) Adding the Account Expression [Begin]
            *IF EVALUATE(lcStatusEx) AND ;
            *   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
            *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
            *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
            *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
            *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
            *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
            *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
            *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
            *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
            *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
            *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
      
            IF EVALUATE(lcStatusEx) AND ;
               IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
               (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
               (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
               IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
               IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
               (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
               IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
               &lcPriorExp AND ;
               IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
               IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
               IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND &lcAcctExpr
            *C101995,1 1) Adding the Account Expression [End]

            *B803648,1 Add feature to print Bid and Complete Status [Begin]

            *-- If style make or by. 
               IF STYLE.MAKE
                 lcFabColor = ALLTRIM(STR(lnFabColoM))
               ELSE
                 lcFabColor = ALLTRIM(STR(lnFabColoI))          
               ENDIF
           
               *-- Seek in the BOM file and get the color of the style - fabric color 
               IF SEEK(PADR(LEFT(Style,lnMajorLen),19)+lcFabColor,'BOM')
                 *-- If find the style cheak for the color.
                 IF SUBSTR(BOM.cItmmask,lnNonMajSt,lnColorLen) = '******' 
                   *-- If all style color have same as fabric color.
                  IF  BOM.ICLR ='******'
                    lcColor = SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)
                  ELSE  && else it have constant color for all style colors.
                    lcColor = BOM.ICLR
                  ENDIF
                ELSE
                  *-- if styles colors use different fabric color.
                  =SEEK(PADR(LEFT(Style,lnMajorLen),19)+lcFabColor+STYLE.STYLE,'BOM') 
                  lcColor = BOM.ICLR
                ENDIF
              ELSE
                lcColor = SPACE(06)
              ENDIF
              SCATTER MEMVAR MEMO
          
              *B803648,1 IF Complete status print Booked Qty Because TotQty is equal to Zero [Begin]
              IF ORDHDR.STATUS = "C"
                *-- Get qty from book qty becauce qty =0
                FOR I = 1 To 8
                  lcCount = ALLTRIM(STR(I))
                  m.QTY&lcCount = BOOK&lcCount
                ENDFOR
                m.TOTQTY = TOTBOOK
              ENDIF
              *B803648,1 IF Complete status print Booked Qty Because TotQty is equal to Zero [End]
          
              m.cTempKey = PADR(STYLE.FABRIC,7) + lcColor +PADR(ORDHDR.REP1,3)
              m.cCurrCode = ORDHDR.cCurrCode
              INSERT INTO (lcTempLine) FROM MEMVAR
            ENDIF    && end if <ordhdr filter>, <ordline filter> and <style group filter>
          ENDSCAN    && end scan file for full index expression (rushmore).
          
          ********
        ENDSCAN
      ENDIF
    ENDSCAN
  *ENDIF
  ELSE
      * The user did not select any one of account, Fabric and order.
      SELECT ORDLINE
      SET ORDER TO    && To activate rushmore.
      SCAN FOR CORDTYPE + ORDER + STR(LINENO,6) = 'O'
        WAIT WINDOW "Collecting data for order # "  + Order   NOWAIT
        **-- if <ordhdr filter>, <ordline filter> and <style group filter> 
        **-- and <Color Filter>.
        **-- Add the lcRpEdiFlt to the expression .

        **B803648,1 Add feature to print Bid and Complete Status [Begin]
        **IF TotQty > 0 AND ;
        **   EVALUATE(lcStatusEx) AND ;
        **   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
        **   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
        **   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
        **   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
        **   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
        **   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
        **   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
        **   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
        **   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
        **   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
        **   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))         

        **C101995,1 1) Adding the Account Expression [Begin]
        **IF EVALUATE(lcStatusEx) AND ;
        **   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
        **   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
        **   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
        **   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
        **   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
        **   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
        **   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
        **   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
        **   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
        **   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
        **   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
      
        IF EVALUATE(lcStatusEx) AND ;
           IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
           (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
           (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
           IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
           IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
           (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
           IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
           &lcPriorExp AND ;
           IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
           IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
           IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND &lcAcctExpr
        **C101995,1 1) Adding the Account Expression [End]

        **B803648,1 Add feature to print Bid and Complete Status [Begin]

           **-- If style make or by. 
           IF STYLE.MAKE
             lcFabColor = ALLTRIM(STR(lnFabColoM))
           ELSE
             lcFabColor = ALLTRIM(STR(lnFabColoI))          
           ENDIF
           
          **-- Seek in the BOM file and get the color of the style - fabric color 
          IF SEEK(PADR(LEFT(Style,lnMajorLen),19)+lcFabColor,'BOM')
            **-- If find the style cheak for the color.
            IF SUBSTR(BOM.cItmmask,lnNonMajSt,lnColorLen) = '******' 
              **-- If all style color have same as fabric color.
              IF  BOM.ICLR ='******'
                lcColor = SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)
              ELSE  && else it have constant color for all style colors.
                lcColor = BOM.ICLR
              ENDIF
            ELSE
              **-- if styles colors use different fabric color.
              =SEEK(PADR(LEFT(Style,lnMajorLen),19)+lcFabColor+STYLE.STYLE,'BOM') 
              lcColor = BOM.ICLR
            ENDIF
          ELSE
            lcColor = SPACE(06)
          ENDIF
          SCATTER MEMVAR MEMO
          
          **B803648,1 IF Complete status print Booked Qty Because TotQty is equal to Zero [Begin]
          IF ORDHDR.STATUS = "C"
            **-- Get qty from book qty becauce qty =0
            FOR I = 1 To 8
              lcCount = ALLTRIM(STR(I))
              m.QTY&lcCount = BOOK&lcCount
            ENDFOR
            m.TOTQTY = TOTBOOK
          ENDIF
          **B803648,1 IF Complete status print Booked Qty Because TotQty is equal to Zero [End]
          
          m.cTempKey = PADR(STYLE.FABRIC,7) + lcColor +PADR(ORDHDR.REP1,3)
          m.cCurrCode = ORDHDR.cCurrCode
          INSERT INTO (lcTempLine) FROM MEMVAR
        ENDIF    && end if <ordhdr filter>, <ordline filter> and <style group filter>
      ENDSCAN    && end scan file for full index expression (rushmore).
     *B803922,1 Close if stat.[Begin]
     ENDIF
     *B803922,1 Close if stat.[End]
      SELECT ORDLINE
      SET ORDER TO ORDLINE IN ORDLINE
      lcMastFile = lcTempLine
    ENDIF          && end if user does not select any orders [no data found],
  ENDIF            && end if User does not select orders but use select type.
ENDIF  && end If User select data by any select case, beside selecting orders. 

*-- if report master file is the work file and index 
*-- does not match sort by case, reindex data. 
SELECT STYLE
SET RELATION OFF INTO FABRIC  && break relation.
IF (lcMastFile = lcWorkFile)
  SELECT (lcWorkFile)
  INDEX ON &lcIndexTg TAG (lcWorkFile)
ENDIF
*-- End of lfCollData.
*!*************************************************************
*! Name      : lfCurrPrnt
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Compute Currency symbol to print.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCurrPrnt()
*!*************************************************************
*!
FUNCTION lfCurrPrnt
PRIVATE lcCurrCode
*-- Not Multi Currency Or it is and any Equavelent method.
IF !llMultCurr OR lcRpCurr <> "F"
  lcCurrCode = [gcBaseCurr]
ELSE && Multi Currency and Print forign currency.
  lcCurrCode = [Ordhdr.cCurrCode]
ENDIF
RETURN lcCurrCode

*-- End of lfCurrPrnt.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
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

*-- End of lfItmPos.
*!*************************************************************
*! Name      : lfCollTime
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/22/2000
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
*
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)

*-- End of lfCollTime.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/30/2000
*! Purpose   : Display Account Inlist 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
*C101995,1 1) Adding the Inlist valid function
FUNCTION lfsrAcc
PARAMETERS lcParm
PRIVATE lcOldAlias
IF lcParm = 'S'
  lcOldAlias = ALIAS()
  SELECT Customer
  LOCATE
  SELECT (lcOldAlias) 
ENDIF
*-- End of lfsrAcc.

*!***************************************************************************
*! Name      : lpGenExp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/30/2000
*! Purpose   : Generate Account Expression.
*!***************************************************************************
*! Example   : DO lpGenExp
*!***************************************************************************
*
PROCEDURE lpGenExp
PRIVATE lcAlias
lcAlias = ALIAS()
    
*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1]
=ACOPY(laOGFxFlt,laTempExpr)

*-- If user selected Account 
IF USED(laTempExpr[lnAcctPos,6]) AND RECCOUNT(laTempExpr[lnAcctPos,6]) > 0  
  PRIVATE lcBrowPos

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy all Browse Filter array which is used in case of INLIST 
  *-- to another temp array to save the old value.
  DIMENSION laBrTmpFlt[1]
  =ACOPY(laBrFldFlt,laBrTmpFlt)

  *-- resize Browse filter array to hold one browse expression only.
  DIMENSION laBrFldFlt[1,5]
  laBrFldFlt = ""

  *-- Copy the Account expression to laOGFxFlt
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnAcctPos,1),7)
  
  *-- Convert numeric position to string.
  lcBrowPos = ALLTRIM(STR(lnAcctPos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
      EXIT
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = 'laOGFxFlt:1' 
  
  *-- Generate expression for Account.
  lcAcctExpr = gfGenFlt('laOGFxFlt',.T.,.T.)
  
  *-- Restore original laBrFldFlt
  DIMENSION laBrFldFlt[1]
  =ACOPY(laBrTmpFlt,laBrFldFlt)      
ENDIF
*-- EndIf of user selected Account 
  
*-- If user selected Priority
IF !EMPTY(laTempExpr[lnPriPos,6])
  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy the Priority expression to laOGFxFlt
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnPriPos,1),7)
  
  *-- If more than one priority selected
  IF "|" $ laOGFxFlt[1,6]
    *-- Generate expression for Account.
    lcPriorExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  ELSE   && else only one priority selected
    lcPriorExp = "ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]"
  ENDIF    
ENDIF
*-- EndIf of user selected Priority 

*-- Restore original laOGFxFlt
DIMENSION laOGFxFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)

SELECT (lcAlias)
*-- End of lpGenExp.

*!***************************************************************************
*! Name      : lfClearRep
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 14/05/2001
*! Purpose   : Function that we call when Close the option grid.
*!***************************************************************************
*! Called from : [Option Grid] < Close > button.
*!***************************************************************************
*! Example     : = lfClearRep()
*!***************************************************************************
*
FUNCTION lfClearRep
SELECT Style
CLOSE INDEXES

IF FILE(gcWorkDir+lcStyIndex+'.CDX')
  ERASE (gcWorkDir+lcStyIndex+'.CDX')
ENDIF
*-- End of lfClearRep.
