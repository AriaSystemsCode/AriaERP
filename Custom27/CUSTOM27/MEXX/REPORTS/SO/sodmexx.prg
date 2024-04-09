*:***************************************************************************
*: Program file  : SoDMexx
*: Program desc. : Customized Order Detail Report for MEXX
*: For Report    : (SoDMexxA.FRX,SoDMexxB.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : C200097
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe ,
*:               : gfADel , gfGetMemVar,gfOptMsg,gfBrows.
*:               : lfGetLogo,lfAdrShift,lfSolSpAdr,lfHeadVar,lfGetNotes,
*:               : lfNoteHead,lfNoteData,lfEndGroup,lfwRepWhen,lfFormName,
*:               : lfvOptMsg,lfwOldVal,lfvOrder,lfClearRep.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : 1- All IF llFrTime Blocks executed one time in the option grid seasson.
*:         :    and if user selection opens any temp. files that may be used later
*:         :    in another selection I take this file open to use untill user choice
*:         :    is to press < Close > button, to save more time. 
*:         : 2- Any variable start by (llCh) means that some thing in 
*:         :    selected critria was changed, you must collect data again.
*:***************************************************************************
*: Example : DO SoDMexx
*:***************************************************************************
*: This Program is a copy from Order Detail Report (SORDDET)
*:***************************************************************************
*:Modifications
*:B602562,1 MAB 02/18/1999 Change all Enable and disable objects to use suppress 
*:B602562,1 Expression (Enhances OG interface and adjust filter selection)
*:B602562,1 Note : due to this fix the code be shorter than old version
*:B602562,1        and some functions no longer used like
*:B602562,1        lfObjState and lfSelcObjs. And other have only 
*:B602562,1        CLEAR READ line.
*:B602532,1 Adjust report sign to print UK sign or any other according
*:B602532,1 to country sign. <SORDDETA.FRX, SORDDETB.FRX>
*:E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.
*:B602590,1 MAB 03/31/1999 Adjust currency symbol in Printing.
*:B602590,1                Note: I adjust all filter expressions adding llMultCurr condition
*:B602590,1                      to them without documenting them.
*:B802101,1 MAB 04/01/1999 Adjust color filter to depend to variable postions.
*:B802113,1 MAB 04/01/1999 All in list codes changed to be $ not INLIST function, like we done in gfopgrid.
*:B802122,1 MAB 04/05/1999 Set Relation with customer file for order in list.
*:B802077,1 MAB 04/05/1999 Fix Both SORDDETA.FRX and adjust order record in syrepuvr file.
*:B802246,1 SSE 05/25/1999 1) Set Default of "Print Size Scale" in OG to Yes instead of No
*:                         2) In layout, Customer PO# field need to be closer to its title "Cust PO#"
*:                         3) in Detail Form, a Second Sort lcRpSrt2 appears when  
*:                            lcRpSortBY is made on ACCOUNT,ORDER,LOCATION,SALESREP 
*:C101557,1 HDM 06/20/1999 Add Order Category to Report filter.    
*:E301265,1 MAB 06/21/1999 Make text format from this report.
*:E301272,1 MAB 06/21/1999 Full multi currency format.
*:C101569,1 MAB 06/29/1999 Add sort by store if first sort is by order.
*:B802418,1 MAB 07/27/1999 Adjust array dimensions.
*:B802444,1 MAB 07/27/1999 Fix Complete date bug.
*:B802429,1 MAB 07/27/1999 Fix Filter Bug.
*:E500271,4 SSE   07/14/1999 Add popup in OG to filter for the flag field named
*:E500271,4                  LEDIORDER which indicate whether these Orders is coming
*:E500271,4                  from EDI Module or not 
*:B603104,1 MAB 08/10/1999 Avoid duplicate records if user select by style
*:                         and select a range.
*:C200097,1 SSE 12/11/1999 Customized order details report for MEXX, Similar to Standard 
*:C200097,1 SSE            report with the following changes
*:C200097,1 SSE            1) Remove scale code from printed style code
*:C200097,1 SSE            2) Print style description in front of style code
*:C200097,1 SSE            3) Print color description in front of color code
*:C200097,1 SSE            4) Print start & completion date optionally 
*:C200097,1 SSE            5) Columns Price , Piktkt and Diff will be removed
*:C200097,1 SSE            6) style code will be printed in bigger font
*:***************************************************************************
*:           

lcTime     = TIME()                     && Variable to hold the Time
lcStTime   = lcTime                     && Time in which we start collect data.
llPrntBoth = llRpOrdNot AND llRpOrdLnt  && True if you print both types of notes.
lnLastRec  = 2                          && Record No. Of last record in order group.
lcTitle    = ''                         && Title of Note. 
lcNotes    = ''                         && Notes.
llNoIndex  = .F.                        && I don't make index for file.

*E301265,1==E301272,1 Define variables used in Print dos format and multi currency format [Begin
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

lcSeekVal  = ''                         && Get Last record seek value.
llTextMode = (UPPER(ALLTRIM(lcRepMode))=="TEXT")  && Print Text Format

*C101569,1 Add sort by store if first sort is by order [Begin]
lcStorCond = ''
*C101569,1 Add sort by store if first sort is by order [End  ]

*E301265,1==E301272,1 Define variables used in Print dos format and multi currency format [End

*Sameh (SSE) Adding a variable to detect the changes of SCALE in FRX 
*so as not to print it except when it changes 
lcOldScale = SPACE(3)

DIMENSION laStock[9],laWip[9]
STORE '' TO laStock,laWip               && Work process arrays

STORE '' TO lcGrpExp,lcSeaExp,lcDivExp,lcStatusEx,lcCatExp

*-- Show messages in status bar when collecting data. [begin]
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON
*-- Show messages in status bar when collecting data. [begin]

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime
  lcStyTitle = IIF ('GFITEM' $ ALLTRIM(UPPER(lcStyTitle)),;
                    EVALUATE(lcStyTitle),lcStyTitle)  && style title.

  lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.

  *-- Create temporary file that holding order line data. [begin]
  lcWorkFile = gfTempName()
  lcTempLine = gfTempName()

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

  *-- Create temporary file that holding order line data. [end]
  llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.
ENDIF  && end if it's first time you run option Grid.

*-- Create temporary cursors from structure array. [begin]
IF EMPTY(lcMastFile) OR !USED(lcMastFile)

  *-- Setting for report [begin]
  lcSetHour = SET('HOURS')
  SET HOURS TO 24

  *-- Setting for report [end]

  *-- lcNoteLns : Name of Temp. Loop File which is used to print both line notes 
  *--           : and notepad from notepad file.
  *--           : note that this name and temp. file is created 
  *--           : one for every optional grid seasson run.
  lcNoteLns = gfTempName()

  *-- create temp. file that used if you have both types of notes. [begin]
  CREATE CURSOR (lcNoteLns)  (cRecord C(2))
  INDEX ON cRecord TAG (lcNoteLns) OF (gcWorkDir+lcNoteLns)
  FOR lnI = 1 TO 2
    APPEND BLANK
   REPLACE cRecord WITH "N"+ALLTRIM(STR(lnI))
  ENDFOR
  *-- create temp. file that used if you have both types of notes. [end]

  *-- Create work file.
  = lfCreatCur(lcWorkFile)  && Create work cursor.
  = lfCreatCur(lcTempLine)  && Create line cursor.
  
ENDIF
*-- Create temporary cursors from structure array. [end]

= lfGetRepVr()      && Get Report variables such as groups and index.

*-- If user change report critria, Collect report data. 
*E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes. [Begin
*IF llClearFn OR !(lcRpExp == lcLastExpr)   OR ;
*   (llChSelect OR llChStatus OR llChCoord  OR ;
*    llChSumm   OR llChAcc    OR llChStyle  OR ;
*    llChFabric OR llChRep    OR llChOrder  OR ;
*    llChLoc )
IF llClearFn OR llOGFltCh
*E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes. [End

  llClearFn = .F.

  lcStartSt = DTOS(CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],1,;
                  ATC('|',laOGFxFlt[lnStartPos,6])-1)))
  lcStartEd = DTOS(CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],;
                   ATC('|',laOGFxFlt[lnStartPos,6])+1)))

  *B802444,1 Depend on both sides Flag when collecting data [Begin]
  llSrtSides = EMPTY(ALLTRIM(lcStartSt+lcStartEd))
  *B802444,1 Depend on both sides Flag when collecting data [End  ]

  lcCompSt  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],1,;
                   ATC('|',laOGFxFlt[lnCompPos,6])-1)))
  lcCompEd  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],;
                   ATC('|',laOGFxFlt[lnCompPos,6])+1)))

  *B802444,1 Depend on both sides Flag when collecting data [Begin]
  llCmpSides = EMPTY(ALLTRIM(lcCompSt + lcCompEd))
  *B802444,1 Depend on both sides Flag when collecting data [End  ]
  
  lcStatusEx = [ORDHDR.STATUS $ lcRpStatus]
  
  *B802113,1 All in list codes Changed to be $ not INLIST function
  *B802113,1 1- Style group. [Begin]
  *-- if user select Style group, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnGrpPos,6])
    *lnPipeNum  = OCCUR('|',laOGFxFlt[lnGrpPos,6])
    *lcGrpExp   = IIF(lnPipeNum = 0 , '"' + laOGFxFlt[lnGrpPos,6] + '"' ,;
    *             lfPipeExpr(laOGFxFlt[lnGrpPos,6],lnPipeNum))
    
    *lcGrpExp   = 'INLIST(STYLE.CSTYGROUP,' + lcGrpExp + ')'
    lcGrpExp  = "&laOGFxFlt[lnGrpPos,1]." + ' $ laOGFxFlt[lnGrpPos,6]'
  ENDIF  && end if user select Style group, evaluate its expression.
  *B802113,1 1- Style group. [End  ]

  *B802113,1 All in list codes Changed to be $ not INLIST function
  *B802113,1 2- Season. [Begin]
  *-- if user select Season, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnSeaPos,6])
    *lnPipeNum  = OCCUR('|',laOGFxFlt[lnSeaPos,6])
    *lcSeaExp   = IIF(lnPipeNum = 0 , '"' + laOGFxFlt[lnSeaPos,6] + '"' ,;
    *             lfPipeExpr(laOGFxFlt[lnSeaPos,6],lnPipeNum))
    
    *lcSeaExp   = 'INLIST(ORDHDR.SEASON,' + lcSeaExp + ')'
    lcSeaExp  = "&laOGFxFlt[lnSeaPos,1]." + ' $ laOGFxFlt[lnSeaPos,6]'
  ENDIF  && end if user select Season, evaluate its expression.
  *B802113,1 2- Season. [End  ]

  *B802113,1 All in list codes Changed to be $ not INLIST function
  *B802113,1 3- Division. [Begin]
  *-- if user select Division, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnDivPos,6])
    *lnPipeNum  = OCCUR('|',laOGFxFlt[lnDivPos,6])
    *lcDivExp   = IIF(lnPipeNum = 0 , '"' + laOGFxFlt[lnDivPos,6] + '"' ,;
    *             lfPipeExpr(laOGFxFlt[lnDivPos,6],lnPipeNum))
    
    *lcDivExp   = 'INLIST(ORDHDR.CDIVISION,' + lcDivExp + ')'
    lcDivExp  = "&laOGFxFlt[lnDivPos,1]." + ' $ laOGFxFlt[lnDivPos,6]'
  ENDIF  && if user select Division, evaluate its expression.
  *B802113,1 3- Division. [End  ]
  
  *C101557,1 ,Add Order Category to filter. [Begin]
  IF !EMPTY(laOGFxFlt[lnCatPos,6])
    lcCatExp  = "&laOGFxFlt[lnCatPos,1]." + ' $ laOGFxFlt[lnCatPos,6]'
  ENDIF
  *C101557,1 ,Add Order Category to filter. [End  ]
  
  *-- Evaluate Color/Free Expression. [begin]
  *-- Note that: We use either Only This XXX color object or direct XXX 
  *--            Free object, and you must know that both types of 
  *--            expressions can't be enable at the same time.

  *B802101,1 Adjust color filter to depend to variable postions. [Begin]
  * lcListExp = ''
  *-- if Style Non major have Color segment.
  * IF !EMPTY(laOGFxFlt[lnClrSgPos,6]) 
  *   lcListExp = IIF(OCCUR('|',laOGFxFlt[lnClrSgPos,6]) = 0,;
  *               PADR(laOGFxFlt[lnClrSgPos,6],6),;
  *               lfMakeExpr(laOGFxFlt[lnClrSgPos,6]))
  * ENDIF  && end if Style have Color segment.
  
  *-- if Style  Non major does not have Color segment 
  *-- but have free segment(S).
  * IF !EMPTY(laOGFxFlt[lnFreSgPos,6])
  *   lcListExp = IIF(OCCUR('|',laOGFxFlt[lnFreSgPos,6]) = 0,;
  *               PADR(laOGFxFlt[lnFreSgPos,6],6),;
  *               lfMakeExpr(laOGFxFlt[lnFreSgPos,6]))
  * ENDIF  && end if Style  Non major does not have Color segment.

  *-- lcCrFrExp : Color Or free seg. expr.
  *-- if you have Style non major Coler or free segment.
  
  *lcCrFrExp = IIF(EMPTY(lcListExp),'',[SUBSTR(STYLE.STYLE,9,6) $ lcListExp])

  lcCrFrExp = ''
  IF EMPTY(laOGFxFlt[lnClrSgPos,6]) 
    IF !EMPTY(laOGFxFlt[lnFreSgPos,6])
      lcCrFrExp  = "&laOGFxFlt[lnFreSgPos,1]." + ' $ laOGFxFlt[lnFreSgPos,6]'
    ENDIF
  ELSE
    lcCrFrExp  = "&laOGFxFlt[lnClrSgPos,1]." + ' $ laOGFxFlt[lnClrSgPos,6]'
  ENDIF
  
  * lcCrFrExp  = IIF(EMPTY(lcListExp),'',IIF(EMPTY(laOGFxFlt[lnClrSgPos,6]),;
  *            "&laOGFxFlt[lnFreSgPos,1].","&laOGFxFlt[lnClrSgPos,1].") + ' $ lcListExp')
  *B802101,1 Adjust color filter to depend to variable postions. [End  ]

  *-- Evaluate Color/Free Expression. [end]
  lcLastExpr = lcRpExp   && To later know that user change critria.

  = lfScanData()  && Scan around master file.
  
  *-- Unrise all Critria variables.
  STORE .F. TO llChSelect,llChStatus,llChCoord,llChSumm,;
               llChAcc,llChStyle,llChFabric,llChRep,llChOrder,llChLoc

ELSE  &&  user does not change report critria.

  *-- if user Sort By .
  IF lcLastTag != lcIndexTg
    SELECT (lcMastFile)
    INDEX ON &lcIndexTg TAG (lcMastFile)
    lcLastTag = lcIndexTg  && To later know that user change Sort case.
  ENDIF		&& end if user Sort By . 
ENDIF       && end If user change report critria, Collect report data. 

SELECT OrdLine
SET RELATION OFF INTO ORDHDR  && break relation.
SET RELATION OFF INTO STYLE  && break relation.

*-- If Sort by Sales Rep. , set relation to Primary sales rep. file.
IF lcRpSortBy = 'R'
  SELECT ORDHDR
  SET RELATION TO REP1 INTO SALESREP ADDITIVE
ENDIF  && end If Sort by Sales Rep.

*-- Temporary File relations, in the way that help us in report form [begin]
SELECT (lcMastFile)
SET RELATION TO cOrdType + Order INTO OrdHdr      && To order header file.
SET RELATION TO style INTO Style ADDITIVE         && To style file.
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE   && To scale file.
SET RELATION TO cWareCode INTO WAREHOUS ADDITIVE  && To warehouse file.

lcCustRel = IIF(llRpSummMt,['M' + Account],;
            [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
            
SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.

*-- If sort by style group , set relation to codes file.
IF lcRpSortBy = 'G'
  SET RELATION TO gcAct_Comp+SUBSTR(cTempKey,8,6) INTO CODES ADDITIVE
ENDIF  && end If sort by style group.

*-- If sort by fabric , set relation to fabric file.
IF lcRpSortBy = 'F'
  SET RELATION TO LEFT(cTempKey,7) INTO Fabric ADDITIVE
ENDIF  && end If sort by fabric.

*-- if you print both type of notes.
IF llPrntBoth
  SELECT (lcMastFile)
  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
  SET SKIP TO &lcNoteLns
ENDIF   && end if you print both type of notes.

*-- if you print order notepad, open master file in another alias to 
*-- help us to know what last line in order group to print notepad
*-- after it, note we do this because we print notepad in detail band
*-- not in order group band .

IF llRpOrdNot
  USE (gcWorkDir+lcMastFile) ORDER TAG (lcMastFile) IN 0 AGAIN ALIAS GETLAST
ENDIF  && end if you print order notepad.

lcScaleGrp = IIF(llRpScale,[STYLE.SCALE],[''])  && group to print size scale.

lcRepExpr = [IIF(llPrntBoth,;
             IIF(&lcNoteLns..cRecord = 'N2',RECNO(lcMastFile) = lnLastRec ,.T.),;
             .T.)]    && Report expression.

*-- Select Master report file.
SELECT (lcMastFile)
GO BOTTOM
lnLastOne  = RECNO()
GO TOP    && Refresh Relation

*E301265,1 if dos mode do the following [Begin
IF llTextMode
  STORE ' ' TO lcInnGrpIn,lcOutGrpIn,lcInnGrpOp,lcOutGrpOp

  STORE '' TO lcPrnInnL
  STORE 0 TO lnInnQty1,lnInnQty2,lnInnQty3,lnInnQty4,;
             lnInnQty5,lnInnQty6,lnInnQty7,lnInnQty8,;
             lnInnTtQty,lnGrInnAmt
  STORE 0 TO lnPrnInnQ1,lnPrnInnQ2,lnPrnInnQ3,lnPrnInnQ4,;
             lnPrnInnQ5,lnPrnInnQ6,lnPrnInnQ7,lnPrnInnQ8,;
             lnPrnInnTQ,lnPrnInnAm
  
  *C101569,1 Add sort by store if first sort is by order [Begin]
  llLstMulti = (ORDHDR.MULTI = "Y")
  *C101569,1 Add sort by store if first sort is by order [End  ]

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
*E301265,1 if dos mode do the following [End

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)

*E301265,1 if dos mode do the following [Begin
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
  SELECT ORDHDR
  SET RELATION OFF INTO SALESREP
  SELECT (lcCurSel)
ENDIF

IF llRpOrdNot
  USE IN GETLAST
ENDIF  

SET STATUS BAR &lcStatusBr
RETURN
*-- End of Report Code.


*-- Function section 
*-------------------------------------------
*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Option Grid When function
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!**************************************************************************
*! Example     : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
*-- if it's first time to run the report.
*-- using TYPE of variable instead of global llFirstTime, to control
*-- reset case which does not rise llFirsttime, but restore initial
*-- value for lnVarbEnd and advanced case which keep the variables same.
IF TYPE('lnVarbEnd') = 'C'
  lnVarbEnd = 0

  DECLARE laRpSource[3],laRpTarget[1]
  STORE 'Open'     TO laRpSource[1]
  STORE 'Hold'     TO laRpSource[2]
  STORE 'Canceled' TO laRpSource[3]
  lcRpStatus = 'OHX'
  *-- MAB 05/02/99 Work sheet bug By default all status was selected.. [End  ]

  SET ORDER TO ORDHDR IN ORDHDR      && To use it to validate ORDER   # in option grid.
  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO STYLE IN STYLE        && To use it to validate STYLE   # in option grid.
  SET ORDER TO SALESREP IN SALESREP  && To use it to validate REP     # in option grid.
  SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate LOCATION# in option grid.

  IF 'MA' $ gcCmpModules
    SET ORDER TO FABRIC IN FABRIC      && To use it to validate FABRIC  # in option grid.
  ENDIF  

  lnAccPos   = lfItmPos('CUSTOMER.ACCOUNT')
  lnStyPos   = lfItmPos('STYLE.STYLE')
  lnFabPos   = lfItmPos('FABRIC.FABRIC')
  lnLocPos   = lfItmPos('WAREHOUS.CWARECODE')
  lnRepPos   = lfItmPos('SALESREP.REPCODE')
  lnOrdPos   = lfItmPos('ORDHDR.ORDER')
  lnStartPos = lfItmPos('ORDLINE.START')
  lnCompPos  = lfItmPos('ORDLINE.COMPLETE')
  lnGrpPos   = lfItmPos('STYLE.CSTYGROUP')
  lnSeaPos   = lfItmPos('STYLE.SEASON')
  lnDivPos   = lfItmPos('STYLE.CDIVISION')
  lnPriPos   = lfItmPos('ORDHDR.PRIORITY')
  
  *C101557,1 ,Add Order Category to filter. [Begin]
  lnCatPos   = lfItmPos('ORDHDR.CORDERCAT')
  *C101557,1 ,Add Order Category to filter. [End  ]
  
  lnClrSgPos = lfItmPos('SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)')
  *-- Convert all ceiling functions to use lfItmPos because [End..
  lnFreSgPos = lnClrSgPos + 1

  *B602590,1 Adjust currency symbol [Begin]
  IF llMultCurr
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
    
    lnCurrPos  = lfItmPos('ORDHDR.CCURRCODE')
    
  ENDIF
  *B602590,1 Adjust currency symbol [End  ]

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
  *--MAN End
ENDIF  && END IF you first time enter when function.

*-- Disable/enable By account, style, fabric, location, sales representative. [begin]
*-- note that disable and enable is according to value of laRpFltVal.
STORE .T. TO llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep

*B602562,1 use suppress expression instead of enable/disable [Begin
*= lfSelcObjs(lnAccPos,laRpFltVal[lnAccPos])
*= lfSelcObjs(lnStyPos,laRpFltVal[lnStyPos])

*IF 'MA' $ gcCmpModules
*  = lfSelcObjs(lnFabPos,laRpFltVal[lnFabPos])
*ENDIF
  
*= lfSelcObjs(lnLocPos,laRpFltVal[lnLocPos])
*= lfSelcObjs(lnRepPos,laRpFltVal[lnRepPos])
*-- Disable By account, style, fabric, location, sales representative. [end]
*B602562,1 use suppress expression instead of enable/disable [End..

*C101569,1 Add sort by store if first sort is by order [Begin]
IF lcRpSortBy = "O" AND lcRpKind = "D" AND ALEN(laSort2Des,1) = 2
  DIMENSION laSort2Des[4,1] , laSort2Val[4,1]
  laSort2Des[3,1] = "Store/Line#"
  laSort2Des[4,1] = "Store/" + lcStyMajor
  laSort2Val[3,1] = "T"
  laSort2Val[4,1] = "Y"
  CLEAR READ
ENDIF
*C101569,1 Add sort by store if first sort is by order [End  ]
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfScanData
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Collect report data.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfSumStyle,lfSumMulti
*!**************************************************************************
*! Example   : =lfScanData()
*!**************************************************************************
*! Notes     : To easy understand documentaion of this function
*!           : keep in your mind that 
*!           : 1- <ordhdr filter>  is for : open quantity, order status,; 
*!           :                              season, division and priority. 
*!           : 2- <ordline filter> is for : total quantity, line group,;
*!           :                              start date, complete date.
*!           : 3- <style group filter> is for   : style group
*!           : 4- <Coler Filter>  is for   : Color/Free.
*!           : 5- There is relation between ordline file and both ordhdr and ; 
*!           :    style files, to easy lock its data.  
*!           : 6- Because we have a field called cTempKey in temp. files
*!           :    we fill its memory(m.cTempKey) with the required data
*!           :    that help us collecting data rush more and fast printing
*!           :    in the report without evaluating values that spent time. 
*!           : 7- lcSeekExp, is expression we sum rush more for it in case 
*!           :    of summarize multi store orders.  
*!**************************************************************************
*
FUNCTION lfScanData
*-- If you find any data (i.e: not first time you run), clear it. 
IF RECCOUNT(lcTempLine) > 0
  *-- We need temp. files to be files not cursor to open it in another alias [Begin]
  *USE IN (lcTempLine)
  *= lfCreatCur(lcTempLine)  && Create line cursor again.
  SELECT (lcTempLine)
  ZAP

  *E301265,1==E301272,1 Rest any relation before data collection.
  SET RELATION TO
  
  *-- We need temp. files to be files not cursor to open it in another alias [End  ]
  llNoIndex = .T.
ENDIF		&& end If you find any data.

*-- Change index due to changes to Sort By type.
IF llNoIndex OR (lcLastTag != lcIndexTg)
  SELECT (lcTempLine)
  INDEX ON &lcIndexTg TAG (lcTempLine)
  IF llNoIndex
    llNoIndex = .F.
  ELSE  
    lcLastTag = lcIndexTg
  ENDIF  
ENDIF		&& end if Change index.  

*-- lcWorkTag : Variable which hold value of working file index.
*--           : Note that putting index this way help us in collecting
*--           : data rush more.
lcWorkTag = IIF(lcRpSelcBy = 'A',[ACCOUNT+CORDTYPE+ORDER+STR(LINENO,6)]                ,;
            IIF(lcRpSelcBy = 'S',[STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)]   ,;
            IIF(lcRpSelcBy = 'F',[LEFT(cTempKey,7) +STYLE+CORDTYPE+ORDER+STR(LINENO,6)],;
            IIF(lcRpSelcBy = 'W',[CWARECODE+CORDTYPE+ORDER+STR(LINENO,6)]              ,;
            IIF(lcRpSelcBy = 'R',[RIGHT(cTempKey,3)+CORDTYPE+ORDER+STR(LINENO,6)]      ,;
            lcIndexTg)))))

SELECT (lcWorkFile)
IF RECCOUNT(lcWorkFile) > 0
  *-- We need temp. files to be files not cursor to open it in another alias [Begin]
  *USE IN (lcWorkFile)
  *= lfCreatCur(lcWorkFile)  && Create work cursor again.
  SELECT (lcWorkFile)
  ZAP

  *E301265,1==E301272,1 Rest any relation before data collection.
  SET RELATION TO
  *-- We need temp. files to be files not cursor to open it in another alias [End  ]
  
  SELECT (lcWorkFile)
ENDIF
INDEX ON &lcWorkTag TAG (lcWorkFile)

*-- Relation with master order file to help data collecting. [begin]
SELECT OrdLine
SET RELATION TO cOrdType + Order INTO OrdHdr
SET RELATION TO style INTO Style ADDITIVE
*-- Relation with master order file to help data collecting. [end]

*-- llWorkDeal : Flag to know that we start dealing with work file.
*-- llLineDeal : Flag to know that we deal with temp. line file.
STORE .F. TO llWorkDeal , llLineDeal

*MAB HERE [Begin]
*lnOrdTrueP = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ORDER'),1)
*lcTOrdFile = laFxFltCur[ASUBSCRIPT(laFxFltCur,ASCAN(laFxFltCur,lnOrdTrueP),1),2]  && Name of order cursor (inlist func.).
*-- if you find order cursor. 
*IF !EMPTY(lcTOrdFile) AND USED(lcTOrdFile)
*  SELECT (lcTOrdFile)
*  COUNT TO lnHaveRecs FOR !DELETED()
*  llWorkDeal = (lnHaveRecs > 0)  && .T. is there is data.
*ENDIF
lcTOrdFile = laOGFxFlt[lnOrdPos,6]
llWorkDeal = !EMPTY(lcTOrdFile) AND USED(lcTOrdFile) AND RECCOUNT(lcTOrdFile) > 0
*MAB HERE [End  ]

*-- If user select specific orders, collect data of this orders only. [begin]
IF llWorkDeal
  SELECT (lcTOrdFile)
  *-- Scan order cursor.
  SCAN  
    SELECT ORDLINE
    SET ORDER TO TAG ORDLINE
    *-- if find first order record in ordline file and <ordhdr filter>
    *E500271,4 add the lcRpEdiFlt to the expression [Begin.] 
    IF SEEK('O'+&lcTOrdFile..ORDER) AND;
       EVALUATE(lcStatusEx) AND ;
       (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
       IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
       IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
       IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
    *E500271,4 add the lcRpEdiFlt to the expression [End.] 

      *-- Scan ordline file for rest order data.
      SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+&lcTOrdFile..Order
        
        *-- if <ordline filter> and <style group filter> and <Color Filter>
        *-- insert this data into workfile.
        IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
           (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
           (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
           IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
           IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
           IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
           IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))

          SCATTER MEMVAR MEMO
          m.cTempKey  = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
          m.cCurrCode = ORDHDR.cCurrCode

          INSERT INTO (lcWorkFile) FROM MEMVAR
        ENDIF  && end if <ordline filter>.
      ENDSCAN  && end Scan ordline file for rest order data.
    ENDIF      && end if find first order record in ordline.
  ENDSCAN      && end Scan order cursor.
  lcMastFile = lcWorkFile
ENDIF  
*-- If user select specific orders, collect data of this orders only. [end]

*-- Know which type of select we use and its position [begin]
lnUsedItem = IIF(lcRpSelcBy = 'A',lnAccPos,IIF(lcRpSelcBy = 'S',lnStyPos,;
             IIF(lcRpSelcBy = 'F',lnFabPos,IIF(lcRpSelcBy = 'W',lnLocPos,;
             IIF(lcRpSelcBy = 'R',lnRepPos,0)))))

*MAB HERE [Begin]
*lnUsedItem = CEILING(ASCAN(laFxFltCur,lnUsedItem)/3)
*-- Know which type of select we use and its position [end]
*lcSlctFile = IIF(lnUsedItem = 0,'',laFxFltCur[lnUsedItem,2])  &&Name of selected cursor (inlist func.).
*-- if you find selected cursor. 
*IF !EMPTY(lcSlctFile) AND USED(lcSlctFile)
*  SELECT (lcSlctFile)
*  COUNT TO lnHaveRecs FOR !DELETED()
*  llLineDeal = (lnHaveRecs > 0)
*ENDIF
IF lnUsedItem > 0
  lcSlctFile = laOGFxFlt[lnUsedItem,6]
  llLineDeal = !EMPTY(lcSlctFile) AND USED(lcSlctFile) AND RECCOUNT(lcSlctFile) > 0
ENDIF  
*MAB HERE [End  ]

llRpStyLoc = (lcRpSelcBy = 'W') AND llLineDeal

*-- If User select data by any select case, beside selecting orders. 
            *-- IMPORT must be good described before add any line in it.
IF RECCOUNT(lcWorkFile) > 0 AND llLineDeal
  
  *B603104,1 Avoid duplicate records [Begin]
  *lcSlctKey = IIF(lcRpSelcBy = 'A',"ACCOUNT"   ,;
              IIF(lcRpSelcBy = 'S',"ALLTRIM(CSTYMAJOR)"     ,;
              IIF(lcRpSelcBy = 'W',"CWARECODE" ,;
              IIF(lcRpSelcBy = 'F',"FABRIC"    ,;
              "REPCODE"))))  && Field which we seek for in workfile.

  lcSlctKey = IIF(lcRpSelcBy = 'A',"ACCOUNT"   ,;
              IIF(lcRpSelcBy = 'S',"PADR(CSTYMAJOR,lnMajorLen)"     ,;
              IIF(lcRpSelcBy = 'W',"CWARECODE" ,;
              IIF(lcRpSelcBy = 'F',"FABRIC"    ,;
              "REPCODE"))))  && Field which we seek for in workfile.
  *B603104,1 Avoid duplicate records [End  ]

  PRIVATE lcScaned
  SELECT (lcSlctFile)
  *-- Scan selected cursor
  SCAN
    
    lcScaned = EVALUATE(lcSlctKey)
    
    *-- if you find seeking critria in work file. 
    IF SEEK(&lcSlctKey,lcWorkFile)
      SELECT (lcWorkFile)
      *-- scan work file for the rest data have the same seek critria.
      *SCAN REST WHILE &lcWorkTag = &lcSlctFile..&lcSlctKey
      SCAN REST WHILE &lcWorkTag = lcScaned
        *-- if Summarize multi store orders.
        IF llRpSummMt
          lcSeekExp = Style + DTOS(Complete) + cordtype + order
          *-- if you do not find this (style + order) in line file, add record for it.
          IF !SEEK(lcSeekExp,lcTempLine)
            SCATTER MEMVAR MEMO
            m.cCurrCode = ORDHDR.cCurrCode

            = lfSumStyle(lcWorkFile,lcSeekExp)  && sum for this style.
            INSERT INTO (lcTempLine) FROM MEMVAR
          ENDIF
        
        ELSE  && normal case, add line to temp. line file.
          SCATTER MEMVAR MEMO
          m.cCurrCode = ORDHDR.cCurrCode

          INSERT INTO (lcTempLine) FROM MEMVAR
        ENDIF  
      ENDSCAN  && end scan work file for the rest data have the same seek critria.
    ENDIF      && end if you find seeking critria in work file.
  ENDSCAN      && end Scan selected cursor.
  lcMastFile = lcTempLine

ELSE  && User either Does not select orders or does not use any select type.

  *-- if User does not select orders but use select type.
  IF llLineDeal
    *-- set files order [begin]
    lcOrdVar = IIF(INLIST(lcRpSelcBy,'S','F'),'ORDLINES','ORDLINE')	
    SET ORDER TO &lcOrdVar IN ORDLINE
    
    *-- if select by account.
    IF lcRpSelcBy = 'A'
      SET ORDER TO ORDACCT IN ORDHDR
    ELSE
      SET ORDER TO ORDHDR IN ORDHDR
    ENDIF
    *-- set files order [end]

    *-- Different select by cases.
    DO CASE
      CASE lcRpSelcBy = 'A'   && Account case
        
        SELECT ORDLINE 
        SET RELATION OFF INTO ORDHDR  && break relation.
        
        SELECT (lcSlctFile)
        *-- scan selected cursor.
        SCAN
          *-- if you find this account in ordhdr file.
          IF SEEK(ACCOUNT,'ORDHDR')
            SELECT ORDHDR
            *-- scan ordhdr file rest for this account. 
            SCAN REST WHILE account+cordtype+order = &lcSlctFile..ACCOUNT
              *-- if order type is 'O' , <ordhdr filter>, and seek for this
              *-- order in order line file.
              *E500271,4 add the lcRpEdiFlt to the expression [Begin.] 
              IF CORDTYPE = 'O' AND ;
                 EVALUATE(lcStatusEx) AND ;
                 (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                 IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                 SEEK('O'+ORDER,'ORDLINE')
              *E500271,4 add the lcRpEdiFlt to the expression [End.] 
                 
                SELECT ORDLINE 
                *-- scan ordline for rest order lines.
                SCAN REST WHILE cordtype+order+STR(lineno,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
                  *-- if <ordline filter> and <style group filter> and <Color Filter>
                  IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                     (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                     (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                     IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
                     IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
                     IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))   AND ;
                     IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))
                  
                    SCATTER MEMVAR MEMO
                    m.cTempKey = PADR(STYLE.FABRIC,7) + ;
                                 PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
                    m.cCurrCode = ORDHDR.cCurrCode

                    INSERT INTO (lcWorkFile) FROM MEMVAR
                  ENDIF  && end if <ordline filter> and <style group filter>
                ENDSCAN  && end scan ordline for rest order lines.
              ENDIF      && end if order type is 'O' , <ordhdr filter>.
            ENDSCAN      && end scan ordhdr file rest for this account.
          ENDIF          && end if you find this account in ordhdr file.
        ENDSCAN  
 
        *-- Set relation again.
        SET ORDER TO ORDHDR IN ORDHDR
        SELECT ORDLINE 
        SET RELATION TO cOrdType + Order INTO OrdHdr

      CASE lcRpSelcBy = 'S'   && Style case.

        SELECT (lcSlctFile)
        SCAN
          *-- if you find this style in ordline file and <style group filter>

          *B603104,1 Avoid duplicate records [Begin]
          *IF SEEK(CSTYMAJOR,'ORDLINE') AND ;
          *   IIF(EMPTY(lcGrpExp),.T., EVALUATE(lcGrpExp)) AND ;
          *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
          *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))
          IF SEEK(PADR(CSTYMAJOR,lnMajorLen),'ORDLINE') AND ;
             IIF(EMPTY(lcGrpExp),.T., EVALUATE(lcGrpExp)) AND ;
             IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
             IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))
          *B603104,1 Avoid duplicate records [End  ]
             
            SELECT ORDLINE
            *-- scan ordline for the rest of this style.

            *B603104,1 Avoid duplicate records [Begin]
            *SCAN REST WHILE STYLE+DTOS(complete)+cordtype+order+store+STR(lineno,6) = &lcSlctFile..CSTYMAJOR
            SCAN REST WHILE STYLE+DTOS(complete)+cordtype+order+store+STR(lineno,6) = PADR(&lcSlctFile..CSTYMAJOR,lnMajorLen)
            *B603104,1 Avoid duplicate records [End  ]

              *-- if <ordhdr filter> and <ordline filter> and <Color Filter>
              *E500271,4 add the lcRpEdiFlt to the expression [Begin.] 
              IF CORDTYPE = 'O' AND TotQty > 0 AND ;
                 EVALUATE(lcStatusEx) AND ;
                 IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                 IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
              *E500271,4 add the lcRpEdiFlt to the expression [End.] 
                
                SCATTER MEMVAR MEMO
                m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
                m.cCurrCode = ORDHDR.cCurrCode

                INSERT INTO (lcWorkFile) FROM MEMVAR
              
              ENDIF      && end if <ordhdr filter> and <ordline filter>
            ENDSCAN      && end scan ordline for the rest of this style.
          ENDIF          && end if you find this style in ordline file and <style group>
        ENDSCAN

      CASE lcRpSelcBy = 'F'   && Fabric case
        
        SELECT ORDLINE
        SET RELATION OFF INTO STYLE  && break relation.

        SELECT (lcSlctFile)
        SCAN
          *-- in this case you can not rushmore data, there is no index in master files. 
          SELECT STYLE
          SET FILTER TO FABRIC = &lcSlctFile..FABRIC
          *-- scan style file for fabric filter
          SCAN 
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
                *E500271,4 add the lcRpEdiFlt to the expression [Begin.] 
                IF CORDTYPE = 'O' AND TotQty > 0 AND ;
                   EVALUATE(lcStatusEx) AND ;
                   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
                *E500271,4 add the lcRpEdiFlt to the expression [End.] 

                  SCATTER MEMVAR MEMO
                  m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
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

      CASE lcRpSelcBy = 'W'   && Location case

        SELECT ORDLINE 
        SET RELATION OFF INTO ORDHDR  && break relation.

        SELECT (lcSlctFile)
        SCAN
          SELECT ORDHDR

          *-- filter to cwarecode and <ordhdr filter>
          *E500271,4 add the lcRpEdiFlt to the expression [Begin.] 
          SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
                        CORDTYPE = 'O' AND  ;
                        EVALUATE(lcStatusEx) AND ;
                        (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                        IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                        IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
          *E500271,4 add the lcRpEdiFlt to the expression [End.] 
                        
          *-- scan ordhdr for this Location.
          SCAN
            = SEEK('O'+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
            SELECT ORDLINE
            *-- Scan ordline file to get lines of this order
            *-- that evaluate critria.
            SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O' + ORDHDR.ORDER

              *-- if <ordline filter> and <style group filter> and <Color Filter>
              IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
                 IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
                 IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
              
                SCATTER MEMVAR MEMO
                m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
                m.cCurrCode = ORDHDR.cCurrCode

                INSERT INTO (lcWorkFile) FROM MEMVAR
              ENDIF    && end if <ordline filter>
            ENDSCAN
          ENDSCAN      && end scan ordhdr for this sales rep.
        ENDSCAN  

        *-- Refilter ordhdr file [begin]
        SELECT ORDHDR
        SET FILTER TO
        *-- Refilter ordhdr file [end]

        *-- Set relation again.
        SELECT ORDLINE 
        SET RELATION TO cOrdType + Order INTO OrdHdr

      CASE lcRpSelcBy = 'R'   && Sales rep. case

        SELECT ORDLINE 
        SET RELATION OFF INTO ORDHDR  && break relation.

        SELECT (lcSlctFile)
        SCAN
          SELECT ORDHDR

          *B802429,1 Fix Filtering Bug [Begin]
          *          Season and division are belong to OrdLine filter.
          *-- filter to repcode and <ordhdr filter>
          *SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
                        CORDTYPE = 'O' AND ;
                        EVALUATE(lcStatusEx) AND ;
                        IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
                        IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
                        (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                        IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                        IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)

          *E500271,4 add the lcRpEdiFlt to the expression [Begin.] 
          SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
                        CORDTYPE = 'O' AND ;
                        EVALUATE(lcStatusEx) AND ;
                        (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                        (EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
                        (EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
          *E500271,4 add the lcRpEdiFlt to the expression [End.] 
                        
          *B802429,1 Fix Filtering Bug [End  ]
                        
          *-- scan ordhdr for this sales rep.
          SCAN
            = SEEK('O'+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
            SELECT ORDLINE
            *-- Scan ordline file to get lines of this order
            *-- that evaluate critria.
            SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O' + ORDHDR.ORDER

              *-- if <ordline filter> and <style group filter> and <Color Filter>
              IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 (EMPTY(lcSeaExp) OR EVALUATE(lcSeaExp)) AND ;
                 (EMPTY(lcDivExp) OR EVALUATE(lcDivExp)) AND ;
                 IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
              
                SCATTER MEMVAR MEMO
                m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
                m.cCurrCode = ORDHDR.cCurrCode

                INSERT INTO (lcWorkFile) FROM MEMVAR
              ENDIF    && end if <ordline filter>
            ENDSCAN
          ENDSCAN      && end scan ordhdr for this sales rep.
        ENDSCAN  

        *-- Refilter ordhdr file [begin]
        SELECT ORDHDR
        SET FILTER TO
        *-- Refilter ordhdr file [end]

        *-- Set relation again.
        SELECT ORDLINE 
        SET RELATION TO cOrdType + Order INTO OrdHdr

    ENDCASE

    lcMastFile = lcWorkFile

  ELSE  && user does not use any select type.

    *-- if user does not select any orders [no data found],
    *-- in this case we select all file.
    IF (RECCOUNT(lcWorkFile) = 0) AND !llWorkDeal
      SELECT ORDLINE
      SET ORDER TO    && To activate rushmore.

      * IMPORT MUST BE GOOD DESCRIPED BEFORE ADD ANY LINE OF CODE.
      *-- if summarize multi store orders.
      IF llRpSummMt
        *-- we again open ordline in another alias then using it 
        *-- to sum style data, to avoid changing record pointer. 
        USE (gcDataDir+'ORDLINE') AGAIN ALIAS SUMMULTI ORDER TAG ORDLINES IN 0
        
        m.cTempKey = 0  && initially define it to have total amount.

        SELECT ORDLINE
        *-- scan ordline file for full index expression (rushmore)
        SCAN FOR style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = ''
          lcSeekExp = Style + DTOS(Complete) + cordtype + order
          
          *-- if you does not find line in temp line file,;
          *-- and order type is 'O', <ordhdr filter>, <ordline filter> ;
          *-- and <style group filter> and <Color Filter>
          *E500271,4 add the lcRpEdiFlt to the expression [Begin.] 
          IF !SEEK(lcSeekExp,lcTempLine) AND CORDTYPE = 'O' AND ;
             TotQty > 0 AND ;
             EVALUATE(lcStatusEx) AND ;
             IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
             (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
             (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
             IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
             IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
             (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
             IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
             IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
             IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
             IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
             IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
          *E500271,4 add the lcRpEdiFlt to the expression [End.]  
            SCATTER MEMVAR MEMO
            m.cCurrCode = ORDHDR.cCurrCode

            = lfSumMulti(lcSeekExp)  && summarize data.
            INSERT INTO (lcTempLine) FROM MEMVAR
  
          ENDIF    && end if you does not find line in temp line file,
        ENDSCAN    && end scan file for full index expression (rushmore).
        USE IN SUMMULTI

      ELSE  && Normal collection case for all data in ordline file.
        *-- scan ordline file for full index expression (rushmore)
        SCAN FOR CORDTYPE + ORDER + STR(LINENO,6) = 'O'

          *-- if <ordhdr filter>, <ordline filter> and <style group filter> 
          *-- and <Color Filter>.
          *E500271,4 add the lcRpEdiFlt to the expression [Begin.]
          IF TotQty > 0 AND ;
             EVALUATE(lcStatusEx) AND ;
             IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
             (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
             (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
             IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
             IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
             (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
             IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
             IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
             IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
             IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
             IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
          *E500271,4 add the lcRpEdiFlt to the expression [End.]
      
            SCATTER MEMVAR MEMO
            m.cTempKey = PADR(STYLE.FABRIC,7)+PADR(STYLE.CSTYGROUP,6)+PADR(ORDHDR.REP1,3)
            m.cCurrCode = ORDHDR.cCurrCode

            INSERT INTO (lcTempLine) FROM MEMVAR
          
          ENDIF    && end if <ordhdr filter>, <ordline filter> and <style group filter>
        
        ENDSCAN    && end scan file for full index expression (rushmore).
      ENDIF        && end if summarize multi store orders.
      SELECT ORDLINE
      SET ORDER TO ORDLINE IN ORDLINE
      lcMastFile = lcTempLine
    ENDIF          && end if user does not select any orders [no data found],
  ENDIF            && end if User does not select orders but use select type.
ENDIF  && end If User select data by any select case, beside selecting orders. 

* IMPORT MUST BE GOOD DESCRIBED BEFORE ADD ANY LINE OF COOD
*-- if user select by orders only, and want to summarize data.
IF (RECCOUNT(lcTempLine) = 0 AND RECCOUNT(lcWorkFile) > 0) AND llRpSummMt
  SELECT(lcWorkFile)
  SCAN
    lcSeekExp = Style + DTOS(Complete) + cordtype + order
    IF !SEEK(lcSeekExp,lcTempLine)
      SCATTER MEMVAR MEMO
      m.cCurrCode = ORDHDR.cCurrCode

      = lfSumStyle(lcWorkFile,lcSeekExp)
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
*-- End of lfScanData.

*!**************************************************************************
*! Name      : lfvOptMsg
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!**************************************************************************
*! Called from : Option Grid    [Optional Message option]
*!**************************************************************************
*! Calls       : gfOptMsg()
*!**************************************************************************
*! Example     : = lfvOptMsg()
*!**************************************************************************
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[1,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[1,2] = 75                && Line length
= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- End of lfvOptMsg.

*!**************************************************************************
*! Name      : lfwOldVal
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : When function to get the Old value
*!**************************************************************************
*! Called from : Some of the Option Grid fields
*!**************************************************************************
*! Example     : = lfwOldVal()
*!**************************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- End of lfwOldVal.

*!**************************************************************************
*! Name      : lfvSelcBy
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Validate select by option in option grid.
*!           : [Simply it enable and disable selecting buttons]
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfSelcObjs
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Note      : In this function we want to know old value to disable
*!           : last object, and we transfer it to its corressponding 
*!           : character because option grid returns its item number in popup. 
*!**************************************************************************
*! Example   : =lfvSelcBy()
*!**************************************************************************
FUNCTION lfvSelcBy
lcDummy = "Y"
*B602562,1 only function it done is clear read [Begin
*STORE .T. TO llChSelect,llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
llChSelect = .T.

llClearAcc = (lcRpSelcBy # 'A')
llClearSty = (lcRpSelcBy # 'S')
llClearFab = (lcRpSelcBy # 'F')
llClearLoc = (lcRpSelcBy # 'L')
llClearRep = (lcRpSelcBy # 'R')
CLEAR READ
*-- End of lfvSelect.

*!**************************************************************************
*! Name      : lfSelcObjs
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Enable and disable selected objects.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!**************************************************************************
*! Example   : =lfSelcObjs()
*!**************************************************************************
FUNCTION lfSelcObjs
PARAMETERS lnObjNum,lcObjState,llClearVal
IF llClearVal AND (lcObjState = 'D' AND !EMPTY(laOGFxFlt[lnObjNum,6]))
  laOGFxFlt[lnObjNum,6] = ''
ENDIF  
laOGObjCnt[lnObjNum + lnVarbEnd] = (lcObjState = 'E')
= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnObjNum)) + ',6]')  && Enable / Disable Object .
*-- End of lfSelcObjs.

*!**************************************************************************
*! Name      : lfvSortBy
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : 1- Enable and disable some variavle objects due to sort case
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState,lfPreObjs
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Note      : 1- In this function we want to know old value to enable/disable
*!           :    objects due to some sort cases, and we transfer it to 
*!           :    its corressponding character because option grid returns
*!           :    its item number in popup, the idea of enable/disable in
*!           :    this function is to full control printing and do not enable
*!           :    enabled button or disable disabled button.
*!           : 2- In some cases we rise summarization flag to Recollect data again.
*!**************************************************************************
*! Example   : =lfvSortBy()
*!**************************************************************************
FUNCTION lfvSortBy
laOldVal = IIF(laOldVal = 1 , 'A' , IIF(laOldVal = 2 , 'O',;
           IIF(laOldVal = 3 , 'S' , IIF(laOldVal = 4 , 'G',;
           IIF(laOldVal = 5 , 'F' , IIF(laOldVal = 6 , 'W',;
           IIF(laOldVal = 7 , 'R' , 'D')))))))

*C101569,1 Add sort by store if first sort is by order [Begin]
IF lcRpKind = 'D'
  IF lcRpSortBy = "O"

    DIMENSION laSort2Des[4,1] , laSort2Val[4,1]
    laSort2Des[3,1] = "Store/Line#"
    laSort2Des[4,1] = "Store/" + lcStyMajor
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
*C101569,1 Add sort by store if first sort is by order [End  ]

*B602562,1 [Begin]
*B802246,1  this change was made so as when user select SORT#1 (lcRpSortBy)
* and chooses STYLE or FABRIC or STYLE GROUP (S,F,G) , this makes 
* llRpScale = .F. so when choosing something else this will return 
* llRpScale to YES instead of NO 
* llRpScale  = IIF(lcRpSortBy $ 'SFG',.F.,llRpScale)
llRpScale  = IIF(lcRpSortBy $ 'SFG',.F.,.T.)

llRpSummMt = IIF(lcRpSortBy <> 'S' ,.F.,llRpSummMt)
lcRpStyPrn = IIF(lcRpSortBy <> 'S' ,'N',lcRpStyPrn)
llRpOrdNot = IIF(!(lcRpSortBy $ 'AO'),.F.,llRpOrdNot)
CLEAR READ

*B602562,1 [End  ]

*-- Notes [begin]
*-- if In case of sort by (STYLE,FABRIC OR STYLE GROUP) and old value is
*-- another sort type and user want to print sizes, we must disable
*-- reprint scale when diff. because it is the normal print case here,
*-- and vice versa.
*-- Notes [end]

*-- If user want to print size scale
*B602562,1 IF llRpSizes
*B602562,1   IF INLIST(laOldVal,'S','F','G') AND !INLIST(lcRpSortBy,'S','F','G')
*B602562,1     = lfObjState('E',lnScale,'llRpScale')  && Enable Reprint scale when diff.
*B602562,1   ENDIF

*B602562,1   IF !INLIST(laOldVal,'S','F','G') AND INLIST(lcRpSortBy,'S','F','G')
*B602562,1     = lfObjState('D',lnScale,'llRpScale')  && Disable Reprint scale when diff.
*B602562,1   ENDIF
*B602562,1 ELSE
*B602562,1   = lfObjState('D',lnScale,'llRpScale')  && Disable Reprint scale when diff.
*B602562,1 ENDIF    && end If user want to print size scale.

IF lcRpSortBy != laOldVal

  *-- Different sort by cases.
  DO CASE
    CASE lcRpSortBy = 'A'   && Sort by account 

      *-- Enable/disable variable objects due to sort case. [begin]
      *B602562,1 IF laOldVal   = 'S'
      *B602562,1   = lfObjState('D',lnWork,'lcRpStyPrn','N') && Disable Working processing object.
      *B602562,1 ENDIF
      
      *-- if report kind is detail
      IF lcRpKind = 'D'
        *B602562,1 IF laOldVal != 'O'
        *B602562,1   = lfObjState('E',lnNotePad,'llRpOrdNot')    && Enable notepad object.
        *B602562,1 ENDIF
        IF laOldVal = 'S'

          *B602562,1 IF !llRpSummMt AND !llOrdLnDis
          *B602562,1   = lfObjState('E',lnLineNote,'llRpOrdLnt')    && Enable Line notes object.
          *B602562,1 ENDIF

          llChSumm = IIF(llRpSummMt,.T.,llChSumm)  && Rise summarize flag.
          *B602562,1 = lfObjState('D',lnSumm,'llRpSummMt')         && Disable Summarization object.
        ENDIF
      ENDIF
      *-- Enable/disable variable objects due to sort case. [end]

    CASE lcRpSortBy = 'O'    && Sort by order

      *-- Enable/disable variable objects due to sort case. [begin]
      *B602562,1 IF laOldVal   = 'S'
      *B602562,1   = lfObjState('D',lnWork,'lcRpStyPrn','N')      && Disable Working processing line.
      *B602562,1 ENDIF
      
      IF lcRpKind = 'D'
        *B602562,1 IF laOldVal != 'A'
        *B602562,1   = lfObjState('E',lnNotePad,'llRpOrdNot')    && Enable notepad line.
        *B602562,1 ENDIF
        IF laOldVal = 'S'

          *B602562,1 IF !llRpSummMt AND !llOrdLnDis
          *B602562,1   = lfObjState('E',lnLineNote,'llRpOrdLnt')    && Enable Line notes line.
          *B602562,1 ENDIF

          llChSumm = IIF(llRpSummMt,.T.,llChSumm)
          *B602562,1 = lfObjState('D',lnSumm,'llRpSummMt')      && Disable Summarization line.
        ENDIF
      ENDIF
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'S'      && Sort by style

      *B602562,1 = lfObjState('E',lnWork,'lcRpStyPrn')      && Enable Working processing line.
      
      IF lcRpKind = 'D'
        *B602562,1 IF INLIST(laOldVal,'A','O')
        *B602562,1   = lfObjState('D',lnNotePad,'llRpOrdNot')   && Disable notepad line.
        *B602562,1 ENDIF
        
        *B602562,1 IF llRpSummMt AND !llOrdLnDis
        *B602562,1   = lfObjState('D',lnLineNote,'llRpOrdLnt')    && Disable Line notes line.
        *B602562,1 ENDIF
        *B602562,1 = lfObjState('E',lnSumm,'llRpSummMt')      && Enable Summarization line.
      ENDIF
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'G'    && Sort by style group

      *-- Enable/disable variable objects due to sort case. [begin]
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() && Prepair objects.
      ENDIF  
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'F'    && Sort by fabric

      *-- Enable/disable variable objects due to sort case. [begin]
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'W'    && Sort by location

      *-- Enable/disable variable objects due to sort case. [begin]
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'R'    && Sort by sales representative

      *-- Enable/disable variable objects due to sort case. [begin]
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'D'    && Sort by complete date

      *-- Enable/disable variable objects due to sort case. [begin]
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
      *-- Enable/disable variable objects due to sort case. [begin]

  ENDCASE          && end Different sort by cases.
ENDIF
*-- End of lfvSortBy.

*!**************************************************************************
*! Name      : lfPreObjs
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Enable/Disable controled objects in 4 sort cases
*!           : - Style group, Fabric, Sales Rep., Complete date
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!**************************************************************************
*! Example   : =lfPreObjs()
*!**************************************************************************
FUNCTION lfPreObjs

*B602562,1 IF laOldVal   = 'S'
*B602562,1   = lfObjState('D',lnWork,'lcRpStyPrn','N')      && Disable Working processing line.
*B602562,1 ENDIF

IF lcRpKind = 'D'
  *B602562,1 IF INLIST(laOldVal,'A','O')
  *B602562,1   = lfObjState('D',lnNotePad,'llRpOrdNot')    && Disable notepad line.
  *B602562,1 ENDIF
  IF laOldVal = 'S'

    *B602562,1 IF !llRpSummMt AND !llOrdLnDis
    *B602562,1   = lfObjState('E',lnLineNote,'llRpOrdLnt')    && Enable Line notes line.
    *B602562,1 ENDIF

    llChSumm = IIF(llRpSummMt,.T.,llChSumm)
    *B602562,1 = lfObjState('D',lnSumm,'llRpSummMt')      && Disable Summarization line.
  ENDIF
ENDIF
*-- End of lfPreObjs.

*!**************************************************************************
*! Name      : lfObjState
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : This function used to calculate object number and call 
*!           : global show function to enable/disable object due to passed state.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!**************************************************************************
*! Passed Parameters  : 1- ('E' -> enable,'D' disable)
*!                    : 2- Object number
*!                    : 3- Object variable
*!                    : 2- Object value
*!**************************************************************************
*! Example   : =lfObjState()
*!**************************************************************************
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
*-- End of lfObjState.

*!**************************************************************************
*! Name      : lfvSizes
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Control Form name, Enable/disable some objects.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfvSizes()
*!**************************************************************************
FUNCTION lfvSizes
lcRpForm = IIF(llRpSizes,'SoDMexxA','SoDMexxB')
= lfRepPltFr(lcRpForm)
*B602562,1 = lfObjState(IIF(llRpSizes,'E','D'),lnScale,'llRpScale')  && Enable/Disable Reprint scale when diff.

*B802246,1 llRpScale return True after changing llRpSizes to YES again [Begin.]
*llRpScale = IIF(llRpSizes,llRpScale,.F.)
llRpScale = IIF(llRpSizes,.T.,.F.)
*B802246,1 llRpScale return True after changing llRpSizes to YES again [End.]

CLEAR READ
*-- End of lfvSizes.

*!**************************************************************************
*! Name      : lfvKind
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Enable/disable some objects due to report kind (Detail/Summary)
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfvKind()
*!**************************************************************************
FUNCTION lfvKind
*B602562,1
IF lcRpKind = 'S'
  STORE .F. TO llRpSummMt,llRpOrdLnt,llRpOrdNot
  *C101569,1 Default by line No. [Begin]
  lcRpSrt2 = "L"
  *C101569,1 Default by line No. [End  ]
ENDIF
CLEAR READ

*-- End of lfvKind.

*!**************************************************************************
*! Name      : lfvSumm
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Enable/disable Order line notes object
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfvSumm()
*!**************************************************************************
FUNCTION lfvSumm
llRpOrdLnt = IIF(llRpSummMt ,.F.,llRpOrdLnt)
llChSumm   = .T.
CLEAR READ

*B602562,1 IF !llOrdLnDis
*B602562,1   IF llRpSummMt
*B602562,1     = lfObjState('D',lnLineNote,'llRpOrdLnt')    && disable Line notes line.
*B602562,1   ELSE
*B602562,1     = lfObjState('E',lnLineNote,'llRpOrdLnt')    && Enable Line notes line.
*B602562,1   ENDIF
*B602562,1 ENDIF
*-- End of lfvSumm.

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsrvSty()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
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
  OTHERWISE      && Valid code
    *lcAlias = ALIAS()
    *SELECT STYLE
    *LOCATE FOR STYLE.Fabric = Fabric.Fabric
    *llHaveSty = FOUND()
    *-- If no styles found for this fabric
    *IF !llHaveSty
      *-- the following message is
      *-- No styles in fabric group XXX .
      *--           <Ok>
      *= gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
    *ENDIF
    *SELECT (lcAlias)
    *RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
*-- End of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : sum a specific field for the current style in style file
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Returns   : Calculated field value.
*!**************************************************************************
*! Example   : =lfStySum()
*!**************************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

*MAB [Begin]
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
*MAB [Begin]
RETURN INT(lnTotcomp)
*-- End of lfStySum.

*!**************************************************************************
*! Name      : lfSRVFab
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSRVFab()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfFabSum
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : sum a specific field for the current fabric in fabric file
*!**************************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!**************************************************************************
*! Returns : Calculated field value.
*!**************************************************************************
*! Example   : =lfFabSum()
*!**************************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
*MAB 05/11/1999 Avoiding do calculation for empty file [Begin]
*lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')

  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
*MAB 05/11/1999 Avoiding do calculation for empty file [End  ]
RETURN INT(lnTotcomp)
*-- End of lfFabSum.

*!**************************************************************************
*! Name      : lfsrAcc
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- End of lfsrAcc.

*!**************************************************************************
*! Name      : lfsrLoc
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Rise change Location flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsrLoc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
FUNCTION lfsrLoc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChLoc = .T.
    GO TOP IN WAREHOUS
  CASE lcParm = 'R'
    llClearLoc = .F.
ENDCASE
*-- End of lfsrLoc.

*!**************************************************************************
*! Name      : lfsrRep
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Rise change sales rep. flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsrRep()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
FUNCTION lfsrRep
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChRep = .T.
    GO TOP IN SALESREP
  CASE lcParm = 'R'
    llClearRep = .F.
ENDCASE
*-- End of lfsrRep.

*!**************************************************************************
*! Name      : lfSROrder
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Rise change order flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSROrder()
*!**************************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm
llChOrder = .T.
*B802122,1 Set Relation with customer file[Begin]
DO CASE
  CASE lcParm = 'S'

    SELECT ORDHDR
    lcCustRel = IIF(llRpSummMt,['M' + Account],;
                [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])

    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
    GO TOP
  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClearOrd = .F.

ENDCASE
*B802122,1 Set Relation with customer file[End  ]
*-- End of lfsChOrder.

*!**************************************************************************
*! Name      : lfCollTime
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!**************************************************************************
*! Returns            : Spent time.
*!**************************************************************************
*! Example   : =lfCollTime()
*!**************************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- End of lfCollTime.

*!**************************************************************************
*! Name      : lfSumStyle
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Summarize multi store styles using one file for scan and sum.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : sum file, sum expression
*!**************************************************************************
*! Example   : =lfSumStyle()
*!**************************************************************************
FUNCTION lfSumStyle
PARAMETERS lcSumFile,lcSumExpr
*-- initial value for sum variables.
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
           m.Qty7,m.Qty8,m.TotQty,m.cTempKey
lnRecNum = RECNO(lcSumFile)
SUM  Qty1 , Qty2 , Qty3 , Qty4 , Qty5 , Qty6 , Qty7 , Qty8 , TotQty , TotQty*Price ;
 TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,m.cTempKey   ;
 REST WHILE Style + DTOS(Complete) + cordtype + order = lcSumExpr  

m.cTempKey = STR(m.cTempKey,16,2)  && Total amount.
GO lnRecNum IN (lcSumFile)
*-- End of lfSumStyle.

*!**************************************************************************
*! Name      : lfSumMulti
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Summarize multi store styles using two aliass 
*!           : from same file for scan and sum,
*!           : in this case ordline file is used with out any 
*!           ; order to activiate rushmore, thus we open another 
*!           ; alias for make sum in the fastest way.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : sum expression
*!**************************************************************************
*! Example   : =lfSumMulti()
*!**************************************************************************
FUNCTION lfSumMulti
PARAMETERS lcSumExpr
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
           m.Qty7,m.Qty8,m.TotQty,m.cTempKey

SELECT SUMMULTI  && Order line alias (sum for all file)
= SEEK(lcSumExpr)

SUM  Qty1 , Qty2 , Qty3 , Qty4 , Qty5 , Qty6 , Qty7 , Qty8 , TotQty , TotQty*Price ;
 TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty, m.cTempKey   ;
 REST WHILE Style + DTOS(Complete) + cordtype + order = lcSumExpr  

m.cTempKey = STR(m.cTempKey,16,2)
*-- End of lfSumMulti.

*!**************************************************************************
*! Name        : lfGetNotes
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!**************************************************************************
*! Called from : SoDMexxA.FRX, OR SoDMexxB.FRX [Variable lcDum in the report]
*!*************************************************************
*! Example     : = lfGetNotes()
*!**************************************************************************
FUNCTION lfGetNotes
lcTitle = ''     && Title of the note (Line Note OR NotePad).
lcNotes = ''     && Note Data.

*-- if you print both notes.
IF llPrntBoth
  *-- Note that the following Scheme
  *-- ....... cRecord = 'N1' ............. Line Notepad.
  *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
  DO CASE 
    CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcMastFile..Note_Mem))
      lcTitle = 'Order : ' + ORDER + ' - Style : ' + STYLE + ' - Line #  ' + ALLTRIM(STR(LINENO)) + '    Notes.'
      *lcNotes  =  ALLTRIM(Note_Mem) + CHR(10) + CHR(13)
      lcNotes  =  ALLTRIM(Note_Mem) + CHR(10)
    CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcTitle = IIF(RECNO(lcMastFile) = lnLastRec,;
                'Order :' + ORDER + ' Notepad.','')
      *lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10) + CHR(13),'')
      lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10) ,'')
  ENDCASE
ELSE && Else You print either Line or Order/contract Notepad.
  *-- Note that the following Scheme
  *-- ....... llRoOrdLnt ............. Line Notepad.
  *-- ....... llRoOrdNot ............. Order or Contract Notepad.
  DO CASE
    CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcMastFile..Note_Mem))
      lcTitle = 'Order : ' + ORDER + ' - Style : ' + STYLE + ' - Line #  ' + ALLTRIM(STR(LINENO)) + '    Notes.'
      *lcNotes  =  ALLTRIM(Note_Mem) + CHR(10) + CHR(13)
      lcNotes  =  ALLTRIM(Note_Mem) + CHR(10)
    CASE llRpOrdNot AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcTitle = IIF(RECNO(lcMastFile) = lnLastRec,;
                'Order :' + ORDER + ' Notepad.','')
      *lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10) + CHR(13),'')
      lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10),'')
  ENDCASE
ENDIF
RETURN ''
*-- End of lfGetNotes.

*!**************************************************************************
*! Name      : lfLastRec
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Calculate last Record in order details. [ORDER GROUP]
*!           : we use another alias to unchange record pointer of report file.
*!**************************************************************************
*! Called from : [SoDMexxA.FRX OR SoDMexxB.FRX, ORDER GROUP HEADER BAND] 
*!**************************************************************************
*! Example     : = lfLastRec()
*!**************************************************************************
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
*-- End of lfLastRec.

*!*************************************************************
*! Name      : lfvCoorGrp
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Rise change print coordinate groups flag.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfvCoorGrp()
*!*************************************************************
FUNCTION lfvCoorGrp
llChCoord = .T.
*-- End of lfvCoorGrp.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Function that we call when Close the option grid.
*!**************************************************************************
*! Called from : [Option Grid] < Close > button.
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
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

*E301272,1 Restore old currency setting before exit.
IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign
ENDIF
*-- End of lfClearRep.

*!**************************************************************************
*! Name      : lfEvalSegs
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Evaluate NonMajor Type and variables.
*!**************************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!**************************************************************************
*! Example     : = lfEvalSegs()
*!**************************************************************************
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


*B602590,1 Adjust currency symbol [Begin]
*-- if multi currency evaluate currency arrays [Begin]
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
*-- if multi currency evaluate currency arrays [Begin]
*B602590,1 Adjust currency symbol [Begin]

RETURN ''
*-- End of lfEvalSegs.

*!**************************************************************************
*! Name      : lfNMajType
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Mask NonMajor segments .
*!**************************************************************************
*! Called from : lfEvalSegs.
*!**************************************************************************
*! Example     : = lfNMajType()
*!**************************************************************************
FUNCTION lfNMajType
PARAMETERS lcNMajType,lnMajSegs
*-- Loop Around Non Major elements.
FOR lnI = lnMajSegs + 1 TO ALEN(laMajSegs,1)
  *C200097 Calculate the Ext. Size Scale Position , Length and Separator Length
  IF laMajSegs[lnI,1] = 'S'
    lnExtScPos = IIF(lnExtScPos=0,laMajSegs[lnI,4],lnExtScPos)
    lnExtScLen = IIF(lnExtScLen=0,LEN(laMajSegs[lnI,3]),lnExtScLen)
    lnExtScSep = IIF(lnExtScSep=0,LEN(ALLTRIM(laMajSegs[lnI,6])),lnExtScLen)
  ENDIF
  *C200097 Calculate the Ext. Size Scale Position , Length and Separator Length

  IF laMajSegs[lnI,1] = lcNMajType

    lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
    lnNonMajSt = IIF(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSegs[lnI,3],;
                     lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

    lcNonMajTl = IIF(EMPTY(lcNonMajTl),PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                     lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))

  ENDIF

  *C200097,1 In order to remove scale code from printed style code 
  *we have to check Company Extended size scale value so as the For Loop
  *will not Exit after finding the color segment in case of not Extended size
  *-- If you Find Color Type or Find Free Type and current type not Free.  
  IF !llUseExtSc 
    IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
      EXIT
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
  ENDIF
ENDFOR    && end Loop Around Non Major elements.

RETURN !EMPTY(lcFree_Clr)
*-- End of lfNMajType. 

*!**************************************************************************
*! Name      : lfMakeExpr
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Make expression for operator is either BETWEEN or INLIST.
*!**************************************************************************
*! Called From : lfSscanData.
*!**************************************************************************
*! Returns   : Operator expression.
*!**************************************************************************
*! Example   : = lfMakeExpr()
*!**************************************************************************
*:B802101,1 MAB 04/01/1999 This function is no longer in use after fix this bug.
FUNCTION lfMakeExpr
PARAMETERS lcString
PRIVATE lnPipeNo,lcExpr

lnPipeNo = OCCUR('|',lcString)

lcExpr = ''
FOR lnI = 1 TO lnPipeNo
  lcExpr    = IIF(EMPTY(lcExpr),"'" +;
              PADR(SUBSTR(lcString,1,ATC('|',lcString)-1),6) + "'",;
              lcExpr + "," + "'" +;
              PADR(SUBSTR(lcString,1,ATC('|',lcString)-1),6) + "'")
  lcString  = SUBSTR(lcString,ATC('|',lcString)+1)
ENDFOR
RETURN (lcExpr + "," + "'" + PADR(lcString,6) + "'")
*-- End of lfMakeExpr.

*!**************************************************************************
*! Name      : lfGetRepVr
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : 1- Put both index and group expressions for all sort cases.
*!**************************************************************************
*! Called from : Report code
*!**************************************************************************
*! Notes     : 1- lcIndexTg : is master report file index due to sort case.
*!**************************************************************************
*! Example   : = lfGetRepVr()
*!**************************************************************************
FUNCTION lfGetRepVr
*-- lcOutHeadL : Left  title of outer group header.
*-- lcOutHeadR : Right title of outer group header.  
*-- lcInnHeadL : Left  title of inner group header.
*-- lcInnHeadR : Right title of inner group header.
IF lcRpSortBy = 'S'
  lcOutHeadL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen),19) + '  : ']
  
  *C200097,1 Print style description in front of style code [Begin.]
  *lcOutHeadR = [Style.cStyMajor]
  lcOutHeadR = [ALLTRIM(Style.cStyMajor) + '  ' + IIF(lcRpKind='D' AND ;
               lcRpSelcBy='L' AND lcRpSortBy='S',ALLTRIM(Style.Desc),'')]
  *C200097,1 Print style description in front of style code [End.]

  *C200097,1 Remove scale code from printed style code [Begin.]
  IF llUseExtSc 
    lcInnHeadL = [PADR(SUBSTR(lcStyTitle,lnNonMajSt,lnExtScPos-lnNonMajSt-IIF(lnExtScSep>0,lnExtScSep,0)) + ;
                 SUBSTR(lcStyTitle,lnExtScPos+lnExtScLen),19)+ '  : ']
  ELSE
    lcInnHeadL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19)+ '  : ']
  ENDIF  
  *C200097,1 Remove scale code from printed style code [Begin.]
  
  *C200097,1 Print color description in front of color code [Begin.]
  *lcInnHeadR = [SUBSTR(Style,lnMajorLen + 2) + '  ----  ' + ALLTRIM(STYLE.Desc1)]
  lcInnHeadR = [SUBSTR(Style,lnNonMajSt,lnExtScPos-lnNonMajSt-IIF(lnExtScSep>0,lnExtScSep,0)) + '  ----  ' + ;
               gfCodDes(SUBSTR(Style.Style,lnNonMajSt,lnExtScPos-lnNonMajSt-IIF(lnExtScSep>0,lnExtScSep,0)),'COLOR')]
  *C200097,1 Print color description in front of color code [End.]
  
ELSE
  STORE [''] TO lcOutHeadL,lcOutHeadR,lcInnHeadL,lcInnHeadR
ENDIF

*B802246,1 in Case of ACCOUNT,ORDER,LOCATION,SALESREP another Sort by (Sort# 2)
*appear in option grid (Sort by Line#/Style)          

lcLineCurr = lfCurrPrnt()

*-- Different sort by cases.
DO CASE
  CASE lcRpSortBy = 'A'   && Sort by account 

    *B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [Begin.]
    IF lcRpSrt2 = 'L'    && Sort by Line#
      IF llCurInGrp
        lcIndexTg = 'ACCOUNT+CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
      ELSE
        lcIndexTg = 'ACCOUNT+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
      ENDIF  
	ELSE				  && Else Sort by Style	
      IF llCurInGrp
        lcIndexTg = 'ACCOUNT+CCURRCODE+CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
      ELSE
        lcIndexTg = 'ACCOUNT+CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
      ENDIF  
	ENDIF
    *B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [End.]
          
    *-- report variables data account case [begin]
    lcSubTitle = 'Account Number'  && Report sort title
    lcInnGrp   = [ORDER]           && Inner .FRX group field.

    *B802246,1 changing these lines so as in layout, Customer PO# 
    *field need to be closer to its title "Cust PO #"   [Begin.]   
    *lcInnFootL = ['Order # ' + ORDER + '   Cust PO #']
    *lcInnFootR = [IIF(Ordhdr.MultiPo,'*Multi PO*',ALLTRIM(OrdHdr.CustPo)) + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]  && Right title of inner group footer.
    
    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
    IF llTextMode
      lcInnFootL = [ORDER + ', Cust PO#'+] +;
                   [IIF(Ordhdr.MultiPo,'*Multi PO*',PADL(OrdHdr.CustPo,10)) + ' Enter in ' + ALLTRIM(DTOC(OrdHdr.Entered))]

      lcOutFootL = [Customer.Account+' '+PADR(ALLTRIM(CUSTOMER.BtName),15)+"/"+EVALUATE(lcLineCurr)+', ' + TRANSFORM(CUSTOMER.Phone1,lcPhonPict)]
    ELSE
      lcInnFootL = ['Order # ' + ORDER + '     Cust PO #']    
      lcInnFootR = [IIF(Ordhdr.MultiPo,'*Multi PO*', PADL(OrdHdr.CustPo,15)) + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]  && Right title of inner group footer.
	  *B802246,1 [End.]
    
      lcOutFootL = ['Account # ' + Customer.Account]  && Left title of outer group footer.
      lcOutFootR = [PADR(ALLTRIM(CUSTOMER.BtName),25) +"/"+EVALUATE(lcLineCurr)+ ", " + TRANSFORM(CUSTOMER.Phone1,lcPhonPict)]  && Right title of outer group footer.
      *-- report variables data account case [end]
    ENDIF
    IF llCurInGrp
      *man
      *lcSeekVal  = [ACCOUNT+'O'+ORDER]
      lcSeekVal  = [ACCOUNT+cCurrCode+'O'+ORDER]
      lcOutGrp   = [ACCOUNT+CCURRCODE]         && Outer .FRX group field.
    ELSE
      *man
      *lcSeekVal  = [ACCOUNT+cCurrCode+'O'+ORDER]
      lcSeekVal  =  [ACCOUNT+'O'+ORDER]
      lcOutGrp   = [ACCOUNT]         && Outer .FRX group field.
    ENDIF  
    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [End  ]

  CASE lcRpSortBy = 'O'    && Sort by order
    lcSubTitle = 'Order'
    lcInnGrp   = ['']
    lcInnFootL = [''] 

    *C101569,1 Add sort by store if first sort is by order [Begin]
    *B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [Begin.]
    *IF lcRpSrt2 = 'L'    && Sort by Line#
    *  IF llCurInGrp
    *    lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
    *  ELSE
    *    lcIndexTg = 'CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
    *  ENDIF
    *ELSE                  && Else Sort by Style	
    *  IF llCurInGrp
    *    lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
    *  ELSE
    *    lcIndexTg = 'CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
    *  ENDIF  
	*ENDIF
    
    DO CASE
      CASE lcRpSrt2 = 'L'    && Sort by Line#
        IF llCurInGrp
          lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
        ELSE
          lcIndexTg = 'CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
        ENDIF
      CASE lcRpSrt2 = 'S'    && Sort by Style
        IF llCurInGrp
          lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
        ELSE
          lcIndexTg = 'CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
        ENDIF  

      CASE lcRpSrt2 $ 'TY'    
        
        IF llCurInGrp
          lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STORE'
        ELSE
          lcIndexTg = 'CORDTYPE+ORDER+STORE'
        ENDIF
        
        *-- Sort by Store/Line#
        IF lcRpSrt2="T"
          lcIndexTg = lcIndexTg + '+STR(LINENO,6)+STYLE'
        ELSE  && Sort by Store/Style
          lcIndexTg = lcIndexTg + '+STYLE+STR(LINENO,6)'
        ENDIF  
        
        lcStorCond = IIF(llTextMode,[.T.],[ORDHDR.MULTI="Y"])
        lcInnGrp   = [IIF(EVALUATE(lcStorCond),STORE,"")]
        lcInnFootL = [IIF(EVALUATE(lcStorCond),"Store : "+STORE,"")] 

    ENDCASE
    *B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [End.]
    *-- report variables data order case [begin]
    *C101569,1 Add sort by store if first sort is by order [End  ]

    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
    IF llTextMode
      IF lcRpSrt2 $ 'TY'    
        lcOutFootL = [Order+ '/' + EVALUATE(lcLineCurr)+] +;
                     [IIF(ORDHDR.MULTI="Y",',Acct#' + Account,',Stor#' + Store) + ',Entered :' + ALLTRIM(DTOC(OrdHdr.Entered))]
      ELSE
        lcOutFootL = [Order+ '/' + EVALUATE(lcLineCurr)+] +;
                     [',Acct#' + Account + ',Entered in:' + ALLTRIM(DTOC(OrdHdr.Entered))]

      ENDIF               
    
    ELSE
      lcInnFootR = ['']
      IF lcRpSrt2 $ 'TY'    
        lcOutFootL = [EVALUATE(lcLineCurr)+ "/"+'Order # ' + Order + IIF(EVALUATE(lcStorCond),' Account:',' Store :')]
        lcOutFootR = [IIF(EVALUATE(lcStorCond),Account,Store) + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]
      ELSE
        lcOutFootL = [EVALUATE(lcLineCurr)+ "/"+'Order # ' + Order + '    Account # ']
        lcOutFootR = [Account  + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]
      ENDIF  

      *-- report variables data order case [end]
    ENDIF  
    IF llCurInGrp
      lcSeekVal = [cCurrCode+'O'+ORDER]
      lcOutGrp  = [CCURRCODE+ORDER]
    ELSE
      lcSeekVal = ['O'+ORDER]
      lcOutGrp  = [ORDER]
    ENDIF  
    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [end ]

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

*   lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)]

    *-- report variables data style case [begin]
    lcStyTitle = IIF ('GFITEM' $ ALLTRIM(UPPER(lcStyTitle)),;
                      EVALUATE(lcStyTitle),lcStyTitle)
    *C200097,1 Remove scale code from printed style code from Group Header [Begin.] 
    *lcSubTitle = lcStyTitle
    IF llUseExtSc 
      lcSubTitle = SUBSTR(lcStyTitle,1,lnExtScPos-1-IIF(lnExtScSep>0,lnExtScSep,0)) + ;
                   SUBSTR(lcStyTitle,lnExtScPos+lnExtScLen)
    ELSE
      lcSubTitle = lcStyTitle
    ENDIF
    *C200097,1 Remove scale code from printed style code from Group Header [End.]
    
    *C200097,1 Modify Inner Group to be by Color only & not on Non Major[Begin.]
    *lcInnGrp   = [SUBSTR(Style,lnMajorLen + 2)]
    lcInnGrp   = [SUBSTR(Style,lnNonMajSt,lnExtScPos-lnNonMajSt-IIF(lnExtScSep>0,lnExtScSep,0))]
    *C200097,1 Modify Inner Group to be by Color only [End.]

    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
    IF llTextMode
      *C200097,1 Remove scale code from printed style code from Group Footer [Begin.]
      *lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19) + ' : '+]+;
      *             [SUBSTR(Style,lnMajorLen + 2)]
      IF llUseExtSc 
        lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnNonMajSt,lnExtScPos-lnNonMajSt-IIF(lnExtScSep>0,lnExtScSep,0)),19) + ' : '+]+;
                     [SUBSTR(Style,lnNonMajSt,lnExtScPos-lnNonMajSt-IIF(lnExtScSep>0,lnExtScSep,0))]
      ELSE
        lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19) + ' : '+]+;
                     [SUBSTR(Style,lnMajorLen + 2)]
      ENDIF  
      *C200097,1 Remove scale code from printed style code from Group Footer [End.]

      lcOutFootL = [ALLTRIM(Style.cStyMajor) + "/"+EVALUATE(lcLineCurr)+ ", "+ '(' + ALLTRIM(Style.Fabric) + ')']  
    ELSE
      *C200097,1 Remove scale code from printed style code from Group Footer [Begin.]
      *lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19) + '  : ']
      *lcInnFootR = [SUBSTR(Style,lnMajorLen + 2)]
      IF llUseExtSc 
        lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnNonMajSt,lnExtScPos-lnNonMajSt-IIF(lnExtScSep>0,lnExtScSep,0)),19) + '  : ']
        lcInnFootR = [SUBSTR(Style,lnNonMajSt,lnExtScPos-lnNonMajSt-IIF(lnExtScSep>0,lnExtScSep,0))]
 	  ELSE
        lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19) + '  : ']
        lcInnFootR = [SUBSTR(Style,lnMajorLen + 2)]
 	  ENDIF   
      *C200097,1 Remove scale code from printed style code from Group Footer [Begin.]

      lcOutFootL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen),19) + '  : ']
      lcOutFootR = [ALLTRIM(Style.cStyMajor) +"/"+EVALUATE(lcLineCurr)+ ", "+ '( ' + ALLTRIM(Style.Fabric) + ' )']
      *-- report variables data style case [end]
    ENDIF  
    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [end ]

  CASE lcRpSortBy = 'G'    && Sort by style group
    IF llCurInGrp
      lcIndexTg = 'SUBSTR(cTempKey,8,6)+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
      lcSeekVal = [SUBSTR(cTempKey,8,6)+CCURRCODE+STYLE]
      lcOutGrp  = [SUBSTR(cTempKey,8,6)+CCURRCODE]
    ELSE
      lcIndexTg = 'SUBSTR(cTempKey,8,6)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
      lcSeekVal = [SUBSTR(cTempKey,8,6)+STYLE]
      lcOutGrp  = [SUBSTR(cTempKey,8,6)]
    ENDIF  

    *-- report variables data Style group case [begin]
    lcSubTitle = 'Style Group'
    lcInnGrp   = [STYLE]

    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
    IF llTextMode
      lcInnFootL = ['Style (' + Style + ') :'+ALLTRIM(Style.Desc)]
      lcOutFootL = [gfCodDes(SUBSTR(cTempKey,8,6),'CSTYGROUP',.T.)+"/"+EVALUATE(lcLineCurr)]

    ELSE
      lcInnFootL = ['Style  ( ' + Style + ' )  :']
      lcInnFootR = [ALLTRIM(Style.Desc)]
      lcOutFootL = ['Group  ( ' + SUBSTR(cTempKey,8,6) + ' )  :']
      lcOutFootR = [Codes.cDiscrep+"/"+EVALUATE(lcLineCurr)]
      *-- report variables data Style group case [end]
    ENDIF  
    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [end

  CASE lcRpSortBy = 'F'    && Sort by fabric
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

    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
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
    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [end

  CASE lcRpSortBy = 'W'    && Sort by location
    *B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [Begin.]
    IF lcRpSrt2 = 'L'       && Sort by Line#
      IF llCurInGrp
        lcIndexTg = 'CWARECODE+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
      ELSE
        lcIndexTg = 'CWARECODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
      ENDIF  
	ELSE                     && Else Sort by Style	
      IF llCurInGrp
        lcIndexTg = 'CWARECODE+CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'	
      ELSE
        lcIndexTg = 'CWARECODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'	
      ENDIF  
	ENDIF

    IF llCurInGrp
      lcOutGrp   = [CWARECODE+CCURRCODE]
    ELSE
      lcOutGrp   = [CWARECODE]
    ENDIF  
    *B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [End.]
	    
    *-- report variables data location case [begin]
    lcSubTitle = 'Location'
    lcInnGrp   = ['']

    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
    lcInnFootL = [''] 
    IF llTextMode
      lcOutFootL = [cWareCode + ':'+ALLTRIM(Warehous.cDesc)+"/"+EVALUATE(lcLineCurr)]

    ELSE
      lcInnFootR = ['']
      lcOutFootL = ['Location # ' + cWareCode + ' :']
      lcOutFootR = [ALLTRIM(Warehous.cDesc)+"/"+EVALUATE(lcLineCurr)]
      *-- report variables data location case [end]
    ENDIF  
    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [end]

  CASE lcRpSortBy = 'R'    && Sort by sales representative
    *B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [Begin.]
    IF lcRpSrt2 = 'L'       && Sort by Line#
      IF llCurInGrp
	    lcIndexTg = 'RIGHT(cTempKey,3)+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
	  ELSE
	    lcIndexTg = 'RIGHT(cTempKey,3)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
	  ENDIF  
	ELSE                     && Else Sort by Style	
      IF llCurInGrp
		lcIndexTg = 'RIGHT(cTempKey,3)+CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
      ELSE
		lcIndexTg = 'RIGHT(cTempKey,3)+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
      ENDIF		
	ENDIF
    *B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [End.]
    	
    IF llCurInGrp
      lcOutGrp   = [RIGHT(cTempKey,3)+CCURRCODE]
    ELSE
      lcOutGrp   = [RIGHT(cTempKey,3)]
    ENDIF  

    *-- report variables data sales Rep. case [begin]
    lcSubTitle = 'Primary Sales Representative'
    lcInnGrp   = ['']

    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [end
    lcInnFootL = [''] 
    IF llTextMode
      lcOutFootL = [RIGHT(cTempKey,3) + ':'+PADR(SalesRep.Name,21)+"/"+EVALUATE(lcLineCurr)]  

    ELSE
      lcInnFootR = ['']
      lcOutFootL = ['Primary Sales Rep. # ' + RIGHT(cTempKey,3) + ' :']
      lcOutFootR = [SalesRep.Name+"/"+EVALUATE(lcLineCurr)]
    ENDIF  
    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [end]

    *-- report variables data sales Rep. case [end]

  CASE lcRpSortBy = 'D'    && Sort by complete date
    
    IF llCurInGrp
      lcIndexTg = 'DTOS(COMPLETE)+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
      lcOutGrp  = [DTOS(COMPLETE)+CCURRCODE]
    ELSE
      lcIndexTg = 'DTOS(COMPLETE)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
      lcOutGrp  = [DTOS(COMPLETE)]
    ENDIF  

    *-- report variables data Complete date case [begin]
    lcSubTitle = 'Complete Date'
    lcInnGrp   = ['']

    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
    lcInnFootL = [''] 
    IF llTextMode
      lcOutFootL = ["Date Completed : " + DTOC(Complete)+"/"+EVALUATE(lcLineCurr)]
    ELSE  
      lcInnFootR = ['']
      lcOutFootL = ['******']
      lcOutFootR = [DTOC(Complete)+"/"+EVALUATE(lcLineCurr)]
      *-- report variables data Complete date case [end]
    ENDIF
    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [end
  
  *E301272,1 multi currency situation sort by currency.
  CASE lcRpSortBy = 'U'    && Sort by currency

    IF lcRpSrt2 = 'L'    && Sort by Line#
      lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
    ELSE                  && Else Sort by Style	
      lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
	ENDIF
    
    *-- report variables data order case [begin]
    lcSubTitle = 'Currency'
    lcInnGrp   = [ORDER]

    IF llTextMode
      lcInnFootL = [Order+', Account#'+Account  + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))] 
      lcOutFootL = ["Currency : " + lfCurrDesc()]
    
    ELSE
      lcInnFootR = ['Order:']
      lcInnFootL = [Order+', Account#'+Account  + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))] 
      lcOutFootL = ['Currency : ']
      lcOutFootR = [lfCurrDesc()]
      *-- report variables data order case [end]
    ENDIF  

    lcSeekVal = [cCurrCode+'O'+ORDER]
    lcOutGrp  = [CCURRCODE]
    
ENDCASE          && end Different sort by cases.

*E301272,1 if it is currency format and sort by any thing rather than currency.
IF llCurInGrp AND (lcRpSortBy <> 'U')
  lcSubTitle = lcSubTitle+"/Currency"
ENDIF
*-- End of lfGetRepVr.

*!**************************************************************************
*! Name      : lfCreatCur
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Create cursor
*!**************************************************************************
*! Passed Parameters  : Cursor Name
*!**************************************************************************
*! Example   : = lfCreatCur()
*!**************************************************************************
FUNCTION lfCreatCur
PARAMETERS lcCurName
CREATE TABLE (gcWorkDir+lcCurName) ;
   FROM ARRAY laTempStru
*-- End of lfCreatCur.

*!**************************************************************************
*! Name      : lfPipeExpr
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Mask inlist expression.
*!**************************************************************************
*! Called from : Report code
*!**************************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!**************************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!**************************************************************************
*! Example   : = lfPipeExpr('AS|BS|CS',2)
*!**************************************************************************
*:B802113,1 MAB 04/01/1999 This function is no longer in use after fix this bug.
FUNCTION lfPipeExpr
PARAMETERS lcString,lnPipeNo
PRIVATE lcExpr
lcExpr = ''

FOR lnI = 1 TO lnPipeNo
  lcExpr    = IIF(EMPTY(lcExpr),"'" +;
              SUBSTR(lcString,1,ATC('|',lcString)-1) + "'",;
              lcExpr + "," + "'" +;
              SUBSTR(lcString,1,ATC('|',lcString)-1) + "'")
  lcString      = SUBSTR(lcString,ATC('|',lcString)+1)
ENDFOR

RETURN (lcExpr + "," + "'" + lcString + "'")
*-- End of lfPipeExpr.

*!**************************************************************************
*! Name      : lfvOStatus
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!**************************************************************************
*! Called from : Report code
*!**************************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!**************************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!**************************************************************************
*! Example   : = lfvOStatus()
*!**************************************************************************
*
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))

  ENDFOR  && end Loop to make Status expression.
ENDIF

*B802077,1 if empty of status is like select all available values [Begin]
*lcRpStatus = ALLTRIM(lcRpStatus)
lcRpStatus = IIF(EMPTY(lcRpStatus),'OHX',ALLTRIM(lcRpStatus))
*B802077,1 if empty of status is like select all available values [End  ]

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

*!**************************************************************************
*! Name      : lfGetWork
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : - Compute work proccessing
*!**************************************************************************
*! Example   : = lfGetWork()
*!**************************************************************************
FUNCTION lfGetWork
STORE '' TO laStock,laWip,lnStkOrWip
*-- Calculate Wip and Stock Values [Begin]

*-- If User select specific locations 
IF llRpStyLoc
  lcRepAlias = ALIAS()
  SELECT (lcSlctFile)
  STORE 0 TO lnS1,lnS2,lnS3,lnS4,lnS5,lnS6,lnS7,lnS8,lnS9
  STORE 0 TO lnW1,lnW2,lnW3,lnW4,lnW5,lnW6,lnW7,lnW8,lnW9
  *-- Scan for Selected locations only.
  SCAN
    *-- If you find this style location in Stydye file, compute its work proc.
    IF SEEK(&lcMastFile..Style + &lcSlctFile..cWareCode, 'STYDYE')

      SELECT STYDYE
      SUM REST WHILE Style+cWareCode+Dyelot = ;
                     &lcMastFile..Style + &lcSlctFile..cWareCode ;
          Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk,;
          Wip1,Wip2,Wip3,Wip4,Wip5,Wip6,Wip7,Wip8,TotWip ;
      TO  lnStk1,lnStk2,lnStk3,lnStk4,lnStk5,lnStk6,lnStk7,lnStk8,lnTotStk,;
          lnWip1,lnWip2,lnWip3,lnWip4,lnWip5,lnWip6,lnWip7,lnWip8,lnTotWip

      *-- if you print stock or (stock and wip)
      IF INLIST(lcRpStyPrn,'S','P')
        lnS1 = lnS1 + lnStk1 
        lnS2 = lnS2 + lnStk2 
        lnS3 = lnS3 + lnStk3 
        lnS4 = lnS4 + lnStk4 
        lnS5 = lnS5 + lnStk5 
        lnS6 = lnS6 + lnStk6 
        lnS7 = lnS7 + lnStk7 
        lnS8 = lnS8 + lnStk8 
        lnS9 = lnS9 + lnTotStk

      ENDIF  && end if you print stock or (stock and wip)

      *-- if if you print wip or (stock and wip)
      IF INLIST(lcRpStyPrn,'W','P')
        lnW1 = lnW1 + lnWip1 
        lnW2 = lnW2 + lnWip2 
        lnW3 = lnW3 + lnWip3 
        lnW4 = lnW4 + lnWip4 
        lnW5 = lnW5 + lnWip5 
        lnW6 = lnW6 + lnWip6 
        lnW7 = lnW7 + lnWip7 
        lnW8 = lnW8 + lnWip8 
        lnW9 = lnW9 + lnTotWip
      
      ENDIF  && end if you print wip or (stock and wip)
      
    ENDIF  && end If you find this style location in Stydye file.
  ENDSCAN  && end Scan for Selected locations only.
  SELECT (lcRepAlias)

  *-- if you print stock or (stock and wip) and total stock not equal 0
  IF INLIST(lcRpStyPrn,'S','P') AND (lnS9 # 0) 
    laStock[1] = IIF(lnS1 = 0 ,'',TRANSFORM(lnS1,'99999')) 
    laStock[2] = IIF(lnS2 = 0 ,'',TRANSFORM(lnS2,'99999')) 
    laStock[3] = IIF(lnS3 = 0 ,'',TRANSFORM(lnS3,'99999')) 
    laStock[4] = IIF(lnS4 = 0 ,'',TRANSFORM(lnS4,'99999')) 
    laStock[5] = IIF(lnS5 = 0 ,'',TRANSFORM(lnS5,'99999')) 
    laStock[6] = IIF(lnS6 = 0 ,'',TRANSFORM(lnS6,'99999')) 
    laStock[7] = IIF(lnS7 = 0 ,'',TRANSFORM(lnS7,'99999')) 
    laStock[8] = IIF(lnS8 = 0 ,'',TRANSFORM(lnS8,'99999')) 
    laStock[9] = IIF(lnS9 = 0 ,'',TRANSFORM(lnS9,'9999999')) 
  ENDIF

  *-- if you print wip or (stock and wip) and total wip not equal 0
  IF INLIST(lcRpStyPrn,'W','P') AND (lnW9 # 0)
    laWip[1] = IIF(lnW1 = 0 ,'',TRANSFORM(lnW1,'99999')) 
    laWip[2] = IIF(lnW2 = 0 ,'',TRANSFORM(lnW2,'99999')) 
    laWip[3] = IIF(lnW3 = 0 ,'',TRANSFORM(lnW3,'99999')) 
    laWip[4] = IIF(lnW4 = 0 ,'',TRANSFORM(lnW4,'99999')) 
    laWip[5] = IIF(lnW5 = 0 ,'',TRANSFORM(lnW5,'99999')) 
    laWip[6] = IIF(lnW6 = 0 ,'',TRANSFORM(lnW6,'99999')) 
    laWip[7] = IIF(lnW7 = 0 ,'',TRANSFORM(lnW7,'99999')) 
    laWip[8] = IIF(lnW8 = 0 ,'',TRANSFORM(lnW8,'99999')) 
    laWip[9] = IIF(lnW9 = 0 ,'',TRANSFORM(lnW9,'9999999')) 
  ENDIF

  lnStkOrWip = TRANSFORM(lnS9 + lnW9,'999999')  && Calculate wip + stock values
  lnStkOrWip = IIF(VAL(lnStkOrWip) = 0 , '' , lnStkOrWip)

ELSE  && User does not select specific locations.

  IF INLIST(lcRpStyPrn,'S','P')
    laStock[1] = IIF(STYLE.Stk1 = 0,'',TRANSFORM(STYLE.Stk1,'99999'))
    laStock[2] = IIF(STYLE.Stk2 = 0,'',TRANSFORM(STYLE.Stk2,'99999'))
    laStock[3] = IIF(STYLE.Stk3 = 0,'',TRANSFORM(STYLE.Stk3,'99999'))
    laStock[4] = IIF(STYLE.Stk4 = 0,'',TRANSFORM(STYLE.Stk4,'99999'))
    laStock[5] = IIF(STYLE.Stk5 = 0,'',TRANSFORM(STYLE.Stk5,'99999'))
    laStock[6] = IIF(STYLE.Stk6 = 0,'',TRANSFORM(STYLE.Stk6,'99999'))
    laStock[7] = IIF(STYLE.Stk7 = 0,'',TRANSFORM(STYLE.Stk7,'99999'))
    laStock[8] = IIF(STYLE.Stk8 = 0,'',TRANSFORM(STYLE.Stk8,'99999'))
    laStock[9] = IIF(STYLE.TotStk = 0,'',TRANSFORM(STYLE.TotStk,'9999999'))
  ENDIF

  IF INLIST(lcRpStyPrn,'W','P')
    laWip[1] = IIF(STYLE.Wip1 = 0,'',TRANSFORM(STYLE.Wip1,'99999'))
    laWip[2] = IIF(STYLE.Wip2 = 0,'',TRANSFORM(STYLE.Wip2,'99999'))
    laWip[3] = IIF(STYLE.Wip3 = 0,'',TRANSFORM(STYLE.Wip3,'99999'))
    laWip[4] = IIF(STYLE.Wip4 = 0,'',TRANSFORM(STYLE.Wip4,'99999'))
    laWip[5] = IIF(STYLE.Wip5 = 0,'',TRANSFORM(STYLE.Wip5,'99999'))
    laWip[6] = IIF(STYLE.Wip6 = 0,'',TRANSFORM(STYLE.Wip6,'99999'))
    laWip[7] = IIF(STYLE.Wip7 = 0,'',TRANSFORM(STYLE.Wip7,'99999'))
    laWip[8] = IIF(STYLE.Wip8 = 0,'',TRANSFORM(STYLE.Wip8,'99999'))
    laWip[9] = IIF(STYLE.TotWip = 0,'',TRANSFORM(STYLE.TotWip,'9999999'))
  ENDIF

  *-- Calculate Wip and Stock Values [End]

  lnStkOrWip = TRANSFORM(STYLE.TotStk + STYLE.TotWip,'999999')  && Calculate wip + stock values
  lnStkOrWip = IIF(VAL(lnStkOrWip) = 0 , '' , lnStkOrWip)

ENDIF  && end If User select specific locations .

RETURN ''
*-- End of lfGetWork.

*!**************************************************************************
*! Name      : lfWorkEnd
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : - End Compute work proccessing
*!**************************************************************************
*! Called from : Report code
*!**************************************************************************
*! Example   : = lfWorkEnd()
*!**************************************************************************
FUNCTION lfWorkEnd
STORE '' TO laStock,laWip,lnStkOrWip
RETURN ''
*-- End of lfWorkEnd.

*!**************************************************************************
*! Name      : lfArrDumy
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Fill Sort and select arrays
*!**************************************************************************
*! Example   : = lfArrDumy()
*!**************************************************************************
*B802418,1 Adjust array dimensions
*-- Function to fill select by and sort by arrays.
FUNCTION lfArrDumy
PRIVATE lnSelElms , lnSrtElms
lnSelElms = 4
lnSrtElms = 6
DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1],;
          laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1],;
          laSort2Des[2,1],laSort2Val[2,1]

*-- Sort 2 elementes [Begin]
laSort2Des[1,1] = "Line#"
laSort2Des[2,1] = lcStyMajor
laSort2Val[1,1] = "L"
laSort2Val[2,1] = "S"
*-- Sort 2 elementes [End  ]

*-- Sort 1 base elements [Begin]
laSortDesc[1,1] = 'Account'
laSortDesc[2,1] = 'Order'
laSortDesc[3,1] = lcStyMajor
laSortDesc[4,1] = lcStyMajor + ' Group'
laSortDesc[5,1] = 'Primary Sales Representative'
laSortDesc[6,1] = 'Complete Date'

laSortVal[1,1]  = 'A'
laSortVal[2,1]  = 'O'
laSortVal[3,1]  = 'S'
laSortVal[4,1]  = 'G'
laSortVal[5,1]  = 'R'
laSortVal[6,1]  = 'D'
*-- Sort 1 base elements [End  ]

*-- Fill Select by array base elements. [Begin]
laSlctDesc[1,1] = 'Account'
laSlctDesc[2,1] = 'Primary Sales Representative'
laSlctDesc[3,1] = lcStyMajor
laSlctDesc[4,1] = 'All'

laSlctVal[1,1]  = 'A'
laSlctVal[2,1]  = 'R'
laSlctVal[3,1]  = 'S'
laSlctVal[4,1]  = 'L'
*-- Fill Select by array base elements. [End  ]

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

*E301272,1 Add sort by currency if multi currency company.
IF llMultCurr
  lnSrtElms = lnSrtElms + 1
  DIMENSION laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1]
  laSortDesc[ALEN(laSortDesc,1),1] = "Currency"
  laSortVal[ALEN(laSortDesc,1),1]  = "U"
ENDIF
*-- End of lfArrDumy.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Evaluate fixed filter position within array.
*!**************************************************************************
*! Called from : Report code
*!**************************************************************************
*! Returns   : Position
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfvCurr
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : set currency symbol
*!**************************************************************************
*! Called from : Report code
*!**************************************************************************
*! Example   : = lfvCurr()
*!**************************************************************************
*B602590,1
FUNCTION lfvCurr
*PRIVATE lnCurPos
*lnCurPos = EVALUATE(SYS(18))
*SET CURRENCY TO ALLTRIM(laCurrSmbl[lnCurPos,1])
*-- End of lfvCurr.


*!**************************************************************************
*! Name      : lfAssignSc
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : to save the current Scale after printing it in order not to 
*!             print it except when Scale changes 
*!**************************************************************************
*! Called from : All FRXs
*!**************************************************************************
*! Example   : = lfAssignSc()
*!**************************************************************************
*B802246,1
FUNCTION lfAssignSc
lcOldScale = Scale
RETURN ''
*-- End of lfAssignSc.

*!**************************************************************************
*! Name      : lfScalePgH
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : to empty lcOldScale var. in each Page Header Band in  
*!             order to be printed once at the start of the Page if 
*!             the Scale is not changed 
*!**************************************************************************
*! Called from : All FRXs
*!**************************************************************************
*! Example   : = lfScalePgH()
*!**************************************************************************
*B802246,1
FUNCTION lfScalePgH
lcOldScale = SPACE(3)
RETURN ''
*-- End of lfScalePgH.

*!**************************************************************************
*! Name      : lfInnGrpIn
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Evaluate inner group values, when you enter group header.
*!**************************************************************************
*! Called from : All FRXs (DOS Format)
*!**************************************************************************
*! Example   : =lfInnGrpIn()
*!**************************************************************************
*E301265,1
FUNCTION lfInnGrpIn
lcInnGrpIn  = EVALUATE(lcInnGrp)
RETURN ''
*-- End of lfInnGrpIn.

*!**************************************************************************
*! Name      : lfOutGrpIn
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/17/1999
*! Purpose   : Evaluate outer group values, when you enter group header.
*!**************************************************************************
*! Called from : All FRXs (DOS Format)
*!**************************************************************************
*! Example   : =lfOutGrpIn()
*!**************************************************************************
*E301265,1
FUNCTION lfOutGrpIn
lcOutGrpIn = EVALUATE(lcOutGrp)
RETURN ''
*-- End of lfOutGrpIn.

*!**************************************************************************
*! Name      : lfInnGrpOp
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Evaluate inner group values, when you in group Footer.
*!**************************************************************************
*! Called from : All FRXs (DOS Format)
*!**************************************************************************
*! Example   : =lfInnGrpOp()
*!**************************************************************************
*E301265,1
FUNCTION lfInnGrpOp
*C101569,1 Add sort by store if first sort is by order [Begin]
llLstMulti = (ORDHDR.MULTI = "Y")
*C101569,1 Add sort by store if first sort is by order [End  ]

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

*!**************************************************************************
*! Name      : lfOutGrpOp
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Evaluate outer group values, when you in group Footer.
*!**************************************************************************
*! Called from : All FRXs (DOS Format)
*!**************************************************************************
*! Example   : =lfOutGrpOp()
*!**************************************************************************
*E301265,1
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
*-- End of lfOutGrpOp.

*!**************************************************************************
*! Name      : lfvCurDisp
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Activate currency display screen to get user 
*!           : selection for currencies.
*!**************************************************************************
*! Example   : =lfvCurDisp()
*!**************************************************************************
*!E301272,1
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- End of lfvCurDisp.

*!**************************************************************************
*! Name      : lfCurrPrnt
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Compute Currency symbol to print.
*!**************************************************************************
*! Example   : =lfCurrPrnt()
*!**************************************************************************
*!E301272,1
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

*!**************************************************************************
*! Name      : lfChCurSm
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Share with last function to Compute Currency symbol to print.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Called from : All FRXs (DOS Format)
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfChCurSm()
*!**************************************************************************
*!E301272,1
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

*!**************************************************************************
*! Name      : lfCurrDesc
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : Currency description if sort by currency.
*!**************************************************************************
*! Returns   : Currency description.
*!**************************************************************************
*! Example   : =lfCurrDesc()
*!**************************************************************************
*!E301272,1
FUNCTION lfCurrDesc
PRIVATE lcCurrVal , lcCurDesc
lcCurDesc = ''
lcCurrVal  = ALLTRIM(cCurrCode)
lnCurVlPos = ASCAN(laCurrVal,lcCurrVal)
IF lnCurVlPos > 0
  lcCurDesc  = laCurrDesc[lnCurVlPos,1]
ENDIF  
RETURN lcCurDesc
*-- End of lfCurrDesc.

*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Sameh Saiid Ezzat
*! Date      : 12/13/1999
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
*!E500271,4
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
IF 'EB' $ gcCmpModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF
llClearOrd = .T.
*-- End of lfvEdiOrd.
