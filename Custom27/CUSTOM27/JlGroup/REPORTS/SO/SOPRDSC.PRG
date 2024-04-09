************************************************************************
*: Program file  : SOPRDSC.PRG (Copied from SORDDET.PRG)
*: Program desc. : Custom Production Schecduling Report for JGroup.
*: For Report    : (SOPRDSCD.FRX,SOPRDSCS.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Adel Mohammed El Gazzar (ADEL)
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
*: Refer to C(102233)
*:******************************************************************************************
*:B605989,4 AMH 06/16/2002 Rename the status field in ICDESIGN file to cdsgstatus.
*:B606331,1 RAE 09/17/2002 Fix the bug of variable 'CLRCHG0' or 'CLRCHG28' is not found.
*:******************************************************************************************

*--lcLocSrc   && Disgn Status target array.
*--laRpCdsVal && Disgn Loc codes array.
*--lcRpStatSrc  && Disgn Status source array.
*--laRpStatTar  && Disgn Status target array.  

lcWorkTag = [cStylePos+ORDER+STR(LINENO,6)]
STORE 0 TO lnDsnRun,lnTotRun,lnADsnRun,lnATotRun
lcTime     = TIME()                     && Variable to hold the Time
lcStTime   = lcTime                     && Time in which we start collect data.
lnLastRec  = 2                          && Record No. Of last record in order group.
lcTitle    = ''                         && Title of Note. 
lcNotes    = ''                         && Notes.
llNoIndex  = .F.                        && I don't make index for file.
llRpSummMt = .F.
lcPhonPict = gfPhoneTem()
STORE '' TO lcLineCurr
STORE .T. TO llInnTotal,llOutTotal
lcSeekVal  = ''                         && Get Last record seek value.
llTextMode = (UPPER(ALLTRIM(lcRepMode))=="TEXT")  && Print Text Format
lcStorCond = ''
lcOldScale = SPACE(3)
llFirstSc = .F.
lnFinal = 0
lnFinal1 = 0
LNEST = 0
DIMENSION laStock[9],laWip[9]
STORE '' TO laStock,laWip               && Work process arrays
STORE '' TO lcGrpExp,lcSeaExp,lcDivExp,lcStatusEx,lcCatExp
*-- Show messages in status bar when collecting data. [begin]
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON

IF !USED('PRODUCTN')
  lcCurrPath = SYS(5) + SYS(2003) + '\'
  IF !FILE(lcCurrPath+'PRODUCTN.DBF')
    *-- Cannot work without Time table Excel sheet.
    =gfModalGen("TRM000000B00000","DIALOG",'','','Cannot work without Time table Excel sheet.')  
    RETURN
  ENDIF
  =gfOpenFile(lcCurrPath+'PRODUCTN',lcCurrPath+'PRODUCTN','SH')
ENDIF

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
*  DIMENSION laTempStru[ALEN(laTempStru,1) + 2, 4]
  DIMENSION laTempStru[ALEN(laTempStru,1) + 6, 4]
  *-- cTempKey :  field used in most sort by case as the master key ,
  *--          :  and in case of summarize multi store as the total amount.
  laTempStru[ALEN(laTempStru,1) -5  ,1] = 'cTempKey'
  laTempStru[ALEN(laTempStru,1) -5  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) -5  ,3] = 16
  laTempStru[ALEN(laTempStru,1) -5 ,4] = 0
  laTempStru[ALEN(laTempStru,1) -4 ,1] = 'cdsgncode'
  laTempStru[ALEN(laTempStru,1) -4 ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) -4 ,3] = 6
  laTempStru[ALEN(laTempStru,1) -4 ,4] = 0
  laTempStru[ALEN(laTempStru,1) -3 ,1] = 'cStylePos'
  laTempStru[ALEN(laTempStru,1) -3 ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) -3 ,3] = 6
  laTempStru[ALEN(laTempStru,1) -3 ,4] = 0
  laTempStru[ALEN(laTempStru,1) -2 ,1] = 'Stitches'
  laTempStru[ALEN(laTempStru,1) -2 ,2] = 'N'
  laTempStru[ALEN(laTempStru,1) -2 ,3] = 6
  laTempStru[ALEN(laTempStru,1) -2 ,4] = 0
  laTempStru[ALEN(laTempStru,1) -1 ,1] = 'lLastLine'
  laTempStru[ALEN(laTempStru,1) -1 ,2] = 'L'
  laTempStru[ALEN(laTempStru,1) -1 ,3] = 0
  laTempStru[ALEN(laTempStru,1) -1 ,4] = 0
  laTempStru[ALEN(laTempStru,1)    ,1] = 'nEstMin'
  laTempStru[ALEN(laTempStru,1)    ,2] = 'N'
  laTempStru[ALEN(laTempStru,1)    ,3] = 12
  laTempStru[ALEN(laTempStru,1)    ,4] = 2

  *-- Create temporary file that holding order line data. [end]
  llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.
ENDIF  && end if it's first time you run option Grid.
*-- Create temporary cursors from structure array. [begin]
IF EMPTY(lcMastFile) OR !USED(lcMastFile)
  *-- Setting for report [begin]
  lcSetHour = SET('HOURS')
  SET HOURS TO 24
  *-- create temp. file that used if you have both types of notes. [end]
  *-- Create work file.
  = lfCreatCur(lcWorkFile)  && Create work cursor.
  = lfCreatCur(lcTempLine)  && Create line cursor.
ENDIF

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
  lcStatusEx = [ORDHDR.STATUS $ lcRpStatus]
  *-- if user select Style group, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnGrpPos,6])
    lcGrpExp  = "&laOGFxFlt[lnGrpPos,1]." + ' $ laOGFxFlt[lnGrpPos,6]'
  ENDIF  && end if user select Style group, evaluate its expression.
  *-- if user select Season, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnSeaPos,6])
    lcSeaExp  = "&laOGFxFlt[lnSeaPos,1]." + ' $ laOGFxFlt[lnSeaPos,6]'
  ENDIF  && end if user select Season, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnDivPos,6])
    lcDivExp  = "&laOGFxFlt[lnDivPos,1]." + ' $ laOGFxFlt[lnDivPos,6]'
  ENDIF  && if user select Division, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnCatPos,6])
    lcCatExp  = "&laOGFxFlt[lnCatPos,1]." + ' $ laOGFxFlt[lnCatPos,6]'
  ENDIF
  *-- Evaluate Color/Free Expression. [begin]
  *-- Note that: We use either Only This XXX color object or direct XXX 
  *--            Free object, and you must know that both types of 
  *--            expressions can't be enable at the same time.
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
  = lfScanData()  && Scan around master file.
  *-- Unrise all Critria variables.
  STORE .F. TO llChSelect,llChStatus,llChCoord,llChSumm,;
               llChAcc,llChStyle,llChFabric,llChRep,llChOrder,llChLoc
ENDIF       && end If user change report critria, Collect report data. 

SELECT OrdLine
SET RELATION OFF INTO ORDHDR  && break relation.
SET RELATION OFF INTO STYLE  && break relation.
*-- Temporary File relations, in the way that help us in report form [begin]
SELECT (lcMastFile)
SET RELATION TO cOrdType + Order INTO OrdHdr      && To order header file.
SET RELATION TO style INTO Style ADDITIVE         && To style file.
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE   && To scale file.
SET RELATION TO cWareCode INTO WAREHOUS ADDITIVE  && To warehouse file.
SET ORDER TO TAG Icdesign IN Icdesign
SET RELATION TO cDsgnCode INTO ICDESIGN ADDITIVE

lcCustRel = IIF(llRpSummMt,['M' + Account],;
            [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
            
SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.

*-- If sort by style group , set relation to codes file.
*-- if you print order notepad, open master file in another alias to 
*-- help us to know what last line in order group to print notepad
*-- after it, note we do this because we print notepad in detail band
*-- not in order group band .
*-- Select Master report file.
SELECT (lcMastFile)
SET FILTER TO TOTQTY <> 0
=lfPutLineNo()
GO BOTTOM
lnLastOne  = RECNO()
GO TOP    && Refresh Relation
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)
IF llTextMode
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMastFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
ELSE  && else window format
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMastFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' TIMEOUT 1
ENDIF
*--Prepare Design Location & Design Status filters.
DO gfDispRe WITH EVAL('lcRpForm')

WAIT CLEAR
SET STATUS BAR &lcStatusBr
RETURN
*-- end of Report Code.


*-- Function section 
*-------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Option Grid When function
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
    *B603946,1 Check if empty of target array [End]
  ELSE
    DECLARE laRpTarget[1]
  ENDIF
  *--B603870,1 RAMY [end]
  STORE 'Bid'      TO laRpSource[1]  
  STORE 'Open'     TO laRpSource[2]
  STORE 'Hold'     TO laRpSource[3]
  STORE 'Complete' TO laRpSource[4]
  STORE 'Canceled' TO laRpSource[5]
  lcRpStatus = 'BOHCX'
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
  lnCatPos   = lfItmPos('ORDHDR.CORDERCAT')
  lnClrSgPos = lfItmPos('SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)')
  *-- Convert all ceiling functions to use lfItmPos because [End..
  lnFreSgPos = lnClrSgPos + 1
  *-- Collect all the Style Positions from Codes file
  DECLARE laRpDsnSrc[1] , laRpDsnTar[1] , laRpCdsVal[1]     && Used in Design Location 
  SELECT cDiscrep FROM Codes WHERE cDefCode = "N" AND cFld_Name = "CSTYLEPOS" INTO ARRAY laRpDsnSrc
  SELECT cCode_No FROM Codes WHERE cDefCode = "N" AND cFld_Name = "CSTYLEPOS" INTO ARRAY laRpCdsVal
  *-- For Design Status. 
  DECLARE laRpStatSrc[2] , laRpStatTar[1]
  laRpStatTar[1] = ''
  laRpStatSrc[1] = 'Approved'
  laRpStatSrc[2] = 'Pending'
ELSE
  FOR lnElm = 1 TO ALEN(laOgObjType,1) 
   IF lcDummy = "Y" AND laOgObjType[lnElm,1] == "lcRpSelcBy"
     _CUROBJ= OBJNUM(&laOgObjType[lnElm,2].)+1
     lcDummy = "N"
   ENDIF
  ENDFOR
  *--MAN End
ENDIF  && END IF you first time enter when function.
*-- Disable/enable By account, style, fabric, location, sales representative. [begin]
*-- note that disable and enable is according to value of laRpFltVal.
IF lcRpSelcBy = "L" OR !(lcRpSelcBy == lcOldSelc)
  STORE .T. TO llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
ENDIF
lcOldSelc = lcRpSelcBy
DO lpChkStat

IF !USED('BOMVAR')
  =gfOpenFile(gcDataDir+'BOMVAR',gcDataDir+'Cdsgncode','SH')
ENDIF

IF !USED('icdesign')
  =gfOpenFile(gcDataDir+'icdesign',gcDataDir+'Acctdsgn','SH')
ENDIF
*-- end of lfwRepWhen. --*


*!**************************************************************************
*! Name      : lfwOldSelc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/08/2000
*! Purpose   : To store the Old Select By value
*!**************************************************************************
*! Example   : = lfwOldSelc()
*!**************************************************************************
FUNCTION lfwOldSelc
lcOldSelc = lcRpSelcBy
*-- End of lfwOldSelc.

*!**************************************************************************
*! Name      : lfVarPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/10/2000
*! Purpose   : To get the position of the Variable in OG.
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : = lfVarPos()
*!**************************************************************************
*E500342,1 This function was added in order to get the Position of the 2 options
*E500342,1 Added in Selection grid so as to Enable/Disable them.
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
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/10/2000
*! Purpose   : To check for Order status in Order to Enable/Disable the 2
*!             Option added to the Selection grid.
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : DO lpChkStat
*!**************************************************************************
*E500342,1 This function was added in order to check for the Order Status so 
*E500342,1 as to Enable/Disbale the 2 options added in Selection grid.

PROCEDURE lpChkStat
PRIVATE llStatus
llStatus = IIF('O' $ lcRpStatus OR 'H' $ lcRpStatus,.T.,.F.)
IF !llStatus
  llRpPrtShp = .F.
  llRpAloOrd = .T.
ENDIF

*-- Disable (Partially shipped Orders & Include Allocated Order Lines) if Order Status
*-- contains only Complete or Cancelled
laOGObjCnt[lfVarPos('llRpPrtShp')] = llStatus
=lfOGShowGet('llRpPrtShp')

laOGObjCnt[lfVarPos('llRpAloOrd')] = llStatus
=lfOGShowGet('llRpAloOrd')
*-- End of lpChkStat.

*!**************************************************************************
*! Name      : lfvDisMesg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/10/2000
*! Purpose   : Diaplay message to inform user to ignore Complete,Cancel,Bid Orders
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : = lfvDisMesg()
*!**************************************************************************
*E500342,1 This function was added in order to get the Position of the 2 options
*E500342,1 Added in Selection grid so as to Enable/Disable them.
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

*!*************************************************************
*! Name      : lfFormName
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Function to get the Form name
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Form name
*!*************************************************************
*! Example     : = lfFormName()
*!*************************************************************
*FUNCTION lfFormName

*RETURN 'SORDDETA'
*-- end of lfFormName.

*!*************************************************************
*! Name      : lfScanData
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/07/98
*! Purpose   : Collect report data.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfSumStyle,lfSumMulti
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfScanData()
*!*************************************************************
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
*!*************************************************************
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


lcWorkTag = [cStylePos+ORDER+STR(LINENO,6)]

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

lcTOrdFile = laOGFxFlt[lnOrdPos,6]
llWorkDeal = !EMPTY(lcTOrdFile) AND USED(lcTOrdFile) AND RECCOUNT(lcTOrdFile) > 0
*-- If user select specific orders, collect data of this orders only. [begin]
SET ORDER TO TAG Acctdsgn IN Icdesign
IF llWorkDeal
  SELECT (lcTOrdFile)
  *-- Scan order cursor.
  SCAN  
    SELECT ORDLINE
    SET ORDER TO TAG ORDLINE
    *-- if find first order record in ordline file and <ordhdr filter>
    *E500271,4 add the lcRpEdiFlt to the expression [Begin.] 
    *E500342,1 Adding the 1st Option to the If condition [Begin]
    *E500342,1 Print Partially Shipped Orders (Y/N)
    *IF SEEK('O'+&lcTOrdFile..ORDER) AND;
    *   EVALUATE(lcStatusEx) AND ;
    *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
    *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
    *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
    *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
    IF SEEK('O'+&lcTOrdFile..ORDER) AND;
       EVALUATE(lcStatusEx) AND ;
       IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
       IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
       IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
    *E500342,1 Adding the 1st Option to the If condition [End]
    *E500342,1 Print Partially Shipped Orders (Y/N)
    *E500271,4 add the lcRpEdiFlt to the expression [End.] 

      *-- Scan ordline file for rest order data.
      *C102262,1 (Begin) Get proper orders not only 'O' type.
      *SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+&lcTOrdFile..Order
      SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = cOrdType+&lcTOrdFile..Order
      *C102262,1 (End)
        *-- if <ordline filter> and <style group filter> and <Color Filter>
        *-- insert this data into workfile.
        *E301421,1 ABD- Add Feature to allow printing the Bid and Complete status,
        *E301421,1 ABD- And in case of Complete status we will print Booked Qty
        *E301421,1 ABD- Because the TotQty is equal to Zero and change the IF
        *E301421,1 ABD- Condition to accept totQty = Zero. [Begin]
        
        *E500342,1 Adding the 2nd Option to the If condition [Begin]
        *E500342,1 Include Allocated Order Lines (Y/N)
        IF (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
           (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
           IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
           IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
           IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
           IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp)) AND ;
           IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;           
           IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
        *B803674,1 Apply Partially Shipped on Order lines also [End]
        *E500342,1 Adding the 2 Options to the If condition [End]
        *E500342,1 Include Allocated Order Lines (Y/N)

          *E301421,1 ABD- [End]  
        
          SCATTER MEMVAR MEMO
        
          *E301421,1 ABD- Add Feature to allow printing the Bid and Complete status,
          *E301421,1 ABD- And in case of Complete status we will print Booked Qty
          *E301421,1 ABD- Because the TotQty is equal to Zero. [Begin]
          IF ORDHDR.STATUS = "C"
            *-- Get qty from book qty becauce qty =0
            FOR I = 1 To 8
              lcCount = ALLTRIM(STR(I))
              m.QTY&lcCount = BOOK&lcCount
            ENDFOR
            m.TOTQTY = TOTBOOK
          ENDIF
          m.cTempKey  = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
          =lfGetRec(ALIAS(),lcWorkFile)
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
IF lnUsedItem > 0
  lcSlctFile = laOGFxFlt[lnUsedItem,6]
  llLineDeal = !EMPTY(lcSlctFile) AND USED(lcSlctFile) AND RECCOUNT(lcSlctFile) > 0
ENDIF  
llRpStyLoc = (lcRpSelcBy = 'W') AND llLineDeal
*-- If User select data by any select case, beside selecting orders. 
            *-- IMPORT must be good described before add any line in it.
IF RECCOUNT(lcWorkFile) > 0 AND llLineDeal
  lcSlctKey = IIF(lcRpSelcBy = 'A',"ACCOUNT"   ,;
              IIF(lcRpSelcBy = 'S',"PADR(CSTYMAJOR,lnMajorLen)"     ,;
              IIF(lcRpSelcBy = 'W',"CWARECODE" ,;
              IIF(lcRpSelcBy = 'F',"FABRIC"    ,;
              "REPCODE"))))  && Field which we seek for in workfile.
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
            = lfSumStyle(lcWorkFile,lcSeekExp)  && sum for this style.
            =lfGetRec(ALIAS(),lcTempLine)
          ENDIF
        ELSE  && normal case, add line to temp. line file.
          SCATTER MEMVAR MEMO
          =lfGetRec(ALIAS(),lcTempLine)
        ENDIF  
      ENDSCAN  && end scan work file for the rest data have the same seek critria.
    ENDIF      && end if you find seeking critria in work file.
  ENDSCAN      && end Scan selected cursor.
  lcMastFile = lcTempLine
ELSE  && User either Does not select orders or does not use any select type.
  *-- if User does not select orders but use select type.
  IF llLineDeal
    *-- set files order [begin]
    lcOrdVar = IIF(INLIST(lcRpSelcBy,'S','F') , 'ORDLINES' , 'ORDLINE')	
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
          *E500271,4 add the lcRpEdiFlt to the expression [End.] 
          
            SELECT ORDHDR
            *-- scan ordhdr file rest for this account. 
            *E500342,1 Adding the 1st Option to the If condition [Begin]
            *E500342,1 Print Partially Shipped Orders (Y/N)
            *SCAN REST WHILE account+cordtype+order = &lcSlctFile..ACCOUNT
            SCAN REST WHILE account+cordtype+order = &lcSlctFile..ACCOUNT FOR ;
                           IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
            *E500342,1 Adding the 1st Option to the If condition [End]
            *E500342,1 Print Partially Shipped Orders (Y/N)
              *-- if order type is 'O' , <ordhdr filter>, and seek for this
              *-- order in order line file.
              *E500271,4 add the lcRpEdiFlt to the expression [Begin.] 
              IF CORDTYPE = 'O' AND ;
                 EVALUATE(lcStatusEx) AND ;
                 IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 SEEK('O'+ORDER,'ORDLINE')
                SELECT ORDLINE 
                *-- scan ordline for rest order lines.
                SCAN REST WHILE cordtype+order+STR(lineno,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
                  IF (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                     (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                     IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
                     IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
                     IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))   AND ;
                     IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp)) AND ;
                     IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
                     IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
                    SCATTER MEMVAR MEMO
                    IF ORDHDR.STATUS = "C"
                      *-- Get qty from book qty becauce qty =0
                      FOR I = 1 To 8
                        lcCount = ALLTRIM(STR(I))
                        m.QTY&lcCount = BOOK&lcCount
                      ENDFOR
                      m.TOTQTY = TOTBOOK
                    ENDIF
                    *E301421,1 ABD- [End]
                    
                    m.cTempKey = PADR(STYLE.FABRIC,7) + ;
                                 PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
                     =lfGetRec(ALIAS(),lcWorkFile)
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
          IF SEEK(PADR(CSTYMAJOR,lnMajorLen),'ORDLINE') AND ;
             IIF(EMPTY(lcGrpExp),.T., EVALUATE(lcGrpExp)) AND ;
             IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
             IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))
            SELECT ORDLINE
            *-- scan ordline for the rest of this style.
            SCAN REST WHILE STYLE+DTOS(complete)+cordtype+order+store+STR(lineno,6) = PADR(&lcSlctFile..CSTYMAJOR,lnMajorLen) ;
                      FOR IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
            *E500342,1 Adding the 1st Option to the If condition [End]
            *E500342,1 Print Partially Shipped Orders (Y/N)
            *B603104,1 Avoid duplicate records [End  ]

              *-- if <ordhdr filter> and <ordline filter> and <Color Filter>
              *E500271,4 add the lcRpEdiFlt to the expression [Begin.] 
              *E301421,1 ABD- Add Feature to allow printing the Bid and Complete status,
              *E301421,1 ABD- And in case of Complete status we will print Booked Qty
              *E301421,1 ABD- Because the TotQty is equal to Zero and change the IF
              *E301421,1 ABD- Condition to accept totQty = Zero. [Begin]
              *IF CORDTYPE = 'O' AND TotQty > 0 AND ;
                 EVALUATE(lcStatusEx) AND ;
                 (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
                              
              *E500342,1 Adding the 2 Options to the If condition [Begin]
              *E500342,1 Include Allocated Order Lines (Y/N)
              *IF CORDTYPE = 'O' AND ;
              *   EVALUATE(lcStatusEx) AND ;
              *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
              *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
              *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
              *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
              *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
              *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
              *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
              *B803674,1 Apply Partially Shipped on Order lines also [Begin]                     
              IF CORDTYPE = 'O' AND ;
                 EVALUATE(lcStatusEx) AND ;
                 (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                 IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
                 IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
              *B803674,1 Apply Partially Shipped on Order lines also [End]    
              *E500342,1 Adding the 2 Options to the If condition [End]
              *E500342,1 Include Allocated Order Lines (Y/N)

              *E301421,1 ABD-  [End]
                
                SCATTER MEMVAR MEMO

                *E301421,1 ABD- Add Feature to allow printing the Bid and Complete status,
                *E301421,1 ABD- And in case of Complete status we will print Booked Qty
                *E301421,1 ABD- Because the TotQty is equal to Zero. [Begin]
                IF ORDHDR.STATUS = "C"
                  *-- Get qty from book qty becauce qty =0
                  FOR I = 1 To 8
                    lcCount = ALLTRIM(STR(I))
                    m.QTY&lcCount = BOOK&lcCount
                  ENDFOR
                  m.TOTQTY = TOTBOOK
                ENDIF
                *E301421,1 ABD- [End]
                
                m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
                =lfGetRec(ALIAS(),lcWorkFile)
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
              *E500342,1 Adding the 1st Option to the If condition [Begin]
              *E500342,1 Print Partially Shipped Orders (Y/N)
              *SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = ;
              *                STYLE.STYLE
              SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = ;
                              STYLE.STYLE FOR IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
              *E500342,1 Adding the 1st Option to the If condition [End]
              *E500342,1 Print Partially Shipped Orders (Y/N)
                *-- if <ordhdr filter> and <ordline filter>
                *--B603870,1 RAMY consider the case of completed orders [start]
                IF CORDTYPE = 'O' AND TotQty > 0 AND ;
                   EVALUATE(lcStatusEx) AND ;
                   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])
                *B803674,1 Apply Partially Shipped on Order lines also [End]
                *E500342,1 Adding the 2 Options to the If condition [End]
                *E500342,1 Include Allocated Order Lines (Y/N)
                *--B603870,1 RAMY [end]

  
                  SCATTER MEMVAR MEMO
                  m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
                 
                  *--B603870,1 RAMY if the order is completed get the Book Qty insted of the Tot Qty [start]
                  IF ORDHDR.STATUS = "C"
                  *-- Get qty from book qty becauce qty =0
                    FOR I = 1 To 8
                      lcCount = ALLTRIM(STR(I))
                      m.QTY&lcCount = BOOK&lcCount
                    ENDFOR
                    m.TOTQTY = TOTBOOK
                  ENDIF
                  =lfGetRec(ALIAS(),lcWorkFile)
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

          *E500342,1 Adding the 1st Option to the If condition [Begin]
          *E500342,1 Print Partially Shipped Orders (Y/N)
          *SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
          *              CORDTYPE = 'O' AND  ;
          *              EVALUATE(lcStatusEx) AND ;
          *              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
          *              IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
          *              IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]))
          SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
                        CORDTYPE = 'O' AND  ;
                        EVALUATE(lcStatusEx) AND ;
                        IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                        IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
          *E500342,1 Adding the 1st Option to the If condition [End]
          *E500342,1 Print Partially Shipped Orders (Y/N)
                        
          *-- scan ordhdr for this Location.
          SCAN
            = SEEK('O'+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
            *C102262,1 (End)
            SELECT ORDLINE
            *-- Scan ordline file to get lines of this order
            *-- that evaluate critria.
            SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O' + ORDHDR.ORDER
              *-- if <ordline filter> and <style group filter> and <Color Filter>
              *--B603870,1 RAMY consider the case of completed orders [start]

              *E500342,1 Adding the 2 Options to the If condition [Begin]
              *E500342,1 Include Allocated Order Lines (Y/N)
              IF (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
                 IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
                 IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
                 IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
                 IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
              *B803674,1 Apply Partially Shipped on Order lines also [End]
              *E500342,1 Adding the 2 Options to the If condition [End]
              *E500342,1 Include Allocated Order Lines (Y/N)
              *--B603870,1 RAMY [end]


                SCATTER MEMVAR MEMO
                m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
                *--B603870,1 RAMY if the order is completed get the Book Qty insted of the Tot Qty [start]
                IF ORDHDR.STATUS = "C"
                *-- Get qty from book qty becauce qty =0
                  FOR I = 1 To 8
                    lcCount = ALLTRIM(STR(I))
                    m.QTY&lcCount = BOOK&lcCount
                  ENDFOR
                  m.TOTQTY = TOTBOOK
                ENDIF
                =lfGetRec(ALIAS(),lcWorkFile)
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
                        IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                        IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])

          *E500271,4 add the lcRpEdiFlt to the expression [Begin.] 

          *E500342,1 Adding the 1st Option to the If condition [Begin]
          *E500342,1 Print Partially Shipped Orders (Y/N)
          *SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
          *              CORDTYPE = 'O' AND ;
          *              EVALUATE(lcStatusEx) AND ;
          *              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
          *              (EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
          *              (EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
          SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
                        CORDTYPE = 'O' AND ;
                        EVALUATE(lcStatusEx) AND ;
                        (EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
                        (EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
          *E500342,1 Adding the 1st Option to the If condition [End]
          *E500342,1 Print Partially Shipped Orders (Y/N)

          *E500271,4 add the lcRpEdiFlt to the expression [End.] 
                        
          *B802429,1 Fix Filtering Bug [End  ]
                        
          *-- scan ordhdr for this sales rep.
          SCAN
            = SEEK('O'+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
            SELECT ORDLINE
            *-- Scan ordline file to get lines of this order
            *-- that evaluate critria.
            *C102262,1 (Begin) Get proper orders not only 'O' type.
            *SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O' + ORDHDR.ORDER
            SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = cOrdType + ORDHDR.ORDER
            *C102262,1 (End)
              *-- if <ordline filter> and <style group filter> and <Color Filter>
              *--B603870,1 RAMY consider the case of completed orders [start]
              IF (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 (EMPTY(lcSeaExp) OR EVALUATE(lcSeaExp)) AND ;
                 (EMPTY(lcDivExp) OR EVALUATE(lcDivExp)) AND ;
                 IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
                 IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
                 IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
              *B803674,1 Apply Partially Shipped on Order lines also [End]
              *E500342,1 Adding the 2 Options to the If condition [End]
              *E500342,1 Include Allocated Order Lines (Y/N)
              *--B603870,1 RAMY [end]


                SCATTER MEMVAR MEMO
                m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
                *--B603870,1 RAMY if the order is completed get the Book Qty insted of the Tot Qty [start]
                IF ORDHDR.STATUS = "C"
                *-- Get qty from book qty becauce qty =0
                  FOR I = 1 To 8
                    lcCount = ALLTRIM(STR(I))
                    m.QTY&lcCount = BOOK&lcCount
                  ENDFOR
                  m.TOTQTY = TOTBOOK
                ENDIF
                =lfGetRec(ALIAS(),lcWorkFile)
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
          
          IF !SEEK(lcSeekExp,lcTempLine) AND CORDTYPE = 'O' AND ;
             EVALUATE(lcStatusEx) AND ;
             (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
             (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
             IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
             IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
             IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
             IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
             IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
             IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
             IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
             IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
             IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
            SCATTER MEMVAR MEMO
            IF ORDHDR.STATUS = "C"
            *-- Get qty from book qty becauce qty =0
              FOR I = 1 To 8
                lcCount = ALLTRIM(STR(I))
                m.QTY&lcCount = BOOK&lcCount
              ENDFOR
              m.TOTQTY = TOTBOOK
            ENDIF
            = lfSumMulti(lcSeekExp)  && summarize data.
            *--Check Desion code
            =lfGetRec(ALIAS(),lcTempLine)
          ENDIF    && end if you does not find line in temp line file,
        ENDSCAN    && end scan file for full index expression (rushmore).
        USE IN SUMMULTI

      ELSE  && Normal collection case for all data in ordline file.
        SCAN FOR CORDTYPE + ORDER + STR(LINENO,6) = 'O'
          IF EVALUATE(lcStatusEx) AND ;
             (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
             (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
             IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
             IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
             IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
             IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
             IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
             IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
             IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
             IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
             IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
            SCATTER MEMVAR MEMO
            m.cTempKey = PADR(STYLE.FABRIC,7)+PADR(STYLE.CSTYGROUP,6)+PADR(ORDHDR.REP1,3)
            *--B603870,1 RAMY if the order is completed get the Book Qty insted of the Tot Qty [start]
            IF ORDHDR.STATUS = "C"
            *-- Get qty from book qty becauce qty =0
              FOR I = 1 To 8
                lcCount = ALLTRIM(STR(I))
                m.QTY&lcCount = BOOK&lcCount
              ENDFOR
              m.TOTQTY = TOTBOOK
            ENDIF
            =lfGetRec(ALIAS(),lcTempLine)
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
      = lfSumStyle(lcWorkFile,lcSeekExp)
      =lfGetRec(ALIAS(),lcTempLine)
    ENDIF  
  ENDSCAN
  lcMastFile = lcTempLine
ENDIF

SELECT (lcMastFile)
INDEX ON &lcWorkTag TAG (lcMastFile)


*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/07/98
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
*B602562,1 only function it done is clear read [Begin
*STORE .T. TO llChSelect,llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
llChSelect = .T.

llClearAcc = (lcRpSelcBy # 'A')
llClearSty = (lcRpSelcBy # 'S')
llClearFab = (lcRpSelcBy # 'F')
llClearLoc = (lcRpSelcBy # 'L')
llClearRep = (lcRpSelcBy # 'R')
CLEAR READ
*B602562,1 only function it done is clear read [End..

*B602562,1 PRIVATE lnLastEnab,lnEnabled
*B602562,1 lnLastEnab = laOldVal
*B602562,1 llChSelect = IIF(lnLastEnab = 6,llChSelect,.T.) && Select changed.

*B602562,1 laOldVal = IIF(laOldVal = 1 , 'A' , IIF(laOldVal = 2 , 'W',;
*B602562,1            IIF(laOldVal = 3 , 'R' , IIF(laOldVal = 4 , 'S',;
*B602562,1            IIF(laOldVal = 5 , 'F','L')))))

*B602562,1 IF lcRpSelcBy != laOldVal
*B602562,1   lnEnabled = IIF(lcRpSelcBy = 'A',1,IIF(lcRpSelcBy = 'W',2,;
*B602562,1               IIF(lcRpSelcBy = 'R',3,IIF(lcRpSelcBy = 'S',4,;
*B602562,1               IIF(lcRpSelcBy = 'F',5,6)))))

  *-- if you select to use all selections
*B602562,1   IF lnEnabled = 6
*B602562,1     = lfSelcObjs(lnLastEnab,'D')  && Disable last selection case.
*B602562,1     laRpFltVal[lnLastEnab] = 'D'
*B602562,1   ELSE
*B602562,1     = lfSelcObjs(lnEnabled,'E')   && Enable  This selection case.
*B602562,1     laRpFltVal[lnEnabled] = 'E'
    *-- if last selection case is some thing rather than <All>
    *-- then you must disable it. 
*B602562,1     IF lnLastEnab != 6
*B602562,1       = lfSelcObjs(lnLastEnab,'D') 
*B602562,1       laRpFltVal[lnLastEnab] = 'D'
*B602562,1     ENDIF  && end if last selection case is some thing rather than <All>
*B602562,1   ENDIF    && end if you select to use all selections.
*B602562,1 ENDIF
*-- end of lfvSelect.

*!*************************************************************
*! Name      : lfSelcObjs
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/07/98
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
*! Name      : 
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/07/98
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
RETURN


*!*************************************************************
*! Name      : lfPreObjs
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Enable/Disable controled objects in 4 sort cases
*!           : - Style group, Fabric, Sales Rep., Complete date
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
*-- end of lfPreObjs.

*!*************************************************************
*! Name      : lfObjState
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*! Name      : lfvSizes
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Control Form name, Enable/disable some objects.
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
*! Example   : =lfvSizes()
*!*************************************************************
FUNCTION lfvSizes
lcRpForm = IIF(llRpSizes,'SORDDETA','SORDDETB')
= lfRepPltFr(lcRpForm)
*B602562,1 = lfObjState(IIF(llRpSizes,'E','D'),lnScale,'llRpScale')  && Enable/Disable Reprint scale when diff.

*B802246,1 llRpScale return True after changing llRpSizes to YES again [Begin.]
*llRpScale = IIF(llRpSizes,llRpScale,.F.)
CLEAR READ
*-- end of lfvSizes.

*!*************************************************************
*! Name      : lfvKind
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*B602562,1
lcRpForm = IIF(lcRpKind = 'D','SOPRDSCD','SOPRDSCS')
CLEAR READ

*-- end of lfvKind.

*!*************************************************************
*! Name      : lfvSumm
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
llChSumm   = .T.
CLEAR READ
*-- end of lfvSumm.


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*-- end of lfStySum.


*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/01/98
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*-- end of lfsChOrder.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*! Name      : lfSumStyle
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Summarize multi store styles using one file for scan and sum.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : sum file, sum expression
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfSumStyle()
*!*************************************************************
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
*-- end of lfSumStyle.

*!*************************************************************
*! Name      : lfSumMulti
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Summarize multi store styles using two aliass 
*!           : from same file for scan and sum,
*!           : in this case ordline file is used with out any 
*!           ; order to activiate rushmore, thus we open another 
*!           ; alias for make sum in the fastest way.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : sum expression
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfSumMulti()
*!*************************************************************
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
*-- end of lfSumMulti.

*!*************************************************************
*! Name      : lfLastRec
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Calculate last Record in order details. [ORDER GROUP]
*!           : we use another alias to unchange record pointer of report file.
*!*************************************************************
*! Called from : [SORDDETA.FRX OR SORDDETB.FRX, ORDER GROUP HEADER BAND] 
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
RETURN ''
*-- end of lfLastRec.

*!*************************************************************
*! Name      : lfvCoorGrp
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change print coordinate groups flag.
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
*! Example   : =lfvCoorGrp()
*!*************************************************************
FUNCTION lfvCoorGrp
llChCoord = .T.
*-- end of lfvCoorGrp.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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

*-- if user change setting [enter report <Preview> or <Run>]
IF !llFrTime
  SET HOURS TO &lcSetHour
ENDIF  && end if user change setting [enter report <Preview> or <Run>].

*E301272,1 Restore old currency setting before exit.
IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/27/1998
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
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfNMajType
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/27/1998
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
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

RETURN !EMPTY(lcFree_Clr)
*-- end of lfNMajType. 

*!*************************************************************
*! Name      : lfMakeExpr
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/18/98
*! Purpose   : Make expression for operator is either BETWEEN or INLIST.
*!*************************************************************
*! Called From : lfSscanData.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Operator expression.
*!*************************************************************
*! Example   : = lfMakeExpr()
*!*************************************************************
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
*-- end of lfMakeExpr.

*!*************************************************************
*! Name      : lfCreatCur
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/12/98
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
*-- We need temp. files to be files not cursor to open it in another alias [Begin]
*CREATE CURSOR (lcCurName) ;
*   FROM ARRAY laTempStru
CREATE TABLE (gcWorkDir+lcCurName) ;
   FROM ARRAY laTempStru
*-- We need temp. files to be files not cursor to open it in another alias [End  ]
*-- end of lfCreatCur.

*!*************************************************************
*! Name      : lfPipeExpr
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/12/98
*! Purpose   : Mask inlist expression.
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
*! Example   : = lfPipeExpr('AS|BS|CS',2)
*!*************************************************************
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
*-- end of lfPipeExpr.

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/12/98
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

*B802077,1 if empty of status is like select all available values [Begin]
*lcRpStatus = ALLTRIM(lcRpStatus)
*E301421,1 ABD Add Feature to allow printing the Bid and Complete status. [Begin]
*lcRpStatus = IIF(EMPTY(lcRpStatus),'OHX',ALLTRIM(lcRpStatus))
lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))
*E301421,1 ABD [End]
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

*E500342,1 Checking for the Order Status to Enable/Disable the 2 options which
*E500342,1 was added to the selection grid.
DO lpChkStat
*-- end of lfvOStatus.

*!*************************************************************
*! Name      : lfGetWork
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/98
*! Purpose   : - Compute work proccessing
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
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
RETURN ''
*-- end of lfGetWork.

*!*************************************************************
*! Name      : lfWorkEnd
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/98
*! Purpose   : - End Compute work proccessing
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : = lfWorkEnd()
*!*************************************************************
FUNCTION lfWorkEnd
STORE '' TO laStock,laWip,lnStkOrWip
RETURN ''
*-- end of lfWorkEnd.

*!*************************************************************
*! Name      : lfArrDumy
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/27/99
*! Purpose   : Fill Sort and select arrays
*!*************************************************************
*! Example   : = lfArrDumy()
*!*************************************************************
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
*-- end of lfArrDumy.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/98
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

*!*************************************************************
*! Name      : lfvCurr
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/31/99
*! Purpose   : set currency symbol
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvCurr()
*!*************************************************************
*B602590,1
FUNCTION lfvCurr
*PRIVATE lnCurPos
*lnCurPos = EVALUATE(SYS(18))
*SET CURRENCY TO ALLTRIM(laCurrSmbl[lnCurPos,1])
*-- end of lfvCurr.


*!**************************************************************************
*! Name      : lfAssignSc
*! Developer : Sameh (SSE)
*! Date      : 05/17/99
*! Purpose   : to save the current Scale after printing it in order not to 
*!             print it except when Scale changes 
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : All FRXs
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns            : ....
*!**************************************************************************
*! Example   : = lfAssignSc()
*!**************************************************************************
*B802246,1
FUNCTION lfAssignSc
*B604559,1 (Begin) Remark the following line and don't get the scale unless 
*B604559,1         it's printed once.
*lcOldScale = Scale
*IF llFirstSc
  lcOldScale = Scale
*ENDIF  
llFirstSc = .T.
*B604559,1 (End)

RETURN ''
*-- end of lfAssignSc.

*!**************************************************************************
*! Name      : lfScalePgH
*! Developer : Sameh (SSE)
*! Date      : 05/24/99
*! Purpose   : to empty lcOldScale var. in each Page Header Band in  
*!             order to be printed once at the start of the Page if 
*!             the Scale is not changed 
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : All FRXs
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns            : ....
*!**************************************************************************
*! Example   : = lfScalePgH()
*!**************************************************************************
*B802246,1
FUNCTION lfScalePgH
lcOldScale = SPACE(3)
RETURN ''
*-- end of lfScalePgH.

*!*************************************************************
*! Name      : lfInnGrpIn
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/17/1999
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
*E301265,1
FUNCTION lfInnGrpIn
lcInnGrpIn  = EVALUATE(lcInnGrp)
RETURN ''
*-- end of lfInnGrpIn.

*!*************************************************************
*! Name      : lfOutGrpIn
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/17/1999
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
*E301265,1
FUNCTION lfOutGrpIn
lcOutGrpIn = EVALUATE(lcOutGrp)
RETURN ''
*-- end of lfOutGrpIn.

*!*************************************************************
*! Name      : lfInnGrpOp
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/17/1999
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
*-- end of lfInnGrpOp.

*!*************************************************************
*! Name      : lfOutGrpOp
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/17/1999
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
*-- end of lfOutGrpOp.

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/15/98
*! Purpose   : Activate currency display screen to get user 
*!           : selection for currencies.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*!E301272,1
FUNCTION lfvCurDisp

*!*************************************************************
*! Name      : lfCurrPrnt
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/15/98
*! Purpose   : Compute Currency symbol to print.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCurrPrnt()
*!*************************************************************
*!E301272,1
FUNCTION lfCurrPrnt

*!*************************************************************
*! Name      : lfChCurSm
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/15/98
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
*!E301272,1
FUNCTION lfChCurSm
PRIVATE lcCurrCurr

*!*************************************************************
*! Name      : lfCurrDesc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/15/98
*! Purpose   : Currency description if sort by currency.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Currency description.
*!*************************************************************
*! Example   : =lfCurrDesc()
*!*************************************************************
*!E301272,1
FUNCTION lfCurrDesc

*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Sameh (SSE)
*! Date      : 07/28/99
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
*!E500271,4
FUNCTION lfvEdiOrd

*!**************************************************************************
*! Name      : lfvDsnLoc
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/03/2001
*! Purpose   : Validate Design locations.
*!**************************************************************************
*: Refer to C(102233)
FUNCTION lfvDsnLoc
PRIVATE lnLoop , lcLoop , lnInSource
= gfMover(@laRpDsnSrc ,@laRpDsnTar,'Design Locations ',.T.)
lcLocSrc = ''
IF !EMPTY(laRpDsnTar[1])
  FOR lnLoop   = 1 TO ALEN(laRpDsnTar,1)
    lnInSource = ASCAN(laRpDsnSrc,"\"+laRpDsnTar[lnLoop],1)
    lcLocSrc   = lcLocSrc + IIF(EMPTY(lcLocSrc),'',',') +laRpCdsVal[lnInSource] 
  ENDFOR
ENDIF
llOgFltCh = .T.

*!**************************************************************************
*! Name      : lfvDsnStat
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/03/2001
*! Purpose   : Validate Design location status.
*!**************************************************************************
*: Refer to C(102233)
FUNCTION lfvDsnStat

PRIVATE lnLoop , lcLoop , lnInSource
= gfMover(@laRpStatSrc ,@laRpStatTar,'Design Status ',.T.)
lcStatSrc = ''
*-- Loop to make Status expression.
IF !EMPTY(laRpStatTar[1])
  FOR lnI = 1 TO ALEN(laRpStatTar,1)
     lcStatSrc = lcStatSrc +IIF(laRpStatTar[lnI] = 'Approved','A','P')
  ENDFOR  && end Loop to make Status expression.
ENDIF
llOgFltCh = .T.

*!**************************************************************************
*! Name      : lfGetRec
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/03/2001
*! Purpose   : Append records going with the selected criteria..
*!**************************************************************************
*: Refer to C(102233)
FUNCTION lfGetRec
PARAMETERS lcAlias,lcTmp

PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcTmp)
IF EMPTY(ORDER())
  INDEX ON &lcWorkTag TAG (lcWorkFile)
ENDIF
IF SEEK('SO'+&lcAlias..ORDER+STR(&lcAlias..LineNo,6),'BOMVAR')
  SELECT BOMVAR
  SCAN REST WHILE cidtype+ccost_id+STR(lineno,6) = 'SO'+&lcAlias..ORDER+STR(&lcAlias..LineNo,6);
       FOR  IIF(EMPTY(lcLocSrc) AND EMPTY(lcStatSrc),.T.,IIF(EMPTY(lcLocSrc),.T.,BOMVAR.cStylePos $ lcLocSrc))
     
     *B605989,4 AMH Rename the status field in ICDESIGN file to cdsgstatus [Start]
     *IF SEEK(&lcAlias..ACCOUNT+&lcAlias..STORE+BOMVAR.cdsgncode,'ICDESIGN');
        AND IIF(EMPTY(lcStatSrc),.T.,ICDESIGN.STATUS $ lcStatSrc)
     IF SEEK(&lcAlias..ACCOUNT+&lcAlias..STORE+BOMVAR.cdsgncode,'ICDESIGN');
        AND IIF(EMPTY(lcStatSrc),.T.,ICDESIGN.CDSGSTATUS $ lcStatSrc)
     *B605989,4 AMH [End]
     
        m.cdsgncode = BOMVAR.cdsgncode 
        M.cStylePos = BOMVAR.cStylePos
        M.Stitches  = BOMVAR.nStitches        
        IF !SEEK(BOMVAR.cStylePos+&lcAlias..ORDER,lcTmp)
           INSERT INTO (lcTmp) FROM MEMVAR
           =lfCalcAct(&lcTmp..TOTQTY,.T.)
        ELSE
          SELECT (lcTmp)
           LOCATE REST WHILE cStylePos+ORDER+STR(LINENO,6) = BOMVAR.cStylePos+&lcAlias..ORDER;
                       FOR   stitches = BOMVAR.nstitches
                      
          IF FOUND()
            =lfCalcAct(&lcTmp..totqty+&lcAlias..TOTQTY,.T.)
            INSERT INTO (lcTmp) FROM MEMVAR            
          ELSE
            INSERT INTO (lcTmp) FROM MEMVAR             
            =lfCalcAct(&lcTmp..TOTQTY,.T.)
          ENDIF  
       ENDIF
     ENDIF
  ENDSCAN
ENDIF 
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfImport
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/03/2001
*! Purpose   : Import Time table excel sheet.
*!**************************************************************************
*: Refer to C(102233)
FUNCTION lfImport

PRIVATE lcFileName
*--Get the Excle sheet file.
lcFileName = GETFILE('XLS','Timetable spreadsheet..','Import')
*-- Checking if User selected File not of Type Excel Extension.
IF !(RIGHT(lcFileName,3)=="XLS")
  *-- Message : < Only Excel files are allowed for conversion >
  *-- Buttons : <                      OK                     >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Only Excel files are allowed for conversion.')  
  RETURN
ENDIF
*-- Importing from Excel Data to DBF.
SELECT 0                            && Prepare Empty Work Area.
lcCurrPath = SYS(5) + SYS(2003)     && get Default Drive and Directory.
IMPORT FROM (lcFileName) TYPE XLS
lcCurAlias = ALIAS()                && get Current File Alias.
*--Properly rename the file's fileds like this : StitchNo,ClrChg1,ClrChg2,.....etc.
lcFields = ""
lnNoOfFld = FCOUNT()

FOR lnFrom = 1 TO FCOUNT()
  lnFldLen  = FSIZE(FIELD(lnFrom))
  lnFldCrtL = IIF(lnFldLen < 10,1,IIF(lnFldLen<100,2,3))
  IF EMPTY(lcFields)
    lcFields = lcFields + "StitchNo C("+STR(lnFldLen,lnFldCrtL) +")"
  ELSE
    lnChngNo = IIF(lnFrom<=10,1,2)
    lcFields = lcFields + "," + "ClrChg" + STR(lnFrom-1,lnChngNo)+ " N("+STR(lnFldLen,lnFldCrtL) +",2)"
  ENDIF
ENDFOR
lcFields = "(" + lcFields + ")"
*--Create the DBF
IF USED('PRODUCTN')
  USE IN PRODUCTN
ENDIF
SELECT 0
CREATE TABLE PRODUCTN &lcFields
INDEX ON StitchNo TAG PRODUCTN
*--Get records
SELECT (lcCurAlias)

*--Delete header records.
DELE FOR EMPTY(EVAL(FIELD(1))) OR VAL(EVAL(FIELD(1))) = 0
*--Fill the new file with excel data.
SCAN
  SELECT PRODUCTN
  APPEND BLANK
  REPLACE StitchNo WITH EVAL(lcCurAlias+'.'+FIELD(1,lcCurAlias)),;
          ClrChg1  WITH VAL(EVAL(lcCurAlias+'.'+FIELD(2,lcCurAlias)))
  FOR lnFrom = 3 TO lnNoOfFld
    lcFrom  = STR(lnFrom-1,IIF(lnFrom<=10,1,2))
    IF (lnFrom = 6 OR lnFrom = 7) AND TYPE(lcCurAlias+'.'+FIELD(lnFrom,lcCurAlias)) = 'C'
      REPLACE PRODUCTN.ClrChg&lcFrom WITH VAL(EVAL(lcCurAlias+'.'+FIELD(lnFrom,lcCurAlias)))
    ELSE
      REPLACE PRODUCTN.ClrChg&lcFrom WITH EVAL(lcCurAlias+'.'+FIELD(lnFrom,lcCurAlias))
    ENDIF  
  ENDFOR
ENDSCAN
USE IN (lcCurAlias)

*!**************************************************************************
*! Name      : lfCalcAct
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/03/2001
*! Purpose   : Calculate Estimated production time.
*!**************************************************************************
*: Refer to C(102233)
FUNCTION lfCalcAct
PARAMETER lnOrdTot,llInside

PRIVATE lnAlias
lnAlias = SELECT()

*B606331,1 RAE Fix the bug of variable 'CLRCHG0' or 'CLRCHG28' is not found. [start] 
*IF EMPTY(ICDESIGN.CCOLORCHG) OR ICDESIGN.NSTITCHES = 0
  *RETURN 'N/A'
IF EMPTY(ICDESIGN.CCOLORCHG) .OR. ALLTRIM(ICDESIGN.CCOLORCHG) = "0" .OR. ;
   ALLTRIM(ICDESIGN.CCOLORCHG) > ALLTRIM(STR(FCOUNT("PRODUCTN")-1)) .OR. ;
   ICDESIGN.NSTITCHES = 0
  IF lcRpKind = 'D'
    RETURN 'N/A'
  ELSE
    RETURN 0
  ENDIF
*B606331,1 RAE [end]  
ENDIF
lnStitches = STITCHES
*--No of Runs = Open Qty/12 + 1
lnNoOfRun = ROUND((lnOrdTot/12),0) + 1
*--Production Time = No of Runs * Mininutes per run (Come from excel sheet)
SELECT PRODUCTN
LOCATE FOR StitchNo >= STR(lnStitches,6)
lcClrChg  = ICDESIGN.CCOLORCHG
lnMnPrRun = PRODUCTN.ClrChg&lcClrChg
lnPrdTime = lnNoOfRun*lnMnPrRun
lnDsnRun  = lnDsnRun + lnPrdTime
lnTotRun  = lnTotRun + lnPrdTime
IF llInside
  REPLACE &lcTmp..nEstMin WITH lnPrdTime 
ENDIF
SELECT (lnAlias)
RETURN lnPrdTime

*!**************************************************************************
*! Name      : lfCalcEst
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/03/2001
*! Purpose   : Calculate Actual production time.
*!**************************************************************************
*: Refer to C(102233)
FUNCTION lfCalcEst
PARAMETER lnOrdTot
*IF EMPTY(ICDESIGN.CCOLORCHG) OR ICDESIGN.NSTITCHES = 0
  *RETURN 'N/A'
IF EMPTY(ICDESIGN.CCOLORCHG) .OR. ALLTRIM(ICDESIGN.CCOLORCHG) = "0" .OR. ;
   ALLTRIM(ICDESIGN.CCOLORCHG) > ALLTRIM(STR(FCOUNT("PRODUCTN")-1)) .OR. ;
   ICDESIGN.NSTITCHES = 0
  IF lcRpKind = 'D'
    RETURN 'N/A'
  ELSE
    RETURN 0
  ENDIF
*B606331,1 RAE [end]  
ENDIF
PRIVATE lnAlias
lnAlias = SELECT()
*--No of Runs = Open Qty/12 + 1
lnNoOfRun = ROUND((lnOrdTot/12),0) + 1
*--Production Time = No of Runs * Mininutes per run (Come from excel sheet)
SELECT PRODUCTN
=SEEK('  8000')
lcClrChg = ICDESIGN.CCOLORCHG
lnMnPrRun = PRODUCTN.ClrChg5
lnPrdTime = lnNoOfRun*lnMnPrRun
lnADsnRun = lnADsnRun + lnPrdTime
lnATotRun = lnATotRun + lnPrdTime
SELECT (lnAlias)
RETURN lnPrdTime

*!**************************************************************************
*! Name      : lfZeroit
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/03/2001
*! Purpose   : Zero out totals
*!**************************************************************************
*: Refer to C(102233)
FUNCTION lfZeroit
PARAMETERS lnEst


IF lnEst = 1
  STORE 0 TO lnDsnRun
ELSE  
  STORE 0 TO lnADsnRun
ENDIF  
RETURN 0

FUNCTION lftry

lnFinal  = lnFinal + lnADsnRun
lnFinal1 = lnFinal1 + lnDsnRun
RETURN 0

FUNCTION lfPutLineNo
LOCATE
lcOrder = ORDER
lcSrty  = cStylePos
SCAN
  IF ORDER <> lcOrder or  cStylePos <> lcSrty
    SKIP -1
    REPLACE lLastLine WITH .T.
    SKIP
    lcOrder=ORDER
    lcSrty  = cStylePos
  ENDIF
ENDSCAN
IF EOF() AND !BOF()
 SKIP -1
ENDIF
REPLACE lLastLine WITH .T.