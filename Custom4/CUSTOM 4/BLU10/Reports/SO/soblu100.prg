*!*	Functions to use with aria4xp 
*!*	gfSeek
*!*	gfSetOrder
*!*	gfSqlRun
*!*	gfOpenTable

*!*	no relations used

*:***************************************************************************
*: Program file  : SIBLU100.PRG (Copied from v:\..\Sorddet.PRG with needed modifications)
*: Program desc. : Order Detail Report
*: For Report    : (SOBL100A.FRX,SOBL100B.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Tarek Mohammed Ibrahim
*: Refer to      : *T20070131.0011 TMI 04/10/2007
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
*:***************************************************************************
loOGScroll.cCROrientation = 'L'




* Define fixed array filter from the one from the option grig object
DIMENSION laOGFxFlt[ALEN(loOgScroll.laOgFxFlt,1),ALEN(loOgScroll.laOgFxFlt,2)]
=ACOPY(loOgScroll.laOgFxFlt,laOgFxFlt)

*- Open files using global sql functions
=lfOpenFiles()

lcTime     = TIME()                     && Variable to hold the Time
lcStTime   = lcTime                     && Time in which we start collect data.
llPrntBoth = llRpOrdNot AND llRpOrdLnt  && True if you print both types of notes.
lnLastRec  = 2                          && Record No. Of last record in order group.
lcTitle    = ''                         && Title of Note. 
lcNotes    = ''                         && Notes.
llNoIndex  = .F.                        && I don't make index for file.

*--Define variables used in Print dos format and multi currency format [Begin
lcPhonPict = gfPhoneTem()
*-- Print totals if and only if [Not multi currency or user want any equavelent method or 
*-- select only one currency to print]
llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
             ((!EMPTY(laOGFxFlt[lnCurrPos,6]) AND ATC("|",laOGFxFlt[lnCurrPos,6])=0) OR ;
              (!EMPTY(laOGFxFlt[lnOrdPos ,6]) AND USED(laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(laOGFxFlt[lnOrdPos,6])=1))
llCurInGrp = !llPrintTot

* Initialize variables for sizes and define var in Syrepuvr [Start]
*--lnScaleLen                 variable to get width of ext. size scale (defined in Syrepuvr)
STORE 0 TO m.Size1 , m.Size2 , m.Size3 , m.Size4 , m.Size5 , m.Size6 , m.Size7 , m.Size8

STORE 0 TO m.qty9  , m.qty10  , m.qty11  , m.qty12  , m.qty13  , m.qty14

STORE '' TO lcLineCurr
STORE .T. TO llInnTotal,llOutTotal
llGrdTotal = llPrintTot
lcSeekVal  = ''                         && Get Last record seek value.
*- Is the compan using Extended Size Scale.
llExtSizSc = gfGetMemVar('M_USEEXSSC',gcAct_Comp)
DECLARE laSclDesc[14]
laSclDesc = ''
=lfGetSCale()

*-- Add sort by store if first sort is by order [Begin]
lcStorCond = ''
lcOldScale = SPACE(3)
* print all the Stock qty,wip qty
DIMENSION laStock[14],laWip[14]
STORE '' TO laStock,laWip               && Work process arrays
STORE '' TO lcGrpExp,lcSeaExp,lcDivExp,lcStatusEx,lcCatExp

*-- Show messages in status bar when collecting data. [begin]
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON
*-- Show messages in status bar when collecting data. [begin]
*-- if it's first time you run option Grid, i.e: you have unknown variables.

* SET RELATION with scale file [Start]
lnAlase = SELECT()
SELECT ORDLINE
SET RELATION TO 'S' + Scale INTO SCALE    && To scale file.
SELECT (lnAlase)

IF llFrTime
  lcStyTitle = IIF ('GFITEM' $ ALLTRIM(UPPER(lcStyTitle)),;
                    EVALUATE(lcStyTitle),lcStyTitle)  && style title.
  lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.
  *-- Create temporary file that holding order line data. [begin]
  lcWorkFile = loOGScroll.gfTempName()
               
  lcTempLine = loOGScroll.gfTempName()
  
  *C101889,1 (Begin) If the company using Extended Size Scale then initialize temp file for scales.
  
  DIMENSION laTempStru[1,18]
  SELECT ORDLINE
  =AFIELDS(laTempStru)
  lnOldLen = ALEN(laTempStru,1)

  *-- cTempKey :  field used in most sort by case as the master key ,
  *--          :  and in case of summarize multi store as the total amount.
  laNewArrLen = ALEN(laTempStru,1)+IIF(llExtSizSc,32,2)
  DIMENSION laTempStru[laNewArrLen, 18]
  
  lnLen = lnOldLen
  lnLen = lnLen + 1
  laTempStru[lnLen,1] = 'cTempKey'
  laTempStru[lnLen,2] = 'C'
  laTempStru[lnLen,3] = 16
  laTempStru[lnLen,4] = 0
  
  *-- cCurrCode :  used if multi currency only to sort by it.
  lnLen = lnLen + 1
  laTempStru[lnLen,1] = 'cCurrCode'
  laTempStru[lnLen,2] = 'C'
  laTempStru[lnLen,3] = 3
  laTempStru[lnLen,4] = 0

  IF llExtSizSc
  
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'nAmountt'
    laTempStru[lnLen,2] = 'n'
    laTempStru[lnLen,3] = 15
    laTempStru[lnLen,4] = 3

    *--Scale Cnt
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Cnt'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 2
    laTempStru[lnLen,4] = 0

    *--qTY9
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'QTY9'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 6
    laTempStru[lnLen,4] = 0

    *--qTY10
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'QTY10'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 6
    laTempStru[lnLen,4] = 0

    *--qTY11
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'QTY11'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 6
    laTempStru[lnLen,4] = 0

    *--qTY12
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'QTY12'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 6
    laTempStru[lnLen,4] = 0

    *--qTY13
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'QTY13'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 6
    laTempStru[lnLen,4] = 0

    *--qTY14
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'QTY14'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 6
    laTempStru[lnLen,4] = 0

    *--wip9
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'WIP9'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 6
    laTempStru[lnLen,4] = 0

    *--wip10
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'WIP10'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 6
    laTempStru[lnLen,4] = 0

    *--wip11
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'WIP11'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 6
    laTempStru[lnLen,4] = 0

    *--wip12
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'WIP12'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 6
    laTempStru[lnLen,4] = 0

    *--wip13
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'WIP13'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 6
    laTempStru[lnLen,4] = 0

    *--wip14
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'WIP14'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 6
    laTempStru[lnLen,4] = 0
    
    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'lMulPrice'
    laTempStru[lnLen,2] = 'L'
    laTempStru[lnLen,3] = 1
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size1'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size2'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size3'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size4'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size5'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size6'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size7'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size8'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size9'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size10'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size11'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size12'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size13'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'Size14'
    laTempStru[lnLen,2] = 'C'
    laTempStru[lnLen,3] = 5
    laTempStru[lnLen,4] = 0

    lnLen = lnLen + 1
    laTempStru[lnLen,1] = 'SzCount'
    laTempStru[lnLen,2] = 'N'
    laTempStru[lnLen,3] = 3
    laTempStru[lnLen,4] = 0
  ENDIF
 
  FOR lnI = lnOldLen+1 TO ALEN(laTempStru,1)
    STORE .F. TO laTempStru[lnI,5],;
                 laTempStru[lnI,6]
    STORE ''  TO laTempStru[lnI,7],;
                 laTempStru[lnI,8],;
                 laTempStru[lnI,9],;
                 laTempStru[lnI,10],;
                 laTempStru[lnI,11],;
                 laTempStru[lnI,12],;
                 laTempStru[lnI,13],;
                 laTempStru[lnI,14],;
                 laTempStru[lnI,15],;
                 laTempStru[lnI,16]
    STORE 0   TO laTempStru[lnI,17],;
                 laTempStru[lnI,18]
  ENDFOR
  
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
  lcNoteLns = loOGScroll.gfTempName()
  *-- create temp. file that used if you have both types of notes. [begin]
  CREATE CURSOR (lcNoteLns)  (cRecord C(2))
  INDEX ON cRecord TAG (lcNoteLns) OF (gcWorkDir+lcNoteLns)
  FOR lnI = 1 TO 2
    APPEND BLANK
   REPLACE cRecord WITH "N"+ALLTRIM(STR(lnI))
  ENDFOR
  
  *-- create temp. file that used if you have both types of notes. [end]
  *-- Create work file.  
  =gfCrtTmp(lcWorkFile,@laTempStru)  && Create work cursor.
  =gfCrtTmp(lcTempLine,@laTempStru)  && Create line cursor.

ENDIF
*-- Create temporary cursors from structure array. [end]
=lfGetRepVr()      && Get Report variables such as groups and index.
*-- If user change report critria, Collect report data. 
*-- Use variable llOGFltCh that detect OG filter changes. [Begin

IF llClearFn OR llOGFltCh
  llClearFn = .F.
  lcStartSt = DTOS(CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],1,;
                  ATC('|',laOGFxFlt[lnStartPos,6])-1)))
  lcStartEd = DTOS(CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],;
                   ATC('|',laOGFxFlt[lnStartPos,6])+1)))
  *-- Depend on both sides Flag when collecting data [Begin]
  llSrtSides = EMPTY(ALLTRIM(lcStartSt+lcStartEd))
  lcCompSt  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],1,;
                   ATC('|',laOGFxFlt[lnCompPos,6])-1)))
  lcCompEd  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],;
                   ATC('|',laOGFxFlt[lnCompPos,6])+1)))

  *-- Depend on both sides Flag when collecting data [Begin]
  llCmpSides = EMPTY(ALLTRIM(lcCompSt + lcCompEd))
  lcStatusEx = [ORDHDR.STATUS $ lcRpStatus]
  
  *-- All in list codes Changed to be $ not INLIST function
  *-- 1- Style group. [Begin]
  *-- if user select Style group, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnGrpPos,6])
    lcGrpExp  = "&laOGFxFlt[lnGrpPos,1]." + ' $ laOGFxFlt[lnGrpPos,6]'
  ENDIF  && end if user select Style group, evaluate its expression.
  *-- if user select Season, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnSeaPos,6])
    lcSeaExp  = "&laOGFxFlt[lnSeaPos,1]." + ' $ laOGFxFlt[lnSeaPos,6]'
  ENDIF  && end if user select Season, evaluate its expression.
  *-- if user select Division, evaluate its expression.
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

*:B803902,1 MHM 01/01/2001 [Start]
SET RELATION OFF INTO SCALE  && break relation.
*:B803902,1 MHM 01/01/2001 [end]

*-- If Sort by Sales Rep. , SET RELATION to Primary sales rep. file.
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
*-- If sort by style group , SET RELATION to codes file.
IF lcRpSortBy = 'G'
  SET RELATION TO gcAct_Comp+SUBSTR(cTempKey,8,6) INTO CODES ADDITIVE
ENDIF  && end If sort by style group.
*-- If sort by fabric , SET RELATION to fabric file.
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
*-- if dos mode do the following [End
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMastFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' TIMEOUT 1

SELECT (lcMastFile)
DELETE FOR TOTQTY = 0

*!*	*T20070131.0011 TMI [Start] 
*!*	IF llOGFltCh
*!*	  _SCREEN.Visible = .T.
*!*	  DEBUG 
*!*	  SUSPEND
*!*	ENDIF
*!*	*T20070131.0011 TMI [End  ] 

DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRepExpr

WAIT CLEAR

*-- If Sort by Sales Rep. , SET RELATION to Primary sales rep. file.
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
*-- end of Report Code.
*-- Function section 

*-------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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

  DECLARE laRpSource[3],laRpTarget[1]
  STORE 'Open'     TO laRpSource[1]
  STORE 'Hold'     TO laRpSource[2]
  STORE 'Canceled' TO laRpSource[3]
  lcRpStatus = 'OHX'

  =lfSetOrder('ORDHDR','ORDHDR')          && To use it to validate ORDER   # in option grid.
  =lfSetOrder('CUSTOMER','CUSTOMER')      && To use it to validate ACCOUNT # in option grid.
  =lfSetOrder('STYLE','STYLE')            && To use it to validate STYLE   # in option grid.
  =lfSetOrder('SALESREP','SALESREP')      && To use it to validate REP     # in option grid.
  =lfSetOrder('WAREHOUS','WAREHOUS')      && To use it to validate LOCATION# in option grid.
  IF 'MA' $ gcCmpModules
    =lfSetOrder('FABRIC','FABRIC')        && To use it to validate FABRIC  # in option grid.
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

  IF llMultCurr
    =lfSetOrder('SYCCURR','CCURRCODE')      && To VALIDATE currency code.    
    lnCurrPos  = lfItmPos('ORDHDR.CCURRCODE')
  ENDIF
ELSE
  *FOR lnElm = 1 TO ALEN(laOgObjType,1) 
   *IF laOgObjType[lnElm,1] == "lcRpSortBy"
   *  _CUROBJ= OBJNUM(&laOgObjType[lnElm,2].)+1
   *ENDIF
   *IF lcDummy = "Y" AND laOgObjType[lnElm,1] == "lcRpSelcBy"
   *  _CUROBJ= OBJNUM(&laOgObjType[lnElm,2].)+1
   *  lcDummy = "N"
   *ENDIF
  *ENDFOR
ENDIF  && END IF you first time enter when function.

*B603946,1 Clear SELECT BY if user change only SELECT BY option from 
*B603946,1 one type to another or user presses RESET button [Begin]
*STORE .T. TO llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
IF lcRpSelcBy = "L" OR !(lcRpSelcBy == lcOldSelc)
  STORE .T. TO llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
ENDIF
lcOldSelc = lcRpSelcBy

IF lcRpSortBy = "O" AND lcRpKind = "D" AND ALEN(laSort2Des,1) = 2
  DIMENSION laSort2Des[4,1] , laSort2Val[4,1]
  laSort2Des[3,1] = "Store/Line#"
  laSort2Des[4,1] = "Store/" + lcStyMajor
  laSort2Val[3,1] = "T"
  laSort2Val[4,1] = "Y"  
  ClearRead()
ENDIF

*!*************************************************************
*! Name      : lfScanData
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
  SELECT (lcTempLine)
  ZAP
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

*C101889,1 (Begin) Zap temp scal file.

*:B803902,1 MHM 01/01/2001 [Start]
*IF llExtSizSc AND RECCOUNT(lcScalTmp) > 0
*  SELECT (lcScalTmp)
*  ZAP
*ENDIF
*:B803902,1 MHM 01/01/2001 [end]

*C101889,1 (End)
SELECT (lcWorkFile)
IF RECCOUNT(lcWorkFile) > 0
  *-- We need temp. files to be files not cursor to open it in another alias [Begin]
  SELECT (lcWorkFile)
  ZAP
  *--Rest any relation before data collection.
  SET RELATION TO
  *-- We need temp. files to be files not cursor to open it in another alias [End  ]
  SELECT (lcWorkFile)
ENDIF
INDEX ON &lcWorkTag TAG (lcWorkFile)
*-- Relation with master order file to help data collecting. [begin]
SELECT OrdLine

*:B803902,1 MHM 01/01/2001 [Start]
*SET RELATION TO cOrdType + Order INTO OrdHdr 
SET RELATION TO cOrdType + Order INTO OrdHdr ADDITIVE
*:B803902,1 MHM 01/01/2001 [end]

SET RELATION TO style INTO Style ADDITIVE
*-- Relation with master order file to help data collecting. [end]

*-- llWorkDeal : Flag to know that we start dealing with work file.
*-- llLineDeal : Flag to know that we deal with temp. line file.
STORE .F. TO llWorkDeal , llLineDeal
lcTOrdFile = laOGFxFlt[lnOrdPos,6]
llWorkDeal = !EMPTY(lcTOrdFile) AND USED(lcTOrdFile) AND RECCOUNT(lcTOrdFile) > 0

*T20070131.0011 TMI [Start] To avoide the problem that when selecting some records and then unselect them without clear the file is not packed
*                           and still shows that RECCOUNT(lcTOrdFile) is greater than 0
IF llWorkDeal
  GO TOP IN &lcTOrdFile
  llWorkDeal = !EOF(lcTOrdFile)
ENDIF
*T20070131.0011 TMI [End  ] 

*-- If user select specific orders, collect data of this orders only. [begin]
IF llWorkDeal
  SELECT (lcTOrdFile)
  *-- Scan order cursor.
  SCAN  
    SELECT ORDLINE
    *T20070131.0011 TMI [Start] 
    =lfSetOrder('ORDLINE','ORDLINE')
    *T20070131.0011 TMI [End  ] 
    
    *-- if find first order record in ordline file and <ordhdr filter>
    IF SEEK('O'+&lcTOrdFile..ORDER) AND ;
       EVALUATE(lcStatusEx) AND ;
       (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
       IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
       IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
       IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)

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
          *C101889,1 (Begin) The user use extended size scale having 14 sizes that will be printed in one line.
          *:B803902,1 MHM 01/01/2001 function to get sizes [Start] 
          =lfExSzis()
          *:B803902,1 MHM 01/01/2001 function to get sizes [End] 
          IF llExtSizSc
            =lfExtenSz(lcWorkFile)
          ELSE
            INSERT INTO (lcWorkFile) FROM MEMVAR
          ENDIF  
          *C101889,1 (End)
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
  *T20070131.0011 TMI [Start] 
  IF llLineDeal
    GO TOP IN &lcSlctFile
    llLineDeal = !EOF(lcSlctFile)
  ENDIF
  *T20070131.0011 TMI [End  ] 
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
      SCAN REST WHILE &lcWorkTag = lcScaned

        *-- if Summarize multi store orders.
        *:B803902,1 MHM 01/01/2001 function to get sizes [Start] 
        =lfExSzis()
        *:B803902,1 MHM 01/01/2001 function to get sizes [End] 
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
    SELECT ORDLINE
    =lfSetOrder('ORDLINE',lcOrdVar)
    
    *-- if select by account.
    IF lcRpSelcBy = 'A'
      =lfSetOrder('ORDHDR','ORDACCT')
    ELSE
      =lfSetOrder('ORDHDR','ORDHDR')
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
              IF CORDTYPE = 'O' AND ;
                 EVALUATE(lcStatusEx) AND ;
                 (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                 IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                 SEEK('O'+ORDER,'ORDLINE')
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
                    *C101889,1 (Begin) The user use extended size scale having 14 sizes that will be printed in one line.
                    *:B803902,1 MHM 01/01/2001 function to get sizes [Start] 
                    =lfExSzis()
                    *:B803902,1 MHM 01/01/2001 function to get sizes [End] 
                    IF llExtSizSc
                      =lfExtenSz(lcWorkFile)
                    ELSE
                      INSERT INTO (lcWorkFile) FROM MEMVAR
                    ENDIF  
                    *C101889,1 (End)
                  ENDIF  && end if <ordline filter> and <style group filter>
                ENDSCAN  && end scan ordline for rest order lines.
              ENDIF      && end if order type is 'O' , <ordhdr filter>.
            ENDSCAN      && end scan ordhdr file rest for this account.
          ENDIF          && end if you find this account in ordhdr file.
        ENDSCAN  
 
        *-- SET RELATION again.
        =lfSetOrder('ORDHDR','ORDHDR')
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
            SCAN REST WHILE STYLE+DTOS(complete)+cordtype+order+store+STR(lineno,6) = PADR(&lcSlctFile..CSTYMAJOR,lnMajorLen)
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
                SCATTER MEMVAR MEMO
                m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
                m.cCurrCode = ORDHDR.cCurrCode
                *:B803902,1 MHM 01/01/2001 function to get sizes [Start] 
                =lfExSzis()
                *:B803902,1 MHM 01/01/2001 function to get sizes [End] 
                IF llExtSizSc
                  =lfExtenSz(lcWorkFile,.T.)
                ELSE
                  INSERT INTO (lcWorkFile) FROM MEMVAR
                ENDIF  
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
                IF CORDTYPE = 'O' AND TotQty > 0 AND ;
                   EVALUATE(lcStatusEx) AND ;
                   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
                  SCATTER MEMVAR MEMO
                  m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
                  m.cCurrCode = ORDHDR.cCurrCode
                  *C101889,1 (Begin) The user use extended size scale having 14 sizes that will be printed in one line.
                  *:B803902,1 MHM 01/01/2001 function to get sizes [Start] 
                  =lfExSzis()
                  *:B803902,1 MHM 01/01/2001 function to get sizes [End] 
                  IF llExtSizSc
                    =lfExtenSz(lcWorkFile,.T.)
                  ELSE
                    INSERT INTO (lcWorkFile) FROM MEMVAR
                  ENDIF  
                  *C101889,1 (End)
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
          SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
                        CORDTYPE = 'O' AND  ;
                        EVALUATE(lcStatusEx) AND ;
                        (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                        IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                        IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
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
                *C101889,1 (Begin) The user use extended size scale having 14 sizes that will be printed in one line.
                *:B803902,1 MHM 01/01/2001 function to get sizes [Start] 
                =lfExSzis()
                *:B803902,1 MHM 01/01/2001 function to get sizes [End] 
                IF llExtSizSc
                  =lfExtenSz(lcWorkFile)
                ELSE
                  INSERT INTO (lcWorkFile) FROM MEMVAR
                ENDIF  
                *C101889,1 (End)
              ENDIF    && end if <ordline filter>
            ENDSCAN
          ENDSCAN      && end scan ordhdr for this sales rep.
        ENDSCAN  

        *-- Refilter ordhdr file [begin]
        SELECT ORDHDR
        SET FILTER TO
        *-- Refilter ordhdr file [end]

        *-- SET RELATION again.
        SELECT ORDLINE 
        SET RELATION TO cOrdType + Order INTO OrdHdr

      CASE lcRpSelcBy = 'R'   && Sales rep. case

        SELECT ORDLINE 
        SET RELATION OFF INTO ORDHDR  && break relation.

        SELECT (lcSlctFile)
        SCAN
          SELECT ORDHDR
          SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
                        CORDTYPE = 'O' AND ;
                        EVALUATE(lcStatusEx) AND ;
                        (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                        (EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
                        (EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
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
                *C101889,1 (Begin) The user use extended size scale having 14 sizes that will be printed in one line.
                *:B803902,1 MHM 01/01/2001 function to get sizes [Start] 
                =lfExSzis()
                *:B803902,1 MHM 01/01/2001 function to get sizes [End] 
                IF llExtSizSc
                  =lfExtenSz(lcWorkFile)
                ELSE
                  INSERT INTO (lcWorkFile) FROM MEMVAR
                ENDIF  
                *C101889,1 (End)
              ENDIF    && end if <ordline filter>
            ENDSCAN
          ENDSCAN      && end scan ordhdr for this sales rep.
        ENDSCAN  

        *-- Refilter ordhdr file [begin]
        SELECT ORDHDR
        SET FILTER TO
        *-- Refilter ordhdr file [end]

        *-- SET RELATION again.
        SELECT ORDLINE 
        SET RELATION TO cOrdType + Order INTO OrdHdr

    ENDCASE
    lcMastFile = lcWorkFile
  ELSE  && user does not use any select type.
    *-- if user does not select any orders [no data found],
    *-- in this case we select all file.
    IF (RECCOUNT(lcWorkFile) = 0) AND !llWorkDeal
      =lfSetOrder('ORDLINE','')
*      SET ORDER TO    && To activate rushmore.

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
            SCATTER MEMVAR MEMO
            m.cCurrCode = ORDHDR.cCurrCode
            *C101889,1 (Begin) The user use extended size scale having 14 sizes that will be printed in one line.
            *--If its the same order and style/color and the scale is different, Update from 9 :14 size
            *:B803902,1 MHM 01/01/2001 function to get sizes [Start] 
            =lfExSzis()
            *:B803902,1 MHM 01/01/2001 function to get sizes [End] 
            IF llExtSizSc
              =lfExtenSz(lcTempLine,.T.)
            ELSE
              = lfSumMulti(lcSeekExp)  && summarize data.
              INSERT INTO (lcTempLine) FROM MEMVAR
            ENDIF  
            *C101889,1 (End)
          ENDIF    && end if you does not find line in temp line file,
        ENDSCAN    && end scan file for full index expression (rushmore).
        USE IN SUMMULTI

      ELSE  && NoPrmal collection case for all data in ordline file.
        *-- scan ordline file for full index expression (rushmore)
        SCAN FOR CORDTYPE + ORDER + STR(LINENO,6) = 'O'
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
            SCATTER MEMVAR MEMO
            m.cTempKey = PADR(STYLE.FABRIC,7)+PADR(STYLE.CSTYGROUP,6)+PADR(ORDHDR.REP1,3)
            m.cCurrCode = ORDHDR.cCurrCode
            *C101889,1 (Begin) The user use extended size scale having 14 sizes that will be printed in one line.
            *--If its the same order and style/color and the scale is different, Update from 9 :14 size
            *:B803902,1 MHM 01/01/2001 function to get sizes [Start] 
            =lfExSzis()
            *:B803902,1 MHM 01/01/2001 function to get sizes [End] 
            IF llExtSizSc
              =lfExtenSz(lcTempLine)
            ELSE
              INSERT INTO (lcTempLine) FROM MEMVAR
            ENDIF  
            *C101889,1 (End)
          ENDIF    && end if <ordhdr filter>, <ordline filter> and <style group filter>
        
        ENDSCAN    && end scan file for full index expression (rushmore).
      ENDIF        && end if summarize multi store orders.
      SELECT ORDLINE
      =lfSetOrder('ORDLINE','ORDLINE')
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
      *:B803902,1 MHM 01/01/2001 function to get sizes [Start] 
      =lfExSzis()
      *:B803902,1 MHM 01/01/2001 function to get sizes [End] 
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
*-- end of lfScanData.

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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

laOldVal = EVALUATE(OGSYS18())  && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvSelcBy
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
llClearSty = (lcRpSelcBy # 'S')
llClearFab = (lcRpSelcBy # 'F')
llClearLoc = (lcRpSelcBy # 'L')
llClearRep = (lcRpSelcBy # 'R')
ClearRead()
*-- end of lfvSelect.

*!*************************************************************
*! Name      : lfSelcObjs
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
llRpScale  = IIF(lcRpSortBy $ 'SFG',.F.,.T.)

llRpSummMt = IIF(lcRpSortBy <> 'S' ,.F.,llRpSummMt)
lcRpStyPrn = IIF(lcRpSortBy <> 'S' ,'N',lcRpStyPrn)
llRpOrdNot = IIF(!(lcRpSortBy $ 'AO'),.F.,llRpOrdNot)
ClearRead()

*-- Notes [begin]
*-- if In case of sort by (STYLE,FABRIC OR STYLE GROUP) and old value is
*-- another sort type and user want to print sizes, we must disable
*-- reprint scale when diff. because it is the normal print case here,
*-- and vice versa.
*-- Notes [end]

IF lcRpSortBy != laOldVal

  *-- Different sort by cases.
  DO CASE
    CASE lcRpSortBy = 'A'   && Sort by account 

      *-- if report kind is detail
      IF lcRpKind = 'D'
        IF laOldVal = 'S'
          llChSumm = IIF(llRpSummMt,.T.,llChSumm)  && Rise summarize flag.
        ENDIF
      ENDIF
      *-- Enable/disable variable objects due to sort case. [end]

    CASE lcRpSortBy = 'O'    && Sort by order

      IF lcRpKind = 'D'
        IF laOldVal = 'S'
          llChSumm = IIF(llRpSummMt,.T.,llChSumm)
        ENDIF
      ENDIF
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'S'      && Sort by style
      IF lcRpKind = 'D'
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
*-- end of lfvSortBy.


*!*************************************************************
*! Name      : lfPreObjs
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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

IF lcRpKind = 'D'
  IF laOldVal = 'S'

    llChSumm = IIF(llRpSummMt,.T.,llChSumm)
  ENDIF
ENDIF
*-- end of lfPreObjs.

*!*************************************************************
*! Name      : lfObjState
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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

lcRpForm = IIF(llRpSizes,'SOBL100A','SOBL100B')
= lfRepPltFr(lcRpForm)
llRpScale = IIF(llRpSizes,.T.,.F.)

ClearRead()

*-- end of lfvSizes.

*!*************************************************************
*! Name      : lfvKind
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
ClearRead()


*-- end of lfvKind.

*!*************************************************************
*! Name      : lfvSumm
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
ClearRead()


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
    *T20070131.0011 TMI [Start] 
    =lfSetOrder('STYLE','Cstyle')
    *T20070131.0011 TMI [End  ] 
    
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    *T20070131.0011 TMI [Start] 
    =lfSetOrder('STYLE','STYLE')
    *T20070131.0011 TMI [End  ] 
    
    llClearSty = .F.
  OTHERWISE      && Valid code
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*-- end of lfStySum.


*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
    *T20070131.0011 TMI [Start] 
    =lfSetOrder('FABRIC','cFabric')
    *T20070131.0011 TMI [End  ] 
    
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    *T20070131.0011 TMI [Start] 
    =lfSetOrder('FABRIC','FABRIC')
    *T20070131.0011 TMI [End  ] 
    
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    lcCustRel = IIF(llRpSummMt,['M' + Account],;
                [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])

    *T20070131.0011 TMI [Start] 
    =lfSetOrder('CUSTOMER','Customer')
    *T20070131.0011 TMI [End  ]
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
    GO TOP
  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClearOrd = .F.

ENDCASE
*-- end of lfsChOrder.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*C101889,1 (Begin) If the company using Extended Size Scale then sum 9:14 also.
IF !llExtSizSc
  SUM  Qty1 , Qty2 , Qty3 , Qty4 , Qty5 , Qty6 , Qty7 , Qty8 , TotQty , TotQty*Price ;
   TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,m.cTempKey   ;
   REST WHILE Style + DTOS(Complete) + cordtype + order = lcSumExpr  
ELSE
  SUM  Qty1 , Qty2 , Qty3 , Qty4 , Qty5 , Qty6 , Qty7 , Qty8 ,Qty9,Qty10,Qty11,Qty12,;
       Qty13,Qty14, TotQty , TotQty*Price ;
   TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Qty9,m.Qty10,;
       m.Qty11,m.Qty12,m.Qty13,m.Qty14,m.TotQty,m.cTempKey   ;
   REST WHILE Style + DTOS(Complete) + cordtype + order = lcSumExpr  
ENDIF   
*C101889,1 (End)

m.cTempKey = STR(m.cTempKey,16,2)  && Total amount.
GO lnRecNum IN (lcSumFile)
*-- end of lfSumStyle.

*!*************************************************************
*! Name      : lfSumMulti
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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

PRIVATE lnSelect
lnSelect = SELECT(0)
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
           m.Qty7,m.Qty8,m.Qty9,m.Qty10,m.Qty11,m.Qty12,m.Qty13,m.Qty14,m.TotQty,m.cTempKey

SELECT SUMMULTI  && Order line alias (sum for all file)
= SEEK(lcSumExpr)
*C101889,1 (Begin) If the company using Extended Size Scale then sum 9:14 also.
IF !llExtSizSc
  SUM  Qty1 , Qty2 , Qty3 , Qty4 , Qty5 , Qty6 , Qty7 , Qty8 , TotQty , TotQty*Price ;
   TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty, m.cTempKey   ;
   REST WHILE Style + DTOS(Complete) + cordtype + order = lcSumExpr
ENDIF   
*C101889,1 (End)
m.cTempKey = STR(m.cTempKey,16,2)
SELECT (lnSelect)
*-- end of lfSumMulti.

*!*************************************************************
*! Name        : lfGetNotes
*! Developer   : Adel Mohammed El Gazzar (ADEL)
*! Date        : 05/27/1998
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : SOBL100A.FRX, OR SOBL100B.FRX [Variable lcDum in the report]
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
  *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
  DO CASE 
    CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcMastFile..Note_Mem))
      lcTitle = 'Order : ' + ORDER + ' - Style : ' + STYLE + ' - Line #  ' + ALLTRIM(STR(LINENO)) + '    Notes.'
      lcNotes  =  ALLTRIM(Note_Mem) + CHR(10)
    CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcTitle = IIF(RECNO(lcMastFile) = lnLastRec,;
                'Order :' + ORDER + ' Notepad.','')
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
      lcNotes  =  ALLTRIM(Note_Mem) + CHR(10)
    CASE llRpOrdNot AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcTitle = IIF(RECNO(lcMastFile) = lnLastRec,;
                'Order :' + ORDER + ' Notepad.','')
      lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10),'')
  ENDCASE
ENDIF
RETURN ''
*-- end of lfGetNotes.

*!*************************************************************
*! Name      : lfLastRec
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
*! Purpose   : Calculate last Record in order details. [ORDER GROUP]
*!           : we use another alias to unchange record pointer of report file.
*!*************************************************************
*! Called from : [SOBL100A.FRX OR SOBL100B.FRX, ORDER GROUP HEADER BAND] 
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
*! Name      : lfvCoorGrp
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]


*-- if multi currency evaluate currency arrays [Begin]
IF llMultCurr
  DIMENSION laCurrVal[1,1]

  IF !USED('SYCCURR')
    =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1],laCurrSmbl[ALEN(laCurrVal,1),1]

  SELECT SYCCURR
  *T20070131.0011 TMI [Start] 
  =lfSetOrder('SYCCURR','CCURRCODE')  && To VALIDATE currency code.
  *T20070131.0011 TMI [End  ] 
  
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrSmbl[lnI,1] = ALLTRIM(PADR(CCURRSMBL,3))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF
RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfNMajType
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Name      : lfGetRepVr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*-- lcOutHeadL : Left  title of outer group header.
*-- lcOutHeadR : Right title of outer group header.  
*-- lcInnHeadL : Left  title of inner group header.
*-- lcInnHeadR : Right title of inner group header.
*B607041,1 WAB (Start) - no need to print he Scale Title
lcStyTitle = SUBSTR(lcStyTitle,1,lnMajorLen + 1+lnColorlen)
*B607041,1 WAB (End)

IF lcRpSortBy = 'S'
  lcOutHeadL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen),19) +  '   : ']
  lcOutHeadR = [Style.cStyMajor]
  lcInnHeadL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2,lnColorLen-2),19)+ '    : ']
  lcInnHeadR = [SUBSTR(Style,lnMajorLen + 2,lnColorlen) + '  ----  ' + ALLTRIM(STYLE.Desc1)]
ELSE
  STORE [''] TO lcOutHeadL,lcOutHeadR,lcInnHeadL,lcInnHeadR
ENDIF

lcLineCurr = lfCurrPrnt()

*-- Different sort by cases.
DO CASE
  CASE lcRpSortBy = 'A'   && Sort by account 

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
    *-- report variables data account case [begin]
    lcSubTitle = 'Account Number'  && Report sort title
    lcInnGrp   = [ORDER]           && Inner .FRX group field.
    lcInnFootL = ['Order # ' + ORDER + '     Cust PO #']    
    lcInnFootR = [IIF(Ordhdr.MultiPo,'*Multi PO*', PADL(OrdHdr.CustPo,15)) + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]  && Right title of inner group footer.
    lcOutFootL = ['Account # ' + Customer.Account]  && Left title of outer group footer.
    lcOutFootR = [PADR(ALLTRIM(CUSTOMER.BtName),25) +"/"+EVALUATE(lcLineCurr)+ ", " + TRANSFORM(CUSTOMER.Phone1,lcPhonPict)]  && Right title of outer group footer.
    IF llCurInGrp
      lcSeekVal  = [ACCOUNT+cCurrCode+'O'+ORDER]
      lcOutGrp   = [ACCOUNT+CCURRCODE]         && Outer .FRX group field.
    ELSE
      lcSeekVal  =  [ACCOUNT+'O'+ORDER]
      lcOutGrp   = [ACCOUNT]         && Outer .FRX group field.
    ENDIF  
  CASE lcRpSortBy = 'O'    && Sort by order
    lcSubTitle = 'Order'
    lcInnGrp   = ['']
    lcInnFootL = [''] 
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
        
        lcStorCond = [ORDHDR.MULTI="Y"]
        lcInnGrp   = [IIF(EVALUATE(lcStorCond),STORE,"")]
        lcInnFootL = [IIF(EVALUATE(lcStorCond),"Store : "+STORE,"")] 

    ENDCASE
    lcInnFootR = ['']
    IF lcRpSrt2 $ 'TY'    
      *lcOutFootL = [EVALUATE(lcLineCurr)+ "/"+'Order # ' + Order + IIF(EVALUATE(lcStorCond),' Account:',' Store :')]
      lcOutFootL = [EVALUATE(lcLineCurr)+ "/"+'Order#' + Order + IIF(EVALUATE(lcStorCond),' Account:',' Store :')]
      lcOutFootR = [IIF(EVALUATE(lcStorCond),Account,Store) + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]
    ELSE
      *lcOutFootL = [EVALUATE(lcLineCurr)+ "/"+'Order # ' + Order + '    Account # ']
      lcOutFootL = [EVALUATE(lcLineCurr)+ "/"+'Order#' + Order + '    Account # ']
      lcOutFootR = [Account  + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]
    ENDIF  
    IF llCurInGrp
      lcSeekVal = [cCurrCode+'O'+ORDER]
      lcOutGrp  = [CCURRCODE+ORDER]
    ELSE
      lcSeekVal = ['O'+ORDER]
      lcOutGrp  = [ORDER]
    ENDIF  
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
    lcStyTitle = IIF ('GFITEM' $ ALLTRIM(UPPER(lcStyTitle)),;
                      EVALUATE(lcStyTitle),lcStyTitle)

    lcSubTitle = lcStyTitle
    lcInnGrp   = [SUBSTR(Style,lnMajorLen + 2,lncolorlen)]
    lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2,lncolorlen-2),19) + '    : ']
    lcInnFootR = [SUBSTR(Style,lnMajorLen + 2,lnColorlen-2)]
    lcOutFootL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen+lncolorlen),19) + '  : ']
    lcOutFootR = [ALLTRIM(Style.cStyMajor) +"/"+EVALUATE(lcLineCurr)+ ", "+ '( ' + ALLTRIM(Style.Fabric) + ' )']

    *-- report variables data style case [end]
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
    lcInnFootL = ['Style  ( ' + SUBSTR(Style,lnMajorLen + 2,lnColorlen+2)+ ' )  :']
    lcInnFootR = [ALLTRIM(Style.Desc)]
    lcOutFootL = ['Group  ( ' + SUBSTR(cTempKey,8,6) + ' )  :']
    lcOutFootR = [Codes.cDiscrep+"/"+EVALUATE(lcLineCurr)]
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
    lcInnFootL = ['Style  ( ' + SUBSTR(Style,lnMajorLen + 2,lnColorlen+2)+ ' )  :']
    lcInnFootR = [ALLTRIM(Style.Desc)]
    lcOutFootL = ['Fabric  ( ' + LEFT(cTempKey,7) + ' )  :']
    lcOutFootR = [ALLTRIM(Fabric.Desc)+"/"+EVALUATE(lcLineCurr)]
  CASE lcRpSortBy = 'W'    && Sort by location
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
    *-- report variables data location case [begin]
    lcSubTitle = 'Location'
    lcInnGrp   = ['']
    lcInnFootL = [''] 
    lcInnFootR = ['']
    lcOutFootL = ['Location # ' + cWareCode + ' :']
    lcOutFootR = [ALLTRIM(Warehous.cDesc)+"/"+EVALUATE(lcLineCurr)]
    *-- report variables data location case [end]
  CASE lcRpSortBy = 'R'    && Sort by sales representative
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
    IF llCurInGrp
      lcOutGrp   = [RIGHT(cTempKey,3)+CCURRCODE]
    ELSE
      lcOutGrp   = [RIGHT(cTempKey,3)]
    ENDIF  

    *-- report variables data sales Rep. case [begin]
    lcSubTitle = 'Primary Sales Representative'
    lcInnGrp   = ['']

    lcInnFootL = [''] 
    lcInnFootR = ['']
    lcOutFootL = ['Primary Sales Rep. # ' + RIGHT(cTempKey,3) + ' :']
    lcOutFootR = [SalesRep.Name+"/"+EVALUATE(lcLineCurr)]
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

    lcInnFootL = [''] 
    lcInnFootR = ['']
    lcOutFootL = ['******']
    lcOutFootR = [DTOC(Complete)+"/"+EVALUATE(lcLineCurr)]
  CASE lcRpSortBy = 'U'    && Sort by currency

    IF lcRpSrt2 = 'L'    && Sort by Line#
      lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
    ELSE                  && Else Sort by Style	
      lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
	ENDIF
    
    *-- report variables data order case [begin]
    lcSubTitle = 'Currency'
    lcInnGrp   = [ORDER]
    lcInnFootR = ['Order:']
    lcInnFootL = [Order+', Account#'+Account  + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))] 
    lcOutFootL = ['Currency : ']
    lcOutFootR = [lfCurrDesc()]
    lcSeekVal = [cCurrCode+'O'+ORDER]
    lcOutGrp  = [CCURRCODE]
    
ENDCASE          && end Different sort by cases.

IF llCurInGrp AND (lcRpSortBy <> 'U')
  lcSubTitle = lcSubTitle+"/Currency"
ENDIF
*-- end of lfGetRepVr.

*!*************************************************************
*! Name      : lfCreatCur
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
CREATE TABLE (gcWorkDir+lcCurName) ;
   FROM ARRAY laTempStru
*-- We need temp. files to be files not cursor to open it in another alias [End  ]
*-- end of lfCreatCur.

*!*************************************************************
*! Name      : lfPipeExpr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
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
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))

  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpStatus = IIF(EMPTY(lcRpStatus),'OHX',ALLTRIM(lcRpStatus))
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
*-- end of lfvOStatus.

*!*************************************************************
*! Name      : lfGetWork
*! Developer : Adel Mohammed El Gazzar (ADEL)
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
          lnWip1,lnWip2,lnWip3,lnWip4,lnWip5,lnWip6,lnWip7,lnWip8,;
          lnWip9,lnWip10,lnWip11,lnWip12,lnWip13,lnWip14,lnTotWip
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

*B607312,1 WAB (Start) -- we need to put any stok qty under the correct column
  *-- if you print stock or (stock and wip) and total stock not equal 0
  *IF INLIST(lcRpStyPrn,'S','P') AND (lnS9 # 0) 
  *  laStock[1] = IIF(lnS1 = 0 ,'',TRANSFORM(lnS1,'99999')) 
  *  laStock[2] = IIF(lnS2 = 0 ,'',TRANSFORM(lnS2,'99999')) 
  *  laStock[3] = IIF(lnS3 = 0 ,'',TRANSFORM(lnS3,'99999')) 
  *  laStock[4] = IIF(lnS4 = 0 ,'',TRANSFORM(lnS4,'99999')) 
  *  laStock[5] = IIF(lnS5 = 0 ,'',TRANSFORM(lnS5,'99999')) 
  *  laStock[6] = IIF(lnS6 = 0 ,'',TRANSFORM(lnS6,'99999')) 
  *  laStock[7] = IIF(lnS7 = 0 ,'',TRANSFORM(lnS7,'99999')) 
  *  laStock[8] = IIF(lnS8 = 0 ,'',TRANSFORM(lnS8,'99999')) 
  *  laStock[9] = IIF(lnS9 = 0 ,'',TRANSFORM(lnS9,'9999999')) 
  *ENDIF
  *-- if you print wip or (stock and wip) and total wip not equal 0
  *IF INLIST(lcRpStyPrn,'W','P') AND (lnW9 # 0)
  *  laWip[1] = IIF(lnW1 = 0 ,'',TRANSFORM(lnW1,'99999')) 
  *  laWip[2] = IIF(lnW2 = 0 ,'',TRANSFORM(lnW2,'99999')) 
  *  laWip[3] = IIF(lnW3 = 0 ,'',TRANSFORM(lnW3,'99999')) 
  *  laWip[4] = IIF(lnW4 = 0 ,'',TRANSFORM(lnW4,'99999')) 
  *  laWip[5] = IIF(lnW5 = 0 ,'',TRANSFORM(lnW5,'99999')) 
  *  laWip[6] = IIF(lnW6 = 0 ,'',TRANSFORM(lnW6,'99999')) 
  *  laWip[7] = IIF(lnW7 = 0 ,'',TRANSFORM(lnW7,'99999')) 
  *  laWip[8] = IIF(lnW8 = 0 ,'',TRANSFORM(lnW8,'99999')) 
  *  laWip[9] = IIF(lnW9 = 0 ,'',TRANSFORM(lnW9,'9999999')) 
  *ENDIF
  *lnStkOrWip = TRANSFORM(lnS9 + lnW9,'999999')  && Calculate wip + stock values
  *lnStkOrWip = IIF(VAL(lnStkOrWip) = 0 , '' , lnStkOrWip)
*ELSE
  *IF INLIST(lcRpStyPrn,'S','P')
  *  laStock[1] = IIF(STYLE.Stk1 = 0,'',TRANSFORM(STYLE.Stk1,'99999'))
  *  laStock[2] = IIF(STYLE.Stk2 = 0,'',TRANSFORM(STYLE.Stk2,'99999'))
  *  laStock[3] = IIF(STYLE.Stk3 = 0,'',TRANSFORM(STYLE.Stk3,'99999'))
  *  laStock[4] = IIF(STYLE.Stk4 = 0,'',TRANSFORM(STYLE.Stk4,'99999'))
  *  laStock[5] = IIF(STYLE.Stk5 = 0,'',TRANSFORM(STYLE.Stk5,'99999'))
  *  laStock[6] = IIF(STYLE.Stk6 = 0,'',TRANSFORM(STYLE.Stk6,'99999'))
  *  laStock[7] = IIF(STYLE.Stk7 = 0,'',TRANSFORM(STYLE.Stk7,'99999'))
  *  laStock[8] = IIF(STYLE.Stk8 = 0,'',TRANSFORM(STYLE.Stk8,'99999'))
  *  laStock[9] = IIF(STYLE.TotStk = 0,'',TRANSFORM(STYLE.TotStk,'9999999'))
  *ENDIF
  *IF INLIST(lcRpStyPrn,'W','P')
  *  laWip[1] = IIF(STYLE.Wip1 = 0,'',TRANSFORM(STYLE.Wip1,'99999'))
  *  laWip[2] = IIF(STYLE.Wip2 = 0,'',TRANSFORM(STYLE.Wip2,'99999'))
  *  laWip[3] = IIF(STYLE.Wip3 = 0,'',TRANSFORM(STYLE.Wip3,'99999'))
  *  laWip[4] = IIF(STYLE.Wip4 = 0,'',TRANSFORM(STYLE.Wip4,'99999'))
  *  laWip[5] = IIF(STYLE.Wip5 = 0,'',TRANSFORM(STYLE.Wip5,'99999'))
  *  laWip[6] = IIF(STYLE.Wip6 = 0,'',TRANSFORM(STYLE.Wip6,'99999'))
  *  laWip[7] = IIF(STYLE.Wip7 = 0,'',TRANSFORM(STYLE.Wip7,'99999'))
  *  laWip[8] = IIF(STYLE.Wip8 = 0,'',TRANSFORM(STYLE.Wip8,'99999'))
  *  laWip[9] = IIF(STYLE.TotWip = 0,'',TRANSFORM(STYLE.TotWip,'9999999'))
  *ENDIF
  *-- Calculate Wip and Stock Values [End]
  *lnStkOrWip = TRANSFORM(STYLE.TotStk + STYLE.TotWip,'999999')  && Calculate wip + stock values
  *lnStkOrWip = IIF(VAL(lnStkOrWip) = 0 , '' , lnStkOrWip)
  IF SEEK(&lcMastFile..Style,'STYLE') AND SEEK('S'+STYLE.SCALE,'SCALE')
    FOR lnCount = 1 to SCALE.cnt
       lcCount   = STR(lnCount,1)
       lnSize = ASCAN(laSclDesc,SCALE.Sz&lcCount)
      lnSize = IIF(lnSize = 0 ,14,lnSize)
       IF INLIST(lcRpStyPrn,'S','P') 
          laStock[lnSize] = laStock[lnSize] + lnS&lcCount 
        ENDIF
        IF INLIST(lcRpStyPrn,'W','P')
          laWip[lnSize] = laWip[lnSize] + lnW&lcCount 
        ENDIF      
      ENDFOR
  ENDIF
  lnStkOrWip = 0
  FOR lnCount  = 1 TO 14
    lnStkOrWip = lnStkOrWip + laStock[lnCount] + laWip[lnCount] 
    laStock[lnCount] = TRANSFORM(laStock[lnCount],'99999')
    laWip[lnCount]   = TRANSFORM(laWip[lnCount],'99999')
  ENDFOR 
  lnStkOrWip  = IIF(lnStkOrWip = 0 , '' , TRANSFORM(lnStkOrWip,'999999'))
ELSE  && User does not select specific locations.
   lnAlias = SELECT()
   SELECT STYLE
   lnRecNo = RECNO()
   lcStyle = STYLE
   STORE 0 TO laStock,laWip
   SCAN REST WHILE SUBSTR(STYLE,1,lnMajorlen+1+lnColorlen)= SUBSTR(lcStyle,1,lnMajorlen+1+lnColorlen)
      = SEEK('S'+STYLE.SCALE,'SCALE')
      FOR lnCount = 1 to SCALE.cnt
        lcCount   = STR(lnCount,1)
        lnSize = ASCAN(laSclDesc,SCALE.Sz&lcCount)
        lnSize = IIF(lnSize = 0 ,14,lnSize)
        IF INLIST(lcRpStyPrn,'S','P') 
          laStock[lnSize] = laStock[lnSize] + STYLE.Stk&lcCount 
        ENDIF
        IF INLIST(lcRpStyPrn,'W','P')
          laWip[lnSize] = laWip[lnSize] + STYLE.Wip&lcCount 
        ENDIF      
      ENDFOR
   ENDSCAN
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GO lnRecNo 
  ENDIF
  SELECT (lnAlias)
  lnStkOrWip = 0
  FOR lnCount  = 1 TO 14
    lnStkOrWip = lnStkOrWip + laStock[lnCount] + laWip[lnCount] 
    laStock[lnCount] = TRANSFORM(laStock[lnCount],'99999')
    laWip[lnCount]   = TRANSFORM(laWip[lnCount],'99999')
  ENDFOR 
  lnStkOrWip  = IIF(lnStkOrWip = 0 , '' , TRANSFORM(lnStkOrWip,'999999'))
  *B607312,1 WAB (END)
ENDIF  && end If User select specific locations .

RETURN ''
*-- end of lfGetWork.

*!*************************************************************
*! Name      : lfWorkEnd
*! Developer : Adel Mohammed El Gazzar (ADEL)
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 07/27/99
*! Purpose   : Fill Sort and select arrays
*!*************************************************************
*! Example   : = lfArrDumy()
*!*************************************************************
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

IF llMultCurr
  lnSrtElms = lnSrtElms + 1
  DIMENSION laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1]
  laSortDesc[ALEN(laSortDesc,1),1] = "Currency"
  laSortVal[ALEN(laSortDesc,1),1]  = "U"
ENDIF
*-- end of lfArrDumy.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Adel Mohammed El Gazzar (ADEL)
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


*!**************************************************************************
*! Name      : lfAssignSc
*! Developer : Adel Mohammed El Gazzar (ADEL)
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
FUNCTION lfAssignSc
lcOldScale = Scale
RETURN ''
*-- end of lfAssignSc.

*!**************************************************************************
*! Name      : lfScalePgH
*! Developer : Adel Mohammed El Gazzar (ADEL)
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
FUNCTION lfInnGrpOp
llLstMulti = (ORDHDR.MULTI = "Y")
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
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
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
llOGFltCh = .T.
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfCurrPrnt
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/15/98
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
  lcCurrCode = [Ordhdr.cCurrCode]
ENDIF
RETURN lcCurrCode
*-- end of lfCurrPrnt.

*!*************************************************************
*! Name      : lfChCurSm
*! Developer : Adel Mohammed El Gazzar (ADEL)
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/15/98
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
*! Name      : lfvEdiOrd
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 07/28/99
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
IF 'EB' $ gcCmpModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF
llClearOrd = .T.
*-- end of lfvEdiOrd.

*!**************************************************************************
*! Name      : lfExtenSz
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/06/2000
*! Purpose   : to collect sizes till 14 size as this report must do.
*!**************************************************************************
*! PARAMETERS : lcTmpName : Temp file to be updated.
*!            : llGetOrd  : If the sort in ORDLINE is not on order then this
*!                          parameter will be used to locate the order in temp file.
*!**************************************************************************
*!C101889
FUNCTION lfExtenSz
PARAMETERS lcTmpName,llGetOrd

PRIVATE lnAlias
lnAlias = SELECT()
IF llGetOrd
  SELECT (lcTmpName)
  *B607041,1 WAB (Start) - locate for the ordline and the style 
  *LOCATE FOR ORDER = ORDLINE.ORDER
  LOCATE FOR ORDER = ORDLINE.ORDER AND SUBSTR(STYLE,1,lnMajorlen+1+lnColorlen)= SUBSTR(ordline.STYLE,1,lnMajorlen+1+lnColorlen)
  *B607041,1 WAB (End)
  SELECT ORDLINE
ENDIF

ll14Size = .F.
lnRealPr = IIF(lcRpCurr = "F" OR OrdHdr.cCurrCode=gcBaseCurr,OrdLine.Price,gfAmntDisp(OrdLine.Price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"ORDHDR"))
=SEEK('S'+SCALE,'SCALE','SCALE')
*--If its the same order and style/color and the scale is different, Update from 9 :14 size
*:B803902,1 MHM 01/01/2001 lnScaleLen var, define in SYREPUVR [Start] 
*IF &lcTmpName..Order = Order;
*  AND SUBSTR(&lcTmpName..STYLE,1,lnMajorlen+1+lnColorlen+1)= SUBSTR(STYLE,1,lnMajorlen+1+lnColorlen+1);
*  AND &lcTmpName..SCALE <> SCALE AND LEFT(&lcTmpName..SCALE,1) = LEFT(SCALE,1)

*B607041,1 WAB (Start) all the scales for the same style color will print in one line
*IF &lcTmpName..Order = Order;
*  AND SUBSTR(&lcTmpName..STYLE,1,lnMajorlen+1+lnColorlen+1)= SUBSTR(STYLE,1,lnMajorlen+1+lnColorlen+1);
*  AND &lcTmpName..SCALE <> SCALE AND LEFT(&lcTmpName..SCALE,lnScaleLen) = LEFT(SCALE,lnScaleLen)
IF &lcTmpName..Order = Order;
  AND SUBSTR(&lcTmpName..STYLE,1,lnMajorlen+1+lnColorlen)= SUBSTR(STYLE,1,lnMajorlen+1+lnColorlen);
  AND &lcTmpName..SCALE <> SCALE AND LEFT(&lcTmpName..SCALE,lnScaleLen) = LEFT(SCALE,lnScaleLen)
*B607041,1 WAB (End)
*:B803902,1 MHM 01/01/2001 lnScaleLen var, define in SYREPUVR [End] 

  *--Update this line with its sizes added to the exsited sizes.
  lnCalcTot   = 0
  lnOrdCnt    = 0
  lnSzesQty   = 0
  FOR lnCnt   = 1 TO SCALE.CNT

    *B606937,1 WAB (Start)
    *lcSizeStr = ALLTRIM(STR(&lcTmpName..Cnt+1,2))
    *B606937,1 WAB (END)

    lnOrdCnt  = lnOrdCnt + 1
    lcOrdStr  = STR(lnOrdCnt,1)
    lnCalcTot = lnCalcTot + ORDLINE.QTY&lcOrdStr
    lnSzesQty = lnSzesQty + ORDLINE.QTY&lcOrdStr

    *B606937,1 WAB (Start) - if there is sizes more than 14 the 14'th one il be others 
    lnSize     = ASCAN(laSclDesc,SCALE.SZ&lcOrdStr)
    lnSize     = IIF(lnSize = 0 ,14,lnSize)
    lcSizeStr = ALLTRIM(STR(lnSize))
    *B606937,1 WAB (End)

    *:B803902,1 MHM 01/01/2001 add size to updated with Qyuantity [Start] 
    *REPLACE &lcTmpName..QTY&lcSizeStr  WITH ORDLINE.QTY&lcOrdStr,;
    *        &lcTmpName..Cnt WITH &lcTmpName..Cnt+1
    lcSZStrng = ALLTRIM(STR(lnCnt,2))

    *B606937,1 WAB (Start) - Acumulate the qty becasue in case of more than 14 the last one will be others
    *REPLACE &lcTmpName..QTY&lcSizeStr  WITH ORDLINE.QTY&lcOrdStr,;
            &lcTmpName..Cnt WITH &lcTmpName..Cnt+1,;
            &lcTmpName..Size&lcSizeStr  WITH SCALE.SZ&lcSZStrng
    REPLACE &lcTmpName..QTY&lcSizeStr  WITH &lcTmpName..QTY&lcSizeStr +ORDLINE.QTY&lcOrdStr,;
            &lcTmpName..Cnt WITH &lcTmpName..Cnt+1,;
            &lcTmpName..Size&lcSizeStr  WITH SCALE.SZ&lcSZStrng
    *B606937,1 WAB (End)

    *:B803902,1 MHM 01/01/2001 add size to updated with Qyuantity [End] 

    REPLACE &lcTmpName..lMulPrice WITH IIF(OrdLine.Price<>&lcTmpName..Price,.T.,&lcTmpName..lMulPrice)

    *B606937,1 WAB (Start) -- no need for the follwing lines 
    *IF &lcTmpName..Cnt = 14
    *  ll14Size = .T.
    *  m.cTempKey = STR(lnCalcTot*ORDLINE.Price,16,2)  && Total amount.
    *  REPLACE &lcTmpName..TOTQTY WITH &lcTmpName..TOTQTY+lnCalcTot,;
    *          &lcTmpName..cTempKey WITH &lcTmpName..cTempKey+m.cTempKey
    *  STORE 0 TO lnCalcTot,lnCalcTot
    *  STORE 0 TO m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,m.QTY7,m.QTY8,m.TotQty
    *  INSERT INTO (lcTmpName) FROM MEMVAR
    *  *-- Take only first 14.
    *  *:B803902,1 MHM 01/01/2001 [Start] 
    *  *EXIT
    *  *:B803902,1 MHM 01/01/2001 [End] 
    *ENDIF         
    *B606937,1 WAB (END)

  ENDFOR
  
  IF ll14Size
    ll14Size = .F.
    SKIP-1 IN (lcTmpName) 
  ENDIF
  REPLACE &lcTmpName..TOTQTY    WITH &lcTmpName..TOTQTY+lnCalcTot,;
          &lcTmpName..nAmountt  WITH &lcTmpName..nAmountt+(lnSzesQty*lnRealPr)
ELSE
  STORE 0 TO m.qty9,m.qty10,m.qty11,m.qty12,m.qty13,m.qty14
  =lfReScale()
  INSERT INTO (lcTmpName) FROM MEMVAR
  REPLACE &lcTmpName..Cnt      WITH SCALE.CNT,;
          &lcTmpName..nAmountt WITH &lcTmpName..TotQty*lnRealPr
ENDIF
=SEEK('S'+SCALE,'SCALE','SCALE')
*:B803902,1 MHM 01/01/2001 we remove temp scale file [Start] 
*SET ORDER TO TAG (lcScalTmp) ASCE IN (lcScalTmp)
*IF !SEEK(LEFT(SCALE,2),lcScalTmp)
*  INSERT INTO (lcScalTmp) (SCALE,SIZE1,SIZE2,SIZE3,SIZE4,SIZE5,SIZE6,SIZE7,SIZE8,Cnt);
*               VALUES     (SCALE.SCALE,SCALE.SZ1,SCALE.SZ2,SCALE.SZ3,SCALE.SZ4,;
*                           SCALE.SZ5,SCALE.SZ6,SCALE.SZ7,SCALE.SZ8,8)
*ELSE
  *--Don't fill sizes 9 : 14 if it's the same scale dimension 
  *--Get the recird being in work not the first one for this scale.
*  SET ORDER TO TAG (lcScalTmp) DESC IN (lcScalTmp)
*  =SEEK(LEFT(SCALE,1),lcScalTmp)
*  IF SCALE <> &lcScalTmp..Scale
*    IF &lcScalTmp..Cnt < 14
*      FOR lnCnt   = 1 TO SCALE.CNT
*        lcSizeStr = ALLTRIM(STR(&lcScalTmp..Cnt+1,2))
*        lcStr     = STR(lnCnt,1)
*        REPLACE &lcScalTmp..SIZE&lcSizeStr  WITH SCALE.SZ&lcStr,;
*                &lcScalTmp..Cnt WITH &lcScalTmp..Cnt + 1
*        IF &lcScalTmp..Cnt = 14 AND lnCnt < SCALE.CNT 
*           INSERT INTO (lcScalTmp) (SCALE,Cnt) VALUES (scale.SCALE,0)
*           *--mhm
*           *EXIT
*           *--mhm
*        ENDIF
*     ENDFOR
*    ENDIF
*  ENDIF          
*ENDIF                        
*:B803902,1 MHM 01/01/2001 we remove temp scale file [End] 

SELECT (lnAlias)
*!**************************************************************************
*! Name      : lfExSzis
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/01/2001
*! Purpose   : to get sizes From Scale file and put it in Tem. File
*!**************************************************************************
*! Example   : =lfExSzis()
*!**************************************************************************
*!B803902
FUNCTION lfExSzis

m.Size1= Scale.Sz1
m.Size2= Scale.Sz2
m.Size3= Scale.Sz3
m.Size4= Scale.Sz4
m.Size5= Scale.Sz5
m.Size6= Scale.Sz6
m.Size7= Scale.Sz7
m.Size8= Scale.Sz8
*--End lfExSzis

*!**************************************************************************
*! Name      : lfGetSCale
*! Developer : Walid A. Wahab (WAB)
*! Date      : 02/17/2003
*! Purpose   : get the Sizes description to the laSclDesc
*!**************************************************************************
*! Example   : =lfGetSCale()
*!**************************************************************************
*B606937,1 
FUNCTION lfGetSCale
PRIVATE lnAlias ,lnScale

lnAlias = SELECT()
SELECT SCALE
lnScale= 1
*--get frst the scale S1Y to S3y

=Seek('S'+'S')
SCAN REST WHILE TYPE+SCALE = 'S'+'S' FOR (SCALE = 'S1Y' OR SCALE = 'S2Y' OR SCALE = 'S3Y')
  FOR lnCount = 1 TO Cnt
    lcStr = STR(lnCount,1)
    IF ASCAN(laSclDesc,ALLTRIM(Sz&lcStr)) = 0 
      laSclDesc[MIN(lnScale,14)] = IIF(lnScale>14,'Others',Sz&lcStr)
      lnScale = lnScale+1
    ENDIF
  ENDFOR
  IF lnScale > 15 
    EXIT
  ENDIF
ENDSCAN
SCAN FOR  TYPE = 'S'
  FOR lnCount = 1 TO Cnt
    lcStr = STR(lnCount,1)
    IF ASCAN(laSclDesc,ALLTRIM(Sz&lcStr)) = 0 
      laSclDesc[MIN(lnScale,14)] = IIF(lnScale>14,'Others',Sz&lcStr)
      lnScale = lnScale+1
    ENDIF
  ENDFOR
  IF lnScale > 15 
    EXIT
  ENDIF
ENDSCAN

SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfReScale
*! Developer : Walid A. Wahab (WAB)
*! Date      : 02/17/2003
*! Purpose   : re-positon the Qty depend on the laSclDesc
*!**************************************************************************
*! Example   : =lfReScale()
*!**************************************************************************
*B606937,1 
*!**************************************************************************
FUNCTION lfReScale

*B607041,1 WAB (Start)
*DECLARE laQty[8]
*FOR lnCount = 1 TO 8 
*  lcStr= STR(lnCount,1)
*  laQty[lnCount] = m.Qty&lcStr
*  m.Qty&lcStr    = 0
*ENDFOR
*FOR lnCount = 1 TO 8 
*  lcStr= STR(lnCount,1)
*  lnSize = ASCAN(laSclDesc,m.SiZe&lcStr)
*  lnSize = IIF(lnSize = 0 ,14,lnSize)
*  lcSize = ALLTRIM(STR(lnSize))
*  m.Qty&lcSize = laQty[lnCount]
*ENDFOR
*
DECLARE laQty[8,2]
FOR lnCount = 1 TO 8 
  lcStr= STR(lnCount,1)
  laQty[lnCount,1] = m.SiZe&lcStr
  laQty[lnCount,2] = m.Qty&lcStr
  m.SiZe&lcStr   = ''
  m.Qty&lcStr    = 0
ENDFOR

FOR lnCount = 1 TO 8 
  lcStr= STR(lnCount,1)
  lnSize = ASCAN(laSclDesc,laQty[lnCount,1])
  lnSize = IIF(lnSize = 0 ,14,lnSize)
  lcSize = ALLTRIM(STR(lnSize))
  *- Acumulate only for the Other Qty
  lnOldQty =  IIF(lnSize = 14,m.Qty14,0)
  m.SiZe&lcSize = laQty[lnCount,1]
  m.Qty&lcSize  = laQty[lnCount,2] + lnOldQty
  
ENDFOR


*:**************************************************************************
*:* Name        : lfOpenFiles
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/10/2007
*:* Purpose     : Open needed files useing statndard sql functions
*:***************************************************************************
FUNCTION lfOpenFiles

=gfOpenTable('STYDYE','STYDYE','SH')
=gfOpenTable('WAREHOUS','WAREHOUS','SH')
=gfOpenTable('SALESREP','SALESREP','SH')
=gfOpenTable('FABRIC','FABRIC','SH')
=gfOpenTable('SYCINT','CCONTCODE','SH')
=gfOpenTable('OBJLINK','OBJLNKTY','SH')
=gfOpenTable('OBJECTS','OBJECTID','SH')
=gfOpenTable('CODES','CODES','SH')
=gfOpenTable('NOTEPAD','NOTEPAD','SH')
=gfOpenTable('CUSTOMER','CUSTOMER','SH')
=gfOpenTable('SCALE','SCALE','SH')
=gfOpenTable('STYLE','STYLE','SH')
=gfOpenTable('ORDLINE','','SH')
=gfOpenTable('ORDHDR','ORDHDR','SH')

*-- end of lfOpenFiles.

*:**************************************************************************
*:* Name        : lfSetOrder
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/12/2007
*:* Purpose     : Set the order to a file useing the global function gfSetOrder
*:***************************************************************************
FUNCTION lfSetOrder
PARAMETERS lcAlias,lcOrder
PRIVATE lnSlct
lnSlct = SELECT(0)

SELECT (lcAlias)
=gfSetOrder(lcOrder)

SELECT (lnSlct)
*-- end of lfSetOrder.