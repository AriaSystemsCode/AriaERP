*!******************************************************************************************************
*! Program file  : SORDRED.PRG
*! Program desc. : Custom Order Detail Report for RED ENGINE JEANS (RED07
*! For Report    : (SORDREDD.FRX,SORDREDS.FRX)
*! System        : Aria Advantage Series.
*! Module        : Sales Order (SO)
*! Developer     : NADER NABIL ABD EL-MONEAM (NNA)
*! Date          : 04/18/2005
*! Due to        : CP#126488
*!******************************************************************************************************
*! Calls : Functions: gfItemMask,gfPhoneTem,gfTempName,gfDispRe,gfGetMemVar,gfOptMsg,gfBrows,lfGetNotes
*!                  : lfvOptMsg,lfwOldVal,lfClearRep,lfwRepWhen,lfVarPos,lfvDisMesg,lfScanData,lfvSelcBy
*!                  : lfSelcObjs,lfvSortBy,lfPreObjs,lfObjState,lfvSizes,lfvKind,lfvSumm,lfsrvSty,lfStySum
*!                  : lfSRVFab,lfFabSum,lfsrAcc,lfsrLoc,lfsrRep,lfSScale,lfSROrder,lfCollTime,lfSumStyle
*!                  : lfSumMulti,lfLastRec,lfvCoorGrp,lfClearRep,lfEvalSegs,lfNMajType,lfMakeExpr,lfGetRepVr
*!                  : lfCreatCur,lfPipeExpr,lfvOStatus,lfGetWork,lfWorkEnd,lfArrDumy,lfItmPos,lfAssignSc
*!                  : lfScalePgH,lfInnGrpIn,lfFillArry,lfvEdiOrd,lfCurrDesc,lfChCurSm,lfCurrPrnt,lfvCurDisp
*!                  : lfOutGrpOp,lfInnGrpOp,lfOutGrpIn,lfOpenFilS,lfExprt2XL,lfvExprt2XL,lfGetCnclQ,lfGetShpQt
*!                  : lfFilMmVar,lfvStates,lfZtitle,lfStitle
*!         Procedure: lpChkStat
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Notes   : 1- All IF llFrTime Blocks executed one time in the option grid seasson.
*!         :    and if user selection opens any temp. files that may be used later
*!         :    in another selection I take this file open to use untill user choice
*!         :    is to press < Close > button, to save more time. 
*!         : 2- Any variable start by (llCh) means that some thing in 
*!         :    selected critria was changed, you must collect data again.
*!******************************************************************************************************
*! Example : DO SordRed
*!******************************************************************************************************
*! Modifications :
*! B129773,1 NNA 10/24/2005 fix bug that when you select Any state or style that contains Chr. (M), you'll get
*! B129773,1 NNA            "There is no records to display"
*! B130313,1 NNA 11/14/2005 Fix bug that When you choose a style then choose INVERT. NO data appears. 
*! B130313,1 NNA            and you'll get a massage 'No record to display'
*! B130352,1 EIH 12/25/2005 Fix bug that display wrong sales representative when we select Primary Sales 
*! B130352,1 EIH 12/25/2005 Representative from Sort# 1 .
*!******************************************************************************************************
*--Get the Style/Color/Scale Length and starting Position
=lfChkStrct()
lcRpexp  = IIF('INLIST(STYLE.STYLE' $ UPPER(lcRpexp),STRTRAN(lcRpexp,'INLIST(STYLE.STYLE','INLIST(STYLE.CSTYMAJOR'),lcRpexp)
lcRpexp  = IIF(lnFabPos>0 ,STRTRAN(lcRpexp,'INLIST(FABRIC.FABRIC+FABRIC.COLOR','INLIST(STYLE.FABRIC'),lcRpexp)
lcTime     = TIME()                     && Variable to hold the Time
lcStTime   = lcTime                     && Time in which we start collect data.
llPrntBoth = llRpOrdNot AND llRpOrdLnt  && True if you print both types of notes.
lnLastRec  = 2                          && Record No. Of last record in order group.
lcTitle    = ''                         && Title of Note. 
lcNotes    = ''                         && Notes.
lnLineNo   = 1                          && Order line No.
llNoIndex  = .F.                        && I don't make index for file.
*!* Define variables used in Print dos format and multi currency format
lcPhonPict = gfPhoneTem()
*-- Print totals if and only if [Not multi currency or user want any equavelent method or 
*-- select only one currency to print]
llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
             ((!EMPTY(laOGFxFlt[lnCurrPos,6]) AND ATC("|",laOGFxFlt[lnCurrPos,6])=0) OR ;
              (!EMPTY(laOGFxFlt[lnOrdPos ,6]) AND USED(laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(laOGFxFlt[lnOrdPos,6])=1))
llCurInGrp = !llPrintTot
STORE '' TO lcLineCurr , lcForExp , lcScanExp
STORE .T. TO llInnTotal,llOutTotal
llGrdTotal = llPrintTot
=ACOPY(laOGFxFlt,laStateFlt,((lnStatePos-1)*7)+1,7)
DIME laStateFlt[1,7]
lcRpExpSta = gfGenFlt('laStateFlt',.T.,.T.)
=ACOPY(laOGFxFlt,laZipFlt,((lnZipPos-1)*7)+1,7)
DIME laZipFlt[1,7]
lcRpExpZip = gfGenFlt('laZipFlt',.T.,.T.)

=ACOPY(laOGFxFlt,laScalFlt,((lnScalePos-1)*7)+1,7)
DIME laScalFlt[1,7]

=ACOPY(laBrFldFlt,laOldBr)
laBrFldFlt[8,5] = "laScalFlt:1"
lcRpExpSca = gfGenFlt('laScalFlt',.T.,.T.)
=ACOPY(laOldBr,laBrFldFlt)

=ACOPY(laOGFxFlt,laShpDaFlt,((lnShpDaPos-1)*7)+1,7)
DIME laShpDaFlt[1,7]
ldRpExpShp = gfGenFlt('laShpDaFlt',.T.,.T.)

*--If user select Some States.
IF !EMPTY(lcRpExpSta)
  lcForExp = lcForExp + IIF(EMPTY(lcForExp) , '' , ' .AND. ') + 'EVAL(lcRpExpSta)'
ENDIF
*--If user Select a Zip Code
IF !EMPTY(lcRpExpZip)
  lcForExp = lcForExp + IIF(EMPTY(lcForExp) , '' , ' .AND. ') + 'EVAL(lcRpExpZip)'
ENDIF
*--If user Select Some Scale
IF !EMPTY(lcRpExpSca)
  lcForExp = lcForExp + IIF(EMPTY(lcForExp) , '' , ' .AND. ') + 'EVAL(lcRpExpSca)'
ENDIF

lcSeekVal  = ''                                   && Get Last record seek value.
llTextMode = (UPPER(ALLTRIM(lcRepMode))=="TEXT")  && Print Text Format

*!* Add sort by store if first sort is by order 
lcStorCond = ''

*!* Adding a variable to detect the changes of SCALE in FRX to print it when it changes
lcOldScale = SPACE(3)
*--Declare a variable for the first scale.
llFirstSc = .F.
DIMENSION laStock[9],laWip[9]
STORE '' TO laStock,laWip               && Work process arrays
STORE '' TO lcGrpExp,lcSeaExp,lcDivExp,lcStatusEx,lcCatExp
STORE 0 TO lnFileStru
*-- Show messages in status bar when collecting data. 
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON
lcRpExp = STRTRAN(lcRpExp, 'CUSTOMER.TYPE+CUSTOMER.ACCOUNT+CUSTOMER.STORE', 'CUSTOMER.ACCOUNT')

*B129773,1 NNA 10/24/2005 (Start) Seek for the Account's index key in the Option Grid's Temp file that hold the list of
*B129773,1 NNA            Account that I selected to remve chr. "M" instead of removing it from lcRpExp that because if 
*B129773,1 NNA            we selected any State or Style that contain this Chr. , it will be removed.
*lcRpExp = STRTRAN(lcRpExp, '"M', '"')
lnOldAlias = SELECT(0)
IF !EMPTY(laOgFxFlt[1,6]) AND USED(laOgFxFlt[1,6])
  lcTempFile = laOgFxFlt[1,6]
  SELECT(lcTempFile) 
  IF RECCOUNT()>0
    SCAN
      lcRpExp = STRTRAN(lcRpExp, 'M'+ACCOUNT, ALLTRIM(ACCOUNT))
    ENDSCAN
  ENDIF
ENDIF
SELECT(lnOldAlias)

*B130313,1 NNA 11/14/2005 (Begin) Change Style.style to STYLE.CSTYMAJOR . that because if you select all styles or more than
*B130313,1 NNA            25 stlye,the lcrpexp will has (between(style.style,'xxxxx','xxxxx') and Seek(style.style,Temp file)
*B130313,1 NNA            this Temp File is containing a field that hold the Style.CstyMajor where the lcRpExp hold Style.Style
lcRpExp = STRTRAN(lcRpExp, 'SEEK(STYLE.STYLE', 'SEEK(STYLE.CSTYMAJOR')
*B130313,1 NNA (End)

*B129773,1 (End)

SELECT ORDLINE
SET RELATION TO 
SET RELATION TO Ordline.order+STR( Ordline.lineno,6) INTO Consinvl ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE   && To scale file.
SET RELATION TO cWareCode INTO WAREHOUS ADDITIVE  && To warehouse file.
lcCustRel = IIF(llRpSummMt,['M' + Account],;
            [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.
IF !EMPTY(ldRpExpShp) OR llRpShipQt
  SET RELATION TO Ordline.order+STR(LINENO,6) INTO Invline ADDITIVE
  set skip to invline
ENDIF

SELECT ORDHDR
SET RELATION TO REP1 INTO SALESREP ADDITIVE

SELECT SCALE
SET RELATION TO SUBSTR(Scale.scale,1,2) INTO Scalehd ADDITIVE
*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime
  lcStyTitle = IIF ('GFITEM' $ ALLTRIM(UPPER(lcStyTitle)),;
                    EVALUATE(lcStyTitle),lcStyTitle)  && style title.

  lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.

  *-- Create temporary file that holding order line data.
  lcWorkFile = gfTempName()
  lcTempLine = gfTempName()

  DIMENSION laTempStru[1,4]
  laTempStru = ''
  SELECT ORDLINE
  = AFIELDS(laTempStru)
  lnFileStru = ALEN(laTempStru,1)
  DIMENSION laTempStru[lnFileStru +10, 4]

  *-- cTempKey :  field used in most sort by case as the master key ,
  *--          :  and in case of summarize multi store as the total amount.
  laTempStru[lnFileStru +  1,1] = 'cTempKey'
  laTempStru[lnFileStru +  1,2] = 'C'
  laTempStru[lnFileStru +  1,3] = 16
  laTempStru[lnFileStru +  1,4] = 0

  *-- cCurrCode :  used if multi currency only to sort by it.
  laTempStru[lnFileStru +  2,1] = 'cCurrCode'
  laTempStru[lnFileStru +  2,2] = 'C'
  laTempStru[lnFileStru +  2,3] = 3
  laTempStru[lnFileStru +  2,4] = 0
  *!* Name    : Customer Name
  laTempStru[lnFileStru +  3,1] = 'Name'
  laTempStru[lnFileStru +  3,2] = 'C'
  laTempStru[lnFileStru +  3,3] = 30
  laTempStru[lnFileStru +  3,4] = 0
  *!* Buyer   : Customer's Buyer
  laTempStru[lnFileStru +  4,1] = 'Buyer'
  laTempStru[lnFileStru +  4,2] = 'C'
  laTempStru[lnFileStru +  4,3] = 30
  laTempStru[lnFileStru +  4,4] = 0
  *!* Phone   : Customer Phone1
  laTempStru[lnFileStru +  5,1] = 'Phone'
  laTempStru[lnFileStru +  5,2] = 'C'
  laTempStru[lnFileStru +  5,3] = 16
  laTempStru[lnFileStru +  5,4] = 0
  *!* State   : Customer's Caddress4
  laTempStru[lnFileStru +  6,1] = 'State'
  laTempStru[lnFileStru +  6,2] = 'C'
  laTempStru[lnFileStru +  6,3] = 30
  laTempStru[lnFileStru +  6,4] = 0

  *!* QtType  : Qty. Type (Ship - Cancel - book - open)
  laTempStru[lnFileStru +  7,1] = 'QTTYPE'
  laTempStru[lnFileStru +  7,2] = 'C'
  laTempStru[lnFileStru +  7,3] = 10
  laTempStru[lnFileStru +  7,4] = 0

  *!* ordhdr.Status + ordhdr.priority
  laTempStru[lnFileStru +  8,1] = 'SP'
  laTempStru[lnFileStru +  8,2] = 'C'
  laTempStru[lnFileStru +  8,3] = 2
  laTempStru[lnFileStru +  8,4] = 0

  *!* To print or not the QtType
  laTempStru[lnFileStru +  9,1] = 'llPrint'
  laTempStru[lnFileStru +  9,2] = 'L'
  laTempStru[lnFileStru +  9,3] = 1
  laTempStru[lnFileStru +  9,4] = 0

  *!* To use it in the relation
  laTempStru[lnFileStru + 10,1] = 'cRelation'
  laTempStru[lnFileStru + 10,2] = 'C'
  laTempStru[lnFileStru + 10,3] = 1
  laTempStru[lnFileStru + 10,4] = 0


  *-- Create temporary file that holding order line data.   
  llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.
ENDIF  && end if it's first time you run option Grid.

*-- Create temporary cursors from structure array. 
IF EMPTY(lcMastFile) OR !USED(lcMastFile)

  *-- Setting for report
  lcSetHour = SET('HOURS')
  SET HOURS TO 24

  *-- Setting for report   

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
  *-- create temp. file that used if you have both types of notes. 
  *-- Create work file.
  = lfCreatCur(lcWorkFile)  && Create work cursor.
  = lfCreatCur(lcTempLine)  && Create line cursor.

  *!*Create temp file to hold the Shipped Qty for the order
  CREATE TABLE (gcWorkDir+lcTmpShip)(CordType C(1),Order C(6),Store C(8),LINENO N(6),Style C(19),QTY1 N(6),QTY2 N(6),QTY3 N(6),;
               QTY4 N(6),QTY5 N(6),QTY6 N(6),QTY7 N(6),QTY8 N(6),TOTQTY N(8),cRelation C(1))
  SELECT(lcTmpShip)
  INDEX ON (Style + CordType + Order + cRelation) TAG SUMSTYLE
  INDEX ON (CordType+Order+Store+Style+STR(LineNo,6)+cRelation) TAG (lcTmpShip) 
ENDIF

= lfGetRepVr()      && Get Report variables such as groups and index.
*-- If user change report critria, Collect report data. 
*!* Use variable llOGFltCh that detect OG filter changes. 
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
  
  *!* if user select Style group, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnGrpPos,6])
    lcGrpExp  = "&laOGFxFlt[lnGrpPos,1]." + ' $ laOGFxFlt[lnGrpPos,6]'
  ENDIF  && end if user select Style group, evaluate its expression.

  *!* if user select Season, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnSeaPos,6])
    lcSeaExp  = "&laOGFxFlt[lnSeaPos,1]." + ' $ laOGFxFlt[lnSeaPos,6]'
  ENDIF  && end if user select Season, evaluate its expression.
  *!* if user select Division, evaluate its expression.
  IF !EMPTY(laOGFxFlt[lnDivPos,6])
    lcDivExp  = "&laOGFxFlt[lnDivPos,1]." + ' $ laOGFxFlt[lnDivPos,6]'
  ENDIF  && if user select Division, evaluate its expression.
  
  *!* Add Order Category to filter.
  IF !EMPTY(laOGFxFlt[lnCatPos,6])
    lcCatExp  = "&laOGFxFlt[lnCatPos,1]." + ' $ laOGFxFlt[lnCatPos,6]'
  ENDIF
  
  *-- Evaluate Color/Free Expression. 
  *-- Note that: We use either Only This XXX color object or direct XXX 
  *--            Free object, and you must know that both types of 
  *--            expressions can't be enable at the same time.

  
  *-- if Style  Non major does not have Color segment but have free segment(S).
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
  
  *-- Evaluate Color/Free Expression.   
  lcLastExpr = lcRpExp   && To later know that user change critria.

  *!* Give the user the ability to filter by 'Stye season' or 'Order header season'
  IF !EMPTY(lcSeaExp)
    IF lcRpSeaFlt = 'S' 
      lcSeaExp  = STRTRAN(lcSeaExp,'ORDHDR.SEASON','STYLE.SEASON')
    ELSE
      lcSeaExp  = STRTRAN(lcSeaExp,'STYLE.SEASON','ORDHDR.SEASON')
    ENDIF
  ENDIF  
  = lfScanData()  && Scan around master file.
  
  *-- Unrise all Critria variables.
  STORE .F. TO llChSelect,llChStatus,llChCoord,llChSumm,;
               llChAcc,llChStyle,llChFabric,llChRep,llChOrder,llChLoc

ELSE  &&  user does not change report critria.
  *-- if user Sort By .
  IF lcLastTag != lcIndexTg
    SELECT (lcMastFile)
    USE IN (lcMastFile)
    =gfOpenFile(gcWorkDir+lcMastFile, gcWorkDir+lcMastFile,'EX')
    INDEX ON &lcIndexTg TAG (lcMastFile)
    =gfOpenFile(gcWorkDir+lcMastFile, gcWorkDir+lcMastFile,'SH')
    lcLastTag = lcIndexTg  && To later know that user change Sort case.
  ENDIF		&& end if user Sort By . 
ENDIF       && end If user change report critria, Collect report data. 

SELECT OrdLine
SET RELATION OFF INTO ORDHDR 
SET RELATION OFF INTO STYLE  
SET RELATION OFF INTO CUSTOMER  
SET RELATION OFF INTO SCALE
SET RELATION OFF INTO INVLINE
SET RELATION OFF INTO CONSINVL
SET SKIP TO
SELECT STYLE
SET RELATION OFF INTO SCALE

SELECT ORDHDR
SET RELATION OFF INTO SALESREP

SELECT SCALE
SET RELATION OFF INTO SCALEHD 

SELECT INVLINE
SET RELATION OFF INTO INVHDR
*-- Temporary File relations, in the way that help us in report form 
SELECT (lcMastFile)
SET RELATION TO
SET RELATION TO cOrdType + Order INTO OrdHdr      && To order header file.
SET RELATION TO style INTO Style ADDITIVE         && To style file.
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE   && To scale file.
SET RELATION TO cWareCode INTO WAREHOUS ADDITIVE  && To warehouse file.

*B130352,1 EIH 12/25/2005 Set relation to SALESREP file.[Begin] 
SET RELATION TO RIGHT(cTempKey,3) INTO SALESREP ADDITIVE  && To salesrep file.
*B130352,1 EIH 12/25/2005 [End] 

IF !llRpSummMt
  SET RELATION TO cOrdType + Order + Store + Style + STR(LineNo,6)+cRelation INTO (lcTmpShip) ADDITIVE
ELSE
  SET RELATION TO Style + cOrdType + Order + cRelation INTO (lcTmpShip) ADDITIVE
ENDIF
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
SET FILTER TO TOTQTY <> 0 .OR. OrdHdr.cOrdType = 'T'
GO BOTTOM
lnLastOne  = RECNO()
GO TOP    && Refresh Relation
lcOrderNo = ''
SCAN
  IF !(lcOrderNo==ORDER)
    lcOrderNo = ORDER
    IF UPPER(QTTYPE)='CANCEL'
      REPLACE llPrint WITH .T.
    ENDIF
  ENDIF
ENDSCAN
*!* if dos mode do the following.
IF llTextMode
  STORE ' ' TO lcInnGrpIn,lcOutGrpIn,lcInnGrpOp,lcOutGrpOp

  STORE '' TO lcPrnInnL
  STORE 0 TO lnInnQty1,lnInnQty2,lnInnQty3,lnInnQty4,;
             lnInnQty5,lnInnQty6,lnInnQty7,lnInnQty8,;
             lnInnTtQty,lnGrInnAmt
  STORE 0 TO lnInnShp1,lnInnShp2,lnInnShp3,lnInnShp4,;
             lnInnShp5,lnInnShp6,lnInnShp7,lnInnShp8,;
             lnInnTShp
  STORE 0 TO lnInnCNL1,lnInnCNL2,lnInnCNL3,lnInnCNL4,;
             lnInnCNL5,lnInnCNL6,lnInnCNL7,lnInnCNL8,;
             lnInnTCNL

  STORE 0 TO lnPrnInnQ1,lnPrnInnQ2,lnPrnInnQ3,lnPrnInnQ4,;
             lnPrnInnQ5,lnPrnInnQ6,lnPrnInnQ7,lnPrnInnQ8,;
             lnPrnInnTQ,lnPrnInnAm
  STORE 0 TO lnPrnIShp1,lnPrnIShp2,lnPrnIShp3,lnPrnIShp4,;
             lnPrnIShp5,lnPrnIShp6,lnPrnIShp7,lnPrnIShp8,;
             lnPrnITShp
  STORE 0 TO lnPrnICNL1,lnPrnICNL2,lnPrnICNL3,lnPrnICNL4,;
             lnPrnICNL5,lnPrnICNL6,lnPrnICNL7,lnPrnICNL8,;
             lnPrnITCNL
  
  *!* Add sort by store if first sort is by order.
  llLstMulti = (ORDHDR.MULTI = "Y")

  =lfInnGrpOp()

  STORE '' TO lcPrnOutL
  STORE 0 TO lnOutQty1,lnOutQty2,lnOutQty3,lnOutQty4,;
             lnOutQty5,lnOutQty6,lnOutQty7,lnOutQty8,;
             lnOutTtQty,lnGrOutAmt
  STORE 0 TO lnOutShp1,lnOutShp2,lnOutShp3,lnOutShp4,;
             lnOutShp5,lnOutShp6,lnOutShp7,lnOutShp8,;
             lnOutTShp
  STORE 0 TO lnOutCnl1,lnOutCnl2,lnOutCnl3,lnOutCnl4,;
             lnOutCnl5,lnOutCnl6,lnOutCnl7,lnOutCnl8,;
             lnOutTCnl

  STORE 0 TO lnPrnOutQ1,lnPrnOutQ2,lnPrnOutQ3,lnPrnOutQ4,;
             lnPrnOutQ5,lnPrnOutQ6,lnPrnOutQ7,lnPrnOutQ8,;
             lnPrnOutTQ,lnPrnOutAm
  STORE 0 TO lnPrnOShp1,lnPrnOShp2,lnPrnOShp3,lnPrnOShp4,;
             lnPrnOShp5,lnPrnOShp6,lnPrnOShp7,lnPrnOShp8,;
             lnPrnOTShp
  STORE 0 TO lnPrnOCnl1,lnPrnOCnl2,lnPrnOCnl3,lnPrnOCnl4,;
             lnPrnOCnl5,lnPrnOCnl6,lnPrnOCnl7,lnPrnOCnl8,;
             lnPrnOTCnl

  =lfOutGrpOp()
  lcRpSort2 = IIF(lcRpSrt2$"TY","T",lcRpSrt2)
ENDIF             

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)

*!* if dos mode do the following.
IF llTextMode
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMastFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
ELSE  && else window format
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMastFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' TIMEOUT 1
ENDIF  
IF llRpExport
  =lfExprt2XL()
ELSE
  DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRepExpr
ENDIF
SET RELATION TO
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
*-- end of Report Code.

*-- Functions section.
*!******************************************************************************************************
*! Name      : lfwRepWhen
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Option Grid When function
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!******************************************************************************************************
*! Passed Parameters : None
*!******************************************************************************************************
*! Return      : None
*!******************************************************************************************************
*! Example     : = lfwRepWhen()
*!******************************************************************************************************
FUNCTION lfwRepWhen

*-- if it's first time to run the report.
*-- using TYPE of variable instead of global llFirstTime, to control
*-- reset case which does not rise llFirsttime, but restore initial
*-- value for lnVarbEnd and advanced case which keep the variables same.
IF TYPE('lnVarbEnd') = 'C'
  lnVarbEnd = 0
  *!* check if the first element of the array holds character string,then check the rest otherwise declare the array 
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
  lnStatePos = lfItmPos('CUSTOMER.CADDRESS4')
  lnZipPos   = lfItmPos('CUSTOMER.CADDRESS5')
  lnScalePos = lfItmPos('SCALE.SCALE')
  lnShpDaPos = lfItmPos('INVHDR.SHIPDATE')

  *!* Convert all ceiling functions to use lfItmPos because
  lnFreSgPos = lnClrSgPos + 1

  IF llMultCurr
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
    lnCurrPos  = lfItmPos('ORDHDR.CCURRCODE')
  ENDIF
ELSE
  FOR lnElm = 1 TO ALEN(laOgObjType,1) 
   IF laOgObjType[lnElm,1] == "lcRpSortBy"
     _CUROBJ= OBJNUM(&laOgObjType[lnElm,2].)+1
   ENDIF
  ENDFOR
ENDIF  && END IF you first time enter when function.


IF lcRpSortBy = "O" AND lcRpKind = "D" AND ALEN(laSort2Des,1) = 2
  DIMENSION laSort2Des[4,1] , laSort2Val[4,1]
  laSort2Des[3,1] = "Store/Line#"
  laSort2Des[4,1] = "Store/" + lcStyMajor
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
STORE 0 TO lnLenth
lnLenth = LEN(gfItemMask('PM'))

DO lpChkStat
IF llRpExport
  lcRunStat = IIF(llRpExport,'DISABLE','ENABLE')
  SHOW GET pbRun &lcRunStat
ENDIF

*--End of lfwRepWhen.
*!******************************************************************************************************
*! Name      : lfVarPos
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : To get the position of the Variable in OG.
*!******************************************************************************************************
*! Called from : OG when function
*!******************************************************************************************************
*! Example   : = lfVarPos()
*!******************************************************************************************************
FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGObjType,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfVarPos.
*!******************************************************************************************************
*! Name      : lpChkStat
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : To check for Order status in Order to Enable/Disable the 2
*!             Option added to the Selection grid.
*!******************************************************************************************************
*! Called from : OG when function
*!******************************************************************************************************
*! Example   : DO lpChkStat
*!******************************************************************************************************
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

*!******************************************************************************************************
*! Name      : lfvDisMesg
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Diaplay message to inform user to ignore Complete,Cancel,Bid Orders
*!******************************************************************************************************
*! Called from : OG when function
*!******************************************************************************************************
*! Example   : = lfvDisMesg()
*!******************************************************************************************************
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

*!******************************************************************************************************
*! Name      : lfScanData
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Collect report data.
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfSumStyle,lfSumMulti
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfScanData()
*!******************************************************************************************************
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
*!******************************************************************************************************
FUNCTION lfScanData

*-- If you find any data (i.e: not first time you run), clear it. 
IF RECCOUNT(lcTempLine) > 0
  lcSafty = SET('SAFETY')
  SET SAFETY OFF 
  SELECT (lcTempLine)
  USE IN (lcTempLine)
  USE (gcWorkDir+(lcTempLine)) IN 0 EXCLUSIVE
  ZAP
  SET RELATION TO
  llNoIndex = .T.
  SET SAFETY &lcSafty  
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

IF RECCOUNT(lcTmpShip) > 0
  lcSafty = SET('SAFETY')
  SET SAFETY OFF 
  SELECT (lcTmpShip)
  USE IN (lcTmpShip)
  USE (gcWorkDir+(lcTmpShip)) IN 0 EXCLUSIVE ORDER TAG (lcTmpShip)
  ZAP
  *!* Rest any relation before data collection.
  SET RELATION TO
  SELECT (lcTmpShip)
  SET SAFETY &lcSafty  
ENDIF

*-- lcWorkTag : Variable which hold value of working file index.
*--           : Note that putting index this way help us in collecting data rush more.
lcWorkTag = lcIndexTg
SELECT (lcWorkFile)
IF RECCOUNT(lcWorkFile) > 0
  lcSafty = SET('SAFETY')
  SET SAFETY OFF 
  SELECT (lcWorkFile)
  USE IN (lcWorkFile)
  USE (gcWorkDir+lcWorkFile) ORDER 1 IN 0 EXCLUSIVE
  ZAP
  *!* Rest any relation before data collection.
  SET RELATION TO
  SELECT (lcWorkFile)
  SET SAFETY &lcSafty
ENDIF
INDEX ON &lcWorkTag TAG (lcWorkFile)

*-- Relation with master order file to help data collecting. 
SELECT INVLINE
SET RELATION TO Invline.invoice INTO Invhdr ADDITIVE
SELECT OrdLine
SET RELATION TO cOrdType + Order INTO OrdHdr
SET RELATION TO style INTO Style ADDITIVE
SET RELATION TO Ordline.order+STR( Ordline.lineno,6) INTO Consinvl ADDITIVE
SET RELATION TO Ordline.order+STR(LINENO,6) INTO Invline ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE   && To scale file.
SET RELATION TO cWareCode INTO WAREHOUS ADDITIVE  && To warehouse file.
lcCustRel = IIF(llRpSummMt,['M' + Account],;
            [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.
IF !EMPTY(ldRpExpShp) AND llRpShipQt
  set skip to invline
ENDIF
*-- Relation with master order file to help data collecting.   

*-- llWorkDeal : Flag to know that we start dealing with work file.
STORE .F. TO llWorkDeal 
lcTOrdFile = laOGFxFlt[lnOrdPos,6]
llWorkDeal = !EMPTY(lcTOrdFile) AND USED(lcTOrdFile) AND RECCOUNT(lcTOrdFile) > 0
lcOrdNo  = ''
lnLinNo  = 0
WAIT WINDOW NOWAIT 'Please wait...'
*-- If user select specific orders, collect data of this orders only. 
IF llWorkDeal
  SELECT (lcTOrdFile)
  *-- Scan order cursor.
  SCAN
    WAIT WINDOW NOWAIT 'Please wait...'
    SELECT ORDLINE
    SET ORDER TO TAG ORDLINE
    IF IIF(EMPTY(lcRpOrdTyp),SEEK('O'+&lcTOrdFile..ORDER) OR SEEK('T'+&lcTOrdFile..ORDER),SEEK(lcRpOrdTyp+&lcTOrdFile..ORDER)) AND;    
       EVALUATE(lcStatusEx)                                                                           AND ;
       (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt))                                                    AND ;
       IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp))                                                   AND ;
       IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])                  AND ;
       IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
       IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)         AND ;
       IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.)) 
  
       IF !EMPTY(lcForExp)
         lcScanExp = 'FOR ' + lcForExp + IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
       ELSE
         lcScanExp =IIF(!EMPTY(lcRpExp),' FOR ' + lcRpExp ,'')
       ENDIF  
       SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = cOrdType+&lcTOrdFile..Order &lcScanExp
           
        IF IIF(llRpCorrGp ,!EMPTY(Group),.T.)                                                         AND ;
           (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))                                   AND ;
           (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd))                                  AND ;
           IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))                                               AND ;
           IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))                                               AND ;
           IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))                                                AND ;
           IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))                                             AND ;
           IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.)                              AND ;           
           IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
          SCATTER MEMVAR MEMO
          WAIT WINDOW NOWAIT 'Please wait...'
          m.llPrint  =.T.
          m.cRelation= 'N'
          IF ORDHDR.STATUS = "C"
            *-- Get qty from book qty becauce qty =0
            FOR I = 1 To 8
              lcCount = ALLTRIM(STR(I))
              m.QTY&lcCount = BOOK&lcCount
            ENDFOR
            m.TOTQTY = TOTBOOK
            m.QtType = 'Book'
            m.SP     = ORDHDR.STATUS + ALLTRIM(ORDHDR.PRIORITY)
          ELSE
            m.QtType = IIF(ORDHDR.STATUS='X','Book','Open')
            m.SP     = ORDHDR.STATUS + ALLTRIM(ORDHDR.PRIORITY)
          ENDIF
          m.cTempKey  = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
          m.cCurrCode = ORDHDR.cCurrCode
          IF SEEK(IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store),'CUSTOMER')
            m.Name      = CUSTOMER.BTname
            m.Buyer     = CUSTOMER.Buyer
            m.Phone     = TRANSFORM(CUSTOMER.Phone1,lcPhonPict)
            m.State     = CUSTOMER.Caddress4
          ENDIF
          INSERT INTO (lcWorkFile) FROM MEMVAR
          IF ORDHDR.STATUS='X' AND llRpCnclQt
            m.QtType = 'Cancel'
            m.llPrint= .F.
            INSERT INTO (lcWorkFile) FROM MEMVAR
          ENDIF
          IF llRpShipQt
            =lfGetShpQt()
          ENDIF
          IF llRpCnclQt
            IF !(lcOrdNo + STR(lnLinNo,6) == ORDER + STR(LINENO,6))
              lcOrdNo  = ORDLINE.ORDER
              lnLinNo  = ORDLINE.LineNo
              =lfGetCnclQ(lcWorkFile)
            ENDIF
          ENDIF
        ENDIF  && end if <ordline filter>.
      ENDSCAN
    ENDIF      && end if find first order record in ordline.
  ENDSCAN      && end Scan order cursor.
  IF llRpCnclQt
    SELECT(lcTOrdFile)
    SCAN
      WAIT WINDOW NOWAIT 'Please wait...'
      SELECT STYLE
      SET RELATION TO
      SET RELATION TO 'S'+scale INTO Scale ADDITIVE
      SELECT ORDCANLN
      SET RELATION TO
      SET RELATION TO style INTO Style ADDITIVE
      lcCustRel = IIF(llRpSummMt,['M' + Account],;
                [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
      SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.

      IF IIF(EMPTY(lcRpOrdTyp),SEEK('O'+&lcTOrdFile..ORDER) OR SEEK('T'+&lcTOrdFile..ORDER),SEEK(lcRpOrdTyp+&lcTOrdFile..ORDER))
        IF !EMPTY(lcForExp)
          lcScanExp = 'FOR ' + lcForExp  + IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
        ELSE
          lcScanExp =IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
        ENDIF  
        SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = cOrdType+&lcTOrdFile..Order &lcScanExp
          WAIT WINDOW NOWAIT 'Please wait...'
          SCATTER MEMVAR MEMO
          IF SEEK(ORDCANLN.STYLE,'STYLE')
             IF IIF(EMPTY(lcRpOrdTyp),SEEK('O'+&lcTOrdFile..ORDER,'ORDHDR') OR SEEK('T'+&lcTOrdFile..ORDER,'ORDHDR'),SEEK(lcRpOrdTyp+&lcTOrdFile..ORDER,'ORDHDR'))
               m.cTempKey = PADR(STYLE.FABRIC,7)+PADR(STYLE.CSTYGROUP,6)+PADR(ORDHDR.REP1,3)
             ENDIF
          ENDIF
           IF 'CCURRCODE' $ UPPER(lcIndexTg)
            lcCcode   = ORDHDR.CCURRCODE
            lcWareCode= ORDLINE.CWareCode
            lcSkExp = lcIndexTg
            lcSkExp = STRTRAN(lcSkExp,'CCURRCODE','lcCcode')
            lcSkExp = STRTRAN(lcSkExp,'CWARECODE','lcWareCode')
            lcSkExp = STRTRAN(lcSkExp,'COMPLETE','ORDLINE.COMPLETE')
          ENDIF
          IF !SEEK(&lcSkExp,lcWorkFile)
            m.QtType  = 'Cancel'
            m.llPrint =.T.
            m.cRelation= ''
            m.SCALE   = IIF(SEEK(STYLE,'STYLE'),STYLE.SCALE,m.SCALE)
            INSERT INTO (lcWorkFile) FROM MEMVAR
           ENDIF
        ENDSCAN
      ENDIF   
      SELECT STYLE
      SET RELATION TO
      SELECT ORDCANLN
      SET RELATION TO
    ENDSCAN
  ENDIF
  lcMastFile = lcWorkFile
ENDIF  
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
    IF !EMPTY(lcForExp)
      lcScanExp = ' AND ' + lcForExp + IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
    ELSE
      lcScanExp =IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
    ENDIF  
    SCAN FOR style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = '' &lcScanExp
      WAIT WINDOW NOWAIT 'Please wait...'
      lcSeekExp = Style + DTOS(Complete) + cordtype + order
      *-- if you does not find line in temp line file,;
      *-- and order type is 'O', <ordhdr filter>, <ordline filter> ;
      *-- and <style group filter> and <Color Filter>
      IF !SEEK(lcSeekExp,lcTempLine) AND IIF(EMPTY(lcRpOrdTyp),CORDTYPE <> 'C',CORDTYPE= lcRpOrdTyp) AND ;
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
         IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
         IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
         IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
         IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT))) AND ;
         IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.))             
        SCATTER MEMVAR MEMO
        m.llPrint  =.T.
        m.cRelation= 'N'
        m.cCurrCode = ORDHDR.cCurrCode
        IF ORDHDR.STATUS = "C"
        *-- Get qty from book qty becauce qty =0
          FOR I = 1 To 8
            lcCount = ALLTRIM(STR(I))
            m.QTY&lcCount = BOOK&lcCount
          ENDFOR
          m.TOTQTY = TOTBOOK
          m.QtType = 'Book'
          m.SP     = ORDHDR.STATUS + ALLTRIM(ORDHDR.PRIORITY)
        ELSE
          m.QtType = 'Open'
          m.SP     = ORDHDR.STATUS + ALLTRIM(ORDHDR.PRIORITY)
        ENDIF
        = lfSumMulti(lcSeekExp)  && summarize data.
        IF SEEK(IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store),'CUSTOMER')
          m.Name      = CUSTOMER.BTname
          m.Buyer     = CUSTOMER.Buyer
          m.Phone     = TRANSFORM(CUSTOMER.Phone1,lcPhonPict)
          m.State     = CUSTOMER.Caddress4
        ENDIF
        INSERT INTO (lcTempLine) FROM MEMVAR
        *--Get the order's Shipped Qty.
        IF llRpShipQt
          =lfGetShpQt()
        ENDIF
        *--Get the order's Cancelled Qty.
        IF llRpCnclQt
            IF !(lcOrdNo + STR(lnLinNo,6) == ORDER + STR(LINENO,6))
              lcOrdNo  = ORDLINE.ORDER
              lnLinNo  = ORDLINE.LineNo
              =lfGetCnclQ(lcTempLine)
            ENDIF
          ENDIF
      ENDIF    && end if you does not find line in temp line file,
    ENDSCAN    && end scan file for full index expression (rushmore).
   ***************
    IF llRpCnclQt
      SELECT ORDLINE
      WAIT WINDOW NOWAIT 'Please wait...'
      SCAN FOR style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = '' &lcScanExp
        lcSeekExp = Style + DTOS(Complete) + cordtype + order
        IF !SEEK(lcSeekExp,lcTempLine) AND IIF(EMPTY(lcRpOrdTyp),CORDTYPE <> 'C',CORDTYPE= lcRpOrdTyp) AND ;
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
           IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
           IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
           IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
           IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT))) AND ;
           IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.))             
          
           SELECT STYLE
           SET RELATION TO
	       SET RELATION TO 'S'+scale INTO Scale ADDITIVE
           SELECT ORDCANLN
	       SET RELATION TO
           SET RELATION TO style INTO Style ADDITIVE
	       lcCustRel = IIF(llRpSummMt,['M' + Account],[IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
           SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.
           IF IIF(EMPTY(lcRpOrdTyp),SEEK('O'+OrdLine.ORDER) OR SEEK('T'+OrdLine.ORDER),SEEK(lcRpOrdTyp+OrdLine.ORDER))
	         IF !EMPTY(lcForExp)
	           lcScanExp = 'FOR ' + lcForExp  + IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
    	     ELSE
        	   lcScanExp =IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
	       ENDIF  
    	     SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = OrdHdr.cOrdType+OrdHdr.Order+STR(OrdLine.LineNo,6) &lcScanExp
	           SCATTER MEMVAR MEMO
               WAIT WINDOW NOWAIT 'Please wait...'
    	       IF SEEK(ORDCANLN.STYLE,'STYLE')
        	     IF IIF(EMPTY(lcRpOrdTyp),SEEK('O'+OrdLine.ORDER,'ORDHDR') OR SEEK('T'+OrdLine.ORDER,'ORDHDR'),SEEK(lcRpOrdTyp+OrdLine.ORDER,'ORDHDR'))
	               m.cTempKey = PADR(STYLE.FABRIC,7)+PADR(STYLE.CSTYGROUP,6)+PADR(ORDHDR.REP1,3)
    	         ENDIF
          	   ENDIF
               IF 'CCURRCODE' $ UPPER(lcIndexTg)
                 lcCcode   = ORDHDR.CCURRCODE
                 lcWareCode= ORDLINE.CWareCode
                 lcSkExp = lcIndexTg
                 lcSkExp = STRTRAN(lcSkExp,'CCURRCODE','lcCcode')
                 lcSkExp = STRTRAN(lcSkExp,'CWARECODE','lcWareCode')
                 lcSkExp = STRTRAN(lcSkExp,'COMPLETE','ORDLINE.COMPLETE')
               ENDIF
	           IF !SEEK(&lcSkExp,lcTempLine)
    	         m.QtType   = 'Cancel'
          	     m.llPrint  =.T.
                 m.cRelation= ''
                 m.SCALE   = IIF(SEEK(STYLE,'STYLE'),STYLE.SCALE,m.SCALE)
    	         INSERT INTO (lcTempLine) FROM MEMVAR
        	   ENDIF
 	         ENDSCAN
    	   ENDIF   
 	       SELECT STYLE
           SET RELATION TO
 	       SELECT ORDCANLN
    	   SET RELATION TO
		 ENDIF
       ENDSCAN
	 ENDIF   
     ***************
     USE IN SUMMULTI
  ELSE  && Normal collection case for all data in ordline file.
    *-- scan ordline file for full index expression (rushmore)
    IF !EMPTY(lcForExp)
      lcScanExp = ' AND ' + lcForExp + IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
    ELSE
      lcScanExp =IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
    ENDIF  
    IF ORDER('ORDLINE')=''
      SET ORDER TO 1 IN ORDLINE
    ENDIF
    LOCATE
    SCAN FOR CORDTYPE + ORDER + STR(LINENO,6) = lcRpOrdTyp &lcScanExp
      WAIT WINDOW NOWAIT 'Please wait...'
      IF EVALUATE(lcStatusEx) AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
         (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
         (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
         IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
         IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
         (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
         IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
         IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
         IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
         IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
         IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
         IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
         IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
         IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT))) AND ;
         IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.))             
         SCATTER MEMVAR MEMO
         m.llPrint   =.T.
         m.cRelation = 'N'
         m.cTempKey = PADR(STYLE.FABRIC,7)+PADR(STYLE.CSTYGROUP,6)+PADR(ORDHDR.REP1,3)
         m.cCurrCode = ORDHDR.cCurrCode
         IF ORDHDR.STATUS = "C"
         *-- Get qty from book qty becauce qty =0
           FOR I = 1 To 8
             lcCount = ALLTRIM(STR(I))
             m.QTY&lcCount = BOOK&lcCount
           ENDFOR
           m.TOTQTY = TOTBOOK
           m.QtType = 'Book'
           m.SP     = ORDHDR.STATUS + ALLTRIM(ORDHDR.PRIORITY)
         ELSE
           m.QtType = IIF(ORDHDR.STATUS='X','Book','Open')
           m.SP     = ORDHDR.STATUS + ALLTRIM(ORDHDR.PRIORITY)
         ENDIF
         IF SEEK(IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store),'CUSTOMER')
           m.Name      = CUSTOMER.BTname
           m.Buyer     = CUSTOMER.Buyer
           m.Phone     = TRANSFORM(CUSTOMER.Phone1,lcPhonPict)
           m.State     = CUSTOMER.Caddress4
         ENDIF
         INSERT INTO (lcTempLine) FROM MEMVAR
          IF ORDHDR.STATUS='X' AND llRpCnclQt
            m.QtType = 'Cancel'
            m.llPrint= .F.
         INSERT INTO (lcTempLine) FROM MEMVAR
          ENDIF
         IF llRpShipQt
           =lfGetShpQt()
         ENDIF
         IF llRpCnclQt
            IF !(lcOrdNo + STR(lnLinNo,6) == ORDER + STR(LINENO,6))
              lcOrdNo  = ORDLINE.ORDER
              lnLinNo  = ORDLINE.LineNo
              =lfGetCnclQ(lcTempLine)
            ENDIF
          ENDIF
       ENDIF    && end if <ordhdr filter>, <ordline filter> and <style group filter>        
     ENDSCAN    && end scan file for full index expression (rushmore).
   ***************
    IF llRpCnclQt
      SELECT ORDLINE
      WAIT WINDOW NOWAIT 'Please wait...'
      SCAN FOR CORDTYPE + ORDER + STR(LINENO,6) = lcRpOrdTyp &lcScanExp
        IF EVALUATE(lcStatusEx) AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
           (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
           (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
           IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
           IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
           (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
           IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
           IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
           IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
           IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
           IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
           IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
           IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
           IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT))) AND ;
           IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.))             
        
           SELECT STYLE
           SET RELATION TO
	       SET RELATION TO 'S'+scale INTO Scale ADDITIVE
           SELECT ORDCANLN
	       SET RELATION TO
           SET RELATION TO style INTO Style ADDITIVE
	       lcCustRel = IIF(llRpSummMt,['M' + Account],[IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
           SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.
           IF IIF(EMPTY(lcRpOrdTyp),SEEK('O'+OrdLine.ORDER) OR SEEK('T'+OrdLine.ORDER),SEEK(lcRpOrdTyp+OrdLine.ORDER))
	         IF !EMPTY(lcForExp)
	           lcScanExp = 'FOR ' + lcForExp  + IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
    	     ELSE
        	   lcScanExp =IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
	       ENDIF  
    	     SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = OrdHdr.cOrdType+OrdHdr.Order+STR(OrdLine.LineNo,6) &lcScanExp
	           SCATTER MEMVAR MEMO
    	       IF SEEK(ORDCANLN.STYLE,'STYLE')
        	     IF IIF(EMPTY(lcRpOrdTyp),SEEK('O'+OrdLine.ORDER,'ORDHDR') OR SEEK('T'+OrdLine.ORDER,'ORDHDR'),SEEK(lcRpOrdTyp+OrdLine.ORDER,'ORDHDR'))
	               m.cTempKey = PADR(STYLE.FABRIC,7)+PADR(STYLE.CSTYGROUP,6)+PADR(ORDHDR.REP1,3)
    	         ENDIF
          	   ENDIF
               IF 'CCURRCODE' $ UPPER(lcIndexTg)
                 lcCcode   = ORDHDR.CCURRCODE
                 lcSkExp = lcIndexTg
                 lcSkExp = STRTRAN(lcSkExp,'CCURRCODE','lcCcode')
               ELSE
                 lcSkExp = lcIndexTg
               ENDIF
               IF 'CWARECODE' $ UPPER(lcIndexTg)
                 lcWareCode= ORDLINE.CWareCode
                 lcSkExp = lcIndexTg
                 lcSkExp = STRTRAN(lcSkExp,'CWARECODE','lcWareCode')
               ELSE
                 lcSkExp = lcIndexTg
               ENDIF
               IF 'COMPLETE' $ UPPER(lcIndexTg)
                 lcSkExp = lcIndexTg
                 lcSkExp = STRTRAN(lcSkExp,'COMPLETE','ORDLINE.COMPLETE')
               ELSE
                 lcSkExp = lcIndexTg
               ENDIF
	           IF !SEEK(&lcSkExp,lcTempLine)
    	         m.QtType   = 'Cancel'
          	     m.llPrint  =.T.
                 m.cRelation= ''
                 m.SCALE   = IIF(SEEK(STYLE,'STYLE'),STYLE.SCALE,m.SCALE)
    	         INSERT INTO (lcTempLine) FROM MEMVAR
        	   ENDIF
 	         ENDSCAN
    	   ENDIF   
 	       SELECT STYLE
           SET RELATION TO
 	       SELECT ORDCANLN
    	   SET RELATION TO
		 ENDIF
       ENDSCAN
	 ENDIF   
   ***************
   ENDIF        && end if summarize multi store orders.
   SELECT ORDLINE
   SET ORDER TO ORDLINE IN ORDLINE
   lcMastFile = lcTempLine
ENDIF          && end if user does not select any orders [no data found],
*-- if user select by orders only, and want to summarize data.
IF (RECCOUNT(lcTempLine) = 0 AND RECCOUNT(lcWorkFile) > 0) AND llRpSummMt
  SELECT(lcWorkFile)
  PRIVATE lcOldKey
  lcOldKey = KEY()
  IF llCurInGrp
    INDEX ON STYLE + CCURRCODE + DTOS(COMPLETE) + CORDTYPE + ORDER TAG (lcWorkFile)
  ELSE
    INDEX ON STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER TAG (lcWorkFile)
  ENDIF
  SCAN
    IF llCurInGrp
      lcSeekExp = STYLE + CCURRCODE + DTOS(COMPLETE) + CORDTYPE + ORDER
    ELSE
      lcSeekExp = STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER
    ENDIF
    IF !SEEK(lcSeekExp,lcTempLine)
      SCATTER MEMVAR MEMO
      m.cCurrCode = ORDHDR.cCurrCode
      *--Initial value for sum variables.
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
                 m.Qty7,m.Qty8,m.TotQty,m.cTempKey
      lnRecNum = RECNO(lcWorkFile)
      =SEEK(lcSeekExp,lcworkfile)
      IF llCurInGrp
        SUM  Qty1 , Qty2 , Qty3 , Qty4 , Qty5 , Qty6 , Qty7 , Qty8 , TotQty , TotQty*Price ;
        TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,m.cTempKey   ;
        REST WHILE STYLE + CCURRCODE + DTOS(COMPLETE) + CORDTYPE + ORDER = lcSeekExp
      ELSE
       SUM  Qty1 , Qty2 , Qty3 , Qty4 , Qty5 , Qty6 , Qty7 , Qty8 , TotQty , TotQty*Price ;
       TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,m.cTempKey   ;
       REST WHILE Style + DTOS(Complete) + cordtype + order = lcSeekExp
      ENDIF
      m.cTempKey = STR(m.cTempKey,16,2)  && Total amount.
      GO lnRecNum IN (lcWorkFile)
      *--End of SumStyle.
      IF ORDHDR.STATUS = "C"
        m.QtType = 'Book'
        m.SP     = ORDHDR.STATUS + ALLTRIM(ORDHDR.PRIORITY)
      ELSE
        m.QtType = 'Open'
        m.SP     = ORDHDR.STATUS + ALLTRIM(ORDHDR.PRIORITY)
      ENDIF
      IF SEEK(IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store),'CUSTOMER')
        m.Name      = CUSTOMER.STname
        m.Buyer     = CUSTOMER.Buyer
        m.Phone     = TRANSFORM(CUSTOMER.Phone1,lcPhonPict)
        m.State     = CUSTOMER.Caddress4
      ENDIF
      INSERT INTO (lcTempLine) FROM MEMVAR
    ENDIF  
  ENDSCAN

  IF llCurInGrp
    lcSeekExp = STYLE + CCURRCODE + DTOS(COMPLETE) + CORDTYPE + ORDER
  ELSE
    lcSeekExp = STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER
  ENDIF
  INDEX ON &lcOldKey TAG (lcWorkFile)
  lcMastFile = lcTempLine
ENDIF

*-- if report master file is the work file and index 
*-- does not match sort by case, reindex data. 
IF (lcMastFile = lcWorkFile) AND (lcWorkTag != lcIndexTg)
  SELECT (lcWorkFile)
  INDEX ON &lcIndexTg TAG (lcWorkFile)
ENDIF
*-- end of lfScanData.

*!******************************************************************************************************
*! Name      : lfvOptMsg
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!******************************************************************************************************
*! Called from : Option Grid    [Optional Message option]
*!******************************************************************************************************
*! Calls       : gfOptMsg()
*!******************************************************************************************************
*! Passed Parameters : None
*!******************************************************************************************************
*! Return      : None
*!******************************************************************************************************
*! Example     : = lfvOptMsg()
*!******************************************************************************************************
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[1,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[1,2] = 75                && Line length
= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- end of lfvOptMsg.

*!******************************************************************************************************
*! Name      : lfwOldVal
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : When function to get the Old value
*!******************************************************************************************************
*! Called from : Some of the Option Grid fields
*!******************************************************************************************************
*! Calls       : None
*!******************************************************************************************************
*! Passed Parameters : None
*!******************************************************************************************************
*! Return      : None
*!******************************************************************************************************
*! Example     : = lfwOldVal()
*!******************************************************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.
*!******************************************************************************************************
*! Name      : lfSelcObjs
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Enable and disable selected objects.
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfSelcObjs()
*!******************************************************************************************************
FUNCTION lfSelcObjs
PARAMETERS lnObjNum,lcObjState,llClearVal
IF llClearVal AND (lcObjState = 'D' AND !EMPTY(laOGFxFlt[lnObjNum,6]))
  laOGFxFlt[lnObjNum,6] = ''
ENDIF  
laOGObjCnt[lnObjNum + lnVarbEnd] = (lcObjState = 'E')
= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnObjNum)) + ',6]')  && Enable / Disable Object .
*-- end of lfSelcObjs.

*!******************************************************************************************************
*! Name      : lfvSortBy
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : 1- Enable and disable some variavle objects due to sort case
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState,lfPreObjs
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Note      : 1- In this function we want to know old value to enable/disable
*!           :    objects due to some sort cases, and we transfer it to 
*!           :    its corressponding character because option grid returns
*!           :    its item number in popup, the idea of enable/disable in
*!           :    this function is to full control printing and do not enable
*!           :    enabled button or disable disabled button.
*!           : 2- In some cases we rise summarization flag to Recollect data again.
*!******************************************************************************************************
*! Example   : =lfvSortBy()
*!******************************************************************************************************
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
CLEAR READ
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
    CASE lcRpSortBy = 'O'    && Sort by order
      *-- Enable/disable variable objects due to sort case.
      IF lcRpKind = 'D'
        IF laOldVal = 'S'
          llChSumm = IIF(llRpSummMt,.T.,llChSumm)
        ENDIF
      ENDIF
      *-- Enable/disable variable objects due to sort case. 
    CASE lcRpSortBy = 'S'      && Sort by style
      IF lcRpKind = 'D'
      ENDIF
    CASE lcRpSortBy = 'G'    && Sort by style group
      *-- Enable/disable variable objects due to sort case.
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() && Prepair objects.
      ENDIF  
    CASE lcRpSortBy = 'F'    && Sort by fabric
      *-- Enable/disable variable objects due to sort case.
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
    CASE lcRpSortBy = 'W'    && Sort by location
      *-- Enable/disable variable objects due to sort case. 
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
    CASE lcRpSortBy = 'R'    && Sort by sales representative
      *-- Enable/disable variable objects due to sort case. 
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
    CASE lcRpSortBy = 'D'    && Sort by complete date
      *-- Enable/disable variable objects due to sort case.
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
  ENDCASE          && end Different sort by cases.
ENDIF
*-- end of lfvSortBy.

*!******************************************************************************************************
*! Name      : lfPreObjs
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Enable/Disable controled objects in 4 sort cases
*!           : - Style group, Fabric, Sales Rep., Complete date
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfPreObjs()
*!******************************************************************************************************
FUNCTION lfPreObjs
IF lcRpKind = 'D'
  IF laOldVal = 'S'
    llChSumm = IIF(llRpSummMt,.T.,llChSumm)
  ENDIF
ENDIF
*-- end of lfPreObjs.

*!******************************************************************************************************
*! Name      : lfObjState
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : This function used to calculate object number and call 
*!           : global show function to enable/disable object due to passed state.
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!******************************************************************************************************
*! Passed Parameters  : 1- ('E' -> enable,'D' disable)
*!                    : 2- Object number
*!                    : 3- Object variable
*!                    : 2- Object value
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfObjState()
*!******************************************************************************************************
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

*!******************************************************************************************************
*! Name      : lfvSizes
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Control Form name, Enable/disable some objects.
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfvSizes()
*!******************************************************************************************************
FUNCTION lfvSizes
lcRpForm = IIF(llRpSizes,'SORDREDD','SORDREDS')
= lfRepPltFr(lcRpForm)
llRpScale = IIF(llRpSizes,.T.,.F.)
CLEAR READ
*-- end of lfvSizes.

*!******************************************************************************************************
*! Name      : lfvKind
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Enable/disable some objects due to report kind (Detail/Summary)
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfvKind()
*!******************************************************************************************************
FUNCTION lfvKind
IF lcRpKind = 'S'
  STORE .F. TO llRpSummMt,llRpOrdLnt,llRpOrdNot
  lcRpSrt2 = "L"
ENDIF
CLEAR READ
*-- end of lfvKind.

*!******************************************************************************************************
*! Name      : lfvSumm
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Enable/disable Order line notes object
*!******************************************************************************************************
*! Calls     : Procedures : ....
*!             Functions  : lfObjState
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfvSumm()
*!******************************************************************************************************
FUNCTION lfvSumm
llRpOrdLnt = IIF(llRpSummMt ,.F.,llRpOrdLnt)
llChSumm   = .T.
CLEAR READ

*-- end of lfvSumm.
*!******************************************************************************************************
*! Name      : lfsrvSty
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Rise change style flag, in range browse screen.
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfsrvSty()
*!******************************************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!******************************************************************************************************
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

*!******************************************************************************************************
*! Name      : lfStySum
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : sum a specific field for the current style in style file
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : Calculated field value.
*!******************************************************************************************************
*! Example   : =lfStySum()
*!******************************************************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0
IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(LEFT(Style,lnLenth)) == ALLTRIM(lcSty)
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
*!******************************************************************************************************
*! Name      : lfSRVFab
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfSRVFab()
*!******************************************************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!******************************************************************************************************
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

*!******************************************************************************************************
*! Name      : lfFabSum
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : sum a specific field for the current fabric in fabric file
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : Calculated field value.
*!******************************************************************************************************
*! Example   : =lfFabSum()
*!******************************************************************************************************
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

*!******************************************************************************************************
*! Name        : lfsrAcc
*! Developer   : NADER NABIL (NNA)
*! Date        : 04/18/2005
*! Purpose     : Rise change account flag, in range browse screen.
*!******************************************************************************************************
*! Calls       : NONE
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! P_arameters : None
*!******************************************************************************************************
*! Returns     : None
*!******************************************************************************************************
*! Example     : =lfsrAcc()
*!******************************************************************************************************
*! Note        : S symbol is [S,Set]
*!******************************************************************************************************
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
*!******************************************************************************************************
*! Name        : lfsrLoc
*! Developer   : NADER NABIL (NNA)
*! Date        : 04/18/2005
*! Purpose     : Rise change Location flag, in range browse screen.
*!******************************************************************************************************
*! Calls       : NONE
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! P_Parameters: None
*!******************************************************************************************************
*! Returns     : None
*!******************************************************************************************************
*! Example     : =lfsrLoc()
*!******************************************************************************************************
*! Note        : S symbol is [S,Set]
*!******************************************************************************************************
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
*!******************************************************************************************************
*! Name        : lfsrRep
*! Developer   : NADER NABIL (NNA)
*! Date        : 04/18/2005
*! Purpose     : Rise change sales rep. flag, in range browse screen.
*!******************************************************************************************************
*! Calls       : NONE
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! P_Parameters: None
*!******************************************************************************************************
*! Returns     : None
*!******************************************************************************************************
*! Example     : =lfsrRep()
*!******************************************************************************************************
*! Note        : S symbol is [S,Set]
*!******************************************************************************************************
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
*!******************************************************************************************************
*! Name      : lfSScale
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Scale In Range set function.
*!******************************************************************************************************
*! Passed Parameters : Dummy Parameter for set only.
*!******************************************************************************************************
*! Return      : ....
*!******************************************************************************************************
*! Example     : =lfSScale()
*!******************************************************************************************************
FUNCTION lfSScale
PARAMETERS lcDummyPrm
*IF llExtSize
*  GO TOP IN SCALEHD
*ELSE
  GO TOP IN SCALE
*ENDIF
*-- end of lfSScale.
*!******************************************************************************************************
*! Name        : lfSROrder
*! Developer   : NADER NABIL (NNA)
*! Date        : 04/18/2005
*! Purpose     : Rise change order flag, in range browse screen.
*!******************************************************************************************************
*! Calls       : NONE
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! P_Parameters: None
*!******************************************************************************************************
*! Returns     : None
*!******************************************************************************************************
*! Example     : =lfSROrder()
*!******************************************************************************************************
*! Note        : S symbol is [S,Set- R,ReSet]
*!******************************************************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm
llChOrder = .T.
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

*-- end of lfSROrder.
*!******************************************************************************************************
*! Name      : lfCollTime
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Calcualte spent time in data collection.
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!******************************************************************************************************
*! Returns            : Spent time.
*!******************************************************************************************************
*! Example   : =lfCollTime()
*!******************************************************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.
*!******************************************************************************************************
*! Name      : lfSumStyle
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Summarize multi store styles using one file for scan and sum.
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Passed Parameters  : sum file, sum expression
*!******************************************************************************************************
*! Returns            : None.
*!******************************************************************************************************
*! Example   : =lfSumStyle()
*!******************************************************************************************************
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
*!******************************************************************************************************
*! Name      : lfSumMulti
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Summarize multi store styles using two aliass from same file for scan and sum,
*!           : in this case ordline file is used with out any order to activiate rushmore, thus we open another 
*!           ; alias for make sum in the fastest way.
*!******************************************************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Passed Parameters  : sum expression
*!******************************************************************************************************
*! Returns            : None.
*!******************************************************************************************************
*! Example   : =lfSumMulti()
*!******************************************************************************************************
FUNCTION lfSumMulti
PARAMETERS lcSumExpr
lnOldAlias=SELECT(0)
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
           m.Qty7,m.Qty8,m.TotQty,m.cTempKey

SELECT SUMMULTI  && Order line alias (sum for all file)
= SEEK(lcSumExpr)

SUM  Qty1 , Qty2 , Qty3 , Qty4 , Qty5 , Qty6 , Qty7 , Qty8 , TotQty , TotQty*Price ;
 TO  m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty, m.cTempKey   ;
 REST WHILE Style + DTOS(Complete) + cordtype + order = lcSumExpr  

m.cTempKey = STR(m.cTempKey,16,2)
SELECT(lnOldAlias)
*-- end of lfSumMulti.
*!******************************************************************************************************
*! Name        : lfGetNotes
*! Developer   : NADER NABIL (NNA)
*! Date        : 05/27/1998
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!******************************************************************************************************
*! Called from : SORDREDD.FRX, OR SORDREDS.FRX [Variable lcDum in the report]
*!******************************************************************************************************
*! Calls       : Procedures : ....
*!               Functions  : ....
*!******************************************************************************************************
*! Passed Parameters : None
*!******************************************************************************************************
*! Return            : Null
*!******************************************************************************************************
*! Example           : = lfGetNotes()
*!******************************************************************************************************
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
    CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B' + IIF(ORDHDR.cOrdType = 'T','T','')+Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
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
    CASE llRpOrdNot AND SEEK('B' + IIF(ORDHDR.cOrdType = 'T','T','')+Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcTitle = IIF(RECNO(lcMastFile) = lnLastRec,;
                'Order :' + ORDER + ' Notepad.','')
      lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10),'')
  ENDCASE
ENDIF
RETURN ''

*-- end of lfGetNotes.
*!******************************************************************************************************
*! Name      : lfLastRec
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Calculate last Record in order details. [ORDER GROUP]
*!           : we use another alias to unchange record pointer of report file.
*!******************************************************************************************************
*! Called from : [SORDREDD.FRX OR SORDREDS.FRX, ORDER GROUP HEADER BAND] 
*!******************************************************************************************************
*! Calls       : ....
*!******************************************************************************************************
*! Passed Parameters : None
*!******************************************************************************************************
*! Return      : NULL
*!******************************************************************************************************
*! Example     : = lfLastRec()
*!******************************************************************************************************
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
*!******************************************************************************************************
*! Name      : lfvCoorGrp
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Rise change print coordinate groups flag.
*!******************************************************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfvCoorGrp()
*!******************************************************************************************************
FUNCTION lfvCoorGrp
llChCoord = .T.
*-- end of lfvCoorGrp.

*!******************************************************************************************************
*! Name      : lfClearRep
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Function that we call when Close the option grid.
*!******************************************************************************************************
*! Called from : [Option Grid] < Close > button.
*!******************************************************************************************************
*! Calls       : ....
*!******************************************************************************************************
*! Passed Parameters : None
*!******************************************************************************************************
*! Return      : None
*!******************************************************************************************************
*! Example     : = lfClearRep()
*!******************************************************************************************************
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
*!******************************************************************************************************
*! Name      : lfEvalSegs
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Evaluate NonMajor Type and variables.
*!******************************************************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!******************************************************************************************************
*! Calls       : ....
*!******************************************************************************************************
*! Passed Parameters : None
*!******************************************************************************************************
*! Return      : None
*!******************************************************************************************************
*! Example     : = lfEvalSegs()
*!******************************************************************************************************
FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure.
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
 
*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you does not find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- if multi currency evaluate currency arrays.
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
RETURN ''

*-- end of lfEvalSegs.
*!******************************************************************************************************
*! Name      : lfNMajType
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Mask NonMajor segments .
*!******************************************************************************************************
*! Called from : lfEvalSegs.
*!******************************************************************************************************
*! Calls       : ....
*!******************************************************************************************************
*! Passed Parameters : None
*!******************************************************************************************************
*! Return      : None
*!******************************************************************************************************
*! Example     : = lfNMajType()
*!******************************************************************************************************
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

*!******************************************************************************************************
*! Name      : lfMakeExpr
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Make expression for operator is either BETWEEN or INLIST.
*!******************************************************************************************************
*! Called From : lfSscanData.
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : Operator expression.
*!******************************************************************************************************
*! Example   : = lfMakeExpr()
*!******************************************************************************************************
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

*!******************************************************************************************************
*! Name      : lfGetRepVr
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : 1- Put both index and group expressions for all sort cases.
*!******************************************************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Report code
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Notes     : 1- lcIndexTg : is master report file index due to sort case.
*!******************************************************************************************************
*! Example   : = lfGetRepVr()
*!******************************************************************************************************
FUNCTION lfGetRepVr
*-- lcOutHeadL : Left  title of outer group header.
*-- lcOutHeadR : Right title of outer group header.  
*-- lcInnHeadL : Left  title of inner group header.
*-- lcInnHeadR : Right title of inner group header.
IF lcRpSortBy = 'S'
  lcOutHeadL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen),19) + '  : ']
  lcOutHeadR = [Style.cStyMajor]
  lcInnHeadL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19)+ '  : ']
  lcInnHeadR = [SUBSTR(Style,lnMajorLen + 2) + '  ----  ' + ALLTRIM(STYLE.Desc1)]
ELSE
  STORE [''] TO lcOutHeadL,lcOutHeadR,lcInnHeadL,lcInnHeadR
ENDIF

lcLineCurr = lfCurrPrnt()

*-- Different sort by cases.
DO CASE
  CASE lcRpSortBy = 'A'   && Sort by account 

    *!* lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style
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
    *-- report variables data account case 
    lcSubTitle = 'Account Number'  && Report sort title
    lcInnGrp   = [ORDER]           && Inner .FRX group field.
    IF llTextMode
      lcInnFootL = [ORDER + ', Cust PO#'+] +;
                   [IIF(Ordhdr.MultiPo,'*Multi PO*',PADL(OrdHdr.CustPo,10)) + ' Enter in ' + ALLTRIM(DTOC(OrdHdr.Entered))]

      lcOutFootL = [Customer.Account+' '+PADR(ALLTRIM(CUSTOMER.BtName),15)+"/"+EVALUATE(lcLineCurr)+', ' + TRANSFORM(CUSTOMER.Phone1,lcPhonPict)]
    ELSE
      lcInnFootL = ['Order # ' + ORDER + '     Cust PO #']    
      lcInnFootR = [IIF(Ordhdr.MultiPo,'*Multi PO*', PADL(OrdHdr.CustPo,15)) + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]  && Right title of inner group footer.
      lcOutFootL = ['Account # ' + Customer.Account]  && Left title of outer group footer.
      lcOutFootR = [PADR(ALLTRIM(CUSTOMER.BtName),25) +"/"+EVALUATE(lcLineCurr)+ ", " + TRANSFORM(CUSTOMER.Phone1,lcPhonPict)]  && Right title of outer group footer.
    ENDIF
    IF llCurInGrp
      lcSeekVal  = [ACCOUNT+cCurrCode+cOrdType+ORDER]
      lcOutGrp   = [ACCOUNT+CCURRCODE]         && Outer .FRX group field.
    ELSE
      lcSeekVal  =  [ACCOUNT+cOrdType+ORDER]
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
        
        lcStorCond = IIF(llTextMode,[.T.],[ORDHDR.MULTI="Y"])
        lcInnGrp   = [IIF(EVALUATE(lcStorCond),STORE,"")]
        lcInnFootL = [IIF(EVALUATE(lcStorCond),"Store : "+STORE,"")] 

    ENDCASE
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

      *-- report variables data order case   
    ENDIF  
    IF llCurInGrp
      lcSeekVal = [cCurrCode+cOrdType+ORDER]
      lcOutGrp  = [CCURRCODE+ORDER]
    ELSE
      lcSeekVal = [cOrdType+ORDER]
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
    *-- report variables data style case 
    lcStyTitle = IIF ('GFITEM' $ ALLTRIM(UPPER(lcStyTitle)),;
                      EVALUATE(lcStyTitle),lcStyTitle)

    lcSubTitle = lcStyTitle
    lcInnGrp   = [SUBSTR(Style,lnMajorLen + 2)]
    IF llTextMode
      lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19) + ' : '+]+;
                   [SUBSTR(Style,lnMajorLen + 2)]   

      lcOutFootL = [ALLTRIM(Style.cStyMajor) + "/"+EVALUATE(lcLineCurr)+ ", "+ '(' + ALLTRIM(Style.Fabric) + ')']  
    ELSE
      lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19) + '  : ']
      lcInnFootR = [SUBSTR(Style,lnMajorLen + 2)]
      lcOutFootL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen),19) + '  : ']
      lcOutFootR = [ALLTRIM(Style.cStyMajor) +"/"+EVALUATE(lcLineCurr)+ ", "+ '( ' + ALLTRIM(Style.Fabric) + ' )']
    ENDIF  
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

    *-- report variables data Style group case
    lcSubTitle = 'Style Group'
    lcInnGrp   = [STYLE]
    IF llTextMode
      lcInnFootL = ['Style (' + Style + ') :'+ALLTRIM(Style.Desc)]
      lcOutFootL = [gfCodDes(SUBSTR(cTempKey,8,6),'CSTYGROUP',.T.)+"/"+EVALUATE(lcLineCurr)]

    ELSE
      lcInnFootL = ['Style  ( ' + Style + ' )  :']
      lcInnFootR = [ALLTRIM(Style.Desc)]
      lcOutFootL = ['Group  ( ' + SUBSTR(cTempKey,8,6) + ' )  :']
      lcOutFootR = [Codes.cDiscrep+"/"+EVALUATE(lcLineCurr)]
      *-- report variables data Style group case   
    ENDIF  
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
    *-- report variables data fabric case
    lcSubTitle = 'Primary Fabric'
    lcInnGrp   = [STYLE]
    IF llTextMode
      lcInnFootL = ['Style (' + Style + ') :'+ALLTRIM(Style.Desc)]
      lcOutFootL = [LEFT(cTempKey,7) + ':'+ALLTRIM(Fabric.Desc)+"/"+EVALUATE(lcLineCurr)]

    ELSE

      lcInnFootL = ['Style  ( ' + Style + ' )  :']
      lcInnFootR = [ALLTRIM(Style.Desc)]
      lcOutFootL = ['Fabric  ( ' + LEFT(cTempKey,7) + ' )  :']
      lcOutFootR = [ALLTRIM(Fabric.Desc)+"/"+EVALUATE(lcLineCurr)]
      *-- report variables data fabric case   
    ENDIF  
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
    *-- report variables data location case.
    lcSubTitle = 'Location'
    lcInnGrp   = ['']
    lcInnFootL = [''] 
    IF llTextMode
      lcOutFootL = [cWareCode + ':'+ALLTRIM(Warehous.cDesc)+"/"+EVALUATE(lcLineCurr)]

    ELSE
      lcInnFootR = ['']
      lcOutFootL = ['Location # ' + cWareCode + ' :']
      lcOutFootR = [ALLTRIM(Warehous.cDesc)+"/"+EVALUATE(lcLineCurr)]
      *-- report variables data location case   
    ENDIF  
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
    *-- report variables data sales Rep. case.
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
      lcIndexTg = 'DTOS(COMPLETE)+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
      lcOutGrp  = [DTOS(COMPLETE)+CCURRCODE]
    ELSE
      lcIndexTg = 'DTOS(COMPLETE)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
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
      *-- report variables data Complete date case   
    ENDIF
  CASE lcRpSortBy = 'U'    && Sort by currency
    IF lcRpSrt2 = 'L'    && Sort by Line#
      lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
    ELSE                  && Else Sort by Style	
      lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
	ENDIF
    *-- report variables data order case.
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
    ENDIF  
    lcSeekVal = [cCurrCode+cOrdType+ORDER]
    lcOutGrp  = [CCURRCODE]
    
ENDCASE          && end Different sort by cases.
IF llCurInGrp AND (lcRpSortBy <> 'U')
  lcSubTitle = lcSubTitle+"/Currency"
ENDIF

*-- end of lfGetRepVr.
*!******************************************************************************************************
*! Name      : lfCreatCur
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Create cursor
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Report code
*!******************************************************************************************************
*! Passed Parameters  : Cursor Name
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : = lfCreatCur()
*!******************************************************************************************************
FUNCTION lfCreatCur
PARAMETERS lcCurName
CREATE TABLE (gcWorkDir+lcCurName) FROM ARRAY laTempStru
*-- end of lfCreatCur.

*!******************************************************************************************************
*! Name      : lfPipeExpr
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Mask inlist expression.
*!******************************************************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Report code
*!******************************************************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!******************************************************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!******************************************************************************************************
*! Example   : = lfPipeExpr('AS|BS|CS',2)
*!******************************************************************************************************
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

*!******************************************************************************************************
*! Name      : lfvOStatus
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!******************************************************************************************************
*! Calls     : None
*!******************************************************************************************************
*! Called from : Report code
*!******************************************************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!******************************************************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!******************************************************************************************************
*! Example   : = lfvOStatus()
*!******************************************************************************************************
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
lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))

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

*!******************************************************************************************************
*! Name      : lfGetWork
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : - Compute work proccessing
*!******************************************************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Report code
*!******************************************************************************************************
*! Passed Parameters  : ...
*!******************************************************************************************************
*! Returns            : Null
*!******************************************************************************************************
*! Example   : = lfGetWork()
*!******************************************************************************************************
FUNCTION lfGetWork
STORE '' TO laStock,laWip,lnStkOrWip
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
*-- Calculate Wip and Stock Values   
lnStkOrWip = TRANSFORM(STYLE.TotStk + STYLE.TotWip,'999999')  && Calculate wip + stock values
lnStkOrWip = IIF(VAL(lnStkOrWip) = 0 , '' , lnStkOrWip)
RETURN ''

*-- End of lfGetWork.
*!******************************************************************************************************
*! Name      : lfWorkEnd
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : - End Compute work proccessing
*!******************************************************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Report code
*!******************************************************************************************************
*! Passed Parameters  : ...
*!******************************************************************************************************
*! Returns            : Null
*!******************************************************************************************************
*! Example   : = lfWorkEnd()
*!******************************************************************************************************
FUNCTION lfWorkEnd
STORE '' TO laStock,laWip,lnStkOrWip
RETURN ''
*-- End of lfWorkEnd.
*!******************************************************************************************************
*! Name      : lfArrDumy
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Fill Sort and select arrays
*!******************************************************************************************************
*! Example   : = lfArrDumy()
*!******************************************************************************************************
FUNCTION lfArrDumy
PRIVATE lnSelElms , lnSrtElms
lnSelElms = 4
lnSrtElms = 6
DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1],;
          laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1],;
          laSort2Des[2,1],laSort2Val[2,1]

*-- Sort 2 elementes.
laSort2Des[1,1] = "Line#"
laSort2Des[2,1] = lcStyMajor
laSort2Val[1,1] = "L"
laSort2Val[2,1] = "S"

*-- Sort 1 base elements.
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

*-- Fill Select by array base elements.
laSlctDesc[1,1] = 'Account'
laSlctDesc[2,1] = 'Primary Sales Representative'
laSlctDesc[3,1] = lcStyMajor
laSlctDesc[4,1] = 'All'

laSlctVal[1,1]  = 'A'
laSlctVal[2,1]  = 'R'
laSlctVal[3,1]  = 'S'
laSlctVal[4,1]  = 'L'
*-- Fill Select by array base elements.

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

*-- End of lfArrDumy.
*!******************************************************************************************************
*! Name      : lfItmPos
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Evaluate fixed filter position within array.
*!******************************************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!******************************************************************************************************
*! Called from : Report code
*!******************************************************************************************************
*! Passed Parameters  : ...
*!******************************************************************************************************
*! Returns            : Position
*!******************************************************************************************************
*! Example   : = lfItmPos()
*!******************************************************************************************************
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
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : to save the current Scale after printing it in order not to 
*!             print it except when Scale changes 
*!**************************************************************************
*! Calls     : Procedures : ....
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
llFirstSc = .T.
RETURN ''
*-- end of lfAssignSc.
*!**************************************************************************
*! Name      : lfScalePgH
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
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
*!******************************************************************************************************
*! Name      : lfInnGrpIn
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Evaluate inner group values, when you enter group header.
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Called from : All FRXs (DOS Format)
*!**************************************************************************
*! Returns            : Null
*!******************************************************************************************************
*! Example   : =lfInnGrpIn()
*!******************************************************************************************************
FUNCTION lfInnGrpIn
lcInnGrpIn  = EVALUATE(lcInnGrp)
RETURN ''
*-- end of lfInnGrpIn.

*!******************************************************************************************************
*! Name      : lfOutGrpIn
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Evaluate outer group values, when you enter group header.
*!******************************************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Called from : All FRXs (DOS Format)
*!******************************************************************************************************
*! Returns            : Null
*!******************************************************************************************************
*! Example   : =lfOutGrpIn()
*!******************************************************************************************************
FUNCTION lfOutGrpIn
lcOutGrpIn = EVALUATE(lcOutGrp)
RETURN ''
*-- end of lfOutGrpIn.

*!******************************************************************************************************
*! Name      : lfInnGrpOp
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Evaluate inner group values, when you in group Footer.
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Called from : All FRXs (DOS Format)
*!******************************************************************************************************
*! Returns            : Null
*!******************************************************************************************************
*! Example   : =lfInnGrpOp()
*!******************************************************************************************************
FUNCTION lfInnGrpOp
llLstMulti = (ORDHDR.MULTI = "Y")
lcInnGrpOp = EVALUATE(lcInnGrp)
lcPrnInnL  = EVALUATE(lcInnFootL)

FOR I = 1 TO 8
  LCI=ALLTRIM(STR(I))
  lnPrnInnQ&LCI = lnInnQty&LCI
  lnPrnIShp&LCI = lnInnShp&LCI
  lnPrnICnl&LCI = lnInnCnl&LCI
ENDFOR
lnPrnInnTQ = lnInnTtQty
lnPrnInnAm = lnGrInnAmt
lnPrnITShp = lnInnTShp
lnPrnITCnl = lnInnTCnl
RETURN ''
*-- end of lfInnGrpOp.

*!******************************************************************************************************
*! Name      : lfOutGrpOp
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Evaluate outer group values, when you in group Footer.
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Called from : All FRXs (DOS Format)
*!******************************************************************************************************
*! Returns            : Null
*!******************************************************************************************************
*! Example   : =lfOutGrpOp()
*!******************************************************************************************************
FUNCTION lfOutGrpOp
lcOutGrpOp = EVALUATE(lcOutGrp)
lcPrnOutL  = EVALUATE(lcOutFootL)
FOR R = 1 TO 8
  LCI=ALLTRIM(STR(R))
  lnPrnOutQ&LCI = lnOutQty&LCI
  lnPrnOShp&LCI = lnOutShp&LCI
  lnPrnOCnl&LCI = lnOutCnl&LCI
ENDFOR
lnPrnOutTQ = lnOutTtQty
lnPrnOutAm = lnGrOutAmt
lnPrnOTShp = lnOutTShp
lnPrnOTCnl = lnOutTCnl
RETURN ''
*-- end of lfOutGrpOp.

*!******************************************************************************************************
*! Name      : lfvCurDisp
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Activate currency display screen to get user 
*!           : selection for currencies.
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfvCurDisp()
*!******************************************************************************************************
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- end of lfvCurDisp.

*!******************************************************************************************************
*! Name      : lfCurrPrnt
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Compute Currency symbol to print.
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfCurrPrnt()
*!******************************************************************************************************
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

*!******************************************************************************************************
*! Name      : lfChCurSm
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Share with last function to Compute Currency symbol to print.
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Called from : All FRXs (DOS Format)
*!******************************************************************************************************
*! Returns            : None
*!******************************************************************************************************
*! Example   : =lfChCurSm()
*!******************************************************************************************************
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

*!******************************************************************************************************
*! Name      : lfCurrDesc
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Currency description if sort by currency.
*!******************************************************************************************************
*! Passed Parameters  : None
*!******************************************************************************************************
*! Returns            : Currency description.
*!******************************************************************************************************
*! Example   : =lfCurrDesc()
*!******************************************************************************************************
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
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
DO CASE
  *-- Orders
  CASE lcRpEdiPrn="O"
    lcRpEdiFlt = [OrdHdr.cOrdType = 'O' AND !OrdHdr.lEdiOrder]
    lcRpOrdTyp = 'O'
  *-- EDI recieved orders
  CASE lcRpEdiPrn="R"
    lcRpEdiFlt = [OrdHdr.cOrdType = 'O' AND OrdHdr.lEdiOrder]
    lcRpOrdTyp = 'O'
  *-- EDI temp orders
  CASE lcRpEdiPrn="T"
    lcRpEdiFlt = [OrdHdr.cOrdType = 'T']
    lcRpOrdTyp = 'T'
  CASE lcRpEdiPrn="B"
  *-- Both Orders + EDI Orders.
    lcRpEdiFlt = [OrdHdr.cOrdType = 'O']
    lcRpOrdTyp = 'O'
  CASE lcRpEdiPrn="A"
  *-- All
    lcRpEdiFlt = ''
    lcRpOrdTyp = ''
ENDCASE
llClearOrd = .T.
*-- end of lfvEdiOrd.

*!**************************************************************************
*! Name      : lfFillArry
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : to Fill the array with the selected Seasons.
*!**************************************************************************
*! Example   : =lfFillArry()
*!**************************************************************************
FUNCTION lfFillArry

DIMENSION laSeasnS[2,2]
laSeasnS[1,1] = lcStyMajor +' season'
laSeasnS[2,1] = 'Order header season'

DIMENSION laSeasnV[2,2]
laSeasnV[1,1] = 'S'
laSeasnV[2,1] = 'O'

*!******************************************************************************************************
*! Name      : lfStitle
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : To get the Title for the STATE ,ZIP
*!             according to its country
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Calls       : ....
*!******************************************************************************************************
*! Passed Parameters : None
*!******************************************************************************************************
*! Return      : None
*!******************************************************************************************************
*! Example     : = lfStitle()
*!******************************************************************************************************
FUNCTION lfStitle
SET ORDER TO Ccomp_id IN SYCCOMP   && To use it to get state title.
IF !USED('SYCINT')
  = gfOpenFile(gcSysHome+'SYCINT',gcSysHome+'Ccontcode','SH')
ELSE
  SET ORDER TO Ccontcode IN SYCINT   && To use it to get state title.
ENDIF
= SEEK(gcAct_Comp,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')
lcZipTitle = SYCINT.CPART5LAB
RETURN (SYCINT.CPART4LAB)
*-- end of lfStitle.
*!******************************************************************************************************
*! Name      : lfZtitle
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Valid for the ZIP selection
*!             according to its country
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Passed Parameters : None
*!******************************************************************************************************
*! Return      : None
*!******************************************************************************************************
*! Example     : = lfZtitle()
*!******************************************************************************************************
FUNCTION lfZtitle
SET ORDER TO Ccomp_id IN SYCCOMP   && To use it to get state title.
IF !USED('SYCINT')
  = gfOpenFile(gcSysHome+'SYCINT',gcSysHome+'Ccontcode','SH')
ELSE
  SET ORDER TO Ccontcode IN SYCINT   && To use it to get state title.
ENDIF
= SEEK(gcAct_Comp,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')
lcZipTitle = SYCINT.CPART5LAB

RETURN (SYCINT.CPART5LAB)
*-- end of lfZtitle.
*!******************************************************************************************************
*! Name      : lfvStates
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Valid for the states selection
*!             according to its country
*!******************************************************************************************************
*! Called from : Option Grid
*!******************************************************************************************************
*! Calls       : ....
*!******************************************************************************************************
*! Passed Parameters : None
*!******************************************************************************************************
*! Return      : None
*!******************************************************************************************************
*! Example     : = lfvStates()
*!******************************************************************************************************
FUNCTION lfvStates

PRIVATE lcFile_Ttl, lcBrfields, lcStateObj , lcStateVal
lcStateObj = SYS(18)                    && Varible to hold  the name of the memory variable used to create the current GET field
lcStateVal = PADR(EVALUATE(SYS(18)),6)  && Varible to hold  the value of the current GET field
IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK('N'+PADR('STATE',10) + ALLTRIM(lcStateVal),'CODES'))
   lnCurAlias = SELECT(0)
   *-- browse all country codes.
   SELECT CODES
   DECLARE laCodeFld[2]
   lcFile_Ttl = 'State Codes'
   lcBrfields = 'cCode_No :H= "State Code" ,cDiscrep :H="Description" :30'
   IF gfBrows('FOR cDefCode + cfld_name+ccode_no+cdiscrep = ;
     "N" + "STATE" AND ;
     cRltField="N"','cCode_No,cDiscrep','laCodeFld')
  
     lcStateVal = laCodeFld[1]
   ENDIF
   SELECT (lnCurAlias)
ENDIF    
*-- If user type inValid code does not have ? Accept it.
lcStateVal    = IIF('?' $ lcStateVal,'',lcStateVal)
&lcStateObj = lcStateVal

*-- end of lfvStates.
*!******************************************************************************************************
*! Name      : lfFilMmVar
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Fill Memory variables.
*!******************************************************************************************************
*! Passed Parameters : ....
*!******************************************************************************************************
*! Return      : Null
*!******************************************************************************************************
*! Example     : =lfFilMmVar()
*!******************************************************************************************************
FUNCTION lfFilMmVar
PRIVATE laSetups
DIMENSION laSetups[2,2]
laSetups[1,1] = 'M_USEEXSSC'
laSetups[2,1] = 'M_EXTWIDTH'
=gfGetMemVar(@laSetups)
llExtSize  = laSetups[1,2]
lnExtWidth = laSetups[2,2]
*-- end of lfFilMmVar.

*!******************************************************************************************************
*! Name      : lfGetShpQt
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Get the Shipped Qty for the Order/ Store / Style
*!******************************************************************************************************
*! Passed Parameters : ....
*!******************************************************************************************************
*! Return      : Null
*!******************************************************************************************************
*! Example     : =lfGetShpQt()
*!******************************************************************************************************
FUNCTION lfGetShpQt
PRIVATE lnOldAlias , lcIfExp , lnRecInvNo , lcSumSeek , lnLoopNo
STORE 0 TO lnOldAlias
lnOldAlias = SELECT(0)
lnLoopNo   = 0
lnRecInvNo = RECNO('INVLINE')
lcIfExp = IIF(!EMPTY(ldRpExpShp),' AND EVAL(ldRpExpShp)','')
IF llRpSummMt
  SET ORDER TO SUMSTYLE IN (lcTmpShip)
ENDIF
IF SEEK(ORDLINE.ORDER,'INVLINE') AND INVHDR.CONSOL='Y'
  IF SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'CONSINVL')
    IF !EMPTY(ldRpExpShp) OR llRpShipQt
      set skip to CONSINVL
    ENDIF
    SELECT INVLINE
    SET RELATION OFF INTO INVHDR    
    SELECT CONSINVL
    SET RELATION TO 
    SET RELATION TO Consinvl.invoice+ Consinvl.store+ Consinvl.order INTO Consinvh ADDITIVE
    SET RELATION TO Consinvl.invoice INTO Invhdr ADDITIVE
    SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE   && To scale file.
    lcCustRel = IIF(llRpSummMt,['M' + Account],;
                [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
    SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.
    IF !EMPTY(lcForExp)
      lcScanExp = ' AND ' + lcForExp + IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
    ELSE
      lcScanExp =IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
    ENDIF  
    SCAN REST WHILE Order+STR(Lineno,6)+Invoice = ORDLINE.ORDER+STR(ORDLINE.LINENO,6) FOR CONSINVH.STATUS<>'V' ;
        AND CONSINVL.STORE = ORDLINE.STORE &lcIfExp &lcScanExp
      SCATTER MEMVAR MEMO
      m.Cordtype = ORDLINE.Cordtype
      m.llPrint  = .F.
      m.cRelation= 'N'
      lcSumSeek = IIF(llRpSummMt,"SEEK(ORDLINE.Style+ORDLINE.Cordtype+ORDLINE.Order,lcTmpShip)",;
                                 "SEEK(ORDLINE.Cordtype+ORDLINE.Order+ORDLINE.Store+ORDLINE.Style,lcTmpShip)")
        IF &lcSumSeek
          SELECT(lcTmpShip)
          REPLACE QTY1   WITH QTY1+m.QTY1		,;
                  QTY2   WITH QTY2+m.QTY2		,;
                  QTY3   WITH QTY3+m.QTY3		,;
                  QTY4   WITH QTY4+m.QTY4		,;
                  QTY5   WITH QTY5+m.QTY5		,;
                  QTY6   WITH QTY6+m.QTY6		,;
                  QTY7   WITH QTY7+m.QTY7		,;
                  QTY8   WITH QTY8+m.QTY8		,;
                  TOTQTY WITH TOTQTY+m.TOTQTY
        ELSE
          INSERT INTO (lcTmpShip) FROM MEMVAR
        ENDIF
    ENDSCAN
    SELECT CONSINVL
    SET RELATION OFF INTO SCALE
    SET RELATION OFF INTO CUSTOMER
  ENDIF
ELSE
  IF SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'INVLINE')
    SELECT CONSINVL
    SET RELATION OFF INTO INVHDR    
    SELECT INVLINE
    SET RELATION TO 
    SET RELATION TO Invline.invoice INTO Invhdr ADDITIVE
    SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE   && To scale file.
    lcCustRel = IIF(llRpSummMt,['M' + Account],;
                [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
    SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.
    *-- If lcForExp not Empty then Add 'FOR'
    IF !EMPTY(lcForExp)
      lcScanExp = ' AND ' + lcForExp + IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
    ELSE
      lcScanExp =IIF(!EMPTY(lcRpExp),' AND ' + lcRpExp ,'')
    ENDIF  
    SCAN REST WHILE Order+STR(Lineno,6)+Invoice = ORDLINE.ORDER+STR(ORDLINE.LINENO,6) FOR INVHDR.STATUS<>'V' ;
            AND INVLINE.STORE = ORDLINE.STORE &lcIfExp &lcScanExp
      SCATTER MEMVAR MEMO
      m.llPrint  = .F.
      m.Cordtype = ORDLINE.Cordtype
      lnLoopNo = lnLoopNo + 1
      IF lnLoopNo > 1
        lnRecInvNo = RECNO('INVLINE')      
      ENDIF
      m.cRelation= 'N'
      lcSumSeek = IIF(llRpSummMt,"SEEK(ORDLINE.Style+ORDLINE.Cordtype+ORDLINE.Order,lcTmpShip)",;
                                 "SEEK(ORDLINE.Cordtype+ORDLINE.Order+ORDLINE.Store+ORDLINE.Style,lcTmpShip)")
      IF &lcSumSeek
        SELECT(lcTmpShip)
        REPLACE QTY1   WITH QTY1+m.QTY1		,;
                QTY2   WITH QTY2+m.QTY2		,;
                QTY3   WITH QTY3+m.QTY3		,;
                QTY4   WITH QTY4+m.QTY4		,;
                QTY5   WITH QTY5+m.QTY5		,;
                QTY6   WITH QTY6+m.QTY6		,;
                QTY7   WITH QTY7+m.QTY7		,;
                QTY8   WITH QTY8+m.QTY8		,;
                TOTQTY WITH TOTQTY+m.TOTQTY
      ELSE  
        INSERT INTO (lcTmpShip) FROM MEMVAR
      ENDIF
    ENDSCAN
    SELECT INVLINE
    SET RELATION OFF INTO SCALE
    SET RELATION OFF INTO CUSTOMER
    IF BETWEEN(lnRecInvNo,1,RECCOUNT())
      GOTO lnRecInvNo
    ENDIF
  ENDIF
ENDIF
SELECT(lnOldAlias)
*!* End of function lfGetShpQt.
*!******************************************************************************************************
*! Name      : lfGetCnclQ
*! Developer : NADER NABIL (NNA)
*! Date      : 04/18/2005
*! Purpose   : Get the Cancelled Qty for the Order/ Store / Style
*!******************************************************************************************************
*! Passed Parameters : ....
*!******************************************************************************************************
*! Return      : Null
*!******************************************************************************************************
*! Example     : =lfGetCnclQ()
*!******************************************************************************************************
FUNCTION lfGetCnclQ
PARAMETERS lcTmpFile
PRIVATE lnOldAlias
STORE 0 TO lnOldAlias
lnOldAlias = SELECT(0)
IF SEEK(OrdLine.Cordtype+OrdLine.Order+STR(OrdLine.lineno,6),'ORDCANLN')
  SELECT ORDCANLN
  SET RELATION TO
  SET RELATION TO style INTO Style ADDITIVE
  lcCustRel = IIF(llRpSummMt,['M' + Account],;
              [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
  SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.
  IF EMPTY(lcForExp)
    SCATTER MEMVAR MEMO
    m.QtType   = 'Cancel'
    m.llPrint  = .F.
    m.cRelation= ''
    INSERT INTO (lcTmpFile) FROM MEMVAR
  ELSE
    IF &lcForExp
      SCATTER MEMVAR MEMO
      m.QtType   = 'Cancel'
      m.llPrint  = .F.
      m.cRelation= ''
      INSERT INTO (lcTmpFile) FROM MEMVAR
    ENDIF
  ENDIF
  SELECT ORDCANLN
  SET RELATION TO
ENDIF
SELECT(lnOldAlias)
*!* End of function lfGetCnclQ.
*!*************************************************************
*! Name        : lfvExprt2XL
*! Developer   : Nader Nabil (NNA)
*! Date        : 04/18/2005
*! Purpose     : Valid function for Export to Excel choice
*!*************************************************************
*! Called from : ARPINVDIR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvExprt2XL()
*!*************************************************************
FUNCTION lfvExprt2XL
lcRunStat = IIF(llRpExport,'DISABLE','ENABLE')
SHOW GET pbRun &lcRunStat

*-- End of lfvExprt2XL.
*!*************************************************************
*! Name        : lfExprt2XL
*! Developer   : Nader Nabil (NNA)
*! Date        : 04/18/2005
*! Purpose     : Export data to Excel file
*!*************************************************************
*! Called from : Report Code
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfExprt2XL()
*!*************************************************************
FUNCTION lfExprt2xl

IF RECCOUNT(ALIAS())=0
  = gfModalGen("TRM00052B00000","DIALOG",'')  && 'There are no records to display' 
  RETURN
ENDIF
WAIT WINDOW NOWAIT 'Please wait...'
Dimension laHeadrs[1],laFields[1]
IF llRpSizes 
  lcStr    = "Account,Name,Buyer,Phone,State,Order,Start,Complete,SP,Style,Color,Scale,Qty. Type ,Store,(1),"
  *--NNA
  *lcStr    = lcStr + "(2),(3),(4),(5),(6),(7),(8),Total Qty.,Price"
  lcStr    = lcStr + "(2),(3),(4),(5),(6),(7),(8),Total Qty.,Price,Amount"
  *--NNA
  lcFields = "Account,Name,Buyer,Phone,State,Order,Start,Complete,SP,Style,Style,Style,QTTYPE,Store,QTY1,QTY2,"
  lcFields = lcFields + "QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,PRICE"
ELSE
  *--NNA
  *lcStr    = "Account,Name,Buyer,Phone,State,Order,Start,Complete,SP,Style,Color,Scale,Qty. Type,Store,Total Qty.,Price"
  lcStr    = "Account,Name,Buyer,Phone,State,Order,Start,Complete,SP,Style,Color,Scale,Qty. Type,Store,Total Qty.,Price,Amount"
  *--NNA
  lcFields = "Account,Name,Buyer,Phone,State,Order,Start,Complete,SP,Style,Style,Style,QTTYPE,Store,TotQty,Price"
ENDIF
= gfSubStr(lcStr,@laHeadrs,',')         
= gfSubStr(lcFields,@laFields,',')         
lcTempMemo = gfTempName()
SAVE TO (gcWorkDir+lcTempMemo+'.MEM') 
lcCommLine = (gcWorkDir+lcTempMemo+'.MEM')
IF USED(lcMastFile)
  USE IN &lcMastFile
ENDIF
IF USED('GETLAST')
  USE IN GETLAST
ENDIF
IF USED(lcTmpShip)
  USE IN &lcTmpShip
ENDIF

lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",gcRepHome+'SO\'+[SORED2XL.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF
llAftrExcl = .T.
=lfOpenFilS()
WAIT CLEAR
*-- End of Function lfExprt2XL.

*!*************************************************************
*! Name        : lfOpenFilS
*! Developer   : Nader Nabil (NNA)
*! Date        : 04/18/2005
*! Purpose     : Reopen files that closed in function lfExprt2XL
*!*************************************************************
*! Called from : Report Code
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOpenFilS()
*!*************************************************************
FUNCTION lfOpenFilS
IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR', gcDataDir+'ORDHDR','SH')
ENDIF
IF !USED('CUSTOMER')
  =gfOpenFile(gcDataDir+'CUSTOMER', gcDataDir+'CUSTOMER','SH')
ENDIF
IF !USED('WAREHOUS')
  =gfOpenFile(gcDataDir+'WAREHOUS', gcDataDir+'WAREHOUS','SH')
ENDIF
IF !USED('SCALE')
  =gfOpenFile(gcDataDir+'SCALE', gcDataDir+'SCALE','SH')
ENDIF
IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE', gcDataDir+'STYLE','SH')
ENDIF
IF !USED('CODES')
  =gfOpenFile(gcDataDir+'CODES', gcDataDir+'CODES','SH')
ENDIF
IF !USED('FABRIC')
  =gfOpenFile(gcDataDir+'FABRIC', gcDataDir+'FABRIC','SH')
ENDIF
IF !USED('SALESREP')
  =gfOpenFile(gcDataDir+'SALESREP', gcDataDir+'SALESREP','SH')
ENDIF
IF !USED('NOTEPAD')
  =gfOpenFile(gcDataDir+'NOTEPAD', gcDataDir+'NOTEPAD','SH')
ENDIF
IF !USED(lcTmpShip)
  =gfOpenFile(gcWorkDir+lcTmpShip, gcWorkDir+lcTmpShip,'SH')
ENDIF
IF !USED(lcMastFile)
  =gfOpenFile(gcWorkDir+lcMastFile, gcWorkDir+lcMastFile,'SH')
ENDIF
IF llRpOrdNot
*=gfOpenFile(gcWorkDir+lcMastFile, gcWorkDir+lcMastFile,'SH',"GETLAST")
  USE (gcWorkDir+lcMastFile) ORDER TAG (lcMastFile) IN 0 AGAIN ALIAS GETLAST
ENDIF  

SELECT (lcMastFile)
SET RELATION TO cOrdType + Order INTO OrdHdr      && To order header file.
SET RELATION TO style INTO Style ADDITIVE         && To style file.
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE   && To scale file.
SET RELATION TO cWareCode INTO WAREHOUS ADDITIVE  && To warehouse file.
SET RELATION TO cOrdType + Order + Store + Style+STR(LineNo,6) INTO (lcTmpShip) ADDITIVE

lcCustRel = IIF(llRpSummMt,['M' + Account],;
            [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.
IF llPrntBoth
  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
  SET SKIP TO &lcNoteLns
ENDIF   && end if you print both type of notes.
*--End of function lfOpenFilS.

*!*************************************************************
*! Name      : lfChkStrct
*! Developer : Nader Nabil (NNA)
*! Date      : 04/18/2005
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from        : SOACJERE.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen   = LEN(laItemSeg[lnCount,3])
    lnClrStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLen  = LEN(laItemSeg[lnCount,3])
    lnStyStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLen   = LEN(laItemSeg[lnCount,3])
    lnScaStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
