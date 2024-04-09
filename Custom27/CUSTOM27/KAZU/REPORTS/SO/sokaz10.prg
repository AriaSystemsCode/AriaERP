*:***************************************************************************
*: Program file  : SOKAZ10
*: Program desc. : Custom Order Detail Report (For Kazu)
*: For Report    : (SOKAZ10.FRX , SOKAZ10B.FRX)
*: Reference     : Order Detail Report
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : IHB
*:***************************************************************************
*: Calls : 
*:    Procedures : NONE
*:    Functions  : lfwRepWhen, lfScanData, lfvOptMsg, lfwOldVal, lfvSelcBy,
*:               : lfSelcObjs, lfvSortBy, lfPreObjs, lfObjState, lfvSumm,
*:               : lfSRVSty, lfStySum, lfSRVFab, lfFabSum, lfsrAcc, lfsrLoc,
*:               : lfsrRep, lfSROrder, lfCollTime, lfSumStyle, lfSumMulti,
*:               : lfGetNotes, lfLastRec, lfvCoorGrp, lfClearRep, lfEvalSegs,
*:               : lfNMajType, lfMakeExpr, lfGetRepVr, lfCreatCur, lfPipeExpr,
*:               : lfvOStatus, lfGetWork, lfWorkEnd, lfItmPos, lfvCurr
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOKAZ10
*:***************************************************************************
*: This Program is due to C101461
*:***************************************************************************
*-- Option Grid Layout :
*-- Optional Title             : ...
*-- Sort By                    : Account / Order / Style Major / Major Group
*--                              / Fabric / Location / Primary Sales Rep.
*--                              / Completeion Date / Vendor
*-- Order Status               :  ..
*-- Lines In Coordinates Group :  Y / N (Default N)
*-- Print Line Notes           :  Y / N (Default N)
*-- Print Notepad              :  Y / N (Default N)
*-- Select By                  : Account / Location / Sales Rep. 
*--                              / Major / Fabric / All 
*-- Order Number               : Range
*-- Start Date                 : From .. To ..
*-- Complete  Date             : From .. To ..
*-- Style Groups               : In List
*-- Season                     : In List
*-- Division                   : In List
*-- Priority                   : ..
*-- Note that : This report is taken from order detail report and i had fixed
*--             both report format is detail and format by size is 'N' . Plus adding new sort as demanded
*--             adding new sort criteria (by vendor).
*:***************************************************************************
*:Modifications :
*E802360,1 IHB   07/18/1999 Bugs in the report :
*E802360,1       (1) missing expression if sort by vendor is selected
*E802360,1       (2) remove empty line of color when select to sort
*E802360,1           by style major
*E802360,1       (3) grand total for shipped amount prints with 4 decimal
*E802360,1           (the format in the .frx is modified)
*E802360,1       (4) if one sales order has several c/t (or po) numbers
*E802360,1           each one applied for a different style then we must
*E802360,1           collect the exact c/t or po number from cutpick file
*C124058,1 BWA 01/16/2005 Add new sort to the report and new totals.
*T20061031.0018 TMI 11/09/2006 use the correct subscript number , also correcting the nvarpos field in syrepuvr
*:***************************************************************************
*-- Report Layout:
*--                                                                                      Company Name
*-- Time: 16:12                                                              Order  Detail  Report                                                             Date: 02/19/99
*--                                                                                                                                                                                  Page:  1
*-- Sorted By:  Account Number                         Completion: xx/xx/xx To xx/xx/xx    
*-- ----------------------------------------------------------------------------------------------------------------------------------------------------------
*-- Acct# Order#   Start        Complete SP  Style               Cust.PO    Vendor  C/T-PO#  …… SHIPPED …..   ………. OPEN .……
*-- ----------------------------------------------------------------------------------------------------------------------------------------------------------
*-- xxxxx xxxxxx xx/xx/xx  xx/xx/xx  xx xxxxxxxxxxxx xxxxxxxxx xxxxxx xxxxxx   xxxxxxx xxxxxxxxxx  xxxxxxx  xxxxxxxxxx   
*-- xxxxx xxxxxx xx/xx/xx  xx/xx/xx  xx xxxxxxxxxxxx xxxxxxxxx xxxxxx xxxxxx   xxxxxxx xxxxxxxxxx  xxxxxxx  xxxxxxxxxx   
*-- -----------------------------------------------------------------------------------------------------------------------------------------------------------
*-- ** Grand Total **           xx Accounts                                                                         xxxxxxx xxxxxxxxxx  xxxxxxx  xxxxxxxxxx            
*-- -----------------------------------------------------------------------------------------------------------------------------------------------------------
*:***************************************************************************
lcTime     = TIME()                     && Variable to hold the Time
lcStTime   = lcTime                     && Time in which we start collect data.
llPrntBoth = llRpOrdNot AND llRpOrdLnt  && True if you print both types of notes.
lnLastRec  = 2                          && Record No. Of last record in order group.
lcTitle    = ''                         && Title of Note. 
lcNotes    = ''                         && Notes.
llNoIndex  = .F.                        && I don't make index for file.

*C124058,1 BWA 01/16/2005 Create array hold the 2nd index.[START]
PRIVATE lcFrstSort , lcScndSort , lcHoldFrst , lcHoldScnd
STORE SPACE(0) TO lcFrstSort , lcHoldFrst , lcScndSort , lcHoldScnd

DO CASE
  CASE lcRpSortBy = 'A'
    lcFrstSort = "ACCOUNT"
    lcHoldFrst = [ACCOUNT]

  CASE lcRpSortBy = 'O'
    lcFrstSort = "Order"
    lcHoldFrst = [ORDER]

  CASE lcRpSortBy = 'S'
    lcFrstSort = "Style"
    lcHoldFrst = [SUBSTR(Style,1,lnMajorLen)]

  CASE lcRpSortBy = 'G'
    lcFrstSort = "Style Group"
    lcHoldFrst = [SUBSTR(cTempKey,8,6)]

  CASE lcRpSortBy = 'F'
    lcFrstSort = "Fabric"
    lcHoldFrst = [LEFT(cTempKey,7)]

  CASE lcRpSortBy = 'W'
    lcFrstSort = "Warehouse"
    lcHoldFrst = [CWARECODE]

  CASE lcRpSortBy = 'D'
    lcFrstSort = "Complete Date"
    lcHoldFrst = [COMPLETE]

  CASE lcRpSortBy = 'R'
    lcFrstSort = "Sales Rep"
    lcHoldFrst = [RIGHT(cTempKey,3)]

  CASE lcRpSortBy = 'V'
    lcFrstSort = "Vandor"
    lcHoldFrst = [ACCOUNT]

ENDCASE

DO CASE
  CASE lcRpSrt2By = 'A'
    lcScndSort = "ACCOUNT"
    lcHoldScnd = [ACCOUNT]

  CASE lcRpSrt2By = 'O'
    lcScndSort = "Order"
    lcHoldScnd = [ORDER]

  CASE lcRpSrt2By = 'S'
    lcScndSort = "Style"
    lcHoldScnd = [SUBSTR(Style,1,lnMajorLen)]

  CASE lcRpSrt2By = 'G'
    lcScndSort = "Style Group"
    lcHoldScnd = [SUBSTR(cTempKey,8,6)]

  CASE lcRpSrt2By = 'F'
    lcScndSort = "Fabric"
    lcHoldScnd = [LEFT(cTempKey,7)]

  CASE lcRpSrt2By = 'W'
    lcScndSort = "Warehouse"
    lcHoldScnd = [CWARECODE]

  CASE lcRpSrt2By = 'D'
    lcScndSort = "Complete Date"
    lcHoldScnd = [COMPLETE]

  CASE lcRpSrt2By = 'R'
    lcScndSort = "Sales Rep"
    lcHoldScnd = [RIGHT(cTempKey,3)]

  CASE lcRpSrt2By = 'V'
    lcScndSort = "Vandor"
    lcHoldScnd = [ACCOUNT]

ENDCASE

DIMENSION laIndxStrn[9, 2]
laIndxStrn[1,1]  = 'A'     
laIndxStrn[1,2]  = 'Account'
laIndxStrn[2,1]  = 'O'
laIndxStrn[2,2]  = 'CORDTYPE+ORDER'
laIndxStrn[3,1]  = 'S'
laIndxStrn[3,2]  = 'STYLE'
laIndxStrn[4,1]  = 'G'
laIndxStrn[4,2]  = 'SUBSTR(cTempKey,8,6)'
laIndxStrn[5,1]  = 'F'
laIndxStrn[5,2]  = 'LEFT(cTempKey,7)'
laIndxStrn[6,1]  = 'W'
laIndxStrn[6,2]  = 'CWARECODE'
laIndxStrn[7,1]  = 'R'
laIndxStrn[7,2]  = 'RIGHT(cTempKey,3)'
laIndxStrn[8,1]  = 'D'
laIndxStrn[8,2]  = 'DTOS(cMonthKy)'
laIndxStrn[9,1]  = 'V'
laIndxStrn[9,2]  = 'VENDOR'

*C124058,1 BWA 01/16/2005.[END]

DIMENSION laStock[9],laWip[9]
STORE '' TO laStock,laWip               && Work process arrays

STORE '' TO lcGrpExp,lcSeaExp,lcDivExp,lcStatusEx

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
  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]

  *-- cTempKey :  field used in most sort by case as the master key ,
  *--          :  and in case of summarize multi store as the total amount.
  laTempStru[ALEN(laTempStru,1)  ,1] = 'cTempKey'
  laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)  ,3] = 16
  laTempStru[ALEN(laTempStru,1)  ,4] = 0

  *C124058,1 BWA 01/16/2005 Add new field used in the sort with date.[START]
  =lfAddField("laTempStru", "cMonthKy" , "D", 8 , 0)       && SQ >> Month and year only to able to index with this field.
  *C124058,1 BWA 01/16/2005.[END]

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
  INDEX ON cRecord TAG (lcNoteLns)
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
*-- IHB initialize report expressions if changed -or not- the OG [start]
llOGFltCh = .T.
*-- IHB [end]

IF llClearFn OR llOGFltCh

  llClearFn = .F.

  lcStartSt = DTOS(CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],1,;
                  ATC('|',laOGFxFlt[lnStartPos,6])-1)))
  lcStartEd = DTOS(CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],;
                   ATC('|',laOGFxFlt[lnStartPos,6])+1)))

  lcCompSt  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],1,;
                   ATC('|',laOGFxFlt[lnCompPos,6])-1)))
  lcCompEd  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],;
                   ATC('|',laOGFxFlt[lnCompPos,6])+1)))

  
  lcStatusEx = [ORDHDR.STATUS $ lcRpStatus]
  
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

*-- Print size scale is .F.
llRpScale  = .F.

lcScaleGrp = IIF(llRpScale,[STYLE.SCALE],[''])  && group to print size scale.

lcRepExpr = [IIF(llPrntBoth,;
             IIF(&lcNoteLns..cRecord = 'N2',RECNO(lcMastFile) = lnLastRec ,.T.),;
             .T.)]    && Report expression.

*-- Select Master report file.
SELECT (lcMastFile)
GO TOP    && Refresh Relation

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMastFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' TIMEOUT 2


*-- Collecting for Kazu (Custom order detail)
*-- it is left the following :
*-- (1) create another workfile which has the needed stru. to be printed
*-- (2) insert into that file all previously collected records (if any)
*--     grouped by style major
*-- (3) we need in addition to that the following data :
*--    * customer PO# (in ordhdr)
*--    * vendor and PO# if the style is not domestic and there's style PO
*--             (one or more) for this style
*--    * contractor code and C/T# if the style is domestic and there's C/T
*--             (one or more) for this style
*--    * total shipped qty and amount for the style per order per
*--            all selected nonmajor per all sizes
*--    * total open qty and amount for the style per order per
*--            all selected nonmajor per all sizes
*--
*-- reference structure
*-- (ACCOUNT C(05), ORDER C(06), START D(08), COMPLETE D(10), STATUS C(01) ,;
*-- PRIORITY C(01), STYMAJOR C(lnMajorLen), CUSTPO   C(15), VENDOR C(08)   ,;
*-- CTPO C(06), SHIPQTY N(07), SHIPAMT  N(13,2) , OPENQTY N(07), OPENAMT N(13,2))
*-- in the cutpick file tag Cutord key trancd+ctktno+style
*-- there exist the PO# or C/T# referenced with order
*-- note that cutpick.transcd = 1 for CutTkt
*-- and cutpick.transcd = 2 for PO.
*-- so the relation will be depends on style.make
*-- if .T. then relation between (lcfile) and cutpick will be
*-- on '1'+order else will be on '2'+order

lcFile = gfTempName()   && assign temp alias name to the file which contains the collected data

*-- Select major needed data from the previously collected file and ORDHDR
SELECT a.STYLE, a.CORDTYPE, a.ACCOUNT, a.ORDER, a.START, a.COMPLETE, b.STATUS, b.PRIORITY ,;
  SUBSTR(a.Style,1,lnMajorLen) AS STYMAJOR, a.CUSTPO, SUM(a.TotQty) AS OPENQTY, a.cTempKey ,;
  SUM(a.TotQty*a.Price) AS OPENAMT, SPACE(08) AS VENDOR, SPACE(06) AS CTPO, a.CWARECODE ,;
  0000000 AS SHIPQTY, 0000000000.00 AS SHIPAMT, b.STORE, a.LINENO , a.cMonthKy;
  FROM &lcMastFile a, ORDHDR b ;
  INTO TABLE (gcWorkDir+lcFile) ;
  WHERE a.ORDER = b.ORDER ;
  GROUP BY a.ORDER, STYMAJOR

*-- Open INVLINE to collect ship qty and amount
IF !USED('INVLINE')
  =gfOpenFile(gcDataDir+'INVLINE','Invlineo','SH')  && key order+STR(lineno,6)+invoice
ENDIF
*-- Open CUTPICK to collect C/T # or PO#
IF !USED('CUTPICK')
  =gfOpenFile(gcDataDir+'CUTPICK','Cutord','SH')  && key trancd+order+cordline
ENDIF
*-- Open POSHDR to collect vendor in case of nondomestic style and PO
IF !USED('POSHDR')
  =gfOpenFile(gcDataDir+'POSHDR','Poshdr','SH')  && key cstytype+po
ENDIF
*-- Open MFGOPRHD to collect first contractor code in case of domestic style and C/T
IF !USED('MFGOPRHD')
  =gfOpenFile(gcDataDir+'MFGOPRHD','Mfgoprhd','SH')  && key cimtyp+ctktno+coprcode
ENDIF

SELECT INVLINE
SET ORDER TO TAG  Invlineo

SELECT CUTPICK
SET ORDER TO TAG Cutord

SELECT POSHDR
SET ORDER TO TAG Poshdr
SELECT MFGOPRHD
SET ORDER TO TAG Mfgoprhd
*-- Setting necessary relations
SELECT (lcFile)

SET RELATION TO order INTO Invline
SET RELATION TO cOrdType + Order INTO OrdHdr ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE

SET RELATION TO IIF(STYLE.MAKE = .F. ,'2'+ORDER,'1'+ORDER) INTO CUTPICK ADDITIVE

SET RELATION TO 'P'+CUTPICK.Ctktno INTO POSHDR ADDITIVE
SET RELATION TO 'M'+CUTPICK.Ctktno INTO MFGOPRHD ADDITIVE
lcCustRel = IIF(llRpSummMt,['M' + Account],;
            [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])
            
SET RELATION TO &lcCustRel INTO CUSTOMER ADDITIVE  && To customer file.
*-- lcORDER    : stores the value of the current order in the temp
*-- lnSHIP     : accumulates the ship qty per invoice if it is equal the ORDHDR.SHIP
*--              then no more ship qty/amount will be searched for in the INVLINE 
*-- laContr[3] : stores the needed contractor code 
*-- lnTShipQty, lnTOpenQty, lnTShipAmt, lnTOpenAmt : grand total ship and open
lcORDER = ORDER
STORE 0 TO lnSHIP
DIMENSION laContr[3]
STORE 0.00 TO lnTShipQty, lnTOpenQty, lnTShipAmt, lnTOpenAmt
*-- scans the temp file and calculate the remaining fields
SCAN
  lnTOpenQty = lnTOpenQty + OPENQTY
  lnTOpenAmt = lnTOpenAmt + OPENAMT 
  *-- if order is changed, reset lnship
  IF lcORDER <> ORDER  
    STORE 0 TO lnSHIP
  ENDIF  && endif order is changed
  *-- if there're records in the INVLINE, collect ship
  IF !EOF("INVLINE")
    STORE 0 TO lnSHIPQTY , lnSHIPAMT
    *-- if reached the ship qty in the ORDHDR
    IF lnSHIP <> ORDHDR.SHIP
      SELECT INVLINE
      *-- scan INVLINE through the same order
      SCAN FOR ORDER = &lcFile..ORDER
        *-- if stymajor is the same like temp
        IF &lcFile..STYMAJOR = SUBSTR(Style,1,lnMajorLen)
          lnSHIP     = lnSHIP    + TotQty 
          lnSHIPQTY  = lnSHIPQTY + TotQty
          lnSHIPAMT  = lnSHIPAMT + TotQty*Price
          lnTShipQty = lnTShipQty + TotQty
          lnTShipAmt = lnTShipAmt + TotQty*Price
        ENDIF  && endif stymajor is the same like temp
      ENDSCAN  && endscan INVLINE through the same order
    ENDIF  && endif reached the ship qty in the ORDHDR
    SELECT (lcFile)
    REPLACE SHIPQTY WITH lnSHIPQTY ,;
            SHIPAMT WITH lnSHIPAMT  
  ENDIF  && endif there're records in the INVLINE
  *-- C/T# or PO# in cutpick file
  *-- inserting the first transaction
  *-- if there're records in the CUTPICK , meaning there's /re C/T or PO
  IF !EOF("CUTPICK")
    SELECT (lcFile)
    
    *E802360,1 Bugs in the report [start]
    *-- we have to match the same style also cuz might be a different
    *-- style in the same order has another po # (or c/t)
    SELECT CUTPICK
    SCAN
      IF &lcFile..Style = Style
        SELECT (lcFile)
        REPLACE CTPO WITH CUTPICK.CTKTNO
        *-- finding vendor / contractor code
        IF CUTPICK.Trancd = '1' && cuttkt
          IF !EMPTY("MFGOPRHD")
            lnCurAlias = SELECT(0)
            SELECT MFGOPRHD
            STORE SPACE(08) TO laContr
            lnInd  = 1
            SCAN WHILE CIMTYP+CTKTNO = 'M'+CUTPICK.cTktNo FOR !lInHouse
              laContr[lnInd] = MFGOPRHD.CCONTCODE
              EXIT         && only the first contractor code is needed
            ENDSCAN
            SELECT (lnCurAlias)
            REPLACE VENDOR WITH laContr[1]
          ENDIF
        ELSE  && style po
          IF !EMPTY("POSHDR")
            REPLACE VENDOR WITH POSHDR.VENDOR
          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
    
    *REPLACE CTPO WITH CUTPICK.CTKTNO
    **-- finding vendor / contractor code
    *IF CUTPICK.Trancd = '1' && cuttkt
    *  IF !EMPTY("MFGOPRHD")
    *    lnCurAlias = SELECT(0)
    *    SELECT MFGOPRHD
    *    STORE SPACE(08) TO laContr
    *    lnInd  = 1
    *    SCAN WHILE CIMTYP+CTKTNO = 'M'+CUTPICK.cTktNo FOR !lInHouse
    *       laContr[lnInd] = MFGOPRHD.CCONTCODE
    *       EXIT         && only the first contractor code is needed
    *    ENDSCAN
    *    SELECT (lnCurAlias)
    *    REPLACE VENDOR WITH laContr[1]
    *  ENDIF
    *ELSE  && style po
    *  IF !EMPTY("POSHDR")
    *    REPLACE VENDOR WITH POSHDR.VENDOR
    *  ENDIF
    *ENDIF
    *E802360,1 Bugs in the report [end]
    
  ENDIF  && endif there're records in the CUTPICK
  lcORDER = ORDER
ENDSCAN  && end scan temp file

*-- Different sort by cases.
DO CASE
  CASE lcRpSortBy = 'A'   && Sort by account 
    lcIndexTg = 'ACCOUNT+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'

  CASE lcRpSortBy = 'O'    && Sort by order
    lcIndexTg = 'CORDTYPE+ORDER+STR(LINENO,6)+STYLE'

  CASE lcRpSortBy = 'S'      && Sort by style
    lcIndexTg = 'STYMAJOR+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)'

  CASE lcRpSortBy = 'G'    && Sort by style group
    lcIndexTg = 'SUBSTR(cTempKey,8,6)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'

  CASE lcRpSortBy = 'F'    && Sort by fabric
    lcIndexTg = 'LEFT(cTempKey,7)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'

  CASE lcRpSortBy = 'W'    && Sort by location
    lcIndexTg = 'CWARECODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'

  CASE lcRpSortBy = 'R'    && Sort by sales representative
    lcIndexTg = 'RIGHT(cTempKey,3)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'

  CASE lcRpSortBy = 'D'    && Sort by complete date
    *C124058,1 BWA 01/16/2005 Sort with the new field.[START]
    *lcIndexTg = 'DTOS(COMPLETE)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
    lcIndexTg = 'DTOS(cMonthKy)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
    *C124058,1 BWA 01/16/2005.[END]

    *--IHB modifying title in case of sort by completion date [start]
*    lcCompSt  = SUBSTR(laOGFxFlt[lnCompPos,6],1,10)
*    lcCompEd  = SUBSTR(laOGFxFlt[lnCompPos,6],12,10)
    
*    lcDateTtl = 'Completion:'
*    DO CASE
*      CASE EMPTY(lcCompSt) AND !EMPTY(lcCompEd)
*        lcDateTtl = lcDateTtl+' To ' + lcCompEd
*      CASE EMPTY(lcCompEd) AND !EMPTY(lcCompSt)
*        lcDateTtl = lcDateTtl+lcCompSt
*      CASE EMPTY(lcCompEd) AND EMPTY(lcCompSt)
*        lcDateTtl = ''
*      CASE !EMPTY(lcCompEd) AND !EMPTY(lcCompSt)
*        lcDateTtl = lcDateTtl+lcCompSt + ' To ' + lcCompEd
*    ENDCASE
*    lcSubTitle = 'Complete Date' + SPACE(10) + lcDateTtl
*    *--IHB [end]

  CASE lcRpSortBy = 'V'   && Sort by vandor
    lcIndexTg = 'VENDOR+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'

ENDCASE          && end Different sort by cases.

*C124058,1 BWA 01/16/2005 Rearrange the sorts.[START]
STORE 0 TO lnFrstPls
STORE SPACE(0) TO lcSort1Prt , lcSort2Prt
lnFrstPls = ATC('+' , lcIndexTg) - 1
lcSort1Prt = SUBSTR(lcIndexTg , 1 , lnFrstPls)
lcSort2Prt = laIndxStrn[ASUBSCRIPT(laIndxStrn,ASCAN(laIndxStrn , lcRpSrt2By),1) , 2]

IF lcRpSortBy == 'D' AND lcRpSrt2By == "G"
  lcIndexTg = lcSort1Prt + "+" + lcSort2Prt + "+STYLE+CORDTYPE+ORDER+STR(LINENO,6)"
ELSE
  lcIndexTg = lcSort1Prt + "+" + lcSort2Prt
ENDIF
*C124058,1 BWA 01/16/2005.[END]

*--IHB modifying title in case of sort by completion date [start]
lcCompSt  = SUBSTR(laOGFxFlt[lnCompPos,6],1,10)
lcCompEd  = SUBSTR(laOGFxFlt[lnCompPos,6],12,10)
lcDateTtl = 'Completion Date : '
DO CASE
  CASE EMPTY(lcCompSt) AND !EMPTY(lcCompEd)
    lcDateTtl = lcDateTtl+' To ' + lcCompEd
  CASE EMPTY(lcCompEd) AND !EMPTY(lcCompSt)
    lcDateTtl = lcDateTtl+lcCompSt
  CASE EMPTY(lcCompEd) AND EMPTY(lcCompSt)
    lcDateTtl = ''
  CASE !EMPTY(lcCompEd) AND !EMPTY(lcCompSt)
    lcDateTtl = lcDateTtl+lcCompSt + ' To ' + lcCompEd
ENDCASE
lcSubTitle = lcSubTitle + SPACE(15) + lcDateTtl
*--IHB [end]

*-- IHB Close unnecessary files [start]
IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF
IF USED('INVLINE')
  USE IN INVLINE
ENDIF
IF USED('POSHDR')
  USE IN POSHDR
ENDIF
IF USED('POSHDR')
  USE IN POSHDR
ENDIF
IF USED('MFGOPRHD')
  USE IN MFGOPRHD
ENDIF

SELECT (lcFile)
*-- Sort temp file according to d\selected sort type
INDEX ON &lcIndexTg TAG (lcWorkFile)

*-- For Kazu
*-- if you print order notepad, open master file in another alias to 
*-- help us to know what last line in order group to print notepad
*-- after it, note we do this because we print notepad in detail band
*-- not in order group band .
IF llRpOrdNot AND !USED('GTLAST')
  USE (gcWorkDir+lcFile) ORDER TAG (lcWorkFile) IN 0 AGAIN ALIAS GTLAST
ENDIF  && end if you print order notepad.

*C124058,1 BWA 01/16/2005 Add new sort to the report and new totals.[START]
SELECT (lcFile)
IF lcRpSortBy == 'D' AND lcRpSrt2By == "G"
  SELECT (lcFile)
  =AFIELDS(laFileStru)
  =lfAddField("laFileStru", "TotMonSQ" , "N", 7 , 2)       && SQ >> Shipped quantity.
  =lfAddField("laFileStru", "TotMonSV" , "N", 7 , 2)       && SV >> Shipped Value.
  =lfAddField("laFileStru", "TotMonOQ" , "N", 7 , 2)       && OQ >> Open quantity.
  =lfAddField("laFileStru", "TotMonOV" , "N", 7 , 2)       && OV >> Open Value.
  
  =lfAddField("laFileStru", "StyGrpSQ" , "N", 7 , 2)       && SQ >> Shipped quantity.
  =lfAddField("laFileStru", "StyGrpSV" , "N", 7 , 2)       && SV >> Shipped Value.
  =lfAddField("laFileStru", "StyGrpOQ" , "N", 7 , 2)       && OQ >> Open quantity.
  =lfAddField("laFileStru", "StyGrpOV" , "N", 7 , 2)       && OV >> Open Value.
  
  =lfAddField("laFileStru", "llPrnDat" , "L", 1 , 0)
  =lfAddField("laFileStru", "llPrnGrp" , "L", 1 , 0)
  
  CREATE TABLE (gcWorkDir + lcTotFile) FROM ARRAY laFileStru
  INDEX ON &lcIndexTg TAG TotFile OF (gcWorkDir + lcTotFile)
  SELECT (lcFile)
  SET RELATION TO &lcIndexTg INTO (lcTotFile) ADDITIVE

  PRIVATE lcMonDat , lcStyGrop
  STORE SPACE(0) TO lcMonDat , lcStyGrop
  STORE 0 TO lnTotMonSQ , lnTotMonSV , lnTotMonOQ , lnTotMonOV
  STORE 0 TO lnStyGrpSQ , lnStyGrpSV , lnStyGrpOQ , lnStyGrpOV

  SCAN
    SCATTER MEMVAR MEMO
    SELECT (lcTotFile)
    INSERT INTO (lcTotFile) FROM MEMVAR

    lcMonYear = ALLTRIM(STR(MONTH(Complete))) + ALLTRIM(STR(YEAR(Complete)))
    IF (lcMonDat # lcMonYear) AND !EMPTY(lcMonDat)
      STORE 0 TO lnTotMonSQ , lnTotMonSV , lnTotMonOQ , lnTotMonOV
      STORE 0 TO lnStyGrpSQ , lnStyGrpSV , lnStyGrpOQ , lnStyGrpOV
      REPLACE &lcTotFile..TotMonSQ WITH &lcFile..SHIPQTY ,;
              &lcTotFile..TotMonSV WITH &lcFile..SHIPAMT ,;
              &lcTotFile..TotMonOQ WITH &lcFile..OPENQTY ,;
              &lcTotFile..TotMonOV WITH &lcFile..OPENAMT

      REPLACE &lcTotFile..StyGrpSQ WITH &lcFile..SHIPQTY ,;
              &lcTotFile..StyGrpSV WITH &lcFile..SHIPAMT ,;
              &lcTotFile..StyGrpOQ WITH &lcFile..OPENQTY ,;
              &lcTotFile..StyGrpOV WITH &lcFile..OPENAMT

      SKIP -1
      REPLACE &lcTotFile..llPrnDat WITH .T. ,;
              &lcTotFile..llPrnGrp WITH .T.
      SKIP
    ELSE
      IF (lcStyGrop # STYLE.CSTYGROUP) AND !EMPTY(lcStyGrop)
        STORE 0 TO lnStyGrpSQ , lnStyGrpSV , lnStyGrpOQ , lnStyGrpOV
        REPLACE &lcTotFile..StyGrpSQ WITH &lcFile..SHIPQTY ,;
                &lcTotFile..StyGrpSV WITH &lcFile..SHIPAMT ,;
                &lcTotFile..StyGrpOQ WITH &lcFile..OPENQTY ,;
                &lcTotFile..StyGrpOV WITH &lcFile..OPENAMT
        SKIP -1
        REPLACE &lcTotFile..llPrnGrp WITH .T.
        SKIP

      ELSE
        REPLACE &lcTotFile..StyGrpSQ WITH lnStyGrpSQ + &lcFile..SHIPQTY ,;
                &lcTotFile..StyGrpSV WITH lnStyGrpSV + &lcFile..SHIPAMT ,;
                &lcTotFile..StyGrpOQ WITH lnStyGrpOQ + &lcFile..OPENQTY ,;
                &lcTotFile..StyGrpOV WITH lnStyGrpOV + &lcFile..OPENAMT
      ENDIF

      REPLACE &lcTotFile..TotMonSQ WITH lnTotMonSQ + &lcFile..SHIPQTY ,;
              &lcTotFile..TotMonSV WITH lnTotMonSV + &lcFile..SHIPAMT ,;
              &lcTotFile..TotMonOQ WITH lnTotMonOQ + &lcFile..OPENQTY ,;
              &lcTotFile..TotMonOV WITH lnTotMonOV + &lcFile..OPENAMT
    ENDIF

    lnStyGrpSQ = &lcTotFile..StyGrpSQ
    lnStyGrpSV = &lcTotFile..StyGrpSV
    lnStyGrpOQ = &lcTotFile..StyGrpOQ
    lnStyGrpOV = &lcTotFile..StyGrpOV

    lnTotMonSQ = &lcTotFile..TotMonSQ
    lnTotMonSV = &lcTotFile..TotMonSV
    lnTotMonOQ = &lcTotFile..TotMonOQ
    lnTotMonOV = &lcTotFile..TotMonOV

    lcStyGrop = STYLE.CSTYGROUP
    lcMonDat  = lcMonYear
  ENDSCAN
  SELECT (lcTotFile)
  GOTO BOTTOM
  REPLACE &lcTotFile..llPrnDat WITH .T. ,;
          &lcTotFile..llPrnGrp WITH .T.

ENDIF
*C124058,1 BWA 01/16/2005.[END]

*C124058,1 BWA 01/16/2005 Recreate the table of the FRX that will be used.[START]
IF lcRpSortBy == 'D' AND lcRpSrt2By == "G"
  lcRpForm = 'SOKAZ10'
ELSE
  lcRpForm = 'SOKAZ10B'
ENDIF
=gfCrtFrm(lcRpForm,lcOGFormArr,llOGRefForm)
=lfRepPltFr(lcRpForm)
*C124058,1 BWA 01/16/2005.[END]

SELECT (lcFile)
LOCATE
DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRepExpr

WAIT CLEAR

*C124058,1 BWA 01/16/2005 Function to delete the temp. file used in the program.[START]
=lfBasToClr(lcTotFile , 'F')
*C124058,1 BWA 01/16/2005.[END]

*-- If Sort by Sales Rep. , set relation to Primary sales rep. file.
IF lcRpSortBy = 'R'
  PRIVATE lcCurSel
  lcCurSel = ALIAS()
  SELECT ORDHDR
  SET RELATION OFF INTO SALESREP
  SELECT (lcCurSel)
ENDIF

SELECT (lcFile)
SET RELATION TO
USE

IF llRpOrdNot
  USE IN GETLAST
  *-- For Kazu
  USE IN GTLAST
ENDIF  

SET STATUS BAR &lcStatusBr
RETURN
*-- end of Report Code.
*-- Function section 
*-------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : IHB
*! Date      : 04/20/1999
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

  DECLARE laRpSource[3],laRpTarget[2]
  STORE 'Open'     TO laRpSource[1],laRpTarget[1]
  STORE 'Hold'     TO laRpSource[2],laRpTarget[2]
  STORE 'Canceled' TO laRpSource[3]
  lcRpStatus = 'OH'

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

  lnClrSgPos = lfItmPos('SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)')
  *-- Convert all ceiling functions to use lfItmPos because [End..
  lnFreSgPos = lnClrSgPos + 1

  *B602590,1 Adjust currency symbol [Begin]
  IF llMultCurr
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
    lnCurrPos  = lfItmPos('ORDHDR.CCURRCODE')
    
    IF lnOGSeting = 1
      laOGFxFlt[lnCurrPos,6] = gcBaseCurr
      = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
    ENDIF  
  ENDIF
  *-- Compute Start of variable filter to control its apperance [begin]
  *-- in option grid.
  
  *-- Calculate length of variables appears in option grid.
  *-- and items that we enable and disable.
  *-- Compute Start of variable filter to control its apperance [end]

  *-- laRpFltVal : Array to hold D for disable and E for enable, to control
  *--            : INLIST appearance in option grid.
  *-- laRpVarNow : Array to hold .T. or .F., to control variables 
  *--            : appearance in option grid. 
    *-- Disable objects that disabled before. [begin]
    *-- Disable objects that disabled before. [begin]
    
    *-- ceiling is not a good tech.
    *-- Convert all ceiling functions to use lfItmPos because [End..
ENDIF  && END IF you first time enter when function.

*-- Disable/enable By account, style, fabric, location, sales representative. [begin]
*-- note that disable and enable is according to value of laRpFltVal.
STORE .T. TO llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfScanData
*! Developer : IHB
*! Date      : 04/20/1999
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
  SELECT (lcWorkFile)
  ZAP
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

lnOrdTrueP = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ORDER'),1)
lcTOrdFile = laFxFltCur[ASUBSCRIPT(laFxFltCur,ASCAN(laFxFltCur,lnOrdTrueP),1),2]  && Name of order cursor (inlist func.).

*-- if you find order cursor. 
IF !EMPTY(lcTOrdFile) AND USED(lcTOrdFile)
  SELECT (lcTOrdFile)
  COUNT TO lnHaveRecs FOR !DELETED()
  llWorkDeal = (lnHaveRecs > 0)  && .T. is there is data.
ENDIF

*-- If user select specific orders, collect data of this orders only. [begin]
IF llWorkDeal
  SELECT (lcTOrdFile)
  *-- Scan order cursor.
  SCAN  
    SELECT ORDLINE
    SET ORDER TO TAG ORDLINE
    *-- if find first order record in ordline file and <ordhdr filter>
    IF SEEK('O'+&lcTOrdFile..ORDER) AND;
       EVALUATE(lcStatusEx) AND ;
       IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
       IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
       IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
       IIF(llMultCurr,ORDHDR.CCURRCODE=laOGFxFlt[lnCurrPos,6],.T.)

      *-- Scan ordline file for rest order data.
      SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+&lcTOrdFile..Order
        
        *-- if <ordline filter> and <style group filter> and <Color Filter>
        *-- insert this data into workfile.
        IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
           IIF(EMPTY(lcStartSt),.T.,BETWEEN(DTOS(START),lcStartSt,lcStartEd)) AND ;
           IIF(EMPTY(lcCompSt),.T.,BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
           IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
           IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))

          SCATTER MEMVAR MEMO
          m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)

          *C124058,1 BWA 01/16/2005 Change the day to "01" to help in indexing.[START]
          SS = LEFT(DTOS(COMPLETE),6) + STRTRAN(SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , "01")
          m.cMonthKy = CTOD(SUBSTR(SS,5,2) + "/" +  SUBSTR(SS,7,2) + "/" + LEFT(SS,4))
          *C124058,1 BWA 01/16/2005.[END]

          *-- IHB adding custpo from ordhdr file
          m.CustPO = ORDHDR.CustPO
          *
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
lnUsedItem = CEILING(ASCAN(laFxFltCur,lnUsedItem)/3)
*-- Know which type of select we use and its position [end]

lcSlctFile = IIF(lnUsedItem = 0,'',laFxFltCur[lnUsedItem,2])  &&Name of selected cursor (inlist func.).

*-- if you find selected cursor. 
IF !EMPTY(lcSlctFile) AND USED(lcSlctFile)
  SELECT (lcSlctFile)
  COUNT TO lnHaveRecs FOR !DELETED()
  llLineDeal = (lnHaveRecs > 0)
ENDIF

llRpStyLoc = (lcRpSelcBy = 'W') AND llLineDeal

*-- If User select data by any select case, beside selecting orders. 
IF RECCOUNT(lcWorkFile) > 0 AND llLineDeal
  lcSlctKey = IIF(lcRpSelcBy = 'A',"ACCOUNT"   ,;
              IIF(lcRpSelcBy = 'S',"ALLTRIM(CSTYMAJOR)"     ,;
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
        IF llRpSummMt
          lcSeekExp = Style + DTOS(Complete) + cordtype + order
          *-- if you do not find this (style + order) in line file, add record for it.
          IF !SEEK(lcSeekExp,lcTempLine)
            SCATTER MEMVAR MEMO
            *-- IHB adding custpo from ordhdr file
            m.CustPO = ORDHDR.CustPO
            *
            = lfSumStyle(lcWorkFile,lcSeekExp)  && sum for this style.
            INSERT INTO (lcTempLine) FROM MEMVAR
          ENDIF
        
        ELSE  && normal case, add line to temp. line file.
          SCATTER MEMVAR MEMO
          *-- IHB adding custpo from ordhdr file
          m.CustPO = ORDHDR.CustPO
          *
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
              IF CORDTYPE = 'O' AND ;
                 EVALUATE(lcStatusEx) AND ;
                 IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
                 IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(llMultCurr,CCURRCODE=laOGFxFlt[lnCurrPos,6],.T.) AND ;
                 SEEK('O'+ORDER,'ORDLINE')
                 
                SELECT ORDLINE 
                *-- scan ordline for rest order lines.
                SCAN REST WHILE cordtype+order+STR(lineno,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
                  *-- if <ordline filter> and <style group filter> and <Color Filter>
                  IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                     IIF(EMPTY(lcStartSt),.T.,BETWEEN(DTOS(START),lcStartSt,lcStartEd)) AND ;
                     IIF(EMPTY(lcCompSt),.T.,BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                     IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
                     IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))
                  
                    SCATTER MEMVAR MEMO
                    *-- IHB adding custpo from ordhdr file
                    m.CustPO = ORDHDR.CustPO
                    *
                    m.cTempKey = PADR(STYLE.FABRIC,7) + ;
                                 PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)

                    *C124058,1 BWA 01/16/2005 Change the day to "01" to help in indexing.[START]
                    SS = LEFT(DTOS(COMPLETE),6) + STRTRAN(SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , "01")
                    m.cMonthKy = CTOD(SUBSTR(SS,5,2) + "/" +  SUBSTR(SS,7,2) + "/" + LEFT(SS,4))
                    *C124058,1 BWA 01/16/2005.[END]

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
          IF SEEK(ALLTRIM(CSTYMAJOR),'ORDLINE') AND ;
             IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))
             
            SELECT ORDLINE
            *-- scan ordline for the rest of this style.
            SCAN REST WHILE STYLE+DTOS(complete)+cordtype+order+store+STR(lineno,6) = ALLTRIM(&lcSlctFile..CSTYMAJOR)

              *-- if <ordhdr filter> and <ordline filter> and <Color Filter>
              IF CORDTYPE = 'O' AND TotQty > 0 AND ;
                 EVALUATE(lcStatusEx) AND ;
                 IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 IIF(EMPTY(lcStartSt),.T.,BETWEEN(DTOS(START),lcStartSt,lcStartEd)) AND ;
                 IIF(EMPTY(lcCompSt),.T.,BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
                 IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(llMultCurr,ORDHDR.CCURRCODE=laOGFxFlt[lnCurrPos,6],.T.) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
                
                SCATTER MEMVAR MEMO
                *-- IHB adding custpo from ordhdr file
                m.CustPO = ORDHDR.CustPO
                *
                m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)

                *C124058,1 BWA 01/16/2005 Change the day to "01" to help in indexing.[START]
                SS = LEFT(DTOS(COMPLETE),6) + STRTRAN(SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , "01")
                m.cMonthKy = CTOD(SUBSTR(SS,5,2) + "/" +  SUBSTR(SS,7,2) + "/" + LEFT(SS,4))
                *C124058,1 BWA 01/16/2005.[END]

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
               IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ; 
               SEEK(STYLE.STYLE,'ORDLINE')
               
              SELECT ORDLINE 
              *-- scan ordline for the rest of this style.
              SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = ;
                              STYLE.STYLE
                *-- if <ordhdr filter> and <ordline filter>
                IF CORDTYPE = 'O' AND TotQty > 0 AND ;
                   EVALUATE(lcStatusEx) AND ;
                   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                   IIF(EMPTY(lcStartSt),.T.,BETWEEN(DTOS(START),lcStartSt,lcStartEd)) AND ;
                   IIF(EMPTY(lcCompSt),.T.,BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
                   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
                   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                   IIF(llMultCurr,ORDHDR.CCURRCODE=laOGFxFlt[lnCurrPos,6],.T.)

                  SCATTER MEMVAR MEMO
                  *-- IHB adding custpo from ordhdr file
                  m.CustPO = ORDHDR.CustPO
                  *
                  m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)

                  *C124058,1 BWA 01/16/2005 Change the day to "01" to help in indexing.[START]
                  SS = LEFT(DTOS(COMPLETE),6) + STRTRAN(SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , "01")
                  m.cMonthKy = CTOD(SUBSTR(SS,5,2) + "/" +  SUBSTR(SS,7,2) + "/" + LEFT(SS,4))
                  *C124058,1 BWA 01/16/2005.[END]

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
          SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
                        CORDTYPE = 'O' AND  ;
                        EVALUATE(lcStatusEx) AND ;
                        IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
                        IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
                        IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr,ORDHDR.CCURRCODE=laOGFxFlt[lnCurrPos,6],.T.)
                        
          *-- scan ordhdr for this Location.
          SCAN
            = SEEK('O'+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
            SELECT ORDLINE
            *-- Scan ordline file to get lines of this order
            *-- that evaluate critria.
            SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O' + ORDHDR.ORDER

              *-- if <ordline filter> and <style group filter> and <Color Filter>
              IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 IIF(EMPTY(lcStartSt),.T.,BETWEEN(DTOS(START),lcStartSt,lcStartEd)) AND ;
                 IIF(EMPTY(lcCompSt),.T.,BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
              
                SCATTER MEMVAR MEMO
                *-- IHB adding custpo from ordhdr file
                m.CustPO = ORDHDR.CustPO
                *
                m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)

                *C124058,1 BWA 01/16/2005 Change the day to "01" to help in indexing.[START]
                SS = LEFT(DTOS(COMPLETE),6) + STRTRAN(SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , "01")
                m.cMonthKy = CTOD(SUBSTR(SS,5,2) + "/" +  SUBSTR(SS,7,2) + "/" + LEFT(SS,4))
                *C124058,1 BWA 01/16/2005.[END]

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

          *-- filter to repcode and <ordhdr filter>
          SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
                        CORDTYPE = 'O' AND ;
                        EVALUATE(lcStatusEx) AND ;
                        IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
                        IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
                        IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr,ORDHDR.CCURRCODE=laOGFxFlt[lnCurrPos,6],.T.)
                        
          *-- scan ordhdr for this sales rep.
          SCAN
            = SEEK('O'+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
            SELECT ORDLINE
            *-- Scan ordline file to get lines of this order
            *-- that evaluate critria.
            SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O' + ORDHDR.ORDER

              *-- if <ordline filter> and <style group filter> and <Color Filter>
              IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 IIF(EMPTY(lcStartSt),.T.,BETWEEN(DTOS(START),lcStartSt,lcStartEd)) AND ;
                 IIF(EMPTY(lcCompSt),.T.,BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
              
                SCATTER MEMVAR MEMO
                *-- IHB adding custpo from ordhdr file
                m.CustPO = ORDHDR.CustPO
                *
                m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)

                *C124058,1 BWA 01/16/2005 Change the day to "01" to help in indexing.[START]
                SS = LEFT(DTOS(COMPLETE),6) + STRTRAN(SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , "01")
                m.cMonthKy = CTOD(SUBSTR(SS,5,2) + "/" +  SUBSTR(SS,7,2) + "/" + LEFT(SS,4))
                *C124058,1 BWA 01/16/2005.[END]

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
             IIF(EMPTY(lcStartSt),.T.,BETWEEN(DTOS(START),lcStartSt,lcStartEd)) AND ;
             IIF(EMPTY(lcCompSt),.T.,BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
             IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
             IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
             IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
             IIF(llMultCurr,ORDHDR.CCURRCODE=laOGFxFlt[lnCurrPos,6],.T.) AND ;
             IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
             IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
  
            SCATTER MEMVAR MEMO
            *-- IHB adding custpo from ordhdr file
            m.CustPO = ORDHDR.CustPO
            *
            = lfSumMulti(lcSeekExp)  && summarize data.

            *C124058,1 BWA 01/16/2005 Change the day to "01" to help in indexing.[START]
            SS = LEFT(DTOS(COMPLETE),6) + STRTRAN(SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , "01")
            m.cMonthKy = CTOD(SUBSTR(SS,5,2) + "/" +  SUBSTR(SS,7,2) + "/" + LEFT(SS,4))
            *C124058,1 BWA 01/16/2005.[END]

            INSERT INTO (lcTempLine) FROM MEMVAR
  
          ENDIF    && end if you does not find line in temp line file,
        ENDSCAN    && end scan file for full index expression (rushmore).
        USE IN SUMMULTI

      ELSE  && Normal collection case for all data in ordline file.
        *-- scan ordline file for full index expression (rushmore)
        SCAN FOR CORDTYPE + ORDER + STR(LINENO,6) = 'O'

          *-- if <ordhdr filter>, <ordline filter> and <style group filter> 
          *-- and <Color Filter>.
          IF TotQty > 0 AND ;
             EVALUATE(lcStatusEx) AND ;
             IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
             IIF(EMPTY(lcStartSt),.T.,BETWEEN(DTOS(START),lcStartSt,lcStartEd)) AND ;
             IIF(EMPTY(lcCompSt),.T.,BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
             IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
             IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
             IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
             IIF(llMultCurr,ORDHDR.CCURRCODE=laOGFxFlt[lnCurrPos,6],.T.) AND ;
             IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
             IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
      
            SCATTER MEMVAR MEMO
            *-- IHB adding custpo from ordhdr file
            m.CustPO = ORDHDR.CustPO
            *
            m.cTempKey = PADR(STYLE.FABRIC,7)+PADR(STYLE.CSTYGROUP,6)+PADR(ORDHDR.REP1,3)

            *C124058,1 BWA 01/16/2005 Change the day to "01" to help in indexing.[START]
            SS = LEFT(DTOS(COMPLETE),6) + STRTRAN(SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , SUBSTR(DTOS(COMPLETE) , LEN(DTOS(COMPLETE)) - 1) , "01")
            m.cMonthKy = CTOD(SUBSTR(SS,5,2) + "/" +  SUBSTR(SS,7,2) + "/" + LEFT(SS,4))
            *C124058,1 BWA 01/16/2005.[END]

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

*-- if user select by orders only, and want to summarize data.
IF (RECCOUNT(lcTempLine) = 0 AND RECCOUNT(lcWorkFile) > 0) AND llRpSummMt
  SELECT(lcWorkFile)
  SCAN
    lcSeekExp = Style + DTOS(Complete) + cordtype + order
    IF !SEEK(lcSeekExp,lcTempLine)
      SCATTER MEMVAR MEMO
      *-- IHB adding custpo from ordhdr file
      m.CustPO = ORDHDR.CustPO
      *
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
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
llChSelect = .T.
llClearAcc = (lcRpSelcBy # 'A')
llClearSty = (lcRpSelcBy # 'S')
llClearFab = (lcRpSelcBy # 'F')
llClearLoc = (lcRpSelcBy # 'L')
llClearRep = (lcRpSelcBy # 'R')
CLEAR READ
*-- end of lfvSelect.

*!*************************************************************
*! Name      : lfSelcObjs
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
*-- IHB adding new sort type "VENDOR" [start]
*laOldVal = IIF(laOldVal = 1 , 'A' , IIF(laOldVal = 2 , 'O',;
           IIF(laOldVal = 3 , 'S' , IIF(laOldVal = 4 , 'G',;
           IIF(laOldVal = 5 , 'F' , IIF(laOldVal = 6 , 'W',;
           IIF(laOldVal = 7 , 'R' , 'D')))))))
laOldVal = IIF(laOldVal = 1 , 'A' , IIF(laOldVal = 2 , 'O',;
           IIF(laOldVal = 3 , 'S' , IIF(laOldVal = 4 , 'G',;
           IIF(laOldVal = 5 , 'F' , IIF(laOldVal = 6 , 'W',;
           IIF(laOldVal = 7 , 'R' , IIF(laOldVal = 8 , 'D','V'))))))))
*-- IHB [end]

*C124058,1 BWA 01/16/2005 Function adds the values of the 2nd sort.[START]
=lfSort2()
*C124058,1 BWA 01/16/2005.[END]

*-- print size scale is .F. 
llRpScale  = .F.
llRpSummMt = IIF(lcRpSortBy <> 'S' ,.F.,llRpSummMt)
lcRpStyPrn = IIF(lcRpSortBy <> 'S' ,'N',lcRpStyPrn)
llRpOrdNot = IIF(!(lcRpSortBy $ 'AO'),.F.,llRpOrdNot)
CLEAR READ
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

      *-- Enable/disable variable objects due to sort case. [begin]
      *-- if report kind is detail
      IF lcRpKind = 'D'
        IF laOldVal = 'S'
          llChSumm = IIF(llRpSummMt,.T.,llChSumm)  && Rise summarize flag.
        ENDIF
      ENDIF
      *-- Enable/disable variable objects due to sort case. [end]

    CASE lcRpSortBy = 'O'    && Sort by order

      *-- Enable/disable variable objects due to sort case. [begin]
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
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Name      : lfvSumm
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Name      : lfsrvSty
*! Developer : IHB
*! Date      : 04/20/1999
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
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : IHB
*! Date      : 04/20/1999
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
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
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
*! Name      : lfSRVFab
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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

    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
    GO TOP
  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.

ENDCASE
*-- end of lfsChOrder.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Name        : lfGetNotes
*! Developer   : IHB
*! Date        : 05/27/1998
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : SORDDETA.FRX, OR SORDDETB.FRX [Variable lcDum in the report]
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
*-- For Kazu, only the order notes is to be printed.
*-- so both the flags : llPrntBoth and llRpOrdLNt will be .F.
IF llPrntBoth
  *-- Note that the following Scheme
  *-- ....... cRecord = 'N1' ............. Line Notepad.
  *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
  DO CASE 
    CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcMastFile..Note_Mem))
      lcTitle = 'Order : ' + ORDER + ' - Style : ' + STYLE + ' - Line #  ' + ALLTRIM(STR(LINENO)) + '    Notes.'
      lcNotes  =  ALLTRIM(Note_Mem) + CHR(10) + CHR(13)
    
    CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcTitle = IIF(RECNO(lcMastFile) = lnLastRec,;
                'Order :' + ORDER + ' Notepad.','')
      lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10) + CHR(13),'')
  ENDCASE
ELSE && Else You print either Line or Order/contract Notepad.
  *-- Note that the following Scheme
  *-- ....... llRoOrdLnt ............. Line Notepad.
  *-- ....... llRoOrdNot ............. Order or Contract Notepad.
  DO CASE
    CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcMastFile..Note_Mem))
      lcTitle = 'Order : ' + ORDER + ' - Style : ' + STYLE + ' - Line #  ' + ALLTRIM(STR(LINENO)) + '    Notes.'
      lcNotes  =  ALLTRIM(Note_Mem) + CHR(10) + CHR(13)
    *-- For Kazu [start] 
    *CASE llRpOrdNot AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
    *  lcTitle = IIF(RECNO(lcMastFile) = lnLastRec,;
    *            'Order :' + ORDER + ' Notepad.','')
    *  lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
    *             IIF(lcRpSortBy = 'A',CHR(10) + CHR(13),'')
    CASE llRpOrdNot AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcTitle = IIF(RECNO(lcFile) = lnLastRec,;
                'Order :' + ORDER + ' Notepad.','')
      *-- IHB removing enter key to avoid if multiple order lines' spacing
      *lcNotes  = IIF(RECNO(lcFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10) + CHR(13),'')
      lcNotes  = IIF(RECNO(lcFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10) ,'')
    *-- For Kazu [end] 

  ENDCASE
ENDIF
RETURN ''
*-- end of lfGetNotes.

*!*************************************************************
*! Name      : lfLastRec
*! Developer : IHB
*! Date      : 04/20/1999
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
*-- For Kazu
*IF llRpOrdNot
*  PRIVATE lcThAlias
*  lcThAlias = ALIAS()           && Save Current Alias.
*  SELECT GETLAST
*  *-- lcLocExpr : Expression we locate for in the file
*  *--           : and we put full index expression in start
*  *--           : of it to make it rushmore as possibile.
*  lcLocExpr = lcIndexTg + '>' + IIF(lcRpSortBy = 'A',;
*              [&lcMastFile..ACCOUNT+'O'+&lcMastFile..ORDER],;
*              ['O'+&lcMastFile..ORDER])
*  LOCATE REST FOR &lcLocExpr
*  IF EOF() OR ORDER <> &lcMastFile..ORDER
*    SKIP -1
*  ENDIF

*  lnLastRec = RECNO('GETLAST')  && Evaluate record Number of last record in detail lines.
*  SELECT (lcThAlias)             && Restore Alias.
*ENDIF

IF llRpOrdNot
  PRIVATE lcThAlias
  lcThAlias = ALIAS()           && Save Current Alias.
  SELECT GTLAST
  *-- lcLocExpr : Expression we locate for in the file
  *--           : and we put full index expression in start
  *--           : of it to make it rushmore as possibile.
  lcLocExpr = lcIndexTg + '>' + IIF(lcRpSortBy = 'A',;
              [&lcFile..ACCOUNT+'O'+&lcFile..ORDER],;
              ['O'+&lcFile..ORDER])
  LOCATE REST FOR &lcLocExpr
  IF EOF() OR ORDER <> &lcFile..ORDER
    SKIP -1
  ENDIF

  lnLastRec = RECNO('GTLAST')  && Evaluate record Number of last record in detail lines.
  SELECT (lcThAlias)             && Restore Alias.
ENDIF

RETURN ''
*-- end of lfLastRec.

*!*************************************************************
*! Name      : lfvCoorGrp
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
  SET CURRENCY TO lcCurrSymb
  SET HOURS TO &lcSetHour
ENDIF  && end if user change setting [enter report <Preview> or <Run>].
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : IHB
*! Date      : 04/20/1999
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
*-- Substituting values to report format by size and print scale
*-- for custom order detail (kazu)

lcRpForm = 'SOKAZ10B'
=lfRepPltFr(lcRpForm)
llRpScale = .F.
RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfNMajType
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
IF lcRpSortBy = 'S'
  lcOutHeadL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen),19) + '  : ']
  lcOutHeadR = [Style.cStyMajor]
  
  *E802360,1 Bugs in the report [start]
  *-- the following removes unwanted nonmajor line which
  *-- is printed when select to sort by style major
  *lcInnHeadL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19)+ '  : ']
  STORE [''] TO lcInnHeadL
  *E802360,1 Bugs in the report [end]
  
  lcInnHeadR = [SUBSTR(Style,lnMajorLen + 2) + '  ----  ' + ALLTRIM(STYLE.Desc1)]
ELSE
  STORE [''] TO lcOutHeadL,lcOutHeadR,lcInnHeadL,lcInnHeadR
ENDIF

*-- Different sort by cases.
DO CASE
  CASE lcRpSortBy = 'A'   && Sort by account 
    lcIndexTg = 'ACCOUNT+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'

    *-- report variables data account case [begin]
    lcSubTitle = 'Account Number'  && Report sort title
    *lcInnGrp   = [ORDER]           && Inner .FRX group field.
    lcOutGrp   = [ACCOUNT]         && Outer .FRX group field.
    lcInnFootL = ['Order # ' + ORDER + '   Cust PO #']
    lcInnFootR = [IIF(Ordhdr.MultiPo,'*Multi PO*',ALLTRIM(OrdHdr.CustPo)) + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]  && Right title of inner group footer.
    lcOutFootL = ['Account # ' + Customer.Account]  && Left title of outer group footer.
    lcOutFootR = [ALLTRIM(CUSTOMER.BtName) + ' -- ' + CUSTOMER.Phone1]  && Right title of outer group footer.
    *-- report variables data account case [end]

  CASE lcRpSortBy = 'O'    && Sort by order
    lcIndexTg = 'CORDTYPE+ORDER+STR(LINENO,6)+STYLE'

    *-- report variables data order case [begin]
    lcSubTitle = 'Order'
    *lcInnGrp   = ['']
    lcOutGrp   = [ORDER]
    lcInnFootL = [''] 
    lcInnFootR = ['']
    lcOutFootL = ['Order # ' + Order + '    Account # ']
    lcOutFootR = [Account  + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]
    *-- report variables data order case [end]

  CASE lcRpSortBy = 'S'      && Sort by style
    lcIndexTg = 'STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)'

    *-- report variables data style case [begin]
    lcStyTitle = IIF ('GFITEM' $ ALLTRIM(UPPER(lcStyTitle)),;
                      EVALUATE(lcStyTitle),lcStyTitle)

    lcSubTitle = lcStyTitle
    *lcInnGrp   = [SUBSTR(Style,lnMajorLen + 2)]
    lcOutGrp   = [SUBSTR(Style,1,lnMajorLen)]
    lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19) + '  : ']
    lcInnFootR = [SUBSTR(Style,lnMajorLen + 2)]
    lcOutFootL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen),19) + '  : ']
    lcOutFootR = [ALLTRIM(Style.cStyMajor) + '( ' + ALLTRIM(Style.Fabric) + ' )']
    *-- report variables data style case [end]

  CASE lcRpSortBy = 'G'    && Sort by style group
    lcIndexTg = 'SUBSTR(cTempKey,8,6)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'

    *-- report variables data Style group case [begin]
    lcSubTitle = 'Style Group'
    *lcInnGrp   = [STYLE]
    lcOutGrp   = [SUBSTR(cTempKey,8,6)]
    lcInnFootL = ['Style  ( ' + Style + ' )  :']
    lcInnFootR = [ALLTRIM(Style.Desc)]
    lcOutFootL = ['Group  ( ' + SUBSTR(cTempKey,8,6) + ' )  :']
    lcOutFootR = [Codes.cDiscrep]
    *-- report variables data Style group case [end]

  CASE lcRpSortBy = 'F'    && Sort by fabric
    lcIndexTg = 'LEFT(cTempKey,7)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'

    *-- report variables data fabric case [begin]
    lcSubTitle = 'Primary Fabric'
    *lcInnGrp   = [STYLE]
    lcOutGrp   = [LEFT(cTempKey,7)]
    lcInnFootL = ['Style  ( ' + Style + ' )  :']
    lcInnFootR = [ALLTRIM(Style.Desc)]
    lcOutFootL = ['Fabric  ( ' + LEFT(cTempKey,7) + ' )  :']
    lcOutFootR = [ALLTRIM(Fabric.Desc)]
    *-- report variables data fabric case [end]

  CASE lcRpSortBy = 'W'    && Sort by location
    lcIndexTg = 'CWARECODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'

    *-- report variables data location case [begin]
    lcSubTitle = 'Location'
    *lcInnGrp   = ['']
    lcOutGrp   = [CWARECODE]
    lcInnFootL = [''] 
    lcInnFootR = ['']
    lcOutFootL = ['Location # ' + cWareCode + ' :']
    lcOutFootR = [ALLTRIM(Warehous.cDesc)]
    *-- report variables data location case [end]

  CASE lcRpSortBy = 'R'    && Sort by sales representative
    lcIndexTg = 'RIGHT(cTempKey,3)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'

    *-- report variables data sales Rep. case [begin]
    lcSubTitle = 'Primary Sales Representative'
    *lcInnGrp   = ['']
    lcOutGrp   = [RIGHT(cTempKey,3)]
    lcInnFootL = [''] 
    lcInnFootR = ['']
    lcOutFootL = ['Primary Sales Rep. # ' + RIGHT(cTempKey,3) + ' :']
    lcOutFootR = [SalesRep.Name]
    *-- report variables data sales Rep. case [end]

  CASE lcRpSortBy = 'D'    && Sort by complete date
    *C124058,1 BWA 01/16/2005 Sort with the new field.[START]
    *lcIndexTg = 'DTOS(COMPLETE)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
    lcIndexTg = 'DTOS(cMonthKy)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
    *C124058,1 BWA 01/16/2005.[END]
    
    *-- report variables data Complete date case [begin]
    lcSubTitle = 'Complete Date'
    *lcInnGrp   = ['']
    lcOutGrp   = [DTOS(COMPLETE)]
    lcInnFootL = [''] 
    lcInnFootR = ['']
    lcOutFootL = ['******']
    lcOutFootR = [Complete]
    *-- report variables data Complete date case [end]
    
  *IHB adding new sort type "VENDOR"
  CASE lcRpSortBy = 'V'    && Sort by vendor
      lcIndexTg = 'ACCOUNT+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
      *lcIndexTg = 'VENDOR+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
    
    *E802360,1 Bugs in the report [start]
    *-- the following corrects missing expression error
    *-- when select to sort by vendor
    lcSubTitle = 'Account Number'  && Report sort title
    *lcInnGrp   = [ORDER]           && Inner .FRX group field.
    lcOutGrp   = [ACCOUNT]         && Outer .FRX group field.
    lcInnFootL = ['Order # ' + ORDER + '   Cust PO #']
    lcInnFootR = [IIF(Ordhdr.MultiPo,'*Multi PO*',ALLTRIM(OrdHdr.CustPo)) + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]  && Right title of inner group footer.
    lcOutFootL = ['Account # ' + Customer.Account]  && Left title of outer group footer.
    lcOutFootR = [ALLTRIM(CUSTOMER.BtName) + ' -- ' + CUSTOMER.Phone1]  && Right title of outer group footer.
    *E802360,1 Bugs in the report [end]

ENDCASE          && end Different sort by cases.

*C124058,1 BWA 01/16/2005 Assign the "lcInnGrp" it's new variable.[START]
DO CASE
  CASE lcRpSrt2By = 'A'
    lcInnGrp = [ACCOUNT]

  CASE lcRpSrt2By = 'O'
    lcInnGrp = [ORDER]

  CASE lcRpSrt2By = 'S'
    lcInnGrp = [SUBSTR(Style,1,lnMajorLen)]

  CASE lcRpSrt2By = 'G'
    lcInnGrp = [SUBSTR(cTempKey,8,6)]

  CASE lcRpSrt2By = 'F'
    lcInnGrp = [LEFT(cTempKey,7)]

  CASE lcRpSrt2By = 'W'
    lcInnGrp = [CWARECODE]

  CASE lcRpSrt2By = 'D'
    lcInnGrp = [DTOS(COMPLETE)]

  CASE lcRpSrt2By = 'R'
    lcInnGrp = [RIGHT(cTempKey,3)]

  CASE lcRpSrt2By = 'V'
    lcInnGrp = [ACCOUNT]

ENDCASE
*C124058,1 BWA 01/16/2005.[END]

*--End of lfGetRepVr.
*!*************************************************************
*! Name      : lfCreatCur
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
FOR lnI = 1 TO ALEN(laRpTarget,1)
  lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                            IIF(laRpTarget[lnI] = 'Hold','H',;
                            IIF(laRpTarget[lnI] = 'Canceled','X','')))

ENDFOR  && end Loop to make Status expression.

lcRpStatus = IIF(EMPTY(lcRpStatus),'OHX',ALLTRIM(lcRpStatus))
*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]
*-- end of lfvOStatus.

*!*************************************************************
*! Name      : lfGetWork
*! Developer : IHB
*! Date      : 04/20/1999
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
    laStock[1] = IIF(lnS1 = 0 ,'',TRANSFORM(lnS1,'999999')) 
    laStock[2] = IIF(lnS2 = 0 ,'',TRANSFORM(lnS2,'999999')) 
    laStock[3] = IIF(lnS3 = 0 ,'',TRANSFORM(lnS3,'999999')) 
    laStock[4] = IIF(lnS4 = 0 ,'',TRANSFORM(lnS4,'999999')) 
    laStock[5] = IIF(lnS5 = 0 ,'',TRANSFORM(lnS5,'999999')) 
    laStock[6] = IIF(lnS6 = 0 ,'',TRANSFORM(lnS6,'999999')) 
    laStock[7] = IIF(lnS7 = 0 ,'',TRANSFORM(lnS7,'999999')) 
    laStock[8] = IIF(lnS8 = 0 ,'',TRANSFORM(lnS8,'999999')) 
    laStock[9] = IIF(lnS9 = 0 ,'',TRANSFORM(lnS9,'999999')) 
  ENDIF

  *-- if you print wip or (stock and wip) and total wip not equal 0
  IF INLIST(lcRpStyPrn,'W','P') AND (lnW9 # 0)
    laWip[1] = IIF(lnW1 = 0 ,'',TRANSFORM(lnW1,'999999')) 
    laWip[2] = IIF(lnW2 = 0 ,'',TRANSFORM(lnW2,'999999')) 
    laWip[3] = IIF(lnW3 = 0 ,'',TRANSFORM(lnW3,'999999')) 
    laWip[4] = IIF(lnW4 = 0 ,'',TRANSFORM(lnW4,'999999')) 
    laWip[5] = IIF(lnW5 = 0 ,'',TRANSFORM(lnW5,'999999')) 
    laWip[6] = IIF(lnW6 = 0 ,'',TRANSFORM(lnW6,'999999')) 
    laWip[7] = IIF(lnW7 = 0 ,'',TRANSFORM(lnW7,'999999')) 
    laWip[8] = IIF(lnW8 = 0 ,'',TRANSFORM(lnW8,'999999')) 
    laWip[9] = IIF(lnW9 = 0 ,'',TRANSFORM(lnW9,'999999')) 
  ENDIF

  lnStkOrWip = TRANSFORM(lnS9 + lnW9,'999999')  && Calculate wip + stock values
  lnStkOrWip = IIF(VAL(lnStkOrWip) = 0 , '' , lnStkOrWip)

ELSE  && User does not select specific locations.

  IF INLIST(lcRpStyPrn,'S','P')
    laStock[1] = IIF(STYLE.Stk1 = 0,'',TRANSFORM(STYLE.Stk1,'999999'))
    laStock[2] = IIF(STYLE.Stk2 = 0,'',TRANSFORM(STYLE.Stk2,'999999'))
    laStock[3] = IIF(STYLE.Stk3 = 0,'',TRANSFORM(STYLE.Stk3,'999999'))
    laStock[4] = IIF(STYLE.Stk4 = 0,'',TRANSFORM(STYLE.Stk4,'999999'))
    laStock[5] = IIF(STYLE.Stk5 = 0,'',TRANSFORM(STYLE.Stk5,'999999'))
    laStock[6] = IIF(STYLE.Stk6 = 0,'',TRANSFORM(STYLE.Stk6,'999999'))
    laStock[7] = IIF(STYLE.Stk7 = 0,'',TRANSFORM(STYLE.Stk7,'999999'))
    laStock[8] = IIF(STYLE.Stk8 = 0,'',TRANSFORM(STYLE.Stk8,'999999'))
    laStock[9] = IIF(STYLE.TotStk = 0,'',TRANSFORM(STYLE.TotStk,'999999'))
  ENDIF

  IF INLIST(lcRpStyPrn,'W','P')
    laWip[1] = IIF(STYLE.Wip1 = 0,'',TRANSFORM(STYLE.Wip1,'999999'))
    laWip[2] = IIF(STYLE.Wip2 = 0,'',TRANSFORM(STYLE.Wip2,'999999'))
    laWip[3] = IIF(STYLE.Wip3 = 0,'',TRANSFORM(STYLE.Wip3,'999999'))
    laWip[4] = IIF(STYLE.Wip4 = 0,'',TRANSFORM(STYLE.Wip4,'999999'))
    laWip[5] = IIF(STYLE.Wip5 = 0,'',TRANSFORM(STYLE.Wip5,'999999'))
    laWip[6] = IIF(STYLE.Wip6 = 0,'',TRANSFORM(STYLE.Wip6,'999999'))
    laWip[7] = IIF(STYLE.Wip7 = 0,'',TRANSFORM(STYLE.Wip7,'999999'))
    laWip[8] = IIF(STYLE.Wip8 = 0,'',TRANSFORM(STYLE.Wip8,'999999'))
    laWip[9] = IIF(STYLE.TotWip = 0,'',TRANSFORM(STYLE.TotWip,'999999'))
  ENDIF

  *-- Calculate Wip and Stock Values [End]

  lnStkOrWip = TRANSFORM(STYLE.TotStk + STYLE.TotWip,'999999')  && Calculate wip + stock values
  lnStkOrWip = IIF(VAL(lnStkOrWip) = 0 , '' , lnStkOrWip)

ENDIF  && end If User select specific locations .

RETURN ''
*-- end of lfGetWork.

*!*************************************************************
*! Name      : lfWorkEnd
*! Developer : IHB
*! Date      : 04/20/1999
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


*-- Function to fill select by and sort by arrays.
FUNCTION lfArrDumy
lnSubSt = IIF('MA' $ gcCmpModules,0,1)
lnSubSt = IIF(llMultLoc,lnSubSt,lnSubSt+1)

*--IHB adding element to the sort types "VENDOR" [start]
*DIMENSION laSortDesc[8-lnSubSt,1],laSortVal[8-lnSubSt,1]
DIMENSION laSortDesc[9-lnSubSt,1],laSortVal[9-lnSubSt,1]
*--IHB [end]

laSortDesc[1,1] = 'Account'
laSortDesc[2,1] = 'Order'
laSortDesc[3,1] = lcStyMajor
laSortDesc[4,1] = lcStyMajor + ' Group'
laSortDesc[5,1] = 'Primary Sales Representative'
laSortDesc[6,1] = 'Complete Date'
*--IHB adding element to the sort types "VENDOR" [start]
*laSortDesc[9,1] = 'Vendor'
*--IHB [end]

laSortVal[1,1]  = 'A'
laSortVal[2,1]  = 'O'
laSortVal[3,1]  = 'S'
laSortVal[4,1]  = 'G'
laSortVal[5,1]  = 'R'
laSortVal[6,1]  = 'D'
*--IHB adding element to the sort types "VENDOR" [start]
*laSortVal[9,1]  = 'V'
*--IHB [end]

DIMENSION laSlctDesc[6-lnSubSt,1],laSlctVal[6-lnSubSt,1]
*-- Fill Select by array.
laSlctDesc[1,1] = 'Account'
laSlctDesc[2,1] = 'Primary Sales Representative'
laSlctDesc[3,1] = lcStyMajor
laSlctDesc[4,1] = 'All'

laSlctVal[1,1]  = 'A'
laSlctVal[2,1]  = 'R'
laSlctVal[3,1]  = 'S'
laSlctVal[4,1]  = 'L'

IF llMultLoc
  =AINS(laSortDesc,5,1)
  =AINS(laSortVal,5,1)
  =AINS(laSlctDesc,2,1)
  =AINS(laSlctVal,2,1)
  STORE 'Location' TO laSortDesc[5,1],laSlctDesc[2,1]
  STORE 'W' TO laSortVal[5,1],laSlctVal[2,1]
ENDIF

IF 'MA' $ gcCmpModules
  =AINS(laSortDesc,5,1)
  =AINS(laSortVal,5,1)

  lnInsFabIn = ASCAN(laSlctDesc,'All',1)
  =AINS(laSlctDesc,lnInsFabIn,1)
  =AINS(laSlctVal,lnInsFabIn,1)
  STORE 'Fabric' TO laSortDesc[5,1],laSlctDesc[lnInsFabIn,1]
  STORE 'F' TO laSortVal[5,1],laSlctVal[lnInsFabIn,1]
ENDIF

*--IHB adding element to the sort types "VENDOR" [start]
*T20061031.0018 TMI [Start] use the correct subscript number
*laSortDesc[9,1] = 'Vendor'
*laSortVal[9,1]  = 'V'
laSortDesc[9-lnSubSt,1] = 'Vendor'
laSortVal[9-lnSubSt,1]  = 'V'
*T20061031.0018 TMI [End  ] 
*--IHB [end]

*C124058,1 BWA 01/16/2005 Rearrange the 2nd sort due to the select of the first 1.[START]
DIMENSION laSort2Des[ALEN(laSortDesc,1) -1 , 1]
DIMENSION laSort2Val[ALEN(laSortVal,1)  -1 , 1]

lcRpSortBy = IIF( TYPE("lcRpSortBy") = "U" , "A" , lcRpSortBy)
llentered = .T.
FOR lnI = 1 TO ALEN(laSortDesc,1) - 1
  IF lcRpSortBy = laSortVal[lnI,1]
    lnA = lnI + 1
    llentered = .F.
  ELSE
    IF llentered
      lnA = lnI
    ELSE
      lnA = lnI + 1
    ENDIF
  ENDIF
  laSort2Des[lnI,1] = laSortDesc[lnA,1]
  laSort2Val[lnI,1] = laSortVal[lnA,1]
ENDFOR
lcRpSrt2By = laSort2Val[1,1]
lcCharVal = laSort2Val[1,1]
*C124058,1 BWA 01/16/2005.[END]

*--End of lfArrDumy.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : IHB
*! Date      : 04/20/1999
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
*! Developer : IHB
*! Date      : 04/20/1999
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
FUNCTION lfvCurr   
PRIVATE lnCurrPos
lnCurrPos = EVALUATE(SYS(18))
SET CURRENCY TO ALLTRIM(laCurrSmbl[lnCurrPos,1])
*-- end of lfvCurr.

*C124058,1 BWA 01/16/2005 The new functions used in the progrom.[START]
*!*************************************************************
*! Name      : lfSort2
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 01/13/2005
*! Purpose   : Function add elements to the 2nd sort.
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
*! Example   : =lfSort2()
*!*************************************************************
FUNCTION lfSort2

lcRpSortBy = IIF( TYPE("lcRpSortBy") = "U" , "A" , lcRpSortBy)
llentered = .T.
FOR lnI = 1 TO ALEN(laSortDesc,1) - 1
  IF lcRpSortBy = laSortVal[lnI,1]
    lnA = lnI + 1
    llentered = .F.
  ELSE
    IF llentered
      lnA = lnI
    ELSE
      lnA = lnI + 1
    ENDIF
  ENDIF
  laSort2Des[lnI,1] = laSortDesc[lnA,1]
  laSort2Val[lnI,1] = laSortVal[lnA,1]
ENDFOR
lcRpSrt2By = laSort2Val[1,1]
lcCharVal = laSort2Val[1,1]

*--End of lfSort2.
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 01/13/2005
*! Purpose   : Add fields to the array of file structer.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : None
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
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 01/13/2005
*! Purpose   : Deleting temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.
*C124058,1 BWA 01/16/2005.[END]