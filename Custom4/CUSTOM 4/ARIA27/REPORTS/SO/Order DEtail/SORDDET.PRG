************************************************************************
*: Program file  : SORDDET.PRG
*: Program desc. : Order Detail Report
*: For Report    : (SORDDETA.FRX,SORDDETB.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mohamed Atia Badran (MAB)
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
*: Example : DO Sorddet
*:***************************************************************************
*: This Program is due to E300846 ...
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
*:B802748,1 RAMY 12/16/1999 Adjust the grand total field 
*							Note : that was done in (SORDDETA.FRX , SORDDETB.FRX) only
*:E301421,1 ABD 05/25/2000 Add Feature to allow printing the Bid and Complete status,
*:E301421,1 ABD            And in case of Complete status we will print Booked Qty.
*:C101890,1 ABD 06/08/2000 Add feature to the report to print new column Represent 
*:C101890,1 ABD            The store In case of sort by style.
*E301401,1 ABD 06/07/2000 For EDI Temporary Orders, change price columns to labelled
*E301401,1 ABD            "Transmitted Price" and Amount to labelled "Wholesale Price," 
*E301401,1 ABD            Both on-screen and in outputs. [Modify in FRX]
*B603567,1 ABD 07/02/2000 Fix Bug that When you select sort by style group ,change 
*B603567,1 ABD            "Format by size " to No then return it to Yes , 
*B603567,1 ABD            When you preview you will find the size description duplicated.
*B603567,1 ABD            Fix In FRX.
*B603870,1 RAMY 08/04/2000 1- Consider the case of printing the completed orders
*                          2- Fix the bug of saving the filter
*:E500342,1 SSE 09/10/2000 Add 2 Options in Selection Grid 
*:E500342,1                1) Print Partially Shipped Orders (Y/N)
*:E500342,1                2) Include Allocated Order Lines (Y/N)
*:B803639,1 SSE 09/10/2000 Fix Bug of having a funny character in the Diff field when selecting
*:B803639,1                Format By Size = No (Format B) , I've included the PRG for documentaion only.
*:B803674,1 SSE 09/19/2000 Fix Bug of applying Partially Shipped Orders on Orders lines also
*:B603946,1 SSE 10/04/2000 Fix bug of wrong position of options in Order Status button when selecting
*:B603946,1                Some options and pressing RESET and then selecting options again from Order Status.
*:B803742,1 ADEL 10/18/2000 Fix the bug of printing lines with TOTQTY = 0
*:B803834,1 MHM 11/13/2000 Fix the bug of report has no filter for Order Status , we add Status $ lcRpStatus 
*:B803834,1                 in SYREPUVR in the Order# record in field mbrwfltexp .
*:C102262,1 ADEL 06/10/2001 Add a new option for EDI orders.
*:B604559,1 ADEL 06/18/2001 Fix the bug of not printing the sizes for the first line in evry order.
*:B604559,1                 Modifications are done in both Prg and Frx. 
*:E301621,1 SSE 07/01/2001 Enh. Include the Web Order option in Report option grid and in collecting data.
*:B604748,1 SSE 08/27/2001 Fix bug 'Syntax error' when you select Primary Sales Rep and select any valid Primary rep code.
*:B604681,1 SSE 08/28/2001 Fix bug if you select "All" in "Print Orders" option , the Order# list will not allow locate feature.
*:B604681,1                I've included the Prg for documentation only. (Modifications in Syrepuvr)
*:B604687,1 SSE 08/28/2001 
*:B604687,1 SSE 08/28/2001 Fix bug of displaying "No records to display" message when you select to
*:B604687,1                Print "EDI Temp Orders" , from Select By option select "Account" and select any Account Code.
*:E500444,1 HBG 03/19/2002 In case of print the report by Account Add new option to OG to allow
*:E500444,1                the user to filter on particular styles.
*:B606069,1 RAE 06/23/2002 Print the modificated EDI temporary orders.
*:C102722,1 ADEL 10/09/02  Give the user the ability to filter by 'Stye season' or 'Order header season'
*:B606575,1 BWA 11/12/2002 Fix bug of the report prints wrong qty in case Summarize Multi Store = "YES"
*:B606653,1 ASH 11/13/2002 Print wrong data in case of filter by Order Header season.
*:B606304,1 BWA 01/29/2003 Fix the bug of WIP field get wrong data.
*:B607020,1 ABD 03/25/2003 Fix problem that the text format not printing the scale in case we print 
*:B607020,1 ABD            2 styles or more with the same sizes, fix the Dos detail FRX.
*:******************************************************************************************************

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

lcSeekVal  = ''                                   && Get Last record seek value.
llTextMode = (UPPER(ALLTRIM(lcRepMode))=="TEXT")  && Print Text Format

*C101569,1 Add sort by store if first sort is by order [Begin]
lcStorCond = ''
*C101569,1 Add sort by store if first sort is by order [End]

*E301265,1==E301272,1 Define variables used in Print dos format and multi currency format [End

*Sameh (SSE) Adding a variable to detect the changes of SCALE in FRX 
*so as not to print it except when it changes
lcOldScale = SPACE(3)
*B604559,1 (Begin) Declare a variable for the first scale.
llFirstSc = .F.
*B604559,1 (End)

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

  *C102722,1 (Begin) Give the user the ability to filter by 'Stye season' or 'Order header season'
  IF !EMPTY(lcSeaExp)
    IF lcRpSeaFlt = 'S' 
      lcSeaExp  = STRTRAN(lcSeaExp,'ORDHDR.SEASON','STYLE.SEASON')
    ELSE
      lcSeaExp  = STRTRAN(lcSeaExp,'STYLE.SEASON','ORDHDR.SEASON')
    ENDIF
  ENDIF  
  *C102722,1 (End)

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
*B803742,1 (Begin) filter lines for TOTQTY <>0
*B606069,1 RAE Stop filtering on TOTQTY <> 0 only [Start]
*SET FILTER TO TOTQTY <> 0
SET FILTER TO TOTQTY <> 0 .OR. OrdHdr.cOrdType = 'T'
*B606069,1 RAE [End]
*B803742,1 (End)

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

  *-- MAB 05/02/99 Work sheet bug By default all status was selected.. [Begin]
  *DECLARE laRpSource[3],laRpTarget[2]
  *STORE 'Open'     TO laRpSource[1],laRpTarget[1]
  *STORE 'Hold'     TO laRpSource[2],laRpTarget[2]
  *STORE 'Canceled' TO laRpSource[3]
  *lcRpStatus = 'OH'

  *E301421,1 ABD Add Feature to allow printing the Bid and Complete status. [Begin]
  *DECLARE laRpSource[3],laRpTarget[1]
  *STORE 'Open'     TO laRpSource[1]
  *STORE 'Hold'     TO laRpSource[2]
  *STORE 'Canceled' TO laRpSource[3]
  *lcRpStatus = 'OHX'
  
  *--B603870,1 RAMY check if the first element of the array holds character string, 
  *--               then check the rest otherwise declare the array [start]
  *DECLARE laRpSource[5],laRpTarget[1]
  DECLARE laRpSource[5]
  
  IF TYPE('laRpTarget[1]') = 'C'
    *B603946,1 Check if empty of target array [Begin]
    *FOR lnI = 2 TO 5
    *  IF TYPE('laRpTarget[lnI]') = 'L'
    *    laRpTarget[lnI] = ""
    *  ENDIF
    *ENDFOR
    
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
  *E301421,1 ABD [End]


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
    
    *E301272,1 No default currency [Begin
    * IF lnOGSeting = 1 AND EMPTY(laOGFxFlt[lnCurrPos,6])
    *   laOGFxFlt[lnCurrPos,6] = gcBaseCurr
    *   = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
    * ENDIF 
    *E301272,1 No default currency [End
    
  ENDIF
  *B602590,1 Adjust currency symbol [End  ]

  *B602562,1 PRIVATE lcThAlias
  *B602562,1 lcThAlias = ALIAS()

  *-- Compute Start of variable filter to control its apperance [begin]
  *-- in option grid.
  
  *B602562,1 SELECT SYREPUVR
  *-- Calculate length of variables appears in option grid.
  *-- and items that we enable and disable.
  *B602562,1 = SEEK('SORDDET V')
  *B602562,1 SCAN REST WHILE cRep_ID+cExpType+STR(nVarPos) = 'SORDDET V' FOR nVarPos != 0
    *B602562,1 lnVarbEnd = lnVarbEnd + 1
    *B602562,1 DO CASE
      *B602562,1 CASE UPPER(ALLTRIM(MFLD_NAME)) = 'LLRPSCALE'
        *B602562,1 lnScale = nVarPos
      *B602562,1 CASE UPPER(ALLTRIM(MFLD_NAME)) = 'LLRPSUMMMT'
        *B602562,1 lnSumm = nVarPos
      *B602562,1 CASE UPPER(ALLTRIM(MFLD_NAME)) = 'LCRPSTYPRN'
        *B602562,1 lnWork = nVarPos
      *B602562,1 CASE UPPER(ALLTRIM(MFLD_NAME)) = 'LLRPORDLNT'
        *B602562,1 lnLineNote = nVarPos
      *B602562,1 CASE UPPER(ALLTRIM(MFLD_NAME)) = 'LLRPORDNOT'
        *B602562,1 lnNotePad = nVarPos
    *B602562,1 ENDCASE 
  *B602562,1 ENDSCAN
  *-- Compute Start of variable filter to control its apperance [end]

  *-- laRpFltVal : Array to hold D for disable and E for enable, to control
  *--            : INLIST appearance in option grid.
  *-- laRpVarNow : Array to hold .T. or .F., to control variables 
  *--            : appearance in option grid. 
  *B602562,1 IF EMPTY(laRpFltVal)
    *B602562,1 DECLARE laRpFltVal[ALEN(laOGFxFlt,1)],laRpVarNow[lnVarbEnd],laRpSource[3],laRpTarget[2]
    *B602562,1 STORE 'D' TO laRpFltVal  
    *B602562,1 STORE .T. TO laRpVarNow
  

    *B602562,1 Suppress this line instead of enable/disable
    *IF The Add notes to order lines seting is set to No
    *B602562,1 llOrdLnDis = (gfGetMemVar('M_OR_NOTE') <> 'Y')

    *B602562,1 IF llOrdLnDis
    *B602562,1  = lfObjState('D',lnLineNote,'llRpOrdLnt')        && Disable Line notes line.
    *B602562,1 ENDIF  

    *B602562,1 = lfObjState('D',lnSumm,'llRpSummMt')          && Disable Summarization line.

    *B602562,1 = lfObjState('D',lnWork,'lcRpStyPrn','N')      && Disable Working processing line.

  *B602562,1 ELSE
    *-- Disable objects that disabled before. [begin]
    *B602562,1 FOR lnJ = 1 TO lnVarbEnd
    *B602562,1   IF !laRpVarNow[lnJ] AND SEEK('SORDDET V'+ STR(lnJ),'SYREPUVR')
    *B602562,1       lcVar = ALLTRIM(mFld_Name)
    *B602562,1       lcVal = &lcVar
    *B602562,1       = lfObjState('D',lnJ,lcVar,lcVal)    && Again disable this line.
    *B602562,1   ENDIF
    *B602562,1 ENDFOR
    *-- Disable objects that disabled before. [begin]
    
    *B602562,1 Convert all ceiling functions to use lfItmPos because [Begin
    *-- ceiling is not a good tech.
    *lnAccPos   = CEILING(ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT')/7)
    *lnStyPos   = CEILING(ASCAN(laOGFxFlt,'STYLE.STYLE')/7)
    *lnFabPos   = CEILING(ASCAN(laOGFxFlt,'FABRIC.FABRIC')/7)
    *lnLocPos   = CEILING(ASCAN(laOGFxFlt,'WAREHOUS.CWARECODE')/7)
    *lnRepPos   = CEILING(ASCAN(laOGFxFlt,'SALESREP.REPCODE')/7)
    *lnOrdPos   = CEILING(ASCAN(laOGFxFlt,'ORDHDR.ORDER')/7)
    *lnStartPos = CEILING(ASCAN(laOGFxFlt,'ORDLINE.START')/7)
    *lnCompPos  = CEILING(ASCAN(laOGFxFlt,'ORDLINE.COMPLETE')/7)
    *lnGrpPos   = CEILING(ASCAN(laOGFxFlt,'STYLE.CSTYGROUP')/7)
    *lnSeaPos   = CEILING(ASCAN(laOGFxFlt,'ORDHDR.SEASON')/7)
    *lnDivPos   = CEILING(ASCAN(laOGFxFlt,'ORDHDR.CDIVISION')/7)
    *lnPriPos   = CEILING(ASCAN(laOGFxFlt,'ORDHDR.PRIORITY')/7)
    *-- Convert all ceiling functions to use lfItmPos because [End..

    *B602562,1 In Fixed filter position does not change [Begin
    *B602562,1 thus comment out the following block
  
    *lnSubTract = 0
    *IF EMPTY(ALLTRIM(lcFree_Clr))
    *  lnSubTract = 2
    *ELSE

      *-- Convert all ceiling functions to use lfItmPos because [Begin
      *lnClrSgPos = CEILING(ASCAN(laOGFxFlt,;
      *             'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)')/7)

      *lnClrSgPos = lfItmPos('SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)')
      *-- Convert all ceiling functions to use lfItmPos because [End..
      *lnFreSgPos = lnClrSgPos
      *lnSubTract = 1
    *ENDIF               

    *IF 'MA' $ gcCmpModules
    *  SET ORDER TO FABRIC IN FABRIC      && To use it to validate FABRIC  # in option grid.
    *ELSE
    *  lnClrSgPos = lnClrSgPos - 1
    *  lnFreSgPos = lnFreSgPos - 1
    *  lnOrdPos   = lnOrdPos   - 1
    *  lnStartPos = lnStartPos - 1
    *  lnCompPos  = lnCompPos  - 1
    *  lnGrpPos   = lnGrpPos   - 1
    *  lnSeaPos   = lnSeaPos   - 1
    *  lnDivPos   = lnDivPos   - 1
    *  lnPriPos   = lnPriPos   - 1
    *ENDIF  

    *lnOrdPos   = lnOrdPos   - lnSubTract
    *lnStartPos = lnStartPos - lnSubTract
    *lnCompPos  = lnCompPos  - lnSubTract
    *lnGrpPos   = lnGrpPos   - lnSubTract
    *lnSeaPos   = lnSeaPos   - lnSubTract
    *lnDivPos   = lnDivPos   - lnSubTract
    *lnPriPos   = lnPriPos   - lnSubTract
    *B602562,1 In Fixed filter position does not change [End..
  
    *SELECT (lcThAlias)

*ELSE  && not first time to run the report. [you come from advanced.]

  *-- Disable objects that disabled before. [begin]
  *B602562,1 FOR lnJ = 1 TO lnVarbEnd
  *B602562,1   IF !laRpVarNow[lnJ] AND SEEK('SORDDET V'+ STR(lnJ),'SYREPUVR')
  *B602562,1       lcVar = ALLTRIM(mFld_Name)
  *B602562,1       lcVal = &lcVar
  *B602562,1       = lfObjState('D',lnJ,lcVar,lcVal)    && Again disable this line.
  *B602562,1   ENDIF
  *B602562,1 ENDFOR
  *-- Disable objects that disabled before. [begin]
  *--MAN
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

*B603946,1 Clear SELECT BY if user change only SELECT BY option from 
*B603946,1 one type to another or user presses RESET button [Begin]
*STORE .T. TO llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
IF lcRpSelcBy = "L" OR !(lcRpSelcBy == lcOldSelc)
  STORE .T. TO llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
ENDIF
lcOldSelc = lcRpSelcBy
*B603946,1 Clear SELECT BY if user change only SELECT BY option [End]

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

*B604748,1 Check the pre-saved filter setting. [Begin]
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
*B604748,1 Check the pre-saved filter setting. [End]

*B606304,1 BWA 01/29/2003 Get the length of the style.[START]
*--The Style length
STORE 0 TO lnLenth
lnLenth = LEN(gfItemMask('PM'))
*B606304,1 BWA 01/29/2003.[END]

*E500342,1 Checking for the Order Status to Enable/Disable the 2 options which
*E500342,1 was added to the selection grid.
DO lpChkStat

*--End of lfwRepWhen.
*!**************************************************************************
*! Name      : lfwOldSelc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/08/2000
*! Purpose   : To store the Old Select By value
*!**************************************************************************
*! Example   : = lfwOldSelc()
*!**************************************************************************
*B603946,1 Add when function to SELECT BY option to save SELECT BY old value
*B603946,1 so as to compare it with the new value in lfwRepWhen 
*B603946,1 Note: If you are going to modify this function please refer to bug # 603946 in lfwRepWhen.
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
    *E500342,1 Adding the 1st Option to the If condition [Begin]
    *E500342,1 Print Partially Shipped Orders (Y/N)
    *IF SEEK('O'+&lcTOrdFile..ORDER) AND;
    *   EVALUATE(lcStatusEx) AND ;
    *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
    *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
    *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
    *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
    *C102262,1 (Begin) Get proper orders not only 'O' type.
    *IF SEEK('O'+&lcTOrdFile..ORDER) AND;
       EVALUATE(lcStatusEx) AND ;
       (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
       IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
       IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
       IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
       IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
    
    *E301621,1 Include Web Orders. [Begin]
    *IF IIF(EMPTY(lcRpOrdTyp),SEEK('O'+&lcTOrdFile..ORDER) OR SEEK('T'+&lcTOrdFile..ORDER),SEEK(lcRpOrdTyp+&lcTOrdFile..ORDER)) AND;    
    *   EVALUATE(lcStatusEx) AND ;
    *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
    *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
    *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
    *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
    *   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
    IF IIF(EMPTY(lcRpOrdTyp),SEEK('O'+&lcTOrdFile..ORDER) OR SEEK('T'+&lcTOrdFile..ORDER),SEEK(lcRpOrdTyp+&lcTOrdFile..ORDER)) AND;    
       EVALUATE(lcStatusEx) AND ;
       (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
       IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
       IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
       IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
       IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) ;
       AND IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.)) 
    *E301621,1 Include Web Orders. [End]  
       
    *C102262,1 (End)       
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
        *IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
           (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
           (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
           IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
           IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
           IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
           IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))
        
        *E500342,1 Adding the 2nd Option to the If condition [Begin]
        *E500342,1 Include Allocated Order Lines (Y/N)
        *IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
        *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
        *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
        *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
        *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
        *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
        *   IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))
        *B803674,1 Apply Partially Shipped on Order lines also [Begin]        
        *IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
        *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
        *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
        *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
        *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
        *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
        *   IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp)) AND ;
        *   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
        IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
           (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
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
           
           *E500444,1 HBG 03/19/2002 In case of print the report by Account , allow 
           *E500444,1                the user to filter on particular styles[Begin]
           IF lcRpSelcBy = 'A'
             lcTmpSty = laOGFxFlt[lnStyPos,6]
             IF USED(lcTmpSty) AND RECCOUNT(lcTmpSty) > 0 AND;
               !SEEK(SUBSTR(ORDLINE.Style,1,lnMajorlen),lcTmpSty)
                 LOOP
             ENDIF  
           ENDIF  
           *E500444,1 [End]
 
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

          *B606575,1 BWA 11/12/2002 Fix bug of the report prints wrong qty in case Summarize Multi Store = "YES".[START]
          *lcSeekExp = Style + DTOS(Complete) + cordtype + order
          *-- if you do not find this (style + order) in line file, add record for it.
          *IF !SEEK(lcSeekExp,lcTempLine)
          *  SCATTER MEMVAR MEMO
          *  m.cCurrCode = ORDHDR.cCurrCode
          *  = lfSumStyle(lcWorkFile,lcSeekExp)  && sum for this style.
          *  INSERT INTO (lcTempLine) FROM MEMVAR
          *ENDIF

          PRIVATE lcOldKey , lnRcnoOrd
          IF llCurInGrp
            lcSeekExp = STYLE + CCURRCODE + DTOS(COMPLETE) + CORDTYPE + ORDER
          ELSE
            lcSeekExp = STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER
          ENDIF

          lcOldKey = KEY()
          lnRcnoOrd = RECNO(lcWorkFile)
          SCATTER MEMVAR MEMO
          IF llCurInGrp
            INDEX ON STYLE + CCURRCODE + DTOS(COMPLETE) + CORDTYPE + ORDER TAG (lcWorkFile)
          ELSE
            INDEX ON STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER TAG (lcWorkFile)
          ENDIF

          IF !SEEK(lcSeekExp,lcTempLine)
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

            INSERT INTO (lcTempLine) FROM MEMVAR
          ENDIF  

          INDEX ON &lcOldKey TAG (lcWorkFile)
          GO lnRcnoOrd IN (lcWorkFile)
          IF llCurInGrp
            lcSeekExp = STYLE + CCURRCODE + DTOS(COMPLETE) + CORDTYPE + ORDER
          ELSE
            lcSeekExp = STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER
          ENDIF
          *B606575,1 BWA 11/12/2002.[END]

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
              *C102262,1 (Begin) Get proper orders not only 'O' type.
              *IF CORDTYPE = 'O' AND ;
                 EVALUATE(lcStatusEx) AND ;
                 (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                 IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                 SEEK('O'+ORDER,'ORDLINE')
              
              *E301621,1 Include Web Orders. [Begin]
              *IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
              *   EVALUATE(lcStatusEx) AND ;
              *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
              *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
              *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
              *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
              *   SEEK('O'+ORDER,'ORDLINE')
              
              *B604687,1 Seek either by "O" or "T" in OrdLine file. [Begin] 
              *IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
              *   EVALUATE(lcStatusEx) AND ;
              *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
              *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
              *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
              *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
              *   SEEK('O'+ORDER,'ORDLINE') AND IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.)) 
              IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
                 EVALUATE(lcStatusEx) AND ;
                 (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                 IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                 IIF(EMPTY(lcRpOrdTyp),SEEK('O'+ORDER,'ORDLINE') OR SEEK('T'+ORDER,'ORDLINE'),SEEK(lcRpOrdTyp+ORDER,'ORDLINE')) AND ;
                 IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.)) 
              *B604687,1 Seek either by "O" or "T" in OrdLine file. [End]

              *E301621,1 Include Web Orders. [End]
                 
              *C102262,1 (End)
              *E500271,4 add the lcRpEdiFlt to the expression [End.] 
                 
                SELECT ORDLINE 
                *-- scan ordline for rest order lines.
                SCAN REST WHILE cordtype+order+STR(lineno,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
                  *-- if <ordline filter> and <style group filter> and <Color Filter>
                  *E301421,1 ABD- Add Feature to allow printing the Bid and Complete status,
                  *E301421,1 ABD- And in case of Complete status we will print Booked Qty
                  *E301421,1 ABD- Because the TotQty is equal to Zero and change the IF
                  *E301421,1 ABD- Condition to accept totQty = Zero. [Begin]
                  *IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                     (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                     (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                     IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
                     IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
                     IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))   AND ;
                     IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))

                  *E500342,1 Adding the 2nd Option to the If condition [Begin]
                  *E500342,1 Include Allocated Order Lines (Y/N)
                  *IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                  *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                  *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                  *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
                  *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
                  *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))   AND ;
                  *   IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))
                  *B803674,1 Apply Partially Shipped on Order lines also [Begin]
                  *IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                  *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                  *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                  *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
                  *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
                  *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))   AND ;
                  *   IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp)) AND ;
                  *   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
                  IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                     (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                     (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                     IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
                     IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
                     IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))   AND ;
                     IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp)) AND ;
                     IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
                     IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
                  *B803674,1 Apply Partially Shipped on Order lines also [End]
                  *E500342,1 Adding the 2 Options to the If condition [End]
                  *E500342,1 Include Allocated Order Lines (Y/N)
                  
                    *E500444,1 HBG 03/19/2002 In case of print the report by Account , allow 
                    *E500444,1                the user to filter on particular styles[Begin]
                    lcTmpSty = laOGFxFlt[lnStyPos,6]
                    IF USED(lcTmpSty) AND RECCOUNT(lcTmpSty) > 0 AND;
                          !SEEK(SUBSTR(ORDLINE.Style,1,lnMajorlen),lcTmpSty)
                      LOOP
                    ENDIF  
                    *E500444,1 [End]
 
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
                    *E301421,1 ABD- [End]
                    
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
          *B606653,1 ASH 11/13/2002 (Begin) ONLY evaluate the season filter in case of filter by style.season, else we will filter in the IF condition.
          IF SEEK(PADR(CSTYMAJOR,lnMajorLen),'ORDLINE') AND ;
             IIF(EMPTY(lcGrpExp),.T., EVALUATE(lcGrpExp)) AND ;
             IIF(EMPTY(lcSeaExp) OR lcRpSeaFlt ='O',.T., EVALUATE(lcSeaExp)) AND ;
             IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))
          *B606653,1 ASH 11/13/2002 (End)
          *B603104,1 Avoid duplicate records [End  ]
             
            SELECT ORDLINE
            *-- scan ordline for the rest of this style.

            *B603104,1 Avoid duplicate records [Begin]
            *SCAN REST WHILE STYLE+DTOS(complete)+cordtype+order+store+STR(lineno,6) = &lcSlctFile..CSTYMAJOR

            *E500342,1 Adding the 1st Option to the If condition [Begin]
            *E500342,1 Print Partially Shipped Orders (Y/N)
            *SCAN REST WHILE STYLE+DTOS(complete)+cordtype+order+store+STR(lineno,6) = PADR(&lcSlctFile..CSTYMAJOR,lnMajorLen)
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
                 IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                 IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
                              
              *E500342,1 Adding the 2 Options to the If condition [Begin]
              *E500342,1 Include Allocated Order Lines (Y/N)
              *IF CORDTYPE = 'O' AND ;
              *   EVALUATE(lcStatusEx) AND ;
              *   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
              *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
              *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
              *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
              *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
              *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
              *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
              *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
              *B803674,1 Apply Partially Shipped on Order lines also [Begin]                     
              *IF CORDTYPE = 'O' AND ;
              *   EVALUATE(lcStatusEx) AND ;
              *   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
              *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
              *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
              *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
              *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
              *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
              *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
              *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
              *   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
              *C102262,1 (Begin) Get proper orders not only 'O' type.              
              *IF CORDTYPE = 'O' AND ;
                 EVALUATE(lcStatusEx) AND ;
                 IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                 IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
                 IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
                 IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
              
              *E301621,1 Include Web Orders. [Begin]   
              *IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
              *   EVALUATE(lcStatusEx) AND ;
              *   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
              *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
              *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
              *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
              *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
              *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
              *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
              *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
              *   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
              *   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
              *B606653,1 ASH 11/13/2002 (Begin) Add the season filter in case filtering by ordhdr.season.
              IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
                 EVALUATE(lcStatusEx) AND ;
                 IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                 IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                 IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                 IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
                 IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
                 IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT))) ;
                 AND IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.)) ;
                 AND IIF(EMPTY(lcSeaExp),.T.,IIF(lcRpSeaFlt ='O',EVAL(lcSeaExp),.T.))                
                
               *B606653,1 ASH 11/13/2002 (End)
              *E301621,1 Include Web Orders. [End]
                 
              *C102262,1 (End)   
              *B803674,1 Apply Partially Shipped on Order lines also [End]    
              *E500342,1 Adding the 2 Options to the If condition [End]
              *E500342,1 Include Allocated Order Lines (Y/N)

              *E301421,1 ABD-  [End]
              *E500271,4 add the lcRpEdiFlt to the expression [End.] 
                
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
              *E500342,1 Adding the 1st Option to the If condition [Begin]
              *E500342,1 Print Partially Shipped Orders (Y/N)
              *SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = ;
              *                STYLE.STYLE
              SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = ;
                              STYLE.STYLE FOR IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
              *E500342,1 Adding the 1st Option to the If condition [End]
              *E500342,1 Print Partially Shipped Orders (Y/N)
                *-- if <ordhdr filter> and <ordline filter>
                *E500271,4 add the lcRpEdiFlt to the expression [Begin.] 
                *--B603870,1 RAMY consider the case of completed orders [start]
                *IF CORDTYPE = 'O' AND TotQty > 0 AND ;
                   EVALUATE(lcStatusEx) AND ;
                   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
                *E500271,4 add the lcRpEdiFlt to the expression [End.] 

                *E500342,1 Adding the 2 Options to the If condition [Begin]
                *E500342,1 Include Allocated Order Lines (Y/N)
                *IF CORDTYPE = 'O' AND ;
                *   EVALUATE(lcStatusEx) AND ;
                *   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
                *B803674,1 Apply Partially Shipped on Order lines also [Begin]
                *IF CORDTYPE = 'O' AND ;
                *   EVALUATE(lcStatusEx) AND ;
                *   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                *   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
                *C102262,1 (Begin) Get proper orders not only 'O' type.
                *IF CORDTYPE = 'O' AND ;
                   EVALUATE(lcStatusEx) AND ;
                   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
                   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))

                *E301621,1 Include Web Orders. [Begin]
                *IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
                *   EVALUATE(lcStatusEx) AND ;
                *   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                *   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
                *   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
                IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
                   EVALUATE(lcStatusEx) AND ;
                   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
                   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT))) ;
                   AND IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.))
                *E301621,1 Include Web Orders. [End]
                   
                *C102262,1 (End)
                *B803674,1 Apply Partially Shipped on Order lines also [End]
                *E500342,1 Adding the 2 Options to the If condition [End]
                *E500342,1 Include Allocated Order Lines (Y/N)
                *--B603870,1 RAMY [end]

  
                  SCATTER MEMVAR MEMO
                  m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
                  m.cCurrCode = ORDHDR.cCurrCode
                  
                  *--B603870,1 RAMY if the order is completed get the Book Qty insted of the Tot Qty [start]
                  IF ORDHDR.STATUS = "C"
                  *-- Get qty from book qty becauce qty =0
                    FOR I = 1 To 8
                      lcCount = ALLTRIM(STR(I))
                      m.QTY&lcCount = BOOK&lcCount
                    ENDFOR
                    m.TOTQTY = TOTBOOK
                  ENDIF
                  *--B603870,1 RAMY [end]
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

          *E500342,1 Adding the 1st Option to the If condition [Begin]
          *E500342,1 Print Partially Shipped Orders (Y/N)
          *SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
          *              CORDTYPE = 'O' AND  ;
          *              EVALUATE(lcStatusEx) AND ;
          *              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
          *              IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
          *              IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
          *              IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
          *C102262,1 (Begin) Get proper orders not only 'O' type.                          
          *SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
                        CORDTYPE = 'O' AND  ;
                        EVALUATE(lcStatusEx) AND ;
                        (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                        IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                        IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                        IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
                     
          *E301621,1 Include Web Orders. [Begin]          
          *SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
          *              IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND  ;
          *              EVALUATE(lcStatusEx) AND ;
          *              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
          *              IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
          *              IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
          *              IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
          *              IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
          SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
                        IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND  ;
                        EVALUATE(lcStatusEx) AND ;
                        (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                        IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
                        IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                        IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) ;
                        AND IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.))
          *E301621,1 Include Web Orders. [End]
                        
          *C102262,1 (End)              
          *E500342,1 Adding the 1st Option to the If condition [End]
          *E500342,1 Print Partially Shipped Orders (Y/N)
          *E500271,4 add the lcRpEdiFlt to the expression [End.] 
                        
          *-- scan ordhdr for this Location.
          SCAN
            *C102262,1 (Begin) Get proper orders not only 'O' type.
            *= SEEK('O'+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
            = SEEK(IIF(EMPTY(lcRpOrdTyp),'O',lcRpOrdTyp)+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
            IF EOF('ORDLINE') AND EMPTY(lcRpOrdTyp)
              = SEEK('T'+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
            ENDIF
            *C102262,1 (End)
            SELECT ORDLINE
            *-- Scan ordline file to get lines of this order
            *-- that evaluate critria.
            *C102262,1 (Begin) Get proper orders not only 'O' type.
            *SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O' + ORDHDR.ORDER
            SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = cOrdType + ORDHDR.ORDER
            *C102262,1 (End)
              *-- if <ordline filter> and <style group filter> and <Color Filter>
              *--B603870,1 RAMY consider the case of completed orders [start]
              *IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
                 IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
                 IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))

              *E500342,1 Adding the 2 Options to the If condition [Begin]
              *E500342,1 Include Allocated Order Lines (Y/N)
              *IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
              *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
              *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
              *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
              *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
              *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
              *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
              *B803674,1 Apply Partially Shipped on Order lines also [Begin]
              *IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
              *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
              *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
              *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
              *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
              *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
              *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
              *   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
              IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
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
                m.cCurrCode = ORDHDR.cCurrCode

                *--B603870,1 RAMY if the order is completed get the Book Qty insted of the Tot Qty [start]
                IF ORDHDR.STATUS = "C"
                *-- Get qty from book qty becauce qty =0
                  FOR I = 1 To 8
                    lcCount = ALLTRIM(STR(I))
                    m.QTY&lcCount = BOOK&lcCount
                  ENDFOR
                  m.TOTQTY = TOTBOOK
                ENDIF
                *--B603870,1 RAMY [end]
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
          
          *E500342,1 Adding the 1st Option to the If condition [Begin]
          *E500342,1 Print Partially Shipped Orders (Y/N)
          *SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
          *              CORDTYPE = 'O' AND ;
          *              EVALUATE(lcStatusEx) AND ;
          *              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
          *              (EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
          *              (EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
          *              IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
          *C102262,1 (Begin) Get proper orders not only 'O' type.                    
          *SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
                        CORDTYPE = 'O' AND ;
                        EVALUATE(lcStatusEx) AND ;
                        (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                        (EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
                        (EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                        IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)

          *E301621,1 Include Web Orders. [Begin]
          *SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
          *              IIF(EMPTY(lcRpOrdTyp),CORDTYPE <> 'C',CORDTYPE = lcRpOrdTyp);
          *              EVALUATE(lcStatusEx) AND ;
          *              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
          *              (EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
          *              (EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
          *              IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
          *              IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
          
          *B604748,1 Add an 'And' operator. [Begin] 
          *SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
          *              IIF(EMPTY(lcRpOrdTyp),CORDTYPE <> 'C',CORDTYPE = lcRpOrdTyp);
          *              EVALUATE(lcStatusEx) AND ;
          *              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
          *              (EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
          *              (EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
          *              IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
          *              IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) ;
          *              AND IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.))                        
          SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
                        IIF(EMPTY(lcRpOrdTyp),CORDTYPE <> 'C',CORDTYPE = lcRpOrdTyp) AND ;
                        EVALUATE(lcStatusEx) AND ;
                        (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
                        (EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
                        (EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
                        IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
                        IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) ;
                        AND IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.))                        
          *B604748,1 Add an 'And' operator. [End] 
           
          *E301621,1 Include Web Orders. [End]
                        
          *C102262,1 (End)                        
          *E500342,1 Adding the 1st Option to the If condition [End]
          *E500342,1 Print Partially Shipped Orders (Y/N)

          *E500271,4 add the lcRpEdiFlt to the expression [End.] 
                        
          *B802429,1 Fix Filtering Bug [End  ]
                        
          *-- scan ordhdr for this sales rep.
          SCAN
            *C102262,1 (Begin) Get proper orders not only 'O' type.          
            *= SEEK('O'+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
            = SEEK(IIF(EMPTY(lcRpOrdTyp),'O',lcRpOrdTyp)+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
            IF EOF('ORDLINE') AND EMPTY(lcRpOrdTyp)
              = SEEK('T'+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
            ENDIF
            *C102262,1 (End)
            SELECT ORDLINE
            *-- Scan ordline file to get lines of this order
            *-- that evaluate critria.
            *C102262,1 (Begin) Get proper orders not only 'O' type.
            *SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O' + ORDHDR.ORDER
            SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = cOrdType + ORDHDR.ORDER
            *C102262,1 (End)
              *-- if <ordline filter> and <style group filter> and <Color Filter>
              *--B603870,1 RAMY consider the case of completed orders [start]
              *IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
                 (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
                 (EMPTY(lcSeaExp) OR EVALUATE(lcSeaExp)) AND ;
                 (EMPTY(lcDivExp) OR EVALUATE(lcDivExp)) AND ;
                 IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
                 IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
                 
              *E500342,1 Adding the 2 Options to the If condition [Begin]
              *E500342,1 Include Allocated Order Lines (Y/N)
              *IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
              *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
              *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
              *   (EMPTY(lcSeaExp) OR EVALUATE(lcSeaExp)) AND ;
              *   (EMPTY(lcDivExp) OR EVALUATE(lcDivExp)) AND ;
              *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
              *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
              *B803674,1 Apply Partially Shipped on Order lines also [Begin]
              *IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
              *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
              *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
              *   (EMPTY(lcSeaExp) OR EVALUATE(lcSeaExp)) AND ;
              *   (EMPTY(lcDivExp) OR EVALUATE(lcDivExp)) AND ;
              *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
              *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
              *   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
              IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
                 (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
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
                m.cCurrCode = ORDHDR.cCurrCode

                *--B603870,1 RAMY if the order is completed get the Book Qty insted of the Tot Qty [start]
                IF ORDHDR.STATUS = "C"
                *-- Get qty from book qty becauce qty =0
                  FOR I = 1 To 8
                    lcCount = ALLTRIM(STR(I))
                    m.QTY&lcCount = BOOK&lcCount
                  ENDFOR
                  m.TOTQTY = TOTBOOK
                ENDIF
                  *--B603870,1 RAMY [end]
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
          *IF !SEEK(lcSeekExp,lcTempLine) AND CORDTYPE = 'O' AND ;
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

          *E500342,1 Adding the 1st and 2nd Option to the If condition [Begin]
          *E500342,1 Print Partially Shipped Orders (Y/N)
          *E500342,1 Include Allocated Order lines (Y/N)
          *IF !SEEK(lcSeekExp,lcTempLine) AND CORDTYPE = 'O' AND ;
          *   EVALUATE(lcStatusEx) AND ;
          *   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
          *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
          *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
          *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
          *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
          *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
          *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
          *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
          *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
          *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
          *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
          *B803674,1 Apply Partially Shipped on Order lines also [Begin]
          *IF !SEEK(lcSeekExp,lcTempLine) AND CORDTYPE = 'O' AND ;
          *   EVALUATE(lcStatusEx) AND ;
          *   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
          *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
          *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
          *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
          *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
          *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
          *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
          *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
          *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
          *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
          *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
          *   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
          *   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
          *C102262,1 (Begin) Get proper orders not only 'O' type.
          *IF !SEEK(lcSeekExp,lcTempLine) AND CORDTYPE = 'O' AND ;
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
             IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
             
          *E301621,1 Include Web Orders. [Begin]
          *IF !SEEK(lcSeekExp,lcTempLine) AND IIF(EMPTY(lcRpOrdTyp),CORDTYPE <> 'C',CORDTYPE= lcRpOrdTyp) AND ;
          *   EVALUATE(lcStatusEx) AND ;
          *   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
          *   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
          *   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
          *   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
          *   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
          *   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
          *   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
          *   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
          *   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
          *   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
          *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
          *   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
          *   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
          *   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
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
          *E301621,1 Include Web Orders. [End]
             
          *C102262,1 (End)             
          *B803674,1 Apply Partially Shipped on Order lines also [End]
          *E500342,1 Adding the 1st and 2nd Option to the If condition [End]
          *E500342,1 Print Partially Shipped Orders (Y/N)
          *E500342,1 Include Allocated Order lines (Y/N)

            SCATTER MEMVAR MEMO
            m.cCurrCode = ORDHDR.cCurrCode


            IF ORDHDR.STATUS = "C"
            *-- Get qty from book qty becauce qty =0
              FOR I = 1 To 8
                lcCount = ALLTRIM(STR(I))
                m.QTY&lcCount = BOOK&lcCount
              ENDFOR
              m.TOTQTY = TOTBOOK
            ENDIF
            = lfSumMulti(lcSeekExp)  && summarize data.
            INSERT INTO (lcTempLine) FROM MEMVAR
          ENDIF    && end if you does not find line in temp line file,
        ENDSCAN    && end scan file for full index expression (rushmore).
        USE IN SUMMULTI

      ELSE  && Normal collection case for all data in ordline file.
        *-- scan ordline file for full index expression (rushmore)
        *C102262,1 (Begin) Get proper orders not only 'O' type.
        *SCAN FOR CORDTYPE + ORDER + STR(LINENO,6) = 'O'
        SCAN FOR CORDTYPE + ORDER + STR(LINENO,6) = lcRpOrdTyp
        *C102262,1 (End)
          *-- if <ordhdr filter>, <ordline filter> and <style group filter> 
          *-- and <Color Filter>.
          *E500271,4 add the lcRpEdiFlt to the expression [Begin.]
          
          *--B603870,1 RAMY consider the case of completed orders [start]
          *IF TotQty > 0 AND ;
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

          *E500342,1 Adding the 1st and 2nd Option to the If condition [Begin]
          *E500342,1 Print Partially Shipped Orders (Y/N)
          *E500342,1 Include Allocated Order lines (Y/N)
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
          *B803674,1 Apply Partially Shipped on Order lines also [Begin]
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
          *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
          *   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
          *   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
          
          *E301621,1 Include Web Orders. [Begin]
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
          *   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
          *   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
          *   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
          *   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
          IF EVALUATE(lcStatusEx) AND ;
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
             IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
             IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
             IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
             IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT))) AND ;
             IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.))             
          *E301621,1 Include Web Orders. [End]
             
          *B803674,1 Apply Partially Shipped on Order lines also [End]
          *E500342,1 Adding the 1st and 2nd Option to the If condition [End]
          *E500342,1 Print Partially Shipped Orders (Y/N)
          *E500342,1 Include Allocated Order lines (Y/N)
          *--B603870,1 RAMY [end]

            SCATTER MEMVAR MEMO
            m.cTempKey = PADR(STYLE.FABRIC,7)+PADR(STYLE.CSTYGROUP,6)+PADR(ORDHDR.REP1,3)
            m.cCurrCode = ORDHDR.cCurrCode

            *--B603870,1 RAMY if the order is completed get the Book Qty insted of the Tot Qty [start]
            IF ORDHDR.STATUS = "C"
            *-- Get qty from book qty becauce qty =0
              FOR I = 1 To 8
                lcCount = ALLTRIM(STR(I))
                m.QTY&lcCount = BOOK&lcCount
              ENDFOR
              m.TOTQTY = TOTBOOK
            ENDIF
            *--B603870,1 RAMY [end]
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

  *B606575,1 BWA 11/12/2002 Fix bug of the report prints wrong qty in case Summarize Multi Store = "YES".[START]
  *SCAN
  *  lcSeekExp = Style + DTOS(Complete) + cordtype + order
  *  IF !SEEK(lcSeekExp,lcTempLine)
  *    SCATTER MEMVAR MEMO
  *    m.cCurrCode = ORDHDR.cCurrCode
  *    = lfSumStyle(lcWorkFile,lcSeekExp)
  *    INSERT INTO (lcTempLine) FROM MEMVAR
  *  ENDIF  
  *ENDSCAN

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

      INSERT INTO (lcTempLine) FROM MEMVAR
    ENDIF  
  ENDSCAN

  IF llCurInGrp
    lcSeekExp = STYLE + CCURRCODE + DTOS(COMPLETE) + CORDTYPE + ORDER
  ELSE
    lcSeekExp = STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER
  ENDIF
  INDEX ON &lcOldKey TAG (lcWorkFile)
  *B606575,1 BWA 11/12/2002.[END]

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
*! Name      : lfvSortBy
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
*-- end of lfvSortBy.


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
llRpScale = IIF(llRpSizes,.T.,.F.)
*B802246,1 llRpScale return True after changing llRpSizes to YES again [End.]

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
IF lcRpKind = 'S'
  STORE .F. TO llRpSummMt,llRpOrdLnt,llRpOrdNot
  *C101569,1 Default by line No. [Begin]
  lcRpSrt2 = "L"
  *C101569,1 Default by line No. [End  ]
ENDIF
CLEAR READ

*B602562,1 IF lcRpKind = 'D'

  *B602562,1 IF !INLIST(lcRpSortBy,'S','F','G')
  *B602562,1   = lfObjState('E',lnScale,'llRpScale')  && Enable Reprint scale when diff.
  *B602562,1 ENDIF

*B602562,1   DO CASE
*B602562,1     CASE lcRpSortBy = 'A' OR lcRpSortBy = 'O'
      *B602562,1 = lfObjState('E',lnNotePad,'llRpOrdNot')    && Enable notepad line.

      *B602562,1 IF !llOrdLnDis
      *B602562,1   = lfObjState('E',lnLineNote,'llRpOrdLnt')     && Enable Line notes line.
      *B602562,1 ENDIF  

*B602562,1     CASE lcRpSortBy = 'S'
      *B602562,1 = lfObjState('E',lnSumm,'llRpSummMt')     && Enable Summarization line.

      *B602562,1 IF !llRpSummMt AND !llOrdLnDis
      *B602562,1   = lfObjState('E',lnLineNote,'llRpOrdLnt')   && Enable Line notes line.
      *B602562,1 ENDIF

*B602562,1     OTHERWISE

      *B602562,1 IF !llOrdLnDis
      *B602562,1   = lfObjState('E',lnLineNote,'llRpOrdLnt')   && Enable Line notes line.
      *B602562,1 ENDIF

*B602562,1   ENDCASE

*B602562,1 ELSE
  
  *B602562,1 IF !INLIST(lcRpSortBy,'S','F','G')
  *B602562,1   = lfObjState('D',lnScale,'llRpScale')  && Disable Reprint scale when diff.
  *B602562,1 ENDIF

*B602562,1   DO CASE
    *B602562,1 CASE lcRpSortBy = 'A' OR lcRpSortBy = 'O'
    *B602562,1   = lfObjState('D',lnNotePad,'llRpOrdNot')    && Disable notepad line.

      *B602562,1 IF !llOrdLnDis
      *B602562,1   = lfObjState('D',lnLineNote,'llRpOrdLnt')     && disable Line notes line.
      *B602562,1 ENDIF  

*B602562,1     CASE lcRpSortBy = 'S'
      *-- IT'S THE SAME.
      *B602562,1 = lfObjState('D',lnSumm,'llRpSummMt')     && disable Summarization line.

      *B602562,1 IF !llRpSummMt AND !llOrdLnDis
      *B602562,1   = lfObjState('D',lnLineNote,'llRpOrdLnt')   && disable Line notes line.
      *B602562,1 ENDIF

*B602562,1     OTHERWISE

      *B602562,1 IF !llOrdLnDis
      *B602562,1   = lfObjState('D',lnLineNote,'llRpOrdLnt')     && disable Line notes line.
      *B602562,1 ENDIF  
 
*B602562,1   ENDCASE
*B602562,1 ENDIF
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

  *B606304,1 BWA 01/29/2003 Fix the compare line.[START]
  *SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(LEFT(Style,lnLenth)) == ALLTRIM(lcSty)
  *B606304,1 BWA 01/29/2003.[FIX]

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
*! Name        : lfGetNotes
*! Developer   : Mohamed Badran (MAB)
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
IF llPrntBoth
  *-- Note that the following Scheme
  *-- ....... cRecord = 'N1' ............. Line Notepad.
  *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
  DO CASE 
    CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcMastFile..Note_Mem))
      lcTitle = 'Order : ' + ORDER + ' - Style : ' + STYLE + ' - Line #  ' + ALLTRIM(STR(LINENO)) + '    Notes.'
      *lcNotes  =  ALLTRIM(Note_Mem) + CHR(10) + CHR(13)
      lcNotes  =  ALLTRIM(Note_Mem) + CHR(10)
    *C102262,1 (Begin)  Print EDI Tempp orders notepad.
    *CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
    CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B' + IIF(ORDHDR.cOrdType = 'T','T','')+Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
    *C102262,1 (End)
    
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
    *C102262,1 (Begin)  Print EDI Tempp orders notepad.  
    *CASE llRpOrdNot AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
    CASE llRpOrdNot AND SEEK('B' + IIF(ORDHDR.cOrdType = 'T','T','')+Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
    *C102262,1 (End)
      lcTitle = IIF(RECNO(lcMastFile) = lnLastRec,;
                'Order :' + ORDER + ' Notepad.','')
      *lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10) + CHR(13),'')
      lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
                 IIF(lcRpSortBy = 'A',CHR(10),'')
  ENDCASE
ENDIF
RETURN ''
*-- end of lfGetNotes.

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
*! Name      : lfGetRepVr
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/07/98
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
  lcInnHeadL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19)+ '  : ']
  lcInnHeadR = [SUBSTR(Style,lnMajorLen + 2) + '  ----  ' + ALLTRIM(STYLE.Desc1)]
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
      *C102262,1 (Begin) Sort on proper orders not only 'O' type.
      *lcSeekVal  = [ACCOUNT+cCurrCode+'O'+ORDER]
      lcSeekVal  = [ACCOUNT+cCurrCode+cOrdType+ORDER]
      *C102262,1 (End)

      lcOutGrp   = [ACCOUNT+CCURRCODE]         && Outer .FRX group field.
    ELSE
      *man
      *lcSeekVal  = [ACCOUNT+cCurrCode+'O'+ORDER]
      *C102262,1 (Begin) Sort on proper orders not only 'O' type.
      *lcSeekVal  =  [ACCOUNT+'O'+ORDER]
      lcSeekVal  =  [ACCOUNT+cOrdType+ORDER]
      *C102262,1 (End)
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
      *C102262,1 (Begin) Sort on proper orders not only 'O' type.
      *lcSeekVal = [cCurrCode+'O'+ORDER]
      lcSeekVal = [cCurrCode+cOrdType+ORDER]
      *C102262,1 (End)
      lcOutGrp  = [CCURRCODE+ORDER]
    ELSE
      *C102262,1 (Begin) Sort on proper orders not only 'O' type.
      *lcSeekVal = ['O'+ORDER]
      lcSeekVal = [cOrdType+ORDER]
      *C102262,1 (End)
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

    lcSubTitle = lcStyTitle
    lcInnGrp   = [SUBSTR(Style,lnMajorLen + 2)]

    *E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
    *E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
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

    *C102262,1 (Begin) Sort on proper orders not only 'O' type.
    *lcSeekVal = [cCurrCode+'O'+ORDER]
    lcSeekVal = [cCurrCode+cOrdType+ORDER]
    *C102262,1 (End)
    lcOutGrp  = [CCURRCODE]
    
ENDCASE          && end Different sort by cases.

*E301272,1 if it is currency format and sort by any thing rather than currency.
IF llCurInGrp AND (lcRpSortBy <> 'U')
  lcSubTitle = lcSubTitle+"/Currency"
ENDIF
*-- end of lfGetRepVr.

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
    *E301421,1 ABD Add Feature to allow printing the Bid and Complete status. [Begin]
    *lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;                                                            
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))))
    *E301421,1 ABD [End]
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
*! Developer : Mohamed Atia Badran (MAB)
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
*! Developer : Mohamed Atia Badran (MAB)
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
*! Developer : Mohamed Atia Badran (MAB)
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
*! Developer : Mohamed Atia Badran (MAB)
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
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- end of lfvCurDisp.

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
*! Developer : Sameh (SSE)
*! Date      : 07/28/99
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
*!E500271,4
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
*C102262,1 (Begin) Remark the following and apply new features.
*IF 'EB' $ gcCmpModules AND lcRpEdiPrn <> "B"
*  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
*ENDIF
  DO CASE
    * -- Orders
    CASE lcRpEdiPrn="O"
      lcRpEdiFlt = [OrdHdr.cOrdType = 'O' AND !OrdHdr.lEdiOrder]
      lcRpOrdTyp = 'O'
    * -- EDI recieved orders
    CASE lcRpEdiPrn="R"
      lcRpEdiFlt = [OrdHdr.cOrdType = 'O' AND OrdHdr.lEdiOrder]
      lcRpOrdTyp = 'O'
    * -- EDI temp orders
    CASE lcRpEdiPrn="T"
      lcRpEdiFlt = [OrdHdr.cOrdType = 'T']
      lcRpOrdTyp = 'T'
    CASE lcRpEdiPrn="B"
    * -- Both Orders + EDI Orders.
      lcRpEdiFlt = [OrdHdr.cOrdType = 'O']
      lcRpOrdTyp = 'O'
    CASE lcRpEdiPrn="A"
    * -- All
      lcRpEdiFlt = ''
      lcRpOrdTyp = ''
  ENDCASE
*C102262,1 (End)
llClearOrd = .T.
*-- end of lfvEdiOrd.

*adel
FUNCTION lfFillArry

DIMENSION laSeasnS[2,2]
laSeasnS[1,1] = lcStyMajor +' season'
laSeasnS[2,1] = 'Order header season'

DIMENSION laSeasnV[2,2]
laSeasnV[1,1] = 'S'
laSeasnV[2,1] = 'O'
