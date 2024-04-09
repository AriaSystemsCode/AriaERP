*:**************************************************************************
*: Program file  : ICDATSEN.PRG
*: Program desc. : DATE SENSITIVE INVENTORY REPORT FOR (OLSEN)
*: Date          : 08/05/1999
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Sameh (SSE)
*:**************************************************************************
*: Calls :  
*:         Procedures : lpCollData
*:             
*:         Functions  : lfwRepWhen()
*:                    : lfItmPos()
*:                    : lfNoRecord()
*:                    : gfModalGen()
*:                    : gfOpenFile()
*:                    : gfItemMask()
*:                    : gfBroWWare()
*:                    : gfStyBrw()
*:                    : FaBrow()
*:                    : lfCreatFil() 
*:                    : lfNonMaj()
*:                    : lfSRVSty()
*:                    : lfStySum()
*:                    : lfvStyle()
*:                    : lfvFabric()
*:                    : lfFillAray() 
*:                    : lfvWareHos() 
*:                    : lfwOldWare() 
*:                    : lfvSortBy()
*:                    : lfGetScale()
*:                    : lfvPrnWhD()
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*: Notes   : .... 
*:**************************************************************************
*: Example : DO ICDATSEN
*:**************************************************************************
*: This Program is due to CUSTOM PROGRAM FOR OLSEN (101587) 
*:**************************************************************************
*:Modifications
*:C101587 
*:E301406,1 KHM 05/23/2000 Changing the collecting data to be from StyInvJl
*:E301406,1                instead of Style & StyDye
*:B604007,1 WAB 11/02/2000 change the colocting of the date for the new index 
*:B603713,4 AAN 11/19/2000 Increase the width of totcost in tmp file from 12.2 to 13.2
*:B604407,1 WAB 04/18/2001 the report should not print styles with zero qty 
*:B605472,1 BWA 02/12/2002 Fix the bug of When "print location details" is Yes, it does not print location details.
*:B605336,1 BWA 04/01/2002 Fix the bug of printing the right cost.[Add record "llStanderd" IN SYREPUVR , And modify FRX]
*:B606592,1 BWA 10/31/2002 Fix the bug of the value cost isn't equal in different sort.
*:B606919,1 BWA 01/29/2003 Increase the width of the total fields 1 chr.[FIX IN FRX]
*:B607273,1 ALB 06/15/2003 Fix the header alliment [Begin]
*:B607419,1 ABD 07/21/2003 Print more than one loaction in case Sort by location.
*:B122222,1 NNA 04/11/2004 fix bug of that the report print only 1ST Size scale if the company is
*:B122222,1 NNA            extended size scale , so I fixed it in the FRX by removing [lfGeyScale(Scale)]
*:B122222,1 NNA            From The Group Header To The Group Details and the style too
*:B126528,1 BWA 03/09/2005 1) Increase the performance of the collecting data in the program.
*:B126528,1 BWA            2) Fix the bug of changing the value of the option "Print Location details" 
*:B126528,1 BWA               to .T. after sorting location and this add more records to the style sort.
*:B126528,1 BWA            3) Remove the Zero values from the fields of the size scale to make the report more readable.
*:B126528,2 MMR 08/26/2005 Fix bug of not printing the style name in some cases.          
*:B130039,1 NNA 11/17/2005 Fix bug that you get multiples records for the same Style/Color/Scale if you Sorted by Location
*:B129862,1 NNA 11/29/2005 Fix bug that this report don't show styles with totqty=0 and stkval<>0 with link to GL=Yes
*T20060731.0003 TMI 08/07/2006 Add a new filter to deny/show zero qty styls
*B608031,1 TMI 04/04/2007 fix a bug that not showing the style header
*E302393,1 TMI 05/09/2007 Suppress 0 qty transactions ( T20070424.0006  )
*:**************************************************************************
lcStTime = TIME()       && To store the current time
lcRpExp = STRTRAN(lcRpExp,"STYLE.","")

*T20060731.0003   TMI [Start] Add a new filter to deny/show zero qty styls
lcRpExp = lcRpExp + IIF(llRpShzero,'','.AND. TOTSTK<>0')
*T20060731.0003   TMI [End  ] 

*B126528,1 BWA 03/09/2005 Add the scale variables.[START]
*--Initial the variables used in the program.
STORE 0 TO lnScaLnGl , lnScaPosGl
=lfChkStrct() && Get the length of the style , color and scale.
*B126528,1 BWA 03/09/2005.[END]
*-- check first of Sort by WareHouse and Empty of Warehouse Code

*B607419,1 ABD - Remark the next lines. [Begin]
*IF lcRpSortBy = 'W' AND (EMPTY(lcWareCode) OR !SEEK(lcWareCode,'WareHous'))
*  *-- < WareHouse Cannot be empty , cannot proceed >
*  = gfModalGen('INM42167B00000','DIALOG')    
*  SET DEVICE TO SCREEN	    
*  RETURN
*ENDIF
*B607419,1 ABD - [End]

*-- Logical (GLOBAL) Flag used to determine whether the report is run for 
*-- the first time or not , if first time llOGFltCh = .T. and report will 
*-- collect data , if not first time llOGFltch = .F. and report will skip
*-- collecting data (save time) and goes directly to @ SAY (print loop)
*-- also if user changes the filter in Option Grid or presses Reset Push
*-- Button llOGFltCh will be .T. and collects data again.

*B607419,1 ABD - Save the old warehouse to assign it after collect 
*B607419,1 ABD - The date in case print more than one warehouse.  [Begin]
lcOldWare = lcWareCode
*B607419,1 ABD - [End]
*-- If User changed the Filter in OG [Begin]
IF llOGFltCh
  *-- If Temp file is used and has records inside
  IF USED(lcStyleTmp) AND RECCOUNT(lcStyleTmp) > 0
    = lfCreatFil()
  ENDIF
  
  *-- make the Style File match the criteria selected
  SELECT Style
  SET FILTER TO &lcRpExp
  *-- If print Warehouse Details is set to YES [Begin]

  *B126528,1 BWA 03/09/2005.[START]
  *IF llRpPrnWhD
  IF lcRpSortBy = 'W'
  *B126528,1 BWA 03/09/2005.[END]
    SELECT StyDye
    *-- If Multi Bins is true [Begin]
    IF llMultiBin
      SET RELATION TO Style+cWareCode INTO WhsLoc ADDITIVE
    ENDIF
    *-- If Multi Bins is true [End]
    SET RELATION TO Style INTO Style ADDITIVE

    *B606592,1 BWA 10/31/2002 Empty the variable when sorting by Style.[START]
    lcWareCode = IIF(lcRpSortBy = 'W',lcWareCode,'')
    *B606592,1 BWA 10/31/2002.[END]

	IF !EMPTY(lcWareCode)
      SET FILTER TO CWARECODE = lcWareCode AND !EOF('Style')
    ENDIF  

    *--  StyDye 
    *--       |___
    *--       |   WhsLoc     (If Multi Bins)     
    *--       |
    *--       |___
    *--           Style       
    
  ELSE         && Else print Warehouse Details is set to NO
    SELECT Style
    IF llMultiBin
      SET RELATION TO Style INTO WhsLoc ADDITIVE
    ENDIF  

    *--  Style 
    *--       |___
    *--           WhsLoc     (If Multi Bins)     

  ENDIF
  *-- Endif print Warehouse Details is Yes [Begin]
  
  *-- If no records match criteria

  IF lfNoRecord()
    RETURN
  ELSE
    STORE 0 TO lnRpTVlCst,lnRpTSalVl
    DO lpCollData   && collect the data to the temporary file
  ENDIF  
  *-- EndIf of no records match criteria  
ENDIF
*-- If User changed the Filter in OG [End]

*-- if end of file (no records match criteria) [Begin]
SELECT (lcStyleTmp)
IF lfNoRecord()
  *B607419,1 ABD - Assign the old warehouse to global warehouse variable after collect 
  *B607419,1 ABD - The date in case print more than one warehouse.  [Begin]
  lcWareCode = lcOldWare
  *B607419,1 ABD - [End]

  RETURN
ELSE
  *B604407,1 - WAB (start) the report should not print styles with zero qty 
  *DO gfDispRe WITH EVALUATE('lcRpName') 

  *B606592,1 BWA 10/31/2002 Add the check of the value.[START]
  *lcExpr = 'ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) <> 0'
  *removed by ASH on 08/05/03 for Issue # 118951.
  *lcExpr = 'ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8)+ABS(nStkval) <> 0'

  *B129862,1 NNA 11/29/2005 (Begin) if the Customer is linked to GL and Generate G/L cost Entries is True then 
  *B129862,1 NNA            Show Styles that have Zero Qty and Stock Value <> Zero
  STORE .F. TO lllink2gl,llGnglcst
  lllink2gl = IIF(gfGetMemVar('M_LINK_GL')='Y',.T.,.F.)
  llGnglcst = IIF(gfGetMemVar('M_GL_COST')='Y',.T.,.F.)
  IF lllink2gl AND llGnglcst
    lcExpr = 'ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) <> 0 OR ;
              (ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) = 0 AND nStkVal<>0)'
  
    *E302393,1 TMI [Start] Suppress 0 qty transactions whatever the nstkval is.
    IF llRpSprZQt
      lcExpr = 'ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) <> 0'
    ENDIF
    *E302393,1 TMI [End  ]               
  
  ELSE
    *B129862,1 NNA (End)
  
    lcExpr = 'ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) <> 0'

    *B129862,1 NNA (Begin)
  ENDIF
  *B129862,1 NNA (End)

  *B606592,1 BWA 10/31/2002.[END]
  *B126528,2 MMR 08/26/2005 Fix bug of not printing the style name in some cases.[Start]
  STORE SPACE(0) TO lcScalVal,lcOrder
  STORE 0 TO lnTotStk,lnRec,lnRecn
  SELE STYLE
  lnRecn=RECNO()
  lcOrder=Order()
  SET ORDER TO STYLE
  SELECT (lcStyleTmp)
  lnRec=RECNO()
  IF lcRpSortBy = 'W' 
    SCAN
      lnTotStk=ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8)
      
      =SEEK (STYLE,'STYLE')
      
      *B129862,1 NNA 11/29/2005 (Begin) Show style code and description if total qty or stock value <>0
      *IF lcScalVal # ALLTRIM(Style.cstymajor)+EVAL(lcStyleTmp+'.Scale')+EVAL(lcStyleTmp+'.cwarecode') AND lnTotStk<>0  
      *B608031,1 TMI [Start] check the variables lllink2gl AND llGnglcst to update the llPrnColr field
      *IF lcScalVal # ALLTRIM(Style.cstymajor)+EVAL(lcStyleTmp+'.Scale')+EVAL(lcStyleTmp+'.cwarecode') AND (lnTotStk<>0 OR NSTKVAL<>0)
      IF lcScalVal # ALLTRIM(Style.cstymajor)+EVAL(lcStyleTmp+'.Scale')+EVAL(lcStyleTmp+'.cwarecode') AND ;
         (lnTotStk<>0 OR IIF(lllink2gl AND llGnglcst,NSTKVAL<>0,.F.))
      *B608031,1 TMI [End  ] 
      *B129862,1 NNA (End)

        REPLACE llPrnColr WITH .T.
        lcScalVal = ALLTRIM(Style.cstymajor)+EVAL(lcStyleTmp+'.Scale')+EVAL(lcStyleTmp+'.cwarecode')
      ENDIF  
    ENDSCAN
  ELSE
    SCAN
       lnTotStk=ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8)
       =SEEK (STYLE,'STYLE')
   
       *B129862,1 NNA 11/29/2005 (Begin) Show style code and description if total qty or stock value <>0
       *IF lcScalVal # ALLTRIM(Style.cstymajor)+EVAL(lcStyleTmp+'.Scale') AND lnTotStk<>0  
       *B608031,1 TMI [Start] check the variables lllink2gl AND llGnglcst to update the llPrnColr field
       *IF lcScalVal # ALLTRIM(Style.cstymajor)+EVAL(lcStyleTmp+'.Scale') AND (lnTotStk<>0 OR NSTKVAL<>0)
       IF lcScalVal # ALLTRIM(Style.cstymajor)+EVAL(lcStyleTmp+'.Scale') AND ;
          (lnTotStk<>0 OR IIF(lllink2gl AND llGnglcst,NSTKVAL<>0,.F.))
       *B608031,1 TMI [End  ] 
       *B129862,1 NNA (End)
       
         REPLACE llPrnColr WITH .T.
         lcScalVal = ALLTRIM(Style.cstymajor)+EVAL(lcStyleTmp+'.Scale')
       ENDIF  
    ENDSCAN
  ENDIF 
  SELECT STYLE
  SET ORDER TO (lcOrder)
  IF lnRecn<>RECNO()
    IF BETWEEN(lnRecn,1,RECCOUNT())
      GOTO lnRecn 
    ENDIF  
  ENDIF  
  SELECT (lcStyleTmp)
  IF lnRec<>RECNO()
    IF BETWEEN(lnRec,1,RECCOUNT())
      GOTO lnRec 
    ENDIF  
  ENDIF  
  *B126528,2 MMR[End]           
  DO gfDispRe WITH EVALUATE('lcRpName') , 'FOR ' + lcExpr

  *B607419,1 ABD - Assign the old warehouse to global warehouse variable after collect 
  *B607419,1 ABD - The date in case print more than one warehouse.  [Begin]
  lcWareCode = lcOldWare
  *B607419,1 ABD - [End]

  *B604407,1 - WAB (End) 
ENDIF
*-- Endif of (no records match criteria) [End]
****************************************************************************
*************************** *-- End of Report--* ***************************
****************************************************************************

*!**************************************************************************
*! Name      : lfNoRecord
*! Developer : Sameh (SSE)
*! Date      : 08/05/99
*! Purpose   : Dectect if no records match criteria
*!**************************************************************************
*! Called from : PRG
*!**************************************************************************
*! Calls       : gfModalGen()
*!**************************************************************************
*! Example     : = lfNoRecord()
*!**************************************************************************
*
FUNCTION lfNoRecord
GO TOP               && To activate the file
IF EOF()             && if end of file (no records match criteria)
  *-- No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN .T.
ELSE
  RETURN .F.  
ENDIF
*-- End of lfNoRecord.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh (SSE)
*! Date      : 08/05/99
*! Purpose   : Option Grid When function
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : lfCreatFil()
*!**************************************************************************
*! Example     : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
SELECT StyInvJl
SET ORDER TO StyInvJl DESCENDING

lnStatPos = lfItmPos('STYLE.STATUS')    && store the Style Status position
laOGFxFlt[lnStatPos,6] = 'A'            && make status target defaulted to "A"

= lfCreatFil()       && to create the temp file 
*-- end of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh (SSE)
*! Date      : 08/05/1999
*! Purpose   : Evaluate fixed filter position within array.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Report code
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns            : Position
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
*! Name      : lfCreatFil
*! Developer : Sameh (SSE)
*! Date      : 08/05/1999
*! Purpose   : Create temporary file structure.
*!**************************************************************************
*! Called from : OG When function. OR Main PRG
*!**************************************************************************
*! Example   : =lfCreatFil()
*!**************************************************************************
*
FUNCTION lfCreatFil
*-- If change Sort By from Style to Warehouse or vise versa close the temp file first 
IF USED(lcStyleTmp)
  USE IN (lcStyleTmp)
ENDIF
*-- Var lcFileName to hold the Master File whether it's STYLE or STYDYE
lcFileName = IIF(lcRpSortBy = 'S','STYLE','STYDYE')

*E301406,1 KHM 05/23/2000 (Begin) Commenting the following line because
*E301406,1                we are using StyInvJl instead of Style of StyDye 
*E301406,1                in creating the structure of the temporary file.
*E301406,1                then we add the necessary fields.

SELECT StyInvJl
=AFIELDS(laFileStru) 
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'PRICEA'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 12
laFileStru[lnNewFld,4] = 2

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'SCALE'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 3
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'TOTCOST'
laFileStru[lnNewFld,2] = 'N'
*:B603713,4 AAN Increase the width of totcost in tmp file from 12.2 to 13.2[Start].
*laFileStru[lnNewFld,3] = 9
laFileStru[lnNewFld,3] = 10
*:B603713,4 AAN Increase the width of totcost in tmp file from 12.2 to 13.2[End].
laFileStru[lnNewFld,4] = 2

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'DESC'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 20
laFileStru[lnNewFld,4] = 0
  
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'Ave_Cost'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 10
laFileStru[lnNewFld,4] = 2

*B605472,1 BWA 02/12/2002 Add a field to print the locations.[START]
lnNewFld = ALEN(laFileStru,1) + 1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'llWare'
laFileStru[lnNewFld,2] = 'L'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0
*B605472,1 BWA 02/12/2002.[END]

*B126528,1 BWA 03/09/2005.[START]
=lfAddField("laFileStru", "ColrDesc"  , "C" ,25 ,0) &&Field hold the color description.
=lfAddField("laFileStru", "llPrnColr" , "L" ,1  ,0) &&Field check the scale change.
FOR lnCrtTmp = 1 TO 8
  lcNumQty = ALLTRIM(STR(lnCrtTmp))
  =lfAddField("laFileStru", "ScalSz"  + lcNumQty , "C" , 5 ,0)
ENDFOR
*B126528,1 BWA 03/09/2005.[END]

CREATE DBF (gcWorkDir+lcStyleTmp) FROM ARRAY laFileStru     && create the temp file
*E301406,1 KHM 05/23/2000 (End)
=gfOpenFile(gcWorkDir+lcStyleTmp,'','EX')   && to open the Temp File Exclusively
*E301406,1 KHM 05/23/2000 (Begin) Adding the an index.
*B607419,1 ABD - Remark the Next Line and create index on 
*B607419,1 ABD - Warehouse + style in case sort By location 
*B607419,1 ABD - and the location field is empty otherwise it 
*B607419,1 ABD - will work as it is. [Begin]
*INDEX ON Style+cWareCode TAG (lcStyleTmp) OF (lcStyleTmp)

*B130039,1 NNA 11/17/2005 (Begin) index on (cWareCode +Style) when user select sorted by location even he input a location or not
*IF lcRpSortBy = 'W' AND EMPTY(lcWareCode)
IF lcRpSortBy = 'W'
*B130039,1 NNA (End)

  INDEX ON cWareCode +Style TAG (lcStyleTmp) OF (lcStyleTmp)
ELSE
  INDEX ON Style+cWareCode TAG (lcStyleTmp) OF (lcStyleTmp)
ENDIF
*B607419,1 ABD - [End]

*E301406,1 KHM 05/23/2000 (End)

*-- End of lfCreatFil.
*!**************************************************************************
*! Name      : lpCollData
*! Developer : Khalid Mohi El-Din
*! Date      : 05/23/2000
*! Purpose   : Collect the data from StyInvJl file.
*!**************************************************************************
*! Called from : FRX
*!**************************************************************************
*! Example   : =lpCollData()
*!**************************************************************************
*E301406,1 KHM 05/23/2000 (Begin) Collecting the data in another way by using
*E301406,1                the StyInvJl file
*!**************************************************************************
PROCEDURE lpCollData
lcWareCode = IIF(lcRpSortBy = 'W',lcWareCode,'')
SELECT StyInvJl
lcOldTag = SET("ORDER")
*B604007,1 WAB (START) change to the new index STYLE+CWARECODE+DTOS(dTrDate)+cSession+cIRType
*SET ORDER TO TAG StyInvJl
SET ORDER TO TAG Stydate
*B604007,1 - WAB (END)

*B126528,1 BWA 03/09/2005.[START]
PRIVATE lcClrName , lcClrDes , lcScalVal
STORE SPACE(0) TO lcClrName , lcClrDes , lcScalVal
*B126528,1 BWA 03/09/2005.[END]
SELECT (lcFileName)

SCAN
  *B607419,1 ABD - Assign the current warehouse to global warehouse variable while collect 
  *B607419,1 ABD - The date in case print more than one warehouse.  [Begin]
  IF lcRpSortBy = 'W'
    lcWareCode = cWarecode
  ENDIF
  *B607419,1 ABD - [End]

  WAIT WINDOW 'Collecting Data for ' + lcMajTtl + ' ' + Style NOWAIT
  lcStyle   = IIF(lcFileName = 'STYDYE',Style.Style,Style)
  *B605336,1 ASH 04/08/2002 (Begin) Fix the bug of duplicating the qty's in case of using dyelot.
  lcDyelot  = IIF(lcFileName = 'STYDYE',STYDYE.Dyelot,SPACE(10))
  *B605336,1 ASH 04/08/2002 (End)
  SELECT StyInvJl
  IF SEEK(lcStyle+lcWareCode)
    *B604007,1 WAB (START) - change the experision to the new index 
    *SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrDate)+cTrCode = lcStyle;
              FOR dTrDate <= ldRpDate AND ;
              IIF(!EMPTY(lcWareCode),cWareCode = lcWareCode,.T.)
    *B605336,1 ASH 04/08/2002 (Begin) Fix the bug of duplicating the qty's in case of using dyelot.
    *B605336,1 ASH            by adding "AND IIF(lcFileName = 'STYDYE',cDyelot = lcDyelot,.T.)" to the for condition
    SCAN REST WHILE Style+cWareCode+DTOS(dTrDate)+cSession+cIrType= lcStyle;
              FOR dTrDate <= ldRpDate AND ;
              IIF(!EMPTY(lcWareCode),cWareCode = lcWareCode,.T.) ;
              AND IIF(lcFileName = 'STYDYE',cDyelot = lcDyelot,.T.)
    *B605336,1 ASH 04/08/2002 (End)
    *B604007,1 WAB (END)               
      WAIT WINDOW 'Collecting Data for ' + lcMajTtl + ' ' + Style NOWAIT
      SCATTER MEMVAR
      =SEEK(lcStyle,'Style')

      SELECT(lcStyleTmp)
      *B126528,1 ASH 03/14/05 (Begin) The index is cwarecode+style in case of sort by location.
      *IF !SEEK(lcStyle+lcWareCode)
      lcSeekExp = IIF(lcRpSortBy = 'W',lcWareCode+lcStyle,lcStyle+lcWareCode)
      IF !SEEK(lcSeekExp)
      *B126528,1 ASH 03/14/05 (End)
        INSERT INTO (lcStyleTmp) FROM MEMVAR
        REPLACE Scale    WITH Style.Scale    ,;
                TotCost  WITH Style.TotCost  ,;
                PriceA   WITH Style.PriceA   ,;
                Desc     WITH Style.Desc     ,;
                Ave_Cost WITH IIF(nTotStk<>0,nStkVal/nTotStk,0)
		*B126528,1 BWA 03/09/2005.[START]
		IF lcScalVal # ALLTRIM(Style.cstymajor)+Scale 
          *B126528,2 MMR 08/26/2005 Fix bug of not printing the style name in some cases.[Start]          
          *REPLACE llPrnColr WITH .T.
          *B126528,2 MMR[End]  
          lcScalVal = ALLTRIM(Style.cstymajor)+Scale
          =SEEK ('S'+Scale,'SCALE')
		ENDIF
		IF lcClrName # SUBSTR(Style,lnClrPo,lnColorLen)
		  lcClrDes = gfCodDes(SUBSTR(Style,lnClrPo,lnColorLen),'COLOR',.T.)
		  lcClrName = SUBSTR(Style,lnClrPo,lnColorLen)
		ENDIF
        REPLACE ColrDesc WITH lcClrDes
        
        FOR lnCrtTmp = 1 TO 8
		  lcNumQty = ALLTRIM(STR(lnCrtTmp))
		  REPLACE ScalSz&lcNumQty WITH SCALE.SZ&lcNumQty
		ENDFOR
        *B126528,1 BWA 03/09/2005.[END]

       *B605472,1 BWA 02/12/2002 Replace the location field with blank.[START]
       IF llRpPrnWhD AND lcRpSortBy = 'S'
         REPLACE cWareCode WITH SPACE(0)
       ENDIF
       *B605472,1 BWA 02/12/2002.[END]

      ELSE
        FOR lnStkCnt = 1 TO 8
          lcStkCnt = STR(lnStkCnt,1)
          REPLACE nStk&lcStkCnt WITH nStk&lcStkCnt + m.nStk&lcStkCnt
        ENDFOR
        REPLACE nTotStk WITH nTotStk + m.nTotStk,;
                nStkVal WITH nStkVal + m.nStkVal,;
                Ave_Cost WITH IIF(nTotStk<>0,nStkVal/nTotStk,0)
      ENDIF

      *B605472,1 BWA 02/12/2002 Add the details records for the locations in case Print details YES.[START]
      IF lcRpSortBy = 'S'
        IF llRpPrnWhD
          SELECT(lcStyleTmp)
          IF !SEEK(lcStyle + StyInvJl.cWareCode)
            INSERT INTO (lcStyleTmp) FROM MEMVAR
            REPLACE Scale     WITH Style.Scale        ,;
                    TotCost   WITH Style.TotCost      ,;
                    PriceA    WITH Style.PriceA       ,;
                    Desc      WITH Style.Desc         ,;
                    cWareCode WITH StyInvJl.cWareCode ,;
                    llWare    WITH .T.                ,;
                    Ave_Cost  WITH IIF(nTotStk<>0,nStkVal/nTotStk,0)
			*B126528,1 BWA 03/09/2005.[START]                    
			IF lcScalVal # ALLTRIM(Style.cstymajor)+Scale
              *B126528,2 MMR 08/26/2005 Fix bug of not printing the style name in some cases.[Start]          
    	      *REPLACE llPrnColr WITH .T.
    	      *B126528,2 MMR.[End]
        	  lcScalVal = ALLTRIM(Style.cstymajor)+Scale
			ENDIF

			IF lcClrName # SUBSTR(Style,lnClrPo,lnColorLen)
			  lcClrDes = gfCodDes(SUBSTR(Style,lnClrPo,lnColorLen),'COLOR',.T.)
			  lcClrName = SUBSTR(Style,lnClrPo,lnColorLen)
			ENDIF
        	REPLACE ColrDesc WITH lcClrDes
            
            FOR lnCrtTmp = 1 TO 8
     		  lcNumQty = ALLTRIM(STR(lnCrtTmp))
    		  REPLACE ScalSz&lcNumQty WITH SCALE.SZ&lcNumQty
	    	ENDFOR
            *B126528,1 BWA 03/09/2005.[END]

          ELSE
            FOR lnStkCnt = 1 TO 8
              lcStkCnt = STR(lnStkCnt,1)
              REPLACE nStk&lcStkCnt WITH nStk&lcStkCnt + m.nStk&lcStkCnt
            ENDFOR
            REPLACE nTotStk  WITH nTotStk + m.nTotStk ,;
                    nStkVal  WITH nStkVal + m.nStkVal ,;
                    Ave_Cost WITH IIF(nTotStk<>0,nStkVal/nTotStk,0)
          ENDIF
        ENDIF
      ENDIF
      *B605472,1 BWA 02/12/2002.[END]

    ENDSCAN
  ENDIF  
ENDSCAN
SET ORDER TO &lcOldTag IN StyInvJl

*--End of lpCollData.
*!**************************************************************************
*! Name      : lfNonMaj
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : To get the style nonmajor segment structure
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfItemMask()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfNonMaj()
*!**************************************************************************
*
FUNCTION lfNonMaj
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'  
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT  
    ELSE      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.      
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- End of lfNonMaj.

*!**************************************************************************
*! Name      : lfvStyle
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : validate style
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfStyBrw()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfvStyle()
*!**************************************************************************
* 
FUNCTION lfvStyle
lcStyle = VARREAD()
lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF
SET ORDER TO lcTag IN STYLE
*--End of lfvStyle.

*!**************************************************************************
*! Name      : lfvFabric
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : validate fabric
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : FaBrow()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfvFabric()
*!**************************************************************************
* 
FUNCTION lfvFabric
lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF  
*-- End of lfvFabric.

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : lcParm
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfsrvSty()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
* 
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
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : sum a specific field for the current style in style file
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Passed Parameters  : lcSty,lccomp,lnAddToVar
*!**************************************************************************
*! Returns            : Calculated field value.
*!**************************************************************************
*! Example   : =lfStySum()
*!**************************************************************************
* 
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  *SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)  
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
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


*!**************************************************************************
*! Name      : lfFillAray
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : Fill Sort By Array
*!**************************************************************************
*! Example   : = lfArrDummy()
*!**************************************************************************
*
FUNCTION lfFillAray
DIMENSION laSortDesc[2,1] , laSortVal[2,1]
laSortDesc[1,1] = PROPER(lcMajTtl)
laSortDesc[2,1] = 'Location'
laSortVal[1,1]  = 'S'
laSortVal[2,1]  = 'W'
*-- End of lfFillAray.

*!**************************************************************************
*! Name      : lfvWareHos
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : Validate Warehouse Code
*!**************************************************************************
*! Example   : = lfvWareHos()
*!**************************************************************************
*
FUNCTION lfvWareHos
PRIVATE lcWareHous , lcTag

lcWareHous = VARREAD()
lcTag      = ORDER('WAREHOUS')

SET ORDER TO WAREHOUS IN WAREHOUS

*B607419,1 ABD - Valid Ony if the Field is not Empty. [Begin]
IF !EMPTY(&lcWareHous) 
  *B607419,1 ABD - [End]
  IF LASTKEY() = 13 AND !MDOWN()
    IF SEEK(&lcWareHous.,'WAREHOUS') 
      &lcWareHous = WAREHOUS.cWareCode
    ELSE
      &lcWareHous = gfBroWWare(.T.)
    ENDIF
  ELSE
    &lcWareHous = lcOldWare
  ENDIF
  *B607419,1 ABD - End IF for IF Statment. [Begin]
ENDIF
*B607419,1 ABD - [End]

SET ORDER TO &lcTag IN WAREHOUS
*-- End of lfvWareHos.


*!**************************************************************************
*! Name      : lfwOldWare
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : To get the old value of warehouse
*!**************************************************************************
*! Example   : = lfwOldWare()
*!**************************************************************************
*
FUNCTION lfwOldWare
lcOldWare = EVALUATE(SYS(18))
*-- End of lfwOldWare.


*!**************************************************************************
*! Name      : lfvSortBy
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : valid sort by function
*!**************************************************************************
*! Example   : =lfvSortBy()
*!**************************************************************************
*
FUNCTION lfvSortBy
*llRpPrnWhD = IIF(lcRpSortBy='W',.T.,.N.)
CLEAR READ
*-- End of lfvSortBy.

*!**************************************************************************
*! Name      : lfvPrnWhD
*! Developer : Sameh (SSE)
*! Date      : 08/03/99
*! Purpose   : valid print warehouse details
*!**************************************************************************
*! Example   : =lfvPrnWhD()
*!**************************************************************************
*
*B126528,1 BWA 03/09/2005.[START]
FUNCTION remlfvPrnWhD
llOldPrWhD = llRpPrnWhD   && Save current print ware house details in another 
*-- End of lfvPrnWhD.
*B126528,1 BWA 03/09/2005.[END]

*!**************************************************************************
*! Name      : lfGetScale
*! Developer : Sameh (SSE)
*! Date      : 08/05/99
*! Purpose   : get the Scale sizes
*!**************************************************************************
*! Called from : FRX
*!**************************************************************************
*! Example   : =lfGetScale()
*!**************************************************************************
*
FUNCTION lfGetScale
PARAMETERS lcScale
PRIVATE lcOldAlias,lnX,lcString,lcScale,lcZ
lcOldAlias = ALIAS()

SELECT SCALE
SEEK 'S'+lcScale
lnX      = 1
lcString = ''
IF FOUND() 
  DO WHILE lnX <= CNT
    lcZ = STR(lnX,1)
    *B607273,1 ALB Fix the header alliment [Begin]
    *lcString = lcString + PADL(ALLTRIM(SZ&lcZ),5,' ') + IIF(lnX=CNT,'','  ')
    lcString = lcString + PADL(ALLTRIM(SZ&lcZ),5,' ') + IIF(lnX=CNT,'','   ')
    *B607273,1 ALB Fix the header alliment [end]
    lnX= lnX + 1
  ENDDO
ELSE
  lcString = '* * * E R R O R * * *'
ENDIF
IF LEN(TRIM(lcOldAlias)) > 0
  SELECT (lcOldAlias)
ENDIF
RETURN(lcString)
*-- End of lfGetScale.
*B126528,1 BWA 03/09/2005.[START]
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/09/2005
*! Purpose   : Add fields to the array of file structure.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : lcFldName -- Field Name
*!                   : lcFldType -- Field Type (C;N;L....M)
*!                   : lnFldLen  -- Field Length
*!                   : lnFldDec  -- Field Decimal
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
*! Name      : lfChkStrct
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/09/2005
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
*B126528,1 BWA 03/09/2005.[END]