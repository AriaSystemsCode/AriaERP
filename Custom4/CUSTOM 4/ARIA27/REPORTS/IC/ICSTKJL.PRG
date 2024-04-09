*:***************************************************************************
*: Program file  : ICSTKJL
*: Program desc. : Stock Adjustment Journal Report
*: For Report    : (@x,y SAY)
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:    Procedures : lpPrnData,lpLongB
*:    Functions  : gfModalGen,gfItemMask,gfBrowWare,gfMover.
*:               : lfWRunGrid,lfwOldVal,lfClearRep,
*:               : lfEvalSegs,lfvLoc,lfVRepF,lfInvExpr,
*:               : lfSRVSty,lfStySum,lfvInvTran,
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICSTKJL
*:***************************************************************************
*: This Report Program is due to E301074 ...
*:***************************************************************************
*:Modifications :
*B602342,1 MAB 12/14/1998, Typ is now aligned with its column in form B
*E301170,1 MAB 03/15/1999 Use variables detect OG filter changes. [Begin
*B802305,1 SSE 05/27/1999 Print period "FROM: / /  TO: / / " under report title
*B802687,1 RAMY 10/25/1999 Fixing close the WherHose file after closing the OG 
*B802687,1                                     from the Inventory Adj screen
*B802903,1 AMM Add the phisycal inventory to the totals
*B803033,1 BWA 29/02/2000 Fix the bug of Incorrect reporting of totals in adjustment journal.
*B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment
*B603229,1 ABD 11/08/2000 Fix bug that when select transaction type (Physical or Transfer) 
*B603229,1 ABD            You will not find any total for the Physical or Transfer transaction
*B603229,1 ABD            But you will find the total Adjustment =0
*E301800,1 BWA 01/21/2002 Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
*B605419,1 BWA 01/28/2002 Fix the bug of the broken fields in case there is no records to display.
*B605858,1 BWA 04/22/2002 Fix the bug of not printing the transferred location in case transferee and select specific location.
*B605975,1 BWA 09/07/2002 Fix the bug of printing the wrong title for the reason column.
*B607014,1 ASH 03/06/2003 Fix bug of 'end of file encountered'.
*B607162,1 KHM 06/18/2003 fix the bug of numeric overflow by increasing the width of the fields.
*E302212,1 TMI 07/27/2003 Allow Stock Adjustment report showing sizes
*B130558,1 EIH 12/21/2005 Fix bug of displaying wrong qty's in case of locking transaction.
*:******************************************************************************************

*B802687,1 RAMY Add new variables [start]
*B802903,1 AMM COMMENT OUT, initialize this variable in syrepuvr to be shown in all functions
*llWhare = .F.
*B802687,1 RAMY [end]
*B802903,1 AMM Initialize two arrays hold sub and grand total quantities
DIMENSION laAdjQty[8],laTotQty[8]
STORE 0 TO laAdjQty,laTotQty
*B802903,1 AMM end

*B803556,1 AMH Add variable to careate tmp file [Start]
lcInvTmp = gftempname()
*B803556,1 AMH Add variable to careate tmp file [End  ]
*-- if user change last selection.
*E301170,1 MAB 03/15/1999 Use variables detect OG filter changes. [Begin
*IF !(lcRpInvExp == lcLstInvEx) OR !(lcRpExp == lcLastExpr) OR llChStyle OR llClearFn

IF llOGFltCh OR llClearFn
*E301170,1 MAB 03/15/1999 Use variables detect OG filter changes. [End
  lcLastExpr = lcRpExp       && To later know that user change critria.
  lcLstInvEx = lcRpInvExp    && To later know that user change critria.
  llChStyle  = .F.           && To later know that user change critria.
  llClearFn  = .F.           && To later know that user change critria.

  llNoData   = .F.           && To Filter on data.

  lcFullExpr = [STYLE = '']
  lcFullExpr = IIF('.T.' $ lcRpExp,lcFullExpr,lcFullExpr + [ AND ] + STRTRAN(lcRpExp,'INVTADJ.',''))

  *-- Check for CFROMWARE in filter then change filter to be in Both CFROMWARE or CToWARE [begin]
  IF !EMPTY(laOGFxFlt[lnLocPos,6])  
    lnWareSt   = ATC('CFROMWARE',lcFullExpr)         && cFromWare start position.
    lnWareLen  = ATC(laOGFxFlt[lnLocPos,6],lcFullExpr)+LEN(laOGFxFlt[lnLocPos,6])+1-lnWareSt
    lcFromExpr = SUBSTR(lcFullExpr,lnWareSt,lnWareLen)
    lcToExpr   = STRTRAN(lcFromExpr,'CFROMWARE','CTOWARE')
    lcWareExpr = '((' + lcFromExpr + ') OR (' + lcToExpr + '))'
    lcFullExpr = STRTRAN(lcFullExpr,lcFromExpr,lcWareExpr)
  ENDIF
  *-- Check for CFROMWARE in filter then change filter to be in Both CFROMWARE or CToWARE [end  ]

  IF !EMPTY(lcRpInvExp)
    lcFullExpr = lcFullExpr + [ AND TYPE $ lcRpInvExp]
  ENDIF

ENDIF  && end if user change last selection.

*-- Previous run have no data and expression does not change.
IF llNoData

  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')

  *B605419,1 BWA 01/28/2002 The page broken after this message because the device didn't return to its original status which is screen.[START]
  SET DEVICE TO SCREEN
  *B605419,1 BWA 01/28/2002.[END]


ELSE    && else expression changed or previous run have data

  *B803556,1 AMH 23/08/2000 Creating Tmp file to add Locking transactions [Start]
  *SELECT INVTADJ
  
  *SET ORDER TO INVTADJ
  *SET RELATION TO STYLE INTO STYLE

  *SET FILTER TO &lcFullExpr
  *GO TOP
  =gfOpenFile(gcDataDir+"STYINVJL", "STYINVJL", "SH")
  SELECT INVTADJ
  LOCATE
  
  *B607162,1 KHM 06/18/2003 (Begin) Changing the way of creating the temporary file in order
  *B607162,1                to increase the width of the fileds.
  *DIMENSION laInvTmp[1,4]
  *=AFIELDS(laInvTmp)
  *CREATE CURSOR (lcInvTmp) FROM ARRAY laInvTmp
  =lfCreatFil()
  *B607162,1 KHM 06/18/2003 (End)
  
  SELECT STYINVJL
  SET FILTER TO CTRTYPE = '9'
  LOCATE
  DO WHILE !EOF('STYINVJL')
    SELECT INVTADJ
    IF !EOF()
      IF DATE < STYINVJL.DTRDATE
        =lfAPTAppen()
      ELSE
        =lfLockApen()
      ENDIF
    ELSE
      =lfLockApen()
    ENDIF
  ENDDO
  SELECT INVTADJ
  DO WHILE !EOF()
    =lfAPTAppen()
  ENDDO
  SELECT (lcInvTmp)
  
  INDEX ON STYLE TAG (lcInvTmp) OF (gcDataDir+lcInvTmp)
  SET RELATION TO STYLE INTO STYLE
  SET FILTER TO &lcFullExpr
  *E302212,1  TMI [Start] Open scale file and set relation
  IF !USED('SCALE')
    =gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
  ENDIF
  SELECT STYLE
  IF !'INTO SCALE' $ SET('RELATION')
    SET RELATION TO 'S'+STYLE.SCALE INTO SCALE ADDITIVE
  ENDIF  
  SELECT (lcInvTmp)
  *E302212,1  TMI [End  ]   
  LOCATE
  *B803556,1 AMH 23/08/2000 Creating Tmp file to add Locking transactions [End  ]

  IF EOF()
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')

    *B605419,1 BWA 01/28/2002 The page broken after this message because the device didn't return to its original status which is screen.[START]
    SET DEVICE TO SCREEN
    *B605419,1 BWA 01/28/2002.[END]

    llNoData = .T.
    SELECT (lcInvTmp)
    SET FILTER TO
  ELSE
  
    SET DEVICE TO PRINT
    *-- Call Report code...
    DO lpPrnData
    *-- Call Report code...
    DO ENDREPORT
    SET DEVICE TO SCREEN

  ENDIF
  SET FILTER TO
  SELECT STYINVJL
  SET FILTER TO 

ENDIF
*B803556,1 AMH Erase tmp files [Start]
ERASE (gcDataDir+lcInvTmp+'.*')
*-- end of report code...
*----------------------- Report Code End -----------------------------


*------------------ Functions Section ---------------------
*----------------------------------------------------------
*!*************************************************************

*!*************************************************************
*! Name      : lfWRunGrid
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
*! Purpose   : When run OG function.
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
*! Example   : =lfWRunGrid()
*!*************************************************************
FUNCTION lfWRunGrid
*-- First time run or user press < Reset > .
IF EMPTY(lcLastExpr)
  R_WIDTH    = 'W'
  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVTADJ.DATE'),1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(DATE())+'|'+DTOC(DATE())
  ENDIF
  lnLocPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVTADJ.CFROMWARE'),1)
ENDIF  
*--

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
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
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
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
*! Name      : lfvLoc
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
*! Purpose   : Valid function of the Location.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrowWare()
*!*************************************************************
*! Called from : Option Grid,Get Location Field.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvLoc()
*!*************************************************************
FUNCTION lfvLoc
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

IF !USED('WAREHOUS')
  USE (gcDataDir+'WAREHOUS') ORDER TAG Warehous IN 0 SHARED
  *B802687,1 RAMY  Add this line to change the value of the variable used to check on opening the file 
  *B802687,1 RAMY  when we run from the Inv Adj screen[start]
  llWhare = .T.
  *B802687,1 RAMY [end]
ENDIF

*IF The user want to Browse or if the Warehouse he entered is not in the
*file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfWOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
*! Purpose   : Get any get field old value.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid, any Get Field.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWOldVal()
*!*************************************************************
FUNCTION lfWOldVal
laOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
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

lnMajSeg   = gfItemMask('SM')  && No. of major segments.
lcMajPict  = gfItemMask("PM")

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

lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style code Structure. [End]


lcStyGrp = lcStyMajor + ' Group' 

*-- Define Mover for Inventory adjustments arrays. [begin]
*B803556,1 AMH 23/08/2000 add locking transaction to array [Start]
*lnArrayLen = IIF(llMultiWH,3,2)
lnArrayLen = IIF(llMultiWH,4,3)
DECLARE laRpSorInv[lnArrayLen,1],laRpTarInv[lnArrayLen,1]
STORE 'Adjustments' TO laRpSorInv[1],laRpTarInv[1]
STORE 'Physical'    TO laRpSorInv[2],laRpTarInv[2]
STORE 'Locking'     TO laRpSorInv[3],laRpTarInv[3]
*IF lnArrayLen = 3
*  STORE 'Transfer'    TO laRpSorInv[3],laRpTarInv[3]
*ENDIF  
IF lnArrayLen = 4
  STORE 'Transfer'    TO laRpSorInv[4],laRpTarInv[4]
ENDIF
*B803556,1 AMH 23/08/2000 add locking transaction to array [End  ]
*-- Define Mover for Inventory adjustments arrays. [end  ]

RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfVRepF
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
*! Purpose   : If user change report form valid function.
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
*! Example   : =lfVRepF()
*!*************************************************************
FUNCTION lfVRepF
CLEAR READ
*-- end of lfVRepF.

*!*************************************************************
*! Name      : lfvInvTran
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
*! Purpose   : If user Press Inventort transaction button.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvInvTran()
*!*************************************************************
FUNCTION lfvInvTran
= gfMover(@laRpSorInv,@laRpTarInv,'Select Inventory Transaction type',.T.,'')  && call mover function.
= lfInvExpr()
*-- end of lfvInvTran.

*!*************************************************************
*! Name      : lfInvExpr
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/21/1998
*! Purpose   : - Evaluate Company expression.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : lfvInvTran
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfInvExpr()
*!*************************************************************
FUNCTION lfInvExpr
PRIVATE laTarget

*-- Copy Used array.
IF EMPTY(laRpTarInv)
  = ACOPY(laRpSorInv,laTarget)
ELSE
  = ACOPY(laRpTarInv,laTarget)
ENDIF
  
= ASORT(laTarget)  && Sort array.
lcRpInvExp = ''

*-- loop to get inventory transaction type expression.
FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpInvExp = IIF(EMPTY(lcRpInvExp),PADR(laTarget[lnI],1),;
                    lcRpInvExp + ','+PADR(laTarget[lnI],1))
ENDFOR
*-- end of lfInvExpr.
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
llClearFn = .T.  && Rise clear done flag.
*B802687,1 RAMY change this line to check if we opend it from the Inv Adj screen or not [start]
*IF USED('WAREHOUS')
IF USED('WAREHOUS') .AND. llWHare
*B802687,1 RAMY  [end]
  USE IN WAREHOUS
ENDIF
IF USED('CODES')
  USE IN CODES
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lpPrnData
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : Print data.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrnData
*!*************************************************************
*!
PROCEDURE lpPrnData

IF llMultiWH
  lnStrtCol = 7
ELSE
  lnStrtCol = 0
ENDIF

R_TITLE = 'ADJUSTMENTS JOURNAL'
XREPORT = 'ICSTKJL'

*B802305,1 add period entered in OG "FROM: / /  TO: / / " to XTITLE [Begin.]
*XTITLE  = 'Stock Adjustment Report'
XTITLE  = [FROM: ] + SUBSTR(laOGFxFlt[lnDatePos,6],1,10) +; 
          [ TO: ]  + SUBSTR(laOGFxFlt[lnDatePos,6],12,21)
*B802305,1 add period entered in OG "FROM: / /  TO: / / " to XTITLE [End.]

ROW      = 99
XSTYLE   = PADR(STYLE,lnMajorLen)
XSTOTPCS = 0
XSTOTAMT = 0
XGTOTPCS = 0
XGTOTAMT = 0
PAGENO   = 0
*E302212,1  TMI [Start] Save last used scale
XSCALE = SCALE.SCALE
*E302212,1  TMI [End  ] 
  
XSTOTPhPCS = 0
XSTOTPhAMT = 0
XGTOTPhPCS = 0
XGTOTPhAMT = 0
DIMENSION XNEWQTY(8)                    && USED ONLY IN NEW LONG FORMAT
*B802903,1 AMM declare two arrays for physical inventory sub and grand total
*B803556,1 AMH declare Six arrays for physical ,Transfer 
*and Locking inventory sub and grand total [Start]
*DIMENSION laPhyQty[8],laPTotQty[8]
*STORE 0 TO laPhyQty,laPTotQty
DIMENSION laAllQty[8,4],laAlTotQty[8,4],XSTOTAlPCS[4],XSTOTAlAMT[4],XGTOTAlPCS[4],XGTOTAlAMT[4]
STORE 0 TO laAllQty,laAlTotQty,XSTOTAlPCS,XSTOTAlAMT,XGTOTAlPCS,XGTOTAlAMT
*B803556,1 AMH [End  ]
*B802903,1 AMM end

DO WHILE lcRpFormat='S'        && Short form the old one.

  *-- multiple warehouse in 'NO'.
  IF !llMultiWH .AND. Type = 'T'
    SKIP
    LOOP
  ENDIF

  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH  XREPORT,XTITLE,R_WIDTH
    ROW = 5

    *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
    *@ ROW,00 SAY IIF(llMultiWH ,'Locats ' ,'') + lcStyTitle  + ' ' +;
                  IIF (XDYELOT_S,'DYELOT #',SPACE(8))               +;
                  '   REFERENCE  TY DV     SZ1   SZ2   SZ3   SZ4' +;
                  '   SZ5   SZ6   SZ7   SZ8  TOTQTY  TOT. COST DATE'

    IF llMultiWH AND !llRpDec

      *B605975,1 BWA 09/07/2002 Fix the bug of printing the wrong title for the reason column.[START]
      *@ ROW,00 SAY IIF(llMultiWH ,'Locats ' ,'') + lcStyTitle  + ' ' +;
                   IIF (XDYELOT_S,'DYELOT #',SPACE(8))               +;
                   ' REFERENCE TY DV    SZ1    SZ2    SZ3    SZ4' +;
                   '    SZ5    SZ6    SZ7    SZ8  TOTQTY TOTCOST DATE'

      @ ROW,00 SAY IIF(llMultiWH ,'Locats ' ,'') + lcStyTitle  + ' ' +;
                   IIF (XDYELOT_S,'DYELOT #',SPACE(8))               +;
                   ' REASON    TY DV    SZ1    SZ2    SZ3    SZ4' +;
                   '    SZ5    SZ6    SZ7    SZ8  TOTQTY TOTCOST DATE'
      *B605975,1 BWA 09/07/2002.[END]

    ELSE

      *B605975,1 BWA 09/07/2002 Fix the bug of printing the wrong title for the reason column.[START]
      *@ ROW,00 SAY IIF(llMultiWH ,'Locats ' ,'') + lcStyTitle  + ' ' +;
                   IIF (XDYELOT_S,'DYELOT #',SPACE(8))               +;
                   ' REFERENCE TY DV    SZ1    SZ2    SZ3    SZ4' +;
                   '    SZ5    SZ6    SZ7    SZ8  TOTQTY  TOT. COST DATE'

      @ ROW,00 SAY IIF(llMultiWH ,'Locats ' ,'') + lcStyTitle  + ' ' +;
                   IIF (XDYELOT_S,'DYELOT #',SPACE(8))               +;
                   ' REASON    TY DV    SZ1    SZ2    SZ3    SZ4' +;
                   '    SZ5    SZ6    SZ7    SZ8  TOTQTY  TOT. COST DATE'
      *B605975,1 BWA 09/07/2002.[END]

    ENDIF
    *E301800,1 BWA 01/21/2002 (End)
    *E302212,1  TMI [Start] Add a line showing Scales in page header lines
    ROW = ROW + 1
    @ ROW,00 SAY IIF(llMultiWH ,'       ' ,'') + SPACE(LEN(lcStyTitle))  + ' ' +;
                 SPACE(8) +'                    '+;
                 SCALE.SZ1+'  '+SCALE.SZ2+'  '+SCALE.SZ3+'  '+SCALE.SZ4+'  '+;
                 SCALE.SZ5+'  '+SCALE.SZ6+'  '+SCALE.SZ7+'  '+SCALE.SZ8
    *E302212,1  TMI [End  ] 
    
    ROW = ROW + 1
  ENDIF

  IF PADR(STYLE,lnMajorLen) <> XSTYLE
    @ ROW,00 SAY REPLICATE('.',132)
    ROW = ROW + 1
    @ ROW,000           SAY '*** ADJUSTMENTS  SUBTOTAL *** ' + XSTYLE
    *B802903,1 AMM Print sub total size quantities
    *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
    *@ ROW,050+lnStrtCol SAY laAdjQty[1]   PICTURE '@Z 99999'
    *@ ROW,056+lnStrtCol SAY laAdjQty[2]   PICTURE '@Z 99999'
    *@ ROW,062+lnStrtCol SAY laAdjQty[3]   PICTURE '@Z 99999'
    *@ ROW,068+lnStrtCol SAY laAdjQty[4]   PICTURE '@Z 99999'
    *@ ROW,074+lnStrtCol SAY laAdjQty[5]   PICTURE '@Z 99999'
    *@ ROW,080+lnStrtCol SAY laAdjQty[6]   PICTURE '@Z 99999'
    *@ ROW,086+lnStrtCol SAY laAdjQty[7]   PICTURE '@Z 99999'
    *@ ROW,092+lnStrtCol SAY laAdjQty[8]   PICTURE '@Z 99999'

    @ ROW,045 + lnStrtCol SAY laAdjQty[1]   PICTURE '@Z 999999'
    @ ROW,052 + lnStrtCol SAY laAdjQty[2]   PICTURE '@Z 999999'
    @ ROW,059 + lnStrtCol SAY laAdjQty[3]   PICTURE '@Z 999999'
    @ ROW,066 + lnStrtCol SAY laAdjQty[4]   PICTURE '@Z 999999'
    @ ROW,073 + lnStrtCol SAY laAdjQty[5]   PICTURE '@Z 999999'
    @ ROW,080 + lnStrtCol SAY laAdjQty[6]   PICTURE '@Z 999999'
    @ ROW,087 + lnStrtCol SAY laAdjQty[7]   PICTURE '@Z 999999'
    @ ROW,094 + lnStrtCol SAY laAdjQty[8]   PICTURE '@Z 999999'
    *E301800,1 BWA 01/21/2002 (End)

    *B802903,1 AMM end

    *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
    *@ ROW,098+lnStrtCol SAY XSTOTPCS PICTURE '9999999'      && YMA 05/03/94
    @ ROW,101 + lnStrtCol SAY XSTOTPCS PICTURE '9999999'
    *E301800,1 BWA 01/21/2002 (End)

    IF qCostPrv              && COSTING ACCESS.
      *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
      *@ ROW,106+lnStrtCol SAY XSTOTAMT PICTURE '9999999.99' && YMA 05/03/94
      IF llMultiWH AND !llRpDec
        @ ROW,109 + lnStrtCol SAY XSTOTAMT PICTURE '9999999'
      ELSE
        @ ROW,109 + lnStrtCol SAY XSTOTAMT PICTURE '9999999.99'
      ENDIF
      *E301800,1 BWA 01/21/2002 (End)
    ENDIF

    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('.',132)
    ROW = ROW + 1
    XSTYLE   = PADR(STYLE,lnMajorLen)
    XSTOTPCS = 0
    XSTOTAMT = 0
    *B802903,1 AMM Reinitialize
    laAdjQty = 0
    *B802903,1 AMM end
    *E302212,1  TMI [Start] if Scale is changed , print the new scale &&>>> 
    IF SCALE.SCALE <> XSCALE .AND. !EOF()
      XSCALE = SCALE.SCALE
      @ ROW,00 SAY IIF(llMultiWH ,'       ' ,'') + SPACE(LEN(lcStyTitle))  + ' ' +;
                   SPACE(8) +'                    '+;
                   SCALE.SZ1+'  '+SCALE.SZ2+'  '+SCALE.SZ3+'  '+SCALE.SZ4+'  '+;
                   SCALE.SZ5+'  '+SCALE.SZ6+'  '+SCALE.SZ7+'  '+SCALE.SZ8  
      ROW = ROW + 1
    ENDIF
    *E302212,1  TMI [End  ] 
  ENDIF
  
  IF EOF()
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    @ ROW,000 SAY '*** ADJUSTMENTS  GRAND TOTAL ***'
    *B802903,1 AMM Print grand total quantity

    *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
    *@ ROW,050+lnStrtCol SAY laTotQty[1]   PICTURE '@Z 99999'
    *@ ROW,056+lnStrtCol SAY laTotQty[2]   PICTURE '@Z 99999'
    *@ ROW,062+lnStrtCol SAY laTotQty[3]   PICTURE '@Z 99999'
    *@ ROW,068+lnStrtCol SAY laTotQty[4]   PICTURE '@Z 99999'
    *@ ROW,074+lnStrtCol SAY laTotQty[5]   PICTURE '@Z 99999'
    *@ ROW,080+lnStrtCol SAY laTotQty[6]   PICTURE '@Z 99999'
    *@ ROW,086+lnStrtCol SAY laTotQty[7]   PICTURE '@Z 99999'
    *@ ROW,092+lnStrtCol SAY laTotQty[8]   PICTURE '@Z 99999'

    @ ROW,045 + lnStrtCol SAY laTotQty[1]   PICTURE '@Z 999999'
    @ ROW,052 + lnStrtCol SAY laTotQty[2]   PICTURE '@Z 999999'
    @ ROW,059 + lnStrtCol SAY laTotQty[3]   PICTURE '@Z 999999'
    @ ROW,066 + lnStrtCol SAY laTotQty[4]   PICTURE '@Z 999999'
    @ ROW,073 + lnStrtCol SAY laTotQty[5]   PICTURE '@Z 999999'
    @ ROW,080 + lnStrtCol SAY laTotQty[6]   PICTURE '@Z 999999'
    @ ROW,087 + lnStrtCol SAY laTotQty[7]   PICTURE '@Z 999999'
    @ ROW,094 + lnStrtCol SAY laTotQty[8]   PICTURE '@Z 999999'
    *E301800,1 BWA 01/21/2002 [END]

    *B802903,1 AMM end

    *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
    *@ ROW,098+lnStrtCol SAY XGTOTPCS PICTURE '9999999'     && YMA 05/03/94
    @ ROW,101 + lnStrtCol SAY XGTOTPCS PICTURE '9999999'
    *E301800,1 BWA 01/21/2002 [END]

    *- ARH 03/27/94 Changed QPW(14) with qCostPrv
    IF qCostPrv              && COSTING ACCESSS

      *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
      *@ ROW,106+lnStrtCol SAY XGTOTAMT PICTURE '9999999.99' && YMA 05/03/94
      IF llMultiWH AND !llRpDec
        @ ROW,109 + lnStrtCol SAY XGTOTAMT PICTURE '9999999'
      ELSE
        @ ROW,109 + lnStrtCol SAY XGTOTAMT PICTURE '9999999.99'
      ENDIF
      *E301800,1 BWA 01/21/2002 [END]

    ENDIF

    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    EXIT
  ENDIF

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*Locats STYLE        COLOR  DYELOT #   REFERENCE    TY DV   SZ1   SZ2   SZ3   SZ4   SZ5   SZ6   SZ7   SZ8  TOTQTY  TOT. COST DATE
*123456 123456789012 123456 XXXXXXXXXX 1234567890123 A 12 12345 12345 12345 12345 12345 12345 12345 12345 1234567 1234567.99 99/99/99
*                                                    P

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*STYLE        COLOR  DYELOT #   REFERENCE    TY DV   SZ1   SZ2   SZ3   SZ4   SZ5   SZ6   SZ7   SZ8  TOTQTY  TOT. COST DATE
*123456789012 123456 XXXXXXXXXX 1234567890123 A 12 12345 12345 12345 12345 12345 12345 12345 12345 1234567 1234567.99 99/99/99
*                                                    P

  IF llRpShow

    IF llMultiWH
      *B603229,1 Abd - Add Cheak if the type of transaction is transfer
      *B603229,1 Abd - and total Qty < 0 that you transfer from this wearhose.
      *B603229,1 Abd - else you will recive to this wearhouse. [Begin]
      *@ ROW,00 SAY IIF(EMPTY(laOGFxFlt[lnLocPos,6]), cFromWare, laOGFxFlt[lnLocPos,6])      
      DO CASE
        CASE TYPE = 'T'
          IF  TOTADJ < 0
            @ ROW,00 SAY IIF(EMPTY(laOGFxFlt[lnLocPos,6]), cFromWare, laOGFxFlt[lnLocPos,6])
          ELSE
            @ ROW,00 SAY CtoWare            
          ENDIF
        OTHERWISE
          @ ROW,00 SAY IIF(EMPTY(laOGFxFlt[lnLocPos,6]), cFromWare, laOGFxFlt[lnLocPos,6])        
       ENDCASE 
      *B603229,1 Abd - [End]
    ENDIF

    @ ROW,000+lnStrtCol SAY STYLE
*    @ ROW,013+lnStrtCol SAY COLOR

    IF XDYELOT_S
      @ ROW,020+lnStrtCol SAY DYELOT
    ENDIF
    *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
    *@ ROW,031 + lnStrtCol SAY SUBSTR(cREASON,1,11)
    *@ ROW,043 + lnStrtCol SAY TYPE
    *@ ROW,045 + lnStrtCol SAY STYLE.CDIVISION  PICT '!!'

    @ ROW,029 + lnStrtCol SAY SUBSTR(cREASON,1,11)
    @ ROW,040 + lnStrtCol SAY TYPE
    @ ROW,042 + lnStrtCol SAY STYLE.CDIVISION  PICT '!!'
    *E301800,1 BWA 01/21/2002 [END]

    *B803033,1 BWA 29/02/2000 Fix the bug of Incorrect reporting of totals in adjustment journal.[START]
    *@ ROW,050+lnStrtCol SAY ADJ1   PICTURE '@Z 99999'
    *@ ROW,056+lnStrtCol SAY ADJ2   PICTURE '@Z 99999'
    *@ ROW,062+lnStrtCol SAY ADJ3   PICTURE '@Z 99999'
    *@ ROW,068+lnStrtCol SAY ADJ4   PICTURE '@Z 99999'
    *@ ROW,074+lnStrtCol SAY ADJ5   PICTURE '@Z 99999'
    *@ ROW,080+lnStrtCol SAY ADJ6   PICTURE '@Z 99999'
    *@ ROW,086+lnStrtCol SAY ADJ7   PICTURE '@Z 99999'
    *@ ROW,092+lnStrtCol SAY ADJ8   PICTURE '@Z 99999'
    *@ ROW,098+lnStrtCol SAY TOTADJ PICTURE '@Z 9999999'
    

    *B603229,1 Abd - We will print the recored also if type 
    *B603229,1 Abd - Of transaction is transfer. [Begin]
    *IF TYPE = 'A'
    IF TYPE $ 'TA'
      *B603229,1 Abd - [End]
      *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
      *@ ROW,050+lnStrtCol SAY ADJ1   PICTURE '@Z 99999'
      *@ ROW,056+lnStrtCol SAY ADJ2   PICTURE '@Z 99999'
      *@ ROW,062+lnStrtCol SAY ADJ3   PICTURE '@Z 99999'
      *@ ROW,068+lnStrtCol SAY ADJ4   PICTURE '@Z 99999'
      *@ ROW,074+lnStrtCol SAY ADJ5   PICTURE '@Z 99999'
      *@ ROW,080+lnStrtCol SAY ADJ6   PICTURE '@Z 99999'
      *@ ROW,086+lnStrtCol SAY ADJ7   PICTURE '@Z 99999'
      *@ ROW,092+lnStrtCol SAY ADJ8   PICTURE '@Z 99999'
      *@ ROW,098+lnStrtCol SAY TOTADJ PICTURE '@Z 9999999'

      @ ROW,045 + lnStrtCol SAY ADJ1   PICTURE '@Z 999999'
      @ ROW,052 + lnStrtCol SAY ADJ2   PICTURE '@Z 999999'
      @ ROW,059 + lnStrtCol SAY ADJ3   PICTURE '@Z 999999'
      @ ROW,066 + lnStrtCol SAY ADJ4   PICTURE '@Z 999999'
      @ ROW,073 + lnStrtCol SAY ADJ5   PICTURE '@Z 999999'
      @ ROW,080 + lnStrtCol SAY ADJ6   PICTURE '@Z 999999'
      @ ROW,087 + lnStrtCol SAY ADJ7   PICTURE '@Z 999999'
      @ ROW,094 + lnStrtCol SAY ADJ8   PICTURE '@Z 999999'
      @ ROW,101 + lnStrtCol SAY TOTADJ PICTURE '@Z 9999999'
      *E301800,1 BWA 01/21/2002 [END]

    ENDIF

    *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [Start]
    *IF TYPE = 'P'
    IF TYPE $ 'LP'
      *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [End  ]
      *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
      *@ ROW,050+lnStrtCol SAY ADJ1-OldQty1   PICTURE '@Z 99999'
      *@ ROW,056+lnStrtCol SAY ADJ2-OldQty2   PICTURE '@Z 99999'
      *@ ROW,062+lnStrtCol SAY ADJ3-OldQty3   PICTURE '@Z 99999'
      *@ ROW,068+lnStrtCol SAY ADJ4-OldQty4   PICTURE '@Z 99999'
      *@ ROW,074+lnStrtCol SAY ADJ5-OldQty5   PICTURE '@Z 99999'
      *@ ROW,080+lnStrtCol SAY ADJ6-OldQty6   PICTURE '@Z 99999'
      *@ ROW,086+lnStrtCol SAY ADJ7-OldQty7   PICTURE '@Z 99999'
      *@ ROW,092+lnStrtCol SAY ADJ8-OldQty8   PICTURE '@Z 99999'
      *@ ROW,098+lnStrtCol SAY TOTADJ-TotOld PICTURE '@Z 9999999'

      @ ROW,045 + lnStrtCol SAY ADJ1-OldQty1   PICTURE '@Z 999999'
      @ ROW,052 + lnStrtCol SAY ADJ2-OldQty2   PICTURE '@Z 999999'
      @ ROW,059 + lnStrtCol SAY ADJ3-OldQty3   PICTURE '@Z 999999'
      @ ROW,066 + lnStrtCol SAY ADJ4-OldQty4   PICTURE '@Z 999999'
      @ ROW,073 + lnStrtCol SAY ADJ5-OldQty5   PICTURE '@Z 999999'
      @ ROW,080 + lnStrtCol SAY ADJ6-OldQty6   PICTURE '@Z 999999'
      @ ROW,087 + lnStrtCol SAY ADJ7-OldQty7   PICTURE '@Z 999999'
      @ ROW,094 + lnStrtCol SAY ADJ8-OldQty8   PICTURE '@Z 999999'
      @ ROW,101 + lnStrtCol SAY TOTADJ-TotOld  PICTURE '@Z 9999999'
      *E301800,1 BWA 01/21/2002 [END]

    ENDIF  
    *B803033,1 BWA 29/02/2000 [END]

    IF qCostPrv              && COSTING ACCESS.
      *B803033,1 BWA 29/02/2000 Fix the bug of Incorrect reporting of totals in adjustment journal.[START]
      *@ ROW,106+lnStrtCol SAY TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) ;
                          PICTURE '9999999.99'

      *B603229,1 Abd - We will print the recored also if type 
      *B603229,1 Abd - Of transaction is transfer. [Begin]
      *IF TYPE = 'A'
      IF TYPE $ 'TA'
        *B603229,1 Abd - [End]
        *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
        *@ ROW,106+lnStrtCol SAY TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) ;
                               PICTURE '9999999.99'
        IF llMultiWH AND !llRpDec
          @ ROW,109 + lnStrtCol SAY TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) ;
                                   PICTURE '9999999'
        ELSE
          @ ROW,109 + lnStrtCol SAY TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) ;
                                   PICTURE '9999999.99'
        ENDIF
        *E301800,1 BWA 01/21/2002 [END]

      ENDIF
      *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [Start]
      *IF TYPE = 'P'
      IF TYPE $ 'LP'
        *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [End  ]
        *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
        *@ ROW,106+lnStrtCol SAY TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) ;
                               -TotOld*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST) ;
                                PICTURE '9999999.99'
        IF llMultiWH AND !llRpDec
          @ ROW,109 + lnStrtCol SAY TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) ;
                                   -TotOld*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST) ;
                                    PICTURE '9999999'
        ELSE
          @ ROW,109 + lnStrtCol SAY TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) ;
                                   -TotOld*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST) ;
                                    PICTURE '9999999.99'
        ENDIF
        *E301800,1 BWA 01/21/2002 [END]
      ENDIF
      *B803033,1 BWA 29/02/2000 [END]
    ENDIF

    *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
    *@ ROW,117+lnStrtCol SAY DATE
    IF llMultiWH AND !llRpDec
      @ ROW,117+lnStrtCol SAY DATE
    ELSE
      @ ROW,120+lnStrtCol SAY DATE
    ENDIF
    *E301800,1 BWA 01/21/2002 [END]

    ROW = ROW + 1
  ENDIF

  *-- UPDATE THE SUBTOTALS

  *B605858,1 BWA 04/22/2002 Add the calcultion of the transfere in case there is a warehouse in the criteria.[START]
  *IF TYPE = 'A'                                                    && SUB TOTAL ON ADJUSTMENTS ONLY
  IF IIF(EMPTY(laOGFxFlt[lnLocPos,6]) , TYPE = 'A' , TYPE $ 'TA' )  && SUB TOTAL ON ADJUSTMENTS AND TRANSFERE
  *B605858,1 BWA 04/22/2002.[END]

    XSTOTPCS = XSTOTPCS + TOTADJ
    XSTOTAMT = XSTOTAMT + (TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST))
    XGTOTPCS = XGTOTPCS + TOTADJ
    XGTOTAMT = XGTOTAMT + (TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST))
    *B802903,1 AMM Calculate sub and grand total quantities
    FOR lnC = 1 TO 8
      lcC = STR(lnC,1)
      laAdjQty[lnC] = laAdjQty[lnC] + EVAL('Adj'+lcC)
      laTotQty[lnC] = laTotQty[lnC] + EVAL('Adj'+lcC)
    ENDFOR
    *B802903,1 AMM end
  ENDIF
  *B802903,1 AMM Calculate totals in physical inventory case
  *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [Start]
  *IF TYPE = 'P'
  IF TYPE $ 'LP'
  *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [End  ]
    XSTOTPCS  = XSTOTPCS + TOTADJ-TotOld
    FOR lnC = 1 TO 8
      lCC = STR(lnC,1)
      laAdjQty[lnC] = laAdjQty[lnC] + EVAL('Adj'+lcC) - EVAL('OldQty'+lcC)
      laTotQty[lnC] = laTotQty[lnC] + EVAL('Adj'+lcC) - EVAL('OldQty'+lcC)
    ENDFOR
    XSTOTAMT = XSTOTAMT + (TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST)) - (TotOld*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST))
    XGTOTPCS  = XGTOTPCS + TOTADJ-TotOld
    XGTOTAMT = XGTOTAMT + (TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST)) - (TotOld*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST))
  ENDIF
  *B802903,1 AMM end
  SKIP
ENDDO

********************************************************************

******* INVENTORY ADJUSTMENT JOURNAL THE LONG FOR THE NEW ONE ******

********************************************************************

DO WHILE lcRpFormat='L' .AND. lcRpRepF='A' && Long form 'A' the new one.

  IF !llMultiWH .AND. Type = 'T'
    SKIP
    LOOP
  ENDIF

  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    ROW = 5

    @ ROW,00 SAY IIF(llMultiWH ,'Locats ' ,'')+lcStyTitle + ' ' +;
                 IIF(XDYELOT_S,'DYELOT #',SPACE(8))                   +;
                 '   DV     TY              SZ1    SZ2    SZ3    SZ4' +;
                 '    SZ5    SZ6    SZ7    SZ8  TOTQTY  TOT. COST'  

    ROW = ROW + 1
    
    *B605975,1 BWA 09/07/2002 Fix the bug of printing the wrong title for the reason column.[START]
    *@ ROW,00 SAY 'ADJUSTMENT REFERENCE       DATE'
    @ ROW,00 SAY 'ADJUSTMENT REASON          DATE'
    *B605975,1 BWA 09/07/2002.[END]

    *E302212,1  TMI [Start] Add a line showing Scales
    @ ROW,31 SAY IIF(llMultiWH ,'       ' ,'')+SPACE(LEN(lcStyTitle)) + ' ' +;
                 SPACE(3) +;
                 SCALE.SZ1+'  '+SCALE.SZ2+'  '+SCALE.SZ3+'  '+SCALE.SZ4+'  '+;
                 SCALE.SZ5+'  '+SCALE.SZ6+'  '+SCALE.SZ7+'  '+SCALE.SZ8
    *E302212,1  TMI [End  ] 

    ROW = ROW + 1
  ENDIF

  IF PADR(STYLE,lnMajorLen) <> XSTYLE
    @ ROW,00 SAY REPLICATE('.',132)
    ROW = ROW + 1
    @ ROW,000           SAY '*** ADJUSTMENTS SUBTOTAL *** ' + XSTYLE
    *B802903,1 AMM Print Sub total quantities
    @ ROW,051+lnStrtCol SAY laAdjQty[1] PICTURE '@Z 999999'
    @ ROW,058+lnStrtCol SAY laAdjQty[2] PICTURE '@Z 999999'
    @ ROW,065+lnStrtCol SAY laAdjQty[3] PICTURE '@Z 999999'
    @ ROW,072+lnStrtCol SAY laAdjQty[4] PICTURE '@Z 999999'
    @ ROW,079+lnStrtCol SAY laAdjQty[5] PICTURE '@Z 999999'
    @ ROW,086+lnStrtCol SAY laAdjQty[6] PICTURE '@Z 999999'
    @ ROW,093+lnStrtCol SAY laAdjQty[7] PICTURE '@Z 999999'
    @ ROW,100+lnStrtCol SAY laAdjQty[8] PICTURE '@Z 999999'
    *B802903,1 AMM end

    @ ROW,107+lnStrtCol SAY XSTOTPCS PICTURE '9999999'

    IF qCostPrv              && COSTING ACCESS
      @ ROW,115+lnStrtCol SAY XSTOTAMT PICTURE '9999999.99'
    ENDIF

    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('.',132)
    ROW = ROW + 1
    XSTYLE   = PADR(STYLE,lnMajorLen)
    XSTOTPCS  = 0
    XSTOTAMT  = 0
    *B802903,1 AMM Reinitialize
    laAdjQty = 0
    *B802903,1 AMM end
    *E302212,1  TMI [Start] if Scale is changed , show the new scale &&>>> 
    IF SCALE.SCALE <> XSCALE .AND. !EOF()
      XSCALE = SCALE.SCALE
      @ ROW,00 SAY IIF(llMultiWH ,'       ' ,'')+SPACE(LEN(lcStyTitle)) + ' ' +;
                   SPACE(8)                   +'                          '+;
                   SCALE.SZ1+'  '+SCALE.SZ2+'  '+SCALE.SZ3+'  '+SCALE.SZ4+'  '+;
                   SCALE.SZ5+'  '+SCALE.SZ6+'  '+SCALE.SZ7+'  '+SCALE.SZ8  
      ROW = ROW + 1
    ENDIF
    *E302212,1  TMI [End  ] 
  ENDIF

  IF EOF()
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    @ ROW,000           SAY '*** ADJUSTMENTS GRAND TOTAL ***'
    *B802903,1 AMM Print Grand totals
    @ ROW,051+lnStrtCol SAY laTotQty[1] PICTURE '@Z 999999'
    @ ROW,058+lnStrtCol SAY laTotQty[2] PICTURE '@Z 999999'
    @ ROW,065+lnStrtCol SAY laTotQty[3] PICTURE '@Z 999999'
    @ ROW,072+lnStrtCol SAY laTotQty[4] PICTURE '@Z 999999'
    @ ROW,079+lnStrtCol SAY laTotQty[5] PICTURE '@Z 999999'
    @ ROW,086+lnStrtCol SAY laTotQty[6] PICTURE '@Z 999999'
    @ ROW,093+lnStrtCol SAY laTotQty[7] PICTURE '@Z 999999'
    @ ROW,100+lnStrtCol SAY laTotQty[8] PICTURE '@Z 999999'
    *B802903,1 AMM end

    @ ROW,107+lnStrtCol SAY XGTOTPCS PICTURE '9999999'       && YMA 05/03/94

    IF qCostPrv              && COSTING ACCESS.
      @ ROW,115+lnStrtCol SAY XGTOTAMT PICTURE '9999999.99'  && YMA 05/03/94
    ENDIF

    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    EXIT
  ENDIF

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*STYLE        COLOR  DYELOT #   DV TY                  SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY  TOT. COST
*ADJUSTMENT REFERENCE       DATE
*123456789012 123456 XXXXXXXXXX 12 X EXISTING INVT: 123456 123456 123456 123456 123456 123456 123456 123456 1234567 1234567.99
*1234567890123456789012345  99/99/99 ADJUSTED INVT:
*                                    NEW INVENTORY:


*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*Locat STYLE        COLOR  DYELOT #   DV TY                  SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY  TOT. COST
*ADJUSTMENT REFERENCE       DATE
*123456 123456789012 123456 XXXXXXXXXX 12 X EXISTING INVT: 123456 123456 123456 123456 123456 123456 123456 123456 1234567 1234567.99
*1234567890123456789012345  99/99/99        ADJUSTED INVT:
*                                           NEW INVENTORY:
*                                           FROM.. 123456:
*                                           TRANSFERED...:
*                                           TO.... 123456:


  IF llRpShow

    IF llMultiWH
      @ ROW,00 SAY IIF(EMPTY(laOGFxFlt[lnLocPos,6]), cFromWare, laOGFxFlt[lnLocPos,6])
    ENDIF

    @ ROW,000+lnStrtCol SAY STYLE
*    @ ROW,013+lnStrtCol SAY COLOR

    IF XDYELOT_S
      @ ROW,020+lnStrtCol SAY DYELOT
    ENDIF

    @ ROW,031+lnStrtCol SAY STYLE.CDIVISION
    @ ROW,038+lnStrtCol SAY TYPE

    IF TYPE = 'T'
      *-- Remove the subtraction of the adjustment quantity from the old quantity.
      @ ROW,040+lnStrtCol SAY 'FR. ' + cFromWare + ':'
      @ ROW,051+lnStrtCol SAY OldQty1 PICTURE '@Z 999999'
      @ ROW,058+lnStrtCol SAY OldQty2 PICTURE '@Z 999999'
      @ ROW,065+lnStrtCol SAY OldQty3 PICTURE '@Z 999999'
      @ ROW,072+lnStrtCol SAY OldQty4 PICTURE '@Z 999999'
      @ ROW,079+lnStrtCol SAY OldQty5 PICTURE '@Z 999999'
      @ ROW,086+lnStrtCol SAY OldQty6 PICTURE '@Z 999999'
      @ ROW,093+lnStrtCol SAY OldQty7 PICTURE '@Z 999999'
      @ ROW,100+lnStrtCol SAY OldQty8 PICTURE '@Z 999999'
      @ ROW,107+lnStrtCol SAY TotOld  PICTURE '@Z 9999999'

    ELSE
 
      @ ROW,040+lnStrtCol SAY 'EXIST INV:'
      @ ROW,051+lnStrtCol SAY OLDQTY1 PICTURE '@Z 999999'
      @ ROW,058+lnStrtCol SAY OLDQTY2 PICTURE '@Z 999999'
      @ ROW,065+lnStrtCol SAY OLDQTY3 PICTURE '@Z 999999'
      @ ROW,072+lnStrtCol SAY OLDQTY4 PICTURE '@Z 999999'
      @ ROW,079+lnStrtCol SAY OLDQTY5 PICTURE '@Z 999999'
      @ ROW,086+lnStrtCol SAY OLDQTY6 PICTURE '@Z 999999'
      @ ROW,093+lnStrtCol SAY OLDQTY7 PICTURE '@Z 999999'
      @ ROW,100+lnStrtCol SAY OLDQTY8 PICTURE '@Z 999999'
      @ ROW,107+lnStrtCol SAY TOTOLD  PICTURE '@Z 9999999'
    ENDIF

    IF qCostPrv                      && COSTING ACCESS.
      @ ROW,115+lnStrtCol SAY TOTOLD*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST) ;
                          PICTURE '9999999.99'
    ENDIF

    ROW = ROW + 1
    @ ROW,000           SAY cREASON
    @ ROW,027           SAY DATE

    IF TYPE = 'T'
      @ ROW,040+lnStrtCol SAY 'TRANSFER.:'
    ELSE
      @ ROW,040+lnStrtCol SAY 'ADJ. INV.:'
    ENDIF
    *B802903,1 AMM If physical inventory print the difference between old and new inventory
    *@ ROW,051+lnStrtCol SAY ADJ1   PICTURE '@Z 999999'
    *@ ROW,058+lnStrtCol SAY ADJ2   PICTURE '@Z 999999'
    *@ ROW,065+lnStrtCol SAY ADJ3   PICTURE '@Z 999999'
    *@ ROW,072+lnStrtCol SAY ADJ4   PICTURE '@Z 999999'
    *@ ROW,079+lnStrtCol SAY ADJ5   PICTURE '@Z 999999'
    *@ ROW,086+lnStrtCol SAY ADJ6   PICTURE '@Z 999999'
    *@ ROW,093+lnStrtCol SAY ADJ7   PICTURE '@Z 999999'
    *@ ROW,100+lnStrtCol SAY ADJ8   PICTURE '@Z 999999'
    *@ ROW,107+lnStrtCol SAY TOTADJ PICTURE '@Z 9999999'
    *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [Start]
    *IF TYPE = 'P'
    IF TYPE $ 'LP'
    *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [End  ]
      @ ROW,051+lnStrtCol SAY ADJ1-OldQty1   PICTURE '@Z 999999'
      @ ROW,058+lnStrtCol SAY ADJ2-OldQty2   PICTURE '@Z 999999'
      @ ROW,065+lnStrtCol SAY ADJ3-OldQty3   PICTURE '@Z 999999'
      @ ROW,072+lnStrtCol SAY ADJ4-OldQty4   PICTURE '@Z 999999'
      @ ROW,079+lnStrtCol SAY ADJ5-OldQty5   PICTURE '@Z 999999'
      @ ROW,086+lnStrtCol SAY ADJ6-OldQty6   PICTURE '@Z 999999'
      @ ROW,093+lnStrtCol SAY ADJ7-OldQty7   PICTURE '@Z 999999'
      @ ROW,100+lnStrtCol SAY ADJ8-OldQty8   PICTURE '@Z 999999'
      @ ROW,107+lnStrtCol SAY TOTADJ-TotOld PICTURE '@Z 9999999'
    ELSE
      @ ROW,051+lnStrtCol SAY ADJ1   PICTURE '@Z 999999'
      @ ROW,058+lnStrtCol SAY ADJ2   PICTURE '@Z 999999'
      @ ROW,065+lnStrtCol SAY ADJ3   PICTURE '@Z 999999'
      @ ROW,072+lnStrtCol SAY ADJ4   PICTURE '@Z 999999'
      @ ROW,079+lnStrtCol SAY ADJ5   PICTURE '@Z 999999'
      @ ROW,086+lnStrtCol SAY ADJ6   PICTURE '@Z 999999'
      @ ROW,093+lnStrtCol SAY ADJ7   PICTURE '@Z 999999'
      @ ROW,100+lnStrtCol SAY ADJ8   PICTURE '@Z 999999'
      @ ROW,107+lnStrtCol SAY TOTADJ PICTURE '@Z 9999999'
    ENDIF
    *B802903,1 AMM end
    IF qCostPrv                      && COSTING ACCESS.
      *B802903,1 AMM In physical inventory case, calculate on the absolute value
      *@ ROW,115+lnStrtCol SAY TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) ;
                          PICTURE '9999999.99'
      *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [Start]
      *@ ROW,115+lnStrtCol SAY TOTADJ *IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) - IIF(TYPE#'P',0, ;
      *                        (TotOld*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST))  ) ;
      *                    PICTURE '9999999.99'
      @ ROW,115+lnStrtCol SAY TOTADJ *IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) - IIF(!(TYPE$'LP') ,0, ;
                              (TotOld*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST))  ) ;
                          PICTURE '9999999.99'

      *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [End  ]
      *B802903,1 AMM end
    ENDIF

    ROW=ROW+1

    IF TYPE $ 'AT'
      XNEWQTY1 = IIF(Type = 'A', OLDQTY1, nOldTo1) + ADJ1
      XNEWQTY2 = IIF(Type = 'A', OLDQTY2, nOldTo2) + ADJ2
      XNEWQTY3 = IIF(Type = 'A', OLDQTY3, nOldTo3) + ADJ3
      XNEWQTY4 = IIF(Type = 'A', OLDQTY4, nOldTo4) + ADJ4
      XNEWQTY5 = IIF(Type = 'A', OLDQTY5, nOldTo5) + ADJ5
      XNEWQTY6 = IIF(Type = 'A', OLDQTY6, nOldTo6) + ADJ6
      XNEWQTY7 = IIF(Type = 'A', OLDQTY7, nOldTo7) + ADJ7
      XNEWQTY8 = IIF(Type = 'A', OLDQTY8, nOldTo8) + ADJ8
      XTOTNEW  = IIF(Type = 'A', TOTOLD , nTotOldTo) + TOTADJ
    ELSE
      XNEWQTY1 = ADJ1
      XNEWQTY2 = ADJ2
      XNEWQTY3 = ADJ3
      XNEWQTY4 = ADJ4
      XNEWQTY5 = ADJ5
      XNEWQTY6 = ADJ6
      XNEWQTY7 = ADJ7
      XNEWQTY8 = ADJ8
      XTOTNEW  = TOTADJ
    ENDIF

    IF Type = 'T'
      @ ROW,040+lnStrtCol SAY 'TO.'+ cToWare + ':'
    ELSE
      @ ROW,040+lnStrtCol SAY 'NEW INV..:'
    ENDIF
    @ ROW,051+lnStrtCol SAY XNEWQTY1 PICTURE '@Z 999999'
    @ ROW,058+lnStrtCol SAY XNEWQTY2 PICTURE '@Z 999999'
    @ ROW,065+lnStrtCol SAY XNEWQTY3 PICTURE '@Z 999999'
    @ ROW,072+lnStrtCol SAY XNEWQTY4 PICTURE '@Z 999999'
    @ ROW,079+lnStrtCol SAY XNEWQTY5 PICTURE '@Z 999999'
    @ ROW,086+lnStrtCol SAY XNEWQTY6 PICTURE '@Z 999999'
    @ ROW,093+lnStrtCol SAY XNEWQTY7 PICTURE '@Z 999999'
    @ ROW,100+lnStrtCol SAY XNEWQTY8 PICTURE '@Z 999999'
    @ ROW,107+lnStrtCol SAY XTOTNEW  PICTURE '@Z 9999999'

    IF qCostPrv              && COSTING ACCESS.
      @ ROW,115+lnStrtCol SAY XTOTNEW*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) ;
                          PICTURE '9999999.99'
    ENDIF
    ROW=ROW+1

  ENDIF

  *-- UPDATE THE SUBTOTALS
  IF TYPE = 'A'                          && SUB TOTAL ON ADJUSTMENTS ONLY
    XSTOTPCS  = XSTOTPCS + TOTADJ
    *B802903,1 AMM Calculate Sub and grand total quantities
    FOR lnC = 1 TO 8
      lcC = STR(lnC,1)
      laAdjQty[lnC] = laAdjQty[lnC]  + EVAL('Adj'+lcC)
      laTotQty[lnC] = laTotQty[lnC]  + EVAL('Adj'+lcC)
    ENDFOR
    *B802903,1 AMM end
    XSTOTAMT  = XSTOTAMT+(TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST))
    XGTOTPCS  = XGTOTPCS + TOTADJ
    XGTOTAMT  = XGTOTAMT+(TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST ))
  ENDIF
  *B802903,1 AMM Calculate totals in physical inventory case
  *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [Start]
  *IF TYPE = 'P'
  IF TYPE $ 'LP'
  *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [End  ]
    XSTOTPCS  = XSTOTPCS + TOTADJ-TotOld
    FOR lnC = 1 TO 8
      lcC = STR(lnC,1)
      laAdjQty[lnC] = laAdjQty[lnC] + EVAL('Adj'+lcC) - EVAL('OldQty'+lcC)
      laTotQty[lnC] = laTotQty[lnC] + EVAL('Adj'+lcC) - EVAL('OldQty'+lcC)
    ENDFOR
    XSTOTAMT = XSTOTAMT + (TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST)) - (TotOld*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST))
    XGTOTPCS  = XGTOTPCS + TOTADJ-TotOld
    XGTOTAMT = XGTOTAMT + (TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST)) - (TotOld*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST))
  ENDIF
  *B802903,1 AMM end
  SKIP
ENDDO

IF lcRpFormat='L' .AND. lcRpRepF='B'        && Long form 'B'
  DO lpLongB
ENDIF
*-- end of lpPrnData.

*!*************************************************************
*! Name      : lpLongB
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Print Long report form B
*!*************************************************************
*! Called from : Print data.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpLongB
*!*************************************************************
*!B602342,1 MAB 12/14/1998, Typ is now aligned with its column in form B
*!
PROCEDURE lpLongB

******* INVENTORY ADJUSTMENT JOURNAL THE LONG FOR THE NEW ONE ******
DO WHILE .T.

  IF !llMultiWH .AND. Type = 'T'
    SKIP
    LOOP
  ENDIF

  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH  XREPORT,XTITLE,R_WIDTH
    ROW = 5

    *B602342,1 Change division title to Divisn and the suppress one char. after Typ, Header now 
    *B602342,1 aligned with its columns.
    *@ ROW,01 SAY lcStyTitle + ' ' + 'Di TYP                 SZ1   SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY    TOT. COST  DATE'
    @ ROW,01 SAY lcStyTitle + ' ' + 'Divisn TYP                  SZ1   SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY    TOT. COST  DATE'

    ROW = ROW + 1
    *E302212,1  TMI [Start] Add a line showing Scales
    @ ROW,01 SAY SPACE(LEN(lcStyTitle)) + ' ' + '                            '+;
                 SCALE.SZ1+'  '+SCALE.SZ2+'  '+SCALE.SZ3+'  '+SCALE.SZ4+'  '+;
                 SCALE.SZ5+'  '+SCALE.SZ6+'  '+SCALE.SZ7+'  '+SCALE.SZ8
    ROW = ROW + 1
    *E302212,1  TMI [End  ] 
  
  ENDIF

  IF PADR(STYLE,lnMajorLen) <> XSTYLE
    @ ROW,00 SAY REPLICATE('.',130)
    *B803556,1 AMH Replace with new Arrays [Start]
    *ROW = ROW + 1
    *@ ROW,000 SAY '*** ADJUSTMENTS  SUBTOTAL ***'+XSTYLE
    *B802903,1 AMM Print sub total size quantities
    *@ ROW,045 SAY laAdjQty[1]   PICTURE '@Z 999999'
    *@ ROW,052 SAY laAdjQty[2]   PICTURE '@Z 999999'
    *@ ROW,059 SAY laAdjQty[3]   PICTURE '@Z 999999'
    *@ ROW,066 SAY laAdjQty[4]   PICTURE '@Z 999999'
    *@ ROW,073 SAY laAdjQty[5]   PICTURE '@Z 999999'
    *@ ROW,080 SAY laAdjQty[6]   PICTURE '@Z 999999'
    *@ ROW,087 SAY laAdjQty[7]   PICTURE '@Z 999999'
    *@ ROW,094 SAY laAdjQty[8]   PICTURE '@Z 999999'
    *B802903,1 AMM end
    *@ ROW,101 SAY XSTOTPCS PICTURE '9999999'
    *IF qCostPrv              && COSTING ACCESS. 1/30/92
    *  @ ROW,111 SAY XSTOTAMT PICTURE '9999999.99'
    *ENDIF
    *
    *ROW = ROW + 1
    *@ ROW,000 SAY '***   PHYSICAL SUBTOTAL   ***'+XSTYLE
    *B802903,1 AMM Print sub total size quantities
    *@ ROW,045 SAY laPhyQty[1]   PICTURE '@Z 999999'
    *@ ROW,052 SAY laPhyQty[2]   PICTURE '@Z 999999'
    *@ ROW,059 SAY laPhyQty[3]   PICTURE '@Z 999999'
    *@ ROW,066 SAY laPhyQty[4]   PICTURE '@Z 999999'
    *@ ROW,073 SAY laPhyQty[5]   PICTURE '@Z 999999'
    *@ ROW,080 SAY laPhyQty[6]   PICTURE '@Z 999999'
    *@ ROW,087 SAY laPhyQty[7]   PICTURE '@Z 999999'
    *@ ROW,094 SAY laPhyQty[8]   PICTURE '@Z 999999'
    *B802903,1 AMM end
    *@ ROW,101 SAY XSTOTPhPCS PICTURE '9999999'
    *IF qCostPrv              && COSTING ACCESS. 1/30/92
    *  @ ROW,111 SAY XSTOTPhAMT PICTURE '9999999.99'
    *ENDIF
    FOR lnCnt = 1 TO 4
      ROW = ROW + 1
      lcTitle = IIF(lnCnt=1,'*** ADJUSTMENTS  SUBTOTAL ***',;
                IIF(lnCnt=2,'***  TRANSFERS  SUBTOTAL  ***',;
                IIF(lnCnt=3,'***   LOCKINGS SUBTOTAL   ***','***   PHYSICAL SUBTOTAL   ***')))
      @ ROW,000 SAY lcTitle+XSTYLE
      FOR I = 1 TO 8
        @ ROW,038+(I*7) SAY laAllQty[I,lnCnt]   PICTURE '@Z 999999'
      ENDFOR
      @ ROW,101 SAY XSTOTAlPCS[lnCnt] PICTURE '9999999'
      IF qCostPrv              && COSTING ACCESS. 1/30/92
        @ ROW,111 SAY XSTOTAlAMT[lnCnt] PICTURE '9999999.99'
      ENDIF
    ENDFOR
    *B803556,1 AMH Replace with new Arrays [End  ]
    
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('.',130)
    ROW = ROW + 1
    XSTYLE   = PADR(STYLE,lnMajorLen)
    XSTOTPCS  = 0
    XSTOTAMT  = 0
    XSTOTPhPCS  = 0
    XSTOTPhAMT  = 0
    *B802903,1 AMM Initialize
    laAdjQty = 0
    *B803556,1 AMH Replace with new Arrays [Start]
    *laPhyQty = 0
    laAllQty = 0
    XSTOTAlPCS = 0
    XSTOTAlAMT = 0
    *B803556,1 AMH Replace with new Arrays [End  ]
    *B802903,1 AMM end
    *E302212,1  TMI [Start] if Scale is changed , show the new scale &&>>> 
    IF SCALE.SCALE <> XSCALE .AND. !EOF()
      XSCALE = SCALE.SCALE
      @ ROW,01 SAY SPACE(LEN(lcStyTitle)) + ' ' + '                            '+;
                   SCALE.SZ1+'  '+SCALE.SZ2+'  '+SCALE.SZ3+'  '+SCALE.SZ4+'  '+;
                   SCALE.SZ5+'  '+SCALE.SZ6+'  '+SCALE.SZ7+'  '+SCALE.SZ8  
      ROW = ROW + 1
    ENDIF
    *E302212,1  TMI [End  ] 
  ENDIF

  IF EOF()
    @ ROW,00 SAY REPLICATE('-',130)
    ROW = ROW + 1
    *B803556,1 AMH Replace with new Arrays [Start]
    *@ ROW,000 SAY '*** ADJUSTMENTS  GRAND TOTAL ***'

    *B802903,1 AMM Print sub total size quantities of inventory adjustment
    *@ ROW,045 SAY laTotQty[1]   PICTURE '@Z 999999'
    *@ ROW,052 SAY laTotQty[2]   PICTURE '@Z 999999'
    *@ ROW,059 SAY laTotQty[3]   PICTURE '@Z 999999'
    *@ ROW,066 SAY laTotQty[4]   PICTURE '@Z 999999'
    *@ ROW,073 SAY laTotQty[5]   PICTURE '@Z 999999'
    *@ ROW,080 SAY laTotQty[6]   PICTURE '@Z 999999'
    *@ ROW,087 SAY laTotQty[7]   PICTURE '@Z 999999'
    *@ ROW,094 SAY laTotQty[8]   PICTURE '@Z 999999'
    *B802903,1 AMM end
    *@ ROW,101 SAY XGTOTPCS PICTURE '9999999'

    *IF qCostPrv             && COSTING ACCESS. 1/30/92
    *  @ ROW,111 SAY XGTOTAMT PICTURE '9999999.99'
    *ENDIF
    
    *ROW = ROW + 1
    *@ ROW,000 SAY '***   PHYSICAL GRAND TOTAL   ***'
    *B802903,1 AMM Print sub total size quantities of physical inventory
    *@ ROW,045 SAY laPTotQty[1]   PICTURE '@Z 999999'
    *@ ROW,052 SAY laPTotQty[2]   PICTURE '@Z 999999'
    *@ ROW,059 SAY laPTotQty[3]   PICTURE '@Z 999999'
    *@ ROW,066 SAY laPTotQty[4]   PICTURE '@Z 999999'
    *@ ROW,073 SAY laPTotQty[5]   PICTURE '@Z 999999'
    *@ ROW,080 SAY laPTotQty[6]   PICTURE '@Z 999999'
    *@ ROW,087 SAY laPTotQty[7]   PICTURE '@Z 999999'
    *@ ROW,094 SAY laPTotQty[8]   PICTURE '@Z 999999'
    *B802903,1 AMM end

    *@ ROW,101 SAY xGTotPhPcs PICTURE '9999999'
    *IF qCostPrv              && COSTING ACCESS. 1/30/92
    *  @ ROW,111 SAY xGTotPhAmt PICTURE '9999999.99'
    *ENDIF
    *ROW = ROW + 1
    FOR lnCnt = 1 TO 4
      ROW = ROW + 1
      lcTitle = IIF(lnCnt=1,'*** ADJUSTMENTS  GRAND TOTAL ***',;
                IIF(lnCnt=2,'***  TRANSFERS  GRAND TOTAL  ***',;
                IIF(lnCnt=3,'***   LOCKINGS GRAND TOTAL   ***','***   PHYSICAL GRAND TOTAL   ***')))
      @ ROW,000 SAY lcTitle+XSTYLE
      FOR I = 1 TO 8
        @ ROW,038+(I*7) SAY laAlTotQty[I,lnCnt]   PICTURE '@Z 999999'
      ENDFOR
      @ ROW,101 SAY XGTOTAlPCS[lnCnt] PICTURE '9999999'
      IF qCostPrv              && COSTING ACCESS. 1/30/92
        @ ROW,111 SAY XGTOTAlAMT[lnCnt] PICTURE '9999999.99'
      ENDIF
    ENDFOR
    *B803556,1 AMH Replace with new Arrays [End  ]
    
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',130)
    ROW = ROW + 1
    EXIT
  ENDIF

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
*STYLE   CLR DYELOT #   DV TYP                     SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY TOT. COST  DATE
*ADJUSTMENT REFERENCE
*1234567 123 XXXXXXXXXX 12 XXX EXISTING INVT:-> 123456 123456 123456 123456 123456 123456 123456 123456 1234567 1234567.99 99/99/99
*1234567890123456789012345     ADJUSTED INVT:->
*                              NEW INVENTORY:->
  IF llRpShow
    @ ROW,001 SAY STYLE

    @ ROW,021 SAY STYLE.CDIVISION
    *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [Start]
    *@ ROW,028 SAY IIF(TYPE='A','ADJ','PHY')
    @ ROW,028 SAY IIF(TYPE='A','ADJ',IIF(TYPE='T','TRN',IIF(TYPE='L','LCK','PHY')))
    *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [End  ]

    @ ROW,032 SAY 'EXISTING INVT:'

    @ ROW,045 SAY OLDQTY1 PICTURE '@Z 999999'
    @ ROW,052 SAY OLDQTY2 PICTURE '@Z 999999'
    @ ROW,059 SAY OLDQTY3 PICTURE '@Z 999999'
    @ ROW,066 SAY OLDQTY4 PICTURE '@Z 999999'
    @ ROW,073 SAY OLDQTY5 PICTURE '@Z 999999'
    @ ROW,080 SAY OLDQTY6 PICTURE '@Z 999999'
    @ ROW,087 SAY OLDQTY7 PICTURE '@Z 999999'
    @ ROW,094 SAY OLDQTY8 PICTURE '@Z 999999'

    @ ROW,101 SAY TOTOLD PICTURE '@Z 9999999'
    IF qCostPrv                      && COSTING ACCESS. 1/30/92
      ***Modified by ARH on 11/03/93 to put the old unit cost into
      ***consideration when calculating the cost if the style
      ***costing method was by AVERAGE
      @ ROW,111 SAY TOTOLD*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST) ;
                    PICTURE '9999999.99'
    ENDIF
    ROW = ROW + 1
    @ ROW,01 SAY cREASON
    @ ROW,032 SAY 'ADJUSTED INVT:'

    *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [Start]
    *IF TYPE = 'P'
    IF TYPE $ 'LP'
    *B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment [End  ]
      @ ROW,045 SAY ADJ1-OLDQTY1   PICTURE '@Z 999999'
      @ ROW,052 SAY ADJ2-OLDQTY2   PICTURE '@Z 999999'
      @ ROW,059 SAY ADJ3-OLDQTY3   PICTURE '@Z 999999'
      @ ROW,066 SAY ADJ4-OLDQTY4   PICTURE '@Z 999999'
      @ ROW,073 SAY ADJ5-OLDQTY5   PICTURE '@Z 999999'
      @ ROW,080 SAY ADJ6-OLDQTY6   PICTURE '@Z 999999'
      @ ROW,087 SAY ADJ7-OLDQTY7   PICTURE '@Z 999999'
      @ ROW,094 SAY ADJ8-OLDQTY8   PICTURE '@Z 999999'
      @ ROW,101 SAY TOTADJ-TOTOLD PICTURE '@Z 9999999'
      IF qCostPrv                      && COSTING ACCESS. 1/30/92
        ***Modified by ARH on 11/03/93 to put the unit cost into
        ***consideration when calculating the cost if the style
        ***costing method was by AVERAGE
        *B802903,1 AMM Calculate total
        *@ ROW,111 SAY (TOTADJ-TOTOLD)*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) ;
                      PICTURE '9999999.99'
        @ ROW,111 SAY TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) - TOTOLD*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST) ;
                      PICTURE '9999999.99'
        *B802903,1 AMM end
      ENDIF

    ELSE

      @ ROW,045 SAY ADJ1   PICTURE '@Z 999999'
      @ ROW,052 SAY ADJ2   PICTURE '@Z 999999'
      @ ROW,059 SAY ADJ3   PICTURE '@Z 999999'
      @ ROW,066 SAY ADJ4   PICTURE '@Z 999999'
      @ ROW,073 SAY ADJ5   PICTURE '@Z 999999'
      @ ROW,080 SAY ADJ6   PICTURE '@Z 999999'
      @ ROW,087 SAY ADJ7   PICTURE '@Z 999999'
      @ ROW,094 SAY ADJ8    PICTURE '@Z 999999'
      @ ROW,101 SAY TOTADJ PICTURE '@Z 9999999'

      IF qCostPrv                      && COSTING ACCESS. 1/30/92
        @ ROW,111 SAY TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) ;
                      PICTURE '9999999.99'
      ENDIF

    ENDIF

    @ ROW,123 SAY DATE
    ROW=ROW+1

    *B803556,1 AMH add transfer transactions [Start]
    *IF TYPE='A'        && ADJUSTMENT
    IF TYPE$'AT'        && ADJUSTMENT or TRANSFER
    *B803556,1 AMH add transfer transactions [End  ]
      XNEWQTY1 = OLDQTY1 + ADJ1
      XNEWQTY2 = OLDQTY2 + ADJ2
      XNEWQTY3 = OLDQTY3 + ADJ3
      XNEWQTY4 = OLDQTY4 + ADJ4
      XNEWQTY5 = OLDQTY5 + ADJ5
      XNEWQTY6 = OLDQTY6 + ADJ6
      XNEWQTY7 = OLDQTY7 + ADJ7
      XNEWQTY8 = OLDQTY8 + ADJ8
      XTOTNEW  = TOTOLD  + TOTADJ
    ELSE
      XNEWQTY1 = ADJ1
      XNEWQTY2 = ADJ2
      XNEWQTY3 = ADJ3
      XNEWQTY4 = ADJ4
      XNEWQTY5 = ADJ5
      XNEWQTY6 = ADJ6
      XNEWQTY7 = ADJ7
      XNEWQTY8 = ADJ8
      XTOTNEW  = TOTADJ
    ENDIF

    @ ROW,032 SAY 'NEW INVENTORY:'

    @ ROW,045 SAY XNEWQTY1 PICTURE '@Z 999999'
    @ ROW,052 SAY XNEWQTY2 PICTURE '@Z 999999'
    @ ROW,059 SAY XNEWQTY3 PICTURE '@Z 999999'
    @ ROW,066 SAY XNEWQTY4 PICTURE '@Z 999999'
    @ ROW,073 SAY XNEWQTY5 PICTURE '@Z 999999'
    @ ROW,080 SAY XNEWQTY6 PICTURE '@Z 999999'
    @ ROW,087 SAY XNEWQTY7 PICTURE '@Z 999999'
    @ ROW,094 SAY XNEWQTY8 PICTURE '@Z 999999'
    @ ROW,101 SAY XTOTNEW PICTURE '@Z 9999999'

    IF qCostPrv              && COSTING ACCESS. 1/30/92

      @ ROW,111 SAY ( XTotNew * IIF( XAvg_Cost, ;
                      IIF(Type='A',STYLE.Ave_Cost,Unt_Cost), ;
                      STYLE.TotCost) ) PICTURE '9999999.99'   &&TAK 03/14/95

    ENDIF
    ROW=ROW+1
  ENDIF

  * UPDATE THE SUBTOTALS

  *B803556,1 AMH add transfer and locking transactions [Start]
  *IF TYPE='A'           && SUB TOTAL ON ADJUSTMENTS ONLY
    *B802903,1 AMM Calculate sub and grand total quantities of inventory adjustment
  *  FOR lnC = 1 TO 8
  *    lcC = STR(lnC,1)
  *    laAdjQty[lnC]  = laAdjQty[lnC]  + EVAL('Adj'+lcC)
  *    laTotQty[lnC]  = laTotQty[lnC]  + EVAL('Adj'+lcC)
  *  ENDFOR
    *B802903,1 AMM end
  *  XSTOTPCS  = XSTOTPCS + TOTADJ
  *  XSTOTAMT  = XSTOTAMT + (TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST))
  *  XGTOTPCS  = XGTOTPCS + TOTADJ
  *  XGTOTAMT  = XGTOTAMT + (TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST )) 
  *ELSE
    *B802903,1 AMM Calculate sub and grand total quantities of physical inventory
    *xSTotPhPcs  = xSTotPhPcs + TOTADJ
    *xSTotPhAmt  = xSTotPhAmt + TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST)
    *xGTotPhPcs  = xGTotPhPcs + TOTADJ
    *xGTotPhAmt  = xGTotPhAmt + TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST)
  *  FOR lnC = 1 TO 8
  *    lcC = STR(lnC,1)
  *    laPhyQty[lnC]  = laPhyQty[lnC]  + EVAL('Adj'+lcC) - EVAL('OldQty'+lcC)
  *    laPTotQty[lnC] = laPTotQty[lnC] + EVAL('Adj'+lcC) - EVAL('OldQty'+lcC)
  *  ENDFOR
  *  xSTotPhPcs  = xSTotPhPcs + TOTADJ - TotOld
  *  xSTotPhAmt  = xSTotPhAmt + TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) - (TOTOLD*IIF(XAVG_COST,Old_COST,STYLE.TOTCOST))
  *  xGTotPhPcs  = xGTotPhPcs + TOTADJ - TotOld
  *  xGTotPhAmt  = xGTotPhAmt + TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST) - (TOTOLD*IIF(XAVG_COST,Old_COST,STYLE.TOTCOST))
    *B802903,1 AMM end
  *ENDIF
  =lfUpdtSTot()
  *B803556,1 AMH add transfer and locking transactions [End  ]
  SKIP
ENDDO
*-- end of lpLongB.
*!*************************************************************
*! Name      : lfLockApen
*! Developer : AHMED MAHER (AMH)
*! Date      : 24/08/2000
*! Purpose   : Function that Append Locking Transaction to tmp file
*!*************************************************************
*! Called from : ....
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfLockApen()
*!*************************************************************
*!B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment
*!
FUNCTION lfLockApen

SELECT (lcInvTmp)
APPEND BLANK
REPLACE STYLE     WITH STYINVJL.STYLE                                              ,;
        DATE      WITH STYINVJL.DTRDATE                                            ,;
        TYPE      WITH 'L'                                                         ,;
        CSESSION  WITH STYINVJL.CSESSION                                           ,;
        OLD_COST  WITH IIF(STYINVJL.NTOTSTK=0,0,STYINVJL.NSTKVAL/STYINVJL.NTOTSTK) ,;
        TOTOLD    WITH -STYINVJL.NTOTSTK                                           ,;
        CFROMWARE WITH STYINVJL.CWARECODE
FOR I = 1 TO 8
  Z = STR(I,1)
  REPLACE OLDQTY&Z. WITH -STYINVJL.NSTK&Z
ENDFOR
*B607014,1 ASH 03/06/2003 (Begin) Fix bug of 'end of file encountered'.
IF !EOF('STYINVJL')
*B607014,1 ASH 03/06/2003 (End)
  SELECT STYINVJL
  SKIP
ENDIF

*B130558,1 EIH 12/21/2005 Must sure if we refere to the same style or not. [Begin]
IF &lcInvTmp..STYLE = STYINVJL.STYLE
*B130558,1 EIH 12/21/2005 [End]

  SELECT (lcInvTmp)
  REPLACE UNT_COST WITH IIF(STYINVJL.NTOTSTK=0,0,STYINVJL.NSTKVAL/STYINVJL.NTOTSTK) ,;
          TOTADJ   WITH STYINVJL.NTOTSTK
  FOR I = 1 TO 8
    Z = STR(I,1)
    REPLACE ADJ&Z. WITH STYINVJL.NSTK&Z
  ENDFOR
*B130558,1 EIH 12/21/2005 Must sure if we refere to the same style or not. [Begin]
ENDIF
*B130558,1 EIH 12/21/2005 [End]

*B607014,1 ASH 03/06/2003 (Begin) Fix bug of 'end of file encountered'.
IF !EOF('STYINVJL')
*B607014,1 ASH 03/06/2003 (End)
  SELECT STYINVJL
  SKIP
ENDIF
*-- end of lfLockApen.
*!*************************************************************
*! Name      : lfAPTAppen
*! Developer : AHMED MAHER (AMH)
*! Date      : 24/08/2000
*! Purpose   : Function that Append Not Locking Transaction to tmp file
*!*************************************************************
*! Called from : ....
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAPTAppen()
*!*************************************************************
*!B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment
*!
FUNCTION lfAPTAppen

SCATTER MEMVAR
SELECT (lcInvTmp)
APPEND BLANK
GATHER MEMVAR

*B603229,1 Abd - We will add recored  with Negative qty if type of 
*B603229,1 Abd - transaction is transfer to void accumulate wrong Qty. [Begin]

*B605858,1 BWA 04/22/2002 Add the record of the -ve value in case there is no warehouse in the criteria.[START]
*IF TYPE = 'T'
IF TYPE = 'T' AND EMPTY(laOGFxFlt[lnLocPos,6])
*B605858,1 BWA 04/22/2002.[END]

  APPEND BLANK
  GATHER MEMVAR
  REPLACE ADJ1   WITH  -1 * ADJ1  ,;
          ADJ2   WITH  -1 * ADJ2  ,;
          ADJ3   WITH  -1 * ADJ3  ,;
          ADJ4   WITH  -1 * ADJ4  ,;
          ADJ5   WITH  -1 * ADJ5  ,;
          ADJ6   WITH  -1 * ADJ6  ,;
          ADJ7   WITH  -1 * ADJ7  ,;
          ADJ8   WITH  -1 * ADJ8  ,;
          TOTADJ WITH  -1 * TOTADJ

*B605858,1 BWA 04/22/2002 Check if the warehouse received to it or transfere from it.[START]
ELSE
  IF TYPE = 'T' AND !EMPTY(laOGFxFlt[lnLocPos,6]) AND ( laOGFxFlt[lnLocPos,6] == cFromWare)
    REPLACE ADJ1   WITH  -1 * ADJ1  ,;
            ADJ2   WITH  -1 * ADJ2  ,;
            ADJ3   WITH  -1 * ADJ3  ,;
            ADJ4   WITH  -1 * ADJ4  ,;
            ADJ5   WITH  -1 * ADJ5  ,;
            ADJ6   WITH  -1 * ADJ6  ,;
            ADJ7   WITH  -1 * ADJ7  ,;
            ADJ8   WITH  -1 * ADJ8  ,;
            TOTADJ WITH  -1 * TOTADJ
  ENDIF
*B605858,1 BWA 04/22/2002.[END]
ENDIF
*B603229,1 Abd - [End]


SELECT INVTADJ
SKIP
*-- end of lfAPTAppen.

*!*************************************************************
*! Name      : lfUpdtSTot
*! Developer : AHMED MAHER (AMH)
*! Date      : 24/08/2000
*! Purpose   : Function that Update SubTotals in Long B Format
*!*************************************************************
*! Called from : ....
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUpdtSTot()
*!*************************************************************
*!B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment
*!
FUNCTION lfUpdtSTot
lnCol = IIF(TYPE='A',1,IIF(TYPE='T',2,IIF(TYPE='L',3,4)))
FOR lnC = 1 TO 8
  lcC = STR(lnC,1)
  laAllQty[lnC,lnCol] = laAllQty[lnC,lnCol] + EVAL('Adj'+lcC) - IIF(lnCol>2,EVAL('OldQty'+lcC),0)
  laAlTotQty[lnC,lnCol] = laAlTotQty[lnC,lnCol] + EVAL('Adj'+lcC) - IIF(lnCol>2,EVAL('OldQty'+lcC),0)
ENDFOR
XSTOTAlPCS[lnCol]  = XSTOTAlPCS[lnCol] + TOTADJ - IIF(lnCol>2,TotOld,0)
XSTOTAlAMT[lnCol]  = XSTOTAlAMT[lnCol] + (TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST)) ;
                         - IIF(lnCol>2,(TOTOLD*IIF(XAVG_COST,Old_COST,STYLE.TOTCOST)),0)
XGTOTAlPCS[lnCol]  = XGTOTAlPCS[lnCol] + TOTADJ - IIF(lnCol>2,TotOld,0)
XGTOTAlAMT[lnCol]  = XGTOTAlAMT[lnCol] + (TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST)) ;
                         - IIF(lnCol>2,(TOTOLD*IIF(XAVG_COST,Old_COST,STYLE.TOTCOST)),0)
*-- end of lfUpdtSTot.

*!*************************************************************
*! Name      : lfCreatFil
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 06/18/2003
*! Purpose   : To create the temporary file
*!*************************************************************
*! Example     : = lfCreatFil()
*!*************************************************************
*! B607162,1 KHM 06/18/2003 
*!*************************************************************
FUNCTION lfCreatFil
PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT INVTADJ
CREATE CURSOR (lcInvTmp);
  (Style C(19), Dyelot C(10), cReason C(25), Date D, dPostDate D, Type C(1), cSession C(6),;
   Unt_Cost N(12,2), Old_Cost N(12,2), Adj1 N(8), Adj2 N(8), Adj3 N(8), Adj4 N(8), Adj5 N(8),;
   Adj6 N(8), Adj7 N(8), Adj8 N(8), TotAdj N(10), OldQty1 N(8), OldQty2 N(8), OldQty3 N(8),;
   OldQty4 N(8), OldQty5 N(8), OldQty6 N(8), OldQty7 N(8), OldQty8 N(8), TotOld N(10),;
   nOldTo1 N(8), nOldTo2 N(8), nOldTo3 N(8), nOldTo4 N(8), nOldTo5 N(8), nOldTo6 N(8),;
   nOldTo7 N(8), nOldTo8 N(8), nTotOldTo N(10),Link_Code C(6),glFYear C(4), glPeriod C(2),;
   cFromWare C(6), cToWare C(6), cOwner C(16))

SELECT(lnAlias)
