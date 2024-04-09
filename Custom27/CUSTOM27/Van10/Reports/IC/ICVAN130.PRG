*:****************************************************************
*: Program file  : ICVAN130.PRG
*: Program desc. : Date Sensitive Inventory Report for VAN10.
*: System        : Aria Apparel System (A27).
*: Module        : Inventory Contol  (IC)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#102152,1
*: Date          : 01/22/2001
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfEvalSegs , lfsrvSty   , lfStySum
*:               :            : lfvFabric  , lfwVOldVal ,lfvWareCode
*:               :            : lfWchgSort , lfCretTemp .
*:               -----------------------------------------------
*:               : PROCEDURE  : lpCollData , lpGetScale.
*:               :            : 
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*:C#102152,1          : 
*:****************************************************************
*:Modifications :
*:****************************************************************
*:
*-- Declaration variables.
*-- llDyelot  :- Company setup parameters To know if company dyelot or not.
*-- lcCstMth  :- Variable Hold Cost Method.
*-- lcStyTemp :- Temp File hold select styles.	
*-- lcAlias   :- Variable hold the selected file.
STORE 0 TO TotStk1,TotStk2,TotStk3,TotStk4,TotStk5,TotStk6,TotStk7,TotStk8,TotValCost,TotSalVal,;
           lcStk1 , lcStk2 , lcStk3  , lcStk4 , lcStk5 , lcStk6 , lcStk7 , lcStk8
STORE.F. To llDyelot
STORE '' To lcCstMth,lcAlias

*-- End Declaration variables.

*-- The user should select warehouse if sort by warehouse
IF  lcRpSortBy = 'W' .AND. EMPTY(lcRpWareH)
  *-- Message Text  : You must select a location first .
  *-- Message number: 00052.
  *-- Button Text   : OK.
  *-- button Number : 00000
  =gfModalGen('TRM42150B00000','DIALOG')
  RETURN
ENDIF

llDyelot   = gfGetMemVar('M_Dyelot')   ='Y'
llAvgCstMh = gfGetMemVar('M_Cost_Met') = 'A'
lcRpName   = "ICVAN130"


*-- Call Procedure To Collect The data.
DO lpCollData

SELECT (lcStyTemp)
GO TOP

IF EOF()
 *-- Message Text  : 'No Record Selected for the report..!'
 *-- Message number: 00052.
 *-- Button Text   : OK.
 *-- button Number : 00000
 =gfModalGen('TRM00052B00000','DIALOG')
 RETURN
ENDIF


R_TITLE     = [Date Sensitive Inventory Report]  && Report title.
R_WIDTH     = 'W'                                && Wide report.

DO gfDispRe WITH EVAL('lcRpName')
RETURN
*-- [ END ] Of Report Code.
*:****************************************************************
*: Name        : lfEvalSegs
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 01/22/2001
*: Purpose     : Evaluate NonMajor Type and variables.
*:****************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfEvalSegs()
*:****************************************************************
*
FUNCTION lfEvalSegs
  lnMajSeg    = gfItemMask('SM')  && No. of major segments.
  lnMajorLen  =  LEN(gfItemMask("PM"))
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

  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'

  *-- Compute Variables titles. [begin]
  lcStyMake  = lcStyMajor + ' (Make/Buy)'
  *-- Compute Variables titles. [end]
  
  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
RETURN ''

*-- End of lfEvalSegs.
*:****************************************************************
*: Name        : lfsrvSty
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 01/22/2001
*: Purpose     : Rise change style flag, in range browse screen.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : None
*:****************************************************************
*: Example     : =lfsrvSty()
*:****************************************************************
*: Note        : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*:****************************************************************
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
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE

*-- End of lfsrvSty.
*:****************************************************************
*: Name        : lfStySum
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 01/22/2001
*: Purpose     : sum a specific field for the current style in style file
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid,style browse calculated fields.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : Calculated field value.
*:****************************************************************
*: Example     : =lfStySum()
*:****************************************************************
*
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

*-- End OF lfStySum.
*:****************************************************************
*: Name        : lfvFabric
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 01/22/2001
*: Purpose     : Valid function of the Fabric
*:****************************************************************
*: Called from : Option grid [Fabric Get field]
*:****************************************************************
*: Calls       : FaBrow()
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*
FUNCTION lfvFabric

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Fabric he entered is not in the file.
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*-- End of lfvFabric.
*:****************************************************************
*: Name      : lfwVOldVal
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/22/2001
*: Purpose   : When function to get the Old value
*:****************************************************************
*: Called from : Some of the Option Grid fields
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfwOldVal()
*:****************************************************************
*
FUNCTION lfwVOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*-- End of lfwVOldVal.
*:*************************************************************
*: Name      : lfvWareCode
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/22/2001
*: Purpose   : Function to WareHouse Code.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfvWareCode()
*:*************************************************************
FUNCTION lfvWareCode

*-- Varible to hold  the name of the memory variable used to create the current GET field .
lcObjName = SYS(18)
*-- Varible to hold  the value of the current GET field .
lcObjVal = EVALUATE(SYS(18))

*-- IF The user want to Browse or if the Warehouse he entered is not in the
*-- file.
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*-- End OF lfvWareCode.
*:****************************************************************
*: Name      : lfWchgSort
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/22/2001
*: Purpose   : When function Clear read.
*:****************************************************************
*: Called from : Some of the Option Grid fields
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfWchgSort()
*:****************************************************************
*
FUNCTION  lfWchgSort

CLEAR READ
*-- End OF lfWchgSort.
*:*************************************************************
*: Name      : lpCollData.
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/22/2001
*: Purpose   : Select the records to be printed in the report.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : DO lpCollData
*:****************************************************************
*
PROCEDURE lpCollData

*-- llRpPrtDtl :- Print wareHouse details Yes or No.
*-- lcRpPrtDye :- Flag to print dyelot details.
*-- llRpPrtDtl :- Print WareHouse details Yes Or No.
*-- lcRpSortBy :- Type Of sorting.
*-- llMultiWare:- .T. If Company is Multiple Location.

IF llRpPrtDtl .OR. lcRpSortBy  = 'W'
  SELECT StyDye
  IF lcRpSortBy = 'W'
    SET ORDER TO TAG StyDyeW
  ENDIF
  IF llMultiWare
    SET RELATION TO Style+cWareCode INTO WHSLOC ADDITIVE
  ENDIF
  SET RELATION TO Style  INTO Style ADDITIVE
  
  IF !EMPTY(lcRpWareH)
    SET FILTER TO cWareCode = lcRpWareH .AND. !EOF('Style')
  ELSE
    SET FILTER TO !EOF('Style')
  ENDIF
  
ELSE
  SELECT STYLE
  IF llMultiWare
    SET RELATION TO Style INTO WHSLOC ADDITIVE
  ENDIF
ENDIF


*-- Add If Cheak when select style make or buy in the Option Grid.
IF lcRpMake $ 'MU'
  lcFilter = lcRpExp + " .AND. MAKE = IIF(lcRpMake='M',.T.,.F.)"
ELSE
  lcFilter = lcRpExp
ENDIF
SELECT STYLE
SET FILTER TO &lcFilter


*-- Function to create the temp file.
= lfCretTemp ()
GO TOP             && To activate the relations and the filters.

SELECT (lcAlias)
GOTO TOP
SCAN
  WAIT WINDOW 'Select Data ... :- ' +STYLE NOWAIT
  *-- We should call function when style change
  SCATTER TO MEMVAR
  IF lcRpSortBy = 'W'
    = SEEK(STYLE,'STYLE')
  ENDIF
  lcScale  = STYLE.Scale
  lcTotCost= STYLE.TotCost
  lcPricea = STYLE.PriceA
  
  *-- We should Save the style.
  lcStyle  = STYLE

  *-- Variables to hold the average cost.
  lnOldCost = &lcAlias..Ave_cost 
  lnAvCost  = &lcAlias..Ave_cost 

  = SEEK('S' + lcScale,'SCALE')
  lnCnt = SCALE.Cnt  && Hold number of Sizes.
  SELECT (lcStyTemp)
  APPEND BLANK
  GATHER FROM MEMVAR
  REPLACE STYLE   WITH lcStyle   ,;
          SCALE   WITH lcScale   ,;
          TOTCOST WITH lcTotCost ,;
          PRICEA  WITH lcPricea

  FOR I = 1 TO lnCnt
    Z=STR(I,1)
    lcStk&Z = &lcAlias..Stk&Z
  ENDFOR

  lcTotStk = 0
  lcWhrExp = ".T."
  SELECT STYINVJL

  *-- Make the index desc. to calculate the av.cost.
  SET ORDER TO STYINVJL DESCENDING
  IF lcRpSortBy = 'W' .AND. !EMPTY(lcRpWareH)
    = SEEK(lcStyle + lcRpWareH)    
    lcWhrExp = 'cWareCode = '+"'"+lcRpWareH+"'"
  ELSE
    = SEEK(lcStyle)
  ENDIF
  
  
  SCAN WHILE Style = lcStyle .AND. &lcWhrExp FOR dTrDate > ldRpDate
    DO CASE
      CASE CirType = "I" .AND. CtrType $ "29"
        FOR C = 1 TO lnCnt
          Y=STR(C,1)
          lcStk&Y = lcStk&Y + NStk&Y
        ENDFOR
      CASE CirType = "I" .AND. (CtrType $ "38" )
        FOR C = 1 TO lnCnt
          Y=STR(C,1)
          lcStk&Y = lcStk&Y + NStk&Y
        ENDFOR
      OTHERWISE
        IF CirType = "R"
          lnOldStk = lcStk1+lcStk2+lcStk3+lcStk4+lcStk5+lcStk6+lcStk7+lcStk8
          lnNewStk = nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
          IF lnOldStk - lnNewStk > 0
            lnAvCost = (lnOldStk * lnOldCost - lnNewStk * nCost) / (lnOldStk - lnNewStk)
          ENDIF
        ENDIF
        FOR C = 1 TO lnCnt
          Y=STR(C,1)
          lcStk&Y = lcStk&Y - NStk&Y
        ENDFOR
      ENDCASE  
      lnOldCost = nCost
    ENDSCAN
    SELECT (lcStyTemp)
     FOR C = 1 TO lnCnt    
      Y=STR(C,1)
      REPLACE Stk&Y WITH lcStk&Y
      TotStk&Y = TotStk&Y + lcStk&Y
      lcTotStk = lcTotStk + lcStk&Y
    ENDFOR
    TotValCost = TotValCost + ROUND(lcTotStk*IIF(llAvgCstMh,IIF(lcRpSortBy = 'S',&lcAlias..AVE_COST,StyDye.AVE_COST),STYLE.TotCost),2)
    TotSalVal  = TotSalVal  + ROUND(lcTotStk*STYLE.PriceA,2)
    
    REPLACE TOTStk WITH lcTotStk
    *-- If cost methoed is average.
    IF llAvgCstMh
      REPLACE Ave_Cost WITH lnAvCost
    ENDIF
    
    IF lcStk1 = 0 .AND. lcStk2 = 0 .AND. lcStk3 = 0 .AND. lcStk4 = 0 .AND. lcStk5 = 0 .AND. ;
       lcStk6 = 0 .AND. lcStk7 = 0 .AND. lcStk8 = 0
      DELETE
    ENDIF
    SELECT &lcAlias
ENDSCAN    

*-- End Of lpCollData.
*:****************************************************************
*: Name      : lfCretTemp
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/22/2001
*: Purpose   : Function create the temp file.
*:****************************************************************
*: Called from : None.
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfCretTemp()
*:****************************************************************
*
FUNCTION lfCretTemp

lcAlias = IIF(lcRpSortBy = 'W','StyDye','Style')

IF lcRpSortBy = 'W'
  SELECT &lcAlias
  =AFIELDS(laMlnTmp)
  lnNewFld = ALEN(laMlnTmp,1)  
  DIMENSION laMlnTmp[lnNewFld+3,4]
  laMlnTmp[lnNewFld + 1,1] = 'PriceA'
  laMlnTmp[lnNewFld + 1,2] = 'N'
  laMlnTmp[lnNewFld + 1,3] = 7
  laMlnTmp[lnNewFld + 1,4] = 2

  laMlnTmp[lnNewFld + 2,1] = 'Scale'
  laMlnTmp[lnNewFld + 2,2] = 'C'
  laMlnTmp[lnNewFld + 2,3] = 1
  laMlnTmp[lnNewFld + 2,4] = 0

  laMlnTmp[lnNewFld + 3,1] = 'TotCost'
  laMlnTmp[lnNewFld + 3,2] = 'N'
  laMlnTmp[lnNewFld + 3,3] = 9
  laMlnTmp[lnNewFld + 3,4] = 2

  CREATE DBF (gcWorkDir+lcStyTemp) FROM ARRAY laMlnTmp  
 
ELSE
  SELECT &lcAlias
  IF USED(lcStyTemp)
    SELECT (lcStyTemp)
    ZAP
  ELSE
    COPY STRUCTURE TO (gcWorkDir+lcStyTemp)
  ENDIF
ENDIF

=gfOpenFile(gcWorkDir+lcStyTemp,'','EX')

*-- End Of lfCretTemp.
*:****************************************************************
*: Name      : lpGetScale
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/22/2001
*: Purpose   : function create the temp file.
*:****************************************************************
*: Called from : None.
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lpGetScale()
*:****************************************************************
*
PROCEDURE lpGetScale
PARAMETERS lcscale , lnSpace

PRIVATE lnOldAlias ,lnNumber,lcReturn ,lcscale,lnSpace,Z

lnOldAlias = SELECT (0)

SELECT SCALE
=SEEK ('S'+lcscale)

IF FOUND ()
  lnNumber = 1
  lcReturn = ''
  FOR  lnNumber = 1 To SCALE.Cnt
    Z = STR(lnNumber,1)
    lcScale&Z=PADL(ALLTRIM(SZ&Z),5,' ')
    lcReturn = lcReturn + lcScale&Z + IIF(lnNumber = SCALE.Cnt,'',lnSpace)
  ENDFOR
ELSE
  lcReturn ='* * * E R R O R * * *'
ENDIF

SELECT(lnOldAlias)

RETURN(lcReturn)

*-- End OF lpGetScale.
*:****************************************************************