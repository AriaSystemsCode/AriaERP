*:***************************************************************************
*: Program file  : ICSRANK.PRG
*: Program desc. : Style Ranking Report
*: For Report    : (ICSRANK.FRX)
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,gfGetMemVar,gfModalGen,gfItemMask,gfBrowWare.
*:               : lfwRepWhen,lfwOldVal,lfClearRep,
*:               : lfEvalSegs,lfCollTime,lfvRankBy,
*:               : lfSRVSty,lfStySum,lfvFabric,FaBrow,
*:               : lfvInvWare,lfRankFile,lfGetMem,lfvColor,lfPreGrd.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICSRANK
*:***************************************************************************
*: This Report Program is due to E301042 ...
*:***************************************************************************
*: Modifications :
*E301090,7 MAB 12/14/98 Change INVHDR File to RETHDR File to avoid,
*E301090,7              because now invline file have voided records.
*E301170,1 MAB 03/15/1999 Use variables detect OG filter changes.
*B603713,4 AAN 11/19/2000 resize a fields
*B605334,1 ASH 01/23/2001 Add Hold option to the style status in the option grid.
*B606661,1 BWA 01/01/2003 Let the Date filter empty.
*E125824,1 BWA 02/21/2004 Add the Currency option.
*:***************************************************************************

lcStTime = TIME()
llCalRetrn = .F.  && Compute returns.

llMultCurr = gfGetMemvar('LLMULCURR')

lcMakeOp  = IIF(lcRpMake='M','','!')
*B605334,1 ASH 01/23/2001 (Begin) Change the status to be Active,Hold,Canceled,All.
*lcRepExpr = [style+invoice+STR(lineno,6) = ''] +;
            IIF(lcRpMake='B','', [ AND ] + lcMakeOp+[Style.Make]) +;
            IIF(lcRpStatus='B','',[ AND (Style.Status="] + lcRpStatus + [")])
lcRepExpr = [style+invoice+STR(lineno,6) = ''] +;
            IIF(lcRpMake='B','', [ AND ] + lcMakeOp+[Style.Make]) +;
            IIF(lcRpStatus='L','',[ AND (Style.Status="] + lcRpStatus + [")])            
*B605334,1 ASH 01/23/2001 (End)
lcRpExp =  lcRepExpr + IIF( '.T.' $ lcRpExp , '' , [ AND ] + lcRpExp )

*-- if user clear setting or change criteria.

*E301170,1 MAB 03/15/1999 Use variables detect OG filter changes. [Begin
*IF llClearFn OR !(lcRpExp == lcLastExpr) OR llChStyle OR llChClr
IF llClearFn OR llOGFltCh OR llChCurr
*E301170,1 MAB 03/15/1999 Use variables detect OG filter changes. [End

  lcLastExpr = lcRpExp   && To later know that user change critria.
  STORE .F. TO llClearFn,llChStyle,llChClr , llChCurr

  llCalRetrn = ('RM' $ gcCmpModules)  && do Return invoice Procedure.
  
  IF RECCOUNT(RANK_FILE) > 0
    USE IN (RANK_FILE)
    = lfRankFile()
  ENDIF

  *-- Start collecting rank data.
  llRpColor = llRpColor AND (lcFree_Clr = 'C')  && Style/Color
  SET ORDER TO (RANK_FILE) IN (RANK_FILE)
  lcOldRank = ''  && To ReIndex File again.

  *-- initial value for memory variables.
  STORE '' TO m.Style,m.Desc
  STORE 0 TO m.AVG_PRICE,m.COST,m.TOT_PCS,m.TOT_AMT,;
             m.RET_PCS,m.RET_AMT,m.NET_PCSP,m.NET_AMTP,;
             m.NET_PCS,m.NET_AMT,m.CGS,m.GROS_PRO

  SELECT INVLINE
  SET RELATION TO Style INTO STYLE
  SET RELATION TO Invoice INTO InvHdr ADDITIVE
  
  *-- Scan all invoices in selected critria.
  SCAN FOR &lcRpExp

    =lfGetMem()  && Fill Memory variables with its corresponding values.
    
    *-- if you find this style in temp. file
    IF SEEK(IIF(llRpColor,STYLE,ALLTRIM(STYLE.CSTYMAJOR)),RANK_FILE)

      SELECT (RANK_FILE)
      REPLACE TOT_PCS    WITH TOT_PCS    + m.TOT_PCS   ,;
              TOT_AMT    WITH TOT_AMT    + m.TOT_AMT   ,;
              NET_PCSP   WITH NET_PCSP   + m.TOT_PCS   ,;
              NET_AMT    WITH NET_AMT    + m.TOT_AMT   ,;
              NET_PCS    WITH NET_PCS    + m.TOT_PCS   ,;
              NET_AMTP   WITH NET_AMTP   + m.TOT_AMT   ,;
              CGS        WITH CGS        + m.CGS       ,; 
              GROS_PRO   WITH NET_AMT    - CGS         ,;
              AVG_PRICE  WITH IIF(TOT_PCS<>0,(TOT_AMT/TOT_PCS),0)
              
	  SELECT INVLINE
    ELSE
     INSERT INTO (RANK_FILE) FROM MEMVAR
    ENDIF
  ENDSCAN
ENDIF  
*B605334,1 ASH 01/23/2001 (Begin) Comment the following code to allow the program to compute the return even if there are no invoices.
*IF RECCOUNT(RANK_FILE) = 0
*  llCalRetrn = .F.  && Avoiding doing nex block of code.
*ENDIF
*B605334,1 ASH 01/23/2001 (End)
*-- if 'RM' module is installed and temp. file have data.
IF llCalRetrn
  
  *-- Change expression to Return case. [Begin]
  lcRpExp = STRTRAN(lcRpExp, 'invoice', 'crmemo')
  lcRpExp = STRTRAN(lcRpExp, 'STR(lineno,6)', 'STR(RECNO(),7)')
  lcRpExp = STRTRAN(lcRpExp, 'INVLINE', 'RETLINE')
  lcRpExp = STRTRAN(lcRpExp, 'INVDATE', 'CRDATE')

  *E301090,7 Change INVHDR File to RETHDR File to avoid  [Begin]
  *E301090,7 bug due to adding this line to hidden filter
  lcRpExp = STRTRAN(lcRpExp, 'INVHDR', 'RETHDR')
  *E301090,7 Change INVHDR File to RETHDR File to avoid  [End  ]
  *-- Change expression to Return case. [End]

  *-- intially free all memory variables.
  STORE '' TO m.Style,m.Desc
  STORE 0 TO m.AVG_PRICE,m.COST,m.TOT_PCS,m.TOT_AMT,;
             m.RET_PCS,m.RET_AMT,m.NET_PCSP,m.NET_AMTP,;
             m.NET_PCS,m.NET_AMT,m.CGS,m.GROS_PRO

  SELECT RETLINE
  SET RELATION TO Style INTO STYLE
  SET RELATION TO cRMemo INTO RetHdr ADDITIVE
  
  *-- Scan all returns in the specific criteria.
  SCAN FOR &lcRpExp

	*E125824,1 BWA 02/21/2004 Add the Currency option.[START]
    *m.STYLE     = STYLE
    *m.COST      = IIF(XAVG_COST,STYLE.AVE_COST,STYLE.TOTCOST)
    *m.DESC      = STYLE.DESC
    *m.AMOUNT    = AMOUNT
    *m.TOTQTY    = TOTQTY
    *m.AVG_PRICE = m.AMOUNT / m.TOTQTY

    m.STYLE     = STYLE
    m.COST      = IIF(XAVG_COST,STYLE.AVE_COST,STYLE.TOTCOST)

    *E125824,1 BWA 02/21/2004 Add the Currency option.[START]
	*m.COST     = IIF(XAVG_COST,IIF(lcRpCurr == 'F' , lfGetFCurr(STYLE.AVE_COST, lcRpCurr , ldRpExDate , lcRpTmpNam) , STYLE.AVE_COST),;
                               IIF(lcRpCurr == 'F' , lfGetFCurr(STYLE.TOTCOST , lcRpCurr , ldRpExDate , lcRpTmpNam) , STYLE.TOTCOST))

  	PRIVATE lcAliasR
	lcAliasR = ALIAS()
	SELECT RETHDR

    *E125824,1 BWA 02/21/2004.[END]

    m.DESC      = STYLE.DESC
    m.AMOUNT    = IIF(lcRpCurr <> 'F' , gfAmntDisp(RETLINE.AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),RETLINE.AMOUNT)

   	SELECT (lcAliasR)
	*--THEN RETURN TO THE RETLINE.
    
    m.TOTQTY    = TOTQTY
    m.AVG_PRICE = m.AMOUNT / m.TOTQTY
    *E125824,1 BWA 02/21/2004[END]
    
    *-- if style was not exist in temp. file
    IF !SEEK(IIF(llRpColor,STYLE,ALLTRIM(STYLE.CSTYMAJOR)),RANK_FILE)
      INSERT INTO (RANK_FILE) FROM MEMVAR
    ENDIF
    
    SELECT (RANK_FILE)
    REPLACE RET_PCS   WITH RET_PCS + m.TOTQTY ,;
            RET_AMT   WITH RET_AMT + m.AMOUNT ,;
            NET_PCS   WITH ( TOT_PCS - RET_PCS ),;
            NET_AMT   WITH ( TOT_AMT - RET_AMT )
    

    *-- if return from invoice.
    IF !EMPTY(RETLINE.INVOICE)
      REPLACE NET_PCSP WITH ( NET_PCSP - m.TOTQTY ),;
              NET_AMTP WITH ( NET_AMTP - m.AMOUNT )
  
      IF llRpColor   
        REPLACE  CGS WITH NET_PCSP * m.COST
      ELSE
        REPLACE  CGS WITH CGS - (m.TOTQTY * m.COST)
      ENDIF
  
      *-- 2.6 Code is like this.
      *REPLACE GROS_PRO WITH NET_AMTP - CGS
    ENDIF
    *-- 2.7 Code By MAB.
    REPLACE GROS_PRO WITH NET_AMT - CGS
    
    SELECT RETLINE

  ENDSCAN

  *-- (restore master expression)
  lcRpExp =  lcLastExpr  

ENDIF

SELECT (RANK_FILE)
IF RECCOUNT(RANK_FILE) > 0
  *-- ReIndex File if old index was changed.
  IF lcOldRank <> lcRpRankBy
    lcOldRank = lcRpRankBy
    DO CASE

      CASE lcRpRankBy = 'S'
        *AMH - Fix bug invalid file name [Start]
        *INDEX ON - NET_AMT  TAG (RANK_FILE1)
        INDEX ON - NET_AMT  TAG (RANK_FILE1) OF (RANK_FILE)
        *AMH - [End]

      CASE lcRpRankBy = 'P'
        *AMH - Fix bug invalid file name [Start]
        *INDEX ON - NET_PCS  TAG (RANK_FILE1)
        INDEX ON - NET_PCS  TAG (RANK_FILE1) OF (RANK_FILE)
        *AMH - [End]

      CASE lcRpRankBy = 'G'
        *AMH - Fix bug invalid file name [Start]
        *INDEX ON - GROS_PRO  TAG (RANK_FILE1)
        INDEX ON - GROS_PRO  TAG (RANK_FILE1) OF (RANK_FILE)
        *AMH - [End]

    ENDCASE
  ENDIF
ENDIF  

*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(RANK_FILE))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
DO gfDispRe WITH EVAL('lcRpForm')
*-- end of report code.

*-- Start of function and procedure section.
*-------------------------------------------

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : When function of OG
*!*************************************************************
*! Called from : OG read cycle
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*
FUNCTION lfwRepWhen

IF TYPE('lcLastExpr') $ 'UL' OR EMPTY(lcLastExpr)

  SET ORDER TO FABRIC IN FABRIC
  SET ORDER TO STYLE IN STYLE
  SET ORDER TO WAREHOUS IN WAREHOUS

  *B606661,1 BWA 01/01/2003 Let the fiter of the date be empty.[START]
  *lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVLINE.INVDATE'),1)
  *IF EMPTY(laOGFxFlt[lnDatePos,6])
  *  laOGFxFlt[lnDatePos,6] = DTOC(DATE())+'|'+DTOC(DATE())
  *ENDIF
  *B606661,1 BWA 01/01/2003.[END]

  = lfRankFile()  && Create temporary used in collecting data.

  XAVG_COST = (gfGetMemVar('M_COST_MET') = 'A')

ENDIF
*-- end of lfwRepWhen.

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
  lnMajSeg    = gfItemMask('SM')  && No. of major segments.
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

  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
  *-- Compute Free/Color Items in Style code Structure. [End]

  *-- Compute Variables titles. [begin]
  lcShowVar = 'Show details by ' + lcNonMajTl
  lcStyStat = lcStyMajor + ' Status'
  lcStyMake = lcStyMajor + ' (Make/Buy)'
  *-- Compute Variables titles. [end]

RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfvRankBy
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Rank By validation, Save old Rank to use in when reindex.
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRankBy()
*!*************************************************************
*
FUNCTION lfvRankBy
lcOldRank = IIF(laOldVal=1,'S',IIF(laOldVal=2,'P','G'))
*-- end of lfvRankBy.

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
    llClearSty = .F.
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
*! Name      : lfvFabric
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/1998
*! Purpose   : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvFabric

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- end of lfvFabric.

*!*************************************************************
*! Name      : lfvInvWare
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/1998
*! Purpose   : Valid function of the Order Warehouse
*!*************************************************************
*! Called from : Option grid [Order Warehouse Get field]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvInvWare

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Warehouse he entered is not in the
*file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- end of lfvInvWare.

*!*************************************************************
*! Name      : lfRankFile
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Create temporary cursor.
*!*************************************************************
*! Called from : Program code, OG when function.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfRankFile()
*!*************************************************************
*
FUNCTION lfRankFile

*CREATE CURSOR (RANK_FILE) (STYLE C(19),DESC C(20), AVG_PRICE N(7,2),;
                           COST N(7,2), TOT_PCS N(7,0), TOT_AMT N(10,2),;
                           RET_PCS N(7,0), RET_AMT N(10,2), NET_PCSP N(7,0),;
                           NET_AMTP N(10,2), NET_PCS N(7,0), NET_AMT N(10,2),;
                           CGS N(10,2), GROS_PRO N(10,2))
                           
CREATE CURSOR (RANK_FILE) (STYLE C(19)     , DESC C(20)      , AVG_PRICE N(12,2)   ,;
                           COST N(7,2)     , TOT_PCS N(7,0)  , TOT_AMT N(13,2)     ,;
                           RET_PCS N(7,0)  , RET_AMT N(14,2) , NET_PCSP N(7,0)     ,;
                           NET_AMTP N(13,2), NET_PCS N(7,0)  , NET_AMT N(13,2)     ,;
                           CGS N(13,2)     , GROS_PRO N(13,2), lnexrate N(9,4))

INDEX ON STYLE TAG (RANK_FILE) OF (RANK_FILE)

*--End of lfRankFile.
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
*-- Close temp. opended files, if it used.
IF USED(RANK_FILE)
 USE IN (RANK_FILE)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfGetMem
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/21/1998
*! Purpose   : Fill RANK_FILE memory variables with its corresponding data.
*!*************************************************************
*! Called from : Program code,
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetMem()
*!*************************************************************
FUNCTION lfGetMem
m.Style     = IIF(llRpColor,STYLE,ALLTRIM(STYLE.CSTYMAJOR))
m.Desc      = STYLE.DESC
m.COST      = IIF(XAVG_COST,STYLE.AVE_COST,STYLE.TOTCOST)

STORE TOTQTY TO m.TOT_PCS,m.NET_PCS,m.NET_PCSP
STORE ((TOTQTY*PRICE)*(1-INVHDR.DISCPCNT/100))*(1-INVHDR.TRDE_DISC/100) TO ;
      m.NET_AMT,m.NET_AMTP,m.TOT_AMT 

m.CGS       = m.COST*m.TOT_PCS

*E125824,1 BWA 02/21/2004 Add the Currency option.[START]
*m.COST     = IIF(XAVG_COST,IIF(lcRpCurr == 'F' , lfGetFCurr(STYLE.AVE_COST, lcRpCurr , ldRpExDate , lcRpTmpNam) , STYLE.AVE_COST),;
                           IIF(lcRpCurr == 'F' , lfGetFCurr(STYLE.TOTCOST , lcRpCurr , ldRpExDate , lcRpTmpNam) , STYLE.TOTCOST))

*--MUST SELECT INVHDR
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT INVHDR

m.TOT_AMT  = IIF(lcRpCurr <> 'F' , gfAmntDisp(m.TOT_AMT, lcRpCurr , ldRpExDate , lcRpTmpNam),m.TOT_AMT)
m.NET_AMT  = m.TOT_AMT
m.NET_AMTP = m.TOT_AMT
*m.CGS      = IIF(lcRpCurr == 'F' , gfAmntDisp(m.COST, lcRpCurr , ldRpExDate , lcRpTmpNam),m.COST) * m.TOT_PCS

SELECT (lcAlias)
*E125824,1 BWA 02/21/2004[END]

m.GROS_PRO  = m.NET_AMT - m.CGS
m.AVG_PRICE = m.TOT_AMT / m.TOT_PCS
  
*--End of lfGetMem.
*!*************************************************************
*! Name      : lfvColor
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/21/1998
*! Purpose   : Validation function for print by color, Rise change color flag.
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvColor()
*!*************************************************************
*
FUNCTION lfvColor
llChClr = .T.
*-- end of lfvColor.

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
*! Called from : Report code section.
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
*! Name      : lfPreGrd
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/21/1998
*! Purpose   : Function called to determine if we continue showing OG or not.
*!*************************************************************
*! Called from : ICREPORT program
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPreGrd()
*!*************************************************************
*
FUNCTION lfPreGrd
PRIVATE lcUserMake
lcUserMake = ''

*-- if no setups for AR module
IF !('AR' $ gcComp_Mdl)
  lcUserMake = 'setup'
ENDIF

*-- if AR module not installed.
IF EMPTY(lcUserMake) AND !('AR' $ gcCmpModules) 
  lcUserMake = 'install'
ENDIF  

IF !EMPTY(lcUserMake)
  *-- Message 
  *-- Rank Styles Report require XXX of XXX module for Company XXX,
  *-- First, you have to XXX it through the company information program.
  *--                               < Ok >
  =gfModalGen('TRM00336B00000','DIALOG',lcUserMake + '|' + 'Accounts Receivable' + '|' + gcCom_Name + '|' + lcUserMake)
  llDoOpGrid = .F.
ENDIF
*-- end of lfPrOpGd.

*E125824,1 BWA 02/21/2004 Add the Currency option.[START]
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 10/21/1998
*! Purpose   : Get the description of Curr.
*!*************************************************************
*! Called from : Syrepuvr "Currency Display" Record.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvCurDisp()
*!*************************************************************
FUNCTION lfvCurDisp

llChCurr = .T.
=gfRepCur(.T., @lcRpCurr,@ldRpExDate ,lcRpTmpNam)

*--End of lfvCurDisp.
*!*************************************************************
*! Name      : lfDefCurr
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 10/21/1998
*! Purpose   : Return Default currency value.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : currency default value.
*!*************************************************************
*! Example     : = lfDefCurr()
*!*************************************************************
FUNCTION lfDefCurr

llMultCur = IIF(TYPE('llMultCur') = "U" , gfGetMemVar('llMulCurr') , llMultCur)
RETURN IIF(llMultCur,'F','O')

*--End of lfDefCurr.
*!*************************************************************
*! Name      : lfvCurCode
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 10/21/1998
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example    : =lfvCurCode()
*!*************************************************************
FUNCTION lfvCurCode
PRIVATE lnCurrPos

lnCurrPos = ASCAN(laOgFxFlt,'INVHDR.CCURRCODE')
IF lnCurrPos > 0
  lnCurrPos  = ASUBSCRIPT(laOgFxFlt,lnCurrPos,1)
ENDIF

lnAlias=SELECT(0)

*-- Allow blank curreny in case user have no selected forign currency.
IF EMPTY(laOGFxFlt[lnCurrPos,6]) .AND. lcRpCurr <> 'F' 
  RETURN
ENDIF

IF !SEEK(laOGFxFlt[lnCurrPos,6],'SYCCURR') .OR. ATC("?",laOGFxFlt[lnCurrPos,6]) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    laOGFxFlt[lnCurrPos,6] = lcOldCurr
  ELSE
    laOGFxFlt[lnCurrPos,6] = laTemp[1]
  ENDIF
ENDIF

SHOW GET laOGFxFlt[lnCurrPos,6]

SELECT(lnAlias)

*--End of lfvCurCode.
*!*************************************************************
*! Name      : lfwCurCode
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 10/21/1998
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example   : =lfwCurCode()
*!*************************************************************
FUNCTION lfwCurCode
PRIVATE lnCurrPos

lnCurrPos = ASCAN(laOgFxFlt,'INVHDR.CCURRCODE')
IF lnCurrPos > 0
  lnCurrPos  = ASUBSCRIPT(laOgFxFlt,lnCurrPos,1)
ENDIF

lcOldCurr = laOGFxFlt[lnCurrPos,6]

*--End of lfwCurCode.
*!*************************************************************
*! Name      : lfRepShow
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 10/21/1998
*! Purpose   : Get the defaukt value. [This () Called from SYDREPRT]
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example   : =lfRepShow()
*!*************************************************************
FUNCTION lfRepShow
PRIVATE lnCurrPos

lnCurrPos = ASCAN(laOgFxFlt,'INVHDR.CCURRCODE')
IF lnCurrPos > 0
  lnCurrPos  = ASUBSCRIPT(laOgFxFlt,lnCurrPos,1)
ENDIF

laOGFxFlt[lnCurrPos,6]= gcBaseCurr
=lfOGShowGet("lnRepCurr")

*--End of lfRepShow.
*E125824,1 BWA 02/21/2004[END]

*!********************************************************************
*! Name      : lfGetFCurr
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 03/14/2005
*! Purpose   : Return the Foreign amount From Base currency.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : lcRpDispCur  && The way to display the amount.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcTmepFile   && The temp file name that hold the temp. 
*!           :                 exchange rates.
*!           : llAprvCurr   && If you are using the Approved currency.
*!********************************************************************
*! Call      : From all the AP reports that is using the currency display
*!           : feature.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
*! Example   : lfGetFCurr(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.).
*!********************************************************************
FUNCTION lfGetFCurr
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile

PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)
IF lcRpDispCur = 'F'
  lnExRate   = 0
  lnUnit     = 0
  
  IF EMPTY(lcGetFile)
    IF "INV" $ ALIAS()
	  lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,INVHDR.CCURRCODE)
	ELSE
	  lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,RETHDR.CCURRCODE)
	ENDIF
  ELSE
    lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
  ENDIF
  
  IF lcCurrCode = gcBaseCurr
    lnExRate = 1
    lnUnit   = 1
  ELSE
    
    IF "INV" $ ALIAS()
      ldExRateDt = INVLINE.INVDATE
      lnExRate   = INVHDR.Nexrate
      lnUnit     = INVHDR.Ncurrunit
    ELSE
      ldExRateDt = RETLINE.CRMEMO
      lnExRate   = RETHDR.Nexrate
      lnUnit     = RETHDR.Ncurrunit

    ENDIF
  ENDIF
  
  lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
  lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
  
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
  lcExSin1 = IIF(lcExSin1 = '/','*','/')
  lcExSin2 = IIF(lcExSin2 = '*','/','*')
  
  lnAmount = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
  
ENDIF  
  
SELECT (lnSavAlias)
RETURN lnAmount

*--End of lfGetFCurr.