*:***************************************************************************
*: Program file  : ICRNKVA
*: Program desc. : Style Ranking Report
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Ahmed Mohamed El_Anwar (AME)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,gfGetMemVar,gfModalGen,gfItemMask,gfBrowWare.
*:               : lfwRepWhen,lfwOldVal,lfClearRep,
*:               : lfEvalSegs,lfCollTime,lfvRankBy,
*:               : lfSRVSty,lfStySum,lfvFabric,FaBrow,
*:               : lfvInvWare,lfRankFile,lfGetMem,lfvColor,lfPreGrd,lfVZAct.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICRNKVA
*:***************************************************************************
*: This Report Program is due to C102154 ...
*: In Aria 2.6 it was Van700 with C101832
*:***************************************************************************
*: Modifications :
*:***************************************************************************
lcStTime = TIME()
llCalRetrn = .F.  && Compute returns.

STORE SPACE(30) TO XTITLE

lcMakeOp  = IIF(lcRpMake='M','','!')
lcRepExpr = [style+invoice+STR(lineno,6) = ''] +;
            IIF(lcRpMake='B','', [ AND ] + lcMakeOp+[Style.Make]) +;
            IIF(lcRpStatus='B','',[ AND (Style.Status="] + lcRpStatus + [")])

lcRpExp =  lcRepExpr + IIF( '.T.' $ lcRpExp , '' , [ AND ] + lcRpExp )

*-- if user clear setting or change criteria.
IF llClearFn OR llOGFltCh
  llChZAc = .T.
  lcLastExpr = lcRpExp   && To later know that user change critria.
  STORE .F. TO llClearFn,llChStyle,llChClr

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
    WAIT WINDOW "Collecting Data... Invoice#"+Invoice NOWAIT
    =lfGetMem()  && Fill Memory variables with its corresponding values.
    
    *-- if you find this style in temp. file
    IF SEEK(IIF(llRpColor,STYLE,ALLTRIM(STYLE.CSTYMAJOR)),RANK_FILE)

      SELECT (RANK_FILE)
      REPLACE TOT_PCS    WITH TOT_PCS    + m.TOT_PCS ,;
             TOT_AMT    WITH TOT_AMT    + m.TOT_AMT ,;
             NET_PCSP   WITH NET_PCSP   + m.TOT_PCS ,;
             NET_AMT    WITH NET_AMT    + m.TOT_AMT ,;
             NET_PCS    WITH NET_PCS    + m.TOT_PCS ,;
             NET_AMTP   WITH NET_AMTP   + m.TOT_AMT ,;
             AVG_PRICE  WITH IIF(TOT_PCS<>0,(TOT_AMT/TOT_PCS),0)      ,;
             CGS        WITH CGS        + m.CGS     ,; 
             GROS_PRO   WITH NET_AMT -  CGS

      SELECT INVLINE

    ELSE  && Add new style record.

     INSERT INTO (RANK_FILE) FROM MEMVAR

    ENDIF
  
  ENDSCAN
  
ENDIF  

IF RECCOUNT(RANK_FILE) = 0
  llCalRetrn = .F.  && Avoiding doing nex block of code.
ENDIF

*-- if 'RM' module is installed and temp. file have data.
IF llCalRetrn
  
  *-- Change expression to Return case. [Begin]
  lcRpExp = STRTRAN(lcRpExp, 'invoice', 'crmemo')
  lcRpExp = STRTRAN(lcRpExp, 'STR(lineno,6)', 'STR(RECNO(),7)')
  lcRpExp = STRTRAN(lcRpExp, 'INVLINE', 'RETLINE')
  lcRpExp = STRTRAN(lcRpExp, 'INVDATE', 'CRDATE')

  lcRpExp = STRTRAN(lcRpExp, 'INVHDR', 'RETHDR')
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
    WAIT WINDOW "Collecting Data...Credit Memo#"+crmemo NOWAIT
    m.STYLE     = IIF(llRpColor,STYLE,ALLTRIM(STYLE.CSTYMAJOR))
    m.COST      = IIF(XAVG_COST,STYLE.AVE_COST,STYLE.TOTCOST)
    m.DESC      = STYLE.DESC
    m.AMOUNT    = AMOUNT
    m.TOTQTY    = TOTQTY
    m.AVG_PRICE = m.AMOUNT / m.TOTQTY
    
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
  
    ENDIF
    *-- 2.7 Code By AME.
    REPLACE GROS_PRO WITH NET_AMT - CGS
    
    SELECT RETLINE
    *-- (restore master expression)
    lcRpExp =  lcLastExpr  

  ENDSCAN
ENDIF

SELECT &RANK_FILE


IF llChZAc
  
  llChZAc = .F.
  IF lcRpStyZr # 'N'

    *-- Change expression from Return case. [Begin]

    lcRpExp  = STRTRAN(lcRpExp, '+crmemo+STR(RECNO(),7)', '')
    lcRpExp  = STRTRAN(lcRpExp, 'RETHDR.STATUS', "[X]")
    lcRpExp  = STRTRAN(lcRpExp, 'RETLINE' ,'INVLINE')

    *-- Change expression from Return case. [End]    
    lcRpExp  = STRTRAN(lcRpExp, '+invoice+STR(lineno,6)', '')
    lcRpExp  = STRTRAN(lcRpExp, 'INVHDR.STATUS', "[X]")
    
    lnDtExBg = AT("BETWEEN(DTOS(INVLINE",lcRpExp) 
    lnDtExLn = AT("}",SUBSTR(lcRpExp,lnDtExBg),2)+3
    
    IF lnDtExBg # 0
      lcRpExp = STRTRAN(lcRpExp,SUBSTR(lcRpExp,lnDtExBg,lnDtExLn) ,' .T.')
    ENDIF
    SET ORDER TO TAG &Rank_File
    GO TOP
    
    SELECT STYLE
    SCAN FOR &lcRpExp
      WAIT WINDOW "Collecting Data...Style: "+Style NOWAIT
      IF SEEK(IIF(llRpColor,STYLE,ALLTRIM(STYLE.CSTYMAJOR)),Rank_File)
        IF lcRpStyZr = 'O'
          SELECT (Rank_File)
          DELETE
        ENDIF
      ELSE
        STORE 0 TO m.Avg_Price , m.Tot_Pcs , m.Tot_Amt , m.Ret_Pcs , m.Ret_Amt ,;
                   m.Net_Pcsp , m.Net_Amtp , m.Net_Pcs , m.Net_Amt , m.Cgs ,;
                   m.Gros_Pro
        m.COST      = IIF(XAVG_COST,Style.Ave_Cost,Style.TotCost)
        m.Style     = IIF(llRpColor,Style,ALLTRIM(STYLE.CSTYMAJOR))
        m.Desc      = Style.Desc
        INSERT INTO (Rank_File) FROM MEMVAR
      ENDIF
    ENDSCAN  && style file FOR lcRpExp

    
  ENDIF  && lcRpStyZr # 'N'
ENDIF    && llChZAc
SELECT &RANK_FILE
IF lcRpStyZr = 'N'
  SET FILTER TO TOT_PCS > 0
ELSE
  SET FILTER TO
ENDIF
GO TOP
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

SELECT (RANK_FILE)
IF RECCOUNT(RANK_FILE) > 0
  *-- ReIndex File if old index was changed.
  IF lcOldRank <> lcRpRankBy
    lcOldRank = lcRpRankBy
    DO CASE

      CASE lcRpRankBy = 'S'
        XSORT = "SALES"
        INDEX ON - NET_AMT  TAG (RANK_FILE1)

      CASE lcRpRankBy = 'P'
        XSORT = "PIECES"
        INDEX ON - NET_PCS  TAG (RANK_FILE1)

      CASE lcRpRankBy = 'G'
        XSORT = "GROSS PROFIT"
        INDEX ON - GROS_PRO  TAG (RANK_FILE1)

    ENDCASE
  ENDIF
ENDIF  

R_WIDTH = 'W'
R_TITLE = 'STYLE RANKING REPORT'
XTITLE1 = 'SORTED BY &XSORT - &XTITLE'     


*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(RANK_FILE))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

ROW    = 99
PAGENO = 0

DIMENSION XTOT(7)  
STORE 0 TO XTOT

*CLEAR TYPEAHEAD
SET DEVICE TO PRINT

SCAN WHILE INKEY()<> 32

  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'ICRNKVA',XTITLE1,R_WIDTH   
    @ 05,00 SAY 'STYLE        COLOR  DESCRIPTION AVG.PRICE    COST TOT. PCS   TOT. AMT RET. PCS   RET. AMT NET. PCS   NET. AMT       COGS GROSS PROF.'      &&TAK06/21/94
    
    @ 06,00 SAY REPLICATE('-',132)
    ROW = 7
  ENDIF
  *....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
  *STYLE        COLOR  DESCRIPTION AVG.PRICE    COST TOT. PCS   TOT. AMT RET. PCS   RET. AMT NET. PCS   NET. AMT       COGS GROSS PROF.
  *123456789012 123456 1234567890    9999.99 9999.99  9999999 9999999.99  9999999 9999999.99  9999999 9999999.99 9999999.99  9999999.99
  @ ROW,000 SAY STYLE
  @ ROW,020 SAY DESC
  @ ROW,034 SAY AVG_PRICE  PICTURE '9999.99'
  @ ROW,042 SAY COST       PICTURE '9999.99'
  @ ROW,051 SAY TOT_PCS    PICTURE '9999999'
  @ ROW,059 SAY TOT_AMT    PICTURE '9999999.99'
  @ ROW,071 SAY RET_PCS    PICTURE '9999999'
  @ ROW,079 SAY RET_AMT    PICTURE '9999999.99'
  @ ROW,091 SAY NET_PCS    PICTURE '9999999'
  @ ROW,099 SAY NET_AMT    PICTURE '9999999.99'
  @ ROW,110 SAY CGS        PICTURE '9999999.99'
  @ ROW,122 SAY GROS_PRO   PICTURE '9999999.99'
  ROW = ROW + 1
  XTOT(1) = XTOT(1) + TOT_PCS
  XTOT(2) = XTOT(2) + TOT_AMT
  XTOT(3) = XTOT(3) + RET_PCS
  XTOT(4) = XTOT(4) + RET_AMT
  XTOT(5) = XTOT(5) + NET_PCS
  XTOT(6) = XTOT(6) + NET_AMT
  XTOT(7) = XTOT(7) + CGS

ENDSCAN

@ ROW,000 SAY REPLICATE('*',132 )
ROW = ROW + 1
@ ROW,000 SAY '*** GRAND TOTAL ***'
@ ROW,051 SAY XTOT(1)    PICTURE '9999999'
@ ROW,059 SAY XTOT(2)    PICTURE '9999999.99'
@ ROW,071 SAY XTOT(3)    PICTURE '9999999'
@ ROW,079 SAY XTOT(4)    PICTURE '9999999.99'
@ ROW,091 SAY XTOT(5)    PICTURE '9999999'
@ ROW,099 SAY XTOT(6)    PICTURE '9999999.99'
@ ROW,110 SAY XTOT(7)    PICTURE '9999999.99'
@ ROW,122 SAY XTOT(6) - XTOT(7) PICTURE '9999999.99'
ROW = ROW + 1
@ ROW,000 SAY REPLICATE('*',132 )
DO ENDREPORT
SET DEVICE TO SCREEN


*-- end of report code.

*-- Start of function and procedure section.
*-------------------------------------------

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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

  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVLINE.INVDATE'),1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(DATE())+'|'+DTOC(DATE())
  ENDIF

  = lfRankFile()  && Create temporary used in collecting data.

  XAVG_COST = (gfGetMemVar('M_COST_MET') = 'A')

ENDIF
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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

CREATE CURSOR (RANK_FILE) (STYLE C(19),DESC C(10), AVG_PRICE N(12,2),;
                           COST N(7,2), TOT_PCS N(7,0), TOT_AMT N(13,2),;
                           RET_PCS N(7,0), RET_AMT N(14,2), NET_PCSP N(7,0),;
                           NET_AMTP N(13,2), NET_PCS N(7,0), NET_AMT N(13,2),;
                           CGS N(13,2), GROS_PRO N(13,2))                           

INDEX ON STYLE TAG (RANK_FILE)
*-- end of lfRankFile.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*
FUNCTION lfGetMem
m.Style     = IIF(llRpColor,STYLE,ALLTRIM(STYLE.CSTYMAJOR))
m.Desc      = STYLE.DESC
m.COST      = IIF(XAVG_COST,STYLE.AVE_COST,STYLE.TOTCOST)

STORE TOTQTY TO m.TOT_PCS,m.NET_PCS,m.NET_PCSP
STORE ((TOTQTY*PRICE)*(1-INVHDR.DISCPCNT/100))*(1-INVHDR.TRDE_DISC/100) TO ;
      m.NET_AMT,m.NET_AMTP,m.TOT_AMT 

m.CGS       = m.COST*m.TOT_PCS
m.GROS_PRO  = m.NET_AMT - m.CGS
m.AVG_PRICE = m.TOT_AMT / m.TOT_PCS
*-- end of lfGetMem.

*!*************************************************************
*! Name      : lfvColor
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*! Developer : Ahmed Mohamed El_Anwar (AME)
*! Date      : 01/31/2001
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
*!*************************************************************
*! Name      : lfVZAct
*! Developer : AHMED MOHAMED EL_ANWAR
*! Date      : 01/30/2001
*! Purpose   : Print styles with zero activity., validation clears read 
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfVZAct()
*!*************************************************************
*
FUNCTION lfVZAct

IF (lcRpStyZr # lcOldZAc)
  llChZAc = .T.
ENDIF
lcOldZAc = lcRpStyZr
CLEAR READ
=lfwRepWhen()
*-- end of lfVZAct.