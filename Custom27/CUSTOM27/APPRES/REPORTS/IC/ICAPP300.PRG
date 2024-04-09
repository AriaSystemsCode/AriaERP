*:********************************************************************
*: Program file  : ICAPP300  (REFERENCE, C102208)
*: Program desc. : Customize Style Ranking Based on Booking Report For (APPRES).
*: For screen    : None.
*:         System: ARIA APPAREL SERIES 2.7
*:         Module: REPORT
*:      Developer: ABD ELRAIM KOURA . KAM
*!********************************************************************
*!********************************************************************
*: Calls         : =lfEvalSegs()
*:                 =lfvColor()
*:                 =lfvFabric()
*:                 =lfwOldVal()                 
*:                 =lfvInvWare()
*:                 =lfsrvSty()
*:                 =lfwRepWhen()
*:                 =lfRankFile()
*:                 =lfStySum()
*:                 =lfvRankBy()
*:********************************************************************
*: Passed Parameters  :NONE
*:********************************************************************
*: Example            : 
*:********************************************************************
*--hold type of sort
STORE SPACE(1)  TO lcSort                           
STORE 0 TO lnClrLen,lnClrPos   &&hold color position and color length
*--call function to get color position and color length
=lfGetClrD()

*--SET RELATIONS BETWEEN FILES
SELECT ORDLINE
SET RELATION TO STYLE INTO STYLE
SET RELATION TO ORDER INTO ORDHDR ADDI


lcMakeOp  = IIF(lcRpMake='M','','!')
lcRepExpr = IIF(lcRpMake='B','',"AND "+  lcMakeOp +"Style.Make") 
lcStusRp   = IIF(lcRpStatus='B','',[ AND (Style.Status="] + lcRpStatus + [")])
lcRpExp = lcRpExp+lcRepExpr+ lcStusRp

*--search in ordline for lcrpexp
WAIT 'SELECTING RECORDS FOR THE REPORT .....' WINDOW NOWAIT
SELECT ORDLINE
LOCATE FOR &lcRpExp
*--if no record satisfy the conditions 
IF EOF()
  SET DEVICE TO SCREEN
  =gfDialog('I','There are no records to display')
  RETURN
ENDIF  
*--create  temp file 
 = lfRankFile()
*-- index rank_file depend on lcRpRankby
SELECT(RANK_FILE)
SET UNIQUE OFF
DO CASE
  CASE lcRpRankby = 'S'
    lcSort = 'SALES'
    INDEX ON - NET_AMT  TAG (RANK_FILE1)
  CASE lcRpRankby = 'P'
    lcSort = 'PIECES'
    INDEX ON - NET_PCS  TAG (RANK_FILE1)
  OTHERWISE
    lcSort = 'GROSS PROFIT'
    INDEX ON - GROS_PRO  TAG (RANK_FILE1)
ENDCASE
SET UNIQUE ON
SET ORDER TO TAG (RANK_FILE)
*-- MAIN LOOP TO SCAN THE ORDLINE FILE  AND UPDATE THE RANKING FILE
WAIT 'Calculating Total Qtys Booked by Style'+IIF(llRpColor, ;
'/Color','') WINDOW NOWAIT
*--set zero value to set of memvar
SELECT ORDLINE
SCAN For &lcRpExp
   *--adding records to rank_file
  SELECT (RANK_FILE)
  IF !SEEK(IIF(llRpColor,ORDLINE.STYLE,ALLTRIM(STYLE.CSTYMAJOR)))
    APPEND BLANK
    REPLACE STYLE WITH IIF(llRpColor,ORDLINE.STYLE,ALLTRIM(STYLE.CSTYMAJOR))
    REPLACE DESC  WITH STYLE.DESC
  ENDIF
  lnCost    = IIF(XAVG_COST,STYLE.AVE_COST,STYLE.TOTCOST)
  lnTotAmnt = ((OrdLine.TOTQTY*OrdLine.Price)*(1-ordhdr.DISC/100))
  REPLACE TOT_PCS   WITH TOT_PCS +ORDLINE.TOTQTY,;
  TOT_AMT   WITH TOT_AMT +lnTotAmnt,;
  COST      WITH lnCost,;   
  NET_PCSP  WITH NET_PCSP+ORDLINE.TOTQTY,;
  NET_AMTP  WITH NET_AMTP+lnTotAmnt,;
  NET_PCS   WITH NET_PCS +ORDLINE.TOTQTY,;
  NET_AMT   WITH NET_AMT +lnTotAmnt,;
  AVG_PRICE WITH IIF(TOT_PCS<>0,TOT_AMT / TOT_PCS,0),;
  CGS       WITH CGS + lnCost * ORDLINE.TOTQTY,;
  GROS_PRO  WITH NET_AMT - CGS    
ENDSCAN
R_WIDTH = 'W'
R_TITLE = 'STYLE RANKING BASED ON BOOKING REPORT'
XTITLE1 = 'SORTED BY &lcSort - '       
SELECT(RANK_FILE)
SET ORDER TO TAG (RANK_FILE1)
GO TOP

*--set value to row,pageno
*-- declare xtot array for holding sum values
ROW    = 99
PAGENO = 0
DIMENSION XTOT(7)   
STORE 0 TO XTOT
WAIT 'Report printing press <SPACE BAR>  to abort ...' WINDOW NOWAIT
SET DEVICE TO PRINT
SCAN WHILE INKEY()<> 32
  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'CITI100',XTITLE1,R_WIDTH      &&SMH 08/18/93
    @ 05,00 SAY 'STYLE        CLR    DESCRIPTION AVG.PRICE    COST TOT.PCS    TOT.AMT RET.PCS    RET.AMT NET.PCS    NET.AMT COST OF GOODS GROSS PROF.'
    ROW = 6
  ENDIF
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....
*STYLE               DESCRIPTION   AVG.PRICE    COST TOT. PCS   TOT. AMT  RET.PCS   RET. AMT  NET. PCS   NET. AMT COST OF GOODS GROSS PROF.
*123456789012345678  1234567890      9999.99 9999.99  9999999 9999999.99   9999999 9999999.99   9999999 9999999.99 9999999.99     9999999.99

  @ ROW,00 SAY SUBSTR(STYLE,1,lnClrPos-2)
  @ ROW,013 SAY SUBSTR(STYLE,lnClrPos,lnClrLen)
  @ ROW,020 SAY SUBSTR(DESC,1,10)
  @ ROW,034 SAY AVG_PRICE  PICTURE '9999.99'
  @ ROW,042 SAY COST       PICTURE '9999.99'
  @ ROW,050 SAY TOT_PCS    PICTURE '9999999'
  @ ROW,058 SAY TOT_AMT    PICTURE '9999999.99'
  @ ROW,069 SAY RET_PCS    PICTURE '9999999'
  @ ROW,077 SAY RET_AMT    PICTURE '9999999.99'
  @ ROW,088 SAY NET_PCS    PICTURE '9999999'
  @ ROW,096 SAY NET_AMT    PICTURE '9999999.99'
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
*--print totals 
@ ROW,00 SAY REPLICATE('*',132 )
ROW = ROW + 1
@ ROW,01 SAY '*** GRAND TOTAL ***'
@ ROW,050 SAY XTOT(1)    PICTURE '9999999'
@ ROW,058 SAY XTOT(2)    PICTURE '9999999.99'
@ ROW,069 SAY XTOT(3)    PICTURE '9999999'
@ ROW,077 SAY XTOT(4)    PICTURE '9999999.99'
@ ROW,088 SAY XTOT(5)    PICTURE '9999999'
@ ROW,096 SAY XTOT(6)    PICTURE '9999999.99'
@ ROW,110 SAY XTOT(7)    PICTURE '9999999.99'
@ ROW,122 SAY XTOT(6)-XTOT(7)  PICTURE '9999999.99'  &&TAK 01/23/94
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('*',132 )
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 24/02/2001
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
*! Name      : lfvColor
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 24/02/2001
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
*! Name      : lfvFabric
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 24/02/2001
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

*--IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- end of lfvFabric.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 24/02/2001
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
*! Name      : lfvInvWare
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 24/02/2001
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

*-IF The user want to Browse or if the Warehouse he entered is not in the
*-file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- end of lfvInvWare.
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 24/02/2001
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
*! Name      : lfwRepWhen
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 24/02/2001
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

  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.COMPLETE'),1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(DATE())+'|'+DTOC(DATE())
  ENDIF

*  = lfRankFile()  && Create temporary used in collecting data.
  
  XAVG_COST = (gfGetMemVar('M_COST_MET') = 'A')

ENDIF
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfRankFile
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 24/02/2001
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

CREATE CURSOR (RANK_FILE) (STYLE C(19),DESC C(20), AVG_PRICE N(12,2),;
                           COST N(7,2), TOT_PCS N(7,0), TOT_AMT N(13,2),;
                           RET_PCS N(7,0), RET_AMT N(14,2), NET_PCSP N(7,0),;
                           NET_AMTP N(13,2), NET_PCS N(7,0), NET_AMT N(13,2),;
                           CGS N(13,2), GROS_PRO N(13,2))                           

INDEX ON STYLE TAG (RANK_FILE)
*-- end of lfRankFile.
*!*************************************************************
*! Name      : lfStySum
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 24/02/2001
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
*! Name      : lfvRankBy
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 24/02/2001
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
*! Name      : lfGetClrD
*! Developer : AbdElrahim Koura-KAM
*! Date      : 14/01/2001
*! Purpose   : To get color position
*!*************************************************************
*! Example            :  =lfGetClrD()
*!*************************************************************
function lfGetClrD
DECLARE laItemSeg[1]
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*--Get Color Discreption