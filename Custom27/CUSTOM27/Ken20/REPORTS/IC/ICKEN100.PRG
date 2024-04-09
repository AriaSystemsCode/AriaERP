*!********************************************************************
*: Program file  : ICKEN100.PRG  (REFERENCE, C102265)
*: Program desc. : CUSTOMIZED STYLE STATUS REPORT (FOR KENT)
*: For screen    : None.
*:         System: ARIA APPAREL SERIES 2.7
*:         Module: REPORT
*:      Developer: ABD ELRAIM KOURA . KAM
*:      Date     : 02/04/2001 
*!********************************************************************
*: Calls : FUNCTIONS  :=lfGetClrD() 
*:                    :=lfPrtHdr()
*:                    :=lfPrtDet()
*:                    :=lfPrtSub()
*:                    :=lfColData()
*:                    :=lfEvalSegs()
*:                    :=lfsrvSty() 
*:                    :=lfStySum()
*:       : PROCEDURE  : ENDREPORT,RPT_HDR.
*:********************************************************************
*: Passed Parameters  : None
*:********************************************************************
*: Modifications :
*: B605950,1 BWA 09/07/2002 Fix the bug of printing broken screen in printing.
*:********************************************************************

*--variable(s) hold color position and color length 
STOR 0 TO lnClrLen,lnClrPos

*--check for make buy option
lcMakeOp  = IIF(lcRpMake='M','','!')
lcRepExpr = IIF(lcRpMake='B','', lcMakeOp +"Style.Make") 

*--add make buy option to lcrpexp

*B605950,1 BWA 09/07/2002 Fix the bug of missing exprension.[START]
*lcRpExp = IIF( '.T.' $ lcRpExp , lcRepExpr, + IIF(EMPTY(lcRepExpr),lcRpExp,lcRpExp + "  AND  " +lcRepExpr))
lcRpExp = IIF( '.T.' $ lcRpExp , IIF(EMPTY(lcRepExpr) , '.T.' , lcRepExpr), + IIF(EMPTY(lcRepExpr),lcRpExp,lcRpExp + "  AND  " +lcRepExpr))
*B605950,1 BWA 09/07/2002.[END]

*--check for status option
lcStusRp   = IIF(lcRpStatus='B','',[  (Style.Status="] + lcRpStatus + [")])

*--add status option to lcrpexp
lcRpExp = IIF(EMPTY(lcRpExp) , lcStusRp , IIF(EMPTY(lcStusRp),lcRpExp,lcRpExp + "  AND  " + lcStusRp))

R_WIDTH   = 'W'
ROW       = 0                         && variable hold row number
PAGENO    = 0                         && variable hold page number
R_TITLE   = " STYLE STATUS REPORT "   && variable hold main title for report
XTITLE    =lcRpTitle                  && variable hold optional title for report

lnTBCTot = 0
lnTotWip = 0                          && variable hold total WIP
lnTotStk = 0                          && variable hold total stock 
lnTotOrd = 0                          && variable hold total qty of order 
lcStyTmp = gfTempName()               && varaible hold temp file name 

*--create temp cursor for data collecting
CREATE CURSOR (lcStyTmp) (STYLE C(19) ,StyDesc C(20),;
                             DIV C(02) , TBC N(10),SWIP N(10),STOCK N(10),;
                             Orders N(7))
INDEX ON Div+Style TAG &lcStyTmp
*--call function to get color position and color length
=lfGetClrD()
*--call function to get desired data
=lfColData()

SELECT (lcStyTmp)

*B605950,1 BWA 09/07/2002 Fix the bug of printing broken screen in printing.[START]
*GOTO TOP
*IF EOF()
*  =gfDialog('I','No record selected .')
*  RETURN
*ENDIF
*SET DEVICE TO PRINTER
*WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
*=lfPrtHdr()
*=lfPrtDet()
*DO ENDREPORT
*SET DEVICE TO SCREEN
*RETURN

LOCATE
IF RECCOUNT() = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ELSE
  SET DEVICE TO PRINTER
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  =lfPrtHdr()
  =lfPrtDet()
  DO ENDREPORT
  SET DEVICE TO SCREEN
  RETURN
ENDIF
*B605950,1 BWA 09/07/2002.[END]

*--- END MAIN PROG
**********************************************************************
*:Name             : lfColData
*:Developer        : ABD ELRAHIM KOURA (KAM) 
*:Purpose          : Function to collect data.
*:Date             : 02/04/2001
*:********************************************************************
*: Calls : None
*:********************************************************************
*: Passed Parameters  : None
*:********************************************************************
*: Example         :=lfColData()
*:********************************************************************
*
FUNCTION lfColData
SELECT Style
SCAN FOR &lcRpExp
  SCATTER MEMVAR MEMO
  m.lcStyle = Style
  lcStyMaj  =cstymajor
  WAIT WINDOW 'Selecting record for style : ' + m.lcStyle NOWAIT
  m.StyDesc = DESC
  m.Div     = cDivision
  m.Orders  = TotOrd
  m.TBC     = 0
  m.SWIP    = 0
  m.Stock   = TotStk
  SELECT CUTTKTH
  SET ORDER TO CUTTKTHS
  =SEEK(lcStyMaj)
  SCAN REST WHILE Style+CutTkt = lcStyMaj FOR STATUS $ 'OH'
    lcCuttkt = Cuttkt
    SELECT CUTTKTL
    =SEEK(lcCuttkt+m.lcStyle)
    DO CASE
      CASE CUTTKTH.STATUS = 'H'
        WAIT WINDOW 'Selecting cuttkt  : ' + lcCuttkt NOWAIT
        m.TBC  = m.TBC  + TotQty
      CASE CUTTKTH.STATUS = 'O'
        WAIT WINDOW 'Selecting cuttkt : ' + lcCuttkt NOWAIT
        SCAN REST WHILE  CUTTKT + STYLE = lcCuttkt+m.lcStyle
          m.SWIP = m.SWIP + IIF(TranCd = '1',TotQty,-TotQty)
        ENDSCAN      
    ENDCASE
  ENDSCAN
  IF m.SWIP <> 0 .OR. m.TBC <> 0 .OR. m.Orders <> 0 OR m.Stock <> 0
    INSERT INTO (lcStyTmp) FROM MEMVAR
  ENDIF
ENDSCAN
WAIT CLEAR
**********************************************************************
*:Name             : lfPrtHdr
*:Developer        : ABD ELRAHIM KOURA (KAM) 
*:Purpose          : Function to Print report header.
*:Date             : 02/04/2001
*:********************************************************************
*: Calls : None
*:********************************************************************
*: Passed Parameters  : None
*:********************************************************************
*: Example         :=lfPrtHdr()
*:********************************************************************
FUNCTION lfPrtHdr
PAGENO = PAGENO + 1
DO RPT_HDR WITH 'ICKEN100',''+ XTITLE,R_WIDTH
ROW = 5
*0****0****1****0****2****0****3****0****4****0****5****0****6****0****7****0****8****0****9****0****0****0****1****0****2****0****3
*STYLE         COLOR  DESCRIPTION           DI TBC        WIP        STOCK      ORDERS     IMMEDIATE OTS  TOTAL OTS                                                           
@ ROW , 000 SAY "Style"
@ ROW , 014 SAY "Color"
@ ROW , 023 SAY "Style Description"
@ ROW , 045 SAY "DIV"
@ ROW , 049 SAY "       TBC"
@ ROW , 060 SAY "       WIP"
@ ROW , 071 SAY "     Stock"
@ ROW , 082 SAY "    Orders"
@ ROW , 093 SAY "      Immediate OTS"
@ ROW , 120 SAY "Total OTS"
ROW = ROW + 1
@ ROW , 000 SAY REPLICATE('_',132)
ROW = ROW + 1
*--end function lfPrtHdr
**********************************************************************
*:Name             : lfPrtDet
*:Developer        : ABD ELRAHIM KOURA (KAM) 
*:Purpose          : Function to Print report detail.
*:Date             : 02/04/2001
*:********************************************************************
*: Calls : None
*:********************************************************************
*: Passed Parameters  : None
*:********************************************************************
*: Example         :=lfPrtDet()
*:********************************************************************
FUNCTION lfPrtDet
lcCurrStyle = SPACE(01)
lcDiv       = SPACE(01)
SELECT (lcStyTmp)
GOTO TOP
DO WHILE !EOF() .AND. INKEY() <> 32
  lcDiv = Div
  DO WHILE Div = lcDiv
    *lcCurrStyle = Style
    lcCurrStyle = SUBSTR(Style,1,lnClrPos-1)
    @ ROW , 000 SAY SUBSTR(Style,1,lnClrPos-1)
    SCAN REST WHILE Div+Style = lcDiv+lcCurrStyle
      @ ROW , 014 SAY SUBSTR(STYLE,lnClrPos,lnClrLen)
      @ ROW , 023 SAY StyDesc
      @ ROW , 045 SAY DIV
      @ ROW , 049 SAY TBC       PICTURE '9999999999'
      @ ROW , 060 SAY SWIP      PICTURE '9999999999'
      @ ROW , 071 SAY Stock     PICTURE '9999999999'
      @ ROW , 082 SAY Orders    PICTURE '9999999999'
      @ ROW , 102 SAY Stock - Orders PICTURE '9999999999'
      @ ROW , 119 SAY (SWIP+Stock)-Orders  PICTURE '9999999999'
      lnTBCTot = lnTBCTot + TBC 
      lnTotWip = lnTotWip + SWIP
      lnTotStk = lnTotStk + STOCK
      lnTotOrd = lnTotOrd + ORDERS
      ROW = ROW + 1
      IF ROW > 54
        =lfPrtHdr()
      ENDIF
    ENDSCAN
    IF ROW > 54
      =lfPrtHdr()
    ENDIF
  ENDDO  && DO WHILE Div = lcDiv
  =lfPrtSub()
ENDDO  && DO WHILE !EOF() .AND. INKEY() <> 32
RETURN
*-- end function lfPrtDet
**********************************************************************
*:Name             : lfPrtSub
*:Developer        : ABD ELRAHIM KOURA (KAM) 
*:Purpose          : Function to Print report detail.
*:Date             : 02/04/2001
*:********************************************************************
*: Calls : None
*:********************************************************************
*: Passed Parameters  : None
*:********************************************************************
*: Example         :=lfPrtSub()
*:********************************************************************
FUNCTION  lfPrtSub
@ ROW , 000 SAY REPLICATE('-',132)
ROW = ROW + 1
@ ROW , 000 SAY "Division  "+lcDiv+ " Subtotal"
@ ROW , 049 SAY lnTBCTot   PICTURE '9999999999'
@ ROW , 060 SAY lnTotWip   PICTURE '9999999999'
@ ROW , 071 SAY lnTotStk   PICTURE '9999999999'
@ ROW , 082 SAY lnTotOrd   PICTURE '9999999999'
@ ROW , 102 SAY lnTotStk - lnTotOrd PICTURE '9999999999'
@ ROW , 119 SAY (lnTotWip+lnTotStk)-lnTotOrd PICTURE '9999999999'
ROW = ROW + 1
@ ROW , 000 SAY REPLICATE('-',132)
ROW = ROW + 1
lnTBCTot = 0
lnTotWip = 0
lnTotStk = 0
lnTotOrd = 0
*--end function lfPrtSub
*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : ABD ELRAHIM KOURA (KAM)
*: Date        : 20/02/2001
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs

STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt
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
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF
*-- end of lfEvalSegs.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : ABD ELRAHIM KOURA (KAM)
*! Date      : 20/02/2001
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************
FUNCTION lfMajPic
*--lcmajpic var hold the picture of major part of style
lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic
*--end function lfMajPic
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 08/03/2001
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
    =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'cStyle','SH',@lcTStyFile,.T.)
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN (lcTStyFile)
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.
*!*************************************************************
*! Name      : lfStySum
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 08/03/2001
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
SELECT(lcTStyFile)
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
IF BETWEEN(lnStyRec,1,RECCOUNT('STYLE'))
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
RETURN INT(lnTotcomp)
*-- end of lfStySum.
*!*************************************************************
*! Name      : lfGetClrD
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 04/03/2001
*! Purpose   : To get color position
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetClrD
*!*************************************************************
FUNCTION lfGetClrD
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
*--end function lfGetClrD
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 08/03/2001
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
*! Name      : lfvFabric
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 08/03/2001
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