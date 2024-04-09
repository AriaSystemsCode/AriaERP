*:**************************************************************************
*: Program file  : ICCMPSEL.PRG
*: Program desc. : compare scaned qty with stock qty 
*: Date          : 09/05/2001
*: System        : Aria Advantage Series.
*: Module        : IVENTORY CONTROL (IC)
*: Developer     : Walid A. Wahab (WAB)
*:**************************************************************************
*: Calls :  
*:         Procedures : lpCollData
*:             
*:         Functions  : lfwRepWhen()
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*: Notes   : .... 
*:**************************************************************************
*: Example : DO ICCMPSEL
*:**************************************************************************
*: This Program is due to c#200169
*:**************************************************************************

lcRpExp=IIF( '.T.' $ lcRpExp,"LIKE(PADR(STRTRAN(lcRpStyMsk,'*','?'),19),STYLE)",;
          lcRpExp + " .AND. " + "LIKE(PADR(STRTRAN(lcRpStyMsk,'*','?'),19),STYLE)") 

llcheck = .T.
SELECT STYLE 
LOCATE FOR &lcRpExp
IF EOF()
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN
ENDIF
lcWareCode = ''
lcTmpRecv = gfTempName()
CREATE DBF (gcWorkDir+lcTmpRecv) (cWareCode C(6) , cStatus C(1), Dyelot C(10) , Style C(19) , Size C(1) , ;
            cStyDesc C(60) , lAccepted L(1) , nQty N(6) , lByUpc L(1) , ;
            lFoundUPC L(1) , Reference C(30) , nLineNo N(6) , ;
            lCrStyDye L(1) , lAdjustPO L(1) , nPrice N(8,2) , SzDesc C(5))
INDEX ON Style TAG (lcTmpRecv)

lcTmpFile = gfTempName()
CREATE DBF (gcWorkDir+lcTmpFile) (cStyMajor C(19) , Qty N(6), ScnQty N(6) )
INDEX ON cStyMajor TAG (lcTmpFile)

IF !EMPTY(lcRpTxt)
  SELECT (lcTmpRecv)
  APPEND FROM (lcRpTxt) TYPE DELIMITED
  IF !SEEK(&lcTmpRecv..Style,'Style')
    =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,"Invalid File Format.")
    llcheck = .F.
  ELSE
    LOCATE
    lcWareCode = cWareCode
    =lfClctData()
  ENDIF
ELSE
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,"You must Select the Txt File.")
  llcheck = .F.
ENDIF
SELECT (lcTmpFile)
LOCATE
IF llCheck
  lcExpr = 'FOR Qty <> ScnQty'
  DO gfDispRe WITH EVALUATE('lcRpName') , lcExpr
ENDIF

















*:****************************************************************
*: Name        : lfEvalSegs
*: Developer   : Walid A. Wahab (WAB)
*: Date        : 09/03/2001
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
  lcItmPct = ''
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
  
  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
RETURN ''

*-- End of lfEvalSegs.
*:****************************************************************
*: Name        : lfsrvSty
*: Developer   : Walid A. Wahab (WAB)
*: Date        : 09/02/2001
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
    *USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'cStyle','SH',@lcTStyFile,.T.)
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO (lcTStyFile)
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
*    USE IN (lcTStyFile)
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE

*-- End of lfsrvSty.
*:****************************************************************
*: Name        : lfStySum
*: Developer   : Walid A. Wahab (WAB)
*: Date        : 09/02/2001
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
SELECT (lcTStyFile)
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

*-- End of lfStySum.
*:*************************************************************
*: Name      : lfVStyMsk
*: Developer : Walid A. Wahab (WAB)
*: Date      : 09/02/2001
*: Purpose   : assign astriks to style mask field
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfVStyMsk()
*:*************************************************************
FUNCTION lfVStyMsk
lcItmPct = gfItemMask('PI')
lnItmWid = LEN(lcItmPct)
RETURN TRANSFORM(REPLICATE('*',lnItmWid),lcItmPct)

*--end of lfVStyMsk() function 

FUNCTION lfvTxtFl
PRIVATE lnHandle,lnResp,llContinue,lcSaf

*lcRpTxt = GETFILE('TXT',lcRpTxt)
lcOldfile = lcRpTxt
lcfile = IIF(EMPTY(lcRpTxt),'TXT',lcRptxt)
lcRpTxt = GETFILE('TXT')
IF Empty(lcRpTxt)
  lcRpTxt = lcOldfile
ENDIF
*-- end of lfvExcelFl.

*:**************************************************************************
*:* Name        : lfwExcelFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/15/2001
*:* Purpose     : When function for Output excel file
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwExcelFl()
*:***************************************************************************
FUNCTION lfwTxtFl
*lcOldExcl = lcRpExcel
*-- end of lfwExcelFl.
*C102417,1 TMI [End  ]

FUNCTION lfClctData
SELECT (lcTmpFile)
SCATTER MEMVAR MEMO BLANK
SELECT STYLE
SET ORDER TO TAG cStyle

SCAN FOR &lcRpEXP
  WAIT WINDOW "STYLE :-  " + Style.cStyMajor NOWAIT
  m.cStyMajor = Style.cStyMajor
  m.Qty       = 0
  
  SELECT STYDYE
  IF SEEK(PADR(Style.cStyMajor,lnMajorLen))
    SCAN REST WHILE PADR(Style,lnMajorLen) = PADR(Style.cStyMajor,lnMajorLen) FOR cWareCode = lcWareCode
      m.Qty = m.Qty + TOTSTK
    ENDSCAN
  ENDIF
  SELECT (lcTmpRecv)
  m.ScnQty = 0
  IF SEEK(PADR(Style.cStyMajor,lnMajorLen))
    SCAN WHILE PADR(&lcTmpRecv..Style,lnMajorLen) =   PADR(Style.cStyMajor,lnMajorLen)
      m.ScnQty = m.ScnQty + nQty
    ENDSCAN
  *--mhm2000
  ELSE
    LOOP  
  *--mhm2000  
  ENDIF
  INSERT INTO (lcTmpFile) FROM MEMVAR
ENDSCAN

SELECT STYLE
SET ORDER TO TAG STYLE


