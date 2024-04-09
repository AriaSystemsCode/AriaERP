*************************************************************************************
*: Program file  : SOGMACP.PRG (Copied from SOALOCP.PRG for GMA
*: Program desc. : Custom CutTkt/Po Allocation Report.
*: Module        : SO (Sales Order)
*:         System: Aria Apparel System (A2.7)
*:      Developer: Adel Mohmmed El Gazzar (ADEL)
*************************************************************************************
*: Calls : FUNCTIONS  : lfCreat,lfGetCutPo,lfAppend,lfGetOgVar,lfCretExp
*:                      lfWRep,lfClrRed,lfsrCut,lfsrPO,lfvStyle,lfwOldVal
*:                      lfClearRep
*************************************************************************************
*: Passed Parameters  : None
*************************************************************************************
*: Refer to C102277,1 Add 2 custom options (Consolidate Multi Store SO, S/O Staus) for GMA
*************************************************************************************
*: 605106,1 RAE 11/26/2001 Adjust the print layout in sogmacp.frx. 
*************************************************************************************
*--- Get Style Picture
lcStyPic = gfItemMask('HI')
IF llOgFltCh
  *--- OG Variable.
  lcSeason = ''
  lcDiv    = ''
  lcDivOpg = ''
  lcCutPo  = ''
  lcStyExp = ''
  lcSty1   = ''
  lcSty2   = ''
  ldComp1  = {}
  ldComp2  = {}
  =lfGetOgVar()
  *--- Expretions Variable.
  llTrnsExp = .F.
  lcCompExp = '.T.'
  lcDivExp  = ''
  lcSesExp  = ''
  lcCutPick = '.T.'
  =lfCretExp()
  =lfCreat()
  =lfGetCutPo()
ENDIF
*--- Start Display Report.
SELECT(lcRpOrder)
GOTO TOP
WAIT CLEAR
DO gfDispRe WITH EVAL('lcRpForm')

*!*************************************************************
*! Name      : lfCreat  (E# 301381)
*! Developer : Adel Mohmmed El Gazzar (ADEL)
*! Date      : 02/24/00
*! Purpose   : Function Create Temp Files.
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
*! Example   : = lfCreat()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
*!
FUNCTION lfCreat

=lfClearRep()
IF lcRpPrintBy='C'
  SELECT CutTktH
  COPY STRUCTURE TO &gcWorkDir.&lcRpTrans
  =gfOpenFile(gcWorkDir+lcRpTrans,'','EX')
  INDEX ON CutTkt TAG &lcRpTrans
ELSE
  SELECT PosHdr
  COPY STRUCTURE TO &gcWorkDir.&lcRpTrans
  =gfOpenFile(gcWorkDir+lcRpTrans,'','EX')
  INDEX ON PO TAG &lcRpTrans
ENDIF
SELECT CutPick
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'lMulSTORE'
laFileStru[lnFileStru,2] = 'L'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0
Create Table (gcWorkDir+lcRpOrder) FROM ARRAY laFileStru
USE IN (lcRpOrder)

=gfOpenFile(gcWorkDir+lcRpOrder,'','EX')
INDEX ON ctktno+order TAG &lcRpTrans
SELECT(lcRpOrder)
SET RELATION TO
SET RELATION TO ctktno INTO &lcRpTrans ADDIT
SET RELATION TO "O"+order  INTO ORDHDR ADDIT
SET RELATION TO "O"+order+cOrdLine INTO ORDLINE ADDIT
SET RELATION TO "M"+OrdLine.Account INTO CusTomer ADDIT

*!*************************************************************
*! Name      : lfGetCutPo  (E# 301381)
*! Developer : Adel Mohmmed El Gazzar (ADEL)
*! Date      : 02/24/00
*! Purpose   : Function to collect data.
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
*! Example   : = lfGetCutPo()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
*!
FUNCTION lfGetCutPo

*--- OG Variable To Colect data for
*--- Trans             Range
*--- Trans Compl. Date Range
*--- Style             Range  (From To)
*--- Style Division    Range
*--- Style Season      Range
SELECT CutPick
SET RELATION TO 
SET RELATION TO STYLE INTO STYLE ADDIT
*--If the user browses PO/CUT and select nothing
IF !USED(lcCutPo) OR RECCOUNT(lcCutPo) = 0
  llTrnsExp = .F.
ENDIF
IF llTrnsExp
  SELECT (lcCutPo)
  GOTO TOP
ELSE
  SELECT CutPick
  =SEEK(IIF(lcRpPrintBy='C','1','2'))
ENDIF
*C102277,1 (Begin) Show status mover.
lncAlias = SELECT()
SELECT CutPick
SET RELATION TO "O"+order  INTO ORDHDR ADDIT
lcStatusEx = [ORDHDR.STATUS $ lcRpStatus]
SELECT (lncAlias)
SCAN  REST WHILE IIF(llTrnsExp,.T., trancd+ctktno+style = IIF(lcRpPrintBy='C','1','2'));
           FOR IIF(llTrnsExp,.T.,&lcCutPick AND EVAL(lcStatusEx)) 
  IF llTrnsExp
    SELECT (lcCutPo)
    lcTempCutPo = IIF(lcRpPrintBy='C',CutTkt,Po)
    SELECT CutPick
    =IIF(llTrnsExp, SEEK(IIF(lcRpPrintBy='C','1'+lcTempCutPo,'2'+lcTempCutPo)), .T.)
    SCAN REST WHILE IIF(llTrnsExp, trancd+ctktno+style = IIF(lcRpPrintBy='C','1'+lcTempCutPo,'2'+lcTempCutPo), .T.);
              FOR &lcCutPick AND EVAL(lcStatusEx)
      =lfAppeRec()
    ENDSCAN
  ELSE
    =lfAppeRec()
  ENDIF
ENDSCA
*C102277,1 (End)

*!*************************************************************
*! Name      : lfAppend  (E# 301381)
*! Developer : Adel Mohmmed El Gazzar (ADEL)
*! Date      : 02/24/00
*! Purpose   : Function Append record.
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
*! Example   : = lfAppend()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
*!
FUNCTION lfAppend

SCAT MEMVAR MEMO
SELECT(lcRpOrder)
IF SEEK(IIF(lcRpPrintBy='C',m.cTktNo,'P'+m.cTktNo),IIF(lcRpPrintBy='C','CutTktH','PosHdr'))
  IF lcRpPrintBy='C'
    SELECT CutTktH
  ELSE
    SELECT PosHdr
  ENDIF
  IF  STATUS<>'X' .AND. &lcCompExp
    WAIT WINDOW "Selecting record for "+IIF(lcRpPrintBy='C','C/T # ','P/O # ')+m.cTktNo NOWAIT
    INSERT INTO (lcRpOrder) FROM MEMVAR
    SCAT MEMVAR MEMO
    IF !SEEK(IIF(lcRpPrintBy='C',m.CutTkt,m.PO),lcRpTrans)
      INSERT INTO (lcRpTrans) FROM MEMVAR
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfGetOgVar  (E# 301381)
*! Developer : Adel Mohmmed El Gazzar (ADEL)
*! Date      : 02/24/00
*! Purpose   : Function to update OG variable.
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
*! Example   : = lfGetOgVar()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
*!
FUNCTION lfGetOgVar

FOR lnInd  = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.SEASON' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcSeason = laOgFxFlt[lnInd,6]
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CDIVISION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcDiv = laOgFxFlt[lnInd,6]
    CASE  ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUTTKTH.CUTTKT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]));
       OR ALLTRIM(laOgFxFlt[lnInd,1]) = 'POSHDR.PO' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6])) 
      lcCutPo = laOgFxFlt[lnInd,6]
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.STYLE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6])) .AND. ALLTRIM(laOgFxFlt[lnInd,6]) <> '|'
      lcSty1 = PADR(SUBSTR(laOgFxFlt[lnInd,6],1,AT('|',laOgFxFlt[lnInd,6])-1),19)
      lcSty2 = PADR(SUBSTR(laOgFxFlt[lnInd,6],AT('|',laOgFxFlt[lnInd,6])+1),19)
      lcStyExp = "BETWEEN(Style.cStyMAjor, lcSty1, lcSty2)"
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'POSHDR.COMPLETE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      ldComp1 = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      ldComp2 = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
  ENDCASE
ENDFOR

*!*************************************************************
*! Name      : lfCretExp  (E# 301381)
*! Developer : Adel Mohmmed El Gazzar (ADEL)
*! Date      : 02/24/00
*! Purpose   : Function To create Separate exp to optimize colecting data.
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
*! Example   : = lfCretExp()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
*!
FUNCTION lfCretExp

*--- OG Variable To Colect data for
*------------------------------------------------------
*--- Option            Type           Expretion Name *
*------------------------------------------------------
*--- Trans             Range          (llTrnsExp)    *
*--- Trans Compl. Date Range          (lcCompExp)    *
*--- Style             Range(From To) (llStyExp)     *
*--- Style Division    Range          (lcDivExp)     *
*--- Style Season      Range          (lcSesExp)     *
*------------------------------------------------------

*--- Create Ctkt / Po Expr.
llTrnsExp = IIF(USED(lcCutPo),.T.,.F.)

*--- Create Trans. Completion Date Expr.
IF ldComp2 <> {}
  lcCompExp = 'BETWEEN(Complete, ldComp1, ldComp2)'
ENDIF

*--- Create Style Division Expr.
lcDivExp = IIF(!EMPTY(lcDiv),   'Style.cDivision $ lcDiv' ,'.T.')
lcSesExp = IIF(!EMPTY(lcSeason),'Style.Season $ lcSeason' ,'.T.')

lcCutPick = " IIF(!EMPTY(lcStyExp), EVAL(lcStyExp),.T. ) .AND. EVAL(lcDivExp) .AND. EVAL(lcSesExp)"

*!*************************************************************
*! Name      : lfWRep  (E# 301381)
*! Developer : Adel Mohmmed El Gazzar (ADEL)
*! Date      : 02/24/00
*! Purpose   : When Rep Function.
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
*! Example   : = lfWRep()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
*!
FUNCTION lfWRep

*C102277,1 (Begin) Fill the status source and target arrays.
DECLARE laRpSource[3],laRpTarget[2]
STORE 'Open'      TO laRpSource[1]
STORE 'Hold'      TO laRpSource[2]
STORE 'Completed' TO laRpSource[3]
laRpTarget = ''
STORE 'Open'      TO laRpTarget[1]
STORE 'Hold'      TO laRpTarget[2]
IF TYPE('laRpTarget[1]') = 'C'
  IF EMPTY(laRpTarget[1])
    DECLARE laRpTarget[1]
  ELSE
    FOR lnI = 2 TO ALEN(laRpTarget)
      IF TYPE('laRpTarget[lnI]') = 'U'
        laRpTarget[lnI] = ""
      ENDIF
    ENDFOR    
  ENDIF
ELSE
  DECLARE laRpTarget[1]
ENDIF
lcRpStatus = 'OH'
*C102277,1 (End)

*!*************************************************************
*! Name      : lfClrRed  (E# 301381)
*! Developer : Adel Mohmmed El Gazzar (ADEL)
*! Date      : 02/24/00
*! Purpose   : Clear read function.
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
*! Example   : = lfClrRed()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
*!
FUNCTION lfClrRed

CLEAR READ

*!*************************************************************
*! Name      : lfsrCut  (E# 301381)
*! Developer : Adel Mohmmed El Gazzar (ADEL)
*! Date      : 02/24/00
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
*! Example   : =lfsrCut()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
*!
FUNCTION lfsrCut
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    *llChAcc = .T.
    GO TOP IN CutTktH
  CASE lcParm = 'R'
    llClearCut = .F.
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfsrCut  (E# 301381)
*! Developer : Adel Mohmmed El Gazzar (ADEL)
*! Date      : 01/24/00
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
*! Example   : =lfsrCut()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrSPO
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    *llChAcc = .T.
    GO TOP IN POSHDR
  CASE lcParm = 'R'
    llClearSPO = .F.
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfvStyle     (E# 301381)
*! Developer : Adel Mohmmed El Gazzar (ADEL)
*! Date      : 01/24/00
*! Purpose   : validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
*!
FUNCTION lfvStyle
PRIVATE lcStyle
lcStyle = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE
IF EMPTY(&lcStyle)
  RETURN
ENDIF
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
  IF EMPTY(&lcStyle)
    &lcStyle = laOldVal
  ENDIF
ELSE
  &lcStyle = laOldVal
ENDIF

SET ORDER TO lcTag IN STYLE

*!*************************************************************
*! Name      : lfwOldVal     (E# 301381)
*! Developer : Adel Mohmmed El Gazzar (ADEL)
*! Date      : 01/24/00
*! Purpose   : When Old Value
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
*!
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))


*!*************************************************************
*! Name      : lfClearRep     (E# 301381)
*! Developer : Adel Mohmmed El Gazzar (ADEL)
*! Date      : 01/24/00
*! Purpose   : Clear Temp.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*!
FUNCTION lfClearRep

llOgFltCh = .T.
USE IN IIF(USED(lcRpTrans),lcRpTrans,0)
ERASE &gcWorkDir.&lcRpTrans+'.DBF'
ERASE &gcWorkDir.&lcRpTrans+'.CDX'

USE IN IIF(USED(lcRpOrder),lcRpOrder,0)
ERASE &gcWorkDir.&lcRpOrder+'.DBF'
ERASE &gcWorkDir.&lcRpOrder+'.CDX'


*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/06/2001
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
* C102277,1
FUNCTION lfvOStatus
*C102277,1 (Begin) Show status mover.
PRIVATE lcOldStat,lcCurrChr
lcOldStat = lcRpStatus  && Save old status value.
= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.
lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Completed','C','')))
  ENDFOR  && end Loop to make Status expression.
ENDIF
lcRpStatus = IIF(EMPTY(lcRpStatus),'OHCX',ALLTRIM(lcRpStatus))
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

*C102277,1 (End)
*!*************************************************************
*! Name      : lfAppeRec
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/06/2001
*! Purpose   : Append records.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAppeRec()
*!*************************************************************
*! C102277
FUNCTION lfAppeRec

SCAT MEMVAR MEMO
SELECT(lcRpOrder)
IF SEEK(IIF(lcRpPrintBy='C',m.cTktNo,'P'+m.cTktNo),IIF(lcRpPrintBy='C','CutTktH','PosHdr'))
  IF lcRpPrintBy='C'
    SELECT CutTktH
  ELSE
    SELECT PosHdr
  ENDIF
  IF  STATUS<>'X' .AND. &lcCompExp
    WAIT WINDOW "Selecting record for "+IIF(lcRpPrintBy='C','C/T # ','P/O # ')+m.cTktNo NOWAIT
    lcKey = m.cTktNo + m.order
    IF llRpConso AND SEEK(lcKey,lcRpOrder)
      lnAlias = SELECT()
      SELECT (lcRpOrder)
      LOCATE REST WHILE ctktno+order = lcKey ;
                  FOR   STYLE = m.Style
      IF FOUND()
        REPLACE QTY1 WITH QTY1 + m.Qty1,;
                QTY2 WITH QTY2 + m.Qty2,;
                QTY3 WITH QTY3 + m.Qty3,;
                QTY4 WITH QTY4 + m.Qty4,;
                QTY5 WITH QTY5 + m.Qty5,;
                QTY6 WITH QTY6 + m.Qty6,;
                QTY7 WITH QTY7 + m.Qty7,;
                QTY8 WITH QTY8 + m.Qty8,;
                TOTQTY WITH TOTQTY + m.TOTQty;
                lMulStore WITH .T.
      ELSE
        INSERT INTO (lcRpOrder) FROM MEMVAR
      ENDIF
      SELECT (lnAlias)
    ELSE
      INSERT INTO (lcRpOrder) FROM MEMVAR
    ENDIF  
    SCAT MEMVAR MEMO
    IF !SEEK(IIF(lcRpPrintBy='C',m.CutTkt,m.PO),lcRpTrans)
      INSERT INTO (lcRpTrans) FROM MEMVAR
    ENDIF
  ENDIF
ENDIF
