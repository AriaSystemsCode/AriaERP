*****************************************************************************
*: Program file  : SOALOCP.PRG (E# 301381)
*: Program desc. : CutTkt/Po Allocation Report.
*: Module        : SO (Sales Order)
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls : FUNCTIONS  : lfCreat,lfGetCutPo,lfAppend,lfGetOgVar,lfCretExp
*:                      lfWRep,lfClrRed,lfsrCut,lfsrPO,lfvStyle,lfwOldVal
*:                      lfClearRep
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
* Modifications:
* B604595,1 ADEL 27/06/2001 1- Fix the bug of wrong PO totals in Text mode (IN FRX).
* B604595,1                 2- Fix the bug of "No recors to display".
* B804356,1 AAN 20/09/2001 Check about the installed modules before run the report.
* B605898,1 TMI 06/16/2002 Allow to filter on the order status
*****************************************************************************

*--- Get Style Picture
*B804356,1 AAN Check about the installed modules before run the report[Begin]
PRIVATE llExit
llExit = .F.
=lfChckMod()
IF llExit
  RETURN
ENDIF
*B804356,1 AAN Check about the installed modules before run the report[End]
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
  *B605898,1 TMI [Start] Add OG Vars
  lcStGrp = ''
  lcFabrc = ''
  lcStats = ''
  *B605898,1 TMI [End  ] Add OG Vars
  =lfGetOgVar()
  *--- Expretions Variable.
  llTrnsExp = .F.
  lcCompExp = '.T.'
  lcDivExp  = ''
  lcSesExp  = ''
  lcCutPick = '.T.'
  *B605898,1 TMI [Start] Expr Vars
  lcStGrpExp = ''
  lcFabrcExp = ''
  lcStatsExp = ''
  *B605898,1 TMI [End  ] Expr Vars 
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
*! Developer : AHMED SALAH SHALABY - (SSH)
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
COPY STRUCTURE TO &gcWorkDir.&lcRpOrder
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
*! Developer : AHMED SALAH SHALABY - (SSH)
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
*B605898,1 TMI [Start] Add a relation for ORDHDR
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDIT
*B605898,1 TMI [End  ] Add a relation for ORDHDR
*B604595,1 (Begin) If the user browses PO/CUT and doesn't select any, then llTrnsExp must be .F.
IF !USED(lcCutPo) OR RECCOUNT(lcCutPo) = 0
  llTrnsExp = .F.
ENDIF
*B604595,1 (End)
IF llTrnsExp
  SELECT (lcCutPo)
  GOTO TOP
ELSE
  SELECT CutPick
  =SEEK(IIF(lcRpPrintBy='C','1','2'))
ENDIF
SCAN  REST WHILE IIF(llTrnsExp,.T., trancd+ctktno+style = IIF(lcRpPrintBy='C','1','2'));
           FOR IIF(llTrnsExp,.T.,&lcCutPick)
  IF llTrnsExp
    SELECT (lcCutPo)
    lcTempCutPo = IIF(lcRpPrintBy='C',CutTkt,Po)
    SELECT CutPick
    =IIF(llTrnsExp, SEEK(IIF(lcRpPrintBy='C','1'+lcTempCutPo,'2'+lcTempCutPo)), .T.)
    SCAN REST WHILE IIF(llTrnsExp, trancd+ctktno+style = IIF(lcRpPrintBy='C','1'+lcTempCutPo,'2'+lcTempCutPo), .T.);
              FOR &lcCutPick
      =lfAppend()
    ENDSCAN
  ELSE
    =lfAppend()
  ENDIF
ENDSCA

*!*************************************************************
*! Name      : lfAppend  (E# 301381)
*! Developer : AHMED SALAH SHALABY - (SSH)
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
*! Developer : AHMED SALAH SHALABY - (SSH)
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

*B605898,1 TMI [Start] Get OG Vars
FOR lnInd  = 1 TO ALEN(laOgVrFlt,1)
  DO CASE
    CASE ALLTRIM(laOgVrFlt[lnInd,1]) = 'STYLE.CSTYGROUP' .AND. !EMPTY(ALLTRIM(laOgVrFlt[lnInd,6]))
      lcStGrp = laOgVrFlt[lnInd,6]
    CASE ALLTRIM(laOgVrFlt[lnInd,1]) = 'STYLE.FABRIC' .AND. !EMPTY(ALLTRIM(laOgVrFlt[lnInd,6]))
      lcFabrc = laOgVrFlt[lnInd,6]
    CASE  ALLTRIM(laOgVrFlt[lnInd,1]) = 'ORDHDR.STATUS' .AND. !EMPTY(ALLTRIM(laOgVrFlt[lnInd,6]))
      lcStats = laOgVrFlt[lnInd,6]
  ENDCASE
ENDFOR
*B605898,1 TMI [End  ] Get OG Vars

*!*************************************************************
*! Name      : lfCretExp  (E# 301381)
*! Developer : AHMED SALAH SHALABY - (SSH)
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

*B605898,1 TMI [Start] Create Expressions
*lcCutPick = " IIF(!EMPTY(lcStyExp), EVAL(lcStyExp),.T. ) .AND. EVAL(lcDivExp) .AND. EVAL(lcSesExp)"
lcStGrpExp = IIF(!EMPTY(lcStGrp),'STYLE.CSTYGROUP $ lcStGrp','.T.')
lcFabrcExp = IIF(!EMPTY(lcFabrc),'STYLE.FABRIC $ lcFabrc'   ,'.T.')
lcStatsExp = IIF(!EMPTY(lcStats),'ORDHDR.STATUS $ lcStats'  ,'.T.')

lcCutPick = " IIF(!EMPTY(lcStyExp), EVAL(lcStyExp),.T. ) .AND. EVAL(lcDivExp) .AND. EVAL(lcSesExp)"+;
            " .AND. EVAL(lcStGrpExp) .AND. EVAL(lcFabrcExp) .AND. EVAL(lcStatsExp)"
*B605898,1 TMI [End  ] Create Expressions

*!*************************************************************
*! Name      : lfWRep  (E# 301381)
*! Developer : AHMED SALAH SHALABY - (SSH)
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


*!*************************************************************
*! Name      : lfClrRed  (E# 301381)
*! Developer : AHMED SALAH SHALABY - (SSH)
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
*! Developer : AHMED SALAH SHALABY - (SSH)
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
*! Developer : AHMED SALAH SHALABY - (SSH)
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
*! Developer : AHMED SALAH SHALABY - (SSH)
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
*! Developer : AHMED SALAH SHALABY - (SSH)
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
*! Developer : AHMED SALAH SHALABY - (SSH)
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
*! Name      : lfChckMod  (B# 804356)
*! Developer : AHMED Abdel Naby - (AAN)
*! Date      : 09/20/2001
*! Purpose   : Function To Check for the modules.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfChckMod()
*!*************************************************************
*!
FUNCTION lfChckMod
IF lcRpPrintBy='C'
  IF OCCURS('MF',gcCmpModules)=0
    =gfModalGen('TRM32096B00000','ALERT','MF'))
    llExit = .T.
    RETURN
  ELSE
    llExit = .F.
  ENDIF
ELSE
  IF OCCURS('PO',gcCmpModules)=0
    =gfModalGen('TRM32096B00000','ALERT','PO'))
    llExit = .T.
    RETURN
  ELSE
    llExit = .F.
  ENDIF
ENDIF


*:**************************************************************************
*:* Name        : lfvFabric                                       *B605898,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/16/2002
*:* Purpose     : Valid function for fabric
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvFabric()
*:***************************************************************************
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