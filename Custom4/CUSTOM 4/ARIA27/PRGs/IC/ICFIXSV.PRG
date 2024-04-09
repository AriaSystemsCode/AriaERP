*:********************************************************************************************
*: Program file  : ICFIXSV (B# 038753)
*: Program desc. : Fix material stock value
*! Date          : 12/19/2004
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : KHALID MOHI EL-DIN
*:********************************************************************************************
*: Modifications:
*:********************************************************************************************

IF !gfSetup()
  RETURN
ENDIF

IF gfGetMemVar('M_COST_MET') <> "S"
  =gfModalGen("INM00000B00000","Dialog",.F.,.F.,;
                 "This program is used in case of style costing method is standard. Cannot proceed.")

  RETURN
ENDIF

=gfModalGen("INM00000B00000","Dialog",.F.,.F.,;
            "Its highly recommended that you run the rebalance databases program before you run this program.")

llClearSty = .T.
llChStyle  = .F.
STORE 0 TO lnLenth

lcExpr = gfOpGrid('ICFIXSVL',.T.)

IF TYPE('lcExpr') = 'L'
  RETURN
ENDIF

IF !("STYINVJL.CADJREASON" $ UPPER(lcexpr))
  =gfModalGen("INM00000B00000","Dialog",.F.,.F.,;
                 "You have to select inventory adjustment reason code. Cannot proceed.")

  RETURN
ENDIF


lnLenth  = LEN(gfItemMask('PM'))
llGlLink = gfGetMemVar('M_Link_GL')  ='Y'
llDyelot = gfGetMemVar('M_Dyelot')   ='Y'
STORE "" TO lcGLFYear,lcGLPeriod,lcAdjLink

=CHECKPRD(gdSysDate,'lcGLFYear','lcGLPeriod', 'IP',.T.)

=gfOpenFile(gcDataDir+'Style','Style','SH')
=gfOpenFile(gcDataDir+'StyDye','StyDye','SH')
=gfOpenFile(gcDataDir+'StyInvJl','StyInvJl','SH')
=gfOpenFile(gcDataDir+"INVTADJ", "INVTADJ", "SH")

IF llGlLink
  =gfOpenFile(gcDataDir+'GLDist','GLDistAc','SH')

  SELECT GLDist
  lcTmpGlDt = gfTempName()
  COPY STRU TO &gcWorkDir.&lcTmpGlDt
  USE (gcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
  lcGlSess = gfsequence('GLSESSION')
ENDIF

lcRefer     = "**Adjust Stock Value**"
lcExpr      = ALLTRIM(lcExpr)
lcAdjReason = SUBSTR(lcExpr,LEN(lcExpr)-7,LEN(lcExpr))
lcAdjReason = &lcAdjReason
lcExpr      = STRTRAN(lcExpr,SUBSTR(lcExpr,ATC("STYINVJL.CADJREASON",lcExpr),len(lcexpr)),'') + ".T."
lcStyFile   = gfTempName()
lcStyDyeF   = gfTempName()
lcTmpAdj    = gfTempName()
lcExpr      = STRTRAN(lcExpr,"STYLE.","")

=gfOpenFile(gcDataDir+'Style','CStyle','SH', @lcStyFile, .T.)
=gfOpenFile(gcDataDir+'StyDye','StyDye','SH', @lcStyDyeF, .T.)

*--Gl adjustment account.
IF llGlLink AND !EMPTY(lcAdjReason)
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjLink'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF

=gfOpenFile(gcDataDir+"INVTADJ", "INVTADJ", "SH", @lcTmpAdj, .T.)


SELECT (lcStyFile)
SCAN FOR &lcExpr

  lcStyMajor = SUBSTR(cStyMajor,1,lnLenth)
  llDyeLvl = (llDyelot AND cDye_Flg = 'Y')
  lcFCond = IIF(llDyeLvl, "!EMPTY(Dyelot)", ".T.")
  
  IF SEEK(lcStyMajor,lcStyDyeF)
    SELECT (lcStyDyeF)
    SCAN REST WHILE Style+cWareCode+Dyelot = lcStyMajor FOR &lcFCond AND TotStk <> 0
      WAIT WINDOW "Updating Style: " + Style + " - Warehouse: " + cWareCode + ;
                   IIF(llDyelot, " - Dyelot: "+Dyelot,"") NOWAIT

      =lpModOK()
    ENDSCAN
    
    SELECT (lcTmpGlDt)
    LOCATE    
     IF llGlLink AND !EOF(lcTmpGlDt)
        WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
        REPLACE ALL GLSESSION WITH lcGlSess
        SELECT GLDIST  
        APPEND FROM &gcWorkDir.&lcTmpGlDt   
        SELECT (lcTmpGlDt)
        ZAP
        WAIT CLEAR
     ENDIF
  ENDIF  

ENDSCAN

*-- Erase distripution  file
IF llGlLink AND !EOF(lcTmpGlDt)
  SELECT (lcTmpGlDt)
  USE
  ERASE (gcWorkDir+lcTmpGlDt+'.DBF')
ENDIF

=gfModalGen("INM00000B00000","Dialog",.F.,.F.,;
                 "Adjusting stock value has been done successfully.")

*!*************************************************************
*! Name      : lpModOK()
*! Developer : KHALID MOHI EL-DIN 
*! Date      : 12/19/2004
*! Purpose   : Updating files
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lpModOK

SELECT (lcStyDyeF)
SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
lnTotStk = TotStk

SELECT (lcTmpAdj)
APPEND BLANK
GATHER FROM laToSave FIELDS OldQty1,OldQty2,OldQty3,OldQty4,OldQty5,OldQty6,OldQty7,OldQty8
GATHER FROM laToSave FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8
REPLACE Style     WITH &lcStyDyeF..Style ,;
        TotOld    WITH lnTotStk ,;
        TotAdj    WITH lnTotStk ,;
        dPostDate WITH gdSysDate ,;
        cReason   WITH lcRefer   ,;
        Date      WITH gdSysDate  ,;
        Type      WITH 'P'        ,; 
        Unt_Cost  WITH &lcStyFile..TotCost ,;
        Old_Cost  WITH &lcStyFile..TotCost

REPLACE dAdd_Date WITH gdSysDate  ,;
        cAdd_Time WITH TIME()     ,;
        cAdd_User WITH gcUser_id  ,;
        GlFYear   WITH lcGlFyear  ,;
        GlPeriod  WITH lcGlPeriod ,;  
        Dyelot    WITH &lcStyDyeF..Dyelot,;
        cFromWare WITH &lcStyDyeF..cWareCode
  
*--Start Updating.
=lfInvUpdt()
SELECT (lcTmpAdj)
REPLACE cSession WITH STYINVJL.cSession


*!*************************************************************
*! Name      : lfInvUpdt
*! Developer : KHALID MOHI EL-DIN 
*! Date      : 12/19/2004
*! Purpose   : Update Inventory Files
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpUnLock
*!*************************************************************
FUNCTION lfInvUpdt

lcLinkCode = IIF(!EMPTY(&lcStyDyeF..GL_Link), &lcStyDyeF..GL_Link, &lcStyFile..Link_Code)
lcLinkCode = IIF(llGlLink, IIF(!EMPTY(lcLinkCode), lcLinkCode, 'DEFDEF'), "")
  
*--G/L Array difinition and initialization.
IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  STORE 'IP' TO laGLDistAr[1,4],laGLDistAr[2,4]
  
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE gdSysDate  TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt  TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjLink
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

*--Adjustment quantity array.
DECLARE laAdjust[9]
SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laAdjust
lnTotCost = IIF(&lcStyFile..Make,&lcStyFile..nmCost1+&lcStyFile..nmCost2+&lcStyFile..nmCost3+;
								 &lcStyFile..nmCost4+&lcStyFile..nmCost5,;
								 &lcStyFile..niCost1+&lcStyFile..niCost2+&lcStyFile..niCost3+;
								 &lcStyFile..niCost4+&lcStyFile..niCost5)
*--Call the global function for update style inventory control.
lnRet = gfStyCrl('2',&lcStyDyeF..Style,&lcStyDyeF..cWareCode,&lcStyDyeF..Dyelot,gdSysDate,'',;
                 @laAdjust,lnTotCost,lcRefer,.T.,lcAdjReason,0,'','',@laGLDistAr,0,"","")

RETURN

*!*************************************************************
*! Name      : lfSRVSty
*! Developer : KHALID MOHI EL-DIN 
*! Date      : 12/19/2004
*! Purpose   : control browsing primary style and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Example   : =lfSRVSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major 
  *-- unique index.
  USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
ELSE  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDIF
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : KHALID MOHI EL-DIN 
*! Date      : 12/19/2004
*! Purpose   : Sum a specific field for the current style in style file
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X

  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(LEFT(Style,lnLenth)) == ALLTRIM(lcSty)

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
*-- end of lfStySum.