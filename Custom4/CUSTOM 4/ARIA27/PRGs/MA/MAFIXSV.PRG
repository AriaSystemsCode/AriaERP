*:********************************************************************************************
*: Program file  : MAFIXSV (B# 038753)
*: Program desc. : Fix material stock value
*! Date          : 12/19/2004
*: System        : Aria Advantage Series.
*: Module        : MATERIALS (MA)
*: Developer     : KHALID MOHI EL-DIN
*:********************************************************************************************
*: Modifications:
*:********************************************************************************************

IF !gfSetup()
  RETURN
ENDIF

IF gfGetMemVar('M_MATCSTMT') <> "S"
  =gfModalGen("INM00000B00000","Dialog",.F.,.F.,;
                 "This program is used in case of material costing method is standard. Cannot proceed.")

  RETURN
ENDIF

=gfModalGen("INM00000B00000","Dialog",.F.,.F.,;
            "Its highly recommended that you run the rebalance databases program before you run this program.")

lcExpr = gfOpGrid('MAFIXSVL',.T.)

IF TYPE('lcExpr') = 'L'
  RETURN
ENDIF

IF !("MATINVJL.CADJREASON" $ UPPER(lcexpr))
  =gfModalGen("INM00000B00000","Dialog",.F.,.F.,;
                 "You have to select inventory adjustment reason code. Cannot proceed.")

  RETURN
ENDIF

llGlLink = gfGetMemVar('M_Link_GL')  ='Y'
llDyelot = gfGetMemVar('M_MATCSTMT')   ='Y'
STORE "" TO lcGLFYear,lcGLPeriod,lcAdjLink

=CHECKPRD(gdSysDate,'lcGLFYear','lcGLPeriod', 'IP',.T.)

=gfOpenFile(gcDataDir+'Fabric','Fabric','SH')
=gfOpenFile(gcDataDir+'FabDye','FabDye','SH')
=gfOpenFile(gcDataDir+'MatInvJl','MatInvJl','SH')
=gfOpenFile(gcDataDir+"FINVTADJ", "FINVTADJ", "SH")

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
lcExpr      = STRTRAN(lcExpr,SUBSTR(lcExpr,ATC("MATINVJL.CADJREASON",lcExpr),len(lcexpr)),'') + ".T."
lcFCond     = IIF(llDyelot, "!EMPTY(Dyelot)", ".T.")
lcFabFile   = gfTempName()
lcFabDyeF   = gfTempName()
lcTmpAdj    = gfTempName()
lcExpr      = STRTRAN(lcExpr,"FABRIC.","")

=gfOpenFile(gcDataDir+'Fabric','CFabric','SH', @lcFabFile, .T.)
=gfOpenFile(gcDataDir+'FabDye','FabDye','SH', @lcFabDyeF, .T.)

*--Gl adjustment account.
IF llGlLink AND !EMPTY(lcAdjReason)
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjLink'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF

=gfOpenFile(gcDataDir+"FINVTADJ", "FINVTADJ", "SH", @lcTmpAdj, .T.)


SELECT (lcFabFile)
SCAN FOR &lcExpr

  lcFabric = Fabric
  llDyeLvl = (llDyelot AND cDye_Flg = 'Y')
  lcFCond = IIF(llDyeLvl, "!EMPTY(Dyelot)", ".T.")
  
  IF SEEK(lcFabric,lcFabDyeF)
    SELECT (lcFabDyeF)
    SCAN REST WHILE Fabric+Color+cWarecode+Dyelot = lcFabric FOR &lcFCond AND OnHand <> 0
      WAIT WINDOW "Updating Fabric/Color: " + Fabric+"/"+Color + " - Warehouse: " + cWareCode + ;
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

SELECT (lcFabDyeF)
lnTotStk = OnHand

SELECT (lcTmpAdj)
APPEND BLANK
REPLACE Fabric     WITH &lcFabDyeF..Fabric ,;
        Color      WITH &lcFabDyeF..Color,;
        OldQty     WITH lnTotStk ,;
        nmTotAdj   WITH lnTotStk ,;
        Date       WITH gdSysDate ,;
        cReason    WITH lcRefer ,;
        Date       WITH gdSysDate ,;
        Type       WITH 'P' ,;
        nFUnitCost WITH &lcFabFile..CostUse  ,;
        nUntCstBuy WITH &lcFabFile..CostBuy

REPLACE dAdd_Date WITH gdSysDate  ,;
        cAdd_Time WITH TIME()     ,;
        cAdd_User WITH gcUser_id  ,;
        GlFYear   WITH lcGlFyear  ,;
        GlPeriod  WITH lcGlPeriod ,;
        Dyelot    WITH &lcFabDyeF..Dyelot,;
        cFromWare WITH &lcFabDyeF..cWareCode
  
*--Start Updating.
=lfInvUpdt()
SELECT (lcTmpAdj)
IF !EOF('MATINVJL')
  REPLACE CTRN_SEQ   WITH MATINVJL.CTRN_SEQ ,;
          NFUNITCOST WITH MATINVJL.NUNITCOST
ENDIF


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

lcLinkCode = IIF(!EMPTY(&lcFabDyeF..GL_Link), &lcFabDyeF..GL_Link, &lcFabFile..Link_Code)
lcLinkCode = IIF(llGlLink, IIF(!EMPTY(lcLinkCode), lcLinkCode, 'DEFDEF'), "")
  
*--G/L Array difinition and initialization.
IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '015'
  laGLDistAr[2,2] = '016'
  STORE 'MP' TO laGLDistAr[1,4],laGLDistAr[2,4]
  
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
laAdjust[1] = &lcFabDyeF..OnHand

*--Call the global function for update style inventory control.
lnRet = gfMatCrl('3',&lcFabDyeF..Fabric, &lcFabDyeF..Color,;
                 &lcFabDyeF..cWareCode,&lcFabDyeF..Dyelot,gdSysDate,;
                 gdSysDate,SPACE(6),laAdjust[1],&lcFabFile..CostUse,;
                 lcRefer,lcAdjReason,0,'','',@laGLDistAr)

RETURN


*!*************************************************************
*! Name      : lfvFabric
*! Developer : KHALID MOHI EL-DIN 
*! Date      : 12/19/2004
*! Purpose   : VALIDATE THE FABRIC
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
FUNCTION lfvFabric

PRIVATE lcFabld,lcFabrc,lnSelFile,lcFaTag 

lcFabld   = VARREAD()
lcFabrc  = EVAL(lcFabld)

lnSelFile = SELECT(0)

SELECT FABRIC
lcFaTag   = ORDER('FABRIC')

SET ORDER TO TAG FABRIC IN FABRIC

IF !EMPTY(lcFabrc) .AND. ('?' $ lcFabrc .OR. !SEEK(lcFabrc , 'FABRIC'))
  =FABROW (@lcFabrc,'*')
ENDIF
 
&lcFabld = lcFabrc

SET ORDER TO lcFaTag

SELECT (lnSelFile)

*!*************************************************************
*! Name      : lfSRVFab
*! Developer : KHALID MOHI EL-DIN 
*! Date      : 12/19/2004
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : KHALID MOHI EL-DIN 
*! Date      : 12/19/2004
*! Purpose   : Sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.