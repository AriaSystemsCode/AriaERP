*!**************************************************************************
*! Name      : PLAMAIN.PRG
*! Developer : Nader NABIL (NNA)
*! Date      : 01/16/2005
*! Purpose   : new way to make a sales order invoice called (Back Flush Invoice)
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C125587,1
*!**************************************************************************
*! Modifications
*!**************************************************************************

*!* laScrMode [1] --> SELECT MODE
*!* laScrMode [2] --> VIEW   MODE
*!* laScrMode [3] --> EDIT   MODE
*!* laScrMode [4] --> ADD    MODE

PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*!*Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : lfFlushScr
*! Developer : Nader NABIL (NNA)
*! Date      : 01/16/2005
*! Purpose   : Function to check if user want to use the standard SO invoice
*!           : screen or the Back Flush Invoice screen
*!**************************************************************************
*! Example   : lfFlushScr()
*!**************************************************************************
*! C125587,1 NADER 01/16/2005
*!**************************************************************************
FUNCTION lfFlushScr
PRIVATE lnOldAlias
STORE 0 TO lnOldAlias
lnOldAlias = SELECT(0)
lcTrgCursr = lcInvLine + 'A'
SELECT (lcTrgCursr)
IF !EVAL(lcTrgCursr+'.llChecked') AND gfModalGen("QRM00000B40000",.F.,.F.,.F.,'Do you want to use the Back Flush Invoice screen?' )=1
  REPLACE llFlushScr WITH .T.  && In this case we'll work with the Back Flush Invoice Screen
ENDIF
REPLACE llChecked  WITH .T.
SELECT(lnOldAlias)

*!*END OF FUNCTION lfFlushScr
*!**************************************************************************
*! Name      : lfPLACRTMP
*! Developer : Nader NABIL (NNA)
*! Date      : 01/16/2005
*! Purpose   : Create a new cursor
*!**************************************************************************
*! Example   : lfPLACRTMP()
*!**************************************************************************
*! C125587,1 NADER 01/16/2005
*!**************************************************************************
FUNCTION lfPLACRTMP
lcTrgCursr = lcInvLine + 'A'
CREATE CURSOR (lcTrgCursr) (llChecked L(1),llFlushScr L(1),HoldGlArr M,HoldRef M)
INSERT INTO (lcTrgCursr) (llChecked,llFlushScr,HoldGlArr,HoldRef) VALUES (.F.,.F.,'','')
RETURN ''

*!*END OF FUNCTION lfPLACRTMP
*!**************************************************************************
*! Name      : lfCHECKOBJ
*! Developer : Nader NABIL (NNA)
*! Date      : 01/16/2005
*! Purpose   : IF user decide to use the Back Flush Invoice Screen then
*!           : I'll set filter to the open orders in the OrdHdr and make the
*!             piktkt browse disable (if AL installed)
*!**************************************************************************
*! Example   : lfCHECKOBJ()
*!**************************************************************************
*! C125587,1 NADER 01/16/2005
*!**************************************************************************
FUNCTION lfCHECKOBJ
PRIVATE lnOldAlias
STORE 0 TO lnOldAlias
lnOldAlias = SELECT(0)
lcTrgCursr = lcInvLine + 'A'
SELECT (lcTrgCursr)
IF EVAL(lcTrgCursr+'.llFlushScr')
  SELECT ORDHDR
  IF EMPTY(SET('FILTER'))
    SET FILTER TO STATUS='O'
  ELSE
    SET FILTER TO 
  ENDIF
  IF 'AL' $ gcCmpModules 
    SHOW GET ibPikTkt  DISABLE
    SHOW GET laData[3] DISABLE    
  ENDIF
ENDIF
SELECT(lnOldAlias)

*!*END OF FUNCTION lfCHECKOBJ.
*!**************************************************************************
*! Name      : lfArrCopy
*! Developer : Nader NABIL (NNA)
*! Date      : 01/16/2005
*! Purpose   : Hold the laGlArray Array and lcRefer Variable from Arinv.prg
*!**************************************************************************
*! Example   : lfArrCopy()
*!**************************************************************************
*! C125587,1 NADER 01/16/2005
*!**************************************************************************
FUNCTION lfArrCopy
PRIVATE lnOldAlias
STORE 0 TO lnOldAlias
lnOldAlias = SELECT(0)
lcTrgCursr = lcDetFile + 'A'
SELECT (lcTrgCursr)
IF EVAL(lcTrgCursr+'.llFlushScr')
  SAVE TO MEMO HoldGlArr ALL LIKE laGlArray
  SAVE TO MEMO HoldRef   ALL LIKE lcRefer
ENDIF
SELECT(lnOldAlias)

*!*END OF FUNCTION lfArrCopy.
*!**************************************************************************
*! Name      : lfPlaSave
*! Developer : Nader NABIL (NNA)
*! Date      : 01/16/2005
*! Purpose   : Update the StyinvJl and MatInvJl file
*!**************************************************************************
*! Example   : lfPlaSave()
*!**************************************************************************
*! C125587,1 NADER 01/16/2005
*!**************************************************************************
FUNCTION lfPlaSave
PRIVATE lnStyCost , lcRefer , lcGlSession , lcDistFile , lnMjrWid , lnUseQty , lcMatFile ,;
        llUpdSty
STORE '' TO lcNonMjr, lcRefer , lcGlSession , lcDistFile, lcMjrPct , lcMatFile
STORE 0 TO lnUseQty , lnStyCost , lnMjrWid , lnClrPo , lnColorLen
STORE .F. TO llUpdSty
lcTrgCursr = lcInvLine + 'A'
IF !EVAL(lcTrgCursr+'.llFlushScr')
  RETURN
ENDIF
*!* Array to hold sizes from BOM.Msizes if style costing by size
DECLARE laSizes[1]
*!*Create Temp File to collect all Fabrics that used in the Order Styles with its Accumulated required Qty.
lcMatFile = gfTempName()
CREATE TABLE (gcWorkDir + lcMatFile)(FABRIC C(7),COLOR C(6),DYELOT C(10),cWareCode C(6),;
              nAve_Cost N(6),NRequired N(6))
INDEX ON (FABRIC+COLOR) TAG (lcMatFile) OF (lcMatFile)

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')
lcMjrPct  = gfItemMask('PM')
lnMjrWid  = LEN(lcMjrPct)
= lfNonMaj()
IF SEEK(laData[4]+laData[1],lcInvLine)
  DECLARE laInvSetup[10,2]
  laInvSetup[1,1] = 'M_LINK_GL'          &&  Check for Gl link
  laInvSetup[2,1] = 'M_WareHouse'        &&  use maltiple locations Y Or N  Ic Setup
  laInvSetup[3,1] = 'M_COST_METH'        &&  Get the style cost method
  laInvSetup[4,1] = 'M_DYELOT'           &&  Use Dylot Y Or N
  laInvSetup[5,1] = 'M_DIV_LINK'         &&  GL link codes at Division level
  laInvSetup[6,1] = 'M_STY_COM'          &&  Commision at Style level
  laInvSetup[7,1] = 'M_TAX'              &&  use Taxes Y or N
  laInvSetup[8,1] = 'XPOSTFINV'          &&  Post Factored invoice to customer
  laInvSetup[9,1] = 'XAGINGTYPE'         &&  Aging AR by Date\Terms
  laInvSetup[10,1]= 'M_GenOrNum'         &&  Enter order # Manually Y Or N
  =gfGetMemVar(@laInvSetup,gcAct_Comp)
  lcGlSession = gfsequence('GLSESSION')
  SELECT (lcTrgCursr)
  *!*Restore laGlArray
  RESTORE FROM MEMO HoldGlArr ADDITIVE
  *!*Restore lcRefer
  RESTORE FROM MEMO HoldRef   ADDITIVE
  *!* Create lcDistFile 
  lcDistFile = gfTempName()
  SELECT GLDIST
  =AFIELDS(laFileStru)
  =gfCrtTmp(lcDistFile,@laFileStru,.F.,'')
  STORE lcDistFile TO laGlArray[1,9],laGlArray[2,9]
  DECLARE laAdjust[9]
  SELECT (lcInvLine)
  SCAN REST WHILE Account+Order+Store+Piktkt+STR(Lineno,6) = laData[4]+laData[1]
    llUpdSty = .F.
    lcNonMjr   = SUBSTR(STYLE.Style,lnClrPo,lnColorLen)
    STORE 0 TO laAdjust
    SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TotQty TO laAdjust
    *!* Compare Between the Required Qty. and Stock per Size
    STORE 0 TO laAdjust[9]
    FOR Q = 1 TO 8
      LCQ = ALLTRIM(STR(Q))
      *!* For Example: if Stk1 before invoicing = 3 and was Required in order 5 then here
      *!* in this Function Stock Already updated to become(-2) so adding the Required to current
      *!* stock give us the Previous Stock that equal (5+(-2)=3) after that I compare between 
      *!* Required Qty and Stock to know that we need 2 Pieces to complete this size.
      IF (STYLE.STK&LCQ+laAdjust[Q]) < laAdjust[Q] 
        llUpdSty = .T.
        *--if Stock less than Zero then I'll update only with Qty. ordered
        IF (STYLE.STK&LCQ+laAdjust[Q]) >= 0
          STORE (laAdjust[Q] - (STYLE.STK&LCQ+laAdjust[Q])) TO laAdjust[Q]        
          STORE laAdjust[9]+laAdjust[Q] TO laAdjust[9]
        ENDIF
      ELSE
        STORE 0 TO laAdjust[Q]        
      ENDIF
    ENDFOR
    lnStyCost = IIF(laInvSetup[3,2]='A',;
                IIF(laInvSetup[2,2]='Y',StyDye.Ave_Cost,STYLE.Ave_Cost),STYLE.TotCost)
    SELECT (lcInvLine)
    *!*If there isn't Enough Stock then we'll update styinvjl and matinvjl with the needed Qty.
    IF llUpdSty
      =gfStyCrl('1',&lcInvLine..STYLE,&lcInvLine..CWARECODE,&lcInvLine..DYELOT,INVHDR.DPOSTDATE,laData[1],@laAdjust,;
               lnStyCost,lcRefer,lcGlSession,'',0,lcInvLine,'',@laGlArray,&lcInvLine..LineNo)                    
      =lfUpdMatFb(SUBSTR(Style,1,lnMjrWid),&lcInvLine..Style,lcNonMjr,laAdjust[9])
    ENDIF
  ENDSCAN
ENDIF                    
*!*Update the MatinvJl File
IF RECCOUNT(lcMatFile) > 0
  DECLARE laGLDistAr[2,13]
  lcLinkCode = IIF(EMPTY(FABDYE.Gl_Link),Fabric.Link_Code,FABDYE.Gl_Link)
  ldPost     = INVHDR.DPOSTDATE
  laGLDistAr[1,1] = IIF(EMPTY(lcLinkCode),'DEFDEF',lcLinkCode)
  laGLDistAr[2,1] = IIF(EMPTY(lcLinkCode),'DEFDEF',lcLinkCode)
  laGLDistAr[1,2] = '015'
  laGLDistAr[2,2] = '013'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'MO'       TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE ldPost     TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE laGlArray[1,7]  TO laGLDistAr[1,7],laGLDistAr[2,7]  && FISCAL YEAR
  STORE laGlArray[1,8]  TO laGLDistAr[1,8],laGLDistAr[2,8]  && FISCAL PERIOD
  STORE lcDistFile TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = ''
  SELECT(lcMatFile)
  IF !EOF()
    SCAN
      =gfMatCrl('2',Fabric,Color,cWareCode,Dyelot,DATE(),ldPost,laData[1],;
                    NRequired*-1,nAve_Cost,'','',0,'','',@laGLDistAr)
    ENDSCAN
  ENDIF
ENDIF
USE IN &lcMatFile
ERASE (gcWorkDir + lcMatFile+".DBF")
ERASE (gcWorkDir + lcMatFile+".CDX")
*!*END OF FUNCTION lfPlaSave.
*!***************************************************************************
*! Name      : lfNonMaj
*! Developer : Nader NABIL (NNA)
*! Date      : 01/16/2005
*! Purpose   : To get the style nonmajor segement structure
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Calls       : ....
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
*! Example     : = lfNonMaj()
*!***************************************************************************
FUNCTION lfNonMaj

PRIVATE lnOldAlias
STORE 0 TO lnOldAlias
lnOldAlias = SELECT(0)
*!* Compute Free/Color Items in Style Structure. [Begin]
*!* Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *!* If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *!* this means that another type is found rather than color or free
      *!* and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
SELECT(lnOldAlias)
RETURN ''
*!*End function of lfNonMaj.
*!**************************************************************************
*! Name      : lfUpdMatFb
*! Developer : Nader NABIL (NNA)
*! Date      : 01/16/2005
*! Purpose   : Update the MatinvJl file 
*!**************************************************************************
*! Example   : lfUpdMatFb()
*!**************************************************************************
*! C125587,1 NADER 01/16/2005
*!**************************************************************************
FUNCTION lfUpdMatFb
PARAMETERS lcStyMajr,lcStyleClr,lcColor,lnStyQty
IF !USED('BOM')
  =gfOpenFile(gcDataDir+'BOM',gcDataDir+'BOM','SH')
ENDIF
IF !USED('FABRIC')
  =gfOpenFile(gcDataDir+'FABRIC',gcDataDir+'FABRIC','SH')
ENDIF
IF !USED('FABDYE')
  =gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYE','SH')
ENDIF
SELECT BOM
IF EMPTY(SET('RELATION'))
  SET RELATION TO ALLTRIM(ITEM) INTO FABRIC ADDITIVE
ENDIF
SELECT FABRIC
IF EMPTY(SET('RELATION'))
  SET RELATION TO FABRIC + COLOR INTO FABDYE ADDITIVE
ENDIF
IF SEEK(lcStyMajr,'BOM') 
 SELECT BOM
 SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = lcStyMajr FOR CcatgTyp='F'
   IF '*' $ citmmask
     lcKey = IIF('*' $ Iclr ,'SUBSTR(BOM.ITEM,1,7)+lcColor','SUBSTR(BOM.ITEM,1,7)+BOM.ICLR')
     IF SEEK(&lcKey,'FABRIC')
       SELECT FABRIC
       SCAN REST WHILE Fabric+Color =&lcKey
         lnUseQty = lnStyQty * BOM.nEstBomQty
         *!*If style costing by size then get the used Qty Per size Else get for total Pieces of Style
         lnUseQty = IIF(BOM.lBasonsiz AND !EMPTY(BOM.Msizes),lfgetSzQty()* BOM.nEstBomQty,;
                        lnUseQty)
         SELECT(lcMatFile)
         IF SEEK(Fabric.Fabric+Fabric.Color)
           REPLACE NRequired WITH NRequired + lnUseQty
         ELSE
           INSERT INTO &lcMatFile (FABRIC,COLOR,DYELOT,cWareCode,nAve_Cost,NRequired) VALUES ;
                  (FABDYE.Fabric,FABDYE.Color,FABDYE.Dyelot,FABDYE.cWareCode,FABDYE.nFave_Cost,;
                  lnUseQty)
         ENDIF
       ENDSCAN
     ENDIF
   ELSE
     IF citmmask = lcStyleClr
       lcKey = IIF('*' $ Iclr ,'SUBSTR(BOM.ITEM,1,7)+lcColor','SUBSTR(BOM.ITEM,1,7)+BOM.ICLR')
       IF SEEK(&lcKey,'FABRIC')
         SELECT FABRIC
         SCAN REST WHILE Fabric+Color=&lcKey
           lnUseQty = lnStyQty * BOM.nEstBomQty
           SELECT(lcMatFile)
           IF SEEK(Fabric.Fabric+Fabric.Color)
             REPLACE NRequired WITH NRequired + lnUseQty
           ELSE
             INSERT INTO &lcMatFile (FABRIC,COLOR,DYELOT,cWareCode,nAve_Cost,NRequired) VALUES ;
                    (FABDYE.Fabric,FABDYE.Color,FABDYE.Dyelot,FABDYE.cWareCode,FABDYE.nFave_Cost,;
                    lnUseQty)
           ENDIF
         ENDSCAN
       ENDIF
     ENDIF
   ENDIF
 ENDSCAN
ENDIF
*!*END OF FUNCTION lfUpdMatFb.
*!**************************************************************************
*! Name      : lfgetSzQty
*! Developer : Nader NABIL (NNA)
*! Date      : 01/16/2005
*! Purpose   : Get the Required Qty. for Size if style Costing By Size
*!**************************************************************************
*! Example   : lfgetSzQty()
*!**************************************************************************
*! C125587,1 NADER 01/16/2005
*!**************************************************************************
FUNCTION lfgetSzQty
PRIVATE lnCurAlias , lcLineTxt , lnRequired
STORE 0 TO lnRequired , lnCurAlias
STORE '' TO lcLineTxt
lnCurAlias = SELECT(0)
SELECT BOM
FOR M = 1 TO MEMLINES(Msizes)		&& Loop for The Memo Field Lines
  lcLineTxt = SUBSTR(MLINE(Msizes,M),5)
  =gfSubStr(lcLineTxt,@laSizes,',') && Hold the Memeo Field Sizes in an Array        
  FOR N = 1 TO ALEN(laSizes,1)		&& Loop for The Array Length
    FOR P = 1 TO 8					&& Loop For the Style sizes
      LCP = ALLTRIM(STR(P))
      IF laSizes[N] = LCP			&& if the Current Style Sizes is the Same as the hold one in the memo Field
        lnRequired = lnRequired + laAdjust[P]
      ENDIF
    ENDFOR
  ENDFOR
ENDFOR
SELECT(lnCurAlias)
RETURN lnRequired
*!*END OF FUNCTION lfgetSzQty.
