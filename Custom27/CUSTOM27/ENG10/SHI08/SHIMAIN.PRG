*:****************************************************************************************
*: Program file  : SHIMAIN.PRG
*: Program desc. : Main Program.
*: System        : Aria Apparel System (A27).
*: Developer     : ALBERT RAIF - (ALB) Due to C#200444,1
*: Date          : 12/23/2002
*: Purpose       : ENG Custom Process Program .
*:****************************************************************************************
*: Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*:             lcFunPars -> Process function parameters, sent as a string.
*:****************************************************************************************
*: Returns   : Logical value.       
*:****************************************************************************************
*C200444,1
*C200447,1
*C200449,1
*C200451,1 ABD 01/27/2003 Add new function for the standared material Requierment report.
*B607333,1 ALB 07/10/2003 Use period and fiscal year for receiving date not system default
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************
*
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'
*-- Run the function.
llLinkToGl   = IIF(gfGetMemVar('M_LINKEDGL') = 'Y',.T.,.F.)
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!*************************************************************
*! Name      : lfUpdtRcGl
*! Developer : Albert Raif (ALB)
*! Date      : 12/19/2002
*! Purpose   : Add GL entries in gldist
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfUpdtRcGl()
*!*************************************************************
*!C200444,1
FUNCTION lfUpdtRcGl
IF &lcTmpLine..cStyType $ "P"
  lcOldAlias = ALIAS()
  DIMENSION laMfgRFld[7,2]
  IF llLinkToGl
    STORE ''  TO laMfgRFld,lcmfgglacnt,lcContCode,lcContName,lcOperSeq,llInHouse,llMfgOpr,lnLeadTime
    laMfgRFld[1,1] = 'CCONTCODE'
    laMfgRFld[1,2] = 'lcContCode'
    laMfgRFld[2,1] = 'CCONTNAME'
    laMfgRFld[2,2] = 'lcContName'
    laMfgRFld[3,1] = 'COPERSEQ'
    laMfgRFld[3,2] = 'lcOperSeq'
    laMfgRFld[4,1] = 'LINHOUSE'
    laMfgRFld[4,2] = 'llInHouse'
    laMfgRFld[5,1] = 'LMFGOPR'
    laMfgRFld[5,2] = 'llMfgOpr'
    laMfgRFld[6,1] = 'LEADTIME'
    laMfgRFld[6,2] = 'lnLeadTime'
    laMfgRFld[7,1] = 'GLACCOUNT'
    laMfgRFld[7,2] = 'lcMfgGlAcnt'

    lcPo_No = ''
    lnTotGlOpr = 0
    *---Get some informatiom one time for each po
    IF !USED('CODES')
      =gfOpenFile(gcDataDir+'CODES','','SH')
    ENDIF
    IF !USED('MFGOPRHD')
      =gfOpenFile(gcDataDir+'MFGOPRHD','TKTOPER','SH')
    ENDIF

    *---Get total of operation have Gl
    lcStyle = &lcTmpLine..STYLE
    SELECT BOMLINE
    lcOldOrder = ORDER()
    SET ORDER TO MFGOPR

    =SEEK('I'+&lcTmpLine..po)
    SUM REST ITEMAMT WHILE cImTyp+cTktNo+cOprCode+cType+Style+SClr+;
                           IIF(cCatgTyp$"MDP",PADR(MfgCode,12),Item)+IClr = 'I'+&lcTmpLine..po ;
        FOR gfRltFld(BOMLINE.MFGCODE,@laMfgRFld,'MFGCODE') .AND. !EMPTY(lcMfgGlAcnt) AND CTYPE='1';
        .AND. Style = lcStyle TO lnTotGlOpr

    SET ORDER TO &lcOldOrder.
    *---Get total amount of received operation (Non GL)
    SELECT MFGOPRHD
    lcOldOrder = ORDER()
    SET ORDER TO TKTOPER
    lcLastOpr = ''
    =SEEK('I'+&lcTmpLine..po)
    SCAN WHILE cimtyp+ctktno+coperseq = 'I'+&lcTmpLine..po
      lcLastOpr = COPRCODE
    ENDSCAN
    SET ORDER TO &lcOldOrder
    
    SELECT CTKTBOM
    lcOldOrder = ORDER()
    SET ORDER TO CTKTBOM
    =SEEK('I'+&lcTmpLine..po)
    lnUnitOpCst = 0
    SCAN REST WHILE cImTyp+CutTkt+Typ+Item+IClr+MfgCode+Dyelot = 'I'+&lcTmpLine..Po;
              FOR   !EMPTY(MfgCode)      
      IF !('AP' $ gcCmpModules) .OR. ;
         (cCatGTyp = 'M' .AND. gfRltFld(CTKTBOM.MFGCODE,@laMfgRFld,'MFGCODE') ;
          .AND. EMPTY(lcMfgGlAcnt)) AND llMfgOpr AND (CTKTBOM.MFGCODE $ lcLastOpr )
      
        lnUnitOpCst = lnUnitOpCst + (Untcost*untqty)
      ENDIF
    ENDSCAN
    SET ORDER TO &lcOldOrder.

    *----Update the GLDIST
    lnTotNGLOp = lnUnitOpCst * laAdjust[9]
    SELECT (lcGlDist)
    GOTO TOP
    SCAN FOR EMPTY(GLSESSION) AND CATG_KEY = '013'
      SCATTER MEMVAR MEMO
      REPLACE GLSESSION WITH 'DONE'                              ,;
              NGLAMOUNT WITH NGLAMOUNT + lnTotNGLOp              ,;
              NEQVAMNT  WITH NGLAMOUNT
      m.NGLAMOUNT = m.NGLAMOUNT - NGLAMOUNT
      m.NEQVAMNT  = m.NEQVAMNT  - NEQVAMNT
      m.GLSESSION = 'DONE'
      SELE GL_LINK  
      IF !SEEK(PADR(lcGLPLkC,6)+'018')    
        =SEEK('DEFDEF'+'018')
      ENDIF   
      m.CATG_KEY   = '018'
      m.GLACCOUNT  = GLACNT 
      m.GLACNTTYPE = 'D'
      IF m.NGLAMOUNT <> 0
        INSERT INTO (lcGlDist) FROM MEMVAR   
      ENDIF
    ENDSCAN
    SELECT (lcOldAlias)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfUpdtOpGl
*! Developer : Albert Raif (ALB)
*! Date      : 12/19/2002
*! Purpose   : Add GL entries in gldist
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfUpdtOpGl()
*!*************************************************************
*!C200444,1
FUNCTION lfUpdtOpGl

*B607333,1 ALB Use period and fiscal year for receiving date not system default[BEGIN]
PRIVATE lcFisPrd,lcFisYer
STORE '' TO lcFisPrd,lcFisYer
*B607333,1 ALB Use period and fiscal year for receiving date not system default[END]

IF lcTranType $ "IS"
  lcOldAlias = ALIAS()
  IF llLinkToGl
    lcSession = GFSEQUENCE('GLSESSION')
    IF !USED('GLDIST')
      =gfOpenFile(gcWorkDir+'GLDIST','','SH')
    ENDIF
    lcGlDistUp = gfTempName()
    SELECT GLDIST
    COPY STRUCTURE TO (gcWorkDir+lcGlDistUp)
    =gfOpenFile(gcWorkDir+lcGlDistUp,'','SH')

    SELECT CTKTBOM
    SET ORDER TO CTKTBOM
    lcBomKey = lcTranType + laData[1]

    IF SEEK(lcBomKey)
      LOCATE REST WHILE cImTyp+CutTkt+Typ+Item+IClr+MfgCode+Dyelot = lcTranType + laData[1];
                  FOR MfgCode = &lcRcvFile..cOprCode
      
      IF !('AP' $ gcCmpModules) .OR. ;
        (cCatGTyp = 'M' .AND. gfRltFld(&lcRcvFile..cOprCode,@laMfgRFld,'MFGCODE') ;
        .AND. EMPTY(lcMfgGlAcnt)) 
        IF (lcAction = '2' OR lcAction = '3')

          *B607333,1 ALB Use period and fiscal year for receiving date not system default[BEGIN]
          =CHECKPRD(&lcRcvFile..dTranDate,'lcFisYer','lcFisPrd','  ',.T.)
          *DO GlDist WITH ctktbom.link_code,'013',(ctktbom.untcost)*(ctktbom.untqty)*(&lcRcvFile..nTotRec),'NL',;
                   laData[1],&lcRcvFile..dTranDate,gcCurrYear,gcCurrPrd,lcGlDistUp,'',;
                   poshdr.CPriceCur,poshdr.ncurrunit,poshdr.NPriceRat
          *DO GlDist WITH ctktbom.link_code,'018',-(ctktbom.untcost)*(ctktbom.untqty)*(&lcRcvFile..nTotRec),'NL',;
                   laData[1],&lcRcvFile..dTranDate,gcCurrYear,gcCurrPrd,lcGlDistUp,'',;
                   poshdr.CPriceCur,poshdr.ncurrunit,poshdr.NPriceRat

          DO GlDist WITH ctktbom.link_code,'013',(ctktbom.untcost)*(ctktbom.untqty)*(&lcRcvFile..nTotRec),'NL',;
                   laData[1],&lcRcvFile..dTranDate,lcFisYer,lcFisPrd,lcGlDistUp,'',;
                   poshdr.CPriceCur,poshdr.ncurrunit,poshdr.NPriceRat
          DO GlDist WITH ctktbom.link_code,'018',-(ctktbom.untcost)*(ctktbom.untqty)*(&lcRcvFile..nTotRec),'NL',;
                   laData[1],&lcRcvFile..dTranDate,lcFisYer,lcFisPrd,lcGlDistUp,'',;
                   poshdr.CPriceCur,poshdr.ncurrunit,poshdr.NPriceRat
          *B607333,1 ALB Use period and fiscal year for receiving date not system default[END]

        ENDIF
      ENDIF
    ENDIF
    SELECT (lcGlDistUp)
    REPLACE ALL GLSESSION WITH lcSession
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lcGlDistUp)
    SELECT (lcOldAlias)
    USE IN (lcGlDistUp)
    ERASE (gcWorkDir+lcGlDistUp+'.DBF')
  ENDIF
ENDIF
*---------------------
*!*************************************************************
*! Name      : lfUpdtApGl
*! Developer : Albert Raif (ALB)
*! Date      : 12/19/2002
*! Purpose   : Add GL entries in gldist
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfUpdtApGl()
*!*************************************************************
*!C200447,1
FUNCTION lfUpdtApGl
IF m.cIMTyp $ "IS"
  lcOldAlias = ALIAS()
  DIMENSION laMfgRFld[7,2]
  IF llLinkToGl
    STORE ''  TO laMfgRFld,lcmfgglacnt,lcContCode,lcContName,lcOperSeq,llInHouse,llMfgOpr,lnLeadTime
    laMfgRFld[1,1] = 'CCONTCODE'
    laMfgRFld[1,2] = 'lcContCode'
    laMfgRFld[2,1] = 'CCONTNAME'
    laMfgRFld[2,2] = 'lcContName'
    laMfgRFld[3,1] = 'COPERSEQ'
    laMfgRFld[3,2] = 'lcOperSeq'
    laMfgRFld[4,1] = 'LINHOUSE'
    laMfgRFld[4,2] = 'llInHouse'
    laMfgRFld[5,1] = 'LMFGOPR'
    laMfgRFld[5,2] = 'llMfgOpr'
    laMfgRFld[6,1] = 'LEADTIME'
    laMfgRFld[6,2] = 'lnLeadTime'
    laMfgRFld[7,1] = 'GLACCOUNT'
    laMfgRFld[7,2] = 'lcMfgGlAcnt'

    *---Get some informatiom one time for each po
    IF !USED('CODES')
      =gfOpenFile(gcDataDir+'CODES','','SH')
    ENDIF
    IF !('AP' $ gcCmpModules) .OR.  (gfRltFld(m.COPRCODE,@laMfgRFld,'MFGCODE') ;
         .AND. EMPTY(lcMfgGlAcnt)) AND llMfgOpr
      SELECT GL_LINK
      IF !SEEK(PADR(lcLinkCode,6)+'018')    
        =SEEK('DEFDEF'+'018')
      ENDIF   
      m.CAPDGLACT   = GLACNT 
    ENDIF
    SELECT (lcOldAlias)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfUpdAApGl
*! Developer : Albert Raif (ALB)
*! Date      : 12/19/2002
*! Purpose   : Add GL entries in gldist
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfUpdtApGl()
*!*************************************************************
*!C200447,1
FUNCTION lfUpdAApGl
IF laTickets[lnAplTkt,2] $ "IS"
  lcOldAlias = ALIAS()
  DIMENSION laMfgRFld[7,2]
  IF llLinkToGl
    STORE ''  TO laMfgRFld,lcmfgglacnt,lcContCode,lcContName,lcOperSeq,llInHouse,llMfgOpr,lnLeadTime
    laMfgRFld[1,1] = 'CCONTCODE'
    laMfgRFld[1,2] = 'lcContCode'
    laMfgRFld[2,1] = 'CCONTNAME'
    laMfgRFld[2,2] = 'lcContName'
    laMfgRFld[3,1] = 'COPERSEQ'
    laMfgRFld[3,2] = 'lcOperSeq'
    laMfgRFld[4,1] = 'LINHOUSE'
    laMfgRFld[4,2] = 'llInHouse'
    laMfgRFld[5,1] = 'LMFGOPR'
    laMfgRFld[5,2] = 'llMfgOpr'
    laMfgRFld[6,1] = 'LEADTIME'
    laMfgRFld[6,2] = 'lnLeadTime'
    laMfgRFld[7,1] = 'GLACCOUNT'
    laMfgRFld[7,2] = 'lcMfgGlAcnt'
    *---Get some informatiom one time for each po
    IF !USED('CODES')
      =gfOpenFile(gcDataDir+'CODES','','SH')
    ENDIF
    IF !('AP' $ gcCmpModules) .OR.  (gfRltFld(lcMFGCode,@laMfgRFld,'MFGCODE') ;
         .AND. EMPTY(lcMfgGlAcnt)) AND llMfgOpr
      SELECT GL_LINK
      IF !SEEK(PADR(lcLinkCode,6)+'018')    
        =SEEK('DEFDEF'+'018')
      ENDIF   
      STORE GLACNT TO lcWipAcnt,lcApDGlAct
      SHOW GET lcWipAcnt
      SHOW GET lcApDGlAct
    ENDIF
    SELECT (lcOldAlias)
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfUpdtClGl
*! Developer : Albert Raif (ALB)
*! Date      : 12/19/2002
*! Purpose   : Add GL entries in gldist
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfUpdtClGl()
*!*************************************************************
*!C200449,1
FUNCTION lfUpdtClGl
IF lcTranType $ "IS"
  lcOldAlias = ALIAS()
  DIMENSION laMfgRFld[7,2]
  IF llLinkToGl
    lcBomType = ''
    FOR lnCount = 1 TO 5
      IF laSetups[12+lnCount,2] = 'M'
        lcBomType = STR(lnCount,1)
        STORE ''  TO laMfgRFld,lcmfgglacnt,lcContCode,lcContName,lcOperSeq,llInHouse,llMfgOpr,lnLeadTime
        laMfgRFld[1,1] = 'CCONTCODE'
        laMfgRFld[1,2] = 'lcContCode'
        laMfgRFld[2,1] = 'CCONTNAME'
        laMfgRFld[2,2] = 'lcContName'
        laMfgRFld[3,1] = 'COPERSEQ'
        laMfgRFld[3,2] = 'lcOperSeq'
        laMfgRFld[4,1] = 'LINHOUSE'
        laMfgRFld[4,2] = 'llInHouse'
        laMfgRFld[5,1] = 'LMFGOPR'
        laMfgRFld[5,2] = 'llMfgOpr'
        laMfgRFld[6,1] = 'LEADTIME'
        laMfgRFld[6,2] = 'lnLeadTime'
        laMfgRFld[7,1] = 'GLACCOUNT'
        laMfgRFld[7,2] = 'lcMfgGlAcnt'

        lcPo_No = ''
        lnTotLnOpr = 0
        *---Get some informatiom one time for each po
        IF !USED('CODES')
          =gfOpenFile(gcDataDir+'CODES','','SH')
        ENDIF
        *---Get total of operation have Gl
        SELECT BOMLINE
        lcOldOrder = ORDER()
        SET ORDER TO MFGOPR

        =SEEK('I'+laData[1])
        SUM REST ITEMAMT WHILE cImTyp+cTktNo+cOprCode+cType+Style+SClr+;
                           IIF(cCatgTyp$"MDP",PADR(MfgCode,12),Item)+IClr = 'I'+laData[1] ;
            FOR gfRltFld(BOMLINE.MFGCODE,@laMfgRFld,'MFGCODE') .AND. EMPTY(lcMfgGlAcnt);
            AND llMfgOpr AND CTYPE='2' TO lnTotLnOpr

        SET ORDER TO &lcOldOrder.
        *---Get total amount of received operation (Non GL)
        SELECT BOMCOST
        lcOldOrder = ORDER()
        SET ORDER TO BOMCSTKT
        lnTotAcOpr = 0
        =SEEK(lcBomType +'I'+laData[1])
        SUM REST NTOTACST WHILE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+;
                                crsession+cisession = lcBomType +'I'+laData[1];
            FOR gfRltFld(BOMCOST.MFGCODE,@laMfgRFld,'MFGCODE') .AND. EMPTY(lcMfgGlAcnt);
            AND llMfgOpr TO lnTotAcOpr
            
        SET ORDER TO &lcOldOrder.

        *----Update the GLDIST
  
        IF lnTotAcOpr <> lnTotLnOpr
          DO GlDist WITH laData[29],'018',(lnTotLnOpr - lnTotAcOpr),lcGLTrnTyp,;
                         laData[1],ldClsDate,lcGlYear,lcGlPeriod,lcGlDTemp

          DO GlDist WITH laData[29],IIF(EMPTY(GL_LINK.GlAcnt),'019','022'),;
                        (lnTotAcOpr-lnTotLnOpr),lcGLTrnTyp,laData[1],ldClsDate,;
                         lcGlYear,lcGlPeriod,lcGlDTemp

          laData[16+VAL(lcBomType)] = laData[16+VAL(lcBomType)] - lnTotLnOpr
          laData[21+VAL(lcBomType)] = laData[21+VAL(lcBomType)] - lnTotAcOpr 
        ENDIF
        SELECT (lcOldAlias)

      ENDIF
    ENDFOR
  ENDIF
ENDIF
*:*************************************************************
*: Name      : lfGetPrdPl
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/19/2002
*: Purpose   : Open Forcast File and get the Production Qty.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfUpdtClGl()
*:*************************************************************
*:C200451,1
FUNCTION lfGetPrdPl
PRIVATE lnPrvAls , lnPQty , ldDate, St_Day , lnWeek_No , lnCuryear;
        lcScanExp

lnPQty = 0
lnPrvAls = SELECT (0)
IF lcRpBasePj = 'P' .AND. llRpPlanPr
  IF !USED('FORCAST')
    *-- style+STR(nyear,4)+STR(nweek,2)
    =gfOpenFile(gcDataDir+'FORCAST',gcDataDir+'FORCAST','SH')
  ENDIF
  
  *-- Check for the style
  IF SEEK(Style.Style,'FORCAST')
    
    
    IF !EMPTY(lDRpPrDate)
      *-- lDRpPrDate :- the report date.
      ldDate    = CTOD('01/01/'+STR(YEAR(gdsysdate),4))
      St_Day    = ldDate-DOW(ldDate)+1
      lnWeek_No = (MAX(CEILING((lDRpPrDate - St_Day)/7),1))
      lnCuryear = YEAR(ldDate)

      lcScanExp = 'nyear <= lnCuryear .AND. nweek <= lnWeek_No '
      
    ELSE  
      lcScanExp = '.T.'
    ENDIF

    SELECT FORCAST
    SCAN REST WHILE style+STR(nyear,4)+STR(nweek,2) = Style.Style;
    FOR &lcScanExp
      lnPQty = lnPQty + FORCAST.npqty&z
    ENDSCAN

  ENDIF
  laRequr[3,I] = IIF(lnPQty > 0, lnPQty + ROUND(lnPQty*((lnRpPrIncr/100)),0), 0)  
  
ENDIF
*-- End OF lfGetPrdPl
*:*************************************************************
*: Name      : lfCLOSEFLS
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/19/2002
*: Purpose   : close Forcast File.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfUpdtClGl()
*:*************************************************************
*:C200451,1
FUNCTION lfCLOSEFLS

IF USED('FORCAST')
  USE IN FORCAST
ENDIF
*-- End of lfCLOSEFLS
*:*************************************************************