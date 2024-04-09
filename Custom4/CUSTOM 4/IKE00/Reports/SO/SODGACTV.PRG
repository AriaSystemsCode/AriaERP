*:***************************************************************************
*: Program file  : SODGACTV.PRG
*: Program desc. : SBT Summary/Detail Reports
*: Date          : 04/12/2016
*: System        : Aria 4 XP
*: Module        : Sales Order (SO)
*: Developer     : Mariam Mazhar (MMT)
*: Tracking Job #: C201804 [T20151014.0017]
*:***************************************************************************
*: Modificaton:
* C201804,2 MMT 05/17/2016 Fix bug of wrong transactions signs in case of returns and Sales
*:***************************************************************************
IF loOgScroll.llOGFltCh
  lfCreateTmp()
  lfCollectData()
ELSE
  IF FILE(oAriaApplication.WorkDir+lcDataTmp+'.DBF')
    USE (oAriaApplication.WorkDir+lcDataTmp+'.DBF') IN 0 
  ENDIF  
ENDIF

IF RECCOUNT(lcDataTmp) == 0
  = gfModalGen('TRM00052B40011','ALERT')
  RETURN .F. 
ENDIF

USE IN (lcDataTmp)
DIMENSION loOGScroll.laCRParams[4,2]
loOGScroll.laCRParams[1,1] = 'Format'
loOGScroll.laCRParams[1,2] = lcRpForm  
loOGScroll.laCRParams[2,1] = 'SortBy'
loOGScroll.laCRParams[2,2] = lcRpGroup
loOGScroll.laCRParams[3,1] = 'BasedOn'
loOGScroll.laCRParams[3,2] = lcRpBase
loOGScroll.laCRParams[4,1] = 'ReportName'
loOGScroll.laCRParams[4,2] = IIF(lcRpForm  ='S','SBT Summary Report','SBT Detail Report')

loOgScroll.lcOGLastForm  = "SODGACTV"
DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcDataTmp+'.DBF'

= gfDispRe()
*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/10/2016
*! Purpose   : Collecting data
*!*************************************************************
FUNCTION lfCollectData
IF !USED('POSLN')
  =gfOpenTable('POSLN','POSLN')
ENDIF
IF !USED('SCALE')
  =gfOpenTable('SCALE','SCALE')
ENDIF
IF !USED('STYLE_C')
  =gfOpenTable('STYLE','STYLE','SH','STYLE_C')
ENDIF
IF !USED('STYLEUPC')
  =gfOpenTable('STYLEUPC','STYLEUPC')
ENDIF

IF !USED('PRODACTI')
  =gfOpenTable('PRODACTI','PRODACTI')
ENDIF

lcSTYTmp =  ''
lnPOSty = ASCAN(loOgScroll.laOGFxFlt,"STYLE.STYLE")
IF lnPOSty > 0
  lnPOSty = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPOSty,1)
  lcSTYTmp =  loOgScroll.laOGFxFlt[lnPOSty,6]
  IF USED(lcSTYTmp)
    SELECT (lcSTYTmp)
    LOCATE 
    IF EOF()
	  lcSTYTmp = ''       
    ENDIF
  ENDIF
ENDIF

lcSTORETmp =  ''
lnPOStore = ASCAN(loOgScroll.laOGFxFlt,"CUSTOMER.STORE")
IF lnPOStore > 0
  lnPOStore= ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPOStore,1)
  lcSTORETmp=  loOgScroll.laOGFxFlt[lnPOStore,6]
  IF USED(lcSTORETmp)
    SELECT (lcSTORETmp)
    LOCATE 
    IF EOF()
	  lcSTORETmp= ''       
    ENDIF
  ENDIF
ENDIF

**
lcSMARTETmp =  ''
lnSMINVOICE = ASCAN(loOgScroll.laOGVRFlt,"SMART_INVOICE_HEADER.SMART_INVOICE_NO")
IF lnSMINVOICE> 0
  lnSMINVOICE = ASUBSCRIPT(loOgScroll.laOGVRFlt,lnSMINVOICE,1)
  lcSMARTETmp =  loOgScroll.laOGVRFlt[lnSMINVOICE,6]
  IF USED(lcSMARTETmp)
    SELECT (lcSMARTETmp)
    LOCATE 
    IF EOF()
	  lcSMARTETmp = ''       
    ENDIF
  ENDIF
ENDIF
*
lcReturnTmp =  ''
lnRetPos = ASCAN(loOgScroll.laOGFxFlt,"RETHDR.CRMEMO")
IF lnRetPos > 0
  lnRetPos = ASUBSCRIPT(loOgScroll.laOGFXFlt,lnRetPos,1)
  lnRetPos=  loOgScroll.laOGFXFlt[lnRetPos,6]
  IF USED(lcReturnTmp)
    SELECT (lcReturnTmp )
    LOCATE 
    IF EOF()
	  lcReturnTmp = ''       
    ENDIF
  ENDIF
ENDIF


ldTranStart =  {}
ldTranEnd =  {}
lnPODate = ASCAN(loOgScroll.laOGFxFlt,"POSHDR.COMPLETE")
IF lnPODate > 0
  lnPODate = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPODate ,1)
  lcDateValue=  loOgScroll.laOGFxFlt[lnPODate ,6]
  IF !EMPTY(lcDateValue)
    ldTranStart =  IIF(EMPTY(SUBSTR(lcDateValue,1,10)),{},CTOD(SUBSTR(lcDateValue,1,10)))
    ldTranEnd =   IIF(EMPTY(SUBSTR(lcDateValue,12,21)),{},CTOD(SUBSTR(lcDateValue,12,21)))
  ENDIF
ENDIF

** Inter-Location
IF 'I' $ lcRpType
  lcPOTmp = '' 
  IF lcRpType == 'I'
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,"POSHDR.PO")
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcPOTmp =  loOgScroll.laOGFxFlt[lnPOS,6]
      IF USED(lcPOTmp)
        SELECT (lcPOTmp)
        LOCATE 
        IF EOF()
		  lcPOTmp = ''       
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  
  IF !EMPTY(lcPOTmp)
    SELECT (lcPOTmp)
    LOCATE
    SCAN
      lcPONo = &lcPOTmp..PO 
      SELECT POSLN
      IF gfSeek('NN'+lcPONo)
        SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = 'NN'+lcPONo FOR TRANCD ='2' AND;
                        IIF(!EMPTY(lcSTYTmp),SEEK(POSLN.STYLE,lcSTYTmp),.T.) AND IIF(!EMPTY(lcSTORETmp),SEEK(POSLN.cWareCode,lcSTORETmp),.T.)
           IF lcRpInvt ='N' AND (!EMPTY(ldTranStart) OR !EMPTY(ldTranEnd))
             IF  !EMPTY(ldTranStart) AND !EMPTY(ldTranEnd) AND !BETWEEN(POSLN.Date,ldTranStart,ldTranEnd)
               LOOP
             ENDIF
             IF EMPTY(ldTranStart) AND !EMPTY(ldTranEnd) AND  !(POSLN.Date =< ldTranEnd)
               LOOP
             ENDIF
           ENDIF
           SELECT POSLN
           SCATTER MEMO MEMVAR
           m.TRANTYPE = 'InterLocation'
           =gfSeek(m.Style,'STYLE_C','STYLE')
           =gfSeek('S'+STYLE_C.Scale,'Scale','Scale')
           FOR lnSc = 1 TO Scale.cnt 
             lcSC = STR(lnSc,1)
             m.Qty = m.Qty&lcSC.
             IF m.Qty  = 0
               LOOP 
             ENDIF
             =gfSeek(m.Style+lcSC,'STYLEUPC','STYLEUPC')
             m.UPC  = StyleUPC.cUPCNum1+StyleUPC.cUPCNum2+StyleUPC.cUPCNum3
             m.TRAN_NO = POSLN.PO
             m.SIZE = Scale.Sz&lcSC.
             m.TOTCOST = STYLE_C.TOTCOST
             m.DESC = STYLE_C.DESC
             m.STORE = POSLN.cWareCode
             m.DATE = POSLN.Date
             m.PRICEA = STYLE_C.PriceA
 			 m.NSUGRETPRI  = STYLE_C.nsugretpri
 			 IF !SEEK(m.TranType+m.TRAN_NO+m.UPC,lcDataTmp)
   			  INSERT INTO (lcDataTmp) FROM MEMVAR
   			 ELSE
   			   SELECT (lcDataTmp)
   			   REPLACE Qty WITH QTy + m.Qty 
   			 ENDIF 
   			 SELECT POSLN
           ENDFOR
        ENDSCAN
      ENDIF
    ENDSCAN
  ELSE
    SELECT (lcPOSHDR)
    LOCATE
    SCAN
      lcPONo = &lcPOSHDR..PO 
      SELECT POSLN
      IF gfSeek('NN'+lcPONo)
        SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = 'NN'+lcPONo FOR TRANCD ='2' AND;
                        IIF(!EMPTY(lcSTYTmp),SEEK(POSLN.STYLE,lcSTYTmp),.T.) AND IIF(!EMPTY(lcSTORETmp),SEEK(POSLN.cWareCode,lcSTORETmp),.T.)
           IF lcRpInvt ='N' AND (!EMPTY(ldTranStart) OR !EMPTY(ldTranEnd))
             IF  !EMPTY(ldTranStart) AND !EMPTY(ldTranEnd) AND !BETWEEN(POSLN.Date,ldTranStart,ldTranEnd)
               LOOP
             ENDIF
             IF EMPTY(ldTranStart) AND !EMPTY(ldTranEnd) AND  !(POSLN.Date =< ldTranEnd)
               LOOP
             ENDIF
           ENDIF
           SELECT POSLN
           SCATTER MEMO MEMVAR
           m.TRANTYPE = 'InterLocation'
           =gfSeek(m.Style,'STYLE_C','STYLE')
           =gfSeek('S'+STYLE_C.Scale,'Scale','Scale')
           FOR lnSc = 1 TO Scale.cnt 
             lcSC = STR(lnSc,1)
             m.Qty = m.Qty&lcSC.
             IF m.Qty  = 0
               LOOP 
             ENDIF

             =gfSeek(m.Style+lcSC,'STYLEUPC','STYLEUPC')
             m.UPC  = StyleUPC.cUPCNum1+StyleUPC.cUPCNum2+StyleUPC.cUPCNum3
             m.TRAN_NO = POSLN.PO
             m.SIZE = Scale.Sz&lcSC.
             m.TOTCOST = STYLE_C.TOTCOST
             m.DESC = STYLE_C.DESC
             m.STORE = POSLN.cWareCode
             m.DATE = POSLN.Date
             m.PRICEA = STYLE_C.PriceA
 			 m.NSUGRETPRI  = STYLE_C.nsugretpri
 			 IF !SEEK(m.TranType+m.TRAN_NO+m.UPC,lcDataTmp)
   			  INSERT INTO (lcDataTmp) FROM MEMVAR
   			 ELSE
   			   SELECT (lcDataTmp)
   			   REPLACE Qty WITH QTy + m.Qty 
   			 ENDIF 
   			 SELECT POSLN
           ENDFOR
        ENDSCAN
      ENDIF
    ENDSCAN  
  ENDIF
ENDIF

** Sales 
IF 'S' $ lcRpType
  SELECT PRODACTI
  =gfSeek('DOLLR')
  SCAN REST WHILE ACCOUNT+DTOS(DPRD_BGN)+DTOS(DPRD_END)+DEPT+STORE+STYLE+TRAN_TYPE = 'DOLLR' FOR tran_type = 'QS' AND ;
		    IIF(!EMPTY(lcSTYTmp),SEEK(PRODACTI.STYLE,lcSTYTmp),.T.) AND IIF(!EMPTY(lcSTORETmp),SEEK(PRODACTI.STORE,lcSTORETmp),.T.)
    IF  lcRpType == 'S' AND !EMPTY(lcSMARTETmp) AND !SEEK(DTOS(PRODACTI.dprd_bgn)+PADL(ALLTRIM(PRODACTI.Store),5,'0')+"18318",lcSMARTETmp)		    
      LOOP
    ENDIF
    IF lcRpInvt ='N' AND (!EMPTY(ldTranStart) OR !EMPTY(ldTranEnd))
      IF  !EMPTY(ldTranStart) AND !EMPTY(ldTranEnd) AND !BETWEEN(PRODACTI.dprd_bgn,ldTranStart,ldTranEnd)
        LOOP
      ENDIF
      IF EMPTY(ldTranStart) AND !EMPTY(ldTranEnd) AND  !(PRODACTI.dprd_bgn =< ldTranEnd)
        LOOP
      ENDIF
    ENDIF
    SELECT PRODACTI
    SCATTER MEMO MEMVAR
    m.TRANTYPE = 'Sales'
    =gfSeek(m.Style,'STYLE_C','STYLE')
    =gfSeek('S'+STYLE_C.Scale,'Scale','Scale')
    FOR lnSc = 1 TO Scale.cnt 
      lcSC = STR(lnSc,1)
      * C201804,2 MMT 05/17/2016 Fix bug of wrong transactions signs in case of returns and Sales[Start]
      *m.Qty = m.nactiqty&lcSC.      
      m.Qty = -1 * m.nactiqty&lcSC.
      * C201804,2 MMT 05/17/2016 Fix bug of wrong transactions signs in case of returns and Sales[End]
      IF m.Qty  = 0
        LOOP 
      ENDIF

      =gfSeek(m.Style+lcSC,'STYLEUPC','STYLEUPC')
      m.UPC  = StyleUPC.cUPCNum1+StyleUPC.cUPCNum2+StyleUPC.cUPCNum3
      m.TRAN_NO = DTOS(PRODACTI.dprd_bgn)+PADL(ALLTRIM(PRODACTI.Store),5,'0')+"18318"
      m.SIZE = Scale.Sz&lcSC.
      m.TOTCOST = STYLE_C.TOTCOST
      m.DESC = STYLE_C.DESC
      m.STORE = PRODACTI.STORE
      m.DATE = PRODACTI.dprd_bgn 
      m.PRICEA = STYLE_C.PriceA
      m.NSUGRETPRI  = STYLE_C.nsugretpri
      IF !SEEK(m.TranType+m.TRAN_NO+m.UPC,lcDataTmp)
        INSERT INTO (lcDataTmp) FROM MEMVAR
   	  ELSE
   		SELECT (lcDataTmp)
   		REPLACE Qty WITH QTy + m.Qty 
   	  ENDIF 
   	  SELECT PRODACTI
    ENDFOR
  ENDSCAN		    
ENDIF


***Returns 
IF 'R' $ lcRpType
  IF !USED('SMART_INVOICE_LINE')
    lnResult=oAriaApplication.remotecompanydata.SqlRun("Select * from SMART_INVOICE_LINES WHERE CTRCODE <>  '' AND TOTQTY < 0  AND"+;
           " ISNULL(CTRCODE,'X') <> 'X'",;
    'SMART_INVOICE_LINE','SMART_INVOICE_LINES',oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
    If lnResult<>1
      oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
    ELSE
      SELECT 'SMART_INVOICE_LINE'
      CURSORSETPROP("Buffering" ,3)
      INDEX on  SMART_INVOICE_NO + STYLE TAG 'SMARTLN'
    ENDIF 
  ENDIF
  
  
  SELECT PRODACTI
  =gfSeek('DOLLR')
  SCAN REST WHILE ACCOUNT+DTOS(DPRD_BGN)+DTOS(DPRD_END)+DEPT+STORE+STYLE+TRAN_TYPE = 'DOLLR' FOR tran_type = 'QU' AND ;
		    IIF(!EMPTY(lcSTYTmp),SEEK(PRODACTI.STYLE,lcSTYTmp),.T.) AND IIF(!EMPTY(lcSTORETmp),SEEK(PRODACTI.STORE,lcSTORETmp),.T.)
	
    
    IF lcRpType == 'R' AND (SEEK(DTOS(PRODACTI.dprd_bgn)+PADL(ALLTRIM(PRODACTI.Store),5,'0')+"18318"+PRODACTI.Style,'SMART_INVOICE_LINE','SMARTLN') AND !EMPTY(lcReturnTmp) AND !SEEK(SMART_INVOICE_LINE.CTRCODE,lcReturnTmp)) 
      LOOP
    ENDIF
    IF lcRpInvt ='N' AND (!EMPTY(ldTranStart) OR !EMPTY(ldTranEnd))
      IF  !EMPTY(ldTranStart) AND !EMPTY(ldTranEnd) AND !BETWEEN(PRODACTI.dprd_bgn,ldTranStart,ldTranEnd)
        LOOP
      ENDIF
      IF EMPTY(ldTranStart) AND !EMPTY(ldTranEnd) AND  !(PRODACTI.dprd_bgn =< ldTranEnd)
        LOOP
      ENDIF
    ENDIF
    
    SELECT PRODACTI
    SCATTER MEMO MEMVAR
    m.TRANTYPE = 'Returns'
    =gfSeek(m.Style,'STYLE_C','STYLE')
    =gfSeek('S'+STYLE_C.Scale,'Scale','Scale')
    FOR lnSc = 1 TO Scale.cnt 
      lcSC = STR(lnSc,1)
      * C201804,2 MMT 05/17/2016 Fix bug of wrong transactions signs in case of returns and Sales[Start]
      *m.Qty = m.nactiqty&lcSC. * -1
      m.Qty = m.nactiqty&lcSC. 
      * C201804,2 MMT 05/17/2016 Fix bug of wrong transactions signs in case of returns and Sales[End]
      IF m.Qty  = 0
        LOOP 
      ENDIF

      =gfSeek(m.Style+lcSC,'STYLEUPC','STYLEUPC')
      m.UPC  = StyleUPC.cUPCNum1+StyleUPC.cUPCNum2+StyleUPC.cUPCNum3
      IF SEEK(DTOS(PRODACTI.dprd_bgn)+PADL(ALLTRIM(PRODACTI.Store),5,'0')+"18318"+PRODACTI.Style,'SMART_INVOICE_LINE','SMARTLN')
        m.TRAN_NO = SMART_INVOICE_LINE.CTRCODE
      ELSE
        m.TRAN_NO = ''
      ENDIF
      m.SIZE = Scale.Sz&lcSC.
      m.TOTCOST = STYLE_C.TOTCOST
      m.DESC = STYLE_C.DESC
      m.STORE = PRODACTI.STORE
      m.DATE = PRODACTI.dprd_bgn 
      m.PRICEA = STYLE_C.PriceA
      m.NSUGRETPRI = STYLE_C.nsugretpri
      IF !SEEK(m.TranType+m.TRAN_NO+m.UPC,lcDataTmp)
        INSERT INTO (lcDataTmp) FROM MEMVAR
   	  ELSE
   		SELECT (lcDataTmp)
   		REPLACE Qty WITH QTy + m.Qty 
   	  ENDIF 
   	  SELECT PRODACTI
    ENDFOR
  ENDSCAN		    
ENDIF

*!*************************************************************
*! Name      : lfCreateTmp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/10/2016
*! Purpose   : Create Temp to Hold data
*!*************************************************************
FUNCTION lfCreateTmp
DIMENSION laDataStru[12,4]
laDataStru[1,1] = 'TRANTYPE'
laDataStru[1,2] = 'C'
laDataStru[1,3] = 13
laDataStru[1,4] = 0

laDataStru[2,1] = 'TRAN_NO'
laDataStru[2,2] = 'C'
laDataStru[2,3] = 18
laDataStru[2,4] = 0

laDataStru[3,1] = 'STORE'
laDataStru[3,2] = 'C'
laDataStru[3,3] = 8
laDataStru[3,4] = 0

laDataStru[4,1] = 'STYLE'
laDataStru[4,2] = 'C'
laDataStru[4,3] = 19
laDataStru[4,4] = 0

laDataStru[5,1] = 'DESC'
laDataStru[5,2] = 'C'
laDataStru[5,3] = 30
laDataStru[5,4] = 0

laDataStru[6,1] = 'SIZE'
laDataStru[6,2] = 'C'
laDataStru[6,3] = 5
laDataStru[6,4] = 0

laDataStru[7,1] = 'UPC'
laDataStru[7,2] = 'C'
laDataStru[7,3] = 13
laDataStru[7,4] = 0

laDataStru[8,1] = 'QTY'
laDataStru[8,2] = 'N'
laDataStru[8,3] = 12
laDataStru[8,4] = 0

laDataStru[9,1] = 'DATE'
laDataStru[9,2] = 'D'
laDataStru[9,3] = 8
laDataStru[9,4] = 0

laDataStru[10,1] = 'PRICEA'
laDataStru[10,2] = 'N'
laDataStru[10,3] = 12
laDataStru[10,4] = 2

laDataStru[11,1] = 'NSUGRETPRI'
laDataStru[11,2] = 'N'
laDataStru[11,3] = 12
laDataStru[11,4] = 2

laDataStru[12,1] = 'TOTCOST'
laDataStru[12,2] = 'N'
laDataStru[12,3] = 13
laDataStru[12,4] = 2

= gfCrtTmp(lcDataTmp,@laDataStru,'TranType+TRAN_NO+UPC' ,lcDataTmp,.F.)
*!*************************************************************
*! Name      : lfvTypes
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/10/2016
*! Purpose   : Evaluate order types expression.
*!*************************************************************
FUNCTION lfvTypes
PRIVATE lcOldTypes,lcCurrChr

lcOldTypes = lcRpType  && Save old status value.
= lfOGMover(@laRpTypSrc,@laRpTypDst,"Select Transaction Type",.T.,'')  && call mover function.

lcRpType = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTypDst[1])
  FOR lnI = 1 TO ALEN(laRpTypDst,1)
    lcRpType = lcRpType + IIF(laRpTypDst[lnI] = "Sales",'S',;
                          IIF(laRpTypDst[lnI] = "Interlocation",'I',;
                          IIF(laRpTypDst[lnI] = "Returns",'R','')))
  ENDFOR
ENDIF
lcRpType = IIF(EMPTY(lcRpType),'SIR',ALLTRIM(lcRpType))
clearread()
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/10/2016
*! Purpose   : When function of the OG
*!*************************************************************
FUNCTION lfwRepWhen
IF EMPTY(laRpTypSrc)
  DECLARE laRpTypSrc[3],laRpTypDst[3]  && Redeclare the source and target arrays.
  STORE "Sales"               TO laRpTypSrc[1],laRpTypDst[1]
  STORE "Interlocation" TO laRpTypSrc[2],laRpTypDst[2]
  STORE "Returns"  TO laRpTypSrc[3],laRpTypDst[3]
ENDIF
lcRpType = ' '
IF !EMPTY(laRpTypDst[1])
  FOR lnI = 1 TO ALEN(laRpTypDst,1)
    lcRpType = lcRpType + IIF(laRpTypDst[lnI] = "Sales",'S',;
                          IIF(laRpTypDst[lnI] = "Interlocation",'I',;
                          IIF(laRpTypDst[lnI] = "Returns",'R','')))
  ENDFOR
ENDIF
lcRpType = IIF(EMPTY(lcRpType),'SIR',ALLTRIM(lcRpType))
IF !USED(lcTmpCrLst)
  lnResult=oAriaApplication.remotecompanydata.SqlRun("Select * from RETHDR WHERE Account = 'DOLLR' AND  Status <> 'V' and CRMEMO in (Select DISTINCT CTRCODE from SMART_INVOICE_LINES Where TOTQTY < 0  AND"+;
           " CTRCODE <> '' and ISNULL(CTRCODE,'X') <> 'X')",;
    lcTmpCrLst,'RETHDR',oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
  If lnResult<>1
    oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
  ELSE
    SELECT (lcTmpCrLst)
    CURSORSETPROP("Buffering" ,3)
    INDEX on CRMEMO TAG (lcTmpCrLst)
  ENDIF 
ENDIF

IF !USED(lcPOSHDR)
  lnResult=oAriaApplication.remotecompanydata.SqlRun("Select * from POSHDR WHERE CSTYTYPE+CBUSDOCU ='NN' and STATUS <> 'X' and CPIKTKT <> '' AND CWARECODE = ''",;           
    lcPOSHDR,'POSHDR',oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
  If lnResult<>1
    oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
  ELSE
    SELECT (lcPOSHDR)
    CURSORSETPROP("Buffering" ,3)
    INDEX on CPIKTKT TAG "PIKTKT"
    INDEX on PO TAG (lcPOSHDR) ADDITIVE 
  ENDIF 
ENDIF
  
*!*************************************************************
*! Name      : RefreshTypes
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/10/2016
*! Purpose   : Return the selected types in the ReadBox
*!*************************************************************
FUNCTION RefreshTypes
LOCAL lcTypesStr, lnTarget
lcTypesStr = ""
  IF !EMPTY(laRpTypDst)
    FOR lnTarget = 1 TO ALEN(laRpTypDst,1)
      lcTypesStr = lcTypesStr+ "," + laRpTypDst[lnTarget]
    ENDFOR
    lcTypesStr = SUBSTR(lcTypesStr,2)
  ENDIF
  RETURN lcTypesStr
ENDFUNC

