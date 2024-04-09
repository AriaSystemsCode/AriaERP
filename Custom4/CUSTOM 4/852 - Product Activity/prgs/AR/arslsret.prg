*:***************************************************************************
*: Program file  : ARSLSRET.PRG
*: Program desc. : Sales and Return Screen
*: Date          : 10/15/2018
*: System        : Aria 4 XP
*: Module        : Accounts Receivable (AR)
*: Developer     : Mariam Mazhar (MMT)
*: Tracking Job #: E304036 [P20171120.0011]
*:***************************************************************************
*: Modificaton:
*:***************************************************************************
*!*************************************************************
*! Name      : lfFormInit
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/15/2018
*! Purpose   : Form Init Method
*!*************************************************************
FUNCTION lfFormInit
LPARAMETERS loFormSet

WITH loFormSet
 DECLARE .lapanelobj [1,6]
  .laPanelObj[1,1] = "cmdScope"
  .laPanelObj[1,2] = oAriaApplication.BitMapHome+ 'SCOPE.BMP'
  .laPanelObj[1,3] = 'mcallscope' 
  .laPanelObj[1,4] = 'Scope'
  .laPanelObj[1,5] = 'Scope'&&IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SCOPE,oAriaApplication.GetHeaderText("LANG_SCOPE",AHEADERFILE))
  .laPanelObj[1,6] = 'S'
ENDWITH  
*!*************************************************************
*! Name      : lfChangeScrMode
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/15/2018
*! Purpose   : Form Change Mode Method
*!*************************************************************
FUNCTION lfChangeScrMode
LPARAMETERS loFormSet,lcModeToChange
IF lcModeToChange ='E'
  loFormSet.AriaForm1.pgfSlsRet.Pages(1).grdinvoices.Enabled = .T.
  loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.Enabled = .T.
  loFormSet.AriaForm1.pgfSlsRet.Pages(1).grdinvoices.Readonly= .T.
  loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.Readonly= .T.
ENDIF
IF lcModeToChange ='S'
  loFormSet.AriaForm1.pgfSlsRet.Pages(1).grdinvoices.Enabled = .F.
  loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.Enabled = .F.
  loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.RecordSource =''
  loFormSet.AriaForm1.pgfSlsRet.Pages(1).grdinvoices.RecordSource =''  
ENDIF
*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/15/2018
*! Purpose   : Option Grid When Function
*!*************************************************************
FUNCTION lfwOGWhen
IF !USED('smart_invoice_header')
  =gfOpenTable('smart_invoice_header','sminvhdr')
ENDIF
SELECT 'smart_invoice_header'
=gfSQLRUN("Select * FROM smart_invoice_header where Status <> 'C'",'smart_invoice_header',.F.,'smart_header')
SELECT smart_header
CURSORSETPROP("Buffering" ,3)
INDEX On smart_invoice_no TAG 'smartinvh'
LOCATE
*!*************************************************************
*! Name      : lfCallScopeOG
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/15/2018
*! Purpose   : Function to call scope option grid
*!*************************************************************
FUNCTION lfCallScopeOG
PARAMETERS loFormSet

*lcRpQtyTyp = loFormSet.lcRpQtyTyp
DIMENSION laogFxFlt[1,7]
DIMENSION laogVrFlt[1,7]
laogVrFlt =''
laogFxFlt =''
lcExpr = gfOpGrid('ARSLSRT' , .T.)
IF lcExpr <> '.F.'
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SMART_INVOICE'),1)
  llUseSmrtInv = .F.
  lcSmrtInvFile = ''
  IF lnPosition > 0
    lcSmrtInvFile= laOGFxFlt[lnPosition,6]
    llUseSmrtInv = IIF(!EMPTY(lcSmrtInvFile) .AND. USED(lcSmrtInvFile) .AND. RECCOUNT(lcSmrtInvFile)>0,.T.,.F.)
  ENDIF
  lnPositionAc = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
  llUseAcc = .F.
  lcAccFile = ''
  IF lnPositionAc> 0
    lcAccFile = laOGFxFlt[lnPositionAc,6]
    llUseAcc = IIF(!EMPTY(lcAccFile) .AND. USED(lcAccFile) .AND. RECCOUNT(lcAccFile)>0,.T.,.F.)
  ENDIF
  ldStartDate = {}
  ldEndDate    = {}
  lnDatePos = 0
  llDateSelect = .F.
  lnDatePos = ASCAN(laOgFxFlt,'INVOICE.DATE')
  IF lnDatePos > 0
    lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
    llDateSelect = !EMPTY(laOGFxFlt[lnDatePos,6])
    IF llDateSelect 
      ldStartDate = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10))
      ldEndDate   = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],12,21))
    ENDif  
  ENDIF
  lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
  lnResult=oAriaApplication.remotecompanydata.SqlRun("Select SMART_INVOICE_LINES.*,'      ' as CDIVISION,'      ' as SEASON from SMART_INVOICE_LINES WHERE 1=2 ",;
           'SMRTSHPLIN','SMART_INVOICE_LINES',lcTranCode ,4,'SAVE',Set("Datasession"))
  IF lnResult<>1
    oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
    RETURN 
  ELSE
    SELECT 'SMRTSHPLIN'
    CURSORSETPROP("Buffering" ,3)
    INDEX ON Account+STORE+ORDER TAG 'SMARTLINE'
    INDEX on SMART_INVOICE_NO + STYLE TAG 'SMARTLN' ADDITIVE 
    SET ORDER TO 'SMARTLN' 
  ENDIF             
  IF !USED("STYLE")
    =gfOpenTable("STYLE",'STYLE')
  ENDIF
  IF !USED("STYDYE")
    =gfOpenTable("STYDYE",'STYDYE')
  ENDIF  
  IF llUseSmrtInv 
    SELECT(lcSmrtInvFile)
    LOCATE
    SCAN 
      lcSmartInv = &lcSmrtInvFile..SMART_INVOICE_NO 
      lnResult=oAriaApplication.remotecompanydata.SqlRun("Select SMART_INVOICE_LINES.* from SMART_INVOICE_LINES INNER JOIN SMART_INVOICE_HEADER ON  "+;
          "SMART_INVOICE_HEADER.SMART_INVOICE_NO = SMART_INVOICE_LINES.SMART_INVOICE_NO"+;
		  " WHERE SMART_INVOICE_HEADER.Invoice ='' AND Status !='C' AND"+;
          " (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S') AND SMART_INVOICE_HEADER.SMART_INVOICE_NO = '"+lcSmartInv+"'"+;
          IIF(llDateSelect," AND SUBSTRING(SMART_INVOICE_HEADER.SMART_INVOICE_NO,1,8) BETWEEN '"+DTOS(ldStartDate)+"' AND '"+DTOS(ldEndDate)+"'",""),;
          'SMART_INVOICE_LINE','SMART_INVOICE_LINES',lcTranCode ,4,'SAVE',Set("Datasession"))
      If lnResult<>1
        oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
        LOOP
      ENDIF 
      SELECT 'SMART_INVOICE_LINE'
      SCAN FOR IIF(llUseAcc,SEEK(SMRTSHPLIN.Account,lcAccFile),.T.)
        SCATTER MEMO MEMVAR
        =gfSeek(m.Style,'Style','Style')
        m.CDIVISION = Style.CDIVISION
        m.Season = Style.Season
        INSERT INTO 'SMRTSHPLIN' FROM MEMVAR 
      ENDSCAN 
    ENDSCAN
  ELSE
    IF llUseAcc 
      SELECT (lcAccFile)
      LOCATE
      SCAN
        lcAccountId = &lcAccFile..Account
        lnResult=oAriaApplication.remotecompanydata.SqlRun("Select SMART_INVOICE_LINES.* from SMART_INVOICE_LINES INNER JOIN SMART_INVOICE_HEADER On  "+;
          "SMART_INVOICE_HEADER.SMART_INVOICE_NO = SMART_INVOICE_LINES.SMART_INVOICE_NO"+;
		  " WHERE SMART_INVOICE_HEADER.Invoice ='' AND Status !='C' AND"+;
          " (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S') AND SMART_INVOICE_HEADER.Account= '"+lcAccountId +"'"+;
          IIF(llDateSelect," AND SUBSTRING(SMART_INVOICE_HEADER.SMART_INVOICE_NO,1,8) BETWEEN '"+DTOS(ldStartDate)+"' AND '"+DTOS(ldEndDate)+"'",""),;
          'SMART_INVOICE_LINE','SMART_INVOICE_LINES',lcTranCode ,4,'SAVE',Set("Datasession"))
        IF lnResult<>1
          oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
          LOOP
        ENDIF 
        SELECT 'SMART_INVOICE_LINE'
        SCAN
          SCATTER MEMO MEMVAR
           =gfSeek(m.Style,'Style','Style')
           m.CDIVISION = Style.CDIVISION
           m.Season = Style.Season           
          INSERT INTO 'SMRTSHPLIN' FROM MEMVAR 
        ENDSCAN 
      ENDSCAN
    ELSE
        lnResult=oAriaApplication.remotecompanydata.SqlRun("Select SMART_INVOICE_LINES.* from SMART_INVOICE_LINES INNER JOIN SMART_INVOICE_HEADER ON "+;
          "SMART_INVOICE_HEADER.SMART_INVOICE_NO = SMART_INVOICE_LINES.SMART_INVOICE_NO"+;
		  " WHERE SMART_INVOICE_HEADER.Invoice ='' AND Status !='C' AND"+;
          " (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S') "+;
          IIF(llDateSelect," AND SUBSTRING(SMART_INVOICE_HEADER.SMART_INVOICE_NO,1,8) BETWEEN '"+DTOS(ldStartDate)+"' AND '"+DTOS(ldEndDate)+"'",""),;
          'SMART_INVOICE_LINE','SMART_INVOICE_LINES',lcTranCode ,4,'SAVE',Set("Datasession"))
        IF lnResult<>1
          oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
          LOOP
        ENDIF 
        SELECT 'SMART_INVOICE_LINE'
        SCAN
          SCATTER MEMO MEMVAR
           =gfSeek(m.Style,'Style','Style')
           m.CDIVISION = Style.CDIVISION
           m.Season = Style.Season
          INSERT INTO 'SMRTSHPLIN' FROM MEMVAR 
        ENDSCAN 
    ENDIF
  ENDIF

  SELECT 'SMRTSHPLIN'
  Locate
  IF !Eof()
    *lcInvNo = lfCreateConsLines()
    loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.RecordSource =''
    loFormSet.AriaForm1.pgfSlsRet.Pages(1).grdinvoices.RecordSource =''
    IF !USED("CODES")
      =gfOpenTable("CODES",'CODES')
    ENDIF
    IF !USED("Customer")
      =gfOpenTable("Customer",'Customer')
    ENDIF
    IF !USED("ORDHDR")
      =gfOpenTable("ORDHDR",'ORDHDR')
    ENDIF
    
    LOCATE FOR totqty < 0 
    IF FOUND()
      lcDefRetRes = ''
      
      IF gfSeek('D'+'REASON','Codes','CCODE_NO')
        lcDefRetResCode = Codes.CCODE_NO
        lcDefRetRes = gfCodDes(lcDefRetResCode ,'REASON    ')  
      ENDIF
      SELECT SPACE(6) as 'CRMEMO',lcDefRetRes as 'DefReason',Account,Store,CDIVISION,STR(SUM(TOTQTY),7)As 'RetQty',;
             str(SUM(TotQty*Price),12,2) As 'RetAmt',oAriaApplication.SystemDate As 'Date' ;
             FROM SMRTSHPLIN WHERE totqty < 0 GROUP BY Account,Store,CDIVISION  INTO CURSOR 'Acc_Store' READWRITE 
      SELECT 'Acc_Store'
      CURSORSETPROP("Buffering" ,3)
      INDEX on  CRMEMO + Account+Store TAG 'Acc_Store' 
      
          
      
      loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.RecordSource ='Acc_Store'
      loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.Column1.ControlSource = 'Acc_Store.CRMEMO'
      loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.Column2.ControlSource = 'Acc_Store.Account'
      loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.Column3.ControlSource = 'Acc_Store.Date'
      loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.Column4.ControlSource = 'Acc_Store.RetQty'
      loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.Column5.ControlSource = 'Acc_Store.RetAmt'
      loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.Column6.ControlSource = 'Acc_Store.DefReason'                        
      loFormSet.AriaForm1.pgfSlsRet.Pages(2).grdCrmemo.Column7.ControlSource = 'Acc_Store.Store'      
    ENDIF
    SELECT 'SMRTSHPLIN'
    LOCATE FOR totqty > 0 
    IF FOUND()
      CREATE CURSOR 'INV_STORE' (Account C(5),Store C(8),CDIVISION C(6),INVOICE C(6),Date D(8), ShipQty N(9),ShipAmt N(12,2),cCurrCode C(3),DIST_CTR  C(8),Type C(1))
      SELECT 'INV_STORE'
      INDEX on INVOICE+Account+CDIVISION+cCurrCode+Type+DIST_CTR TAG  'INV_STORE'
      SELECT  SMRTSHPLIN 
      SCAN FOR TotQty >0 AND !EMPTY(Order)
        =gfSeek('O'+SMRTSHPLIN.Order,'ORDHDR','ORDHDR')
        llConsDist = .F.
        llConsAcc = .F.
        IF gfSeek('M'+SMRTSHPLIN.Account,'Customer','Customer')  AND CUSTOMER.Consol = 'Y'
          llConsDist = CUSTOMER.ConByDc $ 'SY'
          llConsAcc = .T.  
        ENDIF 
        =IIF(EMPTY(SMRTSHPLIN.Store),gfSeek('M'+SMRTSHPLIN.ACCOUNT,'Customer'),gfSeek('S'+SMRTSHPLIN.ACCOUNT+SMRTSHPLIN.Store,'Customer'))
        LCDIST_CTR = CUSTOMER.DIST_CTR  
        IF llConsAcc AND SEEK(SPACE(6)+SMRTSHPLIN.Account+ORDHDR.CDIVISION+ORDHDR.cCurrCode+'S'+Iif(llConsDist,LCDIST_CTR,SPACE(8)),'INV_STORE','INV_STORE') &&Iif(llConsDist,LCDIST_CTR,SPACE(8))
          REPLACE ShipAmt WITH ShipAmt +(SMRTSHPLIN.TOTQTY*SMRTSHPLIN.Price),;
                  ShipQty WITH ShipQty + SMRTSHPLIN.TOTQTY,;
                  Store   WITH IIF(Store <> SMRTSHPLIN.Store,'',Store) in 'INV_STORE'
        ELSE
          INSERT INTO 'INV_STORE' (Account,Store,CDIVISION,INVOICE,Date,ShipQty,ShipAmt,cCurrCode,DIST_CTR,Type) VALUES ;
          (SMRTSHPLIN.Account,SMRTSHPLIN.Store,ORDHDR.CDIVISION,'',oAriaApplication.SystemDate,SMRTSHPLIN.TOTQTY,;
          SMRTSHPLIN.TOTQTY*SMRTSHPLIN.Price,ORDHDR.cCurrCode,Iif(llConsDist,LCDIST_CTR,SPACE(8)),'S')
        ENDIF
      ENDSCAN
      
*!*	      SELECT Account,Store,CDIVISION,Order,SPACE(6) AS 'INVOICE',oAriaApplication.SystemDate As 'Date',SUM(TOTQTY) As 'ShipQty',;
*!*	           SUM(TotQty*Price) As 'ShipAmt' FROM SMRTSHPLIN WHERE totqty > 0 GROUP BY Account,Store,CDIVISION,Order INTO CURSOR 'INV_STORE' READWRITE 
           
      SELECT 'SMRTSHPLIN'
      SCAN FOR TotQty >0 AND EMPTY(Order)
        IF !SEEK(SPACE(6)+SMRTSHPLIN.Account+SMRTSHPLIN.CDIVISION+SPACE(3)+'D'+SPACE(8),'INV_STORE','INV_STORE')
           INSERT INTO 'INV_STORE' (Account,Store,CDIVISION,INVOICE,Date,ShipQty,ShipAmt,cCurrCode,DIST_CTR,type) VALUES ;
          (SMRTSHPLIN.Account,SMRTSHPLIN.STORE,SMRTSHPLIN.CDIVISION,'',oAriaApplication.SystemDate,SMRTSHPLIN.TOTQTY,;
          SMRTSHPLIN.TOTQTY*SMRTSHPLIN.Price,'','','D')
        ELSE
          REPLACE ShipAmt WITH ShipAmt +(SMRTSHPLIN.TOTQTY*SMRTSHPLIN.Price),;
                  ShipQty WITH ShipQty + SMRTSHPLIN.TOTQTY in 'INV_STORE'
        ENDIF
	  ENDSCAN  
	  SELECT 'INV_STORE'
	  LOCATE  
*      SELECT Account,Store,CDIVISION,Order,SPACE(6) AS 'INVOICE',oAriaApplication.SystemDate As 'Date',SUM(TOTQTY) As 'ShipQty',;
           SUM(TotQty*Price) As 'ShipAmt' FROM SMRTSHPLIN WHERE totqty > 0 GROUP BY Account,Store,CDIVISION,Order INTO CURSOR 'INV_STORE' READWRITE 
      *SELECT Account,Store,CDIVISION,IIF(EMPTY(Order),Store,'') as 'Store',SPACE(6) AS 'INVOICE',oAriaApplication.SystemDate As 'Date',SUM(TOTQTY) As 'ShipQty',;
           SUM(TotQty*Price) As 'ShipAmt' FROM SMRTSHPLIN WHERE totqty > 0 GROUP BY Account,CDIVISION,IIF(EMPTY(Order),Store,'') INTO CURSOR 'INV_STORE' READWRITE            
*!*	      SELECT 'INV_STORE'
*!*	      CURSORSETPROP("Buffering" ,3)
*!*	      INDEX on  INVOICE + Account+Store TAG 'INV_STORE'             
           
	  loFormSet.AriaForm1.pgfSlsRet.Pages(1).grdinvoices.RecordSource ='INV_STORE'
	  loFormSet.AriaForm1.pgfSlsRet.Pages(1).grdinvoices.Column1.ControlSource = 'INV_STORE.Invoice'
	  loFormSet.AriaForm1.pgfSlsRet.Pages(1).grdinvoices.Column2.ControlSource = 'INV_STORE.Account'
	  loFormSet.AriaForm1.pgfSlsRet.Pages(1).grdinvoices.Column3.ControlSource = 'INV_STORE.Date'
	  loFormSet.AriaForm1.pgfSlsRet.Pages(1).grdinvoices.Column4.ControlSource = 'INV_STORE.ShipQty'
	  loFormSet.AriaForm1.pgfSlsRet.Pages(1).grdinvoices.Column5.ControlSource = 'INV_STORE.ShipAmt'
	  loFormSet.AriaForm1.pgfSlsRet.Pages(1).grdinvoices.Column6.ControlSource = 'INV_STORE.Store'                        
   ENDIF 
   loFormSet.ChangeMode('E')
  ELSE
    =gfModalgen ('TRM00342B000000','ALERT') 
  ENDIF
ENDIF
  
*!*************************************************************
*! Name      : lfCreateConsLines
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/15/2018
*! Purpose   : Function to create Transactions
*!*************************************************************
FUNCTION lfCreateConsLines

Private lcKey
STORE 1 TO LNEXRATE, LNCURRUNIT
ldPaymntDate = oAriaApplication.SystemDate
lnDataSessionCurrent =SET("Datasession")
lcDefWareHouse = ''
IF  !USED('WareH')
  =gfOpenTable('WareHous','WAREHOUS','SH','WareH') 
ENDIF  
SELECT 'WareH' 
=gfSeek('')
LOCATE FOR lDefWare
IF FOUND()
  lcDefWareHouse  =  WareH.CWARECODE 
ENDIF
Select SMRTSHPLIN
LOCATE
SCAN
  Select SMRTSHPLIN
  IF !gfSEEK(SUBSTR(SMRTSHPLIN.STORE,1,6),'WareH','WAREHOUS')
    SELECT 'WareH' 
    APPEND BLANK 
    REPLACE CWARECODE  WITH SMRTSHPLIN.STORE,;
            cDesc WITH SMRTSHPLIN.STORE
    =gfAdd_Info('WareH')       
    =gfReplace('')        
  ENDIF
ENDSCAN
SELECT 'WareH' 
=gfTableUpdate()
Select SMRTSHPLIN
LOCATE
SCAN
  IF !gfSEEK('S'+SMRTSHPLIN.Account+SMRTSHPLIN.STORE,'Customer' ,'Customer')
    lfAddStore(SMRTSHPLIN.Account,SMRTSHPLIN.STORE)
  ENDIF
ENDSCAN 

  * Check Credit memo Lines 
*  ON Error MESSAGEBOX(MESSAGE())
SELECT SMRTSHPLIN
LOCATE FOR totqty < 0
IF FOUND()
  lfCreateCreditMemo()
ENDIF
  SET DATASESSION TO (lnDataSessionCurrent)
  * Check Lines with Qty > 0 and has no linked orders
  Select SMRTSHPLIN
  LOCATE FOR totqty > 0 AND EMPTY(Order)
  IF FOUND()
    lfCreateDirectInvoice()
  ENDIF
  SET DATASESSION TO (lnDataSessionCurrent)
  if !USED('INVHDR_X')
    =gfOpenTable('INVHDR','INVHDRA','SH','INVHDR_X')
  ENDIF
*!*	  Select 'INVHDR_X'
*!*	  =gfSeek(lcAccount)
*!*	  Select Invoice from INVHDR_X Where Account+Invoice =lcAccount And Consol ='Y' and Status <>'V' into Array laInvCrt &&and allt(Note2) = lcFileCode 
*!*	  if _TALLY > 0
*!*	    If !Empty(Alltrim(laInvCrt[1]))
*!*	      SEleCT  'RemitsInv'
*!*	      LOCATE
*!*	        lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_HEADER Set INVOICE ='"+laInvCrt[1]+;
*!*	        "', Status ='C' Where SMART_INVOICE_NO >= '"+lcFirstInv+;
*!*	        "' AND  SMART_INVOICE_NO <= '"+lcLastInv +"' AND  Invoice ='' AND Status !='C'",'SMRTSHPHDR' , ;
*!*	        "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
*!*	        If lnResult<>1
*!*	          oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
*!*	          Wait Window 'SMART_INVOICE_HEADER'+'.SqlRun()'
*!*	          loop
*!*	        ELSE
*!*	         lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_LINES Set cTrCode ='"+laInvCrt[1]+;
*!*	                  "' Where SMART_INVOICE_NO >= '"+lcFirstInv+;
*!*	                  "' AND  SMART_INVOICE_NO <= '"+lcLastInv +"' and [Order]<>''  AND (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S')",'SMRTSHPLINUP' , ;
*!*	                  "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
*!*	          If lnResult<>1
*!*	            oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
*!*	            Wait Window 'SMART_INVOICE_LINES'+'.SqlRun()'
*!*	            loop
*!*	          Endif
*!*	        Endif
*!*	    ENDIF
*!*	    Return laInvCrt[1]
*!*	  ENDIF
 
  lcInvHdr = gfTempName()
  lcConsInvH = gfTempName()
  lcConsInvD = gfTempName()
  lcInvLine = gfTempName()
  llUseTradeDisc = gfGetMemVar('M_TRDDISCL',oAriaApplication.activecompanyid)
  lcCostingMethod = gfGetMemVar('M_COST_METH',oAriaApplication.activecompanyid)
  lfCreateInvTemp()
  IF !USED("CODES")
    =gfOpenTable("CODES",'CODES')
  ENDIF
  Wait window 'Creating Consolidated Invoice...' nowait 
  Select SMRTSHPLIN
  Count to lnTotCons FOR  totqty > 0 AND !EMPTY(Order) &&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv')
  lnCountLine = 0
  Select SMRTSHPLIN
  Locate
  lnNoIncVar = 0
  SCAN FOR  totqty > 0 AND !EMPTY(Order) &&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv','RemitsInv')
    lnCountLine = lnCountLine + 1
    lnPerCent = (lnCountLine / lnTotCons)* 100
    WAIT WINDOW 'Adding Smart invoice# '+SMRTSHPLIN.SMART_INVOICE_NO +" to the consolidated invoice...."+ ALLT(Str(lnPerCent,3))+"%" nowait
    Scatter Memo Memvar
    m.Dyelot =''
    =gfSeek(m.Style, 'STYLE', 'STYLE')
    =gfSeek('O'+m.Order,'Ordhdr','Ordhdr')
    =gfSeek('S'+m.ACCOUNT+m.Store,'Customer','Customer')
    =gfSeek('O'+m.Order+Str(m.nOrdlineNo,6),'Ordline','Ordline')
    m.CWARECODE = OrdLine.CWARECODE
    m.ALTSTYLE =''
    m.Comm1 = OrdLine.Comm1
    m.Comm2 = OrdLine.Comm2
    m.DESC1 = OrdLine.DESC1
    LCDIST_CTR = ''
    LCSTORE = m.Store
    LCSALEREP1 = ORDHDR.REP1
    LNCOMM1    = ORDHDR.Comm1
    LCSALEREP2 = ORDHDR.REP2
    LNCOMM2    = ORDHDR.Comm2
    If ORDHDR.Multi='Y' And Empty(ORDHDR.REP1) And Empty(ORDHDR.REP2)
      LCSALEREP1 = CUSTOMER.SALESREP
      LNCOMM1    = CUSTOMER.Comm
      LCSALEREP2 = CUSTOMER.REP2
      LNCOMM2    = CUSTOMER.Comm2
    Endif
    If !Empty(OrdLine.ALTSTYLE)
      m.Style = OrdLine.ALTSTYLE
      m.ALTSTYLE = OrdLine.Style
      =gfSeek(OrdLine.ALTSTYLE,'STYLE')
      m.DESC1  = Style.DESC1
    Endif
    =Iif(Empty(m.Store),gfSeek('M'+m.ACCOUNT,'Customer'),gfSeek('S'+m.ACCOUNT+m.Store,'Customer'))
    LCDIST_CTR = CUSTOMER.DIST_CTR
    =Seek(m.Style+m.CWARECODE+m.Dyelot,'StyDye')
    m.COST = STYDYE.AVE_COST
    =Seek(m.Style,'Style')
    m.GROS_PRICE = m.PRICE
    If llUseTradeDisc
      m.TRD_PRICE = Round((m.GROS_PRICE*(100-m.TRDE_DISC)/100),2)
      m.OTRDDISC = m.TRDE_DISC
    Endif
    m.LTAXABLE = Style.LTAXABLE
    *-- Compute merchandise tax amount for england
    Store 0 To LNTAXQTY,LNTAXRATE
     IF !SEEK(m.Account+m.Order+m.Store+m.STYLE+STR(m.nOrdLineNo,6),lcInvLine,'ConsORDLN')   
      lnNoIncVar = lnNoIncVar + 1
      Insert Into (lcInvLine);
      (Order,ACCOUNT,Lineno,Store,PIKTKT,Style,ALTSTYLE,Dyelot,NOTE_MEM,Comm1,Comm2,BOOK1,BOOK2,;
      BOOK3,BOOK4,BOOK5,BOOK6,BOOK7,BOOK8,TOTBOOK,Flag,PIK1,PIK2,PIK3,PIK4,PIK5,;
      PIK6,PIK7,PIK8,TOTPIK,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,;
      PRICE,PACK_ID,GROS_PRICE,DISC_PCNT,LPACKED,LBACKORD,NTAXRATE,Group,PREPAK,;
      DESC1,SEASON,PPQTY,Scale,CWARECODE,Consol,CDIVISION,cCurrCode,LTAXABLE,CDYEFLAG,CWARECODE,DIST_CTR,NORDLINE);
      VALUES (m.Order,m.ACCOUNT,lnNoIncVar,m.Store,'',m.Style,m.ALTSTYLE,m.Dyelot,;
      OrdLine.NOTE_MEM,m.Comm1,m.Comm2,m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,;
      M.QTY7,m.QTY8,m.TOTQTY,' ',0,0,0,0,;
      0,0,0,0,0,m.QTY1,m.QTY2,m.QTY3,m.QTY4,;
      M.QTY5,m.QTY6,m.QTY7,m.QTY8,m.TOTQTY,m.PRICE,OrdLine.PACK_ID,;
      M.GROS_PRICE,0,.F.,.T.,LNTAXRATE,OrdLine.Group,OrdLine.PREPAK,;
      M.DESC1,OrdLine.SEASON,OrdLine.PPQTY,OrdLine.Scale,OrdLine.CWARECODE,'N',ORDHDR.CDIVISION,ORDHDR.cCurrCode,m.LTAXABLE,Style.CDYE_FLG,OrdLine.CWARECODE,LCDIST_CTR,OrdLine.Lineno)
    ELSE
      FOR lnCnr = 1 TO 8
        lcCnr = STR(lnCnr ,1)
        REPLACE Qty&lcCnr. WITH Qty&lcCnr. + m.Qty&lcCnr. ,;
                BOOK&lcCnr. WITH BOOK&lcCnr. + m.Qty&lcCnr. IN (lcInvLine)
      ENDFOR
      REPLACE GROS_PRICE WITH ((TotQty * GROS_PRICE + m.TotQty * m.GROS_PRICE)/(TotQty + m.TotQty)),;
              PRICE    WITH   ((TotQty * PRICE + m.TotQty * m.PRICE)/(TotQty + m.TotQty)) IN (lcInvLine)
      REPLACE TotQty WITH TotQty + m.TotQty,;
              TOTBOOK WITH TOTBOOK + m.TotQty IN (lcInvLine)
    ENDIF  
    Replace OLDDYELOT With m.Dyelot In (lcInvLine)
    If llUseTradeDisc
      Replace TRDE_DISC With OrdLine.TRDE_DISC,;
              OTRDDISC With OrdLine.TRDE_DISC,;
              TRD_PRICE With OrdLine.TRD_PRICE  In (lcInvLine)
    Endif
    *-- Do not ship more than on hand quantity if LIFO of FIFO costing methods is used.
    If Inlist(lcCostingMethod ,'F','L') And gfSeek(m.Style+m.CWARECODE+m.Dyelot,'StyDye')
      Replace QTY1 With Max(Min(QTY1,STYDYE.STK1),0) ,;
        QTY2 With Max(Min(QTY2,STYDYE.STK2),0) ,;
        QTY3 With Max(Min(QTY3,STYDYE.STK3),0) ,;
        QTY4 With Max(Min(QTY4,STYDYE.STK4),0) ,;
        QTY5 With Max(Min(QTY5,STYDYE.STK5),0) ,;
        QTY6 With Max(Min(QTY6,STYDYE.STK6),0) ,;
        QTY7 With Max(Min(QTY7,STYDYE.STK7),0) ,;
        QTY8 With Max(Min(QTY8,STYDYE.STK8),0) ,;
        TOTQTY With QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 In (lcInvLine)
    Endif
    Declare LATRLTFLD[6,2]
    LATRLTFLD[1,1] = 'NTERDISCR'
    LATRLTFLD[1,2] = 'lnTerDiscR'
    LATRLTFLD[2,1] = 'EOM '
    LATRLTFLD[2,2] = 'lcTEOM'
    LATRLTFLD[3,1] = 'NTERDUED'
    LATRLTFLD[3,2] = 'lnTDaysDue'
    LATRLTFLD[4,1] = 'CODYN'
    LATRLTFLD[4,2] = 'lcTCod'
    LATRLTFLD[5,1] = 'EOMDAY'
    LATRLTFLD[5,2] = 'lnEomDay'
    LATRLTFLD[6,1] = 'LINSTALLM'
    LATRLTFLD[6,2] = 'llInstTerm'  && Use Instalment
    Store 0  To LNTERDISCR, LNTDAYSDUE, LNEOMDAY
    Store '' To LCTEOM, LCTCOD
    Store .F. To LLINSTTERM
    =gfRltFld(ORDHDR.CTERMCODE,@LATRLTFLD,'CTERMCODE')
    LCCURRCODE = Iif(Empty(ORDHDR.cCurrCode), oAriaApplication.BASECURRENCY, ORDHDR.cCurrCode)
    Select (lcInvHdr)
    If !Seek(m.ACCOUNT+m.Order+m.Store)
      Append Blank
      Replace Order     With m.Order ,;
        STORE     With m.Store ,;
        PIKTKT    With '' ,;
        CUSTPO    With ORDHDR.CUSTPO,;
        DIST_CTR  With LCDIST_CTR  ,;
        ACCOUNT   With ORDHDR.ACCOUNT ,;
        CTERMCODE With ORDHDR.CTERMCODE  ,;
        SPCINST   With ORDHDR.SPCINST ,;
        LUPSINS   With (ORDHDR.CINSUR='Y') ,;
        REP1      With LCSALEREP1 ,;
        Comm1     With LNCOMM1    ,;
        REP2      With LCSALEREP2 ,;
        Comm2     With LNCOMM2    ,;
        NOTE1     With ORDHDR.NOTE1 ,;
        NOTE2     With ORDHDR.NOTE2 ,;
        LCOMPUPS  With .T. ,;
        CONSOL    With 'N' ,;
        LASTLINE  With ORDHDR.LASTLINE,;
        CCARTRCKNO With  ORDHDR.CCARTRCKNO
        
        LDDUEDATE = Iif(LCTEOM <> 'Y',ldPaymntDate+LNTDAYSDUE,;
        GOMONTH(Ctod(Substr(Dtoc(ldPaymntDate),1,3)+'10'+;
        SUBSTR(Dtoc(ldPaymntDate),6,5)),Iif(Day(ldPaymntDate) > LNEOMDAY,2,1))+LNTDAYSDUE)
        
      Replace  DISCPCNT   With  ORDHDR.DISC                                      ,;
        INVDATE    With ldPaymntDate ,;
        SHIPDATE   With ldPaymntDate , ;
        DPOSTDATE  With ldPaymntDate  ,;
        DUEDATE    With LDDUEDATE                                         ,;
        DEPT       With ORDHDR.DEPT                                       ,;
        CFACCODE   With ORDHDR.CFACCODE                                   ,;
        APPROVAL   With ORDHDR.APPROVAL                                   ,;
        appramt    With ORDHDR.appramt                                    ,;
        SEASON     With ORDHDR.SEASON                                     ,;
        CDIVISION  With ORDHDR.CDIVISION                                  ,;
        UPSZONE    With CUSTOMER.UPSZONE                                  ,;
        PHONE      With CUSTOMER.PHONE1                                   ,;
        CWARECODE  With ORDHDR.CWARECODE ,;
        TRDE_DISC  With LNTERDISCR                                        ,;
        TAX_RATE   With 0 ,;
        NPSTRATE   With 0 ,;
        CTAXRULE   With '' ,;
        NHSTRATE   With 0 ,;
        COD_FLAG   With Iif(LCTCOD='Y','Y','N'),;
        STATUS     With ORDHDR.Status ,;
        cCurrCode  With LCCURRCODE ,;
        nExRate    With LNEXRATE   ,;
        nCurrUnit  With LNCURRUNIT ,;
        DADD_DATE  With Date()   ,;
        CADD_TIME  With Time()     ,;
        CADD_USER  With oAriaApplication.USER_ID

    ENDIF

    LNCARTONS  = NCARTONS + Iif(Style.QTY_CTN>0,m.TOTQTY/Style.QTY_CTN,0)
    LNALLCARTN = Iif(Ceiling(LNCARTONS)=0,1,Ceiling(LNCARTONS))
    Replace ORDERED   With ORDERED  + OrdLine.TOTBOOK ,;
      SHIP      With SHIP     + m.TOTQTY ,;
      SHIPAMT   With SHIPAMT  + m.TOTQTY*m.PRICE ,;
      DISCOUNT  With -SHIPAMT * DISCPCNT/100,;
      WEIGHT    With WEIGHT   + m.TOTQTY*Style.NSTYWEIGHT ,;
      NCARTONS  With LNCARTONS ,;
      CARTONS   With LNALLCARTN ,;
      PICKED    With 0 ,;
      SHIPVIA   With Iif(CUSTOMER.NBRKWEIGHT <> 0 And WEIGHT > CUSTOMER.NBRKWEIGHT,CUSTOMER.CALTSHPVIA,;
      IIF(Alltrim(ORDHDR.SHIPVIA)='*',CUSTOMER.SHIPVIA,ORDHDR.SHIPVIA)),;
      NMERCHTAX With NMERCHTAX + LNTAXQTY * m.PRICE * LNTAXRATE/100 ,;
      TAX_AMT   With NMERCHTAX*(100-DISCPCNT)/100*(100-TRDE_DISC)/100  ,;
      COD_AMT   With 0 ,;
      TOTALCHG  With 0 ,;
      NTAXDUE   With NTAXDUE + Iif(m.LTAXABLE,m.TOTQTY*m.PRICE,0)

    Select (lcInvHdr)
    Replace NOLDCARTON With CARTONS,;
      NOLDWIGHT  With WEIGHT

    If llUseTradeDisc
      Replace TRDDSCAMNT With TRDDSCAMNT+Round(((m.TOTQTY*m.PRICE*m.TRDE_DISC)/100),2),;
        TRDE_DISC  With  Iif((SHIPAMT+DISCOUNT)<>0, (TRDDSCAMNT/(SHIPAMT+DISCOUNT))*100,TRDE_DISC)
    Endif
  ENDSCAN 
  llconsbydc = .F.
  Select (lcInvLine)
  Set Order To CONSDIV
  Select (lcInvHdr)
  Set Order To Tag Consol
  Go Top

  Do While !Eof()
    lcAccount = ACCOUNT
    llConsDist = gfSeek('M'+lcAccount,'Customer') And CUSTOMER.ConByDc $ 'SY'
    llConsStore = gfSeek('M'+lcAccount,'Customer') And CUSTOMER.ConByDc = 'S'

    If llConsDist
      llconsbydc = .T.
    Endif
    lcKey = Consol+ACCOUNT+CDIVISION+cCurrCode+Iif(llConsDist,DIST_CTR,"")

    lnInvoices = 0
    lcOrderNo  = Order
    lcTermCode = CTERMCODE
    lcShipvia  = SHIPVIA
    If gfSeek('M'+lcAccount,'Customer') And CUSTOMER.Consol = 'Y'
      Scan Rest While Consol+ACCOUNT+CDIVISION+cCurrCode+DIST_CTR = lcKey
        lnInvoices = lnInvoices + 1
        lcOrderNo  = Iif(Order    =lcOrderNo ,lcOrderNo ,Space(6))
        lcTermCode = Iif(CTERMCODE=lcTermCode,lcTermCode,Space(6))
        lcShipvia  = Iif(SHIPVIA = lcShipvia ,lcShipvia ,'*')
      Endscan
    Endif
    llConsInv = gfSeek('M'+lcAccount,'Customer') And CUSTOMER.Consol='Y' And lnInvoices > 1
    =Seek(lcKey)
    Local lnTax_Amt,lnTOTALCHG,lnOrdTax
    lnTax_Amt  = 0
    lnTOTALCHG = 0
    Scan Rest While Consol+ACCOUNT+CDIVISION+cCurrCode+DIST_CTR = lcKey For lnInvoices > 1
      Scatter Memvar
      m.SHIPVIA = lcShipvia
      Select (lcConsInvH)
      If !Seek('Y'+m.ACCOUNT+m.CDIVISION+m.cCurrCode+Iif(llConsDist,m.DIST_CTR,""))
        Append Blank
        Replace cSelect   With '»'         ,;
          Consol    With 'Y'         ,;
          ACCOUNT   With m.ACCOUNT   ,;
          CDIVISION With m.CDIVISION ,;
          cCurrCode With m.cCurrCode ,;
          nExRate   With m.nExRate   ,;
          nCurrUnit With m.nCurrUnit ,;
          Order     With lcOrderNo   ,;
          CTERMCODE With m.CTERMCODE ,;
          SPCINST   With m.SPCINST   ,;
          SHIPVIA   With m.SHIPVIA   ,;
          LUPSINS   With m.LUPSINS   ,;
          INVDATE   With m.INVDATE   ,;
          SHIPDATE  With m.SHIPDATE  ,;
          DPOSTDATE With m.DPOSTDATE ,;
          UPSZONE   With m.UPSZONE   ,;
          PHONE     With m.PHONE     ,;
          TAX_RATE  With m.TAX_RATE  ,;
          NPSTRATE  With m.NPSTRATE  ,;
          NHSTRATE  With 0,;
          CTAXRULE  With m.CTAXRULE  ,;
          Status    With m.Status    ,;
          NOTE1     With m.NOTE1     ,;
          NOTE2     With m.NOTE2     ,;
          REP1      With m.REP1      ,;
          Comm1     With 0           ,;
          REP2      With m.REP2      ,;
          Comm2     With 0           ,;
          DEPT      With m.DEPT      ,;
          CFACCODE  With m.CFACCODE  ,;
          CWARECODE With m.CWARECODE ,;
          DUEDATE   With m.DUEDATE   ,;
          TRDE_DISC With m.TRDE_DISC ,;
          APPROVAL  With m.APPROVAL  ,;
          SEASON    With m.SEASON    ,;
          CUSTPO    With m.CUSTPO    ,;
          COD_FLAG  With m.COD_FLAG  ,;
          LCOMPUPS  With .T.         ,;
          DADD_DATE With oAriaApplication.SystemDate   ,;
          CADD_TIME With Time()      ,;
          CADD_USER With oAriaApplication.USER_ID
        Replace DIST_CTR With Iif(llConsDist,m.DIST_CTR,"")
        If llConsStore
          Replace cConStore With  m.Store
        Endif
      Else  && if the CustPo Changed then blank the CustPo (MultiPo).
        Replace CUSTPO With Iif(m.CUSTPO <> CUSTPO,'',m.CUSTPO)
      Endif
      Replace appramt   With Iif(appramt>0 .And. Evaluate(lcConsInvH+'.consol')='Y',appramt,appramt+m.appramt) ,;
        SEASON    With Iif(SEASON=m.SEASON,SEASON,'*') ,;
        SHIP      With SHIP    + m.SHIP    ,;
        ORDERED   With ORDERED + m.ORDERED ,;
        SHIPAMT   With SHIPAMT + m.SHIPAMT ,;
        PICKED    With PICKED  + m.PICKED  ,;
        DISCOUNT  With DISCOUNT - m.SHIPAMT*m.DISCPCNT/100,;
        DISCPCNT  With Iif(SHIPAMT=0,0,Abs(DISCOUNT)*100/SHIPAMT)  ,;
        WEIGHT    With WEIGHT  + m.WEIGHT  ,;
        CARTONS   With CARTONS + m.CARTONS ,;
        NMERCHTAX With NMERCHTAX + m.NMERCHTAX ,;
        COD_AMT   With COD_AMT  + m.COD_AMT  ,;
        NTAXDUE   With NTAXDUE  + m.NTAXDUE

      If llUseTradeDisc
        Replace TRDDSCAMNT With TRDDSCAMNT+m.TRDDSCAMNT,;
                TRDE_DISC  With  Iif((SHIPAMT+DISCOUNT)<>0,(TRDDSCAMNT/(SHIPAMT+DISCOUNT))*100,TRDE_DISC)
        m.TRDE_DISC = TRDE_DISC
      Endif

      lnOrdTax   = m.NMERCHTAX*(100-m.DISCPCNT)/100*(100-m.TRDE_DISC)/100
      lnTax_Amt  = lnTax_Amt  + lnOrdTax
      lnTOTALCHG = lnTOTALCHG + m.SHIPAMT+lnOrdTax+m.DISCOUNT

      Select (lcInvLine)
      =Seek(m.ACCOUNT+m.Order+m.Store)
      Scan Rest While ACCOUNT+Order+Store+PIKTKT+Str(Lineno,6) = ;
                M.ACCOUNT+m.Order+m.Store
        Scatter Memvar
        Select (lcConsInvD)
        If !Seek('Y'+m.ACCOUNT+m.CDIVISION+m.cCurrCode+m.CWARECODE+m.Style+m.Dyelot)
          Append Blank
          Replace Consol    With 'Y'         ,;
            ACCOUNT   With m.ACCOUNT   ,;
            CDIVISION With m.CDIVISION ,;
            cCurrCode With m.cCurrCode ,;
            CWARECODE With m.CWARECODE ,;
            Style     With m.Style     ,;
            ALTSTYLE  With m.ALTSTYLE  ,;
            Order     With lcOrderNo   ,;
            Dyelot    With m.Dyelot    ,;
            SEASON    With m.SEASON    ,;
            Scale     With m.Scale     ,;
            LTAXABLE  With m.LTAXABLE  ,;
            DIST_CTR  With Iif(llConsDist,m.DIST_CTR,"")
          Replace DESC1 With m.DESC1
          Replace OLDDYELOT With m.Dyelot
        Endif
        Replace BOOK1   With BOOK1   + m.BOOK1 ,;
          BOOK2   With BOOK2   + m.BOOK2 ,;
          BOOK3   With BOOK3   + m.BOOK3 ,;
          BOOK4   With BOOK4   + m.BOOK4 ,;
          BOOK5   With BOOK5   + m.BOOK5 ,;
          BOOK6   With BOOK6   + m.BOOK6 ,;
          BOOK7   With BOOK7   + m.BOOK7 ,;
          BOOK8   With BOOK8   + m.BOOK8 ,;
          TOTBOOK With TOTBOOK + m.TOTBOOK ,;
          PIK1   With PIK1   + m.PIK1 ,;
          PIK2   With PIK2   + m.PIK2 ,;
          PIK3   With PIK3   + m.PIK3 ,;
          PIK4   With PIK4   + m.PIK4 ,;
          PIK5   With PIK5   + m.PIK5 ,;
          PIK6   With PIK6   + m.PIK6 ,;
          PIK7   With PIK7   + m.PIK7 ,;
          PIK8   With PIK8   + m.PIK8 ,;
          TOTPIK With TOTPIK + m.TOTPIK ,;
          QTY1   With QTY1   + m.QTY1 ,;
          QTY2   With QTY2   + m.QTY2 ,;
          QTY3   With QTY3   + m.QTY3 ,;
          QTY4   With QTY4   + m.QTY4 ,;
          QTY5   With QTY5   + m.QTY5 ,;
          QTY6   With QTY6   + m.QTY6 ,;
          QTY7   With QTY7   + m.QTY7 ,;
          QTY8   With QTY8   + m.QTY8 ,;
          TOTQTY With TOTQTY + m.TOTQTY,;
          nNetAmnt   With nNetAmnt +m.TOTQTY*m.PRICE ,;
          nGrosAmnt  With nGrosAmnt+m.TOTQTY*m.GROS_PRICE ,;
          PRICE      With Iif(TOTQTY=0,0,nNetAmnt/TOTQTY)  ,;
          GROS_PRICE With Iif(TOTQTY=0,0,nGrosAmnt/TOTQTY) ,;
          DISC_PCNT  With Iif(nGrosAmnt=0,0,(nGrosAmnt-nNetAmnt)*100/nGrosAmnt)
        *-- Update Packs fields in Temp consalidated invoice line
        lnComAmt1 = ((nNetAmnt - m.TOTQTY*m.PRICE) * Comm1 /100)+(m.TOTQTY*m.PRICE* m.Comm1 / 100)
        lnComAmt2 = ((nNetAmnt - m.TOTQTY*m.PRICE) * Comm2 /100)+(m.TOTQTY*m.PRICE* m.Comm2 / 100)
        Replace Comm1 With Iif(nNetAmnt <> 0,lnComAmt1 /nNetAmnt *100,0),;
          Comm2 With Iif(nNetAmnt <> 0,lnComAmt2 /nNetAmnt *100,0)
        If llUseTradeDisc
          Replace nTrdNetAmt With nTrdNetAmt +m.TOTQTY*m.TRD_PRICE ,;
            TRD_PRICE  With Iif(TOTQTY=0,0,nTrdNetAmt/TOTQTY)  ,;
            TRDE_DISC  With Iif(nGrosAmnt=0,0,(nGrosAmnt-nTrdNetAmt)*100/nGrosAmnt)
        Endif
        lcDetFile = lcConsInvD
      Endscan
    Endscan
    Local lnSlct
    lnSlct = Select(0)
    Select (lcConsInvH)
    Replace TAX_AMT   With lnTax_Amt  ;
      TOTALCHG  With lnTOTALCHG
    Select (lnSlct)
  Enddo

  Select (lcConsInvH)
  Scan
    *-- Store consolidated invoice line#
    llConsDist = gfSeek('M'+ACCOUNT,'Customer') And CUSTOMER.ConByDc $ 'SY'
    =Seek(Consol+ACCOUNT+CDIVISION+cCurrCode,lcConsInvD)
    Select (lcConsInvD)
    lnLineNo = 0
    Scan For Consol+ACCOUNT+CDIVISION+cCurrCode+CWARECODE+Style+Dyelot+DIST_CTR = ;
        &lcConsInvH..Consol+&lcConsInvH..ACCOUNT+&lcConsInvH..CDIVISION+&lcConsInvH..cCurrCode ;
        AND DIST_CTR = &lcConsInvH..DIST_CTR
      Scatter Memvar
      lnLineNo = lnLineNo + 1
      m.LineNo = lnLineNo
      m.OLDDYELOT = m.Dyelot
      Insert Into (lcInvLine) From Memvar
    Endscan
    Select (lcConsInvH)
    Scatter Memvar
    m.LASTLINE = lnLineNo
    Select (lcInvHdr)
    =Seek('N'+m.ACCOUNT+m.CDIVISION+m.cCurrCode+Iif(llConsDist,m.DIST_CTR,""))
    If llUseTradeDisc
      Replace Rest Flag      With Iif(llConsDist,'C',Iif(Empty(m.Order),'A','O')) ,;
        CTERMCODE With m.CTERMCODE ,;
        COD_FLAG  With m.COD_FLAG  ,;
        DUEDATE   With m.DUEDATE   ,;
        SHIPDATE  With m.SHIPDATE  ;
        WHILE Consol+ACCOUNT+CDIVISION+cCurrCode+DIST_CTR=;
        'N'+m.ACCOUNT+m.CDIVISION+m.cCurrCode+Iif(llConsDist,m.DIST_CTR,"")
    Else
      Replace Rest Flag      With Iif(llConsDist,'C',Iif(Empty(m.Order),'A','O')) ,;
        CTERMCODE With m.CTERMCODE ,;
        TRDE_DISC With m.TRDE_DISC ,;
        COD_FLAG  With m.COD_FLAG  ,;
        DUEDATE   With m.DUEDATE   ,;
        SHIPDATE  With m.SHIPDATE  ;
        WHILE Consol+ACCOUNT+CDIVISION+cCurrCode+DIST_CTR=;
        'N'+m.ACCOUNT+m.CDIVISION+m.cCurrCode+Iif(llConsDist,m.DIST_CTR,"")
    Endif
    m.NOLDCARTON = m.CARTONS
    m.NOLDWIGHT  = m.WEIGHT
    Insert Into (lcInvHdr) From Memvar
  Endscan
  Select (lcInvHdr)
  If llconsbydc
    Set Order To Tag ConsDist
  Else
    Set Order To Tag (lcInvHdr)
  Endif
  Go Top
  loFormSet = Createobject('Custom')
  loFormSet.AddProperty ('laEvntTrig[1]','')
  Declare laInv[1]
  Store '' To laInv
  lcGlSession  = gfSequence('GLSESSION')
  Do gpSaveInv In (oAriaApplication.applicationhome+"AR"+'\ARINV.FXP') With ;
    lcInvHdr,lcInvLine,'','','',;
    '','','',;
    lcGlSession  ,'laInv',.F.,.F.,loFormSet
 
  If !Empty(Alltrim(laInv[1]))
    FOR lnA = 1 TO ALEN(laInv,1)
      =gfSeek(laInv[lnA],'INVHDR','INVHDR')
      IF INVHDR.CONSOL ='Y'
        =gfSeek(laInv[lnA],'CONSINVH','CONSINVH')
        SELECT CONSINVH
      ELSE 
        SELECT INVHDR 
      ENDIF
      lchdrFile =  IIF(INVHDR.CONSOL ='Y','CONSINVH','INVHDR')
      SCAN REST WHILE IIF(INVHDR.CONSOL ='Y',INVOICE+STORE+ORDER+PIKTKT,INVOICE) = laInv[lnA]
         SELECT SMRTSHPLIN
         SET ORDER TO SMARTLINE
         =SEEK(&lchdrFile..Account+&lchdrFile..STORE+&lchdrFile..ORDER)
         SCAN REST WHILE Account+Store+ORder = &lchdrFile..Account+&lchdrFile..STORE+&lchdrFile..ORDER
           lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_HEADER Set INVOICE ='"+laInv[lnA]+;
                    "', Status ='C' Where SMART_INVOICE_NO = '"+SMRTSHPLIN.SMART_INVOICE_NO +"' AND  Account ='"+&lchdrFile..Account+"' And STORE ='"+&lchdrFile..Store+"' AND  Invoice ='' AND Status !='C'",'SMRTSHPHDR' , ;
                    "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
 	      If lnResult<>1
	         oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
	         loop
	       ELSE
	         lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_LINES Set cTrCode ='"+laInv[lnA]+;
	                  "' Where SMART_INVOICE_NO = '"+SMRTSHPLIN.SMART_INVOICE_NO+"' and [ORDER]='"+SMRTSHPLIN.ORDER+"' AND [Order]<>''  AND (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S')",'SMRTSHPLINUP' , ;
	                 "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
	         If lnResult<>1
	           oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
 	          loop
	        Endif
	      ENDIF
        ENDSCAN 
      ENDSCAN
*!*	    SELECT 'RemitsInv'
*!*	    LOCATE
*!*	    SCAN
*!*	      lcINVSMARTINV = allt(RemitsInv.crdrno)
*!*	      lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_HEADER Set INVOICE ='"+laInv[1]+;
*!*	      "', Status ='C' Where SMART_INVOICE_NO = '"+lcINVSMARTINV+"' AND  Invoice ='' AND Status !='C'",'SMRTSHPHDR' , ;
*!*	      "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
*!*	      If lnResult<>1
*!*	        oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
*!*	        Wait Window 'SMART_INVOICE_HEADER'+'.SqlRun()'
*!*	        loop
*!*	      ELSE
*!*	       lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_LINES Set cTrCode ='"+laInv[1]+;
*!*	                  "' Where SMART_INVOICE_NO = '"+lcINVSMARTINV+"' and [Order]<>''  AND (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S')",'SMRTSHPLINUP' , ;
*!*	                 "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
*!*	        If lnResult<>1
*!*	          oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
*!*	          Wait Window 'SMART_INVOICE_LINES'+'.SqlRun()'
*!*	          loop
*!*	        Endif
*!*	      Endif
*!*	    ENDSCAN
    ENDFOR 
    IF ALEN(laInv,1) >= 1
      FOR lnd = 1 TO ALEN(laInv,1) 
        =gfSeek(laInv[lnd],'INVHDR','INVHDR')
        IF INVHDR.CONSOL ='Y'
          =gfSeek(laInv[lnd],'CONSINVH','CONSINVH')
          SELECT CONSINVH
          SCAN REST WHILE INVOICE+STORE+ORDER+PIKTKT = laInv[lnd]
            INSERT INTO  'CRDIRECT' VALUES (laInv[lnd],0,'I', CONSINVH.ORDER, '',CONSINVH.Account,CONSINVH.Cdivision)     
          ENDSCAN 
        ELSE
          INSERT INTO  'CRDIRECT' VALUES (laInv[lnd],0,'I', INVHDR.ORDER, '',INVHDR.Account,INVHDR.Cdivision)
        ENDIF  
      ENDFOR
    ENDIF
    if used(lcInvHdr)
      USE IN (lcInvHdr)
    ENDIF
    if used(lcConsInvH)
      USE IN (lcConsInvH)
    ENDIF
    if USED(lcConsInvD)
      USE IN (lcConsInvD)
    ENDIF
    if USED(lcInvLine)
      USE IN (lcInvLine)
    ENDIF 
    Return laInv[1]
  Else
    Return .F.
  ENDIF
  
*!*************************************************************
*! Name      : lfCreateCreditMemo
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/15/2018
*! Purpose   : Function to create Credit memo
*!*************************************************************  
FUNCTION lfCreateCreditMemo
 
Select SMRTSHPLIN
LOCATE FOR totqty < 0 
IF FOUND()
  SELECT SMRTSHPLIN
  SELECT Distinct Account,Store,CDIVISION FROM SMRTSHPLIN WHERE totqty < 0 INTO CURSOR 'CrStore'
  loCrMem =''
  IF TYPE('loCrMem') <> "O"
    DO FORM (oAriaApplication.ScreenHome+"RMCRMEM.scx") NOSHOW NAME loCrMem LINKED
  ENDIF 
  lnCntCr = 0
  SELECT 'CrStore'
  LOCATE 
  SCAN 
    lnCntCr = lnCntCr + 1
    Wait window 'Creating Credit Memo...'+Allt(STR((lnCntCr /Reccount('CrStore'))*100,3))+"%" nowait 
    lnChoice = 1  
    loCrMem.ChangeMode ('A')
    loCrMem.AriaForm1.pgfReCrmem.pgheader.cntHeader.DtCrdDate.Text1.VALUE  = oAriaApplication.SystemDate
    loCrMem.AriaForm1.DtPostDate.Text1.VALUE = oAriaApplication.SystemDate
    loCrMem.AriaForm1.KBAccount.keyTextbox.VALUE = CrStore.Account
    loCrMem.AriaForm1.KBAccount.keyTextbox.oldValue = SPACE(5)
    loCrMem.AriaForm1.KBAccount.keyTextbox.VALID
    loCrMem.ariaform1.KBSTORE.keytextbox.oldValue = SPACE(5)
    loCrMem.ariaform1.KBSTORE.keytextbox.VALUE = CrStore.Store
    loCrMem.ariaform1.KBSTORE.keytextbox.VALID
    loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboDivision.VALUE =  CrStore.CDIVISION
    loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.VALUE =  loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.CodeDefaultValue
    loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboLocation.VALUE =  SUBSTR(CrStore.Store,1,6)
    loCrMem.ariaForm1.rmCRMBUS.mUpdHdrFl(loCrMem.ariaForm1.rmCRMBUS.lcCrMemHdr)
    loCrMem.ariaForm1.pgfReCrmem.pgHeader.cntHeader.txtpONo.Value= ''
    loCrMem.ariaForm1.pgfReCrmem.pgHeader.cntHeader.txtpONo.Valid
    =SEEK('S'+CrStore.Account+CrStore.Store,'CUSTOMER','CUSTOMER')
     lnDisc = Customer.Disc
     lnTotAmt = 0
     SELECT SMRTSHPLIN
     SCAN FOR Account = CrStore.Account AND Store = CrStore.Store AND  CDIVISION = CrStore.CDIVISION AND TotQty < 0 &&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') 
       lnTotAmt = lnTotAmt + (ABS(SMRTSHPLIN.TotQty) * SMRTSHPLIN.Price)
       lnRetPriceValue = SMRTSHPLIN.PRICE
       loCrMem.ariaForm1.rmCRMBUS.llAddLine = .T.
       WITH loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail
         .cboReason.VALUE = loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.VALUE
         .cboReason.REQUERY()
         .cboQuality.VALUE = loCrMem.laStyGrade[1,2]
         .cboQuality.REQUERY()
         STORE " " TO .KbStyle.VALUE , .txtStyDesc.VALUE , loCrMem.ariaForm1.rmCRMBUS.lcCurLine ,;
         loCrMem.ariaForm1.rmCRMBUS.lcTranCd , .KbStyRetTo.VALUE ,;
         loCrMem.ariaForm1.rmCRMBUS.lcScale  , .kbConfig.KeyTExtBOx.VALUE , .kbDyelot.KeyTExtBOx.VALUE
         STORE 0   TO .txtTotAlQty.VALUE , .txtPrice.VALUE , .txtTotAmount.VALUE , .txtGrsPrice.VALUE , .txtDiscount.VALUE ,;
                      .txtHstRat.VALUE , .txtPstRat.VALUE ,;
         loCrMem.ariaForm1.rmCRMBUS.lnScaleCnt , loCrMem.ariaForm1.rmCRMBUS.lnCost , loCrMem.ariaForm1.rmCRMBUS.lnDisc_Amt , loCrMem.ariaForm1.rmCRMBUS.lnTrde_Amt ,;
         loCrMem.ariaForm1.rmCRMBUS.lnPstTotal , loCrMem.ariaForm1.rmCRMBUS.lnDiscPcnt
         STORE '' TO .cboEmpl.VALUE
         STORE 0 TO .txtCost.VALUE
         .SbrkBackToStk.SCALE = ""
         FOR lnI = 1 TO 8
           lcI = STR(lnI,1)
           .SbrkBackToStk.txtQty&lcI..VALUE = 0
           .SbrkBackToStk.txtsizelbl&lcI..VALUE = ""
         ENDFOR
         .SbrkBackToStk.txtTotQty.VALUE =  0
         .SbrkBackToStk.txtTotQty.ENABLED = .F.
       ENDWITH
       loCrMem.AriaForm1.llLinStat  = .T.
       STORE .F. TO loCrMem.AriaForm1.llRetStat  ,loCrMem.AriaForm1.llDyeStat  , loCrMem.AriaForm1.llsizestat
       loCrMem.AriaForm1.mShowObj()
       loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALUE = " "
       loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.TxtItem.VALUE =SMRTSHPLIN.STYLE
       loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALID(.T.,0,SMRTSHPLIN.STYLE,SPACE(19),SMRTSHPLIN.STYLE)
       lnCstValue = SMRTSHPLIN.Price
       loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboReason.VALUE = loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboReason.CodeDefaultValue
       loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboReason.VALID()
       loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.VALUE= loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.CodeDefaultValue
       loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.OldValue = 0
       loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.VALUE = SMRTSHPLIN.PRICE
       loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.VALID
       IF TYPE('loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.OldValue') <> 'N'
         loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.OldValue = 0
       ENDIF
       FOR lnX= 1 TO 8
         lcX = STR(lnX,1)
         IF SMRTSHPLIN.Qty&lcX. < 0
           loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcX..OldValue  = 0
           loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcX..VALUE  = ABS(SMRTSHPLIN.Qty&lcX.)
           loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcX..VALID
         ENDIF
       ENDFOR
     ENDSCAN
     lnChoice = 2
     lcCrMemo = ''
     loCrMem.SaveFiles(.F.)
     IF !EMPTY(lcCrMemo) 
        INSERT INTO 'CRDIRECT' Values(lcCrMemo,lnTotAmt,'C',SPACE(6),CrStore.Store,CrStore.Account,CrStore.cDivision)
        Select SMRTSHPLIN
        LOCATE
        SCAN FOR Store = CrStore.Store AND Account = CrStore.Account AND totqty < 0 AND EMPTY(Order) AND CDIVISION = CrStore.CDIVISION &&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv')
           lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_LINES Set cTrCode ='"+lcCrMemo+;
                 "'  Where SMART_INVOICE_NO = '"+SMRTSHPLIN.SMART_INVOICE_NO +"' AND STYLE ='"+SMRTSHPLIN.STYLE+"' AND  [Order]='' AND TotQty < 0  AND (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S')",'SMRTSHPLINUP' , ;
                 "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
          If lnResult<>1
             oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
            Return .F.
          ELSE
             lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_HEADER Set INVOICE ='"+lcCrMemo+;
             "', Status ='C' Where SMART_INVOICE_NO = '"+SMRTSHPLIN.SMART_INVOICE_NO +"' AND  Account ='"+SMRTSHPLIN.Account+"' And STORE ='"+SMRTSHPLIN.Store+"' AND  Invoice ='' AND Status !='C'",'SMRTSHPHDR' , ;
             "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
  	       If lnResult<>1
               oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
             RETURN .F.
           ENDIF
         ENDIF  
       ENDSCAN 
    ENDIF 
  ENDSCAN 
  IF TYPE('loCrMem') = 'O'
    loCrMem.Release
    loCrMem= Null
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfSaveInvRet 
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/15/2018
*! Purpose   : Screen Saving Function 
*!*************************************************************  
FUNCTION lfSaveInvRet 
LPARAMETERS loFormSet


CREATE CURSOR 'CRDIRECT' (CTRCODE C(6),Amount N(14,2),Type C(1),Order C(6),Store C(8),Account C(5),CDIVISION C(6))
lfCreateConsLines()
SELECT 'CRDIRECT'
LOCATE 
IF !EOF()
 SCAN 
   IF Type ='I'
     =gfSeek(CRDIRECT.CtrCode,'INVHDR','INVHDR')
     IF SEEK(SPACE(6)+CRDIRECT.Account+CRDIRECT.CDIVISION+IIF(!INVHDR.Direct_inv,INVHDR.cCurrCode+'S'+IIF(INVHDR.CONSOL='Y',INVHDR.DIST_CTR,SPACE(8)),SPACE(3)+'D'+SPACE(8)),'INV_STORE')&&INVOICE+Account+CDIVISION+cCurrCode+DIST_CTR+Type
*!*	       SELECT 'INV_STORE'
*!*	       LOCATE REST WHILE Invoice+ Account+CDIVISION+cCurrCode+DIST_CTR =SPACE(6)+CRDIRECT.Account+CRDIRECT.CDIVISION &?&FOR Order = CRDIRECT.Order 
*!*	       IF FOUND()
         REPLACE Invoice WITH CRDIRECT.CtrCode IN 'INV_STORE'
*!*	       ENDIF
     ENDIF
   ELSE
     IF SEEK(SPACE(6)+CRDIRECT.Account+CRDIRECT.Store ,'Acc_Store')   
        REPLACE CRMEMO WITH CRDIRECT.CtrCode IN 'Acc_Store'
     ENDIF
   ENDIF
 ENDSCAN
ENDIF
*!*************************************************************
*! Name      : lfCreateDirectInvoice
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/15/2018
*! Purpose   : Function to create Direct Invoice
*!*************************************************************  
FUNCTION lfCreateDirectInvoice

 SELECT Distinct Account,Store,CDIVISION  FROM SMRTSHPLIN WHERE totqty > 0 AND EMPTY(Order) AND !EMPTY(Account) INTO CURSOR 'INVStore'
 loDInv =''
 IF TYPE('loDInv') <> "O"
   DO FORM (oAriaApplication.ScreenHome+"ar\ardinv.scx") NOSHOW NAME loDInv LINKED
 ENDIF
 ldPaymntDate = oAriaApplication.SYSTEMDATE
 lnCountLine = 0
  
 SELECT 'INVStore'
 SCAN 
   lnLinNumS = RECNO('INVStore')
   lnCountLine = lnCountLine + 1
   Wait window 'Creating Direct Invoice...'+Allt(STR((lnCountLine /Reccount('INVStore'))*100,3))+"%" nowait 
   lnChoice = 1  
   loDInv.mCreateTempfiles
   loDInv.changemode ('A')
   loDInv.DefaultInvoiceDate = ldPaymntDate 
   loDInv.DefaultPostingDate = ldPaymntDate 
   loDInv.laSetups[18,2] = 'N'
   loDInv.mCreateTempfiles
   IF BETWEEN(lnLinNumS ,1,RECCOUNT('INVStore'))
     GO RECORD lnLinNumS IN 'INVStore'
   ENDIF
   loDInv.DefaultWarecode = SUBSTR(INVStore.Store,1,6)
   WITH loDInv.AriaForm1.AriaPageframe1.Page2.InvoiceEditRegion1
     STORE 0 TO .TaxDueAmount, .Merchandisetax, .TotalCartons
   ENDWITH
   loDInv.DefaultSeason =  '*'
   loDInv.DefaultDivision = INVStore.CDIVISION &&loDInv.AriaForm1.AriaPageFrame1.Page1.cboDivision.CodeDefaultValue
   loDInv.ariaform1.keyAccount.keytextbox.oldValue = SPACE(5)
   loDInv.ariaform1.keyAccount.keytextbox.VALUE = INVStore.Account
   loDInv.ariaform1.keyAccount.keytextbox.VALID
   loDInv.ariaform1.keySTORE.keytextbox.oldValue = SPACE(5)
   loDInv.ariaform1.keySTORE.keytextbox.VALUE = INVStore.Store
   loDInv.ariaform1.keySTORE.keytextbox.VALID
   IF !EMPTY(INVStore.Store)
     =SEEK('S'+INVStore.Account+INVStore.Store,'CUSTOMER','CUSTOMER')
   ELSE
     =SEEK('M'+INVStore.Account,'CUSTOMER','CUSTOMER')
   ENDIF  
   lnDisc = Customer.Disc
   Select SMRTSHPLIN
   lnTotAmt = 0
   SCAN FOR Account = INVStore.Account AND Store = INVStore.Store AND totqty > 0 AND EMPTY(Order) AND CDIVISION = INVStore.CDIVISION &&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv')
     lnTotAmt = lnTotAmt + SMRTSHPLIN.PRICE * SMRTSHPLIN.TotQty
     lnRetPriceValue = SMRTSHPLIN.PRICE
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.mResetControlSource ()
     STORE .T. TO loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.llNewline,loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.llAddLine
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.ENABLED = .T.
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.TxtItem.VALUE = SMRTSHPLIN.STYLE
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.VALID(.T.,0,SMRTSHPLIN.STYLE,SPACE(19),SMRTSHPLIN.STYLE)
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.mResetControlSource ()

     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.OldValue  = 0
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.VALUE  = SMRTSHPLIN.PRICE
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.LOSTFOCUS()
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.oldValue = 0
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.VALUE = 0
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.LOSTFOCUS()
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty1.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty2.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty3.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty4.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty5.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty6.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty7.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty8.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtTotQty.CONTROLSOURCE = ''
     FOR lnX= 1 TO 8
       lcX = STR(lnX,1)
       IF SMRTSHPLIN.Qty&lcX. > 0
         loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty&lcX..VALUE  =  SMRTSHPLIN.Qty&lcX.
         loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty&lcX..VALID
       ENDIF
     ENDFOR
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.LOSTFOCUS()
      =gfAdd_Info(loDInv.lcInvLine,loDInv)
   ENDSCAN
   loDInv.AriaForm1.AriaPageFrame1.Page3.ACTIVATE()
   lnChoice = 2
   DIMENSION  laInv[1]
   laInv[1] = ''  
   IF lnTotAmt <> 0
     loDInv.HIDE()
     loDInv.SaveFiles(.F.)
   ENDIF  

   IF !EMPTY(laInv[1])
     IF !USED('INVHDR')
       =gfOpenTable('INVHDR','INVHDR')
     ENDIF
     =gfSeek(laInv[1],'INVHDR','INVHDR')
     INSERT INTO  'CRDIRECT' VALUES (laInv[1],lnTotAmt,'I', '', INVHDR.Store,INVHDR.Account,INVHDR.cDivision)
     Select SMRTSHPLIN
     LOCATE
     SCAN FOR Account =INVHDR.Account AND Store = INVHDR.Store AND totqty > 0 AND EMPTY(Order) AND CDIVISION = INVHDR.CDIVISION &&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv')
        lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_LINES Set cTrCode ='"+laInv[1]+;
                 "'  Where SMART_INVOICE_NO = '"+SMRTSHPLIN.SMART_INVOICE_NO +"' AND STYLE ='"+SMRTSHPLIN.STYLE+"' AND  [Order]='' AND TotQty > 0  AND (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S')",'SMRTSHPLINUP' , ;
                 "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
        If lnResult<>1
          oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
          Return .F.
        ELSE
          lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_HEADER Set INVOICE ='"+laInv[1]+;
          "', Status ='C' Where SMART_INVOICE_NO = '"+SMRTSHPLIN.SMART_INVOICE_NO +"' AND  Account ='"+SMRTSHPLIN.Account+"' And STORE ='"+SMRTSHPLIN.Store+"' AND  Invoice ='' AND Status !='C'",'SMRTSHPHDR' , ;
          "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
          IF lnResult<>1
            oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
          RETURN .F.
        ENDIF
      Endif  
     ENDSCAN 
   ENDIF  
 ENDSCAN 
 IF TYPE('loDInv') ='O'
   loDInv.Release()
   loDInv = Null
 ENDIF 
*!*************************************************************
*! Name      : lfAddStore
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/15/2018
*! Purpose   : Function to Add missing stores
*!*************************************************************   
FUNCTION  lfAddStore
PARAMETERS lcAccountID ,lcSTORENo
lnCurAls = SELECT()
IF gfSEEK('M'+lcAccountID ,'Customer' ,'Customer')
  SELECT Customer
  SCATTER MEMO MEMVAR 
  m.TYPE ='S'
  m.Store = lcSTORENo
  m.StName = lcSTORENo
  m.BillTo = 'M'
  m.Btname = lcSTORENo
  INSERT INTO 'Customer' FROM MEMVAR
  SELECT Customer
  =gfAdd_Info('Customer')       
  =gfreplace('')
ENDIF
SELECT Customer
=gftableUpdate()
SELECT(lnCurAls)
*!*************************************************************
*! Name      : lfCreateInvTemp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/15/2018
*! Purpose   : Function to create invoice temp. files
*!************************************************************* 
Function lfCreateInvTemp
  IF !USED("INVHDR")
    =gfOpenTable("INVHDR",'INVHDR')
  ENDIF
  IF !USED("INVLINE")
    =gfOpenTable("INVLINE",'INVLINE')
  ENDIF  
  Select InvHdr
  =Afields(laFileStru)
  lnFileStru = Alen(laFileStru,1)
  Dimension laFileStru[lnFileStru+14,18]
  laFileStru[lnFileStru+1,1] = 'cSelect'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'Picked'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 7
  laFileStru[lnFileStru+2,4] = 0
  laFileStru[lnFileStru+3,1] = 'lUpsIns'
  laFileStru[lnFileStru+3,2] = 'L'
  laFileStru[lnFileStru+3,3] = 1
  laFileStru[lnFileStru+3,4] = 0
  laFileStru[lnFileStru+4,1] = 'nChrgTax'
  laFileStru[lnFileStru+4,2] = 'N'
  laFileStru[lnFileStru+4,3] = 13
  laFileStru[lnFileStru+4,4] = 2
  laFileStru[lnFileStru+5,1] = 'nMerchTax'
  laFileStru[lnFileStru+5,2] = 'N'
  laFileStru[lnFileStru+5,3] = 17
  laFileStru[lnFileStru+5,4] = 8
  laFileStru[lnFileStru+6,1] = 'lCompUps'
  laFileStru[lnFileStru+6,2] = 'L'
  laFileStru[lnFileStru+6,3] = 1
  laFileStru[lnFileStru+6,4] = 0
  laFileStru[lnFileStru+7,1] = 'LastLine'
  laFileStru[lnFileStru+7,2] = 'N'
  laFileStru[lnFileStru+7,3] = 6
  laFileStru[lnFileStru+7,4] = 0
  laFileStru[lnFileStru+8,1] = 'LKEYOFF'
  laFileStru[lnFileStru+8,2] = 'L'
  laFileStru[lnFileStru+8,3] = 0
  laFileStru[lnFileStru+8,4] = 0
  laFileStru[lnFileStru+9,1] = 'NTAXDUE'
  laFileStru[lnFileStru+9,2] = 'N'
  laFileStru[lnFileStru+9,3] = 17
  laFileStru[lnFileStru+9,4] = 6
  laFileStru[lnFileStru+10,1] = 'NCARTONS'
  laFileStru[lnFileStru+10,2] = 'N'
  laFileStru[lnFileStru+10,3] = 11
  laFileStru[lnFileStru+10,4] = 5
  laFileStru[lnFileStru+11,1] = 'Ordered'
  laFileStru[lnFileStru+11,2] = 'N'
  laFileStru[lnFileStru+11,3] = 7
  laFileStru[lnFileStru+11,4] = 0
  laFileStru[lnFileStru+12,1] = 'cConStore'
  laFileStru[lnFileStru+12,2] = 'C'
  laFileStru[lnFileStru+12,3] = 8
  laFileStru[lnFileStru+12,4] = 0
  laFileStru[lnFileStru+13,1] = 'nOldCarton'
  laFileStru[lnFileStru+13,2] = 'N'
  laFileStru[lnFileStru+13,3] = 5
  laFileStru[lnFileStru+13,4] = 0
  laFileStru[lnFileStru+14,1] = 'nOldWight'
  laFileStru[lnFileStru+14,2] = 'N'
  laFileStru[lnFileStru+14,3] = 8
  laFileStru[lnFileStru+14,4] = 2
  lnNewArrLen = Alen(laFileStru,1)+1
  Dimension laFileStru[lnNewArrLen,ALEN(laFileStru,2)]
  laFileStru[lnNewArrLen,1] = 'cEdtCrtns'
  laFileStru[lnNewArrLen,2] = 'C'
  laFileStru[lnNewArrLen,3] = 1
  laFileStru[lnNewArrLen,4] = 0
  For lnCount = 1 To Alen(laFileStru,1)-lnFileStru
    Store '' To laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
      laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
      laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
      laFileStru[lnFileStru+lnCount,16]
    Store 0  To laFileStru[lnFileStru+lnCount,17],laFileStru[lnFileStru+lnCount,18]
  Endfor


  Declare laIndex[3,2]
  laIndex[1,1] = 'Account+Order+Store+PikTkt+cDivision'
  laIndex[1,2] = lcInvHdr
  laIndex[2,1] = 'Consol+Account+cDivision+cCurrCode+Dist_Ctr'
  laIndex[2,2] = 'Consol'
  laIndex[3,1] = 'Account+Dist_Ctr+Order+Store+PikTkt+cDivision'
  laIndex[3,2] = 'ConsDist'

  =gfCrtTmp(lcInvHdr,@laFileStru,@laIndex)
  =gfCrtTmp(lcConsInvH,@laFileStru,[Consol+Account+cDivision+cCurrCode+Dist_Ctr],lcConsInvH)
  IF !USED("OrdLine")
    =gfOpenTable("OrdLine",'OrdLine')
  ENDIF

  Select OrdLine
  =Afields(laFileStru)
  lnFileStru = Alen(laFileStru,1)
  If llUseTradeDisc
    Dimension laFileStru[lnFileStru+20,18]
  Else
    Dimension laFileStru[lnFileStru+17,18]
  Endif
  laFileStru[lnFileStru+1,1] = 'LNEWLINE'
  laFileStru[lnFileStru+1,2] = 'L'
  laFileStru[lnFileStru+1,3] = 0
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'LPACKED'
  laFileStru[lnFileStru+2,2] = 'L'
  laFileStru[lnFileStru+2,3] = 0
  laFileStru[lnFileStru+2,4] = 0
  laFileStru[lnFileStru+3,1] = 'LBACKORD'
  laFileStru[lnFileStru+3,2] = 'L'
  laFileStru[lnFileStru+3,3] = 0
  laFileStru[lnFileStru+3,4] = 0
  laFileStru[lnFileStru+4,1] = 'nTaxRate'
  laFileStru[lnFileStru+4,2] = 'N'
  laFileStru[lnFileStru+4,3] = 10
  laFileStru[lnFileStru+4,4] = 2
  laFileStru[lnFileStru+5,1] = 'cCurrCode'
  laFileStru[lnFileStru+5,2] = 'C'
  laFileStru[lnFileStru+5,3] = 3
  laFileStru[lnFileStru+5,4] = 0
  laFileStru[lnFileStru+6,1] = 'cDivision'
  laFileStru[lnFileStru+6,2] = 'C'
  laFileStru[lnFileStru+6,3] = 6
  laFileStru[lnFileStru+6,4] = 0
  laFileStru[lnFileStru+7,1] = 'Consol'
  laFileStru[lnFileStru+7,2] = 'C'
  laFileStru[lnFileStru+7,3] = 1
  laFileStru[lnFileStru+7,4] = 0
  laFileStru[lnFileStru+8,1] = 'nNetAmnt'
  laFileStru[lnFileStru+8,2] = 'N'
  laFileStru[lnFileStru+8,3] = 18
  laFileStru[lnFileStru+8,4] = 10
  laFileStru[lnFileStru+9,1] = 'nGrosAmnt'
  laFileStru[lnFileStru+9,2] = 'N'
  laFileStru[lnFileStru+9,3] = 18
  laFileStru[lnFileStru+9,4] = 10
  laFileStru[lnFileStru+10,1] = 'LTAXABLE'
  laFileStru[lnFileStru+10,2] = 'L'
  laFileStru[lnFileStru+10,3] = 0
  laFileStru[lnFileStru+10,4] = 0
  laFileStru[lnFileStru+11,1] = 'cDyeFlag'
  laFileStru[lnFileStru+11,2] = 'C'
  laFileStru[lnFileStru+11,3] = 1
  laFileStru[lnFileStru+11,4] = 0
  laFileStru[lnFileStru+12,1] = 'cConStore'
  laFileStru[lnFileStru+12,2] = 'C'
  laFileStru[lnFileStru+12,3] = 8
  laFileStru[lnFileStru+12,4] = 0
  laFileStru[lnFileStru+13,1] = 'lContract'
  laFileStru[lnFileStru+13,2] = 'L'
  laFileStru[lnFileStru+13,3] = 0
  laFileStru[lnFileStru+13,4] = 0
  laFileStru[lnFileStru+14,1] = 'Dist_Ctr'
  laFileStru[lnFileStru+14,2] = 'C'
  laFileStru[lnFileStru+14,3] = 8
  laFileStru[lnFileStru+14,4] = 0
  laFileStru[lnFileStru+15,1] = 'llUpdHdr'
  laFileStru[lnFileStru+15,2] = 'L'
  laFileStru[lnFileStru+15,3] = 1
  laFileStru[lnFileStru+15,4] = 0
  laFileStru[lnFileStru+16,1] = 'OldDyelot'
  laFileStru[lnFileStru+16,2] = 'C'
  laFileStru[lnFileStru+16,3] = 10
  laFileStru[lnFileStru+16,4] = 0
  laFileStru[lnFileStru+17,1] = 'Nordline'
  laFileStru[lnFileStru+17,2] = 'N'
  laFileStru[lnFileStru+17,3] = 6
  laFileStru[lnFileStru+17,4] = 0
  If llUseTradeDisc
    laFileStru[lnFileStru+18,1] = 'TRD_price'
    laFileStru[lnFileStru+18,2] = 'N'
    laFileStru[lnFileStru+18,3] =  12
    laFileStru[lnFileStru+18,4] = 2
    laFileStru[lnFileStru+19,1] = 'nTrdNetAmt'
    laFileStru[lnFileStru+19,2] = 'N'
    laFileStru[lnFileStru+19,3] =  18
    laFileStru[lnFileStru+19,4] = 10
    laFileStru[lnFileStru+20,1] = 'OTRDDISC'
    laFileStru[lnFileStru+20,2] = 'N'
    laFileStru[lnFileStru+20,3] =  5
    laFileStru[lnFileStru+20,4] = 2
  Endif
  For lnCount = 1 To Iif(llUseTradeDisc,20,17)
    Store '' To laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
      laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
      laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
      laFileStru[lnFileStru+lnCount,16]
    Store 0  To laFileStru[lnFileStru+lnCount,17],laFileStru[lnFileStru+lnCount,18]
  Endfor

  Declare laIndex[9,2]
  laIndex[1,1] = 'Account+Order+Store+PikTkt+STR(LineNo,6)'
  laIndex[1,2] = lcInvLine
  laIndex[2,1] = 'Account+Order+Store+PikTkt+Style+Dyelot'
  laIndex[2,2] = 'InvLines'
  laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode+Style+Dyelot+Dist_Ctr'
  laIndex[3,2] = 'Consol'
  laIndex[4,1] = 'Account+Order+Store+PikTkt+cDyeFlag+Dyelot'
  laIndex[4,2] = 'Dyelot'
  laIndex[5,1] = 'Account+Order+Store+PikTkt+Consol+cDivision+Dist_Ctr'
  laIndex[5,2] = 'ConsDiv'
  laIndex[6,1] = 'Account+Dist_Ctr+Order+Store+PikTkt+STR(LineNo,6)'
  laIndex[6,2] = 'ConsDist'
  laIndex[7,1] = 'Order+Store+STYLE+Dyelot+STR(LineNo,6)+Dist_Ctr+Consol'
  laIndex[7,2] = 'CONFIGLIN'
  laIndex[8,1] = 'Account+Order+Store+Consol+cDivision+Dist_Ctr'
  laIndex[8,2] = 'ConsDivFlt'
  laIndex[9,1] = 'Account+Order+Store+STYLE+STR(NORDLINE,6)'
  laIndex[9,2] = 'ConsORDLN'
  =gfCrtTmp(lcInvLine,@laFileStru,@laIndex)
  =gfCrtTmp(lcConsInvD,@laFileStru,[Consol+Account+cDivision+cCurrCode+cWareCode+Style+Dyelot+Dist_Ctr],lcConsInvD)
*!*************************************************************
*! Name      : gfModalGen 
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/15/2018
*! Purpose   : Function to stop displaying messages while creating invoices/Credit memos
*!*************************************************************   
FUNCTION  gfModalGen 
  LPARAMETER lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg
  IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
    oAriaApplication.oToolBar.Enabled = .F.
  ENDIF

  LOCAL oMessageBox, llActiveFormLocked, llFormExist
  oMessageBox = NEWOBJECT("AriaMessageBox",ADDBS(oAriaApplication.ClassDir)+"Utility.vcx")
  IF VARTYPE(oMessageBox) != "O"

    IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
      oAriaApplication.oToolBar.Enabled = .T.
    ENDIF

    RETURN 0
  ENDIF 
  
  *-- Get the dialog and buttons from the dictionary.
  IF !oMessageBox.GetMessage(lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg)
    IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
      oAriaApplication.oToolBar.Enabled = .T.
    ENDIF

    RETURN 0
  ENDIF 

  IF (TYPE("_SCREEN.ActiveForm.LockScreen") = "L") AND !EMPTY(TYPE("_SCREEN.ActiveForm.LockScreen")) AND TYPE('_SCREEN.ActiveForm') = 'O' AND !ISNULL(_SCREEN.ActiveForm)
    llFormExist = .T.
    llActiveFormLocked = _SCREEN.ActiveForm.LockScreen
    _SCREEN.ActiveForm.LockScreen = .F.
  ENDIF 

  PRIVATE lnMessageChoice
  IF TYPE('lnChoice') ='U'
    lnChoice = 1
  ENDIF
  lnMessageChoice = lnChoice
  RETURN lnMessageChoice  && Return the response.
ENDFUNC   
