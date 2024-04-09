*!**************************************************************************
*! Name      : PODGREP.prg
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/02/2015
*! Purpose   : Create Inter-Location PO from Picking Tickets
*! Entry     : C201743[T20151014.0017] 
*!**************************************************************************
*Modifications:
*B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017]
*!*****************************************************************************************
*! Name      : lfsrOrder
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/02/2015 
*! Purpose   : Reset/Set order browser
*!*****************************************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm
IF lcParm = "S"
  SELECT ORDHDR
  LOCATE
ENDIF
*!*************************************************************
*! Name      : lfOGWhen
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/02/2015 
*! Purpose   : When function for the Option Grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfOGWhen

IF TYPE('loPiktkt') <> 'O'
  loPiktkt = CreateObject("RemoteTable","Piktkt","Piktkt","Piktkt",SET("DATASESSION"))
ENDIF
IF TYPE('loOrdhdr') <> 'O'
  loOrdhdr= CreateObject("RemoteTable","Ordhdr","Ordhdr","Ordhdr",SET("DATASESSION"))
ENDIF
IF TYPE('loCustomer') <> 'O'
  loCustomer= CreateObject("RemoteTable","Customer","Customer","Customer",SET("DATASESSION"))
ENDIF

IF TYPE('loPOSHDR') <> 'O'
  loPOSHDR= CreateObject("RemoteTable","POSHDR","POSHDR","POSHDR",SET("DATASESSION"))
ENDIF
loPOSHDR.SqlRun("Select CPIKTKT,PO,STATUS,CWARECODE,Vendor,ENTERED from POSHDR WHERE CSTYTYPE ='N' and CBUSDOCU='N' AND CPIKTKT !='' and ISNULL(CPIKTKT,'C') != 'C'",lcTmpPoh)
IF USED(lcTmpPoh)
  SELECT(lcTmpPoh) 
  CURSORSETPROP("Buffering" ,3)
  IF loFormSet.lcDcOrStore ='S'
    INDEX on CPIKTKT+Vendor TAG 'PIKVENDOR' 
  ENDIF
  INDEX on CPIKTKT+CWARECODE TAG (lcTmpPoh) ADDITIVE 
ENDIF
SELECT PIKTKT
lnField = AFIELDS(laPikStru)
= gfCrtTmp(lcTmpPik, @laPikStru, 'PIKTKT', lcTmpPik)

loPiktkt.Seek('')
IF loFormSet.lcDcOrStore ='D'
  SCAN FOR Status = "O" AND !EMPTY(Store) AND (!SEEK(PIKTKT.PIKTKT,lcTmpPoh) OR (SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh) AND &lcTmpPoh..Status ='O'))
    SCATTER MEMO MEMVAR
    IF !SEEK(m.Piktkt,lcTmpPik)
      INSERT INTO (lcTmpPik) FROM MEMVAR
    ENDIF
  ENDSCAN  
ELSE
  SCAN FOR Status = "C" AND !EMPTY(Store) AND ;
    (SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,lcTmpPoh) AND &lcTmpPoh..Status ='C') AND ;
    (!SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,'PIKVENDOR') OR (SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,'PIKVENDOR') AND &lcTmpPoh..Status ='O'))
    SCATTER MEMO MEMVAR
    IF !SEEK(m.Piktkt,lcTmpPik)
      INSERT INTO (lcTmpPik) FROM MEMVAR
    ENDIF
  ENDSCAN  
ENDIF  
*!*************************************************************
*! Name      : lfCollectDataGrid
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/02/2015 
*! Purpose   : Function to arange the push button select prompt
*!*************************************************************
FUNCTION lfCollectDataGrid
ACOPY(loogscroll.laogFxflt,laOgSelect)
*!**************************************************************************
*! Name      : lfvShipAddress1
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/02/2015 
*! Purpose   : Fix the Order Browse customer shipping address 1
*!**************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : Valid Shipping Address 1
*!**************************************************************************
*! Example   : =lfvShipAddress1("MA100","HOB")
*!**************************************************************************
*!
FUNCTION lfvShipAddress1
LPARAMETERS lcAccount, lcStore
LOCAL lcAddress1, lnSelect
lnSelect = SELECT(0)
IF !USED("Customer")
  gfOpenTable("Customer","Customer")
ENDIF
IF EMPTY(lcStore)
  =gfSEEK("M"+lcAccount,"CUSTOMER","CUSTOMER")
ELSE
  =gfSEEK("S"+lcAccount+lcStore,"CUSTOMER","CUSTOMER")
ENDIF
lcAddress1 = Customer.cAddress1

SELECT(lnSelect)
RETURN lcAddress1
ENDFUNC

*!*************************************************************
*! Name      : lfvScope
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/02/2015 
*! Purpose   : Valid function of push button Scope
*!*************************************************************
*! Called from : Control Panel , lpShow
*!*************************************************************
*! Calls       : gfOpGrid() , lfCrTmKtTk() , lfDispBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfvScope
PARAMETERS loFormSet

lndataSessPre = SET("Datasession" )
IF loFormSet.llCalledFromOp
  loFormSet.llCallOption = loFormSet.llCalledFromOp
ENDIF
loFormSet.llCallScop = .F.             && Screen Already Initialized
lcDataSessI = SET("Datasession" )&&THIS.loFormSet.DatasessionID
DIMENSION laOgSelect[1]
laOgSelect =''
LNRPDAYS = loFormSet.LNRPDAYS
lcExpr = gfOpGrid('PODGREPL' , .T.)&&,.F.,.F.,.T.,.T.)
loFormSet.LNRPDAYS = LNRPDAYS 
SET DATASESSION TO lcDataSessI

IF lcExpr <> ".F."
  *!*	PIKTKT.PIKTKT
  llSelePIK = .F. && flag to indicate if there is any piktkt selected
  lnPosPik = ASCAN(laOgSelect,"PIKTKT.PIKTKT")
  IF lnPosPik > 0
    lnPosPik = ASUBSCRIPT(laOgSelect,lnPosPik,1)
    lcFilePik =IIF(!EMPTY(laOgSelect[lnPosPik,6]),laOgSelect[lnPosPik,6],'')
    IF !EMPTY(lcFilePik) AND USED(lcFilePik) AND RECCOUNT(lcFilePik)> 0
      SELECT(lcFilePik)
      LOCATE For !DELETED()
      IF !EOF()
        llSelePIK  = .T.
      ENDIF  
    ENDIF
  ENDIF
   
  *!*	ORDHDR.ORDER     
  llSeleOrder= .F. && flag to indicate if there is any piktkt selected
  lnPosOrder= ASCAN(laOgSelect,"ORDHDR.ORDER")
  IF lnPosOrder> 0
    lnPosOrder= ASUBSCRIPT(laOgSelect,lnPosOrder,1)
	lcFileOrder=IIF(!EMPTY(laOgSelect[lnPosOrder,6]),laOgSelect[lnPosOrder,6],'')
	IF !EMPTY(lcFileOrder) AND USED(lcFileOrder) AND RECCOUNT(lcFileOrder)> 0
	  SELECT(lcFileOrder)
	  LOCATE For !DELETED()
	  IF !EOF()
	    llSeleOrder= .T.
	  ENDIF  
	ENDIF
  ENDIF
  
  *!*	PIKTKT.ACCOUNT   
  llSeleAcc= .F. && flag to indicate if there is any piktkt selected
  lnPosAcc= ASCAN(laOgSelect,"PIKTKT.ACCOUNT")
  IF lnPosAcc> 0
    lnPosAcc= ASUBSCRIPT(laOgSelect,lnPosAcc,1)
    lcFileAccount=IIF(!EMPTY(laOgSelect[lnPosAcc,6]),laOgSelect[lnPosAcc,6],'')
    IF !EMPTY(lcFileAccount) AND USED(lcFileAccount) AND RECCOUNT(lcFileAccount)> 0
      SELECT(lcFileAccount)
      LOCATE For !DELETED()
      IF !EOF()
        llSeleAcc= .T.
      ENDIF  
    ENDIF
  ENDIF

  *!*	PIKTKT.DATE
  ldStartDate = {}
  ldEndDate   = {}
  llSeleDate= .F. && flag to indicate if there is any piktkt selected
  lnDatePos= ASCAN(laOgSelect,"PIKTKT.DATE")
  IF lnDatePos> 0
    lnDatePos= ASUBSCRIPT(laOgSelect,lnDatePos,1)
    llSeleDate = !EMPTY(laOgSelect[lnDatePos,6])
    ldStartDate = CTOD(SUBSTR(laOgSelect[lnDatePos,6],1,10))
    ldEndDate   = CTOD(SUBSTR(laOgSelect[lnDatePos,6],12,21))
  ENDIF
 
  IF TYPE('loPOSHDR') <> 'O'
    loPOSHDR= CreateObject("RemoteTable","POSHDR","POSHDR","POSHDR",SET("DATASESSION"))
  ENDIF
  loPOSHDR.SqlRun("Select CPIKTKT ,PO,CWARECODE,STATUS,Vendor,ENTERED from POSHDR WHERE CSTYTYPE ='N' and CBUSDOCU='N' AND CPIKTKT !='' and ISNULL(CPIKTKT,'C') != 'C'",loFormSet.LCTMPPOH)
  lcTmpPoh = loFormSet.LCTMPPOH
  IF USED(loFormSet.LCTMPPOH)
    SELECT(loFormSet.LCTMPPOH) 
    CURSORSETPROP("Buffering" ,3)
    IF loFormSet.lcDcOrStore ='S'
      INDEX on CPIKTKT+Vendor TAG 'PIKVENDOR' 
    ENDIF
    INDEX on CPIKTKT+CWARECODE+Vendor TAG (loFormSet.LCTMPPOH) 
  ENDIF
  SELECT (loFormSet.lcTempFileHdr)
  ZAP 
  DO CASE 
    CASE llSelePIK && Case Picking ticket range is selected
      SELECT PIKTKT
      =gfSetOrder('PIKTKT')
      SELECT (lcFilePik)
      LOCATE 
      SCAN 
        IF gfSeek(&lcFilePik..PIKTKT,'PIKTKT','PIKTKT')
          SELECT PIKTKT
          SCATTER MEMO MEMVAR 
          m.LLSEL = .T.
          IF !SEEK(m.PIKTKT,loFormSet.lcTempFileHdr) AND IIF(llSeleDate,BETWEEN(PIKTKT.DATE,ldStartDate ,ldEndDate),.T.) AND ;
            IIF(llSeleAcc,SEEK(PIKTKT.Account,lcFileAccount),.T.) AND IIF(llSeleOrder,SEEK(Piktkt.Order,lcFileOrder),.T.) AND ;
            IIF(loFormSet.lcDcOrStore ='D', (!SEEK(PIKTKT,loFormSet.LCTMPPOH) OR ;
              (SEEK(PIKTKT+SUBSTR(STORE,1,6),loFormSet.LCTMPPOH) AND EVAL(loFormSet.LCTMPPOH+'.Status')='O')),;
              (SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,lcTmpPoh) AND &lcTmpPoh..Status ='C' AND &lcTmpPoh..ENTERED +loFormSet.lnRpDays =< oAriaApplication.SystemDate) AND (!SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,'PIKVENDOR') OR (SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,'PIKVENDOR') AND &lcTmpPoh..Status ='O')))
            m.Po =''
	        IF loFormSet.lcDcOrStore ='D' AND SEEK(m.PIKTKT+SUBSTR(m.STORE,1,6),loFormSet.LCTMPPOH) AND EVALUATE(loFormSet.LCTMPPOH+'.Status') = 'O'
  	          m.PO = EVALUATE(loFormSet.LCTMPPOH+'.PO')
	        ENDIF
	        IF loFormSet.lcDcOrStore ='S' AND SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,'PIKVENDOR') AND &lcTmpPoh..Status ='O'
      	       m.PO = EVALUATE(loFormSet.LCTMPPOH+'.PO')
	        ENDIF
            m.TOT_PCS = lfGetPikQty(m.Order,m.Piktkt)
            INSERT INTO (loFormSet.lcTempFileHdr) FROM MEMVAR
          ENDIF
        ENDIF  
      ENDSCAN 
    CASE llSeleOrder && Case Sales Order range is selected
      
      SELECT PIKTKT
      =gfSetOrder('ORDPIK')
      SELECT (lcFileOrder)
      LOCATE 
      SCAN
        IF gfSeek(&lcFileOrder..oRDER,'PIKTKT','ORDPIK') 
          SELECT PIKTKT
          SCAN REST WHILE ORDER+PIKTKT = &lcFileOrder..oRDER FOR ;
              IIF(loFormSet.lcDcOrStore ='D',STATUS ='O',STATUS ='C') AND !EMPTY(Store) AND;
              IIF(loFormSet.lcDcOrStore ='D', (!SEEK(PIKTKT,loFormSet.LCTMPPOH) OR ;
              (SEEK(PIKTKT+SUBSTR(STORE,1,6),loFormSet.LCTMPPOH) AND EVAL(loFormSet.LCTMPPOH+'.Status')='O')),;
              (SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,lcTmpPoh) AND &lcTmpPoh..Status ='C' AND &lcTmpPoh..ENTERED +loFormSet.lnRpDays =< oAriaApplication.SystemDate) AND (!SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,'PIKVENDOR') OR (SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,'PIKVENDOR') AND &lcTmpPoh..Status ='O'))) AND ; 
              IIF(llSeleAcc,SEEK(PIKTKT.Account,lcFileAccount),.T.) AND IIF(llSeleDate,BETWEEN(PIKTKT.DATE,ldStartDate ,ldEndDate),.T.)
              SELECT PIKTKT
            SCATTER MEMO MEMVAR 
            m.LLSEL = .T.
            IF !SEEK(m.PIKTKT,loFormSet.lcTempFileHdr)
              m.Po =''
              
    	      IF loFormSet.lcDcOrStore ='D' AND SEEK(m.PIKTKT+SUBSTR(m.STORE,1,6),loFormSet.LCTMPPOH) AND EVALUATE(loFormSet.LCTMPPOH+'.Status') = 'O'
  	             m.PO = EVALUATE(loFormSet.LCTMPPOH+'.PO')
    	      ENDIF
              IF loFormSet.lcDcOrStore ='S' AND SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,'PIKVENDOR') AND &lcTmpPoh..Status ='O'
      	        m.PO = EVALUATE(loFormSet.LCTMPPOH+'.PO')
	          ENDIF
	          

              m.TOT_PCS = lfGetPikQty(m.Order,m.Piktkt)
             INSERT INTO (loFormSet.lcTempFileHdr) FROM MEMVAR
            ENDIF
          ENDSCAN 
        ENDIF
      ENDSCAN 
      
    OTHERWISE 
      SELECT PIKTKT
      =gfSetOrder('PIKTKT')
      =gfSeek('')
	  LOCATE 
      SCAN FOR IIF(loFormSet.lcDcOrStore ='D',STATUS ='O',STATUS ='C') AND !EMPTY(Store) AND ;
          IIF(loFormSet.lcDcOrStore ='D',(!SEEK(PIKTKT,loFormSet.LCTMPPOH) OR (SEEK(PIKTKT+SUBSTR(STORE,1,6),loFormSet.LCTMPPOH) AND EVALUATE(loFormSet.LCTMPPOH+'.Status') = 'O')),;
              (SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,lcTmpPoh) AND &lcTmpPoh..Status ='C'  AND &lcTmpPoh..ENTERED +loFormSet.lnRpDays =< oAriaApplication.SystemDate) AND;
               (!SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,'PIKVENDOR') OR (SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,'PIKVENDOR') AND &lcTmpPoh..Status ='O'))) AND ;
              IIF(llSeleAcc,SEEK(PIKTKT.Account,lcFileAccount),.T.) AND IIF(llSeleDate,BETWEEN(PIKTKT.DATE,ldStartDate ,ldEndDate),.T.)
	    SELECT PIKTKT
	    SCATTER MEMO MEMVAR 
	    m.LLSEL = .T.
	    IF !SEEK(m.PIKTKT,loFormSet.lcTempFileHdr)
	      m.Po =''
	      IF loFormSet.lcDcOrStore ='D' AND SEEK(m.PIKTKT+SUBSTR(m.STORE,1,6),loFormSet.LCTMPPOH) AND EVALUATE(loFormSet.LCTMPPOH+'.Status') = 'O'
  	        m.PO = EVALUATE(loFormSet.LCTMPPOH+'.PO')
	      ENDIF
	      IF loFormSet.lcDcOrStore ='S' AND SEEK(PIKTKT.PIKTKT+SUBSTR(PIKTKT.STORE,1,6),lcTmpPoh,'PIKVENDOR') AND &lcTmpPoh..Status ='O'
  	        m.PO = EVALUATE(loFormSet.LCTMPPOH+'.PO')
	      ENDIF
	      m.TOT_PCS = lfGetPikQty(m.Order,m.Piktkt)
	      INSERT INTO (loFormSet.lcTempFileHdr) FROM MEMVAR
	    ENDIF
      ENDSCAN 
  ENDCASE
  SELECT (loFormSet.lcTempFileHdr)
  LOCATE
  IF !EOF()
    IF loFormSet.lcDcOrStore ='S'
      SELECT (loFormSet.lcPikline)
      ZAP 
      SELECT (loFormSet.lcStorDetail)
      ZAP
      IF !USED('PIKLINE_DC')
        =gfOpenTable('PIKLINE','PIKLINE','Sh','PIKLINE_DC')
      ENDIF
      SELECT (loFormSet.lcTempFileHdr)
	  SCAN 
	    IF gfSeek(EVALUATE(loFormSet.lcTempFileHdr+'.PIKTKT'),'PIKLINE_DC')
	      SELECT PIKLINE_DC
	      SCAN REST WHILE PIKTKT+ORDER+STR(LINENO,6) = EVALUATE(loFormSet.lcTempFileHdr+'.PIKTKT')
	        SCATTER MEMO MEMVAR 

	        INSERT INTO (loFormSet.lcPikline) FROM MEMVAR
	      ENDSCAN 
	    ENDIF
        SELECT (loFormSet.lcTempFileHdr)
      ENDSCAN  
      IF !USED('Ordline_bulk')
        =gfOpenTable('Ordline','ORDBULK','Sh','Ordline_bulk')
      ENDIF
      IF !USED('Customer_bulk')
        =gfOpenTable('Customer','Customer','Sh','Customer_bulk')
      ENDIF
      SELECT (loFormSet.lcPikline) 
      LOCATE
      SCAN 
        lcPiktkt = EVALUATE(loFormSet.lcPikline+'.PIKTKT')
        lcBulkOrder =  EVALUATE(loFormSet.lcPikline+'.Order')
        lcDctr  =  EVALUATE(loFormSet.lcPikline+'.Store')
        lnLineNo = EVALUATE(loFormSet.lcPikline+'.LineNo')
        lcStyle =  EVALUATE(loFormSet.lcPikline+'.Style')
        lcPikline = loFormSet.lcPikline
        SELECT Ordline_bulk
        =gfSeek('O'+lcBulkOrder+STR(lnLineNo,6),'Ordline_bulk','ORDBULK')  
        SCAN REST WHILE CORDTYPE+CFROMORDER+STR(BULKLINENO,6)='O'+lcBulkOrder+STR(lnLineNo,6) FOR !EMPTY(Store) AND;
             gfSeek('S'+Ordline_bulk.Account+Ordline_bulk.Store,'Customer_bulk','Customer') AND Customer_bulk.Dist_Ctr = lcDctr AND Style = lcStyle 
          SELECT (loFormSet.lcPikline)
          IF PIK1 = 0 AND PIK2 = 0 AND PIK3 = 0 AND PIK4 = 0 AND PIK5 = 0 AND PIK6 = 0 AND PIK7 = 0 AND PIK8 = 0 
            LOOP
          ENDIF 
          

          IF !Seek(lcPiktkt +  Ordline_bulk.STORE,loFormSet.lcStorDetail)   
            INSERT INTO (loFormSet.lcStorDetail) (PIKTKT,Store,Order,TotQty) VALUES (lcPiktkt ,Ordline_bulk.STORE,Ordline_bulk.Order,MIN(Ordline_bulk.TotQty,EVALUATE(loFormSet.lcPikline+'.TotPIK')))
          ELSE
            REPLACE TotQty WITH totQty +MIN(Ordline_bulk.TotQty,EVALUATE(loFormSet.lcPikline+'.TotPIK')) IN (loFormSet.lcStorDetail)
          ENDIF
          
          IF !SEEK(lcPiktkt +lcStyle +SUBSTR(lcDctr,1,6)+Ordline_bulk.Store,loFormSet.lcpolines)
            INSERT INTO (loFormSet.lcpolines) (Piktkt,Store,cWareCode,Style,Qty1,Qty2,qty3,qty4,qty5,qty6,qty7,qty8)values;
                        (lcPiktkt,Ordline_bulk.Store,SUBSTR(lcDctr,1,6),lcStyle,MIN(&lcPikline..PIK1,ORDLINE_BULK.Qty1),;
                        MIN(&lcPikline..PIK2,ORDLINE_BULK.Qty2),MIN(&lcPikline..PIK3,ORDLINE_BULK.Qty3),MIN(&lcPikline..PIK4,ORDLINE_BULK.Qty4),;
                        MIN(&lcPikline..PIK5,ORDLINE_BULK.Qty5),MIN(&lcPikline..PIK6,ORDLINE_BULK.Qty6),;
                        MIN(&lcPikline..PIK7,ORDLINE_BULK.Qty7),MIN(&lcPikline..PIK8,ORDLINE_BULK.Qty8))
          ELSE
            REPLACE Qty1 WITH Qty1 + MIN(&lcPikline..PIK1,ORDLINE_BULK.Qty1),; 
		            Qty2 WITH Qty2 + MIN(&lcPikline..PIK2,ORDLINE_BULK.Qty2),; 
		            Qty3 WITH Qty3 + MIN(&lcPikline..PIK3,ORDLINE_BULK.Qty3),; 
		            Qty4 WITH Qty4 + MIN(&lcPikline..PIK4,ORDLINE_BULK.Qty4),; 
		            Qty5 WITH Qty5 + MIN(&lcPikline..PIK5,ORDLINE_BULK.Qty5),; 
		            Qty6 WITH Qty6 + MIN(&lcPikline..PIK6,ORDLINE_BULK.Qty6),; 
		            Qty7 WITH Qty7 + MIN(&lcPikline..PIK7,ORDLINE_BULK.Qty7),; 
		            Qty8 WITH Qty8 + MIN(&lcPikline..PIK8,ORDLINE_BULK.Qty8) IN (loFormSet.lcpolines)
          ENDIF

          SELECT (loFormSet.lcPikline)
          REPLACE PIK1 WITH MAX(PIK1 - ORDLINE_BULK.PIK1,0),;
                  PIK2 WITH MAX(PIK2 - ORDLINE_BULK.PIK2,0),;
                  PIK3 WITH MAX(PIK3 - ORDLINE_BULK.PIK3,0),;
                  PIK4 WITH MAX(PIK4 - ORDLINE_BULK.PIK4,0),;
                  PIK5 WITH MAX(PIK5 - ORDLINE_BULK.PIK5,0),;
                  PIK6 WITH MAX(PIK6 - ORDLINE_BULK.PIK6,0),;
                  PIK7 WITH MAX(PIK7 - ORDLINE_BULK.PIK7,0),;
                  PIK8 WITH MAX(PIK8 - ORDLINE_BULK.PIK8,0),;
                  TotPIk WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8
                  
          
        ENDSCAN 
        SELECT (loFormSet.lcPikline) 
      ENDSCAN 
      SELECT (loFormSet.lcTempFileHdr)
      SET RELATION TO  piktkt INTO (loFormSet.lcStorDetail)
      DELETE FOR EOF(loFormSet.lcStorDetail) 
      LOCATE 
    ENDIF
    lfUpdTotals(loFormSet)
    loFormSet.ChangeMode("V")
  ENDIF
  lfAddControlSource(loFormSet)
  SELECT (loFormSet.lcTempFileHdr)
  loFormSet.lnSelRec   = RECCOUNT()
  loFormSet.lnDelRec   = 0
  loFormSet.llenablerel = .F.
  loFormSet.lnUnSelRec = 0
  IF !EOF()
    loFormSet.llEnableInvert = .T.
    loFormSet.llEnableSelect = .T.
    loFormSet.llEnableSelectall = .F.
    loFormSet.llEnableSelectnone = .T.
  ELSE    && Else
    loFormSet.llEnableInvert = .F.
    loFormSet.llEnableSelect = .F.
    loFormSet.llEnableSelectAll = .F.
    loFormSet.llEnableSelectNone = .F.
  ENDIF    && End of IF
ELSE
  loFormSet.llEnableInvert = .F.
  loFormSet.llEnableSelect = .F.
  loFormSet.llEnableSelectAll = .F.
  loFormSet.llEnableSelectNone = .F.
  RETURN
ENDIF
*!*************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/02/2015 
*! Purpose   : init function of th from
*!*************************************************************
FUNCTION lfInit
  LPARAMETERS loFormSet
  SET MULTILOCKS ON
  loFormSet.lcTempFileHdr = gfTempName()
  loFormSet.lcPodetail = gfTempName()
  loFormSet.LCTMPPOH = gfTempName()
  IF loFormSet.lcDcOrStore = 'S'
    loFormSet.lcStorDetail = gfTempName()
    loFormSet.lcPikline = gfTempName()
    loFormSet.lcpolines = gfTempName()
    DIMENSION laStoreArr[5,4]
    
    laStoreArr[1,1] = 'PIKTKT'
    laStoreArr[1,2] = 'C'
    laStoreArr[1,3] = 6
    laStoreArr[1,4] = 0
    
    laStoreArr[2,1] = 'DC'
    laStoreArr[2,2] = 'C'
    laStoreArr[2,3] = 8
    laStoreArr[2,4] = 0

    
    laStoreArr[3,1] = 'Store'
    laStoreArr[3,2] = 'C'
    laStoreArr[3,3] = 8
    laStoreArr[3,4] = 0
    
    laStoreArr[4,1] = 'Order'
    laStoreArr[4,2] = 'C'
    laStoreArr[4,3] = 6
    laStoreArr[4,4] = 0

    laStoreArr[5,1] = 'TotQty'
    laStoreArr[5,2] = 'N'
    laStoreArr[5,3] = 12
    laStoreArr[5,4] = 0
    
    
    =gfCrtTmp(loFormSet.lcStorDetail ,@laStoreArr,"PIKTKT+STORE", loFormSet.lcStorDetail ,.T.)
    
    IF !USED('PIKLINE')
      =gfOpenTable('PIKLINE','PIKLINE')
    ENDIF
    SELECT 'PIKLINE'
    AFIELDS(laPikStru)
    =gfCrtTmp(loFormSet.lcPikline,@laPikStru,"PIKTKT+ORDER+STR(LINENO,6)", loFormSet.lcPikline,.T.)
    DIMENSION laDetStru [12,4]
    laDetStru [1,1] = 'PIKTKT'
    laDetStru [1,2] = 'C'
    laDetStru [1,3] = 6
    laDetStru [1,4] = 0

    laDetStru [2,1] = 'Style'
    laDetStru [2,2] = 'C'
    laDetStru [2,3] = 19
    laDetStru [2,4] = 0

    laDetStru [3,1] = 'cWareCode'
    laDetStru [3,2] = 'C'
    laDetStru [3,3] = 6
    laDetStru [3,4] = 0

    laDetStru [4,1] = 'Store'
    laDetStru [4,2] = 'C'
    laDetStru [4,3] = 8
    laDetStru [4,4] = 0

    laDetStru [5,1] = 'Qty1'
    laDetStru [5,2] = 'N'
    laDetStru [5,3] = 9
    laDetStru [5,4] = 0

    laDetStru [6,1] = 'Qty2'
    laDetStru [6,2] = 'N'
    laDetStru [6,3] = 9
    laDetStru [6,4] = 0


    laDetStru [7,1] = 'Qty3'
    laDetStru [7,2] = 'N'
    laDetStru [7,3] = 9
    laDetStru [7,4] = 0

    laDetStru [8,1] = 'Qty4'
    laDetStru [8,2] = 'N'
    laDetStru [8,3] = 9
    laDetStru [8,4] = 0

    laDetStru [9,1] = 'Qty5'
    laDetStru [9,2] = 'N'
    laDetStru [9,3] = 9
    laDetStru [9,4] = 0
    
    laDetStru [10,1] = 'Qty6'
    laDetStru [10,2] = 'N'
    laDetStru [10,3] = 9
    laDetStru [10,4] = 0

    laDetStru [11,1] = 'Qty7'
    laDetStru [11,2] = 'N'
    laDetStru [11,3] = 9
    laDetStru [11,4] = 0

    
    laDetStru [12,1] = 'Qty8'
    laDetStru [12,2] = 'N'
    laDetStru [12,3] = 9
    laDetStru [12,4] = 0
    
    =gfCrtTmp(loFormSet.lcpolines,@laDetStru ,"PIKTKT+Style+cWareCode+Store", loFormSet.lcpolines,.T.)
  ENDIF  
  
  gfOpenTable('PIKTKT','PIKTKT')

  DIMENSION laFileStru[8,4]

  laFileStru[1,1] = 'LLSEL'
  laFileStru[1,2] = 'L'
  laFileStru[1,3] = 1
  laFileStru[1,4] = 0

  laFileStru[2,1] = 'PIKTKT'
  laFileStru[2,2] = 'C'
  laFileStru[2,3] = 6
  laFileStru[2,4] = 0

  laFileStru[3,1] = 'Account'
  laFileStru[3,2] = 'C'
  laFileStru[3,3] = 5
  laFileStru[3,4] = 0
  
  laFileStru[4,1] = 'PO'
  laFileStru[4,2] = 'C'
  laFileStru[4,3] = 6
  laFileStru[4,4] = 0

  laFileStru[5,1] = 'CWARECODE'
  laFileStru[5,2] = 'C'
  laFileStru[5,3] = 6
  laFileStru[5,4] = 0

  laFileStru[6,1] = 'STORE'
  laFileStru[6,2] = 'C'
  laFileStru[6,3] = 8
  laFileStru[6,4] = 0

  laFileStru[7,1] = 'ORDER'
  laFileStru[7,2] = 'C'
  laFileStru[7,3] = 6
  laFileStru[7,4] = 0

  laFileStru[8,1] = 'TOT_PCS'
  laFileStru[8,2] = 'N'
  laFileStru[8,3] = 8
  laFileStru[8,4] = 0

  =gfCrtTmp(loFormSet.lcTempFileHdr ,@laFileStru,"PIKTKT",  loFormSet.lcTempFileHdr ,.T.)
  lfAddControlSource(loFormSet)

  WITH loFormSet
    .nWorkArea        = 'PIKTKT'
    .DataEnvironment.InitialSelectedAlias = 'PIKTKT'
    .cbrowsetabledbengine   = 'NATIVE'
  ENDWITH
  loFormSet.llCallScop = .F.   &&Flag to hold the first time of the session
  loFormSet.llFrstTime = .T.       && Flag to know if we are going to call lpShow for the first time
ENDFUNC
*!*************************************************************
*! Name      : lfAddControlSource
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/02/2015 
*! Purpose   : Adjust grid control source
*!*************************************************************
FUNCTION lfAddControlSource
PARAMETERS loFormSet
WITH loFormSet.ariaform1.grdPiktkt.grdMultiSelectionGrid
    .RecordSource = ""
    .RecordSource = loFormSet.lcTempFileHdr 
    .Column1.Header1.Caption = ""
    .Column1.CurrentControl = "AriaCheckBox1"
    .column1.ControlSource ='Thisformset.mgetValueLogic()'
    .column2.ControlSource = loFormSet.lcTempFileHdr +'.PIKTKT'
    .column3.ControlSource = loFormSet.lcTempFileHdr +'.ACCOUNT'
    .column4.ControlSource = loFormSet.lcTempFileHdr +'.STORE'
    .column5.ControlSource = loFormSet.lcTempFileHdr +'.ORDER'
    .column6.ControlSource = loFormSet.lcTempFileHdr +'.PO'
    .column7.ControlSource = loFormSet.lcTempFileHdr +'.CWARECODE'
    .column8.ControlSource = loFormSet.lcTempFileHdr +'.TOT_PCS'
  
    .Column1.Enabled = .T.
    .SETALL('ReadOnly',.T.,'COLUMN')
    .Column1.readonly = .F.
	.Enabled = .T.
    .Column1.AriaCheckBox1.Enabled = .T.
    .refresh()
ENDWITH
IF loFormSet.lcDcOrStore ='S'
  WITH loFormSet.ariaform1.grdStores
    .RecordSource = ""
    .RecordSource = loFormSet.lcStorDetail 
    .column1.ControlSource = loFormSet.lcStorDetail +'.Store'
    .column2.ControlSource = loFormSet.lcStorDetail +'.ORDER'
    .column3.ControlSource = loFormSet.lcStorDetail +'.TotQty'
    .refresh()
ENDWITH
  
ENDIF
  
  
ENDFUNC
*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/02/2015
*! Purpose   : Valid function of push button Select all
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelAll
PARAMETERS loFormSet
SELECT(loFormSet.lcTempFileHdr)
lnRecCurrn = RECNO()

REPLACE ALL LLSEL WITH .T.
loFormSet.lnSelRec   = RECCOUNT() - loFormSet.lnDelRec
loFormSet.lnUnSelRec = 0
GO lnRecCurrn

loFormSet.lcCaptionSel = "UnSelect"

loFormSet.llenableselectall = .F.
loFormSet.llenableselectnone = .T.
loFormSet.llenablerel = .T.

*!*************************************************************
*! Name      : lfvSelNon
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/02/2015
*! Purpose   : Valid function of push button Select none
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelNon
PARAMETERS loFormSet
SELECT(loFormSet.lcTempFileHdr)

lnRecCurr = RECNO()

REPLACE ALL LLSEL WITH .F.

loFormSet.lnSelRec   = 0
loFormSet.lnUnSelRec = RECCOUNT() - loFormSet.lnDelRec
GO lnRecCurr
loFormSet.lcCaptionSel = 'Select'

loFormSet.llEnableSelectAll  = .T.
loFormSet.llEnableSelectNone = .F.
loFormSet.llEnableRel = .F.
*!*************************************************************
*! Name      : lfvInvert
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/02/2015
*! Purpose   : Valid function of push button Invert
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvInvert
PARAMETERS loFormSet
SELECT(loFormSet.lcTempFileHdr)
lnRecNOCurr = RECNO()
REPLACE ALL LLSEL WITH !LLSEL
GO lnRecNOCurr

lfvpbSel(loFormSet)

loFormSet.lnUnSelRec = loFormSet.lnSelRec
loFormSet.lnSelRec   = RECCOUNT() - loFormSet.lnDelRec - loFormSet.lnSelRec

*there is no selected records
IF loFormSet.lnSelRec = 0
  loFormSet.llenableselectall = .T.
  loFormSet.llenableselectnone = .F.
  loFormSet.llenablerel = .F.
ELSE
  loFormSet.llenableselectnone = .T.
  loFormSet.llenablerel = .T.

  *--All the records were selected
  IF loFormSet.lnUnSelRec = 0
    loFormSet.llenableselectall = .F.
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvSelect
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/02/2015
*! Purpose   : Valid function of push button Select
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelect
PARAMETERS loFormSet
SELECT(loFormSet.lcTempFileHdr)
REPLACE LLSEL WITH !LLSEL
lfvpbSel(loFormSet)

loFormSet.lnSelRec   = IIF(llSel , loFormSet.lnSelRec + 1 , loFormSet.lnSelRec - 1)
loFormSet.lnUnSelRec = IIF(llSel , loFormSet.lnUnSelRec - 1 , loFormSet.lnUnSelRec + 1)

*No records was selected
IF loFormSet.lnSelRec = 0
  loFormSet.llenableinvert = .T.
  loFormSet.llenableselect = .T.
  loFormSet.llenableselectall = .T.
  loFormSet.llenableselectnone = .F.
  loFormSet.llenablerel = .F.
ELSE    && Else
  loFormSet.llenableselectnone = .T.
  loFormSet.llenablerel = .T.

  *-- All the records were selected
  IF loFormSet.lnUnSelRec = 0
    loFormSet.llenableselectall = .F.
  ELSE
    loFormSet.llenableselectall = .T.
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvpbSel
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/02/2015
*! Purpose   : Function to arrange the push button select prompt
*!*************************************************************
*! Called from : lfvSelect() , lfvInvert() , The Browse [lcPickBrow]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T.
*!*************************************************************
FUNCTION lfvpbSel
PARAMETERS loFormSet
IF LLSEL
  loFormSet.lcCaptionSel = "Unselect"
ELSE
  loFormSet.lcCaptionSel = "Select"
ENDIF

RETURN .T.

*!*************************************************************
*! Name      : lfGetPikQty
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/02/2015
*! Purpose   : Function to get piktkt Qty
*!*************************************************************
FUNCTION lfGetPikQty
LPARAMETERS lcOrder,lcPiktkt
lcSelect = SELECT()
IF !USED('Ordline')
  =gfOpenTable('Ordline','Ordline')
ENDIF
SELECT Ordline
=gfSeek("O"+lcOrder)
lnPikQty = 0
SUM TotPik TO lnPikQty REST WHILE CORDTYPE+ORDER+STR(LINENO,6)= "O"+lcOrder FOR PIKTKT = lcPiktkt
IF lnPikQty = 0
  IF !USED('PIKline_Qty')
    =gfOpenTable('PIKline','PIKline','SH','PIKline_Qty')
  ENDIF
  SELECT PIKline_QTY
  =gfSeek(lcPiktkt)
  SUM TotPik TO lnPikQty REST WHILE PIKTKT+ORDER+STR(LINENO,6)= lcPiktkt
ENDIF
SELECT(lcSelect)
RETURN lnPikQty 

*!*************************************************************
*! Name      : lfCreateInterLocationPO
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/02/2015
*! Purpose   : Function to create Inter-location PO
*!*************************************************************
FUNCTION lfCreateInterLocationPO
LPARAMETERS cSourceWareCode, cDestinationWareCode, cReplenishmentAlias,llMultiWarehouse,lcPiktk
lcSavedPO = ''
IF EMPTY(cSourceWareCode) OR (!llMultiWarehouse AND EMPTY(cDestinationWareCode))
  RETURN .F.
ENDIF

SELECT SUM(Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8) FROM (cReplenishmentAlias) INTO ARRAY laSumQty
IF laSumQty[1] <= 0
  RETURN .F.
ENDIF
DECLARE laMouse[1]
laMouse =''
lcOnError = ON('Error')
ON ERROR llNewError =.T.
DO FORM (oAriaApplication.ScreenHome+'PO\POINTRC') WITH 'N', 'N', .F., .T. NAME objPOIntrc NOSHOW
ON erRor &lcOnError.  
objPOIntrc.llSilentMod = .T.
objPOIntrc.Visible = .F.
objPOIntrc.AddNew()
WITH objPOIntrc.AriaForm1.cntPoHeader
  .KbVendor.Keytextbox.Value = cSourceWareCode
  .KbVendor.Keytextbox.Valid()
  
  IF llMultiWarehouse
    .chkMulShpTo.Value = llMultiWarehouse
    .chkMulShpTo.Valid()
  ELSE
    .kbWareHouse.Keytextbox.Value = cDestinationWareCode
    .kbWareHouse.Keytextbox.Valid()    
  ENDIF  
  *- Get Division and Purchase Group
  SELECT (cReplenishmentAlias)
  LOCATE
  IF USED('StyleInfo')
    gfCloseTable('StyleInfo')
  ENDIF    
  =gfOpenTable("Style","Style",'SH','StyleInfo')

  IF gfSEEK(EVALUATE(cReplenishmentAlias+'.Style'), 'StyleInfo')
    .Parent.PgfPOStyle.page1.cntheaderFolder.cboDivision.Value = StyleInfo.cdivision
    .Parent.PgfPOStyle.page1.cntheaderFolder.cboPurGroup.Value = StyleInfo.cpurcode
  ENDIF
  gfCloseTable('StyleInfo')
ENDWITH
*B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][Start]
IF !USED('STYDYE_A')
  =gfOpenTable('STYDYE','STYDYE','SH','STYDYE_A')
ENDIF
*B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][End]
SELECT (cReplenishmentAlias)
LOCATE 
SCAN
  *- Check for empty lines
  LOCAL llEmptyLine
  FOR lnI = 1 TO 8
    llEmptyLine = (EVALUATE(cReplenishmentAlias+".Qty"+ALLTRIM(STR(lnI))) == 0)
    IF !llEmptyLine
      EXIT
    ENDIF
  ENDFOR
  IF llEmptyLine
    LOOP
  ENDIF
  
  WITH objPOIntrc.AriaForm1.PgfPOStyle.page2.cntDetailFolder.Editregion1
    .cmdNew.Click()
    lcOnError = ON('Error')
    ON ERROR llNewError =.T.
    *B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][Start]
    IF !SEEK(PADR(EVALUATE(cReplenishmentAlias+".Style"),19)+IIF(llMultiWarehouse,EVALUATE(cReplenishmentAlias+".cWareCode"),cDestinationWareCode)+SPACE(10),'STYDYE_A')
      DO gpAdStyWar WITH PADR(EVALUATE(cReplenishmentAlias+".Style"),19),SPACE(10),IIF(llMultiWarehouse,EVALUATE(cReplenishmentAlias+".cWareCode"),cDestinationWareCode)
    ENDIF
    *B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][End]
    .kbStyle.txtItem.Value = EVALUATE(cReplenishmentAlias+".Style")
    .kbStyle.txtItem.Valid()
    ON erRor &lcOnError. 
    *- Remove control source ------ [Start]
    FOR lnSz = 1 TO 8
      lcSz = ALLTRIM(STR(lnSz))
      .cntSizesQty.txtQty&lcSz..ControlSource = ''      
    ENDFOR
    .txtTotalQty.ControlSource = ''
    *- Remove control source ------ [End]
    
    *- Add sizes quantities
    FOR lnSz = 1 TO 8
      lcSz = ALLTRIM(STR(lnSz))
      .cntSizesQty.txtQty&lcSz..Value = EVALUATE(cReplenishmentAlias+".Qty"+lcSz)
      .cntSizesQty.txtQty&lcSz..Valid()
    ENDFOR
    
    *- Recover control source ----- [Start]
    LOCAL lcTmpFile
    lcTmpFile = .Parent.oMainClass.cPoLine
    FOR lnSz = 1 TO 8
      lcSz = ALLTRIM(STR(lnSz))
      .cntSizesQty.txtQty&lcSz..ControlSource = lcTmpFile + '.Qty' + lcSz
    ENDFOR
    .txtTotalQty.ControlSource = lcTmpFile + '.TotQty'
    *- Recover control source ----- [End]
    IF llMultiWarehouse
      .cboLineShipTo.Value = "L"
      .cboLineShipTo.InteractiveChange
      .kbLineWareHouse.KEYTEXTBOX.Value = EVALUATE(cReplenishmentAlias+".cWareCode")
      .kbLineWareHouse.KEYTEXTBOX.valid()
    ENDIF
  ENDWITH
ENDSCAN
lnOldDataSe = SET("Datasession" )
SET DATASESSION TO objPOIntrc.DataSessionID
REPLACE CPIKTKT WITH lcPiktk IN (objPOIntrc.AriaForm1.PgfPOStyle.page2.cntDetailFolder.Editregion1.Parent.oMainClass.cPoshdr)
SET DATASESSION TO lnOldDataSe 
objPOIntrc.Visible = .F.
objPOIntrc.HasNotes = .F.
IF objPOIntrc.BeforeSave()
  objPOIntrc.SaveFiles(.F.)  
  lcSavedPO= SUBSTR(STRTRAN(STRTRAN(objPOIntrc.lcSydKey,"'"), '"'), 2)
  objPOIntrc.HasNotes = .T.
  objPOIntrc.Release()
ELSE
  lcSavedPO = ''
  objPOIntrc.HasNotes = .T.
  objPOIntrc.Release()
ENDIF
objPOIntrc =null
RETURN lcSavedPO

*!*************************************************************
*! Name      : lfBefSaveFunc
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/02/2015
*! Purpose   : Check if there is records selected
*!*************************************************************
FUNCTION lfBefSaveFunc
PARAMETERS loFormSet
SELECT (loFormSet.lcTempFileHdr)
LOCATE FOR llSel
IF !FOUND()
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,"No Picking Ticket is selected to create Inter-Location PO for it. Cannot proceed")
  RETURN .F.
ELSE
  RETURN .T.  
ENDIF

*!*************************************************************
*! Name      : lfSaveInterLocPO
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/02/2015
*! Purpose   : Function to call create Inter-location PO function
*!*************************************************************
FUNCTION lfSaveInterLocPO
lPARAMETERS loFormSet
SELECT (loFormSet.lcTempFileHdr)
lnCurrRecond = RECNO()
IF !lfBefSaveFunc(loFormSet)
  GO RECORD lnCurrRecond IN (loFormSet.lcTempFileHdr)
  RETURN .F. 
ENDIF

IF !USED('Ordline_Lines')
  =gfOpenTable('Ordline','Ordline','SH','Ordline_Lines')
ENDIF

oPross = CREATEOBJECT('ariaprogressbar')
SELECT (loFormSet.lcTempFileHdr)
COUNT FOR llSel AND EMPTY(PO) TO lnProg
oPross.TotalProgress = lnProg
oPross.AUTOCENTER = .T.
oPross.Show()
lnCntrP = 1
IF loFormSet.lcDcOrStore ='D' 
  SELECT (loFormSet.lcTempFileHdr)
  LOCATE
  SCAN FOR llSel AND EMPTY(PO)
    lcPiktktNo = EVALUATE(loFormSet.lcTempFileHdr+'.Piktkt')
    lcOrderNum = EVALUATE(loFormSet.lcTempFileHdr+'.Order')
    lcSource = EVALUATE(loFormSet.lcTempFileHdr+'.cWareCode')
    lcDestination = PADR(EVALUATE(loFormSet.lcTempFileHdr+'.Store'),6)
    *WAIT WINDOW "Creating Inter-Location PO for Picking ticket# "+ lcPiktktNo NOWAIT  
    =gfSeek('O'+lcOrderNum ,'Ordline_Lines','Ordline')  
    SELECT Style, Pik1 Qty1, Pik2 Qty2, Pik3 Qty3, Pik4 Qty4, Pik5 Qty5, Pik6 Qty6, ;
           Pik7 Qty7, Pik8 Qty8  ;
           FROM Ordline_Lines ;
           WHERE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrderNum  AND PIKTKT = lcPiktktNo INTO CURSOR (loFormSet.lcPodetail) READWRITE 
     oPross.CurrentProgress(lnCntrP)
     oPross.lblFirstLabel.CAPTION = "Creating Inter-Location PO for Picking ticket# "+ lcPiktktNo   
     PoNum =  lfCreateInterLocationPO(lcSource , lcDestination , loFormSet.lcPodetail,.F.,lcPiktktNo)
     lnCntrP = lnCntrP +  1 
     SELECT (loFormSet.lcTempFileHdr)
     REPLACE Po WITH  PoNum 
     REPLACE llSel WITH .T.
  ENDSCAN 
ELSE
  SELECT (loFormSet.lcTempFileHdr)
  LOCATE
  SCAN FOR llSel AND EMPTY(PO)
    lcSource = SUBSTR(EVALUATE(loFormSet.lcTempFileHdr+'.Store'),1,6)
    lcPiktktNo = EVALUATE(loFormSet.lcTempFileHdr+'.Piktkt')
    *WAIT WINDOW "Creating Inter-Location PO for Picking ticket# "+ lcPiktktNo NO 
    SELECT Style, Qty1, Qty2, Qty3, Qty4, Qty5, Qty6, ;
           Qty7, Qty8 ,SUBSTR(Store,1,6) as cWareCode ;
           FROM (loFormSet.lcpolines) ;
           WHERE PIKTKT+Style+cWareCode+Store = lcPiktktNo AND Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 > 0 INTO CURSOR (loFormSet.lcPodetail) READWRITE 
     oPross.CurrentProgress(lnCntrP)
     oPross.lblFirstLabel.CAPTION = "Creating Inter-Location PO for Picking ticket# "+ lcPiktktNo            
     PoNum =  lfCreateInterLocationPO(lcSource , .F. , loFormSet.lcPodetail,.T.,lcPiktktNo)
     lnCntrP = lnCntrP +  1 
     SELECT (loFormSet.lcTempFileHdr)
     REPLACE Po WITH  PoNum 
     REPLACE llSel WITH .T.
  ENDSCAN 
ENDIF  
oPross = null 
SELECT (loFormSet.lcTempFileHdr)
LOCATE
loFormSet.ariaform1.grdPiktkt.grdMultiSelectionGrid.Refresh()
lfAddControlSource(loFormSet)
GO RECORD lnCurrRecond IN (loFormSet.lcTempFileHdr)
IF gfModalgen("QRM00000B00006","DIALOG",.F.,.F.,'Do you want to Issue and receive the created Inter-Location POs') = 1
  lfIssueReceive(loFormSet) 
ENDIF
GO RECORD lnCurrRecond IN (loFormSet.lcTempFileHdr)
*!*************************************************************
*! Name      : lfIssueReceive
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/02/2015
*! Purpose   : Function to call Issue/Receive Inter-Location PO function
*!*************************************************************
FUNCTION lfIssueReceive
lPARAMETERS loFormSet
SELECT (loFormSet.lcTempFileHdr)
lnCurrRecond = RECNO()
IF !lfBefSaveFunc(loFormSet)
  RETURN .F. 
ENDIF
SELECT (loFormSet.lcTempFileHdr)
LOCATE FOR  !EMPTY(PO)
IF !FOUND()
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,"None of the selected Picking Tickets has Inter-Location PO to issue/receive. Cannot proceed")
  SELECT (loFormSet.lcTempFileHdr)
  LOCATE 
  RETURN .F. 
ENDIF

IF !USED("POSHDR_INT")
  =gfOpenTable("POSHDR",'POSHDR',"SH","POSHDR_INT")
ENDIF


oPross = CREATEOBJECT('ariaprogressbar')
SELECT (loFormSet.lcTempFileHdr)
COUNT FOR llSel AND !EMPTY(PO) TO lnProg
oPross.TotalProgress = lnProg
oPross.AUTOCENTER = .T.
oPross.Show()
lnCntrP = 1


SELECT (loFormSet.lcTempFileHdr)
LOCATE

SCAN FOR llSel AND !EMPTY(PO)
  IF gfSeek('NN'+EVALUATE(loFormSet.lcTempFileHdr+".PO"),'POSHDR_INT','POSHDR') AND POSHDR_INT.Status = 'C'
    LOOP
  ENDIF
  lfIssueReceivePOs(EVALUATE(loFormSet.lcTempFileHdr+'.PO'))
  oPross.CurrentProgress(lnCntrP)
  oPross.lblFirstLabel.CAPTION = "Issuing and receiving PO# "+EVALUATE(loFormSet.lcTempFileHdr+'.PO') 
  lnCntrP = lnCntrP +  1 
  SELECT (loFormSet.lcTempFileHdr)
  REPLACE llSel WITH .T.
ENDSCAN 

oPross = null
SELECT (loFormSet.lcTempFileHdr)
LOCATE 
loFormSet.ariaform1.grdPiktkt.grdMultiSelectionGrid.Refresh()  
GO RECORD lnCurrRecond IN (loFormSet.lcTempFileHdr)
*!*************************************************************
*! Name      : lfIssueReceivePOs
*! Developer : Mariam Mazhar [MMT]
*! Date      : 12/02/2015
*! Purpose   : Function to Issue/Receive Inter-Location PO
*!*************************************************************
FUNCTION lfIssueReceivePOs
PARAMETERS lcPoNum
PRIVATE objPosRec

objPosRec = 'PosRec'
*--- Issue Inter-Location PO
lcOnError = ON('Error')
ON ERROR llNewError =.T.
DO (oAriaApplication.ApplicationHome + 'POSTREC.FXP') WITH '0001', 'N', objPosRec
ON ERROR &lcOnError.
objPosRec.Visible = .F.
objPosRec.AriaForm1.dtPickerPostingDate.Valid()
objPosRec.Visible = .F.
objPosRec.AriaForm1.DtpickerReceivingDate.Valid()
objPosRec.AriaForm1.cboReceivingTypes.Valid()
objPosRec.AriaForm1.KbPONo.Keytextbox.Value = lcPoNum
objPosRec.AriaForm1.KbPONo.Keytextbox.Valid()
objPosRec.Visible = .F.

IF objPosRec.BeforeSave()
  lcOnError = ON('Error')
  ON ERROR llNewError =.T.
  objPosRec.SaveFiles(.F.)
  ON ERROR &lcOnError.
  objPosRec.Release()
  objPosRec =null
ELSE
  objPosRec.Release()
  objPosRec =null
  RETURN .F.
ENDIF

*--- Receive Inter-Location PO
DO (oAriaApplication.ApplicationHome + 'POSTREC.FXP') WITH '0001', 'O', objPosRec
objPosRec.Visible = .F.
objPosRec.AriaForm1.dtPickerPostingDate.Valid()
objPosRec.Visible = .F.
objPosRec.AriaForm1.DtpickerReceivingDate.Valid()
objPosRec.AriaForm1.cboReceivingTypes.Valid()
objPosRec.AriaForm1.KbPONo.Keytextbox.Value = lcPoNum
objPosRec.AriaForm1.KbPONo.Keytextbox.Valid()
objPosRec.Visible = .F.
IF objPosRec.BeforeSave()
  lcOnError = ON('Error')
  ON ERROR llNewError =.T.
  objPosRec.SaveFiles(.F.)
  ON ERROR &lcOnError.
  objPosRec.Release()
  objPosRec =null
  RETURN .T.
ELSE
  objPosRec.Release()
  objPosRec =null
  RETURN .F.
ENDIF
*!*****************************************************************************************
*! Name      : lfvDays
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/02/2015 
*! Purpose   : Validate #days field
*!*****************************************************************************************
FUNCTION lfvDays
IF lnRpDays <= 0
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,"No of passed days cannot be less than or equal to 0")
  lnRPDay1 = 14&&loOgScroll.ActiveControl.oldValue
ENDIF 
*!*****************************************************************************************
*! Name      : lfUpdTotals
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 12/02/2015 
*! Purpose   : Update Total fields 
*!*****************************************************************************************
FUNCTION lfUpdTotals
lPARAMETERS loFormSet
IF loFormSet.lcDcOrStore ='S'
  lcStorDetail = loFormSet.lcStorDetail
  lcTempFileHdr = loFormSet.lcTempFileHdr
  SELECT Distinct a.Store FROM (loFormSet.lcStorDetail) a INNER JOIN (loFormSet.lcTempFileHdr) b ON ;
  a.PIKTKT=b.PIKTKT  WHERE b.llSel INTO ARRAY laTagertStore
  IF _tally > 0
    loFormset.ariaForm1.txtStores.Value = ALEN(laTagertStore,1)  
  ELSE
    loFormset.ariaForm1.txtStores.Value = 0  
  ENDIF  
ENDIF
SELECT Distinct Store FROM (loFormSet.lcTempFileHdr) WHERE LLSEL  INTO ARRAY laStores
IF _tally > 0
  loFormset.ariaForm1.txtdcs.Value = ALEN(laStores,1)
ELSE
  loFormset.ariaForm1.txtdcs.Value = 0
ENDIF
SELECT SUM(TOT_PCS) FROM (loFormSet.lcTempFileHdr) WHERE LLSEL INTO ARRAY laTotQty
IF ISNULL(laTotQty[1])
  loFormset.ariaform1.txtqty.Value = 0
Else
  loFormset.ariaform1.txtqty.Value = laTotQty[1]
ENDIF
