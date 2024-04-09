Lparameters lcAccount,lcFileCode
if type('m.Account') ='C' and !Empty(m.Account)
  lcAccount = m.Account
ENDIF
if type('m.cFileCode') ='C' and !Empty(m.cFileCode)
  lcFileCode= m.cFileCode
ENDIF
Set Resource Off
Set Exclusive Off
Set Reprocess To 2 Seconds
Set Cpdialog Off
Set Deleted On
Set Exact Off
Set Safety Off
*Set Classlib To (oAriaApplication.lcAria4Class+"MAIN.VCX"),(oAriaApplication.lcAria4Class+"UTILITY.VCX") ADDITIVE 
aria27path = Strtran(Upper(oAriaApplication.dbfspath),'DBFS\','')
aria4path=oAriaApplication.a4sharedpath
companyid = oAriaApplication.activecompanyid
aria27sysfilespath = (aria27path+'sysfiles\')
dbfspath = oAriaApplication.datadir
classespath = (aria4path+'Classes\')
aria4exe = aria4path+'Aria.exe'
MainClass = Upper(classespath+"Main.VCX")
globalclass = Upper(classespath+'globals.VCX')
utilityclass  = Upper(classespath+'UTILITY.VCX')

set step ON
Set Default To (aria4path)
Set Procedure To (aria4exe) Additive
If  .Not. (MainClass$Set("CLASSLIB"))
	Set Classlib To (MainClass)  ALIAS 'MAIN4' Additive
Endif
If  .Not. (globalclass$Set("CLASSLIB"))
	Set Classlib To (globalclass) ALIAS 'Global4'  Additive
Endif

If  .Not. (utilityclass  $ Set("CLASSLIB"))
	Set Classlib To (utilityclass) ALIAS 'Util4' Additive
Endif
Set Procedure To (aria4path+"PRGS\SY\Ariaglb.fxp") AddIt

lcOldAppHome =oAriaApplication.applicationhome
oAriaApplication.applicationhome = aria4path+"PRGS\"
*oAriaApplication.datadir = dbfspath
oAriaApplication.AddProperty ('cShortVersion','')
lcHeaderCursor  = gfTempName()
lcLineCursor  = gfTempName()
if !used('Style')
  Use (dbfspath +'STYLE.dbf') In 0 Shared Order Style
Else
  Set Order to Style in Style
ENDIF  
if !used('PRODACTI')
  Use (dbfspath +'PRODACTI.dbf') In 0 Shared Order PRODACTI
Else
  Set order to PRODACTI in PRODACTI
ENDIF  
Select PRODACTI
Dimension laFldProd[1]
lnFlds = Afields(laFldProd)
If lnFlds > 0
	Create Cursor 'ProductLines' From Array  laFldProd
Endif
llContinue = .T.
Select PRODACTI
=Seek(lcAccount)
Select Distinct PRODACTI.dprd_bgn,PRODACTI.Store From PRODACTI Where Invoiced And cFileCode = lcFileCode And Account = lcAccount AND INLIST(TRAN_TYPE,'QS','QU') Into Cursor 'InvProdStore'
Select 'InvProdStore'
Go Top
If !Eof()
	lcConsInvoicList= ""
	llHasInvoices = .F.
	llDeleteInvoices = .F.
	Scan
		lcInvoiceNumber = Dtos(InvProdStore.dprd_bgn)+Padl(Alltrim(InvProdStore.Store),5,'0')+"18318"
		Wait Window 'Validating Invoice# '+lcInvoiceNumber  Nowait
    lnResult=oAriaApplication.remotecompanydata.SqlRun("Select * from SMART_INVOICE_HEADER Where SMART_INVOICE_NO = '"+lcInvoiceNumber +"'",lcHeaderCursor , ;
                 "SMART_INVOICE_HEADER",oariaapplication.activecompanyconstr,3,'SAVE',SET("Datasession"))
    IF lnResult<>1
      oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
      WAIT WINDOW 'SMART_INVOICE_HEADER '+'.SqlRun()'
      llContinue = .F.
      EXIT
    ELSE
  		Select(lcHeaderCursor)
  		Locate
	  	If !Eof()
	  		llHasInvoices = .T.
		  	Scan For !Empty(&lcHeaderCursor..INVOICE) And &lcHeaderCursor..Status = 'C'
	  			If ! &lcHeaderCursor..INVOICE $ lcConsInvoicList
		  			lcConsInvoicList= lcConsInvoicList+ "," + &lcHeaderCursor..INVOICE
		  		Endif
	  		Endscan
		  Endif
    ENDIF
		Select 'InvProdStore'
	Endscan

	If !llContinue
	   oAriaApplication.applicationhome = lcOldAppHome 
		Return .F.
	Endif
	If !Empty(lcConsInvoicList)
		lcConsInvoicList= Substr(lcConsInvoicList,2)
		lcMsg = "One or more consolidated invoices "+lcConsInvoicList+" already generated on the selected product activity, so you must delete or void them to be able to regenerate smart invoices again"
		lnMsgRet = Messagebox(lcMsg ,0,_Screen.Caption)
        oAriaApplication.applicationhome = lcOldAppHome 
		Return .F.
	Else
		If llHasInvoices
			lcMsg = "One or more smart invoices already generated on this product activity, press OK to regenaret them or Cancel"
			lnMsgRet = Messagebox(lcMsg ,1,_Screen.Caption)
			If lnMsgRet <> 1
			    	   oAriaApplication.applicationhome = lcOldAppHome 
				Return .T.
			Else
				llDeleteInvoices = .T.
			Endif
		Endif
	Endif
	If llHasInvoices And llDeleteInvoices
    LCTRANCODE = OARIAAPPLICATION.REMOTECOMPANYDATA.BEGINTRAN(OARIAAPPLICATION.ACTIVECOMPANYCONSTR,3,'')
    IF TYPE('lcTranCode') = 'N'
    	   oAriaApplication.applicationhome = lcOldAppHome 
      RETURN .F.
    ENDIF
		llError = .F.
		Select 'InvProdStore'
		Locate
		Scan
			lcInvoiceNumber = Dtos(InvProdStore.dprd_bgn)+Padl(Alltrim(InvProdStore.Store),5,'0')+"18318"
			Wait Window 'Deleting Invoice# '+lcInvoiceNumber  Nowait
			
      lnResult=oAriaApplication.remotecompanydata.SqlRun("DELETE from SMART_INVOICE_HEADER Where SMART_INVOICE_NO = '"+lcInvoiceNumber +"'",lcHeaderCursor , ;
               "SMART_INVOICE_HEADER",oariaapplication.activecompanyconstr,3,'SAVE',SET("Datasession"))
      IF lnResult = 1
        lnResult2 = oAriaApplication.remotecompanydata.SqlRun("DELETE from SMART_INVOICE_LINES Where SMART_INVOICE_NO = '"+lcInvoiceNumber +"'",lcLineCursor , ;
             "SMART_INVOICE_LINES",oariaapplication.activecompanyconstr,3,'SAVE',SET("Datasession"))
       IF lnResult2 <> 1
         llError = .T.
         EXIT
       ENDIF
      ELSE
        llError = .T.
        EXIT
      ENDIF
		Endscan
		If !llError
      LLRETVALUE = (OARIAAPPLICATION.REMOTECOMPANYDATA.COMMITTRAN(LCTRANCODE)=1)
		Else
      LLRETVALUE = (OARIAAPPLICATION.REMOTECOMPANYDATA.ROLLBACKTRAN(LCTRANCODE)=1)
      	   oAriaApplication.applicationhome = lcOldAppHome 
  		Return .F.
		Endif
	Endif
Endif
**Start Update
Select Distinct PRODACTI.dprd_bgn,PRODACTI.Store From PRODACTI Where  cFileCode = lcFileCode And Account = lcAccount AND INLIST(TRAN_TYPE,'QS','QU') Into Cursor 'InvProdStore'
Select 'InvProdStore'
Locate
If !Eof()
	If Used(lcHeaderCursor)
		Use In (lcHeaderCursor)
	Endif
	If Used(lcLineCursor)
		Use In (lcLineCursor)
	Endif
	lnResult=oAriaApplication.remotecompanydata.SqlRun("Select * from SMART_INVOICE_HEADER Where 1= 2",lcHeaderCursor , ;
		"SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
	If lnResult = 1
		lnResult2 = oAriaApplication.remotecompanydata.SqlRun("Select * from SMART_INVOICE_LINES Where 1= 2",lcLineCursor , ;
			"SMART_INVOICE_LINES",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
		If lnResult2 <> 1
			   oAriaApplication.applicationhome = lcOldAppHome 
			Return .F.
		Endif
	ELSE
		   oAriaApplication.applicationhome = lcOldAppHome 
		Return .F.
	Endif
	Select (lcHeaderCursor)
	=CursorSetProp("Buffering" ,3,lcHeaderCursor)
	Index On SMART_INVOICE_NO Tag (lcHeaderCursor)
	Select(lcLineCursor)
	=CursorSetProp("Buffering" ,3,lcLineCursor)
	Index On SMART_INVOICE_NO Tag 'INVINDEX'
	Index On Order+Style+Str(nOrdlineNo,6) Tag (lcLineCursor) Additive
	Set Order To (lcLineCursor)
	=CursorSetProp("Buffering" ,5,lcLineCursor)
	=CursorSetProp("Buffering" ,5,lcHeaderCursor)
	llLineIssue = .F.
	Select 'InvProdStore'
	Locate
	Scan
		linNo = 0
		llGotOrders = .T.
		lcInvoiceNumber = Dtos(InvProdStore.dprd_bgn)+Padl(Alltrim(InvProdStore.Store),5,'0')+"18318"
		Wait Window 'Creating Invoice# '+lcInvoiceNumber  Nowait
		Select PRODACTI
		=Seek(lcAccount+Dtos(InvProdStore.dprd_bgn))
		Scan Rest While Account+Dtos(dprd_bgn)+Dtos(DPRD_END)+DEPT+Store+Style+TRAN_TYPE = lcAccount+Dtos(InvProdStore.dprd_bgn) FOR;
		    cFileCode = lcFileCode And Store =InvProdStore.Store AND INLIST(TRAN_TYPE,'QS','QU')
  		Replace Invoiced With .T.
			Scatter Memo Memvar
			m.Status ='O'
			m.SMART_INVOICE_NO  = lcInvoiceNumber
			If !Seek(m.SMART_INVOICE_NO,lcHeaderCursor,lcHeaderCursor)
				Insert Into (lcHeaderCursor) From Memvar
			Endif

			Dimension laQtyArr[8]
			laQtyArr = 0
			For lnY = 1 To 8
				lcY = Str(lnY,1)
				m.Qty&lcY. = m.nactiqty&lcY. * IIF(PRODACTI.TRAN_TYPE = 'QU',-1,1)
				laQtyArr[lnY] = m.Qty&lcY.
			Endfor
	    m.TotQty = m.ntotactqty * IIF(PRODACTI.TRAN_TYPE = 'QU',-1,1)
   	 IF PRODACTI.TRAN_TYPE = 'QU'
        If Seek(m.Style,'STYLE','STYLE')
          m.Price = Style.PriceA
        Endif     		
	    	linNo = linNo + 1
			  m.LineNo = linNo
	      INSERT INTO (lcLineCursor) FROM MEMVAR
		    llGotOrders = .T.
	    ELSE
  	     llGotOrders = lfAddLinesFromOrder(lcAccount,m.Style,m.Store,@laQtyArr,m.Price)
 		 ENDIF  
			If !llGotOrders
				llLineIssue = .T.
				Exit
			ELSE 
				Select (lcLineCursor)
				Set Order To 'INVINDEX'
				=Seek(m.SMART_INVOICE_NO)
				Sum TotQty, TotQty * Price To Qty,Amt Rest While  SMART_INVOICE_NO = m.SMART_INVOICE_NO
				Set Order To (lcLineCursor)
				=Seek(m.SMART_INVOICE_NO,lcHeaderCursor,lcHeaderCursor)
				Replace Ship With Qty, ShipAmt With Amt In (lcHeaderCursor)
			Endif
			Select PRODACTI
		Endscan
		If llLineIssue
			Exit
		Endif
	Endscan
	If llLineIssue
		lnMsgRet = Messagebox("Some lines have no enough ordered quantity, cannot proceed." ,0,_Screen.Caption)
			   oAriaApplication.applicationhome = lcOldAppHome 
		Return .F.
	Endif


	If Reccount(lcLineCursor) > 0 And Reccount(lcHeaderCursor) > 0
		LCTRANCODE = oAriaApplication.remotecompanydata.BEGINTRAN(oAriaApplication.activecompanyconstr,3,'')
		If Type('lcTranCode') = 'N'
		    	   oAriaApplication.applicationhome = lcOldAppHome 
			Return .F.
		Endif
		lnResult=oAriaApplication.remotecompanydata.sqlupdate(lcLineCursor,LCTRANCODE,;
			SET("Datasession") ,"SMART_INVOICE_NO,LINENO",'SMART_INVOICE_LINES')
		If  lnResult < 1
			LLRETVALUE = (oAriaApplication.remotecompanydata.ROLLBACKTRAN(LCTRANCODE)=1)
			Select PRODACTI
			Tablerevert(.T.)
				   oAriaApplication.applicationhome = lcOldAppHome  
			Return .F.
		Else
			lnResult=oAriaApplication.remotecompanydata.sqlupdate(lcHeaderCursor ,LCTRANCODE,;
				SET("Datasession") ,"SMART_INVOICE_NO",'SMART_INVOICE_HEADER')
			If  lnResult < 1
				LLRETVALUE = (oAriaApplication.remotecompanydata.ROLLBACKTRAN(LCTRANCODE)=1)
				Select PRODACTI
				Tablerevert(.T.)
					   oAriaApplication.applicationhome = lcOldAppHome 
				Return .F.
			Else
				LLRETVALUE = (oAriaApplication.remotecompanydata.COMMITTRAN(LCTRANCODE)=1)
				Select PRODACTI
				Replace Invoiced With .T. Rest WHILE Account+Dtos(dprd_bgn)+Dtos(DPRD_END)+DEPT+Store+Style+TRAN_TYPE = lcAccount FOR cfileCode = lcFileCode
        WAIT CLEAR
        Messagebox("Smart invoices are generated successfully." ,0,_Screen.Caption)
                	   oAriaApplication.applicationhome = lcOldAppHome  
				Return .T.
			Endif
		Endif
	ENDIF
ENDIF 
	   oAriaApplication.applicationhome = lcOldAppHome 
Function lfAddLinesFromOrder
Lparameters lcAccountID,lcStyle,lcStore,laQtyArray,lnPriceSent
llReturnValue = .T.


lnCurAlias = Select(0)
Use (dbfspath +'ORDHDR.dbf') In 0 Shared Order ORDACCT   && ACCOUNT+CORDTYPE+ORDER
Use (dbfspath +'ORDLINE.dbf') In 0 Shared Order ORDLINST   && CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
Select ORDHDR
=Seek(lcAccountID+"O")
IF !USED('TempHdr')
  Select Order From ORDHDR Where Account+CORDTYPE+Order = lcAccountID+"O" And !(Status $ 'CX') And Multi ='Y' Order By entered Into Cursor 'TempHdr'
ENDIF  
Select 'TempHdr'
LOCATE 
If !Eof()
	Scan
		lcOrderNo = Order
		Select Ordline
		llIncludeOrder = .F.
		If Seek("O"+lcOrderNo+lcStore+lcStyle)
			Scan Rest While CORDTYPE+Order+Store+Style+Str(Lineno,6)="O"+lcOrderNo+lcStore+lcStyle For Ordline.TotQty > 0

				lnResult=oAriaApplication.remotecompanydata.SqlRun("Select SUM(Qty1) As QTY1,SUM(Qty2) As QTY2,SUM(Qty3) As QTY3,SUM(Qty4) As QTY4,SUM(Qty5) As QTY5,"+;
					"SUM(Qty6) As QTY6,SUM(Qty7) As QTY7,SUM(Qty8) As QTY8 from SMART_INVOICE_LINES INNER JOIN SMART_INVOICE_HEADER ON "+;
					"SMART_INVOICE_HEADER.SMART_INVOICE_NO = SMART_INVOICE_LINES.SMART_INVOICE_NO   Where [Order]= '"+;
					lcOrderNo +"' AND nordlineno = "+Str(Ordline.Lineno,6)+" AND STYLE = '"+Ordline.Style+;
					"' AND SMART_INVOICE_HEADER.INVOICE ='' and SMART_INVOICE_HEADER.STATUS !='C'","ORDERQTY" , ;
					"SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
				If lnResult<>1
					oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
					Wait Window 'SMART_INVOICE_HEADER'+'.SqlRun()'
					llReturnValue = .F.
					Exit
				Else

					Select 'ORDERQTY'
					=CursorSetProp("Buffering" ,3,'ORDERQTY')
					Select (lcLineCursor)
					Set Order To (lcLineCursor)
					=Seek(lcOrderNo +Ordline.Style+Str(Ordline.Lineno ,6))
					Sum  Qty1,Qty2,Qty3,QTY4,QTY5,QTY6,QTY7,QTY8 To Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8 Rest While  Order+Style+Str(nOrdlineNo,6)= lcOrderNo +Ordline.Style +Str(Ordline.Lineno,6)
					If  Reccount('ORDERQTY')> 0
						Select 'ORDERQTY'
						Replace Qty1 With Iif(Isnull(Qty1),0,Qty1) +  Q1,;
							Qty2 With Iif(Isnull(Qty2),0,Qty2 ) + Q2,;
							Qty3 With Iif(Isnull(Qty3),0,Qty3 ) + Q3,;
							QTY4 With Iif(Isnull(QTY4),0,QTY4 ) + Q4,;
							QTY5 With Iif(Isnull(QTY5),0,QTY5 ) + Q5,;
							QTY6 With Iif(Isnull(QTY6),0,QTY6) +  Q6,;
							QTY7 With Iif(Isnull(QTY7),0,QTY7)  + Q7,;
							QTY8 With Iif(Isnull(QTY8),0,QTY8 ) + Q8
					Else
						Select 'ORDERQTY'
						Append Blank
						Replace Qty1 With Q1,;
							Qty2 With Q2,;
							Qty3 With Q3,;
							QTY4 With Q4,;
							QTY5 With Q5,;
							QTY6 With Q6,;
							QTY7 With Q7,;
							QTY8 With Q8
					Endif
					Store 0 To m.Qty1,m.Qty2,m.Qty3,m.QTY4,m.QTY5,m.QTY6,m.QTY7,m.QTY8,m.TotQty,m.Price
					If Seek(m.Style,'STYLE','STYLE')
						m.Price = Style.PriceA
            IF Seek(m.Style,'STYLE','STYLE') AND !EMPTY(Style.cdisccode)
*!*	              DECLARE laDisRltFld[4,2]
*!*	              STORE '' TO lcDisType
*!*	              STORE {} TO ldstartDte, ldEndDate
*!*	              STORE 0  TO lnDisc_Pcnt
*!*	              laDisRltFld[1,1] = 'CCOSTAFECT'
*!*	              laDisRltFld[1,2] = 'lcDisType'
*!*	              laDisRltFld[2,1] = 'START'
*!*	              laDisRltFld[2,2] = 'ldstartDte'
*!*	              laDisRltFld[3,1] = 'DENDATE'
*!*	              laDisRltFld[3,2] = 'ldEndDate'
*!*	              laDisRltFld[4,1] = 'DISCPCNT'
*!*	              laDisRltFld[4,2] = 'lnDisc_Pcnt'
*!*	              =gfRltFld(Style.cdisccode, @laDisRltFld, 'CDISCCODE')
*!*	              IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(PRODACTI.dprd_bgn,ldstartDte,ldEndDate)
*!*	                m.Price = Style.PriceA * (100-lnDisc_Pcnt)/100  
*!*	              ENDIF
            ENDIF
					ENDIF 
					For lnY = 1 To 8
						lcY = Str(lnY,1)
						If (Ordline.Qty&lcY. - Iif(Reccount('ORDERQTY')> 0 And !Isnull(ORDERQTY.Qty&lcY.),ORDERQTY.Qty&lcY.,0)) > 0 And laQtyArray[lnY] > 0
							m.Qty&lcY. = Min(laQtyArray[lnY],(Ordline.Qty&lcY. - Iif(Reccount('ORDERQTY')> 0 And !Isnull(ORDERQTY.Qty&lcY.),ORDERQTY.Qty&lcY.,0)))
							laQtyArray[lnY] = Max(laQtyArray[lnY] - (Ordline.Qty&lcY. - Iif(Reccount('ORDERQTY')> 0 And !Isnull(ORDERQTY.Qty&lcY.),ORDERQTY.Qty&lcY.,0)) ,0)
							llIncludeOrder = .T.
							m.Order = lcOrderNo
							m.nOrdlineNo = Ordline.Lineno
							m.TotQty = m.TotQty +  m.Qty&lcY.
						Endif
					Endfor
					IF  m.TotQty > 0
						linNo = linNo + 1
						m.LineNo = linNo
            m.Price = lnPriceSent
            m.Gros_Price = lnPriceSent
            m.disc_pcnt = 0
						Insert Into (lcLineCursor) From Memvar
					ENDIF 
				Endif
				IF laQtyArray[1]+laQtyArray[2]+laQtyArray[3]+laQtyArray[4]+laQtyArray[5]+laQtyArray[6]+laQtyArray[7]+laQtyArray[8] = 0
					EXIT 
				ENDIF 
			Endscan
		Endif
		IF  laQtyArray[1]+laQtyArray[2]+laQtyArray[3]+laQtyArray[4]+laQtyArray[5]+laQtyArray[6]+laQtyArray[7]+laQtyArray[8] = 0
			EXIT 
		ENDIF 
		Select 'TempHdr'
	ENDSCAN 
	If laQtyArray[1]+laQtyArray[2]+laQtyArray[3]+laQtyArray[4]+laQtyArray[5]+laQtyArray[6]+laQtyArray[7]+laQtyArray[8] <> 0
    m.Price =  0
    If Seek(m.Style,'STYLE','STYLE')
      m.Price = Style.PriceA
      IF Seek(m.Style,'STYLE','STYLE') AND !EMPTY(Style.cdisccode)
*!*	        DECLARE laDisRltFld[4,2]
*!*	        STORE '' TO lcDisType
*!*	        STORE {} TO ldstartDte, ldEndDate
*!*	        STORE 0  TO lnDisc_Pcnt
*!*	        laDisRltFld[1,1] = 'CCOSTAFECT'
*!*	        laDisRltFld[1,2] = 'lcDisType'
*!*	        laDisRltFld[2,1] = 'START'
*!*	        laDisRltFld[2,2] = 'ldstartDte'
*!*	        laDisRltFld[3,1] = 'DENDATE'
*!*	        laDisRltFld[3,2] = 'ldEndDate'
*!*	        laDisRltFld[4,1] = 'DISCPCNT'
*!*	        laDisRltFld[4,2] = 'lnDisc_Pcnt'
*!*	        =gfRltFld(Style.cdisccode, @laDisRltFld, 'CDISCCODE')
*!*	        IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(PRODACTI.dprd_bgn,ldstartDte,ldEndDate)
*!*	          m.Price = Style.PriceA * (100-lnDisc_Pcnt)/100  
*!*	        ENDIF
      ENDIF
    ENDIF
    m.Order =''
    m.nOrdlineNo = 0
    m.TotQty = 0 
    For lnY = 1 To 8
      lcY = Str(lnY,1)
      m.Qty&lcY. = laQtyArray[lnY]
      m.TotQty = m.TotQty + laQtyArray[lnY]
    ENDFOR 
    linNo = linNo + 1
    m.LineNo = linNo
    m.Price = lnPriceSent
    m.Gros_Price = lnPriceSent
    m.disc_pcnt = 0
    Insert Into (lcLineCursor) From Memvar
	ENDIF
ELSE
  If laQtyArray[1]+laQtyArray[2]+laQtyArray[3]+laQtyArray[4]+laQtyArray[5]+laQtyArray[6]+laQtyArray[7]+laQtyArray[8] <> 0
    m.Price =  0
    If Seek(m.Style,'STYLE','STYLE')
*!*	      m.Price = Style.PriceA
*!*	      IF Seek(m.Style,'STYLE','STYLE') AND !EMPTY(Style.cdisccode)
*!*	        DECLARE laDisRltFld[4,2]
*!*	        STORE '' TO lcDisType
*!*	        STORE {} TO ldstartDte, ldEndDate
*!*	        STORE 0  TO lnDisc_Pcnt
*!*	        laDisRltFld[1,1] = 'CCOSTAFECT'
*!*	        laDisRltFld[1,2] = 'lcDisType'
*!*	        laDisRltFld[2,1] = 'START'
*!*	        laDisRltFld[2,2] = 'ldstartDte'
*!*	        laDisRltFld[3,1] = 'DENDATE'
*!*	        laDisRltFld[3,2] = 'ldEndDate'
*!*	        laDisRltFld[4,1] = 'DISCPCNT'
*!*	        laDisRltFld[4,2] = 'lnDisc_Pcnt'
*!*	        =gfRltFld(Style.cdisccode, @laDisRltFld, 'CDISCCODE')
*!*	        IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(PRODACTI.dprd_bgn,ldstartDte,ldEndDate)
*!*	          m.Price = Style.PriceA * (100-lnDisc_Pcnt)/100  
*!*	        ENDIF
*!*	      ENDIF
    ENDIF
    m.Order =''
    m.nOrdlineNo = 0
    m.TotQty = 0 
    For lnY = 1 To 8
      lcY = Str(lnY,1)
      m.Qty&lcY. = laQtyArray[lnY]
      m.TotQty = m.TotQty + laQtyArray[lnY]
    ENDFOR 
    linNo = linNo + 1
    m.LineNo = linNo
    m.Price = lnPriceSent
    m.Gros_Price = lnPriceSent
    m.disc_pcnt = 0
    Insert Into (lcLineCursor) From Memvar  
  ENDIF    
Endif
Select(lnCurAlias)
Use In ORDHDR
Use In Ordline
Return llReturnValue
