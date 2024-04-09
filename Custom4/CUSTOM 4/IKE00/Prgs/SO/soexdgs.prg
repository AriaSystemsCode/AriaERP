*:***************************************************************************
*: Program file  : ARINVTK.PRG
*: Program desc. : Custom Program to import invoice tracking number in invoice note
*: System        : Aria Advantage Series.
*: Module        : AR
*: Developer     : Abdelrahman Esam(AEG)  C201785(T20151014.0017)
*:***************************************************************************
*:***************************************************************************

*!*Try
*!*	Set Step On
	Do Form (oAriaApplication.clientscreenhome+"SO\soexdgs.scx")

*!*Catch
*!*Endtry



*!*************************************************************
*! Name      : lfInitPoForm
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 02/16/2016
*! Purpose   : Init Method of the Form
*!*************************************************************
Function lfInitPoForm
Lparameters loFormSet
Set Multilocks On
If !Used('poshdr')
	llT  =gfOpenTable('poshdr','poshdr','SH','poshdr')
Endif

If !Used('posln')
	llT  =gfOpenTable('posln','posln','SH','posln')
Endif

If !Used('styleupc')
	llT  =gfOpenTable('styleupc','STYLEUPC','SH','styleupc')
Endif


If !Used('prodacti')
	llT  =gfOpenTable('prodacti','ACTIPRD','SH','PRODACTI')
Endif

If !Used('style')
	llT  =gfOpenTable('style','Style','SH','style')
Endif

If !Used('piktkt')
	llT  =gfOpenTable('piktkt','PIKTKT','SH','piktkt')
Endif

If !Used('scale')
	llT  =gfOpenTable('SCALE','SCALE','SH','SCALE')
Endif



With loFormSet.AriaForm1
	.dtpFrom.Enabled = .T.
	.dtpFrom.text1.Enabled = .T.
	.dtpFrom.text1.Value = ""
	.dtpFrom.cmdMonth.Enabled = .T.
	.dtpTo.Enabled = .T.
	.dtpTo.text1.Enabled = .T.
	.dtpTo.text1.Value = ""
	.dtpTo.cmdMonth.Enabled = .T.
	.dtpTo.Enabled = .T.
	.cmdGenEx.Enabled=.T.
Endwith

loFormSet.lcTmpImp = gfTempName()
Create Cursor (loFormSet.lcTmpImp) (Transaction_Type C(20), Store C(8),Style C(19), Size C(8), UPC C(18),;
	QTY N(6), Date D, whole_Sale_Price N(12,2),  retail_price N(12,2))
Select (loFormSet.lcTmpImp)
Index On Transaction_Type+Style+Size+Store Tag (loFormSet.lcTmpImp)

*!*************************************************************
*! Name      : lfChangeMod
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 02/14/2016
*! Purpose   : Change  mode
*!*************************************************************
Function lfChangeMod
Lparameters loFormSet

With loFormSet.AriaForm1
	.dtpFrom.Enabled = .T.
	.dtpFrom.text1.Enabled = .T.
	.dtpFrom.text1.Value = ""
	.dtpFrom.cmdMonth.Enabled = .T.
	.dtpTo.Enabled = .T.
	.dtpTo.text1.Enabled = .T.
	.dtpTo.text1.Value = ""
	.dtpTo.cmdMonth.Enabled = .T.
	.dtpTo.Enabled = .T.
	.cmdGenEx.Enabled=.T.
Endwith



*!*************************************************************
*! Name      : lfGenExc
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 02/11/2016
*! Purpose   : Start Import invoice tracking numebr
*!*************************************************************
Function lfGenExc
Lparameters loFormSet



SET STEP ON 

ldFrom=loFormSet.AriaForm1.dtpFrom.text1.Value
ldTo=loFormSet.AriaForm1.dtpTo.text1.Value
lcFrom=IIF(EMPTY(ldFrom),'',Dtos(ldFrom))
lcTo=IIF(EMPTY(ldTo),'',Dtos(ldTo))
lcAnt='DOLLR'

IF TYPE('ldFrom ')='D' AND TYPE('ldTo')='D' AND  ldFrom > ldTo
	=gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Invalid Date Range')
	Return
Endif
lcDir=GETFILE('CSV')

lcSelPos="Select * from poshdr "
lcCondPos="WHERE  cbusdocu = 'N' AND cstytype = 'N' AND cpiktkt <> '' AND entered IS NOT NULL AND cwarecode = '' "


If !Empty(lcDir)

  
	If Empty(ldFrom) .And. Empty(ldTo)
		Select * From prodacti Where account=lcAnt Into Cursor TMPPROD
	Endif

	If !Empty(ldFrom) .And. Empty(ldTo)
		lcCondPos = lcCondPos + " AND entered > '"+lcFrom+"'"
		Select * From prodacti Where account=lcAnt And dprd_bgn > ldFrom Into Cursor TMPPROD
	Endif

	If Empty(ldFrom) .And. !Empty(ldTo)
		lcCondPos = lcCondPos + " AND entered < '"+lcTo+"'"
		Select * From prodacti Where account=lcAnt And dprd_bgn < ldTo Into Cursor TMPPROD
	Endif

	If !Empty(ldFrom) .And. !Empty(ldTo)
		lcCondPos = lcCondPos + " AND entered BETWEEN '"+lcFrom+ "' AND '"+lcTo+"'"
		Select * From prodacti Where account=lcAnt And dprd_bgn > ldFrom .And. dprd_bgn < ldTo Into Cursor TMPPROD
	Endif

	llPoRet=GFSQLRUN(lcSelPos+lcCondPos , 'poshdr',.F.,'TMPPOSH')
    Select (loFormSet.lcTmpImp)
	
   SET STEP ON
	Select TMPPOSH
	Go Top
	If !Eof()
      WAIT WINDOW "Collecting Data ..." nowait
		Scan
			If Seek(cpiktkt,'piktkt','PIKTKT') .And. piktkt.Status='C'
				llPoRet=GFSQLRUN("Select * from posln where po ='"+TMPPOSH.PO+"' and Trancd ='2'"  , 'posln',.F.,'TMPPOSl')
				If llPoRet
					Select TMPPOSl
					Go Top
					If !Eof()
						SCAN

							If Seek(Style,'style' ,'style')  .And. Seek('S'+Style.Scale,'scale','scale')
								lnQTYV = 0
								lcUPC=""
								lncnt=1
								Do While lncnt<=Scale.Cnt
									If Seek(Style+Alltrim(Str(lncnt)),'styleupc','styleupc')
										lcUPC=styleupc.cupcnum1+styleupc.cupcnum2+styleupc.cupcnum3
									Else
										lcUPC=""
									Endif
									lnQTYV=Evaluate("TMPPOSl.qty"+Alltrim(Str(lncnt)))
									If lnQTYV >0
										Select (loFormSet.lcTmpImp)
										Append Blank
										Replace Transaction_Type With "Interlocation" , Store With TMPPOSl.cwarecode ,Style With TMPPOSl.Style ,;
											Size With EVALUATE("scale.sz"+Alltrim(Str(lncnt))) , UPC With Alltrim(lcUPC),QTY With lnQTYV , whole_Sale_Price With Style.pricea ,;
											retail_price With Style.nsugretpri , Date With TMPPOSH.entered

									Endif

									lncnt=lncnt+1
								Enddo
							Endif
						Endscan
					Endif
				Endif
			Endif
		Endscan
	Endif

	Set Step On
	Select TMPPROD
	Go Top
	If !Eof()
	 WAIT WINDOW "Checking Product activity" nowait
		SCAN FOR INLIST(tran_type,'QU','QS')
			If Seek(Style,'style' ,'style')  .And. Seek('S'+Style.Scale,'scale','scale')
			
				lnQTYV = 0
				lcUPC=""
				lncnt=1
				Do While lncnt<=Scale.Cnt
			
					If Seek(Style+Alltrim(Str(lncnt)),'styleupc','styleupc')
						lcUPC=styleupc.cupcnum1+styleupc.cupcnum2+styleupc.cupcnum3
					Else
						lcUPC=""
					Endif
					lnQTYV=Evaluate("TMPPROD.nactiqty"+Alltrim(Str(lncnt)))
					If lnQTYV <> 0 
						Select (loFormSet.lcTmpImp)
						
						Append Blank
						Replace Transaction_Type With IIf(TMPPROD.tran_type='QU' , "Return" ,"Sales") , Store With TMPPROD.Store ,Style With TMPPROD.Style ,;
							Size With EVALUATE("scale.sz"+Alltrim(Str(lncnt))) , UPC With Alltrim(lcUPC),QTY With IIF(TMPPROD.tran_type='QU',lnQTYV,lnQTYV*-1) , whole_Sale_Price With Style.pricea ,;
							retail_price With Style.nsugretpri , Date With TMPPROD.dprd_bgn
					Endif
					lncnt=lncnt+1
				Enddo
			Endif
		Endscan
	Endif

	Select (loFormSet.lcTmpImp)
	SET DELETED ON
    locate	
	If Reccount() <= 0
		=gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'No records exist.')
		Return
	ENDIF

	
	

*!*		IF FILE(lcDir+"DG stores activities.XLS") 
*!*	     ERASE (lcDir+"DG stores activities.XLS")
*!*	   ENDIF
    WAIT WINDOW "Generating Excel Sheet ..." nowait
    COPY TO (lcDir) TYPE CSV
*!*	LOCAL oExcel, lnActiveAlias, lnRowRef, lcCelRef, lcCellValue
*!*	   lnActiveAlias = SELECT(0)
*!*		oExcel = CreateOBJECT("Table2Excel",lcDir+"DG stores activities.CSV")
*!*	   IF VARTYPE(oExcel) != "O" OR ISNULL(oExcel)
*!*	     WAIT CLEAR 
*!*	     =gfModalGen("INM00406B00000","DIALOGE")  
*!*	     RETURN .F.
*!*	   ENDIF
*!*	   oExcel.XLSFORMAT = 'XLSX'
*!*	   oExcel.Save()                 && Save this sheet.
*!*	   oExcel.Close()                && Close the sheet.
*!*	   oExcel = .NULL.
*!*	   
*!*	   IF FILE(lcDir+"DG stores activities.CSV")
*!*	     =gfModalgen("INM00000B00000","DIALOG",.F.,.F.,"File: DG stores activities.CSV has been created successfully")
*!*	   ENDIF
	
	 =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Excel file generated successfully')
	DELETE ALL
	
Endif




*!*************************************************************
*! Name      : lfDestroy
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 02/14/2016
*! Purpose   : destroy import tracking screen
*!*************************************************************
Function lfDestroy
Lparameters loFormSet

If Used('poshdr')
	Use In poshdr
Endif

If Used('posln')
	Use In posln
Endif


If Used('styleupc')
	Use In styleupc
Endif

If Used('prodacti')
	Use In prodacti
Endif

If Used('style')
	Use In Style
Endif

If Used('loFormSet.lcTmpImp')
	Use In loFormSet.lcTmpImp
Endif


If Used('TMPPOSH')
	Use In TMPPOSH
Endif

If Used('TMPPROD')
	Use In TMPPROD
Endif

If Used('TMPPOSl')
	Use In TMPPOSl
Endif

If Used('piktkt')
	Use In piktkt
Endif

If Used('scale')
	Use In Scale
Endif
