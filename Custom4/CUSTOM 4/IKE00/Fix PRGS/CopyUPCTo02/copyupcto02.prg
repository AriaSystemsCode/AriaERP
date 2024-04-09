**************************************************************************************************
*T20151111.0002	Copy UPCs for the styles with the same style code from company 01 to company 02
* Developer : Sara Osama
*!*	1. Select each style in 02.(style.dbf)
*!*	2. Get its scale id in 02.(scale.dbf)
*!*	3. Search for each style + size in 02.(styleUPC.dbf)
*!*	4. If there is any missing style + size in 02 in "styleUPC.dbf"
*!*	5. Search for that record in 01 company in "styleUPC.dbf" and get copy from it
*!*	5.1. If not found in 01 add that style in the log file.
*!* 6. Make sure that UPC isn't exist in 02 company in "styleUPC.dbf" table.
*!* 7. Paste the record to 02 company in "styleUPC.dbf" table.
*!*	7. Repeate the above steps for each style in 02 company.


**************************************************************************************************
* The following code
Parameters lcSysfilespath, lcFromCompanyID, lcToCompanyID

If Type('lcFromCompanyID')='C' And !Empty(Alltrim(lcFromCompanyID))
Else
	lcFromCompanyID = '01'
Endif
If Type('lcToCompanyID')='C' And !Empty(Alltrim(lcToCompanyID))
Else
	lcToCompanyID= '02'
Endif

lnCnt = 0
lnAbort = 0

*lnStart = DATETIME()
*?lnStart

Close Databases
Set Exclusive Off
Set Deleted On
Set Cpdialog Off
Set Safety Off

lcSyspath = ''
lcSyspath = Getdir('','Sysfiles Path')
If Empty(lcSyspath) Or !File(lcSyspath+'SYCCOMP.dbf')
	Messagebox('Invalid Sysfiles Path')
	Return
Endif


*!*	IF EMPTY(lcFromCompanyID)
*!*	  MESSAGEBOX('Pls specifiy the Co. id as a prarmeter to the program')
*!*	  RETURN
*!*	ELSE
*!*
*!*	  lcMsg = 'run a fix data on the Co. &lcFromCompanyID to replace the *** in BOM.citmmask with the scale value'
*!*	  MESSAGEBOX('Pls confirm that you will '+lcMsg)
*!*	  IF MESSAGEBOX('Are you sure you want to '+lcMsg,4+256)<>6
*!*	    RETURN
*!*	  ENDIF
*!*	ENDIF


*- connect to sql server database
Select 0

lcFromCompanyPath = ""
Use (lcSyspath+'SYCCOMP') Order 1 SHARED 
If Seek( lcFromCompanyID )
	lcFromCompanyPath = Addbs(Alltrim(SYCCOMP.ccom_ddir))

	If lcFromCompanyPath == "" Or !Directory(lcFromCompanyPath)
		Messagebox('Unable To Connect To Company '+lcFromCompanyID +' Database')
		Return
	Endif
Else
	Messagebox('Company ID'+lcFromCompanyID +' Not Found!')
	Return
Endif


If Seek(lcToCompanyID)
	lcToCompanyPath = Addbs(Alltrim(SYCCOMP.ccom_ddir))

	If lcToCompanyPath == "" Or !Directory(lcToCompanyPath)
		Messagebox('Unable To Connect To Company '+lcToCompanyID+' Database')
		Return
	Endif
Else
	Messagebox('Company ID'+lcToCompanyID+' Not Found!')
	Return
Endif

*Company 02
*Use Style
USE (lcToCompanyPath +'STYLE.DBF') In 0 Shared Alias STYLETO Order Style

*Use Scale
USE (lcToCompanyPath +'scale.DBF') In 0 Shared Alias ScaleTo Order Scale

*Use Style UPC
USE (lcToCompanyPath +'STYLEUPC.DBF') In 0 Shared Alias STYLEUPCTo Order STYLEUPC

*Company 01
*Use Style UPC
USE (lcFromCompanyPath +'STYLEUPC.DBF') In 0 Shared Alias STYLEUPCFrom Order STYLEUPC

STRTOFILE("Errors Found during copying UPCs:"+CHR(13)+CHR(10),ADDBS(lcToCompanyPath)+"ErrorUPC.txt",0)
STRTOFILE("Copied UPCs:"+CHR(13)+CHR(10),ADDBS(lcToCompanyPath)+"SuccessUPC.txt",0)

Select STYLETo
Locate
Wait Window  "Start copying" Nowait
Scan
	Wait Window  "Get scale of style "+ STYLETo.Style +"in 02 company." Nowait
	If Seek('S'+STYLETo.Scale, "ScaleTo" )

		For I = 1 To Scaleto.Cnt
			Wait Window  "Search for UPC of style "+STYLETo.Style+ " with size "+Alltrim(Str(I))+"in 02 company." Nowait
			If !Seek(STYLETo.Style+Alltrim(Str(I)),"STYLEUPCTo" )
				Wait Window  "The UPC of style "+STYLETo.Style+ " with size "+Alltrim(Str(I))+"Not found in 02 company." Nowait
				* If seek() company 01
				If Seek(STYLETo.Style+Alltrim(Str(I)),"STYLEUPCFrom" )

					Wait Window  "Copy the UPC of style "+STYLETo.Style+ " with size "+Alltrim(Str(I))+"from 01 company to 02 company." Nowait
					SELECT STYLEUPCFrom
					Scatter Memvar Memo
					If Seek(STYLEUPCFrom.CUPCNUM1+STYLEUPCFrom.CUPCNUM2+STYLEUPCFrom.CUPCNUM3,'STYLEUPCTo','STYUPCN')
						* write error log
						* Write log file
						STRTOFILE("The UPC of Style#:"+STYLETo.Style+ " with size " +Alltrim(Str(I))+" is already existed in 02 Company."+Chr(13)+Chr(10),ADDBS(lcToCompanyPath)+"ErrorUPC.txt",1)
						* End Write log file
					
					Else
						
						*Scatter Memvar Memo
						* append to cmopany 02
*!*							Select STYLEUPCTo

*!*							Append Blank
*!*							Gather Memvar Memo
						INSERT INTO STYLEUPCTo FROM MEMVAR
						* Write log file
						STRTOFILE("Style#:"+STYLETo.Style+" with Size " +Alltrim(Str(I))+" is Successfully copied from 01 company to 02 Company."+Chr(13)+Chr(10),ADDBS(lcToCompanyPath)+"SuccessUPC.txt",1)
						* End Write log file
						* Write log file by append
					Endif

*!*						Scatter Memvar Memo
*!*						* append to cmopany 02
*!*						Select STYLEUPCTo

*!*						Append Blank
*!*						Gather Memvar Memo
					* Write log file by append

				Else
					Wait Window  "The UPC of style "+STYLETo.Style+ " with size "+Alltrim(Str(I))+" not found in 01 company." Nowait
					* Write log file
					STRTOFILE("Style#:"+STYLETo.Style+ " with size " +Alltrim(Str(I))+" not found in 01 Company."+Chr(13)+Chr(10),ADDBS(lcToCompanyPath)+"ErrorUPC.txt",1)
					* End Write log file
				Endif
			Endif
		Endfor
	Endif
Endscan
CLOSE All

Messagebox('Copying UPCs is Done and Log files are found in '+ALLTRIM(ADDBS(lcToCompanyPath)))

* Display erros
* Display append

*************************************

