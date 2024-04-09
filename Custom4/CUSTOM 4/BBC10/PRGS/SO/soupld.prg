**:****************************************************************
*: Program file  : SOUPLD.PRG
*: Program desc. : Import Sales orders from Excel files
*: System        : Aria Apparel System - Version 4XP
*: Module        : Sales Order(SO)
*: Developer     : Moustafa Aboushady- [MAA]
*: Date          : 20/06/2017. (C202037)
***************************************************
Do Form (oAriaApplication.CLIENTSCREENHOME+ oAriaApplication.ActiveModuleID+ '\SOUPLD.SCX')
*Do Form ('x:\aria4xp\screens\so\SOUPLD.SCX')

*!*************************************************************
*! Name      : lfvgetFile
*: Developer     : Moustafa Aboushady- [MAA]
*: Date          : 20/06/2017.
*! Purpose   : Function to get the Excel file Path
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : = lfvgetFile()
*!*************************************************************
Function lfvgetFile
Private lcOld_Path

lcOld_Path = Fullpath('')

lcPathName  = Getfile('CSV', 'Excel sheet Path : ','Select')

Set Default To &lcOld_Path

If Empty(lcPathName)
  Return ''
Else
  Return lcPathName
Endif

*-- End Of lfvgetFile

*!*************************************************************
*! Name      : lfvProceed
*! Developer : Moustafa Aboushady (MAA)
*! Date      : 20/06/2017
*! Purpose   : To import the excel file
*!*************************************************************
*! Example            :  lfvProceed()
*!*************************************************************
Function lfvProceed
Parameters lcPathName,LOFORMSET

If Len(Alltrim(lcPathName)) = 1
  lcPathName = Alltrim(lcPathName)
Endif
If Empty(lcPathName)
  *Path name can not be empty.
  =gfModalGen('TRM04074B00000','DIALOG','Path name')
  Return
Endif

*-- Check if File Is .CSV file.
If !(".CSV" $ lcPathName)
  = gfModalGen('INM00389B00000','F','ALERT')
  Return
Endif

If !File(lcPathName)
  * "File does not exist. Cannot proceed."
  =gfModalGen('TRM00273B00000','DIALOG')
  Return
Endif

*Using Tables.

If !Used('pack_hdr')
  =gfOpenTable('pack_hdr','pack_hdr','SH','pack_hdr')
Endif


If !Used('ediacprt')
  =gfOpenTable('ediacprt','Accfact','SH','ediacprt')
Endif


If !Used('EDIPH')
  =gfOpenTable('EDIPH', 'partner', 'SH', 'EDIPH')
Endif


If !Used('EDINET')
  =gfOpenTable('EDINET', 'networkid', 'SH', 'EDINET')
Endif

If !Used('EDILIBDT')
  =gfOpenTable('EDILIBDT', 'Filetran', 'SH', 'EDILIBDT')
Endif

If !Used('EDILIBHD')
  =gfOpenTable('EDILIBHD', 'typecode', 'SH', 'EDILIBHD')
Endif


lcPathNameNew = Addbs(oAriaApplication.EDIPATH)+"OUTBOX\"
lcPathNameNew = Strtran(lcPathNameNew,'ARIA4XP','ARIA3EDI')
lcPathOutFolder = lcPathNameNew

lcPathNameNew = lcPathNameNew  + Justfname(lcPathName )

* OPen Excel
oExcel = Createobject("Excel.application")
oExcel.displayAlerts = .F.
oWorkbook=oExcel.Workbooks.Open(lcPathName)

* Delete header row
ORANGE = oWorkbook.SHEETS(1).Rows(1)
ORANGE.Delete()

* Loop until the bottom
xlSheet = oExcel.activesheet
nLastRow = oExcel.activesheet.UsedRange.Rows.Count

For I = 1 To nLastRow
  xlSheet.cells(I,5).NumberFormat = '@'
  If Empty(xlSheet.cells(I,2).Value)
    ORANGE = oWorkbook.SHEETS(1).Rows(I)
    ORANGE .Delete()
    Exit
  Endif
  Wait "Uploading Row #" + Str(I) Window Timeout 0.1
Endfor
*getting the Network outbox name
xlSheet.cells(1,1).NumberFormat = '@'
lcWare_code = Alltrim(xlSheet.cells(1,1).Value)
lcpartcode = ""
lcoutfile = ""
lcnetwork = ""

*!*	if seek(lcpack_no,'pack_hdr')
*!*	  lcwarehous = pack_hdr.cwarecode
If Seek("W"+lcWare_code, 'EDIACPRT')
  lcpartcode = EDIACPRT.cpartcode
  If Seek(lcpartcode, 'EDIPH')
    lcnetwork = EDIPH.cnetwork
    If Seek(lcnetwork, 'EDINET')
      *        STORE EDINET.coutfile TO lcoutfile
      lcoutfile = EDINET.coutfile
    Endif
    
  Endif
Endif
*!*	ENDIF


*Renaming the File to the Network
*Set Step On
*lcOut = SUBSTR(lcoutfile, 1, 6)

*lcOut = ALLTRIM(STRTRAN(alltrim(upper(lcoutfile)), '.CSV', ' '))
lcOut = Upper(Juststem(Alltrim(lcoutfile)))
lcOut1 = lcOut + '*'

*WAIT WINDOW "This is Lcout" + lcOut nowait
lcCurPath = 'cd '+Fullpath('')
lcPathOutFolder1  = 'cd '+lcPathOutFolder
&lcPathOutFolder1.

lncount = Adir(Net, lcOut1) + 1

&lcCurPath.

lcOut = lcOut + "_" + Alltrim(Str(lncount)) + ".CSV"

lcPathFileNameNew = lcPathOutFolder+ lcOut

*Getting Seq numbers.
Local oSeqObj
oSeqObj = Createobject('GetSequence')
lcFileCode = oSeqObj.Do('CFILECODE')

*Updating EDILIBDT
Select 'EDILIBDT'
For I=1 To nLastRow

  lcPoNum = str(xlSheet.cells(I,5).Value)
  Append Blank
  Replace cedifiltyp With 'S' ,;
    cfilecode  With lcFileCode  ,;
    cpartcode  With lcpartcode,;
    ceditrntyp With '943',;
    ceditranno With lcPoNum  ,;
    cediref WITH lcponum &&IIF(EMPTY(this.lcPoNum), ' ',this.lcPoNum)
Endfor

*Updating EDILIBHD
Select 'EDILIBHD'
Append Blank
Replace cfilecode  With lcFileCode  ,;
  cedifilnam With lcOut ,;
  cfilepath  With 'OutBox\' ,;
  cedifiltyp With 'S' ,;
  cnetwork   With lcnetwork

*Save AS lcPathNameNew

If Val(oExcel.Version) > 11
  oWorkbook.SaveAs(lcPathFileNameNew, 56) && xlExcel8
Else
  oWorkbook.SaveAs(lcPathFileNameNew)
Endif

oWorkbook.Close()
MESSAGEBOX("File Saved as: "  +lcPathFileNameNew ,6+512,_screen.caption)
*Wait Window "File Saved as: " + lcPathFileNameNew

*-- Creating a cursor that will hold the imported data from the
*-- excel file in order to append it in the master database file.
