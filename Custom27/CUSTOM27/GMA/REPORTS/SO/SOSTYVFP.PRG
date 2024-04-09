*!*	* This program sends the results of a Temp File    to 
*!*	* Microsoft Excel to calculate print styles and Price for 
*!*	* Sales Rep  IN GMA , we have a templete that Will add.

*-- lcRpDtDir  [gcDataDir]  && Save The Data Directory.
*-- lcRpWrDir  [gcWorkDir]  && Save The Work Directory.
*-- lcRpSysDir [gcSysHome]  && Save The system  Directory.
*-- lcRpRpDir  [gcRepHome]  && Save The report Directory.
*-- lcRpDefPth [gcDef_Path] && Save The DefPath.
*-- lcRpComp   [gcAct_Comp] && Save The Active Company .
*-- lcRpCmpNme [cCom_Name]  && Company Name.
*-- lcRpTmpFle [lcTemp]     && Save The TempName for the printed file.
*-- lcRpTitle  :- Custom Titel we will print in the Report.

LPARAMETERS lcMemoFile
IF TYPE('lcMemoFile')#'C'
	RETURN
ENDIF
RESTORE FROM (lcMemoFile) ADDI

LOCAL xlsheet,XLApp,tmpsheet

*-- Opren Temp File.
IF !USED(lcRpTmpFle)
	USE (ALLTRIM(lcRpWrDir)+lcRpTmpFle) ORDER 1 IN 0
ENDIF

tmpsheet = GetObject(ALLTRIM(lcRpDefPth)+'\Templete','excel.sheet')

XLApp = tmpsheet.application
XLApp.WorkBooks.Add(ALLTRIM(lcRpDefPth)+'\Templete')

XLSheet = XLApp.ActiveSheet
    
*-- Change the Company Name if Not 
IF lcRpComp = '01'
  XLSheet.Cells(1,1).Value      =   "CAPELLI OF NEW YORK"
ELSE
  XLSheet.Cells(1,1).Value      = lcRpCmpNme  
ENDIF

XLSheet.Cells(1,1).Font.Name  = "Arial"
XLSheet.Cells(1,1).Font.Size  = 13
XLSheet.Cells(1,1).Font.FontStyle = "Bold"
 
*B605849,1 HBG 05/06/2002 Print date and the Page # in the header of the report [Begin]
*-- Print Date
XLSheet.Cells(1,13).Value      = ldRpSysDat 
XLSheet.Cells(1,13).Font.Name  = "Arial"
XLSheet.Cells(1,13).Font.Size  = 10
XLSheet.Cells(1,13).Font.FontStyle = "Reguler"
XLSheet.Cells(1,13).NumberFormat = "d-mmm-yyyy"
lnPageNo = 1
XLSheet.Cells(2,13).Value      = lnPageNo 
XLSheet.Cells(2,13).Font.Name  = "Arial"
XLSheet.Cells(2,13).Font.Size  = 10
XLSheet.Cells(2,13).Font.FontStyle = "Reguler"
XLSheet.Cells(2,13).NumberFormat = "General"
*B605849,1 [End]

*-- Print Report Titel
XLSheet.Cells(2,6).Value      = lcRpTitle
XLSheet.Cells(2,6).Font.Name  = "Arial"
XLSheet.Cells(2,6).Font.Size  = 11
XLSheet.Cells(2,6).Font.FontStyle = "Bold"
*-- Print Company Address & Phone & Fax.
XLSheet.Cells(4,1).Value = laRpCpAdd1
XLSheet.Cells(5,1).Value = laRpCpAdd3
XLSheet.Cells(6,1).Value = lcRpCPhone
XLSheet.Cells(7,1).Value = lcRpCFax

*-- Print the Warehouse address. 
XLSheet.Cells(4,4).Value = laRpWrAdd1
XLSheet.Cells(5,4).Value = laRpWrAdd3
XLSheet.Cells(6,4).Value = lcRpWRPhne
XLSheet.Cells(7,4).Value = lcRpWRFx

*B605849,1 HBG 05/06/2002 Print Dept & Group[Begin]
XLSheet.Cells(19,3).Value = lcRpDept
XLSheet.Cells(20,3).Value = lcRpGroup
*B605849,1 [End]

SELECT (lcRpTmpFle)
LOCATE
lnRow = 22
*B605849,1 HBG 05/06/2002 Add variable to get # of pages [Begin]
lnRowNo = 22
*B605849,1[End]
SCAN
  *-- This lines to add new line.
  XLSheet.ROWS(lnRow).INSERT
  XLSheet.Cells(lnRow,1).Value = cStyMajor
  XLSheet.Cells(lnRow,2).Value = COLOR
  XLSheet.Cells(lnRow,3).Value = desc1
  XLSheet.Cells(lnRow,8).Value = pricea
  XLSheet.Cells(lnRow,10).Value = [=(H] + ALLTRIM(STR(lnRow,6)) + [*I] + ALLTRIM(STR(lnRow,6))+')'
  *B605849,1 HBG 05/06/2002 Print Page # in the each page [Begin]
  IF lnRowNo = 38
    XLSheet.Cells(lnRow,12).Value      = "Date   :"
    XLSheet.Cells(lnRow,12).Font.Name  = "Arial"
    XLSheet.Cells(lnRow,12).Font.Size  = 10
    XLSheet.Cells(lnRow,12).Font.FontStyle = "Bold"
    XLSheet.Cells(lnRow,13).Value      = ldRpSysDat 
    XLSheet.Cells(lnRow,13).Font.Name  = "Arial"
    XLSheet.Cells(lnRow,13).Font.Size  = 10
    XLSheet.Cells(lnRow,13).Font.FontStyle = "Reguler"
    XLSheet.Cells(lnRow,13).NumberFormat = "d-mmm-yyyy"   
  ENDIF
  IF lnRowNo > 38
    lnRowNo  = 1
    lnPageNo = lnPageNo + 1
    XLSheet.Cells(lnRow,12).Value      = "Page # :"
    XLSheet.Cells(lnRow,12).Font.Name  = "Arial"
    XLSheet.Cells(lnRow,12).Font.Size  = 10
    XLSheet.Cells(lnRow,12).Font.FontStyle = "Bold"
    XLSheet.Cells(lnRow,13).Value      = lnPageNo 
    XLSheet.Cells(lnRow,13).Font.Name  = "Arial"
    XLSheet.Cells(lnRow,13).Font.Size  = 10
    XLSheet.Cells(lnRow,13).Font.FontStyle = "Reguler"
    XLSheet.Cells(lnRow,13).NumberFormat = "General"
  ENDIF
  lnRowNo = lnRowNo + 1
  *B605849,1 [End]
  lnRow = lnRow + 1
  *-- Add new Line.
ENDSCAN

*-- =SUM(I23:I25)
XLSheet.Cells(lnRow+1,9).Value = [=SUM(I] + ALLTRIM(STR(22,6)) + [:I] + ALLTRIM(STR(lnRow-1,6))+')'
XLSheet.Cells(lnRow+1,10).Value = [=SUM(J] + ALLTRIM(STR(22,6)) + [:J] + ALLTRIM(STR(lnRow-1,6))+')'
XLApp.Visible = .T.
