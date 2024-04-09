*:************************************************************************
*: Program file  : MAMatVFP.PRG 
*: Program desc. : Material Req.
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: MATERIALS (MA).
*:      Developer: Mohamed Shokry (MHM)
*:************************************************************************
*: Calls         : 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example       : DO MAMATVFP
*:************************************************************************
*!*	* This program sends the results of a Temp File    to 
*!*	* Microsoft Excel to calculate Material req for GMA.

*-- lcRpDtDir  [gcDataDir]  && Save The Data Directory.
*-- lcRpWrDir  [gcWorkDir]  && Save The Work Directory.
*-- lcRpSysDir [gcSysHome]  && Save The system  Directory.
*-- lcRpRpDir  [gcRepHome]  && Save The report Directory.
*-- lcRpDefPth [gcDef_Path] && Save The DefPath.
*-- lcRpComp   [gcAct_Comp] && Save The Active Company .
*-- lcRpCmpNme [cCom_Name]  && Company Name.
*-- lcRpTmpFl1 [lcTemp]     && Save The TempName for the First printed file.
*-- lcRpTmpFl2 [lcTemp]     && Save The TempName for the First printed file.
*-- lcRpTmpFl3 [lcTemp]     && Save The TempName for the First printed file.
*-- lcRpWareHs  [lcWareHos]  && Save WareHouse 
*-- lcRpTitle  :- Custom Titel we will print in the Report.
*-- lcRpCounPo :- countr for the inserted cols
LPARAMETERS lcMemoFile
IF TYPE('lcMemoFile')#'C'
	RETURN
ENDIF
RESTORE FROM (lcMemoFile) ADDI

LOCAL xlsheet,XLApp,tmpsheet

*-- Opren Temp File.
IF !USED(lcRpTmpFl1)
	USE (ALLTRIM(lcRpWrDir)+lcRpTmpFl1) ORDER 1 IN 0
ENDIF

IF !USED(lcRpTmpFl2)
	USE (ALLTRIM(lcRpWrDir)+lcRpTmpFl2) ORDER 1 IN 0
ENDIF

tmpsheet = GetObject(ALLTRIM(lcRpDefPth)+'\Templete1','excel.sheet')

XLApp = tmpsheet.application
XLApp.WorkBooks.Add(ALLTRIM(lcRpDefPth)+'\Templete1')

XLSheet = XLApp.ActiveSheet
    
*-- Change the Company Name if Not 
XLSheet.Cells(1,4).Value      =   "Material Req. By Vendor"
XLSheet.Cells(2,5).Value      =   "Detail Format"

XLSheet.Cells(1,4).Font.Name  = "Arial"
XLSheet.Cells(1,4).Font.Size  = 13
XLSheet.Cells(1,4).Font.FontStyle = "Bold"

XLSheet.Cells(2,5).Font.Name  = "Arial"
XLSheet.Cells(2,5).Font.Size  = 13
XLSheet.Cells(2,5).Font.FontStyle = "Bold"

XLSheet.Cells(5,1).Value      = "Warehouse"
XLSheet.Cells(5,1).Font.Name  = "Arial"
XLSheet.Cells(5,1).Font.Size  = 13
XLSheet.Cells(5,1).Font.FontStyle = "Bold"

XLSheet.Cells(5,2).Value      = lcRpWareHs
XLSheet.Cells(5,2).Font.Name  = "Arial"
XLSheet.Cells(5,2).Font.Size  = 13
XLSheet.Cells(5,2).Font.FontStyle = "Reguler"


XLSheet.Cells(3,4).Value      = lcRpTitle
XLSheet.Cells(3,4).Font.Name  = "Arial"
XLSheet.Cells(3,4).Font.Size  = 11
XLSheet.Cells(3,4).Font.FontStyle = "Bold"

XLSheet.Cells(6,1).Value      = "Fabric/Color"
XLSheet.Cells(6,1).Font.Name  = "Arial"
XLSheet.Cells(6,1).Font.Size  = 11
XLSheet.Cells(6,1).Font.FontStyle = "Bold"

XLSheet.Cells(6,2).Value      = "Stock"
XLSheet.Cells(6,2).Font.Name  = "Arial"
XLSheet.Cells(6,2).Font.Size  = 11
XLSheet.Cells(6,2).Font.FontStyle = "Bold"

XLSheet.Cells(7,1).Value      = "PO# ->"
XLSheet.Cells(7,1).Font.Name  = "Arial"
XLSheet.Cells(7,1).Font.Size  = 11
XLSheet.Cells(7,1).Font.FontStyle = "Bold"

XLSheet.Cells(8,1).Value      = "Vendor"
XLSheet.Cells(8,1).Font.Name  = "Arial"
XLSheet.Cells(8,1).Font.Size  = 11
XLSheet.Cells(8,1).Font.FontStyle = "Bold"

XLSheet.Cells(9,1).Value      = "Status"
XLSheet.Cells(9,1).Font.Name  = "Arial"
XLSheet.Cells(9,1).Font.Size  = 11
XLSheet.Cells(9,1).Font.FontStyle = "Bold"

lnCol = 3
SELECT(lcRpTmpFl2)
LOCATE
SCAN
  
  XLSheet.Cells(6,lnCol).Value = "Req."
  XLSheet.Cells(6,lnCol).Font.Name  = "Arial"
  XLSheet.Cells(6,lnCol).Font.Size  = 11
  XLSheet.Cells(6,lnCol).Font.FontStyle = "Bold"
  
  XLSheet.Cells(7,lnCol).Value = &lcRpTmpFl2..PO
  XLSheet.Cells(7,lnCol).Font.Name  = "Arial"
  XLSheet.Cells(7,lnCol).Font.Size  = 11
  XLSheet.Cells(7,lnCol).Font.FontStyle = "Regular"
  
  XLSheet.Cells(8,lnCol).Value = &lcRpTmpFl2..Vendor
  XLSheet.Cells(8,lnCol).Font.Name  = "Arial"
  XLSheet.Cells(8,lnCol).Font.Size  = 11
  XLSheet.Cells(8,lnCol).Font.FontStyle = "Regular"

  XLSheet.Cells(9,lnCol).Value = &lcRpTmpFl2..Status
  XLSheet.Cells(9,lnCol).Font.Name  = "Arial"
  XLSheet.Cells(9,lnCol).Font.Size  = 11
  XLSheet.Cells(9,lnCol).Font.FontStyle = "Regular"
  
  lnCol = lnCol+1

  XLSheet.Cells(6,lnCol).Value = "Issued."
  XLSheet.Cells(6,lnCol).Font.Name  = "Arial"
  XLSheet.Cells(6,lnCol).Font.Size  = 11
  XLSheet.Cells(6,lnCol).Font.FontStyle = "Bold"
  
  XLSheet.Cells(7,lnCol).Value = &lcRpTmpFl2..PO
  XLSheet.Cells(7,lnCol).Font.Name  = "Arial"
  XLSheet.Cells(7,lnCol).Font.Size  = 11
  XLSheet.Cells(7,lnCol).Font.FontStyle = "Regular"
  
  XLSheet.Cells(8,lnCol).Value = &lcRpTmpFl2..Vendor
  XLSheet.Cells(8,lnCol).Font.Name  = "Arial"
  XLSheet.Cells(8,lnCol).Font.Size  = 11
  XLSheet.Cells(8,lnCol).Font.FontStyle = "Regular"
  
  XLSheet.Cells(9,lnCol).Value = &lcRpTmpFl2..Status
  XLSheet.Cells(9,lnCol).Font.Name  = "Arial"
  XLSheet.Cells(9,lnCol).Font.Size  = 11
  XLSheet.Cells(9,lnCol).Font.FontStyle = "Regular"
  
  lnCol = lnCol+1

ENDSCAN

XLSheet.Cells(6,lnCol).Value = "Balance"
XLSheet.Cells(6,lnCol).Font.Name  = "Arial"
XLSheet.Cells(6,lnCol).Font.Size  = 11
XLSheet.Cells(6,lnCol).Font.FontStyle = "Bold"

lnCol = lnCol+1
XLSheet.Cells(6,lnCol).Value = "On Order"
XLSheet.Cells(6,lnCol).Font.Name  = "Arial"
XLSheet.Cells(6,lnCol).Font.Size  = 11
XLSheet.Cells(6,lnCol).Font.FontStyle = "Bold"

lnCol = lnCol+1
XLSheet.Cells(6,lnCol).Value = "Avlbl"
XLSheet.Cells(6,lnCol).Font.Name  = "Arial"
XLSheet.Cells(6,lnCol).Font.Size  = 11
XLSheet.Cells(6,lnCol).Font.FontStyle = "Bold"

SELECT (lcRpTmpFl1)
LOCATE
lnRow = 11
SCAN

  XLSheet.ROWS(lnRow).INSERT
  XLSheet.Cells(lnRow,1).Value = FabrColr
  XLSheet.Cells(lnRow,2).Value = NSTOCK
  lnColCont = 3
  *--mhm 606523,1
  *FOR lnCount = 1 TO lcRpCounPo
  *  lcCount = ALLTRIM(STR(lnCount,5))
  *  lcColCont = ALLTRIM(STR(lnColCont,5))
    *WAIT WINDOW "Test Three " +lcCount
    *XLSheet.Cells(lnRow,lnColCont+lnCount-1).Value = NREQ&lcCount
    *XLSheet.Cells(lnRow,lnColCont+lnCount ).Value = NISSU&lcCount
    *lnColCont = lnColCont + 2
   * XLSheet.Cells(lnRow,lnColCont+(lnCount-1)*2).Value = NREQ&lcCount
   * XLSheet.Cells(lnRow,lnColCont+lnCount*2 ).Value = NISSU&lcCount
  *ENDFOR  
  *lnColCont = lnColCont  
  *XLSheet.Cells(lnRow,lnColCont).Value      = NBALANCE
  *XLSheet.Cells(lnRow,lnColCont+1).Value      = NONORDER
  *XLSheet.Cells(lnRow,lnColCont+2).Value      = NAVALABL
  SELECT(lcRpTmpFl2)
  lnCount = 1
  SCAN
    LcPo = Po
    SELECT (lcRpTmpFl1)  
    XLSheet.Cells(lnRow,lnColCont +(lnCount*2)-1).Value = NREQ&LcPo
    XLSheet.Cells(lnRow,lnColCont +lnCount*2 ).Value    = NISS&LcPo
    lnCount = 1 + lnCount
  ENDSCAN
  SELECT (lcRpTmpFl1)    
  
  XLSheet.Cells(lnRow,lnCount*2+1).Value         = NBALANCE
  XLSheet.Cells(lnRow,(lnCount*2) +2).Value    = NONORDER
  XLSheet.Cells(lnRow,(lnCount*2)+3).Value     = NAVALABL
  *--mhm 606523,1

  lnRow = lnRow + 1

ENDSCAN

XLApp.Visible = .T.
