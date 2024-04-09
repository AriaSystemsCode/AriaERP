*:************************************************************************
*: Program file  : POSTYVFP.PRG 
*: Program desc. : Style Summary Report.
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Purchase Order (PO).
*:      Developer: Mohamed Shokry (MHM)
*:************************************************************************
*: Calls         : 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example       : DO POSTYVFP
*:************************************************************************
*!200422,1
*!*	* This program sends the results of a Temp File    to 
*!*	* Microsoft Excel to calculate Material req for GMA.

*-- lcRpDtDir  [gcDataDir]  && Save The Data Directory.
*-- lcRpWrDir  [gcWorkDir]  && Save The Work Directory.
*-- lcRpSysDir [gcSysHome]  && Save The system  Directory.
*-- lcRpRpDir  [gcRepHome]  && Save The report Directory.
*-- lcRpDefPth [gcDef_Path] && Save The DefPath.
*-- lcRpComp   [gcAct_Comp] && Save The Active Company .
*-- lcRpCmpNme [cCom_Name]  && Company Name.
*--lcRpTmpFl1 = lcTmpDtlSo && Save The TempName for  Sales.
*--lcRpTmpFl2 = lcTmpDtlSt && Save The TempName for Style.
*--lcRpTmpFl3 = lcTmpDtlPo && Save The TempName for Po's.
*-- lcRpWareHs  [lcWareHos]  && Save WareHouse 
*-- lcRpTitle  :- Custom Titel we will print in the Report.
*-- lcRpCounPo :- countr for the inserted cols
LPARAMETERS lcMemoFile
IF TYPE('lcMemoFile')#'C'
	RETURN
ENDIF
RESTORE FROM (lcMemoFile) ADDI

*B606954,1 TMI [Start] 
ON ERROR DO lfError
*B606954,1 TMI [End  ] 

LOCAL xlsheet,XLApp,tmpsheet

*-- Opren Temp File.
IF !USED(lcRpTmpFl1)
	USE (ALLTRIM(lcRpWrDir)+lcRpTmpFl1) ORDER 1 IN 0
ENDIF

IF !USED(lcRpTmpFl2)
	USE (ALLTRIM(lcRpWrDir)+lcRpTmpFl2) ORDER 1 IN 0
ENDIF

IF !USED(lcRpTmpFl3)
	USE (ALLTRIM(lcRpWrDir)+lcRpTmpFl3) ORDER 1 IN 0
ENDIF


tmpsheet = GetObject(ALLTRIM(lcRpPath)+lcRpFilNam,'excel.sheet')

XLApp = tmpsheet.application
XLApp.WorkBooks.Add(ALLTRIM(lcRpPath)+lcRpFilNam)

IF "T" $ lcRpType
  XLApp.Sheets("Style").Select

  *-- Change the Company Name if Not 
  XLApp.Sheets("Style").Cells(3,4).Value      = lcRpOptTtl  
  XLApp.Sheets("Style").Cells(3,4).Font.Name  = "Arial"
  XLApp.Sheets("Style").Cells(3,4).Font.Size  = 11
  XLApp.Sheets("Style").Cells(3,4).Font.FontStyle = "Bold"

  SELECT (lcRpTmpFl2)
  LOCATE
  lnRow = 6
  SCAN
    XLApp.Sheets("Style").Cells(lnRow,1).Value = cStyDept
    XLApp.Sheets("Style").Cells(lnRow,2).Value = Style 
    XLApp.Sheets("Style").Cells(lnRow,3).Value = cStyDesc    
    XLApp.Sheets("Style").Cells(lnRow,4).Value = cStyGroup 
    XLApp.Sheets("Style").Cells(lnRow,5).Value = cStyCat
    XLApp.Sheets("Style").Cells(lnRow,6).Value = CstyClss    
    XLApp.Sheets("Style").Cells(lnRow,7).Value = CstyCls1    
    XLApp.Sheets("Style").Cells(lnRow,8).Value = CstyCls2    
    XLApp.Sheets("Style").Cells(lnRow,9).Value = CstyCls3    
    XLApp.Sheets("Style").Cells(lnRow,10).Value = NShpQty
    XLApp.Sheets("Style").Cells(lnRow,11).Value = NShpAmnt
    XLApp.Sheets("Style").Cells(lnRow,12).Value = NOpnQty
    XLApp.Sheets("Style").Cells(lnRow,13).Value = NOpnAmt
    XLApp.Sheets("Style").Cells(lnRow,14).Value = NPoOrdQty
    XLApp.Sheets("Style").Cells(lnRow,15).Value = NPoOrdAmt
    XLApp.Sheets("Style").Cells(lnRow,16).Value = NPoRcdQty
    XLApp.Sheets("Style").Cells(lnRow,17).Value = NpoRcdAmt
    XLApp.Sheets("Style").Cells(lnRow,18).Value = NPoTrnQty
    XLApp.Sheets("Style").Cells(lnRow,19).Value = NpoTrnAmt
    XLApp.Sheets("Style").Cells(lnRow,20).Value = NpoOpnQty
    XLApp.Sheets("Style").Cells(lnRow,21).Value = NpoOpnAmt
    XLApp.Sheets("Style").Cells(lnRow,22).Value = NtotStkQty
    XLApp.Sheets("Style").Cells(lnRow,23).Value = NtotStkAmt
    XLApp.Sheets("Style").Cells(lnRow,24).Value = NNgOTSQty
    *tmi 2/10/2003
    *XLApp.Sheets("Style").Cells(lnRow,25).Value = NNgOTSAmt
    *XLApp.Sheets("Style").Cells(lnRow,26).Value = NPsOTSQty
    XLApp.Sheets("Style").Cells(lnRow,25).Value = NPsOTSQty
    XLApp.Sheets("Style").Cells(lnRow,26).Value = NNgOTSAmt
    *B606954,1 TMI [End  ] 
    XLApp.Sheets("Style").Cells(lnRow,27).Value = NPsOTSAmt
    lnRow = lnRow + 1
  ENDSCAN
ENDIF

IF "P" $ lcRpType
  XLApp.Sheets("Purchases").Select

  *-- Change the Company Name if Not 
  XLApp.Sheets("Purchases").Cells(3,4).Value      = lcRpOptTtl  
  XLApp.Sheets("Purchases").Cells(3,4).Font.Name  = "Arial"
  XLApp.Sheets("Purchases").Cells(3,4).Font.Size  = 11
  XLApp.Sheets("Purchases").Cells(3,4).Font.FontStyle = "Bold"
  SELECT (lcRpTmpFl3)
  LOCATE
  lnRow = 6
  SCAN

    XLApp.Sheets("Purchases").Cells(lnRow,1).Value = cStyDept
    XLApp.Sheets("Purchases").Cells(lnRow,2).Value = Style 
    XLApp.Sheets("Purchases").Cells(lnRow,3).Value = cStyDesc    
    XLApp.Sheets("Purchases").Cells(lnRow,4).Value = cStyGroup   
    XLApp.Sheets("Purchases").Cells(lnRow,5).Value = cStyCat  
    XLApp.Sheets("Purchases").Cells(lnRow,6).Value = CstyClss    
    XLApp.Sheets("Purchases").Cells(lnRow,7).Value = CstyCls1    
    XLApp.Sheets("Purchases").Cells(lnRow,8).Value = CstyCls2    
    XLApp.Sheets("Purchases").Cells(lnRow,9).Value = CstyCls3
    XLApp.Sheets("Purchases").Cells(lnRow,10).Value = CVenCode
    XLApp.Sheets("Purchases").Cells(lnRow,11).Value = CVenNam
    XLApp.Sheets("Purchases").Cells(lnRow,12).Value = NPoOrdQty
    XLApp.Sheets("Purchases").Cells(lnRow,13).Value = NPoOrdAmt
    XLApp.Sheets("Purchases").Cells(lnRow,14).Value = NPoRcdQty
    XLApp.Sheets("Purchases").Cells(lnRow,15).Value = NpoRcdAmt
    XLApp.Sheets("Purchases").Cells(lnRow,16).Value = NPoTrnQty
    XLApp.Sheets("Purchases").Cells(lnRow,17).Value = NpoTrnAmt
    XLApp.Sheets("Purchases").Cells(lnRow,18).Value = NpoOpnQty
    XLApp.Sheets("Purchases").Cells(lnRow,19).Value = NpoOpnAmt
    
    lnRow = lnRow + 1
  ENDSCAN
ENDIF

IF "S" $ lcRpType
  XLApp.Sheets("Sales").Select

  *-- Change the Company Name if Not 
  XLApp.Sheets("Sales").Cells(3,4).Value      = lcRpOptTtl  
  XLApp.Sheets("Sales").Cells(3,4).Font.Name  = "Arial"
  XLApp.Sheets("Sales").Cells(3,4).Font.Size  = 11
  XLApp.Sheets("Sales").Cells(3,4).Font.FontStyle = "Bold"

  SELECT (lcRpTmpFl1)
  LOCATE
  lnRow = 6
  SCAN
    XLApp.Sheets("Sales").Cells(lnRow,1).Value = cStyDept
    XLApp.Sheets("Sales").Cells(lnRow,2).Value = Style 
    XLApp.Sheets("Sales").Cells(lnRow,3).Value = cStyDesc    
    XLApp.Sheets("Sales").Cells(lnRow,4).Value = cStyGroup   
    XLApp.Sheets("Sales").Cells(lnRow,5).Value = cStyCat   
    XLApp.Sheets("Sales").Cells(lnRow,6).Value = CstyClss    
    XLApp.Sheets("Sales").Cells(lnRow,7).Value = CstyCls1    
    XLApp.Sheets("Sales").Cells(lnRow,8).Value = CstyCls2    
    XLApp.Sheets("Sales").Cells(lnRow,9).Value = CstyCls3    
    XLApp.Sheets("Sales").Cells(lnRow,10).Value = CAccount
    XLApp.Sheets("Sales").Cells(lnRow,11).Value = CCustNam
    XLApp.Sheets("Sales").Cells(lnRow,12).Value = NShpQty
    XLApp.Sheets("Sales").Cells(lnRow,13).Value = NShpAmnt
    XLApp.Sheets("Sales").Cells(lnRow,14).Value = NOpnQty
    XLApp.Sheets("Sales").Cells(lnRow,15).Value = NOpnAmt
    lnRow = lnRow + 1
  ENDSCAN
ENDIF

XLApp.Visible = .T.


***********************************************************
*  TMI - TAREK MOHAMED IBRAHIM                   *B606954,6
*  2/16/2003  
*  Quit if any error
***********************************************************
FUNCTION lfError
DO FORM GMAMSGBOX.SCX
READ EVENTS
*--End of lfError