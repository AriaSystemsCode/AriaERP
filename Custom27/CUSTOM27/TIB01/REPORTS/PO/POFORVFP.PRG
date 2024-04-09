*:************************************************************************
*: Program file  : POFORVFP.PRG   
*: Program desc. : Po Forecasting for TIBI .
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Purchase Order (PO).
*:      Developer: Mohamed Shokry (MHM)
*:************************************************************************
*: Calls         : 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example       : DO POSDTVFP
*:************************************************************************
*!C#123140
*!*	* This program sends the results of a Temp File    to 
*!*	* Microsoft Excel to calculate Po Detail Reoprt.

*-- lcRpDtDir  [gcDataDir]  && Save The Data Directory.
*-- lcRpWrDir  [gcWorkDir]  && Save The Work Directory.
*-- lcRpSysDir [gcSysHome]  && Save The system  Directory.
*-- lcRpRpDir  [gcRepHome]  && Save The report Directory.
*-- lcRpDefPth [gcDef_Path] && Save The DefPath.
*-- lcRpComp   [gcAct_Comp] && Save The Active Company .
*-- lcRpCmpNme [cCom_Name]  && Company Name.
*--WorkFile = lcTmpDtlDt && Save The TempName for  Date.
LPARAMETERS lcMemoFile

IF TYPE('lcMemoFile')#'C'
	RETURN
ENDIF

RESTORE FROM (lcMemoFile) ADDI
SET CONSOLE OFF 
SET SAFETY OFF 
SET CPDIALOG OFF 
LOCAL xlsheet,XLApp,tmpsheet

*-- Opren Temp File.

IF !USED(lcRWorkFil)
	USE (ALLTRIM(lcRpWrDir)+lcRWorkFil) ORDER 1 IN 0 EXCLUSIVE 
ENDIF

tmpsheet = GetObject(ALLTRIM(lcRpDefPth) +'POFORVFP','excel.sheet')

XLApp = tmpsheet.application
XLApp.WorkBooks.Add(ALLTRIM(lcRpDefPth) +'POFORVFP')


XLApp.Sheets("PO ForeCast").Select
XLApp.Sheets("PO ForeCast").Cells(2,1).Value = 'Date'
XLApp.Sheets("PO ForeCast").Cells(2,2).Value = DATE()
  
SELECT (lcRWorkFil)
* SET ORDER TO to 1 
*INDEX ON cSeason TAG (lcRWorkFil) OF (lcRpWrDir+lcrWorkFil+'.CDX')
LOCATE
lnRow = 5 

*--tell here we will change other tommorow
lnFabAmtSub   = 0
lnFabAmtGRD   = 0
lnStylcSub    = 0
lnStylcGRD    = 0
lnWrPaySub    = 0
lnWrPayGRD    = 0
lnQoutaSub    = 0
lnQoutaGRD    = 0
lnFreightSub  = 0
lnFreightGRD  = 0
lnDutySub     = 0
lnDutyGRD     = 0
lnCommSub     = 0
lnCommGRD     = 0
lnTotalSub    = 0
lnTotalGRD    = 0

lcSeason = cSeason
SCAN 
  IF lcSeason <> cSeason
    lcSeason = cSeason
     
  	XLApp.Sheets("PO ForeCast").Cells(lnRow,1).Value = 'Sub Total'
	  XLApp.Sheets("PO ForeCast").Cells(lnRow,1).Font.FontStyle = "Bold"
  	XLApp.Sheets("PO ForeCast").Cells(lnRow,1).Font.Size = 11
	  XLApp.Sheets("PO ForeCast").Cells(lnRow,9).Value = lnFabAmtSub
  	XLApp.Sheets("PO ForeCast").Cells(lnRow,9).Font.FontStyle = "Bold"
 	 XLApp.Sheets("PO ForeCast").Cells(lnRow,15).Value = lnStylcSub   
  	XLApp.Sheets("PO ForeCast").Cells(lnRow,15).Font.FontStyle = "Bold"
	  XLApp.Sheets("PO ForeCast").Cells(lnRow,18).Value = lnWrPaySub   
  	XLApp.Sheets("PO ForeCast").Cells(lnRow,18).Font.FontStyle = "Bold"
	  XLApp.Sheets("PO ForeCast").Cells(lnRow,19).Value = lnQoutaSub   
  	XLApp.Sheets("PO ForeCast").Cells(lnRow,19).Font.FontStyle = "Bold"
	  XLApp.Sheets("PO ForeCast").Cells(lnRow,21).Value = lnFreightSub 
  	XLApp.Sheets("PO ForeCast").Cells(lnRow,21).Font.FontStyle = "Bold"
	  XLApp.Sheets("PO ForeCast").Cells(lnRow,23).Value = lnDutySub    
  	XLApp.Sheets("PO ForeCast").Cells(lnRow,23).Font.FontStyle = "Bold"
	  XLApp.Sheets("PO ForeCast").Cells(lnRow,25).Value = lnCommSub    
  	XLApp.Sheets("PO ForeCast").Cells(lnRow,25).Font.FontStyle = "Bold"
	  XLApp.Sheets("PO ForeCast").Cells(lnRow,27).Value = lnTotalSub   
  	XLApp.Sheets("PO ForeCast").Cells(lnRow,27).Font.FontStyle = "Bold"

    lnRow = lnRow + 2
	  
		lnFabAmtSub  = 0
		lnStylcSub   = 0
		lnWrPaySub   = 0
		lnQoutaSub   = 0
		lnFreightSub = 0
		lnDutySub    = 0
		lnCommSub    = 0
		lnTotalSub   = 0

  ENDIF
    
	XLApp.Sheets("PO ForeCast").Cells(lnRow,1).Value = IIF(EMPTY(cgmttype),'',cgmttype)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,2).Value = IIF(EMPTY(vendor),'',vendor)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,3).Value = IIF(EMPTY(cFabDesc),'',cFabDesc)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,4).Value = IIF(EMPTY(cSeason),'',cSeason)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,5).Value = IIF(EMPTY(complete),'',complete)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,6).Value = IIF(EMPTY(cMatVen),'',cMatVen)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,7).Value = IIF(EMPTY(cfabpo),'',cfabpo)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,8).Value = IIF(EMPTY(dFabPo),'',dFabPo)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,9).Value = IIF(EMPTY(nFabAmnt),'',nFabAmnt)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,10).Value = IIF(EMPTY(cFabLC),'',cFabLC)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,11).Value = IIF(EMPTY(dFabRelPo),'',dFabRelPo)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,12).Value = Po
	XLApp.Sheets("PO ForeCast").Cells(lnRow,13).Value = IIF(EMPTY(dConfPo),'',dConfPo)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,14).Value = IIF(EMPTY(dStyPoLc),'',dStyPoLc)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,15).Value = IIF(EMPTY(nStyPoLc),'',nStyPoLc)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,16).Value = IIF(EMPTY(cstylc),'',cstylc)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,17).Value = IIF(EMPTY(dGMTLC),'',dGMTLC)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,18).Value = IIF(EMPTY(nWrPyMnt),'',nWrPyMnt)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,19).Value = IIF(EMPTY(nQouta),'',nQouta)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,20).Value = IIF(EMPTY(dQuotWir),'',dQuotWir)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,21).Value = IIF(EMPTY(nFreight),'',nFreight)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,22).Value = IIF(EMPTY(dFrghtWir),'',dFrghtWir)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,23).Value = IIF(EMPTY(nDuty),'',nDuty)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,24).Value = IIF(EMPTY(dDutyWir),'',dDutyWir)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,25).Value = IIF(EMPTY(nCommAmt),'',nCommAmt)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,26).Value = IIF(EMPTY(dCommAmt),'',dCommAmt)
	XLApp.Sheets("PO ForeCast").Cells(lnRow,27).Value = IIF(EMPTY(nTotalAmt),'',nTotalAmt)

	lnFabAmtSub   = lnFabAmtSub    + nFabAmnt
	lnFabAmtGRD   = lnFabAmtGRD    + nFabAmnt
	lnStylcSub    = lnStylcSub     + nStyPoLc
	lnStylcGRD    = lnStylcGRD     + nStyPoLc
	lnWrPaySub    = lnWrPaySub     + nWrPyMnt
	lnWrPayGRD    = lnWrPayGRD     + nWrPyMnt
	lnQoutaSub    = lnQoutaSub     + nQouta
	lnQoutaGRD    = lnQoutaGRD     + nQouta
	lnFreightSub  = lnFreightSub   + nFreight
	lnFreightGRD  = lnFreightGRD   + nFreight
	lnDutySub     = lnDutySub      + nDuty
	lnDutyGRD     = lnDutyGRD      + nDuty
	lnCommSub     = lnCommSub      + nCommAmt
	lnCommGRD     = lnCommGRD      + nCommAmt
	lnTotalSub    = lnTotalSub     + nTotalAmt
	lnTotalGRD    = lnTotalGRD     + nTotalAmt
 	lnRow = lnRow + 1
ENDSCAN
  
lnRow = lnRow + 1
*XLApp.Sheets("PO ForeCast").ROWS(lnRow).INSERT
XLApp.Sheets("PO ForeCast").Cells(lnRow,1).Value = 'Sub Total'
XLApp.Sheets("PO ForeCast").Cells(lnRow,1).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,1).Font.Size = 11
XLApp.Sheets("PO ForeCast").Cells(lnRow,9).Value = lnFabAmtSub
XLApp.Sheets("PO ForeCast").Cells(lnRow,9).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,15).Value = lnStylcSub   
XLApp.Sheets("PO ForeCast").Cells(lnRow,15).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,18).Value = lnWrPaySub   
XLApp.Sheets("PO ForeCast").Cells(lnRow,18).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,19).Value = lnQoutaSub   
XLApp.Sheets("PO ForeCast").Cells(lnRow,19).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,21).Value = lnFreightSub 
XLApp.Sheets("PO ForeCast").Cells(lnRow,21).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,23).Value = lnDutySub    
XLApp.Sheets("PO ForeCast").Cells(lnRow,23).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,25).Value = lnCommSub    
XLApp.Sheets("PO ForeCast").Cells(lnRow,25).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,27).Value = lnTotalSub   
XLApp.Sheets("PO ForeCast").Cells(lnRow,27).Font.FontStyle = "Bold"

lnRow = lnRow + 1
  
XLApp.Sheets("PO ForeCast").Cells(lnRow,1).Value = 'Grand Total'
XLApp.Sheets("PO ForeCast").Cells(lnRow,1).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,1).Font.Size = 11
XLApp.Sheets("PO ForeCast").Cells(lnRow,9).Value = lnFabAmtSub
XLApp.Sheets("PO ForeCast").Cells(lnRow,9).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,15).Value = lnStylcGrd   
XLApp.Sheets("PO ForeCast").Cells(lnRow,15).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,18).Value = lnWrPayGrd   
XLApp.Sheets("PO ForeCast").Cells(lnRow,18).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,19).Value = lnQoutaGrd   
XLApp.Sheets("PO ForeCast").Cells(lnRow,19).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,21).Value = lnFreightGrd 
XLApp.Sheets("PO ForeCast").Cells(lnRow,21).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,23).Value = lnDutyGrd    
XLApp.Sheets("PO ForeCast").Cells(lnRow,23).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,25).Value = lnCommGrd    
XLApp.Sheets("PO ForeCast").Cells(lnRow,25).Font.FontStyle = "Bold"
XLApp.Sheets("PO ForeCast").Cells(lnRow,27).Value = lnTotalGrd   
XLApp.Sheets("PO ForeCast").Cells(lnRow,27).Font.FontStyle = "Bold"

XLApp.Visible = .T.
IF USED(lcrWorkFil)
  USE IN (lcrWorkFil)
ENDIF
ERASE (lcRpWrDir+lcrWorkFil+'.DBF')
ERASE (lcRpWrDir+lcrWorkFil+'.CDX')
