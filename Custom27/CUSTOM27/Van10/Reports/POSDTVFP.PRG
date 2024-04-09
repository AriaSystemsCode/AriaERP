*:************************************************************************
*: Program file  : POSDTVFP.PRG   
*: Program desc. : Po Detail Report for VAN MAR .
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
*! Modifications :
*:**********************************************************************************************
*!B127075,1 MHM 04/10/2005 Change Account field to be custom field "cVanCode"
*:**********************************************************************************************
*!119662
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
*--lcRpTmpFl2 = lcTmpDtlCs && Save The TempName for Customer.
*--lcRpTmpFl3 = lcTmpDtlPo && Save The TempName for Po's.
*-- lcRpWareHs  [lcWareHos]  && Save WareHouse 
*-- lcRpTitle  :- Custom Titel we will print in the Report.
*-- lcRpCounPo :- countr for the inserted cols
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
tmpsheet = GetObject(ALLTRIM(lcRpDefPth) +'POSDTVFP','excel.sheet')

XLApp = tmpsheet.application
XLApp.WorkBooks.Add(ALLTRIM(lcRpDefPth) +'POSDTVFP')

IF !USED("Customer")
	USE (ALLTRIM(lcRpDtDir)+ ('Customer')) ORDER 1 IN 0
ENDIF

IF !USED ("POSHDR")
	USE (ALLTRIM(lcRpDtDir)+ ('POSHDR')) ORDER 1 IN 0
ENDIF
IF !USED ("SCALE")
	USE (ALLTRIM(lcRpDtDir)+ ('SCALE')) ORDER 1 IN 0
ENDIF

* Sort by PO
IF lcRpSortBy='P'
  XLApp.Sheets("Sort by PO").Select
  XLApp.Sheets("Sort by PO").Cells(2,13).Value = DATE()
  
  SELECT (lcRWorkFil)
  
  INDEX ON PO + ALLTRIM(STYLE) TAG (lcRWorkFil) OF (lcRpWrDir+lcrWorkFil+'.CDX')
  LOCATE
  lnRow = 5 
  PRIVATE lcPo,lnTotQty,lnTotGqty, llcheckPrnt
  lnTotQty=0
  lnTotGQty=0
  llcheckPrnt=.T.
  lcPo = Po
  SCAN FOR TRANCD = '1'
    IF po <> lcpo
      lcpo= Po
      lnRow = lnRow + 1
      XLApp.Sheets("Sort by PO").ROWS(lnRow).INSERT
      
	  XLApp.Sheets("Sort by PO").Cells(lnRow,1).Value = 'TOTAL'
	  XLApp.Sheets("Sort by PO").Cells(lnRow,1).Font.FontStyle = "Bold"
	  XLApp.Sheets("Sort by PO").Cells(lnRow,1).Font.Size = 11
	  XLApp.Sheets("Sort by PO").Cells(lnRow,3).Value = lnTotQty
	  XLApp.Sheets("Sort by PO").Cells(lnRow,3).Font.FontStyle = "Bold"
      lnRow = lnRow + 2
      XLApp.Sheets("Sort by PO").ROWS(lnRow).INSERT
	  
      lnTotQty = 0
	  llcheckPrnt=.T.
      lcPo = Po
    ENDIF
    
	=SEEK(cstytype+po,'POSHDR')
    *!B127075,1 MHM 04/10/2005 Change Account field to be custom field "cVanCode" [Start]
    *=SEEK('M'+cAccCode,'CUSTOMER')
    =SEEK('M'+cVanCode,'CUSTOMER')
    *!B127075,1 MHM [End]
     
    =SEEK('S'+Scale,'SCALE')
    lcRShipVia   = GetCodDes(POSHDR.ShipVia , 'SHIPVIA')
    
    lcScale=''
    FOR lncount = 1 TO Scale.cnt 
		lcCount = STR(lnCount , 1)
		IF lnCount = scale.cnt
			lcScale=lcScale+Scale.sz&lcCount
		Else
	    	lcScale=lcScale+Scale.sz&lcCount+','
    	Endif
	ENDFOR
	IF llcheckPrnt
        XLApp.Sheets("Sort by PO").Cells(lnRow,1).NumberFormat = "@"	
		XLApp.Sheets("Sort by PO").Cells(lnRow,1).Value = Po
    	XLApp.Sheets("Sort by PO").Cells(lnRow+1,1).Value = POSHDR.origin
    ENDIF
    llcheckPrnt=.F.
	XLApp.Sheets("Sort by PO").Cells(lnRow,2).Value =IIF(Empty(CSpec),'',CSpec)
    XLApp.Sheets("Sort by PO").Cells(lnRow,3).Value = TotQty
	XLApp.Sheets("Sort by PO").Cells(lnRow,4).Value = SUBSTR(STYLE,1,lcrMajPic)
	XLApp.Sheets("Sort by PO").Cells(lnRow,5).Value = ALLTRIM(SUBSTR(Style,lcrNonMajS,lcRColorLe))
	XLApp.Sheets("Sort by PO").Cells(lnRow,6).Value =IIF(EMPTY(ccompldesc),'',ccompldesc) 
	XLApp.Sheets("Sort by PO").Cells(lnRow,7).Value = IIF(Empty(POSHDR.cfabpo),'',POSHDR.cfabpo)
	XLApp.Sheets("Sort by PO").Cells(lnRow,8).Value = IIF(EMPTY(POSHDR.complete),'',POSHDR.complete)
	XLApp.Sheets("Sort by PO").Cells(lnRow,9).Value = lcRShipVia
	XLApp.Sheets("Sort by PO").Cells(lnRow,10).Value = IIF(EMPTY(POSHDR.available),'',POSHDR.available)
	XLApp.Sheets("Sort by PO").Cells(lnRow,11).Value = Customer.btname
	XLApp.Sheets("Sort by PO").Cells(lnRow,12).Value = IIF(EMPTY(Dshpdte),'',Dshpdte)
	XLApp.Sheets("Sort by PO").Cells(lnRow,13).Value = lcScale
	lnTotQty = lnTotQty +TotQty
	lnTotGqty = lnTotGqty +TotQty
    lnRow = lnRow + 1
    *XLApp.Sheets("Sort by PO").ROWS(lnRow).INSERT
  ENDSCAN
  lnRow = lnRow + 1
  XLApp.Sheets("Sort by PO").ROWS(lnRow).INSERT
  XLApp.Sheets("Sort by PO").Cells(lnRow,1).Value = 'TOTAL'
  XLApp.Sheets("Sort by PO").Cells(lnRow,3).Value = lnTotQty
  XLApp.Sheets("Sort by PO").Cells(lnRow,1).Font.Size = 11
  XLApp.Sheets("Sort by PO").Cells(lnRow,3).Font.FontStyle = "Bold"
  XLApp.Sheets("Sort by PO").Cells(lnRow,1).Font.FontStyle = "Bold"
  lnRow = lnRow + 2
  XLApp.Sheets("Sort by PO").ROWS(lnRow).INSERT
  XLApp.Sheets("Sort by PO").Cells(lnRow,1).Value = 'GRAND TOTAL'
  XLApp.Sheets("Sort by PO").Cells(lnRow,3).Value = lnTotGQty
  XLApp.Sheets("Sort by PO").Cells(lnRow,1).Font.FontStyle = "Bold"
  XLApp.Sheets("Sort by PO").Cells(lnRow,1).Font.Size = 11
  XLApp.Sheets("Sort by PO").Cells(lnRow,3).Font.FontStyle = "Bold"
ENDIF

* Sort by Customer Name
IF lcRpSortBy ='A'
  XLApp.Sheets("Sort by Customer").Select
  XLApp.Sheets("Sort by Customer").Cells(2,13).Value = DATE()
  
  SELECT (lcRWorkFil)
  *!B127075,1 MHM 04/10/2005 Change Account field to be custom field "cVanCode" [Start]
  *INDEX ON cAccCode + DTOS(Dshpdte) TAG (lcRWorkFil) OF (lcRpWrDir+lcrWorkFil+'.CDX') ADDITIVE 
  INDEX ON cVanCode + DTOS(Dshpdte) TAG (lcRWorkFil) OF (lcRpWrDir+lcrWorkFil+'.CDX') ADDITIVE 
  *!B127075,1 MHM  [End]
  
  lnRow = 5 
  PRIVATE lcAccount,lnTotQty,lnTotGqty
  lnTotQty = 0
  lnTotGQty = 0
  *!B127075,1 MHM 04/10/2005 Change Account field to be custom field "cVanCode" [Start]
  *lcAccount = cAccCode
  lcAccount = cVanCode 
  *!B127075,1 MHM  [End]
  
  SCAN FOR TRANCD = '1'
    *!B127075,1 MHM 04/10/2005 Change Account field to be custom field "cVanCode" [Start]
    *IF cAccCode <> lcAccount
      *lcAccount= cAccCode
    IF cVanCode <> lcAccount
      lcAccount= cVanCode 
    *!B127075,1 MHM  [End]
    
      lnRow = lnRow + 1
      XLApp.Sheets("Sort by Customer").ROWS(lnRow).INSERT
	  XLApp.Sheets("Sort by Customer").Cells(lnRow,1).Value = 'TOTAL'
	  XLApp.Sheets("Sort by Customer").Cells(lnRow,3).Value = lnTotQty
	  XLApp.Sheets("Sort by Customer").Cells(lnRow,1).Font.Size = 11
	  XLApp.Sheets("Sort by Customer").Cells(lnRow,1).Font.FontStyle = "Bold"
	  XLApp.Sheets("Sort by Customer").Cells(lnRow,3).Font.FontStyle = "Bold"
      lnRow = lnRow + 2
      XLApp.Sheets("Sort by Customer").ROWS(lnRow).INSERT
      lnTotQty = 0
    ENDIF
    
	=SEEK(cstytype + po ,'POSHDR')
    lcRShipVia   = GetCodDes(POSHDR.ShipVia , 'SHIPVIA')
    *!B127075,1 MHM 04/10/2005 Change Account field to be custom field "cVanCode" [Start]
    *=SEEK('M'+cAccCode,'CUSTOMER')
    =SEEK('M'+cVanCode,'CUSTOMER')
    *!B127075,1 MHM [End]
    =SEEK('S'+Scale,'SCALE')
    lcScale=''
    FOR lncount = 1 TO Scale.cnt 
		lcCount = STR(lnCount , 1)
		IF lnCount = scale.cnt
			lcScale=lcScale+Scale.sz&lcCount
		Else
	    	lcScale=lcScale+Scale.sz&lcCount+','
    	Endif
	Endfor

    XLApp.Sheets("Sort by Customer").Cells(lnRow,1).NumberFormat = "@"	
	XLApp.Sheets("Sort by Customer").Cells(lnRow,1).Value = Po
	XLApp.Sheets("Sort by Customer").Cells(lnRow,2).Value = IIF(Empty(CSpec),'',CSpec)
    XLApp.Sheets("Sort by Customer").Cells(lnRow,3).Value = TotQty
	XLApp.Sheets("Sort by Customer").Cells(lnRow,4).Value = SUBSTR(STYLE,1,lcrMajPic)
	XLApp.Sheets("Sort by Customer").Cells(lnRow,5).Value = ALLTRIM(SUBSTR(Style,lcrNonMajS,lcRColorLe))
	XLApp.Sheets("Sort by Customer").Cells(lnRow,6).Value = IIF(Empty(ccompldesc),'',ccompldesc)
	XLApp.Sheets("Sort by Customer").Cells(lnRow,7).Value = IIF(Empty(POSHDR.cfabpo),'',POSHDR.cfabpo)
	XLApp.Sheets("Sort by Customer").Cells(lnRow,8).Value = IIF(Empty(POSHDR.complete),'',POSHDR.complete)
	XLApp.Sheets("Sort by Customer").Cells(lnRow,9).Value = lcRShipVia
	XLApp.Sheets("Sort by Customer").Cells(lnRow,10).Value =IIF(Empty(POSHDR.available),'',POSHDR.available)
	XLApp.Sheets("Sort by Customer").Cells(lnRow,11).Value = Customer.btname
	XLApp.Sheets("Sort by Customer").Cells(lnRow,12).Value = IIF(Empty(Dshpdte),'',Dshpdte)
	XLApp.Sheets("Sort by Customer").Cells(lnRow,13).Value = lcScale
	lnTotQty = lnTotQty +TotQty
	lnTotGqty = lnTotGqty +TotQty
    lnRow = lnRow + 1
    XLApp.Sheets("Sort by Customer").ROWS(lnRow).INSERT
  ENDSCAN
  lnRow = lnRow + 1
  XLApp.Sheets("Sort by Customer").ROWS(lnRow).INSERT
  XLApp.Sheets("Sort by Customer").Cells(lnRow,1).Value = 'TOTAL'
  XLApp.Sheets("Sort by Customer").Cells(lnRow,3).Value = lnTotQty
  XLApp.Sheets("Sort by Customer").Cells(lnRow,1).Font.Size = 11
  XLApp.Sheets("Sort by Customer").Cells(lnRow,1).Font.FontStyle = "Bold"
  XLApp.Sheets("Sort by Customer").Cells(lnRow,3).Font.FontStyle = "Bold"
  lnRow = lnRow + 2
  XLApp.Sheets("Sort by Customer").ROWS(lnRow).INSERT
  XLApp.Sheets("Sort by Customer").Cells(lnRow,1).Value = 'GRAND TOTAL'
  XLApp.Sheets("Sort by Customer").Cells(lnRow,3).Value = lnTotGQty
  XLApp.Sheets("Sort by Customer").Cells(lnRow,1).Font.Size = 11
  XLApp.Sheets("Sort by Customer").Cells(lnRow,1).Font.FontStyle = "Bold"
  XLApp.Sheets("Sort by Customer").Cells(lnRow,3).Font.FontStyle = "Bold"
ENDIF

* Sort by Date
IF lcRpSortBy='D'
  XLApp.Sheets("Sort by Del. Date").Select
  XLApp.Sheets("Sort by Del. Date").Cells(2,13).Value = DATE()
  
  SELECT (lcRWorkFil)
  *!B127075,1 MHM 04/10/2005 Change Account field to be custom field "cVanCode" [Start]
  *INDEX ON DTOS(DShpDte) + cAccCode+ Style TAG (lcRWorkFil) OF (lcRpWrDir+lcrWorkFil+'.CDX')
  INDEX ON DTOS(DShpDte) + cVanCode+ Style TAG (lcRWorkFil) OF (lcRpWrDir+lcrWorkFil+'.CDX')
  *!B127075,1 MHM [End]
  
  LOCATE
  lnRow = 5 
  PRIVATE lcDShpDte,lnTotQty,lnTotGqty
  lnTotQty=0
  lnTotGQty=0
  lcDShpDte = DShpDte
  SCAN FOR TRANCD = '1'
    IF DShpDte <> lcDShpDte
      lcDShpDte= DShpDte
      lnRow = lnRow + 1
      XLApp.Sheets("Sort by Del. Date").ROWS(lnRow).INSERT
	  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,1).Value = 'TOTAL'
	  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,3).Value = lnTotQty
	  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,1).Font.Size = 11
	  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,1).Font.FontStyle = "Bold"
	  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,3).Font.FontStyle = "Bold"
      lnRow = lnRow + 2
      XLApp.Sheets("Sort by Del. Date").ROWS(lnRow).INSERT
      lnTotQty = 0
    ENDIF
	=SEEK(cstytype+po,'POSHDR')
    lcRShipVia = GetCodDes(POSHDR.ShipVia , 'SHIPVIA')
    *!B127075,1 MHM 04/10/2005 Change Account field to be custom field "cVanCode" [Start]
    *=SEEK('M'+cAccCode,'CUSTOMER')
    =SEEK('M'+cVanCode,'CUSTOMER')
    *!B127075,1 MHM [End]
    =SEEK('S'+Scale,'SCALE')
    lcScale=''
    FOR lncount = 1 TO Scale.cnt 
		lcCount = STR(lnCount , 1)
		IF lnCount = scale.cnt
			lcScale=lcScale+Scale.sz&lcCount
		Else
	    	lcScale=lcScale+Scale.sz&lcCount+','
    	Endif
	ENDFOR
    XLApp.Sheets("Sort by Del. Date").Cells(lnRow,1).NumberFormat = "@"	
	XLApp.Sheets("Sort by Del. Date").Cells(lnRow,1).Value = Po
	XLApp.Sheets("Sort by Del. Date").Cells(lnRow,2).Value = IIF(Empty(CSpec),'',CSpec)
    XLApp.Sheets("Sort by Del. Date").Cells(lnRow,3).Value = TotQty
	XLApp.Sheets("Sort by Del. Date").Cells(lnRow,4).Value = SUBSTR(STYLE,1,lcrMajPic)
	XLApp.Sheets("Sort by Del. Date").Cells(lnRow,5).Value = ALLTRIM(SUBSTR(Style,lcrNonMajS,lcRColorLe))
	XLApp.Sheets("Sort by Del. Date").Cells(lnRow,6).Value = IIF(Empty(ccompldesc),'',ccompldesc)
	XLApp.Sheets("Sort by Del. Date").Cells(lnRow,7).Value = IIF(Empty(POSHDR.cfabpo),'',POSHDR.cfabpo)
	XLApp.Sheets("Sort by Del. Date").Cells(lnRow,8).Value = IIF(Empty(POSHDR.complete),'',POSHDR.complete)
	XLApp.Sheets("Sort by Del. Date").Cells(lnRow,9).Value = lcRShipVia
	XLApp.Sheets("Sort by Del. Date").Cells(lnRow,10).Value = IIF(Empty(POSHDR.available),'',POSHDR.available)
	XLApp.Sheets("Sort by Del. Date").Cells(lnRow,11).Value = Customer.btname
	XLApp.Sheets("Sort by Del. Date").Cells(lnRow,12).Value = IIF(Empty(Dshpdte),'',Dshpdte)
	XLApp.Sheets("Sort by Del. Date").Cells(lnRow,13).Value = lcScale
	lnTotQty = lnTotQty +TotQty
	lnTotGqty = lnTotGqty +TotQty
    lnRow = lnRow + 1
    XLApp.Sheets("Sort by Del. Date").ROWS(lnRow).INSERT
    
  ENDSCAN
  lnRow = lnRow + 1
  XLApp.Sheets("Sort by Del. Date").ROWS(lnRow).INSERT
  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,1).Value = 'TOTAL'
  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,3).Value = lnTotQty
  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,1).Font.Size = 11
  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,1).Font.FontStyle = "Bold"
  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,3).Font.FontStyle = "Bold"
  lnRow = lnRow + 2
  XLApp.Sheets("Sort by Del. Date").ROWS(lnRow).INSERT
  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,1).Value = 'GRAND TOTAL'
  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,3).Value = lnTotGQty
  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,1).Font.Size = 11
  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,1).Font.FontStyle = "Bold"
  XLApp.Sheets("Sort by Del. Date").Cells(lnRow,3).Font.FontStyle = "Bold"
ENDIF

XLApp.Visible = .T.
IF USED(lcrWorkFil)
  USE IN (lcrWorkFil)
ENDIF
ERASE (lcRpWrDir+lcrWorkFil+'.DBF')
ERASE (lcRpWrDir+lcrWorkFil+'.CDX')


*!*************************************************************
*! Name      : GetCodDes
*! Developer : Mohamed Shokry
*! Date      : 10/07/2003
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*! Calls: GetCodDes()               
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION GetCodDes

PARAMETERS lcCodeVal, lcFldName , llChkEdit

PRIVATE lcSavOrder,lcReturnVal,lcSavSelct

* Rise array flag and the program is the same if it is direct value. [Begin]
llRetArray = (TYPE("lcCodeVal[1,1]") = "C")
IF !llRetArray AND (TYPE("lcCodeVal") <> "C" OR TYPE("lcFldName") <> "C")
  RETURN ''
ENDIF

IF !llRetArray
  lcCodeVal   = PADR(UPPER(lcCodeVal),6)
  lcFldName   = PADR(UPPER(lcFldName),10)
ENDIF  

lcReturnVal = ""
lcSavSelct  = ALIAS()      && Variable to save the currently selected file.

llUseCodes = .F.
IF !USED("CODES")
  USE (lcRpDtDir+"Codes") IN 0
  llUseCodes = .T.
ENDIF

SELECT CODES               && Select CODES file
lcSavOrder = SYS(22)       && Save the file order
SET ORDER TO TAG CODES     && Change the order

*-- if pass array of codes.
IF llRetArray
  PRIVATE lnArrLen , lnCodeLen
  lnCodeLen = 6
  lnArrLen  = 0
  FOR lnArrLen = 1 TO ALEN(lcCodeVal,1)
    lcCodeVal[lnArrLen,1] = PADR(UPPER(lcCodeVal[lnArrLen,1]),6)
    lcCodeVal[lnArrLen,2] = PADR(UPPER(lcCodeVal[lnArrLen,2]),10)
    IF EMPTY(lcCodeVal[lnArrLen,1]) .OR. lcCodeVal[lnArrLen,1] = "*"
      lcCurFlt = FILTER()
      lnFltRec = IIF(EOF() .OR. BOF(), 0, RECNO())
      SET FILTER TO
      * Changed lcCodeVal to lcCodeVal[lnArrLen,1] Start
      IF !SEEK(SPACE(1)+LEFT(lcCodeVal[lnArrLen,1],1))
        APPEND BLANK
        REPLACE cFld_Name  WITH IIF(lcCodeVal[lnArrLen,1] = '*','ALL','N/A') ;
                cCode_No   WITH IIF(lcCodeVal[lnArrLen,1] = '*','*','')      ;
                cDiscrep   WITH IIF(lcCodeVal[lnArrLen,1] = '*','All','N/A') ;
                cRltField  WITH 'N'
      ENDIF  
      lcCodeVal[lnArrLen,3] = CODES.cDiscrep
      SET FILTER TO &lcCurFlt.
      IF BETWEEN(lnFltRec,1,RECCOUNT())
        GO lnFltRec
      ENDIF    

    ELSE
  
      IF SEEK('N' + lcCodeVal[lnArrLen,1] + "N" + lcCodeVal[lnArrLen,2])  
        lcCodeVal[lnArrLen,3] = CODES.cDiscrep
      ELSE
        lcCodeVal[lnArrLen,3] = ''       && In case of this code record is deleted
      ENDIF  

    ENDIF

    IF !EMPTY(lcCodeVal[lnArrLen,3]) AND gfIsEdtble(ALLTRIM(lcCodeVal[lnArrLen,2]) , @lnCodeLen)
       lcCodeVal[lnArrLen,3] = PADR(lcCodeVal[lnArrLen,1],lnCodeLen) + '-' + lcCodeVal[lnArrLen,3]
    ENDIF

  ENDFOR
ELSE && Pass one code only

  IF EMPTY(lcCodeVal) .OR. lcCodeVal = "*"
    lcCurFlt = FILTER()
    lnFltRec = IIF(EOF() .OR. BOF(), 0, RECNO())
    SET FILTER TO
    IF !SEEK(SPACE(1)+LEFT(lcCodeVal,1))
      APPEND BLANK
      REPLACE cFld_Name  WITH IIF(lcCodeVal = '*','ALL','N/A') ;
              cCode_No   WITH IIF(lcCodeVal = '*','*','')      ;
              cDiscrep   WITH IIF(lcCodeVal = '*','All','N/A') ;
              cRltField  WITH 'N'
    ENDIF  
    lcReturnVal = CODES.cDiscrep

    SET FILTER TO &lcCurFlt.
    IF BETWEEN(lnFltRec,1,RECCOUNT())
      GO lnFltRec
    ENDIF    
  ELSE
    IF SEEK('N' + lcCodeVal + "N" + lcFldName)  
      lcReturnVal = CODES.cDiscrep
    ELSE
      lcReturnVal = ''       && In case of this code record is deleted
    ENDIF  
  ENDIF  

  PRIVATE lnCodeLen
  lnCodeLen = 6
  IF llChkEdit AND !EMPTY(lcReturnVal) AND gfIsEdtble(ALLTRIM(lcFldName) , @lnCodeLen)
    lcReturnVal = PADR(lcCodeVal,lnCodeLen) + '-' + lcReturnVal
  ENDIF

ENDIF

SET ORDER TO &lcSavOrder
IF llUseCodes
  USE IN Codes
ENDIF
SELECT IIF(EMPTY(lcSavSelct),0,lcSavSelct)
RETURN lcReturnVal
*-- end of GetCodDes.

*:---------------------------------------------------------------------
*! Name      : gfIsEdtble
*! Developer : Mohamed Shokry- (MHM)
*! Date      : 10-07-2003
*! Purpose   : Function to tell if a specific code is editable by the
*!             user or not.
*: Job ID    : 
*:---------------------------------------------------------------------
*: Calls              : None
*:---------------------------------------------------------------------
*: Passed Parameters  : lcField  -> The code to be checked.
*:                      lnFieldW -> Pointer to a numeric variable to 
*:                                  hold the field width.
*:---------------------------------------------------------------------
*: Example            : = gfIsEdtble("TERMS", @lnWidth)
*:---------------------------------------------------------------------
FUNCTION gfIsEdtble
PARAMETERS lcPField, lnFieldW, lcActvComp
PRIVATE llRetVal, lcDataDir

*-- Set the company used variable with it's path.
lcActvComp = IIF(TYPE("lcActvComp")#"C", lcRpComp, lcActvComp)
lcDataDir  = lcRpDtDir
IF !(lcActvComp == lcRpComp)
  USE (lcRpSysDir+"SYCCOMP") IN 0 ORDER cComp_ID AGAIN ALIAS CompFile
  IF SEEK(lcActvComp,'CompFile')
    lcDataDir = gfGetDataDir(ALLTRIM(CompFile.cCom_DDir))
  ENDIF
  USE IN CompFile
ENDIF  

USE (lcRpSysDir+"SYDFIELD") IN 0 ORDER CFLD_NAME AGAIN ALIAS FieldFile
USE (lcDataDir+"CODESET" ) IN 0 ORDER Fildname  AGAIN ALIAS CodeSetF
llRetVal = IIF(SEEK(lcPField, "FieldFile"),;
              ("EDITABLE" $ UPPER(ALLTRIM(FieldFile.mCodeInfo))), .F.)
lnFieldW = IIF(SEEK(lcPField, "CodeSetF" ), CodeSetF.nfld_wdth, FieldFile.nFld_Wdth)
USE IN FieldFile
USE IN CodeSetF

RETURN llRetVal

