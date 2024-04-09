*!**********************************************************************************************************
*! Program file  : ICSTUN19.PRG 
*! Program desc. : Export the [Free To Sell Report] to Excel
*! System        : ARIA APPAREL SYSTEM 2.7
*! Module        : IC
*! Developer     : NADER NABIL - NNA
*! Date          : 01/18/2005
*! Purpose       : This program sends the results of a Temp File to MS Excel for STU20.
*! REF.          : C#125285,1
*!**********************************************************************************************************
*! Passed Parameters  : None
*!**********************************************************************************************************
*! Example       : DO ICSTUN20
*!**********************************************************************************************************
*! Modifications :
*!**********************************************************************************************************
LPARAMETERS lcMemoFile
PRIVATE lnTotFre , lnTotWip , lnRow , lnPageNo , lcCentury , xlsheet , XLApp , tmpsheet
DIMENSION  laTotPages(1)
STORE 0 TO lnTotFre , lnTotWip , lnRow , lnPageNo , laTotPages
STORE '' TO lcCentury

*--Hold the current Set Century
lcCentury = SET('CENTURY')
SET CENTURY ON

ON ERROR DO lPError WITH ERROR(), MESSAGE(), LINENO()
IF TYPE('lcMemoFile')#'C'
	RETURN
ENDIF
*--Restore Variables that was used in Report IcfreStu.prg
RESTORE FROM (lcMemoFile) ADDITIVE

*--Get The Template File (Exported.xls)
tmpsheet = GetObject(gcWorkDir+lcRpFilNam,'Excel.Sheet')

XLApp = tmpsheet.application
XLApp.WorkBooks.Add(gcWorkDir+lcRpFilNam)
lnRow = 1
IF !USED(lcTmpFile)		&& the Header File
  USE (gcWorkDir+lcTmpFile) ORDER 1 IN 0
ENDIF
IF !USED(lcShwRepFl)	&& the Header File
  USE (gcWorkDir+lcShwRepFl) ORDER 2 IN 0
ENDIF
SELECT (lcShwRepFl)
SET RELATION TO cStyClr INTO &lcTmpFile

*-- Get the Maximum Number in field of (PageNO) to get Total Number of Pages
SELECT MAX(PageNo) FROM &lcShwRepFl INTO ARRAY laTotPages
LOCATE

*--Setup the Excel File 
=lfFileSet()

WITH XLApp.Sheets("style")
  *--Write the Page Header in the excel file
  =lfPageHdr(lnRow)
  lnRow = lnRow + 1      
  =lfDetails()
  lnRow = lnRow + 1      
  lcRowFr  = ALLTRIM(STR(lnRow))
  lcRowTo  = ALLTRIM(STR(lnRow+2))
  lcRange = "E" + lcRowFr +":" + "O" + lcRowTo
  .RANGE(lcRange).SELECT
  FOR I=7 TO 10
    With XLApp.Selection.Borders(I)
      .LineStyle  = 1
      .Weight     = 3
      .ColorIndex = 5
    ENDWITH
    WITH XLApp.Selection
      .Merge
  	  .FONT.Name = 'Arial'
      .FONT.Size = 10
	  .FONT.Bold = .T.
    ENDWITH
    lcText = IIF(lcRpRepClc='S','Grand Total WIP.....'+SPACE(10)+ALLTRIM(STR(lnTotWip)),'')
    XLApp.ActiveCell.FormulaR1C1 = 'Greand Total Free to Sell.....'+SPACE(10)+;
    								  ALLTRIM(STR(lnTotFre))+SPACE(10)+lcText
  ENDFOR
ENDWITH
XLApp.Visible = .T.
ERASE (lcMemoFile)
SET CENTURY &lcCentury
*!***************************************************************************
*! Name      : lfFileSet
*! Developer : NADER NABIL (NNA)
*! Date      : 01/18/2005
*! Purpose   : Setup the Excel Sheet File to Print the Report
*!***************************************************************************
*! Called from : Icstun19
*!***************************************************************************
*! Example     : =lfFileSet()
*!***************************************************************************
FUNCTION lfFileSet
With XLApp.Sheets("style")
  .Columns("A:A").ColumnWidth = 18
  .Columns("B:T").ColumnWidth = 6
  .Rows("1:2").RowHeight = 19
  .Rows("3:2000").RowHeight = 12
EndWith
*--End Of function lfFileSet.

*!***************************************************************************
*! Name      : lfPageHdr
*! Developer : NADER NABIL (NNA)
*! Date      : 01/18/2005
*! Purpose   : Setup the page header (For Every Page)
*!***************************************************************************
*! Called from : Icstun20
*!***************************************************************************
*! Example     : =lfPageHdr()
*!***************************************************************************
FUNCTION lfPageHdr
PARAMETERS lnCurRow 
LOCAL lcRow , lcRow2 , lcRange
STORE '' TO lcRow , lcRow2 , lcRange
WITH XLApp.Sheets("style") 
  *--The First Line in the header
  lcRow   = ALLTRIM(STR(lnCurRow))
  .Rows(lcRow).RowHeight = 19			&& The Row height for the first title (Company name)
  lcRange = "B" + lcRow +":" + "P" + lcRow
  .Range(lcRange).Select
  WITH XLApp.SELECTION
    .Merge
    .HorizontalAlignment = 7
    .FONT.NAME='ARIAL'
    .FONT.SIZE=14
    .FONT.BOLD=.T.
    .FONT.ColorIndex = 5
  ENDWITH
  XLApp.ActiveCell.FormulaR1C1 = gcCom_Name
  lcRow   = ALLTRIM(STR(lnCurRow))
  lcRange = "Q" + lcRow +":" + "S" + lcRow
  .Range(lcRange).Select
  XLApp.Selection.Merge
  XLApp.ActiveCell.FormulaR1C1='Page : ' + ALLTRIM(STR(CEILING(lnCurRow/47))) + ;
   								  '   of   ' + ALLTRIM(STR(laTotPages(1)))
  *--The Second Line in the header
   lnRow    = lnRow + 1      
   lnCurRow = lnCurRow + 1
   lcRow   = ALLTRIM(STR(lnCurRow))
   .Rows(lcRow).RowHeight = 19			&& The Row height for the Second title (Report name)
   lcRange = "B" + lcRow +":" + "P" + lcRow
  .Range(lcRange).Select
  WITH XLApp.SELECTION
    .Merge
    .HorizontalAlignment = 7
    .FONT.NAME='ARIAL'
    .FONT.SIZE=14
    .FONT.BOLD=.T.
    .FONT.ColorIndex = 5
  ENDWITH
  XLApp.ActiveCell.FormulaR1C1 = 'Free to Sell Report'
  *-- Draw Line Under the Header
   lcRange = "A" + lcRow +":" + "S" + lcRow
  .Range(lcRange).Select
  With XLApp.Selection.Borders(9)
    .LineStyle  = 1
    .Weight     = 3
    .ColorIndex = 5
  ENDWITH
   lcRow   = ALLTRIM(STR(lnCurRow))
   lcRange = "Q" + lcRow +":" + "S" + lcRow
  .Range(lcRange).Select
  XLApp.Selection.Merge
  XLApp.ActiveCell.FormulaR1C1='Date :' + DTOC(DATE())		&&print Date in Cell(R2:S2) after merging them

  lcRow2   = ALLTRIM(STR(lnCurRow))
  lcRow    = ALLTRIM(STR(lnCurRow - 1))
  lcRange  = "Q" + lcRow +":" + "S" + lcRow2
  .Range(lcRange).Select
  XLApp.Selection.HorizontalAlignment  =2	&&to make Date align to left
  XLApp.SELECTION.FONT.Bold=.T.
EndWith
*--End Of function lfPageHdr.
*!***************************************************************************
*! Name      : lfDetails
*! Developer : NADER NABIL (NNA)
*! Date      : 01/18/2005
*! Purpose   : Print The details Data in the Excel File
*!***************************************************************************
*! Called from : Icstun20
*!***************************************************************************
*! Example     : =lfDetails()
*!***************************************************************************
FUNCTION lfDetails
WITH XLApp.Sheets("style")
  DO WHILE !EOF()
    lcStyle = &lcTmpFile..CSTYCLR
    lcFr = ALLTRIM(STR(lnRow))
    SCAN REST WHILE CSTYCLR = lcStyle
      IF !(lnPageNo == EVALUATE(lcShwRepFl+'.PageNo')) AND lnRow = 3
        lnPageNo = EVALUATE(lcShwRepFl+'.PageNo')
      ENDIF
      WAIT WINDOW NOWAIT 'Updating Excel file with style :'+lcStyle
      IF TYPE='1'
        IF EVALUATE(lcShwRepFl+'.PageNo')<>lnPageNo AND lnRow >= 3
          lnPageNo = EVALUATE(lcShwRepFl+'.PageNo')
          =lfSkipPage()
        ENDIF
        .Cells(lnRow,1).Value  = ALLTRIM(lcStyle)
        .Cells(lnRow,2).Value  = 'Stock'
        .Cells(lnRow,1).Font.FontStyle  = 'Bold'
        .Cells(lnRow,2).Font.FontStyle  = 'Bold'
        .Cells(lnRow,2).HorizontalAlignment = 4
        FOR I= 1 TO 16
          LCI = ALLTRIM(STR(I))
          .Cells(lnRow,I+2).Value = ALLTRIM(EVALUATE(lcTmpFile+'.CLR&LCI'))
          .Cells(lnRow,I+2).Font.FontStyle = 'Bold'
          .Cells(lnRow,I+2).HorizontalAlignment = 4
        ENDFOR
        .Cells(lnRow,19).Value = 'S.Total'
        .Cells(lnRow,19).Font.FontStyle = 'Bold'
      ENDIF
      IF TYPE='W' AND FLAG1='W'
        .Cells(lnRow,1).Value  = 'First Delivery Due'
      ENDIF

      *--Print Style Desc1
      IF FLAG1='D'
        .Cells(lnRow,1).Value  = ALLTRIM(DESC1)
      ENDIF

      *--Print the first Delivery Due Date
      IF TYPE='W' AND CDATE='D'
        .Cells(lnRow,1).Value  = ErlDate       
        .Cells(lnRow,1).HorizontalAlignment  =2
        .Cells(lnRow,1).Font.ColorIndex = 10
      ENDIF

      *--Print Word 'WIP'
      IF TYPE='S' 
        .Cells(lnRow,2).Value  = 'WIP'
        .Cells(lnRow,2).Font.FontStyle = 'Bold'
        .Cells(lnRow,2).HorizontalAlignment = 4
      ENDIF

	  *--Print Price
      IF TYPE='R' AND lcRpPriLn<>'N' AND EVALUATE(lcTmpFile+'.PRICEA1')>0
        FOR I= 1 TO 16
          LCI = ALLTRIM(STR(I))
          .Cells(lnRow,I+2).Value  = EVALUATE(lcTmpFile+'.PRICEA&LCI')
        ENDFOR
      ENDIF

      *--Print Word (Total) 
      IF TYPE='X'  .OR. (TYPE='R' AND IIF(lcRpPriLn<>'N', EVALUATE(lcTmpFile+'.Pricea16')=0,.T.))
        .Cells(lnRow,18).Value  = 'Total'
      ENDIF

      *--Print the Total Qty.
      IF TYPE$'RX' 
         .Cells(lnRow,19).Value = EVALUATE(lcShwRepFl+'.TotQty')
         .Cells(lnRow,19).Font.FontStyle  = 'Bold'
      ENDIF

      *--Print The Style Sizes.
      IF CDATE=' ' AND IIF(TYPE='W',lcRpRepClc='S',TYPE='Q')
        .Cells(lnRow,2).Value = EVALUATE(lcShwRepFl+'.SIZE')
      ENDIF
      
      *--Print Sizes's Qty.
      IF CDATE=' ' AND IIF(TYPE='W',lcRpRepClc='S',TYPE='Q') 
        FOR I= 1 TO 16
          LCI = ALLTRIM(STR(I))
          IF EVALUATE(lcShwRepFl+'.QTY&LCI') >0
            .Cells(lnRow,I+2).Value = EVALUATE(lcShwRepFl+'.QTY&LCI')
          ENDIF
            *-- Show the Negative Qty with Red color
          IF EVALUATE(lcShwRepFl+'.QTY&LCI') < 0
            .Cells(lnRow,I+2).Value = EVALUATE(lcShwRepFl+'.QTY&LCI')
            .Cells(lnRow,I+2).Font.ColorIndex = 3
          ENDIF
        ENDFOR
        .Cells(lnRow,19).Value = EVALUATE(lcShwRepFl+'.TotQty')
      ENDIF
      
      *--Accumulate the TotQty for Stock
      IF TYPE='Q'
        lnTotFre=lnTotFre+TotQty
      ENDIF
      *--Accumulate the TotQty for WIP
      IF TYPE='W'
        lnTotWip=lnTotWip+TotQty
      ENDIF
      *--Draw Thin Line After every Style Records
      IF TYPE$'RX' AND CDATE = 'x' AND MOD(lnRow,47)<>0 
        lcRow = ALLTRIM(STR(lnRow))
        lcRange = "A" + lcRow +":" + "S" + lcRow
        .RANGE(lcRange).SELECT
        With XLApp.Selection.Borders(9)
          .LineStyle = 1
          .Weight = 2
          .ColorIndex = 5
        EndWith
        XLApp.Selection.Borders(10).LineStyle = Null
        XLApp.Selection.Borders(11).LineStyle = Null
      ENDIF
      lnRow = lnRow + 1      
      *--If we're in a new page
      IF MOD(lnRow,47)=1		&& 47 Represents the Rows Numbers inside the Page
        =lfPageHdr(lnRow)
        lnRow = lnRow + 1      
      ENDIF
    ENDSCAN
  ENDDO
ENDWITH
*--End Of function lfDetails.
*!***************************************************************************
*! Name      : lfSkipPage
*! Developer : NADER NABIL (NNA)
*! Date      : 01/18/2005
*! Purpose   : Loop on the Rows To move the next page
*!***************************************************************************
*! Called from : Icstun20
*!***************************************************************************
*! Example     : =lfSkipPage()
*!***************************************************************************
FUNCTION lfSkipPage
IF MOD(lnRow,47)<> 3
  DO WHILE MOD(lnRow,47)<>0 
    lnRow = lnRow + 1      
  ENDDO
  lnRow = lnRow + 1      
ENDIF
IF MOD(lnRow,47)=1
  =lfPageHdr(lnRow)
  lnRow = lnRow + 1      
ENDIF
*--End Of function lfSkipPage.
*!***************************************************************************
*! Name      : lPError
*! Developer : NADER NABIL (NNA)
*! Date      : 01/18/2005
*! Purpose   : Quit if any error
*!***************************************************************************
*! Called from : ARPINVNC
*!***************************************************************************
*! Example     : =lPError()
*!***************************************************************************
PROCEDURE lPError
PARAMETERS lnError,lcMsg,lnLine
MESSAGEBOX('Error#:'+STR(lnError)+'   '+lcMsg+'   ,'+STR(lnLine))
READ EVENTS
*--End of lPError.
