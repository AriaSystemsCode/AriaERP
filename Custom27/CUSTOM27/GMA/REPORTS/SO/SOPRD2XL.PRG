*!**********************************************************************************************************
*! Program file  : Soprd2Xls.PRG 
*! Program desc. : Export the [Production Status Report] to Excel
*! System        : ARIA APPAREL SYSTEM 2.7
*! Module        : SO
*! Developer     : NADER NABIL - NNA
*! Date          : 03/04/2005
*! Purpose       : This program sends the results of a Temp File to MS Excel for GMA10.
*! REF.          : C#124806,1
*!**********************************************************************************************************
*! Passed Parameters  : None
*!**********************************************************************************************************
*! Example       : DO SOPRD2XLS
*!**********************************************************************************************************
*! Modifications :
*!**********************************************************************************************************
LPARAMETERS lcMemoFile
PRIVATE lcCentury , RpSheet
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

IF !USED(lcTmpFile)
  USE (gcWorkDir+lcTmpFile) ORDER 1 IN 0
ENDIF

*--Create a new Excel file
RpSheet = CREATEOBJECT("EXCEL.APPLICATION")
WITH RpSheet
  .WORKBOOKS.ADD
  .ACTIVEWORKBOOK.SHEETS(1).ACTIVATE
  *--Give the Report Name to the Excel sheet1
  .SHEETS(1).NAME ='Production Status Report'
  *--Repeat the Row1 Headers with Every Page
  .SHEETS(1).PageSetup.PrintTitleRows = "$1:$1"                                                    
  .SHEETS(1).PageSetup.Orientation =2
  FOR I = 1 TO ALEN(laHdrs,1)
    .Cells(1,I).VALUE=laHdrs[I]
    .Cells(1,I).Font.FontStyle = 'Bold'
  ENDFOR
  ROW = 2
  SELECT(lcTmpFile)
  SCAN
    WAIT WINDOW NOWAIT 'Updating Excel file with data for order :'+ EVAL(lcTmpFile+'.ORDER')
    FOR I = 3 TO FCOUNT()
      FLDNAME = FIELD(I)
      DO CASE 
        CASE TYPE(FLDNAME)='D'
          .Cells(ROW,I-2).VALUE = IIF(EMPTY(&FLDNAME),'',&FLDNAME)
        CASE TYPE(FLDNAME)='N'
          .Cells(ROW,I-2).VALUE = IIF(EMPTY(&FLDNAME),0,&FLDNAME)
        CASE TYPE(FLDNAME)='C' OR TYPE(FLDNAME)='M'
          *-- format Cell as Text if the table field is a Character type
          .Cells(ROW,I-2).NumberFormat = "@"
          .Cells(ROW,I-2).VALUE = &FLDNAME
        OTHERWISE
          .Cells(ROW,I-2).VALUE = &FLDNAME
      ENDCASE
    ENDFOR
    ROW = ROW + 1
  ENDSCAN
  *-- Columns Auto Fit
  .Columns("A:AG").EntireColumn.AutoFit
  DECLARE laCol[8]
  laCol[1] = 'M'
  laCol[2] = 'N'
  laCol[3] = 'O'
  laCol[4] = 'P'
  laCol[5] = 'Q'
  laCol[6] = 'R'
  laCol[7] = 'S'
  laCol[8] = 'T'
  FOR I=1 TO 8
    IF EMPTY(laSizes[I])
      .Columns("&laCol[I]:&laCol[I]").EntireColumn.HIDDEN=.T.
    ENDIF
  ENDFOR
ENDWITH
DO CASE
ENDCASE
RpSheet.VISIBLE = .T.
ERASE (lcMemoFile)
RELEASE RpSheet

SET CENTURY &lcCentury

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
