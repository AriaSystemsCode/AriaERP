*:**********************************************************************************************************
*: Program file  : POSTYVFP.PRG 
*: Program desc. : Style Summary Report.
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: IC2.
*:      Developer: TMI - TAREK MOHAMMED IBRAHIM
*!*	* This program sends the results of a Temp File to 
*!*	* Microsoft Excel for LOW10.
*!* ENTRY        : C# 037373,1
*:**********************************************************************************************************
*: Passed Parameters  : None
*:**********************************************************************************************************
*: Example       : DO POSTYVFP
*:**********************************************************************************************************
* Modifications :
*C123380,1 NNA 10/25/2004 Add new field (Shipped to Date) to the report to get the total shipment Qty.
*C127561,1 MMR 07/10/2005 Add New columns and Print Titles on each page in the Excel
*C127561,1 MMR            and add option print decimal yes/no with modification in FRX.
*:**********************************************************************************************************
LPARAMETERS lcMemoFile

ON ERROR DO lfError WITH ERROR(), MESSAGE(), LINENO()
IF TYPE('lcMemoFile')#'C'
	RETURN
ENDIF
RESTORE FROM (lcMemoFile) ADDI

LOCAL xlsheet,XLApp,tmpsheet

tmpsheet = GetObject(gcWorkDir+lcRpFilNam,'excel.sheet')

XLApp = tmpsheet.application
XLApp.WorkBooks.Add(gcWorkDir+lcRpFilNam)

lnRow = 5
IF !USED(lcCollect)
  USE (gcWorkDir+lcCollect) ORDER 1
ENDIF
*C127561,1 MMR 07/10/2005 Add New columns and Print Titles on each page. [Start]
XLApp.SHEETS(1).PageSetup.PrintTitleRows = "$1:$4"                                                    
*C127561,1 MMR [End]
SELECT (lcCollect)  
LOCATE
WITH XLApp.Sheets("Style")
  *C037373,3  TMI [Start] Print the season / cpurcode descreptions for the XL file
  *.Cells(2,2).Value = lcRpPurcod
  *.Cells(3,2).Value = lcRpSeason
  .Cells(2,2).Value = CPURCDES
  .Cells(3,2).Value = SeasonDES
  *C037373,3  TMI [End  ] 

   *C123380,1 NNA 10/25/2004  (Begin) Change the Position of (Date From & Date To ) 
  *.Cells(2,6).Value = ldRpFromDt
  *.Cells(2,9).Value = ldRpToDt
  .Cells(2,7).Value = ldRpFromDt
  .Cells(2,10).Value = ldRpToDt
   *C123380,1 NNA  (End)
 *C127561,1 MMR 07/10/2005 Add New columns and Print Titles on each page. [Start]
 Store 0 TO lnTotCnt , lnTotShp,lnTotActW,lnTotActD,lnTotStk,lnTotCllW,lnTotCllD,lnTotCp,lnTotCllVlueW,lnTotCllVlue,lnTotCntVlu,lnTotBalnce
*C127561,1 MMR [End]
  DO WHILE !EOF()
    lcStyle = SUBSTR(&lcCollect..CSTYMAJOR,1,lnClrPos-2)
    lcFr = ALLTRIM(STR(lnRow))
    SCAN REST WHILE STYLE = lcStyle
       WAIT WINDOW NOWAIT 'Updating Excel file with style :'+STYLE
      .Cells(lnRow,1).Value  = &lcCollect..CSTYMAJOR
      .Cells(lnRow,2).Value  = &lcCollect..DESC
      .Cells(lnRow,3).Value  = &lcCollect..COLORDES
      .Cells(lnRow,4).Value  = &lcCollect..TOTCNTRCT
     
     *C123380,1 NNA 10/25/2004  (Begin) Add new Column (Shipped To Date) to the Report
     * .Cells(lnRow,5).Value  = &lcCollect..ACTWEEK
      *.Cells(lnRow,7).Value  = &lcCollect..ACTTODATE
      *.Cells(lnRow,10).Value = &lcCollect..STKVAL
      *.Cells(lnRow,11).Value = &lcCollect..INVWEEK
      *.Cells(lnRow,12).Value = &lcCollect..INVTODATE
      *.Cells(lnRow,13).Value = &lcCollect..PRICEA
      *.Cells(lnRow,14).Value = &lcCollect..STKVALCP
      *.Cells(lnRow,15).Value = &lcCollect..STKVALSP
      *.Cells(lnRow,16).Value = &lcCollect..INVVAL
     *C127561,1 MMR 07/10/2005 Add New columns and Print Titles on each page. [Start]
     * .Cells(lnRow,5).Value  = &lcCollect..ShpToDate
      *.Cells(lnRow,6).Value  = &lcCollect..ACTWEEK
      *.Cells(lnRow,8).Value  = &lcCollect..ACTTODATE
      *.Cells(lnRow,11).Value = &lcCollect..STKVAL
      *.Cells(lnRow,12).Value = &lcCollect..INVWEEK
      *.Cells(lnRow,13).Value = &lcCollect..INVTODATE
      *.Cells(lnRow,14).Value = &lcCollect..PRICEA
      *.Cells(lnRow,15).Value = &lcCollect..STKVALCP
      *.Cells(lnRow,16).Value = &lcCollect..STKVALSP
     * .Cells(lnRow,17).Value = &lcCollect..INVVAL
     TtlContVal=  &lcCollect..TOTCNTRCT * &lcCollect..PRICEA
      .Cells(lnRow,5).Value  = &lcCollect..ShpToDate
      .Cells(lnRow,6).Value  = &lcCollect..ACTWEEK
      .Cells(lnRow,8).Value  = &lcCollect..ACTTODATE
      .Cells(lnRow,11).Value = &lcCollect..STKVAL
      .Cells(lnRow,12).Value = &lcCollect..INVWEEK
      .Cells(lnRow,13).Value = &lcCollect..INVTODATE
      .Cells(lnRow,14).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,14).Value = &lcCollect..PRICEA
      .Cells(lnRow,15).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,15).Value = &lcCollect..STKVALCP
      .Cells(lnRow,16).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,16).Value = &lcCollect..STKVALSP
      .Cells(lnRow,17).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,17).Value = &lcCollect..PRICEA *  &lcCollect..INVWEEK
      .Cells(lnRow,18).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,18).Value = &lcCollect..INVVAL
      .Cells(lnRow,19).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,19).Value =  TtlContVal
      .Cells(lnRow,20).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,20).Value = TtlContVal - &lcCollect..INVVAL
      *C123380,1 NNA (End)
      *C127561,1 MMR [End]
      lcTo = ALLTRIM(STR(lnRow))          
      lnRow = lnRow + 1 
     *C127561,1 MMR 07/10/2005 Add New columns and Print Titles on each page. [Start]
     lnTotCnt               =lnTotCnt +&lcCollect..TOTCNTRCT
     lnTotShp               =lnTotShp+&lcCollect..ShpToDate
     lnTotActW              =lnTotActW+&lcCollect..ACTWEEK
     lnTotActD              =lnTotActD+ &lcCollect..ACTTODATE
     lnTotStk               =lnTotStk+&lcCollect..STKVAL
     lnTotCllW              =lnTotCllW+&lcCollect..INVWEEK
     lnTotCllD              =lnTotCllD+&lcCollect..INVTODATE
     lnTotCp                =lnTotCp+&lcCollect..STKVALCP
     lnTotCllVlueW          =lnTotCllVlueW+ &lcCollect..PRICEA *  &lcCollect..INVWEEK
     lnTotCllVlue           =lnTotCllVlue+&lcCollect..INVVAL
     lnTotCntVlu            = lnTotCntVlu+&lcCollect..TOTCNTRCT * &lcCollect..PRICEA 
     lnTotBalnce            =lnTotBalnce+((&lcCollect..TOTCNTRCT * &lcCollect..PRICEA)- &lcCollect..INVVAL)
     *C127561,1 MMR [End]
    ENDSCAN
    lnRow = lnRow + 1
    *C037373,3  TMI [Start] Add the "Subtotal" word
    .Cells(lnRow,3).Value = 'Subtotal'
    .Cells(lnRow,3).Font.FontStyle = 'Bold'
    *C037373,3  TMI [End  ] 
    *- Update the summatino row for current style
    .Cells(lnRow,4).Value = '=SUM(D&lcFr.:D&lcTo.)'
    .Cells(lnRow,5).Value = '=SUM(E&lcFr.:E&lcTo.)'
   
   *C123380,1 NNA 10/25/2004 (Begin) Add new Column as a total for Column (Shipped To Date) Add Rearrange Cells
    *.Cells(lnRow,7).Value = '=SUM(G&lcFr.:G&lcTo.)'
    *.Cells(lnRow,10).Value = '=SUM(J&lcFr.:J&lcTo.)'
    *.Cells(lnRow,11).Value = '=SUM(K&lcFr.:K&lcTo.)'
    *.Cells(lnRow,12).Value = '=SUM(L&lcFr.:L&lcTo.)'
    *.Cells(lnRow,14).Value = '=SUM(N&lcFr.:N&lcTo.)'
    *.Cells(lnRow,16).Value = '=SUM(P&lcFr.:P&lcTo.)'  
    *C127561,1 MMR 07/10/2005 Add New columns and Print Titles on each page. [Start]
    *.Cells(lnRow,6).Value = '=SUM(F&lcFr.:F&lcTo.)'
    *.Cells(lnRow,8).Value = '=SUM(H&lcFr.:H&lcTo.)'
    *.Cells(lnRow,11).Value = '=SUM(K&lcFr.:K&lcTo.)'
    *.Cells(lnRow,12).Value = '=SUM(L&lcFr.:L&lcTo.)'
    *.Cells(lnRow,13).Value = '=SUM(M&lcFr.:M&lcTo.)'
    *.Cells(lnRow,15).Value = '=SUM(O&lcFr.:O&lcTo.)'
    *.Cells(lnRow,17).Value = '=SUM(Q&lcFr.:Q&lcTo.)'  
    .Cells(lnRow,6).Value = '=SUM(F&lcFr.:F&lcTo.)'
    .Cells(lnRow,8).Value = '=SUM(H&lcFr.:H&lcTo.)'
    .Cells(lnRow,11).Value = '=SUM(K&lcFr.:K&lcTo.)'
    .Cells(lnRow,12).Value = '=SUM(L&lcFr.:L&lcTo.)'
    .Cells(lnRow,13).Value = '=SUM(M&lcFr.:M&lcTo.)'
    .Cells(lnRow,15).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,15).Value = '=SUM(O&lcFr.:O&lcTo.)'
    .Cells(lnRow,17).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,17).Value = '=SUM(Q&lcFr.:Q&lcTo.)'  
    .Cells(lnRow,18).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,18).Value = '=SUM(R&lcFr.:R&lcTo.)'
    .Cells(lnRow,19).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,19).Value = '=SUM(S&lcFr.:S&lcTo.)' 
    .Cells(lnRow,20).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,20).Value = '=SUM(T&lcFr.:T&lcTo.)' 
    *C123380,1 NNA (End)
     *C127561,1 MMR [End]
    *- Set summation row to Bold
    .Cells(lnRow,4).Font.FontStyle = 'Bold'
    .Cells(lnRow,5).Font.FontStyle = 'Bold'

   *C123380,1 NNA 10/25/2004 (Begin) Make the new Column (Shipped To Date ) Bold and Rearrange Cells
   * .Cells(lnRow,7).Font.FontStyle = 'Bold'
    *.Cells(lnRow,10).Font.FontStyle = 'Bold'
    *.Cells(lnRow,11).Font.FontStyle = 'Bold'
    *.Cells(lnRow,12).Font.FontStyle = 'Bold'
    *.Cells(lnRow,14).Font.FontStyle = 'Bold'
    *.Cells(lnRow,16).Font.FontStyle = 'Bold'
    .Cells(lnRow,6).Font.FontStyle = 'Bold'
    .Cells(lnRow,8).Font.FontStyle = 'Bold'
    .Cells(lnRow,11).Font.FontStyle = 'Bold'
    .Cells(lnRow,12).Font.FontStyle = 'Bold'
    .Cells(lnRow,13).Font.FontStyle = 'Bold'
    .Cells(lnRow,15).Font.FontStyle = 'Bold'
    .Cells(lnRow,17).Font.FontStyle = 'Bold'
     *C127561,1 MMR 07/10/2005 Add New columns and Print Titles on each page. [Start]
    .Cells(lnRow,18).Font.FontStyle = 'Bold'
    .Cells(lnRow,19).Font.FontStyle = 'Bold'
    .Cells(lnRow,20).Font.FontStyle = 'Bold'
     *C123380,1 NNA (End)
     *C127561,1 MMR [End]
    lnRow = lnRow + 2
  ENDDO
 *C127561,1 MMR 07/10/2005 Add New columns and Print Titles on each page. [Start]
*ENDWITH
    lnRow = lnRow + 1
    .Cells(lnRow,3).Value = 'Dept total'
    .Cells(lnRow,3).Font.FontStyle = 'Bold'
    .Cells(lnRow,4).Value =lnTotCnt  
    .Cells(lnRow,5).Value =lnTotShp  
    .Cells(lnRow,6).Value = lnTotActW 
    .Cells(lnRow,8).Value =lnTotActD  
    .Cells(lnRow,11).Value = lnTotStk 
    .Cells(lnRow,12).Value =lnTotCllW  
    .Cells(lnRow,13).Value = lnTotCllD
    .Cells(lnRow,15).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,15).Value = lnTotCp 
    .Cells(lnRow,17).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,17).Value = lnTotCllVlueW 
    .Cells(lnRow,18).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,18).Value = lnTotCllVlue     
    .Cells(lnRow,19).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,19).Value =lnTotCntVlu  
    .Cells(lnRow,20).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,20).Value =lnTotBalnce          
    .Cells(lnRow,4).Font.FontStyle = 'Bold'
    .Cells(lnRow,5).Font.FontStyle = 'Bold'
    .Cells(lnRow,6).Font.FontStyle = 'Bold'
    .Cells(lnRow,8).Font.FontStyle = 'Bold'
    .Cells(lnRow,11).Font.FontStyle = 'Bold'
    .Cells(lnRow,12).Font.FontStyle = 'Bold'
    .Cells(lnRow,13).Font.FontStyle = 'Bold'
    .Cells(lnRow,15).Font.FontStyle = 'Bold'
    .Cells(lnRow,17).Font.FontStyle = 'Bold'
    .Cells(lnRow,18).Font.FontStyle = 'Bold'
    .Cells(lnRow,19).Font.FontStyle = 'Bold'
    .Cells(lnRow,20).Font.FontStyle = 'Bold'
ENDWITH
*C127561,1 MMR [End] 
XLApp.Visible = .T.

ERASE (lcMemoFile)

*:**********************************************************************************************************
*  TMI - TAREK MOHAMED IBRAHIM                  *C#037373,1
*  2/11/2004
*  Quit if any error 
*:**********************************************************************************************************
FUNCTION lfError
PARAMETERS lnError,lcMsg,lnLine
DO FORM FRMERR.SCX WITH lnError,lcMsg,lnLine
READ EVENTS
*--End of lfError