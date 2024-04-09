***********************************************************************
*:  Program File: ARDSHBR.PRG
*:  Desc.       : Dashboard Data Collection
*:  System      : Aria 4XP
*:  Developer   : MMT - Mariam Mazhar
*:  Date        : 01/10/2019
*:  Reference   : E304101,1
*:************************************************************************
* Modifications:
*E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001]
*-------------------------------------------------------------------------
PARAMETERS lcRequestID, lcXMLFileName, CLIENTID
   *=STRTOFILE('Mariam Begin',"d:\shared\test.txt",1)

lcROutpFile= ''
IF  TYPE('lcXMLFileName') = 'C'
*XX
*!*	  =STRTOFILE(CHR(13)+CHR(10)+"RestoreXML","d:\shared\test.txt",1)
*!*	  ON ERROR  STRTOFILE(CHR(13)+CHR(10)+MESSAGE(),"d:\shared\test.txt",1)
*!*	  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
*!*	  =STRTOFILE(CHR(13)+CHR(10)+"After RestoreXML","d:\shared\test.txt",1)  
*XXX
ELSE 
 * =STRTOFILE(CHR(13)+CHR(10)+"Before OG","d:\shared\test.txt",1)  
  lcExpr = gfOpGrid('ARDSHBR' , .T.)  && Run selection grid.
 * =STRTOFILE(CHR(13)+CHR(10)+"After OG","d:\shared\test.txt",1)    
ENDIF  
*=STRTOFILE(CHR(13)+CHR(10)+TYPE('lcXMLFileName'),"d:\shared\test.txt",1)
*_Screen.Visible =.t.
*!*	SET STEP ON 
IF IIF(TYPE('lcXMLFileName') = 'C',.T.,TYPE('lcExpr') <> 'L' AND lcExpr <> '.F.')
  * =STRTOFILE('Mariam Begin1',"d:\shared\test.txt",1)
  IF TYPE('lcXMLFileName') = 'C'
    PRIVATE loAgent
    PRIVATE loProgress
    *=STRTOFILE('before loProgress ','D:\Shared\Test.txt',1)
    loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
    *=STRTOFILE('after loProgress ','D:\Shared\Test.txt',1)
    LOAGENT = GOREMOTECALL.GETREMOTEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
     *=STRTOFILE('after LOAGENT ','D:\Shared\Test.txt',1)
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
      *=STRTOFILE('after UpdateObjectProgress','D:\Shared\Test.txt',1)
    LOCAL loEnvironment
    LOENVIRONMENT = GOREMOTECALL.GETREMOTEOBJECT("Aria.Environment.AriaEnviromentVariables")
   * =STRTOFILE('after LOENVIRONMENT ','D:\Shared\Test.txt',1)
    loEnvironment.ClientID = ClientID
     *   =STRTOFILE('after ClientID ','D:\Shared\Test.txt',1)
    loEnvironment.ConnectionsRefresh()
    * =STRTOFILE('after ConnectionsRefresh','D:\Shared\Test.txt',1)
    LOCAL lcCurrentProcedure
    LCCURRENTPROCEDURE =    LOENVIRONMENT.ARIA40SHAREDPATH
    *  =STRTOFILE('after LCCURRENTPROCEDURE ','D:\Shared\Test.txt',1)
    DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID),ClientID
    * =STRTOFILE('after ariamain','D:\Shared\Test.txt',1)
    loRequestObj = loAgent.GetRequest(lcRequestID, ClientID)
    *XX
  *   =STRTOFILE(CHR(13)+CHR(10)+"RestoreXML","d:\shared\test.txt",1)
  *ON ERROR  STRTOFILE(CHR(13)+CHR(10)+MESSAGE(),"d:\shared\test.txt",1)
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
 * =STRTOFILE(CHR(13)+CHR(10)+"After RestoreXML","d:\shared\test.txt",1) 
  *XXX
    lcROutpFile  = lcRpFile
  ENDIF
  lcWorkFile = "Dashboard"&&gfTempName()
  lfCreateCursor()
 *  =STRTOFILE('Create Cursor','D:\Shared\Test.txt',1)
  IF !USED('INVHDR')
    =gfOpenTable('INVHDR','INVHDR')
  ENDIF

  IF !USED('ORDHDR')
    =gfOpenTable('ORDHDR','ORDHDR')
  ENDIF

  IF !USED('RETHDR')
    =gfOpenTable('RETHDR','RETHDR')
  ENDIF

  IF !USED('POSHDR')
    =gfOpenTable('POSHDR','POSHDR')
  ENDIF
  IF !USED('POSLN')
    =gfOpenTable('POSLN','POSLN')
  ENDIF

  IF !USED('ORDCANLN')
    =gfOpenTable('ORDCANLN','ORDCANLN')
  ENDIF

  IF !USED('DEBIT')
    =gfOpenTable('DEBIT','DEBIT')
  ENDIF
  *E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][Start]
  IF !USED('ARHIST')
    =gfOpenTable('ARHIST','ARHISTT')
  ENDIF
  IF !USED('CREDIT')
    =gfOpenTable('CREDIT','CREDIT')
  ENDIF
  *E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][End]
  * =STRTOFILE('After Open tables','D:\Shared\Test.txt',1)
  ldToday = DATE()
  lnMonth = MONTH(ldToday)
  lnYear =  YEAR(ldToday)
  *E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][Start]
  ldStartDate = DATE(lnYear -1,lnMonth ,1)
  ldStartDate = CTOD('01/01/2017')
  lnMonthCnt = INT((DATE()-CTOD('01/01/2017'))/30)
  *E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][End]
  ldEndDate = GOMONTH(ldStartDate,1)-1
  lnCntMonth = 1
  lnOutStandingAmt = 0
  lnDebit = 0
  SELECT DEBIT
  =gfSeek('')
  LOCATE 
  SUM Amount TO lnDebit FOR !DELETED() ALL 

  lnCredit = 0
  SELECT Credit
  =gfSeek('')
  LOCATE 
  SUM ABS(Amount) TO lnCredit FOR !DELETED() ALL 

  lnOutStandingAmt = lnDebit  - lnCredit 
  *E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][Start]
  *DO WHILE (lnCntMonth <=13)
  DO WHILE (Year(ldStartDate) < YEAR(DATE())  OR (Year(ldStartDate)=YEAR(DATE()) AND  MONTH(ldStartDate) <= MONTH(DATE())))
  *E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][End]
    IF TYPE('lcXMLFileName') = 'C'
      *E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][Start]
*      loProgress.Percent     = lnCntMonth / 13      
      loProgress.Percent     = lnCntMonth / lnMonthCnt 
      *E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][End]
      loProgress.DESCRIPTION = "Collecting Data..."
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, CLIENTID)
    ENDIF
    lfGetRequiredData(ldStartDate,ldEndDate)
    ldStartDate = GOMONTH(ldStartDate,1)
    ldEndDate = GOMONTH(ldStartDate,1)-1
    lnCntMonth = lnCntMonth +  1 
  ENDDO
  SELECT(lcWorkFile)
  IF FILE(lcROutpFile )
    ERASE (lcROutpFile)
  ENDIF
   ***X
*  COPY TO (lcROutpFile) TYPE XLS 
*\\10.0.1.8\lac99sh\Aria4XP\Classes\vfpxworkbookxlsx.vcx
  *lcTmpFile = oAriaApplication.workDir+"Dashboard"+".dbf" 
 * COPY TO (lcTmpFile)

*:B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][Start]
*lcXTmpFile = FORCEEXT(lcTmpFile ,".XLSX")
lcExcel = lcROutpFile
*lcTable = lcTmpFile 
* =STRTOFILE(CHR(13)+CHR(10)+"before XL object","d:\shared\test.txt",1)
loExcel = NEWOBJECT("VFPxWorkbookXLSX", "\\10.0.1.8\lac99sh\Aria4XP\Classes\vfpxworkbookxlsx.vcx")
loExcel.SaveTabletoWorkbook(lcWorkFile, lcExcel, .T., .T.)





*!*	 ON ERROR  STRTOFILE(CHR(13)+CHR(10)+MESSAGE(),"d:\shared\test.txt",1)
*!*	oExcelSheet = CREATEOBJECT('Excel.Application') 
*!*	 =STRTOFILE(CHR(13)+CHR(10)+"after XL object","d:\shared\test.txt",1)
*!*	loFileExcel =oExcelSheet.Workbooks.Open (lcTmpFile)
 *=STRTOFILE(CHR(13)+CHR(10)+"after open file XLS","d:\shared\test.txt",1)
*!*	loSheet = loFileExcel.Sheets [1]
*!*	loSheet.Range ("A1")= "ProductNumber"
*!*	loSheet.Range ("B1")= "ColorCode"
*!*	loSheet.Range ("C1")= "GenderCode"
*!*	loSheet.Range ("D1")= "SizeCode"
*!*	loSheet.Range ("E1")= "AvailableQuantity"
*!*	loSheet.Range ("F1")= "AvailableDate"
*!*	loSheet.Range ("G1")= "SeasonCode"
*!*	loSheet.Range ("H1")= "DivisionCode"
*loFileExcel.SaveAs(lcROutpFile,51)
* =STRTOFILE(CHR(13)+CHR(10)+"after save as file XLS","d:\shared\test.txt",1)
*loFileExcel.Close (.t.)
*oExcelSheet = NULL
*loFileExcel = NULL
*lcTmpFile  = lcXTmpFile
loExcel = NULL
  ***X
  
  
   *  =STRTOFILE('After Copying XLS','D:\Shared\Test.txt',1)
  IF FILE(lcROutpFile) AND TYPE('lcXMLFileName') <> 'C'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'File: '+ALLTRIM(lcROutpFile)+' has been created.')
  ENDIF
ELSE
  RETURN                                 
ENDIF                                 
********************************************************************************
*Name       : lfGetRequiredData
*Developer  : MMT
*Date       : 01/10/2019
*Purpose    : Get Required Data
********************************************************************************
FUNCTION lfGetRequiredData
PARAMETERS lpdStartDate,lpdEndDate
IF TYPE('lcXMLFileName') <> 'C'
  WAIT WINDOW  "Collecting Data...." NOWAIT 
ENDIF
lnMonth = MONTH(lpdStartDate)
lnYear =  YEAR(lpdStartDate)

lnBook=0
lnBookAmt=0
lnCancel=0
lnCancelAmt=0
lnOpen=0
lnOpenAmt=0
lnCreditAmt = 0
*E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][Start]
lnCashRecp = 0
SELECT Credit
=gfSetOrder('CRTRAN')   && TRANTYPE+TRAN
=gfSeek('4')
SCAN REST WHILE TRANTYPE+TRAN = '4' FOR BETWEEN(Trandate,ldStartDate ,ldEndDate) AND !DELETED()
  lnCashRecp = lnCashRecp + ABS(Amount)
ENDSCAN 
SELECT ARHIST
=gfSetOrder('ARHISTT')
=gfSeek('')
LOCATE
SCAN FOR TRANTYPE ='4' AND BETWEEN(Trandate,ldStartDate ,ldEndDate) AND !DELETED()
  lnCashRecp = lnCashRecp + ABS(Amount)
ENDSCAN 
*E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][End]
= gfSqlRun("Select SUM(TOTCREDIT) As 'TotCredit' From RETHDR Where Status <>'V' AND CRDATE BETWEEN ?lpdStartDate and ?lpdEndDate",'RETHDR',.T.,'RETHDR')                        
lnCreditAmt = IIF(ISNULL(RETHDR.TotCredit),0,RETHDR.TotCredit)

SELECT ORDHDR
gfseek('O')
SCAN REST WHILE cordtype+order='O' FOR BETWEEN(Entered,ldStartDate ,ldEndDate )
  lnBook = lnBook + book
  lnBookAmt = lnBookAmt + bookamt
ENDSCAN 

SELECT ORDHDR
gfseek('O')
SCAN REST WHILE cordtype+order='O' FOR BETWEEN(Complete,ldStartDate ,ldEndDate )
  lnOpen = lnOpen + open
  lnOpenAmt = lnOpenAmt + openamt
ENDSCAN 

SELECT 'ORDCANLN'
=gfSeek('O')
SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) ='O' FOR BETWEEN(CANCELLED,ldStartDate ,ldEndDate )
  lnCancel = lnCancel + TotQty
  lnCancelAmt = lnCancelAmt + (TotQty*Price)
ENDSCAN

lnShip=0
lnShipAmt=0
lnDisAmt=0
SELECT INVHDR
=gfSeek('')
LOCATE
SCAN FOR BETWEEN(invdate,ldStartDate ,ldEndDate) 
  lnShip = lnShip + ship
  lnShipAmt = lnShipAmt + shipamt
  lnDisAmt = lnDisAmt + ABS(discount)
ENDSCAN 

lnPoQty=0
lnPoAmt=0
lnRecQty=0
lnRecAmt=0
lnPoOpenQty=0
lnPoOpenAmt=0

= gfSqlRun("Select SUM(POSLN.TOTQTY) As 'POTotQty',SUM(POSLN.TotQty * (POSLN.niCost1+POSLN.niCost2+POSLN.niCost3+POSLN.niCost4+POSLN.niCost5+POSLN.niCost6+POSLN.niCost7)) As 'PoAmt' From "+;
           " POSHDR INNER JOIN POSLN ON POSLN.cStyType = POSHDR.CSTYTYPE and POSLN.cBusDocu= POSHDR.cBusDocu And POSLN.PO= POSHDR.PO "+;
           " Where POSHDR.CBUSDOCU ='P' AND POSHDR.CSTYTYPE ='P' AND POSLN.Trancd ='1' AND POSHDR.Status <>'X' AND Poshdr.ENTERED BETWEEN ?m.lpdStartDate and ?m.lpdEndDate",'POSHDR',.T.,'POSHDR')

lnPoQty = IIF(ISNULL(POSHDR.POTotQty),0,POSHDR.POTotQty)
lnPoAmt = IIF(ISNULL(POSHDR.PoAmt),0,POSHDR.PoAmt)


= gfSqlRun("Select SUM(POSLN.TOTQTY) As 'RecTotQty',SUM(POSLN.TotQty * (POSLN.niCost1+POSLN.niCost2+POSLN.niCost3+POSLN.niCost4+POSLN.niCost5+POSLN.niCost6+POSLN.niCost7)) As 'RecAmt' From "+;
           " POSHDR INNER JOIN POSLN ON POSLN.cStyType = POSHDR.CSTYTYPE and POSLN.cBusDocu= POSHDR.cBusDocu And POSLN.PO= POSHDR.PO "+;
           " Where POSHDR.CBUSDOCU ='P' AND POSHDR.CSTYTYPE ='P' AND POSLN.Trancd ='2' AND POSHDR.Status <>'X' AND Posln.[Date]  BETWEEN ?m.lpdStartDate and ?m.lpdEndDate",'POSHDR',.T.,'POSHDR')


lnRecQty= IIF(ISNULL(POSHDR.RecTotQty),0,POSHDR.RecTotQty)
lnRecAmt = IIF(ISNULL(POSHDR.RecAmt),0,POSHDR.RecAmt)

= gfSqlRun("Select SUM((Case(Trancd) when 1 "+;
             "then 1 Else -1 End) * Posln.TotQty) As 'OpTotQty',SUM((Case(Trancd) when 1 "+;
             "then 1 Else -1 End) * POSLN.TotQty * (POSLN.niCost1+POSLN.niCost2+POSLN.niCost3+POSLN.niCost4+POSLN.niCost5+POSLN.niCost6+POSLN.niCost7)) As 'OpnAmt' From "+;
           " POSHDR INNER JOIN POSLN ON POSLN.cStyType = POSHDR.CSTYTYPE and POSLN.cBusDocu= POSHDR.cBusDocu And POSLN.PO= POSHDR.PO "+;
           " Where POSHDR.CBUSDOCU ='P' AND POSHDR.CSTYTYPE ='P' AND POSHDR.Status <>'X' AND Poshdr.[Complete] BETWEEN ?m.lpdStartDate and ?m.lpdEndDate",'POSHDR',.T.,'POSHDR')

lnPoOpenQty = IIF(ISNULL(POSHDR.OpTotQty),0,POSHDR.OpTotQty)
lnPoOpenAmt = IIF(ISNULL(POSHDR.OpnAmt),0,POSHDR.OpnAmt)

*E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][Start]
*!*	SELECT INVHDR
*!*	SCAN FOR duedate  <= ldEndDate &&BETWEEN(duedate,ldStartDate ,ldEndDate) 
*!*	  IF gfSEEK(INVHDR.Account+INVHDR.Invoice,'Debit','Debit')
*!*	    lnOutStandingAmt = lnOutStandingAmt + INVHDR.totalchg
*!*	  ELSE
*!*	    IF gfSEEK(INVHDR.Account+INVHDR.Invoice,'ARHIST','ARHISTT')  && ACCOUNT+TRAN+CINSTALNO
*!*	      SELECT ARHIST
*!*	      LOCATE REST WHILE ACCOUNT+TRAN+CINSTALNO = INVHDR.Account+INVHDR.Invoice FOR TranType ='1'
*!*	      IF FOUND()
*!*	        IF ARHIST.HISTDATE > ldEndDate
*!*	          lnOutStandingAmt = lnOutStandingAmt + INVHDR.totalchg
*!*	        ENDIF
*!*	      ENDIF  
*!*	    ENDIF
*!*	  ENDIF  
*!*	ENDSCAN 
*E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][End]

*E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][Start]
*!*	INSERT INTO (lcWorkFile) Values(MONTH(ldStartDate),YEAR(ldStartDate),lnDisAmt,lnCreditAmt,lnOutStandingAmt,lnBookAmt,lnBook,lnShip,lnShipAmt ,;
*!*	                                lnCancel,lnCancelAmt,lnOpen,lnOpenAmt ,lnPoOpenQty ,lnPoOpenAmt ,lnRecQty,lnRecAmt ,lnPoOpenQty ,lnPoOpenAmt)
*XXX 
*!*	INSERT INTO (lcWorkFile) Values(MONTH(ldStartDate),YEAR(ldStartDate),lnDisAmt,lnCreditAmt,lnOutStandingAmt,lnBookAmt,lnBook,lnShip,lnShipAmt ,;
*!*	                                lnCancel,lnCancelAmt,lnOpen,lnOpenAmt ,lnPoQty,lnPoAmt ,lnRecQty,lnRecAmt ,lnPoOpenQty ,lnPoOpenAmt)
INSERT INTO (lcWorkFile) Values(MONTH(ldStartDate),YEAR(ldStartDate),lnDisAmt,lnCreditAmt,lnOutStandingAmt,lnBookAmt,lnBook,lnShip,lnShipAmt ,;
                                lnCancel,lnCancelAmt,lnOpen,lnOpenAmt ,lnPoQty,lnPoAmt ,lnRecQty,lnRecAmt ,lnPoOpenQty ,lnPoOpenAmt,lnCashRecp)
*E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][End]
********************************************************************************
*Name       : lfCreateCursor
*Developer  : MMT
*Date       : 01/10/2019
*Purpose    : Create Cursor
********************************************************************************
FUNCTION lfCreateCursor
*E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][Start]
*!*	CREATE CURSOR (lcWorkFile) (MONTH N(2),Year N(4),Discount_Amount N(12,3),Return_Credit_Memo N(12,3),Outstanding_AR_Transaction N(12,3),Booked_Amount N(12,3),Booked_Quantity N(9),Shipped_Quantity N(9),Shipped_Amount N(12,3),;
*!*	                            CANCELLED_Quantity N(9),CANCELLED_Amount N(12,3),Outstanding_open_Order_Qty N(9),Outstanding_open_Order_Amt N(12,3),Purchase_Import_Qty N(9),Purchase_Import_Amt N(12,3),Receipt_Ship_Import_Qty N(9),Receipt_Ship_Import_Amt N(12,3),Outstanding_Import_Qty N(9),Outstanding_Import_Amt N(12,3))
CREATE CURSOR (lcWorkFile) (MONTH N(2),Year N(4),Discount_Amount N(12,3),Return_Credit_Memo N(12,3),Outstanding_AR_Transaction N(12,3),;
							Booked_Amount N(12,3),Booked_Quantity N(9),Shipped_Quantity N(9),Shipped_Amount N(12,3),;
                            CANCELLED_Quantity N(9),CANCELLED_Amount N(12,3),Outstanding_open_Order_Qty N(9),Outstanding_open_Order_Amt N(12,3),;
                            Purchase_Import_Qty N(9),Purchase_Import_Amt N(12,3),Receipt_Ship_Import_Qty N(9),Receipt_Ship_Import_Amt N(12,3),;
                            Outstanding_Import_Qty N(9),Outstanding_Import_Amt N(12,3),Cash_Receipt  N(12,3))
			    		
*E612006,1 MMT 1/21/2020 Displaye all years and cash receipts pers month[T20191220.0001][End]
********************************************************************************
*Name       : lfvPath
*Developer  : MMT
*Date       : 01/10/2019
*Purpose    : Validate file and path
********************************************************************************
FUNCTION lfvPath
IF (!EMPTY(lcRpFile) AND !DIRECTORY(JUSTPATH(lcRpFile))) OR '?' $ lcRpFile
  *XX
  *  lcRpFile= GETFILE('XLS','Output File','Save',1)
  lcRpFile= GETFILE('XLSX','Output File','Save',1)
  *XX
ENDIF				    		
lcROutpFile =lcRpFile
********************************************************************************
*Name       : lfwRepWhen
*Developer  : MMT
*Date       : 01/10/2019
*Purpose    : Validate file and path
********************************************************************************
FUNCTION lfwRepWhen
lcROutpFile =lcRpFile