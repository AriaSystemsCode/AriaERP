*:***************************************************************************
*: Program file  : ARSCALFG.PRG
*: Program desc. : CUSTOMIZED ACTIVITY SCALE QUANTITY REPORT FOR F&G.
*: Date          : 09/12/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C200124
*:
*:***************************************************************************
*: Calls :
*:    Functions  : lfChngDat  , lfDatHdr   , lfCreatTmp , lfRelation , lfFillARAY
*:               : lfvTerm    , lfvDvsion  , lfvRegion  , lfvShpVia  , lfvSpcIns 
*:               : lfvCustom  , lfvSason   , lfvStyGrp  , lfvRoyalty , lfvPurchas
*:               : lfvCratVar , lfvCutSty  , lfcolctDat , lfNetSals  , lfCrdtMem
*:               : lfNetSize  , lfCrdtSiz  , lfChange   , lfQuntity  , lfSubTot
*:               : lfGrndTot  , lfSizQnty  , lfStlSize  , lfSubStyl  , lfSubScal
*:               : lfSubAcnt  , lfGrandTot , lfTotSize  , lfTotQnty  , lfvDateRng
*:               : lfvChekF   , lfvChekTo  , lfvPbOk    , lfsrRep    , lfEvalSegs
*:               : lfAccount  , lfClarAll  , lfClrRead  , lfSRVSty   , lfQunLop
*:               : lfReSort
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARSCALFG
*:***************************************************************************
*: Modification        :
*:**************************************************************************
*:B604006,1 BWA 12/11/2000 1) Fix the bug of printing the account code & name 
*:B604006,1                   in case the user select no to print them.[FRX]
*:B604006,1                2) Modify the header of the monthes in the reports
*:B604006,1                3) ADD the credit limit field to the reports.
*:B604236,1 BWA 26/02/2001 1) ADD the Discount field to the reports.[PRG & FRX]
*:B604236,1                2) Fix the bug some lines for the colors get befor the title.
*:**************************************************************************

*--Get the full picture of the date.
lcCentury  = SET('CENTURY')
SET CENTURY ON

*--The transaction month must be fill with a date to get from it the month.
IF ldRpFMon = {}
  =gfModalGen('TRM00250B00000','DIALOG', 'Transaction Month')
  RETURN
ENDIF

*-- Get the Variables its Initial value
STORE SPACE(0) TO lcFilter   , lcTerms  , lcRegion   , lcShipvia , lcSpeIns , lcCusClass
STORE SPACE(0) TO lcDivision , lcSeason , lcStylGrp  , lcRoyalty , lcPurchase
STORE SPACE(0) TO lcStylFlt  , lcColrFlt, lcStyFtRet , lcClrFtRet
STORE  0       TO lnDatStyl  , lnDatColr

llFlgDtl = .T.      && Flag used in Collecting data in Detail report.

IF !USED(lcStyle)
  =gfOpenFile(gcDataDir+"Style","Style", 'SH', @lcStyle, .T.)
ENDIF

IF !USED(lcScalHd)
  =gfOpenFile(gcDataDir+"ScaleHd","Extscale", 'SH', @lcScalHd, .T.)
ENDIF

*-- Check if the user open the transaction month option or not 
IF llChkdat
  =lfChngDat()
ENDIF

*B604236,1 [START]
*--Case the user select Detail and Sub Total By Customer = 'NO'
IF lcFormat = 'D' AND llSubCust = .F.
  lcColect = gfTempName()
ENDIF
*B604236,1 [END]

=lfDatHdr()
=lfCreatTmp()
=lfRelation()
=lfvCrATVar()
=lfColctDat()

*B604236,1 [START]
*--Case the user select Detail and Sub Total By Customer = 'NO'

*-- llReplac this variable to check if we calculate the styles in 
*-- one record or not,if there is no record to calculate there 
*-- is no work to delete all the records in the lclines file 
*-- and append from the lccolect.--*

IF lcFormat = 'D' AND llSubCust = .F.
  SELECT (lclines)
  llReplac = .F.
  =lfSortStyl()
ENDIF
*B604236,1 [END]

SELECT (lclines)
SET ORDER TO TAG SALESREP
SET RELATION TO &lclines..SALESREP INTO SALESREP ADDITIVE
LOCATE

DO gfDispRe WITH EVALUATE('lcRpForm') , 'FOR ' + "&lclines..TotQnt <> 0 "

*-- use this function to erase the tmp. files and its index in case the user
*-- select sunnary report then select the detail report directly.
=lfClarAll()

SET CENTURY &lcCentury

      ***************** END ACTIVITY SCALE QUANTITY REPORT *****************

*!*************************************************************
*! Name      : lfChngDat
*! Developer : Bassem Rafaat (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function to Change Date to the first day of the
*!           : month.
*!*************************************************************
*! Called from : ARSCALFG.PRG
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfChngDat

*-- Change the date to be look like the report in months not in days.
*-- The change in the from only to be between the first of the month
*-- and the date of the system date if the user didn't open the option
*-- of the transaction month.
IF DAY(ldRpFMon) # 01
  IF SET('DATE') = "BRITISH"
    ldRpFMon = CTOD('01'+'/'+STR(MONTH(ldRpFMon))+'/'+STR(YEAR(ldRpFMon)))
  ELSE
    ldRpFMon = CTOD(STR(MONTH(ldRpFMon))+'/01/'+STR(YEAR(ldRpFMon)))
  ENDIF
ENDIF

*-- End of lfChngDat.
*!*************************************************************
*! Name      : lfDatHdr
*! Developer : Bassem Rafaat (BWA)
*! Date      : 09/12/2000
*! Purpose   : 1) Function to get Date Header.
*!           : 2) Make array to use it in filtering the date.
*!*************************************************************
*! Called from : ARSCALFG.PRG
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfDatHdr

*-- Make the arrays dimension 13 to use the laFltrDate array in the
*-- Do case in collecting the data.

DIMENSION laDateTitl[13],laYearTitl[13],laNoOfDays[13] ,laFltrDate[13]
STORE {} TO laDateTitl , laFltrDate

laNoOfDays = '31'
STORE '30' TO laNoOfDays[4],laNoOfDays[6],laNoOfDays[9],laNoOfDays[11]

lcPrdMonth = ALLTRIM(STR(MONTH(ldRpFMon)))
lnPrdMonth = INT(VAL(ALLTRIM(STR(MONTH(ldRpFMon)))))
lcPrdYear  = ALLTRIM(STR(YEAR(ldRpFMon)))

IF SET('DATE') = "BRITISH"
  FOR I=1 TO 13
    laDateTitl[I] = ALLTRIM(CMONTH(CTOD('01'+"/"+lcPrdMonth+"/"+lcPrdYear)))

    *B604006,1 BWA 12/11/2000 Modify the header of the monthes in the reports.[START]
    laDateTitl[I] = LEFT(laDateTitl[I],3)+'.'
    *B604006,1 [END]

    laYearTitl[I] = ALLTRIM(lcPrdYear)
    laNoOfDays[2] = IIF(MOD(YEAR(CTOD(lcPrdMonth+'/01/'+lcPrdYear)),4)=0 ,'29' ,'28' )
    ldNewDate     = (CTOD(ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdMonth+'/'+lcPrdYear)+1)
    laFltrDate[I] = CTOD('01'+'/'+lcPrdMonth+'/'+lcPrdYear)
    lcPrdMonth    = ALLTRIM(STR(MONTH(ldNewDate)))
    lnPrdMonth    = INT(VAL(lcPrdMonth))
    lcPrdYear     = ALLTRIM(STR(YEAR(ldNewDate)))
  ENDFOR
ELSE
  FOR I=1 TO 13
    laDateTitl[I] = ALLTRIM(CMONTH(CTOD(lcPrdMonth+'/01/'+lcPrdYear)))

    *B604006,1 BWA 12/11/2000 Modify the header of the monthes in the reports.[START]
    laDateTitl[I] = LEFT(laDateTitl[I],3)+'.'
    *B604006,1 [END]

    laYearTitl[I] = ALLTRIM(lcPrdYear)
    laNoOfDays[2] = IIF(MOD(YEAR(CTOD(lcPrdMonth+'/01/'+lcPrdYear)),4)=0 ,'29' ,'28' )
    ldNewDate     = (CTOD(lcPrdMonth+'/'+ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdYear)+1)
    laFltrDate[I] = CTOD(lcPrdMonth+'/01/'+lcPrdYear)
    lcPrdMonth    = ALLTRIM(STR(MONTH(ldNewDate)))
    lnPrdMonth    = INT(VAL(lcPrdMonth))
    lcPrdYear     = ALLTRIM(STR(YEAR(ldNewDate)))
  ENDFOR
ENDIF

*-- End of lfDatHdr.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from : ARSCALFG.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

*--Creat table for the trasactions month for the summary report.
*NetQ --> Net Quntity  from 1 --> 12

IF lcFormat = 'S'

  *B604006,1 BWA 12/11/2000 ADD the credit limit field to the reports.[START]
  *CREATE Table (lclines) (Salesrep C(3) , Account C(5) , Division C(6), Type C(3)     , Btname C(30)  , DvsonDisc c(30) ,;
                          Netq1 N(9,2)  , Netq2 N(9,2) , Netq3 N(9,2) , Netq4 N(9,2)  , Netq5 N(9,2)   , Netq6 N(9,2)   ,;
                          Netq7 N(9,2)  , Netq8 N(9,2) , Netq9 N(9,2) , Netq10 N(9,2) , Netq11 N(9,2)  , Netq12 N(9,2)  ,;
                          TotQnt N(15,2))

  *B604236,1 BWA 26/02/2001 1) ADD the Discount field to the reports.[START]
  *CREATE Table (lclines) (Salesrep C(3) , Account C(5) , Division C(6), Type C(3)     , Btname C(30)  , DvsonDisc c(30) ,;
                          Netq1 N(9,2)  , Netq2 N(9,2) , Netq3 N(9,2) , Netq4 N(9,2)  , Netq5 N(9,2)   , Netq6 N(9,2)   ,;
                          Netq7 N(9,2)  , Netq8 N(9,2) , Netq9 N(9,2) , Netq10 N(9,2) , Netq11 N(9,2)  , Netq12 N(9,2)  ,;
                          TotQnt N(15,2), Crlimit N(15))

  CREATE Table (lclines) (Salesrep C(3) , Account C(5) , Division C(6), Type C(3)     , Btname C(30)  , DvsonDisc c(30) ,;
                          Netq1 N(9,2)  , Netq2 N(9,2) , Netq3 N(9,2) , Netq4 N(9,2)  , Netq5 N(9,2)   , Netq6 N(9,2)   ,;
                          Netq7 N(9,2)  , Netq8 N(9,2) , Netq9 N(9,2) , Netq10 N(9,2) , Netq11 N(9,2)  , Netq12 N(9,2)  ,;
                          TotQnt N(15,2), Crlimit N(15), Disc N(5,2))

  *B604006,1 [END]
  *B604236,1 [END]

  INDEX ON SALESREP+Account+Division+Type TAG SALESREP OF (gcWorkDir+lcLines)

  *-- The Sub Total and the grand total table.
  *B604006,1 BWA 12/11/2000 ADD the credit limit field to the reports.[START]
  *CREATE Table (lcTotal) (Salesrep C(3) , Account C(5) , Division C(6), Type C(3)    ,;
                          Netq1 N(9,2)  , Netq2 N(9,2) , Netq3 N(9,2) , Netq4 N(9,2) , Netq5 N(9,2) , Netq6 N(9,2)  ,;
                          Netq7 N(9,2)  , Netq8 N(9,2) , Netq9 N(9,2) , Netq10 N(9,2), Netq11 N(9,2), Netq12 N(9,2) ,;
                          TotQnt N(15,2), DvsonDisc c(30))

  *B604236,1 BWA 26/02/2001 1) ADD the Discount field to the reports.[START]
  *CREATE Table (lcTotal) (Salesrep C(3) , Account C(5) , Division C(6), Type C(3)    ,;
                          Netq1 N(9,2)  , Netq2 N(9,2) , Netq3 N(9,2) , Netq4 N(9,2) , Netq5 N(9,2) , Netq6 N(9,2)  ,;
                          Netq7 N(9,2)  , Netq8 N(9,2) , Netq9 N(9,2) , Netq10 N(9,2), Netq11 N(9,2), Netq12 N(9,2) ,;
                          TotQnt N(15,2), Crlimit N(15), DvsonDisc c(30))

  CREATE Table (lcTotal) (Salesrep C(3) , Account C(5) , Division C(6), Type C(3)    ,;
                          Netq1 N(9,2)  , Netq2 N(9,2) , Netq3 N(9,2) , Netq4 N(9,2) , Netq5 N(9,2) , Netq6 N(9,2)  ,;
                          Netq7 N(9,2)  , Netq8 N(9,2) , Netq9 N(9,2) , Netq10 N(9,2), Netq11 N(9,2), Netq12 N(9,2) ,;
                          TotQnt N(15,2), Crlimit N(15), DvsonDisc c(30) , Disc N(5,2))

  *B604006,1 [END]
  *B604236,1 [END]

  INDEX ON SALESREP+Account+Division+Type TAG SALESREP OF (gcWorkDir+lcTotal)
ELSE
  *-- llStyle , I use it to not duplicate the header and description of the style that has many colors.
  *B604006,1 BWA 12/11/2000 ADD the credit limit field to the reports.[START]
  *CREATE Table (lclines) (Salesrep C(3) , Account C(5) , Type C(3) ,Btname C(30) , Style C(19) ,;
                          StylDisc C(20), Color C(lnColorLen) ,ClrDisc C(30), Scale C(3) , ScaleDisc C(30), SZ1 C(5) ,;
                          SZ2 C(5)      , SZ3 C(5)      , SZ4 C(5)     , SZ5 C(5)     , SZ6 C(5)     , SZ7 C(5)      ,;
                          SZ8 C(5)      , SZ9 C(5)      , SZ10 C(5)    , SZ11 C(5)    , Qty1 N(6)    , Qty2 N(6)     ,;
                          Qty3 N(6)     , Qty4 N(6)     , Qty5 N(6)    , Qty6 N(6)    , Qty7 N(6)    , Qty8 N(6)     ,;
                          Qty9 N(6)     , Qty10 N(6)    , Qty11 N(6)   , llStyle L(1) , TotQnt N(15,2))

  *B604236,1 BWA 26/02/2001 1) ADD the Discount field to the reports.[START]
  *CREATE Table (lclines) (Salesrep C(3) , Account C(5) , Type C(3) ,Btname C(30) , Style C(19) ,;
                          StylDisc C(20), Color C(lnColorLen) ,ClrDisc C(30), Scale C(3) , ScaleDisc C(30), SZ1 C(5) ,;
                          SZ2 C(5)      , SZ3 C(5)      , SZ4 C(5)     , SZ5 C(5)     , SZ6 C(5)     , SZ7 C(5)      ,;
                          SZ8 C(5)      , SZ9 C(5)      , SZ10 C(5)    , SZ11 C(5)    , Qty1 N(6)    , Qty2 N(6)     ,;
                          Qty3 N(6)     , Qty4 N(6)     , Qty5 N(6)    , Qty6 N(6)    , Qty7 N(6)    , Qty8 N(6)     ,;
                          Qty9 N(6)     , Qty10 N(6)    , Qty11 N(6)   , llStyle L(1) , Crlimit N(15), TotQnt N(15,2))

  CREATE Table (lclines) (Salesrep C(3) , Account C(5) , Type C(3) ,Btname C(30) , Style C(19) ,;
                          StylDisc C(20), Color C(lnColorLen) ,ClrDisc C(30), Scale C(3) , ScaleDisc C(30), SZ1 C(5) ,;
                          SZ2 C(5)      , SZ3 C(5)      , SZ4 C(5)     , SZ5 C(5)     , SZ6 C(5)     , SZ7 C(5)      ,;
                          SZ8 C(5)      , SZ9 C(5)      , SZ10 C(5)    , SZ11 C(5)    , Qty1 N(6)    , Qty2 N(6)     ,;
                          Qty3 N(6)     , Qty4 N(6)     , Qty5 N(6)    , Qty6 N(6)    , Qty7 N(6)    , Qty8 N(6)     ,;
                          Qty9 N(6)     , Qty10 N(6)    , Qty11 N(6)   , llStyle L(1) , Crlimit N(15), TotQnt N(15,2),;
                          Disc N(5,2)   , SubScal C(3))

  *B604006,1 [END]
  *B604236,1 [END]

  INDEX ON SALESREP+Account+LEFT(Scale,1)+STYLE+Color TAG SALESREP OF (gcWorkDir+lcLines)

  *-- The Sub Total and the grand total table.
  *B604006,1 BWA 12/11/2000 ADD the credit limit field to the reports.[START]
  *CREATE Table (lcTotal) (Salesrep C(3) , Account C(5)   , Scale C(3)      , Type C(3) , Style C(19) , Color C(lnColorLen),;
                          TotQnt N(15,2), StylDisc C(20) , ScaleDisc C(30) , SZ1 C(5)  , SZ2 C(5)    , SZ3 C(5)   ,;
                          SZ4 C(5)      , SZ5 C(5)       , SZ6 C(5)        , SZ7 C(5)  , SZ8 C(5)    , SZ9 C(5)   ,;
                          SZ10 C(5)     , SZ11 C(5)      , Qty1 N(6)       , Qty2 N(6) , Qty3 N(6)   , Qty4 N(6)  ,;
                          Qty5 N(6)     , Qty6 N(6)      , Qty7 N(6)       , Qty8 N(6) , Qty9 N(6)   , Qty10 N(6) , Qty11 N(6))

  *B604236,1 BWA 26/02/2001 1) ADD the Discount field to the reports.[START]
  *CREATE Table (lcTotal) (Salesrep C(3) , Account C(5)   , Scale C(3)      , Type C(3) , Style C(19) , Color C(lnColorLen),;
                          TotQnt N(15,2), StylDisc C(20) , ScaleDisc C(30) , SZ1 C(5)  , SZ2 C(5)    , SZ3 C(5)   ,;
                          SZ4 C(5)      , SZ5 C(5)       , SZ6 C(5)        , SZ7 C(5)  , SZ8 C(5)    , SZ9 C(5)   ,;
                          SZ10 C(5)     , SZ11 C(5)      , Qty1 N(6)       , Qty2 N(6) , Qty3 N(6)   , Qty4 N(6)  ,;
                          Qty5 N(6)     , Qty6 N(6)      , Qty7 N(6)       , Qty8 N(6) , Qty9 N(6)   , Qty10 N(6) ,;
                          Qty11 N(6)    , Crlimit N(15))

  CREATE Table (lcTotal) (Salesrep C(3) , Account C(5)   , Scale C(3)      , Type C(3) , Style C(19) , Color C(lnColorLen),;
                          TotQnt N(15,2), StylDisc C(20) , ScaleDisc C(30) , SZ1 C(5)  , SZ2 C(5)    , SZ3 C(5)   ,;
                          SZ4 C(5)      , SZ5 C(5)       , SZ6 C(5)        , SZ7 C(5)  , SZ8 C(5)    , SZ9 C(5)   ,;
                          SZ10 C(5)     , SZ11 C(5)      , Qty1 N(6)       , Qty2 N(6) , Qty3 N(6)   , Qty4 N(6)  ,;
                          Qty5 N(6)     , Qty6 N(6)      , Qty7 N(6)       , Qty8 N(6) , Qty9 N(6)   , Qty10 N(6) ,;
                          Qty11 N(6)    , Crlimit N(15)  , Disc N(5,2)     , SubScal C(3))

  *B604006,1 [END]
  *B604236,1 [END]

  INDEX ON Type TAG TYPE OF (gcWorkDir+lcTotal)
  INDEX ON SALESREP+Account+LEFT(Scale,1)+Type+Style TAG SALESREP OF (gcWorkDir+lcTotal)

  *B604236,1 [START]
  IF lcFormat = 'D' AND llSubCust = .F.
    CREATE Table (lcColect) (Salesrep C(3) , Account C(5) , Type C(3) ,Btname C(30) , Style C(19) ,;
                            StylDisc C(20), Color C(lnColorLen) ,ClrDisc C(30), Scale C(3) , ScaleDisc C(30), SZ1 C(5) ,;
                            SZ2 C(5)      , SZ3 C(5)      , SZ4 C(5)     , SZ5 C(5)     , SZ6 C(5)     , SZ7 C(5)      ,;
                            SZ8 C(5)      , SZ9 C(5)      , SZ10 C(5)    , SZ11 C(5)    , Qty1 N(6)    , Qty2 N(6)     ,;
                            Qty3 N(6)     , Qty4 N(6)     , Qty5 N(6)    , Qty6 N(6)    , Qty7 N(6)    , Qty8 N(6)     ,;
                            Qty9 N(6)     , Qty10 N(6)    , Qty11 N(6)   , llStyle L(1) , Crlimit N(15), TotQnt N(15,2),;
                            Disc N(5,2)   , SubScal C(3))

    INDEX ON SALESREP+LEFT(Scale,1)+STYLE+Color TAG lcColect OF (gcWorkDir+lcColect)
  ENDIF
  *B604236,1 [END]

ENDIF

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfRelation
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function to prepare the relations of the program.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRelation

SELECT CUSTOMER
SET RELATION TO Customer.Account INTO Invhdr ADDITIVE
SET RELATION TO Customer.Account INTO Rethdr ADDITIVE

SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE

SELECT RETHDR
SET RELATION TO Rethdr.CRMEMO INTO RETLINE ADDITIVE

SELECT INVLINE
SET RELATION TO STYLE INTO STYLE ADDITIVE

SELECT STYLE
SET RELATION TO LEFT(STYLE.SCALE,1) INTO Scalehd ADDITIVE

SELECT RETLINE
SET RELATION TO STYLE INTO (lcStyle) ADDITIVE

SELECT (lcStyle)
SET RELATION TO
SET RELATION TO LEFT(&lcStyle..SCALE,1) INTO (lcScalHd) ADDITIVE

*-- End of lfRelation.
*!*************************************************************
*! Name      : lfFillARAY
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function to fill Terms,Division,Region,Ship via,
*!           : Special Instruction,Customer classification,Seasons
*!           : Style Group, Royalty,Purchasing group.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillARAY()
*!*************************************************************
FUNCTION lfFillARAY

DIMENSION laRpSource[1,1],laRpTarget[1,1],laRpSDivi[1,1],laRpTDivi[1,1],laRpSRegn[1,1]   ,;
          laRpTRegn[1,1],laRpSShpv[1,1],laRpTShpv[1,1],laRpSSpIn[1,1],laRpTSPIn[1,1]     ,;
          laRpSCLass[1,1],laRpTCLass[1,1],laRpSsason[1,1],laRpTsason[1,1],laRpSstygr[1,1],;
          laRpTstygr[1,1],laRpSrylty[1,1],laRpTrylty[1,1],laRpSpurch[1,1],laRpTpurch[1,1]

STORE '' TO laRpSource,laRpTarget,laRpSDivi,laRpTDivi,laRpSRegn,laRpTRegn,laRpSShpv   ,;
            laRpTShpv,laRpSSpIn,laRpTSPIn,laRpSCLass,laRpTCLass,laRpSsason,laRpTsason ,;
            laRpSstygr,laRpTstygr,laRpSrylty,laRpTrylty,laRpSpurch,laRpTpurch

*--The Terms.
SELECT CODES
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CTERMCODE ' AND crltField = 'N';
                                        INTO ARRAY laRpSource
*--The Division.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CDIVISION ' AND crltField = 'N';
                                        INTO ARRAY laRpSDivi
*--The Region.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'REGION    ' AND crltField = 'N';
                                        INTO ARRAY laRpSRegn
*--The Ship Via.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'SHIPVIA   ' AND crltField = 'N';
                                        INTO ARRAY laRpSShpv
*--The Special Instruction.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'SPCINST   ' AND crltField = 'N';
                                        INTO ARRAY laRpSSpIn
*--The Customer Classification.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CLASS     ' AND crltField = 'N';
                                        INTO ARRAY laRpSCLass
*--The Seasons.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'SEASON    ' AND crltField = 'N';
                                        INTO ARRAY laRpSsason
*--The Style Group.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CSTYGROUP ' AND crltField = 'N';
                                        INTO ARRAY laRpSstygr
*--The Royalty.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'ROYALTY   ' AND crltField = 'N';
                                        INTO ARRAY laRpSrylty
*--The Purchasing group.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CPURCODE  ' AND crltField = 'N';
                                        INTO ARRAY laRpSpurch
*-- End of lfFillARAY.
*!*************************************************************
*! Name      : Header for the functions calling the mover.
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function to call mover Terms,Division,Region,Ship via,
*!           : Special Instruction,Customer classification.
*!*************************************************************
*! Calls     : 
*!      Procedures : ....
*!      Functions  : lfvTerm    , lfvDvsion , lfvRegion , lfvShpVia
*!                 : lfvSpcIns  , lfvCustom , lfvSason  , lfvStyGrp
*!                 : lfvRoyalty , lfvPurchas
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Example   : =lfvTerm(),lfvDvsion()
*!*************************************************************
*--The Terms.
FUNCTION lfvTerm
= gfMover(@laRpSource,@laRpTarget,'Terms ',.T.,'')

*--The Division.
FUNCTION lfvDvsion
= gfMover(@laRpSDivi,@laRpTDivi,'Division',.T.,'')

*--The Region.
FUNCTION lfvRegion
= gfMover(@laRpSRegn,@laRpTRegn,'Region',.T.,'')

*--The Ship Via.
FUNCTION lfvShpVia
= gfMover(@laRpSShpv,@laRpTShpv,'Ship Via',.T.,'')

*--The Special Instruction.
FUNCTION lfvSpcIns
= gfMover(@laRpSSpIn,@laRpTSPIn,'Special Instruction',.T.,'')

*--The Customer Classification.
FUNCTION lfvCustom
= gfMover(@laRpSCLass,@laRpTCLass,'Customer Classification',.T.,'')

*--The lcSeason.
FUNCTION lfvSason
= gfMover(@laRpSsason,@laRpTsason,'Seasons ',.T.,'')

*--The Style Group.
FUNCTION lfvStyGrp
= gfMover(@laRpSstygr,@laRpTstygr,'Style Group ',.T.,'')

*--The Royalty.
FUNCTION lfvRoyalty
= gfMover(@laRpSrylty,@laRpTrylty,'Royalty ',.T.,'')

*--The Purchasing group.
FUNCTION lfvPurchas
= gfMover(@laRpSpurch,@laRpTpurch,'Purchase Grouýp ',.T.,'')

*-- End of Calling Mover for all codes.
*!*************************************************************
*! Name      : lfvCratVar
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function to prepare the variables holds the collection
*!           : Criteria
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvCratVar

*--Salesrep
lnDataPos = ASCAN(laOGFxFlt,'SALESREP.REPCODE')
IF lnDataPos > 0
  LCRPEXP = STRTRAN(LCRPEXP ,'SALESREP.REPCODE','CUSTOMER.SALESREP' )
ENDIF

*--The filter
IF lcFormat = 'D'
  lnDatStyl = AT('INLIST(STYLE.CSTYMAJOR',LCRPEXP)
  lnDatColr = AT('INLIST(SUBSTR(STYLE.STYLE',LCRPEXP)
  IF lnDatStyl > 0 .OR. lnDatColr > 0
    =lfvCutSty()
  ENDIF
ENDIF

lcFilter = IIF(!EMPTY(LCRPEXP),LCRPEXP,'.T.')

           *-- Filters from customer file [START] --*
*-- The Terms.
FOR lnInd = 1 TO ALEN(laRpTarget)
  lcTerms = lcTerms + PADR(laRpTarget[lnInd],6) + ' | '
ENDFOR
lcTerms = IIF(ALLTRIM(lcTerms) = '|','',lcTerms)

IF !EMPTY(lcTerms)
  lcFilter = lcFilter + ' AND CUSTOMER.ctermcode $ lcTerms'
ENDIF
  
*--The Region.
FOR lnInd = 1 TO ALEN(laRpTRegn)
  lcRegion = lcRegion + PADR(laRpTRegn[lnInd],6) + ' | '
ENDFOR
lcRegion = IIF(ALLTRIM(lcRegion) = '|','',lcRegion)

IF !EMPTY(lcRegion)
  lcFilter = lcFilter + ' AND CUSTOMER.Region $ lcRegion'
ENDIF

*--The Ship Via.
FOR lnInd = 1 TO ALEN(laRpTShpv)
  lcShipvia = lcShipvia + PADR(laRpTShpv[lnInd],6) + ' | '
ENDFOR
lcShipvia = IIF(ALLTRIM(lcShipvia) = '|','',lcShipvia)

IF !EMPTY(lcShipvia)
  lcFilter = lcFilter + ' AND CUSTOMER.Shipvia $ lcShipvia'
ENDIF

*--The Special Instruction.
FOR lnInd = 1 TO ALEN(laRpTSPIn)
  lcSpeIns = lcSpeIns + PADR(laRpTSPIn[lnInd],6) + ' | '
ENDFOR
lcSpeIns = IIF(ALLTRIM(lcSpeIns) = '|','',lcSpeIns)

IF !EMPTY(lcSpeIns)
  lcFilter = lcFilter + ' AND CUSTOMER.Spcinst $ lcSpeIns'
ENDIF

*--The Customer Classification.
FOR lnInd = 1 TO ALEN(laRpTCLass)
  lcCusClass = lcCusClass + PADR(laRpTCLass[lnInd],6) + ' | '
ENDFOR
lcCusClass = IIF(ALLTRIM(lcCusClass) = '|','',lcCusClass)

IF !EMPTY(lcCusClass)
  lcFilter = lcFilter + ' AND CUSTOMER.Class $ lcCusClass'
ENDIF

              *-- Filters from customer file [END] --*

              *-- Filters from style file [START] --*
*--The Division.
FOR lnInd = 1 TO ALEN(laRpTDivi)
  lcDivision = lcDivision + PADR(laRpTDivi[lnInd],6) + ' | '
ENDFOR
lcDivision = IIF(ALLTRIM(lcDivision) = '|','',lcDivision)

*--The Season.
FOR lnInd = 1 TO ALEN(laRpTsason)
  lcSeason = lcSeason + PADR(laRpTsason[lnInd],6) + ' | '
ENDFOR
lcSeason = IIF(ALLTRIM(lcSeason) = '|','',lcSeason)

*--The Style Group.
FOR lnInd = 1 TO ALEN(laRpTstygr)
  lcStylGrp = lcStylGrp + PADR(laRpTstygr[lnInd],6) + ' | '
ENDFOR
lcStylGrp = IIF(ALLTRIM(lcStylGrp) = '|','',lcStylGrp)

*--The Royalty.
FOR lnInd = 1 TO ALEN(laRpTrylty)
  lcRoyalty = lcRoyalty + PADR(laRpTrylty[lnInd],6) + ' | '
ENDFOR
lcRoyalty = IIF(ALLTRIM(lcRoyalty) = '|','',lcRoyalty)

*--The Purchasing group.
FOR lnInd = 1 TO ALEN(laRpTpurch)
  lcPurchase = lcPurchase + PADR(laRpTpurch[lnInd],6) + ' | '
ENDFOR
lcPurchase = IIF(ALLTRIM(lcPurchase) = '|','',lcPurchase)

             *-- Filters from style file [END] --*

*-- End of lfvCrATVar.
*!*************************************************************
*! Name      : lfColctDat
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function to Collect the data from the invhdr and
*!           : the rethdr file.
*!*************************************************************
*! Called from : ARSCLAFG.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat
PRIVATE lcAlias,lcAccount

lcAlias = ALIAS()
SELECT CUSTOMER
lcFilter = IIF(EMPTY(lcFilter) , '.T.' , lcFilter)

SCAN WHILE Type+Account+Store ='M' FOR &lcFilter
  STORE SPACE(0) TO lcAccount , lcSalsRep , lcDivsion , lcStylDif , lcColrDif , lcScalDif , lcInvCrDf
  lcAccount = ACCOUNT

  IF lcFormat = 'S'
    *--The net quantity = Invoice quantity - Credit memo quantity
    *--Invoice quantity     --> InvLine file
    =lfNetSals()
    
    *--Credit memo quantity --> RetLine file
    =lfCrdtMem()
  ELSE
    *--The net quantity for each size = Invoice quantity - Credit memo quantity
    *--Invoice quantity     --> InvLine file
    =lfNetSize()

    *--Credit memo quantity --> RetLine file
    =lfCrdtSiz()

    *B604236,1 BWA 26/02/2001 Fix the bug some lines for the colors get befor the title.[START]
    *--Resort the Temp. file according the style.
    =lfReSort()
    *B604236,1 BWA 26/02/2001 [END]

  ENDIF
ENDSCAN

*-- In detail report use this function to change the type of the scale to "Y" in
*-- case the subtotal for the customer to get the records directly after each other
*-- to get in the FRX after the ending of the account group.
IF lcFormat = 'D'
 =lfChange()
ENDIF

*-- Append all the records for the sub total and the grand total to tha main temp. file.
SELECT (lclines)
APPEND FROM (lcTotal)

SELECT (lcAlias)
*-- End of lfcolctDat.
*!*************************************************************
*! Name      : lfNetSals
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function to Collect the data from the invhdr.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfNetSals
PRIVATE lcAlias

lcAlias = ALIAS()

*--The Ship +ve value
SELECT INVHDR
IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+INVOICE = lcAccount      ;
            FOR BETWEEN(Invdate,ldRpFMon,ldRpTMon) ;
            .AND. INVHDR.STATUS <> 'V'             ;
            .AND. SEEK(INVHDR.INVOICE,'INVLINE')   ;
            .AND. IIF(EMPTY(lcDivision), STYLE.CDIVISION = "" , STYLE.CDIVISION $ lcDivision ) ;
            .AND. IIF(EMPTY(lcSeason)  , STYLE.SEASON    = "" , STYLE.SEASON    $ lcSeason   ) ;
            .AND. IIF(EMPTY(lcStylGrp) , STYLE.CSTYGROUP = "" , STYLE.CSTYGROUP $ lcStylGrp  ) ;
            .AND. IIF(EMPTY(lcRoyalty) , STYLE.Royalty   = "" , STYLE.Royalty   $ lcRoyalty  ) ;
            .AND. IIF(EMPTY(lcPurchase), STYLE.CPURCODE  = "" , STYLE.CPURCODE  $ lcPurchase )

    SELECT INVLINE
    SCAN REST WHILE INVOICE+STR(lineno,6) = INVHDR.INVOICE
      SELECT (lclines)
        IF lcSalsRep # CUSTOMER.Salesrep .OR. lcDivsion # STYLE.CDIVISION
          IF !SEEK(CUSTOMER.Salesrep+lcAccount+STYLE.CDIVISION , lclines )
            APPEND BLANK
            *B604006,1 BWA 12/11/2000 ADD the credit limit field to the reports.[START]
            *REPLACE Btname   WITH CUSTOMER.Btname   ;
                    Salesrep  WITH CUSTOMER.Salesrep ;
                    Account   WITH lcAccount         ;
                    DIVISION  WITH STYLE.CDIVISION   ;
                    DvsonDisc WITH gfCodDes(STYLE.CDIVISION,'CDIVISION')

            *B604236,1 BWA 26/02/2001 1) ADD the Discount field to the reports.[START]
            *REPLACE Btname    WITH CUSTOMER.Btname   ;
                    Salesrep  WITH CUSTOMER.Salesrep ;
                    Account   WITH lcAccount         ;
                    DIVISION  WITH STYLE.CDIVISION   ;
                    Crlimit   WITH CUSTOMER.Crlimit  ;
                    DvsonDisc WITH gfCodDes(STYLE.CDIVISION,'CDIVISION')

            REPLACE Btname    WITH CUSTOMER.Btname   ;
                    Salesrep  WITH CUSTOMER.Salesrep ;
                    Account   WITH lcAccount         ;
                    DIVISION  WITH STYLE.CDIVISION   ;
                    Crlimit   WITH CUSTOMER.Crlimit  ;
                    Disc      WITH CUSTOMER.Disc     ;
                    DvsonDisc WITH gfCodDes(STYLE.CDIVISION,'CDIVISION')

            *B604006,1 [END]
            *B604236,1 [END]

            *-- Flag to indicate where we append new record or accumulate.
          ENDIF
        ENDIF

        =lfQuntity('A' , 'F')     && 'F' --> False

      *-- Functions collect the subtotal and the grand total.
      =lfSubTot('A')
      =lfGrndTot('A')
      lcDivsion = STYLE.CDIVISION
      lcSalsRep = CUSTOMER.Salesrep
    ENDSCAN
  ENDSCAN
ENDIF

SELECT (lcAlias)
*-- End of lfNetSals.
*!*************************************************************
*! Name      : lfCrdtMem
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function to Collect the data from the rethdr file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCrdtMem
PRIVATE lcAlias

lcAlias = ALIAS()

*--The Ship -ve value
SELECT RETHDR
IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+CRMEMO = lcAccount      ;
            FOR BETWEEN(CRDATE,ldRpFMon,ldRpTMon) ;
            .AND. RETHDR.STATUS <> 'V'            ;
            .AND. SEEK(RETHDR.CRMEMO,'RETLINE')   ;
            .AND. IIF(EMPTY(lcDivision), &lcSTYLE..CDIVISION = "" , &lcSTYLE..CDIVISION $ lcDivision ) ;
            .AND. IIF(EMPTY(lcSeason)  , &lcSTYLE..SEASON    = "" , &lcSTYLE..SEASON    $ lcSeason   ) ;
            .AND. IIF(EMPTY(lcStylGrp) , &lcSTYLE..CSTYGROUP = "" , &lcSTYLE..CSTYGROUP $ lcStylGrp  ) ;
            .AND. IIF(EMPTY(lcRoyalty) , &lcSTYLE..Royalty   = "" , &lcSTYLE..Royalty   $ lcRoyalty  ) ;
            .AND. IIF(EMPTY(lcPurchase), &lcSTYLE..CPURCODE  = "" , &lcSTYLE..CPURCODE  $ lcPurchase )

    SELECT RETLINE
    SCAN REST WHILE CRMEMO+Style+Cret_linno+Cret_trncd = RETHDR.CRMEMO 
      SELECT (lclines)
        IF lcSalsRep # CUSTOMER.Salesrep .OR. lcDivsion # &lcSTYLE..CDIVISION
          IF !SEEK(CUSTOMER.Salesrep+lcAccount+&lcSTYLE..CDIVISION , lclines )
            APPEND BLANK
            *B604006,1 BWA 12/11/2000 ADD the credit limit field to the reports.[START]
            *REPLACE Btname    WITH CUSTOMER.Btname     ;
                    Salesrep  WITH CUSTOMER.Salesrep   ;
                    Account   WITH lcAccount           ;
                    DIVISION  WITH &lcSTYLE..CDIVISION ;
                    DvsonDisc WITH gfCodDes(&lcStyle..CDIVISION,'CDIVISION')

            *B604236,1 BWA 26/02/2001 1) ADD the Discount field to the reports.[START]
            *REPLACE Btname    WITH CUSTOMER.Btname     ;
                    Salesrep  WITH CUSTOMER.Salesrep   ;
                    Account   WITH lcAccount           ;
                    DIVISION  WITH &lcSTYLE..CDIVISION ;
                    Crlimit   WITH CUSTOMER.Crlimit    ;
                    DvsonDisc WITH gfCodDes(&lcStyle..CDIVISION,'CDIVISION')

            REPLACE Btname    WITH CUSTOMER.Btname     ;
                    Salesrep  WITH CUSTOMER.Salesrep   ;
                    Account   WITH lcAccount           ;
                    DIVISION  WITH &lcSTYLE..CDIVISION ;
                    Crlimit   WITH CUSTOMER.Crlimit    ;
                    Disc      WITH CUSTOMER.Disc       ;
                    DvsonDisc WITH gfCodDes(&lcStyle..CDIVISION,'CDIVISION')

           *B604006,1 [END]
           *B604236,1 [END]

          ENDIF
        ENDIF

        =lfQuntity('B' , 'F')    && 'F' --> False

      *-- Functions collect the subtotal and the grand total.
      =lfSubTot('B')
      =lfGrndTot('B')
      lcDivsion = &lcSTYLE..CDIVISION
      lcSalsRep = CUSTOMER.Salesrep
    ENDSCAN
  ENDSCAN
ENDIF

SELECT (lcAlias)
*-- End of lfCrdtMem.
*!*************************************************************
*! Name      : lfNetSize
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function to Collect the data from the invhdr for 
*!           : the detail report.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfNetSize
PRIVATE lcAlias , lcExact

lcAlias = ALIAS()
lcExact  = SET('EXACT')

*--The Ship +ve value
SELECT INVHDR
IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+INVOICE = lcAccount      ;
            FOR BETWEEN(Invdate,ldRpFMon,ldRpTMon) ;
            .AND. INVHDR.STATUS <> 'V'             ;
            .AND. SEEK(INVHDR.INVOICE,'INVLINE')   ;
            .AND. IIF(EMPTY(lcDivision), STYLE.CDIVISION = "" , STYLE.CDIVISION $ lcDivision ) ;
            .AND. IIF(EMPTY(lcSeason)  , STYLE.SEASON    = "" , STYLE.SEASON    $ lcSeason   ) ;
            .AND. IIF(EMPTY(lcStylGrp) , STYLE.CSTYGROUP = "" , STYLE.CSTYGROUP $ lcStylGrp  ) ;
            .AND. IIF(EMPTY(lcRoyalty) , STYLE.Royalty   = "" , STYLE.Royalty   $ lcRoyalty  ) ;
            .AND. IIF(EMPTY(lcPurchase), STYLE.CPURCODE  = "" , STYLE.CPURCODE  $ lcPurchase ) ;
            .AND. IIF(EMPTY(lcStylFlt),.T.,EVAL(lcStylFlt))

    *-- Change the setting set exact to on for the comparing and return it after the comparing.
    SET EXACT ON
    lcExpr = IIF(EMPTY(lcColrFlt) , .T. , EVAL(lcColrFlt))
    IF lcExpr
      SET EXACT &lcExact

      SELECT INVLINE
      SCAN REST WHILE INVOICE+STR(lineno,6) = INVHDR.INVOICE
        SELECT (lclines)
        IF lcSalsRep # CUSTOMER.Salesrep .OR. lcStylDif #  STYLE.CSTYMAJOR .OR. lcColrDif # SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen) .OR. lcInvCrDf # INVHDR.INVOICE
          IF !SEEK(CUSTOMER.Salesrep+lcAccount+LEFT(STYLE.Scale,1)+STYLE.CSTYMAJOR+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen) , lclines )
            APPEND BLANK
            *B604006,1 BWA 12/11/2000 ADD the credit limit field to the reports.[START]
            *REPLACE Btname    WITH CUSTOMER.Btname    ;
                    Salesrep  WITH CUSTOMER.Salesrep  ;
                    Account   WITH lcAccount          ;
                    Style     WITH STYLE.Cstymajor    ;
                    StylDisc  WITH STYLE.Desc         ;
                    Scale     WITH LEFT(STYLE.Scale,1);
                    ScaleDisc WITH SCALEHD.cscaledes  ;
                    Color     WITH SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen);
                    ClrDisc   WITH gfCodDes(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'COLOR');
                    llStyle   WITH IIF(lcStylDif #  STYLE.CSTYMAJOR , .T. , .F.);
                    Type      WITH "AAA"

            *B604236,1 BWA 26/02/2001 1) ADD the Discount field to the reports.[START]
            *REPLACE Btname    WITH CUSTOMER.Btname    ;
                    Salesrep  WITH CUSTOMER.Salesrep  ;
                    Account   WITH lcAccount          ;
                    Style     WITH STYLE.Cstymajor    ;
                    StylDisc  WITH STYLE.Desc         ;
                    Scale     WITH LEFT(STYLE.Scale,1);
                    ScaleDisc WITH SCALEHD.cscaledes  ;
                    Crlimit   WITH CUSTOMER.Crlimit   ;
                    Color     WITH SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen);
                    ClrDisc   WITH gfCodDes(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'COLOR');
                    llStyle   WITH IIF(lcStylDif #  STYLE.CSTYMAJOR , .T. , .F.);
                    Type      WITH "AAA"

            REPLACE Btname    WITH CUSTOMER.Btname    ;
                    Salesrep  WITH CUSTOMER.Salesrep  ;
                    Account   WITH lcAccount          ;
                    Style     WITH STYLE.Cstymajor    ;
                    StylDisc  WITH STYLE.Desc         ;
                    Scale     WITH LEFT(STYLE.Scale,1);
                    SubScal   WITH LEFT(STYLE.Scale,1);
                    ScaleDisc WITH SCALEHD.cscaledes  ;
                    Crlimit   WITH CUSTOMER.Crlimit   ;
                    Disc      WITH CUSTOMER.Disc      ;
                    Color     WITH SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen);
                    ClrDisc   WITH gfCodDes(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'COLOR');
                    llStyle   WITH IIF(lcStylDif #  STYLE.CSTYMAJOR , .T. , .F.);
                    Type      WITH "AAA"

            *B604006,1 [END]
            *B604236,1 [END]

            IF lcStylDif #  STYLE.CSTYMAJOR .OR. lcColrDif # SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen) .OR. lcInvCrDf # INVHDR.INVOICE
              =lfStlSize('A')
            ENDIF
          ENDIF
        ENDIF

        =lfSizQnty('A')

        *-- Functions collect the subtotal for the style , scale and customer for salesrep and the grand total.
        =lfSubStyl('A')
        =lfSubScal('A')

        IF llSubCust          && in case Sub total by customer = "Yes"
          =lfSubAcnt('A')
        ENDIF
      
        =lfGrandTot('A')
        lcStylDif = STYLE.CSTYMAJOR
        lcSalsRep = CUSTOMER.Salesrep
        lcColrDif = SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)
        lcScalDif = LEFT(STYLE.Scale,1)
        lcInvCrDf = INVHDR.INVOICE
      ENDSCAN
    ENDIF
    SET EXACT &lcExact      && In case did not get inside the IF condition.
  ENDSCAN
ENDIF

SELECT (lcAlias)
*-- End of lfNetSize.
*!*************************************************************
*! Name      : lfCrdtSiz
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function to Collect the data from the rethdr file
*!           : for the detail report.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCrdtSiz
PRIVATE lcAlias , lcExact

lcAlias = ALIAS()
lcExact  = SET('EXACT')

*--The Ship -ve value
SELECT RETHDR
IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+CRMEMO = lcAccount      ;
            FOR BETWEEN(CRDATE,ldRpFMon,ldRpTMon) ;
            .AND. RETHDR.STATUS <> 'V'            ;
            .AND. SEEK(RETHDR.CRMEMO,'RETLINE')   ;
            .AND. IIF(EMPTY(lcDivision), &lcSTYLE..CDIVISION = "" , &lcSTYLE..CDIVISION $ lcDivision ) ;
            .AND. IIF(EMPTY(lcSeason)  , &lcSTYLE..SEASON    = "" , &lcSTYLE..SEASON    $ lcSeason   ) ;
            .AND. IIF(EMPTY(lcStylGrp) , &lcSTYLE..CSTYGROUP = "" , &lcSTYLE..CSTYGROUP $ lcStylGrp  ) ;
            .AND. IIF(EMPTY(lcRoyalty) , &lcSTYLE..Royalty   = "" , &lcSTYLE..Royalty   $ lcRoyalty  ) ;
            .AND. IIF(EMPTY(lcPurchase), &lcSTYLE..CPURCODE  = "" , &lcSTYLE..CPURCODE  $ lcPurchase ) ;
            .AND. IIF(EMPTY(lcStyFtRet),.T.,EVAL(lcStyFtRet))

    *-- Change the setting set exact to on for the comparing and return it after the comparing.
    SET EXACT ON
    lcExpr2 = IIF(EMPTY(lcClrFtRet) , .T. , EVAL(lcClrFtRet))
    IF lcExpr2
      SET EXACT &lcExact

      SELECT RETLINE
      SCAN REST WHILE CRMEMO+Style+Cret_linno+Cret_trncd = RETHDR.CRMEMO
        SELECT (lclines)
        IF lcSalsRep # CUSTOMER.Salesrep .OR. lcStylDif #  &lcSTYLE..CSTYMAJOR .OR. lcColrDif # SUBSTR(&lcSTYLE..STYLE,lnNonMajSt,lnColorLen) .OR. lcInvCrDf # RETHDR.CRMEMO
          IF !SEEK(CUSTOMER.Salesrep+lcAccount+LEFT(&lcSTYLE..Scale,1)+&lcSTYLE..CSTYMAJOR+SUBSTR(&lcSTYLE..STYLE,lnNonMajSt,lnColorLen) , lclines )
            APPEND BLANK
            *B604006,1 BWA 12/11/2000 ADD the credit limit field to the reports.[START]
            *REPLACE Btname    WITH CUSTOMER.Btname     ;
                    Salesrep  WITH CUSTOMER.Salesrep   ;
                    Account   WITH lcAccount           ;
                    Style     WITH &lcSTYLE..Cstymajor ;
                    StylDisc  WITH &lcSTYLE..Desc      ;
                    ScaleDisc WITH &lcScalHd..cscaledes;
                    Scale     WITH LEFT(&lcSTYLE..Scale,1);
                    Color     WITH SUBSTR(&lcSTYLE..STYLE,lnNonMajSt,lnColorLen);
                    ClrDisc   WITH gfCodDes(SUBSTR(&lcSTYLE..STYLE,lnNonMajSt,lnColorLen),'COLOR');
                    llStyle   WITH IIF(lcStylDif #  &lcSTYLE..CSTYMAJOR , .T. , .F.);
                    Type      WITH "AAA"

            *B604236,1 BWA 26/02/2001 1) ADD the Discount field to the reports.[START]
            *REPLACE Btname    WITH CUSTOMER.Btname     ;
                    Salesrep  WITH CUSTOMER.Salesrep   ;
                    Account   WITH lcAccount           ;
                    Style     WITH &lcSTYLE..Cstymajor ;
                    StylDisc  WITH &lcSTYLE..Desc      ;
                    ScaleDisc WITH &lcScalHd..cscaledes;
                    Crlimit   WITH CUSTOMER.Crlimit    ;
                    Scale     WITH LEFT(&lcSTYLE..Scale,1);
                    Color     WITH SUBSTR(&lcSTYLE..STYLE,lnNonMajSt,lnColorLen);
                    ClrDisc   WITH gfCodDes(SUBSTR(&lcSTYLE..STYLE,lnNonMajSt,lnColorLen),'COLOR');
                    llStyle   WITH IIF(lcStylDif #  &lcSTYLE..CSTYMAJOR , .T. , .F.);
                    Type      WITH "AAA"

            REPLACE Btname    WITH CUSTOMER.Btname     ;
                    Salesrep  WITH CUSTOMER.Salesrep   ;
                    Account   WITH lcAccount           ;
                    Style     WITH &lcSTYLE..Cstymajor ;
                    StylDisc  WITH &lcSTYLE..Desc      ;
                    ScaleDisc WITH &lcScalHd..cscaledes;
                    Crlimit   WITH CUSTOMER.Crlimit    ;
                    Disc      WITH CUSTOMER.Disc       ;
                    Scale     WITH LEFT(&lcSTYLE..Scale,1);
                    SubScal   WITH LEFT(&lcSTYLE..Scale,1);
                    Color     WITH SUBSTR(&lcSTYLE..STYLE,lnNonMajSt,lnColorLen);
                    ClrDisc   WITH gfCodDes(SUBSTR(&lcSTYLE..STYLE,lnNonMajSt,lnColorLen),'COLOR');
                    llStyle   WITH IIF(lcStylDif #  &lcSTYLE..CSTYMAJOR , .T. , .F.);
                    Type      WITH "AAA"

            *B604006,1 [END]
            *B604236,1 [END]

            IF lcStylDif #  &lcSTYLE..CSTYMAJOR .OR. lcColrDif # SUBSTR(&lcSTYLE..STYLE,lnNonMajSt,lnColorLen) .OR. lcInvCrDf # RETHDR.CRMEMO
              =lfStlSize('B')
            ENDIF
          ENDIF
        ENDIF

        =lfSizQnty('B')

        *-- Functions collect the subtotal for the scale and for the customers for salesrep and the grand total.
        =lfSubStyl('B')
        =lfSubScal('B')

        IF llSubCust          && in case Sub total by customer = "Yes"
          =lfSubAcnt('B')
        ENDIF

        =lfGrandTot('B')
        lcStylDif = &lcSTYLE..CSTYMAJOR
        lcSalsRep = CUSTOMER.Salesrep
        lcColrDif = SUBSTR(&lcSTYLE..STYLE,lnNonMajSt,lnColorLen)
        lcScalDif = LEFT(STYLE.Scale,1)
        lcInvCrDf = RETHDR.CRMEMO
      ENDSCAN
    ENDIF
    SET EXACT &lcExact      && In case did not get inside the IF condition.
  ENDSCAN
ENDIF

SELECT (lcAlias)
*-- End of lfCrdtSiz.

    *************** Functions for the summary report ***************

*!*************************************************************
*! Name      : lfQuntity
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Fill the temp. file with the quantity in the specified month.
*!*************************************************************
*! Calls     : 
*!    Procedures : ....
*!    Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfQuntity()
*!*************************************************************
FUNCTION lfQuntity

*-- lcType this is the first parameter when the call from lfNetSals , lfCrdtMem
*-- lcSubGrnd this is the 2nd parameter when the call from lfSubTot , lfGrndTot
PARAMETER lcType , lcSubGrnd

*--Fill the TOTAL QUANTITY field with the total QUANTITY from the INVLINE.
IF lcType = 'A'
  SELECT INVHDR
  IF lcSubGrnd = 'A'     && for the subtotal and grand total.
    REPLACE &lcTotal..TotQnt  WITH &lcTotal..TotQnt  + INVLINE.TOTQTY
  ELSE
    REPLACE &lclines..TotQnt  WITH &lclines..TotQnt  + INVLINE.TOTQTY
  ENDIF
ELSE
  SELECT RETHDR
  IF lcSubGrnd = 'B'     && for the subtotal and grand total.
    REPLACE &lcTotal..TotQnt  WITH &lcTotal..TotQnt  - RETLINE.TOTQTY
  ELSE
    REPLACE &lclines..TotQnt  WITH &lclines..TotQnt  - RETLINE.TOTQTY
  ENDIF
ENDIF

DO CASE
  *--- Period # 1
  CASE BETWEEN(IIF(lcType = 'A' ,Invdate,CRDATE),ldRpFMon,laFltrDate[2])
    =lfQunLop('1')

  *--- Period # 2
  CASE BETWEEN(IIF(lcType = 'A' ,Invdate,CRDATE),laFltrDate[2],laFltrDate[3])
    =lfQunLop('2')

  *--- Period # 3
  CASE BETWEEN(IIF(lcType = 'A' ,Invdate,CRDATE),laFltrDate[3],laFltrDate[4])
    =lfQunLop('3')

  *--- Period # 4
  CASE BETWEEN(IIF(lcType = 'A' ,Invdate,CRDATE),laFltrDate[4],laFltrDate[5])
    =lfQunLop('4')

  *--- Period # 5
  CASE BETWEEN(IIF(lcType = 'A' ,Invdate,CRDATE),laFltrDate[5],laFltrDate[6])
    =lfQunLop('5')

  *--- Period # 6
  CASE BETWEEN(IIF(lcType = 'A' ,Invdate,CRDATE),laFltrDate[6],laFltrDate[7])
    =lfQunLop('6')

  *--- Period # 7
  CASE BETWEEN(IIF(lcType = 'A' ,Invdate,CRDATE),laFltrDate[7],laFltrDate[8])
    =lfQunLop('7')

  *--- Period # 8
  CASE BETWEEN(IIF(lcType = 'A' ,Invdate,CRDATE),laFltrDate[8],laFltrDate[9])
    =lfQunLop('8')

  *--- Period # 9
  CASE BETWEEN(IIF(lcType = 'A' ,Invdate,CRDATE),laFltrDate[9],laFltrDate[10])
    =lfQunLop('9')

  *--- Period # 10
  CASE BETWEEN(IIF(lcType = 'A' ,Invdate,CRDATE),laFltrDate[10],laFltrDate[11])
    =lfQunLop('10')

  *--- Period # 11
  CASE BETWEEN(IIF(lcType = 'A' ,Invdate,CRDATE),laFltrDate[11],laFltrDate[12])
    =lfQunLop('11')

  *--- Period # 12
  CASE BETWEEN(IIF(lcType = 'A' ,Invdate,CRDATE),laFltrDate[12],laFltrDate[13])
    =lfQunLop('12')

ENDCASE

*-- End of lfQuntity.
*!*************************************************************
*! Name      : lfQunLop
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function update the quantity fields of the temp. files.
*!*************************************************************
*! Calls     : 
*!   Procedures : ....
*!   Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfQunLop()
*!*************************************************************
FUNCTION lfQunLop
PARAMETER lnCase

IF lcType = 'A'
  IF lcSubGrnd = 'A'
    REPLACE &lcTotal..NetQ&lnCase WITH &lcTotal..NetQ&lnCase + INVLINE.TOTQTY
  ELSE
    REPLACE &lclines..NetQ&lnCase WITH &lclines..NetQ&lnCase + INVLINE.TOTQTY
  ENDIF
ELSE
  IF lcSubGrnd = 'B'
    REPLACE &lcTotal..NetQ&lnCase WITH &lcTotal..NetQ&lnCase - RETLINE.TOTQTY
  ELSE
    REPLACE &lclines..NetQ&lnCase WITH &lclines..NetQ&lnCase - RETLINE.TOTQTY
  ENDIF
ENDIF

*-- End of lfQunLop.
*!*************************************************************
*! Name      : lfSubTot
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function collects the subtotal total for the summary.
*!*************************************************************
*! Calls     : 
*!   Procedures : ....
*!   Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSubTot()
*!*************************************************************
FUNCTION lfSubTot
PARAMETER lcType
PRIVATE lcAlias

*-- "WWW" For the subtotal
lcAlias = ALIAS()
SELECT (lcTotal)

IF !SEEK(CUSTOMER.Salesrep +"ZZZZZ"+IIF(lcType = 'A', STYLE.CDIVISION , &lcSTYLE..CDIVISION)+"WWW" , lcTotal )
  APPEND BLANK
  REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
          DIVISION  WITH IIF(lcType = 'A', STYLE.CDIVISION , &lcSTYLE..CDIVISION);
          Account   WITH "ZZZZZ"           ;
          DvsonDisc WITH gfCodDes(IIF(lcType = 'A', STYLE.CDIVISION , &lcSTYLE..CDIVISION),'CDIVISION');
          Type      WITH "WWW"
ENDIF

IF lcType = 'A'
  =lfQuntity('A' , 'A')         && lcType = 'A'  when come from lfNetSals.
ELSE
  =lfQuntity('B' , 'B')         && lcType = 'B'  when come from lfCrdtMem.
ENDIF

SELECT (lcTotal)
GO TOP

SELECT(lcAlias)
*-- End of lfSubTot.
*!*************************************************************
*! Name      : lfGrndTot
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function collects the grand total for the summary.
*!*************************************************************
*! Calls     : 
*!    Procedures : ....
*!    Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGrndTot()
*!*************************************************************
FUNCTION lfGrndTot
PARAMETER lcType
PRIVATE lcAlias

*-- "ZZZ" For the grand total
lcAlias = ALIAS()
SELECT (lcTotal)

IF !SEEK("ZZZ"+"ZZZZZ"+IIF(lcType = 'A', STYLE.CDIVISION , &lcSTYLE..CDIVISION)+"ZZZ" , lcTotal )
  APPEND BLANK
  REPLACE Salesrep  WITH "ZZZ"             ;
          Account   WITH "ZZZZZ"           ;
          DvsonDisc WITH gfCodDes(IIF(lcType = 'A', STYLE.CDIVISION , &lcSTYLE..CDIVISION),'CDIVISION');
          DIVISION  WITH IIF(lcType = 'A', STYLE.CDIVISION , &lcSTYLE..CDIVISION);
          Type      WITH "ZZZ"
ENDIF

IF lcType = 'A'
  =lfQuntity('A' , 'A')    && lcType = 'A'  when come from lfNetSals.
ELSE
  =lfQuntity('B' , 'B')    && lcType = 'B'  when come from lfCrdtMem.
ENDIF

SELECT (lcTotal)
GO TOP

SELECT(lcAlias)
*-- End of lfGrndTot.



    *************** Functions for the detail report ***************

*!*************************************************************
*! Name      : lfvCutSty
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Cut the style and color filter in case the detail report.
*!*************************************************************
*! Calls     : 
*!   Procedures : ....
*!   Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCutSty()
*!*************************************************************
FUNCTION lfvCutSty

lnDatStyl = AT('INLIST(STYLE.CSTYMAJOR',LCRPEXP)
IF lnDatStyl > 0
  lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnDatStyl))
  IF lnPos1 > 0
    lcStylFlt = SUBSTR(lcRpExp , lnDatStyl , lnPos1-1)
  ELSE
    lcStylFlt = SUBSTR(lcRpExp , lnDatStyl)
  ENDIF
  lcRpExp  = STRTRAN(lcRpExp , lcStylFlt ," .T. ")
ENDIF

*-- Use this variable in the functions of the retline
*-- that use the temp. file of the style not the style file in the relation
lcStyFtRet = STRTRAN(lcStylFlt , "STYLE.CSTYMAJOR" ,"EVAL(LCSTYLE+'.CSTYMAJOR')")

lnDatColr = AT('INLIST(SUBSTR(STYLE.STYLE',LCRPEXP)
IF lnDatColr > 0
  lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnDatColr))
  IF lnPos1 > 0
    lcColrFlt = SUBSTR(lcRpExp , lnDatColr , lnPos1-1)
  ELSE
    lcColrFlt = SUBSTR(lcRpExp , lnDatColr)
  ENDIF
  lcRpExp  = STRTRAN(lcRpExp , lcColrFlt ," .T. ")
ENDIF

*-- Use this variable in the functions of the retline
*-- that use the temp. file of the style not the style file in the relation
lcClrFtRet = STRTRAN(lcColrFlt , "STYLE.STYLE" ,"EVAL(LCSTYLE+'.STYLE')")

*-- End of lfvCutSty.
*!*************************************************************
*! Name      : lfSizQnty
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Fill the temp. file with the size quantity.
*!*************************************************************
*! Calls     : 
*!    Procedures : ....
*!    Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSizQnty()
*!*************************************************************
FUNCTION lfSizQnty
PARAMETER lcType
PRIVATE lcAlias

lcAlias = ALIAS()
llFRSTEXT = .F.

IF lcType = 'A'
  SELECT INVLINE
  =SEEK('S' + InvLine.Scale,'SCALE')
ELSE
  SELECT RETLINE
  lcScale = "LEFT(ALLTRIM(SUBSTR(RETLINE.STYLE,LEN(ALLTRIM(RETLINE.STYLE))-2)),1)"
  =SEEK('S' + &lcScale,'SCALE')
ENDIF

SELECT (lcLines)
*--Fill the TOTAL QUANTITY field with the total QUANTITY from the INVLINE or from the RETLINE.
IF lcType = 'A'
  REPLACE &lclines..TotQnt  WITH &lclines..TotQnt  + INVLINE.TOTQTY
ELSE
  REPLACE &lclines..TotQnt  WITH &lclines..TotQnt  - RETLINE.TOTQTY
ENDIF

FOR I = 1 TO 8
  IF llFRSTEXT
    EXIT
  ENDIF

  FOR N = 1 TO 11
    lcSzT  = 'SZ' + ALLTRIM(STR(N))        && variable for the lclines
    lcSzI  = 'SZ' + ALLTRIM(STR(I))        && variable for the scale

    lcQtyT = 'QTY' + ALLTRIM(STR(N))        && variable for the lclines
    lcQtyI = 'QTY' + ALLTRIM(STR(I))        && variable for the Invline

    IF EMPTY(SCALE.&lcSzI)
      llFRSTEXT = .T.
      EXIT
    ELSE
      IF SCALE.&lcSzI = &lcLines..&lcSzT
        IF lcType = 'A'
          REPLACE &lcLines..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI
        ELSE
          REPLACE &lcLines..&lcQtyT     WITH &lcQtyT - RETLINE.&lcQtyI
        ENDIF

        I = I + 1
        N = 0
      ENDIF
    ENDIF
  ENDFOR
ENDFOR

SELECT (lcAlias)
*-- End of lfSizQnty.
*!*************************************************************
*! Name      : lfStlSize
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function collects the all sizes from the scale file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfStlSize()
*!*************************************************************
FUNCTION lfStlSize
PARAMETER lcType
PRIVATE lcAliasSc

lcAliasSc = ALIAS()
SELECT SCALE
IF lcType = 'A'
  =SEEK('S' + LEFT(STYLE.Scale,1) , 'SCALE')
ELSE
  =SEEK('S' + LEFT(&LCSTYLE..Scale,1) , 'SCALE')
ENDIF

SCAN REST WHILE Type+Scale+Prepak = 'S' + IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1))
  llScalExt = .F.
  FOR lnScale = 1 TO 8
    IF llScalExt
      EXIT
    ENDIF

    lcSzI  = 'SZ' + ALLTRIM(STR(lnScale))        && variable for the scale.
    IF EMPTY(SCALE.&lcSzI)
      llScalExt = .T.
      EXIT
    ELSE
      SELECT(lcLines)
      FOR lnAll = 1 TO 11
        lcSzT  = 'SZ' + ALLTRIM(STR(lnAll))
        IF EMPTY(&lcLines..&lcSzT)
          REPLACE &lcLines..&lcSzT WITH SCALE.&lcSzI
          EXIT
        ENDIF
      ENDFOR
    ENDIF
  ENDFOR
ENDSCAN

SELECT (lcAliasSc)
*-- End of lfStlSize.
*!*************************************************************
*! Name      : lfSubStyl
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function collects the Subtotal for the Style.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSubStyl()
*!*************************************************************
FUNCTION lfSubStyl
PARAMETER lcType
PRIVATE lcAlias

*-- "WWW" For the subtotal for the Style.

lcAlias = ALIAS()
SELECT (lcTotal)

IF !SEEK(CUSTOMER.Salesrep +lcAccount+ IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1)) +"WWW"+IIF(lcType = 'A' , STYLE.CSTYMAJOR , &LCSTYLE..CSTYMAJOR ), lcTotal )
  APPEND BLANK
  REPLACE Salesrep WITH CUSTOMER.Salesrep  ;
          Account  WITH lcAccount          ;
          Type     WITH "WWW"              ;
          Scale    WITH IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1) );
          SubScal  WITH IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1) );
          STYLE    WITH IIF(lcType = 'A' , STYLE.CSTYMAJOR , &LCSTYLE..CSTYMAJOR ) ;
          StylDisc WITH IIF(lcType = 'A' , STYLE.Desc , &LCSTYLE..Desc ) ;
          Color    WITH REPLICATE('W',lnColorLen)
ENDIF

*-- Call function to select all the sizes for the style subtotal.
IF lcType = 'A'
  =lfTotSize('A')
ELSE
  =lfTotSize('B')
ENDIF

IF lcType = 'A'
  =lfTotQnty('A')    && lcType = 'A'  when come from lfNetSize.
ELSE
  =lfTotQnty('B')    && lcType = 'B'  when come from lfCrdtSiz.
ENDIF

SELECT (lcTotal)
GO TOP

SELECT(lcAlias)
*-- End of lfSubStyl.
*!*************************************************************
*! Name      : lfSubScal
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function collects the Subtotal for the Scale.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSubScal()
*!*************************************************************
FUNCTION lfSubScal
PARAMETER lcType
PRIVATE lcAlias

*-- "XXX" For the subtotal for the scale.
lcAlias = ALIAS()
SELECT (lcTotal)

IF !SEEK(CUSTOMER.Salesrep +lcAccount+ IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1)) +"XXX" , lcTotal )
  APPEND BLANK
  REPLACE Salesrep  WITH CUSTOMER.Salesrep  ;
          Account   WITH lcAccount          ;
          Scale     WITH IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1));
          SubScal   WITH IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1));
          ScaleDisc WITH SCALEHD.cscaledes  ;
          Type      WITH "XXX"              ;
          STYLE     WITH REPLICATE('X',19)  ;
          Color     WITH REPLICATE('X',lnColorLen)
ENDIF

*-- Call function to select all the sizes for the scale subtotal.
IF lcType = 'A'
  =lfTotSize('A')
ELSE
  =lfTotSize('B')
ENDIF

IF lcType = 'A'
  =lfTotQnty('A')    && lcType = 'A'  when come from lfNetSize.
ELSE
  =lfTotQnty('B')    && lcType = 'B'  when come from lfCrdtSiz.
ENDIF

SELECT (lcTotal)
GO TOP

SELECT(lcAlias)
*-- End of lfSubScal.
*!*************************************************************
*! Name      : lfSubAcnt
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function collects the Subtotal for the Account.
*!*************************************************************
*! Calls     : 
*!   Procedures : ....
*!   Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSubAcnt()
*!*************************************************************
FUNCTION lfSubAcnt
PARAMETER lcType
PRIVATE lcAlias

*-- "YYY" For the subtotal for the Account.
lcAlias = ALIAS()
SELECT (lcTotal)

IF !SEEK(CUSTOMER.Salesrep+lcAccount+ IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1)) +"YYY" , lcTotal )
  APPEND BLANK
  REPLACE Salesrep  WITH CUSTOMER.Salesrep  ;
          Account   WITH lcAccount          ;
          Scale     WITH IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1));
          SubScal   WITH IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1));
          STYLE     WITH REPLICATE('Y',19)  ;
          ScaleDisc WITH SCALEHD.cscaledes  ;
          Type      WITH "YYY"              ;
          Color     WITH REPLICATE('Y',lnColorLen)
ENDIF

*-- Call function to select all the sizes for the scale subtotal.
IF lcType = 'A'
  =lfTotSize('A')
ELSE
  =lfTotSize('B')
ENDIF

IF lcType = 'A'
  =lfTotQnty('A')    && lcType = 'A'  when come from lfNetSize.
ELSE
  =lfTotQnty('B')    && lcType = 'B'  when come from lfCrdtSiz.
ENDIF

SELECT (lcTotal)
GO TOP

SELECT(lcAlias)
*-- End of lfSubAcnt.
*!*************************************************************
*! Name      : lfGrandTot
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function collects the Grand total for all salesrep.
*!*************************************************************
*! Calls     : 
*!   Procedures : ....
*!   Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGrandTot()
*!*************************************************************
FUNCTION lfGrandTot
PARAMETER lcType
PRIVATE lcAlias

*-- "ZZZ" For the Grand total for all salesrep.
lcAlias = ALIAS()
SELECT (lcTotal)

IF !SEEK("ZZZ"+"ZZZZZ"+ IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1)) +"ZZZ" , lcTotal )
  APPEND BLANK
  REPLACE Salesrep  WITH "ZZZ"              ;
          Account   WITH "ZZZZZ"            ;
          Scale     WITH IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1));
          SubScal   WITH IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1));
          STYLE     WITH REPLICATE('Z',19)  ;
          ScaleDisc WITH SCALEHD.cscaledes  ;
          Type      WITH "ZZZ"              ;
          Color     WITH REPLICATE('Z',lnColorLen)
ENDIF

*-- Call function to select all the sizes for the scale subtotal.
IF lcType = 'A'
  =lfTotSize('A')
ELSE
  =lfTotSize('B')
ENDIF

IF lcType = 'A'
  =lfTotQnty('A')    && lcType = 'A'  when come from lfNetSize.
ELSE
  =lfTotQnty('B')    && lcType = 'B'  when come from lfCrdtSiz.
ENDIF

SELECT (lcTotal)
GO TOP

SELECT(lcAlias)
*-- End of lfGrandTot.
*!*************************************************************
*! Name      : lfTotSize
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function collects the all sizes from the scale file.
*!*************************************************************
*! Calls     : 
*!   Procedures : ....
*!   Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfTotSize()
*!*************************************************************
FUNCTION lfTotSize
PARAMETER lcType
PRIVATE lcAliasSc

lcAliasSc = ALIAS()
SELECT SCALE
IF lcType = 'A'
  =SEEK('S' + LEFT(STYLE.Scale,1) , 'SCALE')
ELSE
  =SEEK('S' + LEFT(&LCSTYLE..Scale,1) , 'SCALE')
ENDIF

SCAN REST WHILE Type+scale+prepak = 'S' + IIF(lcType = 'A' , LEFT(STYLE.Scale,1) , LEFT(&LCSTYLE..Scale,1))
  llScalExt = .F.
  FOR lnScale = 1 TO 8
    IF llScalExt
      EXIT
    ENDIF

    lcSzI  = 'SZ' + ALLTRIM(STR(lnScale))        && variable for the scale.
    IF EMPTY(SCALE.&lcSzI)
      llScalExt = .T.
      EXIT
    ELSE
      SELECT (lcTotal)
      FOR lnAll = 1 TO 11
        lcSzT  = 'SZ' + ALLTRIM(STR(lnAll))
        IF EMPTY(&lcTotal..&lcSzT)
          REPLACE &lcTotal..&lcSzT      WITH SCALE.&lcSzI
          EXIT
        ENDIF
        IF &lcTotal..&lcSzT = SCALE.&lcSzI
          EXIT
        ENDIF
      ENDFOR
    ENDIF
  ENDFOR
ENDSCAN

SELECT (lcAliasSc)
*-- End of lfTotSize.
*!*************************************************************
*! Name      : lfTotQnty
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function collects the Subtotal for the Scale.
*!*************************************************************
*! Calls     : 
*!   Procedures : ....
*!   Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfTotQnty()
*!*************************************************************
FUNCTION lfTotQnty
PARAMETER lcType
PRIVATE lcAlias

lcAlias = ALIAS()
llFRSTEXT = .F.

IF lcType = 'A'
  SELECT INVLINE
  =SEEK('S' + InvLine.Scale,'SCALE')
ELSE
  SELECT RETLINE
  lcScale = "LEFT(ALLTRIM(SUBSTR(RETLINE.STYLE,LEN(ALLTRIM(RETLINE.STYLE))-2)),1)"
  =SEEK('S' + &lcScale,'SCALE')
ENDIF

SELECT (lcTotal)
*--Fill the TOTAL QUANTITY field with the total QUANTITY from the INVLINE or RETLINE.
IF lcType = 'A'
  REPLACE &lcTotal..TotQnt  WITH &lcTotal..TotQnt  + INVLINE.TOTQTY
ELSE
  REPLACE &lcTotal..TotQnt  WITH &lcTotal..TotQnt  - RETLINE.TOTQTY
ENDIF

FOR I = 1 TO 8
  IF llFRSTEXT
    EXIT
  ENDIF

  FOR N = 1 TO 11
    lcSzT  = 'SZ' + ALLTRIM(STR(N))        && variable for the lcTotal
    lcSzI  = 'SZ' + ALLTRIM(STR(I))        && variable for the scale

    lcQtyT = 'QTY' + ALLTRIM(STR(N))        && variable for the lcTotal
    lcQtyI = 'QTY' + ALLTRIM(STR(I))        && variable for the Invline

    IF EMPTY(SCALE.&lcSzI)
      llFRSTEXT = .T.
      EXIT
    ELSE
      IF SCALE.&lcSzI = &lcTotal..&lcSzT
        IF lcType = 'A'
          REPLACE &lcTotal..&lcQtyT     WITH &lcQtyT + InvLine.&lcQtyI
        ELSE
          REPLACE &lcTotal..&lcQtyT     WITH &lcQtyT - RETLINE.&lcQtyI
        ENDIF

        I = I + 1
        N = 0
      ENDIF
    ENDIF
  ENDFOR
ENDFOR

SELECT(lcAlias)
*-- End of lfTotQnty.
*!*************************************************************
*! Name      : lfChange
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function to the type of the subtotal account.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfChange()
*!*************************************************************
FUNCTION lfChange
PRIVATE lcAlias , lcOrder

lcAlias = ALIAS()
SELECT (lcTotal)
lcOrder  = ORDER()

SET ORDER TO TAG TYPE
SCAN
  IF &lcTotal..TYPE = 'YYY'
    REPLACE Scale WITH 'Y'
  ENDIF
ENDSCAN

SET ORDER TO &lcOrder
SELECT (lcAlias)
*-- End of lfChange.
*B604236,1 [START]
*!*************************************************************
*! Name      : lfSortStyl
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 05/07/2001
*! Purpose   : Function to the collect the style color.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfSortStyl()
*!*************************************************************
FUNCTION lfSortStyl

*DN >> Detail customer no.
PRIVATE lcSalsRpDN , lcYYYDn , lcZZZDn

lcYYYDn = REPLICATE('Y',lnColorLen)
lcZZZDn = REPLICATE('Z',lnColorLen)

SELECT (lclines)
SCAN 
  SCATTER MEMVAR MEMO
  IF !SEEK(M.SALESREP+LEFT(M.Scale,1)+M.STYLE+M.Color,lcColect) OR (TYPE = "YYY" AND STYLE = 'YYYYYYYYYYYYYYYYYYY' AND Color = lcYYYDn) ;
                                                                OR (TYPE = "ZZZ" AND STYLE = 'ZZZZZZZZZZZZZZZZZZZ' AND Color = lcZZZDn)

    SELECT (lcColect)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ELSE
    SELECT (lcColect)
    REPLACE QTY1   WITH QTY1   + M.QTY1    ;
            QTY2   WITH QTY2   + M.QTY2    ;
            QTY3   WITH QTY3   + M.QTY3    ;
            QTY4   WITH QTY4   + M.QTY4    ;
            QTY5   WITH QTY5   + M.QTY5    ;
            QTY6   WITH QTY6   + M.QTY6    ;
            QTY7   WITH QTY7   + M.QTY7    ;
            QTY8   WITH QTY8   + M.QTY8    ;
            QTY9   WITH QTY9   + M.QTY9    ;
            QTY10  WITH QTY10  + M.QTY10   ;
            QTY11  WITH QTY11  + M.QTY11   ;
            TotQnt WITH TotQnt + M.TotQnt

     llReplac = .T.
  ENDIF
ENDSCAN

IF llReplac
  SELECT (lclines)
  DELETE ALL
  APPEND FROM (lcColect)
ENDIF
*-- End of lfSortStyl.
*B604236,1 [END]
    *************** End Functions for the detail report ***************



    *************** Validations Function for the report ***************
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Showes date range screen.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng

lcTitle = 'Transaction Month'

DO (gcRepHome + gcAct_Appl + '\Monthrng.SPR')

*-- End of lfvDateRng.
*!*************************************************************
*! Name      : lfvChekF
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Validate date from range screen's to get it 01 
*!           : the first day in the month .
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvChekF()
*!*************************************************************
FUNCTION lfvChekF
PRIVATE lcMonth,lcYear

lcMonth = ALLTRIM(STR(MONTH(ldRpFMon)))
lcYear  = ALLTRIM(STR(YEAR(ldRpFMon)))

IF DAY(ldRpFMon) # 01
  IF SET('DATE') = "BRITISH"
    ldRpFMon = CTOD('01'+'/'+lcMonth+'/'+lcYear)
  ELSE
    ldRpFMon = CTOD(lcMonth+'/01/'+lcYear)
  ENDIF
ENDIF
llChkdat = .F.    && Flag used to check if the user get inside the transaction month option or not.[In the syrepuvr.dba]

*-- End of lfvChekF.
*!*************************************************************
*! Name      : lfvChekTo
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Validate date from range screen's to get it 01 
*!           : the first day in the month .
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvChekTo()
*!*************************************************************
FUNCTION lfvChekTo
PRIVATE lcMonth,lcYear

lcMonth = ALLTRIM(STR(MONTH(ldRpTMon)))
lcYear  = ALLTRIM(STR(YEAR(ldRpTMon)))

IF DAY(ldRpTMon) # 01
  IF SET('DATE') = "BRITISH"
    ldRpTMon = CTOD('01'+'/'+lcMonth+'/'+lcYear)
  ELSE
    ldRpTMon = CTOD(lcMonth+'/01/'+lcYear)
  ENDIF
ENDIF

*-- End of lfvChekTo.
*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Validate Transaction Month screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPbOk()
*!*************************************************************
FUNCTION lfvPbOk

IF ldRpFMon > ldRpTMon
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldRpFMon)
ELSE
  CLEAR READ
ENDIF

*-- End of lfvPbOk.
*!*************************************************************
*! Name      : lfsrRep
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Sales rep. in range browse screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrRep()
*!*************************************************************
*! Note      : S symbol is [S,Set] ,R symbol is [R ,Reset]
*!*************************************************************
FUNCTION lfsrRep
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    GO TOP IN SALESREP
ENDCASE

*-- End of lfsrRep.
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

lnMajSeg   = gfItemMask('SM')  && No. of major segments.
lcMajPict  = gfItemMask("PM")

*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style code Structure. [End]

RETURN ''
*-- End of lfEvalSegs.
*!*************************************************************
*! Name      : lfAccount
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Account in range browse screen .
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAccount ()
*!*************************************************************
*! Note      : S symbol is [S,Set] ,R symbol is [R ,Reset]
*!*************************************************************
FUNCTION lfAccount
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
ENDCASE
*-- End of lfAccount.
*!*************************************************************
*! Name      : lfClarAll
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/12/2000
*! Purpose   : Function clear all the temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfClarAll()
*!*************************************************************
FUNCTION lfClarAll

IF USED (lcLines)
  SELECT (lcLines)
  USE
  ERASE (gcWorkDir + lcLines + '.DBF')
  CLOSE INDEX
  ERASE (gcWorkDir + lcLines + '.CDX')
ENDIF

IF USED (lcTotal)
  SELECT (lcTotal)
  USE
  ERASE (gcWorkDir + lcTotal + '.DBF')
  CLOSE INDEX
  ERASE (gcWorkDir + lcTotal + '.CDX')
ENDIF

*-- End of lfClarAll.
*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Refresh the filters in the option grid and select
*!           : the FRX due to the report format.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfClrRead ()
*!*************************************************************
FUNCTION lfClrRead

*-- Summery|Detail~S|D
IF lcFormat = 'S'
  lcRpForm = 'ARSCALFG'
ELSE  
  lcRpForm = 'ARSCALFD'
ENDIF

*-- Refresh the filters in the option grid.
CLEAR READ

*-- End of lfClrRead.
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/12/2000
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE

*-- End of lfsrvSty.
*B604236,1 BWA 26/02/2001 Fix the bug some lines for the colors get befor the title.[START]
*!*************************************************************
*! Name      : lfReSort
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 26/02/2001
*! Purpose   : Function to resort the temp. file by the style.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfReSort

lcAlias = ALIAS()
STORE SPACE(0) TO lcRsrtSty
SELECT (lclines)
LOCATE

SCAN
  IF lcRsrtSty # Salesrep+Account+LEFT(STYLE.Scale,1)+Style
    REPLACE llStyle WITH .T.
  ELSE
    REPLACE llStyle WITH .F.
  ENDIF
  lcRsrtSty = Salesrep+Account+LEFT(STYLE.Scale,1)+Style
ENDSCAN
LOCATE
SELECT (lcAlias)

*-- End of lfReSort.
*B604236,1 BWA 26/02/2001 [END]
    *************** End Validations Function for the report ***************