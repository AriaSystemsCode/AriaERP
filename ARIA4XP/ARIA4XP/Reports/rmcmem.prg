*:***************************************************************************
*: Program file  : RmCMem
*: Program desc. : Credit Memo
*: For Report    : RmCMemA.FRX
*: System        : Aria Advantage Series.
*: Module        : Return Merchandise (RM)
*: Developer     : Ayman Mahmoud (AYM N040117)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe,
*:               : gfRltFld, gfCodDes,gfGetMemVar,gfOptMsg,
*:               : lfGetLogo,lfAdrShift,lfSolSpAdr,lfHeadVar,lfSaveFlag,
*:               : lfEndGroup,lfwRepWhen,lfFormName,
*:               : lfvOptMsg,lfwOldVal.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : All [[IF llFrTime]] Blocks executed one time in the option grid seasson.
*:         : and if user selection opens any temp. files that may be used later
*:         : in another selection I take this file open to use untill user choice
*:         : is to press < Close > button, to save more time. 
*:***************************************************************************
*: Example : DO RmCMem
*:***************************************************************************
*: This Program is due to E300846 ...
*:***************************************************************************
*: Modifications :
*: B607936,1 MMT 01/17/07 fix bug of worng phone format T20061214.0015
*: B608395,1 MMT 12/30/2007 Fix bug of wrong amount Values while printing CM [T20071019.0005]
*: B608555,1 MMT 05/15/2008 Fix bug of credit memo screen print button is not working[T20080511.0001]
*: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[T20090504.0014]
*: B608985,1 MMT 08/26/2009 Fix bug of Error if company is multui Currency{T20090818.0012}
*: B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[T20090831.0015]
*! B609376,1 MMT 08/09/2010 Credit memo print does not calculate correctly with merchandise discount[T20100714.0006]
*! B610059,1 MMT 08/27/2012 Exporting Credit memo to PDF does not update print flag in RETHDR[T20120823.0002]
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [T20120711.0001]
*! E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE []
*! B611018,1 SAR 06/18/2015 Request builder items missing T20150422.0007
*:***************************************************************************

*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientID

*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE [Start]
IF TYPE('lcRequestID') = 'C' .AND. 'TEMP.TXT' $ UPPER(lcRequestID)
  STRTOFILE("2.0.0.1", lcRequestID, .F.)
  RETURN
ENDIF
*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE [End]

IF TYPE('lcXMLFileName') = 'C'
  PUBLIC gcRequestID, gcClientID
  gcRequestID = lcRequestID
  gcClientID = ClientId  
  
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  
  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  
  LOCAL loEnvironment
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientID = ClientID
  	
  LOCAL lcCurrentProcedure
  lcCurrentProcedure =    loEnvironment.Aria40SharedPath
  loEnvironment.ConnectionsRefresh()
  
  LOCAL lcRequestCompany, lcClientRoot, lcEnvOutput
  lcRequestCompany = loAgent.GetRequestCompany(lcRequestID, ClientId)
  lcClientRoot = loEnvironment.Aria40SharedPath 
   
  lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH lcRequestCompany , ClientId, lcCurrentProcedure, loEnvironment
  
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  
  PUBLIC gcAct_Appl
  gcAct_Appl  = 'RM'  
  oAriaEnvironment.REPORT.gcAct_Appl = 'RM'
  oAriaEnvironment.ActiveModuleID = 'RM'
  oAriaEnvironment.RequestID = lcRequestID
  
  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF 
  
  oAriaEnvironment.Report.cCROrientation = 'P'
  
  llOgFltCh = .T.  
  lfwrepwhen()
ELSE
  loogscroll.cCROrientation = 'P'
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]

PRIVATE lcSqlTempStatment,lcSqlStatment,lcSelectedFields,lcSelectedFields1,lcSqlTempStatment,;
        lcSqlTempStatment1,lcWhereconditoin,lcOrderConditoin
STORE .F. TO     llCreditWare ,llCreditAccount, llCreditNo
STORE '' TO   lcTempWareHous ,lcTempAccount ,lcTempCreditNo,lcTempCreditAcc,lcTempCreditWare
lcTime = TIME()          && Variable to hold the Time
llTitle    = .T.         && Flag to print Detail header.
llEndGroup = .T.         && Flag to detect end of the Group.
lcDivLName = ''
lcTime1 = SECONDS()
lcTaxRefr = gfGetMemVar('M_TAX_REFE',oAriaApplication.ActiveCompanyID)
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
*llTextMode = (UPPER(ALLTRIM(lcRepMode))== 'TEXT')  && Print Text Format
IF TYPE('lcXMLFileName') = 'C'
  llTextMode = .F.
ELSE
  llTextMode = (UPPER(ALLTRIM(lcRepMode))== 'TEXT')  && Print Text Format
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
lluse_Confg = (ALLTRIM(gfGetMemVar("M_STYCNFG")) = "Y")
lcTax_Rate = ''          && Hole the percentage rate of tax
lcVatNo    = ''          && Hold the VAT number
lcCompFax  = ''          && Hold the company Fax
*  Check if we are going to display tocca Custome format
*  If So raise a flag tell that also
*  declare variables to hold Vendor# and DUNS# 
IF TYPE('lcTocca')='C' AND ALLTRIM(lcTocca) ='Y'
  llTocca = .T.
  STORE '' TO lcVendCD , lcDunsNo , lcDept
ELSE
  llTocca = .F.
ENDIF
*! B610059,1 MMT 08/27/2012 Exporting Credit memo to PDF does not update print flag in RETHDR[Start]
llPrinter = .F.
*! B610059,1 MMT 08/27/2012 Exporting Credit memo to PDF does not update print flag in RETHDR[End]
*--Section to get the style and color length --*
*--THE COLOR LENGTH
STORE 0 TO lnClrLnR1 , lnClrPosR1
DECLARE laItemSeg[1]
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
*=gfItemMask(@laItemSeg)
IF TYPE('lcXMLFileName') = 'C'
  ItemMask = CREATEOBJECT("GetItemMask")
  =ItemMask.Do(@laItemSeg)
ELSE
  =gfItemMask(@laItemSeg)
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnR1  = LEN(laItemSeg[lnCount,3])
    lnClrPosR1 = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*--THE STYLE LENGTH
STORE 0 TO lnLenthR1
lnLenthR1 = LEN(gfItemMask('PM'))

*--Initial variable to check if print the report from the main or from the optional prorgram.
llRmMemo = .T.            
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
*WAIT WINDOW "Collecting data for report..." NOWAIT
IF TYPE('lcXMLFileName') <> 'C'
  WAIT WINDOW "Collecting data for report..." NOWAIT
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]

*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
*IF loOGScroll.llOGFltCh 
IF llOGFltCh 
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
  *-- if it's first time you run option Grid, i.e: you have unknown variables.
  * Add the condition to check if we print the f&g form or not, that prevent the data 
  *         collection in the main program and replace it with the Optional Progaram [start]
  IF TYPE('lcColec') <> 'C'
      IF llFrTime 
      llTax      = gfGetMemVar('M_TAX') = 'Y'  && This company use taxes.
      IF llTax
        lcTaxDsc = gfGetMemVar('M_TAX_DESC')
        lcTaxDsc = IIF(EMPTY(lcTaxDsc),'GST',lcTaxDsc )
      ENDIF
      lcStyTitle = gfItemMask('HI')            && Title of the style.
      lnMajorLen = LEN(gfItemMask("PM"))       && Style major length.
      lcObj_Id   = ''                           && Object Identification in Objlink file. 
      *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
      IF TYPE('lcXMLFileName') = 'C'
        =gfOpenFile('OBJECTS','OBJECTID')
        =gfOpenFile('OBJLINK','OBJLNKTY')
      ENDIF      
      *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
      *-- if this company have a logo, put it in temp. file and then use it in .FRX
         IF SEEK('*' + 'LOGO' , 'OBJLINK') AND ;
           SEEK(OBJLINK.cObject_ID,'OBJECTS')
           = lfGetLogo()  && Function to Fill the temp. With company Logo.
         ENDIF
      *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
      *lcRetLine = loOgScroll.gfTempName()      && Name of temp. File have lines data.    
      IF TYPE('lcXMLFileName') = 'C'        
        lcRetLine = gfTempName()      && Name of temp. File have lines data.          
      ELSE
        lcRetLine = loOgScroll.gfTempName()      && Name of temp. File have lines data.    
      ENDIF
      *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
      *-- Add Two fields to file structure.
      DIMENSION laFileStru[1,18]
      laFileStru = ''
      *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
      IF TYPE('lcXMLFileName') = 'C'
        =gfOpenTable('RETHDR','')
        =gfOpenTable('RETLINE','')
      ENDIF
      *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
      SELECT RETLINE
      =AFIELDS(laFileStru)
      lnFileStru = ALEN(laFileStru,1) 

      *: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[Start]
      *DIMENSION laFileStru[lnFileStru+3, 18]
      DIMENSION laFileStru[lnFileStru+4, 18]
      *: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[End]
      laFileStru[lnFileStru+1,1] = 'lEndLine'
      laFileStru[lnFileStru+1,2] = 'L'
      laFileStru[lnFileStru+1,3] = 1
      laFileStru[lnFileStru+1,4] = 0
      laFileStru[lnFileStru+2,1] = 'mNotes'
      laFileStru[lnFileStru+2,2] = 'M'
      laFileStru[lnFileStru+2,3] = 1
      laFileStru[lnFileStru+2,4] = 0
      laFileStru[lnFileStru+3,1] = 'cIndx'
      laFileStru[lnFileStru+3,2] = 'C'
      laFileStru[lnFileStru+3,3] = 1
      laFileStru[lnFileStru+3,4] = 0
      *: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[Start]
      laFileStru[lnFileStru+4,1] = 'cDivLngNam'
      laFileStru[lnFileStru+4,2] = 'C'
      laFileStru[lnFileStru+4,3] = 30
      laFileStru[lnFileStru+4,4] = 0
      *: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[End]
      
      FOR  lnInc=7 TO 16 
        *: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[Start]
        *STORE SPACE(1) TO laFileStru[lnFileStru+1,lnInc],laFileStru[lnFileStru+2,lnInc],laFileStru[lnFileStru+3,lnInc]
        STORE SPACE(1) TO laFileStru[lnFileStru+1,lnInc],laFileStru[lnFileStru+2,lnInc],laFileStru[lnFileStru+3,lnInc],laFileStru[lnFileStru+4,lnInc]
        *: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[End]
      ENDFOR 
      STORE SPACE(0) TO laFileStru[lnFileStru+1,17],laFileStru[lnFileStru+1,18] 
      STORE SPACE(0) TO laFileStru[lnFileStru+2,17],laFileStru[lnFileStru+2,18]  
      STORE SPACE(0) TO laFileStru[lnFileStru+3,17],laFileStru[lnFileStru+3,18]
      *: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[Start]
      STORE SPACE(0) TO laFileStru[lnFileStru+4,17],laFileStru[lnFileStru+4,18]
      *: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[End]

      gfCrtTmp(lcRetLine,@laFileStru,"CRMEMO",lcRetLine,.F.)
    ENDIF  && End if llFrTime block
    *--1.Create Cursor from RetHdr,RetLine Files after Appling all filters on this file.[Begin]
    *--Create index for files used.     [BEGIN]    
    lfCreateindex('RETHDR','CRMEMO')
    lfCreateindex(lcRetLine,'CRMEMO+cIndx+STYLE+CRET_LINNO+CRET_TRNCD')
*!*	    lfCreateindex(lcRetLine,'CRMEMO+CRET_LINNO+STYLE+CRET_TRNCD')

    *-Create index for files used.      [END]
    *--Merge Collecting data with the retrieved fields form SQL in order to improve the performance
    *  ,we will retrived only the required fields and only according to the selected options [Begin]
    STORE .F. TO llWareHous,llAccount,llCreditNo,llSqlErro,llSqlErro1,llSqlErro2,llSqlErro3
    STORE '' TO lcTempWareHous,lcTempAccount
    lcSelectedFields   = "crMemo,crDate,RetHdr.cWareCode,Reference,RetHdr.Account,Store,SalesRep1,SalesRep2,;
                        CustPo,Cartons,Cretnote1,CretNote2,Amount,Other,Tax_Amt,nHstAmt,;
                        RaNo,[Order],Invoice,cTermCode,cDivision,Flag,nPstAmt"
    lcSelectedFields   = lcSelectedFields   + ",RETHDR.CCURRCODE,RETHDR.STATUS,RETHDR.VDATE,RETHDR.Pieces "
*! B609376,1 MMT 08/09/2010 Credit memo print does not calculate correctly with merchandise discount[Start]
*!*	    lcSelectedFields1  = "crMemo,Style,Cret_LinNo,cRet_trncd,Invoice,Reason,Qty1,Qty2,;
*!*	                        Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Price,Dyelot"
    lcSelectedFields1  = "crMemo,Style,Cret_LinNo,cRet_trncd,Invoice,Reason,Qty1,Qty2,;
                        Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Price,Dyelot,AMOUNT"
*! B609376,1 MMT 08/09/2010 Credit memo print does not calculate correctly with merchandise discount[End]
    
    lcWhereConditoin   = lfCreateWhereCondition()
    lcSqlTempStatment1 = "SELECT "+lcSelectedFields1+" FROM RETLINE(index = RETLINE)  "      
    lcInnerJoincondition = lfGetJoins()
    lcSqlTempStatment1 = lcSqlTempStatment1 + " WHERE RETLINE.CRMEMO = '"

    *--Empty Files
    =lfEmptyFiles()
    *--collect data    
    *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
    IF TYPE('lcXMLFileName') = 'C'
      *--Open all required files Remotly using remote table.
      loRETHDR   = CREATEOBJECT('RemoteTable','RETHDR',,'RETHDR',SET("Datasession"),,.T.) 
      loRETLINE  = CREATEOBJECT('RemoteTable','RETLINE',,'RETLINE',SET("Datasession"),,.T.) 
      loRETHDR_X = CREATEOBJECT('RemoteTable','RETHDR','RETHDR','RETHDR_X',SET("Datasession"),,.T.) 
      IF !USED('NOTEPAD')
        =gfOpenFile('NOTEPAD', 'NOTEPAD', 'SH', 'NOTEPAD')
      ENDIF
    ENDIF
    *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]    
    =lfCollectData()
    IF RECCOUNT('RETHDR') = 0
       *-- Message : There are no records to display...!
       *--                < Ok >
      *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
      *=gfModalGen('TRM00052B40011','ALERT')
      IF TYPE('lcXMLFileName') <> 'C'
        =gfModalGen('TRM00052B40011','ALERT')
      ENDIF
      *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
      
      RETURN
    ENDIF  
    IF RECCOUNT(lcRetLine) = 0
       *-- Message : There are no records to display...!
       *--                < Ok >
      *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]      
      *=gfModalGen('TRM00052B40011','ALERT')
      IF TYPE('lcXMLFileName') <> 'C'
        =gfModalGen('TRM00052B40011','ALERT')
      ENDIF
      *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
      RETURN
    ENDIF 
    IF llFrTime
      llFrTime = .F.  && After this time all of your variables have been defined, you not need to goto any llFrTime block again.
      DECLARE laCompAdd[6,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2], laMemoData[3,2]
      laCompAdd  = ''            && Array to hold the Company address
      laSoldTo   = ''            && Array to hold the Sold To address
      laShipTo   = ''            && Array to hold the Ship To address
      laMemoData = ''            && Array to hold (R/A # , Order #, Invoice #) 
      laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
      laDivLName[1,2] = 'lcDivLName'
      *-- Get company Address [begin].
      lcWorkArea = SELECT()
      PRIVATE  lcSqlCommand , lnResult
      lcSqlCommand=[SELECT cCom_Name,cCom_Phon,cCom_fax,cCont_code,cAddress1,cAddress2,cAddress3,cAddress4,cAddress5,cAddress6 FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
      lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
      IF lnResult >= 1 
        lcCompName = cCom_Name
        lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
        lcCompFax  = cCom_fax              && Variable to hold the Company Fax
        lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
        *-- Load Company address.
        laCompAdd[1] = gfGetAdr('SYCCOMP', '' , '' , '' , 1)
        laCompAdd[2] = gfGetAdr('SYCCOMP', '' , '' , '' , 2)
        laCompAdd[3] = gfGetAdr('SYCCOMP', '' , '' , '' , 3)
        laCompAdd[4] = gfGetAdr('SYCCOMP', '' , '' , '' , 4)
        laCompAdd[5] = gfGetAdr('SYCCOMP', '' , '' , '' , 5)
        *B607936,1 MMT 01/17/07 fix bug of worng phone format[Start]
        *laCompAdd[6] = "Phone #:" + TRANSFORM(lcCompPhon , lcPhonPict) 
        laCompAdd[6] = "Phone #:" + TRANSFORM(lcCompPhon , '@R '+lcPhonPict) 
        *B607936,1 MMT 01/17/07 fix bug of worng phone format[END]
*!*	        laCompAdd[7] = IIF(UPPER(ALLTRIM(gcContCode))='ENG',"Vat Reg No: " + lcTaxRefr,'')
        = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
      ENDIF 
      SELECT (lcWorkArea) 
      lcDunsNo  = gfGetMemVar('XDUNS')
    ENDIF  && End if llFrTime block
  ENDIF 
ENDIF &&end of filter change

*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
IF TYPE('lcXMLFileName') = 'C'
  IF !USED('STYLE')
    =gfOpenFile('STYLE', 'STYLE', 'SH', 'STYLE')
  ENDIF
  IF !USED('WAREHOUS')
    =gfOpenFile('WAREHOUS', 'WAREHOUS', 'SH', 'WAREHOUS')
  ENDIF
  IF !USED('SCALE')
    =gfOpenFile('SCALE', 'SCALE', 'SH', 'SCALE')
  ENDIF
  IF !USED('CUSTOMER')
    =gfOpenFile('CUSTOMER', 'CUSTOMER', 'SH', 'CUSTOMER')
  ENDIF
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]

SELECT (lcRetLine)
SET RELATION TO 
SET RELATION TO Style INTO Style ADDITIVE

SELECT RetHdr
SET RELATION TO 
SET RELATION TO cwarecode INTO Warehous ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,'S' + Account + Store) INTO CUSTOMER ADDITIVE

SET RELATION TO CRMemo INTO (lcRetLine) ADDITIVE
SELECT STYLE
SET RELATION TO 
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
lcSkipExpr = [(lcRetLine)]  && Expression we skip to later
SELECT RETHDR

*: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[Start]
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
*lcTmpDivFl = loOgScroll.gfTempName()
IF TYPE('lcXMLFileName') = 'C'
  lcTmpDivFl = gfTempName()
ELSE
  lcTmpDivFl = loOgScroll.gfTempName()
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
=lfDivLngNam()
SELECT RETHDR
*: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[End]

SET SKIP TO &lcSkipExpr
llMulCurr = gfGetMemVar('llMulCurr',oAriaApplication.ActiveCompanyID)
lcCurrPost = "LEFT "                              && Default Value.
IF llMulCurr
  *: B608985,1 MMT 08/26/2009 Fix bug of Error if company is multui Currency{Start}
  *lcSqlCommand="SELECT SycInt.cCurrency FROM SycInt WHERE SycInt.CCONTCODE  = '"+SycComp.cCont_Code+"'"
  lcSqlCommand="SELECT SycInt.cCurrency FROM SycInt WHERE SycInt.CCONT_CODE = '"+SycComp.cCont_Code+"'"
  *: B608985,1 MMT 08/26/2009 Fix bug of Error if company is multui Currency{End}
  lnResultSelect  = oAriaApplication.RemoteSystemData.Execute(lcSqlCommand,"",lcSycIntInfo ,"",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  IF lnResultSelect = 1
    lcCurrPost = &lcSycIntInfo..cCurrency
  ENDIF   
ENDIF
lcSqlCommand="SELECT SYCCURR.cCurrSmbl,CCURRCODE   FROM SYCCURR "
lnResultCurr  = oAriaApplication.RemoteSystemData.Execute(lcSqlCommand,"",lcSYCCURR,"",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
IF lnResultCurr  > 0
  lfCreateIndex(lcSYCCURR,'CCURRCODE')
ENDIF

*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
*=lfOptProg()
IF TYPE('lcXMLFileName') = 'C'
  lcFormName = oAriaEnvironment.REPORT.GetForm('RMCMEM')
  lcPrgName  = lcFormName

  IF !EMPTY(oAriaEnvironment.Report.lcOptProg)
    lcPrgName = oAriaEnvironment.Report.lcOptProg
  ENDIF
  IF oAriaEnvironment.multiinst AND  FILE(oAriaEnvironment.clientreporthome+lcPrgName+'.FXP')
    =lfOptProg(oAriaEnvironment.clientreporthome+lcPrgName)
  ELSE
    IF FILE(oAriaEnvironment.ReportHome+lcPrgName+'.FXP')
      =lfOptProg(oAriaEnvironment.ReportHome+lcPrgName)
    ENDIF    
  ENDIF   
ELSE 
  =lfOptProg()
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]

PRIVATE lcPrgName
lcPrgName  = lcFormName
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
*llIsAparel = lfIsApparl(@lcPrgName)
IF TYPE('lcXMLFileName') = 'C'
  lcFormName = oAriaEnvironment.REPORT.GetForm('RMCMEM')
  lcPrgName  = lcFormName
  llIsAparel = oAriaEnvironment.REPORT.isapparell(@lcPrgName)
ELSE
  llIsAparel = lfIsApparl(@lcPrgName)
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
llNoRec    = .F.

IF llIsAparel
  DO EVAL('lcPrgName')
  IF !llNoRec
    DO ENDREPORT
  ENDIF
ELSE
SELECT RETHDR
lnInterval = SECONDS() - lcTime1
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
*WAIT WINDOW "Selected " + ALLTRIM(STR(RECCOUNT(lcRetLine))) + "Credit Memo(s), " + ALLTRIM(STR(RECCOUNT())) + " Transaction(s) in " + ALLTRIM(STR(lnInterval,6,2)) + " Seconds..." NOWAIT
IF TYPE('lcXMLFileName') <> 'C'
  WAIT WINDOW "Selected " + ALLTRIM(STR(RECCOUNT(lcRetLine))) + "Credit Memo(s), " + ALLTRIM(STR(RECCOUNT())) + " Transaction(s) in " + ALLTRIM(STR(lnInterval,6,2)) + " Seconds..." NOWAIT
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]

*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
*!*	IF llRmMemo
*!*	  loogScroll.cCROrientation = 'P'
*!*	  DO gfDispRe WITH EVAL('lcFormName') 
*!*	ENDIF
IF llRmMemo
  IF TYPE('lcXMLFileName') = 'C'
    oAriaEnvironment.REPORT.OGLastForm = lcFormName&& lcRpForm
    loProgress.Percent = 0.9
    loProgress.DESCRIPTION = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    PRIVATE loProxy
    loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
    
    oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
    loProgress.Percent = 1.0
    loProgress.DESCRIPTION = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  ELSE
    *=gfDispRe()
    DO gfDispRe WITH EVAL('lcFormName') 
  ENDIF
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]

ENDIF
IF RIGHT(lcFormName , 2) = 'FG'
  =lfUsrVldFn('lfSetCnt','RMCMEMFG')
ENDIF
*--Add the loop to update the 'FLAG' field [start]
*! B610059,1 MMT 08/27/2012 Exporting Credit memo to PDF does not update print flag in RETHDR[Start]
*IF oAriaApplication.gcDevice = 'PRINTER'
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
*IF loOgScroll.ll2Printer
LOCAL ll2Printer
IF TYPE('lcXMLFileName') = 'C'
  ll2Printer = .T.
ELSE
  ll2Printer = loOgScroll.ll2Printer
ENDIF
IF ll2Printer
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
*! B610059,1 MMT 08/27/2012 Exporting Credit memo to PDF does not update print flag in RETHDR[End]
  lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
  IF TYPE('lcTranCode') = 'N'
    =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
    RETURN .F.
  ENDIF
  llUpdate = .F.
  SELECT RetHdr
  SCAN 
    IF loRETHDR_X.seek(Rethdr.CRMEMO)
      loRETHDR_X.REPLACE([FLAG WITH 'Y'])
    ENDIF 
  ENDSCAN
  llUpdate =loRETHDR_X.TableUpdate(lcTranCode)
  IF !llUpdate
    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    RETURN .F.
  ELSE
    =oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
  ENDIF   
ENDIF

*-- Function section
*-------------------------------------------
*!*************************************************************
*! Name      : lfGetLogo
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
*! Purpose   : Function to Save the company logo in temp. file 
*!             which is used after this to print the logo for company.
*!*************************************************************
*! Called from : SORDCON.PRG
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetLogo()
*!*************************************************************
FUNCTION lfGetLogo
llLogo = .T.
lcLogoPic = loOgScroll.gfTempName()
lcObj_Id = OBJLINK.cObject_ID
*-- Select general field which have company logo.
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
*IF USED('Objects')
* SELECT ('Objects')
* USE 
*ENDIF
*=gfOpenFile(oAriaApplication.DataDir+'Objects',oAriaApplication.DataDir+'OBJECTID','SH')
IF TYPE('lcXMLFileName') <> 'C'
  IF USED('Objects')
    SELECT ('Objects')
    USE 
  ENDIF
 =gfOpenFile(oAriaApplication.DataDir+'Objects',oAriaApplication.DataDir+'OBJECTID','SH')  
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]

*: B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[Start]
*!*	SELECT gobject;
*!*	 FROM Objects         ;
*!*	 WHERE Objects.cobject_id = lcObj_Id ;
*!*	 INTO CURSOR (lcLogoPic)
SELECT mimgpath;
 FROM Objects         ;
 WHERE Objects.cobject_id = lcObj_Id ;
 INTO CURSOR (lcLogoPic)
*: B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[End]
*-- end of lfGetLogo.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 09/09/2005
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : SORDCON.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.

*!*************************************************************
*! Name        : lfHeadVar
*! Developer   : Heba Mohamed Amin (HMA)
*! Date        : 09/09/2005
*! Purpose     : Function to fill the approparate data for report header.
*!*************************************************************
*! Called from : SORDCONA.FRX [Header Band]
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : lfSolSpAdr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfHeadVar()
*!*************************************************************
FUNCTION lfHeadVar

lcAlias = ALIAS()
llEndGroup = .F.
= lfSolSpAdr()  && Calculate [Sold To and Ship To Data]

*-- Calculate (R/A # , Order #, Invoice #) data [Begin]
laMemoData[1,1] = IIF(!EMPTY(RetHdr.RaNo),'R/A #','')
laMemoData[1,2] = RetHdr.RaNo

laMemoData[2,1] = IIF(!EMPTY(RetHdr.Order),'Order #','')
laMemoData[2,2] = RetHdr.Order

laMemoData[3,1] = IIF(!EMPTY(RetHdr.Invoice),'Invoice #','')
laMemoData[3,2] = RetHdr.Invoice

*-- Shift empty data
FOR lnMemoData = 1 TO 3
  IF EMPTY(laMemoData[lnMemoData,1])
    = ADEL(laMemoData,lnMemoData)
  ENDIF
ENDFOR    && End of FOR Loop

*-- Replace logical items with ''
FOR lnMemoData = 1 TO 6
  IF TYPE('laMemoData[lnMemoData]') $ "UL"
    laMemoData[lnMemoData] = ''
  ENDIF
ENDFOR    && End of FOR Loop

*-- Add ":" to the end of 2nd and 3rd titles.
FOR lnMemoData = 2 TO 3
  IF !EMPTY(laMemoData[lnMemoData,1])
    laMemoData[lnMemoData,1] = laMemoData[lnMemoData,1] + ;
                               SPACE(16 - LEN(laMemoData[lnMemoData,1])) + ":"
  ENDIF
ENDFOR     && End of FOR Loop
*-- Calculate (R/A # , Order #, Invoice #) data [End]

SELECT (lcAlias)
RETURN ''
*-- end of lfAdrShift.

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 09/09/2005
*! Purpose   : Function to Get the Sold to Address, Ship to Address,
*!           : the Description of the Ship Via, Season,
*!           : Special Instructions, and Terms.
*!*************************************************************
*! Called from : lfHeadVar Function
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : gfRltFld, gfCodDes, gfGetAdr, lfAdrShift.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
FUNCTION lfSolSpAdr
lcAlias = ALIAS()
*: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[Start]
*= gfRltFld(RetHdr.CDivision , @laDivLName , 'CDIVISION')  && Get Division.
*: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[End]
lcTerms   = gfCodDes(RetHdr.CTERMCODE , 'CTERMCODE')        && Get Terms.

SELECT CUSTOMER
lcSolTName = BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
*!*	laSoldTo[6] = IIF(UPPER(ALLTRIM(gcContCode))='ENG',"Customer VAT No: " +CUSTOMER.CVATNO,'')
= lfAdrShift('laSoldTo')

lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
= lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.

* [Start] Get the Vendor # from Customwer file
IF llTocca
  lcVendCD = Customer.CCUSVEND
  lcDept = IIF(!EMPTY(RetHdr.INVOICE) AND SEEK(RetHdr.INVOICE,'INVHDR'),INVHDR.DEPT,'')
ENDIF
*C101751,1[End]
*-- end of lfSolSpAdr.

*!*************************************************************
*! Name        : lfGetNotes
*! Developer   : Heba Mohamed Amin (HMA)
*! Date        : 09/09/2005
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : SORDCONA.FRX [Variable lcDum in the report]
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetNotes()
*!*************************************************************
FUNCTION lfSaveFlag
llTitle =  !EVALUATE(lcRetLine+'.lEndLine')
RETURN ''
*-- end of lfGetNotes.

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 09/09/2005
*! Purpose   : Function to Update the End of Group flag and to update
*!             the Flag field in the ORDHDR file if the divice is not
*!             Screen.
*!*************************************************************
*! Called from : SORDCONA.FRX [GROUP FOOTER BAND]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : = lfEndGroup()
*!*************************************************************
FUNCTION lfEndGroup
llEndGroup = .T.   && We are in the end of the group (i.e : Order end.)
llTitle    = .T.

*IF The Divice is not Screen
IF llTextMode
  REPLACE RetHdr.Flag WITH 'Y'
ENDIF    && End of IF
RETURN '    '
*-- end of lfEndGroup.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 09/09/2005
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
*--Open all required files Remotly using remote table.
loRETHDR   = CREATEOBJECT('RemoteTable','RETHDR',,'RETHDR',SET("Datasession"),,.T.) 
loRETLINE  = CREATEOBJECT('RemoteTable','RETLINE',,'RETLINE',SET("Datasession"),,.T.) 
loRETHDR_X = CREATEOBJECT('RemoteTable','RETHDR','RETHDR','RETHDR_X',SET("Datasession"),,.T.) 
IF TYPE('lcTocca')='C' AND ALLTRIM(lcTocca) ='Y'
  llInvOp = gfOpenFile(gcDataDir+'INVHDR' , 'INVHDR' , 'SH')
ENDIF

*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 09/09/2005
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOptMsg()
*!*************************************************************
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
laOptMsg[1,2] = 75                && Line length

= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- end of lfvOptMsg.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 09/09/2005
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(OGSYS18())    && Varible to hold the old value
*-- end of lfwOldVal.


*!*************************************************************
*! Name      : lfChkSysDy
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 09/09/2005
*! Purpose   : To check if the system uses dyelot or not
*!*************************************************************
*! Called from : Option grid (Variable llDyelot)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : = lfChkSysDy()
*!*************************************************************

FUNCTION lfChkSysDy

RETURN (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')



*!*************************************************************
*! Name      : lfCreateWhereCondition
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 09/05/2005
*! Purpose   : Create Where Condition for SQL Statement
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCreateWhereCondition
lcWhere=""
*--Apply Filter of Status(Hidden filter)
lcWhere = "RETHDR.STATUS <> 'V' "

*--Create variable have the selected Return Date Range
lcDateRng=""
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
*lnDateRng=AT('RETHDR.CRDATE',loOgScroll.lcRpSqlExp)
IF TYPE('lcXMLFileName') = 'C'
  lnDateRng=AT('RETHDR.CRDATE', lcRpExp)
ELSE
  lnDateRng=AT('RETHDR.CRDATE',loOgScroll.lcRpSqlExp)
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
IF lnDateRng > 0
  *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
  *lcleftExp = SUBSTR(loOgScroll.lcRpSqlExp,lnDateRng)
  IF TYPE('lcXMLFileName') = 'C'
    lcleftExp = SUBSTR(lcRpExp, lnDateRng)
  ELSE
    lcleftExp = SUBSTR(loOgScroll.lcRpSqlExp,lnDateRng)
  ENDIF
  *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
  lnStrOfRng=AT('RETHDR.CRDATE',lcleftExp)
  lnEndOfRng=AT(')',lcLeftExp)
  lcDateRng=SUBSTR(lcleftexp,lnStrOfRng,lnEndOfRng-1)
  *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
  IF TYPE('lcXMLFileName') = 'C'
    lcDateRng = lcDateRng + " BETWEEN " + SUBSTR(lcLeftExp, AT(',', lcLeftExp, 1)+1, AT(',', lcLeftExp, 2) - AT(',', lcLeftExp, 1)-1)
    lcDateRng = lcDateRng + " AND " + SUBSTR(lcLeftExp, AT(',', lcLeftExp, 2)+1, AT(')', lcLeftExp, 2) - AT(',', lcLeftExp, 2)-1)
  ENDIF
  *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
ENDIF   
IF !EMPTY(lcDateRng) 
  lcwhere= lcwhere + " AND "+lcDateRng
ENDIF 

RETURN (lcwhere)

*!*************************************************************
*! Name      : lfGetJoins
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 09/05/2005
*! Purpose   : Create Join Statement required for Fox Statement
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfGetJoins
PRIVATE lcJoin
lcJoin=""

llCreditNo = .F.
lcTempCreditNo= lfCheckFilter(3, 'RETHDR.CRMEMO')
llCreditNo = !EMPTY(lcTempCreditNo) and USED(lcTempCreditNo) AND RECCOUNT(lcTempCreditNo)>0 

lcTempCreditAcc= lfCheckFilter(3, 'RETHDR.ACCOUNT')
llCreditAccount=!EMPTY(lcTempCreditAcc) and USED(lcTempCreditAcc) AND RECCOUNT(lcTempCreditAcc)>0 

lcTempCreditWare= lfCheckFilter(3, 'RETHDR.CWARECODE')
llCreditWare =!EMPTY(lcTempCreditWare) and USED(lcTempCreditWare) AND RECCOUNT(lcTempCreditWare)>0 

DO CASE 
  CASE llCreditNo 
      IF llCreditWare 
        lcJoin = lcJoin +" Inner join "+ lcTempCreditWare+" ON RETHDR.CWARECODE = "+lcTempCreditWare+".CWARECODE "
      ENDIF
      IF llCreditAccount
        lcJoin = lcJoin +" Inner join "+ lcTempCreditAcc+" ON RETHDR.ACCOUNT = "+lcTempCreditAcc+".ACCOUNT "
      ENDIF
   CASE   llCreditAccount 
      IF llCreditWare 
        lcJoin = lcJoin +" Inner join "+ lcTempCreditWare+" ON RETHDR.CWARECODE = "+lcTempCreditWare+".CWARECODE "
      ENDIF
ENDCASE 

RETURN (lcJoin)
*!*************************************************************
*! Name      : lfCheckFilter
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 09/05/2005
*! Purpose   : Check if the filter was selected
*!*************************************************************
*! Parameters : -Array type(Fixed ,variable,hidden)
*!              -Filter Name     
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCheckFilter()
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS   
lcReturn = ""     
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
IF TYPE('lcXMLFileName') = 'C'
  DO CASE
  CASE lnArrayType = 1 
    lnPOS = ASCAN(laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(laOGFxFlt,lnPos,1)
      lcReturn = laOGFxFlt[lnPOS,6]    
    ENDIF
  CASE lnArrayType = 2  
    lnPOS = ASCAN(laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(laOGHDFlt,lnPos,1)
      lcReturn = laOGHDFlt[lnPOS,6]    
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(laOGvrFlt,lnPos,1)
      
      *! B611018,1 SAR 06/18/2015 Request builder items missing T20150422.0007 [START]
      	*IF (ALLTRIM(UPPER(lcFilter)) = 'RETHDR.CRMEMO' AND ALLTRIM(UPPER(laOGvrFlt[lnPOS ,5])) = 'LIKE') AND !EMPTY(laOGvrFlt[lnPOS,6])
      	IF (ALLTRIM(UPPER(lcFilter)) = 'RETHDR.CRMEMO' AND ALLTRIM(UPPER(laOGvrFlt[lnPOS ,5])) = 'IN LIST') AND !EMPTY(laOGvrFlt[lnPOS,6])
      	   lcidpos = AT("INLIST(RETHDR.CRMEMO, '", lcrpExp)
      *! B611018,1 SAR 06/18/2015 Request builder items missing T20150422.0007 [END]
   
        lcAlias = ALIAS()
        lcTempCur =  gfTempName()
        CREATE CURSOR (lcTempCur) (KEYEXP C(6),CRMEMO C(6))
        SELECT(lcTempCur)
        INDEX on KEYEXP TAG (lcTempCur)
        APPEND BLANK 
         *! B611018,1 SAR 06/18/2015 Request builder items missing T20150422.0007 [START]
        *REPLACE KEYEXP  WITH laOGvrFlt[lnPOS,6],;
	  	 *     CRMEMO  WITH laOGvrFlt[lnPOS,6]
	  	 
	  	 REPLACE KEYEXP  WITH SUBSTR(lcrpExp, lcidpos + LEN("INLIST(RETHDR.CRMEMO, '"),6),;
	  	      CRMEMO  WITH  SUBSTR(lcrpExp, lcidpos + LEN("INLIST(RETHDR.CRMEMO, '"),6)
		 *! B611018,1 SAR 06/18/2015 Request builder items missing T20150422.0007 [END]  
        laOGvrFlt[lnPOS,6]	= lcTempCur
        SELECT(lcAlias )   
      ENDIF 
      lcReturn = laOGvrFlt[lnPOS,6]    
    ENDIF
  ENDCASE
ELSE
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
  DO CASE
  CASE lnArrayType = 1 
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ENDIF
  CASE lnArrayType = 2  
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      
      *B608555,1 MMT 05/15/2008 Fix bug of credit memo screen print button is not working[Start]
      IF (ALLTRIM(UPPER(lcFilter)) = 'RETHDR.CRMEMO' AND ALLTRIM(UPPER(loOgScroll.laOGvrFlt[lnPOS ,5])) = 'LIKE') AND !EMPTY(loOgScroll.laOGvrFlt[lnPOS,6])
        lcAlias = ALIAS()
        lcTempCur =  gfTempName()
        CREATE CURSOR (lcTempCur) (KEYEXP C(6),CRMEMO C(6))
        SELECT(lcTempCur)
        INDEX on KEYEXP TAG (lcTempCur)
        APPEND BLANK 
        REPLACE KEYEXP  WITH loOgScroll.laOGvrFlt[lnPOS,6],;
	  	      CRMEMO  WITH loOgScroll.laOGvrFlt[lnPOS,6]
		        
        loOgScroll.laOGvrFlt[lnPOS,6]	= lcTempCur
        SELECT(lcAlias )   
      ENDIF 
      *B608555,1 MMT 05/15/2008 Fix bug of credit memo screen print button is not working[End]
    
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ENDIF
  ENDCASE
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
ENDIF
*! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
RETURN lcReturn


*!*************************************************************
*! Name      : lfCreateIndex
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 09/14/2005
*! Purpose   : Create to the 
*!*************************************************************
*! Parameters : -File Name
*!              -Index Epression     
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCreateIndex
PARAMETERS lcFileName,lcIndexExp
lcOldAlias = ALIAS()

SELECT (lcFileName)
=CURSORSETPROP("Buffering",3,lcFileName)
INDEX ON &lcIndexExp TAG &lcFileName ADDITIVE   
SET ORDER TO TAG &lcFileName

SELECT(lcOldAlias )  
*!***************************************************************************
*!* Name        : lfGetSign
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 09/22/2005
*!* Purpose     : Get the symbol of the used currancy.
*!***************************************************************************
*!* Called from : the Report Frx
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfGetSign()
*!***************************************************************************
*!129694,1
FUNCTION lfGetSign

*B608395,1 MMT 12/30/2007 Fix bug of wrong amount Values while printing CM [Start]
PARAMETERS lcCurrCode
*B608395,1 MMT 12/30/2007 Fix bug of wrong amount Values while printing CM [End]

PRIVATE lcSign
lcSign = SPACE(3)
*    lcSign = IIF(SEEK(RETHDR.CCURRCODE,'SYCCURR'),SYCCURR.cCurrSmbl,lcSign)

*B608395,1 MMT 12/30/2007 Fix bug of wrong amount Values while printing CM [Start]
*lcSign = IIF(SEEK(RETHDR.CCURRCODE,lcSYCCURR),&lcSYCCURR..cCurrSmbl,lcSign)
lcSign = IIF(SEEK(lcCurrCode,lcSYCCURR),&lcSYCCURR..cCurrSmbl,lcSign)
*B608395,1 MMT 12/30/2007 Fix bug of wrong amount Values while printing CM [End]

RETURN lcSign
*-- End of Function .
*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/20/2005
*! Purpose   : Collect data
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCollectData
DO CASE
  CASE llCreditNo
    lcSqlTempStatment  = "SELECT "+lcSelectedFields+" FROM RETHDR(Index =RETHDR) WHERE "+lcWhereconditoin
    lcSqlTempStatment  = lcSqlTempStatment + " AND RETHDR.CRMEMO = '"        
    SELECT (lcTempCreditNo)
    SCAN 
      lcTempCredit = CRMEMO
      llSqlErro = loRetHdr.SqlRun(lcSqlTempStatment+lcTempCredit+"'","TCreditNo",.T.)
      IF llSqlErro 
        SELECT TCreditNo
        lcSelectedFields = "crMemo,crDate,RetHdr.cWareCode,Reference,RetHdr.Account,Store,SalesRep1,SalesRep2,;
                     CustPo,Cartons,Cretnote1,CretNote2,Amount,Other,Tax_Amt,nHstAmt,;
                       RaNo,Order,Invoice,cTermCode,cDivision,Flag,nPstAmt"
        lcSelectedFields =lcSelectedFields+",RETHDR.CCURRCODE"
        IF !EMPTY(lcInnerJoincondition)
           lcInnerJoincondition = STRTRAN(lcInnerJoincondition,'RETHDR','TCreditNo',-1,-1,1)
           lcSelectedFields = STRTRAN(lcSelectedFields ,'RETHDR','TCreditNo',-1,-1,1)
           SELECT &lcSelectedFields FROM TCreditNo &lcInnerJoincondition. INTO CURSOR TCreditNo READWRITE 
        ENDIF
        IF RECCOUNT() <> 0
          SELECT RETHDR
          APPEND FROM DBF('TCreditNo') 
          *** Get Details
          llSqlErro1 = loRetLine.SqlRun(lcSqlTempStatment1+lcTempCredit +"' AND CRET_TRNCD = '2'","TCreditNo1",.T.)
          IF llSqlErro1 
            SELECT TCreditNo1
            IF RECCOUNT() <> 0
              SELECT TCreditNo1 
              SCAN
                SCATTER MEMVAR
                SELECT (lcRetLine)
                APPEND BLANK
                GATHER MEMVAR
                SELECT TCreditNo1
              ENDSCAN     
              SELECT (lcRetLine)
              REPLACE lEndLine WITH .T.   && End lines for every group.
              *-- if user want to print notepad and there is notepad for this CrMemo 
			  IF llRpOrdNot AND SEEK('R' + crMemo , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
			    SELECT (lcRetLine)
			    LCMEMO=CrMemo
			    LCRETNO=CRET_LINNO
			    APPEND BLANK
			    REPLACE CrMemo     WITH LCMEMO ,;
			            mNotes     WITH ALLTRIM(NOTEPAD.mNotes) ,;
			            lEndLine   WITH .T. ,;
			            cindx      WITH '1'
			  ENDIF  
            ENDIF 
          ENDIF   
        ENDIF 
      ENDIF  
    ENDSCAN 
  
  CASE llCreditAccount
      
      lcSqlTempStatment  = "SELECT "+lcSelectedFields+" FROM RETHDR(Index =RETHDRA) WHERE "+lcWhereconditoin
      lcSqlTempStatment  = lcSqlTempStatment + " AND RETHDR.ACCOUNT = '"
      
      SELECT(lcTempCreditAcc)
      SCAN 
        lcTmpAccount=ACCOUNT
        llSqlErro = loRetHdr.SqlRun(lcSqlTempStatment+lcTmpAccount+"'","TCreditNo",.T.)
      IF llSqlErro 
        SELECT TCreditNo
        lcSelectedFields = "crMemo,crDate,RetHdr.cWareCode,Reference,RetHdr.Account,Store,SalesRep1,SalesRep2,;
                     CustPo,Cartons,Cretnote1,CretNote2,Amount,Other,Tax_Amt,nHstAmt,;
                       RaNo,Order,Invoice,cTermCode,cDivision,Flag,nPstAmt"
        lcSelectedFields =lcSelectedFields+",RETHDR.CCURRCODE"
        IF !EMPTY(lcInnerJoincondition)
           lcInnerJoincondition = STRTRAN(lcInnerJoincondition,'RETHDR','TCreditNo',-1,-1,1)
           lcSelectedFields = STRTRAN(lcSelectedFields ,'RETHDR','TCreditNo',-1,-1,1)
           SELECT &lcSelectedFields FROM TCreditNo &lcInnerJoincondition. INTO CURSOR TCreditNo READWRITE 
        ENDIF
        IF RECCOUNT() <> 0
          SELECT RETHDR
          APPEND FROM DBF('TCreditNo') 
          *** Get Details
          SELECT TCreditNo
          SCAN
		      lcMemoNo=crMemo
		      llSqlErro1 = loRetLine.SqlRun(lcSqlTempStatment1+lcMemoNo+"' AND CRET_TRNCD = '2'","TCreditNo1",.T.)
		      IF llSqlErro1 
		        SELECT TCreditNo1
		        IF RECCOUNT() <> 0
		          SELECT TCreditNo1 
		          SCAN
		            SCATTER MEMVAR
		            SELECT (lcRetLine)
		            APPEND BLANK
		            GATHER MEMVAR
		            SELECT TCreditNo1
		          ENDSCAN     
		          SELECT (lcRetLine)
		          REPLACE lEndLine WITH .T.   && End lines for every group.
		          *-- if user want to print notepad and there is notepad for this CrMemo 
			      IF llRpOrdNot AND SEEK('R' + lcMemoNo, 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
			        SELECT (lcRetLine)
			        LCMEMO=CrMemo
			        LCRETNO=CRET_LINNO
			        APPEND BLANK
			        REPLACE CrMemo     WITH LCMEMO ,;
			                mNotes     WITH ALLTRIM(NOTEPAD.mNotes) ,;
			                lEndLine   WITH .T. ,;
    			            cindx      WITH '1'
			      ENDIF  && End if 
		        ENDIF 
		      ENDIF
		  ENDSCAN
        ENDIF &&end of Reccount(TcreditNo) file
      ENDIF  && End if 
      ENDSCAN 
      
  CASE  llCreditWare    
    lcSqlTempStatment  = "SELECT "+lcSelectedFields+" FROM RETHDR(Index =RETHDR) WHERE "+lcWhereconditoin
    lcSqlTempStatment  = lcSqlTempStatment + " AND RETHDR.CWARECODE = '"

    SELECT(lcTempCreditWare)
    SCAN 
      lcWareHouse = &lcTempCreditWare..cwarecode 
      llSqlErro = loRetHdr.SqlRun(lcSqlTempStatment+lcWareHouse+"'","TCreditNo",.T.)
      IF llSqlErro 
        SELECT TCreditNo
        lcSelectedFields = "crMemo,crDate,RetHdr.cWareCode,Reference,RetHdr.Account,Store,SalesRep1,SalesRep2,;
                     CustPo,Cartons,Cretnote1,CretNote2,Amount,Other,Tax_Amt,nHstAmt,;
                       RaNo,Order,Invoice,cTermCode,cDivision,Flag,nPstAmt"
        lcSelectedFields =lcSelectedFields+",RETHDR.CCURRCODE"
        IF !EMPTY(lcInnerJoincondition)
           lcInnerJoincondition = STRTRAN(lcInnerJoincondition,'RETHDR','TCreditNo',-1,-1,1)
           lcSelectedFields = STRTRAN(lcSelectedFields ,'RETHDR','TCreditNo',-1,-1,1)
           SELECT &lcSelectedFields FROM TCreditNo &lcInnerJoincondition. INTO CURSOR TCreditNo READWRITE 
        ENDIF
        IF RECCOUNT() <> 0
          SELECT RETHDR
          APPEND FROM DBF('TCreditNo') 
          *** Get Details
          SELECT TCreditNo
          SCAN
			  lcMemoNo=crMemo
	          llSqlErro1 = loRetLine.SqlRun(lcSqlTempStatment1+lcMemoNo+"' AND CRET_TRNCD = '2'","TCreditNo1",.T.)
	          IF llSqlErro1 
	            SELECT TCreditNo1
	            IF RECCOUNT() <> 0
	              SELECT TCreditNo1 
	              SCAN
	                SCATTER MEMVAR
	                SELECT (lcRetLine)
	                APPEND BLANK
	                GATHER MEMVAR
	                SELECT TCreditNo1
	              ENDSCAN     
	              SELECT (lcRetLine)
	              REPLACE lEndLine WITH .T.   && End lines for every group.
	              *-- if user want to print notepad and there is notepad for this CrMemo 
			      IF llRpOrdNot AND SEEK('R' + lcMemoNo, 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
			        SELECT (lcRetLine)
			        LCMEMO=CrMemo
			        LCRETNO=CRET_LINNO
			        APPEND BLANK
			        REPLACE CrMemo     WITH LCMEMO ,;
			                mNotes     WITH ALLTRIM(NOTEPAD.mNotes) ,;
			                lEndLine   WITH .T. ,;
    			            cindx      WITH '1'
			      ENDIF  && End if 
	            ENDIF 
	          ENDIF   
          ENDSCAN
        ENDIF &&end of Reccount(TcreditNo) file
      ENDIF  && End if 
     ENDSCAN 

   OTHERWISE 
      
      lcSqlTempStatment = "SELECT "+lcSelectedFields+" FROM RETHDR (Index = RETHDR) WHERE " + lcWhereconditoin
      llSqlErro = loRetHdr.SqlRun(lcSqlTempStatment,"RetHdr",.T.)      
      lcStatement = "SELECT RetHdr.CRMEMO,RetHdr.ACCOUNT,RetHdr.cWareCode,RetHdr.Store," +;
                    "       RETLINE.Style,RETLINE.Cret_LinNo,RETLINE.cRet_trncd,RETLINE.Invoice,RETLINE.Reason,Qty1,Qty2," +;
                    "       Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,RETLINE.Price,RETLINE.Dyelot" +;
                    "  FROM RETLINE(INDEX = RETLINE)" +;
                    " INNER JOIN RETHDR (INDEX = RETHDR)" +;
                    "    ON RETLINE.CRMEMO = RETHDR.CRMEMO" +;
                    " WHERE CRET_TRNCD = '2'" + IIF(EMPTY(lcWhereconditoin), "", " AND ") + lcWhereconditoin
      llSqlErro = loRetHdr.SQLRUN(lcStatement, "TCreditNo1", .T.)
      SELECT TCreditNo1
      =CURSORSETPROP("Buffering", 3)
      INDEX ON CRMEMO TAG TCreditNo1
      =CURSORSETPROP("Buffering", 5)
      SET ORDER TO TCreditNo1
      LOCATE
      LOCAL lcCurMem
      lcCurMem = CRMEMO
      SCAN
        *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
        *WAIT WINDOW "Collecting data for Credit Memo: " + ALLTRIM(CRMEMO) NOWAIT
        IF TYPE('lcXMLFileName') <> 'C'
          WAIT WINDOW "Collecting data for Credit Memo: " + ALLTRIM(CRMEMO) NOWAIT
        ENDIF
        *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
        IF lcCurMem # CRMEMO
          lcCurMem = CRMEMO
          SELECT (lcRetLine)
          REPLACE lEndLine WITH .T.   && End lines for every group.??
          *-- if user want to print notepad and there is notepad for this CrMemo 
		  IF llRpOrdNot AND SEEK('R' + CRMEMO, 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
		    SELECT (lcRetLine)
	        LCMEMO=CrMemo
	        LCRETNO=CRET_LINNO
	        APPEND BLANK
	        REPLACE CrMemo     WITH LCMEMO ,;
	                mNotes     WITH ALLTRIM(NOTEPAD.mNotes) ,;
	                lEndLine   WITH .T. ,;
		            cindx      WITH '1'
		  ENDIF  && End if 
        ENDIF  
        SELECT TCreditNo1
        SCATTER MEMVAR
        SELECT (lcRetLine)
        APPEND BLANK
        GATHER MEMVAR
        
      ENDSCAN
      
       lfCreateindex('RETHDR','CRMEMO')
      lfCreateindex(lcRetLine,'CRMEMO+cIndx+STYLE+CRET_LINNO+CRET_TRNCD')
*!*	      lfCreateindex(lcRetLine,'CRMEMO+CRET_LINNO+STYLE+CRET_TRNCD')
     
ENDCASE
*!*************************************************************
*! Name      : lfEmptyFiles
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/20/2005
*! Purpose   : zap files
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfEmptyFiles

IF RECCOUNT('RETHDR')<>0
  SELECT RETHDR
  SET RELATION TO 
  =CURSORSETPROP("Buffering",3,"RETHDR")
  ZAP
ENDIF

IF USED(lcRetLine) AND RECCOUNT(lcRetLine) > 0
  SELECT(lcRetLine)
  SET RELATION TO 
  ZAP
ENDIF 

IF USED('RetLine') AND RECCOUNT('RetLine') > 0
  SELECT('RetLine')
  ZAP
ENDIF 

*: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[Start]
*!*******************************************************************
*! Name      : lfDivLngNam
*! Developer : Hesham Elmasry (HES)
*! Date      : 05/18/2009
*! Purpose   : Fill the cDivLngNam field with the Division long name
*!*******************************************************************
*! Return      : None
*!*******************************************************************

FUNCTION lfDivLngNam

CREATE CURSOR &lcTmpDivFl (CRMEMO C(6),cDivLngNam C(30))
INDEX ON CRMEMO TAG CRMEMO

SELECT RETHDR
LOCATE
SCAN
  =gfRltFld(RetHdr.CDivision , @laDivLName , 'CDIVISION')  && Get Division.
  INSERT INTO &lcTmpDivFl VALUES (RETHDR.CRMEMO,lcDivLName)
ENDSCAN

SELECT RETHDR
SET RELATION TO RETHDR.CRMEMO INTO &lcTmpDivFl ADDITIVE
* End of lfDivLngNam
*: B608865,1 HES 05/18/2009 Fix bug of Some CM's printing hundreds of identical copies[End]
*! B610059,1 MMT 08/27/2012 Exporting Credit memo to PDF does not update print flag in RETHDR[Start]
*!*************************************************************
*! Name      : lfUpdate
*! Developer : Mariam Mazhar(MMT)
*! Date      : 08/27/2012
*! Purpose   : CHECK IF Credit memo is PRINTED
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Called From : Frx
*!*************************************************************
FUNCTION lfUpdate

IF SYS(2040)='2'
  llPrinter = .T.
  *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [Start]
  *loOgScroll.ll2Printer = .T.
  IF TYPE('lcXMLFileName') <> 'C'  
    loOgScroll.ll2Printer = .T.
  ENDIF
  *! E303310,1 SAB 11/30/2012 Modify the Credit Memo Report to Run from Request Builder [End]
ENDIF
RETURN .T.
*! B610059,1 MMT 08/27/2012 Exporting Credit memo to PDF does not update print flag in RETHDR[End]