*:***************************************************************************
*: Program file  : ARCSLST
*: Program desc. : Customers Master List
*: System        : Aria 4XP
*: Module        : ACCOUNTS RECEIVABLE (AR), SALES ORDER (SO)
*: Developer     : Amgad EL Halawany (AEH)
*: Issue #       : 037683
*:***************************************************************************
*: Calls : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example       : DO ARCSLST
*:***************************************************************************
*:---OG Filter
*:    CUSTOMER.ACCOUNT   
*:    CUSTOMER.CFACCODE  
*:    CUSTOMER.SALESREP  
*:    CUSTOMER.CADDRESS4 
*:    CUSTOMER.CADDRESS5
*:    CUSTOMER.CLASS
*:    CUSTOMER.REGION    
*:    CUSTOMER.SPCINST   
*:    CUSTOMER.CTERMCODE 
*:    CUSTOMER.SHIPVIA   
*:    CUSTOMER.STATUS    
*:    CUSTOMER.lnZip
*:    CUSTOMER.ACCOUNT   
*:    CUSTOMER.ACCOUNT   
*:    customer.CDIVISION 
*:    Cutomer  lcRpStatus
*:    customer lcPrt     
*:    lcRpSort  
*:    
*:    
*****************************************************************************
*:MODIFICATIONS : 
*! AYM Account browser gets error -- alias conflicts
*! B131866,1 TNA 04/20/2006 Customer stores not printed when selecting a specific account
*! B609212,1 MMT 04/21/2010 Fix bug of Wrong report orientataion[T20100331.0044]
*! B609456,1 MMT 11/10/2010 customer report all codes don’t show in the report long format[T20101021.0012]
*! E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004]
*! E303079,2 MMT 03/08/2012 Fixing Media issues[T20120304.0004]
*! E303385,1 TMI 05/02/2013 Add the email address to the Long report format [T20130418.0038] 

#INCLUDE R:\Aria4xp\reports\arcslst.h
*! B609212,1 MMT 04/21/2010 Fix bug of Wrong report orientataion[Start]
lfvType()
*! B609212,1 MMT 04/21/2010 Fix bug of Wrong report orientataion[End]
DIMENSION laSoldTo[5] , laShipTo[5]
DIMENSION laCodDesc[5,3]
STORE '' TO laSoldTo , laShipTo , laCodDesc , lcShipVia , lcSRepName
STORE '' TO lcNotes
 
*! B609456,1 MMT 11/10/2010 customer report all codes don’t show in the report long format[Start]
laCodDesc[1,2] =   "CTERMCODE"
laCodDesc[2,2] =   "CDIVISION"  
laCodDesc[3,2] =   "REGION"
laCodDesc[4,2] =   "SPCINST"
laCodDesc[5,2] =   "CLASS"
*! B609456,1 MMT 11/10/2010 customer report all codes don’t show in the report long format[End] 
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
lcOldCustValue  = ''
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]
IF loOgScroll.llOGFltCh 
  *AYM Account browser gets error -- alias conflicts  [Begin]
  SELECT * FROM CUSTOMER_X WHERE ACCOUNT='*****' INTO CURSOR CUSTOMERF READWRITE
  *AYM Account browser gets error -- alias conflicts  [End]
  PRIVATE lcSqlTempStatment,lcSqlStatment,lcSelectedFields,lcWhereconditoin,lcOrderConditoin
  STORE .F. TO  llACCOUNT,llSalesrep,llFactorCode,lcTempAccount,lcTempFacCode,lcTempSales,llSqlErro
  *E303385,1 TMI 05/02/2013 [Start] add the field 'CEMAIL_ADD' to the fields list
  *lcSelectedFields = "Customer.Type,Customer.Status,Customer.Account,Customer.store,Phone1,Fax,shipVia,SalesRep,Comm,Buyer,Btname,stname,Customer.CFACCODE,cCont_Cod2,cCont_Code,"+;
                      "caddress1,caddress2,caddress3,Customer.caddress4,caddress5,caddress6,caddress12,caddress22,caddress32,caddress42,caddress52,caddress62,"+;
                      "Region,Class,ctermCode,cDivision,Spcinst,Priority,factacct,Duns,upszone,cinsur,crlimit,cravail"
  lcSelectedFields = "Customer.Type,Customer.Status,Customer.Account,Customer.store,Phone1,Fax,shipVia,SalesRep,Comm,Buyer,Btname,stname,Customer.CFACCODE,cCont_Cod2,cCont_Code,"+;
                      "caddress1,caddress2,caddress3,Customer.caddress4,caddress5,caddress6,caddress12,caddress22,caddress32,caddress42,caddress52,caddress62,"+;
                      "Region,Class,ctermCode,cDivision,Spcinst,Priority,factacct,Duns,upszone,cinsur,crlimit,cravail,CEMAIL_ADD"
  *E303385,1 TMI 05/02/2013 [End  ] 
  lcWhereConditoin = lfCreateWhereCondition()
  lcSqlTempStatment = "SELECT "+lcSelectedFields+" FROM  Customer"+lcWhereConditoin 
  WAIT WINDOW LANG_ARCSLST_SELCUST Nowait
  lcInnerJoincondition = lfGetJoins()
   DO CASE
    CASE llACCOUNT
      SELECT (lcTempAccount)
      lcSqlTempStatment = lcSqlTempStatment + " AND Customer.Account = '"
      SCAN
        lcTempAccountNo = Account
        llSqlErro = loDBFCustomer.SqlRun(lcSqlTempStatment+lcTempAccountNo+"'","TCustomer",.T.)
        SELECT TCustomer
        * B131866,1 TNA [Begin]
        *SCATTER MEMVAR MEMO
        *INSERT INTO CUSTOMERF FROM MEMVAR
        SCAN 
          SCATTER MEMVAR MEMO
          INSERT INTO CUSTOMERF FROM MEMVAR
        ENDSCAN
        * B131866,1 TNA [End]
      ENDSCAN
    CASE llFactorCode
      SELECT (lcTempFacCode)
      lcSqlTempStatment = lcSqlTempStatment + " AND Customer.CFACCODE = '"
      SCAN
        lcTempFactor = CFACCODE
        llSqlErro = loDBFCustomer.SqlRun(lcSqlTempStatment+lcTempFactor+"'","TFactCode",.T.)
        SELECT TFactCode
        * B131866,1 TNA [Begin]
        *SCATTER MEMVAR MEMO
        *INSERT INTO CUSTOMERF FROM MEMVAR
        SCAN 
          SCATTER MEMVAR MEMO
          INSERT INTO CUSTOMERF FROM MEMVAR
        ENDSCAN
        * B131866,1 TNA [End]
      ENDSCAN
    CASE llSalesrep
      SELECT (lcTempSales)
      lcSqlTempStatment = lcSqlTempStatment + " AND Customer.SalesRep = '"
      SCAN
        lcTempSalesRep = REPCODE
        llSqlErro = loDBFCustomer.SqlRun(lcSqlTempStatment+lcTempSalesRep+"'","TSalsRep",.T.)
        SELECT TSalsRep
        * B131866,1 TNA [Begin]
        *SCATTER MEMVAR MEMO
        *INSERT INTO CUSTOMERF FROM MEMVAR
        SCAN 
          SCATTER MEMVAR MEMO
          INSERT INTO CUSTOMERF FROM MEMVAR
        ENDSCAN
        * B131866,1 TNA [End]
      ENDSCAN
    OTHERWISE
      llSqlErro = loDBFCustomer.SqlRun(lcSqlTempStatment,"CUSTOMERF",.T.) 
  ENDCASE
  SELECT CUSTOMERF
  *AYM Account browser gets error -- alias conflicts  [Begin]
  *=TABLEUPDATE(.T.)
  *AYM Account browser gets error -- alias conflicts  [End]
  WAIT WINDOW LANG_ARCSLST_SELCUST Nowait
  lcFinalSQL=lfCreateInitalSQLStatment()
  *E303385,1 TMI 05/02/2013 [Start] add the 'CEMAIL_ADD' to the fields list
  *lcSelectedFields = "CUSTOMERF.Type,CUSTOMERF.Status,CUSTOMERF.Account,CUSTOMERF.store,Phone1,Fax,shipVia,SalesRep,Comm,Buyer,Btname,stname,CUSTOMERF.CFACCODE,cCont_Cod2,cCont_Code,"+;
                      "caddress1,caddress2,caddress3,CUSTOMERF.caddress4,caddress5,caddress6,caddress12,caddress22,caddress32,caddress42,caddress52,caddress62,"+;
                      "Region,Class,ctermCode,cDivision,Spcinst,Priority,factacct,Duns,upszone,cinsur,crlimit,cravail"
  lcSelectedFields = "CUSTOMERF.Type,CUSTOMERF.Status,CUSTOMERF.Account,CUSTOMERF.store,Phone1,Fax,shipVia,SalesRep,Comm,Buyer,Btname,stname,CUSTOMERF.CFACCODE,cCont_Cod2,cCont_Code,"+;
                      "caddress1,caddress2,caddress3,CUSTOMERF.caddress4,caddress5,caddress6,caddress12,caddress22,caddress32,caddress42,caddress52,caddress62,"+;
                      "Region,Class,ctermCode,cDivision,Spcinst,Priority,factacct,Duns,upszone,cinsur,crlimit,cravail,CEMAIL_ADD"
  *E303385,1 TMI 05/02/2013 [End  ] 
  DO CASE 
    CASE !EMPTY(lcFinalSQL) AND !EMPTY(lcInnerJoincondition)

      *WSH [Start]
      *SELECT &lcSelectedFields FROM CUSTOMERF WHERE &lcFinalSQL. &lcInnerJoincondition. INTO CURSOR CUSTOMERF
      SELECT &lcSelectedFields FROM CUSTOMERF &lcInnerJoincondition. WHERE &lcFinalSQL. INTO CURSOR CUSTOMERF READWRITE
      *WSH [End]
      
    CASE EMPTY(lcFinalSQL)  AND !EMPTY(lcInnerJoincondition)
      SELECT &lcSelectedFields FROM CUSTOMERF &lcInnerJoincondition. INTO CURSOR CUSTOMERF READWRITE
    CASE EMPTY(lcInnerJoincondition) AND !EMPTY(lcFinalSQL)
      SELECT &lcSelectedFields FROM CUSTOMERF WHERE &lcFinalSQL.  INTO CURSOR CUSTOMERF READWRITE
    CASE EMPTY(lcInnerJoincondition) AND EMPTY(lcFinalSQL)
  ENDCASE
  IF llSQLErro
    SELECT CUSTOMERF
    IF RECCOUNT()>0
      DO CASE
        CASE lcRpSort $ ' A'
          SORTFLD= "ACCOUNT+TYPE+STORE"
        CASE lcRpSort ='N'
          SORTFLD="STNAME+TYPE+STORE"
        CASE lcRpSort = 'S'
          SORTFLD = "CADDRESS4+ACCOUNT+TYPE+STORE"
        CASE lcRpSort ='R'
          SORTFLD='REGION+SALESREP+ACCOUNT+TYPE+STORE'
        CASE lcRpSort = 'Z'
          SORTFLD='CADDRESS5+ACCOUNT+TYPE+STORE'
      ENDCASE
      WAIT WINDOW LANG_ARCSLST_SORTCUST Nowait
      SELECT CUSTOMERF
      =CURSORSETPROP("Buffering",3,"CUSTOMERF")
      INDEX ON &SORTFLD TAG "CUSTOMERF"
      QSTATE = lcSTitle && State Title
      QZIP   = lcZTitle && Zip Title
      QSTATESORT = lcRpSort
      QZIPSORT   = lcRpSort
      IF llRpShipTo
        SCAN
        IF Type = "M"
            lcDivision  = cDivision  
            lcRegion    = Region
            lcTermCode  = cTermCode
            lcClass     = Class
            lcSpcInst   = SpcInst
            lcShipVia   = ShipVia
            lcAccount   = Account
            lcPriority  = Priority
            lcFacCode  = cFacCode
            lcFactAcct  = FactAcct
            lcDuns      = Duns
            IF SEEK('S'+lcAccount)
              SCAN REST WHILE TYPE+ACCOUNT = 'S'+lcAccount
                Replace cDivision WITH lcDivision,;
                        Region    WITH lcRegion,; 
                        cTermCode WITH lcTermCode,;
                        Class     WITH lcClass,;
                        SpcInst   WITH lcSpcInst,;
                        Priority  WITH lcPriority,;
                        cFacCode  WITH lcFacCode,;  
                        FactAcct  WITH lcFactAcct,;
                        Duns      WITH lcDuns
              ENDSCAN
            ENDIF   
          ELSE
            EXIT
          ENDIF
          =SEEK('M'+lcAccount)
        ENDSCAN
      ENDIF  
      WAIT CLEAR
      =gfDispRe(lcRpName)
    ELSE
      =gfModalGen('TRM00052B40011','ALERT')
     *AYM Account browser gets error -- alias conflicts  [Begin]
     *!*  USE IN CUSTOMERF
     *AYM Account browser gets error -- alias conflicts  [End]
      RETURN .F.
    ENDIF
  ELSE
    *-- SQL connection error. can't open the report
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN .F.
  ENDIF  
ELSE
  SELECT CUSTOMERF
  WAIT CLEAR
  gfDispRe(lcRpName)
ENDIF 
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
if !Empty(lcOldCustValue)
  lfRetCustVal()
ENDIF
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]

*-----                FUNCTIONS SECTION                 --------------------
*---------------------------------------------------------------------------
*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************

FUNCTION lfCheckFilter()
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS   
lcReturn = ""     
DO CASE
CASE lnArrayType = 1 
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
  
 *E302656 Show Task Pane Hassan [Begin] 2009-11-25
	IF (ALLTRIM(UPPER(lcFilter)) = 'CUSTOMER.ACCOUNT' AND ALLTRIM(UPPER(loOgScroll.laOGFXFlt[lnPOS ,5])) = 'LIKE') AND !EMPTY(loOgScroll.laOGFXFlt[lnPOS,6])
	  lcAlias = ALIAS()
	  lcTempCur =  gfTempName()
	  CREATE CURSOR (lcTempCur) (KEYEXP C(6),ACCOUNT C(5))
	  SELECT(lcTempCur)
	  INDEX on KEYEXP TAG (lcTempCur)
	  APPEND BLANK 
	  REPLACE KEYEXP  WITH loOgScroll.laOGFXFlt[lnPOS,6],;
		      Account   WITH loOgScroll.laOGFXFlt[lnPOS,6]
	  *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
 	  lcOldCustValue  = loOgScroll.laOGFXFlt[lnPOS,6]   
	  *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][end]
	  loOgScroll.laOGFXFlt[lnPOS,6]	= lcTempCur
	  SELECT(lcAlias)   
	ENDIF 
  *E302656 Show Task Pane Hassan [End] 2009-11-25  
  
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
    lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
  ENDIF
ENDCASE  
RETURN lcReturn

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Hossam El Etreby
*! Date      : 08/17/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen
DECLARE laRpSource[4],laRpTarget[1]
*--- Get Filter Postions in OG.
lnAccPos   = ASUBSCRIPT(loogscroll.laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
lnFactCode = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CFACCODE'),1)
lnRepPos   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.SALESREP'),1)
lnDivision = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CDIVISION'),1)
lnRegion   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.REGION'),1)
lnTerms    = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CTERMCODE'),1)
lnClass    = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CLASS'),1)
lnSpInst   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.SPCINST'),1)
lnShipVia  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.SHIPVIA'),1)
lnZip      = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CADDRESS5'),1)
lnStatus = lcRpStatus
STORE 'Active'    TO laRpSource[1] 
STORE ''          TO laRpTarget[1]
STORE 'Hold'      TO laRpSource[2] &&,laRpTarget[2]
STORE 'Potential' TO laRpSource[3]
STORE 'Canceled'  TO laRpSource[4]
lcRpStatus = "'A','H','P','X'"

                                        *lcTablename, lcTagName, lcViewAlias, lnDataSession, lcComp_Id, llFastTable
loDBFSales    = CreateObject("RemoteTable","SalesRep","SalesRep","SalesReps",SET("DATASESSION"),,.T.)
*! E303079,2 MMT 03/08/2012 Fixing Media issues[T20120304.0004][Start]
*loDBFCustomer = CreateObject("RemoteTable","CUSTOMER",,"CUSTOMER_X",SET("DATASESSION"),,.T.)
loDBFCustomer = CreateObject("RemoteTable","CUSTOMER","CUSTOMER","CUSTOMER_X",SET("DATASESSION"),,.T.)
*! E303079,2 MMT 03/08/2012 Fixing Media issues[END]
loDBFNotePad  = CreateObject("RemoteTable","NotePad","NotePad","NotePads",SET("DATASESSION"),,.T.)

*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfStitle
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : To get the Title for the STATE ,ZIP
*!             according to its country
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStitle()
*!*************************************************************

FUNCTION lfStitle
*HMA
*!*  PRIVATE lc2Return

*SET ORDER TO CCOMP_ID IN SYCCOMP   && To use it to get state title.
*IF !USED('SYCINT')
*  = gfOpenFile(oAriaApplication.SysPath + "SYCINT","CCONTCODE",'SH')
*ELSE
*  SET ORDER TO CCONTCODE IN SYCINT   && To use it to get state title.
*ENDIF


*!*  loDBSYCINT1 = CreateObject("RemoteTable","SYCINT","CCONTCODE","SYCINT",SET("DATASESSION"))
*!*  = SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP','CCOMP_ID') AND loDBSYCINT1.SEEK(SYCCOMP.CCONT_CODE)
*!*  lcZipTitle = SYCINT.CPART5LAB
*!*  lc2Return = SYCINT.CPART4LAB
*!*  USE IN SYCINT
*!*  RETURN (lc2Return)
PRIVATE lcSelectCommand,lnResult,lcSelectCommand1,lnResult1
lcWorkArea = SELECT()
lcSelectCommand=[SELECT CCONT_CODE FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
IF lnResult >= 1 
   lcSelectCommand1=[SELECT CCONT_CODE,CPART4LAB,CPART5LAB FROM SYCINT WHERE SYCINT.CCONT_CODE=SYCCOMP.CCONT_CODE]
   lnResult1 = oAriaApplication.remotesystemdata.execute(lcSelectCommand1,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  IF lnResult1 >= 1  
    SELECT (lcWorkArea)    
    RETURN (SYCINT.CPART4LAB)
    lcZipTitle = SYCINT.CPART5LAB
  ENDIF 
ENDIF      
SELECT (lcWorkArea) 
*HMA
*-- end of lfStitle.
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvStates
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : Valid for the states selection
*!             according to its country
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStates()
*!*************************************************************

FUNCTION lfvStates

PRIVATE lcFile_Ttl, lcBrfields, lcStateObj , lcStateVal

lcStateObj = SYS(18)                    && Varible to hold  the name of the memory variable used to create the current GET field
lcStateVal = PADR(EVALUATE(SYS(18)),6)  && Varible to hold  the value of the current GET field
*IF The user want to Browse or if the state code he entered is not in the file.
IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK('N'+PADR('STATE',10) + ALLTRIM(lcStateVal),'CODES'))
   lnCurAlias = SELECT(0)
   *-- browse all country codes [begin]
   SELECT CODES
   DECLARE laCodeFld[2]
   lcFile_Ttl = 'State Codes'
   lcBrfields = 'cCode_No :H= "State Code" ,cDiscrep :H="Description" :30'

   IF gfBrows('FOR cDefCode + cfld_name+ccode_no+cdiscrep = ;
     "N" + "STATE" AND ;
     cRltField="N"','cCode_No,cDiscrep','laCodeFld')
     lcStateVal = laCodeFld[1]
   ENDIF
   SELECT (lnCurAlias)
ENDIF    
*!*************************************************************
*! Name      : lfSortDumy
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : Fill Sort Arrays.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSortDumy()
*!*************************************************************

FUNCTION lfSortDumy

DIMENSION laSortDesc[5,1] , laSortVal[5,1]

laSortDesc[1] = 'Customer Account'
laSortDesc[2] = 'Customer Name'
laSortDesc[3] = lcSTitle
laSortDesc[4] = lcZTitle
laSortDesc[5] = 'Region'

laSortVal[1] = 'A'
laSortVal[2] = 'N'
laSortVal[3] = 'S'
laSortVal[4] = 'Z'
laSortVal[5] = 'R'
*!*************************************************************
*! Name      : lfZtitle
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : To get the Title for the STATE ,ZIP
*!             according to its country
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfZtitle()
*!*************************************************************

FUNCTION lfZtitle

*HMA
*!*  SET ORDER TO CCOMP_ID IN SYCCOMP   && To use it to get state title.
*!*  loDBSYCINT2    = CreateObject("RemoteTable","SYCINT","CCONTCODE","SYCINT",SET("DATASESSION"))
*!*  = SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP','CCOMP_ID') AND loDBSYCINT2.SEEK(SYCCOMP.CCONT_CODE)
*!*  lcZipTitle = SYCINT.CPART5LAB
*!*  RETURN (SYCINT.CPART5LAB)
PRIVATE lcSelectCommand,lnResult,lcSelectCommand1,lnResult1
IF !USED('SYCCOMP')
  lcWorkArea = SELECT()
  lcSelectCommand=[SELECT CCONT_CODE FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
  lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
  IF lnResult >= 1 
     lcSelectCommand1=[SELECT CCONT_CODE,CPART5LAB FROM SYCINT WHERE SYCINT.CCONT_CODE=SYCCOMP.CCONT_CODE]
     lnResult1 = oAriaApplication.remotesystemdata.execute(lcSelectCommand1,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
    IF lnResult1 >= 1  
      SELECT (lcWorkArea)    
      lcZipTitle = SYCINT.CPART5LAB
      RETURN (SYCINT.CPART5LAB)
    ENDIF     
  ENDIF      
  SELECT (lcWorkArea) 
ELSE 
  lcZipTitle = SYCINT.CPART5LAB
  RETURN (SYCINT.CPART5LAB)
ENDIF 
*HMA
*-- end of lfZtitle.
*--------------------------------------------------------------------------
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
= lfOGMover(@laRpSource,@laRpTarget,'Select Customer Status',.T.,'')  && call mover function.
lcRpStatus = ' '
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Active',"'A'",;
                            IIF(laRpTarget[lnI] = 'Hold',",'H'",;
                            IIF(laRpTarget[lnI] = 'Potential',",'P'",;
                            IIF(laRpTarget[lnI] = 'Canceled',",'X'",''))))

ENDFOR  && end Loop to make Status expression.

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]
*-- End of lfvOStatus.

*!**************************************************************************
*! Name      : lfvType
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/29/1999
*! Purpose   : Valid for Report Type
*!**************************************************************************
*! Called from : OG 
*!**************************************************************************
*! Example     : = lfvType()
*!**************************************************************************
*E301353,1 it's done especially to assign Short or Long form to Report name Frx
FUNCTION lfvType
IF lcRpType = 'S'
  loogScroll.cCROrientation = 'P'  
  lcRpName = 'ARCSLSTS'      && Short form 
ELSE
  loogScroll.cCROrientation = 'L'
  lcRpName = 'ARCSLSTL'      && Long form
ENDIF
*-- End of lfvType.

*!**************************************************************************
*! Name      : lfAddrDesc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/29/1999
*! Purpose   : Counts Main Account Or Stores
*!**************************************************************************
*! Called from : FRX 
*!**************************************************************************
*! Return      : Arrays filled with Sold To, Ship To Addresses , Code Description
*!**************************************************************************
*! Example     : = lfAddrDesc()
*!**************************************************************************
*E301353,1 it's done especially for FRX to get addresses and Code descriptions
FUNCTION lfAddrDesc

=gfGetAdr(lcTempCust, '' , '' , '' , @laSoldTo,'2')
=gfGetAdr(lcTempCust, '' , '' , '' , @laShipTo) 

lcShipVia = gfCodDes(ShipVia,'ShipVia')   && get code description for ShipVia
IF lcRpType = 'L'
  *! B609456,1 MMT 11/10/2010 customer report all codes don’t show in the report long format[Start]
  laCodDesc[1,1] =   CTERMCODE
  laCodDesc[2,1] =   CDIVISION
  laCodDesc[3,1] =   REGION
  laCodDesc[4,1] =   SPCINST
  laCodDesc[5,1] =   CLASS
  *! B609456,1 MMT 11/10/2010 customer report all codes don’t show in the report long format[End] 

  =gfCodDes(@laCodDesc)
ENDIF
  lcNotes = IIF(loDBFNotePad.SEEK('A' + IIF(TYPE='M',Account,SPACE(5))),NotePads.mNotes,'')  
  lcSRepName = IIF(loDBFSales.SEEK(SalesRep),SalesReps.Name,'') 
RETURN ''
*-- End of lfAddrDesc.

*!**************************************************************************
*! Name      : lfPrtNotes
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/29/1999
*! Purpose   : Detect whether to print Customer Notepad or not
*!**************************************************************************
*! Called from : Windows FRX only
*!**************************************************************************
*! Return      : True or False
*!**************************************************************************
*! Example     : = lfPrtNotes()
*!**************************************************************************

FUNCTION lfPrtNotes
RETURN llRpNote AND !EMPTY(lcNotes)



******************** SSH mover functions

FUNCTION lfCreateString
LPARAMETERS lcFilterName

PRIVATE lcString2Return
lcString2Return = ""
lnPostion = ASCAN(loOgScroll.laOgFXFlt,lcFilterName,1,ALEN(loOgScroll.laOgFXFlt,1),1,10)
IF  (lnPostion > 0 ) AND !EMPTY(loOgScroll.laOgFxFlt[lnPostion,6])
  lcString2Return = loOgScroll.laOgFxFlt[lnPostion,6]
  lcString2Return = STRTRAN(lcString2Return,"|",",")
ENDIF
RETURN (lcString2Return)

FUNCTION lfCreateWhereCondition
PRIVATE lcWhere

lcRpStatus = IIF(SUBSTR(lcRpStatus,1,1)=",",SUBSTR(lcRpStatus,2),lcRpStatus)
lcWhere = " Where Customer.Status IN  (" + lcRpStatus +")"
*-------------------------Priority
IF !EMPTY(lcPrt)
  lcWhere = lcWhere + " And Customer.priority = '" + lcPrt + "'"
ENDIF 
IF !llRpShipTo
  lcWhere = lcWhere + " AND Customer.Type = 'M'"
ENDIF
IF  !EMPTY(laOGFxFlt[lnZip     , 6])
  IF !LEN(lcWhere) = 0
    lcWhere = lcWhere + " And Customer.CADDRESS5 = '" + laOGFxFlt[lnZip     , 6] + " '"
  ELSE
    lcWhere = " Where Customer.CADDRESS5 = '" + RTRIM(laOGFxFlt[lnZip     , 6]) + " '"
  ENDIF
ENDIF

RETURN (lcWhere)


FUNCTION lfGetJoins
PRIVATE lcJoin

lcJoin=""
* Check if there is a filter on Account
lcTempAccount = lfCheckFilter(1, 'CUSTOMER.ACCOUNT')    
IF !EMPTY(lcTempAccount) AND USED(lcTempAccount)
  SELECT &lcTempAccount
  llACCOUNT = (RECCOUNT() > 0) 
  IF llACCOUNT
    lcJoin = lcJoin +" Inner join "+ lcTempAccount +" ON CustomerF.Account = "+lcTempAccount+".Account "
  ENDIF
ENDIF

lcTempFacCode = lfCheckFilter(1, 'CUSTOMER.CFACCODE')    
IF !EMPTY(lcTempFacCode) AND USED(lcTempFacCode)
  SELECT &lcTempFacCode
  llFactorCode = (RECCOUNT() > 0) 
  IF llFactorCode
    lcJoin = lcJoin +" Inner join "+ lcTempFacCode +" ON CustomerF.CFACCODE = "+lcTempFacCode+".CFACCODE "
  ENDIF
ENDIF

* Check if there is a filter on Sales Representative
lcTempSales = lfCheckFilter(1, 'CUSTOMER.SALESREP')    
IF !EMPTY(lcTempSales) AND USED(lcTempSales)
  SELECT &lcTempSales
  llSalesrep = (RECCOUNT() > 0) 
  IF llSalesrep
    lcJoin = lcJoin +" Inner join "+ lcTempSales +" ON CustomerF.SalesRep = "+lcTempSales+".REPCODE "
  ENDIF
ENDIF
*!*	susp
*IF !EMPTY(lcDivision)
lcState = lfCreateString("CUSTOMER.CADDRESS4")
IF !EMPTY(lcState)
   CREATE CURSOR StateCur (CADDRESS4 C (6))
   =lcString2Cursor(lcState,"StateCur","CADDRESS4")
   *PARAMETERS      lcString  ,lcCursor ,lcFieldsName
   lcJoin = lcJoin +" Inner join  StateCur  ON CustomerF.CADDRESS4= StateCur.CADDRESS4"
   *WSH [Start]
   *lcSQL2Return = lcSQL2Return+lcOperator+" Customer.CADDRESS4 $ '"+lcState+"'"
   *lcSQL2Return = lcSQL2Return+lcOperator+" ALLTRIM(Customer.CADDRESS4) $ '"+lcState+"'"
   *WSH [End]
ENDIF

RETURN(lcJoin)


****************************************
FUNCTION lfCreateInitalSQLStatment

******************************************
PRIVATE lcSQL2Return , lcOperator
******************************************
lcOperator = " "
lcSQL2Return = ""

lcShipVia = lfCreateString("CUSTOMER.SHIPVIA")
IF !EMPTY(lcShipVia)
   lcSQL2Return = lcSQL2Return+lcOperator+" CustomerF.SHIPVIA $ '"+lcShipVia+"'"
   lcOperator = " AND "
ENDIF
  
lcDivision = lfCreateString("CUSTOMER.CDIVISION")
IF !EMPTY(lcDivision)
   lcSQL2Return = lcSQL2Return+lcOperator+" CustomerF.CDIVISION $ '"+lcDivision+"'"
   lcOperator = " AND "
ENDIF

lcClass = lfCreateString("CUSTOMER.CLASS")

*WSH [Start]
*IF !EMPTY(lcDivision)
IF !EMPTY(lcClass)
*WSH [End]

   lcSQL2Return = lcSQL2Return+lcOperator+" CustomerF.CLASS $ '"+lcClass+"'"
   lcOperator = " AND "
ENDIF

lcRegion = lfCreateString("CUSTOMER.REGION")

*WSH [Start]
*IF !EMPTY(lcDivision)
IF !EMPTY(lcRegion)
*WSH [End]

   lcSQL2Return = lcSQL2Return+lcOperator+" CustomerF.REGION $ '"+lcRegion+"'"
   lcOperator = " AND "
ENDIF

lcSpInst = lfCreateString("CUSTOMER.SPCINST")

*WSH [Start]
*IF !EMPTY(lcDivision)
IF !EMPTY(lcSpInst)
*WSH [End]

   lcSQL2Return = lcSQL2Return+lcOperator+" CustomerF.SPCINST $ '"+lcSpInst+"'"
   lcOperator = " AND "
ENDIF

lcTerms = lfCreateString("CUSTOMER.CTERMCODE")

*WSH [Start]
*IF !EMPTY(lcDivision)
IF !EMPTY(lcTerms)
*WSH [End]

   lcSQL2Return = lcSQL2Return+lcOperator+" CustomerF.CTERMCODE $ '"+lcTerms+"'"
   lcOperator = " AND "
ENDIF

lcState = lfCreateString("CUSTOMER.CADDRESS4")

RETURN(lcSQL2Return)

*!************************************************************
*! Name      : RefreshStatus
*: Developer : Heba Fathi (HFK)
*: Date      : 08/17/2004
*! Purpose   : function to Set the index for the SQL files
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
FUNCTION RefreshStatus
LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 


******************************
FUNCTION lcString2Cursor
PARAMETERS lcString , lcCursor , lcFieldsName
******************************

DO WHILE AT(',',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT(',',lcString)-1)
  lcString = SUBSTR(lcString,AT(',',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
FUNCTION lfRetCustVal 
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,'CUSTOMER.ACCOUNT')
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
   loOgScroll.laOGFxFlt[lnPos,6] = lcOldCustValue   
  ENDIF
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]