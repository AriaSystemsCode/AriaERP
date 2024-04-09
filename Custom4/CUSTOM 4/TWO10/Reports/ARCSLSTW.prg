*:***************************************************************************
*: Program file  : ARCSLSWS
*: Program desc. : Customers Master List For Two Start Dog(TWO10)
*: System        : Aria 4XP
*: Module        : ACCOUNTS RECEIVABLE (AR), SALES ORDER (SO)
*: Developer     : Mariam Mazhar(MMT)
*: Issue #       : C201007 [T20080513.0018]
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
*****************************************************************************

#INCLUDE R:\Aria4xp\reports\arcslst.h

DIMENSION laSoldTo[5] , laShipTo[5]
DIMENSION laCodDesc[5,3]
STORE '' TO laSoldTo , laShipTo , laCodDesc , lcShipVia , lcSRepName
STORE '' TO lcNotes
 
IF loOgScroll.llOGFltCh 
  SELECT * FROM CUSTOMER_X WHERE ACCOUNT='*****' INTO CURSOR CUSTOMERF READWRITE
  PRIVATE lcSqlTempStatment,lcSqlStatment,lcSelectedFields,lcWhereconditoin,lcOrderConditoin
  STORE .F. TO  llACCOUNT,llSalesrep,llFactorCode,lcTempAccount,lcTempFacCode,lcTempSales,llSqlErro
  lcSelectedFields = "Customer.Type,Customer.Status,Customer.Account,Customer.store,Phone1,Fax,shipVia,SalesRep,Comm,Buyer,Btname,stname,Customer.CFACCODE,cCont_Cod2,cCont_Code,"+;
                      "caddress1,caddress2,caddress3,Customer.caddress4,caddress5,caddress6,caddress12,caddress22,caddress32,caddress42,caddress52,caddress62,"+;
                      "Region,Class,ctermCode,cDivision,Spcinst,Priority,factacct,Duns,upszone,cinsur,crlimit,cravail,cemail_add"
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
        SCAN 
          SCATTER MEMVAR MEMO
          INSERT INTO CUSTOMERF FROM MEMVAR
        ENDSCAN
      ENDSCAN
    CASE llFactorCode
      SELECT (lcTempFacCode)
      lcSqlTempStatment = lcSqlTempStatment + " AND Customer.CFACCODE = '"
      SCAN
        lcTempFactor = CFACCODE
        llSqlErro = loDBFCustomer.SqlRun(lcSqlTempStatment+lcTempFactor+"'","TFactCode",.T.)
        SELECT TFactCode
        SCAN 
          SCATTER MEMVAR MEMO
          INSERT INTO CUSTOMERF FROM MEMVAR
        ENDSCAN
      ENDSCAN
    CASE llSalesrep
      SELECT (lcTempSales)
      lcSqlTempStatment = lcSqlTempStatment + " AND Customer.SalesRep = '"
      SCAN
        lcTempSalesRep = REPCODE
        llSqlErro = loDBFCustomer.SqlRun(lcSqlTempStatment+lcTempSalesRep+"'","TSalsRep",.T.)
        SELECT TSalsRep
        SCAN 
          SCATTER MEMVAR MEMO
          INSERT INTO CUSTOMERF FROM MEMVAR
        ENDSCAN
      ENDSCAN
    OTHERWISE
      llSqlErro = loDBFCustomer.SqlRun(lcSqlTempStatment,"CUSTOMERF",.T.) 
  ENDCASE
  SELECT CUSTOMERF
  WAIT WINDOW LANG_ARCSLST_SELCUST Nowait
  lcFinalSQL=lfCreateInitalSQLStatment()
  lcSelectedFields = "CUSTOMERF.Type,CUSTOMERF.Status,CUSTOMERF.Account,CUSTOMERF.store,Phone1,Fax,shipVia,SalesRep,Comm,Buyer,Btname,stname,CUSTOMERF.CFACCODE,cCont_Cod2,cCont_Code,"+;
                      "caddress1,caddress2,caddress3,CUSTOMERF.caddress4,caddress5,caddress6,caddress12,caddress22,caddress32,caddress42,caddress52,caddress62,"+;
                      "Region,Class,ctermCode,cDivision,Spcinst,Priority,factacct,Duns,upszone,cinsur,crlimit,cravail,cemail_add"
  DO CASE 
    CASE !EMPTY(lcFinalSQL) AND !EMPTY(lcInnerJoincondition)

      SELECT &lcSelectedFields FROM CUSTOMERF &lcInnerJoincondition. WHERE &lcFinalSQL. INTO CURSOR CUSTOMERF READWRITE
      
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

*-----                FUNCTIONS SECTION                 --------------------
*---------------------------------------------------------------------------
*************************************************************
*! Name      : lfCheckFilter
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
*! Purpose   : Check if the filter was selected
*!*************************************************************

FUNCTION lfCheckFilter()
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS   
lcReturn = ""     
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
    lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
  ENDIF
ENDCASE  
RETURN lcReturn

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
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
loogScroll.cCROrientation = 'P'  
                                        *lcTablename, lcTagName, lcViewAlias, lnDataSession, lcComp_Id, llFastTable
loDBFSales    = CreateObject("RemoteTable","SalesRep","SalesRep","SalesReps",SET("DATASESSION"),,.T.)
loDBFCustomer = CreateObject("RemoteTable","CUSTOMER",,"CUSTOMER_X",SET("DATASESSION"),,.T.)
loDBFNotePad  = CreateObject("RemoteTable","NotePad","NotePad","NotePads",SET("DATASESSION"),,.T.)

*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfStitle
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
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
*-- end of lfStitle.
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvStates
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
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
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
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
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
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
*-- end of lfZtitle.
*!**************************************************************************
*! Name      : lfvOStatus
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
*! Purpose   : Validate Status
*!**************************************************************************
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
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
*! Purpose   : Valid for Report Type
*!**************************************************************************
*! Called from : OG 
*!**************************************************************************
*! Example     : = lfvType()
*!**************************************************************************
FUNCTION lfvType
IF lcRpType = 'S'
  loogScroll.cCROrientation = 'P'  
  lcRpName = 'ARCSLSWS'      && Short form 
ELSE
  loogScroll.cCROrientation = 'L'
  lcRpName = 'ARCSLSWL'      && Long form
ENDIF
*-- End of lfvType.

*!**************************************************************************
*! Name      : lfAddrDesc
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
*! Purpose   : Counts Main Account Or Stores
*!**************************************************************************
*! Called from : FRX 
*!**************************************************************************
*! Return      : Arrays filled with Sold To, Ship To Addresses , Code Description
*!**************************************************************************
*! Example     : = lfAddrDesc()
*!**************************************************************************
FUNCTION lfAddrDesc

=gfGetAdr(lcTempCust, '' , '' , '' , @laSoldTo,'2')
=gfGetAdr(lcTempCust, '' , '' , '' , @laShipTo) 

lcShipVia = gfCodDes(ShipVia,'ShipVia')   && get code description for ShipVia
IF lcRpType = 'L'
  =gfCodDes(@laCodDesc)
ENDIF
  lcNotes = IIF(loDBFNotePad.SEEK('A' + IIF(TYPE='M',Account,SPACE(5))),NotePads.mNotes,'')  
  lcSRepName = IIF(loDBFSales.SEEK(SalesRep),SalesReps.Name,'') 
RETURN ''
*-- End of lfAddrDesc.

*!**************************************************************************
*! Name      : lfPrtNotes
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
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

*!************************************************************
*! Name      : lfCreateString
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
*! Purpose   : function to Create Mover expression
*!************************************************************
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

*!************************************************************
*! Name      : lfCreateWhereCondition
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
*! Purpose   : function to Create Where Condition
*!************************************************************
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

*!************************************************************
*! Name      : lfGetJoins
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
*! Purpose   : function to Create Join Expression
*!************************************************************
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
lcState = lfCreateString("CUSTOMER.CADDRESS4")
IF !EMPTY(lcState)
   CREATE CURSOR StateCur (CADDRESS4 C (6))
   =lcString2Cursor(lcState,"StateCur","CADDRESS4")
   lcJoin = lcJoin +" Inner join  StateCur  ON CustomerF.CADDRESS4= StateCur.CADDRESS4"
ENDIF

RETURN(lcJoin)


*!************************************************************
*! Name      : lfCreateInitalSQLStatment
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
*! Purpose   : function to Create SQL statement
*!************************************************************
FUNCTION lfCreateInitalSQLStatment
PRIVATE lcSQL2Return , lcOperator
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

IF !EMPTY(lcClass)
   lcSQL2Return = lcSQL2Return+lcOperator+" CustomerF.CLASS $ '"+lcClass+"'"
   lcOperator = " AND "
ENDIF

lcRegion = lfCreateString("CUSTOMER.REGION")

IF !EMPTY(lcRegion)
   lcSQL2Return = lcSQL2Return+lcOperator+" CustomerF.REGION $ '"+lcRegion+"'"
   lcOperator = " AND "
ENDIF

lcSpInst = lfCreateString("CUSTOMER.SPCINST")

IF !EMPTY(lcSpInst)
   lcSQL2Return = lcSQL2Return+lcOperator+" CustomerF.SPCINST $ '"+lcSpInst+"'"
   lcOperator = " AND "
ENDIF

lcTerms = lfCreateString("CUSTOMER.CTERMCODE")

IF !EMPTY(lcTerms)
   lcSQL2Return = lcSQL2Return+lcOperator+" CustomerF.CTERMCODE $ '"+lcTerms+"'"
   lcOperator = " AND "
ENDIF

lcState = lfCreateString("CUSTOMER.CADDRESS4")

RETURN(lcSQL2Return)

*!************************************************************
*! Name      : RefreshStatus
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
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
*!************************************************************
*! Name      : lcString2Cursor
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/02/2008
*! Purpose   : function to Convert string to cursor
*!************************************************************
FUNCTION lcString2Cursor
PARAMETERS lcString , lcCursor , lcFieldsName

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
