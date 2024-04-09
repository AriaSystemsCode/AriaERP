*:****************************************************************************
*: Program file        : SOXLSTY.PRG
*: Program description : Import Customer or Stores or Vendor from Excel file.
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SO)
*: Developer Name      : RANIA ABDEL RAZIK (RAE)
*: Date completed      : 01/10/2002
*: Tracking Job Number : C102731
*:****************************************************************************

lcCurrFile = ""
lcFPer     = "0  %"
lcFTherm   = SPACE(1)
lcOSource  = SPACE(1)   && Old surce directory.
lcDefTerm  = ""
lnThermSiz = 33        && The default size of the thermometers.
DECLARE laFiscYear[1,3]
lcOSource1 = ""
lcSource1  = ""

*------- Vendor file
lcVendorFl = gfTempName()
*------- Customer file
lcCustFl   = gfTempName()
*------- Store file
lcStorFl   = gfTempName()

DO (gcScrDir +  gcAct_Appl + '\APIMPVE.SPR')

*!*************************************************************
*! Name       : lffillcode
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 01/10/2002
*! Purpose    : Check Codes.
*!*************************************************************
*! Calls      : =gfOpenFile()
*!*************************************************************
*! Passed Parameters  : lcCusVen
*!*************************************************************
*! Returns            : ll2Return
*!*************************************************************
*! Example            :  =lffillcode()
*!*************************************************************

FUNCTION lffillcode
PARAMETER lcCusVen
PRIVATE ll2Return

ll2Return = .T.
IF !USED('CODES')
  =gfOpenFile(gcDataDir+'CODES','Ccode_no','SH')  && Key : CDEFCODE+CRLTFIELD+CFLD_NAME
ENDIF  

IF lcCusVen = 2
  IF !USED('APSETUP')
    =gfOpenFile(gcDataDir+'APSETUP','','SH')  && 
  ENDIF  

  IF !USED('APVENHST')
    =gfOpenFile(gcDataDir+'APVENHST','ACTHST','SH')  && 
  ENDIF  

  IF !USED('APVENDOR')
    =gfOpenFile(gcDataDir+'APVENDOR','Vencode','SH')
  ENDIF
  
  IF !USED('FISHD')
    =gfOpenFile(gcDataDir+'FISHD','','SH')  && 
  ENDIF  
 
  SELECT cFisfYear,cFisYStat,cFisNoPrd  ;
  FROM (gcDataDir + 'FISHD')  ;
   INTO ARRAY laFiscYear

  SELECT CODES
  *--- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
  lcDefTerm = IIF(SEEK("DCTERMCODE "+"N") ,Codes.cCode_No,"" )
  ll2Return = !EMPTY(lcDefTerm)
ELSE
  SELECT Codes
  ll2Return = SEEK("DREGION    "+"N") .AND.;
              SEEK("DCLASS     "+"N") .AND.;
              SEEK("DCTERMCODE "+"N") .AND.;
              SEEK("DSHIPVIA   "+"N") .AND.;
              SEEK("DSPCINST   "+"N")
ENDIF

RETURN ll2Return

*!*************************************************************
*! Name       : lfAddUsrDt
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 01/10/2002
*! Purpose    : Add user id ,date  and time to files.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfAddUsrDt()
*!*************************************************************

FUNCTION lfAddUsrDt
REPLACE cAdd_User WITH gcUser_ID,;
        dAdd_Date WITH gdSysDate,;
        cAdd_Time WITH Time()
        
*!*************************************************************
*! Name       : lfDefCod
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 01/10/2002
*! Purpose    : Get the default code
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed Parameters  : lcFldName
*!*************************************************************
*! Returns            : lcRet
*!*************************************************************
*! Example            :  =lfDefCod()
*!*************************************************************

FUNCTION lfDefCod
PARAMETERS lcFldName
PRIVATE lcAlias,lcOrder,lcRet
lcAlias = SELECT()
lcRet = ''
SELECT CODES
lcOrder = ORDER()
SET ORDER TO CCODE_NO 
IF SEEK('D'+UPPER(lcFldName),'CODES')
  lcRet = CODES.CCODE_NO + ' - ' + CODES.CDISCREP
ENDIF

SET ORDER TO &lcOrder
SELECT (lcAlias)
RETURN lcRet

*!*************************************************************
*! Name       : lfOpenFls
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 01/10/2002
*! Purpose    : Open files.
*!*************************************************************
*! Calls      : =gfOpenFile()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfOpenFls()
*!*************************************************************

FUNCTION lfOpenFls

= gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
= gfOpenFile(gcDataDir+'ARCUSHST','Acthst','SH')
= gfOpenFile(gcDataDir+'contact','Contact','SH')
= gfOpenFile(gcDataDir+'APSETUP','','SH')
= gfOpenFile(gcDataDir+'APVENDOR','Vencode','SH')
= gfOpenFile(gcDataDir+'APVENHST','Vendyear','SH')

*!*************************************************************
*! Name       : lfvSource
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 01/10/2002
*! Purpose    : Valid function of source object.
*!*************************************************************
*! Calls      : =GETFILE()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfvSource()
*!*************************************************************

FUNCTION lfvSource

lcSource = GETFILE("XLS","Select Excel source file.")
IF !(lcOSource == lcSource)
  WAIT "Validating entery.. Please wait" WINDOW NOWAIT
  lcStatus = IIF(EMPTY(lcSource),"DISABLE","ENABLE")
  SHOW GET pbConvert &lcStatus
  IF EMPTY(lcSource)
    lcSource = lcOSource
    _CUROBJ = _CUROBJ
  ENDIF  
  WAIT CLEAR 
  SHOW GET lcSource 
ENDIF

*!*************************************************************
*! Name       : lfDummy
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 01/10/2002
*! Purpose    : Funciton to update the thermometer.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfDummy()
*!*************************************************************

FUNCTION lfDummy

RETURN .F.

*!*************************************************************
*! Name       : lfClose
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 01/10/2002
*! Purpose    : Update Contact file.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfClose()
*!*************************************************************

FUNCTION lfClose

IF USED('APVENHST')
  USE IN APVENHST
ENDIF
IF USED('APSETUP')
  USE IN APSETUP
ENDIF
IF USED('APVENDOR')
  USE IN APVENDOR
ENDIF
IF USED('CUSTOMER')
  USE IN CUSTOMER
ENDIF
IF USED('CODES')
  USE IN CODES
ENDIF
IF USED('ARCUSHST')
  USE IN ARCUSHST
ENDIF
IF USED('CONTACT')
  USE IN CONTACT
ENDIF
IF USED (lcVendorFl)
  USE IN (lcVendorFl)
ENDIF
ERASE (gcWorkDir+lcVendorFl+".DBF")
ERASE (gcWorkDir+lcVendorFl+".FPT")

IF USED(lcCustFl)
  USE IN (lcCustFl)
ENDIF
ERASE (gcWorkDir+lcCustFl+".DBF")
ERASE (gcWorkDir+lcCustFl+".FPT")

IF USED(lcStorFl)
  USE IN (lcStorFl)
ENDIF
ERASE (gcWorkDir+lcStorFl+".DBF")
ERASE (gcWorkDir+lcStorFl+".FPT")

CLEAR READ

*!*************************************************************
*! Name       : lfConvert
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 01/10/2002
*! Purpose    : Conversion Function.
*!*************************************************************
*! Calls      : =lfOpenFls() , =lfCreTmp() , =lfImport()
*!              =lfRefresh()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfConvert()
*!*************************************************************

FUNCTION lfConvert

=lfOpenFls()
lcCurrFile = "Checking codes validity...Please wait."
=lfRefresh()
=lfCreTmp(ln2Conv)
=lfImport(ln2Conv)
SHOW GET pbConvert DISABLE
*!*************************************************************
*! Name       : lfCreTmp
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 01/10/2002
*! Purpose    : Update Contact file.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed Parameters  : lnvenCus
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfCreTmp()
*!*************************************************************

FUNCTION lfCreTmp
PARAMETER lnvenCus

IF lnvenCus = 2
  CREATE DBF (gcWorkDir+lcVendorFl) ;
             (cvendcode c(8),cvencomp c(30),caddress1 c(30),caddress2 c(30),;
             caddress3 c(30),caddress4 c(30),caddress5 c(30),cphoneno c(16),cFaxno c(16))
ELSE
  CREATE DBF (gcWorkDir+lcCustFl);
             (Account C(6) , btName C(30) ,caddress1 c(30),caddress2 c(30),;
             caddress3 c(30),caddress4 c(30),caddress5 c(30),phone1 C(16) ,Buyer C(30) ,Fax C(16),;
             Contact C(30) ,phone C(16) ,Faxc C(16) , Type C(1))

  CREATE DBF (gcWorkDir+lcStorFl);
             (Account C(6) , StName C(30) ,caddress1 c(30),caddress2 c(30),;
             caddress3 c(30),caddress4 c(30),caddress5 c(30),Store C(8),phone1 C(16),Type C(1))

ENDIF

*!*************************************************************
*! Name       : lfImport
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 01/10/2002
*! Purpose    : Update Contact file.
*!*************************************************************
*! Calls      : =gfModalGen() , =lfRefresh() , =lfUpdCust()
*!              =lfUpdCont()  , =lfUpdStor()
*!*************************************************************
*! Passed Parameters  : lnvenCus
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfImport()
*!*************************************************************

FUNCTION lfImport
PARAMETER lnCenCus

*--If code not setuped , warn the user
IF lFfillcode(lnCenCus)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
   'One or more codes have not been set up. Please set up and try again.')
  RETURN 
ENDIF

IF lnCenCus = 1
  lcDefaPrior = gfGetMemVar('M_DEFAPRIO')
  lcCurrFile = "Importing Customer."
  =lfRefresh()

  *--- Import Accounts
  IF EMPTY(lcSource) .AND. !EMPTY(lcSource1)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
     "You have to select the Account's excel file.")
    RETURN
  ENDIF
  IF !EMPTY(lcSource)
    SELECT (lcCustFl)
    ZAP
    Append from (ALLTRIM(lcSource)) TYPE XLS
    GO TOP
    DELETE 
    PACK
    REPLACE ALL Type WITH "M"
    INDEX ON ACCOUNT TAG (lcCustFl)
    FLUSH
  ENDIF
  *--- Import Accounts
  
  IF !EMPTY(lcSource1)
    *--- Import Stores
    SELECT (lcStorFl)
    ZAP
    Append from (ALLTRIM(lcSource1)) TYPE XLS
    GO TOP
    DELETE
    PACK
    INDEX ON ACCOUNT+STORE TAG (lcStorFl)
    REPLACE ALL Type WITH "S"
    FLUSH
    *--- Import Stores
  ENDIF
  *--- Update Main Accounts
  lcCurrFile = "Update Customer master file."
  =lfRefresh()
  SELECT (lcCustFl)
  lnRecNo = 0
  lnTotRe = RECCOUNT()
  SCAN
    lnRecNo = lnRecNo + 1
    *--- Parameter lcMessg , lnRecNo , lcFilNam
    =lfUpdFTher("Update Customer :"+Account,lnRecNo,"Customer",lnTotRe)
    SCATTER MEMVAR MEMO
    lcExlsAcct = m.Account
    *--- Update Customer
    =lfUpdCust()

    IF !EMPTY(Contact)
      =lfUpdCont()
    ENDIF
    *--- Update Store    
    IF !EMPTY(lcSource1)
      =lfUpdStor(lcExlsAcct)
    ENDIF
  ENDSCAN
ELSE
  SELECT (lcVendorFl)
  lcCurrFile = "Importing vendor...Please wait."
  =lfRefresh()
  IF !EMPTY(lcSource)
    Append from (ALLTRIM(lcSource)) TYPE XLS
    GO TOP
    DELETE 
    PACK
    lcCurrFile = "Update Vendor master file."
    =lfRefresh()
  ENDIF
  SELECT (lcVendorFl)
  lnRecNo = 0
  lnTotRe = RECCOUNT()
  SCAN
    lnRecNo = lnRecNo + 1
    *--- Parameter lcMessg , lnRecNo , lcFilNam
    =lfUpdFTher("Update vendor# "+cvendcode,lnRecNo,"Vendor",lnTotRe)
    SCATTER MEMVAR MEMO
    SELECT APVENDOR
    IF !SEEK(m.cvendcode)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE cAddRess6 WITH gcContCode       ;
              CVenPrior WITH '1'              ;
              cVenPMeth WITH 'P'              ;
              cTaxType  WITH APSETUP.cTaxType ;
              cCurrCode WITH gcBaseCurr       ;
              cTermCode WITH lcDefTerm        ;
              cdivision WITH lfDefCod("CDIVISION")         ,;
              caddress5 WITH PADL(ALLTRIM(m.caddress5),5,'0'),;
              cPhoneNo  WITH getPhone(cPhoneNo),;
              cFaxNo    WITH getPhone(cFaxNo)
      =gfAdd_Info('APVENDOR')      
        SELECT APVENHST
        FOR lnCounter = 1 TO ALEN(laFiscYear,1) 
          IF SUBSTR(laFiscYear[lnCounter,2],1) <> 'H'
            INSERT INTO &gcDataDir.APVENHST   ;
                      (cVendCode  ,cFisfYear) ;
                   VALUES                  ;
                   (m.cvendcode  ,laFiscYear[lnCounter,1])
            =gfAdd_Info('APVENHST')      
          ENDIF 
        ENDFOR
      ENDIF
      SELECT (lcVendorFl)
  ENDSCAN
ENDIF

*!****************************************************************
*! Name       : lfUpdFTher
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 01/10/2002
*! Purpose    : Funciton to update the thermometer.
*!****************************************************************
*! Calls      : =lfRefresh()
*!****************************************************************
*! Passed Parameters  : lcMessg , lnRecNo , lcFilNam , lnTotRecNo
*!****************************************************************
*! Returns            : None
*!****************************************************************
*! Example            :  =lfUpdFTher()
*!****************************************************************

FUNCTION lfUpdFTher
PARAMETER lcMessg , lnRecNo , lcFilNam , lnTotRecNo
PRIVATE lnPer
lnPer = (lnRecNo/lnTotRecNo) *100
lcPerSign  = SPACE(1) + "%" + SPACE(1)
IF lnPer > 0
  lcFTherm  = REPLICATE(CHR(219),lnRecNo*(lnThermSiz/lnTotRecNo))
ELSE
  lcFTherm  = ""
ENDIF
lcPersent  = ALLTRIM(STR(lnPer,6,2))+lcPerSign
SHOW GET lcFTherm
lcCurrFile = lcMessg
lcFPer = lcPersent
=lfRefresh()

*!****************************************************************
*! Name       : lfUpdFTher
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 01/10/2002
*! Purpose    : 
*!****************************************************************
*! Calls      : None
*!****************************************************************
*! Passed Parameters  : None
*!****************************************************************
*! Returns            : None
*!****************************************************************
*! Example            :  =lfUpdFTher()
*!****************************************************************

FUNCTION lfvTyp

lcSource =""
SHOW GET lcSource
lcSource1 =""
SHOW GET lcSource1

IF ln2Conv = 2
  SHOW GET pbSource1 DISABLE
ELSE
  SHOW GET pbSource1 ENABLE
ENDIF
SHOW GET pbConvert DISABLE

*!****************************************************************
*! Name       : lfUpdFTher
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 01/10/2002
*! Purpose    : valid function of Source1.
*!****************************************************************
*! Calls      : None
*!****************************************************************
*! Passed Parameters  : lcMessg , lnRecNo , lcFilNam , lnTotRecNo
*!****************************************************************
*! Returns            : None
*!****************************************************************
*! Example            :  =lfUpdFTher()
*!****************************************************************

FUNCTION lfvSource1

lcSource1 = GETFILE("XLS","Select Excel source file.")
IF !(lcOSource1 == lcSource1)
  WAIT "Validating entery.. Please wait" WINDOW NOWAIT
  lcStatus = IIF(EMPTY(lcSource1),"DISABLE","ENABLE")
  SHOW GET pbConvert &lcStatus
  IF EMPTY(lcSource1)
    lcSource1 = lcOSource1
    _CUROBJ = _CUROBJ
  ENDIF  
  WAIT CLEAR 
  SHOW GET lcSource1
ENDIF

*!*************************************************************
*! Name       : lfGetCus
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 01/10/2002
*! Purpose    : Validate Customer code.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed Parameters  : lcAcct
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfGetCus()
*!*************************************************************

FUNCTION lfGetCus
PARAMETER lcAcct

PRIVATE lnCurAls , lnIndex , lcIndex , lcTmpAct
lnCurAls = SELECT(0)
lnIndex = 1
lcIndex = ALLTRIM(PADL(ALLTRIM(STR(lnIndex,(5-LEN(lcAcct)))),(5-LEN(lcAcct)),"0"))
lcTmpAct = lcAcct+lcIndex
SELECT Customer
llFound = SEEK("M"+lcTmpAct)
DO WHILE llFound
  lnIndex = lnIndex + 1
  lcIndex = ALLTRIM(PADL(ALLTRIM(STR(lnIndex,(5-LEN(lcAcct)))),(5-LEN(lcAcct)),"0"))
  llFound = SEEK("M"+lcAcct+lcIndex)
ENDDO
lcTmpAct = lcAcct+lcIndex
SELECT(lnCurAls)
RETURN(lcTmpAct)

*!*************************************************************
*! Name       : lfUpdCust
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 01/10/2002
*! Purpose    : Update Customer file.
*!*************************************************************
*! Calls      : =lfGetCus() , =gfAdd_Info()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfUpdCust()
*!*************************************************************

FUNCTION lfUpdCust

*--Account field validation, it should be 5 digits long.
m.Account = lfGetCus(SUBSTR(ALLTRIM(m.Account),1,3))

SELECT CUSTOMER
IF !SEEK("M"+m.Account)
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE BillTo     WITH "M",;
          Status     WITH "P",;
          Link_Code  WITH "DEFDEF",;
          Consol     WITH "N",;
          Priority   WITH lcDefaPrior,;
          Region     WITH lfDefCod("Region"),;
          Class      WITH lfDefCod("Class"),;
          CTermCode  WITH lfDefCod("CTermCode"),;
          ShipVia    WITH lfDefCod("ShipVia"),;
          SPCINST    WITH lfDefCod("SPCINST"),;
          cInsur     WITH "Y",;
          PriceLvl   WITH "A",;
          SKUTMPL    WITH "DEF",;
          cslsglLink WITH "DEF",;
          Prnt_Statm WITH "Y",;
          cTaxRule   WITH "1",;
          lvatexem   WITH .F.,;
          Ccont_Code WITH gcContCode,;
          Caddress6  WITH gcContCode,;
          cCont_Cod2 WITH gcContCode,;
          cCurrCode  WITH gcBaseCurr,;
          cAddress5  WITH PADL(ALLTRIM(m.cAddress5),5,'0'),;
          Caddress12 WITH Caddress1,;
          Caddress22 WITH Caddress2,;
          Caddress32 WITH Caddress3,;
          Caddress42 WITH Caddress4,;
          Caddress52 WITH Caddress5,;
          Caddress62 WITH Caddress6,;
          Phone1     WITH getPhone(Phone1),;
          Phone2     WITH getPhone(Phone2),;
          Fax        WITH getPhone(Fax)
  =gfAdd_Info('Customer')

  *-- Update ARCUSHST file
  IF m.Type ='M' .AND. !SEEK(m.Account,'ARCUSHST')
    INSERT INTO ARCUSHST (Account,cFisFYear) VALUES (m.Account,STR(VAL(gcCurrYear)-1,4))
    INSERT INTO ARCUSHST (Account,cFisFYear) VALUES (m.Account,gcCurrYear)
    INSERT INTO ARCUSHST (Account,cFisFYear) VALUES (m.Account,STR(VAL(gcCurrYear)+1,4))
  ENDIF
ENDIF

*!*************************************************************
*! Name       : lfUpdStor
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 01/10/2002
*! Purpose    : Update Contact file.
*!*************************************************************
*! Calls      : =gfModalGen() , =gfAdd_Info()
*!*************************************************************
*! Passed Parameters  : lcExlAcct
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfUpdStor()
*!*************************************************************

FUNCTION lfUpdStor
PARAMETER lcExlAcct

SELECT (lcStorFl)
SET FILTER TO ALLTRIM(Account) == ALLTRIM(lcExlAcct)
LOCATE
lnStrInd = 0
SCAN
  lcNewAcct = m.Account
  SCATTER MEMVAR MEMO  
  m.Account = lcNewAcct
  IF EMPTY(m.Store)
    lnStrInd = lnStrInd + 1
    m.Store  = PADL(lnStrInd,2,"0")
  ENDIF

  IF !SEEK("M"+m.Account,'Customer')
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Main Account record does not exist for Account+store.'+m.Account+" "+m.Store)
  ENDIF
  
  IF SEEK("S"+m.Account+m.Store,'Customer')
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Account+store.'+m.Account+" "+m.Store+" already exist.")
  ENDIF
  
  =lfUpdFTher("Update Customer + Store :"+m.Account+" "+m.Store,lnRecNo,"Customer",lnTotRe)
  SELECT CUSTOMER
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE BillTo     WITH "M",;
          Status     WITH "P",;
          Link_Code  WITH "DEFDEF",;
          Consol     WITH "N",;
          Priority   WITH lcDefaPrior,;
          Region     WITH lfDefCod("Region"),;
          Class      WITH lfDefCod("Class"),;
          CTermCode  WITH lfDefCod("CTermCode"),;
          ShipVia    WITH lfDefCod("ShipVia"),;
          SPCINST    WITH lfDefCod("SPCINST"),;
          cInsur     WITH "Y",;
          PriceLvl   WITH "A",;
          SKUTMPL    WITH "DEF",;
          cslsglLink WITH "DEF",;
          Prnt_Statm WITH "Y",;
          cTaxRule   WITH "1",;
          lvatexem   WITH .F.,;
          Ccont_Code WITH gcContCode,;
          Caddress6  WITH gcContCode,;
          cCont_Cod2 WITH gcContCode,;
          cCurrCode  WITH gcBaseCurr,;
          cAddress5  WITH PADL(ALLTRIM(m.cAddress5),5,'0'),;
          Caddress12 WITH Caddress1,;
          Caddress22 WITH Caddress2,;
          Caddress32 WITH Caddress3,;
          Caddress42 WITH Caddress4,;
          Caddress52 WITH Caddress5,;
          Caddress62 WITH Caddress6,;
          Phone1     WITH getPhone(Phone1),;
          Phone2     WITH getPhone(Phone2),;
          Fax        WITH getPhone(Fax)

  =gfAdd_Info('Customer')
ENDSCAN
SET FILTER TO

*!*************************************************************
*! Name       : lfUpdCont
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 01/10/2002
*! Purpose    : Update Contact file.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfUpdCont()
*!*************************************************************

FUNCTION lfUpdCont

lcAlias = SELECT()
SELECT CONTACT

IF !SEEK("C"+m.Account,'CONTACT')
  LOCATE REST WHILE cconttype+ccont_id+store+contact = "C"+m.Account
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE cContType WITH "C" ,;
          cCont_id  WITH m.Account ,;
          Fax       WITH getPhone(m.Faxc),;
          Phone     WITH getPhone(Phone)
ENDIF
SELECT (lcAlias)




FUNCTION getPhone
PARAMETER lcPhone
lcRet = ""
FOR I = 1 to LEN(lcPhone)
  lcRet = lcRet + getChar(SUBSTR(lcPhone,I,1))
ENDFOR
RETURN  lcRet 

FUNCTION getChar
PARAMETERS lcChar
IF !(BETWEEN(ASC(lcChar),65,90) OR BETWEEN(ASC(lcChar),97,122) OR BETWEEN(ASC(lcChar),48,57))
 lcChar = ""
ENDIF
RETURN lcChar