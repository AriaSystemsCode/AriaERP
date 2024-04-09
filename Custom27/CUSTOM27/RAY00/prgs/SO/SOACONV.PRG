*:**************************************************************************
*: PROGRAM   : SOACONV.PRG                FOR : AIMS
*: MODULE    : Aria Apparel Series.
*: DATE      : 02/10/2001
*: DESC.     : Convert customer data file .
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: Refer to  : (C102189)
*:**************************************************************************
*: Calls : 
*:         FUNCTION  : lfBrow()
*:                   : lfRefresh()
*:                   : lfvOkBut()
*:					 : lfOpenFile()
*:					 : lfConvert()
*:					 : lfAddAct()
*:					 : lfAddCont()
*:					 : lfAddNotes() 
*:**************************************************************************
*: Modifications:
*: B604382,1 ADEL 04/10/2001 Fix "Account.cdx not found." error message.
*:**************************************************************************

*---Initialize needed variables.
*-- lcDirect     && Holds the selected data directory by the user.
*-- lcOkBMP      && Holds the OK buttom bmp.
*-- lcCanclBMP   && Holds the CANCEL buttom bmp.
*-- lnNoOfCov    && Holds the no of the converted records.
*-- lnNoOfNon    && Holds the no of the unconverted records.
*-- llAddRec     && Will we add a new account?
STORE ' ' TO lcDirect
lcOkBmp    = gcBMPHome + 'OK.BMP'
lcCanclBmp = gcBMPHome + 'CAN.BMP'
*--P/O Setup.
llImpUse   = gfGetMemVar('M_LImpCost')
STORE .F. TO llError,llOpen
STORE .T. TO llOvWrt
lcFilHandl = ""
IF !FILE(gcWorkDir+'ConvLog.MEM')
  lcLogvFile = '&gcWorkDir.ConvLog.txt'
  SAVE ALL LIKE lcLogvFile* TO (gcWorkDir+'ConvLog.MEM')
ELSE
  RESTORE FROM (gcWorkDir+'ConvLog.MEM') ADDI
ENDIF
ckLog = 1
STORE 0 TO lnNoOfCov,lnNoOfNon
*---Call the directory screen.
DO (gcScrDir+gcWinAppl+"\SOACONV.SPR")


*!*************************************************************
*! Name      : lfBrow
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/10/2001
*! Purpose   : Validate the browse bitton.
*!*************************************************************
FUNCTION lfBrow

lcDirect = GETDIR()
IF !EMPTY(lcDirect)
  SHOW GET pbOkBut ENABLE
ELSE
  SHOW GET pbOkBut DISABLE
ENDIF
=lfRefresh()

*!*************************************************************
*! Name      : lfvOkBut
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/10/2001
*! Purpose   : Validate the OK bitton.
*!*************************************************************
FUNCTION lfvOkBut

*-- False this variable to check the current directory.
*-- Initialize no of converted and unconverted records.
STORE 0 TO lnNoOfCov,lnNoOfNon
*--Open files.
IF !lfOpenFile()
  RETURN
ENDIF
*--Start converting
=lfUpdError()
=lfConvert()
=FCLOSE(lcFilHandl)
llOpen = .F.
IF llError
  =FFLUSH(lcFilHandl)
  CREATE CURSOR TMPSTR (mStrRep M(10))
  APPEND BLANK
  APPEND MEMO mStrRep FROM (lcLogvFile) OVERWRITE
  lcWinTitl = 'Conversion Log Report'
  DO (gcScrDir + gcWinAppl + '\SOSTRREP.SPX')
  USE IN TMPSTR
ENDIF  

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/10/2001
*! Purpose   : Open files.
*!*************************************************************
FUNCTION lfOpenFile

*--Open convert files.
IF !FILE(lcDirect+'CUSTOMER.DBF')
    *** "File 'CUSTOMER.dbf' does not exist in the current directory." ***
    *** < Ok > ***
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"File 'CUSTOMER.dbf' does not exist in the current directory.")    
    RETURN .F.
ENDIF
*B604382,1 (Begin) Remark the following lines.
*IF !FILE(lcDirect+'CUSTOMER.CDX')
*    *** "File 'CUSTOMER.CDX' does not exist in the current directory." ***
*    *** < Ok > ***
*    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"File 'ACCOUNT.ndx' does not exist in the current directory.")    
*    RETURN .F.
*ENDIF
*B604382,1 (End)
IF !FILE(lcDirect+'CODE.DBF')
    *** "File 'CODE.dbf' does not exist in the current directory." ***
    *** < Ok > ***
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"File 'CODE.dbf' does not exist in the current directory.")    
    RETURN .F.
ENDIF
IF !FILE(lcDirect+'CONTROL.DBF')
    *** "File 'CONTROL.dbf' does not exist in the current directory." ***
    *** < Ok > ***
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"File 'CONTROL.dbf' does not exist in the current directory.")    
    RETURN .F.
ENDIF
*--Open master files.
=gfOpenFile(gcDataDir+'Customer',gcDataDir+'Customer','SH')
=gfOpenFile(gcDataDir+'Codes',gcDataDir+'CCODE_NO','SH')
=gfOpenFile(gcDataDir+'ARCUSHST',gcDataDir+'ACTHST','SH')

*!*************************************************************
*! Name      : lfConvert
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/10/2001
*! Purpose   : Concert data.
*!*************************************************************
FUNCTION lfConvert

*--Convert Codes File
=lfUpdCods()
=lfUpdCust()

*!*************************************************************
*! Name      : lfUpdCods
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/10/2001
*! Purpose   : Update Codes File.
*!*************************************************************
FUNCTION lfUpdCods

STORE 0 TO lnNoOfCov,lnNoOfNon
*B604382,1 (Begin) Avoid any error messages coming from unsuccessful Running.
IF USED('OLDCODE')
  USE IN OLDCODE
ENDIF
*B604382,1 (End)
USE lcDirect+'CODE.DBF' IN 0 ALIAS OLDCODE
*--Setups effect in releted fields for the following codes
*--(Credit and debit adj. ,Division ,ShipVia ,State).
llApInst  = ('AP' $ gcComp_mdl)
lcSCountry= UPPER(ALLTRIM(gcContCode))
lcOldCdTyp = ' '
SELECT OLDCODE
GOTO TOP
lnMax1 = RECCOUNT()
=FPUTS(lcFilHandl,REPLICATE('*',68))
=FPUTS(lcFilHandl,"*                    CODES CONVERSION REPORT                    *")
=FPUTS(lcFilHandl,REPLICATE('*',68))
=FPUTS(lcFilHandl,'START PROCESSING     '+DTOC(DATE())+SPACE(5)+TIME())
SET ORDER TO CODES IN CODES
SCAN FOR !(SUBSTR(CODE,1,1) $ 'DUF')
  WAIT WINDOW 'Converting Code file.' NOWAIT
  lcType = SUBSTR(CODE,1,1)
  SCATTER MEMVAR
  DO CASE
    CASE lcType = 'A'
      lcNewType = 'B'
    CASE lcType = 'B'
      lcNewType = 'E'
    CASE lcType = 'C'
      lcNewType = 'L'
    CASE lcType = 'E'
      lcNewType = 'A'
    CASE lcType = 'I'
      lcNewType = 'S'
    CASE lcType = 'W'
      lcNewType = 'A'
    OTHERWISE
      lcNewType = lcType 
  ENDCASE
  lcCode = lcNewType +SUBSTR(CODE,2,2)
  SELECT CODES
  IF !SEEK('N'+lcCode )
    APPEND BLANK
    lnNoOfCov = lnNoOfCov + 1
  ELSE
    lnNoOfNon = lnNoOfNon + 1
  ENDIF  
  =RLOCK()
  REPLACE CDEFCODE  WITH 'N'   ,;
          CCODE_NO  WITH lcCode ,;
          CFLD_NAME WITH lfGetFName(SUBSTR(lcCode,1,1)),;
          CDISCREP  WITH IIF(EMPTY(m.cData),lcCode,m.cData),;
          CRLTFIELD WITH 'N',;
          CRLTD_NAM WITH '',;
          CRLTD_TYP WITH '',;
          CRLTD_VLU WITH '',;
          CADD_USER WITH gcUser_id,;
          DADD_DATE WITH gdSysDate,;
          CADD_TIME WITH TIME()
  SELE OLDCODE
ENDSCAN  
SELECT CODES
SET ORDER TO CCODE_NO
*--Update new code (CUPS)
DIME laUps[13,2]
laUps = '' 
laUps[1,1]  ='USUPSG' 
laUps[1,2]  ='U.S.A. UPS Ground            '
laUps[2,1]  ='USUPST' 
laUps[2,2]  ='U.S.A. UPS Next Day Air saver'
laUps[3,1]  ='USUPS3' 
laUps[3,2]  ='U.S.A. UPS 3 Day Select      '
laUps[4,1]  ='USUPS2' 
laUps[4,2]  ='U.S.A. UPS 2nd Day Air       '
laUps[5,1]  ='USUPSN' 
laUps[5,2]  ='U.S.A. UPS Next Day Air      '
laUps[6,1]  ='USUPSE' 
laUps[6,2]  ='U.S.A. UPS 2nd Day Air A.M.  '
laUps[7,1]  ='CAXPR '
laUps[7,2]  ='Canada Xpresspost Service    '
laUps[8,1]  ='CACOM '
laUps[8,2]  ='Canada Commercial Parcel     '
laUps[9,1]  ='CAEXPR' 
laUps[9,2]  ='Express Service Within Canada  '
laUps[10,1] ='CAEXPD'
laUps[10,2] ='Expedited Service Within Canada'
laUps[11,1] ='CASTAN'
laUps[11,2] ='Standard Service Within Canada '
laUps[12,1] ='CAECON'
laUps[12,2] ='Economy Service Within Canada  '
laUps[13,1] ='OTHER '
laUps[13,2] ='Other'
IF !SEEK('NCUPS      ')
  FOR lnI=1 TO 14
    SELECT CODES
    APPEND BLANK
    lnNoOfCov = lnNoOfCov + 1
    REPLACE CDEFCODE  WITH IIF(lnI=14,'D','N'),;
            CFLD_NAME WITH 'CUPS      ',;
            CCODE_NO  WITH IIF(lnI=14,laUps[1,1],laUps[lnI,1]),;
            CDISCREP  WITH IIF(lnI=14,'',laUps[lnI,2]),;
            CRLTFIELD WITH 'N',;
            cadd_user  WITH gcUser_ID,;
            dadd_date  WITH DATE(),;
            cadd_time  WITH gfGetTime()
  ENDFOR
ENDIF  
*--Update code file for default manufacturing operation use for imported styles
*--if no detail costing setup.
IF !llImpUse AND !SEEK('NMFGCODE')
  lcILlbl1 = gfGetMemVar('M_CICOST1')
  lcILlbl2 = gfGetMemVar('M_CICOST2')
  lcILlbl3 = gfGetMemVar('M_CICOST3')
  lcILlbl4 = gfGetMemVar('M_CICOST4')
  lcILlbl5 = gfGetMemVar('M_CICOST5')
  SELECT CODES
  FOR lnI=1 TO 5
    lcZ=STR(lnI,1)
    APPEND BLANK
    lnNoOfCov = lnNoOfCov + 1
    REPLACE CDEFCODE  WITH 'N',;
            CFLD_NAME WITH 'MFGCODE',;
            CCODE_NO  WITH '*'+lcZ  ,;
            CDISCREP  WITH lcILlbl&lcZ,;
            CRLTFIELD WITH 'N',;
            cadd_user  WITH gcUser_ID,;
            dadd_date  WITH DATE(),;
            cadd_time  WITH gfGetTime()
  ENDFOR 
ENDIF
USE lcDirect+'CONTROL.DBF' IN 0
*--Update Divisions
SELECT CODES
lcAllDiv = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
FOR ln = 1 TO LEN(lcAllDiv)
  lcFldName = 'CONTROL.DIVISION'+SUBSTR(lcAllDiv,ln,1)
  lcRldFld  = 'CONTROL.DIVNAME'+SUBSTR(lcAllDiv,ln,1)
  IF !EMPTY(&lcFldName)
    IF SEEK('NCDIVISION '+'D'+SUBSTR(lcAllDiv,ln,1))
      lnNoOfNon = lnNoOfNon + 1
    ENDIF
  ENDIF
  IF !EMPTY(&lcFldName) AND !SEEK('NCDIVISION '+'D'+SUBSTR(lcAllDiv,ln,1))
    APPEN BLANK
    = RLOCK()
    REPLACE cdefcode   WITH 'N',;
            cfld_name  WITH 'CDIVISION',;
            ccode_no   WITH 'D'+SUBSTR(lcAllDiv,ln,1),;
            cdiscrep   WITH &lcFldName,;
            crltfield  WITH 'N',;
            cadd_user  WITH gcUser_ID,;
            dadd_date  WITH DATE(),;
            cadd_time  WITH gfGetTime()
    UNLOCK
    APPEN BLANK
    = RLOCK()
    REPLACE cdefcode   WITH 'N',;
            cfld_name  WITH 'CDIVISION',;
            ccode_no   WITH 'D'+SUBSTR(lcAllDiv,ln,1),;
            cdiscrep   WITH '',;
            crltfield  WITH 'Y',;
            cRltd_Nam  WITH 'DIVLNAME  ',;
            cRltd_Typ  WITH 'C',;
            cRltd_Vlu  WITH &lcRldFld,;
            cadd_user  WITH gcUser_ID,;
            dadd_date  WITH DATE(),;
            cadd_time  WITH gfGetTime()
  ENDIF
ENDFOR
*--Close files
USE IN OLDCODE
USE IN CONTROL
lcMsg1  = "No. of records added is  " + ALLTRIM(STR(lnNoOfCov,7)) + " And No. of existing records is " + ALLTRIM(STR(lnNoOfNon,7))
=lfUpdError(lcMsg1)
=FPUTS(lcFilHandl,'END PROCESSING       '+DTOC(DATE())+SPACE(5)+TIME())


*!*************************************************************
*! Name      : lfUpdCust
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/10/2001
*! Purpose   : Update Customer File.
*!*************************************************************
FUNCTION lfUpdCust

STORE 0 TO lnNoOfCov,lnNoOfNon
*--Open Salesrep file
=gfOpenFile(gcDataDir+'Salesrep',gcDataDir+'Salesrep','SH')
*-Open Old Customer file.
DISTRO =""
SELECT 0
*B604382,1 (Begin) Avoid any error messages coming from unsuccessful Running.
IF USED('OLDCUST')
  USE IN OLDCUST
ENDIF
*B604382,1 (End)
USE lcDirect+'CUSTOMER.DBF' ALIAS OLDCUST
*B604382,1 (Begin)
*INDEX ON TYPE+ACCOUNT+STORE TAG TMPACT
INDEX ON TYPE+ACCOUNT+STORE TAG TMPACT OF (gcWorkDir+'CUSTOMER.CDX')
*B604382,1 (End)
=FPUTS(lcFilHandl,REPLICATE('*',68))
=FPUTS(lcFilHandl,"*                    CUSTOMER CONVERSION REPORT                    *")
=FPUTS(lcFilHandl,REPLICATE('*',68))
=FPUTS(lcFilHandl,'START PROCESSING     '+DTOC(DATE())+SPACE(5)+TIME())
*--Update Main Accounts first.
=SEEK('1')
SCAN REST WHILE type+account = "1" FOR LEN(ALLTRIM(Account)) = 5 AND type <> 'F' 
  WAIT WINDOW 'Converting data for : ' + Account  NOWAIT
  *--Flag to see if it's ok to add a new customer.
  IF SEEK('M'+OLDCUST.Account,'CUSTOMER')
    llError = .T.
    lcMsg1  = "Account : " + OLDCUST.Account + " already exits."
    =lfUpdError(lcMsg1)
    lnNoOfNon = lnNoOfNon +1
  ELSE
    lnNoOfCov = lnNoOfCov + 1
    *--Add a new account.
    =lfAddAct(.T.)
  ENDIF  
ENDSCAN
*--Update Main Stores.
=SEEK('2')
SCAN REST WHILE type+account = "2" FOR LEN(ALLTRIM(Account)) = 5 AND type <> 'F' 
  WAIT WINDOW 'Converting data for : ' + Account  + ' Store : '+ Store NOWAIT
  *--Flag to see if it's ok to add a new store.
  IF TYPE = '2' AND !SEEK('M'+OLDCUST.Account,'CUSTOMER')
    llError = .T.
    lcMsg1  = "Store : " + OLDCUST.Store + "for account : " + OLDCUST.Account  + " not updated as it has no main account."
    =lfUpdError(lcMsg1)
    *--Increase the no of unconverted records.
    lnNoOfNon = lnNoOfNon +1
  ELSE
    IF SEEK('S'+OLDCUST.Account+OLDCUST.Store,'CUSTOMER')
      llError = .T.
      lcMsg1  = "Store : " + OLDCUST.Store + " already exits for account : " + OLDCUST.Account
      =lfUpdError(lcMsg1)
      *--Increase the no of unconverted records.
      lnNoOfNon = lnNoOfNon +1
    ELSE
      *--Will we careate a new store?
      lnNoOfCov = lnNoOfCov + 1
      *--Add a new store.
      =lfAddAct(.F.)
    ENDIF  
  ENDIF  
ENDSCAN
lcMsg1  = "No. of records added is  " + ALLTRIM(STR(lnNoOfCov,7)) + " And No. of records not added is " + ALLTRIM(STR(lnNoOfNon,7))
=lfUpdError(lcMsg1)
=FPUTS(lcFilHandl,'END PROCESSING       '+DTOC(DATE())+SPACE(5)+TIME())
*--Close files
USE IN OLDCUST
CLOSE INDEX
*B604382,1 (Begin)
ERASE gcWorkDir+'CUSTOMER.CDX'
*B604382,1 (End)


*!*************************************************************
*! Name      : lfAddAct
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/10/2001
*! Purpose   : Add a new account.
*!*************************************************************
FUNCTION lfAddAct
PARAMETERS llMain

=RLOCK()
llSndMain = OLDCUST.BILLADDR='M'.OR. EMPTY(OLDCUST.BILLADDR)
IF !llMaIn AND llSndMain
  SELECT OLDCUST
  lcKey = EVAL(KEY())
ELSE
  IF !SEEK(OLDCUST.Account,'arCusHst')
    INSERT INTO arCusHst (Account,cFisFYear) VALUES (OLDCUST.Account,STR(VAL(gcCurrYear)-1,4))
    INSERT INTO arCusHst (Account,cFisFYear) VALUES (OLDCUST.Account,gcCurrYear)
    INSERT INTO arCusHst (Account,cFisFYear) VALUES (OLDCUST.Account,STR(VAL(gcCurrYear)+1,4))
  ENDIF
ENDIF  
SELECT CUSTOMER
*--Get a new account code
APPEN BLANK
REPLACE TYPE       WITH IIF(llMain,'M','S') ,;
        BillTo     WITH 'M' ,;
        Account    WITH OLDCUST.Account ,;
        Store      WITH OLDCUST.Store ,;
        STATUS     WITH OLDCUST.Status,;
        Link_Code  WITH 'DEFDEF',;
        BtName     WITH OLDCUST.Name,;
        StName     WITH OLDCUST.Name,;
        Consol     WITH 'Y',;
        Phone1     WITH ALLTRIM(OLDCUST.Phone),;
        SalesRep   WITH OLDCUST.Salesrep,;
        Comm       WITH OLDCUST.COMMPCNT,;
        Note       WITH OLDCUST.Note,;
        Priority   WITH OLDCUST.Priority,;
        Region     WITH OLDCUST.MktRegion,;
        Class      WITH OLDCUST.class,;
        cTermCode  WITH OLDCUST.terms,;
        ShipVia    WITH OLDCUST.Shipvia,;
        cDivision  WITH OLDCUST.Division,;
        SPCINST    WITH OLDCUST.SPCINST,;
        Pricelvl   WITH 'A',;
        CINSUR     WITH OLDCUST.insurcd,;
        SKUTMPL    WITH 'DEF',;
        PRNT_STATM WITH 'N',;
        cCont_Code WITH 'USA',;
        CCONT_COD2 WITH 'USA',;
        CCURRCODE  WITH 'USD',;
        cAddress1  WITH OLDCUST.ADDR1,;
        cAddress2  WITH OLDCUST.ADDR2,;
        cAddress3  WITH OLDCUST.CITY,;
        cAddress4  WITH OLDCUST.STATE,;
        cAddress5  WITH OLDCUST.ZIP,;
        cAddress6  WITH OLDCUST.COUNTRY,;
        cAddress12 WITH OLDCUST.ADDR1,;
        cAddress22 WITH OLDCUST.ADDR2,;
        cAddress32 WITH OLDCUST.CITY,;
        cAddress42 WITH OLDCUST.STATE,;
        cAddress52 WITH OLDCUST.ZIP,;
        cAddress62 WITH OLDCUST.COUNTRY,;
        cSlsgllink WITH 'DEF',;
        cAdd_User  WITH gcUser_id,;
        cAdd_Time  WITH TIME(),;
        dAdd_Date  WITH gdSysDate

lcNum = '0123456789'
lcPhone=''
FOR I = 1 TO LEN(ALLTRIM(OLDCUST.PHONE))
   IF SUBSTR(OLDCUST.PHONE,I,1) $ lcNum
     lcPhone = lcPhone + SUBSTR(OLDCUST.PHONE,I,1)
   ENDIF
ENDFOR
IF !llMaIn AND llSndMain
  SELECT OLDCUST
  *--Get the main account.
  *lcSeek = account+store+'1'
  lcSeek = 'M'+account
  =SEEK(lcSeek)
  SELECT CUSTOMER
ENDIF
REPLACE CFACCODE   WITH OLDCUST.FACTOR,;
        FACTACCT   WITH OLDCUST.FACTACCT,;
        PHONE1     WITH lcPhone,;
        UPSZONE WITH OLDCUST.UPSZONE,;
        BUYER   WITH OLDCUST.BUYER,;
        KEEPER  WITH OLDCUST.KEEPER,;
        NOTE    WITH OLDCUST.NOTE,;
        BtName     WITH OLDCUST.Name,;
        cAddress12 WITH OLDCUST.ADDR1,;
        cAddress22 WITH OLDCUST.ADDR2,;
        cAddress32 WITH OLDCUST.CITY,;
        cAddress42 WITH OLDCUST.STATE,;
        cAddress52 WITH OLDCUST.ZIP,;
        cAddress62 WITH OLDCUST.COUNTRY
IF !llMaIn AND llSndMain
  SELECT OLDCUST
  =SEEK(lcKey)
  SELECT CUSTOMER
ENDIF

REPLACE CDIVISION  WITH OLDCUST.DIVISION,;
        COUNTRY    WITH OLDCUST.COUNTRY,;
        DUNS       WITH OLDCUST.DUNS,;
        DUNSRTG    WITH OLDCUST.DUNSRATE,;
        UPS_INCR   WITH OLDCUST.UPSPLUS,;
        PRNT_STATM WITH OLDCUST.STMT,;
        DISC       WITH OLDCUST.DISCPCNT,;
        AGE30      WITH OLDCUST.AGE30,;
        AGE60      WITH OLDCUST.AGE60,;
        AGE90      WITH OLDCUST.AGE90,;
        AGE120     WITH OLDCUST.AGE120,;
        TOTAGE     WITH OLDCUST.TOTAGE,;
        TERAGE30   WITH OLDCUST.TAGE30,;
        TERAGE60   WITH OLDCUST.TAGE60,;
        TERAGE90   WITH OLDCUST.TAGE90,;
        TERAGE120  WITH OLDCUST.TAGE120,;
        AGEDATE    WITH OLDCUST.AGEDATE,;
        CRLIMIT    WITH OLDCUST.CRLIMIT,;
        cravail    WITH OLDCUST.cravail,;
        BillTo     WITH OLDCUST.BILLADDR
UNLOCK

*!*************************************************************
*! Name      : lfUpdError
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Save the occurs errors.
*!*************************************************************
FUNCTION lfUpdError
PARAMETER lcMeg

IF !llOpen
  llOpen = .T.
  IF !EMPTY(lcLogvFile)
    IF llOvWrt
      lcFilHandl = FCREATE(lcLogvFile,0)
    ELSE  
      IF !FILE(lcLogvFile)
        lcFilHandl = FCREATE(lcLogvFile,0)
      ELSE  
        lcFilHandl = FOPEN(lcLogvFile,2)
        =FSEEK(lcFilHandl,0,2)
      ENDIF
    ENDIF  
  ENDIF
ENDIF
IF !EMPTY(lcMeg)
  =FPUTS(lcFilHandl,lcMeg)
ENDIF

*!*************************************************************
*! Name      : lfGetFName
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/10/2001
*! Purpose   : Get code's field name.
*!*************************************************************
FUNCTION lfGetFName
PARAMETER lcCodeTyp

PRIVATE lcFldName
lcFldName = ""
DO CASE
  CASE lcCodeTyp='A'
    lcFldName ='REASON'
  CASE lcCodeTyp='B' OR lcCodeTyp='E' 
    lcFldName = IIF(lcCodeTyp='B','TRANCODE','CCREDITCOD')
  CASE lcCodeTyp='C'
    lcFldName='COLOR'
  CASE lcCodeTyp='D'
    lcFldName='CDIVISION'
  CASE lcCodeTyp='G'
    lcFldName='CSTYGROUP'
  CASE lcCodeTyp='H'
    lcFldName='BANK'
  CASE lcCodeTyp='I'
    lcFldName='DECL_CODE'
  CASE lcCodeTyp='J'  
    lcFldName='ITEM_TYPE'
  CASE lcCodeTyp='K'  
    lcFldName='STATE'
  CASE lcCodeTyp='L'    
    lcFldName='CLASS'
  CASE lcCodeTyp='M'    
    lcFldName='MFGCODE'
  CASE lcCodeTyp='N'    
    lcFldName='CCHRGCODE'
  CASE lcCodeTyp='O'    
    lcFldName='CCANCRESON'
  CASE lcCodeTyp='R'    
    lcFldName='REGION'
  CASE lcCodeTyp='S'    
    lcFldName='SPCINST'
  CASE lcCodeTyp='T'    
    lcFldName='CTERMCODE'
  CASE lcCodeTyp='V'    
    lcFldName='SHIPVIA'
  CASE lcCodeTyp='Z'    
    lcFldName='SEASON'
  CASE lcCodeTyp='P'    
    lcFldName='CPRO_CODE'
  CASE lcCodeTyp='Y'    
    lcFldName='CRESO_CODE'
ENDCASE
RETURN lcFldName

*!*************************************************************
*! Name      : lfvGetName
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/10/2001
*! Purpose   : Get TEXT FILE  name.
*!*************************************************************
FUNCTION lfvGetName

lcFileN = lcLogvFile
llOvWrt = .T.
DO (gcScrDir + gcWinAppl + '\SOCONLG.SPX')
ckLog = 1
SHOW GET ckLog

FUNCTION lfvGetFile

lcName  = GETFILE("TXT","Select file for Log report","Select",0) && Save the Curr.
lcFileN = IIF(EMPTY(lcName),lcLogvFile,lcName)           && Get New File Name
SHOW GETS

*!*************************************************************
*! Name      : lfWhenLog
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/10/2001
*! Purpose   : When function for Log screen.
*!*************************************************************
FUNCTION lfWhenLog

_CUROBJ = OBJNUM(lcFileN)

*!*************************************************************
*! Name      : lfvLogOk
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/10/2001
*! Purpose   : Valid function of OK .
*!*************************************************************
FUNCTION lfvLogOk

IF !EMPTY(lcFileN)
  IF FILE(lcFileN)        && check if the file name is exist 
    lnOption = gfModalGen("QRM00048B00010","DIALOGE")  && Display warrning msg.
    DO CASE
      CASE lnOption = 1      && user select OverWrite the file option
        lcLogvFile = ALLTRIM(lcFileN)
        llOvWrt    = .T.
      CASE lnOption = 2       && user select Append to the file option
        lcLogvFile = ALLTRIM(lcFileN)
        llOvWrt = .F.
      OTHERWISE    && user Cancel
        _CUROBJ = OBJNUM(lcFileN)
        RETURN
    ENDCASE
  ELSE              &&  if file does not exist
    IF OCCURS("\",lcFileN)>0   
      IF !gfValdPath(SUBSTR(lcFileN,1,RAT("\",lcFileN)-1)) && Check if the file have a valid path
        =gfModalGen("INM00049B00000","DIALOGE")
        _CUROBJ = OBJNUM(lcFileN)
        RETURN
      ELSE
        lcLogvFile = ALLTRIM(lcFileN)
      ENDIF
    ELSE && Case the file name is without a path
      lcLogvFile = FULLPATH('')+ALLTRIM(lcFileN)+'.TXT'
    ENDIF
  ENDIF  
ELSE
  =gfModalGen("INM00047B00000","DIALOGE")  
  _CUROBJ = OBJNUM(lcFileN)
  RETURN
ENDIF
SAVE ALL LIKE lcLogvFile* TO (gcWorkDir+'ConvLog.MEM')
CLEAR READ     

*!*************************************************************
*! Name      : lfPrnt
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/10/2001
*! Purpose   : Print the rebalance log report
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfPrnt()
*!*************************************************************
*!102189,4 
FUNCTION lfVPrnt
IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF
