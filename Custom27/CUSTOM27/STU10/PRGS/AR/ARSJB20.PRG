*:************************************************************************
*: Program file  : ARSJB20.PRG         (C102292)
*: Program desc. : Copying Stores from one company to another with it's corresponding data.
*:               : for Stunt.
*: Date          : 06/27/2001.
*: Module        : Aria Apparel Series.
*: Developer     : Ahmed Mohammed ElAnwe (AME).
*:************************************************************************
*: Calls : 
*:    Screens    : ARSJB20.SCX
*:    Procedures : BrowComp
*:    Functions  : gfOpenFile()
*:	             : gfTempName()
*:         		 : lfvComp1,lfvComp2,lfvAccount,lfCopy
*:               : gfDialog()
*:               : lfRefresh()
*:************************************************************************
*REFER TO C100644 IN A26
*:************************************************************************

**Initialize the variables.
SELECT SycComp
lcKey = EVAL(KEY())

STORE .F. TO Lcflag
pbCancel=1
STORE SPACE(20) TO lcComp1n,lcComp2n
STORE SPACE(30) TO lcAccN
STORE SPACE(05) TO lcAccount
STORE SPACE(02) TO lcComp1,lcComp2
STORE SPACE(06) TO lcdir1,lcdir2,lcCdir1,lcCdir2
compflag=.T.
DO (GCScrDir+'AR\ARSJB20.SPR')
ON KEY LABEL ESC
ON KEY LABEL ENTER

*!*************************************************************
*! Name : FUNCTION lfvComp1
*! Auth : Ahmed Mohammed ElAnwer (AME).
*! Date : 06/27/2001
*!*************************************************************
*! Synopsis : The valid function of the source company.
*!*************************************************************
*! Called from : 
*!         Procedures :
*!*************************************************************
*! Modifications :
*!*************************************************************
FUNCTION lfvComp1
IF EMPTY(lcComp1)
  lcComp1n = SPACE(20)
  =lfRefresh()
  RETURN
ENDIF
compflag = .T.
SELECT SycComp
SHOW GET Pbproceed DISABLE
SHOW GET pbCancel,1 PROMPT '\<Close '
IF !SEEK(lcComp1)
  DO BrowComp
ELSE
  lcComp1n = cCom_name
  lcCdir1 = ALLTRIM(cCom_dDir)
ENDIF
IF !EMPTY(lcComp2) .AND. lcComp1 = lcComp2
*IF VAL(lcComp1) = VAL(lcComp2)
  *= gfDialog ('I' , 'You cannot copy the stores to the same company')
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'You cannot copy the stores to the same company')
  _CUROBJ = OBJNUM(lcCOMP1)
  SHOW GET lcComp1 ENABLE
  lcComp1n = " " 
  lcComp1 = "  "
ENDIF
lcAccount = SPACE(05)
lcAccN    = SPACE(30)
IF !EMPTY(lcComp1) .AND. !EMPTY(lcComp2)
  SHOW GET lcAccount ENABLE
ELSE
  SHOW GET lcAccount DISABLE  
ENDIF
lcdir1 = ALLTRIM(lcCdir1)
lcdir2 = ALLTRIM(lcCdir2)

=SEEK(lcKey,'SYCCOMP') 
=lfRefresh()

*!*************************************************************
*! Name : FUNCTION lfvComp2
*! Auth : Ahmed Mohammed ElAnwer (AME).
*! Date : 06/27/2001
*!*************************************************************
*! Synopsis : The valid function of the target company.
*!*************************************************************
*! Called from : 
*!         Procedures :
*!*************************************************************
*! Modifications :
*!*************************************************************
FUNCTION lfvComp2
IF EMPTY(lcComp2)
  lcComp2n = SPACE(20)
  =lfRefresh()
  RETURN
ENDIF
compflag = .F.
SELECT SycComp
SHOW GET Pbproceed DISABLE
SHOW GET pbCancel,1 PROMPT '\<Close '
IF !SEEK(lcComp2)
  DO BrowComp
ELSE
  lcComp2n = cCom_Name
  lcCdir2 = ALLTRIM(cCom_dDir)
ENDIF
IF !EMPTY(lcComp2) .AND. lcComp1 = lcComp2
  *= gfDialog ('I' , 'You cannot copy the stores to the same company')
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'You cannot copy the stores to the same company')
  _CUROBJ = OBJNUM(lcCOMP2)
  SHOW GET lcComp2 ENABLE
  lcComp2n = " " 
  lcComp2 = "  "
ENDIF

IF !EMPTY(lcComp1) .AND. !EMPTY(lcComp2)
  SHOW GET lcAccount ENABLE
ELSE
  SHOW GET lcAccount DISABLE  
ENDIF
lcdir1 = " " 
lcdir2 = " " 
lcdir1 = ALLTRIM(lcCdir1)
lcdir2 = ALLTRIM(lcCdir2)
=SEEK(lcKey,'SYCCOMP')
=lfRefresh()
*!*************************************************************
*! Name : FUNCTION lfvAccount
*! Auth : Ahmed Mohammed ElAnwer (AME).
*! Date : 06/27/2001
*!*************************************************************
*! Synopsis : The valid function of the account.
*!*************************************************************
*! Called from : 
*!         Procedures :
*!*************************************************************
*! Modifications :
*!*************************************************************

FUNCTION lfvAccount
IF USED('CUSTOMER')
  SELECT CUSTOMER
  USE
ENDIF
=gfOpenFile(lcdir1+'CUSTOMER',lcdir1+'CUSTOMER','SH')
lcAccount = PADR(lcAccount,5," ")
SELECT CUSTOMER
IF EMPTY(lcAccount) .OR. !SEEK("M"+lcAccount) 
  DO CUSBROWM WITH lcAccount
ENDIF
IF EMPTY(lcAccount)
  lcAccN = SPACE(30)
  lcAccount = SPACE(05)
  RETURN
ENDIF
lcAccN = BTName
=lfRefresh()
USE
=gfOpenFile(lcdir2+'CUSTOMER',lcdir2+'CUSTOMER','SH')
IF !SEEK("M"+lcAccount)
  *=gfDialog('I','This Account not found in the target Company, Please add it first.')
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'This Account not found in the target Company, Please add it first.')
  SHOW GET Pbproceed DISABLE
  SHOW GET pbCancel,1 PROMPT '\<Close '
  return
ELSE
  IF USED('CODE2')
    SELECT Code2
    USE
  ENDIF
  =gfOpenFile(lcDir2+'CODES',lcDir2+'CODES','SH','Code2')
  IF USED('CODE1')
    SELECT Code1
    USE
  ENDIF
  =gfOpenFile(lcDir1+'CODES',lcDir1+'CODES','SH','Code1')
  IF USED('SRep2')
    SELECT SRep2
    USE
  ENDIF
  =gfOpenFile(lcDir2+'SALESREP',lcDir2+'SALESREP','SH','SRep2')
  SHOW GET Pbproceed ENABLE
  SHOW GET pbCancel,1 PROMPT '\<Cancel '
ENDIF


*!*************************************************************
*! Name : FUNCTION lfCopy
*! Auth : Ahmed Mohammed ElAnwer (AME).
*! Date : 06/27/2001
*!*************************************************************
*! Synopsis : The valid function of the Proceed button.
*!*************************************************************
*! Called from : 
*!         Procedures :
*!*************************************************************
*! Modifications :
*!*************************************************************
FUNCTION lfCopy
*--Create the temp files.
lcTmpStr = gfTempName()
IF USED('CUSTOMER')
  SELECT CUSTOMER
  USE
ENDIF
=gfOpenFile(lcdir1+'CUSTOMER',lcdir1+'CUSTOMER','SH')
SELECT CUSTOMER
GO TOP
WAIT WINDOW "Collecting data" NOWAIT
IF !EMPTY(lcAccount)
  SELECT * FROM CUSTOMER WHERE ACCOUNT=lcAccount .AND. TYPE = 'S';
  INTO TABLE (gcWorkDir+lcTmpStr) 
ENDIF
SELECT &lcTmpStr
IF _TALLY = 0
  WAIT CLEAR
  *=gfDialog('I','No Stores selected.')
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No Stores selected.')
  lcAccN = SPACE(30)
  =lfRefresh()
  RETURN
ENDIF
SELECT CUSTOMER
USE
=gfOpenFile(lcdir2+'CUSTOMER',lcdir2+'CUSTOMER','SH')
SELECT &lcTmpStr
DO WHILE .T.
  lcStore = Store
  SCATTER MEMVAR 
  CusDivision  = cDivision
  CusRegion    = Region
  CusSpcInst   = SpcInst
  CusTerms     = cTermCode
  CusShipVia   = ShipVia
  CusClass     = Class
  CusCaltShpVia= CaltShpVia
  CusRep1      = SalesRep
  CusRep2      = Rep2
  CusFactor    = cFacCode
  
  SELECT CUSTOMER
  *If the store was copied before or already found in the second company.
  IF SEEK("S"+lcAccount+lcStore)
    *= gfDialog ('I' , 'STORE ' + ALLTRIM(lcStore) +' ALREADY EXIST')
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'STORE ' + ALLTRIM(lcStore) +' ALREADY EXIST')
  	Lcflag = .F.
  ELSE
    SEEK("M"+lcAccount)
    lcRegion    = Region
    lcClass     = Class
    lcTerms     = cTermCode
    lcShipvia   = Shipvia
    lcDivision  = cDivision
    lcSpcinst   = Spcinst
    lcCaltShpvia= CaltShpvia
    lcRep1      = SalesRep
    lcRep2      = Rep2
    lcFactor    = cFacCode
  	Lcflag = .T.
    WAIT WINDOW lcstore NOWAIT
    SELECT Code2
    lcDivision  = IIF(SEEK('N'+CusDivision+'NCDIVISION'),CusDivision,lcDivision)
    lcRegion    = IIF(SEEK('N'+CusRegion+'NREGION'),CusRegion,lcRegion)
    lcSpcInst   = IIF(SEEK('N'+CusSpcInst+'NSPCINST'),CusSpcInst,lcSpcInst)
    lcTerms     = IIF(SEEK('N'+CusTerms+'NCTERMCODE'),CusTerms,lcTerms)
    IF EMPTY(ALLTRIM(CusShipVia))
      lcShipVia   = ' '
    ELSE
      = SEEK('N'+CusShipVia+'NSHIPVIA','Code1')
      lcDiscrip = Code1.cDiscrep
      LOCATE FOR UPPER(ALLTRIM(cDiscrep)) = UPPER(ALLTRIM(lcDiscrip))
      IF FOUND()
        lcShipVia = CCODE_NO
      ELSE
        lcShipVia = lcShipVia
      ENDIF
    ENDIF
    
    lcClass     = IIF(SEEK('N'+CusClass+'NCLASS'),CusClass,lcClass)
    
    IF EMPTY(ALLTRIM(CusCaltShpVia))
      lcCaltShpVia   = ' '
    ELSE
      = SEEK('N'+CusCaltShpVia+'NSHIPVIA','Code1')
      lcDiscrip = Code1.cDiscrep
      LOCATE FOR UPPER(ALLTRIM(cDiscrep)) = UPPER(ALLTRIM(lcDiscrip))
      IF FOUND()
        lcCaltShpVia = CCODE_NO
      ELSE
        lcCaltShpVia = lcShipVia
      ENDIF
    ENDIF

    SELECT SRep2
    lcRep1      = IIF(EMPTY(ALLTRIM(CusRep1))," ",IIF(SEEK(CusRep1),CusRep1,lcRep1))
    lcRep2      = IIF(EMPTY(ALLTRIM(CusRep2))," ",IIF(SEEK(CusRep2),CusRep2,lcRep2))
    SELECT CUSTOMER
    lcFactor    = IIF(EMPTY(ALLTRIM(CusFactor))," ",IIF(SEEK('F'+CusFactor),CusFactor,lcFactor))
    APPEND BLANK
    GATHER MEMVAR
    REPLACE LINK_CODE WITH SPACE(3),CONSOL WITH SPACE(1),;
            DUNSRTG WITH SPACE(4),DUNS WITH SPACE(11),UPSZONE WITH SPACE(1),;
            DISC WITH 0,CRLIMIT WITH 0,CRAVAIL WITH 0,EXPIRES WITH {},;
            AGEDATE WITH {},CURRENT WITH 0,AGE30 WITH 0,AGE60 WITH 0,AGE90 WITH 0,;
            AGE120 WITH 0,TOTAGE WITH 0,OPENCR WITH 0,CHGBACK WITH 0,NETBAL WITH 0,;
            TERCURRENT WITH 0,TERAGE30 WITH 0,TERAGE60 WITH 0,TERAGE90 WITH 0,;
            TERAGE120 WITH 0,YTDBAL WITH 0,CINSUR WITH SPACE(1),PRICELVL WITH SPACE(1),;
            UPS_INCR WITH 0,FLAG WITH SPACE(1),SKUTMPL WITH SPACE(3),PRNT_STATM WITH SPACE(1),;
            DADD_DATE WITH DATE(),CADD_TIME WITH TIME(),CADD_USER WITH GCUSER_ID,;
            CLOK_TIME WITH SPACE(15),DLOK_DATE WITH {},CLOK_USER WITH SPACE(10)
    REPLACE Region WITH lcRegion,Class WITH lcClass,cTermCode WITH lcTerms,ShipVia WITH lcShipVia,;
            cDivision WITH lcDivision,SpcInst WITH lcSpcInst,CaltShpVia WITH lcCaltShpVia,;
            SalesRep WITH lcRep1,Rep2 WITH lcRep2,cFacCode WITH lcFactor
    IF EMPTY(lcCaltShpVia)
      REPLACE NBRKWEIGHT WITH 0
    ENDIF
  ENDIF
  WAIT CLEAR
  SELECT &lcTmpStr
  IF !EOF()
    SKIP
    IF !EOF()
      LOOP
    ELSE
      EXIT
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO
WAIT CLEAR
IF Lcflag = .T.
  *= gfDialog ('I' , 'The stores was successfully copied')
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'The stores was successfully copied')
ENDIF
STORE SPACE(20) TO lcComp1n,lcComp2n
STORE SPACE(05) TO lcAccount
STORE SPACE(02) TO lcComp1,lcComp2
STORE SPACE(30) TO lcAccN
SHOW GET lcComp1
SHOW GET lcComp2
STORE SPACE(6) TO lcdir1,lcdir2,lcCdir1,lcCdir2
SHOW GET lcAccount DISABLE
SHOW GET Pbproceed DISABLE
SHOW GET pbCancel,1 PROMPT '\<Close '
=lfRefresh()
SELECT SYCCOMP
=SEEK(lcKey)
SELECT SREP2
USE
SELECT CODE2
USE

*!*************************************************************
*! Name : Procedure BrowComp
*! Auth : Ahmed Mohammed ElAnwer (AME).
*! Date : 06/27/2001
*!*************************************************************
*! Synopsis : Browsing the companies.
*!*************************************************************
*! Called from : lfvComp1,lfvComp2
*!         Procedures :
*!*************************************************************
*! Modifications :
*!*************************************************************

PROCEDURE BrowComp
PRIVATE lcBrFields 

DECLARE laTmpDat[1]
laTmpDat[1] = ' '


IF _DOS OR _UNIX
  DEFINE WINDOW lcwind FROM 7,22 TO 17,60 COLOR SCHEME 10 TITLE "COMPANY"
ELSE
  DEFINE WINDOW lcwind FROM 7,33 TO 19,74 COLOR SCHEME 10 TITLE "COMPANY"  
ENDIF
*ON KEY LABEL ENTER KEYBOARD "{CTRL+W}"
lcField     = 'cComp_ID'
lcBrFields = "cComp_ID :H='Code',cCom_Name :H='Name' "
llChose=gfBrows('FOR  .T.',lcField,'laTmpDat','Company Code','','',.F.)
IF llChose
  IF compflag = .T.
    lccomp1=cComp_ID 
    lcComp1n = cCom_Name
    lcCdir1=ALLTRIM(cCom_dDir)
  ELSE
    lcComp2=cComp_ID 
    lcComp2n = cCom_Name
    lcCdir2=ALLTRIM(cCom_dDir)
  ENDIF
ELSE
  IF compflag = .T.
    lccomp1='  '
    lcComp1n = ' '
    lcCdir1=' '
  ELSE
    lcComp2='  '
    lcComp2n = ' '
    lcCdir2=' '
  ENDIF
ENDIF
RELEASE WINDOW lcwind
ON KEY LABEL ENTER 
