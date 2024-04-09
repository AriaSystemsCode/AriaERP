*:************************************************************************
*: Program file  : ARFRE30.PRG
*: Program desc. : Copying Customer from company to another.
*:                 The program is a conversion from 2.6 ==> 2.7
*: System        : Aria Apparel Series. Ver 2.7
*: Module        : AR
*: Date          : 07/17/2000
*: Developer     : Mohamed Shokry (MHM)
*: Refer to      : C101900
*:************************************************************************
*: Calls : 
*:         Functions  : lfSelComp()
*:                    : lfDefComp()
*:                    : lfvSelComp()
*:                    : lfDataDir()
*:                    : lfSelComT()
*:                    : lfDefComT()
*:                    : lfvSelComT()
*:                    : lfDataDirT()
*:                    : lfChkRp()
*:                    : lfCloseFile()
*:         Procedures : lpAddCustomer()
*:                    : lpAddDept()
*:                    : lpAddNotePad()
*:                    : lpAddTerms()
*:                    : lpAddRegion()
*:                    : lpAddDiv()
*:                    : lpAddSpc()
*:                    : lpAddClass()
*:                    : lpAddShipVia() 
*:                    : lpAddSalesRep()
*:************************************************************************
*:Passed Parameters : None
*:************************************************************************
*:Example           : Do SMFRE20
*:************************************************************************
*:Modifications     :
*:************************************************************************
STORE .F. TO llCopy
STORE '' TO laCompDesc ,laCompCode ,laCompDest ,laCompCodT
STORE '' TO lcRpSelCom ,lcCompArrs ,lcDataDir ,lcRpSelCot ,lcCompArrT ,lcDataDirT , lcAccount
STORE '' TO lnComm1 ,lnComm2 ,lnAlis , lcSales1 ,lcSales2 
STORE .T. TO llchk         && check for multi store

*-- Open option grid
lcExpr = gfOpGrid('ARFRE300',.T.)  && Run selection grid.

IF EMPTY(lcExpr) 
  IF lcExpr = .F.
    RETURN
  ENDIF
ENDIF

TrgtCust  = gfTempName()  
TrgtCode  = gfTempName()  
SorcNtPad = gfTempName()  
TrgRep    = gfTempName()  
lcCustmr  = gfTempName() 
SorcCode  = gfTempName() 
TrgNtPad  = gfTempName() 
SorcRep   = gfTempName() 
TrgDept   = gfTempName()  
SorcDept  = gfTempName()
SorcProf  = gfTempName()  
TrgProf   = gfTempName()  
SorcCont  = gfTempName()  
TrgCont   = gfTempName()  
SorcHist  = gfTempName()  
TrgHist   = gfTempName()  

STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO
= gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',lcRPSelCom)
= gfOpenFile(lcDataDirT+'Customer',lcDataDirT+'Customer','SH',TrgtCust,.T.) && Target table
= gfOpenFile(lcDataDir+'CODES',lcDataDir+'CODES','SH',SorcCode,.T.)
= gfOpenFile(lcDataDirT+'CODES',lcDataDirT+'CODES','SH',TrgtCode,.T.)
= gfOpenFile(lcDataDir+'CustDept',lcDataDir+'CustDept','SH',SorcDept,.T.)
= gfOpenFile(lcDataDirT+'CustDept',lcDataDirT+'CustDept','SH',TrgDept,.T.)
= gfOpenFile(lcDataDir+'NotePad',lcDataDir+'NotePad','SH',SorcNtPad,.T.)
= gfOpenFile(lcDataDirT+'NotePad',lcDataDirT+'NotePad','SH',TrgNtPad,.T.)
= gfOpenFile(lcDataDir+'Profile',lcDataDir+'Profile','SH',SorcProf,.T.)
= gfOpenFile(lcDataDirT+'Profile',lcDataDirT+'Profile','SH',TrgProf,.T.)
= gfOpenFile(lcDataDir+'SalesRep',lcDataDir+'SalesRep','SH',SorcRep,.T.)
= gfOpenFile(lcDataDirT+'SalesRep',lcDataDirT+'SalesRep','SH',TrgRep,.T.)
= gfOpenFile(lcDataDir+'Contact',lcDataDir+'Contact','SH',SorcCont,.T.)
= gfOpenFile(lcDataDirT+'Contact',lcDataDirT+'Contact','SH',TrgCont,.T.)
= gfOpenFile(lcDataDir+'ARhist',lcDataDir+'ARhist','SH',Sorchist,.T.)
= gfOpenFile(lcDataDirT+'ARhist',lcDataDirT+'ARhist','SH',Trghist,.T.)

*-Collect Data
SELECT (lnAlis)
=SEEK ('M'+lcAccount)
DO lpAddCustomer WITH 'M'

=SEEK ('S'+lcAccount)
DO lpAddCustomer WITH 'S'

*--
IF llCopy
  = gfModalGen(.f.,.f.,.f.,.f.,'Process completed successfully.')
ELSE 
  = gfModalGen(.f.,.f.,.f.,.f.,'Process Not completed successfully.')
ENDIF
WAIT CLEAR 
*-- End Of ARFRE30.PRG

*!****************************************************************************
*! Name      : lfSelComp
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : To Select Source Co.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : None 
*!****************************************************************************
*! Example            : =lfSelComp()
*!****************************************************************************
FUNCTION lfSelComp
PRIVATE lnI,lnCurAlias,lcCurTag

  DIMENSION laCompDesc[1,1],laCompCode[1,1]
  STORE SPACE(0) TO laCompDesc[1,1],laCompCode[1,1]

lnCurAlias = SELECT(0)
SELECT SycComp
lcCurTag = ORDER()
SET ORDER TO Ccomp_id
PRIVATE lnI
lnI = 1
SCAN
  lnI = ALEN(laCompDesc,1) + IIF(EMPTY(laCompDesc[lnI]),0,1)
  DIMENSION laCompDesc[lnI,1],laCompCode[lnI,1]
  laCompDesc[lnI,1] = SycComp.cComp_ID+"-"+SycComp.cCom_Name
  laCompCode[lnI,1] = SycComp.cComp_ID
ENDSCAN

SET ORDER TO &lcCurTag

SELECT (lnCurAlias)
*-- End Of lfSelComp.


*!****************************************************************************
*! Name      : lfDefComp
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Select Default Source Co.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : lcRet
*!****************************************************************************
*! Example            : =lfDefComp()
*!****************************************************************************
FUNCTION lfDefComp
PRIVATE lcRet

IF EMPTY(gcAct_Comp)
  DIMENSION laCompDesc[ALEN(laCompDesc,1)+1,1],laCompCode[ALEN(laCompCode,1)+1,1]
  = AINS(laCompDesc , 1)
  = AINS(laCompCode , 1)
  laCompDesc[1,1] = "Select source company"
  laCompCode[1,1] = "NoComp"
  lcRet = "NoComp"
ELSE
  lcRet = gcAct_Comp
  = lfDataDir(lcRet)
ENDIF

RETURN lcRet
*-- End Of lfDefComp.


*!****************************************************************************
*! Name      : lfvSelComp
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Open Code File For Selected Co.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfvSelComp
*!****************************************************************************
FUNCTION lfvSelComp

= lfDataDir(lcRPSelCom)
*-- End Of lfvSelComp.


*!****************************************************************************
*! Name      : lfDataDir
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Get Data Dir.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : lcComp
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfDataDir()
*!****************************************************************************
FUNCTION lfDataDir
PARAMETERS lcComp
PRIVATE lcComp,lnCurAlias,lcCurTag

lnCurAlias = SELECT(0)
SELECT SycComp
lcCurTag = ORDER()
SET ORDER TO Ccomp_id
IF SEEK(lcComp)
  lcDataDir = gfGetDataDir(ALLTRIM(SycComp.cCom_dDir))
ENDIF  
SET ORDER TO &lcCurTag
SELECT (lnCurAlias)
*-- End Of lfDataDir.



*!****************************************************************************
*! Name      : lfSelComT
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : To Select Target Co.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : None 
*!****************************************************************************
*! Example            : =lfSelComT()
*!****************************************************************************
FUNCTION lfSelComT
PRIVATE lnI,lnCurAlias,lcCurTag

DIMENSION laCompDesT[1,1],laCompCodT[1,1]
STORE SPACE(0) TO laCompDesT[1,1],laCompCodT[1,1]
lnCurAlias = SELECT(0)
SELECT SycComp
lcCurTag = ORDER()
SET ORDER TO Ccomp_id
PRIVATE lnI
lnI = 1
SCAN
  lnI = ALEN(laCompDesT,1) + IIF(EMPTY(laCompDesT[lnI]),0,1)
  DIMENSION laCompDesT[lnI,1],laCompCodT[lnI,1]
  laCompDesT[lnI,1] = SycComp.cComp_ID+"-"+SycComp.cCom_Name
  laCompCodT[lnI,1] = SycComp.cComp_ID
ENDSCAN
SET ORDER TO &lcCurTag
SELECT (lnCurAlias)
*-- End Of lfSelComp.


*!****************************************************************************
*! Name      : lfDefComT
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Select Default Target Co.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : lcRet
*!****************************************************************************
*! Example            : =lfDefComT()
*!****************************************************************************
FUNCTION lfDefComT
PRIVATE lcRet

DIMENSION laCompDesT[ALEN(laCompDesT,1)+1,1],laCompCodT[ALEN(laCompCodT,1)+1,1]
= AINS(laCompDesT , 1)
= AINS(laCompCodT , 1)
laCompDesT[1,1] = "Select Target Company"
laCompCodT[1,1] = "NoComp"
lcRet = "NoComp"

RETURN lcRet
*-- End Of lfDefComT.


*!****************************************************************************
*! Name      : lfvSelComT
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Call Function to Get Data Dir.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfvSelComT
*!****************************************************************************
FUNCTION lfvSelComT
PRIVATE lnCurAlias

= lfDataDirT(lcRPSelCoT)
*-- End Of lfvSelComT.


*!****************************************************************************
*! Name      : lfDataDirT
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Get Data Dir.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : lcComp
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfDataDirT()
*!****************************************************************************
FUNCTION lfDataDirT
PARAMETERS lcComp
PRIVATE lcComp,lnCurAlias,lcCurTag

lnCurAlias = SELECT(0)
SELECT SycComp
lcCurTag = ORDER()
SET ORDER TO Ccomp_id
IF SEEK(lcComp)
  lcDataDirT = gfGetDataDir(ALLTRIM(SycComp.cCom_dDir))
ENDIF  
SET ORDER TO &lcCurTag
SELECT (lnCurAlias)
*-- End Of lfDataDir.


*!****************************************************************************
*! Name      : lfChkRp
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Chek if no target company selected 
*!             & if Target company is source company
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : 
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfChkRp()
*!****************************************************************************
FUNCTION lfChkRp

IF lcRpSelCom = "NoComp"
  = gfModalGen(.f.,.f.,.f.,.f.,'No source company selected.')
  RETURN .F.
ELSE
  IF lcRpSelCoT = "NoComp"
    = gfModalGen(.f.,.f.,.f.,.f.,'No target company selected.')
    RETURN .F.
  ELSE
    IF lcRpSelCoT = lcRPSelCom
      = gfModalGen(.f.,.f.,.f.,.f.,'Target company cannot be the source company.')
      RETURN .F.
    ENDIF
  ENDIF
ENDIF
*-- End Of lfChkRp.


*!*************************************************************
*! Name      : lpAddCustomer
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add customer data to the target company.
*!*************************************************************
*! Parameter : lcType
*!*************************************************************
*! Example   : Do lpAddCustomer
*!*************************************************************

PROCEDURE lpAddCustomer
PARAMETER lcType
PRIVATE lnSavAls 
lnSavAls = SELECT(0)

SELECT (lnAlis)
SCAN WHILE TYPE+ACCOUNT=lcType+lcAccount
  SCATTER MEMVAR FIELDS EXCEPT LINK_Code,CRLIMIT,CRAVAIL,AGEDATE,CURRENT,AGE30,;
                               AGE60,AGE90,AGE120,TOTAGE,OPENCR,CHGBACK,;
                               NETBAL,TERCURRENT,TERAGE30,TERAGE60,TERAGE90,;
                               TERAGE120,YTDBAL,SALESREP,COMM,REP2,COMM2
  SELECT (TrgtCust)
  
  IF SEEK ("M"+lcAccount) .AND. llchk
      = gfModalGen(.f.,.f.,.f.,.f.,'Customer Already Exist .')
      SELECT (lnSavAls)
      RETURN
  ELSE
    APPEND BLANK
    GATHER MEMVAR
    REPLACE SalesRep WITH lcSales1,Comm WITH lnComm1,Rep2 WITH lcSales2,Comm2 WITH lnComm2
    lcDivision  = cDivision
    lcRegion    = Region
    lcSpcinst   = Spcinst
    lcTerms     = cTermCode
    lcClass     = Class
    lcShipvia   = Shipvia
    lcCaltShpvia = CaltShpvia
    DO lpAddCont
    DO lpAddProf
    DO lpAddhist
    DO lpAddDept
    DO lpAddNotePad    
    DO lpAddDiv WITH lcDivision
    DO lpAddRegion WITH lcRegion
    DO lpAddSpc WITH lcSpcInst 
    DO lpAddTerms WITH lcTerms
    DO lpAddClass WITH lcClass 
    DO lpAddShipVia WITH lcShipVia
    DO lpAddShipVia WITH lcCaltShpVia
    llCopy=.T.  
  ENDIF  
ENDSCAN
llchk = .F.
*--Save Old Alias
SELECT(lnSavAls)

*!*************************************************************
*! Name      : lpAddDept
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add customer department to the target company.
*!*************************************************************
*! Example   : Do lpAddDept
*!*************************************************************
PROCEDURE lpAddDept
PRIVATE lnSavAls
lnSavAls = SELECT(0)
SELECT (SorcDept)
IF SEEK(lcAccount)
  SCAN WHILE ACCOUNT=lcAccount
    SCATTER MEMVAR
    SELECT (TrgDept)
    IF !SEEK(lcAccount+&TrgDept..Dept)
      APPEND BLANK
      GATHER MEMVAR
      REPLACE Rep1 WITH lcSales1,Comm1 WITH lnComm1,Rep2 WITH lcSales2,Comm2 WITH lnComm2
      lcDRep1=lcSales1
      lcDRep2=lcSales2
      lcDTerms=cTermCode    
      DO lpAddSalesRep WITH lcDRep1
      DO lpAddSalesRep WITH lcDRep2
      DO lpAddTerms    WITH lcDTerms
    ENDIF  
  ENDSCAN
ENDIF
SELECT (lnSavAls)
*!*************************************************************
*! Name      : lpAddNotePad
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add customer notepad to the target company.
*!*************************************************************
*! Example   : Do lpAddNotePad
*!*************************************************************
PROCEDURE lpAddNotePad

PRIVATE lnSavAls
lnSavAls = SELECT(0)

SELECT (SorcNtPad)
IF SEEK('A'+lcAccount)
  SCATTER MEMVAR MEMO
  SELECT (TrgNtPad)
  IF !SEEK('A'+lcAccount)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDIF
ENDIF
SELECT (lnSavAls)
*!*************************************************************
*! Name      : lpAddProf
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add customer Profile to the target company.
*!*************************************************************
*! Example   : Do lpAddProf
*!*************************************************************
PROCEDURE lpAddProf

PRIVATE lnSavAls
lnSavAls = SELECT(0)

SELECT (SorcProf)
IF SEEK('C'+lcAccount)
  SCATTER MEMVAR MEMO
  SELECT (TrgProf)
  IF !SEEK('C'+lcAccount)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDIF
ENDIF
SELECT (lnSavAls)
*!*************************************************************
*! Name      : lpAddCont
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add customer Contact to the target company.
*!*************************************************************
*! Example   : Do lpAddCont
*!*************************************************************
PROCEDURE lpAddCont

PRIVATE lnSavAls
lnSavAls = SELECT(0)

SELECT (SorcCont)
IF SEEK('C'+lcAccount)
  SCATTER MEMVAR MEMO
  SELECT (TrgCont)
  IF !SEEK('C'+lcAccount)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDIF
ENDIF
SELECT (lnSavAls)
*!*************************************************************
*! Name      : lpAddhist
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add customer Contact to the target company.
*!*************************************************************
*! Example   : Do lpAddhist
*!*************************************************************
PROCEDURE lpAddhist

PRIVATE lnSavAls
lnSavAls = SELECT(0)

SELECT (Sorchist)
IF SEEK(lcAccount)
  SCATTER MEMVAR MEMO
  SELECT (Trghist)
  IF !SEEK(lcAccount)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDIF
ENDIF
SELECT (lnSavAls)
*!*************************************************************
*! Name      : lpAddTerms
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add customer terms to the target company.
*!*************************************************************
*! Parameter : lcTerms
*!*************************************************************
*! Example   : Do lpAddTerms
*!*************************************************************

PROCEDURE lpAddTerms
PARAMETER lcTerms
PRIVATE lnSavAls
lnSavAls = SELECT(0)
SELECT (TrgtCode)
IF !EMPTY(lcTerms) AND !SEEK('N'+lcTerms+'N'+PADR('CTERMCODE',10))
  SELECT (SorcCode)
  IF SEEK('N'+lcTerms+'N'+PADR('CTERMCODE',10))
    SCATTER MEMVAR
    SELECT (TrgtCode)
    APPEND BLANK
    GATHER MEMVAR
  ENDIF
ENDIF
SELECT(lnSavAls)
*!*************************************************************
*! Name      : lpAddRegion
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add customer region to the target company.
*!*************************************************************
*! Parameter : lcRegion
*!*************************************************************
*! Example   : Do lpAddRegion
*!*************************************************************

PROCEDURE lpAddRegion
PARAMETER lcRegion
PRIVATE lnSavAls
lnSavAls = SELECT(0)

SELECT (TrgtCode)
IF !EMPTY(lcRegion) AND !SEEK('N'+lcRegion+'N'+PADR('REGION',10))
  SELECT (SorcCode)
  IF SEEK('N'+lcTerms+'N'+PADR('REGION',10))
    SCATTER MEMVAR
    SELECT (TrgtCode)
    APPEND BLANK
    GATHER MEMVAR
  ENDIF
ENDIF
SELECT(lnSavAls)
*!*************************************************************
*! Name      : lpAddDiv
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add customer division to the target company.
*!*************************************************************
*! Parameter : lcDivision
*!*************************************************************
*! Example   : Do lpAddDiv
*!*************************************************************

PROCEDURE lpAddDiv
PARAMETER lcDivision
PRIVATE lnSavAls
lnSavAls = SELECT(0)

SELECT (TrgtCode)
IF !EMPTY(lcDivision) AND !SEEK('N'+lcDivision+'N'+PADR('CDIVISION',10))
  SELECT (SorcCode)
  IF SEEK('N'+lcDivision+'N'+PADR('CDIVISION',10))
    SCATTER MEMVAR
    SELECT (TrgtCode)
    APPEND BLANK
    GATHER MEMVAR
  ENDIF
ENDIF
SELECT(lnSavAls)
*!*************************************************************
*! Name      : lpAddSpc
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add customer special instructions to the target company.
*!*************************************************************
*! Parameter : lcSpcinst
*!*************************************************************
*! Example   : Do lpAddSpc
*!*************************************************************

PROCEDURE lpAddSpc
PARAMETER lcSpcinst
PRIVATE lnSavAls
lnSavAls = SELECT(0)

SELECT (TrgtCode)
IF !EMPTY(lcSpcinst) AND !SEEK('N'+lcSpcinst+'N'+PADR('SPCINST',10))
  SELECT (SorcCode)
  IF SEEK('N'+lcSpcinst+'N'+PADR('SPCINST',10))
    SCATTER MEMVAR
    SELECT (TrgtCode)
    APPEND BLANK
    GATHER MEMVAR
  ENDIF
ENDIF
SELECT(lnSavAls)
*!*************************************************************
*! Name      : lpAddClass
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add customer class to the target company.
*!*************************************************************
*! Parameter : lcClass
*!*************************************************************
*! Example   : Do lpAddClass
*!*************************************************************

PROCEDURE lpAddClass
PARAMETER lcClass
PRIVATE lnSavAls
lnSavAls = SELECT(0)

SELECT (TrgtCode)
IF !EMPTY(lcClass) AND !SEEK('N'+lcClass+'N'+PADR('CLASS',10))
  SELECT (SorcCode)
  IF SEEK('N'+lcClass+'N'+PADR('CLASS',10))
    SCATTER MEMVAR
    SELECT (TrgtCode)
    APPEND BLANK
    GATHER MEMVAR
  ENDIF
ENDIF
SELECT(lnSavAls)
*!*************************************************************
*! Name      : lpAddShipVia
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add customer shipvia to the target company.
*!*************************************************************
*! Parameter : lcShipVia
*!*************************************************************
*! Example   : Do lpAddShipVia
*!*************************************************************

PROCEDURE lpAddShipVia
PARAMETER lcShipvia
PRIVATE lnSavAls
lnSavAls = SELECT(0)

SELECT (TrgtCode)
IF !EMPTY(lcShipVia) AND !SEEK('N'+lcShipVia+'N'+PADR('SHIPVIA',10))
  SELECT (SorcCode)
  IF SEEK('N'+lcShipVia+'N'+PADR('SHIPVIA',10))
    SCATTER MEMVAR
    SELECT (TrgtCode)
    APPEND BLANK
    GATHER MEMVAR
  ENDIF
ENDIF
SELECT(lnSavAls)
*!*************************************************************
*! Name      : lpAddSalesRep
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Add SalesRep data to the target company.
*!*************************************************************
*! Parameter : lcRep
*!*************************************************************
*! Example   : Do lpAddSalesRep
*!*************************************************************

PROCEDURE lpAddSalesRep
PARAMETER lcRep
PRIVATE lnSavAls
lnSavAls = SELECT(0)

SELECT (TrgRep)
IF !EMPTY(lcRep) AND !SEEK(lcRep)
  SELECT (SorcRep)
  IF SEEK(lcRep)
    lcSRegion=Region
    lcSTerms =cTermCode
    SCATTER MEMVAR
    SELECT (TrgRep)
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Current WITH 0,Age30 WITH 0,Age60 WITH 0,Age90 WITH 0,Age120 WITH 0,Balance WITH 0,;
            AgeDate WITH {},DAdd_Date WITH Date(),CAdd_Time WITH Time(),Cadd_user WITH ''
    
    DO lpAddRegion WITH lcSRegion
    DO lpAddTerms  WITH lcSTerms    
  ENDIF
ENDIF
SELECT(lnSavAls)
*!****************************************************************************
*! Name      : lfvCust
*! Developer : Mohamed Shokry (MHM)
*! Date      : 07/20/2000
*! Purpose   : Open Customer File For Source Co.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfvCust
*!****************************************************************************
FUNCTION lfvCust
PARAMETER lcDummy
IF USED (lcCustmr)
  USE IN (lcCustmr)
ENDIF  
IF gcDataDir =lcDataDir
  = gfOpenFile(lcDataDir+'Customer',lcDataDir+'Customer','SH')
  SELECT Customer
  lnAlis = SELECT(0)  
ELSE
  = gfOpenFile(lcDataDir+'Customer',lcDataDir+'Customer','SH',lcCustmr,.T.) && Target table
  SELECT (lcCustmr)
  lnAlis = SELECT(0)  
ENDIF
*---
RETURN ''
*--End of lfvCust
*!****************************************************************************
*! Name      : lfvSales
*! Developer : Mohamed Shokry (MHM)
*! Date      : 07/20/2000
*! Purpose   : Open SalesRep File For Source Co.
*!****************************************************************************
*! Calls              : None
*!****************************************************************************
*! Passed Parameters  : None
*!****************************************************************************
*! Returns            : None
*!****************************************************************************
*! Example            : =lfvSales()
*!****************************************************************************
FUNCTION lfvSales
PARAMETER lcDummy

IF USED (lcSales)
  USE IN (lcSales)
ENDIF  
= gfOpenFile(lcDataDirT+'Salesrep',lcDataDirT+'Salesrep','SH',lcSales,.T.) && Target table
RETURN ''

*--End of lfvSales
*!*************************************************************
*! Name      : lfRepBrow
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Function to browse RepCode For salesrep1.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfRepBrow()
*!*************************************************************
*
FUNCTION lfRepBrow


IF EMPTY(lcDataDirT)
  = gfModalGen(.f.,.f.,.f.,.f.,'Select Target Company.')
  RETURN
ELSE

  lnAlias = SELECT(0)
  *--OPEN FILE
  =lfvSales()
  SELECT (lcSales)
  IF EOF()
    = gfModalGen(.f.,.f.,.f.,.f.,'There Is No SalesRep At Target Company.')
    RETURN
  ENDIF
  IF EMPTY(lcSales1) .OR. !SEEK(lcSales1)
    lcOld_ttl   = lcFile_ttl
    lcPBrowTitl = "Rep1"   && Browse title.
    lcTmpFld    = lcBrFields           && Save browse fields.
    lcBrFields  = "REPCODE :R :H= 'Rep #' , NAME :R :H= 'Name' , PHONE :R :H= 'Phone' , CADDRESS1 :R :H= 'Address ' , CADDRESS6 :R :H= 'Country' , BALANCE :R :H= 'Balance'"

    *-- Array hold RepCode value.
    DIMENSION latemp[1]
    latemp   = ''
    *-- Call aria browse to display the available UPC
    = ariabrow("",lcPBrowTitl, gnBrFsRow1, gnBrFsCol1,;
                      gnBrFsRow2, gnBrFsCol2,'','',;
                      'RepCode','laTemp')
    *-- Fill with style + size.
    lcSales1= latemp[1]
    *-- Restore browse fields.
    lcBrFields = lcTmpFld
    lcFile_ttl = lcOld_ttl
  ENDIF
    SELECT (lnAlias)
ENDIF
*--END lfRepBrow
*!*************************************************************
*! Name      : lfRepBrows
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Function to browse RepCode for salesrep2.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfRepBrowS()
*!*************************************************************
*
FUNCTION lfRepBrows

IF EMPTY(lcDataDir)
  = gfModalGen(.f.,.f.,.f.,.f.,'Select Source Company.')
  RETURN
ELSE

  lnAlias = SELECT(0)
  *--OPEN FILE
  =lfvSales()
  SELECT (lcSales)
  IF EOF()
    = gfModalGen(.f.,.f.,.f.,.f.,'There Is No SalesRep At Source Company.')
    RETURN
  ENDIF

  IF EMPTY(lcSales2) .OR. !SEEK(lcSales2)
    lcOld_ttl   = lcFile_ttl
    lcPBrowTitl = "Rep2"   && Browse title.
    lcTmpFld    = lcBrFields           && Save browse fields.
    lcBrFields  = "REPCODE :R :H= 'Rep #' , NAME :R :H= 'Name' , PHONE :R :H= 'Phone' , CADDRESS1 :R :H= 'Address ' , CADDRESS6 :R :H= 'Country' , BALANCE :R :H= 'Balance'"
    *-- Array hold RepCode value.
    DIMENSION latemp[1]
    latemp   = ''
    *-- Call aria browse to display the available UPC
    = ariabrow("",lcPBrowTitl, gnBrFsRow1, gnBrFsCol1,;
                      gnBrFsRow2, gnBrFsCol2,'','',;
                      'RepCode','laTemp')
    *-- Fill with style + size.
    lcSales2= latemp[1]
    *-- Restore browse fields.
    lcBrFields = lcTmpFld
    lcFile_ttl = lcOld_ttl
  ENDIF
  SELECT (lnAlias)
ENDIF
*--END lfRepBrows
*!*************************************************************
*! Name      : lfRepBrow
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Function to browse Account code.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAccBrow()
*!*************************************************************
*
FUNCTION lfAccBrow
lnAlias = SELECT(0)
*--OPEN FILE
=lfvCust()
IF EOF()
  = gfModalGen(.f.,.f.,.f.,.f.,'There Is No customer At Source Company.')
  RETURN
ENDIF

IF EMPTY(lcAccount) .OR. !SEEK("M"+lcAccount)
lcOld_ttl   = lcFile_ttl
lcPBrowTitl = "Account "   && Browse title.
lcTmpFld    = lcBrFields           && Save browse fields.
lcBrFields  = "ACCOUNT:H='Account' , Type:H='Type' , BTNAME :R :H= 'Name' , PHONE1 :R :H= 'Phone' , CADDRESS6 :R :H= 'Country'"
*-- Array hold RepCode value.
DIMENSION latemp[1]
latemp   = ''
  *-- Call aria browse to display the available UPC
  = ariabrow(["M"],lcPBrowTitl, gnBrFsRow1, gnBrFsCol1, gnBrFsRow2, gnBrFsCol2,'','',;
                    'Account','laTemp')
  *-- Fill with style + size.
  lcAccount= latemp[1]
  *-- Restore browse fields.
  lcBrFields = lcTmpFld
  lcFile_ttl = lcOld_ttl
ENDIF                    

SELECT (lnAlias)

*--END lfRepBrow
*!*************************************************************
*! Name      : lfVcomm
*! Developer : Mohamed Shokry
*! Date      : 08/01/00
*! Purpose   : Function to VALIDATE COMMATION
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfVcomm()
*!*************************************************************
FUNCTION lfVcomm
PARAMETERS lnComm , lnPar

IF lnComm < 0
  IF lnPar = 1
    *-- Message : 42000
    *-- Negative values are not allowed.
    *-- Button  : 40011
    *-- Ok
    = gfModalGen('TRM42000B40011','DIALOG')
    lnComm1 = 0
    _CUROBJ = _CUROBJ
    RETURN
  ELSE  
    *-- Message : 42000
    *-- Negative values are not allowed.
    *-- Button  : 40011
    *-- Ok
    = gfModalGen('TRM42000B40011','DIALOG')
    lnComm2 = 0
    _CUROBJ = _CUROBJ
   RETURN
  ENDIF
ENDIF
