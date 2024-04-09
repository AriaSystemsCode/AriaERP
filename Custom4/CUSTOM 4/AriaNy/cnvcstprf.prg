SET SAFETY OFF
CLOSE DATABASES
PRIVATE lnSource,lnTarget

lnSource=1
lnTarget=2
*lcDataPAth = GETDIR()
lcDataPAth = "R:\ARIANY\DBFS\02\"      && Source psth (Live NY data)
lcTargetPh = "R:\ARIANY\DBFS\SQL\"     && Target Temp path for converted data.
lcCBalPath = "R:\ARIANY\DBFS\98\"      && Customer file for co. 98 for converted data.

*1.1-Customer Profile
=UPDCSTPROF()

*1.3-Aria Applications
=UPDARIAAPP()

*1.2-Application Modules
=UPDAPPMOD()

*1.4-Customer Activation Keys
=UPDACTKEY()

*1.5-Customer Software
=UPDCSTSOFT()

*1.6-Customer Trading Partners Detail
*--Empty.
*1.7-Customer Connections Information
*--As it is (default data).

CLOSE DATABASES
RETURN
************************************************8
FUNCTION UPDCSTPROF

SELECT(lnSource)
USE (lcDataPAth+"CUSTPROF") SHARED ALIAS "XSource"
SELECT(lnTarget)
USE (lcTargetPh+"CUSTPROF") EXCLUSIVE ALIAS "YTarget"
ZAP
SET ORDER TO 1

SELECT(lnSource)
SCAN
  WAIT WINDOW "Customer : "+cCust_id NOWAIT
  SCATTER MEMVAR
  SELECT(lnTarget)
  IF !SEEK(m.cCust_id)
    APPEND BLANK
  ENDIF
  REPLACE CCUST_ID   WITH m.cCust_id,;
          NCUST_USRN WITH m.NCUST_USRN,;
          CCUST_HOMD WITH m.CCUST_HOMD,;
          DCUST_INSD WITH m.DCUST_INSD,;
          DCUST_PADU WITH m.DCUST_PADU,;
          NCUST_QMAM WITH m.NCUST_QMAM,;
          NTIME_BAL  WITH 0.00,;
          NTIME_BLK  WITH 0.00,;
          LACTUSER   WITH .T.,;
          CPROCURE   WITH 'P',;
          NRENTCHRGE WITH 0.00,;
          LHOSTING   WITH .F.,;
          NHOSTCHRGE WITH 0.00,;
          NAVAILMAP  WITH 0.00,;
          CADD_USER  WITH m.cAdd_user,;
          TADD_DATE  WITH CTOT(DTOC(m.dAdd_date)+" "+m.cadd_Time)
ENDSCAN

SELECT(lnSource)
USE (lcCBalPath+"CUSTOMER") SHARED ALIAS "XCustomer"

lnHandle=SQLCONNECT('aria4xp','msp','aria')
IF lnHandle > 0
  SELECT XCustomer
  SCAN FOR Type='M' AND Status $ 'AH' AND NetBal <> 0
    WAIT WINDOW "Customer Balance : "+Account NOWAIT
    SELECT(lnTarget)
    IF SEEK(XCustomer.Account)
      lcSqlCommand = "AddCustTran '"+Customer.Account+"','"+DTOC(DATETIME())+"','Adjustment','',"+ALLTRIM(STR(Customer.NetBal,15,2))+",'Starting Balance as of "+DTOC(DATETIME())+"'"
      =SQLEXEC(lnHandle,lcSqlCommand)
      *REPLACE NTIME_BAL WITH XCustomer.NetBal
    ENDIF
  ENDSCAN
ENDIF
=SQLDISCONNECT(lnHandle)

************************************************8
FUNCTION UPDARIAAPP

SELECT(lnSource)
USE (lcDataPAth+"ARIA_APP") SHARED ALIAS "XSource"
SELECT(lnTarget)
USE (lcTargetPh+"ARIA_APP") EXCLUSIVE ALIAS "YTarget"
ZAP

SELECT(lnSource)
SCAN FOR cBug_app $ 'A26,A27,A40,INT,INS'
  WAIT WINDOW "Application : "+cApp_name NOWAIT
  SCATTER MEMVAR
  SELECT(lnTarget)
  APPEND BLANK
  REPLACE Capp_id    WITH m.cBug_app,;
          cApp_name  WITH m.cApp_name,;
		  LShowInWeb WITH m.LShowInWeb,;
		  cAdd_user  WITH m.cAdd_user,;
          TADD_DATE  WITH CTOT(DTOC(m.dAdd_date)+" "+m.cadd_Time)
ENDSCAN


************************************************8
FUNCTION UPDAPPMOD

SELECT(lnSource)
USE (lcDataPAth+"SUAPPMOD") SHARED ALIAS "XSource"
SELECT(lnTarget)
USE (lcTargetPh+"SUAPPMOD") EXCLUSIVE ALIAS "YTarget"
ZAP

SELECT(lnSource)
SCAN FOR cBug_app $ 'A26,A27,A40,INT,INS'
  WAIT WINDOW "Module : "+cMod_desc NOWAIT
  SCATTER MEMVAR
  lcModType='S'
  IF m.cDefGrpsp='EDI' AND ! (m.cMod_id $ 'EDI BASE  ,UPC MODULE,EB2       ')
    lcModType = 'E'
  ELSE 
    IF m.cMod_id='US' 
      lcModtype = 'U'
    ENDIF
  ENDIF

  SELECT(lnTarget)
  APPEND BLANK
  REPLACE Capp_id    WITH m.cBug_app,;
		  CMOD_ID    WITH m.CMOD_ID,;
		  CMODTYPE   WITH lcModType,;
		  CMOD_DESC  WITH IIF(lcModType='E' AND SUBSTR(m.cMod_Desc,1,5)='EDI -',ALLT(SUBSTR(m.cMod_Desc,6)),m.cMod_Desc),;
		  CDEFGRPSP  WITH m.CDEFGRPSP,;
		  NQTY       WITH 1,;
		  NWINSNGPR  WITH m.NWINSNGPR,;
		  NTHREEUSR  WITH m.NWINMLTPR,;
		  NWINMLTPR  WITH m.NWINMLTPR,;
		  LSUPPORT   WITH IIF(lcModType='U',.F.,.T.),;
		  CITEM      WITH "",;
		  cAdd_user  WITH m.cAdd_user,;
          TADD_DATE  WITH CTOT(DTOC(m.dAdd_date)+" "+m.cadd_Time)
ENDSCAN

*--Add package records.
SELECT(lnTarget)
APPEND BLANK
REPLACE Capp_id    WITH 'A27',;
		CMOD_ID    WITH 'EDI10P',;
		CMODTYPE   WITH 'P',;
		CMOD_DESC  WITH 'EDI 10 Trading Partner Pkg.',;
		CDEFGRPSP  WITH 'EDI',;
		NQTY       WITH 10,;
		NWINSNGPR  WITH 7000.00,;
		NTHREEUSR  WITH 7000.00,;
		NWINMLTPR  WITH 7000.00,;
		LSUPPORT   WITH .F.,;
		CITEM      WITH "",;
		cAdd_user  WITH 'ARIA_CONV',;
        TADD_DATE  WITH DATETIME()
APPEND BLANK
REPLACE Capp_id    WITH 'A27',;
		CMOD_ID    WITH 'EDI15P',;
		CMODTYPE   WITH 'P',;
		CMOD_DESC  WITH 'EDI 15 Trading Partner Pkg.',;
		CDEFGRPSP  WITH 'EDI',;
		NQTY       WITH 15,;
		NWINSNGPR  WITH 10000.00,;
		NTHREEUSR  WITH 10000.00,;
		NWINMLTPR  WITH 10000.00,;
		LSUPPORT   WITH .F.,;
		CITEM      WITH "",;
		cAdd_user  WITH 'ARIA_CONV',;
        TADD_DATE  WITH DATETIME()
APPEND BLANK
REPLACE Capp_id    WITH 'A27',;
		CMOD_ID    WITH 'EDI20P',;
		CMODTYPE   WITH 'P',;
		CMOD_DESC  WITH 'EDI 20 Trading Partner Pkg.',;
		CDEFGRPSP  WITH 'EDI',;
		NQTY       WITH 20,;
		NWINSNGPR  WITH 13000.00,;
		NTHREEUSR  WITH 13000.00,;
		NWINMLTPR  WITH 13000.00,;
		LSUPPORT   WITH .F.,;
		CITEM      WITH "",;
		cAdd_user  WITH 'ARIA_CONV',;
        TADD_DATE  WITH DATETIME()
APPEND BLANK
REPLACE Capp_id    WITH 'A27',;
		CMOD_ID    WITH 'EDI30P',;
		CMODTYPE   WITH 'P',;
		CMOD_DESC  WITH 'EDI 30 Trading Partner Pkg.',;
		CDEFGRPSP  WITH 'EDI',;
		NQTY       WITH 30,;
		NWINSNGPR  WITH 18000.00,;
		NTHREEUSR  WITH 18000.00,;
		NWINMLTPR  WITH 18000.00,;
		LSUPPORT   WITH .F.,;
		CITEM      WITH "",;
		cAdd_user  WITH 'ARIA_CONV',;
        TADD_DATE  WITH DATETIME()
RETURN


************************************************8
FUNCTION UPDACTKEY

SELECT(lnSource)
USE (lcDataPAth+"SUACTKEY") SHARED ALIAS "XSource"
SELECT(lnTarget)
USE (lcTargetPh+"SUACTKEY") EXCLUSIVE ALIAS "YTarget"
ZAP

SELECT(lnSource)
SCAN
  WAIT WINDOW "Customer : "+cCust_id NOWAIT
  SCATTER MEMVAR MEMO
  SELECT(lnTarget)
  APPEND BLANK
  REPLACE CCUST_ID   WITH m.cCust_id,;
		  cactkey    WITH m.cactkey,;
		  Dkeydate   WITH m.Dkeydate,;
		  Nnumuser   WITH m.Nnumuser,;
		  cact_type  WITH IIF(EMPTY(m.cact_type),'R',m.cact_type),;
		  dexpr_date WITH m.dexpr_date,;
		  mactkeymod WITH m.mactkeymod
ENDSCAN


************************************************8
FUNCTION UPDCSTSOFT

SELECT(lnSource)
USE (lcDataPAth+"CSTSOFTW") SHARED ALIAS "XSource"
SELECT(lnTarget)
USE (lcTargetPh+"CSTSOFTW") EXCLUSIVE ALIAS "YTarget"
ZAP
SET ORDER TO SOFTITEM   && CCUST_ID+CAPP_ID+CMOD_ID+CMODTYPE+CENTRYNO

SELECT 3
USE (lcDataPAth+"CUSTPRG") SHARED
SET ORDER TO TAG CSID   && CCPRGID+CCPRGMPRG+STR(RECNO(),8)
SELECT 4
USE (lcTargetPh+"SUAPPMOD") SHARED
SET ORDER TO APPMODUL   && CAPP_ID+CMOD_ID+CMODTYPE
SELECT 5
USE (lcTargetPh+"CUSTPROF") SHARED
SET ORDER TO CUST_ID   && CCUST_ID


SELECT(lnSource)
SCAN
  SCATTER MEMVAR

  lcMod_id = IIF(m.cPrg_mod='P',PADR('CUSTPRG',10),PADR(ALLTRIM(m.cSoft_Modu),10))

  llCstFound=SEEK(m.cCust_id,'CUSTPROF')
  llModFound=SEEK(m.cSoft_APID+lcMod_id,'SUAPPMOD')

  IF m.cPrg_mod = 'P'
    lcModType = 'C'
  ELSE
    lcModType = IIF(llModFound,SUAPPMOD.cModType,'S')
  ENDIF
  lcEntryNo = IIF(lcModType='C',PADR(ALLTRIM(m.cSoft_ver),6),'')


  WAIT WINDOW "Customer / Module : "+m.cCust_id+" / "+lcMod_id NOWAIT
  SELECT(lnTarget)
  IF !SEEK(m.CCUST_ID+PADR(m.cSoft_ApID,3)+lcMod_id+lcModType+lcEntryNo)
    APPEND BLANK
  ENDIF
  REPLACE CCUST_ID   WITH m.cCust_id,;
          CAPP_ID    WITH m.cSoft_ApID,;
          CMOD_ID    WITH lcMod_id ,;
          CMODTYPE   WITH lcModType,;
          CSOFT_MODU WITH IIF(lcModType='C' OR !llModFound,m.cSoft_Modu,SUAPPMOD.cMod_Desc),;
          CENTRYNO   WITH lcEntryNo,; 
          NQTY       WITH 1,; 
          LMAINTAIN  WITH IIF(llModFound,SUAPPMOD.LSUPPORT,.F.),;
          CSOFT_PLAT WITH 'W',; 
          NPRICE     WITH IIF(lcModType='C' AND SEEK(lcEntryNo,'CUSTPRG'),CUSTPRG.ncprgblam,0.00),;
          CREFERENCE WITH "",; 
          DSOFT_INS  WITH IIF(EMPTY(m.DSOFT_INS) AND llCstFound,CUSTPROF.dcust_insd,m.DSOFT_INS),;
          NEDIPUSE   WITH 0.00,;
          LEDIMPPK   WITH .F.,;
		  cAdd_user  WITH m.cAdd_user,;
          TADD_DATE  WITH CTOT(DTOC(m.dAdd_date)+" "+m.cadd_Time)

ENDSCAN

*--Add packages.
SELECT 6
USE (lcTargetPh+"PKGACCT") SHARED
SET ORDER TO TAG acct

lcOldAcct = "  "

lnTotPakQty=0
SELECT PKGACCT
SCAN
  llCstFound=SEEK(PKGACCT.Account,'CUSTPROF')

  IF PKGACCT.Account # lcOldAcct
    lnSqn = 1
  ENDIF
  
  lcMod_id = IIF(pkgacct.Qty1=10,'EDI10P',IIF(pkgacct.Qty1=15,'EDI15P',IIF(pkgacct.Qty1=20,'EDI20P','EDI30P')))
  lcModDes = 'EDI '+ALLTRIM(STR(pkgacct.Qty1,10))+' Trading Partner Pkg.'

  SELECT(lnTarget)
  APPEND BLANK
  REPLACE CCUST_ID   WITH PKGACCT.Account,;
          CAPP_ID    WITH 'A27',;
          CMOD_ID    WITH lcMod_id ,;
          CMODTYPE   WITH 'P',;
          CSOFT_MODU WITH lcModDes,;
		  Centryno   WITH ALLTRIM(STR(lnSqn)),;
          NQTY       WITH pkgacct.Qty1,; 
          LMAINTAIN  WITH .F.,;
          CSOFT_PLAT WITH 'W',; 
          NPRICE     WITH pkgacct.Price,;
          CREFERENCE WITH "Invoice # :"+pkgacct.Invoice,; 
          DSOFT_INS  WITH pkgacct.invdate,;
          NEDIPUSE   WITH 0.00,;
          LEDIMPPK   WITH .F.,;
   	      cAdd_user  WITH 'ARIA_CONV',;
          TADD_DATE  WITH DATETIME()

  IF llCstFound
    SELECT CUSTPROF
    REPLACE NAVAILMAP WITH NAVAILMAP+pkgacct.Qty1
  ENDIF
  
  lcOldAcct = PKGACCT.Account
  lnSqn=lnSqn+1
ENDSCAN

************************************************8
*--SETUP INEXES

NOTE:
*SUACTKEY MEMO -> VARCHAR(500)
*CUSTCONN mnotes MEMO ->VARCAR(500)

*---FIXERRORS
*SELECT Cstsoftw
*LOCATE FOR dsoft_ins>CTOD('01/01/2004')


