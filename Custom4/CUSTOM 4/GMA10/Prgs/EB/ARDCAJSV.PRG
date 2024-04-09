***
*** ReFox XI  #ES110215  Edelmiro González Martínez  Asesoria Informatica Gallega S.L [FP25]
***
*
PROCEDURE LFSAVDCADJ
  PARAMETER LLFROMEDI
  LLEDIACCOU = .F.
  
  LCSYSTYPE = GFGETMEMVAR('M_SYSTYPE',GCACT_COMP)
  IF  .NOT. LLFROMEDI .AND. 'NC'$GCCMPMODUL .AND. LCSYSTYPE='P'
    = GFOPENFILE(GCDATADIR+'EDIACPRT',GCDATADIR+'ACCFACT','SH')
    = GFOPENFILE(GCDATADIR+'EDIPD',GCDATADIR+'PARTTRANS','SH')
    = GFOPENFILE(GCDATADIR+'EDITRANS',GCDATADIR+'TYPEKEY','SH')
    SELECT CODES
    SET ORDER TO Idrltfname
    = SEEK('NYCSITEID')
    LOCATE REST FOR CRLTD_NAM='CCMSITETYP' .AND. CRLTD_VLU='B' WHILE CDEFCODE+CRLTFIELD+CFLD_NAME='NYCSITEID'
    IF FOUND()
      LCSITEID = CODES.CCODE_NO
      SELECT EDIACPRT
      LOCATE FOR CSITEID=LCSITEID
      IF FOUND('EDIACPRT') .AND. SEEK(EDIACPRT.CPARTCODE+'812', 'EDIPD')
        LLEDIACCOU = .T.
      ENDIF
    ENDIF
  ENDIF
  SELECT (LCADJTEMP)
  LOCATE FOR EMPTY(ACCOUNT) .OR. IIF(MCHOICE='D', EMPTY(TRANCODE), EMPTY(CCREDITCOD))
  IF FOUND()
    GOTO TOP
    = GFMODALGEN('INM40078B00000','DIALOG')
    *_CUROBJ = OBJNUM(LCACCOUNT)
    RETURN
  ENDIF
  STORE 0 TO LNCOUNT, LNZERO
  COUNT FOR !DELETED() .AND. &lcAdjTemp..AMOUNT <> 0 TO lnCount
  COUNT FOR &lcAdjTemp..AMOUNT = 0 TO lnZero
  IF LNZERO>=1
    = GFMODALGEN('INM40071B00000','DIALOG')
  ENDIF
  IF LNCOUNT=0
    = GFMODALGEN('INM40083B00000','DIALOG')
    *_CUROBJ = OBJNUM(PBNEW)
    RETURN
  ENDIF
  IF ((LNACTUAL<>LNAMOUNT) .OR. (LNTRNCNT<>LNCOUNT) .OR. LNTRNCNT=0 .OR. LNAMOUNT=0)
    DO CASE
      CASE LNACTUAL<>LNAMOUNT
        LNOPTION = GFMODALGEN('QRM40079B40009','DIALOG')
      CASE LNTRNCNT<>LNCOUNT
        LCCOUNT = '('+ALLTRIM(STR(LNCOUNT))+')'
        LNOPTION = GFMODALGEN('QRM40080B40009','DIALOG',LCCOUNT)
    ENDCASE
    IF LNOPTION=1
      *_CUROBJ = OBJNUM(PBNEW)
      RETURN
    ENDIF
  ENDIF
  IF EMPTY(LCBATCHNO)
    LCBATCHNO = GFSEQUENCE('BATCH')
  ENDIF
  SELECT UNCMSESS
  IF LNUNCMSERC<>0 .AND. LNUNCMSERC<=RECCOUNT()
    GOTO LNUNCMSERC
    REPLACE CCURROBJ WITH 'pbSav'
    UNLOCK
    = RLOCK()
  ENDIF
  LLNOTHING = LFUPDVARS()
  LCTMPSTR = LCBATCHNO+'|'+DTOC(LDBATCHDT)
  = GFMODALGEN('INM40081B00000','DIALOG',LCTMPSTR)
  LLADDMODE = .F.
  LLUPDREP = .F.
  IF LLREPCB
    SELECT (LCREPCBTMP)
    DELETE ALL FOR AMOUNT=0
    GOTO TOP
    LLUPDREP = IIF(EOF(), .F., .T.)
  ENDIF
  LNCURREC = 1
  LNTOTREC = LNCOUNT
  SELECT (LCADJTEMP)
  SET ORDER TO
  GOTO TOP
  SCAN FOR  .NOT. DELETED() .AND. AMOUNT<>0
    SELECT (LCADJTEMP)
    = GFTHERM(LNTOTREC,LNCURREC,'Updating the '+LCMODE+'File.')
    LNCURREC = LNCURREC+1
    IF &lcAdjTemp..nSteps < 1
      LCTOSND = IIF(MCHOICE='D', 'DEBIT', 'CREDIT')
      IF (ASCAN(LAEVNTTRIG, PADR('SEQNO', 10))<>0)
        = GFDOTRIGER('ARDBADJ','SEQNO')
      ENDIF
      LCTRANNO = GFSEQUENCE(LCTOSND,GCACT_COMP,'','','TRAN')
      SELECT (LCADJTEMP)
      = RLOCK()
      REPLACE TRAN WITH LCTRANNO, DPOSTDATE WITH LDBATCHDT, BATCH WITH LCBATCHNO, CCURRCODE WITH LCCURRCODE, NEXRATE WITH LNEXRATE, NCURRUNIT WITH LNCURRUNIT
      UNLOCK
      SCATTER MEMO MEMVAR
      IF (ASCAN(LAEVNTTRIG, PADR('CHRGDATE', 10))<>0)
        = GFDOTRIGER('ARDBADJ','CHRGDATE')
      ENDIF
      SELECT (LCCURALIS)
      APPEND BLANK
      = RLOCK()
      GATHER MEMVAR MEMO
      = GFADD_INFO(LCCURALIS)
      UNLOCK
      SELECT (LCADJTEMP)
      = RLOCK()
      REPLACE NSTEPS WITH 1
      UNLOCK
    ENDIF
    IF &lcAdjTemp..nSteps < 2
      SELECT CUSTOMER
      IF SEEK('M'+&lcAdjTemp..ACCOUNT)
        = RLOCK()
        lnEqvAmnt = ROUND(&lcAdjTemp..AMOUNT &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
        REPLACE OPENCR WITH IIF(MCHOICE='D', OPENCR, OPENCR+LNEQVAMNT), CURRENT WITH IIF(MCHOICE='D', CURRENT+LNEQVAMNT, CURRENT), TOTAGE WITH IIF(MCHOICE='D', TOTAGE+LNEQVAMNT, TOTAGE), NETBAL WITH IIF(MCHOICE='D', NETBAL+LNEQVAMNT, NETBAL+(-1*ABS(LNEQVAMNT)))
        IF MCHOICE = "D" AND UPPER(&lcAdjTemp..CCHRGBACK)='YES'
          REPLACE CHGBACK  WITH CHGBACK+&lcAdjTemp..Amount
        ENDIF
        UNLOCK
        = LFHGWUPDAT()
        LCLINKCODE = IIF( .NOT. EMPTY(CUSTOMER.LINK_CODE), CUSTOMER.LINK_CODE, 'DEFDEF')
        IF !EMPTY(&lcAdjTemp..cFacCode)
          LCLINKCODE = LFGETFACLK(LCLINKCODE)
        ENDIF
      ELSE
        LCLINKCODE = 'DEFDEF'
        IF !EMPTY(&lcAdjTemp..cFacCode)
          LCLINKCODE = LFGETFACLK(LCLINKCODE)
        ENDIF
      ENDIF
      SELECT (LCADJTEMP)
      = RLOCK()
      REPLACE NSTEPS WITH 2
      UNLOCK
    ELSE
      lcLinkCode = IIF(SEEK('M'+&lcAdjTemp..ACCOUNT , "CUSTOMER") , IIF(!EMPTY(CUSTOMER.Link_Code) , CUSTOMER.Link_Code , "DEFDEF") , "DEFDEF")
      IF !EMPTY(&lcAdjTemp..cFacCode)
        LCLINKCODE = LFGETFACLK(LCLINKCODE)
      ENDIF
    ENDIF
    IF &lcAdjTemp..nSteps < 3
      SELECT (LCADJTEMP)
      IF LLUPDREP
        LCTRAN = ALLTRIM('T'+ALLTRIM(STR(RECNO(LCADJTEMP), 5)))
        SELECT (LCREPCBTMP)
        SCAN FOR ALLTRIM(TRAN)=LCTRAN
          = RLOCK()
          REPLACE TRAN WITH LCTRANNO, CCURRCODE WITH LCCURRCODE, NEXRATE WITH LNEXRATE, NCURRUNIT WITH LNCURRUNIT, DATE WITH LDBATCHDT, BATCH WITH LCBATCHNO
          UNLOCK
        ENDSCAN
      ENDIF
      SELECT (LCADJTEMP)
      = RLOCK()
      REPLACE NSTEPS WITH 3
      UNLOCK
    ENDIF
    IF &lcAdjTemp..nSteps < 4
      IF  .NOT. EMPTY(LCGLFYEAR) .AND.  .NOT. EMPTY(LCGLPERIOD) .AND. BETWEEN(VAL(LCGLPERIOD), 1, 13)
        = GFOPENFILE(GCDATADIR+'ARCUSHST',GCDATADIR+'ACTHST','SH')
        LCGLPERIOD = PADL(ALLTRIM(LCGLPERIOD), 2, '0')
        SELECT ARCUSHST
        = RLOCK()
        IF SEEK(&lcAdjTemp..Account + lcGLFYear , "ARCUSHST")
          IF MCHOICE='D'
            REPLACE nDrAdj            WITH nDrAdj + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)  nDrAdj&lcGLPeriod WITH nDrAdj&lcGLPeriod + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
          ELSE
            REPLACE nCrAdj            WITH nCrAdj + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)  nCrAdj&lcGLPeriod WITH nCrAdj&lcGLPeriod + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
          ENDIF
          UNLOCK
        ENDIF
        = GFCLOSEFILE('ARCUSHST')
      ENDIF
      SELECT (LCADJTEMP)
      = RLOCK()
      REPLACE NSTEPS WITH 4
      UNLOCK
    ENDIF
    IF &lcAdjTemp..nSteps < 5
      lcGLAcc = &lcAdjTemp..cAdjAcct
      IF llLink_AP AND !EMPTY(&lcAdjTemp..cBnkCode) AND !EMPTY(&lcAdjTemp..cChkAcct)
        SELECT APPAYMNT
        APPEND BLANK
        = RLOCK()
        REPLACE cPayType  WITH 'A'  cPayMeth  WITH 'A'  cBnkCode  WITH &lcAdjTemp..cBnkCode  cChkAcct  WITH &lcAdjTemp..cChkAcct  dPayDate  WITH ldBatchDt  cFisFYear WITH lcGLFYear  cFspprdid WITH lcGLPeriod  cPayDocNo WITH CUSTOMER.STORE  cPayClNo  WITH CUSTOMER.Account  cPayComp  WITH IIF(!EMPTY(&lcAdjTemp..REFERENCE),&lcAdjTemp..REFERENCE,CUSTOMER.BtName)  nPayAmnt  WITH &lcAdjTemp..Amount  cPayRecSt WITH 'O'   BATCH     WITH lcBatchNo  cCurrCode WITH lcCurrCode  nExRate   WITH lnExRate  nCurrUnit WITH lnCurrUnit
        = GFADD_INFO('APPAYMNT')
        UNLOCK
        lcGLAcc = IIF(SEEK(&lcAdjTemp..cBnkCode + &lcAdjTemp..cChkAcct ,  "APChecks") , APChecks.cChkGLAcc , lcGLAcc)
      ENDIF
      IF LLLINK_GL
        SELECT (LCADJTEMP)
        IF MCHOICE='C'
          DO GLDIST WITH 'DEFDEF','009',-(&lcAdjTemp..AMOUNT),  'CA',&lcAdjTemp..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp',  lcGLAcc,lcCurrCode,lnCurrUnit,lnExRate
          DO GLDIST WITH lcLinkCode,'001',&lcAdjTemp..AMOUNT,  'CA',&lcAdjTemp..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp',  '',lcCurrCode,lnCurrUnit,lnExRate
        ELSE
          DO GLDIST WITH '','010',-(&lcAdjTemp..AMOUNT),  'DA',&lcAdjTemp..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp',  lcGLAcc,lcCurrCode,lnCurrUnit,lnExRate
          DO GLDIST WITH lcLinkCode,'001',&lcAdjTemp..AMOUNT,  'DA',&lcAdjTemp..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp',  '',lcCurrCode,lnCurrUnit,lnExRate
        ENDIF
        SELECT (LCCURALIS)
        = RLOCK()
        REPLACE cArGLAcc WITH &lcGLTemp..GLAccount  cAdjAcct WITH lcGLAcc
        UNLOCK
        SELECT (LCADJTEMP)
        = RLOCK()
        REPLACE cArGLAcc WITH &lcGLTemp..GLAccount  cAdjAcct WITH lcGLAcc
        UNLOCK
      ENDIF
      SELECT (LCADJTEMP)
      = RLOCK()
      REPLACE NSTEPS WITH 5
      UNLOCK
    ENDIF
    IF LLEDIACCOU
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,KEY,TYPE,cPartner,cStatus,lInterComp) VALUES  ('812',&lcAdjTemp..TranType+&lcAdjTemp..TRAN,'A',EdiAcPrt.cPartner,'N',EdiAcPrt.lInterComp)
      = GFADD_INFO('EDITRANS')
    ENDIF
    SELECT (LCADJTEMP)
  ENDSCAN
  SET ORDER TO (LCADJTEMP)
  IF LLLINK_GL
    LCGLSESSIO = GFSEQUENCE('GLSESSION')
    SELECT (LCGLTEMP)
    REPLACE GLSESSION WITH LCGLSESSIO ALL
    USE
    *derby - Change the way of copying data from append to insert.[Start]
    *SELECT GLDIST
    *APPEND FROM &gcWorkDir.&lcGLTemp
    *derby - Change the way of copying data from append to insert.[End]
    = GFOPENFILE(GCWORKDIR+LCGLTEMP,'','EX')
    *derby - Change the way of copying data from append to insert.[Start]
    *C202169,1 ES 21/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[Begin]
    lnConn = Sqlstringconnect(OAriaApplication.ActiveCompanyConStr)
    *C202169,1 ES 21/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[END]
    SELECT &lcGLTemp
    GO TOP 
    SCAN
      SCATTER MEMVAR memo
      *C202169,1 ES 21/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[Begin]
      *INSERT INTO GLDIST FROM MEMVAR 
     
     IF lnConn  > 0
       lcstat =  "INSERT INTO GLDIST (glsession,glaccount,nglamount,tran_date,tran_no,tran_type,tran_desc,catg_key,"+;
                 "posted,printed,glbatch,ctrnsledn,nentryno,glcomp,glperiod,glfyear,glacnttype,ccurrcode,ncurrunit,"+;
                 "nexrate,neqvamnt,cglref,cadd_user,cadd_time,dadd_date,llok_stat,clok_user,dlok_date,clok_time,cowner,"+;
                 "cedit_user,dedit_date,cedit_time,cadd_ver,cedt_ver) VALUES (m.glsession,m.glaccount,m.nglamount,m.tran_date,"+;
                 "m.tran_no,m.tran_type,m.tran_desc,m.catg_key,m.posted,m.printed,m.glbatch,m.ctrnsledn,m.nentryno,m.glcomp,"+;
                 "m.glperiod,m.glfyear,m.glacnttype,m.ccurrcode,m.ncurrunit,m.nexrate,m.neqvamnt,m.cglref,m.cadd_user,m.cadd_time,"+;
                 "m.dadd_date,m.llok_stat,m.clok_user,m.dlok_date,m.clok_time,m.cowner,m.cedit_user,m.dedit_date,m.cedit_time,m.cadd_ver,m.cedt_ver)"
       lnRemResult = SQLExec(lnConn , lcstat)
       If lnRemResult < 0
         RETURN 
       ENDIF 
    ENDIF
    *C202169,1 ES 21/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
  
	ENDSCAN
	*derby - Change the way of copying data from append to insert.[End]
  ENDIF
  IF LLUPDREP .AND. LLREPCB
    SELECT (LCREPCBTMP)
    GOTO TOP
    LNCURREC = 1
    LNTOTREC = RECCOUNT(LCREPCBTMP)
    SCAN
      = GFTHERM(LNTOTREC,LNCURREC,'Updating the salesrep commission File.')
      LNCURREC = LNCURREC+1
      lnCurAmt    = ROUND(&lcRepCBTmp..AMOUNT &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
      lnRpFrnAmnt = &lcRepCBTmp..AMOUNT
      SELECT SALESREP
      IF SEEK(&lcRepCBTmp..REPCODE)
        IF &lcRepCBTmp..nSteps < 1
          LNNEWBAL = SALESREP.BALANCE+LNCURAMT
          = RLOCK()
          REPLACE CURRENT WITH CURRENT+LNCURAMT, BALANCE WITH LNNEWBAL
          UNLOCK
          SELECT (LCREPCBTMP)
          = RLOCK()
          REPLACE NSTEPS WITH 1
          UNLOCK
        ENDIF
        SELECT (LCREPCBTMP)
        IF &lcRepCBTmp..nSteps < 2
          = RLOCK()
          REPLACE BALANCE WITH LNNEWBAL, AMOUNT WITH LNCURAMT, NFORAMNT WITH LNRPFRNAMN
          REPLACE NSTEPS WITH 2
          UNLOCK
        ENDIF
      ENDIF
    ENDSCAN
    SELECT (LCREPCBTMP)
    USE
    SELECT REPCOMM
    APPEND FROM &gcWorkDir.&lcRepCBTmp
    = GFOPENFILE(GCWORKDIR+LCREPCBTMP,GCWORKDIR+LCREPCBTMP,'EX')
  ENDIF
  SELECT UNCMSESS
  IF LNUNCMSERC<>0 .AND. LNUNCMSERC<=RECCOUNT()
    GOTO LNUNCMSERC
    REPLACE STATUS WITH 'C'
    LLCONTINUE = .F.
    UNLOCK
  ENDIF
  LNUNCMSERC = 0
  IF  .NOT. LLFROMEDI
    LASCRMODE = 'S'
    LASCRMODE = .F.
    LASCRMODE[ 1] = .T.
    SHOW GETS
  ENDIF
  *
PROCEDURE LFUPDVARS
  LCFILES = 'lcAdjTemp,'+LCADJTEMP+','+LCADJTEMP+';'+IIF(MCHOICE='C', 'lcRepCBTmp,'+LCREPCBTMP+','+LCREPCBTMP+';', '')
  LLNOTHING = IIF(LNUNCMSERC=0, .T., GFSAVSESS(LCPROGID,LCFILES,@LAVARS,LCSESSION))
  *
PROCEDURE LFHGWUPDAT
  = RLOCK()
  REPLACE NHGWTRMARK WITH IIF(NETBAL>NHGWTRMARK, NETBAL, NHGWTRMARK)
  UNLOCK
  *
FUNCTION LFGETFACLK
  PARAMETER LCLINK
  PRIVATE LCLNKCOD
  IF  .NOT. LLOPENFAC
    LLOPENFAC = GFOPENFILE(GCDATADIR+'FACTOR',GCDATADIR+'FACTOR','SH')
  ENDIF
  IF SEEK(&lcAdjTemp..cfaccode,'FACTOR')
    LCLNKCOD = IIF( .NOT. EMPTY(FACTOR.LINK_CODE), FACTOR.LINK_CODE, LCLINK)
  ELSE
    LCLNKCOD = LCLINK
  ENDIF
  RETURN LCLNKCOD
  *
  ***
  *** ReFox - retrace your steps ...
  ***
