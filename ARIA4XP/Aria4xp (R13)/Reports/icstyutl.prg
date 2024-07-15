************************************************************************
*: Program file  : icStyUtl.PRG
*: Program desc. : Style Utilization Report
*:
*:         System: Aria4XP
*:      Developer: AYM - Ayman MAhmoud Ahmed
*:************************************************************************
*: Calls :
*:         Functions  :
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Modifications      :
*: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL 
*: B611940,1 MMT 07/15/2024 Fix the error when filter by date in style utilization report[T-ERP-20240710.0002]
*!*************************************************************
*!*  _screen.Visible = .T.
*!*  ACTIVATE WINDOW trace
*!*  SUSPEND
#INCLUDE R:\aria4xp\reports\icStyUtl.h
IF llOgFltCh
llDonprnt=.F.

lnDatePos  = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,'STYINVJL.DTRDATE'),1)
LDATE = SUBSTR(laOGVRFlt[lnDatePos,6],1,ATC('|',laOGVRFlt[lnDatePos,6])-1)
HDATE = SUBSTR(laOGVRFlt[lnDatePos,6],  ATC('|',laOGVRFlt[lnDatePos,6])+1)

lcPeriod=IIF(EMPTY(LDATE ),'','Period From : '+LDATE +'  To  '+ HDATE)
*: B611940,1 MMT 07/15/2024 Fix the error when filter by date in style utilization report[T-ERP-20240710.0002][Start]
LDATE = CTOD(LDATE)
HDATE = CTOD(HDATE)
*: B611940,1 MMT 07/15/2024 Fix the error when filter by date in style utilization report[T-ERP-20240710.0002][End]
lcSeek=' .T. '

*-- Style Filter
lcStyFltr= lfCheckFilter(3, 'STYLE.CSTYMAJOR')
llStyFltr   = !EMPTY(lcStyFltr) AND USED(lcStyFltr) AND RECCOUNT(lcStyFltr) > 0
IF llStyFltr
  SELECT (lcStyFltr)
  INDEX ON cstymajor TAG (lcStyFltr)
  lcSeek=lcSeek+" AND SEEK(STYLE.CSTYMAJOR,'"+lcStyFltr+"')"
ELSE
  IF TYPE("lcStyFltr") = "C" AND USED(lcStyFltr)
    USE IN (lcStyFltr)
  ENDIF
  lcStyFltr = ''
ENDIF



llMultiWH  = (ALLTRIM(UPPER(gfGetMemVar('M_WareHouse'))) = 'Y')
IF llMultiWH
  * Warehouse Filter
  lcWarFltr= lfCheckFilter(1, 'STYINVJL.CWARECODE')
  llWarFltr   = !EMPTY(lcWarFltr) AND USED(lcWarFltr) AND RECCOUNT(lcWarFltr) > 0
  IF llWarFltr
    SELECT (lcWarFltr)
    INDEX ON CWARECODE TAG (lcWarFltr)
    *: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL [Start]
    *lcSeek=lcSeek+" AND SEEK(CWARECODE,'"+lcWarFltr+"')"
    *: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL [End]
  ELSE
    IF TYPE("lcWarFltr") = "C" AND USED(lcWarFltr)
      USE IN (lcWarFltr)
    ENDIF
    lcWarFltr= ''
  ENDIF
ENDIF


* Check if there is a filter on Style CDIVISION
lcCurName = lfCheckFilter(3, 'STYLE.CDIVISION')
lcDiv   = loOgScroll.gfTempName()
llDiv   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcDiv ,"CDivision")
IF llDiv
  SELECT (lcDiv)
  INDEX on CDivision TAG (lcDiv)
  lcSeek=lcSeek+" AND SEEK(STYLE.CDIVISION,'"+lcDiv+"')"
ENDIF

* Check if there is a filter on Style SEASON
lcCurName = lfCheckFilter(3, 'STYLE.SEASON')
lcSea  = loOgScroll.gfTempName()
llSea   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcSea  ,"SEASON")
IF llSea
  SELECT (lcSea  )
  INDEX on SEASON TAG (lcSea  )
  lcSeek=lcSeek+" AND SEEK(STYLE.SEASON,'"+lcSea+"')"
ENDIF

* Check if there is a filter on Style Group
lcCurName = lfCheckFilter(3, 'STYLE.CSTYGROUP')
lcGrp  = loOgScroll.gfTempName()
llGrp   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcGrp  ,"Group")
IF llGrp
  SELECT (lcGrp  )
  INDEX on Group TAG (lcGrp  )
  lcSeek=lcSeek+" AND SEEK(STYLE.CSTYGROUP,'"+lcGrp+"')"
ENDIF

* Check if there is a filter on Style FABRIC
lcFAB  = lfCheckFilter(3, 'STYLE.FABRIC')
llFAB   = !EMPTY(lcFAB  )AND USED(lcFAB)  AND RECCOUNT(lcFAB  )<>0
IF llFAB
  SELECT (lcFAB  )
  INDEX on CSTYMAJOR TAG (lcFAB  )
  lcSeek=lcSeek+" AND SEEK(STYLE.FABRIC,'"+lcFAB+"')"
ENDIF



* Check if there is a filter on Style FABRIC
lcCurName = lfCheckFilter(3, 'STYLE.STATUS')
lcStatus= loOgScroll.gfTempName()
llStatus= !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcStatus,"STATUS")
IF llStatus
   SELECT (lcStatus)
  INDEX on STATUS TAG (lcStatus)
  lcSeek=lcSeek+" AND SEEK(STYLE.STATUS,'"+lcStatus+"')"
ENDIF


* Check if there is a filter on Style COLOR
lcCurName = lfCheckFilter(3, 'SUBSTR(STYLE.STYLE,lnNonMjrP,LEN(lcNonMjrM))')
lcCol  = loOgScroll.gfTempName()
llCol   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcCol  ,"Color")
IF llCol
  SELECT (lcCol  )
  INDEX on Color TAG (lcCol  )
  lcSeek=lcSeek+" AND SEEK(SUBSTR(STYLE.STYLE,lnNonMjrP,LEN(lcNonMjrM)),'"+lcCol  +"')"
ENDIF

lcCurName = lfCheckFilter(3, 'SUBSTR(STYLE.STYLE,lnFreePos,LEN(lcFreeMask))')
lcCol1  = loOgScroll.gfTempName()
llCol1   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcCol1  ,"Color")
IF llCol1
  SELECT (lcCol1  )
  INDEX on Color TAG (lcCol1  )
  lcSeek=lcSeek+" AND SEEK(SUBSTR(STYLE.STYLE,lnFreePos,LEN(lcFreeMask)),'"+lcCol1  +"')"
ENDIF



*Date Filter
IF !EMPTY(LDATE)
  *: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL [Start]
  *lcSeek=lcSeek +" AND  BETWEEN(DTRDATE,CTOD('"+LDATE +"'),CTOD('"+HDATE+"')) "
  *: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL [End]
ELSE
  IF  !EMPTY(HDATE)
    *: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL [Start]
    *lcSeek=lcSeek +" AND  DTRDATE<=CTOD('"+HDATE+"') "
    *: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL [End]
  ENDIF
ENDIF
* PATTERN Filter
lcPattern= lfCheckFilter(3, 'STYLE.PATTERN')
llPattern=IIF(EMPTY(lcPattern),.f.,.t.)
IF llPattern
  lcSeek=lcSeek +" AND STYLE.PATTERN='"+lcPattern+"'"
ENDIF


*: E303994,1 MMT 07/27/2018 CONVERT STYINVJL TO SQL[Start]
IF !USED('STYINVJL')
  =gfOpenTable('STYINVJL','STYINVJL','SH')
ENDIF
*: E303994,1 MMT 07/27/2018 CONVERT STYINVJL TO SQL[End]


*-- ApVendor Open With Modules AP,PS,PO,MA,MF
*-- PosHdr   Open With Modules PO,PS,RS
llApVendor = 'AP' $ oAriaApplication.CompanyInstalledModules OR ;
             'PS' $ oAriaApplication.CompanyInstalledModules OR ;
             'PO' $ oAriaApplication.CompanyInstalledModules OR ;
             'MA' $ oAriaApplication.CompanyInstalledModules OR ;
             'MF' $ oAriaApplication.CompanyInstalledModules
llApVendor = llApVendor AND gfOpentable(oAriaApplication.DataDir+'APVENDOR',;
            oAriaApplication.DataDir+'VENCODE','SH')

llInvHdr   = 'AR' $ oAriaApplication.CompanyInstalledModules AND ;
          gfOpentable(oAriaApplication.DataDir+'INVHDR',oAriaApplication.DataDir+'INVHDR','SH')

llPosHdr   = 'PO' $ oAriaApplication.CompanyInstalledModules OR ;
             'PS' $ oAriaApplication.CompanyInstalledModules OR ;
             'RS' $ oAriaApplication.CompanyInstalledModules

llPosHdr = llPosHdr AND gfOpentable(oAriaApplication.DataDir+'POSHDR',oAriaApplication.DataDir+'POSHDR','SH')

llRetHdr   = ('RM' $ oAriaApplication.CompanyInstalledModules OR 'PS' $ oAriaApplication.CompanyInstalledModules);
      AND gfOpenTable(oAriaApplication.DataDir+'RETHDR',oAriaApplication.DataDir+'RETHDR','SH')

  llMOPR='MF' $ oAriaApplication.CompanyInstalledModules AND gfOpentable(oAriaApplication.DataDir+'MFGOPRHD',;
       oAriaApplication.DataDir+'TKTOPER','SH')

lcStyUtil  = loOgScroll.gfTempName()
lcWorkfile = loOgScroll.gfTempName()

SELE STYLE
SET RELATION TO 'S'+Scale INTO Scale


=lfBuildTmp()
lcAccName = ' '
SELECT StyInvJL
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcStyUtil) FROM ARRAY laFileStru

INDEX ON Style+cSession+DTOS(dTRDate)+cTrCode TAG (lcStyUtil)
*: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL [Start]
*SELECT StyInvJL
*: E303994,1 MMT 07/27/2018 CONVERT STYINVJL TO SQL[Start]
*=gfSeek('','STYINVJL')
*: E303994,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL[End]
*SET RELATION TO STYLE INTO STYLE ADDITIVE
SELECT STYLE
*: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL [End]
SET FILTER TO &lcSeek
LOCATE

llAddTrn = .F.  && Flag to add Trnaction to lcStyUtil with Qty zero

* N000682 ,1 Thabet Handle globalization issues [Start]
*WAIT WINDOW "Preparing to collect data ... "  NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Prepare  NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Prepare,oAriaApplication.GetHeaderText("LANG_Prepare",AHEADERFILE))  NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [END]
DO WHILE !EOF()
  lcKey    = Style
  lnFstInv = 0
  ldTrDt   = {}
  *: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL [Start]
  SELECT STYINVJL
  =gfSeek(lcKey ,'STYINVJL','STYINVJL')
  *: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL [End]
  SCATTER MEMVAR BLANK
  llAddTrn = .F.
  *: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL [Start]
  *SCAN REST WHILE Style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = lcKey FOR !INLIST(cTrType,'4','8')
  SCAN REST WHILE Style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = lcKey FOR !INLIST(cTrType,'4','8') AND ;
       IIF(llMultiWH and llWarFltr,SEEK(CWARECODE,lcWarFltr),.T.);
       AND IIF(!EMPTY(LDATE),BETWEEN(DTRDATE,LDATE,HDATE),IIF(!EMPTY(HDATE),DTRDATE <= HDATE,.T.))
  *: E303994,1 SAH  05/17/2018 CONVERT STYINVJL TO SQL [End]
    IF llAddTrn .AND. Csession <> m.Csession
      IF m.Cirtype = 'R'
        SELECT (lcStyUtil)
        DELETE
        m.Csession = PADL(ALLTRIM(m.Csession),6,'0')
        m.Cirtype = 'I'
        m.Cisession = m.Csession
        STORE '' TO m.Crsession , m.Cicacnt
        APPEND BLANK
        GATHER MEMVAR
        SELECT STYINVJL
      ENDIF
      m.Csession = PADL(ALLTRIM(m.Csession),6,'0')
      m.Cirtype = 'R'
      m.Crsession = m.Csession
      STORE '' TO m.Cisession , m.Cicacnt
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        m.Nstk&lcCount = 0
      ENDFOR
      STORE 0 TO m.Ntotstk , Nstkval , Nprvsval , Lineno , Nprvsqty
      SELECT (lcStyUtil)
      APPEND BLANK
      GATHER MEMVAR
      llAddTrn = .F.
      SELECT STYINVJL
    ENDIF
    *Skip Voided Invoices
    IF cTrType='3' AND SEEK(StyInvJl.cTrCode,'INVHDR') AND INVHDR.STATUS='V'
      LOOP
    ENDIF
    *Skip Voided Credit Memos
    IF cTrType='7' AND GFSEEK(StyInvJl.cTrCode,'RETHDR') AND RETHDR.STATUS='V'
      LOOP
    ENDIF
    IF llRpPCnInv .AND. cTrType='3' .AND. dTrDate = ldTrDt
      SELECT (lcStyUtil)
      GOTO lnFstInv
      =RLOCK()
      REPLACE nStk1 WITH nStk1+STYINVJL.nStk1,;
              nStk2 WITH nStk2+STYINVJL.nStk2,;
              nStk3 WITH nStk3+STYINVJL.nStk3,;
              nStk4 WITH nStk4+STYINVJL.nStk4,;
              nStk5 WITH nStk5+STYINVJL.nStk5,;
              nStk6 WITH nStk6+STYINVJL.nStk6,;
              nStk7 WITH nStk7+STYINVJL.nStk7,;
              nStk8 WITH nStk8+STYINVJL.nStk8,;
            nTotStk WITH nTotStk+STYINVJL.nTotStk
      UNLOCK
    ELSE
      IF Cirtype = 'R' .AND. m.Cirtype = 'R' .AND. Csession = m.Csession
        SELECT (lcStyUtil)
        REPLACE Cirtype WITH 'I'
        SELECT STYINVJL
      ENDIF
      SCATTER MEMVAR
      m.Csession = PADL(ALLTRIM(m.Csession),6,'0')
      SELECT (lcStyUtil)
      APPEND BLANK
      GATHER MEMVAR
      IF cTrType = '2'
        llAddTrn = !llAddTrn
      ENDIF
    ENDIF
    IF llRpPCnInv .AND. &lcStyUtil..cTrType='3' .AND. &lcStyUtil..dTrDate <> ldTrDt
      lnFstInv= RECNO(lcStyUtil)
      ldTrDt  = &lcStyUtil..dTrDate
    ENDIF
  ENDSCAN
ENDDO
WAIT CLEAR
IF llAddTrn
      IF m.Cirtype = 'R'
        SELECT (lcStyUtil)
        DELETE
        m.Csession = PADL(ALLTRIM(m.Csession),6,'0')
        m.Cirtype = 'I'
        m.Cisession = m.Csession
        STORE '' TO m.Crsession , m.Cicacnt
        APPEND BLANK
        GATHER MEMVAR
        SELECT STYINVJL
      ENDIF
      m.Csession = PADL(ALLTRIM(m.Csession),6,'0')
      m.Cirtype = 'R'
      m.Crsession = m.Csession
      STORE '' TO m.Cisession , m.Cicacnt
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        m.Nstk&lcCount = 0
      ENDFOR
      STORE 0 TO m.Ntotstk , Nstkval , Nprvsval , Lineno , Nprvsqty
      SELECT (lcStyUtil)
      APPEND BLANK
      GATHER MEMVAR
      llAddTrn = .F.
      SELECT STYINVJL
ENDIF

SELECT StyInvJL
SET FILTER TO

SELECT (lcStyUtil)
LOCATE
lcKey=Style
LLFIRST=.T.
SCAN REST WHILE Style+cSession+DTOS(dTRDate)+cTrCode=''
  IF Style<>lcKey OR LLFIRST
    LLFIRST=.F.
    lcKey=Style

    * N000682 ,1 Thabet Handle globalization issues [Start]
    *WAIT WINDOW "Collecting Data for Style : " +  lcKey NOWAIT
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW lANG_Collecting_Data +  lcKey NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",lANG_Collecting_Data,oAriaApplication.GetHeaderText("lANG_Collecting_Data",AHEADERFILE)) +  lcKey NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

    * N000682 ,1 Thabet Handle globalization issues [END]
    *--Calculating the previous balance for each style.
    STORE 0 TO M.PQTY1,M.PQTY2,M.PQTY3,M.PQTY5,M.PQTY4,M.PQTY6,M.PQTY7,M.PQTY8,lnNewBal
    lnNewBal=lfGetNBal()
    SELE STYLE
    =SEEK(lcKey)
    lcStyle = STYLE
    M.STYLE=lcStyle
    M.DESC=STYLE.DESC
    M.SEASON=STYLE.SEASON
    M.cDivision=STYLE.cDivision
    M.PATTERN=STYLE.PATTERN
    M.cStyGroup=STYLE.cStyGroup
    M.SCALE=STYLE.SCALE
    M.FABRIC=STYLE.FABRIC
    FOR lnCount = 1 TO 8
      lcCount=STR(lnCount,1)
      M.SZ&lcCount=PADL(ALLTRIM(SCALE.SZ&lcCount),5,' ')
    ENDFOR
  ENDIF
  SELECT (lcStyUtil)
    *--Initialize needed variables.
    FOR lnCount=1 TO 8
      lcCount=STR(lnCount,1)
      M.nStk&lcCount=nStk&lcCount
    ENDFOR
    M.nTotStk=nTotStk
    M.dTrDate=dTrDate
    M.cTrCode=cTrCode
    M.LINENO=LINENO
    M.CIRTYPE=CIRTYPE
    M.CTRTYPE=CTRTYPE
    M.cSession=cSession

    STORE '' TO lcAcctvn,lcReason,lcAccName ,lcTrTpDsc
    =lpTrTp(CTRTYPE)
    M.CTRDESC=lcTrTpDsc
    M.ACCVN= lcAcctvn
    M.REASON= lcReason
    M.customer=lcAccName

   INSERT INTO (LCWORKFILE) FROM MEMVAR
ENDSCAN
WAIT CLEAR

SELECT (lcWorkfile )
IF !RECCOUNT()>0
  llDonprnt=.T.
  *-- Message : There are no records to display...!
  *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    

  RETURN
ENDIF

INDEX ON Style+cSession+DTOS(DTRDate)+cTrCode TAG (lcWorkfile )

=lfAdjustCRSettings()
IF USED(lcWorkfile )
    USE IN (lcWorkfile )
ENDIF

=gfDispRe()
ELSE
  IF llDonprnt
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE
    =gfDispRe()
  ENDIF
ENDIF  &&FILTER CHANGE


*!*************************************************************
*! Name      : lpTrTp
*! Developer : AYM
*! Date      : 07/25/2006
*! Purpose   : Get special fields from master files wich not found in
*!             Style inventory journal .
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcTrType : Transaction Type
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpTrTp()
*!*************************************************************
FUNCTION  lpTrTp
LPARAMETERS LCTRTYPE
DO CASE
  CASE LCTRTYPE $ '1I'
    lcTrTpDsc ='INVENTORY ADJ.'
    lcReason  = &lcStyUtil..REFERENCE
  CASE LCTRTYPE='2'
    lcTrTpDsc='PHYSICAL INV.'
    lcReason = &lcStyUtil..REFERENCE
  CASE LCTRTYPE='3'
    =SEEK(&lcStyUtil..cTrCode,'INVHDR')
    lcTrTpDsc = IIF(!llRpPCnInv,'SHIPPED','TOTAL SHIP')
    lcAcctvn  = INVHDR.Account
    lcAccName = IIF(SEEK('M'+lcAcctvn,'Customer'),Customer.BtName,' ')
  CASE LCTRTYPE $'56'
    lcTrTpDsc='RECEIVED'
    IF &lcStyUtil..cTrType='5'
      IF llMOPR AND gfseek('M'+&lcStyUtil..cTrCode,'MFGOPRHD')
        SKIP IN 'MFGOPRHD'
      ENDIF
      IF llMOPR AND MFGOPRHD.cIMTyp+MFGOPRHD.cTktNo='M'+&lcStyUtil..cTrCode
        lcAcctvn  = MFGOPRHD.cContCode
        lcAccName = MFGOPRHD.cContName
      ELSE
        STORE '' TO lcAcctvn,lcAccName
      ENDIF
    ELSE
      IF llPosHdr AND gfseek('PP'+&lcStyUtil..cTrCode,'POSHDR')
        lcAcctvn  = POSHDR.Vendor
      ELSE
        STORE '' TO lcAcctvn
      ENDIF
      IF llApVendor
        lcAccName = IIF(SEEK(lcAcctvn,'ApVendor'),ApVendor.cVenComp,' ')
      ELSE
        STORE '' TO lcAccName
      ENDIF

    ENDIF
    lcReason = REFERENCE
  CASE LCTRTYPE='7'
    lcTrTpDsc='RETURNED'
    =GFSEEK(&lcStyUtil..cTrCode,'RETHDR')
    lcAcctvn  = RETHDR.Account
    lcAccName = IIF(SEEK('M'+lcAcctvn,'Customer'),Customer.BtName,' ')
  CASE LCTRTYPE='9'
    lcTrTpDsc='INV. MARKDOWN'
    lnPrvAlias  = SELECT(0)
    IF SEEK(&lcStyUtil..Style,'MDINVNTL')
      SELECT MDINVNTL
      SCAN REST WHILE Style+Color+Dyelot+Clocation+cBattype+Clkbatch = &lcStyUtil..Style + SPACE(6) + &lcStyUtil..Cdyelot
        IF SEEK(cBatType+clkBatch,'MDINVNTH') .AND. &lcStyUtil..dTrDate = MDINVNTH.Date
          lcReason = MDINVNTL.CReason
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
    SELECT(lnPrvAlias)
ENDCASE




*!*************************************************************
*! Name      : lfGetNBal
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : Calculating the previous balance for each style.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetNBal()
*!*************************************************************
FUNCTION lfGetNBal

SELECT STYINVJL
*E303994,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
*=SEEK(lcKey)
=GFSEEK(lcKey)
*E303994,1 SAH CONVERT STYINVJL TO SQL [END]
lnNewBal=0
*B611940,1 MMT 07/15/2024 Fix the error when filter by date in style utilization report[T-ERP-20240710.0002][Start]
*SCAN REST WHILE Style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = lcKey FOR IIF(EMPTY(lDATE),.F., DTRDate<CTOD(lDATE))
SCAN REST WHILE Style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = lcKey FOR IIF(EMPTY(lDATE),.F., DTRDate<lDATE)
*B611940,1 MMT 07/15/2024 Fix the error when filter by date in style utilization report[T-ERP-20240710.0002][End]
      M.PQTY1=M.PQTY1+NStk1
      M.PQTY2=M.PQTY2+NStk2
      M.PQTY3=M.PQTY3+NStk3
      M.PQTY4=M.PQTY4+NStk4
      M.PQTY5=M.PQTY5+NStk5
      M.PQTY6=M.PQTY6+NStk6
      M.PQTY7=M.PQTY7+NStk7
      M.PQTY8=M.PQTY8+NStk8
      lnNewBal=lnNewBal+NTotStk
ENDSCAN
RETURN(lnNewBal)

*!*************************************************************
*! Name      : lfvstyle
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : Validate Optional Grid selected Styles
*!*************************************************************
*! Calls     : gfStyBrw
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvstyle()
*!*************************************************************
FUNCTION lfvstyle
PRIVATE lcStyle
lcStyle = SYS(18)
IF !EMPTY(&lcStyle) AND !SEEK(&lcStyle,'Style')
  &lcStyle = gfStyBrw("M" , "" , "" , .F.)
ENDIF


*!*************************************************************
*! Name      : lfRptWhen
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : Report When Function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfRptWhen()
*!*************************************************************
FUNCTION lfRptWhen
lcMajPic = "@! " + gfItemMask("PM")

*B604014,1 ABD Get the potion of the field. [Begin]
lnLocPos   = lfItmPos('STYINVJL.CWARECODE')
*B604014,1 ABD [End]

*!*************************************************************
*! Name      : lfClrStat
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : Get Information about Item Segment
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClrStat()
*!*************************************************************
FUNCTION lfClrStat
PRIVATE laItemSeg,lnMajSeg,lcFreeSep

STORE '' TO lcNonMjr,lcNonMjrH,lcNonMjrM,lcFreeSeg,lcFreeHdr,lcFreeMask,lcFreeSep
STORE 0  TO lnNonMjrP,lnFreePos
DECLARE laItemSeg[1]
lnMajSeg = gfItemMask('SM')  && No. of major segments.
=gfItemMask(@laItemSeg)
FOR lnCount = lnMajSeg +1 TO ALEN(laItemSeg,1)
  DO CASE
     CASE laItemSeg[lnCount,1]='C'
       lcNonMjr  = 'C'
       lcNonMjrH = laItemSeg[lnCount,2]
       lcNonMjrM = laItemSeg[lnCount,3]
       lnNonMjrP = laItemSeg[lnCount,4]
       STORE '' TO lcFreeSeg,lcFreeHdr,lcFreeMask
       STORE 0  TO lnFreePos
       EXIT
     CASE lcFreeSeg='F' AND laItemSeg[lnCount,1] <> 'F'
     CASE laItemSeg[lnCount,1]='F'
       lcFreeSeg  = 'F'
       lcFreeHdr = lcFreeHdr+lcFreeSep+laItemSeg[lnCount,2]
       lcFreeMask= lcFreeMask+lcFreeSep+laItemSeg[lnCount,3]
       lnFreePos = IIF(lnFreePos=0,laItemSeg[lnCount,4],lnFreePos)
       lcFreeSep = laItemSeg[lnCount,6]
  ENDCASE
ENDFOR
*B802402,1 - WAB - Add function for order the table when press button in range
*B802402,1 		   at option  grad
*B802402,1 - WAB - Start
*!**************************************************************************
*! Name      : lfSetSTY
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/25/1999
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetSty()
*!**************************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SET ORDER TO TAG CSTYLE IN STYLE

   *B605608,1 BWA 03/31/2002 Fix the bug of locating in another file not the style file.[START]
   *GO TOP
   GO TOP IN STYLE
   *B605608,1 BWA 03/31/2002.[END]

  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE
*B802402,1 - WAB - END
*!*************************************************************
*! Name      : lfWOldVal
*! Developer : Abdou ElGendi - (ABD)
*! Date      : 11/13/2000
*! Purpose   : Get any get field old value.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Called from : Option Grid, any Get Field.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWOldVal()
*!*************************************************************
*B604014,1
FUNCTION lfWOldVal
laOldVal = EVALUATE(SYS(18))


*!*************************************************************
*! Name      : lfItmPos
*! Developer : Abdou ElGendi - (ABD)
*! Date      : 11/13/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : lnItmPos.
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*B604014,1
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.
*!*************************************************************


*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[3,2]

loOgScroll.lcOGLastForm ='ICSTYUTL'
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkfile + ".DBF"


loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= 'Utilization'



loOgScroll.laCRParams[2,1] = 'LCBRSZ'
IF llRpPrntSz
  loOgScroll.laCRParams[2,2] = 'Y'
ELSE
  loOgScroll.laCRParams[2,2] = 'N'
ENDIF
loOgScroll.laCRParams[3,1] = 'lcPeriod'
loOgScroll.laCRParams[3,2] = lcPeriod



*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   :
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[47,18] ,laTempCOM[1,18],laTempLINE[1,18],laTempHDR[1,18]
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTempCOM,laTempLINE,laTempHDR
lcExcStat = SET('EXACT')
SET EXACT ON
SELECT STYLE
=OGAFIELDS(@laTempCOM)
laTempStru[1,1]  = 'STYLE'
laTempStru[2,1]  = 'DESC'
laTempStru[3,1]  = 'SEASON'
laTempStru[4,1]  = 'CDIVISION'
laTempStru[5,1]  = 'PATTERN'
laTempStru[6,1]  = 'SCALE'
laTempStru[7,1]  = 'CSTYGROUP'
laTempStru[8,1]  = 'FABRIC'

*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 1 TO 8
  lnFldRow = ASCAN(laTempCOM,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempCOM,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempCOM[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempCOM[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempCOM[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

SELECT SCALE
=OGAFIELDS(@laTempLINE)
laTempStru[9,1]  = 'SZ1'
laTempStru[10,1] = 'SZ2'
laTempStru[11,1] = 'SZ3'
laTempStru[12,1]  = 'SZ4'
laTempStru[13,1]  = 'SZ5'
laTempStru[14,1]  = 'SZ6'
laTempStru[15,1]  = 'SZ7'
laTempStru[16,1]  = 'SZ8'

*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 9 TO 16
  lnFldRow = ASCAN(laTempLINE,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempLINE,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempLINE[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempLINE[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempLINE[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

SELECT STYINVJL
=OGAFIELDS(@laTempHDR)
laTempStru[17,1]  = 'DTRDATE'
laTempStru[18,1]  = 'CWARECODE'
laTempStru[19,1]  = 'CTRCODE'
laTempStru[20,1]  = 'LINENO'
laTempStru[21,1]  = 'CSESSION'


*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 17 TO 21
  lnFldRow = ASCAN(laTempHDR,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempHDR,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempHDR[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempHDR[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempHDR[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)




laTempStru[22,1] = 'NSTK1'
laTempStru[22,2] = 'N'
laTempStru[22,3] = 10
laTempStru[22,4] = 2

laTempStru[23,1] = 'NSTK2'
laTempStru[23,2] = 'N'
laTempStru[23,3] = 10
laTempStru[23,4] = 2

laTempStru[24,1] = 'NSTK3'
laTempStru[24,2] = 'N'
laTempStru[24,3] = 10
laTempStru[24,4] = 2

laTempStru[25,1] = 'NSTK4'
laTempStru[25,2] = 'N'
laTempStru[25,3] = 10
laTempStru[25,4] = 2

laTempStru[26,1] = 'NSTK5'
laTempStru[26,2] = 'N'
laTempStru[26,3] = 10
laTempStru[26,4] = 2

laTempStru[27,1] = 'NSTK6'
laTempStru[27,2] = 'N'
laTempStru[27,3] = 10
laTempStru[27,4] = 2

laTempStru[28,1] = 'NSTK7'
laTempStru[28,2] = 'N'
laTempStru[28,3] = 10
laTempStru[28,4] = 2

laTempStru[29,1] = 'NSTK8'
laTempStru[29,2] = 'N'
laTempStru[29,3] = 10
laTempStru[29,4] = 2

laTempStru[30,1] = 'NTOTSTK'
laTempStru[30,2] = 'N'
laTempStru[30,3] = 10
laTempStru[30,4] = 2


laTempStru[31,1] = 'CUSTOMER'
laTempStru[31,2] = 'C'
laTempStru[31,3] = 30
laTempStru[31,4] = 0

laTempStru[32,1] = 'ACCVN'
laTempStru[32,2] = 'C'
laTempStru[32,3] = 8
laTempStru[32,4] = 0

laTempStru[33,1] = 'VALUE'
laTempStru[33,2] = 'N'
laTempStru[33,3] = 10
laTempStru[33,4] = 2

laTempStru[34,1] = 'BALANCE'
laTempStru[34,2] = 'N'
laTempStru[34,3] = 10
laTempStru[34,4] = 2

laTempStru[35,1] = 'NEWBALANCE'
laTempStru[35,2] = 'N'
laTempStru[35,3] = 10
laTempStru[35,4] = 2

laTempStru[36,1] = 'REASON'
laTempStru[36,2] = 'C'
laTempStru[36,3] = 30
laTempStru[36,4] = 0

laTempStru[37,1] = 'CTRTYPE'
laTempStru[37,2] = 'C'
laTempStru[37,3] = 1
laTempStru[37,4] = 0

laTempStru[38,1] = 'CIRTYPE'
laTempStru[38,2] = 'C'
laTempStru[38,3] = 1
laTempStru[38,4] = 0

laTempStru[39,1] = 'CTRDESC'
laTempStru[39,2] = 'C'
laTempStru[39,3] = 20
laTempStru[39,4] = 0

laTempStru[40,1] = 'PQTY1'
laTempStru[40,2] = 'N'
laTempStru[40,3] = 10
laTempStru[40,4] = 2

laTempStru[41,1] = 'PQTY2'
laTempStru[41,2] = 'N'
laTempStru[41,3] = 10
laTempStru[41,4] = 2

laTempStru[42,1] = 'PQTY3'
laTempStru[42,2] = 'N'
laTempStru[42,3] = 10
laTempStru[42,4] = 2

laTempStru[43,1] = 'PQTY4'
laTempStru[43,2] = 'N'
laTempStru[43,3] = 10
laTempStru[43,4] = 2

laTempStru[44,1] = 'PQTY5'
laTempStru[44,2] = 'N'
laTempStru[44,3] = 10
laTempStru[44,4] = 2

laTempStru[45,1] = 'PQTY6'
laTempStru[45,2] = 'N'
laTempStru[45,3] = 10
laTempStru[45,4] = 2

laTempStru[46,1] = 'PQTY7'
laTempStru[46,2] = 'N'
laTempStru[46,3] = 10
laTempStru[46,4] = 2

laTempStru[47,1] = 'PQTY8'
laTempStru[47,2] = 'N'
laTempStru[47,3] = 10
laTempStru[47,4] = 2

=gfCrtTmp(lcWorkfile ,@laTempstru,,"",.f.)
SET EXACT &lcExcStat

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 3
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn


*!*************************************************************
*! Name      : lfStr2Curs
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString

