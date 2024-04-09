*:***************************************************************************
*: Program file  : ARSLBMTH.PRG
*: Program desc. : Custom Sales By Month For DCC              
*: For Report    : ARSLBMTH.FRX 
*: System        : ARIA4XP.
*: Module        : Account Rec. (AR)
*: Developer     : Mostafa Eid (MOS)
*: Date          : 01/06/2009
*: Tracking NO   : C201095
*:***************************************************************************
*: Modifications:
*: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[T20110215.0010]
*: B609755,1 MMT 11/28/2011 Sales by Month Report suppressing style details on the excel[T20110728.0001]
*: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[T20120304.0012]
*:***************************************************************************
*---- paper Size
 loogscroll.ccrpapersize = 'A4'
*---- Check if Date Ranges Entered ---* 
 IF  .NOT. lfvsperiod()
    RETURN
 ENDIF
 IF  .NOT. lfvfperiod()
    RETURN
 ENDIF
 FOR lni = 1 TO 12
    lci = ALLTRIM(STR(lni))
    STORE 0 TO lngqty&lci, lngval&lci, lnqty&lci , lnval&lci
    *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
    STORE 0 TO lnCurqty&lci, lnCurval&lci
    *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
 ENDFOR
 STORE 0 TO lntotqty, lntotval, lngtotqty, lngtotval, lntotamnt, lngtotamnt
*: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
 STORE 0 TO lnCurtotqty, lnCurtotval,lnCurtotamnt
*: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
 llendrep = .F.
 lcstyexpr = ""
 llrpsrtbst = (ATC('INVLINE.STORE', lcrpexp)<>0)
 STORE "" TO lcnonmajpi, lcoldval
 lcstymjr = gfitemmask('PM')
 lnstylngth = LEN(lcstymjr)
 IF loogscroll.llogfltch
    IF  .NOT. USED(lctempfile) .OR. (RECCOUNT(lctempfile)>0)
       = lfcreattmp()
    ENDIF
    = lfcollect()
 ENDIF
 *-- Asking if no records (Display message) otherwise print report
 IF RECCOUNT(lctempfile)=0
    *---Text : 'No Record Selected for the report..!'
    = gfmodalgen('TRM00052B00000', 'DIALOG')
    RETURN
 ENDIF
 
 *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[Start]
 SELECT(lctempfile)
 lcOldOrd = ORDER()
 SET ORDER TO 'TmpIndex'
 lcKey = SPACE(1)
 SCAN 
   lnCurRec = RECNO()
   *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
   *IF account+store+style <>  lcKey    
   IF ccurrcode+account+store+style <>  lcKey 
   *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
     REPLACE crepeted WITH 'A'
   ELSE
     REPLACE crepeted WITH 'Z'
   ENDIF 
   *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
   *lcKey = account+store+style   
   lcKey = ccurrcode+account+store+style
   *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
 ENDSCAN
 SET ORDER TO (lcOldOrd)
 LOCATE 
 *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[End]

 IF loogscroll.ctextreptype="EXCEL"
    = lfexportexcel()
 ELSE
    SELECT (lctempfile)
    DO gfdispre WITH EVALUATE('lcFormName')
 ENDIF
 loogscroll.ctextreptype = ""

**
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : MOSTAFA Eid (mos)
*! Date      : 01/06/2009
*! Purpose   : WHEN FUNCTION FOR THE REPORT
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfOpenFile
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
** Open tables 
 = gfopentable(oariaapplication.datadir+'RETHDR', 'RETHDRA', 'SH')
 = gfopentable(oariaapplication.datadir+'CUSTOMER', 'CUSTOMER', 'SH')
 = gfopentable(oariaapplication.datadir+'CUSTOMER', 'CUSTOMER', 'SH', 'CUST')
 = gfopentable(oariaapplication.datadir+'RETLINE', 'RETLINE', 'SH')
 = gfopentable(oariaapplication.datadir+'INVHDR', 'INVHDRA', 'SH')
 = gfopentable(oariaapplication.datadir+'STYLE', 'CSTYLE ', 'SH')
 = gfopentable(oariaapplication.datadir+'CONSINVL', 'CONSINVL', 'SH')
 = gfopentable(oariaapplication.datadir+'INVLINE', 'INVLINE', 'SH')
 IF USED(lctempstor) .AND. RECCOUNT(lctempstor)>0
    USE IN (lctempstor)
 ENDIF
 IF  .NOT. USED(lctempstor)
    lni = 1
    DIMENSION latempstru3[lni, 4]
    latempstru3[lni, 1] = 'Store'
    latempstru3[lni, 2] = 'C'
    latempstru3[lni, 3] = 8
    latempstru3[lni, 4] = 0
    lni = ALEN(latempstru3, 1)+1
    DIMENSION latempstru3[lni, 4]
    latempstru3[lni, 1] = 'BTNAME'
    latempstru3[lni, 2] = 'C'
    latempstru3[lni, 3] = 30
    latempstru3[lni, 4] = 0
    lni = ALEN(latempstru3, 1)+1
    DIMENSION latempstru3[lni, 4]
    latempstru3[lni, 1] = 'PHONE1'
    latempstru3[lni, 2] = 'C'
    latempstru3[lni, 3] = 16
    latempstru3[lni, 4] = 0
    lni = ALEN(latempstru3, 1)+1
    DIMENSION latempstru3[lni, 4]
    latempstru3[lni, 1] = 'CADDRESS6'
    latempstru3[lni, 2] = 'C'
    latempstru3[lni, 3] = 20
    latempstru3[lni, 4] = 0
    lni = ALEN(latempstru3, 1)+1
    DIMENSION latempstru3[lni, 4]
    latempstru3[lni, 1] = 'NETBAL '
    latempstru3[lni, 2] = 'N'
    latempstru3[lni, 3] = 14
    latempstru3[lni, 4] = 2
    lni = ALEN(latempstru3, 1)+1
    DIMENSION latempstru3[lni, 4]
    latempstru3[lni, 1] = 'Account'
    latempstru3[lni, 2] = 'C'
    latempstru3[lni, 3] = 5
    latempstru3[lni, 4] = 0
    = gfcrttmp(lctempstor, @latempstru3, 'Account+Store', lctempstor)
    SELECT (lctempstor)
    INDEX ON store+account TAG 'StorAcc'
 ENDIF
 = lfgetstore()
**
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Mostafa Eid (MOS)
*! Date      : 01/13/2009
*! Purpose   : Create temp cursors.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfOpenFile
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCreatTmp()
*!*************************************************************

FUNCTION lfCreatTmp
 IF USED(lctempfile) .AND. RECCOUNT(lctempfile)>0
    USE IN (lctempfile)
 ENDIF
 IF  .NOT. USED(lctempfile)
    lni = 1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Account'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 5
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'AccName'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 30
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Store'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'StoreNam'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 15
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'STYLE'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = lnstylngth
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Desc'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 30
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Price'
    latempstru[lni, 2] = 'N'
    *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[Start]
    *latempstru[lni, 3] = 6
    latempstru[lni, 3] = 12
    *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[END]
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Qty1'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Qty2'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Qty3'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Qty4'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Qty5'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Qty6'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Qty7'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Qty8'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Qty9'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Qty10'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Qty11'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Qty12'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'TotQty'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 9
    latempstru[lni, 4] = 2
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'Type'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 1
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'cRepeted'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 1
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'llEnd'
    latempstru[lni, 2] = 'L'
    latempstru[lni, 3] = 1
    latempstru[lni, 4] = 0
    
    *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'ccurrcode'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 3
    latempstru[lni, 4] = 0
    *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
    
    = gfcrttmp(lctempfile, @latempstru)
    SELECT (lctempfile)
    *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[Start]
    *INDEX ON account+store+style+crepeted+STR(price, 6)+type TAG lctempfile    
    *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
*!*	    INDEX ON account+store+style+crepeted+STR(price, 12, 2)+type TAG lctempfile
*!*	    INDEX ON account+store+style+STR(price, 12, 2)+type TAG 'TmpIndex' ADDITIVE 
*!*	    *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[End]
*!*	    INDEX ON account+store+style+crepeted TAG display ADDITIVE
    INDEX ON ccurrcode+account+store+style+crepeted+STR(price, 12, 2)+type TAG lctempfile
    INDEX ON ccurrcode+account+store+style+STR(price, 12, 2)+type TAG 'TmpIndex' ADDITIVE 
    INDEX ON ccurrcode+account+store+style+crepeted TAG display ADDITIVE
    *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
    SET ORDER TO lcTempFile
 ENDIF
 ********************************************************************************
 IF USED(lctempretl) .AND. RECCOUNT(lctempretl)>0
    USE IN (lctempretl)
 ENDIF
 IF  .NOT. USED(lctempretl)
    lni = 1
    DIMENSION latempstru2[lni, 4]
    latempstru2[lni, 1] = 'crmemo'
    latempstru2[lni, 2] = 'C'
    latempstru2[lni, 3] = 6
    latempstru2[lni, 4] = 0
    lni = ALEN(latempstru2, 1)+1
    DIMENSION latempstru2[lni, 4]
    latempstru2[lni, 1] = 'cret_linno'
    latempstru2[lni, 2] = 'C'
    latempstru2[lni, 3] = 4
    latempstru2[lni, 4] = 0
    lni = ALEN(latempstru2, 1)+1
    DIMENSION latempstru2[lni, 4]
    latempstru2[lni, 1] = 'STYLE'
    latempstru2[lni, 2] = 'C'
    latempstru2[lni, 3] = lnstylngth
    latempstru2[lni, 4] = 0
    = gfcrttmp(lctempretl, @latempstru2)
    SELECT (lctempretl)
    INDEX ON crmemo+style+cret_linno TAG (lctempretl)
    SET ORDER TO lcTempRetL
 ENDIF


*------ End Of lfCreatTmp 
*!*************************************************************
*! Name      : lfCollect
*! Developer : Mostafa Eid (HBG)
*! Date      : 01/13/2009
*! Purpose   : Collect Data.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfOpenFile
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCollect()
*!*************************************************************
**
FUNCTION lfCollect
 SELECT (lctempretl)
 ZAP
 *--Update period array
 = lfupdperid()
 SELECT customer
 lnaccpos = ASCAN(loogscroll.laogfxflt, 'INVLINE.ACCOUNT')
 IF lnaccpos>0
    lnaccpos = ASUBSCRIPT(loogscroll.laogfxflt, lnaccpos, 1)
 ENDIF
 IF USED(loogscroll.laogfxflt(lnaccpos, 6))
    GOTO TOP IN loogscroll.laogfxflt(lnaccpos, 6)
    llrpacc =  .NOT. EOF(loogscroll.laogfxflt(lnaccpos, 6))
 ELSE
    llrpacc = .F.
 ENDIF
*: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
llCurrSelect = .F.
lcCurrencyCursor  = ''
lnPosCurr = ASCAN(loOgScroll.laOgFXFlt,"INVHDR.CCURRCODE")
IF lnPosCurr > 0 
  lnPosCurr  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosCurr ,1)
  lcCurrencies= loOgScroll.laOgFxFlt[lnPosCurr ,6]
  IF !EMPTY(lcCurrencies)
    llCurrSelect = .T.
    lcCurrencyCursor  = loOgScroll.gfTempName()
    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1]='CCURRCODE'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0
    =gfCrtTmp(lcCurrencyCursor,@laTempacstru,"CCURRCODE",lcCurrencyCursor,.T.)
    lnStart=1
    lnEnd=AT('|',lcCurrencies)
    DO WHILE lnEnd <> 0
      SELECT(lcCurrencyCursor) 
      APPEND BLANK 
      REPLACE CCURRCODE WITH SUBSTR(lcCurrencies,lnStart,lnEnd-1)
      lcCurrencies= STUFF(lcCurrencies,lnStart,lnEnd,"") 
      lnEnd=AT('|',lcCurrencies)
    ENDDO 
    IF lnEnd = 0
      SELECT(lcCurrencyCursor) 
      APPEND BLANK 
      REPLACE CCURRCODE WITH lcCurrencies
    ENDIF 
  ENDIF  
ENDIF
*: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
  SELECT invhdr
 = gfsetorder('INVHDRA')
 *--------------------
 IF llrpacc
    lnaccpos = ASCAN(loogscroll.laogfxflt, 'INVLINE.ACCOUNT')
    IF lnaccpos>0
       lnaccpos = ASUBSCRIPT(loogscroll.laogfxflt, lnaccpos, 1)
       IF  .NOT. EMPTY(laogfxflt(lnaccpos, 6)) .AND. USED(laogfxflt(lnaccpos, 6))
          lcfxacc = (loogscroll.laogfxflt(lnaccpos, 6))
          SELECT (lcfxacc)
          SCAN
             =gfseek('M'+&lcfxacc..account,'Customer')
             lcaccname = customer.btname
             IF gfseek(account, 'INVHDR')
                SELECT invhdr
                *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
                *SCAN REST FOR status<>'V' WHILE account+invoice=customer.account                
                SCAN REST FOR status<>'V' AND IIF(llCurrSelect,SEEK(CCURRCODE,lcCurrencyCursor),.T.) WHILE account+invoice=customer.account
                *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                   WAIT WINDOW NOWAIT 'Collect Data for invoice : '+invoice
                   IF invhdr.consol='Y'
                      lcscnexpr = "invoice+store+order+style+STR(lineno,6) = INVHDR.Invoice"
                      lcinvline = 'CONSINVL'
                      lcstrexpr = '.T.'
                      lnstrpos = ATC('INLIST(INVLINE.STORE', lcrpexp)
                      IF lnstrpos=0
                         lnstrpos = ATC('BETWEEN(INVLINE.STORE', lcrpexp)
                      ENDIF
                      lnstart = IIF(ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)', lcrpexp)<>0, ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)', lcrpexp), LEN(lcrpexp))
                      IF lnstrpos<>0
                         lcleftexpr = SUBSTR(lcrpexp, lnstrpos, lnstart-lnstrpos+IIF(lnstart=LEN(lcrpexp), 1, 0))
                      ELSE
                         lcleftexpr = '.T.'
                      ENDIF
                      lcleftexpr = STRTRAN(lcleftexpr, 'INVLINE.style', 'Style')
                      lccrstrepr = STRTRAN(lcleftexpr, 'INVLINE', 'RETHDR')
                      IF USED(laogfxflt(3, 6))
                         lcrightexpr = 'SEEK(SUBSTR(INVLINE.Style,1,lnStyLngth),laOGFxFlt[3,6])'
                      ELSE
                         lcrightexpr = '.T.'
                      ENDIF
                      lcrightexpr = STRTRAN(lcrightexpr, 'INVLINE.style', 'STYLE')
                      lcrepexpr = lcleftexpr+' AND '+lcrightexpr
                      lcrepexpr = STRTRAN(lcrepexpr, 'INVLINE', 'CONSINVL')
                   ELSE
                      lcscnexpr = "invoice+STR(lineno,6) = INVHDR.Invoice"
                      lcinvline = 'INVLINE'
                      lnstrpos = ATC('INLIST(INVLINE.STORE', lcrpexp)
                      IF lnstrpos=0
                         lnstrpos = ATC('BETWEEN(INVLINE.STORE', lcrpexp)
                      ENDIF
                      lnstart = IIF(ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)', lcrpexp)<>0, ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)', lcrpexp), LEN(lcrpexp))
                      IF lnstrpos<>0
                         lcstrexpr = SUBSTR(lcrpexp, lnstrpos, lnstart-lnstrpos+IIF(lnstart=LEN(lcrpexp), 1, 0))
                         lcstrexpr = STRTRAN(lcstrexpr, 'INVLINE', 'INVLINE')
                      ELSE
                         lcstrexpr = '.T.'
                      ENDIF
                      IF USED(laogfxflt(3, 6)) .AND. RECCOUNT(laogfxflt(3, 6))>0
                         lcrepexpr = 'SEEK(SUBSTR(INVLINE.Style,1,lnStyLngth),laOGFxFlt[3,6])'
                      ELSE
                         lcrepexpr = '.T.'
                      ENDIF
                      lcstrexpr = STRTRAN(lcstrexpr, 'INVLINE.style', 'STYLE')
                      lcrepexpr = STRTRAN(lcrepexpr, 'INVLINE.style', 'STYLE')
                      lccrstrepr = STRTRAN(lcstrexpr, 'INVLINE', 'RETHDR')
                   ENDIF
                   IF gfseek(invoice, lcinvline)
                      SELECT (lcinvline)
                      SCAN REST WHILE &lcscnexpr FOR &lcrepexpr
                         IF SEEK(customer.account,lctempstor) AND !(&lcstrexpr)
                            LOOP
                         ENDIF
                         lcpriod = STR(MONTH(invdate), 2)+'/'+STR(YEAR(invdate), 4)
                         lnpriod = ASCAN(laperiod, lcpriod)
                         IF lnpriod>0
                            lnpriod = ASUBSCRIPT(laperiod, lnpriod, 1)
                            lcprd = ALLTRIM(STR(lnpriod))
                            = gfseek('S'+account+store, 'CUST')
                            lcstyle = SUBSTR(&lcinvline..STYLE,1,lnstylngth)  
                            = gfseek(lcstyle, 'STYLE')
                            lcdesc = style.desc
			                *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[Start]                            
*!*	                            IF SEEK(&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle+'A'+STR(price,6),lctempfile);
*!*	                               OR SEEK(&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle+'Z'+STR(price,6),lctempfile)
                            *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
*!*	                            IF SEEK(&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle+'A'+STR(price,12,2),lctempfile);
*!*	                               OR SEEK(&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle+'Z'+STR(price,12,2),lctempfile)
                            IF SEEK(IIF(lcRpCurr <> "F" ,gcBaseCurr,invhdr.ccurrcode)+&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle+'A'+STR(IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,price,gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")),12,2),lctempfile);
                               OR SEEK(IIF(lcRpCurr <> "F" ,gcBaseCurr,invhdr.ccurrcode)+&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle+'Z'+STR(IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,price,gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")),12,2),lctempfile)
                            *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                            *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[End]
                               SELECT (lctempfile)
                               REPLACE qty&lcprd   WITH qty&lcprd + &lcinvline..totqty ,;
                               	    totqty      WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8+qty9+qty10+qty11+qty12
                               *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[Start]        
                               *IF lcrptype='B' .AND. SEEK(account+IIF(llrpsrtbst, store, SPACE(8))+lcstyle+'Z'+STR(price, 6)+'V', lctempfile)       
                               *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]                                            
                               *IF lcrptype='B' .AND. SEEK(account+IIF(llrpsrtbst, store, SPACE(8))+lcstyle+'Z'+STR(price, 12, 2)+'V', lctempfile)                               
                               IF lcrptype='B' .AND. SEEK(IIF(lcRpCurr <> "F" ,gcBaseCurr,invhdr.ccurrcode)+account+IIF(llrpsrtbst, store, SPACE(8))+lcstyle+'Z'+STR(IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,price,gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")), 12, 2)+'V', lctempfile)
                               *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                               *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[End]  
                                  *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]                          
                                  *REPLACE qty&lcprd   WITH qty&lcprd + (&lcinvline..totqty*&lcinvline..price) , totqty      WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8+qty9+qty10+qty11+qty12                                  
                                  REPLACE qty&lcprd   WITH qty&lcprd + (&lcinvline..totqty*IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,&lcinvline..price,gfAmntDisp(&lcinvline..price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR"))) , totqty      WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8+qty9+qty10+qty11+qty12
                                  *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                               ENDIF
                            ELSE
                               *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
                               *lcrepeted = IIF(!SEEK(&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle,lctempfile),'A','Z')                               
                               lcrepeted = IIF(!SEEK(IIF(lcRpCurr <> "F" ,gcBaseCurr,invhdr.ccurrcode)+&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle,lctempfile),'A','Z')
                               *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                               lcstorenam = cust.store
                               *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
                               *INSERT INTO (lctempfile) (account,accname,STORE,storenam,STYLE,DESC,price,qty&lcprd,totqty,TYPE,crepeted,llend )  VALUES (&lcinvline..account,lcaccname,IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8)),lcstorenam,lcstyle,lcdesc,&lcinvline..price, &lcinvline..totqty,&lcinvline..totqty,'Q',lcrepeted,.F.)                                        
                               INSERT INTO (lctempfile) (ccurrcode,account,accname,STORE,storenam,STYLE,DESC,price,qty&lcprd,totqty,TYPE,crepeted,llend)  VALUES (IIF(lcRpCurr <> "F" ,gcBaseCurr,invhdr.ccurrcode),&lcinvline..account,lcaccname,IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8)),lcstorenam,lcstyle,lcdesc,IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,&lcinvline..price,gfAmntDisp(&lcinvline..price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")), &lcinvline..totqty,&lcinvline..totqty,'Q',lcrepeted,.F.)         
                               *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                               IF lcrptype='B'
                                  *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
                                  *INSERT INTO (lctempfile) (account,accname,STORE,storenam,STYLE,DESC,price,qty&lcprd,totqty,TYPE,crepeted,llend )  VALUES (&lcinvline..account,lcaccname,IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8)),lcstorenam,lcstyle,lcdesc,&lcinvline..price, &lcinvline..totqty*&lcinvline..price,&lcinvline..totqty*&lcinvline..price,'V','Z',.F.)                                           
                                  INSERT INTO (lctempfile) (account,accname,STORE,storenam,STYLE,DESC,price,qty&lcprd,totqty,TYPE,crepeted,llend,ccurrcode)  VALUES (&lcinvline..account,lcaccname,IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8)),lcstorenam,lcstyle,lcdesc,IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,&lcinvline..price,gfAmntDisp(&lcinvline..price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")), &lcinvline..totqty*IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,&lcinvline..price,gfAmntDisp(&lcinvline..price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")),&lcinvline..totqty*IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,&lcinvline..price,gfAmntDisp(&lcinvline..price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")),'V','Z',.F.,IIF(lcRpCurr <> "F" ,gcBaseCurr,invhdr.ccurrcode))         
                                  *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                               ENDIF
                            ENDIF
                            SELECT (lcinvline)
                         ENDIF
                      ENDSCAN
                   ENDIF
                ENDSCAN
             ENDIF
             = lfretcalc(customer.account)
          ENDSCAN
       ENDIF
    ENDIF
 ELSE
    SELECT customer
    IF gfseek('M')
       SCAN FOR type+account+store='M'
          lcaccname = customer.btname
          IF gfseek(account, 'INVHDR')
             SELECT invhdr
             *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start] 
             *SCAN REST FOR status<>'V' WHILE account+invoice=customer.account             
             SCAN REST FOR status<>'V' AND IIF(llCurrSelect,SEEK(CCURRCODE,lcCurrencyCursor),.T.) WHILE account+invoice=customer.account
             *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                WAIT WINDOW NOWAIT 'Collect Data for invoice : '+invoice
                IF invhdr.consol='Y'
                   lcscnexpr = "invoice+store+order+style+STR(lineno,6) = INVHDR.Invoice"
                   lcinvline = 'CONSINVL'
                   lcstrexpr = '.T.'
                   lnstrpos = ATC('INLIST(INVLINE.STORE', lcrpexp)
                   IF lnstrpos=0
                      lnstrpos = ATC('BETWEEN(INVLINE.STORE', lcrpexp)
                   ENDIF
                   lnstart = IIF(ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)', lcrpexp)<>0, ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)', lcrpexp), LEN(lcrpexp))
                   IF lnstrpos<>0
                      lcleftexpr = SUBSTR(lcrpexp, lnstrpos, lnstart-lnstrpos+IIF(lnstart=LEN(lcrpexp), 1, 0))
                   ELSE
                      lcleftexpr = '.T.'
                   ENDIF
                   lccrstrepr = STRTRAN(lcleftexpr, 'INVLINE', 'RETHDR')
                   IF USED(laogfxflt(3, 6))
                      lcrightexpr = 'SEEK(SUBSTR(INVLINE.Style,1,lnStyLngth),laOGFxFlt[3,6])'
                   ELSE
                      lcrightexpr = '.T.'
                   ENDIF
                   lcrepexpr = lcleftexpr+' AND '+lcrightexpr
                   lcrepexpr = STRTRAN(lcrepexpr, 'INVLINE', 'CONSINVL')
                ELSE
                   lcscnexpr = "invoice+STR(lineno,6) = INVHDR.Invoice"
                   lcinvline = 'INVLINE'
                   lnstrpos = ATC('INLIST(INVLINE.STORE', lcrpexp)
                   IF lnstrpos=0
                      lnstrpos = ATC('BETWEEN(INVLINE.STORE', lcrpexp)
                   ENDIF
                   lnstart = IIF(ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)', lcrpexp)<>0, ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)', lcrpexp), LEN(lcrpexp))
                   IF lnstrpos<>0
                      lcstrexpr = SUBSTR(lcrpexp, lnstrpos, lnstart-lnstrpos+IIF(lnstart=LEN(lcrpexp), 1, 0))
                      lcstrexpr = STRTRAN(lcstrexpr, 'INVLINE.', '')
                   ELSE
                      lcstrexpr = '.T.'
                   ENDIF
                   IF USED(laogfxflt(3, 6)) .AND. RECCOUNT(laogfxflt(3, 6))>0
                      lcrepexpr = 'SEEK(SUBSTR(INVLINE.Style,1,lnStyLngth),laOGFxFlt[3,6])'
                   ELSE
                      lcrepexpr = '.T.'
                   ENDIF
                   lcstrexpr = STRTRAN(lcstrexpr, 'INVLINE.', '')
                   lccrstrepr = STRTRAN(lcstrexpr, 'INVLINE', 'RETHDR')
                ENDIF
                IF gfseek(invoice, lcinvline)
                   SELECT (lcinvline)
                   SCAN REST WHILE &lcscnexpr FOR &lcrepexpr
                      IF SEEK(customer.account,lctempstor) AND !(&lcstrexpr)
                         LOOP
                      ENDIF
                      lcpriod = STR(MONTH(invdate), 2)+'/'+STR(YEAR(invdate), 4)
                      lnpriod = ASCAN(laperiod, lcpriod)
                      IF lnpriod>0
                         lnpriod = ASUBSCRIPT(laperiod, lnpriod, 1)
                         lcprd = ALLTRIM(STR(lnpriod))
                         = gfseek('S'+account+store, 'CUST')
                         lcstyle = SUBSTR(&lcinvline..STYLE,1,lnstylngth)  
                         = gfseek(lcstyle, 'STYLE')
                         lcdesc = style.desc
                         *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[Start]        
                         *IF SEEK(&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle+'A'+STR(price,6),lctempfile) OR SEEK(&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle+'Z'+STR(price,6),lctempfile)                         
                         *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start] 
*!*	                         IF SEEK(&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle+'A'+STR(price,12,2),lctempfile) OR;
*!*	                            SEEK(&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle+'Z'+STR(price,12,2),lctempfile)
						 *
                         IF SEEK(IIF(lcRpCurr <> "F" ,gcBaseCurr,invhdr.ccurrcode)+ &lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle+'A'+STR(IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,price,gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")),12,2),lctempfile) OR;
                            SEEK(IIF(lcRpCurr <> "F" ,gcBaseCurr,invhdr.ccurrcode)+ &lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle+'Z'+STR(IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,price,gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")),12,2),lctempfile)
                         *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]   
                         *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[End]        
                            SELECT (lctempfile)
                            REPLACE qty&lcprd   WITH qty&lcprd + &lcinvline..totqty , totqty      WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8+qty9+qty10+qty11+qty12
                            *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[Start]        
                            *IF lcrptype='B' .AND. SEEK(account+IIF(llrpsrtbst, store, SPACE(8))+lcstyle+'Z'+STR(price,6)+'V', lctempfile)                            
                            *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
                            *IF lcrptype='B' .AND. SEEK(account+IIF(llrpsrtbst, store, SPACE(8))+lcstyle+'Z'+STR(price, 12, 2)+'V', lctempfile)                            
                            IF lcrptype='B' .AND. SEEK(IIF(lcRpCurr <> "F" ,gcBaseCurr,invhdr.ccurrcode)+account+IIF(llrpsrtbst, store, SPACE(8))+lcstyle+'Z'+STR(IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,price,gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")), 12, 2)+'V', lctempfile)
                            *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                            *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[End]   
                               *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]     
                               *REPLACE qty&lcprd   WITH qty&lcprd + (&lcinvline..totqty*&lcinvline..price) , totqty      WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8+qty9+qty10+qty11+qty12                            
                               REPLACE qty&lcprd   WITH qty&lcprd + (&lcinvline..totqty*IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,&lcinvline..price,gfAmntDisp(&lcinvline..price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR"))) ,;
                                       totqty      WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8+qty9+qty10+qty11+qty12
                               *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                            ENDIF
                         ELSE
                            *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
                            *lcrepeted = IIF(!SEEK(&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle,lctempfile),'A','Z')                            
                            lcrepeted = IIF(!SEEK(IIF(lcRpCurr <> "F" ,gcBaseCurr,invhdr.ccurrcode)+&lcinvline..account+IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8))+lcstyle,lctempfile),'A','Z')
                            *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                            lcstorenam = cust.store
                            *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
                            *INSERT INTO (lctempfile) (account,accname,STORE,storenam,STYLE,DESC,price,qty&lcprd,totqty,TYPE,crepeted,llend )  VALUES (&lcinvline..account,lcaccname,IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8)),lcstorenam,lcstyle,lcdesc,&lcinvline..price, &lcinvline..totqty,&lcinvline..totqty,'Q',lcrepeted,.F.)                                     
                            INSERT INTO (lctempfile) (ccurrcode,account,accname,STORE,storenam,STYLE,DESC,price,qty&lcprd,totqty,TYPE,crepeted,llend )  VALUES (IIF(lcRpCurr <> "F" ,gcBaseCurr,invhdr.ccurrcode),&lcinvline..account,lcaccname,IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8)),lcstorenam,lcstyle,lcdesc,IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,&lcinvline..price,gfAmntDisp(&lcinvline..price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")), &lcinvline..totqty,&lcinvline..totqty,'Q',lcrepeted,.F.)         
                            *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                            IF lcrptype='B'
                              *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
                              *INSERT INTO (lctempfile) (account,accname,STORE,storenam,STYLE,DESC,price,qty&lcprd,totqty,TYPE,crepeted,llend )  VALUES (&lcinvline..account,lcaccname,IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8)),lcstorenam,lcstyle,lcdesc,&lcinvline..price, &lcinvline..totqty*&lcinvline..price,&lcinvline..totqty*&lcinvline..price,'V','Z',.F.)                                       
                              INSERT INTO (lctempfile) (account,accname,STORE,storenam,STYLE,DESC,price,qty&lcprd,totqty,TYPE,crepeted,llend,ccurrcode)  VALUES (&lcinvline..account,lcaccname,IIF(llrpsrtbst,&lcinvline..STORE,SPACE(8)),lcstorenam,lcstyle,lcdesc,&lcinvline..price, &lcinvline..totqty*IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,&lcinvline..price,gfAmntDisp(&lcinvline..price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")),&lcinvline..totqty*IIF(lcRpCurr = "F" OR INVHdr.cCurrCode=gcBaseCurr,&lcinvline..price,gfAmntDisp(&lcinvline..price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"INVHDR")),'V','Z',.F.,IIF(lcRpCurr <> "F" ,gcBaseCurr,invhdr.ccurrcode))         
                              *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]  
                            ENDIF
                         ENDIF
                         SELECT (lcinvline)
                      ENDIF
                   ENDSCAN
                ENDIF
             ENDSCAN
          ENDIF
          = lfretcalc(customer.account)
       ENDSCAN
    ENDIF
 ENDIF
 SELECT (lctempfile)
 GOTO BOTTOM
 REPLACE llend WITH .T.

**
*!*************************************************************
*! Name      : lfStySum
*! Developer : mostafa eid (mos)
*! Date      : 01/06/2009
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
FUNCTION lfStySum
 PARAMETER lcsty, lccomp, lnaddtovar
 PRIVATE lnstyrec
 lntotcomp = 0
 IF RECCOUNT('STYLE')<>0
    lnstyrec = RECNO('STYLE')
    SELECT style_x
    SUM &lccomp TO lntotcomp WHILE ALLTRIM(cstymajor) = ALLTRIM(lcsty)
    SELECT style
    IF BETWEEN(lnstyrec, 1, RECCOUNT())
       GOTO lnstyrec
    ENDIF
    DO CASE
       CASE lnaddtovar=1
          lno_t_s = lntotcomp
       CASE lnaddtovar=2
          lno_t_s = lno_t_s+lntotcomp
       CASE lnaddtovar=3
          lno_t_s = lno_t_s-lntotcomp
    ENDCASE
 ENDIF
 RETURN INT(lntotcomp)
ENDFUNC
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfSrSty
*! Developer : mostafa eid (mos)
*! Date      : 01/06/2009
*! Purpose   : Style In Range Filter. 2
*!*************************************************************
**
FUNCTION lfSrSty
 PARAMETER lcparm
 DO CASE
    CASE lcparm='S' && Set code
       *-- open this file in another alias to set order to Style Major 
       *-- unique index.
       = gfopentable(oariaapplication.datadir+'Style', 'Style', 'SH', 'STYLE_X')
       SELECT style
       SET ORDER TO Cstyle
       SET RELATION TO style.style INTO style_x
       GOTO TOP IN style
    CASE lcparm='R' && Reset code
       = gfclosetable('STYLE_X')
       SELECT style
       SET ORDER TO STYLE
 ENDCASE

**
*!**************************************************************************
*! Name      : lfwOldVal
*! Developer : mostafa eid(mos)
*! Date      : 01/07/2009
*! Purpose   : Save any Get Object old value
*!**************************************************************************
*! Example   : =lfwOldVal()
*!**************************************************************************
*!
FUNCTION lfwOldVal
 lcoldval = EVALUATE(SYS(18))

*-- End of lfwOldVal.

*!**************************************************************************
*! Name      : lfStorEnab
*! Developer : mostafa eid (mos)
*! Date      : 01/06/2009
*! Purpose   : To Disable or Enable the Store In list
*!**************************************************************************
*! Example   : = lfDeptEnab()
*!**************************************************************************

**
FUNCTION lfStorEnab
 lnstorpos = ASCAN(loogscroll.laogfxflt, 'INVLINE.STORE')
 IF lnstorpos>0
    lnstorpos = ASUBSCRIPT(loogscroll.laogfxflt, lnstorpos, 1)
    laogobjcnt[ALEN(laogobjcnt, 1)-ALEN(loogscroll.laogfxflt, 1)+lnstorpos] = llstorstat
    = lfogshowget('LAOGFXFLT['+ALLTRIM(STR(lnstorpos))+',6]')
    IF USED(loogscroll.laogfxflt(lnstorpos, 6)) .AND.  .NOT. llstorstat
       lcalias = ALIAS()
       SELECT loogscroll.laogfxflt(lnstorpos, 6)
       ZAP
       USE
       SELECT (lcalias)
    ENDIF
 ENDIF

**
*!*************************************************************
*! Name      : lfSrSty
*! Developer : mostafa eid (mos)
*! Date      : 01/08/2009
*! Purpose   : Store In Range Filter.
*!*************************************************************
FUNCTION lfSrStr
 PARAMETER lcparm
 DO CASE
    CASE lcparm='S' && Set code
      *= lfGetStore()     
       SET ORDER IN (lctempstor) TO 'StorAcc'
    CASE lcparm='R' && Reset code
       SELECT customer
       SET FILTER TO
       SET ORDER IN (lctempstor) TO (lctempstor)
 ENDCASE

**
*!**************************************************************************
*! Name      : lfvSPeriod
*! Developer : mostafa eid (mos)
*! Date      : 01/06/2009
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************

FUNCTION lfvSPeriod
 IF  .NOT. EMPTY(lcrpspriod)
    lcsmonth = SUBSTR(lcrpspriod, 1, 2)
    lcsmonth = IIF(EMPTY(lcsmonth), '0', lcsmonth)
    lcsyear = SUBSTR(lcrpspriod, 4, 4)
    lcsyear = IIF(EMPTY(lcsyear), '0', lcsyear)
    DIMENSION laperiod[12, 2]
    IF EVALUATE(lcsmonth)<1 .OR. EVALUATE(lcsmonth)>12
       = gfmodalgen('TRM00000B00000', 'DIALOG', .F., .F., 'Please enter a valid Month.')
       STORE "" TO lcrpspriod, lcsmonth, lcsyear
       RETURN .F.
    ENDIF
    IF LEN(ALLTRIM(lcsyear))<4
       = gfmodalgen('TRM00000B00000', 'DIALOG', .F., .F., 'Please enter a valid Year.')
       STORE "" TO lcrpspriod, lcsmonth, lcsyear
       RETURN .F.
    ENDIF
 ENDIF
ENDFUNC
**
*!**************************************************************************
*! Name      : lfGetMnth
*! Developer : mostafa eid (mos)
*! Date      : 01/08/2009
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************

FUNCTION lfGetMnth
 PARAMETER lnmonth
 DO CASE
    CASE lnmonth=1
       RETURN 'Jan'
    CASE lnmonth=2
       RETURN 'Feb'
    CASE lnmonth=3
       RETURN 'Mar'
    CASE lnmonth=4
       RETURN 'Apr'
    CASE lnmonth=5
       RETURN 'May'
    CASE lnmonth=6
       RETURN 'Jun'
    CASE lnmonth=7
       RETURN 'Jul'
    CASE lnmonth=8
       RETURN 'Aug'
    CASE lnmonth=9
       RETURN 'Sep'
    CASE lnmonth=10
       RETURN 'Oct'
    CASE lnmonth=11
       RETURN 'Nov'
    CASE lnmonth=12
       RETURN 'Dec'
 ENDCASE
ENDFUNC
**
*!**************************************************************************
*! Name      : lfvSPeriod
*! Developer : mostafa eid (mos)
*! Date      : 01/09/2009
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************

FUNCTION lfvFPeriod
 IF  .NOT. EMPTY(lcrpfpriod)
    lcfmonth = SUBSTR(lcrpfpriod, 1, 2)
    lcfmonth = IIF(EMPTY(lcfmonth), '0', lcfmonth)
    lcfyear = SUBSTR(lcrpfpriod, 4, 4)
    lcfyear = IIF(EMPTY(lcfyear), '0', lcfyear)
    IF EVALUATE(lcfmonth)<1 .OR. EVALUATE(lcfmonth)>12
       = gfmodalgen('TRM00000B00000', 'DIALOG', .F., .F., 'Please enter a valid Month.')
       STORE "" TO lcfmonth, lcfyear
       RETURN .F.
    ENDIF
    IF LEN(lcfyear)<4
       = gfmodalgen('TRM00000B00000', 'DIALOG', .F., .F., 'Please enter a valid Year.')
       STORE "" TO lcfmonth, lcfyear
       RETURN .F.
    ENDIF
    IF EVALUATE(lcfyear)<EVALUATE(lcsyear)
       = gfmodalgen('TRM00000B00000', 'DIALOG', .F., .F., "End Year can't be befor start Year.")
       STORE "" TO lcfmonth, lcfyear
       RETURN .F.
    ENDIF
    IF EVALUATE(lcfyear)=EVALUATE(lcsyear) .AND. EVALUATE(lcfmonth)<EVALUATE(lcsmonth)
       = gfmodalgen('TRM00000B00000', 'DIALOG', .F., .F., "End Month can't be befor start Month.")
       STORE "" TO lcfmonth, lcfyear
       RETURN .F.
    ENDIF
    IF EVALUATE(lcfyear)-EVALUATE(lcsyear)>1
       = gfmodalgen('TRM00000B00000', 'DIALOG', .F., .F., "End Month Year can't be more than 12 months later than start Month Year.")
       STORE "" TO lcfmonth, lcfyear
       RETURN .F.
    ENDIF
    IF EVALUATE(lcfyear)>EVALUATE(lcsyear)
       lnremstrt = 12-EVALUATE(lcsmonth)
       lnmnthnum = EVALUATE(lcfmonth)+lnremstrt
       IF lnmnthnum>12
          = gfmodalgen('TRM00000B00000', 'DIALOG', .F., .F., "End Month Year can't be more than 12 months later than start Month Year.")
          STORE "" TO lcfmonth, lcfyear
          RETURN .F.
       ENDIF
    ENDIF
 ENDIF
ENDFUNC
**
*!**************************************************************************
*! Name      : lfMakeVald
*! Developer : mostafa eid (mos)
*! Date      : 01/09/2009
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************

FUNCTION lfMakeVald
 IF EMPTY(lcrpspriod)
    = gfmodalgen('TRM00000B00000', 'DIALOG', .F., .F., "Start Month Year Can't be empty.")
    RETURN .F.
 ENDIF
ENDFUNC
**
*!**************************************************************************
*! Name      : lfStStrVar
*! Developer : mostafa eid (mos)
*! Date      : 01/09/2009
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************

FUNCTION lfStStrVar
 FOR lni = 1 TO 12
    lci = ALLTRIM(STR(lni))
    STORE 0 TO lnqty&lci , lnval&lci
 ENDFOR
 STORE 0 TO lntotqty, lntotval, lntotamnt

**
*!**************************************************************************
*! Name      : lfStAccVar
*! Developer : mostafa eid (mos)
*! Date      : 01/10/2009
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************

FUNCTION lfStAccVar
 FOR lni = 1 TO 12
    lci = ALLTRIM(STR(lni))
    STORE 0 TO lngqty&lci, lngval&lci
 ENDFOR
 STORE 0 TO lngtotqty, lngtotval, lngtotamnt

**
*!*************************************************************
*! Name      : lfUpdPerid
*! Developer : mostafa eid (mos)
*! Date      : 01/10/2009
*! Purpose   : Update period array
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfOpenFile
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpdPerid()
*!*************************************************************

FUNCTION lfUpdPerid
 lcsmth = STR(EVALUATE(lcsmonth)-1, 2)
 lcsyr = lcsyear
 FOR lni = 1 TO 12
    lcsmth = STR(EVALUATE(lcsmth)+1, 2)
    IF EVALUATE(lcsmth)>12
       lcsmth = ' 1'
       lcsyr = STR(EVALUATE(lcsyr)+1, 4)
    ENDIF
    laperiod[lni, 1] = lcsmth+"/"+lcsyr
    lcmth = lfgetmnth(EVALUATE(lcsmth))
    laperiod[lni, 2] = lcmth+SUBSTR(lcsyr, 3, 2)
 ENDFOR
 lcfmth = STR(EVALUATE(lcfmonth), 2)
 lcfyr = lcfyear
 IF TYPE('laPeriod[1,1]')='C'
    lcvalue = lcfmth+"/"+lcfyr
    lnendpos = ASCAN(laperiod, lcvalue)
    IF lnendpos>0
       lnendpos = ASUBSCRIPT(laperiod, lnendpos, 1)
       FOR lni = lnendpos+1 TO ALEN(laperiod, 1)
          STORE "" TO laperiod[lni, 1]
       ENDFOR
    ENDIF
 ENDIF

**
*!*************************************************************
*! Name      : lfRetCalc
*! Developer : mostafa eid (mos)
*! Date      : 01/10/2009
*! Purpose   : Calculate returned
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfOpenFile
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpdPerid()
*!*************************************************************

FUNCTION lfRetCalc
 PARAMETER lcaccount
 lcscnexpr = "invoice+STR(lineno,6) = INVHDR.Invoice"
 lcinvline = 'INVLINE'
 lnstrpos = ATC('INLIST(INVLINE.STORE', lcrpexp)
 IF lnstrpos=0
    lnstrpos = ATC('BETWEEN(INVLINE.STORE', lcrpexp)
 ENDIF
 lnstart = IIF(ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)', lcrpexp)<>0, ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)', lcrpexp), LEN(lcrpexp))
 IF lnstrpos<>0
    lcstrexpr = SUBSTR(lcrpexp, lnstrpos, lnstart-lnstrpos+IIF(lnstart=LEN(lcrpexp), 1, 0))
    lcstrexpr = STRTRAN(lcstrexpr, 'INVLINE', 'RETHDR')
 ELSE
    lcstrexpr = '.T.'
 ENDIF
 IF USED(loogscroll.laogfxflt(3, 6)) .AND. RECCOUNT(loogscroll.laogfxflt(3, 6))>0
    lcrepexpr = 'SEEK(SUBSTR(RETLINE.Style,1,lnStyLngth),loogscroll.LAOGFXFLT[3,6])'
 ELSE
    lcrepexpr = '.T.'
 ENDIF
 IF gfseek(lcaccount, 'RETHDR')
    SELECT rethdr
    *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
    *SCAN REST FOR status<>'V' WHILE account+crmemo=lcaccount    
    SCAN REST FOR status<>'V' AND IIF(llCurrSelect,SEEK(CCURRCODE,lcCurrencyCursor),.T.) WHILE account+crmemo=lcaccount
    *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
       IF gfseek(rethdr.crmemo, 'RETLINE')
          SELECT retline
          SCAN REST WHILE crmemo+STYLE+cret_linno+cret_trncd = rethdr.crmemo FOR &lcrepexpr 
             IF SEEK(customer.account,lctempstor) AND !(&lccrstrepr)
                LOOP
             ENDIF
             lcpriod = STR(MONTH(retline.crdate), 2)+'/'+STR(YEAR(retline.crdate), 4)
             lnpriod = ASCAN(laperiod, lcpriod)
             IF lnpriod>0
                lnpriod = ASUBSCRIPT(laperiod, lnpriod, 1)
                lcprd = ALLTRIM(STR(lnpriod))
                = gfseek('S'+account+rethdr.store, 'CUST')
                lcstyle = SUBSTR(retline.style, 1, lnstylngth)
                = gfseek(lcstyle, 'STYLE')
                lcdesc = style.desc
                IF  .NOT. gfseek(rethdr.crmemo+lcstyle+retline.cret_linno, lctempretl)
                   *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[Start]
                   *IF SEEK(lcaccount+IIF(llrpsrtbst, rethdr.store, SPACE(8))+lcstyle+'A'+STR(retline.price,6), lctempfile) .OR. SEEK(lcaccount+IIF(llrpsrtbst, rethdr.store, SPACE(8))+lcstyle+'Z'+STR(retline.price,6), lctempfile)                   
                   *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
*!*	                   IF SEEK(lcaccount+IIF(llrpsrtbst, rethdr.store, SPACE(8))+lcstyle+'A'+STR(retline.price, 12, 2), lctempfile) .OR.;
*!*	                      SEEK(lcaccount+IIF(llrpsrtbst, rethdr.store, SPACE(8))+lcstyle+'Z'+STR(retline.price, 12, 2), lctempfile)
                   IF SEEK(IIF(lcRpCurr <> "F" ,gcBaseCurr,RETHDR.ccurrcode)+lcaccount+IIF(llrpsrtbst, rethdr.store, SPACE(8))+lcstyle+'A'+STR(IIF(lcRpCurr = "F" OR RETHdr.cCurrCode=gcBaseCurr,retline.price,gfAmntDisp(retline.price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")), 12, 2), lctempfile) .OR.;
                      SEEK(IIF(lcRpCurr <> "F" ,gcBaseCurr,RETHDR.ccurrcode)+lcaccount+IIF(llrpsrtbst, rethdr.store, SPACE(8))+lcstyle+'Z'+STR(IIF(lcRpCurr = "F" OR RETHdr.cCurrCode=gcBaseCurr,retline.price,gfAmntDisp(retline.price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")), 12, 2), lctempfile)
     					     *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                   *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[ENd]
                      SELECT (lctempfile)
                      REPLACE qty&lcprd   WITH qty&lcprd + (-retline.totqty), totqty              WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8+qty9+qty10+qty11+qty12
                      *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[Start]
                      *IF lcrptype='B' .AND. SEEK(account+IIF(llrpsrtbst, store, SPACE(8))+lcstyle+'Z'+STR(price,6)+'V', lctempfile)                      
                      *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
                      *IF lcrptype='B' .AND. SEEK(account+IIF(llrpsrtbst, store, SPACE(8))+lcstyle+'Z'+STR(price, 12, 2)+'V', lctempfile)                      
                      IF lcrptype='B' .AND. SEEK(IIF(lcRpCurr <> "F" ,gcBaseCurr,RETHDR.ccurrcode)+account+IIF(llrpsrtbst, store, SPACE(8))+lcstyle+'Z'+STR(IIF(lcRpCurr = "F" OR RETHdr.cCurrCode=gcBaseCurr,price,gfAmntDisp(price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")), 12, 2)+'V', lctempfile)
                      *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                      *: B609537,1 MMT 02/24/2011 AR - incorrect information on the Custom sales by month report  for DCC[End]
                         *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
                         *REPLACE qty&lcprd   WITH qty&lcprd + (-retline.totqty*retline.price) , totqty              WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8+qty9+qty10+qty11+qty12
                         REPLACE qty&lcprd   WITH qty&lcprd + (-retline.totqty*IIF(lcRpCurr = "F" OR RETHdr.cCurrCode=gcBaseCurr,retline.price,gfAmntDisp(retline.price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR"))) ,;
                                 totqty              WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8+qty9+qty10+qty11+qty12
                         *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                      ENDIF
                   ELSE
                      lcstorenam = cust.store
                      *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
                      *lcrepeted = IIF( .NOT. SEEK(lcaccount+IIF(llrpsrtbst, rethdr.store, SPACE(8))+lcstyle, lctempfile), 'A', 'Z')                      
					  *INSERT INTO (lctempfile) (account,accname,STORE,storenam,STYLE,DESC,price,qty&lcprd,totqty,TYPE,crepeted,llend )  VALUES (lcaccount,lcaccname,IIF(llrpsrtbst,rethdr.STORE,SPACE(8)),lcstorenam,lcstyle,lcdesc,retline.price, -retline.totqty,-retline.totqty,'R',lcrepeted,.F.)                           
                      lcrepeted = IIF( .NOT. SEEK(IIF(lcRpCurr <> "F" ,gcBaseCurr,RETHDR.ccurrcode)+lcaccount+IIF(llrpsrtbst, rethdr.store, SPACE(8))+lcstyle, lctempfile), 'A', 'Z')
                      INSERT INTO (lctempfile) (ccurrcode,account,accname,STORE,storenam,STYLE,DESC,price,qty&lcprd,totqty,TYPE,crepeted,llend )  VALUES (IIF(lcRpCurr <> "F" ,gcBaseCurr,RETHDR.ccurrcode),lcaccount,lcaccname,IIF(llrpsrtbst,rethdr.STORE,SPACE(8)),lcstorenam,lcstyle,lcdesc,IIF(lcRpCurr = "F" OR RETHdr.cCurrCode=gcBaseCurr,retline.price,gfAmntDisp(retline.price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")), -retline.totqty,-retline.totqty,'R',lcrepeted,.F.)     
                      *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
                      IF lcrptype='B'
                         INSERT INTO (lctempfile) (ccurrcode,account,accname,STORE,storenam,STYLE,DESC,price,qty&lcprd,totqty,TYPE,crepeted,llend )  VALUES (IIF(lcRpCurr <> "F" ,gcBaseCurr,RETHDR.ccurrcode),lcaccount,lcaccname,IIF(llrpsrtbst,rethdr.STORE,SPACE(8)),lcstorenam,lcstyle,lcdesc,IIF(lcRpCurr = "F" OR RETHdr.cCurrCode=gcBaseCurr,retline.price,gfAmntDisp(retline.price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")), -retline.totqty*IIF(lcRpCurr = "F" OR RETHdr.cCurrCode=gcBaseCurr,retline.price,gfAmntDisp(retline.price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")),-retline.totqty*IIF(lcRpCurr = "F" OR RETHdr.cCurrCode=gcBaseCurr,retline.price,gfAmntDisp(retline.price,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")),'V','Z',.F.)     
                      ENDIF
                   ENDIF
                   INSERT INTO (lctempretl) (crmemo, cret_linno, style) VALUES (rethdr.crmemo, retline.cret_linno, lcstyle)
                ENDIF
             ENDIF
          ENDSCAN
       ENDIF
    ENDSCAN
 ENDIF

**
*!*************************************************************
*! Name      : lfsrAcc2
*! Developer : mostafa eid (mos)
*! Date      : 01/10/2009
*! Purpose   : Account In Range Filter. 2
*!*************************************************************
*!

FUNCTION lfsrAcc2
 PARAMETER lcparm
 DO CASE
    CASE lcparm='S'
       GOTO TOP IN customer
    CASE lcparm='R'
       *CLEARREAD()   && Clear current read cycle, to activate new one.    
       lfgetstore()
 ENDCASE

**
*!*************************************************************
*! Name      : lfsrAcc2
*! Developer : mostafa eid(mos)
*! Date      : 01/10/2009
*! Purpose   : Account In Range Filter. 2
*!*************************************************************
*!

FUNCTION lfEndRep
 PARAMETER lcparm
 llendrep = &lctempfile..llend

**
*!*************************************************************
*! Name      : lfExportExcel
*! Developer : mostafa eid (mos)
*! Date      : 01/08/2009
*! Purpose   : export to excel file 
*!*************************************************************

FUNCTION lfExportExcel
 = gfopentable(oariaapplication.datadir+'CUSTOMER', 'CUSTOMER', 'SH', 'CUST')
 FOR lni = 1 TO 12
    lci = ALLTRIM(STR(lni))
    STORE 0 TO lngqty&lci, lngval&lci, lnqty&lci , lnval&lci , lneqty&lci , lneval&lci
    *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
    STORE 0 TO lnCurqty&lci, lnCurval&lci
    *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
 ENDFOR
 STORE 0 TO lntotqty, lntotval, lngtotqty, lngtotval, lntotamnt, lngtotamnt, lnetotqty, lnetotval, lnetotamnt
 *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
 STORE 0 TO lnCurtotqty, lnCurtotval,lnCurtotamnt
 *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
 rpsheet = CREATEOBJECT("EXCEL.APPLICATION")
 row = 1
 WITH rpsheet
    .standardfont = "Arial"
    .standardfontsize = "10"
    .sheetsinnewworkbook = 1
    .workbooks.add
    .activeworkbook.sheets(1).activate
    *--Give the Report Name to the sheet'S title AND the form name to the sheet1's name
    .caption = 'Sales By Month Report'
    .sheets(1).name = 'Sales By Month Report'
    *--Repeat the Row1 Headers with Every Page    
    .sheets(1).pagesetup.printtitlerows = "$1:$3"
    .sheets(1).pagesetup.orientation = 2
    row = row+1
    SELECT (lctempfile)
    SET ORDER TO display
    LOCATE
    lnpageno = 1
    lcaccount = &lctempfile..account
    lcstore   = &lctempfile..STORE
    *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
    lcCurrCode = &lctempfile..CCURRCODE
    *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
    = lfprnthdr()
    row = row+1
    = lfpstrhdr()
    llpstrfoot = .T.
    lnrecno = 1
    SCAN
       WAIT WINDOW NOWAIT 'Printing data for account : ' + &lctempfile..account + SPACE(10) + 'Store : ' + &lctempfile..STORE +  SPACE(10) + 'Product : ' + &lctempfile..STYLE + SPACE(10) + "Row : " + ALLTRIM(STR(lnrecno)) + " of " +  ALLTRIM(STR(RECCOUNT()))
       lnrecno = lnrecno+1
       *-- print Store Sub Total
       IF llrpsrtbst AND lcstore <> &lctempfile..STORE
          row = row+1
          = lfpstrfoot()
          STORE 0 TO lnqty1, lnqty2, lnqty3, lnqty4, lnqty5, lnqty6, lnqty7, lnqty8, lnqty9, lnqty10, lnqty11, lnqty12
          STORE 0 TO lnval1, lnval2, lnval3, lnval4, lnval5, lnval6, lnval7, lnval8, lnval9, lnval10, lnval11, lnval12
          STORE 0 TO lntotqty, lntotval, lntotamnt
          lcstore = &lctempfile..STORE
          llpstrfoot = .T.
       ENDIF
       *-- Print Grand Sub Total
       IF lcaccount <> &lctempfile..account
          row = row+1
          = gfseek('M'+lcaccount, 'CUST')
          lcaccname = cust.btname
          = lfpaccfoot()
          STORE 0 TO lngqty1, lngqty2, lngqty3, lngqty4, lngqty5, lngqty6, lngqty7, lngqty8, lngqty9, lngqty10, lngqty11, lngqty12
          STORE 0 TO lngval1, lngval2, lngval3, lngval4, lngval5, lngval6, lngval7, lngval8, lngval9, lngval10, lngval11, lngval12
          STORE 0 TO lngtotqty, lngtotval, lngtotamnt
          lcaccount = &lctempfile..account
          lnpageno = lnpageno+1
          row = row+2
          *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
          IF (llMultCurr AND  lcRpCurr= "F") AND lcCurrCode <> &lctempfile..CCURRCODE
             row = row+1
             = lfpCurrfoot()
             STORE 0 TO lnCurqty1, lnCurqty2, lnCurqty3, lnCurqty4, lnCurqty5, lnCurqty6, lnCurqty7, lnCurqty8, lnCurqty9, lnCurqty10, lnCurqty11, lnCurqty12
             STORE 0 TO lnCurval1, lnCurval2, lnCurval3, lnCurval4, lnCurval5, lnCurval6, lnCurval7, lnCurval8, lnCurval9, lnCurval10, lnCurval11, lnCurval12
             STORE 0 TO lnCurtotqty, lnCurtotval, lnCurtotamnt
             lcCurrCode = &lctempfile..CCURRCODE
             row = row+2
  	      ENDIF
          *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
          = lfprnthdr()
       ENDIF
       
       row = row+1
       *-- Print Body
       = lfprntbody()
       IF llpstrfoot
          llpstrfoot = .F.
          lcrow = ALLTRIM(STR(row))
          lcrange = "A"+lcrow+":"+"Q"+lcrow
          .range(lcrange).select
          WITH rpsheet.selection.borders(8)
             .linestyle = 1
             .weight = 4
             .colorindex = 5
          ENDWITH
       ENDIF
       *-- Update total variables
       IF type='Q' .OR. type='R'
          FOR lni = 1 TO 12
             lci = ALLTRIM(STR(lni))
             lngqty&lci = lngqty&lci + &lctempfile..qty&lci
             *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
             lnCurqty&lci = lnCurqty&lci + &lctempfile..qty&lci
             *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
             lnqty&lci  = lnqty&lci  + &lctempfile..qty&lci
             lneqty&lci = lneqty&lci + &lctempfile..qty&lci
          ENDFOR
          lngtotqty = lngtotqty + &lctempfile..totqty
          *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
          lnCurtotqty = lnCurtotqty + &lctempfile..totqty
          *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
          lnetotqty = lnetotqty + &lctempfile..totqty
          lntotqty  = lntotqty  + &lctempfile..totqty
          lntotamnt  = lntotamnt + &lctempfile..totqty*&lctempfile..price
          *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
          lnCurtotamnt = lnCurtotamnt + &lctempfile..totqty*&lctempfile..price
          *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
          lngtotamnt = lngtotamnt + &lctempfile..totqty*&lctempfile..price
          lnetotamnt = lnetotamnt + &lctempfile..totqty*&lctempfile..price
       ENDIF
       IF type='V' .OR. lcrptype='V'
          FOR lni = 1 TO 12
             lci = ALLTRIM(STR(lni))
             lngval&lci = lngval&lci + IIF(lcrptype = 'V',&lctempfile..qty&lci*&lctempfile..price,&lctempfile..qty&lci)
             *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
             lnCurval&lci = lnCurval&lci + IIF(lcrptype = 'V',&lctempfile..qty&lci*&lctempfile..price,&lctempfile..qty&lci)
             *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
             lneval&lci = lneval&lci + IIF(lcrptype = 'V',&lctempfile..qty&lci*&lctempfile..price,&lctempfile..qty&lci)
             lnval&lci  = lnval&lci  + IIF(lcrptype = 'V',&lctempfile..qty&lci*&lctempfile..price,&lctempfile..qty&lci)
          ENDFOR
          *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
          lnCurtotval= lnCurtotval+ IIF(lcrptype = 'V',&lctempfile..totqty*&lctempfile..price,&lctempfile..totqty)          
          *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
          lngtotval= lngtotval+ IIF(lcrptype = 'V',&lctempfile..totqty*&lctempfile..price,&lctempfile..totqty)
          lnetotval= lnetotval+ IIF(lcrptype = 'V',&lctempfile..totqty*&lctempfile..price,&lctempfile..totqty)
          lntotval = lntotval + IIF(lcrptype = 'V',&lctempfile..totqty*&lctempfile..price,&lctempfile..totqty)
       ENDIF
    ENDSCAN
    *-- print Store Sub Total
    IF llrpsrtbst AND lcstore <> &lctempfile..STORE
       row = row+1
       lcstorename = cust.store
       = lfpstrfoot()
       STORE 0 TO lnqty1, lnqty2, lnqty3, lnqty4, lnqty5, lnqty6, lnqty7, lnqty8, lnqty9, lnqty10, lnqty11, lnqty12
       STORE 0 TO lnval1, lnval2, lnval3, lnval4, lnval5, lnval6, lnval7, lnval8, lnval9, lnval10, lnval11, lnval12
       STORE 0 TO lntotqty, lntotval
       lcstore = &lctempfile..STORE
    ENDIF
    *-- Print Grand Sub Total
    IF lcaccount <> &lctempfile..account
       row = row+1
       = gfseek('M'+lcaccount, 'CUST')
       lcaccname = cust.btname
       = lfpaccfoot()
       STORE 0 TO lngqty1, lngqty2, lngqty3, lngqty4, lngqty5, lngqty6, lngqty7, lngqty8, lngqty9, lngqty10, lngqty11, lngqty12
       STORE 0 TO lngval1, lngval2, lngval3, lngval4, lngval5, lngval6, lngval7, lngval8, lngval9, lngval10, lngval11, lngval12
       STORE 0 TO lngtotqty, lngtotval
       lcaccount = &lctempfile..account
    ENDIF
       *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
       IF (llMultCurr AND  lcRpCurr= "F") AND lcCurrCode <> &lctempfile..CCURRCODE
         row = row+1
         = lfpCurrfoot()
         STORE 0 TO lnCurqty1, lnCurqty2, lnCurqty3, lnCurqty4, lnCurqty5, lnCurqty6, lnCurqty7, lnCurqty8, lnCurqty9, lnCurqty10, lnCurqty11, lnCurqty12
         STORE 0 TO lnCurval1, lnCurval2, lnCurval3, lnCurval4, lnCurval5, lnCurval6, lnCurval7, lnCurval8, lnCurval9, lnCurval10, lnCurval11, lnCurval12
         STORE 0 TO lnCurtotqty, lnCurtotval, lnCurtotamnt
         lcCurrCode = &lctempfile..CCURRCODE
       ENDIF
       *: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
    
    row = row+1
    
    = lfpendfoot()
 ENDWITH
 rpsheet.columns("A:S").entirecolumn.autofit
 rpsheet.visible = .T.
 RELEASE rpsheet

**
*!***************************************************************************
*! Name      : lfPrntHdr
*! Developer : Mostafa Eid (mos)
*! Date      : 01/08/2009
*! Purpose   : Setup the page header (For Every Page)
*!***************************************************************************
*! Called from : the Code
*!***************************************************************************
*! Example     : =lfPageHdr()
*!***************************************************************************

FUNCTION lfPrntHdr
 LOCAL lcrange
 WITH rpsheet
    lcrow = ALLTRIM(STR(row))
    lcrange = "A"+lcrow+":"+"L"+lcrow
    .range(lcrange).select
    WITH rpsheet.selection
       .merge
       .horizontalalignment = 2
       .font.bold = .T.
    ENDWITH
    .activecell.formular1c1 = 'Sales By Month Report For : ' + &lctempfile..accname + SPACE(20) +  'For Period : ' + lcsmonth + '-' + lcsyear + '    To : ' + lcfmonth + '-' + lcfyear
    lcrange = "M"+lcrow+":"+"O"+lcrow
    .range(lcrange).select
    WITH rpsheet.selection
       .merge
       .horizontalalignment = 2
       .font.bold = .T.
    ENDWITH
    .activecell.formular1c1 = 'Date : '+DTOC(oariaapplication.systemdate)
    lcrange = "P"+lcrow+":"+"O"+lcrow
    .range(lcrange).select
    WITH rpsheet.selection
       .merge
       .horizontalalignment = 2
       .font.bold = .T.
    ENDWITH
    .activecell.formular1c1 = 'Page : '+STR(lnpageno)
    row = row+1
    lcrow = ALLTRIM(STR(row))
    lcrange = "A"+lcrow+":"+"C"+lcrow
    .range(lcrange).select
    WITH rpsheet.selection
       .merge
       .horizontalalignment = 2
       .font.bold = .T.
    ENDWITH
    .activecell.formular1c1 = 'Created By : '+ALLTRIM(oariaapplication.user_name)
    lcrange = "M"+lcrow+":"+"Q"+lcrow
    .range(lcrange).select
    WITH rpsheet.selection
       .merge
       .horizontalalignment = 2
       .font.bold = .T.
    ENDWITH
    .activecell.formular1c1 = oariaapplication.activecompanyname
 ENDWITH

**
*!***************************************************************************
*! Name      : lfPStrhdr
*! Developer : Mostafa Eid (mos)
*! Date      : 01/08/2009
*! Purpose   : Setup the page header (For Every Page)
*!***************************************************************************
*! Called from : the Code
*!***************************************************************************
*! Example     : =lfPStrFoot()
*!***************************************************************************

FUNCTION lfPStrhdr
 lcrow = ALLTRIM(STR(row))
 lcrange = "A"+lcrow+":"+"Q"+lcrow
 .range(lcrange).select
 WITH rpsheet.selection
    FOR lni = 7 TO 10
       WITH .borders(lni)
          .linestyle = 1
          .weight = 3
          .colorindex = 5
       ENDWITH
    ENDFOR
    .font.bold = .T.
 ENDWITH
 WITH rpsheet
    .cells(row, 1).value = 'Product'
    .cells(row, 1).horizontalalignment = 2
    .cells(row, 2).value = 'Description'
    .cells(row, 2).horizontalalignment = 2
    lcrange = "C"+lcrow+":"+'N'+lcrow
    .range(lcrange).select
    WITH rpsheet.selection
       .numberformat = "@"
       .horizontalalignment = 4
    ENDWITH
    FOR lni = 1 TO 12
       lncell = lni+2
       .cells(row, lncell).value = laperiod(lni, 2)
    ENDFOR
    lcrange = "O"+lcrow+":"+'Q'+lcrow
    .range(lcrange).select
    WITH rpsheet.selection
       .numberformat = "@"
       .horizontalalignment = 4
    ENDWITH
    .cells(row, 15).value = 'Total Qty.'
    .cells(row, 16).value = 'Price'
    .cells(row, 17).value = 'Amount'
 ENDWITH

*--End of Function lfPStrhdr.

*!***************************************************************************
*! Name      : lfPrntBody
*! Developer : Mostafa Eid (mos)
*! Date      : 01/08/2009
*! Purpose   : Setup the page header (For Every Page)
*!***************************************************************************
*! Called from : the Code
*!***************************************************************************
*! Example     : =lfPrntBody()
*!***************************************************************************

**
FUNCTION lfPrntBody
 lcrow = ALLTRIM(STR(row))
 WITH rpsheet
    *: B609755,1 MMT 11/28/2011 Sales by Month Report suppressing style details on the excel[Start]
    *.cells(ROW,1).VALUE = IIF(&lctempfile..crepeted = 'A',&lctempfile..STYLE,"")
    .cells(ROW,1).VALUE = &lctempfile..STYLE
    *: B609755,1 MMT 11/28/2011 Sales by Month Report suppressing style details on the excel[END]
    .cells(row, 1).horizontalalignment = 2
    *: B609755,1 MMT 11/28/2011 Sales by Month Report suppressing style details on the excel[Start]    
    *.cells(ROW,2).VALUE = IIF(&lctempfile..crepeted = 'A',&lctempfile..DESC,"")
    .cells(ROW,2).VALUE = &lctempfile..DESC    
    *: B609755,1 MMT 11/28/2011 Sales by Month Report suppressing style details on the excel[END]    
    .cells(row, 2).horizontalalignment = 2
    lcrange = "C"+lcrow+":"+'Q'+lcrow
    .range(lcrange).select
    WITH rpsheet.selection
       IF lcrptype = 'V' OR &lctempfile..TYPE = 'V'
          .numberformat = "0.00"
       ENDIF
       .horizontalalignment = 4
    ENDWITH
    FOR lni = 3 TO 14
       lci = ALLTRIM(STR(lni-2))
       .cells(ROW,lni).VALUE = IIF(lcrptype <> 'V',&lctempfile..qty&lci,ROUND(&lctempfile..qty&lci*&lctempfile..price,2))
    ENDFOR
    .cells(ROW ,15).VALUE = IIF(lcrptype<> 'V',&lctempfile..totqty,ROUND(&lctempfile..totqty*&lctempfile..price,2))
    .cells(ROW ,16).VALUE = ROUND(&lctempfile..price,2)
    .cells(ROW ,17).VALUE = IIF(lcrptype<> 'V',IIF(&lctempfile..TYPE <> 'V',ROUND(&lctempfile..totqty*&lctempfile..price,2),0),0)
 ENDWITH

*!* End Of Function lfPrntBody.
*!***************************************************************************
*! Name      : lfPStrFoot
*! Developer : Mostafa Eid (mos)
*! Date      : 01/08/2009
*! Purpose   : Setup the page header (For Every Page)
*!***************************************************************************
*! Called from : the Code
*!***************************************************************************
*! Example     : =lfPStrFoot()
*!***************************************************************************

**
FUNCTION lfPStrFoot
 row = row+1
 WAIT WINDOW NOWAIT "Printing Sub Total For Store : "+lcstore
 lcrow = ALLTRIM(STR(row))
 lcnrow = IIF(lcrptype='B', ALLTRIM(STR(row+1)), ALLTRIM(STR(row)))
 lcrange = "A"+lcrow+":"+"Q"+lcnrow
 .range(lcrange).select
 WITH rpsheet.selection
    FOR lni = 7 TO 10
       WITH .borders(lni)
          .linestyle = 1
          .weight = 3
          .colorindex = 5
       ENDWITH
    ENDFOR
    .font.bold = .T.
 ENDWITH
 WITH rpsheet
    lcrange = "A"+lcrow+":"+"B"+lcnrow
    .range(lcrange).select
    WITH rpsheet.selection
       .merge
       .horizontalalignment = 2
       .verticalalignment = 2
    ENDWITH
    .activecell.formular1c1 = 'Sub Total For Store : '+lcstore
    FOR lni = 1 TO 12
       lci = ALLTRIM(STR(lni))
       lncell = lni+2
       .cells(ROW,lncell).VALUE = IIF(lcrptype<> 'V',lnqty&lci,ROUND(lnval&lci,2))
    ENDFOR
    lcrange = "C"+lcrow+":"+"Q"+lcrow
    .range(lcrange).select
    WITH rpsheet.selection
       IF lcrptype='V'
          .numberformat = "0.00"
       ENDIF
       .horizontalalignment = 4
    ENDWITH
    .cells(row, 15).value = IIF(lcrptype<>'V', lntotqty, ROUND(lntotamnt, 2))
    .cells(row, 17).value = IIF(lcrptype<>'V', ROUND(lntotamnt, 2), 0)
    IF lcrptype='B'
       row = row+1
       lcrange = "C"+lcrow+":"+"Q"+lcrow
       .range(lcrange).select
       WITH rpsheet.selection
          .numberformat = "0.00"
          .horizontalalignment = 4
       ENDWITH
       FOR lni = 1 TO 12
          lci = ALLTRIM(STR(lni))
          lncell = lni+2
          .cells(ROW ,lncell).VALUE = ROUND(lnval&lci,2)
       ENDFOR
       .cells(row, 15).value = ROUND(lntotval, 2)
    ENDIF
 ENDWITH

*!* End Of Function lfPStrFoot.
*!***************************************************************************
*! Name      : lfPAccFoot
*! Developer : Mostafa Eid (mos)
*! Date      : 01/08/2009
*! Purpose   : Setup the page header (For Every Page)
*!***************************************************************************
*! Called from : the Code
*!***************************************************************************
*! Example     : =lfPAccFoot()
*!***************************************************************************
**
FUNCTION lfPAccFoot
 row = row+1
 WAIT WINDOW NOWAIT "Printing Sub Total For Account : "+lcaccname
 lcrow = ALLTRIM(STR(row))
 lcnrow = IIF(lcrptype='B', ALLTRIM(STR(row+1)), ALLTRIM(STR(row)))
 lcrange = "A"+lcrow+":"+"Q"+lcnrow
 .range(lcrange).select
 WITH rpsheet.selection
    FOR lni = 7 TO 10
       WITH .borders(lni)
          .linestyle = 1
          .weight = 3
          .colorindex = 5
       ENDWITH
    ENDFOR
    .font.bold = .T.
 ENDWITH
 lcrange = "A"+lcrow+":"+"B"+lcnrow
 .range(lcrange).select
 WITH rpsheet.selection
    .merge
    .horizontalalignment = 2
    .verticalalignment = 2
 ENDWITH
 .activecell.formular1c1 = 'Total For Account : '+lcaccname
 lcrange = "C"+lcrow+":"+"Q"+lcrow
 .range(lcrange).select
 WITH rpsheet.selection
    IF lcrptype='V'
       .numberformat = "0.00"
    ENDIF
    .horizontalalignment = 4
 ENDWITH
 FOR lni = 1 TO 12
    lci = ALLTRIM(STR(lni))
    lncell = lni+2
    .cells(ROW ,lncell).VALUE = IIF(lcrptype= 'V',ROUND(lngval&lci,2),lngqty&lci)
 ENDFOR
 .cells(row, 15).value = IIF(lcrptype='V', ROUND(lngtotamnt, 2), lngtotqty)
 .cells(row, 17).value = IIF(lcrptype='V', 0, ROUND(lngtotamnt, 2))
 IF lcrptype='B'
    row = row+1
    lcrange = "C"+lcrow+":"+"Q"+lcrow
    .range(lcrange).select
    WITH rpsheet.selection
       .numberformat = "0.00"
       .horizontalalignment = 4
    ENDWITH
    FOR lni = 1 TO 12
       lci = ALLTRIM(STR(lni))
       lncell = lni+2
       .cells(ROW ,lncell).VALUE = ROUND(lngval&lci,2)
    ENDFOR
    .cells(row, 15).value = ROUND(lngtotval, 2)
 ENDIF

**
*!* End Of Function lfPAccFoot.
*!***************************************************************************
*! Name      : lfPEndFoot
*! Developer : Mostafa Eid (mos)
*! Date      : 01/08/2009
*! Purpose   : Setup the page header (For Every Page)
*!***************************************************************************
*! Called from : the Code
*!***************************************************************************
*! Example     : =lfPEndFoot()
*!***************************************************************************

FUNCTION lfPEndFoot
*: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[Start]
IF (llMultCurr AND  lcRpCurr= "F")
  RETURN 
ENDIF
*: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]
 row = row+1
 lcrow = ALLTRIM(STR(row))
 WAIT WINDOW NOWAIT "Printing Grand Total For the Report"
 lcnrow = IIF(lcrptype='B', ALLTRIM(STR(row+1)), ALLTRIM(STR(row)))
 lcrange = "A"+lcrow+":"+"Q"+lcnrow
 .range(lcrange).select
 WITH rpsheet.selection
    FOR lni = 7 TO 10
       WITH .borders(lni)
          .linestyle = 6
          .weight = 3
          .colorindex = 5
       ENDWITH
    ENDFOR
    .font.bold = .T.
 ENDWITH
 lcrange = "A"+lcrow+":"+"B"+lcnrow
 .range(lcrange).select
 WITH rpsheet.selection
    .merge
    .horizontalalignment = 2
    .verticalalignment = 2
 ENDWITH
 .activecell.formular1c1 = 'Grand Total : '
 lcrange = "C"+lcrow+":"+"Q"+lcrow
 .range(lcrange).select
 WITH rpsheet.selection
    IF lcrptype='V'
       .numberformat = "0.00"
    ENDIF
    .horizontalalignment = 4
 ENDWITH
 FOR lni = 1 TO 12
    lci = ALLTRIM(STR(lni))
    lncell = lni+2
    .cells(ROW ,lncell).VALUE = IIF(lcrptype= 'V',ROUND(lneval&lci,2),lneqty&lci)
 ENDFOR
 .cells(row, 15).value = IIF(lcrptype='V', ROUND(lnetotamnt, 2), lnetotqty)
 .cells(row, 17).value = IIF(lcrptype='V', 0, ROUND(lnetotamnt, 2))
 IF lcrptype='B'
    row = row+1
    lcrange = "C"+lcrow+":"+"Q"+lcrow
    .range(lcrange).select
    WITH rpsheet.selection
       .numberformat = "0.00"
       .horizontalalignment = 4
    ENDWITH
    FOR lni = 1 TO 12
       lci = ALLTRIM(STR(lni))
       lncell = lni+2
       .cells(ROW ,lncell).VALUE = ROUND(lneval&lci,2)
    ENDFOR
    .cells(row, 15).value = ROUND(lnetotval, 2)
 ENDIF

**
*!* End Of Function lfPEndFoot.
*!***************************************************************************
*! Name      : lPError
*! Developer : Mostafa Eid (mos)
*! Date      : 01/08/2009
*! Purpose   : Quit if any error
*!***************************************************************************
*! Called from : ARPINVNC
*!***************************************************************************
*! Example     : =lPError()
*!***************************************************************************
FUNCTION lPError
 PARAMETER lnerror, lcmsg, lnline
 MESSAGEBOX('Error#:'+STR(lnerror)+'   '+lcmsg+'   ,'+STR(lnline))
 READ EVENTS

**
*!***************************************************************************
*! Name      : lfGetStore
*! Developer : Mostafa Eid (mos)
*! Date      : 01/08/2009
*! Purpose   : GET STORES FOR AN ACCOUNT 
*!***************************************************************************
*! Example     : =lfGetStore()
*!*************************************************************************** 

FUNCTION lfGetStore
 llstorstat = .F.
 lnaccpos = ASCAN(loogscroll.laogfxflt, 'INVLINE.ACCOUNT')
 IF lnaccpos>0
    lnaccpos = ASUBSCRIPT(loogscroll.laogfxflt, lnaccpos, 1)
    IF  .NOT. EMPTY(laogfxflt(lnaccpos, 6)) .AND. USED(laogfxflt(lnaccpos, 6))
       lcfxacc = (loogscroll.laogfxflt(lnaccpos, 6))
       SELECT (lcfxacc)
       lndeleted = SET("DELETED")
       SET DELETED OFF
       SCAN
          IF DELETED()
             IF SEEK(&lcfxacc..account,lctempstor)
                SELECT (lctempstor)
                DELETE REST WHILE account+STORE =  &lcfxacc..account
             ENDIF
          ELSE
             IF gfseek("S" + &lcfxacc..account,'CUSTOMER','CUSTOMER')
                SELECT customer
                SCAN REST WHILE TYPE+account+STORE = 'S'+ &lcfxacc..account      
                   llstorstat = .T.
                   IF  .NOT. SEEK(account+store, lctempstor, lctempstor)
                      SELECT (lctempstor)
                      APPEND BLANK
                      REPLACE store WITH customer.store, btname WITH customer.btname, phone1 WITH customer.phone1, caddress6 WITH customer.caddress6, netbal WITH customer.netbal, account WITH customer.account
                   ENDIF
                ENDSCAN
             ENDIF
          ENDIF
       ENDSCAN
       SET DELETED &lndeleted.
    ENDIF
 ENDIF
 SELECT (lctempstor)
 && call function which Enable/Disable Store      
 = lfstorenab()

*: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[STARt]
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/30/2012
*! Purpose   : Activate currency display screen to get user 
*!           : selection for currencies exchange rates.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*!E301272,1
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/30/2012
*! Purpose   : set currency symbol
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvCurr()
*!*************************************************************
FUNCTION lfvCurr
*-- end of lfvCurr.
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/30/2012
*! Purpose   : Fill Currency arrays
*!*************************************************************
FUNCTION lfEvalSegs
IF llMultCurr
  DIMENSION laCurrVal[1,1]

  IF !USED('SYCCURR')
    =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1],laCurrSmbl[ALEN(laCurrVal,1),1]

  SELECT SYCCURR
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrSmbl[lnI,1] = ALLTRIM(PADR(CCURRSMBL,3))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF

*!*************************************************************
*! Name      : lfpCurrfoot
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/30/2012
*! Purpose   : Print Currency subtotal in excel
*!*************************************************************
PROCEDURE lfpCurrfoot
 row = row+1
 WAIT WINDOW NOWAIT "Printing Sub Total For Currency : "+lcCurrCode
 lcrow = ALLTRIM(STR(row))
 lcnrow = IIF(lcrptype='B', ALLTRIM(STR(row+1)), ALLTRIM(STR(row)))
 lcrange = "A"+lcrow+":"+"Q"+lcnrow
 .range(lcrange).select
 WITH rpsheet.selection
    FOR lni = 7 TO 10
       WITH .borders(lni)
          .linestyle = 1
          .weight = 3
          .colorindex = 5
       ENDWITH
    ENDFOR
    .font.bold = .T.
 ENDWITH
 lcrange = "A"+lcrow+":"+"B"+lcnrow
 .range(lcrange).select
 WITH rpsheet.selection
    .merge
    .horizontalalignment = 2
    .verticalalignment = 2
 ENDWITH
 .activecell.formular1c1 = 'Total For Currency : '+lcCurrCode
 lcrange = "C"+lcrow+":"+"Q"+lcrow
 .range(lcrange).select
 WITH rpsheet.selection
    IF lcrptype='V'
       .numberformat = "0.00"
    ENDIF
    .horizontalalignment = 4
 ENDWITH
 FOR lni = 1 TO 12
    lci = ALLTRIM(STR(lni))
    lncell = lni+2
    .cells(ROW ,lncell).VALUE = IIF(lcrptype= 'V',ROUND(lnCurval&lci,2),lnCurqty&lci)
 ENDFOR
 .cells(row, 15).value = IIF(lcrptype='V', ROUND(lnCurtotamnt, 2), lnCurtotqty)
 .cells(row, 17).value = IIF(lcrptype='V', 0, ROUND(lnCurtotamnt, 2))
 IF lcrptype='B'
    row = row+1
    lcrange = "C"+lcrow+":"+"Q"+lcrow
    .range(lcrange).select
    WITH rpsheet.selection
       .numberformat = "0.00"
       .horizontalalignment = 4
    ENDWITH
    FOR lni = 1 TO 12
       lci = ALLTRIM(STR(lni))
       lncell = lni+2
       .cells(ROW ,lncell).VALUE = ROUND(lnCurval&lci,2)
    ENDFOR
    .cells(row, 15).value = ROUND(lnCurtotval, 2)
 ENDIF
*: C201481,1 MMT 04/29/2012 Add Currecny option to Custom Sales by month report for DCC[END]


