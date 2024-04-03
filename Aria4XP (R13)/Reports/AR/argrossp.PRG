*:***************************************************************************
*: Program file       : ARGROSSP
*: Program desc.      : Gross profit
*: System             : Aria4XP
*: Module             : Account receivable (AR)
*: Developer          : Tarek Noaman (TNA)
*: Tracking Job Number: N037688
*: Date               : 4/18/2006
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARGROSSP
*:***************************************************************************
*:                   Option grid Filter contains
*:01-Print Decimals                YES/NO                             llRpDec
*:02-Currency Display              Currency                           lnRepCurr
*:03-Invoice Date                  Between                            INVHDR.INVDATE
*:04-Invoice By                    Ship Date/Invoice Date             lcRPInvBy
*:05-Style                         In List                            INVLINE.STYLE
*:06-Account                       In List                            INVHDR.ACCOUNT
*:07-Season                        IS                                 lcRPSeason
*:08-Division                      IS                                 lcRPDiv
*:09-SalesRep                      In List                            INVHDR.REP1
*:10-Include Returns               YES/NO                             llRPIncRet
*:11-Deduct ret.from GR.Sal.       YES/NO                             llRPSubRet
*:12-Report                        Detail/Summary                     lcRPSumDet
*:13-Sort By                       IS                                 lcRPSortBy
*:14-Style Group                   IS                                 lcStyGrp
*:15-Print Grand Total Only        YES/NO                             llRPGrnTot
*:16-Title                         IS                                 lcRPTitle
*:17-Invoice Currency              Like                               INVHDR.CCURRCODE
*:18-Customer Classification       In List                            CUSTOMER.CLASS
*:***************************************************************************
*:                         Tables Used
*:                        _____________
*:01- Invhdr
*:02- Invline
*:03- Customer
*:04- Style
*:05- RETHDR
*:06- RETLINE
*:***************************************************************************
*!MODIFICATIONS
*!B608019,1 03/27/2007 T20070313.0014  AYM : PROBLEM WHEN SELECTING STYLES OR STYLE GROUP
*!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word      [T20071107.0001]
*!B608382,1 12/16/2007 MMT Fix bug of dealing with Rethdr and retline as Fox tables [T20071115.0006]
*!B608382,2 01/22/2008 MMT Fix bug of dealing with Rethdr and retline as Fox tables [T20071115.0006]
*!B608724,1 10/15/2008 WAM don't show canceled return lines [T20081006.0012]
*!E302638,1 09/24/2009 MMT add option deduct trade disc in gross profit reports(So,AR)[T20090724.0021]
*!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line [T20100120.0060]
*!B609746,1 MMT 11/27/2011 Getting an error message when running gross profit report[T20110916.0008]
*!B609876,1 SAB 03/26/2012 Fix calculation of gross price in case of credit memo [T20120322.0037]
*!B610142,1 MMT 11/08/2012 Fix bug of not calculating trade discount of Credit memo is not created from Invoice[T20121009.0018]
*!B610349,1 HIA 06/02/13 T20130509.0007 - Gross profit invoiced report
*!B610868,1 MMT 10/01/2014 Gross profit report is displaying Grand total with foreign currency[T20140930.0011]
*!B610951,1 MMT 02/18/2015 Fix the incorrect trade amount field used from retline[T20150216.0027]
*!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015]
*!E303647,1 MMT 03/06/2016 Add CustPO field to AR Gross profit report excel[T20160125.0010]
*!B611140,1 MMT 04/12/2016 AR Gross profit report does not consider rep2 in filtering with Sales rep.[T20160407.0044]
*!B611187,1 MMT 09/08/2016 Issue#31:AR gross profit calculates Returns net amount incorreectly[P20160601.0001]
*!B611483,1 AHH 05/12/2017 The Account column is exported as empty for Credit memo lines in AR Gross profit report[T20171117.0009]
*!B611691,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001]
*!E304133,1 HMS 04/07/2019 - ERP - Gross Profit Report [T20190301.0007]
*!B611788,1 HMS 18/06/2019 - Company S2 - A/R Gross Profit data on style P19B17Tx - Gross profit figures are crazy [T20190604.0001]
*:***********************************************************************************
#INCLUDE R:\aria4xp\reports\ar\argrossp.H
lcStTime   = TIME() 
llDummy    = loOgScroll.llOGFltCh AND lfCollData()
lcEdTime   = TIME() 
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.*--
SELECT FInvline
GO TOP
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Record(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTED,oAriaApplication.GetHeaderText("LANG_SELECTED",AHEADERFILE))+' ' +;
  ALLTRIM(STR(RECCOUNT())) +;
  ' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RECORDSIN,oAriaApplication.GetHeaderText("LANG_RECORDSIN",AHEADERFILE))+' ' +;
  ALLTRIM(STR(lnInterval,6,2)) +;
  ' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SECONDS,oAriaApplication.GetHeaderText("LANG_SECONDS",AHEADERFILE)) NOWAIT
*N000682,1 MMT 02/10/2013 Globalization changes[End]
IF RECCOUNT()=0
  *--There is no record to display
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF
*!B610868,1 MMT 10/01/2014 Gross profit report is displaying Grand total with foreign currency[T20140930.0011][Start]
*DIMENSION loOGScroll.laCRParams[7,2]
DIMENSION loOgScroll.laCRParams[9,2]
loOgScroll.laCRParams[8,1]  = 'MultiCurr'
loOgScroll.laCRParams[8,2]  = IIF(llMultCurr,'Y','N')
loOgScroll.laCRParams[9,1]  = 'CurrDisp'
loOgScroll.laCRParams[9,2]  = lcRpCurr
*!B610868,1 MMT 10/01/2014 Gross profit report is displaying Grand total with foreign currency[T20140930.0011][End]
loOgScroll.laCRParams[1,1]  = 'Layout'
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*loOGScroll.laCRParams[1,2]  = IIF(lcRPSumDet='D','Detail','Summary')
loOgScroll.laCRParams[1,2]  = IIF(lcRPSumDet='D',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DETAIL,oAriaApplication.GetHeaderText("LANG_DETAIL",AHEADERFILE)),;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SUMMARY,oAriaApplication.GetHeaderText("LANG_SUMMARY",AHEADERFILE)))
*N000682,1 MMT 02/10/2013 Globalization changes[End]
loOgScroll.laCRParams[2,1]  = 'OpTitle'
loOgScroll.laCRParams[2,2]  = lcRPTitle
loOgScroll.laCRParams[3,1]  = 'sortby'
loOgScroll.laCRParams[3,2]  = lcSortBy
loOgScroll.laCRParams[4,1]  = 'ReportName'
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*loOGScroll.laCRParams[4,2]  = 'Gross Profit'
loOgScroll.laCRParams[4,2]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REPORTNAME,oAriaApplication.GetHeaderText("LANG_REPORTNAME",AHEADERFILE))
*N000682,1 MMT 02/10/2013 Globalization changes[ENd]
loOgScroll.laCRParams[5,1]  = 'PrintDecimal'
loOgScroll.laCRParams[5,2]  = llRpDec
loOgScroll.laCRParams[6,1]  = 'PrintGrndTot'
loOgScroll.laCRParams[6,2]  = IIF(llRPGrnTot,'Y','N')
loOgScroll.laCRParams[7,1]  = 'IncludeRet'
loOgScroll.laCRParams[7,2]  = IIF(llRPIncRet,'Y','N')

lcTmpFInv = loOgScroll.gfTempName()

DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes
loOgScroll.lacrTABLES[1] = oAriaApplication.WorkDir+lcTmpFInv+'.DBF'

SELECT FInvline
COPY TO oAriaApplication.WorkDir + lcTmpFInv + ".DBF"
*COPY TO oAriaApplication.WorkDir + "FInvline.DBF"
=gfOpenFile(oAriaApplication.WorkDir + (lcTmpFInv),'','EX')
USE IN (lcTmpFInv)  &&close the file will display from to be opened exclusive

loOgScroll.lcOGLastForm = "ARGROSSP"
loOgScroll.cCROrientation = 'L'

= gfDispRe()

ERASE oAriaApplication.WorkDir+lcTmpFInv+'.DBF'

*!*************************************************************
*! Name      : lfCollData
*! Developer : Tarek Noaman	(TNA)
*! Date      : 4/18/2006
*! Purpose   : Collection of Data
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfCollData
  llCallGfam=EMPTY(laOGFxFlt[2,6]) OR (!EMPTY(laOGFxFlt[2,6]) AND lcRpCurr <> 'F')
  *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][Start]
  *!*	CREATE CURSOR FInvline (invoice C(6),account C(5),STYLE C(19),LINENO N(6),totqty N(7),TYPE C(3),CODE C(6),;
  *!*	  NAME C(30) ,CLASS C(6),rep1 C(3),ccurrcode C(3),nexrate N(9,4),ncurrunit N(4),;
  *!*	  sprice N(12,2),GrossAmnt N(12,2),NetAmnt N(12,2),lnCost N(12,2),Profit N(12,2),Percnt N(12,2),;
  *!*	  ClassName C(30),RepName C(30))
  *!E303647,1 MMT 03/06/2016 Add CustPO field to AR Gross profit report excel[T20160125.0010][Start]
  *!*	CREATE CURSOR FInvline (invoice C(6),account C(5),STYLE C(19),LINENO N(6),totqty N(7),TYPE C(3),CODE C(6),;
  *!*	  NAME C(30) ,CLASS C(6),rep1 C(3),ccurrcode C(3),nexrate N(9,4),ncurrunit N(4),;
  *!*	  sprice N(12,2),GrossAmnt N(12,2),NetAmnt N(12,2),lnCost N(12,2),Profit N(12,2),Percnt N(12,2),;
  *!*	  ClassName C(30),RepName C(30),Season C(30),cStyGroup C(30), cDivision C(30), InvDate D(8))
  *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][Begin]
*!*	  CREATE CURSOR FInvline (invoice C(6),account C(5),STYLE C(19),LINENO N(6),totqty N(7),TYPE C(3),CUSTPO C(15),CODE C(6),;
*!*	    NAME C(30) ,CLASS C(6),rep1 C(3),ccurrcode C(3),nexrate N(9,4),ncurrunit N(4),;
*!*	    sprice N(12,2),GrossAmnt N(12,2),NetAmnt N(12,2),lnCost N(12,2),Profit N(12,2),Percnt N(12,2),;
*!*	    ClassName C(30),RepName C(30),Season C(30),cStyGroup C(30), cDivision C(30), InvDate D(8))
*E304133 , 1 HMS , 04/07/2019 - ERP - Gross Profit Report [T20190301.0007][Begin]
*!*	    CREATE CURSOR FInvline (invoice C(6),account C(5),STYLE C(19),LINENO N(6),totqty N(7),TYPE C(3),CUSTPO C(15),CODE C(6),;
*!*	    NAME C(30) ,CLASS C(6),rep1 C(3),ccurrcode C(3),nexrate N(9,4),ncurrunit N(4),;
*!*	    sprice N(12,2),GrossAmnt N(12,2),NetAmnt N(12,2),lnCost N(12,2),Profit N(12,2),Percnt N(12,2),;
*!*	    ClassName C(30),RepName C(30),Season C(30),cStyGroup C(30), cDivision C(30), Tran_Date D(8))
    
  CREATE CURSOR FInvline (invoice C(6),account C(5),STYLE C(19),LINENO N(6),totqty N(7),TYPE C(3),CUSTPO C(15),CODE C(6),;
    NAME C(30) ,CLASS C(6),rep1 C(3),ccurrcode C(3),nexrate N(9,4),ncurrunit N(4),;
    sprice N(12,2),GrossAmnt N(12,2),NetAmnt N(12,2),lnCost N(12,2),Profit N(12,2),Percnt N(12,2),;
    ClassName C(30),RepName C(30),Season C(30),cStyGroup C(30), cDivision C(30), Tran_Date D(8),SO_GRS_AMT N(12,2))  
*E304133 , 1 HMS , 04/07/2019 - ERP - Gross Profit Report [T20190301.0007][END]
*B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][END]
  *!E303647,1 MMT 03/06/2016 Add CustPO field to AR Gross profit report excel[T20160125.0010][End]
  *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][End]
  SELECT FInvline
  DO CASE
    CASE lcRPSortBy='A'
      INDEX ON account+invoice+STYLE+TYPE+CODE+STR(RECNO(),7) TAG INVLTEMP
      lcSortBy='Account'
    CASE lcRPSortBy='I'
      INDEX ON invoice+STYLE+TYPE+CODE+STR(RECNO(),7) TAG INVLTEMP
      lcSortBy='Invoice'
    CASE lcRPSortBy='S'
      INDEX ON STYLE+invoice+TYPE+CODE+STR(RECNO(),7) TAG INVLTEMP
      lcSortBy='Style'
    CASE lcRPSortBy='C'
      INDEX ON CLASS+invoice+account+STYLE+TYPE+CODE+STR(RECNO(),7) TAG INVLTEMP
      lcSortBy='Class'
    CASE lcRPSortBy='R'
      INDEX ON rep1+invoice+account+STYLE+TYPE+CODE+STR(RECNO(),7) TAG INVLTEMP
      lcSortBy='Salesrep'
  ENDCASE

  SELECT INVHDR
  SET RELATION TO invoice INTO INVLINE ADDITIVE
  SET RELATION TO rep1 INTO SALESREP ADDITIVE
  SELECT INVLINE
  SET RELATION TO 'M'+account INTO CUSTOMER ADDITIVE
  SET RELATION TO STYLE INTO STYLE ADDITIVE


  *!B608382,1 12/16/2007 MMT Fix bug of dealing with Rethdr and retline as Fox tables[Start]
  *!*  =gfOpenFile(gcDataDir+'Rethdr',gcDataDir+'Rethdr','SH')
  *!*  =gfOpenFile(gcDataDir+'Retline',gcDataDir+'Retline','SH')
  *!*  SELECT RETHDR
  *!*  SET RELATION TO CRMEMO INTO RETLINE ADDITIVE
  *!*  SET RELATION TO SALESREP1 INTO SALESREP ADDITIVE
  *!*  SELECT RETLINE
  *!*  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE
  *!*  SET RELATION TO STYLE INTO STYLE ADDITIVE

  *!B608382,2 01/22/2008 MMT Fix bug of dealing with Rethdr and retline as Fox tables[Start]
  IF 'RM' $ oAriaApplication.CompanySetupModules
    *!B608382,2 01/22/2008 MMT Fix bug of dealing with Rethdr and retline as Fox tables[End]

    =gfOpenTable(oAriaApplication.DataDir+'Rethdr',oAriaApplication.DataDir+'Rethdr','SH')
    =gfOpenTable(oAriaApplication.DataDir+'Retline',oAriaApplication.DataDir+'Retline','SH')

    *!B608382,2 01/22/2008 MMT Fix bug of dealing with Rethdr and retline as Fox tables[Start]
  ENDIF
  *!B608382,2 01/22/2008 MMT Fix bug of dealing with Rethdr and retline as Fox tables[End]

  *!B608382,1 12/16/2007 MMT Fix bug of dealing with Rethdr and retline as Fox tables[End]

  lcRpExp2 = lfCreateExp()


  *!B608019,1 T20070313.0014  AYM : pROBLEM WHEN SELECTING STYLES OR SSTYLE GROUP .. BEGIN
  *update for Styles
  lcLineExp =lcRpExp
  lnRpSlsPos = lfItmPos("INVLINE.STYLE")
  IF lnRpSlsPos > 0 .AND. loOgScroll.laOGFxFlt[lnRpSlsPos,7] = 'R'
    IF AT('In List',loOgScroll.laOGFxFlt[lnRpSlsPos,5]) <> 0
      lnSlsPost = AT('INLIST(INVLINE.STYLE',lcRpExp)
    ENDIF
    IF lnSlsPost > 0

      *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[Start]
      *lnPos1 = AT('AND',SUBSTR(lcRpExp,lnSlsPost))
      *IF lnPos1 > 0
      lnPos1 = AT(')',SUBSTR(lcRpExp,lnSlsPost))+1
      IF lnPos1-1 > 0
        *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[End]
        lcRpExp= STRTRAN(lcRpExp, SUBSTR(lcRpExp, lnSlsPost , lnPos1-1) , " .T. ")
      ELSE
        lcRpExp= STRTRAN(lcRpExp, SUBSTR(lcRpExp, lnSlsPost , LEN(lcRpExp)) , ' .T. ')
      ENDIF
    ENDIF
  ENDIF
  *update for Style group
  lnRpGRPPos = lfItmPos("STYLE.CSTYGROUP")
  IF lnRpGRPPos > 0
    IF AT('In List',loOgScroll.laOGFxFlt[lnRpGRPPos ,5]) <> 0
      lnGrpPost = AT('INLIST(STYLE.CSTYGROUP',lcRpExp)
    ENDIF

    IF lnGrpPost > 0
      *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[Start]
      *lnGPos1 = AT('AND',SUBSTR(lcRpExp,lnGrpPost ))
      *IF lnGPos1 > 0
      lnGPos1 = AT(')',SUBSTR(lcRpExp,lnGrpPost ))+1
      IF lnGPos1 - 1  > 0
        *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[End]
        lcRpExp= STRTRAN(lcRpExp, SUBSTR(lcRpExp, lnGrpPost , lnGPos1 ) , " .T. ")
      ELSE
        lcRpExp= STRTRAN(lcRpExp, SUBSTR(lcRpExp, lnGrpPost , LEN(lcRpExp)) , ' .T. ')
      ENDIF
    ENDIF
  ENDIF

  IF llRPIncRet
    *update for Styles
    lcLineExpR =lcRpExp2
    lnRpSlsPosR = lfItmPos("INVLINE.STYLE")
    IF lnRpSlsPosR > 0 .AND. loOgScroll.laOGFxFlt[lnRpSlsPosR,7] = 'R'
      IF AT('In List',loOgScroll.laOGFxFlt[lnRpSlsPosr,5]) <> 0
        lnSlsPostr = AT('INLIST(RETLINE.STYLE',lcRpExp2)
      ENDIF
      IF lnSlsPostr > 0
        *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[Start]
        *lnPos1r = AT('AND',SUBSTR(lcRpExp2,lnSlsPostr))
        *IF lnPos1r > 0
        lnPos1r = AT(')',SUBSTR(lcRpExp2,lnSlsPostr))+1
        IF lnPos1r-1 > 0
          *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[End]

          lcRpExp2= STRTRAN(lcRpExp2, SUBSTR(lcRpExp2, lnSlsPostr , lnPos1r-1) , " .T. ")
        ELSE
          lcRpExp2 = STRTRAN(lcRpExp2, SUBSTR(lcRpExp2, lnSlsPostr , LEN(lcRpExp2)) , ' .T. ')
        ENDIF
      ENDIF
    ENDIF
    *update for Style group
    lnRpGRPPosr = lfItmPos("STYLE.CSTYGROUP")
    IF lnRpGRPPosr > 0
      IF AT('In List',loOgScroll.laOGFxFlt[lnRpGRPPosr ,5]) <> 0
        lnGrpPostr = AT('INLIST(STYLE.CSTYGROUP',lcRpExp2 )
      ENDIF

      IF lnGrpPostr > 0
        *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[Start]
        *lnGPos1r = AT('AND',SUBSTR(lcRpExp2 ,lnGrpPostr ))
        *IF lnGPos1r > 0
        lnGPos1r = AT(')',SUBSTR(lcRpExp2 ,lnGrpPostr ))+1
        IF lnGPos1r -1 > 0
          *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[End]
          lcRpExp2= STRTRAN(lcRpExp2 , SUBSTR(lcRpExp2 , lnGrpPostr , lnGPos1r ) , " .T. ")
        ELSE
          lcRpExp2= STRTRAN(lcRpExp2 , SUBSTR(lcRpExp2 , lnGrpPostr , LEN(lcRpExp2 )) , ' .T. ')
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  *!B608019,1 T20070313.0014  AYM : pROBLEM WHEN SELECTING STYLES OR SSTYLE GROUP .. END

  *!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[BEGIN]
  M_TrdDiscL = gfGetMemVar('M_TRDDISCL')
  *!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[END]

  *B609746,1 MMT 11/27/2011 Getting an error message when running gross profit report[Start]
  *!*	SELECT INVHDR
  *!*	SCAN FOR EVALUATE(lcRpExp) AND INVHDR.Status<>'V'
  *!*	  SELECT INVLINE
  *!*	  IF SEEK(Invhdr.invoice)
  *!*	  *!B608019,1 T20070313.0014  AYM : pROBLEM WHEN SELECTING STYLES OR SSTYLE GROUP .. BEGIN
  *!*	   * SCAN WHILE Invline.invoice = Invhdr.invoice FOR EVALUATE(lcRpExp)
  *!*	    SCAN WHILE Invline.invoice = Invhdr.invoice FOR EVALUATE(lcLineExp )
  *!B608019,1 T20070313.0014  AYM : pROBLEM WHEN SELECTING STYLES OR SSTYLE GROUP .. END
  *Account
  llRetAccount = .F.
  lcAccFile = ''
  lnPosAcc = ASCAN(loOgScroll.laOGFxFlt,"INVHDR.ACCOUNT")
  IF lnPosAcc > 0
    lnPosAcc = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPosAcc,1)
    lcAccFile = loOgScroll.laOGFxFlt[lnPosAcc,6]
    IF !EMPTY(lcAccFile)
      SELECT(lcAccFile)
      LOCATE
      IF !EOF()
        llRetAccount  = .T.
      ENDIF
    ENDIF
  ENDIF

  *Date Range Selected
  lnDatePos = ASCAN(loOgScroll.laOGFxFlt,'INVHDR.INVDATE')
  ldStartDate = {}
  ldEndDate   = {}
  lcDateValue  = ""
  IF lnDatePos > 0
    lnDatePos = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnDatePos,1)
    lcDateValue = loOgScroll.laOGFxFlt[lnDatePos,6]
    IF !EMPTY(lcDateValue)
      ldStartDate = CTOD(SUBSTR(loOgScroll.laOGFxFlt[lnDatePos,6],1,10))
      ldEndDate   = CTOD(SUBSTR(loOgScroll.laOGFxFlt[lnDatePos,6],12,21))
    ENDIF
  ENDIF

  *CLASS
  llRetCustCls = .F.
  lcCustCLFile = ''
  lnPosCls = ASCAN(loOgScroll.laOGFxFlt,"CUSTOMER.CLASS")
  IF lnPosCls > 0
    lnPosCls  = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPosCls,1)
    lcCustClsC = loOgScroll.laOGFxFlt[lnPosCls,6]
    IF !EMPTY(lcCustClsC)
      lcCustCLFile = loOgScroll.gfTempName()
      llRetCustCls  = IIF(LEN(lcCustClsC )>0,.T.,.F.) AND lfConvertToCursor(lcCustClsC ,'CUSTCLS',lcCustCLFile)
    ENDIF
  ENDIF

  *CDIVISION
  llRetDiv = .F.
  lcCDivFile = ''
  lnPosDive = ASCAN(loOgScroll.laOGFxFlt,"INVHDR.CDIVISION")
  IF lnPosDive > 0
    lnPosDive  = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPosDive,1)
    lcDivStr = loOgScroll.laOGFxFlt[lnPosDive,6]
    IF !EMPTY(lcDivStr)
      lcCDivFile = loOgScroll.gfTempName()
      llRetDiv = IIF(LEN(lcDivStr)>0,.T.,.F.) AND lfConvertToCursor(lcDivStr,'CDIVISION',lcCDivFile )
    ENDIF
  ENDIF

  *CSTYGROUP
  llRetStyGrp = .F.
  lcStyGrpFile = ''
  lnPosGrp = ASCAN(loOgScroll.laOGFxFlt,"STYLE.CSTYGROUP")
  IF lnPosGrp> 0
    lnPosGrp= ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPosGrp,1)
    lcStyGRp = loOgScroll.laOGFxFlt[lnPosGrp,6]
    IF !EMPTY(lcStyGRp)
      lcStyGrpFile = loOgScroll.gfTempName()
      llRetStyGrp = IIF(LEN(lcStyGRp)>0,.T.,.F.) AND lfConvertToCursor(lcStyGRp,'CSTYGRP',lcStyGrpFile)
    ENDIF
  ENDIF

  *Season
  llRetSeason = .F.
  lcStySeaFile = ''
  lnPosSea = ASCAN(loOgScroll.laOGFxFlt,"INVHDR.SEASON")
  IF lnPosSea> 0
    lnPosSea= ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPosSea,1)
    lcStySea= loOgScroll.laOGFxFlt[lnPosSea,6]
    IF !EMPTY(lcStySea)
      lcStySeaFile= loOgScroll.gfTempName()
      llRetSeason = IIF(LEN(lcStySea)>0,.T.,.F.) AND lfConvertToCursor(lcStySea,'CSTYGRP',lcStySeaFile)
    ENDIF
  ENDIF

  *Style
  llRetStyle = .F.
  lcStyleFile = ''
  lnPosStyle = ASCAN(loOgScroll.laOGFxFlt,"INVLINE.STYLE")
  IF lnPosStyle > 0
    lnPosStyle = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPosStyle,1)
    lcStyleFile = loOgScroll.laOGFxFlt[lnPosStyle,6]
    IF !EMPTY(lcStyleFile)
      SELECT(lcStyleFile)
      LOCATE
      IF !EOF()
        llRetStyle = .T.
      ENDIF
    ENDIF
  ENDIF

  *SALESREP1
  llRetRep1 = .F.
  lcRepFile = ''
  lnPosRep1  = ASCAN(loOgScroll.laOGFxFlt,"INVHDR.REP1")
  IF lnPosRep1 > 0
    lnPosRep1 = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPosRep1 ,1)
    lcRepFile = loOgScroll.laOGFxFlt[lnPosRep1 ,6]
    IF !EMPTY(lcRepFile )
      SELECT(lcRepFile )
      LOCATE
      IF !EOF()
        llRetRep1 = .T.
      ENDIF
    ENDIF
  ENDIF

  *INVHDR.CCURRCODE
  llRetCurr = .F.
  lcCurrFile = ''
  lnPosCurr  = ASCAN(loOgScroll.laOGFxFlt,"INVHDR.CCURRCODE")
  IF lnPosCurr > 0
    lnPosCurr = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPosCurr,1)
    lcCurrFile= loOgScroll.laOGFxFlt[lnPosCurr,6]
    IF !EMPTY(lcCurrFile) AND USED(lcCurrFile)
      SELECT(lcCurrFile)
      LOCATE
      IF !EOF()
        llRetCurr = .T.
      ENDIF
    ENDIF
  ENDIF

  IF llRetAccount
    SELECT INVHDR
    lcOldInd = ORDER()
    gfSetOrder("INVHDRA")
    SELECT (lcAccFile)
    SCAN
      SELECT INVHDR
      =gfSeek(&lcAccFile..account)
      *!B611140,1 MMT 04/12/2016 AR Gross profit report does not consider rep2 in filtering with Sales rep.[T20160407.0044][Start]
      *!*	    SCAN REST WHILE ACCOUNT+INVOICE = &lcAccFile..Account FOR INVHDR.STATUS<>'V' AND;
      *!*	        IIF(!EMPTY(lcDateValue),BETWEEN(IIF(lcRPInvBy='S',INVHDR.SHIPDATE,INVHDR.INVDATE),ldStartDate ,ldEndDate),.T.) AND ;
      *!*	        SEEK('M'+INVHDR.Account ,"Customer") AND IIF(llRetCustCls  ,SEEK(CUSTOMER.CLASS,lcCustCLFile),.T.) AND ;
      *!*	        IIF(llRetRep1 ,SEEK(INVHDR.REP1,lcRepFile),.T.) AND ;
      *!*	        IIF(llRetCurr ,SEEK(INVHDR.ccurrcode,lcCurrFile),.T.) AND IIF(llRetSeason ,SEEK(INVHDR.SEASON,lcStySeaFile),.T.) AND ;
      *!*	        IIF(llRetDiv ,SEEK(INVHDR.CDIVISION,lcCDivFile ),.T.) AND IIF(!EMPTY(INVHDR.REP1),SEEK(INVHDR.REP1,'SALESREP'),.T.)
      SCAN REST WHILE account+invoice = &lcAccFile..account FOR INVHDR.STATUS<>'V' AND;
          IIF(!EMPTY(lcDateValue),BETWEEN(IIF(lcRPInvBy='S',INVHDR.SHIPDATE,INVHDR.InvDate),ldStartDate ,ldEndDate),.T.) AND ;
          SEEK('M'+INVHDR.account ,"Customer") AND IIF(llRetCustCls  ,SEEK(CUSTOMER.CLASS,lcCustCLFile),.T.) AND ;
          IIF(llRetRep1 ,SEEK(INVHDR.rep1,lcRepFile) OR SEEK(INVHDR.REP2,lcRepFile),.T.) AND ;
          IIF(llRetCurr ,SEEK(INVHDR.ccurrcode,lcCurrFile),.T.) AND IIF(llRetSeason ,SEEK(INVHDR.Season,lcStySeaFile),.T.) AND ;
          IIF(llRetDiv ,SEEK(INVHDR.cDivision,lcCDivFile ),.T.) AND IIF(!EMPTY(INVHDR.rep1),SEEK(INVHDR.rep1,'SALESREP'),.T.)
        *!B611140,1 MMT 04/12/2016 AR Gross profit report does not consider rep2 in filtering with Sales rep.[T20160407.0044][End]
        SELECT INVLINE
        IF SEEK(INVHDR.invoice)
          SCAN WHILE INVLINE.invoice = INVHDR.invoice FOR IIF(llRetStyle ,SEEK(INVLINE.STYLE,lcStyleFile),.T.) AND ;
              SEEK(INVLINE.STYLE,'Style') AND;
              IIF(llRetStyGrp ,SEEK(STYLE.cStyGroup,lcStyGrpFile ),.T.)
            SCATTER MEMVAR MEMO
            INSERT INTO FInvline FROM MEMVAR
            REPLACE FInvline.TYPE      WITH 'INV', ;
              FInvline.CODE      WITH INVLINE.invoice, ;
              FInvline.NAME      WITH CUSTOMER.btname, ;
              FInvline.ccurrcode WITH INVHDR.ccurrcode, ;
              FInvline.CLASS     WITH CUSTOMER.CLASS,  ;
              FInvline.ClassName WITH gfCodDes(CUSTOMER.CLASS,'CLASS'),  ;
              FInvline.rep1      WITH INVHDR.rep1, ;
              FInvline.RepName   WITH SALESREP.NAME, ;
              FInvline.nexrate   WITH INVHDR.nexrate, ;
              FInvline.ncurrunit WITH INVHDR.ncurrunit
            *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][Start]
            *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][Begin]
*!*	            REPLACE FInvline.Season WITH gfCodDes(STYLE.Season ,'SEASON'),;
*!*	              FInvline.cStyGroup WITH gfCodDes(STYLE.cStyGroup ,'CSTYGROUP'),;
*!*	              FInvline.cDivision WITH gfCodDes(STYLE.cDivision  ,'CDIVISION'),;
*!*	              FInvline.InvDate WITH INVHDR.InvDate
              REPLACE FInvline.Season WITH gfCodDes(STYLE.Season ,'SEASON'),;
              FInvline.cStyGroup WITH gfCodDes(STYLE.cStyGroup ,'CSTYGROUP'),;
              FInvline.cDivision WITH gfCodDes(STYLE.cDivision  ,'CDIVISION'),;
              FInvline.Tran_Date WITH INVHDR.InvDate
            *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][End]
            *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][End]
            *!E303647,1 MMT 03/06/2016 Add CustPO field to AR Gross profit report excel[T20160125.0010][Start]
            REPLACE FInvline.CUSTPO WITH INVHDR.CUSTPO
            *!E303647,1 MMT 03/06/2016 Add CustPO field to AR Gross profit report excel[T20160125.0010][End]
            lnGross  = INVLINE.totqty*IIF(llCallGfam,gfAmntDisp(INVLINE.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'INVHDR','ccurrcode'),INVLINE.Price)
            IF M_TrdDiscL
              lnNet    = lnGross * (1 - (INVHDR.DiscPcnt/100)) * (1 - (IIF(llRPTrdDsc,INVLINE.Trde_Disc,0)/100))
            ELSE
              lnNet    = lnGross * (1 - (INVHDR.DiscPcnt/100)) * (1 - (IIF(llRPTrdDsc,INVHDR.Trde_Disc,0)/100))
            ENDIF
            *!B611788,1 HMS 18/06/2019 - Company S2 - A/R Gross Profit data on style P19B17Tx - Gross profit figures are crazy [T20190604.0001][Start]
            *lnCost   = INVLINE.totqty * IIF(!llCallGfam,lfGetFCurr(INVLINE.Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),INVLINE.Cost)
            lnCost   = lfGetLineCost(INVLINE.invoice,INVLINE.LineNo,INVLINE.Style)
            *!B611788,1 HMS 18/06/2019 - Company S2 - A/R Gross Profit data on style P19B17Tx - Gross profit figures are crazy [T20190604.0001][End]
            lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
            lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)

            REPLACE FInvline.sprice    WITH IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'INVHDR','ccurrcode'),Price), ;
              FInvline.NetAmnt   WITH lnNet, ;
              FInvline.lnCost    WITH lnCost, ;
              FInvline.Profit    WITH lnNet - lnCost, ;
              FInvline.Percnt    WITH lnPercnt, ;
              FInvline.GrossAmnt WITH lnGross
              *E304133 , 1 HMS , 04/07/2019 - ERP - Gross Profit Report [T20190301.0007][Begin]
               REPLACE FInvline.SO_GRS_AMT WITH INVLINE.TotQty * INVLINE.gros_price
              *E304133 , 1 HMS , 04/07/2019 - ERP - Gross Profit Report [T20190301.0007][END]
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDSCAN
    SELECT INVHDR
    SET ORDER TO (lcOldInd)
  ELSE
    SELECT INVHDR
    *!B611140,1 MMT 04/12/2016 AR Gross profit report does not consider rep2 in filtering with Sales rep.[T20160407.0044][Start]
    *!*	  SCAN FOR INVHDR.STATUS<>'V' AND;
    *!*	      IIF(!EMPTY(lcDateValue),BETWEEN(IIF(lcRPInvBy='S',INVHDR.SHIPDATE,INVHDR.INVDATE),ldStartDate ,ldEndDate),.T.) AND ;
    *!*	      SEEK('M'+INVHDR.Account ,"Customer") AND IIF(llRetCustCls  ,SEEK(CUSTOMER.CLASS,lcCustCLFile),.T.) AND ;
    *!*	      IIF(llRetRep1 ,SEEK(INVHDR.REP1,lcRepFile),.T.) AND ;
    *!*	      IIF(llRetCurr ,SEEK(INVHDR.ccurrcode,lcCurrFile),.T.) AND IIF(llRetSeason ,SEEK(INVHDR.SEASON,lcStySeaFile),.T.) AND ;
    *!*	      IIF(llRetDiv ,SEEK(INVHDR.CDIVISION,lcCDivFile ),.T.) AND IIF(!EMPTY(INVHDR.REP1),SEEK(INVHDR.REP1,'SALESREP'),.T.)
    SCAN FOR INVHDR.STATUS<>'V' AND;
        IIF(!EMPTY(lcDateValue),BETWEEN(IIF(lcRPInvBy='S',INVHDR.SHIPDATE,INVHDR.InvDate),ldStartDate ,ldEndDate),.T.) AND ;
        SEEK('M'+INVHDR.account ,"Customer") AND IIF(llRetCustCls  ,SEEK(CUSTOMER.CLASS,lcCustCLFile),.T.) AND ;
        IIF(llRetRep1 ,SEEK(INVHDR.rep1,lcRepFile) OR SEEK(INVHDR.REP2,lcRepFile),.T.) AND ;
        IIF(llRetCurr ,SEEK(INVHDR.ccurrcode,lcCurrFile),.T.) AND IIF(llRetSeason ,SEEK(INVHDR.Season,lcStySeaFile),.T.) AND ;
        IIF(llRetDiv ,SEEK(INVHDR.cDivision,lcCDivFile ),.T.) AND IIF(!EMPTY(INVHDR.rep1),SEEK(INVHDR.rep1,'SALESREP'),.T.)
      *!B611140,1 MMT 04/12/2016 AR Gross profit report does not consider rep2 in filtering with Sales rep.[T20160407.0044][End]
      SELECT INVLINE
      IF SEEK(INVHDR.invoice)
        SCAN WHILE INVLINE.invoice = INVHDR.invoice FOR IIF(llRetStyle ,SEEK(INVLINE.STYLE,lcStyleFile),.T.) AND ;
            SEEK(INVLINE.STYLE,'Style') AND IIF(llRetStyGrp ,SEEK(STYLE.cStyGroup,lcStyGrpFile ),.T.)
          *B609746,1 MMT 11/27/2011 Getting an error message when running gross profit report[END]

          SCATTER MEMVAR MEMO
          INSERT INTO FInvline FROM MEMVAR
          REPLACE FInvline.TYPE      WITH 'INV', ;
            FInvline.CODE      WITH INVLINE.invoice, ;
            FInvline.NAME      WITH CUSTOMER.btname, ;
            FInvline.ccurrcode WITH INVHDR.ccurrcode, ;
            FInvline.CLASS     WITH CUSTOMER.CLASS,  ;
            FInvline.ClassName WITH gfCodDes(CUSTOMER.CLASS,'CLASS'),  ;
            FInvline.rep1      WITH INVHDR.rep1, ;
            FInvline.RepName   WITH SALESREP.NAME, ;
            FInvline.nexrate   WITH INVHDR.nexrate, ;
            FInvline.ncurrunit WITH INVHDR.ncurrunit

          *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][Start]
         *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][Begin]
*!*	          REPLACE FInvline.Season WITH gfCodDes(STYLE.Season  ,'SEASON'),;
*!*	            FInvline.cStyGroup WITH gfCodDes(STYLE.cStyGroup ,'CSTYGROUP'),;
*!*	            FInvline.cDivision WITH gfCodDes(STYLE.cDivision  ,'CDIVISION'), ;
*!*	            FInvline.InvDate WITH INVHDR.InvDate
          REPLACE FInvline.Season WITH gfCodDes(STYLE.Season  ,'SEASON'),;
            FInvline.cStyGroup WITH gfCodDes(STYLE.cStyGroup ,'CSTYGROUP'),;
            FInvline.cDivision WITH gfCodDes(STYLE.cDivision  ,'CDIVISION'), ;
            FInvline.Tran_Date WITH INVHDR.InvDate
         *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][ENd]
          *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][End]
          *!E303647,1 MMT 03/06/2016 Add CustPO field to AR Gross profit report excel[T20160125.0010][Start]
          REPLACE FInvline.CUSTPO WITH INVHDR.CUSTPO
          *!E303647,1 MMT 03/06/2016 Add CustPO field to AR Gross profit report excel[T20160125.0010][End]

          *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[Start]
          *lnGross  = invline.totqty*IIF(llCallGfam,gfAmntDisp(invline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam),invline.Price)
          lnGross  = INVLINE.totqty*IIF(llCallGfam,gfAmntDisp(INVLINE.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'INVHDR','ccurrcode'),INVLINE.Price)
          *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[End]

          *!E302638,1 09/24/2009 MMT add option deduct trade disc in gross profit reports(So,AR)[Start]
          *lnNet    = lnGross * (1 - (invhdr.DiscPcnt/100)) * (1 - (invhdr.Trde_Disc/100))

          *!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[BEGIN]
          IF M_TrdDiscL
            lnNet    = lnGross * (1 - (INVHDR.DiscPcnt/100)) * (1 - (IIF(llRPTrdDsc,INVLINE.Trde_Disc,0)/100))
          ELSE
            *!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[END]
            lnNet    = lnGross * (1 - (INVHDR.DiscPcnt/100)) * (1 - (IIF(llRPTrdDsc,INVHDR.Trde_Disc,0)/100))
            *!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[BEGIN]
          ENDIF
          *!E302748,1 09/02/2010 SMA Modify gross profit reports(SO,AR)to deduct the trade discount per line.....[END]

          *!E302638,1 09/24/2009 MMT add option deduct trade disc in gross profit reports(So,AR)[End]
           
            *!B611788,1 HMS 18/06/2019 - Company S2 - A/R Gross Profit data on style P19B17Tx - Gross profit figures are crazy [T20190604.0001][Start]
            *lnCost   = INVLINE.totqty * IIF(!llCallGfam,lfGetFCurr(INVLINE.Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),INVLINE.Cost)
            lnCost   = lfGetLineCost(INVLINE.invoice,INVLINE.LineNo,INVLINE.Style)
            *!B611788,1 HMS 18/06/2019 - Company S2 - A/R Gross Profit data on style P19B17Tx - Gross profit figures are crazy [T20190604.0001][End]
          
          lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
          lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)

          *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[Start]
          *      REPLACE FInvline.SPrice    WITH IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price), ;
          FInvline.NetAmnt   WITH lnNet, ;
          FInvline.lnCost    WITH lnCost, ;
          FInvline.Profit    WITH lnNet - lnCost, ;
          FInvline.Percnt    WITH lnPercnt, ;
          FInvline.GrossAmnt WITH lnGross

          REPLACE FInvline.sprice    WITH IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'INVHDR','ccurrcode'),Price), ;
            FInvline.NetAmnt   WITH lnNet, ;
            FInvline.lnCost    WITH lnCost, ;
            FInvline.Profit    WITH lnNet - lnCost, ;
            FInvline.Percnt    WITH lnPercnt, ;
            FInvline.GrossAmnt WITH lnGross
          *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[End]
          *E304133 , 1 HMS , 04/07/2019 - ERP - Gross Profit Report [T20190301.0007][Begin]
          REPLACE FInvline.SO_GRS_AMT WITH INVLINE.TotQty * INVLINE.gros_price
          *E304133 , 1 HMS , 04/07/2019 - ERP - Gross Profit Report [T20190301.0007][End]
        ENDSCAN
      ENDIF
    ENDSCAN
    *B609746,1 MMT 11/27/2011 Getting an error message when running gross profit report[Start]
  ENDIF
  *B609746,1 MMT 11/27/2011 Getting an error message when running gross profit report[END]


  *!B608382,2 01/22/2008 MMT Fix bug of dealing with Rethdr and retline as Fox tables[Start]
  IF 'RM' $ oAriaApplication.CompanySetupModules
    *!B608382,2 01/22/2008 MMT Fix bug of dealing with Rethdr and retline as Fox tables[End]
    IF llRPIncRet
      *!B608382,1 12/16/2007 MMT Fix bug of dealing with Rethdr and retline as Fox tables[Start]
      *B609746,1 MMT 11/27/2011 Getting an error message when running gross profit report[Start]
      *!*	  *Account Selected
      *!*	  llRetAccount = .F.
      *!*	  lcAccFile = ''
      *!*	  lnPosAcc = ASCAN(loOgScroll.laOgFXFlt,"INVHDR.ACCOUNT")
      *!*	  IF lnPosAcc > 0
      *!*	    lnPosAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosAcc,1)
      *!*	    lcAccFile = loOgScroll.laOgFxFlt[lnPosAcc,6]
      *!*	    IF !EMPTY(lcAccFile)
      *!*	      SELECT(lcAccFile)
      *!*	      LOCATE
      *!*	      IF !EOF()
      *!*	        llRetAccount  = .T.
      *!*	      ENDIF
      *!*	    ENDIF
      *!*	  ENDIF
      *!*
      *!*	  *Date Range Selected
      *!*	  lnDatePos = ASCAN(loOgScroll.laOGFxFlt,'INVHDR.INVDATE')
      *!*	  ldStartDate = {}
      *!*	  ldEndDate   = {}
      *!*	  lcDateValue  = ""
      *!*	  IF lnDatePos > 0
      *!*	    lnDatePos = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnDatePos,1)
      *!*	    lcDateValue = loOgScroll.laOGFxFlt[lnDatePos,6]
      *!*	    IF !EMPTY(lcDateValue)
      *!*	      ldStartDate = CTOD(SUBSTR(loOgScroll.laOGFxFlt[lnDatePos,6],1,10))
      *!*	      ldEndDate   = CTOD(SUBSTR(loOgScroll.laOGFxFlt[lnDatePos,6],12,21))
      *!*	    ENDIF
      *!*	  ENDIF
      *!*
      *!*	  *CUSTOMER.CLASS
      *!*	  llRetCustCls = .F.
      *!*	  lcCustCLFile = ''
      *!*	  lnPosCls = ASCAN(loOgScroll.laOgFXFlt,"CUSTOMER.CLASS")
      *!*	  IF lnPosCls > 0
      *!*	    lnPosCls  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosCls,1)
      *!*	    lcCustClsC = loOgScroll.laOgFxFlt[lnPosCls,6]
      *!*	    IF !EMPTY(lcCustClsC)
      *!*	      lcCustCLFile = loOGScroll.gfTempName()
      *!*	      llRetCustCls  = IIF(LEN(lcCustClsC )>0,.T.,.F.) AND lfConvertToCursor(lcCustClsC ,'CUSTCLS',lcCustCLFile)
      *!*	    ENDIF
      *!*	  ENDIF
      *!*
      *!*	  *RETHDR.CDIVISION
      *!*	  llRetDiv = .F.
      *!*	  lcCDivFile = ''
      *!*	  lnPosDive = ASCAN(loOgScroll.laOgFXFlt,"INVHDR.CDIVISION")
      *!*	  IF lnPosDive > 0
      *!*	    lnPosDive  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDive,1)
      *!*	    lcDivStr = loOgScroll.laOgFxFlt[lnPosDive,6]
      *!*	    IF !EMPTY(lcDivStr)
      *!*	      lcCDivFile = loOGScroll.gfTempName()
      *!*	      llRetDiv = IIF(LEN(lcDivStr)>0,.T.,.F.) AND lfConvertToCursor(lcDivStr,'CDIVISION',lcCDivFile )
      *!*	    ENDIF
      *!*	  ENDIF
      *!*

      *!*

      *!*	  *STYLE.CSTYGROUP
      *!*	  llRetStyGrp = .F.
      *!*	  lcStyGrpFile = ''
      *!*	  lnPosGrp = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYGROUP")
      *!*	  IF lnPosGrp> 0
      *!*	    lnPosGrp= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosGrp,1)
      *!*	    lcStyGRp = loOgScroll.laOgFxFlt[lnPosGrp,6]
      *!*	    IF !EMPTY(lcStyGRp)
      *!*	      lcStyGrpFile = loOGScroll.gfTempName()
      *!*	      llRetStyGrp = IIF(LEN(lcStyGRp)>0,.T.,.F.) AND lfConvertToCursor(lcStyGRp,'CSTYGRP',lcStyGrpFile)
      *!*	    ENDIF
      *!*	  ENDIF
      *!*
      *!*
      *!*	  *STYLE.Season
      *!*	  llRetSeason = .F.
      *!*	  lcStySeaFile = ''
      *!*	  lnPosSea = ASCAN(loOgScroll.laOgFXFlt,"INVHDR.SEASON")
      *!*	  IF lnPosSea> 0
      *!*	    lnPosSea= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSea,1)
      *!*	    lcStySea= loOgScroll.laOgFxFlt[lnPosSea,6]
      *!*	    IF !EMPTY(lcStySea)
      *!*	      lcStySeaFile= loOGScroll.gfTempName()
      *!*	      llRetSeason = IIF(LEN(lcStySea)>0,.T.,.F.) AND lfConvertToCursor(lcStySea,'CSTYGRP',lcStySeaFile)
      *!*	    ENDIF
      *!*	  ENDIF

      *!*
      *!*
      *!*	  *RETHDR.SALESREP1
      *!*	  llRetStyle = .F.
      *!*	  lcStyleFile = ''
      *!*	  lnPosStyle = ASCAN(loOgScroll.laOgFXFlt,"INVLINE.STYLE")
      *!*	  IF lnPosStyle > 0
      *!*	    lnPosStyle = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosStyle,1)
      *!*	    lcStyleFile = loOgScroll.laOgFxFlt[lnPosStyle,6]
      *!*	    IF !EMPTY(lcStyleFile)
      *!*	      SELECT(lcStyleFile)
      *!*	      LOCATE
      *!*	      IF !EOF()
      *!*	        llRetStyle = .T.
      *!*	      ENDIF
      *!*	    ENDIF
      *!*	  ENDIF
      *!*
      *!*
      *!*	  *RETHDR.SALESREP1
      *!*	  llRetRep1 = .F.
      *!*	  lcRepFile = ''
      *!*	  lnPosRep1  = ASCAN(loOgScroll.laOgFXFlt,"INVHDR.REP1")
      *!*	  IF lnPosRep1 > 0
      *!*	    lnPosRep1 = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosRep1 ,1)
      *!*	    lcRepFile = loOgScroll.laOgFxFlt[lnPosRep1 ,6]
      *!*	    IF !EMPTY(lcRepFile )
      *!*	      SELECT(lcRepFile )
      *!*	      LOCATE
      *!*	      IF !EOF()
      *!*	        llRetRep1 = .T.
      *!*	      ENDIF
      *!*	    ENDIF
      *!*	  ENDIF

      *!*	  *INVHDR.CCURRCODE
      *!*

      *!*
      *!*	  llRetCurr = .F.
      *!*	  lcCurrFile = ''
      *!*	  lnPosCurr  = ASCAN(loOgScroll.laOgFXFlt,"INVHDR.CCURRCODE")
      *!*	  IF lnPosCurr > 0
      *!*	    lnPosCurr = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosCurr,1)
      *!*	    lcCurrFile= loOgScroll.laOgFxFlt[lnPosCurr,6]
      *!*	    IF !EMPTY(lcCurrFile) AND USED(lcCurrFile)
      *!*	      SELECT(lcCurrFile)
      *!*	      LOCATE
      *!*	      IF !EOF()
      *!*	        llRetCurr = .T.
      *!*	      ENDIF
      *!*	    ENDIF
      *!*	  ENDIF
      *B609746,1 MMT 11/27/2011 Getting an error message when running gross profit report[END]


      IF llRetAccount
        SELECT Rethdr
        gfSetOrder("RethdrA")
        SELECT (lcAccFile)
        SCAN
          IF gfSeek(&lcAccFile..account,'Rethdr','RethdrA')
            SELECT Rethdr
            *!B611140,1 MMT 04/12/2016 AR Gross profit report does not consider rep2 in filtering with Sales rep.[T20160407.0044][Start]
            *!*	          SCAN REST WHILE account+crmemo = &lcAccFile..Account FOR RETHDR.STATUS<>'V' AND;
            *!*	              IIF(!EMPTY(lcDateValue),BETWEEN(Rethdr.CRDATE,ldStartDate ,ldEndDate),.T.) AND ;
            *!*	              SEEK('M'+&lcAccFile..Account ,"Customer") AND IIF(llRetCustCls  ,SEEK(CUSTOMER.CLASS,lcCustCLFile),.T.) AND ;
            *!*	              IIF(llRetRep1 ,SEEK(RETHDR.SALESREP1,lcRepFile),.T.) AND ;
            *!*	              IIF(llRetCurr ,SEEK(RETHDR.ccurrcode,lcCurrFile),.T.) AND ;
            *!*	              IIF(llRetDiv ,SEEK(RETHDR.CDIVISION,lcCDivFile ),.T.) AND IIF(!EMPTY(RETHDR.SALESREP1),SEEK(RETHDR.SALESREP1,'SALESREP'),.T.)
            SCAN REST WHILE account+crmemo = &lcAccFile..account FOR Rethdr.STATUS<>'V' AND;
                IIF(!EMPTY(lcDateValue),BETWEEN(Rethdr.CRDATE,ldStartDate ,ldEndDate),.T.) AND ;
                SEEK('M'+&lcAccFile..account ,"Customer") AND IIF(llRetCustCls  ,SEEK(CUSTOMER.CLASS,lcCustCLFile),.T.) AND ;
                IIF(llRetRep1 ,SEEK(Rethdr.SALESREP1,lcRepFile) OR SEEK(Rethdr.SALESREP2,lcRepFile),.T.) AND ;
                IIF(llRetCurr ,SEEK(Rethdr.ccurrcode,lcCurrFile),.T.) AND ;
                IIF(llRetDiv ,SEEK(Rethdr.cDivision,lcCDivFile ),.T.) AND IIF(!EMPTY(Rethdr.SALESREP1),SEEK(Rethdr.SALESREP1,'SALESREP'),.T.)
              *!B611140,1 MMT 04/12/2016 AR Gross profit report does not consider rep2 in filtering with Sales rep.[T20160407.0044][End]
              IF gfSeek(Rethdr.crmemo,'RETLINE')
                SELECT RETLINE
                *B608724,1 10/15/2008 WAM don't show canceled return lines
                *SCAN REST WHILE crmemo+style+cret_linno+cret_trncd = Rethdr.CrMemo FOR ;
                IIF(llRetStyle ,SEEK(RETLINE.Style,lcStyleFile ),.T.) AND ;
                SEEK(RETLINE.Style,'Style') AND  IIF(llRetSeason ,SEEK(Style.Season,lcStySeaFile),.T.) AND ;
                IIF(llRetStyGrp ,SEEK(STYLE.CSTYGROUP,LcStyGrpFile ),.T.)
                SCAN REST WHILE crmemo+STYLE+cret_linno+cret_trncd = Rethdr.crmemo FOR cret_trncd = '2' AND ;
                    IIF(llRetStyle ,SEEK(RETLINE.STYLE,lcStyleFile ),.T.) AND ;
                    SEEK(RETLINE.STYLE,'Style') AND  IIF(llRetSeason ,SEEK(STYLE.Season,lcStySeaFile),.T.) AND ;
                    IIF(llRetStyGrp ,SEEK(STYLE.cStyGroup,lcStyGrpFile ),.T.)
                  *B608724,1 10/15/2008 WAM (End)

                  SCATTER MEMVAR MEMO
                  *B611483,1 AHH 05/12/2017 The Account column is exported as empty for Credit memo lines in AR Gross profit report[T20171117.0009][Start]
                  m.account = Rethdr.account
                  *B611483,1 AHH 05/12/2017 The Account column is exported as empty for Credit memo lines in AR Gross profit report[T20171117.0009][End]
                  INSERT INTO FInvline FROM MEMVAR
                  REPLACE FInvline.TYPE      WITH 'RET', ;
                    FInvline.totqty    WITH -FInvline.totqty, ;
                    FInvline.CODE      WITH RETLINE.crmemo, ;
                    FInvline.NAME      WITH CUSTOMER.btname, ;
                    FInvline.ccurrcode WITH Rethdr.ccurrcode, ;
                    FInvline.CLASS     WITH CUSTOMER.CLASS,  ;
                    FInvline.ClassName WITH gfCodDes(CUSTOMER.CLASS,'CLASS'),  ;
                    FInvline.rep1      WITH Rethdr.SALESREP1, ;
                    FInvline.RepName   WITH SALESREP.NAME, ;
                    FInvline.nexrate   WITH Rethdr.nexrate, ;
                    FInvline.ncurrunit WITH Rethdr.ncurrunit

                  *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][Start]
                  REPLACE FInvline.Season WITH gfCodDes(STYLE.Season  ,'SEASON') ,;
                    FInvline.cStyGroup WITH gfCodDes(STYLE.cStyGroup,'CSTYGROUP'),;
                    FInvline.cDivision WITH gfCodDes(STYLE.cDivision  ,'CDIVISION')
                  
                  *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][End]
                  *!E303647,1 MMT 03/06/2016 Add CustPO field to AR Gross profit report excel[T20160125.0010][Start]
                  REPLACE FInvline.CUSTPO WITH Rethdr.CUSTPO
                  *!E303647,1 MMT 03/06/2016 Add CustPO field to AR Gross profit report excel[T20160125.0010][End]
                  
                  IF EMPTY(FInvline.invoice)
                    REPLACE FInvline.invoice WITH 'ZZZZZZ'
                  ENDIF
                  lnQty    = -(RETLINE.totqty)
                  *!B609876,1 SAB 03/26/2012 Fix calculation of gross price in case of credit memo [T20120322.0037][Start]
                  *lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(retline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),retline.Price)
                  *lnNet    = lnGross
                  lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(RETLINE.GROS_Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),RETLINE.GROS_Price)
                  *!B610142,1 MMT 11/08/2012 Fix bug of not calculating trade discount of Credit memo is not created from Invoice[Start]
                  *lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(retline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),retline.Price)) * ;
                  IIF(llRPTrdDsc and !EMPTY(RETHDR.INVOICE) and gfSeek(RETHDR.INVOICE,'INVHDR','INVHDR'),(1 - (INVHDR.Trde_Disc/100)),1)
                  IF !llRPTrdDsc
                    lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(RETLINE.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),RETLINE.Price))
                  ELSE
                    IF !EMPTY(Rethdr.invoice) AND gfSeek(Rethdr.invoice,'INVHDR','INVHDR')
                      *!B610349,1 HIA 06/02/13 T20130509.0007 - Gross profit invoiced report [Start]
                      *lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(retline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),retline.Price)) * ;
                      *           (1 - (INVHDR.Trde_Disc/100))
                      *!B610951,1 MMT 02/18/2015 Fix the incorrect trade amount field used from retline[T20150216.0027][Start]
                      *!*	                    lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(retline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),retline.Price)) * ;
                      *!*	                      (1 - (retline.trde_amnt/100))
                      *!B611187,1 MMT 09/08/2016 Issue#31:AR gross profit calculates Returns net amount incorreectly[P20160601.0001][Start]
                      *lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(retline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),retline.Price)) * ;
                      (1 - (retline.trde_amt/100))
                      lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(RETLINE.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),RETLINE.Price)) + RETLINE.trde_amt
                      *!B611187,1 MMT 09/08/2016 Issue#31:AR gross profit calculates Returns net amount incorreectly[P20160601.0001][End]
                      *!B610951,1 MMT 02/18/2015 Fix the incorrect trade amount field used from retline[T20150216.0027][End]
                      *!B610349,1 HIA 06/02/13 T20130509.0007 - Gross profit invoiced report [End]
                      *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][Start]
                     *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][Begin]
                      *REPLACE  FInvline.InvDate WITH INVHDR.InvDate
                     *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][End]
                      *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][End]

                    ELSE
                      *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][Start]
                      *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][Begin]
                      *REPLACE  FInvline.InvDate WITH {}
                      *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][End]
                      *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][End]

                      DECLARE laTRltFld[1,2]
                      laTRltFld[1,1] = 'NTERDISCR'
                      laTRltFld[1,2] = 'lnTrde_Disc' && Trade Discount
                      STORE 0   TO lnTrde_Disc
                      =gfRltFld(Rethdr.cTermCode,@laTRltFld,'CTERMCODE')
                      lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(RETLINE.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),RETLINE.Price)) * ;
                        (1 - (lnTrde_Disc/100))
                    ENDIF
                  ENDIF
                  *!B610142,1 MMT 11/08/2012 Fix bug of not calculating trade discount of Credit memo is not created from Invoice[End]
                  *!B609876,1 SAB 03/26/2012 Fix calculation of gross price in case of credit memo [T20120322.0037][End]
                  *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][Begin]
                   REPLACE  FInvline.Tran_Date WITH RETHDR.CrDate
                  *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][End]
                  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(RETLINE.Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),RETLINE.Cost)
                  lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
                  lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)
                  REPLACE FInvline.sprice    WITH IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),Price), ;
                    FInvline.NetAmnt   WITH lnNet, ;
                    FInvline.lnCost    WITH lnCost, ;
                    FInvline.Profit    WITH lnNet - lnCost, ;
                    FInvline.Percnt    WITH lnPercnt

                  IF llRPSubRet
                    REPLACE FInvline.GrossAmnt WITH lnGross
                  ENDIF
                ENDSCAN
              ENDIF
            ENDSCAN
          ENDIF
        ENDSCAN
      ELSE
        lcSelectStat = "Select  *  From Rethdr where RETHDR.Status<>'V'"
        IF !EMPTY(lcDateValue)
          lcSelectStat  = lcSelectStat  + " AND Rethdr.CRDATE BETWEEN '"+ DTOS(ldStartDate) +"' AND '" + DTOS(ldEndDate)+ "'"
        ENDIF
        =gfSqlRun(lcSelectStat  ,"Rethdr")



        SELECT Rethdr
        *!B611140,1 MMT 04/12/2016 AR Gross profit report does not consider rep2 in filtering with Sales rep.[T20160407.0044][Start]
        *!*	      SCAN FOR RETHDR.STATUS<>'V' AND;
        *!*	          IIF(!EMPTY(lcDateValue),BETWEEN(Rethdr.CRDATE,ldStartDate ,ldEndDate),.T.) AND ;
        *!*	          SEEK('M'+RETHDR.Account ,"Customer") AND IIF(llRetCustCls  ,SEEK(CUSTOMER.CLASS,lcCustCLFile),.T.) AND ;
        *!*	          IIF(llRetRep1 ,SEEK(RETHDR.SALESREP1,lcRepFile),.T.) AND ;
        *!*	          IIF(llRetCurr ,SEEK(RETHDR.ccurrcode,lcCurrFile),.T.) AND ;
        *!*	          IIF(llRetDiv ,SEEK(RETHDR.CDIVISION,lcCDivFile ),.T.) AND IIF(!EMPTY(RETHDR.SALESREP1),SEEK(RETHDR.SALESREP1,'SALESREP'),.T.)
        SCAN FOR Rethdr.STATUS<>'V' AND;
            IIF(!EMPTY(lcDateValue),BETWEEN(Rethdr.CRDATE,ldStartDate ,ldEndDate),.T.) AND ;
            SEEK('M'+Rethdr.account ,"Customer") AND IIF(llRetCustCls  ,SEEK(CUSTOMER.CLASS,lcCustCLFile),.T.) AND ;
            IIF(llRetRep1 ,SEEK(Rethdr.SALESREP1,lcRepFile) OR SEEK(Rethdr.SALESREP2,lcRepFile),.T.) AND ;
            IIF(llRetCurr ,SEEK(Rethdr.ccurrcode,lcCurrFile),.T.) AND ;
            IIF(llRetDiv ,SEEK(Rethdr.cDivision,lcCDivFile ),.T.) AND IIF(!EMPTY(Rethdr.SALESREP1),SEEK(Rethdr.SALESREP1,'SALESREP'),.T.)
          *!B611140,1 MMT 04/12/2016 AR Gross profit report does not consider rep2 in filtering with Sales rep.[T20160407.0044][End]
          IF gfSeek(Rethdr.crmemo,'RETLINE')
            SELECT RETLINE
            *B608724,1 10/15/2008 WAM don't show canceled return lines
            *SCAN REST WHILE crmemo+style+cret_linno+cret_trncd = Rethdr.CrMemo FOR ;
            IIF(llRetStyle ,SEEK(RETLINE.Style,lcStyleFile ),.T.) AND ;
            SEEK(RETLINE.Style,'Style') AND  IIF(llRetSeason ,SEEK(Style.Season,lcStySeaFile),.T.) AND ;
            IIF(llRetStyGrp ,SEEK(STYLE.CSTYGROUP,LcStyGrpFile ),.T.)
            SCAN REST WHILE crmemo+STYLE+cret_linno+cret_trncd = Rethdr.crmemo FOR cret_trncd = '2' AND ;
                IIF(llRetStyle ,SEEK(RETLINE.STYLE,lcStyleFile ),.T.) AND ;
                SEEK(RETLINE.STYLE,'Style') AND  IIF(llRetSeason ,SEEK(STYLE.Season,lcStySeaFile),.T.) AND ;
                IIF(llRetStyGrp ,SEEK(STYLE.cStyGroup,lcStyGrpFile ),.T.)
              *B608724,1 10/15/2008 WAM (End)

              SCATTER MEMVAR MEMO
              *B611483,1 AHH 05/12/2017 The Account column is exported as empty for Credit memo lines in AR Gross profit report[T20171117.0009][Start]
              m.account = Rethdr.account
              *B611483,1 AHH 05/12/2017 The Account column is exported as empty for Credit memo lines in AR Gross profit report[T20171117.0009][End]
              INSERT INTO FInvline FROM MEMVAR
              REPLACE FInvline.TYPE      WITH 'RET', ;
                FInvline.totqty    WITH -FInvline.totqty, ;
                FInvline.CODE      WITH RETLINE.crmemo, ;
                FInvline.NAME      WITH CUSTOMER.btname, ;
                FInvline.ccurrcode WITH Rethdr.ccurrcode, ;
                FInvline.CLASS     WITH CUSTOMER.CLASS,  ;
                FInvline.ClassName WITH gfCodDes(CUSTOMER.CLASS,'CLASS'),  ;
                FInvline.rep1      WITH Rethdr.SALESREP1, ;
                FInvline.RepName   WITH SALESREP.NAME, ;
                FInvline.nexrate   WITH Rethdr.nexrate, ;
                FInvline.ncurrunit WITH Rethdr.ncurrunit

              *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][Start]
              REPLACE FInvline.Season WITH gfCodDes(STYLE.Season ,'SEASON'),;
                FInvline.cStyGroup WITH gfCodDes(STYLE.cStyGroup ,'CSTYGROUP'),;
                FInvline.cDivision WITH gfCodDes(STYLE.cDivision  ,'CDIVISION')
              *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][End]
              *!E303647,1 MMT 03/06/2016 Add CustPO field to AR Gross profit report excel[T20160125.0010][Start]
              REPLACE FInvline.CUSTPO WITH Rethdr.CUSTPO
              *!E303647,1 MMT 03/06/2016 Add CustPO field to AR Gross profit report excel[T20160125.0010][End]


              IF EMPTY(FInvline.invoice)
                REPLACE FInvline.invoice WITH 'ZZZZZZ'
              ENDIF
              lnQty    = -(RETLINE.totqty)
              *!B609876,1 SAB 03/26/2012 Fix calculation of gross price in case of credit memo [T20120322.0037][Start]
              *lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(retline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),retline.Price)
              *lnNet    = lnGross
              lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(RETLINE.GROS_Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),RETLINE.GROS_Price)
              *!B610142,1 MMT 11/08/2012 Fix bug of not calculating trade discount of Credit memo is not created from Invoice[Start]
              *lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(retline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),retline.Price)) * ;
              IIF(llRPTrdDsc and !EMPTY(RETHDR.INVOICE) and gfSeek(RETHDR.INVOICE,'INVHDR','INVHDR'),(1 - (INVHDR.Trde_Disc/100)),1)
              IF !llRPTrdDsc
                lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(RETLINE.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),RETLINE.Price))
              ELSE
                IF !EMPTY(Rethdr.invoice) AND gfSeek(Rethdr.invoice,'INVHDR','INVHDR')
                  *!B610349,1 HIA 06/02/13 T20130509.0007 - Gross profit invoiced report [Start]
                  *lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(retline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),retline.Price)) * ;
                  *  (1 - (INVHDR.Trde_Disc/100))
                  *!B610951,1 MMT 02/18/2015 Fix the incorrect trade amount field used from retline[T20150216.0027][Start]
                  *!*	                lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(retline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),retline.Price)) * ;
                  *!*	                  (1 - (retline.trde_amnt/100))
                  *!B611187,1 MMT 09/08/2016 Issue#31:AR gross profit calculates Returns net amount incorreectly[P20160601.0001][Start]
                  *!*	                lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(retline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),retline.Price)) * ;
                  *!*	                  (1 - (retline.trde_amt/100))
                  lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(RETLINE.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),RETLINE.Price)) + RETLINE.trde_amt
                  *!B611187,1 MMT 09/08/2016 Issue#31:AR gross profit calculates Returns net amount incorreectly[P20160601.0001][End]
                  *!B610951,1 MMT 02/18/2015 Fix the incorrect trade amount field used from retline[T20150216.0027][End]
                  *!B610349,1 HIA 06/02/13 T20130509.0007 - Gross profit invoiced report [End]
                  *!B611691,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][Begin]
                  *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][Start]
                  *REPLACE  FInvline.InvDate WITH INVHDR.InvDate
                  *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][End]
                  *!B611691,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][END]
                ELSE
                  *!B611691,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][Begin]
                  *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][Start]
                  * REPLACE  FInvline.InvDate WITH {}
                  *!E303554,1 MMT 02/22/2015 Add Division,season, group, and invdate to gross profit excel[T20150203.0015][End]
                  *!B611691,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][END]
                  DECLARE laTRltFld[1,2]
                  laTRltFld[1,1] = 'NTERDISCR'
                  laTRltFld[1,2] = 'lnTrde_Disc' && Trade Discount
                  STORE 0   TO lnTrde_Disc
                  =gfRltFld(Rethdr.cTermCode,@laTRltFld,'CTERMCODE')
                  lnNet    = (lnQty*IIF(llCallGfam,gfAmntDisp(RETLINE.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),RETLINE.Price)) * ;
                    (1 - (lnTrde_Disc/100))
                ENDIF
              ENDIF
              *!B610142,1 MMT 11/08/2012 Fix bug of not calculating trade discount of Credit memo is not created from Invoice[END]
              *!B609876,1 SAB 03/26/2012 Fix calculation of gross price in case of credit memo [T20120322.0037][End]
               *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][Begin]
                REPLACE  FInvline.Tran_Date WITH RETHDR.CrDate
               *B611691 ,1 HMS 11/13/2018 - Gross Profit report is exporting data outside [T20181106.0001][End]
              lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(RETLINE.Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),RETLINE.Cost)
              lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
              lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)
              REPLACE FInvline.sprice    WITH IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),Price), ;
                FInvline.NetAmnt   WITH lnNet, ;
                FInvline.lnCost    WITH lnCost, ;
                FInvline.Profit    WITH lnNet - lnCost, ;
                FInvline.Percnt    WITH lnPercnt

              IF llRPSubRet
                REPLACE FInvline.GrossAmnt WITH lnGross
              ENDIF
            ENDSCAN
          ENDIF
        ENDSCAN
      ENDIF

      *!*    SELECT RETHDR
      *!*
      *!*
      *!*    SCAN FOR EVALUATE(lcRpExp2) AND RETHDR.Status<>'V'
      *!*      SELECT RETLINE
      *!*      IF SEEK(Rethdr.crmemo)
      *!*        *!B608019,1 T20070313.0014  AYM : pROBLEM WHEN SELECTING STYLES OR SSTYLE GROUP .. BEGIN
      *!*        *SCAN WHILE Retline.crmemo = Rethdr.crmemo FOR EVALUATE(lcRpExp2)
      *!*        SCAN WHILE Retline.crmemo = Rethdr.crmemo FOR EVALUATE(lcLineExpR )
      *!*        *!B608019,1 T20070313.0014  AYM : pROBLEM WHEN SELECTING STYLES OR SSTYLE GROUP .. END

      *!*          SCATTER MEMVAR memo
      *!*          INSERT INTO FInvline FROM MEMVAR
      *!*          REPLACE FInvline.type      WITH 'RET', ;
      *!*                  FInvline.totqty    WITH -FInvline.totqty, ;
      *!*                  FInvline.code      WITH retline.crmemo, ;
      *!*                  FInvline.name      WITH customer.btname, ;
      *!*                  FInvline.ccurrcode WITH rethdr.ccurrcode, ;
      *!*                  FInvline.class     WITH customer.class,  ;
      *!*                  FInvline.classname WITH gfCodDes(customer.class,'CLASS'),  ;
      *!*                  FInvline.rep1      WITH rethdr.SalesRep1, ;
      *!*                  FInvline.repname   WITH salesrep.name, ;
      *!*                  FInvline.nexrate   WITH rethdr.nexrate, ;
      *!*                  FInvline.ncurrunit WITH rethdr.ncurrunit
      *!*          IF EMPTY(FInvline.invoice)
      *!*            REPLACE FInvline.invoice WITH 'ZZZZZZ'
      *!*          ENDIF
      *!*          lnQty    = -(retline.TotQty)
      *!*  		*!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[Start]
      *!*      *    lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(retline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam),retline.Price)
      *!*          lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(retline.Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),retline.Price)
      *!*  		*!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[End]
      *!*          lnNet    = lnGross
      *!*          lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(retline.Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),retline.Cost)
      *!*          lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
      *!*          lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)

      *!*  *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[Start]
      *!*  *        REPLACE FInvline.SPrice    WITH IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price), ;
      *!*                  FInvline.NetAmnt   WITH lnNet, ;
      *!*                  FInvline.lnCost    WITH lnCost, ;
      *!*                  FInvline.Profit    WITH lnNet - lnCost, ;
      *!*                  FInvline.Percnt    WITH lnPercnt

      *!*          REPLACE FInvline.SPrice    WITH IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam,.F.,'RETHDR','CCURRCODE'),Price), ;
      *!*                  FInvline.NetAmnt   WITH lnNet, ;
      *!*                  FInvline.lnCost    WITH lnCost, ;
      *!*                  FInvline.Profit    WITH lnNet - lnCost, ;
      *!*                  FInvline.Percnt    WITH lnPercnt
      *!*  *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[End]
      *!*          IF llRPSubRet
      *!*            REPLACE FInvline.GrossAmnt WITH lnGross
      *!*          ENDIF
      *!*        ENDSCAN
      *!*      ENDIF
      *!*    ENDSCAN
      *!B608382,1 12/16/2007 MMT Fix bug of dealing with Rethdr and retline as Fox tables[END]
    ENDIF
    *!B608382,2 01/22/2008 MMT Fix bug of dealing with Rethdr and retline as Fox tables[Start]
  ENDIF
  *!B608382,2 01/22/2008 MMT Fix bug of dealing with Rethdr and retline as Fox tables[End]

  *!*************************************************************
  *! Name      : lfCreateExp
  *! Developer : Tarek Noaman
  *! Date      : 4/18/2006
  *! Purpose   : Get Report Expression without Ordline Part
  *!*************************************************************
  *! Called from : This Program
  *!*************************************************************
FUNCTION lfCreateExp
  PRIVATE lcTempExp
  lcTempExp=lcRpExp
  IF lcRPInvBy='S'
    lcRpExp=STRTRAN(lcRpExp,'INVHDR.INVDATE','INVHDR.SHIPDATE')
  ENDIF
  IF llRPIncRet
    lcTempExp=STRTRAN(lcTempExp,'INVHDR.INVDATE','RETHDR.CRDATE')
    lcTempExp=STRTRAN(lcTempExp,'INVHDR.ACCOUNT','RETHDR.ACCOUNT')
    lcTempExp=STRTRAN(lcTempExp,'INVLINE.STYLE','RETLINE.STYLE')
    lcTempExp=STRTRAN(lcTempExp,'INVHDR.SEASON','STYLE.SEASON')
    lcTempExp=STRTRAN(lcTempExp,'INVHDR.CDIVISION','RETHDR.CDIVISION')
    lcTempExp=STRTRAN(lcTempExp,'INVHDR.REP1','RETHDR.SALESREP1')
    lcTempExp=STRTRAN(lcTempExp,'INVHDR.CCURRCODE','RETHDR.CCURRCODE')
  ENDIF
  RETURN lcTempExp

  *!*************************************************************
  *! Name      : lfCollTime
  *! Developer : Mohamed Atia Badran (MAB)
  *! Date      : 06/04/99
  *! Purpose   : Calcualte spent time in data collection.
  *!*************************************************************
  *! Passed Parameters  : Start collection date,End collection date
  *!*************************************************************
  *! Returns            : Spent time.
  *!*************************************************************
  *! Example   : =lfCollTime()
  *!*************************************************************
FUNCTION lfCollTime
  PARAMETERS lcStart,lcEnd
  lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
  lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
  lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
  lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
  RETURN (lnEnd - lnStart)
  *-- end of lfCollTime.

  *!*************************************************************
  *! Name      : lfMajTtlGet
  *! Developer : AAMER (AHM)
  *! Date      : 03/25/1998
  *! Purpose   : To get the style major segement title
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfMajTtlGet()
  *!*************************************************************
FUNCTION lfMajTtGet
  RETURN gfItemMask("HM")

  *!*************************************************************
  *! Name      : lfNonMaj
  *! Developer : AAMER (AHM)
  *! Date      : 03/25/1998
  *! Purpose   : To get the style nonmajor segement structure
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfNonMaj()
  *!*************************************************************
FUNCTION lfNonMaj
  *-- Compute Free/Color Items in Style Structure. [Begin]
  lnMajSeg  = gfItemMask('SM')  && No. of major segments.
  DIMENSION laMajSeg[1,1]
  = gfItemMask(@laMajSeg)
  llStopConc = .F.
  *-- Loop Around Non Major elements.
  FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
    lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
    IF laMajSeg[lnI,1] = 'F' AND !llStopConc
      lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
      lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
        lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
      lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
        lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
    ENDIF
    *-- If you Find Color Type or Find Free Type and current type not Free.
    IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
      IF laMajSeg[lnI,1] = 'C'
        lnClrPo    = laMajSeg[lnI,4]
        lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
        lcNonMajPi = laMajSeg[lnI,3]
        lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
        EXIT
      ELSE
        *-- this means that another type is found rather than color or free
        *-- and so we neednot to concat. to free variables
        llStopConc = .T.
      ENDIF
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
  ENDFOR    && end Loop Around Non Major elements.
  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
  *-- Compute Free/Color Items in Style Structure. [End]
  RETURN ''

  *!*************************************************************
  *! Name      : lfchkRet
  *! Developer : AAMER (AHM)
  *! Date      : 05/27/1998
  *! Purpose   : Check existence of RM module
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfchkRet()
  *!*************************************************************
FUNCTION lfchkRet
  RETURN ('RM' $ gcCMpModules)

  *!*************************************************************
  *! Name      : lfvIncRet
  *! Developer : AAMER (AHM)
  *! Date      : 05/27/1998
  *! Purpose   : Validate include returns option
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfvIncRet()
  *!*************************************************************
FUNCTION lfvIncRet
  lnSubRetPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPSubRet'),1)
  laOGObjCnt[lnSubRetPo] = llRPIncRet
  llRPSubRet = IIF(llRPIncRet,llRPSubRet,.F.)
  = lfOGShowGet('llRPSubRet')

  *!*************************************************************
  *! Name      : lfvSumDet
  *! Developer : AAMER (AHM)
  *! Date      : 05/27/1998
  *! Purpose   : Validate Print summary or Detail option
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfvSumDet()
  *!*************************************************************
FUNCTION lfvSumDet
  lnGrnTotPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPGrnTot'),1)
  laOGObjCnt[lnGrnTotPo] = lcRPSumDet = 'S'
  llRPGrnTot = .F.
  = lfOGShowGet('llRPGrnTot')

  *!*************************************************************
  *! Name      : lfMajPic
  *! Developer : AAMER (AHM)
  *! Date      : 05/27/1998
  *! Purpose   : Get major segment picture
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfMajPic()
  *!*************************************************************
FUNCTION lfMajPic
  lcMajPic = "@! " + gfItemMask("PM")
  RETURN lcMajPic

  *!*************************************************************
  *! Name      : lfwCurCode
  *! Developer : Walid Abou El-Magd (WAM)
  *! Date      : 08/17/1999
  *! Purpose   : This function called from the currency field to
  *!             validate the currency.
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example            :
  *!*************************************************************
FUNCTION lfwCurCode
  lcOldCurr = laOGFxFlt[2,6]

  *!*************************************************************
  *! Name      : lfvCurCode
  *! Developer : Walid Abou El-Magd (WAM)
  *! Date      : 08/17/1999
  *! Purpose   : This function called from the currency field to
  *!             validate the currency.
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example            :
  *!*************************************************************
  *B603115,1 New valid function to validate the currency code.
FUNCTION lfvCurCode
  lnAlias=SELECT(0)
  *-- Allow blank curreny in case user have no selected forign currency.
  IF EMPTY(laOGFxFlt[2,6]) .AND. lcRpCurr <> 'F'
    RETURN
  ENDIF
  IF !SEEK(laOGFxFlt[2,6],'SYCCURR') .OR. ATC("?",laOGFxFlt[2,6]) > 0
    SELECT SYCCURR
    DIMENSION laTemp[1]
    laTemp     = ''
    lcSavBrFld = lcBrFields
    lcSavTitle = lcFile_Ttl
    *N000682,1 MMT 02/10/2013 Globalization changes[Start]
    *lcFile_Ttl = "Currency"
    lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CURRTTL,oAriaApplication.GetHeaderText("LANG_CURRTTL",AHEADERFILE))
    lcBrFields = "CCURRCODE :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CURRENCY_CODE,oAriaApplication.GetHeaderText("LANG_CURRENCY_CODE",AHEADERFILE))+"' ," +;
      "CCURRDESC :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESCRIPTION,oAriaApplication.GetHeaderText("LANG_DESCRIPTION",AHEADERFILE))+"' ,  " +;
      "CCURRSMBL :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SYMBOL,oAriaApplication.GetHeaderText("LANG_SYMBOL",AHEADERFILE))+"' "
    *N000682,1 11/20/2012 MMT Globlization changes[End]
    =gfBrows('','CCURRCODE','laTemp')
    lcBrFields = lcSavBrFld
    lcFile_Ttl = lcSavTitle
    IF EMPTY(laTemp[1])
      laOGFxFlt[2,6] = lcOldCurr
    ELSE
      laOGFxFlt[2,6] = laTemp[1]
    ENDIF
  ENDIF
  SHOW GET laOGFxFlt[2,6]
  SELECT(lnAlias)

  *!*************************************************************
  *! Name      : lfvCurDisp
  *! Developer : Walid Abou El-Magd (WAM)
  *! Date      : 08/17/1999
  *! Purpose   : This function called from the currency Display
  *!             field to display currency options screen .
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example            :
  *!*************************************************************
  *B603115,1
FUNCTION lfvCurDisp
  llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
  *-- If forgin currency you must be sure invoice currency has value.
  IF lcRpCurr = 'F' .AND. EMPTY(laOGFxFlt[2,6])
    laOGFxFlt[2,6]= gcBaseCurr
  ENDIF
  SHOW GET laOGFxFlt[2,6]
  *!B610868,1 MMT 10/01/2014 Gross profit report is displaying Grand total with foreign currency[T20140930.0011][Start]
  lnGrnTotPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,UPPER('llRPGrnTot')),1)
  laOGObjCnt[lnGrnTotPo] = lcRpCurr <> 'F'
  llRPGrnTot = .F.
  = lfOGShowGet('llRPGrnTot')
  *!B610868,1 MMT 10/01/2014 Gross profit report is displaying Grand total with foreign currency[T20140930.0011][End]

  *!*************************************************************
  *! Name      : lfRepShow
  *! Developer : Walid Abou El-Magd (WAM)
  *! Date      : 08/17/1999
  *! Purpose   :
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example            :
  *!*************************************************************
  *B603115,1
FUNCTION lfRepShow
  laOGFxFlt[2,6]= gcBaseCurr
  *laOGObjCnt[11] = gfGetMemVar('LLMULCURR')
  =lfOGShowGet("lnRepCurr")

  *B603387,1  (Start)
  *!********************************************************************
  *! Name      : lfGetFCurr
  *! Developer : Sameh Aldesouki (SAM)
  *! Date      : 01/16/2000
  *! Purpose   : Return the Foreign amount From Base currency.
  *!********************************************************************
  *! Parameters: lnAmount     && The amount that you want to display.
  *!           : lcRpDispCur  && The way to display the amount.
  *!           : ldExRateDt   && If you are going to display the amount
  *!           :                 with an exchange rate of a specific date.
  *!           : lcTmepFile   && The temp file name that hold the temp.
  *!           :                 exchange rates.
  *!           : llAprvCurr   && If you are using the Approved currency.
  *!********************************************************************
  *! Call      : From all the AP reports that is using the currency display
  *!           : feature.
  *!********************************************************************
  *! Returns   : lnAmount
  *!********************************************************************
  *! Example   : lfGetFCurr(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.).
  *!********************************************************************
FUNCTION lfGetFCurr
  PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile


  PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias
  lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
  lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
  ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
  lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
  llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)
  lcExSin1    = ''       && Variable to hold the first sign in the equation.
  lcExSin2    = ''       && Variable to hold the second sign in the equation.
  lnSavAlias  = SELECT(0)  && Variable to save the alias.
  lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)
  IF lcRpDispCur = 'F'
    lnExRate   = 0
    lnUnit     = 0
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,FInvline.ccurrcode)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..ccurrcode)
    ENDIF
    IF lcCurrCode = gcBaseCurr
      lnExRate = 1
      lnUnit   = 1
    ELSE
      *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[Start]
      *    ldExRateDt=DATE
      *!B608349,1 11/13/2007 MMT Fix bug of Error if Style Color Contains "AND" word[End]
      *    lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
      lnExRate = FInvline.nexrate
      lnUnit   = FInvline.ncurrunit
    ENDIF
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
    lcExSin1 = IIF(lcExSin1 = '/','*','/')
    lcExSin2 = IIF(lcExSin2 = '*','/','*')
    lnAmount = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
  ENDIF
  SELECT (lnSavAlias)
  RETURN lnAmount
  *-- end of lfGetFCurr.
  *B603387,1(End)

  *!*************************************************************
  *! Name      : lfwRepWhen
  *! Developer : BASSEM RAFAAT (BWA)
  *! Date      : 04/26/2000
  *! Purpose   : Option Grid When function
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfwRepWhen()
  *!*************************************************************
FUNCTION lfwRepWhen
  *!B610868,1 MMT 10/01/2014 Gross profit report is displaying Grand total with foreign currency[T20140930.0011][Start]
  IF llMultCurr
    lnGrnTotPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,UPPER('llRPGrnTot')),1)
    laOGObjCnt[lnGrnTotPo] = lcRpCurr <> 'F'
    llRPGrnTot = .F.
    = lfOGShowGet('llRPGrnTot')
  ENDIF
  *!B610868,1 MMT 10/01/2014 Gross profit report is displaying Grand total with foreign currency[T20140930.0011][End]
  RETURN

  *!*************************************************************
  *! Name      : lfFillVars
  *! Developer : ABDOU ELGENDI - (ABD)
  *! Date      : 07/01/2000
  *! Purpose   : Fill most of report memory variables.
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfFillVars()
  *!*************************************************************
  *E301424,1
FUNCTION lfFillVars
  WAIT WINDOW "Test"
  *E301424,1 ABD- Report support multi currency format open needed files.[begin]
  IF !USED('SYCCOMP')
    USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
    llOpenComp = .T.
  ENDIF


  IF llMultCurr
    *-- Open international file.
    IF !USED("SYCINT")
      USE (gcSysHome+"SYCINT.DBF") IN 0
      llOpenInt = .T.
    ENDIF

    *-- Open exchange rates file.
    IF !USED("SYCEXCH")
      USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG CURRENCY
      llOpenExch = .T.
    ENDIF

    *-- Fill Currency arrays [Begin]
    DIMENSION laCurrVal[1,1]
    *-- Open Currency file.
    IF !USED('SYCCURR')
      llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
    ELSE
      SELECT SYCCURR
      SET ORDER TO ccurrcode  && To VALIDATE currency code.
    ENDIF

    SELECT DISTINCT ccurrcode FROM SYCCURR ORDER BY ccurrcode INTO ARRAY laCurrVal
    DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

    FOR lnI = 1 TO ALEN(laCurrVal,1)
      = SEEK(ALLTRIM(laCurrVal[lnI,1]))
      laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
      laCurrDesc[lnI,1] = ccurrcode + ' - ' + ALLTRIM(CCURRDESC)
    ENDFOR
    *-- Fill Currency arrays [End  ]
  ENDIF

  *-- End Of lfFillVars.


  *:*************************************************************
  *: Name        : lfItmPos
  *: Developer   : Abdou Elgendy. [ABD]
  *: Date        : 05/26/2002
  *: Purpose     : Evaluate fixed filter position within array.
  *:*************************************************************
  *: Passed Parameters : None
  *:*************************************************************
  *: Return      : Position.
  *:*************************************************************
  *: Example     : = lfItmPos()
  *:*************************************************************
FUNCTION lfItmPos
  PARAMETERS lcItmInFlt
  PRIVATE lnItmPos

  lnItmPos = ASCAN(loOgScroll.laOGFxFlt,lcItmInFlt)
  IF lnItmPos > 0
    lnItmPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnItmPos,1)
  ENDIF
  RETURN lnItmPos
  *-- End of lfItmPos.


  *!B608382,1 12/16/2007 MMT Fix bug of dealing with Rethdr and retline as Fox tables[Start]
  *!*************************************************************
  *! Name      : lfConvertToCursor
  *: Developer : MAriam Mazhar (MMT)
  *: Date      : 12/16/2007
  *! Purpose   : Convert a list of values into a cusrsor
  *!*************************************************************
FUNCTION lfConvertToCursor
  PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
  lcCursorTemp = lcNewFile &&Cursor Hold Selected values
  DIMENSION laTempacstru[1,4]
  laTempacstru[1,1] = lcFieldName

  DO CASE

    CASE   ALLTRIM(lcFieldName) = 'SEASON'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0

    CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0

    CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0

    CASE  ALLTRIM(lcFieldName) = 'CUSTCLS'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0

  ENDCASE
  = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
  lcValuesToConvert = lcStrToConv
  IF !EMPTY(lcValuesToConvert)
    lnStart=1
    lnEnd=AT('|',lcValuesToConvert )
    DO WHILE lnEnd <> 0
      SELECT(lcCursorTemp )
      APPEND BLANK
      REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
      lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"")
      lnEnd=AT('|',lcValuesToConvert )
    ENDDO
    IF lnEnd = 0
      SELECT(lcCursorTemp )
      APPEND BLANK
      REPLACE &lcFieldName  WITH lcValuesToConvert
    ENDIF
  ENDIF
  RETURN .T.
  *!B608382,1 12/16/2007 MMT Fix bug of dealing with Rethdr and retline as Fox tables[End]
  
*!B611788,1 HMS 18/06/2019 - Company S2 - A/R Gross Profit data on style P19B17Tx - Gross profit figures are crazy [T20190604.0001][Start]
*!*************************************************************
*! Name      : lfGetLineCost
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/18/2019
*! Purpose   : Get the line cost from Styinvjl as the one saved in INVLINE is inaccurate
*!*************************************************************
FUNCTION lfGetLineCost
LPARAMETERS lcTrCode,lnLineNo,lcStyle
lnSelAlias = SELECT()
IF !USED('STYINVJL')
  =gfOpenTable('STYINVJL','MFGOPR' )  && CTRCODE+COPRCODE+CLOTNO+CTRTYPE+STYLE+CWARECODE
ENDIF
lnTotCost = 0
SELECT STYINVJL
IF gfSeek(lcTrCode)
  SUM -1 * nTotStk* IIF(!llCallGfam,lfGetFCurr(nCost, lcRpCurr , ldRpExDate , lcRpTmpNam),nCost) TO lnTotCost REST WHILE CTRCODE+COPRCODE+CLOTNO+CTRTYPE+STYLE+CWARECODE =lcTrCode FOR;
           CTRTYPE ='3' AND lineNo =lnLineNo AND Style =lcStyle AND CIRTYPE ='I'
ENDIF
SELECT(lnSelAlias)
RETURN lnTotCost
*!B611788,1 HMS 18/06/2019 - Company S2 - A/R Gross Profit data on style P19B17Tx - Gross profit figures are crazy [T20190604.0001][End] 