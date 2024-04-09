*:************************************************************************
*: Program file  : ARSESANL.Prg
*: Program desc. : Seasonal Analysis Report
*: System        : Aria4xp
*: Module        : AR
*: Developer     : Hesham Elmasry (HES)
*: Date          : 04/12/2009
*: Reference     : T20090116.0027 
*: Tracking #    : C201132.exe
*:************************************************************************
*: Calls : 
*: Procedures : CALCULATE()
*: Functions  : lfvSelBy()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO ARACTSUM
*:************************************************************************
*  TRAN CODES             FILE-ID
*  0 = CREDIT MEMOS       CREDIT
*  1 = INVOICES           DEBIT
*  3 = CHARGEBACK IN      DEBIT
*  4 = PAYMENT            CREDIT
*  5 = CREDIT ADJUSTMENT  CREDIT
*  2 = DEBIT ADJUSTMENT   DEBIT
*  6 = CREDIT ON ACCOUNT  CREDIT
*  7 = ALLOWANCE          CREDIT
*  8 = CHR.BCK            ARHIST
*  9 = CREDIT ON ACCT     ARHIST
*:***************************************************************************
*: Modifications :
*C201132,1 HES 08/26/2009 Collect data based on trantype = 1 from arhist
*B609221,1 HES 04/27/2010 seasonal analysis report shows wrong information when printed [T20100405.0008]
*B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[T20100826.0014]
*B610642,1 TMI 12/29/2013 Fix a problem that the report didn't considre the ordered amounts [T20131211.0111] 
*B610802,1 MMT 08/12/2014 report includes voided invoices and credit memos in paid amount[T20140728.0010]
*C201707,1 MMT 08/24/2015 Modifications in Seasonal analysis report for FLO09{T20150319.0008}
*:***************************************************************************

*-- If select by month and month value not entered, or select by date and 
*-- date values not entered, so there is no records to display.

#INCLUDE R:\Aria4xp\reports\so\soordhd.h

*          TMI 04/14/2009 [Start] 

*B609426,1 MMT 12/06/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]
llRpVodInv = .F.
*B609426,1 MMT 12/06/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End]

* HES
*SET DATE USA
* HES

lcCurrency=''
llMultCurr=.F.
*          TMI 04/14/2009 [End  ] 

lcSeason = ""
lcSesStr = ""
lcSesFltr= ""

* Season Filter

IF !EMPTY(laRpSesTarget)
  FOR lnSesTarget = 1 TO ALEN(laRpSesTarget,1)
    lcSesStr = lcSesStr + ", " + ALLTRIM(SUBSTR(laRpSesTarget[lnSesTarget],1,6)) && for displaying in Report Header
    *tmi 07/23/2009
    *lcSesFltr = lcSesFltr + ", " + SUBSTR(laRpSesTarget[lnSesTarget],1,6) + ", *" && for the criteria
    lcSesFltr = lcSesFltr + ", " + SUBSTR(laRpSesTarget[lnSesTarget],1,6) + ", *     " && for the criteria
    *tmi 07/23/2009
  ENDFOR 
  lcSesStr = SUBSTR(lcSesStr,3)
  lcSesFltr = SUBSTR(lcSesFltr,3)
ENDIF

llSesFltr = !EMPTY(lcSesFltr)

IF llSesFltr 
  lcSeason = SUBSTR(lcSesStr,1) + ' .' && will be printed in the report header
  lcSesExp = " AND (INVHDR.SEASON $ lcSesFltr)" 
  lcOrdSesExp = " AND (ORDHDR.SEASON $ lcSesFltr)"
ELSE
  lcSesFltr = ''
*!*	  lcSesExp = " AND (INVHDR.SEASON $ lcSesFltr)"
*!*	  lcOrdSesExp = " AND (ORDHDR.SEASON $ lcSesFltr)"
  lcSesExp = ""
  lcOrdSesExp = ""
ENDIF 

LOCAL lnCount 	
IF !EMPTY(laRpTarget[1])
  FOR lnCount = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnCount] = LANG_Soordhd_Open,'O',;
                              IIF(laRpTarget[lnCount] = LANG_Soordhd_Hold,'H',;
                              IIF(laRpTarget[lnCount] = LANG_Soordhd_Completed,'C','')))

  ENDFOR
ENDIF
lcRpStatus = IIF(EMPTY(lcRpStatus),'OHC',ALLTRIM(lcRpStatus))

IF lcrpSelBy = 'D' 
  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVDATE'),1)
  LDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1))
  HDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],  ATC('|',laOGFxFlt[lnDatePos,6])+1))
	IF EMPTY(lDate) .OR. EMPTY(hDate)
*!*			*-- Message 'There are no records to display...!'
*!*			=gfModalGen('TRM00052B00000','DIALOG')
	*-- Message < You have to type a full month range. >
  *-- Buttons <                  OK                  >
   =gfModalGen("TRM000000B00000","DIALOG",'','','You have to type a full date range.')
		 
		RETURN
	ELSE
      ENDING = LDATE - 1

	ENDIF
ENDIF

IF lcrpSelBy = 'M' 
  IF EMPTY(lcRpMonth) 
*!*	    *-- Message 'There are no records to display...!'
*!*	    =gfModalGen('TRM00052B00000','DIALOG')
    *-- Message < You have to type a full month range. >
  *-- Buttons <                  OK                  >
   =gfModalGen("TRM000000B00000","DIALOG",'','','You have to type a month.')
     RETURN
  ELSE
    IF lcrpSelBy = 'M' .AND. EMPTY(lcRpYear)
      lcRpYear=STR(YEAR(DATE()),4)
    ENDIF
    *          TMI 04/14/2009 [Start] 
    IF VAL(lcRpMonth) > 12 OR VAL(lcRpMonth)< 1
      =gfModalGen("TRM000000B00000","DIALOG",'','','Pls enter a valid month value.')
      RETURN
      
    ENDIF
    *          TMI 04/14/2009 [End  ] 
    LcX    = lcRpMONTH+'/'+'01'+'/'+PADL(lcRpYEAR,4,'0')
		LDATE  = CTOD(LcX)
		NDATE  = LDATE+32
		HDATE  = NDATE-(DAY(NDATE))
		ENDING = LDATE-(DAY(LDATE))
  ENDIF   
ENDIF

*          TMI 04/14/2009 [Start] 
loOGScroll.cCROrientation='L'  


lnRep1Pos = lfItmPos('ORDHDR.REP1')
lnRep2Pos = lfItmPos('ORDHDR.REP2')

llRep1Sel = TYPE('laOgFxFlt[lnRep1Pos,6]') = 'C' AND !EMPTY(laOgFxFlt[lnRep1Pos,6]) AND USED(laOgFxFlt[lnRep1Pos,6])
IF llRep1Sel
  SELECT (laOgFxFlt[lnRep1Pos,6])
  LOCATE
  llRep1Sel = !EOF()
ENDIF
llRep2Sel = TYPE('laOgFxFlt[lnRep2Pos,6]') = 'C' AND !EMPTY(laOgFxFlt[lnRep2Pos,6]) AND USED(laOgFxFlt[lnRep2Pos,6])
IF llRep2Sel
  SELECT (laOgFxFlt[lnRep2Pos,6])
  LOCATE
  llRep2Sel = !EOF()
ENDIF

*          TMI 04/14/2009 [End  ] 

IF llOgFltCh OR .t.
  llDonprnt=.F.
  ****INIT. TOTALS***
  STORE 0.00 TO XTOTOPEN,XTOTSALES,XTOTPAY,XTOTRET, XTOTFALL,XTOTDALL, ;
              XTOTMALL,XTOTBAL
 
  lcseek=" .T. "
  lcCurFltr= lfCheckFilter(1, 'CUSTOMER.ACCOUNT')
  RECCOUNT()
  *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]
  *IF !EMPTY(lcCurFltr)
  lnCnt = 0
  IF !EMPTY(lcCurFltr) AND USED(lcCurFltr)
  *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End]
    SELECT(lcCurFltr)
    COUNT FOR !DELETED() TO lnCnt && Just to count the undeleted records only
  ENDIF 

  llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND lnCnt > 0
  IF llCurFltr   
    SELECT (lcCurFltr)
    INDEX ON ACCOUNT TAG (lcCurFltr)
    lcseek=lcseek+" AND SEEK(CUSTOMER.ACCOUNT ,'"+lcCurFltr+"')"
  ELSE
    IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
      USE IN (lcCurFltr)
    ENDIF
    lcCurFltr= ''
  ENDIF
   
  *-- Array to get the Division long name
  DIMENSION laType[1,2]
  lcType = SPACE(1)
  laType[1,1] = 'ALLOW_TYPE'
  laType[1,2] = 'lcType'
  
  XTIME  = SUBSTR(TIME(),1,5)
  
  llFound =.F.             && Indicate if there are records to print or not.
  
  XTITLE='PERIOD :'+ DTOC(LDATE) + ' --- ' + DTOC(HDATE)
  
  *hes
  *lcTempFile =loOgScroll.gfTempName()
  =lfCrtTmpFile()
  *hes
  
  *B608493,1 WAM 03/26/2008 get returns when RM modules is installed 
  * IF 'MA' $ oAriaApplication.CompanyInstalledModules          
  IF 'RM' $ oAriaApplication.CompanyInstalledModules 
  *B608493,1 WAM 03/26/2008 (End)
    =gfOpenTABLE(oAriaApplication.DATADIR+'RETHDR',oAriaApplication.DATADIR+'RETHDRA','SH')
    *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]
    =gfOpenTABLE('RETLINE','RETLINE','SH')
    *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End]
  ENDIF
  =gfOpenTABLE(oAriaApplication.DATADIR+'SYCCURR',oAriaApplication.DATADIR+'CCURRCODE','SH')
  *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]
  =gfOpenTABLE('STYLE','STYLE','SH')
  *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End]
  
  lnSelBPos = lfItmPos('lcrpSelBy')
  lcDatExpr = BETWEEN(Ordhdr.Complete,LDATE,HDATE)
  * HES

  SELECT CUSTOMER
  =GFSEEK('M')
  SCAN REST WHILE  TYPE='M' FOR &lcseek
  
   *          TMI 04/14/2009 [Start] 
   WAIT WINDOW NOWAIT 'Collecting Data for Account : ' + ACCOUNT
   *          TMI 04/14/2009 [End  ] 
  
  * HES
    
    *B610642,1 TMI 12/29/2013 11:24 [Start] move this check after the ORDHDR amount check and include the Ordered amount check in this function
    *IF !llrpNAct
    *  IF lfCalcAcntActv()
    *    LOOP
    *  ENDIF 
    *ENDIF 
    *B610642,1 TMI 12/29/2013 11:24 [End  ] 
    
    SELECT ORDHDR
    LOCATE 
    =SEEK(CUSTOMER.ACCOUNT,'ORDHDR')
    m.Customer = CUSTOMER.ACCOUNT
    m.Cust_name= CUSTOMER.BTNAME
    m.BOOKED = 0
    *          TMI 04/14/2009 [Start] 
    *SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = CUSTOMER.ACCOUNT AND &lcDatExpr
    SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = CUSTOMER.ACCOUNT FOR BETWEEN(Ordhdr.Complete,LDATE,HDATE)
    
*!*	    IF !(ORDHDR.SEASON $ lcSesFltr)
*!*	      LOOP
*!*	    ENDIF                          
    
    IF IIF(EMPTY(lcSesFltr),.F.,!(ORDHDR.SEASON $ lcSesFltr))
      LOOP
    ENDIF                                        
    *          TMI 04/14/2009 [End  ] 
                   
      IF llRep1Sel
        IF !seek(ordhdr.rep1,laOgFxFlt[lnRep1Pos,6])
          loop
        ENDIF 
      ENDIF          
     
      IF llRep2Sel
        IF !seek(ordhdr.rep2,laOgFxFlt[lnRep2Pos,6])
          LOOP
        ENDIF 
      ENDIF  
      
      IF !ordhdr.status$lcRpStatus
        LOOP
      ENDIF        
     
      m.BOOKED = m.BOOKED + ORDHDR.BOOKAMT
    ENDSCAN
    
    *B610642,1 TMI 12/29/2013 11:24 [Start] moved from above to include the Ordered amount check in the function lfCalcAcntActv
    SELECT CUSTOMER
    IF !llrpNAct
      IF lfCalcAcntActv()
        LOOP
      ENDIF 
    ENDIF 
    *B610642,1 TMI 12/29/2013 11:24 [End  ] 

    SELECT INVHDR
    LOCATE 
    =SEEK(CUSTOMER.ACCOUNT,'INVHDR')
    m.SHIPPED = 0
    m.ShipChrg = 0
    m.Discount = 0
    *          TMI 04/14/2009 [Start] 
    *SCAN REST WHILE ACCOUNT+INVOICE = CUSTOMER.ACCOUNT ;
                FOR &lcDatExpr AND Invhdr.Status <> lcVoInv AND IIF(lcRpFact = 'B',.T.,;
                    IIF(lcRpFact = 'F',!EMPTY(Invhdr.cFaccode),EMPTY(Invhdr.cFaccode)))
    SCAN REST WHILE ACCOUNT+INVOICE = CUSTOMER.ACCOUNT ;
                FOR IIF(llRpVodInv,.T.,Invhdr.Status <> 'V') AND BETWEEN(IIF(INVHDR.STATUS<>'V', ;
                        Invhdr.Invdate,INVHDR.VDATE),LDATE,HDATE) AND IIF(lcRpFact = 'B',.T., ;
                    IIF(lcRpFact = 'F',!EMPTY(Invhdr.cFaccode),EMPTY(Invhdr.cFaccode)))
      *        TMI 04/14/2009 [End  ] 
      
*!*	      IF !(INVHDR.SEASON $ lcSesFltr)
*!*	        LOOP
*!*	      ENDIF  
      *B609426,1 MMT 11/28/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]
      *IF IIF(EMPTY(lcSesFltr),.F.,!(INVHDR.SEASON $ lcSesFltr))      
      IF IIF(EMPTY(lcSesFltr),.F.,!(INVHDR.SEASON $ lcSesFltr) OR INVHDR.SEASON ='*     ')
      *B609426,1 MMT 11/28/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End]
        LOOP
      ENDIF

      IF llRep1Sel
        IF !seek(INVHDR.rep1,laOgFxFlt[lnRep1Pos,6])
          loop
        ENDIF 
      ENDIF          
     
      IF llRep2Sel
        IF !seek(INVHDR.rep2,laOgFxFlt[lnRep2Pos,6])
          LOOP
        ENDIF 
      ENDIF  
      
      m.SHIPPED = m.SHIPPED + Invhdr.Shipamt + INVHDR.VSHIPAMT   && Always exactly one of the two fields shipamt and vshipamt is 0 &&HES
      m.ShipChrg = m.ShipChrg + Invhdr.Freight + Invhdr.VFreight
      m.Discount = m.Discount + Invhdr.Discount + Invhdr.VDiscount

    ENDSCAN
    
    * HES 
    SELECT INVHDR
    SET ORDER TO INVHDRA
    * HES
    SELECT RETHDR
    * HES
    *SET RELATION TO ACCOUNT+INVOICE INTO INVHDR
    * HES
    
    LOCATE 
    =gfSEEK(CUSTOMER.ACCOUNT,'RETHDR')
    m.Returns = 0
    *          TMI 04/14/2009 [Start] 
    *SCAN REST WHILE ACCOUNT+CRMEMO = CUSTOMER.ACCOUNT ;
              FOR &lcDatExpr
    SCAN REST WHILE ACCOUNT+CRMEMO = CUSTOMER.ACCOUNT ;
              FOR BETWEEN(Rethdr.crdate,LDATE,HDATE)
     *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]
     IF !EMPTY(RETHDR.INVOICE)
     *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End]
      SELECT INVHDR
      =gfSeek(RETHDR.ACCOUNT+RETHDR.INVOICE,'INVHDR') 
      *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]
      *IF IIF(EMPTY(lcSesFltr),.F.,!(INVHDR.SEASON $ lcSesFltr))  OR IIF(llRpVodInv,.F.,INVHDR.Status = 'V') OR ;
         IIF(lcRpFact = 'F',EMPTY(INVHDR.CFACCODE),IIF(lcRpFact = 'N',!EMPTY(INVHDR.CFACCODE),.F.))
			IF IIF(lcRpFact = 'F',EMPTY(INVHDR.CFACCODE),IIF(lcRpFact = 'N',!EMPTY(INVHDR.CFACCODE),.F.))         
	    *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End]
        LOOP 
      ENDIF
     *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]
     ELSE
       IF IIF(lcRpFact = 'F',EMPTY(RETHDR.CFACCODE),IIF(lcRpFact = 'N',!EMPTY(RETHDR.CFACCODE),.F.))
         LOOP 
       ENDIF 
     ENDIF
     IF IIF(llRpVodInv,.F.,RETHDR.Status = 'V')
       LOOP 
     ENDIF
     IF !EMPTY(lcSesFltr)
       llSeasonFound = .F.
       =gfSeek(Rethdr.CRMEMO,'RETLINE')
       SELECT RETLINE
       SCAN REST WHILE CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD = Rethdr.CRMEMO                                                                                      
         =gfSeek(RETLINE.STYLE,'STYLE')   
         IF STYLE.SEASON $ lcSesFltr
           llSeasonFound = .T.
           EXIT
         ENDIF
       ENDSCAN 
       IF !llSeasonFound   
         LOOP 
       ENDIF
     ENDIF  
     *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End] 

      IF llRep1Sel
        IF !seek(RETHDR.SALESREP1,laOgFxFlt[lnRep1Pos,6])
          loop
        ENDIF 
      ENDIF          
     
      IF llRep2Sel
        IF !seek(RETHDR.SALESREP2,laOgFxFlt[lnRep2Pos,6])
          LOOP
        ENDIF 
      ENDIF      
      
      m.Returns = m.Returns + Rethdr.Totcredit 
    ENDSCAN

    lcSelectedRep = ''    
    IF llRep1Sel
      SELECT (laOgFxFlt[lnRep1Pos,6])
      LOCATE
      SCAN 
        lcSelectedRep = lcSelectedRep + REPCODE + '|'
      ENDSCAN
    ENDIF
    IF llRep2Sel
      SELECT(laOgFxFlt[lnRep2Pos,6])
      LOCATE
      SCAN
        lcSelectedRep = lcSelectedRep + REPCODE + '|'
      ENDSCAN
    ENDIF
    
    SELECT REPCOMM
    SET RELATION TO ACCOUNT+TRAN INTO INVHDR
    *          TMI 04/14/2009 [Start] 
    m.Compaid = 0
    *          TMI 04/14/2009 [End  ] 
    LOCATE 
    *          TMI 04/14/2009 [Start] 
    *SCAN REST FOR ACCOUNT = CUSTOMER.ACCOUNT ;
              AND &lcDatExpr
                                 
    SCAN REST FOR ACCOUNT = CUSTOMER.ACCOUNT ;
              AND BETWEEN(REPCOMM.Date,LDATE,HDATE) AND TRANTYPE $ '156'
*!*	              
*!*	      IF IIF(EMPTY(lcSesFltr),.F.,IIF(TRANTYPE $ '5',.F. , !(INVHDR.SEASON $ lcSesFltr)));
*!*	                                      OR IIF(llRpVodInv,.F.,INVHDR.STATUS = 'V')         ;
*!*	                                      OR IIF(lcRpFact = 'F',EMPTY(INVHDR.CFACCODE),      ;
*!*	                                         IIF(lcRpFact = 'N',!EMPTY(INVHDR.CFACCODE),.F.))
*!*	         
      
*!*	  _screen.Visible = .T.
*!*	  DEBUG
*!*	  SUSPEND      
*!*	      
      *B609221,1 HES 04/27/2010 seasonal analysis report shows wrong information when printed [Start]
      lcTran = TRAN
      lcAls = ALIAS()
      SELECT ARHIST
      SET ORDER TO ARHISTT
      LOCATE
      IF !SEEK(CUSTOMER.ACCOUNT+lcTran)
        LOOP
      ENDIF 
      SUM Amount REST WHILE Account+Tran = CUSTOMER.ACCOUNT+lcTran TO lnArHsAmnt
      SEEK(CUSTOMER.ACCOUNT+lcTran)
      SELECT(lcAls)
      *B609221,1 HES 04/27/2010 seasonal analysis report shows wrong information when printed [End  ]
      
      *B609426,1 MMT 11/28/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]                                
*!*	      IF IIF(EMPTY(lcSesFltr),.F., !(INVHDR.SEASON $ lcSesFltr)) ;
*!*	              OR IIF(llRpVodInv,.F.,INVHDR.STATUS = 'V')         ;
*!*	              OR IIF(lcRpFact = 'F',EMPTY(INVHDR.CFACCODE),      ;
*!*	                 IIF(lcRpFact = 'N',!EMPTY(INVHDR.CFACCODE),.F.))
*!*	                                     
*!*	        LOOP 
*!*	      ENDIF
      IF IIF(EMPTY(lcSesFltr),.F., !(INVHDR.SEASON $ lcSesFltr) OR INVHDR.SEASON ='*     ') ;
              OR IIF(llRpVodInv,.F.,INVHDR.STATUS = 'V')         ;
              OR IIF(lcRpFact = 'F',EMPTY(INVHDR.CFACCODE),      ;
                 IIF(lcRpFact = 'N',!EMPTY(INVHDR.CFACCODE),.F.))
                                     
        LOOP 
      ENDIF
      *B609426,1 MMT 11/28/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End]

      IF !EMPTY(lcSelectedRep) AND !REPCODE $ lcSelectedRep
        LOOP
      ENDIF
      
      *B609221,1 HES 04/27/2010 seasonal analysis report shows wrong information when printed [Start]
*!*	      m.Compaid = m.Compaid + Repcomm.Amount 
      *C201707,1 MMT 08/24/2015 Modifications in Seasonal analysis report for FLO09{T20150319.0008}[Start]
      *m.Compaid = m.Compaid + lnArHsAmnt * (REPCOMM.CommPcnt/100)
      IF lnArHsAmnt >= invhdr.totalchg && Fully Paid Invoice
        m.Compaid = m.Compaid + invhdr.shipamt * (REPCOMM.CommPcnt/100)
      ELSE && Partially Paid Invoice
        m.Compaid = m.Compaid + lnArHsAmnt * (REPCOMM.CommPcnt/100)
      ENDIF
      *C201707,1 MMT 08/24/2015 Modifications in Seasonal analysis report for FLO09{T20150319.0008}[End]

      *B609221,1 HES 04/27/2010 seasonal analysis report shows wrong information when printed [End  ]
    ENDSCAN
    
    
    SELECT ARHIST
    SET ORDER TO ARHISTT
    LOCATE
    *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]
*    SET RELATION TO ACCOUNT+TRAN INTO INVHDR
    *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End]
    *C201132,1 HES 08/26/2009 [start]
    
*!*	    lcArhstTmp = gfTempName()
*!*	    
*!*	    DIMENSION laTempHstStru[1,4]
*!*		laTempHstStru[1,1] = 'ACCOUNT'
*!*		laTempHstStru[1,2] = 'C'
*!*		laTempHstStru[1,3] = 5
*!*		laTempHstStru[1,4] = 0
*!*		
*!*	    DIMENSION laTempHstStru[1,4]
*!*		laTempHstStru[2,1] = 'HISTORY'
*!*		laTempHstStru[2,2] = 'C'
*!*		laTempHstStru[2,3] = 6
*!*		laTempHstStru[2,4] = 0
*!*		
*!*		DIMENSION laTempHstStru[1,4]
*!*		laTempHstStru[3,1] = 'TRANTYPE'
*!*		laTempHstStru[3,2] = 'C'
*!*		laTempHstStru[3,3] = 1
*!*		laTempHstStru[3,4] = 0
*!*		
*!*		DIMENSION laTempHstStru[1,4]
*!*		laTempHstStru[4,1] = 'TRAN'
*!*		laTempHstStru[4,2] = 'C'
*!*		laTempHstStru[4,3] = 6
*!*		laTempHstStru[4,4] = 0
*!*		
*!*		DECLARE laIndeces[1,2]
*!*		laIndeces[1,1] = 'ACCOUNT+HISTORY+TRANTYPE+TRAN'
*!*		laIndeces[1,2] = 'ARHIS'
*!*	    
*!*		=gfCrtTmp(lcArhstTmp,@laTempHstStru,@laIndeces)
*!*		SELECT(lcArhstTmp)
*!*	    SET ORDER TO ARHIS

*C201132,1 HES 08/26/2009 [END]
    SELECT ARHIST
    SET ORDER TO ARHISTT
    LOCATE
    m.PAID = 0
    
    =SEEK(CUSTOMER.ACCOUNT,'ARHIST')
    *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]
    *SCAN REST WHILE ACCOUNT+TRAN+CINSTALNO  = CUSTOMER.ACCOUNT     
    SCAN REST WHILE ACCOUNT+TRAN+CINSTALNO  = CUSTOMER.ACCOUNT  FOR TRANTYPE $ '10'
      IF TRANTYPE = '1'
        =SEEK(ARHIST.TRAN,'INVHDR','INVHDR')
    *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End]
    *B610802,1 MMT 08/12/2014 report includes voided invoices and credit memos in paid amount[Start]
    IF INVHDR.Status ='V'
      LOOP
    ENDIF
    *B610802,1 MMT 08/12/2014 report includes voided invoices and credit memos in paid amount[End]
    *B609426,1 MMT 11/28/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]
    *  IF IIF(EMPTY(lcSesFltr),.F.,!(INVHDR.SEASON $ lcSesFltr)) OR !BETWEEN(TRANDATE,LDATE,HDATE)    
      IF IIF(EMPTY(lcSesFltr),.F.,!(INVHDR.SEASON $ lcSesFltr) OR INVHDR.SEASON ='*     ') OR !BETWEEN(TRANDATE,LDATE,HDATE)
    *B609426,1 MMT 11/28/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End]
        LOOP 
      ENDIF

      *tmi check for invoice to be factored
      NOTE the customer shold make payments only for FACTORED OR Non FACTORED invoices
      IF IIF(lcRpFact = 'F',EMPTY(INVHDR.CFACCODE),IIF(lcRpFact = 'N',!EMPTY(INVHDR.CFACCODE),.F.))
        LOOP
      ENDIF 
      *tmi check for invoice to be factored
      *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[Start]
      ELSE
        =gfSeek(ARHIST.TRAN,'RETHDR','RETHDR')
       *B610802,1 MMT 08/12/2014 report includes voided invoices and credit memos in paid amount[Start]
       IF RETHDR.Status ='V'
         LOOP
       ENDIF
       *B610802,1 MMT 08/12/2014 report includes voided invoices and credit memos in paid amount[End]

        IF !EMPTY(RETHDR.INVOICE)
          =SEEK(RETHDR.INVOICE,'INVHDR')
		      IF IIF(EMPTY(lcSesFltr),.F.,!(INVHDR.SEASON $ lcSesFltr)) OR !BETWEEN(TRANDATE,LDATE,HDATE)
		        LOOP 
		      ENDIF
		      IF IIF(lcRpFact = 'F',EMPTY(INVHDR.CFACCODE),IIF(lcRpFact = 'N',!EMPTY(INVHDR.CFACCODE),.F.))
		        LOOP
		      ENDIF 
		    ELSE
				  IF IIF(lcRpFact = 'F',EMPTY(RETHDR.CFACCODE),IIF(lcRpFact = 'N',!EMPTY(RETHDR.CFACCODE),.F.))
		        LOOP
		      ENDIF
		      IF !BETWEEN(TRANDATE,LDATE,HDATE)
		        LOOP 
		      ENDIF
  		  	IF !EMPTY(lcSesFltr)
			 		 llSeasonFound = .F.
		        =gfSeek(Rethdr.CRMEMO,'RETLINE')
		        SELECT RETLINE
		        SCAN REST WHILE CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD = Rethdr.CRMEMO                                                                                      
		          =gfSeek(RETLINE.STYLE,'STYLE')   
		          IF STYLE.SEASON $ lcSesFltr
		            llSeasonFound = .T.
		            EXIT
		          ENDIF
		        ENDSCAN 
		        IF !llSeasonFound   
		          LOOP 
		        ENDIF  		  	  
    		  ENDIF 		      
        ENDIF  
      ENDIF 
      *B609426,1 MMT 10/11/2010 Custom Seasonal Analysis report gives wrong Paid and returns[End]
      
      *C201132,1 HES [START] sum the m.paid variabel based on TRANTYPE = 1
*!*	      IF IIF(llRpVodInv,ARHIST.TRANTYPE = 'I',.F.) && To be adabted with the "Include Voided Invoice" Option.
*!*	        LOOP 
*!*	      ENDIF

      m.PAID = m.PAID  + ARHIST.AMOUNT

*!*	      SCATTER MEMVAR MEMO
*!*	      SELECT(lcArhstTmp)
*!*	      APPEND BLANK 
*!*	      GATHER MEMVAR MEMO 
      *C201132,1 HES [END  ] sum the m.paid variabel based on TRANTYPE = 1
    ENDSCAN 
    
    SELECT (lcTempFile)
    
    *B609221,1 HES 04/27/2010 seasonal analysis report shows wrong information when printed [Start]
    IF m.Booked+m.Shipped+m.ShipChrg+m.Discount+m.Paid+m.Returns <= 0 AND !llrpNAct
      LOOP
    ELSE     
    *B609221,1 HES 04/27/2010 seasonal analysis report shows wrong information when printed [End  ]
    
      INSERT INTO &lcTempFile FROM MEMVAR
      
    *B609221,1 HES 04/27/2010 seasonal analysis report shows wrong information when printed [Start]
    ENDIF
    *B609221,1 HES 04/27/2010 seasonal analysis report shows wrong information when printed [End  ]
    
      *C201132,1 HES [START] sum the m.paid variabel based on TRANTYPE = 1
*!*	    SELECT ARHIST
*!*	    SET ORDER TO ARHISTHT   && ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO
*!*	    SET RELATION TO ACCOUNT+HISTORY+TRANTYPE+TRAN INTO &lcArhstTmp 
*!*	    m.PAID = 0
*!*	    LOCATE

*!*		=SEEK(CUSTOMER.ACCOUNT,'ARHIST')
*!*	    SCAN REST WHILE ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO = CUSTOMER.ACCOUNT ;
*!*	                FOR SEEK(ARHIST.HISTORY,lcArhstTmp) AND TRANTYPE = '4' AND BETWEEN(TRANDATE,LDATE,HDATE) 
*!*	       
*!*	      m.PAID = m.PAID  + ARHIST.AMOUNT 
*!*	    ENDSCAN 
*!*	    
*!*	    SELECT (lcTempFile)
*!*	    INSERT INTO &lcTempFile FROM MEMVAR
      *C201132,1 HES [START] sum the m.paid variabel based on TRANTYPE = 1

  ENDSCAN
  
  SELECT (lcTempFile )
  IF !RECCOUNT()>0
    llDonprnt=.T.
    *-- Message : There are no records to display...!
    *--                < Ok > 
      =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF  
  SELECT (lcTempFile)
  gfDispRe('ARSESANL')
ELSE
  IF llDonprnt
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE  
    SELECT (lcTempFile)
    gfDispRe('ARSESANL')
  ENDIF  
ENDIF  &&FILTER CHANGE

*!*************************************************************
*! Name      : Calculate
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/98
*! Purpose   : Calculate THE OPEN BALANCE, PAYMENT AND ALLOWANCE
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfRltFld()
*!*************************************************************
*! Called from : ARACTSUM.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =CALCULATE()
*!*************************************************************

PROCEDURE CALCULATE
*------------------------------- D E B I T ----------------------------------*
SELECT DEBIT
=GFSEEK(XACCOUNT)
*B608508,1 WAM 04/08/2008 Consider empty currency as base currency
*SCAN REST WHILE ACCOUNT=XACCOUNT FOR  IIF(!EMPTY(lcCurrency),SEEK(DEBIT.CCURRCODE ,lcCurrency),DEBIT.CCURRCODE=ALLTRIM(gcBaseCurr)) 
SCAN REST WHILE ACCOUNT=XACCOUNT FOR  IIF(!EMPTY(lcCurrency),SEEK(DEBIT.CCURRCODE ,lcCurrency),EMPTY(DEBIT.CCURRCODE) OR DEBIT.CCURRCODE=ALLTRIM(gcBaseCurr)) 
*B608508,1 WAM 04/08/2008 (End)

  *-- OPEN BALANCE --*
  XDFOR = IIF( TRANTYPE='3','TRANDATE<=ENDING .AND. CHGBK_DATE<=ENDING',;
                             'TRANDATE<=ENDING')
  IF &XDFOR

    XOPENDB = XOPENDB + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

  ENDIF

  *---- ADJUSTMENT ---*
  IF (TRANTYPE = '2' AND BETWEEN(TRANDATE,LDATE,HDATE)) OR ;
    (TRANTYPE = '3' AND EMPTY(ChgBk_Date) AND BETWEEN(TRANDATE,LDATE,HDATE))

    lcType = SPACE(1)
    =gfRltFld(DEBIT.TRANCODE , @laType , 'TRANCODE')
    DO CASE
      CASE lcType = 'F'
        XFALL=XFALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

  
      CASE lcType = 'D'
        XDALL=XDALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

      OTHERWISE
        XMALL=XMALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

    ENDCASE        
  ENDIF
ENDSCAN

*-------------------------------- C R E D I T -------------------------------*
SELECT CREDIT
=GFSEEK( XACCOUNT)
*B608508,1 WAM 04/08/2008 Consider empty currency as base currency
*SCAN REST WHILE ACCOUNT=XACCOUNT FOR IIF(!EMPTY(lcCurrency),SEEK(CREDIT.CCURRCODE ,lcCurrency),CREDIT.CCURRCODE=ALLTRIM(gcBaseCurr)) 
SCAN REST WHILE ACCOUNT=XACCOUNT FOR IIF(!EMPTY(lcCurrency),SEEK(CREDIT.CCURRCODE ,lcCurrency),EMPTY(CREDIT.CCURRCODE) OR CREDIT.CCURRCODE=ALLTRIM(gcBaseCurr)) 
*B608508,1 WAM 04/08/2008 (End)

  *-- OPEN BALANCE --*
  XCFOR = IIF( TRANTYPE='6','TRANDATE<=ENDING .AND. CREDT_DATE<=ENDING',;
                            'TRANDATE<=ENDING')
  IF &XCFOR
    XOPENCR=XOPENCR + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

  ENDIF
  *---- PAYMENTS ----*
  IF (TRANTYPE='4' .AND. BETWEEN(TRANDATE,LDATE,HDATE))
    XPAY=XPAY + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

  ENDIF

  *---- ADJUSTMENT ---*
  IF (TRANTYPE = '5' AND BETWEEN(TRANDATE,LDATE,HDATE)) OR ;
    (TRANTYPE = '6' AND EMPTY(Credt_Date) AND BETWEEN(TRANDATE,LDATE,HDATE))  

  
    lcType = SPACE(1)
    =gfRltFld(CREDIT.cCreditCod , @laType , 'CCREDITCOD')
    DO CASE
      CASE lcType = 'F'
        XFALL=XFALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

        
      CASE lcType = 'D'
        XDALL=XDALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

      OTHERWISE
        XMALL=XMALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)


    ENDCASE        
  ENDIF
ENDSCAN

*-------------------------------- A R H I S T -------------------------------*

SELE ARHIST
=GFSEEK (XACCOUNT)

*B608508,1 WAM 04/08/2008 Consider empty currency as base currency
*SCAN REST  WHILE ACCOUNT=XACCOUNT FOR IIF(!EMPTY(lcCurrency),SEEK(ARHIST.CCURRCODE ,lcCurrency),ARHIST.CCURRCODE=ALLTRIM(gcBaseCurr)) 
SCAN REST  WHILE ACCOUNT=XACCOUNT FOR IIF(!EMPTY(lcCurrency),SEEK(ARHIST.CCURRCODE ,lcCurrency),EMPTY(ARHIST.CCURRCODE) OR ARHIST.CCURRCODE=ALLTRIM(gcBaseCurr)) 
*B608508,1 WAM 04/08/2008 (End)

  *-- PAYMENTS --*
  IF (TRANTYPE='4'.AND.BETWEEN(TRANDATE,LDATE,HDATE))
    XPAY=XPAY + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

  ENDIF

  *---- ADJUSTMENT ---*
  IF ((TRANTYPE='2'.OR.TRANTYPE='5'.OR.TRANTYPE='7').AND. ;
      BETWEEN(TRANDATE,LDATE,HDATE))

    lcType = SPACE(1)
    =gfRltFld(ARHIST.TRANCODE , @laType , IIF(TRANTYPE='2','TRANCODE','CCREDITCOD')  )
    DO CASE
      CASE lcType = 'F'
        XFALL=XFALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

      
      CASE lcType = 'D'
        XDALL=XDALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)  

        
      OTHERWISE
        XMALL=XMALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)  


    ENDCASE        
  ENDIF

  *-- OPEN BALANCE --*
  IF (HISTDATE > ENDING) .AND. (TRANDATE <= ENDING)

    *** THE PERIOD CLOSING DATE
    IF TRANTYPE = '3' .AND. CHGBK_DATE>ENDING
      LOOP
    ENDIF

    IF TRANTYPE = '6' .AND. CREDT_DATE>ENDING
      LOOP
    ENDIF

    IF INLIST(TRANTYPE,'1','2','3')
      XOPENDB = XOPENDB + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

    ENDIF
    IF INLIST(TRANTYPE,'0','4','5','6')
      XOPENCR = XOPENCR + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

    ENDIF
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/98
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfTempName()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

*-- If select by date disable the month item, else enable it.
* HES
*lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCMONTH'),1)
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCRPMONTH'),1)
* HES

laOGObjCnt[lnPos] = lcrpSelBy # 'D'

* HES
*= lfOGShowGet('LCMONTH')
= lfOGShowGet('LCRPMONTH')
* HES

*-- If select by date disable the year item, else enable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCRPYEAR'),1)
laOGObjCnt[lnPos] = lcrpSelBy # 'D'
= lfOGShowGet('LCRPYEAR')

*-- If select by date Enable the date item, else disable it.
lnDatInvPo = lfItmPos('INVHDR.INVDATE')
lnDisInvDt = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnDatInvPo)) + ",6]"),1)
laOGObjCnt[lnDisInvDt] = lcrpSelBy = 'D'
= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDatInvPo)) + ',6]')    

*B603591,1 BWA 07/19/2000 Initialize a variable to hold the cusrrency code.[START]
*lcCurrency = gcBaseCurr
*B603591,1 [END]

=gfOpenTABLE(oAriaApplication.DATADIR+'CUSTOMER',oAriaApplication.DATADIR+'CUSTOMER','SH')
=gfOpenTABLE(oAriaApplication.DATADIR+'DEBIT',oAriaApplication.DATADIR+'DEBIT','SH')
=gfOpenTABLE(oAriaApplication.DATADIR+'CREDIT',oAriaApplication.DATADIR+'CREDIT','SH')
=gfOpenTABLE(oAriaApplication.DATADIR+'ARHIST',oAriaApplication.DATADIR+'ARHISTHT','SH')
=gfOpenTABLE(oAriaApplication.DATADIR+'INVHDR',oAriaApplication.DATADIR+'INVHDRA','SH')
*HES
=gfOpenTABLE(oAriaApplication.DATADIR+'ORDHDR',oAriaApplication.DATADIR+'ORDACCT','SH')    && ACCOUNT+CORDTYPE+ORDER
=gfOpenTABLE(oAriaApplication.DATADIR+'APPAYMNT',oAriaApplication.DATADIR+'TYPCLNO','SH') && CPAYTYPE+CPAYCLNO
=gfOpenTABLE(oAriaApplication.DATADIR+'SALESREP',oAriaApplication.DATADIR+'SALESREP','SH')
=gfOpenTABLE(oAriaApplication.DATADIR+'RETHDR',oAriaApplication.DATADIR+'rethdra','SH')    && account+crmemo
=gfOpenTABLE(oAriaApplication.DATADIR+'REPCOMM',oAriaApplication.DATADIR+'REPCOMM','SH')   && REPCODE+DTOS(DATE)+TRAN+TRANTYPE
=gfOpenTABLE(oAriaApplication.DATADIR+'ARHIST',oAriaApplication.DATADIR+'ARHISTT','SH')
=gfOpenTABLE(oAriaApplication.DATADIR+'CODES',oAriaApplication.DATADIR+'CCODE_NO','SH')

IF EMPTY(laRpSource)
  DECLARE laRpSource[3],laRpTarget[3]  && Redeclare the source and target arrays for Order Status.
  STORE LANG_Soordhd_Open      TO laRpSource[1],laRpTarget[1]
  STORE LANG_Soordhd_Hold      TO laRpSource[2],laRpTarget[2]
  STORE LANG_Soordhd_Completed TO laRpSource[3],laRpTarget[3]
ENDIF

IF EMPTY(laRpSesSource)
  DECLARE laRpSesSource[1],laRpSesTarget[1]  && Redeclare the source and target arrays for Order Status.
  Cntr = 0

  SELECT CODES
  LOCATE 
  SCAN FOR CDEFCODE+CFLD_NAME = 'NSEASON    '
    Cntr = Cntr + 1
    DIMENSION laRpSesSource[Cntr],laRpSesTarget[Cntr]
    laRpSesTarget[Cntr]  = CCODE_NO + '-' + CDISCREP
    laRpSesSource[Cntr]  = CCODE_NO + '-' + CDISCREP
  ENDSCAN 
ENDIF

*HES
*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/1998
*! Purpose   : Validate The select by (date/month) setting in the 
*!             option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbDateOk()
*!*************************************************************
FUNCTION lfvSelBy

lnDatInvPo = lfItmPos('INVHDR.INVDATE')

IF lcrpSelBy = 'D'   && Select by date
  *-- Enable Date & disable month and year
  
  * HES
*!*	  STORE SPACE(0) TO lcmonth, lcYear  
*!*	  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCMONTH'),1)

  STORE SPACE(0) TO lcrpmonth, lcrpYear  
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCRPMONTH'),1)
  * HES
  
  laOGObjCnt[lnPos] = .F.
  
  * HES
*!*	  = lfOGShowGet('LCMONTH')
*!*	  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCYEAR'),1)
*!*	  laOGObjCnt[lnPos] = .F.
*!*	  = lfOGShowGet('LCYEAR')
  
  = lfOGShowGet('LCRPMONTH')
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCRPYEAR'),1)
  laOGObjCnt[lnPos] = .F.
  = lfOGShowGet('LCRPYEAR')

  * HES
  
  lnDisInvDt = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnDatInvPo)) + ",6]"),1)
  laOGObjCnt[lnDisInvDt] = .T.
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDatInvPo)) + ',6]')    
ELSE                 && lcRpSelBy = 'M' Select by month
  *-- Enable month and year & disable date
  STORE {} TO lDate, hDate
  
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCRPMONTH'),1)
  laOGObjCnt[lnPos] = .T.
  = lfOGShowGet('LCRPMONTH')
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCRPYEAR'),1)
  laOGObjCnt[lnPos] = .T.
  = lfOGShowGet('LCRPYEAR')
  lnDisInvDt = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnDatInvPo)) + ",6]"),1)
  laOGObjCnt[lnDisInvDt] = .F.
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDatInvPo)) + ',6]') 

ENDIF
*!***************************************************************************
*! Name      : lfItmPos
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
*! Purpose   : to get the position of the fixed filter in OG
*!***************************************************************************
*! Called from : OG When Function 
*!***************************************************************************
*! Example   : = lfItmPos()
*!***************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[3,2]

loOgScroll.lcOGLastForm ='ARACTSUM'
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkfile + ".DBF"

  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= 'ACTIVITY SUMMARY REPORT'

loOgScroll.laCRParams[2,1] = 'LCDEC'
loOgScroll.laCRParams[2,2]= IIF(LLRPDEC,'Y','N')

loOgScroll.laCRParams[3,1] = 'PERIOD'
loOgScroll.laCRParams[3,2]= XTITLE



*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[9,18] ,laTempCOM[1,18]
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTempCOM
lcExcStat = SET('EXACT')
SET EXACT ON
laTempStru[1,1] = 'ACCOUNT'
laTempStru[1,2] = 'C'
laTempStru[1,3] = 6
laTempStru[1,4] = 0

laTempStru[2,1] = 'ACC_NAME'
laTempStru[2,2] = 'C'
laTempStru[2,3] = 30
laTempStru[2,4] = 0

laTempStru[3,1] = 'OP_BAL'
laTempStru[3,2] = 'N'
laTempStru[3,3] = 13
laTempStru[3,4] = 2


laTempStru[4,1] = 'SALES'
laTempStru[4,2] = 'N'
laTempStru[4,3] = 13
laTempStru[4,4] = 2

laTempStru[5,1] = 'PAYM'
laTempStru[5,2] = 'N'
laTempStru[5,3] = 13
laTempStru[5,4] = 2

laTempStru[6,1] = 'RETURN'
laTempStru[6,2] = 'N'
laTempStru[6,3] = 13
laTempStru[6,4] = 2

laTempStru[7,1] = 'FREIGHT'
laTempStru[7,2] = 'N'
laTempStru[7,3] = 13
laTempStru[7,4] = 2

laTempStru[8,1] = 'DISC'
laTempStru[8,2] = 'N'
laTempStru[8,3] = 13
laTempStru[8,4] = 2

laTempStru[9,1] = 'MISC'
laTempStru[9,2] = 'N'
laTempStru[9,3] = 13
laTempStru[9,4] = 2

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


*************************************************************
*! Name      : lfCrtTmpFile
*! Developer : Hesham Elmasry (HES)
*! Date      : 40/12/2009
*! Purpose   : Create Temp file for the seasonal summary 
*!*************************************************************
FUNCTION lfCrtTmpFile

*-- check If File is created or not
*lcTempFile = gfTempFile()
*!*	IF USED(lcTempFile) AND RECCOUNT(lcTempFile) > 0
*!*	  USE IN (lcTempFile)
*!*	ENDIF
*-- Create File
IF !USED(lcTempFile)
  
  lnI = 1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Customer'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Cust_name'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 30
  laTempStru[lnI,4] = 0  
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'BOOKED'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 2
    
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'SHIPPED'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 2
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ShipChrg'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 2
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'DISCOUNT'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 2
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PAID'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 2
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'RETURNS'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 2
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'COMPAID'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 2
 
 =gfCrtTmp(lcTempFile,@laTempStru)
  SELECT (lcTempFile)
  INDEX ON Customer TAG Customer
ELSE
  SELECT (lcTempFile)
  ZAP
ENDIF
* End lfCrtTmpFile

*!**************************************************************************
*! Name      : lfSeTSRep
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 19/06/2000
*! Purpose   : Go top in the SALESREP IN RANGE
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSeTSRep()
*!**************************************************************************
FUNCTION lfSeTSRep
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
   SELECT SALESREP
   SET ORDER TO TAG  SALESREP
   GO TOP
  CASE OpGrdParm = 'R'
    SELECT SALESREP
    SET ORDER TO
ENDCASE

*!**************************************************************************
*! Name      : lfCalcAcntActv
*! Developer : Hesham Elmasry (HES)
*! Date      : 04/13/2009
*! Purpose   : Checks if this Account has activities or not
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSeTSRep()
*!**************************************************************************
FUNCTION lfCalcAcntActv
*          TMI 04/14/2009 [Start] 
PRIVATE XOPENBAL,XSALES,XPAY,XRET,XFALL,XDALL,XMALL,XBAL,XOPENDB,XOPENCR
*          TMI 04/14/2009 [End  ] 
STORE 0 TO XOPENBAL,XSALES,XPAY,XRET,XFALL,XDALL,XMALL,XBAL,XOPENDB,XOPENCR
XACCOUNT=ACCOUNT
*-- PROCEDURE TO CALCULATE OPENBALANCE AND CALCULATE PAYMENT AND ALLOWANCE
=CALCULATE()
*-----------O P E N  B A L .-----------
XOPENBAL = XOPENDB + XOPENCR

*-------------S A L E S----------------
SELE INVHDR
=GFSEEK(XACCOUNT)
IF llRpVodInv      && Variable if the user wanted to Include voided invoices or not.
  XSALES = 0
  SCAN REST WHILE ACCOUNT=XACCOUNT ;
            FOR  IIF(lcRpFact ='F' , !EMPTY(InvHdr.cFacCode) ,;
                 IIF(lcRpFact ='N' , EMPTY(InvHdr.cFacCode),.T.));
                .AND.   IIF(!EMPTY(lcCurrency),SEEK(INVHDR.CCURRCODE ,lcCurrency),EMPTY(INVHDR.CCURRCODE) OR INVHDR.CCURRCODE=ALLTRIM(gcBaseCurr)) 
       
    DO CASE
      CASE STATUS <> 'V' AND BETWEEN(InvDate,LDATE,HDATE)
      XSALES = XSALES + IIF(llMultCurr,gfAmntDisp(TotalChg, lcRpCurr , ldRpExDate , lcRpTmpNam),TotalChg)
       CASE STATUS = 'V'  AND BETWEEN(InvDate,LDATE,HDATE)  AND !BETWEEN(vDate,LDATE,HDATE)
        XSALES = XSALES + IIF(llMultCurr,gfAmntDisp(vTotalChg, lcRpCurr , ldRpExDate , lcRpTmpNam),vTotalChg)
       CASE STATUS = 'V'  AND !BETWEEN(InvDate,LDATE,HDATE) AND BETWEEN(vDate,LDATE,HDATE)
        XSALES = XSALES - IIF(llMultCurr,gfAmntDisp(vTotalChg, lcRpCurr , ldRpExDate , lcRpTmpNam),vTotalChg)
     ENDCASE
  ENDSCAN
ELSE
  SUM REST TOTALCHG TO XSALES WHILE (XACCOUNT=ACCOUNT) ;
  FOR BETWEEN(INVDATE,LDATE,HDATE) AND ;
     IIF(lcRpFact = 'F' , !EMPTY(InvHdr.cFacCode) ,;
         IIF(lcRpFact = 'N' , EMPTY(InvHdr.cFacCode),.T.));
        .AND. IIF(!EMPTY(lcCurrency),SEEK(INVHDR.CCURRCODE ,lcCurrency),EMPTY(INVHDR.CCURRCODE) OR INVHDR.CCURRCODE=ALLTRIM(gcBaseCurr)) 
ENDIF

*--------R E T U R N S----------------
        
IF 'RM' $ oAriaApplication.CompanyInstalledModules          
  SELE RETHDR
  =GFSEEK(XACCOUNT)
  SUM REST IIF(llMultCurr,gfAmntDisp(TOTCREDIT, lcRpCurr , ldRpExDate , lcRpTmpNam),TOTCREDIT);
             TO XRET WHILE XACCOUNT=ACCOUNT ;
             FOR   BETWEEN(CRDATE,LDATE,HDATE);
             .AND. IIF(!EMPTY(lcCurrency),SEEK(RETHDR.CCURRCODE ,lcCurrency),EMPTY(RETHDR.CCURRCODE) OR RETHDR.CCURRCODE=ALLTRIM(gcBaseCurr)) 

  XRET=(-1*XRET)
ENDIF
*--------B A L A N C E------------------
XBAL=(XOPENBAL+XSALES+XPAY+XRET+XFALL+XDALL+XMALL)
*----- T O T A L S -------
XTOTOPEN = XTOTOPEN  + XOPENBAL
XTOTSALES= XTOTSALES + XSALES
XTOTPAY  = XTOTPAY   + XPAY
XTOTRET  = XTOTRET   + XRET
XTOTFALL = XTOTFALL  + XFALL
XTOTDALL = XTOTDALL  + XDALL
XTOTMALL = XTOTMALL  + XMALL
XTOTBAL  = XTOTBAL   + XBAL
*-- Prevent printing customers with no activities.
m.account= CUSTOMER.ACCOUNT
m.acc_name= CUSTOMER.BTNAME
m.op_bal= XOPENBAL
m.sales= XSALES
m.paym= XPAY  
m.return=XRET
m.freight= XFALL
m.disc= XDALL 
m.misc= XMALL 
*B610642,1 TMI 12/29/2013 11:27 [Start] include the booked amount check
*llNoact = IIF(m.op_bal=0 And  m.sales=0 And m.paym=0 And m.return=0 And m.freight=0 And m.disc=0 And m.misc=0 ,.T.,.F.)
llNoact = IIF(m.BOOKED=0 AND m.op_bal=0 And  m.sales=0 And m.paym=0 And m.return=0 And m.freight=0 And m.disc=0 And m.misc=0 ,.T.,.F.)
*B610642,1 TMI 12/29/2013 11:27 [End  ] 

RETURN llNoact
*-* End lfCalcAcntActv

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/12/98
*! Purpose   : - Evaluate Status expression.
*!           : - Raise change status flag. 
*!*************************************************************
*! Calls     : 
*! Procedures : ....
*! Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
=gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.

IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Complete','C',''))) 
                                                        
  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpStatus = IIF(EMPTY(lcRpStatus),'OHC',ALLTRIM(lcRpStatus))

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llOGFltCh = .T.

ELSE  && else if length of current selected status equal previous length

  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llOGFltCh = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF

DO lpChkStat
*-- end of lfvOStatus.

*!*************************************************************
*! Name      : RefreshStatus
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************

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

*-- end of RefreshStatus.
*!*************************************************************
*! Name      : RefreshSeasons
*! Developer : Hesham Elmasry (HES)
*! Date      : 12/04/2003
*! Purpose   : Return the selected seasons in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshSeasons
  LOCAL lcSesStr, lnSesTarget
  
  lcSesStr = ""
  IF !EMPTY(laRpSesTarget)
    FOR lnSesTarget = 1 TO ALEN(laRpSesTarget,1)
      lcSesStr = lcSesStr + ", " + ALLTRIM(laRpSesTarget[lnSesTarget])
    ENDFOR 
    lcSesStr = SUBSTR(lcSesStr,2)
  ENDIF   
  RETURN lcSesStr
ENDFUNC

*-- end of RefreshSeasons.

*!*************************************************************
*! Name      : lfvOSeason
*! Developer : Hesham Elmasry (HES)
*! Date      : 05/11/2009
*! Purpose   : call mover function.
*!*************************************************************
*! Calls     : 
*! Called from : Report code
*!*************************************************************
FUNCTION lfvOSeason
PRIVATE lcOldSes,lcCurrChr

lcOldSes = lcSesStr
=gfMover(@laRpSesSource,@laRpSesTarget,'Select Order Status',.T.,'')  && call mover function.

IF !EMPTY(laRpSesTarget)
  FOR lnSesTarget = 1 TO ALEN(laRpSesTarget,1)
    lcSesStr = lcSesStr + ", " + ALLTRIM(laRpSesTarget[lnSesTarget])
  ENDFOR 
  lcSesStr = SUBSTR(lcSesStr,2)
ENDIF

IF lcOldSes <> lcSesStr 
  llOGFltCh = .T.
ENDIF 
*-- end of lfvOSeason.