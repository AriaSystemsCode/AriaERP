*:***************************************************************************
*: Program file  : SRREPOS
*: Program desc. : Custom Sales Representative commission work sheet Report For OSPREY
*: System        : Aria4XP
*: Module        : Sales Representative (SR )
*: Developer     : Mariam Mazhar (MMT)
*: TRACKING      : C200975    04/03/2008
*:***************************************************************************
*: Calls : 
*:    Functions  : lfwRepWhen,lfwOldVal,lfvRepCode,lfvDate
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:         : - We have several transaction types we seek repcomm for it, like
*:             TranTypes      Description
*:                     1      INVOICE
*:                     2      PAYMENTS
*:                     3      DEBIT ADJUSTMENT
*:                     4      CREDIT ADJUSTMENT
*:                     5      RETURN  (CREDIT MEMO)
*:                     6      VOID INVOICE
*:                     7      VOID CREDIT MEMO
*:***************************************************************************
*: Example : DO SRREPOS
*:***************************************************************************
*:Modifications:
*! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[T20071205.0015]
*:***************************************************************************
IF loogScroll.llOgFltCh
  llDonprnt=.F.

  IF !USED('Arhist')
    gfOpenTable('Arhist','ARHISTT')
  ENDIF 


  *Invoice Payment date
  lnPDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,"INVHDR.DATE"),1)
  LPDATE = SUBSTR(laOGFxFlt[lnPDatePos,6],1,ATC('|',laOGFxFlt[lnPDatePos,6])-1)
  HPDATE = SUBSTR(laOGFxFlt[lnPDatePos,6],  ATC('|',laOGFxFlt[lnPDatePos,6])+1)


  *RepComm Date
  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.DATE'),1)
  LDATE = SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1)
  HDATE = SUBSTR(laOGFxFlt[lnDatePos,6],  ATC('|',laOGFxFlt[lnDatePos,6])+1)

  * SalesRep Filter
  lcRepFltr= lfCheckFilter(1, 'SALESREP.REPCODE')
  llRepFltr   = !EMPTY(lcRepFltr) AND USED(lcRepFltr) AND RECCOUNT(lcRepFltr) > 0
  IF llRepFltr   
    SELECT (lcRepFltr)
    INDEX ON REPCODE TAG (lcRepFltr)
  ELSE
    IF TYPE("lcRepFltr") = "C" AND USED(lcRepFltr)
      USE IN (lcRepFltr)
    ENDIF
    lcRepFltr= ''
  ENDIF

  * CURRENCY Filter
  IF llMultCurr 
    lcCurFltr= lfCheckFilter(1, 'REPCOMM.CCURRCODE')
    llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND RECCOUNT(lcCurFltr) > 0
    IF llCurFltr   
      SELECT (lcCurFltr)
      INDEX ON CCURRCODE TAG (lcCurFltr)
    ELSE
      IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
        USE IN (lcCurFltr)
      ENDIF
      lcCurFltr= ''
    ENDIF
  ENDIF  

  XSTATUS   = IIF(lcRpStatus = 'B','OH',lcRpStatus)  
  lcCommFltr= 'AMOUNT<>0 .AND. STATUS $ ALLTRIM(XSTATUS)'
  IF !EMPTY(LDATE)
    lcCommFltr=lcCommFltr+" AND BETWEEN(DATE,CTOD('"+LDATE +"'),CTOD('"+HDATE+"'))"
  ELSE
    IF  !EMPTY(HDATE)
      lcCommFltr=lcCommFltr+" AND DATE<=CTOD('"+HDATE+"')"
    ENDIF
  ENDIF
  IF llMultCurr AND llCurFltr   
    lcCommFltr=lcCommFltr+" .AND. SEEK(CCURRCODE ,'"+lcCurFltr+"')"
  ENDIF 


  lcRepFltr = IIF(llRepFltr  ," SEEK( REPCODE,'"+lcRepFltr+"')", '.T.')

  IF ('RM'  $ oAriaApplication.CompanyInstalledModules)
    =gfOpenTable(oAriaApplication.DataDir+'RETHDR',oAriaApplication.DataDir+'RETHDR','SH')
    llRM =.T.
  ELSE
    llRM =.F.
  ENDIF
  lcWorkfile =loOgScroll.gfTempName()
  lcLogotmp  =loOgScroll.gfTempName()

  =lfBuildTmp()

  FROMDATE = LDATE
  THRUDATE = HDATE
  lcPERIOD   = 'Period: &FROMDATE - &THRUDATE'

  *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[Start]
  lcPayPeriod = ''
  IF !EMPTY(LPDATE) AND !EMPTY(HPDATE) 
    lcPayPeriod = 'Payment Period: &LPDATE - &HPDATE'
  ENDIF 
  *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[End]
  
  
  STORE 0 To lnNetShip , lnAmount

  SELECT SALESREP
  SCAN FOR &lcRepFltr 
    XSALESREP = REPCODE
    lcSaleName = Name
    WAIT WINDOW 'Sales commissiom for sales representative ' + XSALESREP NOWAIT
    SELECT REPCOMM
    SEEK XSALESREP
    SCAN WHILE REPCODE=XSALESREP FOR &lcCommFltr
      XTERMSPCT = 00.00
      lnDisc = 00.00
      IF  REPCOMM.TranType $ '16'
        SELECT INVHDR
        SEEK REPCOMM.Tran
        IF FOUND()
          lnDisc = DISCPCNT
          IF REPCOMM.TRANTYPE='1'  AND InvHdr.Consol = "Y" 
            SELECT ConsInvh
            SEEK(InvHdr.Invoice+REPCOMM.Store+REPCOMM.Order+IIF(!EMPTY(REPCOMM.piktkt),REPCOMM.piktkt,''))
            XTERMSPCT =ConsInvh.Trde_disc
            SELECT InvHdr
            XNETSHIP = ConsInvh.SHIPAMT + ConsInvh.DISCOUNT
          ENDIF
          IF REPCOMM.TRANTYPE='1'  AND InvHdr.Consol <> "Y" 
            XTERMSPCT =InvHdr.Trde_disc
            XNETSHIP = IIF(INVHDR.STATUS = 'V',INVHDR.VSHIPAMT+INVHDR.VDISCOUNT,INVHDR.SHIPAMT + INVHDR.DISCOUNT)
          ENDIF
          IF REPCOMM.TRANTYPE='6'  AND InvHdr.Consol = "Y" 
            SELECT ConsInvh
            SEEK(InvHdr.Invoice+REPCOMM.Store+REPCOMM.Order+IIF(!EMPTY(REPCOMM.piktkt),REPCOMM.piktkt,''))
            XTERMSPCT =consinvh.Trde_disc
            SELECT InvHdr
            XNETSHIP = (ConsInvh.SHIPAMT + ConsInvh.DISCOUNT)* -1
          ENDIF
          IF REPCOMM.TRANTYPE='6'  AND InvHdr.Consol <> "Y" 
            XTERMSPCT =InvHdr.Trde_disc
            XNETSHIP = (INVHDR.VSHIPAMT + INVHDR.VDISCOUNT)* -1
          ENDIF
          IF XTERMSPCT <>0
          
          *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[Start]
          IF lcRpInvTyp = 'P' AND gfSeek(REPCOMM.ACCOUNT + REPCOMM.Tran ,'Arhist')
            lcHistID = Arhist.History 
            SELECT Arhist
            =gfSetOrder('ARHISTHT')
            =gfSeek(REPCOMM.ACCOUNT + lcHistID )
            LOCATE REST WHILE ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO = REPCOMM.ACCOUNT + lcHistID FOR TRANTYPE = '4'
            IF FOUND()
              ldPayDate = Trandate
              lnDiscDays = 0
			  DIMENSION laPayTermAry[1,2]
  			  laPayTermAry[1,1] = "NTERDISCD "
			  laPayTermAry[1,2] = "lnDiscDays"
			  *-- Fill the related GL information from the codes file.
			  llNoThing = gfRltFld(Invhdr.CTERMCODE, @laPayTermAry, "CTERMCODE")
			  IF lnDiscDays > 0
			    ldInvDate = invhdr.invdate
			    lnDiff = ldPayDate - ldInvDate
			    IF lnDiff < lnDiscDays
  			      XNETSHIP = XNETSHIP - (XNETSHIP * XTERMSPCT/100)  
			    ENDIF 
			  ENDIF 
			ELSE
			  XNETSHIP = XNETSHIP - (XNETSHIP * XTERMSPCT/100) 
            ENDIF 
            SELECT Arhist
            =GFSETORDER('ARHISTT')
          ELSE
          *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[End]
          
            XNETSHIP = XNETSHIP - (XNETSHIP * XTERMSPCT/100) 
          
          *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[Start]
          ENDIF 
          *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[End]
          
          
          ENDIF
          
        ENDIF
        
        IF lcRpInvTyp = 'P'
          IF !gfSeek(REPCOMM.ACCOUNT + REPCOMM.Tran ,'Arhist')
            LOOP 
          ENDIF 
        ELSE
          IF lcRpInvTyp = 'N'  
            IF gfSeek(REPCOMM.ACCOUNT + REPCOMM.Tran ,'Arhist')
              LOOP 
            ENDIF 
          ENDIF 
        ENDIF
        
        
        IF !EMPTY(LPDATE) AND !EMPTY(HPDATE) 
          
          *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[Start]
          *IF gfSeek(REPCOMM.ACCOUNT + REPCOMM.Tran ,'Arhist') AND !BETWEEN(arhist.histdate,CTOD(LPDATE),CTOD(HPDATE))
          IF !gfSeek(REPCOMM.ACCOUNT + REPCOMM.Tran ,'Arhist') OR (gfSeek(REPCOMM.ACCOUNT + REPCOMM.Tran ,'Arhist') AND !BETWEEN(arhist.histdate,CTOD(LPDATE),CTOD(HPDATE)))
          *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[End]
          
            LOOP 
          ENDIF 
        ENDIF 
        
           
      ELSE
        IF llRM .AND. GFSEEK(REPCOMM.TRAN,'RETHDR')
          XNETSHIP = IIF(RETHDR.STATUS='V',RETHDR.VAMOUNT,RETHDR.AMOUNT)
          IF SEEK(REPCOMM.Account+REPCOMM.TRAN,'Credit')
             XNETSHIP = XNETSHIP + Credit.Dsc_Amt
          ENDIF
          XNETSHIP = XNETSHIP * IIF(REPCOMM.TRANTYPE='5',-1,1)
          
          *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[Start]
          IF !EMPTY(LPDATE) AND !EMPTY(HPDATE) 
            IF !gfSeek(REPCOMM.ACCOUNT + REPCOMM.Tran ,'Arhist') OR (gfSeek(REPCOMM.ACCOUNT + REPCOMM.Tran ,'Arhist') AND !BETWEEN(arhist.histdate,CTOD(LPDATE),CTOD(HPDATE)))
              LOOP 
            ENDIF 
          ENDIF 
          *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[End]
          
        ELSE
          IF REPCOMM.TranType = '5'
            XNETSHIP = REPCOMM.NORG_AMNT
          ELSE
            XNETSHIP = IIF(REPCOMM.COMMPCNT<>0,(REPCOMM.AMOUNT/(REPCOMM.COMMPCNT/100)),0)
          ENDIF
          lnAlias = ALIAS()
          SELECT REPCOMM
          FOR lnCount = 1 TO FCOUNT()
            IF FIELD(lnCount) = 'ORGNL_CADJ' AND ORGNL_CADJ<>0
              XNETSHIP = ORGNL_CADJ
              EXIT
            ENDIF  
          ENDFOR
          SELECT (lnAlias)
          
          *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[Start]
          IF !EMPTY(LPDATE) AND !EMPTY(HPDATE) 
            IF !gfSeek(REPCOMM.ACCOUNT + REPCOMM.Tran ,'Arhist') OR (gfSeek(REPCOMM.ACCOUNT + REPCOMM.Tran ,'Arhist') AND !BETWEEN(arhist.histdate,CTOD(LPDATE),CTOD(HPDATE)))
              LOOP 
            ENDIF 
          ENDIF 
          *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[End]
        ENDIF
      ENDIF
      SELECT REPCOMM
      lnNetShip = IIF(llMultCurr,lfBaseAmt(XNETSHIP),XNETSHIP)
      lnAmount  = IIF(llMultCurr,lfBaseAmt(nforamnt),AMOUNT)
      
      *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[Start]
      lnAmount  = lnNetShip  * REPCOMM.commpcnt/100 
      *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[End]
      
      SCATTER MEMVAR
      M.CUSTOMER= IIF(SEEK('M'+REPCOMM.ACCOUNT,'CUSTOMER'), SUBSTR(CUSTOMER.BTNAME,1,15), '')
      SELECT REPCOMM
      M.REPNAME=lcSaleName 
      M.TERMSPCT= XTERMSPCT  
      M.lnDisc= lnDisc  
      M.lnNetShip  =lnNetShip  
      M.lnAmount  =lnAmount  
      M.cCurrCode= IIF(llMultCurr .AND. lcRpCurr = "F",ALLTRIM(cCurrCode),ALLTRIM(gcBaseCurr))
      M.ST='LOGO'
      M.CDATE=LEFT(DTOS(DATE),10)
      INSERT INTO  (lcWorkfile ) FROM MEMVAR
    ENDSCAN
  ENDSCAN

  SELECT (lcWorkfile )
  IF !RECCOUNT()>0
    llDonprnt=.T.
    *-- Message : There are no records to display...!
    *--                < Ok > 
      =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF  

  DO CASE
    CASE lcRpSortBy='I'
      SORTFIELD = 'REPCODE+CCURRCODE+TRAN'
    CASE lcRpSortBy='O'
      SORTFIELD = 'REPCODE+CCURRCODE+ORDER+TRAN'
    CASE lcRpSortBy='C'
      SORTFIELD = 'REPCODE+CCURRCODE+CUSTPO+TRAN'
    CASE lcRpSortBy='D'   
      SORTFIELD = 'REPCODE+CCURRCODE+DTOS(DATE)+TRAN'
      
    *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[Start]
    CASE lcRpSortBy='A'   
      SORTFIELD = 'REPCODE+CCURRCODE+Account+TRAN'
    *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[End]
    
  ENDCASE
  

  INDEX ON &SORTFIELD TAG (lcWorkfile )

  =lfAdjustCRSettings()
  IF USED(lcWorkfile )
    USE IN (lcWorkfile )
  ENDIF
  IF USED(lcLogotmp)
      USE IN (lcLogotmp)
  ENDIF
  
  IF looGScroll.llShowLogo 
    IF TYPE("looGScroll.lcLogoPath") = 'C' .AND. !EMPTY(looGScroll.lcLogoPath) .AND. !ISNULL(looGScroll.lcLogoPath) .AND.  (UPPER(RIGHT(looGScroll.lcLogoPath,3)) == 'BMP')
      IF looGScroll.FileExist(looGScroll.lcLogoPath)
        LOCAL loOleObj
        lcReportName = looGScroll.lcOGLastForm
        lcReportFileName = oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
                         '\' + lcReportName + '.RPT'
         
        IF FILE(lcReportFileName)                 
  	    loMainCr = CREATEOBJECT('CrystalRuntime.Application') 
      	loMain = CREATEOBJECT('CrystalRuntime.Report') 
  	    loMain = loMainCr.OpenReport(lcReportFileName)
  	    FOR lnI = 1 TO loMain.Sections.Item[2].ReportObjects.Count
    	      IF UPPER(loMain.Sections.Item[2].ReportObjects[lnI].Name) = 'PICTURE1'
  			loMain.Sections.Item[2].DeleteObject (loMain.Sections.Item[2].ReportObjects[lnI])
  			EXIT 
  	      ENDIF
  	    ENDFOR  
  	    
          loOleObj = loMain.Sections.Item[2].AddPictureObject(looGScroll.lcLogoPath, 20, 20)
          loOleObj.Width  = 1500
  	    loOleObj.Height = 1500
          loMain.Sections.Item[2].Height = loOleObj.Height + 500 

  	    lcTempName = "_"+looGScroll.lcOGLastForm
  	     
  		loMain.Save(oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
                         '\' + lcTempName + '.RPT')
                         
          COPY FILE (oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
                         '\' + lcTempName + '.RPT') TO  (lcReportFileName)              
                         
          ERASE (oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID + ;
                         '\' + lcTempName + '.RPT')              
                            
   	  ENDIF   
      ENDIF
    ENDIF    
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
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
IF llMultCurr
  lnCurrPos  = lfItmPos('REPCOMM.CCURRCODE')
ELSE
  lcRpCurr = "O"
ENDIF  

*-- End of lfwRepWhen.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/1998
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/31/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End Of lfItmPos.
*!*************************************************************
*! Name      : lfBaseAmt
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 50/31/2000
*! Purpose   : Compute base amount
*!*************************************************************
*! Example   : = lfBaseAmt()
*!*************************************************************
FUNCTION lfBaseAmt
PARAMETERS lnAmntCurr
PRIVATE lnBaseAmt

lnBaseAmt = lnAmntCurr
*-- if Multi currency and user want to print in base currency and 
*-- currency not the base currency.
IF llMultCurr .AND. lcRpCurr <> "F" AND lnBaseAmt <> 0
  lnBaseAmt = gfAmntDisp(lnBaseAmt,lcRpCurr,ldRpExDate,lcRpTmpNam)
ENDIF
RETURN lnBaseAmt

*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[2]

*! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[Start]
*DIMENSION loOgScroll.laCRParams[4,2]
DIMENSION loOgScroll.laCRParams[5,2]
*! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[End]

loOgScroll.lcOGLastForm ='SRREPOS'
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkfile + ".DBF"
loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcLogotmp+ ".DBF"

  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= 'Salesrep Commission Worksheet'

loOgScroll.laCRParams[2,1] = 'SortBy'

DO CASE
  CASE lcRpSortBy = 'C'
    loOgScroll.laCRParams[2,2] = 'Cust PO'
  CASE lcRpSortBy = 'O'
    loOgScroll.laCRParams[2,2] = 'Order'
  CASE lcRpSortBy = 'I'
    loOgScroll.laCRParams[2,2] = 'Invoice'
  CASE lcRpSortBy = 'D'
    loOgScroll.laCRParams[2,2] = 'Date'
  
  *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[Start]
  CASE lcRpSortBy = 'A'
    loOgScroll.laCRParams[2,2] = 'Account'
  *! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[End]
  
ENDCASE  

loOgScroll.laCRParams[3,1] = 'Period'
loOgScroll.laCRParams[3,2] = lcPERIOD

loOgScroll.laCRParams[4,1] = 'PrtDecimal'
loOgScroll.laCRParams[4,2] = lnRpDeclNo 

*! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[Start]
loOgScroll.laCRParams[5,1] = 'PayPeriod'
loOgScroll.laCRParams[5,2] = lcPayPeriod 
*! C200975,2 MMT 02/03/2008 Fix bug of wrong data printed in payment date[End]

*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[20,18] ,laTempCOM[1,18]
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTempCOM
lcExcStat = SET('EXACT')
SET EXACT ON
SELECT REPCOMM
=OGAFIELDS(@laTempCOM)
laTempStru[1,1]  = 'REPCODE'
laTempStru[2,1]  = 'ACCOUNT'
laTempStru[3,1]  = 'CUSTPO'
laTempStru[4,1]  = 'DATE'
laTempStru[5,1]  = 'STATUS '
laTempStru[6,1]  = 'ORDER'
laTempStru[7,1]  = 'AMOUNT'
laTempStru[8,1]  = 'COMMPCNT'
laTempStru[9,1]  = 'DESC'
laTempStru[10,1] = 'STORE'
laTempStru[11,1] = 'TRAN'
laTempStru[12,1] = 'CCURRCODE'
*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 1 TO 12
  lnFldRow = ASCAN(laTempCOM,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempCOM,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempCOM[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempCOM[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempCOM[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

laTempStru[13,1] = 'TERMSPCT'
laTempStru[13,2] = 'N'
laTempStru[13,3] = 10
laTempStru[13,4] = 2

laTempStru[14,1] = 'LNDISC'
laTempStru[14,2] = 'N'
laTempStru[14,3] = 10
laTempStru[14,4] = 2

laTempStru[15,1] = 'LNNETSHIP'
laTempStru[15,2] = 'N'
laTempStru[15,3] = 10
laTempStru[15,4] = 2


laTempStru[16,1] = 'LNAMOUNT'
laTempStru[16,2] = 'N'
laTempStru[16,3] = 10
laTempStru[16,4] = 2

laTempStru[17,1] = 'CUSTOMER'
laTempStru[17,2] = 'C'
laTempStru[17,3] = 30
laTempStru[17,4] = 0

laTempStru[18,1] = 'REPNAME'
laTempStru[18,2] = 'C'
laTempStru[18,3] = 30
laTempStru[18,4] = 0

laTempStru[19,1] = 'ST'
laTempStru[19,2] = 'C'
laTempStru[19,3] = 8
laTempStru[19,4] = 0

laTempStru[20,1] = 'CDATE'
laTempStru[20,2] = 'C'
laTempStru[20,3] = 10
laTempStru[20,4] = 0
=gfCrtTmp(lcWorkfile ,@laTempstru,,"",.f.)
SET EXACT &lcExcStat  

lcPath=oAriaApplication.WorkDir +  lcLogotmp+ ".DBF"

CREATE TABLE (lcPath) (ST C(8),mygenfield G)
SELECT (lcLogotmp)
APPEND BLANK  
REPLACE ST  WITH 'LOGO'

IF TYPE("loogscroll.lcLogoPath") = 'C' .AND. !EMPTY(loogscroll.lcLogoPath) .AND. !ISNULL(loogscroll.lcLogoPath) .AND.  (UPPER(RIGHT(loogscroll.lcLogoPath,3)) == 'BMP')
  IF loogscroll.FileExist(loogscroll.lcLogoPath) .AND. loogscroll.llShowLogo 
      APPEND GENERAL mygenfield FROM (loogscroll.lcLogoPath)
  ENDIF
ENDIF


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
*C200975,2 MMT 04/15/2008 let Paid Date Option Disappear when Not Paid Inv. Selected{Start}
*************************************************************
*! Name      : lfPayInv
*! Developer : MAriam Mazhar (MMT)
*! Date      : 04/15/2008
*! Purpose   : Validate Paid Invoice Filter
*!*************************************************************
FUNCTION lfPayInv
ClearRead()
*C200975,2 MMT 04/15/2008 let Paid Date Option Disappear when Not Paid Inv. Selected{End}