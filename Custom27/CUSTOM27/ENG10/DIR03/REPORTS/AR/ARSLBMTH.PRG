*:***************************************************************************
*: Program file  : ARSLBMTH.PRG
*: Program desc. : Sales By Month              (C#130193)
*: For Report    : ARSLBMTH.FRX 
*: System        : Aria Advantage Series.
*: Module        : Account Rec. (AR)
*: Developer     : Hend Ghanem (HBG)
*: Date          : 07/12/2005
*:***************************************************************************

FOR lnI = 1 TO 12
  lcI = ALLTRIM(STR(lnI))
  STORE 0 TO lnGQty&lcI, lnGVal&lcI, lnQty&lcI , lnVal&lcI
ENDFOR  
STORE 0 TO lnTotQty , lnTotVal , lnGTotQty , lnGTotVal , lnTotAmnt , lnGTotAmnt 
llEndRep = .F.
lcStyExpr = ""
llRpSrtBSt = (ATC('INVLINE.STORE',lcRpExp) <> 0)

Store "" TO lcNonMajPi , lcOldVal
lcStyMjr = gfitemmask('PM')
lnStyLngth = LEN(lcStyMjr)        && Var. hold the length of the style.


IF llOGFltCh
  IF !USED(lcTempFile) OR (RECCOUNT(lcTempFile) > 0)
    =lfCreatTmp()
  ENDIF  
  =lfCollect()
ENDIF  

*-- Asking if no records (Display message) otherwise print report
IF RECCOUNT(lcTempFile) = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name

SELECT (lcTempFile)
SET ORDER TO DISPLAY 
IF llRpPrtExl
  =lfExprt2XL()
ELSE
  *--mhm2006
  SELECT (lcTempFile)
  SCAN
    =lfAcumVar()
  ENDSCAN  
  *--mhm2006
  DO gfDispRe WITH EVAL('lcFormName')
ENDIF  

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/12/2005
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

IF !USED(lcTempStor)
  CREATE TABLE (gcWorkDir+lcTempStor) (Store C(8), BTNAME  C(30),PHONE1 C(16),CADDRESS6 C(20), NETBAL N(14,2),Account C(5))
  INDEX ON Store+Account TAG (lcTempStor)
ELSE
  SELECT (lcTempStor)
  ZAP
ENDIF
llStorStat = .F.
lnAccPos = ASCAN(laOGFxFlt,'INVLINE.ACCOUNT')
IF lnAccPos > 0
  lnAccPos = ASUBSCRIPT(laOGFxFlt,lnAccPos ,1)
  SELECT CUSTOMER
  SET FILTER TO type+account+store = 'S'
  SCAN FOR SEEK(Account,LAOGFXFLT[lnAccPos,6])
    llStorStat = .T.
  ENDSCAN
ENDIF
SELECT CUSTOMER
SET FILTER TO
= lfStorEnab()         && call function which Enable/Disable Store      
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/12/2005
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
CREATE TABLE (gcWorkDir+lcTempFile) (Account C(5), AccName C(30),Store C(8),;
									 StoreNam C(15),STYLE C(lnStyLngth),Desc C(30),Price N(6,2),;
	                                 Qty1 N(8,2) ,Qty2 N(8,2) ,Qty3 N(8,2),;
    	                             Qty4 N(8,2) ,Qty5 N(8,2) ,Qty6 N(8,2),; 
        	                         Qty7 N(8,2) ,Qty8 N(8,2) ,Qty9 N(8,2),;
            	                     Qty10 N(8,2),Qty11 N(8,2),Qty12 N(8,2),;
  		         	                 TotQty N(9,2),Type C(1),cRepeted C(1),llEnd L(1))

INDEX ON Account+Store+Style+cRepeted+STR(PRICE,6)+type TAG (lcTempFile) ADDITIVE
INDEX ON Account+Store+Style+cRepeted TAG DISPLAY ADDITIVE
SET ORDER TO (lcTempFile)

CREATE TABLE (gcWorkDir+lcTempRetL) (crmemo C(6), cret_linno C(4),STYLE C(lnStyLngth))
INDEX ON crmemo +STYLE +cret_linno TAG (lcTempRetL)
SET ORDER TO (lcTempRetL)

*!*************************************************************
*! Name      : lfCollect
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/12/2005
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
FUNCTION lfCollect

=gfOpenFile(gcDataDir+'RETHDR','RETHDRa','SH')
SET ORDER TO CUSTOMER IN CUSTOMER
*--mhm2006
*SET ORDER TO Retlines IN RETLINE
SET ORDER TO Retline IN RETLINE
*--mhm2006
SET ORDER TO Invhdra IN INVHDR
SET ORDER TO CSTYLE IN STYLE
USE gcDataDir+'Customer' IN 0 AGAIN ALIAS CUST ORDER Customer

SELECT (lcTempRetL)
ZAP

=lfUpdPerid()

SELECT CUSTOMER
lnAccPos = ASCAN(laOGFxFlt,'INVLINE.ACCOUNT')
IF lnAccPos > 0
  lnAccPos = ASUBSCRIPT(laOGFxFlt,lnAccPos ,1)
ENDIF
IF USED(LAOGFXFLT[lnAccPos ,6])
  GO TOP IN LAOGFXFLT[lnAccPos ,6]
  llRpAcc = !EOF(LAOGFXFLT[lnAccPos,6])
ELSE
  llRpAcc = .F.
ENDIF

lcTmAccStr = gfTempName()
CREATE TABLE (gcWorkDir+lcTmAccStr) (Store C(8), BTNAME  C(30),PHONE1 C(16),CADDRESS6 C(20), NETBAL N(14,2),Account C(5))
INDEX ON Account+Store TAG (lcTmAccStr)

IF llRpAcc
  lcExpr = "SEEK(Account,LAOGFXFLT["+ALLTRIM(STR(lnAccPos))+",6])"
  SELECT CUSTOMER
  SET FILTER TO type+account+store = 'S'
  SCAN FOR SEEK(Account,LAOGFXFLT[lnAccPos,6])
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTmAccStr) FROM MEMVAR
  ENDSCAN
  SELECT CUSTOMER
  SET FILTER TO
ELSE
  lcExpr = '.T.'
ENDIF

SELECT CUSTOMER
SCAN FOR Type+Account+Store = 'M'
  IF !EVAL(lcExpr)
    LOOP
  ENDIF
  lcAccname = Customer.btName
  IF SEEK(Account,'INVHDR')
    SELECT INVHDR
    SCAN REST WHILE account+invoice = Customer.Account FOR STATUS <> 'V'
      WAIT WINDOW 'Collect Data for invoice : ' + Invoice NOWAIT
      IF INVHDR.Consol = 'Y'
        lcScnExpr = "invoice+store+order+style+STR(lineno,6) = INVHDR.Invoice"
	    lcInvLine = 'CONSINVL'
	    lcStrExpr = '.T.'
    	lnStrPos  = ATC('INLIST(INVLINE.STORE',lcRpExp)	 
    	IF lnStrPos = 0
    	  lnStrPos  = ATC('BETWEEN(INVLINE.STORE',lcRpExp)	 
    	ENDIF
    	
        lnStart   = IIF(ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)',lcRpExp) <> 0,ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)',lcRpExp),LEN(lcRpExp))
    	IF lnStrPos <> 0
	      lcLeftExpr = SUBSTR(lcRpExp,lnStrPos,lnStart-lnStrPos+IIF(lnStart=LEN(lcRpExp),1,0))
	    ELSE  
    	  lcLeftExpr = '.T.'
	    ENDIF  	    
    	lcCrStrEpr =  STRTRAN(lcLeftExpr,'INVLINE','RETHDR')
    	
    	IF USED(laOGFxFlt[3,6])
    	  lcRightExpr = 'SEEK(SUBSTR(INVLINE.Style,1,lnStyLngth),laOGFxFlt[3,6])'
        ELSE
          lcRightExpr = '.T.'
        ENDIF	    
	    lcRepExpr = lcLeftExpr + ' AND ' + lcRightExpr 
	    lcRepExpr = STRTRAN(lcRepExpr,'INVLINE','CONSINVL')
	  ELSE
    	lcScnExpr = "invoice+STR(lineno,6) = INVHDR.Invoice"
	    lcInvLine = 'INVLINE'
        lnStrPos  = ATC('INLIST(INVLINE.STORE',lcRpExp)	 
    	IF lnStrPos = 0
    	  lnStrPos  = ATC('BETWEEN(INVLINE.STORE',lcRpExp)	 
    	ENDIF
	    lnStart   = IIF(ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)',lcRpExp) <> 0,ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)',lcRpExp),LEN(lcRpExp))
    	IF lnStrPos <> 0
	      lcStrExpr  = SUBSTR(lcRpExp,lnStrPos,lnStart-lnStrPos+IIF(lnStart=LEN(lcRpExp),1,0))
    	  lcStrExpr  = STRTRAN(lcStrExpr,'INVLINE','INVHDR')
	    ELSE  
    	  lcStrExpr = '.T.'
	    ENDIF  
	    IF USED(laOGFxFlt[3,6]) AND RECCOUNT(laOGFxFlt[3,6]) >0
    	  lcRepExpr = 'SEEK(SUBSTR(INVLINE.Style,1,lnStyLngth),laOGFxFlt[3,6])'
        ELSE
          lcRepExpr = '.T.'
        ENDIF	
  	    lcCrStrEpr =  STRTRAN(lcStrExpr,'INVHDR','RETHDR')
	  ENDIF  

      IF SEEK(Customer.account,lcTmAccStr) AND !(&lcStrExpr)
        LOOP
      ENDIF
      IF SEEK(Invoice,lcInvLine)
        SELECT (lcInvLine)  
        SCAN REST WHILE &lcScnExpr FOR &lcRepExpr
          lcPriod = STR(MONTH(InvDate),2)+'/'+STR(YEAR(InvDate),4)
          lnPriod = ASCAN(laPeriod,lcPriod)
          IF lnPriod > 0
            lnPriod = ASUBSCRIPT(laPeriod,lnPriod,1)
            lcPrd = ALLTRIM(STR(lnPriod))
            =SEEK('S'+Account+Store,'CUST')
            lcStyle = SUBSTR(&lcInvLine..Style,1,lnStyLngth)  
            =SEEK(lcStyle,'STYLE')
            lcDesc  = STYLE.DESC
            IF SEEK(&lcInvLine..ACCOUNT+IIF(llRpSrtBSt,&lcInvLine..Store,SPACE(8))+lcStyle+'A'+STR(Price,6),lcTempFile) OR;
               SEEK(&lcInvLine..ACCOUNT+IIF(llRpSrtBSt,&lcInvLine..Store,SPACE(8))+lcStyle+'Z'+STR(Price,6),lcTempFile)
              SELECT (lcTempFile)
              Replace Qty&lcPrd   WITH Qty&lcPrd + &lcInvLine..TotQty ,;
                      TotQty      WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8+Qty9+Qty10+Qty11+Qty12
              IF lcRpType = 'B' AND SEEK(ACCOUNT+IIF(llRpSrtBSt,Store,SPACE(8))+lcStyle+'Z'+STR(Price,6)+'V',lcTempFile)
                Replace Qty&lcPrd   WITH Qty&lcPrd + (&lcInvLine..TotQty*&lcInvLine..Price) ,;
                        TotQty      WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8+Qty9+Qty10+Qty11+Qty12
              ENDIF        
            ELSE
              lcRepeted = IIF(!SEEK(&lcInvLine..ACCOUNT+IIF(llRpSrtBSt,&lcInvLine..Store,SPACE(8))+lcStyle,lcTempFile),'A','Z')
              *hbghbg
              lcStorenam = CUST.Store
              *hbghbg
              INSERT INTO (lcTempFile) (Account,AccName,Store,StoreNam,Style,DESC,Price,Qty&lcPrd,TotQty,Type,cRepeted,llEnd ) ;
    	    				    Values (&lcInvLine..Account,lcAccname,IIF(llRpSrtBSt,&lcInvLine..Store,SPACE(8)),lcStoreNam,lcStyle,lcDesc,&lcInvLine..Price,;
      		    						&lcInvLine..TotQty,&lcInvLine..TotQty,'Q',lcRepeted,.F.)         
              IF lcRpType = 'B' 
                INSERT INTO (lcTempFile) (Account,AccName,Store,StoreNam,Style,DESC,Price,Qty&lcPrd,TotQty,Type,cRepeted,llEnd ) ;
      	    				    Values (&lcInvLine..Account,lcAccname,IIF(llRpSrtBSt,&lcInvLine..Store,SPACE(8)),lcStoreNam,lcStyle,lcDesc,&lcInvLine..Price,;
      		    						&lcInvLine..TotQty*&lcInvLine..Price,&lcInvLine..TotQty*&lcInvLine..Price,'V','Z',.F.)         
              ENDIF     
            ENDIF
            SELECT (lcInvLine)
          ENDIF  
          *--mhm2006
          *=lfRetCalc(lcRpAcc)
          IF .F.
          *--mhm2006
          
          IF SEEK(&lcInvLine..Account,'RETHDR')
	        SELECT RETHDR
            SCAN REST WHILE account+crmemo = &lcInvLine..Account FOR STATUS <> 'V'
              IF !(&lcCrStrEpr)
                LOOP
              ENDIF
              IF SEEK(&lcInvLine..Style+RETHDR.crmemo,'RETLINE')
                lcPriod = STR(MONTH(RETLINE.crdate),2)+'/'+STR(YEAR(RETLINE.crdate),4)
                lnPriod = ASCAN(laPeriod,lcPriod)
                IF lnPriod > 0
                  lnPriod = ASUBSCRIPT(laPeriod,lnPriod,1)
                  lcPrd = ALLTRIM(STR(lnPriod))
                  =SEEK('S'+Account+RETHDR.Store,'CUST')
                  lcStyle = SUBSTR(&lcInvLine..Style,1,lnStyLngth)  
                  =SEEK(lcStyle,'STYLE')
                  lcDesc  = STYLE.DESC
                  IF !SEEK(RETHDR.crmemo+lcStyle+RETLINE.cret_linno,lcTempRetL)
     	            IF SEEK(&lcInvLine..ACCOUNT+IIF(llRpSrtBSt,RETHDR.Store,SPACE(8))+lcStyle+'A'+STR(RETLINE.Price,6),lcTempFile) OR ;
       		          SEEK(&lcInvLine..ACCOUNT+IIF(llRpSrtBSt,RETHDR.Store,SPACE(8))+lcStyle+'Z'+STR(RETLINE.Price,6),lcTempFile)
    	              SELECT (lcTempFile) 
  	                  Replace Qty&lcPrd   WITH Qty&lcPrd + (-RETLINE.TotQty),;
                        TotQty      WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8+Qty9+Qty10+Qty11+Qty12
		  		      IF lcRpType = 'B' AND SEEK(ACCOUNT+IIF(llRpSrtBSt,Store,SPACE(8))+lcStyle+'Z'+STR(Price,6)+'V',lcTempFile)
	                    Replace Qty&lcPrd   WITH Qty&lcPrd + (-RETLINE.TotQty*&lcInvLine..Price) ,;
                  	         TotQty      WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8+Qty9+Qty10+Qty11+Qty12
                      ENDIF                                  
            	    ELSE
            	      *hbghbg
            	      lcStorenam = CUST.Store
            	      *hbghbg
        	          lcRepeted = IIF(!SEEK(&lcInvLine..ACCOUNT+IIF(llRpSrtBSt,RETHDR.Store,SPACE(8))+lcStyle,lcTempFile),'A','Z')
	    	          INSERT INTO (lcTempFile) (Account,AccName,Store,StoreNam,Style,DESC,Price,Qty&lcPrd,TotQty,Type,cRepeted,llEnd ) ;
   	       	   			   	  Values (&lcInvLine..Account,lcAccname,IIF(llRpSrtBSt,RETHDR.Store,SPACE(8)),lcStoreNam,lcStyle,lcDesc,RETLINE.Price,;
    				           		   		  -RETLINE.TotQty,-RETLINE.TotQty,'R',lcRepeted,.F.)     
          	          IF lcRpType = 'B' 
            	        INSERT INTO (lcTempFile) (Account,AccName,Store,StoreNam,Style,DESC,Price,Qty&lcPrd,TotQty,Type,cRepeted,llEnd ) ;
            	   			 	  Values (&lcInvLine..Account,lcAccname,IIF(llRpSrtBSt,RETHDR.Store,SPACE(8)),lcStoreNam,lcStyle,lcDesc,RETLINE.Price,;
    			         		   		  -RETLINE.TotQty*RETLINE.Price,-RETLINE.TotQty*RETLINE.Price,'V','Z',.F.)     
                	  ENDIF    			           		   		   
                    ENDIF     
                    INSERT INTO (lcTempRetL) (crmemo , cret_linno ,STYLE ) ;
                                        VALUES (RETHDR.crmemo , RETLINE.cret_linno , lcStyle)
                  ENDIF  
                ENDIF  
              ENDIF
            ENDSCAN
          ENDIF          
          *--mhm
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN
  ENDIF
  *--mhm2006
  =lfRetCalc(CUSTOMER.Account)
  *--mhm2006
  
ENDSCAN
SELECT (lcTempFile)

GO BOTT
REPLACE llEnd WITH .T.
IF USED('CUST')
  USE IN CUST
ENDIF  

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfSrSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Style In Range Filter. 2
*!*************************************************************
FUNCTION lfSrSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE


*!**************************************************************************
*! Name      : lfwOldVal
*! Developer : Sameh (SSE)
*! Date      : 03/22/1999
*! Purpose   : Save any Get Object old value
*!**************************************************************************
*! Example   : =lfwOldVal()
*!**************************************************************************
*!
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))
*-- End of lfwOldVal.

*!**************************************************************************
*! Name      : lfvAccount
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/16/1999
*! Purpose   : Validation function for the Customer Account field
*!**************************************************************************
*! Called from : Customer Account field [Option Grid]
*!**************************************************************************
*! Calls       : CusBrowM()
*!**************************************************************************
*! Example     : =lfvAccount()
*!**************************************************************************
FUNCTION lfvAccount
PRIVATE lcObjVal
lcObjVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field
lcCurObj = SYS(18)
*-- Avoid recurresion clear read.
IF !(lcObjVal == lcOldVal)
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER
  
  lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field
  *-- IF The user want to Browse or if the Account he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
    llObjRet = CusBrowM(@lcObjVal , '' , 'M')
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
    &lcObjName = lcObjVal
  ENDIF    && End of IF

  *-- If the user changed the stored account (old value)   
  IF !(lcObjVal == lcOldVal)
    IF EMPTY(lcObjVal)
      llStorStat = .F.     && this flag will Disable the Store   
      lcRpAcc = ""   
    ELSE 
      lcRpAcc = lcObjVal
      llStorStat = SEEK('S'+lcObjVal,'CUSTOMER')
    ENDIF
  ENDIF  
  SET ORDER TO &lcCustOrd
  SELECT(lnAlsNo)
  CLEAR READ   && Clear current read cycle, to activate new one.
  CUROBJ = OBJNUM(lcCurObj)
ENDIF  
*-- End of lfvAcct.

*!**************************************************************************
*! Name      : lfStorEnab
*! Developer : Sameh Saiid (SSE)
*! Date      : 10/14/1999
*! Purpose   : To Disable or Enable the Store In list
*!**************************************************************************
*! Example   : = lfDeptEnab()
*!**************************************************************************
FUNCTION lfStorEnab

lnStorPos = ASCAN(laOGFxFlt,'INVLINE.STORE')
IF lnStorPos > 0
  lnStorPos = ASUBSCRIPT(laOGFxFlt,lnStorPos,1)
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnStorPos] = llStorStat
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnStorPos)) + ',6]')
  IF USED(laOGFxFlt[lnStorPos,6])
    lcAlias = ALIAS()
    SELECT laOGFxFlt[lnStorPos,6]
    ZAP
    USE 
    SELECT (lcAlias)
  ENDIF
ENDIF  



*!*************************************************************
*! Name      : lfSrSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Store In Range Filter.
*!*************************************************************
FUNCTION lfSrStr
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    lnAccPos = ASCAN(laOGFxFlt,'INVLINE.ACCOUNT')
    IF lnAccPos > 0
      lnAccPos = ASUBSCRIPT(laOGFxFlt,lnAccPos ,1)
    ENDIF
    SELECT CUSTOMER
    SET FILTER TO type+account+store = 'S'
    SCAN FOR SEEK(Account,LAOGFXFLT[lnAccPos,6])
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTempStor) FROM MEMVAR
    ENDSCAN
    SELECT CUSTOMER
    SET FILTER TO

    SELECT (lcTempStor)
    LOCATE
   
  CASE lcParm = 'R'  && Reset code
    SELECT CUSTOMER
    SET FILTER TO
    SELECT (lcTempStor)
    ZAP
ENDCASE

*!**************************************************************************
*! Name      : lfvSPeriod
*! Developer : Sameh Saiid (SSE)
*! Date      : 10/14/1999
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************
FUNCTION lfvSPeriod

IF !EMPTY(lcRpSPriod)
  lcSMonth = SUBSTR(lcRpSPriod,1,2)
  lcSMonth = IIF(EMPTY(lcSMonth ),'0',lcSMonth)
  lcSYear  = SUBSTR(lcRpSPriod,4,4)
  lcSYear  = IIF(EMPTY(lcSYear  ),'0',lcSYear)

  DIMENSION laPeriod[12,2]
  IF EVAL(lcSMonth) < 1 OR EVAL(lcSMonth) > 12
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Please enter a valid Month.'))
    STORE "" TO lcRpSPriod , lcSMonth , lcSYear
    RETURN .F.
  ENDIF

  IF LEN(ALLTRIM(lcSYear)) < 4 
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Please enter a valid Year.'))
    STORE "" TO lcRpSPriod , lcSMonth , lcSYear
    RETURN .F.
  ENDIF

ENDIF
*!**************************************************************************
*! Name      : lfGetMnth
*! Developer : Sameh Saiid (SSE)
*! Date      : 10/14/1999
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************
FUNCTION lfGetMnth
PARAMETERS lnMonth

DO CASE
  CASE lnMonth = 1
    RETURN 'Jan'

  CASE lnMonth = 2
    RETURN 'Feb'

  CASE lnMonth = 3
    RETURN 'Mar'

  CASE lnMonth = 4
    RETURN 'Apr'

  CASE lnMonth = 5
    RETURN 'May'

  CASE lnMonth = 6
    RETURN 'Jun'

  CASE lnMonth = 7
    RETURN 'Jul'

  CASE lnMonth = 8
    RETURN 'Aug'

  CASE lnMonth = 9
    RETURN 'Sep'

  CASE lnMonth = 10
    RETURN 'Oct'

  CASE lnMonth = 11
    RETURN 'Nov'

  CASE lnMonth = 12
    RETURN 'Des'
ENDCASE

*!**************************************************************************
*! Name      : lfvSPeriod
*! Developer : Sameh Saiid (SSE)
*! Date      : 10/14/1999
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************
FUNCTION lfvFPeriod

IF !EMPTY(lcRpFPriod)
  lcFMonth = SUBSTR(lcRpFPriod,1,2)
  lcFMonth = IIF(EMPTY(lcFMonth),'0',lcFMonth)
  lcFYear  = SUBSTR(lcRpFPriod,4,4)
  lcFYear  = IIF(EMPTY(lcFYear),'0',lcFYear  )
  
  IF EVAL(lcFMonth) < 1 OR EVAL(lcFMonth) > 12
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Please enter a valid Month.'))
    STORE "" TO lcRpFPriod , lcFMonth , lcFYear
    RETURN .F.
  ENDIF

  IF LEN(lcFYear) < 4 
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Please enter a valid Year.'))
    STORE "" TO lcRpFPriod , lcFMonth , lcFYear
    RETURN .F.
  ENDIF

  IF EVAL(lcFYear) < EVAL(lcSYear) 
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,"End Year can't be befor start Year.")
    STORE "" TO lcRpFPriod , lcFMonth , lcFYear
    RETURN .F.
  ENDIF
 
  IF  EVAL(lcFYear) = EVAL(lcSYear) AND EVAL(lcFMonth) < EVAL(lcSMonth) 
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,"End Month can't be befor start Month.")
    STORE "" TO lcRpFPriod , lcFMonth , lcFYear
    RETURN .F.
  ENDIF

  IF EVAL(lcFYear) - EVAL(lcSYear) > 1
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,"End Month Year can't be more than 12 months later than start Month Year.")
    STORE "" TO lcRpFPriod , lcFMonth , lcFYear
    RETURN .F.
  ENDIF
  
  IF EVAL(lcFYear) > EVAL(lcSYear)
    lnRemStrt = 12-EVAL(lcSMonth)
    lnMnthNum = EVAL(lcFMonth)+lnRemStrt
    IF lnMnthNum > 12
      =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,"End Month Year can't be more than 12 months later than start Month Year.")
      STORE "" TO lcRpFPriod , lcFMonth , lcFYear
      RETURN .F.
    ENDIF
  ENDIF  
ENDIF

*!**************************************************************************
*! Name      : lfMakeVald
*! Developer : Sameh Saiid (SSE)
*! Date      : 10/14/1999
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************
FUNCTION lfMakeVald

IF EMPTY(lcRpSPriod)
   =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,"Start Month Year Can't be empty.")
   RETURN .F.
ENDIF



*!*************************************************************
*! Name        : lfExprt2XL
*! Developer   : Nader Nabil (NNA)
*! Date        : 04/18/2005
*! Purpose     : Export data to Excel file
*!*************************************************************
*! Called from : Report Code
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfExprt2XL()
*!*************************************************************
FUNCTION lfExprt2xl

WAIT WINDOW NOWAIT 'Please wait...'


lcTempMemo = gfTempName()
SAVE TO (gcWorkDir+lcTempMemo+'.MEM') 
lcCommLine = (gcWorkDir+lcTempMemo+'.MEM')

IF USED(lctempFile)
  USE IN (lctempFile) 
ENDIF

lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",gcRepHome+'AR\'+[ARSLBMEX.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF
IF !USED(lcTempFile)
  USE (gcWorkDir+lcTempFile+'.dbf') ORDER TAG (lcTempFile) IN 0
ENDIF
WAIT CLEAR

*-- End of Function lfExprt2XL.

*!**************************************************************************
*! Name      : lfStStrVar
*! Developer : Sameh Saiid (SSE)
*! Date      : 10/14/1999
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************
FUNCTION lfStStrVar

FOR lnI = 1 TO 12
  lcI = ALLTRIM(STR(lnI))
  STORE 0 TO lnQty&lcI , lnVal&lcI
ENDFOR  
STORE 0 TO lnTotQty , lnTotVal , lnTotAmnt 

*!**************************************************************************
*! Name      : lfStAccVar
*! Developer : Sameh Saiid (SSE)
*! Date      : 10/14/1999
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************
FUNCTION lfStAccVar

FOR lnI = 1 TO 12
  lcI = ALLTRIM(STR(lnI))
  STORE 0 TO lnGQty&lcI, lnGVal&lcI
ENDFOR  
STORE 0 TO lnGTotQty , lnGTotVal  , lnGTotAmnt 



*!**************************************************************************
*! Name      : lfAcumVar
*! Developer : Sameh Saiid (SSE)
*! Date      : 10/14/1999
*! Purpose   :
*!**************************************************************************
*! Example   : 
*!**************************************************************************
FUNCTION lfAcumVar

*-- Update total variables
IF TYPE = 'Q' OR TYPE = 'R'
  FOR lnI = 1 TO 12
    lcI = ALLTRIM(STR(lnI))
    lnGQty&lcI = lnGQty&lcI + &lcTempFile..Qty&lcI
    lnQty&lcI  = lnQty&lcI  + &lcTempFile..Qty&lcI
  ENDFOR  
  lnGTotQty  = lnGTotQty + &lcTempFile..TotQty
  lnTotQty   = lnTotQty  + &lcTempFile..TotQty
  lnTotAmnt  = lnTotAmnt + &lcTempFile..TotQty*&lcTempFile..Price
  lnGTotAmnt = lnGTotAmnt + &lcTempFile..TotQty*&lcTempFile..Price
ENDIF
IF TYPE = 'V' OR lcRpType = 'V'
  FOR lnI = 1 TO 12
    lcI = ALLTRIM(STR(lnI))
    lnGVal&lcI = lnGVal&lcI + IIF(lcRpType = 'V',&lcTempFile..Qty&lcI*&lcTempFile..Price,&lcTempFile..Qty&lcI)
    lnVal&lcI  = lnVal&lcI  + IIF(lcRpType = 'V',&lcTempFile..Qty&lcI*&lcTempFile..Price,&lcTempFile..Qty&lcI)
  ENDFOR  
  lnGTotVal= lnGTotVal+ IIF(lcRpType = 'V',&lcTempFile..TotQty*&lcTempFile..Price,&lcTempFile..TotQty)
  lnTotVal = lnTotVal + IIF(lcRpType = 'V',&lcTempFile..TotQty*&lcTempFile..Price,&lcTempFile..TotQty)
ENDIF
    
    
*!*************************************************************
*! Name      : lfUpdPerid
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/12/2005
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

lcSMth = STR(EVAL(lcSMonth)-1,2)
lcSYr  = lcSYear 
FOR lnI = 1 to 12
  lcSMth = STR(EVAL(lcSMth)+1,2)
  IF EVAL(lcSMth) > 12
    lcSMth = ' 1'
    lcSYr  = STR(EVAL(lcSYr)+1,4)
  ENDIF
  laPeriod[lnI,1] = lcSMth+"/"+lcSYr
  lcMth = lfGetMnth(EVAL(lcSMth))
  laPeriod[lnI,2] = lcMth+SUBSTR(lcSYr,3,2)
ENDFOR  


lcFMth = STR(EVAL(lcFMonth),2)
lcFYr  = lcFYear 
IF TYPE('laPeriod[1,1]') = 'C'
  lcValue  = lcFMth+"/"+lcFYr
  lnEndPOs = ASCAN(laPeriod,lcValue)
  IF lnEndPOs > 0
    lnEndPOs= ASUBSCRIPT(laPeriod,lnEndPOs,1)
    FOR lnI = lnEndPOs+1 TO ALEN(laPeriod,1)
      STORE "" TO laPeriod[lnI,1] 
    ENDFOR
  ENDIF    
ENDIF


*--------mhmhmhmhmhmhm
*!*************************************************************
*! Name      : lfRetCalc
*! Developer : MOhamed Shokry (MHM)
*! Date      : 01/02/2006
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
PARAMETER lcAccount

lcScnExpr = "invoice+STR(lineno,6) = INVHDR.Invoice"
lcInvLine = 'INVLINE'
lnStrPos  = ATC('INLIST(INVLINE.STORE',lcRpExp)	 
IF lnStrPos = 0
  lnStrPos  = ATC('BETWEEN(INVLINE.STORE',lcRpExp)	 
ENDIF
lnStart   = IIF(ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)',lcRpExp) <> 0,ATC('AND  INLIST(SUBSTR(INVLINE.Style,1,lnStyLngth)',lcRpExp),LEN(lcRpExp))
IF lnStrPos <> 0
	lcStrExpr  = SUBSTR(lcRpExp,lnStrPos,lnStart-lnStrPos+IIF(lnStart=LEN(lcRpExp),1,0))
    lcStrExpr  = STRTRAN(lcStrExpr,'INVLINE','INVHDR')
ELSE  
  lcStrExpr = '.T.'
ENDIF  
IF USED(laOGFxFlt[3,6]) AND RECCOUNT(laOGFxFlt[3,6]) > 0
  lcRepExpr = 'SEEK(SUBSTR(RETLINE.Style,1,lnStyLngth),laOGFxFlt[3,6])'
ELSE
  lcRepExpr = '.T.'
ENDIF	
lcCrStrEpr =  STRTRAN(lcStrExpr,'INVHDR','RETHDR')

IF SEEK(lcAccount,'RETHDR')
  SELECT RETHDR
  SCAN REST WHILE account+crmemo = lcAccount FOR STATUS <> 'V'
    IF SEEK(Customer.account,lcTmAccStr) AND !(&lcCrStrEpr)
      LOOP
    ENDIF
    IF SEEK(RETHDR.crmemo,'RETLINE')
      SELECT RETLINE
      SCAN REST WHILE crmemo+style+cret_linno+cret_trncd = RETHDR.crmemo FOR &lcRepExpr 
      
        lcPriod = STR(MONTH(RETLINE.crdate),2)+'/'+STR(YEAR(RETLINE.crdate),4)
        lnPriod = ASCAN(laPeriod,lcPriod)
        IF lnPriod > 0
          lnPriod = ASUBSCRIPT(laPeriod,lnPriod,1)
          lcPrd = ALLTRIM(STR(lnPriod))
          =SEEK('S'+Account+RETHDR.Store,'CUST')
          lcStyle = SUBSTR(RETLINE.Style,1,lnStyLngth)  
          =SEEK(lcStyle,'STYLE')
          lcDesc  = STYLE.DESC
          IF !SEEK(RETHDR.crmemo+lcStyle+RETLINE.cret_linno,lcTempRetL)
     	    IF SEEK(lcAccount+IIF(llRpSrtBSt,RETHDR.Store,SPACE(8))+lcStyle+'A'+STR(RETLINE.Price,6),lcTempFile) OR ;
       		  SEEK(lcAccount+IIF(llRpSrtBSt,RETHDR.Store,SPACE(8))+lcStyle+'Z'+STR(RETLINE.Price,6),lcTempFile)
         	  SELECT (lcTempFile) 
    	      Replace Qty&lcPrd   WITH Qty&lcPrd + (-RETLINE.TotQty),;
              TotQty              WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8+Qty9+Qty10+Qty11+Qty12
  		  	  IF lcRpType = 'B' AND SEEK(ACCOUNT+IIF(llRpSrtBSt,Store,SPACE(8))+lcStyle+'Z'+STR(Price,6)+'V',lcTempFile)
	            Replace Qty&lcPrd   WITH Qty&lcPrd + (-RETLINE.TotQty*RETLINE.Price) ,;
                TotQty              WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8+Qty9+Qty10+Qty11+Qty12
              ENDIF                                  
            ELSE
              *hbghbg
              lcStorenam = CUST.Store
              *hbghbg
          	  lcRepeted = IIF(!SEEK(lcAccount+IIF(llRpSrtBSt,RETHDR.Store,SPACE(8))+lcStyle,lcTempFile),'A','Z')
	    	  INSERT INTO (lcTempFile) (Account,AccName,Store,StoreNam,Style,DESC,Price,Qty&lcPrd,TotQty,Type,cRepeted,llEnd ) ;
   	       	         Values (lcAccount,lcAccname,IIF(llRpSrtBSt,RETHDR.Store,SPACE(8)),lcStoreNam,lcStyle,lcDesc,RETLINE.Price,;
    		         -RETLINE.TotQty,-RETLINE.TotQty,'R',lcRepeted,.F.)     
              IF lcRpType = 'B' 
            	INSERT INTO (lcTempFile) (Account,AccName,Store,StoreNam,Style,DESC,Price,Qty&lcPrd,TotQty,Type,cRepeted,llEnd ) ;
            	Values (lcAccount,lcAccname,IIF(llRpSrtBSt,RETHDR.Store,SPACE(8)),lcStoreNam,lcStyle,lcDesc,RETLINE.Price,;
    			-RETLINE.TotQty*RETLINE.Price,-RETLINE.TotQty*RETLINE.Price,'V','Z',.F.)     
              ENDIF    			           		   		   
            ENDIF     
            INSERT INTO (lcTempRetL) (crmemo , cret_linno ,STYLE ) ;
                   VALUES (RETHDR.crmemo , RETLINE.cret_linno , lcStyle)
          ENDIF  
        ENDIF  
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF          

*!*************************************************************
*! Name      : lfsrAcc2
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Account In Range Filter. 2
*!*************************************************************
*!
FUNCTION lfsrAcc2
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    CLEAR READ   && Clear current read cycle, to activate new one.
ENDCASE
 
 
*!*************************************************************
*! Name      : lfsrAcc2
*! Developer : Mohamed Badran (MAB)
*! Date      : 09/09/99
*! Purpose   : Account In Range Filter. 2
*!*************************************************************
*!
FUNCTION lfEndRep
PARAMETERS lcParm

llEndRep = &lcTempFile..llEnd
