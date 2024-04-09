*:***************************************************************************
*: Program file  : SRORDBOK.PRG
*: Program desc. : Custom Sales Rep. Booking report for ARU10
*: For Report    : SRORDBOK.FRX
*: System        : Aria4XP
*: Module        : SR
*: Developer     : Mariam Mazhar(MMT)
*: Entry         : C201673{T20140606.0005}
*:***************************************************************************
*: Modifications:
*: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012]
*:***************************************************************************
lcStTime = TIME()
*IF llOgFltCh
lfCreateTemp()
lfCollectData()
*ENDIF
SELECT (lcDataFile)
LOCATE 
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN 
ENDIF
IF lcRpGroup ='A'
  SET ORDER TO STATACCT IN (lcDataFile)
ELSE
  SET ORDER TO (lcDataFile) IN (lcDataFile)
ENDIF 
LOCATE 
=gfDispRe(lcRPForm)
*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/04/2015
*! Purpose   : Create temp. Cursor 
*!*************************************************************
FUNCTION lfCreateTemp
DIMENSION laFileStr[13,4]

laFileStr[1,1] = 'repcode'
laFileStr[1,2] = 'C'
laFileStr[1,3] = 3
laFileStr[1,4] = 0

laFileStr[2,1] = 'Name'
laFileStr[2,2] = 'C'
laFileStr[2,3] = 24
laFileStr[2,4] = 0

laFileStr[3,1] = 'State'
laFileStr[3,2] = 'C'
laFileStr[3,3] = 6
laFileStr[3,4] = 0

laFileStr[4,1] = 'Account'
laFileStr[4,2] = 'C'
laFileStr[4,3] = 5
laFileStr[4,4] = 0

laFileStr[5,1] = 'stname'
laFileStr[5,2] = 'C'
laFileStr[5,3] = 30
laFileStr[5,4] = 0

laFileStr[6,1] = 'nCWeQty'
laFileStr[6,2] = 'N'
laFileStr[6,3] = 12
laFileStr[6,4] = 0

laFileStr[7,1] = 'nCWeAmt'
laFileStr[7,2] = 'N'
laFileStr[7,3] = 15
laFileStr[7,4] = 3

laFileStr[8,1] = 'nPWeQty'
laFileStr[8,2] = 'N'
laFileStr[8,3] = 12
laFileStr[8,4] = 0

laFileStr[9,1] = 'nPWeAmt'
laFileStr[9,2] = 'N'
laFileStr[9,3] = 15
laFileStr[9,4] = 3

laFileStr[10,1] = 'nCYTDQty'
laFileStr[10,2] = 'N'
laFileStr[10,3] = 12
laFileStr[10,4] = 0

laFileStr[11,1] = 'nCYTDAmt'
laFileStr[11,2] = 'N'
laFileStr[11,3] = 15
laFileStr[11,4] = 3

laFileStr[12,1] = 'nPYTDQty'
laFileStr[12,2] = 'N'
laFileStr[12,3] = 12
laFileStr[12,4] = 0

laFileStr[13,1] = 'nPYTDAmt'
laFileStr[13,2] = 'N'
laFileStr[13,3] = 15
laFileStr[13,4] = 3

=gfCrtTmp(lcDataFile,@laFileStr,"REPCODE+STATE+Account",lcDataFile,.F.)
SELECT (lcDataFile)
INDEX on STATE+Account ADDITIVE TAG 'STATACCT'
SET ORDER TO (lcDataFile)
*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/04/2015
*! Purpose   : Collect data
*!*************************************************************
FUNCTION lfCollectData

llSeleAccount = .F. 
lcFileAccount= ''
lnPosAccount= ASCAN(laOGFXFLT,"ORDHDR.ACCOUNT")
IF lnPosAccount> 0
  lnPosAccount= ASUBSCRIPT(laOGFXFLT,lnPosAccount,1)
  lcFileAccount=IIF(!EMPTY(laOGFXFLT[lnPosAccount,6]),laOGFXFLT[lnPosAccount,6],'')
  IF !EMPTY(lcFileAccount) 
    SELECT (lcFileAccount)
    LOCATE 
    IF !EOF()
      llSeleAccount = .T.
    ENDIF  
  ENDIF
ENDIF

llSeleRep = .F. 
lcFileRep = ''
lnPosRep= ASCAN(laOGFXFLT,"ORDHDR.Rep1")
IF lnPosRep > 0
  lnPosRep= ASUBSCRIPT(laOGFXFLT,lnPosRep,1)
  lcFileRep=IIF(!EMPTY(laOGFXFLT[lnPosRep,6]),laOGFXFLT[lnPosRep,6],'')
  IF !EMPTY(lcFileRep) 
    SELECT (lcFileRep)
    LOCATE 
    IF !EOF()
      llSeleRep = .T.
    ENDIF  
  ENDIF
ENDIF

llDateStartSel = .F. && flag to indicate if user Selected date range or not
lnPosStartDate = ASCAN(laOGFXFLT,"ORDHDR.START")
ldStartEnd = {}
ldStartStart = {}
IF lnPosStartDate  > 0
  lnPosStartDate = ASUBSCRIPT(laOGFXFLT,lnPosStartDate ,1)
  lcStartDate =IIF(!EMPTY(laOGFXFLT[lnPosStartDate ,6]),laOGFXFLT[lnPosStartDate ,6],'')
  IF !EMPTY(lcStartDate)
    llDateStartSel = .T.
    ldStartEnd =CTOD(SUBSTR(lcStartDate,ATC('|',lnPosStartDate)+1))
    ldStartStart = CTOD(SUBSTR(lcStartDate,1,ATC('|',lnPosStartDate)-1))
  ENDIF
ENDIF

llDateCompSel = .F. && flag to indicate if user Selected date range or not
lnPosCompDate = ASCAN(laOGFXFLT,"ORDHDR.COMPLETE")
ldComplEnd = {}
ldComplStart = {}
IF lnPosCompDate  > 0
  lnPosCompDate= ASUBSCRIPT(laOGFXFLT,lnPosCompDate,1)
  lcCOmpDate =IIF(!EMPTY(laOGFXFLT[lnPosCompDate,6]),laOGFXFLT[lnPosCompDate,6],'')
  IF !EMPTY(lcCOmpDate)
    llDateCompSel= .T.
    ldComplEnd = CTOD(SUBSTR(lcCOmpDate,ATC('|',lnPosCompDate)+1))
    ldComplStart = CTOD(SUBSTR(lcCOmpDate,1,ATC('|',lnPosCompDate)-1))
  ENDIF
ENDIF

*: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][Start]
ldCurrYearBegin ={}
lnCurrYear = Year(ldRpEndDat)
lnPrevYear = Year(ldRpEndDat)-1
ldCurrYearBegin = Ctod("12/01/"+STR(lnCurrYear ,4))
ldPrevYrBegin = Ctod("12/01/"+STR(lnPrevYear ,4))
IF !USED('FISHD')
  =gfOpenTable('FISHD','COMPFYEAR','SH')
ENDIF
Select FISHD
Locate for Between(ldRpEndDat,FISHD.dfisbgdat,FISHD.dfisendat) and !Dele()
if Found()
  ldCurrYearBegin =FISHD.dfisbgdat
  lnCurrYear = Val(FISHD.cfisfyear)
  lnPrevYear = lnCurrYear -1
  if gfSeek(Str(lnPrevYear,4),'FISHD','COMPFYEAR')
    ldPrevYrBegin = FISHD.dfisbgdat
  Endif
ENDIF
*: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][End]


IF !USED('Salesrep_A')
  =gfOpenTable('Salesrep','Salesrep','SH','Salesrep_A')
ENDIF

IF !USED('Customer_A')
  =gfOpenTable('Customer','Customer','SH','Customer_A')
ENDIF


IF !USED('Ordhdr_A')
  =gfOpenTable('Ordhdr','Ordhdr','SH','Ordhdr_A')
ENDIF
SELECT ORDHDR_A
=gfSetOrder('ORDHDR') 
SELECT ORDHDR_A
=gfSEEK("O")
SCAN REST WHILE CORDTYPE+ORDER = "O" FOR Status <> 'X' AND (!EMPTY(ORDHDR_A.rep1) OR !EMPTY(ORDHDR_A.rep2)) AND  ;
                    IIF(llSeleAccount,SEEK(Ordhdr_a.Account,lcFileAccount),.T.) AND ;
                    IIF(llSeleRep,SEEK(ORDHDR_A.rep1,lcFileRep) OR SEEK(ORDHDR_A.rep2,lcFileRep),.T.) AND ;
                    IIF(llDateCompSel,BETWEEN(ORDHDR_A.Complete,ldComplStart,ldComplEnd),.T.) AND;
                    IIF(llDateStartSel ,BETWEEN(ORDHDR_A.Start,ldStartStart ,ldStartEnd ),.T.)
       
       
       =gfSeek('M'+Ordhdr_a.Account,'CUSTOMER_A','CUSTOMER')
       IF lcRpGroup ='A'
        
         IF (!EMPTY(ORDHDR_A.rep1) AND IIF(llSeleRep,SEEK(ORDHDR_A.rep1,lcFileRep) ,.T.)) OR ;
            (!EMPTY(ORDHDR_A.rep2) AND IIF(llSeleRep,SEEK(ORDHDR_A.rep2,lcFileRep) ,.T.))
         IF !SEEK(SUBSTR(Customer_A.caddress4,1,6)+Ordhdr_a.Account,lcDataFile,'STATACCT')  
           SELECT (lcDataFile)
           APPEND BLANK 
           m.repcode = ORDHDR_A.rep1
		   m.Name = IIF(gfSeek(ORDHDR_A.rep1,'Salesrep_A','Salesrep'),Salesrep_A.Name,'')
		   m.State = SUBSTR(Customer_A.caddress4,1,6)
		   m.Account = Ordhdr_a.Account
		   m.stname = Customer_A.StName
		   GATHER MEMO MEMVAR 
		 ENDIF 
		   
         DO CASE
           CASE BETWEEN(Ordhdr_a.entered,ldRpEndDat-6,ldRpEndDat)
             REPLACE nCWeQty WITH nCWeQty + Ordhdr_a.Book - Ordhdr_a.Cancel,;
		  		     nCWeAmt WITH nCWeAmt +   Ordhdr_a.BookAmt - Ordhdr_a.Cancelamt IN (lcDataFile)
  		   CASE BETWEEN(Ordhdr_a.entered,GOMONTH(ldRpEndDat-6,-12),GOMONTH(ldRpEndDat,-12))
             REPLACE npWeQty WITH nPWeQty + Ordhdr_a.Book - Ordhdr_a.Cancel,;
		  		     nPWeAmt WITH nPWeAmt + Ordhdr_a.BookAmt - Ordhdr_a.Cancelamt  IN (lcDataFile)
         ENDCASE  
                      
         DO CASE
           *: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][Start]
           *CASE BETWEEN(Ordhdr_a.entered,CTOD("12/01/"+STR(YEAR(ldRpEndDat)-1)),ldRpEndDat)
           CASE BETWEEN(Ordhdr_a.entered,ldCurrYearBegin,ldRpEndDat)
           *: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][End]
             REPLACE nCYTDQty WITH nCYTDQty + Ordhdr_a.Book- Ordhdr_a.Cancel,;
		  		     nCYTDAmt WITH nCYTDAmt +   Ordhdr_a.BookAmt - Ordhdr_a.Cancelamt   IN (lcDataFile)
		  	*: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][Start]	     
		  	*CASE BETWEEN(Ordhdr_a.entered,CTOD("12/01/"+STR(YEAR(GOMONTH(ldRpEndDat,-12))-1)),GOMONTH(ldRpEndDat,-12))
   		    CASE BETWEEN(Ordhdr_a.entered,ldPrevYrBegin,GOMONTH(ldRpEndDat,-12))
  		    *: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][End]
             REPLACE nPYTDQty WITH nPYTDQty + Ordhdr_a.Book - Ordhdr_a.Cancel,;
		  		     nPYTDAmt WITH nPYTDAmt + Ordhdr_a.BookAmt - Ordhdr_a.Cancelamt   IN (lcDataFile)
         ENDCASE               

       ENDIF  
          
          
          
          
          
       ELSE
       
       IF !EMPTY(ORDHDR_A.rep1) AND IIF(llSeleRep,SEEK(ORDHDR_A.rep1,lcFileRep) ,.T.)
         IF !SEEK(ORDHDR_A.rep1+SUBSTR(Customer_A.caddress4,1,6)+Ordhdr_a.Account,lcDataFile)  
           SELECT (lcDataFile)
           APPEND BLANK 
           m.repcode = ORDHDR_A.rep1
		   m.Name = IIF(gfSeek(ORDHDR_A.rep1,'Salesrep_A','Salesrep'),Salesrep_A.Name,'')
		   m.State = SUBSTR(Customer_A.caddress4,1,6)
		   m.Account = Ordhdr_a.Account
		   m.stname = Customer_A.StName
		   GATHER MEMO MEMVAR 
		 ENDIF 
		   
         DO CASE
           CASE BETWEEN(Ordhdr_a.entered,ldRpEndDat-6,ldRpEndDat)
             REPLACE nCWeQty WITH nCWeQty + Ordhdr_a.Book - Ordhdr_a.Cancel,;
		  		     nCWeAmt WITH nCWeAmt +   Ordhdr_a.BookAmt - Ordhdr_a.Cancelamt IN (lcDataFile)
  		   CASE BETWEEN(Ordhdr_a.entered,GOMONTH(ldRpEndDat-6,-12),GOMONTH(ldRpEndDat,-12))
             REPLACE npWeQty WITH nPWeQty + Ordhdr_a.Book - Ordhdr_a.Cancel,;
		  		     nPWeAmt WITH nPWeAmt + Ordhdr_a.BookAmt - Ordhdr_a.Cancelamt  IN (lcDataFile)
         ENDCASE  
                      
         DO CASE
           *: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][Start]
           *CASE BETWEEN(Ordhdr_a.entered,CTOD("12/01/"+STR(YEAR(ldRpEndDat)-1)),ldRpEndDat)
           CASE BETWEEN(Ordhdr_a.entered,ldCurrYearBegin,ldRpEndDat)
           *: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][End]
             REPLACE nCYTDQty WITH nCYTDQty + Ordhdr_a.Book- Ordhdr_a.Cancel,;
		  		     nCYTDAmt WITH nCYTDAmt +   Ordhdr_a.BookAmt - Ordhdr_a.Cancelamt   IN (lcDataFile)
 		   *: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][Start]	     
  		   *CASE BETWEEN(Ordhdr_a.entered,CTOD("12/01/"+STR(YEAR(GOMONTH(ldRpEndDat,-12))-1)),GOMONTH(ldRpEndDat,-12))
  		   CASE BETWEEN(Ordhdr_a.entered,ldPrevYrBegin,GOMONTH(ldRpEndDat,-12))
  		   *: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][End]
             REPLACE nPYTDQty WITH nPYTDQty + Ordhdr_a.Book - Ordhdr_a.Cancel,;
		  		     nPYTDAmt WITH nPYTDAmt + Ordhdr_a.BookAmt - Ordhdr_a.Cancelamt   IN (lcDataFile)
         ENDCASE               

       ENDIF
       
       IF !EMPTY(ORDHDR_A.rep2) AND IIF(llSeleRep,SEEK(ORDHDR_A.rep2,lcFileRep) ,.T.)
         IF !SEEK(ORDHDR_A.rep2+SUBSTR(Customer_A.caddress4,1,6)+Ordhdr_a.Account,lcDataFile)  
           SELECT (lcDataFile)
           APPEND BLANK 
           m.repcode = ORDHDR_A.rep2
		   m.Name = IIF(gfSeek(ORDHDR_A.rep2,'Salesrep_A','Salesrep'),Salesrep_A.Name,'')
		   m.State = SUBSTR(Customer_A.caddress4,1,6)
		   m.Account = Ordhdr_a.Account
		   m.stname = Customer_A.StName
		   GATHER MEMO MEMVAR 
		 ENDIF 
		   
         DO CASE
           CASE BETWEEN(Ordhdr_a.entered,ldRpEndDat-6,ldRpEndDat)
             REPLACE nCWeQty WITH nCWeQty + Ordhdr_a.Book- Ordhdr_a.Cancel,;
		  		     nCWeAmt WITH nCWeAmt +   Ordhdr_a.BookAmt - Ordhdr_a.Cancelamt   IN (lcDataFile)
  		   CASE BETWEEN(Ordhdr_a.entered,GOMONTH(ldRpEndDat-6,-12),GOMONTH(ldRpEndDat,-12))
             REPLACE npWeQty WITH nPWeQty + Ordhdr_a.Book- Ordhdr_a.Cancel,;
		  		     nPWeAmt WITH nPWeAmt + Ordhdr_a.BookAmt - Ordhdr_a.Cancelamt   IN (lcDataFile)
         ENDCASE  
                      
         DO CASE
           *: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][Start]
           *CASE BETWEEN(Ordhdr_a.entered,CTOD("12/01/"+STR(YEAR(ldRpEndDat)-1)),ldRpEndDat)
           CASE BETWEEN(Ordhdr_a.entered,ldCurrYearBegin,ldRpEndDat)
           *: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][End]
             REPLACE nCYTDQty WITH nCYTDQty + Ordhdr_a.Book - Ordhdr_a.Cancel,;
		  		     nCYTDAmt WITH nCYTDAmt +   Ordhdr_a.BookAmt - Ordhdr_a.Cancelamt   IN (lcDataFile)
		   *: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][Start]
  		   *CASE BETWEEN(Ordhdr_a.entered,CTOD("12/01/"+STR(YEAR(GOMONTH(ldRpEndDat,-12))-1)),GOMONTH(ldRpEndDat,-12))
  		   CASE BETWEEN(Ordhdr_a.entered,ldPrevYrBegin,GOMONTH(ldRpEndDat,-12))
  		   *: B611244,1 MMT 01/11/2017 ARU10 Sales rep. Booking report calculates YTD incorrectly[T20161219.0012][End]
             REPLACE nPYTDQty WITH nPYTDQty + Ordhdr_a.Book - Ordhdr_a.Cancel,;
		  		     nPYTDAmt WITH nPYTDAmt + Ordhdr_a.BookAmt - Ordhdr_a.Cancelamt   IN (lcDataFile)
         ENDCASE               
      ENDIF
      ENDIF 
ENDSCAN
*!**************************************************************************
*! Name      : lfSeTSRep
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/04/2015
*! Purpose   : Go top in Sales Rep file.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : OpGrdParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSerSRep()
*!*************************************************************
FUNCTION lfSetSRep
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SELECT SalesRep
   SET ORDER TO TAG SalesRep
   LOCATE
  CASE OpGrdParm = 'R'
    SELECT SalesRep
    SET ORDER TO
ENDCASE
*-- End of lfSetS
FUNCTION lfWRunGrid()