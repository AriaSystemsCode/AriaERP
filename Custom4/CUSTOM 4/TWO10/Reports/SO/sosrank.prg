*:***************************************************************************
*: Program file  : SOSRANK.PRG
*: Program desc. : Sales Ranking Report for TWO10
*: For Report    : (SOSRANK.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mohamed Shokry Mohamed (MHM)
*: Date          : 01/22/2004
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,lfSeTSRep,lfGetMem,lfRankFile
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOSRANK
*:***************************************************************************
*: This Report Program is due to C037308 ...
*:***************************************************************************
*: Modifications :
*:***************************************************************************
lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.
lcStTime = TIME()

IF loOgScroll.llOGFltCh && OG Filters changed
  WAIT WINDOW "Collecting Data......." NOWAIT 
  *-- if you have previous data clear workfile then recreate it. [begin]
  IF !USED(RANK_FILE) OR (RECCOUNT(RANK_FILE) > 0)
    IF USED(RANK_FILE)
      USE IN (RANK_FILE)
    ENDIF  
    = lfRankFile()  && Create temporary cursor.
  ENDIF
  lfCollect()
ENDIF 


loOgScroll.cCRorientation = 'P'

IF RECCOUNT(RANK_FILE) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF



SELECT (RANK_FILE)
IF RECCOUNT(RANK_FILE) > 0
  IF lcRpSortBy = 'M' 
    INDEX ON STR(Amount,14)+Rep+Account TAG (RANK_FILE1) OF (RANK_FILE) DESC
  ENDIF
  IF lcRpSortBy = 'R' 
    INDEX ON Rep+STR(Amount,14) TAG (RANK_FILE1) OF (RANK_FILE) DESC
   ENDIF
  IF lcRpSortBy = 'A' 
    INDEX ON Account TAG (RANK_FILE1) OF (RANK_FILE)
  ENDIF
ENDIF  

*-- Calculate spent time in collecting data.
DO gfDispRe WITH EVAL('lcRpForm')
*-- end of report code.

*!*************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar(MMT)
*! Date      : 11/08/2006
*! Purpose   : Collecting repor data
*!*************************************************************
FUNCTION lfCollect

*--Shared Filters in Sales and booked cases

*1-Copmplete date
lcStartDate = {}
lcEndDate   = {}
lnDatePos = ASCAN(loogscroll.laOGFxFlt,'ORDHDR.COMPLETE')
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(loogscroll.laOGFxFlt,lnDatePos,1)
ENDIF  
lcStartDate = IIF(EMPTY(SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],1,10)),DTOC(CTOD("")),SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],1,10))
lcEndDate   = IIF(EMPTY(SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],12,21)),DTOC(CTOD("")),SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],12,21))

*2-season
llUseSea = .F.
lnSeaPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'ORDHDR.SEASON'),1)
IF lnSeaPos > 0
  lcSeaStr = LOOGSCROLL.laOGFXFlt[lnSeaPos,6]
  lcSeaFile = loOGScroll.gfTempName()
  llUseSea = IIF(LEN(lcSeaStr)>0,.T.,.F.) AND lfConvertToCursor(lcSeaStr,'SEASON',lcSeaFile)
ENDIF

*3-Account
llUseAcc = .F.
lcAccFile = ''
lnAccPos = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
IF lnAccPos > 0
  lcAccFile = LOOGSCROLL.laOGFxFlt[lnAccPos,6]
  llUseAcc = IIF(!EMPTY(lcAccFile) .AND. USED(lcAccFile) .AND. RECCOUNT(lcAccFile)>0,.T.,.F.)
ENDIF
IF llUseAcc
  SELECT(lcAccFile)
  LOCATE 
  IF EOF()
    llUseAcc = .F.
  ENDIF 
ENDIF 

*4-Sales Rep.
llUseRep = .F.
lcRepFile = ''
lnRepPos = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'ORDHDR.REP1'),1)
IF lnRepPos > 0
  lcRepFile = LOOGSCROLL.laOGFxFlt[lnRepPos,6]
  llUseRep  = IIF(!EMPTY(lcRepFile) .AND. USED(lcRepFile) .AND. RECCOUNT(lcRepFile)>0,.T.,.F.)
ENDIF
IF llUseRep
  SELECT(lcRepFile)
  LOCATE 
  IF EOF()
    llUseRep = .F.
  ENDIF 
ENDIF 


*--Start Collecting Data
*Sales
STORE '' To m.Account , m.Name , m.buyer , m.phone , m.Rep 
STORE 0  TO m.Amount , m.Amtbook , m.Amtopen

IF lcRpRankBy = "S"
  IF llUseAcc
    SELECT INVHDR
    =gfSetorder('INVHDRA')
    SELECT(lcAccFile)
    SCAN
      IF gfSeek(Account,'InvHdr')
        SELECT INVHDR
        SCAN REST WHILE ACCOUNT+INVOICE = &lcAccFile..Account FOR Status <> "V" AND ;
           			IIF(!EMPTY(CTOD(lcStartDate)) AND !EMPTY(CTOD(lcEndDate)),BETWEEN(INVHDR.INVDATE,CTOD(lcStartDate),CTOD(lcEndDate)),.T.);
           			AND  IIF(llUseSea ,SEEK(INVHDR.Season,lcSeaFile),.T.) AND ;
           			IIF(llUseRep,SEEK(INVHDR.REP1,lcRepFile),.T.) AND gfSeek('M'+Account,'Customer')
           
          m.Account   = Customer.Account
    		  m.Name      = Customer.btName
    			m.buyer     = Customer.buyer
      		m.phone     = Customer.phone1
  		    m.Rep   	= InvHdr.Rep1 
    	    m.Amount	= InvHdr.TotalChg
	        IF SEEK(m.Rep+m.account,RANK_FILE)
	  	      SELECT (RANK_FILE)
		    	  REPLACE Amount    WITH Amount    + m.Amount 
    			ELSE  && Add new style record.
		    	  IF !EMPTY(m.account) OR !EMPTY(m.Rep)
			        IF m.Amount<>0 
		            INSERT INTO (RANK_FILE) FROM MEMVAR
  		        ENDIF
	    		    STORE 0 to m.Amount 
	          ENDIF
    			ENDIF
			  ENDSCAN 
      ENDIF 
    ENDSCAN 
  ELSE
    SELECT INVHDR
    =gfSeek('')
    SCAN FOR Status <> "V" AND IIF(!EMPTY(CTOD(lcStartDate)) AND !EMPTY(CTOD(lcEndDate)),BETWEEN(INVHDR.INVDATE,CTOD(lcStartDate),CTOD(lcEndDate)),.T.);
             AND IIF(llUseSea,SEEK(INVHDR.Season,lcSeaFile),.T.) AND ;
             IIF(llUseRep,SEEK(INVHDR.REP1,lcRepFile),.T.) ;
             AND gfSeek('M'+Account,'Customer')
           
      m.Account   = Customer.Account
      m.Name      = Customer.btName
      m.buyer     = Customer.buyer
      m.phone     = Customer.phone1
      m.Rep     = InvHdr.Rep1 
      m.Amount  = InvHdr.TotalChg
      IF SEEK(m.Rep+m.account,RANK_FILE)
        SELECT (RANK_FILE)
        REPLACE Amount    WITH Amount    + m.Amount 
      ELSE  && Add new style record.
        IF !EMPTY(m.account) OR !EMPTY(m.Rep)
          IF m.Amount<>0 
            INSERT INTO (RANK_FILE) FROM MEMVAR
          ENDIF
          STORE 0 to m.Amount 
        ENDIF
      ENDIF
    ENDSCAN 
  ENDIF 
ELSE
  IF lcRpRankBy = "B"
    IF llUseAcc
      SELECT ORDHDR
      =gfSetorder('ORDACCT')
      SELECT(lcAccFile)  
      SCAN 
        IF gfSeek(Account+'O','Ordhdr')
          SELECT ORDHDR
          SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = &lcAccFile..Account+'O' FOR ;
               IIF(lcRpOrdSt='A',.T.,(OrdHdr.Status $ lcRpOrdSt))   AND ;
               IIF(!("X" $ lcRpOrdSt),Status<> "X",.T.) AND;
               IIF(!EMPTY(CTOD(lcStartDate)) AND !EMPTY(CTOD(lcEndDate)),BETWEEN(ORDHDR.COMPLETE,CTOD(lcStartDate),CTOD(lcEndDate)),.T.);
               AND IIF(llUseSea,SEEK(ORDHDR.Season,lcSeaFile),.T.) AND ;
               IIF(llUseRep,SEEK(ORDHDR.REP1,lcRepFile),.T.) ;
               AND gfSeek('M'+Account,'Customer')         

            m.Account   = Customer.Account
            m.Name      = Customer.btName
            m.buyer     = Customer.buyer
            m.phone     = Customer.phone1
            m.Rep       = OrdHdr.Rep1
            IF gfSEEK('O'+ORDHDR.ORDER,'ORDLINE')
              ldDatRange =IIF(!EMPTY(STDATE) OR !EMPTY(NDDATE)," FOR BETWEEN(ORDLINE.Complete,STDATE,NDDATE)",'')
              SELECT ORDLINE


              SCAN REST WHILE cOrdtype+Order = 'O'+ Ordhdr.order FOR;
                   IIF(!EMPTY(CTOD(lcStartDate)) AND !EMPTY(CTOD(lcEndDate)),BETWEEN(ORDLINE.COMPLETE,CTOD(lcStartDate),CTOD(lcEndDate)),.T.)

                =gfSeek(Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6) ,'Ordcanln')
                m.Amount=m.Amount + (Ordline.TotBook*Ordline.Price)
                IF gfSEEK(Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6),'Ordcanln')
                  SELECT OrdCanln
                  SCAN REST WHILE Cordtype+ Order+STR(lineno,6)= Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6)
                    m.Amount=m.Amount - (OrdCanln.TotQty*Ordline.Price)
                  ENDSCAN
                ENDIF
              ENDSCAN
            ENDIF    
            
            IF SEEK(m.Rep+m.account,RANK_FILE)
              SELECT (RANK_FILE)
              REPLACE Amount    WITH Amount    + m.Amount 
              STORE 0 to m.Amount
              REPLACE AMTBOOK   WITH AMTBOOK   + m.AMTBOOK , AMTOPEN WITH AMTOPEN + m.AMTOPEN
              STORE 0 TO m.AMTBOOK,AMTOPEN
            ELSE  && Add new style record.
              IF !EMPTY(m.account) OR !EMPTY(m.Rep)
                IF m.AMTBOOK<>0 OR m.AMTOPEN<>0 OR m.Amount <> 0
                  INSERT INTO (RANK_FILE) FROM MEMVAR
                ENDIF
                STORE 0 to m.Amount , m.AMTBOOK , m.AMTOPEN
              ENDIF
            ENDIF
          ENDSCAN         
        ENDIF 
      ENDSCAN 
    ELSE
      SELECT ORDHDR
      gfSetorder('ORDHDR')
      gfSeek('O')
      SCAN REST WHILE CORDTYPE+ORDER ='O' FOR ;
             IIF(lcRpOrdSt='A',.T.,(OrdHdr.Status $ lcRpOrdSt))   AND ;
             IIF(!("X" $ lcRpOrdSt),Status<> "X",.T.) AND;
             IIF(!EMPTY(CTOD(lcStartDate)) AND !EMPTY(CTOD(lcEndDate)),BETWEEN(ORDHDR.COMPLETE,CTOD(lcStartDate),CTOD(lcEndDate)),.T.);
             AND IIF(llUseSea,SEEK(ORDHDR.Season,lcSeaFile),.T.) AND ;
             IIF(llUseRep,SEEK(ORDHDR.REP1,lcRepFile),.T.) ;
             AND gfSeek('M'+Account,'Customer')         

          m.Account   = Customer.Account
          m.Name      = Customer.btName
          m.buyer     = Customer.buyer
          m.phone     = Customer.phone1
          m.Rep       = OrdHdr.Rep1
          IF gfSEEK('O'+ORDHDR.ORDER,'ORDLINE')
            ldDatRange =IIF(!EMPTY(STDATE) OR !EMPTY(NDDATE)," FOR BETWEEN(ORDLINE.Complete,STDATE,NDDATE)",'')
            SELECT ORDLINE


            SCAN REST WHILE cOrdtype+Order = 'O'+ Ordhdr.order FOR;
                 IIF(!EMPTY(CTOD(lcStartDate)) AND !EMPTY(CTOD(lcEndDate)),BETWEEN(ORDLINE.COMPLETE,CTOD(lcStartDate),CTOD(lcEndDate)),.T.)

              =gfSeek(Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6) ,'Ordcanln')
              m.Amount=m.Amount + (Ordline.TotBook*Ordline.Price)
              IF gfSEEK(Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6),'Ordcanln')
                SELECT OrdCanln
                SCAN REST WHILE Cordtype+ Order+STR(lineno,6)= Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6)
                  m.Amount=m.Amount - (OrdCanln.TotQty*Ordline.Price)
                ENDSCAN
              ENDIF
            ENDSCAN
          ENDIF    
          
          IF SEEK(m.Rep+m.account,RANK_FILE)
            SELECT (RANK_FILE)
            REPLACE Amount    WITH Amount    + m.Amount 
            STORE 0 to m.Amount
            REPLACE AMTBOOK   WITH AMTBOOK   + m.AMTBOOK , AMTOPEN WITH AMTOPEN + m.AMTOPEN
            STORE 0 TO m.AMTBOOK,AMTOPEN
          ELSE  && Add new style record.
            IF !EMPTY(m.account) OR !EMPTY(m.Rep)
              IF m.AMTBOOK<>0 OR m.AMTOPEN<>0 OR m.Amount <> 0
                INSERT INTO (RANK_FILE) FROM MEMVAR
              ENDIF
              STORE 0 to m.Amount , m.AMTBOOK , m.AMTOPEN
            ENDIF
          ENDIF
        ENDSCAN          
    ENDIF
  ELSE
   IF lcRpRankBy = "T" 

  IF llUseAcc
    SELECT INVHDR
    =gfSetorder('INVHDRA')
    SELECT(lcAccFile)
    SCAN
      IF gfSeek(Account,'InvHdr')
        SELECT INVHDR
        SCAN REST WHILE ACCOUNT+INVOICE = &lcAccFile..Account FOR Status <> "V" AND ;
                 IIF(!EMPTY(CTOD(lcStartDate)) AND !EMPTY(CTOD(lcEndDate)),BETWEEN(INVHDR.INVDATE,CTOD(lcStartDate),CTOD(lcEndDate)),.T.);
                 AND  IIF(llUseSea ,SEEK(INVHDR.Season,lcSeaFile),.T.) AND ;
                 IIF(llUseRep,SEEK(INVHDR.REP1,lcRepFile),.T.) AND gfSeek('M'+Account,'Customer')
           
          m.Account   = Customer.Account
          m.Name      = Customer.btName
          m.buyer     = Customer.buyer
          m.phone     = Customer.phone1
          m.Rep     = InvHdr.Rep1 
          m.Amount  = InvHdr.TotalChg
          IF SEEK(m.Rep+m.account,RANK_FILE)
            SELECT (RANK_FILE)
            REPLACE Amount    WITH Amount    + m.Amount 
          ELSE  && Add new style record.
            IF !EMPTY(m.account) OR !EMPTY(m.Rep)
              IF m.Amount<>0 
                INSERT INTO (RANK_FILE) FROM MEMVAR
              ENDIF
              STORE 0 to m.Amount 
            ENDIF
          ENDIF
        ENDSCAN 
      ENDIF 
    ENDSCAN 
  ELSE
    SELECT INVHDR
    =gfSeek('')
    SCAN FOR Status <> "V" AND IIF(!EMPTY(CTOD(lcStartDate)) AND !EMPTY(CTOD(lcEndDate)),BETWEEN(INVHDR.INVDATE,CTOD(lcStartDate),CTOD(lcEndDate)),.T.);
             AND IIF(llUseSea,SEEK(INVHDR.Season,lcSeaFile),.T.) AND ;
             IIF(llUseRep,SEEK(INVHDR.REP1,lcRepFile),.T.) ;
             AND gfSeek('M'+Account,'Customer')
           
      m.Account   = Customer.Account
      m.Name      = Customer.btName
      m.buyer     = Customer.buyer
      m.phone     = Customer.phone1
      m.Rep     = InvHdr.Rep1 
      m.Amount  = InvHdr.TotalChg
      IF SEEK(m.Rep+m.account,RANK_FILE)
        SELECT (RANK_FILE)
        REPLACE Amount    WITH Amount    + m.Amount 
      ELSE  && Add new style record.
        IF !EMPTY(m.account) OR !EMPTY(m.Rep)
          IF m.Amount<>0 
            INSERT INTO (RANK_FILE) FROM MEMVAR
          ENDIF
          STORE 0 to m.Amount 
        ENDIF
      ENDIF
    ENDSCAN 
  ENDIF 
  *-order header
  STORE 0 to m.Amount 
    IF llUseAcc
      SELECT ORDHDR
      =gfSetorder('ORDACCT')
      SELECT(lcAccFile)  
      SCAN 
        IF gfSeek(Account+'O','Ordhdr')
          SELECT ORDHDR
          SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = &lcAccFile..Account+'O' FOR ;
               IIF(lcRpOrdSt='A',.T.,(OrdHdr.Status $ lcRpOrdSt))   AND ;
               IIF(!("X" $ lcRpOrdSt),Status<> "X",.T.) AND;
               IIF(!EMPTY(CTOD(lcStartDate)) AND !EMPTY(CTOD(lcEndDate)),BETWEEN(ORDHDR.COMPLETE,CTOD(lcStartDate),CTOD(lcEndDate)),.T.);
               AND IIF(llUseSea,SEEK(ORDHDR.Season,lcSeaFile),.T.) AND ;
               IIF(llUseRep,SEEK(ORDHDR.REP1,lcRepFile),.T.) ;
               AND gfSeek('M'+Account,'Customer')         

            m.Account   = Customer.Account
            m.Name      = Customer.btName
            m.buyer     = Customer.buyer
            m.phone     = Customer.phone1
            m.Rep       = OrdHdr.Rep1
            IF gfSEEK('O'+ORDHDR.ORDER,'ORDLINE')
            SELECT ORDLINE
            SCAN REST WHILE cOrdtype+Order = 'O'+ Ordhdr.order ;
                    FOR IIF(!EMPTY(CTOD(lcStartDate)) AND !EMPTY(CTOD(lcEndDate)),BETWEEN(ORDLINE.COMPLETE,CTOD(lcStartDate),CTOD(lcEndDate)),.T.)
                    m.Amtbook=m.Amtbook + (Ordline.TotBook*Ordline.Price)
                    IF ORDHDR.STATUS = 'O'
                       m.Amtopen=m.Amtopen + (Ordline.TotQty*Ordline.Price)
                    ENDIF
                    IF gfSEEK(Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6),'Ordcanln')
                      SELECT OrdCanln
                      SCAN REST WHILE Cordtype+ Order+STR(lineno,6)= Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6)
                        m.Amtbook=m.Amtbook - (OrdCanln.TotQty*Ordline.Price)
                      ENDSCAN
                    ENDIF
                  ENDSCAN
               ENDIF    
            
            IF SEEK(m.Rep+m.account,RANK_FILE)
              SELECT (RANK_FILE)
              REPLACE AMTBOOK   WITH AMTBOOK   + m.AMTBOOK , AMTOPEN WITH AMTOPEN + m.AMTOPEN
              STORE 0 TO m.AMTBOOK,AMTOPEN
            ELSE  && Add new style record.
              IF !EMPTY(m.account) OR !EMPTY(m.Rep)
                IF m.AMTBOOK<>0 OR m.AMTOPEN<>0
                  INSERT INTO (RANK_FILE) FROM MEMVAR
                ENDIF
                STORE 0 to m.AMTBOOK , m.AMTOPEN
              ENDIF
            ENDIF
          ENDSCAN         
        ENDIF 
      ENDSCAN 
    ELSE
      SELECT ORDHDR
      gfSetorder('ORDHDR')
      gfSeek('O')
      SCAN REST WHILE CORDTYPE+ORDER ='O' FOR ;
             IIF(lcRpOrdSt='A',.T.,(OrdHdr.Status $ lcRpOrdSt))   AND ;
             IIF(!("X" $ lcRpOrdSt),Status<> "X",.T.) AND;
             IIF(!EMPTY(CTOD(lcStartDate)) AND !EMPTY(CTOD(lcEndDate)),BETWEEN(ORDHDR.COMPLETE,CTOD(lcStartDate),CTOD(lcEndDate)),.T.);
             AND IIF(llUseSea,SEEK(ORDHDR.Season,lcSeaFile),.T.) AND ;
             IIF(llUseRep,SEEK(ORDHDR.REP1,lcRepFile),.T.) ;
             AND gfSeek('M'+Account,'Customer')         

              m.Account   = Customer.Account
              m.Name      = Customer.btName
              m.buyer     = Customer.buyer
              m.phone     = Customer.phone1
              m.Rep       = OrdHdr.Rep1

              IF gfSEEK('O'+ORDHDR.ORDER,'ORDLINE')
                 SELECT ORDLINE
                 SCAN REST WHILE cOrdtype+Order = 'O'+ Ordhdr.order ;
                    FOR IIF(!EMPTY(CTOD(lcStartDate)) AND !EMPTY(CTOD(lcEndDate)),BETWEEN(ORDLINE.COMPLETE,CTOD(lcStartDate),CTOD(lcEndDate)),.T.)
                   m.Amtbook=m.Amtbook + (Ordline.TotBook*Ordline.Price)
                    IF ORDHDR.STATUS = 'O'
                       m.Amtopen=m.Amtopen + (Ordline.TotQty*Ordline.Price)
                    ENDIF
                    IF gfSEEK(Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6),'Ordcanln')
                      SELECT OrdCanln
                      SCAN REST WHILE Cordtype+ Order+STR(lineno,6)= Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6)
                        m.Amtbook=m.Amtbook - (OrdCanln.TotQty*Ordline.Price)
                      ENDSCAN
                    ENDIF
                  ENDSCAN
               ENDIF    

          
          IF SEEK(m.Rep+m.account,RANK_FILE)
            SELECT (RANK_FILE)
            REPLACE AMTBOOK   WITH AMTBOOK   + m.AMTBOOK , AMTOPEN WITH AMTOPEN + m.AMTOPEN
            STORE 0 TO m.AMTBOOK,AMTOPEN
          ELSE  && Add new style record.
            IF !EMPTY(m.account) OR !EMPTY(m.Rep)
              IF m.AMTBOOK<>0 OR m.AMTOPEN<>0 OR m.Amount <> 0
                INSERT INTO (RANK_FILE) FROM MEMVAR
              ENDIF
              STORE 0 to m.AMTBOOK , m.AMTOPEN
            ENDIF
          ENDIF
        ENDSCAN          
    ENDIF
   ENDIF 
  ENDIF 
ENDIF 



*!*************************************************************
*! Name      : lfRankFile
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/22/2004
*! Purpose   : Create temporary cursor.
*!*************************************************************
*! Called from : Program code, OG when function.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfRankFile()
*!*************************************************************
*
FUNCTION lfRankFile
DIMENSION laTempStru[8, 4]
laTempStru[1,1] = 'Account'
laTempStru[1,2] = 'C'
laTempStru[1,3] = 6
laTempStru[1,4] = 0

laTempStru[2,1] = 'Name'
laTempStru[2,2] = 'C'
laTempStru[2,3] = 20
laTempStru[2,4] = 0

laTempStru[3,1] = 'Buyer'
laTempStru[3,2] = 'C'
laTempStru[3,3] = 15
laTempStru[3,4] = 0

laTempStru[4,1] = 'Phone'
laTempStru[4,2] = 'C'
laTempStru[4,3] = 15
laTempStru[4,4] = 0

laTempStru[5,1] = 'Rep'
laTempStru[5,2] = 'C'
laTempStru[5,3] = 3
laTempStru[5,4] = 0
 
laTempStru[6,1] = 'Amount'
laTempStru[6,2] = 'N'
laTempStru[6,3] = 14
laTempStru[6,4] = 2

laTempStru[7,1] = 'AMTOPEN'
laTempStru[7,2] = 'N'
laTempStru[7,3] = 14
laTempStru[7,4] = 2

laTempStru[8,1] = 'AMTBOOK'
laTempStru[8,2] = 'N'
laTempStru[8,3] = 14
laTempStru[8,4] = 2

 = gfCrtTmp(RANK_FILE,@laTempStru,"Rep+Account" ,RANK_FILE,.T.)

*-- end of lfRankFile.
*!**************************************************************************
*! Name      : lfSeTSRep 
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/22/2004
*! Purpose   : Go top in Sales Rep file.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSerSRep()
*!**************************************************************************
FUNCTION lfSetSRep
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
   SELECT SalesRep
   SET ORDER TO TAG SalesRep
   GO TOP
  CASE OpGrdParm = 'R'
    SELECT SalesRep 
    SET ORDER TO 
ENDCASE
*-- End of lfSetSRep.
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/22/2004
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol isReset
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
SELECT Customer
SET ORDER TO Customer
GO TOP

*-- End of lfsrAcc.
*!*************************************************************
*! Name      : lfClrRead
*! Developer : NADER NABIL (NNA)
*! Date      : 05/03/2004
*! Purpose   : Refresh the OPtion Grid if the [Rank by] Changed
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : =lfClrRead()
*!*************************************************************
*!B03793,1
FUNCTION lfClrRead
CLEARREAD()
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : NADER NABIL (NNA)
*! Date      : 06/21/2004
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
*!B03793,1
FUNCTION lfwRepWhen
*--DECLARE array to store order status and make mover
  DECLARE laRpSource[5],laRpTarget[1]
  STORE 'Open'      TO laRpSource[1]
  STORE 'Hold'      TO laRpSource[2]
  STORE 'Bid'       TO laRpSource[3]
  STORE 'Complete'  TO laRpSource[4]
  STORE 'Cancelled' TO laRpSource[5]
  lcRpStatus = 'OHBCX'

=gfOpenTable(oAriaApplication.DataDir+'INVHDR',oAriaApplication.DataDir+'INVHDR','SH')
=gfOpenTable(oAriaApplication.DataDir+'CUSTOMER',oAriaApplication.DataDir+'CUSTOMER','SH')
=gfOpenTable(oAriaApplication.DataDir+'Ordline',oAriaApplication.DataDir+'Ordline','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDHDR',oAriaApplication.DataDir+'ORDHDR','SH')
=gfOpenTable(oAriaApplication.DataDir+'OrdCanln',oAriaApplication.DataDir+'ORDCANLN','SH')

*-- END lfwRepWhen 

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : NADER NABIL (NNA)
*! Date      : 06/21/2004
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
*!B03793,1
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpOrdSt  && Save old status value.

*= lfOGMover(@laRpSource,@laRpTarget,'Style Bin',.T.,'')  && call mover function.
=lfOGMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpOrdSt = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpOrdSt = lcRpOrdSt +IIF(laRpTarget[lnI] = 'Open'     , 'O' ,;
                           IIF(laRpTarget[lnI] = 'Hold'     , 'H' ,;
                           IIF(laRpTarget[lnI] = 'Bid'      , 'B' ,;
                           IIF(laRpTarget[lnI] = 'Complete' , 'C' ,;
                           IIF(laRpTarget[lnI] = 'Cancelled', 'X' ,'')))))
  ENDFOR  && End Loop to make Status expression.
ENDIF

lcRpOrdSt = IIF(EMPTY(lcRpOrdSt),'OHB',ALLTRIM(lcRpOrdSt))

*-- Compare current selected status with old value to rise change status flag.
*-- if length of current selected status differ from previous length 

IF LEN(lcOldStat) != LEN(lcRpOrdSt) 
  llOGFltCh = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpOrdSt)
      llOGFltCh = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/22/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
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
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT) 
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


