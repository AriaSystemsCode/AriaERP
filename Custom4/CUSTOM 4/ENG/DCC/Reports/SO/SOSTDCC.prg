*:***************************************************************************
*: Program file  : SOSTDCC.PRG
*: Program desc. : Order / Packing Analysis for Direct Corporate Clothing [DIR03]
*: Date          : 03/04/2009
*: System        : A4XP.
*: Module        : Sales Order (SO)
*: Developer     : Mostafa Eid (MOS)
*: Tracking Job Number : C201122 A4xp , C201121 A27
*:**************************************************************************
*: Calls : FUNCTIONS  : 
*:**************************************************************************
*: Example : DO SOSTDCC
*:**************************************************************************
*:Modifications:
*:**************
*:B608954,1 AHS 08/13/2009  please quote for creating statistics reports for DCC in Aria4 [T20080812.0069]

**** Globals
lcSvCent = SET("Century")
SET CENTURY ON 
STORE .F. TO llDateSelect 
STORE 0 TO lnDatePos 
STORE {} to ldStartDate , ldendDate 
STORE '' TO LcCusBname , lcUserName ,SystemDate ,StartDate , EndDate , PrinUser 
PrinUser = oAriaApplication.User_Name
*! Pick Ticket date Range         
lnDatePos = ASCAN(laOGFxFlt,'PIKTKT.DATE')
lnDatePos  = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
llDateSelect  = !EMPTY(laOGFxFlt[lnDatePos ,6])
ldStartDate  = CTOD(SUBSTR(laOGFxFlt[lnDatePos ,6],1,10))
ldendDate = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],12,21))

***** Checking Entered DATE 

IF !llDateSelect
  = gfModalGen('INM00000B00000','F','ALERT', ' ',' Please Enter Date Range ')
  RETURN
ENDIF

*****Printing 

loogscroll.cCRPapersize = 'A4'
loOGScroll.cCROrientation = 'P'

***** Open Tables 

IF !USED('PIKTKT')
  =gfOpenTable(oAriaApplication.DataDir+'PIKTKT','ORDPIK','SH')
ENDIF

IF !USED('PACK_HDR')
  =gfOpenTable(oAriaApplication.DataDir+'PACK_HDR','Orderpck','SH')
ENDIF

IF !USED('ORDHDR')
  =gfOpenTable(oAriaApplication.DataDir+'ORDHDR','ORDHDR','SH')
ENDIF

IF !USED('CUSTOMER')
  =gfOpenTable(oAriaApplication.DataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF

IF !USED('ORDLINE')
  =gfOpenTable(oAriaApplication.DataDir+'ORDLINE','ORDLINE','SH')
ENDIF

IF !USED('SYUUSER')
  =gfOpenTable(oAriaApplication.DataDir+'SYUUSER','CUSER_ID','SH')
ENDIF

IF !USED('PIKLINE')
  =gfOpenTable(oAriaApplication.DataDir+'PIKLINE','PIKLINEO','SH')
ENDIF

***** Main 
DO CASE 
  
  CASE lcRpFormN = 'P'
    loOGScroll.cCROrientation = 'L'
    =lfMain() 
    SELECT (lcTemPik)
    lcRpForm  = 'SOSTPIK'
    
    *B608954,1 AHS 08/13/2009 Making relation between the temp file and piktkt [start]
    SELECT piktkt
    SET ORDER TO piktkt
    SELECT &lcTempik
    IF !'PIKTKT INTO PIKTKT' $ UPPER(SET("Relation"))
      SET RELATION TO piktkt INTO piktkt ADDITIVE
    ENDIF
    *B608954,1 AHS 08/13/2009 Making relation between the temp file and piktkt [end]    
 
  CASE lcRpFormN = 'S'
    =lfMain()
    SELECT (lcTemSO)
    lcRpForm  = 'SOSTSO'
   
  
  CASE lcRpFormN = 'A'
    =lfMain()
    SELECT (lcTemPAK)
    lcRpForm  = 'SOSTPAK'
    
ENDCASE 

 =lfRepPltFr(lcRpForm )
 =gfCrtFrm(lcRpForm ,'',llOGRefForm)

 gfDispRe (EVALUATE('lcRpForm'))
 SET CENTURY &lcSvCent

*!*************************************************************
*! Name      : lfCollect    
*: Developer : Mostafa Eid (MOS)
*: Date      : 03/04/2009
*! Purpose   : Collect Data Which Will Be Printed in Frx
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfCollect()
*!*************************************************************
FUNCTION lfCollect

DO CASE 
 CASE lcRpFormN = 'P'
    SELECT Piktkt
	SCAN FOR IIF(llDateSelect,BETWEEN(piktkt.dadd_date,ldStartDate,ldendDate),.t.) 
	  WAIT WINDOW 'Pick Tick #  '+ PIKTKT.PIKTKT +'  Date  '+ DTOC(PikTkt.date)  NOWAIT 
	  SELECT(lcTemPik) 
	  IF !SEEK(PIKTKT.PIKTKT) 
	    IF !gfseek(PIKTKT.ORDER+PIKTKT.STORE+PIKTKT.PIKTKT,'PACK_HDR','ORDERPCK')  
	      APPEND BLANK  
	      REPLACE  PikTkt      WITH PikTkt.PikTkt ;
	               user        WITH IIF(gfseek(PikTkt.Cadd_user,'syuuser','CUSER_ID'),syuuser.cusr_name,PikTkt.Cadd_user); 
	               prtflag     WITH IIF(PikTkt.prtflag = 'P','Printed','');
	               status      WITH IIF(PikTkt.status = 'O','Open',IIF(PikTkt.status ='C','Complete','Cancelled'));
	               date        with PikTkt.date  ;    
	               COUNT       WITH 1  ;
	               ispacked    with "NO"
	    
	    ELSE             
	     APPEND BLANK       
	     REPLACE  PikTkt      WITH PikTkt.PikTkt ;
	              user        WITH IIF(gfseek(PikTkt.Cadd_user,'syuuser','CUSER_ID'),syuuser.cusr_name,PikTkt.Cadd_user) ; 
	              prtflag     WITH IIF(PikTkt.prtflag = 'P','Printed','');
	              status      WITH IIF(PikTkt.status = 'O','Open',IIF(PikTkt.status ='C','Complete','Cancelled'));
	              date        with PikTkt.date  ;    
	              ispacked    with "YES" ; 
	              pickedby    with IIF(gfseek(pack_hdr.pickedby,'syuuser','CUSER_ID'),syuuser.cusr_name,pack_hdr.pickedby);
	              packedby    WITH IIF(gfseek(pack_hdr.checkedby,'syuuser','CUSER_ID'),syuuser.cusr_name,pack_hdr.checkedby);
	              checkedby   WITH IIF(gfseek(pack_hdr.cadd_user,'syuuser','CUSER_ID'),syuuser.cusr_name,pack_hdr.cadd_user);
	              COUNT       with 1  
	    ENDIF 
	  ENDIF  
	ENDSCAN
 
 CASE lcRpFormN = 'S'
 
   SELECT ORDHDR
	SCAN FOR IIF(llDateSelect,BETWEEN(ordhdr.dadd_date,ldStartDate,ldendDate),.t.)
	  WAIT WINDOW 'Sales Order #  '+ ORDHDR.ORDER +'  Date  '+ DTOC(ordhdr.dadd_date)  NOWAIT 
	  LcCusBname = IIF(gfSeek('M'+ Ordhdr.account,'customer','customer'),customer.btname,"")
	  SELECT(lcTemSO) 
	  *LOCATE   && commented by tmi 8/23/2009
	  SET ORDER TO lcTemSO 
      GO TOP   && tmi 8/23/2009
	  IF !SEEK(ORDHDR.Cadd_user+ORDHDR.ACCOUNT)
	  
	      APPEND BLANK          
	      REPLACE ORDER       WITH ORDHDR.ORDER ;
	              user        WITH ORDHDR.Cadd_user ;
	              ACCOUNT     WITH ORDHDR.ACCOUNT;
	              BName       WITH LcCusBname ; 	            
	              Value       with (ordhdr.bookamt-ordhdr.cancelamt)/IIF(ordhdr.nexrate<>0,ordhdr.nexrate,1) ;   
	              qty         with 1      
	  
	  ELSE 
	     
	     REPLACE VALUE        WITH  VALUE + (ordhdr.bookamt-ordhdr.cancelamt)/IIF(ordhdr.nexrate<>0,ordhdr.nexrate,1) ;          
	             QTY          WITH  QTY+ 1 	  
	  ENDIF  
	ENDSCAN
 
 
 CASE lcRpFormN = 'A'

    SELECT Pack_hdr
	gfSetorder('PACK_HDR')
	SCAN FOR IIF(llDateSelect,BETWEEN(Pack_hdr.dadd_date,ldStartDate,ldendDate),.t.) 
	  WAIT WINDOW 'Pack List #  '+ pack_hdr.pack_no + '  Date  '+ DTOC(Pack_hdr.dadd_date)  NOWAIT 
	  STORE 0 TO lnTotnpk , lnValue
      SELECT ORDLINE   
	  IF gfseek('O'+ Pack_hdr.ORDER ,'ORDLINE','ORDLINE')
	    SCAN REST WHILE 'O'+ ORDLINE.ORDER = 'O'+ Pack_hdr.ORDER  
	      IF (ORDLINE.PIKTKT = PACK_HDR.PACK_NO)
             lnTotnpk = lnTotnpk +(npck1+npck2+npck3+npck4+npck5+npck6+npck7+npck8) 
	         lnValue  = lnValue +(npck1+npck2+npck3+npck4+npck5+npck6+npck7+npck8)* Price
	      ELSE 
	         SELECT PIKLINE   
	         IF gfseek(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'PIKLINE','PIKLINEO')
		       SCAN REST WHILE ORDER+STR(LINENO,6) = ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
		         IF (PIKLINE.PIKTKT = PACK_HDR.PACK_NO)
		           lnValue =  lnValue + (PIKLINE.totpik* PIKLINE.price)
		         ENDIF   
		       ENDSCAN  
	         ENDIF 
	      ENDIF   	      
  	  ENDSCAN   
	  ENDIF   
      
      SELECT(lcTemPAK) 
      IF !SEEK(Pack_hdr.account+pack_hdr.pack_no)
        LcCusBname= IIF(gfSeek('S'+ PACK_HDR.account,'customer','customer'),customer.btname,"")
        *B608954 AHS 07/30/2009, Solving problem of missing customer names [T20080812.0069 ][Start]
        IF EMPTY(LcCusBname)
          LcCusBname= IIF(gfSeek('M'+ PACK_HDR.account,'customer','customer'),customer.btname,"")
        ENDIF
        *B608954 AHS 07/30/2009 [End]
        
        APPEND BLANK          
        REPLACE PackNO    WITH PACK_HDR.PACK_NO ;
                ACCOUNT   WITH PACK_HDR.ACCOUNT;
                BName     WITH LcCusBname  ; 	            
                Value     with lnValue ;  
                qty       with 1
      
      ELSE 
        REPLACE Value     with lnValue + lnValue  ;  
                qty       WITH  qty + 1
      
      ENDIF   
    ENDSCAN 	
ENDCASE 

*!*************************************************************
*! Name      : lfCreaTemp  
*: Developer : Mostafa Eid(mos)
*: Date      : 03/04/2009
*! Purpose   : Create Temp file to collect data in
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfCreaTemp()
*!*************************************************************
FUNCTION lfCreaTemp  
DO CASE 
  CASE lcRpFormN = 'P'

	*-- check If File is created or not
	IF USED(lcTemPik) AND RECCOUNT(lcTemPik) > 0
	 USE IN (lcTemPik)
	ENDIF

	*-- Create File
	IF !USED(lcTemPik)
	  
	  lnI = 1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'PikTkt'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 6
	  laTempStru[lnI,4] = 0
	  
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'user'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 35
	  laTempStru[lnI,4] = 0
	    
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'prtflag'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 1
	  laTempStru[lnI,4] = 0
	  
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'date'
	  laTempStru[lnI,2] = 'D'
	  laTempStru[lnI,3] = 8
	  laTempStru[lnI,4] = 0

	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'STATUS'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 10
	  laTempStru[lnI,4] = 0
	  
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'IsPacked'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 3
	  laTempStru[lnI,4] = 0
	  
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'Pickedby'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 20
	  laTempStru[lnI,4] = 0

	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'Packedby'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 20
	  laTempStru[lnI,4] = 0

	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'checkedby'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 20
	  laTempStru[lnI,4] = 0 
     
      lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'count'
	  laTempStru[lnI,2] = 'N'
	  laTempStru[lnI,3] = 1
	  laTempStru[lnI,4] = 0 

	 =gfCrtTmp(lcTemPik,@laTempStru,'user')
	ENDIF  

  CASE lcRpFormN = 'S'
	
	*-- check If File is created or not
	IF USED(lcTemSO) AND RECCOUNT(lcTemSO) > 0
	 USE IN (lcTemSO)
	ENDIF

	*-- Create File
	IF !USED(lcTemSO)
	  
	  lnI = 1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'USER'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 10
	  laTempStru[lnI,4] = 0
	 
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'ORDER'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 6
	  laTempStru[lnI,4] = 0
	    
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'ACCOUNT'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 5
	  laTempStru[lnI,4] = 0
	  
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'BName'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 30
	  laTempStru[lnI,4] = 0

	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'Qty'
	  laTempStru[lnI,2] = 'N'
	  laTempStru[lnI,3] = 10
	  laTempStru[lnI,4] = 0
	  
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'Value'
	  laTempStru[lnI,2] = 'N'
	  laTempStru[lnI,3] = 15
	  laTempStru[lnI,4] = 2
	  =gfCrtTmp(lcTemSO,@laTempStru)
	  SELECT (lcTemSO)
	  INDEX ON USER+ACCOUNT+BName+ORDER TAG lcTemSO
	
	ENDIF  

  CASE lcRpFormN = 'A'

	*-- check If File is created or not
	IF USED(lcTemPAK) AND RECCOUNT(lcTemPAK) > 0
	 USE IN (lcTemPAK)
	ENDIF

	*-- Create File
	IF !USED(lcTemPAK)
	  
	  lnI = 1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'ACCOUNT'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 5
	  laTempStru[lnI,4] = 0
	 
	  
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'PackNo'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 6
	  laTempStru[lnI,4] = 0
	  
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'BName'
	  laTempStru[lnI,2] = 'C'
	  laTempStru[lnI,3] = 30
	  laTempStru[lnI,4] = 0
	  
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'Qty'
	  laTempStru[lnI,2] = 'N'
	  laTempStru[lnI,3] = 10
	  laTempStru[lnI,4] = 0
	  
	  lnI = ALEN(laTempStru,1)+1
	  DIMENSION laTempStru[lnI,4]
	  laTempStru[lnI,1] = 'Value'
	  laTempStru[lnI,2] = 'N'
	  laTempStru[lnI,3] = 15
	  laTempStru[lnI,4] = 2
	  =gfCrtTmp(lcTemPAK,@laTempStru,'ACCOUNT+PACKNO')
  ENDIF  
ENDCASE 

*!*************************************************************
*! Name      : MAIN   
*: Developer : Mostafa Eid (MOS)
*: Date      : 03/04/2009
*! Purpose   : run create temp and collect data
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfCollect()
*!*************************************************************
FUNCTION lfMain 

IF loOGScroll.llOGFltCh
  =lfCreaTemp() 
  =lfCollect() 
ENDIF 
