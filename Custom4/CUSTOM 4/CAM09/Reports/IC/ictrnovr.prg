*:***************************************************************************
*: Program file  : ICTRNOVR
*: Program desc. : Inventory Turn Over Report
*: For Report    : ICTRNOVR.FRX
*: System        : Aria Advantage Series ARIA4XP  
*: Module        : Inventory Control (IC)
*: Developer     : Mariam Mazhar (MMT)(C126945) 03/30/2006
*:***************************************************************************

IF loOGScroll.llOGFltCh
  llNodata = .F. && flag to check if there any data satisfies the selection criteria
  =lfCrtTempTable()
  =lfCollectingData()
  SELECT(lcStyTmp)
  IF RECCOUNT(lcStyTmp) > 0 
    llNodata = .T. 
    IF lcSortBy = 'I'
	  SELECT(lcStyTmp)
	  SET ORDER TO (lcStyTmp)
	ELSE
	  IF lcSortBy = 'V'
        SET ORDER TO 'CVENDIND'
      ELSE
        SET ORDER TO 'CstyGrpInd'
      ENDIF  
    ENDIF   

  ELSE
    llNodata = .F. 
  ENDIF 
ENDIF 

IF !llNodata
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  Return
ENDIF 
loogScroll.cCROrientation = 'P'
SELECT(lcStyTmp)
DO gfDispRe WITH EVAL('lcRpForm')
*!*************************************************************
*! Name      : lfCollectingData
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/30/2006
*! Purpose   : Collect data for report
*!*************************************************************
*!
FUNCTION lfCollectingData

*= gfOpenFile(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
STORE .F. TO llStyleSelect,llVendSelect,llGrpSelect

lnStyPos = ASCAN(loOGScroll.laogFxflt,'STYLE.STYLE')
IF lnStyPos  <> 0 
  lnStyPos     = ASUBSCRIPT(loOGScroll.laogFxflt,lnStyPos ,1)
  lcStyFile    = loOGScroll.laogFxflt[lnStyPos ,6]
  IF !EMPTY(lcStyFile) AND USED(lcStyFile) AND RECCOUNT(lcStyFile) > 0
    llStyleSelect  = .T.
  ENDIF  
ENDIF 


lnVenPos = ASCAN(loOGScroll.laogFxflt,'APVendor.CVENDCODE')
IF lnVenPos <> 0 
  lnVenPos    = ASUBSCRIPT(loOGScroll.laogFxflt,lnVenPos,1)
  lcVenFile    = loOGScroll.laogFxflt[lnVenPos ,6]
  IF !EMPTY(lcVenFile) AND USED(lcVenFile) AND RECCOUNT(lcVenFile) > 0
    llVendSelect  = .T.
  ENDIF  
ENDIF 


lnGrpPos = ASCAN(loOGScroll.laogFxflt,'STYLE.CSTYGROUP')
IF lnGrpPos <> 0 
  lnGrpPos   = ASUBSCRIPT(loOGScroll.laogFxflt,lnGrpPos ,1)
  lcGrpString    = loOGScroll.laogFxflt[lnGrpPos ,6]
  IF !EMPTY(lcGrpString) 
    lcTempGrp = loOgScroll.gfTempName()
  ENDIF   
  IF !EMPTY(lcGrpString) AND  lfConvertToCursor(lcGrpString,lcTempGrp)
    llGrpSelect  = .T.
  ENDIF  
ENDIF 


DO CASE 
  CASE llStyleSelect  
    SELECT(lcStyFile)
    SCAN 
      IF SEEK(&lcStyFile..Style,'Style') AND IIF(llVendSelect,SEEK(Style.Vendor,lcVenFile),.T.) AND ;
         IIF(llGrpSelect , SEEK(STYLE.CSTYGROUP,lcTempGrp),.T.) AND IIF(llRpPrdType <> 'A',IIF(llRpPrdType = 'D',STYLE.Make,!STYLE.MAKE),.T.)
 		  lcDesc   = style.desc1
 		  lcStyGrp = style.Cstygroup        
		  lcVendor = Style.Vendor 		  
		  llMake = style.make
          IF loDBFStyDye.SEEK(Style.style,'Stydye')
            SELECT('Stydye')
            SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.style FOR EMPTY(Stydye.dyelot)
              SCATTER MEMO MEMVAR 
              SELECT(lcStyTmp)
	 		  m.Desc1   = lcDesc 
	 		  m.cStyGroup =lcStyGrp
			  m.Vendor = lcVendor
			  m.Make = llMake
 			  m.IssueQyYtd = 0
 			  m.InvcdQyYtd = 0
 			  lnTotPrStk = 0
			  m.AvergaInv = 0
 			  IF loDBFPosln.SQlrun("SELECT Posln.Style,Posln.Cbusdocu,Posln.cstytype,Posln.cinvtype,Posln.Po,Posln.Trancd,Posln.Cwarecode FROM Posln(Index = poslns) INNER JOIN  POSHDR ON "+;
 			  						"POSHDR.CSTYTYPE  = POSLN.CSTYTYPE  AND POSHDR.CBUSDOCU = POSLN.CBUSDOCU "+;
 			  						" AND POSHDR.PO = POSLN.PO WHERE posln.Cinvtype = '0001' AND Posln.cstytype = 'P'"+;
 			  						" AND posln.cbusdocu = 'P' AND posln.Style ='" +  &lcStyFile..STYLE +"' AND posln.CWARECODE = '" +;
 			  						 Stydye.CWARECODE+"' AND posln.Trancd = '1' and Poshdr.Start Between '"+DTOC(DATE(YEAR(date()),1,1))+"' AND '"+DTOC(DATE())+"'",'POSLN')
              *IF loDBFPosln.Seek('0001'+MakeStyles.STYLE+'P'+'P','poslns')
                SELECT('POSLN')
                SCAN 
                *REST WHILE cinvtype+style+cbusdocu+cstytype+po+lineno+trancd = '0001'+MakeStyles.STYLE+'P'+'P';
                  FOR trancd  = '1' AND CWARECODE = Stydye.CWARECODE  AND loDBFPoshdr.Seek('P'+'P'+Posln.po,'POSHDR') AND BETWEEN(POshdr.Start,DATE(YEAR(date()),1,1),DATE())
                  m.IssueQyYtd = m.IssueQyYtd + posln.TOTQTY
			    ENDSCAN 
		      ENDIF 
		      IF loDBFInvline.Seek(&lcStyFile..STYLE,'INVLINES')
		        SELECT('Invline')
		        SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) = &lcStyFile..STYLE FOR cwarecode = stydye.cwarecode  AND ;
		           loDBFInvhdr.SEEK(INVLINE.INVOICE,'INVHDR') AND BETWEEN(invhdr.shipdate,DATE(YEAR(date()),1,1),DATE())
		           m.InvcdQyYtd = m.InvcdQyYtd + INVLINE.TOTQTY
		        ENDSCAN 
 			  ENDIF 	
  			  IF loDBFStyInvJl.Seek(&lcStyFile..STYLE,'STYINVJL')			 
 			    SELECT('STYINVJL')
 			    lnCurrMonth = MONTH(DATE())
 			    FOR i = 1 TO lnCurrMonth -1 
 	              SCAN REST WHILE  STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = &lcStyFile..STYLE + STYDYE.CWARECODE FOR ;
			         INLIST(CIRTYPE,'I','R') AND BETWEEN(dtrdate,DATE(YEAR(DATE()),i,1),DATE(YEAR(DATE()),i,VAL(lfGetLastDay(i))))
			        IF STYINVJL.CIRTYPE = "R"
				      lnTotPrStk =  lnTotPrStk + Styinvjl.NTotstk
			        ELSE
  	                  lnTotPrStk =  lnTotPrStk - ABS(Styinvjl.NTotstk)
			        ENDIF       
   			      ENDSCAN 
 			    ENDFOR 
 			    m.AvergaInv = lnTotPrStk + STYDYE.TOTSTK / MONTH(DATE())
 			  ENDIF 
	
			  SELECT(lcStyTmp)
              APPEND BLANK 
              GATHER MEMO MEMVAR
            ENDSCAN   
          ENDIF 
          
      ENDIF 
    ENDSCAN 
  CASE llVendSelect  
    SELECT(lcVenFile)
    *SELECT STYLE FROM STYLE INNER JOIN  &lcVenFile ON STYLE.vendor = &lcVenFile..Cvendcode WHERE  IIF(llGrpSelect,(SEEK(STYLE.CSTYGROUP,lcTempGrp)),.T.) ;
      AND IIF(llRpPrdType = 'D',STYLE.Make,!STYLE.MAKE) INTO CURSOR 'VendStyles'
    LOCAL lcStat
    
    lcStat = "SELECT STYLE FROM STYLE INNER JOIN " + lcVenFile + " ON STYLE.vendor = " + lcVenFile + ".Cvendcode WHERE " +;
             IIF(llGrpSelect, "SEEK(STYLE.CSTYGROUP,lcTempGrp)" , ".T.") +;
             " AND IIF(llRpPrdType <> 'A',IIF(llRpPrdType = 'D',STYLE.Make,!STYLE.MAKE),.T.) INTO CURSOR 'VendStyles'"
    &lcStat
      SELECT('VendStyles')
      SCAN
        IF SEEK(VendStyles.Style,'Style') 
 		  lcDesc   = style.desc1
 		  lcStyGrp = style.Cstygroup        
		  lcVendor = Style.Vendor 		  
		  llMake = style.make
          IF loDBFStyDye.SEEK(Style.style,'Stydye')
            SELECT('Stydye')
            SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.style FOR EMPTY(Stydye.dyelot)
              SCATTER MEMO MEMVAR 
              SELECT(lcStyTmp)
	 		  m.Desc1   = lcDesc 
	 		  m.cStyGroup =lcStyGrp
			  m.Vendor = lcVendor
			  m.Make = llMake
 			  m.IssueQyYtd = 0
 			  m.InvcdQyYtd = 0
 			  lnTotPrStk = 0
			  m.AvergaInv = 0
			  IF loDBFPosln.SQlrun("SELECT Posln.Style,Posln.Cbusdocu,Posln.cstytype,Posln.cinvtype,Posln.Po,Posln.Trancd,Posln.Cwarecode FROM Posln(Index = poslns) INNER JOIN  POSHDR ON "+;
 			  						"POSHDR.CSTYTYPE  = POSLN.CSTYTYPE  AND POSHDR.CBUSDOCU = POSLN.CBUSDOCU "+;
 			  						" AND POSHDR.PO = POSLN.PO WHERE posln.Cinvtype = '0001' AND Posln.cstytype = 'P'"+;
 			  						" AND posln.cbusdocu = 'P' AND posln.Style ='" +  VendStyles.STYLE +"' AND posln.CWARECODE = '" +;
 			  						 Stydye.CWARECODE+"' AND posln.Trancd = '1' and Poshdr.Start Between '"+DTOC(DATE(YEAR(date()),1,1))+"' AND '"+DTOC(DATE())+"'",'POSLN')

              *IF loDBFPosln.Seek('0001'+MakeStyles.STYLE+'P'+'P','poslns')
                SELECT('POSLN')
                SCAN 
                *REST WHILE cinvtype+style+cbusdocu+cstytype+po+lineno+trancd = '0001'+MakeStyles.STYLE+'P'+'P';
                  FOR trancd  = '1' AND CWARECODE = Stydye.CWARECODE  AND loDBFPoshdr.Seek('P'+'P'+Posln.po,'POSHDR') AND BETWEEN(POshdr.Start,DATE(YEAR(date()),1,1),DATE())
                  m.IssueQyYtd = m.IssueQyYtd + posln.TOTQTY
			    ENDSCAN 
		      ENDIF 
		      IF loDBFInvline.Seek(VendStyles.STYLE,'INVLINES')
		        SELECT('Invline')
		        SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) = VendStyles.STYLE FOR cwarecode = stydye.cwarecode  AND ;
		           loDBFInvhdr.SEEK(INVLINE.INVOICE,'INVHDR') AND BETWEEN(invhdr.shipdate,DATE(YEAR(date()),1,1),DATE())
		           m.InvcdQyYtd = m.InvcdQyYtd + INVLINE.TOTQTY
		        ENDSCAN 
 			  ENDIF 						 
 			  IF loDBFStyInvJl.Seek(VendStyles.STYLE,'STYINVJL')			 
 			    SELECT('STYINVJL')
 			    lnCurrMonth = MONTH(DATE())
 			    FOR i = 1 TO lnCurrMonth -1 
 	              SCAN REST WHILE  STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = VendStyles.STYLE + STYDYE.CWARECODE FOR ;
			         INLIST(CIRTYPE,'I','R') AND BETWEEN(dtrdate,DATE(YEAR(DATE()),i,1),DATE(YEAR(DATE()),i,VAL(lfGetLastDay(i))))
			        IF STYINVJL.CIRTYPE = "R"
				      lnTotPrStk =  lnTotPrStk + Styinvjl.NTotstk
			        ELSE
  	                  lnTotPrStk =  lnTotPrStk - ABS(Styinvjl.NTotstk)
			        ENDIF       
   			      ENDSCAN 
 			    ENDFOR 
 			    m.AvergaInv = lnTotPrStk + STYDYE.TOTSTK / MONTH(DATE())
 			  ENDIF 

			  SELECT(lcStyTmp)
              APPEND BLANK 
              GATHER MEMO MEMVAR
            ENDSCAN   
          ENDIF 
        ENDIF  
      ENDSCAN 
  CASE llGrpSelect  
    SELECT(lcTempGrp)
     SELECT STYLE FROM STYLE INNER JOIN  &lcTempGrp ON STYLE.CSTYGROUP = &lcTempGrp..CSTYGROUP WHERE  IIF(llRpPrdType <> 'A',IIF(llRpPrdType = 'D',STYLE.Make,!STYLE.MAKE),.T.) INTO CURSOR 'GrpStyles'
      SELECT('GrpStyles')
      SCAN 
        IF SEEK(GrpStyles.Style,'Style')
 		  lcDesc   = style.desc1
 		  lcStyGrp = style.Cstygroup        
		  lcVendor = Style.Vendor 		  
		  llMake = style.make
          IF loDBFStyDye.SEEK(Style.style,'Stydye')
            SELECT('Stydye')
            SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.style FOR EMPTY(Stydye.dyelot)
              SCATTER MEMO MEMVAR 
              SELECT(lcStyTmp)
	 		  m.Desc1   = lcDesc 
	 		  m.cStyGroup =lcStyGrp
			  m.Vendor = lcVendor
			  m.Make = llMake
			  m.IssueQyYtd = 0
			  m.InvcdQyYtd = 0
			  lnTotPrStk   = 0
			  m.AvergaInv  = 0
 			  IF loDBFPosln.SQlrun("SELECT Posln.Style,Posln.Cbusdocu,Posln.cstytype,Posln.cinvtype,Posln.Po,Posln.Trancd,Posln.Cwarecode FROM Posln(Index = poslns) INNER JOIN  POSHDR ON "+;
 			  						"POSHDR.CSTYTYPE  = POSLN.CSTYTYPE  AND POSHDR.CBUSDOCU = POSLN.CBUSDOCU "+;
 			  						" AND POSHDR.PO = POSLN.PO WHERE posln.Cinvtype = '0001' AND Posln.cstytype = 'P'"+;
 			  						" AND posln.cbusdocu = 'P' AND posln.Style ='" +  GrpStyles.STYLE +"' AND posln.CWARECODE = '" +;
 			  						 Stydye.CWARECODE+"' AND posln.Trancd = '1' and Poshdr.Start Between '"+DTOC(DATE(YEAR(date()),1,1))+"' AND '"+DTOC(DATE())+"'",'POSLN')

              *IF loDBFPosln.Seek('0001'+MakeStyles.STYLE+'P'+'P','poslns')
                SELECT('POSLN')
                SCAN 
                *REST WHILE cinvtype+style+cbusdocu+cstytype+po+lineno+trancd = '0001'+MakeStyles.STYLE+'P'+'P';
                  FOR trancd  = '1' AND CWARECODE = Stydye.CWARECODE  AND loDBFPoshdr.Seek('P'+'P'+Posln.po,'POSHDR') AND BETWEEN(POshdr.Start,DATE(YEAR(date()),1,1),DATE())
                  m.IssueQyYtd = m.IssueQyYtd + posln.TOTQTY
			    ENDSCAN 
		    
		      ENDIF 
  		      IF loDBFInvline.Seek(GrpStyles.STYLE,'INVLINES')
		        SELECT('Invline')
		        SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) = GrpStyles.STYLE FOR cwarecode = stydye.cwarecode  AND ;
		           loDBFInvhdr.SEEK(INVLINE.INVOICE,'INVHDR') AND BETWEEN(invhdr.shipdate,DATE(YEAR(date()),1,1),DATE())
		           m.InvcdQyYtd = m.InvcdQyYtd + INVLINE.TOTQTY
		        ENDSCAN 
 			  ENDIF 						 
 			  IF loDBFStyInvJl.Seek(GrpStyles.STYLE,'STYINVJL')			 
 			    SELECT('STYINVJL')
 			    lnCurrMonth = MONTH(DATE())
 			    FOR i = 1 TO lnCurrMonth -1 
 	              SCAN REST WHILE  STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) =GrpStyles.STYLE+ STYDYE.CWARECODE FOR ;
			         INLIST(CIRTYPE,'I','R') AND BETWEEN(dtrdate,DATE(YEAR(DATE()),i,1),DATE(YEAR(DATE()),i,VAL(lfGetLastDay(i))))
			        IF STYINVJL.CIRTYPE = "R"
				      lnTotPrStk =  lnTotPrStk + Styinvjl.NTotstk
			        ELSE
  	                  lnTotPrStk =  lnTotPrStk - ABS(Styinvjl.NTotstk)
			        ENDIF       
   			      ENDSCAN 
 			    ENDFOR 
 			    m.AvergaInv = lnTotPrStk + STYDYE.TOTSTK / MONTH(DATE())
 			  ENDIF 

			  SELECT(lcStyTmp)
              APPEND BLANK 
              GATHER MEMO MEMVAR
            ENDSCAN   
          ENDIF 
        ENDIF   
      ENDSCAN 
    OTHERWISE 
    SELECT STYLE  FROM STYLE WHERE IIF(llRpPrdType <> 'A',IIF(llRpPrdType = 'D',STYLE.Make,!STYLE.MAKE),.T.) INTO CURSOR 'MakeStyles'
    SELECT('MakeStyles')
    SCAN 
      IF SEEK(MakeStyles.STYLE,'Style')
 		  lcDesc   = style.desc1
 		  lcStyGrp = style.Cstygroup        
		  lcVendor = Style.Vendor 		  
		  llMake = style.make
          IF loDBFStyDye.SEEK(Style.style,'Stydye')
            SELECT('Stydye')
            SCAN REST WHILE STYLE+CWARECODE+DYELOT = Style.style FOR EMPTY(Stydye.dyelot)
              SCATTER MEMO MEMVAR 
              SELECT(lcStyTmp)
	 		  m.Desc1   = lcDesc 
	 		  m.cStyGroup =lcStyGrp
			  m.Vendor = lcVendor
			  m.Make = llMake
			  m.IssueQyYtd = 0
			  m.InvcdQyYtd = 0
			  lnTotPrStk = 0
			  m.AvergaInv = 0
  			  IF loDBFPosln.SQlrun("SELECT Posln.Style,Posln.Cbusdocu,Posln.cstytype,Posln.cinvtype,Posln.Po,Posln.Trancd,Posln.Cwarecode FROM Posln(Index = poslns) INNER JOIN  POSHDR ON "+;
 			  						"POSHDR.CSTYTYPE  = POSLN.CSTYTYPE  AND POSHDR.CBUSDOCU = POSLN.CBUSDOCU "+;
 			  						" AND POSHDR.PO = POSLN.PO WHERE posln.Cinvtype = '0001' AND Posln.cstytype = 'P'"+;
 			  						" AND posln.cbusdocu = 'P' AND posln.Style ='" +  MakeStyles.STYLE +"' AND posln.CWARECODE = '" +;
 			  						 Stydye.CWARECODE+"' AND posln.Trancd = '1' and Poshdr.Start Between '"+DTOC(DATE(YEAR(date()),1,1))+"' AND '"+DTOC(DATE())+"'",'POSLN')

              *IF loDBFPosln.Seek('0001'+MakeStyles.STYLE+'P'+'P','poslns')
                SELECT('POSLN')
                SCAN 
                *REST WHILE cinvtype+style+cbusdocu+cstytype+po+lineno+trancd = '0001'+MakeStyles.STYLE+'P'+'P';
                  FOR trancd  = '1' AND CWARECODE = Stydye.CWARECODE  AND loDBFPoshdr.Seek('P'+'P'+Posln.po,'POSHDR') AND BETWEEN(POshdr.Start,DATE(YEAR(date()),1,1),DATE())
                  m.IssueQyYtd = m.IssueQyYtd + posln.TOTQTY
			    ENDSCAN 
		      ENDIF 
		      IF loDBFInvline.Seek( MakeStyles.STYLE,'INVLINES')
		        SELECT('Invline')
		        SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) = MakeStyles.STYLE FOR cwarecode = stydye.cwarecode  AND ;
		           loDBFInvhdr.SEEK(INVLINE.INVOICE,'INVHDR') AND BETWEEN(invhdr.shipdate,DATE(YEAR(date()),1,1),DATE())
		           m.InvcdQyYtd = m.InvcdQyYtd + INVLINE.TOTQTY
		        ENDSCAN 
 			  ENDIF 			
 			  IF loDBFStyInvJl.Seek(MakeStyles.STYLE,'STYINVJL')			 
 			    SELECT('STYINVJL')
 			    lnCurrMonth = MONTH(DATE())
 			    FOR i = 1 TO lnCurrMonth - 1
 	              SCAN REST WHILE  STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = MakeStyles.STYLE + STYDYE.CWARECODE FOR ;
			         INLIST(CIRTYPE,'I','R') AND BETWEEN(dtrdate,DATE(YEAR(DATE()),i,1),DATE(YEAR(DATE()),i,VAL(lfGetLastDay(i))))
			        IF STYINVJL.CIRTYPE = "R"
				      lnTotPrStk =  lnTotPrStk + Styinvjl.NTotstk
			        ELSE
  	                  lnTotPrStk =  lnTotPrStk - ABS(Styinvjl.NTotstk)
			        ENDIF       
   			      ENDSCAN 
 			    ENDFOR 
 			    m.AvergaInv = lnTotPrStk + STYDYE.TOTSTK / MONTH(DATE())
 			  ENDIF 
		      SELECT(lcStyTmp)
              APPEND BLANK 
              GATHER MEMO MEMVAR
            ENDSCAN   
          ENDIF 
*!*	          IF loDBFPosln.Seek('0001'+MakeStyles.STYLE+'P'+'P','poslns')
*!*	             SELECT('POSLN')
*!*	             SCAN REST WHILE cinvtype+style+cbusdocu+cstytype+po+lineno+trancd = '0001'+MakeStyles.STYLE+'P'+'P';
*!*	               FOR trancd  = '1' AND CWARECODE = Stydye.CWARECODE 
*!*				 ENDSCAN 
*!*	          ENDIF 
      ENDIF 
    ENDSCAN 
ENDCASE 


*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/05/2006
*! Purpose   : Convert a list of values into a cursor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcString,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1]='CSTYGROUP' 
laTempacstru[1,2]='C'
laTempacstru[1,3]= 6 
laTempacstru[1,4]= 0
  
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,'CSTYGROUP',lcCursorTemp ,.T.)

lcValuesToConvert = lcString
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp) 
    APPEND BLANK 
    REPLACE CSTYGROUP WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE CSTYGROUP  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/30/2006
*! Purpose   : Create temp file to print from it
*!*************************************************************
*!
FUNCTION lfCrtTempTable

DIMENSION laTempacstru[10,4]

laTempacstru[1,1]='STYLE' 
laTempacstru[1,2]='C'
laTempacstru[1,3]= 19 
laTempacstru[1,4]= 0

laTempacstru[2,1]='Vendor' 
laTempacstru[2,2]='C'
laTempacstru[2,3]= 8 
laTempacstru[2,4]= 0


laTempacstru[3,1]='CSTYGROUP' 
laTempacstru[3,2]='C'
laTempacstru[3,3]= 6 
laTempacstru[3,4]= 0

laTempacstru[4,1]='Make' 
laTempacstru[4,2]='L'
laTempacstru[4,3]= 1 
laTempacstru[4,4]= 0


laTempacstru[5,1]='Desc1' 
laTempacstru[5,2]='C'
laTempacstru[5,3]= 60 
laTempacstru[5,4]= 0

laTempacstru[6,1]='cwarecode' 
laTempacstru[6,2]='C'
laTempacstru[6,3]= 6
laTempacstru[6,4]= 0

laTempacstru[7,1]='Totstk' 
laTempacstru[7,2]='N'
laTempacstru[7,3]= 7
laTempacstru[7,4]= 0

laTempacstru[8,1]='IssueQyYtd' 
laTempacstru[8,2]='N'
laTempacstru[8,3]= 10
laTempacstru[8,4]= 0

laTempacstru[9,1]='InvcdQyYtd' 
laTempacstru[9,2]='N'
laTempacstru[9,3]= 10
laTempacstru[9,4]= 0

laTempacstru[10,1]='AvergaInv' 
laTempacstru[10,2]='N'
laTempacstru[10,3]= 10
laTempacstru[10,4]= 0



 = gfCrtTmp(lcStyTmp,@laTempacstru,'Style',lcStyTmp,.T.)
 =CURSORSETPROP("Buffering",3,lcStyTmp)
 SELECT(lcStyTmp)
 INDEX on  Vendor TAG CVENDIND
 INDEX on  cstygroup TAG CstyGrpInd
*!*************************************************************
*! Name      : lfRepWhen
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/30/2006
*! Purpose   : Create temp file to print from it
*!*************************************************************
*!
FUNCTION lfRepWhen

loDBFStyInvJl = CreateObject("RemoteTable","STYINVJL","STYINVJL","STYINVJL",SET("DATASESSION"),,.T.)

loDBFPoshdr   = CreateObject("RemoteTable","Poshdr","Poshdr","Poshdr",SET("DATASESSION"),,.T.)
loDBFPosln    = CreateObject("RemoteTable","Posln","POSLNS","Posln",SET("DATASESSION"),,.T.)

loDBFInvhdr   = CreateObject("RemoteTable","Invhdr","Invhdr","Invhdr",SET("DATASESSION"),,.T.)
loDBFInvline  = CreateObject("RemoteTable","Invline","INVLINES","Invline",SET("DATASESSION"),,.T.)

loDBFStyDye = CreateObject("RemoteTable","StyDye","StyDye","StyDye",SET("DATASESSION"),,.T.)


*************************************************************
*! Name      : lfGetLastDay
*! Developer : Mariam Mazhar(MMT)
*! Date      : 01/23/2006
*! Purpose   : get the last day of month
*!*************************************************************
FUNCTION lfGetLastDay
PARAMETERS lnMonth
DO CASE 
  Case INLIST(lnMonth,1, 3, 5, 7, 8, 10, 12)
    lnLastDayNo  = "31"

  Case INLIST(lnMonth,4, 6, 9, 11)
    lnLastDayNo = "30"

  Case lnMonth= 2 
    If MOD(Year(DATE()),4) = 0 
      lnLastDayNo = "29"
    Else
      lnLastDayNo = "28"
    ENDIF 
ENDCASE 
RETURN lnLastDayNo 