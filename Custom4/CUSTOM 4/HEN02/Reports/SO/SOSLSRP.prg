*:**************************************************************************
*: Program file  : SOSLSRP
*: Program desc. : Custom SALES REPORT BY SALESMAN For VISION (VIS20)
*: System        : ARIA 4XP.
*: Module        : Sales Order (SO)
*: Developer     : Mariam Mazhar (MMT)
*: Date          : 06/29/2009
*: Reference     : C201177[T20090427.0014]
*:**************************************************************************
*: B609078,1 MMT 11/09/2009 Fix bug of Collecting from Ordhdr not invhdr[T20090427.0014]
*:**************************************************************************
IF !USED('FsPrd')
  =gfOpenTable('FsPrd',"COMFYRPRDI")
ENDIF 

lcMonFile = ""
llUseMon = .F.
lnPos = ASCAN(loOGScroll.laogFxFlt,'ORDLINE.START')
IF lnPos <> 0 
 lnPos = ASUBSCRIPT(loOGScroll.laogFxFlt,lnPos,1)
 lcMonFile = loOGScroll.laogFxFlt[lnPos,6]
 IF !EMPTY(lcMonFile ) AND USED(lcMonFile )
   SELECT(lcMonFile )
   LOCATE 
   IF !EOF()
     llUseMon = .T.
   ENDIF 
 ENDIF 
ENDIF  

ldFromDate  = {}
lDToDate  = {}
IF llUseMon 
  SELECT(lcMonFile)
  LOCATE 
  lcPerFrom = STRTRAN(&lcMonFile..Keyexp ,"-","")   && Remove the hyphen "-"
  GOTO bottom
  lcPerTo   = STRTRAN(&lcMonFile..Keyexp ,"-","")  && Remove the hyphen "-"  
  ldFromDate = IIF(gfSEEK(lcPerFrom,'FsPrd'),FsPrd.dFsppBgDt,{})
  lDToDate = IIF(gfSEEK(lcPerTo,'FsPrd'),FsPrd.dFsppEnDt,{})
ENDIF 

IF !llUseMon OR EMPTY(ldFromDate) OR EmPty(lDToDate)
  = gfModalGen('INM00000B00000','F','ALERT', ' ','You have to Select Date Value')
  RETURN
ENDIF 


IF loOgScroll.llOGFltCh && OG Filters changed
  lfCreatTemp()
  lfCollect()
ELSE
 IF FILE(oAriaApplication.WorkDir+lcTempFile+'.DBF')
   USE (oAriaApplication.WorkDir+lcTempFile+'.DBF') IN 0 
 ENDIF 
ENDIF   


DIMENSION loOGScroll.laCRParams[2,2]
loOGScroll.laCRParams[1,1] = 'ReportName'
loOGScroll.laCRParams[1,2] = "Customer Sales Analysis"

loOGScroll.laCRParams[2,1] = 'ldEndDate'
loOGScroll.laCRParams[2,2] = lDToDate  



DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcTempFile+'.DBF' 

SELECT(lcTempFile)
LOCATE 
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF  


 
USE IN (lcTempFile)
looGScroll.lcOGLastForm = 'SOSLSRP'
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

lcLogoPath = looGScroll.lcLogoPath 
looGScroll.lcLogoPath = ''

= gfDispRe("SOSLSRP")
looGScroll.lcLogoPath = lcLogoPath 

*!**************************************************************************
*! Name      : lfCreatTemp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/29/2009
*! Purpose   : To create Temp File For report
*!**************************************************************************
*! Example   : lfCreatTemp()
*!**************************************************************************
FUNCTION lfCreatTemp
DIMENSION laFlStru[17,4]

laFlStru[1,1] = 'REPCODE'
laFlStru[1,2] = 'C'
laFlStru[1,3] = 3
laFlStru[1,4] = 0

laFlStru[2,1] = 'REPNAME'
laFlStru[2,2] = 'C'
laFlStru[2,3] = 30
laFlStru[2,4] = 0


laFlStru[3,1] = 'ACCOUNT'
laFlStru[3,2] = 'C'
laFlStru[3,3] = 5
laFlStru[3,4] = 0

laFlStru[4,1] = 'BtName'
laFlStru[4,2] = 'C'
laFlStru[4,3] = 30
laFlStru[4,4] = 0

laFlStru[5,1] = 'CAddress'
laFlStru[5,2] = 'C'
laFlStru[5,3] = 30
laFlStru[5,4] = 0

laFlStru[6,1] = 'nCyrSlsM'
laFlStru[6,2] = 'N'
laFlStru[6,3] = 17
laFlStru[6,4] = 2

laFlStru[7,1] = 'nCyrGMM'
laFlStru[7,2] = 'N'
laFlStru[7,3] = 17
laFlStru[7,4] = 2

laFlStru[8,1] = 'nLyrSlsM'
laFlStru[8,2] = 'N'
laFlStru[8,3] = 17
laFlStru[8,4] = 2

laFlStru[9,1] = 'nLyrGMM'
laFlStru[9,2] = 'N'
laFlStru[9,3] = 17
laFlStru[9,4] = 2

laFlStru[10,1] = 'nCLyrMV'
laFlStru[10,2] = 'N'
laFlStru[10,3] = 17
laFlStru[10,4] = 2

laFlStru[11,1] = 'nCYTDM'
laFlStru[11,2] = 'N'
laFlStru[11,3] = 17
laFlStru[11,4] = 2

laFlStru[12,1] = 'nCYTDGM'
laFlStru[12,2] = 'N'
laFlStru[12,3] = 17
laFlStru[12,4] = 2

laFlStru[13,1] = 'nLYTDM'
laFlStru[13,2] = 'N'
laFlStru[13,3] = 17
laFlStru[13,4] = 2

laFlStru[14,1] = 'nLYTDGM'
laFlStru[14,2] = 'N'
laFlStru[14,3] = 17
laFlStru[14,4] = 2

laFlStru[15,1] = 'nLYTDMV'
laFlStru[15,2] = 'N'
laFlStru[15,3] = 17
laFlStru[15,4] = 2

laFlStru[16,1] = 'nTOTLST'
laFlStru[16,2] = 'N'
laFlStru[16,3] = 17
laFlStru[16,4] = 2

laFlStru[17,1] = 'CAddress1'
laFlStru[17,2] = 'C'
laFlStru[17,3] = 30
laFlStru[17,4] = 0

= gfCrtTmp(lcTempFile,@laFlStru,"RepCode+Account",lcTempFile,.F.)

*!**************************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/29/2009
*! Purpose   : To Collect data For report
*!**************************************************************************
*! Example   : lfCreatTemp()
*!**************************************************************************
*: B609078,1 MMT 11/09/2009 Fix bug of Collecting from Ordhdr not invhdr[Start]
*FUNCTION lfCollect
FUNCTION _lfCollect
*: B609078,1 MMT 11/09/2009 Fix bug of Collecting from Ordhdr not invhdr[End]

IF !USED('ORDHDR')
  =gfOpenTable('ORDHDR',"ORDHDR")
ENDIF 

IF !USED('ORDLINE')
  =gfOpenTable('ORDLINE',"ORDLINE")
ENDIF 

IF !USED('INVLINE')
  =gfOpenTable('INVLINE',"INVLINEO")
ENDIF 

IF !USED('SalesRep')
  =gfOpenTable('SalesRep',"SalesRep")
ENDIF 

IF !USED('Customer')
  =gfOpenTable('Customer',"Customer")
ENDIF 

IF !USED('Style')
  =gfOpenTable('Style',"Style")
ENDIF 

IF !USED('INVHDR')
  =gfOpenTable('INVHDR',"INVHDR")
ENDIF 



lcCustFile = ""
llUseCust = .F.
lnPos = ASCAN(loOGScroll.laogFxFlt,'CUSTOMER.ACCOUNT')
IF lnPos <> 0 
 lnPos = ASUBSCRIPT(loOGScroll.laogFxFlt,lnPos,1)
 lcCustFile = loOGScroll.laogFxFlt[lnPos,6]
 IF !EMPTY(lcCustFile) AND USED(lcCustFile)
   SELECT(lcCustFile)
   LOCATE 
   IF !EOF()
     llUseCust = .T.
   ENDIF 
 ENDIF 
ENDIF  

lcRepFile = ""
llUseRep = .F.
lnPos = ASCAN(loOGScroll.laogFxFlt,'SALESREP.REPCODE')
IF lnPos <> 0 
 lnPos = ASUBSCRIPT(loOGScroll.laogFxFlt,lnPos,1)
 lcRepFile = loOGScroll.laogFxFlt[lnPos,6]
 IF !EMPTY(lcRepFile) AND USED(lcRepFile)
   SELECT(lcRepFile)
   LOCATE 
   IF !EOF()
     llUseRep = .T.
   ENDIF 
 ENDIF 
ENDIF  
WAIT  WINDOW  'Collecting Data...' NOWAIT 
*Month Data This Year
SELECT Ordhdr
=gfSeek('O')
SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR BETWEEN(Entered,ldFromDate,lDToDate) AND Status <> 'X' AND !EMPTY(Rep1) AND ;
				IIF(llUseRep ,SEEK(Rep1,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
 
  m.nCyrSlsM= 0
  m.nCyrGMM= 0
  m.nLyrSlsM= 0
  m.nLyrGMM= 0
  m.nCLyrMV= 0
  m.nCYTDM= 0
  m.nCYTDGM= 0
  m.nLYTDM= 0
  m.nLYTDGM= 0
  m.nLYTDMV= 0
  m.nTOTLST= 0
  m.REPCODE  =  Ordhdr.Rep1			
  m.Account = Ordhdr.Account
  IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
    =gfSEEK(m.REPCODE,'SalesRep')
    m.REPNAME = SalesRep.Name
    =gfSEEK('M'+m.Account,'Customer')
    m.BtName = CUstomer.BtNAme
    m.CAddress = CUstomer.caddress3 
    m.CAddress1  = CUstomer.caddress4
    SELECT Ordline
    =gfSeek('O'+Ordhdr.order)
    
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
      =gfSeek(Ordline.Style,'Style')
      IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
        SELECT Invline 
        SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
          IF invline.Price <> 0
            m.nCyrGMM = m.nCyrGMM + (100 - (((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
    ENDSCAN 
    m.nCyrSlsM = m.nCyrSlsM + Ordhdr.BOOKAMT 
    INSERT INTO (lcTempFile) FROM MEMVAR 
  ELSE
    SELECT Ordline
    =gfSeek('O'+Ordhdr.order)
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
      =gfSeek(Ordline.Style,'Style')
      IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
        SELECT Invline 
        SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
          IF invline.Price <> 0
            m.nCyrGMM = m.nCyrGMM + (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
    ENDSCAN 
    REPLACE nCyrSlsM WITH nCyrSlsM + Ordhdr.BOOKAMT,;
            nCyrGMM WITH  nCyrGMM  + m.nCyrGMM IN (lcTempFile)
    
  ENDIF   
ENDSCAN 

IF llUseRep 
  SELECT Ordhdr
  =gfSeek('O')
  SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR BETWEEN(Entered,ldFromDate,lDToDate) AND Status <> 'X' AND !EMPTY(Rep2) AND  ;
          IIF(llUseRep ,SEEK(Rep2,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
   
    m.nCyrSlsM= 0
    m.nCyrGMM= 0
    m.nLyrSlsM= 0
    m.nLyrGMM= 0
    m.nCLyrMV= 0
    m.nCYTDM= 0
    m.nCYTDGM= 0
    m.nLYTDM= 0
    m.nLYTDGM= 0
    m.nLYTDMV= 0
    m.nTOTLST= 0
    m.REPCODE  =  Ordhdr.Rep2      
    m.Account = Ordhdr.Account
    IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
      =gfSEEK(m.REPCODE,'SalesRep')
      m.REPNAME = SalesRep.Name
      =gfSEEK('M'+m.Account,'Customer')
      m.BtName = CUstomer.BtNAme
      m.CAddress = CUstomer.caddress3
      m.CAddress1  = CUstomer.caddress4
      SELECT Ordline
      =gfSeek('O'+Ordhdr.order)
      
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
        =gfSeek(Ordline.Style,'Style')
        IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
          SELECT Invline 
          SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
            IF invline.Price <> 0
              m.nCyrGMM = m.nCyrGMM + (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
            ENDIF   
          ENDSCAN   
        ENDIF 
      ENDSCAN 
      m.nCyrSlsM = m.nCyrSlsM + Ordhdr.BOOKAMT 
      INSERT INTO (lcTempFile) FROM MEMVAR 
    ELSE
      SELECT Ordline
      =gfSeek('O'+Ordhdr.order)
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
        =gfSeek(Ordline.Style,'Style')
        IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
          SELECT Invline 
          SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
            IF invline.Price <> 0
              m.nCyrGMM = m.nCyrGMM + (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
            ENDIF   
          ENDSCAN   
        ENDIF 
      ENDSCAN 
      REPLACE nCyrSlsM WITH nCyrSlsM + Ordhdr.BOOKAMT,;
              nCyrGMM WITH  nCyrGMM  + m.nCyrGMM IN (lcTempFile)
      
    ENDIF   
  ENDSCAN         
ENDIF 
*Month Data Previous Year      
lnYear = YEAR(ldFromDate)
lnMonth = MONTH(ldFromDate)
lnDay = DAY(ldFromDate)
ldPrvFromDate = DATE(lnYear-1 ,lnMonth ,lnDay )


lnYear = YEAR(lDToDate)
lnMonth = MONTH(lDToDate)
lnDay = DAY(lDToDate)
lDPRvToDate= DATE(lnYear-1 ,lnMonth ,lnDay )


SELECT Ordhdr
=gfSeek('O')
SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR BETWEEN(Entered,ldPrvFromDate ,lDPRvToDate) AND Status <> 'X' AND !EMPTY(Rep1) AND ;
        IIF(llUseRep ,SEEK(Rep1,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
 
  m.nCyrSlsM= 0
  m.nCyrGMM= 0
  m.nLyrSlsM= 0
  m.nLyrGMM= 0
  m.nCLyrMV= 0
  m.nCYTDM= 0
  m.nCYTDGM= 0
  m.nLYTDM= 0
  m.nLYTDGM= 0
  m.nLYTDMV= 0
  m.nTOTLST= 0
  m.REPCODE  =  Ordhdr.Rep1      
  m.Account = Ordhdr.Account
  IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
    =gfSEEK(m.REPCODE,'SalesRep')
    m.REPNAME = SalesRep.Name
    =gfSEEK('M'+m.Account,'Customer')
    m.BtName = CUstomer.BtNAme
    m.CAddress = CUstomer.caddress3 
    m.CAddress1  = CUstomer.caddress4
    SELECT Ordline
    =gfSeek('O'+Ordhdr.order)
    
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
      =gfSeek(Ordline.Style,'Style')
      IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
        SELECT Invline 
        SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
          IF invline.Price <> 0
            m.nLyrGMM= m.nLyrGMM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
    ENDSCAN 
    m.nLyrSlsM= m.nLyrSlsM+ Ordhdr.BOOKAMT 
    INSERT INTO (lcTempFile) FROM MEMVAR 
  ELSE
    SELECT Ordline
    =gfSeek('O'+Ordhdr.order)
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
      =gfSeek(Ordline.Style,'Style')
      IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
        SELECT Invline 
        SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
          IF invline.Price <> 0
            m.nLyrGMM= m.nLyrGMM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
    ENDSCAN 
    REPLACE nLyrSlsM WITH nLyrSlsM + Ordhdr.BOOKAMT,;
            nLyrGMM WITH  nLyrGMM+ m.nLyrGMM IN (lcTempFile)
    
  ENDIF   
ENDSCAN 

IF llUseRep 
  SELECT Ordhdr
  =gfSeek('O')
  SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR BETWEEN(Entered,ldPrvFromDate ,lDPRvToDate) AND Status <> 'X' AND !EMPTY(Rep2) AND ;
          IIF(llUseRep ,SEEK(Rep2,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
   
    m.nCyrSlsM= 0
    m.nCyrGMM= 0
    m.nLyrSlsM= 0
    m.nLyrGMM= 0
    m.nCLyrMV= 0
    m.nCYTDM= 0
    m.nCYTDGM= 0
    m.nLYTDM= 0
    m.nLYTDGM= 0
    m.nLYTDMV= 0
    m.nTOTLST= 0
    m.REPCODE  =  Ordhdr.Rep2      
    m.Account = Ordhdr.Account
    IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
      =gfSEEK(m.REPCODE,'SalesRep')
      m.REPNAME = SalesRep.Name
      =gfSEEK('M'+m.Account,'Customer')
      m.BtName = CUstomer.BtNAme
      m.CAddress = CUstomer.caddress3
      m.CAddress1  = CUstomer.caddress4
      SELECT Ordline
      =gfSeek('O'+Ordhdr.order)
      
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
        =gfSeek(Ordline.Style,'Style')
        IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
          SELECT Invline 
          SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
            IF invline.Price <> 0
              m.nLyrGMM= m.nLyrGMM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
            ENDIF   
          ENDSCAN   
        ENDIF 
      ENDSCAN 
      m.nLyrSlsM= m.nLyrSlsM+ Ordhdr.BOOKAMT 
      INSERT INTO (lcTempFile) FROM MEMVAR 
    ELSE
      SELECT Ordline
      =gfSeek('O'+Ordhdr.order)
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
        =gfSeek(Ordline.Style,'Style')
        IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
          SELECT Invline 
          SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
            IF invline.Price <> 0
              m.nLyrGMM= m.nLyrGMM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
            endif   
          ENDSCAN   
        ENDIF 
      ENDSCAN 
      REPLACE nLyrSlsM WITH nLyrSlsM + Ordhdr.BOOKAMT,;
              nLyrGMM WITH  nLyrGMM+ m.nLyrGMM IN (lcTempFile)
      
    ENDIF   
  ENDSCAN         
ENDIF 
      
*Year TO Date (This year)
lnYear = YEAR(ldFromDate)
ldStartDate = DATE(lnYear ,1,1)



SELECT Ordhdr
=gfSeek('O')
SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR BETWEEN(Entered,ldStartDate ,lDToDate) AND Status <> 'X' AND !EMPTY(Rep1) AND ;
        IIF(llUseRep ,SEEK(Rep1,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
 
  m.nCyrSlsM= 0
  m.nCyrGMM= 0
  m.nLyrSlsM= 0
  m.nLyrGMM= 0
  m.nCLyrMV= 0
  m.nCYTDM= 0
  m.nCYTDGM= 0
  m.nLYTDM= 0
  m.nLYTDGM= 0
  m.nLYTDMV= 0
  m.nTOTLST= 0
  m.REPCODE  =  Ordhdr.Rep1      
  m.Account = Ordhdr.Account
  IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
    =gfSEEK(m.REPCODE,'SalesRep')
    m.REPNAME = SalesRep.Name
    =gfSEEK('M'+m.Account,'Customer')
    m.BtName = CUstomer.BtNAme
    m.CAddress = CUstomer.caddress3
    m.CAddress1  = CUstomer.caddress4
    SELECT Ordline
    =gfSeek('O'+Ordhdr.order)
    
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
      =gfSeek(Ordline.Style,'Style')
      IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
        SELECT Invline 
        SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
          IF invline.Price <> 0
            m.nCYTDGM= m.nCYTDGM+(100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
    ENDSCAN 
    m.nCYTDM = m.nCYTDM+ Ordhdr.BOOKAMT 
    INSERT INTO (lcTempFile) FROM MEMVAR 
  ELSE
    SELECT Ordline
    =gfSeek('O'+Ordhdr.order)
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
      =gfSeek(Ordline.Style,'Style')
      IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
        SELECT Invline 
        SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
          IF invline.Price <> 0
            m.nCYTDGM= m.nCYTDGM+(100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
    ENDSCAN 
    REPLACE nCYTDM WITH nCYTDM+ Ordhdr.BOOKAMT,;
            nCYTDGM WITH  nCYTDGM+ m.nCYTDGM IN (lcTempFile)
    
  ENDIF   
ENDSCAN 

IF llUseRep 
  SELECT Ordhdr
  =gfSeek('O')
  SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR BETWEEN(Entered,ldStartDate ,lDToDate) AND Status <> 'X' AND !EMPTY(Rep2) AND ;
          IIF(llUseRep ,SEEK(Rep2,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
   
    m.nCyrSlsM= 0
    m.nCyrGMM= 0
    m.nLyrSlsM= 0
    m.nLyrGMM= 0
    m.nCLyrMV= 0
    m.nCYTDM= 0
    m.nCYTDGM= 0
    m.nLYTDM= 0
    m.nLYTDGM= 0
    m.nLYTDMV= 0
    m.nTOTLST= 0
    m.REPCODE  =  Ordhdr.Rep2      
    m.Account = Ordhdr.Account
    IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
      =gfSEEK(m.REPCODE,'SalesRep')
      m.REPNAME = SalesRep.Name
      =gfSEEK('M'+m.Account,'Customer')
      m.BtName = CUstomer.BtNAme
      m.CAddress = CUstomer.caddress3
      m.CAddress1  = CUstomer.caddress4
      SELECT Ordline
      =gfSeek('O'+Ordhdr.order)
      
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
        =gfSeek(Ordline.Style,'Style')
        IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
          SELECT Invline 
          SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
            IF invline.Price <> 0
              m.nCYTDGM= m.nCYTDGM+(100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
            ENDIF   
          ENDSCAN   
        ENDIF 
      ENDSCAN 
      m.nCYTDM= m.nCYTDM+ Ordhdr.BOOKAMT 
      INSERT INTO (lcTempFile) FROM MEMVAR 
    ELSE
      SELECT Ordline
      =gfSeek('O'+Ordhdr.order)
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
        =gfSeek(Ordline.Style,'Style')
        IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
          SELECT Invline 
          SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
            IF invline.Price <> 0
              m.nCYTDGM= m.nCYTDGM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
            ENDIF   
          ENDSCAN   
        ENDIF 
      ENDSCAN 
      REPLACE nCYTDM WITH nCYTDM+ Ordhdr.BOOKAMT,;
              nCYTDGM WITH  nCYTDGM + m.nCYTDGM IN (lcTempFile)
      
    ENDIF   
  ENDSCAN         
ENDIF 
      

*Last Year TO Date (last year)
lnYear = YEAR(ldFromDate)
ldStartDate = DATE(lnYear-1 ,1,1)

lnYear = YEAR(lDToDate)
lnMonth = MONTH(lDToDate)
lnDay = DAY(lDToDate)
lDPRvToDate= DATE(lnYear-1 ,lnMonth ,lnDay )




SELECT Ordhdr
=gfSeek('O')
SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR BETWEEN(Entered,ldStartDate ,lDPRvToDate) AND Status <> 'X' AND !EMPTY(Rep1) AND ;
        IIF(llUseRep ,SEEK(Rep1,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
 
  m.nCyrSlsM= 0
  m.nCyrGMM= 0
  m.nLyrSlsM= 0
  m.nLyrGMM= 0
  m.nCLyrMV= 0
  m.nCYTDM= 0
  m.nCYTDGM= 0
  m.nLYTDM= 0
  m.nLYTDGM= 0
  m.nLYTDMV= 0
  m.nTOTLST= 0
  m.REPCODE  =  Ordhdr.Rep1      
  m.Account = Ordhdr.Account
  IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
    =gfSEEK(m.REPCODE,'SalesRep')
    m.REPNAME = SalesRep.Name
    =gfSEEK('M'+m.Account,'Customer')
    m.BtName = CUstomer.BtNAme
    m.CAddress = CUstomer.caddress3
    m.CAddress1  = CUstomer.caddress4
    
    SELECT Ordline
    =gfSeek('O'+Ordhdr.order)
    
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
      =gfSeek(Ordline.Style,'Style')
      IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
        SELECT Invline 
        SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
          IF invline.Price <> 0
            m.nLYTDGM= m.nLYTDGM+(100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          eNdif   
        ENDSCAN   
      ENDIF 
    ENDSCAN 
    m.nLYTDM= m.nLYTDM + Ordhdr.BOOKAMT 
    INSERT INTO (lcTempFile) FROM MEMVAR 
  ELSE
    SELECT Ordline
    =gfSeek('O'+Ordhdr.order)
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
      =gfSeek(Ordline.Style,'Style')
      IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
        SELECT Invline 
        SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
          IF invline.Price <> 0
            m.nLYTDGM= m.nLYTDGM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
    ENDSCAN 
    REPLACE nLYTDM WITH nLYTDM + Ordhdr.BOOKAMT,;
            nLYTDGM WITH  nLYTDGM+ m.nLYTDGM IN (lcTempFile)
    
  ENDIF   
ENDSCAN 

IF llUseRep 
  SELECT Ordhdr
  =gfSeek('O')
  SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR BETWEEN(Entered,ldStartDate ,lDPRvToDate) AND Status <> 'X' AND !EMPTY(Rep2) AND ;
          IIF(llUseRep ,SEEK(Rep2,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
   
    m.nCyrSlsM= 0
    m.nCyrGMM= 0
    m.nLyrSlsM= 0
    m.nLyrGMM= 0
    m.nCLyrMV= 0
    m.nCYTDM= 0
    m.nCYTDGM= 0
    m.nLYTDM= 0
    m.nLYTDGM= 0
    m.nLYTDMV= 0
    m.nTOTLST= 0
    m.REPCODE  =  Ordhdr.Rep2      
    m.Account = Ordhdr.Account
    IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
      =gfSEEK(m.REPCODE,'SalesRep')
      m.REPNAME = SalesRep.Name
      =gfSEEK('M'+m.Account,'Customer')
      m.BtName = CUstomer.BtNAme
      m.CAddress = CUstomer.caddress3
      m.CAddress1  = CUstomer.caddress4
      SELECT Ordline
      =gfSeek('O'+Ordhdr.order)
      
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
        =gfSeek(Ordline.Style,'Style')
        IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
          SELECT Invline 
          SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
           IF invline.Price <> 0
              m.nLYTDGM= m.nLYTDGM+(100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
           endif   
          ENDSCAN   
        ENDIF 
      ENDSCAN 
      m.nLYTDM = m.nLYTDM+ Ordhdr.BOOKAMT 
      INSERT INTO (lcTempFile) FROM MEMVAR 
    ELSE
      SELECT Ordline
      =gfSeek('O'+Ordhdr.order)
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+Ordhdr.order
        =gfSeek(Ordline.Style,'Style')
        IF gfSeek(Ordline.Order+STR(Ordline.LINENO,6),'INVLINE')
          SELECT Invline 
          SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE= Ordline.Order+STR(Ordline.LINENO,6) FOR gfSeek(Invline.Invoice,'Invhdr') AND invhdr.Status <> 'V'
            IF invline.Price <> 0
              m.nLYTDGM= m.nLYTDGM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
            ENDIF   
          ENDSCAN   
        ENDIF 
      ENDSCAN 
      REPLACE nLYTDM WITH nLYTDM + Ordhdr.BOOKAMT,;
              nLYTDGM WITH  nLYTDGM+ m.nLYTDGM IN (lcTempFile)
      
    ENDIF   
  ENDSCAN         
ENDIF 
      
*Last Year Sales 
lnYear = YEAR(ldFromDate)
ldStartDate = DATE(lnYear-1 ,1,1)
lDPRvToDate= DATE(lnYear-1 ,12 ,31)




SELECT Ordhdr
=gfSeek('O')
SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR BETWEEN(Entered,ldStartDate ,lDPRvToDate) AND Status <> 'X' AND !EMPTY(Rep1) AND ;
        IIF(llUseRep ,SEEK(Rep1,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
 
  m.nCyrSlsM= 0
  m.nCyrGMM= 0
  m.nLyrSlsM= 0
  m.nLyrGMM= 0
  m.nCLyrMV= 0
  m.nCYTDM= 0
  m.nCYTDGM= 0
  m.nLYTDM= 0
  m.nLYTDGM= 0
  m.nLYTDMV= 0
  m.nTOTLST= 0
  m.REPCODE  =  Ordhdr.Rep1      
  m.Account = Ordhdr.Account
  IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
    =gfSEEK(m.REPCODE,'SalesRep')
    m.REPNAME = SalesRep.Name
    =gfSEEK('M'+m.Account,'Customer')
    m.BtName = CUstomer.BtNAme
    m.CAddress = CUstomer.caddress3
    m.CAddress1  = CUstomer.caddress4

    m.nTOTLST = m.nTOTLST+ Ordhdr.BOOKAMT 
    INSERT INTO (lcTempFile) FROM MEMVAR 
  ELSE
    REPLACE nTOTLST   WITH nTOTLST   + Ordhdr.BOOKAMT IN (lcTempFile)
    
  ENDIF   
ENDSCAN 

IF llUseRep 
  SELECT Ordhdr
  =gfSeek('O')
  SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR BETWEEN(Entered,ldStartDate ,lDPRvToDate) AND Status <> 'X' AND !EMPTY(Rep2) AND ;
          IIF(llUseRep ,SEEK(Rep2,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
   
    m.nCyrSlsM= 0
    m.nCyrGMM= 0
    m.nLyrSlsM= 0
    m.nLyrGMM= 0
    m.nCLyrMV= 0
    m.nCYTDM= 0
    m.nCYTDGM= 0
    m.nLYTDM= 0
    m.nLYTDGM= 0
    m.nLYTDMV= 0
    m.nTOTLST= 0
    m.REPCODE  =  Ordhdr.Rep2      
    m.Account = Ordhdr.Account
    IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
      =gfSEEK(m.REPCODE,'SalesRep')
      m.REPNAME = SalesRep.Name
      =gfSEEK('M'+m.Account,'Customer')
      m.BtName = CUstomer.BtNAme
      m.CAddress = CUstomer.caddress3
      m.CAddress1  = CUstomer.caddress4
      m.nTOTLST = m.nTOTLST+ Ordhdr.BOOKAMT 
      INSERT INTO (lcTempFile) FROM MEMVAR 
    ELSE
      REPLACE nTOTLST  WITH nTOTLST + Ordhdr.BOOKAMT IN (lcTempFile)
    ENDIF   
  ENDSCAN         
ENDIF 

SELECT (lcTempFile)
SCAN 
  REPLACE nCLyrMV WITH nCyrSlsM - nLyrSlsM,;
          nLYTDMV WITH nCYTDM - nLYTDM
ENDSCAN 
      
*: B609078,1 MMT 11/09/2009 Fix bug of Collecting from Ordhdr not invhdr[Start]
*!**************************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/09/2009
*! Purpose   : To Collect data For report
*!**************************************************************************
*! Example   : lfCollect()
*!**************************************************************************
FUNCTION lfCollect

IF !USED('ORDHDR')
  =gfOpenTable('ORDHDR',"ORDHDR")
ENDIF 

IF !USED('ORDLINE')
  =gfOpenTable('ORDLINE',"ORDLINE")
ENDIF 

IF !USED('INVLINE')
  =gfOpenTable('INVLINE',"INVLINE")
ENDIF 

IF !USED('SalesRep')
  =gfOpenTable('SalesRep',"SalesRep")
ENDIF 

IF !USED('Customer')
  =gfOpenTable('Customer',"Customer")
ENDIF 

IF !USED('Style')
  =gfOpenTable('Style',"Style")
ENDIF 

IF !USED('INVHDR')
  =gfOpenTable('INVHDR',"INVHDR")
ENDIF 



lcCustFile = ""
llUseCust = .F.
lnPos = ASCAN(loOGScroll.laogFxFlt,'CUSTOMER.ACCOUNT')
IF lnPos <> 0 
 lnPos = ASUBSCRIPT(loOGScroll.laogFxFlt,lnPos,1)
 lcCustFile = loOGScroll.laogFxFlt[lnPos,6]
 IF !EMPTY(lcCustFile) AND USED(lcCustFile)
   SELECT(lcCustFile)
   LOCATE 
   IF !EOF()
     llUseCust = .T.
   ENDIF 
 ENDIF 
ENDIF  

lcRepFile = ""
llUseRep = .F.
lnPos = ASCAN(loOGScroll.laogFxFlt,'SALESREP.REPCODE')
IF lnPos <> 0 
 lnPos = ASUBSCRIPT(loOGScroll.laogFxFlt,lnPos,1)
 lcRepFile = loOGScroll.laogFxFlt[lnPos,6]
 IF !EMPTY(lcRepFile) AND USED(lcRepFile)
   SELECT(lcRepFile)
   LOCATE 
   IF !EOF()
     llUseRep = .T.
   ENDIF 
 ENDIF 
ENDIF  

WAIT  WINDOW  'Collecting Data...' NOWAIT 
*Month Data This Year
SELECT INVHDR
=gfSeek('')
SCAN FOR BETWEEN(Invhdr.Invdate,ldFromDate,lDToDate) AND Status <> 'V' AND !EMPTY(Rep1) AND ;
				IIF(llUseRep ,SEEK(Rep1,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
 
  m.nCyrSlsM= 0
  m.nCyrGMM= 0
  m.nLyrSlsM= 0
  m.nLyrGMM= 0
  m.nCLyrMV= 0
  m.nCYTDM= 0
  m.nCYTDGM= 0
  m.nLYTDM= 0
  m.nLYTDGM= 0
  m.nLYTDMV= 0
  m.nTOTLST= 0
  m.REPCODE  =  INVHDR.Rep1			
  m.Account = INVHDR.Account
  IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
    =gfSEEK(m.REPCODE,'SalesRep')
    m.REPNAME = SalesRep.Name
    =gfSEEK('M'+m.Account,'Customer')
    m.BtName = CUstomer.BtNAme
    m.CAddress = CUstomer.caddress3 
    m.CAddress1  = CUstomer.caddress4
    IF gfSeek(INVHDR.Invoice,'INVLINE')
      SELECT Invline 
      SCAN REST WHILE INVOICE= INVHDR.Invoice 
        =gfSeek(Invline .Style,'Style')
        IF invline.Price <> 0
          m.nCyrGMM = m.nCyrGMM + (100 - (((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
        ENDIF   
      ENDSCAN   
    ENDIF 
    m.nCyrSlsM = m.nCyrSlsM + Invhdr.Shipamt 
    INSERT INTO (lcTempFile) FROM MEMVAR 
  ELSE
    IF gfSeek(INVHDR.Invoice ,'INVLINE')
      SELECT Invline 
      SCAN REST WHILE INVOICE= INVHDR.Invoice 
        =gfSeek(Invline.Style,'Style')
        IF invline.Price <> 0
          m.nCyrGMM = m.nCyrGMM + (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
        ENDIF   
      ENDSCAN   
    ENDIF 
    REPLACE nCyrSlsM WITH nCyrSlsM + Invhdr.Shipamt ,;
            nCyrGMM WITH  nCyrGMM  + m.nCyrGMM IN (lcTempFile)
    
  ENDIF   
ENDSCAN 

IF llUseRep 
  SELECT Invhdr
  =gfSeek('')
  SCAN FOR BETWEEN(Invhdr.Invdate,ldFromDate,lDToDate) AND Status <> 'V' AND !EMPTY(Rep2) AND  ;
          IIF(llUseRep ,SEEK(Rep2,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
   
    m.nCyrSlsM= 0
    m.nCyrGMM= 0
    m.nLyrSlsM= 0
    m.nLyrGMM= 0
    m.nCLyrMV= 0
    m.nCYTDM= 0
    m.nCYTDGM= 0
    m.nLYTDM= 0
    m.nLYTDGM= 0
    m.nLYTDMV= 0
    m.nTOTLST= 0
    m.REPCODE  =  Invhdr.Rep2      
    m.Account = Invhdr.Account
    IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
      =gfSEEK(m.REPCODE,'SalesRep')
      m.REPNAME = SalesRep.Name
      =gfSEEK('M'+m.Account,'Customer')
      m.BtName = CUstomer.BtNAme
      m.CAddress = CUstomer.caddress3
      m.CAddress1  = CUstomer.caddress4
      
      IF gfSeek(Invhdr.INVOICE,'INVLINE')
        SELECT Invline 
        SCAN REST WHILE INVOICE= Invhdr.INVOICE
          =gfSeek(Invline.Style,'Style')
          IF invline.Price <> 0
            m.nCyrGMM = m.nCyrGMM + (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
      m.nCyrSlsM = m.nCyrSlsM + Invhdr.Shipamt 
      INSERT INTO (lcTempFile) FROM MEMVAR 
    ELSE
      IF gfSeek(Invhdr.INVOICE,'INVLINE')
        SELECT Invline 
        SCAN REST WHILE INVOICE= Invhdr.INVOICE
          =gfSeek(Invline.Style,'Style')
          IF invline.Price <> 0
            m.nCyrGMM = m.nCyrGMM + (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
      REPLACE nCyrSlsM WITH nCyrSlsM + Invhdr.Shipamt ,;
              nCyrGMM WITH  nCyrGMM  + m.nCyrGMM IN (lcTempFile)
      
    ENDIF   
  ENDSCAN         
ENDIF 
*Month Data Previous Year      
lnYear = YEAR(ldFromDate)
lnMonth = MONTH(ldFromDate)
lnDay = DAY(ldFromDate)
ldPrvFromDate = DATE(lnYear-1 ,lnMonth ,lnDay )


lnYear = YEAR(lDToDate)
lnMonth = MONTH(lDToDate)
lnDay = DAY(lDToDate)
lDPRvToDate= DATE(lnYear-1 ,lnMonth ,lnDay )


SELECT Invhdr
=gfSeek('')
SCAN FOR BETWEEN(INVDATE,ldPrvFromDate ,lDPRvToDate) AND Status <> 'V' AND !EMPTY(Rep1) AND ;
        IIF(llUseRep ,SEEK(Rep1,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
 
  m.nCyrSlsM= 0
  m.nCyrGMM= 0
  m.nLyrSlsM= 0
  m.nLyrGMM= 0
  m.nCLyrMV= 0
  m.nCYTDM= 0
  m.nCYTDGM= 0
  m.nLYTDM= 0
  m.nLYTDGM= 0
  m.nLYTDMV= 0
  m.nTOTLST= 0
  m.REPCODE  =  Invhdr.Rep1      
  m.Account = Invhdr.Account
  IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
    =gfSEEK(m.REPCODE,'SalesRep')
    m.REPNAME = SalesRep.Name
    =gfSEEK('M'+m.Account,'Customer')
    m.BtName = CUstomer.BtNAme
    m.CAddress = CUstomer.caddress3 
    m.CAddress1  = CUstomer.caddress4

    IF gfSeek( Invhdr.INVOICE,'INVLINE')
      SELECT Invline 
      SCAN REST WHILE INVOICE=  Invhdr.INVOICE 
        =gfSeek(invline.Style,'Style')
        IF invline.Price <> 0
          m.nLyrGMM= m.nLyrGMM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
        ENDIF   
      ENDSCAN   
    ENDIF 
    m.nLyrSlsM= m.nLyrSlsM+ Invhdr.Shipamt 
    INSERT INTO (lcTempFile) FROM MEMVAR 
  ELSE

    IF gfSeek(Invhdr.INVOICE,'INVLINE')
      SELECT Invline 
      SCAN REST WHILE INVOICE= Invhdr.INVOICE
        =gfSeek(Invline.Style,'Style')
        IF invline.Price <> 0
          m.nLyrGMM= m.nLyrGMM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
        ENDIF   
      ENDSCAN   
    ENDIF 
    REPLACE nLyrSlsM WITH nLyrSlsM + Invhdr.Shipamt,;
            nLyrGMM WITH  nLyrGMM+ m.nLyrGMM IN (lcTempFile)
    
  ENDIF   
ENDSCAN 

IF llUseRep 
  SELECT Invhdr
  =gfSeek('')
  SCAN FOR BETWEEN(INVDATE,ldPrvFromDate ,lDPRvToDate) AND Status <> 'V' AND !EMPTY(Rep2) AND ;
          IIF(llUseRep ,SEEK(Rep2,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
   
    m.nCyrSlsM= 0
    m.nCyrGMM= 0
    m.nLyrSlsM= 0
    m.nLyrGMM= 0
    m.nCLyrMV= 0
    m.nCYTDM= 0
    m.nCYTDGM= 0
    m.nLYTDM= 0
    m.nLYTDGM= 0
    m.nLYTDMV= 0
    m.nTOTLST= 0
    m.REPCODE  =  Invhdr.Rep2      
    m.Account = Invhdr.Account
    IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
      =gfSEEK(m.REPCODE,'SalesRep')
      m.REPNAME = SalesRep.Name
      =gfSEEK('M'+m.Account,'Customer')
      m.BtName = CUstomer.BtNAme
      m.CAddress = CUstomer.caddress3
      m.CAddress1  = CUstomer.caddress4
      IF gfSeek(Invhdr.INVOICE,'INVLINE')
        SELECT Invline 
        SCAN REST WHILE INVOICE= Invhdr.INVOICE
          =gfSeek(Invline.Style,'Style')
          IF invline.Price <> 0
            m.nLyrGMM= m.nLyrGMM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
      m.nLyrSlsM= m.nLyrSlsM+ Invhdr.Shipamt 
      INSERT INTO (lcTempFile) FROM MEMVAR 
    ELSE
      IF gfSeek(Invhdr.INVOICE,'INVLINE')
        SELECT Invline 
        SCAN REST WHILE INVOICE= Invhdr.INVOICE
          =gfSeek(invline.Style,'Style')
          IF invline.Price <> 0
            m.nLyrGMM= m.nLyrGMM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          endif   
        ENDSCAN   
      ENDIF 
      REPLACE nLyrSlsM WITH nLyrSlsM + Invhdr.Shipamt,;
              nLyrGMM WITH  nLyrGMM+ m.nLyrGMM IN (lcTempFile)
      
    ENDIF   
  ENDSCAN         
ENDIF 
      
*Year TO Date (This year)
lnYear = YEAR(ldFromDate)
ldStartDate = DATE(lnYear ,1,1)



SELECT Invhdr
=gfSeek('')
SCAN FOR BETWEEN(INVDATE,ldStartDate ,lDToDate) AND Status <> 'V' AND !EMPTY(Rep1) AND ;
        IIF(llUseRep ,SEEK(Rep1,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
 
  m.nCyrSlsM= 0
  m.nCyrGMM= 0
  m.nLyrSlsM= 0
  m.nLyrGMM= 0
  m.nCLyrMV= 0
  m.nCYTDM= 0
  m.nCYTDGM= 0
  m.nLYTDM= 0
  m.nLYTDGM= 0
  m.nLYTDMV= 0
  m.nTOTLST= 0
  m.REPCODE  =  Invhdr.Rep1      
  m.Account = Invhdr.Account
  IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
    =gfSEEK(m.REPCODE,'SalesRep')
    m.REPNAME = SalesRep.Name
    =gfSEEK('M'+m.Account,'Customer')
    m.BtName = CUstomer.BtNAme
    m.CAddress = CUstomer.caddress3
    m.CAddress1  = CUstomer.caddress4
    IF gfSeek(Invhdr.INVOICE,'INVLINE')
      SELECT Invline 
      SCAN REST WHILE INVOICE= Invhdr.INVOICE
        =gfSeek(Invline.Style,'Style')
        IF invline.Price <> 0
          m.nCYTDGM= m.nCYTDGM+(100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
        ENDIF   
      ENDSCAN   
    ENDIF 
    m.nCYTDM = m.nCYTDM+ Invhdr.Shipamt 
    INSERT INTO (lcTempFile) FROM MEMVAR 
  ELSE
    IF gfSeek(Invhdr.INVOICE,'INVLINE')
      SELECT Invline 
      SCAN REST WHILE INVOICE= Invhdr.INVOICE
        =gfSeek(Invline.Style,'Style')
        IF invline.Price <> 0
          m.nCYTDGM= m.nCYTDGM+(100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
        ENDIF   
      ENDSCAN   
    ENDIF 
    REPLACE nCYTDM WITH nCYTDM+ Invhdr.Shipamt,;
            nCYTDGM WITH  nCYTDGM+ m.nCYTDGM IN (lcTempFile)
    
  ENDIF   
ENDSCAN 

IF llUseRep 
  SELECT Invhdr
  =gfSeek('')
  SCAN FOR BETWEEN(invDate,ldStartDate ,lDToDate) AND Status <> 'V' AND !EMPTY(Rep2) AND ;
          IIF(llUseRep ,SEEK(Rep2,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
   
    m.nCyrSlsM= 0
    m.nCyrGMM= 0
    m.nLyrSlsM= 0
    m.nLyrGMM= 0
    m.nCLyrMV= 0
    m.nCYTDM= 0
    m.nCYTDGM= 0
    m.nLYTDM= 0
    m.nLYTDGM= 0
    m.nLYTDMV= 0
    m.nTOTLST= 0
    m.REPCODE  =  Invhdr.Rep2      
    m.Account = Invhdr.Account
    IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
      =gfSEEK(m.REPCODE,'SalesRep')
      m.REPNAME = SalesRep.Name
      =gfSEEK('M'+m.Account,'Customer')
      m.BtName = CUstomer.BtNAme
      m.CAddress = CUstomer.caddress3
      m.CAddress1  = CUstomer.caddress4
      IF gfSeek(Invhdr.INVOICE,'INVLINE')
        SELECT Invline 
        SCAN REST WHILE INVOICE= Invhdr.INVOICE
          =gfSeek(Invline.Style,'Style')
          IF invline.Price <> 0
            m.nCYTDGM= m.nCYTDGM+(100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
      m.nCYTDM= m.nCYTDM+ Invhdr.Shipamt 
      INSERT INTO (lcTempFile) FROM MEMVAR 
    ELSE

      IF gfSeek(Invhdr.INVOICE,'INVLINE')
        SELECT Invline 
        SCAN REST WHILE INVOICE= Invhdr.INVOICE
          =gfSeek(invline.Style,'Style')
          IF invline.Price <> 0
            m.nCYTDGM= m.nCYTDGM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
      REPLACE nCYTDM WITH nCYTDM+ Invhdr.Shipamt,;
              nCYTDGM WITH  nCYTDGM + m.nCYTDGM IN (lcTempFile)
      
    ENDIF   
  ENDSCAN         
ENDIF 
      

*Last Year TO Date (last year)
lnYear = YEAR(ldFromDate)
ldStartDate = DATE(lnYear-1 ,1,1)

lnYear = YEAR(lDToDate)
lnMonth = MONTH(lDToDate)
lnDay = DAY(lDToDate)
lDPRvToDate= DATE(lnYear-1 ,lnMonth ,lnDay )




SELECT Invhdr
=gfSeek('')
SCAN FOR BETWEEN(InvDate,ldStartDate ,lDPRvToDate) AND Status <> 'V' AND !EMPTY(Rep1) AND ;
        IIF(llUseRep ,SEEK(Rep1,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
 
  m.nCyrSlsM= 0
  m.nCyrGMM= 0
  m.nLyrSlsM= 0
  m.nLyrGMM= 0
  m.nCLyrMV= 0
  m.nCYTDM= 0
  m.nCYTDGM= 0
  m.nLYTDM= 0
  m.nLYTDGM= 0
  m.nLYTDMV= 0
  m.nTOTLST= 0
  m.REPCODE  =  Invhdr.Rep1      
  m.Account = Invhdr.Account
  IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
    =gfSEEK(m.REPCODE,'SalesRep')
    m.REPNAME = SalesRep.Name
    =gfSEEK('M'+m.Account,'Customer')
    m.BtName = CUstomer.BtNAme
    m.CAddress = CUstomer.caddress3
    m.CAddress1  = CUstomer.caddress4
    


    IF gfSeek(Invhdr.INVOICE,'INVLINE')
      SELECT Invline 
      SCAN REST WHILE INVOICE= Invhdr.INVOICE
        =gfSeek(Invline.Style,'Style')
        IF invline.Price <> 0
          m.nLYTDGM= m.nLYTDGM+(100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
        eNdif   
      ENDSCAN   
    ENDIF 
    m.nLYTDM= m.nLYTDM + Invhdr.Shipamt 
    INSERT INTO (lcTempFile) FROM MEMVAR 
  ELSE
    IF gfSeek(Invhdr.INVOICE,'INVLINE')
      SELECT Invline 
      SCAN REST WHILE INVOICE= Invhdr.INVOICE
       =gfSeek(Invline.Style,'Style')
        IF invline.Price <> 0
          m.nLYTDGM= m.nLYTDGM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
        ENDIF   
      ENDSCAN   
    ENDIF 
    REPLACE nLYTDM WITH nLYTDM + Invhdr.Shipamt,;
            nLYTDGM WITH  nLYTDGM+ m.nLYTDGM IN (lcTempFile)
    
  ENDIF   
ENDSCAN 

IF llUseRep 
  SELECT invhdr
  =gfSeek('')
  SCAN FOR BETWEEN(invDate,ldStartDate ,lDPRvToDate) AND Status <> 'V' AND !EMPTY(Rep2) AND ;
          IIF(llUseRep ,SEEK(Rep2,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
   
    m.nCyrSlsM= 0
    m.nCyrGMM= 0
    m.nLyrSlsM= 0
    m.nLyrGMM= 0
    m.nCLyrMV= 0
    m.nCYTDM= 0
    m.nCYTDGM= 0
    m.nLYTDM= 0
    m.nLYTDGM= 0
    m.nLYTDMV= 0
    m.nTOTLST= 0
    m.REPCODE  =  invhdr.Rep2      
    m.Account = invhdr.Account
    IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
      =gfSEEK(m.REPCODE,'SalesRep')
      m.REPNAME = SalesRep.Name
      =gfSEEK('M'+m.Account,'Customer')
      m.BtName = CUstomer.BtNAme
      m.CAddress = CUstomer.caddress3
      m.CAddress1  = CUstomer.caddress4

      IF gfSeek(invhdr.INVOICE,'INVLINE')
        SELECT Invline 
        SCAN REST WHILE INVOICE= invhdr.INVOICE
         =gfSeek(Invline.Style,'Style')
         IF invline.Price <> 0
            m.nLYTDGM= m.nLYTDGM+(100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
         endif   
        ENDSCAN   
      ENDIF 
      m.nLYTDM = m.nLYTDM+ Invhdr.Shipamt 
      INSERT INTO (lcTempFile) FROM MEMVAR 
    ELSE
      IF gfSeek(invhdr.INVOICE,'INVLINE')
        SELECT Invline 
        SCAN REST WHILE INVOICE= invhdr.INVOICE
          =gfSeek(invline.Style,'Style')
          IF invline.Price <> 0
            m.nLYTDGM= m.nLYTDGM+ (100-(((Style.nmCost1+Style.nmCost2+Style.nmCost3+Style.nmCost4+Style.nmCost5+Style.nmCost6+Style.nmCost7)/Invline.price)*100))
          ENDIF   
        ENDSCAN   
      ENDIF 
      REPLACE nLYTDM WITH nLYTDM + Invhdr.Shipamt,;
              nLYTDGM WITH  nLYTDGM+ m.nLYTDGM IN (lcTempFile)
      
    ENDIF   
  ENDSCAN         
ENDIF 
      
*Last Year Sales 
lnYear = YEAR(ldFromDate)
ldStartDate = DATE(lnYear-1 ,1,1)
lDPRvToDate= DATE(lnYear-1 ,12 ,31)




SELECT invhdr
=gfSeek('')
SCAN FOR BETWEEN(invDate,ldStartDate ,lDPRvToDate) AND Status <> 'V' AND !EMPTY(Rep1) AND ;
        IIF(llUseRep ,SEEK(Rep1,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
 
  m.nCyrSlsM= 0
  m.nCyrGMM= 0
  m.nLyrSlsM= 0
  m.nLyrGMM= 0
  m.nCLyrMV= 0
  m.nCYTDM= 0
  m.nCYTDGM= 0
  m.nLYTDM= 0
  m.nLYTDGM= 0
  m.nLYTDMV= 0
  m.nTOTLST= 0
  m.REPCODE  =  invhdr.Rep1      
  m.Account = invhdr.Account
  IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
    =gfSEEK(m.REPCODE,'SalesRep')
    m.REPNAME = SalesRep.Name
    =gfSEEK('M'+m.Account,'Customer')
    m.BtName = CUstomer.BtNAme
    m.CAddress = CUstomer.caddress3
    m.CAddress1  = CUstomer.caddress4

    m.nTOTLST = m.nTOTLST+ Invhdr.Shipamt 
    INSERT INTO (lcTempFile) FROM MEMVAR 
  ELSE
    REPLACE nTOTLST   WITH nTOTLST   + Invhdr.Shipamt IN (lcTempFile)
    
  ENDIF   
ENDSCAN 

IF llUseRep 
  SELECT invhdr
  =gfSeek('')
  SCAN FOR BETWEEN(InvDate,ldStartDate ,lDPRvToDate) AND Status <> 'V' AND !EMPTY(Rep2) AND ;
          IIF(llUseRep ,SEEK(Rep2,lcRepFile ) ,.T.) AND IIF(llUseCust ,SEEK(Account,lcCustFile ),.T.)
   
    m.nCyrSlsM= 0
    m.nCyrGMM= 0
    m.nLyrSlsM= 0
    m.nLyrGMM= 0
    m.nCLyrMV= 0
    m.nCYTDM= 0
    m.nCYTDGM= 0
    m.nLYTDM= 0
    m.nLYTDGM= 0
    m.nLYTDMV= 0
    m.nTOTLST= 0
    m.REPCODE  =  invhdr.Rep2      
    m.Account = invhdr.Account
    IF !SEEK(m.REPCODE + m.Account ,lcTempFile)
      =gfSEEK(m.REPCODE,'SalesRep')
      m.REPNAME = SalesRep.Name
      =gfSEEK('M'+m.Account,'Customer')
      m.BtName = CUstomer.BtNAme
      m.CAddress = CUstomer.caddress3
      m.CAddress1  = CUstomer.caddress4
      m.nTOTLST = m.nTOTLST+ Invhdr.Shipamt 
      INSERT INTO (lcTempFile) FROM MEMVAR 
    ELSE
      REPLACE nTOTLST  WITH nTOTLST + Invhdr.Shipamt IN (lcTempFile)
    ENDIF   
  ENDSCAN         
ENDIF 

SELECT (lcTempFile)
SCAN 
  REPLACE nCLyrMV WITH nCyrSlsM - nLyrSlsM,;
          nLYTDMV WITH nCYTDM - nLYTDM
ENDSCAN 
*: B609078,1 MMT 11/09/2009 Fix bug of Collecting from Ordhdr not invhdr[End]      
