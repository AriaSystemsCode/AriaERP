*!*************************************************************
*! Name      : AREXPSW.prg
*! Developer : Mariam Mazhar
*! Date      : 05/17/2022
*! Purpose   : Export Aria5 Entities to SIIWII
*! Tracking Entry: E612561
*!*************************************************************
*! Modifications :
*! E612624,1 MMT 09/11/2022 Export Style Entity to SIIWII
*! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS
*!*************************************************************
#INCLUDE R:\Aria4xp\reports\arcslst.h

*XXX
IF ('EB' $ oAriaApplication.CompanySetupModules OR 'NC' $ oAriaApplication.CompanySetupModules )
  =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'PARTNER','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDIPD',oAriaApplication.DataDir+'PARTTRANS','SH')
  =gfOpenFile(oAriaApplication.SYSPATH+'SYCEDIPH','PARTNER','SH')
ENDIF
DIMENSION laPartDesc[1],laPartValues[1]
STORE '' TO  laPartDesc[1],laPartValues[1]
SELECT distinct sycediph.cpartname, ediacprt.cpartner FROM sycediph INNER JOIN ediacprt ON ediacprt.cpartcode == sycediph.cpartcode WHERE ;
       ediacprt.cpartcode in (SELECT  CPARTCODE FROM EDIPD WHERE  ceditrntyp='CST')and INLIST(ediacprt.type ,"D","W") ORDER BY sycediph.cpartname INTO CURSOR 'EDIPART'

lnPartCnt = RECCOUNT('EDIPART')
IF RECCOUNT('EDIPART') > 0 
  DIMENSION laPartDesc[lnPartCnt],laPartValues[lnPartCnt]
  SELECT EDIPART
  LOCATE
  lnCntP = 1
  SCAN
    laPartDesc[lnCntP ] = EDIPART.cpartname
    laPartValues[lnCntP ] = EDIPART.cpartner 
    lnCntP = lnCntP + 1 
  ENDSCAN 
ENDIF

DIMENSION laPartVDesc[1],laPartVValues[1]
STORE '' TO  laPartVDesc[1],laPartVValues[1]


SELECT distinct sycediph.cpartname, ediacprt.cpartner FROM sycediph INNER JOIN ediacprt ON ediacprt.cpartcode == sycediph.cpartcode WHERE ;
       ediacprt.cpartcode in (SELECT  CPARTCODE FROM EDIPD WHERE  ceditrntyp='VND')and INLIST(ediacprt.type ,"D","W") ORDER BY sycediph.cpartname INTO CURSOR 'EDIPART'

lnPartCnt = RECCOUNT('EDIPART')
IF RECCOUNT('EDIPART') > 0 
  DIMENSION laPartVDesc[lnPartCnt],laPartVValues[lnPartCnt]
  SELECT EDIPART
  LOCATE
  lnCntP = 1
  SCAN
    laPartVDesc[lnCntP ] = EDIPART.cpartname
    laPartVValues[lnCntP ] = EDIPART.cpartner 
    lnCntP = lnCntP + 1 
  ENDSCAN 
ENDIF
*! E612624,1 MMT 09/11/2022 Export Style Entity to SIIWII[START]
DIMENSION laPartSDesc[1],laPartSValues[1]
STORE '' TO  laPartSDesc[1],laPartSValues[1]

SELECT distinct sycediph.cpartname, ediacprt.cpartner FROM sycediph INNER JOIN ediacprt ON ediacprt.cpartcode == sycediph.cpartcode WHERE ;
       ediacprt.cpartcode in (SELECT  CPARTCODE FROM EDIPD WHERE  ceditrntyp='STY') and INLIST(ediacprt.type ,"D","W");
       ORDER BY sycediph.cpartname INTO CURSOR 'EDIPART'

lnPartCnt = RECCOUNT('EDIPART')
IF RECCOUNT('EDIPART') > 0 
  DIMENSION laPartSDesc[lnPartCnt],laPartSValues[lnPartCnt]
  SELECT EDIPART
  LOCATE
  lnCntP = 1
  SCAN
    laPartSDesc[lnCntP] = EDIPART.cpartname
    laPartSValues[lnCntP] = EDIPART.cpartner 
    lnCntP = lnCntP + 1 
  ENDSCAN 
ENDIF
*! E612624,1 MMT 09/11/2022 Export Style Entity to SIIWII[End]
lcExpr = gfOpGrid('AREXPSW' , .T.)&&,.F.,.F.,.T.,.T.)

*!************************************************************
*! Name      : lfwRepWhen
*: Developer : Mariam Mazhar(MMT)
*: Date      : 05/17/2022
*! Purpose   : When function of the OG
*!************************************************************  
FUNCTION lfwRepWhen
IF !('EB' $ oAriaApplication.CompanySetupModules OR 'NC' $ oAriaApplication.CompanySetupModules )
  RETURN .f.
ENDIF
DECLARE laRpSource[4],laRpTarget[1]
lnStatus = lcRpStatus
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARCSLST_ACTIVE,oAriaApplication.GetHeaderText("LANG_ARCSLST_ACTIVE",AHEADERFILE)) TO laRpSource[1]
STORE ''          TO laRpTarget[1]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARCSLST_HOLD,oAriaApplication.GetHeaderText("LANG_ARCSLST_HOLD",AHEADERFILE))      TO laRpSource[2] &&,laRpTarget[2]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARCSLST_POTENTAIL,oAriaApplication.GetHeaderText("LANG_ARCSLST_POTENTAIL",AHEADERFILE)) TO laRpSource[3]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARCSLST_CANCELLED,oAriaApplication.GetHeaderText("LANG_ARCSLST_CANCELLED",AHEADERFILE)) TO laRpSource[4]
lcRpStatus = "'A','H','P','X'"

*!************************************************************
*! Name      : lfvOStatus
*: Developer : Mariam Mazhar(MMT)
*: Date      : 05/17/2022
*! Purpose   : function to Display Status Mover
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
= lfOGMover(@laRpSource,@laRpTarget,;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARCSLST_SELECTSTATUS,oAriaApplication.GetHeaderText("LANG_ARCSLST_SELECTSTATUS",AHEADERFILE)) ,.T.,'')  && call mover function.
lcRpStatus = ' '
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
	lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARCSLST_ACTIVE,oAriaApplication.GetHeaderText("LANG_ARCSLST_ACTIVE",AHEADERFILE)),"'A'",;
		IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARCSLST_HOLD,oAriaApplication.GetHeaderText("LANG_ARCSLST_HOLD",AHEADERFILE)),",'H'",;
		IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARCSLST_POTENTAIL,oAriaApplication.GetHeaderText("LANG_ARCSLST_POTENTAIL",AHEADERFILE)),",'P'",;
		IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARCSLST_CANCELLED,oAriaApplication.GetHeaderText("LANG_ARCSLST_CANCELLED",AHEADERFILE)),",'X'",''))))
ENDFOR  && end Loop to make Status expression.

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length
IF LEN(lcOldStat) != LEN(lcRpStatus)
	llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
*-- loop to check if it's the same selected status or not.
	FOR lnJ = 1 TO LEN(lcOldStat)
		lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
		IF !(lcCurrChr $ lcRpStatus)
			llChStatus = .T.
			EXIT
		ENDIF
	ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF

*!************************************************************
*! Name      : RefreshStatus
*: Developer : Mariam Mazhar(MMT)
*: Date      : 05/17/2022
*! Purpose   : function to Refresh Status option selected value
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
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

*!*************************************************************
*! Name      : lfStitle
*! Developer : Mariam Mazhar
*! Date      : 05/17/2022
*! Purpose   : To get the Title for the STATE ,ZIP
*!             according to its country
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStitle()
*!*************************************************************

FUNCTION lfStitle
SET STEP ON 
PRIVATE lcSelectCommand,lnResult,lcSelectCommand1,lnResult1
lcWorkArea = SELECT()
lcSelectCommand=[SELECT CCONT_CODE FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
IF lnResult >= 1
	lcSelectCommand1=[SELECT CCONT_CODE,CPART4LAB,CPART5LAB FROM SYCINT WHERE SYCINT.CCONT_CODE=SYCCOMP.CCONT_CODE]
	lnResult1 = oAriaApplication.remotesystemdata.execute(lcSelectCommand1,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
	IF lnResult1 >= 1
		SELECT (lcWorkArea)
		RETURN (SYCINT.CPART4LAB)
		lcZipTitle = SYCINT.CPART5LAB
	ENDIF
ENDIF
SELECT (lcWorkArea)
*!*************************************************************
*! Name      : lfZtitle
*! Developer : Mariam Mazhar
*! Date      : 05/17/2022
*! Purpose   : To get the Title for the STATE ,ZIP
*!             according to its country
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfZtitle()
*!*************************************************************

FUNCTION lfZtitle
lcWorkArea = SELECT()
PRIVATE lcSelectCommand,lnResult,lcSelectCommand1,lnResult1
IF !USED('SYCCOMP')
	*lcWorkArea = SELECT()
	lcSelectCommand=[SELECT CCONT_CODE FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
	lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
	IF lnResult >= 1
		lcSelectCommand1=[SELECT CCONT_CODE,CPART5LAB FROM SYCINT WHERE SYCINT.CCONT_CODE=SYCCOMP.CCONT_CODE]
		lnResult1 = oAriaApplication.remotesystemdata.execute(lcSelectCommand1,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
		IF lnResult1 >= 1
			SELECT (lcWorkArea)
			lcZipTitle = SYCINT.CPART5LAB
			RETURN (SYCINT.CPART5LAB)
		ENDIF
	ENDIF
	SELECT (lcWorkArea)
ELSE
	lcZipTitle = SYCINT.CPART5LAB
	SELECT (lcWorkArea)
	RETURN (SYCINT.CPART5LAB)
ENDIF
*!*************************************************************
*! Name      : lfExportToSIIWII
*: Developer : MAriam Mazhar (MMT)
*: Date      : 05/17/2022
*! Purpose   : Export selected entities to EDITRANS
*!*************************************************************
*!
FUNCTION lfExportToSIIWII
SET STEP ON 

IF ('EB' $ oAriaApplication.CompanySetupModules OR 'NC' $ oAriaApplication.CompanySetupModules )
  =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'PARTNER','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDIPD',oAriaApplication.DataDir+'PARTTRANS','SH')
ENDIF
  
IF lcRPEntity $ 'CV'
*-- To get the selected Division if any.
  lcDivs = ''
  llUseDiv = .F.
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(LOOGSCROLL.laOGFXFlt,'CUSTOMER.CDIVISION'),1)
  IF lnPosition > 0
    lcDivs = LOOGSCROLL.laOGFXFlt[lnPosition,6]
    lcDivFile = loogscroll.gfTempName()
    llUseDiv = IIF(LEN(lcDivs)>0,.T.,.F.) AND lfConvertToCursor(lcDivs,'CDIVISION',lcDivFile)
  ENDIF


  *--State
  lcStates = ''
  llUseStates = .F.
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(LOOGSCROLL.laOGFXFlt,'CUSTOMER.CADDRESS4'),1)
  IF lnPosition > 0
    lcStates = LOOGSCROLL.laOGFXFlt[lnPosition,6]
    lcStatFile = loogscroll.gfTempName()
    llUseStates = IIF(LEN(lcStates)>0,.T.,.F.) AND lfConvertToCursor(lcStates ,'STATE',lcStatFile)
  ENDIF

  *Zip
  lcZip = ''
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(LOOGSCROLL.laOGFXFlt,'CUSTOMER.CADDRESS5'),1)
  IF lnPosition > 0
   lcZip = LOOGSCROLL.laOGFXFlt[lnPosition,6]
  ENDIF
ENDIF

IF ('EB' $ oAriaApplication.CompanySetupModules OR 'NC' $ oAriaApplication.CompanySetupModules )
  =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY','SH')
ENDIF
llDataToExport = .F.
lcPartCode = ''
DO CASE 
  CASE lcRPEntity ='C'
*!*      SELECT 'EDIPD'
*!*      =gfSeek('')
*!*      LOCATE FOR  CEDITRNTYP ='CST'
*!*      IF FOUND()
*!*        IF gfSeek(EDIPD.CPARTCODE,'EDIACPRT','PARTNER')
*!*          lcPartCode = EDIACPRT.CPARTNER
*!*        ENDIF
*!*      ENDIF
    lcPartCode =lcRpPartner
    *--Region
    lcRegionFile =''
    lcRegions = ''
    llUseRegion = .F.
    lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGfxFlt,ASCAN(LOOGSCROLL.laOGfxFlt,'CUSTOMER.REGION'),1)
    IF lnPosition > 0
      lcRegions  = LOOGSCROLL.laOGfxFlt[lnPosition,6]
      lcRegionFile = loogscroll.gfTempName()
      llUseRegion = IIF(LEN(lcRegions)>0,.T.,.F.) AND lfConvertToCursor(lcRegions,'REGION',lcRegionFile )
    ENDIF
    
    lcShipViaFile = ''
    lcShipVia = ''
    llUseShipVia = .F.
    lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGfxFlt,ASCAN(LOOGSCROLL.laOGfxFlt,'CUSTOMER.SHIPVIA'),1)
    IF lnPosition > 0
      lcShipVia  = LOOGSCROLL.laOGfxFlt[lnPosition,6]
      lcShipViaFile = loogscroll.gfTempName()
      llUseShipVia = IIF(LEN(lcShipVia)>0,.T.,.F.) AND lfConvertToCursor(lcShipVia,'SHIPVIA',lcShipViaFile )
    ENDIF
    
    
    lcSpecInstFile = ''
    lcSpecInst = ''
    llUseSpecInst = .F.
    lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGfxFlt,ASCAN(LOOGSCROLL.laOGfxFlt,'CUSTOMER.SPCINST'),1)
    IF lnPosition > 0
      lcSpecInst  = LOOGSCROLL.laOGfxFlt[lnPosition,6]
      lcSpecInstFile = loogscroll.gfTempName()
      llUseSpecInst = IIF(LEN(lcSpecInst)>0,.T.,.F.) AND lfConvertToCursor(lcSpecInst,'SPCINST',lcSpecInstFile )
    ENDIF
    
        
    lcTermCodeFile = ''
    lcTermCodes= ''
    llUseTermCode = .F.
    lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGfxFlt,ASCAN(LOOGSCROLL.laOGfxFlt,'CUSTOMER.CTERMCODE'),1)
    IF lnPosition > 0
      lcTermCodes = LOOGSCROLL.laOGfxFlt[lnPosition,6]
      lcTermCodeFile = loogscroll.gfTempName()
      llUseTermCode = IIF(LEN(lcTermCodes)>0,.T.,.F.) AND lfConvertToCursor(lcTermCodes,'CTERMCODE',lcTermCodeFile )
    ENDIF
    
    lcClassFile = ''
    lcClasses= ''
    llUseClasses = .F.
    lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGfxFlt,ASCAN(LOOGSCROLL.laOGfxFlt,'CUSTOMER.CLASS'),1)
    IF lnPosition > 0
      lcClasses = LOOGSCROLL.laOGfxFlt[lnPosition,6]
      lcClassFile  = loogscroll.gfTempName()
      llUseClasses= IIF(LEN(lcClasses)>0,.T.,.F.) AND lfConvertToCursor(lcClasses ,'CLASS',lcClassFile )
    ENDIF

    lnAccPos = ASCAN(loOgScroll.laOgFxFlt,"CUSTOMER.ACCOUNT")
    lnAccPos  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnAccPos ,1)
    lcAccountFlt= loOgScroll.laOgFxFlt[lnAccPos ,6]
    IF !EMPTY(lcAccountFlt) AND USED(lcAccountFlt) AND RECCOUNT(lcAccountFlt)> 0
      SELECT(lcAccountFlt)
      LOCATE 
      IF EOF()
        lcAccountFlt= ''
      ENDIF
    ENDIF 

  
    lnFacPos = ASCAN(loOgScroll.laOgFxFlt,"CUSTOMER.CFACCODE")
    lnFacPos  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnFacPos,1)
    lcFacFlt= loOgScroll.laOgFxFlt[lnFacPos ,6]
    IF !EMPTY(lcFacFlt) AND USED(lcFacFlt) AND RECCOUNT(lcFacFlt)> 0
      SELECT(lcFacFlt)
      LOCATE 
      IF EOF()
        lcFacFlt = ''
      ENDIF
    ENDIF 
    
    lnSalesRepPos = ASCAN(loOgScroll.laOgFxFlt,"CUSTOMER.SALESREP")
    lnSalesRepPos = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnSalesRepPos,1)
    lcSalesRepFlt = loOgScroll.laOgFxFlt[lnSalesRepPos,6]
    IF !EMPTY(lcSalesRepFlt ) AND USED(lcSalesRepFlt ) AND RECCOUNT(lcSalesRepFlt )> 0
      SELECT(lcSalesRepFlt )
      LOCATE 
      IF EOF()
        lcSalesRepFlt = ''
      ENDIF
    ENDIF 
    
    IF !USED('CUSTOMER')
      =gfOpenTable('Customer','Customer')
    ENDIF
    IF !USED('ORDHDR')
      =gfOpenTable('ORDHDR','ORDACCT')
    ENDIF
    
    
    IF EMPTY(lcAccountFlt)
      SELECT CUSTOMER
      =SEEK('M')
      SCAN REST WHILE TYPE+ACCOUNT+STORE = 'M' FOR IIF(!EMPTY(ldRpAddEdt),(Customer.dAdd_date > ldRpAddEdt OR CUstomer.dedit_date > ldRpAddEdt),.T.) AND ;
          IIF(!EMPTY(lcSalesRepFlt),SEEK(Customer.SalesRep,lcSalesRepFlt) OR SEEK(Customer.Rep2,lcSalesRepFlt),.T.) AND ;
          IIF(!EMPTY(lcFacFlt),SEEK(CUSTOMER.CFACCODE,lcFacFlt),.T.) AND IIF(llUseClasses,SEEK(CUSTOMER.CLASS,lcClassFile),.T.) AND ;
          IIF(llUseTermCode,SEEK(CUSTOMER.CTERMCODE,lcTermCodeFile),.T.) AND ;
          IIF(llUseSpecInst,SEEK(CUSTOMER.SPCINST,lcSpecInstFile ),.T.) AND ;
          IIF(llUseDiv,SEEK(CUSTOMER.CDIVISION,lcDivFile),.T.) AND ;
          IIF(llUseShipVia ,SEEK(CUSTOMER.SHIPVIA,lcShipViaFile),.T.) AND ;
          IIF(llUseRegion,SEEK(CUSTOMER.REGION,lcRegionFile),.T.) AND ;
          IIF(llUseStates,SEEK(CUSTOMER.CADDRESS4,lcStatFile),.T.) AND ;                                        
          IIF(!EMPTY(lcZip),CUSTOMER.CADDRESS5=lcZip,.T.) AND IIF(!EMPTY(lcRpStatus),Customer.Status $ lcRpStatus,.T.)
        WAIT WINDOW LANG_ARCSLST_SELCUST NOWAIT   
        IF !EMPTY(ldRpSoEnt) 
          SELECT ORDHDR
          IF gfSeek(CUSTOMER.Account)
            LOCATE REST WHILE ACCOUNT+CORDTYPE+ORDER = CUSTOMER.Account FOR Entered > ldRpSoEnt
            IF !FOUND()
              SELECT CUSTOMER
              LOOP 
            ENDIF
          ELSE
            SELECT CUSTOMER
            LOOP 
          ENDIF 
        ENDIF  
        llDataToExport = .T.
        IF ('EB' $ oAriaApplication.CompanySetupModules OR 'NC' $ oAriaApplication.CompanySetupModules)
          *! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS[Start]
          *XX
          lcType = 'D'
          IF SEEK('D'+lcPartCode,'EDIACPRT','ACCFACT') OR SEEK('W'+lcPartCode,'EDIACPRT','ACCFACT') 
            lcType = EDIACPRT.TYPE
          ENDIF
          *XX
          *IF !SEEK('CST'+PADR(CUSTOMER.Account,40)+'R'+lcPartCode ,'EDITRANS')
          IF !SEEK('CST'+PADR(CUSTOMER.Account,40)+lcType +lcPartCode ,'EDITRANS')
          *! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS[End]
            INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER) VALUES ;
                ('CST',CUSTOMER.Account,lcType ,lcPartCode )
          ENDIF
          REPLACE cStatus WITH 'N' IN EDITRANS
          =gfAdd_Info('EDITRANS')  
        ENDIF
          
      ENDSCAN
       
    ELSE
      SELECT (lcAccountFlt)
      LOCATE
      SCAN
        WAIT WINDOW LANG_ARCSLST_SELCUST NOWAIT 
        IF gfSEEK('M'+&lcAccountFlt..Account,'CUSTOMER','CUSTOMER')
          IF !EMPTY(ldRpAddEdt) AND (Customer.dAdd_date <= ldRpAddEdt AND CUstomer.dedit_date <= ldRpAddEdt)
            SELECT  (lcAccountFlt)
            LOOP
          ENDIF
          
          IF IIF(!EMPTY(lcSalesRepFlt),SEEK(Customer.SalesRep,lcSalesRepFlt) OR SEEK(Customer.Rep2,lcSalesRepFlt),.T.) AND ;
             IIF(!EMPTY(lcFacFlt),SEEK(CUSTOMER.CFACCODE,lcFacFlt),.T.) AND IIF(llUseClasses,SEEK(CUSTOMER.CLASS,lcClassFile),.T.) AND ;
             IIF(llUseTermCode,SEEK(CUSTOMER.CTERMCODE,lcTermCodeFile),.T.) AND ;
             IIF(llUseSpecInst,SEEK(CUSTOMER.SPCINST,lcSpecInstFile ),.T.) AND ;
             IIF(llUseDiv,SEEK(CUSTOMER.CDIVISION,lcDivFile),.T.) AND ;
             IIF(llUseShipVia ,SEEK(CUSTOMER.SHIPVIA,lcShipViaFile),.T.) AND ;
             IIF(llUseRegion,SEEK(CUSTOMER.REGION,lcRegionFile),.T.) AND ;
             IIF(llUseStates,SEEK(ALLTRIM(CUSTOMER.CADDRESS4),lcStatFile),.T.) AND ;                                        
             IIF(!EMPTY(lcZip),CUSTOMER.CADDRESS5=lcZip,.T.) AND IIF(!EMPTY(lcRpStatus),Customer.Status $ lcRpStatus,.T.)
          
            IF !EMPTY(ldRpSoEnt) 
              SELECT ORDHDR
              IF gfSeek(CUSTOMER.Account)
                LOCATE REST WHILE ACCOUNT+CORDTYPE+ORDER = CUSTOMER.Account FOR Entered > ldRpSoEnt
                IF !FOUND()
                  SELECT (lcAccountFlt)
                  LOOP 
                ENDIF
              ELSE
                SELECT (lcAccountFlt)
                LOOP 
              ENDIF 
            ENDIF  
            llDataToExport = .T.
            IF ('EB' $ oAriaApplication.CompanySetupModules OR 'NC' $ oAriaApplication.CompanySetupModules)
             *XX
             lcType = 'D'
             IF SEEK('D'+lcPartCode,'EDIACPRT','ACCFACT') OR SEEK('W'+lcPartCode,'EDIACPRT','ACCFACT') 
               lcType = EDIACPRT.TYPE
             ENDIF
             *XX
              *! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS[Start]
              *IF !SEEK('CST'+PADR(CUSTOMER.Account,40)+'R'+lcPartCode ,'EDITRANS')              
              IF !SEEK('CST'+PADR(CUSTOMER.Account,40)+lcType +lcPartCode ,'EDITRANS')
              *! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS[EEnd]
                INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER) VALUES ;
                    ('CST',CUSTOMER.Account,lcType ,lcPartCode )
              ENDIF
              REPLACE cStatus WITH 'N' IN EDITRANS
              =gfAdd_Info('EDITRANS')  
            ENDIF
          ENDIF  
        ENDIF
      ENDSCAN
    ENDIF
    WAIT CLEAR 
CASE lcRPEntity ='V'
*!*    SELECT 'EDIPD'
*!*    =gfSeek('')
*!*    LOCATE FOR  CEDITRNTYP ='VND'
*!*    IF FOUND()
*!*      IF gfSeek(EDIPD.CPARTCODE,'EDIACPRT','PARTNER')
*!*        lcPartCode = EDIACPRT.CPARTNER
*!*      ENDIF
*!*    ENDIF
  lcPartCode = lcRpPartV
  IF !USED('APVENDOR')
    =gfOpenTable('APVENDOR','VENCODE')
  ENDIF
  IF !USED('APINVHDR')
    =gfOpenTable('APINVHDR','VENDINV')    && CVENDCODE+CINVNO
  ENDIF
 
  lnVendPos = ASCAN(loOgScroll.laOgFxFlt,"APVENDOR.CVENDCODE")
  lnVendPos = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnVendPos,1)
  lcVendorFlt = loOgScroll.laOgFxFlt[lnVendPos,6]
  IF !EMPTY(lcVendorFlt) AND USED(lcVendorFlt) AND RECCOUNT(lcVendorFlt)> 0
    SELECT(lcVendorFlt)
    LOCATE 
    IF EOF()
      lcVendorFlt = ''
    ENDIF
  ENDIF 

  IF EMPTY(lcVendorFlt )
   
    SELECT APVENDOR
    =gfSeek('')
    SCAN FOR IIF(!EMPTY(ldRpAddEdt),(APVENDOR.dAdd_date > ldRpAddEdt OR APVENDOR.dedit_date > ldRpAddEdt),.T.) AND ;
       IIF(llUseDiv,SEEK(APVENDOR.CDIVISION,lcDivFile),.T.) AND ;
       IIF(llUseStates,SEEK(SUBSTR(APVENDOR.CADDRESS4,1,6),lcStatFile),.T.) AND ;                                        
       IIF(!EMPTY(lcZip),APVENDOR.CADDRESS5=lcZip,.T.) 
        
          IF !EMPTY(ldRpAPEnt) 
            SELECT APINVHDR
            IF gfSeek(APVENDOR.CVENDCODE)
              LOCATE REST WHILE CVENDCODE+CINVNO = APVENDOR.CVENDCODE FOR dinvdate > ldRpAPEnt
              IF !FOUND()
                SELECT APVENDOR
                LOOP 
              ENDIF
            ELSE
              SELECT APVENDOR
              LOOP 
            ENDIF 
          ENDIF  
          llDataToExport = .T.          
          IF ('EB' $ oAriaApplication.CompanySetupModules OR 'NC' $ oAriaApplication.CompanySetupModules)
            *XX
            lcType = 'D'
            IF SEEK('D'+lcPartCode,'EDIACPRT','ACCFACT') OR SEEK('W'+lcPartCode,'EDIACPRT','ACCFACT') 
              lcType = EDIACPRT.TYPE
            ENDIF
            *XX
            *! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS[Start]
            *IF !SEEK('VND'+PADR(APVENDOR.CVENDCODE,40)+'D'+lcPartCode ,'EDITRANS')            
            IF !SEEK('VND'+PADR(APVENDOR.CVENDCODE,40)+lcType +lcPartCode ,'EDITRANS')
            *! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS[End]
              INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER) VALUES ;
                  ('VND',APVENDOR.CVENDCODE,lcType ,lcPartCode)
            ENDIF
            REPLACE cStatus WITH 'N' IN EDITRANS
            =gfAdd_Info('EDITRANS')  
          ENDIF
        *ENDIF 
      *ENDIF
    ENDSCAN 

  ELSE
    SELECT (lcVendorFlt)
    LOCATE
    SCAN
      SELECT APVENDOR
      IF gfSeek(&lcVendorFlt..CVENDCODE,'APVENDOR')
       
         IF IIF(!EMPTY(ldRpAddEdt),(APVENDOR.dAdd_date <= ldRpAddEdt AND  APVENDOR.dedit_date <= ldRpAddEdt),.f.)
           SELECT (lcVendorFlt)
           LOOP 
         ENDIF
         
        IF IIF(llUseDiv,SEEK(APVENDOR.CDIVISION,lcDivFile),.T.) AND ;
           IIF(llUseStates,SEEK(SUBSTR(APVENDOR.CADDRESS4,1,6),lcStatFile),.T.) AND ;                                        
           IIF(!EMPTY(lcZip),APVENDOR.CADDRESS5=lcZip,.T.) 
          
            IF !EMPTY(ldRpAPEnt) 
              SELECT APINVHDR
              IF gfSeek(APVENDOR.CVENDCODE)
                LOCATE REST WHILE CVENDCODE+CINVNO = APVENDOR.CVENDCODE FOR dinvdate > ldRpAPEnt
                IF !FOUND()
                  SELECT (lcVendorFlt)
                  LOOP 
                ENDIF
              ELSE
                SELECT (lcVendorFlt)
                LOOP  
              ENDIF 
            ENDIF  
            llDataToExport = .T.            
            IF ('EB' $ oAriaApplication.CompanySetupModules OR 'NC' $ oAriaApplication.CompanySetupModules)
              *XX
              lcType = 'D'
              IF SEEK('D'+lcPartCode,'EDIACPRT','ACCFACT') OR SEEK('W'+lcPartCode,'EDIACPRT','ACCFACT') 
                lcType = EDIACPRT.TYPE
              ENDIF
              *XX
              *! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS[Start]
              *IF !SEEK('VND'+PADR(APVENDOR.CVENDCODE,40)+'R'+lcPartCode ,'EDITRANS')              
              IF !SEEK('VND'+PADR(APVENDOR.CVENDCODE,40)+lcType +lcPartCode ,'EDITRANS')
              *! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS[End]
                INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER) VALUES ;
                    ('VND',APVENDOR.CVENDCODE,lcType ,lcPartCode )
              ENDIF
              REPLACE cStatus WITH 'N' IN EDITRANS
              =gfAdd_Info('EDITRANS')  
            ENDIF
          ENDIF 
      ENDIF
    ENDSCAN 
  ENDIF
*! E612624,1 MMT 09/11/2022 Export Style Entity to SIIWII[START]
  CASE lcRPEntity ='S'
    lcPartCode = lcRpPartS
    lnStyPos = ASCAN(loOgScroll.laOgFxFlt,"STYLE.STYLE")
    lnStyPos = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnStyPos ,1)
    lcStyFlt = loOgScroll.laOgFxFlt[lnStyPos ,6]
    IF !EMPTY(lcStyFlt) AND USED(lcStyFlt) AND RECCOUNT(lcStyFlt)> 0
      SELECT(lcStyFlt)
      LOCATE 
      IF EOF()
        lcStyFlt = ''
      ENDIF
    ENDIF 
    
    lcSeasonFile = ''
    lcSeasons= ''
    llUseSeasons = .F.
    lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGfxFlt,ASCAN(LOOGSCROLL.laOGfxFlt,'STYLE.SEASON'),1)
    IF lnPosition > 0
      lcSeasons = LOOGSCROLL.laOGfxFlt[lnPosition,6]
      lcSeasonFile = loogscroll.gfTempName()
      llUseSeasons = IIF(LEN(lcSeasons)>0,.T.,.F.) AND lfConvertToCursor(lcSeasons,'SEASON',lcSeasonFile )
    ENDIF
    
    lcStyGroupFile = ''
    lcGroups= ''
    llUseGroups = .F.
    lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGfxFlt,ASCAN(LOOGSCROLL.laOGfxFlt,'STYLE.CSTYGROUP'),1)
    IF lnPosition > 0
      lcGroups = LOOGSCROLL.laOGfxFlt[lnPosition,6]
      lcStyGroupFile = loogscroll.gfTempName()
      llUseGroups = IIF(LEN(lcGroups)>0,.T.,.F.) AND lfConvertToCursor(lcGroups,'CSTYGROUP',lcStyGroupFile)
    ENDIF
    
    lcDivs = ''
    llUseDiv = .F.
    lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(LOOGSCROLL.laOGFXFlt,'CUSTOMER.CDIVISION'),1)
    IF lnPosition > 0
      lcDivs = LOOGSCROLL.laOGFXFlt[lnPosition,6]
      lcDivFile = loogscroll.gfTempName()
      llUseDiv = IIF(LEN(lcDivs)>0,.T.,.F.) AND lfConvertToCursor(lcDivs,'CDIVISION',lcDivFile)
    ENDIF
    
    lnFabricPos = ASCAN(loOgScroll.laOgFxFlt,"STYLE.FABRIC")
    lnFabricPos = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnFabricPos ,1)
    lcFabricFlt = loOgScroll.laOgFxFlt[lnFabricPos ,6]
    IF !EMPTY(lcFabricFlt) AND USED(lcFabricFlt) AND RECCOUNT(lcFabricFlt)> 0
      SELECT(lcFabricFlt)
      LOCATE 
      IF EOF()
        lcFabricFlt = ''
      ENDIF
    ENDIF 

    lnPatternPos = ASCAN(loOgScroll.laOgFxFlt,"STYLE.PATTERN")
    lnPatternPos = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPatternPos ,1)
    lcPattern = loOgScroll.laOgFxFlt[lnPatternPos ,6]
   
  
    lnStatusPos = ASCAN(loOgScroll.laOgFxFlt,"STYLE.STATUS")
    lnStatusPos = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnStatusPos  ,1)
    lcStatuses = loOgScroll.laOgFxFlt[lnStatusPos ,6]
  
    IF !EMPTY(lcStyFlt)
      SELECT (lcStyFlt)
      LOCATE
      SCAN
        SELECT STYLE
       IF gfSeek(&lcStyFlt..KeyExp,'STYLE','CSTYLE') 
         IF IIF(llUseSeasons,SEEK(STYLE.SEASON,lcSeasonFile),.T.) AND;
          IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile ),.T.) AND IIF(llUseGroups ,SEEK(STYLE.CSTYGROUP,lcStyGroupFile),.T.) AND ;
          IIF(!EMPTY(lcFabricFlt),SEEK(STYLE.FABRIC,lcFabricFlt),.T.) AND IIF(!EMPTY(lcPattern),STYLE.PATTERN =lcPattern,.T.) AND ;
          IIF(!EMPTY(lcStatuses),STYLE.STATUS $ lcStatuses,.T.) AND ;
          IIF(!EMPTY(ldRpAddEdt),(STYLE.dAdd_date > ldRpAddEdt OR STYLE.dedit_date > ldRpAddEdt),.T.) 
         llDataToExport = .T.            
         IF ('EB' $ oAriaApplication.CompanySetupModules OR 'NC' $ oAriaApplication.CompanySetupModules)
           *XX
           lcType = 'D'
           IF SEEK('D'+lcPartCode,'EDIACPRT','ACCFACT') OR SEEK('W'+lcPartCode,'EDIACPRT','ACCFACT') 
             lcType = EDIACPRT.TYPE
           ENDIF
           *XX
           *! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS[Start]
           *IF !SEEK('STY'+PADR(STYLE.CSTYMAJOR,40)+'R'+lcPartCode ,'EDITRANS')
           IF !SEEK('STY'+PADR(STYLE.CSTYMAJOR,40)+lcType +lcPartCode ,'EDITRANS')
           *! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS[End]
             INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER) VALUES ;
                  ('STY',STYLE.CSTYMAJOR,lcType ,lcPartCode )
           ENDIF
           REPLACE cStatus WITH 'N' IN EDITRANS
           =gfAdd_Info('EDITRANS')  
         ENDIF 
         ENDIF
        ENDIF
      ENDSCAN
    ELSE
      SELECT STYLE
      =gfSetOrder('CSTYLE')
      =gfSeek('')
      SCAN FOR IIF(llUseSeasons,SEEK(STYLE.SEASON,lcSeasonFile),.T.) AND;
          IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile ),.T.) AND IIF(llUseGroups ,SEEK(STYLE.CSTYGROUP,lcStyGroupFile),.T.) AND ;
          IIF(!EMPTY(lcFabricFlt),SEEK(STYLE.FABRIC,lcFabricFlt),.T.) AND IIF(!EMPTY(lcPattern),STYLE.PATTERN =lcPattern,.T.) AND ;
          IIF(!EMPTY(lcStatuses),STYLE.STATUS $ lcStatuses,.T.) AND ;
          IIF(!EMPTY(ldRpAddEdt),(STYLE.dAdd_date > ldRpAddEdt OR STYLE.dedit_date > ldRpAddEdt),.T.) 
        llDataToExport = .T.            
        IF ('EB' $ oAriaApplication.CompanySetupModules OR 'NC' $ oAriaApplication.CompanySetupModules)
          *XX
          lcType = 'D'
          IF SEEK('D'+lcPartCode,'EDIACPRT','ACCFACT') OR SEEK('W'+lcPartCode,'EDIACPRT','ACCFACT') 
            lcType = EDIACPRT.TYPE
          ENDIF
          *XX
          *! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS[Start]
*         IF !SEEK('STY'+PADR(STYLE.CSTYMAJOR,40)+'R'+lcPartCode ,'EDITRANS')
          IF !SEEK('STY'+PADR(STYLE.CSTYMAJOR,40)+lcType +lcPartCode ,'EDITRANS')
          *! B612657,1 MMT 02/07/2023 Exporting Style duplicates records in EDITRANS[End]
            INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER) VALUES ;
                 ('STY',STYLE.CSTYMAJOR,lcType ,lcPartCode )
          ENDIF
          REPLACE cStatus WITH 'N' IN EDITRANS
          =gfAdd_Info('EDITRANS')  
        ENDIF 
      ENDSCAN 
    ENDIF
*! E612624,1 MMT 09/11/2022 Export Style Entity to SIIWII[End]
ENDCASE 
IF llDataToExport 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Data has been exported successfully.')
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No Data matches the selected criteria.')

ENDIF
RETURN .F.
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 05/17/2022
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName

DO CASE

CASE   ALLTRIM(lcFieldName) = 'REGION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0
CASE   ALLTRIM(lcFieldName) = 'CTERMCODE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CLASS'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'SHIPVIA'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0
  
CASE ALLTRIM(lcFieldName) = 'SPCINST'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0 
CASE ALLTRIM(lcFieldName) = 'STATE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0 
*! E612624,1 MMT 09/11/2022 Export Style Entity to SIIWII[START]
CASE ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0 
CASE ALLTRIM(lcFieldName) = 'CSTYGROUP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0 
*! E612624,1 MMT 09/11/2022 Export Style Entity to SIIWII[End]
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

