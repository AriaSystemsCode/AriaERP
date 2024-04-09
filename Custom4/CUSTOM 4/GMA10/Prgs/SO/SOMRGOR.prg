*!*****************************************************************************************
*! Name      : SOMRGOR.PRG
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 05/04/2015 
*! Purpose   : Merging Orders program for GMA
*! Entry no. : C201672 - [T20150326.0014]
*:************************************************************************
*: Modifications:
*: B611080,1 MMT 11/18/2015 Issues in Custom Sales order merging program[T20151013.0028]
*:************************************************************************
#INCLUDE r:\aria4xp\screens\so\soord.h
lcExpr = gfOpGrid('SOMRGORD' , .T.)&&,.F.,.F.,.T.,.T.)
*!*****************************************************************************************
*! Name      : lfsrOrder
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 05/04/2015 
*! Purpose   : Reset/Set order browser
*!*****************************************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm
IF lcParm = "S"
  SELECT ORDHDR
  LOCATE
ENDIF

*--End of lfsrOrder.
*!**************************************************************************
*! Name      : lfvShipAddress1
*! Developer : MMT - Mariam Mazhar
*! Date      : 05/04/2015
*! Purpose   : Fix the Order Browse customer shipping address 1
*!**************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : Valid Shipping Address 1
*!**************************************************************************
*! Example   : =lfvShipAddress1("MA100","HOB")
*!**************************************************************************
*!
FUNCTION lfvShipAddress1
LPARAMETERS lcAccount, lcStore
LOCAL lcAddress1, lnSelect
lnSelect = SELECT(0)

IF EMPTY(lcStore)
  =SEEK("M"+lcAccount,"CUSTOMER","CUSTOMER")
ELSE
  =SEEK("S"+lcAccount+lcStore,"CUSTOMER","CUSTOMER")
ENDIF
lcAddress1 = Customer.cAddress1

SELECT(lnSelect)
RETURN lcAddress1
ENDFUNC

*!*****************************************************************************************
*! Name      : lfvOrderNO
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 05/04/2015 
*! Purpose   : Validate merge order#
*!*****************************************************************************************
FUNCTION lfvOrderNO
IF !USED('Ordhdr_A')
  =gfOpenTable('Ordhdr','Ordhdr','SH','Ordhdr_A')
ENDIF

IF !EMPTY(lcRpOrder) AND !gfSeek('O'+lcRpOrder,'Ordhdr_A')
  *= ORDBROW(.F., 'O' , @lcRpOrder, .F.,'O')
  xAccount = lcRpAccount
  llSelect = ORDBROWA(XAccount,.T.,'O')
  IF !LLSELECT 
    LCRPORDER = ''
  ELSE
    LCRPORDER = ORDHDR.ORDER
  ENDIF
ENDIF

IF !EMPTY(lcRpOrder) AND gfSeek('O'+lcRpOrder,'Ordhdr_A')
  IF Ordhdr_A.Status = 'C'
    =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Order#'+lcRpOrder+' status is Complete.')
    lcRpOrder = ''
  ENDIF
  IF OrdHdr_A.Status = 'X'
    =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Order#'+lcRpOrder+' status is Cancelled.')
    lcRpOrder = ''
  ENDIF
ENDIF
*!*****************************************************************************************
*! Name      : lfMergData
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 05/04/2015 
*! Purpose   : Merging orders
*!*****************************************************************************************
FUNCTION lfMergData


IF llRpNwOr AND EMPTY(lcRpWrh)
 =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Location is empty. Cannot proceed.')
 RETURN .f.
ENDIF

lcCanReasonCode = ''
lnPosOrdCan= ASCAN(loogScroll.laOGFXFLT,"ORDCANLN.CCANCRESON")
IF lnPosOrdCan > 0
  lnPosOrdCan  = ASUBSCRIPT(loogScroll.laOGFXFLT,lnPosOrdCan,1)
  IF EMPTY(ALLTRIM(loogScroll.laOGFXFLT[lnPosOrdCan ,6])) 
    =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Cancellation Reason is empty. Cannot proceed.')
    RETURN .f.
  ELSE
    lcCanReasonCode = loogScroll.laOGFXFLT[lnPosOrdCan ,6]
  ENDIF
ENDIF

lcCustPO = ''
lnCustPosOrd= ASCAN(loogScroll.laOGFXFLT,"ORDHDR.CUSTPO")
IF lnCustPosOrd > 0
  lnCustPosOrd = ASUBSCRIPT(loogScroll.laOGFXFLT,lnCustPosOrd,1)
  lcCustPO =loogScroll.laOGFXFLT[lnCustPosOrd,6] 
ENDIF

llSeleOrder = .F. 
lcFileOrder = ''
lnOrderCount = 0
lnPosOrd= ASCAN(laOGFXFLT,"ORDHDR.ORDER")
IF lnPosOrd > 0
  lnPosOrd = ASUBSCRIPT(laOGFXFLT,lnPosOrd ,1)
  lcFileOrder =IIF(!EMPTY(laOGFXFLT[lnPosOrd ,6]),laOGFXFLT[lnPosOrd ,6],'')
  IF !EMPTY(lcFileOrder) 
    SELECT (lcFileOrder)
    COUNT FOR !DELETED() TO lnOrderCount
    LOCATE 
    IF !EOF()
      llSeleOrder = .T.
    ENDIF  
  ENDIF
ENDIF
*: B611080,1 MMT 11/18/2015 Issues in Custom Sales order merging program[T20151013.0028][Start]
*IF !llSeleOrder OR (lnOrderCount = 1)
IF !llSeleOrder 
*: B611080,1 MMT 11/18/2015 Issues in Custom Sales order merging program[T20151013.0028][End]
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'No Orders selected to merge. Cannot proceed.')
  RETURN .F.
ENDIF

IF !llRpNwOr AND EMPTY(lcRpOrder)
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'No Orders selected to merge order to. Cannot proceed.')
  RETURN .F.
ENDIF
IF !llRpNwOr AND !EMPTY(lcRpOrder) AND SEEK(lcRpOrder,lcFileOrder)
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'The Order that orders will merged to is selected in the list of the orders to be merged. Cannot proceed.')
  RETURN .F.
ENDIF

IF !USED('Ordhdr_A')
  =gfOpenTable('Ordhdr','Ordhdr','SH','Ordhdr_A')
ENDIF

IF !USED('ORDLINE')
  =gfOpenTable('ORDLINE','ORDLINE','SH')
ENDIF
IF !USED('STYDYE')
  =gfOpenTable(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
ENDIF  
=gfOpenTable(oAriaApplication.DataDir+'WareHous',oAriaApplication.DataDir+'WareHous','SH')
=gfOpenTable(oAriaApplication.DataDir+'Scale',oAriaApplication.DataDir+'Scale','SH')
=gfOpenTable(oAriaApplication.DataDir+'CODES',oAriaApplication.DataDir+'CCODE_NO','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYLE',oAriaApplication.DataDir+'STYLE','SH')
=gfOpenTable(oAriaApplication.DataDir+'CUSTOMER',oAriaApplication.DataDir+'CUSTOMER','SH')
=gfOpenTable(oAriaApplication.DataDir+'SALESREP',oAriaApplication.DataDir+'SALESREP','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDHDR',oAriaApplication.DataDir+'ORDHDR','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDLINE',oAriaApplication.DataDir+'ORDLINE','SH')
=gfOpenTable(oAriaApplication.DataDir+'SYCCURR',oAriaApplication.DataDir+'CCURRCODE','SH')
=gfOpenTable(oAriaApplication.DataDir+'SYCEXCH',oAriaApplication.DataDir+'CURRENCY','SH')
=gfOpenTable(oAriaApplication.DataDir+'SYCINT',oAriaApplication.DataDir+'CCONTCODE','SH')
=gfOpenTable(oAriaApplication.DataDir+'NOTEPAD',oAriaApplication.DataDir+'NOTEPAD','SH')

DIMENSION laAccounts[1]
DIMENSION laStores[1]
*!*	DIMENSION laWareCodes[1]
*!*	DIMENSION laDepart[1]
laAccounts = ''
laStores = ''
*!*	laWareCodes = ''
*!*	laDepart = ''
SELECT (lcFileOrder)
LOCATE
SCAN 
  =gfSeek('O'+&lcFileOrder..Order,'Ordhdr_A','Ordhdr')  
  IF ASCAN(laAccounts ,Ordhdr_A.Account)= 0
    IF EMPTY(laAccounts[1])
      laAccounts[1] = Ordhdr_A.Account
    ELSE
      DIMENSION laAccounts[ALEN(laAccounts,1)+1]  
      laAccounts[ALEN(laAccounts,1)]  = Ordhdr_A.Account
    ENDIF
  ENDIF
  
*!*	  IF ASCAN(laDepart ,Ordhdr_A.Dept)= 0
*!*	    IF EMPTY(laDepart[1])
*!*	      laDepart[1] =Ordhdr_A.Dept
*!*	    ELSE
*!*	      DIMENSION laDepart[ALEN(laDepart,1)+1]  
*!*	      laDepart[ALEN(laDepart,1)]  = Ordhdr_A.Dept
*!*	    ENDIF
*!*	  ENDIF
  
  
  
  IF !EMPTY(Ordhdr_A.Store)
    IF ASCAN(laStores ,Ordhdr_A.Store)= 0
      IF EMPTY(laStores [1])
        laStores [1] = Ordhdr_A.Store
      ELSE
        DIMENSION laStores [ALEN(laStores ,1)+1]  
        laStores [ALEN(laStores,1)]  = Ordhdr_A.Store
      ENDIF
    ENDIF
  ENDIF
*!*	  IF ASCAN(laWareCodes,Ordhdr_A.cWareCode)= 0
*!*	    IF EMPTY(laWareCodes[1])
*!*	      laWareCodes[1] = Ordhdr_A.cWareCode
*!*	    ELSE
*!*	      DIMENSION laWareCodes[ALEN(laWareCodes,1)+1]  
*!*	      laWareCodes[ALEN(laWareCodes,1)]  = Ordhdr_A.cWareCode
*!*	    ENDIF
*!*	  ENDIF
ENDSCAN

IF ALEN(laAccounts,1) > 1
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'The selected orders to be merged are for different accounts. Cannot proceed.')
  RETURN .F.
ENDIF


*!*	IF ALEN(laDepart,1)>1
*!*	  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'The selected orders to be merged are for different Departments. Cannot proceed.')
*!*	  RETURN .F.
*!*	ENDIF

IF ALEN(laStores ,1) > 1
*!*	  IF llRpNwOr
*!*	    IF gfModalgen("QRM00000B00006","DIALOG",.F.,.F.,'The selected orders to be merged are for different Stores. Do you want to create a multi store order.') = 2
*!*	      RETURN .F.
*!*	    ENDIF
*!*	  ELSE
*!*	    IF !llRpNWOR AND gfSeek('O'+lcRpOrder,'Ordhdr_A','Ordhdr') AND Ordhdr_A.Multi <> 'Y'
      =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'The selected orders to be merged have different stores. Cannot proceed.')
      RETURN .F.
*!*	    ENDIF
*!*	  ENDIF  
ENDIF

*!*	IF ALEN(laWareCodes,1) > 1
*!*	  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'The selected orders to be merged are for different Locations. Cannot proceed.')
*!*	  RETURN .F.
*!*	ENDIF

IF !EMPTY(lcRpAccount) AND laAccounts[1] <> lcRpAccount
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'The selected orders to be merged account is different than the selected account. Cannot proceed.')
  RETURN .F.
ENDIF

IF !EMPTY(LCRPORDER) AND  gfSeek('O'+LCRPORDER,'Ordhdr_A','Ordhdr') AND !EMPTY(lcRpAccount) AND Ordhdr_A.Account <> lcRpAccount
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'The selected order to merge orders to account is different than the selected account. Cannot proceed.')
  RETURN .F.
ENDIF

IF !EMPTY(LCRPORDER) AND  gfSeek('O'+LCRPORDER,'Ordhdr_A','Ordhdr') AND !EMPTY(laAccounts[1]) AND Ordhdr_A.Account <> laAccounts[1] 
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'The selected orders to be merged account is different than the selected order account. Cannot proceed.')
  RETURN .F.
ENDIF

IF USED(lcTempHdr)
  USE IN (lcTempHdr)
ENDIF

SELECT 'Ordhdr_A'
lnOrderhdr = AFIELDS(laOrdHdrStr)
= gfCrtTmp(lcTempHdr, @laOrdHdrStr, 'Order', lcTempHdr)
= gfCrtTmp(lcTmpOrdHdr , @laOrdHdrStr, 'Order', lcTmpOrdHdr)

IF USED(lcTempLine)
  USE IN (lcTempLine)
ENDIF
SELECT ORDLINE
lnOrderLines = AFIELDS(laOrdlineStr)
= gfCrtTmp(lcTempLn, @laOrdlineStr, 'STYLE+STORE', lcTempLn)
= gfCrtTmp(lcTempLine, @laOrdlineStr, 'STYLE+STORE+Order', lcTempLine)
= gfCrtTmp(lcConfLn, @laOrdlineStr, 'STYLE+STORE+Order', lcConfLn)


SELECT (lcTempLn)
INDEX ON cOrdType+ORDER+STR(LINENo,6) TAG 'OrdLine' ADDITIVE 
IF 'AL' $ oAriaApplication.CompanyInstalledModules
  =gfOpenTable('PIKTKT','ORDPIK','SH','PIKTKT_A')
ENDIF  

SELECT (lcFileOrder)
LOCATE
SCAN 
  IF gfSeek('O'+&lcFileOrder..Order,'Ordhdr_A','Ordhdr')
    IF Ordhdr_A.Status $ 'OH'
	  llIncOrder = .T.
	  IF 'AL' $ oAriaApplication.CompanyInstalledModules
	    *-- Do not cancel picked orders
	    IF SEEK(Ordhdr_A.ORDER,'PIKTKT_A','ORDPIK')
	      *-- check Piktkt status for picked lines mesg.
	      SELECT PIKTKT_A
	      LOCATE REST WHILE ORDER = Ordhdr_A.ORDER FOR STATUS $ 'OHP'
	      IF FOUND()
	        llIncOrder = .F.
	      ENDIF
	    ENDIF   
	  ENDIF  
	  SELECT (lcFileOrder)
	  IF !llIncOrder 
	    LOOP 
	  ENDIF
      SELECT Ordhdr_A
      SCATTER MEMO MEMVAR 
      IF !SEEK(m.Order,lcTempHdr,lcTempHdr)
        INSERT INTO (lcTempHdr) FROM MEMVAR 
      ENDIF  
    ENDIF  
  ELSE
    LOOP   
  ENDIF
  
    
  IF gfSeek('O'+&lcFileOrder..Order,'Ordline','Ordline')
    SELECT ORDLINE
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6)='O'+&lcFileOrder..Order FOR !PICKED AND TOTQTY > 0 AND ;
         (PIK1=0 AND PIK2=0 AND PIK3=0 AND PIK4=0 AND PIK5=0 AND PIK6=0 AND PIK7=0 AND PIK8=0 )
      SCATTER MEMO MEMVAR 
      INSERT INTO (lcTempLine) FROM MEMVAR
    ENDSCAN 
  ENDIF  
ENDSCAN 


SELECT (lcTempLine)
LOCATE 
IF EOF()
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Could not find lines.')
  RETURN .F.
ENDIF

SELECT DISTINC CDIVISION FROM  (lcTempHdr) INTO ARRAY laDivision
IF ALEN(laDivision,1) > 1 
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'The selected orders to be merged have different Divisions. Cannot proceed.')
  RETURN .F.
ENDIF

IF !llRpNwOr AND !EMPTY(lcRpOrder) AND ALEN(laDivision,1)=1 
  =gfSeek("O"+lcRpOrder,'Ordhdr_A','ORDHDR')
  IF Ordhdr_A.CDIVISION <> laDivision[1]
    =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,;
    'The selected orders to be merged division is not matching the division of order that will be mergerd orders to. Cannot proceed.')
    RETURN .F.
  ENDIF  
ENDIF 


*!*	IF !llRpNwOr AND !EMPTY(lcRpOrder) AND ALEN(laWareCodes,1)=1 
*!*	  =gfSeek("O"+lcRpOrder,'Ordhdr_A','ORDHDR')
*!*	  IF Ordhdr_A.cWareCode <> laWareCodes[1]
*!*	    =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,;
*!*	    'The selected orders to be merged Ship From Address is not matching the Ship From Address of order that will be mergerd orders to. Cannot proceed.')
*!*	    RETURN .F.
*!*	  ENDIF  
*!*	ENDIF 
** Different seasons
llSeaonAll = .F.
SELECT DISTINC Season FROM  (lcTempHdr) WHERE Season<> '*' INTO ARRAY laSeason
IF _TALLY > 1 
  IF llRpNwOr 
    IF gfModalgen("QRM00000B00006","DIALOG",.F.,.F.,'The selected orders to be merged have different Seasons. Do you want to create a new order for Seaosn ALL?') = 2
      RETURN .F.
    ELSE
      llSeaonAll = .T.
    ENDIF  
  ELSE 
    IF !EMPTY(lcRpOrder) AND gfSeek("O"+lcRpOrder,'Ordhdr_A','ORDHDR') AND Ordhdr_A.Season <> '*'
      IF gfModalgen("QRM00000B00006","DIALOG",.F.,.F.,'The selected orders to be merged have different Seasons. Do you want to set order# '+lcRpOrder+' season to ALL?')= 2
        RETURN .F.
      ELSE
        llSeaonAll = .T.
      ENDIF  
    ENDIF  
  ENDIF  
ENDIF



IF !llRpNwOr AND !EMPTY(lcRpOrder) AND TYPE('laSeason') <> 'U' AND ALEN(laSeason,1)=1 
  =gfSeek("O"+lcRpOrder,'Ordhdr_A','ORDHDR')
  IF Ordhdr_A.Season <> laSeason[1]
    =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,;
    'The selected orders to be merged season is not matching the season of order that will be mergerd orders to. Cannot proceed.')
    RETURN .F.
  ENDIF  
ENDIF 


=gfOpenTable(oAriaApplication.DataDir+'BomVar',oAriaApplication.DataDir+'BomVar','SH')
lcT_BomVar = gfTempName()
SELECT BomVar
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]
laFileStru[lnFileStru+1,1] = 'nRecno'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'cStatus'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
FOR lnLoop = 1 to  2
   STORE ' ' TO  laFileStru[lnFileStru+lnLoop,7],laFileStru[lnFileStru+lnLoop,8],;
            laFileStru[lnFileStru+lnLoop,9],laFileStru[lnFileStru+lnLoop,10],;
            laFileStru[lnFileStru+lnLoop,11],laFileStru[lnFileStru+lnLoop,12],;
            laFileStru[lnFileStru+lnLoop,13],laFileStru[lnFileStru+lnLoop,14],;
            laFileStru[lnFileStru+lnLoop,15],laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO    laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
ENDFOR
IF USED(lcT_BomVar)
  ZAP IN (lcT_BomVar)
ELSE
 =gfCrtTmp(lcT_BomVar,@laFileStru,[cIdType+cCost_Id+STR(LineNo,6)],lcT_BomVar)
ENDIF


CREATE CURSOR 'Style_Problem'(Style C(19),Store C(8), ProblemCode C(6))
SELECT  'Style_Problem'
INDEX ON STYLE TAG 'Problems'
SELECT (lcTempLine)
*WHERE IIF(!llRPNwOr,!gfSeek('O'+lcRpOrder+&lcTempLine..Store+&lcTempLine..Style,'ORDLINE','ORDLINST'),.T.)
SELECT DISTINCT STYLE,Store FROM (lcTempLine) INTO CURSOR 'StylesMerge'
SELECT 'StylesMerge'
LOCATE 
*ORDLINST   && CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
SCAN 
  SELECT Distinct gros_price FROM (lcTempLine) WHERE Style = StylesMerge.Style AND Store =StylesMerge.Store   INTO ARRAY laGRSPrices
  IF ALEN(laGRSPrices,1) > 1
    INSERT INTO 'Style_Problem' VALUES (StylesMerge.Style,StylesMerge.Store,"GPRICE")
  ENDIF 
  SELECT Distinct nSugRetPri FROM (lcTempLine) WHERE Style = StylesMerge.Style  AND Store =StylesMerge.Store   INTO ARRAY laRetail
  IF ALEN(laRetail,1) > 1
    INSERT INTO 'Style_Problem' VALUES (StylesMerge.Style,StylesMerge.Store,"Retail")
  ENDIF 
  SELECT Distinct price FROM (lcTempLine) WHERE Style = StylesMerge.Style  AND Store =StylesMerge.Store   INTO ARRAY laPrices
  IF ALEN(laPrices,1) > 1
    INSERT INTO 'Style_Problem' VALUES (StylesMerge.Style,StylesMerge.Store,"PRICES")
  ENDIF 
*!*	  SELECT DISTINCT disc_pcnt FROM (LCTEMPLINE) WHERE STYLE = STYLESMERGE.STYLE INTO ARRAY LADisc
*!*	  IF ALEN(LADisc,1) > 1
*!*	    INSERT INTO 'STYLE_PROBLEM' VALUES (STYLESMERGE.STYLE,"DISCNT")
*!*	  ENDIF 
*!*	  SELECT Distinct Complete  FROM (lcTempLine) WHERE Style = StylesMerge.Style INTO ARRAY laCompletes
*!*	  IF ALEN(laCompletes,1) > 1
*!*	    INSERT INTO 'Style_Problem' VALUES (StylesMerge.Style,"COMPLT")
*!*	  ENDIF 
ENDSCAN 
IF RECCOUNT('Style_Problem') != 0
  SELECT 'Style_Problem'
  LOCATE 
*!*	  LCDELESET =SET("DELETED") 
*!*	  SET DELETED OFF 
  SCAN 
    =SEEK(Style_Problem.Style+Style_Problem.Store,lcTempLine)
*!*	    SCAN REST WHILE Style+order = Style_Problem.Style
*!*	      SCATTER MEMO MEMVAR
*!*	      DELETE 
    IF !SEEK(Style_Problem.Style+Style_Problem.Store,lcConfLn)
      =gfSeek(Style_Problem.Style,'STYLE','STYLE')
      INSERT INTO (lcConfLn) (Style,Store,gros_price ,Price,disc_pcnt,nSugRetPri) VALUES (Style_Problem.Style,Style_Problem.Store,;
                              &lcTempLine..gros_price,&lcTempLine..Price,&lcTempLine..disc_pcnt,&lcTempLine..nSugRetPri)
    ENDIF  
*!*	    ENDSCAN 
    SELECT 'Style_Problem'
  ENDSCAN
*  SET DELETED &lcDeleSet.
  SELECT (lcConfLn)
  SET RELATION TO Style+Store INTO (lcTempLine)
  *SET SKIP TO (lcTempLine)
  LOCATE 
  llResume = .F.
  DO FORM (oAriaApplication.ScreenHome+'SO\somrgoL.scx') WITH lcConfLn,lcTempLine
  IF llResume 
    SELECT (lcConfLn)
    SET RELATION OFF INTO (lcTempLine)
    LOCATE 
    SCAN 
      SELECT (lcTempLine)
      =SEEK(&lcConfLn..Style+&lcConfLn..Store)
      SCAN REST WHILE STYLE+STORE+Order = &lcConfLn..Style+&lcConfLn..Store
        REPLACE &lcTempLine..gros_price WITH &lcConfLn..gros_price,;
                &lcTempLine..Price WITH &lcConfLn..Price ,;
                &lcTempLine..disc_pcnt WITH &lcConfLn..disc_pcnt,;
                &lcTempLine..nSugRetPri WITH &lcConfLn..nSugRetPri
      ENDSCAN 
      SELECT (lcConfLn)
    ENDSCAN
  ELSE
    RETURN .F.
  ENDIF
ENDIF  
IF .T.
  lnLineNo = 0
  IF llRpNwOr
    lnLineNo = 0
  ELSE
    =gfSeek('O'+lcRpOrder,'Ordhdr_A')  
    lnLineNo = Ordhdr_A.lastline + 1
    lcWareCode =  Ordhdr_A.cWareCode
    =gfSeek('O'+lcRpOrder,'ORDLINE','ORDLINE')  
    SELECT ORDLINE
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) =  'O'+lcRpOrder
      SCATTER MEMO MEMVAR 
      INSERT INTO (lcTempLn) FROM MEMVAR 
    ENDSCAN
  ENDIF
  SELECT (lcTempLn)
  SET ORDER TO (lcTempLn)
  SELECT (lcTempLine)  
  LOCATE 
  SCAN
    SCATTER MEMO MEMVAR 
    llAddToLine = .F.
    IF SEEK(m.Style+m.Store,lcTempLn,lcTempLn)
      SELECT (lcTempLn)
      LOCATE REST WHILE Style+Store = m.Style+m.Store FOR gros_price = m.gros_price AND nSugRetPri = m.nSugRetPri AND price = m.price
      IF FOUND()
        llAddToLine = .T.
      ENDIF
    ENDIF
    IF !llAddToLine &&!SEEK(m.Style+m.Store,lcTempLn,lcTempLn)
      m.ppqty = 0
   	  m.prepak = ''
      lnLineNo = lnLineNo + 1
      m.LineNo = lnLineNo 
      m.cWareCode = IIF(llRpNwOr,lcRpWrh,lcWareCode)
      m.Order = IIF(llRpNwOr,SPACE(6),lcRpOrder)
      m.CustPO = IIF(llRpNwOr,lcCustPO,Ordhdr_A.CustPO)
      FOR lnX = 1 TO 8 
        lcX = STR(lnX,1)
        m.BOOK&lcX. = m.Qty&lcX.
        m.Pik&lcX. = 0
      ENDFOR   
      m.TOTQTY = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
      m.totbook = m.TOTQTY 
      m.TotPik = 0
      m.Piktkt = ''
      m.PikDate ={}
      m.Flag       = 'N' 
      m.Picked = .F.
      m.pack_id =''
      m.lrange =.F.
      m.cpcksize= ''
      m.cpkcolor=''
      m.cpkversion=''
      m.npackno =0
      m.npkslprice =0
      m.Complete = IIF(llRpNwOr,ldRpCompDat,Ordhdr_A.Complete)
      m.Start = IIF(llRpNwOr,ldRpStartDat,Ordhdr_A.Start)
      INSERT INTO (lcTempLn) FROM MEMVAR
      =gfAdd_Info(lcTempLn) 
    ELSE
      SELECT (lcTempLn) 
      FOR lnX = 1 TO 8 
        lcX = STR(lnX,1)
        REPLACE Qty&lcX. WITH Qty&lcX.+&lcTempLine..Qty&lcX.
        REPLACE BOOK&lcX. WITH BOOK&lcX.+&lcTempLine..Qty&lcX.
      ENDFOR 
      REPLACE TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
              totbook with TOTQTY ,;
              Flag  WITH IIF(Flag ='N',Flag  ,'M')
    ENDIF
  ENDSCAN
  SELECT SUM(totBook) As Book,SUM(totQty) As Open ,SUM(TotQty * Price) As OpenAmt,SUM(TotBook * Price) as BookAmnt FROM (lcTempLn) INTO CURSOR 'totals'
  SELECT Distinct Store FROM (lcTempLn)  INTO CURSOR 'Stores'
  llMutilStore = .F.
  lcMainStore = ''
  IF RECCOUNT('Stores') > 1
    llMutilStore = .T.
  ENDIF
  IF RECCOUNT('Stores') = 1
    llMutilStore = .F.
    lcMainStore  = Stores.Store
  ENDIF
  IF llRpNwOr
    =gfSeek('M'+lcRpAccount,'Customer','Customer')
    SELECT (lcTempHdr)
    LOCATE
    SCATTER MEMO MEMVAR 
    m.cwarecode = lcRpWrh
    IF llSeaonAll
      m.Season ='*'
    ENDIF
    *m.Dept = laDepart[1]
    m.Order = SPACE(6)
    m.multi = IIF(llMutilStore,'Y','N')
    m.Store = lcMainStore 
    m.CustPO = lcCustPO
    m.Book  = totals.Book  
    m.Open  = totals.Open 
    m.OpenAmt = totals.OpenAmt
    m.BookAmt =totals.BookAmnt
    m.Ship = 0
    m.ShipAmt = 0
    m.Cancel = 0
    m.CancelAmt = 0
    m.Entered = oAriaApplication.SystemDate
    m.Complete = ldRpCompDat
    m.Start = ldRpStartDat
    *****
    m.Comm1 = Customer.Comm
    m.Comm2 = Customer.Comm2
    m.cFacCode  = Customer.cFacCode
    m.ShipVia   = Customer.ShipVia
    m.cTermCode = Customer.cTermCode
    m.SpcInst   = Customer.SpcInst
    m.Disc      = Customer.Disc 
    m.Bulk      = 'Y'
    m.Dept       = lcRpDept 
    m.Rep1      = Customer.SalesRep
    m.Rep2      = Customer.Rep2 
    m.Note1     = Customer.Note 
    m.Note2     = '' 
    m.Buyer     = Customer.Buyer 
    m.Phone     = Customer.Phone1
    m.cCurrCode = Customer.cCurrCode
    m.Priority  = Customer.Priority
    m.Status    = 'O'
    IF !EMPTY(&lcTempHdr..Store) AND !llMutilStore
      =gfSeek('S'+lcRpAccount+&lcTempHdr..Store,'Customer','Customer')    
      m.Disc      = Customer.Disc 
	  m.Rep1      = Customer.SalesRep
	  m.Rep2      = Customer.Rep2 
	  m.Note1     = Customer.Note 
      m.Comm1 = Customer.Comm
      m.Comm2 = Customer.Comm2      
      m.ShipVia   = Customer.ShipVia
    ENDIF

    *****
    INSERT INTO (lcTmpOrdHdr) FROM MEMVAR
    =gfAdd_Info(lcTmpOrdHdr) 
  ELSE
   IF gfSeek('O'+lcRpOrder,'Ordhdr_A','Ordhdr')    
     SELECT Ordhdr_A
     SCATTER MEMO MEMVAR 
     IF llSeaonAll
       m.Season ='*'
     ENDIF
     m.lastline = lnLineNo   
     m.Book  = totals.Book  
     m.Open  = totals.Open 
     m.OpenAmt = totals.OpenAmt
     m.BookAmt = totals.BookAmnt
     m.Bulk      = 'Y'
     INSERT INTO (lcTmpOrdHdr) FROM MEMVAR
     =gfAdd_Info(lcTmpOrdHdr) 
   ENDIF
  ENDIF  
  LOFORMSET = CREATEOBJECT('Custom')
  LOFORMSET.AddProperty ('lcdeposittemp')
  LOFORMSET.AddProperty ('lcWareCode')
  LOFORMSET.AddProperty ('llUpdate')
  LOFORMSET.AddProperty ('laEvntTrig [1]')
  LOFORMSET.AddProperty ('ACTIVEMODE')
  LOFORMSET.laEvntTrig [1] = PADR('SOIMPRO',10)

  
  LOFORMSET.ACTIVEMODE = 'A'
  LOFORMSET.lcdeposittemp = gfTempName()
  lcOrdCanLn=.F.
  IF !llRpNwOr
    lcOrdCanLn= gfTempName()
    =gfOpenTable('ORDCANLN','ORDCANLN','SH',lcOrdCanLn)
    =gfOpenTable('ORDDSGN','DESIGN')  
  ENDIF
  
  IF !llRpNwOr
    = gfSeek('O'+lcRpOrder,'Ordhdr','Ordhdr')  
  ENDIF
  
  SELECT (lcTempLn)
  SET ORDER TO 'OrdLine' 
  *: B611080,1 MMT 11/18/2015 Issues in Custom Sales order merging program[T20151013.0028][Start]
  LOFORMSET.llUpdate =.t.
  *: B611080,1 MMT 11/18/2015 Issues in Custom Sales order merging program[T20151013.0028][End]
  DO lfSavScr IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') ;
  WITH .F., IIF(llRpNwOr,'A','E'), lcTmpOrdHdr,lcTempLn,IIF(llRpNwOr,.F.,gfTempName()),lcOrdCanLn,lcT_BomVar,LOFORMSET
  lcNewOrderNum = Ordhdr.Order
  *: B611080,1 MMT 11/18/2015 Issues in Custom Sales order merging program[T20151013.0028][Start]
  IF !LOFORMSET.llUpdate 
    RETURN .F. 
  ENDIF
  *: B611080,1 MMT 11/18/2015 Issues in Custom Sales order merging program[T20151013.0028][End]
  *MT Update CutPick
  IF !USED('CutPick_UP')
    =gfOpenTable('CutPick','CUTORD','SH','CutPick_UP')
  ENDIF
  SELECT (lcTempHdr)
  LOCATE 
  SCAN 
    IF gfSeek('1'+&lcTempHdr..Order,'CutPick_UP','CUTORD') OR gfSeek('2'+&lcTempHdr..Order,'CutPick_UP','CUTORD') 
      SELECT 'CutPick_UP'
      =gfSqlrun("Update CUTPICK Set [Order] = '"+lcNewOrderNum+"' Where [Order] = '"+&lcTempHdr..Order+"'",'CutPick')
    ENDIF
    SELECT (lcTempHdr)
  ENDSCAN
  *MT
  
  m.mnotes = ""
  IF !llRpNwOr AND gfSeek ('B'+lcRpOrder,'NotePad')
    m.mnotes= NotePad.mnotes+ CHR(13)+ Chr(10) 
  ENDIF

  SELECT (lcTempHdr)
  LOCATE 
  lcMergedOrders = ''
  SCAN 
    IF gfSeek ('B'+&lcTempHdr..Order,'NotePad')
      m.mnotes= m.mnotes+  NotePad.mnotes+ CHR(13)+ Chr(10) 
      REPLACE NotePad.mnotes WITH "Merged to Sales Order#:"+ lcNewOrderNum + CHR(13)+ Chr(10) +NotePad.mnotes
    ELSE
      INSERT INTO 'NotePad' (key,Type,CDESC,mnotes )Values(&lcTempHdr..Order,'B','Notes For Order Number :' +&lcTempHdr..Order,"Merged to Sales Order#:"+ lcNewOrderNum + CHR(13)+ Chr(10))
    ENDIF
    = gfSeek(&lcTempHdr..cOrdType+&lcTempHdr..Order,'ORDHDR','ORDHDR')
    lcMergedOrders = lcMergedOrders + &lcTempHdr..Order + ","
    *DO lfDelScr WITH .T.,LOFORMSET  IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP')
    lfCancelOrder(.F.,LOFORMSET)
    SELECT (lcTempHdr)
  ENDSCAN  
  IF !EMPTY(m.mnotes)
    IF llRpNwOr OR (!llRpNwOr AND !gfSeek ('B'+lcRpOrder,'NotePad'))
      m.Key = IIF(llRpNwOr,lcNewOrderNum,lcRpOrder)
      m.Type = 'B'
      m.cDesc = 'Notes For Order Number :' +IIF(llRpNwOr,lcNewOrderNum ,lcRpOrder)
      INSERT INTO 'NotePad' FROM MEMVAR 
    ELSE
      IF !llRpNwOr AND  gfSeek ('B'+lcRpOrder,'NotePad')  
        SELECT NotePad
        REPLACE mnotes WITH m.mnotes
      ENDIF
    ENDIF  
  ENDIF  
  lfSavefiles()
  =gfModalgen("INM00000B00000","DIALOG",.F.,.F.,'Orders:'+SUBSTR(lcMergedOrders,1,LEN(lcMergedOrders)-1) +' are merged to Order#'+lcNewOrderNum)
ENDIF


*DIMENSION la
*!*	llDateStartSel = .F. && flag to indicate if user Selected date range or not
*!*	lnPosStartDate = ASCAN(laOGFXFLT,"ORDHDR.START")
*!*	ldStartEnd = {}
*!*	ldStartStart = {}
*!*	IF lnPosStartDate  > 0
*!*	  lnPosStartDate = ASUBSCRIPT(laOGFXFLT,lnPosStartDate ,1)
*!*	  lcStartDate =IIF(!EMPTY(laOGFXFLT[lnPosStartDate ,6]),laOGFXFLT[lnPosStartDate ,6],'')
*!*	  IF !EMPTY(lcStartDate)
*!*	    llDateStartSel = .T.
*!*	    ldStartEnd =CTOD(SUBSTR(lcStartDate,ATC('|',lnPosStartDate)+1))
*!*	    ldStartStart = CTOD(SUBSTR(lcStartDate,1,ATC('|',lnPosStartDate)-1))
*!*	  ENDIF
*!*	ENDIF

*!*	llDateCompSel = .F. && flag to indicate if user Selected date range or not
*!*	lnPosCompDate = ASCAN(laOGFXFLT,"ORDHDR.COMPLETE")
*!*	ldComplEnd = {}
*!*	ldComplStart = {}
*!*	IF lnPosCompDate  > 0
*!*	  lnPosCompDate= ASUBSCRIPT(laOGFXFLT,lnPosCompDate,1)
*!*	  lcCOmpDate =IIF(!EMPTY(laOGFXFLT[lnPosCompDate,6]),laOGFXFLT[lnPosCompDate,6],'')
*!*	  IF !EMPTY(lcCOmpDate)
*!*	    llDateCompSel= .T.
*!*	    ldComplEnd = CTOD(SUBSTR(lcCOmpDate,ATC('|',lnPosCompDate)+1))
*!*	    ldComplStart = CTOD(SUBSTR(lcCOmpDate,1,ATC('|',lnPosCompDate)-1))
*!*	  ENDIF
*!*	ENDIF


*!*	llDateEnteredSel = .F. && flag to indicate if user Selected date range or not
*!*	lnPosEnteredDate = ASCAN(laOGFXFLT,"ORDHDR.ENTERED")
*!*	ldEntredEnd = {}
*!*	ldEnteredStart = {}
*!*	IF lnPosEnteredDate > 0
*!*	  lnPosEnteredDate = ASUBSCRIPT(laOGFXFLT,lnPosEnteredDate,1)
*!*	  lcEntredDate =IIF(!EMPTY(laOGFXFLT[lnPosEnteredDate,6]),laOGFXFLT[lnPosEnteredDate,6],'')
*!*	  IF !EMPTY(lcEntredDate)
*!*	    llDateEnteredSel = .T.
*!*	    ldEntredEnd = CTOD(SUBSTR(lcEntredDate,ATC('|',lnPosEnteredDate )+1))
*!*	    ldEnteredStart = CTOD(SUBSTR(lcEntredDate,1,ATC('|',lnPosEnteredDate )-1))
*!*	  ENDIF
*!*	ENDIF

*!*****************************************************************************************
*! Name      : lfvAccount
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 05/04/2015 
*! Purpose   : Validate Account
*!*****************************************************************************************
FUNCTION lfvAccount
xAccount = lcRpAccount
IF !USED('Customer_A')
  =gfOpenTable('Customer','Customer','SH','Customer_A')
ENDIF

IF (!EMPTY(xAccount) .AND. !gfSEEK('M'+xAccount,'Customer_A')) 
  SELECT CUSTOMER
  DO CUSBROWM WITH xAccount
ENDIF
lcRpAccount = xAccount
IF gfSEEK('M'+xAccount,'Customer_A') AND Customer_A.Status <> 'A'
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Account '+xAccount+' status is not Active.')
  lcRpAccount = ''
ENDIF

*!*************************************************************
*! Name      : lfSavefiles
*: Developer     : Mariam Mazhar- [MMT]
*: Date          : 07/09/2007.
*! Purpose   : Function to save 
*!*************************************************************
FUNCTION lfSavefiles
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
   RETURN .F.
ENDIF
lnUpdated = 0
lnAryLen = ALEN(oAriaApplication.laRemoteTable)
FOR lnCounter=1 TO lnAryLen
  IF oAriaApplication.laRemoteTable[lnCounter].lnDataSession == SET("Datasession") 
    IF !oAriaApplication.laRemoteTable[lnCounter].TableUpdate(lcTranCode)
      lnUpdated=lnCounter
      exit
    ENDIF
  ENDIF
NEXT
IF lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  MESSAGEBOX('Saving Process is Rolled Back')
  ThisFormSet.Undo()
  RETURN
ELSE
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ENDIF
*E039415,1 ASM [End]


*!*************************************************************
*! Name      : lfCancelOrder
*! Developer : Wael Aly Mohamed
*! Date      : 01/08/2002
*! Purpose   : Cancel/Uncancel order
*!*************************************************************
FUNCTION lfCancelOrder
PARAMETERS llFromEDI,loFormSet

LOCAL lnRePro
lnRePro = SET("Reprocess")
SET REPROCESS TO -1
llFromEDI = IIF(TYPE('llFromEDI') = 'L' , llFromEDI , .F.)

=gfOpenFile(oAriaApplication.DataDir+'OrdHdr',oAriaApplication.DataDir+'ORDHDR','SH','BlkOrdHd',.T.)
=SEEK(OrdHdr.cOrdType+OrdHdr.cFromOrder,'BlkOrdHd')
=gfOpenFile(oAriaApplication.DataDir+'OrdLine',oAriaApplication.DataDir+'OrdLine','SH','BlkOrdLine',.T.)

IF !llFromEDI AND OrdHdr.cOrdType='O' AND 'EB' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT'  ,'SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDIPD'   ,oAriaApplication.DataDir+'PARTTRANS','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY'  ,'SH')

  *-- Add new 865 record in the EDI transaction file to be sent
  IF SEEK('A'+OrdHdr.Account,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'865','EDIPD')
    IF !SEEK('865'+PADR(OrdHdr.ORDER,40)+'A'+OrdHdr.Account,'EDITRANS')
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,KEY,TYPE,cPartner) VALUES ;
        ('865',OrdHdr.ORDER,'A',OrdHdr.Account)
    ENDIF
    REPLACE cStatus WITH 'N' IN EDITRANS
    =gfAdd_Info('EDITRANS')
  ENDIF
ENDIF

IF OrdHdr.cOrdType='O'
  *Styhist File update
  STORE 0 TO lnMajorLen , lnClrPos, lnClrLen
  lfGetClrD()
  lnMajorLen = LEN(gfItemMask("PM"))

  IF !USED('STYHIST')
    =gfOpenTable('STYHIST','STYHIST','SH')
  ENDIF
  IF !USED('UNIFORM')
    =gfOpenTable('UNIFORM','UNIFORM','SH')
  ENDIF
  IF !USED('Contact_A')
    =gfOpenTable('Contact','Contact','SH','Contact_A')
  ENDIF
ENDIF

*-- Get Fiscal Year and period of order entered date
STORE '' TO lcGlYear,lcGlPeriod
IF OrdHdr.cOrdType='O' .AND. !CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod','',.T.)
  RETURN
ENDIF
*-- Cancel/Uncancel web order, Check the CRM setup
IF OrdHdr.lFromWeb  AND 'CR' $ oAriaApplication.CompanyInstalledModules
  lcConfMail = gfGetMemVar('M_CONFMAIL',oAriaApplication.ActiveCompanyID)
  lcSOAPR    = gfGetMemVar('M_SOAPR',oAriaApplication.ActiveCompanyID)
  lcSOCAN    = gfGetMemVar('M_SOCAN',oAriaApplication.ActiveCompanyID)

  IF EMPTY(lcConfMail)
    *-- Message : 32090
    *-- Text    : The Confirmation E-mail Address on the setup screen is blank.
    *--           We can not save your order.
    *-- Button  : 00000
    *--           <Ok>
    =gfModalGen('TRM32090B00000','DIALOG')
    SET REPROCESS TO lnRePro
    RETURN
  ENDIF
  * Uncancel Web Order
  IF OrdHdr.STATUS = 'X'
    * If the User approved(Open/Hold) the Web Order
    IF EMPTY(lcSOAPR)
      *-- Message : 32091
      *-- Text    : The Approved ID on the setup screen is blank. Are you sure you want to continue?
      *-- Button  : 32000
      *--           <Yes> <No>
      IF gfModalGen('QRM32091B32000','DIALOG') =2
        SET REPROCESS TO lnRePro
        RETURN
      ENDIF
    ENDIF
  ELSE
    *-- Check Order Cancelation Id
    IF EMPTY(lcSOCAN)
      *-- Message : 32092
      *-- Text    : The Cancelled ID on the setup screen is blank. Are you sure you want to continue?
      *-- Button  : 32000
      *--          <Yes> <No>
      IF gfModalGen('QRM32092B32000','DIALOG') = 2
        SET REPROCESS TO lnRePro
        RETURN
      ENDIF
    ENDIF
  ENDIF
ENDIF
lcUntSin = '/'
lcExRSin = gfGetExSin(@lcUntSin, OrdHdr.cCurrCode)
*-- Open the order cancellation table if we have shipped qty.
=gfOpenFile(oAriaApplication.DataDir+'ORDCANLN',oAriaApplication.DataDir+'ORDCANLN','SH')
*-- Uncancel order
  *-- Cancel order
  IF ASCAN(oAriaApplication.laEvntTrig , PADR('DLEALOSO',10)) <> 0
    IF !gfDoTriger('SOORD',PADR('DLEALOSO',10))
      SET REPROCESS TO lnRePro
      RETURN
    ENDIF
  ENDIF
  *-- Do not cancel shipped complete order
  IF OrdHdr.STATUS='C'
    *-- Message : 32003
    *-- Order has been shipped complete! Cannot cancel.
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32003B00000','ALERT')
    SET REPROCESS TO lnRePro
    RETURN
  ENDIF
  *-- Do not cancel order with no lines
  IF !(OrdHdr.cOrdType='T' AND OrdHdr.Mon_Flg='L') AND !SEEK(OrdHdr.cOrdType+OrdHdr.ORDER,'ORDLINE')
    *-- Message : 32000
    *-- The lines for this order are missing! Cannot update cut & sold.
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32000B00000','ALERT')
    SET REPROCESS TO lnRePro
    RETURN
  ENDIF
  *-- Check if allocation module is installed.
  IF 'AL' $ oAriaApplication.CompanyInstalledModules
    =gfOpenFile(oAriaApplication.DataDir+'PIKTKT',oAriaApplication.DataDir+'ORDPIK','SH')
    *-- Do not cancel picked orders
    IF SEEK(OrdHdr.ORDER,'PikTkt','ORDPIK')
      *-- check Piktkt status for picked lines mesg.
      SELECT PikTkt
      LOCATE REST WHILE ORDER = OrdHdr.ORDER FOR STATUS $ 'OHP'
      IF FOUND()
        *-- Start, not allow to proceed,
        IF ASCAN(oAriaApplication.laEvntTrig , PADR('TRMTE_CANL',10)) <> 0
          =gfDoTriger('SOORD',PADR('TRMTE_CANL',10))
          SET REPROCESS TO lnRePro
          RETURN
        ELSE

            *-- Message : 32004
            *-- Some order lines hve been picked.
            *-- Button : 00000
            *-- Ok
            =gfModalGen('INM32004B00000','ALERT')
            IF lfCheck_if_Piktkt_is_Packed()
              RETURN .F.
            ENDIF 
            SELECT piktkt
          IF 'AS' $ oAriaApplication.CompanyInstalledModules
            LOCATE REST WHILE ORDER = OrdHdr.ORDER FOR STATUS $ 'OHP' AND  cSndPkt ='Y'
            IF FOUND()
              *-- Message : 32144
              *-- Warehouse shipping order has been sent for piktkt# 999999. Can't cancel sales order.
              *-- Button : 00000
              *-- Ok
              =gfModalGen('INM32144B00000','ALERT',PikTkt.PikTkt)
              SET REPROCESS TO lnRePro
              RETURN
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    =gfOpenFile(oAriaApplication.DataDir+'PIKLINE',oAriaApplication.DataDir+'PIKLINE','SH')
  ENDIF
  IF OrdHdr.TotCut > 0
    *-- Message : 32054
    *-- This order has quantity allocated.
    *-- Canceling this order will release this allocation.
    *-- Would you like to continue ?
    *-- Button : 32000
    *-- Yes  No
    IF gfModalGen('QRM32054B32000','ALERT')=2
      SET REPROCESS TO lnRePro
      RETURN
    ENDIF
  ENDIF
  *-- Message : 32005
  *-- Cancel all open items on this order?
  *-- Button : 320000
  *-- Yes/No
*!*	  IF gfModalGen('QRM32005B32000','ALERT',IIF(OrdHdr.cOrdType='C',;
*!*	      IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',"Contract",LOFORMSET.GetHeaderText("LANG_CONTRACTLABEL",LOFORMSET.HeaderAlias)),;
*!*	      IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',"Order",LOFORMSET.GetHeaderText("LANG_LabelOrder",LOFORMSET.HeaderAlias)))) = 2
*!*	    SET REPROCESS TO lnRePro
*!*	    RETURN
*!*	  ENDIF
  
  *-- Get Cancellation reason
  lcCanReason = lcCanReasonCode
  IF EMPTY(lcCanReason)
    SET REPROCESS TO lnRePro
    RETURN
  ENDIF
  IF OrdHdr.STATUS='B'
    SELECT OrdHdr
    REPLACE STATUS     WITH 'X' ,;
      cCancReson WITH lcCanReason ,;
      Cancelled  WITH oAriaApplication.SystemDate ,;
      FLAG       WITH SPACE(1)
    *-- Update the CRMesag file
    DO lpUpdMesag
    SET REPROCESS TO lnRePro
    RETURN
  ENDIF
  llCancelBulk = .F.
  IF !EMPTY(ordhdr.cfromorder) AND  gfModalGen('QRM52026B32000','DIALOG',ordhdr.cfromorder) =1
    llCancelBulk = .T.
  ENDIF
*  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('loFormSet')<>'O',LANG_SOUPDATE_CANCELANDUPDATE,loformset.GetHeaderText("LANG_SOUPDATE_CANCELANDUPDATE",loformset.HeaderAlias)) WINDOW NOWAIT
  =gfOpenTable('ICSTYHST','Styhst','SH')
  lcICStyHstTable = ''
  lnTableICStyHst = gfGetRemoteTable(SET("Datasession"),'ICSTYHST')
  IF lnTableICStyHst<>0 && Remote Table Object was Found
    lcICStyHstTable = oAriaApplication.laRemoteTable[lnTableICStyHst].lcCursorUpdate
  ENDIF
  STORE 0 TO lnOpen,lnOpenAmt,lnBook,lnBookAmt
  SELECT ORDLINE
  =SEEK(OrdHdr.cOrdType+OrdHdr.ORDER)
  SCAN REST WHILE cOrdType+ORDER+STR(LINENO,6)= OrdHdr.cOrdType+OrdHdr.ORDER
    IF !EMPTY(ORDLINE.Employee)
      lfUpStyHist(.T.)
    ENDIF
    *-- Decrease warehouse ordered quantity with order open quantity
    IF cOrdType='O' .AND. SEEK(STYLE+cWareCode+SPACE(10),'StyDye')
      =RLOCK("StyDye")
      REPLACE Ord1   WITH MAX(Ord1-ORDLINE.Qty1,0) ,;
        Ord2   WITH MAX(Ord2-ORDLINE.Qty2,0) ,;
        Ord3   WITH MAX(Ord3-ORDLINE.Qty3,0) ,;
        Ord4   WITH MAX(Ord4-ORDLINE.Qty4,0) ,;
        Ord5   WITH MAX(Ord5-ORDLINE.Qty5,0) ,;
        Ord6   WITH MAX(Ord6-ORDLINE.Qty6,0) ,;
        Ord7   WITH MAX(Ord7-ORDLINE.Qty7,0) ,;
        Ord8   WITH MAX(Ord8-ORDLINE.Qty8,0) ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
      *-- Decrease warehouse allocated quantity with order picked quantity
      IF Picked
        REPLACE Alo1   WITH MAX(Alo1-ORDLINE.Pik1,0) ,;
          Alo2   WITH MAX(Alo2-ORDLINE.Pik2,0) ,;
          Alo3   WITH MAX(Alo3-ORDLINE.Pik3,0) ,;
          Alo4   WITH MAX(Alo4-ORDLINE.Pik4,0) ,;
          Alo5   WITH MAX(Alo5-ORDLINE.Pik5,0) ,;
          Alo6   WITH MAX(Alo6-ORDLINE.Pik6,0) ,;
          Alo7   WITH MAX(Alo7-ORDLINE.Pik7,0) ,;
          Alo8   WITH MAX(Alo8-ORDLINE.Pik8,0) ,;
          TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8 IN StyDye
      ENDIF
      UNLOCK IN StyDye
    ENDIF

    *-- Decrease style ordered quantity with order open quantity
    IF cOrdType='O' .AND. SEEK(STYLE,'Style')
      =RLOCK("Style")
      REPLACE Ord1   WITH MAX(Ord1-ORDLINE.Qty1,0) ,;
        Ord2   WITH MAX(Ord2-ORDLINE.Qty2,0) ,;
        Ord3   WITH MAX(Ord3-ORDLINE.Qty3,0) ,;
        Ord4   WITH MAX(Ord4-ORDLINE.Qty4,0) ,;
        Ord5   WITH MAX(Ord5-ORDLINE.Qty5,0) ,;
        Ord6   WITH MAX(Ord6-ORDLINE.Qty6,0) ,;
        Ord7   WITH MAX(Ord7-ORDLINE.Qty7,0) ,;
        Ord8   WITH MAX(Ord8-ORDLINE.Qty8,0) ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN STYLE
      *-- Decrease style allocated quantity with order picked quantity
      IF Picked
        REPLACE Alo1   WITH MAX(Alo1-ORDLINE.Pik1,0) ,;
          Alo2   WITH MAX(Alo2-ORDLINE.Pik2,0) ,;
          Alo3   WITH MAX(Alo3-ORDLINE.Pik3,0) ,;
          Alo4   WITH MAX(Alo4-ORDLINE.Pik4,0) ,;
          Alo5   WITH MAX(Alo5-ORDLINE.Pik5,0) ,;
          Alo6   WITH MAX(Alo6-ORDLINE.Pik6,0) ,;
          Alo7   WITH MAX(Alo7-ORDLINE.Pik7,0) ,;
          Alo8   WITH MAX(Alo8-ORDLINE.Pik8,0) ,;
          TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8 IN STYLE
      ENDIF
      UNLOCK IN STYLE
    ENDIF
    *-- Update dyelot allocated quantity
    IF cOrdType='O' AND !EMPTY(Dyelot) AND SEEK(STYLE+cWareCode+Dyelot,'StyDye')
      =RLOCK("StyDye")
      REPLACE Ord1   WITH MAX(Ord1-ORDLINE.Qty1,0) ,;
        Ord2   WITH MAX(Ord2-ORDLINE.Qty2,0) ,;
        Ord3   WITH MAX(Ord3-ORDLINE.Qty3,0) ,;
        Ord4   WITH MAX(Ord4-ORDLINE.Qty4,0) ,;
        Ord5   WITH MAX(Ord5-ORDLINE.Qty5,0) ,;
        Ord6   WITH MAX(Ord6-ORDLINE.Qty6,0) ,;
        Ord7   WITH MAX(Ord7-ORDLINE.Qty7,0) ,;
        Ord8   WITH MAX(Ord8-ORDLINE.Qty8,0) ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
      IF Picked
        REPLACE Alo1   WITH MAX(Alo1-ORDLINE.Pik1,0) ,;
          Alo2   WITH MAX(Alo2-ORDLINE.Pik2,0) ,;
          Alo3   WITH MAX(Alo3-ORDLINE.Pik3,0) ,;
          Alo4   WITH MAX(Alo4-ORDLINE.Pik4,0) ,;
          Alo5   WITH MAX(Alo5-ORDLINE.Pik5,0) ,;
          Alo6   WITH MAX(Alo6-ORDLINE.Pik6,0) ,;
          Alo7   WITH MAX(Alo7-ORDLINE.Pik7,0) ,;
          Alo8   WITH MAX(Alo8-ORDLINE.Pik8,0) ,;
          TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8 IN StyDye
      ENDIF
      UNLOCK IN StyDye
    ENDIF

    *-- Decrease ordered quantity & amount in the style history with order open quantity
    old_Alias = ALIAS()
    lcOrdType = cOrdType
    lcstyle_var = STYLE
    SELECT icStyHst
    IF !SEEK(lcStyle_Var+ lcGlYear,lcICStyHstTable) AND !gfSEEK(lcStyle_Var+ lcGlYear)
      =gfOpenTable('ICSTYHST','STYHST','SH','ICSTYHSX')
      SELECT ICSTYHSX

      APPEND BLANK
      REPLACE STYLE WITH lcstyle_var
      REPLACE CFISFYEAR  WITH lcGlYear
      = GFREPLACE('')
      =gfTABLEUPDATE()
      =gfcloseTable('ICSTYHSX')
      SELECT ICSTYHST
    ENDIF
    IF lcOrdType='O' .AND.(SEEK(lcstyle_var +lcGlYear,lcICStyHstTable) OR  gfSEEK(lcstyle_var +lcGlYear,'ICSTYHST'))
      SELECT (old_Alias)
      lnOrdAmt = TotQty*Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
      IF !SEEK(lcstyle_var +lcGlYear,lcICStyHstTable)
        =RLOCK("icStyHst")
        REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - ORDLINE.TotQty ,;
          nOrdQty            WITH nOrdQty            - ORDLINE.TotQty ,;
          nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt ,;
          nOrdAmt            WITH nOrdAmt            - lnOrdAmt IN icStyHst
        UNLOCK IN icStyHst
        SELECT icStyHst
        =gfReplace('')
      ELSE
        REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - ORDLINE.TotQty ,;
          nOrdQty            WITH nOrdQty            - ORDLINE.TotQty ,;
          nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt ,;
          nOrdAmt            WITH nOrdAmt            - lnOrdAmt IN (lcICStyHstTable)
      ENDIF
    ENDIF
    SELECT (Old_Alias)
    *-- Release pick ticket and zero out order picked quantity
    IF Picked
      IF SEEK(PikTkt,'PIKTKT','PIKTKT')
        =RLOCK("PIKTKT")
        REPLACE STATUS WITH 'X' IN PikTkt
        REPLACE CEDIT_USER WITH oAriaApplication.User_ID ;               
                DEDIT_DATE WITH DATE() ;
                CEDIT_TIME WITH TIME() ;
                IN PikTkt
        UNLOCK IN PikTkt
      ENDIF
      IF !SEEK(PikTkt+ORDER+STR(LINENO,6),'PIKLINE')
        SCATTER MEMVAR MEMO
        INSERT INTO PIKLINE FROM MEMVAR
      ENDIF
      SELECT ORDLINE
      =RLOCK()
      REPLACE Pik1   WITH 0 ,;
        Pik2   WITH 0 ,;
        Pik3   WITH 0 ,;
        Pik4   WITH 0 ,;
        Pik5   WITH 0 ,;
        Pik6   WITH 0 ,;
        Pik7   WITH 0 ,;
        Pik8   WITH 0 ,;
        TOTPIK WITH 0 ,;
        Picked WITH .F. ,;
        PikTkt WITH SPACE(6) ,;
        PIKDATE WITH {}
      IF SEEK(AltStyle,'STYLE') .AND. SEEK('S'+STYLE.SCALE,'SCALE')
        REPLACE STYLE     WITH AltStyle,;
          AltStyle  WITH SPACE(19),;
          SCALE     WITH SCALE.SCALE,;
          cWareCode WITH OrdHdr.cWareCode
        REPLACE Qty1  WITH IIF(SCALE.CNT>=1,Qty1,0)  ,;
          Book1 WITH IIF(SCALE.CNT>=1,Book1,0) ,;
          Qty2  WITH IIF(SCALE.CNT>=2,Qty2,0)  ,;
          Book2 WITH IIF(SCALE.CNT>=2,Book2,0) ,;
          Qty3  WITH IIF(SCALE.CNT>=3,Qty3,0)  ,;
          Book3 WITH IIF(SCALE.CNT>=3,Book3,0) ,;
          Qty4  WITH IIF(SCALE.CNT>=4,Qty4,0)  ,;
          Book4 WITH IIF(SCALE.CNT>=4,Book4,0) ,;
          Qty5  WITH IIF(SCALE.CNT>=5,Qty5,0)  ,;
          Book5 WITH IIF(SCALE.CNT>=5,Book5,0) ,;
          Qty6  WITH IIF(SCALE.CNT>=6,Qty6,0)  ,;
          Book6 WITH IIF(SCALE.CNT>=6,Book6,0) ,;
          Qty7  WITH IIF(SCALE.CNT>=7,Qty7,0)  ,;
          Book7 WITH IIF(SCALE.CNT>=7,Book7,0) ,;
          Qty8  WITH IIF(SCALE.CNT>=8,Qty8,0)  ,;
          Book8 WITH IIF(SCALE.CNT>=8,Book8,0) ,;
          TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
          TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8
      ENDIF
      UNLOCK
    ENDIF
    SELECT ORDLINE
    =RLOCK()
    REPLACE Cut1   WITH 0 ,;
      Cut2   WITH 0 ,;
      Cut3   WITH 0 ,;
      Cut4   WITH 0 ,;
      Cut5   WITH 0 ,;
      Cut6   WITH 0 ,;
      Cut7   WITH 0 ,;
      Cut8   WITH 0 ,;
      TotCut WITH 0
    UNLOCK
    lnOpen    = lnOpen    + ORDLINE.TotQty
    lnOpenAmt = lnOpenAmt + ORDLINE.TotQty*ORDLINE.Price
    lnBook    = lnBook    + ORDLINE.TotBook
    lnBookAmt = lnBookAmt + ORDLINE.TotBook*ORDLINE.Price
    IF OrdHdr.Ship > 0 .AND. ORDLINE.TotQty > 0
      IF !SEEK(ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'ORDCANLN')
        *-- Update OrdCanLn with Style,Account,Store and Dyelot.
        INSERT INTO ('ORDCANLN') ;
          (cOrdType,ORDER,LINENO,Cancelled,cCancReson,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty, ;
          Account,STYLE,STORE,Dyelot,Price) ;
          VALUES ;
          (ORDLINE.cOrdType,ORDLINE.ORDER,ORDLINE.LINENO,oAriaApplication.SystemDate,lcCanReason,ORDLINE.Qty1,ORDLINE.Qty2,;
          ORDLINE.Qty3,ORDLINE.Qty4,ORDLINE.Qty5,ORDLINE.Qty6,ORDLINE.Qty7,ORDLINE.Qty8,;
          ORDLINE.TotQty,ORDLINE.Account,ORDLINE.STYLE,ORDLINE.STORE,ORDLINE.Dyelot,ORDLINE.Price)
      ENDIF
      REPLACE Qty1   WITH 0 ,;
        Qty2   WITH 0 ,;
        Qty3   WITH 0 ,;
        Qty4   WITH 0 ,;
        Qty5   WITH 0 ,;
        Qty6   WITH 0 ,;
        Qty7   WITH 0 ,;
        Qty8   WITH 0 ,;
        TotQty WITH 0
    ENDIF
    SELECT ORDLINE

    && Adjust Style And StyDye Record Pointers.
    = SEEK(ORDLINE.STYLE,'Style')
    = SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')

    FOR innerstr = 1 TO 8
      cstr = ALLTRIM(STR(innerstr))
      OldQTY&cstr.  = ORDLINE.Qty&cstr.
      OldBook&cstr. = ORDLINE.Book&cstr.
    ENDFOR

    lcFromOrder = ORDLINE.cFromOrder
    lBulkLineNo = ORDLINE.BulkLineNo
    lcOrdType   = ORDLINE.cOrdType
    lcDyelot    = ORDLINE.Dyelot
    lcSTYLE     = ORDLINE.STYLE
    lcWareCode  = ORDLINE.cWareCode

    IF !EMPTY(lcFromOrder) .AND. SEEK(lcOrdType+lcFromOrder+STR(lBulkLineNo,6),'BlkOrdLine') .AND. SEEK(lcOrdType+lcFromOrder,'BlkOrdHd','OrdHdr')
      lnOpen      = 0
      lnOpenAmt   = 0
      lnCancel    = 0
      lnCancelAmt = 0
      FOR innerLoopCounter = 1 TO 8
        cstr = ALLTRIM(STR(innerLoopCounter))
        xDiff = OldQTY&cstr.
        IF !llCancelBulk
          REPLACE Qty&cstr  WITH (Qty&cstr. + xDiff) IN BlkOrdLine
          REPLACE TotQty    WITH (TotQty + xDiff)    IN BlkOrdLine
          lnOpen    = lnOpen + xDiff
          lnOpenAmt = lnOpenAmt + (xDiff * ORDLINE.Price)
        ELSE
          lnOpen    = lnOpen + xDiff
          lnOpenAmt = lnOpenAmt + (xDiff * ORDLINE.Price)
        ENDIF
        xDiff = OldBook&cstr.
        REPLACE Book&cstr  WITH (Book&cstr. + xDiff) IN BlkOrdLine
        REPLACE TotBook    WITH (TotBook + xDiff)    IN BlkOrdLine
        lnCancel = lnCancel + xDiff
        lnCancelAmt = lnCancelAmt + (xDiff * ORDLINE.Price)
        IF !llCancelBulk
          =RLOCK('Style')
          REPLACE  Ord&cstr.   WITH Ord&cstr. + xDiff ,;
            TotOrd      WITH TotOrd    + xDiff IN STYLE
          UNLOCK IN 'Style'

          =RLOCK('StyDye')
          REPLACE Ord&cstr.  WITH Ord&cstr.  + xDiff  ,;
            TotOrd     WITH TotOrd     + xDiff  IN StyDye
          UNLOCK IN 'StyDye'
          *-- Update the Qty on the Configuration level
          IF !EMPTY(lcDyelot) AND SEEK(lcSTYLE+lcWareCode+lcDyelot,'StyDye')
            REPLACE Ord&cstr.   WITH Ord&cstr. + xDiff ,;
              TotOrd      WITH TotOrd    + xDiff IN StyDye
            UNLOCK IN 'StyDye'
          ENDIF
        ENDIF
      ENDFOR
      IF !llCancelBulk
        && Modify the Bulk Order Header Totals And Adjust The Status Depend On The Totals values
        =RLOCK("BlkOrdHd")
        SELECT BlkOrdHd
        REPLACE Book      WITH (Book  + lnCancel)   ,;
                BookAmt   WITH (BookAmt + lnCancelAmt),;
		        OPEN      WITH (OPEN    + lnOpen)     ,;
		        OpenAmt   WITH (OpenAmt + lnOpenAmt)  ,;
		        STATUS    WITH IIF(OPEN = 0 ,'X',STATUS) ,;
		        APPRAMT   WITH MAX(0,APPRAMT + lnOpenAmt) IN BlkOrdHd
        UNLOCK IN BlkOrdHd
        IF BlkOrdHd.STATUS='X' .AND. BlkOrdHd.OPEN > 0 .AND. SEEK('M'+BlkOrdHd.Account,'Customer')
	        =RLOCK("BlkOrdHd")
	        REPLACE STATUS WITH "O" IN BlkOrdHd
	        UNLOCK IN BlkOrdHd

	        =RLOCK('Customer')
	        REPLACE nBulk WITH nBulk + 1 IN Customer
	        UNLOCK IN 'Customer'
	    ENDIF
      ENDIF
    ENDIF
    SELECT ORDLINE

  ENDSCAN
  IF llCancelBulk AND USED('ORDCANLN_A')
    SELECT 'ORDCANLN_A'
    =gfTableUpdate()
  ENDIF

  *-- Release order allocated quantity to any cutting ticket order style purchae order
  llUpdate = .F.
  IF OrdHdr.TotCut > 0
    lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
    IF TYPE('lcTranCode') = 'N'
      =oAriaApplication.RemoteCompanyData.CheckRetResult("BEGINTRAN",lcTranCode,.T.)
    ELSE
      lcSelString =               "SELECT CutPick.*,PosHdr.Status FROM CutPick INNER JOIN PosHdr On PosHdr.cBusDocu+PosHdr.cStyType+PosHdr.Po = 'PU'+CutPick.cTktNo WHERE CutPick.TranCd+CutPick.[Order]+CutPick.cOrdLine LIKE '1" + OrdHdr.ORDER + "%' UNION "
      lcSelString = lcSelString + "SELECT CutPick.*,PosHdr.Status FROM CutPick INNER JOIN PosHdr ON PosHdr.cBusDocu+PosHdr.cStyType+PosHdr.Po = 'PP'+CutPick.cTktNo WHERE CutPick.TranCd+CutPick.[Order]+CutPick.cOrdLine LIKE '2" + OrdHdr.ORDER + "%' ORDER BY trancd,ctktno,cTKtLineNo"
      IF oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"CutPick","CutPick ",lcTranCode,4,'',SET("DATASESSION")) = 1
        LOCAL lcPosLn,loPosLn
        lcPosLn = gfTempName()
        loPosLn = CREATEOBJECT('RemoteTable','POSLN','POSLN',lcPosLn,SET("DATASESSION"))

        SELECT CUTPICK
        GO TOP
        DO WHILE !EOF()
          lcTranCd = TRANCD
          lcTktNo  = CTKTNO
          lnTotOrd = 0
          IF lcTranCd = '1'
            =loPosLn.SEEK("PU"+CUTPICK.CTKTNO+'0001')
          ELSE
            =loPosLn.SEEK("PP"+CUTPICK.CTKTNO+'0001')
          ENDIF
          *-- Message : 32006
          *-- Cuttkt# xxxxxx status is HOLD. Do you wish to update the generated
          *-- cuttkt quantities accordingly?
          *-- Button : 32000
          *-- Yes/No
          *-- Message : 32007
          *-- Purchase order# xxxxxx status is HOLD. Do you wish to update the generated
          *-- Purchase order quantities accordingly?
          *-- Button : 32000
          *-- Yes/No
          llUpdate= STATUS='H' .AND. ((TRANCD='1' .AND. gfModalGen('QRM32006B32000','ALERT',CTKTNO)=1) .OR. ;
            (TRANCD='2' .AND. gfModalGen('QRM32007B32000','ALERT',CTKTNO)=1) )

          SCAN REST WHILE TRANCD+CTKTNO+CTKTLINENO = lcTranCd+lcTktNo
            =SEEK('O'+ORDER+CORDLINE,'OrdLine','OrdLine')
            IF lcTranCd = '1'
              =SEEK("PU"+CUTPICK.CTKTNO+'0001'+CUTPICK.STYLE+CUTPICK.CTKTLINENO+"1",lcPosLn)
            ELSE
              =SEEK("PP"+CUTPICK.CTKTNO+'0001'+CUTPICK.STYLE+CUTPICK.CTKTLINENO+"1",lcPosLn)
            ENDIF
            SELECT (lcPosLn)
            *-- Update ticket lines Ordered quantity
            loPosLn.REPLACE("Ord1   WITH Ord1   - CUTPICK.Qty1,"+;
              "Ord2   WITH Ord2   - CUTPICK.Qty2,"+;
              "Ord3   WITH Ord3   - CUTPICK.Qty3,"+;
              "Ord4   WITH Ord4   - CUTPICK.Qty4,"+;
              "Ord5   WITH Ord5   - CUTPICK.Qty5,"+;
              "Ord6   WITH Ord6   - CUTPICK.Qty6,"+;
              "Ord7   WITH Ord7   - CUTPICK.Qty7,"+;
              "Ord8   WITH Ord8   - CUTPICK.Qty8,"+;
              "TotOrd WITH TotOrd - CUTPICK.TotQty")
            *-- Update ticket lines budget quantities
            IF llUpdate
              loPosLn.REPLACE("Qty1   WITH Qty1   - CUTPICK.Qty1,"+;
                "Qty2   WITH Qty2   - CUTPICK.Qty2,"+;
                "Qty3   WITH Qty3   - CUTPICK.Qty3,"+;
                "Qty4   WITH Qty4   - CUTPICK.Qty4,"+;
                "Qty5   WITH Qty5   - CUTPICK.Qty5,"+;
                "Qty6   WITH Qty6   - CUTPICK.Qty6,"+;
                "Qty7   WITH Qty7   - CUTPICK.Qty7,"+;
                "Qty8   WITH Qty8   - CUTPICK.Qty8,"+;
                "TotQty WITH TotQty - CUTPICK.TotQty")

              REPLACE Qty1   WITH Qty1   - CUTPICK.Qty1,;
                Qty2   WITH Qty2   - CUTPICK.Qty2,;
                Qty3   WITH Qty3   - CUTPICK.Qty3,;
                Qty4   WITH Qty4   - CUTPICK.Qty4,;
                Qty5   WITH Qty5   - CUTPICK.Qty5,;
                Qty6   WITH Qty6   - CUTPICK.Qty6,;
                Qty7   WITH Qty7   - CUTPICK.Qty7,;
                Qty8   WITH Qty8   - CUTPICK.Qty8,;
                TotQty WITH TotQty - CUTPICK.TotQty IN (lcPosLn)

            ENDIF
            IF SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')
              =RLOCK("StyDye")
              REPLACE WIP1   WITH MAX(WIP1-ORDLINE.Qty1,0) ,;
                WIP2   WITH MAX(WIP2-ORDLINE.Qty2,0) ,;
                WIP3   WITH MAX(WIP3-ORDLINE.Qty3,0) ,;
                WIP4   WITH MAX(WIP4-ORDLINE.Qty4,0) ,;
                WIP5   WITH MAX(WIP5-ORDLINE.Qty5,0) ,;
                WIP6   WITH MAX(WIP6-ORDLINE.Qty6,0) ,;
                WIP7   WITH MAX(WIP7-ORDLINE.Qty7,0) ,;
                WIP8   WITH MAX(WIP8-ORDLINE.Qty8,0) ,;
                TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8 IN StyDye
              REPLACE NWO1   WITH MAX(NWO1-ORDLINE.Qty1,0) ,;
                NWO2   WITH MAX(NWO2-ORDLINE.Qty2,0) ,;
                NWO3   WITH MAX(NWO3-ORDLINE.Qty3,0) ,;
                NWO4   WITH MAX(NWO4-ORDLINE.Qty4,0) ,;
                NWO5   WITH MAX(NWO5-ORDLINE.Qty5,0) ,;
                NWO6   WITH MAX(NWO6-ORDLINE.Qty6,0) ,;
                NWO7   WITH MAX(NWO7-ORDLINE.Qty7,0) ,;
                NWO8   WITH MAX(NWO8-ORDLINE.Qty8,0) ,;
                NTOTWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8 IN StyDye
              UNLOCK IN StyDye
            ENDIF
            IF !EMPTY(ORDLINE.Dyelot) AND SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+ORDLINE.Dyelot,'StyDye')
              =RLOCK("StyDye")
              REPLACE WIP1   WITH MAX(WIP1-ORDLINE.Qty1,0) ,;
                WIP2   WITH MAX(WIP2-ORDLINE.Qty2,0) ,;
                WIP3   WITH MAX(WIP3-ORDLINE.Qty3,0) ,;
                WIP4   WITH MAX(WIP4-ORDLINE.Qty4,0) ,;
                WIP5   WITH MAX(WIP5-ORDLINE.Qty5,0) ,;
                WIP6   WITH MAX(WIP6-ORDLINE.Qty6,0) ,;
                WIP7   WITH MAX(WIP7-ORDLINE.Qty7,0) ,;
                WIP8   WITH MAX(WIP8-ORDLINE.Qty8,0) ,;
                TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8 IN StyDye
              REPLACE NWO1   WITH MAX(NWO1-ORDLINE.Qty1,0) ,;
                NWO2   WITH MAX(NWO2-ORDLINE.Qty2,0) ,;
                NWO3   WITH MAX(NWO3-ORDLINE.Qty3,0) ,;
                NWO4   WITH MAX(NWO4-ORDLINE.Qty4,0) ,;
                NWO5   WITH MAX(NWO5-ORDLINE.Qty5,0) ,;
                NWO6   WITH MAX(NWO6-ORDLINE.Qty6,0) ,;
                NWO7   WITH MAX(NWO7-ORDLINE.Qty7,0) ,;
                NWO8   WITH MAX(NWO8-ORDLINE.Qty8,0) ,;
                NTOTWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8 IN StyDye
              UNLOCK IN StyDye
            ENDIF
            *-- Decrease style ordered quantity with order open quantity
            IF SEEK(ORDLINE.STYLE,'Style')
              =RLOCK("Style")
              REPLACE WIP1   WITH MAX(WIP1-ORDLINE.Qty1,0) ,;
                WIP2   WITH MAX(WIP2-ORDLINE.Qty2,0) ,;
                WIP3   WITH MAX(WIP3-ORDLINE.Qty3,0) ,;
                WIP4   WITH MAX(WIP4-ORDLINE.Qty4,0) ,;
                WIP5   WITH MAX(WIP5-ORDLINE.Qty5,0) ,;
                WIP6   WITH MAX(WIP6-ORDLINE.Qty6,0) ,;
                WIP7   WITH MAX(WIP7-ORDLINE.Qty7,0) ,;
                WIP8   WITH MAX(WIP8-ORDLINE.Qty8,0) ,;
                TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8 IN STYLE
              REPLACE NWO1   WITH MAX(NWO1-ORDLINE.Qty1,0) ,;
                NWO2   WITH MAX(NWO2-ORDLINE.Qty2,0) ,;
                NWO3   WITH MAX(NWO3-ORDLINE.Qty3,0) ,;
                NWO4   WITH MAX(NWO4-ORDLINE.Qty4,0) ,;
                NWO5   WITH MAX(NWO5-ORDLINE.Qty5,0) ,;
                NWO6   WITH MAX(NWO6-ORDLINE.Qty6,0) ,;
                NWO7   WITH MAX(NWO7-ORDLINE.Qty7,0) ,;
                NWO8   WITH MAX(NWO8-ORDLINE.Qty8,0) ,;
                NTOTWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8 IN STYLE
              UNLOCK IN STYLE
            ENDIF
            lnTotOrd = lnTotOrd + CUTPICK.TotQty
          ENDSCAN

          SELECT (lcPosLn)
          STORE 0 TO lnCost1,lnCost2,lnCost3,lnCost4,lnCost5,lnCost6,lnCost7 , lnPOTotal
          STORE 0 TO lnFCost1,lnFCost2,lnFCost3,lnFCost4,lnFCost5,lnFCost6,lnFCost7
          SCAN FOR TRANCD = '1'
            lnCost1 = lnCost1 + (TotQty * nicost1)
            lnCost2 = lnCost2 + (TotQty * nicost2)
            lnCost3 = lnCost3 + (TotQty * nicost3)
            lnCost4 = lnCost4 + (TotQty * nicost4)
            lnCost5 = lnCost5 + (TotQty * nicost5)
            lnCost6 = lnCost6 + (TotQty * nicost6)
            lnCost7 = lnCost7 + (TotQty * nicost7)

            lnFCost1 = lnFCost1 + (TotQty * nFcost1)
            lnFCost2 = lnFCost2 + (TotQty * nFcost2)
            lnFCost3 = lnFCost3 + (TotQty * nFcost3)
            lnFCost4 = lnFCost4 + (TotQty * nFcost4)
            lnFCost5 = lnFCost5 + (TotQty * nFcost5)
            lnFCost6 = lnFCost6 + (TotQty * nFcost6)
            lnFCost7 = lnFCost7 + (TotQty * nFcost7)
          ENDSCAN
          lnPOTotal = lnCost1+lnCost2+lnCost3+lnCost4+lnCost5+lnCost6+lnCost7
          loPosLn.TABLEUPDATE(lcTranCode)

          *-- Update ticket total Ordered & budget quantity
          IF lcTranCd = '1'
            *-- Change Cutting quantity if order allocated quantity has been changed
            lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd-"+ALLTRIM(STR(lnTotOrd))+;
              IIF(llUpdate,",nStyOrder = nStyOrder-"+ALLTRIM(STR(lnTotOrd))+",[Open]=[Open]-"+ALLTRIM(STR(lnTotOrd)),'')+;
              " ,niCost1 = "+ALLTRIM(STR(lnCost1))+" ,niCost2 = "+ALLTRIM(STR(lnCost2))+" ,niCost3 = "+ALLTRIM(STR(lnCost3))+" ,niCost4 = "+ALLTRIM(STR(lnCost4))+;
              " ,niCost5 = "+ALLTRIM(STR(lnCost5))+" ,niCost6 = "+ALLTRIM(STR(lnCost6))+" ,niCost7 = "+ALLTRIM(STR(lnCost7))+;
              ",nFCost1= "+ALLTRIM(STR(lnFCost1))+",nFCost2= "+ALLTRIM(STR(lnFCost2))+",nFCost3= "+ALLTRIM(STR(lnFCost3))+",nFCost4= "+ALLTRIM(STR(lnFCost4))+;
              ",nFCost5= "+ALLTRIM(STR(lnFCost5))+",nFCost6= "+ALLTRIM(STR(lnFCost6))+",nFCost7= "+ALLTRIM(STR(lnFCost7))+;
              " ,POTotal = "+ALLTRIM(STR(lnPOTotal))+" WHERE cBusDocu+cstytype+po='PU"+lcTktNo+"'"
          ELSE
            *-- Change Purchased quantity if order allocated quantity has been changed
            lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd-"+ALLTRIM(STR(lnTotOrd))+;
              IIF(llUpdate,",nStyOrder = nStyOrder-"+ALLTRIM(STR(lnTotOrd))+",[Open]=[Open]-"+ALLTRIM(STR(lnTotOrd)),'')+;
              " ,niCost1 = "+ALLTRIM(STR(lnCost1))+" ,niCost2 = "+ALLTRIM(STR(lnCost2))+" ,niCost3 = "+ALLTRIM(STR(lnCost3))+" ,niCost4 = "+ALLTRIM(STR(lnCost4))+;
              " ,niCost5 = "+ALLTRIM(STR(lnCost5))+" ,niCost6 = "+ALLTRIM(STR(lnCost6))+" ,niCost7 = "+ALLTRIM(STR(lnCost7))+;
              ",nFCost1= "+ALLTRIM(STR(lnFCost1))+",nFCost2= "+ALLTRIM(STR(lnFCost2))+",nFCost3= "+ALLTRIM(STR(lnFCost3))+",nFCost4= "+ALLTRIM(STR(lnFCost4))+;
              ",nFCost5= "+ALLTRIM(STR(lnFCost5))+",nFCost6= "+ALLTRIM(STR(lnFCost6))+",nFCost7= "+ALLTRIM(STR(lnFCost7))+;
              " ,POTotal = "+ALLTRIM(STR(lnPOTotal))+" WHERE cBusDocu+cstytype+po='PP"+lcTktNo+"'"
          ENDIF
          lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode,4,'',SET("DATASESSION"))
          SELECT CUTPICK
        ENDDO

        loPosLn = NULL

        SELECT CUTPICK
        DELETE ALL
        lnResult = oAriaApplication.RemoteCompanyData.SqlUpdate('CUTPICK',lcTranCode, SET("DATASESSION"),'trancd,ctktno,ctktlineno,order,style,cordline')
        IF lnResult <=0
          =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
          =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
        ELSE
          IF oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.) = 1
          ELSE
            =oAriaApplication.RemoteCompanyData.CheckRetResult("COMMITTRAN",lnResult,.T.)
          ENDIF
        ENDIF
        USE IN CUTPICK
      ENDIF
    ENDIF
  ENDIF

  *-- Add any removed lines quantity to computed booked quantity
  IF SEEK(OrdHdr.cOrdType+OrdHdr.ORDER,'ORDCANLN')
    SELECT ORDCANLN
    SCAN REST WHILE cOrdType+ORDER+STR(LINENO,6) = OrdHdr.cOrdType+OrdHdr.ORDER
      IF !SEEK(cOrdType+ORDER+STR(LINENO,6),'OrdLine')
        lnBook    = lnBook    + ORDCANLN.TotQty
        lnBookAmt = lnBookAmt + ORDCANLN.TotQty*ORDCANLN.Price
      ENDIF
    ENDSCAN
  ENDIF
  *-- Decrease number of bulk orders for this account
  IF OrdHdr.Bulk='Y' .AND. SEEK('M'+OrdHdr.Account,'Customer')
    =RLOCK("CUSTOMER")
    REPLACE nBulk WITH nBulk - 1 IN Customer
    UNLOCK IN Customer
  ENDIF

  *-- Decrease orders quantity and amount in the customerr history file
  IF OrdHdr.cOrdType='O'
    =gfOpenFile(oAriaApplication.DataDir+'arCusHst',oAriaApplication.DataDir+'Acthst','SH')
    =SEEK(OrdHdr.Account+lcGlYear,'arCusHst')
    lnOrdAmt = OrdHdr.OpenAmt &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
    =RLOCK("arCusHst")
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - OrdHdr.OPEN ,;
      nOrdQty            WITH nOrdQty            - OrdHdr.OPEN ,;
      nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt    ,;
      nOrdAmt            WITH nOrdAmt            - lnOrdAmt IN arCusHst
    UNLOCK IN arCusHst
  ENDIF

  *-- Update order open, Cancel and book quantity and amount and order status
  SELECT OrdHdr
  IF !(Bulk='Y' AND lnBook <= 0)
    REPLACE STATUS     WITH IIF(Ship > 0,'C','X') ,;
      cCancReson WITH lcCanReason ,;
      Cancelled  WITH oAriaApplication.SystemDate  ,;
      CANCEL     WITH IIF(Bulk='Y',CANCEL ,CANCEL+lnOpen),;
      Cancelamt  WITH IIF(Bulk='Y',Cancelamt ,Cancelamt+lnOpenAmt),;
      OPEN       WITH 0,;
      OpenAmt    WITH 0,;
      TotCut     WITH 0,;
      Book       WITH IIF(Bulk='Y',Ship + CANCEL,lnBook)    ,;
      BookAmt    WITH IIF(Bulk='Y',ShipAmt + Cancelamt,lnBookAmt) ,;
      FLAG       WITH SPACE(1)
  ELSE
    = gfModalGen('QRM54040B00000','DIALOG',ordhdr.ORDER)
  ENDIF


lcOldAlias = ALIAS()
SELECT ICSTYHST
=gfTableUpdate()
SELECT(lcOldAlias)

IF OrdHdr.cOrdType='O'
  lcOldAlias = SELECT()
  SELECT STYHIST
  =gfTableUpdate()
  SELECT(lcOldAlias)
ENDIF

IF OrdHdr.STATUS = 'X'
  lfUpdPrjSt(OrdHdr.cOrdType,OrdHdr.ORDER)
ENDIF

*-- Update the CRMesag file.
DO lpUpdMesag
SET REPROCESS TO lnRePro
RETURN
*!*************************************************************
*! Name      : lfUpdPrjSt
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/31/2015
*! Purpose   : Update Project Status
*!*************************************************************
FUNCTION lfUpdPrjSt
LPARAMETERS lcOrdType,lcOrder,lnLineNo
PRIVATE lnPrvAlias
lnPrvAlias = SELECT(0)
IF !USED('PMPRJHD')
  =gfOpenTable('PMPRJHD'  , 'PMPRJHD' , 'SH')
ENDIF

IF !USED('PMPRJDT')
  =gfOpenTable('PMPRJDT'  , 'PMPRJDT' , 'SH')
ENDIF

IF !USED('SYSCHDUL')
  =gfOpenTable('SYSCHDUL' , 'Coprusr' , 'SH')
ENDIF

IF !USED('AUDTRAIL')
  =gfOpenTable('AUDTRAIL' , 'AUDTRAIL', 'SH')
ENDIF

IF !USED("PMCTGHD")
  =gfOpenTable('PMCTGHD','PMCTGHD','SH')
ENDIF

IF gfSEEK(lcOrdType+lcOrder,'PMPRJHD')
  SELECT PMPRJHD
  SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6) = lcOrdType+lcOrder FOR !(PMPRJHD.cprj_stts $ 'CH') AND ;
      IIF(TYPE('lnLineNo') = 'N',LINENO = lnLineNo,.T.)
    gfReplace("cprj_stts WITH 'X'")
    lcProg  = 'SOORD'
    lcKey   =lcOrdType+ lcOrder+ SUBSTR(PMPRJHD.CSTYLE,1,LEN(CSTYLE))+STR(PMPRJHD.LINENO,6)
    SELECT PMPRJDT
    IF gfSEEK(SUBSTR(PMPRJHD.CPRJ_TYP,1,LEN(CPRJ_TYP)) + SUBSTR(PMPRJHD.CPRJ_ID,1,LEN(CPRJ_ID)) +;
        SUBSTR(PMPRJHD.CSTYLE,1,LEN(CSTYLE))+STR(PMPRJHD.LINENO,6))
      SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6) +coprt_ctg+coprt_id =;
          SUBSTR(PMPRJHD.CPRJ_TYP,1,LEN(CPRJ_TYP)) + SUBSTR(PMPRJHD.CPRJ_ID,1,LEN(CPRJ_ID)) +;
          SUBSTR(PMPRJHD.CSTYLE,1,LEN(CSTYLE))+STR(PMPRJHD.LINENO,6)
        IF gfSEEK(PMPRJDT.CPRJ_TYP + PMPRJDT.CPRJ_ID + PMPRJDT.CSTYLE +STR(PMPRJDT.LINENO,6)+ PMPRJDT.coprt_ctg+PMPRJDT.coprt_id,'SYSCHDUL')
          SELECT SYSCHDUL
          gfReplace("COPERSTAT WITH 'X'")

          SELECT PMPRJDT
          =gfSEEK(coprt_ctg,"PMCTGHD")
          lcInform = 'Operation Category: '           + PMCTGHD.cCtg_Dsc                     + CHR(13)+;
            'Activity: '                     + cOprt_Dsc                            + CHR(13)+;
            'User: '                         + cOprt_res                            + CHR(13)+;
            'Remaining Time on Completion: ' + ALLTRIM(STR(nRem_Dur,3)) + ' day(s)' + CHR(13)+;
            'Transaction Date: '             + DTOC(dEst_strt)                      + CHR(13)+;
            'Completion Date: '              + DTOC(dEst_Fnsh)                      + CHR(13)+;
            'Status: '+IIF(SYSCHDUL.COPERSTAT=[O],[Open],IIF(SYSCHDUL.COPERSTAT=[C],[Complete],IIF(SYSCHDUL.COPERSTAT=[P],[In Work],;
            IIF(SYSCHDUL.COPERSTAT=[H],[Hold],IIF(SYSCHDUL.COPERSTAT=[X],[Void],[])))))

          =GFAUDTRL(loFormSet , lcProg  , lcKey , 'MFPROJ' , 'CANCEL', lcInform )
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF
SELECT PMPRJHD
=gfTableUpdate()
SELECT PMPRJDT
=gfTableUpdate()
SELECT (lnPrvAlias)

*!*****************************************************************************************
*! Name      : lfWRunGrid
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 05/31/2015
*! Purpose   : Option Grid when Function
*!*****************************************************************************************
FUNCTION lfWRunGrid
IF !USED('Codes_DEF')
  =gfOpenTable('Codes','CCODE_NO','SH','Codes_DEF')
ENDIF
lcDefaCanReason = ''
IF gfSeek('D'+'CCANCRESON','Codes_DEF','CCODE_NO')
  lcDefaCanReason  = Codes_DEF.CCODE_NO
ENDIF
IF !EMPTY(lcDefaCanReason)
  lnPosOrd= ASCAN(loogScroll.laOGFXFLT,"ORDCANLN.CCANCRESON")
  IF lnPosOrd > 0
    lnPosOrd = ASUBSCRIPT(loogScroll.laOGFXFLT,lnPosOrd ,1)
    loogScroll.laOGFXFLT[lnPosOrd ,6] = lcDefaCanReason
  ENDIF
ENDIF
*: B611080,1 MMT 11/18/2015 Issues in Custom Sales order merging program[T20151013.0028][Start]
lcRpAccount =""
lcRpOrder=""
*: B611080,1 MMT 11/18/2015 Issues in Custom Sales order merging program[T20151013.0028][End]
*!*****************************************************************************************
*! Name      : lfvCustPO
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 05/31/2015 
*! Purpose   : Validate CustPO field
*!*****************************************************************************************
FUNCTION lfvCustPO
lcCustPO = ''
lnCustPosOrd= ASCAN(loogScroll.laOGFXFLT,"ORDHDR.CUSTPO")
IF lnCustPosOrd > 0
  lnCustPosOrd = ASUBSCRIPT(loogScroll.laOGFXFLT,lnCustPosOrd,1)
  lcCustPO =loogScroll.laOGFXFLT[lnCustPosOrd,6] 
ENDIF

IF !EMPTY(lcCustPO)
  IF EMPTY(lcRpAccount)
    =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Please select account.')
    loogScroll.laOGFXFLT[lnCustPosOrd,6] = ''
    RETURN .F. 
  ENDIF
  IF !USED('ORDHDR_CUSTPO')
    =gfOpenTable('ORDHDR','ORDCUST','SH','ORDHDR_CUSTPO')
  ENDIF
  IF GFSEEK(lcRpAccount+UPPER(PADR(lcCustPO,15))+'O','ORDHDR_CUSTPO','ORDCUST') .AND. ;
     gfModalGen('QRM32025B32004','ALERT',ALLTRIM(lcCustPO))=2
     loogScroll.laOGFXFLT[lnCustPosOrd,6] = ''
     RETURN .F.
  ENDIF  
ENDIF
*!*************************************************************
*! Name      : lfGetClrD
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/31/2015
*! Purpose   : To get color position and  color length
*!*************************************************************
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
PRIVATE lnCount
lcOldSelect=SELECT()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
  CASE laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])
  ENDCASE
ENDFOR
SELECT(lcOldSelect)
*!*****************************************************************************************
*! Name      : lfCustMsg
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 05/31/2015
*! Purpose   : Dummy function to be called from soupdate
*!*****************************************************************************************
FUNCTION lfCustMsg

*!*****************************************************************************************
*! Name      : lfvDept
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 05/31/2015
*! Purpose   : Validate Department
*!*****************************************************************************************
FUNCTION lfvDept

IF EMPTY(ALLTRIM(lcRpAccount))
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Please Select Account.')
  lcRpDept =''
  RETURN 
ENDIF
IF !USED('CustDept')
  =gfOpenTable(oAriaApplication.DataDir+'CustDept',oAriaApplication.DataDir+'CustDept','SH')
ENDIF
IF "?" $ lcRpDept .OR. (!EMPTY(lcRpDept) .AND. !gfSEEK(lcRpAccount+lcRpDept,'CustDept'))
  IF !gfSEEK(lcRpAccount,'CustDept')
    IF "?" $ lcRpDept 
      *-- Message : 32026
      *-- No departments assigend to account xxx
      *-- Button : 00000
      *-- ok
      =gfModalGen('TRM32026B00000','ALERT',lcRpAccount)
      STORE SPACE(5) TO lcRpDept
    ENDIF
    RETURN
  ENDIF
  SELECT CustDept
  lcBrFields = [Dept:H="]+LANG_COLDept+[",cDeptDesc :H="]+LANG_LabelStyleDesc+;
  [",Rep1:H="]+LANG_COLRep1+;
  [",Comm1:H="]+LANG_COLComm1+;
  [",Rep2:H="]+LANG_COLRep2+;
  [",Comm2:H="]+LANG_COLComm2+;
  [",cTerms=gfCodDes(cTermCode,'CTERMCODE') :H="]+LANG_ColTerms+["]
  lcRpDept = IIF(ARIABROW('lcRpAccount',LANG_Departments,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Dept','laBrowArr'),;
                  CustDept.Dept,lcRpDept)
ENDIF

*!*************************************************************
*! Name      : lfvWrH
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/31/2015
*! Purpose   : Validate WareHouse
*!*************************************************************
FUNCTION lfvWrH
IF !USED('WAREHOUS')
  =gfOpenTable('WAREHOUS','WAREHOUS','SH')
ENDIF
*-- IF The user want to Browse or if the Warehouse he entered is not in the file
IF '?' $ lcRpWrh .OR. (!EMPTY(lcRpWrh) .AND. !gfSEEK(lcRpWrh, 'WAREHOUS'))
  lcRpWrh = gfBrowWare(.T.)
ENDIF    && End of IF


*: B611080,1 MMT 11/18/2015 Issues in Custom Sales order merging program[T20151013.0028][Start]
*!*************************************************************
*! Name      : RefreshOrder
*! Developer : Mariam Mazhar(MMT)
*! Date      : 11/18/2015
*! Purpose   : Refresh Order
*!*************************************************************
FUNCTION RefreshOrder
RETURN (lcRpOrder)
*!*************************************************************
*! Name      : RefreshAccount
*! Developer : Mariam Mazhar(MMT)
*! Date      : 11/18/2015
*! Purpose   : Refresh Account
*!*************************************************************
FUNCTION RefreshAccount
RETURN (lcRpAccount)
*: B611080,1 MMT 11/18/2015 Issues in Custom Sales order merging program[T20151013.0028][End]