*:************************************************************************************
*: Program file  : ARSWMNT
*: Program desc. : Software Maintance for Aria NY
*: Module        : Accounts Receivable(AR)
*: Developer     : Heba Fathy Khalaf Allah (HFK)
*:************************************************************************************
*: Calls :
*:    Programs   : ....
*:    Screens    : ....
*:    Global Functions  : gfDispRe,gfTempName.
*:************************************************************************************
*: Passed Parameters  : None
*:************************************************************************************
*: Example : DO  ARSWMNT
*:************************************************************************************
*: Modifications : #037909, HFK(Heba Fathi),03/08/2004,Addind filters due to letter types
*: and to improve the way of collecting additional users data.
*B609484, tmi 12/20/2010 I found difference between VSS and the version at the customer installation, I updated the VSS from that version
*:************************************************************************************
*-------------------------------- Report Code Begin ----------------------------------*
*!*	_screen.Visible=.t.
*!*	ACTIVATE WINDOW trace
*!*	susp
loOGScroll.cCROrientation = 'P'
lnContPer2 = .01
*-- Read connection string.    
lcConnString = lfReadConStr()
IF EMPTY(lcConnString)
	RETURN
ENDIF


*-- Get the Connection to the SQL files
=lfvGetConnection()

*- Create Temporary header and detail files to hold all the necessary data for printing.
=lfCrtFile() 


*-- Get the Cursor name of the Customer
IF EMPTY(laOgFxFlt(1,6))  &&user didn't se;ect customers.
  lcCustTmp  = "CUSTOMER"
  lcScanCond = "Customer.Type ='M' AND Customer.status = 'A'"
ELSE
	lcCustTmp  = ALLTRIM(laOgFxFlt(1,6))
  lcScanCond = ".T."
ENDIF 

*--#037909, HFK(Heba Fathi),03/08/2004
SELECT (lcCustTmp)
DO CASE 
  CASE lcLetType='A'  && ALL
    lcScanType = "Custprof.lactuser=.T."
  CASE lcLetType='B'  && -45 Days
    lcScanType = "Custprof.lactuser=.T. .AND. CustProf.dCust_padu <=DATE()+50 .AND. CustProf.dCust_padu >= DATE()+40"
  CASE lcLetType='C'  && -15 Days
    lcScanType = "Custprof.lactuser=.T. .AND. CustProf.dCust_padu <=DATE()+20 .AND. CustProf.dCust_padu >= DATE()+10"      
  CASE lcLetType='D'  && 0 Days
    lcScanType = "Custprof.lactuser=.T. .AND. CustProf.dCust_padu <=DATE()-5 .AND. CustProf.dCust_padu >= DATE()+5"      
  CASE lcLetType='E'  && +15 Days
    lcScanType = "Custprof.lactuser=.T. .AND. CustProf.dCust_padu <=DATE()-10 .AND. CustProf.dCust_padu >= DATE()-20"      
  CASE lcLetType='F'  && +90 Days
    lcScanType = "Custprof.lactuser=.T. .AND. CustProf.dCust_padu <=DATE()-80 .AND. CustProf.dCust_padu >= DATE()-100"      
ENDCASE 
SCAN FOR &lcScanCond
  lcCst = &lcCustTmp..Account
  lcselect = [Select dCust_padu,lactuser from custprof where ccust_id=']+lcCst+[']
  *-lnResult = oLoadData.sqlrun(lcSelect,"CustProf","","'DS01', 'sa', ''",2, "SAVE",SET("Datasession"))  &&ARIA1
  lnResult = oLoadData.sqlrun(lcSelect,"CustProf","",lcConnString,3, "SAVE",SET("Datasession"))  &&ARIANY
  IF lnResult >=1 .AND. &lcScanType &&  To validate active customer and the correct letter type.
    =lfvCustProf(&lcCustTmp..Account)
    =lfvCstSoftw(&lcCustTmp..Account)
    SELECT SUM(&lcDtlFile..nMtnc)AS nTotalPrice FROM (lcDtlFile) WHERE Account+cSortKey+cModId = &lcCustTmp..Account ;
    INTO ARRAY laTotal
    IF _TALLY > 0 AND laTotal[1] <> Null
      REPLACE &lcHdrFile..lnMantAmnt WITH laTotal[1]
    ENDIF 
  ENDIF 
ENDSCAN
*--#037909, HFK(Heba Fathi),03/08/2004

SELECT (lcHdrFile)
LOCATE
IF RECCOUNT() <= 0 
  WAIT WINDOW "No records to display"  NOWAIT 
  RETURN 
ENDIF 


IF lcFormName = 'ARSWMNTA'
  SELECT (lcHdrFile)
  INDEX ON cCountry TAG Country
  SET ORDER TO Country
  LOCATE 
ELSE 
  SELECT (lcDtlFile)
  SET RELATION TO ACCOUNT INTO &lcHdrFile
ENDIF 

DO gfDispRe WITH EVAL('lcFormName')
RETURN  


*!*************************************************************
*! Name      : lfvGetConnection
*! Purpose   : Create object from Remote Data Access Class "remotedataaccess".
*!*************************************************************
FUNCTION lfvGetConnection
oLoadData = CREATEOBJECT("remotedataaccess")
RETURN 

*!*************************************************************
*! Name      : lfvCustProf
*! Purpose   : 1- Open The CustProf Table.
*!           : 2- Get Fields Necessary For Customers.
*!*************************************************************
FUNCTION lfvCustProf
PARAMETERS lcCustID

LOCAL lnResult
lcWorkArea = SELECT()
lnRecNo    = RECNO()
lcSelect   = [SELECT cCust_Id, dCust_padu,nCust_Usrn FROM custprof WHERE cCust_Id =']+lcCustID+[']
*-lnResult = oLoadData.sqlrun(lcSelect,"CustProf","","'DS01', 'sa', ''",2, "SAVE",SET("Datasession"))  &&Aria1
lnResult = oLoadData.sqlrun(lcSelect,"CustProf","",lcConnString,3, "SAVE",SET("Datasession"))  &&ARIANY
IF lnResult >= 1
  SELECT CustProf
  INSERT INTO &lcHdrFile (Account          , nUserNo            , dExpirDate         );
                   VALUES(CustProf.cCust_Id, CustProf.nCust_Usrn, CustProf.dCust_padu) 
  =SEEK('M'+CustProf.cCust_Id,'CUSTOMER')
  REPLACE &lcHdrFile..acc_name   WITH CUSTOMER.btName,;
          &lcHdrFile..dDateFrom  WITH CustProf.dCust_PadU ,;
          &lcHdrFile..dDateTo    WITH CustProf.dCust_PadU ,;
          &lcHdrFile..cState     WITH CUSTOMER.cAddress4,;
          &lcHdrFile..cCountry   WITH  UPPER(CUSTOMER.cAddress6)
  REPLACE &lcHdrFile..dDateFrom  WITH &lcHdrFile..dDateFrom + 1 ,;
      	  &lcHdrFile..dDateTo    WITH &lcHdrFile..dDateTo + IIF(MONTH(&lcHdrFile..dDateTo)>2,IIF(MOD(YEAR(&lcHdrFile..dDateTo)+1, 4) = 0 , 366 ,  365 ),IIF(MOD(YEAR(&lcHdrFile..dDateTo), 4) = 0 , 366 ,  365 ))
ELSE
  =SEEK('M'+lcCustID,'CUSTOMER')
  INSERT INTO &lcHdrFile (Account,acc_name) VALUES(lcCustID,CUSTOMER.btName)
ENDIF 		  		

*B609484, tmi 12/20/2010 [start]
 SELECT &lchdrfile 
 REPLACE ntaxrate WITH customer.ntaxrate
*B609484, tmi 12/20/2010 [end]

SELECT (lcWorkArea)   
GOTO lnRecNo 
RETURN 

*!*************************************************************
*! Name      : lfvCstSoftw
*! Purpose   : 1- Open The CstSoftw Table.
*!           : 2- Get Fields Necessary For Customers.
*!*************************************************************

FUNCTION lfvCstSoftw
PARAMETERS lcCstID

LOCAL lnResult,lcAddUsr ,lcAddUsr1,lnTotal, lnTotal1

lcWorkArea = SELECT()
lcAppId='A27'
lcSelect  = [SELECT cCust_Id, capp_id,cmodtype,csoft_modu,nqty,nprice,cmod_id,lMaintain  FROM cStSoftW WHERE cCust_Id='] +lcCstID+ ['AND cstsoftw.capp_id='] +lcAppId+ [']
*-lnResult = oLoadData.sqlrun(lcSelect,"cStSoftW", "","'DS01', 'sa', ''", 2, "SAVE",SET("Datasession"))  &&Aria1
lnResult = oLoadData.sqlrun(lcSelect,"cStSoftW", "",lcConnString, 3, "SAVE",SET("Datasession"))  &&ARIANY

IF lnResult >=1
  STORE .T. TO llInclude 
  SELECT cStSoftW
  LOCATE 
		
  *-- Get Standard programs data 
  SCAN FOR cModType = 'S' AND lMaintain = .T. 
*SSH
*!*	    INSERT INTO &lcDtlFile (cSortKey ,Account          , cVer            , cModName           , nQty         , nPrice         , cModId          , nMtnc              , cModType);
*!*	                    VALUES ('A'      ,cStSoftW.cCust_Id, cStSoftW.capp_id, cStSoftW.csoft_modu, cStSoftW.nqty, cStSoftW.nprice, cStSoftW.cmod_id, cStSoftW.nprice*0.1, cStSoftW.cModType) 
    INSERT INTO &lcDtlFile (cSortKey ,Account          , cVer            , cModName           , nQty         , nPrice         , cModId          , nMtnc              , cModType,nMtnc16);
                    VALUES ('A'      ,cStSoftW.cCust_Id, cStSoftW.capp_id, cStSoftW.csoft_modu, cStSoftW.nqty, cStSoftW.nprice, cStSoftW.cmod_id, cStSoftW.nprice*.12, cStSoftW.cModType,cStSoftW.nprice*.16)


*SSH
    =lfvGetAppMode('A27',cStSoftW.cmod_id)
	*-- Get Module Price Depending On Number Of Users
    DO CASE
      CASE &lcHdrFile..nUserNo <=2 
*SSH
*!*	        REPLACE &lcDtlFile..NpRICE   WITH SuAppMod.nWinSngPr ,;
*!*	                &lcDtlFile..nMtnc    WITH &lcDtlFile..NpRICE *0.1

        REPLACE &lcDtlFile..NpRICE   WITH SuAppMod.nWinSngPr ,;
                &lcDtlFile..nMtnc    WITH &lcDtlFile..NpRICE *.12,;
                &lcDtlFile..nMtnc16  WITH &lcDtlFile..NpRICE *.16


*SSH                 
      CASE &lcHdrFile..nUserNo >=3 AND  &lcHdrFile..nUserNo < 5
*SSH
*!*	        REPLACE &lcDtlFile..NpRICE   WITH SuAppMod.nThreeUsr ,;
*!*	                &lcDtlFile..nMtnc   WITH &lcDtlFile..NpRICE *0.1

        REPLACE &lcDtlFile..NpRICE   WITH SuAppMod.nThreeUsr ,;
                &lcDtlFile..nMtnc   WITH &lcDtlFile..NpRICE *.12,;
                &lcDtlFile..nMtnc16   WITH &lcDtlFile..NpRICE *.16

*SSH
      CASE &lcHdrFile..nUserNo >= 5  
      *SSH
*!*	        REPLACE &lcDtlFile..NpRICE   WITH SuAppMod.nwinmltpr,;
*!*	                &lcDtlFile..nMtnc  WITH &lcDtlFile..nPrice*0.1
        REPLACE &lcDtlFile..NpRICE   WITH SuAppMod.nwinmltpr,;
                &lcDtlFile..nMtnc  WITH &lcDtlFile..nPrice*.12,;
                &lcDtlFile..nMtnc16   WITH &lcDtlFile..NpRICE *.16


*SSH
    ENDCASE
  ENDSCAN 
  
  *-- Get The Custom Program Data 
	SELECT cStSoftW
	llFirst = .F.
	LOCATE 
	SCAN FOR cModType = 'C' AND lMaintain = .T. 
    lcModName = 'CUSTOM PROGRAMS :' 
	  IF !llFirst 
	    llFirst =.T.
	    INSERT INTO &lcDtlFile (cSortKey,Account          , cModName ,nQty );
	                   VALUES   ('C'     ,cStSoftW.cCust_Id, lcModName,0 ) 
	  ENDIF 
*SSH
*!*		    INSERT INTO &lcDtlFile (cSortKey ,Account           , cVer            , cModName           , nQty         , nPrice, cModId , nMtnc , cModType);
*!*		                  VALUES ('C'      , cStSoftW.cCust_Id, cStSoftW.capp_id, cStSoftW.cSoft_Modu, 1           , cStSoftW.nprice, cStSoftW.cmod_id, cStSoftW.nPrice *0.1, cStSoftW.cModType) 

	    INSERT INTO &lcDtlFile (cSortKey ,Account           , cVer            , cModName           , nQty         , nPrice, cModId , nMtnc , cModType,nMtnc16);
	                  VALUES ('C'      , cStSoftW.cCust_Id, cStSoftW.capp_id, cStSoftW.cSoft_Modu, 1           , cStSoftW.nprice, cStSoftW.cmod_id, cStSoftW.nPrice *.12, cStSoftW.cModType,cStSoftW.nPrice *.16) 

*SSH
	  IF ISNULL(cStSoftW.nprice)
	      REPLACE &lcDtlFile..nPrice WITH 0.0
	 	  REPLACE &lcDtlFile..nMtnc WITH 0.0
	 	  REPLACE &lcDtlFile..nMtnc16 WITH 0.0
	  ENDIF   
	ENDSCAN 

  *-- Get the EDI Trading Partners data    
  SELECT cStSoftW
	lnEDITP  = 0 
	COUNT FOR  cModType = 'E' .AND. lMaintain = .T. TO lnEDITP 
	SCAN FOR cModType = 'E' AND lMaintain = .T. 
	 IF lnEDITP  > 0
	   lcModName = 'EDI TRADING PARTNER @ $975' 
	   INSERT INTO &lcDtlFile (cSortKey ,Account            , cVer            , cModName  , nQty    , nPrice         , cModId          , nMtnc              , cModType,nMtnc16);
	                   VALUES ('D',cStSoftW.cCust_Id,cStSoftW.capp_id, lcModName , lnEDITP ,  lnEDITP * 975 , cStSoftW.cmod_id, lnEDITP * 975 *.12, cStSoftW.cModType,lnEDITP * 975 *.16) 
	   EXIT
   ENDIF 
  ENDSCAN 
  *--#037909, HFK(Heba Fathi),03/09/2004
	*-To Get The Additonal Users Data.
	SELECT cStSoftW
  SCAN FOR cModType = 'U'  && additional users
    **mhm2
    *INSERT INTO &lcDtlFile (cSortKey ,nQty ,Account          , cVer            , cModName                    ,nPrice                                           ,nMtnc                                   ,cModId                 ,cModType,nMtnc16);
              VALUES       ('B'      ,cStSoftW.nqty ,cStSoftW.cCust_Id, cStSoftW.capp_id, cStSoftW.csoft_modu,cStSoftW.nqty*IIF(cStSoftw.cmod_id='US5',500,400),cStSoftW.nqty*IIF(cStSoftw.cmod_id='US5',500,400)*.12,cStSoftW.cmod_id,cStSoftW.cModType,cStSoftW.nqty*IIF(cStSoftw.cmod_id='US5',500,400)*.16) 

    INSERT INTO &lcDtlFile (cSortKey ,nQty ,Account          , cVer            , cModName                    ,nPrice                                           ,nMtnc                                   ,cModId                 ,cModType,nMtnc16);
              VALUES       ('B'      ,cStSoftW.nqty ,cStSoftW.cCust_Id, cStSoftW.capp_id, cStSoftW.csoft_modu,cStSoftW.nqty*IIF(cStSoftw.cmod_id='US5',500,IIF(cStSoftw.cmod_id='US4',400,1000)),cStSoftW.nqty*IIF(cStSoftw.cmod_id='US5',500,IIF(cStSoftw.cmod_id='US4',400,1000))*.12,cStSoftW.cmod_id,cStSoftW.cModType,cStSoftW.nqty*IIF(cStSoftw.cmod_id='US5',500,IIF(cStSoftw.cmod_id='US4',400,1000))*.16) 
  ENDSCAN 
  *--#037909, HFK(Heba Fathi),03/09/2004  
ENDIF 
SELECT (lcWorkArea)   
RETURN 
*!*************************************************************
*! Name      : lfvGetAppMode
*! Purpose   : 1- Open The SuAppMod Table.
*!           : 2- Get Fields Necessary For Customers.
*!*************************************************************
*-- Open The Application Module File
FUNCTION lfvGetAppMode
PARAMETERS lcApp_Id, lcMod_ID 
LOCAL lnResult

lcWorkArea = SELECT()
lcSelect = [SELECT nwinsngpr,nthreeusr,nwinmltpr FROM SuAppMod WHERE cApp_Id=']+lcApp_Id+[' AND cmod_id='] + lcMod_ID+[']
*-lnResult = oLoadData.sqlrun(lcSelect,"SuAppMod","","'DS01', 'sa', ''",2, "SAVE",SET("Datasession"))  &&ARIA1
lnResult = oLoadData.sqlrun(lcSelect,"SuAppMod","",lcConnString,3, "SAVE",SET("Datasession"))   &&ARIANY
SELECT (lcWorkArea)   
RETURN 

*!**************************************************************************
*! Name      : lfReadConStr
*! Purpose   : Read SQL connection string. (get it if not defined).
*!**************************************************************************
FUNCTION lfReadConStr

*--Read connection string.
lcConnFile = oAriaApplication.ResourceHome+"ConnInfo.txt"
IF !FILE(lcConnFile)
  lcConnStr = "driver={SQL Server};server=ariasrv1;DATABASE=DB01;uid=web;pwd=webaria"
  lcConnStr = INPUTBOX("Enter connection string","SQL Connection string",lcConnStr)
  IF EMPTY(lcConnStr)
    MESSAGEBOX("No server connection information are defined, unable to proceed!",16)
  ELSE
    =STRTOFILE(ALLTRIM(lcConnStr),lcConnFile,0)
  ENDIF
ELSE
  lcConnStr = ALLTRIM(FILETOSTR(lcConnFile))
ENDIF
RETURN lcConnStr
*!**************************************************************************
*! Name      : lfgetform()
*! Purpose   : 
*!**************************************************************************
FUNCTION lfvReport
IF lcRepType = 'S'
  lcFormName = 'ARSWMNTA'
ELSE 
  lcFormName = 'ARSWMNT'
ENDIF 


*!*************************************************************
*! Name      : lfCrtFile 
*! Purpose   : 1- Create TempFile For Report Detail 
*!           : 2- Create TempFile For Report Header
*!*************************************************************

FUNCTION lfCrtFile 

*-- Create Detail file 
DIMENSION laDtlFile[10,4]
* "A"  Standard modules
* "B"  Additional Users
* "C"  Custom Program
* "D" Trading Partner

laDtlFile[1,1] = 'cSortKey'
laDtlFile[1,2] = 'C'
laDtlFile[1,3] = 1
laDtlFile[1,4] = 0

laDtlFile[2,1] = 'Account'
laDtlFile[2,2] = 'C'
laDtlFile[2,3] = 5
laDtlFile[2,4] = 0

laDtlFile[3,1] = 'cModName'
laDtlFile[3,2] = 'C'
laDtlFile[3,3] = 100
laDtlFile[3,4] = 0

laDtlFile[4,1] = 'cVer'
laDtlFile[4,2] = 'C'
laDtlFile[4,3] = 5
laDtlFile[4,4] = 0

laDtlFile[5,1] = 'nQty'
laDtlFile[5,2] = 'N'
laDtlFile[5,3] = 2
laDtlFile[5,4] = 0


laDtlFile[6,1] = 'nPrice'
laDtlFile[6,2] = 'N'
laDtlFile[6,3] = 12
laDtlFile[6,4] = 2

laDtlFile[7,1] = 'nMtnc'
laDtlFile[7,2] = 'N'
laDtlFile[7,3] = 12
laDtlFile[7,4] = 2


laDtlFile[8,1] = 'cModId'
laDtlFile[8,2] = 'C'
laDtlFile[8,3] = 5
laDtlFile[8,4] = 0

* "S" Standard, "E" EDI , "C" Custom Program, "U" Users
laDtlFile[9,1] = 'cModType'
laDtlFile[9,2] = 'C'
laDtlFile[9,3] = 1
laDtlFile[9,4] = 0

laDtlFile[10,1] = 'nMtnc16'
laDtlFile[10,2] = 'N'
laDtlFile[10,3] = 12
laDtlFile[10,4] = 2

*-Create Temp File
=gfCrtTmp(lcDtlFile,@laDtlFile)
SELECT (lcDtlFile)
INDEX ON Account+cSortKey+cModId TAG SortAcct  

*-- Create Header file
DIMENSION laHdrFile[9,4]
laHdrFile[1,1] = 'Account'
laHdrFile[1,2] = 'C'
laHdrFile[1,3] = 5
laHdrFile[1,4] = 0

laHdrFile[2,1] = 'nUserNo'
laHdrFile[2,2] = 'N'
laHdrFile[2,3] = 3
laHdrFile[2,4] = 0

laHdrFile[3,1] = 'dExpirDate'
laHdrFile[3,2] = 'D'
laHdrFile[3,3] = 10
laHdrFile[3,4] = 0

laHdrFile[4,1] = 'acc_name'
laHdrFile[4,2] = 'C'
laHdrFile[4,3] = 60
laHdrFile[4,4] = 0


laHdrFile[5,1] = 'dDateFrom'
laHdrFile[5,2] = 'D'
laHdrFile[5,3] = 10
laHdrFile[5,4] = 0


laHdrFile[6,1] = 'dDateTo'
laHdrFile[6,2] = 'D'
laHdrFile[6,3] = 10
laHdrFile[6,4] = 0

laHdrFile[7,1] = 'cState'
laHdrFile[7,2] = 'C'
laHdrFile[7,3] = 2
laHdrFile[7,4] = 0


laHdrFile[8,1] = 'lnMantAmnt'
laHdrFile[8,2] = 'N'
laHdrFile[8,3] = 12
laHdrFile[8,4] = 2

laHdrFile[9,1] = 'cCountry'
laHdrFile[9,2] = 'C'
laHdrFile[9,3] = 20
laHdrFile[9,4] = 0

*B609484, tmi 12/20/2010 [start]
 lnarrlen = ALEN(lahdrfile, 1)+1
 DIMENSION lahdrfile[lnarrlen, 4]
 lahdrfile[lnarrlen, 1] = 'NTAXRATE'
 lahdrfile[lnarrlen, 2] = 'N'
 lahdrfile[lnarrlen, 3] = 7
 lahdrfile[lnarrlen, 4] = 3
*B609484, tmi 12/20/2010 [end]

*-CREATE TEMP FILE.
=gfCrtTmp(lcHdrFile ,@laHdrFile)
SELECT (lcHdrFile)
INDEX ON Account TAG Account 


FUNCTION lfWhen
