*!***************************************************************************************************************************
*: Program file  : ALSTPCR.PRG
*: Program desc. : Carrier Setup
*: Module        : System Manager
*: System        : Aria Apparel System
*: Developer     : Tarek Mohamed Ibrahim
*: Tracking Entry: *E302921,1 TMI 06/28/2011 [T20101207.0006]  entity # 01
*!***************************************************************************************************************************
*E303176,1 MMT 06/20/2012 Add 'Generate Test Files' Option to Carrier Setup program[T20120326.0056]
*B611893,1 Es 08/27/2019 Carrier Integration screen hangs after selecting order if user opened Edited Carrier credentails in Carrier Setup screen [T20190826.0010]

*mt
#iNCLUDE R:\Aria4XP\Screens\AL\ALSTPCR.h
*mt
lcCarrierRepID = 'ALSTPCR' && variable to hold the SYDREPRT.CREP_ID

lcClientID = oAriaApplication.READXml()
lnHandler = sqlstringconnect(oAriaApplication.sqlSysfilesConnectionString)

*!*	IF (lnHandler < 0)
*!*	  lcMsg = "Could not connect to SQL System Files. Either System.Master doesn't exist or invalid SQL creditials."
*!*	  gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
*!*	  RETURN
*!*	ENDIF

lcCarriersCursor = gfTempName()
lcSqlCommand = "SELECT ID,NAME FROM CARRIERS_T WHERE ID IN (SELECT CARRIER_ID FROM CLIENTS_CARRIER_T WHERE CLIENT_ID = '"+;
	lcClientID+"')"
lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlCommand,"",lcCarriersCursor,"",oAriaApplication.sqlSysfilesConnectionString,3,"",SET("DATASESSION"))

DIMENSION laCarrierName[1],laCarrierID[1]
SELECT NAME FROM &lcCarriersCursor INTO ARRAY laCarrierName
SELECT ID   FROM &lcCarriersCursor INTO ARRAY laCarrierID
USE IN &lcCarriersCursor

*- open the SYDREPRT and SYREPUVR tables
lcSelectStatement = 'SELECT * FROM SYDREPRT WHERE CREP_ID = "'+lcCarrierRepID+'"'
lnRemResult = oAriaApplication.RemoteSystemData.Execute(lcSelectStatement ,'',"SYDREPRT","",oAriaApplication.cAria4SysFiles,3,"",SET("Datasession"))

lcSelectStatement = 'SELECT * FROM SYREPUVR WHERE CREP_ID = "'+lcCarrierRepID+'" Order By cExpType, nVarPos'
lnRemResult = oAriaApplication.RemoteSystemData.Execute(lcSelectStatement ,'',"SYREPUVR","",oAriaApplication.cAria4SysFiles,3,"",SET("Datasession"))

*-Create a new connection string the looks at a different path other than the SQLDICTIONARY, in that path get the syrepuvr with the lines
*- come from the CARRIER_LOGIN_REQUIREMENTS_T all added
LOCAL lcTmpVar
lcTmpVar = oAriaApplication.cAria4Sysfiles
lnBeforePos = AT('SOURCEDB',UPPER(oAriaApplication.cAria4Sysfiles))
lcBefore = SUBSTR(oAriaApplication.cAria4Sysfiles,1,lnBeforePos-1)
oAriaApplication.cAria4Sysfiles = SUBSTR(oAriaApplication.cAria4Sysfiles,lnBeforePos)
lnAfterPos  = AT(';',oAriaApplication.cAria4Sysfiles)
lcAfter = SUBSTR(oAriaApplication.cAria4Sysfiles,lnAfterPos)

lcTempFldr = oAriaApplication.WorkDir+gfTempName()+'\'
oAriaApplication.cAria4Sysfiles = lcBefore + 'SourceDB='+lcTempFldr+ lcAfter

DIMENSION laMaxVarPos[1]
SELECT MAX(NVARPOS) FROM SYREPUVR INTO ARRAY laMaxVarPos

*- Fill the SYREPUVR from the CARRIER_LOGIN_REQUIREMENTS_T table
** the suppres values will be true for all lines unless it was selected
lcSqlCommand = "SELECT * FROM [CARRIER_LOGIN_REQUIREMENTS_T];"
lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlCommand,"",'CARRIER_LOGIN_REQUIREMENTS_T',"",oAriaApplication.sqlSysfilesConnectionString,3,"",SET("DATASESSION"))

lnResult  = oAriaApplication.remotesystemdata.execute("SELECT * FROM CARRIER_T","","CARRIER_T","",oAriaApplication.ActiveCompanyConStr,3,"",SET("DATASESSION"))
lnResult  = oAriaApplication.remotesystemdata.execute("SELECT * FROM CARRIER_LOGIN_INFORMATION_T","","CARRIER_LOGIN_INFORMATION_T","",oAriaApplication.ActiveCompanyConStr,3,"",SET("DATASESSION"))
lnCurrDataSession = SET("DATASESSION")
SELECT CARRIER_T
CURSORSETPROP("Buffering", 3, 'CARRIER_T')
INDEX ON CARRIER_ID TAG 'CARRIER_ID'

SELECT CARRIER_LOGIN_INFORMATION_T
CURSORSETPROP("Buffering", 3, 'CARRIER_LOGIN_INFORMATION_T')
INDEX ON CARRIER_ID+LOGIN_REQUIREMENT_ID TAG 'CARRIER_ID'

SELECT CARRIER_LOGIN_REQUIREMENTS_T
CURSORSETPROP("Buffering", 3, 'CARRIER_LOGIN_REQUIREMENTS_T')
INDEX ON CARRIER_ID+REQUIREMENT_ID TAG 'CARR_REQ'
INDEX ON CARRIER_ID+STR(DISPLAY_SEQUENCE,6) TAG 'CARRIER_ID'

SELECT CARRIER_LOGIN_REQUIREMENTS_T
LOCATE

lnVarPos = laMaxVarPos
lnDumm = 0

DO WHILE !EOF('CARRIER_LOGIN_REQUIREMENTS_T')
	SELECT CARRIER_LOGIN_REQUIREMENTS_T
	lcCarrierID = CARRIER_ID

	SCATTER MEMVAR MEMO
	SELECT SYREPUVR
	APPEND BLANK

	=SEEK(m.CARRIER_ID,'CARRIER_T')
	lnVarPos = lnVarPos + 1
	REPLACE CREP_ID    WITH lcCarrierRepID ;
		MFLD_NAME  WITH ALLTRIM(lcCarrierID)+'_STATUS';
		CDATA_TYP  WITH 'C';
		CFLD_HEAD  WITH 'Status';
		MFLD_DES   WITH 'Status';
		NFLD_DEC   WITH 0;
		NFLD_WDTH  WITH 1;
		CFLD_MSG   WITH 'Status';
		LASKRUNT   WITH .T.;
		CEXPTYPE   WITH 'V';
		CDEFA_TYP  WITH 'V';
		MDATA_DEF  WITH CARRIER_T.STATUS ;
		NVARPOS    WITH lnVarPos ;
		LDISPOG    WITH .T.;
		COBJ_TYPE  WITH 'A';
		CUPGRDLVL  WITH 'A';
		COWNER     WITH lcCarrierID ;
		MSUPEXPR   WITH "EMPTY(lcCarrier) OR lcCarrier<>'"+lcCarrierID+"'" ;
		MVENTRIES  WITH 'Active|Hold~A|H'


	SELECT CARRIER_LOGIN_REQUIREMENTS_T
	SCAN REST WHILE CARRIER_ID = lcCarrierID
		SCATTER MEMVAR MEMO

		SELECT SYREPUVR
		APPEND BLANK

		lcKey = m.CARRIER_ID + m.REQUIREMENT_ID
		=SEEK(lcKey,'CARRIER_LOGIN_INFORMATION_T')

		lnVarPos = lnVarPos + 1

		REPLACE CREP_ID    WITH lcCarrierRepID ;
			MFLD_NAME  WITH ALLTRIM(lcCarrierID)+'_'+ALLTRIM(m.REQUIREMENT_ID);
			CDATA_TYP  WITH m.REQUIREMENT_TYPE;
			CFLD_HEAD  WITH m.REQUIREMENT_NAME;
			MFLD_DES   WITH m.REQUIREMENT_NAME;
			NFLD_DEC   WITH 0;
			NFLD_WDTH  WITH m.REQUIREMENT_LENGTH;
			CFLD_MSG   WITH m.REQUIREMENT_NAME;
			LASKRUNT   WITH .T.;
			CEXPTYPE   WITH 'V';
			CDEFA_TYP  WITH 'V';
			MDATA_DEF  WITH CARRIER_LOGIN_INFORMATION_T.LOGIN_REQUIREMENT_VALUE ;
			NVARPOS    WITH lnVarPos ;
			LDISPOG    WITH .T.;
			COBJ_TYPE  WITH 'A';
			CUPGRDLVL  WITH 'A';
			COWNER     WITH lcCarrierID ;
			MSUPEXPR   WITH "EMPTY(lcCarrier) OR lcCarrier<>'"+lcCarrierID+"'"
	ENDSCAN

	=SEEK(lcCarrierID,'CARRIER_T')
	DIMENSION laCarrierData[4,2]
	laCarrierData[1,1] = 'Total cartons shipped'
	laCarrierData[1,2] = IIF(ISNULL(CARRIER_T.SHIPPED_CARTONS),0,CARRIER_T.SHIPPED_CARTONS)
	laCarrierData[2,1] = 'Total weight shipped'
	laCarrierData[2,2] = IIF(ISNULL(CARRIER_T.SHIPPED_WEIGHT),0,CARRIER_T.SHIPPED_WEIGHT)
	laCarrierData[3,1] = 'Total carrier freight'
	laCarrierData[3,2] = IIF(ISNULL(CARRIER_T.TOTAL_FREIGHT),0,CARRIER_T.TOTAL_FREIGHT)
	laCarrierData[4,1] = 'Remaining Balance'
	laCarrierData[4,2] = IIF(ISNULL(CARRIER_T.BALANCE),0,CARRIER_T.BALANCE)


	FOR lnI = 1 TO ALEN(laCarrierData,1)

		SELECT SYREPUVR
		APPEND BLANK

		lnVarPos = lnVarPos + 1
		lnDumm = lnDumm + 1
		REPLACE CREP_ID    WITH lcCarrierRepID ;
			MFLD_NAME  WITH 'lcDummy'+ALLTRIM(STR(lnDumm));
			CDATA_TYP  WITH 'N';
			CFLD_HEAD  WITH laCarrierData[lnI,1];
			MVALD_STR  WITH 'WHEN .F.' ;
			MFLD_DES   WITH laCarrierData[lnI,1];
			NFLD_DEC   WITH 0;
			NFLD_WDTH  WITH 10;
			CFLD_MSG   WITH laCarrierData[lnI,1];
			LASKRUNT   WITH .T.;
			CEXPTYPE   WITH 'V';
			CDEFA_TYP  WITH 'V';
			MDATA_DEF  WITH ALLT(STR(laCarrierData[lnI,2]));
			NVARPOS    WITH lnVarPos ;
			LDISPOG    WITH .T.;
			COBJ_TYPE  WITH 'A';
			CUPGRDLVL  WITH 'A';
			COWNER     WITH lcCarrierID ;
			MSUPEXPR   WITH "EMPTY(lcCarrier) OR lcCarrier<>'"+lcCarrierID+"'"
	ENDFOR
ENDDO

*E303176,1 MMT 06/20/2012 Add 'Generate Test Files' Option to Carrier Setup program[T20120326.0056][Start]
SELECT SYREPUVR
APPEND BLANK
lnVarPos = lnVarPos + 1
REPLACE CREP_ID    WITH lcCarrierRepID ;
	MFLD_NAME  WITH 'LLRPGENFL';
	CDATA_TYP  WITH 'L';
	CFLD_HEAD  WITH 'Generate UPS Test Files';
	MVALD_STR  WITH 'ClearRead()' ;
	MFLD_DES   WITH 'Generate UPS Test Files';
	NFLD_DEC   WITH 0;
	NFLD_WDTH  WITH 1;
	CFLD_MSG   WITH 'Generate UPS Test Files';
	LASKRUNT   WITH .T.;
	CEXPTYPE   WITH 'V';
	CDEFA_TYP  WITH 'V';
	MDATA_DEF  WITH '.F.';
	NVARPOS    WITH lnVarPos ;
	LDISPOG    WITH .T.;
	COBJ_TYPE  WITH 'A';
	CUPGRDLVL  WITH 'A';
	COWNER     WITH '' ;
	MSUPEXPR   WITH "lcCarrier<>'UPS'"

SELECT SYREPUVR
APPEND BLANK
lnVarPos = lnVarPos + 1
REPLACE CREP_ID    WITH lcCarrierRepID ;
	MFLD_NAME  WITH 'LCRPFLPTH';
	CDATA_TYP  WITH 'C';
	CFLD_HEAD  WITH 'Test Files Path';
	MVALD_STR  WITH 'lfvTPath()' ;
	MFLD_DES   WITH 'Test Files Path';
	NFLD_DEC   WITH 0;
	NFLD_WDTH  WITH 100;
	CFLD_MSG   WITH 'Test Files Path';
	LASKRUNT   WITH .T.;
	CEXPTYPE   WITH 'V';
	CDEFA_TYP  WITH 'V';
	MDATA_DEF  WITH "";
	NVARPOS    WITH lnVarPos ;
	LDISPOG    WITH .T.;
	COBJ_TYPE  WITH '';
	CUPGRDLVL  WITH 'A';
	COWNER     WITH '' ;
	MSUPEXPR   WITH "lcCarrier<>'UPS' or !LLRPGENFL"
*E303176,1 MMT 06/20/2012 Add 'Generate Test Files' Option to Carrier Setup program[T20120326.0056][END]


*- copy the filled data to the temp folder
MD (lcTempFldr)
SELECT SYDREPRT
COPY TO (lcTempFldr+'SYDREPRT')
SELECT SYREPUVR
COPY TO (lcTempFldr+'SYREPUVR')

USE IN SYDREPRT
USE IN SYREPUVR

lcExpr = gfOpGrid(lcCarrierRepID,.T.)

*- Restore the old value of the oAriaApplication.cAria4Sysfiles
oAriaApplication.cAria4Sysfiles = lcTmpVar

NOTE  currently this does NOT work [start]
*- remove the temp created fake table
*ERASE (lcTempFldr+'*.*')
*RD &lcTempFldr
NOTE [end]

*- Close temp cursors
IF USED('CARRIER_T')
	USE IN CARRIER_T
ENDIF
IF USED('CARRIER_LOGIN_INFORMATION_T')
	USE IN CARRIER_LOGIN_INFORMATION_T
ENDIF
IF USED('CARRIER_LOGIN_REQUIREMENTS_T')
	USE IN CARRIER_LOGIN_REQUIREMENTS_T
ENDIF


********************************************************************************************************
*
*   FUNCTION lfvCarrier
*
********************************************************************************************************
FUNCTION lfvCarrier

*E302921 TMI 07/10/2011 [Start] if no carrier was selected then do nothing
IF EMPTY(lcCarrier) OR ISNULL(lcCarrier)
	RETURN
ENDIF
*E302921 TMI 07/10/2011 [End  ]

*- Show/Hide related login requiremnts
clearread()

*- Disable fields showing collected data
lnPos = ASCAN(laOgObjType,'LCDUMMY')
DO WHILE lnPos > 0
	lnFldPos = ASUBSCRIPT(laOgObjType,lnPos,1)
	laOgObjCnt[lnFldPos] = .F.
	=LFOGSHOWGET(laOgObjType[lnFldPos,1])
	lnPos = ASCAN(laOgObjType,'LCDUMMY',lnPos+1)
ENDDO

********************************************************************************************************
*
*   FUNCTION lfWhenOG
*
********************************************************************************************************
FUNCTION lfWhenOG


********************************************************************************************************
*
*   FUNCTION lfSaveCarrierInfo
*
********************************************************************************************************
FUNCTION lfSaveCarrierInfo

*E303176,1 MMT 06/20/2012 Add 'Generate Test Files' Option to Carrier Setup program[T20120326.0056][Start]
IF lcCarrier = 'UPS' AND LLRPGENFL
	IF EMPTY(LCRPFLPTH) OR !DIRECTORY(LCRPFLPTH)
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*=gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Invalid Test Files Path')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B00000',.F.,.F.,.F.,LANG_TEST_PATH)
		=gfModalGen('TRM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TEST_PATH,loFormSet.GetHeaderText("LANG_TEST_PATH",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[end]
		RETURN .F.
	ENDIF
	lcAccountBill = gfGetMemVar('XUPSACCT', oAriaApplication.ActiveCompanyID)
	loUPSDLL = CREATEOBJECT('UPS.UPS')
	lcLicenseNum = EVALUATE('loOgScroll.'+'UPS_AccessLicenseNumber')
	lcUserName = EVALUATE('loOgScroll.'+'UPS_Username')
	lcPw = EVALUATE('loOgScroll.'+'UPS_Password')
	loUPSDLL.GenerateCertifyFiles('&lcLicenseNum.','&lcUserName.','&lcPw.',lcAccountBill,LCRPFLPTH)
	IF !EMPTY(loUPSDLL.ErrorMsg)
		=gfModalGen('TRM00000B00000',.F.,.F.,.F.,loUPSDLL.ErrorMsg)
		RETURN .F.
	ENDIF
ENDIF
*E303176,1 MMT 06/20/2012 Add 'Generate Test Files' Option to Carrier Setup program[T20120326.0056][End]

LOCAL lnI , lcCarrier,lcStatus,lnSvDataSession ,lcCr
lcCr = CHR(13)
lcTmpCur = gfTempName()
lnSvDataSession = SET("Datasession")
SET DATASESSION TO lnCurrDataSession
*B611893,1 Es 08/27/2019 Carrier Integration screen hangs after selecting order if user opened Edited Carrier credentails in Carrier Setup screen [Start]
Local lnConnHandler
lnConnHandler = 0
lnResult  = 0
*B611893,1 Es 08/27/2019 Carrier Integration screen hangs after selecting order if user opened Edited Carrier credentails in Carrier Setup screen [End]


FOR lnI = 1 TO ALEN(laCarrierID,1)
	lcCarrier = laCarrierID[lnI]
	lcStatus = EVALUATE('loOgScroll.'+ALLTRIM(laCarrierID[lnI])+'_STATUS')

*- Save Carrier data
	IF !EMPTY(lcStatus)

		IF !SEEK(lcCarrier,'CARRIER_T')
			lcSqlStatement = "INSERT INTO CARRIER_T (CARRIER_ID,STATUS) VALUES ('&lcCarrier','&lcStatus')"
		ELSE
			lcSqlStatement = "UPDATE CARRIER_T SET STATUS='&lcStatus' WHERE CARRIER_ID='&lcCarrier'"
		ENDIF
*B611893,1 Es 08/27/2019 Carrier Integration screen hangs after selecting order if user opened Edited Carrier credentails in Carrier Setup screen [Start]
*lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlStatement,"","","",oAriaApplication.ActiveCompanyConStr,3,"",SET("DATASESSION"))
		lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlStatement,"","","",oAriaApplication.ActiveCompanyConStr,3,"",SET("DATASESSION"),.F.,@lnConnHandler)
		IF  lnResult  < 0
			EXIT
		ENDIF
*B611893,1 Es 08/27/2019 Carrier Integration screen hangs after selecting order if user opened Edited Carrier credentails in Carrier Setup screen [End]


*- Save the Login Information data
		IF !USED('SYREPUVR')
			USE (lcTempFldr+'SYREPUVR') IN 0
		ENDIF
		SELECT SYREPUVR
		LOCATE
		SCAN FOR MFLD_NAME = ALLTRIM(laCarrierID[lnI])+'_' AND MFLD_NAME <> ALLTRIM(laCarrierID[lnI])+'_STATUS'
			lcReqID = PADR(SUBSTR(MFLD_NAME,AT('_',MFLD_NAME)+1),20)
			lcReqVal = EVALUATE('loOgScroll.'+MFLD_NAME)
			IF !EMPTY(lcReqVal)
				=SEEK(lcCarrier+lcReqID,'CARRIER_LOGIN_REQUIREMENTS_T','CARR_REQ')
				lcReqKey = ALLTRIM(CARRIER_LOGIN_REQUIREMENTS_T.LOGIN_REQUIREMENTS_KEY)
				IF !SEEK(lcCarrier+lcReqID,'CARRIER_LOGIN_INFORMATION_T')
					lcSqlStatement = "INSERT INTO CARRIER_LOGIN_INFORMATION_T "+;
						"(CARRIER_ID,LOGIN_REQUIREMENT_ID,LOGIN_REQUIREMENT_VALUE,LOGIN_REQUIREMENTS_KEY) VALUES "+;
						"('&lcCarrier','&lcReqID','&lcReqVal',&lcReqKey)"
				ELSE
					lcSqlStatement = "UPDATE CARRIER_LOGIN_INFORMATION_T SET LOGIN_REQUIREMENT_VALUE='&lcReqVal' WHERE LOGIN_REQUIREMENTS_KEY=&lcReqKey"
				ENDIF

*B611893,1 Es 08/27/2019 Carrier Integration screen hangs after selecting order if user opened Edited Carrier credentails in Carrier Setup screen [Start]
*lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlStatement,"","","",oAriaApplication.ActiveCompanyConStr,3,"",SET("DATASESSION"))
				lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlStatement,"","","",oAriaApplication.ActiveCompanyConStr,3,"",SET("DATASESSION"),.F.,@lnConnHandler)
				IF lnResult  < 0
					EXIT
				Endif
*B611893,1 Es 08/27/2019 Carrier Integration screen hangs after selecting order if user opened Edited Carrier credentails in Carrier Setup screen [End]
			ELSE
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Some fields are empty, all fields should be filled out')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_EMPTY_FIELDS)
				=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_EMPTY_FIELDS,loFormSet.GetHeaderText("LANG_EMPTY_FIELDS",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[end]
				RETURN .F.
			ENDIF
		ENDSCAN

	ENDIF

ENDFOR


*B611893,1 Es 08/27/2019 Carrier Integration screen hangs after selecting order if user opened Edited Carrier credentails in Carrier Setup screen [Start]
IF lnResult  > 0
	Sqlcommit(lnConnHandler)
Else
	IF lnConnHandler > 0
		=oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnResult  ,.T.)
		Sqlrollback(lnConnHandler)
	ENDIF
ENDIF
*B611893,1 Es 08/27/2019 Carrier Integration screen hangs after selecting order if user opened Edited Carrier credentails in Carrier Setup screen [End]

USE IN SYREPUVR
SET DATASESSION TO lnSvDataSession
*- End of FUNCTION lfSaveCarrierInfo


*E303176,1 MMT 06/20/2012 Add 'Generate Test Files' Option to Carrier Setup program[T20120326.0056][Start]
********************************************************************************************************
*
*   FUNCTION lfvTPath
*
********************************************************************************************************
FUNCTION lfvTPath
IF ALLTRIM(LCRPFLPTH)  = "?" OR !DIRECTORY(LCRPFLPTH)
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*LCRPFLPTH = GETDIR('','Select Test Files Path')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*LCRPFLPTH = GETDIR('',LANG_SELECTPATH)
	LCRPFLPTH = GETDIR('',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTPATH,loFormSet.GetHeaderText("LANG_SELECTPATH",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[end]
ENDIF
*E303176,1 MMT 06/20/2012 Add 'Generate Test Files' Option to Carrier Setup program[T20120326.0056][End]
