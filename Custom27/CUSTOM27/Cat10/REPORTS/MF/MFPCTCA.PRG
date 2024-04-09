LPARAMETERS lcMemoFile
IF TYPE('lcMemoFile')#'C'
	RETURN
ENDIF
RESTORE FROM (lcMemoFile) ADDI

HIDE MENU _MSYSMENU
_SCREEN.CAPTION = 'Cutting Ticket'
_SCREEN.WINDOWSTATE = 2
DEFINE WINDOW PRTWIND FROM 0,0 TO SROWS()-1,SCOLS();
	TITLE 'Cutting Ticket';
	SYSTEM CLOSE FLOAT GROW
MOVE WINDOW PRTWIND CENTER

IF !USED(lcRpTmpFle)
	USE (ALLTRIM(lcRpWrDir)+lcRpTmpFle) IN 0
ENDIF

IF !USED('Objlink')
	USE (ALLTRIM(lcRpDtDir)+'Objlink') IN 0
ENDIF
IF !USED('Objects')
	USE (ALLTRIM(lcRpDtDir)+'Objects') IN 0
ENDIF
IF !USED('Scale')
	USE (ALLTRIM(lcRpDtDir)+'Scale') IN 0
ENDIF
*C102114,1 AMH 04/04/2001 open CUTTKTH file [Start]
IF !USED('CUTTKTH')
	USE (ALLTRIM(lcRpDtDir)+'CUTTKTH') IN 0
ENDIF
*C102114,1 AMH [End]

SELECT (lcRpTmpFle)
*B604830,1 AMH Add index to help in collecting the Special Instructions data [Start]
*INDEX ON cCutTkt+ctype TAG TmpMaj OF (ALLTRIM(lcRpWrDir) + lcRpTmpFle)

*B605790,1 AMH Sorting fabrics according to desc. [Start]
*INDEX ON cCutTkt+ctype+cItem_Type+cFabric+cMarker TAG TmpMaj OF (ALLTRIM(lcRpWrDir) + lcRpTmpFle)
INDEX ON cCutTkt+ctype+cItem_Type+cSortDesc+cFabric+cMarker TAG TmpMaj OF (ALLTRIM(lcRpWrDir) + lcRpTmpFle)
*B605790,1 AMH [End]

INDEX ON ctype+cCutTkt+cFabric+cMarker TAG TmpTyp OF (ALLTRIM(lcRpWrDir) + lcRpTmpFle)
*B604830,1 AMH [End]
SET ORDER TO TmpMaj ASCENDING

SELECT OBJECTS
SET ORDER TO TAG Objectid

SELECT OBJLINK
SET ORDER TO TAG ObjLnkty
SET RELATION TO OBJLINK.cobject_id INTO OBJECTS ADDITIVE

SELECT SCALE
SET ORDER TO TAG SCALE

*C102114,1 AMH 04/04/2001 open index of CUTTKTH file [Start]
SELECT CUTTKTH
SET ORDER TO TAG CUTTKTH
*C102114,1 AMH [End]

SELECT (lcRpTmpFle)
SET RELATION TO 'S'+ cMajor INTO OBJLINK ADDITIVE
SET RELATION TO 'S'+ cScale INTO SCALE ADDITIVE

*C102114,1 AMH 04/04/2001 create relation between lcRpTmpFle and CUTTKTH [Start]
SET RELATION TO cCutTkt INTO CUTTKTH ADDITIVE
*C102114,1 AMH [End]

*C102114,1 AMH 04/3/2001 Connect to SPPITEM file [Start]
LOCAL lcSrvr, lcOldClassLib
lcSrvr = ''
lcOldClassLib = SET('CLASSLIB')
SET CLASSLIB TO (lcRpDefPth+'classes\cathy.vcx')
oReg = CREATEOBJECT("Registry")
lnError = oReg.GetRegKey("SQLServer",@lcSrvr,"Aria Connections\AriaServer",0)
lcConnStr='DRIVER={SQL Server};Server='+lcSrvr+';Persist Security Info=True;'+;
	'Initial Catalog=DB'+lcRpComp+';uid=aria;pwd=ariaadmn;database=DB'+lcRpComp
lnConnectionHandle = SQLSTRINGCONNECT(lcConnStr)
IF lnConnectionHandle > 0
	lnConnHandler = SQLSETPROP(lnConnectionHandle , "Asynchronous",.F.)
	IF lnConnHandler > 0
		lnConnectionSet=SQLEXEC(lnConnHandler ,;
			'SELECT * FROM SPPITEM ORDER BY cPatern_No,cType,cKey_Fld,dDate','TmpSppItem')
		IF lnConnectionSet > 0
			SELECT TmpSppItem
			INDEX ON cPatern_No+cRev_No+cKey_Fld TAG TmpSppItem
			SELECT (lcRpTmpFle)
			SET RELATION TO cPattern+cRev_No+cMarker2 INTO TmpSppItem ADDITIVE
		ELSE
			=Server_Error()
			RELEASE WINDOW PRTWIND
			SHOW MENU _MSYSMENU
			RETURN
		ENDIF
*B604830,1 AMH Open the needed cursor to print the Special Instructions [Start]
		lnConnectionSet=SQLEXEC(lnConnHandler ,;
			'SELECT * FROM SPPATRN ORDER BY CPATERN_NO','TmpSpPatrn')
		IF lnConnectionSet > 0
			SELECT TmpSpPatrn
			INDEX ON cPatern_No TAG TmpSpPatrn
		ELSE
			=Server_Error()
			RELEASE WINDOW PRTWIND
			SHOW MENU _MSYSMENU
			RETURN
		ENDIF
		lnConnectionSet=SQLEXEC(lnConnHandler ,;
			'SELECT SPMATCUT.CPATERN_NO AS PatternNo,SPMATCUT.CFABRIC AS Fabric,'+;
			'SPMATCUT.CFABDESCSP AS FabricDescription,SPMATCUT.NTOTQTY AS TotYrdPerDoz,'+;
			'SPMATPOS.CPOSITION AS Position,SPMATPOS.NPIECES AS Pieces,SPMATPOS.CWIDTH AS width,'+;
			'SPMATPOS.NROUND_YRD AS Roundyard,SPMATPOS.NTOTQTY AS Yards '+;
			'FROM SPMATCUT LEFT OUTER JOIN SPMATPOS ON SPMATCUT.CPATERN_NO + SPMATCUT.CFABRIC ='+;
			'SPMATPOS.CPATERN_NO + SPMATPOS.CFABRIC ORDER BY SPMATCUT.CPATERN_NO,SPMATCUT.CFABRIC','TmpSpMatPos')
		IF lnConnectionSet > 0
			SELECT TmpSpMatPos
			INDEX ON PATTERNNO+Fabric+Position TAG TmpSpMatPs
		ELSE
			=Server_Error()
			RELEASE WINDOW PRTWIND
			SHOW MENU _MSYSMENU
			RETURN
		ENDIF
		lnConnectionSet=SQLEXEC(lnConnHandler ,;
			'SELECT * FROM SPSCLDT ORDER BY CSPSCALE,NSIZE','TmpSpSclDt')
		IF lnConnectionSet > 0
			SELECT TmpSpSclDt
			INDEX ON cSpScale+STR(NSIZE,1) TAG TmpSpSclDt
			SELECT TmpSpPatrn
			SET RELATION TO cSpScale INTO TmpSpSclDt ADDITIVE
		ELSE
			=Server_Error()
			RELEASE WINDOW PRTWIND
			SHOW MENU _MSYSMENU
			RETURN
		ENDIF
		CREATE CURSOR TmpSpInstSze ;
			(cPattern C(10), cSize1 C(9), cSize2 C(9), cSize3 C(9), cSize4 C(9), cSize5 C(9), cSize6 C(9), cSize7 C(7), cSize8 C(8))
		INDEX ON cPattern TAG TmpSpInsSz
		lnConnectionSet=SQLEXEC(lnConnHandler ,;
			'SELECT * FROM SPMATSze ORDER BY CPATERN_NO,CFABRIC,CPOSITION','TmpSpMatSze')
		IF lnConnectionSet > 0
			SELECT TmpSpMatSze
			INDEX ON cPatern_No+cFabric+cPosition TAG TmpSpMatSz
			SELECT TmpSpMatPos
			SET RELATION TO PATTERNNO+Fabric+Position INTO TmpSpMatSze ADDITIVE
		ELSE
			=Server_Error()
			RELEASE WINDOW PRTWIND
			SHOW MENU _MSYSMENU
			RETURN
		ENDIF
		
		* B605242,1 AMH Change the cposition field width [Start]
		*CREATE CURSOR TmpSpInst ;
			(cPattern C(10), cFabric C(7), cPosition C(10), cWidth C(6), nPieces N(8,0), nRound_Yrd N(8,3), nTotQty N(11,3), cSizeVal1 C(10), cSizeVal2 C(10),;
			cSizeVal3 C(10), cSizeVal4 C(10), cSizeVal5 C(10), cSizeVal6 C(10), cSizeVal7 C(10), cSizeVal8 C(10), CFABDESCSP C(35))
		CREATE CURSOR TmpSpInst ;
			(cPattern C(10), cFabric C(7), cPosition C(20), cWidth C(6), nPieces N(8,0), nRound_Yrd N(8,3), nTotQty N(11,3), cSizeVal1 C(10), cSizeVal2 C(10),;
			cSizeVal3 C(10), cSizeVal4 C(10), cSizeVal5 C(10), cSizeVal6 C(10), cSizeVal7 C(10), cSizeVal8 C(10), CFABDESCSP C(35))
		* B605242,1 AMH [End]
		
		INDEX ON cPattern+cFabric+cPosition TAG TmpSpInst
*B604830,1 AMH [End]
	ELSE
		=Server_Error()
		RELEASE WINDOW PRTWIND
		SHOW MENU _MSYSMENU
		RETURN
	ENDIF
ELSE
	=Server_Error()
	RELEASE WINDOW PRTWIND
	SHOW MENU _MSYSMENU
	RETURN
ENDIF
SET CLASSLIB TO &lcOldClassLib.
*C102114,1 AMH [End]

*B604830,1 AMH Collecting the Special Instructions Data [Start]
SELECT TmpSpPatrn
LOCATE
SCAN
	SELECT TmpSpSclDt
	SCAN REST WHILE cSpScale+STR(NSIZE,1) = TmpSpPatrn.cSpScale
		lcSize = STR(NSIZE,1)
		SELECT TmpSpInstSze
		IF SEEK(TmpSpPatrn.cPatern_No)
			REPLACE cSize&lcSize.   WITH TmpSpSclDt.cSize
		ELSE
			APPEND BLANK
			REPLACE cPattern        WITH TmpSpPatrn.cPatern_No,;
      				cSize&lcSize.   WITH TmpSpSclDt.cSize
		ENDIF
	ENDSCAN
ENDSCAN
SELECT (lcRpTmpFle)
SET ORDER TO TAG TmpTyp
SEEK 'I '
SCAN REST WHILE ctype+cCutTkt+cFabric+cMarker = 'I '
	lcTmpKey = EVALUATE(KEY())
	lcCutTkt = cCutTkt
	lcPattern = cPattern
	IF SEEK(cPattern,'TmpSpMatPos')
		SELECT TmpSpMatPos
		
		*B605388,1 AMH Initializing fabric, count of positions , total y/z. [Start]
		lcFabric = SPACE(7)
		lnCount = 0
		lnTotQty = 0
		*B605388,1 AMH [End]
		
		SCAN REST WHILE PATTERNNO+Fabric+Position = lcPattern
	  	
	  	*B605388,1 AMH Check if new fabric and has more than one position [Start]
			IF FABRIC # lcFabric
			  IF lnCount > 1
     		 SELECT (lcRpTmpFle)
			    APPEND BLANK
    			REPLACE ctype     WITH 'IN',;
				          cCutTkt   WITH lcCutTkt,;
      				    cPattern  WITH lcPattern,;
     		 		    cFabric   WITH lcFabric,;
      				    cPosition WITH 'ZZZZZZZZZZZZZZZZZZZZ'
  				SELECT TmpSpInst
  				APPEND BLANK
					REPLACE cPattern         WITH lcPattern,;
	      					cFabric          WITH lcFabric,;
      						cPosition        WITH 'ZZZZZZZZZZZZZZZZZZZZ',;
      						nTotQty          WITH lnTotQty
        ENDIF
			  lcFabric = FABRIC
  		  lnCount = 0
			  lnTotQty = 0
			ENDIF
 	   *B605388,1 AMH [End]
 	   
 		 SELECT (lcRpTmpFle)
			APPEND BLANK
			REPLACE ctype     WITH 'IN',;
				      cCutTkt   WITH lcCutTkt,;
      				cPattern  WITH lcPattern,;
     		 		cFabric   WITH TmpSpMatPos.Fabric,;
      				cPosition WITH TmpSpMatPos.Position
			SELECT TmpSpMatSze
			SCAN REST WHILE cPatern_No+cFabric+cPosition = lcPattern+TmpSpMatPos.Fabric+TmpSpMatPos.Position
				lcSize = STR(NSIZE,1)
				SELECT TmpSpInst
				IF SEEK(TmpSpMatSze.cPatern_No+TmpSpMatSze.cFabric+TmpSpMatSze.cPosition)
					REPLACE cSizeVal&lcSize. WITH IIF(ISNULL(TmpSpMatSze.cSizeVal),'',TmpSpMatSze.cSizeVal)
				ELSE
					
	  	    *B605388,1 AMH Count Positions [Start]
					lnCount = lnCount + 1
	      	*B605388,1 AMH [End]
	      	
					APPEND BLANK
					REPLACE cPattern         WITH TmpSpMatSze.cPatern_No,;
	      					cFabric          WITH TmpSpMatSze.cFabric,;
      						cPosition        WITH TmpSpMatSze.cPosition,;
  						    cWidth           WITH IIF(ISNULL(TmpSpMatPos.WIDTH),'',TmpSpMatPos.WIDTH),;
      						nPieces          WITH IIF(ISNULL(TmpSpMatPos.Pieces),0,TmpSpMatPos.Pieces),;
   		 	        nRound_Yrd       WITH IIF(ISNULL(TmpSpMatPos.RoundYard),0,TmpSpMatPos.RoundYard),;
      						nTotQty          WITH IIF(ISNULL(TmpSpMatPos.Yards),0,ROUND(TmpSpMatPos.Yards*12,2)),;
      						CFABDESCSP       WITH IIF(ISNULL(TmpSpMatPos.FabricDescription),'',TmpSpMatPos.FabricDescription),;
				  		    cSizeVal&lcSize. WITH IIF(ISNULL(TmpSpMatSze.cSizeVal),'',TmpSpMatSze.cSizeVal)
				  
	      	*B605388,1 AMH Calculate the total Y/Z [Start]
				  lnTotQty = lnTotQty + nTotQty
	      	*B605388,1 AMH [End]
	      	
				ENDIF
			ENDSCAN
		ENDSCAN
		
  	*B605388,1 AMH Check if the last fabric has more than one position [Start]
	  IF lnCount > 1
      SELECT (lcRpTmpFle)
			APPEND BLANK
    	REPLACE ctype     WITH 'IN',;
				      cCutTkt   WITH lcCutTkt,;
      				cPattern  WITH lcPattern,;
     		 		cFabric   WITH lcFabric,;
      				cPosition WITH 'ZZZZZZZZZZZZZZZZZZZZ'
      SELECT TmpSpInst
  	  APPEND BLANK
			REPLACE cPattern         WITH lcPattern,;
	  					cFabric          WITH lcFabric,;
      				cPosition        WITH 'ZZZZZZZZZZZZZZZZZZZZ',;
      				nTotQty          WITH lnTotQty
    ENDIF
  	*B605388,1 AMH [End]
  	
	ENDIF
	SELECT (lcRpTmpFle)
	SEEK lcTmpKey
ENDSCAN
SELECT (lcRpTmpFle)
SEEK 'I '
DELETE REST WHILE ctype+cCutTkt = 'I '
SEEK '2M'
lcOldKey = SPACE(LEN(EVALUATE(KEY())))
SCAN REST WHILE ctype+cCutTkt+cFabric+cMarker = '2M'
  lcTmpKey = EVALUATE(KEY())
  REPLACE CMARKER2 WITH CMARKER
  REPLACE NPLY WITH ROUND(IIF(TmpSppItem.NUNITS=0,0,nPieces/TmpSppItem.NUNITS),0)
  IF lcOldKey = lcTmpKey
    REPLACE CMARKER2 WITH ''
  ELSE
    lcOldKey = lcTmpKey
  ENDIF
ENDSCAN
SEEK '3M'
lcOldKey = SPACE(LEN(EVALUATE(KEY())))
SCAN REST WHILE ctype+cCutTkt+cFabric+cMarker = '3M'
  lcTmpKey = EVALUATE(KEY())
  REPLACE CMARKER2 WITH CMARKER
  REPLACE NPLY WITH ROUND(IIF(TmpSppItem.NUNITS=0,0,nPieces/TmpSppItem.NUNITS),0)
  IF lcOldKey = lcTmpKey
    REPLACE CMARKER2 WITH ''
  ELSE
    lcOldKey = lcTmpKey
  ENDIF
ENDSCAN
SEEK '5 '
lcOldKey = SPACE(LEN(EVALUATE(KEY())))
SCAN REST WHILE ctype+cCutTkt+cFabric+cMarker = '5 '
  lcTmpKey = EVALUATE(KEY())
  REPLACE CMARKER2 WITH CMARKER
  REPLACE NPLY WITH ROUND(IIF(TmpSppItem.NUNITS=0,0,nPieces/TmpSppItem.NUNITS),0)
  IF lcOldKey = lcTmpKey
    REPLACE CMARKER2 WITH ''
  ELSE
    lcOldKey = lcTmpKey
  ENDIF
ENDSCAN
SET ORDER TO TmpMaj
SET RELATION TO cPattern INTO TmpSpInstSze ADDITIVE
SET RELATION TO cPattern+cFabric+cPosition INTO TmpSpInst ADDITIVE
LOCATE
*B604830,1 AMH [End]

DO CASE
CASE lcRpDevice = 'SCREEN'
	REPORT FORM (ALLTRIM(lcRpRpDir) + 'MF\MFCTKCA.FRX') PREVIEW WINDOW PRTWIND
CASE lcRpDevice = 'PRINTER'
	
	*B605129,1 AMH Display the get printer screen to select the printer [Start]
  *REPORT FORM (ALLTRIM(lcRpRpDir) + 'MF\MFCTKCA.FRX') TO PRINTER NOCONSOLE
	LOCAL lcPrinter
	lcPrinter = GETPRINTER()
	IF !EMPTY(lcPrinter)
	  ON ERROR DO lfEndProg
	  SET PRINTER TO NAME UPPER(lcPrinter)
    REPORT FORM (ALLTRIM(lcRpRpDir) + 'MF\MFCTKCA.FRX') TO PRINTER NOCONSOLE
	ENDIF
  *B605129,1 AMH [End]
  
ENDCASE

IF lnConnectionHandle > 0
	SQLDISCONNECT(lnConnectionHandle)
ENDIF

RELEASE WINDOW PRTWIND
SHOW MENU _MSYSMENU

SELECT (lcRpTmpFle)
USE

IF FILE(lcRpWrDir + lcRpTmpFle + '.DBF')
	ERASE (lcRpWrDir + lcRpTmpFle + '.DBF')
	ERASE (lcRpWrDir + lcRpTmpFle + '.CDX')
ENDIF

	FUNCTION Server_Error
	LOCAL laServerErrors , lcErrMsg
	DECLARE laServerErrors[1]
	AERROR(laServerErrors)
	lcErrMsg = laServerErrors[2]
	=MESSAGEBOX(lcErrMsg,0 + 16, "Aria Advantage Series")

*B605129,1 AMH Dammy funciton [Start]
FUNCTION lfEndProg
*B605129,1 AMH [End]

*-- end of Server_Error.