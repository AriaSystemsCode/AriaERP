*:*************************************************************************
*: Program file  : gfrange.prg
*: Program desc. : Global Range Selection Browse
*:        System : SY
*:        Module : SY
*: Programmer    : MAB - Mohamed Atia Badran
*:*************************************************************************
*! Modification:
*! B038562,1 SMM 09/19/2004 Do not open In range if no records 
*! N038711,1 SMM 11/08/2004 Replace In Range with Global Browse 
*! B039772,1 MAH 10/31/2005 Cannot open Vendor Browse.
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file [T20080820.0006]
PARAMETERS  lcBrowFld  ; && Browse Fields
		  , lcCursor   ; && Cursor that holds the selected values
		  , lcSelFld   ; && Selected Field
		  , lcFltrExp  ; && Filter Expression
		  , lcValidFun ; && Valid Function
		  , lcSeekFlag ; 
		  , lcPict     ;
		  , lcWinTitl  

LOCAL lnOldDataSessionID
PRIVATE lnDataSessionID
IF TYPE("_SCREEN.ACTIVEFORM.DataSessionID") = "N"
  lnDataSessionID = _SCREEN.ACTIVEFORM.DataSessionID
ELSE
  lnDataSessionID = 1
ENDIF

lnOldDataSessionID = SET("Datasession")
SET DATASESSION TO lnDataSessionID

LOCAL laFileStru[1,4],;
      lckeyexp,;
      laFldStruct[1,4],;
      laKeyExp[1],;
      lcForExp,;
      lcTmpCurs,;
      lcCurrKey

lcValidFun = IIF(TYPE('lcValidFun') <> 'C' .OR. EMPTY(lcValidFun) , '' ,ALLTRIM(lcValidFun))

STORE '' TO laKeyExp,lcForExp,lckeyexp,lcCurrKey

PRIVATE lnAlias              && the selected alias when entring the program
lnAlias = SELECT()

* check if the cursor name was passed as a parameter then use this cursor
* if not then get a temprory name to use and return it back to the calling
* program
IF TYPE('lcCursor') # 'C'
  lcCursor = 'X' + RIGHT(SYS(3),7)
ENDIF

* check if there is a selection field or not if not then return .f.
IF TYPE('lcSelFld') # 'C' OR EMPTY(lcSelFld)
  RETURN .F.
ENDIF

lcSelFld = UPPER(lcSelFld)

* if the cursor is not initialized then get the structure of the selection 
* field and create cursor with the same structure 

*! N038711,1 SMM No need for this code [START]
*!*	IF USED(lcCursor)
*!*	  SELECT (lcCursor)
*!*	  =GetFields(@laFldStruct)
*!*	ELSE
*!*	  =GetFields(@laFileStru)
*!*	  lnFldPos = ASCAN(laFileStru,lcSelFld)
*!*	  IF lnFldPos = 0
*!*	    RETURN .F.
*!*	  ENDIF
*!*	  lnFldPos = ASUBSCRIPT(laFileStru,lnFldPos,1)
*!*	  laFldStruct[1,1] = laFileStru[lnFldPos,1]
*!*	  laFldStruct[1,2] = laFileStru[lnFldPos,2]
*!*	  laFldStruct[1,3] = laFileStru[lnFldPos,3]
*!*	  laFldStruct[1,4] = laFileStru[lnFldPos,4]
*!*	  CREATE CURSOR &lcCursor. FROM ARRAY laFldStruct
*!*	  lcCDXPath = STRTRAN(DBF(lcCursor) , '.TMP' , '.CDX')
*!*	  INDEX ON &lcSelFld. TAG &lcSelFld. OF (lcCDXPath)
*!*	ENDIF
*!*	lcTmpCurs =  'X' + RIGHT(SYS(3),7)

*!*	*! N038711,1 SMM No need for the lSelected [START]
*!*	*!*	DECLARE laFldStruct[2,4]
*!*	*!*	laFldStruct[2,1] = "lSelected"
*!*	*!*	laFldStruct[2,2] = "L"
*!*	*!*	laFldStruct[2,3] = 1
*!*	*!*	laFldStruct[2,4] = 0
*!*	*! N038711,1 SMM No need for the lSelected [END]

*!*	CREATE CURSOR &lcTmpCurs. FROM ARRAY laFldStruct
*!*	SELECT (lcTmpCurs)
*!*	lcCDXPath = STRTRAN(DBF(lcTmpCurs) , '.TMP' , '.CDX')
*!*	*! N038711,1 SMM No need for the lSelected [START]
*!*	*!*	INDEX ON lSelected TAG lSelected OF (lcCDXPath)   && For optimizing.
*!*	*! N038711,1 SMM No need for the lSelected [END]
*!*	INDEX ON &lcSelFld. TAG &lcSelFld. OF (lcCDXPath)
*!*	=lfAppend(lcCursor,lcTmpCurs)

*!*	lcSwap    = lcCursor
*!*	lcCursor  = lcTmpCurs
*!*	lcTmpCurs = lcSwap
*! N038711,1 SMM No need for this code [END]
*-- Get Required Expressions... BEGIN
lcSeekFlag = IIF(TYPE('lcSeekFlag') <> 'C' , 'Y' , UPPER(lcSeekFlag))
*!*	lcPict     = IIF(TYPE('lcPict') <> 'C' .OR. EMPTY(lcPict) ,;
*!*	                 '@! ' + REPLICATE('X' , laFldStruct[1,3]) , lcPict)

lcBrowFld = IIF(TYPE('lcBrowFld') = 'C',lcBrowFld,lcSelFld)

*SELECT (lnAlias)
IF TYPE('lcFltrExp')='C'
  
  *! B039985,1 MAH 12/19/2005 problems in range browse. [BEGIN]
  lcFltrExp = lfmacrosub(lcFltrExp)
  *! B039985,1 MAH 12/19/2005 problems in range browse. [END]

  lcForExp = IIF(ATC('FOR ',lcFltrExp)>0,SUBSTR(lcFltrExp,ATC('FOR ',lcFltrExp)),'')
ENDIF  



lcFltrExp = IIF(!EMPTY(lcForExp),SUBSTR(lcFltrExp,1,ATC(lcForExp,lcFltrExp)-1),lcFltrExp)        
lcKeyExp  = IIF(TYPE('lcFltrExp')='C' AND !EMPTY(lcFltrExp),lcFltrExp,'')
= gfSubStr(lcKeyExp,@laKeyExp,',')
*-- Get Required Expressions... END

PRIVATE lcGBKey
lcGBKey = lcKeyExp


*!*	*-- Set Key Expression ... BEGIN
*!*	= ASORT(laKeyExp)
*!*	IF !EMPTY(laKeyExp[1]) OR (ALEN(laKeyExp,1) > 1)
*!*	  lcKeyExp = laKeyExp[1] + "," + laKeyExp[ALEN(laKeyExp,1)]
*!*	  lcKeySet = SET("Key")
*!*	  SET KEY TO RANGE &lcKeyExp.
*!*	ELSE
*!*	  lcKeyExp = ""
*!*	ENDIF 
*-- Set Key Expression ... END

*-- Set Filter Expression ... BEGIN
IF !EMPTY(lcForExp)
  lcForExp = ALLTRIM(lcForExp)
  lcForExp = ALLTRIM(SUBSTR(lcForExp,4))
  lcFltSet = SET("Filter")
  *! B039985,1 MAH 12/19/2005 problems in range browse [BEGIN]
  IF EMPTY(lcFltSet)
    SET FILTER TO &lcForExp.
  ELSE
    SET FILTER TO &lcFltSet. AND &lcForExp.    && Don't break old filter, the programmer is responsible.
  ENDIF   
  *! B039985,1 MAH 12/19/2005 problems in range browse [END]
ELSE
  lcForExp = ""
ENDIF 
*-- Set Filter Expression ... END
*!*	*!B038562,1 SMM Check for record count [START]   
*!*	PRIVATE llOKPressed
*!*	llOKPressed = .F.
*!*	SELECT (lnAlias)
*!*	LOCATE
*!*	IF EOF() then
*!*	  =gfModalGen('TRM00052B40011','ALERT')
*!*	ELSE
*!*	*!B038562,1 SMM Check for record count [END]   
  *-- Add not selected values ...

*! N038711,1 SMM No need for this code [START]
*!*	  LOCAL lnSelectedRecords
*!*	  lnSelectedRecords = RECCOUNT(lcCursor)
*!*	  m.lSelected = .F.
*!*	  SCAN FOR !SEEK(&lcSelFld.,lcCursor)
*!*	    SCATTER MEMVAR 
*!*	    INSERT INTO (lcCursor) FROM MEMVAR 
*!*	  ENDSCAN 
*! N038711,1 SMM No need for this code [END]

  LOCATE 

*! N038711,1 SMM No use for InRange use Browse.Scx instead[START]
*!*	*-- Estiblish the Range Screen ... BEGIN
*!*	PRIVATE oRange as Form
PUSH KEY
ON KEY

*!*	LOCAL lcClassDir
*!*	lcClassDir = ADDBS(oAriaApplication.ClassDir)

*!*	oRange = NEWOBJECT("InRange",lcClassDir+"InRange.VCX","",lnDataSessionID)
*!*	IF (VARTYPE(lcWinTitl) = "C") AND !EMPTY(lcWinTitl)
*!*	  oRange.Caption = "Select " + ALLTRIM(lcWinTitl) + " Range"
*!*	ENDIF   

*!*	*-- Adjust Range Form.
*!*	oRange.SetColumns(lcBrowFld,lcSelFld,lcCursor,lcValidFun, lnSelectedRecords)  

PRIVATE llSelected, lcPreferenceKey, lcIndx
llSelected = .F.
IF TYPE('lcTmpCurs') # 'C' OR EMPTY(lcTmpCurs)
  lcTmpCurs =  'X' + RIGHT(SYS(3),7)
ENDIF

IF USED(lcCursor)
  IF USED(lcTmpCurs)
    DELETE FROM lcTmpCurs
  ELSE	
    SELECT (lcCursor)
    =GetFields(@laFldStruct)
    CREATE CURSOR &lcTmpCurs FROM ARRAY laFldStruct
  ENDIF
  IF RECCOUNT(lcCursor) > 0 
*    INSERT INTO &lcTmpCurs(KeyExp) SELECT KeyExp From &lcCursor
    INSERT INTO &lcTmpCurs SELECT * From &lcCursor
  ENDIF
ENDIF
 
IF USED('SYDREPRT')
  lcPreferenceKey = ALLTRIM(SYDREPRT.cRep_ID)+'_'+ALLTRIM(lcSelFld)
ELSE
  IF TYPE('lcWinTitl')="C" AND !EMPTY(lcWinTitl)
    lcPreferenceKey = ALLTRIM(lcWinTitl)+'_'+ALLTRIM(lcSelFld)
  ELSE
    lcPreferenceKey = 'IN Range ' + '_' + ALLTRIM(lcSelFld)
  Endif
ENDIF

IF (VARTYPE(lcWinTitl) = "C") AND !EMPTY(lcWinTitl)
	IF AT('~',lcWinTitl) > 0  
		lcWinTitl = EVALUATE(SUBSTR(lcWinTitl,2,LEN(lcWinTitl)-1))
	ELSE
    lcWinTitl = "Select " + ALLTRIM(lcWinTitl) + " Range"
  ENDIF
ELSE
   lcWinTitl = "Select Range"
ENDIF

SELECT (lnAlias)
LOCATE

*! B039772,1 MAH 10/31/2005 Cannot open Vendor Browse [BEGIN]
*-- lcIndx = IIF(UPPER(ALIAS(lnAlias))='ITEM','CSTYLE',ALIAS(lnAlias))
lcIndx = ''
IF !EMPTY(loOGScroll.laSelFile)
  LOCAL lnIndex 
  FOR lnIndex = 1 TO ALEN(loOGScroll.laSelFile, 1)
    IF UPPER(ALLTRIM(loOGScroll.laSelFile[lnIndex, 1])) == ALIAS(lnAlias)
      lcIndx = loOGScroll.laSelFile[lnIndex, 3]
      EXIT
    ENDIF
  ENDFOR
ENDIF

IF EMPTY(lcIndx) .AND. UPPER(ALIAS(lnAlias))='ITEM'
  lcIndx = 'CSTYLE'
ENDIF
*! B039772,1 MAH 10/31/2005 [END]

*!*	lcStr="DO FORM BROWSE WITH lcBrowFld,'Select Range',lcGBKey,'',"+;
*!*	      ".F., .T.,.F., .F., .F.,lcCursor, lcSelFld, .F., .F., .F., .F., .F., .F., ALIAS(lnAlias),"+;
*!*	      "'', lcIndx , '', '',lcPreferenceKey "+;
*!*	      "TO llSelected"

*! E040024,1 MAH 12/07/2005 move all & remove all buttons [BEGIN]
*--   lcStr="DO FORM BROWSE WITH lcBrowFld,'"+lcWinTitl+"',lcGBKey,'',"+;
*--     ".F., .T.,.F., .F., .F.,lcCursor, lcSelFld, .F., .F., .F., .F., .F., .F., ALIAS(lnAlias),"+;
*--     "'', lcIndx , '', '',lcPreferenceKey "+;
*--     "TO llSelected"

  *! B039985,1 MAH 12/19/2005 problems in range browse [BEGIN]
  
  *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file [Start]
  IF UPPER(ALIAS(lnAlias))='STYLE'
     llSelected  =gfstybrw(IIF(UPPER(lcSelFld)= 'CSTYMAJOR','M','N'),"","",.F.,lcCursor,lcSelFld,lcValidFun )
  ELSE
  *! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file [End]
  
  
  lcStr="DO FORM BROWSE WITH lcBrowFld,'"+lcWinTitl+"',lcGBKey,'',"+;
     ".F., .T.,.F., '" + lcValidFun + "', .F.,lcCursor, lcSelFld, .F., .F., .F., .F., .F., .F., ALIAS(lnAlias),"+;
     "'', lcIndx , '', '',lcPreferenceKey "+;
     "TO llSelected"
  *PRIVATE lgForExp
  *lgForExp = lcForExp
  *lcStr="DO FORM BROWSE WITH lcBrowFld,'"+lcWinTitl+"',lcGBKey,lgForExp,"+;
    ".F., .T.,.F., '" + lcValidFun + "', .F.,lcCursor, lcSelFld, .F., .F., .F., .F., .F., .F., ALIAS(lnAlias),"+;
    "'', lcIndx , '', '',lcPreferenceKey "+;
    "TO llSelected"
  *! B039985,1 MAH 12/19/2005 problems in range browse [END]
  
*! E040024,1 MAH 12/07/2005 move all & remove all buttons [END]

*!*	lcStr="DO FORM BROWSE WITH lcBrowFld,'"+lcWinTitl+"',lcGBKey,'',"+;
*!*	      ".F., .T.,.F., .F., .F.,lcCursor, lcSelFld, .F., .F., .F., .F., .F., .F., .f.,"+;
*!*	      "'', lcIndx , '', '',lcPreferenceKey "+;
*!*	      "TO llSelected"

gfExecute(lcStr)

*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file [Start]
ENDIF 
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file [End]

*!*	PRIVATE llOKPressed
*!*	llOKPressed = .F.
*!*	oRange.Show()
IF !llSelected AND USED(lcCursor) 
  DELETE From (lcCursor)
  IF USED(lcTmpCurs) AND RECCOUNT(lcTmpCurs) > 0
*    INSERT INTO &lcCursor(KeyExp) SELECT KeyExp From &lcTmpCurs 
    INSERT INTO &lcCursor SELECT * From &lcTmpCurs 
  ENDIF
ENDIF

*! N038711,1 SMM No use for InRange use Browse.Scx instead [END]

  POP KEY
  *-- Estiblish the Range Screen ... END

  *-- Restore Settings ... BEGIN
  SELECT (lnAlias)
  IF !EMPTY(lcForExp)
    SET FILTER TO &lcFltSet.
  ENDIF 
  
*!*	  IF !EMPTY(lcKeyExp)
*!*	    SET KEY TO &lcKeySet.
*!*	  ENDIF 
  *-- Restore Settings ... END
*!*	*!B038562,1 SMM Check for record count [START]   
*!*	ENDIF
*!*	*!B038562,1 SMM Check for record count [END]   

*! N038711,1 SMM No use for this code [START]
*!*	IF llOKPressed    && User press the OKay button.
*!*	  USE IN (lcTmpCurs)
*!*	  SELECT (lcCursor)
*!*	  DIMENSION laFldStruct[1,4]    && Truncate the selected col field.
*!*	  CREATE CURSOR &lcTmpCurs. FROM ARRAY laFldStruct
*!*	  lcCDXPath = STRTRAN(DBF(lcTmpCurs) , '.TMP' , '.CDX')
*!*	  INDEX ON &lcSelFld. TAG &lcSelFld. OF (lcCDXPath)
*!*	  SELECT (lcCursor)  
*!*	  SCAN FOR lSelected
*!*	    SCATTER MEMVAR
*!*	    INSERT INTO (lcTmpCurs) FROM MEMVAR
*!*	  ENDSCAN 
*!*	ENDIF
*!*	USE IN (lcCursor)
*! N038711,1 SMM No use for this code [END]
CLEAR TYPEAHEAD
SET DATASESSION TO lnOldDataSessionID  && Restore data session.
RETURN llSelected
*-- End of main program code.

*!*****************************************************************************************
*! Name      : lfAppend
*! Purpose   : Append records from a table to another.
*!*****************************************************************************************
*!
FUNCTION lfAppend
  LPARAMETERS lcSource,lcDest
  LOCAL lnAlias
  lnAlias = SELECT()
  SELECT (lcSource)
  SCAN ALL
    SCATT MEMVAR MEMO
    m.lSelected = .T.
    INSERT INTO &lcDest. FROM MEMVAR
  ENDSCAN
  SELECT (lnAlias)
ENDFUNC 
*-- end of lfAppend.

*!*****************************************************************************************
*! Name      : GetFields
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/14/2002 11:00:52 PM
*! Purpose   : Get File Fields structure (Old Version compatablity)
*! Entry no. : 
*!*****************************************************************************************
*! Parameters: Array to hold structure passed by reference.
*!****************************************************************************************
*! Returns   : Number of fields
*!****************************************************************************************
*!
FUNCTION GetFields
  LPARAMETERS laFileArray
  IF TYPE("laFileArray[1]") = "U"
    RETURN 0
  ENDIF 
  
  LOCAL lnFields, laTmpFields[1,1], lnField, lnColumns
  lnFields = AFIELDS(laTmpFields)
  lnColumns = ALEN(laTmpFields, 2)
  DECLARE laFileArray[lnFields, 4]
  FOR lnField = 1 TO lnFields
    ACOPY(laTmpFields,laFileArray,((lnField - 1) * lnColumns) + 1,4,((lnField - 1) * 4) + 1)
  ENDFOR

  RETURN lnFields
ENDFUNC 

*!*****************************************************************************************
*! Name      : lfIncrementalSearch
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/17/2002 08:19:48 PM
*! Purpose   : Incremental Search Function
*! Entry no. : N000470,1 - IN-Range
*!*****************************************************************************************
*!
FUNCTION lfIncrementalSearch
  CLEAR TYPEAHEAD

  * if the key was pressed is one of "esc,enter,tab,shift+tab" do nothing
  * else run the incremental search screen
  IF !INLIST(LASTKEY(),27,13,9,15)
    LOCAL oIncrementalSearch
    oIncrementalSearch = CREATEOBJECT('rangeincrementalsearch',oRange.grdRange.Columns(2).Text1,LASTKEY(),lnDataSessionID)
    oIncrementalSearch.Show(1)

    CLEAR TYPEAHEAD
    KEYBOARD "{END}"  && go to the end of the edit field
    
  ENDIF
  RETURN .T.

ENDFUNC 
*-- end of lfIncrementalSearch.


*!*	*:*************************************************************************
*!*	*: Program file  : gfrange.prg
*!*	*: Program desc. : Global Range Selection Browse
*!*	*:        System : SY
*!*	*:        Module : SY
*!*	*: Programmer    : MAB - Mohamed Atia Badran
*!*	*:*************************************************************************
*!*	*!
*!*	LPARAMETERS lcBrowFld  , lcCursor , lcSelFld , lcFltrExp , lcValidFun ,;
*!*	            lcSeekFlag , lcPict, lcWinTitl

*!*	LOCAL lnOldDataSessionID
*!*	PRIVATE lnDataSessionID
*!*	IF TYPE("_SCREEN.ACTIVEFORM.DataSessionID") = "N"
*!*	  lnDataSessionID = _SCREEN.ACTIVEFORM.DataSessionID
*!*	ELSE
*!*	  lnDataSessionID = 1
*!*	ENDIF

*!*	lnOldDataSessionID = SET("Datasession")
*!*	SET DATASESSION TO lnDataSessionID

*!*	LOCAL laFileStru[1,4],;
*!*	      lckeyexp,;
*!*	      laFldStruct[1,4],;
*!*	      laKeyExp[1],;
*!*	      lcForExp,;
*!*	      lcTmpCurs,;
*!*	      lcCurrKey

*!*	lcValidFun = IIF(TYPE('lcValidFun') <> 'C' .OR. EMPTY(lcValidFun) , '' ,ALLTRIM(lcValidFun))

*!*	STORE '' TO laKeyExp,lcForExp,lckeyexp,lcCurrKey

*!*	LOCAL lnAlias              && the selected alias when entring the program
*!*	lnAlias = SELECT()

*!*	* check if the cursor name was passed as a parameter then use this cursor
*!*	* if not then get a temprory name to use and return it back to the calling
*!*	* program
*!*	IF TYPE('lcCursor') # 'C'
*!*	  lcCursor = 'X' + RIGHT(SYS(3),7)
*!*	ENDIF

*!*	* check if there is a selection field or not if not then return .f.
*!*	IF TYPE('lcSelFld') # 'C' OR EMPTY(lcSelFld)
*!*	  RETURN .F.
*!*	ENDIF

*!*	lcSelFld = UPPER(lcSelFld)

*!*	* if the cursor is not initialized then get the structure of the selection 
*!*	* field and create cursor with the same structure 
*!*	IF USED(lcCursor)
*!*	  SELECT (lcCursor)
*!*	  =GetFields(@laFldStruct)
*!*	  
*!*	ELSE

*!*	  =GetFields(@laFileStru)
*!*	  lnFldPos = ASCAN(laFileStru,lcSelFld)
*!*	  IF lnFldPos = 0
*!*	    RETURN .F.
*!*	  ENDIF
*!*	  lnFldPos = ASUBSCRIPT(laFileStru,lnFldPos,1)
*!*	  laFldStruct[1,1] = laFileStru[lnFldPos,1]
*!*	  laFldStruct[1,2] = laFileStru[lnFldPos,2]
*!*	  laFldStruct[1,3] = laFileStru[lnFldPos,3]
*!*	  laFldStruct[1,4] = laFileStru[lnFldPos,4]
*!*	  CREATE CURSOR &lcCursor. FROM ARRAY laFldStruct
*!*	  
*!*	  lcCDXPath = STRTRAN(DBF(lcCursor) , '.TMP' , '.CDX')
*!*	  INDEX ON &lcSelFld. TAG &lcSelFld. OF (lcCDXPath)
*!*	ENDIF
*!*	lcTmpCurs =  'X' + RIGHT(SYS(3),7)

*!*	DECLARE laFldStruct[2,4]
*!*	laFldStruct[2,1] = "lSelected"
*!*	laFldStruct[2,2] = "L"
*!*	laFldStruct[2,3] = 1
*!*	laFldStruct[2,4] = 0

*!*	CREATE CURSOR &lcTmpCurs. FROM ARRAY laFldStruct
*!*	SELECT (lcTmpCurs)
*!*	lcCDXPath = STRTRAN(DBF(lcTmpCurs) , '.TMP' , '.CDX')
*!*	INDEX ON lSelected TAG lSelected OF (lcCDXPath)   && For optimizing.
*!*	INDEX ON &lcSelFld. TAG &lcSelFld. OF (lcCDXPath)
*!*	=lfAppend(lcCursor,lcTmpCurs)

*!*	lcSwap    = lcCursor
*!*	lcCursor  = lcTmpCurs
*!*	lcTmpCurs = lcSwap

*!*	*-- Get Required Expressions... BEGIN
*!*	lcSeekFlag = IIF(TYPE('lcSeekFlag') <> 'C' , 'Y' , UPPER(lcSeekFlag))
*!*	lcPict     = IIF(TYPE('lcPict') <> 'C' .OR. EMPTY(lcPict) ,;
*!*	                 '@! ' + REPLICATE('X' , laFldStruct[1,3]) , lcPict)

*!*	lcBrowFld = IIF(TYPE('lcBrowFld') = 'C',lcBrowFld,lcSelFld)

*!*	SELECT (lnAlias)
*!*	IF TYPE('lcFltrExp')='C'
*!*	  lcForExp = IIF(ATC('FOR ',lcFltrExp)>0,SUBSTR(lcFltrExp,ATC('FOR ',lcFltrExp)),'')
*!*	ENDIF  

*!*	lcFltrExp = IIF(!EMPTY(lcForExp),SUBSTR(lcFltrExp,1,ATC(lcForExp,lcFltrExp)-1),lcFltrExp)        
*!*	lcKeyExp  = IIF(TYPE('lcFltrExp')='C' AND !EMPTY(lcFltrExp),lcFltrExp,'')
*!*	= gfSubStr(lcKeyExp,@laKeyExp,',')
*!*	*-- Get Required Expressions... END

*!*	*-- Set Key Expression ... BEGIN
*!*	= ASORT(laKeyExp)
*!*	IF !EMPTY(laKeyExp[1]) OR (ALEN(laKeyExp,1) > 1)
*!*	  lcKeyExp = laKeyExp[1] + "," + laKeyExp[ALEN(laKeyExp,1)]
*!*	  lcKeySet = SET("Key")
*!*	  SET KEY TO RANGE &lcKeyExp.
*!*	ELSE
*!*	  lcKeyExp = ""
*!*	ENDIF 
*!*	*-- Set Key Expression ... END

*!*	*-- Set Filter Expression ... BEGIN
*!*	IF !EMPTY(lcForExp)
*!*	  lcForExp = ALLTRIM(lcForExp)
*!*	  lcForExp = ALLTRIM(SUBSTR(lcForExp,4))
*!*	  lcFltSet = SET("Filter")
*!*	  IF EMPTY(lcFltSet)
*!*	    SET FILTER TO &lcForExp.
*!*	  ELSE
*!*	    SET FILTER TO &lcFltSet. AND &lcForExp.    && Don't break old filter, the programmer is responsible.
*!*	  ENDIF   
*!*	ELSE
*!*	  lcForExp = ""
*!*	ENDIF 
*!*	*-- Set Filter Expression ... END

*!*	*-- Add not selected values ...
*!*	LOCAL lnSelectedRecords
*!*	lnSelectedRecords = RECCOUNT(lcCursor)

*!*	m.lSelected = .F.
*!*	SCAN FOR !SEEK(&lcSelFld.,lcCursor)
*!*	  SCATTER MEMVAR 
*!*	  INSERT INTO (lcCursor) FROM MEMVAR 
*!*	ENDSCAN 
*!*	LOCATE 

*!*	*-- Estiblish the Range Screen ... BEGIN
*!*	PRIVATE oRange as Form

*!*	PUSH KEY
*!*	ON KEY
*!*	LOCAL lcClassDir
*!*	lcClassDir = ADDBS(oAriaApplication.ClassDir)

*!*	oRange = NEWOBJECT("InRange",lcClassDir+"InRange.VCX","",lnDataSessionID)
*!*	IF (VARTYPE(lcWinTitl) = "C") AND !EMPTY(lcWinTitl)
*!*	  oRange.Caption = "Select " + ALLTRIM(lcWinTitl) + " Range"
*!*	ENDIF   

*!*	*-- Adjust Range Form.
*!*	oRange.SetColumns(lcBrowFld,lcSelFld,lcCursor,lcValidFun, lnSelectedRecords)  

*!*	PRIVATE llOKPressed
*!*	llOKPressed = .F.

*!*	oRange.Show()

*!*	POP KEY
*!*	*-- Estiblish the Range Screen ... END

*!*	*-- Restore Settings ... BEGIN
*!*	IF !EMPTY(lcForExp)
*!*	  SET FILTER TO &lcFltSet.
*!*	ENDIF 

*!*	IF !EMPTY(lcKeyExp)
*!*	  SET KEY TO &lcKeySet.
*!*	ENDIF 
*!*	*-- Restore Settings ... END

*!*	IF llOKPressed    && User press the OKay button.
*!*	  USE IN (lcTmpCurs)
*!*	  SELECT (lcCursor)
*!*	  DIMENSION laFldStruct[1,4]    && Truncate the selected col field.
*!*	  CREATE CURSOR &lcTmpCurs. FROM ARRAY laFldStruct
*!*	  lcCDXPath = STRTRAN(DBF(lcTmpCurs) , '.TMP' , '.CDX')
*!*	  INDEX ON &lcSelFld. TAG &lcSelFld. OF (lcCDXPath)
*!*	  SELECT (lcCursor)  
*!*	  SCAN FOR lSelected
*!*	    SCATTER MEMVAR
*!*	    INSERT INTO (lcTmpCurs) FROM MEMVAR
*!*	  ENDSCAN 
*!*	ENDIF

*!*	USE IN (lcCursor)
*!*	SELECT (lnAlias)
*!*	CLEAR TYPEAHEAD
*!*	SET DATASESSION TO lnOldDataSessionID  && Restore data session.

*!*	RETURN llOKPressed
*!*	*-- End of main program code.

*!*	*!*****************************************************************************************
*!*	*! Name      : lfAppend
*!*	*! Purpose   : Append records from a table to another.
*!*	*!*****************************************************************************************
*!*	*!
*!*	FUNCTION lfAppend
*!*	  LPARAMETERS lcSource,lcDest
*!*	  LOCAL lnAlias
*!*	  lnAlias = SELECT()
*!*	  SELECT (lcSource)
*!*	  SCAN ALL
*!*	    SCATT MEMVAR MEMO
*!*	    m.lSelected = .T.
*!*	    INSERT INTO &lcDest. FROM MEMVAR
*!*	  ENDSCAN
*!*	  SELECT (lnAlias)
*!*	ENDFUNC 
*!*	*-- end of lfAppend.

*!*	*!*****************************************************************************************
*!*	*! Name      : GetFields
*!*	*! Developer : MAB - Mohamed Atia Badran
*!*	*! Date      : 11/14/2002 11:00:52 PM
*!*	*! Purpose   : Get File Fields structure (Old Version compatablity)
*!*	*! Entry no. : 
*!*	*!*****************************************************************************************
*!*	*! Parameters: Array to hold structure passed by reference.
*!*	*!****************************************************************************************
*!*	*! Returns   : Number of fields
*!*	*!****************************************************************************************
*!*	*!
*!*	FUNCTION GetFields
*!*	  LPARAMETERS laFileArray
*!*	  IF TYPE("laFileArray[1]") = "U"
*!*	    RETURN 0
*!*	  ENDIF 
*!*	  
*!*	  LOCAL lnFields, laTmpFields[1,1], lnField, lnColumns
*!*	  lnFields = AFIELDS(laTmpFields)
*!*	  lnColumns = ALEN(laTmpFields, 2)
*!*	  DECLARE laFileArray[lnFields, 4]
*!*	  FOR lnField = 1 TO lnFields
*!*	    ACOPY(laTmpFields,laFileArray,((lnField - 1) * lnColumns) + 1,4,((lnField - 1) * 4) + 1)
*!*	  ENDFOR

*!*	  RETURN lnFields
*!*	ENDFUNC 

*!*	*!*****************************************************************************************
*!*	*! Name      : lfIncrementalSearch
*!*	*! Developer : MAB - Mohamed Atia Badran
*!*	*! Date      : 11/17/2002 08:19:48 PM
*!*	*! Purpose   : Incremental Search Function
*!*	*! Entry no. : N000470,1 - IN-Range
*!*	*!*****************************************************************************************
*!*	*!
*!*	FUNCTION lfIncrementalSearch
*!*	  CLEAR TYPEAHEAD

*!*	  * if the key was pressed is one of "esc,enter,tab,shift+tab" do nothing
*!*	  * else run the incremental search screen
*!*	  IF !INLIST(LASTKEY(),27,13,9,15)
*!*	    LOCAL oIncrementalSearch
*!*	    oIncrementalSearch = CREATEOBJECT('rangeincrementalsearch',oRange.grdRange.Columns(2).Text1,LASTKEY(),lnDataSessionID)
*!*	    oIncrementalSearch.Show(1)

*!*	    CLEAR TYPEAHEAD
*!*	    KEYBOARD "{END}"  && go to the end of the edit field
*!*	    
*!*	  ENDIF
*!*	  RETURN .T.

*!*	ENDFUNC 
*!*	*-- end of lfIncrementalSearch.