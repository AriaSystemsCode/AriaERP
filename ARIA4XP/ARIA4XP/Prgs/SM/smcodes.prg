*:************************************************************************
*: File      : SMCODES.PRG                                              :*
*: System    : ARIA 4.0 XP                                              :*
*: Modules   : SM                                                       :*
*: Program   : Codes Program.                                           :*
*: Developer : Wael M. Abo-Shawareb  (WSH)                              :*
*: Date      : 01/24/2005                                               :*
*: Issue NO. : 037773,1                                                 :*
*:************************************************************************
*:Modifications:
*:B607866,1 MMT 12/06/2006 Error when press OK in op grid with change and browse appears. (T20060911.0003)
*:B607903,1 MMT 12/24/2006 Fix bug of Field width in codes screen is not read from Codeset File. (T20060914.0001)
*:B607937,1 TMI 01/17/2007 Check the case of "Unit of measure" code
*:B607937,1 SSH 01/23/2007 Check if the Code deleted recall, to prevent not saving codes 
*:B608126,1 MMT 06/14/2007 fix bug of not updating the Gl Account Fileds [T20070402.0026]
*:B608452,1 AKA 25/02/2007  Allow empty GL account code in case of Manufacturing Operation [T20080213.0001]
*:B608515,1 MMT 04/10/2008 Fix bug of allow user to add Color Code Starts with '*'[T20080305.0001]
*:B608750,1 MMT 12/03/2008 Fix bugs of rounding while displaying related fields[T20080908.0001]
*:C201070,1 TMI 12/01/2008 add the lines of "CCHRGCODE" related to the "CORDCHG" code for DIR03
*:N000628,1 TMI 02/02/2009 Add a new parameter to be used in the setup wizard screen 
*:E302618,1 MMT 06/17/2009 Add New Related Field for State Code for HST Tax[T20090605.0009]
*!B609356,1 SMA 07/21/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*!B609593,1 MMT 05/25/2011 cannot search in codes screen grid for specific Color[T20110322.0012]
*!B609729,1 MAB 11/13/2011 Include Aria27 files while validating code deletion
*!B609902,1 MMT 05/02/2012 change codes screen to accept 0 EOM days[T20120301.0038]
*:**************************************************************************************
*N000628,1 TMI 02/02/2009 [START] add the parameter "llDoNotRelease", when the screen is called with 
*                                 "llAddOnTheFly" is .T., then do not close the screen when add
*! E303030,1 MAB 12/28/2011 Extend File/Field/Index to NN length instead of 8,10,10
*LPARAMETERS llAddOnTheFly, lcValidField, lcAddCode
LPARAMETERS llAddOnTheFly, lcValidField, lcAddCode,llDoNotRelease
*N000628,1 TMI 02/02/2009 [END]

#INCLUDE R:\ARIA4XP\PRGS\SM\SMCODES.H

IF llAddOnTheFly AND TYPE('lcAddCode') = 'C' AND !EMPTY(lcAddCode)
  PRIVATE lcNewCode
  lcNewCode = EVALUATE(lcAddCode)
ENDIF

*-- Define a Unique Global Name for the Screen to use as a referencee in Related Fields "Option Grid" validations...
PUBLIC loCodesFormName
STORE '' TO loCodesFormName


*--Run the Main Codes Screen
*--  If the llAddOnTheFly parameter is true, then the screen will open in
*--     edit mode and the Insert New Code branch screen will be opened then
*--     the Related Fields screen "if exist" then the codes screen will be closed.
*N000628,1 TMI 02/02/2009 [START] add the parameter "llDoNotRelease" to the screen call
*DO FORM (oAriaApplication.ScreenHome + 'SM\SMCODES') WITH llAddOnTheFly, lcValidField
DO FORM (oAriaApplication.ScreenHome + 'SM\SMCODES') WITH llAddOnTheFly, lcValidField,llDoNotRelease
*N000628,1 TMI 02/02/2009 [END]

*--If Add On The Fly, Fll the lcAddCode Property
IF llAddOnTheFly AND TYPE('lcAddCode') = 'C' AND !EMPTY(lcAddCode)
  &lcAddCode = lcNewCode
ENDIF

RETURN

*!*************************************************************
*! Name      : lfMainInit
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 01/24/2005
*! Purpose   : Main Codes Screen Init Event
*!*************************************************************
*! Example   : =lfMainInit()
*!*************************************************************
FUNCTION lfMainInit
LPARAMETERS loFormSet

LOCAL lnAlias, lnI, laFileStru[1]
lnAlias = SELECT(0)

*wael
lcSetDelete = SET("Deleted" )
SET DELETED ON
*wael
WITH loFormSet
  .lcVldFlds    = gfTempName()  && Temp Cursor holds all valid fields...
  .lcSysComp    = gfTempName()  && Temp Cursor holds all Companies...
  .lcCodes      = gfTempName()  && Main Codes file Temp Cursor...
  .lcSydField   = gfTempName()  && Main SYDFIELD file Temp Cursor...
  .lcTmpCodes   = gfTempName()  && Temp Cursor holds all codees in the grid...
  .lcTmpRelFlds = gfTempName()  && Temp Cursor holds all Related Fields for selected Code...
  .lcTmpHold    = gfTempName()  && Temp cursor holds all Tables that has Current Code Value...
  
  loCodesFormName = loFormSet
  .lcInsCode    = ''     && Holds New Code Value returned from Insert Form...
  .lcInsDesc    = ''     && Holds New Code Description returned from Insert Form...

  *--Create SYDFIELD table Object...
  .loSydField = CREATEOBJECT('RemoteTable', 'SYDFIELD', 'VLDENTRY', .lcSydField, SET("Datasession"))
  
  *--Get System Companies from SYCCOMP file...
  LOCAL lcWhereCond
  lcWhereCond = IIF(.llAddOnTheFly, " WHERE cComp_Id = '" + oAriaApplication.ActiveCompanyID + "'", "")
  
  IF !lfSQLStatement("SELECT * FROM SYCCOMP" + lcWhereCond, .lcSysComp, 'CCOMP_ID|', 'CCOMP_ID|', .T., oAriaApplication.SystemConnectionString)
    SELECT (lnAlias)
    SET DELETED &lcSetDelete  && Wael
    RETURN .F.
  ENDIF
  
  *--Fill Companies array
  WITH .AriaFOrm1.cboCompanies
    SELECT CCOMP_ID + " - " + CCOM_NAME, CCOMP_ID, PADR(gfGetDataDir(ALLT(cCom_DDir)), LEN(cCom_dDir));
      FROM (loFormSet.lcSysComp);
      INTO ARRAY .aSourceArray;
     ORDER BY CCOMP_ID

    DIMENSION .aSourceArray[ALEN(.aSourceArray,1)+1,3]
    = AINS(.aSourceArray, 1)
    .aSourceArray[1] = LANG_SMCODES_SELCOMP
    .aSourceArray[2] = ''
    .aSourceArray[3] = ''
  ENDWITH

  *--Get valid fields from SYDFIELD file...........(1 = 2 Condition to match Fox and SQL ) !!!
  SELECT (.lcSydField)
  =AFIELDS(laFileStru)
  =gfCrtTmp(.lcVldFlds, @laFileStru, "cFld_Head+cFld_Name", "CFLD_NAME")
  
  SELECT (.lcVldFlds)
  =CURSORSETPROP("Buffering", 5)
  
  IF .llAddOnTheFly
    IF .loSydField.SEEK(.lcValidField, "CFLD_NAME")
      SELECT (.lcSydField)
      SCATTER MEMVAR MEMO
      SELECT (.lcVldFlds)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE lLok_Stat WITH .F.   && Temp
    ELSE
      SELECT (lnAlias)
      SET DELETED &lcSetDelete  && Wael
      RETURN .F.
    ENDIF
  ELSE
    IF .loSydField.SEEK(.T.)
      SELECT (.lcSydField)
      SCAN REST WHILE lVldEntry
        IF !SEEK(cFld_Head+cFld_Name, .lcVldFlds)
          SCATTER MEMVAR MEMO
          SELECT (.lcVldFlds)
          APPEND BLANK
          GATHER MEMVAR MEMO
          REPLACE lLok_Stat WITH .F.   && Temp
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
  SELECT (.lcVldFlds)
  =TABLEUPDATE(.T.)
  
  *--Fill valid fields array
  WITH .AriaFOrm1.cboVldFlds
    SELECT CFLD_HEAD, CFLD_NAME, lRltfields, PADR("|"+UPPER(ALLTRIM(mRltfields))+"|",250);
      FROM (loFormSet.lcVldFlds);
      INTO ARRAY .aSourceArray;
     ORDER BY CFLD_HEAD

    DIMENSION .aSourceArray[ALEN(.aSourceArray,1) + 1, 4]
    = AINS(.aSourceArray, 1)
    .aSourceArray[1] = LANG_SMCODES_SELCODE
    .aSourceArray[2] = ''
    .aSourceArray[3] = .F.
    .aSourceArray[4] = ''
  ENDWITH

  *--Check working file for existence of Companies and Valid Fields...
  IF !lfCkhFiles(loFormSet)
    SELECT (lnAlias)
    SET DELETED &lcSetDelete  && Wael
    RETURN .F.
  ENDIF

  *--Set value for FormSet Master File and Data Environment Initial Selected Cursor
  .DataEnvironment.InitialSelectedAlias = .lcVldFlds

  *--Create the temp codes cursor to be the grid record source.
  LOCAL lcTmpCurs, lnFlds
  lcTmpCurs = gfTempName()
  
  *WSH [Start]
  *IF !lfSQLStatement("SELECT * FROM CODES WHERE (1 = 2)", .lcTmpRelFlds, 'CCODE_NO+CRLTD_NAM|', 'CCODE_NO|', .T.)
  IF !lfSQLStatement("SELECT *, ' ' AS cStatus FROM CODES WHERE (1 = 2)", .lcTmpRelFlds, 'CCODE_NO+CRLTD_NAM|cStatus|', 'CCODE_NO|cStatus|', lfIsNative('CODES') = 1)
  *WSH [End]
  
    SELECT (lnAlias)
    SET DELETED &lcSetDelete  && Wael
    RETURN .F.
  ENDIF
  
  SELECT (.lcTmpRelFlds)
  SET ORDER TO CCODE_NO
  =AFIELDS(laFileStru)
  lnFlds = ALEN(laFileStru,1) + 1
  DIMENSION lafilestru[lnFlds,18]
  
  lafilestru[lnFlds,1] = 'llDefCode'
  lafilestru[lnFlds,2] = 'L'
  lafilestru[lnFlds,3] = 1
  lafilestru[lnFlds,4] = 0
  lafilestru[lnFlds,5] = .F.
  lafilestru[lnFlds,6] = .F.
  FOR lnI = 7 TO 16
    lafilestru[lnFlds,lnI] = ''
  ENDFOR
  lafilestru[lnFlds,17] = 0
  lafilestru[lnFlds,18] = 0
  
  =gfCrtTmp(.lcTmpCodes, @laFileStru, "CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM", "CCODE_NO")
  SELECT (.lcTmpCodes)
  INDEX ON cStatus TAG cStatus
  *!B609593,1 MMT 05/25/2011 cannot search in codes screen grid for specific Color[Start]
  INDEX ON cDiscrep TAG "Discrep"
  *!B609593,1 MMT 05/25/2011 cannot search in codes screen grid for specific Color[End]
  INDEX ON cCode_No+cDiscrep TAG (.lcTmpCodes)
  =CURSORSETPROP("Buffering", 5)
  SET ORDER TO CCODE_NO
  
  *--Create cursur that will hold all tables where the field is found...
  DIMENSION lafilestru[2,18]

  lafilestru[1,1] = 'CFILE_NAM'
  lafilestru[1,2] = 'C'
  lafilestru[1,3] = 8
  lafilestru[1,4] = 0
  lafilestru[2,1] = 'CFILE_TTL'
  lafilestru[2,2] = 'C'
  *B607937,1 TMI [START] increase the file title fields width
  *lafilestru[2,3] = 34 
  lafilestru[2,3] = 50
  *B607937,1 TMI [END  ] increase the file title fields width
  lafilestru[2,4] = 0
  FOR lnI = 7 TO 16
    lafilestru[1,lnI] = ''
    lafilestru[2,lnI] = ''
  ENDFOR
  lafilestru[1,17] = 0
  lafilestru[1,18] = 0
  lafilestru[2,17] = 0
  lafilestru[2,18] = 0

  =gfCrtTmp(.lcTmpHold, @laFileStru, "CFILE_NAM", "CFILE_NAM")

  *--Assign Codes Grid Properties
  =lfBuildGrid(loFormSet)
ENDWITH

SELECT (lnAlias)
SET DELETED &lcSetDelete  && Wael

RETURN .T.

*!*************************************************************
*! Name      : lpShow
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/15/2005
*! Purpose   : Main Codes Screen Change Mode Function
*!*************************************************************
*! Example   : =lpShow()
*!*************************************************************
FUNCTION lpShow
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

DO CASE
  CASE loFormSet.ActiveMode = 'S'                       && Select mode.
    =lfClearData(loFormSet)
    loFormSet.AriaForm1.cboCompanies.Enabled  = .T.
    loFormSet.AriaForm1.grdCodes.RecordSource = ''
    
    IF !EMPTY(oAriaApplication.ActiveCompanyID)
      loFormSet.AriaForm1.cboCompanies.Value = oAriaApplication.ActiveCompanyID
      =lfChangeCompany(loFormSet, oAriaApplication.ActiveCompanyID)
    ELSE
      loFormSet.AriaForm1.cboCompanies.Value = ''
    ENDIF
    
    *--Assign Codes Grid Properties
    =lfBuildGrid(loFormSet)
    
    loFormSet.AriaForm1.cboVldFlds.Enabled = .T.
    loFormSet.AriaForm1.cboVldFlds.Value   = ''
    
    loFormSet.AriaForm1.cboCompanies.SetFocus()

  CASE loFormSet.ActiveMode = 'V'                       && View mode.
    loFormSet.AriaForm1.LockScreen = .T.
    
    loFormSet.AriaForm1.cboCompanies.Enabled  = .F.
    loFormSet.AriaForm1.cboVldFlds.Enabled    = .F.
    loFormSet.AriaForm1.grdCodes.RecordSource = ''
    loFormSet.AriaForm1.cboVldFlds.Value      = EVALUATE(loFormSet.lcVldFlds + '.cFld_name')
    
    *--Get all Codes and related fields from Codes file to temp file.
    IF !lfGetData(loFormSet)
      loFormSet.ChangeMode('S')
    ENDIF
    
    *--Assign Codes Grid Properties
    =lfBuildGrid(loFormSet)
    
    SELECT (loFormSet.lcTmpCodes)
    LOCATE
    IF !EOF()
      loFormSet.AriaForm1.grdCodes.ActivateCell(1,1)
    ENDIF
    loFormSet.AriaForm1.LockScreen = .F.

  CASE loFormSet.ActiveMode = 'E'                       && Edit mode.
    loFormSet.AriaForm1.cboCompanies.Enabled = .F.
    loFormSet.AriaForm1.cboVldFlds.Enabled   = .F.
    loFormSet.AriaForm1.grdCodes.RecordSource = ''
    
    *--Get all Codes and related fields from Codes file to temp file.
    IF !lfGetData(loFormSet)
      loFormSet.ChangeMode('V')
    ENDIF
    
    *--Assign Codes Grid Properties
    =lfBuildGrid(loFormSet)
    
    SELECT (loFormSet.lcTmpCodes)
    LOCATE
    IF !EOF()
      loFormSet.AriaForm1.grdCodes.ActivateCell(1,IIF(loFormSet.llIsEditable, 3, 1))
    ENDIF
    loFormSet.AriaForm1.cmdInsert.Enabled = .T.
    loFormSet.AriaForm1.cmdRemove.Enabled = !EOF(loFormSet.lcTmpCodes)
ENDCASE

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfGetData
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/15/2005
*! Purpose   : Get all Codes and Related Fields for selected Field
*!*************************************************************
*! Example   : =lfGetData()
*!*************************************************************
FUNCTION lfGetData
LPARAMETERS loFormSet

LOCAL lnAlias, lcVldFld, lcTmpCurs
lnAlias   = SELECT(0)
lcVldFld  = loFormSet.AriaForm1.cboVldFlds.Value
lcTmpCurs = gfTempName()

*--Clear Temp Codes file befor collecting data
=lfClearData(loFormSet)

*--Get all Codes from Codes file for selected Field.
IF loFormSet.loCodes.SEEK('N'+PADR(lcVldFld, 10))
  *-- Get Codes
  SELECT * FROM (loFormSet.lcCodes);
   WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+PADR(lcVldFld, 10) AND CRLTFIELD = 'N';
    INTO TABLE (oAriaApplication.WorkDir + lcTmpCurs + '.DBF')
  
  SELECT (loFormSet.lcTmpCodes)
  APPEND FROM (oAriaApplication.WorkDir + lcTmpCurs + '.DBF')
  
  *-- Get Related Fields
  SELECT * FROM (loFormSet.lcCodes);
   WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+PADR(lcVldFld, 10) AND CRLTFIELD = 'Y';
    INTO TABLE (oAriaApplication.WorkDir + lcTmpCurs + '.DBF')
  
  SELECT (loFormSet.lcTmpRelFlds)
  APPEND FROM (oAriaApplication.WorkDir + lcTmpCurs + '.DBF')
  
  *-- Delete Temp File
  IF USED(lcTmpCurs)
    USE IN (lcTmpCurs)
  ENDIF
  IF FILE(oAriaApplication.WorkDir + lcTmpCurs + '.DBF')
    DELETE FILE (oAriaApplication.WorkDir + lcTmpCurs + '.DBF')
  ENDIF
ENDIF

*--Get Default code
IF loFormSet.loCodes.SEEK('D' + lcVldFld)
  SELECT (loFormSet.lcCodes)
  IF SEEK('N'+CFLD_NAME+CCODE_NO, loFormSet.lcTmpCodes, 'CCODE_NO')
    SELECT (loFormSet.lcTmpCodes)
    REPLACE llDefCode WITH .T.,;
            cDefCode  WITH 'D'
  ELSE
    SCATTER MEMVAR MEMO
    SELECT (loFormSet.lcTmpCodes)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE llDefCode WITH .T.
  ENDIF
ENDIF

*--Update Temp Codes file...
SELECT (loFormSet.lcTmpCodes)
=TABLEUPDATE(.T.)
LOCATE

SELECT (loFormSet.lcTmpRelFlds)
=TABLEUPDATE(.T.)
LOCATE

*--See if there is related fields for this code.
LOCAL lcRltField, lcORltFld, lnC, lnArayCont, lnActCode
LOCAL lnPos, lnOccur1, lnPos1, lcCurrComp, lcCurrField, I, lcStat

WITH loFormSet.AriaForm1
  lcCurrComp  = .cboCompanies.Value
  lcCurrField = .cboVldFlds.Value
  lnActCode   = ASCAN(.cboVldFlds.aSourceArray, .cboVldFlds.Value, 1, ALEN(.cboVldFlds.aSourceArray), 0, 14)
ENDWITH
loFormSet.laRelFld = ""
IF loFormSet.AriaForm1.cboVldFlds.aSourceArray[lnActCode,3]
  lcRltField = "|" + lfGetRFStr(loFormSet, lcCurrField, lcCurrComp) + "|"
  lcRltField = STRTRAN(lcRltField, SPACE(1))
  lcORltFld  = lcRltField
  lnOccur = OCCUR('~', lcRltField)
  IF lnOccur > 0
    FOR lnC = 1 TO lnOccur
      lnPos      = ATC("~", lcRltField, lnC)
      lnOccur1   = OCCUR('|', SUBSTR(lcRltField, 1, lnPos))
      lnPos1     = ATC('|', lcRltField, lnOccur1 + 1)
      lcRltField = STRTRAN(lcRltField, SUBSTR(lcRltField, lnPos, lnPos1 - lnpos), '')
    ENDFOR
  ENDIF
  
  LOCAL lctTmpRelFld, lcFldName, laTmpArray, lnFld
  lctTmpRelFld = STRTRAN(lcRltField, '|', '', 1, 1)
  lnFld        = 0
  *--Create the Related Fields Array...
  DO WHILE AT("|", lctTmpRelFld, 1) <> 0
    lcFldName    = SUBSTR(lctTmpRelFld, 1, AT("|", lctTmpRelFld, 1) - 1)
    lctTmpRelFld = STRTRAN(lctTmpRelFld, lcFldName + "|", "", 1, 1)
    lcFldName    = ALLTRIM(lcFldName)
    lnFld        = lnFld + 1
    
    IF loFormSet.loSydField.SEEK(IIF(LEFt(lcFldName, 1) == '$', PADR(SUBSTR(lcFldName, 2), 10), PADR(lcFldName, 10)), 'CFLD_NAME')
      SELECT (loFormSet.lcSydField)
      
      DIMENSION loFormSet.laRelFld(lnFld, 14)
      loFormSet.laRelFld[lnFld,01] = lcFldName
      loFormSet.laRelFld[lnFld,02] = PADR(IIF(!EMPTY(cFld_head), cFld_head, cFld_name + SPACE(15)), 25)
      loFormSet.laRelFld[lnFld,03] = cData_typ
      loFormSet.laRelFld[lnFld,04] = nFld_Wdth
      loFormSet.laRelFld[lnFld,05] = nFld_dec
      loFormSet.laRelFld[lnFld,06] = ''
      loFormSet.laRelFld[lnFld,07] = ''
      loFormSet.laRelFld[lnFld,08] = PADR(ALLTRIM(mVald_str), 250)
      loFormSet.laRelFld[lnFld,09] = cVldfnloc
      loFormSet.laRelFld[lnFld,10] = ''
      loFormSet.laRelFld[lnFld,11] = lVldEntry
      loFormSet.laRelFld[lnFld,12] = PADR(ALLTRIM(mVEntries), 250)
      loFormSet.laRelFld[lnFld,13] = .F.
      loFormSet.laRelFld[lnFld,14] = ''
    ENDIF
  ENDDO
  
  FOR lnArayCont = 1 TO ALEN(loFormSet.laRelFld, 1)
    lnPos = ATC(ALLTRIM(loFormSet.laRelFld[lnArayCont,1]), lcRltField)
    IF SUBSTR(lcRltField, lnPos - 1, 1) = '$'
      loFormSet.laRelFld[lnArayCont,13] = .T.
      IF SUBSTR(lcORltFld , lnPos + LEN(ALLTRIM(laRelFld[lnArayCont,1])), 1) = '~'
        lnOccur = OCCUR('|', SUBSTR(lcORltFld, 1, lnPos))
        lnPos1  = ATC("|", lcORltFld, lnOccur + 1)
        loFormSet.laRelFld[lnArayCont,14] = SUBSTR(lcORltFld, lnPos + LEN(ALLTRIM(laRelFld[lnArayCont, 1])) + 1, ;
           lnPos1 - (lnPos + LEN(ALLTRIM(loFormSet.laRelFld[lnArayCont,1]))) - 1)
      ENDIF
    ENDIF
  ENDFOR
  
  FOR lnArayCont = 1 TO ALEN(loFormSet.laRelFld, 1) - 1
    FOR I = lnArayCont + 1 TO ALEN(loFormSet.laRelFld, 1)
      IF ATC(ALLTRIM(loFormSet.laRelFld[lnArayCont,1]), lcRltField) > ATC(ALLTRIM(loFormSet.laRelFld[I,1]), lcRltField)
        =lfXChng(@laRelFld, lnArayCont, I)
      ENDIF
    ENDFOR
  ENDFOR
  
  loFormSet.llRlt_Fld = !EMPTY(loFormSet.laRelFld[1,1])
ELSE
  loFormSet.llRlt_Fld = .F.
ENDIF

LOCAL lnCodeWdth
lnCodeWdth = 0

loFormSet.llIsEditable = gfIsEdtble(lcVldFld, @lnCodeWdth, lcCurrComp)
loFormSet.lcPic        = REPLICATE("!",lnCodeWdth)
loFormSet.lnCodeWdth   = lnCodeWdth

loFormSet.AriaForm1.cmdRelFlds.Enabled = loFormSet.ActiveMode # 'S' AND loFormSet.llRlt_Fld AND !EOF(loFormSet.lcTmpCodes)

*!*  IF ALLTRIM(lcCurrField) = "SEASON" .AND. ASCAN(laEvntTrig , PADR('ENABSEA',10)) <> 0
*!*    =gfDoTriger('SMCODES',PADR('ENABSEA',10))
*!*  ENDIF

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfGetRFStr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/16/2005
*! Purpose   : Check and Return Related Fields from SYDFIELD file.
*!*************************************************************
*! Example   : =lfGetRFStr()
*!*************************************************************
FUNCTION lfGetRFStr
LPARAMETERS loFormSet, lcCodField, lcActComp

LOCAL lcRet, llOpen, lcTag, lnPos, lnAlias, llApInst
lnAlias = SELECT(0)

IF TYPE("lcCodField") # "C" OR TYPE("lcActComp") # "C"
  lcRet = SPACE(0)
ELSE
  lcRet      = SPACE(0)
  lcCodField = UPPER(ALLTRIM(lcCodField))
  lcActComp  = ALLTRIM(lcActComp)
  llGlLink   = (gfGetMemVar("M_LINK_GL", lcActComp) = "Y")
  lcCountry  = SPACE(0)

  SELECT (loFormSet.lcSysComp)
  IF SEEK(lcActComp)
    lcCountry = UPPER(ALLTRIM(EVALUATE(loFormSet.lcSysComp + '.cCont_Code')))
    llApInst  = 'AP' $ UPPER(EVALUATE(loFormSet.lcSysComp + '.mModlSet'))
  ENDIF

  IF !EMPTY(lcCountry)
    DO CASE
      CASE lcCodField = "STATE"
      
        *:E302618,1 MMT 06/17/2009 Add New Related Field for State Code for HST Tax[Start]
        *lcRet = "NTAXRATE" + IIF(lcCountry = "CANADA", "|CTAXRULE", "")        
		lcRet = "NTAXRATE" + IIF(lcCountry = "CANADA", "|CTAXRULE|LHSTTAX", "")
        *:E302618,1 MMT 06/17/2009 Add New Related Field for State Code for HST Tax[End]

      OTHERWISE
        IF loFormSet.loSydField.SEEK(lcCodField, 'CFLD_NAME')
          lcRet = UPPER(ALLTRIM(EVALUATE(loFormSet.lcVldFlds + '.mRltFields')))
          IF !EMPTY(lcRet) 
            DO CASE
              CASE lcCodField = "CDIVISION" 
                IF !(llGlLink .AND. gfGetMemVar("M_DIV_LINK", lcActComp) = 'Y')
                  lnPos = ATC('LINK_CODE', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('LINK_CODE') + lnMand, '')
                  ENDIF
                  lnPos = ATC('CSLSGLLINK', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('CSLSGLLINK') + lnMand, '')
                  ENDIF
                ENDIF

                IF gfGetMemVar("M_DIV_SEQ", lcActComp) # 'Y'
                  lnPos = ATC('DIVGROUP', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand,LEN('DIVGROUP') + lnMand, '')
                  ENDIF
                ENDIF
                IF gfGetMemVar("M_UCCDIV", lcActComp) = 'Y' OR ;
                   gfGetMemVar("M_UPC_USE", lcActComp) <> 'Y'
                  lnPos = ATC('CUPCMAN', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('CUPCMAN') + lnMand, '')
                  ENDIF
                  lnPos = ATC('CUPCGENTYP', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1) 
                    lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('CUPCGENTYP') + lnMand, '')
                  ENDIF  
                ENDIF   
              CASE INLIST(lcCodField, "TRANCODE", "CCREDITCOD")
                IF !llApInst
                  lnPos = ATC('CBNKCODE', lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('CBNKCODE') + lnMand, '')
                  ENDIF
                  lnPos = ATC('CCHKACCT', lcRet)
                  IF lnPos > 0 
                    lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                    lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('CCHKACCT') + lnMand, '')
                  ENDIF
                ENDIF
              CASE lcCodField = "SHIPVIA" .AND. lcCountry = "ENG"
                 lnPos = ATC('NCODCHARGE', lcRet)
                 IF lnPos > 0
                   lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)

                   *--Remove the related field from the related string.
                   lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('NCODCHARGE') + lnMand, '')
                 ENDIF
                 lnPos = ATC('NINSCHARGE', lcRet)
                 IF lnPos > 0
                   lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                   lcRet  = STUFF(lcRet, lnPos - lnMand,LEN('NINSCHARGE') + lnMand, '')
                 ENDIF
                 lnPos = ATC('NFXDPRCNT', lcRet)
                 IF lnPos > 0
                   lnMand = IIF(SUBSTR(lcRet, lnPos - 1, 1) = '$', 2, 1)
                   lcRet  = STUFF(lcRet, lnPos - lnMand, LEN('NFXDPRCNT') + lnMand, '')
                 ENDIF
            ENDCASE
          ENDIF && !EMPTY(lcRet)
        ENDIF
    ENDCASE
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN (lcRet)

*!*************************************************************
*! Name      : lfvCode
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/15/2005
*! Purpose   : Valid Fuction for the Codes Combo Box
*!*************************************************************
*! Example   : =lfvCode()
*!*************************************************************
FUNCTION lfvCode
LPARAMETERS loFormSet

LOCAL lcComp, lcVldField, lnPos

WITH loFormSet.AriaForm1
  lcComp     = .cboCompanies.Value
  lnPos      = ASCAN(.cboVldFlds.aSourceArray, .cboVldFlds.Value, 1, ALEN(.cboVldFlds.aSourceArray), 0, 14)
  lcVldField = PADR(.cboVldFlds.aSourceArray[lnPos,1], 25) + .cboVldFlds.Value
ENDWITH

IF EMPTY(lcComp)
  *** You have to select a company first. ***
  *** <  Ok  > ***
  = gfModalGen("TRM00219B00000", "DIALOG")
  loFormSet.AriaForm1.cboCompanies.SetFocus()
  RETURN .F.
ELSE
  IF loFormSet.SeekRecord(lcVldField, .F., .T.) = 0
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*!*************************************************************
*! Name      : lfwDiscrep
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/15/2005
*! Purpose   : When Event of the Description Colunn in the Grid
*!*************************************************************
*! Example   : =lfwDiscrep()
*!*************************************************************
FUNCTION lfwDiscrep
LPARAMETERS loFormSet

loFormSet.lcOldDesc = EVALUATE(loFormSet.lcTmpCodes + '.cDiscrep')

RETURN

*!*************************************************************
*! Name      : lfvDiscrep
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/15/2005
*! Purpose   : Valid Event of the Description Colunn in the Grid
*!*************************************************************
*! Example   : =lfvDiscrep()
*!*************************************************************
FUNCTION lfvDiscrep
LPARAMETERS loFormSet, lcDiscrep

LOCAL lnAlias, lnOldRec, lcDiscrep, llRetVal
lnAlias   = SELECT(0)
llRetVal  = .T.
lcCodeNo  = EVALUATE(loFormSet.lcTmpCodes + '.cCode_No')

IF !EMPTY(lcDiscrep)
  IF !loFormSet.llIsEditable
    SELECT (loFormSet.lcTmpCodes)
    lnOldRec  = RECNO()
    
    SELECT (loFormSet.lcTmpCodes)
    LOCATE FOR ALLTRIM(UPPER(cDiscrep)) = ALLTRIM(UPPER(lcDiscrep)) AND ;
               !(cCode_No == lcCodeNo)
    IF FOUND()
      *** This field already exists. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00053B00000","DIALOG")
      llRetVal = .F.
    ENDIF
    
    SELECT (loFormSet.lcTmpCodes)
    GOTO (lnOldRec)
  ENDIF
  
  *WSH [Start]
  *IF llRetVal AND loFormSet.loCodes.SEEK('N'+CFLD_NAME+CCODE_NO+loFormSet.lcOldDesc+CRLTD_NAM)
  *  loFormSet.loCodes.REPLACE("cDiscrep WITH '" + lcDiscrep + "'")
  *  
  *  IF loFormSet.loCodes.SEEK('D'++CFLD_NAME+CCODE_NO+loFormSet.lcOldDesc+CRLTD_NAM)
  *    loFormSet.loCodes.REPLACE("cDiscrep WITH '" + lcDiscrep + "'")
  *  ENDIF
  *ENDIF
  REPLACE cStatus WITH 'S'
  *WSH [End]
  
ENDIF

SELECT (lnAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lfvInsert
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/15/2005
*! Purpose   : Valid Function for Insert New Coded Button
*!*************************************************************
*! Example   : =lfvInsert()
*!*************************************************************
FUNCTION lfvInsert
LPARAMETERS loFormSet

LOCAL lcVldFld, lnCount3, lnAlias, lnOldRec, llRetVal
lcVldFld = loFormSet.AriaForm1.cboVldFlds.Value
lnAlias  = SELECT(0)
lcComp   = loFormSet.AriaForm1.cboCompanies.Value
lnOldRec = RECNO(loFormSet.lcTmpCodes)
llRetVal = .T.

*--Insert code in the temp file.
SELECT (loFormSet.lcTmpCodes)
IF loFormSet.llIsEditable
  =SEEK('N'+lcVldFld+SPACE(6), loFormSet.lcTmpCodes, loFormSet.lcTmpCodes)
ELSE
  LOCATE FOR (EMPTY(cDiscrep) OR ALLTRIM(cDiscrep) = CHR(255)) AND cRltField = "N"
ENDIF

IF !FOUND()
  *--Set Default new code value and description...
  loFormSet.lcInsCode = IIF(loFormSet.llIsEditable, SPACE(loFormSet.lnCodeWdth), ALLTRIM(gfSequence("cCode_No", lcComp)))
  loFormSet.lcInsDesc = ''
  
  *--Run the Insert Code Form
  DO FORM (oAriaApplication.ScreenHome + 'SM\SMINSCODE') WITH loFormSet
  
  *--Insert new Code in working and Master files
  IF !(IIF(loFormSet.llIsEditable, EMPTY(loFormSet.lcInsCode), EMPTY(loFormSet.lcInsDesc)))
    SELECT (loFormSet.lcTmpCodes)
    APPEND BLANK
    REPLACE cDefCode  WITH 'N',;
            cFld_Name WITH lcVldFld,;
            cCode_No  WITH loFormSet.lcInsCode,;
            cDiscrep  WITH loFormSet.lcInsDesc,;
            cRltField WITH 'N'
    SCATTER MEMVAR MEMO
    
    *WSH [Start]
    **-- Update Master Codes File
    *loFormSet.loCOdes.APPEND()
    *SELECT (loFormSet.loCOdes.lcCursorUpdate)
    *GATHER MEMVAR MEMO
    *=gfAdd_Info(loFormSet.loCOdes.lcCursorUpdate)
    REPLACE cStatus WITH 'S'
    *WSH [End]
    
    *--If it has related fields add its records.
    IF loFormSet.llRlt_Fld
      FOR lnCount3 = 1 TO ALEN(loFormSet.laRelFld, 1)
        SELECT (loFormSet.lcTmpRelFlds)
        APPEND BLANK
        REPLACE cDefCode  WITH 'N',;
                cFld_Name WITH lcVldFld,;
                cCode_No  WITH loFormSet.lcInsCode,;
                cDiscrep  WITH '',;
                cRltField WITH 'Y',;
                cRltd_Nam WITH IIF(LEFT(loFormSet.laRelFld[lnCount3,1], 1) == '$', SUBSTR(loFormSet.laRelFld[lnCount3,1], 2), loFormSet.laRelFld[lnCount3,1]),;
                cRltd_Typ WITH loFormSet.laRelFld[lnCount3,3],;
                cRltd_Vlu WITH ''
        
        *WSH [Start]
        *SCATTER MEMVAR MEMO
        **-- Update Master Codes File
        *loFormSet.loCOdes.APPEND()
        *SELECT (loFormSet.loCOdes.lcCursorUpdate)
        *GATHER MEMVAR MEMO
        *=gfAdd_Info(loFormSet.loCOdes.lcCursorUpdate)
        REPLACE cStatus WITH 'S'
        *WSH [End]
        
      ENDFOR
      
      *-- Open the Related Fields Screen...
      = loFormSet.AriaForm1.cmdRelFlds.Click()
    ENDIF
    
    *-- If "Add On The Fly" and this is the first code, set the new code to be the Default Code.
    IF loFormSet.llAddOnTheFly AND RECCOUNT(loFormSet.lcTmpCodes) = 1
      = lfvDefa(loFormSet)
    ENDIF
  ELSE
    llRetVal = .F.
  ENDIF
ENDIF

*--Assign Codes Grid Properties
loFormSet.AriaForm1.grdCodes.RecordSource = ''
=lfBuildGrid(loFormSet)

SELECT (loFormSet.lcTmpCodes)
IF lnOldRec <> 0 AND lnOldRec < RECCOUNT()
  GOTO (lnOldRec)
ELSE
  LOCATE
ENDIF
loFormSet.AriaForm1.cmdRemove.Enabled  = !EOF()
loFormSet.AriaForm1.cmdRelFlds.Enabled = loFormSet.llRlt_Fld AND !EOF()

loFormSet.Refresh()

SELECT (lnAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/22/2005
*! Purpose   : Remove Code function
*!*************************************************************
*! Example   : =lfvRemove()
*!*************************************************************
FUNCTION lfvRemove
LPARAMETERS loFormSet, llNoAsk

LOCAL lnAlias, lcVldFld, lcCurrKey, lcCode, llDelete, lnOldRec
lnAlias  = SELECT(0)
lcVldFld = loFormSet.AriaForm1.cboVldFlds.Value
lnOldRec = RECNO(loFormSet.lcTmpCodes)
llDelete = .T.

SELECT (loFormSet.lcTmpCodes)
lcCode = CCODE_NO

IF !llNoAsk
  IF RECNO() < 0
    llDelete = gfModalGen("QRM00035B00007","ALERT") = 1
  ELSE
    *** Do you wish to check the data files ***
    *** before removing this code? ***
    *** < Check data > - < Cancel >
    llDelete = gfModalGen("QRM00040B00013","ALERT") = 1
    IF llDelete
      llDelete = lfChckCode(loFormSet, lcCode)
    ENDIF
  ENDIF
ENDIF

IF llDelete
  *--Delete Grid Line
  SELECT (loFormSet.lcTmpCodes)
  
  *WSH [Start]
  REPLACE cStatus WITH 'S'
  *WSH [End]
  
  DELETE
  LOCATE
  
  *--Delete Related Fields...
  SELECT (loFormSet.lcTmpRelFlds)
  
  *WSH [Start]
  *=SEEK(lcCode+lcVldFld)
  *DELETE REST WHILE CCODE_NO+CRLTD_NAM = lcCode+lcVldFld
  *
  **--Delete Records added to master file...
  *IF loFormSet.loCodes.SEEK('N'+lcVldFld+lcCode)
  *  SELECT (loFormSet.lcCodes)
  *  SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+lcVldFld+lcCode
  *    loFormSet.loCodes.DELETE()
  *  ENDSCAN
  *ENDIF
  *IF loFormSet.loCodes.SEEK('D'+lcVldFld+lcCode)
  *  loFormSet.loCodes.DELETE()
  *ENDIF
  =SEEK(lcCode)
  REPLACE cStatus WITH 'S' REST WHILE CCODE_NO+CRLTD_NAM = lcCode
  *WSH [End]
  
  =SEEK(lcCode)
  DELETE REST WHILE CCODE_NO+CRLTD_NAM = lcCode
  
  IF loFormSet.llRlt_Fld
    SELECT (loFormSet.lcTmpCodes)
    LOCATE
    loFormSet.AriaForm1.cmdRelFlds.Enabled = !EOF()
  ELSE
    loFormSet.AriaForm1.cmdRelFlds.Enabled = .F.
  ENDIF  
  
  SELECT (loFormSet.lcTmpCodes)
  LOCATE
  loFormSet.AriaForm1.cmdRemove.Enabled = !EOF(loFormSet.lcTmpCodes)
  
  IF lnOldRec <> 0 AND lnOldRec < RECCOUNT()
    GOTO (lnOldRec)
  ENDIF
ENDIF

loFormSet.AriaForm1.grdCodes.SetFocus()
loFormSet.Refresh()

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfCheckFile
*! Developer : MAH
*! Date      : 9/11/2011
*! Purpose   : Check if file module exists on company modules
*!*************************************************************
*! Example   : =lfCheckFile('AP,AR', 'AP|SO')
*!*************************************************************
FUNCTION lfCheckFile
LPARAMETERS lcFileModules, lcCompanyModules

LOCAL laFileModules[1]
=gfSubStr(lcFileModules, @laFileModules)


*B609729,1 Sometimes the modules starts with empty spaces ....... BEGIN
*IF EMPTY(laFileModules)
*  RETURN .F.
*ELSE
*  LOCAL lnIndex
*  FOR lnIndex = 1 TO ALEN(laFileModules, 1)
*    IF ALLTRIM(UPPER(laFileModules[lnIndex])) $ UPPER(lcCompanyModules)
*      RETURN .T.
*    ENDIF
*  ENDFOR
*ENDIF

LOCAL lnIndex
FOR lnIndex = 1 TO ALEN(laFileModules, 1)
  IF EMPTY(laFileModules[lnIndex])
    LOOP
  ENDIF
  IF ALLTRIM(UPPER(laFileModules[lnIndex])) $ UPPER(lcCompanyModules)
    RETURN .T.
  ENDIF
ENDFOR
*B609729,1 Sometimes the modules starts with empty spaces ....... END
RETURN .F.

*!*************************************************************
*! Name      : lfChckCode
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/22/2005
*! Purpose   : Check for existance of Removed Code in data files
*!*************************************************************
*! Example   : =lfChckCode()
*!*************************************************************
FUNCTION lfChckCode
LPARAMETERS loFormSet, lcCode

LOCAL lnCurRec, lcFile, llFound, lnAlias, lcStat, lnStLen, lnLen, lcCdTyp, lcVldField, llStyCod
lnAlias    = SELECT(0)
lnCurRec   = 0
llFound    = .F.    && To know if there is any file have this code or not.
lcFileCurs = gfTempName()

*** Array to store the files names that contain the field ***
*** that has to be remove from the temp. file .....
DECLARE laFileList[1,4]

*** Gather the files that contain the removed field ***
lnLen      = 1
lnStLen    = 1
lcCdTyp    = SPACE(0)
llStyCod   = .F.
lcVldField = loFormSet.AriaForm1.cboVldFlds.Value

IF INLIST(lcVldField, "SEASON", "COLOR", "CDIVISION", "CSTYGROUP")
  lcCdTyp = IIF(lcVldField = 'SEASON', 'Z', IIF(lcVldField = 'COLOR', 'C', IIF(lcVldField = 'CDIVISION', 'D', 'G')))
ENDIF

DIMENSION laMajSeg[1,1]
=gfItemMask(@laMajSeg, loFormSet.lcDataDir)

lcSetEx = SET('EXACT')
SET EXACT ON

IF !EMPTY(lcCdTyp) .AND. ASCAN(laMajSeg, lcCdTyp) # 0
  FOR lnC = 1 TO ALEN(laMajSeg,1)
    IF laMajSeg[lnC,1] = lcCdTyp
      lnLen = LEN(laMajSeg[lnC,3])
      llStyCod = .T.
      EXIT
    ENDIF
    lnStLen = lnStLen + LEN(laMajSeg[lnC,3])
  ENDFOR
ENDIF
SET EXACT &lcSetEx

LOCAL loSydFlFld, lcSydFlFld
lcSydFlFld = gfTempName()

* T20110908.0023,1 MAH 9/11/2011 [START]
*!*	lcStat = "SELECT SYDFLFLD.cFile_Nam, SYDFILES.cFile_Ttl " +;
*!*	         "  FROM SYDFLFLD INNER JOIN SYDFILES ON SYDFLFLD.cFile_Nam == SYDFILES.cFile_Nam " +;
*!*	         " WHERE CFLD_NAME = '" + lcVldField + "'"
LOCAL lcCompany, lcCompanyModules, lcFileApp
lcCompany = gfTempName()


IF !lfSQLStatement("SELECT mcomp_mdl FROM SYCCOMP WHERE ccomp_id = '" + oAriaApplication.ActiveCompanyID + "'", lcCompany, '', '', .T., oAriaApplication.SystemConnectionString)
  SELECT (lnAlias)
  RETURN .F.
ENDIF

lcCompanyModules = mcomp_mdl

lcStat = "SELECT SYDFLFLD.cFile_Nam, SYDFILES.cFile_Ttl, SYDFILES.mfile_app " +;
         "  FROM SYDFLFLD INNER JOIN SYDFILES ON SYDFLFLD.cFile_Nam == SYDFILES.cFile_Nam " +;
         " WHERE CFLD_NAME = '" + lcVldField + "'"
* T20110908.0023,1 MAH 9/11/2011 [END]
*BADRAN B609729,1 ..................................................... begin
*!*	IF !lfSQLStatement(lcStat, lcSydFlFld, '', '', .T., oAriaApplication.cAria4SysFiles)
*!*	  SELECT (lnAlias)
*!*	  RETURN .F.
*!*	ENDIF
IF !FileFieldCreated(lcStat, lcSydFlFld)
  SELECT (lnAlias)
  RETURN .F.
ENDIF
*BADRAN B609729,1..................................................... end

lnFiles = 0
SELECT (lcSydFlFld)
SCAN
  * T20110908.0023,1 MAH 9/11/2011 [START]
  lcFileApp = &lcSydFlFld..mfile_app
  IF !lfCheckFile(lcFileApp, lcCompanyModules)
    LOOP
  ENDIF 
  * T20110908.0023,1 MAH 9/11/2011 [END]
  lnFiles = lnFiles + 1
  DIMENSION laFileList[lnFiles,4]

  laFileList[lnFiles,1] = cFile_Nam
  laFileList[lnFiles,2] = 'N'
  laFileList[lnFiles,3] = cFile_Ttl
ENDSCAN

IF llStyCod
  * T20110908.0023,1 MAH 9/11/2011 [START]
  *!*	  lcStat = "SELECT SYDFLFLD.cFile_Nam, SYDFILES.cFile_Ttl " +;
  *!*	           "  FROM SYDFLFLD INNER JOIN SYDFILES ON SYDFLFLD.cFile_Nam == SYDFILES.cFile_Nam " +;
  *!*	           " WHERE CFLD_NAME = 'STYLE '"
  lcStat = "SELECT SYDFLFLD.cFile_Nam, SYDFILES.cFile_Ttl, SYDFILES.mfile_app " +;
           "  FROM SYDFLFLD INNER JOIN SYDFILES ON SYDFLFLD.cFile_Nam == SYDFILES.cFile_Nam " +;
           " WHERE CFLD_NAME = 'STYLE '"
  * T20110908.0023,1 MAH 9/11/2011 [END]
  *BADRAN B609729,1..................................................... begin
  *!*	IF !lfSQLStatement(lcStat, lcSydFlFld, '', '', .T., oAriaApplication.cAria4SysFiles)
  *!*	  SELECT (lnAlias)
  *!*	  RETURN .F.
  *!*	ENDIF
  IF !FileFieldCreated(lcStat, lcSydFlFld)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
  *BADRAN B609729,1..................................................... end
  
  LOCAL loSydFdChg, loSydFiles, lcSydFdChg, lcSydFiles
  lcSydFdChg = gfTempName()
  lcSydFiles = gfTempName()
  loSydFdChg = CREATEOBJECT('RemoteTable', 'SYDFDCHG', 'SYDFDCHG', lcSydFdChg, SET("Datasession"))
  loSydFiles = CREATEOBJECT('RemoteTable', 'SYDFILES', 'CFILE_NAM', lcSydFiles, SET("Datasession"))
  
  SELECT (lcSydFlFld)
  SCAN
    * T20110908.0023,1 MAH 9/11/2011 [START]
    lcFileApp = &lcSydFlFld..mfile_app
    IF !lfCheckFile(lcFileApp, lcCompanyModules)
      LOOP
    ENDIF 
    * T20110908.0023,1 MAH 9/11/2011 [END]
    lnFiles = lnFiles + 1
    DIMENSION laFileList[lnFiles,4]
    
    laFileList[lnFiles,1] = cFile_Nam
    laFileList[lnFiles,2] = 'S'
    laFileList[lnFiles,3] = cFile_Ttl
  ENDSCAN
  
  IF loSydFdChg.SEEK('KSTYLE')
    SELECT (lcSydFdChg)
    SCAN REST WHILE cKeyType+cFld_Name = 'KSTYLE'
      loSydFiles.SEEK(cFile_Nam)
      
      * T20110908.0023,1 MAH 9/11/2011 [START]
      lcFileApp = &lcSydFiles..mfile_app
      IF !lfCheckFile(lcFileApp, lcCompanyModules)
        LOOP
      ENDIF 
      * T20110908.0023,1 MAH 9/11/2011 [END]

      lnFiles = lnFiles + 1
      DIMENSION laFileList[lnFiles,4]
      
      laFileList[lnFiles,1] = cFile_Nam
      laFileList[lnFiles,2] = 'K'
      laFileList[lnFiles,3] = EVALUATE(lcSydFiles + '.cFile_Ttl')
      laFileList[lnFiles,4] = EVALUATE(lcSydFdChg + '.cAltField')
    ENDSCAN
  ENDIF
  
  STORE .NULL. TO loSydFdChg, loSydFiles
ENDIF

*607937,1 TMI [Start] check the case of "Unit of measure" code
IF lcVldField = "CUNTOFMGR" 
  lnFiles = lnFiles + 1
  DIMENSION laFileList[lnFiles,4]
  laFileList[lnFiles,1] = 'UOM'
  laFileList[lnFiles,2] = 'K'
  laFileList[lnFiles,3] = 'UOM Conversion (Base Unit of Measure)'
  laFileList[lnFiles,4] = 'CUOM_B'

  lnFiles = lnFiles + 1
  DIMENSION laFileList[lnFiles,4]
  laFileList[lnFiles,1] = 'UOM'
  laFileList[lnFiles,2] = 'K'
  laFileList[lnFiles,3] = 'UOM Conversion (Converted Unit of Measure)'
  laFileList[lnFiles,4] = 'CUOM_V'
  
  lnStLen = 0
  lnLen   = 6
ENDIF
*607937,1 TMI [End  ] 


IF USED(lcSydFlFld)
  USE IN (lcSydFlFld)
ENDIF

IF lnFiles = 0
  *** There is no file have this code, you can ***
  *** delete it.  Do you want to delete it?    ***
  *** <  Yes  >  -  <  No  > ***
  SELECT (lnAlias)
  RETURN gfModalGen("TRM00201B00006","DIALOG") = 1
ELSE
  LOCAL loProgress, llNative, lnSQLNat
  
  *--Create and Initiate the Progressbar Object...
  loProgress = CREATEOBJECT('AriaProgressBar')
  
  loProgress.TotalProgress = lnFiles
  loProgress.lblFirstLabel.Caption  = LANG_SMCODES_CHKEXIST + IIF(loFormSet.llIsEditable, lcCode + ' - ', '') + ALLTRIM(EVALUATE(loFormSet.lcTmpCodes + '.cDiscrep')) + LANG_SMCODES_EXISTIN
  loProgress.lblSecondLabel.Caption = ''
  loProgress.Left = loFormSet.AriaForm1.Left + (loFormSet.AriaForm1.Width - loProgress.Width) / 2
  loProgress.TOP  = loFormSet.AriaForm1.Top + (loFormSet.AriaForm1.Height - loProgress.Height) / 2
  loProgress.Show()
  
  *--Open Field Tables Cursor
  IF USED(loFormSet.lcTmpHold)
    USE IN (loFormSet.lcTmpHold)
  ENDIF
  USE (oAriaApplication.WorkDir + loFormSet.lcTmpHold) IN 0 ALIAS (loFormSet.lcTmpHold) EXCLUSIVE
  SELECT (loFormSet.lcTmpHold)
  SET ORDER TO CFILE_NAM
  ZAP

  FOR lnCount2 = 1 TO lnFiles
    lnCurRec = lnCurRec + 1
    lcFile   = ALLTRIM(laFileList[lnCount2,1])
    lnSQLNat = lfIsNative(lcFile)
    
    IF lnSQLNat = 0
      loProgress.FinishProgress()
      SELECT (lnAlias)
      RETURN .F.
    ENDIF
    llNative = (lnSQLNat = 1)
    
    loProgress.lblSecondLabel.Caption = LANG_SMCODES_EXISTFL + lcFile
    loProgress.CurrentProgress(lnCurRec)
    
    *T20070109.0025 TMI [Start] for the case of CUNOFMGR field do not check its existence in tmpHold cursor
    *IF SEEK(laFileList[lnCount2,1], loFormSet.lcTmpHold)
    IF IIF(lcVldField <> "CUNTOFMGR" , SEEK(laFileList[lnCount2,1], loFormSet.lcTmpHold) , .F. )
      *T20070109.0025 TMI [End  ] 
      LOOP
    ENDIF
    
    *-- Build the SQL Statement to lock for the code in Data Files...
    DO CASE
      CASE laFileList[lnCount2,2] = 'N'
        lcStat = "SELECT TOP 1 " + lcVldField + " FROM " + lcFile + " WHERE " + lcVldField + " = '" + lcCode + "'" +;
                 " ORDER BY " + lcVldField
      CASE laFileList[lnCount2,2] = 'S'
        lcStat = "SELECT TOP 1 Style FROM " + lcFile + " WHERE " + IIF(llNative, "SUBSTR", "SUBSTRING") + "(Style, " + STR(lnStLen + 1) + ", " + STR(lnLen) + ") = '" + lcCode + "'" +;
                 " ORDER BY Style"
      CASE laFileList[lnCount2,2] = 'K'
        lcFldtmp = IIF(EMPTY(laFileList[lnCount2,4]), 'STYLE', ALLTRIM(laFileList[lnCount2,4]))
        lcStat = "SELECT TOP 1 " + lcFldtmp + " FROM " + lcFile + " WHERE " + IIF(llNative, "SUBSTR", "SUBSTRING") + "(" + lcFldtmp + ", " + STR(lnStLen + 1) + ", " + STR(lnLen) + ") = '" + lcCode + "'" +;
                 " ORDER BY " + lcFldtmp
    ENDCASE
    
    IF !lfSQLStatement(lcStat, lcFileCurs, '', '', .T., IIF(llNative, loFormSet.lcNativeConStr, loFormSet.lcSQLConStr))
      
      *WSH [Start]
      *loProgress.FinishProgress()
      *SELECT (lnAlias)
      *RETURN .F.
      LOOP
      *WSH [Start]
    ENDIF
    
    SELECT (lcFileCurs)
    LOCATE
    IF !EOF()
      llFound = .T.
      INSERT INTO (oAriaApplication.WorkDir + loFormSet.lcTmpHold) (cFile_nam, cFile_ttl) ;
             VALUES (laFileList[lnCount2,1], laFileList[lnCount2,3])
    ENDIF
  ENDFOR
  loProgress.FinishProgress()
  
  IF USED(lcFileCurs)
    USE IN (lcFileCurs)
  ENDIF
  
  IF llFound
    *-- Display all files that have this code in its data...
    DO FORM (oAriaApplication.ScreenHome + 'SM\SMFLCD') WITH loFormSet.lcTmpHold, LANG_SMCODES_FLUSE + IIF(loFormSet.llIsEditable, lcCode + ' - ', '') + ALLTRIM(EVALUATE(loFormSet.lcTmpCodes + '.cDiscrep'))
  ELSE
    *** There is no file have this code, you can ***
    *** delete it.  Do you want to delete it?    ***
    *** <  Yes  >  -  <  No  > ***
    IF gfModalGen("TRM00201B00006","DIALOG") = 1
      USE IN (loFormSet.lcTmpHold)
      SELECT (lnAlias)
      RETURN .T.
    ENDIF
  ENDIF
  
  USE IN (loFormSet.lcTmpHold)
ENDIF

SELECT (lnAlias)
RETURN .F.

*!*************************************************************
*! Name      : lfvReltdFld
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/22/2005
*! Purpose   : Valid Function for Related Felds Button
*!*************************************************************
*! Example   : =lfvReltdFld()
*!*************************************************************
FUNCTION lfvReltdFld
LPARAMETERS loFormSet

LOCAL lnI, lcCode, lcDescrip, lnAlias, laRelFlds[1], lnRelFlds, lcVldFld, IcitialValue
lnAlias   = SELECT(0)
lcCode    = EVALUATE(loFormSet.lcTmpCodes + '.cCode_No')
lcDescrip = EVALUATE(loFormSet.lcTmpCodes + '.cDiscrep')
lcVldFld  = loFormSet.AriaForm1.cboVldFlds.Value
llRetVal  = .F.

*!*  IF ALLTRIM(laCodes[lnActCode,2]) = "SEASON" .AND. ASCAN(laEvntTrig , PADR('CODESCR',10)) <> 0
*!*    =gfDoTriger('SMCODES',PADR('CODESCR',10))
*!*    RETURN
*!*  ENDIF

IF loFormSet.llIsEditable
  IF EMPTY(lcCode)
    *** You should enter a description for this code first...! ***
    *** <  Ok  > ***
    = gfModalGen("TRM00041B00000","DIALOG")
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ELSE
  IF EMPTY(lcDescrip)
    *** You should enter a description for this code first...! ***
    *** <  Ok  > ***
    = gfModalGen("TRM00041B00000","DIALOG")
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDIF

lnRelFlds = ALEN(loFormSet.laRelFld, 1)

DIMENSION laRelFlds(lnRelFlds, 8)

FOR lnI = 1 TO lnRelFlds
  IcitialValue = IIF(loFormSet.laRelFld[lnI,3] = 'C', '', IIF(loFormSet.laRelFld[lnI,3] = 'N', '0', 'F'))

  SELECT (loFormSet.lcTmpRelFlds)
  IF SEEK(lcCode  + IIF(LEFt(loFormSet.laRelFld[lnI,1], 1) == '$', SUBSTR(loFormSet.laRelFld[lnI,1], 2), loFormSet.laRelFld[lnI,1]))
    IcitialValue = EVALUATE(loFormSet.lcTmpRelFlds + '.cRltd_Vlu')
  ENDIF

  DO CASE
    CASE loFormSet.laRelFld[lnI,3] = 'L'
      IcitialValue = IIF(IcitialValue = 'T', .T., .F.)
    CASE loFormSet.laRelFld[lnI,3] = "D"
      IcitialValue = CTOD(IcitialValue)
    CASE loFormSet.laRelFld[lnI,3] = "N"
      
      *:B608750,1 MMT 12/03/2008 Fix bugs of rounding while displaying related fields[Start]
      IF loFormSet.laRelFld[lnI,5] > 0
        lcSetDec = SET("Decimals")
        SET DECIMALS TO loFormSet.laRelFld[lnI,5]
      ENDIF 
      *:B608750,1 MMT 12/03/2008 Fix bugs of rounding while displaying related fields[End]

      IcitialValue = VAL(IcitialValue)
      
      *:B608750,1 MMT 12/03/2008 Fix bugs of rounding while displaying related fields[Start]
      IF loFormSet.laRelFld[lnI,5] > 0
        SET DECIMALS TO &lcSetDec 
      ENDIF 
	  *:B608750,1 MMT 12/03/2008 Fix bugs of rounding while displaying related fields[End]

    CASE loFormSet.laRelFld[lnI,3] = "C" AND !EMPTY(loFormSet.laRelFld[lnI, 12]) AND EMPTY(IcitialValue)
      *-- Make the first valid entry value a Default value for the combo box if initial value is empty...
      LOCAL lcValueSeparat, lcElemnSeparat, lnValSep, lnElemSep
      lcValueSeparat = IIF(TYPE("lcValSep") = "C", lcValSep, "~")
      lcElemnSeparat = IIF(TYPE("lcElmSep") = "C", lcElmSep, "|")
      lnValSep       = ATC(lcValueSeparat, loFormSet.laRelFld[lnI, 12])
      lnElemSep      = ATC(lcElemnSeparat, SUBSTR(loFormSet.laRelFld[lnI, 12], lnValSep + 1))

      IF lnValSep <> 0
        IcitialValue = ALLTRIM(SUBSTR(loFormSet.laRelFld[lnI, 12], lnValSep + 1, lnElemSep - 1))
        IcitialValue = STRTRAN(IcitialValue, '@', '')
      ENDIF
  ENDCASE
  
  laRelFlds[lnI,1] = IIF(LEFt(loFormSet.laRelFld[lnI,1], 1) == '$', SUBSTR(loFormSet.laRelFld[lnI,1], 2), loFormSet.laRelFld[lnI,1])
  laRelFlds[lnI,2] = 'F'
  laRelFlds[lnI,3] = loFormSet.laRelFld[lnI,3]
  laRelFlds[lnI,4] = .T.
  
  *B039820,1 WSH 11/20/2005 Change the work Like to Is... [Start]
  *laRelFlds[lnI,5] = 'Like'
  laRelFlds[lnI,5] = 'Like'
  *B039820,1 WSH 11/20/2005 [End]
  
  laRelFlds[lnI,6] = IcitialValue
  laRelFlds[lnI,7] = 'V'
  laRelFlds[lnI,8] = loFormSet.laRelFld[lnI,8]
ENDFOR

WITH loFormSet.AriaForm1.cboVldFlds
  LOCAL lcOgTitle, lnPos
  lnPos     = ASCAN(.aSourceArray, .Value, 1, ALEN(.aSourceArray), 0, 14)
  lcOgTitle = LANG_SMCODES_RELFLDS + '"' + ALLTRIM(.aSourceArray[lnPos, 1]) + '"'
ENDWITH

lcRetVal = gfOpGrid('SMCODES', .T., IIF(loFOrmSet.ActiveMode $ 'AE', 'T', 'X'), .F., .F., .F., @laRelFlds, loFormSet.lcTmpRelFlds, lcOgTitle)

IF loFormSet.ActiveMode $ 'AE'
  LOCAL lcValue

  FOR lnI = 1 TO lnRelFlds
    lcValue = laRelFlds[lnI,6]

    SELECT (loFormSet.lcTmpRelFlds)
    IF !SEEK(lcCode + laRelFlds[lnI,1])
      APPEND BLANK
      REPLACE cDefCode  WITH 'N',;
              cFld_Name WITH lcVldFld,;
              cCode_No  WITH lcCode,;
              cRltField WITH 'Y',;
              cRltd_Typ WITH loFormSet.laRelFld[lnI,3],;
              cRltd_Nam WITH laRelFlds[lnI,1],;
              cDiscrep  WITH ''
    ENDIF

    DO CASE
      CASE loFormSet.laRelFld[lnI,3] = 'L'
        lcValue = IIF(lcValue = .T., 'T', 'F')
      CASE loFormSet.laRelFld[lnI,3] = "D"
        lcValue = DTOC(lcValue)
      CASE loFormSet.laRelFld[lnI,3] = "N"
        lcValue = ALLTRIM(STR(lcValue, loFormSet.laRelFld[lnI,4], loFormSet.laRelFld[lnI,5]))
    ENDCASE
    
    REPLACE cRltd_Vlu WITH lcValue
    
    *WSH [Start]
    REPLACE cStatus WITH 'S'
    *WSH [End]
    
    *=gfAdd_Info(loFormSet.lcTmpRelFlds, loFormSet)
    
    *WSH [Start]
    *IF !loFormSet.loCodes.SEEK('N' + lcVldFld + lcCode + SPACE(30) + laRelFlds[lnI,1])
    *  loFormSet.loCodes.APPEND()
    *  loFormSet.loCodes.REPLACE(IIF(loFormSet.loCodes.llNative, "", "Rec_No WITH Rec_No"))
    *  SELECT (loFormSet.loCodes.lcCursorUpdate)
    *  REPLACE cDefCode  WITH 'N',;
              cFld_Name WITH lcVldFld,;
              cCode_No  WITH lcCode,;
              cRltField WITH 'Y',;
              cRltd_Typ WITH loFormSet.laRelFld[lnI,3],;
              cRltd_Nam WITH laRelFlds[lnI,1],;
              cDiscrep  WITH ''
    *ENDIF
    *loFormSet.loCodes.REPLACE("cRltd_Vlu WITH '" + lcValue + "'")
    *=gfAdd_Info(loFormSet.loCOdes.lcCursorUpdate)
    *WSH [End]
    
  ENDFOR
  
  llRetVal = .T.
ENDIF

SELECT (lnAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lfBeforeSave
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/22/2005
*! Purpose   : Befor Save Method for the main Code Screen
*!*************************************************************
*! Example   : =lfBeforeSave()
*!*************************************************************
PROCEDURE lfBeforeSave
LPARAMETERS loFormSet

LOCAL lnAlias, lcVldFld, lnTmpRec
lnAlias  = SELECT(0)
lcVldFld = loFormSet.AriaForm1.cboVldFlds.Value
lnTmpRec = RECNO(loFormSet.lcTmpCodes)

SELECT (loFormSet.lcTmpCodes)
IF loFormSet.llIsEditable
  IF SEEK('N'+lcVldFld+SPACE(6))
    = gfModalGen("TRM00211B00000","DIALOG", LANG_SMCODES_SCODE + "|" + LANG_SMCODES_SCODE)
    IF lnTmpRec <> 0 AND lnTmpRec <= RECCOUNT()
      GOTO (lnTmpRec)
    ENDIF
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ELSE
  SELECT (loFormSet.lcTmpCodes)
  LOCATE FOR EMPTY(cDiscrep) AND cRltField = "N"
  IF FOUND()
    *** You cannot save an empty description, ***
    *** You  have to  enter a description  or ***
    *** remove this code. ***
    = gfModalGen("TRM00211B00000","DIALOG",LANG_SMCODES_SDESC + "|" + LANG_SMCODES_SDESC)
    IF lnTmpRec <> 0 AND lnTmpRec <= RECCOUNT()
      GOTO (lnTmpRec)
    ENDIF
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDIF

*WSH [Start]
*IF loFormSet.loCodes.SEEK('N'+lcVldFld) AND !loFormSet.loCodes.SEEK('D'+lcVldFld)
SELECT (loFormSet.lcTmpCodes)
LOCATE FOR llDefCode
IF !FOUND()
*WSH [End]

  *** A default code value should be assigned, cannot save. ***
  = gfModalGen("TRM00256B00000","DIALOG")
  SELECT (lnAlias)
  RETURN .F.
ENDIF

IF lnTmpRec <> 0 AND lnTmpRec <= RECCOUNT()
  GOTO (lnTmpRec)
ENDIF

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/22/2005
*! Purpose   : Save Method for the main Code Screen
*!*************************************************************
*! Example   : =lpSavScr()
*!*************************************************************
PROCEDURE lpSavScr
LPARAMETERS loFormSet

LOCAL lnAlias, lcVldFld, lnTmpRec, lcDefCode, lcComp, lcTranCode, lcCodOrd, lcRltOrd
lnAlias   = SELECT(0)
lcVldFld  = loFormSet.AriaForm1.cboVldFlds.Value
lcDefCode = ''
lcComp    = loFormSet.AriaForm1.cboCompanies.Value
lcCodOrd  = ORDER(loFormSet.lcTmpCodes)
lcRltOrd  = ORDER(loFormSet.lcTmpRelFlds)

*C201070 TMI 12/01/2008 [Start] add the lines of "CCHRGCODE" related to the "CORDCHG" code for DIR03
IF ALLTRIM(loFormSet.AriaForm1.cboVldFlds.Value) = "CORDCHG" .AND. ;
  ASCAN(loFormSet.laEvntTrig , PADR('MRCHDSCD',10)) <> 0
  IF !loFormSet.mDoTrigger(PADR('MRCHDSCD',10))
    RETURN .F.
  ENDIF
ENDIF
*C201070 TMI 12/01/2008 [End  ] 

*WSH [Start]
*IF loFormSet.loCodes.SEEK('D' + lcVldFld)
*  lcDefCode = EVALUATE(loFormSet.lcCodes + '.cCode_No')
*ENDIF
IF loFormSet.loCodes.SEEK('D'+CFLD_NAME, 'CCODE_NO')
  loFormSet.loCodes.Delete()
ENDIF

SELECT (loFormSet.lcTmpCodes)
LOCATE FOR llDefCode
SCATTER MEMVAR MEMO
SELECT (loFormSet.loCodes.lcCursorUpdate)
APPEND BLANK
GATHER MEMVAR MEMO
REPLACE CDEFCODE WITH 'D'

LOCAL lcOldDel
lcOldDel = SET("Deleted")

SET DELETED OFF

SELECT (loFormSet.lcTmpCodes)
SET ORDER TO cStatus
=SEEK('S')
SCAN REST WHILE cStatus = 'S'
  REPLACE CDEFCODE WITH 'N'
  
  IF loFormSet.loCodes.SEEK(CDEFCODE+CCODE_NO+CRLTFIELD+CFLD_NAME, 'CODES')
    IF DELETED()
      loFormSet.loCodes.Delete()
    ELSE
      SCATTER MEMVAR MEMO
      SELECT (loFormSet.lcCodes)
      *:B607937,1 SSH 01/23/2007 Check if Code deleted recall, to prevent not saving codes 
      *:B607937,1 SSH 01/23/2007 incase reenter the same code again
      IF DELETED()
        RECALL
      ENDIF
      *:B607937,1 SSH 01/23/2007 [End]
      GATHER MEMVAR MEMO
      loFormSet.loCodes.Replace('')
      =gfAdd_Info(loFormSet.loCOdes.lcCursorUpdate, loFormSet)
    ENDIF
  ELSE
    IF !DELETED()
      SCATTER MEMVAR MEMO
      SELECT (loFormSet.loCodes.lcCursorUpdate)
      APPEND BLANK
      GATHER MEMVAR MEMO
      =gfAdd_Info(loFormSet.loCOdes.lcCursorUpdate, loFormSet)
    ENDIF
  ENDIF
  
  SELECT (loFormSet.lcTmpCodes)
  IF llDefCode
    REPLACE CDEFCODE WITH 'D'
  ENDIF
ENDSCAN
SET ORDER TO &lcCodOrd

SELECT (loFormSet.lcTmpRelFlds)
SET ORDER TO cStatus
=SEEK('S')
SCAN REST WHILE cStatus = 'S'
  IF loFormSet.loCodes.SEEK(CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM, 'CCODE_NO')
    IF DELETED()
      loFormSet.loCodes.Delete()
    ELSE
      SCATTER MEMVAR MEMO
      SELECT (loFormSet.lcCodes)
      *:B607937,1 SSH 01/23/2007 Check if Code deleted recall, to prevent not saving codes 
      *:B607937,1 SSH 01/23/2007 incase reenter the same code again
      IF DELETED()
        RECALL
      ENDIF
      *:BB607937,1 SSH 01/23/2007 [End]
      GATHER MEMVAR MEMO
      loFormSet.loCodes.Replace('')
      =gfAdd_Info(loFormSet.loCOdes.lcCursorUpdate, loFormSet)
    ENDIF
  ELSE
    IF !DELETED()
      SCATTER MEMVAR MEMO
      SELECT (loFormSet.loCodes.lcCursorUpdate)
      APPEND BLANK
      GATHER MEMVAR MEMO
      =gfAdd_Info(loFormSet.loCOdes.lcCursorUpdate, loFormSet)
    ENDIF
  ENDIF
ENDSCAN
SET ORDER TO &lcRltOrd

SET DELETED &lcOldDel
*WSH [End]

*-- Begin Updating Transaction
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(loFormSet.lcSQLConStr, 3, '')

*-- Check Resule for Begin Transaction
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran", lcTranCode, .T.)
  RETURN .F.
ENDIF

*WSH [Start]
*-- Update Master Codes File
loFormSet.loCodes.TABLEUPDATE(lcTranCode)
*WSH [End]

*--Automatic set new sites with communication information
SELECT (loFormSet.lcTmpCodes)
LOCATE FOR cFld_Name = 'CSITEID' AND cCode_No = lcdefcode AND RECNO() < 0

IF FOUND() AND 'NC' $ oAriaApplication.CompanyInstalledModules
  LOCAL loSetups

  loSetups = CREATEOBJECT('RemoteTable', 'SETUPS', 'MODVAR', .F., SET("Datasession"), lcComp)

  IF loSetups.SEEK('NC'+'M_CURRSITE')
    loSetups.REPLACE("MData_Def WITH '" + lcdefcode + "'")
    loSetups.TABLEUPDATE(lcTranCode)
  ENDIF
  
  loSetups = .NULL.
ENDIF

SELECT (loFormSet.lcTmpCodes)
LOCATE FOR cFld_Name = 'CSITEID' AND cCode_No <> lcdefcode AND RECNO() < 0

IF FOUND() AND 'NC' $ oAriaApplication.CompanyInstalledModules AND gfGetMemVar('M_AUTOSET', lcComp) = 'Y'
  LOCAL lcCountry, loSYCEDIPH, loSYCEDIPD, loSYCEDITR, loCUSTOMER, loEDIPH, loEDIPD, loEDIACPRT, loEDINET, llExitLoop
  LOCAL lcComPhon, lcInsStat, lcClass, lcTermCode, lcShipVia, lcSpcInst, lcRegion, llFound, lcAccount, lcPHoneNo, lcEMail
  STORE '' TO lcClass, lcTermCode, lcShipVia, lcSpcInst, lcRegion, lcAccount, lcPHoneNo, lcEMail
  
  SELECT (loFormSet.lcSysComp)
  IF SEEK(lcComp)
    lcCountry = UPPER(ALLTRIM(EVALUATE(loFormSet.lcSysComp + '.cCont_Code')))
    lcComPhon = UPPER(ALLTRIM(EVALUATE(loFormSet.lcSysComp + '.cCom_Phon')))
  ENDIF
  
  loSYCEDIPH  = CREATEOBJECT('RemoteTable', 'SYCEDIPH', 'Partner', .F., SET("Datasession"), lcComp)
  loSYCEDIPD  = CREATEOBJECT('RemoteTable', 'SYCEDIPD', 'Trans', .F., SET("Datasession"), lcComp)
  loSYCEDITR  = CREATEOBJECT('RemoteTable', 'SYCEDITR', 'Codetype', .F., SET("Datasession"), lcComp)
  loCUSTOMER  = CREATEOBJECT('RemoteTable', 'CUSTOMER', 'CUSTOMER', .F., SET("Datasession"), lcComp)
  loEDIPH     = CREATEOBJECT('RemoteTable', 'EDIPH', 'Partner', .F., SET("Datasession"), lcComp)
  loEDIPD     = CREATEOBJECT('RemoteTable', 'EDIPD', 'Parttrans', .F., SET("Datasession"), lcComp)
  loEDIACPRT  = CREATEOBJECT('RemoteTable', 'EDIACPRT', 'EDIACPRT', .F., SET("Datasession"), lcComp)
  loEDINET    = CREATEOBJECT('RemoteTable', 'EDINET', 'EDINET', .F., SET("Datasession"), lcComp)

  *--Get Default Class, Payment terms, Ship Via, Special Instruction
  SELECT (loFormSet.lcCodes)
  lcCodeOrd = ORDER()
  loCodes.SETORDER("Idrltfname")

  =loCodes.SEEK('DNCLASS')
  lcClass = cCode_No

  =loCodes.SEEK('DNCTERMCODE')
  lcTermCode = cCode_No

  =loCodes.SEEK('DNSHIPVIA')
  lcShipVia = cCode_No

  =loCodes.SEEK('DNSPCINST')
  lcSpcInst = cCode_No

  =loCodes.SEEK('DNREGION')
  lcRegion = cCode_No
  
  loCodes.SETORDER(lcCodeOrd)

  SELECT (loFormSet.lcTmpRelFlds)
  LOCATE
  DO WHILE !EOF()
    lcSiteId   = ccode_no
    lcSiteName = cDiscRep

    STORE '' TO lcAccount,lcPHoneNo,lcEMail
    llFound = .F.
    SCAN REST WHILE cCode_No+cRltd_Nam = lcSiteId ;
              FOR   cCode_No <> lcDefcode AND RECNO() < 0
      llFound = .T.
      IF cRltd_Nam = 'ACCOUNT'
        lcAccount = PADR(cRltd_Vlu, 5)
      ENDIF  
      IF cRltd_Nam = 'PHONE'
        lcPHoneNo = PADR(cRltd_Vlu, 15)
      ENDIF  
      IF cRltd_Nam = 'CEMAIL_ADD'
        lcEMail = ALLTRIM(cRltd_Vlu)
      ENDIF  
    ENDSCAN

    IF llFound
      *--Add new customer
      IF !EMPTY(lcAccount) AND !loCUSTOMER.SEEK('M'+lcAccount)
        lcInsStat = "(TYPE,ACCOUNT,BillTo,Status,StName,btName,Link_Code,Consol,Priority,Region,Class," +;
                    "cTermCode,ShipVia,Spcinst,cInSur,PriceLvl,SkuTmpl,Prnt_Statm,cTaxRule,cCont_Code," +;
                    "cCont_Cod2,cAddress6,cAddress62) VALUES " +;
                    "('M','" + lcAccount + "','M','A','" + lcSiteName + "','" + lcSiteName + "','DEFDEF','N','5','" + lcRegion + "','" + lcClass + "','" +;
                    lcTermCode + "','" + lcShipVia + "','" + lcSpcInst + "','Y','A','DEF','Y',' 1','" + lcCountry + "','" + lcCountry + "','" + lcCountry + "','" + lcCountry + "')"
        loCUSTOMER.INSERT(lcInsStat)
        loCUSTOMER.TABLEUPDATE(lcTranCode)
      ENDIF

      *--Add new trading partner header record
      IF TYPE("loSYCEDIPH") = 'O' AND !loSYCEDIPH.SEEK(lcSiteId)
        lcInsStat = "(cPartCOde,cPartName,cIntChgVer,cVersion,cFieldSep,cLineSep,cNetWork,ccrtntype) VALUES " +;
                    "('" + lcSiteId + "','" + lcSiteName + "','00401','004010VICS',['*'],[''],'" + lcSiteId + "','P')"
        loSYCEDIPH.INSERT(lcInsStat)
        loSYCEDIPH.TABLEUPDATE(lcTranCode)
      ENDIF

      IF TYPE("loEDIPH") = 'O' AND !loEDIPH.SEEK(lcSiteId)
        lcInsStat = "(cPartCOde,cPartName,cIntChgVer,cVersion,cFieldSep,cLineSep,cNetWork,ccrtntype) VALUES " +;
                    "('" + lcSiteId + "','" + lcSiteName + "','00401','004010VICS',['*'],[''],'" + lcSiteId + "','P')"
        loEDIPH.INSERT(lcInsStat)
        loEDIPH.TABLEUPDATE(lcTranCode)
      ENDIF

      IF TYPE("loEDIACPRT") = 'O' AND !loEDIACPRT.SEEK(lcSiteId)
        lcInsStat = "(Type,cPartner,cPartCOde,cCmpIsaQl,cCmpIsaId,cCmpGsId,lInterComp,cSiteId,cmtchpoprc) VALUES " +;
                    "('A','" + lcAccount + "','" + lcSiteId + "','12','" + lcComPhon + "','" + lcComPhon + "',.T.,'" + lcSiteId + "','T')"
        loEDIACPRT.INSERT(lcInsStat)
        loEDIACPRT.TABLEUPDATE(lcTranCode)
      ENDIF

      *--Add new net work record
      IF TYPE("loEDINET") = 'O' AND !loEDINET.SEEK(lcSiteId)
        lcInsStat = "(cNetWork,cNetName,cOutFile,cInFile,lSendAck,lSendByEm) VALUES " +;
                    "('" + lcSiteId + "','" + lcSiteName + "','" + ALLTRIM(lcSiteId) + "OU.EDI','" + ALLTRIM(lcSiteId) + "IN.EDI',.T.,.T.)"
        loEDINET.INSERT(lcInsStat)
        loEDINET.TABLEUPDATE(lcTranCode)
      ENDIF

      IF TYPE("loSYCEDITR") = 'O'
        llExitLoop = !loSYCEDITR.GOTOP()
        SELECT (loSYCEDITR.lcCursorView)

        DO WHILE !llExitLoop AND ('NC' $ cBarModule)
          m.cEdiTrnTyp = cEdiTrnTyp

          IF TYPE("loSYCEDIPD") = 'O' AND !loSYCEDIPD.SEEK(lcSiteId+m.cEdiTrnTyp)
            lcInsStat = "(cPartCode,cEdiTrnTyp,cVersion,ctranactv,cPartQual,cpartid,cPartGsId,cMapSet) VALUES " +;
                        "('" + lcSiteId + "','" + m.cEdiTrnTyp + "','004010','P','12','" + lcPHoneNo + "','" + lcPHoneNo + "','POS')"
            loSYCEDIPD.INSERT(lcInsStat)
            loSYCEDIPD.TABLEUPDATE(lcTranCode)
          ENDIF

          IF TYPE("loEDIPD") = 'O' AND !loEDIPD.SEEK(lcSiteId+m.cEdiTrnTyp)
            lcInsStat = "(cPartCode,cEdiTrnTyp,cVersion,ctranactv,cPartQual,cpartid,cPartGsId,cMapSet,lTrade) VALUES " +;
                        "('" + lcSiteId + "','" + m.cEdiTrnTyp + "','004010','P','12','" + lcPHoneNo + "','" + lcPHoneNo + "','POS',.T.)"
            loEDIPD.INSERT(lcInsStat)
            loEDIPD.TABLEUPDATE(lcTranCode)
          ENDIF

          llExitLoop = !loSYCEDITR.GONEXT()
          SELECT (loSYCEDITR.lcCursorView)
        ENDDO
      ENDIF
    ENDIF
    SELECT (loFormSet.lcTmpRelFlds)
  ENDDO
  
  STORE .NULL. TO loSYCEDIPH, loSYCEDIPD, loSYCEDITR, loCUSTOMER, loEDIPH, loEDIPD, loEDIACPRT, loEDINET
ENDIF

*WSH [Start]
**-- Update Master Codes File
*loFormSet.loCodes.TABLEUPDATE(lcTranCode)
*WSH [End]

*-- Commit Changes and Check Result
lnConnHandler = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnHandler <> 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran", lnConnHandler, .T.)
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  SELECT (lnAlias)
  Return .F.
ENDIF

*-- Update Temp Cursors
SELECT (loFormSet.lcTmpCodes)
=TABLEUPDATE(.T., .T.)

SELECT (loFormSet.lcTmpRelFlds)
=TABLEUPDATE(.T., .T.)

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfvBCode
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/20/2005
*! Purpose   : Valid Function of Codes Grid Column
*!*************************************************************
*! Example   :  =lfvBCode()
*!*************************************************************
FUNCTION lfvBCode
LPARAMETERS loFormSet

LOCAL lnAlias, lcCurrKey
lnAlias = SELECT(0)

SELECT (loFormSet.lcTmpCodes)
lcCurrKey = CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM

IF EMPTY(CCODE_NO)
  SELECT (lnAlias)
  RETURN .F.
ELSE
  IF loFormSet.loCodes.SEEK(lcCurrKey)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfvDefa
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/20/2005
*! Purpose   : Valid Function of Default Code Check Box
*!*************************************************************
*! Example   :  =lfvDefa()
*!*************************************************************
FUNCTION lfvDefa
LPARAMETERS loFormSet

LOCAL lnAlias, lnOldRec, lcCode_No
lnAlias = SELECT(0)

loFormSet.AriaForm1.LockScreen = .T.

SELECT (loFormSet.lcTmpCodes)
lnOldRec  = RECNO()
lcCode_No = CCODE_NO

*--Change the Default Code in the Temp Grid Cursor...

*WSH [Start]
*IF SEEK('D', loFormSet.lcTmpCodes, 'CCODE_NO')
REPLACE llDefCode WITH .F.

LOCATE FOR llDefCode
IF FOUND()
*WSH [End]

  IF EVALUATE(loFormSet.lcTmpCodes + '.cCode_No') == lcCode_No
    loFormSet.AriaForm1.LockScreen = .F.
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
  
  *WSH [Start]
  *REPLACE cDefCode  WITH 'N',;
          llDefCode WITH .F.
  REPLACE llDefCode WITH .F.
  *WSH [End]
  
ENDIF

SELECT (loFormSet.lcTmpCodes)
GOTO (lnOldRec)

*WSH [Start]
*REPLACE cDefCode  WITH 'D',;
        llDefCode WITH .T.
*SCATTER MEMVAR MEMO
**--Change the Default Code in the Temp Grid Cursor...
*IF loFormSet.loCodes.SEEK('D' + m.cFld_Name)
*  LOCAL lcReplStr
*  lcReplStr = "cCode_No WITH '" + m.cCode_No + "'"
*  loFormSet.loCodes.REPLACE(lcReplStr)
*ELSE
*  LOCAL lcInsStr
*  *lcInsStr = "(cDefCode, cCode_No, cFld_Name, cDiscrep, cRltField) " +;
*             "VALUES ('" + m.cDefCode + "','" + m.cCode_No + "','" + m.cFld_Name +;
*                      "','" + m.cDiscrep + "','" + m.cRltField + "')"
*  lcInsStr = "(cDefCode, cCode_No, cFld_Name, cDiscrep, cRltField) " +;
*             "VALUES (m.cDefCode, m.cCode_No, m.cFld_Name," +;
*                     "m.cDiscrep, m.cRltField)"
*  loFormSet.loCodes.INSERT(lcInsStr)
*ENDIF
REPLACE llDefCode WITH .T.
*WSH [End]

loFormSet.AriaForm1.LockScreen = .F.

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfInsInit
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/28/2005
*! Purpose   : Init Event of the Insert Code Screen
*!*************************************************************
*! Example   : =lfInsInit()
*!*************************************************************
FUNCTION lfInsInit
LPARAMETERS loInsForm

LOCAL lnAlias
lnAlias = SELECT(0)

*--Adjust Controls Visibility, Positions, and Initial Values
WITH loInsForm
  IF !.loParent.llIsEditable
    .AriaForm1.cntCode.Visible = .F.
    .AriaForm1.cntDesc.Top = .AriaForm1.shpCode.Top + (.AriaForm1.shpCode.Height - .AriaForm1.cntDesc.Height) / 2
  ENDIF
  
  *B607903,1 MMT 12/24/2006 Fix bug of Field width in codes screen is not read from Codeset File[Start]  
  .ariaForm1.cntCode.txtCode.InputMask = .loParent.lcPic
  *B607903,1 MMT 12/24/2006 Fix bug of Field width in codes screen is not read from Codeset File[End]
  
  .AriaForm1.cntCode.txtCode.Value = .loParent.lcInsCode
  .AriaForm1.cntDesc.txtDesc.Value = ''


ENDWITH

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lpvCancel
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/20/2005
*! Purpose   : Undo Event in Main Codes Screen
*!*************************************************************
*! Example   :  =lpvCancel()
*!*************************************************************
FUNCTION lpvCancel
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

*--Revert Changes done to working files...
SELECT (loFormSet.lcTmpCodes)
=TABLEREVERT(.T.)
LOCATE

SELECT (loFormSet.lcTmpRelFlds)
=TABLEREVERT(.T.)
loFormSet.loCodes.TABLEREVERT()

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfvSCode
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/20/2005
*! Purpose   : Valid function of the code in the insert code screen
*!*************************************************************
*! Example   :  =lfvSCode()
*!*************************************************************
FUNCTION lfvSCode
LPARAMETERS loInsForm

LOCAL lnAlias, loFormSet, lcCurrKey, llRetVal, lcCode
lnAlias   = SELECT(0)
loFormSet = loInsForm.loParent
llRetVal  = .T.
lcCode    = loInsForm.AriaForm1.cntCode.txtCode.Value

SELECT (loFormSet.lcTmpCodes)
lcCurrKey = CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM

IF !EMPTY(lcCode) AND SEEK(lcCode, loFormSet.lcTmpCodes, loFormSet.lcTmpCodes)
  *** This field already exists. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00053B00000","DIALOG")
  llRetVal = .F.
ENDIF


*:B608515,1 MMT 04/10/2008 Fix bug of allow user to add Color Code Starts with '*'[Start]
IF loCodesFormName.ARIAform1.CBoVldFlds.Value ='COLOR' AND SUBSTR(lcCode,1,1) = '*'
  *** Code '*' is Reserved for Aria System***
  *** <  Ok  > ***
  =gfModalGen("TRM00444B00000","DIALOG")
  llRetVal = .F.
ENDIF     
*:B608515,1 MMT 04/10/2008 Fix bug of allow user to add Color Code Starts with '*'[End]

=SEEK(lcCurrKey, loFormSet.lcTmpCodes, 'CCODE_NO')

*B039820,1 WSH 11/20/2005 Enable the OK button if the user entered value [Start]
loInsForm.AriaForm1.cmdOk.Enabled = llRetVal
*B039820,1 WSH 11/20/2005 [End]

SELECT (lnAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lfvsDis
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/21/2005
*! Purpose   : Valid function of the description field in the insert code screen
*!*************************************************************
*! Example   :  =lfvsDis()
*!*************************************************************
FUNCTION lfvsDis
LPARAMETERS loInsForm

LOCAL lnAlias, loFormSet, lcCurrKey, llRetVal, lcDesc
lnAlias   = SELECT(0)
loFormSet = loInsForm.loParent
llRetVal  = .T.
lcDesc    = loInsForm.AriaForm1.cntDesc.txtDesc.Value

SELECT (loFormSet.lcTmpCodes)
lcCurrKey = CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM

IF !loFormSet.llIsEditable AND !EMPTY(lcDesc)
  LOCATE FOR UPPER(cDiscrep) = LEFT(UPPER(lcDesc), 30)
  IF FOUND()
    *** This field already exists. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00053B00000","DIALOG")
    llRetVal = .F.
  ENDIF
ENDIF

SELECT (loFormSet.lcTmpCodes)
=SEEK(lcCurrKey, loFormSet.lcTmpCodes, 'CCODE_NO')

*B039820,1 WSH 11/20/2005 Enable the OK button if the user entered value [Start]
loInsForm.AriaForm1.cmdOk.Enabled = llRetVal
*B039820,1 WSH 11/20/2005 [End]

SELECT (lnAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lfvInsOK
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/21/2005
*! Purpose   : Click event for the OK button in Insert Code Screen
*!*************************************************************
*! Example   :  =lfvInsOK()
*!*************************************************************
FUNCTION lfvInsOK
LPARAMETERS loInsForm

LOCAL lnAlias, loFormSet, lcCurrKey, llRetVal, lcCode, lcDesc
lnAlias   = SELECT(0)
loFormSet = loInsForm.loParent
llRetVal  = .T.
lcCode    = loInsForm.AriaForm1.cntCode.txtCode.Value
lcDesc    = loInsForm.AriaForm1.cntDesc.txtDesc.Value

SELECT (loFormSet.lcTmpCodes)
lcCurrKey = CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM

IF loFormSet.llIsEditable 
  IF !EMPTY(lcCode) AND SEEK(lcCode, loFormSet.lcTmpCodes, loFormSet.lcTmpCodes)
    *** This field already exists. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00053B00000","DIALOG")
    llRetVal = .F.
  ENDIF
ELSE
  IF !EMPTY(lcDesc)
    SELECT (loFormSet.lcTmpCodes)
    LOCATE FOR UPPER(cDiscrep) = LEFT(UPPER(lcDesc), 30)
    IF FOUND()
      *** This field already exists. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00053B00000","DIALOG")
      llRetVal = .F.
    ENDIF
  ENDIF
ENDIF

loFormSet.lcInsCode = loInsForm.AriaForm1.cntCode.txtCode.Value
loFormSet.lcInsDesc = loInsForm.AriaForm1.cntDesc.txtDesc.Value

=SEEK(lcCurrKey, loFormSet.lcTmpCodes, 'CCODE_NO')

SELECT (lnAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lfXChng
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/17/2005
*! Purpose   : Exchange two Rows of an array
*!*************************************************************
*! Example   :  =lfxChng(@array,row1,row2)
*!*************************************************************
FUNCTION lfXchng
LPARAMETERS lcArray, lnBeg, lnEnd

LOCAL laTemp(1)

=ACOPY(lcArray, laTemp, 1 + (lnBeg - 1) * ALEN(lcArray, 2), ALEN(lcArray, 2))
=ACOPY(lcArray, lcArray, 1 + (lnEnd - 1) * ALEN(lcArray, 2), ALEN(lcArray, 2), 1 + (lnBeg - 1) * ALEN(lcArray, 2))
=ACOPY(lcTemp, lcArray, 1, ALEN(lcArray, 2), 1 + (lnEnd - 1) * ALEN(lcArray, 2))

RETURN

*!*************************************************************
*! Name      : lfCkhFiles
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 01/24/2005
*! Purpose   : Check Working Files' Data
*!*************************************************************
*! Example   : =lfCkhFiles()
*!*************************************************************
FUNCTION lfCkhFiles
LPARAMETERS loFormSet

SELECT (loFormSet.lcSysComp)
LOCATE
IF EOF()
  *** There is no compnies available.  You have ***
  *** to enter companies first before you enter ***
  *** their codes...
  *** <  Ok  > ***
  =gfModalGen("TRM00223B00000", "DIALOG")
  RETURN .F.
ENDIF

IF !loFormSet.loSydField.SEEK(.T., "VLDENTRY")
  *** There is no fields have valid entries. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00222B00000", "DIALOG")
  RETURN .F.
ENDIF

RETURN .T.

*!*************************************************************
*! Name      : lfChangeCompany
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 01/24/2005
*! Purpose   : Change Connection String according to Selected Company
*!*************************************************************
*! Example   : =lfChangeCompany()
*!*************************************************************
FUNCTION lfChangeCompany
LPARAMETERS loFormSet, lcCompID

LOCAL lnAlias
lnAlias = SELECT(0)

WITH loFormSet
  *--Fill the ActiveCompany Codes Global variable
  .lcActiveComp = lcCompID
  
  *--Create the Native Connection String for the selected Company
  SELECT (.lcSysComp)
  IF SEEK(lcCompID)
    .lcDataDir = oAriaApplication.GetDataDir(ALLTRIM(EVALUATE(.lcSysComp + '.cCom_dDir')))
  ENDIF
  
  .lcNativeConStr = 'Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;' + ;
                    'SourceDB=' + ALLTRIM(.lcDataDir) + ';' + ;
                    'SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;' + ;
                    'Collate=Machine;Null=Yes;Deleted=Yes;'
  
  *--Create the SQL Connection String for the selected Company
  loFormSet.lcSQLConStr = oAriaApplication.mReadConStr(lcCompID)
  
  *--Recreate the Codes Object for the codes file in the data dir for the selected company
  IF TYPE('.loCodes') = 'O'
    .loCodes = .NULL.
  ENDIF
  .loCodes = CREATEOBJECT('RemoteTable', 'CODES', 'CCODE_NO', .lcCodes, SET("Datasession"), lcCompID, .T.)
ENDWITH

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfSQLStatement
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Runs a SQL Server Query Statement
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfSQLStatement()
*!*************************************************************
FUNCTION lfSQLStatement
LPARAMETERS lcSQLStatment, lcCursor, lcIndex, lcTages, llNative, lcConnString, lnBuffering

LOCAL lnConnectionHandlar, lcConnString

*--Get Connection String Type
IF TYPE("lcConnString") # 'C' OR EMPTY(lcConnString)
  IF llNative
    lcConnString = oAriaApplication.cAriaNativeDataFilesConStr
  ELSE
    lcConnString = oAriaApplication.ActiveCompanyConStr
  ENDIF
ENDIF

*--Loop Until Successfully Connect or Cancel by the user
DO WHILE .T.
  *--Run the Satatement
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.SQLRun(lcSqlStatment, lcCursor, '',;
                                  lcConnString, 3, 'SAVE', SET("Datasession"))

  IF lnConnectionHandlar = 1
    *--If Query Successfully executed, Create Indexes if needed for the result cursor
    lnBuffering = IIF(TYPE('lnBuffering') = 'N', lnBuffering, CURSORGETPROP("Buffering", lcCursor))

    =lfCreateIndecies(lcCursor, lcIndex, lcTages)
    RETURN .T.
  ELSE
    *--Query Execution Error
    =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConnectionHandlar, .T.)

*!*      IF MESSAGEBOX(LANG_CONNERRMSG, 5+16, LANG_CONNERRTTL) = 2
*!*        RETURN .F.
*!*      ENDIF
    RETURN .F.
  ENDIF
ENDDO

*!*************************************************************
*! Name      : lfCreateIndecies
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Create Indecies for a cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
FUNCTION lfCreateIndecies
LPARAMETERS lcCursor, lcIndex, lcTages

LOCAL lnOldBuffMode, lcIndex1, lcTages1, lcIndExp

*--If Query Successfully executed, Create Indexes if needed for the result cursor
lnOldBuffMode = CURSORGETPROP("Buffering", lcCursor)
=CURSORSETPROP("Buffering", 3, lcCursor)

lcTages1 = lcTages
lcIndex1 = lcIndex
SELECT (lcCursor)
DO WHILE AT("|", lcIndex1,1) <> 0
  lcIndex  = SUBSTR(lcIndex1, 1, AT("|", lcIndex1, 1) - 1)
  lcIndex1 = STRTRAN(lcIndex1, lcIndex + "|", "", 1, 1)
  lcTages  = SUBSTR(lcTages1, 1, AT("|", lcTages1, 1) - 1)
  lcTages1 = STRTRAN(lcTages1, lcTages + "|", "", 1, 1)
  *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON &lcIndex. TAG (lcTages) OF (lcCursor)
  INDEX ON &lcIndex. TAG (lcTages)
  *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ENDDO
=CURSORSETPROP("Buffering", IIF(TYPE("lnBuffMode") = 'N', lnBuffMode, lnOldBuffMode), lcCursor)

RETURN .T.
*--End of lfCreateIndecies()

*!*************************************************************
*! Name      : lfBuildGrid
*! Developer : Wael M. Abo-Shawareb
*! Date      : 12/01/2004
*! Purpose   : Build Codes Grid Properties
*!*************************************************************
*! Example   : lfBuildGrid()
*!*************************************************************
FUNCTION lfBuildGrid
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

WITH loFormSet.AriaForm1.grdCodes
  SELECT (loFormSet.lcTmpCodes)
  .RecordSource = loFormSet.lcTmpCodes
  .ChildOrder   = loFormSet.lcTmpCodes
  
  .Column1.ControlSource = loFormSet.lcTmpCodes + '.llDefCode'
  .Column1.ReadOnly      = loFormSet.ActiveMode $ 'SV'
  .Column2.ControlSource = loFormSet.lcTmpCodes + '.cCode_No'
  .Column2.ReadOnly      = .T.
  .Column3.ControlSource = loFormSet.lcTmpCodes + '.cDiscrep'
  .Column3.ReadOnly      = loFormSet.ActiveMode $ 'VS'
  
  IF loFormSet.llIsEditable
    .Column2.Visible = .T.
    *!B609593,1 MMT 05/25/2011 cannot search in codes screen grid for specific Color[Start]
    .cSeekIndex = loFormSet.lcTmpCodes
    *!B609593,1 MMT 05/25/2011 cannot search in codes screen grid for specific Color[End]
  ELSE
    .Column2.Visible = .F.
    *!B609593,1 MMT 05/25/2011 cannot search in codes screen grid for specific Color[Start]
    .cSeekIndex = "Discrep"
    *!B609593,1 MMT 05/25/2011 cannot search in codes screen grid for specific Color[END]
  ENDIF
  .Column1.chkDefault.Enabled = loFormSet.ActiveMode $ 'AE'
ENDWITH

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfClearData
*! Developer : Wael M. Abo-Shawareb
*! Date      : 12/01/2004
*! Purpose   : Clear Temp Codes file befor collecting data
*!*************************************************************
*! Example   : lfClearData()
*!*************************************************************
FUNCTION lfClearData
LPARAMETERS loFormSet

SELECT (loFormSet.lcTmpCodes)
=TABLEREVERT(.T.)
=CURSORSETPROP("Buffering", 3)
ZAP
=CURSORSETPROP("Buffering", 5)

SELECT (loFormSet.lcTmpRelFlds)
=TABLEREVERT(.T.)
=CURSORSETPROP("Buffering", 3)
ZAP
=CURSORSETPROP("Buffering", 5)

*!*************************************************************
*! Name      : lfIsNative
*! Developer : Wael M. Abo-Shawareb
*! Date      : 12/01/2004
*! Purpose   : Check if the passed table is Fox or SQL
*!*************************************************************
*! Example   : lfIsNative()
*!*************************************************************
FUNCTION lfIsNative
LPARAMETERS lcTable

LOCAL lnAlias, llNative, lnResult
lnAlias = SELECT(0)

*E303030,1 BEGIN
*!*	lnResult = oAriaApplication.RemoteSystemData.Execute("SELECT * FROM sydIndex where cfile_nam = '" + PADR(lcTable, 8) + "'",;
*!*	                 "", "SysTmpCursor", "", oAriaApplication.cAria4SysFiles, 3, "", SET("Datasession"))
lnResult = oAriaApplication.RemoteSystemData.Execute("SELECT * FROM sydIndex where cfile_nam = '" + PADR(lcTable, oAriaApplication.FileW) + "'",;
                 "", "SysTmpCursor", "", oAriaApplication.cAria4SysFiles, 3, "", SET("Datasession"))
*E303030,1 END
IF lnResult < 1
  oAriaApplication.RemoteSystemData.CheckRetResult("Execute", lnResult, .T.)
  SELECT (lnAlias)
  RETURN 0
ENDIF

SELECT SysTmpCursor
LOCATE
llNative = EOF()

USE IN SysTmpCursor

SELECT (lnAlias)
RETURN IIF(llNative, 1, 2)
*PADR


***************************************************************
*----------     Related Fields' Validations     --------------*
***************************************************************

*!*************************************************************
*! Name      : lfvEOM
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Valid function of EOM (related field to payment terms code)
*!*************************************************************
*! Example   :  =lfvEOM()
*!*************************************************************
FUNCTION lfvEOM

LOCAL lnDayPos, lnEOMPos

*-- If EOM setting is YES enable EOMDAY field, else disable
lnDayPos = ASUBSCRIPT(loOGScroll.laOGFxFlt, ASCAN(loOGScroll.laOGFxFlt, 'EOMDAY', 1, ALEN(loOGScroll.laOGFxFlt), 0, 7), 1)
lnEOMPos = ASUBSCRIPT(loOGScroll.laOGFxFlt, ASCAN(loOGScroll.laOGFxFlt, 'EOM', 1, ALEN(loOGScroll.laOGFxFlt), 0, 7), 1)

loOgScroll.laOGFxFlt[lnDayPos,6] = IIF(loOgScroll.laOGFxFlt[lnEOMPos,6] = 'Y', 21, 0)
laOGObjCnt[lnDayPos] = (loOGScroll.laOGFxFlt[lnEOMPos,6] = 'Y')
= lfOGShowGet(lnDayPos)

RETURN .T.

*!*************************************************************
*! Name      : lfvEOMDay
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Valid function of EOM day (related field to payment terms code)
*!*************************************************************
*! Example   : =lfvEOMDay()
*!*************************************************************
FUNCTION lfvEOMDay

LOCAL lnDayPos, lnEOMPos

*-- If EOM setting is YES enable EOMDAY field, else disable
lnDayPos = ASUBSCRIPT(loOGScroll.laOGFxFlt, ASCAN(loOGScroll.laOGFxFlt, 'EOMDAY', 1, ALEN(loOGScroll.laOGFxFlt), 0, 7), 1)
lnEOMPos = ASUBSCRIPT(loOGScroll.laOGFxFlt, ASCAN(loOGScroll.laOGFxFlt, 'EOM', 1, ALEN(loOGScroll.laOGFxFlt), 0, 7), 1)

IF loOGScroll.laOGFxFlt[lnEOMPos,6] = 'Y'
  *B609902,1 MMT 05/02/2012 change codes screen to accept 0 EOM days[T20120301.0038][Start]
  *IF loOGScroll.laOGFxFlt[lnDayPos,6] > 31 .OR. loOGScroll.laOGFxFlt[lnDayPos,6] <= 0  
  IF loOGScroll.laOGFxFlt[lnDayPos,6] > 31 .OR. loOGScroll.laOGFxFlt[lnDayPos,6] < 0
  *B609902,1 MMT 05/02/2012 change codes screen to accept 0 EOM days[T20120301.0038][END]
    *-- Message End Of Month day should be between 1 & 31
    =gfModalGen('INM00351B00000','DIALOG', LANG_SMCODES_ENDMONTH)
    RETURN .F.
  ENDIF
ELSE
  loOgScroll.laOGFxFlt[lnDayPos,6] = 0
  laOGObjCnt[lnDayPos] = .F.
  = lfOGShowGet(lnDayPos)
ENDIF

RETURN .T.

*!*************************************************************
*! Name      : lfvAdjAcct
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Function to validate cAdjAcct from the related 
*!           : field screen that branches from the codes screen.
*!*************************************************************
*! Example   : lfvAdjAcct()
*!*************************************************************
FUNCTION lfvAdjAcct

LOCAL lcCompToUse, lcPthToUse, lcLinkChar, loGLACCHAR, llGlLink, lcToGet
LOCAL llRetVal, lcCurVar, lcCurVal, lcFile_Ttl, lcTempVar, lcLinkWith, lcControl
LOCAL lcLinkComp, lcSBTGLDir, lcAcntChrt, lcAcntBrwF, lcAcntFld, lcAcntDesF
PRIVATE lcBrFields, laTemp

*-- Save the current alias.
lnAlias    = SELECT(0)
lcControl  = loOGScroll.FocusControl
lcCurVar   = loOGScroll.&lcControl.

*WSH
*lcCurVal   = lcCurVar.Value
lcCurVal   = PADR(lcCurVar.Value, 24)
*WSH

llGlLink   = (ALLTRIM(gfGetMemVar('M_LINK_GL', loCodesFormName.lcActiveComp)) = 'Y') && if there is GL Link or not in the popup active company.
lcLinkWith = (ALLTRIM(gfGetMemVar('M_GL_VERS', loCodesFormName.lcActiveComp)))       && link GL WITH ?? (Aria - SBT - Other)
lcLinkChar = gfTempName()
llRetVal   = .T.

*!*  IF TYPE("_Screen.ActiveForm.ActiveControl.OldValue") = "U" OR ALLTRIM(lcCurVar.OldValue) == ALLTRIM(lcCurVal)
*!*    RETURN .T.
*!*  ENDIF

*B608452,1 AKA 25/02/2007  Allow empty GL accoutn code in case of Manfucurting Operation (Star)
IF loCodesFormName.ARIAform1.CBoVldFlds.Value = 'MFGCODE' AND EMPTY(lcCurVal)
  RETURN .T.
ENDIF 
*B608452,1 AKA 25/02/2007  Allow empty GL accoutn code in case of Manufacturing Operation (Star)

*-- Validate only if there is GL Link.
*B608126,1 MMT 06/17/2007 fix bug of not updating the Gl Account Fileds [Start]
*IF llGlLink AND !EMPTY(lcCurVal)
IF llGlLink 
*B608126,1 MMT 06/14/2007 fix bug of not updating the Gl Account Fileds [End]

  *-- If link with Aria or Other.
  IF lcLinkWith $ "AO"
    LOCAL loSyCCOMP, lcSYCCOMP
    lcSYCCOMP = gfTempName()

    *-- Open the company file to get the path of the popup active company.
    loSyCCOMP   = CREATEOBJECT("RemoteTable", "SYCCOMP", "CCOMP_ID", lcSYCCOMP, SET("Datasession"))

    =loSyCCOMP.SEEK(loCodesFormName.lcActiveComp)
    lcCompToUse = loCodesFormName.lcActiveComp

    *-- If the popup active company has a parent company.
    IF !EMPTY(EVALUATE(lcSYCCOMP + '.cCompPrnt'))
      lcCompToUse = EVALUATE(lcSYCCOMP + '.cCompPrnt')
    ENDIF
    loSyCCOMP  = .NULL.
    
    loGLACCHAR = CREATEOBJECT("RemoteTable", "GLACCHAR", "ACCTCODE", lcLinkChar, SET("Datasession"), lcCompToUse, .T.)
    lcAcntBrwF = "cAcctCode:H='" + LANG_SMCODES_ACCCODE + "',cAccNlDes:H='" + LANG_SMCODES_DESC + "'"
    lcAcntFld  = "cAcctCode"
    lcAcntDesF = "cAccNlDes"

    *-- If the company not linked to Other.
    IF lcLinkWith <> "O" AND !loGLACCHAR.SEEK(lcCurVal, .F., .T.)
      SELECT (lcLinkChar)
      DIMENSION laTemp[2]
      
      lcCurVal = RTRIM(lcCurVal)
      IF RIGHT(lcCurVal, 1) == '?'
        lcCurVal = SUBSTR(lcCurVal, 1, LEN(lcCurVal) - 1)
        = loGLACCHAR.SEEK(lcCurVal, .F., .T.)
      ENDIF
      
      laTemp     = ''
      lcBrFields = lcAcntBrwF
      lcFile_Ttl = LANG_SMCODES_ACCCHART
      lcToGet    = lcAcntFld + "," + lcAcntDesF
      IF gfBrows(.F., lcToGet, 'laTemp', lcFile_Ttl, .F., .F., .F.,;
                 "GLACCHAR", IIF(loGLACCHAR.llNative, oAriaApplication.cNativeDBID, oAriaApplication.cSQLDBID),;
                 "ACCTCODE", "ACCTCODE")
        llRetVal = .T.
        lcCurVar.Value = laTemp[1]
        IF TYPE('loOgScroll') = 'O'
          loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = laTemp[1]
          = lfOGShowGet(lcCurVar.Parent.nRowIndex)
        ENDIF
      ELSE
        llRetVal = .F.
      ENDIF
    ENDIF  
    loGLACCHAR = .NULL.
  ELSE
    *-- Get the SBT company linked to the popup active comany.
    lcLinkComp = ALLTRIM(gfGetMemVar('M_GL_CO'  , lcActComp))

    *-- Get the directory of the SBT company linked to the popup active company.
    lcSBTGLDir = ALLTRIM(gfGetMemVar('M_SYS_DIR', lcActComp))

    *-- Var. hold the chart of account name & path.
    lcAcntChrt = lcSBTGLDir + "\GLDATA\GLACNT" + lcLinkComp + ".DBF"

    *-- Open the chart of accounts file.
    IF !USED("lcLinkChar")
      USE (lcAcntChrt) IN 0 AGAIN ALIAS (lcLinkChar) ORDER GlAcnt
    ENDIF

    lcAcntBrwF = "glAcnt:24:H='" + LANG_SMCODES_ACCCODE + "',glDesc:53:H='" + LANG_SMCODES_DESC + "'"
    lcAcntFld  = "glAcnt"
    lcAcntDesF = "glDesc"

    *-- If the value enetered does not exist in the chart 
    *-- of accounts file, call the browse.
    IF !SEEK(lcCurVal, lcLinkChar)
      SELECT (lcLinkChar)
      DIMENSION laTemp[2]
      laTemp     = SPACE(0)
      lcBrFields = lcAcntBrwF
      lcFile_Ttl = LANG_SMCODES_ACCCHART
      lcToGet    = lcAcntFld + "," + lcAcntDesF
      
      IF RIGHT(RTRIM(lcCurVal), 1) == '?'  
        lcCurVal = SUBSTR(lcCurVal, 1, LEN(ALLTRIM(lcCurVal)) - 1)
        = SEEK(lcCurVal)
      ENDIF
      
      IF gfBrows(.F., lcToGet, 'laTemp', lcFile_Ttl)
        llRetVal = .T.
        lcCurVar.Value = laTemp[1]
        IF TYPE('loOgScroll') = 'O'
          loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = laTemp[1]
          = lfOGShowGet(lcCurVar.Parent.nRowIndex)
        ENDIF
      ELSE
        llRetVal = .F.
      ENDIF
    ENDIF  
    USE IN (lcLinkChar)
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lfvlnkCod
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Valid function of link code related field
*!*************************************************************
*! Example   :  =lfvlnkCod()
*!*************************************************************
FUNCTION lfvlnkCod

LOCAL lnAlias, lcCurVar, lcCurVal, loGL_LINK, lcGL_LINK, llRetVal, lcControl
lnAlias   = SELECT(0)
lcGLODIR  = ''
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.

*WSH
*lcCurVal  = lcCurVar.Value
lcCurVal  = PADR(lcCurVar.Value, 6)
*WSH

lcGL_LINK = gfTempName()
llRetVal  = .T.

*!*  IF TYPE("_Screen.ActiveForm.ActiveControl.OldValue") = "U" OR ALLTRIM(lcCurVar.OldValue) == ALLTRIM(lcCurVal)
*!*    RETURN .T.
*!*  ENDIF

IF !EMPTY(lcCurVal) .AND. gfGetMemVar("M_LINK_GL", lcComp) = "Y" .AND. gfGetMemVar("M_DIV_LINK", lcComp) = 'Y'
  loGL_LINK = CREATEOBJECT("RemoteTable", "GL_LINK", "GL_LINK", lcGL_LINK, SET("Datasession"), loCodesFormName.lcActiveComp)

  IF !loGL_LINK.SEEK(lcCurVal)
    =gfGlBrowse('01', @lcCurVal, '', 1, .T.)
    IF !EMPTY(lcCurVal)
      lcCurVar.Value = lcCurVal
      IF TYPE('loOgScroll') = 'O'
        loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = lcCurVal
        = lfOGShowGet(lcCurVar.Parent.nRowIndex)
      ENDIF
    ELSE
      llRetVal = .F.
    ENDIF
  ENDIF
  loGL_LINK = .NULL.
ENDIF

SELECT (lnAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lfvlnkSls
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Valid function of sales link code related field
*!*************************************************************
*! Example   :  =lfvlnkSls()
*!*************************************************************
FUNCTION lfvlnkSls

LOCAL lnAlias, lcCurVar, lcCurVal, loGL_LINK, lcGL_LINK, llRetVal, lcControl
lnAlias   = SELECT(0)
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.

*WSH
*lcCurVal  = lcCurVar.Value
lcCurVal  = PADR(lcCurVar.Value, 6)
*WSH

lcGL_LINK = gfTempName()
llRetVal  = .T.

*!*  IF TYPE("_Screen.ActiveForm.ActiveControl.OldValue") = "U" OR ALLTRIM(lcCurVar.OldValue) == ALLTRIM(lcCurVal)
*!*    RETURN .T.
*!*  ENDIF

IF !EMPTY(lcCurVal) .AND. gfGetMemVar("M_LINK_GL", lcComp) = "Y" .AND. gfGetMemVar("M_DIV_LINK", lcComp) = 'Y'
  loGL_LINK = CREATEOBJECT("RemoteTable", "GL_LINK", "GL_LINK1", lcGL_LINK, SET("Datasession"), loCodesFormName.lcActiveComp)

  IF !loGL_LINK.SEEK('02'+lcCurVal)
    =gfGLBrowse('02', @lcCurVal, '', 1, .T.)
    IF !EMPTY(lcCurVal)
      lcCurVar.Value = lcCurVal
      IF TYPE('loOgScroll') = 'O'
        loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = lcCurVal
        = lfOGShowGet(lcCurVar.Parent.nRowIndex)
      ENDIF
    ELSE
      llRetVal = .F.
    ENDIF
  ENDIF
  loGL_LINK = .NULL.
ENDIF

SELECT (lnAlias)
RETURN llRetVal

*!**************************************************************************
*! Name      : lfvMFGOpr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Valid function of the MFG operation code, 
*!             (Contractor Code, lead time, In House ) related fields
*!**************************************************************************
*! Example   :  =lfvMFGOpr()
*!**************************************************************************
FUNCTION lfvMFGOpr

LOCAL lnAlias, llRetVal, lcControl, lcCurVar, lcCurVal, lnPos
lnAlias   = SELECT(0)
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lnPos     = lcCurVar.Parent.nRowIndex
lcFldCod  = loOGScroll.laOGFxFlt[lnPos,1]

*WSH
*lcCurVal  = lcCurVar.Value
lcCurVal  = PADR(lcCurVar.Value, 8)
*WSH
llRetVal  = .T.

*!*  IF TYPE("_Screen.ActiveForm.ActiveControl.OldValue") = "U" OR ALLTRIM(lcCurVal) == ALLTRIM(lcCurVar.OldValue)
*!*    RETURN .T.
*!*  ENDIF

DO CASE
  CASE lcFldCod = 'CCONTCODE'
    LOCAL lnNamePos, lnInHPos
    lnNamePos = ASCAN(loOGScroll.laOGFxFlt, 'CCONTNAME', 1, ALEN(loOGScroll.laOGFxFlt), 0, 14)
    lnInHPos  = ASCAN(loOGScroll.laOGFxFlt, 'LINHOUSE', 1, ALEN(loOGScroll.laOGFxFlt), 0, 14)
    
    *WSH [Start]
    *IF !loOGScroll.laOGFxFlt[lnInHPos,1]
    lcCurVal  = PADR(lcCurVar.Value, 8)
    
    IF !loOGScroll.laOGFxFlt[lnInHPos,6]
    *WSH [End]
    
      LOCAL loVendor, lcVendor
      lcVendor = gfTempName()
      loVendor = CREATEOBJECT("RemoteTable", "APVENDOR", "VenCode", lcVendor, SET("Datasession"), loCodesFormName.lcActiveComp, .T.)
      
      *-- Check if we are going to Browse
      
      *B130868,1 WSH [Start]
      *IF !EMPTY(lcCurVal)
      *B130868,1 WSH [End]
      
      *B607866,1 MMT 12/06/2006 Error when press OK in op grid with change and browse appears[Start]
      IF !EMPTY(lcCurVal)
      *B607866,1 MMT 12/06/2006 Error when press OK in op grid with change and browse appears[End]
      
        IF !('?' $ lcCurVal) .AND. loVendor.SEEK(lcCurVal)
          loOGScroll.laOGFxFlt[lnPos,6] = EVALUATE(lcVendor + '.cVendCode')
          = lfOGShowGet(Pos)
          loOGScroll.laOGFxFlt[lnNamePos,6] = EVALUATE(lcVendor + '.cVenComp')
          = lfOGShowGet(lnNamePos)
        ELSE
          
          IF loVendor.llNative
            SELECT (lcVendor)
            LOCATE FOR "C" $ cVenSupTyp
            
            llFoundCont = FOUND()
          ELSE
            llFoundCont = loVendor.SQLRun("SELECT TOP 1 * FROM APVendor WHERE CHARINDEX('C', cVenSupTyp) <> 0")
          ENDIF
          
          IF llFoundCont
            *-- Filter vendor browse according to Supp. Type = 'C' --> Contractor
            =gfApVnBrow(@lcCurVal, .F., 'C')
            IF !EMPTY(lcCurVal)
              =loVendor.SEEK(lcCurVal)
              loOGScroll.laOGFxFlt[lnPos,6] = lcCurVal      && Update the field
              = lfOGShowGet(lnPos)
              loOGScroll.laOGFxFlt[lnNamePos,6] = EVALUATE(lcVendor + '.cVenComp')
              = lfOGShowGet(lnNamePos)
            
            *B130868,1 WSH [Start]
            ELSE
              llRetVal = .F.
            ENDIF
            *B130868,1 WSH [End]
            
          ELSE
            loOGScroll.laOGFxFlt[lnPos,6] = ""
            = lfOGShowGet(lnPos)
            loOGScroll.laOGFxFlt[lnNamePos,6] = ""
            = lfOGShowGet(lnNamePos)
          ENDIF
        ENDIF
        laOGObjCnt[lnNamePos] = .F.
      
      *B130868,1 WSH [Start]
      *ELSE
      *  llRetVal = .F.
      *ENDIF
      *B130868,1 WSH [End]
      
      *B607866,1 MMT 12/06/2006 Error when press OK in op grid with change and browse appears[Start]
      ENDIF 
      *B607866,1 MMT 12/06/2006 Error when press OK in op grid with change and browse appears[END]
      
      loVendor = .NULL.
    ELSE
      *B607866,1 MMT 12/06/2006 Error when press OK in op grid with change and browse appears[Start]
      loOGScroll.laOGFxFlt[lnPos,6] = lcCurVar.Value
      = lfOGShowGet(Pos)
      IF AT('O_LAOGFXFLT_'+ALLTRIM(STR(lnPos)),lcControl) <> 0
        lcNextControl =lcControl
        lcNextControl='loOGScroll.'+STRTRAN(lcNextControl,'O_LAOGFXFLT_'+ALLTRIM(STR(lnPos)),'O_LAOGFXFLT_'+ALLTRIM(STR(lnNamePos)))
        loOGScroll.laOGFxFlt[lnNamePos,6] = &lcNextControl..Value 
       ENDIF   
       = lfOGShowGet(lnNamePos)
      *B607866,1 MMT 12/06/2006 Error when press OK in op grid with change and browse appears[End]

      laOGObjCnt[lnNamePos] = .T.
    ENDIF
    = lfOGShowGet(lnNamePos)
    
  CASE lcFldCod = 'LEADTIME'
    
    *WSH [Start]
    lcCurVal  = lcCurVar.Value
    *WSH [End]
    
    *-- Lead time must be greater than zero
    IF lcCurVal <= 0
      =gfModalGen('INM00234B00000','DIALOG',LANG_SMCODES_LEADTIME)
      *llRetVal = .F.
    ENDIF
    
  CASE lcFldCod = 'LINHOUSE'
    
    *WSH [Start]
    lcCurVal  = lcCurVar.Value
    *WSH [End]
    
    LOCAL lnCodePos, lnNamePos
    lnCodePos = ASCAN(loOGScroll.laOGFxFlt, 'CCONTCODE', 1, ALEN(loOGScroll.laOGFxFlt), 0, 14)
    lnNamePos = ASCAN(loOGScroll.laOGFxFlt, 'CCONTNAME', 1, ALEN(loOGScroll.laOGFxFlt), 0, 14)
    
    loOGScroll.laOGFxFlt[lnCodePos,6] = SPACE(8)
    = lfOGShowGet(lnCodePos)
    
    loOGScroll.laOGFxFlt[lnNamePos,6] = SPACE(30)
    laOGObjCnt[lnNamePos] = loOGScroll.laOGFxFlt[lnPos,6]
    = lfOGShowGet(lnNamePos)
ENDCASE

SELECT (lnAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lfvDivGrp
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Valid function of division group related field
*!*************************************************************
*! Example   :  =lfvDivGrp()
*!*************************************************************
FUNCTION lfvDivGrp
LPARAMETER lcVar , lcReturn , lcComp , lcOldRltd

LOCAL lnAlias, llRetVal, lcControl, lcCurVar, lcCurVal, lnPos, lcDivGrp, llRetVal
lnAlias   = SELECT(0)
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lnPos     = lcCurVar.Parent.nRowIndex
lcDivGrp  = gfTempName()
lcCurVal  = lcCurVar.Value
llRetVal  = .T.

*!*  IF TYPE("_Screen.ActiveForm.ActiveControl.OldValue") = "U" OR ALLTRIM(lcCurVal) == ALLTRIM(lcCurVar.OldValue)
*!*    RETURN .T.
*!*  ENDIF

IF !EMPTY(lcCurVal) .AND. gfGetMemVar("M_DIV_SEQ", loCodesFormName.lcActiveComp) = 'Y'
  *-- Collect all division group data from sequence file.
  LOCAL lcStat
  
  *WSH [Start]
  *lcStat = "SELECT DISTINCT cseq_group" +;
           " FROM  SEQUENCE Seq_file"   +;
           " WHERE !EMPTY(cseq_group)"  +;
           " ORDER BY cseq_group"
  *IF !lfSQLStatement(lcStat, lcDivGrp, 'cseq_group|', 'cseq_group|', .T., lcCodesFormName.lcNativeConStr)
  LOCAL llNative
  llNative = lfIsNative('SEQUENCE') = 1
  
  IF llNative
    lcStat = "SELECT DISTINCT cseq_group" +;
             " FROM  SEQUENCE Seq_file"   +;
             " WHERE !EMPTY(cseq_group)"  +;
             " ORDER BY cseq_group"
  ELSE
    lcStat = "SELECT DISTINCT cseq_group" +;
             " FROM  SEQUENCE Seq_file"   +;
             " WHERE cseq_group <> ''"  +;
             " ORDER BY cseq_group"
  ENDIF
  IF !lfSQLStatement(lcStat, lcDivGrp, 'cseq_group|', 'cseq_group|', .T., IIF(llNative, loCodesFormName.lcNativeConStr, loCodesFormName.lcSQLConStr))
  *WSH [End]
  
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
  
  SELECT (lcDivGrp)
  =SEEK(lcCurVal)
  IF !FOUND()
    lcBrFields = "cseq_group :H='" + LANG_SMCODES_GROUP + "'"
    
    DIMENSION laTemp[1]
    laTemp = ''
    
    =gfBrows(.F., "cseq_group", 'laTemp', LANG_SMCODES_GROUP)
    loOGScroll.laOGFxFlt[lnPos,6] = laTemp[1]      && Update the field
    = lfOGShowGet(lnPos)
  ENDIF
  
  USE IN (lcDivGrp)
ENDIF

SELECT (lnAlias)
RETURN llRetVal

*!**************************************************************************
*! Function  : lfvSiteTyp
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Validation for site type (No more than one Back Office).
*!**************************************************************************
FUNCTION lfvSiteTyp

LOCAL llRetVal, lnAlias, lcVar, lcReturn, lcComp, lcOldRltd, lcCurVar, lcControl
lnAlias    = SELECT(0)
llRetVal   = .T.
lcControl  = loOGScroll.FocusControl
lcCurVar   = loOGScroll.&lcControl.

*!*  IF TYPE("_Screen.ActiveForm.ActiveControl.OldValue") = "U" OR ALLTRIM(lcCurVar.Value) == ALLTRIM(lcCurVar.OldValue)
*!*    RETURN .T.
*!*  ENDIF

*-- If Back office was selected.
IF "NC" $ oAriaApplication.CompanyInstalledModules .AND. EVALUATE(OGSYS18()) = 'B'
  LOCAL llFound, lnDataSess, lcSiteID
  llFound    = .F.
  lnDataSess = SET("Datasession")

  SET DATASESSION TO (loCodesFormName.loCodes.lnDataSession)

  IF loCodesFormName.loCodes.SEEK('NYCSITEID', 'IDRLTFNAME')
    SELECT (loCodesFormName.loCodes.lcCurSorView)
    LOCATE REST WHILE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NYCSITEID' ;
                FOR cRltd_Nam = 'CCMSITETYP' AND LEFT(cRltd_Vlu,1) = 'B'
    IF FOUND()
      llFound  = .T.
      lcSiteID = cCode_No
    ENDIF
  ENDIF
  
  SET DATASESSION TO (lnDataSess)

  *-- If there is another back office site.
  IF llFound
    =gfModalGen("TRM000000B00000","DIALOG",'','',LANG_SMCODES_SITE + ALLTRIM(lcSiteID) + LANG_SMCODES_ONEBACK)
    llRetVal = .F.
  ENDIF    
ENDIF

SELECT (lnAlias)
RETURN llRetVal

*!**************************************************************************
*! Name      : lfvRelBnk
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Valid function of the Bank Code
*!**************************************************************************
*! Example   :  =lfvRelBnk()
*!**************************************************************************
FUNCTION lfvRelBnk

LOCAL llRetVal, lnAlias, lcCurVar, lcControl, lnPos
lnAlias   = SELECT(0)
llRetVal  = .T.
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lnPos     = ASCAN(loOGScroll.laOGFxFlt, 'CCHKACCT', 1, ALEN(loOGScroll.laOGFxFlt), 0, 14)

*!*  IF TYPE("_Screen.ActiveForm.ActiveControl.OldValue") = "U" OR ALLTRIM(lcCurVar.Value) == ALLTRIM(lcCurVar.OldValue)
*!*    RETURN .T.
*!*  ENDIF

LOCAL loAPBANKS, lcAPBANKS, lcOldValue

*WSH
*lcOldValue = EVALUATE(OGSYS18())
lcOldValue = PADR(EVALUATE(OGSYS18()), 8)
*WSH

lcAPBANKS  = gfTempName()
loAPBANKS  = CREATEOBJECT("RemoteTable", "APBANKS", "BANKCODE", lcAPBANKS, SET("Datasession"), loCodesFormName.lcActiveComp, .T.)

PRIVATE lcBrFields
DIMENSION laTemp[1]

IF TYPE("loAPBANKS") = 'O' 
  IF lnPos = 0
    
    *WSH
    *IF !loAPBANKS.SEEK(EVALUATE(OGSYS18())) OR EVALUATE(lcAPBANKS + '.cBnkType') <> 'S'
    IF !loAPBANKS.SEEK(PADR(EVALUATE(OGSYS18()), 8)) OR EVALUATE(lcAPBANKS + '.cBnkType') <> 'S'
    *WSH
    
      lcBrFields = "cBnkCode :H='" + LANG_SMCODES_BANKCODE + "',cBnklndes :H='" + LANG_SMCODES_LONGDESC + "'"
      
      SELECT (lcAPBANKS)
      =gfBrows("FOR cBnkType = 'S'", "cBnkCode", "laTemp", LANG_SMCODES_BANKS, .F., .F., .F.,;
               "APBANKS", IIF(loAPBANKS.llNative, oAriaApplication.cNativeDBID, oAriaApplication.cSQLDBID),;
               "BANKCODE", "BANKCODE")
      
      IF !EMPTY(laTemp[1])
        loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = laTemp[1]
        = lfOGShowGet(lcCurVar.Parent.nRowIndex)
      ELSE
        llRetVal = .F.
      ENDIF
    ENDIF
  ELSE
    
    *WSH
    *IF !EMPTY(EVALUATE(OGSYS18())) AND (!loAPBANKS.SEEK(EVALUATE(OGSYS18())) OR EVALUATE(lcAPBANKS + '.cBnkType') <> 'B')
    IF !EMPTY(EVALUATE(OGSYS18())) AND (!loAPBANKS.SEEK(PADR(EVALUATE(OGSYS18()),8)) OR EVALUATE(lcAPBANKS + '.cBnkType') <> 'B')
    *WSH
    
      lcBrFields = "cBnkCode :H='" + LANG_SMCODES_BANKCODE + "',cBnklndes :H='" + LANG_SMCODES_LONGDESC + "'"
      
      SELECT (lcAPBANKS)
      =gfBrows("FOR cBnkType = 'B'", "cBnkCode", "laTemp", LANG_SMCODES_BANKS, .F., .F., .F.,;
               "APBANKS", IIF(loAPBANKS.llNative, oAriaApplication.cNativeDBID, oAriaApplication.cSQLDBID),;
               "BANKCODE", "BANKCODE")
      IF !EMPTY(laTemp[1])
        loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = laTemp[1]
        = lfOGShowGet(lcCurVar.Parent.nRowIndex)
        
        *-- Initialize the checking account...
        loOgScroll.laOGFxFlt[lnPos,6] = SPACE(12)
        = lfOGShowGet(lnPos)
      ELSE
        llRetVal = .F.
      ENDIF
    ENDIF
    
    IF EMPTY(loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6])
      *-- Initialize the checking account...
      loOgScroll.laOGFxFlt[lnPos,6] = SPACE(12)
      = lfOGShowGet(lnPos)
    ENDIF
  ENDIF
ENDIF

loAPBANKS = .NULL.

SELECT (lnAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lfvfnRylR
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Valid function of tax rate
*!*************************************************************
*! Example   :  =lfvfnRylR()
*!*************************************************************
FUNCTION lfvfnRylR

LOCAL llRetVal, lcControl, lcCurVar
llRetVal  = .T.
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.

*!*  IF TYPE("_Screen.ActiveForm.ActiveControl.OldValue") = "U" OR ALLTRIM(lcCurVar.Value) == ALLTRIM(lcCurVar.OldValue)
*!*    RETURN .T.
*!*  ENDIF

IF EVALUATE(OGSYS18()) < 0
  =gfModalGen('INM00234B00000', 'DIALOG', LANG_SMCODES_ROYALRAT)
  llRetVal = .F.
ENDIF

RETURN llRetVal

*!*************************************************************
*! Name      : lfvfnTaxR
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Valid function of tax rate
*!*************************************************************
*! Example   :  =lfvfnTaxR()
*!*************************************************************
FUNCTION lfvfnTaxR

LOCAL llRetVal, lcControl, lcCurVar
llRetVal  = .T.
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.

*!*  IF TYPE("_Screen.ActiveForm.ActiveControl.OldValue") = "U" OR ALLTRIM(lcCurVar.Value) == ALLTRIM(lcCurVar.OldValue)
*!*    RETURN .T.
*!*  ENDIF

IF EVALUATE(OGSYS18()) < 0
  =gfModalGen('INM00234B00000','DIALOG',LANG_SMCODES_TAXRATE)
  llRetVal = .F.
ENDIF

RETURN llRetVal

*!**************************************************************************
*! Name      : lfvRelchk
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Valid function of the Bank Checking account
*!**************************************************************************
*! Example   :  =lfvRelchk()
*!**************************************************************************
FUNCTION lfvRelchk

STORE SPACE(0) TO lcOldBnk,lcOldchk

LOCAL llRetVal, lcControl, lcCurVar, lnAlias, loAPCHECKS, lcAPCHECKS, lnPos, lnPos1
lnAlias    = SELECT(0)
llRetVal   = .T.
lcControl  = loOGScroll.FocusControl
lcCurVar   = loOGScroll.&lcControl.
lcAPCHECKS = gfTempName()
lnPos      = ASCAN(loOGScroll.laOGFxFlt, 'CBNKCODE', 1, ALEN(loOGScroll.laOGFxFlt), 0, 14)
lnPos1     = ASCAN(loOGScroll.laOGFxFlt, 'CADJACCT', 1, ALEN(loOGScroll.laOGFxFlt), 0, 14)

*!*  IF TYPE("_Screen.ActiveForm.ActiveControl.OldValue") = "U" OR ALLTRIM(lcCurVar.Value) == ALLTRIM(lcCurVar.OldValue)
*!*    RETURN .T.
*!*  ENDIF

loAPCHECKS = CREATEOBJECT("RemoteTable", "APCHECKS", "BANKCHECK", lcAPCHECKS, SET("Datasession"), loCodesFormName.lcActiveComp, .T.)

DIMENSION laTemp[1]
PRIVATE lcBrFields
lcBrFields = "cBnkCode :H='" + LANG_SMCODES_BANKCODE + "',cChkAcct :H='" + LANG_SMCODES_BNKCHKAC + "'," +;
             "cChkShDes :H='" + LANG_SMCODES_SHORTDESC + "', cChkGlAcc :H='" + LANG_SMCODES_GLCHKACC + "'"

IF TYPE("loAPCHECKS") = 'O'
  IF EMPTY(loOGScroll.laOGFxFlt[lnPos,6])
    IF !EMPTY(lcCurVar.Value)
      SELECT (lcAPCHECKS)
      =gfBrows(.F., "cChkAcct,CBnkCode,cChkGlAcc", "laTemp", LANG_SMCODES_CHECK, .F., .F., .F.,;
               "APCHECKS", IIF(loAPCHECKS.llNative, oAriaApplication.cNativeDBID, oAriaApplication.cSQLDBID),;
               "BANKCHECK", "BANKCHECK")
      
      IF !EMPTY(laTemp[1])
        loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = laTemp[1]
        = lfOGShowGet(lcCurVar.Parent.nRowIndex)
        
        loOgScroll.laOGFxFlt[lnPos,6] = laTemp[2]
        = lfOGShowGet(lnPos)
        
        loOgScroll.laOGFxFlt[lnPos1,6] = laTemp[3]
        = lfOGShowGet(lnPos1)
      ELSE
        llRetVal = .F.
      ENDIF
    ENDIF
  ELSE
    
    *WSH
    *IF !loAPCHECKS.SEEK(PADR(loOGScroll.laOGFxFlt[lnPos,6],8) + lcCurVar.Value)
    IF !loAPCHECKS.SEEK(PADR(loOGScroll.laOGFxFlt[lnPos,6],8) + PADR(lcCurVar.Value, 12))
    *WSH
    
      SELECT (lcAPCHECKS)
      =gfBrows("FOR cBnkCode = " + "'" + loOGScroll.laOGFxFlt[lnPos,6] + "'", "cChkAcct,cChkGlAcc", "laTemp", LANG_SMCODES_CHECK, .F., .F., .F.,;
               "APCHECKS", IIF(loAPCHECKS.llNative, oAriaApplication.cNativeDBID, oAriaApplication.cSQLDBID),;
               "BANKCHECK", "BANKCHECK")
      
      IF !EMPTY(laTemp[1])
        loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = laTemp[1]
        = lfOGShowGet(lcCurVar.Parent.nRowIndex)
        
        loOgScroll.laOGFxFlt[lnPos1,6] = laTemp[2]
        = lfOGShowGet(lnPos1)
      ELSE
        llRetVal = .F.
      ENDIF
    ENDIF
  ENDIF
ENDIF

laOGObjCnt[lnPos1] = EMPTY(lcCurVar.Value)
= lfOGShowGet(lnPos1)

STORE .NULL. TO loAPCHECKS

SELECT (lnAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lfv1099C                              
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/03/2005
*! Purpose   : Valid function for the field [c1099code] 
*!*************************************************************
*! Example            :  =lfv1099C()
*!*************************************************************
FUNCTION lfv1099C

LOCAL llRetVal, lcControl, lcCurVar
llRetVal  = .T.
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.

*!*  IF TYPE("_Screen.ActiveForm.ActiveControl.OldValue") = "U" OR ALLTRIM(lcCurVar.Value) == ALLTRIM(lcCurVar.OldValue)
*!*    RETURN .T.
*!*  ENDIF

*-- IF Statment to check if the field was left empty and ther is old value
IF EMPTY(lcCurVar.Value) .AND. !EMPTY(lcCurVar.OldValue)
  ** MESSAGE : " You have to enter the �              "
  **           "                  � Ok �              "
  **
  =gfModalGen("TRM04066B00000" , "DIALOG" , LANG_SMCODES_1099VAL)
  llRetVal = .F.
ELSE       && Else
  *-- IF Statment to check if the field value is between 1 and 12
  IF BETWEEN(lcCurVar.Value, 1, 11)
    loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = PADL(ALLTRIM(STR(VAL(lcCurVar.Value))), 2, '0')
    = lfOGShowGet(lcCurVar.Parent.nRowIndex)
  ELSE      && Else
    ** MESSAGE : " You have to enter the �              "
    **           "                  � Ok �              "
    **
    =gfModalGen("TRM04066B00000" , "DIALOG" , LANG_SMCODES_1099VAL)
    llRetVal = .F.
  ENDIF     && End of IF
ENDIF     && End of IF

RETURN llRetVal

*!**************************************************************************
*! Name      : lfvVenCode
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 09/01/2006
*! Purpose   : Valid function of the vendor code as Royalty related field E302303
*!**************************************************************************
*! Example   :  =lfvVenCode()
*!**************************************************************************
FUNCTION lfvVenCode

LOCAL lnAlias, llRetVal, lcControl, lcCurVar, lcCurVal, lnPos
lnAlias   = SELECT(0)
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lnPos     = lcCurVar.Parent.nRowIndex
lcCurVal  = PADR(lcCurVar.Value, 8)
llRetVal  = .T.
LOCAL loVendor, lcVendor
lcVendor = gfTempName()
loVendor = CREATEOBJECT("RemoteTable", "APVENDOR", "VenCode", lcVendor, SET("Datasession"), loCodesFormName.lcActiveComp, .T.)
IF !EMPTY(lcCurVal) AND ( !loVendor.SEEK(lcCurVal) OR !("C" $ EVALUATE(lcVendor + '.cVenSupTyp')) )
  IF loVendor.llNative
    SELECT (lcVendor)
    LOCATE FOR "C" $ cVenSupTyp
    llFoundCont = FOUND()
  ELSE
    llFoundCont = loVendor.SQLRun("SELECT TOP 1 * FROM APVendor WHERE CHARINDEX('C', cVenSupTyp) <> 0")
  ENDIF
  IF llFoundCont
    *-- Filter vendor browse according to Supp. Type = 'C' --> Contractor
    =gfApVnBrow(@lcCurVal, .F., 'C')
    IF !EMPTY(lcCurVal)
      =loVendor.SEEK(lcCurVal)
      loOGScroll.laOGFxFlt[lnPos,6] = lcCurVal      && Update the field
    ELSE
      llRetVal = .F.
    ENDIF
  ENDIF
ENDIF
loVendor = .NULL.

SELECT (lnAlias)
RETURN llRetVal

*!**************************************************************************
*! Name      : lfvGetColor
*! Developer : Wael Ali Mohamed
*! Date      : 09/01/2006
*! Purpose   : Valid Paint color related field     E302303
*!**************************************************************************
*! Example   :  =lfvGetColor()
*!**************************************************************************
FUNCTION lfvGetColor
LOCAL lnAlias, llRetVal, lcControl, lcCurVar, lcCurVal, lnPos

lnAlias   = SELECT(0)
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lnPos     = lcCurVar.Parent.nRowIndex
lcCurVal  = PADR(lcCurVar.Value, 8)
llRetVal  = .T.

lcCurVal =ALLTRIM(STR(GETCOLOR(VAL(lcCurVal))))
IF !EMPTY(lcCurVal)
  loOGScroll.laOGFxFlt[lnPos,6] = lcCurVal      && Update the field
ENDIF

RETURN llRetVal


*!**************************************************************************
*! Name      : FileFieldCreated
*! Developer : Mohamed Atia Badran (MAB) B609729,1
*! Date      : 11/13/2011
*! Purpose   : Include Aria27 files while validating code deletion
*!**************************************************************************
*! Example   :  =FileFieldCreated() returns True/False
*!**************************************************************************
*!*
FUNCTION FileFieldCreated
LPARAMETERS lcSqlStatement, lcPassedydFlFld

LOCAL lcAria4SydFlFld, llFailed
lcAria4SydFlFld = gfTempName()

IF USED(lcPassedydFlFld)
  USE IN (lcPassedydFlFld)
ENDIF

llFailed = !(lfSQLStatement(lcSqlStatement, lcAria4SydFlFld, '', '', .T., oAriaApplication.cAria4SysFiles) AND;
            lfSQLStatement(lcSqlStatement, lcPassedydFlFld, '', '', .T., oAriaApplication.SystemConnectionString))

IF llFailed
  IF USED(lcAria4SydFlFld)  
    USE IN (lcAria4SydFlFld)
  ENDIF

ELSE

  LOCAL lcInAria4SQL, lcInAria4Cursor
  lcInAria4Cursor = gfTempName()
  
  *-- Loop Aria27 files to exclude converted files ........
  SELECT (lcPassedydFlFld)
  SCAN 
    SCATTER MEMVAR MEMO
    lcInAria4SQL = "SELECT TOP 1 1 FROM sydfiles f WHERE '" + ALLTRIM(m.cFile_Nam) + "'$f.MFLDFDATA ORDER BY cfile_nam"
    IF lfSQLStatement(lcInAria4SQL, lcInAria4Cursor, '', '', .T., oAriaApplication.cAria4SysFiles)
      IF (RECCOUNT(lcInAria4Cursor) = 1)
        SELECT (lcPassedydFlFld)
        DELETE 
      ENDIF 
      USE IN (lcInAria4Cursor)
    ENDIF
  ENDSCAN 

  *-- Loop Aria4 files to append to list ........
  SELECT (lcAria4SydFlFld)
  SCAN 
    SCATTER MEMVAR MEMO
    SELECT (lcPassedydFlFld)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN 
  USE IN (lcAria4SydFlFld)
  SELECT (lcPassedydFlFld)
  LOCATE 
ENDIF
RETURN !llFailed
ENDFUNC 
*-- End of FileFieldCreated